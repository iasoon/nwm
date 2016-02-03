{-# LANGUAGE OverloadedStrings #-}

module Commands (readCommands) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString
import           Network.Socket             hiding (recv)
import           Network.Socket.ByteString
import           System.Directory
import           System.IO.Error

import           Core
import           Operations
import qualified ZipperTree                 as T

address :: String
address = "/tmp/nwm_socket"

openSocket :: IO Socket
openSocket = do
    sock <- socket AF_UNIX Stream defaultProtocol
    removeFile address `catch` \e -> unless (isDoesNotExistError e) (throwIO e)
    bind sock $ SockAddrUnix address
    listen sock sOMAXCONN
    return sock

readCommands :: XControl ()
readCommands = do
    sock <- liftIO openSocket
    forever (liftIO (accept sock) >>= serveClient)

serveClient :: (Socket, SockAddr) -> XControl ()
serveClient (sock, _) = do
    str <- liftIO $ recv sock 4096
    runNWM $ whenJust $ maybeResult $ parse command str
    liftIO $ close sock

command :: Parser (NWM ())
command =  choice [windowCmd, focusCmd]

windowCmd :: Parser (NWM ())
windowCmd = do
    _ <- string "push "
    direction >>= return . push

focusCmd :: Parser (NWM ())
focusCmd = string "focus " >> choice
    [ string "pointer" >> return focusPointer
    , moveFocus <$> direction
    ]


direction :: Parser T.Direction
direction = choice
    [ string "right" >> return T.right
    , string "left"  >> return T.left
    , string "up"    >> return T.up
    , string "down"  >> return T.down
    ]
