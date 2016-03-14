{-# LANGUAGE OverloadedStrings #-}

module Commands (readCommands) where

import           Control.Exception                (catch, throwIO)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString.Char8
import           Data.Word                        (Word32)
import           Network.Socket                   hiding (recv)
import           Network.Socket.ByteString
import           System.Directory
import           System.Exit                      (exitSuccess)
import           System.IO.Error

import           Core
import           Operations
import           XControl                         (convertXid)
import qualified ZipperTree                       as T

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
    res <- liftIO $ parseOnly command <$> recv sock 4096
    case res of
      Right cmd -> runNWM cmd
      Left  err -> liftIO $ putStrLn err
    liftIO $ close sock

command :: Parser (NWM ())
command =  choice [windowCmd, focusCmd, contextCmd, exitCmd]

windowCmd :: Parser (NWM ())
windowCmd = "push " *> (push <$> direction)

focusCmd :: Parser (NWM ())
focusCmd = "focus " *> choice
    [ "pointer" *> return focusPointer
    , moveFocus <$> direction
    ]

direction :: Parser T.Direction
direction = choice
    [ "right" *> return T.right
    , "left"  *> return T.left
    , "up"    *> return T.up
    , "down"  *> return T.down
    ]

contextCmd :: Parser (NWM ())
contextCmd = choice
    [ "show " *> (showContext <$> context)
    , "hide " *> (hideContext <$> context)
    ]

exitCmd :: Parser (NWM ())
exitCmd = "exit" *> return (liftIO exitSuccess)

window :: Parser Window
window = convertXid <$> hexword

context :: Parser Context
context = Named <$> many1 (notChar '\n')

hexword :: Parser Word32
hexword = option "" "0x" >> hexadecimal
