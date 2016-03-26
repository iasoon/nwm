{-# LANGUAGE OverloadedStrings #-}

module Commands (readCommands) where

import           Control.Exception                (catch, throwIO)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BS
import           Data.Word                        (Word32)
import           Network.Socket                   hiding (recv, send)
import           Network.Socket.ByteString
import           System.Directory
import           System.Exit                      (exitSuccess)
import           System.IO.Error

import           Contexts
import           Core
import           Focus
import           Windows
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
    res <- liftIO $ parseOnly command <$> recv sock 8192
    case res of
      Right cmd -> runNWM (cmd sock)
      Left  err -> liftIO $ putStrLn ("err " ++ err)
    liftIO $ close sock


type Command = Socket -> NWM ()


command :: Parser Command
command =  choice [windowCmd, contextCmd, exitCmd]


windowCmd :: Parser Command
windowCmd = "window " *> (const <$> choice
    [ "push "  *> ((>> arrange) <$> (push <$> direction))
    , "focus " *> (moveFocus <$> direction)
    , "name "  *> (nameFocused <$> name)
    , "show " *> ((\w -> w >>= maybe (return ()) showWindow >> arrange) . windowNamed <$> name)
    , "hide " *> ((\w -> w >>= maybe (return ()) hideWindow >> arrange) . windowNamed <$> name)
    ])

nameFocused :: String -> NWM ()
nameFocused name = do
    focus <- focusedWin
    maybe (return ()) (nameWindow name) focus


direction :: Parser T.Direction
direction = choice
    [ "right" *> return T.right
    , "left"  *> return T.left
    , "up"    *> return T.up
    , "down"  *> return T.down
    ]


contextCmd :: Parser Command
contextCmd = "context " *> choice
        [ manipulateContexts
        , "list" *> return listContexts
        ]


listContexts :: Command
listContexts sock = do
    msg <- BS.pack . unlines <$> namedContexts
    liftIO $ send sock msg >> return ()


manipulateContexts :: Parser Command
manipulateContexts = (\x -> const (x >> arrange)) <$> choice
    [ "show "   *> (showContext <$> context)
    , "hide "   *> (hideContext <$> context)
    , "switch " *> (switchContexts . (:[Root]) <$> context)
    ]


exitCmd :: Parser Command
exitCmd = "exit" *> return (const $ liftIO exitSuccess)


window :: Parser Window
window = convertXid <$> hexword


name :: Parser String
name = many1 (notChar '\n')

context :: Parser Context
context = Named <$> name


hexword :: Parser Word32
hexword = option "" "0x" >> hexadecimal
