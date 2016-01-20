module Commands (readCommands) where

import           Control.Monad
import           Control.Monad.Trans
import           System.IO
import           Text.Parsec

import           Core
import           Operations

readCommands :: XControl ()
readCommands = do
    h <- liftIO $ openFile "nwm_fifo" ReadMode
    forever $ do
        l <- liftIO $ hGetLine h
        either (liftIO . print) id $ runParser command () "command" l

command :: Parsec String () (XControl ())
command = pointerCmd

pointerCmd :: Parsec String () (XControl ())
pointerCmd = do
    _ <- string "pointer" >> spaces >> string "focus"
    return (runNWM focusPointer)
