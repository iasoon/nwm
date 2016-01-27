{-# LANGUAGE OverloadedStrings #-}

module Commands (readCommands) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as BS
import           System.IO

import           Core
import           Operations

readCommands :: XControl ()
readCommands = do
    h <- liftIO $ openFile "nwm_fifo" ReadMode
    forever $ do
        l <- liftIO $ BS.hGetLine h
        whenJust $ maybeResult $ parse command l

command :: Parser (XControl ())
command = pointerCmd

pointerCmd :: Parser (XControl ())
pointerCmd = do
    _ <- string "pointer focus"
    return (runNWM focusPointer)
