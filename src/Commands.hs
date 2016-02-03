{-# LANGUAGE OverloadedStrings #-}

module Commands (readCommands) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as BS
import           System.IO

import           Core
import           Operations
import qualified ZipperTree                 as T

readCommands :: XControl ()
readCommands = do
    h <- liftIO $ openFile "nwm_fifo" ReadMode
    forever $ do
        l <- liftIO $ BS.hGetLine h
        runNWM $ whenJust $ maybeResult $ parse command l

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
