{-# LANGUAGE ExistentialQuantification #-}

module Events (handleEvents) where

import           Control.Monad
import qualified Graphics.XHB  as X

import           Core
import           Operations
import           XControl

data EventHandler = forall e . X.Event e => EventHandler (e -> XControl ())

handleEvent :: X.SomeEvent -> EventHandler -> XControl ()
handleEvent e (EventHandler h) = whenJust (X.fromEvent e >>= return . h)

handleSomeEvent :: X.SomeEvent -> XControl ()
handleSomeEvent e = mapM_ (handleEvent e)
    [ EventHandler handleMapRequest
    , EventHandler handleUnmapNotify
    ]

handleEvents :: XControl ()
handleEvents = forever $ waitForEvent >>= handleSomeEvent

handleMapRequest :: X.MapRequestEvent -> XControl ()
handleMapRequest e = runNWM . manage $ X.window_MapRequestEvent e

handleUnmapNotify :: X.UnmapNotifyEvent -> XControl ()
handleUnmapNotify e = return () --runNWM . unmanage $ X.window_UnmapNotifyEvent e
