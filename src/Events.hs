{-# LANGUAGE ExistentialQuantification #-}

module Events (handleEvents) where

import           Control.Monad
import qualified Graphics.XHB  as X

import           Core
import           Windows
import           XControl

data EventHandler = forall e . X.Event e => EventHandler (e -> XControl ())


handleEvent :: X.SomeEvent -> EventHandler -> XControl ()
handleEvent e (EventHandler h) = maybe (return ()) h $ X.fromEvent e


handleSomeEvent :: X.SomeEvent -> XControl ()
handleSomeEvent e = mapM_ (handleEvent e)
    [ EventHandler handleMapRequest
    , EventHandler handleDestroyNotify
    ]


handleEvents :: XControl ()
handleEvents = forever $ waitForEvent >>= handleSomeEvent


handleMapRequest :: X.MapRequestEvent -> XControl ()
handleMapRequest = runNWM . manage . X.window_MapRequestEvent


handleDestroyNotify :: X.DestroyNotifyEvent -> XControl ()
handleDestroyNotify = runNWM . unmanage . X.window_DestroyNotifyEvent
