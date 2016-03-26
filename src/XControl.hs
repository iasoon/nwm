{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XControl (
    mapWindow, unmapWindow, giveFocus,
    getWindowGeometry, setWindowGeometry,
    subscribeEvents, waitForEvent,
    screenRect, setBorder,
    convertXid
) where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Graphics.XHB as X

import Core

noWindow :: Window
noWindow = X.fromXid X.xidNone

withConn :: HasControl m => (X.Connection -> a -> IO b) -> a -> m b
withConn f arg = askConnection >>= liftIO . flip f arg

withConn2 :: HasControl m => (X.Connection -> a -> b -> IO c) -> a -> b -> m c
withConn2 = curry . withConn . (uncurry .)

getRoot :: HasControl m => m Window
getRoot = X.getRoot <$> askConnection

getWindow :: Window -> Maybe Window
getWindow w = guard (w /= noWindow) >> return w

convertXid :: (X.XidLike a, X.XidLike b) => a -> b
convertXid = X.fromXid . X.toXid

mapWindow :: HasControl m => Window -> m ()
mapWindow = withConn X.mapWindow

unmapWindow :: HasControl m => Window -> m ()
unmapWindow = withConn X.unmapWindow

setWindowGeometry :: HasControl m => Window -> Rect -> m ()
setWindowGeometry window (Rect x y w h) =
    withConn2 X.configureWindow window $ X.toValueParam
        [ (X.ConfigWindowX, fromIntegral x)
        , (X.ConfigWindowY, fromIntegral y)
        , (X.ConfigWindowWidth,  fromIntegral w)
        , (X.ConfigWindowHeight, fromIntegral h)
        ]

getWindowGeometry :: HasControl m => Window -> m Rect
getWindowGeometry win = do
    resp <- withConn X.getGeometry (convertXid win) >>= liftIO . X.getReply
    case resp of
      Right reply -> return $ getRect reply
      Left  err   -> error (show err) -- TODO: do not crash
    where getRect reply = Rect
            (fromIntegral $ X.x_GetGeometryReply reply)
            (fromIntegral $ X.y_GetGeometryReply reply)
            (fromIntegral $ X.width_GetGeometryReply reply)
            (fromIntegral $ X.height_GetGeometryReply reply)


setBorder :: HasControl m => Int -> Int -> Window -> m ()
setBorder width color win = do
    withConn2 X.configureWindow win $ X.toValueParam
        [(X.ConfigWindowBorderWidth, fromIntegral width)]
    withConn2 X.changeWindowAttributes win $ X.toValueParam
        [(X.CWBorderPixel, fromIntegral color)]


warpPointer :: HasControl m => Int -> Int -> m ()
warpPointer x y = do
    root <- getRoot
    withConn X.warpPointer $ X.MkWarpPointer noWindow root 0 0 0 0
                                  (fromIntegral x) (fromIntegral y)

giveFocus :: HasControl m => Window -> m ()
giveFocus window = withConn X.setInputFocus $ X.MkSetInputFocus
                     X.InputFocusPointerRoot window 0


screenRect :: HasControl m => m Rect
screenRect = do
    screen <- head . X.roots_Setup . X.connectionSetup <$> askConnection
    x <- return . fromIntegral . X.width_in_pixels_SCREEN $ screen
    y <- return . fromIntegral . X.height_in_pixels_SCREEN $ screen
    return $ Rect 0 0 x y


subscribeEvents :: HasControl m => m ()
subscribeEvents = do
    root <- getRoot
    withConn2 X.changeWindowAttributes root $ X.toValueParam
        [(X.CWEventMask, X.toMask [ X.EventMaskSubstructureRedirect
                                  , X.EventMaskSubstructureNotify
                                  ])]

waitForEvent :: HasControl m => m X.SomeEvent
waitForEvent = askConnection >>= liftIO . X.waitForEvent
