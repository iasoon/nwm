{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XControl (
    XRequest, runXRequest,
    mapWindow, unmapWindow, giveFocus,
    pointerPosition, pointerWindow,
    getWindowGeometry, setWindowGeometry,
    subscribeEvents, waitForEvent,
    screenRect,
    convertXid
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Graphics.XHB as X

import Core

newtype XRequest m a = XRequest (ExceptT X.SomeError m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadState s)

instance HasControl m => HasControl (XRequest m) where
    askConnection = XRequest $ lift $ askConnection

runXRequest :: (Monad m) => (X.SomeError -> m a) -> XRequest m a -> m a
runXRequest handler (XRequest req) = runExceptT req >>= either handler return

noWindow :: Window
noWindow = X.fromXid X.xidNone

withConn :: HasControl m => (X.Connection -> a -> IO b) -> a -> m b
withConn f arg = askConnection >>= liftIO . flip f arg

withConn2 :: HasControl m => (X.Connection -> a -> b -> IO c) -> a -> b -> m c
withConn2 = curry . withConn . (uncurry .)

requestWithConn :: HasControl m => (X.Connection ->  a -> IO (X.Receipt b))
                                   -> a -> XRequest m b
requestWithConn f arg = withConn f arg >>= getReply
    where getReply = XRequest . ExceptT . liftIO . X.getReply

getRoot :: HasControl m => m Window
getRoot = X.getRoot <$> askConnection

getWindow :: Window -> Maybe Window
getWindow w = guard (w /= noWindow) >> return w

queryPointer :: HasControl m => XRequest m X.QueryPointerReply
queryPointer = getRoot >>= requestWithConn X.queryPointer

pointerWindow :: HasControl m => XRequest m (Maybe Window)
pointerWindow = getWindow . X.child_QueryPointerReply <$> queryPointer

pointerPosition :: HasControl m => XRequest m (Int, Int)
pointerPosition = getPos <$> queryPointer
    where getPos reply = (fromIntegral $ X.root_x_QueryPointerReply reply
                         ,fromIntegral $ X.root_y_QueryPointerReply reply)

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

getWindowGeometry :: HasControl m => Window -> XRequest m Rect
getWindowGeometry = fmap getRect . requestWithConn X.getGeometry . convertXid
    where getRect reply = Rect
            (fromIntegral $ X.x_GetGeometryReply reply)
            (fromIntegral $ X.y_GetGeometryReply reply)
            (fromIntegral $ X.width_GetGeometryReply reply)
            (fromIntegral $ X.height_GetGeometryReply reply)

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
