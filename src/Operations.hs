module Operations (
    whenJust,
    manage, unmanage, arrange,
    focus, focusPointer
) where

import           Control.Lens
import           Control.Monad.Trans

import           Core
import           XControl
import qualified ZipperTree          as T

moveWindow :: Window -> Rect -> NWM ()
moveWindow win rect = do
    windowRect win .= Just rect
    setWindowGeometry win rect

whenJust :: Monad m => Maybe (m ()) -> m ()
whenJust = maybe (return ()) id

printErrors :: MonadIO m => XRequest m () -> m ()
printErrors = runXRequest (liftIO . print)

focus :: Window -> NWM ()
focus win = do
    focused .= Just win
    giveFocus win

focusPointer :: NWM ()
focusPointer = printErrors $ pointerWindow >>= lift . whenJust . fmap focus

tile :: Window -> NWM ()
tile win = windowTree %= T.insert (T.Dir T.X T.Neg) 0.5 (T.leaf win)

manage :: Window -> NWM ()
manage win = do
    registerWindow win
    mapWindow win
    tile win
    arrange

unmanage :: Window -> NWM ()
unmanage win = do
    windowRect win .= Nothing
    windowTree %= T.delete win
    arrange

registerWindow :: Window -> NWM ()
registerWindow win = printErrors $ do
    rect <- getWindowGeometry win
    windowRect win .= Just rect

arrange :: NWM ()
arrange = do
    rect <- screenRect >>= applyGap
    use windowTree >>= arrangeTree rect

shaveRect :: Int -> Rect -> Rect
shaveRect i (Rect x y w h) = Rect (x+i) (y+i) (w-2*i) (h-2*i)

applyGap :: Rect -> NWM Rect
applyGap = (shaveRect <$> (use windowGap) <*>) . return

splitRect :: T.Axis -> Rational -> Rect -> (Rect, Rect)
splitRect T.X f (Rect x y w h) = (Rect x y m h, Rect (x+m) y (w-m) h)
    where m = round (f * (fromIntegral w))
splitRect T.Y f (Rect x y w h) = (Rect x y w m, Rect x (y+m) w (h-m))
    where m = round (f * (fromIntegral h))

arrangeTree :: Rect -> T.ZipperTree Window -> NWM ()
arrangeTree rect tree = case T.node tree of
    T.Empty        -> return ()
    T.Leaf w       -> applyGap rect >>= moveWindow w
    T.Fork d f l r -> let (lr, rr) = splitRect d f rect in
                         arrangeTree lr l >> arrangeTree rr r
