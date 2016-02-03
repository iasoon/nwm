module Operations (
    whenJust,
    manage, unmanage, arrange,
    focus, focusPointer, moveFocus, push
) where

import           Control.Lens
import           Control.Monad.Trans
import           Data.Function       (on)
import           Data.List
import qualified Data.Map            as M

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
tile win = windowTree %= T.insert T.left 0.5 (T.leaf win)

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

push :: T.Direction -> NWM ()
push d = use focused >>= whenJust . fmap (pushWin d)

pushWin :: T.Direction -> Window -> NWM ()
pushWin d win = do
    t <- use windowTree
    whenJust $ assign windowTree . T.tree . T.push d <$> T.find win t
    arrange

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
arrangeTree rect t = case T.cursor t of
    T.Empty        -> return ()
    T.Leaf w       -> applyGap rect >>= moveWindow w
    T.Fork d f l r -> let (lr, rr) = splitRect d f rect in
                         arrangeTree lr l >> arrangeTree rr r

--------------------------------------------------------------------------------

moveFocus :: T.Direction -> NWM ()
moveFocus d = use focused >>= whenJust . fmap (focusClosest d)

focusClosest :: T.Direction -> Window -> NWM ()
focusClosest d win = do
    pt <- fmap center <$> use (windowRect win)
    candidates <- M.assocs . M.delete win <$> use windowRects
    whenJust . fmap focus $ pt >>= closest d candidates


closest :: T.Direction -> [(Window, Rect)] -> (Int, Int) -> Maybe Window
closest d ws pt
    | null candidates = Nothing
    | otherwise       = Just $ fst $ minimumBy (compare `on` snd) candidates
    where candidates = map (over _2 (\(x,y) -> div (x + y) 2)) cone
          cone = filter ((\(x,y) -> y <= x) . snd) wpos
          wpos = map (over _2 (over _2 abs . relativeTo d pt . center)) ws

center :: Rect -> (Int, Int)
center (Rect x y w h) = (x + (w `div` 2), y + (h `div` 2))

relativeTo :: T.Direction -> (Int, Int) -> (Int, Int) -> (Int, Int)
relativeTo d (xRef, yRef) (x,y)
    | d == T.right = ( dx, dy)
    | d == T.left  = (-dx, dy)
    | d == T.up    = (-dy,-dx)
    | otherwise    = ( dy, dx)
    where (dx, dy) = (x - xRef, y - yRef)
