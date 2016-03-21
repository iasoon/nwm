module Windows (
    manage, unmanage, push, arrange
) where

import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           Contexts
import           Control.Lens
import           Core
import           Focus
import           XControl
import qualified ZipperTree          as T


arrange :: NWM ()
arrange = do
    -- Rezip layout tree
    isVisible <- flip S.member <$> visibleWs
    windowTree %= T.zip isVisible . T.unzip
    -- Arrange visible windows
    rect <- screenRect >>= applyGap
    use windowTree >>= arrangeTree rect
    -- Fix focus
    focusedWin >>= \focused -> case focused of
        Just win | isVisible win -> giveFocus win
                 | otherwise     -> focusClosest win
        Nothing                  -> return ()


tile :: Window -> NWM ()
tile win = do
    foc <- focusedWin
    tree <- use windowTree
    let loc = fromMaybe (pure tree) (foc >>= flip T.find tree)
    let dir = maybe (T.right) T.rotRight (T.lastDir loc)
    windowTree .= (T.tree . fmap (T.insert dir 0.5 (T.leaf win)) $ loc)


moveWindow :: Window -> Rect -> NWM ()
moveWindow win rect = do
    assign (windows . at win . _Just . windowRect) rect
    setWindowGeometry win rect


showWindow :: Window -> NWM ()
showWindow win = do
    mapWindow win
    Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
    contextData ctx . visibleWindows %= S.insert win
    focus win


hideWindow :: Window -> NWM ()
hideWindow win = do
    Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
    contextData ctx . visibleWindows %= S.delete win


manage :: Window -> NWM ()
manage win = do
    rect <- getWindowGeometry win
    ctx <- selectedContext
    assign (windows . at win ) $ Just $ WindowData
        { _windowRect    = rect
        , _windowContext = ctx
        }
    tile win
    showWindow win
    arrange



unmanage :: Window -> NWM ()
unmanage win = do
    isClient <- M.member win <$> use windows
    when isClient $ do
        windowTree %= T.delete win . T.unzip
        hideWindow win
        arrange
        windows . at win .= Nothing



push :: T.Direction -> NWM ()
push d = focusedWin >>= whenJust . fmap (pushWin d)


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

