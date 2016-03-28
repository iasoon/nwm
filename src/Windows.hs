module Windows (
    manage, unmanage, push, arrange, nameWindow, windowNamed,
    showWindow, hideWindow
) where

import           Control.Lens        hiding (Context, contexts)
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           Contexts
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
showWindow win = changeFocus $ do
    mapWindow win
    Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
    contextData ctx . visibleWindows %= S.insert win
    return $ Just win


hideWindow :: Window -> NWM ()
hideWindow win = do
    unmapWindow win
    Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
    contextData ctx . visibleWindows %= S.delete win
    try $ do
        focused <- expect focusedWin
        guard (focused == win)
        lift $ changeFocus $ visibleWs >>= closestWindow win


nameWindow :: String -> Window -> NWM ()
nameWindow name win = do
    Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
    try $ do
        w <- expect $ preview (contexts . at ctx . _Just . namedWindows . at name . _Just) <$> get
        assign (windows . at w . _Just . windowName) Nothing
    assign (windows . at win . _Just . windowName) (Just name)
    assign (contexts . at ctx . _Just . namedWindows . at name) (Just win)


windowNamed :: String -> NWM (Maybe Window)
windowNamed name = listToMaybe . mapMaybe find <$> activeContextData
    where find = view (namedWindows . at name)


manage :: Window -> NWM ()
manage win = do
    rect <- getWindowGeometry win
    ctx <- selectedContext
    assign (windows . at win ) $ Just $ WindowData
        { _windowRect    = rect
        , _windowContext = ctx
        , _windowName    = Nothing
        }
    tile win
    showWindow win
    decorate win
    arrange


-- TODO: clean this up
unmanage :: Window -> NWM ()
unmanage win = do
    isClient <- M.member win <$> use windows
    when isClient $ do
        windowTree %= T.delete win . T.unzip
        Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
        contextData ctx . visibleWindows %= S.delete win
        arrange
        use (contextData ctx . focusedWindow) >>= \foc -> case foc of
            Just w | w == win ->
                use (contextData ctx . visibleWindows)
                    >>= closestWindow win
                    >>= assign (contextData ctx . focusedWindow)
            _                 -> return ()
        changeFocus focusedWin
        windows . at win .= Nothing


push :: T.Direction -> NWM ()
push d = try $ expect focusedWin >>= lift . pushWin d


pushWin :: T.Direction -> Window -> NWM ()
pushWin d win = try $ do
    loc <- expect $ T.find win <$> use windowTree
    assign windowTree $ T.tree $ T.push d loc
    lift arrange


shaveRect :: Int -> Rect -> Rect
shaveRect i (Rect x y w h) = Rect (x+i) (y+i) (w-2*i) (h-2*i)


applyGap :: Rect -> NWM Rect
applyGap rect = do
    gap <- use windowGap
    border <- use borderWidth
    return $ shaveRect (gap + border) rect


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

