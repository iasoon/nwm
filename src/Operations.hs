module Operations (
    whenJust,
    unmanage, arrange,
    focus, focusPointer, moveFocus, push,
    showWindow, hideWindow
) where

import           Control.Lens        hiding (Context)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Function       (on)
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           Core
import           XControl
import qualified ZipperTree          as T


whenJust :: Monad m => Maybe (m ()) -> m ()
whenJust = maybe (return ()) id


moveWindow :: Window -> Rect -> NWM ()
moveWindow win rect = do
    windowRect win .= Just rect
    setWindowGeometry win rect


printErrors :: MonadIO m => XRequest m () -> m ()
printErrors = runXRequest (liftIO . print)


focus :: Window -> NWM ()
focus win = do
    Just ctx <- use $ windowContexts . at win
    contextData ctx . focusedWindow .= Just win
    giveFocus win


focusPointer :: NWM ()
focusPointer = printErrors $ pointerWindow >>= lift . whenJust . fmap focus


activeContextData :: NWM [ContextData]
activeContextData = do
    ctxDataMap <- use contextDataMap
    catMaybes . map (flip M.lookup ctxDataMap) <$> use activeContexts


visibleWs :: NWM (S.Set Window)
visibleWs = S.unions . map (view visibleWindows) <$> activeContextData


focusedWin :: NWM (Maybe Window)
focusedWin = first . map (view focusedWindow) <$> activeContextData
    where first = listToMaybe . catMaybes

selectedContext :: NWM Context
selectedContext = fromMaybe Root . listToMaybe <$> use activeContexts


tile :: Window -> NWM ()
tile win = do
    foc <- focusedWin
    tree <- use windowTree
    let loc = fromMaybe (pure tree) (foc >>= flip T.find tree)
    let dir = maybe (T.right) T.rotRight (T.lastDir loc)
    windowTree .= (T.tree . fmap (T.insert dir 0.5 (T.leaf win)) $ loc)


isClient :: Window -> NWM Bool
isClient win = M.member win <$> use windowRects


showWindow :: Window -> NWM ()
showWindow win = do
    mapWindow win
    client <- isClient win
    when (not client) (registerWindow win >> tile win)
    Just ctx <- use $ windowContexts . at win
    contextData ctx . visibleWindows %= S.insert win
    arrange
    focus win


hideWindow :: Window -> NWM ()
hideWindow win = do
    Just ctx <- use $ windowContexts . at win
    contextData ctx . visibleWindows %= S.delete win
    arrange
    focusedWin >>= \foc -> case foc of
        Just w | w == win -> focusClosest win
        _                 -> return ()


unmanage :: Window -> NWM ()
unmanage win = do
    windowTree %= T.delete win . T.unzip
    rezip
    windowRect win .= Nothing


registerWindow :: Window -> NWM ()
registerWindow win = printErrors $ do
    getWindowGeometry win >>= assign (windowRect win) . Just
    lift selectedContext >>= assign (windowContexts . at win) . Just


arrange :: NWM ()
arrange = do
    rezip
    rect <- screenRect >>= applyGap
    use windowTree >>= arrangeTree rect


rezip :: NWM ()
rezip = do
    isVisible <- flip S.member <$> visibleWs
    windowTree %= T.zip isVisible . T.unzip


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


visibleWindowRects :: NWM [(Window, Rect)]
visibleWindowRects = do
    isVisible <- flip S.member <$> visibleWs
    filter (isVisible . fst) . M.assocs <$> use windowRects


focusClosest :: Window -> NWM ()
focusClosest win = do
    Just rect <- use (windowRect win)
    candidates <- filter ((/=win) . fst) <$> visibleWindowRects
    whenJust . fmap focus $ closestWin candidates (center rect)


moveFocus :: T.Direction -> NWM ()
moveFocus d = do
    win' <- focusedWin
    case win' of
      Nothing -> return ()
      Just win -> do
        pt <- fmap center <$> use (windowRect win)
        candidates <- filter ((/=win) . fst) <$> visibleWindowRects
        whenJust . fmap focus $ pt >>= closestWin' d candidates


closest :: [(a, (Int, Int))] -> Maybe a
closest [] = Nothing
closest cs = Just $ fst $ lowest $ map (over _2 centerDist) cs
    where lowest = minimumBy (compare `on` snd)
          centerDist (x,y) = div (abs x + abs y) 2


closestWin :: [(Window, Rect)] -> (Int, Int) -> Maybe Window
closestWin rects pt = closest . relativeCenters T.right pt $ rects


closestWin' :: T.Direction -> [(Window, Rect)] -> (Int, Int) -> Maybe Window
closestWin' d rects pt =  closest . inCone . relativeCenters d pt $ rects
    where inCone = filter ((\(x,y) -> x >= abs y) . snd)


center :: Rect -> (Int, Int)
center (Rect x y w h) = (x + (w `div` 2), y + (h `div` 2))


relativeTo :: T.Direction -> (Int, Int) -> (Int, Int) -> (Int, Int)
relativeTo d (xRef, yRef) (x,y)
    | d == T.right = ( dx, dy)
    | d == T.left  = (-dx, dy)
    | d == T.up    = (-dy,-dx)
    | otherwise    = ( dy, dx)
    where (dx, dy) = (x - xRef, y - yRef)


relativeCenters :: T.Direction -> (Int, Int) -> [(a, Rect)] -> [(a, (Int, Int))]
relativeCenters d pt = map (over _2 (relativeTo d pt . center))
