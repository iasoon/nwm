module Focus (
    fixFocus, changeFocus, focusedWin, closestWindow, moveFocus, visibleWs, decorate
) where

import           Control.Lens
import           Control.Monad.State
import           Data.Function       (on)
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S

import           Contexts
import           Core
import           XControl
import qualified ZipperTree          as T


fixFocus :: NWM () -> NWM ()
fixFocus action = do
    prev <- focusedWin
    action
    maybe (return ()) decorate prev
    try $ do
        win <- expect focusedWin
        lift $ giveFocus win >> decorate win


changeFocus :: NWM (Maybe Window) -> NWM ()
changeFocus action = fixFocus $ try $ do
    expect action >>= \win -> lift $ do
        Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
        contextData ctx . focusedWindow .= Just win
        activeContexts %= (ctx:) . (delete ctx)


decorate :: Window -> NWM ()
decorate win = use borderWidth >>= \width -> try $
    expect focusedWin >>= \focused -> lift $ do
        color <- if focused == win
                    then use focusColor
                    else use borderColor
        setBorder width color win


visibleWs :: NWM (S.Set Window)
visibleWs = S.unions . map (view visibleWindows) <$> activeContextData


focusedWin :: NWM (Maybe Window)
focusedWin = listToMaybe . mapMaybe (view focusedWindow) <$> activeContextData

windowRects :: S.Set Window -> NWM [(Window, Rect)]
windowRects ws = filter (inSet . fst) . rects <$> use windows
    where inSet w = S.member w ws
          rects = map (\(w,d) -> (w, view windowRect d)) . M.assocs



closestWindow :: Window -> S.Set Window -> NWM (Maybe Window)
closestWindow win candidates = do
    Just rect <- preview (windows . at win . _Just . windowRect) <$> get
    closestCenter (center rect) <$> windowRects candidates


moveFocus :: T.Direction -> NWM ()
moveFocus d = try $ do
    win <- expect focusedWin
    lift $ changeFocus $ do
        Just rect <- preview (windows . at win . _Just . windowRect) <$> get
        candidates <- S.delete win <$> visibleWs
        closestCenterDir d (center rect) <$> windowRects candidates


closest :: [(a, (Int, Int))] -> Maybe a
closest [] = Nothing
closest cs = Just $ fst $ lowest $ map (over _2 centerDist) cs
    where lowest = minimumBy (compare `on` snd)
          centerDist (x,y) = div (abs x + abs y) 2


closestCenter :: (Int, Int) -> [(a, Rect)] -> Maybe a
closestCenter pt = closest . relativeCenters T.right pt


closestCenterDir :: T.Direction -> (Int, Int) -> [(a, Rect)] -> Maybe a
closestCenterDir d pt =  closest . inCone . relativeCenters d pt
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
relativeCenters d pt = map $ \(a,r) -> (a, relativeTo d pt (center r))
