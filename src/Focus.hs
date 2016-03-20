module Focus (
    focus, focusedWin, moveFocus, focusClosest, visibleWs
) where

import           Control.Lens
import           Data.Function (on)
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe
import qualified Data.Set      as S
import Control.Monad.State

import           Contexts
import           Core
import           XControl
import qualified ZipperTree    as T


focus :: Window -> NWM ()
focus win = do
    Just ctx <- preview (windows . at win . _Just . windowContext) <$> get
    contextData ctx . focusedWindow .= Just win
    activeContexts %= (ctx:) . (delete ctx)
    giveFocus win


fixFocus :: NWM ()
fixFocus = focusedWin >>= whenJust . fmap giveFocus


visibleWs :: NWM (S.Set Window)
visibleWs = S.unions . map (view visibleWindows) <$> activeContextData


focusedWin :: NWM (Maybe Window)
focusedWin = listToMaybe . mapMaybe (view focusedWindow) <$> activeContextData


visibleWindowRects :: NWM [(Window, Rect)]
visibleWindowRects = do
    isVisible <- flip S.member <$> visibleWs
    filter (isVisible . fst) . rects <$> use windows
        where rects = map (\(w,d) -> (w, view windowRect d)) . M.assocs


focusClosest :: Window -> NWM ()
focusClosest win = do
    Just rect <- preview (windows . at win . _Just . windowRect) <$> get
    candidates <- filter ((/=win) . fst) <$> visibleWindowRects
    whenJust . fmap focus $ closestWin candidates (center rect)


moveFocus :: T.Direction -> NWM ()
moveFocus d = do
    win' <- focusedWin
    case win' of
      Nothing -> return ()
      Just win -> do
        Just rect <- preview (windows . at win . _Just . windowRect) <$> get
        candidates <- filter ((/=win) . fst) <$> visibleWindowRects
        whenJust $ focus <$> closestWin' d candidates (center rect)


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
relativeCenters d pt = map $ \(a,r) -> (a, relativeTo d pt (center r))
