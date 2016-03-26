module Contexts (
    showContext, hideContext, switchContexts,
    namedContexts,
    activeContextData, selectedContext
) where

import           Control.Lens hiding (Context, contexts)
import           Data.List
import qualified Data.Map     as M
import           Data.Maybe

import           Core
import           XControl


selectedContext :: NWM Context
selectedContext = fromMaybe Root . listToMaybe <$> use activeContexts


showContext :: Context -> NWM ()
showContext ctx = do
    activeContexts %= (ctx:) . (delete ctx)
    use (contextData ctx . visibleWindows) >>= mapM_ mapWindow


hideContext :: Context -> NWM ()
hideContext ctx = do
    activeContexts %= delete ctx
    use (contextData ctx . visibleWindows) >>= mapM_ unmapWindow


switchContexts :: [Context] -> NWM ()
switchContexts ctxs = do
    use activeContexts >>= mapM_ hideContext
    mapM_ showContext (reverse ctxs)


activeContextData :: NWM [ContextData]
activeContextData = do
    ctxs <- use contexts
    mapMaybe (flip M.lookup ctxs) <$> use activeContexts


namedContexts :: NWM [String]
namedContexts = mapMaybe name . M.keys <$> use contexts
    where name (Root)    = Nothing
          name (Named n) = Just n
