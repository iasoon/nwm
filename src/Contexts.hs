module Contexts (
    showContext, hideContext, activeContextData, selectedContext
) where

import Control.Lens hiding (Context)
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Core
import XControl


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


activeContextData :: NWM [ContextData]
activeContextData = do
    ctxDataMap <- use contextDataMap
    mapMaybe (flip M.lookup ctxDataMap) <$> use activeContexts
