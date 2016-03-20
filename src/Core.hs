{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Core (
    XControl, runXControl, forkControl,
    windowTree, windowGap, windowRects, windowRect, activeContexts,
    windowContexts,
    contextDataMap, contextData,
    focusedWindow, visibleWindows,
    whenJust,
    NWM, runNWM, NWMState, initialState,
    HasControl (..),
    Rect (..), Window, Context (..), ContextData (..)
) where

import           Control.Concurrent
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Graphics.XHB         (Connection, WINDOW)

import qualified ZipperTree           as T


type Window = WINDOW


data Rect = Rect Int Int Int Int


data Context = Root | Named String deriving (Eq, Ord, Show)


data ContextData = ContextData
    { _visibleWindows :: S.Set Window
    , _focusedWindow  :: Maybe Window
    } deriving (Eq, Show)


emptyContextData :: ContextData
emptyContextData = ContextData
    { _visibleWindows = S.empty
    , _focusedWindow  = Nothing
    }

makeLenses ''ContextData


data NWMState = NWMState
    { _windowRects    :: M.Map Window Rect
    , _windowContexts :: M.Map Window Context
    , _windowTree     :: T.ZipperTree Window
    , _contextDataMap :: M.Map Context ContextData
    , _activeContexts :: [Context]
    , _windowGap      :: Int
    }


makeLenses ''NWMState


windowRect :: Window -> Lens' NWMState (Maybe Rect)
windowRect win = windowRects . at win

contextData :: Context -> Lens' NWMState ContextData
contextData context = contextDataMap . at context . non emptyContextData


initialState :: NWMState
initialState = NWMState
    { _windowRects    = M.empty
    , _windowContexts = M.empty
    , _windowTree     = T.empty
    , _contextDataMap = M.empty
    , _activeContexts = [Root]
    , _windowGap      = 5
    }


data XConf = XConf
    { connection :: Connection
    , statevar   :: MVar NWMState
    }


newtype XControl a = XControl (ReaderT XConf IO a)
    deriving (Functor, Applicative, Monad, MonadIO)


runXControl :: XControl a -> Connection -> NWMState -> IO a
runXControl (XControl x) conn nwmstate = do
    var <- newMVar nwmstate
    runReaderT x XConf
        { connection = conn
        , statevar = var
        }


forkControl :: XControl () -> XControl ThreadId
forkControl (XControl x) = XControl ask >>= liftIO . forkIO . runReaderT x


newtype NWM a = NWM (StateT NWMState XControl a)
    deriving (Functor, Applicative, Monad, MonadIO , MonadState NWMState)


runNWM :: NWM a -> XControl a
runNWM (NWM n) = do
    var <- XControl $ asks statevar
    (a, s) <- liftIO (takeMVar var) >>= runStateT n
    liftIO $ putMVar var s
    return a


class MonadIO m => HasControl m where
    askConnection :: m Connection


instance HasControl XControl where
    askConnection = XControl $ asks connection


instance HasControl NWM where
    askConnection = NWM $ lift askConnection


whenJust :: Monad m => Maybe (m ()) -> m ()
whenJust = maybe (return ()) id
