{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Core (
    XControl, runXControl, forkControl,
    windowTree, windowGap, activeContexts,
    windows, windowContext, windowRect, windowName,
    contexts, contextData,
    borderWidth, borderColor, focusColor,
    whenJust,
    NWM, runNWM, NWMState, initialState,
    HasControl (..),
    Rect (..), Window, WindowData (..),
    Context (..), ContextData (..),
    focusedWindow, visibleWindows, namedWindows
) where

import           Control.Concurrent
import           Control.Lens         hiding (Context, contexts)
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
    , _namedWindows   :: M.Map String Window
    } deriving (Eq, Show)


emptyContextData :: ContextData
emptyContextData = ContextData
    { _visibleWindows = S.empty
    , _focusedWindow  = Nothing
    , _namedWindows   = M.empty
    }


makeLenses ''ContextData


data WindowData = WindowData
    { _windowRect    :: Rect
    , _windowContext :: Context
    , _windowName    :: Maybe String
    }


makeLenses ''WindowData


data NWMState = NWMState
    { _windows        :: M.Map Window WindowData
    , _windowTree     :: T.ZipperTree Window
    , _contexts       :: M.Map Context ContextData
    , _activeContexts :: [Context]
    , _windowGap      :: Int
    , _borderWidth    :: Int
    , _borderColor    :: Int
    , _focusColor     :: Int
    }


makeLenses ''NWMState


contextData :: Context -> Lens' NWMState ContextData
contextData context = contexts . at context . non emptyContextData


initialState :: NWMState
initialState = NWMState
    { _windows        = M.empty
    , _windowTree     = T.empty
    , _contexts       = M.empty
    , _activeContexts = [Root]
    , _windowGap      = 5
    , _borderWidth    = 1
    , _borderColor    = 0xdddddd
    , _focusColor     = 0xff0000
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
