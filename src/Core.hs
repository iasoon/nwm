{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Core (
    XControl, runXControl, forkControl,
    windowTree, focused, windowGap,
    windowRects, windowRect,
    NWM, runNWM, NWMState, initialState,
    HasControl (..),
    Rect (..), Window, Tag
) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as M
import           Graphics.XHB         (Connection, WINDOW)

import qualified ZipperTree           as T

type Window = WINDOW
type Tag = String

data Rect = Rect Int Int Int Int

data NWMState = NWMState
    { _windowRects :: M.Map Window Rect
    , _windowTree  :: T.ZipperTree Window
    , _focused     :: Maybe Window
    , _windowGap   :: Int
    }

makeLenses ''NWMState

windowRect :: Window -> Lens' NWMState (Maybe Rect)
windowRect win = windowRects . at win

initialState :: NWMState
initialState = NWMState
    { _windowRects   = M.empty
    , _windowTree    = T.empty
    , _focused       = Nothing
    , _windowGap     = 0
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
