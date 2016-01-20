import qualified Graphics.XHB as X

import           Commands
import           Core
import           Events
import           XControl

main :: IO ()
main = do
    Just dpy <- X.connect
    runXControl nwm dpy initialState

nwm :: XControl ()
nwm = do
    _ <- forkControl $ subscribeEvents >> handleEvents
    readCommands
