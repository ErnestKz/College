import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Control.Monad.Trans         (liftIO)
import           Data.IORef

main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "Calculator!"


-- ghcid -c 'stack ghci' \
--       --reload=./Main.hs \
--       -T Main.main \
--       --restart=./Cal.cabal
