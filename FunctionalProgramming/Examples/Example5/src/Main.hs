{-# Language ScopedTypeVariables #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny

canvasSize = 400

data Modes = Fill | NoFill deriving Show


main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  return window # set title "Clickable canvas"

  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

  fillMode  <- UI.button #+ [string "Fill"]
  emptyMode <- UI.button #+ [string "Hollow"]
  clear     <- UI.button #+ [string "Clear"]

  getBody window #+
    [column [element canvas]
    , element fillMode, element emptyMode, element clear]


  let -- type declarations in let clauses are allowed by the ScopedTypeVariables extension
      efs :: Event (Modes -> Modes)
      efs = const Fill <$ UI.click fillMode

      ehs :: Event (Modes -> Modes)
      ehs = const NoFill <$ UI.click emptyMode

      modeEvent :: Event (Modes -> Modes)
      modeEvent = unionWith const efs ehs

  drawingMode <- accumB Fill modeEvent
  mousePos <- stepper (0,0) $ UI.mousemove canvas
  let bst :: Behavior (Modes, (Double,Double))
      bst = (,) <$> drawingMode <*> mousePos

      eDraw :: Event (Modes, (Double,Double))
      eDraw = bst <@ UI.click canvas

  onEvent eDraw $ \e -> do drawShape e canvas

  on UI.click clear $ const $
    canvas # UI.clearCanvas

drawShape :: (Modes, (Double,Double)) -> Element -> UI ()
drawShape (Fill, (x,y)) canvas = do
  canvas # set' UI.fillStyle   (UI.htmlColor "black")
  canvas # UI.fillRect (x,y) 100 100
drawShape (NoFill, (x,y)) canvas = do
  canvas # set' UI.fillStyle   (UI.htmlColor "white")
  canvas # UI.fillRect (x,y) 100 100



