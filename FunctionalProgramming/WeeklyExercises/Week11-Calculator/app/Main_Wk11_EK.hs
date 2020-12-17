-- Ernests Kuznecovs
-- 17332791

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

  button9 <- UI.button # set UI.text "9"
  button8 <- UI.button # set UI.text "8"
  button7 <- UI.button # set UI.text "7"
  button6 <- UI.button # set UI.text "6"
  button5 <- UI.button # set UI.text "5"
  button4 <- UI.button # set UI.text "4"
  button3 <- UI.button # set UI.text "3"
  button2 <- UI.button # set UI.text "2"
  button1 <- UI.button # set UI.text "1"
  button0 <- UI.button # set UI.text "0"
  buttonc <- UI.button # set UI.text "c"
  badd <- UI.button # set UI.text "+"
  bmul <- UI.button # set UI.text "x"
  bsub <- UI.button # set UI.text "-"
  bdiv <- UI.button # set UI.text "/"
  beq <- UI.button # set UI.text "="

  output_display <- UI.h4 # set UI.text "supa calculator"

  getBody window #+
    [ row [element button7, element button8, element button9]
    , row [element button4, element button5, element button6]
    , row [element button1, element button2, element button3]
    , row [element button0]
    , row [element beq]
    , row [element bmul, element badd, element bdiv, element bsub]
    , row [element buttonc]
    , row [element output_display]
    ]

  input <- liftIO $ newIORef 0
  operand <- liftIO $ newIORef 1
  output <- liftIO $ newIORef 0

  operator <- liftIO $ newIORef (*)

  display <- liftIO $ newIORef input

  let refresh_display = do
        output_ref <- liftIO $ readIORef display
        current_output <- liftIO $ readIORef output_ref
        element output_display # set UI.text (show current_output)
        return ()

  let add_digit_to_input n = do
        current_num <- readIORef input
        let new_num = (current_num * 10) + n
        return new_num

  let set_display_input = do
        writeIORef display input

  let num_click n = do
        new_num <- liftIO $ add_digit_to_input n
        liftIO $ writeIORef input new_num
        liftIO $ set_display_input
        refresh_display

  let clear = do
        liftIO $ writeIORef input 0
        liftIO $ writeIORef operand 1
        liftIO $ writeIORef output 0
        liftIO $ writeIORef display input
        liftIO $ writeIORef operator (*)
        refresh_display

  let eq = do
        current_input <- liftIO $ readIORef input
        current_operand <- liftIO $ readIORef operand
        current_operator <- liftIO $ readIORef operator
        liftIO $ writeIORef output
          (current_operator current_operand current_input)
        liftIO $ writeIORef display output
        refresh_display

  let set_operator op = do
       current_input <- liftIO $ readIORef input
       current_operand <- liftIO $ readIORef operand
       current_operator <- liftIO $ readIORef operator
       let res = current_operator current_operand current_input
       liftIO $ writeIORef output res
       liftIO $ writeIORef operand res
       liftIO $ writeIORef display output
       liftIO $ writeIORef operator op
       liftIO $ writeIORef input 0
       refresh_display

  on UI.click button0 $ const $ do
    num_click 0
  on UI.click button1 $ const $ do
    num_click 1
  on UI.click button2 $ const $ do
    num_click 2
  on UI.click button3 $ const $ do
    num_click 3
  on UI.click button4 $ const $ do
    num_click 4
  on UI.click button5 $ const $ do
    num_click 5
  on UI.click button6 $ const $ do
    num_click 6
  on UI.click button7 $ const $ do
    num_click 7
  on UI.click button8 $ const $ do
    num_click 8
  on UI.click button9 $ const $ do
    num_click 9

  on UI.click buttonc $ const $ do
    clear

  on UI.click bmul $ const $ do
    set_operator (*)
  on UI.click badd $ const $ do
    set_operator (+)
  on UI.click bdiv $ const $ do
    set_operator (/)
  on UI.click bsub $ const $ do
    set_operator (-)
  on UI.click beq $ const $ do
    eq

-- ghcid -c 'stack ghci' \
--       --reload=./Main.hs \
--       -T Main.main \
--       --restart=./Cal.cabal
