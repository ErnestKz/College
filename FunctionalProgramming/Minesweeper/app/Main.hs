{-# LANGUAGE TupleSections #-}
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import           Control.Monad               (void, when)

import qualified Control.Monad.State         as S

import           Data.Map                    (Map, insert, (!))
import qualified Data.Map                    as Map
import           System.Random               (StdGen, mkStdGen, random, randomR)

boardSize :: (Int, Int)
boardSize = (10, 10)

numMines :: Int
numMines = 15

-- Creating the shape of the buttons, the nested Lists will be used to detemine where the rows lie visually.
cellShape :: [[CellLocation]]
cellShape = [[(i, j) | i <- [1..fst boardSize]] | j <- [1..snd boardSize]]

-- The different states a cell can take on.
data Cell = Unexplored | Clear | Number Int | Bomb | Lose
  deriving (Show, Eq)

-- The coordinates of the cell in the grid/board.
type CellLocation = (Int, Int)
-- A mapping from cell location to cell state.
type CellMap =  Map CellLocation Cell

-- All the internal state that the game is keeping.
data GameState = GameState { cellMap           :: CellMap         -- Cell mapping to determine the state of cells.
                           , firstClick        :: Bool            -- Determines if the user clicked on a cell or not.
                           , genSeed           :: StdGen          -- Random number generator.
                           , userClickLocation :: CellLocation    -- Where the user has clicked for this game tick.
                           , bombGen           :: BombGenState }  -- Mine generation state.

-- Subset of the game state that is used to generate the mines.
-- Not sure if its a good idea to have bombGen as a record on it's own, as the amount of typing it caused felt like it wasn't worth it.
data BombGenState = BombGenState { bombsRemaining  :: Int             -- Count how many bombs have been planted so far.
                                 , cellsRemaining  :: Int             -- Count how many cells are left to potentially plant the bomb in.
                                 , currentLocation :: CellLocation }  -- Bomb planters current location.

-- Beggining values for the state.
startState :: GameState
startState = GameState
             { cellMap = Map.fromList $ map (, Unexplored) $ concat cellShape      -- Initialise the cells all with the Unexplored state.
             , firstClick = True
             , genSeed = mkStdGen 1                                  -- Non-IO seed creation. (IO transfomer not implemented in the setup function)
             , userClickLocation = (1,1)
             , bombGen = BombGenState { bombsRemaining = numMines
                                      , cellsRemaining = uncurry (*) boardSize - 1 -- Size of the board minus the first cell the user has clicked.
                                      , currentLocation = (1, 1) } } 

-- What should the cell display based on its state.
cellIcon :: Cell -> String
cellIcon Unexplored = "-"
cellIcon Clear      = " "
cellIcon Bomb       = "-"
cellIcon Lose       = ":("
cellIcon (Number a) = show a

main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup win = do
  -- buttons :: [[Element]]
  buttons <- mapM (mapM (const mkButton)) cellShape

  -- Keep track of which buttons belong to which cell location.
  -- buttonBindings :: [[(Element, CellLocation)]]
  let buttonBindings = zipWith zip buttons cellShape

  -- clicks :: Event CellLocation
  -- Assigns button clicks to fire the events that contain which button was clicked.
  let clicks = buttonClicks $ concat buttonBindings
  
  -- Assigns action for each click that uses the Events contained data (CellLocation)
  let commands = fmap triggerGame clicks
  
  -- Convert the events into a continuos stream that threads the state from one event to another.
  -- Give it the starting state.
  gridBehaviour <- accumB startState commands

  -- Assigning the the action the buttons should take when the state changes.
  finalButtons <- mapM (mapM (assignSink gridBehaviour)) buttonBindings

  pure win # set UI.title "Minesweeper"
  -- Attatch the buttons to the html document body
  void $ UI.getBody win
    #+ map (UI.row . map return) finalButtons

  where
    mkButton = UI.input
      # set (attr "type") "button"
      # set (attr "style") "width: 40px; height: 40px; font-size: 35px; line-height: 10px;"

    assignSink :: Behavior GameState -> (Element, CellLocation) -> UI Element
    assignSink behaviour (element, location) = pure element
      -- Make the element read some value from Behaviour via sink.
      # sink value (fmap (cellDisplay location) behaviour)

    -- Merge the different streams of events into one combined stream.
    buttonClicks :: [(Element, CellLocation)] -> Event CellLocation
    buttonClicks = foldr1 (UI.unionWith const) . map makeClick
      -- Using the <@ combinator to determine what the Event data should be for a click event on the element.
      where makeClick (e, l) = UI.pure l <@ UI.click e

    cellDisplay :: CellLocation -> GameState -> String
    cellDisplay l s = cellIcon $ cellMap s ! l

    -- We use runstate here to be able to handle the game logic inside the state monad while
    -- keeping the FRP portion out of the state monad.
    triggerGame :: CellLocation -> GameState -> GameState
    triggerGame l s = snd $ S.runState gameTick $ s { userClickLocation = l }


gameTick :: S.State GameState ()
gameTick = do
  fs <- S.gets firstClick
  l <- S.gets userClickLocation
  -- First click should generate bombs before revealing.
  when fs firstClickGenBombs
  revealCell l

-- Count the number of bombs in the surronding cells, display the number of bombs.
-- If a cell is opened with no bombs in its neighbours, all neighbour cells will open.

-- Not sure if there are other ways of writing this that improve the maintainability and readabiliy of the function.
revealCell :: CellLocation -> S.State GameState ()
revealCell l = do
  s <- S.get
  m <- S.gets cellMap
  when (m ! l /= Clear) $ do
    -- Here we update the cell state based on specific conditions.
    -- Done through S.put on the cellMap of the state.
    if m ! l == Bomb then S.put $ s { cellMap = insert l Lose m } else do
      let nBombs = neighbouringBombs l m
      if nBombs > 0 then S.put $ s { cellMap = insert l (Number nBombs) m } else do
        S.put $ s { cellMap = insert l Clear m }
        -- Apply reveal cell to all the neighbours of the current cell (l)
        mapM_ revealCell $ neighbours l

-- Calculate neighbours through list comprehensions rules.
neighbours :: CellLocation -> [CellLocation]
neighbours (i, j) = [(x+i, y+j) | x <- [-1..1], y <- [-1..1],
                     not (x == 0 && y == 0),
                     i + x >= 1,
                     j + y >= 1,
                     i + x <= fst boardSize,
                     j + y <= snd boardSize ]

-- Count how many bombs there are surrounding cell l.
neighbouringBombs :: CellLocation -> CellMap -> Int
neighbouringBombs l m = sum $ map (fromEnum . isBomb m) $ neighbours l

isBomb :: CellMap -> CellLocation -> Bool
-- Using the Data.Map accessing functions to retrieve the a cells state.
isBomb m l = m ! l == Bomb

-- Trigger bomb generation computation and set the firstClick state to false.
firstClickGenBombs :: S.State GameState ()
firstClickGenBombs = do genBombs; S.modify $ \s -> s { firstClick = False }

-- Overall, every cell has an equal chance of containing a bomb.
-- Except for the first cell that the player has clicked, which has a chance of 0.

-- A way of distributing the bombs randomly:
  -- Iterate through each cell:
    -- Each cell has remaining_bomb_count / possible_cells_for_bomb chance of containing a bomb.

-- Using if then else + state mangling feels very bad, how can this be done differently.
genBombs :: S.State GameState ()
genBombs = do
  b <- S.gets bombGen
  c <- S.gets $ currentLocation . bombGen
  br <- S.gets $ bombsRemaining . bombGen
  cr <- S.gets $ cellsRemaining . bombGen
  l <- S.gets userClickLocation
  m <- S.gets cellMap
  g <- S.gets genSeed

  -- When not out of bounds of the board.
  when (fst c <= fst boardSize) $ do
    if c == l then S.modify $ \s -> s { bombGen = b { currentLocation = nextLoc c } } else do

      let chanceCurrentCellBomb = fromIntegral br / fromIntegral cr
      let (numRoll, nextSeed) = randomR ((0,1)::(Float, Float)) g

      if numRoll > chanceCurrentCellBomb then S.modify $ \s -> s { cellMap = insert c Unexplored m }
        else S.modify $ \s -> s { cellMap = insert c Bomb m , bombGen = b { bombsRemaining = br - 1 }}

      S.modify $ \s -> s { genSeed = nextSeed , bombGen = (bombGen s) { cellsRemaining = cr - 1, currentLocation = nextLoc c } }
    genBombs

-- Only works for square grid.
nextLoc :: CellLocation -> CellLocation
nextLoc (i, j) = (i', j')
  where
    c = (i - 1) * fst boardSize + j
    c' = c + 1
    i' = div (c' - 1) (fst boardSize) + 1
    j' = mod (c' - 1) (fst boardSize) + 1
