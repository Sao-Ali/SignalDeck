module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Concurrent (threadDelay)
import Control.Monad.State.Strict (modify)
import System.Random (randomRIO)
import SignalDeck.Pure (addSample, parseMetrics, renderDemoFrame, toRows)
import Brick (App(..), BrickEvent(..), EventM, Widget, attrMap, showFirstCursor, str)
import qualified Brick.Main as BM
import qualified Graphics.Vty as V

data Name = Name deriving (Eq, Ord, Show)

data TuiState = TuiState
  { paused :: Bool
  }

usage :: String
usage = unlines ["Usage: ","signaldeck tui", " signaldeck demo", " signaldeck --help", " signaldeck file <path> (coming later)"]

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["demo"] ->
      runDemo

    ["--help"] -> 
      putStrLn usage

    ["file", path] -> do
      contents <- readFile path
      processFile contents

    ["tui"] ->
      runTui


    ["file"] -> do
      putStrLn "Error: missing path after 'file'"
      putStrLn usage
      exitFailure

    [] -> do
      contents <- getContents
      processFile contents

    _ -> do 
      putStrLn ("Error: Invalid arguments: " ++ show args ++ "\n\n" ++ usage)

processFile :: String -> IO ()
processFile raw = do
  let rows = toRows raw
  let metrics = parseMetrics rows
  let badCount = length rows - length metrics
  putStrLn ("Read " ++ show (length rows) ++ " rows (" ++ show badCount ++ " invalid)")

runDemo :: IO()
runDemo = demoLoop []

clearScreen :: IO()
clearScreen = putStrLn "\ESC[2J\ESC[H"

demoLoop :: [Double] -> IO ()
demoLoop window = do
  sample <- randomRIO (20.0, 80.0)
  let window' = addSample 20 sample window
  clearScreen
  putStrLn (renderDemoFrame window')
  threadDelay 1000000
  demoLoop window'

runTui :: IO()
runTui = do
  let initialState = TuiState { paused = False }
  _ <- BM.defaultMain tuiApp initialState
  pure ()

tuiApp :: App TuiState e Name
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure ()
    , appAttrMap = const (attrMap V.defAttr [])
    }

drawTui :: TuiState -> [Widget Name]
drawTui st =
  [ str $
      unlines
        [ "SignalDeck TUI (Stage 8)"
        , ""
        , "[q] quit [space] pause/resume"
        , "status: " ++ if paused st then "paused" else "running"
        ]
  ]

handleTuiEvent :: BrickEvent Name e -> EventM Name TuiState ()
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = BM.halt
handleTuiEvent (VtyEvent (V.EvKey V.KEsc [])) = BM.halt
handleTuiEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  modify (\st -> st { paused = not (paused st) })
handleTuiEvent _ = pure ()
