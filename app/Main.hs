module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.State.Strict (modify)
import System.Random (randomRIO)
import System.IO (BufferMode (NoBuffering), hSetBuffering, isEOF, stdout)
import SignalDeck.Model
  ( DeckState (..)
  , IngestResult (..)
  , averageWindow
  , currentSample
  , defaultWindowSize
  , emptyDeckState
  , stepDeck
  , stepDeckLine
  , stepDeckLines
  )
import SignalDeck.Render
  ( formatSample
  , renderDeckFrame
  , renderSparkline
  )
import Brick (App(..), BrickEvent(..), EventM, Widget, attrMap, showFirstCursor, str)
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as BM
import qualified Graphics.Vty as V

data Name = Name deriving (Eq, Ord, Show)

data TuiEvent
  = NewSample Double
  deriving (Show)

data TuiState = TuiState
  { paused :: Bool
  , tuiDeck :: DeckState
  }

usage :: String
usage =
  unlines
    [ "Usage:"
    , " signaldeck tui"
    , " signaldeck demo"
    , " signaldeck file <path>"
    , " signaldeck --help"
    , ""
    , "No arguments reads live numeric samples from stdin."
    , "Example: some_pipeline | signaldeck"
    ]

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
      processFile ("file: " ++ path) contents

    ["tui"] ->
      runTui


    ["file"] -> do
      putStrLn "Error: missing path after 'file'"
      putStrLn usage
      exitFailure

    [] -> do
      runStdinStream

    _ -> do 
      putStrLn ("Error: Invalid arguments: " ++ show args ++ "\n\n" ++ usage)

processFile :: String -> String -> IO ()
processFile source raw = do
  let initialState = emptyDeckState source defaultWindowSize
  let finalState = stepDeckLines initialState (lines raw)
  putStr (renderDeckFrame finalState)

runStdinStream :: IO ()
runStdinStream = do
  hSetBuffering stdout NoBuffering
  streamLoop (emptyDeckState "stdin" defaultWindowSize)

streamLoop :: DeckState -> IO ()
streamLoop st = do
  done <- isEOF
  if done
    then pure ()
    else do
      raw <- getLine
      let st' = stepDeckLine st raw
      clearScreen
      putStr (renderDeckFrame st')
      streamLoop st'

runDemo :: IO()
runDemo = demoLoop (emptyDeckState "latency_ms" defaultWindowSize)

clearScreen :: IO()
clearScreen = putStrLn "\ESC[2J\ESC[H"

demoLoop :: DeckState -> IO ()
demoLoop st = do
  sample <- randomRIO (20.0, 80.0)
  let st' = stepDeck st (IngestedSample sample)
  clearScreen
  putStr (renderDeckFrame st')
  threadDelay 1000000
  demoLoop st'

runTui :: IO()
runTui = do
  eventChan <- newBChan 10
  void . forkIO . forever $ do
    sample <- randomRIO (20.0, 80.0)
    writeBChan eventChan (NewSample sample)
    threadDelay 1000000
  let initialState =
        TuiState
          { paused = False
          , tuiDeck = emptyDeckState "demo: latency_ms" defaultWindowSize
          }
  _ <- BM.customMainWithDefaultVty (Just eventChan) tuiApp initialState
  pure ()

tuiApp :: App TuiState TuiEvent Name
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
  [ str (renderTuiFrame st) ]

renderTuiFrame :: TuiState -> String
renderTuiFrame st =
  unlines
    [ "SignalDeck TUI (experimental)"
    , ""
    , "[q] quit [esc] quit [space] pause/resume"
    , "status: " ++ if paused st then "paused" else "running"
    , ""
    , "source: " ++ deckName deck
    , if null (deckSamples deck) then "waiting for samples..." else renderSparkline (deckSamples deck)
    , ""
    , "current: " ++ maybe "n/a" formatSample (currentSample deck)
    , "avg: " ++ maybe "n/a" formatSample (averageWindow deck)
    , "samples: " ++ show (deckValidCount deck)
    , "window: " ++ show (length (deckSamples deck)) ++ "/" ++ show (deckWindowSize deck)
    , "invalid rows: " ++ show (deckInvalidCount deck)
    , "blank rows: " ++ show (deckBlankCount deck)
    ]
  where
    deck = tuiDeck st

handleTuiEvent :: BrickEvent Name TuiEvent -> EventM Name TuiState ()
handleTuiEvent (AppEvent (NewSample sample)) =
  modify addTuiSample
  where
    addTuiSample st
      | paused st = st
      | otherwise =
          st { tuiDeck = stepDeck (tuiDeck st) (IngestedSample sample) }
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = BM.halt
handleTuiEvent (VtyEvent (V.EvKey V.KEsc [])) = BM.halt
handleTuiEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  modify (\st -> st { paused = not (paused st) })
handleTuiEvent _ = pure ()
