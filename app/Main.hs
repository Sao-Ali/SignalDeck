module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import SignalDeck.Pure (addSample, parseMetrics, renderDemoFrame, toRows)


usage :: String
usage = unlines ["Usage: ", " signaldeck demo", " signal --help", " signaldeck file <path> (coming later)"]

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
