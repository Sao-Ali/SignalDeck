module Main where

import System.Environment (getArgs)
import Data.Char (isSpace)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)


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
  putStrLn ("Read " ++ show (length rows) ++ " rows")

toRows :: String -> [String]
toRows = 
  filter (not . null) . map trim . lines

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

parseMetrics :: [String] -> [Double]
parseMetrics = mapMaybe parseMetric

parseMetric :: String -> Maybe Double
parseMetric = readMaybe

addSample :: Int -> Double -> [Double] -> [Double]
addSample size n xs
  | size <= 0 = []
  | otherwise = keepLastN size (xs ++ [n])
    where 
      keepLastN :: Int -> [Double] -> [Double]
      keepLastN size ys = drop (max 0 (length ys - size)) ys 

renderSparkline :: [Double] -> String
renderSparkline [] = ""
renderSparkline xs = 
  let chars = "▁▂▃▄▅▆▇█"
      lo = minimum xs
      hi = maximum xs
      levels = length chars - 1

      toIndex x 
        | hi == lo = 0
        | otherwise = floor (((x-lo) /(hi -lo)) * fromIntegral levels)

      pick i = chars !! i
  in map (pick . toIndex) xs

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

renderDemoFrame :: [Double] -> String
renderDemoFrame ws = unlines [ "SignalDeck Demo", "", "latency_ms", renderSparkline ws, "", "current: " ++ show (if null ws then 0 else last ws)]
