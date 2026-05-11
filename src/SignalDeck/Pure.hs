module SignalDeck.Pure
  ( toRows
  , trim
  , parseMetric
  , parseMetrics
  , addSample
  , renderSparkline
  , renderDemoFrame
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

toRows :: String -> [String]
toRows = filter (not . null) . map trim . lines

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

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
    keepLastN windowSize ys = drop (max 0 (length ys - windowSize)) ys

renderSparkline :: [Double] -> String
renderSparkline [] = ""
renderSparkline xs =
  let chars = "▁▂▃▄▅▆▇█"
      lo = minimum xs
      hi = maximum xs
      levels = length chars - 1

      toIndex x
        | hi == lo = 0
        | otherwise = floor (((x - lo) / (hi - lo)) * fromIntegral levels)

      pick i = chars !! i
   in map (pick . toIndex) xs

renderDemoFrame :: [Double] -> String
renderDemoFrame ws =
  unlines
    [ "SignalDeck Demo"
    , ""
    , "latency_ms"
    , renderSparkline ws
    , ""
    , "current: " ++ show (if null ws then 0 else last ws)
    ]
