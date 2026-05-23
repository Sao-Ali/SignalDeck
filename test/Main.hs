module Main (main) where

import Data.List (isInfixOf)
import SignalDeck.Model
  ( DeckState (..)
  , IngestResult (..)
  , ParseError (..)
  , addSample
  , averageWindow
  , currentSample
  , emptyDeckState
  , ingestLine
  , parseSample
  , stepDeckLines
  )
import SignalDeck.Render
  ( renderDeckFrame
  , renderSparkline
  )
import System.Exit (exitFailure)

type Test = (String, Bool)

main :: IO ()
main = do
  let failures = [name | (name, passed) <- tests, not passed]
  if null failures
    then putStrLn ("Passed " ++ show (length tests) ++ " tests.")
    else do
      putStrLn "Failing tests:"
      mapM_ (\name -> putStrLn ("- " ++ name)) failures
      exitFailure

tests :: [Test]
tests =
  [ ("parseSample accepts finite doubles", parseSample "12.5" == Right 12.5)
  , ("parseSample rejects invalid rows", parseSample "cpu=12.5" == Left (ParseError "cpu=12.5" "expected one numeric sample per line"))
  , ("ingestLine marks blank rows explicitly", ingestLine "  " == IngestedBlank)
  , ("ingestLine rejects non-finite samples", ingestLine "NaN" == IngestedInvalid (ParseError "NaN" "non-finite numeric sample"))
  , ("addSample keeps a rolling window", addSample 3 4 [1, 2, 3] == [2, 3, 4])
  , ("addSample handles non-positive sizes", addSample 0 4 [1, 2, 3] == [])
  , ("renderSparkline handles empty input", renderSparkline [] == "")
  , ("renderSparkline scales values", renderSparkline [0, 5, 10] == "▁▄█")
  , ("renderSparkline handles flat values", renderSparkline [7, 7, 7] == "▁▁▁")
  , ("stepDeckLines preserves rolling state and counts bad input", deckStateTest)
  , ("currentSample reads the latest value", currentSample sampleState == Just 4)
  , ("averageWindow uses the visible rolling window", averageWindow sampleState == Just 3)
  , ("renderDeckFrame reports invalid rows", "invalid rows: 1" `isInfixOf` renderDeckFrame sampleState)
  ]

sampleState :: DeckState
sampleState =
  stepDeckLines
    (emptyDeckState "stdin" 3)
    ["1", "", "oops", "2", "3", "4"]

deckStateTest :: Bool
deckStateTest =
  deckSamples sampleState == [2, 3, 4]
    && deckValidCount sampleState == 4
    && deckInvalidCount sampleState == 1
    && deckBlankCount sampleState == 1
    && deckLastError sampleState == Just (ParseError "oops" "expected one numeric sample per line")
