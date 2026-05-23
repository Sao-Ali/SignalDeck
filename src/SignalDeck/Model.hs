module SignalDeck.Model
  ( ParseError (..)
  , IngestResult (..)
  , DeckState (..)
  , defaultWindowSize
  , emptyDeckState
  , parseSample
  , ingestLine
  , stepDeck
  , stepDeckLine
  , stepDeckLines
  , addSample
  , currentSample
  , averageSample
  , averageWindow
  ) where

import Data.Char (isSpace)
import qualified Data.List as List
import Text.Read (readMaybe)

data ParseError = ParseError
  { parseRawInput :: String
  , parseMessage :: String
  }
  deriving (Eq, Show)

data IngestResult
  = IngestedSample Double
  | IngestedBlank
  | IngestedInvalid ParseError
  deriving (Eq, Show)

data DeckState = DeckState
  { deckName :: String
  , deckWindowSize :: Int
  , deckSamples :: [Double]
  , deckValidCount :: Int
  , deckInvalidCount :: Int
  , deckBlankCount :: Int
  , deckLastError :: Maybe ParseError
  }
  deriving (Eq, Show)

defaultWindowSize :: Int
defaultWindowSize = 30

emptyDeckState :: String -> Int -> DeckState
emptyDeckState name size =
  DeckState
    { deckName = name
    , deckWindowSize = max 0 size
    , deckSamples = []
    , deckValidCount = 0
    , deckInvalidCount = 0
    , deckBlankCount = 0
    , deckLastError = Nothing
    }

parseSample :: String -> Either ParseError Double
parseSample raw =
  let cleaned = trim raw
   in case readMaybe cleaned of
        Just value
          | isNaN value || isInfinite value ->
              Left (ParseError raw "non-finite numeric sample")
          | otherwise ->
              Right value
        Nothing ->
          Left (ParseError raw "expected one numeric sample per line")

ingestLine :: String -> IngestResult
ingestLine raw
  | null (trim raw) = IngestedBlank
  | otherwise =
      case parseSample raw of
        Right value -> IngestedSample value
        Left err -> IngestedInvalid err

stepDeck :: DeckState -> IngestResult -> DeckState
stepDeck st result =
  case result of
    IngestedSample value ->
      st
        { deckSamples = addSample (deckWindowSize st) value (deckSamples st)
        , deckValidCount = deckValidCount st + 1
        }
    IngestedBlank ->
      st { deckBlankCount = deckBlankCount st + 1 }
    IngestedInvalid err ->
      st
        { deckInvalidCount = deckInvalidCount st + 1
        , deckLastError = Just err
        }

stepDeckLine :: DeckState -> String -> DeckState
stepDeckLine st = stepDeck st . ingestLine

stepDeckLines :: DeckState -> [String] -> DeckState
stepDeckLines = List.foldl' stepDeckLine

addSample :: Int -> Double -> [Double] -> [Double]
addSample size n xs
  | size <= 0 = []
  | otherwise = keepLastN size (xs ++ [n])
  where
    keepLastN :: Int -> [Double] -> [Double]
    keepLastN windowSize ys = drop (max 0 (length ys - windowSize)) ys

currentSample :: DeckState -> Maybe Double
currentSample st =
  case deckSamples st of
    [] -> Nothing
    xs -> Just (last xs)

averageSample :: [Double] -> Maybe Double
averageSample [] = Nothing
averageSample xs = Just (sum xs / fromIntegral (length xs))

averageWindow :: DeckState -> Maybe Double
averageWindow = averageSample . deckSamples

trim :: String -> String
trim = List.dropWhileEnd isSpace . dropWhile isSpace
