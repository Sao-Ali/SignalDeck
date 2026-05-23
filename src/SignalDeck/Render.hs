module SignalDeck.Render
  ( formatSample
  , renderSparkline
  , renderParseError
  , renderDeckFrame
  ) where

import SignalDeck.Model
  ( DeckState (..)
  , ParseError (..)
  , averageWindow
  , currentSample
  )
import Text.Printf (printf)

formatSample :: Double -> String
formatSample = printf "%.2f"

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

renderParseError :: ParseError -> String
renderParseError err =
  show (parseRawInput err) ++ " (" ++ parseMessage err ++ ")"

renderDeckFrame :: DeckState -> String
renderDeckFrame st =
  unlines $
    [ "SignalDeck"
    , ""
    , deckName st
    , if null (deckSamples st) then "waiting for samples..." else renderSparkline (deckSamples st)
    , ""
    , "current: " ++ maybe "n/a" formatSample (currentSample st)
    , "avg: " ++ maybe "n/a" formatSample (averageWindow st)
    , "samples: " ++ show (deckValidCount st)
    , "window: " ++ show (length (deckSamples st)) ++ "/" ++ show (deckWindowSize st)
    , "invalid rows: " ++ show (deckInvalidCount st)
    , "blank rows: " ++ show (deckBlankCount st)
    ]
      ++ lastErrorLine
  where
    lastErrorLine =
      case deckLastError st of
        Nothing -> []
        Just err -> ["last invalid: " ++ renderParseError err]
