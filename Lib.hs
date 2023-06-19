import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (void, forM_)

:{
splitAt :: Char -> String -> [String]
splitAt c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> w : splitAt c s''
          where (w, s'') = break (== c) s'

drums :: Pattern String -> ControlPattern
drums p = note (fmap drums2Note p)

drums2Note :: String -> Note
drums2Note s =
  let (name, bank) =
        case splitAt ':' s of
          [name] -> (name, 1)
          [name, bank] -> (name, fromMaybe 1 (readMaybe bank))
          _ -> ("bd", 1)
      baseNote = (-25 +) <$> case name of
        "~" -> Nothing
        "bd" -> Just 1
        "sn" -> Just 2
        "sd" -> Just 2
        "oh" -> Just 3
        "hh" -> Just 4

        "ma" -> Just 5 -- marachas
        "to" -> Just 6 -- tom (low)
        "tl" -> Just 6 -- tom low
        "tm" -> Just 7 -- tom mid
        "th" -> Just 8 -- tom high

        "cl" -> Just 9 -- clap / clave
        "cy" -> Just 10 -- cymbal
        "pc" -> Just 11 -- percussion
        "rs" -> Just 12 -- rim shot

        "cb" -> Just 13 -- cow bell
        "co" -> Just 14 -- conga (low)
        "cm" -> Just 15 -- conga mid
        "ch" -> Just 16 -- conga hi
        _ -> Nothing
  in Note $ maybe (-1) (\bn -> (bn + (bank - 1) * 16)) baseNote

setupCCV :: Int -> IO ()
setupCCV midiChannel = do
  forM_ [1..8] $ \(i :: Int) -> do
    putStrLn $ "Please select knob " <> show i <> " and press ENTER."
    void $ getLine
    once $ ccv 64 # ccn (fromIntegral (30 + i - 1)) # m (pure (fromIntegral midiChannel))
:}
