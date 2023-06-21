module Chordvoicer  where

import Control.Applicative ((<|>), many)
import Data.Functor (($>), void)
import Data.List (takeWhile, findIndex, drop, break, elem)
import Data.Maybe (listToMaybe, catMaybes, fromJust, mapMaybe)
import Debug.Trace (traceM, trace)
import Data.String
import Data.Foldable (foldl')
import Sound.Tidal.Core
import Sound.Tidal.ParseBP as Tidal (parseNote)
import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Scales (scaleTable)
import Sound.Tidal.Show
import Sound.Tidal.Simple
import Text.ParserCombinators.ReadP (ReadP, look, char, string, sepBy, sepBy1, readS_to_P, eof, manyTill, get)
import Text.ParserCombinators.ReadPrec hiding (look, get)
import Text.Read hiding (look, get)
import qualified Sound.Tidal.Pattern as Tidal
import qualified Text.Parsec as Parsec

--
-- chord voicer core
--

-- A note within an implied scale idx=1 first note of the scale, idx=2 second note
-- deltas are semitones to go off-scale
data Note = Note {
  idx :: Int,
  delta :: Int,
  octave :: Int
}
  deriving (Show, Eq)

-- Same as Note but module octave
data VoiceNote = VoiceNote {
  vidx :: Int,
  vdelta :: Int
}
  deriving (Show, Eq)

dot = VoiceNote 0 0

isDot :: VoiceNote -> Bool
isDot v = v == dot

jump = VoiceNote 0 1

isJump :: VoiceNote -> Bool
isJump v = v == jump

-- i = 1..n
-- k int
add :: Int -> Int -> Int -> Int
add i k n =
  ((i - 1 + k) `mod` n) + 1

-- Smallest k > 0
-- so that add i k n == t
solveAddPos :: Int -> Int -> Int -> Int
solveAddPos n i t =
  let d = t - i in
    if d <= 0 then d + n else d

-- Biggest k < 0
-- so that add(i, k, n) == t
solveAddNeg :: Int -> Int -> Int -> Int
solveAddNeg n i t =
  let d = t - i in
    if d >= 0 then d - n else d

noteAdd :: Int -> Note -> Int -> Int -> Note
noteAdd n (Note i _ o) k d =
  let ii = i - 1 + k
      dj | ii < 0    = -1
         | ii >= n   = 1
         | otherwise = 0
  in Note (add i k n) d (o + dj)

splitlohi :: [VoiceNote] -> ([VoiceNote], [VoiceNote])
splitlohi vnotes =
  case findIndex isDot vnotes of
    Nothing -> ([], vnotes)
    Just i ->
      (take i vnotes, drop (i + 1) vnotes)

voice :: Note -> [VoiceNote] -> Int -> [Note]
voice rootNote vnotes n =
  let (vnlo, vnhi) = splitlohi vnotes
      (_, _, notesLo) =
            foldl' (\(i, nt, nl) v ->
                   if isDot v
                     then (i, nt, nl)
                   else
                     if isJump v
                       then (i, noteAdd n nt (-n) (vdelta v + delta rootNote), nl)
                     else
                       let k = solveAddNeg i (vidx v) n
                           i' = vidx v
                           nt' = noteAdd n nt k (vdelta v + delta rootNote)
                       in (i', nt', nt':nl)
                   )
            (1, rootNote, [])
            (reverse vnlo)

      (_, _, notesHi) =
            foldl' (\(i, nt, nl) v ->
                   if isDot v
                     then (i, nt, nl)
                   else
                     if isJump v
                       then (i, noteAdd n nt n (vdelta v + delta rootNote), nl)
                     else
                       let k = solveAddPos n i (vidx v)
                           i' = vidx v
                           nt' = noteAdd n nt k (vdelta v + delta rootNote)
                       in (i', nt', nt':nl)
                   )
            (n, noteAdd n rootNote (-1) (delta rootNote), [])
            vnhi
  in reverse notesLo <> reverse notesHi

---
--- Parsers
---

parsePositive :: ReadP Int
parsePositive = do
  -- n :: Int <- readS_to_P reads
  n :: Int <- readPrec_to_P readPrec 1
  if n > 0
    then pure n
    else fail "expected positive number"

parseInt :: ReadP Int
parseInt = do
  n :: Int <- readPrec_to_P readPrec 1
  pure n

parseVoiceNote :: ReadP VoiceNote
parseVoiceNote = do
  n :: Int <- parsePositive
  sf <- sum <$> many ((char 's' $> 1) <|> (char 'f' $> (-1)))
  pure $ VoiceNote n sf

parseRootNote :: ReadP Note
parseRootNote = do
  n :: Int <- parseInt
  sf <- sum <$> many ((char 's' $> 1) <|> (char 'f' $> (-1)))
  ahead <- look
  o <- case ahead of
    ('.':rs) -> char '.' >> parseInt
    _ -> pure 0
  pure $ Note n sf o

parseToken :: ReadP VoiceNote
parseToken =
  parseVoiceNote <|> string "." $> dot <|> string "_" $> jump

-- Notation:
-- 1:1-3-5
-- 1:5-.-1-3-5 # with dot .
-- 1:1-_-3-5   # octave jump _
-- 1s:1-3-5    # s is sharp => C#-maj.
-- 1b:1-3-5    # b/f is flat => Cb-maj.
-- 1:1-3b-5    # f is flat => C-min.
parseChord :: ReadP (Note, [VoiceNote])
parseChord = do
  rootv <- parseRootNote
  void $ string ":"
  vnotes <- sepBy parseVoiceNote (eof <|> void (string "-"))
  void eof
  pure (rootv, vnotes)

-- Notation:
-- c5:ionian
-- ionian
parseScale :: ReadP (Maybe Tidal.Note, [Tidal.Note])
parseScale = do
  ahead <- look
  note <- if ':' `elem` ahead
    then do
        unparsedNote <- manyTill get (char ':')
        Just <$> readJust ("Could not parse Tidal note from: " <> unparsedNote) (pnote unparsedNote)
    else
        pure Nothing
  unparsedScaleName <- look
  notes <- readJust "" (lookup unparsedScaleName scaleTable)
  pure (note, notes)
  where

    readJust :: String -> Maybe a -> ReadP a
    readJust msg Nothing = fail msg
    readJust _msg (Just x) = pure x

pnote :: String -> Maybe Tidal.Note
pnote s = either (const Nothing) Just  $ Parsec.runParser Tidal.parseNote 0 "" s

--
-- splitString ':' "hi:world"
-- ("hi","world")
--
-- splitString ':' "hiworld"
-- ("hiworld","")
--
-- splitString ':' "hi:wor:ld"
-- ("hi","wor:ld")
splitString :: Char -> String -> (String, String)
splitString sep s =
  let (a, b) = break (== sep) s
  in (a, dropWhile (== sep) b)


runParser :: ReadP a -> String -> Maybe a
runParser parser str = fst <$> listToMaybe (readPrec_to_S (readP_to_Prec (const parser)) 1 str)


parse :: String -> Maybe (Note, [VoiceNote])
parse = runParser parseChord

---
--- chordvoicer tidal integration
---

mkscale :: Pattern String -> Pattern [Tidal.Note]
mkscale = fmap (\s ->
                  case runParser parseScale s of
                    Nothing -> []
                    Just (Just baseNote, notes) -> fmap (+ baseNote) notes
                    Just (Nothing, notes) -> notes
               )

applyScale :: Int -> [Tidal.Note] -> Note -> Maybe Tidal.Note
applyScale n scale (Note idx d o) =
  let
    i = idx - 1
  in
    if 0 <= i && i < n then
      Just $ Tidal.Note $ Tidal.unNote (scale !! i) + fromIntegral d + fromIntegral o * 12
    else Nothing

inscale :: Pattern [Tidal.Note] -> Pattern String -> Pattern [Tidal.Note]
inscale scaleP voicesP = do
  -- traceM ("inscale: ")
  mvoices <- parse <$> voicesP
  scale <- scaleP
  -- traceM ("inscale: scale: " <> show scale)
  let n = length scale
  case mvoices of
    Nothing -> pure []
    Just (rootNote, vs) ->
      let notes = voice rootNote vs n
      in pure $ mapMaybe (applyScale n scale) notes

-- Tests

checkTidalToken :: String -> Maybe String
checkTidalToken input =
  let pat = fromString input :: Pattern String
      s = head (lines (show pat))
      o = init $ drop 1 $ snd $ splitString '|' s
  in if input == o then Nothing else Just o

testIsTidalToken :: String -> IO ()
testIsTidalToken input =
  case checkTidalToken input of
    Nothing -> pure ()
    Just s -> let msg = "\"" <> input <> "\"" <> "is not a token. Parsed: " <> s
              in assertBool msg False

assertBool :: String -> Bool -> IO ()
assertBool msg True = pure ()
assertBool msg False = error $ "\nAssertion failure:\n" <> msg

testParse :: (Eq a, Show a) => ReadP a -> String -> a -> IO ()
testParse parser input expected =
  case runParser parser input of
    Nothing -> assertBool ("Failed to parse: " <> input) False
    Just x -> let msg = ("Failed to parse correctly: " <> input <> "\nExpected: " <> show expected <> "\nBut got : " <> show x <> "\n")
              in assertBool msg (x == expected)

testChordVoicer :: Int -> Note -> [VoiceNote] ->  [Note] -> IO ()
testChordVoicer n baseNote vs expected = do
  let actual = voice baseNote vs n
      msg = "Expected: " <> show expected <> "\nBug got : " <> show actual <> "\n"
  assertBool msg (expected == actual)

tests :: IO ()
tests = do
  testIsTidalToken "1"
  testIsTidalToken "1s"
  testIsTidalToken "1f"
  testIsTidalToken "1.2"
  testIsTidalToken "1.-1"
  testIsTidalToken "1s.-1"
  testIsTidalToken "1:1-3-5"
  testIsTidalToken "1:1s-3-5"
  testIsTidalToken "1.2:1s-3-5"
  testIsTidalToken "1s.2:1s-3-5"
  testIsTidalToken "1s.2:1s-.-5"
  testIsTidalToken "1.1-.-5"
  testIsTidalToken "1.1-_-5"
  testParse parseChord "1:1-3-5" (Note 1 0 0, [VoiceNote 1 0, VoiceNote 3 0,  VoiceNote 5 0])
  testParse parseScale "c4:ionian" (Just (fromJust (pnote "c4") :: Tidal.Note),  [0, 2, 4, 5, 7, 9, 11] :: [Tidal.Note])
  testParse parseScale "ionian" (Nothing,  [0, 2, 4, 5, 7, 9, 11] :: [Tidal.Note])

  testChordVoicer
         7
        (Note 1 0 0)
        [VoiceNote 1 0, VoiceNote 3 0, VoiceNote 5 0]
        [Note 1 0 0, Note 3 0 0, Note 5 0 0]

  testChordVoicer
         7
        (Note 1 1 0)
        [VoiceNote 1 (-1), VoiceNote 3 1, VoiceNote 5 0]
        [Note 1 0 0, Note 3 2 0, Note 5 1 0]
