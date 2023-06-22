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
import Text.ParserCombinators.ReadP (ReadP, look, char, string, sepBy, sepBy1, readS_to_P, eof, manyTill, get, choice)
import Text.ParserCombinators.ReadPrec (readPrec_to_P, readPrec_to_S, readP_to_Prec)
import Text.Read (readPrec)
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

-- idx is 0-based! So 0 is the first note of the scale
data AbsVoice = AbsVoice {
  absVidx :: Int,
  absVdelta :: Int
}
  deriving (Show, Eq)

dot = VoiceNote 0 0

isDot :: VoiceNote -> Bool
isDot v = v == dot

jump = VoiceNote 0 1

isJump :: VoiceNote -> Bool
isJump v = v == jump

data Voicing
  = Stacked Note [VoiceNote]
  | Absolute AbsVoice
  deriving (Eq, Show)

-- i = 1..n
-- k int
add :: Int -> Int -> Int -> Int
add i k n =
  -- same as (ii `mod` n) + 1
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

-- n = 3
-- k = (-3)
-- i = 1
-- dj `shouldBe` -1
-- ii = new index, but 0-based
-- ii = -3
--
-- ii =  3, dj = 1
-- ii =  2, dj = 0
-- ii =  1, dj = 0
-- ii =  0, dj = 0
-- ii = -1, dj = -1, (-ii-1) = 0
-- ii = -2, dj = -1, (-ii-1) = 1
-- ii = -3, dj = -1, (-ii-1) = 2
-- ii = -4, dj = -2

--

noteAddAbs :: Int -> Note -> Int -> Int -> Note
noteAddAbs n (Note i _ o) k d =
  let ii = i - 1 + k
      dj | ii < 0    = -(((-ii-1) `div` n) + 1)
         | ii >= n   = (ii `div` n)
         | otherwise = 0
  in Note (add i k n) d (o + dj)

splitlohi :: [VoiceNote] -> ([VoiceNote], [VoiceNote])
splitlohi vnotes =
  case findIndex isDot vnotes of
    Nothing -> ([], vnotes)
    Just i ->
      (take i vnotes, drop (i + 1) vnotes)

voice :: Int -> Note -> [VoiceNote] -> [Note]
voice n rootNote vnotes =
  let (vnlo, vnhi) = splitlohi vnotes
      (_, _, notesLo) =
            foldl' (\(i, nt, nl) v ->
                   if isDot v
                     then (i, nt, nl)
                   else
                     if isJump v
                       then (i, noteAdd n nt (-n) (vdelta v + delta rootNote), nl)
                     else
                       let
                           i' = vidx v
                           k = solveAddNeg n i i'
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
                       let i' = vidx v
                           k = solveAddPos n i i'
                           nt' = noteAdd n nt k (vdelta v + delta rootNote)
                       in (i', nt', nt':nl)
                   )
            (n, noteAdd n rootNote (-1) (delta rootNote), [])
            vnhi
  in notesLo <> reverse notesHi

voiceAbsolute :: Int -> AbsVoice -> Note
voiceAbsolute n v = noteAddAbs n (Note 1 0 0) (absVidx v) (absVdelta v)

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

parseAbsVoice :: ReadP AbsVoice
parseAbsVoice = do
  idx :: Int <- parseInt
  sf <- sum <$> many ((char 's' $> 1) <|> (char 'f' $> (-1)))
  void eof
  pure $ AbsVoice idx sf

parseToken :: ReadP VoiceNote
parseToken =
  parseVoiceNote <|> string "." $> dot <|> string "_" $> jump

parseChord :: ReadP (Note, [VoiceNote])
parseChord = do
  rootv <- parseRootNote
  void $ string ":"
  vnotes <- sepBy parseVoiceNote (eof <|> void (string "-"))
  void eof
  pure (rootv, vnotes)

parseVoicing :: ReadP Voicing
parseVoicing =
  choice [
    uncurry Stacked <$> parseChord,
    Absolute <$> parseAbsVoice
  ]

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


-- Notation for scales
--
-- ionian     # c5-ionian
-- f:ionian   # 55-ionian
-- see `scaleTable` for name of scales
--
-- Notation for voicings:
--
-- Chords (with interpretation in c:ionian scale):
-- 1:1-3-5       # cmajor chord
-- 1:5-.-1-3-5   # cmajor chord with a g added in the bass
-- 1:1-5-.-1-3-5 # cmajor chord with a c and g added in the bass
-- 5:1-3-5       # gmajor chord
-- 1:1-_-3-5     # octave jumps. between c5 e6 g6
-- 1s:1-3-5      # s is sharp => C#-maj.
-- 1f:1-3-5      # b/f is flat => Cb-maj.
-- 1:1-3f-5      # f is flat => C-min.
-- 1:9-5-7       # is the same as 1:2-5-7
--
-- Absolute scale picks (good for arpeggios)
-- As "scale" provide a chord here, e.g. 1:1-3-5 (c, e, g)
-- (-3)          # e4
-- (-2)          # e4
-- (-1)          # g4
-- 0             # c5
-- 1             # e5
-- 2             # g5
-- 3             # c6
-- 4             # e6
-- 5             # g6
-- 0s            # c#4
-- 0f            # cb4
inscale ::
  -- | scale
  Pattern String ->
  -- | voicings
  Pattern String ->
  Pattern Tidal.Note
inscale scaleP = inscale' (mkscale scaleP)

inscale' :: Pattern [Tidal.Note] -> Pattern String -> Pattern Tidal.Note
inscale' scaleP voicesP = Tidal.uncollect $ do
  scale <- scaleP
  mvoicing <- runParser parseVoicing <$> voicesP
  let n = length scale
  if n == 0
    then pure []
    else
      let notes =
           case mvoicing of
             Nothing -> []
             Just (Stacked rootNote vs) -> voice n rootNote vs
             Just (Absolute v) -> [ voiceAbsolute n v ]
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
  let actual = voice n baseNote vs
      msg = "Expected: " <> show expected <> "\nBug got : " <> show actual <> "\n"
  assertBool msg (expected == actual)

testAbsoluteVoice :: Int -> AbsVoice -> Note -> IO ()
testAbsoluteVoice n v expected =
  let actual = voiceAbsolute n v
  in if actual == expected
        then pure ()
        else assertBool ("Expected: " <> show expected <> "\nBug got : " <> show actual <> "\n") False

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
  testParse parseChord "2:1-3-5" (Note 2 0 0, [VoiceNote 1 0, VoiceNote 3 0,  VoiceNote 5 0])
  testParse parseChord "2s:1-3-5" (Note 2 1 0, [VoiceNote 1 0, VoiceNote 3 0,  VoiceNote 5 0])
  testParse parseChord "2f:1-3-5" (Note 2 (-1) 0, [VoiceNote 1 0, VoiceNote 3 0,  VoiceNote 5 0])
  testParse parseChord "2.-1:1-3-5" (Note 2 0 (-1), [VoiceNote 1 0, VoiceNote 3 0,  VoiceNote 5 0])

  testParse parseVoicing "3" (Absolute (AbsVoice 3 0))
  testParse parseVoicing "0" (Absolute (AbsVoice 0 0))
  testParse parseVoicing "0s" (Absolute (AbsVoice 0 1))
  testParse parseVoicing "0f" (Absolute (AbsVoice 0 (-1)))

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

  testChordVoicer
         7
        (Note 1 0 0)
        [VoiceNote 5 0, dot, VoiceNote 1 0, VoiceNote 3 0]
        [Note 5 0 (-1), Note 1 0 0, Note 3 0 0]

  testChordVoicer
         7
        (Note 1 0 0)
        [VoiceNote 7 0, VoiceNote 5 0, dot, VoiceNote 1 0, VoiceNote 3 0]
        [Note 7 0 (-2), Note 5 0 (-1), Note 1 0 0, Note 3 0 0]

  testChordVoicer
         7
        (Note 5 0 0)
        [VoiceNote 1 0, VoiceNote 3 0, VoiceNote 5 0]
        [Note 5 0 0, Note 7 0 0, Note 2 0 1]

  testChordVoicer
         7
        (Note 1 0 0)
        [VoiceNote 1 0, jump, VoiceNote 3 0]
        [Note 1 0 0, Note 3 0 1]


  testAbsoluteVoice 3 (AbsVoice (-6) 0) (Note 1 0 (-2))
  testAbsoluteVoice 3 (AbsVoice (-5) 0) (Note 2 0 (-2))
  testAbsoluteVoice 3 (AbsVoice (-4) 0) (Note 3 0 (-2))

  testAbsoluteVoice 3 (AbsVoice (-3) 0) (Note 1 0 (-1))
  testAbsoluteVoice 3 (AbsVoice (-2) 0) (Note 2 0 (-1))
  testAbsoluteVoice 3 (AbsVoice (-1) 0) (Note 3 0 (-1))

  testAbsoluteVoice 3 (AbsVoice 0 0) (Note 1 0 0)
  testAbsoluteVoice 3 (AbsVoice 1 0) (Note 2 0 0)
  testAbsoluteVoice 3 (AbsVoice 2 0) (Note 3 0 0)

  testAbsoluteVoice 3 (AbsVoice 3 0) (Note 1 0 1)
  testAbsoluteVoice 3 (AbsVoice 4 0) (Note 2 0 1)
  testAbsoluteVoice 3 (AbsVoice 5 0) (Note 3 0 1)

  testAbsoluteVoice 3 (AbsVoice 0 1) (Note 1 1 0)
  testAbsoluteVoice 3 (AbsVoice 0 (-1)) (Note 1 (-1) 0)
