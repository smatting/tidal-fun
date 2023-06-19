module Chordvoicer (vchord, pnote, gscale, parse) where

import Data.List (takeWhile, findIndex, drop)
import Text.ParserCombinators.ReadPrec hiding (look)
import Text.ParserCombinators.ReadP (ReadP, look, char, string, sepBy, sepBy1, readS_to_P, eof)
import Text.Read hiding (look)
import Control.Applicative ((<|>), many)
import Data.Maybe (catMaybes)
import Data.Functor (($>), void)
import Data.Maybe (listToMaybe)
import Sound.Tidal.ParseBP (parseNote)
import qualified Text.Parsec as Parsec
import Sound.Tidal.Scales (scaleTable)
import qualified Sound.Tidal.Pattern as Tidal
import Sound.Tidal.Pattern (Pattern)

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
solveAddPos i t n =
  let d = t - i in
    if d <= 0 then d + n else d

-- Biggest k < 0
-- so that add(i, k, n) == t
solveAddNeg :: Int -> Int -> Int -> Int
solveAddNeg i t n =
  let d = t - i in
    if d >= 0 then d - n else d

noteAdd :: Note -> Int -> Int -> Int -> Note
noteAdd (Note i _ o) k n d =
  let ii = i - 1 + k
      dj = if ii < 0 then -1 else (if ii >= n then 1 else 0)
  in Note ((ii `mod` n) + 1) d (o + dj)

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
            foldr (\v (i, nt, nl) ->
                   if isDot v
                     then (i, nt, nl)
                   else
                     if isJump v
                       then (i, noteAdd nt (-n) n (vdelta v + delta rootNote), nl)
                     else
                       let k = solveAddNeg i (vidx v) n
                           i = vidx v
                           nt = noteAdd nt k n (vidx v + delta rootNote)
                       in (i, nt, nt:nl)
                   )
            (1, rootNote, [])
            (reverse vnlo)

      (_, _, notesHi) =
            foldr (\v (i, nt, nl) ->
                   if isDot v
                     then (i, nt, nl)
                   else
                     if isJump v
                       then (i, noteAdd nt n n (vdelta v + delta rootNote), nl)
                     else
                       let k = solveAddPos i (vidx v) n
                           i = vidx v
                           nt = noteAdd nt k n (vidx v + delta rootNote)
                       in (i, nt, nt:nl)
                   )
            (n, noteAdd rootNote (-1) n (delta rootNote), [])
            vnhi
  in (reverse notesLo) <> (reverse notesHi)

--
-- Notation:
-- 1:1-3-5
-- 1:5-.-1-3-5 # with dot .
-- 1:1-_-3-5   # octave jump _
-- 1s:1-3-5    # s is sharp => C#-maj.
-- 1b:1-3-5    # b/f is flat => Cb-maj.
-- 1:1-3b-5    # f is flat => C-min.


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

parseChord :: ReadP (Note, [VoiceNote])
parseChord = do
  rootv <- parseRootNote
  void $ string ":"
  vnotes <- sepBy parseVoiceNote (eof <|> void (string "-"))
  void eof
  pure (rootv, vnotes)

runParser :: ReadP a -> String -> Maybe a
runParser parser str = fst <$> listToMaybe (readPrec_to_S (readP_to_Prec (const parser)) 1 str)

parse :: String -> Maybe (Note, [VoiceNote])
parse = runParser parseChord

pnote :: String -> Maybe Int
pnote s = either (const Nothing) Just  $ Parsec.runParser parseNote 0 "" s

gscale :: String -> Maybe [Int]
gscale s = maybe Nothing (Just . map round) (lookup s scaleTable)

applyScale :: Int -> [Tidal.Note] -> Note -> Maybe Tidal.Note
applyScale n scale (Note idx d o) =
  let
    i = idx - 1
  in
    if 0 <= i && i < n then
      Just $ Tidal.Note $ Tidal.unNote (scale !! i) + fromIntegral d + fromIntegral o * 12
    else Nothing

vchord :: Pattern [Tidal.Note] -> Pattern String -> Pattern Tidal.Note
vchord scaleP voicesP = Tidal.uncollect $ do
   mvoices <- parse <$> voicesP
   scale <- scaleP
   let n = length scale
   case mvoices of
     Nothing -> pure []
     Just (rootNote, vs) ->
       let notes = voice rootNote vs n
       in pure $ catMaybes (map (applyScale n scale) notes)
