module Music.Minstrel.Intervals
    ( Interval
    , octave
    , tone
    , semitone
    , major, major'
    , perfect, perfect'
    , minor, minor'
    , aug, aug'
    , dim, dim'
    , interval, intervalN
    , to, toN
    , shift, shiftN
    ) where

import Music.Minstrel.Notes
import Data.Maybe (fromJust)
import Control.Applicative

-- | An interval is a number of semitones between two notes.
type Interval = Int

octave = perfect' 8

tone :: Interval
tone = 2

semitone :: Interval
semitone = 1

-- | Major interval. Only major intervals are second, third, sixth, and seventh.
major :: Int -> Maybe Interval
major 2 = Just 2
major 3 = Just 4
major 6 = Just 9
major 7 = Just 11
major _ = Nothing

major' = fromJust . major

-- | Perfect interval. Only perfect intervals are fourth, fifth, and eighth.
perfect :: Int -> Maybe Interval
perfect 4 = Just 5
perfect 5 = Just 7
perfect 8 = Just 12
perfect _ = Nothing

perfect' = fromJust . perfect

-- | Minor interval. Only minor intervals are second, third, sixth, and seventh.
minor :: Int -> Maybe Interval
minor n = fmap (subtract 1) (major n)

minor' = fromJust . minor

-- | Augmented interval.
aug :: Int -> Maybe Interval
aug n = fmap (+ 1) (major n <|> perfect n)

aug' = fromJust . aug

-- | Diminished interval.
dim :: Int -> Maybe Interval
dim n = fmap (subtract 1) (perfect n <|> minor n)

dim' = fromJust . dim

-- | Finds the specific interval (amount of semitones) from n to m.
interval :: Note -> Note -> Interval
interval n m
    | n == m = 0
    | otherwise = 1 + interval n (pred m)

-- | Finds the specific interval (amount of semitones) from note instances n to m.
-- | Note durations are ignored here.
intervalN :: NoteInstance -> NoteInstance -> Interval
intervalN n@(NoteInstance n1 o1 _) m@(NoteInstance n2 o2 _)
    | (n1, o1) == (n2, o2) = 0
    | otherwise = 1 + intervalN n (predN m)

-- | An alias of `interval`, designed to be used infix.
to = interval

-- | An alias of `intervalN`, designed to be used infix.
toN = intervalN

-- | Shift a note by a given interval.
shift :: Interval -> Note -> Note
shift 0 = id
shift n | n > 0 = succ . shift (n - 1)
        | n < 0 = pred . shift (n + 1)

-- | Shift a note instance by a given interval.
shiftN :: Interval -> NoteInstance -> NoteInstance
shiftN 0 = id
shiftN n | n > 0 = succN . shiftN (n - 1)
         | n < 0 = predN . shiftN (n + 1)