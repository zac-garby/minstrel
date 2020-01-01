module Music.Minstrel.Chords
    ( Chord
    , ChordInstance
    , chord
    , chordN
    , major
    , minor
    , aug
    , dim
    , dom7
    , maj7
    , min7
    , halfDim7
    , dim7
    , invert, invertN
    ) where

import Music.Minstrel.Notes
import Music.Minstrel.Intervals (Interval, shift, shiftN)
import qualified Music.Minstrel.Intervals as I

-- | A chord is a collection of notes played at the same time.
type Chord = [Note]

-- | A chord instance is a collection of note instances played at the same time.
type ChordInstance = [NoteInstance]

-- | Generate a chord from the given root note and intervals.
chord :: [Interval] -> Note -> Chord
chord is n = map (\i -> shift i n) (0 : is)

-- | Generate a chord instance from the given root note instance and intervals.
chordN :: [Interval] -> NoteInstance -> ChordInstance
chordN is n = map (\i -> shiftN i n) (0 : is)

-- Triads
major = [I.major' 3, I.perfect' 5]
minor = [I.minor' 3, I.perfect' 5]
aug = [I.major' 3, I.aug' 5]
dim = [I.minor' 3, I.dim' 5]

-- Seventh Chords
dom7 = major ++ [I.minor' 7]
maj7 = major ++ [I.major' 7]
min7 = minor ++ [I.minor' 7]
halfDim7 = dim ++ [I.minor' 7]
dim7 = dim ++ [I.dim' 7]

-- | Inverts a chord. It's assumed that the lowest note is the first in the list.
invert :: Chord -> Chord
invert (c:cs) = cs ++ [shift I.octave c]

-- | Inverts a chord instance. It's assumed that the lowest note is the first in the list.
invertN :: ChordInstance -> ChordInstance
invertN (c:cs) = cs ++ [shiftN I.octave c]