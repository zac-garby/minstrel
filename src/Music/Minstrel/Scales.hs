module Music.Minstrel.Scales
    ( Scale
    , ScaleInstance
    , scale, scaleN
    , instantiate
    , major
    , minor
    , harmonicMinor
    , melodicMinor
    ) where

import Music.Minstrel.Notes
import Music.Minstrel.Intervals (Interval, tone, semitone, octave, aug', shift, shiftN)

-- | A scale is a sollection of notes played in sequence.
type Scale = [Note]

-- | A scale instance is a collection of note instances played in sequence.
type ScaleInstance = [NoteInstance]

-- | Create a scale from the given root note and intervals.
-- | Involves a normalisation step which assumes that it's possible to write the scale
-- | as a sequence of notes, one of each letter, just by altering the accidentals. Thus,
-- | this function can't be used for arbitrary melodies. Also, the last note (the tonic)
-- | is removed, because it's a duplicate of the first.
scale :: [Interval] -> Note -> Scale
scale is n = normalise $ scale' is n

-- | Create a scale, similar to `scale` except normalisation isn't applied. This
-- | means, for example, F major will turn out as [F,G,A,A#,C,D,E,F] - which, while correct
-- | is written in a non-standard way (two As, instead of A and Bb).
scale' = flip $ scanl (flip shift)

-- | Normalise a scale, for example [F,G,A,A#,C,D,E,F] -> [F,G,A,Bb,C,D,E,F].
-- | Two things are done: the last note is removed, since it's a duplicate of the
-- | first, and the notes are changed to be enharmonic equivalents but written
-- | in the standard form where each letter is present.
normalise :: Scale -> Scale
normalise [] = []
normalise [n] = []
normalise (n@(Note x y):(Note m a):ms) = n : normalise (m' : ms)
  where a' = if m == succ x then a else (if y == Sharp then Sharp else Flat)
        m' = Note (succ x) a'

-- | Create a scale instance from a scale, given an octave and note duration.
instantiate :: Octave -> Duration -> Scale -> ScaleInstance
instantiate o d s = map instantiateNote s
    where precedes a b = fromEnum a < fromEnum b
          tonic = head s
          instantiateNote n = shiftN (octave * o) (NoteInstance n (fromEnum $ n `precedes` tonic) d)

-- | Create a scale instance from the given root note and intervals.
scaleN :: [Interval] -> NoteInstance -> ScaleInstance
scaleN is n@(NoteInstance m o d) = instantiate o d (scale is m)

-- Scales
major :: [Interval]
major = [tone, tone, semitone, tone, tone, tone, semitone]

minor :: [Interval]
minor = [tone, semitone, tone, tone, semitone, tone, tone]

harmonicMinor :: [Interval]
harmonicMinor = [tone, semitone, tone, tone, semitone, aug' 2, semitone]

melodicMinor :: [Interval]
melodicMinor = [tone, semitone, tone, tone, tone, tone, semitone]