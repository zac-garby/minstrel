module Music.Minstrel.Notes
    ( Duration
    , Octave
    , Accidental (..)
    , Letter (..)
    , Note (..)
    , NoteInstance (..)
    , succN
    , predN
    , canonical
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)

-- | The length a note is played for. Represented as how many fit into a bar in 4/4 time.
-- | For example, a crotchet is 4 because it is a quarter note, and a semibreve is 1.
type Duration = Int

-- | Represents an octave on the keyboard. 0 is the lowest octave.
type Octave = Int

-- | The letter corresponding to a musical note. Sharps/flats not included here, look at Note.
data Letter = A | B | C | D | E | F | G
    deriving (Show, Read, Eq, Ord)

instance Enum Letter where
    toEnum n = [C, D, E, F, G, A, B] !! (n `mod` 7)
    fromEnum l = fromJust . elemIndex l $ [C, D, E, F, G, A, B]

-- | An (optional) accidental. Natural represents the absense of an accidental.
data Accidental = Natural | Sharp | Flat
    deriving (Show, Read, Eq)

-- | A combination of a letter (A, B, etc..) and an accidental.
data Note = Note Letter Accidental

instance Show Note where
    show (Note l Natural) = show l
    show (Note l Sharp) = show l ++ "#"
    show (Note l Flat) = show l ++ "b"

-- | Two notes are equal if they represent the same sound.
instance Eq Note where
    x == y = let (Note l1 a1) = canonical x
                 (Note l2 a2) = canonical y
             in (l1 == l2) && (a1 == a2)

-- | Notes begin at C natural, with the value of 0. They go up to B at 11, and then wrap back around.
instance Enum Note where
    toEnum n = [ Note C Natural, Note C Sharp, Note D Natural, Note D Sharp, Note E Natural, Note F Natural,
          Note F Sharp, Note G Natural, Note G Sharp, Note A Natural, Note A Sharp, Note B Natural ] !! (n `mod` 12)
    
    fromEnum n = fromJust . elemIndex (canonical n) $
        [ Note C Natural, Note C Sharp, Note D Natural, Note D Sharp, Note E Natural, Note F Natural,
          Note F Sharp, Note G Natural, Note G Sharp, Note A Natural, Note A Sharp, Note B Natural ]

-- | A specific instance of a note. For example, a C4 minim.
data NoteInstance = NoteInstance Note Octave Duration
    deriving Eq

instance Show NoteInstance where
    show (NoteInstance n o d) = show n ++ show o ++ " " ++ show d

-- Note: NoteInstance can't be an Enum because there's no way to produce a total function for
-- fromEnum, nor a surjective function for toEnum. So, succN and predN are used to shift them.
-- The N prefix can also be used on functions such as upTone, to shift a note instance up a whole
-- tone.

succN :: NoteInstance -> NoteInstance
succN (NoteInstance n@(Note l a) o d) =
    let n'@(Note l' a') = succ n
        o' = if l' < l then o + 1 else o
    in NoteInstance n' o' d

predN :: NoteInstance -> NoteInstance
predN (NoteInstance n@(Note l a) o d) =
    let n'@(Note l' a') = pred n
        o' = if l' > l then o - 1 else o
    in NoteInstance n' o' d

-- | Convert a note to an arbitrary canonical form to allow for easier comparisons
-- | with pattern matching. For example, the canonical forms of F and E# are the same.
-- | The arbitrary form chosen is representing any note as a natural, or a sharp if that
-- | isn't possible.
canonical :: Note -> Note
canonical (Note E Sharp) = Note F Natural
canonical (Note F Flat) = Note E Natural
canonical (Note B Sharp) = Note C Natural
canonical (Note C Flat) = Note B Natural
canonical (Note x Flat) = Note (pred x) Sharp
canonical x = x