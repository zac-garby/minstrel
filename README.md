# Minstrel

Minstrel is a Haskell library/DSL for music theory. It can do various useful tasks, such as computing scales or chords or finding the interval between two notes.

While it can't do a *huge* amount at the moment, the aim is to have a DSL that can be used to help you compose music, in a similar way to how GHCi helps you write Haskell programs.

Here are some examples.

```haskell
import Music.Minstrel.Notes
import qualified Music.Minstrel.Intervals as I
import qualified Music.Minstrel.Scales as S
import qualified Music.Minstrel.Chords as C

eb = C.chord C.major (Note E Flat)
-- [Eb,G,A#]

c7 = C.chord C.dom7 (Note C Natural)
-- [C,E,G,A#]

bFlatMajorScale = S.scale S.major (Note B Flat)
-- [Bb,C,D,Eb,F,G,A]

cMinorScale = S.scale S.minor (Note C Natural)
-- [C,D,Eb,F,G,Ab,Bb]
```
