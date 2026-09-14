-- | `Vetula.Spread` — **the bridge between a `ChordNode` and an editable
-- | voicing.**
-- |
-- | Harmonia models a voicing as a `Spread`: one `Place` per chord tone, each
-- | holding the octaves that tone sounds at, with the bass pinned and not
-- | listed. Vetula models a chord as a `ChordNode`, whose `voicing` is an array
-- | of absolute MIDI uppers over a `bassPc`. Every revoicing gesture wants the
-- | first and every other part of the app wants the second, so the conversion
-- | belongs in one place rather than at each call site.
-- |
-- | ## Which frame
-- |
-- | `Harmonia.OpenVoicing` needs an `Open` to know where the stack starts, and
-- | the answer here is **the chord's own bass**, not a fixed register: these
-- | chords come from five different lenses and sit wherever their lens put
-- | them. So `octave` is read back off `playNotes`, `root` is the chord's
-- | `bassPc` rather than its `root` (an inversion has to keep its own bass
-- | under the stack), and `minTones` is the tone count as it stands, so nothing
-- | is padded that the chord did not already have.
-- |
-- | The consequence worth stating: because the stack is built from the chord's
-- | actual bass upward, **every note of the chord sits at or above its own
-- | stack position**, which is the precondition `spreadOf`'s matching needs.
-- | The reading is therefore exact here even though it is best-effort in
-- | general.
-- |
-- | ## Tones, not slots
-- |
-- | A `Spread` is indexed by CHORD TONE, and that is the improvement over the
-- | array-slot indexing the ladder uses today. The existing drag deliberately
-- | refuses to re-sort a voicing because the slot carries the selection ring,
-- | and re-sorting moved the ring onto a different note. A tone index cannot be
-- | reshuffled by any edit, so the workaround stops being necessary.
module Vetula.Spread
  ( openFor
  , rootedOf
  , spreadOfNode
  , applyToNode
  , ToneRow
  , toneRows
  , ghostRows
  , invertNode
  , refootNode
  , nextBassTone
  , toneIxOfPc
  , toneAt
  , favKey
  ) where

import Prelude

import Data.Array (cons, deleteBy, drop, filter, find, findIndex, head, index, length, mapWithIndex, nub, nubByEq, snoc, sort, uncons, unsnoc)
import Data.Foldable (any, elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Harmonia.Chord (Chord(..))
import Harmonia.OpenVoicing (Open, Place, Rooted, Spread, applySpread, baseStack, defaults, omit, spreadOf)
import Harmonia.Voicing (Voicing(..), voicingMidi)
import Vetula.Harmony (ChordNode, bassMidi, playNotes)

-- | The frame this chord's voicing is read in: the stack rooted on the chord's
-- | own bass, in the octave that bass actually occupies. `reach` is generous
-- | because these voicings were not necessarily made by `openVoicing` and may
-- | already be wider than its own enumeration allows.
-- |
-- | **The stack must be tall enough for every note AND every tone**, and
-- | getting `minTones` wrong breaks things in two different directions.
-- |
-- | Too short for the NOTES and a doubled tone has nowhere to live: `tonesOf`
-- | pads a short chord with extra roots, and those pads are what host the second
-- | copy. In `C` voiced 36·60·64·67 the upper C is a second root, and with only
-- | three stack positions it belongs to none of them and is dropped. (Measured:
-- | 0 of 7 diatonic triads round-tripped before this was raised to the note
-- | count.)
-- |
-- | Too short for the TONES and omitting a note corrupts a different one. The
-- | note count shrinks when you drop a tone, which takes a padded position away
-- | with it — so omitting the E from that same C left the upper C with no
-- | position of its own, and it vanished on the next read. 120 of 186 chords
-- | failed that way.
-- |
-- | So it is the larger of the two. `distinct + 1` gives every pitch class a
-- | position ABOVE the pinned bass, which is what an omitted tone needs in order
-- | to have a ghost row to come back at.
openFor :: ChordNode -> Open
openFor c =
  let notes = playNotes c
      bass = fromMaybe 36 (head notes)
      distinct = length (nub (map (\p -> mod p 12) c.pcs))
  in defaults
       { octave = bass / 12 - 1
       , minTones = max (length notes) (distinct + 1)
       , reach = 4
       }

-- | The chord as Harmonia sees it. `root` is the BASS pitch class, not the
-- | chord's nominal root — the stack has to start under the note that is
-- | actually lowest, or an inversion reads as a displacement of something it
-- | is not.
rootedOf :: ChordNode -> Rooted
rootedOf c = { root: c.bassPc, chord: Chord (nub (map (\p -> mod p 12) c.pcs)) }

-- | This chord's current voicing, read as a spread.
spreadOfNode :: ChordNode -> Spread
spreadOfNode c = spreadOf (openFor c) (rootedOf c) (Voicing (playNotes c))

-- | Re-render the chord from an edited spread. `bassPc` is untouched — the bass
-- | is not a tone the spread can reach, which is the strategy's one constraint —
-- | so `playNotes` still re-grounds it exactly as before.
applyToNode :: ChordNode -> Spread -> ChordNode
applyToNode c sp = c { voicing = drop 1 (voicingMidi (applySpread (openFor c) (rootedOf c) sp)) }

-- | One row of the ladder: a chord tone, where its stack position is, and where
-- | it currently sounds (nowhere, if omitted).
-- |
-- | An omitted tone still gets a row — that is the whole point. Progressions
-- | greys the note out and lets you switch it back on, and you cannot click a
-- | note that is not drawn.
type ToneRow =
  { ix :: Int
  , pc :: Int
  , base :: Int
  , place :: Place
  }

-- | Every non-bass tone of the chord, in stack order.
toneRows :: ChordNode -> Array ToneRow
toneRows c =
  let positions = drop 1 (baseStack (openFor c) (rootedOf c))
      sp = spreadOfNode c
  in mapWithIndex
       (\i b -> { ix: i, pc: mod b 12, base: b, place: fromMaybe omit (index sp i) })
       positions

-- | The tone index a sounding note belongs to — the ladder's dots are drawn
-- | from `playNotes` and its gestures address tones, so one has to map to the
-- | other.
toneIxOfPc :: ChordNode -> Int -> Maybe Int
toneIxOfPc c pc = findIndex (\r -> r.pc == mod pc 12) (toneRows c)

-- | **The tones that are genuinely not heard** — the only ones that should be
-- | drawn as ghosts.
-- |
-- | Not simply "the rows with an empty `Place`", and the difference is a real
-- | bug. `minTones` pads the stack so a DOUBLED tone has somewhere to live, and
-- | that padding gives one pitch class two positions. `spreadOf`'s matching is
-- | greedy — every copy is credited to the lowest position of its pitch class —
-- | so the padded position ends up permanently empty and drew a full column of
-- | ghosts for a note that was sounding two rows below.
-- |
-- | The rule that fixes it is also the truer statement of what a ghost means:
-- | **this pitch class is not sounding anywhere.** Measured against
-- | `playNotes`, the bass included — an earlier version looked only at the
-- | uppers, reasoning that a pinned bass should not stop you putting the tone
-- | back above it. That was wrong twice over: a chord whose root lives only in
-- | the bass (which is every `openVoicing` chord) then offered a ghost for a
-- | note you could plainly hear, and after an inversion it did so for whichever
-- | tone had just become the bass. Doubling a tone is what `doubleTone` is for;
-- | a ghost is for a tone that is GONE.
ghostRows :: ChordNode -> Array ToneRow
ghostRows c =
  nubByEq (\a b -> a.pc == b.pc)
    (filter (\r -> not (elem r.pc heard)) (toneRows c))
  where
  heard = map (\m -> mod m 12) (playNotes c)

-- | **A real inversion: the lowest sounding note up an octave (or the highest
-- | down), and the bass is whatever is lowest afterwards.**
-- |
-- | Two things this must NOT be, both of which it has been:
-- |
-- |   * `rotateBass`, which re-foots the chord without touching the upper
-- |     structure. C·E·G over E is a slash chord; a first inversion is E·G·C.
-- |   * a rotation of `voicing` alone. The uppers are not the chord — a voicing
-- |     from `openVoicing` has its root in the BASS and often nowhere else, so
-- |     rotating the uppers of `CM7` (36·55·71·76) makes G the bass and deletes
-- |     the C. Measured: 62 of 186 chords lost a pitch class that way.
-- |
-- | So it works over `playNotes`, and the bass is re-read from the result. That
-- | also settles the register: `bassOct` follows the new lowest note, so a full
-- | turn of the inversions carries the bass up with the chord instead of
-- | stranding it two octaves below.
-- |
-- | ⟲ and ⟳ are not exact inverses on a widely-spaced chord, and cannot be:
-- | "lowest up" and "highest down" only undo each other when the note that
-- | moved up lands on top, which is true in close position and not otherwise.
-- | They preserve the chord's content in both directions, which is the property
-- | that matters.
invertNode :: Int -> ChordNode -> ChordNode
invertNode dir c =
  if dir > 0 then case uncons ns of
    Just { head: lo, tail: rest } -> refoot (snoc rest (lo + 12))
    Nothing -> c
  else case unsnoc ns of
    Just { init: rest, last: hi } -> refoot (cons (hi - 12) rest)
    Nothing -> c
  where
  ns = sort (playNotes c)
  refoot xs =
    let sorted = sort xs
        b = fromMaybe (bassMidi c) (head sorted)
    in c { bassPc = mod b 12, bassOct = b / 12, voicing = drop 1 sorted }

-- | The sounding chord tone `dir` steps around from the current bass — the
-- | cycle the ⟲/⟳ bass controls walk.
-- |
-- | **Sounding**, not merely present in `pcs`. An omitted tone has no copy to
-- | trade with, so re-footing onto it could only preserve the chord's content by
-- | growing it. Leaving it out of the cycle is the honest fix: you cannot put a
-- | note in the bass that you have just chosen not to play.
nextBassTone :: Int -> ChordNode -> Int
nextBassTone dir c =
  let tones = sort (nub (map (\m -> mod m 12) (playNotes c)))
      n = length tones
  in case findIndex (_ == mod c.bassPc 12) tones of
       Just i | n > 0 -> fromMaybe c.bassPc (index tones (mod (i + dir + n) n))
       _ -> c.bassPc

-- | **Re-foot the chord on another of its sounding tones, changing neither the
-- | note count nor the pitch-class content.**
-- |
-- | The naive version — assign `bassPc` and stop — silently DELETES a pitch
-- | class. A voicing from `openVoicing` keeps its root in the bass and often
-- | nowhere else, so re-footing `CM7` (36·55·71·76) onto G left G·B·E with no C
-- | anywhere. Untested until now, because it lived inline in the component where
-- | no test could reach it.
-- |
-- | Count is the constraint that shapes the fix. `playNotes` is
-- | `[bass] <> voicing`, so the count is `1 + length voicing` and the uppers
-- | must come out the same length as they went in. Two cases, and the
-- | distinction is exactly whether the outgoing bass tone still has a home:
-- |
-- |   * **the old bass tone also sounds above** — then simply re-foot. The new
-- |     bass tone doubles (bass and upper), nothing is lost, nothing is added.
-- |   * **it does not** — then trade: the new bass tone gives up its lowest
-- |     upper copy, and the outgoing tone takes that copy's place at the nearest
-- |     octave. One out, one in.
refootNode :: Int -> ChordNode -> ChordNode
refootNode pc c
  | mod pc 12 == mod c.bassPc 12 = c
  | otherwise =
      let
        oldPc = mod c.bassPc 12
        ups = sort c.voicing
      in
        if any (\m -> mod m 12 == oldPc) ups then c { bassPc = mod pc 12 }
        else case find (\m -> mod m 12 == mod pc 12) ups of
          Nothing -> c { bassPc = mod pc 12 }
          Just v ->
            c { bassPc = mod pc 12
              , voicing = sort (cons (nearestOctaveOf oldPc v) (deleteBy (==) v ups))
              }

-- | The octave of `pc` closest to `near`.
nearestOctaveOf :: Int -> Int -> Int
nearestOctaveOf pc near =
  let up = near + mod (mod pc 12 - mod near 12 + 12) 12
      down = up - 12
  in if near - down <= up - near then down else up

-- | **Which tone, and which of its copies, a sounding note is.**
-- |
-- | `toneIxOfPc` names the tone; this also names the OCTAVE within it, which is
-- | what any gesture aimed at a single note needs. A doubled tone is one tone
-- | heard twice — one `Place` holding two octaves — so a click that only knows
-- | the tone cannot help acting on both copies.
toneAt :: ChordNode -> Int -> Maybe { ix :: Int, oct :: Int }
toneAt c m = case findIndex (\r -> r.pc == mod m 12) rows of
  Nothing -> Nothing
  Just i -> map (\r -> { ix: i, oct: (m - r.base) / 12 }) (index rows i)
  where
  rows = toneRows c

-- | Favourites key. A kept SPREAD applies to any chord, so it is filed by tone
-- | COUNT rather than by note-set — which is the whole reason for keeping one.
favKey :: ChordNode -> String
favKey c = show (length (nub (map (\p -> mod p 12) (sort c.pcs))))
