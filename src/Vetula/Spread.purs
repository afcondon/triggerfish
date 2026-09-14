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
  , toneIxOfPc
  , toneAt
  , favKey
  ) where

import Prelude

import Data.Array (cons, drop, filter, findIndex, head, index, length, mapWithIndex, nub, nubByEq, snoc, sort, uncons, unsnoc)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Harmonia.Chord (Chord(..))
import Harmonia.OpenVoicing (Open, Place, Rooted, Spread, applySpread, baseStack, defaults, omit, spreadOf)
import Harmonia.Voicing (Voicing(..), voicingMidi)
import Vetula.Harmony (ChordNode, playNotes)

-- | The frame this chord's voicing is read in: the stack rooted on the chord's
-- | own bass, in the octave that bass actually occupies. `reach` is generous
-- | because these voicings were not necessarily made by `openVoicing` and may
-- | already be wider than its own enumeration allows.
-- |
-- | **`minTones` is the note count, not the tone count**, and the difference is
-- | load-bearing. `tonesOf` pads a short chord with extra roots, and those pads
-- | are what give a DOUBLED tone somewhere to live: in `C` voiced 36·60·64·67
-- | the upper C is a second root, and with only three stack positions (36·40·43)
-- | it belongs to none of them and is silently dropped. One row per sounding
-- | note guarantees every note a home. Measured: 0 of 7 diatonic triads
-- | round-tripped before this, all of them after.
openFor :: ChordNode -> Open
openFor c =
  let notes = playNotes c
      bass = fromMaybe 36 (head notes)
  in defaults
       { octave = bass / 12 - 1
       , minTones = length notes
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
-- | **this pitch class is not in the chord at all.** Measured against the
-- | voicing's uppers rather than `playNotes`, because the pinned bass sounding
-- | a pitch class should not stop you putting that tone back above it.
ghostRows :: ChordNode -> Array ToneRow
ghostRows c =
  nubByEq (\a b -> a.pc == b.pc)
    (filter (\r -> not (elem r.pc heard)) (toneRows c))
  where
  heard = map (\m -> mod m 12) c.voicing

-- | **A real inversion: roll the lowest voice up an octave (or the highest
-- | down), and let the bass follow.**
-- |
-- | Distinct from `rotateBass`, which re-foots the chord WITHOUT touching the
-- | upper structure — that is a slash chord, and a useful thing, but it is not
-- | an inversion: C·E·G over E is still C·E·G. A first inversion is E·G·C, and
-- | getting there means moving the C.
invertNode :: Int -> ChordNode -> ChordNode
invertNode dir c =
  if dir > 0 then case uncons ns of
    Just { head: lo, tail: rest } -> refoot (snoc rest (lo + 12))
    Nothing -> c
  else case unsnoc ns of
    Just { init: rest, last: hi } -> refoot (cons (hi - 12) rest)
    Nothing -> c
  where
  ns = sort c.voicing
  -- the bass is the new lowest voice: an inversion is named for what is at the
  -- bottom, so leaving `bassPc` behind would sound the old inversion under the
  -- new one.
  refoot xs =
    let sorted = sort xs
    in c { voicing = sorted, bassPc = mod (fromMaybe c.bassPc (head sorted)) 12 }

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
