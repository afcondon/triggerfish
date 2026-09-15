-- | `Vetula.Rehearsal` — **a progression with alternatives at every chord.**
-- |
-- | A progression stops being a list the moment its chords have alternatives:
-- | four options at each of three chords is sixty-four progressions. What you
-- | want then is not to pick one but to keep running them until one is obviously
-- | right — which is what rehearsing is, and why the stage is called that.
-- |
-- | `Harmonia.Trellis` owns the musical question (how a path through the lattice
-- | gets chosen, and how smooth its joins are). This module owns the editing
-- | one: what a slot is, what settling does to it, and how a captured chord
-- | becomes one. The view owns neither.
module Vetula.Rehearsal
  ( Slot
  , slotFrom
  , nodeFromEvent
  , toggleOption
  , dropOptionAt
  , lattice
  , chosen
  , chords
  , motion
  , size
  , around
  , allAround
  , allPaths
  ) where

import Prelude

import Data.Array (catMaybes, deleteAt, drop, filter, find, findIndex, head, index, length, nub, range, snoc, sort, sortBy, take, zipWith)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits as String

import Harmonia.Anchor (Anchor)
import Harmonia.Recognise as R
import Harmonia.Trellis as HT
import Harmonia.Voicing (Voicing(..))
import Harmonia.Walk (seed)
import Vetula.Harmony (ChordNode, playNotes, triadNode)

-- | **One slot: a chord and everything it is allowed to be.**
-- |
-- | Option ZERO is the chord the progression actually said, so a slot is never
-- | empty and "play it as written" is not a special case.
-- |
-- | `locked` settles the slot. Settling is deliberately NOT deleting: the
-- | alternatives stay, so you can lock a choice to hear the rest of the
-- | progression against it and then unlock and keep looking. That is the whole
-- | difference between rehearsing and deciding.
type Slot =
  { options :: Array ChordNode
  , locked :: Maybe Int
  }

-- | **A chord out of a captured event, keeping its EXACT voicing.**
-- |
-- | `triadNode` re-voices from pitch classes, which is right when blooming a
-- | neighbourhood around a chord and wrong here — the whole point of a slot is
-- | the chord as it was actually played. The recogniser supplies the root, and
-- | the name where the event has none.
-- |
-- | Row-polymorphic in the event so this module need not know what else a
-- | capture carries (`at`, and whatever the chyron grows later).
nodeFromEvent
  :: forall r
   . { notes :: Array Int, label :: String, anchor :: Anchor | r }
  -> ChordNode
nodeFromEvent ev =
  let
    ns = sort ev.notes
    b = fromMaybe 60 (head ns)
    ps = sort (nub (map (\n -> mod n 12) ns))
    cand = R.best (R.observeWithBass (mod b 12) ps)
    root = maybe (mod b 12) R.candidateRoot cand
    -- A capture's own label is preferred where it says something — it knows the
    -- spelling the chord was MEANT to have, which the recogniser can only guess
    -- at. But a scene recalled through `mkSavedSeq` carries "1", "2", "3" as
    -- labels, and a progression of numbered boxes beside variations called
    -- `Dm(add9)/E` was telling you least about the chords you started from.
    -- A label with no note letter in it is a position, not a name.
    lab = if named ev.label then ev.label else maybe ev.label R.candidateName cand
  in
    (triadNode root ps lab)
      { bassPc = mod b 12, bassOct = b / 12, voicing = drop 1 ns, anchor = ev.anchor }

-- | One slot from one captured chord: itself as option zero, plus anything
-- | already kept for that exact voicing. That second half is what keying the
-- | free-standing tray by NOTES buys — browse variations first, take up a
-- | progression afterwards, and the work you did comes with it.
slotFrom
  :: forall r k
   . Array { notes :: Array Int, options :: Array ChordNode | k }
  -> { notes :: Array Int, label :: String, anchor :: Anchor | r }
  -> Slot
slotFrom kept ev =
  let
    base = nodeFromEvent ev
    key = sort ev.notes
    extra = case find (\e -> sort e.notes == key) kept of
      Just e -> filter (\o -> sort (playNotes o) /= key) e.options
      Nothing -> []
  in
    { options: [ base ] <> extra, locked: Nothing }

-- | **Add or remove a variation.** Option zero is the progression's own chord:
-- | keeping it again is a no-op and dropping it is refused, because a slot has
-- | to be able to play what was written. Everything else toggles, so an accident
-- | costs exactly what the choice did.
toggleOption :: ChordNode -> Slot -> Slot
toggleOption c sl = case findIndex (\o -> playNotes o == playNotes c) sl.options of
  Just 0 -> sl
  Just j -> dropOptionAt j sl
  Nothing -> sl { options = sl.options <> [ c ] }

-- | Remove an option and keep `locked` pointing at the same CHORD it did. Every
-- | index after the removed one shifts down by one, and a settled slot that
-- | quietly re-settled on its neighbour is the sort of bug you would only ever
-- | notice by ear, three chords later.
dropOptionAt :: Int -> Slot -> Slot
dropOptionAt j sl
  | j == 0 = sl
  | otherwise =
      sl
        { options = fromMaybe sl.options (deleteAt j sl.options)
        , locked = case sl.locked of
            Just k | k == j -> Nothing
            Just k | k > j -> Just (k - 1)
            other -> other
        }

-- | The lattice as Harmonia sees it. A settled slot is narrowed to ONE option,
-- | which is all settling means downstream — and it then anchors the smoothness
-- | of its neighbours without `Harmonia.Trellis` knowing what settling is.
-- |
-- | An out-of-range `locked` reads as unsettled rather than as an error. It
-- | cannot arise through this module — `dropOptionAt` keeps the index honest —
-- | and if it ever did, offering the whole slot is the recoverable failure and
-- | offering nothing is not.
lattice :: Array Slot -> Array (Array Voicing)
lattice = map slotVoicings
  where
  slotVoicings sl = case sl.locked >>= index sl.options of
    Just c -> [ Voicing (playNotes c) ]
    Nothing -> map (\c -> Voicing (playNotes c)) sl.options

-- | **Which option each slot plays on this pass.**
-- |
-- | Computed, never stored: a pure function of the pull, the roll and the slots,
-- | so there is no second copy of the truth to fall out of step with the first.
-- | `lattice` returns index 0 for a settled slot, so the real index is read back
-- | off `locked`.
chosen :: HT.Pull -> Int -> Array Slot -> Array Int
chosen pull roll slots =
  zipWith (\sl i -> fromMaybe i sl.locked) slots (HT.path pull (seed (roll + 1)) (lattice slots))

chords :: HT.Pull -> Int -> Array Slot -> Array ChordNode
chords pull roll slots = catMaybes (zipWith (\sl i -> index sl.options i) slots (chosen pull roll slots))

-- | What this pass costs at its joins — the number the pull dial is moving, so
-- | worth showing beside it: it turns a claim about smoothness into something
-- | the player can watch change.
motion :: HT.Pull -> Int -> Array Slot -> Int
motion pull roll slots = HT.pathMotion (map (\c -> Voicing (playNotes c)) (chords pull roll slots))

-- | How many progressions the lattice holds: the product of the slot widths.
size :: Array Slot -> Int
size = HT.size <<< map _.options

-- | Does this label name a chord, or is it just a position? Anything starting
-- | with a note letter is a name; "1" and "" are not.
named :: String -> Boolean
named lab = case String.uncons lab of
  Just { head: c } -> c >= 'A' && c <= 'G'
  Nothing -> false

-- | **The chord before, this one, and the chord after.**
-- |
-- | Auditioning a substitute alone answers the wrong question. A chord is not
-- | good or bad; it is good or bad *there*, between the two chords it has to
-- | live with — which is why a piano player trying a substitution plays three
-- | chords, never one. The neighbours come from the current pass, so what you
-- | hear is the progression as it actually stands.
around :: HT.Pull -> Int -> Array Slot -> Int -> ChordNode -> Array ChordNode
around pull roll slots i c =
  let played = chords pull roll slots
  in catMaybes [ index played (i - 1), Just c, index played (i + 1) ]

-- | **Every way the neighbours could go, with this chord in the middle.**
-- |
-- | The step past `around`, and AC's: if the chord before and the chord after
-- | have alternatives of their own, then whether THIS one works is a question
-- | about all of them at once. So play the lot — every combination of the
-- | neighbours' options with this chord fixed in the middle.
-- |
-- | Capped, because the product runs away: six options either side is
-- | thirty-six phrases, which is two minutes of listening and nobody is
-- | comparing the first to the last. The cap keeps it inside the span of a
-- | musical decision.
allAround :: Int -> Array Slot -> Int -> ChordNode -> Array (Array ChordNode)
allAround cap slots idx c =
  take cap do
    before <- optionsAt (idx - 1)
    after <- optionsAt (idx + 1)
    pure (before <> [ c ] <> after)
  where
  optionsAt k = case index slots k of
    Nothing -> [ [] ]
    Just sl -> map (\o -> [ o ]) sl.options

-- | **Every path through the lattice, smoothest first.**
-- |
-- | The pull dial plays ONE path and re-rolls; this lays them all out. They are
-- | different questions — the dial is for finding something you did not expect,
-- | this is for choosing once you know roughly what you want — and the ordering
-- | is what makes it usable: the top of the list is the progression that moves
-- | least, the bottom is the one that leaps, and everything in between is a
-- | trade you can hear.
-- |
-- | `Nothing` when the lattice is too large to lay out. That is not a failure to
-- | handle but a thing to say: five slots of four is 1024 readings, which is not
-- | a list anyone chooses from. Settling a slot or two collapses it, which makes
-- | this view the reason to settle rather than a casualty of not having.
allPaths :: Int -> Int -> Array Slot -> Maybe (Array { ixs :: Array Int, motion :: Int })
allPaths limit cap slots
  | size slots > limit = Nothing
  | otherwise =
      Just (take cap (sortBy (comparing _.motion) (map score (combos slots))))
  where
  -- A settled slot contributes its one index, so settling narrows the list in
  -- exactly the way the player expects.
  combos = foldl step [ [] ]
  step acc sl = do
    prefix <- acc
    j <- case sl.locked of
      Just k -> [ k ]
      Nothing -> range 0 (length sl.options - 1)
    pure (snoc prefix j)

  score ixs =
    { ixs
    , motion: HT.pathMotion
        (map (\c -> Voicing (playNotes c))
          (catMaybes (zipWith (\sl j -> index sl.options j) slots ixs)))
    }
