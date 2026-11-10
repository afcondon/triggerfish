-- | `Triggerfish.Selene.Layout` — how the rig is configured to RECEIVE.
-- |
-- | A layout assigns the rig's physical outputs to named **voice groups**. It is
-- | the Yarns idea: you never configure a jack, you pick a layout, and the layout
-- | says how the outputs are partitioned and what each group means. See
-- | `docs/DESIGN-selene-companion.md`.
-- |
-- | ## The one decision everything follows from: keyed by OUTPUT
-- |
-- | Write this group-first — a list of groups, each naming the outputs it uses —
-- | and you can name the same jack twice. Two groups fighting over one output is
-- | exactly the state the router exists to prevent, and it would be back,
-- | catchable only by a validation pass.
-- |
-- | Keyed by output, **an output cannot appear twice, because it is the key.**
-- | The remaining representable incoherence is the benign one: two outputs
-- | serving one voice-role, which is a mult.
-- |
-- | Note this is NOT a total partition. Outputs may be unassigned, and "which
-- | jacks are free?" is a first-class question — see `freeOutputs`. The invariant
-- | is injectivity, not coverage.
-- |
-- | ## A layout carries no sources
-- |
-- | That is what makes it portable, shareable and worth collecting — the same
-- | principle as an envelope shape carrying no range and no jack. Who plays a
-- | group is a *binding*, and lives elsewhere.
-- |
-- | The MIDI **channel** is the exception that proves it: it lives here, because
-- | the MCV is the thing that listens on it. That is a fact about how the rig is
-- | configured, not about who is playing, and keeping it here is what lets a
-- | layout be applied and tested before anything is bound to it.
module Triggerfish.Selene.Layout
  ( Output
  , Role
  , Assignment
  , Allocation(..)
  , Policy(..)
  , Group
  , Layout
  , outputKey
  , allocationKey
  , groupNamed
  , voiceOutputs
  , capabilities
  , freeOutputs
  , Problem(..)
  , validate
  ) where

import Prelude

import Data.Array (elem, filter, findIndex, length, mapMaybe, nub, sort, (!!), (..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)

-- ---------------------------------------------------------------------------
-- The pieces
-- ---------------------------------------------------------------------------

-- | Where a signal physically comes out: bank + slot rather than an absolute
-- | jack number, because that is the vocabulary both devices already use and
-- | exactly what `apply-drumkit` consumes. FH-2 banks are `main` / `cv0`..`cv6` /
-- | `gt0`..`gt7` (see `FH2.Roles.Bank`); slots are 0..7 within a bank.
type Output = { device :: String, bank :: String, slot :: Int }

-- | What an output carries for its voice. **Deliberately an open vocabulary** —
-- | "gate", "pitch", "mod", "accent", "slide", "level", "timbre" — because a 303
-- | and a Plaits do not agree on a fixed set and never will. A closed ADT here
-- | would have to grow every time a module is bought.
type Role = String

-- | Which voice of which group an output serves. The VALUE side of the map; the
-- | output itself is the key.
type Assignment = { group :: String, voice :: Int, role :: Role }

-- | Given a note from the bound source, which voice plays it?
-- |
-- | `Mono` is not `Poly` with one voice: the distinction is **who chooses**.
-- | Mono has no choice to make, `Poly` hands the choice to the engine, `Indexed`
-- | hands it to the source's own lane index. That trichotomy is the whole of
-- | Yarns' mode list, and it covers Odonus (four `Mono`), Vetula (one `Poly`) and
-- | Balistes (one `Indexed` of sixteen).
data Allocation
  = Mono
  | Poly Policy
  | Indexed

derive instance eqAllocation :: Eq Allocation

data Policy = Cyclic | Lowest | Highest | Unison

derive instance eqPolicy :: Eq Policy

-- | One voice group.
-- |
-- | `selectors` is the note each voice's MCV filters on, and applies to `Indexed`
-- | groups only. It is receive-side configuration — the MCV genuinely is set up
-- | with it — and putting it here makes this **the single place the kit
-- | convention is written**. It previously lived in a hardcoded `KIT` table in
-- | another repo (`fh2-config/scripts/apply-drum-breakout.mjs`), which is the
-- | same consolidation the router did for every other kind of placement.
-- |
-- | It must agree with the source's own notes (Balistes' `canonKit`). Nothing
-- | here can enforce that; what it can do is stop the fact being written twice in
-- | two repos with no way to compare them.
type Group =
  { name :: String
  , voices :: Int
  , allocation :: Allocation
  , channel :: Int
  -- | The MCV slots this group consumes. **The contended resource is the MCV,
  -- | not the jack** — polyenv takes 0..7 (one per envelope, channel = slot + 1)
  -- | while the drum breakout takes 0..3, and their OUTPUTS are on different
  -- | hardware entirely, so nothing jack-indexed can see the collision. That was
  -- | learned the hard way; it is why this field exists rather than being derived.
  -- |
  -- | Empty when it does not apply or is not known — the drumkit macro lets the
  -- | daemon allocate, and guessing would be worse than saying nothing.
  , mcvs :: Array Int
  -- | The module this group drives, by its **Amphora `vco-calibrations` label**
  -- | — `saich-1`, `cursus-iteritas-percido`. Distinct from `target`, which is a
  -- | human note: this one is a JOIN KEY, and it is what makes a pitch output
  -- | correctable. An analogue VCO does not track 1 V/oct (the Tona measured
  -- | 1.007 → 1.030 across its range), so the correction table is the difference
  -- | between an intended pitch and one that lands true.
  -- |
  -- | The correction has to live upstream of the CV: the FH-2 self-calibrates its
  -- | DACs and exposes no software trim, and the ES-9 is a plain DAC. So the
  -- | store is keyed by destination VCO, and this is where a layout says which
  -- | VCO a jack reaches.
  , vco :: Maybe String
  , selectors :: Array Int
  , target :: Maybe String   -- ^ intended module; free text now, a reference later
  }

type Layout =
  { name :: String
  , groups :: Array Group
  , outputs :: Map Output Assignment
  }

-- ---------------------------------------------------------------------------
-- Wire spelling
-- ---------------------------------------------------------------------------

-- | `fh2/gt0/3`. This is a wire format: it is the JSON object key, so changing
-- | the spelling orphans every stored layout.
outputKey :: Output -> String
outputKey o = o.device <> "/" <> o.bank <> "/" <> show o.slot

allocationKey :: Allocation -> String
allocationKey = case _ of
  Mono -> "mono"
  Indexed -> "indexed"
  Poly p -> "poly:" <> case p of
    Cyclic -> "cyclic"
    Lowest -> "lowest"
    Highest -> "highest"
    Unison -> "unison"

-- ---------------------------------------------------------------------------
-- Reading a layout
-- ---------------------------------------------------------------------------

groupNamed :: Layout -> String -> Maybe Group
groupNamed lay nm = do
  i <- findIndex (\g -> g.name == nm) lay.groups
  lay.groups !! i

-- | The outputs serving one voice of one group, with the role each carries.
voiceOutputs :: Layout -> String -> Int -> Array { role :: Role, output :: Output }
voiceOutputs lay grp v =
  map (\(Tuple o a) -> { role: a.role, output: o })
    (filter (\(Tuple _ a) -> a.group == grp && a.voice == v) (Map.toUnfoldable lay.outputs))

-- | What a group can offer, taken from voice 0. Matching a source's `requires`
-- | against this is a subset test — which is what makes "this recipe can drive
-- | Odonus fully and Balistes partially" computable rather than merely renderable.
capabilities :: Layout -> String -> Array Role
capabilities lay grp = sort (nub (map _.role (voiceOutputs lay grp 0)))

-- | Of the outputs the rig actually has, the ones nothing claims. A first-class
-- | answer, not a leftover: "which jacks are free?" is asked constantly while
-- | patching, and the forward view cannot express it at all.
freeOutputs :: Array Output -> Layout -> Array Output
freeOutputs rig lay = filter (\o -> isNothing (Map.lookup o lay.outputs)) rig

-- ---------------------------------------------------------------------------
-- Validation — what injectivity does NOT give us for free
-- ---------------------------------------------------------------------------

-- | Making overlap unrepresentable does not make everything unrepresentable.
data Problem
  = UnknownGroup Output String
  | VoiceOutOfRange Output String Int Int
  | RaggedGroup String Int (Array Role)
  | UnwiredVoice String Int
  | SelectorCount String Int Int
  | ChannelCollision Int (Array String)
  | McvCollision Int (Array String)

-- | All problems, in no particular order. **Reports; does not block.** The
-- | daemons already own admission (es9-daemon has capability and overlap checks,
-- | fh2-config has `PortClaim`), and a second opinion computed here would be a
-- | second thing to drift — the standing lesson of the output-range bug.
validate :: Layout -> Array Problem
validate lay = dangling <> ragged <> selectors <> channels <> mcvs
  where
  pairs = Map.toUnfoldable lay.outputs :: Array (Tuple Output Assignment)

  dangling = mapMaybe check pairs
    where
    check (Tuple o a) = case groupNamed lay a.group of
      Nothing -> Just (UnknownGroup o a.group)
      Just g
        | a.voice < 0 || a.voice >= g.voices -> Just (VoiceOutOfRange o a.group a.voice g.voices)
        | otherwise -> Nothing

  -- A group whose voices carry different roles has no meaningful capability set,
  -- so `capabilities` would be quietly lying about it. A voice carrying NOTHING
  -- is a different mistake — the group was declared wider than it was wired — and
  -- deserves to say so rather than being reported as a disagreement with voice 0.
  ragged = do
    g <- lay.groups
    let want = capabilities lay g.name
    if want == [] then [ UnwiredVoice g.name 0 ] else do
      v <- laterVoices g
      let got = sort (nub (map _.role (voiceOutputs lay g.name v)))
      if got == want then []
        else if got == [] then [ UnwiredVoice g.name v ]
        else [ RaggedGroup g.name v got ]

  -- `1 .. 0` is DESCENDING in PureScript — it gives [1, 0], not [] — so a mono
  -- group would check a voice 1 that does not exist and report it as ragged.
  -- Guard rather than subtract. (`Rig.blocks` guards the same way, for the same
  -- reason.)
  laterVoices g = if g.voices <= 1 then [] else 1 .. (g.voices - 1)

  selectors = mapMaybe check lay.groups
    where
    check g =
      if g.allocation == Indexed && length g.selectors /= g.voices
        then Just (SelectorCount g.name (length g.selectors) g.voices)
        else Nothing

  channels = mapMaybe check (nub (map _.channel lay.groups))
    where
    check ch =
      let named = map _.name (filter (\g -> g.channel == ch) lay.groups)
      in if length named > 1 then Just (ChannelCollision ch named) else Nothing

  -- The check a jack-indexed view cannot do. See `Group.mcvs`.
  mcvs = mapMaybe check (nub (lay.groups >>= _.mcvs))
    where
    check n =
      let named = map _.name (filter (\g -> elem n g.mcvs) lay.groups)
      in if length named > 1 then Just (McvCollision n named) else Nothing

-- ---------------------------------------------------------------------------
-- JSON — the interchange format
-- ---------------------------------------------------------------------------
--
-- Plain JSON rather than a PureScript codec, because the whole point is that a
-- Tidal editor, a BEAM emitter or anything else can read a layout without
-- importing this module. See the doc's "the contract is JSON, not a PureScript
-- module".

type WireGroup =
  { voices :: Int
  , allocation :: String
  , channel :: Int
  , mcvs :: Array Int
  , vco :: Maybe String
  , selectors :: Array Int
  , target :: Maybe String
  }

type WireLayout =
  { name :: String
  , groups :: Object WireGroup
  , outputs :: Object Assignment
  }

-- ---------------------------------------------------------------------------
-- Compiling to the FH-2
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- The rig as it stands
-- ---------------------------------------------------------------------------
