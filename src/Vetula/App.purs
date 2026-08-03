-- | Vetula — a growable harmonic exploration surface.
-- |
-- | The surface starts as the McMullen palette laid out on two axes (y = inside
-- | ↔ outside the chosen scale, x = the voice-leading move from the focus
-- | chord). Then you GROW it by ear:
-- |
-- |   * hover a chord, press **space** to hear it;
-- |   * hover + **v / i / s / e** to spawn its children — revoicings,
-- |     inversions, suspensions/omissions, extensions — re-centring on it;
-- |   * **click** a chord to pin it (kept across re-centres);
-- |   * **r** resets to the palette (pinned chords survive).
-- |
-- | `st.chords` is the model (provenance, pin, layout targets); the simulation
-- | supplies positions only; the render joins them by id. Plain Halogen-SVG
-- | render — the simple path; the model carries over to a HATS render if it
-- | scales.
module Vetula.App where

import Prelude

import Data.Array (concat, concatMap, deleteAt, drop, elem, elemIndex, filter, find, findIndex, head, index, insertAt, last, length, mapMaybe, mapWithIndex, modifyAt, nub, nubByEq, range, replicate, sort, take, takeEnd, updateAt, (!!))
import Data.Foldable (all, any, foldl, foldr, for_, maximum, minimum, sum)
import Data.Traversable (traverse)
import Data.Int (fromString, round, toNumber)
import Data.Number as Number
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Effect.Timer (setInterval)
import Data.Nullable (Nullable, null)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), contains, stripPrefix)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith, split, toLower, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Class.Console as Console
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, EventType(..), preventDefault, stopPropagation)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WE
import Vetula.SvgCoord (svgYFromEvent, svgXFromEvent, isFormField, surfaceHidden)
import Vetula.Path as Path
import Vetula.Generate (GenMode(..), generateCandidates)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Midi.Routing as Routing
import Triggerfish.Glyph (ChipView, Glyph, glyphOf, sessionAliasOf)
import Triggerfish.GlyphView (faIcon)
import Triggerfish.Preset (Preset, indexOfContent, presetAlias)
import Vetula.Store as Store
import Triggerfish.Amphora as Amphora
import Vetula.Tank (Specimen, SpecimenId(..), Provenance(..), specNotes)
import Reef.Vetula.Perf (VChord, VVoice, VDest(..), VRenderer(..), PerfClock, cursorAtClock, renderAlphaBlockMidiAt, renderAlphaClockMidiAt) as RV
import Reef.Vetula.Articulate (VArticulator(..), articulate, articLabel, nextArtic) as RA
import Vetula.Playhead (clockFor, defaultPattern, noteClock, patternClock)
import Vetula.Realise (fromChords)
import Vetula.Perform.Types
  ( PerfFx(..)
  , ArpDir(..)
  , VoiceShape(..)
  , PerfSel(..)
  , When(..)
  , Layer
  , PerfTerm(..)
  , PerfDragSrc(..)
  , mkLayer
  , arpDirGlyph
  , cycleArpDir
  , arpOrder
  , whenLabel
  , cycleWhen
  , termLabel
  , termShort
  , termRigOnly
  , printArpDir
  , parseArpDir
  , printVoiceShape
  , parseVoiceShape
  )
import Tidal.Pattern.Core (fast, slow, every)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Arc(..), eventValue, eventWhole, isDigital, mkArc, mkState, query)
import Tidal.Pattern.Types (Pattern) as PT
import Data.Rational as Rat
import Data.Either (Either(..))
import Reef.Vetula.Protocol (encodePerf) as RV
import Binnacle.Time (dateNow)
import Vetula.Tidal (progressionSource, parseProgression)
import Vetula.Lepidoptera (PerfDoc, VoiceSpec, docFromVoices, parsePerform, printAsRecord)
import Vetula.Clipboard (copyText)
import Binnacle.Midi as Midi
import Halogen.Widgets.Select as Select
import Halogen.Widgets.Modal as Modal
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)
import Hylograph.Simulation
  ( Engine(..), SimulationEvent(..), SimulationHandle, SimulationNode
  , Setup, runSimulation, setup, manyBody, collide, link, positionX, positionY
  , withStrength, withRadius, withDistance, withX, withY, static, dynamic )
import Harmonia.Anchor (Anchor(..))
import Harmonia.Voicing (Voicing(..), Selector(..), voicingMidi, takeVoicing, openTriad, rootless, drop2, drop2and4, quartal, cluster)
import Harmonia.Chord (Key, Mode(..), cMajorKey, chordRoot)
import Vetula.Between (bridgeNotes, maxBridge)
import Harmonia.Graded (transpose) as Graded
import Vetula.Palette (butlerChords, stockChords)
import Vetula.Harmony (ChordNode, Family(..), Kind(..), blackKeyPcs, diatonicTriads, generate, interchangeChords, keyX, keyboard, latticeChild, latticeFamily, mcmullenChords, noteName, place, placeOutside, playNotes, scaleSet, suspendSet, triadNode, triadOn, voicingCandidates, whiteKeyPcs)

midiPortName :: String
midiPortName = "IAC"

-- | The rig WebSocket (purerl-tidal); Binnacle subscribes to the Link anchor
-- | here. Same endpoint Odonus/Balistes use, so all three share one clock.
rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Vetula pulse = a 16th note; schedule ~120ms ahead, poll at 25ms — the
-- | same lookahead config as Odonus/Balistes, so the three grids align.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | Slice 4c: the surface's width-focus. Hunt = the lattice pool is dominant (you're
-- | finding chords); Perform = the rail is dominant (you're playing voices). Auto-flips
-- | on the empty↔non-empty path edge (first chord → Perform, cleared → Hunt); the
-- | Hunt/Perform toggle (and the collapsed-pool spine) override by hand. Nothing
-- | mounts/unmounts — focus only biases which column gets the width.
data Focus = Hunt | Perform

derive instance eqFocus :: Eq Focus

-- | Slice 4b: which rail section is expanded. The rail is an accordion — one section
-- | open at a time — over the three rail objects: the Progression (what), the Library
-- | (saved progressions), and the Voices (how it's performed).
data RailSection = SecProgression | SecLibrary | SecVoices

derive instance eqRailSection :: Eq RailSection
derive instance ordRailSection :: Ord RailSection

-- | The LEFT accordion (the reclaimed top bar): Setup (key/scale/family/borrow/
-- | palettes/connection), Tank (caught chords), Lens (view choice). Multi-open.
data LeftSection = SecSetup | SecTank | SecLens

derive instance eqLeftSection :: Eq LeftSection
derive instance ordLeftSection :: Ord LeftSection

-- | Slice C (tank model) — the Stage's swappable LENS. The pool is a frame; the
-- | lens is the view inside it. Adding a lens is ADDITIVE: one constructor, one
-- | `renderLens` branch, one `allLenses` entry — that's the decoupling proof.
-- | Lenses span a density axis: Keyboard is the exhaustive hunting cloud (every
-- | family + seed-bloom on the piano); PadGrid is a sparse, playable 4×4 board of
-- | the tank — the mouse-driven seed of the control-surface idea (see
-- | docs/DESIGN-vetula-tank-model.md).
-- | `LensCircleFifths` is the first of the GEOMETRIC lenses: the same pool chords,
-- | laid out by root on the circle of fifths (a spoke per root, radiating outward)
-- | rather than over the piano. Its geometry and the grade agree — the active
-- | key's diatonic roots form a contiguous highlighted wedge, borrowed roots sit
-- | just outside it, distant roots fall around the far side.
-- | `LensTonnetz` is the second geometric lens: the neo-Riemannian tonal net. Its
-- | own triad-lattice (not a pool re-projection) — every triangle is a major or
-- | minor triad, edge-adjacent triangles share two tones (the P/L/R moves), and
-- | the active key's diatonic triads light up as a connected "spider".
-- | `LensLattices` shows every scale degree's full tertian lattice at once — the
-- | powerset web the keyboard's `l`-explode blooms one degree at a time — as
-- | seven compact clusters of chromatic-circle polygon glyphs (no stave), tiled
-- | in the zoomable container. A firehose meant to be roamed, not read at 1:1.
-- | `LensGenerate` is the tank-seeded generative surface: it takes chords caught
-- | in the tank as SEEDS and blooms a constellation of voice-led relatives around
-- | each one (the "shake the etch-a-sketch, put chords back in, grow what relates"
-- | idea). Catch a relative and it feeds the tank — the compositional loop closes.
-- |
-- | `LensPerform` is the PERFORM surface (docs/DESIGN-vetula-chyron-redesign §Perform):
-- | not another projection of the pool but a live rig — a row of player BOXES, one
-- | per output, onto which you drop saved sequence-tokens; each box loops its token
-- | while the transport plays. The function stack + non-MIDI sinks land on top later.
data StageLens = LensKeyboard | LensPadGrid | LensCircleFifths | LensTonnetz | LensLattices | LensGenerate | LensPerform

derive instance eqStageLens :: Eq StageLens

-- | The lens registry. A new lens appends here (+ a constructor + a render branch).
allLenses :: Array StageLens
allLenses = [ LensKeyboard, LensPadGrid, LensCircleFifths, LensTonnetz, LensLattices, LensGenerate, LensPerform ]

lensLabel :: StageLens -> String
lensLabel = case _ of
  LensKeyboard -> "keyboard"
  LensPadGrid -> "pad grid"
  LensCircleFifths -> "fifths"
  LensTonnetz -> "tonnetz"
  LensLattices -> "voice-leading lattice"
  LensGenerate -> "explore"
  LensPerform -> "perform"

-- | Where Vetula's chord/path AUDITION goes, chosen in the shell's routing modal
-- | (2026-08-01): Off (muted), Continuo (the piano+strings VST preview via the
-- | "continuo" virtual port), or Midi (the rig/IAC bus, on the preview channel).
-- | The shell drives this with SetAuditionQ; connectMidi picks the port from it.
data AuditionSel = AuditionOff | AuditionContinuo | AuditionMidi

derive instance eqAuditionSel :: Eq AuditionSel

-- | The color-overlay layers (2026-07-31 redesign — see
-- | docs/DESIGN-vetula-progression-building.md §"Context-panel redesign").
-- | The palettes stopped being MODE selectors that inject chords into the pool
-- | and became an always-on annotation layer: each set of chords is *painted*
-- | onto the geometric views in its own fixed hue, toggled independently. The
-- | diatonic triads are the base layer; borrowed comes from the BORROW scale;
-- | McMullen/Butler/Stock are the curated exterior signpost sets. Rendering the
-- | layers is Step 3 — this type + its state + the toggles are Step 2.
data ColorLayer = LayerDiatonic | LayerBorrowed | LayerMcMullen | LayerButler | LayerStock

derive instance eqColorLayer :: Eq ColorLayer
derive instance ordColorLayer :: Ord ColorLayer

-- | The layer registry, in legend order (base first).
allColorLayers :: Array ColorLayer
allColorLayers = [ LayerDiatonic, LayerBorrowed, LayerMcMullen, LayerButler, LayerStock ]

layerLabel :: ColorLayer -> String
layerLabel = case _ of
  LayerDiatonic -> "diatonic"
  LayerBorrowed -> "borrowed"
  LayerMcMullen -> "McMullen"
  LayerButler -> "Butler"
  LayerStock -> "Stock"

-- | Each layer's own distinct hue — a legend, NOT the tonnetz outside-distance
-- | ramp (AC decision 3, 2026-07-31). Diatonic is a quiet base ink; the color
-- | sets each get a saturated, legible hue that reads on parchment.
layerHue :: ColorLayer -> String
layerHue = case _ of
  LayerDiatonic -> "#6a6a6a"
  LayerBorrowed -> "#b5622d"
  LayerMcMullen -> "#3f7d54"
  LayerButler -> "#4a6da8"
  LayerStock -> "#8a5a9a"

-- | The chords a color layer paints for the current key: the diatonic triads,
-- | the borrow scale's interchange chords (only when a BORROW mode is chosen),
-- | or one of the curated exterior signpost sets. These are generated on the
-- | fly for annotation — they are NOT added to the pool (`st.chords`).
layerChords :: State -> ColorLayer -> Array ChordNode
layerChords st = case _ of
  LayerDiatonic -> diatonicTriads st.key
  LayerBorrowed -> case st.borrowMode of
    Just v -> interchangeChords (modeOf v) st.key
    Nothing -> []
  LayerMcMullen -> mcmullenChords st.key
  LayerButler -> butlerChords st.key
  LayerStock -> stockChords st.key

-- | A chord's short name from its root + quality (major bare, minor "m", else
-- | the bare root) — the token label on the color corona.
chordTag :: ChordNode -> String
chordTag c =
  let pc = mod c.root 12
  in noteName pc
       <> (if elem (mod (pc + 4) 12) c.pcs then ""
           else if elem (mod (pc + 3) 12) c.pcs then "m"
           else "")

-- | How a voice sounds the chord it's currently on. Block = the whole chord held
-- | for the step; Strummed = re-trigger only the notes that changed (common tones
-- | ring on); Arp = one chord note per pulse, cycling up.
data Renderer = Block | Strummed | Arp

derive instance eqRenderer :: Eq Renderer

-- | Where a voice's chord goes. ToMidi (the default) sounds it on the voice's
-- | MIDI channel per its renderer. ToOdonus sends NO MIDI: the voice becomes a
-- | block chord-conductor for Triggerfish's Odonus — it always runs (no mute),
-- | and the `channel` field is reused as the Odonus id the shell feeds. The
-- | standalone app only ever uses ToMidi (it has no Odonus to conduct).
data VoiceDest = ToMidi | ToOdonus

derive instance eqVoiceDest :: Eq VoiceDest

destName :: VoiceDest -> String
destName = case _ of
  ToMidi -> "→ midi"
  ToOdonus -> "→ odo"

cycleDest :: VoiceDest -> VoiceDest
cycleDest = case _ of
  ToMidi -> ToOdonus
  ToOdonus -> ToMidi

-- | A performance VOICE: its own read-head into the loaded progression (its own
-- | clock + offset, so voices can run locked or phased), a way to sound it, and
-- | a MIDI channel. The shared material is the progression; the position is the
-- | voice's own — overlapping pitch sets from one voice-led source.
type Voice =
  { id :: Int
  , channel :: Int          -- the Odonus id (ToOdonus). ToMidi voices no longer carry a
                            -- MIDI channel here — it comes from the routing map (default 5;
                            -- named voices bind on the Tidal page). See Triggerfish.Midi.Routing.
  , name :: String          -- optional routing NAME for a → midi voice ("" = unnamed → the
                            -- default channel). The Tidal page binds each name to a channel.
  , dest :: VoiceDest        -- MIDI out, or a block chord-conductor for Odonus
  , renderer :: Renderer
  , pattern :: String       -- the LIVE-CODED read-head: a Tidal mini-notation pattern of
                            -- chord indices ("0 1 2 3", "[0 1 2 3]/4", "0(3,8)"). Non-empty
                            -- overrides `durs` (Vetula.Playhead.clockFor); 1 cycle = 1 bar.
  , patternDraft :: String  -- the uncommitted edit buffer; `commit` copies it into `pattern`
                            -- atomically (never debounced), so one edit lands as one change.
  , notePattern :: String   -- Axis-B: a note-index pattern that sequences the CURRENT chord's
                            -- notes ("0 1 2 3" arp, "3" top voice, "[0 1 2 3]*4" fast). Empty
                            -- = use the block/arp/strum renderer instead.
  , notePatternDraft :: String
  , articulator :: RA.VArticulator  -- Axis-B alphabet: how the note-pattern's numbers are
                            -- read — `block` (the chord's own notes) or `voice-led` (a
                            -- smooth line carried through the loop, stable voice roles).
  , durs :: Array Int       -- LEGACY bars-per-chord dwell (one per progression chord; 0 =
                            -- skip). Used when `pattern` is empty, and still the rig-push
                            -- shape until the rig learns patterns (#77).
  , phase :: Int            -- pulse offset (the live-jump re-anchor), applied to either clock
  , cursor :: Int           -- the chord index it's currently on (derived, cached for display)
  , held :: Array Int       -- MIDI notes currently sounding (Strummed sustain / note-off on stop)
  , muted :: Boolean
  }

-- | A saved progression in the library: a self-contained copy (so it's immutable
-- | while a loaded working copy is revoiced live), tagged with its key for search.
-- A library entry's canonical form is its Tidal SOURCE (the "save the rendering"
-- rule) — all-strings, so it persists to localStorage with no ChordNode/Key/Mode
-- codecs and round-trips through the same render/parse as copy-paste + import.
type LibEntry =
  { name :: String
  , keyLabel :: String   -- the display label, e.g. "D major" (was structured Key)
  , source :: String     -- the progression rendered to Tidal source (the chords)
  , kept :: Boolean       -- promoted keeper (★, frozen) vs ephemeral auto-capture (◦, live)
  }

-- | The simulation only needs the layout targets + a size for collision.
type Row = (targetX :: Number, targetY :: Number, radius :: Number)
type VNode = SimulationNode Row

type Slots =
  ( keySelect :: Select.Slot Unit
  , scaleSelect :: Select.Slot Unit
  , familyScaleSelect :: Select.Slot Unit
  , borrowSelect :: Select.Slot Unit
  )

-- | An in-progress octave-drag of one voice on the left-hand pitch ladder.
-- | `voiceIx` indexes the sounding chord's `voicing` (uppers only — the bass
-- | dot is not draggable). `startMidi` is that voice's pitch at grab time; every
-- | move recomputes the new pitch as `startMidi + 12 * offset`, so the gesture
-- | is absolute, never accumulating drift.
type DragState =
  { chordId :: Int
  , voiceIx :: Int
  , startMidi :: Int
  , offset :: Int
  , horizontal :: Boolean   -- the progression rows drag along x; the Explore ladder along y
  , double :: Boolean       -- alt held at grab: leave the original pitch behind (a doubled tone)
  }

-- | A selected voice on the ladder, nudgeable with the arrow keys. The bass is
-- | distinct from the uppers because nudging it ROTATES it through the chord's
-- | existing tones (an inversion by ear) rather than shifting it by octaves.
data VoiceSel = BassVoice | UpperVoice Int

derive instance eqVoiceSel :: Eq VoiceSel

-- | An in-progress Tab-cycle through a chord's candidate voicings. Held until
-- | the voicing is edited by other means (drag / arrow), at which point the
-- | staleness check rebuilds it from the new voicing.
type VoicingCycle =
  { chordId :: Int
  , options :: Array (Array Int)
  , ix :: Int
  }

-- | One audition event on the CHYRON — the rolling harmonic capture buffer (see
-- | docs/DESIGN-vetula-chyron-redesign.md). Every single-chord audition (via
-- | `playChord`/`playSpecimen`, the only two audition choke-points) appends one
-- | of these, whether or not sound actually came out. `pcs` for the glyph/dedup,
-- | `notes` for exact replay, `at` (ms, `dateNow`) for the timing axis that a
-- | later lift can quantise or drop. The trace is what a progression gets LIFTED
-- | from retroactively, replacing build-a-progression-up-front.
-- |
-- | `anchor` carries the chord's *harmonic reading* (`Harmonia.Anchor`) — the one
-- | thing the raw notes cannot reconstruct: where the chord sits in a scale. This
-- | is what Explore needs to bloom the *right* neighbourhood around a captured
-- | chord (a progression can span scales, so the reading is per-chord, never
-- | per-buffer). `notes` already serves as the voicing (`bass : voicing`), so the
-- | anchor is the only enrichment the event needs over its notes. A chord minted
-- | with no reading (recall from a bare note-list) carries `Free`.
type ChyronEvent =
  { pcs    :: Array Int
  , notes  :: Array Int
  , label  :: String
  , at     :: Number
  , anchor :: Anchor
  }

-- | Cap on the rolling chyron buffer — oldest events fall off the left.
chyronCap :: Int
chyronCap = 128

-- | A SAVED sequence: a span lifted out of the live trace and compressed to a
-- | pinned 2-glyph token (its identity, from `glyphOf` over the sequence's
-- | content). Carries the full events so it can be replayed with timing and,
-- | later, `split` into (Progression, Timings). See DESIGN-vetula-chyron-redesign.
type SavedSeq =
  { events :: Array ChyronEvent
  , glyph :: Glyph
  }


-- | A PERFORM box: one persistent player slot on the Perform surface, bound to a
-- | MIDI channel. A dropped token LOOPS through its function `stack` (folded over
-- | the chord pattern) while the transport plays, out its terminal `term`. Empty or
-- | muted boxes are silent; a rig-only terminal is silent+ghosted in Solo.
type PerfBox =
  { channel :: Int
  , label   :: String
  , seq     :: Maybe SavedSeq
  , stack   :: Array Layer   -- ordered function layers (fx + when clause); arp/strum
                             -- among them carry the chord→time realisation (block = none)
  , seqText :: String     -- the TEXT HATCH: a mini-notation sequence over the token's
                          -- chord indices (cycle = 1 bar). "" = default (one/beat).
  , muted   :: Boolean    -- silence this pipeline without tearing it down
  , term    :: PerfTerm   -- the terminal sink: → midi | → odo | → rig
  }

-- | A box is GHOSTED when its terminal can't sound in the current authority — a
-- | rig-only sink anywhere but Atlantis (Rig). Ghosted boxes are silent and dimmed.
boxGhosted :: Sounding -> PerfBox -> Boolean
boxGhosted authority box = termRigOnly box.term && authority /= Rig

type State =
  { key :: Key
  -- The rig's resting harmonic scale (macro-tidal harmonic-authority): Nothing =
  -- follow the key's diatonic set; Just = an explicit `# scale` override (root pc
  -- + intervals from any Reef scale, beyond the diatonic modes the key can name).
  -- Vetula is the single harmonic authority — this is what pitched voices quantise
  -- to when no chord is firing.
  , restScale :: Maybe { root :: Int, offsets :: Array Int }
  , focus :: Focus                     -- Slice 4c: Hunt (lattice-dominant) vs Perform (rail-dominant)
  , railOpen :: Set RailSection        -- Slice 4b: which rail accordion sections are open (multi)
  , leftOpen :: Set LeftSection        -- which LEFT accordion sections are open (multi)
  , chords :: Array ChordNode          -- the model (pin, provenance, layout targets)
  , nodes :: Array VNode               -- live positions from the simulation
  , focusId :: Int
  , hoveredId :: Maybe Int
  , hoveredTriad :: Maybe { root :: Int, pcs :: Array Int }  -- Tonnetz hover (no pool id)
  , hoveredSpec :: Maybe SpecimenId  -- a hovered tank specimen (space previews it)
  , nextId :: Int
  , handle :: Maybe (SimulationHandle Row)
  , subId :: Maybe H.SubscriptionId
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , auditionSel :: AuditionSel      -- where the audition goes (Off/Continuo/Midi); shell-driven
  , previewChan :: Int              -- the MIDI channel chord/path AUDITION plays on (own
                                    -- routable channel, so ATLANTIS preview can be cued
                                    -- separately from the live brush; 0-indexed like voices)
  , sounding :: Maybe Int            -- the chord shown on the left-hand pitch ladder
  , revoicing :: Maybe Int           -- the chord open in the revoice modal (Nothing = closed)
  , drag :: Maybe DragState          -- an in-progress octave-drag on the ladder
  , selected :: Maybe VoiceSel       -- the ladder voice the arrow keys nudge
  , cycle :: Maybe VoicingCycle      -- Tab-cycle position through candidate voicings
  , favorites :: Map String (Array (Array Int))  -- kept voicings, keyed by note-set
  -- path-building on the Lattice: shift-click chords to grow a running sequence.
  -- A click in the same family BRIDGES (shortest single-note walk); a click in a
  -- different family LEAPS (an interconnector). Segments + interconnectors emerge
  -- as the smooth runs and the leaps of this one flat sequence.
  , path :: Array Int                -- the chords of the path so far, in order
  -- each summoned family's own scale (seed id → Key), so a family can be a
  -- different scale than the home one — the substrate for modulation. Diatonic
  -- seeds aren't listed; they default to the home key.
  , familyScale :: Map Int Key
  , focusedFamily :: Maybe Int        -- the family the per-family scale picker re-flavours
  -- the chord the next number / e stack climbs from, with the ABSOLUTE scale
  -- degree of its top note (may exceed the octave, so thirds keep climbing
  -- 7→9→11→13 instead of folding back when a tone repeats a pitch class).
  , stackHead :: Maybe { id :: Int, top :: Int }
  -- the exterior signpost sets currently dropped (generator key → the chord ids
  -- it added), so a button can toggle its set off again.
  , dropped :: Map String (Array Int)
  -- the modal-interchange source mode currently borrowed from (the picker's
  -- value; Nothing = none). Its chords live in `dropped` under "interchange".
  , borrowMode :: Maybe String
  -- the active color-overlay layers (2026-07-31 redesign): which palette sets
  -- are painted onto the geometric views, each in its own hue. Replaces the
  -- old "drop chords into the pool" palette mode. Rendered in Step 3.
  , colorLayers :: Set ColorLayer
  -- the tonnetz triad STACK (2026-07-31 redesign): triads accumulated by
  -- alt-clicking triangles, in pick order. Edge-adjacent triads fold into
  -- 7ths/9ths naturally (the polychord is the pitch-class union); the whole
  -- stack catches to the tank as one Anchor. Empty = not stacking.
  , tonnetzStack :: Array { root :: Int, pcs :: Array Int, major :: Boolean }
  -- chords reconstructed by pasting a saved Tidal progression back in. They live
  -- in `chords` (so the Revoice ladders + export work on them) but are kept off
  -- the Explore/Lattice surfaces — they aren't lattice nodes.
  , imported :: Set Int
  -- the Tidal-source textarea's verbatim content while the user is editing it
  -- (Nothing = show the live-derived source, which tracks revoicing).
  , sourceEdit :: Maybe String
  , sourceOpen :: Boolean   -- is the Tidal-source code revealed? (copy works either way)
  , helpOpen :: Boolean     -- is the ⓘ help overlay open? (the notes, off the canvas)
  -- "pick mode": shift-clicked progression step indices (max 2). When non-empty,
  -- the left surface shows a generated candidate cloud to insert/substitute.
  , genSel :: Array Int
  , candidates :: Array ChordNode
  , adventure :: Number      -- 0 = smoothest candidates … 1 = most striking
  -- ARRANGE (control B): how many bridge chords `Vetula.Between` lays in front of
  -- a tank chord as it's dropped into the progression — the "cadence length" dial
  -- (0 = drop it bare, 1 = V, 2 = ii–V, …). See docs/DESIGN-vetula-progression-building.md.
  , bridgeLen :: Int
  -- Floating-control fold state: each card collapses to just its header (click the
  -- title bar) to cede the stage to the underlying music viz. See `floatCard`.
  , foldCtx :: Boolean
  , foldProg :: Boolean
  -- Performance tab — the progression library + the loaded working copy + voices.
  , library :: Array LibEntry
  -- Auto-capture bookkeeping (Slice 1): the current progression is captured to the
  -- library on a slow timer — one ephemeral entry per building session, UPDATED in
  -- place as you build (deduped by `lastCapSig`), then frozen when you promote it.
  , capSeq :: Int                 -- running number for ephemeral autonames (◦N)
  , lastCapIdx :: Maybe Int       -- library index of the current session's ephemeral (Nothing = start fresh)
  , lastCapSig :: String          -- signature (currentSource) of the last capture, for dedup
  , libSearch :: String           -- filter the library by key
  , saveName :: String            -- name for the next saved progression
  , publishMsg :: Maybe String    -- transient status from a publish-entry-to-Amphora click
  -- Slice 4a — the live `path` IS the performed progression (no separate loaded
  -- working copy). `perfName` is just the title of the snapshot last restored into
  -- the path (Nothing = hand-built on the lattice); the chords come from `path`.
  , perfName :: Maybe String
  , voices :: Array Voice
  , armed :: Boolean              -- the ARM/cue flag (sticky). Vetula keeps its own arm
                                  -- lifecycle (standalone PerfPlay/PerfStop/unload); the
                                  -- shell mirrors it through SetSounding (`Silent` ⇒ disarm).
  , authority :: Sounding        -- where PERFORMANCE output goes (MISU refactor, replaces
                                  -- master+audible): Local = local Web-MIDI, Rig = muted
                                  -- locally + brush on the rig. Standalone stays Local.
  , playing :: Boolean           -- derived: currently sounding (= armed, under authority)
  , pulse :: Int                  -- the shared clock's 16th-note grid index (from the scheduler tick)
  , tempo :: Int                  -- BPM display (tracks the live clock; the bpm field nudges the free baseline)
  , binnacle :: Maybe Binnacle.Binnacle  -- the shared transport (free-run → Link-lock), like Odonus/Balistes
  , clockTempo :: Number          -- the clock's live tempo, read each tick (drives note durations)
  , nextVoiceId :: Int
  , routing :: Map String Int   -- name → canonical MIDI channel, pushed from the Tidal page
  -- Tank model (Slice A): the durable, unordered collection of CAUGHT chords.
  -- Frozen `Specimen`s reference no lattice node, so the volatile lattice can
  -- reflow/regenerate underneath without disturbing them. `k` over a chord catches
  -- it here; the tank persists until cleared and will feed the Stage + Sequences.
  , tank :: Array Specimen
  , nextSpecId :: Int             -- running number for minting SpecimenIds
  , lens :: StageLens             -- Slice C: which Stage lens is showing
  -- Geometric-lens viewport (CoF / Tonnetz): pan centre + zoom, applied as the
  -- surface's viewBox. Wheel zooms toward the cursor; drag pans; reset re-fits.
  , viewCx :: Number
  , viewCy :: Number
  , viewZoom :: Number
  , panning :: Maybe { ux :: Number, uy :: Number }  -- grabbed anchor point in user-space
  , panMoved :: Boolean            -- a real drag happened → swallow the ensuing click
  , genRoll :: Int                 -- Generate lens: the "shake" counter (re-rolls relatives)
  -- Tank model (Slice B): the staged seeds. Clicking a tank specimen injects it
  -- into the pool as a centre chord (`seedChord` maps the specimen → its pool
  -- chord id) and blooms its neighbours around it; clicking again unstages it.
  , seedChord :: Map SpecimenId Int
  -- The unified glyph-chip PRESET bank (docs/DESIGN-scene-modal.md): captured
  -- progression sources, anonymous or named, freely intermixed — distinct from the
  -- auto-capture `library`. `identity` is the parked preset's source text (the chip
  -- glyph; ghosts when the live progression diverges from it). No `lastChip` guard:
  -- Vetula reports its chip by PULL (AskChip), not a change-gated push.
  , presets :: Array Preset
  , identity :: Maybe String
  -- The CHYRON: an append-only (capped) log of everything auditioned this
  -- session, oldest→newest. Phase 1 = the ticker; Phase 2 adds interaction.
  -- See DESIGN-vetula-chyron-redesign.
  , chyron :: Array ChyronEvent
  -- Chyron interaction. `hoveredChyron` = the chip index under the pointer (space
  -- auditions it, no re-log). `chyronSel` = the current selection, Mac text-editing
  -- semantics: a plain click drops a fresh single-chord selection (`lo==hi`) and
  -- sets the `anchor`; a shift-click extends the range from that fixed anchor
  -- (anchor stays put, the clicked chip becomes the moving end). `lo`/`hi` are the
  -- sorted span endpoints the render/save/play all read; `anchor` is the fixed end
  -- a subsequent shift-click re-extends from.
  , hoveredChyron :: Maybe Int
  , chyronSel :: Maybe { lo :: Int, hi :: Int, anchor :: Int }
  -- Saved sequences: pinned 2-glyph tokens on the left of the chyron. Saving a
  -- selection compresses its live chips into one of these (reclaiming space).
  , chyronSaved :: Array SavedSeq
  -- Record-arm: when false, auditions still SOUND but don't log to the trace
  -- (noodle without cluttering). Defaults true — always-on capture, the flow AC
  -- liked; disarm only when you want to explore off the record.
  , chyronArmed :: Boolean
  -- PERFORM surface: player boxes (one per output) + the token "picked up" for
  -- placement (shift-click / drag a saved token, then click / drop on a box), and
  -- an fx "picked up" from the palette for placement onto a box's stack.
  , perfBoxes :: Array PerfBox
  , perfHeld :: Maybe Int
  , perfHeldFx :: Maybe PerfFx
  , perfDrag :: Maybe PerfDragSrc   -- the in-flight HTML5 drag payload
  , perfEditBox :: Maybe Int        -- box whose sequence is open in the editor modal
  -- The persistent Perform SESSION: the container for saved scenes. Resumes across
  -- reloads; scenes save as `⟨alias|name⟩ #nextScene`. Minted/restored in Initialize.
  , perfSession :: Store.SessionState
  -- Recall: scenes fetched from Amphora (collection `vetula-scene`), + modal flag.
  , perfScenes :: Array { hash :: String, name :: String, payload :: String, tags :: Array String }
  , perfRecallOpen :: Boolean
  }

-- | Which floating control a fold toggle targets.
data VPanel = VCtx | VProg

data Action
  = Initialize
  | MidiReady (Maybe Midi.MidiOut) String
  | RetryMidi              -- (re)request Web-MIDI access from a user gesture (chip click)
  | SimTick
  | SimDone
  | Hover (Maybe Int)
  | HoverTriad (Maybe { root :: Int, pcs :: Array Int })  -- Tonnetz: hover a triad for space-preview
  | HoverSpec (Maybe SpecimenId)  -- hover a tank specimen for space-preview
  | SetFocus Focus         -- Slice 4c: switch the surface width-focus (Hunt / Perform)
  | ToggleRailSection RailSection  -- Slice 4b: open/close a rail accordion section
  | ToggleLeftSection LeftSection  -- open/close a left accordion section
  | Key String Boolean     -- key, shift held
  | SelectKey String
  | SelectScale String
  | DragStart Boolean Boolean Int Int Int  -- alt (double)?, horizontal?, chord id, voicing index, MIDI at grab time
  | DragMove Event
  | DragEnd
  | SelectVoice Int VoiceSel -- chord id, the voice to select (and make sounding)
  | ToggleFavorite         -- star / unstar the sounding chord's current voicing
  | PickVoicing Int (Array Int) -- chord id, the favoured voicing to activate
  | PathPick Int           -- shift-click a Lattice chord: arm / complete a path segment
  | SummonRoot Int         -- click a keyboard key: summon (or focus) a family on that pc
  | OpenRevoice            -- open the revoice modal on the hovered/sounding chord
  | CloseRevoice           -- dismiss the revoice modal
  | SlashBass Int          -- set the revoiced chord's bass to a pitch class (slash chord)
  | DropSet String         -- toggle an exterior signpost set (McMullen …)
  | ToggleLayer ColorLayer -- toggle a color-overlay layer on/off (2026-07-31)
  | StackTriad Int (Array Int) Boolean -- alt-click a Tonnetz triad: add/remove it from the stack
  | CommitStack            -- catch the accumulated Tonnetz stack to the tank as one Anchor
  | ClearStack             -- discard the Tonnetz stack
  | BorrowFrom String      -- modal interchange: borrow from a parallel mode (or off)
  | ReflavourFamily String -- re-flavour the focused family's scale (mode value)
  | PlayPath               -- ▶ play the whole progression
  | ClearPath              -- ✕ empty the progression so the next shift-click starts fresh
  | PlayStep Int           -- hear one step (and make it the active chord)
  | CopyTidal String       -- copy the progression's Tidal source to the clipboard
  | EditSource String      -- the Tidal-source textarea was edited (freeze the live view)
  | LoadSource             -- parse the textarea + rebuild the progression from it
  | RevertSource           -- discard edits, go back to the live-derived source
  | ToggleSource           -- reveal / hide the Tidal source code
  | ToggleHelp             -- open / close the ⓘ help overlay
  | StepClick Int Boolean  -- a progression row clicked (index, shift) — play, or arm pick mode
  | PickCandidate Int      -- insert/substitute the chosen candidate into the progression
  | CancelGen              -- leave pick mode
  | SetAdventure String    -- the adventurousness dial (slider value)
  -- Performance tab
  | SetSaveName String
  | SaveProg               -- promote the current Lattice path to the library (a keeper)
  | AutoCapture            -- timer: auto-capture the current path (ephemeral, update-in-place)
  | KeepLib Int            -- promote / demote a library entry between keeper and ephemeral
  | SetLibSearch String
  | LoadProg Int           -- load library entry #i into the performance working copy
  | UnloadProg             -- back to the library
  | DeleteLib Int          -- remove a library entry
  | PublishLib Int         -- publish library entry #i to the Amphora store (vetula-progression)
  | SaveScene              -- serialise the whole Perform surface as a vetulaScene → Amphora
  | PerfNewSession         -- mint a fresh session glyph-triple (rolls the scene counter)
  | PerfOpenRecall         -- fetch saved scenes from Amphora + open the recall modal
  | PerfCloseRecall
  | PerfLoadScene String   -- parse a scene payload and load it onto the surface
  | AddVoice
  | RemoveVoice Int
  | SetVoiceChannel Int String
  | SetVoiceName Int String        -- name a → midi voice (routing identity; "" = unnamed)
  | SetPreviewChan String        -- set the chord/path audition channel
  | CycleVoiceDest Int
  | CycleVoiceRenderer Int
  | BumpCell Int Int Boolean    -- voice id, chord index, shift-held (down) — set a cell's bars
  | SetVoicePhase Int String
  | SetVoicePattern Int String     -- edit a voice's read-head (chord) pattern draft
  | SetVoiceNotePattern Int String -- edit a voice's Axis-B note-index pattern draft
  | CycleVoiceArticulator Int      -- cycle how the note-pattern reads (block / voice-led)
  | CommitVoicePattern Int         -- commit BOTH drafts into the live patterns (atomic)
  | ToggleVoiceMute Int
  | SetTempo String
  | ToggleArm              -- the ▶/■ button: sticky arm/cue under the shell master
  | PushVetula             -- lockstep: push the whole performance to the rig (→ odo conducts reef_voice)
  | PushBrush              -- Option B: push the progression's voicings as a real Tidal Pattern (reef_vetula_brush)
  | PerfPlay
  | PerfStop
  | PerfTick Scheduler.Tick  -- one 16th-note pulse from the shared scheduler
  | SelectPerfChord Int    -- click a working-copy chord row (for live Tab-revoice)
  -- Tank model (Slice A)
  | PlayChordId Int        -- plain-click a pool chord: audition it (no path change)
  | CatchChord Int         -- freeze lattice chord #id into the tank as a Specimen
  | DeleteSpec SpecimenId  -- × a tank specimen
  | AuditionSpec SpecimenId -- shift-click a tank specimen: hear it (no state change)
  | StageSpec SpecimenId   -- click a tank specimen: seed the pool with it (toggle)
  | SequenceSpec SpecimenId -- shift-click a tank specimen: append a snapshot to the progression
  | ArrangeSpec SpecimenId  -- drop a tank chord into the progression, bridged by `bridgeLen`
  | SetBridgeLen Int        -- set the cadence-length dial (clamped 0..maxBridge)
  | ToggleFold VPanel       -- collapse/expand a floating control to just its header
  | ClearStage             -- remove all staged seeds + the chords bloomed from them
  | AuditionTriad Int (Array Int)        -- Tonnetz: hear a triad off the net (root pc, pcs)
  | CatchTriad Int (Array Int) Boolean   -- Tonnetz: freeze a triad into the tank (root, pcs, isMajor)
  | AuditionNode ChordNode               -- Lattices: hear a generated chord (its own voicing)
  | CatchNode ChordNode                  -- Lattices: freeze a generated chord into the tank
  -- Chyron (Phase 2): hover a chip (space auditions it), or click one — plain
  -- click builds the selection span, shift-click lifts (to the tank for now).
  | HoverChyron (Maybe Int)
  | ChyronClick Int Boolean
  | DeleteChyron Int       -- × a single audition out of the trace
  | ClearChyron            -- wipe the whole audition trace
  | SaveChyronSel          -- compress the selection into a pinned 2-glyph token
  | PlaySaved Int          -- replay a pinned saved sequence (with its timing)
  | DeleteSaved Int        -- × a pinned saved sequence
  | ToggleChyronArm        -- record-arm the chyron on/off
  -- PERFORM surface
  | PerfPickup Int         -- pick up saved token i for placement (toggle)
  | PerfDropBox Int        -- place the held token/fx onto box i
  | PerfClearBox Int       -- empty box i (stop its loop)
  | PerfDragOver DragEvent -- allow HTML5 drop onto a box (preventDefault)
  | PerfPickFx PerfFx      -- pick up an fx from the palette for placement (toggle)
  | PerfFxNudge Int Int Int -- nudge box b's stack layer i by delta
  | PerfFxRemove Int Int   -- remove box b's stack layer i
  | PerfFxAlt Int Int      -- box b, layer i: alternate control (arp cycles direction)
  | PerfFxWhen Int Int     -- box b, layer i: cycle the when clause (always / every n)
  | PerfSetTerm Int PerfTerm -- set box b's terminal sink directly
  | PerfSetSeq Int String  -- edit box b's text-hatch sequence (mini-notation) only
  | PerfSetPipeline Int String -- edit box b's WHOLE pipeline text (seq # layers)
  | PerfOpenEdit Int       -- open the sequence editor modal for box b
  | PerfCloseEdit          -- close the sequence editor modal
  | PerfNop                -- no-op (used to stop a click bubbling without a re-render)
  | PerfToggleMute Int     -- silence/unsilence box b's pipeline
  | PerfDragStart PerfDragSrc -- begin an HTML5 drag of a palette/box layer
  | PerfDropOnChip DragEvent Int Int -- drop the dragged layer before box b's chip i
  | PerfDragEnd            -- clear the drag payload (drop landed or was abandoned)
  | PerfStopClick ME.MouseEvent Action -- run Action but stop the click bubbling to the box
  | ZoomAt Event Number    -- geometric lens: wheel-zoom toward the cursor (event, deltaY)
  | PanStart Event         -- geometric lens: begin a grab-to-pan drag
  | PanMove Event          -- geometric lens: drag the viewport
  | PanEnd                 -- geometric lens: end the pan drag
  | ResetView              -- geometric lens: re-fit (zoom 1, centred)
  | ShakeGenerate          -- Generate lens: re-roll the tank-seeded relatives
  | SetLens StageLens      -- Slice C: switch the Stage lens
  | TransposeSpec SpecimenId Int -- Slice E: shift one tank specimen by n semitones (in place)
  | CapoTank Int           -- Slice E: shift the WHOLE tank by n semitones (a capo)

-- | The queries the Triggerfish shell pulls from Vetula: its current Tidal
-- | source (for the aggregate TIDAL tab) and its current progression as PC sets
-- | (for the Odonus chord-quantiser feed). Defined here (not imported from
-- | Triggerfish) so the standalone app — which never queries it — still builds.
data SourceQuery a
  = AskSource (String -> a)
  | AskChords (Array (Array Int) -> a)
  | AskVoiceChords (Array { id :: Int, pcs :: Array Int } -> a)  -- live per-Odonus-voice chord
  | AskHarmonic ({ durs :: Array Int, active :: Int, chord :: String } -> a)  -- nav harmonic strip: voice-0 dwell schedule + live playhead + the active chord's notes
  | AskBrushSig (String -> a)   -- the current rig payload string; the shell diffs it to auto-re-push on change
  -- Live jump: re-anchor every voice so chord `i` reads NOW (from the nav strip).
  -- Playing → the ensemble advances to chord i and continues; stopped → the → odo
  -- feed + the strip's playhead move to i (so Odonus re-quantises). One gesture,
  -- both effects. See jumpVoice.
  | JumpChord Int a
  -- The ONE transport query (control-surface MISU refactor). The shell pushes the
  -- derived `Sounding`: `Silent` disarms, `Local` plays local Web-MIDI, `Rig` mutes
  -- locally + (re)pushes the brush to the rig voice. Replaces SetMaster/SetAudible/
  -- SetArm/SyncToRig/StopRig. `AskSounding` reports the EFFECTIVE sounding (Silent
  -- when self-disarmed, e.g. unloading a progression) so the shell can reconcile.
  | SetSounding Sounding a
  | AskSounding (Sounding -> a)
  | SyncFree Number Number a    -- adopt the rack's shared free-run baseline (start micros, BPM)
  | AskLibrary (Array { name :: String, text :: String } -> a)   -- A5 manager
  | LoadEntry Int a
  | ImportText String (Boolean -> a)
  -- MIDI routing (Tidal-page channel map). The shell pushes the name → channel
  -- bindings; the page asks which → midi voice names are in use so it can list them.
  | SetRouting (Array { name :: String, ch :: Int }) a
  | AskVoiceNames (Array String -> a)
  -- The chord/path AUDITION channel, surfaced in the shell's routing modal now
  -- that Vetula's voice card is gone. Canonical 1..16 across the query boundary
  -- (Vetula stores WebMIDI 0..15 internally).
  | AskPreviewChan (Int -> a)
  | SetPreviewChanC Int a
  -- Where the audition goes (routing modal's per-machine cycle): Off/Continuo/Midi.
  | SetAuditionQ AuditionSel a
  -- macro-tidal harmonic authority. The shell polls the rig's resting harmonic
  -- context (the key's diatonic set, or a `# scale` override) and pushes it into
  -- Odonus's pitchSet. `SetRestingScale` is where the macro `# scale` verb lands
  -- (root pc + intervals) — Vetula owns the scale, every pitched voice follows.
  -- The harmonic context Odonus quantises to — ONE set (chord-or-scale), per the
  -- rule in `harmonicContext`. `SetRestingScale` is the macro `# scale` override.
  | AskContextScale ({ root :: Int, offsets :: Array Int } -> a)
  | SetRestingScale Int (Array Int) a
  -- The unified glyph-chip PRESET bank (docs/DESIGN-scene-modal.md). Vetula reports
  -- its chip by PULL (`AskChip`, polled by the shell's 100ms PollVetula loop) rather
  -- than a push Output, since it has no continuous frame loop. `content` is the
  -- progression's Tidal source (`currentSource`). Capture/recall/star/cull mirror the
  -- other machines; the bank is distinct from Vetula's own auto-capture library.
  | Capture a
  | AskBank (Array { slot :: Int, alias :: String, name :: String, starred :: Boolean } -> a)
  | RecallSlot Int a
  | StarSlot Int a
  | DeleteSlot Int a
  | AskChip (Maybe ChipView -> a)

-- The one thing Vetula tells the shell without being asked: it armed or disarmed
-- itself (its own play / stop / unload). The shell owns the `armed` set, so this
-- event lets it update membership directly — replacing the old per-tick poll of
-- every instrument's effective sounding. Odo/Bal/Sel never self-disarm, so only
-- Vetula needs an output.
data Output = ArmChanged Boolean

component :: forall i m. MonadAff m => H.Component SourceQuery i Output m
component = H.mkComponent
  { initialState: \_ ->
      { key: cMajorKey
      , restScale: Nothing
      , focus: Hunt
      , railOpen: Set.fromFoldable [ SecProgression, SecLibrary, SecVoices ]
      , leftOpen: Set.fromFoldable [ SecSetup, SecTank, SecLens ]
      , chords: []
      , nodes: []
      , focusId: 0           -- the first diatonic triad seed
      , hoveredId: Nothing
      , hoveredTriad: Nothing
      , hoveredSpec: Nothing
      , nextId: 100          -- generated children start here; seeds are 0..17
      , handle: Nothing
      , subId: Nothing
      , midiOut: Nothing
      , midiName: "…"
      , auditionSel: AuditionContinuo   -- audition through the Continuo VST by default
      -- chord/path auditions default to the canonical Vetula channel (MIDI ch 5,
      -- where the standard config parks a pad/strings) — `playChord` sends this raw
      -- to WebMIDI, so it's the 0-indexed toWire form of the canonical constant.
      , previewChan: Routing.toWire Routing.vetulaDefaultChannel
      , sounding: Just 0
      , revoicing: Nothing
      , drag: Nothing
      , selected: Nothing
      , cycle: Nothing
      , favorites: Map.empty
      , path: []
      , familyScale: Map.empty
      , focusedFamily: Nothing
      , stackHead: Nothing
      , dropped: Map.empty
      , borrowMode: Nothing
      , colorLayers: Set.singleton LayerDiatonic
      , tonnetzStack: []
      , imported: Set.empty
      , sourceEdit: Nothing
      , sourceOpen: false
      , helpOpen: false
      , genSel: []
      , candidates: []
      , adventure: 0.25
      , bridgeLen: 2
      , foldCtx: false
      , foldProg: false
      , library: []
      , capSeq: 0
      , lastCapIdx: Nothing
      , lastCapSig: ""
      , libSearch: ""
      , saveName: ""
      , publishMsg: Nothing
      , perfName: Nothing
      -- the four fixed lanes of the bottom voice bar, all MUTED (one toggle from
      -- sounding). See `canonicalVoices`.
      , voices: canonicalVoices 0
      , armed: false
      -- standalone Vetula has no shell, so authority defaults Local (the play button
      -- works as a direct local transport). Inside Triggerfish the shell drives it via
      -- SetSounding (Silent on init since nothing is armed, then Local/Rig on arm).
      , authority: Local
      , playing: false
      , pulse: -1
      , tempo: 120
      , binnacle: Nothing
      , clockTempo: 120.0
      , nextVoiceId: 4
      -- name → canonical MIDI channel, pushed from the shell's Tidal-page routing
      -- table (SetRouting). Unnamed / unbound voices fall back to the default channel.
      , routing: Map.empty :: Map String Int
      , tank: []
      , nextSpecId: 0
      , seedChord: Map.empty
      , presets: [], identity: Nothing
      , lens: LensTonnetz  -- default: the tonal net shows the scale's shape best
      , viewCx: 0.0
      , viewCy: 0.0
      , viewZoom: 1.0
      , panning: Nothing
      , panMoved: false
      , genRoll: 0
      , chyron: []
      , hoveredChyron: Nothing
      , chyronSel: Nothing
      , chyronSaved: []
      , chyronArmed: true
      -- four player boxes on MIDI ch 1-4 (Odonus I-IV in AC's routing); a token
      -- dropped on one loops there while the transport plays.
      , perfBoxes: map (\n -> { channel: n, label: "P" <> show n, seq: Nothing, stack: [], seqText: "", muted: false, term: TMidi }) (range 1 4)
      , perfHeld: Nothing
      , perfHeldFx: Nothing
      , perfDrag: Nothing
      , perfEditBox: Nothing
      -- placeholder; Initialize resumes the persisted session or mints a fresh one
      , perfSession: { alias: "", name: "", nextScene: 1 }
      , perfScenes: []
      , perfRecallOpen: false
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
  }

-- | Answer the shell: the live progression as Tidal (TIDAL tab), or as a
-- | sequence of pitch-class sets (the Odonus chord-quantiser feed).
handleQuery :: forall m a. MonadAff m => SourceQuery a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (currentSource s)))
  AskChords reply -> do
    s <- H.get
    pure (Just (reply (progressionPCs s)))
  -- The live Odonus follow bridge: each ToOdonus voice's CURRENT block chord,
  -- keyed by the voice's channel field (reused as the Odonus id). The shell polls
  -- this ~100ms and feeds it to Odonus's quantiser.
  AskVoiceChords reply -> do
    s <- H.get
    pure (Just (reply (voiceChordFeed s)))
  -- The nav harmonic-context strip: the AUTHORITATIVE voice's dwell schedule padded to
  -- the progression length (bars-per-chord; 0 = a skip), and the live playhead — the
  -- chord index that voice's read-head sits on at the current pulse (-1 = none). The
  -- shell polls this and renders the glyph in the top bar, visible in every pane.
  -- With several heads at different positions there is no single "current chord", so
  -- `harmonicVoice` only names one when it is unambiguous (a lone voice, or an Odonus
  -- conductor) — otherwise `active = -1`/`chord = ""` and the top bar asserts nothing.
  AskHarmonic reply -> do
    s <- H.get
    let cs = perfChords s
        n = length cs
        mv = harmonicVoice s
        durs = maybe (replicate n 1) (\v -> displayDurs n (voiceClock n v)) mv
        active = fromMaybe (-1) (mv >>= cursorAt cs s.pulse)
        -- The active chord's notes, bass-up as note names (unique pitch classes in
        -- voicing order) — the compact echo of the progression row's pitch ladder.
        chord = case cs !! active of
          Just c -> joinWith " " (map noteName (nub (map (\x -> mod x 12) (playNotes c))))
          Nothing -> ""
    pure (Just (reply { durs, active, chord }))
  -- The exact rig payload string the progression would push (Vetula has no
  -- incremental path, so this whole string IS the wire). The shell diffs it each
  -- poll and, in ATLANTIS, calls SyncToRig on a settled change — no manual push.
  AskBrushSig reply -> do
    s <- H.get
    pure (Just (reply (brushMsg s)))
  -- Live jump from the nav strip: re-anchor every voice's read-head to chord i at
  -- the current pulse (phase set so cursorAt = i now; cursor cached to i for the
  -- stopped/feed case). No-op for voices that skip chord i, and for i out of range.
  JumpChord i next -> do
    st <- H.get
    let n = length (perfChords st)
    H.modify_ _ { voices = map (jumpVoice n st.pulse i) st.voices }
    pure (Just next)
  -- The ONE transport query (control-surface MISU refactor). The shell pushes the
  -- derived `Sounding`; we fold it into Vetula's own arm lifecycle and act:
  --   * Silent ⇒ disarm; Local/Rig ⇒ arm (reconcilePerf starts/stops the ticker).
  --   * leaving Local  → silence held local notes (the clock keeps ticking; audition
  --     previews stay local and ungated — the palette's sample-the-harmony gesture).
  --   * entering Rig   → (re)push the brush as a Tidal pattern (re-issuing Rig
  --     re-voices — the shell does this on a settled edit); leaving Rig → vetula-stop.
  SetSounding s next -> do
    st <- H.get
    let nowArmed = s /= Silent
    when (st.authority == Local && s /= Local && st.playing) do
      silenceHeld st
      H.modify_ \s' -> s' { voices = map (_ { held = [] }) s'.voices }
    H.modify_ _ { authority = s }
    when (nowArmed /= st.armed) do
      H.modify_ _ { armed = nowArmed }
      reconcilePerf
    when (st.authority == Rig && s /= Rig) $
      for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "vetula-stop"
    when (s == Rig) $
      for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) (brushMsg st)
    pure (Just next)
  -- Report the EFFECTIVE sounding: Silent when self-disarmed (unload) so the shell
  -- drops us from its armed set; otherwise the pushed authority.
  AskSounding reply -> do
    st <- H.get
    pure (Just (reply (if st.armed then st.authority else Silent)))
  -- The rack's shared free-run baseline: adopt it so Vetula shares the same
  -- downbeat (and tempo) as Odonus/Balistes with no rig.
  SyncFree startMicros tempo next -> do
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  -- A5 cross-instrument library manager (the Triggerfish shell). Each library
  -- entry renders to its voiced Tidal source; import rebuilds chords from a pasted
  -- progression (the same parse path as paste-and-load) and appends an entry.
  AskLibrary reply -> do
    s <- H.get
    pure (Just (reply (map (\e -> { name: e.name, text: e.source }) s.library)))
  LoadEntry i next -> do
    handleAction (LoadProg i)
    pure (Just next)
  ImportText txt reply -> do
    st <- H.get
    let noteLists = filter (\ns -> length ns > 0) (parseProgression txt)
    if length noteLists == 0 then pure (Just (reply false))
    else do
      let chords = mapWithIndex importChord noteLists
      H.modify_ \s -> s { library = s.library <> [ { name: "imported", keyLabel: groupLabel st.key, source: progressionSource (groupLabel st.key) chords, kept: true } ] }
      persistLib
      pure (Just (reply true))
  SetRouting binds next -> do
    H.modify_ _ { routing = Map.fromFoldable (map (\b -> Tuple b.name b.ch) binds) }
    pure (Just next)
  AskVoiceNames reply -> do
    s <- H.get
    let names = nub (filter (_ /= "") (map _.name (filter (\v -> v.dest == ToMidi) s.voices)))
    pure (Just (reply names))
  -- macro-tidal harmonic authority: hand the shell the harmonic context set.
  AskContextScale reply -> do
    s <- H.get
    pure (Just (reply (harmonicContext s)))
  -- The macro `# scale` verb: install an explicit resting scale (any Reef scale).
  SetRestingScale root offsets next -> do
    H.modify_ _ { restScale = Just { root: mod root 12, offsets } }
    pure (Just next)
  -- The shell's CAPTURE hotkey: bank the live progression source as a preset and
  -- park identity on it (the chip shows the freshly-minted glyph, held). No-op with
  -- an empty progression. See captureNow.
  Capture next -> do
    captureNow
    pure (Just next)
  -- The status-board chip's recall menu: report each preset as its glyph alias +
  -- optional name + star flag; recall / star / delete a chosen preset.
  AskBank reply -> do
    s <- H.get
    pure (Just (reply (mapWithIndex (\i p -> { slot: i, alias: presetAlias p, name: fromMaybe "" p.name, starred: p.starred }) s.presets)))
  RecallSlot i next -> do
    recallPreset i
    pure (Just next)
  StarSlot i next -> do
    H.modify_ \s -> s { presets = fromMaybe s.presets (modifyAt i (\p -> p { starred = not p.starred }) s.presets) }
    persistLib
    pure (Just next)
  DeleteSlot i next -> do
    H.modify_ \s -> s { presets = fromMaybe s.presets (deleteAt i s.presets) }
    persistLib
    pure (Just next)
  -- The pull the shell's 100ms PollVetula uses to light Vetula's status-board chip.
  AskChip reply -> do
    s <- H.get
    pure (Just (reply (chipViewOf s)))

  -- Audition channel, canonical 1..16 (stored 0..15).
  AskPreviewChan reply -> do
    s <- H.get
    pure (Just (reply (s.previewChan + 1)))
  SetPreviewChanC ch next -> do
    H.modify_ _ { previewChan = clamp 0 15 (ch - 1) }
    pure (Just next)
  SetAuditionQ sel next -> do
    H.modify_ _ { auditionSel = sel }
    case sel of
      AuditionOff -> H.modify_ _ { midiOut = Nothing, midiName = "muted" }
      _ -> connectMidi   -- re-pick the output port (continuo vs IAC) for the new mode
    pure (Just next)

-- | The ONE harmonic-context set Odonus quantises to (root pc + intervals). The
-- | rule, in precedence order — the decoupling of "Vetula's lens scale" from "what
-- | Odonus quantises to", honouring descriptive-not-prescriptive (a progression's
-- | chords are free of any scale, so the CHORD itself is the set):
-- |
-- |   1. an explicit `# scale` override (the user deliberately imposed a scale);
-- |   2. a loaded progression → its ACTIVE chord's pitch classes (current chord
-- |      when playing, else the sounding/first chord) — chord-quantise, not scale;
-- |   3. otherwise → the lens scale (`st.key`), which re-quantises live as the user
-- |      changes the scale they're browsing.
-- |
-- | (Free-auditioning arbitrary chords with no progression falls into case 3 — the
-- | lens scale — which we accept: unrelated chords can't relate to Odonus. A future
-- | "clever layer" could look at the whole progression holistically — leading
-- | tones, Harmonia-driven expansion — to widen case 2 past bare arpeggiation.)
harmonicContext :: State -> { root :: Int, offsets :: Array Int }
harmonicContext st = case st.restScale of
  Just rs -> rs
  Nothing -> case activeChordPcs st of
    Just pcs | length pcs > 0 -> pcsToSet pcs
    _ -> { root: mod st.key.tonic 12
         , offsets: map (\pc -> mod (pc - st.key.tonic + 12) 12) (scaleSet st.key) }

-- | The active chord of a loaded progression as pitch classes: the chord under the
-- | playhead when playing, else the sounding chord, else the first — `Nothing` when
-- | no progression is loaded (empty path).
activeChordPcs :: State -> Maybe (Array Int)
activeChordPcs st
  | length st.path == 0 = Nothing
  | otherwise =
      let cs = perfChords st
          byPulse =
            if st.playing then (harmonicVoice st >>= cursorAt cs st.pulse) >>= (cs !! _)
            else Nothing
          bySounding = st.sounding >>= \sid -> find (\c -> c.id == sid) cs
          chosen = case byPulse of
            Just c -> Just c
            Nothing -> case bySounding of
              Just c -> Just c
              Nothing -> head cs
      in (\c -> nub (map (\x -> mod x 12) (playNotes c))) <$> chosen

-- | A set of pitch classes → a PitchSet payload (lowest pc as root, ascending
-- | intervals up from it). Order/duplicates normalised.
pcsToSet :: Array Int -> { root :: Int, offsets :: Array Int }
pcsToSet pcs = case sort (nub (map (\x -> mod x 12) pcs)) of
  sorted -> case head sorted of
    Just r -> { root: r, offsets: map (_ - r) sorted }
    Nothing -> { root: 0, offsets: [ 0 ] }

-- | The current path as one PC set per step (each chord's absolute pitch
-- | classes) — what Odonus's quantiser snaps to when fed from Vetula.
progressionPCs :: State -> Array (Array Int)
progressionPCs st = mapMaybe (\pid -> _.pcs <$> find (\c -> c.id == pid) st.chords) st.path

-- | The single voice (if any) whose read-head may honestly stand for "the" harmonic
-- | context in the top bar. An Odonus-feeding voice wins outright — it conducts the
-- | quantiser, so it IS the harmonic reading regardless of what the MIDI heads are
-- | doing. Failing that, a lone voice is unambiguous. Two-or-more MIDI voices at
-- | different positions have no single current chord, so we name none (`Nothing`).
harmonicVoice :: State -> Maybe Voice
harmonicVoice st =
  case head (filter (\v -> v.dest == ToOdonus) st.voices) of
    Just v -> Just v
    Nothing -> case st.voices of
      [ v ] -> Just v
      _ -> Nothing

-- | Each Odonus-bound voice's CURRENT block chord as pitch classes, keyed by its
-- | channel (reused as the Odonus id). The shell polls this ~100ms and feeds it
-- | to Odonus's follow selector. A ToMidi voice contributes nothing.
voiceChordFeed :: State -> Array { id :: Int, pcs :: Array Int }
voiceChordFeed st =
  let chords = perfChords st
      voices = mapMaybe
        (\v -> if v.dest == ToOdonus
                 then (\c -> { id: v.channel, pcs: c.pcs }) <$> index chords v.cursor
                 else Nothing)
        st.voices
  in voices <> perfBoxOdoFeed st

-- | Perform boxes whose terminal is `→ odo` contribute their CURRENT block chord
-- | to the Odonus feed (keyed by the box's channel, reused as the Odonus id) — the
-- | same conductor role a `ToOdonus` voice plays. Muted / ghosted / empty boxes and
-- | non-odo terminals don't feed.
perfBoxOdoFeed :: State -> Array { id :: Int, pcs :: Array Int }
perfBoxOdoFeed st =
  mapMaybe
    (\box ->
       if box.term == TOdo && not box.muted && isJust box.seq && not (boxGhosted st.authority box)
         then case boxCurrentChord box (st.pulse / 4) of
                Just notes | length notes > 0 -> Just { id: box.channel, pcs: nub (map (\x -> mod x 12) notes) }
                _ -> Nothing
         else Nothing)
    st.perfBoxes

-- | The chord a box is sounding at beat-cycle `b` — the first digital event of its
-- | folded pattern over that cycle (the block chord Odonus would quantise).
boxCurrentChord :: PerfBox -> Int -> Maybe (Array Int)
boxCurrentChord box b =
  eventValue <$> head (filter isDigital (query (boxPattern box) (mkState (mkArc (Rat.fromInt b) (Rat.fromInt (b + 1))))))

-- ---------------------------------------------------------------------------
-- Force layout
-- ---------------------------------------------------------------------------

-- | x is pinned HARD to the target; y is firm so the extension strata hold their
-- | rows (a gentler y would let collision beeswarm chords sharing a key).
vetulaForceSetup :: Setup VNode
vetulaForceSetup =
  setup "vetula"
    [ positionX "px" # withX (dynamic _.targetX) # withStrength (static 0.12)
    , positionY "py" # withY (dynamic _.targetY) # withStrength (static 0.55)
    , link "neighbours" # withDistance (static 46.0) # withStrength (static 0.3)
    , collide "collide" # withRadius (dynamic (\n -> n.radius + 6.0)) # withStrength (static 0.9)
    , manyBody "charge" # withStrength (static (-8.0))
    ]

mkSimNode :: ChordNode -> VNode
mkSimNode c =
  { id: c.id
  , x: c.targetX, y: c.targetY, vx: 0.0, vy: 0.0
  , fx: (null :: Nullable Number), fy: (null :: Nullable Number)
  , targetX: c.targetX, targetY: c.targetY, radius: discRadius c.voicing
  }

-- ---------------------------------------------------------------------------
-- Simulation lifecycle — runs over a given chord set, focused on focusId
-- ---------------------------------------------------------------------------

startWith :: forall o m. MonadAff m => Key -> Int -> Array ChordNode -> H.HalogenM State Action Slots o m Unit
startWith key focusId chords0 = do
  st <- H.get
  case find (\c -> c.id == focusId) chords0 of
    Nothing -> pure unit
    Just focus -> do
      -- The assembled progression lives in `path` (Slice 4a: the one source the
      -- performance reads too). A lab rebuild replaces the seed set, so carry the
      -- path's chords across untouched — changing key/scale mustn't wipe it.
      let pathKept = filter (\c -> elem c.id st.path) st.chords
          placed = nubByEq (\a b -> a.id == b.id)
                     (map (place key focus) chords0 <> pathKept)
          simNodes = map mkSimNode (map (place key focus) chords0)
      result <- liftEffect $ runSimulation
        { engine: D3
        , setup: vetulaForceSetup
        , nodes: simNodes
        , links: ([] :: Array { source :: Int, target :: Int })
        , container: "#vetula-surface"
        , alphaMin: 0.005
        }
      emitter <- liftEffect $ toHalogenEmitter result.events
      sid <- H.subscribe $ emitter <#> case _ of
        Tick _ -> SimTick
        Started -> SimTick
        Completed -> SimDone
        Stopped -> SimDone
      H.modify_ \s ->
        s { key = key, chords = placed, focusId = focusId, nodes = simNodes
          , handle = Just result.handle, subId = Just sid }

stopSim :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
stopSim = do
  st <- H.get
  for_ st.subId H.unsubscribe
  for_ st.handle \h -> liftEffect h.stop
  H.modify_ _ { subId = Nothing, handle = Nothing }

-- | Request Web-MIDI access and pick the output. Prefers the Continuo audition
-- | port when it's live (a JUCE virtual dest named "continuo" — Piano One/strings
-- | for hearing Vetula, see the continuo-vst-daemon note), falling back to the IAC
-- | bus that feeds the rig in production. Called on Initialize AND from the chip
-- | click (RetryMidi) — the click is the user gesture Chrome needs to prompt.
connectMidi :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
connectMidi = do
  sel <- H.gets _.auditionSel
  { emitter: midiE, listener: midiL } <- liftEffect HS.create
  _ <- H.subscribe midiE
  liftEffect $ Midi.requestAccess \maccess -> case maccess of
    Nothing -> HS.notify midiL (MidiReady Nothing "no Web-MIDI")
    Just access -> case sel of
      -- Off: no output at all.
      AuditionOff -> HS.notify midiL (MidiReady Nothing "muted")
      -- Midi: the rig/IAC bus only (the rig is the audition, no Continuo fallback).
      AuditionMidi -> do
        miac <- Midi.findOutput access midiPortName
        names <- Midi.outputNames access
        let nm = case miac of
              Just _ -> midiPortName <> " ✓"
              Nothing -> "no '" <> midiPortName <> "' — ports: " <> joinWith ", " names
        HS.notify midiL (MidiReady miac nm)
      -- Continuo: the VST preview port when live, else fall back to IAC.
      AuditionContinuo -> do
        mcont <- Midi.findOutput access "continuo"
        miac <- Midi.findOutput access midiPortName
        names <- Midi.outputNames access
        let mout = case mcont of
              Just _ -> mcont
              Nothing -> miac
            nm = case mcont of
              Just _ -> "continuo ✓"
              Nothing -> case miac of
                Just _ -> midiPortName <> " ✓"
                Nothing -> "no '" <> midiPortName <> "'/continuo — ports: " <> joinWith ", " names
        HS.notify midiL (MidiReady mout nm)

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    -- MIDI out. NB modern Chrome only shows the Web-MIDI permission prompt in
    -- response to a USER GESTURE, so this page-load request often resolves to
    -- "no Web-MIDI" the first time — clicking the MIDI chip (→ RetryMidi) re-runs
    -- it from a real gesture and surfaces the prompt. See connectMidi.
    connectMidi
    -- The shared transport: connect Binnacle (free-run 120 → Link-lock on the
    -- rig) and run the lookahead scheduler. It ticks the 16th-note grid always;
    -- PerfTick gates on `playing`, so Vetula is a clock-peer of Odonus/Balistes
    -- under the shell master — same downbeat, same tempo, background-safe.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (PerfTick tick)
    -- Slice 1: auto-capture the progression you're building to the library on a
    -- slow timer (settle + update-in-place), so nothing is ever silently lost.
    { emitter: capE, listener: capL } <- liftEffect HS.create
    _ <- H.subscribe capE
    _ <- liftEffect $ setInterval 1800 (HS.notify capL AutoCapture)
    H.modify_ _ { binnacle = Just bin }
    -- Restore the persisted library (auto-capture stack) from localStorage. capSeq
    -- continues past the restored count so new ◦ autonames don't collide.
    msaved <- liftEffect Store.loadLibrary
    for_ msaved \sv -> H.modify_ _ { library = sv.library, capSeq = length sv.library, presets = sv.presets }
    -- Resume the persisted Perform session (a reload must NOT start a new session);
    -- mint one only on the very first launch. `perfSession` then rides every save.
    msess <- liftEffect Store.loadSession
    case msess of
      Just sess -> do
        Console.log ("Vetula session: resumed " <> sess.alias <> " (next #" <> show sess.nextScene <> ")")
        H.modify_ _ { perfSession = sess }
      Nothing -> do
        fresh <- liftEffect mintSession
        liftEffect (Store.saveSession fresh)
        Console.log ("Vetula session: minted (first-launch) " <> fresh.alias)
        H.modify_ _ { perfSession = fresh }
    -- Merge the shared Amphora progression library in the BACKGROUND: awaiting it
    -- blocked Initialize (hence the shell's polls of Vetula) until the ~30s offline
    -- timeout. The store being offline is not fatal — keep whatever's local.
    void $ H.fork do
      dbRes <- liftAff (attempt (Amphora.fetchCollection "vetula-progression"))
      case dbRes of
        Right items | length items > 0 ->
          H.modify_ \s -> s { library = mergeLibByName s.library (map amphoraEntry items) }
        _ -> pure unit
    -- keyboard
    { emitter: keyE, listener: keyL } <- liftEffect HS.create
    _ <- H.subscribe keyE
    liftEffect do
      w <- window
      el <- eventListener \ev -> do
        -- while typing in a name / search / source field, the single-key
        -- shortcuts (⌫ = clear, r = reset, space, Tab…) must stand down — they
        -- were wiping the progression mid-type. `c` is the shell's global CAPTURE
        -- hotkey now, not a Vetula key.
        typing <- isFormField ev
        -- and when Vetula is mounted-but-hidden (it's one tab of the Triggerfish
        -- rack), ignore keys entirely so they don't fire phantom chords while
        -- another instrument is on screen. No-op standalone, where it's visible.
        hidden <- surfaceHidden
        when (not typing && not hidden) $ case KE.fromEvent ev of
          Just ke -> do
            let k = KE.key ke
            -- swallow the browser defaults for the keys we drive (scroll / focus)
            when (elem k [ " ", "Tab", "ArrowUp", "ArrowDown" ]) (preventDefault ev)
            -- ignore OS auto-repeat: a held key is one event, so space gives one
            -- sustained chord rather than a machine-gun retrigger
            when (not (KE.repeat ke)) (HS.notify keyL (Key k (KE.shiftKey ke)))
          Nothing -> pure unit
      addEventListener (EventType "keydown") el false (Window.toEventTarget w)
    -- initial palette
    st <- H.get
    startWith st.key seedFocus (seedsFor st.key)

  MidiReady mout nm ->
    H.modify_ _ { midiOut = mout, midiName = nm }

  -- Click the MIDI chip to (re)request access — this runs from a user gesture,
  -- which is what makes Chrome actually show the permission prompt.
  RetryMidi -> do
    H.modify_ _ { midiName = "…" }
    connectMidi

  SimTick -> do
    st <- H.get
    case st.handle of
      Just h -> do
        ns <- liftEffect h.getNodes
        H.modify_ _ { nodes = ns }
      Nothing -> pure unit

  SimDone -> pure unit

  Hover mid -> H.modify_ _ { hoveredId = mid }

  HoverTriad mt -> H.modify_ _ { hoveredTriad = mt }

  -- entering a tank tile sets the hovered specimen (space previews it); leaving
  -- clears it. Also clears any surface hover so space can't fall back to a stale
  -- pool bubble while the pointer is over the tank.
  HoverSpec ms -> H.modify_ _ { hoveredSpec = ms, hoveredId = Nothing, hoveredTriad = Nothing }

  -- Slice 4c: hand-override the width-focus (the Hunt/Perform toggle, the pool spine).
  -- No surface rebuild — the lattice sim keeps running; we only change which column
  -- gets the width.
  SetFocus f -> H.modify_ _ { focus = f }

  -- Toggle a rail accordion section (independent — several may be open at once).
  ToggleRailSection sec -> H.modify_ \s ->
    s { railOpen = if Set.member sec s.railOpen then Set.delete sec s.railOpen else Set.insert sec s.railOpen }

  ToggleLeftSection sec -> H.modify_ \s ->
    s { leftOpen = if Set.member sec s.leftOpen then Set.delete sec s.leftOpen else Set.insert sec s.leftOpen }

  Key k shift -> do
    st <- H.get
    -- while the revoice modal is open the surface keys (stacking / explode / reset)
    -- stand down; only the within-chord controls stay live.
    if isJust st.revoicing
      then case k of
        "Escape" -> H.modify_ _ { revoicing = Nothing }
        "v" -> H.modify_ _ { revoicing = Nothing }
        "Tab" -> cycleVoicing (if shift then -1 else 1)
        "ArrowUp" -> nudgeSelected 1
        "ArrowDown" -> nudgeSelected (-1)
        " " -> playHoveredOrSounding
        "f" -> toggleFavorite
        _ -> pure unit
      else case k of
        "r" -> resetPalette
        " " -> playHoveredOrSounding
        "Tab" -> cycleVoicing (if shift then -1 else 1)
        "ArrowUp" -> nudgeSelected 1
        "ArrowDown" -> nudgeSelected (-1)
        -- Clear the path is Backspace/Delete (the ✕ button also does it). `c` used to
        -- clear here, but it's now the shell's global CAPTURE hotkey (same key on every
        -- pane, docs/DESIGN-scene-modal.md) — so clear yields it the letter.
        "Backspace" -> handleAction ClearPath
        "Delete" -> handleAction ClearPath
        -- Enter: compress the current chyron selection into a saved glyph token
        "Enter" -> handleAction SaveChyronSel
        "p" -> H.gets _.path >>= playPath
        "f" -> toggleFavorite
        -- catch the hovered chord into the tank: a Tonnetz triangle first (no pool
        -- id), else the hovered pool chord, else the sounding one
        "k" -> case st.hoveredTriad of
                 Just t -> handleAction (CatchTriad t.root t.pcs (elem (mod (t.root + 4) 12) t.pcs))
                 Nothing -> for_ (case st.hoveredId of
                                    Just h -> Just h
                                    Nothing -> st.sounding) (handleAction <<< CatchChord)
        -- open the revoice modal on the hovered (else sounding) chord
        "v" -> handleAction OpenRevoice
        -- explode / collapse the focused root's full lattice (the firehose)
        "l" -> for_ st.focusedFamily explode
        -- number-stacking: add the scale tone (n−1) generic steps above the head note
        "2" -> stackOn 2
        "3" -> stackOn 3
        "4" -> stackOn 4
        "5" -> stackOn 5
        "6" -> stackOn 6
        "7" -> stackOn 7
        -- friendly populators: e = next third, s = the suspension set
        "e" -> stackThird
        "s" -> suspend
        _ -> pure unit

  SelectKey v -> case fromString v of
    Just pc -> do
      st <- H.get
      -- picking a key retakes harmonic authority from any `# scale` override
      H.modify_ _ { restScale = Nothing }
      rebuild (st.key { tonic = mod pc 12 })
    Nothing -> pure unit

  SelectScale v -> do
    st <- H.get
    H.modify_ _ { restScale = Nothing }
    rebuild (st.key { mode = modeOf v })

  -- shift-click a Lattice chord to grow the running path. From the current end:
  -- same family → bridge by the shortest single-note walk; different family →
  -- leap (an interconnector). Clicking the current end again clears the path.
  PathPick pid -> do
    st <- H.get
    case last st.path of
      -- Starting a path from empty (after ANY clear: c / double-click / key rebuild)
      -- opens a NEW capture session, so the next auto-capture forks a fresh ◦
      -- instead of overwriting the previous progression's entry in place.
      Nothing -> H.modify_ _ { path = [ pid ], lastCapIdx = Nothing, lastCapSig = "" }
      Just lastId
        | pid == lastId -> H.modify_ _ { path = [] }
        | otherwise -> case Path.shortestPath (Path.adjacency (neighborLinks st.chords)) lastId pid of
            Just bridge -> do
              let added = drop 1 bridge
              H.modify_ _ { path = st.path <> added }
              playPath ([ lastId ] <> added)
            Nothing -> do
              H.modify_ _ { path = st.path <> [ pid ] }
              playPath [ lastId, pid ]

  -- click a keyboard key to summon a family on that root — diatonic OR not — and
  -- focus it for re-flavouring. It defaults to the home mode on the clicked tonic
  -- (a chromatic key brings in a borrowed family). A root that already has a
  -- family is just focused, so clicking its key selects it for the scale picker.
  SummonRoot pc -> do
    st <- H.get
    case find (\c -> c.parentId == Nothing && c.root == pc) st.chords of
      Just existing ->
        let exKey = fromMaybe st.key (Map.lookup existing.id st.familyScale)
        in H.modify_ _ { focusedFamily = Just existing.id, stackHead = Just { id: existing.id, top: seedTopDeg exKey existing } }
      Nothing -> do
        let famKey = st.key { tonic = pc }
        for_ (head (diatonicTriads famKey)) \t0 -> do
          let seed = place famKey t0 (t0 { id = st.nextId, parentId = Nothing })
              chords' = st.chords <> [ seed ]
          H.modify_ _
            { chords = chords'
            , nextId = st.nextId + 1
            , familyScale = Map.insert st.nextId famKey st.familyScale
            , focusedFamily = Just st.nextId
            , stackHead = Just { id: st.nextId, top: seedTopDeg famKey seed }
            }
          for_ st.handle \h ->
            liftEffect $ void $ h.updateData (map mkSimNode chords') (neighborLinks chords')

  -- open the revoice modal on the hovered chord (else the sounding one), making
  -- it the active chord so Tab / arrows / drag / f all target it inside the modal.
  OpenRevoice -> do
    st <- H.get
    let target = case st.hoveredId of
          Just hid | any (\c -> c.id == hid) st.chords -> Just hid
          _ -> st.sounding
    for_ target \cid -> H.modify_ _ { revoicing = Just cid, sounding = Just cid }

  CloseRevoice -> H.modify_ _ { revoicing = Nothing }

  -- a slash chord: set the revoiced chord's bass to a chosen pitch class (same
  -- upper notes, different foundation) — a voicing decision, kept in the modal.
  SlashBass pc -> do
    st <- H.get
    for_ st.revoicing \cid -> do
      let chords' = map (\c -> if c.id == cid then c { bassPc = pc } else c) st.chords
      applyChords chords'
      for_ (find (\c -> c.id == cid) chords') playChord

  -- toggle an exterior signpost set (McMullen Yellow / Borrowed …). On: generate
  -- the curated chords against the home key, drop the ones not already present
  -- (the in-scale members merge into the interior), place each over its own root
  -- floating up by ring index. Off: remove exactly the ids this set added.
  DropSet gkey -> do
    st <- H.get
    for_ st.handle \handle -> case Map.lookup gkey st.dropped of
      Just ids -> do
        let chords' = filter (\c -> not (elem c.id ids)) st.chords
        _ <- liftEffect $ handle.updateData (map mkSimNode chords') (neighborLinks chords')
        H.modify_ _ { chords = chords', dropped = Map.delete gkey st.dropped }
      Nothing ->
        for_ (find (\g -> g.key == gkey) exteriorGens) \g -> do
          let existing = map contentKey st.chords
              fresh = nubByEq (\a b -> contentKey a == contentKey b)
                        (filter (\c -> not (elem (contentKey c) existing)) (g.gen st.key))
              placed = mapWithIndex
                         (\i c -> placeOutside st.key (c { id = st.nextId + i, parentId = Nothing, pinned = false }))
                         fresh
              chords' = st.chords <> placed
          _ <- liftEffect $ handle.updateData (map mkSimNode chords') (neighborLinks chords')
          H.modify_ _
            { chords = chords'
            , nextId = st.nextId + length placed
            , dropped = Map.insert gkey (map _.id placed) st.dropped
            }

  -- toggle a color-overlay layer (2026-07-31 redesign): pure state — the layer
  -- is painted or not painted onto the views (Step 3), the pool is untouched.
  ToggleLayer l ->
    H.modify_ \s ->
      s { colorLayers =
            if Set.member l s.colorLayers then Set.delete l s.colorLayers
            else Set.insert l s.colorLayers }

  -- modal interchange: borrow the chosen parallel mode's chromatic chords (or
  -- "off" to clear). Always replaces the previous interchange set, so the picker
  -- swaps source modes cleanly. The chords live in `dropped` under "interchange".
  BorrowFrom v -> do
    st <- H.get
    for_ st.handle \handle -> do
      let ids = fromMaybe [] (Map.lookup "interchange" st.dropped)
          cleared = filter (\c -> not (elem c.id ids)) st.chords
          famBase = foldr Map.delete st.familyScale ids   -- drop the old set's scales
      if v == "off"
        then do
          _ <- liftEffect $ handle.updateData (map mkSimNode cleared) (neighborLinks cleared)
          H.modify_ _ { chords = cleared, dropped = Map.delete "interchange" st.dropped, familyScale = famBase, borrowMode = Nothing }
        else do
          let srcKey = st.key { mode = modeOf v }
              existing = map contentKey cleared
              fresh = nubByEq (\a b -> contentKey a == contentKey b)
                        (filter (\c -> not (elem (contentKey c) existing)) (interchangeChords (modeOf v) st.key))
              placed = mapWithIndex
                         (\i c -> placeOutside st.key (c { id = st.nextId + i, parentId = Nothing, pinned = false }))
                         fresh
              chords' = cleared <> placed
              -- tag each borrowed chord with its SOURCE scale, so focusing it (click
              -- its key) and stacking extends it in its own modal world, not the home key
              famScale' = Map.union (Map.fromFoldable (map (\c -> Tuple c.id srcKey) placed)) famBase
          _ <- liftEffect $ handle.updateData (map mkSimNode chords') (neighborLinks chords')
          H.modify_ _
            { chords = chords'
            , nextId = st.nextId + length placed
            , dropped = Map.insert "interchange" (map _.id placed) st.dropped
            , familyScale = famScale'
            , borrowMode = Just v
            }

  -- re-flavour the focused family: change its scale (keeping its root), and if
  -- it's currently exploded, re-bloom its lattice from the new scale (collapse +
  -- re-explode reads the updated familyScale). Other families are untouched —
  -- this is what lets, say, a C-major and an F#-Phrygian family coexist.
  ReflavourFamily v -> do
    st <- H.get
    for_ st.focusedFamily \seedId ->
      for_ (find (\c -> c.id == seedId) st.chords) \seed -> do
        let famKey = st.key { tonic = seed.root, mode = modeOf v }
        H.modify_ _ { familyScale = Map.insert seedId famKey st.familyScale }
        when (any (\d -> d.parentId == Just seedId) st.chords) do
          explode seedId   -- collapse
          explode seedId   -- re-bloom in the new scale

  -- a ladder dot grabbed: the chord it belongs to becomes the active (sounding)
  -- one, so the arrow / Tab / f revoicing all follow the widget you touched —
  -- which is what lets the replicated Progression ladders each be independent.
  DragStart alt horiz cid vix sm ->
    H.modify_ _ { drag = Just { chordId: cid, voiceIx: vix, startMidi: sm, offset: 0, horizontal: horiz, double: alt }
                , sounding = Just cid
                , selected = Just (UpperVoice vix) }

  SelectVoice cid sel ->
    H.modify_ _ { sounding = Just cid, selected = Just sel }

  ToggleFavorite -> toggleFavorite

  PickVoicing cid v -> do
    st <- H.get
    let chords' = map (\d -> if d.id == cid then d { voicing = v } else d) st.chords
    applyChords chords'
    H.modify_ _ { sounding = Just cid }
    for_ (find (\d -> d.id == cid) chords') playChord

  PlayPath -> H.gets _.path >>= playPath

  -- ✕ clear: empty the progression and drop back to Hunt, so the next shift-click
  -- STARTS a fresh path instead of extending this one (the Nothing branch of
  -- PathPick then opens a new capture session). The visible twin of the `c` key —
  -- discoverable, and it works with a text field focused (where `c` is swallowed).
  ClearPath -> H.modify_ _ { path = [], focus = Hunt }

  PlayStep pid -> playId pid

  CopyTidal src -> liftEffect (copyText src)

  EditSource s -> H.modify_ _ { sourceEdit = Just s }

  RevertSource -> H.modify_ _ { sourceEdit = Nothing }

  ToggleSource -> H.modify_ \s -> s { sourceOpen = not s.sourceOpen }

  ToggleHelp -> H.modify_ \s -> s { helpOpen = not s.helpOpen }

  -- a progression row: plain click plays that step (out of sequence); shift-click
  -- arms pick mode by selecting it (max two — a 3rd starts fresh).
  StepClick i shift -> do
    st <- H.get
    if not shift
      then for_ (index st.path i) playId
      else do
        let sel' = if elem i st.genSel then filter (_ /= i) st.genSel
                   else if length st.genSel < 2 then st.genSel <> [ i ]
                   else [ i ]
        H.modify_ _ { genSel = sel' }
        regen

  CancelGen -> H.modify_ _ { genSel = [], candidates = [] }

  SetAdventure v -> case Number.fromString v of
    Just x -> do
      H.modify_ _ { adventure = x }
      regen
    Nothing -> pure unit

  -- commit a candidate into the progression at the right place (insert before /
  -- after / between, or replace for a substitute), then leave pick mode.
  PickCandidate cid -> do
    st <- H.get
    for_ (find (\c -> c.id == cid) st.candidates) \cand -> do
      let sel = sort (nub st.genSel)
          n = length st.path
          newId = st.nextId
          newC = cand { id = newId }
          path' = case genModeOf sel n, sel of
            Just Substitute, [ i ] -> fromMaybe st.path (updateAt i newId st.path)
            Just Transition, [ i, _ ] -> fromMaybe (st.path <> [ newId ]) (insertAt (i + 1) newId st.path)
            Just Prepend, _ -> [ newId ] <> st.path
            Just Append, _ -> st.path <> [ newId ]
            _, _ -> st.path
      H.modify_ _
        { chords = st.chords <> [ newC ]
        , imported = Set.insert newId st.imported
        , nextId = newId + 1
        , path = path'
        , genSel = []
        , candidates = []
        , sounding = Just newId
        }
      playChord newC

  -- ----- Performance tab -----

  SetSaveName s -> H.modify_ _ { saveName = s }

  -- Manual save = promote the current path to a KEEPER (frozen). If the current
  -- session's ephemeral is already in the library, promote it in place (+ rename);
  -- otherwise append a fresh keeper.
  SaveProg -> do
    st <- H.get
    let steps = pathSteps st
    when (length steps > 0) do
      let nm = if st.saveName == "" then groupLabel st.key <> " · " <> show (length steps) else st.saveName
          kl = groupLabel st.key
          src = currentSource st
      case st.lastCapIdx of
        Just i | isJust (index st.library i) ->
          H.modify_ _ { library = fromMaybe st.library (modifyAt i (_ { name = nm, keyLabel = kl, source = src, kept = true }) st.library)
                      , saveName = "", lastCapIdx = Nothing }
        _ ->
          H.modify_ _ { library = st.library <> [ { name: nm, keyLabel: kl, source: src, kept: true } ], saveName = "" }
      persistLib

  -- Timer auto-capture (Slice 1). Empty path → close the current session (next
  -- capture starts fresh). Non-empty + changed → UPDATE the session's ephemeral in
  -- place (or open a new one), so a building session is one live-updated entry.
  -- Keepers are never touched. The entry's canonical form is its Tidal source.
  AutoCapture -> do
    st <- H.get
    let steps = pathSteps st
        sig = currentSource st
    if length steps == 0
      then when (isJust st.lastCapIdx) (H.modify_ _ { lastCapIdx = Nothing, lastCapSig = "" })
      else when (sig /= st.lastCapSig) (captureSteps st)

  -- Promote an ephemeral to a keeper (or demote a keeper). Promoting the current
  -- session's ephemeral forks a fresh one for continued edits (lastCapIdx cleared).
  KeepLib i -> do
    H.modify_ \s ->
      let lib' = fromMaybe s.library (modifyAt i (\e -> e { kept = not e.kept }) s.library)
      in s { library = lib', lastCapIdx = if Just i == s.lastCapIdx then Nothing else s.lastCapIdx }
    persistLib

  SetLibSearch s -> H.modify_ _ { libSearch = s }

  -- Parse the entry's Tidal source into note-lists and rebuild them as fresh
  -- imported chords (off the lattice); revoice stays safe, and this is the same
  -- render/parse the source panel uses — so persisted entries reload identically.
  LoadProg i -> do
    st <- H.get
    for_ (index st.library i) \entry -> do
      let noteLists = filter (\ns -> length ns > 0) (parseProgression entry.source)
          fresh = mapWithIndex (\j ns -> importChord (st.nextId + j) ns) noteLists
          ids = map _.id fresh
      H.modify_ _
        { chords = st.chords <> fresh
        , imported = st.imported <> Set.fromFoldable ids
        , nextId = st.nextId + length fresh
        -- Slice 4a: restore the snapshot INTO the path (the one performed progression),
        -- not a parallel working copy.
        , path = ids
        , perfName = Just entry.name
        , voices = canonicalVoices (length fresh)
        , nextVoiceId = 4
        , sounding = head ids
        -- The loaded progression's key becomes the live harmonic context: clear any
        -- `# scale` override, then adopt the entry's saved key so the resting scale
        -- (and every following voice, incl. Odonus) tracks it. Without this the scale
        -- stayed on whatever was loaded before — the "dark pads stayed C minor" bug.
        , restScale = Nothing
        }
      for_ (parseKeyLabel entry.keyLabel) \k -> H.modify_ _ { key = k }

  -- ← library: set the current progression aside to browse the stack. Slice 4a: the
  -- path IS the progression, so snapshot it first (AutoCapture is on a timer and may
  -- not have fired yet), then clear — non-destructive, and it's the design's
  -- "library = snapshots you restore into the path".
  UnloadProg -> do
    st <- H.get
    when (length (pathSteps st) > 0) (captureSteps st)
    stopClock
    H.modify_ _ { path = [], perfName = Nothing, focus = Hunt, voices = [], playing = false }
    -- Unloading self-disarms; tell the shell so it drops Vetula from `armed`.
    when st.armed (H.raise (ArmChanged false))

  -- Delete shifts indices, so drop the session pointer to avoid it dangling.
  DeleteLib i -> do
    H.modify_ \s -> s { library = fromMaybe s.library (deleteAt i s.library), lastCapIdx = Nothing }
    persistLib

  -- Publish library entry #i to the shared Amphora store (vetula-progression).
  -- The entry's `source` is its canonical Tidal form; name + keyLabel + kept ride
  -- the label (keyLabel as a `key:` tag, kept as a `kept` tag). Store offline → a
  -- transient failure message, never fatal.
  PublishLib i -> do
    st <- H.get
    case st.library !! i of
      Nothing -> pure unit
      Just e -> do
        H.modify_ _ { publishMsg = Just "publishing…" }
        let tags = [ "key:" <> e.keyLabel ] <> (if e.kept then [ "kept" ] else [])
        res <- liftAff (attempt (Amphora.publish
          { kind: "vetula-progression", collection: "vetula-progression"
          , name: e.name, source: "user", payload: e.source, tags }))
        H.modify_ _ { publishMsg = Just case res of
          Right hash -> "✓ " <> e.name <> " · " <> SCU.take 8 hash
          Left _ -> "✗ publish failed (store offline?)" }

  -- Serialise the whole Perform surface as a `vetulaScene` record and publish it
  -- to the shared Amphora store (collection `vetula-scene`). The scene is auto-
  -- named `⟨session alias|name⟩ #N` (no naming friction — identity without a name,
  -- per the glyph substrate) and tagged `session:`/`scene:` so recall groups by
  -- session; `nextScene` increments (and persists) only on a successful save, so a
  -- failed store doesn't burn a number. Payload is the Tier-3 `printAsRecord` form;
  -- sources dedup by content. Store offline → a transient message, never fatal.
  SaveScene -> do
    st <- H.get
    H.modify_ _ { publishMsg = Just "saving scene…" }
    let keyLabel = groupLabel st.key
        sess = st.perfSession
        n = sess.nextScene
        handle = if sess.name == "" then sess.alias else sess.name
        label = handle <> " #" <> show n
        doc = docFromVoices keyLabel (map boxSpec st.perfBoxes)
        payload = printAsRecord label doc
        tags = [ "session:" <> sess.alias, "scene:" <> show n, "key:" <> keyLabel ]
    res <- liftAff (attempt (Amphora.publish
      { kind: "vetula-scene", collection: "vetula-scene"
      , name: label, source: "user", payload, tags }))
    case res of
      Right hash -> do
        let sess' = sess { nextScene = n + 1 }
        liftEffect (Store.saveSession sess')
        H.modify_ _ { perfSession = sess', publishMsg = Just ("✓ " <> label <> " · " <> SCU.take 8 hash) }
      Left _ -> H.modify_ _ { publishMsg = Just "✗ save failed (store offline?)" }

  -- Mint a fresh session (a new monochrome glyph-triple, scene counter back to 1)
  -- and persist it. The deliberate "I'm starting a new body of work" boundary — the
  -- only thing besides a first-ever launch that rolls the session (reloads resume).
  PerfNewSession -> do
    fresh <- liftEffect mintSession
    liftEffect (Store.saveSession fresh)
    Console.log ("Vetula session: new-session button → " <> fresh.alias)
    H.modify_ _ { perfSession = fresh, publishMsg = Just ("new session · " <> fresh.alias) }

  -- Open the recall modal, fetching the saved scenes from Amphora (grouped by
  -- session in the view). Store offline → an empty list + a note, never fatal.
  PerfOpenRecall -> do
    H.modify_ _ { perfRecallOpen = true, publishMsg = Just "loading scenes…" }
    res <- liftAff (attempt (Amphora.fetchCollection "vetula-scene"))
    case res of
      Right items -> H.modify_ _ { perfScenes = items, publishMsg = Nothing }
      Left _ -> H.modify_ _ { perfScenes = [], publishMsg = Just "✗ scenes: store offline?" }

  PerfCloseRecall -> H.modify_ _ { perfRecallOpen = false }

  -- Parse a stored scene payload (the `vetulaScene { … }` record) back into a
  -- document and reconstruct the surface's boxes. Lenient: a payload that yields
  -- no voices is left as a note rather than blanking the surface.
  PerfLoadScene payload -> do
    let boxes = boxesFromDoc (parsePerform payload)
    if length boxes == 0
      then H.modify_ _ { perfRecallOpen = false, publishMsg = Just "✗ couldn't read that scene" }
      else H.modify_ _ { perfBoxes = boxes, perfRecallOpen = false, publishMsg = Just "scene loaded" }

  AddVoice -> H.modify_ \s ->
    s { voices = s.voices <> [ defaultVoice s.nextVoiceId (mod s.nextVoiceId 4) (rendOf s.nextVoiceId) (length (perfChords s)) ]
      , nextVoiceId = s.nextVoiceId + 1 }

  RemoveVoice vid -> H.modify_ \s -> s { voices = filter (\v -> v.id /= vid) s.voices }

  SetVoiceChannel vid v -> case fromString v of
    Just ch -> updateVoice vid (_ { channel = clamp 0 15 ch })
    Nothing -> pure unit

  -- Name a → midi voice: its routing identity, which the Tidal page binds to a channel.
  SetVoiceName vid nm -> updateVoice vid (_ { name = nm })

  -- The AUDITION channel (chord/path preview). Its own routable channel so, in
  -- ATLANTIS, the preview can be cued/muted at the desk independently of the brush.
  SetPreviewChan v -> case fromString v of
    Just ch -> H.modify_ _ { previewChan = clamp 0 15 ch }
    Nothing -> pure unit

  CycleVoiceDest vid -> updateVoice vid (\v -> v { dest = cycleDest v.dest })

  CycleVoiceRenderer vid -> updateVoice vid (\v -> v { renderer = nextRenderer v.renderer })

  -- click a grid cell to set how many bars this voice dwells on this chord:
  -- plain click bumps up (wrapping 0→1→…→8→0), shift-click bumps down. 0 = skip.
  BumpCell vid i shift -> do
    st <- H.get
    let n = length (perfChords st)
    updateVoice vid \v ->
      let ds = padDurs n v.durs
      in v { durs = fromMaybe ds (modifyAt i (\d -> mod (d + (if shift then 8 else 1)) 9) ds) }

  SetVoicePhase vid v -> case fromString v of
    Just n -> updateVoice vid (_ { phase = max 0 n })
    Nothing -> pure unit

  -- live-code the read-head (which chord, when) and the note pattern (which note of it)…
  SetVoicePattern vid p -> updateVoice vid (_ { patternDraft = p })
  SetVoiceNotePattern vid p -> updateVoice vid (_ { notePatternDraft = p })
  CycleVoiceArticulator vid -> updateVoice vid (\v -> v { articulator = RA.nextArtic v.articulator })

  -- …then commit BOTH atomically. Committing resets the live-jump phase (the new
  -- patterns re-anchor from bar 0) so an edit is a clean structural change.
  CommitVoicePattern vid -> updateVoice vid \v ->
    v { pattern = trim v.patternDraft, notePattern = trim v.notePatternDraft, phase = 0 }

  ToggleVoiceMute vid -> updateVoice vid (\v -> v { muted = not v.muted })

  -- The bpm field nudges the shared clock's free-run baseline (so it works
  -- standalone). On the rig the Link anchor — and in the rack the shell's
  -- SyncFree — re-asserts the shared tempo, since Vetula is now a clock-peer.
  SetTempo v -> case fromString v of
    Just t -> do
      let t' = clamp 40 240 t
      st <- H.get
      now <- liftEffect dateNow
      for_ st.binnacle \bin ->
        liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros: now * 1000.0, tempo: toNumber t' })
      H.modify_ _ { tempo = t', clockTempo = toNumber t' }
    Nothing -> pure unit

  SelectPerfChord pid -> H.modify_ _ { sounding = Just pid, selected = Nothing }

  -- Tank model (Slice A). Catch a lattice chord into the durable tank as a frozen
  -- Specimen: absolute-MIDI voicing + bass (bassPc grounded an octave below middle
  -- C, matching playNotes), a descriptive label + provenance. It references no
  -- lattice id, so the cloud can regenerate underneath without disturbing it.
  PlayChordId pid -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    else playId pid

  CatchChord pid -> do
    st <- H.get
    for_ (find (\c -> c.id == pid) st.chords) \c -> do
      let spec = { id: SpecimenId st.nextSpecId
                 , voicing: c.voicing
                 , bass: c.bassPc + 36
                 , label: c.label
                 , provenance: FromLens (groupLabel st.key)
                 , anchor: c.anchor   -- freeze the caught chord's harmonic reading
                 }
      H.modify_ _ { tank = st.tank <> [ spec ], nextSpecId = st.nextSpecId + 1 }

  DeleteSpec sid -> H.modify_ \s -> s { tank = filter (\sp -> sp.id /= sid) s.tank }

  AuditionSpec sid -> do
    st <- H.get
    for_ (find (\sp -> sp.id == sid) st.tank) playSpecimen

  -- Seed the Stage from the tank. First click injects the specimen into the pool
  -- as a centre chord and blooms its extensions around it (seed → generate);
  -- clicking a staged specimen again unstages it, pruning the seed + its bloom.
  StageSpec sid -> do
    st <- H.get
    case Map.lookup sid st.seedChord of
      Just cid -> do
        let removeIds = pruneSet st.chords [ cid ]
            surviving = filter (\c -> not (elem c.id removeIds)) st.chords
        H.modify_ _ { chords = surviving, seedChord = Map.delete sid st.seedChord }
        for_ st.handle \h ->
          liftEffect $ void $ h.updateData (map mkSimNode surviving) (neighborLinks surviving)
      Nothing ->
        for_ (find (\sp -> sp.id == sid) st.tank) \spec -> do
          let seedId = st.nextId
              node = specToNode seedId st.key spec
              chords' = st.chords <> [ node ]
          H.modify_ _
            { chords = chords'
            , nextId = seedId + 1
            , seedChord = Map.insert sid seedId st.seedChord
            , focusId = seedId
            , focusedFamily = Just seedId
            , sounding = Just seedId
            }
          spawn Extend seedId    -- a modest neighbourhood; richer lenses come in Slice C+
          playSpecimen spec

  ClearStage -> do
    st <- H.get
    let seedCids = map snd (Map.toUnfoldable st.seedChord :: Array (Tuple SpecimenId Int))
        removeIds = pruneSet st.chords seedCids
        surviving = filter (\c -> not (elem c.id removeIds)) st.chords
    H.modify_ _ { chords = surviving, seedChord = Map.empty }
    for_ st.handle \h ->
      liftEffect $ void $ h.updateData (map mkSimNode surviving) (neighborLinks surviving)

  -- Tonnetz lens: a triad picked straight off the tonal net. Audition sounds it
  -- (no state change); catch freezes it into the tank as a Free-anchored Specimen,
  -- exactly like a palette drop.
  AuditionTriad root pcs -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    -- label it like CatchTriad (root name + minor mark) so the chyron reads it
    else playChord (triadNode root pcs (noteName root <> (if elem (mod (root + 4) 12) pcs then "" else "m")))

  CatchTriad root pcs isMajor -> do
    st <- H.get
    let node = triadNode root pcs (noteName root <> (if isMajor then "" else "m"))
        spec = { id: SpecimenId st.nextSpecId
               , voicing: node.voicing
               , bass: node.bassPc + 36
               , label: node.label
               , provenance: FromLens (groupLabel st.key)
               , anchor: node.anchor
               }
    H.modify_ _ { tank = st.tank <> [ spec ], nextSpecId = st.nextSpecId + 1 }

  -- Tonnetz stacking (2026-07-31): alt-click accumulates triads into a stack
  -- (toggle — alt-clicking a stacked triad removes it). The stack builds a
  -- polychord; edge-adjacent triads fold into 7ths/9ths since the chord is the
  -- pitch-class union. Auditions the triad on the way in so you hear it stack.
  StackTriad root pcs isMajor -> do
    playChord (triadNode root pcs "")
    H.modify_ \s ->
      let same e = e.root == root && e.pcs == pcs
      in s { tonnetzStack = case find same s.tonnetzStack of
               Just _ -> filter (not <<< same) s.tonnetzStack
               Nothing -> s.tonnetzStack <> [ { root, pcs, major: isMajor } ] }

  -- Catch the whole stack to the tank as ONE Anchor: the pitch-class union,
  -- bassed on the first (lowest-picked) triad, labelled as the stacked triads.
  CommitStack -> do
    st <- H.get
    case st.tonnetzStack of
      [] -> pure unit
      stack -> do
        let allPcs = nub (concatMap _.pcs stack)
            root = maybe 0 _.root (head stack)
            label = joinWith "+" (map (\e -> noteName e.root <> (if e.major then "" else "m")) stack)
            node = triadNode root allPcs label
            spec = { id: SpecimenId st.nextSpecId
                   , voicing: node.voicing
                   , bass: node.bassPc + 36
                   , label: node.label
                   , provenance: FromLens (groupLabel st.key)
                   , anchor: node.anchor
                   }
        H.modify_ _ { tank = st.tank <> [ spec ], nextSpecId = st.nextSpecId + 1, tonnetzStack = [] }

  ClearStack -> H.modify_ _ { tonnetzStack = [] }

  -- Lattices lens: a generated lattice chord carries its own voicing, so audition/
  -- catch use it verbatim (unlike the triad path, which re-voices from pcs).
  AuditionNode c -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    else playChord c

  CatchNode c -> do
    st <- H.get
    let spec = { id: SpecimenId st.nextSpecId
               , voicing: c.voicing
               , bass: c.bassPc + 36
               , label: c.label
               , provenance: FromLens (groupLabel st.key)
               , anchor: c.anchor
               }
    H.modify_ _ { tank = st.tank <> [ spec ], nextSpecId = st.nextSpecId + 1 }

  -- Chyron: remember which chip the pointer is over so space auditions it.
  HoverChyron mi -> H.modify_ _ { hoveredChyron = mi }

  -- Chyron click. Plain click walks the selection state machine (endpoint →
  -- span → reset). Shift-click LIFTS: for now, catch the chord into the tank
  -- (Phase 2b will special-case a shift-click INSIDE the span to lift the whole
  -- selection as a named progression).
  -- Chyron selection, Mac text-editing semantics. Plain click drops a fresh
  -- single-chord selection whose `anchor` is that chord; shift-click extends the
  -- range from the fixed anchor (a shift-click with no prior selection is just a
  -- plain click). This is the collector-and-editor's select gesture — it no longer
  -- catches to the tank (the tank is being retired; you explore from the selection
  -- itself, DESIGN-tank-overhaul.md §§3–4).
  ChyronClick i shift -> do
    st <- H.get
    case index st.chyron i of
      Nothing -> pure unit
      Just _ ->
        let sel = case st.chyronSel of
              Just s | shift -> { lo: min s.anchor i, hi: max s.anchor i, anchor: s.anchor }
              _ -> { lo: i, hi: i, anchor: i }
        in H.modify_ _ { chyronSel = Just sel }

  -- Delete one audition (indices shift, so drop any selection/hover to stay safe).
  DeleteChyron i -> H.modify_ \st ->
    st { chyron = fromMaybe st.chyron (deleteAt i st.chyron)
       , chyronSel = Nothing
       , hoveredChyron = Nothing }

  ClearChyron -> H.modify_ _ { chyron = [], chyronSel = Nothing, hoveredChyron = Nothing }

  -- Compress the selected span into a pinned 2-glyph token: mint a SavedSeq from
  -- its events + content-glyph, then REMOVE those events from the live trace
  -- (reclaiming the space — the saving is the compression).
  SaveChyronSel -> do
    st <- H.get
    case st.chyronSel of
      Just sel | sel.hi > sel.lo -> do
        let evs = mapMaybe (\ix -> index st.chyron ix) (range sel.lo sel.hi)
            saved = { events: evs, glyph: glyphOf (seqContent evs) }
            keep = mapMaybe (\(Tuple ix e) -> if ix < sel.lo || ix > sel.hi then Just e else Nothing)
                     (mapWithIndex Tuple st.chyron)
        H.modify_ _ { chyronSaved = st.chyronSaved <> [ saved ], chyron = keep
                    , chyronSel = Nothing, hoveredChyron = Nothing }
      _ -> pure unit

  PlaySaved i -> do
    st <- H.get
    for_ (index st.chyronSaved i) \s -> playEvents s.events

  DeleteSaved i -> H.modify_ \st -> st { chyronSaved = fromMaybe st.chyronSaved (deleteAt i st.chyronSaved) }

  ToggleChyronArm -> H.modify_ \st -> st { chyronArmed = not st.chyronArmed }

  -- PERFORM: pick up / drop / clear a player box. Pickup toggles (click the held
  -- token again to drop it). Drop assigns the held token and clears the hand;
  -- with nothing in hand it is a no-op (so a bubbled × clear is harmless).
  PerfPickup i -> H.modify_ \st ->
    st { perfHeld = if st.perfHeld == Just i then Nothing else Just i, perfHeldFx = Nothing }

  -- Drop onto box b (appends): a DRAGGED layer (palette or moved from another box)
  -- wins; else an fx-in-hand (click-place); else a token-in-hand assigns the
  -- sequence; else no-op (so a bubbled × clear / chip-drop stays harmless).
  PerfDropBox b -> do
    st <- H.get
    case st.perfDrag of
      Just src -> H.modify_ _ { perfBoxes = dropFxInto src b Nothing st.perfBoxes, perfDrag = Nothing }
      Nothing -> case st.perfHeldFx of
        Just fx -> H.modify_ _
          { perfBoxes = mapWithIndex (\j box -> if j == b then box { stack = box.stack <> [ mkLayer fx ] } else box) st.perfBoxes
          , perfHeldFx = Nothing
          }
        Nothing -> case st.perfHeld >>= index st.chyronSaved of
          Just s -> H.modify_ _
            { perfBoxes = mapWithIndex (\j box -> if j == b then box { seq = Just s } else box) st.perfBoxes
            , perfHeld = Nothing
            }
          Nothing -> pure unit

  PerfClearBox b -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box -> if j == b then box { seq = Nothing } else box) st.perfBoxes }

  PerfDragOver ev -> liftEffect (preventDefault (DE.toEvent ev))

  PerfPickFx fx -> H.modify_ \st ->
    st { perfHeldFx = if st.perfHeldFx == Just fx then Nothing else Just fx, perfHeld = Nothing }

  PerfFxNudge b i d -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { stack = fromMaybe box.stack (modifyAt i (\lyr -> lyr { fx = fxNudge d lyr.fx }) box.stack) } else box)
         st.perfBoxes }

  PerfFxWhen b i -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { stack = fromMaybe box.stack (modifyAt i (\lyr -> lyr { when = cycleWhen lyr.when }) box.stack) } else box)
         st.perfBoxes }

  PerfFxRemove b i -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { stack = fromMaybe box.stack (deleteAt i box.stack) } else box)
         st.perfBoxes }

  PerfFxAlt b i -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { stack = fromMaybe box.stack (modifyAt i (\lyr -> lyr { fx = fxAlt lyr.fx }) box.stack) } else box)
         st.perfBoxes }

  PerfToggleMute b -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { muted = not box.muted } else box)
         st.perfBoxes }

  PerfSetTerm b t -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { term = t } else box)
         st.perfBoxes }

  PerfSetSeq b txt -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { seqText = txt } else box)
         st.perfBoxes }

  -- the round-trip commit: parse the whole pipeline text back into the structured
  -- box (seq part + layer stack), so the text field and the chips stay one thing.
  PerfSetPipeline b txt -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then let r = parsePipeline txt in box { seqText = r.seqText, stack = r.stack } else box)
         st.perfBoxes }

  PerfOpenEdit b -> H.modify_ _ { perfEditBox = Just b }

  PerfCloseEdit -> H.modify_ _ { perfEditBox = Nothing }

  PerfNop -> pure unit

  -- Starting a drag abandons any click-to-place hold, so the two gestures can't
  -- coexist and leave a stray held layer to be dropped by a later bubbled event.
  PerfDragStart src -> H.modify_ _ { perfDrag = Just src, perfHeld = Nothing, perfHeldFx = Nothing }

  PerfDragEnd -> H.modify_ _ { perfDrag = Nothing }

  -- Run an inner-control action but stop the click bubbling to the box's
  -- placement onClick — otherwise nudging/removing a layer while something is in
  -- hand would also drop that held item onto the box.
  PerfStopClick ev act -> do
    liftEffect $ stopPropagation (ME.toEvent ev)
    handleAction act

  -- Drop the dragged layer BEFORE box b's chip i (reorder within a box, or precise
  -- cross-box placement). Consumes perfDrag, so the bubbled box-level PerfDropBox
  -- that follows is a no-op.
  PerfDropOnChip ev b i -> do
    -- stop the drop bubbling to the box-level PerfDropBox (which would otherwise
    -- also drop any click-held layer onto the box — a spurious duplicate).
    liftEffect $ stopPropagation (DE.toEvent ev)
    st <- H.get
    case st.perfDrag of
      Just src -> H.modify_ _ { perfBoxes = dropFxInto src b (Just i) st.perfBoxes, perfDrag = Nothing }
      Nothing -> pure unit

  -- Wheel-zoom the geometric viewport toward the cursor. The point under the
  -- pointer stays fixed: the centre's offset from it scales by the zoom ratio.
  ZoomAt ev dy -> do
    liftEffect (preventDefault ev)
    st <- H.get
    ux <- liftEffect (svgXFromEvent ev)
    uy <- liftEffect (svgYFromEvent ev)
    let factor = if dy > 0.0 then 1.0 / 1.06 else 1.06
        z' = max 0.3 (min 5.0 (st.viewZoom * factor))
        r = st.viewZoom / z'
    H.modify_ _
      { viewZoom = z'
      , viewCx = ux + (st.viewCx - ux) * r
      , viewCy = uy + (st.viewCy - uy) * r
      }

  -- Grab-to-pan: remember the user-space point under the cursor; each move shifts
  -- the centre so that point stays under the cursor (self-correcting).
  PanStart ev -> do
    ux <- liftEffect (svgXFromEvent ev)
    uy <- liftEffect (svgYFromEvent ev)
    H.modify_ _ { panning = Just { ux, uy }, panMoved = false }

  PanMove ev -> do
    st <- H.get
    for_ st.panning \anchor -> do
      ux <- liftEffect (svgXFromEvent ev)
      uy <- liftEffect (svgYFromEvent ev)
      let dx = anchor.ux - ux
          dy = anchor.uy - uy
          mag = max (if dx < 0.0 then -dx else dx) (if dy < 0.0 then -dy else dy)
      H.modify_ _
        { viewCx = st.viewCx + dx
        , viewCy = st.viewCy + dy
        , panMoved = st.panMoved || mag > 3.0
        }

  PanEnd -> H.modify_ _ { panning = Nothing }

  ResetView -> H.modify_ _ { viewCx = 0.0, viewCy = 0.0, viewZoom = 1.0, panning = Nothing, panMoved = false }

  ShakeGenerate -> H.modify_ \s -> s { genRoll = s.genRoll + 1 }

  SetLens l -> H.modify_ _
    { lens = l, hoveredId = Nothing, hoveredTriad = Nothing
    , viewCx = 0.0, viewCy = 0.0, viewZoom = 1.0, panning = Nothing, panMoved = false
    }

  -- Slice E — transpose. In-place shift of a specimen's absolute-MIDI voicing +
  -- bass (arithmetic, since everything is absolute MIDI), relabelled to the new
  -- root. Already-sequenced chords are untouched: they're separate imported
  -- snapshots, so a tank capo never rewrites a built progression.
  TransposeSpec sid n -> do
    st <- H.get
    let tank' = map (\sp -> if sp.id == sid then transposeSpecimen n sp else sp) st.tank
    H.modify_ _ { tank = tank' }
    for_ (find (\sp -> sp.id == sid) tank') playSpecimen

  CapoTank n -> H.modify_ \s -> s { tank = map (transposeSpecimen n) s.tank }

  -- Slice D — sequence a tank specimen onto the progression. It's materialised as
  -- an IMPORTED (off-lattice) snapshot with its own stable id: it lives in `chords`
  -- for playback / revoice / export but never renders on the lattice and is never
  -- pruned or regenerated. So the progression is self-contained and survives every
  -- context change — the original path-on-lattice staleness bug, fixed. (Same
  -- stable-snapshot mechanism `PickCandidate` and `rebuild` already rely on.)
  SequenceSpec sid -> do
    st <- H.get
    for_ (find (\sp -> sp.id == sid) st.tank) \spec -> do
      let newId = st.nextId
          node = (specToNode newId st.key spec) { isCentre = false }
      H.modify_ _
        { chords = st.chords <> [ node ]
        , imported = Set.insert newId st.imported
        , nextId = newId + 1
        , path = st.path <> [ newId ]
        , sounding = Just newId
        }
      playSpecimen spec

  SetBridgeLen n -> H.modify_ _ { bridgeLen = clamp 0 maxBridge n }
  ToggleFold p -> H.modify_ \st -> case p of
    VCtx -> st { foldCtx = not st.foldCtx }
    VProg -> st { foldProg = not st.foldProg }

  -- Drop a caught chord into the progression, BRIDGED. Between the current end
  -- and the dropped chord `Vetula.Between` lays `bridgeLen` passing chords (a
  -- tonicizing turnaround into the target's root); the bridge is skipped for the
  -- first chord (nothing to bridge from) or when the dial is 0. All new chords
  -- are minted as imported nodes and appended in order, then the segment plays.
  ArrangeSpec sid -> do
    st <- H.get
    for_ (find (\sp -> sp.id == sid) st.tank) \spec -> do
      let bnotes = case last st.path of
            Just _ -> bridgeNotes st.bridgeLen (specRoot spec)
            Nothing -> []
          nB = length bnotes
          bridgeNodes = mapWithIndex (\i ns -> importChord (st.nextId + i) ns) bnotes
          targetId = st.nextId + nB
          targetNode = (specToNode targetId st.key spec) { isCentre = false }
          newChords = bridgeNodes <> [ targetNode ]
          newIds = map _.id newChords
      H.modify_ _
        { chords = st.chords <> newChords
        , imported = foldr Set.insert st.imported newIds
        , nextId = targetId + 1
        , path = st.path <> newIds
        , sounding = Just targetId
        }
      playPath (maybe newIds (\l -> [ l ] <> newIds) (last st.path))

  -- The play button is now a sticky ARM/cue toggle: flip arm, then let
  -- reconcilePerf start or stop the ticker per (armed && master).
  ToggleArm -> do
    H.modify_ \s -> s { armed = not s.armed }
    reconcilePerf

  -- Push the whole performance to the rig: the shared Reef.Vetula.Perf scheduler
  -- runs on the BEAM (reef_vetula_voice) and its → odo voice conducts reef_voice's
  -- chord overlay — reproducing "Vetula progressions quantising Odonus" entirely in
  -- the backend. A pure function of the absolute pulse, so ONE push is the whole
  -- wire (a re-push after an edit swaps it in place). Push Odonus to the rig first.
  PushVetula -> do
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) ("vetula-perf " <> RV.encodePerf (buildPerf st))

  -- Push the progression's voicings as a real Tidal Pattern (Option B): the rig's
  -- reef_vetula_brush queries it per pulse and, on each chord change, conducts
  -- Odonus (reef_voice) with the active chord's pitch classes — one source of
  -- truth for pads + quantise-target. Single brush voice for now: uses the FIRST
  -- voice's renderer (Strummed → the legato/held reading) and channel. A re-push
  -- live re-voices in place. (Push Odonus to the rig in follow-Vetula mode first.)
  PushBrush -> do
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) (brushMsg st)

  PerfPlay -> H.modify_ _ { armed = true } *> reconcilePerf *> H.raise (ArmChanged true)
  PerfStop -> H.modify_ _ { armed = false } *> reconcilePerf *> H.raise (ArmChanged false)

  -- One 16th-note from the shared scheduler. We use the tick's absolute grid
  -- INDEX as the pulse (so every voice — and every module — aligns to the same
  -- downbeat), read the live tempo for note durations, and schedule at the tick's
  -- lookahead delay so notes land on time.
  PerfTick tick -> do
    st <- H.get
    when st.playing do
      tempo <- case st.binnacle of
        Just bin -> liftEffect (_.tempo <$> Clock.read (Binnacle.clock bin))
        Nothing -> pure (toNumber st.tempo)
      let reefChords = map toReefChord (perfChords st)
          pulseMs = 60000.0 / tempo / 4.0
          -- ATLANTIS (audible=false): keep advancing each voice's read-head so the
          -- pulse + cursor march on (the nav harmonic strip stays live in every
          -- pane), but pass no MIDI-out so nothing sounds locally — the rig's brush
          -- is the sound. SOLO: emit as normal.
          mout = if st.authority == Local then st.midiOut else Nothing
      voices' <- liftEffect $ traverse (stepVoice mout st.routing reefChords tick.index pulseMs tick.delayMs) st.voices
      -- PERFORM boxes: query each filled box's `Pattern` for the current cycle and
      -- schedule the notes it yields (block together; arp/`fast` subdivide). A box
      -- with a text-hatch sequence plays on the BAR grid (mini-notation cycle = one
      -- bar); a plain box on the per-BEAT grid (one chord per beat, unchanged).
      -- MIDI-only (→ odo feeds Odonus via poll, → rig is rig-only).
      let beatMs = pulseMs * 4.0
          barMs = pulseMs * 16.0
      for_ mout \out -> liftEffect $
        for_ st.perfBoxes \box ->
          for_ box.seq \_ ->
            when (not box.muted && box.term == TMidi) $
              if boxUsesSeq box
                then when (tick.index `mod` 16 == 0) $
                       scheduleBox out (tick.index / 16) barMs beatMs tick.delayMs box
                else when (tick.index `mod` 4 == 0) $
                       scheduleBox out (tick.index / 4) beatMs beatMs tick.delayMs box
      H.modify_ _ { pulse = tick.index, voices = voices', clockTempo = tempo, tempo = round tempo }

  -- parse the (possibly edited) Tidal source into note-lists, rebuild them as a
  -- fresh progression of imported chords, and point the path at them. The
  -- voicing = the notes above the (lowest) bass; re-export reproduces the paste.
  LoadSource -> do
    st <- H.get
    let txt = fromMaybe (currentSource st) st.sourceEdit
        noteLists = filter (\ns -> length ns > 0) (parseProgression txt)
        newChords = mapWithIndex (\i ns -> importChord (st.nextId + i) ns) noteLists
        ids = map _.id newChords
    H.modify_ _
      { chords = st.chords <> newChords
      , imported = st.imported <> Set.fromFoldable ids
      , nextId = st.nextId + length newChords
      , path = ids
      , perfName = Nothing
      , sounding = head ids
      , sourceEdit = Nothing
      -- loading a source is a new capture session (don't overwrite the last ◦)
      , lastCapIdx = Nothing
      , lastCapSig = ""
      }

  DragMove ev -> do
    st <- H.get
    for_ st.drag \dg -> do
      -- invert the ladder's pitch map (vertical on Explore, horizontal on the
      -- progression rows), then snap to whole octaves from the grab
      targetMidi <- liftEffect $
        if dg.horizontal
          then (\x -> 36.0 + (x - prowPad) / (prowW - 2.0 * prowPad) * 48.0) <$> svgXFromEvent ev
          else (\y -> 36.0 + (205.0 - y) / 9.8) <$> svgYFromEvent ev
      let newOffset = round ((targetMidi - toNumber dg.startMidi) / 12.0)
      when (newOffset /= dg.offset) do
        let newMidi = clamp 24 96 (dg.startMidi + 12 * newOffset)
        H.modify_ \s -> s
          { drag = Just dg { offset = newOffset }
          , chords = map (setVoice dg.chordId dg.voiceIx newMidi) s.chords
          }
        -- audition the reshaped chord each time it crosses an octave line
        st2 <- H.get
        for_ (find (\c -> c.id == dg.chordId) st2.chords) playChord

  DragEnd -> do
    st <- H.get
    case st.drag of
      -- an actual drag happened: keep the voice in its array SLOT (do NOT re-sort).
      -- The slot is the voice's identity — the ladder-dot colour and the selection
      -- ring are slot-keyed, so re-sorting reshuffled which note wore which colour
      -- (dragging E below C made the gold "E" jump onto C). Dragging moves a voice in
      -- pitch, not in identity, so the slot stays put. Downstream is order-independent:
      -- chordGlyph places each notehead by its own pitch, discRadius uses min/max span,
      -- and the reef realiser sorts its own alphabet. With alt held, the grabbed voice's
      -- ORIGINAL pitch is left behind as a doubled tone (appended as a new slot).
      Just dg | dg.offset /= 0 -> do
        let addDouble v = if dg.double then v <> [ dg.startMidi ] else v
            chords' = map (\c -> if c.id == dg.chordId then c { voicing = addDouble c.voicing } else c) st.chords
        applyChords chords'
        H.modify_ _ { drag = Nothing }
      -- a plain click (select only): leave the voicing alone
      _ -> H.modify_ _ { drag = Nothing }

-- | Run an action with the currently-hovered chord id, if any.
withHovered :: forall o m. MonadAff m => (Int -> H.HalogenM State Action Slots o m Unit) -> H.HalogenM State Action Slots o m Unit
withHovered f = do
  st <- H.get
  for_ st.hoveredId f

-- ---------------------------------------------------------------------------
-- Performance — voices reading the loaded progression on their own clocks
-- ---------------------------------------------------------------------------

-- | A fresh voice over an `n`-chord progression: an empty `pattern` (so it falls back
-- | to the uniform one-bar-per-chord `durs` — the old default clock exactly), nothing
-- | skipped. The user live-codes a pattern to change the read-head.
defaultVoice :: Int -> Int -> Renderer -> Int -> Voice
defaultVoice vid channel renderer n =
  -- prefill the read-head with its REAL default pattern (one chord per bar) as concrete
  -- editable text — WYSIWYG, so the field shows what's actually playing, not a look-alike
  -- placeholder. `durs` stays as the equivalent legacy fallback / rig-push shape.
  { id: vid, channel, name: "", dest: ToMidi, renderer, pattern: defaultPattern n, patternDraft: defaultPattern n
  , notePattern: "", notePatternDraft: "", articulator: RA.ABlock, durs: replicate n 1, phase: 0, cursor: 0, held: [], muted: false }

-- | The FOUR fixed lanes of the bottom voice bar: three MIDI voices (one per
-- | renderer) + one Odonus conductor, ids 0..3, all muted so each is one toggle
-- | from sounding. `n` = progression length (for the legacy `durs` fallback).
-- | Seeded at init AND wherever the voice set is reset, so the bar always finds
-- | its four lanes. Routing (channel/name/odo id) lives in the routing modal.
canonicalVoices :: Int -> Array Voice
canonicalVoices n =
  [ (defaultVoice 0 5 Block n)    { name = "block", muted = true }
  , (defaultVoice 1 5 Strummed n) { name = "strum", muted = true }
  , (defaultVoice 2 5 Arp n)      { name = "arp",   muted = true }
  , (defaultVoice 3 0 Block n)    { dest = ToOdonus, muted = true }
  ]

-- | The clock a voice plays: its committed pattern if non-empty & parseable, else its
-- | legacy `durs`. The single frontend seam onto `Vetula.Playhead` / the reef realiser.
voiceClock :: Int -> Voice -> RV.PerfClock
voiceClock n v = clockFor n { pattern: v.pattern, durs: v.durs }

-- | Bars-per-chord DERIVED from a clock (for the nav strip's dwell display): total
-- | pulses landed on each chord index / 16. 0 = never visited. Works for pattern or durs.
displayDurs :: Int -> RV.PerfClock -> Array Int
displayDurs n clock =
  map (\c -> sum (map _.len (filter (\s -> s.ix == c) clock.segs)) / 16) (range 0 (n - 1))

-- | Fit a voice's duration column to the current chord count (pad new chords with
-- | one bar, drop any trailing extras) — keeps the grid + clock robust if the
-- | progression length and the stored column ever disagree.
padDurs :: Int -> Array Int -> Array Int
padDurs n ds = take n (ds <> replicate n 1)

-- | A voice's timeline: one segment per NON-skipped chord, in chord order, each at
-- | its cumulative pulse offset. 1 bar = 16 pulses (16th notes). Skipped chords
-- | (0 bars) contribute nothing, so a voice plays only the chords it dwells on.
timeline :: Array Int -> Array { ix :: Int, start :: Int, len :: Int }
timeline ds = snd (foldl step (Tuple 0 []) (mapWithIndex Tuple ds))
  where
  step (Tuple off segs) (Tuple i d) =
    if d <= 0 then Tuple off segs
    else Tuple (off + d * 16) (segs <> [ { ix: i, start: off, len: d * 16 } ])

rendOf :: Int -> Renderer
rendOf i = case mod i 3 of
  0 -> Block
  1 -> Arp
  _ -> Strummed

nextRenderer :: Renderer -> Renderer
nextRenderer = case _ of
  Block -> Strummed
  Strummed -> Arp
  Arp -> Block

rendName :: Renderer -> String
rendName = case _ of
  Block -> "block"
  Strummed -> "strum"
  Arp -> "arp"

-- | Modify one voice by id.
updateVoice :: forall o m. MonadAff m => Int -> (Voice -> Voice) -> H.HalogenM State Action Slots o m Unit
updateVoice vid f = H.modify_ \s -> s { voices = map (\v -> if v.id == vid then f v else v) s.voices }

-- | Bring sounding in line with the transport: the shared scheduler runs always,
-- | so this only flips `playing` (= armed && master) and the voice note-state.
-- | Starting clears each voice's cursor/held (a clean attack from the next
-- | onset); stopping note-offs everything held. Called when either flag changes.
reconcilePerf :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
reconcilePerf = do
  st <- H.get
  let want = st.armed
  when (st.playing && not want) (silenceHeld st)
  H.modify_ \s -> s
    { playing = want
    , voices = if want && not s.playing then map (_ { held = [], cursor = 0 }) s.voices
               else if not want then map (_ { held = [] }) s.voices
               else s.voices }

-- | Full stop (used when unloading a progression): disarm, silence, clear voices.
stopClock :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
stopClock = do
  st <- H.get
  silenceHeld st
  H.modify_ _ { playing = false, armed = false, voices = map (_ { held = [] }) st.voices }

-- | Persist the whole library (the auto-capture stack) to localStorage, best-
-- | effort. Called after every library mutation. Entries are all-strings (Tidal
-- | source + label), so this is a plain JSON.stringify — no ChordNode codecs.
persistLib :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
persistLib = do
  s <- H.get
  liftEffect $ Store.saveLibrary { library: s.library, presets: s.presets }

-- | Bank the live progression source as a preset — the CAPTURE hotkey. DEDUPS by
-- | content (an unchanged progression ⇒ identical glyph): already banked ⇒ just
-- | re-park `identity`; otherwise append an anonymous preset. `content` is
-- | `currentSource` (the AskSource text — stable under playback, since a pulse moves
-- | the playhead, not the chords). No-op on an empty progression. Then persist.
captureNow :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
captureNow = do
  s <- H.get
  when (length (pathSteps s) > 0) do
    let text = currentSource s
    case indexOfContent text s.presets of
      Just _ -> H.modify_ _ { identity = Just text }
      Nothing -> H.modify_ \st -> st
        { presets = st.presets <> [ { content: text, name: Nothing, starred: false } ]
        , identity = Just text
        }
    persistLib

-- | Recall preset `i`: rebuild the performed `path` from its source (as `LoadProg`
-- | does from a library entry), and park the chip on the preset's text (glyph SOLID;
-- | ghosts on later divergence). No-op on unparseable / empty source.
recallPreset :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
recallPreset i = do
  st <- H.get
  for_ (st.presets !! i) \p -> do
    let noteLists = filter (\ns -> length ns > 0) (parseProgression p.content)
        fresh = mapWithIndex (\j ns -> importChord (st.nextId + j) ns) noteLists
        ids = map _.id fresh
    when (length ids > 0) do
      H.modify_ _
        { chords = st.chords <> fresh
        , imported = st.imported <> Set.fromFoldable ids
        , nextId = st.nextId + length fresh
        , path = ids
        , perfName = Nothing
        , sounding = head ids
        , identity = Just p.content
        }
      persistLib

-- | The identity-chip view Vetula reports (by pull) to the shell's status board: the
-- | glyph of the parked progression + whether the live progression has diverged from
-- | it (revoiced / edited away). `Nothing` when nothing is parked.
chipViewOf :: State -> Maybe ChipView
chipViewOf s = case s.identity of
  Nothing -> Nothing
  Just text -> Just { glyph: glyphOf text, diverged: currentSource s /= text }

-- | An Amphora library item as a local progression entry. The Tidal source is
-- | the payload; the keyLabel is recovered from a `key:` tag (if present) and
-- | `kept` from a `kept` tag.
amphoraEntry :: Amphora.LibItem -> LibEntry
amphoraEntry it =
  { name: it.name
  , keyLabel: fromMaybe "" (map (SCU.drop 4) (find (\t -> contains (Pattern "key:") t) it.tags))
  , source: it.payload
  , kept: elem "kept" it.tags
  }

-- | Merge incoming (Amphora) entries over the current local ones by name: keep
-- | every local entry, then append any incoming entry whose name isn't present.
mergeLibByName :: Array LibEntry -> Array LibEntry -> Array LibEntry
mergeLibByName current incoming =
  current <> filter (\p -> not (any (\q -> q.name == p.name) current)) incoming

-- | Capture the current progression into the stack — update the session's ephemeral
-- | ◦ in place, or open a new one — then persist. Shared by the settle-timer and the
-- | key-change snapshot; uses the CURRENT key, so a key-change snapshot is
-- | self-contained in the old key. Assumes a non-empty path.
captureSteps :: forall o m. MonadAff m => State -> H.HalogenM State Action Slots o m Unit
captureSteps st = do
  let sig = currentSource st
      kl = groupLabel st.key
  case st.lastCapIdx of
    Just i | Just e <- index st.library i, not e.kept ->
      H.modify_ _ { library = fromMaybe st.library (modifyAt i (_ { keyLabel = kl, source = sig }) st.library), lastCapSig = sig }
    _ -> do
      let n = st.capSeq + 1
          nm = kl <> " ◦" <> show n
      H.modify_ _ { library = st.library <> [ { name: nm, keyLabel: kl, source: sig, kept: false } ]
                  , lastCapIdx = Just (length st.library), capSeq = n, lastCapSig = sig }
  persistLib

-- | Note-off every voice's currently-held notes.
silenceHeld :: forall o m. MonadAff m => State -> H.HalogenM State Action Slots o m Unit
silenceHeld st =
  liftEffect $ for_ st.midiOut \out ->
    for_ st.voices \v -> for_ v.held \nn -> Midi.noteOffAt out { channel: Routing.toWire (midiChannelFor st.routing v), note: nn, delayMs: 0.0 }

-- | The loaded performance progression's chords, resolved from the working copy.
-- | The performed progression. Slice 4a: this IS the live `path` (`pathSteps`) — the
-- | voices read what you're building, with no load-a-copy step. Kept as a named alias
-- | because the reef-projection sites (`buildPerf`, `brushMsg`) read more clearly as
-- | "the performance's chords"; 4b may inline it.
perfChords :: State -> Array ChordNode
perfChords = pathSteps

-- | Project the live performance onto the shared `Reef.Vetula.Perf` (the wire shape
-- | the rig runs): the resolved progression as `{ pcs, notes }` (pcs for the → odo
-- | quantiser, notes = `playNotes` for the V2 MIDI voices) and each voice's dest /
-- | renderer / channel / dwell schedule / phase mapped to the reef enums. The
-- | frontend's own `Renderer`/`VoiceDest` map onto reef's by meaning, not order.
-- | Project a live `ChordNode` onto the shared `Reef.Vetula.Perf` chord: the pitch
-- | classes (→ odo quantiser) + the concrete `playNotes` (→ midi voices). Sorting is
-- | the renderer's job, so `notes` rides through unsorted.
toReefChord :: ChordNode -> RV.VChord
toReefChord c = { pcs: c.pcs, notes: playNotes c }

-- | Project a performance voice onto the shared `VVoice`. The frontend's own
-- | `Renderer`/`VoiceDest` map onto reef's by meaning, not order.
-- | The canonical 1..16 MIDI channel a → midi voice sounds on: its bound routing
-- | name, else the default. Unnamed ("") or unbound names fall to the default channel.
midiChannelFor :: Map String Int -> Voice -> Int
midiChannelFor routing v =
  if v.name == "" then Routing.vetulaDefaultChannel
  else fromMaybe Routing.vetulaDefaultChannel (Map.lookup v.name routing)

toReefVoice :: Map String Int -> Voice -> RV.VVoice
toReefVoice routing v =
  { dest: case v.dest of
      ToMidi -> RV.VToMidi
      ToOdonus -> RV.VToOdonus
  , renderer: case v.renderer of
      Block -> RV.VBlock
      Arp -> RV.VArp
      Strummed -> RV.VStrummed
  -- canonical 1..16 for the rig (link-spike is 1-indexed); no toWire here.
  , channel: midiChannelFor routing v
  , durs: v.durs
  , phase: v.phase
  , muted: v.muted
  }

buildPerf :: State -> { chords :: Array RV.VChord, voices :: Array RV.VVoice }
buildPerf st =
  { chords: map toReefChord (perfChords st)
  , voices: map (toReefVoice st.routing) st.voices
  }

-- | The brush renderer name for the `vetula-voicings` verb. Note Strummed → "held":
-- | on the brush side the old "strum" is the legato / common-tone reading (the
-- | audible voice-leading), not an intra-chord roll.
rendBrush :: Renderer -> String
rendBrush = case _ of
  Block -> "block"
  Arp -> "arp"
  Strummed -> "held"

-- | Build the `vetula-voicings <channel> <renderer> <json>` message: the whole
-- | progression's hand-picked voicings (playNotes per chord) as a compact JSON
-- | `Array (Array Int)` (no spaces — the rig splits the verb on spaces). Single
-- | brush voice: takes the first → MIDI voice's renderer + channel + dwell (the
-- | brush is a MIDI-out voice; a → odo voice only conducts Odonus and sounds no
-- | MIDI, so it must NOT be the brush). Defaults block / ch 8 if there is no MIDI
-- | voice yet. The rig treats <channel> as the link-spike (1-indexed) MIDI channel.
brushMsg :: State -> String
brushMsg st =
  let
    chords = perfChords st
    v0 = find (\v -> v.dest == ToMidi) st.voices
    rend = maybe "block" (rendBrush <<< _.renderer) v0
    -- canonical 1..16 MIDI channel; the rig treats it as link-spike (1-indexed).
    ch = maybe Routing.vetulaDefaultChannel (midiChannelFor st.routing) v0
    durs = maybe (replicate (length chords) 1) _.durs v0
    jsonRow xs = "[" <> joinWith "," (map show xs) <> "]"
    vJson = "[" <> joinWith "," (map (jsonRow <<< playNotes) chords) <> "]"
    dJson = "[" <> joinWith "," (map show durs) <> "]"
    -- {"v":[[..]],"d":[..]} — compact (no spaces; the rig splits the verb on spaces).
    -- v = voicings, d = voice 1's bars-per-chord dwell (0 = skip).
    json = "{\"v\":" <> vJson <> ",\"d\":" <> dJson <> "}"
  in
    "vetula-voicings " <> show ch <> " " <> rend <> " " <> json

-- | One pulse of one voice, rendered by the SHARED `Reef.Vetula.Perf` engine — the
-- | exact code the rig's reef_vetula_voice runs, so browser and rig are identical BY
-- | CONSTRUCTION (block / arp / strum all gated notes; strum's ties come out as one
-- | long gate). A → odo voice sounds no MIDI, it just advances its read-head so the
-- | shell can poll its chord. `held` is retired — gated notes end themselves.
stepVoice :: Maybe Midi.MidiOut -> Map String Int -> Array RV.VChord -> Int -> Number -> Number -> Voice -> Effect Voice
stepVoice mout routing reefChords pulse pulseMs baseDelayMs v =
  let rv = toReefVoice routing v
      clock = voiceClock (length reefChords) v
      cur = fromMaybe v.cursor (RV.cursorAtClock clock v.phase pulse)
      -- Axis B: a non-empty note pattern sequences the current chord's notes; otherwise
      -- fall back to the voice's block / arp / strum renderer.
      -- The voice's articulator alphabet (block = the chord's own notes; voice-led = a
      -- carried line, stable roles) feeds BOTH paths: the Axis-B note-pattern indexes it,
      -- and the block/arp/strum renderer sounds it too (so a plain block or strum voice
      -- honours the articulator — strum over voice-led notes is principled strum).
      alphabets = RA.articulate v.articulator reefChords
      emit = case noteClock v.notePattern of
        Just nc -> RV.renderAlphaClockMidiAt alphabets rv clock nc pulse
        Nothing -> RV.renderAlphaBlockMidiAt alphabets rv clock pulse
  in case v.dest of
    ToOdonus -> pure v { cursor = cur }
    ToMidi -> do
      for_ mout \out ->
        for_ emit \e ->
          Midi.scheduleNote out
            { channel: Routing.toWire (midiChannelFor routing v), note: e.note, velocity: e.velocity
            , delayMs: baseDelayMs, durMs: e.durPulses * pulseMs }
      pure v { cursor = cur, held = [] }

-- | The chord index a voice's read-head is on at this pulse (Nothing if its loop
-- | is empty or it's resting between dwell segments). Shared by the MIDI path and
-- | the Odonus-follow feed, which sounds no MIDI but still tracks the cursor.
-- | Re-anchor a voice so its read-head is at the START of chord `i` at the given
-- | pulse: set `phase` so `cursorAt pulse` = i, and cache `cursor = i` (which the
-- | → odo feed reads directly while stopped). A no-op if the voice skips chord i
-- | (no timeline segment) or the progression is empty. `n` = progression length.
jumpVoice :: Int -> Int -> Int -> Voice -> Voice
jumpVoice n pulse i v =
  let clock = voiceClock n v
      loopLen = clock.loopLen
  in case find (\seg -> seg.ix == i) clock.segs of
       -- normalized positive modulo — `seg.start - pulse` can be negative, and Int
       -- `mod` can return a negative remainder, which cursorAt would then miss.
       Just seg | loopLen > 0 -> v { phase = mod (mod (seg.start - pulse) loopLen + loopLen) loopLen, cursor = i }
       _ -> v

cursorAt :: Array ChordNode -> Int -> Voice -> Maybe Int
cursorAt chords pulse v = RV.cursorAtClock (voiceClock (length chords) v) v.phase pulse

-- | The pick-mode generator mode implied by a step selection over an n-step path:
-- | one chord at the start prepends, at the end appends, in the middle
-- | substitutes; two chords bracket a transition. Nothing = no valid selection.
genModeOf :: Array Int -> Int -> Maybe GenMode
genModeOf sel n = case sel of
  [ i ]
    | n <= 1 -> Just Append
    | i == 0 -> Just Prepend
    | i == n - 1 -> Just Append
    | otherwise -> Just Substitute
  [ _, _ ] -> Just Transition
  _ -> Nothing

-- | Recompute the candidate cloud from the current step selection + adventure dial.
regen :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
regen = do
  st <- H.get
  let steps = pathSteps st
      sel = sort (nub st.genSel)
  case genModeOf sel (length steps) of
    Nothing -> H.modify_ _ { candidates = [] }
    Just mode ->
      H.modify_ _ { candidates = generateCandidates mode (mapMaybe (\ix -> index steps ix) sel) st.key st.adventure st.nextId }

-- | Play one chord (no re-centre) and make it the sounding chord on the ladder.
playId :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
playId pid = do
  H.modify_ _ { sounding = Just pid, selected = Nothing }
  st <- H.get
  for_ (find (\c -> c.id == pid) st.chords) playChord

-- | Append one audition to the CHYRON (the rolling capture buffer), oldest
-- | events falling off past `chyronCap`. Called from the single-chord audition
-- | choke-points below, unconditionally — the trace records intent, so a muted
-- | audition (no `midiOut`) still lands here. See DESIGN-vetula-chyron-redesign.
logChyron :: forall o m. MonadAff m => String -> Array Int -> Array Int -> Anchor -> H.HalogenM State Action Slots o m Unit
logChyron label notes pcs anchor = do
  st <- H.get
  -- record-arm: disarmed → the audition still sounded, it just isn't captured.
  when st.chyronArmed do
    now <- liftEffect dateNow
    -- never log a blank chip: fall back to the pitch-class names if a call site
    -- has no label (e.g. an off-net triad before it's named).
    let lab = if label == "" then joinWith " " (map noteName (sort pcs)) else label
        ev = { label: lab, notes, pcs, at: now, anchor }
    H.modify_ \s -> s { chyron = takeEnd chyronCap (s.chyron <> [ ev ]) }

-- | Schedule notes on the preview channel WITHOUT logging to the chyron — for
-- | re-auditioning a chip already in the trace (no feedback loop).
auditionNotesNoLog :: forall o m. MonadAff m => Array Int -> H.HalogenM State Action Slots o m Unit
auditionNotesNoLog notes = do
  st <- H.get
  for_ st.midiOut \out ->
    liftEffect $ for_ notes \n ->
      Midi.scheduleNote out { channel: st.previewChan, note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }

-- | Canonical content of a sequence — the ordered pc-sets (duplicates KEPT, so a
-- | strum reads as its own token), hashed by `glyphOf` to a stable 2-glyph pair.
seqContent :: Array ChyronEvent -> String
seqContent evs = joinWith " " (map (\e -> joinWith "," (map show (sort e.pcs))) evs)

-- | Play a list of captured events back with their ORIGINAL timing (inter-onset
-- | gaps from each `at`), each chord as a BLOCK (all notes together — no per-note
-- | roll, which read as an unwanted arpeggio). No re-log.
playEvents :: forall o m. MonadAff m => Array ChyronEvent -> H.HalogenM State Action Slots o m Unit
playEvents evs = do
  st <- H.get
  let t0 = maybe 0.0 _.at (head evs)
  for_ st.midiOut \out -> liftEffect $
    for_ evs \ev ->
      for_ ev.notes \n ->
        Midi.scheduleNote out
          { channel: st.previewChan, note: n, velocity: 88, delayMs: ev.at - t0, durMs: 780.0 }

-- | Play the selected chyron span (see `playEvents`). Phase 3 will add a
-- | de-quantised / grid-snapped alternative.
playChyronSelection :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
playChyronSelection = do
  st <- H.get
  case st.chyronSel of
    Just sel | sel.hi > sel.lo -> playEvents (mapMaybe (\ix -> index st.chyron ix) (range sel.lo sel.hi))
    _ -> pure unit

-- | Resolve a drag-drop of a layer into the boxes: pull the layer's value (from the
-- | palette, or out of its source box), remove it from the source box if it came
-- | from one, then insert it into the target box — at `mpos` (Just = before that
-- | chip index) or appended (Nothing). A same-box move shifts the insert index down
-- | by one when the removed layer sat before it.
dropFxInto :: PerfDragSrc -> Int -> Maybe Int -> Array PerfBox -> Array PerfBox
dropFxInto src tb mpos boxes =
  case fxOf src of
    Nothing -> boxes
    Just fx ->
      let removed = case src of
            FromBox sb si -> adjustStack sb (\s -> fromMaybe s (deleteAt si s)) boxes
            FromPalette _ -> boxes
          pos = case mpos of
            Nothing -> maybe 0 (length <<< _.stack) (index removed tb)
            Just i -> case src of
              FromBox sb si | sb == tb && si < i -> i - 1
              _ -> i
      in adjustStack tb (\s -> fromMaybe (s <> [ fx ]) (insertAt pos fx s)) removed
  where
  fxOf = case _ of
    FromPalette fx -> Just (mkLayer fx)
    FromBox b i -> index boxes b >>= \bx -> index bx.stack i
  adjustStack bi f = mapWithIndex (\j bx -> if j == bi then bx { stack = f bx.stack } else bx)

-- | One function-stack layer as a `Pattern (Array Int)` endomorphism. Pitch-shapers
-- | `map` over each chord's notes; Tidal combinators (`Rate`) are polymorphic in
-- | the value, so they compose with the pitch layers at the same type.
applyFx :: PerfFx -> PT.Pattern (Array Int) -> PT.Pattern (Array Int)
applyFx = case _ of
  Transpose k -> map (map (_ + k))
  Octave k -> map (map (_ + 12 * k))
  Rate n
    | n > 0 -> fast (Rat.fromInt n)
    | n < 0 -> slow (Rat.fromInt (-n))
    | otherwise -> identity
  Voice shape -> map (revoice (voiceStrategy shape))
  Select sel -> map (revoice (takeVoicing (selSelector sel)))
  -- arp/strum don't change the chord PATTERN — they explode each chord across
  -- time at the terminal (see `boxRealise`/`scheduleBox`), at a fixed rate. They
  -- sit in the stack as config-carrying layers; their timing applies at the sink.
  Arpg _ _ -> identity
  Strum _ -> identity

-- | The chord→time REALISATION a box's stack asks for — the last arp/strum layer
-- | wins, else a plain block chord. Applied at schedule time (not in the pattern),
-- | at a FIXED per-note rate so dense chords don't rush.
data Realise = RBlock | RArp ArpDir Int | RStrum Int

boxRealise :: Array Layer -> Realise
boxRealise = foldl pick RBlock <<< map _.fx
  where
  pick acc = case _ of
    Arpg dir r -> RArp dir r
    Strum ms -> RStrum ms
    _ -> acc

-- | Apply a layer, gated by its `when` clause — `Always` runs it every cycle;
-- | `Every n` runs it only on cycles divisible by n (Tidal's `every`). Since arp/
-- | strum's `applyFx` is identity (they realise at the sink), gating them here is a
-- | harmless no-op — their `when` is currently ignored.
applyLayer :: Layer -> PT.Pattern (Array Int) -> PT.Pattern (Array Int)
applyLayer { fx, when: w } = case w of
  Always -> applyFx fx
  Every n -> every n (applyFx fx)

-- | Run a Harmonia `Voicing -> Voicing` over one chord's notes. The notes are
-- | sorted low→high first so the strategies and Low/High selectors read voices
-- | correctly, then unwrapped back to a bare `Array Int`.
revoice :: (Voicing -> Voicing) -> Array Int -> Array Int
revoice f = voicingMidi <<< f <<< Voicing <<< sort

voiceStrategy :: VoiceShape -> (Voicing -> Voicing)
voiceStrategy = case _ of
  Open -> openTriad
  Rootless -> rootless
  Drop2 -> drop2
  Drop24 -> drop2and4
  Quartal -> quartal
  Cluster -> cluster

selSelector :: PerfSel -> Selector
selSelector = case _ of
  Low n -> TakeLow n
  High n -> TakeHigh n

-- | A short chip label for a stack layer.
fxLabel :: PerfFx -> String
fxLabel = case _ of
  Transpose n -> "transpose " <> showSigned n
  Octave n -> "8ve " <> showSigned n
  Rate n
    | n > 0 -> "rate ×" <> show n
    | n < 0 -> "rate ÷" <> show (-n)
    | otherwise -> "rate ×1"
  Voice shape -> "voice " <> voiceShapeName shape
  Select (Low n) -> "bottom " <> show n
  Select (High n) -> "top " <> show n
  Arpg dir r -> "arp " <> arpDirGlyph dir <> " ×" <> show r
  Strum ms -> "strum " <> show ms <> "ms"

voiceShapeName :: VoiceShape -> String
voiceShapeName = case _ of
  Open -> "open"
  Rootless -> "rootless"
  Drop2 -> "drop2"
  Drop24 -> "drop2&4"
  Quartal -> "quartal"
  Cluster -> "cluster"

-- | Nudge a layer's parameter by `d` (the chip's − / + controls), clamped. Voice
-- | cycles through the shapes; Select nudges the voice count (min 1).
fxNudge :: Int -> PerfFx -> PerfFx
fxNudge d = case _ of
  Transpose n -> Transpose (clamp (-24) 24 (n + d))
  Octave n -> Octave (clamp (-4) 4 (n + d))
  Rate n -> Rate (clamp (-8) 8 (n + d))
  Voice shape -> Voice (cycleVoiceShape d shape)
  Select (Low n) -> Select (Low (clamp 1 6 (n + d)))
  Select (High n) -> Select (High (clamp 1 6 (n + d)))
  Arpg dir r -> Arpg dir (clamp 1 16 (r + d))     -- nudge the fixed rate (notes/beat)
  Strum ms -> Strum (clamp 0 80 (ms + d))

-- | The layer's ALTERNATE control (the second param when it has one): arp cycles
-- | its direction; everything else is unchanged.
fxAlt :: PerfFx -> PerfFx
fxAlt = case _ of
  Arpg dir r -> Arpg (cycleArpDir dir) r
  other -> other

cycleVoiceShape :: Int -> VoiceShape -> VoiceShape
cycleVoiceShape d shape =
  let shapes = [ Open, Rootless, Drop2, Drop24, Quartal, Cluster ]
      i = fromMaybe 0 (elemIndex shape shapes)
      n = length shapes
  in fromMaybe shape (index shapes (mod (i + d) n))

showSigned :: Int -> String
showSigned n = if n >= 0 then "+" <> show n else show n

-- ============================================================================
-- Canonical pipeline text ⇄ structure — the chrome↔text round-trip.
--
-- A box prints to `<seqPart> # <layer> # <layer> …` and parses back exactly, so
-- the text field and the chips are two views of ONE structured box. The vocabulary
-- is ASCII and round-trippable — NOT the glyph `fxLabel`s, which are lossy. Total +
-- lenient (Selene `Source.purs` discipline): an unknown directive drops; a
-- recognised-but-partial one falls back to a sensible default; values are clamped
-- to the same ranges as the chip nudges. INVARIANT (the reconciliation point):
--   parsePipeline (printPipeline box) == { seqText: trim box.seqText, stack: box.stack }
-- so text-edit and chip-edit can never silently diverge. Use plain `show` here, not
-- `showSigned` — a leading '+' makes `Int.fromString` return Nothing.
-- ============================================================================


printPerfFx :: PerfFx -> String
printPerfFx = case _ of
  Transpose n -> "transpose " <> show n
  Octave n -> "oct " <> show n
  Rate n -> "rate " <> show n
  Voice shape -> "voice " <> printVoiceShape shape
  Select (High n) -> "top " <> show n
  Select (Low n) -> "bottom " <> show n
  Arpg dir r -> "arp " <> printArpDir dir <> " " <> show r
  Strum ms -> "strum " <> show ms

printWhen :: When -> String
printWhen = case _ of
  Always -> ""
  Every n -> " every " <> show n

printLayer :: Layer -> String
printLayer lyr = printPerfFx lyr.fx <> printWhen lyr.when

printPipeline :: PerfBox -> String
printPipeline box =
  let s = trim box.seqText
      layers = map printLayer box.stack
  in if s == "" && length layers == 0 then ""
     else if s == "" then "# " <> joinWith " # " layers
     else joinWith " # " ([ s ] <> layers)

-- whitespace tokens of a segment (drops empty tokens from runs of spaces).
tokensOf :: String -> Array String
tokensOf = filter (_ /= "") <<< split (Pattern " ") <<< trim

-- one integer token, lenient: strips a leading '+' (which `fromString` rejects),
-- falls back to `def` on anything non-numeric.
tokInt :: Int -> String -> Int
tokInt def s = fromMaybe def (fromString (fromMaybe s (stripPrefix (Pattern "+") s)))

parsePerfFx :: Array String -> Maybe PerfFx
parsePerfFx toks = case head toks of
  Nothing -> Nothing
  Just kw ->
    let args = drop 1 toks
        a0 d = tokInt d (fromMaybe "" (head args))
        a1 d = tokInt d (fromMaybe "" (index args 1))
    in case toLower kw of
         "transpose" -> Just (Transpose (clamp (-24) 24 (a0 0)))
         "trans" -> Just (Transpose (clamp (-24) 24 (a0 0)))
         "oct" -> Just (Octave (clamp (-4) 4 (a0 0)))
         "octave" -> Just (Octave (clamp (-4) 4 (a0 0)))
         "8ve" -> Just (Octave (clamp (-4) 4 (a0 0)))
         "rate" -> Just (Rate (clamp (-8) 8 (a0 2)))
         "voice" -> Just (Voice (fromMaybe Open (head args >>= parseVoiceShape)))
         "top" -> Just (Select (High (clamp 1 6 (a0 1))))
         "bottom" -> Just (Select (Low (clamp 1 6 (a0 1))))
         "arp" -> Just (Arpg (parseArpDir (fromMaybe "up" (head args))) (clamp 1 16 (a1 4)))
         "strum" -> Just (Strum (clamp 0 80 (a0 14)))
         _ -> Nothing

parseLayer :: String -> Maybe Layer
parseLayer seg =
  let toks = tokensOf seg
      n = length toks
      -- peel a trailing `every N` (only when the last token is actually a number)
      everyClause = do
        kw <- index toks (n - 2)
        num <- index toks (n - 1) >>= fromString
        if kw == "every" then Just num else Nothing
      body = maybe toks (\_ -> take (n - 2) toks) everyClause
      w = maybe Always Every everyClause
  in map (\fx -> { fx, when: w }) (parsePerfFx body)

parsePipeline :: String -> { seqText :: String, stack :: Array Layer }
parsePipeline txt =
  let segs = split (Pattern "#") txt
  in { seqText: trim (fromMaybe "" (head segs))
     , stack: mapMaybe parseLayer (drop 1 segs)
     }

-- | The `Pattern (Array Int)` a Perform box realises this cycle: its saved sequence
-- | as a looping chord pattern (one chord per beat-cycle), with the box's function
-- | `stack` folded over it (first layer applied first / innermost).
-- | The text-hatch sequence, if the box has a parseable mini-notation over its
-- | token's chord indices: each index event becomes that chord (out-of-range → a
-- | rest), cycle = one bar. `Nothing` when the field is empty or won't parse (fall
-- | back to the default one-chord-per-beat `fromChords`).
seqPattern :: Array (Array Int) -> String -> Maybe (PT.Pattern (Array Int))
seqPattern chords txt
  | trim txt == "" = Nothing
  | otherwise = case parseMiniPattern txt of
      Left _ -> Nothing
      Right idxPat -> Just (map (\s -> fromMaybe [] (fromString (trim s) >>= index chords)) idxPat)

-- | Whether a box plays on the BAR grid (a valid text-hatch sequence) rather than
-- | the default per-beat grid.
boxUsesSeq :: PerfBox -> Boolean
boxUsesSeq box = case box.seq of
  Just s -> isJust (seqPattern (map _.notes s.events) box.seqText)
  Nothing -> false

boxPattern :: PerfBox -> PT.Pattern (Array Int)
boxPattern box = foldl (\p lyr -> applyLayer lyr p) base box.stack
  where
  base = case box.seq of
    Nothing -> fromChords []
    Just s ->
      let chords = map _.notes s.events
      in fromMaybe (fromChords chords) (seqPattern chords box.seqText)

-- | Query a box's pattern over this beat-cycle `b` and schedule every chord-event
-- | it yields on the box's channel, positioned by the event's arc within the beat.
-- | The stack's realisation (`boxRealise`) spreads each chord's notes across TIME:
-- | Block = all together; Arp = a FIXED step per note (beatMs / rate — density
-- | doesn't change the speed); Strum = a small fixed ms onset stagger. `fast`/
-- | `rate` subdivide the beat orthogonally (they change the chord pattern upstream).
scheduleBox :: Midi.MidiOut -> Int -> Number -> Number -> Number -> PerfBox -> Effect Unit
scheduleBox out c cycleMs beatMs baseDelayMs box =
  let realise = boxRealise box.stack
  in for_ (query (boxPattern box) (mkState (mkArc (Rat.fromInt c) (Rat.fromInt (c + 1))))) \ev ->
       when (isDigital ev) $
         for_ (eventWhole ev) \(Arc w) ->
           let startMs = baseDelayMs + Rat.toNumber (w.start - Rat.fromInt c) * cycleMs
               slotMs = max 20.0 (Rat.toNumber (w.stop - w.start) * cycleMs)
               notes = case realise of
                 RArp dir _ -> arpOrder dir (eventValue ev)
                 _ -> eventValue ev
               -- fixed per-note step (ms): arp = one note per (beat / rate);
               -- strum = a small fixed stagger; block = 0 (all together).
               stepMs = case realise of
                 RArp _ rate -> beatMs / toNumber (max 1 rate)
                 RStrum ms -> toNumber ms
                 RBlock -> 0.0
               noteDur = case realise of
                 RArp _ rate -> max 20.0 (beatMs / toNumber (max 1 rate) * 0.9)
                 _ -> max 20.0 (slotMs * 0.9)
           in for_ (mapWithIndex Tuple notes) \(Tuple k note) ->
                Midi.scheduleNote out
                  { channel: box.channel, note, velocity: 90
                  , delayMs: startMs + toNumber k * stepMs, durMs: noteDur }

-- | Send a chord's notes to the MIDI bus (no state change) and log it to the chyron.
playChord :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
playChord c = do
  st <- H.get
  let notes = playNotes c
  for_ st.midiOut \out ->
    liftEffect $ for_ notes \n ->
      Midi.scheduleNote out { channel: st.previewChan, note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }
  logChyron c.label notes (nub (map (\x -> mod x 12) notes)) c.anchor

-- | Audition a tank specimen: sound its notes on the preview channel (no state
-- | change) and log it to the chyron. Same shape as `playChord`, but reads a
-- | self-contained Specimen.
playSpecimen :: forall o m. MonadAff m => Specimen -> H.HalogenM State Action Slots o m Unit
playSpecimen s = do
  st <- H.get
  let notes = specNotes s
  for_ st.midiOut \out ->
    liftEffect $ for_ notes \n ->
      Midi.scheduleNote out { channel: st.previewChan, note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }
  logChyron s.label notes (nub (map (\x -> mod x 12) notes)) s.anchor

-- | In-place transpose of a tank specimen by `n` semitones — the capo move. Bass
-- | and every upper voice shift arithmetically (absolute MIDI), and the label is
-- | recomputed to the new root name. (The label loses any richer suffix — labels
-- | are informational per the model, and the glyph shows the truth — but for the
-- | bare root names the tank carries today that's exactly right.)
transposeSpecimen :: Int -> Specimen -> Specimen
transposeSpecimen n s =
  s { voicing = map (_ + n) s.voicing
    , bass = s.bass + n
    , anchor = Graded.transpose n s.anchor   -- move the reading with the pitches
    , label = noteName (mod (s.bass + n) 12)
    }

-- | Reconstruct a pool `ChordNode` from a frozen tank `Specimen` — the reverse of
-- | a catch. The specimen shed its lattice identity, so we rebuild the fields the
-- | surface needs: pitch-class set from the sounding notes, root ≈ the bass pc (a
-- | fair placement anchor even for slash voicings), and its own voicing verbatim.
-- | `place` then positions it as a centre; generation blooms around it.
-- | A caught chord's ROOT pitch class — from its Harmonia reading when it has one
-- | (`Located` → `chordRoot` against the anchor's own key), else its frozen foot.
-- | The betweening engine needs it to tonicize toward the target.
specRoot :: Specimen -> Int
specRoot s = case s.anchor of
  Located k dc -> chordRoot k dc
  Free -> mod s.bass 12

specToNode :: Int -> Key -> Specimen -> ChordNode
specToNode newId key s =
  let pcs = nub (map (\n -> mod n 12) ([ s.bass ] <> s.voicing))
      base =
        { id: newId, parentId: Nothing, root: mod s.bass 12, bassPc: mod s.bass 12
        , pcs, voicing: s.voicing, kind: Seed, label: s.label, pinned: false
        , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: true
        , anchor: s.anchor }   -- carry the tank reading back onto the surface
  in place key base base

-- | Arpeggiate a path: each chord in turn, lightly rolled, ~440ms apart — the
-- | segment heard as a phrase (the consonant, directional walk AC noticed).
playPath :: forall o m. MonadAff m => Array Int -> H.HalogenM State Action Slots o m Unit
playPath ids = do
  st <- H.get
  let chordsOnPath = mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) ids
      stepMs = 440.0
      rollMs = 22.0
  for_ st.midiOut \out -> liftEffect $
    for_ (mapWithIndex Tuple chordsOnPath) \(Tuple i c) ->
      for_ (mapWithIndex Tuple (playNotes c)) \(Tuple j n) ->
        Midi.scheduleNote out
          { channel: st.previewChan, note: n, velocity: 88
          , delayMs: toNumber i * stepMs + toNumber j * rollMs, durMs: stepMs * 0.9 }

-- | Write a changed chord set back for rendering. A revoice (Tab / arrow-nudge)
-- | changes only the glyph and the bubble's size — both read from `chords` — and
-- | never a node's lattice position. So we deliberately do NOT re-feed the
-- | simulation: `h.updateData` re-heats it, which reads as a distracting jolt of
-- | the whole cloud on every Tab. The disc resizes from the new voicing; we just
-- | don't let collision re-settle, which is exactly what we want here.
applyChords :: forall o m. MonadAff m => Array ChordNode -> H.HalogenM State Action Slots o m Unit
applyChords chords' = H.modify_ _ { chords = chords' }

-- | Tab-cycle the sounding chord through its candidate voicings (dir +1 / -1),
-- | auditioning each. The candidate list is captured on the first Tab and reused
-- | while it stays in sync with the chord; any other edit (drag / arrow) leaves
-- | the cycle stale, so we rebuild it from the current voicing.
cycleVoicing :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
cycleVoicing dir = do
  st <- H.get
  for_ st.sounding \sid ->
    for_ (find (\c -> c.id == sid) st.chords) \c -> do
      let cyc = case st.cycle of
            Just vc | vc.chordId == sid && index vc.options vc.ix == Just c.voicing -> vc
            _ -> { chordId: sid, options: voicingCandidates c, ix: 0 }
          n = length cyc.options
      when (n > 0) do
        let ix' = mod (cyc.ix + dir + n) n
            newV = fromMaybe c.voicing (index cyc.options ix')
            chords' = map (\d -> if d.id == sid then d { voicing = newV } else d) st.chords
        H.modify_ _ { cycle = Just (cyc { ix = ix' }) }
        applyChords chords'
        for_ (find (\d -> d.id == sid) chords') playChord

-- | Nudge the selected ladder voice (dir +1 = up / -1 = down): an upper voice
-- | shifts by an octave; the bass rotates to the next chord tone already present
-- | (an inversion, never adding a note).
nudgeSelected :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
nudgeSelected dir = do
  st <- H.get
  for_ st.sounding \sid ->
    for_ st.selected \sel ->
      for_ (find (\c -> c.id == sid) st.chords) \c -> do
        let c' = case sel of
              UpperVoice ix ->
                c { voicing = fromMaybe c.voicing (modifyAt ix (\m -> clamp 24 96 (m + 12 * dir)) c.voicing) }
              BassVoice -> c { bassPc = rotateBass dir c }
            chords' = map (\d -> if d.id == sid then c' else d) st.chords
        applyChords chords'
        playChord c'

-- | Space: play the hovered bubble if pointing at one; otherwise re-audition the
-- | sounding chord (so you can hear it again while working the ladder / Tabbing).
playHoveredOrSounding :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
playHoveredOrSounding = do
  st <- H.get
  case st.hoveredChyron of
    -- pointer over the chyron: audition the hovered chip, or — if it's inside a
    -- completed span — play the whole selection with its original timing. Never
    -- re-logs (it's already in the trace).
    Just i | Just ev <- index st.chyron i ->
      case st.chyronSel of
        Just sel | sel.hi > sel.lo, i >= sel.lo, i <= sel.hi -> playChyronSelection
        _ -> auditionNotesNoLog ev.notes
    _ -> case st.hoveredSpec of
      -- pointer over a tank tile: preview that frozen specimen straight from its
      -- own voicing (no state change), so you can explore the tank by ear too.
      Just sid | Just spec <- find (\sp -> sp.id == sid) st.tank -> playSpecimen spec
      _ -> case st.hoveredTriad of
        -- Tonnetz: a hovered triangle has no pool id, so preview it straight from its
        -- root + pitch classes (no state change, like the candidate preview below).
        Just t -> playChord (triadNode t.root t.pcs "")
        Nothing -> case st.hoveredId of
          -- in pick mode the hovered bubble is a candidate (not yet in `chords`);
          -- preview it without committing (no sounding change, no insert)
          Just hid | Just cand <- find (\c -> c.id == hid) st.candidates -> playChord cand
          Just hid -> playId hid
          Nothing -> for_ st.sounding \sid -> for_ (find (\c -> c.id == sid) st.chords) playChord

-- | The key/scale a chord is gathered under — the bubblepack it joins.
groupLabel :: Key -> String
groupLabel key = noteName key.tonic <> " " <> modeShort key.mode

modeShort :: Mode -> String
modeShort = case _ of
  Ionian -> "major"
  Aeolian -> "minor"
  Dorian -> "dorian"
  Phrygian -> "phrygian"
  Lydian -> "lydian"
  Mixolydian -> "mixolydian"
  Locrian -> "locrian"
  HarmonicMinor -> "harm. minor"
  MelodicMinor -> "mel. minor"
  LocrianNat6 -> "locrian ♮6"
  IonianSharp5 -> "ionian ♯5"
  DorianSharp4 -> "dorian ♯4"
  PhrygianDominant -> "phryg. dom."
  LydianSharp2 -> "lydian ♯2"
  Ultralocrian -> "ultralocrian"
  DorianFlat2 -> "dorian ♭2"
  LydianAugmented -> "lydian aug."
  LydianDominant -> "lydian dom."
  MixolydianFlat6 -> "mixo. ♭6"
  LocrianNat2 -> "locrian ♮2"
  Altered -> "altered"
  Custom _ -> "custom"

-- | Inverse of `groupLabel`: parse a stored key label ("F# phryg. dom.", "C major")
-- | back into a Key. The note name is the first word; the rest is a `modeShort`
-- | value (which can itself contain spaces, so split at the FIRST space only). Used
-- | when loading a saved progression so its key becomes the live harmonic context.
parseKeyLabel :: String -> Maybe Key
parseKeyLabel lbl = case SCU.indexOf (Pattern " ") lbl of
  Nothing -> Nothing
  Just ix -> do
    let noteTok = SCU.take ix lbl
        modeTok = SCU.drop (ix + 1) lbl
    tonic <- find (\pc -> noteName pc == noteTok) (range 0 11)
    mode <- _.mode <$> find (\c -> modeShort c.mode == modeTok) modeChoices
    pure { tonic, mode }

-- | Star / unstar the sounding chord's current voicing in its favourites — the
-- | kept voicings of this one note-set, surfaced in the strip above the ladder.
toggleFavorite :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
toggleFavorite = do
  st <- H.get
  for_ st.sounding \sid ->
    for_ (find (\c -> c.id == sid) st.chords) \c -> do
      let key = pcsKey c
          cur = fromMaybe [] (Map.lookup key st.favorites)
          next = if elem c.voicing cur then filter (_ /= c.voicing) cur else cur <> [ c.voicing ]
          favs' = if length next == 0 then Map.delete key st.favorites else Map.insert key next st.favorites
      H.modify_ _ { favorites = favs' }

-- | A chord's favourites key — its note-set, so favoured voicings follow the
-- | actual notes (and survive key changes) rather than a transient node id.
pcsKey :: ChordNode -> String
pcsKey c = show (sort (nub c.pcs))

-- | The next chord tone (cyclically) above/below the current bass — limited to
-- | pitch classes already in the chord.
rotateBass :: Int -> ChordNode -> Int
rotateBass dir c =
  let tones = sort (nub c.pcs)
      n = length tones
  in case elemIndex c.bassPc tones of
       Just i | n > 0 -> fromMaybe c.bassPc (index tones (mod (i + dir + n) n))
       _ -> c.bassPc

-- | Spawn a family of children of the hovered chord and ENTER them into the
-- | graph without disturbing what's already there. The focus (the reference
-- | frame the x-axis measures from) stays put, so existing nodes keep their
-- | positions; `handle.updateData` does the keyed enter, animating only the new
-- | children in. New chords whose content already exists are skipped, so
-- | re-pressing a key doesn't pile up duplicates.
spawn :: forall o m. MonadAff m => Family -> Int -> H.HalogenM State Action Slots o m Unit
spawn fam hid = do
  st <- H.get
  case find (\c -> c.id == hid) st.chords, find (\c -> c.id == st.focusId) st.chords, st.handle of
    Just h, Just focus, Just handle -> do
      let -- prune h's abandoned siblings (and their subtrees), unless pinned —
          -- but only when h was itself generated; expanding a palette seed never
          -- prunes its peers.
          siblingRoots = case h.parentId of
            Just p -> map _.id (filter (\c -> c.parentId == Just p && c.id /= h.id && not c.pinned) st.chords)
            Nothing -> []
          pruned = pruneSet st.chords siblingRoots
          surviving = filter (\c -> not (elem c.id pruned)) st.chords
          existing = map contentKey surviving
          fresh = filter (\c -> not (elem (contentKey c) existing)) (generate fam h)
          kids = mapWithIndex (\i c -> place st.key focus (c { id = st.nextId + i, parentId = Just h.id })) fresh
          chords' = surviving <> kids
      _ <- liftEffect $ handle.updateData (map mkSimNode chords') []
      H.modify_ _ { chords = chords', nextId = st.nextId + length kids }
    _, _, _ -> pure unit

-- | Lattice tab: explode a seed triad into the web of its extensions,
-- | suspensions and bass-inversions (linked back to the seed) — or, if already
-- | exploded, collapse it back to the bare triad.
explode :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
explode pid = do
  st <- H.get
  case find (\c -> c.id == pid) st.chords, st.handle of
    Just c, Just handle ->
      if any (\d -> d.parentId == Just pid) st.chords
        then do
          let descendants = filter (_ /= pid) (pruneSet st.chords [ pid ])
              surviving = filter (\d -> not (elem d.id descendants)) st.chords
          _ <- liftEffect $ handle.updateData (map mkSimNode surviving) (neighborLinks surviving)
          H.modify_ _ { chords = surviving }
        else do
          let famKey = fromMaybe st.key (Map.lookup c.id st.familyScale)
              family = latticeFamily famKey c
              existing = map contentKey st.chords
              fresh = nubByEq (\a b -> contentKey a.chord == contentKey b.chord)
                        (filter (\f -> not (elem (contentKey f.chord) existing)) family)
              kids = mapWithIndex
                       (\i f -> latticePlace c f.level f.lean (f.chord { id = st.nextId + i, parentId = Just c.id }))
                       fresh
              chords' = st.chords <> kids
          _ <- liftEffect $ handle.updateData (map mkSimNode chords') (neighborLinks chords')
          H.modify_ _ { chords = chords', nextId = st.nextId + length kids }
    _, _ -> pure unit

-- | Number-key stacking: add the scale tone `(nKey−1)` generic steps above the
-- | focused family's current head note, materialising the next interior chord and
-- | making it the new head. `3·3·3` climbs a seventh; `2·4` builds a sus2; the
-- | added tone's quality is the scale's to decide. (`e` is the friendly third.)
stackOn :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
stackOn nKey = stackBy (nKey - 1)

stackBy :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
stackBy steps = do
  st <- H.get
  for_ st.focusedFamily \seedId ->
    for_ (find (\c -> c.id == seedId) st.chords) \seed -> do
      let famKey = fromMaybe st.key (Map.lookup seedId st.familyScale)
          hd = stackingHead st seedId seed
          s = scaleSet famKey
          n = length s
          newTop = hd.top + steps                       -- ABSOLUTE degree — keeps climbing
          newPc = fromMaybe seed.root (s !! mod newTop n)
          newPcs = sort (nub ([ newPc ] <> hd.pcs))
      if sort (nub hd.pcs) == newPcs
        -- the new tone repeats a pitch class already present (an octave up): no new
        -- node, but advance the top so the next third reaches a fresh scale tone.
        then H.modify_ _ { stackHead = Just { id: hd.id, top: newTop } }
        else do
          hid <- materialize seedId famKey newPcs
          for_ hid \i -> do
            H.modify_ _ { stackHead = Just { id: i, top: newTop } }
            playId i

-- | Friendly `e` — "add the next third." On a bare root it lays the whole triad
-- | in one press (the lonely dyad is skipped); otherwise it is exactly a third
-- | stacked (`stackBy 2`), so `e·e` reaches the seventh, `e·e·e` the ninth.
stackThird :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
stackThird = do
  st <- H.get
  for_ st.focusedFamily \seedId ->
    for_ (find (\c -> c.id == seedId) st.chords) \seed -> do
      let famKey = fromMaybe st.key (Map.lookup seedId st.familyScale)
          hd = stackingHead st seedId seed
      if length hd.pcs <= 1
        then do
          let triad = triadOn famKey seed.root
          hid <- materialize seedId famKey triad
          for_ hid \i -> do
            H.modify_ _ { stackHead = Just { id: i, top: seedTopDeg famKey (seed { pcs = triad }) } }
            playId i
        else stackBy 2

-- | Friendly `s` — drop the focused seed's scale-pure suspension set (sus2, sus4,
-- | no-3, no-5) into the family. Each is an interior member, so they dedupe and
-- | wire like any other lattice node. Leaves the stacking head where it was.
suspend :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
suspend = do
  st <- H.get
  for_ st.focusedFamily \seedId ->
    for_ (find (\c -> c.id == seedId) st.chords) \seed -> do
      let famKey = fromMaybe st.key (Map.lookup seedId st.familyScale)
      for_ (suspendSet famKey seed) \pcs ->
        void (materialize seedId famKey pcs)

-- | The chord the next stack press climbs from: the remembered head (with its
-- | absolute top degree) if it still belongs to the focused family, else the
-- | seed itself (its top degree read off its pitch classes).
stackingHead :: State -> Int -> ChordNode -> { id :: Int, top :: Int, pcs :: Array Int }
stackingHead st seedId seed =
  case st.stackHead >>= \h -> map (\c -> { node: c, top: h.top }) (find (\c -> c.id == h.id) st.chords) of
    Just { node, top } | node.id == seedId || node.parentId == Just seedId ->
      { id: node.id, top, pcs: node.pcs }
    _ ->
      let famKey = fromMaybe st.key (Map.lookup seedId st.familyScale)
      in { id: seed.id, top: seedTopDeg famKey seed, pcs: seed.pcs }

-- | The absolute scale degree of a chord's highest note (read off its pitch
-- | classes — correct for any chord spanning at most an octave, which the seeds
-- | and freshly-summoned triads always do).
seedTopDeg :: Key -> ChordNode -> Int
seedTopDeg key c =
  let s = scaleSet key
  in fromMaybe 0 (maximum (map (\pc -> fromMaybe 0 (elemIndex (mod pc 12) s)) c.pcs))

-- | Add a lattice child with explicit pitch classes to the focused family,
-- | placed in its stratum and deduped by content against existing nodes; returns
-- | the resulting node's id (existing or fresh). The surgical counterpart to
-- | `explode`'s firehose — shared by the number / `e` / `s` populators.
materialize :: forall o m. MonadAff m => Int -> Key -> Array Int -> H.HalogenM State Action Slots o m (Maybe Int)
materialize seedId famKey pcs = do
  st <- H.get
  case find (\c -> c.id == seedId) st.chords, st.handle of
    Just seed, Just handle -> do
      let meta = latticeChild famKey seed pcs
          childBase = meta.chord { id = st.nextId, parentId = Just seedId }
          -- in-scale extensions climb the interior column by level; extensions that
          -- step OUTSIDE the home scale (e.g. when growing a borrowed chord in its
          -- own modal world) rise onto the outside shelf by their ring index.
          shelf = placeOutside st.key childBase
          child = if shelf.outside == 0
                    then latticePlace seed meta.level meta.lean childBase
                    else shelf
          ck = contentKey child
      case find (\c -> contentKey c == ck) st.chords of
        Just existing -> pure (Just existing.id)
        Nothing -> do
          let chords' = st.chords <> [ child ]
          _ <- liftEffect $ handle.updateData (map mkSimNode chords') (neighborLinks chords')
          H.modify_ _ { chords = chords', nextId = st.nextId + 1 }
          pure (Just child.id)
    _, _ -> pure Nothing

-- | Place an exploded child in the stratified lattice that floats above the seed
-- | (and above the keyboard): y = extension level (the vertical rank — triad tones
-- | at the base, 7→9→11→13 rising), so stacking climbs straight up the root's
-- | "column of light." x = the root's column plus a small signed `lean` nudge
-- | (sus2 left, sus4 right — see `Harmony.familyMeta`); chords sharing a level
-- | then beeswarm apart horizontally under the collide force.
latticePlace :: ChordNode -> Int -> Int -> ChordNode -> ChordNode
latticePlace seed level lean child =
  child
    { targetX = keyX seed.root + toNumber lean * latticeColW
    , targetY = latticeBaseY - toNumber level * latticeRowH
    , outside = 0
    , isCentre = false
    }

-- | The lattice's edges: every pair of exploded chords (in the same seed's
-- | family) that differ by exactly one note — the covering relations of the
-- | subset lattice (its Hasse diagram). Used both as force links (they pull
-- | one-step neighbours together) and as the drawn web. Seeds are excluded, so
-- | the old star-of-links to the root triad is gone.
neighborLinks :: Array ChordNode -> Array { source :: Int, target :: Int }
neighborLinks chords =
  let fam = filter (\c -> c.parentId /= Nothing) chords
      pairs = concat (mapWithIndex (\i a -> map (\b -> Tuple a b) (drop (i + 1) fam)) fam)
  in concatMap
       (\(Tuple a b) ->
          if a.parentId == b.parentId && pcSymDiff a.pcs b.pcs == 1
            then [ { source: a.id, target: b.id } ]
            else [])
       pairs

-- | Size of the symmetric difference of two pitch-class sets (already nubbed).
pcSymDiff :: Array Int -> Array Int -> Int
pcSymDiff a b =
  length (filter (\x -> not (elem x b)) a) + length (filter (\x -> not (elem x a)) b)

latticeBaseY :: Number
latticeBaseY = 150.0

latticeColW :: Number
latticeColW = 50.0

latticeRowH :: Number
latticeRowH = 52.0

-- | The exterior signpost generators — each a curated set the scale-pure interior
-- | can't reach, dropped (and re-toggled) by a button. A generator is just
-- | `Key -> Array ChordNode`, so new harmonic worlds (quartal, Satie, whole-tone)
-- | are additive entries here.
exteriorGens :: Array { key :: String, label :: String, gen :: Key -> Array ChordNode }
exteriorGens =
  [ { key: "mcmullen", label: "McMullen", gen: mcmullenChords }
  , { key: "butler",   label: "Butler",   gen: butlerChords }
  , { key: "stock",    label: "Stock",    gen: stockChords }
  ]

-- | Seeds + focus for the lattice surface.
seedsFor :: Key -> Array ChordNode
seedsFor = diatonicTriads

seedFocus :: Int
seedFocus = 0

-- | The set of ids to remove: the given roots plus all their (non-pinned)
-- | descendants, by parent chain. Pinned chords are never pruned.
pruneSet :: Array ChordNode -> Array Int -> Array Int
pruneSet chords roots = go roots
  where
  go acc =
    let next = filter
          (\c -> not c.pinned && not (elem c.id acc) && maybe false (\p -> elem p acc) c.parentId)
          chords
    in if length next == 0 then acc else go (acc <> map _.id next)

-- | Set one upper voice of a chord to a new MIDI pitch (an octave-drag move).
-- | Pitch classes are untouched, so the chord's identity (root, scale-outsideness,
-- | dissonance) is preserved — only its vertical spread, glyph and sound change.
setVoice :: Int -> Int -> Int -> ChordNode -> ChordNode
setVoice cid vix midi c
  | c.id == cid = c { voicing = fromMaybe c.voicing (updateAt vix midi c.voicing) }
  | otherwise = c

-- | A chord's harmonic identity, for de-duplicating generated children.
contentKey :: ChordNode -> String
-- voicing sorted here so a pure slot-reorder (a drag that keeps the same notes) reads
-- as the same identity — the array order is a UI detail, not harmonic content.
contentKey c = show c.pcs <> "|" <> show (sort c.voicing) <> "|" <> show c.bassPc

-- | Reset to the McMullen palette in the current key/scale; pinned survive.
resetPalette :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
resetPalette = do
  st <- H.get
  let kept = filter _.pinned st.chords
      set = nubByEq (\a b -> a.id == b.id) (seedsFor st.key <> kept)
  stopSim
  H.modify_ _ { dropped = Map.empty, borrowMode = Nothing, focusedFamily = Nothing, stackHead = Nothing }
  startWith st.key seedFocus set

-- | Rebuild the palette in a new key/scale, keeping pinned chords.
-- |
-- | Slice 2 — non-destructive key change. Instead of wiping the progression, we
-- | snapshot it (in the OLD key) to the stack, then TRANSPOSE it into the new key
-- | and keep it. To the user the on-screen progression simply transposes; the old
-- | key is preserved on the stack as an undo. The transposed chords become imported
-- | (off-lattice) voicings, keeping their ids so `path` stays valid; the session
-- | pointer is reset so the transposed copy forks a fresh ◦ (the old one is frozen).
rebuild :: forall o m. MonadAff m => Key -> H.HalogenM State Action Slots o m Unit
rebuild key = do
  st <- H.get
  when (length (pathSteps st) > 0) (captureSteps st)
  st2 <- H.get
  let d = nearestShift st2.key.tonic key.tonic
      pathIds = Set.fromFoldable st2.path
      chords' = map (\c -> if Set.member c.id pathIds then transposeChord d c else c) st2.chords
      kept = filter _.pinned st2.chords
      set = nubByEq (\a b -> a.id == b.id) (seedsFor key <> kept)
  stopSim
  H.modify_ _
    { chords = chords'
    , imported = Set.union st2.imported pathIds
    , familyScale = Map.empty, focusedFamily = Nothing, stackHead = Nothing
    , dropped = Map.empty, borrowMode = Nothing, sourceEdit = Nothing
    , genSel = [], candidates = []
    -- keep `path` (the progression, now transposed); fork a fresh capture session
    , lastCapIdx = Nothing, lastCapSig = "" }
  startWith key seedFocus set

-- ---------------------------------------------------------------------------
-- Scale (mode) choices
-- ---------------------------------------------------------------------------

modeChoices :: Array { value :: String, label :: String, mode :: Mode }
modeChoices =
  [ { value: "Ionian", label: "Ionian (major)", mode: Ionian }
  , { value: "Dorian", label: "Dorian", mode: Dorian }
  , { value: "Phrygian", label: "Phrygian", mode: Phrygian }
  , { value: "Lydian", label: "Lydian", mode: Lydian }
  , { value: "Mixolydian", label: "Mixolydian", mode: Mixolydian }
  , { value: "Aeolian", label: "Aeolian (minor)", mode: Aeolian }
  , { value: "Locrian", label: "Locrian", mode: Locrian }
  , { value: "HarmonicMinor", label: "Harmonic minor", mode: HarmonicMinor }
  , { value: "MelodicMinor", label: "Melodic minor", mode: MelodicMinor }
  -- modes of harmonic minor
  , { value: "LocrianNat6", label: "Locrian ♮6", mode: LocrianNat6 }
  , { value: "IonianSharp5", label: "Ionian ♯5 (aug major)", mode: IonianSharp5 }
  , { value: "DorianSharp4", label: "Dorian ♯4 (Romanian)", mode: DorianSharp4 }
  , { value: "PhrygianDominant", label: "Phrygian dominant", mode: PhrygianDominant }
  , { value: "LydianSharp2", label: "Lydian ♯2", mode: LydianSharp2 }
  , { value: "Ultralocrian", label: "Ultralocrian", mode: Ultralocrian }
  -- modes of melodic minor
  , { value: "DorianFlat2", label: "Dorian ♭2", mode: DorianFlat2 }
  , { value: "LydianAugmented", label: "Lydian augmented", mode: LydianAugmented }
  , { value: "LydianDominant", label: "Lydian dominant (acoustic)", mode: LydianDominant }
  , { value: "MixolydianFlat6", label: "Mixolydian ♭6", mode: MixolydianFlat6 }
  , { value: "LocrianNat2", label: "Locrian ♮2 (half-dim)", mode: LocrianNat2 }
  , { value: "Altered", label: "Altered (super locrian)", mode: Altered }
  ]

-- | The 21 modes split into their three parent-scale families, for the fly-out
-- | (`cascadingInput`) scale pickers. Labels come from `modeChoices` (single
-- | source of truth); each family lists its modes by `value`.
modeGroups :: Array Select.OptionGroup
modeGroups =
  [ grp "Major modes"
      [ "Ionian", "Dorian", "Phrygian", "Lydian", "Mixolydian", "Aeolian", "Locrian" ]
  , grp "Harmonic minor modes"
      [ "HarmonicMinor", "LocrianNat6", "IonianSharp5", "DorianSharp4", "PhrygianDominant", "LydianSharp2", "Ultralocrian" ]
  , grp "Melodic minor modes"
      [ "MelodicMinor", "DorianFlat2", "LydianAugmented", "LydianDominant", "MixolydianFlat6", "LocrianNat2", "Altered" ]
  ]
  where
  grp label vals = { label, options: mapMaybe lookupOpt vals }
  lookupOpt v = map (\m -> { value: m.value, label: m.label }) (find (\m -> m.value == v) modeChoices)

-- | The borrow-source picker's groups: a leading single-item "clear" family
-- | (cascade hides flat options, so the off-switch must live in a group) then
-- | the same three mode families.
borrowGroups :: Array Select.OptionGroup
borrowGroups =
  [ { label: "Clear borrowing", options: [ { value: "off", label: "— none —" } ] } ] <> modeGroups

modeOf :: String -> Mode
modeOf v = maybe Ionian _.mode (find (\m -> m.value == v) modeChoices)

currentModeValue :: Mode -> String
currentModeValue mode = maybe "Ionian" _.value (find (\m -> m.mode == mode) modeChoices)

-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

cn :: String -> HH.ClassName
cn = HH.ClassName

-- Triggerfish idiom (Hainbach × Rams): a warm-paper canvas under darker-beige
-- floating panels with brass borders and soft shadows.
canvasBg :: String
canvasBg = "#f6f3ea"

panelCss :: String
panelCss = "background: #e8e1cf; border: 1px solid #cdbb96; border-radius: 8px; box-shadow: 0 2px 14px #0000002a;"

-- | The view SVG fills its layer (the canvas); the fixed 880×600 viewBox still
-- | drives coordinates (scaled to fit by the browser), pan/zoom on top.
surfaceFillCss :: String
surfaceFillCss = "max-width: none; touch-action: none; width: 100%; height: 100%; display: block;"

-- | The whole instrument: a near-fullscreen view canvas with the Setup/Tank/Lens
-- | accordion floating top-left and the Progression/Library/Voices rail floating
-- | top-right — both darker-beige cards sitting over the paper canvas.
render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div
    -- Pushed down by one nav-height (`--tf-bar`) so the stage clears the AUDITION
    -- bar now docked under the shell nav; the old bottom voice bar is gone, so the
    -- stage fills to the window bottom (freed lower strip → future MIDI-flow chyron).
    [ HP.style ("position: relative; margin-top: var(--tf-bar); width: 100%; height: calc(100vh - 88px); min-height: 620px; overflow: hidden; border-radius: 8px; background: " <> canvasBg <> ";") ]
    [ HH.div [ HP.style "position: absolute; inset: 0;" ] [ surface st ]
    -- Three FLOATING controls (docs/DESIGN-vetula-progression-building.md), each
    -- owning one Harmonia layer: A = harmonic context (Key + palette + geometry),
    -- B = tank & progression (the Phrase), C = voices (Voicing). Placeholder names
    -- A/B/C; placement provisional — they float over the stage and will be made
    -- movable later. Library panel retired (→ between-sessions modal, #16); the
    -- "grow" and "pad grid" lenses left the geometry selector (grow → B; pad grid
    -- retired).
    , floatCard "context" st.foldCtx (ToggleFold VCtx)
        "position: absolute; top: 12px; left: 12px; width: 248px; max-height: calc(100% - 24px); overflow-y: auto; overflow-x: visible; z-index: 6;"
        [ setupPane st
        , subGroup "Lens" (lensBar st)
        ]
    , floatCard "tank & progression" st.foldProg (ToggleFold VProg)
        "position: absolute; top: 12px; right: 12px; width: 340px; max-height: calc(100% - 264px); overflow-y: auto; overflow-x: hidden; z-index: 6;"
        [ stackBar st
        , subGroup ("Tank · " <> show (length st.tank) <> " caught") (tankPane st)
        , arrangeBar st
        , growBar st
        , subGroup ("Progression · " <> countLabel (length (pathSteps st)) "step") (progressionPanel st)
        ]
    -- The AUDITION bar (chyron) now docks under the shell nav (top). The old bottom
    -- voice bar (four mini-notation lanes) was removed — the Perform surface
    -- supersedes it — and the freed bottom is reserved for a future MIDI-flow chyron.
    , chyronBar st
    , HH.div
        [ HP.style "position: absolute; bottom: 44px; left: 50%; transform: translateX(-50%); z-index: 5;" ]
        [ pickBar st ]
    , helpOverlay st
    , revoiceModal st
    ]

-- | The CHYRON — a thin ticker pinned just above the voice bar that logs every
-- | audition this session (see DESIGN-vetula-chyron-redesign.md). Oldest→newest,
-- | newest pinned at the right; older events clip off the left as the row fills
-- | (one notch per new chord). Phase 1 is read-only — a running trace of what you
-- | played; Phases 2–3 add span-selection, lift-to-progression, and timing verbs.
chyronBar :: forall m. State -> H.ComponentHTML Action Slots m
chyronBar st =
  HH.div
    [ HP.style ( "position: fixed; top: var(--tf-bar); left: 0; right: 0; z-index: 39; box-sizing: border-box; "
        <> "display: flex; gap: 10px; align-items: center; padding: 3px 12px; min-height: 44px; overflow: hidden; "
        -- shift-click is a selection gesture here (extend the range), so kill the
        -- browser's own shift-click text selection across the bar. user-select
        -- inherits to the chips.
        <> "user-select: none; -webkit-user-select: none; "
        <> "font-family: Georgia, serif; background: linear-gradient(#efe9d8,#e7e0cb); "
        <> "border-bottom: 1px solid #0000000f; box-shadow: 0 1px 3px #0000000d;" ) ]
    [ HH.div
        [ HP.style "flex: 0 0 auto; display: flex; align-items: center; gap: 6px;" ]
        ( [ -- record-arm toggle: ● red = capturing, ○ = paused (still audible)
            HH.button
              [ HP.style ("border: none; background: none; cursor: pointer; padding: 0; font-size: 13px; line-height: 1; color: "
                           <> (if st.chyronArmed then "#c0392b" else "#b9ad8c") <> ";")
              , HP.title (if st.chyronArmed then "recording auditions — click to pause capture" else "capture paused (auditions still sound) — click to record")
              , HE.onClick \_ -> ToggleChyronArm ]
              [ HH.text (if st.chyronArmed then "●" else "○") ]
          , HH.span
              [ HP.style "font-size: 10px; letter-spacing: 0.16em; text-transform: uppercase; color: #8a7d5a;" ]
              [ HH.text "audition" ] ]
          -- ⏎ save appears only while a completed span is selected
          <> ( case st.chyronSel of
                 Just sel | sel.hi > sel.lo ->
                   [ HH.button
                       [ HP.style "border: 1px solid #b8860b; background: #fbf6ea; color: #7a5c00; font-size: 11px; line-height: 1; cursor: pointer; padding: 2px 6px; border-radius: 3px;"
                       , HP.title "save selection as a glyph token (⏎)"
                       , HE.onClick \_ -> SaveChyronSel ]
                       [ HH.text "⏎ save" ] ]
                 _ -> [] )
          <> ( if length st.chyron == 0 then []
               else [ HH.button
                        [ HP.style "border: 1px solid #d8ceb4; background: #faf7ee; color: #9a8d6a; font-size: 11px; line-height: 1; cursor: pointer; padding: 2px 5px; border-radius: 3px;"
                        , HP.title "clear the whole audition trace"
                        , HE.onClick \_ -> ClearChyron ]
                        [ HH.text "clear ✕" ] ]
             )
        )
    -- SAVED region: pinned 2-glyph tokens, left, natural width (they push the
    -- live region rightward as they accumulate — saving reclaims live space).
    , HH.div
        [ HP.style "flex: 0 0 auto; display: flex; align-items: center; gap: 7px;" ]
        (mapWithIndex savedToken st.chyronSaved)
    -- LIVE region: fills the rest; newest right, oldest clips left; shrinks as the
    -- saved region grows (min-width:0).
    , HH.div
        [ HP.style "flex: 1 1 auto; min-width: 0; overflow: hidden; display: flex; gap: 5px; align-items: center; justify-content: flex-end;" ]
        ( if length st.chyron == 0
            then [ HH.span [ HP.style "font-size: 11px; color: #b3a888; font-style: italic;" ] [ HH.text "play a chord anywhere — it lands here" ] ]
            else let off = max 0 (length st.chyron - 30)
                 in mapWithIndex (\j ev -> chyronChip (off + j) ev) (takeEnd 30 st.chyron)
        )
    ]
  where
  -- a SAVED sequence: its 2-glyph identity (FA icon pair — visually distinct from
  -- the live stave-glyphs, so "named unit" reads at a glance). Click the icons to
  -- replay it with timing; × deletes. Tooltip carries the chord names.
  savedToken i s =
    let held = st.perfHeld == Just i
    in HH.span
      [ HP.style ("position: relative; flex: 0 0 auto; display: inline-flex; align-items: center; gap: 3px; border: 1px solid "
                   <> (if held then "#b8860b" else "#cdbb8c")
                   <> "; background: " <> (if held then "#fbf1d6" else "#f6efdc")
                   <> "; box-shadow: " <> (if held then "0 0 0 2px #f1e2b4" else "none")
                   <> "; border-radius: 4px; padding: 3px 6px; line-height: 1;")
      , HP.draggable true
      , HE.onDragStart \_ -> PerfPickup i
      , HP.title ("saved · " <> joinWith " " (map _.label s.events) <> " · click plays · shift-click / drag → a Perform box") ]
      [ HH.span
          [ HP.style "display: inline-flex; align-items: center; gap: 3px; cursor: pointer;"
          , HE.onClick \e -> if ME.shiftKey e then PerfPickup i else PlaySaved i ]
          [ faIcon s.glyph.first, faIcon s.glyph.second ]
      , HH.button
          [ HP.style "position: absolute; top: -5px; right: -3px; z-index: 2; border: 1px solid #cdbb8c; background: #f6efdc; color: #b06a5a; font-size: 10px; line-height: 1; cursor: pointer; padding: 0 3px; border-radius: 8px;"
          , HP.title "delete this saved sequence"
          , HE.onClick \_ -> DeleteSaved i ]
          [ HH.text "×" ]
      ]
  -- one chip = the chord's mini stave-glyph (same as the Tank), name-free. Hover
  -- + space auditions it; click selects this one chord; shift-click extends the
  -- range from the anchor. In-span chips wear a warm wash; the endpoints a gold rim.
  chyronChip i ev =
    let inSel = case st.chyronSel of
                  Just sel -> i >= sel.lo && i <= sel.hi
                  Nothing -> false
        isEnd = case st.chyronSel of
                  Just sel -> i == sel.lo || i == sel.hi
                  Nothing -> false
        hov = st.hoveredChyron == Just i
        bg = if inSel then "#efe6c8" else "#faf7ee"
        brd = if isEnd then "#b8860b" else if inSel then "#cdbb8c" else "#d8ceb4"
        pcNames = joinWith " " (map noteName (sort ev.pcs))
        -- a delete × surfaces on hover (its own element, NOT the select target)
        delX = if hov
          then [ HH.button
                   [ HP.style "position: absolute; top: -1px; right: -1px; z-index: 2; border: none; background: #faf7ee; color: #b06a5a; font-size: 11px; line-height: 1; cursor: pointer; padding: 0 2px; border-radius: 6px;"
                   , HP.title "delete this audition"
                   , HE.onClick \_ -> DeleteChyron i ]
                   [ HH.text "×" ] ]
          else []
    in HH.span
        [ HP.style ("position: relative; flex: 0 0 auto; white-space: nowrap; border: 1px solid " <> brd
                     <> "; background: " <> bg <> "; border-radius: 3px; padding: 0 1px; cursor: pointer; line-height: 0;")
        , HP.title (ev.label <> (if pcNames == "" then "" else " · " <> pcNames))
        , HE.onMouseEnter \_ -> HoverChyron (Just i)
        , HE.onMouseLeave \_ -> HoverChyron Nothing ]
        ( delX <>
          [ SE.svg
              [ SA.viewBox (-18.0) (-22.0) 36.0 44.0, SA.width 30.0, SA.height 38.0
              , HE.onClick \e -> ChyronClick i (ME.shiftKey e) ]
              (chordGlyph [] 0.0 0.0 ev.notes) ]
        )

-- | The Tonnetz-stack HUD in the tank card (2026-07-31): shown only while a stack
-- | is accumulating. Names the picked triads and the resulting polychord's pitch
-- | classes, and offers to catch the whole stack to the tank as one Anchor, or
-- | clear it. (Renders nothing when the stack is empty.)
stackBar :: forall m. State -> H.ComponentHTML Action Slots m
stackBar st =
  let stack = st.tonnetzStack in
  if length stack == 0 then HH.text ""
  else
    let names = joinWith " + " (map (\e -> noteName e.root <> (if e.major then "" else "m")) stack)
        pcs = joinWith " " (map noteName (sort (nub (concatMap _.pcs stack))))
        btn bg fg brd act lbl =
          HH.button
            [ HP.style ("border: 1px solid " <> brd <> "; background: " <> bg <> "; color: " <> fg
                         <> "; cursor: pointer; padding: 4px 10px; border-radius: 4px; font-size: 12px;")
            , HE.onClick \_ -> act ]
            [ HH.text lbl ]
    in HH.div
         [ HP.style "border: 1px solid #cbb8e0; background: #f6f1fb; border-radius: 6px; padding: 8px 10px; margin: 0 0 8px; display: flex; flex-direction: column; gap: 6px;" ]
         [ HH.div [ HP.style "font-size: 10px; color: #7a5c9a; letter-spacing: 0.1em; text-transform: uppercase;" ]
             [ HH.text ("Tonnetz stack · " <> countLabel (length stack) "triad") ]
         , HH.div [ HP.style "font-size: 13px; color: #4a3a5a;" ] [ HH.text names ]
         , HH.div [ HP.style "font-size: 11px; color: #8a7a9a; letter-spacing: 0.04em;" ] [ HH.text pcs ]
         , HH.div [ HP.style "display: flex; gap: 6px;" ]
             [ btn "#6a4a9a" "#ffffff" "#6a4a9a" CommitStack "catch as anchor"
             , btn "#faf7fd" "#7a5c9a" "#d8c8ea" ClearStack "clear"
             ]
         ]

-- | A floating control card: a clickable title bar (the concern name), then the
-- | panel body — which collapses to just the bar when `collapsed`, ceding the
-- | stage to the music viz underneath. Positioning is passed in (provisional —
-- | these will be made draggable once the placement settles).
floatCard :: forall m. String -> Boolean -> Action -> String -> Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
floatCard title collapsed toggle posCss body =
  HH.div
    [ HP.style (posCss <> " padding: 2px 12px " <> (if collapsed then "4px" else "10px") <> "; " <> panelCss) ]
    ( [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 8px; padding: 6px 2px 2px; cursor: pointer; user-select: none;"
          , HP.title (if collapsed then "expand" else "collapse")
          , HE.onClick \_ -> toggle ]
          [ HH.span [ HP.style "font-size: 9px; color: #b0b0b0; width: 9px;" ] [ HH.text (if collapsed then "▸" else "▾") ]
          , HH.span [ HP.style "font-size: 11px; font-weight: 700; color: #1a1a1a; letter-spacing: 0.12em; text-transform: uppercase;" ] [ HH.text title ]
          ]
      ] <> (if collapsed then [] else body) )

-- | What the cadence dial's `n` means, in Roman numerals — the tonicizing
-- | turnaround `Vetula.Between` lays in front of the dropped chord.
cadenceName :: Int -> String
cadenceName = case _ of
  0 -> "bare"
  1 -> "V"
  2 -> "ii–V"
  3 -> "vi–ii–V"
  _ -> "iii–vi–ii–V"

-- | The ARRANGE row (control B): the bridge between gather and compose. A hint
-- | (shift-click a caught chord to drop it in) and the CADENCE dial — how many
-- | passing chords `Vetula.Between` lays in front of each dropped chord.
arrangeBar :: forall m. State -> H.ComponentHTML Action Slots m
arrangeBar st =
  HH.div
    [ HP.style "border-top: 1px solid #d8ceb4; margin-top: 8px; padding-top: 6px; display: flex; flex-direction: column; gap: 6px;" ]
    [ HH.div [ HP.style "font-size: 10px; color: #b0b0b0; letter-spacing: 0.12em; text-transform: uppercase; margin: 0 2px;" ] [ HH.text "Arrange" ]
    , HH.div [ HP.style "font-size: 11px; color: #a0a0a0; margin: 0 2px;" ] [ HH.text "shift-click a caught chord to drop it into the progression, bridged." ]
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; margin: 0 2px;" ]
        [ HH.span [ HP.style "font-size: 11px; color: #7a7a7a;" ] [ HH.text "cadence" ]
        , stepBtn "−" (SetBridgeLen (st.bridgeLen - 1)) (st.bridgeLen <= 0)
        , HH.span [ HP.style "font-size: 12px; color: #1a1a1a; min-width: 12px; text-align: center;" ] [ HH.text (show st.bridgeLen) ]
        , stepBtn "+" (SetBridgeLen (st.bridgeLen + 1)) (st.bridgeLen >= maxBridge)
        , HH.span [ HP.style "font-size: 12px; color: #7a5c00; font-variant: small-caps;" ] [ HH.text (cadenceName st.bridgeLen) ]
        ]
    ]
  where
  stepBtn glyph act disabled =
    HH.button
      [ HP.style ("border: 1px solid #dcdcdc; background: #fafafa; border-radius: 4px; width: 22px; height: 22px; font-size: 13px; line-height: 1; "
                   <> if disabled then "color: #d8d8d8; cursor: default;" else "color: #6a6a6a; cursor: pointer;")
      , HP.disabled disabled
      , HE.onClick \_ -> act ]
      [ HH.text glyph ]

-- | Grow lives with the tank now (it operates on CAUGHT chords, not on the
-- | geometry). A single toggle: enter the grow surface, re-roll it, or leave.
growBar :: forall m. State -> H.ComponentHTML Action Slots m
growBar st =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 6px; padding: 8px 2px; border-top: 1px solid #d8ceb4;" ]
    ( if st.lens == LensGenerate then
        [ HH.button
            [ HP.style "border: 1px solid #b8860b; background: #fbf6ea; color: #7a5c00; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px;"
            , HP.title "re-roll the relatives around each tank seed"
            , HE.onClick \_ -> ShakeGenerate ]
            [ HH.text "shake ⟳" ]
        , HH.button
            [ HP.style "border: 1px solid #dcdcdc; background: #fafafa; color: #6a6a6a; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px;"
            , HP.title "leave the explore surface"
            , HE.onClick \_ -> SetLens LensTonnetz ]
            [ HH.text "done" ]
        ]
      else
        [ HH.button
            [ HP.style "border: 1px solid #b8860b; background: #fbf6ea; color: #7a5c00; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px;"
            , HP.title "bloom voice-led relatives around the caught chords"
            , HE.onClick \_ -> SetLens LensGenerate ]
            [ HH.text "explore ⟳" ]
        ]
    )

-- | A flat labelled group inside a floating control — a small uppercase caption
-- | over a divider, then the body. Replaces the old collapsible accordion box:
-- | the controls float, they don't fold.
subGroup :: forall m. String -> H.ComponentHTML Action Slots m -> H.ComponentHTML Action Slots m
subGroup label body =
  HH.div
    [ HP.style "border-top: 1px solid #d8ceb4; margin-top: 8px; padding-top: 6px;" ]
    [ HH.div [ HP.style "font-size: 10px; color: #b0b0b0; letter-spacing: 0.12em; text-transform: uppercase; margin: 0 2px 6px;" ] [ HH.text label ]
    , body
    ]

-- | "N steps" / "N step" for the control captions.
countLabel :: Int -> String -> String
countLabel n noun = show n <> " " <> noun <> (if n == 1 then "" else "s")

-- | The Stage GEOMETRY selector. The two "lenses" that weren't geometries have
-- | left: `grow` is a tank operation (it lives in control B now) and `pad grid`
-- | is retired. What remains are the four ways of LAYING OUT chords — two families
-- | (relational: tonnetz / lattices; root-picker: keyboard / fifths). Switching
-- | re-projects the SAME material (the sim keeps running underneath).
-- | The geometric views the CONTEXT card offers. Down to three after the
-- | 2026-07-31 redesign (see docs/DESIGN-vetula-progression-building.md): the
-- | keyboard was a root-picker subset of the (now interactive) circle of
-- | fifths, so it retired. Circle of fifths · Tonnetz · voice-leading lattice.
geometryLenses :: Array StageLens
geometryLenses = [ LensCircleFifths, LensTonnetz, LensLattices ]

lensBar :: forall m. State -> H.ComponentHTML Action Slots m
lensBar st =
  HH.div
    [ HP.style "display: flex; align-items: center; flex-wrap: wrap; gap: 6px; margin: 0 0 4px;" ]
    ( map lensChip geometryLenses <> [ lensChip LensPerform ] <> resetChip )
  where
  -- a way back to the fitted view (scroll to zoom · drag to pan), once it's moved.
  moved = st.viewZoom /= 1.0 || st.viewCx /= 0.0 || st.viewCy /= 0.0
  resetChip =
    if moved then
      [ HH.button
          [ HP.style "border: 1px solid #dcdcdc; background: #fafafa; color: #6a6a6a; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px; margin-left: 8px;"
          , HP.title "reset the view · scroll to zoom · drag to pan"
          , HE.onClick \_ -> ResetView
          ]
          [ HH.text "reset view" ]
      ]
    else []
  lensChip l =
    let active = st.lens == l
    in HH.button
        [ HP.style ("border: 1px solid " <> (if active then "#1a1a1a" else "#dcdcdc")
                     <> "; background: " <> (if active then "#1a1a1a" else "#fafafa")
                     <> "; color: " <> (if active then "#ffffff" else "#6a6a6a")
                     <> "; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px; white-space: nowrap;")
        , HE.onClick \_ -> SetLens l ]
        [ HH.text (lensLabel l) ]

-- | The collapsed pool (Perform mode): a thin clickable spine that expands the lattice
-- | again — the always-available "back to hunt" gesture.
poolSpine :: forall m. H.ComponentHTML Action Slots m
poolSpine =
  HH.div
    [ HP.style "flex: 0 0 42px; align-self: stretch; min-height: 460px; border: 1px solid #ededed; border-radius: 6px; background: #fafafa; cursor: pointer; display: flex; flex-direction: column; align-items: center; padding: 12px 0; gap: 12px;"
    , HP.title "expand the lattice — hunt for chords"
    , HE.onClick \_ -> SetFocus Hunt ]
    [ HH.span [ HP.style "font-size: 15px; color: #7a7a7a;" ] [ HH.text "▸" ]
    , HH.span [ HP.style "writing-mode: vertical-rl; font-size: 11px; letter-spacing: 0.14em; text-transform: uppercase; color: #b0b0b0;" ] [ HH.text "pool" ]
    ]

-- | The Setup pane — the reclaimed top bar, stacked vertically in the left
-- | accordion: key, scale, the focused-family scale override, the borrow source,
-- | the palette populators, and the rig connection + help. (The old `Vetula` title
-- | is gone — the Triggerfish top nav already names the instrument.)
setupPane :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
setupPane st =
  HH.div
    [ HP.style "display: flex; flex-direction: column; gap: 11px;" ]
    ( [ field "KEY"
          [ HH.slot (Proxy :: _ "keySelect") unit Select.component
              ((Select.defaultInput keyOptions) { selected = Just (show st.key.tonic), placeholder = "Key" })
              \(Select.Selected v) -> SelectKey v ]
      , field "SCALE"
          [ HH.slot (Proxy :: _ "scaleSelect") unit Select.component
              ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue st.key.mode), searchable = true })
              \(Select.Selected v) -> SelectScale v ]
      ]
        <> familyField
        <> [ field "PALETTES"
               [ HH.div [ HP.style "display: flex; flex-wrap: wrap; gap: 4px;" ] (map layerChip allColorLayers) ]
           ]
        <> borrowField
        <> [ connectionRow ]
    )
  where
  -- the borrow-scale picker only appears when the BORROWED color layer is
  -- engaged — it is that layer's source, meaningless otherwise (AC, 2026-07-31).
  borrowField =
    if Set.member LayerBorrowed st.colorLayers then
      [ field "BORROW"
          [ HH.slot (Proxy :: _ "borrowSelect") unit Select.component
              ((Select.cascadingInput borrowGroups) { selected = Just (fromMaybe "off" st.borrowMode), searchable = true })
              \(Select.Selected v) -> BorrowFrom v ]
      ]
    else []
  labelStyle = "font-size: 10px; color: #9a9a9a; letter-spacing: 0.1em; text-transform: uppercase;"
  field lbl controls =
    HH.div [ HP.style "display: flex; flex-direction: column; gap: 4px;" ]
      ([ HH.span [ HP.style labelStyle ] [ HH.text lbl ] ] <> controls)
  -- the palette chips are now SHOW/HIDE toggles for the color-overlay layers
  -- (2026-07-31 redesign), not pool-injecting mode buttons. Each carries a
  -- swatch in the layer's own hue; active = the swatch fills + hue-tinted chip.
  layerChip l =
    let on = Set.member l st.colorLayers
        hue = layerHue l
    in HH.button
         [ HP.style ("display: inline-flex; align-items: center; gap: 6px; border: 1px solid "
                      <> (if on then hue else "#dcdcdc")
                      <> "; background: " <> (if on then "#fafafa" else "#fafafa")
                      <> "; color: " <> (if on then hue else "#9a9a9a")
                      <> "; cursor: pointer; padding: 3px 9px; border-radius: 4px; font-size: 12px;")
         , HP.title (if on then "hide the " <> layerLabel l <> " layer" else "show the " <> layerLabel l <> " layer")
         , HE.onClick \_ -> ToggleLayer l ]
         [ HH.span
             [ HP.style ("width: 9px; height: 9px; border-radius: 2px; border: 1px solid " <> hue
                          <> "; background: " <> (if on then hue else "transparent") <> ";") ]
             []
         , HH.text (layerLabel l) ]
  -- a contextual scale picker for the focused family (click a keyboard key to
  -- focus one) — this is what lets two families hold different modes at once.
  familyField = case st.focusedFamily >>= (\sid -> find (\c -> c.id == sid) st.chords) of
    Just seed ->
      let famMode = (fromMaybe st.key (Map.lookup seed.id st.familyScale)).mode
      in [ field ("FAMILY " <> noteName seed.root)
             [ HH.slot (Proxy :: _ "familyScaleSelect") unit Select.component
                 ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue famMode), searchable = true })
                 \(Select.Selected v) -> ReflavourFamily v ] ]
    Nothing -> []
  -- rig connection (IAC) + the help modal trigger.
  connectionRow =
    HH.div [ HP.style "display: flex; align-items: center; justify-content: space-between; gap: 8px; margin-top: 2px;" ]
      [ midiChip st.midiName
      , HH.button
          [ HP.style helpBtnStyle, HP.title "keys & help", HE.onClick \_ -> ToggleHelp ]
          [ HH.text "ⓘ" ]
      ]
  -- clickable: a click is the user gesture Chrome needs to actually show the
  -- Web-MIDI permission prompt (the page-load request stays silent), so clicking
  -- the chip (re)connects. Green = connected, amber = click to enable/retry.
  midiChip nm =
    let ok = nm /= "…" && nm /= "" && nm /= "no Web-MIDI" && nm /= "muted"
    in HH.button
         [ HP.style "display: inline-flex; align-items: center; gap: 5px; font-size: 11px; color: #8a8a8a; background: #f4f4f4; border: 1px solid #e8e8e8; border-radius: 10px; padding: 2px 9px; cursor: pointer;"
         , HP.title (if ok then "Web-MIDI connected — click to reconnect" else "click to enable Web-MIDI (grant the permission prompt)")
         , HE.onClick \_ -> RetryMidi ]
         [ HH.span [ HP.style ("width: 7px; height: 7px; border-radius: 50%; background: " <> (if ok then "#5aa86a" else "#c9a23a") <> ";") ] []
         , HH.text nm ]
  helpBtnStyle = "border: 1px solid #e0e0e0; background: #fafafa; color: #7a7a7a; cursor: pointer; width: 22px; height: 22px; border-radius: 50%; font-size: 12px; line-height: 1; padding: 0;"

-- | The Tank pane — the durable, unordered collection of caught chords, as a
-- | wrapping grid of specimen tiles in the left accordion. `k` over a surface
-- | chord catches it; × deletes; click stages a seed, shift-click sequences it.
-- | The accordion header carries the "N caught" count, so this is just the
-- | capo/clear toolbar + tiles (the gesture legend lives behind the ⓘ help).
tankPane :: forall m. State -> H.ComponentHTML Action Slots m
tankPane st =
  HH.div
    [ HP.style "-webkit-user-select: none; user-select: none;" ]
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 8px; font-size: 11px; color: #b0b0b0; min-height: 16px;" ]
        ( (if Map.isEmpty st.seedChord then []
                else [ HH.button
                         [ HP.style "border: none; background: none; color: #9a7a2a; font-size: 11px; cursor: pointer; padding: 0;"
                         , HP.title "unstage every seeded specimen"
                         , HE.onClick \_ -> ClearStage ]
                         [ HH.text ("clear stage (" <> show (Map.size st.seedChord) <> ")") ] ])
          <> (if length st.tank == 0 then []
                else [ HH.span [ HP.style "margin-left: auto;" ] [ HH.text "capo" ]
                     , capoBtn (-1) "♭" "whole tank down a semitone"
                     , capoBtn 1 "♯" "whole tank up a semitone" ])
        )
    , if length st.tank == 0
        then HH.div [ HP.style "font-size: 12px; color: #c4c4c4; padding: 4px 0;" ]
               [ HH.text "empty — hover a chord and press k to catch it" ]
        else HH.div [ HP.style "display: flex; flex-wrap: wrap; gap: 8px;" ]
               (map (\s -> specimenTile (Map.member s.id st.seedChord) s) st.tank)
    ]

-- | One tank specimen: a small treble-staff thumbnail of its voicing (reusing the
-- | cloud's `chordGlyph`), its label, and a × delete. Plain-click STAGES it as a
-- | seed (bloom around it in the pool); shift-click ARRANGES it into the
-- | progression — bridged by the cadence dial (`ArrangeSpec`). A staged tile wears
-- | a gold frame so the pool ↔ tank link reads at a glance. (Staged-ness and
-- | arranged-ness are orthogonal.)
specimenTile :: forall m. Boolean -> Specimen -> H.ComponentHTML Action Slots m
specimenTile staged s =
  HH.div
    [ HP.style ("position: relative; width: 66px; padding: 6px 6px 4px; border-radius: 6px; display: flex; flex-direction: column; align-items: center; "
                 <> if staged then "border: 1px solid #c9a23a; background: #fbf3df;"
                              else "border: 1px solid #eee; background: #fbfbfa;")
    , HE.onMouseEnter \_ -> HoverSpec (Just s.id)
    , HE.onMouseLeave \_ -> HoverSpec Nothing ]
    [ HH.button
        [ HP.style "position: absolute; top: 1px; right: 3px; border: none; background: none; color: #c4c4c4; font-size: 13px; line-height: 1; cursor: pointer; padding: 0;"
        , HP.title "remove from tank"
        , HE.onClick \_ -> DeleteSpec s.id ]
        [ HH.text "×" ]
    , SE.svg
        [ SA.viewBox (-18.0) (-22.0) 36.0 44.0, SA.width 52.0, SA.height 46.0
        , HP.style "cursor: pointer;"
        , HE.onClick \e -> if ME.shiftKey e then ArrangeSpec s.id else StageSpec s.id ]
        (chordGlyph [] 0.0 0.0 s.voicing)
    , HH.div [ HP.style "font-size: 10px; color: #6a6a6a; margin-top: 2px; max-width: 60px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;" ]
        [ HH.text s.label ]
    , HH.div [ HP.style "display: flex; gap: 10px; margin-top: 1px;" ]
        [ transposeBtn s.id (-1) "♭" "down a semitone"
        , transposeBtn s.id 1 "♯" "up a semitone" ]
    ]

-- | A small ♭/♯ button that transposes ONE tank specimen in place.
transposeBtn :: forall m. SpecimenId -> Int -> String -> String -> H.ComponentHTML Action Slots m
transposeBtn sid n glyph tip =
  HH.button
    [ HP.style "border: none; background: none; color: #b0b0b0; font-size: 12px; line-height: 1; cursor: pointer; padding: 0 1px;"
    , HP.title tip
    , HE.onClick \_ -> TransposeSpec sid n ]
    [ HH.text glyph ]

-- | A small ♭/♯ button that capos the WHOLE tank.
capoBtn :: forall m. Int -> String -> String -> H.ComponentHTML Action Slots m
capoBtn n glyph tip =
  HH.button
    [ HP.style "border: 1px solid #dcdcdc; background: #fafafa; color: #6a6a6a; font-size: 11px; line-height: 1; cursor: pointer; padding: 2px 6px; border-radius: 3px;"
    , HP.title tip
    , HE.onClick \_ -> CapoTank n ]
    [ HH.text glyph ]

-- | Slice 4c: the Hunt/Perform width-focus toggle — a segmented control that replaces
-- | the old Lab/Performance tabs. Auto-flips on the path emptiness edge; this is the
-- | manual override.
focusTab :: forall m. State -> Focus -> String -> H.ComponentHTML Action Slots m
focusTab st f label =
  HH.button
    [ HP.style (btnStyle (st.focus == f)), HE.onClick \_ -> SetFocus f ]
    [ HH.text label ]
  where
  btnStyle isActive =
    "border: none; background: none; cursor: pointer; padding: 4px 10px; font-size: 13px; "
      <> if isActive then "color: #1a1a1a; border-bottom: 2px solid #1a1a1a; font-weight: 600;"
                     else "color: #9a9a9a; border-bottom: 2px solid transparent;"

helpText :: String
helpText =
  "One triad per scale degree. Click a piano key to focus that root — a beam lights its column. Stack notes on the focused chord: number keys 2–7 add an interval that many steps up (3 = a third, so 3·3·3 climbs a seventh; 2·4 makes a sus2), e adds the next third (e·e = seventh), s drops the suspensions; press l to explode its whole lattice at once (l again to collapse). The McMullen button drops a curated signpost palette and BORROW pulls chromatic chords from a parallel mode (modal interchange) — chords the scale-pure lattice can't reach, floating up over their own roots and shaded warmer the further outside the chosen scale they sit. Hover any chord + space to hear it. Hover a chord and press v to REVOICE it — a modal with its pitch ladder (drag a note by octaves, ⌥ to double), Tab to cycle voicings, ↑↓ to nudge a voice, f to keep one, and a slash row to re-foot the bass; Esc closes. Click any chord — triads included — to grow the progression on the right: same family bridges by the shortest single-note walk (gold); a chord in another family leaps across as an interconnector (dashed violet). Chromatic keys summon borrowed roots (modulation). On the right: click a step to hear it (shift-click one or two to offer chords to add), then Tab / Shift-Tab cycles its voicings, ↑/↓ nudges a clicked voice, drag a note to move it by octaves (⌥-drag to double it); ▶ plays the whole thing, c clears it. The Tidal source tracks it live — copy to save, paste + Load to work on a saved one again. “save → library” stores it in the progression library for later recall."

-- | The keys-and-help overlay (the ⓘ button). The reference text that used to sit as a
-- | paragraph under the canvas, moved off it. Static, so click-anywhere dismisses.
helpOverlay :: forall m. State -> H.ComponentHTML Action Slots m
helpOverlay st =
  if not st.helpOpen then HH.text ""
  else HH.div
    [ HP.style "position: fixed; inset: 0; background: rgba(20,20,20,0.28); z-index: 50; display: flex; align-items: flex-start; justify-content: center; padding: 56px 20px;"
    , HE.onClick \_ -> ToggleHelp ]
    [ HH.div
        [ HP.style "background: #fff; max-width: 720px; max-height: 80vh; overflow-y: auto; border-radius: 8px; box-shadow: 0 10px 44px rgba(0,0,0,0.18); padding: 20px 26px 26px;" ]
        [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 10px; margin: 0 0 14px;" ]
            [ HH.h2 [ HP.style "font-size: 15px; font-weight: 600; margin: 0; color: #2a2a2a;" ] [ HH.text "Keys & help" ]
            , HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ] [ HH.text "click anywhere to close" ]
            ]
        , helpSection "Lattice" helpText
        ]
    ]
  where
  helpSection heading body =
    HH.div [ HP.style "margin: 0 0 14px;" ]
      [ HH.div [ HP.style "font-size: 11px; letter-spacing: 0.06em; text-transform: uppercase; color: #9a7a2a; margin: 0 0 5px;" ] [ HH.text heading ]
      , HH.p [ HP.style "font-size: 12.5px; line-height: 1.65; color: #555; margin: 0;" ] [ HH.text body ]
      ]

-- | The Stage frame: the pick-mode cloud always wins; otherwise the active lens
-- | renders. Adding a lens is one more branch here + one `allLenses` entry.
surface :: forall m. State -> H.ComponentHTML Action Slots m
surface st
  | length st.genSel > 0 && length st.candidates > 0 = pickSurface st
  | otherwise = case st.lens of
      LensKeyboard -> keyboardSurface st
      LensPadGrid -> padGridSurface st
      LensCircleFifths -> circleFifthsSurface st
      LensTonnetz -> tonnetzSurface st
      LensLattices -> latticesSurface st
      LensGenerate -> generativeSurface st
      LensPerform -> performSurface st

-- | The PERFORM surface — a row of player BOXES, one per output. Shift-click (or
-- | drag) a saved token in the chyron to pick it up, then click (or drop it onto)
-- | a box: the box loops that token's chords on its MIDI channel while the
-- | transport plays. This is the first slice of the Perform view (DESIGN §Perform);
-- | function stacks and non-MIDI sinks come later.
-- | Mint a fresh Perform session: a random seed → a monochrome glyph-triple alias,
-- | scene counter at 1. Random (not content-derived) — a session is a container.
mintSession :: Effect Store.SessionState
mintSession = do
  seed <- randomInt 0 999999
  pure { alias: sessionAliasOf seed, name: "", nextScene: 1 }

-- | Mint a `SavedSeq` back from a source's chords (the inverse of `boxSpec`'s
-- | `map _.notes s.events`): one synthesised event per chord, carrying the notes
-- | (pcs/label/at are cosmetic for Perform playback, which reads only `.notes`),
-- | and a content glyph over the chords so the reconstructed token still has an
-- | identity. Used by `boxesFromDoc` to recall a saved scene onto the surface.
mkSavedSeq :: Array (Array Int) -> SavedSeq
mkSavedSeq chords =
  { events: mapWithIndex evt chords
  , glyph: glyphOf (joinWith " " (map (joinWith "," <<< map show) chords))
  }
  where
  -- `Free`: a recalled note-list carries no scale reading (the source grammar
  -- stores notes only). Re-deriving an anchor from pcs+key is the §7 open
  -- decision; until then a recalled chord explores as an unlocated pitch-bag.
  evt i notes = { pcs: map (\n -> mod n 12) notes, notes, label: show (i + 1), at: toNumber i, anchor: Free }

-- | Reconstruct the live Perform boxes from a parsed scene document (the inverse
-- | of `map boxSpec perfBoxes` at save): one box per voice, on its channel, with
-- | its source's chords minted back into a `SavedSeq`; a sourceless voice (or an
-- | unknown source name) gets an empty box. This is what `PerfLoadScene` applies.
boxesFromDoc :: PerfDoc -> Array PerfBox
boxesFromDoc doc = map voiceToBox doc.voices
  where
  chordsOf name = maybe [] _.chords (find (\s -> s.name == name) doc.sources)
  voiceToBox v =
    let cs = maybe [] chordsOf v.source
    in { channel: v.channel
       , label: "P" <> show v.channel
       , seq: if length cs == 0 then Nothing else Just (mkSavedSeq cs)
       , stack: v.stack
       , seqText: v.seqText
       , muted: v.muted
       , term: v.term
       }

-- | A live Perform box → the neutral `VoiceSpec` the Lepidoptera serialiser takes
-- | (its chords are the token's event notes; empty seq = a sourceless voice).
boxSpec :: PerfBox -> VoiceSpec
boxSpec box =
  { channel: box.channel
  , chords: maybe [] (\s -> map _.notes s.events) box.seq
  , seqText: box.seqText
  , stack: box.stack
  , term: box.term
  , muted: box.muted
  }

-- | The persistent SESSION identity next to the save button: the session's
-- | monochrome glyph-TRIPLE (three black FontAwesome icons — deliberately unlike a
-- | chord token's coloured PAIR) reconstructed from its persisted alias, plus the
-- | alias/name and the next scene number a save will mint.
sessionChip :: forall m. Store.SessionState -> H.ComponentHTML Action Slots m
sessionChip sess =
  HH.span
    [ HP.style "display: inline-flex; align-items: center; gap: 7px; padding: 3px 11px; border: 1px solid #d8cfa8; border-radius: 5px; background: #faf7ee;"
    , HP.title "this session's identity — every scene you save is tagged with it; ↻ new session rolls it" ]
    [ HH.span
        [ HP.style "display: inline-flex; align-items: center; gap: 4px;" ]
        (if sess.alias == "" then [ HH.text "…" ]
         else map (\name -> faIcon { icon: name, color: "#2a2a2a" }) (split (Pattern "-") sess.alias))
    , HH.span
        [ HP.style "font-size: 11px; color: #6a5a2a; letter-spacing: 0.03em;" ]
        [ HH.text (if sess.name == "" then sess.alias else sess.name) ]
    ]

-- | The recall modal — saved scenes fetched from Amphora, grouped by SESSION (each
-- | group headed by its monochrome triple). Click a scene to parse its payload and
-- | reconstruct the surface (`PerfLoadScene`). Empty / offline → a gentle note.
perfRecallModal :: forall m. State -> H.ComponentHTML Action Slots m
perfRecallModal st =
  if not st.perfRecallOpen then HH.text ""
  else
    HH.div
      [ HP.style "position: fixed; inset: 0; background: rgba(20,20,20,0.32); z-index: 60; display: flex; align-items: center; justify-content: center; padding: 40px;"
      , HE.onClick \_ -> PerfCloseRecall ]
      [ HH.div
          [ HP.style "background: #fbfaf4; width: 520px; max-width: 92vw; max-height: 84vh; overflow-y: auto; border-radius: 10px; box-shadow: 0 12px 48px rgba(0,0,0,0.24); padding: 22px 26px 24px;"
          , HE.onClick \e -> PerfStopClick e PerfNop ]
          [ HH.div [ HP.style "display: flex; align-items: baseline; justify-content: space-between; margin: 0 0 14px;" ]
              [ HH.h2 [ HP.style "font-size: 15px; font-weight: 600; margin: 0; color: #2a2a2a;" ] [ HH.text "Recall scene" ]
              , HH.button
                  [ HP.style "border: none; background: transparent; color: #9a9a9a; font-size: 18px; cursor: pointer; line-height: 1;"
                  , HP.title "close", HE.onClick \_ -> PerfCloseRecall ]
                  [ HH.text "×" ]
              ]
          , if length st.perfScenes == 0
              then HH.div [ HP.style "font-size: 12px; color: #9a8a5a; padding: 8px 0 4px;" ]
                     [ HH.text "no scenes saved yet — save one from the surface (or the store is offline)." ]
              else HH.div_ (concatMap groupView (nub (map sessionTagOf st.perfScenes)))
          ]
      ]
  where
  sessionTagOf item = fromMaybe "?" (head (mapMaybe (stripPrefix (Pattern "session:")) item.tags))
  groupView alias =
    [ HH.div [ HP.style "display: flex; align-items: center; gap: 7px; margin: 12px 0 6px; padding-bottom: 5px; border-bottom: 1px solid #eee4cc;" ]
        ( map (\n -> faIcon { icon: n, color: "#2a2a2a" }) (split (Pattern "-") alias)
          <> [ HH.span [ HP.style "font-size: 11px; color: #8a7a4a; letter-spacing: 0.03em;" ] [ HH.text alias ] ] )
    ] <> map sceneRow (filter (\i -> sessionTagOf i == alias) st.perfScenes)
  sceneRow item =
    HH.button
      [ HP.style "display: block; width: 100%; text-align: left; border: 1px solid #e4dcc2; background: #fcfaf3; color: #4a4436; cursor: pointer; padding: 7px 12px; margin: 0 0 5px; border-radius: 5px; font-size: 13px; font-family: ui-monospace, monospace;"
      , HP.title "load this scene onto the Perform surface"
      , HE.onClick \_ -> PerfLoadScene item.payload ]
      [ HH.text item.name ]

performSurface :: forall m. State -> H.ComponentHTML Action Slots m
performSurface st =
  HH.div
    [ HP.style "position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 26px; padding: 40px;" ]
    [ HH.div
        [ HP.style "font-size: 12px; letter-spacing: 0.14em; text-transform: uppercase; color: #9a7a2a; text-align: center; max-width: 520px; line-height: 1.6;" ]
        [ HH.text $ case st.perfHeldFx of
            Just fx -> "layer in hand (" <> fxLabel fx <> ") — click a player to add it to its stack"
            Nothing -> case st.perfHeld of
              Just _ -> "token in hand — click a player to drop it"
              Nothing ->
                if any (\b -> isJust b.seq) st.perfBoxes
                  then "press PLAY to loop the players · click an FX below then a player to stack it"
                  else "shift-click (or drag) a saved token below onto a player — it loops while the transport plays"
        ]
    , HH.div
        [ HP.style "display: flex; align-items: center; gap: 12px; min-height: 26px; flex-wrap: wrap; justify-content: center;" ]
        [ sessionChip st.perfSession
        , HH.button
            [ HP.style ("border: 1px solid #cdbb8c; border-radius: 5px; padding: 5px 14px; font-size: 12px; letter-spacing: 0.06em; cursor: pointer; "
                         <> (if any (\b -> isJust b.seq) st.perfBoxes
                              then "background: #f3ead2; color: #6a5a2a;"
                              else "background: #f6f3ea; color: #c2b790; cursor: default;"))
            , HP.title "serialise this Perform surface as a vetulaScene and save it to Amphora"
            , HP.enabled (any (\b -> isJust b.seq) st.perfBoxes)
            , HE.onClick \_ -> SaveScene ]
            [ HH.text ("⬡ save scene #" <> show st.perfSession.nextScene) ]
        , HH.button
            [ HP.style "border: 1px solid #ddd3b4; border-radius: 5px; padding: 5px 11px; font-size: 11px; letter-spacing: 0.04em; cursor: pointer; background: transparent; color: #9a8a5a;"
            , HP.title "start a new session — a fresh glyph-triple and scene counter (reloads keep the current session; this is the deliberate new-body-of-work boundary)"
            , HE.onClick \_ -> PerfNewSession ]
            [ HH.text "↻ new session" ]
        , HH.button
            [ HP.style "border: 1px solid #cdbb8c; border-radius: 5px; padding: 5px 12px; font-size: 12px; letter-spacing: 0.04em; cursor: pointer; background: #f6f1e3; color: #6a5a2a;"
            , HP.title "recall a saved scene onto the surface"
            , HE.onClick \_ -> PerfOpenRecall ]
            [ HH.text "↴ scenes" ]
        , case st.publishMsg of
            Just m -> HH.span [ HP.style "font-size: 11px; color: #7a6a3a; font-family: ui-monospace, monospace;" ] [ HH.text m ]
            Nothing -> HH.text ""
        ]
    , fxPalette st
    , HH.div
        [ HP.style "display: flex; gap: 18px; flex-wrap: wrap; justify-content: center; align-items: flex-start; max-width: 940px;" ]
        (mapWithIndex (perfBox st) st.perfBoxes)
    , perfEditModal st
    , perfRecallModal st
    ]

-- | The sequence-editor modal — a roomier surface for the text hatch than the
-- | inline field, with a mini-notation guide and clickable examples in place. Edits
-- | box `perfEditBox`'s `seqText` directly (same `PerfSetSeq` path, committed on
-- | blur). Examples drop straight into the field; the guide makes the notation
-- | learnable where you use it (the complexity-budget point).
perfEditModal :: forall m. State -> H.ComponentHTML Action Slots m
perfEditModal st = case st.perfEditBox >>= \i -> map (Tuple i) (index st.perfBoxes i) of
  Nothing -> HH.text ""
  Just (Tuple i box) ->
    HH.div
      [ HP.style "position: fixed; inset: 0; background: rgba(20,20,20,0.32); z-index: 60; display: flex; align-items: center; justify-content: center; padding: 40px;"
      , HE.onClick \_ -> PerfCloseEdit ]
      [ HH.div
          [ HP.style "background: #fbfaf4; width: 560px; max-width: 92vw; max-height: 84vh; overflow-y: auto; border-radius: 10px; box-shadow: 0 12px 48px rgba(0,0,0,0.24); padding: 22px 26px 24px;"
          , HE.onClick \e -> PerfStopClick e PerfNop ]
          [ HH.div [ HP.style "display: flex; align-items: baseline; justify-content: space-between; margin: 0 0 14px;" ]
              [ HH.h2 [ HP.style "font-size: 15px; font-weight: 600; margin: 0; color: #2a2a2a;" ]
                  [ HH.text ("Sequence · " <> box.label <> " · ch " <> show box.channel) ]
              , HH.button
                  [ HP.style "border: none; background: transparent; color: #9a9a9a; font-size: 18px; cursor: pointer; line-height: 1;"
                  , HP.title "close"
                  , HE.onClick \_ -> PerfCloseEdit ]
                  [ HH.text "×" ]
              ]
          , HH.input
              [ HP.style ("width: 100%; box-sizing: border-box; border: 1px solid "
                           <> (if boxUsesSeq box then "#b8860b" else "#cdbb8c")
                           <> "; background: #fff; color: #3a3a3a; border-radius: 6px; padding: 9px 12px; font-size: 15px; font-family: ui-monospace, monospace;")
              , HP.value (printPipeline box)
              , HP.attr (AttrName "placeholder") "0 1 2 3 # arp up 4"
              , HE.onValueChange \s -> PerfSetPipeline i s ]
          , HH.div [ HP.style "font-size: 11px; color: #9a8a5a; margin: 8px 0 16px;" ]
              [ HH.text "One cycle = one bar; numbers index the token's chords (out-of-range = rest). Type freely, click away to apply." ]
          , sectionLabel "Examples — click to use"
          , HH.div [ HP.style "display: flex; flex-wrap: wrap; gap: 6px; margin: 0 0 18px;" ]
              (map (exampleChip i) examples)
          , sectionLabel "Mini-notation (the sequence)"
          , HH.div [ HP.style "display: grid; grid-template-columns: auto 1fr; gap: 4px 14px; font-size: 12.5px; color: #555;" ]
              (concatMap guideRow guide)
          , HH.div [ HP.style "height: 14px;" ] []
          , sectionLabel "Layers (after each #)"
          , HH.div [ HP.style "display: grid; grid-template-columns: auto 1fr; gap: 4px 14px; font-size: 12.5px; color: #555;" ]
              (concatMap guideRow layerGuide)
          ]
      ]
  where
  sectionLabel t =
    HH.div [ HP.style "font-size: 10px; letter-spacing: 0.1em; text-transform: uppercase; color: #b0a684; margin: 0 0 7px;" ] [ HH.text t ]
  exampleChip i ex =
    HH.button
      [ HP.style "border: 1px solid #cdbb8c; background: #f3ead2; color: #6a5a2a; cursor: pointer; padding: 3px 9px; border-radius: 4px; font-size: 12px; font-family: ui-monospace, monospace;"
      , HP.title (snd ex)
      , HE.onClick \_ -> PerfSetSeq i (fst ex) ]
      [ HH.text (fst ex) ]
  guideRow (Tuple syntax meaning) =
    [ HH.code [ HP.style "font-family: ui-monospace, monospace; color: #7a5c00;" ] [ HH.text syntax ]
    , HH.span_ [ HH.text meaning ] ]
  examples =
    [ Tuple "0 1 2 3" "one chord per beat"
    , Tuple "0 ~ 2 ~" "beats 1 and 3 only (rests)"
    , Tuple "<0 2> 1" "alternate 0/2 each bar, then 1"
    , Tuple "0(3,8)" "euclidean — 3 hits over 8"
    , Tuple "[0 1] 2" "0 and 1 share a beat, then 2"
    , Tuple "0!3 1" "repeat chord 0 three times, then 1"
    , Tuple "0*2 1" "chord 0 twice as fast, then 1"
    ]
  guide =
    [ Tuple "0 1 2" "a sequence — one step each"
    , Tuple "~" "a rest"
    , Tuple "[a b]" "group into one step (subdivide)"
    , Tuple "<a b>" "alternate, one per cycle"
    , Tuple "a(k,n)" "euclidean rhythm — k hits in n"
    , Tuple "a!n" "repeat a, n times"
    , Tuple "a*n / a/n" "speed up / slow down"
    , Tuple "a?" "randomly drop (degrade)"
    ]
  layerGuide =
    [ Tuple "# transpose 5" "shift every chord ±semitones"
    , Tuple "# oct -1" "shift ±octaves"
    , Tuple "# rate 2" "loop faster (negative = slower)"
    , Tuple "# voice open" "re-voice: open/rootless/drop2/drop24/quartal/cluster"
    , Tuple "# top 1 · # bottom 1" "keep the top / bottom N voices"
    , Tuple "# arp up 4" "arpeggiate: up/down/updown, notes per beat"
    , Tuple "# strum 14" "strum — ms between notes"
    , Tuple "… every 4" "apply a layer only every N cycles"
    ]

-- | The FX palette: click a layer to pick it up, then click a player box to append
-- | it to that box's stack (drag comes in a later slice). The held chip lights gold.
fxPalette :: forall m. State -> H.ComponentHTML Action Slots m
fxPalette st =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 8px;" ]
    ( [ HH.span [ HP.style "font-size: 10px; letter-spacing: 0.12em; text-transform: uppercase; color: #b0a684;" ] [ HH.text "fx" ] ]
        <> map paletteChip [ Transpose 0, Octave (-1), Rate 2, Voice Open, Select (High 1), Select (Low 1), Arpg ArpUp 4, Strum 14 ]
    )
  where
  paletteChip fx =
    let held = st.perfHeldFx == Just fx
    in HH.button
         [ HP.style ("border: 1px solid " <> (if held then "#b8860b" else "#dcd2b4")
                      <> "; background: " <> (if held then "#fbf1d6" else "#faf6ea")
                      <> "; color: #6a5a2a; cursor: grab; padding: 3px 10px; border-radius: 4px; font-size: 11px; white-space: nowrap;")
         , HP.title "drag onto a player (or click, then click a player) to stack this layer"
         , HP.draggable true
         , HE.onDragStart \_ -> PerfDragStart (FromPalette fx)
         , HE.onDragEnd \_ -> PerfDragEnd
         , HE.onClick \_ -> PerfPickFx fx ]
         [ HH.text (fxLabel fx) ]

-- | One PERFORM player box: its channel label, the assigned token's 2-glyph
-- | identity (or a ＋ placeholder), and a × to empty it. A drop target for both
-- | the click-to-place gesture and HTML5 drag-drop; when a token is in hand every
-- | box lights as a receiver.
perfBox :: forall m. State -> Int -> PerfBox -> H.ComponentHTML Action Slots m
perfBox st i box =
  let held = isJust st.perfHeld || isJust st.perfHeldFx
      filled = isJust box.seq
      brd = if held then "#b8860b" else if filled then "#cdbb8c" else "#d8ceb4"
      bg = if held then "#fbf6ea" else "#faf7ee"
  in HH.div
       [ HP.style ("position: relative; width: 208px; min-height: 118px; border: 2px "
                    <> (if held then "dashed " else "solid ") <> brd
                    <> "; background: " <> bg
                    <> "; border-radius: 10px; display: flex; flex-direction: column; align-items: center; justify-content: flex-start; gap: 8px; padding: 10px 10px; cursor: pointer;"
                    <> (if box.muted || ghost then " opacity: 0.5;" else ""))
       , HP.title (if held then "drop the held token/layer here" else box.label <> " · MIDI ch " <> show box.channel)
       , HE.onClick \_ -> PerfDropBox i
       , HE.onDragOver PerfDragOver
       , HE.onDrop \_ -> PerfDropBox i
       ]
       ( [ HH.div [ HP.style "display: flex; align-items: center; gap: 8px;" ]
             [ HH.span [ HP.style "font-size: 10px; letter-spacing: 0.1em; text-transform: uppercase; color: #b0a684;" ]
                 [ HH.text (box.label <> " · ch " <> show box.channel) ]
             , HH.button
                 [ HP.style ("border: 1px solid " <> (if box.muted then "#c8a24a" else "#dcd2b4")
                              <> "; background: " <> (if box.muted then "#f3e6c4" else "#faf6ea")
                              <> "; color: " <> (if box.muted then "#9a6a1a" else "#8a7a4a")
                              <> "; cursor: pointer; padding: 1px 8px; border-radius: 3px; font-size: 9px; letter-spacing: 0.06em; text-transform: uppercase;")
                 , HP.title (if box.muted then "muted — click to play" else "playing — click to mute")
                 , HE.onClick \e -> PerfStopClick e (PerfToggleMute i) ]
                 [ HH.text (if box.muted then "muted" else "on") ]
             ]
         , case box.seq of
             Just s ->
               HH.div [ HP.style "display: flex; align-items: center; gap: 6px; font-size: 22px; color: #7a5c00; margin: 2px 0;" ]
                 [ faIcon s.glyph.first, faIcon s.glyph.second ]
             Nothing ->
               HH.div [ HP.style "font-size: 28px; color: #d8ceb4; line-height: 1; margin: 2px 0;" ] [ HH.text "＋" ]
         ]
         <> [ seqRow ]
         <> stackRows
         -- the terminal SINK — a midi · odo · rig pill row (the fold's cap)
         <> [ HH.div [ HP.style "display: inline-flex; border: 1px solid #dcd2b4; border-radius: 3px; overflow: hidden; margin-top: 2px;" ]
                (map termBtn [ TMidi, TOdo, TRig ])
            , if ghost
                then HH.div [ HP.style "font-size: 9px; letter-spacing: 0.06em; text-transform: uppercase; color: #a05a3a;" ]
                       [ HH.text "✕ rig only" ]
                else HH.text ""
            , if filled
                then HH.button
                       [ HP.style "position: absolute; top: 4px; right: 7px; border: none; background: transparent; color: #b06a5a; font-size: 15px; line-height: 1; cursor: pointer;"
                       , HP.title "clear this player"
                       , HE.onClick \e -> PerfStopClick e (PerfClearBox i) ]
                       [ HH.text "×" ]
                else HH.text "" ]
       )
  where
  ghost = boxGhosted st.authority box
  -- the TEXT HATCH: a mini-notation sequence over the token's chord indices (cycle
  -- = one bar). Empty = default one-chord-per-beat. Border lights when it's driving.
  seqRow =
    HH.div
      [ HP.style "display: flex; align-items: center; width: 100%; gap: 3px;" ]
      [ HH.input
          [ HP.style ("flex: 1 1 auto; min-width: 0; box-sizing: border-box; border: 1px solid "
                       <> (if boxUsesSeq box then "#b8860b" else "#dcd2b4")
                       <> "; background: #fbfaf4; color: #6a5a2a; border-radius: 4px; padding: 2px 6px; font-size: 11px; font-family: ui-monospace, monospace;")
          , HP.value (printPipeline box)
          , HP.attr (AttrName "placeholder") "0 1 2 3 # arp up 4"
          , HP.title "the box pipeline as text (two views of one thing — edit here or the chips below): a mini-notation sequence (cycle = 1 bar) then # layers, e.g. 0 1 2 3 # voice open # arp up 4"
          -- commit on CHANGE (blur / enter), not on every keystroke: binding the live
          -- value back via `HP.value` each input snaps the caret to the end and blocks
          -- editing. `onValueChange` leaves the field uncontrolled while you type, then
          -- commits — so you can freely edit the pipeline and hear it on blur. The value
          -- is DERIVED (`printPipeline`), so chip edits re-render it and text edits parse
          -- back into the structured box — the round-trip's single reconciliation point.
          , HE.onValueChange \s -> PerfSetPipeline i s
          -- stop a focus-click bubbling to the box's drop handler, without a re-render.
          , HE.onClick \e -> PerfStopClick e PerfNop ]
      , HH.button
          [ HP.style "flex: 0 0 auto; border: 1px solid #dcd2b4; background: #faf6ea; color: #8a7a4a; cursor: pointer; padding: 1px 6px; border-radius: 4px; font-size: 12px; line-height: 1.2;"
          , HP.title "open the sequence editor — notation guide + examples"
          , HE.onClick \e -> PerfStopClick e (PerfOpenEdit i) ]
          [ HH.text "⤢" ] ]
  -- the box's function stack, one FULL-WIDTH row per layer: name · (alt control) ·
  -- − / + to nudge · × to remove. Rows are draggable to reorder or move between
  -- boxes. First row = applied first (innermost); arp/strum realise at the sink.
  stackRows = mapWithIndex fxRow box.stack
  fxRow fxIx lyr =
    HH.div
      [ HP.style "display: flex; align-items: center; width: 100%; box-sizing: border-box; gap: 3px; border: 1px solid #cdbb8c; background: #f3ead2; border-radius: 4px; padding: 2px 5px; font-size: 11px; color: #6a5a2a; cursor: grab;"
      , HP.draggable true
      , HP.title "drag to reorder, or onto another player to move it"
      , HE.onDragStart \_ -> PerfDragStart (FromBox i fxIx)
      , HE.onDragEnd \_ -> PerfDragEnd
      , HE.onDragOver PerfDragOver
      , HE.onDrop \e -> PerfDropOnChip e i fxIx ]
      ( [ HH.span [ HP.style "flex: 1 1 auto; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;" ] [ HH.text (fxLabel lyr.fx) ]
        , whenBtn fxIx lyr.when ]
          <> altBtns lyr.fx fxIx
          <> [ nudge fxIx (-1) "−"
             , nudge fxIx 1 "+"
             , HH.button
                 [ HP.style "border: none; background: transparent; color: #b06a5a; font-size: 12px; line-height: 1; cursor: pointer; padding: 0 2px;"
                 , HP.title "remove this layer"
                 , HE.onClick \e -> PerfStopClick e (PerfFxRemove i fxIx) ]
                 [ HH.text "×" ]
             ]
      )
  -- the layer's WHEN clause — a pill showing ∀ (every cycle) or eN (every n cycles);
  -- click to cycle. Faint when Always, gold when conditional (so an active clause reads).
  whenBtn fxIx w =
    let on = w /= Always
    in HH.button
         [ HP.style ("border: 1px solid " <> (if on then "#b8860b" else "#dcd2b4")
                      <> "; background: " <> (if on then "#fbf1d6" else "#faf6ea")
                      <> "; color: " <> (if on then "#7a5c00" else "#b0a684")
                      <> "; font-size: 10px; line-height: 1.2; cursor: pointer; padding: 0 5px; border-radius: 3px;")
         , HP.title "when this layer applies — click to cycle every-n"
         , HE.onClick \e -> PerfStopClick e (PerfFxWhen i fxIx) ]
         [ HH.text (whenLabel w) ]
  -- an extra per-layer control: arp gets a direction-cycle button; others none.
  altBtns fx fxIx = case fx of
    Arpg _ _ ->
      [ HH.button
          [ HP.style "border: 1px solid #cdbb8c; background: #faf6ea; color: #8a7a4a; font-size: 10px; line-height: 1.2; cursor: pointer; padding: 0 4px; border-radius: 3px;"
          , HP.title "cycle arp direction"
          , HE.onClick \e -> PerfStopClick e (PerfFxAlt i fxIx) ]
          [ HH.text "↻" ] ]
    _ -> []
  nudge fxIx d glyph =
    HH.button
      [ HP.style "border: 1px solid #cdbb8c; background: #faf6ea; color: #8a7a4a; font-size: 12px; line-height: 1.1; cursor: pointer; padding: 0 5px; border-radius: 3px;"
      , HP.title "nudge this layer's value"
      , HE.onClick \e -> PerfStopClick e (PerfFxNudge i fxIx d) ]
      [ HH.text glyph ]
  -- one segment of the midi · odo · rig terminal selector; the active sink filled,
  -- rig tinted when it would be ghosted (rig-only outside Atlantis).
  termBtn t =
    let active = box.term == t
        rigCol = t == TRig && ghost
    in HH.button
         [ HP.style ("border: none; cursor: pointer; padding: 1px 8px; font-size: 9px; letter-spacing: 0.04em; text-transform: uppercase; background: "
                      <> (if active then "#8a7a4a" else "#faf6ea")
                      <> "; color: " <> (if active then "#ffffff" else if rigCol then "#a05a3a" else "#8a7a4a") <> ";")
         , HP.title ("sink " <> termLabel t)
         , HE.onClick \e -> PerfStopClick e (PerfSetTerm i t) ]
         [ HH.text (termShort t) ]

-- | The Keyboard lens — the exhaustive hunting cloud: the piano keyboard, diatonic
-- | triad families, seed-blooms, the voice-leading lattice, and the path overlay.
keyboardSurface :: forall m. State -> H.ComponentHTML Action Slots m
keyboardSurface st =
  let scl = scaleSet st.key
      posMap = Map.fromFoldable (map (\n -> Tuple n.id { x: n.x, y: n.y }) st.nodes)
      links = latticeLinkLines posMap st.chords
      -- chord id → 1-based step in the running sequence (the node step-number badge)
      pathOrder = Map.fromFoldable (mapWithIndex (\i pid -> Tuple pid (i + 1)) st.path)
      -- the focused root (from a keyboard-key click) lights a beam up its column
      focusRoot = st.focusedFamily >>= \fid -> map _.root (find (\c -> c.id == fid) st.chords)
      -- a faint divider marking the OUTSIDE shelf — only when outside chords exist
      shelfMarker =
        if any (\c -> c.outside > 0) st.chords
          then [ SE.line [ SA.x1 (-436.0), SA.y1 (-80.0), SA.x2 440.0, SA.y2 (-80.0), SA.class_ (cn "shelf-line") ]
               , SE.text [ SA.x (-430.0), SA.y (-86.0), SA.class_ (cn "shelf-label") ] [ HH.text "OUTSIDE THE SCALE ↑" ]
               ]
          else []
      vb = geoView st
  in SE.svg
      ( [ SA.viewBox vb.x vb.y vb.w vb.h
        , SA.width 880.0
        , SA.height 600.0
        , SA.class_ (cn "vetula-surface")
        -- no left ladder on the Lab surface — let the cloud fill the wide window.
        , HP.style surfaceFillCss
        , HE.onWheel \we -> ZoomAt (WE.toEvent we) (WE.deltaY we)
        , HE.onMouseDown (PanStart <<< ME.toEvent)
        ]
          <> geoPanAttrs st
          -- also listen for moves while a ladder voice is being dragged (mutually
          -- exclusive with a pan — the keyboard never starts a ladder drag itself)
          <> (case st.drag of
                Just _ ->
                  [ HE.onMouseMove (DragMove <<< ME.toEvent)
                  , HE.onMouseUp \_ -> DragEnd
                  , HE.onMouseLeave \_ -> DragEnd
                  ]
                Nothing -> [])
      )
      -- revoiceModal is no longer drawn into this SVG — it's a DOM-level modal
      -- (shared Modal widget), rendered at the top of `render`.
      [ cloudClipDef
      , clippedCloud
          ( focusBeam focusRoot <> keyboardView scl <> axisLabels <> shelfMarker <> links
              <> map (nodeView scl pathOrder Set.empty posMap)
                   (filter (\c -> not (Set.member c.id st.imported)) st.chords) )
      ]

-- | The PadGrid lens — a sparse, playable 4×4 board of the tank. The mouse-driven
-- | precursor to the MidiFighter/Push idea: click a pad to PLAY it (audition, no
-- | commitment), shift-click to stage it as a seed. Empty cells are faint holders.
-- | Sixteen cells, row-major over the tank; a tank beyond 16 is browsed in the
-- | strip above (recipes + paging come later — Slice F).
padGridSurface :: forall m. State -> H.ComponentHTML Action Slots m
padGridSurface st =
  -- fill the canvas and settle the board centred at the bottom, clear of the
  -- floating side panels (it's HTML, not a viewBox-scaled SVG like the other lenses)
  HH.div
    [ HP.style "width: 100%; height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: flex-end; padding-bottom: 24px; -webkit-user-select: none; user-select: none;" ]
    [ HH.div
        [ HP.style "width: 560px; max-width: 90%;" ]
        [ HH.div [ HP.style "font-size: 11px; color: #9a9488; margin: 0 0 8px; letter-spacing: 0.04em; text-align: center;" ]
            [ HH.text "click a pad to play it · shift-click to seed the stage — nothing is committed" ]
        , HH.div
            [ HP.style "display: grid; grid-template-columns: repeat(4, 1fr); gap: 10px;" ]
            (map (\i -> padCell (index st.tank i)) (range 0 15))
        ]
    ]

-- | One pad on the board: a filled pad (glyph + label, playable) or a faint empty
-- | holder. Same gesture split as the tank strip, but PLAY is the default here —
-- | a pad's job is to sound, not to stage.
padCell :: forall m. Maybe Specimen -> H.ComponentHTML Action Slots m
padCell = case _ of
  Nothing ->
    HH.div
      [ HP.style "aspect-ratio: 1 / 1; border: 1px dashed #ececec; border-radius: 8px; background: #fcfcfc;" ]
      []
  Just s ->
    HH.div
      [ HP.style "aspect-ratio: 1 / 1; border: 1px solid #e2e2e2; border-radius: 8px; background: #fbfbfa; cursor: pointer; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 2px;"
      , HE.onClick \e -> if ME.shiftKey e then StageSpec s.id else AuditionSpec s.id ]
      [ SE.svg
          [ SA.viewBox (-20.0) (-24.0) 40.0 48.0, SA.width 66.0, SA.height 62.0 ]
          (chordGlyph [] 0.0 0.0 s.voicing)
      , HH.div [ HP.style "font-size: 11px; color: #6a6a6a; max-width: 88%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;" ]
          [ HH.text s.label ]
      ]

-- ---------------------------------------------------------------------------
-- The Circle-of-Fifths lens (first geometric view)
-- ---------------------------------------------------------------------------

-- | Position of a pitch class on the circle of fifths (0 = C, 1 = G, 2 = D, …).
-- | Seven is its own inverse mod 12, so the map is its own round-trip.
cofIndex :: Int -> Int
cofIndex pc = mod (pc * 7) 12

-- | Radial geometry for the fifths wheel, in the surface's centred coordinates.
-- | The wheel of root names sits at the HUB; chords bead outward along each
-- | spoke from `baseR`, so a spoke has unlimited room to grow away from centre.
cofWheel :: { hubR :: Number, baseR :: Number, dr :: Number, spokeLen :: Number }
cofWheel = { hubR: 64.0, baseR: 122.0, dr: 56.0, spokeLen: 320.0 }

-- | Angle (radians) of a pitch class on the wheel, with the active tonic rotated
-- | to the top (12 o'clock) and the dominant direction clockwise. Diatonic roots
-- | then fall as a contiguous run from the subdominant (one step anticlockwise)
-- | clockwise through the sharp side.
cofAngle :: Int -> Int -> Number
cofAngle tonic pc =
  (-Number.pi / 2.0) + toNumber (mod (cofIndex pc - cofIndex tonic) 12) * (Number.pi / 6.0)

-- | The geometric viewport as a viewBox: the base window (−440,−300,880,600)
-- | scaled by `viewZoom` about the pan centre (`viewCx`,`viewCy`).
geoView :: State -> { x :: Number, y :: Number, w :: Number, h :: Number }
geoView st =
  let hw = 440.0 / st.viewZoom
      hh = 300.0 / st.viewZoom
  in { x: st.viewCx - hw, y: st.viewCy - hh, w: 2.0 * hw, h: 2.0 * hh }

-- | The mouse-move / up / leave handlers a geometric surface adds *only while a
-- | pan drag is live* — mirrors the ladder-drag pattern (no per-move action churn
-- | when idle).
geoPanAttrs
  :: forall r
   . State
  -> Array (HP.IProp (onMouseMove :: ME.MouseEvent, onMouseUp :: ME.MouseEvent, onMouseLeave :: ME.MouseEvent | r) Action)
geoPanAttrs st = case st.panning of
  Just _ ->
    [ HE.onMouseMove (PanMove <<< ME.toEvent)
    , HE.onMouseUp \_ -> PanEnd
    , HE.onMouseLeave \_ -> PanEnd
    ]
  Nothing -> []

-- | The Circle-of-Fifths lens — the same pool chords as the Keyboard lens, but
-- | laid out by root around a wheel of fifths instead of over the piano. The root
-- | names sit at the HUB; each root owns a spoke, and chords sharing a root bead
-- | OUTWARD along it from the centre — so every spoke has unlimited room to grow.
-- | The active key's diatonic roots light up as a contiguous wedge (geometry ==
-- | grade), so the friendly diatonic spokes cluster near the top and the
-- | borrowings fan out to the sides and around the back. Rendering reuses
-- | `nodeView` (same glyph, same audition/catch gestures, same path badges) via a
-- | computed position map.
circleFifthsSurface :: forall m. State -> H.ComponentHTML Action Slots m
circleFifthsSurface st =
  let scl = scaleSet st.key
      tonic = st.key.tonic
      shown = filter (\c -> not (Set.member c.id st.imported)) st.chords
      pathOrder = Map.fromFoldable (mapWithIndex (\i pid -> Tuple pid (i + 1)) st.path)
      -- group the pool by root pitch class, so same-root chords share a spoke
      rootsPresent = nub (map (\c -> mod c.root 12) shown)
      posFor c =
        let pc = mod c.root 12
            sameRoot = filter (\d -> mod d.root 12 == pc) shown
            k = fromMaybe 0 (elemIndex c.id (map _.id sameRoot))
            rad = cofWheel.baseR + toNumber k * cofWheel.dr
            ang = cofAngle tonic pc
        in Tuple c.id { x: rad * Number.cos ang, y: rad * Number.sin ang }
      posMap = Map.fromFoldable (map posFor shown)
      vb = geoView st
  in SE.svg
      ( [ SA.viewBox vb.x vb.y vb.w vb.h
        , SA.width 880.0
        , SA.height 600.0
        , SA.class_ (cn "vetula-surface")
        , HP.style surfaceFillCss
        , HE.onWheel \we -> ZoomAt (WE.toEvent we) (WE.deltaY we)
        , HE.onMouseDown (PanStart <<< ME.toEvent)
        ] <> geoPanAttrs st )
      ( cofBackdrop st.key tonic scl rootsPresent
          <> map (nodeView scl pathOrder Set.empty posMap) shown
          <> cofCorona st tonic scl
      )

-- | The active color layers' chords, DE-DUPLICATED by content: each unique chord
-- | appears once, carrying the list of layers that contain it (so a chord that is
-- | both McMullen and borrowed is one glyph with two source badges, not two
-- | overlapping tokens). Layer order follows `allColorLayers` (diatonic first).
mergedLayerChords :: State -> Array { chord :: ChordNode, layers :: Array ColorLayer }
mergedLayerChords st =
  let active = filter (\l -> Set.member l st.colorLayers) allColorLayers
      tagged = concatMap (\l -> map (\c -> { layer: l, chord: c }) (layerChords st l)) active
      keys = nub (map (\e -> contentKey e.chord) tagged)
      forKey k =
        let ms = filter (\e -> contentKey e.chord == k) tagged
        in map (\e -> { chord: e.chord, layers: nub (map _.layer ms) }) (head ms)
  in mapMaybe forKey keys

-- | A row of small badge pips, centred at (cx,cy), one per source layer in that
-- | layer's hue. This is the "why is this here" key — a chord that is both
-- | McMullen and borrowed wears two pips. The CONTEXT PALETTE swatches name each hue.
layerBadges :: forall m. Number -> Number -> Array ColorLayer -> Array (H.ComponentHTML Action Slots m)
layerBadges cx cy layers =
  let k = length layers
  in mapWithIndex
       (\j l ->
          SE.circle
            [ SA.cx (cx - toNumber (k - 1) * 3.5 + toNumber j * 7.0), SA.cy cy, SA.r 2.8
            , HP.style ("fill: " <> layerHue l <> "; stroke: #ffffff; stroke-width: 0.8; pointer-events: none;") ])
       layers

-- | The transparent click target over a color-overlay chord: the unified gesture
-- | (matching every other Vetula surface) — plain click auditions the chord,
-- | shift-click catches it into the tank. Hover space-previews it.
colorHit :: forall m. ChordNode -> Number -> Number -> Number -> H.ComponentHTML Action Slots m
colorHit chord cx cy r =
  SE.circle
    [ SA.cx cx, SA.cy cy, SA.r r
    , HP.style "fill: transparent; cursor: pointer;"
    , HE.onMouseEnter \_ -> HoverTriad (Just { root: chord.root, pcs: chord.pcs })
    , HE.onMouseLeave \_ -> HoverTriad Nothing
    , HE.onClick \e -> if ME.shiftKey e then CatchNode chord else AuditionNode chord
    ]

-- | One color-overlay chord drawn as notes-on-stave (the same `chordGlyph` the
-- | pool uses — the notation IS the chord's identity) on a soft backing disc,
-- | with a row of source badges above it, over a catch/audition hit target. Used
-- | on the circle of fifths, whose native chord glyph is the stave.
colorGlyphAt
  :: forall m
   . Array Int -> Number -> Number -> ChordNode -> Array ColorLayer
  -> Array (H.ComponentHTML Action Slots m)
colorGlyphAt scl cx cy chord layers =
  [ SE.circle
      [ SA.cx cx, SA.cy cy, SA.r 15.0
      , HP.style "fill: #fcfbf8; stroke: #e4e0d4; stroke-width: 1; pointer-events: none;" ]
  ]
    <> chordGlyph scl cx cy chord.voicing
    <> layerBadges cx (cy - 19.0) layers
    <> [ colorHit chord cx cy 15.0 ]

-- | The color-overlay corona on the circle of fifths (2026-07-31 redesign): the
-- | active layers' chords (de-duplicated, badged by source) painted as staff
-- | glyphs beyond the pool, each at its root's wheel angle. Same-root chords stack
-- | radially outward along the spoke. The CONTEXT card's PALETTE swatches map hue
-- | → set. Non-interactive for now — the unified catch gesture lands in Step 4.
cofCorona :: forall m. State -> Int -> Array Int -> Array (H.ComponentHTML Action Slots m)
cofCorona st tonic scl =
  let entries = mergedLayerChords st
      place j e =
        let pc = mod e.chord.root 12
            dupIx = length (filter (\d -> mod d.chord.root 12 == pc) (take j entries))
            ang = cofAngle tonic pc
            rad = 246.0 + toNumber dupIx * 34.0
            x = rad * Number.cos ang
            y = rad * Number.sin ang
        in colorGlyphAt scl x y e.chord e.layers
  in concat (mapWithIndex place entries)

-- | The wheel behind the chords: twelve spokes radiating OUT from the hub, and the
-- | twelve root names ringed tightly around the centre. Diatonic roots (in the
-- | active scale) are inked dark with a soft parchment disc; the rest are ghosted
-- | grey. The tonic wears a gold ring. The spokes run from the hub outward so the
-- | chords beaded along them read as belonging to their root.
cofBackdrop
  :: forall m
   . Key -> Int -> Array Int -> Array Int -> Array (H.ComponentHTML Action Slots m)
cofBackdrop key tonic scl rootsPresent =
  concatMap spoke (range 0 11) <> concatMap marker (range 0 11)
  where
  spoke i =
    let pc = mod (i * 7) 12   -- walk the wheel in fifths so i is the wheel slot
        diat = elem pc scl
        ang = cofAngle tonic pc
        x0 = cofWheel.hubR * Number.cos ang
        y0 = cofWheel.hubR * Number.sin ang
        x1 = cofWheel.spokeLen * Number.cos ang
        y1 = cofWheel.spokeLen * Number.sin ang
    in [ SE.line
           [ SA.x1 x0, SA.y1 y0, SA.x2 x1, SA.y2 y1
           , HP.style ("stroke: " <> (if diat then "#e2ddcb" else "#f2f2f2") <> "; stroke-width: 1;")
           ]
       ]
  marker i =
    let pc = mod (i * 7) 12
        diat = elem pc scl
        isTonic = pc == mod tonic 12
        present = elem pc rootsPresent
        ang = cofAngle tonic pc
        x = cofWheel.hubR * Number.cos ang
        y = cofWheel.hubR * Number.sin ang
        disc =
          if diat then
            [ SE.circle
                [ SA.cx x, SA.cy y, SA.r 12.0
                , HP.style ("fill: " <> (if present then "#f1ead6" else "#f7f3e8") <> "; stroke: none;")
                ]
            ]
          else []
        tonicRing =
          if isTonic then
            [ SE.circle
                [ SA.cx x, SA.cy y, SA.r 15.0
                , HP.style "fill: none; stroke: #b8860b; stroke-width: 1.5;"
                ]
            ]
          else []
        txtColor = if diat then "#2a2a2a" else "#c4c4c4"
        -- the diatonic triad rooted here — the wheel is now a catch surface:
        -- plain click auditions the root's triad, shift-click catches it (the
        -- same gesture as the Tonnetz). Every root gets a transparent hit disc,
        -- so out-of-scale roots (label-only, no parchment disc) click too.
        triadPcs = triadOn key pc
        isMajor = elem (mod (pc + 4) 12) triadPcs
        hit =
          [ SE.circle
              [ SA.cx x, SA.cy y, SA.r 14.0
              , HP.style "fill: transparent; cursor: pointer;"
              , HE.onMouseEnter \_ -> HoverTriad (Just { root: pc, pcs: triadPcs })
              , HE.onMouseLeave \_ -> HoverTriad Nothing
              , HE.onClick \e -> if ME.shiftKey e then CatchTriad pc triadPcs isMajor else AuditionTriad pc triadPcs
              ]
          ]
    in disc <> tonicRing <>
         [ SE.text
             [ SA.x x, SA.y (y + 4.0)
             , HP.attr (AttrName "text-anchor") "middle"
             , HP.style ("font-size: 12px; fill: " <> txtColor <> "; letter-spacing: 0.02em; pointer-events: none; -webkit-user-select: none; user-select: none;")
             ]
             [ HH.text (noteName pc) ]
         ] <> hit

-- ---------------------------------------------------------------------------
-- The Tonnetz lens (neo-Riemannian tonal net)
-- ---------------------------------------------------------------------------

-- | Lattice geometry. A node at grid (u,v) carries pitch class (7u + 4v) mod 12
-- | — u steps a perfect fifth, v a major third — so up-triangles are major triads
-- | and down-triangles minor. Drawn over a bounded window (a peek into the
-- | infinite net); roaming by pan/zoom comes with the shared geometric host.
tonnetz :: { s :: Number, rowH :: Number, uLo :: Int, uHi :: Int, vLo :: Int, vHi :: Int, nodeR :: Number }
tonnetz =
  { s: 78.0, rowH: 78.0 * 0.8660254, uLo: -5, uHi: 5, vLo: -3, vHi: 3, nodeR: 15.0 }

tonPc :: Int -> Int -> Int
tonPc u v = mod (7 * u + 4 * v) 12

-- | Screen position of lattice node (u,v): fifths run horizontally, thirds up-and-
-- | to-the-right (each row up shears half a step right), origin centred.
tonPos :: Int -> Int -> { x :: Number, y :: Number }
tonPos u v =
  { x: (toNumber u + toNumber v * 0.5) * tonnetz.s
  , y: negate (toNumber v) * tonnetz.rowH
  }

type TonTri =
  { root :: Int
  , pcs :: Array Int
  , major :: Boolean
  , verts :: Array { x :: Number, y :: Number }
  }

-- | The triad in a lattice triangle. An up-triangle {(u,v),(u+1,v),(u,v+1)} is the
-- | major triad on its lower-left node; a down-triangle a minor triad a major-third
-- | above (its lowest-left corner (u+1,v) plus the two up neighbours).
tonTri :: Boolean -> Int -> Int -> TonTri
tonTri isUp u v =
  let b = tonPc u v
      verts =
        if isUp then [ tonPos u v, tonPos (u + 1) v, tonPos u (v + 1) ]
        else [ tonPos (u + 1) v, tonPos u (v + 1), tonPos (u + 1) (v + 1) ]
      pcs =
        if isUp then [ b, mod (b + 7) 12, mod (b + 4) 12 ]
        else [ mod (b + 7) 12, mod (b + 4) 12, mod (b + 11) 12 ]
  in { root: if isUp then b else mod (b + 4) 12
     , pcs: sort (nub pcs)
     , major: isUp
     , verts
     }

triLabel :: TonTri -> String
triLabel t = noteName t.root <> (if t.major then "" else "m")

ptsStr :: Array { x :: Number, y :: Number } -> String
ptsStr = joinWith " " <<< map (\p -> show p.x <> "," <> show p.y)

centroid :: Array { x :: Number, y :: Number } -> { x :: Number, y :: Number }
centroid ps =
  let n = max 1 (length ps)
  in { x: sum (map _.x ps) / toNumber n, y: sum (map _.y ps) / toNumber n }

-- | The Tonnetz lens — the tonal net as its OWN triad source (not a pool
-- | projection). Every triangle is a triad; edge-adjacent triangles share two
-- | tones (a P/L/R move). Diatonic triads of the active key (all three tones in
-- | scale) fill parchment and carry a name — the connected "spider". Click a
-- | triangle to audition it; shift-click to catch it into the tank.
tonnetzSurface :: forall m. State -> H.ComponentHTML Action Slots m
tonnetzSurface st =
  let scl = scaleSet st.key
      tonic = mod st.key.tonic 12
      cells = do
        u <- range tonnetz.uLo tonnetz.uHi
        v <- range tonnetz.vLo tonnetz.vHi
        pure (Tuple u v)
      triCells = do
        u <- range tonnetz.uLo (tonnetz.uHi - 1)
        v <- range tonnetz.vLo (tonnetz.vHi - 1)
        pure (Tuple u v)
      tris = map (\(Tuple u v) -> tonTri true u v) triCells
          <> map (\(Tuple u v) -> tonTri false u v) triCells
      diatonic t = all (\p -> elem p scl) t.pcs
      vb = geoView st
  in SE.svg
      ( [ SA.viewBox vb.x vb.y vb.w vb.h
        , SA.width 880.0
        , SA.height 600.0
        , SA.class_ (cn "vetula-surface")
        , HP.style surfaceFillCss
        , HE.onWheel \we -> ZoomAt (WE.toEvent we) (WE.deltaY we)
        , HE.onMouseDown (PanStart <<< ME.toEvent)
        ] <> geoPanAttrs st )
      ( concatMap (tonFill diatonic) tris
          <> concatMap (tonEdgesFrom scl) cells
          <> concatMap (tonNode scl tonic) cells
          <> concatMap (tonTriName diatonic) tris
          <> concatMap (tonStackMark st.tonnetzStack) tris
          <> map tonHit tris
      )

-- | A parchment fill for a diatonic triad (major warm, minor cool); nothing for a
-- | non-diatonic one (still clickable via its transparent hit polygon).
tonFill :: forall m. (TonTri -> Boolean) -> TonTri -> Array (H.ComponentHTML Action Slots m)
tonFill diatonic t =
  if diatonic t then
    [ SE.element (ElemName "polygon")
        [ HP.attr (AttrName "points") (ptsStr t.verts)
        , HP.style ((if t.major then "fill: #f1ead6;" else "fill: #ebeee6;") <> " stroke: none; pointer-events: none;")
        ]
        []
    ]
  else []

-- | The three lattice edges leading out of a node (fifth, major third, minor
-- | third); each drawn once, stronger when both endpoints are in the scale.
tonEdgesFrom :: forall m. Array Int -> Tuple Int Int -> Array (H.ComponentHTML Action Slots m)
tonEdgesFrom scl (Tuple u v) =
  let p = tonPos u v
      inU w = w >= tonnetz.uLo && w <= tonnetz.uHi
      inV w = w >= tonnetz.vLo && w <= tonnetz.vHi
      mk du dv =
        if inU (u + du) && inV (v + dv) then
          let q = tonPos (u + du) (v + dv)
              diat = elem (tonPc u v) scl && elem (tonPc (u + du) (v + dv)) scl
          in [ SE.line
                 [ SA.x1 p.x, SA.y1 p.y, SA.x2 q.x, SA.y2 q.y
                 , HP.style ("stroke: " <> (if diat then "#ddd6c2" else "#eeeeee") <> "; stroke-width: 1; pointer-events: none;")
                 ]
             ]
        else []
  in mk 1 0 <> mk 0 1 <> mk 1 (-1)

-- | A lattice node: a small white disc with the note name, inked when in-scale and
-- | ghosted otherwise; the tonic wears a gold ring. Non-interactive (the triangles
-- | take the clicks).
tonNode :: forall m. Array Int -> Int -> Tuple Int Int -> Array (H.ComponentHTML Action Slots m)
tonNode scl tonic (Tuple u v) =
  let pc = tonPc u v
      p = tonPos u v
      inScale = elem pc scl
      isTonic = pc == tonic
      ring =
        if isTonic then
          [ SE.circle
              [ SA.cx p.x, SA.cy p.y, SA.r (tonnetz.nodeR + 3.0)
              , HP.style "fill: none; stroke: #b8860b; stroke-width: 1.5; pointer-events: none;"
              ]
          ]
        else []
  in [ SE.circle
         [ SA.cx p.x, SA.cy p.y, SA.r tonnetz.nodeR
         , HP.style ("fill: #ffffff; stroke: " <> (if inScale then "#d8d2be" else "#ededed") <> "; stroke-width: 1; pointer-events: none;")
         ]
     ]
       <> ring
       <>
         [ SE.text
             [ SA.x p.x, SA.y (p.y + 4.0)
             , HP.attr (AttrName "text-anchor") "middle"
             , HP.style ("font-size: 13px; fill: " <> (if inScale then "#2a2a2a" else "#cfcfcf") <> "; pointer-events: none; -webkit-user-select: none; user-select: none;")
             ]
             [ HH.text (noteName pc) ]
         ]

-- | The chord name at the centroid of a diatonic triangle — makes the spider read
-- | as named chords rather than bare geometry.
tonTriName :: forall m. (TonTri -> Boolean) -> TonTri -> Array (H.ComponentHTML Action Slots m)
tonTriName diatonic t =
  if diatonic t then
    let c = centroid t.verts
    in [ SE.text
           [ SA.x c.x, SA.y (c.y + 4.0)
           , HP.attr (AttrName "text-anchor") "middle"
           , HP.style "font-size: 11px; fill: #7a6a3a; pointer-events: none; -webkit-user-select: none; user-select: none;"
           ]
           [ HH.text (triLabel t) ]
       ]
  else []

-- | The transparent click target over a triangle: plain click auditions the triad,
-- | shift-click catches it into the tank, alt-click adds it to the Tonnetz stack
-- | (2026-07-31 — the freeform triad-stacking gesture).
tonHit :: forall m. TonTri -> H.ComponentHTML Action Slots m
tonHit t =
  SE.element (ElemName "polygon")
    [ HP.attr (AttrName "points") (ptsStr t.verts)
    , HP.style "fill: transparent; cursor: pointer;"
    , HE.onMouseEnter \_ -> HoverTriad (Just { root: t.root, pcs: t.pcs })
    , HE.onMouseLeave \_ -> HoverTriad Nothing
    , HE.onClick \e ->
        if ME.altKey e then StackTriad t.root t.pcs t.major
        else if ME.shiftKey e then CatchTriad t.root t.pcs t.major
        else AuditionTriad t.root t.pcs
    ]
    []

-- | The stack highlight over a triangle that is currently in the Tonnetz stack:
-- | a violet wash + a numbered badge at its centroid showing its pick order.
tonStackMark
  :: forall m
   . Array { root :: Int, pcs :: Array Int, major :: Boolean }
  -> TonTri -> Array (H.ComponentHTML Action Slots m)
tonStackMark stack t =
  case findIndex (\e -> e.root == t.root && e.pcs == t.pcs) stack of
    Nothing -> []
    Just i ->
      let c = centroid t.verts
      in [ SE.element (ElemName "polygon")
             [ HP.attr (AttrName "points") (ptsStr t.verts)
             , HP.style "fill: rgba(106,74,154,0.20); stroke: #6a4a9a; stroke-width: 2; pointer-events: none;" ]
             []
         , SE.circle
             [ SA.cx c.x, SA.cy (c.y - 15.0), SA.r 7.0
             , HP.style "fill: #6a4a9a; stroke: #ffffff; stroke-width: 1; pointer-events: none;" ]
         , SE.text
             [ SA.x c.x, SA.y (c.y - 15.0 + 3.0)
             , HP.attr (AttrName "text-anchor") "middle"
             , HP.style "font-size: 9px; fill: #ffffff; pointer-events: none; -webkit-user-select: none; user-select: none;" ]
             [ HH.text (show (i + 1)) ]
         ]

-- ---------------------------------------------------------------------------
-- The Lattices lens (every degree's tertian powerset web, tiled + zoomable)
-- ---------------------------------------------------------------------------

-- | How high the tertian stack climbs (0 = triad tones … 4 = the 13th). The full
-- | web is 63 chords per degree; keep it full and lean on zoom to roam it.
latticeCap :: Int
latticeCap = 4

-- | Layout constants for the tiled lattices, in the surface's centred space. Seven
-- | degree-bands span the width; the whole thing fits at zoom 1, zoom to read.
latticeLeft :: Number
latticeLeft = -360.0

-- | `bandW` is the per-degree horizontal slot; `packW` is the narrower width the
-- | glyphs actually pack into, so the difference is a gutter between the degree
-- | lanes (keeps the same total span → still fits at zoom 1).
latGeo :: { bandW :: Number, packW :: Number, cellH :: Number, levelGap :: Number, baseY :: Number, glyphR :: Number }
latGeo = { bandW: 120.0, packW: 88.0, cellH: 25.0, levelGap: 9.0, baseY: 250.0, glyphR: 9.0 }

latPerRow :: Int
latPerRow = 4

-- | A placed lattice member: its chord and screen centre, plus a synthetic id
-- | (degree ×1000 + index) so the Hasse edges only join siblings of one degree.
type LatMember = { id :: Int, chord :: ChordNode, cx :: Number, cy :: Number }

-- | Lay out one degree's capped lattice as a compact cluster: levels stack upward
-- | (triad at the base, extensions climbing), each level's members wrapped into
-- | rows of `latPerRow`, centred in the degree's band.
degreeCluster :: Key -> Int -> ChordNode -> Array LatMember
degreeCluster key i seed =
  let fam = filter (\f -> f.level <= latticeCap) (latticeFamily key seed)
      cellW = latGeo.packW / toNumber latPerRow
      degX = latticeLeft + toNumber i * latGeo.bandW
      go level y acc =
        if level > latticeCap then acc
        else
          let members = map _.chord (filter (\f -> f.level == level) fam)
              n = length members
              rows = (n + latPerRow - 1) / latPerRow
              placed = mapWithIndex
                (\k c ->
                   { chord: c
                   , cx: degX - latGeo.packW / 2.0 + cellW * (toNumber (mod k latPerRow) + 0.5)
                   , cy: y - toNumber (k / latPerRow) * latGeo.cellH
                   })
                members
              blockH = toNumber rows * latGeo.cellH + latGeo.levelGap
          in go (level + 1) (y - blockH) (acc <> placed)
  in mapWithIndex (\j m -> { id: i * 1000 + j, chord: m.chord, cx: m.cx, cy: m.cy }) (go 0 latGeo.baseY [])

-- | The covering edges (Hasse diagram) within one degree's cluster: members that
-- | differ by exactly one note. Scoped per degree so it stays ~O(63²), not O(441²).
degreeEdges :: forall m. Array LatMember -> Array (H.ComponentHTML Action Slots m)
degreeEdges ms =
  let pairs = concat (mapWithIndex (\i a -> map (\b -> Tuple a b) (drop (i + 1) ms)) ms)
  in concatMap
       (\(Tuple a b) ->
          if pcSymDiff a.chord.pcs b.chord.pcs == 1 then
            [ SE.line [ SA.x1 a.cx, SA.y1 a.cy, SA.x2 b.cx, SA.y2 b.cy, HP.style "stroke: #e8e4d6; stroke-width: 1; pointer-events: none;" ] ]
          else [])
       pairs

-- | Hover-discovery state for a glyph. `HiNone` = nothing hovered (even field).
-- | `HiSame` = pitch-class IDENTICAL to the hovered chord (an "anagram" — the same
-- | chord under another spelling/name, e.g. C# vs E♭m); it gets a contrasting teal,
-- | wholly off the warm ramp, because it isn't *related* — it IS the chord.
-- | `HiTier n` grades genuine relatedness on a warm ramp: 0 = shares nothing
-- | (ghosted) … 4 = highly similar.
data GlyphHi = HiNone | HiSame | HiTier Int

-- | Pitch classes two chords share.
sharedTones :: Array Int -> Array Int -> Int
sharedTones a b =
  let bs = nub (map (\x -> mod x 12) b)
  in length (filter (\x -> elem x bs) (nub (map (\x -> mod x 12) a)))

sameChordSet :: Array Int -> Array Int -> Boolean
sameChordSet a b = sort (nub (map (\x -> mod x 12) a)) == sort (nub (map (\x -> mod x 12) b))

-- | Grade a glyph against the hovered chord — the hovered chord itself is tier 5;
-- | otherwise the Jaccard similarity (shared ÷ union of pitch classes) is banded
-- | into tiers 0–4. Jaccard normalises for chord size, so a big chord that merely
-- | overlaps a small one lands mid-ramp, and genuinely similar chords rank high —
-- | the field reads as a graded web of relatedness rather than an on/off split.
hiFor :: Maybe { root :: Int, pcs :: Array Int } -> Array Int -> GlyphHi
hiFor mh pcs = case mh of
  Nothing -> HiNone
  Just h ->
    if sameChordSet h.pcs pcs then HiSame
    else
      let a = nub (map (\x -> mod x 12) h.pcs)
          s = sharedTones h.pcs pcs
          u = length a + length (nub (map (\x -> mod x 12) pcs)) - s
          j = if u == 0 then 0.0 else toNumber s / toNumber u
      in HiTier (if j >= 0.6 then 4 else if j >= 0.45 then 3 else if j >= 0.28 then 2 else if j > 0.0 then 1 else 0)

-- | The warm relatedness ramp: pale straw (weakly related) → gold → amber → burnt
-- | orange (the hovered chord), with tier 0 ghosted back so the related web lifts.
hiStyle :: GlyphHi -> { fill :: String, stroke :: String, sw :: String, rootDot :: String, otherDot :: String }
hiStyle = case _ of
  HiNone   -> { fill: "rgba(184,134,11,0.09)", stroke: "#bcac78", sw: "1",   rootDot: "#b8860b", otherDot: "#9a9a9a" }
  HiSame   -> { fill: "rgba(20,130,128,0.26)",  stroke: "#0f7d7b", sw: "1.9", rootDot: "#0b5a58", otherDot: "#3a8f8d" }
  HiTier 0 -> { fill: "rgba(150,150,150,0.02)", stroke: "#efeee9", sw: "1",   rootDot: "#e6ddc6", otherDot: "#ededed" }
  HiTier 1 -> { fill: "rgba(200,180,120,0.11)", stroke: "#d8c98f", sw: "1",   rootDot: "#c9a94e", otherDot: "#c6c1ab" }
  HiTier 2 -> { fill: "rgba(190,160,70,0.17)",  stroke: "#c9a445", sw: "1.2", rootDot: "#b8860b", otherDot: "#a9a48c" }
  HiTier 3 -> { fill: "rgba(180,130,20,0.23)",  stroke: "#b3801f", sw: "1.4", rootDot: "#9a5f06", otherDot: "#8f8a72" }
  HiTier 4 -> { fill: "rgba(160,95,5,0.29)",    stroke: "#9a5f06", sw: "1.6", rootDot: "#7a4300", otherDot: "#7a745c" }
  HiTier _ -> { fill: "rgba(150,80,0,0.37)",    stroke: "#7a4300", sw: "1.9", rootDot: "#5c3200", otherDot: "#6a6450" }

-- | A chord drawn as a polygon inscribed in the chromatic circle: a vertex per
-- | pitch class (12 o'clock = C, clockwise by semitone), the root dotted gold. The
-- | shape *is* the chord's interval structure — a compact, stave-less glyph. `hi`
-- | tints it for hover-discovery (self / near relative / dimmed).
pcPolygon :: forall m. GlyphHi -> Int -> Array Int -> Number -> Number -> Number -> Array (H.ComponentHTML Action Slots m)
pcPolygon hi root pcs cx cy r =
  let sty = hiStyle hi
      ang p = (-Number.pi / 2.0) + toNumber p * (Number.pi / 6.0)
      pt p = { x: cx + r * Number.cos (ang p), y: cy + r * Number.sin (ang p) }
      sorted = sort (nub (map (\p -> mod p 12) pcs))
      verts = map pt sorted
      dot p =
        let q = pt p
        in SE.circle [ SA.cx q.x, SA.cy q.y, SA.r 1.6, HP.style ("fill: " <> (if p == mod root 12 then sty.rootDot else sty.otherDot) <> "; pointer-events: none;") ]
  in [ SE.element (ElemName "polygon")
         [ HP.attr (AttrName "points") (ptsStr verts)
         , HP.style ("fill: " <> sty.fill <> "; stroke: " <> sty.stroke <> "; stroke-width: " <> sty.sw <> "; pointer-events: none;")
         ]
         []
     ]
       <> map dot sorted

-- | The Lattices lens — every diatonic degree's full tertian lattice at once, as
-- | seven compact clusters of chromatic-circle polygons with their Hasse edges.
-- | Hover a glyph to space-preview it; click to audition, shift-click to catch.
latticesSurface :: forall m. State -> H.ComponentHTML Action Slots m
latticesSurface st =
  let seeds = diatonicTriads st.key
      clusters = mapWithIndex (degreeCluster st.key) seeds
      members = concat clusters
      edges = concatMap degreeEdges clusters
      mh = st.hoveredTriad
      vb = geoView st
  in SE.svg
      ( [ SA.viewBox vb.x vb.y vb.w vb.h
        , SA.width 880.0
        , SA.height 600.0
        , SA.class_ (cn "vetula-surface")
        , HP.style surfaceFillCss
        , HE.onWheel \we -> ZoomAt (WE.toEvent we) (WE.deltaY we)
        , HE.onMouseDown (PanStart <<< ME.toEvent)
        ] <> geoPanAttrs st )
      ( edges
          <> concatMap (latMemberView mh) members
          <> mapWithIndex latDegreeLabel seeds
          <> latColorRibbon st
      )

-- | The color-overlay layers on the voice-leading lattice (2026-07-31 redesign).
-- | The lattice's own glyphs are chromatic-circle POLYGONS (stave-less), and its
-- | tertian webs climb UPWARD from baseY — so the empty top of the surface carries
-- | the color chords as a wrapping ribbon of polygon glyphs (matching this
-- | surface's vocabulary, the way the fifths corona matches the stave), each
-- | de-duplicated and badged by source. The CONTEXT PALETTE swatches name the hues.
-- | Non-interactive for now (catch = Step 4). A deeper pass would place each color
-- | chord by voice-leading distance into the web itself — logged as a follow-up.
latColorRibbon :: forall m. State -> Array (H.ComponentHTML Action Slots m)
latColorRibbon st =
  let entries = mergedLayerChords st
      perRow = 22
      place j e =
        let col = mod j perRow
            row = j / perRow
            x = latticeLeft + 24.0 + toNumber col * 34.0
            y = -280.0 + toNumber row * 42.0
        in pcPolygon HiNone e.chord.root e.chord.pcs x y 9.0
             <> layerBadges x (y - 16.0) e.layers
             <> [ colorHit e.chord x y 11.0 ]
  in concat (mapWithIndex place entries)

-- | One lattice glyph plus its transparent click target (the polygon itself is
-- | click-through so the disc-shaped hit region stays uniform). `mh` is the hovered
-- | chord, driving cross-degree hover-discovery highlighting.
latMemberView :: forall m. Maybe { root :: Int, pcs :: Array Int } -> LatMember -> Array (H.ComponentHTML Action Slots m)
latMemberView mh m =
  pcPolygon (hiFor mh m.chord.pcs) m.chord.root m.chord.pcs m.cx m.cy latGeo.glyphR
    <>
      [ SE.circle
          [ SA.cx m.cx, SA.cy m.cy, SA.r latGeo.glyphR
          , HP.style "fill: transparent; cursor: pointer;"
          , HE.onMouseEnter \_ -> HoverTriad (Just { root: m.chord.root, pcs: m.chord.pcs })
          , HE.onMouseLeave \_ -> HoverTriad Nothing
          , HE.onClick \e -> if ME.shiftKey e then CatchNode m.chord else AuditionNode m.chord
          ]
      ]

-- | The degree's root name under its cluster.
latDegreeLabel :: forall m. Int -> ChordNode -> H.ComponentHTML Action Slots m
latDegreeLabel i seed =
  SE.text
    [ SA.x (latticeLeft + toNumber i * latGeo.bandW), SA.y (latGeo.baseY + 26.0)
    , HP.attr (AttrName "text-anchor") "middle"
    , HP.style "font-size: 13px; fill: #6a6a6a; letter-spacing: 0.04em; -webkit-user-select: none; user-select: none;"
    ]
    [ HH.text (noteName seed.root) ]

-- ---------------------------------------------------------------------------
-- The Generate lens (tank-seeded relatives — the compositional loop)
-- ---------------------------------------------------------------------------

genRadius :: Number
genRadius = 80.0

-- | Where each seed's constellation sits — a 3-wide grid, so up to six tank seeds
-- | tile two rows across the zoomable frame.
genCenter :: Int -> { x :: Number, y :: Number }
genCenter i =
  { x: -230.0 + toNumber (mod i 3) * 230.0
  , y: -120.0 + toNumber (i / 3) * 250.0
  }

-- | The Generate lens — each tank chord as a SEED with a ring of voice-led
-- | relatives bloomed around it (reusing `generateCandidates`, the same engine the
-- | Lab pick-mode uses). "shake" re-rolls: a different adventure + a rotated crop
-- | of the ranked relatives. Hover a relative to preview, click to audition,
-- | shift-click to catch it back into the tank — closing the catch→grow→catch loop.
generativeSurface :: forall m. State -> H.ComponentHTML Action Slots m
generativeSurface st =
  let vb = geoView st
  in SE.svg
      ( [ SA.viewBox vb.x vb.y vb.w vb.h
        , SA.width 880.0
        , SA.height 600.0
        , SA.class_ (cn "vetula-surface")
        , HP.style surfaceFillCss
        , HE.onWheel \we -> ZoomAt (WE.toEvent we) (WE.deltaY we)
        , HE.onMouseDown (PanStart <<< ME.toEvent)
        ] <> geoPanAttrs st )
      ( if length st.tank == 0
          then
            [ SE.text
                [ SA.x 0.0, SA.y 0.0, HP.attr (AttrName "text-anchor") "middle"
                , HP.style "font-size: 15px; fill: #b8b8b8; -webkit-user-select: none; user-select: none;"
                ]
                [ HH.text "catch chords into the tank, then grow relatives here — press shake ⟳" ]
            ]
          else concat (mapWithIndex (genCluster st) (take 6 st.tank))
      )

-- | One seed's constellation: the seed glyph at the centre, its relatives ringed
-- | around it with faint spokes. `genRoll` varies both the adventure dial and which
-- | slice of the ranked relatives shows, so each shake crops a fresh set.
genCluster :: forall m. State -> Int -> Specimen -> Array (H.ComponentHTML Action Slots m)
genCluster st i spec =
  let key = st.key
      center = genCenter i
      seedN = specToNode (9000 + i) key spec
      adv = toNumber (mod st.genRoll 5) * 0.2
      rollRot = toNumber st.genRoll * 0.37
      full = generateCandidates Append [ seedN ] key adv 0
      rel = take 8 (drop (mod (st.genRoll * 2) 7) full)
      n = max 1 (length rel)
      placed = mapWithIndex
        (\j c ->
           let ang = toNumber j * (2.0 * Number.pi / toNumber n) + rollRot
           in { c, cx: center.x + genRadius * Number.cos ang, cy: center.y + genRadius * Number.sin ang })
        rel
      spokes = map
        (\p -> SE.line [ SA.x1 center.x, SA.y1 center.y, SA.x2 p.cx, SA.y2 p.cy, HP.style "stroke: #eceae2; stroke-width: 1; pointer-events: none;" ])
        placed
  in spokes
       <> concatMap (\p -> genGlyph st.hoveredTriad p.cx p.cy 11.0 false p.c) placed
       <> genGlyph st.hoveredTriad center.x center.y 15.0 true seedN

-- | A generative glyph: the chromatic-circle polygon, a name below, and a
-- | transparent hit target. The seed wears a gold ring and only auditions; a
-- | relative auditions on click and catches on shift-click. `mh` (the hovered
-- | chord) drives cross-constellation hover-discovery highlighting.
genGlyph :: forall m. Maybe { root :: Int, pcs :: Array Int } -> Number -> Number -> Number -> Boolean -> ChordNode -> Array (H.ComponentHTML Action Slots m)
genGlyph mh cx cy r isSeed c =
  pcPolygon (hiFor mh c.pcs) c.root c.pcs cx cy r
    <> (if isSeed then [ SE.circle [ SA.cx cx, SA.cy cy, SA.r (r + 4.0), HP.style "fill: none; stroke: #b8860b; stroke-width: 1.5; pointer-events: none;" ] ] else [])
    <>
      [ SE.text
          [ SA.x cx, SA.y (cy + r + 11.0), HP.attr (AttrName "text-anchor") "middle"
          , HP.style ("font-size: 10px; fill: " <> (if isSeed then "#7a5c00" else "#8a8a8a") <> "; pointer-events: none; -webkit-user-select: none; user-select: none;")
          ]
          [ HH.text c.label ]
      , SE.circle
          [ SA.cx cx, SA.cy cy, SA.r r
          , HP.style "fill: transparent; cursor: pointer;"
          , HE.onMouseEnter \_ -> HoverTriad (Just { root: c.root, pcs: c.pcs })
          , HE.onMouseLeave \_ -> HoverTriad Nothing
          , HE.onClick \e -> if (not isSeed) && ME.shiftKey e then CatchNode c else AuditionNode c
          ]
      ]

-- | The clip region for the chord cloud. On Explore it stops at the pitch
-- | ladder's edge (x −304) so a dense beeswarm can't paint over the ladder; on
-- | the Lattice there's no ladder, so it spans almost the full width (x −436),
-- | reclaiming the old ladder strip for left-rooted families.
cloudClipDef :: forall m. H.ComponentHTML Action Slots m
cloudClipDef =
  let x0 = -436.0
      w = 440.0 - x0
  in SE.defs []
       [ SE.element (ElemName "clipPath") [ SA.id "cloudClip" ]
           [ SE.rect [ SA.x x0, SA.y (-300.0), SA.width w, SA.height 600.0 ] ]
       , SE.element (ElemName "linearGradient")
           [ SA.id "focusBeam"
           , HP.attr (AttrName "gradientUnits") "userSpaceOnUse"
           , HP.attr (AttrName "x1") "0", HP.attr (AttrName "y1") "232"
           , HP.attr (AttrName "x2") "0", HP.attr (AttrName "y2") "-300"
           ]
           [ beamStop "0" "0.22", beamStop "0.5" "0.07", beamStop "1" "0" ]
       ]

-- | One stop of the focused-root beam gradient (gold, fading up).
beamStop :: forall m. String -> String -> H.ComponentHTML Action Slots m
beamStop off op =
  SE.element (ElemName "stop")
    [ HP.attr (AttrName "offset") off
    , HP.attr (AttrName "stop-color") "#b8860b"
    , HP.attr (AttrName "stop-opacity") op
    ] []

-- | The focused-root beam: a soft gold gradient column rising from the focused
-- | key up through the cloud — marks the root you're building on and lights its
-- | x-column (where chords stacked on it land). Brightest at the key, fading up;
-- | pointer-events off so it never blocks a click.
focusBeam :: forall m. Maybe Int -> Array (H.ComponentHTML Action Slots m)
focusBeam = case _ of
  Nothing -> []
  Just pc ->
    [ SE.rect
        [ SA.x (keyX pc - 20.0), SA.y (-300.0), SA.width 40.0, SA.height (keyboard.top + 300.0)
        , HP.attr (AttrName "fill") "url(#focusBeam)"
        , HP.style "pointer-events: none;"
        ]
    ]

clippedCloud :: forall m. Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
clippedCloud kids =
  SE.element (ElemName "g") [ HP.attr (AttrName "clip-path") "url(#cloudClip)" ] kids

-- ---------------------------------------------------------------------------
-- Pick mode — the generated candidate cloud (shift-select steps on the right)
-- ---------------------------------------------------------------------------

-- | The candidate cloud that replaces the lattice while a progression selection
-- | is live. The anchor chord(s) sit at fixed positions; the candidates spread
-- | by voice-leading distance (x) and outside-ness (y). Click a candidate to
-- | commit it; click the empty backdrop to cancel.
pickSurface :: forall m. State -> H.ComponentHTML Action Slots m
pickSurface st =
  let steps = pathSteps st
      sel = sort (nub st.genSel)
      anchors = mapMaybe (\ix -> index steps ix) sel
      mode = genModeOf sel (length steps)
      anchorNodes = case anchors of
        [ a, b ] -> [ anchorView (-360.0) 30.0 a, anchorView 360.0 30.0 b ]
        [ a ] -> [ anchorView 0.0 (-248.0) a ]
        _ -> []
      title = case mode of
        Just Prepend -> "chords to begin with — click one to prepend"
        Just Append -> "where to next? — click one to append"
        Just Transition -> "transitions between the two — click one to insert"
        Just Substitute -> "plausible substitutes — click one to replace"
        Nothing -> ""
  in SE.svg
      [ SA.viewBox (-440.0) (-300.0) 880.0 600.0, SA.width 880.0, SA.height 600.0
      , SA.class_ (cn "vetula-surface"), HP.style "max-width: none;" ]
      ( [ SE.rect [ SA.x (-440.0), SA.y (-300.0), SA.width 880.0, SA.height 600.0
                  , HP.style "fill: #fafafa;", HE.onClick \_ -> CancelGen ]
        , SE.text [ SA.x 0.0, SA.y (-278.0), SA.class_ (cn "pick-title") ] [ HH.text title ]
        ] <> anchorNodes <> map candidateView st.candidates )

-- | An anchor chord in pick mode: its disc + glyph, ringed to mark it as fixed.
anchorView :: forall m. Number -> Number -> ChordNode -> H.ComponentHTML Action Slots m
anchorView x y c =
  let r = nodeRadius c
  in SE.g [ SA.class_ (cn (nodeClass c)) ]
      ( [ SE.circle [ SA.cx x, SA.cy y, SA.r (r + 3.0), SA.class_ (cn "anchor-ring") ]
        , SE.circle [ SA.cx x, SA.cy y, SA.r r, SA.class_ (cn "vn-disc") ]
        ] <> chordGlyph [] x y c.voicing
          <> [ SE.text [ SA.x x, SA.y (y + r + 13.0), SA.class_ (cn "cand-label") ] [ HH.text c.label ] ] )

-- | A candidate chord: disc + glyph + name, clickable to commit it.
candidateView :: forall m. ChordNode -> H.ComponentHTML Action Slots m
candidateView c =
  let r = nodeRadius c
  in SE.g
      [ SA.class_ (cn (nodeClass c <> " cand"))
      , HE.onMouseEnter \_ -> Hover (Just c.id)
      , HE.onMouseLeave \_ -> Hover Nothing
      , HE.onClick \_ -> PickCandidate c.id
      ]
      ( [ SE.circle [ SA.cx c.targetX, SA.cy c.targetY, SA.r r, SA.class_ (cn "vn-disc") ] ]
          <> chordGlyph [] c.targetX c.targetY c.voicing
          <> [ SE.text [ SA.x c.targetX, SA.y (c.targetY + r + 12.0), SA.class_ (cn "cand-label") ] [ HH.text c.label ] ] )

-- | The pick-mode control strip above the surface: the adventurousness dial
-- | (smooth ↔ striking) + a cancel. Empty when not picking.
pickBar :: forall m. State -> H.ComponentHTML Action Slots m
pickBar st =
  if length st.genSel > 0
    then HH.div
      [ HP.style "display: flex; align-items: center; gap: 10px; margin: 0 0 8px; font-size: 12px; color: #6a6a6a;" ]
      [ HH.span [ HP.style "letter-spacing: 0.04em;" ] [ HH.text "smooth" ]
      , HH.input
          [ HP.attr (AttrName "type") "range", HP.attr (AttrName "min") "0"
          , HP.attr (AttrName "max") "1", HP.attr (AttrName "step") "0.01"
          , HP.value (show st.adventure), HE.onValueInput SetAdventure
          , HP.style "width: 160px;"
          ]
      , HH.span [ HP.style "letter-spacing: 0.04em;" ] [ HH.text "striking" ]
      , HH.button
          [ HP.style "margin-left: 8px; border: 1px solid #d8d8d8; background: #fafafa; color: #4a4a4a; cursor: pointer; padding: 3px 12px; border-radius: 3px; font-size: 12px;"
          , HE.onClick \_ -> CancelGen ]
          [ HH.text "cancel" ]
      ]
    else HH.text ""

-- ---------------------------------------------------------------------------
-- The Progression tab — the path laid out as a row of voicing ladders
-- ---------------------------------------------------------------------------

-- | The path you built on the Lattice, one step per column, each its own copy of
-- | the voicing ladder + favourites strip. Click a step's number to hear it and
-- | make it active; then the Tab / arrow / f revoicing all act on that step. The
-- | drawn voicings (and the bubble glyphs everywhere) update live as you edit.
-- | The chords of the path, in order (the steps of the progression).
pathSteps :: State -> Array ChordNode
pathSteps st = mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) st.path

-- | The live-derived Tidal source for the current progression.
currentSource :: State -> String
currentSource st = progressionSource (groupLabel st.key) (pathSteps st)

-- | Rebuild a chord from a pasted note-list: the lowest note grounds the bass,
-- | the rest are the voicing (uppers). Re-export reproduces the paste, since
-- | `playNotes` re-grounds the bass at the same pitch class.
importChord :: Int -> Array Int -> ChordNode
importChord nid notes =
  let sorted = sort notes
      bp = mod (fromMaybe 60 (head sorted)) 12
  in { id: nid, parentId: Nothing, root: bp, bassPc: bp
     , pcs: nub (map (\m -> mod m 12) sorted)
     , voicing: drop 1 sorted
     , kind: Voiced, label: noteName bp
     , pinned: false, outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
     , anchor: Free }   -- imported from raw MIDI: no scale reading

-- | Nearest chromatic shift (semitones, in [-5,6]) between two tonics — so a
-- | key-change transposition keeps the progression in a similar register.
nearestShift :: Int -> Int -> Int
nearestShift old new = let d = mod (new - old) 12 in if d > 6 then d - 12 else d

-- | Transpose a chord by `d` semitones, KEEPING its id (so `path` stays valid).
-- | Re-imports the shifted notes, so pcs / bass / label come out right; the chord
-- | becomes an imported (off-lattice) voicing in the new key.
transposeChord :: Int -> ChordNode -> ChordNode
transposeChord d c = importChord c.id (map (_ + d) (playNotes c))

-- | The progression panel — the Lattice's right-hand side. The path assembles
-- | top→bottom, each step a compact horizontal note-row (pitch left→right) you
-- | can revoice; ▶ plays the whole thing; the Tidal source at the foot saves and
-- | loads it. Replaces the old collection on the Lattice.
progressionPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
progressionPanel st =
  let steps = pathSteps st
  in HH.div
      -- capped so the note-rows stay a compact reference even when the rail is wide
      -- (Perform); the playheads, not the progression, get the extra width.
      [ HP.style "margin: 0 0 6px; max-width: 360px;" ]
      [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 8px; margin: 0 0 8px;" ]
          ( [ HH.button
                [ HP.style "border: 1px solid #b8860b; background: #fbf6e9; color: #7a5c00; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px; font-weight: 600;"
                , HE.onClick \_ -> PlayPath
                ]
                [ HH.text "▶ preview" ]
            ]
            -- ✕ clear appears only with a progression to clear; one click empties it
            -- so the next shift-click on the lattice starts a NEW progression.
            <> ( if length steps == 0 then [] else
                   [ HH.button
                       [ HP.style "border: 1px solid #e0d4d4; background: #fdf7f7; color: #9a6a6a; cursor: pointer; padding: 3px 11px; border-radius: 4px; font-size: 12px;"
                       , HE.onClick \_ -> ClearPath
                       ]
                       [ HH.text "✕ clear" ]
                   ]
               )
          )
      , if length steps == 0 then HH.text "" else
          HH.div [ HP.style "display: flex; align-items: center; gap: 6px; margin: 0 0 8px;" ]
            [ HH.input
                [ HP.value st.saveName, HE.onValueInput SetSaveName, HP.attr (AttrName "placeholder") "name…"
                , HP.style "flex: 1; font-size: 12px; padding: 2px 7px; border: 1px solid #ddd; border-radius: 3px;" ]
            , HH.button
                [ HP.style "border: 1px solid #d8d8d8; background: #fafafa; color: #4a4a4a; cursor: pointer; padding: 2px 12px; border-radius: 3px; font-size: 12px;"
                , HE.onClick \_ -> SaveProg ]
                [ HH.text "save → library" ]
            ]
      , if length steps == 0
          then HH.p
                 [ HP.style "color: #c0c0c0; font-size: 12px; line-height: 1.6; margin: 4px 0 10px;" ]
                 [ HH.text "Shift-click chords on the lattice to grow a progression here. Or paste a saved one into the Tidal source below and press Load." ]
          else HH.div [ HP.style "margin: 0 0 10px;" ] (mapWithIndex (progressionRow st) steps)
      ]

-- | The performance rack: transport (bpm / preview channel) + one live-coded Tidal
-- | read-head per voice. Extracted from the old Performance `loadedView`; the rail shows
-- | it only in Perform focus, where it has the width for the (wide) voice rows.
-- | The new bottom VOICE BAR (docs/DESIGN-scene-modal.md) — a one-line strip of
-- | FOUR FIXED lanes (block · strum · arp · odo). Each lane is just its live
-- | surface: an enable light (green on / red off), its two mini-notation fields
-- | (read-head + notes), and a commit arrow (red when there are uncommitted edits).
-- | Deliberately NO routing / bpm / channel here — those move to the routing modal.
-- | Built alongside the old Voices card (control C); C is deleted once this is at
-- | parity (`canonicalVoices` guarantees the four lanes always exist).
voiceBar :: forall m. State -> H.ComponentHTML Action Slots m
voiceBar st =
  HH.div
    -- Tied to the WINDOW bottom edge like the shell's top nav — `fixed`, not
    -- `absolute`, because the Vetula container's `calc(100vh - 118px)` height
    -- stops short of the true bottom. Same gradient/bevel treatment as the nav,
    -- but COLOURED and thin — minimal padding, ceding vertical space to the
    -- lattice above.
    [ HP.style ( "position: fixed; bottom: 0; left: 0; right: 0; z-index: 40; box-sizing: border-box; "
        <> "display: flex; gap: 10px; align-items: center; padding: 3px 12px; overflow: hidden; "
        <> "font-family: Georgia, serif; background: linear-gradient(#b6c3cc,#a4b4be); "
        <> "border-top: 1px solid #00000026; box-shadow: 0 -1px 4px #00000018;" ) ]
    ( [ HH.span [ HP.style "font-size: 11px; letter-spacing: 0.16em; text-transform: uppercase; color: #33424d; flex: 0 0 auto;" ] [ HH.text "voices" ] ]
        <> map (voiceLane st) [ Tuple 0 "block", Tuple 1 "strum", Tuple 2 "arp", Tuple 3 "odo" ] )

-- | One fixed lane of the voice bar, found by its canonical id.
voiceLane :: forall m. State -> Tuple Int String -> H.ComponentHTML Action Slots m
voiceLane st (Tuple vid label) = case find (\v -> v.id == vid) st.voices of
  Nothing -> HH.text ""
  Just v ->
    let dirty = v.patternDraft /= v.pattern || v.notePatternDraft /= v.notePattern
        on = not v.muted
    in HH.div
        [ HP.style "flex: 1 1 0; min-width: 0; display: flex; align-items: center; gap: 6px; border-left: 1px solid #00000022; padding-left: 8px;" ]
        [ HH.button
            [ HP.style ("border: none; background: none; cursor: pointer; font-size: 12px; line-height: 1; padding: 0; " <> if on then "color: #2f8f3f;" else "color: #c14a4a;")
            , HP.title (if on then label <> " on — click to mute" else label <> " off — click to enable")
            , HE.onClick \_ -> ToggleVoiceMute vid ]
            [ HH.text "●" ]
        , HH.span [ HP.style "font-size: 11px; color: #2c3944; width: 32px; letter-spacing: 0.03em;" ] [ HH.text label ]
        , laneInput v.patternDraft "chord" (SetVoicePattern vid)
        , laneInput v.notePatternDraft "notes" (SetVoiceNotePattern vid)
        , HH.button
            [ HP.style ("border: none; background: none; cursor: pointer; font-size: 13px; line-height: 1; padding: 0; " <> if dirty then "color: #c0392b;" else "color: #7d8d97;")
            , HP.title "commit both patterns"
            , HE.onClick \_ -> CommitVoicePattern vid ]
            [ HH.text "▶" ]
        ]

-- | A compact monospace mini-notation field for one lane of the voice bar.
laneInput :: forall m. String -> String -> (String -> Action) -> H.ComponentHTML Action Slots m
laneInput val ph onInput =
  HH.input
    [ HP.value val, HE.onValueInput onInput, HP.placeholder ph
    , HP.style "flex: 1 1 0; min-width: 36px; font-family: ui-monospace, monospace; font-size: 11px; padding: 3px 6px; border-radius: 4px; border: 1px solid #ddd6c6; background: #fff;" ]

playheadsRack :: forall m. State -> H.ComponentHTML Action Slots m
playheadsRack st =
  let chords = perfChords st
  -- Voices are stacked cards, tiled by a responsive grid: one column in a narrow
  -- rail, several across a wide one. No fixed width / overflow band-aid needed —
  -- each card fills its own track, so it can't "eat the display" (#91).
  in HH.div_
      [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 12px; margin: 0 0 8px; flex-wrap: wrap;" ]
          [ HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ] [ HH.text "commit applies both boxes · empty ♪ = block/arp/strum renderer" ]
          , cellBtn "+ add voice" false AddVoice
          , numField "bpm" st.tempo SetTempo
          , numField "preview ch" st.previewChan SetPreviewChan
          ]
      , HH.keyed (ElemName "div")
          [ HP.style "display: grid; grid-template-columns: repeat(auto-fill, minmax(260px, 1fr)); gap: 8px; align-items: start;" ]
          (map (\v -> Tuple (show v.id) (voicePlayheadRow (length chords) v)) st.voices)
      ]

-- | The progression rows' pitch-axis geometry (shared with DragMove's inverse).
prowW :: Number
prowW = 300.0

prowH :: Number
prowH = 34.0

prowPad :: Number
prowPad = 16.0

prowPitchX :: Int -> Number
prowPitchX m = prowPad + (toNumber (clamp 36 84 m - 36) / 48.0) * (prowW - 2.0 * prowPad)

-- | One progression step as a horizontal note-row: the chord's voiced notes
-- | (bass + uppers) plotted along a left→right pitch axis. Click the row to hear
-- | it and make it active; click a bass note to select it, DRAG an upper note
-- | left/right to move it by octaves (then ↑/↓ nudge the selected voice too).
-- | The active step is tinted; its selected voice gets a ring.
progressionRow :: forall m. State -> Int -> ChordNode -> H.ComponentHTML Action Slots m
progressionRow st i c =
  let active = Just c.id == st.sounding
      picked = elem i st.genSel        -- shift-selected as a pick-mode anchor
      sel = if active then st.selected else Nothing
      selHere j = case sel of
        Just BassVoice -> j == 0
        Just (UpperVoice k) -> j == k + 1
        Nothing -> false
      cy = prowH / 2.0
      octLine m = SE.line [ SA.x1 (prowPitchX m), SA.y1 4.0, SA.x2 (prowPitchX m), SA.y2 (prowH - 4.0), SA.class_ (cn "prow-oct") ]
      dot j m = SE.circle
        [ SA.cx (prowPitchX m), SA.cy cy, SA.r 4.5
        -- colour = pitch class (see the revoice ladder) — one hue per chromatic tone
        , SA.class_ (cn ("ladder-dot ladder-dot--" <> show (mod m 12)
                         <> (if j == 0 then " ladder-dot--bass" else " ladder-dot--drag")
                         <> (if selHere j then " ladder-dot--sel" else "")))
        , if j == 0 then HE.onMouseDown \_ -> SelectVoice c.id BassVoice
                    else HE.onMouseDown \ev -> DragStart (ME.altKey ev) true c.id (j - 1) m
        ]
  in HH.div
      [ HP.style ("display: flex; align-items: center; gap: 8px; padding: 0 2px; border-radius: 3px; cursor: pointer; "
          <> (if picked then "background: #e7eef4; box-shadow: inset 0 0 0 1px #9bb8d4;"
              else if active then "background: #f1efe7;" else ""))
      , HE.onClick \ev -> StepClick i (ME.shiftKey ev)
      ]
      [ HH.span
          [ HP.style ("flex: 0 0 16px; text-align: right; font-size: 11px; font-weight: 600; "
              <> (if picked then "color: #3f5f8a;" else if active then "color: #7a5c00;" else "color: #b0b0b0;")) ]
          [ HH.text (show i) ]
      , SE.svg
          ( [ SA.viewBox 0.0 0.0 prowW prowH, HP.style "width: 100%; height: auto; display: block;" ]
              <> (case st.drag of
                    Just _ ->
                      [ HE.onMouseMove (DragMove <<< ME.toEvent)
                      , HE.onMouseUp \_ -> DragEnd
                      , HE.onMouseLeave \_ -> DragEnd
                      ]
                    Nothing -> [])
          )
          (map octLine [ 36, 48, 60, 72, 84 ] <> mapWithIndex dot (playNotes c))
      , HH.span [ HP.style "flex: 0 0 26px; font-size: 10px; color: #b0b0b0;" ] [ HH.text c.label ]
      ]

-- | The progression as TidalCycles source — a `note "<…>"` pattern of the voiced
-- | chords (the "preserve these pitch sets for Tidal" thread). It's an EDITABLE
-- | textarea: by default it tracks the live progression (re-deriving as you
-- | revoice), but paste a saved block + press Load and it parses back into a
-- | fresh progression to work on. `copy` for the clipboard, `revert` to drop
-- | edits and return to the live view.
tidalExport :: forall m. Boolean -> Maybe String -> String -> Array ChordNode -> H.ComponentHTML Action Slots m
tidalExport open sourceEdit keyLabel steps =
  let derived = progressionSource keyLabel steps
      eff = fromMaybe derived sourceEdit
      editing = isJust sourceEdit
  in HH.div
      [ HP.style "margin: 8px 0 0;" ]
      [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 8px; margin: 0 0 5px;" ]
          ( [ HH.button
                [ HP.style "border: none; background: none; cursor: pointer; padding: 0; font-size: 12px; color: #6a6a6a; letter-spacing: 0.06em; text-transform: uppercase;"
                , HE.onClick \_ -> ToggleSource ]
                [ HH.text ((if open then "▾ " else "▸ ") <> "Tidal source") ]
            , HH.button [ HP.style btnStyle, HE.onClick \_ -> CopyTidal eff ] [ HH.text "copy" ]
            ]
              -- load / revert only matter when the code is open for editing
              <> (if open
                    then [ HH.button [ HP.style loadBtnStyle, HE.onClick \_ -> LoadSource ] [ HH.text "load" ] ]
                      <> (if editing then [ HH.button [ HP.style btnStyle, HE.onClick \_ -> RevertSource ] [ HH.text "revert" ] ] else [])
                    else [])
          )
      -- the code itself is hidden until revealed; copy still works collapsed
      , if open
          then HH.textarea
            [ HP.value eff
            , HP.rows 6
            , HE.onValueInput EditSource
            , HP.style ("width: 100%; box-sizing: border-box; resize: vertical; "
                <> "font-family: 'SF Mono', Menlo, monospace; font-size: 12px; line-height: 1.5; "
                <> "color: #2a2a2a; background: #f7f7f5; border: 1px solid "
                <> (if editing then "#cdbb96" else "#e2e2e2") <> "; "
                <> "border-radius: 4px; padding: 8px 10px;")
            ]
          else HH.text ""
      ]
  where
  btnStyle =
    "border: 1px solid #d8d8d8; background: #fafafa; color: #4a4a4a; cursor: pointer; "
      <> "padding: 2px 10px; border-radius: 3px; font-size: 12px;"
  loadBtnStyle =
    "border: 1px solid #cdbb96; background: #fbf6e9; color: #7a5c00; cursor: pointer; "
      <> "padding: 2px 10px; border-radius: 3px; font-size: 12px; font-weight: 600;"

cellBtn :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action Slots m
cellBtn label on act =
  HH.button
    [ HP.style ("border: 1px solid " <> (if on then "#7a5c00" else "#d8d8d8") <> "; cursor: pointer; "
        <> "padding: 2px 9px; border-radius: 3px; font-size: 12px; "
        <> (if on then "background: #fbf6e9; color: #7a5c00; font-weight: 600;" else "background: #fafafa; color: #4a4a4a;"))
    , HE.onClick \_ -> act ]
    [ HH.text label ]

numField :: forall m. String -> Int -> (String -> Action) -> H.ComponentHTML Action Slots m
numField lbl val act =
  HH.label [ HP.style "display: inline-flex; align-items: center; gap: 4px; font-size: 11px; color: #9a9a9a;" ]
    [ HH.text lbl
    , HH.input
        [ HP.attr (AttrName "type") "number", HP.value (show val), HE.onValueInput act
        , HP.style "width: 44px; font-size: 12px; padding: 1px 4px; border: 1px solid #ddd; border-radius: 3px;" ]
    ]

-- | The library — saved progressions, searchable by key, each loadable.
libraryView :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
libraryView st =
  let q = trimLower st.libSearch
      shown = filter (\(Tuple _ e) -> q == "" || contains (Pattern q) (trimLower (e.keyLabel <> " " <> e.name)))
                (mapWithIndex Tuple st.library)
  in HH.div [ HP.style "padding: 2px 0;" ]
      [ HH.div [ HP.style "display: flex; align-items: center; gap: 12px; margin: 0 0 10px;" ]
          [ HH.input
              [ HP.value st.libSearch, HE.onValueInput SetLibSearch, HP.attr (AttrName "placeholder") "search by key…"
              , HP.style "font-size: 13px; padding: 3px 8px; border: 1px solid #ddd; border-radius: 4px; width: 100%; box-sizing: border-box;" ]
          ]
      , case st.publishMsg of
          Nothing -> HH.text ""
          Just msg -> HH.div [ HP.style "font-size: 11px; color: #5a7458; margin: 0 0 8px;" ] [ HH.text msg ]
      , if length st.library == 0
          then HH.p [ HP.style "color: #c0c0c0; font-size: 13px; line-height: 1.6;" ]
                 [ HH.text "Nothing captured yet. Build a progression on the Lattice — it's auto-saved here as you go (◦). Star (★) the keepers." ]
          else HH.div [] (map libRow shown)
      ]
  where
  libRow (Tuple i e) =
    HH.div
      [ HP.style ("display: flex; align-items: center; gap: 12px; padding: 6px 4px; border-bottom: 1px solid #f0f0f0;"
                  <> (if e.kept then "" else " opacity: 0.66;")) ]
      -- ★ keeper (frozen) vs ☆ ephemeral auto-capture (live-updated). Click to toggle.
      [ HH.button
          [ HP.style ("border: none; background: none; cursor: pointer; font-size: 14px; color: "
                      <> (if e.kept then "#c8a86a" else "#c8c4b8") <> ";")
          , HP.title (if e.kept then "keeper — click to release to ephemeral" else "ephemeral — click to keep")
          , HE.onClick \_ -> KeepLib i ]
          [ HH.text (if e.kept then "★" else "☆") ]
      , HH.span [ HP.style "flex: 0 0 120px; font-size: 12px; color: #7a5c00;" ] [ HH.text e.keyLabel ]
      , HH.span [ HP.style "flex: 1; font-size: 13px; color: #2a2a2a;" ] [ HH.text e.name ]
      , HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ] [ HH.text (show (length (parseProgression e.source)) <> " chords") ]
      , cellBtn "load" false (LoadProg i)
      , HH.button
          [ HP.style "border: none; background: none; cursor: pointer; color: #5a7458; font-size: 14px;"
          , HP.title "publish this progression to the Amphora store"
          , HE.onClick \_ -> PublishLib i ] [ HH.text "⚱" ]
      , HH.button [ HP.style "border: none; background: none; cursor: pointer; color: #c0c0c0; font-size: 15px;", HE.onClick \_ -> DeleteLib i ] [ HH.text "×" ]
      ]

-- | One voice's live-coded read-head: its controls (destination / mute / renderer /
-- | channel / remove) then a mini-notation input, a commit button, and a status
-- | readout — parse error if the DRAFT is broken, else the committed loop length in
-- | bars, with a • when there are uncommitted edits. Placeholder shows the default
-- | (`[0 1 … n-1]/n`) that an empty pattern falls back to.
voicePlayheadRow :: forall m. Int -> Voice -> H.ComponentHTML Action Slots m
voicePlayheadRow n v =
  let draft = trim v.patternDraft
      draftErr = if draft == "" then Nothing
                 else case patternClock n draft of
                        Left e -> Just e
                        Right _ -> Nothing
      -- Axis-B note pattern parse-check (indices bound generously, reef wraps to chord size)
      noteDraft = trim v.notePatternDraft
      noteErr = if noteDraft == "" then Nothing
                else case patternClock 128 noteDraft of
                       Left e -> Just e
                       Right _ -> Nothing
      dirty = v.patternDraft /= v.pattern || v.notePatternDraft /= v.notePattern
      colOf = case _ of
        Just _ -> "#e2b6ae"
        Nothing -> "#dcdcdc"
      borderCol = colOf draftErr
      noteBorderCol = colOf noteErr
      loopBars = (voiceClock n v).loopLen / 16
      anyErr = isJust draftErr || isJust noteErr
      status = if anyErr
        then HH.span [ HP.style "font-size: 11px; color: #c0392b;" ] [ HH.text "⚠ parse error" ]
        else HH.span [ HP.style "font-size: 11px; color: #9a9a9a;" ]
          [ HH.text ((if dirty then "• " else "") <> "loop " <> show loopBars <> "b") ]
      -- one voice = one vertical card that fills its own grid track, so nothing
      -- stretches across the whole rail (the old wide-row "eats the display" bug, #91).
      fieldLabel txt = HH.div [ HP.style "font-size: 10px; letter-spacing: 0.04em; text-transform: uppercase; color: #b8b8b8; margin: 6px 0 2px;" ] [ HH.text txt ]
      patInput val ph col act =
        HH.input
          -- controlled (like the bpm field, which edits fine); the field carries the
          -- REAL pattern text. Placeholder only shows if you clear it, and reads as a hint.
          [ HP.value val
          , HP.placeholder ph
          , HP.style ("width: 100%; box-sizing: border-box; font-family: ui-monospace, monospace; font-size: 12px; padding: 4px 6px; border-radius: 4px; border: 1px solid " <> col <> ";")
          , HE.onValueInput act
          ]
  in HH.div
      [ HP.style ("display: flex; flex-direction: column; padding: 8px; border: 1px solid #eee; border-radius: 6px; background: #fbfbfa;"
          <> (if v.muted && v.dest == ToMidi then " opacity: 0.5;" else "")) ]
      [ -- header: destination + per-dest controls, remove on the right
        HH.div [ HP.style "display: flex; align-items: center; gap: 6px; flex-wrap: wrap;" ]
          ( [ cellBtn (destName v.dest) (v.dest == ToOdonus) (CycleVoiceDest v.id) ]
              <> (case v.dest of
                    ToMidi ->
                      [ cellBtn (if v.muted then "off" else "on") (not v.muted) (ToggleVoiceMute v.id)
                      , cellBtn (rendName v.renderer) true (CycleVoiceRenderer v.id)
                      , HH.input
                          [ HP.value v.name
                          , HE.onValueInput (SetVoiceName v.id)
                          , HP.placeholder "name → ch5"
                          , HP.style "width:84px;font-family:ui-monospace,monospace;font-size:11px;padding:2px 5px;border-radius:4px;border:1px solid #d8d3c6;background:#fff"
                          ]
                      ]
                    ToOdonus ->
                      [ numField "id" v.channel (SetVoiceChannel v.id) ])
              <>
              [ HH.div [ HP.style "flex: 1 1 auto;" ] []
              , HH.button [ HP.style "border: none; background: none; cursor: pointer; color: #c8c8c8; font-size: 14px; line-height: 1;", HE.onClick \_ -> RemoveVoice v.id ] [ HH.text "×" ]
              ] )
      , fieldLabel "read-head — which chord, when"
      , patInput v.patternDraft ("empty = " <> defaultPattern n) borderCol (SetVoicePattern v.id)
      , HH.div [ HP.style "display: flex; align-items: center; justify-content: space-between; gap: 6px; margin: 6px 0 2px;" ]
          [ HH.span [ HP.style "font-size: 10px; letter-spacing: 0.04em; text-transform: uppercase; color: #b8b8b8;" ] [ HH.text "♪ notes — 0 = lowest · -1 = top" ]
          , cellBtn (RA.articLabel v.articulator) (v.articulator /= RA.ABlock) (CycleVoiceArticulator v.id)
          ]
        -- Axis B: how to sound the chord — a note-index pattern. Empty = the renderer
        -- (block/arp/strum). The articulator button above picks the note ALPHABET both
        -- this pattern AND the renderer use: block = the chord's own notes; voice-led = a
        -- fixed-N line carried through the loop (0=bass, -1=melody); entering = just the
        -- notes new to each chord (arp the newcomers in).
      , patInput v.notePatternDraft ("empty = " <> rendName v.renderer) noteBorderCol (SetVoiceNotePattern v.id)
      , HH.div [ HP.style "display: flex; align-items: center; justify-content: space-between; gap: 6px; margin-top: 8px;" ]
          [ cellBtn "commit" dirty (CommitVoicePattern v.id)
          , status
          ]
      ]

trimLower :: String -> String
trimLower = toLower <<< trim

-- | The drawn Hasse web on the Lattice tab: a faint line between every pair of
-- | chords that differ by one note (the same edges that act as force links).
latticeLinkLines
  :: forall m
   . Map Int { x :: Number, y :: Number }
  -> Array ChordNode
  -> Array (H.ComponentHTML Action Slots m)
latticeLinkLines posMap chords =
  concatMap
    (\l -> case Map.lookup l.source posMap, Map.lookup l.target posMap of
        Just p, Just q -> [ SE.line [ SA.x1 p.x, SA.y1 p.y, SA.x2 q.x, SA.y2 q.y, SA.class_ (cn "lattice-link") ] ]
        _, _ -> [])
    (neighborLinks chords)

-- | The left-hand pitch ladder: a fixed pitch axis (C2..C6) showing the chord
-- | that is currently sounding as coloured dots — the prototype's display.
ladderView :: forall m. Maybe VoiceSel -> Maybe ChordNode -> Array (H.ComponentHTML Action Slots m)
ladderView msel msound = grid <> octs <> dots
  where
  lx = -432.0
  rx = -320.0
  dotX = -376.0
  midiToY m = 205.0 - toNumber (m - 36) * 9.8
  grid = map
    (\m -> SE.line [ SA.x1 lx, SA.y1 (midiToY m), SA.x2 rx, SA.y2 (midiToY m), SA.class_ (cn "ladder-line") ])
    (range 36 84)
  octs = concatMap oct [ 36, 48, 60, 72, 84 ]
  oct m =
    [ SE.line [ SA.x1 lx, SA.y1 (midiToY m), SA.x2 rx, SA.y2 (midiToY m), SA.class_ (cn "ladder-oct") ]
    , SE.text [ SA.x (lx - 6.0), SA.y (midiToY m + 3.0), SA.class_ (cn "ladder-label") ] [ HH.text ("C" <> show (m / 12 - 1)) ]
    ]
  dots = case msound of
    Nothing -> []
    Just c -> mapWithIndex (dot c.id) (playNotes c)
  -- index 0 is the bass: click selects it, arrows rotate it through chord tones.
  -- uppers (≥1) octave-drag or click-then-arrow; the voicing index is i-1.
  selHere i = case msel of
    Just BassVoice -> i == 0
    Just (UpperVoice j) -> i == j + 1
    Nothing -> false
  -- the chord id rides on every handler so a replicated ladder targets its own
  -- chord (and makes it the active one) rather than a single global `sounding`.
  dot cid i m = SE.circle
    ( [ SA.cx dotX, SA.cy (midiToY m), SA.r 6.5
      -- colour = pitch class (one hue per chromatic tone), so a voice keeps its colour
      -- through octave-drag / re-sort / doubling — identity follows the tone, not the slot.
      , SA.class_ (cn ("ladder-dot ladder-dot--" <> show (mod m 12)
                       <> (if i == 0 then " ladder-dot--bass" else " ladder-dot--drag")
                       <> (if selHere i then " ladder-dot--sel" else "")))
      , if i == 0 then HE.onMouseDown \_ -> SelectVoice cid BassVoice
                  else HE.onMouseDown \ev -> DragStart (ME.altKey ev) false cid (i - 1) m
      ]
    )

-- | The favoured-voicings strip above the ladder: one swatch per kept voicing of
-- | the sounding chord's note-set, each a vertical bar showing that voicing's
-- | register and span. The active voicing (the one currently sounding) is
-- | highlighted; click a swatch to switch to it. `f` adds/removes the current
-- | voicing. Empty until you keep one.
voicingStrip
  :: forall m
   . Maybe ChordNode
  -> Map String (Array (Array Int))
  -> Array (H.ComponentHTML Action Slots m)
voicingStrip msound favs = case msound of
  Nothing -> []
  Just c ->
    let vs = fromMaybe [] (Map.lookup (pcsKey c) favs)
        n = length vs
    in if n == 0
         then [ SE.text
                  [ SA.x ((sLeft + sRight) / 2.0), SA.y (sTop + 15.0), SA.class_ (cn "vstrip-hint") ]
                  [ HH.text "press f to keep a voicing" ] ]
         else mapWithIndex (swatch c.id c.voicing n) vs
  where
  sLeft = -430.0
  sRight = -322.0
  sTop = -298.0
  sBot = -272.0
  yReg m = sBot - 3.0 - (toNumber (clamp 36 84 m - 36) / 48.0) * (sBot - sTop - 6.0)
  swatch cid active n i v =
    let w = (sRight - sLeft) / toNumber n
        x0 = sLeft + toNumber i * w
        cx = x0 + w / 2.0
        lo = fromMaybe 60 (minimum v)
        hi = fromMaybe 60 (maximum v)
    in SE.g
        [ SA.class_ (cn ("vstrip-sw" <> if v == active then " vstrip-sw--active" else ""))
        , HE.onClick \_ -> PickVoicing cid v
        ]
        [ SE.rect [ SA.x (x0 + 1.0), SA.y sTop, SA.width (w - 2.0), SA.height (sBot - sTop), SA.class_ (cn "vstrip-bg") ]
        , SE.line [ SA.x1 cx, SA.y1 (yReg hi), SA.x2 cx, SA.y2 (yReg lo), SA.class_ (cn "vstrip-span") ]
        ]

-- | The revoice modal: a dim backdrop + a left "drawer" lighting up the kept
-- | pitch ladder + favourites strip for one chosen chord — the one-stop shop for
-- | every WITHIN-chord change. Octave-drag a note (⌥ doubles it), Tab/Shift-Tab
-- | cycle voicings, ↑/↓ nudge a selected voice, f keeps a voicing, and the slash
-- | row re-foots the chord on any of its tones. Esc / click-away closes.
revoiceModal :: forall m. State -> H.ComponentHTML Action Slots m
revoiceModal st =
  Modal.modal
    { open: isJust mc
    , title: maybe "revoice" (\c -> noteName c.root <> " · revoice") mc
    , onClose: CloseRevoice
    }
    (maybe [] revoiceBody mc)
  where
  mc = st.revoicing >>= \cid -> find (\c -> c.id == cid) st.chords
  -- The revoice content stays SVG (the ladder's octave-drag + voicing swatches are
  -- intrinsically spatial), but now lives in a small self-contained <svg> INSIDE the
  -- shared Modal widget's body rather than being painted into the lattice canvas. The
  -- viewBox is a cropped window of the SAME user-space, so ladderView/voicingStrip keep
  -- their exact coordinates and svgYFromEvent (reads currentTarget's own viewBox) keeps
  -- the drag math (205 − y)/9.8 correct. Backdrop, title and × are the widget's job now.
  revoiceBody c =
    let tones = sort (nub c.pcs)
    in [ SE.svg
           ( [ SA.viewBox (-455.0) (-303.0) 290.0 565.0
             , SA.class_ (cn "rv-svg")
             -- suppress text-selection during octave-drag (the ladder labels were
             -- getting selected, hijacking the pointer) — the lattice surface does this
             -- via .vetula-surface CSS; the modal svg needs it inline.
             , HP.style "display: block; margin: 0 auto; width: 280px; height: 545px; max-width: 100%; user-select: none; -webkit-user-select: none;"
             ]
               -- octave-drag: mirror the lattice surface's move/up/leave, only while dragging
               <> (case st.drag of
                     Just _ ->
                       [ HE.onMouseMove (DragMove <<< ME.toEvent)
                       , HE.onMouseUp \_ -> DragEnd
                       , HE.onMouseLeave \_ -> DragEnd
                       ]
                     Nothing -> []) )
           ( voicingStrip (Just c) st.favorites
               <> ladderView st.selected (Just c)
               <> [ SE.text [ SA.x (-447.0), SA.y 250.0, SA.class_ (cn "rv-bass-label") ] [ HH.text "bass /" ] ]
               <> mapWithIndex (slashBtn c.bassPc) tones )
       , HH.div [ HP.style "margin-top: 10px; font-size: 11px; color: #9a9a9a; text-align: center;" ]
           [ HH.text "Tab voicings · ↑↓ nudge · drag = 8ve · ⌥ doubles · f keep · Esc" ]
       ]
  slashBtn activeBass i pc =
    let w = 27.0
        x0 = -408.0 + toNumber i * (w + 2.0)
    in SE.g
        [ SA.class_ (cn ("rv-slash" <> if pc == activeBass then " rv-slash--active" else ""))
        , HE.onClick \_ -> SlashBass pc ]
        [ SE.rect [ SA.x x0, SA.y 238.0, SA.width w, SA.height 18.0, SA.class_ (cn "rv-slash-bg") ]
        , SE.text [ SA.x (x0 + w / 2.0), SA.y 251.0, SA.class_ (cn "rv-slash-tx") ] [ HH.text (noteName pc) ]
        ]

-- | The imaginary piano: white keys, black keys on top, note labels; scale
-- | keys tinted.
keyboardView :: forall m. Array Int -> Array (H.ComponentHTML Action Slots m)
keyboardView scl = whites <> blacks <> labels
  where
  kb = keyboard
  inScale pc = elem pc scl
  whites = mapWithIndex whiteKey whiteKeyPcs
  whiteKey slot pc =
    SE.rect
      [ SA.x (kb.left + toNumber slot * kb.whiteW)
      , SA.y kb.top
      , SA.width kb.whiteW
      , SA.height (kb.bot - kb.top)
      , SA.class_ (cn ("pkey pkey--w" <> if inScale pc then " pkey--in" else ""))
      , HE.onClick \_ -> SummonRoot pc
      ]
  blacks = map blackKey blackKeyPcs
  blackKey pc =
    let w = kb.whiteW * 0.58
    in SE.rect
        [ SA.x (keyX pc - w / 2.0)
        , SA.y kb.top
        , SA.width w
        , SA.height ((kb.bot - kb.top) * 0.62)
        , SA.class_ (cn ("pkey pkey--b" <> if inScale pc then " pkey--in" else ""))
        , HE.onClick \_ -> SummonRoot pc
        ]
  labels = map lbl whiteKeyPcs
  lbl pc = SE.text [ SA.x (keyX pc), SA.y (kb.bot - 5.0), SA.class_ (cn "pkey-label") ] [ HH.text (noteName pc) ]

axisLabels :: forall m. Array (H.ComponentHTML Action Slots m)
axisLabels =
  [ lab 0.0 (-272.0) "↑ more extended (7 · 9 · 11 · 13)"
  , lab 0.0 216.0 "click a key to focus · 2–7 / e / s / l to grow"
  ]
  where
  lab x y t = SE.text [ SA.x x, SA.y y, SA.class_ (cn "axis-label") ] [ HH.text t ]

nodeView
  :: forall m
   . Array Int
  -> Map Int Int
  -> Set Int
  -> Map Int { x :: Number, y :: Number }
  -> ChordNode
  -> H.ComponentHTML Action Slots m
nodeView scl pathOrder collectedHere posMap c =
  let pos = fromMaybe { x: c.targetX, y: c.targetY } (Map.lookup c.id posMap)
      r = nodeRadius c
      -- a gold ring + numbered step badge when this chord is on the path segment
      pathHi = case Map.lookup c.id pathOrder of
        Just n ->
          [ SE.circle [ SA.cx pos.x, SA.cy pos.y, SA.r (r + 3.0), SA.class_ (cn "path-ring") ]
          , SE.circle [ SA.cx (pos.x - r * 0.7), SA.cy (pos.y - r * 0.7), SA.r 6.5, SA.class_ (cn "path-badge") ]
          , SE.text [ SA.x (pos.x - r * 0.7), SA.y (pos.y - r * 0.7 + 3.0), SA.class_ (cn "path-num") ] [ HH.text (show n) ]
          ]
        Nothing -> []
  in SE.g
      [ SA.class_ (cn (nodeClass c))
      , HE.onMouseEnter \_ -> Hover (Just c.id)
      , HE.onMouseLeave \_ -> Hover Nothing
      -- Tank model (Slice D): the pool is a hunting ground, not a progression
      -- builder. Plain click AUDITIONS the chord (hear it, make it sounding);
      -- shift-click CATCHES it into the tank (a mouse alternative to `k`).
      -- Progressions are now sequenced from the tank, not walked on the lattice.
      , HE.onClick \e -> if ME.shiftKey e then CatchChord c.id else PlayChordId c.id
      ]
      ( [ -- the disc; size = stave-span (cluster ↔ wide), fill = ring index
          -- (cool in-scale → warm the further outside the chosen scale it sits)
          SE.circle [ SA.cx pos.x, SA.cy pos.y, SA.r r, SA.class_ (cn "vn-disc") ]
        ]
          <> chordGlyph scl pos.x pos.y c.voicing
          <> (if Set.member c.id collectedHere
                then [ SE.circle [ SA.cx (pos.x + r * 0.7), SA.cy (pos.y - r * 0.7), SA.r 3.5, SA.class_ (cn "vn-pin") ] ]
                else [])
          <> pathHi
      )

-- | A tiny five-line treble-staff thumbnail of the chord's upper voicing —
-- | noteheads at diatonic pitch positions, with accidentals. The chord IS its
-- | notation, not a letter name. The notes are centred on the middle line by
-- | their MEAN diatonic step, so the glyph sits centred in the bubble whatever
-- | the register (a fixed reference left the top of the staff perpetually empty
-- | and the whole cloud looking low / misaligned).
chordGlyph :: forall m. Array Int -> Number -> Number -> Array Int -> Array (H.ComponentHTML Action Slots m)
chordGlyph scl cx cy notes = staff <> concatMap noteGlyph notes
  where
  staff = map
    (\dy -> SE.line [ SA.x1 (cx - 13.0), SA.y1 (cy + dy), SA.x2 (cx + 13.0), SA.y2 (cy + dy), SA.class_ (cn "staff-line") ])
    [ 12.0, 6.0, 0.0, -6.0, -12.0 ]
  steps = map diatonicStep notes
  ctr = if length steps == 0 then 32.0 else toNumber (sum steps) / toNumber (length steps)
  -- a notehead is tinted when its pitch class lies OUTSIDE the base scale, so the
  -- borrowed/colour tones show as one, two or three coloured dots — the chord's
  -- degree-of-difference read straight off its notation. (Empty scl = no tint.)
  noteGlyph midi =
    let y = cy - (toNumber (diatonicStep midi) - ctr) * 3.0
        outside = length scl > 0 && not (elem (mod midi 12) scl)
        headCls = if outside then "notehead notehead--out" else "notehead"
    in [ SE.circle [ SA.cx cx, SA.cy y, SA.r 3.0, SA.class_ (cn headCls) ] ]
         <> (case accOf midi of
               "" -> []
               a -> [ SE.text [ SA.x (cx - 11.0), SA.y (y + 2.5), SA.class_ (cn "accidental") ] [ HH.text a ] ])

-- | Diatonic step of a MIDI note (E4 = 30, the staff's bottom line).
diatonicStep :: Int -> Int
diatonicStep midi = (midi / 12 - 1) * 7 + letterOf midi

letterOf :: Int -> Int
letterOf midi = case mod midi 12 of
  0 -> 0
  1 -> 0
  2 -> 1
  3 -> 2
  4 -> 2
  5 -> 3
  6 -> 3
  7 -> 4
  8 -> 5
  9 -> 5
  10 -> 6
  _ -> 6

accOf :: Int -> String
accOf midi = case mod midi 12 of
  1 -> "♯"
  6 -> "♯"
  3 -> "♭"
  8 -> "♭"
  10 -> "♭"
  _ -> ""

-- | Disc class: fill encodes internal dissonance (d0 calm … d4 hot); the centre
-- | (home) chord gets a bold ring on top.
nodeClass :: ChordNode -> String
nodeClass c =
  "vn vn--d" <> show (min 4 c.outside) <> (if c.isCentre then " vn--centre" else "")

-- | Disc size = the voicing's pitch SPAN, top-to-bottom on the stave — not note
-- | count. A 4-note chord smeared across 2½ octaves is a big bubble; a tight
-- | cluster is small. Collision uses the same radius so wide chords claim room.
nodeRadius :: ChordNode -> Number
nodeRadius c = discRadius c.voicing

discRadius :: Array Int -> Number
discRadius voicing = min 40.0 (12.0 + spreadSpan voicing * 0.7)

-- ---------------------------------------------------------------------------
-- Visual dimensions (grammar-of-graphics channels)
-- ---------------------------------------------------------------------------

-- | The voicing's pitch span in semitones — small = clustered, large = spread.
spreadSpan :: Array Int -> Number
spreadSpan xs = case maximum xs, minimum xs of
  Just hi, Just lo -> toNumber (hi - lo)
  _, _ -> 0.0

keyOptions :: Array { value :: String, label :: String }
keyOptions = map (\pc -> { value: show pc, label: noteName pc }) (range 0 11)
