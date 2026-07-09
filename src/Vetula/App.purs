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

import Data.Array (concat, concatMap, deleteAt, drop, elem, elemIndex, filter, find, head, index, insertAt, last, length, mapMaybe, mapWithIndex, modifyAt, nub, nubByEq, range, replicate, sort, take, updateAt, (!!))
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
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith, toLower, trim)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
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
import Web.Event.Event (Event, EventType(..), preventDefault)
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
import Vetula.Store as Store
import Vetula.Tank (Specimen, SpecimenId(..), Provenance(..), specNotes)
import Reef.Vetula.Perf (VChord, VVoice, VDest(..), VRenderer(..), PerfClock, cursorAtClock, renderAlphaBlockMidiAt, renderAlphaClockMidiAt) as RV
import Reef.Vetula.Articulate (VArticulator(..), articulate, articLabel, nextArtic) as RA
import Vetula.Playhead (clockFor, defaultPattern, noteClock, patternClock)
import Data.Either (Either(..))
import Reef.Vetula.Protocol (encodePerf) as RV
import Binnacle.Time (dateNow)
import Vetula.Tidal (progressionSource, parseProgression)
import Vetula.Clipboard (copyText)
import Binnacle.Midi as Midi
import Hylograph.Halogen.UI.Select as Select
import Hylograph.Halogen.UI.Modal as Modal
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)
import Hylograph.Simulation
  ( Engine(..), SimulationEvent(..), SimulationHandle, SimulationNode
  , Setup, runSimulation, setup, manyBody, collide, link, positionX, positionY
  , withStrength, withRadius, withDistance, withX, withY, static, dynamic )
import Harmonia.Anchor (Anchor(..))
import Harmonia.Chord (Key, Mode(..), cMajorKey)
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
data StageLens = LensKeyboard | LensPadGrid | LensCircleFifths | LensTonnetz | LensLattices | LensGenerate

derive instance eqStageLens :: Eq StageLens

-- | The lens registry. A new lens appends here (+ a constructor + a render branch).
allLenses :: Array StageLens
allLenses = [ LensKeyboard, LensPadGrid, LensCircleFifths, LensTonnetz, LensLattices, LensGenerate ]

lensLabel :: StageLens -> String
lensLabel = case _ of
  LensKeyboard -> "keyboard"
  LensPadGrid -> "pad grid"
  LensCircleFifths -> "fifths"
  LensTonnetz -> "tonnetz"
  LensLattices -> "lattices"
  LensGenerate -> "grow"

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

type State =
  { key :: Key
  , focus :: Focus                     -- Slice 4c: Hunt (lattice-dominant) vs Perform (rail-dominant)
  , railOpen :: Set RailSection        -- Slice 4b: which rail accordion sections are open (multi)
  , leftOpen :: Set LeftSection        -- which LEFT accordion sections are open (multi)
  , chords :: Array ChordNode          -- the model (pin, provenance, layout targets)
  , nodes :: Array VNode               -- live positions from the simulation
  , focusId :: Int
  , hoveredId :: Maybe Int
  , hoveredTriad :: Maybe { root :: Int, pcs :: Array Int }  -- Tonnetz hover (no pool id)
  , nextId :: Int
  , handle :: Maybe (SimulationHandle Row)
  , subId :: Maybe H.SubscriptionId
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
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
  }

data Action
  = Initialize
  | MidiReady (Maybe Midi.MidiOut) String
  | SimTick
  | SimDone
  | Hover (Maybe Int)
  | HoverTriad (Maybe { root :: Int, pcs :: Array Int })  -- Tonnetz: hover a triad for space-preview
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
  | ClearStage             -- remove all staged seeds + the chords bloomed from them
  | AuditionTriad Int (Array Int)        -- Tonnetz: hear a triad off the net (root pc, pcs)
  | CatchTriad Int (Array Int) Boolean   -- Tonnetz: freeze a triad into the tank (root, pcs, isMajor)
  | AuditionNode ChordNode               -- Lattices: hear a generated chord (its own voicing)
  | CatchNode ChordNode                  -- Lattices: freeze a generated chord into the tank
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

component :: forall i o m. MonadAff m => H.Component SourceQuery i o m
component = H.mkComponent
  { initialState: \_ ->
      { key: cMajorKey
      , focus: Hunt
      , railOpen: Set.fromFoldable [ SecProgression, SecLibrary, SecVoices ]
      , leftOpen: Set.fromFoldable [ SecSetup, SecTank, SecLens ]
      , chords: []
      , nodes: []
      , focusId: 0           -- the first diatonic triad seed
      , hoveredId: Nothing
      , hoveredTriad: Nothing
      , nextId: 100          -- generated children start here; seeds are 0..17
      , handle: Nothing
      , subId: Nothing
      , midiOut: Nothing
      , midiName: "…"
      , previewChan: 0
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
      , imported: Set.empty
      , sourceEdit: Nothing
      , sourceOpen: false
      , helpOpen: false
      , genSel: []
      , candidates: []
      , adventure: 0.25
      , library: []
      , capSeq: 0
      , lastCapIdx: Nothing
      , lastCapSig: ""
      , libSearch: ""
      , saveName: ""
      , perfName: Nothing
      , voices: []
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
      , nextVoiceId: 0
      -- name → canonical MIDI channel, pushed from the shell's Tidal-page routing
      -- table (SetRouting). Unnamed / unbound voices fall back to the default channel.
      , routing: Map.empty :: Map String Int
      , tank: []
      , nextSpecId: 0
      , seedChord: Map.empty
      , lens: LensKeyboard
      , viewCx: 0.0
      , viewCy: 0.0
      , viewZoom: 1.0
      , panning: Nothing
      , panMoved: false
      , genRoll: 0
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
  }

-- | Answer the shell: the live progression as Tidal (TIDAL tab), or as a
-- | sequence of pitch-class sets (the Odonus chord-quantiser feed).
handleQuery :: forall o m a. MonadAff m => SourceQuery a -> H.HalogenM State Action Slots o m (Maybe a)
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
  in mapMaybe
       (\v -> if v.dest == ToOdonus
                then (\c -> { id: v.channel, pcs: c.pcs }) <$> index chords v.cursor
                else Nothing)
       st.voices

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

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- MIDI out
    { emitter: midiE, listener: midiL } <- liftEffect HS.create
    _ <- H.subscribe midiE
    liftEffect $ Midi.requestAccess \maccess -> case maccess of
      Just access -> do
        mout <- Midi.findOutput access midiPortName
        names <- Midi.outputNames access
        let nm = case mout of
              Just _ -> midiPortName <> " ✓"
              Nothing -> "no '" <> midiPortName <> "' — ports: " <> joinWith ", " names
        HS.notify midiL (MidiReady mout nm)
      Nothing -> HS.notify midiL (MidiReady Nothing "no Web-MIDI")
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
    for_ msaved \sv -> H.modify_ _ { library = sv.library, capSeq = length sv.library }
    -- keyboard
    { emitter: keyE, listener: keyL } <- liftEffect HS.create
    _ <- H.subscribe keyE
    liftEffect do
      w <- window
      el <- eventListener \ev -> do
        -- while typing in a name / search / source field, the single-key
        -- shortcuts (c = clear, r = reset, space, Tab…) must stand down — they
        -- were wiping the progression mid-type
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
        "c" -> handleAction ClearPath
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
      rebuild (st.key { tonic = mod pc 12 })
    Nothing -> pure unit

  SelectScale v -> do
    st <- H.get
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
        , voices = [ defaultVoice 0 0 Block (length fresh) ]
        , nextVoiceId = 1
        , sounding = head ids
        }

  -- ← library: set the current progression aside to browse the stack. Slice 4a: the
  -- path IS the progression, so snapshot it first (AutoCapture is on a timer and may
  -- not have fired yet), then clear — non-destructive, and it's the design's
  -- "library = snapshots you restore into the path".
  UnloadProg -> do
    st <- H.get
    when (length (pathSteps st) > 0) (captureSteps st)
    stopClock
    H.modify_ _ { path = [], perfName = Nothing, focus = Hunt, voices = [], playing = false }

  -- Delete shifts indices, so drop the session pointer to avoid it dangling.
  DeleteLib i -> do
    H.modify_ \s -> s { library = fromMaybe s.library (deleteAt i s.library), lastCapIdx = Nothing }
    persistLib

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
    else playChord (triadNode root pcs "")

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

  PerfPlay -> H.modify_ _ { armed = true } *> reconcilePerf
  PerfStop -> H.modify_ _ { armed = false } *> reconcilePerf

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
  lib <- H.gets _.library
  liftEffect $ Store.saveLibrary { library: lib }

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

-- | Send a chord's notes to the MIDI bus (no state change).
playChord :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
playChord c = do
  st <- H.get
  for_ st.midiOut \out ->
    liftEffect $ for_ (playNotes c) \n ->
      Midi.scheduleNote out { channel: st.previewChan, note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }

-- | Audition a tank specimen: sound its notes on the preview channel (no state
-- | change). Same shape as `playChord`, but reads a self-contained Specimen.
playSpecimen :: forall o m. MonadAff m => Specimen -> H.HalogenM State Action Slots o m Unit
playSpecimen s = do
  st <- H.get
  for_ st.midiOut \out ->
    liftEffect $ for_ (specNotes s) \n ->
      Midi.scheduleNote out { channel: st.previewChan, note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }

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
  case st.hoveredTriad of
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
    [ HP.style ("position: relative; width: 100%; height: calc(100vh - 118px); min-height: 620px; overflow: hidden; border-radius: 8px; background: " <> canvasBg <> ";") ]
    [ HH.div [ HP.style "position: absolute; inset: 0;" ] [ surface st ]
    , HH.div
        [ HP.style ("position: absolute; top: 12px; left: 12px; width: 256px; max-height: calc(100% - 24px); overflow: visible; z-index: 6; padding: 2px 12px 10px; " <> panelCss) ]
        [ leftColumn st ]
    , HH.div
        [ HP.style ("position: absolute; top: 12px; right: 12px; width: 340px; max-height: calc(100% - 24px); overflow-x: hidden; overflow-y: auto; z-index: 5; padding: 2px 12px 10px; " <> panelCss) ]
        [ railView st ]
    , HH.div
        [ HP.style "position: absolute; bottom: 12px; left: 50%; transform: translateX(-50%); z-index: 5;" ]
        [ pickBar st ]
    , helpOverlay st
    , revoiceModal st
    ]

-- | The reclaimed top bar, folded into a left accordion beside the full-height
-- | pool: Setup (key/scale/family/borrow/palettes/connection), Tank (caught
-- | chords), Lens (view choice). Multi-open, mirroring the right rail.
leftColumn :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
leftColumn st =
  HH.div
    [ HP.style "" ]
    [ accBox (Set.member SecSetup st.leftOpen) (ToggleLeftSection SecSetup) "Setup" "" (setupPane st)
    , accBox (Set.member SecTank st.leftOpen) (ToggleLeftSection SecTank) "Tank" (show (length st.tank) <> " caught") (tankPane st)
    , accBox (Set.member SecLens st.leftOpen) (ToggleLeftSection SecLens) "Lens" (lensLabel st.lens) (lensBar st)
    ]

-- | The Stage lens selector — a segmented control over `allLenses`. Switching the
-- | lens re-projects the SAME material (the sim keeps running underneath); adding
-- | a lens needs only a new `allLenses` entry, which is the decoupling proof.
lensBar :: forall m. State -> H.ComponentHTML Action Slots m
lensBar st =
  HH.div
    [ HP.style "display: flex; align-items: center; flex-wrap: wrap; gap: 6px; margin: 0 0 4px;" ]
    ( [ HH.span [ HP.style "font-size: 10px; color: #b0b0b0; letter-spacing: 0.12em; text-transform: uppercase; margin-right: 4px; width: 100%;" ] [ HH.text "Lens" ] ]
        <> map lensChip allLenses
        <> shakeChip
        <> resetChip )
  where
  -- geometric lenses only, and only once the viewport has moved: a way back to the
  -- fitted view (scroll to zoom · drag to pan).
  geometric = st.lens /= LensPadGrid
  -- the Generate lens's re-roll: a fresh crop of relatives around the same seeds.
  shakeChip =
    if st.lens == LensGenerate then
      [ HH.button
          [ HP.style "border: 1px solid #b8860b; background: #fbf6ea; color: #7a5c00; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px; margin-left: 8px;"
          , HP.title "re-roll the relatives around each tank seed"
          , HE.onClick \_ -> ShakeGenerate
          ]
          [ HH.text "shake ⟳" ]
      ]
    else []
  moved = st.viewZoom /= 1.0 || st.viewCx /= 0.0 || st.viewCy /= 0.0
  resetChip =
    if geometric && moved then
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

-- | The right rail: an accordion over the three rail objects — Progression (what),
-- | Library (saved progressions), Voices (how it's performed). One open at a time.
railView :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
railView st =
  HH.div
    [ HP.style "" ]
    [ accSection st SecProgression "Progression" (countLabel (length (pathSteps st)) "step") (progressionPanel st)
    , accSection st SecLibrary "Library" (countLabel (length st.library) "saved") (libraryView st)
    , accSection st SecVoices "Voices" (countLabel (length st.voices) "voice") (playheadsRack st)
    ]
  where
  countLabel n noun = show n <> " " <> noun <> (if n == 1 then "" else "s")

-- | One accordion section: a click-to-open header (chevron + title + count) and, when
-- | open, its body. Headers stay visible when collapsed so the column reads as a stack.
-- | Section-agnostic — the rail and the left column both drive it.
accBox :: forall m. Boolean -> Action -> String -> String -> H.ComponentHTML Action Slots m -> H.ComponentHTML Action Slots m
accBox open toggle title subtitle body =
  HH.div
    [ HP.style "border-top: 1px solid #d8ceb4;" ]
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; padding: 9px 2px; cursor: pointer; user-select: none;"
        , HE.onClick \_ -> toggle ]
        [ HH.span [ HP.style "font-size: 10px; color: #b0b0b0; width: 9px;" ] [ HH.text (if open then "▾" else "▸") ]
        , HH.span [ HP.style "font-size: 12px; color: #6a6a6a; letter-spacing: 0.06em; text-transform: uppercase;" ] [ HH.text title ]
        , HH.span [ HP.style "font-size: 11px; color: #bcbcbc;" ] [ HH.text subtitle ]
        ]
    , if open then HH.div [ HP.style "padding: 0 2px 14px;" ] [ body ] else HH.text ""
    ]

accSection :: forall m. State -> RailSection -> String -> String -> H.ComponentHTML Action Slots m -> H.ComponentHTML Action Slots m
accSection st sec = accBox (Set.member sec st.railOpen) (ToggleRailSection sec)

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
        <> [ field "BORROW"
               [ HH.slot (Proxy :: _ "borrowSelect") unit Select.component
                   ((Select.cascadingInput borrowGroups) { selected = Just (fromMaybe "off" st.borrowMode), searchable = true })
                   \(Select.Selected v) -> BorrowFrom v ]
           , field "PALETTES"
               [ HH.div [ HP.style "display: flex; flex-wrap: wrap; gap: 4px;" ] (map dropBtn exteriorGens) ]
           , connectionRow
           ]
    )
  where
  labelStyle = "font-size: 10px; color: #9a9a9a; letter-spacing: 0.1em; text-transform: uppercase;"
  field lbl controls =
    HH.div [ HP.style "display: flex; flex-direction: column; gap: 4px;" ]
      ([ HH.span [ HP.style labelStyle ] [ HH.text lbl ] ] <> controls)
  -- the exterior signpost buttons: drop a curated chord set onto the pool (toggle
  -- to remove). Active = amber, matching the ring-index warmth.
  dropBtn g =
    HH.button
      [ HP.style (dropBtnStyle (Map.member g.key st.dropped)), HE.onClick \_ -> DropSet g.key ]
      [ HH.text g.label ]
  dropBtnStyle active =
    "border: 1px solid " <> (if active then "#c9a23a" else "#dcdcdc")
      <> "; background: " <> (if active then "#fbf3df" else "#fafafa")
      <> "; color: " <> (if active then "#7a5c00" else "#6a6a6a")
      <> "; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px;"
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
  midiChip nm =
    let ok = nm /= "…" && nm /= ""
    in HH.span
         [ HP.style "display: inline-flex; align-items: center; gap: 5px; font-size: 11px; color: #8a8a8a; background: #f4f4f4; border: 1px solid #e8e8e8; border-radius: 10px; padding: 2px 9px;" ]
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
-- | seed (bloom around it in the pool); shift-click APPENDS it to the progression
-- | as a stable snapshot. A staged tile wears a gold frame so the pool ↔ tank link
-- | reads at a glance. (Staged-ness and sequenced-ness are orthogonal.)
specimenTile :: forall m. Boolean -> Specimen -> H.ComponentHTML Action Slots m
specimenTile staged s =
  HH.div
    [ HP.style ("position: relative; width: 66px; padding: 6px 6px 4px; border-radius: 6px; display: flex; flex-direction: column; align-items: center; "
                 <> if staged then "border: 1px solid #c9a23a; background: #fbf3df;"
                              else "border: 1px solid #eee; background: #fbfbfa;") ]
    [ HH.button
        [ HP.style "position: absolute; top: 1px; right: 3px; border: none; background: none; color: #c4c4c4; font-size: 13px; line-height: 1; cursor: pointer; padding: 0;"
        , HP.title "remove from tank"
        , HE.onClick \_ -> DeleteSpec s.id ]
        [ HH.text "×" ]
    , SE.svg
        [ SA.viewBox (-18.0) (-22.0) 36.0 44.0, SA.width 52.0, SA.height 46.0
        , HP.style "cursor: pointer;"
        , HE.onClick \e -> if ME.shiftKey e then SequenceSpec s.id else StageSpec s.id ]
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
      ( cofBackdrop tonic scl rootsPresent
          <> map (nodeView scl pathOrder Set.empty posMap) shown
      )

-- | The wheel behind the chords: twelve spokes radiating OUT from the hub, and the
-- | twelve root names ringed tightly around the centre. Diatonic roots (in the
-- | active scale) are inked dark with a soft parchment disc; the rest are ghosted
-- | grey. The tonic wears a gold ring. The spokes run from the hub outward so the
-- | chords beaded along them read as belonging to their root.
cofBackdrop
  :: forall m
   . Int -> Array Int -> Array Int -> Array (H.ComponentHTML Action Slots m)
cofBackdrop tonic scl rootsPresent =
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
    in disc <> tonicRing <>
         [ SE.text
             [ SA.x x, SA.y (y + 4.0)
             , HP.attr (AttrName "text-anchor") "middle"
             , HP.style ("font-size: 12px; fill: " <> txtColor <> "; letter-spacing: 0.02em; -webkit-user-select: none; user-select: none;")
             ]
             [ HH.text (noteName pc) ]
         ]

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
-- | shift-click catches it into the tank.
tonHit :: forall m. TonTri -> H.ComponentHTML Action Slots m
tonHit t =
  SE.element (ElemName "polygon")
    [ HP.attr (AttrName "points") (ptsStr t.verts)
    , HP.style "fill: transparent; cursor: pointer;"
    , HE.onMouseEnter \_ -> HoverTriad (Just { root: t.root, pcs: t.pcs })
    , HE.onMouseLeave \_ -> HoverTriad Nothing
    , HE.onClick \e -> if ME.shiftKey e then CatchTriad t.root t.pcs t.major else AuditionTriad t.root t.pcs
    ]
    []

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
      )

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
