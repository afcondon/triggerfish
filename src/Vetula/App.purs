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

import Data.Array (catMaybes, concat, concatMap, deleteAt, sortBy, drop, elem, elemIndex, filter, find, findIndex, head, index, insertAt, last, length, mapMaybe, mapWithIndex, modifyAt, nub, nubByEq, range, replicate, snoc, sort, take, takeEnd, unsnoc, updateAt, zipWith, (!!))
import Data.Foldable (all, any, foldl, foldr, for_, maximum, minimum, sum)
import Data.Traversable (traverse)
import Data.Int (ceil, floor, fromString, round, toNumber)
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
import Effect.Exception (try, message)
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
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
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
import Triggerfish.Glyph (ChipView, Glyph, sessionAliasOf)
-- Qualified: `chordGlyph` is also the name of this module's lattice-node
-- renderer, which draws a chord and has nothing to do with identity.
import Triggerfish.Clips.Share as Share
import Triggerfish.Glyph as TGlyph
import Triggerfish.GlyphView (faIcon, faIcons)
import Triggerfish.Preset (Preset, indexOfContent)
import Vetula.Store as Store
import Triggerfish.Amphora as Amphora
import Vetula.Tank (Specimen, SpecimenId(..), Provenance(..), specNotes)
import Reef.Vetula.Perf (VChord, VVoice, VDest(..), VRenderer(..), PerfClock, cursorAtClock, renderAlphaBlockMidiAt, renderAlphaClockMidiAt) as RV
import Reef.Vetula.Articulate (VArticulator(..), articulate, articLabel, nextArtic) as RA
import Reef.Route (printKey) as Route
import Vetula.Playhead (clockFor, defaultPattern, noteClock, patternClock)
import Vetula.Realise (fromChords)
import Vetula.Perform.Types
  ( PerfFx(..)
  , ArpDir(..)
  , PhraseAttach
  , ChannelMode(..)
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
  , termShort
  , termRigOnly
  , printArpDir
  , parseArpDir
  , printVoiceShape
  , parseVoiceShape
  )
import Triggerfish.PatternArg (PatternArg(..), argSrc, printArg, glyphArg, mkArg, tokenize, unq)
import Tidal.Pattern.Core (compress, stack, fast, slow, every, whenCycle)
import Vetula.Pattern (arpIndexed, arpRate, cycleRand, withSampledArg)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Arc(..), eventPart, eventValue, eventWhole, isDigital, mkArc, mkState, query)
import Tidal.Pattern.Types (Pattern, Event) as PT
import Haskell.Rational as Rat
import Haskell.Rational ((%))
import Triggerfish.Clips (MidiClip, NoteEvent, headCount)
import Triggerfish.Clips.Store as ClipStore
import Triggerfish.Clips.View as ClipsView
import Data.Either (Either(..))
import Reef.Vetula.Protocol (encodePerf) as RV
import Binnacle.Time (dateNow, perfNow)
import Effect.Ref as Ref
import Triggerfish.Capture.Logbook as Logbook
import Triggerfish.Capture.Types (Orientation(..), PlaySource(..), Zoom(..))
import Triggerfish.Capture.River (Flow(..), riverPanel, windowMicros) as River
import Triggerfish.Capture.View (CaptureState, capturePanel, markCode)
import Triggerfish.Capture.View as CaptureView
import Triggerfish.Ui.Pointer as Pointer
import Vetula.Tidal (progressionSource, parseProgression)
import Vetula.Lepidoptera (PerfDoc, VoiceSpec, docFromVoices, parseCard, parsePerform, printAsRecord, printCard)
import Vetula.StageCards as SC
import Triggerfish.Capture.RigLoops as RL
import Triggerfish.Capture.Runs as Runs
import Unsafe.Reference (unsafeRefEq)
import Vetula.Clipboard (copyText)
import Binnacle.Midi as Midi
import Halogen.Widgets.Select as Select
import Halogen.Widgets.MultiSelect as MultiSelect
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
import Vetula.Banks (butlerChords, stockChords)
import Vetula.Pads as Pads
import Harmonia.Trellis as HT
import Harmonia.Vary as HV
import Vetula.Rehearsal (Slot)
import Vetula.Rehearsal as RH
import Vetula.Vary as Vary
import Vetula.Spread (applyToNode, ghostRows, invertNode, nextBassTone, refootNode, spreadOfNode, toneAt)
import Harmonia.OpenVoicing (at, dropAt, setTone, sounds) as OV
import Vetula.Harmony (ChordNode, Family(..), Kind(..), bassMidi, blackKeyPcs, octaveShift, diatonicTriads, generate, interchangeChords, keyX, keyboard, latticeChild, latticeFamily, mcmullenChords, noteName, place, placeOutside, playNotes, scaleSet, suspendSet, triadNode, triadOn, voicingCandidates, whiteKeyPcs)

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

-- `Focus` (Hunt/Rail width-bias) and `RailSection` (the rail accordion) were
-- deleted 2026-08-06 with the Stage collapse. Both had outlived the rail UI:
-- `focus` was still being written but never read, `railOpen` was initialised and
-- toggled with no renderer left to observe it, and `poolSpine`/`focusTab` were
-- defined but never called. `Hunt` is now a Stage constructor.

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
-- | (Keyboard / pad-grid retired — subsumed by the interactive fifths.)
data Viewtype = Fifths | Tonnetz | Lattice | Explore | Pads | Vary

derive instance eqViewtype :: Eq Viewtype

-- | **The STAGE — Vetula's one mode axis** (AC, 2026-08-06).
-- |
-- | Three stages, in the order material flows through them:
-- |
-- |   HUNT ──→ PERFORM ──→ REVIEW ──┐
-- |    ↑   harmonic material        │
-- |    └── voiced into notes ───────┘
-- |             lifted into clips
-- |
-- |   * `Hunt lens` — hunt harmonic space through one of four projections
-- |     (fifths / tonnetz / lattice / explore). Produces the tank and the
-- |     progression. Adding a projection is still one `Viewtype` constructor and
-- |     one `surface` branch; the other stages stay untouched.
-- |   * `Perform` — player boxes voicing that material live, with the capture
-- |     river alongside. Produces notes.
-- |   * `Review` — the whole-session capture roll, for cherry-picking a phrase
-- |     into the clip library. Produces clips.
-- |
-- | **Why one type and not two.** This replaced `View = Browse Viewtype | Perform`
-- | plus a separate `captureView :: CapLive | CapReplay` flag nested inside
-- | Perform. The type said "two modes, one with a layout switch"; the player
-- | counted three (AC: "vetula is complex because it has three forms"). When the
-- | user's count and the type's count disagree, the type is wrong. The flat
-- | version also killed the dishonest lens dropdown, which used to display
-- | `browseOr lastBrowse view` — a *remembered* projection presented as the
-- | current one, because it had nowhere truthful to stand while Perform was up.
-- |
-- | **A stage is what you are LOOKING AT, not what is running.** The transport is
-- | orthogonal and lives in the shell's top nav: voices keep sounding while you
-- | hunt chords, the generator keeps generating while you review. Top nav owns
-- | what's running; the machine's secondary nav owns what you're looking at.
-- | Odonus has the same axis minus Hunt (Vetula is the harmonic authority, so
-- | Odonus has nothing to hunt) — see `Triggerfish.Odonus.Grid.Types.Stage`.
data Stage = Hunt Viewtype | Rehearse | Perform | Review

derive instance eqStage :: Eq Stage

viewtypeLabel :: Viewtype -> String
viewtypeLabel = case _ of
  Fifths -> "fifths"
  Tonnetz -> "tonnetz"
  Lattice -> "voice-leading lattice"
  Explore -> "explore"
  Pads -> "banks"
  Vary -> "vary"

-- | The browse projections, in bar order.
viewtypes :: Array Viewtype
viewtypes = [ Fifths, Tonnetz, Lattice, Explore, Pads, Vary ]

-- | The four HUNT projections as the dropdown hanging off the HUNT tab: a stable
-- | string `value` ↔ `Viewtype`, with a readable menu `label`. Perform and Review
-- | are NOT here — they're peer stages, not projections.
viewtypeValue :: Viewtype -> String
viewtypeValue = case _ of
  Fifths -> "fifths"
  Tonnetz -> "tonnetz"
  Lattice -> "lattice"
  Explore -> "explore"
  Pads -> "pads"
  Vary -> "vary"

viewtypeFromValue :: String -> Viewtype
viewtypeFromValue = case _ of
  "fifths" -> Fifths
  "tonnetz" -> Tonnetz
  "lattice" -> Lattice
  "explore" -> Explore
  "pads" -> Pads
  "vary" -> Vary
  _ -> Tonnetz

viewtypeMenuLabel :: Viewtype -> String
viewtypeMenuLabel = case _ of
  Fifths -> "circle of fifths"
  Tonnetz -> "tonnetz"
  Lattice -> "voice-leading lattice"
  Explore -> "explore"
  Pads -> "banks (freedom × complexity)"
  Vary -> "vary a chord (drift × density)"

browseOptions :: Array { value :: String, label :: String }
browseOptions = map (\vt -> { value: viewtypeValue vt, label: viewtypeMenuLabel vt }) viewtypes

-- | The projection a Stage names, or `fallback` when it isn't Hunt. Feeds
-- | `lastLens`, so leaving Hunt and coming back returns to the same projection.
huntOr :: Viewtype -> Stage -> Viewtype
huntOr fallback = case _ of
  Hunt vt -> vt
  _ -> fallback

-- | Is this stage Hunt (in any projection)? The stage tab's active test.
isHunt :: Stage -> Boolean
isHunt = case _ of
  Hunt _ -> true
  _ -> false

-- | The URL segments for a stage: `["hunt","tonnetz"]`, `["perform"]`,
-- | `["review"]`. Vetula owns this vocabulary — `Triggerfish.Route` carries the
-- | segments opaquely and never learns what a stage is.
stagePath :: Stage -> Array String
stagePath = case _ of
  Hunt vt -> [ "hunt", viewtypeValue vt ]
  Rehearse -> [ "rehearse" ]
  Perform -> [ "perform" ]
  Review -> [ "review" ]

-- | The inverse. `Nothing` for anything unrecognised, so a stale or hand-typed
-- | URL leaves the app where it is rather than dumping it somewhere arbitrary.
-- | A bare `["hunt"]` (no projection) is legal and lands on `lastLens`, which is
-- | why the caller passes it in.
stageFromPath :: Viewtype -> Array String -> Maybe Stage
stageFromPath fallbackLens segs = case segs of
  [ "perform" ] -> Just Perform
  [ "review" ] -> Just Review
  [ "rehearse" ] -> Just Rehearse
  [ "hunt" ] -> Just (Hunt fallbackLens)
  [ "hunt", vt ] -> Just (Hunt (viewtypeFromValue vt))
  _ -> Nothing

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

-- | Inverse of `layerLabel` — the palettes multiselect hands back these strings.
layerFromLabel :: String -> Maybe ColorLayer
layerFromLabel s = find (\l -> layerLabel l == s) allColorLayers

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
  , paletteSelect :: MultiSelect.Slot Unit
  , viewSelect :: Select.Slot Unit
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
  -- | The mini-notation sequence over this token's own chord indices, or `""`
  -- | for the default one-a-beat reading. Carried on the TOKEN so a hand-off
  -- | from Rehearse arrives self-describing: a shortlist of approved readings is
  -- | `"<[0 1 2 3] [0 1 4 3]>"`, and a whole lattice is `"0 1 <3 4 5> 2"`.
  -- | `PerfDropBox` copies it into the box's `seqText`, which a scene already
  -- | persists — so the pattern survives from here to a saved scene.
  , pattern :: String
  }


-- | **Variations KEPT for one chord** — the selection pool the Vary lens feeds.
-- |
-- | Browsing a nine-cell grid is not composing, so a Vary pad sounds without
-- | touching the chyron; shift-click is what says *keep this one*. The kept set
-- | is therefore small and deliberate, where the trace would have been 144 near
-- | identical chords deep after a minute of listening.
-- |
-- | Keyed by the source chord's exact NOTES rather than its id. Every lens but
-- | Explore recomputes its chords on each render, so an id is good only until
-- | the next frame; the notes are what the chord actually is, so a kept set
-- | survives leaving the lens and coming back to the same chord.
type KeptFor =
  { notes :: Array Int         -- the source chord, exactly as voiced
  , label :: String
  , options :: Array ChordNode -- what this slot may sound; option ZERO is the
                               -- source chord itself
  }

-- | **The source chord is option zero, always.**
-- |
-- | Keeping it inside the set rather than beside it makes every later question
-- | uniform: locking to the original is not a special case, a slot with nothing
-- | kept is a one-element set that multiplies to 1 rather than 0, and forgetting
-- | every variation leaves a well-formed slot instead of a hole. It also says
-- | the true thing — when the progression runs, the chord you started with is a
-- | legitimate choice unless you have decided otherwise.

-- | One approved path, as the notes it sounds — one array per slot.
type MarkedPath = Array (Array Int)

-- | **What is open UNDER the progression.**
-- |
-- | One at a time, and peers: each answers a different question about the same
-- | chords. `Vary` finds alternatives for one slot, `Paths` lays out every way
-- | through the ones you have. A hand-tweak panel joins them here rather than
-- | anywhere else, which is the point of naming the axis.
data Pane = PaneVary Int | PanePaths

derive instance eqPane :: Eq Pane

-- | A PERFORM box: one persistent player slot on the Perform surface, bound to a
-- | MIDI channel. A dropped token LOOPS through its function `stack` (folded over
-- | the chord pattern) while the transport plays, out its terminal `term`. Empty or
-- | muted boxes are silent; a rig-only terminal is silent+ghosted in Solo.
type PerfBox =
  { cardId  :: Int        -- the card's stable number (`v3`): its name on the stage and
                          -- in Limulus (docs/kb/plans/text-on-the-stage.md); the
                          -- smallest free, kept when another card is deleted
  , channel :: Int
  , label   :: String
  , seq     :: Maybe SavedSeq
  , stack   :: Array Layer   -- ordered function layers (fx + when clause); arp/strum
                             -- among them carry the chord→time realisation (block = none)
  , seqText :: String     -- the TEXT HATCH: a mini-notation sequence over the token's
                          -- chord indices (cycle = 1 bar). "" = default (one/beat).
  , muted   :: Boolean    -- silence this pipeline without tearing it down
  , term    :: PerfTerm   -- the terminal sink: → midi | → odo | → rig
  , phrase  :: Maybe PhraseAttach  -- a captured phrase as the source (#27), in place of
                                   -- chords; when present it drives `base` and the box
                                   -- plays on the bar grid. Nothing = chord-sourced.
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
  -- The cards as the rig's stage holds them, as far as this page knows (card id →
  -- its line); Nothing until the stage has answered a subscribe. See Vetula.StageCards.
  , stageCards :: Maybe (Map Int String)
  -- Vetula's key as last written to the stage (`vetula/key`, Reef.Route.printKey),
  -- which the router's `vetula key` row feeds Odonus's grid from.
  , stageKey :: Maybe String
  , clockTempo :: Number          -- the clock's live tempo, read each tick (drives note durations)
  , nextVoiceId :: Int
  , routing :: Map String Int   -- name → canonical MIDI channel, pushed from the Tidal page
  -- Tank model (Slice A): the durable, unordered collection of CAUGHT chords.
  -- Frozen `Specimen`s reference no lattice node, so the volatile lattice can
  -- reflow/regenerate underneath without disturbing them. `k` over a chord catches
  -- it here; the tank persists until cleared and will feed the Stage + Sequences.
  , tank :: Array Specimen
  , nextSpecId :: Int             -- running number for minting SpecimenIds
  , stage :: Stage                -- Hunt <projection> | Perform | Review — see `Stage`
  , lastLens :: Viewtype          -- the Hunt projection to return to from Perform/Review
  -- Geometric-lens viewport (CoF / Tonnetz): pan centre + zoom, applied as the
  -- surface's viewBox. Wheel zooms toward the cursor; drag pans; reset re-fits.
  , viewCx :: Number
  , viewCy :: Number
  , viewZoom :: Number
  , panning :: Maybe { ux :: Number, uy :: Number }  -- grabbed anchor point in user-space
  , panMoved :: Boolean            -- a real drag happened → swallow the ensuing click
  , genRoll :: Int                 -- Generate lens: the "shake" counter (re-rolls relatives)
  -- Banks lens: the shuffle number. ONE Int regenerates all nine banks, because
  -- each is a `Harmonia.Progression.Spec` whose seed is fanned out from this.
  , padRoll :: Int
  , varyRoll :: Int
  , kept :: Array KeptFor
  -- REHEARSE: a progression with alternatives at each slot. Empty = nothing
  -- taken up yet, and the stage offers the saved tokens to start from.
  , rehearsal :: Array Slot
  , rehearseRoll :: Int       -- the seed of the current pass; a pass is an address
  , rehearsalFrom :: Maybe Int -- which saved token is up, so the shelf can show it
  -- **Paths you approved.** Held by CONTENT, not by index: dropping an option
  -- renumbers every index above it, and a mark that silently re-pointed at its
  -- neighbour would be the worst kind of wrong — a decision you made, recorded
  -- against a chord you did not choose. Content survives any edit.
  --
  -- Not regenerable, which is why it is stored at all: the lattice can produce
  -- every path, but which ones you liked exists nowhere else.
  , marked :: Array MarkedPath
  , pull :: HT.Pull           -- how hard the previous chord pulls on the next choice
  -- What is open under the progression, if anything.
  , pane :: Maybe Pane
  -- The chord the Vary lens is working on. `Nothing` falls back to whatever is
  -- sounding, so the lens is never empty for no reason.
  , varying :: Maybe ChordNode
  -- The hovered PAD, carried whole. `hoveredTriad` would be enough to highlight
  -- it, but not to preview it: that path re-voices from pitch classes, and a
  -- pad's open voicing (bass pinned to the root, contour smoothed against its
  -- neighbours in the bank) is exactly what must NOT be thrown away — space and
  -- click have to sound the same chord.
  , hoveredNode :: Maybe ChordNode
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
  -- The chip index currently being dragged to REORDER the buffer (Nothing = no
  -- drag in flight). Reordering makes the buffer a list, not a tape (§8): order,
  -- not timestamps, becomes the arrangement.
  , chyronDrag :: Maybe Int
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
  -- The session/scene command menu in the secondary nav (the ⋯ dropdown off the
  -- session badge) — session + scene + chyron housekeeping, moved off the Perform
  -- header. (AC, 2026-08-03.)
  , perfMenuOpen :: Boolean
  -- The shared MIDI clip library (#27), loaded from `Triggerfish.Clips.Store` in
  -- Initialize — the pool the phrase picker offers. `perfPhrasePick` is the box index
  -- whose picker is open (Nothing = closed).
  , clipLibrary :: Array MidiClip
  , perfPhrasePick :: Maybe Int
  -- The always-on capture logbook (#28): every note the voices/boxes emit is tapped
  -- in PerfTick and appended here (the "player piano" roll), shown live beside the
  -- voices in PERFORM and given the whole surface in REVIEW, where a phrase is
  -- lifted into the shared clip library. Which of those you see is `stage`; there
  -- is no second flag (the old `captureView` folded into `Stage`).
  , capture :: CaptureState
  -- The rig keeps the marks and plays the loops (Capture.RigLoops): true once
  -- it has said so, and from then this page holds no loop of its own.
  , rigLoops :: Boolean
  -- when it last asked (perf ms): until the rig answers it asks every two
  -- seconds, since a request sent before the socket opens is lost
  , rigAsked :: Number
  -- the document listeners of a ✂ drag across the Review surface
  , captureDragSub :: Maybe H.SubscriptionId
  -- The LIVE river's two reads (`Capture.River`): the current instant, advanced by
  -- a 33ms frame timer so the roll FLOWS rather than jumping a 16th at a time, and
  -- the recent notes it draws — pruned to the river's fade span each frame. The
  -- logbook keeps everything; this is just the window that's on screen.
  , nowMicros :: Number
  , riverNotes :: Array NoteEvent
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
  | SetLayers (Array String) -- set the active layers from the palettes multiselect
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
  | PerfMenuToggle         -- open/close the session/scene command menu (nav ⋯)
  | PerfMenuClose          -- close it (backdrop click, or after picking an item)
  | PerfMenuPick Action    -- close the menu, then run the picked command
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
  | DeleteSpec SpecimenId  -- × a tank specimen
  | AuditionSpec SpecimenId -- shift-click a tank specimen: hear it (no state change)
  | StageSpec SpecimenId   -- click a tank specimen: seed the pool with it (toggle)
  | SequenceSpec SpecimenId -- shift-click a tank specimen: append a snapshot to the progression
  | ArrangeSpec SpecimenId  -- drop a tank chord into the progression, bridged by `bridgeLen`
  | SetBridgeLen Int        -- set the cadence-length dial (clamped 0..maxBridge)
  | ToggleFold VPanel       -- collapse/expand a floating control to just its header
  | ClearStage             -- remove all staged seeds + the chords bloomed from them
  | AuditionTriad Int (Array Int)        -- Tonnetz: hear a triad off the net (root pc, pcs)
  | AuditionNode ChordNode               -- Lattices: hear a generated chord (its own voicing)
  -- Chyron: hover a chip (space auditions it), or click one — plain click selects
  -- a single chord, shift-click extends the range from the anchor (Mac semantics).
  | HoverChyron (Maybe Int)
  | ChyronClick Int Boolean
  | DeleteChyron Int       -- × a single audition out of the trace
  | ClearChyron            -- wipe the whole audition trace
  | DedupeChyron           -- drop repeat chords (same pcs), keeping first occurrence
  | SaveChyronSel          -- compress the selection into a pinned 2-glyph token
  | ChyronDragStart Int    -- begin dragging chip i to reorder the buffer
  | ChyronDropOn Int       -- drop the dragged chip before chip i (reorder)
  | ChyronDragEnd          -- drag ended (clear the in-flight index)
  | PlaySaved Int          -- replay a pinned saved sequence (with its timing)
  | DeleteSaved Int        -- × a pinned saved sequence
  | Unbundle Int           -- open a saved token back into the working buffer (§6)
  | ToggleChyronArm        -- record-arm the chyron on/off
  -- PERFORM surface
  | PerfPickup Int         -- pick up saved token i for placement (toggle)
  | PerfDropBox Int        -- place the held token/fx onto box i
  | PerfClearBox Int       -- empty box i (stop its loop)
  | PerfAddBox             -- append a new empty player on the next free MIDI channel
  | PerfRemoveBox Int      -- delete player i outright (not just empty it)
  | StageOpen              -- the rig socket (re)connected: subscribe to the cards on the stage
  | StageFrameIn String    -- a frame from the rig; the stage's card frames are acted on
  | CardToLimulus Int      -- ask Limulus to show card n (`stage-open vetula/vN`)
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
  | PerfOpenPhrasePick Int -- open the clip-library picker for box b (#27)
  | PerfClosePhrasePick    -- close the phrase picker
  | PerfAttachPhrase Int MidiClip -- attach a clip (copy) as box b's phrase source
  | PerfDetachPhrase Int   -- drop box b's phrase source (back to chords)
  | PerfPhraseMode Int ChannelMode -- box b: flatten-to-channel vs original-source-channels
  | PerfPhraseMuteHead Int Int     -- box b: toggle mute of source head h in the phrase
  -- Clip-library management (#33), operating on the SHARED store rather than a box: an
  -- attached box holds a self-contained copy, so rename/delete here never touch a voice.
  | ClipAudition MidiClip  -- play a library clip once, faithfully (own channels/vel/gate)
  | ClipRename String String -- rename library clip by id (commit on blur), persist
  | ClipDelete String      -- remove library clip by id, persist
  | ClipShare MidiClip     -- ◴ declare a captured clip to Quadrat, as a phrase
  -- Capture band (#28): the always-on player-piano roll in the lower third.
  | CaptureMark            -- flag "the last couple of bars" as a good bit
  | CaptureRegionSelect Int -- click a gold band → show its lift card
  | CaptureStopSel         -- dismiss the lift card
  | CaptureSaveClip Int    -- lift region i out into the shared clip library
  | CaptureToggleContext   -- show/hide the region's harmonic context
  | CaptureToggleCode      -- show/hide the region's mark as code
  | CaptureToLimulus Int   -- hand mark i, as code, to Limulus (stage-paste)
  | CaptureZoom Zoom       -- whole / last N / crop to a loop
  | CaptureClear           -- purge the capture logbook
  | CaptureCutArm          -- ✂: the next drag across the surface selects a stretch to cut
  | CaptureCutDown Int Int -- the cut's drag starts: clientX, clientY
  | CaptureCutMove Int Int -- it moves
  | CaptureCutUp           -- it ends: the rig cuts the stretch
  | CaptureTrim            -- cut all but the marks' windows (on the rig)
  | CaptureUndo            -- put back the last cut or trim (on the rig)
  | CaptureFrame           -- 33ms tick: advance the river's clock, prune its window
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
  | ShufflePads            -- Banks lens: re-walk all nine banks
  | HoverPad (Maybe ChordNode)  -- Banks lens: hover a pad (highlight + exact preview)
  | DropTone Event Int Int Int  -- silence chord `id`'s tone `i` at octave `k` (shift-click a note)
  | ShuffleVary            -- re-draw all nine cells of the Vary lens from a new seed
  | VaryAudition ChordNode -- hear a variation WITHOUT capturing it to the chyron
  | KeepVariation ChordNode -- shift-click: keep (or un-keep) a variation for this chord
  | ForgetKept Int         -- drop a whole chord's kept set
  -- REHEARSE
  | TakeUp Int             -- build a rehearsal from saved token i
  | DropRehearsal          -- put the whole rehearsal down
  | RollPass               -- draw a new pass through the lattice
  | PlayPass               -- hear the current pass
  | SetPull HT.Pull        -- loose / mid / smooth
  | HearOption Int Int     -- hear slot i's option j (without capturing)
  | HearAround Int ChordNode   -- hear it BETWEEN its neighbours, as the pass stands
  | SweepAround Int ChordNode  -- hear it against every way the neighbours could go
  | ShowPaths              -- lay out every way through the chords, smoothest first
  | HearPath (Array Int)   -- hear one of them
  | MarkPath (Array Int)   -- approve (or un-approve) one of them
  | KeepPass               -- mint the current pass onto the shelf as a progression
  | KeepMarked             -- mint the marked shortlist as ONE alternating token
  | KeepLattice            -- mint the whole lattice, each slot alternating
  | ToQuadrat Int          -- publish shelf token i as a clip for Quadrat to sample
  | SettleSlot Int Int     -- lock slot i to option j (or unlock if already it)
  | SettlePass             -- lock every slot to what this pass chose
  | LoosenAll              -- unlock every slot
  | DropOption Int Int     -- remove slot i's option j (never option zero)
  | VaryFromSlot Int       -- excurse to the Vary lens on slot i's base chord
  | BackToRehearsal        -- return from that excursion
  | OpenVary ChordNode     -- send a chord to the Vary lens and go there
  | RollBass Int           -- roll the revoiced chord's bass to the next/previous chord tone
  | ShiftOctave Int        -- move the revoiced chord bodily up/down an octave
  | PlaceTone Event Int Int Int -- put chord `id`'s tone `i` at octave `k` (click a ghost)
  | SetStage Stage         -- switch stage: Hunt <projection> | Perform | Review
  | TransposeSpec SpecimenId Int -- Slice E: shift one tank specimen by n semitones (in place)
  | CapoTank Int           -- Slice E: shift the WHOLE tank by n semitones (a capo)

-- | The queries the Triggerfish shell pulls from Vetula: its current Tidal
-- | source (for the aggregate TIDAL tab) and its current progression as PC sets
-- | (for the Odonus chord-quantiser feed). Defined here (not imported from
-- | Triggerfish) so the standalone app — which never queries it — still builds.
data SourceQuery a
  = AskSource (String -> a)
  -- A mark made rig-wide (Triggerfish.SourceQuery): Vetula as text for a
  -- mark (`markText`), and another machine's text for one of its own marks.
  | AskMarkText (String -> a)
  | AddMarkSnapshot Number String String a
  -- URL routing: adopt the stage named by these path segments (see `stagePath`).
  -- Unrecognised segments are ignored rather than guessed at, so a stale link
  -- switches machine and leaves the stage alone.
  | SetStagePath (Array String) a
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
  -- `SetRestingScale` is where the macro `# scale` verb lands (root pc +
  -- intervals): an override of the key's scale, published as `vetula/key`.
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
-- | `StageChanged` carries the new stage's URL segments so the shell can write
-- | the hash. Push, not poll: the shell would otherwise have to interrogate every
-- | machine on a timer to notice a mode change it didn't cause.
-- | `Marked at`: a mark was made in Review, named by its time, so the shell
-- | can gather the rest of the rig's state for it.
data Output = ArmChanged Boolean | StageChanged (Array String) | Marked Number

component :: forall i m. MonadAff m => H.Component SourceQuery i Output m
component = H.mkComponent
  { initialState: \_ ->
      { key: cMajorKey
      , restScale: Nothing
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
      , stageCards: Nothing
      , stageKey: Nothing
      , clockTempo: 120.0
      , nextVoiceId: 4
      -- name → canonical MIDI channel, pushed from the shell's Tidal-page routing
      -- table (SetRouting). Unnamed / unbound voices fall back to the default channel.
      , routing: Map.empty :: Map String Int
      , tank: []
      , nextSpecId: 0
      , seedChord: Map.empty
      , presets: [], identity: Nothing
      , stage: Hunt Tonnetz   -- default: the tonal net shows the scale's shape best
      , lastLens: Tonnetz
      , viewCx: 0.0
      , viewCy: 0.0
      , viewZoom: 1.0
      , panning: Nothing
      , panMoved: false
      , genRoll: 0
      , padRoll: 0
      , varyRoll: 0
      , kept: []
      , rehearsal: []
      , rehearseRoll: 0
      , rehearsalFrom: Nothing
      , marked: []
      , pull: HT.Mid
      , pane: Nothing
      , varying: Nothing
      , hoveredNode: Nothing
      , chyron: []
      , hoveredChyron: Nothing
      , chyronSel: Nothing
      , chyronDrag: Nothing
      , chyronSaved: []
      , chyronArmed: true
      -- four player boxes on MIDI ch 1-4 (Odonus I-IV in AC's routing); a token
      -- dropped on one loops there while the transport plays.
      , perfBoxes: map (\n -> { cardId: n, channel: n, label: "P" <> show n, seq: Nothing, stack: [], seqText: "", muted: false, term: TMidi, phrase: Nothing }) (range 1 4)
      , perfHeld: Nothing
      , perfHeldFx: Nothing
      , perfDrag: Nothing
      , perfEditBox: Nothing
      -- placeholder; Initialize resumes the persisted session or mints a fresh one
      , perfSession: { alias: "", name: "", nextScene: 1 }
      , perfScenes: []
      , perfRecallOpen: false
      , perfMenuOpen: false
      , clipLibrary: []
      , perfPhrasePick: Nothing
      , capture: { logbook: Logbook.emptyLog, playing: Nothing, regionDrag: Nothing, contextOpen: false, codeOpen: false, zoom: Whole, rig: Nothing, cutting: false, cutSel: Nothing }
      , rigLoops: false, rigAsked: 0.0, captureDragSub: Nothing
      , nowMicros: 0.0
      , riverNotes: []
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
  AskMarkText reply -> do
    s <- H.get
    pure (Just (reply (markText s)))
  AddMarkSnapshot at machine text next -> do
    H.modify_ \s -> s { capture = s.capture { logbook = Logbook.addSnapshot at { machine, text } s.capture.logbook } }
    pure (Just next)
  -- Routed in from the URL. Goes through `handleAction SetStage` rather than
  -- writing `stage` directly, so a link into REVIEW gets the same hush/clear
  -- treatment as clicking the tab — arriving by URL must not be a second, laxer
  -- path into a stage.
  SetStagePath segs next -> do
    s <- H.get
    for_ (stageFromPath s.lastLens segs) \stg ->
      when (stg /= s.stage) (handleAction (SetStage stg))
    pure (Just next)
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
        -- With NO arranged progression the strip used to go blank, even while a box
        -- was plainly sounding chords; it now falls back to the harmonic-context
        -- voice (the `→ odo` box), in the SAME precedence `harmonicContext` uses —
        -- so the nav readout and what Odonus is quantising to can't disagree.
        chord = case cs !! active of
          Just c -> joinWith " " (map noteName (nub (map (\x -> mod x 12) (playNotes c))))
          Nothing -> case odoBoxPcs s of
            Just pcs | length pcs > 0 -> joinWith " " (map noteName pcs)
            _ -> ""
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
      for_ st.binnacle \bin -> liftEffect do
        Transport.send (Binnacle.socket bin) (brushMsg st)
        -- the cards play on the rig, read from the stage (vetula_cards); the page
        -- plays them itself only in Local
        Transport.send (Binnacle.socket bin) "vetula-cards-play"
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
    pure (Just (reply (mapWithIndex (\i p -> { slot: i, alias: (progGlyph p.content).alias, name: fromMaybe "" p.name, starred: p.starred }) s.presets)))
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

-- | What the HARMONIC-CONTEXT VOICE is sounding right now — case 3 of
-- | `harmonicContext`. That's the single box on the `→ odo` terminal: `PerfSetTerm`
-- | keeps the terminal exclusive, so there is one conductor or none, never a blend.
-- | (AC's rule, 2026-08-06: a per-voice feed makes no sense, but ONE voice standing
-- | for the harmonic context does — it's the same call `harmonicVoice` already makes
-- | for the nav chyron.) `head` is belt-and-braces for state predating exclusivity.
-- |
-- | Reads `perfBoxOdoFeed`, which queries each box's OWN pattern — so unlike case 2
-- | it works with no arranged progression, which is the whole point.
-- |
-- | NB `perfBoxOdoFeed` requires `isJust box.seq`, so a box playing a captured PHRASE
-- | doesn't conduct — a recorded phrase has no single block chord to quantise to.
odoBoxPcs :: State -> Maybe (Array Int)
odoBoxPcs st = _.pcs <$> head (perfBoxOdoFeed st)

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

-- | Perform boxes whose terminal is `→ odo` contribute their CURRENT block chord
-- | to the Odonus feed (keyed by the box's channel, reused as the Odonus id) — the
-- | same conductor role a `ToOdonus` voice plays. Muted / ghosted / empty boxes and
-- | non-odo terminals don't feed.
perfBoxOdoFeed :: State -> Array { id :: Int, pcs :: Array Int }
perfBoxOdoFeed st =
  mapMaybe
    (\box ->
       if box.term == TOdo && not box.muted && isJust box.seq && not (boxGhosted st.authority box)
         then case boxCurrentChord box st.pulse of
                Just notes | length notes > 0 -> Just { id: box.channel, pcs: nub (map (\x -> mod x 12) notes) }
                _ -> Nothing
         else Nothing)
    st.perfBoxes

-- | The chord a box is sounding AT `pulse` — the digital event under the PLAYHEAD.
-- |
-- | Was: the first digital event of a whole cycle `[b, b+1)`, with `b` a beat index.
-- | That is constant for a given pattern — `head` of a full-cycle query always
-- | returns the pattern's FIRST chord — so a box on `→ odo` conducted one frozen
-- | chord for ever while its MIDI plainly moved. Latent since Perform slice 4
-- | (2026-08-02): nothing consumed the feed until `harmonicContext` case 3 did
-- | (2026-08-06), which is when it became visible. Fixed 2026-08-06.
-- |
-- | A seq box's pattern cycles once per BAR — `PerfTick` schedules it on
-- | `tick.index mod 16 == 0` — so pulse p sits at cycle position p/16, and we ask
-- | for the one-pulse window there rather than the whole bar.
boxCurrentChord :: PerfBox -> Int -> Maybe (Array Int)
boxCurrentChord box pulse =
  let p = max 0 pulse
      pos = p % pulsesPerBar
      nxt = (p + 1) % pulsesPerBar
  in eventValue <$> head (filter isDigital (query (boxPattern box) (mkState (mkArc pos nxt))))

-- | Scheduler pulses in one bar: the 16th-note grid `PerfTick` runs on, and one
-- | full cycle of a box's mini-notation sequence.
pulsesPerBar :: Int
pulsesPerBar = 16

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
handleAction a = do
  before <- H.gets _.perfBoxes
  handleActionCore a
  after <- H.gets _.perfBoxes
  -- the cards changed (any edit, by hand or from the stage): publish what differs
  unless (unsafeRefEq before after) publishCards
  publishKey

-- | Write Vetula's key to the stage when this page's key changes. `stageKey`
-- | is the key the page last stood in as far as the stage goes: what it wrote,
-- | or, when it subscribed to a stage that already had a key, the one it had
-- | then. So opening a page does not overwrite the key another page (or
-- | Limulus) set; it writes only when the stage has none, or when its own key
-- | is changed.
publishKey :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
publishKey = do
  st <- H.get
  let ctx = contextKey st
      text = Route.printKey ctx
  when (isJust st.stageCards && st.stageKey /= Just text) do
    for_ st.binnacle \bin -> do
      liftEffect $ Transport.send (Binnacle.socket bin) ("stage-text vetula/key " <> text)
      H.modify_ _ { stageKey = Just text }

-- | The key Vetula stands in: the resting scale if one is set, else the key.
contextKey :: State -> { root :: Int, offsets :: Array Int }
contextKey st = case st.restScale of
  Just rs -> { root: rs.root, offsets: rs.offsets }
  Nothing ->
    { root: mod st.key.tonic 12
    , offsets: map (\pc -> mod (pc - st.key.tonic + 12) 12) (scaleSet st.key)
    }

-- | Vetula as text for a mark: its key, then each card as the line Limulus
-- | edits (`v3 $ ch3 "…" "…" # …`), so a mark hands Limulus code it can
-- | evaluate (docs/kb/plans/the-deck.md).
markText :: State -> String
markText st =
  joinWith "\n\n"
    ( [ "-- vetula key " <> Route.printKey (contextKey st) ]
        <> map (\(Tuple n line) -> "v" <> show n <> " $ " <> line) (Map.toUnfoldable (cardTexts st.perfBoxes))
    )

-- | Bring the stage's copy of the cards up to date with the page's.
publishCards :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
publishCards = do
  st <- H.get
  for_ st.stageCards \seen -> for_ st.binnacle \bin -> do
    let now = cardTexts st.perfBoxes
    liftEffect $ for_ (SC.publishLines seen now) (Transport.send (Binnacle.socket bin))
    H.modify_ _ { stageCards = Just now }

cardTexts :: Array PerfBox -> Map Int String
cardTexts boxes = Map.fromFoldable (map (\b -> Tuple b.cardId (printCard (boxSpec b))) boxes)

-- | A card read from the stage as a box: the page's own box for that card, if
-- | it has one, with what the line says; its token kept when the chords match,
-- | so the glyph survives an edit that did not touch them.
boxOfCard :: Int -> VoiceSpec -> Maybe PerfBox -> PerfBox
boxOfCard n spec old =
  { cardId: n
  , channel: spec.channel
  , label: "P" <> show spec.channel
  , seq: case old >>= _.seq of
      Just s | map _.notes s.events == spec.chords -> Just s
      _ -> if length spec.chords == 0 then Nothing else Just (mkSavedSeq spec.chords)
  , stack: spec.stack
  , seqText: spec.seqText
  , muted: spec.muted
  , term: spec.term
  , phrase: old >>= _.phrase
  }

handleActionCore :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleActionCore = case _ of
  StageOpen -> do
    H.modify_ _ { stageCards = Nothing, stageKey = Nothing }
    st <- H.get
    for_ st.binnacle \bin -> liftEffect do
      Transport.send (Binnacle.socket bin) SC.subscribeLine
      -- and the marks and loops the rig keeps (RigLoops)
      Transport.send (Binnacle.socket bin) RL.syncLine
  -- The marks and loops the rig keeps (RigLoops): this surface draws them,
  -- and takes its own loop off, since the rig plays them now. A mark it has
  -- not met (made from Limulus, or by ◆ a moment ago) takes Vetula's text
  -- now; one from before this page opened has none to show.
  StageFrameIn msg | RL.readClear "vetula" msg -> do
    hushCapture
    H.modify_ \s -> s { capture = s.capture { logbook = Logbook.emptyLog, playing = Nothing, contextOpen = false, codeOpen = false } }
  StageFrameIn msg | isJust (stripPrefix (Pattern "loops-notes ") msg) -> do
    mclock <- vetulaClock
    for_ mclock \clock -> for_ (RL.readNotes "vetula" clock msg) \r ->
      H.modify_ \s -> s { capture = s.capture { logbook = RL.seedNotes r s.capture.logbook } }
  StageFrameIn msg | Just rig <- RL.readLoops "vetula" msg -> do
    mclock <- vetulaClock
    for_ mclock \clock -> do
      st <- H.get
      when (isJust st.capture.playing) hushCapture
      let r = RL.reconcile clock rig st.capture.logbook.marks
      H.modify_ \s -> s { rigLoops = true, capture = s.capture { playing = Nothing, rig = Just clock, logbook = s.capture.logbook { marks = r.marks } } }
      -- the first word from the rig: ask it for what was played before this page
      unless st.rigLoops $ rigSend (RL.notesLine "vetula")
      unixMs <- liftEffect dateNow
      for_ r.fresh \rm -> do
        s <- H.get
        let recent = unixMs * 1000.0 - rm.us < 2.0e6
            m = { atMicros: RL.microsOf clock rm.beat, beat: rm.beat
                , from: RL.microsOf clock rm.from, to: RL.microsOf clock rm.to
                , patch: if recent then markText s else "", now: "", sounding: Nothing
                , rig: [], tempo: clock.tempo, n: rm.n, id: rm.id
                , loop: if rm.playing then rm.start else Nothing }
        H.modify_ _ { capture = s.capture { logbook = s.capture.logbook { marks = RL.insertMark m s.capture.logbook.marks } } }
        when recent $ H.raise (Marked m.atMicros)
      st2 <- H.get
      when (any RL.looping st2.capture.logbook.marks && not (any RL.looping st.capture.logbook.marks) && st2.stage /= Review)
        (handleAction (SetStage Review))
  -- The notes the rig played for the cards (`vetula-notes`, Unix µs): into the
  -- Review logbook, as the page's own notes go in Local, so marks and loops work.
  -- Logged whatever this page's authority: these notes did sound.
  StageFrameIn msg | Just notes <- SC.readNotes msg -> do
    perfMs <- liftEffect perfNow
    unixMs <- liftEffect dateNow
    let offsetUs = (unixMs - perfMs) * 1000.0
        fresh = map (\n -> { pitch: n.pitch, headIdx: n.ch, fireUnixMicros: n.atUs - offsetUs, vel: n.vel, gateMs: n.gateMs }) notes
    -- the logbook (Review) and the river (Perform's live strip), as the page's
    -- own notes go to both
    H.modify_ \s -> s { capture = s.capture { logbook = Logbook.logAppend (perfMs * 1000.0) fresh s.capture.logbook }
                      , riverNotes = fresh <> s.riverNotes }
  StageFrameIn msg -> do
    for_ (SC.tableHasKey msg) \has -> when has do
      st <- H.get
      H.modify_ _ { stageKey = Just (Route.printKey (contextKey st)) }
    case SC.readFrame msg of
      Nothing -> pure unit
      -- the stage has no cards (a fresh rig): it gets ours
      Just (SC.Table table) | Map.isEmpty table -> do
        H.modify_ _ { stageCards = Just Map.empty }
        publishCards
      -- the stage has cards: they are the current ones (another tab, Limulus, or
      -- this page before a reload); adopt them, in card order
      Just (SC.Table table) -> do
        st <- H.get
        let
          readable = Map.toUnfoldable table # mapMaybe \(Tuple n text) ->
            (\spec -> Tuple n (boxOfCard n spec (find (\b -> b.cardId == n) st.perfBoxes))) <$> parseCard text
        H.modify_ _ { perfBoxes = map snd readable, stageCards = Just table }
      Just (SC.Written n Nothing) ->
        H.modify_ \s -> s { perfBoxes = filter (\b -> b.cardId /= n) s.perfBoxes
                          , stageCards = map (Map.delete n) s.stageCards }
      Just (SC.Written n (Just text)) -> do
        st <- H.get
        H.modify_ _ { stageCards = map (Map.insert n text) st.stageCards }
        case parseCard text of
          -- unreadable: refuse it; the publish that follows puts the card back
          Nothing -> do
            for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin)
              (SC.rejectLine n "Vetula could not read this card (want: chN \"<[c4,e4,g4] …>\" \"0 1 2 3\" # layer …)")
            publishCards
          Just spec -> H.modify_ \s -> s
            { perfBoxes = case find (\b -> b.cardId == n) s.perfBoxes of
                Just old -> map (\b -> if b.cardId == n then boxOfCard n spec (Just old) else b) s.perfBoxes
                Nothing -> s.perfBoxes <> [ boxOfCard n spec Nothing ] }
  CardToLimulus n -> do
    publishCards
    st <- H.get
    for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) (SC.openLine n)
  Initialize -> do
    -- Announce the opening stage so the shell can write a COMPLETE URL from a cold
    -- start (`#{slug}/{stage}`, not the bare `#{slug}`). Without this the address
    -- bar under-specifies until you touch a stage tab — still a valid route, since
    -- an empty stage path means "leave the stage alone", but not a link that
    -- reopens what you were actually looking at.
    H.gets _.stage >>= \stg -> H.raise (StageChanged (stagePath stg))
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
    -- The LIVE river's animation clock (33ms ≈ 30fps, the same cadence Odonus's
    -- scope runs at). The handler no-ops off the Perform surface, so this costs
    -- nothing while you're on the tonnetz.
    { emitter: frameE, listener: frameL } <- liftEffect HS.create
    _ <- H.subscribe frameE
    _ <- liftEffect $ setInterval 33 (HS.notify frameL CaptureFrame)
    H.modify_ _ { binnacle = Just bin }
    -- The cards on the rig's stage (docs/kb/plans/text-on-the-stage.md): subscribe
    -- on every connect, since a rig restart empties the stage.
    { emitter: stageE, listener: stageL } <- liftEffect HS.create
    _ <- H.subscribe stageE
    liftEffect $ Binnacle.onAppMessage bin (HS.notify stageL <<< StageFrameIn)
    liftEffect $ Binnacle.onOpen bin (HS.notify stageL StageOpen)
    -- Restore the persisted library (auto-capture stack) from localStorage. capSeq
    -- continues past the restored count so new ◦ autonames don't collide.
    msaved <- liftEffect Store.loadLibrary
    for_ msaved \sv -> H.modify_ _ { library = sv.library, capSeq = length sv.library, presets = sv.presets }
    -- Load the shared MIDI clip library (#27) — the pool the phrase picker offers.
    savedClips <- liftEffect ClipStore.loadClips
    H.modify_ _ { clipLibrary = savedClips }
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

  -- A pad sets BOTH: the triad hover drives the glyph highlighting every lens
  -- shares, the node drives the preview, which needs the voicing the triad
  -- reading has already discarded.
  HoverPad mc -> H.modify_ _
    { hoveredNode = mc
    , hoveredTriad = map (\c -> { root: c.root, pcs: c.pcs }) mc
    }

  -- entering a tank tile sets the hovered specimen (space previews it); leaving
  -- clears it. Also clears any surface hover so space can't fall back to a stale
  -- pool bubble while the pointer is over the tank.
  HoverSpec ms -> H.modify_ _ { hoveredSpec = ms, hoveredId = Nothing, hoveredTriad = Nothing, hoveredNode = Nothing }

  -- Slice 4c: hand-override the width-focus (the Hunt/Perform toggle, the pool spine).
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
    -- What to revoice, most direct first: the pad under the pointer (Banks),
    -- then the pool bubble under it, then whatever is sounding.
    let candidate = case st.hoveredNode of
          Just c -> Just c
          Nothing -> case st.hoveredId >>= \hid -> find (\c -> c.id == hid) st.chords of
            Just c -> Just c
            Nothing -> st.sounding >>= \sid -> find (\c -> c.id == sid) st.chords
    for_ candidate \c ->
      -- **Every lens but Explore recomputes its chords on each render**, so most
      -- of what you can click — a lattice member, a colour-layer chord, a Banks
      -- pad — is not in `st.chords` and has no id the modal can hold. Addressing
      -- the modal by pool id therefore meant `v` silently fell back to the last
      -- SOUNDING pool chord, which from a cold start is the home chord: the
      -- modal looked like it always opened C.
      --
      -- So catch it. A chord you have decided to revoice is one you are working
      -- on, and the pool is the hunting ground. Matched on CONTENT first, so
      -- re-opening the same chord reuses its entry instead of piling up twins.
      case find (\d -> d.bassPc == c.bassPc && d.bassOct == c.bassOct && d.voicing == c.voicing) st.chords of
        Just existing -> H.modify_ _ { revoicing = Just existing.id, sounding = Just existing.id, selected = Nothing }
        Nothing -> do
          let caught = place st.key c (c { id = st.nextId, isCentre = false, pinned = false })
          applyChords (st.chords <> [ caught ])
          H.modify_ _
            { nextId = st.nextId + 1
            , revoicing = Just caught.id
            , sounding = Just caught.id
            , selected = Nothing
            }

  CloseRevoice -> H.modify_ _ { revoicing = Nothing }

  -- a slash chord: set the revoiced chord's bass to a chosen pitch class (same
  -- upper notes, different foundation) — a voicing decision, kept in the modal.
  -- Inversion as a single gesture, and a REAL one: the lowest voice rolls up an
  -- octave (or the highest down) and the bass follows it. Re-footing alone —
  -- what the slash row does — leaves C·E·G over E, which is a slash chord and
  -- not a first inversion. The upper structure has to move.
  RollBass dir -> do
    st <- H.get
    for_ st.revoicing \cid ->
      for_ (find (\c -> c.id == cid) st.chords) \c -> do
        let chords' = map (\d -> if d.id == cid then invertNode dir c else d) st.chords
        applyChords chords'
        for_ (find (\d -> d.id == cid) chords') playChord

  -- Bass AND uppers together. Shifting only the uppers spreads a chord; it does
  -- not transpose it, which is why this needed `bassOct` to exist first.
  ShiftOctave d -> do
    st <- H.get
    for_ st.revoicing \cid ->
      for_ (find (\c -> c.id == cid) st.chords) \c -> do
        let chords' = map (\e -> if e.id == cid then octaveShift d c else e) st.chords
        applyChords chords'
        for_ (find (\e -> e.id == cid) chords') playChord

  SlashBass pc -> do
    st <- H.get
    for_ st.revoicing \cid -> do
      let chords' = map (\c -> if c.id == cid then refootNode pc c else c) st.chords
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

  -- The palettes MultiSelect hands back the full new selection as value strings
  -- (each a `layerLabel`); rebuild the layer Set from them.
  SetLayers vs -> H.modify_ _ { colorLayers = Set.fromFoldable (mapMaybe layerFromLabel vs) }

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

  -- Omission — the axis the ladder never had, and the one that makes the five- and
  -- six-note chords of the Banks lens playable. Shift-click a note to drop it,
  -- click its ghost to bring it back.
  --
  -- A restored tone returns to its STACK POSITION, not to where it was, because
  -- omitting genuinely discards that: `Place []` holds no octave. The ghost is
  -- therefore drawn at the position it will return to, so the gesture is honest
  -- rather than surprising.
  DropTone ev cid i k -> do
    -- A progression row handles its own click (play / arm pick mode), and this
    -- gesture lives on a dot INSIDE that row — so it has to be stopped here, or
    -- dropping a note would also select the step.
    liftEffect (stopPropagation ev)
    st <- H.get
    for_ (find (\c -> c.id == cid) st.chords) \c -> do
      let c' = applyToNode c (OV.dropAt i k (spreadOfNode c))
          chords' = map (\d -> if d.id == cid then c' else d) st.chords
      applyChords chords'
      H.modify_ _ { sounding = Just cid }
      playChord c'

  -- Restore an omitted tone AT THE OCTAVE CLICKED. The ghosts stand at every
  -- octave the tone could occupy, so bringing a dropped note back where you
  -- want it is one gesture rather than restore-then-drag.
  PlaceTone ev cid i k -> do
    liftEffect (stopPropagation ev)
    st <- H.get
    for_ (find (\c -> c.id == cid) st.chords) \c -> do
      let c' = applyToNode c (OV.setTone i (OV.at k) (spreadOfNode c))
          chords' = map (\d -> if d.id == cid then c' else d) st.chords
      applyChords chords'
      H.modify_ _ { sounding = Just cid }
      playChord c'

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
  ClearPath -> H.modify_ _ { path = [] }

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
    H.modify_ _ { path = [], perfName = Nothing, voices = [], playing = false }
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

  PerfMenuToggle -> H.modify_ \st -> st { perfMenuOpen = not st.perfMenuOpen }
  PerfMenuClose -> H.modify_ _ { perfMenuOpen = false }
  PerfMenuPick act -> do
    H.modify_ _ { perfMenuOpen = false }
    handleAction act

  -- Parse a stored scene payload (the `vetulaScene { … }` record) back into a
  -- document and reconstruct the surface's boxes. Lenient: a payload that yields
  -- no voices is left as a note rather than blanking the surface.
  -- Load a scene onto the Perform surface AND surface its progressions in the
  -- chyron: each named source becomes a saved 2-glyph token (its content glyph),
  -- so a recalled scene's chord sets are right there to replay or unbundle for
  -- editing — closing the save→recall→edit loop (DESIGN-tank-overhaul.md §6). One
  -- token per distinct source preserves the multi-source separation (a voice's
  -- substitution-sibling or different-key set stays its own token). The saved
  -- region belongs to the loaded document, so it REPLACES what was there; the live
  -- capture buffer is left untouched.
  PerfLoadScene payload -> do
    let doc = parsePerform payload
        boxes = boxesFromDoc doc
        tokens = map (\s -> mkSavedSeq s.chords) doc.sources
    if length boxes == 0
      then H.modify_ _ { perfRecallOpen = false, publishMsg = Just "✗ couldn't read that scene" }
      else H.modify_ _ { perfBoxes = boxes, chyronSaved = tokens
                       , perfRecallOpen = false, publishMsg = Just "scene loaded" }

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
                   , bass: bassMidi node
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
    else do
      -- Make it the ACTIVE chord, so `v` opens what you just clicked. Without
      -- this, `sounding` was only ever set by `playId` (the pool-bubble path),
      -- so auditioning from any other lens left `v` pointing at whatever chord
      -- was last opened — it reads as the modal being stuck.
      --
      -- Guarded on pool membership because the modal edits `st.chords`: a Banks
      -- pad or a generated candidate is not in there, and claiming it as
      -- `sounding` would point the ladder at a chord it cannot find.
      when (any (\d -> d.id == c.id) st.chords) $
        H.modify_ _ { sounding = Just c.id, selected = Nothing }
      playChord c


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

  -- Delete one audition. Indices shift, so ADJUST the selection rather than drop it:
  -- a delete below the span slides it down; a delete inside shrinks the top; deleting
  -- the lone selected chord clears it. Keeping the selection alive through edits is
  -- what leaves the ⏎ save (rebundle) button available while you're editing an
  -- unbundled progression (the check-out → edit → check-in loop, §6).
  DeleteChyron i -> H.modify_ \st ->
    let sel' = case st.chyronSel of
          Nothing -> Nothing
          Just s
            | i < s.lo    -> Just { lo: s.lo - 1, hi: s.hi - 1, anchor: max 0 (s.anchor - 1) }
            | i > s.hi    -> Just s
            | s.hi > s.lo -> Just { lo: s.lo, hi: s.hi - 1, anchor: clamp s.lo (s.hi - 1) s.anchor }
            | otherwise   -> Nothing   -- the single selected chord was deleted
    in st { chyron = fromMaybe st.chyron (deleteAt i st.chyron)
          , chyronSel = sel'
          , hoveredChyron = Nothing }

  ClearChyron -> H.modify_ _ { chyron = [], chyronSel = Nothing, hoveredChyron = Nothing }

  -- Drop repeat chords from the trace (same pitch-class set), keeping the first of
  -- each. Selection is cleared since indices shift.
  DedupeChyron -> H.modify_ \st ->
    st { chyron = nubByEq (\a b -> a.pcs == b.pcs) st.chyron
       , chyronSel = Nothing, hoveredChyron = Nothing }

  -- Compress the selected span into a pinned 2-glyph token: mint a SavedSeq from
  -- its events + content-glyph, then REMOVE those events from the live trace
  -- (reclaiming the space — the saving is the compression).
  SaveChyronSel -> do
    st <- H.get
    case st.chyronSel of
      Just sel -> do
        let evs = mapMaybe (\ix -> index st.chyron ix) (range sel.lo sel.hi)
            saved = { events: evs, glyph: TGlyph.chordGlyph (map _.notes evs), pattern: "" }
            keep = mapMaybe (\(Tuple ix e) -> if ix < sel.lo || ix > sel.hi then Just e else Nothing)
                     (mapWithIndex Tuple st.chyron)
        H.modify_ _ { chyronSaved = shelve saved st.chyronSaved, chyron = keep
                    , chyronSel = Nothing, hoveredChyron = Nothing }
      _ -> pure unit

  -- Reorder the buffer by drag-and-drop (§10.4): drop chip `f` before chip `t`.
  -- Arrangement is ORDER now, not timestamps (§8), so a plain array move is the
  -- whole story; the selection is dropped since its indices no longer mean the
  -- same chords.
  ChyronDragStart i -> H.modify_ _ { chyronDrag = Just i }

  ChyronDropOn t -> H.modify_ \st -> case st.chyronDrag of
    Nothing -> st
    Just f
      | f == t -> st { chyronDrag = Nothing }
      | otherwise -> case index st.chyron f of
          Nothing -> st { chyronDrag = Nothing }
          Just el ->
            let without = fromMaybe st.chyron (deleteAt f st.chyron)
                t' = if f < t then t - 1 else t     -- removing f before t shifts t left
                reordered = fromMaybe (without <> [ el ]) (insertAt t' el without)
            in st { chyron = reordered, chyronDrag = Nothing, chyronSel = Nothing }

  ChyronDragEnd -> H.modify_ _ { chyronDrag = Nothing }

  PlaySaved i -> do
    st <- H.get
    for_ (index st.chyronSaved i) \s -> playEvents s.events

  DeleteSaved i -> H.modify_ \st -> st { chyronSaved = fromMaybe st.chyronSaved (deleteAt i st.chyronSaved) }

  -- Unbundle (check OUT) a saved token into the working buffer to edit it (§6): the
  -- token LEAVES the shelf, its events append to the buffer, and the appended run is
  -- SELECTED so you can immediately reorder / revoice / Explore / delete it — and the
  -- ⏎ save (rebundle) button is right there. Editing then ⏎-saving mints a fresh token
  -- (new content-glyph) back onto the shelf: check-out → edit → check-in, additive,
  -- never a silent overwrite. (The cap can trim the front, so the selection is
  -- computed against the merged length.)
  Unbundle i -> H.modify_ \st -> case index st.chyronSaved i of
    Nothing -> st
    Just s ->
      let merged = takeEnd chyronCap (st.chyron <> s.events)
          addN = length s.events
          selLo = max 0 (length merged - addN)
          selHi = max selLo (length merged - 1)
      in st { chyron = merged
            , chyronSaved = fromMaybe st.chyronSaved (deleteAt i st.chyronSaved)
            , chyronSel = if addN == 0 then st.chyronSel else Just { lo: selLo, hi: selHi, anchor: selLo } }

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
          -- A token that names its own reading brings it with it; one that does
          -- not leaves whatever the box was already doing alone.
          Just sq -> H.modify_ _
            { perfBoxes = mapWithIndex
                (\j box -> if j /= b then box
                           else box { seq = Just sq
                                    , seqText = if sq.pattern == "" then box.seqText else sq.pattern })
                st.perfBoxes
            , perfHeld = Nothing
            }
          Nothing -> pure unit

  PerfClearBox b -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box -> if j == b then box { seq = Nothing } else box) st.perfBoxes }

  -- Append a fresh empty player on the lowest free MIDI channel (1..16), so the
  -- Perform surface is a growable palette of voices rather than a fixed four.
  PerfAddBox -> H.modify_ \st ->
    let used = map _.channel st.perfBoxes
        free = fromMaybe (length st.perfBoxes + 1) (find (\c -> not (elem c used)) (range 1 16))
    in st { perfBoxes = st.perfBoxes <>
              [ { cardId: freeCardId st.perfBoxes, channel: free, label: "P" <> show free, seq: Nothing, stack: [], seqText: "", muted: false, term: TMidi, phrase: Nothing } ] }

  -- Delete a player outright (distinct from PerfClearBox, which only empties its
  -- token). Its channel frees for the next add.
  PerfRemoveBox b -> H.modify_ \st ->
    st { perfBoxes = fromMaybe st.perfBoxes (deleteAt b st.perfBoxes) }

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

  -- `→ odo` is EXCLUSIVE (AC, 2026-08-06): exactly one box may conduct Odonus's
  -- quantiser at a time — the harmonic-context voice. Promoting one demotes any
  -- other to `→ midi`, so there is never an ambiguous "which chord is THE context"
  -- and the ONE-set model has one unambiguous source. Any other terminal is free.
  PerfSetTerm b t -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { term = t }
           else if t == TOdo && box.term == TOdo then box { term = TMidi }
           else box)
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

  -- Phrase picker (#27): open/close the clip library, attach a chosen clip as a box's
  -- source (a self-contained copy), or detach back to chords. Attaching coerces a → odo
  -- terminal to → midi, since a frozen phrase can only sound (midi/rig), not condition.
  -- re-read the shared clip library when opening the picker, so a clip captured in
  -- Odonus this session shows up without a reload.
  PerfOpenPhrasePick b -> do
    clips <- liftEffect ClipStore.loadClips
    H.modify_ _ { perfPhrasePick = Just b, clipLibrary = clips }

  PerfClosePhrasePick -> H.modify_ _ { perfPhrasePick = Nothing }

  PerfAttachPhrase b clip -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b
             then box { phrase = Just { clip, mutedHeads: [], channelMode: Flatten }
                      , term = if box.term == TOdo then TMidi else box.term }
             else box)
         st.perfBoxes
       , perfPhrasePick = Nothing }

  PerfDetachPhrase b -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { phrase = Nothing } else box)
         st.perfBoxes }

  PerfPhraseMode b mode -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b then box { phrase = map (_ { channelMode = mode }) box.phrase } else box)
         st.perfBoxes }

  -- toggle source head `h` in box `b`'s phrase mute mask (silence it from the recording).
  PerfPhraseMuteHead b h -> H.modify_ \st ->
    st { perfBoxes = mapWithIndex (\j box ->
           if j == b
             then box { phrase = map (\ph -> ph { mutedHeads =
                          if elem h ph.mutedHeads then filter (_ /= h) ph.mutedHeads else [ h ] <> ph.mutedHeads }) box.phrase }
             else box)
         st.perfBoxes }

  -- Clip-library management (#33). These write the WHOLE library back to the shared
  -- store (clipLibrary was loaded fresh when the picker opened, so it's authoritative).
  -- Attached box copies are snapshots and untouched — a renamed/deleted clip keeps
  -- sounding in any voice it was already dropped into.
  ClipAudition clip -> auditionClip clip

  ClipRename cid newName -> do
    st <- H.get
    let lib' = map (\c -> if c.id == cid then c { name = newName } else c) st.clipLibrary
    H.modify_ _ { clipLibrary = lib' }
    liftEffect (ClipStore.saveClips lib')

  ClipDelete cid -> do
    st <- H.get
    let lib' = filter (\c -> c.id /= cid) st.clipLibrary
    H.modify_ _ { clipLibrary = lib' }
    liftEffect (ClipStore.saveClips lib')

  -- The shelf's ◴ declares a PROGRESSION — chords that are alternatives to each
  -- other, one sample each. This one declares a PHRASE: a marked region whose
  -- rhythm is the material, sampled whole. Same store, same collection, and the
  -- kind is what tells the sampler which it is holding.
  ClipShare clip -> do
    H.modify_ _ { publishMsg = Just "sending to Quadrat…" }
    res <- liftAff (attempt (Amphora.publish (Share.phraseSpec clip)))
    H.modify_ _ { publishMsg = Just case res of
        Right hash -> "✓ for Quadrat · " <> SCU.take 8 hash
        Left _ -> "✗ send failed (store offline?)" }

  -- Capture band (#28). Mark flags the last two bars (the roll runs newest-at-top,
  -- so a mark drops a default region back over what you just played). Selecting a
  -- band shows the lift card; saveClip materializes the region and appends it to the
  -- shared library with source "vetula". No in-surface audition — you hear the lifted
  -- clip in the library modal (or attached into a voice).
  -- With the rig, it makes the mark and numbers it; it comes back in a loops
  -- frame, and takes Vetula's text then.
  CaptureMark -> do
    nowMs <- liftEffect perfNow
    st <- H.get
    if st.rigLoops then rigSend (RL.cueLine "vetula" "mark")
    else do
      let atMic = nowMs * 1000.0
          barMic = 60.0e6 / (if st.clockTempo > 1.0 then st.clockTempo else 120.0) * 4.0
          mark = { atMicros: atMic, beat: 0.0, from: atMic - 2.0 * barMic, to: atMic, patch: markText st, now: "", sounding: Nothing, rig: [], tempo: st.clockTempo, n: 0, id: RL.nextId st.capture.logbook.marks, loop: Nothing }
      H.modify_ \s -> s { capture = s.capture { logbook = let lb = Logbook.pushMark mark s.capture.logbook in lb { marks = RL.renumber lb.marks } } }
      H.raise (Marked atMic)

  -- Click a gold band → LOOP it (Odonus's affordance). Materialise the region's
  -- notes rebased to [0, len) and stamp the loop clock; `driveCaptureReplay` queues
  -- them a frame at a time from there. The watermark starts a hair before the
  -- origin so a phase-0 note isn't lost on the strict `>` boundary of frame one.
  CaptureRegionSelect i -> do
    st <- H.get
    -- with the rig, a click starts its loop of the mark, or stops it
    if st.rigLoops then for_ (st.capture.logbook.marks !! i) \m ->
      rigSend (RL.cueLine "vetula" (if RL.looping m then "loop " <> show m.n <> " hush" else "loop " <> show m.n))
    else for_ (st.capture.logbook.marks !! i) \m -> do
      nowMs <- liftEffect perfNow
      H.modify_ \s -> s { capture = s.capture { playing = Just
        { source: FromRegion i
        , events: Logbook.materializeRegion m.from m.to s.capture.logbook
        , lenMicros: m.to - m.from
        , fromMicros: m.from, toMicros: m.to
        , loopStartMs: nowMs, scheduledUntilMs: nowMs - 1.0, playheadFrac: 0.0 } } }

  CaptureStopSel -> do
    st <- H.get
    if st.rigLoops then for_ (RL.focus st.capture.logbook.marks) \m -> rigSend (RL.cueLine "vetula" ("loop " <> show m.n <> " hush"))
    else do
      hushCapture
      H.modify_ \s -> s { capture = s.capture { playing = Nothing } }

  CaptureToggleContext -> H.modify_ \s -> s { capture = s.capture { contextOpen = not s.capture.contextOpen } }
  CaptureToggleCode -> H.modify_ \s -> s { capture = s.capture { codeOpen = not s.capture.codeOpen } }
  CaptureToLimulus i -> do
    st <- H.get
    for_ (st.capture.logbook.marks !! i) \m -> for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin)
        ("stage-paste vetula/mark " <> markCode "vetula" _.patch m)
  CaptureZoom z -> H.modify_ \s -> s { capture = s.capture { zoom = z } }

  CaptureCutArm -> H.modify_ \s -> s { capture = s.capture { cutting = not s.capture.cutting, cutSel = Nothing } }
  CaptureCutDown cx cy -> do
    sid <- H.subscribe $ HS.makeEmitter \emit -> do
      moveFn <- eventListener \e -> case ME.fromEvent e of
        Just me -> emit (CaptureCutMove (ME.clientX me) (ME.clientY me))
        Nothing -> pure unit
      upFn <- eventListener \_ -> emit CaptureCutUp
      target <- Window.toEventTarget <$> window
      addEventListener (EventType "mousemove") moveFn false target
      addEventListener (EventType "mouseup") upFn false target
      pure do
        removeEventListener (EventType "mousemove") moveFn false target
        removeEventListener (EventType "mouseup") upFn false target
    at <- capturePointer cx cy
    H.modify_ \s -> s { captureDragSub = Just sid, capture = s.capture { cutSel = Just { from: at, to: at } } }
  CaptureCutMove cx cy -> do
    at <- capturePointer cx cy
    H.modify_ \s -> s { capture = s.capture { cutSel = map (_ { to = at }) s.capture.cutSel } }
  -- the rig cuts the stretch (stopping at marks' windows) and sends every
  -- page the record buffer again
  CaptureCutUp -> do
    st <- H.get
    for_ st.captureDragSub H.unsubscribe
    for_ st.capture.cutSel \sel -> do
      let span = (CaptureView.bounds st.capture.zoom st.capture.logbook).span
      mclock <- vetulaClock
      for_ mclock \clock -> when (max (sel.to - sel.from) (sel.from - sel.to) > span * 0.003) $
        rigSend (RL.cutLine "vetula" clock sel.from sel.to)
    H.modify_ \s -> s { captureDragSub = Nothing, capture = s.capture { cutting = false, cutSel = Nothing } }
  CaptureTrim -> rigSend (RL.cueLine "vetula" "trim")
  CaptureUndo -> rigSend (RL.cueLine "vetula" "undo")
  -- With the rig, clear its record buffer too; it says so to every page
  -- (loops-clear), and this one clears then.
  CaptureClear -> do
    st <- H.get
    if st.rigLoops then rigSend (RL.cueLine "vetula" "clear")
    else do
      hushCapture
      H.modify_ \s -> s { capture = s.capture { logbook = Logbook.emptyLog, playing = Nothing, contextOpen = false } }

  -- Leaving REPLAY drops the selected region and its context card: the lift card is
  -- a REPLAY affordance, and a stale one hanging over the strip in LIVE reads as if
  -- something were still armed.
  -- The river's animation tick. Guarded to the Perform surface in LIVE: this fires
  -- ~30×/s and Vetula's render is not cheap, so it must not run while you're
  -- hunting or reviewing (neither stage reads `nowMicros`).
  CaptureFrame -> do
    st <- H.get
    when (st.stage == Perform) do
      nowMs <- liftEffect perfNow
      let now = nowMs * 1000.0
      H.modify_ _ { nowMicros = now
                  , riverNotes = filter (\n -> (now - n.fireUnixMicros) < River.windowMicros) st.riverNotes }
    -- A run starts and stops with the transport (Capture.Runs): the Review
    -- surface draws only time inside runs. The rig keeps them too.
    when (st.playing /= Runs.running st.capture.logbook.runs) do
      nowMs <- liftEffect perfNow
      H.modify_ \s -> s { capture = s.capture { logbook = s.capture.logbook { runs = (if st.playing then Runs.startRun else Runs.stopRun) (nowMs * 1000.0) s.capture.logbook.runs } } }
      rigSend (RL.runLine "vetula" st.playing)
    -- Ask the rig for its marks and loops (RigLoops): every two seconds
    -- until it answers, then every five, so a page sees a rig that restarted
    do
      ms <- liftEffect perfNow
      when (ms - st.rigAsked > (if st.rigLoops then 5000.0 else 2000.0)) do
        rigSend RL.syncLine
        H.modify_ _ { rigAsked = ms }
    -- The rig's loops' playheads move on Review's clock
    when (st.stage == Review && st.rigLoops && any RL.looping st.capture.logbook.marks) do
      mclock <- vetulaClock
      H.modify_ \s -> s { capture = s.capture { rig = mclock } }
    -- The REPLAY loop rides the same frame clock; it no-ops when nothing is looping.
    driveCaptureReplay

  CaptureSaveClip i -> do
    st <- H.get
    for_ (st.capture.logbook.marks !! i) \m -> do
      nowMs <- liftEffect perfNow
      let evs = Logbook.materializeRegion m.from m.to st.capture.logbook
          clip =
            { id: "vetula-" <> show m.atMicros
            , events: evs
            , lenMicros: m.to - m.from
            , heads: headCount evs
            , capturedMicros: nowMs * 1000.0
            , source: "vetula"
            , name: "vetula clip"
            , tags: [ "vetula" ]
            , notes: ""
            , bpm: Just st.clockTempo
            , key: Nothing
            , context: Nothing
            }
      -- reload → prepend → save the whole library, so a clip Odonus wrote this
      -- session isn't clobbered (clipLibrary may be stale).
      existing <- liftEffect ClipStore.loadClips
      let lib' = [ clip ] <> existing
      liftEffect (ClipStore.saveClips lib')
      -- lifting ends the preview: hush before dropping it, or the lookahead rings on
      hushCapture
      H.modify_ _ { clipLibrary = lib', capture = st.capture { playing = Nothing } }

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

  ShufflePads -> H.modify_ \s -> s { padRoll = s.padRoll + 1 }

  ShuffleVary -> H.modify_ \s -> s { varyRoll = s.varyRoll + 1 }

  -- Sounds, does not capture. The chyron is what a progression gets lifted
  -- from, and a browse through 144 variations of one chord would bury the
  -- trace in things you were only listening to.
  VaryAudition c -> playChordQuiet c

  -- Shift-click TOGGLES: the same gesture keeps and un-keeps, so a mistake
  -- costs the same as the choice did.
  --
  -- In an EXCURSION the keep belongs to the SLOT that sent us, not to the
  -- free-standing tray: you went looking on behalf of a chord in a progression,
  -- and making you carry the result back by hand would be the tool forgetting
  -- what you asked it.
  KeepVariation c -> do
    st <- H.get
    case slotForKeep st of
      Just i -> do
        H.modify_ \s -> s { rehearsal = modifyIx i (RH.toggleOption c) s.rehearsal }
        playChordQuiet c
      Nothing -> keepFreely c

  ForgetKept i -> H.modify_ \s -> s { kept = fromMaybe s.kept (deleteAt i s.kept) }

  -- **Declare a progression to the sampler.** One path, never a lattice: you
  -- can only name what you sampled if you know which reading played, and a
  -- lattice deliberately plays a different one each cycle. A token carrying an
  -- alternating pattern is refused rather than silently flattened — flattening
  -- would hand over a reading nobody chose.
  ToQuadrat i -> do
    st <- H.get
    case index st.chyronSaved i of
      Nothing -> pure unit
      Just sq
        | sq.pattern /= "" && contains (Pattern "<") sq.pattern ->
            H.modify_ _ { publishMsg = Just "✗ that one alternates — settle a single path first" }
        | otherwise -> do
            now <- liftEffect dateNow
            let clip = clipOfSeq st sq now
                spec = Share.shareSpec clip
                         { kind: "chord-hits", glyph: sq.glyph.alias }
            H.modify_ _ { publishMsg = Just "sending to Quadrat…" }
            res <- liftAff (attempt (Amphora.publish spec))
            case res of
              Right hash -> H.modify_ _
                { publishMsg = Just ("✓ for Quadrat · " <> SCU.take 8 hash) }
              Left _ -> H.modify_ _ { publishMsg = Just "✗ send failed (store offline?)" }

  -- ── The hand-off ──────────────────────────────────────────────────────
  -- Everything leaves Rehearse the same way: as a token on the shelf. From
  -- there the app already knows what to do — drag it to a Perform box, and the
  -- box loops it on the transport and feeds Odonus if its terminal is → odo.
  -- Building a second route would have meant a second looping mechanism and a
  -- second thing that can be stale.
  --
  -- A settled pass mints a NEW token rather than replacing the one you took up:
  -- its chords differ, so its rebus differs, so it IS a different progression.
  -- Check out, edit, check in — the shape `Unbundle` already uses.
  KeepPass -> do
    st <- H.get
    mintFromRehearsal [ passIxs st ]

  KeepMarked -> do
    st <- H.get
    mintFromRehearsal (map _.ixs (markedRows st))

  -- Every slot's options at once, each slot alternating independently. The
  -- generative shape: the rig walks the space per cycle instead of us choosing.
  KeepLattice -> do
    st <- H.get
    mintLattice st.rehearsal

  -- ── REHEARSE ──────────────────────────────────────────────────────────
  -- Take up a saved progression: one slot per chord, its own voicing as option
  -- zero, and any variations already kept for that exact chord folded in — which
  -- is why `kept` is keyed by notes and not by id.
  TakeUp i -> do
    st <- H.get
    for_ (index st.chyronSaved i) \sq -> do
      let slots = map (RH.slotFrom st.kept) sq.events
      H.modify_ _ { rehearsal = slots, rehearseRoll = 0, stage = Rehearse
                  , rehearsalFrom = Just i, pane = Nothing, marked = [] }

  DropRehearsal -> H.modify_ _ { rehearsal = [], pane = Nothing, rehearsalFrom = Nothing, marked = [] }

  RollPass -> do
    H.modify_ \s -> s { rehearseRoll = s.rehearseRoll + 1 }
    handleAction PlayPass

  PlayPass -> do
    st <- H.get
    playEvents (passEvents st)

  SetPull p -> do
    H.modify_ _ { pull = p }
    handleAction PlayPass

  -- The chord ALONE, the same as a click in the vary grid. Two surfaces where
  -- pointing at a chord means different things is the thing you mis-predict a
  -- fortnight later; and the paths view now answers "how does it go" properly,
  -- so the three-chord preview no longer has to muddy this one.
  HearOption i j -> do
    st <- H.get
    for_ (index st.rehearsal i >>= \sl -> index sl.options j) playChordQuiet

  -- A chord is not good or bad, it is good or bad THERE. Three chords, which is
  -- what a player trying a substitution actually plays.
  HearAround i c -> do
    st <- H.get
    playEvents (spaced 0.0 (RH.around st.pull st.rehearseRoll st.rehearsal i c))

  -- And if the neighbours have alternatives of their own, whether this one
  -- works is a question about all of them at once. A gap between phrases, so
  -- you can hear where one reading ends and the next begins.
  SweepAround i c -> do
    st <- H.get
    let phrases = RH.allAround sweepCap st.rehearsal i c
        laid = mapWithIndex (\k ph -> spaced (toNumber k * phraseGap) ph) phrases
    playEvents (concat laid)

  ShowPaths -> H.modify_ \s -> s { pane = if s.pane == Just PanePaths then Nothing else Just PanePaths }

  HearPath ixs -> do
    st <- H.get
    playEvents (spaced 0.0 (pathChords st ixs))

  MarkPath ixs -> do
    st <- H.get
    let key = map playNotes (pathChords st ixs)
    H.modify_ \s -> s { marked = if elem key s.marked
                                   then filter (_ /= key) s.marked
                                   else s.marked <> [ key ] }
    playEvents (spaced 0.0 (pathChords st ixs))

  -- Toggling, so the same click settles and unsettles.
  SettleSlot i j -> do
    H.modify_ \s -> s { rehearsal = modifyIx i (\sl ->
      sl { locked = if sl.locked == Just j then Nothing else Just j }) s.rehearsal }
    st <- H.get
    for_ (index st.rehearsal i >>= \sl -> index sl.options j) playChordQuiet

  -- Settle the WHOLE pass: the converging gesture. You heard it, you keep it,
  -- and every alternative is still there when you unlock.
  SettlePass -> H.modify_ \s ->
    s { rehearsal = zipWith (\sl j -> sl { locked = Just j }) s.rehearsal (passIxs s) }

  LoosenAll -> H.modify_ \s -> s { rehearsal = map (_ { locked = Nothing }) s.rehearsal }

  -- Option zero is the progression's own chord and is not removable: a slot has
  -- to be able to play what was written.
  DropOption i j -> H.modify_ \s ->
    if j == 0 then s
    else s { rehearsal = modifyIx i (\sl ->
      sl { options = fromMaybe sl.options (deleteAt j sl.options)
         , locked = case sl.locked of
             Just k | k == j -> Nothing
             Just k | k > j -> Just (k - 1)
             other -> other
         }) s.rehearsal }

  -- Opens the grid UNDERNEATH the progression rather than navigating to it.
  -- AC: there is plenty of room, and scrolling down a little to find a chord is
  -- fine — where losing sight of the progression you are varying is not, since
  -- what you keep only makes sense against the chords either side of it.
  -- Clicking the same slot again closes the panel.
  VaryFromSlot i -> do
    st <- H.get
    if varyingSlot st == Just i then H.modify_ _ { pane = Nothing }
    else for_ (index st.rehearsal i >>= \sl -> index sl.options 0) \base -> do
      H.modify_ _ { varying = Just base, pane = Just (PaneVary i), revoicing = Nothing }
      playChordQuiet base

  BackToRehearsal -> H.modify_ _ { stage = Rehearse, pane = Nothing }

  -- Opening the lens closes the revoice modal: they are two views of the same
  -- question at different magnifications, and both up at once is just clutter.
  OpenVary c -> do
    H.modify_ \s -> s { varying = Just c, revoicing = Nothing, stage = Hunt Vary, lastLens = Vary
                      -- A fresh excursion from a chord is not a return trip.
                      , pane = Nothing }
    playChord c

  -- The one mode switch. Absorbed the old `SetCaptureView`, so leaving REVIEW by
  -- ANY route — Perform, or off to Hunt — hushes the region preview and drops the
  -- lift card. Under the old split you could escape a looping preview sideways
  -- into Browse and it would keep ringing.
  SetStage v -> do
    when (v /= Review) hushCapture
    H.raise (StageChanged (stagePath v))
    H.modify_ \st -> st
      { stage = v
      , lastLens = huntOr st.lastLens v
      , hoveredId = Nothing, hoveredTriad = Nothing
      , viewCx = 0.0, viewCy = 0.0, viewZoom = 1.0, panning = Nothing, panMoved = false
      , capture = if v == Review then st.capture else st.capture { playing = Nothing, contextOpen = false }
      -- An excursion is a round trip between two places. Navigating anywhere
      -- ELSE ends it, or a later free visit to the Vary lens would quietly post
      -- its keeps into a slot you had stopped thinking about.
      -- A panel belongs to the Rehearse pane; leaving for anywhere but the
      -- standalone Vary lens closes it, so a keep can never post into a slot you
      -- had stopped thinking about.
      , pane = if v == Rehearse || v == Hunt Vary then st.pane else Nothing
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
      -- Capture tap (#28): a wall-clock base for this tick + a Ref the emit sites
      -- prepend to (newest-first). Every voice/box note tapped this tick lands here,
      -- then folds into the always-on logbook below — the "player piano" roll.
      nowMs <- liftEffect perfNow
      capRef <- liftEffect (Ref.new ([] :: Array NoteEvent))
      let rec r = Ref.modify_ (\xs -> [ { pitch: r.pitch, headIdx: r.headIdx
                    , fireUnixMicros: (nowMs + r.delayMs) * 1000.0, vel: r.vel, gateMs: r.gateMs } ] <> xs) capRef
      let reefChords = map toReefChord (perfChords st)
          pulseMs = 60000.0 / tempo / 4.0
          -- ATLANTIS (audible=false): keep advancing each voice's read-head so the
          -- pulse + cursor march on (the nav harmonic strip stays live in every
          -- pane), but pass no MIDI-out so nothing sounds locally — the rig's brush
          -- is the sound. SOLO: emit as normal.
          mout = if st.authority == Local then st.midiOut else Nothing
      voices' <- liftEffect $ traverse (stepVoice mout st.routing reefChords tick.index pulseMs tick.delayMs rec) st.voices
      -- PERFORM boxes: query each filled box's `Pattern` for the current cycle and
      -- schedule the notes it yields (block together; arp/`fast` subdivide). A box
      -- with a text-hatch sequence plays on the BAR grid (mini-notation cycle = one
      -- bar); a plain box on the per-BEAT grid (one chord per beat, unchanged).
      -- MIDI-only (→ odo feeds Odonus via poll, → rig is rig-only).
      let beatMs = pulseMs * 4.0
          barMs = pulseMs * 16.0
      for_ mout \out -> liftEffect $
        for_ st.perfBoxes \box ->
          -- a box sounds if it has a source: a chord token (`seq`) OR a captured
          -- phrase (#27). Phrase boxes have no `seq`, so gate on either.
          when (isJust box.seq || isJust box.phrase) $
            when (not box.muted && box.term == TMidi) $
              -- each box's scheduling is isolated: a throw in one box (e.g. a malformed
              -- phrase) must not silence the others, so catch + log rather than abort
              -- the whole per-tick loop.
              let onGrid = if boxUsesSeq box then tick.index `mod` 16 == 0 else tick.index `mod` 4 == 0
                  cyc = if boxUsesSeq box then tick.index / 16 else tick.index / 4
                  slotMs = if boxUsesSeq box then barMs else beatMs
              in when onGrid do
                   r <- try (scheduleBox out cyc slotMs beatMs tick.delayMs rec box)
                   case r of
                     Left err -> Console.error ("perf box P" <> show box.channel <> " schedule threw: " <> message err)
                     Right _ -> pure unit
      -- Fold this tick's tapped notes into the always-on capture logbook (#28).
      fresh <- liftEffect (Ref.read capRef)
      -- playing here, the page sends the rig what it played, for the record
      -- buffer (the rig records the cards itself when it plays them)
      when (st.authority == Local && st.rigLoops) do
        unixMs <- liftEffect dateNow
        let offsetUs = (unixMs - nowMs) * 1000.0
        for_ (RL.recordLine "vetula" (_ + offsetUs) fresh) rigSend
      H.modify_ \st2 -> st2
        { pulse = tick.index, voices = voices', clockTempo = tempo, tempo = round tempo
        , capture = st2.capture { logbook = Logbook.logAppend (nowMs * 1000.0) fresh st2.capture.logbook }
        -- the river draws the same notes over a short window; CaptureFrame prunes it
        , riverNotes = fresh <> st2.riverNotes }

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
  Just text -> Just { glyph: progGlyph text, diverged: currentSource s /= text }

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
stepVoice :: Maybe Midi.MidiOut -> Map String Int -> Array RV.VChord -> Int -> Number -> Number -> (EmitRec -> Effect Unit) -> Voice -> Effect Voice
stepVoice mout routing reefChords pulse pulseMs baseDelayMs rec v =
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
        for_ emit \e -> do
          Midi.scheduleNote out
            { channel: Routing.toWire (midiChannelFor routing v), note: e.note, velocity: e.velocity
            , delayMs: baseDelayMs, durMs: e.durPulses * pulseMs }
          -- tap: record the emitted note for the capture logbook (#28). headIdx = the
          -- voice's (1-based) channel, so the tracker colours by voice.
          rec { pitch: e.note, headIdx: midiChannelFor routing v, delayMs: baseDelayMs
              , vel: e.velocity, gateMs: e.durPulses * pulseMs }
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

-- | **Every chord identity in Vetula, through one function.**
-- |
-- | There used to be three: the rendered Tidal source (presets, the parked
-- | chip), the ordered pitch-class sets (a saved chyron sequence), and a
-- | hand-written copy of Rebus's chord wire format (a recalled scene). Three
-- | formats meant the same progression wore three different pictures depending
-- | on which door it came in by — before any question of agreeing with another
-- | application.
-- |
-- | Now all of them go to `Triggerfish.Glyph.chordGlyph`, over the absolute
-- | MIDI of the chords. Source text is dropped because a key label and a
-- | comment are context, not content; pitch classes are dropped because
-- | register is exactly what a voicing IS.
-- |
-- | The comments are dropped by `parseProgression` already, so this is safe to
-- | run over a source that has been hand-edited in the source box.
progGlyph :: String -> Glyph
progGlyph = TGlyph.chordGlyph <<< parseProgression

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
-- | `map` over each chord's notes; Tidal time combinators (`slow`/`fast`) are
-- | polymorphic in the value, so they compose with the pitch layers at the same type.
applyFx :: PerfFx -> PT.Pattern (Array Int) -> PT.Pattern (Array Int)
applyFx = case _ of
  -- value verbs sample their PatternArg per chord (`withSampledArg`): the arg is a
  -- mini-notation, so `transpose "0 7 <5 3>"` shifts differently per chord and per
  -- cycle. A literal (`transpose 7`) is the degenerate constant pattern. Invalid/empty
  -- sampled atom → the verb's default (0 / Open / keep-1), Selene-lenient.
  Transpose arg -> withSampledArg (\s -> map (_ + tokInt 0 s)) (argEval arg)
  Octave arg -> withSampledArg (\s -> map (_ + 12 * tokInt 0 s)) (argEval arg)
  Slow n -> slow (Rat.fromInt (max 1 n))
  Fast n -> fast (Rat.fromInt (max 1 n))
  Voice arg -> withSampledArg (\s -> revoice (voiceStrategy (shapeOf s))) (argEval arg)
  Select (Low arg) -> withSampledArg (\s -> revoice (takeVoicing (TakeLow (selInt s)))) (argEval arg)
  Select (High arg) -> withSampledArg (\s -> revoice (takeVoicing (TakeHigh (selInt s)))) (argEval arg)
  -- arp IS a pattern transform: it explodes each chord into singleton-note events
  -- spread across that chord's OWN whole (`arpeggiate`), so it composes with
  -- slow/fast — `slow 8 # arp up 4` unfolds the arp over eight bars. Pre-`map` the
  -- notes into the direction's order, then arpeggiate cycles through them.
  Arpg dir rate -> arpRate rate <<< map (arpOrder dir)
  -- the power arp: an explicit index figure over the chord (0 = lowest voice),
  -- octave-wrapping past the top (`arpSelect`). The figure is a mini-notation, so
  -- rests/subdivision/alternation/euclid all compose — and it stretches under slow
  -- like everything else (`arpWith` keeps each figure-step's arc).
  ArpP src -> arpIndexed arpSelect (idxPattern src)
  -- strum stays a sink ornament (a fast ms onset stagger at the chord's onset — it
  -- rolls a block chord, it doesn't stretch), so it's identity in the pattern.
  Strum _ -> identity

-- | The index-figure of an `ArpP` layer as a `Pattern String` of positions. Lenient:
-- | an unparseable figure falls back to a steady root (`"0"`), Selene-style.
idxPattern :: String -> PT.Pattern String
idxPattern src = case parseMiniPattern src of
  Right p -> p
  Left _ -> pure "0"

-- | Select a note from a chord by a figure step: parse the token as an index into the
-- | chord's notes SORTED low→high (0 = lowest), wrapping up/down an octave past the
-- | ends (index `n` on an `n`-note chord = the root an octave up). A non-numeric token
-- | (or an empty chord) selects nothing — a rest.
arpSelect :: Array Int -> String -> Maybe Int
arpSelect ns tok = case fromString (trim tok) of
  Nothing -> Nothing
  Just idx ->
    let sorted = sort ns
        m = length sorted
    in if m == 0 then Nothing
       else
         let i = ((idx `mod` m) + m) `mod` m   -- 0..m-1 (Euclidean, handles idx < 0)
             oct = (idx - i) / m               -- floor division → octave displacement
         in (\v -> v + 12 * oct) <$> index sorted i

-- | A verb argument as a `Pattern String`: its mini-notation source parsed, falling
-- | back to the raw source as a constant pattern when it won't parse (so a literal
-- | like `open` or `7`, and any typo, still samples to itself — the verb then
-- | interprets it and defaults if need be).
argEval :: PatternArg -> PT.Pattern String
argEval arg = case parseMiniPattern (argSrc arg) of
  Right p -> p
  Left _ -> pure (argSrc arg)

-- | Interpret a sampled atom as a VoiceShape (default Open) / a Low-High count (1..6).
shapeOf :: String -> VoiceShape
shapeOf s = fromMaybe Open (parseVoiceShape (trim s))

selInt :: String -> Int
selInt s = clamp 1 6 (tokInt 1 s)

-- | The chord→time REALISATION a box's stack asks for at the SINK. Arp is no longer
-- | here — it's a pattern transform now (`applyFx`/`arpeggiate`), so by schedule time
-- | its notes are already singleton events. Only strum stays a sink ornament (a fast
-- | ms onset stagger); everything else is a plain block chord struck on its onset.
data Realise = RBlock | RStrum Int

boxRealise :: Array Layer -> Realise
boxRealise = foldl pick RBlock <<< map _.fx
  where
  pick acc = case _ of
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
  Prob p -> whenCycle (\c -> cycleRand c < p) (applyFx fx)   -- a P-fraction of cycles
  AfterBar n -> whenCycle (\c -> c >= n) (applyFx fx)        -- only from bar n onward
  Whenmod n r -> whenCycle (\c -> mod c n >= r) (applyFx fx) -- Tidal's whenmod n r

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

-- | A short chip label for a stack layer.
fxLabel :: PerfFx -> String
fxLabel = case _ of
  Transpose arg -> "transpose " <> glyphArg arg
  Octave arg -> "8ve " <> glyphArg arg
  Slow n -> "slow ×" <> show (max 1 n)
  Fast n -> "fast ×" <> show (max 1 n)
  Voice arg -> "voice " <> glyphArg arg
  Select (Low arg) -> "bottom " <> glyphArg arg
  Select (High arg) -> "top " <> glyphArg arg
  Arpg dir r -> "arp " <> arpDirGlyph dir <> " ×" <> show r
  ArpP src -> "arp " <> src
  Strum ms -> "strum " <> show ms <> "ms"

voiceShapeName :: VoiceShape -> String
voiceShapeName = case _ of
  Open -> "open"
  Rootless -> "rootless"
  Drop2 -> "drop2"
  Drop24 -> "drop2&4"
  Quartal -> "quartal"
  Cluster -> "cluster"

-- | Nudge a layer's parameter by `d` (the chip's − / + controls), clamped. A LITERAL
-- | arg nudges (transpose/oct/select the number, voice the shape); a PATTERN arg is
-- | left untouched — you edit a figure in the text hatch, per the two-views rule.
fxNudge :: Int -> PerfFx -> PerfFx
fxNudge d = case _ of
  Transpose arg -> Transpose (nudgeIntArg (-24) 24 d arg)
  Octave arg -> Octave (nudgeIntArg (-4) 4 d arg)
  Slow n -> Slow (max 1 (n + d))
  Fast n -> Fast (max 1 (n + d))
  Voice arg -> Voice (cycleShapeArg d arg)
  Select (Low arg) -> Select (Low (nudgeIntArg 1 6 d arg))
  Select (High arg) -> Select (High (nudgeIntArg 1 6 d arg))
  Arpg dir r -> Arpg dir (clamp 1 16 (r + d))     -- nudge the steps-per-bar
  ArpP src -> ArpP src                            -- the figure is edited in the text hatch
  Strum ms -> Strum (clamp 0 80 (ms + d))

-- | Nudge a literal-integer arg within [lo,hi]; a pattern (or non-numeric literal) is
-- | left as-is (edit it in the text hatch).
nudgeIntArg :: Int -> Int -> Int -> PatternArg -> PatternArg
nudgeIntArg lo hi d = case _ of
  Lit s -> case fromString (fromMaybe s (stripPrefix (Pattern "+") s)) of
    Just n -> Lit (show (clamp lo hi (n + d)))
    Nothing -> Lit s
  Pat s -> Pat s

-- | Cycle a literal voice-shape arg through the shapes; a pattern is left as-is.
cycleShapeArg :: Int -> PatternArg -> PatternArg
cycleShapeArg d = case _ of
  Lit s -> Lit (printVoiceShape (cycleVoiceShape d (fromMaybe Open (parseVoiceShape (trim s)))))
  Pat s -> Pat s

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
  Transpose arg -> "transpose " <> printArg arg
  Octave arg -> "oct " <> printArg arg
  Slow n -> "slow " <> show (max 1 n)
  Fast n -> "fast " <> show (max 1 n)
  Voice arg -> "voice " <> printArg arg
  Select (High arg) -> "top " <> printArg arg
  Select (Low arg) -> "bottom " <> printArg arg
  Arpg dir r -> "arp " <> printArpDir dir <> " " <> show r
  ArpP src -> "arp \"" <> src <> "\""
  Strum ms -> "strum " <> show ms

printWhen :: When -> String
printWhen = case _ of
  Always -> ""
  Every n -> " every " <> show n
  Prob p -> " prob " <> show p
  AfterBar n -> " afterbar " <> show n
  Whenmod n r -> " whenmod " <> show n <> " " <> show r

printLayer :: Layer -> String
printLayer lyr = printPerfFx lyr.fx <> printWhen lyr.when

printPipeline :: PerfBox -> String
printPipeline box =
  let s = trim box.seqText
      layers = map printLayer box.stack
  in if s == "" && length layers == 0 then ""
     else if s == "" then "# " <> joinWith " # " layers
     else joinWith " # " ([ s ] <> layers)

-- | The pipeline printed one segment per LINE — the head sequence, then each `# layer`
-- | on its own line. Display-only, for the voice-card textarea: it parses back
-- | identically (`parsePipeline` splits on `#` and trims each segment, so the newlines
-- | are harmless), but keeps a many-layer card NARROW instead of running the whole
-- | pipeline off one line. The reconciliation invariant still holds through
-- | `parsePipeline`; this is purely how the text is laid out for the eye.
printPipelineLines :: PerfBox -> String
printPipelineLines box =
  let s = trim box.seqText
      -- a gated layer prints on TWO lines — the verb, then its gate as its own `# …`
      -- line — matching how the eye reads the card (each `#` line one modifier). It
      -- re-attaches on parse (`parseBareGate`), so the round-trip is stable.
      layerLines l = [ "# " <> printPerfFx l.fx ] <> case printWhen l.when of
                       "" -> []
                       g -> [ "#" <> g ]
      layers = concatMap layerLines box.stack
  in if s == "" && length layers == 0 then ""
     else if s == "" then joinWith "\n" layers
     else joinWith "\n" ([ s ] <> layers)

-- the arp DIRECTION keywords (vs. an index figure like "0 1 2").
isArpDir :: String -> Boolean
isArpDir d = elem (toLower d) [ "up", "down", "updown" ]

-- one integer token, lenient: strips a leading '+' (which `fromString` rejects),
-- falls back to `def` on anything non-numeric.
tokInt :: Int -> String -> Int
tokInt def s = fromMaybe def (fromString (fromMaybe s (stripPrefix (Pattern "+") s)))

parsePerfFx :: Array String -> Maybe PerfFx
parsePerfFx toks = case head toks of
  Nothing -> Nothing
  Just kw ->
    let args = drop 1 toks
        -- slot 0 as a PatternArg, defaulting to a bare literal `d` when absent.
        arg0 d = mkArg (fromMaybe d (head args))
        a1 d = tokInt d (fromMaybe "" (index args 1))
    in case toLower kw of
         "transpose" -> Just (Transpose (arg0 "0"))
         "trans" -> Just (Transpose (arg0 "0"))
         "oct" -> Just (Octave (arg0 "0"))
         "octave" -> Just (Octave (arg0 "0"))
         "8ve" -> Just (Octave (arg0 "0"))
         "slow" -> Just (Slow (max 1 (tokInt 4 (fromMaybe "" (head args)))))
         "fast" -> Just (Fast (max 1 (tokInt 2 (fromMaybe "" (head args)))))
         "voice" -> Just (Voice (arg0 "open"))
         "top" -> Just (Select (High (arg0 "1")))
         "bottom" -> Just (Select (Low (arg0 "1")))
         "arp" -> case head args of
           Nothing -> Just (Arpg ArpUp 4)
           Just d
             | isArpDir d -> Just (Arpg (parseArpDir (toLower d)) (clamp 1 16 (a1 4)))
             | otherwise -> Just (ArpP (unq (joinWith " " args)))
         "strum" -> Just (Strum (clamp 0 80 (tokInt 14 (fromMaybe "" (head args)))))
         _ -> Nothing

parseLayer :: String -> Maybe Layer
parseLayer seg =
  let g = peelGate (tokenize seg)
  in map (\fx -> { fx, when: g.when }) (parsePerfFx g.body)

-- | Peel a trailing GATE clause off a layer's tokens, returning the gate and the
-- | remaining verb `body`. Two-arg `whenmod n r` (three tokens) is tried first, then
-- | the one-arg gates `every`/`prob`/`afterbar` (two tokens). No recognised gate →
-- | `Always`, body unchanged. Only recognised keywords peel, so a verb whose trailing
-- | tokens aren't a gate keeps them (value args are single/quoted tokens).
peelGate :: Array String -> { when :: When, body :: Array String }
peelGate toks =
  let n = length toks
      two = do
        kw <- index toks (n - 3)
        a <- index toks (n - 2) >>= fromString
        b <- index toks (n - 1) >>= fromString
        if toLower kw == "whenmod" then Just (Whenmod a b) else Nothing
      one = do
        kw <- index toks (n - 2)
        val <- index toks (n - 1)
        parseGate1 kw val
  in case two of
       Just w -> { when: w, body: take (n - 3) toks }
       Nothing -> case one of
         Just w -> { when: w, body: take (n - 2) toks }
         Nothing -> { when: Always, body: toks }

-- | A one-arg gate clause `<keyword> <value>` → a `When`. Unrecognised → Nothing.
parseGate1 :: String -> String -> Maybe When
parseGate1 kw val = case toLower kw of
  "every" -> Every <$> fromString val
  "prob" -> Prob <$> Number.fromString val
  "afterbar" -> AfterBar <$> fromString val
  _ -> Nothing

parsePipeline :: String -> { seqText :: String, stack :: Array Layer }
parsePipeline txt =
  let segs = split (Pattern "#") txt
  in { seqText: trim (fromMaybe "" (head segs))
     , stack: foldl addSeg [] (drop 1 segs)
     }
  where
  -- a `#` segment is either a verb layer (append it) or a BARE gate with no verb —
  -- in which case attach it to the layer above, so `# arp "0 1 2"` / `# prob 0.4` on
  -- separate lines works (the gate is a suffix on a layer, not a layer of its own).
  addSeg stack seg = case parseLayer seg of
    Just lyr -> snoc stack lyr
    Nothing -> case parseBareGate seg of
      Just w -> attachWhen w stack
      Nothing -> stack

-- | Attach a gate to the last layer in the stack (a bare `# prob 0.4` line modifies
-- | the layer above it). No preceding layer → the gate is dropped (nothing to gate).
attachWhen :: When -> Array Layer -> Array Layer
attachWhen w stack = case unsnoc stack of
  Just { init: i, last: l } -> snoc i (l { when = w })
  Nothing -> stack

-- | A segment that is ONLY a gate clause (`every 4`, `prob 0.3`, `whenmod 8 1`) — no
-- | verb left after peeling. Used to attach a bare `# prob 0.4` line to the layer above.
parseBareGate :: String -> Maybe When
parseBareGate seg =
  let g = peelGate (tokenize seg)
  in if length g.body == 0 then (case g.when of
                                   Always -> Nothing
                                   w -> Just w)
     else Nothing

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

-- | Whether a box plays on the BAR grid (a valid text-hatch sequence, or a phrase
-- | source — a phrase always spans one bar) rather than the default per-beat grid.
boxUsesSeq :: PerfBox -> Boolean
boxUsesSeq box = case box.phrase of
  Just _ -> true
  Nothing -> case box.seq of
    Just s -> isJust (seqPattern (map _.notes s.events) box.seqText)
    Nothing -> false

-- | The PPQ grid a captured phrase is re-quantized onto within one cycle. 96 is
-- | fine enough that onset nuance survives while positions stay exact rationals.
phrasePpq :: Int
phrasePpq = 96

-- | How many BARS a clip spans — so a multi-bar phrase plays over that many cycles
-- | (`slow`) instead of being crammed into one bar (which sounds 2× fast for the
-- | default 2-bar loop). Derived from the capture tempo when known (bar = 4 beats =
-- | 240e6 µs / bpm); salvaged clips that lost their tempo fall back to the 2-bar
-- | default loop window. Rounded — captured loops are bar-aligned.
phraseBars :: MidiClip -> Int
phraseBars c = case c.bpm of
  Just b | b > 1.0 -> max 1 (round (c.lenMicros * b / 240.0e6))
  _ -> 2

-- | A captured phrase → `Pattern (Array Int)` in FLATTEN mode (#27): each note's
-- | absolute onset (rebased to [0, lenMicros)) is re-quantized to the PPQ grid within
-- | ONE cycle — so it RE-TEMPOS to the current clock — simultaneous onsets group into
-- | a chord, and each chord is held to the next onset (legato). Muted source heads
-- | drop out first. The box's transform stack then folds over this exactly as over a
-- | chord progression, so transpose / voice / arp / slow all apply. Velocity/gate drop
-- | at this seam (as they do for chord voices); Original channel mode is #27b-2.
-- | Build a `Pattern (Array Int)` from a set of note events over a clip of `len`
-- | micros, spread across `bars` cycles. Used both for the FLATTEN whole-clip pattern
-- | and, in ORIGINAL mode, per source head.
phrasePatternFrom :: Number -> Int -> Array NoteEvent -> PT.Pattern (Array Int)
phrasePatternFrom len bars evs =
  let stepOf e = clamp 0 (phrasePpq - 1) (round (e.fireUnixMicros / max 1.0 len * toNumber phrasePpq))
      grouped = foldl (\m e -> Map.insertWith (<>) (stepOf e) [ e.pitch ] m) Map.empty evs
      steps = (Map.toUnfoldable grouped) :: Array (Tuple Int (Array Int))
      slotEnd idx s = case steps !! (idx + 1) of
        Just (Tuple s2 _) -> max (s + 1) s2   -- hold to the next onset (legato)
        Nothing -> phrasePpq                   -- last chord holds to the cycle end
      arcs = mapWithIndex
        (\idx (Tuple s pitches) -> compress (s % phrasePpq) (slotEnd idx s % phrasePpq) (pure (nub pitches)))
        steps
  -- spread the one-cycle layout over the clip's true bar count, so it plays at the
  -- captured speed (re-tempo'd to the current clock) and loops every `bars`.
  in slow (Rat.fromInt bars) (stack arcs)

-- | The clip's events with muted source heads removed (a live performance control).
phraseActiveEvents :: PhraseAttach -> Array NoteEvent
phraseActiveEvents ph = filter (\e -> not (elem e.headIdx ph.mutedHeads)) ph.clip.events

-- | Distinct source heads present in the clip, ascending (drives the mute-mask UI).
phraseClipHeads :: PhraseAttach -> Array Int
phraseClipHeads ph = sort (nub (map _.headIdx ph.clip.events))

-- | The FLATTEN-mode pattern: all sounding heads collapsed into one chord stream.
phrasePattern :: PhraseAttach -> PT.Pattern (Array Int)
phrasePattern ph = phrasePatternFrom ph.clip.lenMicros (phraseBars ph.clip) (phraseActiveEvents ph)

boxPattern :: PerfBox -> PT.Pattern (Array Int)
boxPattern box = foldl (\p lyr -> applyLayer lyr p) base box.stack
  where
  base = case box.phrase of
    Just ph -> phrasePattern ph
    Nothing -> case box.seq of
      Nothing -> fromChords []
      Just s ->
        let chords = map _.notes s.events
        in fromMaybe (fromChords chords) (seqPattern chords box.seqText)

-- | Query a box's pattern over this cycle `c` and schedule every chord-event it
-- | yields on the box's channel, positioned by the event's arc within the cycle.
-- | ONSET-GUARDED: only events whose whole STARTS in this query are struck, so a
-- | chord held across many cycles by `slow` is played once and sustained — not
-- | re-struck every bar (its continuation fragments have no onset here). Arp already
-- | exploded into singleton-note onset-events upstream (`arpeggiate`), so those just
-- | flow through the block path, each on its own onset. `boxRealise` only picks the
-- | sink ornament: Block = all notes together, held for the slot; Strum = a fast ms
-- | onset stagger. `slow`/`fast` stretch the slot orthogonally (upstream, in-pattern).
-- | One tapped emitted note, in the shape the emit sites can build cheaply: the
-- | capture closure (PerfTick) stamps `fireUnixMicros` from the tick's wall clock +
-- | `delayMs` and prepends it to the logbook (#28).
type EmitRec = { pitch :: Int, headIdx :: Int, delayMs :: Number, vel :: Int, gateMs :: Number }

scheduleBox :: Midi.MidiOut -> Int -> Number -> Number -> Number -> (EmitRec -> Effect Unit) -> PerfBox -> Effect Unit
scheduleBox out c cycleMs _beatMs baseDelayMs rec box =
  case box.phrase of
    -- ORIGINAL channel mode (#27b): faithful multi-channel playback — each active
    -- source head on its OWN channel (headIdx → the clip's original channel), no chord
    -- stack (a recording plays back as recorded). FLATTEN + all chord boxes fall
    -- through to the single-channel path below.
    Just ph | ph.channelMode == Original ->
      for_ (phraseClipHeads ph) \h ->
        when (not (elem h ph.mutedHeads)) $
          schedulePat out (Routing.odonusHeadChannel h) c cycleMs baseDelayMs RBlock
            (phrasePatternFrom ph.clip.lenMicros (phraseBars ph.clip)
              (filter (\e -> e.headIdx == h) ph.clip.events)) rec
    _ -> schedulePat out box.channel c cycleMs baseDelayMs (boxRealise box.stack) (boxPattern box) rec

-- | Schedule one `Pattern (Array Int)` on one channel over cycle `c` — the shared core
-- | of `scheduleBox` (single channel = chord box / flattened phrase; per head = ORIGINAL
-- | phrase). Onset-guarded and realisation-aware exactly as before, plus a `rec` tap for
-- | the capture logbook (#28) alongside each emitted note.
schedulePat :: Midi.MidiOut -> Int -> Int -> Number -> Number -> Realise -> PT.Pattern (Array Int) -> (EmitRec -> Effect Unit) -> Effect Unit
schedulePat out channel c cycleMs baseDelayMs realise pat rec =
  for_ (query pat (mkState (mkArc (Rat.fromInt c) (Rat.fromInt (c + 1))))) \ev ->
    for_ (onsetWhole ev) \(Arc w) ->
        let startMs = baseDelayMs + Rat.toNumber (w.start - Rat.fromInt c) * cycleMs
            slotMs = max 20.0 (Rat.toNumber (w.stop - w.start) * cycleMs)
            notes = eventValue ev
            -- per-note onset step (ms): strum = a small fixed stagger; block = 0.
            stepMs = case realise of
              RStrum ms -> toNumber ms
              RBlock -> 0.0
            noteDur = max 20.0 (slotMs * 0.9)
        in for_ (mapWithIndex Tuple notes) \(Tuple k note) -> do
             let d = startMs + toNumber k * stepMs
             Midi.scheduleNote out { channel, note, velocity: 90, delayMs: d, durMs: noteDur }
             rec { pitch: note, headIdx: channel, delayMs: d, vel: 90, gateMs: noteDur }

-- | A digital event's whole, but ONLY when its onset falls in this query (whole start
-- | == part start). Analog events and mid-sustain continuation fragments give Nothing,
-- | so the scheduler strikes each event exactly once, on its onset.
onsetWhole :: forall a. PT.Event a -> Maybe Arc
onsetWhole ev = do
  wa@(Arc w) <- eventWhole ev
  let Arc p = eventPart ev
  if w.start == p.start then Just wa else Nothing

-- | Send a chord's notes to the MIDI bus (no state change) and log it to the chyron.
-- | Replace one element of an array, leaving it alone if the index is out.
modifyIx :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyIx i f xs = fromMaybe xs (modifyAt i f xs)

-- | The pass, read off state. `Vetula.Rehearsal` owns the computation; these are
-- | only the three readings the view and the transport ask for.
passIxs :: State -> Array Int
passIxs st = RH.chosen st.pull st.rehearseRoll st.rehearsal

passChords :: State -> Array ChordNode
passChords st = RH.chords st.pull st.rehearseRoll st.rehearsal

-- | The pass as playable events, evenly spaced. Timing is not the point here —
-- | Perform owns that — so one chord a beat is enough to hear the joins.
passEvents :: State -> Array ChyronEvent
passEvents st = spaced 0.0 (passChords st)

-- | Chords laid out one a beat from `t0`, as playable events. `playEvents`
-- | rebases on the FIRST event, so every phrase in a sweep must be laid on one
-- | shared clock rather than each starting at zero.
spaced :: Number -> Array ChordNode -> Array ChyronEvent
spaced t0 cs = mapWithIndex ev cs
  where
  ev i c = { pcs: c.pcs, notes: playNotes c, label: c.label, at: t0 + toNumber i * 700.0, anchor: c.anchor }

-- | Three chords and a breath, so one reading is audibly separate from the next.
phraseGap :: Number
phraseGap = 2800.0

-- | How many readings a sweep will play. Six options either side is thirty-six
-- | phrases — near two minutes, and nobody is comparing the first to the last.
-- | Eight keeps it inside the span of a musical decision.
sweepCap :: Int
sweepCap = 8

-- | **A shelf token as a clip.**
-- |
-- | One `NoteEvent` per note of every chord, at the chord's own onset — which is
-- | the whole point: a sampler reading these times knows where each chord starts
-- | exactly, so its division is a fact rather than a detector's opinion. Notes of
-- | one chord share an onset, which is the cluster Quadrat looks for; declared,
-- | that cluster is exact.
-- |
-- | `heads: 1` — a progression is one voice's worth of material however many
-- | notes a chord has. `bpm` is left absent: the clip carries times, and what
-- | tempo they are read at belongs to whatever plays it.
clipOfSeq :: State -> SavedSeq -> Number -> MidiClip
clipOfSeq st sq now =
  -- SECONDS, not millis. `round` targets a 32-bit Int and epoch millis
  -- (1.79e12) saturate it at 2147483647 — so every clip minted from the same
  -- rebus got the identical id, which is the one field that must not collide.
  -- Seconds (1.79e9) fit until 2038, and Amphora content-addresses anyway, so a
  -- genuine duplicate dedupes on its hash rather than on this.
  { id: "vetula-" <> sq.glyph.alias <> "-" <> show (round (now / 1000.0))
  , events: concat (mapWithIndex evs sq.events)
  , lenMicros: toNumber (length sq.events) * chordMicros
  , heads: 1
  , capturedMicros: now
  , source: "vetula"
  , name: sq.glyph.alias
  , tags: [ "progression" ]
  , notes: ""
  , bpm: Nothing
  , key: Just (noteName (mod st.key.tonic 12) <> " " <> show st.key.mode)
  , context: Nothing
  }
  where
  evs i ev =
    map (\n -> { pitch: n, headIdx: 0, fireUnixMicros: toNumber i * chordMicros
                , vel: 92, gateMs: 700.0 })
      ev.notes

-- | One chord a beat, in micros. The same 700 ms the pass auditions at, so what
-- | a sampler is told matches what you heard when you chose it.
chordMicros :: Number
chordMicros = 700000.0

-- | **Mint a token from a set of paths and put it on the shelf.**
-- |
-- | One path gives a plain sequence; several give one alternation over whole
-- | bracketed readings, so a cycle picks a progression you approved rather than
-- | crossing slots independently into one you never heard.
-- |
-- | The token's `events` are the DISTINCT chords the readings use, which is what
-- | a box's bag is, and its glyph is content-derived — so a settled pass gets a
-- | different rebus from the one you took up, because it is a different
-- | progression. That is the lineage, not a collision.
mintFromRehearsal
  :: forall o m. MonadAff m
  => Array (Array Int) -> H.HalogenM State Action Slots o m Unit
mintFromRehearsal paths = do
  st <- H.get
  when (length paths > 0 && length st.rehearsal > 0) do
    let h = RH.handOff st.rehearsal paths
    mintToken h.chords h.pattern

-- | The whole lattice: every slot's options, each slot alternating on its own.
-- | The generative shape — the rig walks the space per cycle instead of us
-- | choosing a reading now.
mintLattice :: forall o m. MonadAff m => Array Slot -> H.HalogenM State Action Slots o m Unit
mintLattice slots = when (length slots > 0) do
  let bag = nub (concatMap (\sl -> map playNotes sl.options) slots)
      ixOf ns = show (fromMaybe 0 (elemIndex ns bag))
      slotTxt sl = case map ixOf (map playNotes sl.options) of
        [ one ] -> one
        many -> "<" <> joinWith " " many <> ">"
  mintToken bag (joinWith " " (map slotTxt slots))

mintToken
  :: forall o m. MonadAff m
  => Array (Array Int) -> String -> H.HalogenM State Action Slots o m Unit
mintToken chords pattern = do
  let evs = mapWithIndex
              (\i ns -> { pcs: nub (map (\n -> mod n 12) ns), notes: ns
                        , label: show (i + 1), at: toNumber i, anchor: Free })
              chords
      tok = { events: evs, glyph: TGlyph.chordGlyph chords, pattern }
  st <- H.get
  if any (sameToken tok) st.chyronSaved
    then H.modify_ _ { publishMsg = Just "already on the shelf" }
    else H.modify_ \s -> s { chyronSaved = shelve tok s.chyronSaved
                           , perfHeld = Nothing
                           , publishMsg = Just ("⏎ kept · " <> show (length chords) <> " chords") }

-- | **Two tokens are the same progression when they sound the same.**
-- |
-- | Content, not glyph: the glyph is DERIVED from the content, so equal glyphs
-- | almost always mean equal chords — but "almost always" is the wrong standard
-- | for deciding whether to throw one away.
-- |
-- | The pattern counts. The same bag of chords read as one pass and read as an
-- | alternating lattice are different progressions that happen to be built from
-- | the same material, and collapsing them would lose the more interesting one.
sameToken :: SavedSeq -> SavedSeq -> Boolean
sameToken a b = map _.notes a.events == map _.notes b.events && a.pattern == b.pattern

-- | Append, and collapse any duplicates already on the shelf. Keeping a pass
-- | identical to the progression you took up used to mint a second, identical
-- | rebus — two tokens you cannot tell apart, because there is nothing to tell.
shelve :: SavedSeq -> Array SavedSeq -> Array SavedSeq
shelve tok existing = nubByEq sameToken (existing <> [ tok ])

-- | The chords a path names, in order.
pathChords :: State -> Array Int -> Array ChordNode
pathChords st ixs = catMaybes (zipWith (\sl j -> index sl.options j) st.rehearsal ixs)

-- | **The approved paths, resolved back to the slots as they stand now.**
-- |
-- | Read from the stored CONTENT rather than by enumerating the lattice, which
-- | matters most in the case this exists for: when the space is too large to lay
-- | out, your shortlist still has to be readable.
-- |
-- | A mark whose chord has since been dropped simply does not resolve, and is
-- | not shown. It stays in `marked` rather than being deleted — put the option
-- | back and the mark comes back with it, which is kinder than discarding a
-- | decision because of an edit that might be a mistake.
markedRows :: State -> Array { ixs :: Array Int, motion :: Int }
markedRows st = mapMaybe row st.marked
  where
  row notesPer =
    let ixs = zipWith slotIx st.rehearsal notesPer
    in if length notesPer /= length st.rehearsal || any (_ < 0) ixs
         then Nothing
         else Just { ixs, motion: HT.pathMotion (map Voicing notesPer) }
  slotIx sl ns = fromMaybe (-1) (findIndex (\o -> playNotes o == ns) sl.options)

-- | Is this path one you approved? Compared by content, so a mark survives
-- | options being dropped or re-kept underneath it.
pathMarked :: State -> Array Int -> Boolean
pathMarked st ixs = elem (map playNotes (pathChords st ixs)) st.marked

passMotion :: State -> Int
passMotion st = RH.motion st.pull st.rehearseRoll st.rehearsal

-- | Keep a variation in the free-standing tray (no excursion in flight).
keepFreely :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
keepFreely c = do
  st <- H.get
  for_ (varySource st) \src -> do
    let key = playNotes src
        alreadyKept e = e.notes == key
        held o = playNotes o == playNotes c
        toggle e =
          if not (alreadyKept e) then e
          else e { options = if any held e.options
                               then filter (not <<< held) e.options
                               else e.options <> [ c ] }
    if any alreadyKept st.kept
      -- A slot whose variations have all been dropped is back to just its
      -- source, which is no slot at all — so it leaves rather than lingering
      -- as a one-option row that can never vary.
      then H.modify_ \s -> s { kept = filter (\e -> length e.options > 1) (map toggle s.kept) }
      else H.modify_ \s -> s { kept = s.kept <> [ { notes: key, label: src.label, options: [ src, c ] } ] }
    playChordQuiet c

-- | **Sound a chord without capturing it.**
-- |
-- | `playChord` is an audition choke-point and logs to the chyron, which is
-- | right when every click is a compositional act. It is wrong while browsing a
-- | grid of variations on ONE chord: the trace would fill with near-twins and
-- | the progression you meant to lift out of it would be unfindable. So the
-- | Vary lens sounds through here and captures only what you shift-click.
-- |
-- | Not `chyronArmed` — that is the user's own record switch, and a lens
-- | silently flipping it would be a worse surprise than the pollution.
playChordQuiet :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
playChordQuiet c = do
  st <- H.get
  for_ st.midiOut \out ->
    liftEffect $ for_ (playNotes c) \n ->
      Midi.scheduleNote out { channel: st.previewChan, note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }

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

-- | Audition a library clip (#33): replay its captured events once, FAITHFULLY — each
-- | note on its own source channel (`odonusHeadChannel headIdx`, matching ORIGINAL-mode
-- | playback), with its recorded velocity + gate, staggered by its rebased onset. No
-- | tempo re-map and no transform stack: this is "hear the recording as recorded",
-- | routed through whatever output the shell has Vetula pointed at (Continuo / IAC).
-- | How far ahead the REPLAY loop queues notes. One frame's worth plus slack: a
-- | stop leaves at most this much already in WebMIDI's queue.
replayLookaheadMs :: Number
replayLookaheadMs = 120.0

-- | REPLAY loop driver — a WINDOWED scheduler run each frame, ported from Odonus's
-- | `driveReplay` (#151 R2b). It queues only the notes falling in the short window
-- | ahead of the watermark, never a whole loop iteration, so stopping is nearly
-- | instant rather than letting a full queued loop ring out. Each note is a
-- | self-contained `scheduleNoteAtMs` (auto note-off). Also advances the 0..1
-- | playhead the capture surface draws. No-op when nothing is looping.
-- |
-- | Notes sound on the channels they were CAPTURED on (`odonusHeadChannel headIdx`),
-- | the same convention as the clip-library audition, and through `st.midiOut` — so
-- | an ⌥1 AuditionOff silences a region preview too.
driveCaptureReplay :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
driveCaptureReplay = do
  st <- H.get
  for_ st.capture.playing \ps -> do
    nowMs <- liftEffect perfNow
    let loopLenMs = max 1.0 (ps.lenMicros / 1000.0)
        horizon = nowMs + replayLookaheadMs
    -- Each event sits at phase `off` in the loop, so it sounds at
    -- loopStartMs + off + k·loopLenMs; take the first k past the watermark and
    -- queue it if it lands inside this frame's window (≤ one hit per event).
    for_ st.midiOut \out -> liftEffect $ for_ ps.events \e -> do
      let off = e.fireUnixMicros / 1000.0
          k = ceil ((ps.scheduledUntilMs - ps.loopStartMs - off) / loopLenMs)
          atMs = ps.loopStartMs + off + toNumber k * loopLenMs
      when (atMs > ps.scheduledUntilMs && atMs <= horizon) $
        Midi.scheduleNoteAtMs out
          { channel: Routing.toWire (Routing.odonusHeadChannel e.headIdx)
          , note: e.pitch, velocity: e.vel, atMs, durMs: e.gateMs }
    H.modify_ \s -> case s.capture.playing of
      Just p ->
        let elapsed = nowMs - p.loopStartMs
            frac = (elapsed - toNumber (floor (elapsed / loopLenMs)) * loopLenMs) / loopLenMs
        in s { capture = s.capture { playing = Just p
                 { scheduledUntilMs = max p.scheduledUntilMs horizon
                 , playheadFrac = max 0.0 (min 1.0 frac) } } }
      Nothing -> s

-- | All-notes-off on exactly the channels the looping region uses — cuts anything
-- | the lookahead already queued, so stopping (or leaving REPLAY) is silent at once.
-- | Scoped to the region's own channels rather than all 16, so a preview can't
-- | interrupt voices that are still performing.
-- | Where the pointer is on the Review surface, as a time (the surface's µs).
capturePointer :: forall o m. MonadAff m => Int -> Int -> H.HalogenM State Action Slots o m Number
capturePointer cx cy = do
  st <- H.get
  p <- liftEffect $ Pointer.padNorm "vetula-capture-timeline" cx cy
  pure ((CaptureView.bounds st.capture.zoom st.capture.logbook).fromFrac (CaptureView.pointerFrac Horizontal p))

-- | A line to the rig, if this page has one.
rigSend :: forall o m. MonadAff m => String -> H.HalogenM State Action Slots o m Unit
rigSend line = do
  st <- H.get
  for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) line

-- | Now, in the surface's µs (performance time, here) and in Link beats.
vetulaClock :: forall o m. MonadAff m => H.HalogenM State Action Slots o m (Maybe RL.Clock)
vetulaClock = do
  st <- H.get
  traverse (\bin -> liftEffect do
    nowMs <- perfNow
    r <- Clock.read (Binnacle.clock bin)
    pure { micros: nowMs * 1000.0, beat: r.beat, tempo: r.tempo }) st.binnacle

hushCapture :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
hushCapture = do
  st <- H.get
  for_ st.capture.playing \ps ->
    for_ st.midiOut \out -> liftEffect $
      for_ (nub (map (\e -> Routing.odonusHeadChannel e.headIdx) ps.events)) \ch ->
        Midi.sendCC out { channel: Routing.toWire ch, controller: 123, value: 0 }

auditionClip :: forall o m. MonadAff m => MidiClip -> H.HalogenM State Action Slots o m Unit
auditionClip clip = do
  st <- H.get
  for_ st.midiOut \out ->
    liftEffect $ for_ clip.events \e ->
      Midi.scheduleNote out
        { channel: Routing.odonusHeadChannel e.headIdx
        , note: e.pitch
        , velocity: e.vel
        , delayMs: e.fireUnixMicros / 1000.0
        , durMs: e.gateMs }

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
        { id: newId, parentId: Nothing, root: mod s.bass 12, bassPc: mod s.bass 12, bassOct: s.bass / 12
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
              BassVoice -> refootNode (nextBassTone dir c) c
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
      -- Banks: a pad carries its own open voicing, so play it VERBATIM. This has
      -- to come before the triad branch below, which would re-voice it close.
      --
      -- QUIET in the Vary grid: space there is browsing, same as a click, and a
      -- sweep through a hundred near-twins of one chord would bury the trace a
      -- progression gets lifted from. Banks keeps logging — clicking through a
      -- row of banks IS building a progression.
      _ | Just c <- st.hoveredNode ->
            if inVaryGrid st then playChordQuiet c else playChord c
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
    -- Pushed down by the nav (`--tf-bar`) + the 42px CONTEXT bar + the 44px AUDITION
    -- chyron, so the stage clears both top strips; the old bottom voice bar is gone,
    -- so it fills to the window bottom (freed lower strip → future MIDI-flow chyron).
    [ HP.style ("position: relative; margin-top: calc(var(--tf-bar) + 44px); width: 100%; height: calc(100vh - 132px); min-height: 620px; overflow: hidden; border-radius: 8px; background: " <> canvasBg <> ";") ]
    [ HH.div [ HP.style "position: absolute; inset: 0;" ] [ surface st ]
    -- Scene recall belongs to the INSTRUMENT, not to Perform. Its entry point
    -- has always been the session menu in `contextBar`, which renders on every
    -- stage — but the modal itself was inside `performSurface`, so anywhere
    -- else the click set the flag and nothing appeared. Loading a scene is legal
    -- wherever you can use one, which is certainly Rehearse and reasonably Hunt.
    , perfRecallModal st
    -- CONTEXT is now a docked control bar between the nav and the chyron (the last
    -- floating overlay is gone, reclaiming the whole left column): key · scale ·
    -- palettes · lens · rig/help. See `contextBar`.
    , contextBar st
    -- The Tank & Progression card is retired (Tank overhaul §10.6): the chyron is
    -- now the single surface for collect · select · reorder · bundle/unbundle, so
    -- the tank tiles, the tonnetz stack, arrange/grow, and the built-progression
    -- panel (with its ▶ preview) are all superseded. The underlying code —
    -- specimens, `Vetula.Between` (the cadence bridge), ArrangeSpec/SequenceSpec —
    -- is kept dormant in the source for re-homing onto the chyron later.
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
-- | The PERFORM river's width. ONE constant, read by the river column and by both
-- | docked bars, because the whole arrangement is the bars inseting their right
-- | edge by exactly what the river occupies. Two numbers here would drift into
-- | either a covered control or a seam of paper beside the river.
-- |
-- | Bounded from above by the CONTEXT bar, whose non-flexible content measures
-- | ~1111px of a 1512 viewport — and MORE whenever the transient `publishMsg`
-- | ("scene loaded") is showing, which is exactly what caught the first attempt
-- | at 360px. 330px leaves room for that message; the message itself now
-- | ellipsises rather than pushing, so the bar has two defences instead of one.
riverWidth :: String
riverWidth = "330px"

-- | How far the docked bars pull their right edge in. Perform is the only stage
-- | with a river, so it is the only stage that squashes.
barRightInset :: State -> String
barRightInset st = if st.stage == Perform then riverWidth else "0"

chyronBar :: forall m. State -> H.ComponentHTML Action Slots m
chyronBar st =
  HH.div
    [ HP.style ( "position: fixed; top: calc(var(--tf-bar) + 44px); left: 0; right: " <> barRightInset st <> "; z-index: 39; box-sizing: border-box; "
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
          -- ⏎ save (bundle) shows for ANY selection — single chord or span — so the
          -- rebundle path is reliably available while editing a checked-out
          -- progression (not just when a multi-chord span happens to be selected).
          <> ( case st.chyronSel of
                 Just sel ->
                   [ HH.button
                       [ HP.style "border: 1px solid #b8860b; background: #fbf6ea; color: #7a5c00; font-size: 11px; line-height: 1; cursor: pointer; padding: 2px 6px; border-radius: 3px;"
                       , HP.title "bundle the selected chord(s) into a glyph token (⏎)"
                       , HE.onClick \_ -> SaveChyronSel ]
                       [ HH.text ("⏎ bundle " <> show (sel.hi - sel.lo + 1)) ] ]
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
  -- replay it with timing; ✎ unbundles it back into the buffer to edit; × deletes.
  -- Tooltip carries the chord names.
  -- On REHEARSE the rebus is how you choose what to work on: click takes it up.
  -- These glyphs already ARE the shelf of saved progressions, so the stage does
  -- not draw a second row of them — that was the same set twice in one window.
  savedToken i s =
    let held = st.perfHeld == Just i
        upNow = st.stage == Rehearse && st.rehearsalFrom == Just i && length st.rehearsal > 0
        lit = held || upNow
    in HH.span
      [ HP.style ("position: relative; flex: 0 0 auto; display: inline-flex; align-items: center; gap: 3px; border: 1px solid "
                   <> (if lit then "#b8860b" else "#cdbb8c")
                   <> "; background: " <> (if lit then "#fbf1d6" else "#f6efdc")
                   <> "; box-shadow: " <> (if lit then "0 0 0 2px #f1e2b4" else "none")
                   <> "; border-radius: 4px; padding: 3px 6px; line-height: 1;")
      , HP.draggable true
      , HE.onDragStart \_ -> PerfPickup i
      , HP.title ("saved · " <> joinWith " " (map _.label s.events)
                   <> (if st.stage == Rehearse then " · click takes it up to rehearse" else " · click plays")
                   <> " · ✎ unbundles to the buffer · shift-click / drag → a Perform box") ]
      [ HH.span
          [ HP.style "display: inline-flex; align-items: center; gap: 3px; cursor: pointer;"
          , HE.onClick \e ->
              if ME.shiftKey e then PerfPickup i
              else if st.stage == Rehearse then TakeUp i
              else PlaySaved i ]
          (faIcons s.glyph)
      , HH.button
          [ HP.style "position: absolute; top: -5px; left: -3px; z-index: 2; border: 1px solid #cdbb8c; background: #f6efdc; color: #7a5c00; font-size: 10px; line-height: 1; cursor: pointer; padding: 0 3px; border-radius: 8px;"
          , HP.title "check out to the buffer to edit — the token leaves the shelf; ⏎ save re-bundles a new one"
          , HE.onClick \_ -> Unbundle i ]
          [ HH.text "✎" ]
      -- Declare this progression to the sampler. Not a MIDI route: Quadrat is a
      -- different origin and learns the chords by being TOLD, which is exact
      -- where listening to the wire is a detector's best guess.
      , HH.button
          [ HP.style "position: absolute; bottom: -5px; left: -3px; z-index: 2; border: 1px solid #cdbb8c; background: #f6efdc; color: #4a6a3a; font-size: 9px; line-height: 1; cursor: pointer; padding: 0 3px; border-radius: 8px;"
          , HP.title "send to Quadrat to sample — the chords, their times and this rebus"
          , HE.onClick \_ -> ToQuadrat i ]
          [ HH.text "◴" ]
      , HH.button
          [ HP.style "position: absolute; top: -5px; right: -3px; z-index: 2; border: 1px solid #cdbb8c; background: #f6efdc; color: #b06a5a; font-size: 10px; line-height: 1; cursor: pointer; padding: 0 3px; border-radius: 8px;"
          , HP.title "delete this saved sequence"
          , HE.onClick \_ -> DeleteSaved i ]
          [ HH.text "×" ]
      ]
  -- one chip = the chord's mini stave-glyph (same as the Tank), name-free. Hover
  -- + space auditions it; click selects this one chord; shift-click extends the
  -- range from the anchor; DRAG it to reorder the buffer (§10.4). In-span chips
  -- wear a warm wash; the endpoints a gold rim; the dragged chip dims.
  chyronChip i ev =
    let inSel = case st.chyronSel of
                  Just sel -> i >= sel.lo && i <= sel.hi
                  Nothing -> false
        isEnd = case st.chyronSel of
                  Just sel -> i == sel.lo || i == sel.hi
                  Nothing -> false
        hov = st.hoveredChyron == Just i
        dragging = st.chyronDrag == Just i
        bg = if inSel then "#efe6c8" else "#faf7ee"
        brd = if isEnd then "#b8860b" else if inSel then "#cdbb8c" else "#d8ceb4"
        pcNames = joinWith " " (map noteName (sort ev.pcs))
        -- a delete × surfaces on hover (its own element, above the drag handle so it
        -- stays clickable). Bigger hit target than before.
        delX = if hov
          then [ HH.button
                   [ HP.style "position: absolute; top: -4px; right: -4px; z-index: 3; border: 1px solid #e4d9be; background: #faf7ee; color: #b06a5a; font-size: 12px; line-height: 1; cursor: pointer; padding: 0 3px; border-radius: 8px;"
                   , HP.title "delete this audition"
                   , HE.onClick \_ -> DeleteChyron i ]
                   [ HH.text "×" ] ]
          else []
    in HH.span
        -- the whole chip is a DROP target for reorder; the inner glyph is the drag
        -- HANDLE. The container is deliberately NOT draggable — a draggable container
        -- swallows child-button clicks, which made the corner × unresponsive.
        [ HP.style ("position: relative; flex: 0 0 auto; white-space: nowrap; border: 1px solid " <> brd
                     <> "; background: " <> bg <> "; border-radius: 3px; padding: 0 1px; line-height: 0; opacity: "
                     <> (if dragging then "0.4" else "1") <> ";")
        , HP.title (ev.label <> (if pcNames == "" then "" else " · " <> pcNames))
        , HE.onDragOver PerfDragOver
        , HE.onDrop \_ -> ChyronDropOn i
        , HE.onMouseEnter \_ -> HoverChyron (Just i)
        , HE.onMouseLeave \_ -> HoverChyron Nothing ]
        ( delX <>
          [ HH.div
              [ HP.style "cursor: grab; line-height: 0;"
              , HP.draggable true
              , HE.onDragStart \_ -> ChyronDragStart i
              , HE.onDragEnd \_ -> ChyronDragEnd ]
              [ SE.svg
                  [ SA.viewBox (-18.0) (-22.0) 36.0 44.0, SA.width 30.0, SA.height 38.0
                  , HE.onClick \e -> ChyronClick i (ME.shiftKey e) ]
                  (chordGlyph [] 0.0 0.0 ev.notes) ]
          ]
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

-- | The CONTEXT bar — a thin control strip docked between the top nav and the
-- | AUDITION chyron (Tank-overhaul follow-on: kills the last floating overlay,
-- | reclaiming the whole left column). Horizontal: key · scale · [family] ·
-- | palettes · [borrow] · lens, with the rig connection + help pushed right. The
-- | fields keep their little labels stacked over each control, so it reads as a
-- | labelled toolbar. Dropdowns open DOWN over the surface (z above the chyron),
-- | so the bar must not clip overflow.
-- | The STAGE TABS — Vetula's one mode control, hard left in the secondary nav
-- | so it sits directly under the shell's transport. Three peers, in the order
-- | material flows through them; see `Stage`.
-- |
-- | HUNT returns to `lastLens`, so the projection you were last using is where
-- | you land — the tab is the stage, the dropdown beside it (a Hunt control) is
-- | the projection.
stageTabs :: forall m. State -> H.ComponentHTML Action Slots m
stageTabs st =
  HH.div
    [ HP.style "display: flex; flex: 0 0 auto; border: 1px solid #00000026; border-radius: 6px; overflow: hidden; box-shadow: 0 1px 2px #0000001a;" ]
    [ tab (isHunt st.stage) (Hunt st.lastLens) "HUNT" "hunt harmonic space — catch chords into the tank"
    , tab (st.stage == Rehearse) Rehearse "REHEARSE" "a progression with alternatives at every chord — run it until it settles"
    , tab (st.stage == Perform) Perform "PERFORM" "play the voices, with the capture river alongside"
    , tab (st.stage == Review) Review "REVIEW" "the whole take — cherry-pick a phrase into the clip library"
    ]
  where
  tab active target label tip =
    HH.button
      [ HP.title tip
      , HE.onClick \_ -> SetStage target
      , HP.style ("padding: 5px 14px; border: none; cursor: pointer; font-family: Georgia, serif; font-size: 11px; letter-spacing: 0.12em; "
                   <> (if active then "background: linear-gradient(#c8a86a,#b8975a); color: #1c1a12; font-weight: 600;"
                                 else "background: linear-gradient(#e9e5d9,#dcd8c9); color: #5a564b;")) ]
      [ HH.text label ]

contextBar :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
contextBar st =
  HH.div
    -- `white-space: nowrap` INHERITS, so one declaration here stops every chip in
    -- the bar breaking its own label across two lines. `flex-wrap: nowrap` alone
    -- was not enough: it keeps the items on one row, but each item still shrinks
    -- and wraps its text inside itself — which is what put "scene loaded",
    -- "horse-bell-bomb" and "continuo ✓" on two lines and grew the bar.
    [ HP.style ( "position: fixed; top: var(--tf-bar); left: 0; right: " <> barRightInset st <> "; z-index: 40; box-sizing: border-box; "
        <> "display: flex; align-items: center; flex-wrap: nowrap; white-space: nowrap; gap: 10px; padding: 0 12px; height: 44px; overflow: visible; "
        <> "background: linear-gradient(#f3eee0,#ece5d0); border-bottom: 1px solid #0000000f; box-shadow: 0 1px 3px #0000000d;" ) ]
    -- LEFT: the stage tabs, then ONLY the controls that mean something in the
    -- stage you're in. RIGHT: the housekeeping and the harmonic authority.
    --
    -- The old bar was one flat row of ten slots regardless of mode, of which six
    -- (family · palettes · borrow · lens · shake · reset) were Hunt-only — lit and
    -- clickable while you performed, meaning nothing. Meanwhile the one control
    -- that WAS live in Perform, the capture switch, wasn't in the bar at all: it
    -- floated absolutely-positioned over the surface, because the bar had no
    -- notion of stage to hang it on. Exactly inverted.
    --
    -- Two groups never move — the tabs (far left, beside the shell's transport)
    -- and key/scale (far right, under the shell's pitch set, which they feed).
    -- Only the middle-left group changes with the stage, so it's one contiguous
    -- region you learn to expect rather than a row that rearranges under you.
    ( [ stageTabs st ]
        <> stageControls
        <> [ HH.div [ HP.style "flex: 1 1 auto; min-width: 8px;" ] [] ]
        -- The one item allowed to give ground. It's transient status, so when the
        -- bar is tight it should ellipsis rather than push the controls — which is
        -- what it was doing: "scene loaded" appearing was enough to overflow the
        -- squashed Perform bar.
        <> [ case st.publishMsg of
               Just m -> HH.span
                 [ HP.style ( "font-size: 11px; color: #7a6a3a; font-family: ui-monospace, monospace; "
                     <> "flex: 0 1 auto; min-width: 0; overflow: hidden; text-overflow: ellipsis;" ) ]
                 [ HH.text m ]
               Nothing -> HH.text ""
           , sessionMenu
           , divider
           ]
        -- The harmonic column — HUNT ONLY. Vetula's key and scale are the source of
        -- the pitch set in the shell's top nav and of Odonus's inherited-context
        -- readout, so the VALUE still travels everywhere; it is the CONTROL that
        -- has no business in Perform or Review.
        --
        -- Removed from those two stages 2026-08-07 (AC), for three reasons that
        -- turn out to be one:
        --
        --   * The chord sets are deliberately free — borrowed chords, and
        --     progressions assembled across incompatible scales. A performance
        --     built that way is not "in a key", so offering to change its key
        --     asks a question the material cannot answer.
        --   * A global mode switch is un-Tidal. Transposition belongs in the
        --     pattern language, as a function over voices, not as an ambient
        --     setting the whole surface sits inside.
        --   * It moved the music without moving its identity. `transposeChord`
        --     keeps the chord id, a token's glyph is frozen at save time from its
        --     own event content, and the boxes read that snapshot while `buildPerf`
        --     sends the rig the live (transposed) path — so one glyph could name
        --     two different chord sets, differently on Solo and on Atlantis.
        --
        -- That last one is the real indictment: Solo and Atlantis are supposed to
        -- be the same computation with the same result, and a stage-level key was
        -- a lever that could quietly break that equivalence.
        --
        -- What may replace it: a Tidal-style transposition over all voices, or a
        -- transposition MAPPED over selected ones. Both are pattern functions, so
        -- both keep the identity honest. Neither is built.
        <> (if isHunt st.stage then harmonicColumn else [])
        <> [ midiChip st.midiName
           , HH.button [ HP.style helpBtnStyle, HP.title "keys & help", HE.onClick \_ -> ToggleHelp ] [ HH.text "ⓘ" ]
           ]
    )
  where
  -- Key · scale · divider. Only mounted in Hunt (see the note at the call site),
  -- so the Select children only exist where they can mean something.
  harmonicColumn =
    [ HH.slot (Proxy :: _ "keySelect") unit Select.component
        ((Select.defaultInput keyOptions) { selected = Just (show st.key.tonic), placeholder = "Key", minWidth = Just "72px" })
        \(Select.Selected v) -> SelectKey v
    , HH.slot (Proxy :: _ "scaleSelect") unit Select.component
        ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue st.key.mode), searchable = true })
        \(Select.Selected v) -> SelectScale v
    , divider
    ]

  -- The stage-specific group. HUNT gets its projection picker and the pool
  -- controls; PERFORM and REVIEW share the capture controls, deliberately
  -- identical and in the same place, so ◆ mark doesn't move when you change
  -- stage to look at what you just marked.
  stageControls = case st.stage of
    Hunt _ -> huntControls
    -- Rehearse's controls live in its own pane: they are about the progression
    -- in front of you, not about the app's mode.
    Rehearse -> []
    Perform -> captureControls
    Review -> captureControls

  -- The projection picker is a HUNT control, so it exists only while hunting.
  -- It used to sit in the bar permanently, displaying `browseOr lastBrowse view`
  -- — a *remembered* projection presented as the current one, because with
  -- Perform up there was no honest value for it to show. Now it never lies.
  huntControls =
    [ HH.slot (Proxy :: _ "viewSelect") unit Select.component
        ((Select.defaultInput browseOptions)
           { selected = Just (viewtypeValue (huntOr st.lastLens st.stage)), minWidth = Just "116px" })
        \(Select.Selected v) -> SetStage (Hunt (viewtypeFromValue v)) ]
      <> shakeChip
      <> familyField
      <> [ divider ]
      <> [ HH.slot (Proxy :: _ "paletteSelect") unit MultiSelect.component
             ((MultiSelect.defaultInput paletteOptions)
                { selected = activeLayerLabels, placeholder = "palettes", maxLabels = 3, minWidth = Just "128px" })
             \(MultiSelect.SelectedMany vs) -> SetLayers vs ]
      <> borrowField
      <> resetChip

  -- ◆ mark · the running counts · clear. Lifted out of the roll's own header:
  -- they're shared by PERFORM and REVIEW, and a control that belongs to two
  -- stages belongs to the chrome, not to either surface. Same slot as Odonus's.
  captureControls =
    [ HH.button
        [ HP.style "border: 1px solid #d8c98a; background: #fdf7e4; color: #8a6a10; cursor: pointer; padding: 3px 12px; border-radius: 5px; font-size: 11px; font-family: Georgia, serif;"
        , HP.title "flag the last couple of bars as a good bit"
        , HE.onClick \_ -> CaptureMark ]
        [ HH.text "◆ mark" ]
    , HH.span [ HP.style "font-size: 10px; color: #9a9482; font-family: 'SF Mono', Menlo, monospace;" ]
        [ HH.text (show (Logbook.noteCount st.capture.logbook) <> " notes · " <> show (length st.capture.logbook.marks) <> " ◆") ]
    , HH.button
        [ HP.style "border: 1px solid #e2ddcc; background: transparent; color: #a09a88; cursor: pointer; padding: 3px 10px; border-radius: 5px; font-size: 10px; font-family: Georgia, serif;"
        , HP.title "clear the Review surface: its notes, marks and loops (on the rig too: vetula $ clear)"
        , HE.onClick \_ -> CaptureClear ]
        [ HH.text "clear" ]
    ]

  -- a way back to the fitted view (scroll to zoom · drag to pan), once it's moved.
  resetChip =
    if st.viewZoom /= 1.0 || st.viewCx /= 0.0 || st.viewCy /= 0.0 then
      [ HH.button
          [ HP.style "border: 1px solid #dcdcdc; background: #fafafa; color: #6a6a6a; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px; white-space: nowrap;"
          , HP.title "reset the view · scroll to zoom · drag to pan"
          , HE.onClick \_ -> ResetView ]
          [ HH.text "reset view" ] ]
    else []
  -- a hairline group separator.
  divider = HH.div [ HP.style "width: 1px; height: 22px; background: #00000016;" ] []
  -- The session badge doubling as the session/scene/chyron command menu (the
  -- Perform header's old control row, moved here). Click the 3-glyph badge to drop
  -- the menu; a transparent backdrop closes it; each item runs via `PerfMenuPick`
  -- (close, then act). Global — reachable from every view, not just Perform.
  sessionMenu =
    HH.div [ HP.style "position: relative; display: flex; align-items: center;" ]
      ( [ HH.button
            [ HP.style ("display: inline-flex; align-items: center; gap: 7px; padding: 3px 10px; border: 1px solid #d8cfa8; border-radius: 5px; cursor: pointer; background: "
                         <> (if st.perfMenuOpen then "#f3ead2" else "#faf7ee") <> ";")
            , HP.title "session · scenes · chyron housekeeping"
            , HE.onClick \_ -> PerfMenuToggle ]
            [ badgeGlyphs st.perfSession
            , HH.span [ HP.style "font-size: 11px; color: #6a5a2a; letter-spacing: 0.03em;" ]
                [ HH.text sessionName ]
            , HH.span [ HP.style "font-size: 10px; color: #b0a684;" ] [ HH.text "▾" ] ] ]
          <> (if st.perfMenuOpen then [ menuBackdrop, menuDropdown ] else []) )
  badgeGlyphs sess =
    HH.span [ HP.style "display: inline-flex; align-items: center; gap: 4px;" ]
      (if sess.alias == "" then [ HH.text "…" ]
       else map (\name -> faIcon { icon: name, color: "#2a2a2a" }) (split (Pattern "-") sess.alias))
  sessionName = if st.perfSession.name == "" then st.perfSession.alias else st.perfSession.name
  menuBackdrop =
    HH.div [ HP.style "position: fixed; inset: 0; z-index: 45;", HE.onClick \_ -> PerfMenuClose ] []
  menuDropdown =
    HH.div
      [ HP.style "position: absolute; top: 38px; left: 0; z-index: 46; min-width: 200px; background: #fff; border: 1px solid #e0d8bf; border-radius: 7px; box-shadow: 0 8px 28px rgba(0,0,0,0.16); padding: 5px 0; overflow: hidden;"
      , HE.onClick \e -> PerfStopClick e PerfNop ]
      [ menuItem hasFilled ("⬡ save scene #" <> show st.perfSession.nextScene) SaveScene
      , menuItem true "↴ load scene…" PerfOpenRecall
      , menuItem true "↻ new session" PerfNewSession
      , menuDivider
      , menuItem hasChyron "⌫ clear chyron" ClearChyron
      , menuItem hasChyron "≡ de-dupe chyron" DedupeChyron
      ]
    where
    hasFilled = any (\b -> isJust b.seq) st.perfBoxes
    hasChyron = length st.chyron > 0
  menuDivider = HH.div [ HP.style "height: 1px; background: #efe8d4; margin: 4px 0;" ] []
  menuItem enabled label act =
    HH.button
      [ HP.style ("display: block; width: 100%; text-align: left; border: none; background: transparent; padding: 6px 14px; font-size: 12px; "
                   <> (if enabled then "color: #4a4a4a; cursor: pointer;" else "color: #c4bfa8; cursor: default;"))
      , HP.enabled enabled
      , HE.onClick \_ -> if enabled then PerfMenuPick act else PerfNop ]
      [ HH.text label ]
  -- Perform as its own button — a mode apart from the browse projections. Filled
  -- dark when active; a toggle, so clicking it while in Perform returns to the last
  -- browse view (a guaranteed way back, since re-picking the dropdown's current
  -- value wouldn't fire).
  -- shake re-rolls Explore's relatives; only meaningful while Explore is showing.
  -- The Banks lens borrows the same chip with its own verb: there it re-walks
  -- all nine banks from a fresh number.
  shakeChip = case st.stage of
    Hunt Explore -> [ rollChip "shake ⟳" "re-roll the relatives around each seed" ShakeGenerate ]
    Hunt Pads -> [ rollChip "shuffle ⟳" "re-walk all nine banks from a new seed" ShufflePads ]
    Hunt Vary -> [ rollChip "shuffle ⟳" "re-draw all nine cells from a new seed" ShuffleVary ]
    _ -> []
  rollChip label tip act =
    HH.button
      [ HP.style "border: 1px solid #cdbb8c; background: #fbf6ea; color: #7a5c00; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px; white-space: nowrap;"
      , HP.title tip
      , HE.onClick \_ -> act ]
      [ HH.text label ]
  -- the borrow-scale picker only appears when the BORROWED color layer is
  -- engaged — it is that layer's source, meaningless otherwise (AC, 2026-07-31).
  -- Kept with a small inline label (unlike key/scale) since it appears
  -- contextually — a bare dropdown popping in would be a mystery.
  borrowField =
    if Set.member LayerBorrowed st.colorLayers then
      [ inlineField "borrow"
          [ HH.slot (Proxy :: _ "borrowSelect") unit Select.component
              ((Select.cascadingInput borrowGroups) { selected = Just (fromMaybe "off" st.borrowMode), searchable = true })
              \(Select.Selected v) -> BorrowFrom v ]
      ]
    else []
  labelStyle = "font-size: 10px; color: #9a9a9a; letter-spacing: 0.1em; text-transform: uppercase;"
  inlineField lbl controls =
    HH.div [ HP.style "display: flex; align-items: center; gap: 5px;" ]
      ([ HH.span [ HP.style labelStyle ] [ HH.text lbl ] ] <> controls)
  -- the color-overlay layers are now a compact MultiSelect (was a row of
  -- swatch chips): one option per layer, the active set controlled from
  -- `colorLayers` and written back via `SetLayers`.
  paletteOptions = map (\l -> { value: layerLabel l, label: layerLabel l }) allColorLayers
  activeLayerLabels = map layerLabel (filter (\l -> Set.member l st.colorLayers) allColorLayers)
  -- a contextual scale picker for the focused family (click a keyboard key to
  -- focus one) — this is what lets two families hold different modes at once.
  familyField = case st.focusedFamily >>= (\sid -> find (\c -> c.id == sid) st.chords) of
    Just seed ->
      let famMode = (fromMaybe st.key (Map.lookup seed.id st.familyScale)).mode
      in [ inlineField ("family " <> noteName seed.root)
             [ HH.slot (Proxy :: _ "familyScaleSelect") unit Select.component
                 ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue famMode), searchable = true })
                 \(Select.Selected v) -> ReflavourFamily v ] ]
    Nothing -> []
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

-- | The Stage frame: the pick-mode cloud always wins; otherwise the active View
-- | renders. `Perform` is its own surface; every `Browse` viewtype is one branch.
surface :: forall m. State -> H.ComponentHTML Action Slots m
surface st
  | length st.genSel > 0 && length st.candidates > 0 = pickSurface st
  | otherwise = case st.stage of
      Perform -> performSurface st
      Rehearse -> rehearseSurface st
      Review -> reviewSurface st
      Hunt Fifths -> circleFifthsSurface st
      Hunt Tonnetz -> tonnetzSurface st
      Hunt Lattice -> latticesSurface st
      Hunt Explore -> generativeSurface st
      Hunt Pads -> padsSurface st
      Hunt Vary -> varySurface st

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
  , glyph: TGlyph.chordGlyph chords
  -- A scene stores a box's `seqText` in its own right, so a recalled token
  -- needs none of its own.
  , pattern: ""
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
boxesFromDoc doc = mapWithIndex voiceToBox doc.voices
  where
  chordsOf name = maybe [] _.chords (find (\s -> s.name == name) doc.sources)
  -- a loaded scene numbers its cards 1..n, as a loaded Tidal file reuses d1
  voiceToBox i v =
    let cs = maybe [] chordsOf v.source
    in { cardId: i + 1
       , channel: v.channel
       , label: "P" <> show v.channel
       , seq: if length cs == 0 then Nothing else Just (mkSavedSeq cs)
       , stack: v.stack
       , seqText: v.seqText
       , muted: v.muted
       , term: v.term
       , phrase: Nothing   -- phrase boxes aren't carried in the eDSL doc (#27)
       }

-- | The smallest card number no card has.
freeCardId :: Array PerfBox -> Int
freeCardId boxes = fromMaybe (length boxes + 1)
  (find (\n -> not (elem n (map _.cardId boxes))) (range 1 (length boxes + 1)))

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

-- The LIVE/REPLAY switch that used to float here (absolute, top-right of the
-- surface) is gone: it was the mode control for a mode the type didn't admit,
-- with nowhere in the chrome to stand. It's now the PERFORM/REVIEW stage tabs in
-- `stageTabs`. The roll's own header went the same way — ◆ mark, the counts and
-- clear are STAGE CONTROLS, so they live in the nav beside the tabs, in the same
-- slot Odonus puts them (`captureControls`).

-- | The LIVE roll: `Capture.River`, flowing RIGHT — notes are emitted at the strip's
-- | left edge, next to the voice that played them, and age away from the voices at a
-- | constant speed. A river, NOT the whole-session fit: the fit renderer rescales on
-- | every new note (the roll lurches) and squeezes its marks toward slivers as the
-- | take grows. That renderer is right for REPLAY and wrong here.
-- | The river as a FIXED column running from the shell nav to the window bottom,
-- | mirroring Odonus's full-height capture surface. It used to be a flex sibling
-- | of the voices inside the padded stage, which left a 30px seam of paper between
-- | it and the bars above — the padding was doing its job, but the river is not
-- | stage content and should not be inset by the stage's margin.
-- |
-- | The two docked bars inset their right edge by `riverWidth` instead, so nothing
-- | is covered: the CONTEXT and AUDITION bars squash leftward and the river owns
-- | its column outright.
-- |
-- | z-index sits BELOW both bars deliberately. If the CONTEXT bar's content ever
-- | outgrows the squashed width, it overflows visibly over the river rather than
-- | being silently painted under it — a visible bug beats an unclickable control.
riverColumn :: forall m. State -> H.ComponentHTML Action Slots m
riverColumn st =
  HH.div
    [ HP.style ( "position: fixed; top: var(--tf-bar); right: 0; bottom: 0; width: " <> riverWidth <> "; "
        <> "z-index: 38; display: flex; flex-direction: column; background: #0b0a07; border-left: 1px solid #2a281f;" ) ]
    [ riverPane st ]

riverPane :: forall m. State -> H.ComponentHTML Action Slots m
riverPane st =
  HH.div
    [ HP.style "flex: 1 1 auto; min-height: 0; position: relative; overflow: hidden;" ]
    [ River.riverPanel
        { flow: River.FlowRight, headColor: captureHeadColor }
        { nowMicros: st.nowMicros, notes: st.riverNotes, marks: st.capture.logbook.marks }
    ]

-- | The REPLAY roll — the shared whole-session surface, left to right: the
-- | oldest notes at the left, now at the right, marks numbered 1, 2, 3 across,
-- | as on Odonus. The Perform river runs the other way (the newest note at its
-- | left edge, ageing rightward); Review is the session laid out from its
-- | start, not that strip slid over (AC's drawing, 2026-10-03).
capturePane :: forall m. State -> H.ComponentHTML Action Slots m
capturePane st =
  HH.div
    [ HP.style "flex: 1 1 auto; min-height: 0; position: relative; overflow: hidden;" ]
    [ capturePanel captureWiring st.capture ]
  where
  captureWiring =
    { orientation: Horizontal
    , timelineId: "vetula-capture-timeline"
    , headColor: captureHeadColor
    , contextSummary: \_ -> Nothing
    , regionDown: \i _ _ _ -> CaptureRegionSelect i
    , stopPlay: CaptureStopSel
    , saveClip: CaptureSaveClip
    , saveScene: Nothing
    , toggleContext: CaptureToggleContext
    , setZoom: CaptureZoom
    , machine: "vetula"
    , ownCode: _.patch
    , toggleCode: CaptureToggleCode
    , toLimulus: CaptureToLimulus
    , edits: if st.rigLoops then Just { trim: CaptureTrim, undo: CaptureUndo, cut: Just { arm: CaptureCutArm, down: CaptureCutDown } } else Nothing
    }

-- | Colour a captured note by its source channel/voice (up to six distinct hues).
captureHeadColor :: Int -> String
captureHeadColor h = case h `mod` 6 of
  0 -> "#2f5fb0"
  1 -> "#b0492f"
  2 -> "#2f8a5c"
  3 -> "#b07a2f"
  4 -> "#6a4a8a"
  _ -> "#2f7d8a"

-- | The REVIEW surface — the whole-session roll, full bleed. The negative margins
-- | cancel the surface's own padding so it reaches all four edges, exactly as
-- | Odonus's does. Its controls (◆ mark · counts · clear) are in the nav, not
-- | here: they're shared with PERFORM, so putting them on the surface would move
-- | them under you every time you changed stage.
reviewSurface :: forall m. State -> H.ComponentHTML Action Slots m
reviewSurface st =
  HH.div
    [ HP.style "position: absolute; inset: 0; display: flex; flex-direction: column; align-items: stretch; padding: 30px 28px;" ]
    [ HH.div
        -- Bleeds on three sides: the capture surface is the whole stage here,
        -- not something laid out within it. Not the top: the 44px AUDITION
        -- chyron lies over the stage's top edge, and bled up under it the
        -- surface's own top row (zoom, ✂ cut, trim, undo) was hidden.
        [ HP.style "flex: 1 1 auto; min-height: 0; margin: 14px -28px -30px -28px; display: flex; flex-direction: column; background: #0b0a07; border-top: 1px solid #2a281f;" ]
        [ capturePane st ]
    ]

-- | The PERFORM surface: voices in the left two thirds, the capture river as a
-- | narrow strip down the right third. Notes enter the strip at ITS left edge,
-- | next to the voice that played them, and age rightward (`HorizontalOutward`).
-- |
-- | This replaced the full-bleed VERTICAL tracker band of #28b: the vertical axis
-- | read well on its own but cost Vetula too much of its voices. Differentiation
-- | from Odonus is now POSITION (a side strip vs a full-width surface) and time
-- | DIRECTION (outward from the voices vs oldest-first), not the axis.
performSurface :: forall m. State -> H.ComponentHTML Action Slots m
performSurface st =
  HH.div
    [ HP.style ( "position: absolute; inset: 0; display: flex; flex-direction: column; align-items: stretch; "
        <> "justify-content: flex-start; gap: 22px; padding: 30px 28px; padding-right: calc(" <> riverWidth <> " + 28px);" ) ]
    ( body <> [ riverColumn st, perfEditModal st, perfPhrasePickModal st ] )
  where
  body =
      [ HH.div
          -- the voices now take the full surface; the river is no longer a flex
          -- sibling but a fixed column, so the padding-right above is what keeps
          -- the cards clear of it.
          [ HP.style "flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; gap: 22px; width: 100%; overflow-y: auto;" ]
          -- FX palette floated to the top of the surface (holding pattern — its final
          -- home and framing, "training wheels for Tidal" vs "starter-pack
          -- suggestions", is a parked design question). AC, 2026-08-03.
          -- The scene/session controls (save · new session · scenes · the 3-glyph
          -- badge) live in the secondary nav's session menu (⋯).
          [ fxPalette st
          , HH.div
              [ HP.style "display: flex; gap: 18px; flex-wrap: wrap; justify-content: flex-start; align-items: flex-start; width: 100%;" ]
              (mapWithIndex (perfBox st) st.perfBoxes <> [ addPlayerTile ])
          ]
      ]

  -- a dashed ＋ tile sitting inline with the cards: the Perform surface is a
  -- growable palette of voices, one per MIDI channel (1..16), not a fixed four.
  addPlayerTile =
    HH.button
      [ HP.style "width: 208px; min-height: 118px; border: 2px dashed #d8ceb4; background: transparent; border-radius: 10px; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 4px; cursor: pointer; color: #b0a684;"
      , HP.title "add a player on the next free MIDI channel"
      , HP.enabled (length st.perfBoxes < 16)
      , HE.onClick \_ -> PerfAddBox ]
      [ HH.div [ HP.style "font-size: 30px; line-height: 1;" ] [ HH.text "＋" ]
      , HH.div [ HP.style "font-size: 10px; letter-spacing: 0.1em; text-transform: uppercase;" ] [ HH.text "add player" ] ]

-- | The sequence-editor modal — a roomier surface for the text hatch than the
-- | inline field, with a mini-notation guide and clickable examples in place. Edits
-- | box `perfEditBox`'s `seqText` directly (same `PerfSetSeq` path, committed on
-- | blur). Examples drop straight into the field; the guide makes the notation
-- | learnable where you use it (the complexity-budget point).
-- | The phrase picker (#27/#28-lib): the SHARED clip-library surface
-- | (`Clips.View.libraryPanel`, also the shell's ⌥6 modal) in its attach mode —
-- | same rows, plus an ＋ column that lands a self-contained COPY of the clip on
-- | box `perfPhrasePick` as its source. Only the wiring lives here now; the rows
-- | themselves are the one renderer both entry points share.
-- |
-- | The controls don't need `PerfStopClick`: the panel div below already stops the
-- | click before it reaches the backdrop, so a button inside can't close the modal.
perfPhrasePickModal :: forall m. State -> H.ComponentHTML Action Slots m
perfPhrasePickModal st = case st.perfPhrasePick of
  Nothing -> HH.text ""
  Just i ->
    HH.div
      [ HP.style "position: fixed; inset: 0; background: rgba(20,20,20,0.32); z-index: 60; display: flex; align-items: center; justify-content: center; padding: 40px;"
      , HE.onClick \_ -> PerfClosePhrasePick ]
      [ HH.div
          [ HP.style "background: #fbf9f2; border: 1px solid #cdbb8c; border-radius: 10px; padding: 18px 20px; max-width: 460px; width: 100%; max-height: 70vh; overflow-y: auto; box-shadow: 0 10px 40px rgba(0,0,0,0.25);"
          , HE.onClick \e -> PerfStopClick e PerfNop ]
          [ ClipsView.libraryPanel
              { audition: ClipAudition
              , rename: ClipRename
              , delete: ClipDelete
              , attach: Just
                  { label: "P" <> show (maybe (i + 1) _.channel (index st.perfBoxes i))
                  , onAttach: PerfAttachPhrase i }
              , share: Just { onShare: ClipShare, msg: fromMaybe "" st.publishMsg }
              }
              st.clipLibrary
          , HH.button
              [ HP.style "margin-top: 12px; border: 1px solid #dcd2b4; background: #faf6ea; color: #8a7a4a; cursor: pointer; padding: 4px 12px; border-radius: 4px; font-size: 11px;"
              , HE.onClick \_ -> PerfClosePhrasePick ]
              [ HH.text "cancel" ]
          ]
      ]

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
          , HH.textarea
              [ HP.style ("width: 100%; box-sizing: border-box; resize: vertical; field-sizing: content; border: 1px solid "
                           <> (if boxUsesSeq box then "#b8860b" else "#cdbb8c")
                           <> "; background: #fff; color: #3a3a3a; border-radius: 6px; padding: 9px 12px; font-size: 15px; line-height: 1.6; font-family: ui-monospace, monospace;")
              , HP.rows 2
              , HP.value (printPipelineLines box)
              , HP.attr (AttrName "placeholder") "0 1 2 3\n# arp up 4"
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
    , Tuple "# transpose \"0 7 <5 3>\"" "any arg can be a pattern — sampled per chord/cycle"
    , Tuple "# oct -1" "shift ±octaves"
    , Tuple "# slow 4" "unfold over N bars — the harmonic-progression verb"
    , Tuple "# fast 2" "pack into 1/N of a bar"
    , Tuple "# voice open" "re-voice: open/rootless/drop2/drop24/quartal/cluster"
    , Tuple "# top 1 · # bottom 1" "keep the top / bottom N voices"
    , Tuple "# arp up 4" "arpeggiate: up/down/updown, steps per bar (spreads under slow)"
    , Tuple "# arp \"0 1 2 3\"" "arp an index figure: 0 = lowest, wraps up an 8ve; rests/<alt>/euclid ok"
    , Tuple "# strum 14" "strum — ms between notes"
    , Tuple "… every 4" "gate: apply the layer only every N cycles"
    , Tuple "… prob 0.3" "gate: apply it on ~30% of cycles (random)"
    , Tuple "… afterbar 16" "gate: apply it only from bar N onward (a build-up)"
    , Tuple "… whenmod 8 1" "gate: apply when (cycle mod n) ≥ r — e.g. every cycle but every 8th"
    ]

-- | The FX palette: click a layer to pick it up, then click a player box to append
-- | it to that box's stack (drag comes in a later slice). The held chip lights gold.
fxPalette :: forall m. State -> H.ComponentHTML Action Slots m
fxPalette st =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 8px;" ]
    ( [ HH.span [ HP.style "font-size: 10px; letter-spacing: 0.12em; text-transform: uppercase; color: #b0a684;" ] [ HH.text "fx" ] ]
        <> map paletteChip [ Transpose (Lit "0"), Octave (Lit "-1"), Slow 4, Fast 2, Voice (Lit "open"), Select (High (Lit "1")), Select (Low (Lit "1")), Arpg ArpUp 4, Strum 14 ]
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
                 [ HH.text ("v" <> show box.cardId <> " · ch " <> show box.channel) ]
             , HH.button
                 [ HP.style "border: 1px solid #dcd2b4; background: #faf6ea; color: #8a7a4a; cursor: pointer; padding: 1px 6px; border-radius: 3px; font-size: 10px;"
                 , HP.title ("show this card in Limulus as a line (v" <> show box.cardId <> " $ …); edits there come back here")
                 , HE.onClick \e -> PerfStopClick e (CardToLimulus box.cardId) ]
                 [ HH.text "λ" ]
             , HH.button
                 [ HP.style ("border: 1px solid " <> (if box.muted then "#c8a24a" else "#dcd2b4")
                              <> "; background: " <> (if box.muted then "#f3e6c4" else "#faf6ea")
                              <> "; color: " <> (if box.muted then "#9a6a1a" else "#8a7a4a")
                              <> "; cursor: pointer; padding: 1px 8px; border-radius: 3px; font-size: 9px; letter-spacing: 0.06em; text-transform: uppercase;")
                 , HP.title (if box.muted then "muted — click to play" else "playing — click to mute")
                 , HE.onClick \e -> PerfStopClick e (PerfToggleMute i) ]
                 [ HH.text (if box.muted then "muted" else "on") ]
             , if isJust box.phrase then HH.text "" else
                 HH.button
                   [ HP.style "border: 1px solid #d0c4e0; background: #f6f1fb; color: #6a4a8a; cursor: pointer; padding: 1px 8px; border-radius: 3px; font-size: 9px; letter-spacing: 0.06em; text-transform: uppercase;"
                   , HP.title "attach a captured phrase from the clip library"
                   , HE.onClick \e -> PerfStopClick e (PerfOpenPhrasePick i) ]
                   [ HH.text "♪ phrase" ]
             ]
         -- the SOURCE row: a captured phrase (distinct, violet) takes precedence over
         -- the chord token's glyph; ＋ when the box is empty.
         , case box.phrase of
             Just ph ->
               HH.div [ HP.style "display: flex; align-items: center; gap: 6px; font-size: 12px; color: #5a3a7a; margin: 2px 0; max-width: 100%;" ]
                 [ HH.span [ HP.style "font-size: 15px;" ] [ HH.text "♪" ]
                 , HH.span [ HP.style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 120px;" ] [ HH.text ph.clip.name ]
                 , HH.span [ HP.style "font-size: 9px; color: #a89ac0;" ] [ HH.text (show (length ph.clip.events) <> "n") ]
                 , HH.button
                     [ HP.style "border: none; background: transparent; color: #b06a5a; font-size: 13px; line-height: 1; cursor: pointer; padding: 0 2px;"
                     , HP.title "detach the phrase (back to chords)"
                     , HE.onClick \e -> PerfStopClick e (PerfDetachPhrase i) ]
                     [ HH.text "×" ] ]
             Nothing -> case box.seq of
               Just s ->
                 HH.div [ HP.style "display: flex; align-items: center; gap: 6px; font-size: 22px; color: #7a5c00; margin: 2px 0;" ]
                   (faIcons s.glyph)
               Nothing ->
                 HH.div [ HP.style "font-size: 28px; color: #d8ceb4; line-height: 1; margin: 2px 0;" ] [ HH.text "＋" ]
         ]
         -- phrase controls: flatten/original channel mode + per-source-head mute chips.
         <> (case box.phrase of
               Just ph -> [ phraseControlsRow ph ]
               Nothing -> [])
         -- the seq/text-hatch row is inert for a phrase box (its source is the phrase,
         -- not chord indices), so hide it; the stack chips below still apply.
         <> (if isJust box.phrase then [] else [ seqRow ])
         <> stackRows
         -- the terminal SINK — a midi · odo · rig pill row (the fold's cap)
         <> [ HH.div [ HP.style "display: inline-flex; border: 1px solid #dcd2b4; border-radius: 3px; overflow: hidden; margin-top: 2px;" ]
                -- a phrase can only SOUND (→ midi/rig); → odo (harmonic conditioning) is
                -- meaningless for a frozen foreground gesture, so it's dropped here.
                (map termBtn (if isJust box.phrase then [ TMidi, TRig ] else [ TMidi, TOdo, TRig ]))
            , if ghost
                then HH.div [ HP.style "font-size: 9px; letter-spacing: 0.06em; text-transform: uppercase; color: #a05a3a;" ]
                       [ HH.text "✕ rig only" ]
                else HH.text ""
            -- corner ×: on a FILLED card it clears the token (keep the player); on an
            -- EMPTY card it removes the player outright. So clearing twice, or × on a
            -- bare card, deletes it — no separate control, no accidental one-click loss.
            , HH.button
                [ HP.style "position: absolute; top: 4px; right: 7px; border: none; background: transparent; color: #b06a5a; font-size: 15px; line-height: 1; cursor: pointer;"
                , HP.title (if filled then "clear this player's token" else "remove this player")
                , HE.onClick \e -> PerfStopClick e (if filled then PerfClearBox i else PerfRemoveBox i) ]
                [ HH.text "×" ] ]
       )
  where
  ghost = boxGhosted st.authority box
  -- phrase (#27b): flatten vs original-channels toggle + per-source-head mute chips.
  -- `flatten` collapses to this voice's channel (full stack); `orig ch` plays each head
  -- on its own channel (faithful, no stack). Head chips silence a head from the recording.
  phraseControlsRow ph =
    let modeBtn m label =
          HH.button
            [ HP.style ("border: 1px solid " <> (if ph.channelMode == m then "#8a6ac0" else "#d0c4e0")
                         <> "; background: " <> (if ph.channelMode == m then "#e8def7" else "#faf7ff")
                         <> "; color: #5a3a7a; cursor: pointer; padding: 0 6px; border-radius: 3px; font-size: 9px;")
            , HP.title "flatten to this voice's channel (with the stack), or keep the clip's original per-head channels"
            , HE.onClick \e -> PerfStopClick e (PerfPhraseMode i m) ]
            [ HH.text label ]
        headChip h =
          let muted = elem h ph.mutedHeads
          in HH.button
               [ HP.style ("border: 1px solid #d0c4e0; background: " <> (if muted then "#eeeaf2" else "#faf7ff")
                            <> "; color: " <> (if muted then "#bbb2c8" else "#5a3a7a")
                            <> (if muted then "; text-decoration: line-through" else "")
                            <> "; cursor: pointer; padding: 0 5px; border-radius: 3px; font-size: 9px;")
               , HP.title ("source head " <> show (h + 1) <> " — click to mute/unmute in the recording")
               , HE.onClick \e -> PerfStopClick e (PerfPhraseMuteHead i h) ]
               [ HH.text (show (h + 1)) ]
    in HH.div
         [ HP.style "display: flex; align-items: center; gap: 4px; flex-wrap: wrap; width: 100%;" ]
         ( [ modeBtn Flatten "flat", modeBtn Original "orig ch" ]
           <> (if length (phraseClipHeads ph) > 1
                 then [ HH.span [ HP.style "font-size: 8px; color: #a89ac0; margin: 0 1px;" ] [ HH.text "heads" ] ]
                      <> map headChip (phraseClipHeads ph)
                 else []) )
  -- the TEXT HATCH: a mini-notation sequence over the token's chord indices (cycle
  -- = one bar). Empty = default one-chord-per-beat. Border lights when it's driving.
  seqRow =
    HH.div
      [ HP.style "display: flex; align-items: flex-start; width: 100%; gap: 3px;" ]
      [ HH.textarea
          -- `field-sizing: content` (Chrome 123+, and the rig is Chrome) grows the
          -- textarea to fit its lines — one line for a bare sequence, more as layers
          -- stack — so the WHOLE pipeline is visible without a fixed height or a
          -- horizontal run-off. `rows 1` is the floor. `resize: none` keeps the card
          -- tidy; long single layers soft-wrap rather than widening the card.
          [ HP.style ("flex: 1 1 auto; min-width: 0; box-sizing: border-box; resize: none; field-sizing: content; overflow: hidden; border: 1px solid "
                       <> (if boxUsesSeq box then "#b8860b" else "#dcd2b4")
                       <> "; background: #fbfaf4; color: #6a5a2a; border-radius: 4px; padding: 2px 6px; font-size: 11px; line-height: 1.5; font-family: ui-monospace, monospace;")
          , HP.rows 1
          -- DERIVED, line-broken on `#`: the head sequence then each layer on its own
          -- line (the round-trip's display projection). Chip edits re-render it; text
          -- edits parse back through `parsePipeline` (which trims each `#` segment, so
          -- the newlines are harmless) — the single reconciliation point is unchanged.
          , HP.value (printPipelineLines box)
          , HP.attr (AttrName "placeholder") "0 1 2 3\n# arp up 4"
          , HP.title "the box pipeline as text (two views of one thing — edit here or the chips below): a mini-notation sequence (cycle = 1 bar) then # layers, one per line, e.g. 0 1 2 3 / # voice open / # arp up 4"
          -- commit on CHANGE (blur), not on every keystroke: binding the live value
          -- back via `HP.value` each input would fight the caret. `onValueChange`
          -- leaves the field uncontrolled while you type (Enter adds a line), then
          -- commits on blur — edit the whole multi-line pipeline, hear it when you
          -- click away.
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
  -- `→ odo` is the harmonic-context voice: exclusive, and it conducts Odonus's
  -- quantiser rather than emitting MIDI of its own.
  termTip t = case t of
    TOdo -> "→ odo · route odonus.out to this card (the router's Vetula voice row): Odonus snaps to its chords; one card at a time"
    TMidi -> "→ midi · emit this box on its own MIDI channel"
    TRig -> "→ rig · hand this box to the rig"

  termBtn t =
    let active = box.term == t
        rigCol = t == TRig && ghost
    in HH.button
         [ HP.style ("border: none; cursor: pointer; padding: 1px 8px; font-size: 9px; letter-spacing: 0.04em; text-transform: uppercase; background: "
                      <> (if active then "#8a7a4a" else "#faf6ea")
                      <> "; color: " <> (if active then "#ffffff" else if rigCol then "#a05a3a" else "#8a7a4a") <> ";")
         , HP.title (termTip t)
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
    , HE.onClick \_ -> AuditionNode chord
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
        -- the diatonic triad rooted here — click auditions the root's triad (it
        -- lands in the chyron). Every root gets a transparent hit disc, so
        -- out-of-scale roots (label-only, no parchment disc) click too.
        triadPcs = triadOn key pc
        hit =
          [ SE.circle
              [ SA.cx x, SA.cy y, SA.r 14.0
              , HP.style "fill: transparent; cursor: pointer;"
              , HE.onMouseEnter \_ -> HoverTriad (Just { root: pc, pcs: triadPcs })
              , HE.onMouseLeave \_ -> HoverTriad Nothing
              , HE.onClick \_ -> AuditionTriad pc triadPcs
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
    , HE.onClick \_ -> AuditionTriad t.root t.pcs
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
          , HE.onClick \_ -> AuditionNode m.chord
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

-- | A chyron audition lifted into a SEED specimen for Explore. Carries the event's
-- | real harmonic reading (`anchor`, enriched in step 1) so `specToNode` blooms the
-- | RIGHT neighbourhood per chord — a selection can span scales, so the reading must
-- | travel with each seed, never the buffer (DESIGN-tank-overhaul.md §§1, 4). `notes`
-- | is `bass : voicing`, so the foot is the low note and the rest is the voicing.
specFromEvent :: Int -> ChyronEvent -> Specimen
specFromEvent i ev =
  let sorted = sort ev.notes
  in { id: SpecimenId i
     , voicing: drop 1 sorted
     , bass: fromMaybe 0 (head sorted)
     , label: ev.label
     , provenance: Imported     -- lifted from the audition trace
     , anchor: ev.anchor
     }

-- | The seeds Explore blooms around: the chyron SELECTION when there is one (each
-- | selected chord seeds its own neighbourhood), else the tail of the tape as a
-- | convenience so Explore is never blank. Capped at 6 rings to keep the layout
-- | sane (mirrors the old `take 6` over the tank).
chyronSeeds :: State -> Array Specimen
chyronSeeds st =
  let evs = case st.chyronSel of
        Just sel -> mapMaybe (\ix -> index st.chyron ix) (range sel.lo sel.hi)
        Nothing  -> takeEnd 6 st.chyron
  in mapWithIndex specFromEvent (take 6 evs)

-- | The Generate lens — each SEED (now a chyron-selection chord, formerly a tank
-- | specimen) with a ring of voice-led relatives bloomed around it (reusing
-- | `generateCandidates`, the same engine the Lab pick-mode uses). "shake" re-rolls:
-- | a different adventure + a rotated crop of the ranked relatives. Hover a relative
-- | to preview, click to audition — which appends it to the chyron, growing the
-- | buffer (DESIGN-tank-overhaul.md §4).
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
      ( let seeds = chyronSeeds st
        in if length seeds == 0
          then
            [ SE.text
                [ SA.x 0.0, SA.y 0.0, HP.attr (AttrName "text-anchor") "middle"
                , HP.style "font-size: 15px; fill: #b8b8b8; -webkit-user-select: none; user-select: none;"
                ]
                [ HH.text "audition chords — they land in the chyron; select some, then grow relatives here — shake ⟳" ]
            ]
          else concat (mapWithIndex (genCluster st) seeds)
      )

-- | One seed's constellation: the seed glyph at the centre, its relatives ringed
-- | around it with faint spokes. `genRoll` varies both the adventure dial and which
-- | slice of the ranked relatives shows, so each shake crops a fresh set.
genCluster :: forall m. State -> Int -> Specimen -> Array (H.ComponentHTML Action Slots m)
genCluster st i spec =
  let key = st.key
      center = genCenter i
      -- name the seed from its own pitches (quality-aware: minor gets its "m"),
      -- not the captured audition label — some lens paths label a minor triad with
      -- just its bare root, which reads fine for C major but wrong for F minor. The
      -- relatives are already named by the generator.
      seedBase = specToNode (9000 + i) key spec
      seedN = seedBase { label = chordTag seedBase }
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

-- ---------------------------------------------------------------------------
-- The BANKS lens — the chord space as nine banks of sixteen
-- ---------------------------------------------------------------------------

-- | **The Banks lens** — `Vetula.Pads` rendered as a labelled 3×3 of 4×4 pads.
-- |
-- | The other four Hunt lenses are geometric: a chord's position is COMPUTED
-- | from its pitch content and redrawn as the selection moves. This one is a
-- | table, and deliberately so — a chord's position is its address in the
-- | generator's two axes, fixed for the session. Rows are how far from home the
-- | walk may roam, columns are how richly it may colour, and the axes are
-- | labelled once at the edges rather than nine times in the cells.
-- |
-- | HTML rather than SVG: this is a grid of buttons, so CSS grid does the
-- | layout that the geometric lenses need a viewBox and trigonometry for. The
-- | only SVG is the chromatic-circle glyph inside each pad, which keeps the
-- | pads speaking Vetula's visual language rather than becoming a chord chart.
padsSurface :: forall m. State -> H.ComponentHTML Action Slots m
padsSurface st =
  let cells = Pads.grid st.key st.padRoll
      bankAt r c = filter (\x -> x.reach == r && x.colour == c) cells
  in HH.div
      -- `vetula-surface` is load-bearing, not cosmetic: `surfaceHidden` finds the
      -- surface by this class and reports "hidden" when the selector matches
      -- nothing, which stands the WHOLE keyboard down. Every other lens gets it
      -- free by being an `SE.svg`; an HTML surface has to say it.
      [ HP.class_ (cn "vetula-surface vetula-surface--wide")
      , HP.style "position: absolute; inset: 0; overflow: auto; padding: 16px 22px 26px;" ]
      [ HH.div
          [ HP.style "font-size: 11px; color: #a09880; letter-spacing: 0.04em; margin-bottom: 10px; -webkit-user-select: none; user-select: none;" ]
          [ HH.text ("144 chords of a 480-chord vocabulary · rows roam further from "
                      <> noteName (mod st.key.tonic 12)
                      <> ", columns colour more richly · each bank is one seeded walk, so any row of four is already a progression") ]
      , HH.div
          [ HP.style "display: grid; grid-template-columns: 62px repeat(3, minmax(0, 1fr)); gap: 10px 12px; align-items: start;" ]
          ( [ HH.div [] [] ]
              <> map padColHead Pads.colours
              <> concatMap
                   (\r -> [ padRowHead r ] <> map (\c -> padBank st (bankAt r c)) Pads.colours)
                   Pads.reaches
          )
      ]

-- | A column header — the complexity axis, named once.
padColHead :: forall m. Pads.Colour -> H.ComponentHTML Action Slots m
padColHead c =
  HH.div
    [ HP.style "font-size: 11px; color: #7a7360; letter-spacing: 0.08em; text-transform: uppercase; padding-bottom: 2px; border-bottom: 1px solid #e6dfcc; -webkit-user-select: none; user-select: none;" ]
    [ HH.text (Pads.colourLabel c) ]

-- | A row header — the freedom axis, named once. Rotated would be prettier and
-- | less readable; three short words do not need the space.
padRowHead :: forall m. Pads.Reach -> H.ComponentHTML Action Slots m
padRowHead r =
  HH.div
    [ HP.style "font-size: 11px; color: #7a7360; letter-spacing: 0.08em; text-transform: uppercase; padding-top: 14px; text-align: right; -webkit-user-select: none; user-select: none;" ]
    [ HH.text (Pads.reachLabel r) ]

-- | One bank: sixteen pads, four across. Takes an array because the lookup that
-- | finds it is a filter — an empty one renders as nothing rather than throwing.
padBank :: forall m. State -> Array Pads.Cell -> H.ComponentHTML Action Slots m
padBank st cs = case head cs of
  Nothing -> HH.div [] []
  Just cell ->
    HH.div
      [ HP.style "display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 4px; background: #fbf8f0; border: 1px solid #ece5d2; border-radius: 5px; padding: 6px;" ]
      (map (padButton st) cell.chords)

-- | One pad. Click auditions it, which is also what puts it in the chyron — so
-- | this lens feeds the same buffer as every other, and everything downstream
-- | (Continuo, the Odonus quantiser, a Quadrat sample set) is already wired.
padButton :: forall m. State -> ChordNode -> H.ComponentHTML Action Slots m
padButton st c =
  HH.button
    [ HP.style ("display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 1px; "
                 <> "border: 1px solid " <> (if padLit st c then "#cdbb8c" else "#eee7d6") <> "; "
                 <> "background: " <> (if padLit st c then "#fdf6e4" else "#ffffff") <> "; "
                 <> "border-radius: 4px; padding: 5px 2px 4px; cursor: pointer; min-width: 0;")
    , HP.title (c.label <> " — " <> show (playNotes c))
    , HE.onMouseEnter \_ -> HoverPad (Just c)
    , HE.onMouseLeave \_ -> HoverPad Nothing
    , HE.onClick \_ -> AuditionNode c
    ]
    [ SE.svg
        [ SA.viewBox (-15.0) (-15.0) 30.0 30.0, SA.width 30.0, SA.height 30.0 ]
        (pcPolygon (hiFor st.hoveredTriad c.pcs) c.root c.pcs 0.0 0.0 12.0)
    , HH.div
        [ HP.style "font-size: 10px; color: #6a6250; line-height: 1.1; text-align: center; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%; -webkit-user-select: none; user-select: none;" ]
        [ HH.text c.label ]
    ]

-- | A pad lights when its pitch-class set matches whatever is hovered anywhere
-- | in the app — so hovering one pad shows you every other bank holding the
-- | same chord, which is how the nesting between cells becomes visible.
padLit :: State -> ChordNode -> Boolean
padLit st c = case st.hoveredTriad of
  Nothing -> false
  Just h -> sort (nub (map (\p -> mod p 12) h.pcs)) == sort (nub (map (\p -> mod p 12) c.pcs))

-- | **The REHEARSE surface — a progression with alternatives at every chord.**
-- |
-- | A column per slot, its options stacked beneath it. The pass the transport
-- | would play is highlighted; a settled slot wears a pin and stops moving.
-- |
-- | The whole stage exists because a progression stops being a list the moment
-- | its chords have alternatives: four options at each of three chords is
-- | sixty-four progressions, and what you want is not to pick one but to keep
-- | running them until one of them is obviously right.
rehearseSurface :: forall m. State -> H.ComponentHTML Action Slots m
rehearseSurface st =
  HH.div
    [ HP.class_ (cn "vetula-surface vetula-surface--wide")
    , HP.style "position: absolute; inset: 0; overflow: auto; padding: 14px 22px 26px;" ]
    (if length st.rehearsal == 0 then [ nothingTakenUp st ] else rehearsalBody st)

-- | Nothing in hand. The rebus glyphs on the AUDITION bar are the shelf — this
-- | stage had its own row of them for a day, which was the same set of
-- | progressions drawn twice in one window.
nothingTakenUp :: forall m. State -> H.ComponentHTML Action Slots m
nothingTakenUp st =
  HH.div
    [ HP.style "font-size: 12px; color: #a09880; line-height: 1.6; max-width: 480px; padding-top: 20px;" ]
    [ HH.text (if length st.chyronSaved == 0
                 then "Nothing saved yet — hunt a progression and ⏎ it onto the shelf, then click its rebus above."
                 else "Click a progression's rebus on the audition bar above and every chord in it becomes a slot you can give alternatives to.") ]

-- | The controls, the readout, the slots, and — when a slot is being varied —
-- | the grid underneath. The controls live HERE rather than in the context bar:
-- | they are about the progression in front of you, not about the app's mode,
-- | and the bar is for things true of the whole instrument.
rehearsalBody :: forall m. State -> Array (H.ComponentHTML Action Slots m)
rehearsalBody st =
  let ixs = passIxs st
  in
    [ HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; flex-wrap: wrap; margin-bottom: 12px;" ]
        ( [ paneBtn true "play ▶" "hear this pass" PlayPass
          , paneBtn false "pass ⟳" "draw a new pass through the lattice" RollPass
          , paneDivider
          ]
            <> map (pullBtn st) HT.pulls
            <> [ paneDivider
               , paneBtn false "settle" "lock every slot to what this pass chose — the alternatives stay" SettlePass
               , paneBtn false "loosen" "unlock every slot" LoosenAll
               , paneDivider
               , paneBtn (st.pane == Just PanePaths) "paths"
                   "lay out every way through these chords, smoothest first" ShowPaths
               , paneDivider
               , paneBtn false "keep ⏎"
                   "put this pass on the shelf as a progression — drag it to a Perform box from there" KeepPass
               , paneBtn false "keep all ⇶"
                   "put the WHOLE lattice on the shelf: each slot alternates, so the rig walks the space per cycle"
                   KeepLattice
               , HH.div [ HP.style "flex: 1 1 auto;" ] []
               , HH.span
                   [ HP.style "font-size: 11px; color: #b3aa92; -webkit-user-select: none; user-select: none;" ]
                   [ HH.text (show (length st.rehearsal) <> " chords · " <> show (RH.size st.rehearsal)
                               <> " progressions · pass " <> show st.rehearseRoll
                               <> " · motion " <> show (passMotion st)) ]
               , HH.button
                   [ HP.style "border: none; background: none; color: #b3aa92; font-size: 11px; cursor: pointer; padding: 0;"
                   , HP.title "put this progression down — the kept variations survive it"
                   , HE.onClick \_ -> DropRehearsal ]
                   [ HH.text "put down" ]
               ]
        )
    , HH.div
        [ HP.style "display: flex; gap: 10px; align-items: flex-start; flex-wrap: wrap;" ]
        (mapWithIndex (slotColumn ixs) st.rehearsal)
    ]
      <> panelBelow st

-- | Whatever is open under the progression. One at a time, and each a different
-- | question about the same chords.
panelBelow :: forall m. State -> Array (H.ComponentHTML Action Slots m)
panelBelow st = case st.pane of
  Nothing -> []
  Just PanePaths -> pathsPanel st
  Just (PaneVary _) -> varyPanel st

-- | **Every way through the chords, smoothest first.**
-- |
-- | The pull dial plays one path and re-rolls; this lays them all out. Different
-- | questions — the dial is for finding what you did not expect, this is for
-- | choosing once you roughly know — and the ordering is what makes it usable:
-- | the top moves least, the bottom leaps, and the middle is a trade you can
-- | hear. Click one to play it.
pathsPanel :: forall m. State -> Array (H.ComponentHTML Action Slots m)
pathsPanel st =
  [ HH.div
      [ HP.style "margin-top: 22px; border-top: 1px solid #e6dfcc; padding-top: 14px;" ]
      ( [ HH.div
            [ HP.style "display: flex; align-items: baseline; gap: 10px; margin-bottom: 10px; font-size: 11px; color: #a09880; letter-spacing: 0.04em; -webkit-user-select: none; user-select: none;" ]
            [ HH.span [ HP.style "color: #6a6250; font-weight: 500;" ] [ HH.text "paths" ]
            , HH.text ("every way through these chords, least motion first · click to hear one · ★ marks one to keep"
                        <> (if length st.marked == 0 then "" else " · " <> show (length st.marked) <> " marked"))
            , HH.div [ HP.style "flex: 1 1 auto;" ] []
            , if length (markedRows st) == 0 then HH.text ""
              else HH.button
                     [ HP.style "border: 1px solid #b8975a; background: #f2e7c6; color: #5a564b; font-size: 11px; padding: 2px 10px; border-radius: 3px; cursor: pointer;"
                     , HP.title "put the marked readings on the shelf as ONE token — a cycle picks between them"
                     , HE.onClick \_ -> KeepMarked ]
                     [ HH.text ("keep " <> show (length (markedRows st)) <> " marked ⏎") ]
            , HH.button
                [ HP.style "border: none; background: none; color: #b3aa92; font-size: 11px; cursor: pointer; padding: 0;"
                , HE.onClick \_ -> ShowPaths ]
                [ HH.text "close" ]
            ]
        ]
          <> body
      )
  ]
  where
  body = case RH.allPaths pathLimit pathCap st.rehearsal of
    -- Not an error to swallow but a thing to say, and an actionable one: the
    -- list collapses the moment you settle a slot, which makes this view the
    -- reason to settle rather than a casualty of not having. What you have
    -- already approved still shows — marks are most useful exactly when the
    -- space is too big to read.
    Nothing ->
      [ HH.div
          [ HP.style "font-size: 12px; color: #a09880; line-height: 1.6; max-width: 480px; margin-bottom: 10px;" ]
          [ HH.text (show (RH.size st.rehearsal)
                      <> " paths — too many to lay out. Settle a slot or two and they collapse."
                      <> (if length st.marked == 0 then "" else " What you have marked is below.")) ]
      ]
        <> rows (markedRows st)
    Just ps -> rows ps

  -- Approved paths float to the top — your shortlist is what you want to
  -- compare — and the rest stay in order of least motion.
  rows ps =
    [ HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 3px; max-width: 900px;" ]
        (map pathRow (sortBy (comparing (\r -> Tuple (if pathMarked st r.ixs then 0 else 1) r.motion)) ps))
    ]

  live = passIxs st

  pathRow r =
    let now = r.ixs == live
        mk = pathMarked st r.ixs
    in HH.div
        [ HP.style ("display: flex; align-items: baseline; gap: 8px; "
                     <> "border: 1px solid " <> (if mk then "#b8975a" else if now then "#cdbb8c" else "#ece5d2")
                     <> "; background: " <> (if mk then "#f2e7c6" else if now then "#fdf6e4" else "#fdfbf5")
                     <> "; border-radius: 3px; padding: 4px 6px 4px 10px; font-size: 11px; color: #6a6250;") ]
        [ HH.button
            [ HP.style "flex: 1 1 auto; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; text-align: left; border: none; background: none; color: inherit; font-size: 11px; cursor: pointer; padding: 0;"
            , HP.title (if now then "the pass you are hearing" else "hear this one")
            , HE.onClick \_ -> HearPath r.ixs ]
            [ HH.text (joinWith "  ·  " (namesFor r.ixs)) ]
        , HH.span [ HP.style "color: #b3aa92; flex: 0 0 auto;" ] [ HH.text (show r.motion) ]
        , HH.button
            [ HP.style ("border: none; background: none; cursor: pointer; font-size: 12px; line-height: 1; padding: 0 2px; flex: 0 0 auto; color: "
                         <> (if mk then "#b8860b" else "#d8cfb8"))
            , HP.title (if mk then "marked — click to drop it" else "mark this one to keep")
            , HE.onClick \_ -> MarkPath r.ixs ]
            [ HH.text (if mk then "★" else "☆") ]
        ]

  namesFor ixs = catMaybes (zipWith (\sl j -> map _.label (index sl.options j)) st.rehearsal ixs)

-- | Beyond this the list stops being something a person chooses from — five
-- | slots of four is 1024 readings.
pathLimit :: Int
pathLimit = 240

-- | And of the ones we do lay out, the smoothest handful is where the answer is.
pathCap :: Int
pathCap = 40

-- | The Vary grid, opened UNDER the progression rather than in place of it.
varyPanel :: forall m. State -> Array (H.ComponentHTML Action Slots m)
varyPanel st = case varyingSlot st of
  Nothing -> []
  Just i -> case index st.rehearsal i >>= \sl -> head sl.options of
    Nothing -> []
    Just src ->
      [ HH.div
          [ HP.style "margin-top: 22px; border-top: 1px solid #e6dfcc; padding-top: 14px;" ]
          ( [ HH.div
                [ HP.style "display: flex; align-items: baseline; gap: 10px; margin-bottom: 10px; font-size: 11px; color: #a09880; letter-spacing: 0.04em; -webkit-user-select: none; user-select: none;" ]
                [ HH.span [ HP.style "color: #6a6250; font-weight: 500;" ]
                    [ HH.text ("slot " <> show (i + 1) <> " · " <> src.label) ]
                , HH.text "rows let the notes drift, columns spread and double them · click hears it alone · ⌥-click in place · shift-click keeps"
                , HH.div [ HP.style "flex: 1 1 auto;" ] []
                , HH.button
                    [ HP.style "border: none; background: none; color: #b3aa92; font-size: 11px; cursor: pointer; padding: 0;"
                    , HP.title "close the grid"
                    , HE.onClick \_ -> VaryFromSlot i ]
                    [ HH.text "close" ]
                ]
            ]
              <> varyGridFor st src
          )
      ]

paneBtn :: forall m. Boolean -> String -> String -> Action -> H.ComponentHTML Action Slots m
paneBtn strong label tip act =
  HH.button
    [ HP.title tip
    , HE.onClick \_ -> act
    , HP.style ("border: 1px solid " <> (if strong then "#b8975a" else "#ddd5c0")
                 <> "; background: " <> (if strong then "#f2e7c6" else "#fdfbf5")
                 <> "; color: #5a564b; font-size: 12px; padding: 4px 12px; border-radius: 3px; cursor: pointer;") ]
    [ HH.text label ]

paneDivider :: forall m. H.ComponentHTML Action Slots m
paneDivider = HH.span [ HP.style "width: 1px; height: 18px; background: #e6dfcc;" ] []

pullBtn :: forall m. State -> HT.Pull -> H.ComponentHTML Action Slots m
pullBtn st p =
  HH.button
    [ HP.title (HT.pullBlurb p)
    , HE.onClick \_ -> SetPull p
    , HP.style ("border: 1px solid " <> (if st.pull == p then "#b8975a" else "#ddd5c0")
                 <> "; background: " <> (if st.pull == p then "linear-gradient(#c8a86a,#b8975a)" else "#fdfbf5")
                 <> "; color: " <> (if st.pull == p then "#1c1a12" else "#5a564b")
                 <> "; font-size: 12px; padding: 4px 12px; border-radius: 3px; cursor: pointer;") ]
    [ HH.text (HT.pullLabel p) ]

-- | One slot: the chord's name, an excursion button, and its options beneath.
slotColumn :: forall m. Array Int -> Int -> Slot -> H.ComponentHTML Action Slots m
slotColumn ixs i sl =
  let playing = fromMaybe 0 (index ixs i)
  in HH.div
      [ HP.style "flex: 0 0 auto; min-width: 116px; background: #fbf8f0; border: 1px solid #ece5d2; border-radius: 5px; padding: 8px 8px 6px;" ]
      -- No slot number: the position is implicit in the row, and a numeral beside
      -- a chord name reads as part of the name.
      ( [ HH.div
            [ HP.style "display: flex; align-items: baseline; justify-content: flex-end; gap: 6px; margin-bottom: 6px;" ]
            [ HH.button
                [ HP.style "border: none; background: none; color: #a09880; font-size: 11px; cursor: pointer; padding: 0;"
                , HP.title "open this chord's variations below"
                , HE.onClick \_ -> VaryFromSlot i ]
                [ HH.text "vary ⋯" ]
            ]
        ]
          <> mapWithIndex (optionRow i sl playing) sl.options
      )

-- | One option. Click hears it; the ✓ settles the slot on it; the × drops it
-- | (never option zero, which is the chord the progression actually said).
optionRow :: forall m. Int -> Slot -> Int -> Int -> ChordNode -> H.ComponentHTML Action Slots m
optionRow i sl playing j c =
  let
    settled = sl.locked == Just j
    live = j == playing
    bg = if settled then "#f2e7c6" else if live then "#fdf6e4" else "#ffffff"
    brd = if settled then "#b8975a" else if live then "#cdbb8c" else "#eee7d6"
  in
    HH.div
      [ HP.style ("display: flex; align-items: center; gap: 4px; border: 1px solid " <> brd
                   <> "; background: " <> bg <> "; border-radius: 3px; padding: 3px 4px 3px 6px; margin-bottom: 3px;") ]
      [ HH.button
          [ HP.style "flex: 1 1 auto; min-width: 0; border: none; background: none; text-align: left; font-size: 11px; color: #6a6250; cursor: pointer; padding: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;"
          , HP.title (show (playNotes c) <> (if j == 0 then " — as written" else "")
                       <> " · ⌥-click hears it in place")
          , HE.onClick \e -> if ME.altKey e then HearAround i c else HearOption i j ]
          [ HH.text c.label ]
      , HH.button
          [ HP.style ("border: none; background: none; font-size: 10px; cursor: pointer; padding: 0 2px; color: "
                       <> (if settled then "#8d7a4a" else "#cbc2aa"))
          , HP.title (if settled then "unsettle this slot" else "settle the slot on this one")
          , HE.onClick \_ -> SettleSlot i j ]
          [ HH.text (if settled then "◉" else "○") ]
      , if j == 0 then HH.text ""
        else HH.button
               [ HP.style "border: none; background: none; color: #cbc2aa; font-size: 11px; cursor: pointer; padding: 0 2px;"
               , HP.title "drop this variation"
               , HE.onClick \_ -> DropOption i j ]
               [ HH.text "×" ]
      ]

-- | **The VARY surface — the Banks widget pointed at one chord.**
-- |
-- | Rows are drift (how far the content may travel, revoicing → substitution),
-- | columns are density (how widely it may be spread and doubled). Pads, banks,
-- | shuffle and hover-lighting are all the Banks lens's, unchanged: this is the
-- | same gesture asking a different question, which is the whole argument for
-- | building it this way rather than as its own screen.
varySurface :: forall m. State -> H.ComponentHTML Action Slots m
varySurface st = case varySource st of
  Nothing ->
    HH.div
      [ HP.class_ (cn "vetula-surface vetula-surface--wide")
      , HP.style "position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; padding: 24px;" ]
      [ HH.div
          [ HP.style "font-size: 12px; color: #a09880; text-align: center; max-width: 380px; line-height: 1.6;" ]
          [ HH.text "Nothing to vary yet. Play a chord in any lens — or open one for revoicing and press "
          , HH.span [ HP.style "color: #6a6250;" ] [ HH.text "vary ⋯" ]
          , HH.text " — and its neighbourhood appears here."
          ]
      ]
  Just src ->
    HH.div
        -- `vetula-surface` is load-bearing, not cosmetic — see `padsSurface`.
        [ HP.class_ (cn "vetula-surface vetula-surface--wide")
        , HP.style "position: absolute; inset: 0; overflow: auto; padding: 16px 22px 26px;" ]
        ( [ HH.div
            [ HP.style "font-size: 11px; color: #a09880; letter-spacing: 0.04em; margin-bottom: 10px; -webkit-user-select: none; user-select: none;" ]
            [ HH.text "varying "
            , HH.span [ HP.style "color: #6a6250; font-weight: 500;" ] [ HH.text src.label ]
            , HH.text (" · " <> show (playNotes src)
                        <> " · rows let the notes themselves drift, columns spread and double them")
            ]
        , case varyingSlot st of
            Nothing -> HH.text ""
            Just i ->
              HH.div
                [ HP.style "margin-bottom: 10px; font-size: 11px;" ]
                [ HH.button
                    [ HP.style "border: 1px solid #b8975a; background: #f2e7c6; color: #6a6250; border-radius: 3px; padding: 3px 10px; cursor: pointer; font-size: 11px;"
                    , HP.title "back to the progression — what you kept is already in the slot"
                    , HE.onClick \_ -> BackToRehearsal ]
                    [ HH.text ("← rehearsal · slot " <> show (i + 1)) ]
                , HH.span
                    [ HP.style "color: #b3aa92; margin-left: 8px;" ]
                    [ HH.text "shift-click keeps into that slot" ]
                ]
        ]
          <> varyGridFor st src
          <> [ keptTray st ]
        )

-- | **The nine cells themselves**, shared by the standalone lens and the panel
-- | that opens under a rehearsal slot. Identical either way on purpose: the two
-- | places are the same tool at different distances from the music, and a grid
-- | that behaved differently inline would be a second thing to learn.
varyGridFor :: forall m. State -> ChordNode -> Array (H.ComponentHTML Action Slots m)
varyGridFor st src =
  let cells = Vary.grid st.key src st.varyRoll
      cellAt d dn = filter (\x -> x.drift == d && x.density == dn) cells
  in
    [ HH.div
        [ HP.style "display: grid; grid-template-columns: 62px repeat(3, minmax(0, 1fr)); gap: 10px 12px; align-items: start;" ]
        ( [ HH.div [] [] ]
            <> map varyColHead HV.densities
            <> concatMap
                 (\d -> [ varyRowHead d ] <> map (\dn -> varyBank st (cellAt d dn)) HV.densities)
                 HV.drifts
        )
    ]

-- | The chord under the lens: the one explicitly sent here, else whatever is
-- | sounding. Falling back means the lens is never blank merely because you
-- | arrived by the dropdown rather than by the button.
varySource :: State -> Maybe ChordNode
varySource st = case st.varying of
  Just c -> Just c
  Nothing -> st.sounding >>= \sid -> find (\c -> c.id == sid) st.chords

-- | One variation pad. Click HEARS it (and nothing else); shift-click KEEPS it.
-- | A kept pad wears a filled ring, so the nine cells double as the record of
-- | what you have chosen out of them.
varyPad :: forall m. State -> ChordNode -> H.ComponentHTML Action Slots m
varyPad st c =
  let mine = isKept st c
  in HH.button
      [ HP.style ("display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 1px; "
                   <> "border: 1px solid " <> (if mine then "#8d7a4a" else if padLit st c then "#cdbb8c" else "#eee7d6") <> "; "
                   <> (if mine then "box-shadow: inset 0 0 0 1px #8d7a4a; " else "")
                   <> "background: " <> (if mine then "#f6efd9" else if padLit st c then "#fdf6e4" else "#ffffff") <> "; "
                   <> "border-radius: 4px; padding: 5px 2px 4px; cursor: pointer; min-width: 0;")
      , HP.title (c.label <> " — " <> show (playNotes c)
                   <> (if mine then " · kept (shift-click to drop)" else " · shift-click to keep")
                   <> " · ⌥-click hears it in place")
      , HE.onMouseEnter \_ -> HoverPad (Just c)
      , HE.onMouseLeave \_ -> HoverPad Nothing
      -- Plain click hears the chord ALONE. In the grid you are judging the
      -- chord itself — whether it is a thing you want at all — and three chords
      -- would answer a question you have not got to yet. ⌥ hears it in place
      -- when you have.
      , HE.onClick \e ->
          if ME.shiftKey e then KeepVariation c
          else case varyingSlot st of
            Just i | ME.altKey e -> HearAround i c
            _ -> VaryAudition c
      ]
      [ SE.svg
          [ SA.viewBox (-15.0) (-15.0) 30.0 30.0, SA.width 30.0, SA.height 30.0 ]
          (registerStrip c)
      , HH.div
          [ HP.style "font-size: 10px; color: #6a6250; line-height: 1.1; text-align: center; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%; -webkit-user-select: none; user-select: none;" ]
          [ HH.text c.label ]
      ]

-- | **A chord as REGISTER, not as content.**
-- |
-- | The Banks pad wears a chromatic-circle polygon, which is right there — that
-- | lens varies which chord you are looking at, so the shape differs pad to pad.
-- | In the Vary grid it is the wrong glyph twice over: down the `held` row every
-- | pad has the SAME pitch classes, so sixteen identical polygons say nothing,
-- | and the thing actually being varied — where the notes sit and how far apart
-- | — is precisely what a polygon throws away.
-- |
-- | So: one dot per sounding note, up a register axis, coloured by pitch class
-- | off the same twelve-hue table the ladder uses. Colour carries the content
-- | (the `drift` rows), height carries the spacing (the `density` columns), and
-- | the glyph therefore shows both axes of the grid it sits in. It also stops
-- | the lens looking like Banks, which it was never doing on purpose.
registerStrip :: forall m. ChordNode -> Array (H.ComponentHTML Action Slots m)
registerStrip c =
  [ SE.line
      [ SA.x1 0.0, SA.y1 (-13.5), SA.x2 0.0, SA.y2 13.5
      , HP.style "stroke: #e6dfcc; stroke-width: 1;" ]
  -- Middle C, so a register can be read rather than only compared.
  , SE.line
      [ SA.x1 (-4.0), SA.y1 (yFor 60), SA.x2 4.0, SA.y2 (yFor 60)
      , HP.style "stroke: #ece5d2; stroke-width: 1;" ]
  ]
    <> map dot (sort (nub (playNotes c)))
  where
  -- C1 to C7 across the glyph: wider than any voicing the app makes, so nothing
  -- ever clips and two pads are always on the same scale.
  yFor m = 13.0 - (toNumber (clamp 24 96 m) - 24.0) * 26.0 / 72.0
  dot m =
    SE.circle
      [ SA.cx 0.0, SA.cy (yFor m), SA.r 2.0
      , SA.class_ (cn ("ladder-dot ladder-dot--" <> show (mod m 12)))
      , HP.style "pointer-events: none;" ]

-- | Is the pointer in a Vary grid — either the standalone lens or the panel
-- | under a rehearsal slot? The two are the same tool, so they audition alike.
inVaryGrid :: State -> Boolean
inVaryGrid st = st.stage == Hunt Vary || isJust (varyingSlot st)

-- | The slot whose variations are open under the progression, if that is what
-- | is open. Most of the app only cares about this one case, so it asks for it
-- | directly rather than matching the whole `Pane`.
varyingSlot :: State -> Maybe Int
varyingSlot st = case st.pane of
  Just (PaneVary i) -> Just i
  _ -> Nothing

-- | **Which rehearsal slot a keep belongs to, if any.**
-- |
-- | The open panel first, then — for the case where you reached the Vary lens
-- | some other way while a rehearsal happens to be up — the slot whose chord
-- | this actually IS. Keyed by notes, the same key the free-standing tray uses,
-- | so a keep lands in the progression however you got to the grid. Without the
-- | second case a keep made from the lens dropdown silently went to the tray and
-- | never reached the slot it plainly belonged to.
slotForKeep :: State -> Maybe Int
slotForKeep st = case varyingSlot st of
  Just i -> Just i
  Nothing -> varySource st >>= \src ->
    findIndex (\sl -> map playNotes (head sl.options) == Just (playNotes src)) st.rehearsal

-- | Is this variation already kept? During an EXCURSION that means the slot
-- | that sent us; otherwise the free-standing tray. Same question, two places
-- | the answer can live, and the ring on the pad has to tell the truth in both.
isKept :: State -> ChordNode -> Boolean
isKept st c = case slotForKeep st of
  Just i -> case index st.rehearsal i of
    Just sl -> any (\o -> playNotes o == playNotes c) (drop 1 sl.options)
    Nothing -> false
  Nothing -> case varySource st of
    Nothing -> false
    Just src ->
      any (\e -> e.notes == playNotes src && any (\o -> playNotes o == playNotes c) e.options) st.kept

-- | **What you have kept, and how big a space it makes.**
-- |
-- | The product is the point: four variations on each of three chords is
-- | sixty-four progressions, and the number is worth showing because it is the
-- | thing you are actually building. One chord with variations is a choice; three
-- | is a space.
keptTray :: forall m. State -> H.ComponentHTML Action Slots m
keptTray st
  | length st.kept == 0 = HH.text ""
  | otherwise =
      HH.div
        -- Sticky rather than in flow: the tray is the RECORD of what you are
        -- doing, and it was landing below the fold of a nine-cell grid, which
        -- meant the count you were building was the one thing you could not see.
        [ HP.style ("position: sticky; bottom: 0; margin-top: 18px; border-top: 1px solid #ece5d2; "
                     <> "padding: 10px 0 2px; background: linear-gradient(180deg, #ffffffd9, #ffffff); "
                     <> "backdrop-filter: blur(2px);") ]
        [ HH.div
            [ HP.style "font-size: 11px; color: #a09880; letter-spacing: 0.04em; margin-bottom: 8px; -webkit-user-select: none; user-select: none;" ]
            [ HH.text ("kept · " <> show (length st.kept)
                        <> (if length st.kept == 1 then " chord · " else " chords · ")
                        <> show (keptSpace st) <> " progressions in the space"
                        <> " · each count includes the original") ]
        , HH.div
            [ HP.style "display: flex; flex-wrap: wrap; gap: 8px;" ]
            (mapWithIndex keptChip st.kept)
        ]

-- | How many distinct progressions the kept sets describe: the product of the
-- | per-chord option counts.
keptSpace :: State -> Int
keptSpace st = foldl (\n e -> n * max 1 (length e.options)) 1 st.kept

keptChip :: forall m. Int -> KeptFor -> H.ComponentHTML Action Slots m
keptChip i e =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 6px; border: 1px solid #ece5d2; background: #fbf8f0; border-radius: 4px; padding: 4px 6px 4px 8px; font-size: 11px; color: #6a6250;" ]
    [ HH.span [ HP.style "font-weight: 500;" ] [ HH.text e.label ]
    , HH.span [ HP.style "color: #a09880;" ] [ HH.text (show (length e.options) <> "×") ]
    , HH.button
        [ HP.style "border: none; background: none; color: #b3aa92; cursor: pointer; font-size: 12px; line-height: 1; padding: 0 2px;"
        , HP.title "forget every variation kept for this chord"
        , HE.onClick \_ -> ForgetKept i ]
        [ HH.text "×" ]
    ]

varyColHead :: forall m. HV.Density -> H.ComponentHTML Action Slots m
varyColHead dn =
  HH.div
    [ HP.style "font-size: 11px; color: #7a7360; letter-spacing: 0.08em; text-transform: uppercase; padding-bottom: 2px; border-bottom: 1px solid #e6dfcc; -webkit-user-select: none; user-select: none;"
    , HP.title (HV.densityBlurb dn) ]
    [ HH.text (HV.densityLabel dn) ]

varyRowHead :: forall m. HV.Drift -> H.ComponentHTML Action Slots m
varyRowHead d =
  HH.div
    [ HP.style "font-size: 11px; color: #7a7360; letter-spacing: 0.08em; text-transform: uppercase; padding-top: 14px; text-align: right; -webkit-user-select: none; user-select: none;"
    , HP.title (HV.driftBlurb d) ]
    [ HH.text (HV.driftLabel d) ]

-- | One cell. A cell holding fewer than sixteen has been EXHAUSTED, not
-- | truncated — near the chord the neighbourhood is genuinely small — so the
-- | count is shown rather than padded out with repeats.
varyBank :: forall m. State -> Array Vary.Cell -> H.ComponentHTML Action Slots m
varyBank st cs = case head cs of
  Nothing -> HH.div [] []
  Just cell ->
    HH.div
      [ HP.style "background: #fbf8f0; border: 1px solid #ece5d2; border-radius: 5px; padding: 6px;" ]
      [ HH.div
          [ HP.style "display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 4px;" ]
          (map (varyPad st) cell.chords)
      , if length cell.chords >= Vary.varyRows * Vary.varyCols then HH.text ""
        else HH.div
               [ HP.style "font-size: 9px; color: #b3aa92; text-align: right; padding-top: 4px; -webkit-user-select: none; user-select: none;"
               , HP.title "every distinct voicing this cell holds — the neighbourhood is exhausted" ]
               [ HH.text (show (length cell.chords) <> " — all there is") ]
      ]


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
          , HE.onClick \_ -> AuditionNode c
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
      bo = fromMaybe 60 (head sorted) / 12
  in { id: nid, parentId: Nothing, root: bp, bassPc: bp, bassOct: bo
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
          else HE.onMouseDown \ev ->
                 if ME.shiftKey ev
                   then maybe (SelectVoice c.id (UpperVoice (j - 1))) (\tn -> DropTone (ME.toEvent ev) c.id tn.ix tn.oct) (toneAt c m)
                   else DragStart (ME.altKey ev) true c.id (j - 1) m
        ]
      -- the omitted tones, clickable back on at the position they would return to
      ghost r =
        if OV.sounds r.place then []
        else map (\k ->
          SE.circle
            [ SA.cx (prowPitchX (r.base + 12 * k)), SA.cy cy, SA.r 4.5
            , SA.class_ (cn ("ladder-dot--off ladder-dot--" <> show r.pc))
            , HE.onClick \ev -> PlaceTone (ME.toEvent ev) c.id r.ix k
            ]) (ghostOctaves r.base)
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
          (map octLine [ 36, 48, 60, 72, 84 ] <> mapWithIndex dot (playNotes c) <> concatMap ghost (ghostRows c))
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
    Just c -> mapWithIndex (dot c.id) (playNotes c) <> concatMap (ghost c.id) (ghostRows c)
  -- A tone that is not played still gets a mark, at the position it would
  -- return to. Without it there is nothing to click, which is the whole reason
  -- omission has never been reachable here.
  -- One ghost per octave the tone could occupy, not just its lowest: the
  -- stack position is the LOWEST place a tone can sit, so every other option is
  -- above it, and offering them all turns restore-then-drag into one click.
  -- They stay legible because a ghost wears its tone's own hue (the same
  -- pitch-class colour the solid dots use), so an interleaved column of two
  -- dropped tones still reads as two.
  ghost cid r =
    if OV.sounds r.place then []
    else map (ghostAt cid r) (ghostOctaves r.base)
  ghostAt cid r k =
    SE.circle
      [ SA.cx dotX, SA.cy (midiToY (r.base + 12 * k)), SA.r 6.5
      , SA.class_ (cn ("ladder-dot--off ladder-dot--" <> show r.pc))
      , HE.onClick \ev -> PlaceTone (ME.toEvent ev) cid r.ix k
      ]
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
      -- shift-click omits the tone; plain drag is unchanged. The bass is not a
      -- tone the spread can reach, so it is never omittable.
      , if i == 0 then HE.onMouseDown \_ -> SelectVoice cid BassVoice
        else HE.onMouseDown \ev ->
               if ME.shiftKey ev
                 then maybe (SelectVoice cid (UpperVoice (i - 1))) (\tn -> DropTone (ME.toEvent ev) cid tn.ix tn.oct) (msound >>= \c -> toneAt c m)
                 else DragStart (ME.altKey ev) false cid (i - 1) m
      ]
    )

-- | The octaves a dropped tone could return at — from its stack position (the
-- | lowest it can sit) up to the top of the drawn ladder. Bounded by what is
-- | visible rather than by `reach`, because an option you cannot see is not an
-- | option.
ghostOctaves :: Int -> Array Int
ghostOctaves base = filter (\k -> base + 12 * k <= 84) (range 0 3)

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
    -- SOUNDING tones only: you cannot foot the chord on a note you have just
    -- chosen not to play, and `refootNode` has nothing to trade with if you try.
    let tones = sort (nub (map (\m -> mod m 12) (playNotes c)))
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
       , HH.div
           [ HP.style "display: flex; gap: 6px; justify-content: center; margin-top: 10px;" ]
           [ rvBtn "8ve ▼" "the whole chord down an octave, bass included" (ShiftOctave (-1))
           , rvBtn "⟲ invert" "roll the lowest voice down — the previous inversion" (RollBass (-1))
           , rvBtn "invert ⟳" "roll the lowest voice up — the next inversion" (RollBass 1)
           , rvBtn "8ve ▲" "the whole chord up an octave, bass included" (ShiftOctave 1)
           , rvBtn "vary ⋯" "open this chord's whole neighbourhood — drift × density" (OpenVary c)
           ]
       , HH.div [ HP.style "margin-top: 8px; font-size: 11px; color: #9a9a9a; text-align: center;" ]
           [ HH.text "Tab voicings · ↑↓ nudge · drag = 8ve · ⌥ doubles · ⇧ drops a note · f keep · Esc" ]
       ]
  rvBtn label tip act =
    HH.button
      [ HP.style ("border: 1px solid #d8d8d8; background: #fafafa; color: #4a4a4a; cursor: pointer; "
                   <> "padding: 3px 12px; border-radius: 3px; font-size: 12px;")
      , HP.title tip
      , HE.onClick \_ -> act ]
      [ HH.text label ]
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
      , HE.onClick \_ -> PlayChordId c.id
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
