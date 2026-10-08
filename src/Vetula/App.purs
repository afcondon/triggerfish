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

import Control.Alt ((<|>))

import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Effect.Now (now)

import Data.Array (concat, concatMap, deleteAt, drop, elem, elemIndex, filter, find, findIndex, head, index, insertAt, last, length, mapMaybe, mapWithIndex, modifyAt, nub, nubByEq, range, replicate, snoc, sort, sortBy, take, updateAt, zipWith, (!!))
import Data.Foldable (all, any, foldl, for_, maximum, minimum, sum)
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
import Effect.Aff (Milliseconds(..), attempt, delay)
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


import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WE
import Vetula.SvgCoord (svgYFromEvent, svgXFromEvent, isFormField, surfaceHidden)
import Vetula.Field as Field
import Vetula.Voice as Voice

import Vetula.Generate (GenMode(..), generateCandidates)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Midi.Routing as Routing
import Triggerfish.Glyph (ChipView, Glyph, sessionAliasOf)
import Triggerfish.Bar (Bar)
-- Qualified: `chordGlyph` is also the name of this module's lattice-node
-- renderer, which draws a chord and has nothing to do with identity.
import Triggerfish.Clips.Share as Share
import Triggerfish.Glyph as TGlyph
import Triggerfish.GlyphView (faIcon)
import Triggerfish.Preset (Preset, indexOfContent)
import Vetula.Store as Store
import Triggerfish.Amphora as Amphora
import Reef.Vetula.Perf (VChord, VVoice, VDest(..), VRenderer(..), PerfClock, cursorAtClock, renderAlphaBlockMidiAt, renderAlphaClockMidiAt, wrapAt) as RV
import Reef.Vetula.Articulate (VArticulator(..), articulate) as RA
import Reef.Route (printKey) as Route
import Vetula.Playhead (clockFor, defaultPattern, noteClock)
import Vetula.Realise (fromChords)
import Vetula.Perform.Types (ArpDir(..), ChannelMode(..), Layer, PerfFx(..), PerfSel(..), PerfTerm(..), PhraseAttach, VoiceShape(..), When(..), arpOrder, mkLayer, parseVoiceShape, termRigOnly)
import Triggerfish.PatternArg (PatternArg, argSrc)
import Tidal.Pattern.Core (compress, stack, fast, slow, every, whenCycle)
import Vetula.Pattern (arpIndexed, arpRate, cycleRand, withSampledArg)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Arc(..), eventPart, eventValue, eventWhole, isDigital, mkArc, mkState, query)
import Tidal.Pattern.Types (Pattern, Event) as PT
import Haskell.Rational as Rat
import Haskell.Rational ((%))
import Triggerfish.Clips (MidiClip, NoteEvent, headCount)
import Triggerfish.Clips.Store as ClipStore

import Data.Either (Either(..))

import Binnacle.Time (dateNow, perfNow)
import Effect.Ref as Ref
import Triggerfish.Capture.Logbook as Logbook
import Triggerfish.Capture.Types (Orientation(..), PlaySource(..), RegionEdge(..), Zoom(..))
import Triggerfish.Capture.River (Flow(..), riverPanel)
import Triggerfish.Capture.River as River

import Triggerfish.Capture.View (CaptureState, capturePanel, markCode)
import Triggerfish.Capture.View as CaptureView
import Triggerfish.Ui.Pointer as Pointer
import Vetula.Tidal (progressionSource, progressionSourceIn, parseBeats, parseProgression)
import Reef.Vetula.VoiceName (voiceLetter)
import Vetula.Lepidoptera (PerfDoc, VoiceSpec, cardProgression, docFromVoices, parseCardIn, parsePerform, printAsRecord, printCard, printProgressionIn)
import Vetula.StageCards as SC
import Triggerfish.Selene.Drop as Drop
import Vetula.Score as Score
import Harmonia.Substitute as HSub
import Harmonia.Voicing (voiceLead) as HVL
import Harmonia.Chord (Chord(..)) as HC
import Data.Array as Array
import Triggerfish.Odonus.View.Progression (chordName) as OP
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
import Harmonia.Chord (Key, Mode(..), cMajorKey)



import Vetula.Pads as Pads
import Harmonia.Recognise (best, candidateName, observeWithBass)
import Harmonia.Vary as HV
import Vetula.Vary as Vary
import Vetula.Spread (applyToNode, ghostRows, invertNode, nextBassTone, refootNode, spreadOfNode, toneAt)
import Harmonia.OpenVoicing (at, dropAt, setTone, sounds) as OV
import Vetula.Harmony (ChordNode, Kind(..), bassMidi, diatonicSevenths, diatonicTriads, keyX, latticeFamily, noteName, octaveShift, place, playNotes, scaleSet, triadNode, triadOn, voicingCandidates)

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
-- |
-- | **Explore's ladder** (AC, 2026-10-06). The stage the UI calls EXPLORE (the
-- | constructor is still `Hunt`) offers its views in a strict order of
-- | complexity, as buttons: Key · + four-note · + extended · Banks. The first
-- | three are ONE surface, the voice-leading lattice, shown in growing
-- | amounts: the key's triads and sevenths along its bottom, then every
-- | chord of four notes or fewer above them, then the rest (AC, 2026-10-06:
-- | "the lattice could be strictly additive to that view"). Nothing moves
-- | between them; chords appear in place. The colour sets are not a rung: they
-- | are a layer, added from a tray on the surface, ringed in every rung, and the
-- | key's own chords are one of them (diatonic).
-- | Vary and the relatives bloom stopped being views: both work AROUND a chord
-- | you already have, so they open in a side panel on it (`SideTab`). Tonnetz is
-- | kept but off the ladder: too simple to follow triads and sevenths, too
-- | unfamiliar to open with, and at its best as a playing surface. The circle
-- | of fifths (`Fifths`) is off the ladder too, reachable by URL.
data Viewtype = Fifths | Tonnetz | KeyChords | Common | Lattice4 | Lattice | Pads | Score | River

-- | The side panel beside an Explore view: a chord's variations (Harmonia.Vary,
-- | drift × density) or its relatives (voice-led neighbours, `generateCandidates`).
data SideTab = SideVariations | SideRelatives | SideSubstitutes

derive instance eqSideTab :: Eq SideTab

derive instance eqViewtype :: Eq Viewtype

-- | **What the main space shows** (docs/kb/plans/vetula-one-surface.md).
-- |
-- | One surface since 2026-10-08: the four stages (Explore, Rehearse,
-- | Perform, Review) are gone, and the main space shows one view at a time,
-- | a lattice rung, the score, or the river, picked in the bar. `Hunt` is
-- | the name the stage had while it was one of several; a view is what you
-- | are LOOKING AT, never what is running: voices keep sounding whichever
-- | view is up.
data Stage = Hunt Viewtype

derive instance eqStage :: Eq Stage

viewtypeLabel :: Viewtype -> String
viewtypeLabel = case _ of
  Fifths -> "fifths"
  Tonnetz -> "tonnetz"
  KeyChords -> "key"
  Common -> "+ common"
  Lattice4 -> "+ four-note"
  Lattice -> "+ extended"
  Pads -> "banks"
  Score -> "score"
  River -> "river"

-- | A view's stable string, for the URL and the remembered default.
viewtypeValue :: Viewtype -> String
viewtypeValue = case _ of
  Fifths -> "fifths"
  Tonnetz -> "tonnetz"
  KeyChords -> "key"
  Common -> "common"
  Lattice4 -> "lattice4"
  Lattice -> "lattice"
  Pads -> "banks"
  Score -> "score"
  River -> "river"

-- | The inverse; the old names ("fifths", "pads") still land, and anything
-- | unknown (the retired "explore" and "vary" views) lands on Key.
viewtypeFromValue :: String -> Viewtype
viewtypeFromValue = case _ of
  "fifths" -> Fifths
  "tonnetz" -> Tonnetz
  "common" -> Common
  "lattice4" -> Lattice4
  "lattice" -> Lattice
  "banks" -> Pads
  "pads" -> Pads
  "score" -> Score
  "river" -> River
  _ -> KeyChords

-- | What each rung shows, for its button's tooltip.
viewtypeTip :: Viewtype -> String
viewtypeTip = case _ of
  Fifths -> "the key's chords on the circle of fifths"
  Tonnetz -> "the tonal net: triads sharing two notes sit side by side"
  KeyChords -> "the key's own chords: a triad and a seventh on every degree"
  Common -> "the chords a lead sheet names, on every degree: sus2, sus4, 6, add9, 7sus4, shells, 9ths, 6/9"
  Lattice4 -> "the lattice above them: every chord of up to four notes built from each degree"
  Lattice -> "the whole lattice: chords of five notes and more, up to the thirteenth"
  Pads -> "nine banks of sixteen: how far from home, by how rich"
  Score -> "the progression on a grand staff, and every progression a voice is playing, with the voices' letters"
  River -> "what was played: every note, by voice, in time; mark, loop, cut a phrase into the clip library"

-- | The view a Stage shows.
huntOr :: Viewtype -> Stage -> Viewtype
huntOr _ (Hunt vt) = vt

-- | The URL segments for a stage: `["hunt","tonnetz"]`, `["perform"]`,
-- | `["review"]`. Vetula owns this vocabulary — `Triggerfish.Route` carries the
-- | segments opaquely and never learns what a stage is.
stagePath :: Stage -> Array String
stagePath = case _ of
  Hunt vt -> [ "explore", viewtypeValue vt ]

-- | The inverse. `Nothing` for anything unrecognised, so a stale or hand-typed
-- | URL leaves the app where it is rather than dumping it somewhere arbitrary.
-- | A bare `["hunt"]` (no projection) is legal and lands on `lastLens`, which is
-- | why the caller passes it in.
stageFromPath :: Viewtype -> Array String -> Maybe Stage
stageFromPath fallbackLens segs = case segs of
  -- the stages before one surface: Review's river is a view now; Rehearse
  -- and Perform are gone, and land on the lattice
  [ "review" ] -> Just (Hunt River)
  [ "perform" ] -> Just (Hunt KeyChords)
  [ "rehearse" ] -> Just (Hunt KeyChords)
  [ "explore" ] -> Just (Hunt fallbackLens)
  [ "explore", vt ] -> Just (Hunt (viewtypeFromValue vt))
  [ "hunt" ] -> Just (Hunt fallbackLens)
  [ "hunt", vt ] -> Just (Hunt (viewtypeFromValue vt))
  _ -> Nothing

-- | Where Vetula's chord/path AUDITION goes, chosen in the shell's routing modal
-- | (2026-08-01): Off (muted), Browser (Vetula's own Web Audio electric piano,
-- | 2026-10-06), Continuo (the piano+strings VST preview via the "continuo"
-- | virtual port), or Midi (the rig/IAC bus, on the preview channel). The shell
-- | drives this with SetAuditionQ; connectMidi picks the port from it. Whenever
-- | no port answers, the audition falls back to the browser voice, so a page
-- | with nothing installed still sounds.
data AuditionSel = AuditionOff | AuditionBrowser | AuditionContinuo | AuditionMidi

-- | How an audition plays a chord (plan: "From exploring to progressions"):
-- | a sticky lens, shown in the bar and set by the number keys on the field.
-- | Block and arpeggio first; the strings styles take keys 3–6 when a second
-- | voice exists.
data AuditionStyle = StyleBlock | StyleArp

derive instance eqAuditionStyle :: Eq AuditionStyle

derive instance eqAuditionSel :: Eq AuditionSel

-- | The sound chip's names, and its cycle: browser → continuo → MIDI → off.
soundValue :: AuditionSel -> String
soundValue = case _ of
  AuditionOff -> "off"
  AuditionBrowser -> "browser"
  AuditionContinuo -> "continuo"
  AuditionMidi -> "midi"

soundFromValue :: String -> Maybe AuditionSel
soundFromValue = case _ of
  "off" -> Just AuditionOff
  "browser" -> Just AuditionBrowser
  "continuo" -> Just AuditionContinuo
  "midi" -> Just AuditionMidi
  _ -> Nothing

nextSound :: AuditionSel -> AuditionSel
nextSound = case _ of
  AuditionBrowser -> AuditionContinuo
  AuditionContinuo -> AuditionMidi
  AuditionMidi -> AuditionOff
  AuditionOff -> AuditionBrowser

-- | A chord's short name from its root + quality (major bare, minor "m", else
-- | the bare root).
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
type SimRow = (targetX :: Number, targetY :: Number, radius :: Number)
type VNode = SimulationNode SimRow

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



-- | **The source chord is option zero, always.**
-- |
-- | Keeping it inside the set rather than beside it makes every later question
-- | uniform: locking to the original is not a special case, a slot with nothing
-- | kept is a one-element set that multiplies to 1 rather than 0, and forgetting
-- | every variation leaves a well-formed slot instead of a hole. It also says
-- | the true thing — when the progression runs, the chord you started with is a
-- | legitimate choice unless you have decided otherwise.



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
  -- The key's lattice, laid out: built when the key changes rather than on
  -- every render, which it was, at 150 ms a hover.
  , lattice :: Array (Array LatMember)
  -- The banks' pads, placed: rebuilt with the key or a shuffle, for the same
  -- reason (the walks are the dearest thing on the page to compute).
  , bankPads :: Array BankPad
  -- Explore's field, drawn outside Halogen (Vetula.Field); made in Initialize
  , field :: Maybe FieldRt
  -- The rig's resting harmonic scale (macro-tidal harmonic-authority): Nothing =
  -- follow the key's diatonic set; Just = an explicit `# scale` override (root pc
  -- + intervals from any Reef scale, beyond the diatonic modes the key can name).
  -- Vetula is the single harmonic authority — this is what pitched voices quantise
  -- to when no chord is firing.
  , restScale :: Maybe { root :: Int, offsets :: Array Int }
  , chords :: Array ChordNode          -- the model (pin, provenance, layout targets)
  , nodes :: Array VNode               -- live positions from the simulation
  , hoveredId :: Maybe Int
  , hoveredTriad :: Maybe { root :: Int, pcs :: Array Int }  -- Tonnetz hover (no pool id)
  , nextId :: Int
  , handle :: Maybe (SimulationHandle SimRow)
  , subId :: Maybe H.SubscriptionId
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , auditionSel :: AuditionSel      -- where the audition goes (Off/Continuo/Midi); shell-driven
  , previewChan :: Int              -- the MIDI channel chord/path AUDITION plays on (own
                                    -- routable channel, so ATLANTIS preview can be cued
                                    -- separately from the voices; 0-indexed like voices)
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
  -- the tonnetz triad STACK (2026-07-31 redesign): triads accumulated by
  -- alt-clicking triangles, in pick order. Edge-adjacent triads fold into
  -- 7ths/9ths naturally (the polychord is the pitch-class union); the whole
  -- stack catches to the tank as one Anchor. Empty = not stacking.
  -- chords reconstructed by pasting a saved Tidal progression back in. They live
  -- in `chords` (so the Revoice ladders + export work on them) but are kept off
  -- the Explore/Lattice surfaces — they aren't lattice nodes.
  , imported :: Set Int
  -- the Tidal-source textarea's verbatim content while the user is editing it
  -- (Nothing = show the live-derived source, which tracks revoicing).
  , helpOpen :: Boolean     -- is the ⓘ help overlay open? (the notes, off the canvas)
  -- "pick mode": shift-clicked progression step indices (max 2). When non-empty,
  -- the left surface shows a generated candidate cloud to insert/substitute.
  , genSel :: Array Int
  , candidates :: Array ChordNode
  , adventure :: Number      -- 0 = smoothest candidates … 1 = most striking
  -- ARRANGE (control B): how many bridge chords `Vetula.Between` lays in front of
  -- a tank chord as it's dropped into the progression — the "cadence length" dial
  -- (0 = drop it bare, 1 = V, 2 = ii–V, …). See docs/DESIGN-vetula-progression-building.md.
  -- Floating-control fold state: each card collapses to just its header (click the
  -- title bar) to cede the stage to the underlying music viz. See `floatCard`.
  -- Performance tab — the progression library + the loaded working copy + voices.
  , library :: Array LibEntry
  -- Auto-capture bookkeeping (Slice 1): the current progression is captured to the
  -- library on a slow timer — one ephemeral entry per building session, UPDATED in
  -- place as you build (deduped by `lastCapSig`), then frozen when you promote it.
  , capSeq :: Int                 -- running number for ephemeral autonames (◦N)
  , lastCapIdx :: Maybe Int       -- library index of the current session's ephemeral (Nothing = start fresh)
  , lastCapSig :: String          -- signature (currentSource) of the last capture, for dedup
  , publishMsg :: Maybe String    -- transient status from a publish-entry-to-Amphora click
  -- Slice 4a — the live `path` IS the performed progression (no separate loaded
  -- working copy); the chords come from `path`. `progName` is its NAME, frozen
  -- (plan: "Names that survive edits"): minted as a glyph at the first chord
  -- taken, or the library entry's when one is loaded, and kept through every
  -- edit; it is the Amphora label each settled version is published under.
  , progName :: Maybe String
  , lastPubSig :: String          -- the source last SAVED (a version in the library and Amphora)
  -- The AUDITION CARD (plan, step 4a): a card on the stage holding the
  -- progression in the current style, kept current by Vetula until it is
  -- edited in Limulus, when it becomes the composer's (`owned`) and is never
  -- rewritten again.
  , auditionCard :: Maybe { cardId :: Int, owned :: Boolean }
  , voices :: Array Voice
  , armed :: Boolean              -- the ARM/cue flag (sticky). Vetula keeps its own arm
                                  -- lifecycle (standalone PerfPlay/PerfStop/unload); the
                                  -- shell mirrors it through SetSounding (`Silent` ⇒ disarm).
  , authority :: Sounding        -- where PERFORMANCE output goes (MISU refactor, replaces
                                  -- master+audible): Local = local Web-MIDI, Rig = muted
                                  -- locally, the voices on the rig. Standalone stays Local.
  , playing :: Boolean           -- derived: currently sounding (= armed, under authority)
  , pulse :: Int                  -- the shared clock's 16th-note grid index (from the scheduler tick)
  , tempo :: Int                  -- BPM display (tracks the live clock; the bpm field nudges the free baseline)
  , binnacle :: Maybe Binnacle.Binnacle  -- the shared transport (free-run → Link-lock), like Odonus/Balistes
  -- The cards as the rig's stage holds them, as far as this page knows (card id →
  -- its line); Nothing until the stage has answered a subscribe. See Vetula.StageCards.
  , stageCards :: Maybe (Map Int String)
  -- | The saved progressions the stage holds, by name (step 4b), as far as
  -- | this page knows; `Nothing` until the rig answers.
  , stageProgs :: Maybe (Map String String)
  -- | The cards that name a progression, by number, as last written: the
  -- | composer's lines, which this page never prints back (not even after
  -- | refusing a write to one).
  , namedCards :: Map Int String
  -- the score's chosen run of chords (a row by its title), for the scales
  , scoreSel :: Maybe { row :: String, anchor :: Int, to :: Int }
  -- rows spelled in their own reading rather than the key
  , scoreRead :: Set String
  -- the open progression's bar the side panel's chords go into, and a bar being dragged
  , scoreBar :: Maybe Int
  , scoreDrag :: Maybe Int
  , hoveredBar :: Maybe Int       -- the bar of the score under the pointer
  -- an unsaved progression about to be lost, asked once: the same act again
  -- within a moment goes through (`guardLoss`)
  , lossArmed :: Maybe { what :: String, at :: Number }
  -- score mode's two drawers: Limulus on the right, the candidates below
  -- the candidates for the chord in hand, computed once per chord (`refreshCands`)
  , cands :: Maybe Cands
  , scoreCands :: Boolean
  -- the open progression's rhythm, tapped in: each bar's length in beats
  -- (empty: one chord a bar); and a take in progress (the bar sounding, the
  -- press times so far)
  , rhythm :: Array Int
  , tapping :: Maybe { at :: Int, times :: Array Number }
  , scorePad :: Maybe ChordNode
  -- Vetula's key as last written to the stage (`vetula/key`, Reef.Route.printKey),
  -- which the router's `vetula key` row feeds Odonus's grid from.
  , stageKey :: Maybe String
  , clockTempo :: Number          -- the clock's live tempo, read each tick (drives note durations)
  , routing :: Map String Int   -- name → canonical MIDI channel, pushed from the Tidal page
  -- Tank model (Slice A): the durable, unordered collection of CAUGHT chords.
  -- Frozen `Specimen`s reference no lattice node, so the volatile lattice can
  -- reflow/regenerate underneath without disturbing them. `k` over a chord catches
  -- it here; the tank persists until cleared and will feed the Stage + Sequences.
  , stage :: Stage                -- Hunt <projection> | Perform | Review — see `Stage`
  , lastLens :: Viewtype          -- the Hunt projection to return to from Perform/Review
  , fieldLens :: Viewtype         -- the last lattice view (a rung or banks), to return to from the score
  , lastRung :: Viewtype          -- the last lattice rung, for the lattice | banks switch
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
  -- REHEARSE: a progression with alternatives at each slot. Empty = nothing
  -- taken up yet, and the stage offers the saved tokens to start from.
  -- **Paths you approved.** Held by CONTENT, not by index: dropping an option
  -- renumbers every index above it, and a mark that silently re-pointed at its
  -- neighbour would be the worst kind of wrong — a decision you made, recorded
  -- against a chord you did not choose. Content survives any edit.
  --
  -- Not regenerable, which is why it is stored at all: the lattice can produce
  -- every path, but which ones you liked exists nowhere else.
  -- What is open under the progression, if anything.
  -- The chord the Vary lens is working on. `Nothing` falls back to whatever is
  -- sounding, so the lens is never empty for no reason.
  -- the density the variations column shows (an index into `HV.densities`)
  , lastHeard :: Maybe ChordNode
  , style :: AuditionStyle
  -- the viewer chose where auditions sound (the chip, or the shell's ⌥1);
  -- until then, the browser, and Continuo once the rig answers
  , soundChosen :: Boolean
  -- Explore's cursor: the chord a click selected (by its field key). Keys act
  -- on it: space plays it, return takes it, esc lets it go.
  , cursor :: Maybe { key :: String, chord :: ChordNode }
  , sideDensity :: Int
  -- The wheel's travel since the last level step, and when that step was, so
  -- one flick of a trackpad moves one level, not three.
  , wheelAcc :: Number
  , wheelAt :: Number
  , wheelLast :: Number
  -- the progression open when the page last closed, kept (not reopened) so
  -- the score can offer it back
  , resumable :: Maybe { name :: String, source :: String, saved :: String }
  -- The hovered PAD, carried whole. `hoveredTriad` would be enough to highlight
  -- it, but not to preview it: that path re-voices from pitch classes, and a
  -- pad's open voicing (bass pinned to the root, contour smoothed against its
  -- neighbours in the bank) is exactly what must NOT be thrown away — space and
  -- click have to sound the same chord.
  , hoveredNode :: Maybe ChordNode
  -- Tank model (Slice B): the staged seeds. Clicking a tank specimen injects it
  -- into the pool as a centre chord (`seedChord` maps the specimen → its pool
  -- chord id) and blooms its neighbours around it; clicking again unstages it.
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
  -- Chyron interaction. `hoveredChyron` = the chip index under the pointer (space
  -- auditions it, no re-log). `chyronSel` = the current selection, Mac text-editing
  -- semantics: a plain click drops a fresh single-chord selection (`lo==hi`) and
  -- sets the `anchor`; a shift-click extends the range from that fixed anchor
  -- (anchor stays put, the clicked chip becomes the moving end). `lo`/`hi` are the
  -- sorted span endpoints the render/save/play all read; `anchor` is the fixed end
  -- a subsequent shift-click re-extends from.
  -- The chip index currently being dragged to REORDER the buffer (Nothing = no
  -- drag in flight). Reordering makes the buffer a list, not a tape (§8): order,
  -- not timestamps, becomes the arrangement.
  -- Saved sequences: pinned 2-glyph tokens on the left of the chyron. Saving a
  -- selection compresses its live chips into one of these (reclaiming space).
  -- Record-arm: when false, auditions still SOUND but don't log to the trace
  -- (noodle without cluttering). Defaults true — always-on capture, the flow AC
  -- liked; disarm only when you want to explore off the record.
  -- PERFORM surface: player boxes (one per output) + the token "picked up" for
  -- placement (shift-click / drag a saved token, then click / drop on a box), and
  -- an fx "picked up" from the palette for placement onto a box's stack.
  , perfBoxes :: Array PerfBox
  -- The persistent Perform SESSION: the container for saved scenes. Resumes across
  -- reloads; scenes save as `⟨alias|name⟩ #nextScene`. Minted/restored in Initialize.
  , perfSession :: Store.SessionState
  -- Recall: scenes fetched from Amphora (collection `vetula-scene`), + modal flag.
  , perfScenes :: Array { hash :: String, name :: String, payload :: String, tags :: Array String }
  , perfRecallOpen :: Boolean
  -- The session/scene command menu in the secondary nav (the ⋯ dropdown off the
  -- session badge) — session + scene + chyron housekeeping, moved off the Perform
  -- header. (AC, 2026-08-03.)
  -- The shared MIDI clip library (#27), loaded from `Triggerfish.Clips.Store` in
  -- Initialize — the pool the phrase picker offers. `perfPhrasePick` is the box index
  -- whose picker is open (Nothing = closed).
  , clipLibrary :: Array MidiClip
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
  -- the live river's clock (performance µs), advanced only while review shows
  , riverNow :: Number
  -- The LIVE river's two reads (`Capture.River`): the current instant, advanced by
  -- a 33ms frame timer so the roll FLOWS rather than jumping a 16th at a time, and
  -- the recent notes it draws — pruned to the river's fade span each frame. The
  -- logbook keeps everything; this is just the window that's on screen.
  }


data Action
  = Initialize
  | MidiReady (Maybe Midi.MidiOut) String
  | SimTick
  | SimDone
  | Hover (Maybe Int)
  | HoverTriad (Maybe { root :: Int, pcs :: Array Int })  -- Tonnetz: hover a triad for space-preview
  | Key String Boolean     -- key, shift held
  | SelectKey String
  | SelectScale String
  | DragStart Boolean Boolean Int Int Int  -- alt (double)?, horizontal?, chord id, voicing index, MIDI at grab time
  | DragMove Event
  | DragEnd
  | SelectVoice Int VoiceSel -- chord id, the voice to select (and make sounding)
  | PickVoicing Int (Array Int) -- chord id, the favoured voicing to activate
  | OpenRevoice            -- open the revoice modal on the hovered/sounding chord
  | CloseRevoice           -- dismiss the revoice modal
  | SlashBass Int          -- set the revoiced chord's bass to a pitch class (slash chord)
  | ReflavourFamily String -- re-flavour the focused family's scale (mode value)
  | PlayPath               -- ▶ play the whole progression
  | ClearPath              -- ✕ empty the progression so the next shift-click starts fresh
  | ScorePress Int         -- a press on a bar: a drag starts (a release on it is a click)
  | HoverBar (Maybe Int)   -- the pointer over a bar of the score, or off it
  | AltClick ChordNode     -- an alternative clicked: heard, and in hand
  | AltPut ChordNode       -- an alternative shift-clicked: into the chosen bar
  | AltDragEnd             -- a drag of an alternative ended, dropped or not
  | ScoreHear (Array Int)  -- the score: hear a chord of a progression that is not open
  | ScoreRevoice Int       -- the score: open the ladder on a chord of the open progression (its id)
  | RevoiceFocus Int       -- the progression's ladders: focus (and hear) the chord at a position
  | RevoiceStep Int        -- move the focus a bar left / right
  | RevoiceLead            -- voice-lead every later bar from the focused one
  | VoiceDragOver Event    -- a progression dragged over a voice's badge: let it land
  | VoiceDrop Int Event    -- dropped on voice n: the voice reads that progression
  | TitleDrag String Event -- a score row's title dragged: it carries its progression
  | ScoreSelect String Int -- the score: shift-click a chord on a row (by title), choosing a run for the scales
  | ScoreUnselect
  | ScoreStep Int            -- the score: click a chord of the open progression (hear it, make it the bar the panel's chords go into)
  | ScoreDuplicate Int
  | ScoreDelete Int
  | ScoreDropAt Int
  | ScorePutAt Int ChordNode
  | ScoreHearPad ChordNode    -- a panel chord heard in the chosen bar's register
  | ScorePadDrag ChordNode    -- a panel chord picked up, to drop on a bar
  | ScoreDropPad Int
  | ScoreDragOver Event
  | ScoreReadIn String       -- the score: spell a row (by title) in its own reading, or back in the key
  | ScoreAdopt Int Mode      -- the score: make the open progression's reading the key, transposing nothing
  | TapStart               -- tap a rhythm in: the first chord sounds, space moves on
  | TapNext                -- space while tapping: the next chord (after the last, the take ends)
  | TapStop                -- Esc: the take abandoned, the rhythm as it was
  | ClearRhythm            -- back to one chord a bar
  | CopyTidal String       -- copy the progression's Tidal source to the clipboard
  | ToggleHelp             -- open / close the ⓘ help overlay
  | PickCandidate Int      -- insert/substitute the chosen candidate into the progression
  | CancelGen              -- leave pick mode
  | SetAdventure String    -- the adventurousness dial (slider value)
  -- Performance tab
  | AutoCapture            -- timer: auto-capture the current path (ephemeral, update-in-place)
  | LoadProg Int           -- load library entry #i into the performance working copy
  | SaveScene              -- serialise the whole Perform surface as a vetulaScene → Amphora
  | FetchScenes            -- fetch them for the browser drawer, quietly
  | PerfCloseRecall
  | PerfLoadScene String   -- parse a scene payload and load it onto the surface
  | SetTempo String
  | PerfTick Scheduler.Tick  -- one 16th-note pulse from the shared scheduler
  -- Tank model (Slice A)
  | PlayChordId Int        -- plain-click a pool chord: audition it (no path change)
  | AuditionTriad Int (Array Int)        -- Tonnetz: hear a triad off the net (root pc, pcs)
  -- Chyron: hover a chip (space auditions it), or click one — plain click selects
  -- a single chord, shift-click extends the range from the anchor (Mac semantics).
  -- PERFORM surface
  | StageOpen              -- the rig socket (re)connected: subscribe to the cards on the stage
  | StageFrameIn String    -- a frame from the rig; the stage's card frames are acted on
  | CardToLimulus Int      -- ask Limulus to show card n (`stage-open vetula/vN`)
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
  | CaptureRegionDown Int RegionEdge Int Int  -- a band's edge or body grabbed
  | CaptureRegionMove Int Int                 -- the pointer, while a band is dragged
  | CaptureRegionUp                           -- released: a click loops, a drag sets the window
  | CaptureDeleteMark Int                     -- delete a mark (its loop stops)
  | CaptureDismissCard Int                    -- put the loop card away; the loop plays on
  | Nop                    -- nothing (a handler that must name an action)
  | PerfStopClick ME.MouseEvent Action -- run Action but stop the click bubbling to the box
  | ZoomAt Event Number    -- geometric lens: wheel-zoom toward the cursor (event, deltaY)
  | PanStart Event         -- geometric lens: begin a grab-to-pan drag
  | PanMove Event          -- geometric lens: drag the viewport
  | PanEnd                 -- geometric lens: end the pan drag
  | ResetView              -- geometric lens: re-fit (zoom 1, centred)
  | ShakeGenerate          -- Generate lens: re-roll the tank-seeded relatives
  | ShufflePads            -- Banks lens: re-walk all nine banks
  | HoverPad (Maybe ChordNode)  -- Banks lens: hover a pad (highlight + exact preview)
  | SelectMark String ChordNode  -- Explore: a click makes a chord the cursor, and plays it
  | SetStyle AuditionStyle       -- Explore: the audition style (keys 1, 2), heard at once
  | SaveProg Boolean             -- save the progression (⌘S); true = as a new sibling (⌘⇧S)
  | TakeMark String ChordNode    -- Explore: a shift-click selects and takes it
  | ShuffleVary            -- re-draw all nine cells of the Vary lens from a new seed
  -- REHEARSE
  | ToQuadrat String       -- publish a saved progression (by name) as a clip for Quadrat to sample
  | ResumeWorking          -- reopen the progression open when the page last closed
  | ToggleScoreCands       -- score mode: the candidates drawer up, or tucked down
  | CycleSound             -- where previews sound: browser, continuo, MIDI, off
  | SetSideDensity Int
  | LevelWheel Event Number -- the wheel over the field: step the level of detail
  | RollBass Int           -- roll the revoiced chord's bass to the next/previous chord tone
  | ShiftOctave Int        -- move the revoiced chord bodily up/down an octave
  | PlaceTone Event Int Int Int -- put chord `id`'s tone `i` at octave `k` (click a ghost)
  | SetStage Stage         -- switch stage: Hunt <projection> | Perform | Review

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
  -- Live jump: re-anchor every voice so chord `i` reads NOW (from the nav strip).
  -- Playing → the ensemble advances to chord i and continues; stopped → the → odo
  -- feed + the strip's playhead move to i (so Odonus re-quantises). One gesture,
  -- both effects. See jumpVoice.
  | JumpChord Int a
  -- The ONE transport query (control-surface MISU refactor). The shell pushes the
  -- derived `Sounding`: `Silent` disarms, `Local` plays local Web-MIDI, `Rig` mutes
  -- locally, the rig plays the voices. Replaces SetMaster/SetAudible/
  -- SetArm/SyncToRig/StopRig. `AskSounding` reports the EFFECTIVE sounding (Silent
  -- when self-disarmed, e.g. unloading a progression) so the shell can reconcile.
  | SetSounding Sounding a
  | AskSounding (Sounding -> a)
  | SyncFree Number Number a    -- adopt the rack's shared free-run baseline (start micros, BPM)
  | AskLibrary (Array { name :: String, text :: String } -> a)   -- A5 manager
  | AskProgressions (Array { slot :: Int, name :: String, key :: String, current :: Boolean, rebus :: Array TGlyph.GlyphIcon } -> a)  -- the drawer's saved progressions
  | LoadEntry Int a
  | OpenChannelCard Int a   -- the drawer's voice row: show that channel's card in Limulus
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
  -- The browser drawer (docs/kb/plans/the-deck.md, 2026-10-05): the saved
  -- scenes (Amphora `vetula-scene`), carried as they are; load one; save one.
  | AskScenes (Array { name :: String, session :: String, key :: String } -> a)
  -- The controls the shell draws in its top bar (Triggerfish.Bar, AC
  -- 2026-10-05): the stage tabs, ◆ mark with its counts and clear (PERFORM and
  -- REVIEW), and the session's rebus; and what was pressed there.
  | AskBar (Bar -> a)
  | BarAct String a
  | LoadSceneAt Int a
  | SaveSceneQ a
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
      , lattice: latticeFor cMajorKey
      , bankPads: bankPadsFor cMajorKey 0
      , field: Nothing
      , restScale: Nothing
      , chords: []
      , nodes: []
      , hoveredId: Nothing
      , hoveredTriad: Nothing
      , nextId: 100          -- generated children start here; seeds are 0..17
      , handle: Nothing
      , subId: Nothing
      , midiOut: Nothing
      , midiName: "…"
      , auditionSel: AuditionBrowser    -- the browser's voice until the rig answers (then Continuo)
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
      , imported: Set.empty
      , helpOpen: false
      , genSel: []
      , candidates: []
      , adventure: 0.25
      , library: []
      , capSeq: 0
      , lastCapIdx: Nothing
      , lastCapSig: ""
      , lastPubSig: ""
      , auditionCard: Nothing
      , publishMsg: Nothing
      , progName: Nothing
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
      , stageProgs: Nothing
      , namedCards: Map.empty
      , scoreSel: Nothing
      , scoreRead: Set.empty
      , scoreBar: Nothing
      , scoreDrag: Nothing
      , hoveredBar: Nothing
      , lossArmed: Nothing
      , cands: Nothing
      , scoreCands: true
      , rhythm: []
      , tapping: Nothing
      , scorePad: Nothing
      , stageKey: Nothing
      , clockTempo: 120.0
      -- name → canonical MIDI channel, pushed from the shell's Tidal-page routing
      -- table (SetRouting). Unnamed / unbound voices fall back to the default channel.
      , routing: Map.empty :: Map String Int
      , presets: [], identity: Nothing
      , stage: Hunt KeyChords   -- Key, the first rung, unless a default is pinned
      , lastLens: KeyChords
      , fieldLens: KeyChords
      , lastRung: KeyChords
      , resumable: Nothing
      , lastHeard: Nothing
      , style: StyleBlock
      , soundChosen: false
      , cursor: Nothing
      , sideDensity: 0
      , wheelAcc: 0.0
      , wheelAt: 0.0
      , wheelLast: 0.0
      , viewCx: 0.0
      , viewCy: 0.0
      , viewZoom: 1.0
      , panning: Nothing
      , panMoved: false
      , genRoll: 0
      , padRoll: 0
      , varyRoll: 0
      , hoveredNode: Nothing
      -- four player boxes on MIDI ch 1-4 (Odonus I-IV in AC's routing); a token
      -- dropped on one loops there while the transport plays.
      , perfBoxes: map (\n -> { cardId: n, channel: n, label: "P" <> show n, seq: Nothing, stack: [], seqText: "", muted: false, term: TMidi, phrase: Nothing }) (range 1 4)
      -- placeholder; Initialize resumes the persisted session or mints a fresh one
      , perfSession: { alias: "", name: "", nextScene: 1 }
      , perfScenes: []
      , perfRecallOpen: false
      , clipLibrary: []
      , capture: { logbook: Logbook.emptyLog, playing: Nothing, regionDrag: Nothing, contextOpen: false, codeOpen: false, zoom: Whole, rig: Nothing, cutting: false, cutSel: Nothing, cardShut: Nothing }
      , rigLoops: false, rigAsked: 0.0, captureDragSub: Nothing, riverNow: 0.0
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
  --   * entering Rig   → the rig plays the voices (vetula-cards-play); leaving Rig
  --     → vetula-stop.
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
      -- the voices play on the rig, read from the stage (vetula_cards); the
      -- page plays them itself only in Local
      for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "vetula-cards-play"
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
  -- a progression from the drawer: work on it, on the score
  LoadEntry i next -> do
    guardLoss ("load " <> show i) do
      handleAction (LoadProg i)
      handleAction (SetStage (Hunt Score))
    pure (Just next)
  -- Limulus keeps in step only the cards it has a block for: this puts one
  -- back (or reveals it), for a card whose block was lost or never added.
  -- the drawer's voice row: its line in Limulus (the row is the voice's number)
  -- A voice from the drawer: the score for the progression it plays (if it
  -- names a saved one), with Limulus open and showing its block.
  OpenChannelCard n next -> do
    s <- H.get
    let named = s.stageCards >>= Map.lookup n >>= cardProgression
        loads = isJust named && s.progName /= named
        open = do
          for_ (named >>= \nm -> findIndex (\e -> e.kept && e.name == nm) s.library) \i ->
            when loads (handleAction (LoadProg i))
          handleAction (SetStage (Hunt Score))
          -- Limulus may only now be opening: give it a moment to listen
          liftAff (delay (Milliseconds 600.0))
          handleAction (CardToLimulus n)
    -- only a different progression replaces the open one
    if loads then guardLoss ("voice " <> show n) open else open
    pure (Just next)
  AskProgressions reply -> do
    s <- H.get
    pure $ Just $ reply $ map (\(Tuple i e) -> { slot: i, name: e.name, key: e.keyLabel, current: s.progName == Just e.name
                                               , rebus: bundleRebus (parseProgression e.source) })
      (filter (\(Tuple _ e) -> e.kept) (mapWithIndex Tuple s.library))
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
  AskScenes reply -> do
    s <- H.get
    let
      tagged pre item = maybe "" (SCU.drop (SCU.length pre)) (find (\t -> SCU.take (SCU.length pre) t == pre) item.tags)
      scene item = { name: item.name, session: tagged "session:" item, key: tagged "key:" item }
    pure (Just (reply (map scene s.perfScenes)))
  AskBar reply -> do
    s <- H.get
    pure $ Just $ reply
      -- the workspace's four views, and the key they share, in the shell's
      -- bar (AC, 2026-10-08): the second bar went
      { tabs:
          let v = huntOr s.lastLens s.stage
          in [ { id: "lattice", label: "lattice", active: elem v rungs, tip: "the chords built on the key, from triads up" }
             , { id: "banks", label: "banks", active: v == Pads, tip: "nine banks of sixteen: how far from home, by how rich" }
             , { id: "score", label: "score", active: v == Score, tip: "the progression on a grand staff, to arrange and play" }
             , { id: "review", label: "review", active: v == River, tip: "everything played this session, as a river: mark the good bits, loop them" } ]
      -- marking is in the review view, as on Odonus
      , marks: ""
      -- the session names saved scenes; it is not shown (AC: two things
      -- named by three glyphs side by side read as one)
      , icons: []
      , rebusTip: ""
      , pickers:
          [ { id: "key", input: (Select.defaultInput keyOptions) { selected = Just (show s.key.tonic), placeholder = "Key", minWidth = Just "72px" } }
          , { id: "scale", input: (Select.cascadingInput modeGroups) { selected = Just (currentModeValue s.key.mode), searchable = true } } ]
      , help: "keys & help"
      , chips:
          -- the progression being built: its frozen name as a monochrome
          -- glyph and whether this version is saved (its length is on the
          -- view, AC); pressing it saves, or once saved arranges it on the
          -- score. Then clear.
          [ case s.progName of
              Nothing ->
                { id: "prog", label: "no progression", icons: [], active: false, attention: false
                , tip: "on the lattice, shift-click (or return) takes a chord into a new progression" }
              Just nm ->
                let unsaved = length s.path > 0 && currentSource s /= s.lastPubSig
                in { id: "prog"
                   , icons: map (\icon -> { icon, color: "#2a2a2a" }) (filter (\w -> w /= "" && not (isJust (fromString w))) (split (Pattern "-") (SCU.takeWhile (_ /= '′') nm)))
                   , label: (if SCU.contains (Pattern "′") nm then "′ " else "") <> maybe "" (\k -> show k <> " \x00b7 ") (last (split (Pattern "-") nm) >>= fromString)
                       <> (if isJust s.lossArmed then "\x25cf unsaved: again to discard" else if unsaved then "\x25cf save" else "\x2713 saved")
                   , active: true, attention: unsaved
                   , tip: nm <> (if unsaved then " \x00b7 unsaved: click or \x2318S saves this version, \x2318\x21e7S a new sibling" else " \x00b7 saved \x00b7 click: arrange it, on the score")
                       <> " \x00b7 on the lattice, backspace takes back the last chord, delete starts a new progression" }
          ] <> (if length s.path == 0 then [] else
          [ { id: "clear", label: "clear", icons: [], active: false, attention: false
            , tip: "put this progression away and start a new one (delete, on the lattice)"
                <> (if currentSource s /= s.lastPubSig then " \x00b7 its unsaved changes are lost" else "") } ])
      }
  BarAct act next -> do
    s <- H.get
    case act of
      "stage:lattice" -> handleAction (SetStage (Hunt s.lastRung))
      "stage:banks" -> handleAction (SetStage (Hunt Pads))
      "stage:score" -> handleAction (SetStage (Hunt Score))
      "stage:review" -> handleAction (SetStage (Hunt River))
      "help" -> handleAction ToggleHelp
      _ | Just v <- SCU.stripPrefix (Pattern "pick:key:") act -> handleAction (SelectKey v)
        | Just v <- SCU.stripPrefix (Pattern "pick:scale:") act -> handleAction (SelectScale v)
      -- the progression: arrange it, on the score (saving is ⌘S, or the row's save)
      -- the progression's chip says save while unsaved, and does it; saved,
      -- it arranges the progression on the score
      "chip:prog"
        | unsavedNow s -> handleAction (SaveProg false)
        | otherwise -> when (length s.path > 0) (handleAction (SetStage (Hunt Score)))
      "chip:clear" -> guardLoss "clear" (handleAction ClearPath)
      _ -> pure unit
    pure (Just next)
  LoadSceneAt i next -> do
    s <- H.get
    for_ (s.perfScenes !! i) \item -> handleAction (PerfLoadScene item.payload)
    pure (Just next)
  SaveSceneQ next -> do
    handleAction SaveScene
    pure (Just next)
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
    H.modify_ _ { soundChosen = true }
    liftEffect (Store.saveSound (soundValue sel))
    setSound sel
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
        s { key = key, lattice = latticeFor key, bankPads = bankPadsFor key s.padRoll, chords = placed, nodes = simNodes
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
-- | Send auditions somewhere new, and find its port.
setSound :: forall m. MonadAff m => AuditionSel -> H.HalogenM State Action Slots Output m Unit
setSound sel = do
  H.modify_ _ { auditionSel = sel }
  case sel of
    AuditionOff -> H.modify_ _ { midiOut = Nothing, midiName = "muted" }
    AuditionBrowser -> H.modify_ _ { midiOut = Nothing, midiName = "browser" }
    _ -> connectMidi   -- re-pick the output port (continuo vs IAC) for the new mode

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
      AuditionBrowser -> HS.notify midiL (MidiReady Nothing "browser")
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
  refreshCands
  syncField
  syncAuditionCard
  after <- H.gets _.perfBoxes
  -- the cards changed (any edit, by hand or from the stage): publish what differs
  unless (unsafeRefEq before after) publishCards
  publishKey

-- | The audition card as the progression and the style say it should be:
-- | one chord a bar, arpeggiated if the style is.
auditionBoxFor :: State -> PerfBox -> PerfBox
auditionBoxFor st box =
  let chords = map playNotes (pathSteps st)
  in box
       { seq = if length chords == 0 then Nothing else Just (mkSavedSeq chords)
       , seqText = "<" <> joinWith " " (map show (range 0 (length chords - 1))) <> ">"
       , stack = case st.style of
           StyleBlock -> []
           StyleArp -> [ mkLayer (Arpg ArpUp 8) ]
       }

-- | Keep the audition card current while it is Vetula's: rewritten whenever
-- | the progression or the style changes, until it is taken over in Limulus.
syncAuditionCard :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
syncAuditionCard = do
  st <- H.get
  for_ st.auditionCard \ac -> unless ac.owned do
    for_ (find (\b -> b.cardId == ac.cardId) st.perfBoxes) \box -> do
      let want = auditionBoxFor st box
          sameSeq = map (map _.notes <<< _.events) want.seq == map (map _.notes <<< _.events) box.seq
      unless (sameSeq && want.seqText == box.seqText && map _.fx want.stack == map _.fx box.stack) $
        H.modify_ \s -> s { perfBoxes = map (\b -> if b.cardId == ac.cardId then want else b) s.perfBoxes }

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
    -- a card naming a progression is the composer's (Limulus): never
    -- printed back, which would write its chords in
    -- and any card keeps the line it has on the stage unless the page has
    -- changed what it means: a line typed in Limulus is never respelled
    let named = st.namedCards
        lineOf b = case Map.lookup b.cardId seen of
          Just old | readCard st b.cardId old == Just (boxSpec b) -> old
          _ -> printCard (boxSpec b)
        own = Map.fromFoldable (map (\b -> Tuple b.cardId (lineOf b)) st.perfBoxes)
        now = Map.union named (Map.filterKeys (not <<< flip Map.member named) own)
    liftEffect $ for_ (SC.publishLines seen now) (Transport.send (Binnacle.socket bin))
    H.modify_ _ { stageCards = Just now }

-- | Card `n`'s line, a progression it names looked up in the library.
readCard :: State -> Int -> String -> Maybe VoiceSpec
readCard st n text = parseCardIn lookup n text
  where
  lookup name = (\e -> { chords: filter (\ns -> length ns > 0) (parseProgression e.source), beats: parseBeats e.source })
    <$> find (\e -> e.kept && e.name == name) st.library

-- | Bring the stage's copy of the saved progressions up to date (step 4b):
-- | each kept one as `vetula/progression/<name>`, which a card names
-- | (`v1 $ vetula "bolt-tractor-horse"`) and the rig resolves. Internal: the
-- | user sees progressions and voices, never this copy
-- | (docs/kb/plans/vetula-visibility-audit.md).
publishProgressions :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
publishProgressions = do
  st <- H.get
  for_ st.stageProgs \seen -> for_ st.binnacle \bin -> do
    let now = Map.fromFoldable (mapMaybe stageText st.library)
    -- the rig's copy follows the library exactly, a deleted progression going
    -- too (its voices fall silent); a library with nothing kept is a browser
    -- that has not saved here yet, not one that deleted everything
    unless (Map.isEmpty now) do
      liftEffect $ for_ (SC.progressionLines seen now) (Transport.send (Binnacle.socket bin))
      H.modify_ _ { stageProgs = Just now }
  where
  stageText e =
    if e.kept && SC.stageName e.name
    then Just (Tuple e.name (printProgressionIn (parseBeats e.source) (filter (\ns -> length ns > 0) (parseProgression e.source))))
    else Nothing

-- | Kept progressions whose names a voice line cannot say (a space, the old
-- | prime) renamed once to ones it can (`cleanName`), with the renames made.
-- | Safe because no voice can have named them.
renamePrimes :: Array Store.Entry -> { library :: Array Store.Entry, renames :: Array (Tuple String String) }
renamePrimes lib = foldl step { library: lib, renames: [] } (range 0 (length lib - 1))
  where
  step acc i = case index acc.library i of
    Just e | e.kept && not (SC.stageName e.name) ->
      let nm0 = cleanName e.name
          nm = if any (\o -> o.name == nm0) acc.library then siblingName acc.library nm0 else nm0
      in { library: fromMaybe acc.library (modifyAt i (_ { name = nm }) acc.library), renames: snoc acc.renames (Tuple e.name nm) }
    _ -> acc

-- | A name a voice line can say: spaces become hyphens, primes the sibling
-- | number (skull-tornado′ → skull-tornado-2), anything else unsayable goes.
cleanName :: String -> String
cleanName nm = (if base == "" then "progression" else base) <> (if primes > 0 then "-" <> show (primes + 1) else "")
  where
  cs = SCU.toCharArray (SCU.takeWhile (_ /= '′') nm)
  primes = length (filter (_ == '′') (SCU.toCharArray nm))
  base = SCU.fromCharArray (mapMaybe keep cs)
  keep c
    | c == ' ' = Just '-'
    | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-' || c == '.' = Just c
    | otherwise = Nothing

-- | A sibling's name: the progression's stem and the next free number
-- | (skull-tornado-x → skull-tornado-x-2), typeable in a card where the old
-- | prime (′) was not.
siblingName :: Array Store.Entry -> String -> String
siblingName lib base = fromMaybe base (find free (map (\k -> stem <> "-" <> show k) (range 2 999)))
  where
  parts = split (Pattern "-") base
  stem = case last parts >>= fromString of
    Just _ -> joinWith "-" (take (length parts - 1) parts)
    Nothing -> base
  free nm = not (any (\e -> e.name == nm) lib)

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
    H.modify_ _ { stageCards = Nothing, stageProgs = Nothing, stageKey = Nothing }
    -- The rig answered, so this is not the zero-install page: unless the
    -- viewer chose, auditions go to Continuo (which falls back to the
    -- browser if no port answers).
    st0 <- H.get
    when (not st0.soundChosen && st0.auditionSel == AuditionBrowser) (setSound AuditionContinuo)
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
      when (any RL.looping st2.capture.logbook.marks && not (any RL.looping st.capture.logbook.marks) && not (showsRiver st2.stage))
        (handleAction (SetStage (Hunt River)))
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
                      }
  StageFrameIn msg -> do
    for_ (SC.tableHasKey msg) \has -> when has do
      st <- H.get
      H.modify_ _ { stageKey = Just (Route.printKey (contextKey st)) }
    -- the stage's saved progressions: it gets any of ours it lacks
    for_ (SC.progressionTable msg) \held -> do
      H.modify_ _ { stageProgs = Just held }
      publishProgressions
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
            (\spec -> Tuple n (boxOfCard n spec (find (\b -> b.cardId == n) st.perfBoxes))) <$> readCard st n text
        H.modify_ _ { perfBoxes = map snd readable, stageCards = Just table
                    , namedCards = Map.filter (isJust <<< cardProgression) table }
      Just (SC.Written n Nothing) ->
        H.modify_ \s -> s { perfBoxes = filter (\b -> b.cardId /= n) s.perfBoxes
                          , stageCards = map (Map.delete n) s.stageCards
                          , namedCards = Map.delete n s.namedCards }
      Just (SC.Written n (Just text)) -> do
        st <- H.get
        -- the audition card written as something other than what Vetula last
        -- wrote: it was edited in Limulus, so it is the composer's now
        for_ st.auditionCard \ac -> when (ac.cardId == n && not ac.owned) $
          for_ (find (\b -> b.cardId == n) st.perfBoxes) \box ->
            when (printCard (boxSpec box) /= text) $ H.modify_ _ { auditionCard = Just ac { owned = true } }
        H.modify_ _ { stageCards = map (Map.insert n text) st.stageCards }
        case readCard st n text of
          -- unreadable: refuse it; the publish that follows puts the card back
          Nothing -> do
            for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin)
              (SC.rejectLine n "Vetula could not read this card (want: chN \"<[c4,e4,g4] …>\" \"0 1 2 3\" # layer …)")
            publishCards
          Just spec -> H.modify_ \s -> s
            { namedCards = if isJust (cardProgression text) then Map.insert n text s.namedCards else Map.delete n s.namedCards
            , perfBoxes = case find (\b -> b.cardId == n) s.perfBoxes of
                Just old -> map (\b -> if b.cardId == n then boxOfCard n spec (Just old) else b) s.perfBoxes
                Nothing -> s.perfBoxes <> [ boxOfCard n spec Nothing ] }
  CardToLimulus n -> do
    publishCards
    st <- H.get
    for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) (SC.openLine n)
  Initialize -> do
    -- Explore's field: its marks call back into Halogen through this listener
    { emitter: fieldE, listener: fieldL } <- liftEffect HS.create
    _ <- H.subscribe fieldE
    fh <- liftEffect Field.new
    seen <- liftEffect (Ref.new Nothing)
    H.modify_ _ { field = Just { handle: fh, listener: fieldL, seen } }
    -- the saved scenes, for the browser drawer, in the background
    void $ H.fork (handleAction FetchScenes)
    -- Where this viewer chose to hear auditions, if they have.
    msound <- liftEffect Store.loadSound
    for_ (msound >>= soundFromValue) \sel -> H.modify_ _ { auditionSel = sel, soundChosen = true }
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
    -- old siblings named with a prime (skull-tornado′) take the numbered
    -- form once, so every kept progression can be named in a card
    let migrated = renamePrimes (maybe [] _.library msaved)
    for_ msaved \sv -> H.modify_ _ { library = migrated.library, capSeq = length sv.library, presets = sv.presets }
    when (length migrated.renames > 0) persistLib
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
            -- ⌘S / ctrl-S saves the progression (not the browser's page);
            -- any other modified key belongs to the browser or the shell
            if KE.metaKey ke || KE.ctrlKey ke then
              when (k == "s" || k == "S") do
                preventDefault ev
                HS.notify keyL (SaveProg (KE.shiftKey ke))
            -- shift-space is the shell's transport (Standalone's spaceHears)
            else when (not (KE.repeat ke) && not (k == " " && KE.shiftKey ke)) (HS.notify keyL (Key k (KE.shiftKey ke)))
          Nothing -> pure unit
      addEventListener (EventType "keydown") el false (Window.toEventTarget w)
    -- initial palette
    st <- H.get
    startWith st.key seedFocus (seedsFor st.key)
    -- The page opens neutral: the key rung, C major, nothing loaded (AC,
    -- 2026-10-08). The progression open when it last closed (the working
    -- copy) is kept, so a crash or a reload loses nothing, and the score
    -- offers it back.
    mwork <- liftEffect Store.loadWorking
    for_ mwork \w -> when (length (parseProgression w.source) > 0) $
      H.modify_ _ { resumable = Just (w { name = maybe w.name snd (find (\r -> fst r == w.name) migrated.renames) }) }

  ToggleScoreCands -> H.modify_ \s -> s { scoreCands = not s.scoreCands }

  CycleSound -> do
    s <- H.get
    let nxt = nextSound s.auditionSel
    H.modify_ _ { soundChosen = true }
    liftEffect (Store.saveSound (soundValue nxt))
    setSound nxt


  -- Reopen the working copy, numbered above every chord in the pool: a clash
  -- of ids with the seed triads made the path point at them instead (AC,
  -- 2026-10-06).
  ResumeWorking -> do
    st0 <- H.get
    for_ st0.resumable \w -> do
      let base = max st0.nextId (1 + fromMaybe 0 (maximum (map _.id st0.chords)))
          noteLists = filter (\ns -> length ns > 0) (parseProgression w.source)
          fresh = mapWithIndex (\j ns -> importChord (base + j) ns) noteLists
          ids = map _.id fresh
      when (length ids > 0) do
        let name = w.name
        H.modify_ _
          { chords = st0.chords <> fresh
          , imported = st0.imported <> Set.fromFoldable ids
          , nextId = base + length fresh
          , path = ids
          , rhythm = parseBeats w.source
          , progName = Just name
          , lastCapSig = w.source
          , lastPubSig = w.saved
          , resumable = Nothing
          }
        -- its key, as loading it from the library adopts it
        for_ (find (\e -> e.kept && e.name == name) st0.library >>= \e -> parseKeyLabel e.keyLabel) \k ->
          H.modify_ \s -> s { key = k, lattice = latticeFor k, bankPads = bankPadsFor k s.padRoll }
        showPathRung

  MidiReady mout nm ->
    H.modify_ _ { midiOut = mout, midiName = nm }

  -- The simulated positions were read only by the retired keyboard lens, so a
  -- tick writes nothing: a write redraws the whole page, and the simulation
  -- ticks every frame (2026-10-06: it held an idle page at 100% of a core).
  SimTick -> pure unit

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

  Key k shift -> do
    st <- H.get
    -- while the revoice modal is open the surface keys (stacking / explode / reset)
    -- stand down; only the within-chord controls stay live.
    -- a rhythm being tapped in owns space (next chord) and Esc (stop)
    if isJust st.tapping
      then case k of
        " " -> handleAction TapNext
        "Escape" -> handleAction TapStop
        _ -> pure unit
    else if isJust st.revoicing
      then case k of
        "Escape" -> H.modify_ _ { revoicing = Nothing }
        "v" -> H.modify_ _ { revoicing = Nothing }
        "Tab" -> cycleVoicing (if shift then -1 else 1)
        "ArrowUp" -> nudgeSelected 1
        "ArrowDown" -> nudgeSelected (-1)
        "ArrowLeft" -> handleAction (RevoiceStep (-1))
        "ArrowRight" -> handleAction (RevoiceStep 1)
        " " -> playHoveredOrSounding
        "f" -> toggleFavorite
        _ -> pure unit
      -- every view the same (docs: Vetula gestures, 2026-10-08): the keys
      -- act on what is under the pointer, else on the chord in hand
      else case k of
        " " -> hearHoveredOrHand
        "Enter" -> putHand
        "Escape" ->
          if isJust st.cursor then H.modify_ _ { cursor = Nothing }
          else when (isJust st.scoreSel) (handleAction ScoreUnselect)
        "Backspace" -> takeBack
        "Delete" -> guardLoss "clear" (handleAction ClearPath)
        "1" -> handleAction (SetStyle StyleBlock)
        "2" -> handleAction (SetStyle StyleArp)
        "Tab" -> cycleHand (if shift then -1 else 1)
        "ArrowUp" -> octaveHand 1
        "ArrowDown" -> octaveHand (-1)
        "f" -> for_ (handChord st) toggleFavoriteOf
        "v" -> handleAction OpenRevoice
        "p" -> H.gets _.path >>= playPath
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

  -- open the revoice modal on the hovered chord (else the sounding one), making
  -- it the active chord so Tab / arrows / drag / f all target it inside the modal.
  OpenRevoice -> do
    st <- H.get
    -- What to revoice, most direct first: the pad under the pointer (Banks),
    -- then the pool bubble under it, then whatever is sounding.
    let candidate = (st.hoveredBar >>= barChord st)
          <|> st.hoveredNode
          <|> (st.hoveredId >>= \hid -> find (\c -> c.id == hid) st.chords)
          <|> handChord st
          <|> (st.sounding >>= \sid -> find (\c -> c.id == sid) st.chords)
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
    H.modify_ \s -> s { drag = Just { chordId: cid, voiceIx: vix, startMidi: sm, offset: 0, horizontal: horiz, double: alt }
                     , sounding = Just cid
                     , revoicing = map (const cid) s.revoicing
                     , selected = Just (UpperVoice vix) }

  SelectVoice cid sel ->
    H.modify_ \s -> s { sounding = Just cid, selected = Just sel, revoicing = map (const cid) s.revoicing }

  -- A restored tone returns to its STACK POSITION, not to where it was, because
  -- omitting genuinely discards that (`omitTone`). The ghost is therefore drawn
  -- at the position it will return to.
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
      H.modify_ \s -> s { sounding = Just cid, revoicing = map (const cid) s.revoicing }
      playChord c'

  PickVoicing cid v -> do
    st <- H.get
    let chords' = map (\d -> if d.id == cid then d { voicing = v } else d) st.chords
    applyChords chords'
    H.modify_ \s -> s { sounding = Just cid, revoicing = map (const cid) s.revoicing }
    for_ (find (\d -> d.id == cid) chords') playChord

  PlayPath -> H.gets _.path >>= playPath

  -- ✕ clear: empty the progression and drop back to Hunt, so the next shift-click
  -- STARTS a fresh path instead of extending this one (the Nothing branch of
  -- PathPick then opens a new capture session). The visible twin of the `c` key —
  -- discoverable, and it works with a text field focused (where `c` is swallowed).
  ClearPath -> do
    st0 <- H.get
    -- no progression open: the lattice, where one starts
    when (st0.stage == Hunt Score) (handleAction (SetStage (Hunt st0.fieldLens)))
    H.modify_ _ { path = [], progName = Nothing, lastCapIdx = Nothing, lastCapSig = "", rhythm = [], tapping = Nothing }
    -- and the working copy with it, or a reload brings the cleared one back
    liftEffect (Store.saveWorking { name: "", source: "", saved: "" })

  ScoreHear notes -> auditionNotesNoLog notes
  ScoreRevoice cid -> H.modify_ _ { revoicing = Just cid, sounding = Just cid, selected = Nothing }

  -- **A rhythm, tapped in** (docs/kb/plans/vetula-progressions-not-tokens.md
  -- §5): the first chord sounds; each space moves to the next, and the time
  -- between presses is that chord's length, in beats at the clock's tempo
  -- (whole beats, at least one). Space after the last chord ends the take.
  -- Retaking is doing it again; the take is part of the progression, so
  -- saving keeps it and the voices naming it play it.
  TapStart -> do
    st <- H.get
    when (length st.path > 0) do
      now <- liftEffect perfNow
      for_ (nodeAtBar st 0) playChord
      H.modify_ _ { tapping = Just { at: 0, times: [ now ] }, scoreBar = Just 0, selected = Nothing }

  TapNext -> do
    st <- H.get
    for_ st.tapping \t -> do
      now <- liftEffect perfNow
      let times = snoc t.times now
          at = t.at + 1
      if at < length st.path then do
        for_ (nodeAtBar st at) playChord
        H.modify_ _ { tapping = Just { at, times }, scoreBar = Just at }
      else do
        let beatMs = 60000.0 / (if st.clockTempo > 0.0 then st.clockTempo else 120.0)
            beats = map (\ms -> max 1 (round (ms / beatMs))) (zipWith (-) (drop 1 times) times)
        H.modify_ _ { tapping = Nothing, rhythm = beats, scoreBar = Nothing
                    , publishMsg = Just ("rhythm: " <> joinWith " " (map show beats) <> " beats · save to keep it") }

  TapStop -> H.modify_ _ { tapping = Nothing, scoreBar = Nothing }

  ClearRhythm -> H.modify_ _ { rhythm = [] }

  -- **Repointing a voice** (docs/kb/plans/vetula-progressions-not-tokens.md
  -- §2): a progression dropped on a voice's badge, and the voice's line
  -- reads it, its sequence and manner kept. The line is the composer's, so
  -- it is written only by this gesture; Limulus follows it (its block is
  -- rewritten unless an edit is in hand there).
  VoiceDragOver ev -> do
    d <- liftEffect Drop.currentDrag
    when (isJust (stripPrefix (Pattern progDragPrefix) d)) (liftEffect (Drop.allowDrop ev))

  VoiceDrop n ev -> do
    name0 <- liftEffect (Drop.dropProgression ev)
    st <- H.get
    for_ (if name0 == "" then Nothing else Just name0) \name ->
      for_ (st.stageCards >>= Map.lookup n) \old ->
        case SC.repoint name old of
          Nothing -> H.modify_ _ { publishMsg = Just ("✗ voice " <> voiceLetter n <> "'s line names no progression to swap") }
          Just line -> do
            for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) ("stage-text " <> SC.cardKey n <> " " <> line)
            H.modify_ \s -> s
              { stageCards = map (Map.insert n line) s.stageCards
              , namedCards = Map.insert n line s.namedCards
              , publishMsg = Just ("voice " <> voiceLetter n <> " plays " <> name) }
            st2 <- H.get
            for_ (readCard st2 n line) \spec -> H.modify_ \s -> s
              { perfBoxes = map (\b -> if b.cardId == n then boxOfCard n spec (Just b) else b) s.perfBoxes }

  TitleDrag name ev -> liftEffect (Drop.startDrag ev (progDragPrefix <> name))

  -- **The progression's ladders.** The focus is the chord the single-chord
  -- tools act on (and Tab / arrows / f, through `sounding`).
  RevoiceFocus i -> do
    st <- H.get
    for_ (st.path !! i >>= \pid -> find (\c -> c.id == pid) st.chords) \c -> do
      playChord c
      H.modify_ _ { revoicing = Just c.id, sounding = Just c.id, selected = Nothing, scoreBar = Just i }

  RevoiceStep d -> do
    st <- H.get
    let n = length st.path
        at = revoiceAt st
        inPath = maybe false (\cid -> elem cid st.path) st.revoicing
    -- a chord revoiced on its own has no neighbours to step to
    when (n > 0 && inPath) $ handleAction (RevoiceFocus (clamp 0 (n - 1) (at + d)))

  -- Each later bar voice-led from the one before it, from the focus on: the
  -- least motion that keeps every voice. A bar with a different number of
  -- notes is left as it is (no voice has an obvious place to go), and the
  -- next leads on from it.
  RevoiceLead -> do
    st <- H.get
    let at = revoiceAt st
        nodeAt i = st.path !! i >>= \pid -> find (\c -> c.id == pid) st.chords
        step acc i = case acc.prev, nodeAt i of
          Just prev, Just cur
            | Set.member cur.id acc.done -> acc { prev = Just cur }
            | length cur.voicing == length prev.voicing ->
                let notes = voiceNear prev cur.bassPc (map (\m -> mod m 12) cur.voicing)
                    cur' = case Array.uncons notes of
                      Just { head: b, tail: ups } | length ups == length cur.voicing ->
                        cur { bassOct = (b - mod cur.bassPc 12) / 12, voicing = ups }
                      _ -> cur
                in { prev: Just cur', done: Set.insert cur.id acc.done, changed: Map.insert cur.id cur' acc.changed }
          _, cur -> acc { prev = cur }
        led = foldl step { prev: nodeAt at, done: Set.fromFoldable (map _.id (nodeAt at)), changed: Map.empty } (range (at + 1) (length st.path - 1))
    applyChords (map (\c -> fromMaybe c (Map.lookup c.id led.changed)) st.chords)
    H.gets _.path >>= playPath
  -- the first shift-click chooses one chord; the next on the same row
  -- stretches the run to it; one on the only chord chosen lets it go
  -- shift-click: this chord's scales. With ONE chord chosen on the row, a
  -- shift-click on another stretches it to a run; inside a run, or on the
  -- chord chosen alone, it lets go; outside a run, a fresh start
  ScoreSelect row i -> H.modify_ \st -> st { scoreSel = case st.scoreSel of
      Just sel | sel.row == row && sel.anchor == sel.to && sel.anchor == i -> Nothing
      Just sel | sel.row == row && sel.anchor == sel.to -> Just sel { to = i }
      Just sel | sel.row == row && i >= min sel.anchor sel.to && i <= max sel.anchor sel.to -> Nothing
      _ -> Just { row, anchor: i, to: i } }
  ScoreUnselect -> H.modify_ _ { scoreSel = Nothing }
  -- also the side panel's subject: variations and relatives follow the bar
  -- on the press: the chord sounds first, before any state changes (each
  -- redraws the page), and the press may become a drag
  ScoreStep i -> do
    st <- H.get
    let node = st.path !! i >>= \pid -> find (\c -> c.id == pid) st.chords
    for_ node playChord
    H.modify_ _ { sounding = map _.id node <|> st.sounding, selected = Nothing, cursor = Nothing
                , scoreBar = Just i, lastHeard = node <|> st.lastHeard }
  -- a copy with its own id, after it: revoicing or replacing one leaves the other
  ScoreDuplicate i -> do
    st <- H.get
    for_ (st.path !! i >>= \pid -> find (\c -> c.id == pid) st.chords) \c -> do
      let copy = c { id = st.nextId, isCentre = false, pinned = false }
      applyChords (st.chords <> [ copy ])
      H.modify_ \s -> s { nextId = st.nextId + 1, path = fromMaybe s.path (insertAt (i + 1) copy.id s.path), scoreBar = Just (i + 1)
                       , rhythm = inStep s (\r -> r !! i >>= \b -> insertAt (i + 1) b r) }
  ScoreDelete i -> H.modify_ \s -> s
    { path = fromMaybe s.path (deleteAt i s.path)
    , rhythm = inStep s (deleteAt i)
    , scoreBar = if s.scoreBar == Just i then Nothing else map (\b -> if b > i then b - 1 else b) s.scoreBar
    , scoreSel = Nothing }
  -- dropped on another bar: the dragged chord takes that place
  ScorePress i -> H.modify_ _ { scoreDrag = Just i }
  HoverBar mi -> do
    st <- H.get
    let c = mi >>= barChord st
    H.modify_ _ { hoveredBar = mi, hoveredNode = c, hoveredTriad = map (\d -> { root: d.root, pcs: d.pcs }) c }
  -- an alternative: heard where it would go, and in hand (return puts it)
  AltClick c -> do
    H.modify_ _ { cursor = Just { key: "alt", chord: c } }
    hearLoose c
  AltPut c -> do
    st <- H.get
    for_ st.scoreBar \i -> do
      handleAction (ScorePutAt i c)
      st' <- H.get
      for_ (st'.path !! i) playId
  AltDragEnd -> H.modify_ _ { scorePad = Nothing }
  ScoreDropAt j -> H.gets _.scoreDrag >>= case _ of
    -- released where it was pressed: a click, the bar heard and chosen
    Just i | i == j -> do
      H.modify_ _ { scoreDrag = Nothing }
      handleAction (ScoreStep j)
    _ -> H.modify_ \s -> case s.scoreDrag of
        Just i | i /= j ->
          let moved = do
                pid <- s.path !! i
                rest <- deleteAt i s.path
                insertAt j pid rest
              -- the length moves with its chord
              movedR = inStep s \r -> do
                b <- r !! i
                rest <- deleteAt i r
                insertAt j b rest
          in s { path = fromMaybe s.path moved, rhythm = movedR, scoreDrag = Nothing, scoreBar = Just j, scoreSel = Nothing }
        _ -> s { scoreDrag = Nothing }
  ScorePutAt i c -> do
    st <- H.get
    when (st.stage == Hunt Score && i < length st.path) do
      let old = st.path !! i >>= \pid -> find (\d -> d.id == pid) st.chords
          fitted = maybe c (\o -> fitRegister o c) old
          caught = place st.key fitted (fitted { id = st.nextId, isCentre = false, pinned = false })
      applyChords (st.chords <> [ caught ])
      H.modify_ \s -> s { nextId = st.nextId + 1, path = fromMaybe s.path (updateAt i caught.id s.path)
                        , sounding = Just caught.id, scoreBar = Just i, scorePad = Nothing }
  -- a pad heard where it would go: in its bar's register
  ScoreHearPad c -> do
    st <- H.get
    let old = st.scoreBar >>= \i -> st.path !! i >>= \pid -> find (\d -> d.id == pid) st.chords
    auditionNotesNoLog (playNotes (maybe c (\o -> fitRegister o c) old))
  ScorePadDrag c -> H.modify_ _ { scorePad = Just c }
  ScoreDropPad i -> do
    st <- H.get
    for_ st.scorePad \c -> do
      handleAction (ScorePutAt i c)
      st' <- H.get
      for_ (st'.path !! i) playId
  -- only a chord being carried from the alternatives drops on a bar
  ScoreDragOver ev -> whenM (isJust <$> H.gets _.scorePad) (liftEffect (preventDefault ev))
  ScoreReadIn row -> H.modify_ \st -> st { scoreRead = if Set.member row st.scoreRead then Set.delete row st.scoreRead else Set.insert row st.scoreRead }
  -- as loading a progression adopts its saved key: the key moves under the
  -- chords, which stay where they are (`rebuild` would transpose them)
  ScoreAdopt root mode -> H.modify_ \s ->
    let k = { tonic: root, mode }
    in s { key = k, lattice = latticeFor k, bankPads = bankPadsFor k s.padRoll, restScale = Nothing
         , scoreRead = Set.delete (fromMaybe "new progression" s.progName) s.scoreRead }
  CopyTidal src -> liftEffect (copyText src)

  ToggleHelp -> H.modify_ \s -> s { helpOpen = not s.helpOpen }

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

  -- Manual save = promote the current path to a KEEPER (frozen). If the current
  -- session's ephemeral is already in the library, promote it in place (+ rename);
  -- otherwise append a fresh keeper.
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
      -- the working copy: saved locally on every change, for a crash or a
      -- reload; keeping it is explicit (SaveProg)
      else when (sig /= st.lastCapSig) do
        H.modify_ _ { lastCapSig = sig }
        for_ st.progName \nm -> liftEffect (Store.saveWorking { name: nm, source: sig, saved: st.lastPubSig })

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
        , rhythm = parseBeats entry.source
        , progName = Just entry.name
        -- continue this entry's line of versions: edits update it in place
        -- (or fork a sibling if it is kept)
        , lastCapIdx = Just i
        , lastCapSig = entry.source
        , lastPubSig = entry.source
        , voices = canonicalVoices (length fresh)
        
        , sounding = head ids
        -- The loaded progression's key becomes the live harmonic context: clear any
        -- `# scale` override, then adopt the entry's saved key so the resting scale
        -- (and every following voice, incl. Odonus) tracks it. Without this the scale
        -- stayed on whatever was loaded before — the "dark pads stayed C minor" bug.
        , restScale = Nothing
        }
      for_ (parseKeyLabel entry.keyLabel) \k -> H.modify_ \s -> s { key = k, lattice = latticeFor k, bankPads = bankPadsFor k s.padRoll }
      showPathRung

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
        handleAction FetchScenes
      Left _ -> H.modify_ _ { publishMsg = Just "✗ save failed (store offline?)" }

  PerfCloseRecall -> H.modify_ _ { perfRecallOpen = false }

  -- The saved scenes, for the browser drawer, without opening the modal.
  FetchScenes -> do
    res <- liftAff (attempt (Amphora.fetchCollection "vetula-scene"))
    for_ res \items -> H.modify_ _ { perfScenes = items }

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
    if length boxes == 0
      then H.modify_ _ { perfRecallOpen = false, publishMsg = Just "✗ couldn't read that scene" }
      else H.modify_ _ { perfBoxes = boxes
                       , perfRecallOpen = false, publishMsg = Just "scene loaded" }

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

  -- Tank model (Slice A). Catch a lattice chord into the durable tank as a frozen
  -- Specimen: absolute-MIDI voicing + bass (bassPc grounded an octave below middle
  -- C, matching playNotes), a descriptive label + provenance. It references no
  -- lattice id, so the cloud can regenerate underneath without disturbing it.
  PlayChordId pid -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    else playId pid

  -- Tonnetz lens: a triad picked straight off the tonal net. Audition sounds it
  -- (no state change); catch freezes it into the tank as a Free-anchored Specimen,
  -- exactly like a palette drop.
  AuditionTriad root pcs -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    -- label it like CatchTriad (root name + minor mark) so the chyron reads it
    else playChord (triadNode root pcs (noteName root <> (if elem (mod (root + 4) 12) pcs then "" else "m")))

  -- **Hearing is not taking** (plan: "From exploring to progressions"). A click
  -- on the field selects and plays, quietly; return (or a shift-click) takes.
  SelectMark k c -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    else do
      H.modify_ _ { cursor = Just { key: k, chord: c }, lastHeard = Just c }
      playChordQuiet c

  -- **Save** (AC, 2026-10-06: "explicit save and unsaved current version"):
  -- a version of the progression under its frozen name, in the library
  -- (updated in place, one entry per progression) and published to Amphora,
  -- which keeps every version as the label re-points, and put on the stage,
  -- where cards naming it play it (step 4b). ⌘⇧S saves as a new sibling
  -- (skull-tornado-x-2) instead.
  SaveProg fork -> do
    st <- H.get
    when (length st.path > 0) do
      let sig = currentSource st
          kl = groupLabel st.key
          base = fromMaybe kl st.progName
          nm = if fork then siblingName st.library base else base
          entry = { name: nm, keyLabel: kl, source: sig, kept: true }
          lib = case findIndex (\e -> e.name == nm) st.library of
            Just i -> fromMaybe st.library (updateAt i entry st.library)
            Nothing -> st.library <> [ entry ]
      H.modify_ _ { library = lib, progName = Just nm, lastPubSig = sig, lastCapIdx = findIndex (\e -> e.name == nm) lib
                  , publishMsg = Just ("saved " <> nm) }
      persistLib
      liftEffect (Store.saveWorking { name: nm, source: sig, saved: sig })
      void $ H.fork do
        res <- liftAff $ attempt $ Amphora.publish
          { kind: "vetula-progression", collection: "vetula-progression"
          , name: nm, source: "user", payload: sig, tags: [ "key:" <> kl, "kept" ] }
        H.modify_ _ { publishMsg = Just case res of
          Right _ -> "saved " <> nm
          Left _ -> "saved " <> nm <> " here (no store)" }
      publishProgressions

  SetStyle sty -> do
    H.modify_ _ { style = sty }
    st <- H.get
    for_ (maybe (map _.chord st.cursor) Just st.hoveredNode) playChordQuiet

  TakeMark k c -> do
    st <- H.get
    if st.panMoved then H.modify_ _ { panMoved = false }
    else do
      H.modify_ _ { cursor = Just { key: k, chord: c } }
      playChordQuiet c
      takeChord c

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
      -- bar-aligned, as the rig and Odonus make it: the bar before and the
      -- bar the mark falls in
      mclock <- vetulaClock
      let atMic = nowMs * 1000.0
          beat = maybe 0.0 _.beat mclock
          tempo = maybe st.clockTempo _.tempo mclock
          rb = Logbook.regionBounds tempo atMic beat
          mark = { atMicros: atMic, beat, from: rb.from, to: rb.to, patch: markText st, now: "", sounding: Nothing, rig: [], tempo, n: 0, id: RL.nextId st.capture.logbook.marks, loop: Nothing }
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

  -- a band's edge resizes its window, its body slides it (past a small
  -- threshold, so a click still loops it); released, the edges snap to beats
  -- and the rig is told (Odonus's gesture, Grid.RegionDown)
  CaptureRegionDown i edge cx cy -> do
    sid <- H.subscribe $ HS.makeEmitter \emit -> do
      moveFn <- eventListener \e -> case ME.fromEvent e of
        Just me -> emit (CaptureRegionMove (ME.clientX me) (ME.clientY me))
        Nothing -> pure unit
      upFn <- eventListener \_ -> emit CaptureRegionUp
      target <- Window.toEventTarget <$> window
      addEventListener (EventType "mousemove") moveFn false target
      addEventListener (EventType "mouseup") upFn false target
      pure do
        removeEventListener (EventType "mousemove") moveFn false target
        removeEventListener (EventType "mouseup") upFn false target
    grab <- capturePointer cx cy
    st <- H.get
    for_ (st.capture.logbook.marks !! i) \m ->
      H.modify_ \s -> s { captureDragSub = Just sid
                       , capture = s.capture { regionDrag = Just { markIdx: i, edge, grabMicros: grab, startFrom: m.from, startTo: m.to, moved: false } } }
  CaptureRegionMove cx cy -> do
    st <- H.get
    for_ st.capture.regionDrag \rd -> do
      cur <- capturePointer cx cy
      let ax = CaptureView.bounds st.capture.zoom st.capture.logbook
          d = ax.toFrac cur - ax.toFrac rd.grabMicros
          past = max d (negate d) > 0.005
      when (rd.moved || rd.edge /= EdgeBody || past) do
        let minLen = 60.0e6 / max 30.0 st.clockTempo
            slid = ax.fromFrac (ax.toFrac rd.startFrom + ax.toFrac cur - ax.toFrac rd.grabMicros)
            bounds = case rd.edge of
              EdgeFrom -> { from: min (rd.startTo - minLen) cur, to: rd.startTo }
              EdgeTo -> { from: rd.startFrom, to: max (rd.startFrom + minLen) cur }
              EdgeBody -> Runs.offSeams ax { from: slid, to: slid + (rd.startTo - rd.startFrom) }
        H.modify_ \s -> s { capture = Logbook.applyBounds rd.markIdx bounds (s.capture { regionDrag = map (_ { moved = true }) s.capture.regionDrag }) }
  CaptureRegionUp -> do
    st <- H.get
    for_ st.captureDragSub H.unsubscribe
    H.modify_ \s -> s { captureDragSub = Nothing, capture = s.capture { regionDrag = Nothing } }
    for_ st.capture.regionDrag \rd -> case rd.edge, rd.moved of
      -- a band whose card was put away only brings the card back
      EdgeBody, false
        | map _.id (st.capture.logbook.marks !! rd.markIdx) == st.capture.cardShut ->
            H.modify_ \s -> s { capture = s.capture { cardShut = Nothing } }
        | otherwise -> handleAction (CaptureRegionSelect rd.markIdx)
      _, _ -> for_ (st.capture.logbook.marks !! rd.markIdx) \m -> do
        let snapped = { from: Logbook.snapMicrosToBeat st.clockTempo m m.from
                      , to: Logbook.snapMicrosToBeat st.clockTempo m m.to }
        H.modify_ \s -> s { capture = Logbook.applyBounds rd.markIdx snapped s.capture }
        -- the rig's window is the one that plays
        when st.rigLoops do
          mclock <- vetulaClock
          for_ mclock \clock -> rigSend (RL.windowLine "vetula" clock (m { from = snapped.from, to = snapped.to }))
  CaptureDismissCard mid -> H.modify_ \s -> s { capture = s.capture { cardShut = Just mid } }
  CaptureDeleteMark i -> do
    st <- H.get
    if st.rigLoops then for_ (st.capture.logbook.marks !! i) \m -> rigSend (RL.deleteLine "vetula" m.n)
    else H.modify_ \s -> s { capture = s.capture { playing = Nothing, logbook = let lb = Logbook.deleteMark i s.capture.logbook in lb { marks = RL.renumber lb.marks } } }
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
  CaptureFrame -> do
    st <- H.get
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
    when (showsRiver st.stage && st.rigLoops && any RL.looping st.capture.logbook.marks) do
      mclock <- vetulaClock
      H.modify_ \s -> s { capture = s.capture { rig = mclock } }
    -- the live river moves only while it is on screen
    when (showsRiver st.stage) do
      nowMs <- liftEffect perfNow
      H.modify_ _ { riverNow = nowMs * 1000.0 }
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

  Nop -> pure unit

  -- Run an inner-control action but stop the click bubbling to the box's
  -- placement onClick — otherwise nudging/removing a layer while something is in
  -- hand would also drop that held item onto the box.
  PerfStopClick ev act -> do
    liftEffect $ stopPropagation (ME.toEvent ev)
    handleAction act

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

  ShufflePads -> H.modify_ \s -> s { padRoll = s.padRoll + 1, bankPads = bankPadsFor s.key (s.padRoll + 1) }

  ShuffleVary -> H.modify_ \s -> s { varyRoll = s.varyRoll + 1 }

  -- **Declare a progression to the sampler.** One path, never a lattice: you
  -- can only name what you sampled if you know which reading played. A saved
  -- progression is one plain path (alternation is a voice's business), so
  -- there is nothing to refuse. Named by the progression; its glyph is the
  -- chord rebus, which Quadrat mints the same way from the same chords.
  ToQuadrat name -> do
    st <- H.get
    case find (\e -> e.kept && e.name == name) st.library of
      Nothing -> H.modify_ _ { publishMsg = Just ("✗ " <> name <> " is not saved") }
      Just entry -> do
            now <- liftEffect dateNow
            let chords = filter (\ns -> length ns > 0) (parseProgression entry.source)
                glyph = TGlyph.chordGlyph chords
                clip = clipOfChords st name glyph.alias chords now
                spec = Share.shareSpec clip
                         { kind: "chord-hits", glyph: glyph.alias }
            H.modify_ _ { publishMsg = Just "sending to Quadrat…" }
            res <- liftAff (attempt (Amphora.publish spec))
            case res of
              Right hash -> H.modify_ _
                { publishMsg = Just ("✓ for Quadrat · " <> SCU.take 8 hash) }
              Left _ -> H.modify_ _ { publishMsg = Just "✗ send failed (store offline?)" }

  SetSideDensity i -> H.modify_ _ { sideDensity = i }

  -- Semantic zoom: wheel in for more chords, out for fewer, a level a flick.
  LevelWheel ev dy -> do
    liftEffect (preventDefault ev)
    st <- H.get
    t <- liftEffect (unwrap <<< unInstant <$> now)
    let acc = (if t - st.wheelLast > 300.0 then 0.0 else st.wheelAcc) + dy
        cur = huntOr st.lastLens st.stage
        ladder = rungs
        ix = fromMaybe (-1) (elemIndex cur ladder)
        step d = for_ (if ix < 0 then Nothing else index ladder (ix + d)) \v -> do
          H.modify_ _ { wheelAcc = 0.0, wheelAt = t, wheelLast = t }
          handleAction (SetStage (Hunt v))
    -- after a step, swallow the rest of the flick (a trackpad's inertia)
    if t - st.wheelAt < 450.0 then H.modify_ _ { wheelAcc = 0.0, wheelLast = t }
    else if acc <= -60.0 then step 1
    else if acc >= 60.0 then step (-1)
    else H.modify_ _ { wheelAcc = acc, wheelLast = t }

  -- The one mode switch. Absorbed the old `SetCaptureView`, so leaving REVIEW by
  -- ANY route — Perform, or off to Hunt — hushes the region preview and drops the
  -- lift card. Under the old split you could escape a looping preview sideways
  -- into Browse and it would keep ringing.
  SetStage v -> do
    when (not (showsRiver v)) hushCapture
    H.raise (StageChanged (stagePath v))
    H.modify_ \st -> st
      { stage = v
      , cursor = if (v == Hunt Score) /= (st.stage == Hunt Score) then Nothing else st.cursor
      , lastLens = huntOr st.lastLens v
      , fieldLens = case st.stage of
          Hunt vt | not (fullView (Hunt vt)) -> vt
          _ -> st.fieldLens
      , lastRung = case v of
          Hunt vt | elem vt rungs -> vt
          _ -> st.lastRung
      , hoveredId = Nothing, hoveredTriad = Nothing
      , viewCx = 0.0, viewCy = 0.0, viewZoom = 1.0, panning = Nothing, panMoved = false
      , capture = if showsRiver v then st.capture else st.capture { playing = Nothing, contextOpen = false }
      -- An excursion is a round trip between two places. Navigating anywhere
      -- ELSE ends it, or a later free visit to the Vary lens would quietly post
      -- its keeps into a slot you had stopped thinking about.
      }

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
          -- pane), but pass no MIDI-out so nothing sounds locally — the rig's voices
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
      -- ⌥-click, no drag: omit the note
      Just dg | dg.double -> do
        H.modify_ _ { drag = Nothing }
        for_ (find (\c -> c.id == dg.chordId) st.chords >>= \c -> toneAt c dg.startMidi) \tn ->
          omitTone dg.chordId tn.ix tn.oct
      -- a plain click (select only): leave the voicing alone
      _ -> H.modify_ _ { drag = Nothing }

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

-- | A voice's timeline: one segment per NON-skipped chord, in chord order, each at
-- | its cumulative pulse offset. 1 bar = 16 pulses (16th notes). Skipped chords
-- | (0 bars) contribute nothing, so a voice plays only the chords it dwells on.
timeline :: Array Int -> Array { ix :: Int, start :: Int, len :: Int }
timeline ds = snd (foldl step (Tuple 0 []) (mapWithIndex Tuple ds))
  where
  step (Tuple off segs) (Tuple i d) =
    if d <= 0 then Tuple off segs
    else Tuple (off + d * 16) (segs <> [ { ix: i, start: off, len: d * 16 } ])

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
        , progName = Nothing
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
  -- a kept name a voice cannot say arrives as the one it was renamed to
  { name: if elem "kept" it.tags && not (SC.stageName it.name) then cleanName it.name else it.name
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
          nm0 = fromMaybe (kl <> " ◦" <> show n) st.progName
          -- a kept entry of the same name is frozen: editing it forks a
          -- sibling (skull-tornado → skull-tornado-2), keeping both
          nm = if any (\e -> e.name == nm0) st.library then siblingName st.library nm0 else nm0
      H.modify_ _ { library = st.library <> [ { name: nm, keyLabel: kl, source: sig, kept: false } ]
                  , lastCapIdx = Just (length st.library), capSeq = n, lastCapSig = sig
                  , progName = Just nm }
  persistLib

-- | Note-off every voice's currently-held notes.
silenceHeld :: forall o m. MonadAff m => State -> H.HalogenM State Action Slots o m Unit
silenceHeld st =
  liftEffect $ for_ st.midiOut \out ->
    for_ st.voices \v -> for_ v.held \nn -> Midi.noteOffAt out { channel: Routing.toWire (midiChannelFor st.routing v), note: nn, delayMs: 0.0 }

-- | The loaded performance progression's chords, resolved from the working copy.
-- | The performed progression. Slice 4a: this IS the live `path` (`pathSteps`) — the
-- | voices read what you're building, with no load-a-copy step. Kept as a named alias
-- | because the reef-projection sites (`buildPerf`) read more clearly as
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

-- | Schedule notes on the preview channel WITHOUT logging to the chyron — for
-- | re-auditioning a chip already in the trace (no feedback loop).
auditionNotesNoLog :: forall o m. MonadAff m => Array Int -> H.HalogenM State Action Slots o m Unit
auditionNotesNoLog notes = auditionStyled notes

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


-- one integer token, lenient: strips a leading '+' (which `fromString` rejects),
-- falls back to `def` on anything non-numeric.
tokInt :: Int -> String -> Int
tokInt def s = fromMaybe def (fromString (fromMaybe s (stripPrefix (Pattern "+") s)))

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
      -- an index past the end wraps: a progression is a stream of itself
      Right idxPat -> Just (map (\s -> fromMaybe [] (fromString (trim s) >>= RV.wrapAt chords)) idxPat)

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

-- | A progression as a clip for Quadrat: one chord a beat (`chordMicros`),
-- | named, its id from its rebus and the time.
clipOfChords :: State -> String -> String -> Array (Array Int) -> Number -> MidiClip
clipOfChords st name alias chords now =
  -- SECONDS, not millis. `round` targets a 32-bit Int and epoch millis
  -- (1.79e12) saturate it at 2147483647 — so every clip minted from the same
  -- rebus got the identical id, which is the one field that must not collide.
  -- Seconds (1.79e9) fit until 2038, and Amphora content-addresses anyway, so a
  -- genuine duplicate dedupes on its hash rather than on this.
  { id: "vetula-" <> alias <> "-" <> show (round (now / 1000.0))
  , events: concat (mapWithIndex evs chords)
  , lenMicros: toNumber (length chords) * chordMicros
  , heads: 1
  , capturedMicros: now
  , source: "vetula"
  , name
  , tags: [ "progression" ]
  , notes: ""
  , bpm: Nothing
  , key: Just (noteName (mod st.key.tonic 12) <> " " <> show st.key.mode)
  , context: Nothing
  }
  where
  evs i notes =
    map (\n -> { pitch: n, headIdx: 0, fireUnixMicros: toNumber i * chordMicros
                , vel: 92, gateMs: 700.0 })
      notes

-- | One chord a beat, in micros. The same 700 ms the pass auditions at, so what
-- | a sampler is told matches what you heard when you chose it.
chordMicros :: Number
chordMicros = 700000.0

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
playChordQuiet c = auditionStyled (playNotes c)

-- | **Every audition note goes through here**: to the MIDI port the shell chose,
-- | or to the browser's own voice when that is the choice or no port answered.
auditionNote :: State -> Voice.Note -> Effect Unit
auditionNote st n = case st.auditionSel, st.midiOut of
  AuditionOff, _ -> pure unit
  AuditionBrowser, _ -> Voice.play n
  _, Just out -> Midi.scheduleNote out { channel: st.previewChan, note: n.note, velocity: n.velocity, delayMs: n.delayMs, durMs: n.durMs }
  _, Nothing -> Voice.play n

-- | A chord's notes as the current style plays them. An arpeggio climbs, and
-- | each note rings on to the chord's end, as under a held pedal.
styledNotes :: AuditionStyle -> Array Int -> Array Voice.Note
styledNotes sty notes = case sty of
  StyleBlock -> map (\n -> { note: n, velocity: 92, delayMs: 0.0, durMs: 900.0 }) notes
  StyleArp ->
    let up = sort notes
        step = 150.0
        len = toNumber (length up)
    in mapWithIndex (\i n -> { note: n, velocity: 88, delayMs: toNumber i * step, durMs: 900.0 + (len - toNumber i) * step }) up

-- | Sound a chord's notes in the current style.
auditionStyled :: forall o m. MonadAff m => Array Int -> H.HalogenM State Action Slots o m Unit
auditionStyled notes = do
  st <- H.get
  liftEffect $ for_ (styledNotes st.style notes) (auditionNote st)

-- | **Take a chord into the progression** (`path`): the step after hearing it.
-- | The chord joins the pool once; taking it again reuses it, so a slot refers
-- | to its chord rather than copying it, and revoicing that chord later changes
-- | every place it is used (plan: "Names that survive edits"). AutoCapture banks
-- | the progression from here as before.
takeChord :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
takeChord c = do
  st0 <- H.get
  -- the first chord of a new progression: a fresh frozen name, and a fresh
  -- capture entry, so it never overwrites the last progression's
  when (length st0.path == 0 || not (isJust st0.progName)) do
    seed <- liftEffect (randomInt 0 999999)
    H.modify_ _
      { progName = Just (sessionAliasOf seed)
      , lastCapIdx = if length st0.path == 0 then Nothing else st0.lastCapIdx
      , lastCapSig = if length st0.path == 0 then "" else st0.lastCapSig
      , lastPubSig = if length st0.path == 0 then "" else st0.lastPubSig }
  st <- H.get
  let same d = mod d.root 12 == mod c.root 12 && pcSetOf d == pcSetOf c && playNotes d == playNotes c
  case find same st.chords of
    Just d -> H.modify_ _
      { path = st.path <> [ d.id ], lastHeard = Just c
      , chords = map (\e -> if e.id == d.id then e { label = chordNameOf e } else e) st.chords }
    Nothing -> do
      -- named in full (a lattice chord's label is only its root), so the
      -- chyron, Rehearse and the Tidal header read "Am", not "A"
      let fresh = c { id = st.nextId, label = chordNameOf c }
      H.modify_ _
        { chords = st.chords <> [ fresh ]
        , imported = Set.insert fresh.id st.imported
        , nextId = st.nextId + 1
        , path = st.path <> [ fresh.id ]
        , lastHeard = Just c
        }

playChord :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
playChord c = do
  let notes = playNotes c
  auditionStyled notes
  -- Whatever it came from (the pool, a pad, a colour set), the side panel's
  -- "last chord played" is this one.
  H.modify_ _ { lastHeard = Just c }

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
-- | Notes sound on the channels they were captured on: a Vetula note's `headIdx`
-- | IS its voice's 1-based channel (unlike an Odonus head, 0..3), so it is sent
-- | as it is. Through `st.midiOut`, so an ⌥1 AuditionOff silences a preview too.
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
          { channel: Routing.toWire e.headIdx
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
      for_ (nub (map _.headIdx ps.events)) \ch ->
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

-- | Arpeggiate a path: each chord in turn, lightly rolled, ~440ms apart — the
-- | segment heard as a phrase (the consonant, directional walk AC noticed).
playPath :: forall o m. MonadAff m => Array Int -> H.HalogenM State Action Slots o m Unit
playPath ids = do
  st <- H.get
  let chordsOnPath = mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) ids
      stepMs = 440.0
      rollMs = 22.0
  liftEffect $
    for_ (mapWithIndex Tuple chordsOnPath) \(Tuple i c) ->
      for_ (mapWithIndex Tuple (playNotes c)) \(Tuple j n) ->
        auditionNote st
          { note: n, velocity: 88
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
cycleVoicing dir = H.gets _.sounding >>= \ms -> for_ ms \sid -> cycleVoicingOf sid dir

-- | Tab through a pool chord's candidate voicings, in place: every bar that
-- | uses it changes with it.
cycleVoicingOf :: forall o m. MonadAff m => Int -> Int -> H.HalogenM State Action Slots o m Unit
cycleVoicingOf sid dir = do
  st <- H.get
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
  case st.hoveredNode of
      -- Banks: a pad carries its own open voicing, so play it VERBATIM. This has
      -- to come before the triad branch below, which would re-voice it close.
      -- Quiet in the Vary grid: space there is browsing, same as a click.
      Just c -> if inVaryGrid st then playChordQuiet c else playChord c
      Nothing -> case st.hoveredTriad of
        -- Tonnetz: a hovered triangle has no pool id, so preview it straight from its
        -- root + pitch classes (no state change, like the candidate preview below).
        Just t -> playChord (triadNode t.root t.pcs "")
        Nothing -> case st.hoveredId of
          -- in pick mode the hovered bubble is a candidate (not yet in `chords`);
          -- preview it without committing (no sounding change, no insert)
          Just hid | Just cand <- find (\c -> c.id == hid) st.candidates -> playChord cand
          Just hid -> playId hid
          Nothing -> for_ st.sounding \sid -> for_ (find (\c -> c.id == sid) st.chords) playChord

-- | **The chord in hand** (docs: Vetula gestures, 2026-10-08): the one last
-- | clicked, on any view, which the keys act on. A bar of the progression
-- | (a pool chord, changed in place), or a loose chord: one clicked on the
-- | lattice, the banks or the alternatives, which return puts.
data Hand = InBar Int ChordNode | Loose ChordNode

hand :: State -> Maybe Hand
hand st = case st.cursor of
  Just cur -> Just (Loose cur.chord)
  Nothing
    | st.stage == Hunt Score -> st.scoreBar >>= \i -> InBar i <$> barChord st i
    | otherwise -> Nothing

handChord :: State -> Maybe ChordNode
handChord st = hand st <#> case _ of
  InBar _ c -> c
  Loose c -> c

-- | The chord in a bar of the progression.
barChord :: State -> Int -> Maybe ChordNode
barChord st i = st.path !! i >>= \pid -> find (\c -> c.id == pid) st.chords

-- | A loose chord heard: on the score with a bar chosen, where it would sit
-- | in that bar; elsewhere as it is.
hearLoose :: forall m. MonadAff m => ChordNode -> H.HalogenM State Action Slots Output m Unit
hearLoose c = do
  st <- H.get
  if puts st then handleAction (ScoreHearPad c) else playChordQuiet c

-- | Space: what is under the pointer, else the chord in hand. Heard only:
-- | nothing it does changes what the alternatives are for.
hearHoveredOrHand :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
hearHoveredOrHand = do
  st <- H.get
  case st.hoveredBar >>= barChord st of
    Just c -> playChordQuiet c
    Nothing -> case st.hoveredNode of
      Just c -> hearLoose c
      -- the parked fifths and Tonnetz views
      Nothing -> case st.hoveredTriad, st.hoveredId >>= \hid -> find (\c -> c.id == hid) st.chords of
        Just t, _ -> playChordQuiet (triadNode t.root t.pcs "")
        _, Just c -> playChordQuiet c
        _, _ -> for_ (hand st) case _ of
          InBar _ c -> playChordQuiet c
          Loose c -> hearLoose c

-- | Return: put a loose chord in hand into the progression: at its end on
-- | the lattice and the banks, into the chosen bar on the score.
putHand :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
putHand = do
  st <- H.get
  for_ st.cursor \cur ->
    if st.stage == Hunt Score then handleAction (AltPut cur.chord) else takeChord cur.chord

-- | Backspace: take back the progression's last chord.
takeBack :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
takeBack = H.modify_ \s ->
  let n = length s.path
  in if n == 0 then s
     else s { path = fromMaybe s.path (deleteAt (n - 1) s.path)
            , rhythm = inStep s (deleteAt (n - 1))
            , scoreBar = s.scoreBar >>= \b -> if b < n - 1 then Just b else if n > 1 then Just (n - 2) else Nothing }

-- | The cycle position of a loose chord's voicings (it has no pool id).
looseCycle :: Int
looseCycle = -1

-- | Tab: the next voicing of the chord in hand.
cycleHand :: forall m. MonadAff m => Int -> H.HalogenM State Action Slots Output m Unit
cycleHand dir = do
  st <- H.get
  for_ (hand st) case _ of
    InBar _ c -> cycleVoicingOf c.id dir
    Loose c -> do
      let cyc = case st.cycle of
            Just vc | vc.chordId == looseCycle && index vc.options vc.ix == Just c.voicing -> vc
            _ -> { chordId: looseCycle, options: voicingCandidates c, ix: 0 }
          n = length cyc.options
      when (n > 0) do
        let ix' = mod (cyc.ix + dir + n) n
            c' = c { voicing = fromMaybe c.voicing (index cyc.options ix') }
        H.modify_ \s -> s { cycle = Just (cyc { ix = ix' }), cursor = map (_ { chord = c' }) s.cursor }
        hearLoose c'

-- | ↑ ↓: the chord in hand an octave up or down.
octaveHand :: forall m. MonadAff m => Int -> H.HalogenM State Action Slots Output m Unit
octaveHand d = do
  st <- H.get
  for_ (hand st) case _ of
    InBar _ c -> do
      let c' = octaveShift d c
      applyChords (map (\e -> if e.id == c.id then c' else e) st.chords)
      playChord c'
    Loose c -> do
      let c' = octaveShift d c
      H.modify_ \s -> s { cursor = map (_ { chord = c' }) s.cursor }
      hearLoose c'

-- | Whether the open progression has changes not saved (as its chip says).
unsavedNow :: State -> Boolean
unsavedNow st = length st.path > 0 && currentSource st /= st.lastPubSig

-- | **Losing unsaved work asks once, without a dialog** (AC, 2026-10-08): the
-- | first press only marks the progression's chip; the same act again within
-- | a moment goes through.
guardLoss :: forall m. MonadAff m => String -> H.HalogenM State Action Slots Output m Unit -> H.HalogenM State Action Slots Output m Unit
guardLoss what act = do
  st <- H.get
  if not (unsavedNow st) then act
  else do
    at <- liftEffect dateNow
    case st.lossArmed of
      Just a | a.what == what && at - a.at < lossWindowMs -> do
        H.modify_ _ { lossArmed = Nothing }
        act
      _ -> do
        H.modify_ _ { lossArmed = Just { what, at } }
        H.raise (StageChanged (stagePath st.stage))
        void $ H.fork do
          liftAff (delay (Milliseconds lossWindowMs))
          H.modify_ \s -> if map _.at s.lossArmed == Just at then s { lossArmed = Nothing } else s
          s' <- H.get
          H.raise (StageChanged (stagePath s'.stage))

lossWindowMs :: Number
lossWindowMs = 2500.0

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
-- | A mode as a word in a sentence: "phrygian dominant", "major", "minor".
modeWord :: Mode -> String
modeWord m = case m of
  Ionian -> "major"
  Aeolian -> "minor"
  _ -> toLower (trim (SCU.takeWhile (_ /= '(') (maybe "" _.label (find (\c -> c.mode == m) modeChoices))))

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
  for_ (st.sounding >>= \sid -> find (\c -> c.id == sid) st.chords) toggleFavoriteOf

-- | Star / unstar a chord's voicing among its note-set's favourites.
toggleFavoriteOf :: forall o m. MonadAff m => ChordNode -> H.HalogenM State Action Slots o m Unit
toggleFavoriteOf c = H.modify_ \st ->
  let key = pcsKey c
      cur = fromMaybe [] (Map.lookup key st.favorites)
      next = if elem c.voicing cur then filter (_ /= c.voicing) cur else cur <> [ c.voicing ]
  in st { favorites = if length next == 0 then Map.delete key st.favorites else Map.insert key next st.favorites }

-- | A chord's favourites key — its note-set, so favoured voicings follow the
-- | actual notes (and survive key changes) rather than a transient node id.
pcsKey :: ChordNode -> String
pcsKey c = show (sort (nub c.pcs))

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
    , dropped = Map.empty
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
    -- right of the browser drawer (Triggerfish.Standalone's --tf-left)
    [ HP.style ("position: relative; margin-top: calc(var(--tf-bar) + " <> contextBarHeight st <> "); margin-left: var(--tf-left, 0px); width: calc(100% - var(--tf-left, 0px) - var(--tf-right, 0px)); height: calc(100vh - var(--tf-bar) - " <> contextBarHeight st <> "); min-height: 620px; overflow: hidden; border-radius: 8px; background: " <> canvasBg <> ";") ]
    -- In HUNT the views start below the audition strip, which is fixed over
    -- the stage's top; Rehearse, Perform and Review pad for it themselves.
    -- Keyed by what the surface is, so a different surface gets a fresh
    -- element: the field draws its own <svg> into its container, which
    -- Halogen does not know about, and a reused container kept the old
    -- lattice under the score
    ( [ HH.keyed (ElemName "div") [ HP.style ("position: absolute; inset: 0px " <> sideInset st <> " " <> "0px" <> " 0;") ] [ Tuple (surfaceKey st) (surface st) ] ]
      <> latticeControls st
      <> previewControls st
      -- Limulus's drawer, out while the score is up
      <> [ if st.stage == Hunt Score then limulusWanted else HH.text "" ]
      <>
    -- Scene recall belongs to the INSTRUMENT, not to Perform. Its entry point
    -- has always been the session menu in `contextBar`, which renders on every
    -- stage — but the modal itself was inside `performSurface`, so anywhere
    -- else the click set the flag and nothing appeared. Loading a scene is legal
    -- wherever you can use one, which is certainly Rehearse and reasonably Hunt.
    [ perfRecallModal st
    -- The Tank & Progression card is retired (Tank overhaul §10.6): the chyron is
    -- now the single surface for collect · select · reorder · bundle/unbundle, so
    -- the tank tiles, the tonnetz stack, arrange/grow, and the built-progression
    -- panel (with its ▶ preview) are all superseded. The underlying code —
    -- specimens, `Vetula.Between` (the cadence bridge), ArrangeSpec/SequenceSpec —
    -- is kept dormant in the source for re-homing onto the chyron later.
    -- The AUDITION bar (chyron) now docks under the shell nav (top). The old bottom
    -- voice bar (four mini-notation lanes) was removed — the Perform surface
    -- supersedes it — and the freed bottom is reserved for a future MIDI-flow chyron.
    , HH.div
        [ HP.style "position: absolute; bottom: 44px; left: 50%; transform: translateX(-50%); z-index: 5;" ]
        [ pickBar st ]
    , helpOverlay st
    , revoiceModal st
    ] )


sideInset :: State -> String
sideInset _ = "0px"

-- | The lattice's rungs, fewest chords first: what its slider steps through
-- | (and the wheel over it).
rungs :: Array Viewtype
rungs = [ KeyChords, Common, Lattice4, Lattice ]

-- | **The lattice pane's own controls** (AC, 2026-10-08: a control lives in
-- | the pane it controls): a lattice | banks switch; on the lattice, a slider
-- | through its rungs, which moves with the wheel and sets it when clicked or
-- | dragged, the top the most chords; on banks, its shuffle; and a way back
-- | to the fitted view once it has moved.
latticeControls :: forall m. State -> Array (H.ComponentHTML Action Slots m)
latticeControls st = case st.stage of
  Hunt v | elem v rungs || v == Pads ->
    (if v == Pads then [] else [ slider v ])
      <> [ HH.div [ HP.style ("position: absolute; right: 16px; bottom: 12px; z-index: 6; display: flex; gap: 6px;") ]
             ( (if v == Pads then [ chip "shuffle \x27f3" "re-walk all nine banks from a new seed" ShufflePads ] else [])
                 <> (if st.viewZoom /= 1.0 || st.viewCx /= 0.0 || st.viewCy /= 0.0
                       then [ chip "reset view" "back to the fitted view \x00b7 scroll to change the rung \x00b7 drag to pan" ResetView ] else []) ) ]
  _ -> []
  where
  chip label tip act =
    HH.button
      [ HP.style "border: 1px solid #cdbb8c; background: #fbf6ea; color: #7a5c00; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px; white-space: nowrap;"
      , HP.title tip, HE.onClick \_ -> act ]
      [ HH.text label ]
  -- the most chords at the top, as the wheel goes up
  slider v =
    HH.div
      [ HP.style "position: absolute; top: 60px; right: 16px; z-index: 6; display: flex; flex-direction: column; align-items: flex-end; gap: 0; user-select: none; -webkit-user-select: none;"
      , HP.title "how much of the lattice: scroll over it, or click or drag here" ]
      (mapWithIndex (stop v) (Array.reverse rungs))
  stop v i r =
    let on = r == v
        last = i == length rungs - 1
    in HH.div
         [ HP.style "display: flex; align-items: center; gap: 8px; cursor: pointer; height: 34px;"
         , HP.title (viewtypeTip r)
         , HE.onMouseDown \_ -> SetStage (Hunt r)
         , HE.onMouseEnter \e -> if ME.buttons e > 0 then SetStage (Hunt r) else Nop ]
         [ HH.span [ HP.style ("font-size: 11px; " <> (if on then "color: #3a3428; font-weight: 600;" else "color: #a09880;")) ] [ HH.text (viewtypeLabel r) ]
         , HH.div [ HP.style "position: relative; width: 14px; height: 34px; display: flex; align-items: center; justify-content: center;" ]
             ( [ HH.div [ HP.style ("position: absolute; left: 6px; width: 2px; background: #d8cfb6; top: " <> (if i == 0 then "17px" else "0") <> "; bottom: " <> (if last then "17px" else "0") <> ";") ] []
               , HH.div [ HP.style ("position: relative; width: " <> (if on then "12px" else "8px") <> "; height: " <> (if on then "12px" else "8px") <> "; border-radius: 50%; "
                                     <> (if on then "background: #8d7a4a;" else "background: #fbf8f0; border: 1px solid #b8ab84;")) ] [] ] )
         ]

-- | **Limulus, out while the score is up** (AC, 2026-10-08): not docked in
-- | a column of the page but the shell's own right-hand drawer, opened on
-- | arriving and withdrawable like the library's (its rail, or `). A marker
-- | the shell watches (`Standalone.watchDocks`, "open"): it places nothing.
limulusWanted :: forall m. H.ComponentHTML Action Slots m
limulusWanted = HH.div [ HP.attr (HH.AttrName "data-limulus-dock") "open", HP.style "display: none;" ] []

-- | **The workspace's views** (AC, 2026-10-08): river, lattice, banks and
-- | score are four views of one workspace, the drawers around it (library,
-- | Limulus, the score's candidates) supporting it. One switch, always at the
-- | left of the workspace's bar, whichever view is up.
viewTabs :: forall m. State -> H.ComponentHTML Action Slots m
viewTabs st =
  barGroup
    [ barBtn (elem v rungs) "lattice" "the chords built on the key, from triads up" (SetStage (Hunt st.lastRung))
    , barBtn (v == Pads) "banks" "nine banks of sixteen: how far from home, by how rich" (SetStage (Hunt Pads))
    , barBtn (v == Score) "score" "the progression on a grand staff, to arrange and play" (SetStage (Hunt Score))
    , barBtn (v == River) "review" "everything played this session, as a river: mark the good bits, loop them" (SetStage (Hunt River)) ]
  where
  v = huntOr st.lastLens st.stage

-- | **How a preview sounds**, whichever view it comes from: struck or
-- | rolled, and where it sounds.
soundControls :: forall m. State -> Array (H.ComponentHTML Action Slots m)
soundControls st =
  [ barGroup
      [ barBtn (st.style == StyleBlock) "block" "previews strike the chord (key 1)" (SetStyle StyleBlock)
      , barBtn (st.style == StyleArp) "arpeggio" "previews roll the chord (key 2)" (SetStyle StyleArp) ]
  , barGroup
      [ barBtn (st.auditionSel /= AuditionOff) ("\x266a " <> soundValue st.auditionSel)
          ("where previews sound (" <> st.midiName <> ") \x00b7 click: browser \x2192 continuo \x2192 MIDI \x2192 off") CycleSound ]
  ]

-- | **The workspace's own controls** (AC, 2026-10-08: the second bar went):
-- | how a preview sounds, top right, on every view that previews; and the
-- | focused family's scale, top left, while a family is focused.
previewControls :: forall m. MonadAff m => State -> Array (H.ComponentHTML Action Slots m)
previewControls st = case st.stage of
  Hunt River -> []
  _ ->
    -- on the score, inside the open row's header line, which is empty on the right
    [ HH.div [ HP.style ("position: absolute; z-index: 7; display: flex; align-items: center; gap: 8px; "
                          <> (if st.stage == Hunt Score then "top: 20px; right: 36px;" else "top: 14px; right: 16px;")) ] (soundControls st) ]
      <> case st.focusedFamily >>= (\sid -> find (\c -> c.id == sid) st.chords) of
        Just seed | st.stage /= Hunt Score ->
          let famMode = (fromMaybe st.key (Map.lookup seed.id st.familyScale)).mode
          in [ HH.div [ HP.style "position: absolute; top: 14px; left: 16px; z-index: 7; display: flex; align-items: center; gap: 6px;" ]
                 [ HH.span [ HP.style "font-size: 10px; color: #9a9a9a; letter-spacing: 0.1em; text-transform: uppercase;" ] [ HH.text ("family " <> noteName seed.root) ]
                 , HH.slot (Proxy :: _ "familyScaleSelect") unit Select.component
                     ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue famMode), searchable = true })
                     \(Select.Selected v) -> ReflavourFamily v ] ]
        _ -> []

barGroup :: forall m. Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
barGroup = HH.div [ HP.style "display: flex; border: 1px solid #d8cfb6; border-radius: 5px; overflow: hidden; background: #fbf8f0; box-shadow: 0 1px 2px #0000000d;" ]

barBtn :: forall m. Boolean -> String -> String -> Action -> H.ComponentHTML Action Slots m
barBtn on label tip act =
  HH.button
    [ HP.style ("border: none; padding: 4px 12px; font-size: 12px; cursor: pointer; white-space: nowrap; "
                 <> (if on then "background: #8d7a4a; color: #fff;" else "background: #fbf8f0; color: #5a5240;"))
    , HP.title tip, HE.onClick \_ -> act ]
    [ HH.text label ]

sideLabel :: SideTab -> String
sideLabel = case _ of
  SideVariations -> "variations"
  SideRelatives -> "relatives"
  SideSubstitutes -> "substitutes"

sideTip :: SideTab -> String
sideTip = case _ of
  SideVariations -> "the chord last played, varied: revoiced, thinned, or swapped for a substitute"
  SideRelatives -> "the chords that lead well from the one last played, smoothest first"
  SideSubstitutes -> "chords that could stand in for the one last played: its tritone substitute, the same root, three or two notes in common"

-- | **Score mode's three columns**, under the score (AC's sketch,
-- | 2026-10-08): substitutes, variations and relatives of the chord in hand
-- | (the bar last clicked), side by side so candidates compare at a glance.
-- | A click hears one in the bar's register, a shift-click puts it in the
-- | bar, or drag it onto any bar.
scoreCandidates :: forall m. State -> H.ComponentHTML Action Slots m
scoreCandidates st =
  HH.div
    [ HP.style ("flex: " <> (if st.scoreCands then "1 1 0" else "0 0 auto") <> "; min-height: 0; display: flex; flex-direction: column; border-top: 1px solid #e6dfcc; background: #fffdf8;") ]
    ( [ HH.div [ HP.style "display: flex; align-items: baseline; gap: 10px; padding: 8px 16px 6px; font-size: 11px; color: #a09880; border-bottom: 1px solid #f0eadb; cursor: pointer;"
               , HP.title (if st.scoreCands then "tuck the candidates down" else "bring the candidates up")
               , HE.onClick \_ -> ToggleScoreCands ]
        ( ( case varySource st of
            Just src ->
              [ HH.span [ HP.style "font-size: 14px; color: #3a3428; font-weight: 500;" ] [ HH.text (chordTitle st src) ]
              , HH.text (case st.scoreBar of
                  Just i | puts st -> "bar " <> show (i + 1) <> " \x00b7 click hears it in its register \x00b7 shift-click puts it there \x00b7 or drag it onto any bar"
                  _ -> "click a bar to see its candidates") ]
            Nothing -> [ HH.text "click a bar of the score: its substitutes, variations and relatives show here" ] )
            <> [ HH.span [ HP.style "flex: 1 1 auto;" ] []
               , HH.span [ HP.style "font-size: 12px; color: #8d7a4a;" ]
                   [ HH.text (if st.scoreCands then "substitutes \x00b7 variations \x00b7 relatives \x25be" else "substitutes \x00b7 variations \x00b7 relatives \x25b4") ] ] ) ]
      <> (if not st.scoreCands then [] else
      [ HH.div [ HP.style "flex: 1 1 auto; min-height: 0; display: grid; grid-template-columns: repeat(3, minmax(0, 1fr));" ]
          (map column [ SideSubstitutes, SideVariations, SideRelatives ]) ])
    )
  where
  column t =
    HH.div [ HP.style "min-height: 0; overflow: auto; padding: 8px 12px 16px; border-right: 1px solid #f0eadb;" ]
      ( [ HH.div [ HP.style "font-size: 11px; letter-spacing: 0.08em; text-transform: uppercase; color: #3a3428; margin-bottom: 6px;", HP.title (sideTip t) ]
            [ HH.text (sideLabel t) ] ]
          <> maybe [] (sideContent st t) (varySource st) )

-- | A chord's name as the score spells it (a chord loaded from a saved
-- | progression is labelled by its root alone).
chordTitle :: State -> ChordNode -> String
chordTitle st c = Score.spellName (Score.spellingOf st.key.tonic (scaleSet st.key)) (OP.chordName (playNotes c))

-- | **What the side panel shows for a chord**, by tab: its substitutes,
-- | variations or relatives. The side panel shows one beside the lattice; in
-- | score mode the three stand side by side under the score.
-- | **The candidates for a chord**: its substitutes (voiced near it), its
-- | variations and its relatives. Expensive (the variations alone are
-- | hundreds of trial voicings), so computed once per chord, key and roll,
-- | and kept: rendering them on every frame made a click on the score take
-- | three seconds (2026-10-08).
type Cands =
  { sig :: String
  , subs :: Array (Tuple HSub.Substitute ChordNode)
  , cells :: Array Vary.Cell
  , smooth :: Array ChordNode
  , middle :: Array ChordNode
  , far :: Array ChordNode
  }

candsSig :: State -> ChordNode -> String
candsSig st src = show (playNotes src) <> "|" <> show st.key.tonic <> show (scaleSet st.key) <> "|" <> show st.varyRoll <> "|" <> show st.genRoll <> "|" <> show st.sideDensity

computeCands :: State -> ChordNode -> Cands
computeCands st src =
  { sig: candsSig st src, subs, smooth, middle, far
  -- the density on show only (the panel shows one)
  , cells: Vary.gridIn (maybe HV.densities pure (index HV.densities st.sideDensity)) st.key src st.varyRoll }
  where
  -- **Substitutes** (Harmonia.Substitute), voiced near the chord they would
  -- replace: the bass nearest its bass, the rest voice-led from its notes
  subs =
    let found = HSub.substitutes 6 (scaleSet st.key) src.root (map (\m -> mod m 12) (playNotes src))
        node i sub = (importChord (-5000 - i) (voiceNear src sub.root sub.pcs)) { label = Score.spellName (Score.spellingOf st.key.tonic (scaleSet st.key)) (noteName sub.root <> sub.suffix) }
    in mapWithIndex (\i sub -> Tuple sub (node i sub)) found
  -- Three rows from the same generator at three settings of its adventure
  -- dial; a chord shows once, in the smoothest row that holds it.
  seed = src { label = chordTag src }
  row adv = take 8 (drop (mod (st.genRoll * 2) 5) (filter (not <<< same src) (generateCandidates Append [ seed ] st.key adv 0)))
  smooth = row 0.0
  middle = filter (\c -> not (any (same c) smooth)) (row 0.5)
  far = filter (\c -> not (any (same c) (smooth <> middle))) (row 1.0)
  same a b = pcSetOf a == pcSetOf b

-- | Bring the kept candidates up to date with the chord in hand, only while
-- | the score shows them.
refreshCands :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
refreshCands = do
  st <- H.get
  let showing = st.stage == Hunt Score
  case varySource st of
    Just src | showing ->
      unless (map _.sig st.cands == Just (candsSig st src)) $
        H.modify_ _ { cands = Just (computeCands st src) }
    _ -> pure unit

sideContent :: forall m. State -> SideTab -> ChordNode -> Array (H.ComponentHTML Action Slots m)
sideContent st t src = case st.cands of
  Just cs | cs.sig == candsSig st src -> case t of
    SideVariations -> variations cs
    SideRelatives -> relatives cs
    SideSubstitutes -> substitutesOf cs
  _ -> []
  where
  smallBtn label tip act =
    HH.button
      [ HP.style "border: 1px solid #cdbb8c; background: #fbf6ea; color: #7a5c00; cursor: pointer; padding: 2px 8px; border-radius: 4px; font-size: 11px; white-space: nowrap;"
      , HP.title tip, HE.onClick \_ -> act ]
      [ HH.text label ]
  substitutesOf cs =
    let
      nodes = cs.subs
      section reason heading blurb =
        let cs' = map snd (filter (\(Tuple sub _) -> sub.reason == reason) nodes)
        in if length cs' == 0 then [] else
          [ HH.div [ HP.style "font-size: 10px; letter-spacing: 0.1em; text-transform: uppercase; color: #9a8d6a; margin: 10px 0 2px;" ] [ HH.text heading ]
          , HH.div [ HP.style "font-size: 11px; color: #a09880; margin-bottom: 4px;" ] [ HH.text blurb ]
          , HH.div [ HP.style "display: grid; grid-template-columns: repeat(4, 1fr); gap: 4px;" ] (map (padButton st) cs') ]
    in section HSub.Tritone "tritone substitute" "the dominant a tritone away: same third and seventh, swapped"
         <> section HSub.SameRoot "same root" "another quality on this root"
         <> section (HSub.Shares 3) "three notes in common" "the closest stand-ins"
         <> section (HSub.Shares 2) "two notes in common" "further, but still sharing"

  -- One density column at a time: a panel has room for one, and the three
  -- drift rows (held, thinned, swapped) are the question it answers.
  variations cs =
    let cells = cs.cells
        dn = fromMaybe HV.densities (map pure (index HV.densities st.sideDensity))
        cellAt d = filter (\x -> x.drift == d && elem x.density dn) cells
    in [ HH.div [ HP.style "display: flex; align-items: center; gap: 4px; margin-bottom: 10px;" ]
           ( mapWithIndex densityBtn HV.densities
               <> [ HH.div [ HP.style "flex: 1 1 auto;" ] []
                  , smallBtn "shuffle \x27f3" "re-draw the cells from a new seed" ShuffleVary ] )
       ]
         <> concatMap (\d -> [ HH.div [ HP.style "margin: 6px 0 4px;" ] [ varyRowHead d ], varyBank st (cellAt d) ]) HV.drifts
  densityBtn i d =
    let on = i == st.sideDensity
    in HH.button
         [ HP.style ("border: 1px solid " <> (if on then "#8d7a4a" else "#e0d8c2") <> "; background: " <> (if on then "#f2e7c6" else "#fbf8f0") <> "; color: #5a5240; border-radius: 4px; padding: 2px 9px; font-size: 11px; cursor: pointer;")
         , HP.title (HV.densityBlurb d), HE.onClick \_ -> SetSideDensity i ]
         [ HH.text (HV.densityLabel d) ]
  -- Three rows from the same generator at three settings of its adventure
  -- dial; a chord shows once, in the smoothest row that holds it.
  relatives cs =
    let group lbl tip cs' =
          if length cs' == 0 then []
          else [ HH.div [ HP.style "font-size: 11px; color: #7a7360; letter-spacing: 0.08em; text-transform: uppercase; margin: 8px 0 4px;", HP.title tip ] [ HH.text lbl ]
               , HH.div [ HP.style "display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 4px; background: #fbf8f0; border: 1px solid #ece5d2; border-radius: 5px; padding: 6px;" ]
                   (map (padButton st) cs') ]
    in [ HH.div [ HP.style "display: flex; margin-bottom: 4px;" ]
           [ HH.div [ HP.style "flex: 1 1 auto; font-size: 11px; color: #a09880;" ] [ HH.text "what could come next, voice-led from it" ]
           , smallBtn "shake \x27f3" "a different crop of the candidates" ShakeGenerate ] ]
         <> group "smooth" "the nearest moves: most notes held or moved by a step" cs.smooth
         <> group "further" "a little more adventurous" cs.middle
         <> group "striking" "the most adventurous of the plausible" cs.far

-- | Vetula's own bar (`contextBar`) is HUNT's controls alone since its stage
-- | tabs and mark went to the shell's bar (2026-10-05), so it takes room only
-- | in HUNT.
contextBarHeight :: State -> String
contextBarHeight _ = "0px"

-- | **The gestures, one meaning each on every view** (docs: Vetula gestures,
-- | 2026-10-08). The help is this table, so it says what the code does.
helpRows :: Array { gesture :: String, meaning :: String }
helpRows =
  [ { gesture: "click", meaning: "hear a chord, and take it in hand: the keys below act on it. On the score, a bar clicked is the bar chosen; the alternatives under it are for that bar" }
  , { gesture: "shift-click", meaning: "put it: on the lattice and banks at the end of the progression; from the alternatives, into the chosen bar. On the score's bars, shift-click chooses a run, for the scales that fit them" }
  , { gesture: "hover", meaning: "light the chords related to it" }
  , { gesture: "hover + space", meaning: "hear it. With nothing under the pointer, the chord in hand. On the score an alternative is heard where it would sit in the chosen bar" }
  , { gesture: "drag", meaning: "a bar to another place; an alternative onto a bar. On the lattice, drag the view" }
  , { gesture: "return", meaning: "put the chord in hand" }
  , { gesture: "tab · shift-tab", meaning: "the next or previous voicing of the chord in hand" }
  , { gesture: "↑ ↓", meaning: "the chord in hand an octave up or down" }
  , { gesture: "v", meaning: "revoice the chord under the pointer, else the one in hand. In the revoice view: drag a note by octaves, ⌥-drag to double it, ⌥-click to omit it, click its ghost to bring it back; ← → step through the bars, esc closes" }
  , { gesture: "f", meaning: "keep this voicing among the chord's favourites" }
  , { gesture: "p", meaning: "play the progression" }
  , { gesture: "backspace", meaning: "take back the progression's last chord" }
  , { gesture: "delete", meaning: "clear the progression (unsaved: press again to confirm)" }
  , { gesture: "esc", meaning: "close the innermost thing: tapping, the revoice view, the chord in hand" }
  , { gesture: "1 · 2", meaning: "previews struck (block) or rolled (arpeggio)" }
  , { gesture: "⌘S · ⌘⇧S", meaning: "save the progression; save it as a new sibling" }
  , { gesture: "shift-space", meaning: "the transport: play or stop" }
  , { gesture: "b · `", meaning: "the library; Limulus" }
  ]

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
            [ HH.h2 [ HP.style "font-size: 15px; font-weight: 600; margin: 0; color: #2a2a2a;" ] [ HH.text "Gestures and keys" ]
            , HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ] [ HH.text "click anywhere to close" ]
            ]
        , HH.div [ HP.style "display: grid; grid-template-columns: max-content 1fr; gap: 7px 18px; font-size: 12.5px; line-height: 1.5;" ]
            (concatMap row helpRows)
        ]
    ]
  where
  row r =
    [ HH.div [ HP.style "font-family: 'SF Mono', Menlo, monospace; font-size: 11.5px; color: #7a5c00; white-space: nowrap;" ] [ HH.text r.gesture ]
    , HH.div [ HP.style "color: #555;" ] [ HH.text r.meaning ] ]

-- | A chord (by root and pitch classes) voiced near another: its root as the
-- | bass, nearest the other's bass; the rest voice-led from the other's
-- | upper notes (Harmonia.Voicing.voiceLead), or placed nearest their middle
-- | when the counts differ.
voiceNear :: ChordNode -> Int -> Array Int -> Array Int
voiceNear src root pcs =
  let
    oldBass = bassMidi src
    bass = nearestOf oldBass root
    uppers = src.voicing
    mid = if length uppers == 0 then oldBass + 12 else sum uppers / length uppers
    placed =
      if length uppers == length pcs
        then voicingMidi (HVL.voiceLead (Voicing uppers) (HC.Chord pcs))
        else map (nearestOf mid) pcs
    lifted = map (\m -> if m <= bass then m + 12 else m) placed
  in [ bass ] <> sort (nub lifted)
  where
  nearestOf target pc =
    let base = target - mod (target - pc) 12
    in if target - base > 6 then base + 12 else base

-- | A chord moved by octaves to sit where another sat (their middles
-- | nearest): a substitute takes its bar's register.
fitRegister :: ChordNode -> ChordNode -> ChordNode
fitRegister old c =
  let mean ns = if length ns == 0 then 60.0 else toNumber (sum ns) / toNumber (length ns)
      k = round ((mean (playNotes old) - mean (playNotes c)) / 12.0)
  in octaveShift k c

-- | The lowest lattice rung that shows every chord of the progression: the
-- | key's own, + common, up to four notes, or the whole lattice.
rungForPath :: State -> Viewtype
rungForPath st =
  let
    keyChords = map (\c -> Tuple (mod c.root 12) (pcSetOf c)) (diatonicTriads st.key <> diatonicSevenths st.key)
    levelOf c
      | elem (Tuple (mod c.root 12) (pcSetOf c)) keyChords = 0
      | commonChord st.key c = 1
      | length (pcSetOf c) <= 4 = 2
      | otherwise = 3
    nodes = mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) st.path
    lvl = fromMaybe 0 (maximum (map levelOf nodes))
  in fromMaybe KeyChords (index [ KeyChords, Common, Lattice4, Lattice ] lvl)

-- | Raise the field to the rung the progression needs, if it is on a lower
-- | one (never lower it: a richer view the composer chose stays).
showPathRung :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
showPathRung = do
  st <- H.get
  let need = rungForPath st
      rank v = fromMaybe (-1) (elemIndex v [ KeyChords, Common, Lattice4, Lattice ])
  case st.stage of
    Hunt v | rank v >= 0 && rank v < rank need ->
      H.modify_ _ { stage = Hunt need, lastLens = need, fieldLens = need }
    _ -> pure unit

-- | Whether the side panel's chords go into the score: it is showing, with a
-- | bar of the open progression chosen.
puts :: State -> Boolean
puts st = st.stage == Hunt Score && maybe false (_ < length st.path) st.scoreBar

-- | Which surface is showing, as a key: every field rung shares one, so the
-- | field's own drawing survives a rung change.
surfaceKey :: State -> String
surfaceKey st
  | length st.genSel > 0 && length st.candidates > 0 = "pick"
  | otherwise = case st.stage of
      Hunt Score -> "score"
      Hunt River -> "river"
      Hunt Fifths -> "fifths"
      Hunt Tonnetz -> "tonnetz"
      Hunt _ -> "field"

-- | The Stage frame: the pick-mode cloud always wins; otherwise the active View
-- | renders. `Perform` is its own surface; every `Browse` viewtype is one branch.
surface :: forall m. State -> H.ComponentHTML Action Slots m
surface st
  | length st.genSel > 0 && length st.candidates > 0 = pickSurface st
  | otherwise = case st.stage of
      Hunt Fifths -> circleFifthsSurface st
      Hunt Tonnetz -> tonnetzSurface st
      Hunt KeyChords -> fieldSurface st
      Hunt Common -> fieldSurface st
      Hunt Lattice4 -> fieldSurface st
      Hunt Lattice -> fieldSurface st
      Hunt Pads -> fieldSurface st
      Hunt Score -> scoreSurface st
      Hunt River -> riverSurface st

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
          , HE.onClick \e -> PerfStopClick e Nop ]
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
      , HP.title "load this scene: its voices onto the stage"
      , HE.onClick \_ -> PerfLoadScene item.payload ]
      [ HH.text item.name ]

-- The LIVE/REPLAY switch that used to float here (absolute, top-right of the
-- surface) is gone: it was the mode control for a mode the type didn't admit,
-- with nowhere in the chrome to stand. It's now the PERFORM/REVIEW stage tabs in
-- `stageTabs`. The roll's own header went the same way — ◆ mark, the counts and
-- clear are STAGE CONTROLS, so they live in the nav beside the tabs, in the same
-- slot Odonus puts them (`captureControls`).

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
    , regionDown: CaptureRegionDown
    , stopPlay: CaptureStopSel
    , saveClip: CaptureSaveClip
    , saveScene: Nothing
    , deleteMark: Just CaptureDeleteMark
    , dismissCard: CaptureDismissCard
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

-- | **The river as a main-space view** (docs/kb/plans/vetula-one-surface.md,
-- | step 2): Review's whole-session roll, beside the lattice and the score
-- | rather than a mode of its own. Full height: no chyron over it here.
riverSurface :: forall m. State -> H.ComponentHTML Action Slots m
riverSurface st =
  HH.div
    -- `.vetula-surface`: the key listener acts while one is on screen
    [ HP.class_ (HH.ClassName "vetula-surface vetula-surface--wide")
    , HP.style "position: absolute; inset: 0; display: flex; flex-direction: column; background: #0b0a07;" ]
    -- the live river on top, flowing in from the right, where Limulus's
    -- drawer is (AC: "as if emitting from the Limulus drawer, which it is");
    -- the whole session below, where marks are looped and their windows set
    [ HH.div [ HP.style "flex: 0 0 38%; position: relative; border-bottom: 1px solid #e8c14a33;" ]
        [ riverPanel { flow: FlowLeft, headColor: captureHeadColor }
            { nowMicros: st.riverNow, notes: recentNotes st, marks: st.capture.logbook.marks }
        , riverTools st ]
    , capturePane st ]

-- | The notes the live river shows: the last few seconds of the record.
recentNotes :: State -> Array NoteEvent
recentNotes st =
  let lb = st.capture.logbook
      cutoff = st.riverNow - River.windowMicros
  in filter (\n -> n.fireUnixMicros > cutoff) (lb.live <> maybe [] _.events (head lb.chunks))

-- | **The river's own tools**, on the live river (AC, 2026-10-08: marking
-- | moves into the river, as Odonus has it): ◆ mark, the running counts,
-- | clear. Top left of the live strip.
riverTools :: forall m. State -> H.ComponentHTML Action Slots m
riverTools st =
  HH.div [ HP.style "position: absolute; top: 8px; left: 10px; z-index: 9; display: flex; align-items: center; gap: 8px;" ]
    [ HH.button
        [ HE.onClick \_ -> CaptureMark
        , HP.title "flag the last couple of bars as a good bit (on the rig too: vetula $ mark)"
        , HP.style "padding: 3px 12px; border-radius: 6px; cursor: pointer; border: 1px solid #e8c14a55; background: #e8c14a1a; color: #e8c14a; font-size: 11px; white-space: nowrap;" ]
        [ HH.text "\x25c6 mark" ]
    , HH.span [ HP.style "font-family: 'SF Mono', Menlo, monospace; font-size: 9px; color: #ffffff55; white-space: nowrap;" ]
        [ HH.text (show (Logbook.noteCount st.capture.logbook) <> " notes \x00b7 " <> show (length st.capture.logbook.marks) <> " \x25c6") ]
    , HH.button
        [ HE.onClick \_ -> CaptureClear
        , HP.title "clear the river: its notes, marks and loops (on the rig too: vetula $ clear)"
        , HP.style "padding: 3px 10px; border-radius: 6px; cursor: pointer; border: 1px solid #ffffff1a; background: transparent; color: #ffffff66; font-size: 10px; white-space: nowrap;" ]
        [ HH.text "clear" ] ]

-- | The views that take the whole main space: no chyron above them, no
-- | colour tray below.
fullView :: Stage -> Boolean
fullView = case _ of
  Hunt Score -> true
  Hunt River -> true
  _ -> false

-- | Where the river is the surface: the river view.
showsRiver :: Stage -> Boolean
showsRiver = case _ of
  Hunt River -> true
  _ -> false

-- | **Omit a note** (⌥-click on the ladder): the axis that makes the five- and
-- | six-note chords of the banks playable. Its ghost brings it back.
omitTone :: forall o m. MonadAff m => Int -> Int -> Int -> H.HalogenM State Action Slots o m Unit
omitTone cid i k = do
  st <- H.get
  for_ (find (\c -> c.id == cid) st.chords) \c -> do
    let c' = applyToNode c (OV.dropAt i k (spreadOfNode c))
    applyChords (map (\d -> if d.id == cid then c' else d) st.chords)
    H.modify_ \s -> s { sounding = Just cid, revoicing = map (const cid) s.revoicing }
    playChord c'

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
geoView st = geoViewFit st 1.0

-- | The same, with the base window grown by `k` (never shrunk) to fit content.
geoViewFit :: State -> Number -> { x :: Number, y :: Number, w :: Number, h :: Number }
geoViewFit st k =
  let f = max 1.0 k
      hw = 440.0 * f / st.viewZoom
      hh = 300.0 * f / st.viewZoom
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
      -- Fit the window to how far the beads reach (the pool's spokes), so a
      -- full set never runs off the top.
      perRoot xs = fromMaybe 0 (maximum (map (\pc -> length (filter (\r -> r == pc) xs)) (range 0 11)))
      poolReach = cofWheel.baseR + toNumber (perRoot (map (\c -> mod c.root 12) shown) - 1) * cofWheel.dr
      vb = geoViewFit st ((poolReach + 40.0) / 300.0)
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
      )

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
-- | Every degree's cluster, for a key.
latticeFor :: Key -> Array (Array LatMember)
latticeFor key = mapWithIndex (degreeCluster key) (diatonicTriads key)

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

-- ---------------------------------------------------------------------------
-- The FIELD: Explore's one surface (AC, 2026-10-06)
-- ---------------------------------------------------------------------------

-- | **Explore as one view, zoomed semantically.** Every chord any rung can
-- | show is one persistent mark: the key's lattice (441 chords) and whatever
-- | the banks add. A rung only decides where each mark is, how large, and whether it is
-- | visible:
-- |
-- | - key · + four-note · + extended reveal more of the lattice in place;
-- | - banks moves the lattice chords that are in a bank to their pads, fades
-- |   in the banks' other chords, and fades the rest out.
-- |
-- | The field is drawn outside Halogen (`Vetula.Field`): Halogen renders an
-- | empty container, and `syncField` hands the field a scene after any action
-- | that changed what it shows. HATS keeps each mark the same DOM node and
-- | eases it to its new place; hover restyles the glyphs in place.
type Box = { minX :: Number, maxX :: Number, minY :: Number, maxY :: Number }

-- | A lattice chord's identity across arrangements: its root and notes.
markKey :: ChordNode -> String
markKey c = show (mod c.root 12) <> ":" <> show (pcSetOf c)

-- | The banks' geometry, in the field's units.
bankGeo :: { padW :: Number, padH :: Number, pad :: Number, gap :: Number, headW :: Number, headH :: Number }
bankGeo = { padW: 58.0, padH: 52.0, pad: 8.0, gap: 22.0, headW: 64.0, headH: 34.0 }

bankBox :: Box
bankBox =
  let w = bankGeo.headW + 3.0 * (4.0 * bankGeo.padW + 2.0 * bankGeo.pad) + 2.0 * bankGeo.gap
      h = bankGeo.headH + 3.0 * (4.0 * bankGeo.padH + 2.0 * bankGeo.pad) + 2.0 * bankGeo.gap
  in { minX: -w / 2.0, maxX: w / 2.0, minY: -h / 2.0, maxY: h / 2.0 }

-- | Where bank (r, c) starts.
bankOrigin :: Int -> Int -> { x :: Number, y :: Number }
bankOrigin r c =
  { x: bankBox.minX + bankGeo.headW + toNumber c * (4.0 * bankGeo.padW + 2.0 * bankGeo.pad + bankGeo.gap)
  , y: bankBox.minY + bankGeo.headH + toNumber r * (4.0 * bankGeo.padH + 2.0 * bankGeo.pad + bankGeo.gap)
  }

-- | Every pad, placed: its chord, centre, and the lattice key it may claim.
type BankPad = { chord :: ChordNode, x :: Number, y :: Number }

bankPadsFor :: Key -> Int -> Array BankPad
bankPadsFor key roll =
  let cells = Pads.grid key roll
  in concatMap
       (\cell ->
          let o = bankOrigin (fromMaybe 0 (elemIndex cell.reach Pads.reaches)) (fromMaybe 0 (elemIndex cell.colour Pads.colours))
          in mapWithIndex
               (\i c ->
                  { chord: c
                  , x: o.x + bankGeo.pad + (toNumber (mod i 4) + 0.5) * bankGeo.padW
                  , y: o.y + bankGeo.pad + toNumber (i / 4) * bankGeo.padH + 18.0
                  })
               cell.chords)
       cells

-- | The field's runtime: the drawing, the listener its marks call back on,
-- | and what it last drew.
type FieldRt =
  { handle :: Field.Handle
  , listener :: HS.Listener Action
  , seen :: Ref.Ref (Maybe FieldSeen)
  }

-- | What a drawing depends on. The lattice and the pads are cached in state,
-- | so a new array is a new key or a new shuffle.
type FieldSeen =
  { view :: Viewtype
  , lattice :: Array (Array LatMember)
  , pads :: Array BankPad
  , frame :: Box
  , pan :: { x :: Number, y :: Number, zoom :: Number }
  , hover :: Maybe { root :: Int, pcs :: Array Int }
  , cursor :: Maybe String
  , path :: Array Int
  , chords :: Array ChordNode
  }

-- | The field rung on screen, if Explore is showing the field.
fieldView :: State -> Maybe Viewtype
fieldView st
  | length st.genSel > 0 && length st.candidates > 0 = Nothing
  | otherwise = case st.stage of
      Hunt v | elem v [ KeyChords, Common, Lattice4, Lattice, Pads ] -> Just v
      _ -> Nothing

-- | Bring the field up to date with the state: a new scene when what it shows
-- | changed, a moved frame for a pan, new tints for a hover, nothing otherwise.
syncField :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
syncField = do
  st <- H.get
  for_ st.field \rt -> liftEffect case fieldView st of
    Nothing -> Ref.write Nothing rt.seen
    Just v -> do
      prev <- Ref.read rt.seen
      mounted <- Field.mounted
      let pan = { x: st.viewCx, y: st.viewCy, zoom: st.viewZoom }
          cursorKey = map _.key st.cursor
          same p = p.view == v && unsafeRefEq p.lattice st.lattice && unsafeRefEq p.pads st.bankPads
            && p.path == st.path && unsafeRefEq p.chords st.chords
      case prev of
        Just p | mounted && same p -> do
          when (p.pan /= pan) (Field.setView rt.handle false (fieldViewBox st p.frame))
          when (p.hover /= st.hoveredTriad) (Field.restyle rt.handle (glyphTint st))
          when (p.cursor /= cursorKey) (Field.select rt.handle cursorKey)
          Ref.write (Just p { pan = pan, hover = st.hoveredTriad, cursor = cursorKey }) rt.seen
        _ -> do
          let sc = fieldScene st v rt.listener
          fresh <- Field.draw rt.handle sc.scene
          Field.setView rt.handle (not fresh && mounted) (fieldViewBox st sc.frame)
          Ref.write (Just { view: v, lattice: st.lattice, pads: st.bankPads, frame: sc.frame, pan, hover: st.hoveredTriad, cursor: cursorKey
                               , path: st.path, chords: st.chords }) rt.seen

-- | A glyph's tint under the current hover, as the field's `data-hi` value.
glyphTint :: State -> Int -> Array Int -> String
glyphTint st _ pcs = hiName (hiFor st.hoveredTriad pcs)

hiName :: GlyphHi -> String
hiName = case _ of
  HiNone -> "none"
  HiSame -> "same"
  HiTier n -> "t" <> show (max 0 (min 5 n))

-- | The field's tints: `hiStyle`'s ramp as rules on each mark's `data-hi`.
fieldTintCss :: String
fieldTintCss =
  ".vf-poly, .vf-dot { pointer-events: none; }\n"
    <> ".vf-cursor { display: none; fill: none; stroke: #3b3428; stroke-width: 1.6; pointer-events: none; }\n"
    <> ".vf-mark[data-sel=\"1\"] .vf-cursor { display: inline; }\n"
    <> joinWith "\n"
         (map
            (\hi ->
               let sty = hiStyle hi
                   at = ".vf-mark[data-hi=\"" <> hiName hi <> "\"] "
               in at <> ".vf-poly { fill: " <> sty.fill <> "; stroke: " <> sty.stroke <> "; stroke-width: " <> sty.sw <> "; }\n"
                    <> at <> ".vf-dot { fill: " <> sty.otherDot <> "; }\n"
                    <> at <> ".vf-root { fill: " <> sty.rootDot <> "; }")
            ([ HiNone, HiSame ] <> map HiTier (range 0 5)))

-- | The frame fitted to the 880 × 600 window, then panned and zoomed.
fieldViewBox :: State -> Box -> Field.ViewBox
fieldViewBox st box =
  let fit = min (880.0 / (box.maxX - box.minX)) (600.0 / (box.maxY - box.minY)) * st.viewZoom
      w = 880.0 / fit
      h = 600.0 / fit
  in { x: (box.minX + box.maxX) / 2.0 + st.viewCx - w / 2.0
     , y: (box.minY + box.maxY) / 2.0 + st.viewCy - h / 2.0
     , w, h }

-- | Halogen's part of the field: an empty container, the wheel, and the pan.
-- | **The score** (docs/kb/plans/vetula-one-surface.md): the open
-- | progression first, then every progression a voice on the rig is playing,
-- | each as one system with its voices as letter badges. A voice whose chords
-- | are written into its line gets a system of its own. The badges come from
-- | the voices on the stage, which hush removes, so they are what is playing.
scoreSurface :: forall m. State -> H.ComponentHTML Action Slots m
scoreSurface st =
  -- `.vetula-surface` is how the key listener knows Vetula is on screen:
  -- without it every key (Esc, Tab, the arrows) stood down on the score
  HH.div
    [ HP.class_ (cn "vetula-surface vetula-surface--wide")
    , HP.style "position: absolute; inset: 0; display: flex; flex-direction: column;" ]
    [ HH.div [ HP.style "flex: 0 0 auto; max-height: 60%; overflow: auto; padding: 16px 22px 12px;" ]
        ( [ Score.system sp openHandlers openRow ]
        )
    , scoreCandidates st
    ]
  where
  sp = Score.spellingOf st.key.tonic (scaleSet st.key)
  pathNodes = mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) st.path
  openChords = map playNotes pathNodes
  unsaved = length st.path > 0 && currentSource st /= st.lastPubSig

  -- the voices on the stage, in letter order: their progression's name (if
  -- they name one), their chords, and how they play them
  cards = Array.sortWith _.n $ mapMaybe card (maybe [] Map.toUnfoldable st.stageCards)
  card (Tuple n text) = do
    spec <- readCard st n text
    pure { n, name: cardProgression text, chords: spec.chords
         , badge: { n, letter: voiceLetter n, how: howOf text, muted: spec.muted } }
  -- what the line does after its source: its `# …`, as written, less `mute`
  howOf text = case Array.drop 1 (split (Pattern "#") text) of
    [] -> ""
    rest -> joinWith " \x00b7 " (filter (\w -> w /= "" && w /= "mute") (map trim rest))

  openBadges = maybe [] (\nm -> map _.badge (filter (\c -> c.name == Just nm) cards)) st.progName
  openRow =
    { title: fromMaybe "new progression" st.progName
    , note: if length st.path == 0 then "empty: take chords in from the lattice"
            else if unsaved && not (Array.null openBadges) then "edited: its voices play the saved version until you save"
            else if unsaved then "not saved"
            else ""
    , top: true
    , chords: openChords
    , names: map OP.chordName openChords
    , badges: openBadges
    , active: st.scoreBar
    , selected: selOf (fromMaybe "new progression" st.progName) (length openChords)
    , dragging: st.scoreDrag
    , ownKey: Set.member (fromMaybe "new progression" st.progName) st.scoreRead
    -- the page's key, said: the open progression is in it
    , saved: Just { tonic: st.key.tonic, scale: scaleSet st.key, mode: modeWord st.key.mode, saved: false }
    , rebus: bundleRebus openChords
    , beats: rhythmOf st
    , tapping: map _.at st.tapping
    }
  openHandlers =
    { press: ScorePress
    , hover: \i -> HoverBar (Just i)
    , unhover: HoverBar Nothing
    , revoice: Just \i -> maybe (ScoreHear []) ScoreRevoice (st.path !! i)
    , select: ScoreSelect openRow.title
    , unselect: ScoreUnselect
    , readIn: ScoreReadIn openRow.title
    , adopt: Just ScoreAdopt
    , duplicate: Just ScoreDuplicate
    , remove: Just ScoreDelete
    , dropAt: Just ScoreDropAt
    , padOver: Just ScoreDragOver
    , padDrop: Just ScoreDropPad
    , voiceOver: VoiceDragOver
    , voiceDrop: VoiceDrop
    , tap: Just { start: TapStart, stop: TapStop, clear: ClearRhythm }
    , resume: (\w -> { name: w.name, act: ResumeWorking }) <$> st.resumable
    , addChords: Nothing
    , save: if unsaved then Just (SaveProg false) else Nothing
    , toQuadrat: case st.progName of
        Just nm | any (\e -> e.kept && e.name == nm) st.library -> Just (ToQuadrat nm)
        _ -> Nothing
    -- the open progression as the voices would play it: once saved
    , titleDrag: case st.progName of
        Just nm | any (\e -> e.kept && e.name == nm) st.library -> Just (TitleDrag nm)
        _ -> Nothing
    }
  -- the chosen run on a row, if it is this one and still fits it
  selOf title k = st.scoreSel >>= \sel ->
    if sel.row == title && sel.anchor < k && sel.to < k
      then Just { from: min sel.anchor sel.to, to: max sel.anchor sel.to } else Nothing


-- | **A progression's chords as a set**, as a picture: its distinct chords,
-- | in no order, through the chord rebus. The set is a function of the
-- | progression and never stored, so it cannot go stale; two progressions
-- | of the same chords in another order wear the same one.
bundleRebus :: Array (Array Int) -> Array TGlyph.GlyphIcon
bundleRebus chords =
  let set = sort (nub (map sort (filter (\ns -> length ns > 0) chords)))
  in if length set == 0 then [] else (TGlyph.chordGlyph set).icons

-- | The open progression's rhythm, if it has one a bar (an edit the
-- | rhythm could not follow leaves it out of step: then none).
rhythmOf :: State -> Array Int
rhythmOf st = if length st.rhythm == length st.path then st.rhythm else []

-- | A rhythm changed with its bars, while it is in step with them.
inStep :: State -> (Array Int -> Maybe (Array Int)) -> Array Int
inStep st f = if length st.rhythm == length st.path then fromMaybe [] (f st.rhythm) else []

-- | The chord at a bar of the open progression.
nodeAtBar :: State -> Int -> Maybe ChordNode
nodeAtBar st i = st.path !! i >>= \pid -> find (\c -> c.id == pid) st.chords

-- | What a dragged progression carries (the drawer's rows, a score title).
progDragPrefix :: String
progDragPrefix = "vetula-progression "

fieldSurface :: forall m. State -> H.ComponentHTML Action Slots m
fieldSurface st =
  HH.div
    ( [ HP.id Field.containerId
      , HP.style "width: 100%; height: 100%;"
      , HE.onWheel \we -> LevelWheel (WE.toEvent we) (WE.deltaY we)
      , HE.onMouseDown (PanStart <<< ME.toEvent)
      ] <> geoPanAttrs st )
    []

-- | The scene for a rung, and the frame it should fill.
fieldScene :: State -> Viewtype -> HS.Listener Action -> { scene :: Field.Scene, frame :: Box }
fieldScene st view listener =
  let
    members = concat st.lattice
    banks = view == Pads
    keyChords = map (\c -> Tuple (mod c.root 12) (pcSetOf c)) (diatonicTriads st.key <> diatonicSevenths st.key)
    isKey c = elem (Tuple (mod c.root 12) (pcSetOf c)) keyChords
    atLevel c = case view of
      KeyChords -> isKey c
      Common -> isKey c || commonChord st.key c
      Lattice4 -> length (pcSetOf c) <= 4 || commonChord st.key c
      Lattice -> true
      _ -> false
    -- The pads, each claiming the lattice mark of the same chord if there is
    -- one still unclaimed (C opens every bank; only the first takes it).
    latticeKeys = Set.fromFoldable (map (\m -> markKey m.chord) members)
    claim = foldl
      (\acc p ->
         let k = markKey p.chord
         in if Set.member k latticeKeys && not (Map.member k acc.claimed)
              then acc { claimed = Map.insert k p acc.claimed }
              else acc { own = snoc acc.own p })
      { claimed: Map.empty, own: [] }
      st.bankPads
    named = view == KeyChords
    mark k c x y scale shown label below =
      { key: k, root: c.root, pcs: c.pcs, x, y, scale, shown, label, below
      , tint: glyphTint st c.root c.pcs
      , enter: HS.notify listener (HoverPad (Just c))
      , leave: HS.notify listener (HoverPad Nothing)
      , select: HS.notify listener (SelectMark k c)
      , take: HS.notify listener (TakeMark k c)
      }
    latticeMark m =
      let k = markKey m.chord
      in case (if banks then Map.lookup k claim.claimed else Nothing) of
           Just p -> mark k p.chord p.x p.y 1.3 true p.chord.label true
           Nothing ->
             let vis = not banks && atLevel m.chord
             in mark k m.chord m.cx m.cy 1.0 vis (if named && vis then chordNameOf m.chord else "") false
    ownMark i p = mark ("pad" <> show i) p.chord p.x p.y 1.3 banks p.chord.label true
    shownLattice = filter (\m -> atLevel m.chord) members
    topY = fromMaybe latGeo.baseY (minimum (map _.cy shownLattice))
    marks0 = map latticeMark members <> mapWithIndex ownMark claim.own
    -- The progression as a path (plan: "Seeing what you have"): each chord
    -- taken, on the mark of the same root and notes (else of the same notes,
    -- a shown one first). Its chords stay lit at every level, so it never
    -- vanishes.
    pathKeyOf c =
      let set = pcSetOf c
          cands = filter (\m -> sort (nub (map (\p -> mod p 12) m.pcs)) == set) marks0
          rank m = (if mod m.root 12 == mod c.root 12 then 0 else 2) + (if m.shown then 0 else 1)
      in map _.key (head (sortBy (comparing rank) cands))
    -- a chord no mark holds (a borrowed or chromatic one) gets its own, in a
    -- row under the lattice, across by its root, so every chord of the
    -- progression is drawn (AC: "the progression's chords should all be visible")
    steps = if banks then [] else pathSteps st
    -- chords on one root stack downwards; a chord met again keeps its mark
    strayMark i k c =
      let deg = toNumber (mod (c.root - st.key.tonic + 12) 12) / 12.0 * 7.0
      in mark ("path:" <> show i) c (latticeLeft + deg * latGeo.bandW) (latGeo.baseY + 80.0 + 56.0 * toNumber k) 1.0 true (chordNameOf c) false
    strayKey c = Tuple (mod c.root 12) (pcSetOf c)
    stepKeys = (foldl (\acc (Tuple i c) -> case pathKeyOf c of
                  Just k -> acc { out = snoc acc.out (Tuple k Nothing) }
                  Nothing -> case Map.lookup (strayKey c) acc.seen of
                    Just k -> acc { out = snoc acc.out (Tuple k Nothing) }
                    Nothing ->
                      let row = fromMaybe 0 (Map.lookup (mod c.root 12) acc.rows)
                          key = "path:" <> show i
                      in { out: snoc acc.out (Tuple key (Just (strayMark i row c)))
                         , seen: Map.insert (strayKey c) key acc.seen
                         , rows: Map.insert (mod c.root 12) (row + 1) acc.rows })
                 { out: [], seen: Map.empty, rows: Map.empty } (mapWithIndex Tuple steps)).out
    path = map fst stepKeys
    marks = map (\m -> if elem m.key path then m { shown = true } else m) (marks0 <> mapMaybe snd stepKeys)
    -- The lattice's framing: the shown chords, and
    -- the progression's own chords wherever they sit (a progression from
    -- higher rungs, reopened on "key", had its beads off the top)
    pathMarks = filter (\m -> elem m.key path) marks
    base =
      { minX: latticeLeft - 70.0
      , maxX: latticeLeft + 6.0 * latGeo.bandW + 70.0 + (if named then 70.0 else 0.0)
      , minY: topY - 30.0
      , maxY: latGeo.baseY + 40.0
      }
    latBox = foldl (\b m -> b { minX = min b.minX (m.x - 50.0), maxX = max b.maxX (m.x + 50.0)
                              , minY = min b.minY (m.y - 40.0), maxY = max b.maxY (m.y + 40.0) }) base pathMarks
    -- a chord's level: 0 the key's own, 1 common, 2 up to four notes, 3 beyond
    levelOf c
      | isKey c = 0
      | commonChord st.key c = 1
      | length (pcSetOf c) <= 4 = 2
      | otherwise = 3
    edges = concatMap (fieldEdges levelOf) st.lattice
    degreeLabels =
      mapWithIndex
        (\i seed ->
           { x: latticeLeft + toNumber i * latGeo.bandW, y: latGeo.baseY + 26.0, anchor: "middle", text: noteName seed.root
           , css: "font-size: 13px; fill: #6a6a6a; letter-spacing: 0.04em; -webkit-user-select: none; user-select: none;" })
        (diatonicTriads st.key)
  in
    { scene:
        { banks
        , lattice: { rects: [], texts: degreeLabels }
        , bank: bankFurniture
        , edges
        , edgeLevels: case view of
            KeyChords -> 1
            Common -> 2
            Lattice4 -> 3
            Lattice -> 4
            _ -> 0
        , marks
        , css: fieldTintCss
        , selected: map _.key st.cursor
        , path
        }
    , frame: if banks then bankBox { minX = bankBox.minX - 20.0, maxX = bankBox.maxX + 20.0, minY = bankBox.minY - 16.0, maxY = bankBox.maxY + 24.0 } else latBox
    }

-- | **The common chords** (AC, 2026-10-06): what a pop or jazz lead sheet
-- | would name, between the key's own triads and sevenths and the full
-- | lattice. A lattice chord is its root plus some of the tones a third, fifth,
-- | seventh, ninth, eleventh and thirteenth above it in the scale (k = 1 … 6),
-- | so "common" is a list of those tone sets, and every quality follows the
-- | key. Power chords are left out (AC).
commonShapes :: Array (Array Int)
commonShapes =
  [ [ 1, 2 ]          -- triad
  , [ 1, 2, 3 ]       -- seventh
  , [ 2, 4 ]          -- sus2
  , [ 2, 5 ]          -- sus4
  , [ 1, 2, 6 ]       -- 6
  , [ 1, 2, 4 ]       -- add9
  , [ 2, 3, 5 ]       -- 7sus4
  , [ 1, 3 ]          -- shell: root, third, seventh
  , [ 1, 2, 3, 4 ]    -- 9th
  , [ 1, 2, 4, 6 ]    -- 6/9
  ]

commonChord :: Key -> ChordNode -> Boolean
commonChord key c =
  let sc = scaleSet key
      n = length sc
      r = mod c.root 12
      others = filter (_ /= r) (pcSetOf c)
  in case elemIndex r sc of
       Nothing -> false
       Just rd ->
         let toneOf k = fromMaybe (-1) (index sc (mod (rd + 2 * k) n))
             ks = filter (\k -> elem (toneOf k) others) (range 1 6)
         in length (nub (map toneOf ks)) == length others && elem ks commonShapes

-- | One degree's covering edges, each at the level of its higher end, so it
-- | shows only while both its ends do.
fieldEdges :: (ChordNode -> Int) -> Array LatMember -> Array Field.Edge
fieldEdges levelOf members =
  let ms = map (\m -> { m, lv: levelOf m.chord }) members
      pairs = concat (mapWithIndex (\i a -> map (\b -> Tuple a b) (drop (i + 1) ms)) ms)
  in concatMap
       (\(Tuple a b) ->
          if pcSymDiff a.m.chord.pcs b.m.chord.pcs == 1 then
            [ { x1: a.m.cx, y1: a.m.cy, x2: b.m.cx, y2: b.m.cy, level: max a.lv b.lv } ]
          else [])
       pairs

-- | The banks' backgrounds and axis names.
bankFurniture :: Field.Furniture
bankFurniture =
  { rects:
      concat (mapWithIndex (\r _ -> mapWithIndex (\c _ -> bankRect r c) Pads.colours) Pads.reaches)
  , texts:
      mapWithIndex (\c col -> heading (bankOrigin 0 c).x (bankBox.minY + 20.0) "start" (Pads.colourLabel col)) Pads.colours
        <> mapWithIndex (\r row -> heading (bankBox.minX + bankGeo.headW - 12.0) ((bankOrigin r 0).y + 26.0) "end" (Pads.reachLabel row)) Pads.reaches
  }
  where
  bankRect r c =
    let o = bankOrigin r c
    in { x: o.x, y: o.y, w: 4.0 * bankGeo.padW + 2.0 * bankGeo.pad, h: 4.0 * bankGeo.padH + 2.0 * bankGeo.pad }
  heading x y anchor t =
    { x, y, anchor, text: t
    , css: "font-size: 12px; fill: #7a7360; letter-spacing: 0.08em; text-transform: uppercase; -webkit-user-select: none; user-select: none;" }

-- | A chord's name from its pitch classes over its root: Harmonia's best
-- | reading, as Vary and Odonus name theirs.
chordNameOf :: ChordNode -> String
chordNameOf c =
  let r = mod c.root 12
      ivs = sort (map (\p -> mod (p - r) 12) (pcSetOf c))
      common = case ivs of
        [ 0, 4, 7 ] -> Just ""
        [ 0, 3, 7 ] -> Just "m"
        [ 0, 3, 6 ] -> Just "dim"
        [ 0, 4, 8 ] -> Just "aug"
        [ 0, 4, 7, 11 ] -> Just "maj7"
        [ 0, 4, 7, 10 ] -> Just "7"
        [ 0, 3, 7, 10 ] -> Just "m7"
        [ 0, 3, 6, 10 ] -> Just "m7b5"
        [ 0, 3, 6, 9 ] -> Just "dim7"
        [ 0, 3, 7, 11 ] -> Just "mMaj7"
        [ 0, 4, 8, 11 ] -> Just "maj7#5"
        _ -> Nothing
  in case common of
       Just q -> noteName r <> q
       -- the recogniser is thorough and slow; only the uncommon go to it
       Nothing -> fromMaybe c.label (map candidateName (best (observeWithBass r (pcSetOf c))))


-- ---------------------------------------------------------------------------
-- The BANKS lens — the chord space as nine banks of sixteen
-- ---------------------------------------------------------------------------

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
    , HE.onClick \e -> if ME.shiftKey e then AltPut c else AltClick c
    , HE.onDragEnd \_ -> AltDragEnd
    , HP.draggable (st.stage == Hunt Score)
    , HE.onDragStart \_ -> ScorePadDrag c
    ]
    [ SE.svg
        [ SA.viewBox (-19.0) (-19.0) 38.0 38.0, SA.width 34.0, SA.height 34.0 ]
        (pcPolygon (hiFor st.hoveredTriad c.pcs) c.root c.pcs 0.0 0.0 12.0
)
    , HH.div
        [ HP.style "font-size: 10px; color: #6a6250; line-height: 1.1; text-align: center; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%; -webkit-user-select: none; user-select: none;" ]
        [ HH.text c.label ]
    ]

-- | A chord's pitch classes as a set: what "the same chord" means across views,
-- | whatever its voicing or inversion.
pcSetOf :: ChordNode -> Array Int
pcSetOf c = sort (nub (map (\p -> mod p 12) c.pcs))


-- | A pad lights when its pitch-class set matches whatever is hovered anywhere
-- | in the app — so hovering one pad shows you every other bank holding the
-- | same chord, which is how the nesting between cells becomes visible.
padLit :: State -> ChordNode -> Boolean
padLit st c = case st.hoveredTriad of
  Nothing -> false
  Just h -> sort (nub (map (\p -> mod p 12) h.pcs)) == sort (nub (map (\p -> mod p 12) c.pcs))

-- | The chord the candidates are for: the one last heard (on the score, the
-- | bar last clicked).
varySource :: State -> Maybe ChordNode
varySource = soundingChord

-- | The chord last played, from any view.
soundingChord :: State -> Maybe ChordNode
soundingChord st = st.lastHeard

-- | One variation pad. Click HEARS it; on the score, shift-click (or drag)
-- | puts it in the chosen bar.
varyPad :: forall m. State -> ChordNode -> H.ComponentHTML Action Slots m
varyPad st c =
  HH.button
      [ HP.style ("display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 1px; "
                   <> "border: 1px solid " <> (if padLit st c then "#cdbb8c" else "#eee7d6") <> "; "
                   <> "background: " <> (if padLit st c then "#fdf6e4" else "#ffffff") <> "; "
                   <> "border-radius: 4px; padding: 5px 2px 4px; cursor: pointer; min-width: 0;")
      , HP.title (c.label <> " — " <> show (playNotes c))
      , HE.onMouseEnter \_ -> HoverPad (Just c)
      , HE.onMouseLeave \_ -> HoverPad Nothing
      -- a click hears the chord alone; on the score, in its bar's register
      , HE.onClick \e -> if ME.shiftKey e then AltPut c else AltClick c
      , HE.onDragEnd \_ -> AltDragEnd
      , HP.draggable (st.stage == Hunt Score)
      , HE.onDragStart \_ -> ScorePadDrag c
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
inVaryGrid st = st.stage == Hunt Score

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
currentSource st = progressionSourceIn (groupLabel st.key) (rhythmOf st) (pathSteps st)

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

-- | The progression rows' pitch-axis geometry (shared with DragMove's inverse).
prowW :: Number
prowW = 300.0

prowPad :: Number
prowPad = 16.0

-- | The left-hand pitch ladder: a fixed pitch axis (C2..C6) showing the chord
-- | that is currently sounding as coloured dots — the prototype's display.
-- |
-- | `dx` moves the ladder sideways (the progression's ladders stand in a row,
-- | in one user space, so the drag's y-maths is the same for every one);
-- | `labels` writes the octave names (the first only); `scl`, when not empty,
-- | rings the notes outside it in red, as on the score.
ladderView :: forall m. Number -> Boolean -> Array Int -> Maybe VoiceSel -> Maybe ChordNode -> Array (H.ComponentHTML Action Slots m)
ladderView dx labels scl msel msound = grid <> octs <> outs <> dots
  where
  lx = -432.0 + dx
  rx = -320.0 + dx
  dotX = -376.0 + dx
  midiToY m = 205.0 - toNumber (m - 36) * 9.8
  outs = case msound of
    Just c | length scl > 0 ->
      map (\m -> SE.circle [ SA.cx dotX, SA.cy (midiToY m), SA.r 9.5, SA.class_ (cn "ladder-out") ])
        (filter (\m -> not (elem (mod m 12) scl)) (playNotes c))
    _ -> []
  grid = map
    (\m -> SE.line [ SA.x1 lx, SA.y1 (midiToY m), SA.x2 rx, SA.y2 (midiToY m), SA.class_ (cn "ladder-line") ])
    (range 36 84)
  octs = concatMap oct [ 36, 48, 60, 72, 84 ]
  oct m =
    [ SE.line [ SA.x1 lx, SA.y1 (midiToY m), SA.x2 rx, SA.y2 (midiToY m), SA.class_ (cn "ladder-oct") ] ]
      <> (if labels then [ SE.text [ SA.x (lx - 6.0), SA.y (midiToY m + 3.0), SA.class_ (cn "ladder-label") ] [ HH.text ("C" <> show (m / 12 - 1)) ] ] else [])
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
      -- drag moves the note by octaves; ⌥-drag leaves a double behind, and
      -- ⌥-click (no drag) omits it (DragEnd). The bass is not a tone the
      -- spread can reach, so it is never omittable.
      , if i == 0 then HE.onMouseDown \_ -> SelectVoice cid BassVoice
        else HE.onMouseDown \ev -> DragStart (ME.altKey ev) false cid (i - 1) m
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
   . Number
  -> Maybe ChordNode
  -> Map String (Array (Array Int))
  -> Array (H.ComponentHTML Action Slots m)
voicingStrip dx msound favs = case msound of
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
  sLeft = -430.0 + dx
  sRight = -322.0 + dx
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

-- | The bar the progression's ladders focus: the score's, if it still holds
-- | the chord being revoiced, else that chord's first place in the
-- | progression (a chord can stand in more than one bar).
revoiceAt :: State -> Int
revoiceAt st = case st.scoreBar of
  Just b | st.path !! b == st.revoicing -> b
  _ -> fromMaybe 0 (st.revoicing >>= \cid -> elemIndex cid st.path)

-- | The revoice modal: a dim backdrop + a left "drawer" lighting up the kept
-- | pitch ladder + favourites strip for one chosen chord — the one-stop shop for
-- | every WITHIN-chord change. Octave-drag a note (⌥ doubles it), Tab/Shift-Tab
-- | cycle voicings, ↑/↓ nudge a selected voice, f keeps a voicing, and the slash
-- | row re-foots the chord on any of its tones. Esc / click-away closes.
revoiceModal :: forall m. State -> H.ComponentHTML Action Slots m
revoiceModal st =
  Modal.modal
    { open: isJust mc
    , title:
        if onPath then fromMaybe "new progression" st.progName <> " · revoice"
        else maybe "revoice" (\c -> nameOf c <> " · revoice") mc
    , onClose: CloseRevoice
    }
    (maybe [] (if onPath then progressionBody else revoiceBody) mc)
  where
  mc = st.revoicing >>= \cid -> find (\c -> c.id == cid) st.chords
  sp = Score.spellingOf st.key.tonic (scaleSet st.key)
  nameOf c = Score.spellName sp (OP.chordName (playNotes c))
  onPath = maybe false (\cid -> elem cid st.path) st.revoicing
  pathNodes = mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) st.path
  -- the focused bar: the score's, if it still holds the chord, else the
  -- chord's first place in the progression
  focusAt = revoiceAt st

  -- **Every bar's ladder, side by side** on one pitch axis, each voice joined
  -- to the same voice of the next chord (lowest to lowest, and so on up).
  -- Where two chords have different numbers of notes there is no such
  -- voice, and nothing is drawn: the gap says so. The focused bar carries
  -- the single-chord tools; click a ladder to focus it.
  progressionBody c =
    let
      n = length pathNodes
      colW = 122.0
      vbW = 150.0 + toNumber (max 0 (n - 1)) * colW
      dxOf i = toNumber i * colW
      midiToY m = 205.0 - toNumber (m - 36) * 9.8
      dotX i = -376.0 + dxOf i
      cols = mapWithIndex col pathNodes
      col i _ =
        SE.rect
          [ SA.x (-438.0 + dxOf i), SA.y (-330.0), SA.width (colW - 4.0), SA.height 548.0, SA.rx 5.0, SA.ry 5.0
          , SA.class_ (cn ("rv-col" <> if i == focusAt then " rv-col--focus" else ""))
          , HE.onClick \_ -> RevoiceFocus i ]
      names = mapWithIndex (\i d ->
        SE.text [ SA.x (dotX i), SA.y (-312.0), SA.class_ (cn ("rv-name" <> if i == focusAt then " rv-name--focus" else "")) ]
          [ HH.text (nameOf d) ]) pathNodes
      joins = concat (mapWithIndex (\i d -> case pathNodes !! (i + 1) of
        Just e | length (playNotes d) == length (playNotes e) ->
          zipWith (\a b ->
            SE.line [ SA.x1 (dotX i + 8.0), SA.y1 (midiToY a), SA.x2 (dotX (i + 1) - 8.0), SA.y2 (midiToY b)
                    , SA.class_ (cn ("rv-join" <> if max (b - a) (a - b) > 7 then " rv-join--leap" else "")) ])
            (sort (playNotes d)) (sort (playNotes e))
        _ -> []) pathNodes)
      ladders = concat (mapWithIndex (\i d ->
        ladderView (dxOf i) (i == 0) (scaleSet st.key) (if i == focusAt then st.selected else Nothing) (Just d)) pathNodes)
      tones = sort (nub (map (\m -> mod m 12) (playNotes c)))
    in
      [ HH.div [ HP.class_ (cn "rv-wide"), HP.style "overflow-x: auto; overflow-y: hidden; max-width: 100%;" ]
          [ SE.svg
              ( [ SA.viewBox (-455.0) (-335.0) vbW 560.0
                , SA.class_ (cn "rv-svg")
                , HP.style ("display: block; margin: 0 auto; width: " <> show vbW <> "px; height: 540px; user-select: none; -webkit-user-select: none;")
                ]
                  <> (case st.drag of
                        Just _ ->
                          [ HE.onMouseMove (DragMove <<< ME.toEvent)
                          , HE.onMouseUp \_ -> DragEnd
                          , HE.onMouseLeave \_ -> DragEnd
                          ]
                        Nothing -> []) )
              -- the backgrounds first and fixed in number, so nothing a press
              -- changes is inserted ahead of the dots
              ( cols <> names <> voicingStrip (dxOf focusAt) (Just c) st.favorites <> joins <> ladders )
          ]
      , HH.div [ HP.style "display: flex; gap: 6px; justify-content: center; align-items: baseline; flex-wrap: wrap; margin-top: 10px;" ]
          ( [ HH.span [ HP.style "font-size: 12px; font-weight: 600; color: #4a4a4a; margin-right: 4px;" ] [ HH.text (nameOf c) ]
            , rvBtn "8ve ▼" "this chord down an octave, bass included" (ShiftOctave (-1))
            , rvBtn "⟲ invert" "roll the lowest voice down: the previous inversion" (RollBass (-1))
            , rvBtn "invert ⟳" "roll the lowest voice up: the next inversion" (RollBass 1)
            , rvBtn "8ve ▲" "this chord up an octave, bass included" (ShiftOctave 1)
            , HH.span [ HP.style "font-size: 11px; color: #9a9a9a; margin-left: 8px;" ] [ HH.text "bass /" ]
            ]
              <> map (\pc -> slashHtml c.bassPc pc) tones )
      , HH.div [ HP.style "display: flex; gap: 6px; justify-content: center; margin-top: 8px;" ]
          [ rvBtn "▶ play through" "the progression, bar by bar" PlayPath
          , rvBtn "lead on from here →" "voice every later bar from the one before it, with the least motion (bars with a different number of notes are left alone)" RevoiceLead
          ]
      , HH.div [ HP.style "margin-top: 8px; font-size: 11px; color: #9a9a9a; text-align: center;" ]
          [ HH.text "click a ladder to focus it · ←→ bars · Tab voicings · ↑↓ nudge · drag = 8ve · ⌥ doubles · ⇧ drops a note · f keep · Esc" ]
      ]
  slashHtml activeBass pc =
    HH.button
      [ HP.style ("border: 1px solid " <> (if pc == activeBass then "#4a4a4a" else "#d8d8d8") <> "; background: "
                   <> (if pc == activeBass then "#4a4a4a; color: #fff;" else "#fafafa; color: #4a4a4a;")
                   <> " cursor: pointer; padding: 2px 7px; border-radius: 3px; font-size: 11px;")
      , HP.title "re-foot the chord on this note"
      , HE.onClick \_ -> SlashBass pc ]
      [ HH.text (Score.spellName sp (noteName pc)) ]
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
           ( voicingStrip 0.0 (Just c) st.favorites
               <> ladderView 0.0 true (scaleSet st.key) st.selected (Just c)
               <> [ SE.text [ SA.x (-447.0), SA.y 250.0, SA.class_ (cn "rv-bass-label") ] [ HH.text "bass /" ] ]
               <> mapWithIndex (slashBtn c.bassPc) tones )
       , HH.div
           [ HP.style "display: flex; gap: 6px; justify-content: center; margin-top: 10px;" ]
           [ rvBtn "8ve ▼" "the whole chord down an octave, bass included" (ShiftOctave (-1))
           , rvBtn "⟲ invert" "roll the lowest voice down — the previous inversion" (RollBass (-1))
           , rvBtn "invert ⟳" "roll the lowest voice up — the next inversion" (RollBass 1)
           , rvBtn "8ve ▲" "the whole chord up an octave, bass included" (ShiftOctave 1)
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


