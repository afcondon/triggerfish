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

import Data.Array (concat, concatMap, deleteAt, drop, elem, elemIndex, filter, find, head, index, insertAt, last, length, mapMaybe, mapWithIndex, modifyAt, nub, nubByEq, range, replicate, sort, take, updateAt, zip, (!!))
import Data.Foldable (any, foldl, foldr, for_, maximum, minimum, sum)
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
import Vetula.SvgCoord (svgYFromEvent, svgXFromEvent, isFormField, surfaceHidden)
import Vetula.Path as Path
import Vetula.Generate (GenMode(..), generateCandidates)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Triggerfish.Transport (Sounding(..))
import Vetula.Store as Store
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
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)
import Hylograph.Simulation
  ( Engine(..), SimulationEvent(..), SimulationHandle, SimulationNode
  , Setup, runSimulation, setup, manyBody, collide, link, positionX, positionY
  , withStrength, withRadius, withDistance, withX, withY, static, dynamic )
import Harmonia.Chord (Key, Mode(..), cMajorKey)
import Vetula.Harmony (ChordNode, Family, Kind(..), blackKeyPcs, diatonicTriads, generate, interchangeChords, keyX, keyboard, latticeChild, latticeFamily, mcmullenChords, noteName, place, placeOutside, playNotes, scaleSet, suspendSet, triadOn, voicingCandidates, whiteKeyPcs)

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

-- | The surfaces. Lab = the merged harmonic surface: one triad per scale degree,
-- | each of which explodes into the web of its extensions / suspensions /
-- | bass-inversions; grow chords by ear, click chords to walk a progression that
-- | assembles down the right-hand side (each step a compact note-row to revoice +
-- | a Tidal source to save/load). Performance = fan a saved progression to voices,
-- | each reading the same chords on its own clock.
data Tab = Lab | Performance

derive instance eqTab :: Eq Tab

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
  , channel :: Int          -- MIDI channel 0..15 (ToMidi) / the Odonus id (ToOdonus)
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
  , tab :: Tab                         -- which surface is showing
  , chords :: Array ChordNode          -- the model (pin, provenance, layout targets)
  , nodes :: Array VNode               -- live positions from the simulation
  , focusId :: Int
  , hoveredId :: Maybe Int
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
  , perfProg :: Maybe { name :: String, chordIds :: Array Int }  -- loaded copy (ids into `chords`)
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
  }

data Action
  = Initialize
  | MidiReady (Maybe Midi.MidiOut) String
  | SimTick
  | SimDone
  | Hover (Maybe Int)
  | SetTab Tab
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
  | PlayStep Int           -- hear one step (and make it the active chord)
  | CopyTidal String       -- copy the progression's Tidal source to the clipboard
  | EditSource String      -- the Tidal-source textarea was edited (freeze the live view)
  | LoadSource             -- parse the textarea + rebuild the progression from it
  | RevertSource           -- discard edits, go back to the live-derived source
  | ToggleSource           -- reveal / hide the Tidal source code
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

component :: forall i o m. MonadAff m => H.Component SourceQuery i o m
component = H.mkComponent
  { initialState: \_ ->
      { key: cMajorKey
      , tab: Lab
      , chords: []
      , nodes: []
      , focusId: 0           -- the first diatonic triad seed
      , hoveredId: Nothing
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
      , genSel: []
      , candidates: []
      , adventure: 0.25
      , library: []
      , capSeq: 0
      , lastCapIdx: Nothing
      , lastCapSig: ""
      , libSearch: ""
      , saveName: ""
      , perfProg: Nothing
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

-- | x is pinned HARD to the target; y-pin depends on the tab. Explore wants a
-- | gentle y so collision can beeswarm chords sharing a key into a vertical
-- | cluster; the Lattice wants a firm y so the extension strata hold their rows.
forceSetupFor :: Tab -> Setup VNode
forceSetupFor = case _ of
  -- the Lab surface wants a firm y so the extension strata hold their rows; the
  -- fallback (Performance, which never starts a simulation) is the gentle beeswarm.
  Lab ->
    setup "vetula"
      [ positionX "px" # withX (dynamic _.targetX) # withStrength (static 0.12)
      , positionY "py" # withY (dynamic _.targetY) # withStrength (static 0.55)
      , link "neighbours" # withDistance (static 46.0) # withStrength (static 0.3)
      , collide "collide" # withRadius (dynamic (\n -> n.radius + 6.0)) # withStrength (static 0.9)
      , manyBody "charge" # withStrength (static (-8.0))
      ]
  _ ->
    setup "vetula"
      [ positionX "px" # withX (dynamic _.targetX) # withStrength (static 0.95)
      , positionY "py" # withY (dynamic _.targetY) # withStrength (static 0.3)
      , collide "collide" # withRadius (dynamic (\n -> n.radius + 6.0)) # withStrength (static 0.85)
      , manyBody "charge" # withStrength (static (-6.0))
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
      -- a loaded performance progression's chords also live in `chords` (as
      -- imported copies). A lab rebuild replaces the seed set, so carry the perf
      -- chords across untouched — changing key/scale mustn't wipe the loaded prog.
      let perfIds = maybe [] _.chordIds st.perfProg
          perfKept = filter (\c -> elem c.id perfIds) st.chords
          -- Carry the Lab PROGRESSION's chords across the rebuild too (it lives in
          -- `path`), so the assembled progression survives a key/scale change or a
          -- tab round-trip — not just the loaded-performance copy.
          pathKept = filter (\c -> elem c.id st.path) st.chords
          placed = nubByEq (\a b -> a.id == b.id)
                     (map (place key focus) chords0 <> perfKept <> pathKept)
          simNodes = map mkSimNode (map (place key focus) chords0)
      result <- liftEffect $ runSimulation
        { engine: D3
        , setup: forceSetupFor st.tab
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
          , handle = Just result.handle, subId = Just sid
          , imported = Set.union s.imported (Set.fromFoldable perfIds) }

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
    startWith st.key (seedFocus st.tab) (seedsFor st.tab st.key)

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

  -- switching tabs is a hard rebuild of the SURFACE (Performance never sims) and
  -- of the transient exploration scaffolding (families / candidates / borrow). But
  -- the PROGRESSION (`path`) SURVIVES — it's the work you've assembled, so a peek
  -- at Performance and back must not wipe it. `startWith` carries its chords across.
  SetTab t -> do
    st <- H.get
    when (t /= st.tab) do
      stopSim
      H.modify_ _ { tab = t, hoveredId = Nothing, revoicing = Nothing, familyScale = Map.empty, focusedFamily = Nothing, stackHead = Nothing, dropped = Map.empty, borrowMode = Nothing, imported = Set.empty, sourceEdit = Nothing, genSel = [], candidates = [] }
      startWith st.key (seedFocus t) (seedsFor t st.key)

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
        "c" -> H.modify_ _ { path = [] }
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
    when (st.tab == Lab) do
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

  PlayStep pid -> playId pid

  CopyTidal src -> liftEffect (copyText src)

  EditSource s -> H.modify_ _ { sourceEdit = Just s }

  RevertSource -> H.modify_ _ { sourceEdit = Nothing }

  ToggleSource -> H.modify_ \s -> s { sourceOpen = not s.sourceOpen }

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
        , perfProg = Just { name: entry.name, chordIds: ids }
        , voices = [ defaultVoice 0 0 Block (length fresh) ]
        , nextVoiceId = 1
        , sounding = head ids
        }

  UnloadProg -> do
    stopClock
    H.modify_ _ { perfProg = Nothing, voices = [], playing = false }

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
      voices' <- liftEffect $ traverse (stepVoice mout reefChords tick.index pulseMs tick.delayMs) st.voices
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
      -- an actual drag happened: re-sort the voicing so colours/indices stay
      -- tidy, then resize the bubble (span changed) and let collision settle
      -- a real drag: re-sort the voicing. With alt held, the grabbed voice's
      -- ORIGINAL pitch is left behind as a doubled tone (root / fifth an octave
      -- away) — append startMidi before sorting; otherwise it's a plain move.
      Just dg | dg.offset /= 0 -> do
        let addDouble v = if dg.double then v <> [ dg.startMidi ] else v
            chords' = map (\c -> if c.id == dg.chordId then c { voicing = sort (addDouble c.voicing) } else c) st.chords
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
  { id: vid, channel, dest: ToMidi, renderer, pattern: defaultPattern n, patternDraft: defaultPattern n
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
    for_ st.voices \v -> for_ v.held \nn -> Midi.noteOffAt out { channel: v.channel, note: nn, delayMs: 0.0 }

-- | The loaded performance progression's chords, resolved from the working copy.
perfChords :: State -> Array ChordNode
perfChords st = case st.perfProg of
  Just pp -> mapMaybe (\pid -> find (\c -> c.id == pid) st.chords) pp.chordIds
  Nothing -> []

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
toReefVoice :: Voice -> RV.VVoice
toReefVoice v =
  { dest: case v.dest of
      ToMidi -> RV.VToMidi
      ToOdonus -> RV.VToOdonus
  , renderer: case v.renderer of
      Block -> RV.VBlock
      Arp -> RV.VArp
      Strummed -> RV.VStrummed
  , channel: v.channel
  , durs: v.durs
  , phase: v.phase
  , muted: v.muted
  }

buildPerf :: State -> { chords :: Array RV.VChord, voices :: Array RV.VVoice }
buildPerf st =
  { chords: map toReefChord (perfChords st)
  , voices: map toReefVoice st.voices
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
    ch = maybe 8 _.channel v0
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
stepVoice :: Maybe Midi.MidiOut -> Array RV.VChord -> Int -> Number -> Number -> Voice -> Effect Voice
stepVoice mout reefChords pulse pulseMs baseDelayMs v =
  let rv = toReefVoice v
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
            { channel: v.channel, note: e.note, velocity: e.velocity
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

-- | The drawn path: bold gold edges for the smooth single-note bridges, and a
-- | dashed violet edge for each interconnector leap (a step between two chords
-- | that aren't graph-adjacent — i.e. across families / degrees).
pathLinkLines :: forall m. Map Int { x :: Number, y :: Number } -> Path.Graph -> Array Int -> Array (H.ComponentHTML Action Slots m)
pathLinkLines posMap g ids = concatMap edge (zip ids (drop 1 ids))
  where
  adjacent a b = elem b (fromMaybe [] (Map.lookup a g))
  edge (Tuple a b) = case Map.lookup a posMap, Map.lookup b posMap of
    Just p, Just q ->
      [ SE.line [ SA.x1 p.x, SA.y1 p.y, SA.x2 q.x, SA.y2 q.y, SA.class_ (cn (if adjacent a b then "path-edge" else "path-jump")) ] ]
    _, _ -> []

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
  case st.hoveredId of
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
  ]

-- | Seeds + focus for a tab.
seedsFor :: Tab -> Key -> Array ChordNode
seedsFor _ = diatonicTriads

seedFocus :: Tab -> Int
seedFocus _ = 0

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
contentKey c = show c.pcs <> "|" <> show c.voicing <> "|" <> show c.bassPc

-- | Reset to the McMullen palette in the current key/scale; pinned survive.
resetPalette :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
resetPalette = do
  st <- H.get
  let kept = filter _.pinned st.chords
      set = nubByEq (\a b -> a.id == b.id) (seedsFor st.tab st.key <> kept)
  stopSim
  H.modify_ _ { dropped = Map.empty, borrowMode = Nothing, focusedFamily = Nothing, stackHead = Nothing }
  startWith st.key (seedFocus st.tab) set

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
      set = nubByEq (\a b -> a.id == b.id) (seedsFor st2.tab key <> kept)
  stopSim
  H.modify_ _
    { chords = chords'
    , imported = Set.union st2.imported pathIds
    , familyScale = Map.empty, focusedFamily = Nothing, stackHead = Nothing
    , dropped = Map.empty, borrowMode = Nothing, sourceEdit = Nothing
    , genSel = [], candidates = []
    -- keep `path` (the progression, now transposed); fork a fresh capture session
    , lastCapIdx = Nothing, lastCapSig = "" }
  startWith key (seedFocus st2.tab) set

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

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div
    [ HP.style "padding: 12px 24px 18px; max-width: 1600px; margin: 0 auto;" ]
    [ topNav st
    , case st.tab of
        Performance -> performanceView st
        _ ->
          HH.div
            [ HP.style "display: flex; gap: 16px; align-items: flex-start;" ]
            [ HH.div [ HP.style "flex: 1; min-width: 0;" ] [ pickBar st, surface st ]
            , HH.div [ HP.style "flex: 0 0 340px;" ]
                [ progressionPanel st ]
            ]
    , HH.p
        [ HP.style "color: #9a9a9a; font-size: 13px; margin: 8px 0 0;" ]
        [ HH.text (helpText st.tab) ]
    ]

-- | Everything that used to stack down the page — title, tab switch, key, scale,
-- | MIDI status — crushed into one slim top bar, to give the surface its room.
topNav :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
topNav st =
  HH.div
    [ HP.style "display: flex; align-items: center; gap: 12px; margin: 0 0 12px; padding-bottom: 9px; border-bottom: 1px solid #ededed; flex-wrap: wrap;" ]
    ( [ HH.h1 [ HP.style "font-weight: 600; font-size: 19px; letter-spacing: -0.01em; margin: 0 6px 0 0;" ] [ HH.text "Vetula" ]
      , navTab st.tab Lab "Lab"
      , navTab st.tab Performance "Performance"
      , divider
      , HH.span [ HP.style labelStyle ] [ HH.text "KEY" ]
      , HH.slot (Proxy :: _ "keySelect") unit Select.component
          ((Select.defaultInput keyOptions) { selected = Just (show st.key.tonic), placeholder = "Key" })
          \(Select.Selected v) -> SelectKey v
      , HH.span [ HP.style (labelStyle <> " margin-left: 8px;") ] [ HH.text "SCALE" ]
      , HH.slot (Proxy :: _ "scaleSelect") unit Select.component
          ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue st.key.mode), searchable = true })
          \(Select.Selected v) -> SelectScale v
      ]
      <> familyPicker
      <> (if st.tab == Lab then dropButtons <> borrowPicker else [])
      <> [ HH.span
             [ HP.style "font-size: 12px; color: #9a9a9a; margin-left: auto;" ]
             [ HH.text ("MIDI: " <> st.midiName) ]
         ]
    )
  where
  labelStyle = "font-size: 12px; color: #6a6a6a;"
  divider = HH.span [ HP.style "width: 1px; align-self: stretch; background: #e6e6e6; margin: 2px 4px;" ] []
  -- the exterior signpost buttons: drop a curated chord set onto the outer rings
  -- (toggle to remove). Active = amber, matching the ring-index warmth.
  dropButtons = [ divider ] <> map dropBtn exteriorGens
  dropBtn g =
    HH.button
      [ HP.style (dropBtnStyle (Map.member g.key st.dropped)), HE.onClick \_ -> DropSet g.key ]
      [ HH.text g.label ]
  dropBtnStyle active =
    "border: 1px solid " <> (if active then "#c9a23a" else "#dcdcdc")
      <> "; background: " <> (if active then "#fbf3df" else "#fafafa")
      <> "; color: " <> (if active then "#7a5c00" else "#6a6a6a")
      <> "; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px; margin-left: 4px;"
  -- modal interchange: borrow chromatic chords from a parallel mode (explicit
  -- "borrow from C ‹mode›"); the home tonic stays, the source mode is chosen here.
  borrowPicker =
    [ HH.span [ HP.style (labelStyle <> " margin-left: 8px;") ] [ HH.text "BORROW" ]
    , HH.slot (Proxy :: _ "borrowSelect") unit Select.component
        ((Select.cascadingInput borrowGroups) { selected = Just (fromMaybe "off" st.borrowMode), searchable = true })
        \(Select.Selected v) -> BorrowFrom v
    ]
  -- a contextual scale picker for the focused family (click a keyboard key to
  -- focus one) — this is what lets two families hold different modes at once.
  familyPicker = case st.focusedFamily >>= (\sid -> find (\c -> c.id == sid) st.chords) of
    Just seed ->
      let famMode = (fromMaybe st.key (Map.lookup seed.id st.familyScale)).mode
      in [ divider
         , HH.span [ HP.style (labelStyle <> " color: #7a5c00;") ] [ HH.text ("FAMILY " <> noteName seed.root) ]
         , HH.slot (Proxy :: _ "familyScaleSelect") unit Select.component
             ((Select.cascadingInput modeGroups) { selected = Just (currentModeValue famMode), searchable = true })
             \(Select.Selected v) -> ReflavourFamily v
         ]
    Nothing -> []

navTab :: forall m. Tab -> Tab -> String -> H.ComponentHTML Action Slots m
navTab active t label =
  HH.button
    [ HP.style (btnStyle (t == active)), HE.onClick \_ -> SetTab t ]
    [ HH.text label ]
  where
  btnStyle isActive =
    "border: none; background: none; cursor: pointer; padding: 4px 10px; font-size: 13px; "
      <> if isActive then "color: #1a1a1a; border-bottom: 2px solid #1a1a1a; font-weight: 600;"
                     else "color: #9a9a9a; border-bottom: 2px solid transparent;"

helpText :: Tab -> String
helpText = case _ of
  Lab ->
    "One triad per scale degree. Click a piano key to focus that root — a beam lights its column. Stack notes on the focused chord: number keys 2–7 add an interval that many steps up (3 = a third, so 3·3·3 climbs a seventh; 2·4 makes a sus2), e adds the next third (e·e = seventh), s drops the suspensions; press l to explode its whole lattice at once (l again to collapse). The McMullen button drops a curated signpost palette and BORROW pulls chromatic chords from a parallel mode (modal interchange) — chords the scale-pure lattice can't reach, floating up over their own roots and shaded warmer the further outside the chosen scale they sit. Hover any chord + space to hear it. Hover a chord and press v to REVOICE it — a modal with its pitch ladder (drag a note by octaves, ⌥ to double), Tab to cycle voicings, ↑↓ to nudge a voice, f to keep one, and a slash row to re-foot the bass; Esc closes. Click any chord — triads included — to grow the progression on the right: same family bridges by the shortest single-note walk (gold); a chord in another family leaps across as an interconnector (dashed violet). Chromatic keys summon borrowed roots (modulation). On the right: click a step to hear it (shift-click one or two to offer chords to add), then Tab / Shift-Tab cycles its voicings, ↑/↓ nudges a clicked voice, drag a note to move it by octaves (⌥-drag to double it); ▶ plays the whole thing, c clears it. The Tidal source tracks it live — copy to save, paste + Load to work on a saved one again. “save → library” stores it for the Performance tab."
  Performance ->
    "Load a saved progression and fan it to VOICES. The chords are ROWS; each voice is a COLUMN; every cell is how many BARS that voice dwells on that chord — click a cell to bump it up (shift-click down), 0 = skip. So a block voice can hold one chord for four bars while an arp runs every chord at one bar each, and any voice can sit out any chord. Each column header sets its renderer (block / strum = only new notes / arp), MIDI channel and phase offset; voices loop their own columns independently, so different totals drift them apart. ▶ runs the transport. Click a chord row to select it, then Tab / ↑↓ revoices it LIVE without changing the saved version."

surface :: forall m. State -> H.ComponentHTML Action Slots m
surface st
  | st.tab == Lab && length st.genSel > 0 && length st.candidates > 0 = pickSurface st
  | otherwise =
  let scl = scaleSet st.key
      posMap = Map.fromFoldable (map (\n -> Tuple n.id { x: n.x, y: n.y }) st.nodes)
      links = latticeLinkLines posMap st.chords
      -- the path overlay: chord id → 1-based step in the running sequence
      pathOrder = Map.fromFoldable (mapWithIndex (\i pid -> Tuple pid (i + 1)) st.path)
      pathEdges = pathLinkLines posMap (Path.adjacency (neighborLinks st.chords)) st.path
      -- the focused root (from a keyboard-key click) lights a beam up its column
      focusRoot = st.focusedFamily >>= \fid -> map _.root (find (\c -> c.id == fid) st.chords)
      -- a faint divider marking the OUTSIDE shelf — only when outside chords exist
      shelfMarker =
        if any (\c -> c.outside > 0) st.chords
          then [ SE.line [ SA.x1 (-436.0), SA.y1 (-80.0), SA.x2 440.0, SA.y2 (-80.0), SA.class_ (cn "shelf-line") ]
               , SE.text [ SA.x (-430.0), SA.y (-86.0), SA.class_ (cn "shelf-label") ] [ HH.text "OUTSIDE THE SCALE ↑" ]
               ]
          else []
  in SE.svg
      ( [ SA.viewBox (-440.0) (-300.0) 880.0 600.0
        , SA.width 880.0
        , SA.height 600.0
        , SA.class_ (cn "vetula-surface")
        -- no left ladder on the Lab surface — let the cloud fill the wide window.
        , HP.style "max-width: none;"
        ]
          -- only listen for moves while a ladder voice is actually being dragged
          <> (case st.drag of
                Just _ ->
                  [ HE.onMouseMove (DragMove <<< ME.toEvent)
                  , HE.onMouseUp \_ -> DragEnd
                  , HE.onMouseLeave \_ -> DragEnd
                  ]
                Nothing -> [])
      )
      ( [ cloudClipDef st.tab
        , clippedCloud
            ( focusBeam focusRoot <> keyboardView scl <> axisLabels st.tab <> shelfMarker <> links <> pathEdges
                <> map (nodeView scl pathOrder Set.empty posMap)
                     (filter (\c -> not (Set.member c.id st.imported)) st.chords) )
        ] <> revoiceModal st )

-- | The clip region for the chord cloud. On Explore it stops at the pitch
-- | ladder's edge (x −304) so a dense beeswarm can't paint over the ladder; on
-- | the Lattice there's no ladder, so it spans almost the full width (x −436),
-- | reclaiming the old ladder strip for left-rooted families.
cloudClipDef :: forall m. Tab -> H.ComponentHTML Action Slots m
cloudClipDef _ =
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
  if st.tab == Lab && length st.genSel > 0
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
     , pinned: false, outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false }

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
      [ HP.style "border-left: 1px solid #ededed; padding-left: 14px; max-height: 620px; overflow-y: auto;" ]
      [ HH.div
          [ HP.style "display: flex; align-items: center; gap: 10px; margin: 2px 0 8px;" ]
          [ HH.span
              [ HP.style "font-size: 12px; color: #6a6a6a; letter-spacing: 0.06em; text-transform: uppercase;" ]
              [ HH.text "Progression" ]
          , HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ]
              [ HH.text (show (length steps) <> (if length steps == 1 then " step" else " steps")) ]
          , HH.button
              [ HP.style "margin-left: auto; border: 1px solid #b8860b; background: #fbf6e9; color: #7a5c00; cursor: pointer; padding: 3px 12px; border-radius: 4px; font-size: 12px; font-weight: 600;"
              , HE.onClick \_ -> PlayPath
              ]
              [ HH.text "▶ preview" ]
          ]
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
      , tidalExport st.sourceOpen st.sourceEdit (groupLabel st.key) steps
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
        , SA.class_ (cn ("ladder-dot ladder-dot--" <> show (mod j 5)
                         <> (if j == 0 then "" else " ladder-dot--drag")
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

-- ---------------------------------------------------------------------------
-- The Performance tab — load a saved progression, fan it to voices
-- ---------------------------------------------------------------------------

-- | The Performance tab: either the library (when nothing's loaded) or the
-- | loaded progression's transport + voices rack. Isolated from the lab.
performanceView :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
performanceView st = case st.perfProg of
  Nothing -> libraryView st
  Just pp -> loadedView st pp

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
  in HH.div [ HP.style "max-width: 720px; padding: 6px 0;" ]
      [ HH.div [ HP.style "display: flex; align-items: center; gap: 12px; margin: 0 0 12px;" ]
          [ HH.span [ HP.style "font-size: 13px; color: #6a6a6a; letter-spacing: 0.06em; text-transform: uppercase;" ] [ HH.text "Library" ]
          , HH.input
              [ HP.value st.libSearch, HE.onValueInput SetLibSearch, HP.attr (AttrName "placeholder") "search by key…"
              , HP.style "font-size: 13px; padding: 3px 8px; border: 1px solid #ddd; border-radius: 4px; width: 200px;" ]
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

-- | The loaded progression: transport + the chords (revoiceable live) + the
-- | voices rack (each a read-head into the progression on its own clock).
loadedView :: forall m. MonadAff m => State -> { name :: String, chordIds :: Array Int } -> H.ComponentHTML Action Slots m
loadedView st pp =
  let chords = perfChords st
  in HH.div [ HP.style "max-width: 900px; padding: 6px 0;" ]
      [ HH.div [ HP.style "display: flex; align-items: center; gap: 12px; margin: 0 0 12px;" ]
          [ HH.button [ HP.style "border: 1px solid #d8d8d8; background: #fafafa; cursor: pointer; padding: 3px 10px; border-radius: 4px; font-size: 12px; color: #6a6a6a;", HE.onClick \_ -> UnloadProg ] [ HH.text "← library" ]
          , HH.span [ HP.style "font-size: 14px; font-weight: 600; color: #2a2a2a;" ] [ HH.text pp.name ]
          -- Control-surface refinement: ARM now lives on the VETULA tab's dot in the
          -- top switcher; no manual → RIG / → BRUSH push buttons (ATLANTIS auto-syncs).
          , numField "bpm" st.tempo SetTempo
          , numField "preview ch" st.previewChan SetPreviewChan
          ]
      -- the PROGRESSION: chords as rows (pitch-ladder + label), the live playhead
      -- lighting the active chord. Its READ-HEADS live below, as Tidal patterns.
      , HH.div [ HP.style "display: flex; align-items: baseline; gap: 12px; margin: 8px 0 6px;" ]
          [ HH.span [ HP.style "font-size: 12px; color: #6a6a6a; letter-spacing: 0.06em; text-transform: uppercase;" ] [ HH.text "Progression" ]
          , HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ] [ HH.text "chords numbered 0-based (the indices you type below) · click to select · Tab / ↑↓ revoice live" ]
          ]
      , HH.table [ HP.style "border-collapse: collapse;" ]
          (mapWithIndex (gridChordRow st (length chords)) chords)
      -- the PLAYHEADS: one live-coded Tidal read-head per voice.
      , HH.div [ HP.style "display: flex; align-items: baseline; gap: 12px; margin: 18px 0 6px;" ]
          [ HH.span [ HP.style "font-size: 12px; color: #6a6a6a; letter-spacing: 0.06em; text-transform: uppercase;" ] [ HH.text "Playheads" ]
          , HH.span [ HP.style "font-size: 11px; color: #b0b0b0;" ] [ HH.text "commit applies both boxes · empty ♪ = block/arp/strum renderer" ]
          , cellBtn "+ add voice" false AddVoice
          ]
      -- column header for the two pattern boxes (roughly aligned over them).
      , HH.div [ HP.style "display: flex; align-items: center; gap: 6px; padding: 0 6px 2px; font-size: 10px; letter-spacing: 0.04em; text-transform: uppercase; color: #b8b8b8;" ]
          [ HH.div [ HP.style "flex: 0 0 210px;" ] [ HH.text "voice" ]
          , HH.div [ HP.style "flex: 1 1 auto; min-width: 120px;" ] [ HH.text "read-head — which chord, when" ]
          , HH.span [ HP.style "flex: 0 0 auto; visibility: hidden;" ] [ HH.text "♪" ]
          , HH.div [ HP.style "flex: 1 1 auto; min-width: 110px;" ] [ HH.text "♪ notes — which note (0 = lowest · -1 = top)" ]
          , HH.div [ HP.style "flex: 0 0 130px;" ] []
          ]
      -- keyed by voice id: the pattern inputs are UNCONTROLLED (so the scheduler's
      -- ~10Hz re-render can't fight the caret while you live-code), so their DOM must
      -- stay pinned to their voice across add / remove — the key guarantees that.
      , HH.keyed (ElemName "div") [ HP.style "display: flex; flex-direction: column; gap: 6px;" ]
          (map (\v -> Tuple (show v.id) (voicePlayheadRow (length chords) v)) st.voices)
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
  in HH.div
      [ HP.style ("display: flex; align-items: center; gap: 6px; padding: 4px 6px; border: 1px solid #eee; border-radius: 5px; background: #fbfbfa;"
          <> (if v.muted && v.dest == ToMidi then " opacity: 0.5;" else "")) ]
      ( [ cellBtn (destName v.dest) (v.dest == ToOdonus) (CycleVoiceDest v.id) ]
          <> (case v.dest of
                ToMidi ->
                  [ cellBtn (if v.muted then "off" else "on") (not v.muted) (ToggleVoiceMute v.id)
                  , cellBtn (rendName v.renderer) true (CycleVoiceRenderer v.id)
                  , numField "ch" v.channel (SetVoiceChannel v.id)
                  ]
                ToOdonus ->
                  [ numField "id" v.channel (SetVoiceChannel v.id) ])
          <>
          [ HH.input
              -- controlled (like the bpm field, which edits fine); the field carries the
              -- REAL pattern text. Placeholder only shows if you clear it, and reads as a
              -- hint ("empty = …"), not as content.
              [ HP.value v.patternDraft
              , HP.placeholder ("empty = " <> defaultPattern n)
              , HP.style ("flex: 1 1 auto; min-width: 120px; font-family: ui-monospace, monospace; font-size: 12px; padding: 4px 6px; border-radius: 4px; border: 1px solid " <> borderCol <> ";")
              , HE.onValueInput (SetVoicePattern v.id)
              ]
          , cellBtn (RA.articLabel v.articulator) (v.articulator /= RA.ABlock) (CycleVoiceArticulator v.id)
          , HH.span [ HP.style "font-size: 12px; color: #c0c0c0;" ] [ HH.text "♪" ]
          , HH.input
              -- Axis B: how to sound the chord — a note-index pattern. Empty = the
              -- renderer (block/arp/strum). "0 1 2 3" arp · "3" top voice · "3 2 1 0" down.
              -- The button to the left picks the note ALPHABET both this pattern AND the
              -- renderer use: block = the chord's own notes; voice-led = a fixed-N line
              -- carried through the loop (0=bass, -1=melody, strum's common tones ring);
              -- entering = just the notes new to each chord (arp the newcomers in).
              [ HP.value v.notePatternDraft
              , HP.placeholder ("empty = " <> rendName v.renderer)
              , HP.style ("flex: 1 1 auto; min-width: 110px; font-family: ui-monospace, monospace; font-size: 12px; padding: 4px 6px; border-radius: 4px; border: 1px solid " <> noteBorderCol <> ";")
              , HE.onValueInput (SetVoiceNotePattern v.id)
              ]
          , cellBtn "commit" dirty (CommitVoicePattern v.id)
          , status
          , HH.button [ HP.style "border: none; background: none; cursor: pointer; color: #c8c8c8; font-size: 14px;", HE.onClick \_ -> RemoveVoice v.id ] [ HH.text "×" ]
          ] )

-- | A grid row for one chord: the left info cell (number + pitch-row + label,
-- | clickable to select for live Tab-revoicing). Read-heads are the Playheads list.
gridChordRow :: forall m. State -> Int -> Int -> ChordNode -> H.ComponentHTML Action Slots m
gridChordRow st _ i c =
  let active = Just c.id == st.sounding
      onHead = any (\v -> not v.muted && v.cursor == i) st.voices
      rowBg = if active then "#f1efe7" else if onHead && st.playing then "#eef4ee" else "transparent"
  in HH.tr [ HP.style ("background: " <> rowBg <> ";") ]
      ( [ HH.td
            [ HP.style "padding: 0 8px 0 0; cursor: pointer; white-space: nowrap;"
            , HE.onClick \_ -> SelectPerfChord c.id ]
            [ HH.div [ HP.style "display: flex; align-items: center; gap: 8px;" ]
                [ HH.span [ HP.style ("flex: 0 0 18px; text-align: right; font-size: 11px; font-weight: 600; " <> (if active then "color: #7a5c00;" else "color: #b0b0b0;")) ] [ HH.text (show i) ]
                , SE.svg [ SA.viewBox 0.0 0.0 prowW prowH, HP.style "width: 300px; height: auto; display: block;" ]
                    (map (\m -> SE.line [ SA.x1 (prowPitchX m), SA.y1 4.0, SA.x2 (prowPitchX m), SA.y2 (prowH - 4.0), SA.class_ (cn "prow-oct") ]) [ 36, 48, 60, 72, 84 ]
                      <> mapWithIndex (\j m -> SE.circle [ SA.cx (prowPitchX m), SA.cy (prowH / 2.0), SA.r 4.5, SA.class_ (cn ("ladder-dot ladder-dot--" <> show (mod j 5))) ]) (playNotes c))
                , HH.span [ HP.style "flex: 0 0 26px; font-size: 10px; color: #b0b0b0;" ] [ HH.text c.label ]
                ]
            ]
        ] )

-- | One grid cell: how many bars voice `v` dwells on chord `i`. Click bumps it up
-- | (wrapping at 8), shift-click down; 0 shows as a faint dot (skipped). Lights up
-- | green when this voice's playhead is currently on this chord.
durCell :: forall m. State -> Int -> Int -> Voice -> H.ComponentHTML Action Slots m
durCell st n i v =
  let d = fromMaybe 1 (index (padDurs n v.durs) i)
      isCur = st.playing && not v.muted && v.cursor == i && d > 0
  in HH.td
      [ HP.style ("text-align: center; min-width: 52px; padding: 5px 0; cursor: pointer; border-left: 1px solid #f0f0f0; font-size: 12px; "
          <> (if isCur then "background: #dcebd9; " else "")
          <> (if d == 0 then "color: #d4d4d4;" else "color: #3a3a3a; font-weight: 600;")
          <> (if v.muted then " opacity: 0.45;" else ""))
      , HE.onClick \ev -> BumpCell v.id i (ME.shiftKey ev) ]
      [ HH.text (if d == 0 then "·" else show d) ]

-- | A voice's column header. A destination toggle (→ midi / → odo) leads; a MIDI
-- | voice then shows mute + renderer + channel, an Odonus voice just its id (it's
-- | always on, sounds no MIDI of its own). Phase + remove are common to both.
voiceHeaderCell :: forall m. Voice -> H.ComponentHTML Action Slots m
voiceHeaderCell v =
  HH.td
    [ HP.style ("padding: 4px 6px; border-left: 1px solid #f0f0f0; vertical-align: bottom; min-width: 52px; "
        <> (if v.muted && v.dest == ToMidi then "opacity: 0.5;" else "")) ]
    [ HH.div [ HP.style "display: flex; flex-direction: column; gap: 3px; align-items: stretch;" ]
        ( [ cellBtn (destName v.dest) (v.dest == ToOdonus) (CycleVoiceDest v.id) ]
            <> (case v.dest of
                  ToMidi ->
                    [ cellBtn (if v.muted then "off" else "on") (not v.muted) (ToggleVoiceMute v.id)
                    , cellBtn (rendName v.renderer) true (CycleVoiceRenderer v.id)
                    , numField "ch" v.channel (SetVoiceChannel v.id)
                    ]
                  ToOdonus ->
                    [ numField "id" v.channel (SetVoiceChannel v.id) ])
            <> [ numField "φ" v.phase (SetVoicePhase v.id)
               , HH.button [ HP.style "border: none; background: none; cursor: pointer; color: #c8c8c8; font-size: 14px; align-self: center;", HE.onClick \_ -> RemoveVoice v.id ] [ HH.text "×" ]
               ] )
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
      , SA.class_ (cn ("ladder-dot ladder-dot--" <> show (mod i 5)
                       <> (if i == 0 then "" else " ladder-dot--drag")
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
revoiceModal :: forall m. State -> Array (H.ComponentHTML Action Slots m)
revoiceModal st = case st.revoicing >>= \cid -> find (\c -> c.id == cid) st.chords of
  Nothing -> []
  Just c ->
    let tones = sort (nub c.pcs)
    in [ SE.rect
           [ SA.x (-440.0), SA.y (-300.0), SA.width 880.0, SA.height 600.0
           , SA.class_ (cn "rv-backdrop"), HE.onClick \_ -> CloseRevoice ]
       , SE.rect
           [ SA.x (-452.0), SA.y (-300.0), SA.width 218.0, SA.height 600.0
           , SA.class_ (cn "rv-drawer") ]
       ]
       <> voicingStrip (Just c) st.favorites
       <> ladderView st.selected (Just c)
       <> [ SE.text [ SA.x (-343.0), SA.y 222.0, SA.class_ (cn "rv-title") ]
              [ HH.text (noteName c.root <> " · revoice") ]
          , SE.text [ SA.x (-447.0), SA.y 250.0, SA.class_ (cn "rv-bass-label") ] [ HH.text "bass /" ]
          ]
       <> mapWithIndex (slashBtn c.bassPc) tones
       <> [ SE.text [ SA.x (-343.0), SA.y 282.0, SA.class_ (cn "rv-hint") ]
              [ HH.text "Tab voicings · ↑↓ nudge · drag = 8ve · ⌥ doubles · f keep · Esc" ] ]
  where
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

axisLabels :: forall m. Tab -> Array (H.ComponentHTML Action Slots m)
axisLabels _ =
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
      -- a plain click on ANY chord grows the progression — triads included
      -- (the bare-triad-can't-be-pathed defect is gone now that explode lives
      -- on the `l` key, not on a seed click).
      , HE.onClick \_ -> PathPick c.id
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
