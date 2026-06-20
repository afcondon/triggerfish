-- | The Odonus grid + four-head bank, Hainbach dress. Quartered pads (value knob
-- | + glide/gate/skip lamps) over a bank of four playheads; each head carries a
-- | René-style access **pattern** shown as a small-multiple thumbnail beside its
-- | direction / speed / interval knobs. Aesthetic: Swiss rigor × vintage-lab
-- | materiality (see BRIEF.md).
module Triggerfish.Odonus.Grid (component) where

import Prelude

import Data.Array (concatMap, deleteAt, elem, filter, findIndex, length, mapWithIndex, null, range, (!!))
import Data.Foldable (for_)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (joinWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Binnacle (Binnacle)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Triggerfish.Scale as Scale
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

data KnobTarget
  = CellNote Int
  | HeadDir Int
  | HeadSpeed Int
  | HeadTransp Int
  | HeadOffset Int
  | HeadLen Int
  | Spread

targetRange :: KnobTarget -> { lo :: Int, hi :: Int }
targetRange = case _ of
  CellNote _ -> { lo: 36, hi: 84 }
  HeadDir _ -> { lo: 0, hi: 2 }
  HeadSpeed _ -> { lo: 0, hi: length M.speedTable - 1 }
  HeadTransp _ -> { lo: -24, hi: 24 }
  HeadOffset _ -> { lo: 0, hi: 15 }
  HeadLen _ -> { lo: 1, hi: 16 }
  Spread -> { lo: 1, hi: 12 }

applyTarget :: KnobTarget -> Int -> M.Odonus -> M.Odonus
applyTarget t v = case t of
  CellNote i -> M.setNote i v
  HeadDir h -> M.setHeadDir h v
  HeadSpeed h -> M.setHeadSpeedIx h v
  HeadTransp h -> M.setHeadTransp h v
  HeadOffset h -> M.setHeadOffset h v
  HeadLen h -> M.setHeadLen h v
  Spread -> M.setSpread v

type DragState = { target :: KnobTarget, startY :: Int, startVal :: Int }

-- | One emitted note in the scrolling monitor. `fireUnixMicros` is the
-- | wall-clock instant it sounds; the river positions it by how long ago
-- | that was (so the visual onset lands exactly on the audio onset).
type NoteEvent = { pitch :: Int, headIdx :: Int, fireUnixMicros :: Number }

-- | A saved whole-Odonus setting: notes, heads, scale — the unit of
-- | composition. Sequencing scenes builds flowing fugues with key changes
-- | and voices dropping in and out.
type Scene = { name :: String, odo :: M.Odonus }

type State =
  { odo :: M.Odonus
  , running :: Boolean
  , dragging :: Maybe DragState
  , dragSub :: Maybe H.SubscriptionId
  , notes :: Array NoteEvent
  , binnacle :: Maybe Binnacle
  , nowMicros :: Number
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , scenes :: Array Scene
  , chain :: Boolean        -- auto-advance scenes at bar boundaries
  , sceneIx :: Int          -- current scene in the chain
  , sceneBarAnchor :: Int   -- bar at which the current scene started
  , barsPerScene :: Int
  , stepDiv :: Int          -- global clock divider (1=1/16 .. 16=whole note)
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ToggleRun
  | ToggleGlide Int
  | ToggleGate Int
  | ToggleSkip Int
  | ToggleHeadMute Int
  | CyclePattern Int
  | UnifyHeads
  | CycleScaleType Int
  | ToggleDist
  | SetRoot Int
  | SetOctave Int
  | SetDegShift Int
  | ToggleScaleNote Int
  | CaptureScene
  | RecallScene Int
  | DeleteScene Int
  | ToggleChain
  | BumpBars Int
  | SetStepDiv Int
  | KnobDown KnobTarget Int
  | DragMove Int
  | DragEnd

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { odo: M.defaultOdonus, running: false, dragging: Nothing, dragSub: Nothing
        , notes: [], binnacle: Nothing, nowMicros: 0.0
        , midiOut: Nothing, midiName: "…", clockTempo: 120.0, clockLocked: false
        , clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , scenes: [], chain: false, sceneIx: 0, sceneBarAnchor: 0, barsPerScene: 4
        , stepDiv: 1 }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Connect to the rig. Binnacle's clock free-runs at 120 until the
    -- Link anchor arrives, then phase-locks — so Triggerfish runs solo
    -- without the rig, and joins the ensemble the moment it's up.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    -- The lookahead scheduler drives both model advance and audio.
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (Step tick)
    -- A separate ~30fps ticker scrolls the river smoothly.
    frameE <- frameTimer
    _ <- H.subscribe frameE
    -- Web MIDI out (async permission prompt) — e.g. an IAC bus into Ableton.
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
      Nothing -> HS.notify midiL (MidiReady Nothing "unavailable")
    H.modify_ _ { binnacle = Just bin }
  Step tick -> do
    st <- H.get
    -- Global step divider: the scheduler ticks on a fine 1/16 grid; advance the
    -- model only every stepDiv ticks, so STEP LENGTH sets what a 1× head plays.
    when (st.running && tick.index `mod` st.stepDiv == 0) do
      let r = M.stepEmit st.odo
      -- Each unmuted head that lands on a gated cell schedules a MIDI note
      -- on its own channel (head I → ch 1 …), delivered at the scheduler's
      -- precise fire-time so it's jitter-immune.
      case st.midiOut of
        Just out -> liftEffect $ for_ r.fired \f -> do
          -- Glide → portamento on + a short glide time on this head's
          -- channel; non-glide notes turn portamento off. (Needs a glide-
          -- capable synth in Ableton; harmless otherwise. The es9 path maps
          -- glide to cv-slew instead.)
          if f.glide
            then do
              Midi.sendCC out { channel: f.headIdx, controller: 65, value: 127 }
              Midi.sendCC out { channel: f.headIdx, controller: 5, value: 40 }
            else Midi.sendCC out { channel: f.headIdx, controller: 65, value: 0 }
          Midi.scheduleNote out
            { channel: f.headIdx, note: f.pitch, velocity: 100
            , delayMs: tick.delayMs, durMs: 160.0 }
        Nothing -> pure unit
      let fresh = map (\f -> { pitch: f.pitch, headIdx: f.headIdx
                             , fireUnixMicros: tick.fireUnixMicros }) r.fired
      H.modify_ \s -> s { odo = r.odo, notes = fresh <> s.notes }
  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        H.modify_ \s ->
          let
            base = s
              { nowMicros = now
              , clockTempo = r.tempo
              , clockLocked = r.locked
              , clockBeat = r.beat
              , clockBar = r.bar
              , anchorCount = r.anchorCount
              , notes = filter (\n -> (now - n.fireUnixMicros) < windowMicros) s.notes
              }
            -- Chain mode: advance to the next scene once barsPerScene bars have
            -- elapsed, carrying playhead phase across the swap.
            advance = s.chain && not (null s.scenes)
              && (r.bar - s.sceneBarAnchor) >= s.barsPerScene
          in
            if advance then
              let ni = (s.sceneIx + 1) `mod` length s.scenes
              in case s.scenes !! ni of
                Just sc -> base
                  { odo = M.recallScene s.odo sc.odo, sceneIx = ni, sceneBarAnchor = r.bar }
                Nothing -> base
            else base
      Nothing -> pure unit
  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }
  ToggleRun -> H.modify_ \s -> s { running = not s.running }
  ToggleGlide i -> H.modify_ \s -> s { odo = M.toggleGlide i s.odo }
  ToggleGate i -> H.modify_ \s -> s { odo = M.toggleGate i s.odo }
  ToggleSkip i -> H.modify_ \s -> s { odo = M.toggleSkip i s.odo }
  ToggleHeadMute h -> H.modify_ \s -> s { odo = M.toggleHeadMute h s.odo }
  CyclePattern h -> H.modify_ \s -> s { odo = M.cyclePattern h s.odo }
  UnifyHeads -> H.modify_ \s -> s { odo = M.unifyHeads s.odo }
  CycleScaleType dir -> H.modify_ \s -> s { odo = M.cycleScaleType dir s.odo }
  ToggleDist -> H.modify_ \s -> s { odo = M.toggleDistribution s.odo }
  SetRoot pc -> H.modify_ \s -> s { odo = M.setRoot pc s.odo }
  SetOctave n -> H.modify_ \s -> s { odo = M.setOctaveShift n s.odo }
  SetDegShift n -> H.modify_ \s -> s { odo = M.setDegShift n s.odo }
  ToggleScaleNote pc -> H.modify_ \s -> s { odo = M.toggleScaleNote pc s.odo }
  CaptureScene -> H.modify_ \s ->
    s { scenes = s.scenes <> [ { name: sceneName s, odo: s.odo } ] }
  RecallScene i -> H.modify_ \s -> case s.scenes !! i of
    Just sc -> s { odo = M.recallScene s.odo sc.odo, sceneIx = i }
    Nothing -> s
  DeleteScene i -> H.modify_ \s -> s { scenes = fromMaybe s.scenes (deleteAt i s.scenes) }
  ToggleChain -> H.modify_ \s -> s { chain = not s.chain, sceneBarAnchor = s.clockBar }
  BumpBars d -> H.modify_ \s -> s { barsPerScene = clampI 1 32 (s.barsPerScene + d) }
  SetStepDiv d -> H.modify_ \s -> s { stepDiv = d }
  KnobDown target startVal -> do
    sid <- setupDrag
    H.modify_ _ { dragging = Just { target, startY: 0, startVal }, dragSub = Just sid }
  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just drag
        | drag.startY == 0 ->
            H.modify_ _ { dragging = Just drag { startY = clientY } }
        | otherwise -> do
            let
              r = targetRange drag.target
              delta = round (toNumber (drag.startY - clientY) * toNumber (r.hi - r.lo) / 140.0)
              newVal = clampI r.lo r.hi (drag.startVal + delta)
            H.modify_ \s -> s { odo = applyTarget drag.target newVal s.odo }
      _ -> pure unit
  DragEnd -> do
    st <- H.get
    case st.dragSub of
      Just sid -> H.unsubscribe sid
      Nothing -> pure unit
    H.modify_ _ { dragging = Nothing, dragSub = Nothing }

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

-- | The rig WebSocket (purerl-tidal). Binnacle subscribes to the Link
-- | anchor here and relays gates/CV to es9-daemon.
rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Odonus step = a 16th note; schedule ~120ms ahead, poll at 25ms.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | MIDI output port (substring match). On macOS enable the IAC Driver in
-- | Audio MIDI Setup and receive this bus in Ableton; each head sends on
-- | its own channel (I→1 … IV→4). (The es9 modular path lives in
-- | Binnacle.Output for when the rig is patched.)
midiPortName :: String
midiPortName = "IAC"

-- | Drop monitor notes once they've scrolled off the left edge (~8s).
windowMicros :: Number
windowMicros = 8000000.0

-- | ~30fps UI ticker that scrolls the scope + refreshes the clock readout.
-- | Driven by `setInterval` (a browser timer) rather than a forked Aff
-- | `forever` loop — the latter can silently fail to run for the component's
-- | life (the symptom: audio tracked Link via the scheduler's own
-- | setInterval while this loop was dead, freezing the whole UI). This is the
-- | same robust mechanism Scheduler.startGrid uses.
frameTimer :: forall m. MonadAff m => m (HS.Emitter Action)
frameTimer = liftEffect do
  { emitter, listener } <- HS.create
  _ <- setInterval 33 (HS.notify listener Frame)
  pure emitter

setupDrag :: forall o m. MonadAff m => H.HalogenM State Action () o m H.SubscriptionId
setupDrag =
  H.subscribe $ HS.makeEmitter \emit -> do
    moveFn <- eventListener \e -> case ME.fromEvent e of
      Just me -> emit (DragMove (ME.clientY me))
      Nothing -> pure unit
    upFn <- eventListener \_ -> emit DragEnd
    target <- Window.toEventTarget <$> window
    addEventListener (EventType "mousemove") moveFn false target
    addEventListener (EventType "mouseup") upFn false target
    pure do
      removeEventListener (EventType "mousemove") moveFn false target
      removeEventListener (EventType "mouseup") upFn false target

-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")

svgEl :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr n v = HP.attr (HH.AttrName n) v

headColor :: Int -> String
headColor h = case h `mod` 4 of
  0 -> "#2f5fb0"
  1 -> "#b0492f"
  2 -> "#2f8a5c"
  _ -> "#b07a2f"

roman :: Int -> String
roman = case _ of
  0 -> "I"
  1 -> "II"
  2 -> "III"
  _ -> "IV"

dirName :: Int -> String
dirName = case _ of
  0 -> "FWD"
  1 -> "BCK"
  _ -> "PEND"

speedRatio :: Int -> String
speedRatio ix = maybe "1.0" show (M.speedTable !! ix) <> "×"

signed :: Int -> String
signed n = if n > 0 then "+" <> show n else show n

engrave :: String
engrave = "font-family:Georgia,'Times New Roman',serif;letter-spacing:0.12em;text-transform:uppercase;color:#5a564b"

-- | Fullscreen, no margins: a right→left signal chain of full-height panels,
-- | mirroring the scope's leftward note-flow. SOURCE (eDSL) ← GRID ← PLAYHEADS
-- | ← QUANTIZE ← SCOPE. The grid authors integers, the playheads shift them in
-- | degree-space, the quantizer collapses degrees to pitches, the scope shows
-- | them flowing out the left.
render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;inset:0;display:flex;align-items:stretch;overflow:hidden;"
        <> "background:#b7b1a0;font-family:Georgia,serif" ]
    [ scopePanel s
    , quantizerPanel s
    , playheadsPanel s
    , gridPanel s
    , edslPanel s
    , scenesPanel s
    ]

-- | A pale Hainbach control panel: engraved header + body, full viewport height.
panelShell
  :: forall m
   . String -> String -> String
  -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panelShell label sub widthCss body =
  HH.div
    [ style $ widthCss <> ";height:100vh;box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
        <> "padding:18px 14px;display:flex;flex-direction:column" ]
    ( [ HH.div
          [ style $ engrave <> ";font-size:11px;display:flex;justify-content:space-between;"
              <> "align-items:baseline;margin-bottom:14px;border-bottom:1px solid #00000018;padding-bottom:6px" ]
          [ HH.span [ style "font-size:14px;letter-spacing:0.16em;color:#3f3c33" ] [ HH.text label ]
          , HH.span [ style "font-size:8px" ] [ HH.text sub ]
          ]
      ] <> body )

riverW :: Number
riverW = 380.0

riverH :: Number
riverH = 520.0

pxPerMs :: Number
pxPerMs = 0.05

pitchToY :: Int -> Number
pitchToY pitch = riverH * (1.0 - (toNumber (clampI 24 96 pitch) - 24.0) / 72.0)

-- | The scope — the hero panel on the far left, full height, flex-grow. Octave
-- | gridlines + note labels (HTML, undistorted) under the stretched note SVG;
-- | notes emit at the right edge and flow left, fading with age.
scopePanel :: forall m. State -> H.ComponentHTML Action () m
scopePanel s =
  HH.div
    [ style $ "flex:1 1 360px;min-width:0;height:100vh;position:relative;overflow:hidden;"
        <> "background:radial-gradient(140% 100% at 100% 50%,#15140f,#0b0a07)" ]
    ( octaveGuides
        <>
          [ svgEl "svg"
              [ svgAttr "width" "100%", svgAttr "height" "100%"
              , svgAttr "viewBox" "0 0 380 520", svgAttr "preserveAspectRatio" "none"
              , style "position:absolute;inset:0" ]
              (map (noteBar s.nowMicros) s.notes)
          ]
    )

-- | Faint horizontal line + a "C4"-style label at each octave C (HTML, so the
-- | text isn't stretched by the scope's preserveAspectRatio=none).
octaveGuides :: forall m. Array (H.ComponentHTML Action () m)
octaveGuides = concatMap guide [ 24, 36, 48, 60, 72, 84, 96 ]
  where
  guide pitch =
    let pct = pitchToY pitch / riverH * 100.0
    in
      [ HH.div [ style $ "position:absolute;left:0;right:0;top:" <> show pct
            <> "%;height:1px;background:#ffffff12" ] []
      , HH.div [ style $ "position:absolute;left:7px;top:calc(" <> show pct
            <> "% - 7px);font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#ffffff3a" ]
          [ HH.text ("C" <> show (pitch / 12 - 1)) ]
      ]

noteBar :: forall m. Number -> NoteEvent -> H.ComponentHTML Action () m
noteBar now n =
  let
    elapsedMs = (now - n.fireUnixMicros) / 1000.0
    x = riverW - elapsedMs * pxPerMs - 10.0
  in
    svgEl "rect"
      [ svgAttr "x" (show x)
      , svgAttr "y" (show (pitchToY n.pitch))
      , svgAttr "width" "9", svgAttr "height" "5", svgAttr "rx" "2"
      , svgAttr "fill" (headColor n.headIdx)
      , svgAttr "opacity" (show (max 0.12 (1.0 - elapsedMs / 7000.0)))
      ] []

-- ── QUANTIZE panel — the live pitch lens (scale + distribution) ──────────────

quantizerPanel :: forall m. State -> H.ComponentHTML Action () m
quantizerPanel s =
  panelShell "KEY" "Quantize · Transpose" "flex:0 1 232px;min-width:0"
    [ pcKeyboard s.odo
    , stepperRow "ROOT" (Scale.rootName s.odo.rootPc)
        (SetRoot (s.odo.rootPc - 1)) (SetRoot (s.odo.rootPc + 1))
    , HH.div [ style "display:flex;align-items:flex-end;gap:10px;margin:8px 0" ]
        [ HH.div [ style "flex:1" ]
            [ stepperRow "SCALE" (M.scaleTypeName s.odo) (CycleScaleType (-1)) (CycleScaleType 1) ]
        , spreadBlock s.odo
        ]
    -- OCTAVE: chromatic ± octaves applied to the whole output.
    , labelledRow "OCTAVE"
        (map (\n -> tabBtn (octLabel n) (s.odo.octaveShift == n) (SetOctave n)) [ -2, -1, 0, 1, 2 ])
    -- SCALAR TRANSP: shift the whole pattern by whole scale degrees, in-key.
    , labelledRow "SCALAR TRANSP."
        (map (\i -> tabBtn (romanNum i) (s.odo.degShift == i) (SetDegShift i)) (range 0 6))
    , HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin:10px 0 4px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "MODE" ]
        , HH.button
            [ HE.onClick \_ -> ToggleDist
            , style $ "padding:4px 10px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
                <> "background:linear-gradient(#efece1,#ddd9cb);font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33" ]
            [ HH.text (show s.odo.dist) ]
        ]
    , HH.div [ style $ engrave <> ";font-size:8px;color:#888273;margin-top:2px;line-height:1.5" ]
        [ HH.text (case s.odo.dist of
            Scale.Natural -> "Natural · cells snap to nearest scale tone"
            Scale.Equal -> "Equal · cells index scale degrees from root") ]
    ]

-- | A 12-key chromatic strip: in-scale pitch classes lit, the root accented.
-- | Click a key to toggle it in/out of the scale (direct note choice); the
-- | root is set by the ROOT stepper.
pcKeyboard :: forall m. M.Odonus -> H.ComponentHTML Action () m
pcKeyboard odo =
  let lit = Scale.pitchClassesOf (M.scaleOf odo)
  in
    HH.div [ style "display:flex;gap:2px;margin-bottom:14px" ]
      (map (pcKey odo.rootPc lit) (range 0 11))

pcKey :: forall m. Int -> Array Int -> Int -> H.ComponentHTML Action () m
pcKey rootPc lit pc =
  let
    on = elem pc lit
    isRoot = pc == rootPc
    bg = if isRoot then "#b5832b" else if on then "#8a9b6e" else "#bdb8a7"
    fg = if isRoot || on then "#1c1a12" else "#7d7868"
  in
    HH.div
      [ HE.onClick \_ -> ToggleScaleNote pc
      , style $ "flex:1;height:38px;border-radius:3px;border:1px solid #00000018;cursor:pointer;background:" <> bg
          <> ";display:flex;align-items:flex-end;justify-content:center;padding-bottom:2px" ]
      [ HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:7px;color:" <> fg ]
          [ HH.text (Scale.rootName pc) ] ]

-- | The Marbles-style SPREAD knob: drag to grow the scale from the root
-- | outward (unison → fifth → fourth → … → full chromatic). Value = note count.
spreadBlock :: forall m. M.Odonus -> H.ComponentHTML Action () m
spreadBlock odo =
  let n = length odo.scaleIvls
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:52px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text "SPREAD" ]
      , HH.div [ style "width:40px;height:40px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color: "#8a9b6e", lo: 1, hi: 12, value: n }
              (KnobDown Spread n) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#3f3c33;margin-top:1px" ]
          [ HH.text (show n <> "n") ]
      ]

-- | A label over a row of tab buttons (OCTAVE / SCALAR TRANSP, Xynthesizr-style).
labelledRow :: forall m. String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
labelledRow lbl btns =
  HH.div [ style "margin:8px 0" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;margin-bottom:4px" ] [ HH.text lbl ]
    , HH.div [ style "display:flex;gap:3px" ] btns
    ]

tabBtn :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
tabBtn label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:1;padding:5px 0;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:10px;color:" <> (if active then "#1c1a12" else "#3f3c33")
        <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

octLabel :: Int -> String
octLabel n = if n > 0 then "+" <> show n else show n

romanNum :: Int -> String
romanNum i = fromMaybe (show (i + 1))
  ([ "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" ] !! i)

stepperRow :: forall m. String -> String -> Action -> Action -> H.ComponentHTML Action () m
stepperRow lbl val decA incA =
  HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin:8px 0" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text lbl ]
    , HH.div [ style "display:flex;align-items:center;gap:6px" ]
        [ stepBtn "‹" decA
        , HH.span
            [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;min-width:78px;text-align:center" ]
            [ HH.text val ]
        , stepBtn "›" incA
        ]
    ]

stepBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
stepBtn glyph act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "width:22px;height:22px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:13px;color:#3f3c33;line-height:1" ]
    [ HH.text glyph ]

-- ── PLAYHEADS panel — the extracted Fugue-Machine head bank ──────────────────

playheadsPanel :: forall m. State -> H.ComponentHTML Action () m
playheadsPanel s =
  panelShell "PLAYHEADS" "Fugue · Access" "flex:0 1 290px;min-width:0"
    [ HH.button
        [ HE.onClick \_ -> UnifyHeads
        , style $ "width:100%;padding:6px;margin-bottom:10px;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
            <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:11px;color:#3f3c33" ]
        [ HH.text "≡ Unison · all heads = I" ]
    , headBank s
    ]

-- ── GRID panel — the 16 quartered pads + transport ───────────────────────────

gridPanel :: forall m. State -> H.ComponentHTML Action () m
gridPanel s =
  panelShell "ODONUS" "16 · Cartesian" "flex:0 1 322px;min-width:0"
    [ grid s
    , clockRow s
    , controls s
    , statusBar s
    , nameplate s
    ]

-- | Global step length — what a 1× head plays. Buttons map to the clock
-- | divider (1=whole … 1/16=fast); per-head SPD multiplies from here.
clockRow :: forall m. State -> H.ComponentHTML Action () m
clockRow s =
  labelledRow "STEP LENGTH"
    (map (\d -> tabBtn d.lbl (s.stepDiv == d.div) (SetStepDiv d.div))
      [ { lbl: "1", div: 16 }, { lbl: "½", div: 8 }, { lbl: "¼", div: 4 }
      , { lbl: "⅛", div: 2 }, { lbl: "1/16", div: 1 } ])

-- ── SOURCE panel — the live eDSL of the current setup (read-only) ────────────

edslPanel :: forall m. State -> H.ComponentHTML Action () m
edslPanel s =
  panelShell "SOURCE" "eDSL" "flex:0 1 244px;min-width:0"
    [ HH.div
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10.5px;line-height:1.55;"
            <> "white-space:pre;color:#3a372e;background:#00000008;border:1px solid #00000012;"
            <> "border-radius:6px;padding:10px;overflow-x:auto" ]
        [ HH.text (edslText s.odo) ] ]

-- | The current setup rendered as odonusWith{…} eDSL text. One-directional
-- | (GUI→text), updating live — the consistency-with-text the rig will consume.
edslText :: M.Odonus -> String
edslText o =
  let
    arr f = "[ " <> joinWith ", " (map f o.cells) <> " ]"
    bool b = if b then "T" else "F"
    headLine i hd =
      "    " <> roman i <> "  "
        <> maybe "?" _.name (M.patternLibrary !! hd.patternIx)
        <> "  " <> speedRatio hd.speedIx
        <> " " <> dirName hd.direction
        <> " " <> signed hd.transp
        <> " off " <> show hd.offset
        <> " len " <> show hd.len
        <> (if hd.mute then "  (mute)" else "")
  in
    joinWith "\n"
      ( [ "odonusWith"
        , "  { scale: " <> Scale.scaleName (M.scaleOf o)
        , "  , distribution: " <> show o.dist
        , "  , octave: " <> octLabel o.octaveShift
        , "  , scalarTransp: " <> romanNum o.degShift
        , "  , notes: " <> arr (show <<< _.note)
        , "  , gate:  " <> arr (bool <<< _.gate)
        , "  , skip:  " <> arr (bool <<< _.skip)
        , "  , glide: " <> arr (bool <<< _.glide)
        , "  , heads:"
        ] <> mapWithIndex headLine o.heads <> [ "  }" ] )

-- ── SCENES panel — save whole settings, then sequence them ───────────────────

-- | Auto-name a captured scene by its position + its scale.
sceneName :: State -> String
sceneName s = show (length s.scenes + 1) <> " · " <> Scale.scaleName (M.scaleOf s.odo)

scenesPanel :: forall m. State -> H.ComponentHTML Action () m
scenesPanel s =
  panelShell "SCENES" "Song" "flex:0 1 198px;min-width:0"
    [ HH.button
        [ HE.onClick \_ -> CaptureScene
        , style $ "width:100%;padding:7px;margin-bottom:10px;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
            <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:12px;color:#3f3c33" ]
        [ HH.text "＋ Capture current" ]
    , HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:6px" ]
        [ HH.button
            [ HE.onClick \_ -> ToggleChain
            , style $ "padding:5px 10px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;font-family:Georgia,serif;font-size:11px;color:#3f3c33;background:"
                <> (if s.chain then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
            [ HH.text (if s.chain then "■ Chain" else "▶ Chain") ]
        , HH.div [ style "display:flex;align-items:center;gap:5px" ]
            [ stepBtn "‹" (BumpBars (-1))
            , HH.span [ style $ engrave <> ";font-size:9px;min-width:48px;text-align:center" ]
                [ HH.text (show s.barsPerScene <> " bar" <> (if s.barsPerScene == 1 then "" else "s")) ]
            , stepBtn "›" (BumpBars 1)
            ]
        ]
    , HH.div [ style "display:flex;flex-direction:column;gap:5px;margin-top:8px" ]
        ( if null s.scenes
            then [ HH.div [ style $ engrave <> ";font-size:8px;color:#888273;margin-top:6px" ]
                     [ HH.text "capture a few settings, then chain them" ] ]
            else mapWithIndex (sceneChip s) s.scenes )
    ]

sceneChip :: forall m. State -> Int -> Scene -> H.ComponentHTML Action () m
sceneChip s i sc =
  let active = s.chain && s.sceneIx == i
  in
    HH.div
      [ style $ "display:flex;align-items:center;gap:6px;padding:6px 8px;border-radius:7px;cursor:pointer;"
          <> "background:#cbc6b6;box-shadow:0 0 0 1px " <> (if active then "#b5832b" else "#00000018")
          <> (if active then ";outline:2px solid #b5832b66" else "") ]
      [ HH.div
          [ HE.onClick \_ -> RecallScene i
          , style "flex:1;font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33" ]
          [ HH.text sc.name ]
      , HH.span
          [ HE.onClick \_ -> DeleteScene i
          , style "font-family:Georgia,serif;font-size:11px;color:#a06048;padding:0 3px" ]
          [ HH.text "×" ]
      ]

-- | Format a positive Number to one decimal place (so Link's constant
-- | sub-BPM nudging is visible — the readout flickers when truly locked).
oneDp :: Number -> String
oneDp x =
  let n = round (x * 10.0)
  in show (n `div` 10) <> "." <> show (n `mod` 10)

statusBar :: forall m. State -> H.ComponentHTML Action () m
statusBar s =
  HH.div [ style $ engrave <> ";font-size:8px;margin-top:10px;display:flex;gap:14px;color:#6a6456" ]
    [ HH.span [ style $ "color:" <> (if s.clockLocked then "#2f8a5c" else "#b0492f") ]
        [ HH.text $ "CLOCK " <> oneDp s.clockTempo <> " · "
            <> (if s.clockLocked then "LINK" else "FREE") ]
      -- BEAT climbs iff the frame loop runs and the clock advances.
    , HH.span [] [ HH.text $ "BEAT " <> show (floor s.clockBeat) ]
      -- ANCHORS climbs iff the rig is actually feeding us (the diagnostic).
    , HH.span [] [ HH.text $ "ANCHORS " <> show s.anchorCount ]
    , HH.span [] [ HH.text $ "MIDI " <> s.midiName ]
    ]

grid :: forall m. State -> H.ComponentHTML Action () m
grid s =
  HH.div
    [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin:18px 0" ]
    (mapWithIndex (pad s.odo) s.odo.cells)

headAt :: M.Odonus -> Int -> Maybe { idx :: Int, mute :: Boolean }
headAt o i = case findIndex (\hd -> hd.cursor == i) o.heads of
  Just idx -> Just { idx, mute: maybe false _.mute (o.heads !! idx) }
  Nothing -> Nothing

pad :: forall m. M.Odonus -> Int -> M.Cell -> H.ComponentHTML Action () m
pad odo i c =
  let
    mh = headAt odo i
    ring = maybe "#a79f86" (\r -> headColor r.idx) mh
    glow = case mh of
      Just r -> if r.mute then ",0 0 0 2px " <> ring <> "33" else ",0 0 0 3px " <> ring <> "66"
      Nothing -> ""
  in
    HH.div
      [ style $ "background:#cbc6b6;border-radius:9px;padding:5px;box-shadow:0 0 0 1px " <> ring <> glow
          <> ";display:grid;grid-template-columns:1fr 1fr;grid-template-rows:1fr 1fr;gap:4px;aspect-ratio:1"
      ]
      [ knobQuarter i c
      , lamp "G" c.glide "#4f9d69" (ToggleGlide i)
      , lamp "T" c.gate "#e0a32e" (ToggleGate i)
      , lamp "S" c.skip "#c0563f" (ToggleSkip i)
      ]

knobQuarter :: forall m. Int -> M.Cell -> H.ComponentHTML Action () m
knobQuarter i c =
  HH.div
    [ style "display:flex;flex-direction:column;align-items:center;justify-content:center" ]
    [ HH.div [ style "width:100%;height:100%;min-height:0" ]
        [ knob
            { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 9.0, color: "#b5832b", lo: 36, hi: 84, value: c.note }
            (KnobDown (CellNote i) c.note)
        ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#4a463d;margin-top:1px" ]
        [ HH.text (show c.note) ]
    ]

lamp :: forall m. String -> Boolean -> String -> Action -> H.ComponentHTML Action () m
lamp label on color act =
  HH.div
    [ HE.onClick \_ -> act
    , style $ "display:flex;align-items:center;justify-content:center;gap:3px;cursor:pointer;background:#bfbaa9;border-radius:6px;user-select:none"
    ]
    [ HH.div
        [ style $ "width:8px;height:8px;border-radius:50%;border:1px solid #00000022;background:"
            <> (if on then color else "#46433a")
            <> (if on then ";box-shadow:0 0 5px " <> color else "")
        ] []
    , HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text label ]
    ]

headBank :: forall m. State -> H.ComponentHTML Action () m
headBank s =
  HH.div [ style "display:flex;flex-direction:column;gap:8px" ]
    (mapWithIndex headStrip s.odo.heads)

headStrip :: forall m. Int -> M.Head -> H.ComponentHTML Action () m
headStrip h hd =
  let
    col = headColor h
    dim = if hd.mute then "opacity:0.42;" else ""
    pat = fromMaybe { name: "?", order: [] } (M.patternLibrary !! hd.patternIx)
  in
    HH.div
      [ style $ "display:flex;align-items:center;gap:10px;padding:7px 10px;border-radius:8px;background:#cbc6b6;box-shadow:0 0 0 1px " <> col <> "66;" <> dim ]
      [ muteBlock h hd col
      , patBlock h pat col hd.seqPos
      , HH.div
          [ style "display:grid;grid-template-columns:repeat(3,1fr);gap:6px 4px" ]
          [ miniKnob (HeadDir h) hd.direction col "DIR" (dirName hd.direction)
          , miniKnob (HeadSpeed h) hd.speedIx col "SPD" (speedRatio hd.speedIx)
          , miniKnob (HeadTransp h) hd.transp col "INT" (signed hd.transp)
          , miniKnob (HeadOffset h) hd.offset col "OFF" (show hd.offset)
          , miniKnob (HeadLen h) hd.len col "LEN" (show hd.len)
          ]
      ]

muteBlock :: forall m. Int -> M.Head -> String -> H.ComponentHTML Action () m
muteBlock h hd col =
  HH.div
    [ HE.onClick \_ -> ToggleHeadMute h
    , style "display:flex;flex-direction:column;align-items:center;width:28px;cursor:pointer"
    ]
    [ HH.div
        [ style $ "width:11px;height:11px;border-radius:50%;background:"
            <> (if hd.mute then "#46433a" else col)
            <> (if hd.mute then "" else ";box-shadow:0 0 6px " <> col)
        ] []
    , HH.span [ style $ engrave <> ";font-size:11px;margin-top:2px;color:" <> col ] [ HH.text (roman h) ]
    ]

patBlock :: forall m. Int -> M.Pattern -> String -> Int -> H.ComponentHTML Action () m
patBlock h pat col seqPos =
  HH.div
    [ HE.onClick \_ -> CyclePattern h
    , style "display:flex;flex-direction:column;align-items:center;cursor:pointer;width:54px"
    ]
    [ patternThumb pat col seqPos
    , HH.span [ style $ engrave <> ";font-size:8px;margin-top:1px" ] [ HH.text pat.name ]
    ]

patternThumb :: forall m. M.Pattern -> String -> Int -> H.ComponentHTML Action () m
patternThumb pat color seqPos =
  let
    st = 11.0
    pd = 5.0
    cx g = pd + toNumber (g `mod` 4) * st + st / 2.0
    cy g = pd + toNumber (g / 4) * st + st / 2.0
    pts = joinWith " " (map (\g -> show (cx g) <> "," <> show (cy g)) pat.order)
    g0 = fromMaybe 0 (pat.order !! 0)
    gc = fromMaybe 0 (pat.order !! seqPos)
    gline i =
      [ svgEl "line"
          [ svgAttr "x1" (show (pd + toNumber i * st)), svgAttr "y1" (show pd)
          , svgAttr "x2" (show (pd + toNumber i * st)), svgAttr "y2" (show (pd + 4.0 * st))
          , svgAttr "stroke" "#0000001a", svgAttr "stroke-width" "0.5" ] []
      , svgEl "line"
          [ svgAttr "x1" (show pd), svgAttr "y1" (show (pd + toNumber i * st))
          , svgAttr "x2" (show (pd + 4.0 * st)), svgAttr "y2" (show (pd + toNumber i * st))
          , svgAttr "stroke" "#0000001a", svgAttr "stroke-width" "0.5" ] []
      ]
  in
    svgEl "svg" [ svgAttr "viewBox" "0 0 54 54", svgAttr "width" "46", svgAttr "height" "46" ]
      ( gline 0 <> gline 1 <> gline 2 <> gline 3 <> gline 4
          <>
            [ svgEl "polyline"
                [ svgAttr "points" pts, svgAttr "fill" "none", svgAttr "stroke" color
                , svgAttr "stroke-width" "1.4", svgAttr "stroke-linejoin" "round"
                , svgAttr "stroke-linecap" "round", svgAttr "opacity" "0.9" ] []
            , svgEl "circle"
                [ svgAttr "cx" (show (cx g0)), svgAttr "cy" (show (cy g0))
                , svgAttr "r" "2", svgAttr "fill" "none", svgAttr "stroke" "#3a362c", svgAttr "stroke-width" "1" ] []
            , svgEl "circle"
                [ svgAttr "cx" (show (cx gc)), svgAttr "cy" (show (cy gc))
                , svgAttr "r" "2.6", svgAttr "fill" color ] []
            ]
      )

miniKnob :: forall m. KnobTarget -> Int -> String -> String -> String -> H.ComponentHTML Action () m
miniKnob target val color topLabel valText =
  let r = targetRange target
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:46px" ]
      [ HH.span [ style $ engrave <> ";font-size:8px;margin-bottom:1px" ] [ HH.text topLabel ]
      , HH.div [ style "width:38px;height:38px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color, lo: r.lo, hi: r.hi, value: val } (KnobDown target val) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#3f3c33;margin-top:1px" ]
          [ HH.text valText ]
      ]

controls :: forall m. State -> H.ComponentHTML Action () m
controls s =
  HH.div [ style "display:flex;gap:8px;align-items:center;margin-top:14px" ]
    [ HH.button
        [ HE.onClick \_ -> ToggleRun
        , style $ "padding:6px 12px;border:1px solid #a8a392;border-radius:7px;"
            <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:12px;color:#3f3c33;cursor:pointer"
        ]
        [ HH.text (if s.running then "❚❚ Stop" else "▶ Run") ]
    , HH.span [ style $ engrave <> ";font-size:8px;color:#888273" ]
        [ HH.text "click a thumbnail to change a head's pattern" ]
    ]

nameplate :: forall m. State -> H.ComponentHTML Action () m
nameplate s =
  HH.div
    [ style $ "margin-top:16px;padding:7px 10px;border-radius:6px;"
        <> "background:linear-gradient(#c8a86a,#b8975a);box-shadow:0 1px 0 #00000022 inset;"
        <> engrave <> ";color:#3a3320;font-size:9px;display:flex;justify-content:space-between"
    ]
    [ HH.span [ style "letter-spacing:0.22em;color:#2c2718" ] [ HH.text "TRIGGERFISH" ]
    , HH.span [] [ HH.text $ "Model Odonus · " <> show (length s.odo.heads) <> "-head" ]
    ]
