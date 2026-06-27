-- | Triggerfish.Balistes — a virtual Mutable Instruments Grids, dressed in the
-- | Hainbach/Braun panel. Three things on one surface:
-- |
-- |   • the STYLE PAD — the iconic X/Y cursor roaming the 5×5 node grid; drag it
-- |     and the kit morphs by bilinear interpolation between the four nearest
-- |     drum maps. The hardware's one knob-pair, made spatial.
-- |   • the PATTERN map — a live 3×32 heatmap of the interpolated landscape (the
-- |     plan's insight: the interpolation *is* a visualization), with the density
-- |     threshold rendered as which cells light up, and the playhead sweeping it.
-- |   • DENSITY + RANDOMNESS knobs and a transport.
-- |
-- | The brain is `Balistes.Model`/`Engine`, a faithful port of the MIT firmware
-- | (and of the BEAM `balistes_voice`), so this app is also the design surface
-- | for that module — the same role Odonus plays for `odonus_engine`. Output is
-- | one MIDI channel (BD/SD/HH = 36/38/42) to an IAC bus into Ableton, clocked
-- | by Binnacle (free-run → Link-lock), exactly like Odonus.
module Triggerfish.Balistes.Component (component) where

import Prelude

import Data.Array (concatMap, filter, length, null, range, (!!))
import Data.Foldable (any, for_, sum)
import Data.Int (round, toNumber)
import Data.Int.Bits (shr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Source as Source
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Balistes.Tables as T
import Triggerfish.Balistes.Tidal as Tidal
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Ui.Pointer as Pointer
import Triggerfish.Odonus.Grid.Widgets (clampI, engrave, style, svgAttr, svgEl)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

-- ---------------------------------------------------------------------------
-- State / Actions
-- ---------------------------------------------------------------------------

-- | A recent hit, kept just long enough to flash the transport pilot lamps.
type Flash = { inst :: Int, accent :: Boolean, fireUnixMicros :: Number }

data KnobTarget = KDens Int | KRand | KPush Int

-- | Which colour layer is in focus. The other desaturates to grey so the two
-- | colour systems (the kit's lane hues, the routing accent) never shout at
-- | once — your "monochrome one side at a time".
data Focus = FocusBoth | FocusKit | FocusPatterns

derive instance Eq Focus

nextFocus :: Focus -> Focus
nextFocus = case _ of
  FocusBoth -> FocusKit
  FocusKit -> FocusPatterns
  FocusPatterns -> FocusBoth

focusLabel :: Focus -> String
focusLabel = case _ of
  FocusBoth -> "BOTH"
  FocusKit -> "KIT"
  FocusPatterns -> "PATTERNS"

-- | A document-tracked drag either turns a knob or subdivides a Grids cell into
-- | ratchets (`DCell lane step`). One drag plumbing for both.
data DragKind = DKnob KnobTarget | DCell Int Int

type Drag = { kind :: DragKind, startY :: Int, startVal :: Int }

-- | The value range each knob spans (so the drag scales correctly per target).
targetRange :: KnobTarget -> { lo :: Int, hi :: Int }
targetRange = case _ of
  KDens _ -> { lo: 0, hi: 255 }
  KRand -> { lo: 0, hi: 255 }
  KPush _ -> { lo: -50, hi: 50 }

type State =
  { bal :: M.Balistes
  , running :: Boolean
  , playStep :: Int
  , flash :: Array Flash
  , binnacle :: Maybe Binnacle.Binnacle
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , nowMicros :: Number
  , dragging :: Maybe Drag
  , dragSub :: Maybe H.SubscriptionId
  , focus :: Focus
  -- the editable SOURCE document — verbatim user text, the authority for the
  -- typed lane sources + routing patterns (parsed into `bal` on every edit).
  , sourceDoc :: String
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ToggleRun
  | ResetPat
  | Dice
  | PadAt Int Int Int          -- clientX clientY buttons
  | StartDrag DragKind Int     -- kind, startVal
  | DragMove Int
  | DragEnd
  | DillaPreset
  | FlatGroove
  | TogglePad Int Int          -- pad-lane index, cell (within that lane's meter)
  | SetSourceDoc String        -- the whole editable SOURCE document, verbatim
  | SetLabel Int String        -- pad-lane index, new label
  | CycleFocus
  | NoOp

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { bal: Source.parseBody Source.starterDoc M.defaultBalistes
        , running: false, playStep: 0, flash: []
        , binnacle: Nothing, midiOut: Nothing, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, dragSub: Nothing, focus: FocusBoth
        , sourceDoc: Source.starterDoc }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell's TIDAL-tab query: the reflective header (X/Y, densities,
-- | groove, ratchets, tapped pads) over the editable lane/routing doc.
handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery (AskSource reply) = do
  s <- H.get
  pure (Just (reply (Source.headerText s.bal <> "\n\n" <> s.sourceDoc)))

-- ---------------------------------------------------------------------------
-- handleAction
-- ---------------------------------------------------------------------------

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Same rig handshake as Odonus: Binnacle free-runs at 120 until the Link
    -- anchor arrives, then phase-locks, so Balistes plays solo or in ensemble.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (Step tick)
    frameE <- frameTimer
    _ <- H.subscribe frameE
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
    when st.running do
      let
        playedStep = st.bal.step
        r = M.tick st.bal
        stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
        pads = range 0 (M.padCount st.bal - 1)
        -- One Tidal cycle == one Grids loop (cycleSteps steps). A pad lane is a
        -- pattern with its own meter; we filter each lane's true cycle onsets to
        -- THIS step's window and fire each at its sub-step offset — so a triplet
        -- (`bd*3`) or a septuplet euclid lands off the 32-grid, exactly.
        lo = toNumber playedStep / toNumber cycleSteps
        hi = toNumber (playedStep + 1) / toNumber cycleSteps
        inWin o = o >= lo && o < hi
        -- each pad lane gets its OWN onsets (source + clicks) plus the onsets
        -- routed to it by name from the routing patterns; both fire on the
        -- lane's channel (OH on the Grids channel, the kit on its own).
        padHits = map
          ( \i ->
              { i
              , own: filter inWin (Tidal.laneCycleOnsetsAt st.bal i)
              , routed: filter inWin (Tidal.routedOnsetsForLane st.bal i)
              }
          )
          pads
        firedPads = filter (\p -> not (null p.own) || not (null p.routed)) padHits
      for_ st.midiOut \out -> liftEffect do
        -- the three Grids lanes (still step-quantised, firmware-faithful)
        for_ r.fired \t ->
          emitHit out st.bal drumChannel stepMs tick.delayMs playedStep t.inst (M.instNote t.inst) t.accent
        -- the pattern-native pad lanes, at true fractional times
        for_ padHits \p ->
          let
            lane = M.firstPadLane + p.i
            note = M.padNote st.bal p.i
            ch = padChannel p.i
            fire o =
              let sub = (o * toNumber cycleSteps - toNumber playedStep) * stepMs
              in emitHit out st.bal ch stepMs (tick.delayMs + sub) playedStep lane note false
          in do
            for_ p.own fire
            for_ p.routed fire
      let
        gridsFlash = map (\t -> { inst: t.inst, accent: t.accent, fireUnixMicros: tick.fireUnixMicros }) r.fired
        padFlash = map (\p -> { inst: M.firstPadLane + p.i, accent: false, fireUnixMicros: tick.fireUnixMicros }) firedPads
      H.modify_ \s -> s { bal = r.bal, playStep = playedStep, flash = gridsFlash <> padFlash <> s.flash }

  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        H.modify_ \s -> s
          { nowMicros = now
          , clockTempo = r.tempo
          , clockLocked = r.locked
          , clockBeat = r.beat
          , clockBar = r.bar
          , anchorCount = r.anchorCount
          , flash = filter (\f -> (now - f.fireUnixMicros) < flashWindow) s.flash
          }
      Nothing -> pure unit

  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }

  ToggleRun -> H.modify_ \s -> s { running = not s.running }
  ResetPat -> H.modify_ \s -> s { bal = M.reset s.bal, playStep = 0 }
  Dice -> H.modify_ \s -> s { bal = M.reseed s.bal }

  PadAt cx cy btns ->
    when (btns == 1) do
      { x, y } <- liftEffect $ Pointer.padNorm padId cx cy
      H.modify_ \s ->
        let
          b1 = M.setY (round ((1.0 - y) * 255.0)) (M.setX (round (x * 255.0)) s.bal)
          moved = b1.x /= s.bal.x || b1.y /= s.bal.y
        in
          s { bal = if moved then M.clearRatchets b1 else b1 }

  StartDrag kind startVal -> do
    sid <- setupDrag
    H.modify_ _ { dragging = Just { kind, startY: 0, startVal }, dragSub = Just sid }
  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just d
        | d.startY == 0 -> H.modify_ _ { dragging = Just d { startY = clientY } }
        | otherwise -> do
            let dist = d.startY - clientY
            case d.kind of
              DKnob target ->
                let r = targetRange target
                    delta = round (toNumber dist * toNumber (r.hi - r.lo) / 200.0)
                    newVal = clampI r.lo r.hi (d.startVal + delta)
                in H.modify_ \s -> s { bal = applyKnob target newVal s.bal }
              DCell inst step ->
                let newVal = clampI 1 8 (d.startVal + round (toNumber dist / 22.0))
                in H.modify_ \s -> s { bal = M.setRatchetAt inst step newVal s.bal }
      Nothing -> pure unit
  DragEnd -> do
    st <- H.get
    for_ st.dragSub H.unsubscribe
    H.modify_ _ { dragging = Nothing, dragSub = Nothing }

  DillaPreset -> H.modify_ \s -> s { bal = M.dillaPush s.bal }
  FlatGroove -> H.modify_ \s -> s { bal = M.flatPush s.bal }
  TogglePad i cell -> H.modify_ \s -> s { bal = Tidal.toggleLaneClick i cell s.bal }
  -- the textarea is the authority for typed sources + routes: store the verbatim
  -- text, then re-derive those slices of the model from it (clicks/knobs untouched).
  SetSourceDoc doc -> H.modify_ \s -> s { sourceDoc = doc, bal = Source.parseBody doc s.bal }
  SetLabel i nm -> H.modify_ \s -> s { bal = M.setPadName i nm s.bal }
  CycleFocus -> H.modify_ \s -> s { focus = nextFocus s.focus }
  NoOp -> pure unit

-- | Emit one hit on a lane: apply that lane's Dilla push (signed ms), and — for
-- | the Grids lanes — explode a ratcheted cell into N evenly-spaced retriggers
-- | at flat velocity. Lane 3 (open hat) rings longer; everything else is a blip.
emitHit
  :: Midi.MidiOut -> M.Balistes -> Int -> Number -> Number -> Int -> Int -> Int -> Boolean -> Effect Unit
emitHit out b channel stepMs baseDelayMs step lane note accent =
  let
    fullDur = if note == 46 then 200.0 else 30.0   -- open hat rings; rest are blips
    v0 = if accent then accentVel else baseVel
    delay0 = max 0.0 (baseDelayMs + toNumber (M.pushOf lane b))
    n = M.ratchetAt b lane step
  in
    if n <= 1 then
      Midi.scheduleNote out
        { channel, note, velocity: v0, delayMs: delay0, durMs: fullDur }
    else
      let sub = stepMs / toNumber n
      in for_ (range 0 (n - 1)) \k ->
           Midi.scheduleNote out
             { channel, note, velocity: v0
             , delayMs: delay0 + toNumber k * sub, durMs: sub * 0.9 }

applyKnob :: KnobTarget -> Int -> M.Balistes -> M.Balistes
applyKnob (KDens i) v = M.setDensity i v
applyKnob KRand v = M.setRandomness v
applyKnob (KPush i) v = M.setPush i v

knobValue :: KnobTarget -> M.Balistes -> Int
knobValue (KDens i) b = M.densityOf i b
knobValue KRand b = b.randomness
knobValue (KPush i) b = M.pushOf i b

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Grids step = a 16th note (32 steps = two bars). Same lookahead as Odonus.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | Steps in one Grids loop — and one Tidal cycle. A pad lane's pattern is
-- | mapped onto [0,1) across the whole loop, so `bd*4` is four hits per loop;
-- | bump this mapping (or shorten the loop) if cycles should feel faster.
cycleSteps :: Int
cycleSteps = 32

midiPortName :: String
midiPortName = "IAC"

-- | GM drum channel (MIDI ch 10) — the Grids device + OH (the bridge).
drumChannel :: Int
drumChannel = 9

-- | The Tidal kit's own channel (MIDI ch 11), so it's a genuinely separate
-- | drum machine: route a second Ableton track here. OH stays on `drumChannel`.
tidalChannel :: Int
tidalChannel = 10

-- | A pad lane's MIDI channel: OH bridges to the Grids channel; the rest of the
-- | kit is on the Tidal channel.
padChannel :: Int -> Int
padChannel i = if i == M.ohPadIndex then drumChannel else tidalChannel

-- | The routing layer's accent colour — one ink-violet hue distinct from the
-- | warm kit palette, so a multi-lane routed gesture binds by shared colour.
routeAccent :: String
routeAccent = "#46415f"

-- | The grey the unfocused colour layer desaturates to.
dimGrey :: String
dimGrey = "#b3afa3"

accentVel :: Int
accentVel = 120

baseVel :: Int
baseVel = 78

padId :: String
padId = "balistes-pad"

-- | Keep a hit around ~0.4s — long enough for the pilot lamps to glow.
flashWindow :: Number
flashWindow = 400000.0

instColor :: Int -> String
instColor = case _ of
  0 -> "#b04a2f"   -- BD, amber-red
  1 -> "#5f7d3f"   -- SD, green
  2 -> "#3f6f8a"   -- HH, steel-blue
  _ -> "#2f8a8a"   -- OH, teal (the pattern lane)

-- ---------------------------------------------------------------------------
-- Timers / drag plumbing (mirrors Odonus)
-- ---------------------------------------------------------------------------

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

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;inset:0;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ transportPanel s
    , controlsPanel s
    , patternPanel s
    , sourcePanel s
    ]

-- A pale Hainbach panel (header + body). Scrolls vertically if its content is
-- taller than the viewport (the consolidated CONTROL panel can be).
panel :: forall m. String -> String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panel label widthCss body =
  HH.div
    [ style $ widthCss <> ";height:100vh;box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
        <> "padding:18px 16px;display:flex;flex-direction:column" ]
    ( [ HH.div
          [ style $ engrave <> ";font-size:14px;letter-spacing:0.16em;color:#3f3c33;"
              <> "margin-bottom:14px;border-bottom:1px solid #00000018;padding-bottom:6px" ]
          [ HH.text label ]
      ] <> body )

-- ---------------------------------------------------------------------------
-- Transport panel
-- ---------------------------------------------------------------------------

transportPanel :: forall m. State -> H.ComponentHTML Action () m
transportPanel s =
  panel "BALISTES" "flex:0 0 196px"
    [ HH.div [ style "display:flex;flex-direction:column;gap:12px;margin-top:4px" ]
        [ HH.button
            [ HE.onClick \_ -> ToggleRun
            , style $ "padding:12px 0;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
                <> "font-family:Georgia,serif;font-size:15px;letter-spacing:0.12em;color:#1c1a12;"
                <> "background:" <> (if s.running then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
            [ HH.text (if s.running then "■ STOP" else "▶ RUN") ]
        , HH.div [ style "display:flex;gap:8px" ]
            [ flatBtn "RESET" ResetPat
            , flatBtn "DICE" Dice
            ]
        , HH.div [ style "display:flex;align-items:center;gap:7px" ]
            [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6;width:30px;flex:0 0 auto" ] [ HH.text "FOCUS" ]
            , HH.button
                [ HE.onClick \_ -> CycleFocus
                , style $ "flex:1;padding:6px 0;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
                    <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;letter-spacing:0.1em;color:#3f3c33;"
                    <> "background:linear-gradient(#efece1,#ddd9cb)" ]
                [ HH.text (focusLabel s.focus) ]
            ]
        , lampRow s
        , readout "TEMPO" (show (round s.clockTempo) <> " bpm" <> (if s.clockLocked then " ⛓" else " ·"))
        , readout "BAR" (show s.clockBar <> "  ·  step " <> pad2 (s.playStep + 1) <> "/32")
        , readout "MIDI" s.midiName
        , readout "CH" (show (drumChannel + 1) <> "  ·  36 / 38 / 42")
        , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-top:6px;line-height:1.5" ]
            [ HH.text "DRAG THE STYLE PAD TO MORPH THE KIT BETWEEN THE 25 NODES. DENSITY SETS HOW MANY HITS; RANDOMNESS NUDGES OFF-GRID EACH PATTERN." ]
        ]
    ]

flatBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
flatBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:1;padding:8px 0;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.08em;color:#3f3c33;"
        <> "background:linear-gradient(#efece1,#ddd9cb)" ]
    [ HH.text label ]

readout :: forall m. String -> String -> H.ComponentHTML Action () m
readout label val =
  HH.div [ style "display:flex;justify-content:space-between;align-items:baseline;border-bottom:1px dotted #0000001a;padding-bottom:3px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text label ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;text-align:right" ] [ HH.text val ]
    ]

-- Three pilot lamps that glow on a recent hit (bright = accented).
lampRow :: forall m. State -> H.ComponentHTML Action () m
lampRow s =
  HH.div [ style "display:flex;gap:10px;justify-content:center;margin:6px 0" ]
    (map lamp [ 0, 1, 2 ])
  where
  lamp inst =
    let
      hits = filter (\f -> f.inst == inst && (s.nowMicros - f.fireUnixMicros) < flashWindow) s.flash
      on = not (null hits)
      accent = any _.accent hits
      col = instColor inst
      fill = if on then col else "#8c887a"
      glow = if on then ";box-shadow:0 0 9px " <> col <> (if accent then "" else "aa") else ""
    in
      HH.div [ style "display:flex;flex-direction:column;align-items:center;gap:3px" ]
        [ HH.div [ style $ "width:18px;height:18px;border-radius:50%;border:1px solid #00000033;background:" <> fill <> glow ] []
        , HH.span [ style $ engrave <> ";font-size:8px" ] [ HH.text (M.instName inst) ]
        ]

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

-- ---------------------------------------------------------------------------
-- Control panel — the STYLE pad + density/groove knobs, consolidated so the
-- SOURCE pane has room. Pad on top; two knob columns (DENSITY · PUSH) below.
-- ---------------------------------------------------------------------------

controlsPanel :: forall m. State -> H.ComponentHTML Action () m
controlsPanel s =
  panel "CONTROL" "flex:0 0 300px"
    [ HH.div [ style "display:flex;justify-content:center" ]
        [ HH.div [ style "width:252px;height:252px" ] [ padSvg s ] ]
    , HH.div [ style "display:flex;justify-content:space-between;margin:2px 6px 8px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text ("X " <> show s.bal.x) ]
        , HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text ("Y " <> show s.bal.y) ]
        ]
    , HH.div [ style "height:1px;background:#00000018;margin-bottom:10px" ] []
    , HH.div [ style "display:flex;gap:16px;justify-content:center" ]
        [ HH.div [ style "display:flex;flex-direction:column;gap:9px;align-items:center" ]
            [ grLabel "DENSITY"
            , bigKnob (KDens 0) (instColor 0) "BD" s.bal
            , bigKnob (KDens 1) (instColor 1) "SD" s.bal
            , bigKnob (KDens 2) (instColor 2) "HH" s.bal
            , bigKnob KRand "#6a6657" "RAND" s.bal
            ]
        , HH.div [ style "display:flex;flex-direction:column;gap:9px;align-items:center" ]
            [ grLabel "PUSH ms"
            , bigKnob (KPush 0) (instColor 0) "BD" s.bal
            , bigKnob (KPush 1) (instColor 1) "SD" s.bal
            , bigKnob (KPush 2) (instColor 2) "HH" s.bal
            , HH.div [ style "display:flex;flex-direction:column;gap:5px;width:64px;margin-top:2px" ]
                [ flatBtn "DILLA" DillaPreset, flatBtn "FLAT" FlatGroove ]
            ]
        ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.55;text-align:center;line-height:1.5;margin-top:12px" ]
        [ HH.text "CLICK A PAD LANE TO PROGRAM IT · ⌥ ALT-DRAG ANY CELL TO RATCHET" ]
    ]

padSvg :: forall m. State -> H.ComponentHTML Action () m
padSvg s =
  let
    b = s.bal
    -- node value 0..255 for grid index 0..4
    nodeVal k = toNumber k * 255.0 / 4.0
    sx v = v
    sy v = 255.0 - v
    i0 = b.x `shr` 6
    j0 = b.y `shr` 6
    bracket i j = (i == i0 || i == i0 + 1) && (j == j0 || j == j0 + 1)
    -- a node dot, radius by its overall energy
    dot i j =
      let
        nv = T.node (T.drumMapIx i j)
        energy = if null nv then 0.0 else toNumber (sum nv) / (96.0 * 255.0)
        r = 3.0 + energy * 7.0
        hot = bracket i j
        dcx = sx (nodeVal i)
        dcy = sy (nodeVal j)
      in
        svgEl "circle"
          [ svgAttr "cx" (show dcx), svgAttr "cy" (show dcy), svgAttr "r" (show r)
          , svgAttr "fill" (if hot then "#7a7460" else "#9a9583")
          , svgAttr "fill-opacity" (if hot then "0.85" else "0.4")
          ] []
    cx = sx (toNumber b.x)
    cy = sy (toNumber b.y)
  in
    svgEl "svg"
      [ svgAttr "viewBox" "-8 -8 272 272", svgAttr "width" "100%", svgAttr "height" "100%"
      , svgAttr "id" padId
      , svgMouse "mousedown" \e -> PadAt (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , svgMouse "mousemove" \e -> PadAt (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , svgAttr "style" "display:block;cursor:crosshair;touch-action:none"
      ]
      ( [ svgEl "rect"
            [ svgAttr "x" "-8", svgAttr "y" "-8", svgAttr "width" "272", svgAttr "height" "272"
            , svgAttr "rx" "8", svgAttr "fill" "#c4bfae", svgAttr "stroke" "#a8a392" ] []
        ]
        <> (range 0 4 `concatMap'` \i -> range 0 4 `concatMap'` \j -> [ dot i j ])
        <> crosshair cx cy
      )

-- crosshair lines + a filled cursor dot
crosshair :: forall w i. Number -> Number -> Array (HH.HTML w i)
crosshair cx cy =
  [ svgEl "line"
      [ svgAttr "x1" "0", svgAttr "y1" (show cy), svgAttr "x2" "255", svgAttr "y2" (show cy)
      , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.25", svgAttr "stroke-width" "0.8" ] []
  , svgEl "line"
      [ svgAttr "x1" (show cx), svgAttr "y1" "0", svgAttr "x2" (show cx), svgAttr "y2" "255"
      , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.25", svgAttr "stroke-width" "0.8" ] []
  , svgEl "circle"
      [ svgAttr "cx" (show cx), svgAttr "cy" (show cy), svgAttr "r" "7"
      , svgAttr "fill" "#1c1a12", svgAttr "stroke" "#efece1", svgAttr "stroke-width" "2" ] []
  ]

-- Flipped concatMap so the call sites read `range … `concatMap'` \i -> …`.
concatMap' :: forall a b. Array a -> (a -> Array b) -> Array b
concatMap' xs f = concatMap f xs

-- Clamp a Number to [lo, hi].
clampNum :: Number -> Number -> Number -> Number
clampNum lo hi v = if v < lo then lo else if v > hi then hi else v

-- An SVG mouse handler (the `svgEl` row is `()`, so the typed HE.onMouse* props
-- don't fit; coerce the MouseEvent decode like Ui.Knob's mousedown handler).
svgMouse :: forall r i. String -> (ME.MouseEvent -> i) -> HH.IProp r i
svgMouse name f = HE.handler (EventType name) (unsafeCoerce f)

-- ---------------------------------------------------------------------------
-- Pattern heatmap — 3 instruments × 32 steps
-- ---------------------------------------------------------------------------

patternPanel :: forall m. State -> H.ComponentHTML Action () m
patternPanel s =
  panel "PATTERN" "flex:1 1 480px;min-width:380px"
    [ HH.div [ style "width:100%;max-width:640px;margin:0 auto" ] [ heatSvg s ]
    , sectionLabel "KIT — COLOUR · LABEL (EDIT) · DERIVED METER"
    , HH.div [ style "display:flex;flex-wrap:wrap;gap:5px 14px;max-width:640px;margin:0 auto" ]
        (map (laneLegendRow s) (range 0 (M.padCount s.bal - 1)))
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;margin-top:12px;line-height:1.5;max-width:640px" ]
        [ HH.text "EDIT LANE SOURCES + ROUTES IN THE SOURCE PANE → · A LANE LINE IS \"label \"\"mini-notation\"\"\"; A ROUTE LINE IS \"route \"\"…\"\"\". COMMENT (--) TO MUTE." ]
    ]

sectionLabel :: forall m. String -> H.ComponentHTML Action () m
sectionLabel t = HH.div [ style $ engrave <> ";font-size:9px;margin:16px 0 7px;opacity:0.65" ] [ HH.text t ]

-- One kit-legend entry: colour chip, the (editable, except OH) label, and the
-- lane's derived meter. Sources + routes are authored in the SOURCE pane now;
-- this row keeps the kit identity visible and the digraph editable.
laneLegendRow :: forall m. State -> Int -> H.ComponentHTML Action () m
laneLegendRow s i =
  HH.div [ style "display:flex;align-items:center;gap:5px" ]
    [ HH.div [ style $ "width:9px;height:9px;border-radius:2px;flex:0 0 auto;background:" <> M.padColor s.bal i ] []
    , if i == M.ohPadIndex then
        HH.span [ style "flex:0 0 auto;font-family:Georgia,serif;font-size:9px;color:#3f3c33;letter-spacing:0.04em" ]
          [ HH.text (M.padName s.bal i) ]
      else
        HH.input
          [ HP.value (M.padName s.bal i)
          , HE.onValueInput (SetLabel i)
          , style $ "width:30px;flex:0 0 auto;padding:2px 4px;border:1px solid #b3ae9c66;border-radius:3px;"
              <> "background:#efece1;font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#3f3c33"
          ]
    , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.5;flex:0 0 auto" ]
        [ HH.text ("×" <> show (Tidal.laneMeterAt s.bal i)) ]
    ]

heatSvg :: forall m. State -> H.ComponentHTML Action () m
heatSvg s =
  let
    b = s.bal
    cols = 32
    colW = 16.0
    rowH = 30.0
    groupGap = 9.0   -- vertical space between banks of 4 lanes (MPC-style)
    nLanes = 3 + M.padCount b
    -- the top of a lane, with a gap added before each new bank of 4.
    laneY lane = toNumber lane * rowH + toNumber (lane / 4) * groupGap
    w = toNumber cols * colW
    h = toNumber nLanes * rowH + toNumber ((nLanes - 1) / 4) * groupGap
    rectBlock x0 y0 wid hgt c op =
      svgEl "rect"
        [ svgAttr "x" (show x0), svgAttr "y" (show y0)
        , svgAttr "width" (show wid), svgAttr "height" (show hgt), svgAttr "rx" "2"
        , svgAttr "fill" c, svgAttr "fill-opacity" (show op) ] []
    accentOutline x0 y0 =
      svgEl "rect"
        [ svgAttr "x" (show (x0 + 1.0)), svgAttr "y" (show (y0 + 1.0))
        , svgAttr "width" (show (colW - 3.0)), svgAttr "height" (show (rowH - 3.0)), svgAttr "rx" "3"
        , svgAttr "fill" "none", svgAttr "stroke" "#1c1a12", svgAttr "stroke-width" "1.2"
        , svgAttr "style" "pointer-events:none" ] []
    -- ratchets as a vertical STACK of n blocks down the tall pill (readable,
    -- since the cell is taller than wide), all at the same (flat) opacity.
    stackBlocks x y c n op =
      if n <= 1 then [ rectBlock (x + 2.0) (y + 2.0) (colW - 5.0) (rowH - 5.0) c op ]
      else
        range 0 (n - 1) `concatMap'` \k ->
          let segH = (rowH - 4.0) / toNumber n
              sy = y + 2.0 + toNumber k * segH
          in [ rectBlock (x + 2.0) sy (colW - 5.0) (segH - 1.0) c op ]
    -- the small ⋮N marker shown on a ratcheted slot that isn't firing.
    ratchetHint x y c n =
      svgEl "text"
        [ svgAttr "x" (show (x + colW / 2.0)), svgAttr "y" (show (y + rowH / 2.0 + 3.0))
        , svgAttr "fill" c, svgAttr "fill-opacity" "0.6", svgAttr "font-size" "8"
        , svgAttr "text-anchor" "middle", svgAttr "font-family" "Georgia,serif"
        , svgAttr "style" "pointer-events:none" ]
        [ HH.text ("⋮" <> show n) ]
    -- a Grids lane cell: faint interpolated landscape + ratcheted hit + accent.
    gridsCell lane step =
      let
        level = M.levelAt b lane step
        fires = M.wouldFire b lane step
        accent = fires && level > 192
        n = M.ratchetAt b lane step
        x = toNumber step * colW
        y = laneY lane
        c = instColor lane
        landscape = rectBlock x y (colW - 1.0) (rowH - 1.0) c (toNumber level / 255.0 * 0.32)
        segs = if not fires then [] else stackBlocks x y c n (if accent then 0.95 else 0.7)
        acc = if accent then [ accentOutline x y ] else []
        hint = if n > 1 && not fires then [ ratchetHint x y c n ] else []
      in
        [ landscape ] <> segs <> acc <> hint
    -- a pad lane drawn at its OWN meter (the polymeter): m equal cells across
    -- the full width, lit from the merged source+click mask, with thin ticks at
    -- the source's TRUE onset positions (which need not land on the cells — that
    -- is the Patterning-ring geometry showing through the notation).
    padRow i =
      let
        laneIdx = M.firstPadLane + i
        m = Tidal.laneMeterAt b i
        mask = Tidal.laneCellMaskAt b i
        onsets = Tidal.laneSourceOnsetsAt b i
        routed = Tidal.routedOnsetsForLane b i
        y = laneY laneIdx
        -- the lane's own hue (its kit identity) and the routing accent, each
        -- desaturated when the OTHER layer is in focus.
        kc = if s.focus == FocusPatterns then dimGrey else M.padColor b i
        rc = if s.focus == FocusKit then dimGrey else routeAccent
        cw = w / toNumber m
        -- Each pill is LEFT-aligned to its cell's onset (the beat), with the gap
        -- on the right — so every lane's cell 0 starts at the same x, lined up
        -- with the Grids downbeat and the first beat line. The gap scales with
        -- cell width (capped) so fat and thin cells both read as distinct pills.
        leftPad = 2.0
        gap = clampNum 2.0 13.0 (cw * 0.13)
        cell k =
          let
            x = toNumber k * cw
            on = fromMaybe false (mask !! k)
          in
            [ svgEl "rect"
                [ svgAttr "x" (show (x + leftPad)), svgAttr "y" (show (y + 2.0))
                , svgAttr "width" (show (max 2.0 (cw - leftPad - gap))), svgAttr "height" (show (rowH - 4.0)), svgAttr "rx" "3"
                , svgAttr "fill" kc, svgAttr "fill-opacity" (if on then "0.85" else "0.07")
                , svgAttr "stroke" kc, svgAttr "stroke-opacity" (if on then "0.92" else "0.20")
                , svgAttr "stroke-width" (if on then "1.1" else "0.7")
                , svgAttr "style" "pointer-events:none" ] [] ]
        -- ticks only when the source packs MORE onsets than there are cells (a
        -- nested pattern like `[bd sn] cp`) — then they reveal the true sub-cell
        -- hits the meter grid can't show. Aligned onsets would just be noise.
        tick o =
          [ svgEl "line"
              [ svgAttr "x1" (show (o * w)), svgAttr "y1" (show (y + 3.0))
              , svgAttr "x2" (show (o * w)), svgAttr "y2" (show (y + rowH - 3.0))
              , svgAttr "stroke" "#1c1a12", svgAttr "stroke-opacity" "0.35", svgAttr "stroke-width" "0.8"
              , svgAttr "style" "pointer-events:none" ] [] ]
        ticks = if length onsets > m then onsets `concatMap'` tick else []
        -- routed hits: accent-colour markers at their TRUE x (the routing
        -- pattern's positions, not this lane's cells), overlaid on the row.
        mark o =
          [ svgEl "rect"
              [ svgAttr "x" (show (o * w)), svgAttr "y" (show (y + 3.5))
              , svgAttr "width" "6.5", svgAttr "height" (show (rowH - 7.0)), svgAttr "rx" "3"
              , svgAttr "fill" rc, svgAttr "fill-opacity" "0.95"
              , svgAttr "stroke" "#efece1", svgAttr "stroke-opacity" "0.35", svgAttr "stroke-width" "0.6"
              , svgAttr "style" "pointer-events:none" ] [] ]
        marks = routed `concatMap'` mark
      in
        (range 0 (m - 1) `concatMap'` cell) <> ticks <> marks
    -- a pad cell click target: plain click toggles that cell of the lane's
    -- clicked overlay (stacked onto the typed source).
    padTarget i k =
      let
        m = Tidal.laneMeterAt b i
        cw = w / toNumber m
        x = toNumber k * cw
        y = laneY (M.firstPadLane + i)
      in
        svgEl "rect"
          [ svgAttr "x" (show x), svgAttr "y" (show y)
          , svgAttr "width" (show (cw - 1.0)), svgAttr "height" (show (rowH - 1.0))
          , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:pointer;pointer-events:all"
          , svgMouse "mousedown" \_ -> TogglePad i k ] []
    -- Grids cells have no plain action (hits come from X/Y); Alt-drag sets the
    -- beat's ratchet (drag up/down).
    gridsTarget lane step =
      let
        x = toNumber step * colW
        y = laneY lane
        act e =
          if ME.altKey e then StartDrag (DCell lane step) (M.ratchetAt b lane step)
          else NoOp
      in
        svgEl "rect"
          [ svgAttr "x" (show x), svgAttr "y" (show y)
          , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
          , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:ns-resize;pointer-events:all"
          , svgMouse "mousedown" act ] []
    playhead =
      svgEl "rect"
        [ svgAttr "x" (show (toNumber s.playStep * colW)), svgAttr "y" "0"
        , svgAttr "width" (show colW), svgAttr "height" (show h)
        , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" (if s.running then "0.10" else "0.0")
        , svgAttr "stroke" "#1c1a12", svgAttr "stroke-opacity" (if s.running then "0.5" else "0.15")
        , svgAttr "stroke-width" "1", svgAttr "style" "pointer-events:none" ] []
    beatLines =
      range 0 8 `concatMap'` \k ->
        let x = toNumber (k * 4) * colW
        in [ svgEl "line"
               [ svgAttr "x1" (show x), svgAttr "y1" "0", svgAttr "x2" (show x), svgAttr "y2" (show h)
               , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.18", svgAttr "stroke-width" "0.8"
               , svgAttr "style" "pointer-events:none" ] [] ]
    laneDivider lane =
      svgEl "line"
        [ svgAttr "x1" "0", svgAttr "y1" (show (laneY lane)), svgAttr "x2" (show w)
        , svgAttr "y2" (show (laneY lane))
        , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.12", svgAttr "stroke-width" "0.6"
        , svgAttr "style" "pointer-events:none" ] []
    laneName lane = if lane < 3 then "Grids " <> M.instName lane else M.padName b (lane - 3)
    rowLabel lane =
      svgEl "text"
        [ svgAttr "x" "3", svgAttr "y" (show (laneY lane + 11.0))
        , svgAttr "fill" "#3f3c33", svgAttr "fill-opacity" "0.55", svgAttr "style" "pointer-events:none"
        , svgAttr "font-size" "8", svgAttr "font-family" "Georgia,serif" ]
        [ HH.text (laneName lane) ]
    pads = range 0 (M.padCount b - 1)
    visuals =
      (range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> gridsCell lane step)
        <> (pads `concatMap'` padRow)
    targets =
      (range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> [ gridsTarget lane step ])
        <> (pads `concatMap'` \i -> range 0 (Tidal.laneMeterAt b i - 1) `concatMap'` \k -> [ padTarget i k ])
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h)
      , svgAttr "width" "100%", svgAttr "style" "display:block;max-height:90vh" ]
      ( visuals <> beatLines
          <> map laneDivider (filter (\l -> l `mod` 4 /= 0) (range 1 (nLanes - 1)))
          <> [ playhead ] <> map rowLabel (range 0 (nLanes - 1)) <> targets )

bigKnob :: forall m. KnobTarget -> String -> String -> M.Balistes -> H.ComponentHTML Action () m
bigKnob target color label b =
  let
    v = knobValue target b
    r = targetRange target
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:72px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text label ]
      , HH.div [ style "width:58px;height:58px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color, lo: r.lo, hi: r.hi, value: v, ticks: 0 } (StartDrag (DKnob target) v) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;margin-top:2px" ]
          [ HH.text (show v) ]
      ]

grLabel :: forall m. String -> H.ComponentHTML Action () m
grLabel t = HH.div [ style $ engrave <> ";font-size:8px;opacity:0.7" ] [ HH.text t ]

-- ---------------------------------------------------------------------------
-- Source panel — the live state as a read-only `balistes { … }` cell, the
-- growing spec of the BEAM module (selectable so it's copyable).
-- ---------------------------------------------------------------------------

sourcePanel :: forall m. State -> H.ComponentHTML Action () m
sourcePanel s =
  panel "SOURCE" "flex:0 0 340px"
    [ -- the reflective header: knob/pad/drag/click state as a read-only cell.
      HH.pre
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10px;line-height:1.5;"
            <> "color:#3f3c33;opacity:0.62;white-space:pre-wrap;word-break:break-word;margin:0 0 10px;"
            <> "user-select:text;-webkit-user-select:text" ]
        [ HH.text (Source.headerText s.bal) ]
    , HH.div [ style "height:1px;background:#00000018;margin-bottom:8px" ] []
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:6px" ]
        [ HH.text "PATTERNS — EDITABLE · -- TO MUTE A LINE" ]
    , -- the instrument: editable lane sources + routes, parsed live.
      HH.textarea
        [ HP.value s.sourceDoc
        , HE.onValueInput SetSourceDoc
        , HP.spellcheck false
        , style $ "flex:1 1 auto;min-height:260px;resize:none;box-sizing:border-box;"
            <> "padding:9px 10px;border:1px solid #a8a392;border-radius:6px;background:#f4f1e8;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;line-height:1.55;color:#2b2922;"
            <> "white-space:pre;overflow:auto;outline:none"
        ]
    ]
