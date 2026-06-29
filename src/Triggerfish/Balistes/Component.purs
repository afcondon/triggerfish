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

import Data.Array (concatMap, filter, null, range)
import Data.Foldable (any, for_, sum)
import Data.Int (round, toNumber)
import Data.Int.Bits (shr)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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

data KnobTarget = KDens Int | KRand | KPush Int | KOpen

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
  KOpen -> { lo: 0, hi: 255 }

type State =
  { bal :: M.Balistes
  , running :: Boolean        -- the ARM/cue flag (sticky); sounds only when master too
  , master :: Boolean         -- the shell's master transport (pushed via SetMaster)
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
  | NoOp

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { bal: M.defaultBalistes
        , running: false, master: false, playStep: 0, flash: []
        , binnacle: Nothing, midiOut: Nothing, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, dragSub: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the source (TIDAL tab) — the reflective header (X/Y,
-- | densities, groove, ratchets, tapped pads) over the editable lane/routing
-- | doc — or adopt the rack's shared free-run baseline.
handleQuery :: forall o m a. MonadAff m => Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (Source.headerText s.bal)))
  SyncFree startMicros tempo next -> do
    s <- H.get
    for_ s.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  FeedChords _ next -> pure (Just next)   -- a drum machine; no chord quantiser
  FeedVoiceChords _ next -> pure (Just next)   -- ditto
  SetMaster m next -> do
    H.modify_ _ { master = m }
    pure (Just next)

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
    when (st.master && st.running) do
      let
        playedStep = st.bal.step
        r = M.tick st.bal
        stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
      for_ st.midiOut \out -> liftEffect $
        -- the three Grids voices (step-quantised, firmware-faithful). A firing
        -- HH that clears the OPEN boundary rings as an open hat (note 46, OH
        -- push slot, long gate) and chokes its closed self; everything else is a
        -- short blip. Ratchet roll + per-voice Dilla push applied on emit.
        for_ r.fired \t ->
          let
            b = st.bal
            opens = t.inst == 2 && M.opensAt b playedStep
            note = if opens then ohNote else M.instNote t.inst
            durMs = if opens then openGateMs else closedGateMs
            pushLane = if opens then 3 else t.inst   -- open hats ride the OH push slot
            delay0 = max 0.0 (tick.delayMs + toNumber (M.pushOf pushLane b))
            n = M.ratchetAt b t.inst playedStep
            v0 = if t.accent then accentVel else baseVel
          in
            emitHit out drumChannel stepMs delay0 note durMs v0 n
      let
        gridsFlash = map (\t -> { inst: t.inst, accent: t.accent, fireUnixMicros: tick.fireUnixMicros }) r.fired
      H.modify_ \s -> s { bal = r.bal, playStep = playedStep, flash = gridsFlash <> s.flash }

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

  -- The RUN button is now a sticky ARM toggle; Balistes sounds only when armed
  -- AND the shell's master is playing. Drum hits are scheduled one-shots, so
  -- stopping just gates the next Step — nothing to silence.
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
  NoOp -> pure unit

-- | Emit one already-resolved hit: schedule `note` at `delay0` for `durMs`, or —
-- | when the cell is ratcheted (n > 1) — explode it into n evenly-spaced
-- | retriggers at flat velocity. Push/ratchet/open are resolved by the caller.
emitHit
  :: Midi.MidiOut -> Int -> Number -> Number -> Int -> Number -> Int -> Int -> Effect Unit
emitHit out channel stepMs delay0 note durMs velocity n =
  if n <= 1 then
    Midi.scheduleNote out
      { channel, note, velocity, delayMs: delay0, durMs }
  else
    let sub = stepMs / toNumber n
    in for_ (range 0 (n - 1)) \k ->
         Midi.scheduleNote out
           { channel, note, velocity
           , delayMs: delay0 + toNumber k * sub, durMs: sub * 0.9 }

applyKnob :: KnobTarget -> Int -> M.Balistes -> M.Balistes
applyKnob (KDens i) v = M.setDensity i v
applyKnob KRand v = M.setRandomness v
applyKnob (KPush i) v = M.setPush i v
applyKnob KOpen v = M.setOpen v

knobValue :: KnobTarget -> M.Balistes -> Int
knobValue (KDens i) b = M.densityOf i b
knobValue KRand b = b.randomness
knobValue (KPush i) b = M.pushOf i b
knobValue KOpen b = M.openOf b

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Grids step = a 16th note (32 steps = two bars). Same lookahead as Odonus.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

midiPortName :: String
midiPortName = "IAC"

-- | GM drum channel (MIDI ch 10) — the Grids device (BD/SD/HH).
drumChannel :: Int
drumChannel = 9

accentVel :: Int
accentVel = 120

baseVel :: Int
baseVel = 78

-- | GM open hat — what a hat fires when it clears the OPEN boundary.
ohNote :: Int
ohNote = 46

-- | Gate lengths: an open hat rings, a closed hat is a blip.
openGateMs :: Number
openGateMs = 200.0

closedGateMs :: Number
closedGateMs = 30.0

-- | The open hat's teal — distinct from HH steel-blue, so opening cells read as
-- | a different voice in the heatmap.
ohColor :: String
ohColor = "#2f8a8a"

padId :: String
padId = "balistes-pad"

-- | Keep a hit around ~0.4s — long enough for the pilot lamps to glow.
flashWindow :: Number
flashWindow = 400000.0

instColor :: Int -> String
instColor = case _ of
  0 -> "#b04a2f"   -- BD, amber-red
  1 -> "#5f7d3f"   -- SD, green
  _ -> "#3f6f8a"   -- HH, steel-blue

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
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ transportPanel s
    , controlsPanel s
    , patternPanel s
    ]

-- A pale Hainbach panel (header + body). Scrolls vertically if its content is
-- taller than the viewport (the consolidated CONTROL panel can be).
panel :: forall m. String -> String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panel label widthCss body =
  HH.div
    [ style $ widthCss <> ";height:calc(100vh - var(--tf-bar));box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
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
            [ HH.text (if s.running then (if s.master then "❚❚ PLAYING" else "◆ CUED") else "▶ ARM") ]
        , HH.div [ style "display:flex;gap:8px" ]
            [ flatBtn "RESET" ResetPat
            , flatBtn "DICE" Dice
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
    , HH.div [ style "height:1px;background:#00000018;margin:14px 0 10px" ] []
    , HH.div [ style "display:flex;flex-direction:column;align-items:center;gap:2px" ]
        [ grLabel "OPEN HAT"
        , bigKnob KOpen ohColor "OPEN" s.bal
        , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;text-align:center;line-height:1.5;max-width:200px;margin-top:4px" ]
            [ HH.text "TURNS THE STRESSED HH HITS INTO OPEN HATS (TEAL), LOUDEST FIRST — IT CHOKES THE CLOSED HIT AND RINGS LONGER." ]
        ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.55;text-align:center;line-height:1.5;margin-top:12px" ]
        [ HH.text "DRAG ANY HEATMAP CELL UP / DOWN TO RATCHET IT" ]
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
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;margin-top:14px;line-height:1.6;max-width:640px" ]
        [ HH.text "THE 3 GRIDS VOICES (BD · SD · HH). FAINT = THE INTERPOLATED LANDSCAPE THE X/Y CURSOR SELECTS; SOLID = WHAT FIRES AT THIS DENSITY. DRAG A CELL UP/DOWN TO RATCHET IT." ]
    ]

heatSvg :: forall m. State -> H.ComponentHTML Action () m
heatSvg s =
  let
    b = s.bal
    cols = 32
    colW = 16.0
    rowH = 30.0
    nLanes = 3
    laneY lane = toNumber lane * rowH
    w = toNumber cols * colW
    h = toNumber nLanes * rowH
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
        opens = lane == 2 && fires && M.opensAt b step   -- an open hat here
        n = M.ratchetAt b lane step
        x = toNumber step * colW
        y = laneY lane
        c = if opens then ohColor else instColor lane
        landscape = rectBlock x y (colW - 1.0) (rowH - 1.0) c (toNumber level / 255.0 * 0.32)
        segs = if not fires then [] else stackBlocks x y c n (if accent then 0.95 else 0.7)
        acc = if accent then [ accentOutline x y ] else []
        hint = if n > 1 && not fires then [ ratchetHint x y c n ] else []
      in
        [ landscape ] <> segs <> acc <> hint
    -- Grids cells have no plain action (hits come from X/Y); a vertical drag
    -- sets the beat's ratchet (up = more retriggers).
    gridsTarget lane step =
      let
        x = toNumber step * colW
        y = laneY lane
      in
        svgEl "rect"
          [ svgAttr "x" (show x), svgAttr "y" (show y)
          , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
          , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:ns-resize;pointer-events:all"
          , svgMouse "mousedown" \_ -> StartDrag (DCell lane step) (M.ratchetAt b lane step) ] []
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
    rowLabel lane =
      svgEl "text"
        [ svgAttr "x" "3", svgAttr "y" (show (laneY lane + 11.0))
        , svgAttr "fill" "#3f3c33", svgAttr "fill-opacity" "0.55", svgAttr "style" "pointer-events:none"
        , svgAttr "font-size" "8", svgAttr "font-family" "Georgia,serif" ]
        [ HH.text ("Grids " <> M.instName lane) ]
    visuals =
      range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> gridsCell lane step
    targets =
      range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> [ gridsTarget lane step ]
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h)
      , svgAttr "width" "100%", svgAttr "style" "display:block;max-height:90vh" ]
      ( visuals <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
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

