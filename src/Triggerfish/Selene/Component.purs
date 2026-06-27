-- | Triggerfish.Selene.Component — the polysignal rack, as a stack of
-- | destinations. Each destination is a group of eight signals (one generator
-- | kind) bound to a physical target; you add destinations in groups of eight
-- | and configure each in place.
-- |
-- | The visual language: every slot is drawn, not formed — LFOs as scaled,
-- | log-frequency waveforms; Euclids as step-rings; clocks + notes as numbers;
-- | trig lanes as step-rows / Euclid rings with a route strip. The SOURCE pane
-- | is the editable authority.
-- |
-- | Output (this pass): **POLYTRIG plays over MIDI**, on the shared Binnacle
-- | clock, under the master transport — each jack's own pattern stacked with
-- | the route onsets addressed to it, scheduled at true fractional times. The
-- | es9 CV/gate path (LFO/Euclid/Clock/Note → ES-9 buses) and FH-2 delegation
-- | are the next increment; non-MIDI targets are silent for now.
module Triggerfish.Selene.Component (component) where

import Prelude

import Data.Array (filter, length, mapWithIndex, range, (!!))
import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Number (cos, pi, sin) as Num
import Data.String.Common (joinWith, toLower)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Triggerfish.Odonus.Grid.Widgets (engrave, style, svgAttr, svgEl)
import Triggerfish.Selene.Model as M
import Triggerfish.Selene.Source as Source
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Tidal.Lane as Lane
import Data.Maybe (Maybe(..), fromMaybe)

-- ---------------------------------------------------------------------------
-- State / Actions
-- ---------------------------------------------------------------------------

-- | The SOURCE document is the authority for the rack: `sel` is its parsed
-- | projection. The rest is the transport (mirrors Balistes): an ARM flag that
-- | sounds only under the shell's master, the Binnacle clock + MIDI out, and
-- | the live clock readouts.
type State =
  { sel :: M.Selene
  , doc :: String
  , running :: Boolean        -- ARM/cue (sticky); sounds only when master too
  , master :: Boolean         -- the shell's master transport (via SetMaster)
  , playStep :: Int
  , binnacle :: Maybe Binnacle.Binnacle
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBar :: Int
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ToggleArm
  | AddDest M.GenKind         -- append a template block (comment-safe)
  | SetDoc String             -- the whole editable document, verbatim

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        let doc = Source.printRack M.defaultSelene
        in
          { sel: Source.parseRack doc, doc
          , running: false, master: false, playStep: 0
          , binnacle: Nothing, midiOut: Nothing, midiName: "…"
          , clockTempo: 120.0, clockLocked: false, clockBar: 0
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

handleQuery :: forall o m a. MonadAff m => Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply s.doc))
  SyncFree startMicros tempo next -> do
    s <- H.get
    for_ s.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  FeedChords _ next -> pure (Just next)
  FeedVoiceChords _ next -> pure (Just next)   -- no chord quantiser
  SetMaster m next -> do
    H.modify_ _ { master = m }
    pure (Just next)

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (Step tick)
    { emitter: frameE, listener: frameL } <- liftEffect HS.create
    _ <- H.subscribe frameE
    _ <- liftEffect $ setInterval 33 (HS.notify frameL Frame)
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
      let playedStep = tick.index `mod` cycleSteps
      H.modify_ _ { playStep = playedStep }
      for_ st.midiOut \out -> liftEffect $
        for_ st.sel.destinations \d -> case d.target of
          M.Midi ch -> emitDestination out (ch - 1) st.clockTempo tick playedStep d.bank
          _ -> pure unit   -- non-MIDI targets await the es9 output path

  Frame -> do
    st <- H.get
    for_ st.binnacle \bin -> do
      r <- liftEffect $ Clock.read (Binnacle.clock bin)
      H.modify_ _ { clockTempo = r.tempo, clockLocked = r.locked, clockBar = r.bar }

  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }

  ToggleArm -> H.modify_ \s -> s { running = not s.running }

  -- append a fresh block to the document (so existing comments survive), then
  -- re-derive the rack from the new text.
  AddDest k -> H.modify_ \s ->
    let block = Source.printDest { target: M.defaultTargetFor k, range: M.Bipolar5V, bank: M.freshBank k }
        doc = s.doc <> "\n\n" <> block
    in s { doc = doc, sel = Source.parseRack doc }
  SetDoc doc -> H.modify_ \s -> s { doc = doc, sel = Source.parseRack doc }

-- ---------------------------------------------------------------------------
-- Emit — POLYTRIG over MIDI (other kinds await the es9 path)
-- ---------------------------------------------------------------------------

-- | One Tidal cycle == `cycleSteps` grid steps (one bar). A trig jack's onsets
-- | (its own pattern stacked with the route onsets addressed to its name) that
-- | fall in THIS step's window fire at their true fractional sub-step time.
emitDestination :: Midi.MidiOut -> Int -> Number -> Scheduler.Tick -> Int -> M.GenBank -> Effect Unit
emitDestination out channel tempo tick playedStep bank = case bank of
  M.GTrig tb ->
    let
      stepMs = 0.25 * 60000.0 / max 30.0 tempo
      lo = toNumber playedStep / toNumber cycleSteps
      hi = toNumber (playedStep + 1) / toNumber cycleSteps
      inWin o = o >= lo && o < hi
      routeOns name = map _.at (filter (\e -> eqName e.name name) (concatNamed tb.routes))
      fireJack jack =
        let
          own = filter inWin (Lane.onsetsOf jack.source)
          routed = filter inWin (routeOns jack.name)
          fire o =
            let sub = (o * toNumber cycleSteps - toNumber playedStep) * stepMs
            in Midi.scheduleNote out
                 { channel, note: jack.note, velocity: trigVel, delayMs: tick.delayMs + sub, durMs: trigGateMs }
        in
          for_ (own <> routed) fire
    in
      for_ tb.jacks fireJack
  _ -> pure unit   -- LFO/Euclid/Clock/Note → es9 CV/gate, next increment

-- All named onsets across every route line of a trig block.
concatNamed :: Array String -> Array { name :: String, at :: Number }
concatNamed = (=<<) Lane.namedOnsetsOf

eqName :: String -> String -> Boolean
eqName a b = toLower a == toLower b

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Transport constants (mirror Odonus/Balistes)
-- ---------------------------------------------------------------------------

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | One Tidal cycle == one bar (16 sixteenths), so `x*4` is four hits on the
-- | beat. The absolute grid index is the pulse, so Selene shares the rig downbeat.
cycleSteps :: Int
cycleSteps = 16

midiPortName :: String
midiPortName = "IAC"

trigVel :: Int
trigVel = 100

trigGateMs :: Number
trigGateMs = 40.0

accent :: String
accent = "#3f6f8a"   -- steel-blue, Selene's electric accent

-- A distinct muted violet for the lane-spanning route layer, so a routed
-- gesture reads apart from the steel jacks (echoes Balistes' route accent).
routeAccent :: String
routeAccent = "#6a5f8a"

ink :: String
ink = "#2b2922"

-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;inset:0;display:flex;align-items:stretch;overflow:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ rackPanel s
    , sourcePanel s
    ]

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
-- The rack: a stack of destinations + the add bar
-- ---------------------------------------------------------------------------

rackPanel :: forall m. State -> H.ComponentHTML Action () m
rackPanel s =
  panel "SELENE · DESTINATIONS" "flex:1 1 auto;min-width:0"
    ( [ transportStrip s ]
        <> mapWithIndex destinationRow s.sel.destinations
        <> [ addBar, footNote ]
    )

-- A compact horizontal transport: the ARM/cue toggle (sounds only under the
-- shell master) + live clock + MIDI readouts. POLYTRIG plays through it today.
transportStrip :: forall m. State -> H.ComponentHTML Action () m
transportStrip s =
  HH.div
    [ style $ "display:flex;align-items:center;gap:14px;margin-bottom:14px;padding:8px 10px;"
        <> "border-radius:7px;background:#00000008;border:1px solid #00000012" ]
    [ HH.button
        [ HE.onClick \_ -> ToggleArm
        , style $ "padding:8px 16px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
            <> "font-family:Georgia,serif;font-size:13px;letter-spacing:0.1em;color:#1c1a12;"
            <> "background:" <> (if s.running then "linear-gradient(#8fb0c0,#7a9eb0)" else "linear-gradient(#efece1,#ddd9cb)") ]
        [ HH.text (if s.running then (if s.master then "❚❚ PLAYING" else "◆ CUED") else "▶ ARM") ]
    , stat "TEMPO" (show (round s.clockTempo) <> " bpm" <> (if s.clockLocked then " ⛓" else " ·"))
    , stat "BAR" (show s.clockBar <> " · step " <> show (s.playStep + 1) <> "/" <> show cycleSteps)
    , stat "MIDI" s.midiName
    ]
  where
  stat label val =
    HH.div [ style "display:flex;flex-direction:column;gap:1px" ]
      [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55" ] [ HH.text label ]
      , HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:" <> ink ] [ HH.text val ]
      ]

addBar :: forall m. H.ComponentHTML Action () m
addBar =
  HH.div [ style "display:flex;align-items:center;gap:8px;margin-top:14px" ]
    ( [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.6;margin-right:2px" ] [ HH.text "ADD 8 →" ] ]
        <> map addButton M.allKinds
    )

addButton :: forall m. M.GenKind -> H.ComponentHTML Action () m
addButton k =
  HH.button
    [ HE.onClick \_ -> AddDest k
    , style $ "padding:6px 11px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.08em;color:#3f3c33;"
        <> "background:linear-gradient(#efece1,#ddd9cb)" ]
    [ HH.text (M.kindLabel k) ]

footNote :: forall m. H.ComponentHTML Action () m
footNote =
  HH.div [ style $ engrave <> ";font-size:8px;opacity:0.45;margin-top:14px;line-height:1.6" ]
    [ HH.text "EACH DESTINATION = 8 SIGNALS → 8 JACKS. EDIT THE NUMBERS — AND RE-PATCH / REMOVE BLOCKS — IN THE SOURCE PANE. -- MUTES A SLOT." ]

-- One destination: a target/header strip on the left, eight visualised slots.
destinationRow :: forall m. Int -> M.Destination -> H.ComponentHTML Action () m
destinationRow i d =
  HH.div
    [ style $ "display:flex;align-items:stretch;gap:12px;padding:11px 12px;margin-bottom:10px;border-radius:8px;"
        <> "background:#00000008;border:1px solid #00000012" ]
    [ destHeader i d
    , HH.div [ style (slotWrap d.bank) ] (slotViews d.bank)
    ]

-- | The slot layout per kind. POLYTRIG lays its eight lanes 4-to-a-row (a 4×2
-- | grid, wide columns) so real mini-notation stays legible; the compact kinds
-- | flow eight-across.
slotWrap :: M.GenBank -> String
slotWrap = case _ of
  M.GTrig _ -> "display:grid;grid-template-columns:repeat(4,1fr);gap:7px;flex:1 1 auto;min-width:0"
  _ -> "display:flex;flex-wrap:wrap;gap:7px;align-items:center;flex:1 1 auto"

destHeader :: forall m. Int -> M.Destination -> H.ComponentHTML Action () m
destHeader _ d =
  HH.div [ style "display:flex;flex-direction:column;gap:5px;flex:0 0 132px;justify-content:center" ]
    [ HH.div [ style $ engrave <> ";font-size:10px;letter-spacing:0.1em;color:" <> ink ]
        [ HH.text (M.kindLabel (M.bankKind d.bank)) ]
    , HH.div
        [ style $ "padding:4px 8px;border-radius:5px;text-align:left;display:inline-block;align-self:flex-start;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#efece1;background:" <> accent ]
        [ HH.text (M.targetLabel d.target) ]
    , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.45" ]
        [ HH.text ("→ " <> M.targetWire d.target) ]
    ]

-- ---------------------------------------------------------------------------
-- Per-kind slot visualisations
-- ---------------------------------------------------------------------------

slotViews :: forall m. M.GenBank -> Array (H.ComponentHTML Action () m)
slotViews = case _ of
  M.GLfo slots -> map lfoCell slots
  M.GEuclid slots -> map euclidRing slots
  M.GClock slots -> mapWithIndex clockNumber slots
  M.GNote slots -> map noteCell slots
  M.GTrig tb -> map trigCell tb.jacks <> map routeRow tb.routes

-- --- POLYLFO: a scaled waveform, 0V baseline, log-frequency, rate label ------

lfoCell :: forall m. M.ModSlot -> H.ComponentHTML Action () m
lfoCell sl =
  let
    w = 84.0
    h = 50.0
    mid = h / 2.0
    -- normalise the summed wave into the cell (90% of half-height)
    span = max 1.0 (abs sl.level + sl.sin + sl.sqr + sl.tri + abs sl.saw)
    samples = 48
    pt k =
      let
        u = toNumber k / toNumber samples
        x = u * w
        y = mid - (M.lfoValue sl u / span) * (mid * 0.9)
      in
        show (round2 x) <> "," <> show (round2 y)
    poly = joinWith " " (map pt (range 0 samples))
    hint = lfoShapeHint sl
  in
    cellBox 90.0
      [ svgEl "svg"
          [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h), svgAttr "width" "100%"
          , svgAttr "height" (show h), svgAttr "style" "display:block;overflow:visible" ]
          [ svgEl "line"
              [ svgAttr "x1" "0", svgAttr "y1" (show mid), svgAttr "x2" (show w), svgAttr "y2" (show mid)
              , svgAttr "stroke" "#00000022", svgAttr "stroke-width" "0.8", svgAttr "stroke-dasharray" "2 2" ] []
          , svgEl "polyline"
              [ svgAttr "points" poly, svgAttr "fill" "none", svgAttr "stroke" accent
              , svgAttr "stroke-width" "1.6", svgAttr "stroke-linejoin" "round" ] []
          ]
      , cellCaption (fmt2 sl.rate <> " Hz" <> hint)
      ]

-- which non-drawn shapes are present, as a tiny tag
lfoShapeHint :: M.ModSlot -> String
lfoShapeHint sl =
  let tags = filter (_ /= "") [ if sl.rnd > 0.0 then "+rnd" else "", if sl.nse > 0.0 then "+nse" else "" ]
  in if length tags == 0 then "" else " " <> joinWith "" tags

-- --- POLYEUCLID: a ring of step-dots with k / n in the centre ----------------

euclidRing :: forall m. M.EuclidSlot -> H.ComponentHTML Action () m
euclidRing sl = cellBox 70.0 [ ringFigure 64.0 sl.beats sl.steps ]

-- | The Euclidean ring — a dot per step, filled on a pulse, k/n in the centre.
-- | Shared by POLYEUCLID and any POLYTRIG jack whose source is a pure Euclid, so
-- | the same rhythm reads identically wherever it lives (structure-driven viz).
ringFigure :: forall m. Number -> Int -> Int -> H.ComponentHTML Action () m
ringFigure sz k n =
  let
    c = sz / 2.0
    r = c - 8.0
    bits = M.euclidBits { beats: k, steps: n, rate: 0, accentRate: 0 }
    nb = length bits
    dotFor i on =
      let
        ang = (toNumber i / toNumber (max 1 nb)) * 2.0 * pi - pi / 2.0
        dx = c + r * cos ang
        dy = c + r * sin ang
        rad = if on then 3.4 else 2.0
      in
        svgEl "circle"
          [ svgAttr "cx" (show (round2 dx)), svgAttr "cy" (show (round2 dy)), svgAttr "r" (show rad)
          , svgAttr "fill" (if on then accent else "none")
          , svgAttr "stroke" accent, svgAttr "stroke-width" (if on then "0" else "1") ] []
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show sz <> " " <> show sz), svgAttr "width" "100%"
      , svgAttr "height" (show sz), svgAttr "style" "display:block" ]
      ( mapWithIndex dotFor bits
          <> [ svgEl "text"
                 [ svgAttr "x" (show c), svgAttr "y" (show (c + 4.0)), svgAttr "text-anchor" "middle"
                 , svgAttr "fill" ink, svgAttr "font-family" "'SF Mono',Menlo,monospace"
                 , svgAttr "font-size" "13" ]
                 [ HH.text (show k <> "/" <> show n) ]
             ]
      )

-- --- POLYCLOCK: a list of division numbers -----------------------------------

clockNumber :: forall m. Int -> M.ClockSlot -> H.ComponentHTML Action () m
clockNumber _ sl =
  cellBox 58.0
    [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:18px;color:" <> ink <> ";text-align:center" ]
        [ HH.text ("×" <> show sl.multiplier) ]
    , cellCaption (M.clockBaseLabel sl.base <> " · " <> show sl.pulseWidth <> "%")
    ]

-- --- POLYNOTE: a list of notes -----------------------------------------------

noteCell :: forall m. M.PresetNoteSlot -> H.ComponentHTML Action () m
noteCell sl =
  cellBox 58.0
    [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:16px;color:" <> ink <> ";text-align:center" ]
        [ HH.text (M.noteName sl.note) ]
    , cellCaption ("midi " <> show sl.note)
    ]

-- --- POLYTRIG: a linear step row, lit at the pattern's onset cells -----------

-- One POLYTRIG jack: its name + note, then a figure that follows the source's
-- structure — the Euclid ring when the source is a pure `x(k,n)`, otherwise a
-- linear step row. An empty source (the jack is driven only by routes) shows a
-- faint "↳ route" caption.
trigCell :: forall m. M.TrigSlot -> H.ComponentHTML Action () m
trigCell sl =
  let
    figure = case Lane.euclidOf sl.source of
      Just e -> ringFigure 46.0 e.k e.n
      Nothing -> stepFigure sl.source
    caption = if sl.source == "" then "↳ route" else sl.source
  in
    HH.div
      [ style $ "padding:5px 6px;border-radius:6px;background:#ffffff55;border:1px solid #00000010;"
          <> "display:flex;flex-direction:column;gap:4px;align-items:stretch;min-width:0" ]
      [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;gap:6px" ]
          [ HH.span [ style $ "font-family:Georgia,serif;font-size:11px;color:" <> ink ] [ HH.text sl.name ]
          , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.5" ] [ HH.text (M.noteName sl.note) ]
          ]
      , figure
      , cellCaption caption
      ]

-- A linear step row, lit at the pattern's onset cells (HTML so it fills width).
stepFigure :: forall m. String -> H.ComponentHTML Action () m
stepFigure src =
  let
    m = Lane.meterOf src
    mask = Lane.cellMaskOf src
    stepDiv k =
      let on = fromMaybe false (mask !! k)
      in
        HH.div
          [ style $ "flex:1 1 0;min-width:0;height:18px;border-radius:2px;"
              <> (if on then "background:" <> accent
                  else "background:#00000008;border:1px solid " <> accent <> "55;box-sizing:border-box") ]
          []
  in
    HH.div [ style "display:flex;gap:2px;width:100%;height:18px;align-items:center" ]
      (map stepDiv (range 0 (m - 1)))

-- A lane-spanning route, full-width across the 4-column grid: its atoms placed
-- at their true fractional times, each tick labelled with the jack it fires.
routeRow :: forall m. String -> H.ComponentHTML Action () m
routeRow src =
  let
    ons = Lane.namedOnsetsOf src
    mark o =
      HH.div
        [ style $ "position:absolute;top:0;left:" <> show (round2 (o.at * 100.0)) <> "%;"
            <> "transform:translateX(-50%);display:flex;flex-direction:column;align-items:center;gap:1px" ]
        [ HH.div [ style $ "width:0;height:11px;border-left:2px solid " <> routeAccent ] []
        , HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:8px;color:" <> routeAccent ] [ HH.text o.name ]
        ]
  in
    HH.div
      [ style $ "grid-column:1 / -1;display:flex;align-items:center;gap:10px;padding:4px 8px;min-width:0;"
          <> "border-radius:6px;background:#ffffff35;border:1px dashed " <> routeAccent <> "44" ]
      [ HH.span [ style $ engrave <> ";font-size:8px;color:" <> routeAccent <> ";flex:0 0 auto" ] [ HH.text "ROUTE" ]
      , HH.div [ style "position:relative;flex:1 1 auto;height:24px;min-width:40px" ] (map mark ons)
      , HH.span
          [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#2b2922;opacity:0.65;flex:0 0 auto" ]
          [ HH.text ("\"" <> src <> "\"") ]
      ]

-- ---------------------------------------------------------------------------
-- Cell chrome
-- ---------------------------------------------------------------------------

cellBox :: forall m. Number -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
cellBox widthPx body =
  HH.div
    [ style $ "width:" <> show (round widthPx) <> "px;flex:0 0 auto;padding:5px 4px;border-radius:6px;"
        <> "background:#ffffff55;border:1px solid #00000010;display:flex;flex-direction:column;gap:2px;align-items:center" ]
    body

cellCaption :: forall m. String -> H.ComponentHTML Action () m
cellCaption t =
  HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55;text-align:center" ] [ HH.text t ]

-- ---------------------------------------------------------------------------
-- Source panel — the growing-spec eDSL, one block per destination (read-only)
-- ---------------------------------------------------------------------------

sourcePanel :: forall m. State -> H.ComponentHTML Action () m
sourcePanel s =
  panel "SOURCE" "flex:0 0 340px"
    [ HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:6px" ]
        [ HH.text "THE RACK · EDIT THE NUMBERS · -- MUTES A SLOT" ]
    , HH.textarea
        [ HP.value s.doc
        , HE.onValueInput SetDoc
        , HP.spellcheck false
        , style $ "flex:1 1 auto;min-height:420px;resize:none;box-sizing:border-box;"
            <> "padding:9px 10px;border:1px solid #a8a392;border-radius:6px;background:#f4f1e8;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;line-height:1.5;color:#2b2922;"
            <> "white-space:pre;overflow:auto;outline:none" ]
    ]

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

pi :: Number
pi = Num.pi

cos :: Number -> Number
cos = Num.cos

sin :: Number -> Number
sin = Num.sin

abs :: Number -> Number
abs x = if x < 0.0 then -x else x

round2 :: Number -> Number
round2 x = toNumber (round (x * 100.0)) / 100.0

fmt2 :: Number -> String
fmt2 x = show (toNumber (round (x * 100.0)) / 100.0)
