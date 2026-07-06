-- | Triggerfish.Stellatus — the sixth instrument. A circular, pattern-placed,
-- | stochastic sample re-sequencer, taking Kymatica's Sector as a jumping-off
-- | point and re-thinking it in Tidal terms (docs/STELLATUS-DESIGN.md).
-- |
-- | The one reframe: THE RING IS ONE TIDAL CYCLE. Samples are *placed* on it by
-- | a pattern — arc onset = event time, arc length = event span.
-- |
-- | Programmed in TEXT (AC steer), not knobs. Two floating panels hold the
-- | Lepidoptera surface: a KIT (name → sample) and a PLAYER (a `place` pattern
-- | OR a `slice N`, plus SuperDirt verbs, glitch combinators, and a `jump`
-- | matrix). As of S1b the whole PLAYER is LIVE — placement runs through the
-- | real Tidal parser; `# speed/gain/begin/end` are number patterns sampled at
-- | each arc's onset; `# sometimes/# rarely` roll per-hit warps; and the `jump`
-- | matrix steers the walk's leaps (all in Stellatus.Lang). Pure visualizer
-- | (rig-only, no browser audio). Its own dark radar aesthetic.
module Triggerfish.Stellatus.Component (component) where

import Prelude

import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Transport as Transport
import Data.Array (concatMap, elemIndex, find, head, length, mapWithIndex, range, (!!))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (cos, pi, sin) as Num
import Data.String (Pattern(..))
import Data.String.CodeUnits (drop, indexOf, stripSuffix, take) as SCU
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Reef.Stellatus.Engine (Emit, Scene, Slot, walk) as SE
import Reef.Stellatus.Engine (GlitchRule) as SEG
import Reef.Stellatus.Protocol (encodeScene) as SP
import Triggerfish.Odonus.Grid.Widgets (style, svgEl, svgAttr, engrave)
import Triggerfish.Stellatus.Lang (Arc, ArcParams, GlitchEffect(..), GlitchRule, JumpSpec, KitEntry, Mode(..), parseScene)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type State =
  { kitText :: String
  , playerText :: String
  , kitOpen :: Boolean
  , playerOpen :: Boolean
  , seed :: Int
  , beat :: Number
  , playing :: Boolean
  -- parsed from the panels: the live ring (S1a) + playback steering (S1b)
  , arcs :: Array Arc
  , mode :: Mode
  , kit :: Array KitEntry
  , kitNames :: Array String
  , params :: Array ArcParams
  , glitch :: Array GlitchRule
  , jumps :: JumpSpec
  , parseErr :: Maybe String
  -- the resolved reef Scene (the shipping wire form) + rig transport
  , scene :: SE.Scene
  , binnacle :: Maybe Binnacle.Binnacle
  }

data Action
  = Init | Tick
  | SetKitText String | SetPlayerText String
  | ToggleKit | TogglePlayer
  | Shake | TogglePlay | PushScene | StopRig

kitTextDefault :: String
kitTextDefault =
  "-- KIT   name = sample:index  (Dirt-Samples folders)\n"
    <> "bd = \"808bd:3\"\n"
    <> "sn = \"sn:4\"\n"
    <> "hh = \"hh27:6\"\n"
    <> "cp = \"cp:1\""

playerTextDefault :: String
playerTextDefault =
  "-- PLACEMENT   the ring is one cycle  (place = KIT, slice N = one buffer)\n"
    <> "place \"bd sn hh*2 cp sn\"\n"
    <> "\n"
    <> "-- PLAYBACK   verbs sampled at each arc's onset\n"
    <> "# speed \"1 1 2 1 0.5\"\n"
    <> "# begin \"0 0 0.5 0 0\"\n"
    <> "# gain  \"1 0.9 0.8 1 0.85\"\n"
    <> "\n"
    <> "-- GLITCH   stochastic per-hit warps\n"
    <> "# sometimes rev\n"
    <> "# rarely (# speed 2)\n"
    <> "\n"
    <> "-- JUMPS   leap instead of advance (name -> targets weight)\n"
    <> "jump 0.22\n"
    <> "  bd -> sn 0.6  hh 0.4\n"
    <> "  sn -> cp 0.5  bd 0.5\n"
    <> "  hh -> hh 0.7  sn 0.3\n"
    <> "  cp -> bd 1"

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

emptyScene :: SE.Scene
emptyScene = { slots: [], glitch: [], jumps: { prob: 0.0, table: [] }, seed: 3 }

-- Reparse the panels into the live scene; keep the last good ring on error. Also
-- resolves the reef Scene (slots + wire glitch + jumps) — the shipping wire form
-- pushed to the rig and the source of truth for the visualizer's walk.
reparse :: State -> State
reparse st = case parseScene st.kitText st.playerText of
  Right sc -> st
    { arcs = sc.arcs, mode = sc.mode, kit = sc.kit, kitNames = map _.name sc.kit
    , params = sc.params, glitch = sc.glitch, jumps = sc.jumps, parseErr = Nothing
    , scene = resolveScene st.seed sc.mode sc.kit sc.params sc.arcs sc.glitch sc.jumps }
  Left e -> st { parseErr = Just e }

-- Build the reef Scene: resolve each arc into a Slot (sample + window + base
-- speed/gain), project glitch effects to the wire shape, pass the jump table
-- through (structurally identical). The rig runs the walk from exactly this.
resolveScene
  :: Int -> Mode -> Array KitEntry -> Array ArcParams -> Array Arc
  -> Array GlitchRule -> JumpSpec -> SE.Scene
resolveScene seed mode kit params arcs glitch jumps =
  { slots: mapWithIndex (slotOf mode kit params) arcs
  , glitch: map wireGlitch glitch
  , jumps
  , seed
  }

slotOf :: Mode -> Array KitEntry -> Array ArcParams -> Int -> Arc -> SE.Slot
slotOf mode kit params i arc =
  let smp = case mode of
        KitMode ->
          let sp = splitSrc (fromMaybe "" (map _.src (find (\e -> e.name == arc.name) kit)))
          in { s: sp.folder, n: sp.n, begin: 0.0, end: 1.0 }
        BufferMode ->
          let sp = splitSrc (fromMaybe "" (map _.src (head kit)))
          in { s: sp.folder, n: sp.n, begin: arc.onset, end: arc.onset + arc.span }
      p = fromMaybe defaultParams (params !! i)
  in { name: arc.name, onset: arc.onset, span: arc.span
     , s: smp.s, n: smp.n
     , begin: fromMaybe smp.begin p.begin
     , end: fromMaybe smp.end p.end
     , speed: p.speed, gain: p.gain }

-- Lang's glitch effect ADT → the wire shape reef reads: kind 0 = reverse, 1 =
-- speed×amount.
wireGlitch :: GlitchRule -> SEG.GlitchRule
wireGlitch r = case r.effect of
  GReverse -> { prob: r.prob, kind: 0, amount: 0.0 }
  GSpeed m -> { prob: r.prob, kind: 1, amount: m }

-- ---------------------------------------------------------------------------
-- Geometry (SVG; 0 rad = 12 o'clock, increasing clockwise)
-- ---------------------------------------------------------------------------

vb :: Number
vb = 720.0

cx :: Number
cx = 360.0

cy :: Number
cy = 360.0

ringR :: Number
ringR = 288.0

hubR :: Number
hubR = 64.0

tau :: Number
tau = 2.0 * Num.pi

ptx :: Number -> Number -> Number
ptx r a = cx + r * Num.sin a

pty :: Number -> Number -> Number
pty r a = cy - r * Num.cos a

arcPath :: Number -> Number -> Number -> String
arcPath r a0 a1 =
  let large = if (a1 - a0) > Num.pi then "1" else "0"
  in "M" <> show (ptx r a0) <> " " <> show (pty r a0)
       <> " A" <> show r <> " " <> show r <> " 0 " <> large <> " 1 "
       <> show (ptx r a1) <> " " <> show (pty r a1)

-- ---------------------------------------------------------------------------
-- Deterministic helpers (hash, warp, walk)
-- ---------------------------------------------------------------------------

hashNoise :: Int -> Int -> Number
hashNoise seed i =
  let x = Num.sin (toNumber (seed * 374761 + i * 668265 + 9127)) * 43758.5453
  in x - toNumber (floor x)

-- ---------------------------------------------------------------------------
-- Scene resolution helpers (shared by the reef Scene build above)
-- ---------------------------------------------------------------------------

-- `folder:index` → the SuperDirt `s`/`n` pair.
splitSrc :: String -> { folder :: String, n :: Int }
splitSrc src = case SCU.indexOf (Pattern ":") src of
  Just i -> { folder: SCU.take i src, n: fromMaybe 0 (Int.fromString (SCU.drop (i + 1) src)) }
  Nothing -> { folder: src, n: 0 }

defaultParams :: ArcParams
defaultParams = { speed: 1.0, gain: 0.9, begin: Nothing, end: Nothing }

-- ---------------------------------------------------------------------------
-- Component
-- ---------------------------------------------------------------------------

component :: forall m. MonadAff m => H.Component (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> reparse
        { kitText: kitTextDefault, playerText: playerTextDefault
        , kitOpen: true, playerOpen: true, seed: 3, beat: 0.0, playing: true
        , arcs: [], mode: KitMode, kit: [], kitNames: [], parseErr: Nothing
        , params: [], glitch: [], jumps: { prob: 0.0, table: [] }
        , scene: emptyScene, binnacle: Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Init -> do
    -- Open the rig transport (shared with the other instruments). The BEAM is the
    -- audio authority; this browser only pushes the Scene and visualizes it.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    H.modify_ _ { binnacle = Just bin }
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    _ <- liftEffect $ setInterval 45 (HS.notify listener Tick)
    pure unit
  -- Read the rig's Link beat so the highlight sweep is aligned to what the BEAM
  -- actually plays (Binnacle phase-locks the clock to the rig's anchor; free-runs
  -- at the fallback tempo when no anchor). The walk CONTENT already matched; this
  -- aligns the sweep PHASE too (D2).
  Tick -> do
    st <- H.get
    when st.playing $ for_ st.binnacle \bin -> do
      r <- liftEffect $ Clock.read (Binnacle.clock bin)
      H.modify_ _ { beat = r.beat }
  SetKitText t -> H.modify_ (reparse <<< _ { kitText = t })
  SetPlayerText t -> H.modify_ (reparse <<< _ { playerText = t })
  ToggleKit -> H.modify_ \st -> st { kitOpen = not st.kitOpen }
  TogglePlayer -> H.modify_ \st -> st { playerOpen = not st.playerOpen }
  Shake -> H.modify_ \st ->
    let s = mod (st.seed * 1103515245 + 12345) 2147483 + 1
    in st { seed = s, scene = st.scene { seed = s } }
  TogglePlay -> H.modify_ \st -> st { playing = not st.playing }
  -- Push the resolved Scene to the rig (reef_stellatus_voice runs the walk).
  PushScene -> do
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) ("stellatus-scene " <> SP.encodeScene st.scene)
  StopRig -> do
    st <- H.get
    for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "stellatus-stop"

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

ink :: String
ink = "#9aa4b0"

cyanAccent :: String
cyanAccent = "#4fd0e0"

arcCol :: Mode -> Array String -> Int -> Int -> String -> String
arcCol mode kitNames count idx name =
  let hsl h = "hsl(" <> show h <> ",68%,58%)"
  in case mode of
       KitMode -> case elemIndex name kitNames of
         Just i -> hsl (360.0 * toNumber i / toNumber (max 1 (length kitNames)))
         Nothing -> "#6f7885"
       BufferMode -> hsl (360.0 * toNumber idx / toNumber (max 1 count))

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;box-sizing:border-box;"
        <> "display:flex;flex-direction:column;background:#0c0e11;color:" <> ink
        <> ";font-family:Georgia,serif;overflow:hidden" ]
    [ nameplate
    , HH.div
        [ style "flex:1 1 auto;position:relative;min-height:0;display:flex;align-items:center;justify-content:center" ]
        [ ring st
        , panelStack st
        , transport st
        ]
    ]

nameplate :: forall m. H.ComponentHTML Action () m
nameplate =
  HH.div
    [ style $ "flex:0 0 auto;display:flex;align-items:center;justify-content:space-between;"
        <> "padding:9px 18px;background:#111418;border-bottom:1px solid #000" ]
    [ HH.div [ style "display:flex;align-items:baseline;gap:12px" ]
        [ HH.span [ style "font-family:Georgia,serif;font-size:15px;letter-spacing:0.28em;color:#dfe6ee" ]
            [ HH.text "TRIGGERFISH" ]
        , HH.span [ style "font-size:11px;letter-spacing:0.34em;color:#6f7885" ] [ HH.text "· MODEL STELLATUS" ]
        ]
    , HH.span
        [ style $ "font-size:9px;letter-spacing:0.18em;text-transform:uppercase;padding:4px 10px;border-radius:3px;"
            <> "background:#0f2a2e;border:1px solid #1c4a50;color:" <> cyanAccent ]
        [ HH.text "Rig-only · no browser audio" ]
    ]

-- ---------------------------------------------------------------------------
-- The ring
-- ---------------------------------------------------------------------------

ring :: forall m. State -> H.ComponentHTML Action () m
ring st =
  let count = length st.arcs
      walk = SE.walk st.scene
      len = length walk
      -- one ring slot per 1/16 (reef STEP_BEATS = 0.25 → 4 steps/beat), indexed
      -- exactly as reef_stellatus_voice does: absolute step `mod` loop length.
      s = st.beat * 4.0
      absStep = floor s
      step = if len <= 0 then 0 else ((absStep `mod` len) + len) `mod` len
      local = s - toNumber absStep
      cur = walk !! step
      curArc = fromMaybe 0 (map _.slot cur)
      arcs = mapWithIndex (\i a -> { i, a }) st.arcs
  in svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show vb <> " " <> show vb)
      , svgAttr "height" "100%"
      , svgAttr "preserveAspectRatio" "xMidYMid meet"
      , svgAttr "style" "display:block;max-height:calc(100vh - var(--tf-bar) - 40px)"
      ]
      ( ringGuide
        <> ringWave st.seed
        <> concatMap (arcView st.mode st.kitNames count curArc) arcs
        <> concatMap (paramGlyph st.params) arcs
        <> jumpChord st.arcs cur local
        <> centre st.arcs curArc count
      )

ringGuide :: forall m. Array (H.ComponentHTML Action () m)
ringGuide =
  [ svgEl "circle"
      [ svgAttr "cx" (show cx), svgAttr "cy" (show cy), svgAttr "r" (show ringR)
      , svgAttr "fill" "none", svgAttr "stroke" "#1b2026", svgAttr "stroke-width" "1" ] []
  ]

ringWave :: forall m. Int -> Array (H.ComponentHTML Action () m)
ringWave seed =
  let n = 168
      bar i =
        let a = tau * toNumber i / toNumber n
            mag = 0.22 + 0.78 * hashNoise (seed + 101) i
            r0 = ringR - 5.0
            r1 = r0 - mag * 40.0
        in svgEl "line"
             [ svgAttr "x1" (show (ptx r0 a)), svgAttr "y1" (show (pty r0 a))
             , svgAttr "x2" (show (ptx r1 a)), svgAttr "y2" (show (pty r1 a))
             , svgAttr "stroke" "#39424c", svgAttr "stroke-width" "1.4" ] []
  in map bar (range 0 (n - 1))

arcView :: forall m. Mode -> Array String -> Int -> Int -> { i :: Int, a :: Arc } -> Array (H.ComponentHTML Action () m)
arcView mode kitNames count curArc { i, a } =
  let a0 = a.onset * tau
      a1 = (a.onset + a.span) * tau
      pad = min 0.02 (a.span * 0.12)
      col = arcCol mode kitNames count i a.name
      lit = i == curArc
      mid = (a.onset + a.span / 2.0) * tau
  in [ svgEl "path"
         [ svgAttr "d" (arcPath ringR (a0 + pad) (a1 - pad))
         , svgAttr "fill" "none", svgAttr "stroke" col
         , svgAttr "stroke-width" (if lit then "13" else "8")
         , svgAttr "stroke-linecap" "round"
         , svgAttr "opacity" (if lit then "1" else "0.8")
         , svgAttr "style" (if lit then "filter:drop-shadow(0 0 7px " <> col <> ")" else "") ] []
     , svgEl "text"
         [ svgAttr "x" (show (ptx (ringR - 26.0) mid)), svgAttr "y" (show (pty (ringR - 26.0) mid + 4.0))
         , svgAttr "text-anchor" "middle"
         , svgAttr "style" ("font-family:Georgia,serif;font-size:13px;letter-spacing:0.04em;fill:"
             <> (if lit then "#eef3f8" else "#7a8490")) ]
         [ HH.text a.name ]
     ]

-- The base `# speed` verb, shown outside its arc so the ring reflects what the
-- text programmed: `◀` for reverse (negative), `×N` for a non-unit ratio. The
-- stochastic glitch rolls aren't shown (they differ per hit); this is the
-- steady-state steering.
paramGlyph :: forall m. Array ArcParams -> { i :: Int, a :: Arc } -> Array (H.ComponentHTML Action () m)
paramGlyph params { i, a } =
  let arcPx = a.span * tau * ringR
      lbl = case map _.speed (params !! i) of
        Just sp | sp < 0.0 -> "◀"
        Just sp | sp /= 1.0 -> "×" <> fmtNum sp
        _ -> ""
      ang = (a.onset + a.span / 2.0) * tau
  in if arcPx < 24.0 || lbl == "" then []
     else [ svgEl "text"
              [ svgAttr "x" (show (ptx (ringR + 20.0) ang)), svgAttr "y" (show (pty (ringR + 20.0) ang + 3.0))
              , svgAttr "text-anchor" "middle"
              , svgAttr "style" ("font-family:'SF Mono',monospace;font-size:11px;fill:" <> ink) ]
              [ HH.text lbl ] ]

-- Drop a trailing ".0" so 2.0 reads "2", 0.5 stays "0.5".
fmtNum :: Number -> String
fmtNum n = let s = show n in fromMaybe s (SCU.stripSuffix (Pattern ".0") s)

jumpChord :: forall m. Array Arc -> Maybe SE.Emit -> Number -> Array (H.ComponentHTML Action () m)
jumpChord arcs mstep local = fromMaybe [] do
  s <- mstep
  from <- s.from
  a <- arcs !! from
  b <- arcs !! s.slot
  let aa = (a.onset + a.span / 2.0) * tau
      ab = b.onset * tau
      op = show (0.85 * (1.0 - local))
  pure [ svgEl "line"
           [ svgAttr "x1" (show (ptx ringR aa)), svgAttr "y1" (show (pty ringR aa))
           , svgAttr "x2" (show (ptx ringR ab)), svgAttr "y2" (show (pty ringR ab))
           , svgAttr "stroke" cyanAccent, svgAttr "stroke-width" "2", svgAttr "opacity" op
           , svgAttr "style" ("filter:drop-shadow(0 0 5px " <> cyanAccent <> ")") ] [] ]

centre :: forall m. Array Arc -> Int -> Int -> Array (H.ComponentHTML Action () m)
centre arcs curArc count =
  let nm = fromMaybe "" (map _.name (arcs !! curArc))
  in [ svgEl "circle"
         [ svgAttr "cx" (show cx), svgAttr "cy" (show cy), svgAttr "r" (show hubR)
         , svgAttr "fill" "#0e1116", svgAttr "stroke" "#20262d", svgAttr "stroke-width" "1" ] []
     , svgEl "text"
         [ svgAttr "x" (show cx), svgAttr "y" (show (cy - 2.0)), svgAttr "text-anchor" "middle"
         , svgAttr "style" "font-family:Georgia,serif;font-size:20px;letter-spacing:0.04em;fill:#eef3f8" ]
         [ HH.text nm ]
     , svgEl "text"
         [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 16.0)), svgAttr "text-anchor" "middle"
         , svgAttr "style" ("font-family:'SF Mono',monospace;font-size:10px;letter-spacing:0.06em;fill:" <> ink) ]
         [ HH.text (show curArc <> " / " <> show count) ]
     ]

-- ---------------------------------------------------------------------------
-- The floating text panels — the instrument is programmed here
-- ---------------------------------------------------------------------------

panelStack :: forall m. State -> H.ComponentHTML Action () m
panelStack st =
  HH.div
    [ style "position:absolute;top:14px;left:14px;width:344px;display:flex;flex-direction:column;gap:10px;z-index:6" ]
    [ textPanel st.kitOpen ToggleKit "KIT" "names → samples" st.kitText SetKitText 6
    , textPanel st.playerOpen TogglePlayer "PLAYER" "place · verbs · jumps" st.playerText SetPlayerText 18
    , case st.parseErr of
        Just e ->
          HH.div
            [ style $ "font-family:'SF Mono',monospace;font-size:10px;color:#e88;"
                <> "background:#2a1416;border:1px solid #5a2226;border-radius:5px;padding:6px 9px" ]
            [ HH.text ("⚠ " <> e) ]
        Nothing ->
          HH.div [ style "font-size:9px;letter-spacing:0.1em;color:#4a525c;font-style:italic;padding-left:2px" ]
            [ HH.text "edit · → RIG to push · the BEAM plays it · pitch → Vetula next" ]
    ]

textPanel
  :: forall m
   . Boolean -> Action -> String -> String -> String -> (String -> Action) -> Int
  -> H.ComponentHTML Action () m
textPanel open toggle title sub value onInput rows =
  HH.div
    [ style $ "background:rgba(15,18,22,0.94);border:1px solid #263039;border-radius:8px;"
        <> "box-shadow:0 4px 18px #00000060;overflow:hidden;backdrop-filter:blur(3px)" ]
    ( [ HH.div
          [ HE.onClick \_ -> toggle
          , style $ "display:flex;align-items:baseline;justify-content:space-between;cursor:pointer;"
              <> "user-select:none;padding:9px 12px;border-bottom:" <> (if open then "1px solid #1c242c" else "0") ]
          [ HH.div [ style "display:flex;align-items:baseline;gap:9px" ]
              [ HH.span [ style $ engrave <> ";font-size:12px;letter-spacing:0.18em;color:#cfd6de" ] [ HH.text title ]
              , HH.span [ style "font-family:'SF Mono',monospace;font-size:9px;color:#5a626c" ] [ HH.text sub ]
              ]
          , HH.span [ style "font-size:10px;color:#6f7885" ] [ HH.text (if open then "▾" else "▸") ]
          ]
      ] <>
        if open then
          [ HH.textarea
              [ HP.value value
              , HE.onValueInput onInput
              , HP.spellcheck false
              , HP.rows rows
              , style $ "width:100%;box-sizing:border-box;resize:vertical;border:0;outline:0;"
                  <> "padding:10px 12px;background:transparent;color:#c6cdd5;"
                  <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11.5px;line-height:1.55;"
                  <> "tab-size:2;white-space:pre" ]
          ]
        else [] )

transport :: forall m. State -> H.ComponentHTML Action () m
transport st =
  HH.div
    [ style $ "position:absolute;top:14px;right:14px;display:flex;align-items:center;gap:8px;z-index:6;"
        <> "background:rgba(15,18,22,0.94);border:1px solid #263039;border-radius:8px;padding:8px 10px" ]
    [ HH.button
        [ HE.onClick \_ -> TogglePlay
        , style $ "padding:7px 13px;border-radius:6px;cursor:pointer;border:1px solid #2a333c;"
            <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.12em;color:" <> ink <> ";background:#1a1f25" ]
        [ HH.text (if st.playing then "❚❚ HOLD" else "▶ RUN") ]
    , HH.button
        [ HE.onClick \_ -> Shake
        , style $ "padding:7px 13px;border-radius:6px;cursor:pointer;border:1px solid #1c4a50;"
            <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.12em;color:#08181a;background:" <> cyanAccent ]
        [ HH.text "⟳ SHAKE" ]
    , HH.span [ style "width:1px;height:22px;background:#2a333c" ] []
    -- Push the resolved Scene to the rig; the BEAM runs the walk and emits to
    -- SuperDirt. STOP silences just this voice.
    , HH.button
        [ HE.onClick \_ -> PushScene
        , style $ "padding:7px 13px;border-radius:6px;cursor:pointer;border:1px solid #1c4a50;"
            <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.12em;color:#08181a;background:" <> cyanAccent ]
        [ HH.text "→ RIG" ]
    , HH.button
        [ HE.onClick \_ -> StopRig
        , style $ "padding:7px 13px;border-radius:6px;cursor:pointer;border:1px solid #2a333c;"
            <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.12em;color:" <> ink <> ";background:#1a1f25" ]
        [ HH.text "■ STOP" ]
    , HH.span [ style ("font-family:'SF Mono',monospace;font-size:10px;color:#5a626c;padding-left:2px") ]
        [ HH.text (maybe "connecting…" (const "rig :3012") st.binnacle) ]
    ]
