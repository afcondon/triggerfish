-- | Triggerfish.Selene.Component — the fourth instrument in the rack: a
-- | direct-manipulation surface for the Selene polysignal generators. Skeleton:
-- | a generator selector (POLYLFO / POLYCLOCK / POLYEUCLID / POLYNOTE), an
-- | eight-slot bank editor for whichever is in focus, and the growing-spec
-- | SOURCE pane (the `octoLfo …` eDSL the BEAM cell / es9-daemon speak).
-- |
-- | What is NOT here yet (the substantive next pass): the output path — Selene
-- | drives CV/gate over the es9-daemon `apply-polysignal` socket, not MIDI, so
-- | it wants Binnacle.Output (es9 cv-out/fire-at) rather than the Midi path the
-- | other two use; and the real polysignal drawing (live phase animation). This
-- | lays out the editable shape so those can land on top.
module Triggerfish.Selene.Component (component) where

import Prelude

import Data.Array (filter, length, mapWithIndex)
import Data.Int (round, toNumber)
import Data.String.Common (joinWith)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Grid.Widgets (engrave, style)
import Triggerfish.Selene.Model as M

-- ---------------------------------------------------------------------------
-- State / Actions
-- ---------------------------------------------------------------------------

type State = { sel :: M.Selene }

data Action
  = PickGen M.Gen
  | PickRange M.OutputRange
  | LfoRate Int Number       -- slot, Hz delta
  | ClkMult Int Int          -- slot, multiplier delta
  | ClkPW Int Int            -- slot, pulse-width delta
  | EucBeats Int Int         -- slot, beats delta
  | EucSteps Int Int         -- slot, steps delta
  | NoteBump Int Int         -- slot, semitone delta

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { sel: M.defaultSelene }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  PickGen g -> H.modify_ \s -> s { sel = M.setGen g s.sel }
  PickRange r -> H.modify_ \s -> s { sel = M.setRange r s.sel }
  LfoRate i d -> H.modify_ \s -> s { sel = M.modLfo i (\sl -> sl { rate = clampNum 0.01 20.0 (sl.rate + d) }) s.sel }
  ClkMult i d -> H.modify_ \s -> s { sel = M.modClock i (\sl -> sl { multiplier = M.clampI 1 32 (sl.multiplier + d) }) s.sel }
  ClkPW i d -> H.modify_ \s -> s { sel = M.modClock i (\sl -> sl { pulseWidth = M.clampI 1 99 (sl.pulseWidth + d) }) s.sel }
  EucBeats i d -> H.modify_ \s -> s { sel = M.modEuclid i (\sl -> sl { beats = M.clampI 1 sl.steps (sl.beats + d) }) s.sel }
  EucSteps i d -> H.modify_ \s -> s { sel = M.modEuclid i (\sl -> let steps = M.clampI 1 32 (sl.steps + d) in sl { steps = steps, beats = M.clampI 1 steps sl.beats }) s.sel }
  NoteBump i d -> H.modify_ \s -> s { sel = M.modNote i (\sl -> sl { note = M.clampI 0 127 (sl.note + d) }) s.sel }

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

accent :: String
accent = "#3f6f8a"   -- steel-blue, Selene's electric accent (cf. the HH lane)

-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;inset:0;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ selectorPanel s
    , bankPanel s
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
-- Selector panel — pick the generator family + the output range
-- ---------------------------------------------------------------------------

selectorPanel :: forall m. State -> H.ComponentHTML Action () m
selectorPanel s =
  panel "SELENE" "flex:0 0 210px"
    [ HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:7px" ] [ HH.text "GENERATOR" ]
    , HH.div [ style "display:flex;flex-direction:column;gap:7px" ]
        (map (genButton s) M.allGens)
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin:18px 0 7px" ] [ HH.text "OUTPUT RANGE" ]
    , HH.div [ style "display:flex;flex-wrap:wrap;gap:5px" ]
        (map (rangeButton s) M.allRanges)
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.55;margin-top:20px;line-height:1.6" ]
        [ HH.text (genBlurb s.sel.gen) ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.4;margin-top:14px;line-height:1.6" ]
        [ HH.text "EIGHT SLOTS → EIGHT CV/GATE JACKS. THE RELATION BETWEEN THEM IS THE PATCH." ]
    ]

genButton :: forall m. State -> M.Gen -> H.ComponentHTML Action () m
genButton s g =
  let active = s.sel.gen == g
  in
    HH.button
      [ HE.onClick \_ -> PickGen g
      , style $ "padding:9px 11px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;text-align:left;"
          <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.1em;color:"
          <> (if active then "#1c1a12" else "#5a564b")
          <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
      [ HH.text (M.genLabel g) ]

rangeButton :: forall m. State -> M.OutputRange -> H.ComponentHTML Action () m
rangeButton s r =
  let active = s.sel.range == r
  in
    HH.button
      [ HE.onClick \_ -> PickRange r
      , style $ "padding:4px 8px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
          <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:"
          <> (if active then "#1c1a12" else "#5a564b")
          <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
      [ HH.text (M.rangeLabel r) ]

genBlurb :: M.Gen -> String
genBlurb = case _ of
  M.GenLfo -> "PHASE-SPREAD FREE-RUNNING LFOS — A DC LEVEL PLUS SIX SUMMED SHAPES, EACH SLOT A SLICE OF THE TRAVELLING WAVE."
  M.GenClock -> "TEMPO-LOCKED GATE DIVISIONS. BASE ÷ MULTIPLIER SETS THE PERIOD; PULSE WIDTH THE DUTY. RIDES THE LINK TEMPO."
  M.GenEuclid -> "EUCLIDEAN GATES — K BEATS SPREAD OVER N STEPS BY THE BRESENHAM RULE, EACH SLOT ITS OWN POLYMETRIC RING."
  M.GenNote -> "EIGHT HELD V/OCT PITCHES — A CHORD AS CONSTANT VOLTAGE. THE QUIET ROOT OF A PATCH."

-- ---------------------------------------------------------------------------
-- Bank panel — the eight slots of the active generator
-- ---------------------------------------------------------------------------

bankPanel :: forall m. State -> H.ComponentHTML Action () m
bankPanel s =
  panel (M.genLabel s.sel.gen <> " · 8") "flex:1 1 560px;min-width:420px"
    [ HH.div [ style "display:flex;flex-direction:column;gap:6px;max-width:720px" ]
        (mapWithIndex slotRow (slotData s.sel))
    ]

-- A uniform handle on "the active generator's slots" for rendering.
data SlotView
  = VLfo M.ModSlot
  | VClock M.ClockSlot
  | VEuclid M.EuclidSlot
  | VNote M.PresetNoteSlot

slotData :: M.Selene -> Array SlotView
slotData sel = case sel.gen of
  M.GenLfo -> map VLfo sel.lfo
  M.GenClock -> map VClock sel.clock
  M.GenEuclid -> map VEuclid sel.euclid
  M.GenNote -> map VNote sel.note

slotRow :: forall m. Int -> SlotView -> H.ComponentHTML Action () m
slotRow i v =
  HH.div
    [ style $ "display:flex;align-items:center;gap:10px;padding:6px 8px;border-radius:6px;"
        <> "background:#00000008;border:1px solid #00000010" ]
    ( [ slotChip i ] <> slotBody i v )

slotChip :: forall m. Int -> H.ComponentHTML Action () m
slotChip i =
  HH.div
    [ style $ "width:20px;height:20px;flex:0 0 auto;border-radius:4px;display:flex;align-items:center;justify-content:center;"
        <> "background:" <> accent <> ";color:#efece1;font-family:'SF Mono',Menlo,monospace;font-size:10px" ]
    [ HH.text (show (i + 1)) ]

slotBody :: forall m. Int -> SlotView -> Array (H.ComponentHTML Action () m)
slotBody i = case _ of
  VLfo sl ->
    [ stepper (fmt2 sl.rate <> " Hz") (LfoRate i (-0.25)) (LfoRate i 0.25)
    , readField "φ" (fmt2 sl.phase)
    , readField "shapes" (lfoShapes sl)
    , bar (clampNum 0.0 1.0 (sl.rate / 8.0))
    ]
  VClock sl ->
    [ readField "base" (M.clockBaseLabel sl.base)
    , stepper ("×" <> show sl.multiplier) (ClkMult i (-1)) (ClkMult i 1)
    , stepper (show sl.pulseWidth <> "% pw") (ClkPW i (-5)) (ClkPW i 5)
    , dutyBar sl.pulseWidth
    ]
  VEuclid sl ->
    [ stepper (show sl.beats <> " k") (EucBeats i (-1)) (EucBeats i 1)
    , stepper (show sl.steps <> " n") (EucSteps i (-1)) (EucSteps i 1)
    , euclidDots sl
    ]
  VNote sl ->
    [ stepper (M.noteName sl.note) (NoteBump i (-1)) (NoteBump i 1)
    , readField "midi" (show sl.note)
    , octave (NoteBump i (-12)) (NoteBump i 12)
    , bar (clampNum 0.0 1.0 (toNumber sl.note / 127.0))
    ]

-- ---------------------------------------------------------------------------
-- Small shared field widgets
-- ---------------------------------------------------------------------------

-- A ▼ value ▲ stepper.
stepper :: forall m. String -> Action -> Action -> H.ComponentHTML Action () m
stepper valueStr dec inc =
  HH.div [ style "display:flex;align-items:center;gap:4px;flex:0 0 auto" ]
    [ tick "▼" dec
    , HH.span
        [ style "min-width:62px;text-align:center;font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#2b2922" ]
        [ HH.text valueStr ]
    , tick "▲" inc
    ]

octave :: forall m. Action -> Action -> H.ComponentHTML Action () m
octave dec inc =
  HH.div [ style "display:flex;align-items:center;gap:3px;flex:0 0 auto" ]
    [ tick "−8va" dec, tick "+8va" inc ]

tick :: forall m. String -> Action -> H.ComponentHTML Action () m
tick glyph act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:2px 6px;border:1px solid #a8a392;border-radius:4px;cursor:pointer;"
        <> "background:linear-gradient(#efece1,#ddd9cb);font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33" ]
    [ HH.text glyph ]

readField :: forall m. String -> String -> H.ComponentHTML Action () m
readField label val =
  HH.div [ style "display:flex;align-items:baseline;gap:4px;flex:0 0 auto" ]
    [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55" ] [ HH.text label ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#2b2922" ] [ HH.text val ]
    ]

-- A thin proportional fill bar, 0..1.
bar :: forall m. Number -> H.ComponentHTML Action () m
bar frac =
  HH.div [ style "flex:1 1 auto;min-width:40px;height:7px;border-radius:4px;background:#0000000f;overflow:hidden" ]
    [ HH.div [ style $ "height:100%;width:" <> show (round (frac * 100.0)) <> "%;background:" <> accent <> "cc" ] [] ]

-- Pulse-width as a duty cycle bar (filled portion = high).
dutyBar :: forall m. Int -> H.ComponentHTML Action () m
dutyBar pw =
  HH.div [ style "flex:1 1 auto;min-width:60px;height:14px;border-radius:3px;background:#0000000f;overflow:hidden;display:flex" ]
    [ HH.div [ style $ "height:100%;width:" <> show pw <> "%;background:" <> accent <> "cc" ] []
    , HH.div [ style "height:100%;flex:1 1 auto" ] []
    ]

-- The Euclidean pattern as a row of filled / hollow dots.
euclidDots :: forall m. M.EuclidSlot -> H.ComponentHTML Action () m
euclidDots sl =
  HH.div [ style "display:flex;flex-wrap:wrap;gap:3px;flex:1 1 auto;align-items:center" ]
    (map dot (M.euclidBits sl))
  where
  dot on =
    HH.div
      [ style $ "width:11px;height:11px;border-radius:50%;border:1px solid " <> accent
          <> ";background:" <> (if on then accent else "transparent") ] []

-- The active shapes of an LFO slot, as a compact label (sin/sqr/tri/saw/rnd/nse).
lfoShapes :: M.ModSlot -> String
lfoShapes sl =
  let
    parts =
      filter (_ /= "")
        [ tagIf (sl.sin > 0.0) "sin"
        , tagIf (sl.sqr > 0.0) "sqr"
        , tagIf (sl.tri > 0.0) "tri"
        , tagIf (sl.saw /= 0.0) "saw"
        , tagIf (sl.rnd > 0.0) "rnd"
        , tagIf (sl.nse > 0.0) "nse"
        ]
  in
    if length parts == 0 then "—" else joinWith ", " parts
  where
  tagIf c t = if c then t else ""

-- ---------------------------------------------------------------------------
-- Source panel — the growing-spec eDSL cell (read-only, like the others)
-- ---------------------------------------------------------------------------

sourcePanel :: forall m. State -> H.ComponentHTML Action () m
sourcePanel s =
  panel "SOURCE" "flex:0 0 320px"
    [ HH.pre
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10px;line-height:1.55;"
            <> "color:#3f3c33;white-space:pre-wrap;word-break:break-word;margin:0;"
            <> "user-select:text;-webkit-user-select:text" ]
        [ HH.text (seleneSource s.sel) ]
    ]

-- The active generator rendered as its `octo…` constructor — the spec the BEAM
-- cell + es9-daemon already understand.
seleneSource :: M.Selene -> String
seleneSource sel =
  case sel.gen of
    M.GenLfo -> block "octoLfo es9Main" (mapWithIndex lfoRow sel.lfo)
    M.GenClock -> block "octoClock es9Gt0" (mapWithIndex clockRow sel.clock)
    M.GenEuclid -> block "octoEuclid es9Gt0" (mapWithIndex euclidRow sel.euclid)
    M.GenNote -> block "octoPresetNote es9Main" (mapWithIndex noteRow sel.note)
  where
  block ctor rows =
    ctor <> "\n  [ " <> joinWith "\n  , " rows <> "\n  ] (Just " <> M.rangeToWire sel.range <> ")"
  lfoRow i sl = "silent { rate = " <> fmt2 sl.rate <> ", phase = " <> fmt2 sl.phase
    <> ", sin = " <> fmt2 sl.sin <> " }" <> slotComment i
  clockRow i sl = "clk " <> M.clockBaseToWire sl.base <> " × " <> show sl.multiplier
    <> " pw " <> show sl.pulseWidth <> slotComment i
  euclidRow i sl = "euclid " <> show sl.beats <> " " <> show sl.steps
    <> " @ " <> show sl.rate <> slotComment i
  noteRow i sl = "note " <> show sl.note <> "  -- " <> M.noteName sl.note <> slotComment i
  slotComment i = "    -- " <> show (i + 1)

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

clampNum :: Number -> Number -> Number -> Number
clampNum lo hi v = if v < lo then lo else if v > hi then hi else v

-- two-decimal fixed (good enough for the readouts)
fmt2 :: Number -> String
fmt2 x = show (toNumber (round (x * 100.0)) / 100.0)
