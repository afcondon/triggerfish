-- | GENERATE panel — the randomisation matrix. One row per aspect (NOTES,
-- | STEPS, HEADS, TRANSP, PATTERN, SPEED, KEY·SCALE): an LED enable, a label,
-- | and a bare draggable NUMBER giving the firing period ("one change per N
-- | steps", big range — drag up for rarer). The NOTES row additionally carries
-- | the Marbles X-Y pad (X = bias, Y = spread) over the live Beta histogram and
-- | a one-shot Roll. Each source drifts one notch at a time, so several can run
-- | at once without chaos. See the Mutable Instruments Marbles manual for the
-- | SPREAD×BIAS shapes.
module Triggerfish.Odonus.View.Generate (generatePanel) where

import Prelude

import Data.Array (find)
import Data.Foldable (maximum)
import Data.Int (round)
import Data.Maybe (fromMaybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Types
  ( Action(..), GenKind(..), KnobTarget(..), State, genKinds, genLabel, genSub, marblesPadId, periodOf )
import Triggerfish.Odonus.Grid.Widgets (engrave, panelShell, style, tabBtn)
import Triggerfish.Odonus.Marbles (betaWeights)
import Triggerfish.Odonus.Model as M

generatePanel :: forall m. State -> HH.ComponentHTML Action () m
generatePanel s =
  panelShell s.collapsed "GENERATE" "Random sources" "flex:0 1 236px;min-width:min-content"
    (map (genRow s) genKinds)

-- | One source row. The NOTES row unfolds the X-Y pad + Roll beneath its head.
genRow :: forall m. State -> GenKind -> HH.ComponentHTML Action () m
genRow s kind =
  let
    on = maybe false _.on src
    rate = maybe 90 _.rate src
    amt = maybe 30 _.amt src
    src = find (\g -> g.kind == kind) s.gen
  in
    HH.div [ style "margin-bottom:7px;padding-bottom:7px;border-bottom:1px solid #00000010" ]
      ( [ HH.div [ style "display:flex;align-items:center;gap:7px" ]
            [ led on kind
            , HH.div [ style "flex:1;min-width:0" ]
                [ HH.div [ style $ engrave <> ";font-size:11px;color:#3f3c33;line-height:1.1" ]
                    [ HH.text (genLabel kind) ]
                , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.65;letter-spacing:0.06em" ]
                    [ HH.text (genSub kind) ]
                ]
            , amtNumber kind amt on
            , freqNumber kind rate on
            ]
        ] <> (if kind == GNotes then notesExtras s else [])
      )

-- | The Marbles distribution controls only the NOTES source draws from.
notesExtras :: forall m. State -> Array (HH.ComponentHTML Action () m)
notesExtras s =
  [ xyPad s
  , readout s
  , rollGrid s.odo
  ]

-- | The NOTES-source one-shots as a compact 2×2: flatten every note to the scale
-- | root low (LOW, basslines) or middle (MID, melodies), seed a fresh MELODY line,
-- | or ROLL the Marbles once. Both octave floors follow the current key.
rollGrid :: forall m. M.Odonus -> HH.ComponentHTML Action () m
rollGrid odo =
  HH.div [ style "display:grid;grid-template-columns:1fr 1fr;gap:4px;margin-top:7px" ]
    [ tabBtn "LOW" false (SetAllNotes 0)
    , tabBtn "MID" false (SetAllNotes (M.cellIndexMax odo `div` 2))
    , tabBtn "MELODY" false SeedMelody
    , tabBtn "⟳ ROLL" false MarblesRoll
    ]

-- | A round source-enable lamp. Click toggles; debounced in the handler so the
-- | doubled re-render dispatch can't cancel the flip.
led :: forall m. Boolean -> GenKind -> HH.ComponentHTML Action () m
led on kind =
  HH.div
    [ HE.onClick \_ -> ToggleGen kind
    , style $ "width:15px;height:15px;border-radius:50%;cursor:pointer;flex:0 0 auto;"
        <> "border:1px solid #a8a392;box-shadow:inset 0 1px 1px #00000022;background:"
        <> (if on then "radial-gradient(circle at 35% 30%, #f0c25a, #b5832b)" else "#c4bfb0") ]
    []

-- | The mutation-depth number (how MUCH each change is), dragged vertically.
-- | Small constant movement at low %, a real shake-up near 100%.
amtNumber :: forall m. GenKind -> Int -> Boolean -> HH.ComponentHTML Action () m
amtNumber kind amt on =
  HH.div
    [ HE.onMouseDown \_ -> KnobDown (GenAmt kind) amt
    , style "display:flex;flex-direction:column;align-items:flex-end;cursor:ns-resize;min-width:32px;user-select:none" ]
    [ HH.span
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:12px;line-height:1;color:"
            <> (if on then "#5a564b" else "#a9a497") ]
        [ HH.text (show amt <> "%") ]
    , HH.span [ style $ engrave <> ";font-size:7px;opacity:0.55;margin-top:1px" ]
        [ HH.text "depth" ]
    ]

-- | The bare period number, dragged vertically (up = rarer). Reuses the knob
-- | drag infra via the GenRate target; renders as a plain number, no dial.
freqNumber :: forall m. GenKind -> Int -> Boolean -> HH.ComponentHTML Action () m
freqNumber kind rate on =
  HH.div
    [ HE.onMouseDown \_ -> KnobDown (GenRate kind) rate
    , style "display:flex;flex-direction:column;align-items:flex-end;cursor:ns-resize;min-width:52px;user-select:none" ]
    [ HH.span
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:17px;line-height:1;font-weight:600;color:"
            <> (if on then "#7a3b1f" else "#9a9588") ]
        [ HH.text (show (periodOf rate)) ]
    , HH.span [ style $ engrave <> ";font-size:7px;opacity:0.6;margin-top:1px" ]
        [ HH.text "1 / N steps" ]
    ]

-- | The 2-D control: drag a puck through the live distribution. X = bias (peak
-- | position, low→high notes), Y = spread (up = wider). The histogram behind
-- | the puck is the Beta distribution for the current setting.
xyPad :: forall m. State -> HH.ComponentHTML Action () m
xyPad s =
  let
    nbars = 24
    ws = betaWeights nbars s.genBias s.genSpread
    peak = fromMaybe 0.0 (maximum ws)
    bar w =
      let h = if peak <= 0.0 then 0.0 else (w / peak) * 100.0
      in HH.div [ style "flex:1;display:flex;align-items:flex-end;justify-content:center;height:100%" ]
           [ HH.div [ style $ "width:78%;height:" <> show h <> "%;background:#c0563f33;border-radius:1px 1px 0 0" ] [] ]
    px = s.genBias * 100.0
    py = (1.0 - s.genSpread) * 100.0
  in
    HH.div
      [ HP.id marblesPadId
      , HE.onMouseDown \e -> MarblesPad (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , HE.onMouseMove \e -> MarblesPad (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , style $ "position:relative;width:100%;height:108px;margin-top:8px;border-radius:6px;cursor:crosshair;"
          <> "background:#cbc6b6;box-shadow:inset 0 0 0 1px #00000018;overflow:hidden;user-select:none" ]
      [ HH.div [ style "position:absolute;inset:0;display:flex;align-items:flex-end" ]
          (map bar ws)
      , HH.div
          [ style $ "position:absolute;width:13px;height:13px;border-radius:50%;background:#b5832b;"
              <> "box-shadow:0 0 0 2px #fff8,0 0 6px #b5832b;transform:translate(-50%,-50%);pointer-events:none;"
              <> "left:" <> show px <> "%;top:" <> show py <> "%" ] []
      ]

readout :: forall m. State -> HH.ComponentHTML Action () m
readout s =
  HH.div [ style "display:flex;justify-content:space-between;font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#5a564b;margin-top:4px" ]
    [ HH.span_ [ HH.text ("bias " <> pct s.genBias) ]
    , HH.span_ [ HH.text ("spread " <> pct s.genSpread) ]
    ]

pct :: Number -> String
pct x = show (round (x * 100.0)) <> "%"
