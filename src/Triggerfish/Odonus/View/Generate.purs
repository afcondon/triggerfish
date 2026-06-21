-- | GENERATE panel — the Marbles note-value source. An X-Y pad (X = SPREAD,
-- | Y = BIAS) over the live Beta histogram, a déjà-vu AMOUNT knob, a boundary
-- | selector (when it fires), an ON switch, and a one-shot ROLL. This is
-- | source 1; the whole-rig low-probability drift (source 2) will sit beside
-- | it in the same panel.
module Triggerfish.Odonus.View.Generate (generatePanel) where

import Prelude

import Data.Foldable (maximum)
import Data.Int (round)
import Data.Maybe (fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Types
  (Action(..), Boundary, KnobTarget(..), MarblesCfg, State, boundaries, boundaryShort, marblesPadId)
import Triggerfish.Odonus.Grid.Widgets (engrave, panelShell, style, tabBtn)
import Triggerfish.Odonus.Marbles (betaWeights)
import Triggerfish.Ui.Knob (knob)

generatePanel :: forall m. State -> HH.ComponentHTML Action () m
generatePanel s =
  panelShell "GENERATE" "Marbles · Random" "flex:0 1 218px;min-width:min-content"
    [ onSwitch s.marbles.on
    , xyPad s.marbles
    , HH.div [ style "display:flex;align-items:flex-end;gap:10px;margin:12px 0 6px" ]
        [ HH.div [ style "flex:1;min-width:0" ] [ readout s.marbles ]
        , amountKnob s.marbles.amount
        ]
    , boundaryRow s.marbles.boundary
    , rollButton
    ]

-- | The ON/OFF master for the source.
onSwitch :: forall m. Boolean -> HH.ComponentHTML Action () m
onSwitch on =
  HH.button
    [ HE.onClick \_ -> ToggleMarbles
    , style $ "width:100%;padding:6px;margin-bottom:10px;border-radius:7px;cursor:pointer;border:1px solid #a8a392;"
        <> "font-family:Georgia,serif;font-size:11px;color:" <> (if on then "#1c1a12" else "#3f3c33")
        <> ";background:" <> (if on then "linear-gradient(#cbb27a,#b89a58)" else "linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text (if on then "● MARBLES — running" else "○ MARBLES — off") ]

-- | The 2-D control: drag a puck through the live distribution. X = spread
-- | (narrow → wide → rails), Y = bias (top = high notes). The histogram behind
-- | the puck is the Beta distribution for the current setting.
xyPad :: forall m. MarblesCfg -> HH.ComponentHTML Action () m
xyPad cfg =
  let
    nbars = 24
    ws = betaWeights nbars cfg.bias cfg.spread
    peak = fromMaybe 0.0 (maximum ws)
    -- Manual-style histogram: pitch runs left→right, bar HEIGHT = probability
    -- (tallest = most likely). BIAS is on X, so the peak sits under the puck's
    -- horizontal position; SPREAD is on Y (up = wider).
    bar w =
      let h = if peak <= 0.0 then 0.0 else (w / peak) * 100.0
      in HH.div [ style "flex:1;display:flex;align-items:flex-end;justify-content:center;height:100%" ]
           [ HH.div [ style $ "width:78%;height:" <> show h <> "%;background:#c0563f33;border-radius:1px 1px 0 0" ] [] ]
    px = cfg.bias * 100.0
    py = (1.0 - cfg.spread) * 100.0
  in
    HH.div
      [ HP.id marblesPadId
      , HE.onMouseDown \e -> MarblesPad (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , HE.onMouseMove \e -> MarblesPad (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , style $ "position:relative;width:100%;height:128px;border-radius:6px;cursor:crosshair;"
          <> "background:#cbc6b6;box-shadow:inset 0 0 0 1px #00000018;overflow:hidden;user-select:none" ]
      [ HH.div [ style "position:absolute;inset:0;display:flex;align-items:flex-end" ]
          (map bar ws)
      , HH.div
          [ style $ "position:absolute;width:13px;height:13px;border-radius:50%;background:#b5832b;"
              <> "box-shadow:0 0 0 2px #fff8,0 0 6px #b5832b;transform:translate(-50%,-50%);pointer-events:none;"
              <> "left:" <> show px <> "%;top:" <> show py <> "%" ] []
      ]

-- | The déjà-vu knob: per-cell regenerate probability at each boundary
-- | (0 = frozen loop, 100 = fully fresh each time). Uses the standard
-- | relative-drag knob via the MarblesAmt target.
amountKnob :: forall m. Number -> HH.ComponentHTML Action () m
amountKnob amount =
  let v = round (amount * 100.0)
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:52px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text "AMOUNT" ]
      , HH.div [ style "width:40px;height:40px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color: "#6f7f88", lo: 0, hi: 100, value: v, ticks: 0 }
              (KnobDown MarblesAmt v) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#3f3c33;margin-top:1px" ]
          [ HH.text (show v <> "%") ]
      ]

readout :: forall m. MarblesCfg -> HH.ComponentHTML Action () m
readout cfg =
  HH.div [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#5a564b;line-height:1.6" ]
    [ HH.div_ [ HH.text ("spread " <> pct cfg.spread) ]
    , HH.div_ [ HH.text ("bias   " <> pct cfg.bias) ]
    ]

-- | When the source fires.
boundaryRow :: forall m. Boundary -> HH.ComponentHTML Action () m
boundaryRow cur =
  HH.div_
    [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.85;display:block;margin-bottom:3px" ]
        [ HH.text "EVERY" ]
    , HH.div [ style "display:flex;gap:4px" ]
        (map (\b -> tabBtn (boundaryShort b) (b == cur) (SetBoundary b)) boundaries)
    ]

rollButton :: forall m. HH.ComponentHTML Action () m
rollButton =
  HH.button
    [ HE.onClick \_ -> MarblesRoll
    , style $ "width:100%;margin-top:10px;padding:5px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:10px;color:#3f3c33" ]
    [ HH.text "⟳ Roll once" ]

pct :: Number -> String
pct x = show (round (x * 100.0)) <> "%"
