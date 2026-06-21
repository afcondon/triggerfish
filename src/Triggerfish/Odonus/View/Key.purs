-- | KEY panel — the live pitch lens (scale + distribution + transpose).
module Triggerfish.Odonus.View.Key (quantizerPanel) where

import Prelude

import Data.Array (elem, length, range)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget(..), State)
import Triggerfish.Odonus.Grid.Widgets
  ( engrave, labelledRow, octLabel, panelShell, romanNum, stepperRow, style, tabBtn )

quantizerPanel :: forall m. State -> H.ComponentHTML Action () m
quantizerPanel s =
  panelShell s.collapsed "KEY" "Quantize · Transpose" "flex:0 1 278px;min-width:min-content"
    [ pcKeyboard s.odo
    , stepperRow "ROOT" (Scale.rootName s.odo.rootPc)
        (SetRoot (s.odo.rootPc - 1)) (SetRoot (s.odo.rootPc + 1))
    , HH.div [ style "display:flex;align-items:flex-end;gap:10px;margin:8px 0" ]
        [ HH.div [ style "flex:1;min-width:0" ]
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
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color: "#8a9b6e", lo: 1, hi: 12, value: n, ticks: 0 }
              (KnobDown Spread n) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#3f3c33;margin-top:1px" ]
          [ HH.text (show n <> "n") ]
      ]
