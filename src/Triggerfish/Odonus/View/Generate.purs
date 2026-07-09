-- | PARAMETERS panel — the per-cell parameter grids, each co-located with the
-- | random source that mutates it, plus the sources that have no grid.
-- |
-- | Top: the grid-less generators (HEADS, TRANSP, PATTERN, SPEED, KEY·SCALE) as
-- | plain rows — an LED enable, a label, a mutation DEPTH and a firing PERIOD.
-- | Below: one card per per-cell parameter (GATE / SKIP / GLIDE / LENGTH /
-- | RATCHET), each a generator row over its 4×4 grid — one label serving both the
-- | generator controls and the grid. VELOCITY has a grid but no generator, so its
-- | card is grid-only. The NOTES source + its grid moved to the NOTES pane.
module Triggerfish.Odonus.View.Generate (generatePanel) where

import Prelude

import Data.Array (mapWithIndex)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Grid.Types (Action(..), GenKind(..), KnobTarget(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (cellChrome, genRow, panelShell, style)
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)

generatePanel :: forall m. State -> HH.ComponentHTML Action Slots m
generatePanel s =
  panelShell s.collapsed "PARAMETERS" "sources · grids" "flex:0 1 300px;min-width:min-content"
    ( [ freezeToggle s ]
        <> map (topRow s) [ GHeads, GTransp, GPattern, GSpeed ]
        <>
        [ paramCard s GGate    (toggleGrid "#e0a32e" _.gate ToggleGate s.odo)
        , paramCard s GSkip    (toggleGrid "#c0563f" _.skip ToggleSkip s.odo)
        , paramCard s GGlide   (toggleGrid "#4f9d69" _.glide ToggleGlide s.odo)
        , paramCard s GLen     (perCellKnobGrid "#7d8a93" 1 8 8 CellDur _.dur s.odo)
        , paramCard s GRatchet (perCellKnobGrid "#9d6b8a" 1 8 8 CellRatchet _.ratchet s.odo)
        , paramCard s GVel     (perCellKnobGrid "#8a9d6b" 1 127 0 CellVel _.vel s.odo)
        ]
    )

-- | The freeze toggle — pauses ALL generation WITHOUT touching the config, so a
-- | liked moment holds still long enough to hear, extend, or save it before the
-- | matrix drifts on. Deferred-on-both, so the rig freezes on the same step.
freezeToggle :: forall m. State -> HH.ComponentHTML Action Slots m
freezeToggle s =
  HH.div
    [ HE.onClick \_ -> ToggleFreeze
    , style $ "display:flex;align-items:center;justify-content:center;cursor:pointer;user-select:none;"
        <> "margin-bottom:9px;padding:5px 0;border-radius:6px;font-size:11px;letter-spacing:0.07em;"
        <> "border:1px solid " <> (if s.genFrozen then "#5b8bb0" else "#00000018") <> ";"
        <> (if s.genFrozen then "background:#dcebf5;color:#2b5878;font-weight:600"
                           else "background:#ffffff30;color:#6a6a6a") ]
    [ HH.text (if s.genFrozen then "❄ FROZEN — generation paused" else "❄ freeze generation") ]

-- | A grid-less generator (heads / transp / pattern / speed / key): the plain
-- | source row with a divider under it, as the old GENERATE pane rendered them.
topRow :: forall m. State -> GenKind -> HH.ComponentHTML Action Slots m
topRow s kind =
  HH.div [ style "margin-bottom:7px;padding-bottom:7px;border-bottom:1px solid #00000010" ]
    [ genRow s kind ]

-- | A parameter card: the generator row header (LED · label · depth · period)
-- | over the bare 4×4 grid it drives. The generator's label serves for both, so
-- | the grid carries no second label.
paramCard :: forall m. State -> GenKind -> HH.ComponentHTML Action Slots m -> HH.ComponentHTML Action Slots m
paramCard s kind gridBody =
  HH.div [ style cardStyle ]
    [ genRow s kind
    , HH.div [ style "margin-top:7px" ] [ gridBody ]
    ]

cardStyle :: String
cardStyle = "margin-bottom:9px;padding:8px 9px;border-radius:7px;background:#ffffff30;border:1px solid #00000012"

-- ---------------------------------------------------------------------------
-- Bare grids — the 4×4 small multiples, sans label (the card header labels them).
-- ---------------------------------------------------------------------------

-- | A boolean field — a 4×4 of clickable lamps for one per-cell boolean.
toggleGrid
  :: forall m
   . String -> (M.Cell -> Boolean) -> (Int -> Action) -> M.Odonus
  -> HH.ComponentHTML Action Slots m
toggleGrid color get act odo =
  HH.div
    [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:4px" ]
    (mapWithIndex (\i c -> toggleCell odo color (get c) (act i) i) odo.cells)

toggleCell :: forall m. M.Odonus -> String -> Boolean -> Action -> Int -> HH.ComponentHTML Action Slots m
toggleCell odo color on act i =
  HH.div
    [ HE.onClick \_ -> act
    , style $ cellChrome odo i
        <> ";height:14px;cursor:pointer;user-select:none;display:flex;align-items:center;justify-content:center"
    ]
    [ HH.div
        [ style $ "width:7px;height:7px;border-radius:50%;border:1px solid #00000022;background:"
            <> (if on then color else "#46433a")
            <> (if on then ";box-shadow:0 0 5px " <> color else "")
        ] []
    ]

-- | A per-cell knob field — a 4×4 small multiple of small knobs over one cell
-- | parameter (LENGTH / RATCHET / VEL…). `mkTarget` is the knob's drag target per
-- | index, `getVal` reads the value; `ticks > 0` draws detents.
perCellKnobGrid
  :: forall m
   . String -> Int -> Int -> Int -> (Int -> KnobTarget) -> (M.Cell -> Int) -> M.Odonus
  -> HH.ComponentHTML Action Slots m
perCellKnobGrid color lo hi ticks mkTarget getVal odo =
  HH.div
    [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:4px" ]
    (mapWithIndex (\i c -> perCellKnob odo color lo hi ticks (mkTarget i) (getVal c) i) odo.cells)

perCellKnob
  :: forall m
   . M.Odonus -> String -> Int -> Int -> Int -> KnobTarget -> Int -> Int
  -> HH.ComponentHTML Action Slots m
perCellKnob odo color lo hi ticks target val i =
  HH.div
    [ style $ cellChrome odo i
        <> ";padding:3px;aspect-ratio:1;display:flex;align-items:center;justify-content:center"
    ]
    [ HH.div [ style "width:100%;height:100%;min-height:0" ]
        [ knob
            { cx: 24.0, cy: 24.0, rOuter: 18.0, rInner: 7.0, color, lo, hi, value: val, ticks }
            (KnobDown target val)
        ]
    ]
