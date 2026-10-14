-- | PARAMETERS panel — the per-cell parameter grids, each co-located with the
-- | random source that mutates it, plus the sources that have no grid.
-- |
-- | Top: the grid-less generators (HEADS, TRANSP, PATTERN, SPEED, KEY·SCALE) as
-- | plain rows — an LED enable, a label, a mutation DEPTH and a firing PERIOD.
-- | Below: one card per per-cell parameter (GATE / SKIP / GLIDE / LENGTH /
-- | RATCHET), each a generator row over its 4×4 grid — one label serving both the
-- | generator controls and the grid. VELOCITY has a grid but no generator, so its
-- | card is grid-only. The NOTES source + its grid moved to the NOTES pane.
module Triggerfish.Odonus.View.Generate (generatePanel, cellParamsPanel) where

import Prelude

import Data.Array (mapWithIndex)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Grid.Types (Action(..), CellShortcut(..), GenKind(..), KnobTarget(..), Slots, State)
import Triggerfish.Odonus.View.Grid (transportRow)
import Triggerfish.Odonus.Grid.Widgets (cellPad, diceBtn, genRow, panelShell, style, tabBtn)
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)

generatePanel :: forall m. State -> HH.ComponentHTML Action Slots m
generatePanel s =
  panelShell s.collapsed "PARAMETERS" "sources · grids" "flex:0 1 280px;min-width:min-content"
    ( [ transportRow s ]
        <> map (topRow s) [ GHeads, GTransp, GPattern, GSpeed ]
        <>
        [ lampCard s GGate (toggleGrid "#e0a32e" _.gate ToggleGate s.odo)
            [ shortcut "ALL" (GateAll true), shortcut "NONE" (GateAll false), diceBtn "gate each cell, about three in four" (Shortcut GateRoll) ]
        , lampCard s GSkip (toggleGrid "#c0563f" _.skip ToggleSkip s.odo)
            [ shortcut "NONE" (SkipAll false), diceBtn "skip about one cell in five" (Shortcut SkipRoll) ]
        , lampCard s GGlide (toggleGrid "#4f9d69" _.glide ToggleGlide s.odo)
            [ shortcut "NONE" (GlideAll false), diceBtn "glide about one cell in five" (Shortcut GlideRoll) ]
        ]
    )

-- | The PER-CELL pane — LEN / RATCHET / VELOCITY, the three knob-grid parameters
-- | (as against PARAMETERS' toggle grids). Split out of `generatePanel` and moved
-- | into the column the KEY pane used to hold (AC, 2026-08-06): KEY's context +
-- | scenes moved up to Odonus's secondary nav, and PARAMETERS had grown long
-- | enough to need a scrollbar while its neighbour sat half empty. Same cards,
-- | same actions — only the column changed.
cellParamsPanel :: forall m. State -> HH.ComponentHTML Action Slots m
cellParamsPanel s =
  -- No subtitle: "len · ratchet · velocity" wrapped the header onto two lines and
  -- cost the column the vertical space its third knob grid needed — and each card
  -- below is already labelled LEN / RATCHET / VELOCITY (AC, 2026-08-06).
  panelShell s.collapsed "PER CELL" "" "flex:0 1 270px;min-width:min-content"
    [ paramCard s GLen (perCellKnobGrid "#7d8a93" 1 8 8 CellDur _.dur s.odo)
        [ shortcut "1" (DurAll 1), shortcut "2" (DurAll 2), shortcut "4" (DurAll 4), diceBtn "a length for each cell, mostly 1 or 2" (Shortcut DurRoll) ]
    , paramCard s GRatchet (perCellKnobGrid "#9d6b8a" 1 8 8 CellRatchet _.ratchet s.odo)
        [ shortcut "NONE" (RatchetAll 1), shortcut "×2" (RatchetAll 2), diceBtn "ratchet about one cell in five, ×2 to ×4" (Shortcut RatchetRoll) ]
    , paramCard s GVel (perCellKnobGrid "#8a9d6b" 1 127 0 CellVel _.vel s.odo)
        [ shortcut "SOFT" (VelAll 60), shortcut "MID" (VelAll 90), shortcut "LOUD" (VelAll 115)
        , shortcut "ACCENT" VelAccent ]
    ]

-- | A grid-less generator (heads / transp / pattern / speed / key): the plain
-- | source row with a divider under it, as the old GENERATE pane rendered them.
topRow :: forall m. State -> GenKind -> HH.ComponentHTML Action Slots m
topRow s kind =
  HH.div [ style "margin-bottom:7px;padding-bottom:7px;border-bottom:1px solid #00000010" ]
    [ genRow s kind ]

-- | A parameter card: the generator row header (LED · label · depth · period)
-- | over the bare 4×4 grid it drives. The generator's label serves for both, so
-- | the grid carries no second label.
paramCard
  :: forall m
   . State -> GenKind -> HH.ComponentHTML Action Slots m -> Array (HH.ComponentHTML Action Slots m)
  -> HH.ComponentHTML Action Slots m
paramCard s kind gridBody buttons =
  HH.div [ style cardStyle ]
    [ genRow s kind
    , HH.div [ style "margin-top:7px" ] [ gridBody ]
    , HH.div [ style "display:flex;gap:3px;margin-top:6px" ] buttons
    ]

-- | A lamp card: the generator row over a compact 4×4 of lamps, its buttons
-- | stacked beside the lamps rather than under them (as small as the
-- | COMBINATIONS key, and as playable).
lampCard
  :: forall m
   . State -> GenKind -> HH.ComponentHTML Action Slots m -> Array (HH.ComponentHTML Action Slots m)
  -> HH.ComponentHTML Action Slots m
lampCard s kind lamps buttons =
  HH.div [ style cardStyle ]
    [ genRow s kind
    , HH.div [ style "display:flex;gap:10px;align-items:stretch;margin-top:6px" ]
        [ lamps
        , HH.div [ style "flex:1;display:flex;flex-direction:column;justify-content:center;gap:3px" ] buttons
        ]
    ]

-- | One of a card's buttons: set the card's parameter on every cell, or roll it.
shortcut :: forall m. String -> CellShortcut -> HH.ComponentHTML Action Slots m
shortcut label sc = tabBtn label false (Shortcut sc)

cardStyle :: String
cardStyle = "margin-bottom:6px;padding:6px 9px;border-radius:7px;background:#ffffff30;border:1px solid #00000012"

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
    [ style "flex:none;display:grid;grid-template-columns:repeat(4,18px);gap:3px" ]
    (mapWithIndex (\i c -> toggleCell color (get c) (act i)) odo.cells)

toggleCell :: forall m. String -> Boolean -> Action -> HH.ComponentHTML Action Slots m
toggleCell color on act =
  HH.div
    [ HE.onClick \_ -> act
    , style "width:18px;height:18px;cursor:pointer;user-select:none;display:flex;align-items:center;justify-content:center"
    ]
    [ HH.div
        [ style $ "width:9px;height:9px;border-radius:50%;border:1px solid #00000022;background:"
            <> (if on then color else "#b4ae9c")
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
  -- fixed 38px pads rather than the card's width (AC, 2026-10-14: PER CELL
  -- fits an MBP's height, and a pad this size plays as well as the COMBINATIONS key)
  HH.div
    [ style "display:grid;grid-template-columns:repeat(4,38px);gap:3px;justify-content:center" ]
    (mapWithIndex (\i c -> perCellKnob color lo hi ticks (mkTarget i) (getVal c)) odo.cells)

perCellKnob
  :: forall m
   . String -> Int -> Int -> Int -> KnobTarget -> Int
  -> HH.ComponentHTML Action Slots m
perCellKnob color lo hi ticks target val =
  HH.div
    [ style $ cellPad
        <> ";padding:2px;aspect-ratio:1;display:flex;align-items:center;justify-content:center"
    ]
    [ HH.div [ style "width:100%;height:100%;min-height:0" ]
        [ knob
            { cx: 24.0, cy: 24.0, rOuter: 18.0, rInner: 7.0, color, lo, hi, value: val, ticks }
            (KnobDown target val)
        ]
    ]
