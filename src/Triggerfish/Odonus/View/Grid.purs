-- | GRID panel — the 16 cells as parameter-major small multiples + transport.
-- | One NOTE field of value knobs, then GATE / SKIP / GLIDE toggle fields and a
-- | LENGTH knob field, over the clock / gate / run / status / nameplate chrome.
module Triggerfish.Odonus.View.Grid (gridPanel) where

import Prelude

import Data.Int (floor, round)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget(..), State)
import Triggerfish.Odonus.Grid.Widgets
  ( cellChrome, engrave, labelledRow, panelShell, style, tabBtn )
import Data.Array (length, mapWithIndex)

gridPanel :: forall m. State -> H.ComponentHTML Action () m
gridPanel s =
  panelShell "ODONUS" "16 · Cartesian" "flex:0 1 340px;min-width:min-content"
    [ grid s
    , HH.div [ style "display:flex;align-items:flex-end;gap:12px;margin-top:6px" ]
        [ HH.div [ style "flex:1" ] [ clockRow s ]
        , gateBlock s.odo
        ]
    , controls s
    , statusBar s
    , nameplate s
    ]

-- | GATE knob: gated-note length as % of step (10..200; past 100 the notes
-- | overlap into the next = legato, which lets portamento/glide slide).
gateBlock :: forall m. M.Odonus -> H.ComponentHTML Action () m
gateBlock odo =
  HH.div [ style "display:flex;flex-direction:column;align-items:center;width:52px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text "GATE" ]
    , HH.div [ style "width:40px;height:40px" ]
        [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color: "#b5832b", lo: 10, hi: 200, value: odo.gatePct, ticks: 0 }
            (KnobDown GateLen odo.gatePct) ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#3f3c33;margin-top:1px" ]
        [ HH.text (show odo.gatePct <> "%") ]
    ]

-- | Global step length — what a 1× head plays. Buttons map to the clock
-- | divider (1=whole … 1/16=fast); per-head SPD multiplies from here.
clockRow :: forall m. State -> H.ComponentHTML Action () m
clockRow s =
  labelledRow "STEP LENGTH"
    (map (\d -> tabBtn d.lbl (s.stepDiv == d.div) (SetStepDiv d.div))
      [ { lbl: "1", div: 16 }, { lbl: "½", div: 8 }, { lbl: "¼", div: 4 }
      , { lbl: "⅛", div: 2 }, { lbl: "1/16", div: 1 } ])

-- | Format a positive Number to one decimal place (so Link's constant
-- | sub-BPM nudging is visible — the readout flickers when truly locked).
oneDp :: Number -> String
oneDp x =
  let n = round (x * 10.0)
  in show (n `div` 10) <> "." <> show (n `mod` 10)

statusBar :: forall m. State -> H.ComponentHTML Action () m
statusBar s =
  HH.div [ style $ engrave <> ";font-size:8px;margin-top:10px;display:flex;gap:14px;color:#6a6456" ]
    [ HH.span [ style $ "color:" <> (if s.clockLocked then "#2f8a5c" else "#b0492f") ]
        [ HH.text $ "CLOCK " <> oneDp s.clockTempo <> " · "
            <> (if s.clockLocked then "LINK" else "FREE") ]
      -- BEAT climbs iff the frame loop runs and the clock advances.
    , HH.span [] [ HH.text $ "BEAT " <> show (floor s.clockBeat) ]
      -- ANCHORS climbs iff the rig is actually feeding us (the diagnostic).
    , HH.span [] [ HH.text $ "ANCHORS " <> show s.anchorCount ]
    , HH.span [] [ HH.text $ "MIDI " <> s.midiName ]
    ]

-- | The grid is four **parameter-major small multiples** of the same 16
-- | cells: one NOTE field (knobs) + three toggle fields (gate / skip /
-- | glide). Each cell, in every field, carries the same head-presence
-- | chrome, so you watch the playheads sweep through gate and skip and
-- | glide, not only through the notes. This is the layout that scales —
-- | a new per-cell parameter (length, pulses, gate-mode…) is just another
-- | small multiple appended below, never a busier pad.
grid :: forall m. State -> H.ComponentHTML Action () m
grid s =
  HH.div
    [ style "display:flex;flex-direction:column;gap:10px;margin:14px 0" ]
    [ noteField s.odo
    -- The boolean fields compress two-per-row into a 2×2 sub-grid; the
    -- fourth slot holds the first per-cell knob field, LENGTH (PULSES /
    -- GATE-MODE will extend it to 2×3).
    , HH.div
        [ style "display:grid;grid-template-columns:1fr 1fr;gap:10px 11px;align-items:start" ]
        [ toggleField "GATE" "#e0a32e" _.gate ToggleGate s.odo
        , toggleField "SKIP" "#c0563f" _.skip ToggleSkip s.odo
        , toggleField "GLIDE" "#4f9d69" _.glide ToggleGlide s.odo
        , lengthField s.odo
        ]
    ]

-- | A labelled small multiple: small-caps engraved label, then a 4×4 body.
fieldShell :: forall m. String -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
fieldShell label body =
  HH.div [ style "display:flex;flex-direction:column;gap:3px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.85" ] [ HH.text label ]
    , body
    ]

-- | NOTE field — a 4×4 of value knobs, the only field that edits a number.
noteField :: forall m. M.Odonus -> H.ComponentHTML Action () m
noteField odo =
  fieldShell "NOTE"
    ( HH.div_
        [ HH.div
            [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:6px" ]
            (mapWithIndex (noteCell odo) odo.cells)
        , setAllRow
        ]
    )

-- | Flatten-the-grid macros: drop every note into the bass register (MIN)
-- | or the melodic register (CENTER) as a starting point to sculpt from.
-- | MARBLES (a spread/bias/déjà-vu generator) will join this row.
setAllRow :: forall m. H.ComponentHTML Action () m
setAllRow =
  HH.div [ style "display:flex;align-items:center;gap:5px;margin-top:7px" ]
    [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.7;margin-right:1px" ] [ HH.text "SET ALL" ]
    , tabBtn "MIN" false (SetAllNotes 36)
    , tabBtn "CENTER" false (SetAllNotes 60)
    ]

noteCell :: forall m. M.Odonus -> Int -> M.Cell -> H.ComponentHTML Action () m
noteCell odo i c =
  HH.div
    [ style $ cellChrome odo i
        <> ";padding:4px;aspect-ratio:1;display:flex;flex-direction:column;align-items:center;justify-content:center"
    ]
    [ HH.div [ style "width:100%;flex:1;min-height:0" ]
        [ knob
            { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 9.0, color: "#b5832b", lo: 36, hi: 84, value: c.note, ticks: 0 }
            (KnobDown (CellNote i) c.note)
        ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#4a463d;margin-top:1px" ]
        [ HH.text (show c.note) ]
    ]

-- | LENGTH field — the fourth small multiple, the first knob field down
-- | here: a 4×4 of small **detented** knobs, each = how many steps that
-- | cell's note sustains (1..8). The square footprint keeps the grid's
-- | rhythm; the eight detents read it as a discrete selector, slate-blue
-- | to set it apart from the amber NOTE knobs.
lengthField :: forall m. M.Odonus -> H.ComponentHTML Action () m
lengthField odo =
  fieldShell "LENGTH"
    ( HH.div
        [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:4px" ]
        (mapWithIndex (lengthCell odo) odo.cells)
    )

lengthCell :: forall m. M.Odonus -> Int -> M.Cell -> H.ComponentHTML Action () m
lengthCell odo i c =
  HH.div
    [ style $ cellChrome odo i
        <> ";padding:3px;aspect-ratio:1;display:flex;align-items:center;justify-content:center"
    ]
    [ HH.div [ style "width:100%;height:100%;min-height:0" ]
        [ knob
            { cx: 24.0, cy: 24.0, rOuter: 18.0, rInner: 7.0, color: "#7d8a93", lo: 1, hi: 8, value: c.dur, ticks: 8 }
            (KnobDown (CellDur i) c.dur)
        ]
    ]

-- | A toggle field — a 4×4 of clickable lamps for one boolean per cell.
-- | Cells are flat (not square) so three fields stack under the NOTE grid
-- | while their four columns stay aligned with it.
toggleField
  :: forall m
   . String -> String -> (M.Cell -> Boolean) -> (Int -> Action) -> M.Odonus
  -> H.ComponentHTML Action () m
toggleField label color get act odo =
  fieldShell label
    ( HH.div
        [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:4px" ]
        (mapWithIndex (\i c -> toggleCell odo color (get c) (act i) i) odo.cells)
    )

toggleCell :: forall m. M.Odonus -> String -> Boolean -> Action -> Int -> H.ComponentHTML Action () m
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

controls :: forall m. State -> H.ComponentHTML Action () m
controls s =
  HH.div [ style "display:flex;gap:8px;align-items:center;margin-top:14px" ]
    [ HH.button
        [ HE.onClick \_ -> ToggleRun
        , style $ "padding:6px 12px;border:1px solid #a8a392;border-radius:7px;"
            <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:12px;color:#3f3c33;cursor:pointer"
        ]
        [ HH.text (if s.running then "❚❚ Stop" else "▶ Run") ]
    , HH.span [ style $ engrave <> ";font-size:8px;color:#888273" ]
        [ HH.text "click a thumbnail to change a head's pattern" ]
    ]

nameplate :: forall m. State -> H.ComponentHTML Action () m
nameplate s =
  HH.div
    [ style $ "margin-top:16px;padding:7px 10px;border-radius:6px;"
        <> "background:linear-gradient(#c8a86a,#b8975a);box-shadow:0 1px 0 #00000022 inset;"
        <> engrave <> ";color:#3a3320;font-size:9px;display:flex;justify-content:space-between"
    ]
    [ HH.span [ style "letter-spacing:0.22em;color:#2c2718" ] [ HH.text "TRIGGERFISH" ]
    , HH.span [] [ HH.text $ "Model Odonus · " <> show (length s.odo.heads) <> "-head" ]
    ]
