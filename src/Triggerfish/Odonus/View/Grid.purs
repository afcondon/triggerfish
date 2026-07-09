-- | NOTES panel — the pitch surface. The NOTES random source (Marbles Beta
-- | distribution + LOW/MID/MELODY/ROLL one-shots) sits above the 4×4 field of
-- | value knobs it draws from. The other per-cell parameter grids (gate, skip,
-- | glide, length, ratchet, velocity) moved to the PARAMETERS pane, each beside
-- | its own generator. The transport chrome (step length, groove, status,
-- | nameplate) stays here for now.
module Triggerfish.Odonus.View.Grid (gridPanel) where

import Prelude

import Data.Foldable (maximum)
import Data.Int (floor, round)
import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Odonus.Marbles (betaWeights)
import Triggerfish.Odonus.Grid.Types
  ( Action(..), GenKind(..), KnobTarget(..), Slots, State, marblesPadId )
import Triggerfish.Odonus.Grid.Widgets
  ( cellChrome, engrave, genRow, labelledRow, miniKnob, octLabel, panelShell, style, tabBtn )
import Data.Array (length, mapWithIndex, (!!))

gridPanel :: forall m. State -> H.ComponentHTML Action Slots m
gridPanel s =
  panelShell s.collapsed "NOTES" "pitch · Marbles" "flex:0 1 340px;min-width:min-content"
    -- OCTAVE + DEGREE: the two global pitch moves, brought here from KEY — both
    -- shift the register the note grid plays and its knob labels now show. OCTAVE
    -- is chromatic (±octaves); DEGREE is scalar — shift every voice by a scale
    -- degree, in the set's own space, ahead of any chord snap. The buttons are the
    -- interval you jump TO: "1" = unison (no shift), "3" = a third, "5" = a fifth,
    -- "7" = a seventh — so `degShift` is the button number minus one. (Distinct
    -- from the per-head chromatic INT — see the PARAMETERS/Playheads panes.)
    [ HH.div [ style "margin-bottom:12px;display:flex;flex-direction:column;gap:8px" ]
        [ labelledRow "OCTAVE"
            (map (\n -> tabBtn (octLabel n) (s.odo.octaveShift == n) (SetOctave n)) [ -2, -1, 0, 1, 2 ])
        , labelledRow "DEGREE"
            (map (\d -> tabBtn (show (d + 1)) (s.odo.degShift == d) (SetDegShift d)) [ 0, 1, 2, 3, 4, 5, 6 ])
        ]
    , genRow s GNotes
    , xyPad s
    , readout s
    , rollGrid
    , HH.div [ style "margin-top:12px" ] [ noteField s.odo ]
    , HH.div [ style "display:flex;align-items:flex-end;gap:10px;margin-top:12px" ]
        [ HH.div [ style "flex:1" ] [ clockRow s ]
        , feelBlock s
        ]
    , controls s
    , statusBar s
    , nameplate s
    ]

-- ---------------------------------------------------------------------------
-- NOTES random source — the Marbles distribution + one-shots (moved here from
-- the old GENERATE pane, now co-located with the note grid it feeds).
-- ---------------------------------------------------------------------------

-- | The 2-D control: drag a puck through the live distribution. X = bias (peak
-- | position, low→high notes), Y = spread (up = wider). The histogram behind
-- | the puck is the Beta distribution for the current setting.
xyPad :: forall m. State -> H.ComponentHTML Action Slots m
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

readout :: forall m. State -> H.ComponentHTML Action Slots m
readout s =
  HH.div [ style "display:flex;justify-content:space-between;font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#5a564b;margin-top:4px" ]
    [ HH.span_ [ HH.text ("bias " <> pct s.genBias) ]
    , HH.span_ [ HH.text ("spread " <> pct s.genSpread) ]
    ]

pct :: Number -> String
pct x = show (round (x * 100.0)) <> "%"

-- | The NOTES one-shots as a compact 2×2: flatten every note to the scale root
-- | low (LOW, basslines) or middle (MID, melodies), seed a fresh MELODY line, or
-- | ROLL the Marbles once. Both octave floors follow the current key.
rollGrid :: forall m. H.ComponentHTML Action Slots m
rollGrid =
  HH.div [ style "display:grid;grid-template-columns:1fr 1fr;gap:4px;margin-top:7px" ]
    [ tabBtn "LOW" false (SetAllNotes 0)
    , tabBtn "MID" false (SetAllNotes (M.knobMax `div` 2))
    , tabBtn "MELODY" false SeedMelody
    , tabBtn "⟳ ROLL" false MarblesRoll
    ]

-- ---------------------------------------------------------------------------
-- Groove + transport chrome
-- ---------------------------------------------------------------------------

-- | GROOVE block: GATE length + SWING (off-beat lag) + HUMANISE (velocity
-- | jitter) — the controls that pull the sequence off the metronome.
feelBlock :: forall m. State -> H.ComponentHTML Action Slots m
feelBlock s =
  HH.div [ style "display:flex;align-items:flex-end;gap:8px" ]
    [ gateBlock s.odo
    , miniKnob SwingAmt (round (s.swing * 100.0)) "#7d8a93" "SWING" (show (round (s.swing * 100.0)) <> "%")
    , miniKnob VelHuman s.velHumanize "#7d8a93" "HUMAN" ("±" <> show s.velHumanize)
    ]

-- | GATE knob: gated-note length as % of step (10..200; past 100 the notes
-- | overlap into the next = legato, which lets portamento/glide slide).
gateBlock :: forall m. M.Odonus -> H.ComponentHTML Action Slots m
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
clockRow :: forall m. State -> H.ComponentHTML Action Slots m
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

statusBar :: forall m. State -> H.ComponentHTML Action Slots m
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

-- ---------------------------------------------------------------------------
-- NOTE field — the one grid that stays here (the value knobs)
-- ---------------------------------------------------------------------------

-- | A labelled small multiple: small-caps engraved label, then a 4×4 body.
fieldShell :: forall m. String -> H.ComponentHTML Action Slots m -> H.ComponentHTML Action Slots m
fieldShell label body =
  HH.div [ style "display:flex;flex-direction:column;gap:3px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.85" ] [ HH.text label ]
    , body
    ]

-- | NOTE field — a 4×4 of value knobs, the only field that edits a number.
noteField :: forall m. M.Odonus -> H.ComponentHTML Action Slots m
noteField odo =
  fieldShell "NOTE"
    ( HH.div_
        [ HH.div
            [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:6px" ]
            (mapWithIndex (noteCell odo) odo.cells)
        ]
    )

noteCell :: forall m. M.Odonus -> Int -> M.Cell -> H.ComponentHTML Action Slots m
noteCell odo i c =
  HH.div
    [ style $ cellChrome odo i
        <> ";padding:4px;aspect-ratio:1;display:flex;flex-direction:column;align-items:center;justify-content:center"
    ]
    [ HH.div [ style "width:100%;flex:1;min-height:0" ]
        [ knob
            -- cell.note is a raw KNOB now (0 .. knobMax); the pipeline equal-maps it
            -- over the scale, so the range is fixed, not the set cardinality.
            { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 9.0, color: "#b5832b", lo: 0, hi: M.knobMax, value: c.note, ticks: 0 }
            (KnobDown (CellNote i) c.note)
        ]
      -- Label the cell with the pitch its knob currently sounds — re-colours live as
      -- the harmony moves (the offset-free voiceLabel).
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#4a463d;margin-top:1px" ]
        [ HH.text (midiName (M.cellLabel odo c.note)) ]
    ]

-- | MIDI note name, scientific pitch (middle C = C4 = 60).
midiName :: Int -> String
midiName n =
  let names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]
  in fromMaybe "?" (names !! (n `mod` 12)) <> show ((n `div` 12) - 1)

controls :: forall m. State -> H.ComponentHTML Action Slots m
controls _ =
  -- The ARM toggle now lives on the tab in the top switcher (the ▶/❚❚ dot); this
  -- row keeps only its hint.
  HH.div [ style "display:flex;gap:8px;align-items:center;margin-top:14px" ]
    [ HH.span [ style $ engrave <> ";font-size:8px;color:#888273" ]
        [ HH.text "click a thumbnail to change a head's pattern" ]
    ]

nameplate :: forall m. State -> H.ComponentHTML Action Slots m
nameplate s =
  HH.div
    [ style $ "margin-top:16px;padding:7px 10px;border-radius:6px;"
        <> "background:linear-gradient(#c8a86a,#b8975a);box-shadow:0 1px 0 #00000022 inset;"
        <> engrave <> ";color:#3a3320;font-size:9px;display:flex;justify-content:space-between"
    ]
    [ HH.span [ style "letter-spacing:0.22em;color:#2c2718" ] [ HH.text "TRIGGERFISH" ]
    , HH.span [] [ HH.text $ "Model Odonus · " <> show (length s.odo.heads) <> "-head" ]
    ]
