-- | KEY panel — the pitch quantizer. Re-ordered around the reframe: the pitch
-- | SOURCE is the top-level choice, not the scale. OCTAVE sits above everything
-- | (always safe). Below the source selector, exactly one sub-section is live —
-- | SCALE-KEY (root / scale / spread / scalar-transpose / mode), CHORD (the
-- | internal McMullen progression), or VETULA (follow a Performance voice) — and
-- | the inactive ones grey out. Scalar transpose lives inside SCALE because it's
-- | a scale-degree move, meaningless when an external source (Vetula/chord) drives.
module Triggerfish.Odonus.View.Key (quantizerPanel) where

import Prelude

import Data.Array (elem, length, mapWithIndex, null, range, (:))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget(..), SourceTag(..), State)
import Triggerfish.Odonus.Grid.Widgets
  ( engrave, labelledRow, octLabel, panelShell, romanNum, stepperRow, style, tabBtn )

quantizerPanel :: forall m. State -> H.ComponentHTML Action () m
quantizerPanel s =
  let tag = sourceTagOf s
  in
    panelShell s.collapsed "KEY" "Source · Transpose" "flex:0 1 278px;min-width:min-content"
      -- OCTAVE: chromatic ± octaves, COMMON to every source — always safe, so it
      -- sits at the top above the source-specific controls.
      [ labelledRow "OCTAVE"
          (map (\n -> tabBtn (octLabel n) (s.odo.octaveShift == n) (SetOctave n)) [ -2, -1, 0, 1, 2 ])
      -- SOURCE: the pitch-set that drives the snap. The selected one's section is
      -- live; the others grey out.
      , labelledRow "SOURCE"
          [ tabBtn "Scale" (tag == SScale) (SetSource SScale)
          , tabBtn "Chord" (tag == SChord) (SetSource SChord)
          , tabBtn "Vetula" (tag == SVetula) (SetSource SVetula)
          ]
      , subSection (tag == SScale) (scaleSection s)
      , subSection (tag == SChord) (chordSection s)
      , subSection (tag == SVetula) [ followSection s ]
      ]

-- | Which source is active, derived from the overlay + follow state (consistent
-- | with the `PitchSource` print/parse model): a followed voice → Vetula; the
-- | overlay on with no follow → the internal Chord progression; else the Scale.
sourceTagOf :: State -> SourceTag
sourceTagOf s =
  if isJust s.follow then SVetula
  else if s.odo.chord.on then SChord
  else SScale

-- | A source sub-section: live, or greyed + inert when its source isn't selected.
subSection :: forall m. Boolean -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
subSection active body =
  HH.div
    [ style $ "margin-top:10px;padding-top:9px;border-top:1px solid #00000018;"
        <> (if active then "" else "opacity:0.34;pointer-events:none;filter:grayscale(0.5)") ]
    body

-- ---------------------------------------------------------------------------
-- SCALE-KEY source
-- ---------------------------------------------------------------------------

scaleSection :: forall m. State -> Array (H.ComponentHTML Action () m)
scaleSection s =
  [ pcKeyboard s.odo
  , stepperRow "ROOT" (Scale.rootName s.odo.rootPc)
      (SetRoot (s.odo.rootPc - 1)) (SetRoot (s.odo.rootPc + 1))
  , HH.div [ style "display:flex;align-items:flex-end;gap:10px;margin:8px 0" ]
      [ HH.div [ style "flex:1;min-width:0" ]
          [ stepperRow "SCALE" (M.scaleTypeName s.odo) (CycleScaleType (-1)) (CycleScaleType 1) ]
      , spreadBlock s.odo
      ]
  -- SCALAR TRANSP: shift the whole pattern by whole scale degrees, in-key. Lives
  -- in the SCALE section — it needs a known scale, so it's not safe off-scale.
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

-- ---------------------------------------------------------------------------
-- CHORD source — the internal McMullen "Yellow" progression
-- ---------------------------------------------------------------------------

chordSection :: forall m. State -> Array (H.ComponentHTML Action () m)
chordSection s =
  [ HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:7px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "PROGRESSION" ]
      , HH.button
          [ HE.onClick \_ -> ChordRoll
          , style $ "padding:4px 10px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
              <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:10px;color:#3f3c33" ]
          [ HH.text "⟳ new" ]
      ]
  , HH.div [ style "display:flex;gap:4px;flex-wrap:wrap;margin-bottom:9px" ]
      (mapWithIndex (chordChip s.odo.chord.ix) s.odo.chord.picks)
  , HH.div [ style "display:flex;align-items:center;gap:9px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "STEPS/CHORD" ]
      , HH.div [ style "width:38px;height:38px" ]
          [ knob
              { cx: 22.0, cy: 22.0, rOuter: 18.0, rInner: 7.0, color: "#b5832b"
              , lo: M.chordPeriodMin, hi: M.chordPeriodMax, value: s.odo.chord.period, ticks: 0 }
              (KnobDown ChordStep s.odo.chord.period) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33" ]
          [ HH.text (show s.odo.chord.period) ]
      ]
  ]

-- One chord of the progression: its name, the current one in the cycle accented.
chordChip :: forall m. Int -> Int -> Int -> H.ComponentHTML Action () m
chordChip curIx i pk =
  HH.span
    [ style $ "padding:4px 8px;border-radius:5px;font-family:'SF Mono',Menlo,monospace;font-size:10px;"
        <> (if curIx == i then "background:#b5832b;color:#1c1a12" else "background:#cbc6b6;color:#3f3c33") ]
    [ HH.text (M.chordNameAt pk) ]

-- ---------------------------------------------------------------------------
-- VETULA source — follow a Performance voice
-- ---------------------------------------------------------------------------

-- | CHORDS · FOLLOW VETULA sub-panel: the live Vetula→Odonus bridge. The overlay
-- | snaps the output to a chord conducted by a Vetula Performance voice. Pick one
-- | of the Odonus-bound voices (each shown by its id = the voice's channel field)
-- | to follow. The shell repolls the live chord ~100ms, so as the Vetula voice
-- | walks its progression Odonus follows.
followSection :: forall m. State -> H.ComponentHTML Action () m
followSection s =
  HH.div_
    [ HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:7px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "FOLLOW VETULA" ]
        , HH.span [ style $ engrave <> ";font-size:8px;color:#888273" ]
            [ HH.text (if isJust s.follow then "● following" else "○ free") ]
        ]
    , labelledRow "VOICE"
        ( tabBtn "free" (isNothing s.follow) (SetFollow Nothing)
            : map (\vc -> tabBtn (show vc.id) (s.follow == Just vc.id) (SetFollow (Just vc.id))) s.voiceChords )
    , if null s.voiceChords
        then HH.div [ style $ engrave <> ";font-size:8px;color:#888273;margin-top:4px;line-height:1.5" ]
               [ HH.text "No Odonus-bound Vetula voices. In Vetula's Performance rack, set a voice's destination to → ODO (its id = the voice channel field)." ]
        else HH.text ""
    ]

-- ---------------------------------------------------------------------------
-- shared scale widgets
-- ---------------------------------------------------------------------------

-- | A 12-key chromatic strip: in-scale pitch classes lit, the root accented.
-- | Click a key to toggle it in/out of the scale (direct note choice).
pcKeyboard :: forall m. M.Odonus -> H.ComponentHTML Action () m
pcKeyboard odo =
  let lit = Scale.pitchClassesOf (M.scaleOf odo)
  in
    HH.div [ style "display:flex;gap:2px;margin-bottom:12px" ]
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

-- | The Marbles-style SPREAD knob: drag to grow the scale from the root outward
-- | (unison → fifth → fourth → … → full chromatic). Value = note count.
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
