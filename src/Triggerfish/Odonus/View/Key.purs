-- | KEY panel — the pitch quantizer. The pitch SOURCE is the top-level choice: a
-- | Scale ⇄ Vetula SWITCH heads the pane and only the chosen view shows. The Scale
-- | view carries the KEY·SCALE random source, the pitch-class keyboard, ROOT and
-- | SCALE; the Vetula view follows a Performance voice. (OCTAVE/DEGREE moved to the
-- | NOTES pane; SPREAD and the dead Equal/Natural MODE toggle were removed.)
module Triggerfish.Odonus.View.Key (quantizerPanel) where

import Prelude

import Data.Array (elem, null, range, (:))
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hylograph.Halogen.UI.Select as Select
import Type.Proxy (Proxy(..))
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action(..), GenKind(..), Slots, SourceTag(..), State)
import Triggerfish.Odonus.Grid.Widgets
  ( engrave, genRow, labelledRow, panelShell, stepperRow, style, tabBtn )
import Triggerfish.Odonus.View.Scenes (scenesBody)

-- | The merged KEY pane (#139): the pitch-quantizer controls, then the SCENES
-- | song machinery stacked below in the same scrolling column — folded here so
-- | the two no longer cost two horizontal columns. `scenesBody` leads with its
-- | own divider + label, so the seam reads cleanly.
quantizerPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
quantizerPanel s =
  let tag = s.source
  in
    panelShell s.collapsed "KEY" "Source · Song" "flex:0 1 290px;min-width:min-content"
      ( -- SOURCE is a two-way SWITCH now (not a selector over two dimmed sections):
        -- pick Scale or Vetula and only that view shows. The pitch-set that drives
        -- the snap. (OCTAVE/DEGREE live at the NOTES pane; this is the set itself.)
        [ sourceToggle tag
        , HH.div [ style "margin-top:10px;padding-top:9px;border-top:1px solid #00000018" ]
            (case tag of
               SScale -> scaleSection s
               SVetula -> [ followSection s ])
        ] <> scenesBody s )

-- | The Scale ⇄ Vetula switch that heads the pane — the two pitch sources, one
-- | shown at a time.
sourceToggle :: forall m. SourceTag -> H.ComponentHTML Action Slots m
sourceToggle tag =
  HH.div [ style "display:flex;gap:3px" ]
    [ tabBtn "SCALE" (tag == SScale) (SetSource SScale)
    , tabBtn "VETULA" (tag == SVetula) (SetSource SVetula)
    ]

-- ---------------------------------------------------------------------------
-- SCALE-KEY source
-- ---------------------------------------------------------------------------

scaleSection :: forall m. MonadAff m => State -> Array (H.ComponentHTML Action Slots m)
scaleSection s =
  -- The KEY·SCALE random source (moved here from the PARAMETERS pane — it mutates
  -- the scale/key, so it belongs with the scale controls).
  [ genRow s GKey
  , HH.div [ style "margin-top:9px" ] [ pcKeyboard s.odo ]
  , stepperRow "ROOT" (Scale.rootName s.odo.rootPc)
      (SetRoot (s.odo.rootPc - 1)) (SetRoot (s.odo.rootPc + 1))
  , scalePicker s
  ]

-- | SCALE — the shared Hylograph Select widget (dogfooded from Vetula), a
-- | cascading nested menu over Odonus's OWN preset scales (no Vetula/Harmonia
-- | linkage). Picking one raises `Selected name` → `PickScale`, which jumps the
-- | scale by driving the existing relative CycleScaleType input.
scalePicker :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
scalePicker s =
  HH.div [ style "margin:8px 0" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;margin-bottom:4px" ] [ HH.text "SCALE" ]
    , HH.slot (Proxy :: _ "scaleSelect") unit Select.component
        ((Select.cascadingInput scaleGroups)
           { selected = Just (M.scaleTypeName s.odo), searchable = true, placeholder = "Scale…" })
        (\(Select.Selected v) -> PickScale v)
    ]

-- | Odonus's preset scales, grouped by family for the cascading menu. `value` is
-- | the reef `scaleTypes` name (what PickScale matches); `label` is the readable
-- | form. A curated starter set — the eventual scale-library page will grow it and
-- | let favourites feed this picker.
scaleGroups :: Array Select.OptionGroup
scaleGroups =
  [ { label: "Major Modes"
    , options:
        [ { value: "major", label: "Ionian (Major)" }
        , { value: "dorian", label: "Dorian" }
        , { value: "phrygian", label: "Phrygian" }
        , { value: "lydian", label: "Lydian" }
        , { value: "mixolydian", label: "Mixolydian" }
        , { value: "minor", label: "Aeolian (Minor)" }
        , { value: "locrian", label: "Locrian" }
        ]
    }
  , { label: "Melodic Minor Modes"
    , options:
        [ { value: "melodicMinor", label: "Melodic Minor" }
        , { value: "dorianFlat2", label: "Dorian ♭2" }
        , { value: "lydianAugmented", label: "Lydian Augmented" }
        , { value: "lydianDominant", label: "Lydian Dominant" }
        , { value: "melodicMajor", label: "Melodic Major" }
        , { value: "halfDiminished", label: "Half-Diminished" }
        , { value: "altered", label: "Altered" }
        ]
    }
  , { label: "Harmonic Minor Modes"
    , options:
        [ { value: "harmonicMinor", label: "Harmonic Minor" }
        , { value: "locrianNat6", label: "Locrian ♮6" }
        , { value: "ionianAugmented", label: "Ionian Augmented" }
        , { value: "ukrainianDorian", label: "Ukrainian Dorian" }
        , { value: "phrygianDominant", label: "Phrygian Dominant" }
        , { value: "lydianSharp2", label: "Lydian ♯2" }
        , { value: "alteredDiminished", label: "Altered Diminished" }
        ]
    }
  , { label: "Pentatonic"
    , options:
        [ { value: "pentaMajor", label: "Penta Major" }
        , { value: "pentaMinor", label: "Penta Minor" }
        ]
    }
  , { label: "Messiaen"
    , options:
        [ { value: "wholetone", label: "Mode 1 · Whole Tone" }
        , { value: "messiaen2", label: "Mode 2 · Octatonic" }
        , { value: "messiaen3", label: "Mode 3" }
        , { value: "messiaen4", label: "Mode 4" }
        , { value: "messiaen5", label: "Mode 5" }
        , { value: "messiaen6", label: "Mode 6" }
        , { value: "messiaen7", label: "Mode 7" }
        ]
    }
  , { label: "Carnatic"
    , options:
        [ { value: "mayamalavagowla", label: "Mayamalavagowla" }
        , { value: "simhendramadhyamam", label: "Simhendramadhyamam" }
        , { value: "shanmukhapriya", label: "Shanmukhapriya" }
        ]
    }
  , { label: "Symmetric"
    , options:
        [ { value: "chromatic", label: "Chromatic" } ]
    }
  ]

-- ---------------------------------------------------------------------------
-- VETULA source — follow a Performance voice
-- ---------------------------------------------------------------------------

-- | CHORDS · FOLLOW VETULA sub-panel: the live Vetula→Odonus bridge. The overlay
-- | snaps the output to a chord conducted by a Vetula Performance voice. Pick one
-- | of the Odonus-bound voices (each shown by its id = the voice's channel field)
-- | to follow. The shell repolls the live chord ~100ms, so as the Vetula voice
-- | walks its progression Odonus follows.
followSection :: forall m. State -> H.ComponentHTML Action Slots m
followSection s =
  let live = s.odo.chord.on   -- a Vetula chord is actually driving the snap
  in
    HH.div_
      [ HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:7px" ]
          [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "FOLLOW VETULA" ]
          , HH.span [ style $ engrave <> ";font-size:8px;color:" <> (if live then "#5a7a3a" else "#a07a30") ]
              [ HH.text (if live then "● live chord" else "◌ not active") ]
          ]
      , labelledRow "VOICE"
          ( tabBtn "free" (isNothing s.follow) (SetFollow Nothing)
              : map (\vc -> tabBtn (show vc.id) (s.follow == Just vc.id) (SetFollow (Just vc.id))) s.voiceChords )
      -- The clear callout: Vetula is the chosen source, but nothing is feeding it.
      , if null s.voiceChords
          then HH.div
                 [ style $ "margin-top:8px;padding:7px 9px;border-radius:6px;border:1px solid #d8b66a;"
                     <> "background:#f6edd6;font-family:Georgia,serif;font-size:9px;color:#7a5c1a;line-height:1.5" ]
                 [ HH.text "Not active — no Vetula voice is feeding. In Vetula's Performance tab, load a progression and set a voice's destination → odo (its id = the voice's channel field)." ]
          else HH.text ""
      ]

-- ---------------------------------------------------------------------------
-- shared scale widgets
-- ---------------------------------------------------------------------------

-- | A 12-key chromatic strip: in-scale pitch classes lit, the root accented.
-- | Click a key to toggle it in/out of the scale (direct note choice).
pcKeyboard :: forall m. M.Odonus -> H.ComponentHTML Action Slots m
pcKeyboard odo =
  let lit = Scale.pitchClassesOf (M.scaleOf odo)
  in
    HH.div [ style "display:flex;gap:2px;margin-bottom:12px" ]
      (map (pcKey odo.rootPc lit) (range 0 11))

pcKey :: forall m. Int -> Array Int -> Int -> H.ComponentHTML Action Slots m
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
