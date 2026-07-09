-- | PLAYHEADS panel — the extracted Fugue-Machine head bank: the 16-combination
-- | head-activation matrix and the four head strips (mute, pattern thumbnail,
-- | direction / speed / interval / offset / length knobs).
module Triggerfish.Odonus.View.Playheads (playheadsPanel) where

import Prelude

import Data.Array (findIndex, mapWithIndex, range, (!!))
import Data.Int (round, toNumber)
import Data.Int.Bits (and, shr)
import Data.Maybe (fromMaybe, maybe)
import Data.Number (cos, pi, sin) as Num
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets
  ( engrave, headColor, miniKnob, panelShell, roman, signed
  , stepBtn, style, svgAttr, svgEl )

playheadsPanel :: forall m. State -> H.ComponentHTML Action Slots m
playheadsPanel s =
  panelShell s.collapsed "PLAYHEADS" "Fugue · Access" "flex:0 1 290px;min-width:min-content"
    [ phasingBlock s.odo
    , HH.span [ style $ engrave <> ";font-size:9px;opacity:0.85;display:block;margin-bottom:4px" ]
        [ HH.text "COMBINATIONS" ]
    , headMatrix s.odo
    , headBank s
    ]

-- | PHASING — the Reichian macros over all four heads at once. UNISON collapses
-- | to unison phase; FAN spreads the offsets into a static canon (0, n, 2n, 3n);
-- | STAGGER ramps the loop lengths for metric phasing (Clapping-Music drift);
-- | PHASE ± rotates the whole canon a step. FAN/STAGGER read back from head II,
-- | so they round-trip the gesture and reflect the live spread.
phasingBlock :: forall m. M.Odonus -> H.ComponentHTML Action Slots m
phasingBlock odo =
  let
    fanN = maybe 0 _.offset (odo.heads !! 1)
    stagN = 16 - maybe 16 _.len (odo.heads !! 1)
    -- SPREAD is a voicing morph now, so read the knob back by matching the heads'
    -- current transposes against the voicing table (exact right after a spread; a
    -- manual INT edit that breaks the match just reads as 0, like FAN/STAGGER).
    spreadN = fromMaybe 0 (findIndex (\v -> v == map _.transp odo.heads) M.spreadVoicings)
  in
    HH.div [ style "margin-bottom:12px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.85;display:block;margin-bottom:5px" ]
          [ HH.text "PHASING" ]
      , HH.button
          [ HE.onClick \_ -> UnifyHeads
          , style $ "width:100%;padding:6px;margin-bottom:8px;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
              <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:11px;color:#3f3c33" ]
          [ HH.text "≡ Unison · all heads = I" ]
      , HH.div [ style "display:flex;align-items:flex-end;justify-content:space-between;gap:8px" ]
          [ miniKnob FanOff fanN "#6f7f88" "FAN" (show fanN)
          , miniKnob StaggerLen stagN "#6f7f88" "STAGGER" (show stagN)
          , miniKnob HeadSpread spreadN "#6f7f88" "SPREAD" (show spreadN)
          , HH.div [ style "display:flex;flex-direction:column;align-items:center;width:52px" ]
              [ HH.span [ style $ engrave <> ";font-size:8px;margin-bottom:3px" ] [ HH.text "PHASE" ]
              , HH.div [ style "display:flex;gap:5px" ]
                  [ stepBtn "‹" (PhaseShift (-1)), stepBtn "›" (PhaseShift 1) ]
              ]
          ]
      ]

-- | The 16 head-activation combinations (2⁴) as a switch palette: each
-- | switch is four vertical bars (I–IV, lit when that head sounds in this
-- | combo); clicking it sets every head's mute in one move. The live combo
-- | is ringed. Cut from any group of heads to any other in a single click.
headMatrix :: forall m. M.Odonus -> H.ComponentHTML Action Slots m
headMatrix odo =
  let cur = M.headMask odo
  in HH.div
       [ style "display:grid;grid-template-columns:repeat(8,1fr);gap:4px;margin-bottom:12px" ]
       (map (comboSwitch cur) (range 0 15))

comboSwitch :: forall m. Int -> Int -> H.ComponentHTML Action Slots m
comboSwitch cur n =
  let live = n == cur
  in HH.div
       [ HE.onClick \_ -> SetHeadMask n
       , style $ "display:flex;gap:2px;align-items:center;justify-content:center;height:18px;"
           <> "border-radius:5px;cursor:pointer;background:#cbc6b6;box-shadow:0 0 0 1px "
           <> (if live then "#3f3c33,0 0 0 2px #3f3c3355" else "#00000018")
       ]
       (map (\h -> comboBar (and (shr n h) 1 == 1) h) (range 0 3))

comboBar :: forall m. Boolean -> Int -> H.ComponentHTML Action Slots m
comboBar on h =
  HH.div
    [ style $ "width:5px;height:11px;border-radius:2px;background:"
        <> (if on then headColor h else "#46433a")
        <> (if on then ";box-shadow:0 0 4px " <> headColor h else "")
    ] []

headBank :: forall m. State -> H.ComponentHTML Action Slots m
headBank s =
  HH.div [ style "display:flex;flex-direction:column;gap:8px" ]
    (mapWithIndex headStrip s.odo.heads)

headStrip :: forall m. Int -> M.Head -> H.ComponentHTML Action Slots m
headStrip h hd =
  let
    col = headColor h
    dim = if hd.mute then "opacity:0.42;" else ""
    pat = fromMaybe { name: "?", order: [] } (M.patternLibrary !! hd.patternIx)
  in
    HH.div
      [ style $ "display:flex;flex-direction:column;gap:8px;padding:8px 10px;border-radius:8px;background:#cbc6b6;box-shadow:0 0 0 1px " <> col <> "66;" <> dim ]
      -- TOP TIER: engage/bypass in the top-left corner, then SPEED as one wide row.
      [ HH.div [ style "display:flex;align-items:center;gap:12px" ]
          [ muteBlock h hd col
          , speedRow h hd.speedIx col
          ]
      -- BOTTOM TIER: pattern + direction · Euclid ring · INT knob.
      , HH.div [ style "display:flex;align-items:center;gap:14px" ]
          [ HH.div [ style "display:flex;flex-direction:column;align-items:center;gap:4px" ]
              [ patBlock h pat col hd.seqPos
              , dirRadio h hd.direction col
              ]
          , euclidCell h hd col
          , miniKnob (HeadTransp h) hd.transp col "INT" (signed hd.transp)
          ]
      ]

-- | The head's Euclidean gate E(pulses, steps) as a Selene-style dot ring — n dots
-- | around the circle, the k pulses filled, the live step ringed, "k/n" at centre —
-- | with four corner clickers: top nudges pulses (k−/k+), bottom nudges steps (n−/n+).
-- | The model clamps (pulses 0..16, steps 1..16), so the clickers can't run past.
euclidCell :: forall m. Int -> M.Head -> String -> H.ComponentHTML Action Slots m
euclidCell h hd col =
  HH.div [ style "position:relative;width:82px;height:82px;flex:0 0 auto" ]
    [ euclidRing 82.0 hd.pulses hd.esteps hd.seqPos col
    , cornerBtn "top:0;left:0" "k−" (SetHeadPulses h (hd.pulses - 1))
    , cornerBtn "top:0;right:0" "k+" (SetHeadPulses h (hd.pulses + 1))
    , cornerBtn "bottom:0;left:0" "n−" (SetHeadSteps h (hd.esteps - 1))
    , cornerBtn "bottom:0;right:0" "n+" (SetHeadSteps h (hd.esteps + 1))
    ]

-- | The dot ring itself. Consistent with Selene's Euclid rings: dots evenly round
-- | the circle (12 o'clock = step 0, clockwise), filled where E(k,n) pulses, the
-- | current playhead step outlined in ink.
euclidRing :: forall m. Number -> Int -> Int -> Int -> String -> H.ComponentHTML Action Slots m
euclidRing sz k n seqPos col =
  let
    c = sz / 2.0
    r = c - 12.0
    steps = max 1 n
    cur = seqPos `mod` steps
    r2 x = toNumber (round (x * 100.0)) / 100.0
    dotFor i =
      let
        ang = (toNumber i / toNumber steps) * 2.0 * Num.pi - Num.pi / 2.0
        on = M.euclidHit k n i
        live = i == cur
      in
        svgEl "circle"
          [ svgAttr "cx" (show (r2 (c + r * Num.cos ang)))
          , svgAttr "cy" (show (r2 (c + r * Num.sin ang)))
          , svgAttr "r" (if on then "4.0" else "2.6")
          , svgAttr "fill" (if on then col else "none")
          , svgAttr "stroke" (if live then "#3f3c33" else col)
          , svgAttr "stroke-width" (if live then "1.6" else (if on then "0" else "1")) ] []
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show sz <> " " <> show sz)
      , svgAttr "width" (show sz), svgAttr "height" (show sz), svgAttr "style" "display:block" ]
      ( map dotFor (range 0 (steps - 1))
          <>
            [ svgEl "text"
                [ svgAttr "x" (show c), svgAttr "y" (show (c + 4.0)), svgAttr "text-anchor" "middle"
                , svgAttr "fill" "#3f3c33", svgAttr "font-family" "'SF Mono',Menlo,monospace"
                , svgAttr "font-size" "15" ]
                [ HH.text (show (min k n) <> "/" <> show n) ]
            ]
      )

-- | A small absolutely-positioned +/− clicker sitting in one corner of the ring box.
cornerBtn :: forall m. String -> String -> Action -> H.ComponentHTML Action Slots m
cornerBtn pos label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "position:absolute;" <> pos <> ";width:17px;height:14px;padding:0;cursor:pointer;"
        <> "border:1px solid #a8a392;border-radius:4px;background:linear-gradient(#efece1,#ddd9cb);"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:8px;line-height:1;color:#5a564b" ]
    [ HH.text label ]

-- | SPEED as a single wide radio row across the top of the strip: every ratio in
-- | M.speedTable as a chip, the live one lit. flex-wrap so adding dotted values
-- | later simply wraps to a second row rather than overflowing.
speedRow :: forall m. Int -> Int -> String -> H.ComponentHTML Action Slots m
speedRow h cur col =
  HH.div [ style "flex:1;display:flex;flex-wrap:wrap;gap:3px" ]
    (mapWithIndex (speedChip h cur col) M.speedTable)

speedChip :: forall m. Int -> Int -> String -> Int -> Number -> H.ComponentHTML Action Slots m
speedChip h cur col ix val =
  let active = ix == cur
  in HH.button
       [ HE.onClick \_ -> SetHeadSpeed h ix
       , style $ "flex:1 1 auto;min-width:26px;padding:4px 0;border:1px solid #a8a392;border-radius:4px;"
           <> "cursor:pointer;line-height:1;font-family:'SF Mono',Menlo,monospace;font-size:10px;color:"
           <> (if active then "#1c1a12" else "#6a6456")
           <> ";background:" <> (if active then "linear-gradient(" <> col <> "," <> col <> ")"
                                 else "linear-gradient(#efece1,#ddd9cb)") ]
       [ HH.text (speedLbl val) ]

-- | Compact ratio label: unit fractions as glyphs, whole numbers bare, the rest
-- | as a short decimal (so 1.5 stays "1.5" and future dotted values read cleanly).
speedLbl :: Number -> String
speedLbl x
  | x == 0.125 = "⅛"
  | x == 0.25 = "¼"
  | x == 0.5 = "½"
  | x == 0.75 = "¾"
  | x == toNumber (round x) = show (round x)
  | otherwise = show x

-- | Direction as a three-way radio under the pattern thumbnail (→ forward,
-- | ← backward, ↔ pendulum) — frees the knob row, and reads at a glance.
dirRadio :: forall m. Int -> Int -> String -> H.ComponentHTML Action Slots m
dirRadio h cur col =
  HH.div [ style "display:flex;gap:3px;width:54px" ]
    (map (\d -> dirBtn (dirGlyph d) (cur == d) col (SetHeadDir h d)) [ 0, 1, 2 ])

dirGlyph :: Int -> String
dirGlyph = case _ of
  0 -> "→"
  1 -> "←"
  _ -> "↔"

dirBtn :: forall m. String -> Boolean -> String -> Action -> H.ComponentHTML Action Slots m
dirBtn glyph active col act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:1;padding:2px 0;border:1px solid #a8a392;border-radius:4px;cursor:pointer;line-height:1;"
        <> "font-size:11px;color:" <> (if active then "#1c1a12" else "#6a6456")
        <> ";background:" <> (if active then "linear-gradient(" <> col <> "," <> col <> ")" else "linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text glyph ]

muteBlock :: forall m. Int -> M.Head -> String -> H.ComponentHTML Action Slots m
muteBlock h hd col =
  HH.div
    [ HE.onClick \_ -> ToggleHeadMute h
    , style "display:flex;flex-direction:column;align-items:center;width:28px;cursor:pointer"
    ]
    [ HH.div
        [ style $ "width:11px;height:11px;border-radius:50%;background:"
            <> (if hd.mute then "#46433a" else col)
            <> (if hd.mute then "" else ";box-shadow:0 0 6px " <> col)
        ] []
    , HH.span [ style $ engrave <> ";font-size:11px;margin-top:2px;color:" <> col ] [ HH.text (roman h) ]
    ]

patBlock :: forall m. Int -> M.Pattern -> String -> Int -> H.ComponentHTML Action Slots m
patBlock h pat col seqPos =
  HH.div
    [ HE.onClick \_ -> CyclePattern h
    , style "display:flex;flex-direction:column;align-items:center;cursor:pointer;width:54px"
    ]
    [ patternThumb pat col seqPos
    , HH.span [ style $ engrave <> ";font-size:8px;margin-top:1px" ] [ HH.text pat.name ]
    ]

patternThumb :: forall m. M.Pattern -> String -> Int -> H.ComponentHTML Action Slots m
patternThumb pat color seqPos =
  let
    st = 11.0
    pd = 5.0
    cx g = pd + toNumber (g `mod` 4) * st + st / 2.0
    cy g = pd + toNumber (g / 4) * st + st / 2.0
    pts = joinWith " " (map (\g -> show (cx g) <> "," <> show (cy g)) pat.order)
    g0 = fromMaybe 0 (pat.order !! 0)
    gc = fromMaybe 0 (pat.order !! seqPos)
    gline i =
      [ svgEl "line"
          [ svgAttr "x1" (show (pd + toNumber i * st)), svgAttr "y1" (show pd)
          , svgAttr "x2" (show (pd + toNumber i * st)), svgAttr "y2" (show (pd + 4.0 * st))
          , svgAttr "stroke" "#0000001a", svgAttr "stroke-width" "0.5" ] []
      , svgEl "line"
          [ svgAttr "x1" (show pd), svgAttr "y1" (show (pd + toNumber i * st))
          , svgAttr "x2" (show (pd + 4.0 * st)), svgAttr "y2" (show (pd + toNumber i * st))
          , svgAttr "stroke" "#0000001a", svgAttr "stroke-width" "0.5" ] []
      ]
  in
    svgEl "svg" [ svgAttr "viewBox" "0 0 54 54", svgAttr "width" "46", svgAttr "height" "46" ]
      ( gline 0 <> gline 1 <> gline 2 <> gline 3 <> gline 4
          <>
            [ svgEl "polyline"
                [ svgAttr "points" pts, svgAttr "fill" "none", svgAttr "stroke" color
                , svgAttr "stroke-width" "1.4", svgAttr "stroke-linejoin" "round"
                , svgAttr "stroke-linecap" "round", svgAttr "opacity" "0.9" ] []
            , svgEl "circle"
                [ svgAttr "cx" (show (cx g0)), svgAttr "cy" (show (cy g0))
                , svgAttr "r" "2", svgAttr "fill" "none", svgAttr "stroke" "#3a362c", svgAttr "stroke-width" "1" ] []
            , svgEl "circle"
                [ svgAttr "cx" (show (cx gc)), svgAttr "cy" (show (cy gc))
                , svgAttr "r" "2.6", svgAttr "fill" color ] []
            ]
      )
