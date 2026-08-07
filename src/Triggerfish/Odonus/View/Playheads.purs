-- | PLAYHEADS panel — the extracted Fugue-Machine head bank: the 16-combination
-- | head-activation matrix and the four head strips (mute, pattern thumbnail,
-- | direction / speed / interval / offset / length knobs).
module Triggerfish.Odonus.View.Playheads (playheadsPanel, euclidBounds) where

import Prelude

import Data.Array (findIndex, mapWithIndex, range, (!!))
import Data.Int (round, toNumber)
import Data.Int.Bits (and, shr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Ui.Euclid as Euclid
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets
  ( engrave, headColor, miniKnob, panelShell, roman, signed
  , style, svgAttr, svgEl )

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
-- | STAGGER ramps the loop lengths for metric phasing (Clapping-Music drift).
-- | FAN/STAGGER read back from head II, so they round-trip the gesture and reflect
-- | the live spread.
-- |
-- | PHASE ‹ › (rotate the whole canon a step) was dropped 2026-08-06 (AC): it
-- | overlapped FAN, which already authors the offsets, and the block was costing
-- | vertical space the panel didn't have. UNISON shrank from a full-width bar into
-- | the slot PHASE vacated, so the section is one row instead of three.
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
      , HH.div [ style "display:flex;align-items:flex-end;justify-content:space-between;gap:8px" ]
          [ miniKnob FanOff fanN "#6f7f88" "FAN" (show fanN)
          , miniKnob StaggerLen stagN "#6f7f88" "STAGGER" (show stagN)
          , miniKnob HeadSpread spreadN "#6f7f88" "SPREAD" (show spreadN)
          , HH.div [ style "display:flex;flex-direction:column;align-items:center;width:52px" ]
              [ HH.span [ style $ engrave <> ";font-size:8px;margin-bottom:3px" ] [ HH.text "UNISON" ]
              , HH.button
                  [ HE.onClick \_ -> UnifyHeads
                  , HP.title "collapse every head to unison — all heads = I"
                  , style $ "width:44px;padding:7px 0;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
                      <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:13px;color:#3f3c33" ]
                  [ HH.text "≡" ]
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
    (mapWithIndex (headStrip s) s.odo.heads)

-- | Odonus's Euclid limits. Sixteen, and not by taste: `Reef.Odonus.stepHead`
-- | clamps `esteps` to 1..16 as it runs, so a wider ring here would draw steps
-- | the engine never plays — and the engine is shared with the BEAM, where the
-- | conformance suite pins the behaviour. Raise it in reef first, on both
-- | runtimes, and this follows.
euclidBounds :: Euclid.Bounds
euclidBounds = Euclid.boundedBy 16

headStrip :: forall m. State -> Int -> M.Head -> H.ComponentHTML Action Slots m
headStrip s h hd =
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
          , euclidCell s h hd col
          , miniKnob (HeadTransp h) hd.transp col "INT" (signed hd.transp)
          ]
      ]

-- | The head's Euclidean gate E(pulses, steps), drawn by the shared control
-- | (`Triggerfish.Ui.Euclid`) — the same instrument as Selene's POLYEUCLID slots.
-- | Click selects the ring, arrows then edit it: ←/→ steps (n), ↑/↓ pulses (k),
-- | shift for a four-step stride. This replaced four corner ± clickers, which
-- | were four targets for what is really two axes.
euclidCell :: forall m. State -> Int -> M.Head -> String -> H.ComponentHTML Action Slots m
euclidCell s h hd col =
  Euclid.cell (euclidStyle col) euclidChrome
    { selected: s.selEuclid == Just h
    , onSelect: SelectEuclid h
    , onKey: EuclidKey
    }
    (Just hd.seqPos)
    { beats: hd.pulses, steps: hd.esteps }

-- | Odonus's dressing for the shared ring: the voice's own colour, the live step
-- | outlined in ink, sized to the voice strip.
euclidStyle :: String -> Euclid.Style
euclidStyle col = Euclid.defaultStyle
  { size = 82.0, inset = 12.0, dotOn = 4.0, dotOff = 2.6
  , fill = col, ink = "#3f3c33", fontSize = 15.0, stretch = false
  }

-- | The selection box, tuned for the `#cbc6b6` voice strip rather than Selene's
-- | white slot row.
euclidChrome :: Euclid.Chrome
euclidChrome =
  { idle: "background:transparent;border:1px solid transparent;"
  , active: "background:#ffffff44;border:1px solid #3f3c33;"
  }

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
