-- | PLAYHEADS panel — the extracted Fugue-Machine head bank: the 16-combination
-- | head-activation matrix and the four head strips (mute, pattern thumbnail,
-- | direction / speed / interval / offset / length knobs).
module Triggerfish.Odonus.View.Playheads (playheadsPanel) where

import Prelude

import Data.Array (mapWithIndex, range, (!!))
import Data.Int (toNumber)
import Data.Int.Bits (and, shr)
import Data.Maybe (fromMaybe)
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget(..), State)
import Triggerfish.Odonus.Grid.Widgets
  ( dirName, engrave, headColor, miniKnob, panelShell, roman, signed, speedRatio
  , style, svgAttr, svgEl )

playheadsPanel :: forall m. State -> H.ComponentHTML Action () m
playheadsPanel s =
  panelShell "PLAYHEADS" "Fugue · Access" "flex:0 1 290px;min-width:0"
    [ HH.button
        [ HE.onClick \_ -> UnifyHeads
        , style $ "width:100%;padding:6px;margin-bottom:10px;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
            <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:11px;color:#3f3c33" ]
        [ HH.text "≡ Unison · all heads = I" ]
    , HH.span [ style $ engrave <> ";font-size:9px;opacity:0.85;display:block;margin-bottom:4px" ]
        [ HH.text "COMBINATIONS" ]
    , headMatrix s.odo
    , headBank s
    ]

-- | The 16 head-activation combinations (2⁴) as a switch palette: each
-- | switch is four vertical bars (I–IV, lit when that head sounds in this
-- | combo); clicking it sets every head's mute in one move. The live combo
-- | is ringed. Cut from any group of heads to any other in a single click.
headMatrix :: forall m. M.Odonus -> H.ComponentHTML Action () m
headMatrix odo =
  let cur = M.headMask odo
  in HH.div
       [ style "display:grid;grid-template-columns:repeat(8,1fr);gap:4px;margin-bottom:12px" ]
       (map (comboSwitch cur) (range 0 15))

comboSwitch :: forall m. Int -> Int -> H.ComponentHTML Action () m
comboSwitch cur n =
  let live = n == cur
  in HH.div
       [ HE.onClick \_ -> SetHeadMask n
       , style $ "display:flex;gap:2px;align-items:center;justify-content:center;height:18px;"
           <> "border-radius:5px;cursor:pointer;background:#cbc6b6;box-shadow:0 0 0 1px "
           <> (if live then "#3f3c33,0 0 0 2px #3f3c3355" else "#00000018")
       ]
       (map (\h -> comboBar (and (shr n h) 1 == 1) h) (range 0 3))

comboBar :: forall m. Boolean -> Int -> H.ComponentHTML Action () m
comboBar on h =
  HH.div
    [ style $ "width:5px;height:11px;border-radius:2px;background:"
        <> (if on then headColor h else "#46433a")
        <> (if on then ";box-shadow:0 0 4px " <> headColor h else "")
    ] []

headBank :: forall m. State -> H.ComponentHTML Action () m
headBank s =
  HH.div [ style "display:flex;flex-direction:column;gap:8px" ]
    (mapWithIndex headStrip s.odo.heads)

headStrip :: forall m. Int -> M.Head -> H.ComponentHTML Action () m
headStrip h hd =
  let
    col = headColor h
    dim = if hd.mute then "opacity:0.42;" else ""
    pat = fromMaybe { name: "?", order: [] } (M.patternLibrary !! hd.patternIx)
  in
    HH.div
      [ style $ "display:flex;align-items:center;gap:10px;padding:7px 10px;border-radius:8px;background:#cbc6b6;box-shadow:0 0 0 1px " <> col <> "66;" <> dim ]
      [ muteBlock h hd col
      , patBlock h pat col hd.seqPos
      , HH.div
          [ style "display:grid;grid-template-columns:repeat(3,1fr);gap:6px 4px" ]
          [ miniKnob (HeadDir h) hd.direction col "DIR" (dirName hd.direction)
          , miniKnob (HeadSpeed h) hd.speedIx col "SPD" (speedRatio hd.speedIx)
          , miniKnob (HeadTransp h) hd.transp col "INT" (signed hd.transp)
          , miniKnob (HeadOffset h) hd.offset col "OFF" (show hd.offset)
          , miniKnob (HeadLen h) hd.len col "LEN" (show hd.len)
          ]
      ]

muteBlock :: forall m. Int -> M.Head -> String -> H.ComponentHTML Action () m
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

patBlock :: forall m. Int -> M.Pattern -> String -> Int -> H.ComponentHTML Action () m
patBlock h pat col seqPos =
  HH.div
    [ HE.onClick \_ -> CyclePattern h
    , style "display:flex;flex-direction:column;align-items:center;cursor:pointer;width:54px"
    ]
    [ patternThumb pat col seqPos
    , HH.span [ style $ engrave <> ";font-size:8px;margin-top:1px" ] [ HH.text pat.name ]
    ]

patternThumb :: forall m. M.Pattern -> String -> Int -> H.ComponentHTML Action () m
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
