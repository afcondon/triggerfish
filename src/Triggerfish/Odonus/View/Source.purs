-- | SOURCE panel — the live eDSL of the current setup (read-only). One-
-- | directional (GUI→text), updating live — the consistency-with-text the rig
-- | will consume.
module Triggerfish.Odonus.View.Source (edslPanel, edslText) where

import Prelude

import Data.Array (mapWithIndex, (!!))
import Data.Maybe (maybe)
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action, State)
import Triggerfish.Odonus.Grid.Widgets
  ( dirName, octLabel, panelShell, roman, romanNum, signed, speedRatio, style )

edslPanel :: forall m. State -> H.ComponentHTML Action () m
edslPanel s =
  panelShell s.collapsed "SOURCE" "eDSL" "flex:0 1 244px;min-width:0"
    [ HH.div
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10.5px;line-height:1.55;"
            <> "white-space:pre;color:#3a372e;background:#00000008;border:1px solid #00000012;"
            <> "border-radius:6px;padding:10px;overflow-x:auto;"
            <> "user-select:text;-webkit-user-select:text;cursor:text" ]
        [ HH.text (edslText s.odo) ] ]

-- | The current setup rendered as odonusWith{…} eDSL text. One-directional
-- | (GUI→text), updating live — the consistency-with-text the rig will consume.
edslText :: M.Odonus -> String
edslText o =
  let
    arr f = "[ " <> joinWith ", " (map f o.cells) <> " ]"
    bool b = if b then "T" else "F"
    headLine i hd =
      "    " <> roman i <> "  "
        <> maybe "?" _.name (M.patternLibrary !! hd.patternIx)
        <> "  " <> speedRatio hd.speedIx
        <> " " <> dirName hd.direction
        <> " " <> signed hd.transp
        <> " off " <> show hd.offset
        <> " len " <> show hd.len
        <> " E(" <> show hd.pulses <> "," <> show hd.esteps <> ")"
        <> (if hd.mute then "  (mute)" else "")
  in
    joinWith "\n"
      ( [ "odonusWith"
        , "  { scale: " <> Scale.scaleName (M.scaleOf o)
        , "  , distribution: " <> show o.dist
        , "  , octave: " <> octLabel o.octaveShift
        , "  , scalarTransp: " <> romanNum o.degShift
        , "  , gate: " <> show o.gatePct <> "%"
        , "  , notes: " <> arr (show <<< _.note)
        , "  , gate:  " <> arr (bool <<< _.gate)
        , "  , skip:  " <> arr (bool <<< _.skip)
        , "  , glide: " <> arr (bool <<< _.glide)
        , "  , len:   " <> arr (show <<< _.dur)
        , "  , ratchet:" <> arr (show <<< _.ratchet)
        , "  , vel:   " <> arr (show <<< _.vel)
        , "  , heads:"
        ] <> mapWithIndex headLine o.heads <> [ "  }" ] )
