-- | SCOPE panel — the hero panel on the far left: a scrolling note river,
-- | octave gridlines + labels under the stretched note SVG. Notes emit at the
-- | right edge and flow left, fading with age.
module Triggerfish.Odonus.View.Scope (scopePanel) where

import Prelude

import Data.Array (concatMap)
import Data.Int (toNumber)
import Halogen as H
import Halogen.HTML as HH
import Triggerfish.Odonus.Grid.Types (Action, NoteEvent, State)
import Triggerfish.Odonus.Grid.Widgets (clampI, headColor, style, svgAttr, svgEl)

riverW :: Number
riverW = 380.0

riverH :: Number
riverH = 520.0

pxPerMs :: Number
pxPerMs = 0.05

pitchToY :: Int -> Number
pitchToY pitch = riverH * (1.0 - (toNumber (clampI 24 96 pitch) - 24.0) / 72.0)

-- | The scope — the hero panel on the far left, full height, flex-grow. Octave
-- | gridlines + note labels (HTML, undistorted) under the stretched note SVG;
-- | notes emit at the right edge and flow left, fading with age.
scopePanel :: forall m. State -> H.ComponentHTML Action () m
scopePanel s =
  HH.div
    [ style $ "flex:1 1 360px;min-width:0;height:calc(100vh - var(--tf-bar));position:relative;overflow:hidden;"
        <> "background:radial-gradient(140% 100% at 100% 50%,#15140f,#0b0a07)" ]
    ( octaveGuides
        <>
          [ svgEl "svg"
              [ svgAttr "width" "100%", svgAttr "height" "100%"
              , svgAttr "viewBox" "0 0 380 520", svgAttr "preserveAspectRatio" "none"
              , style "position:absolute;inset:0" ]
              (map (noteBar s.nowMicros) s.notes)
          ]
    )

-- | Faint horizontal line + a "C4"-style label at each octave C (HTML, so the
-- | text isn't stretched by the scope's preserveAspectRatio=none).
octaveGuides :: forall m. Array (H.ComponentHTML Action () m)
octaveGuides = concatMap guide [ 24, 36, 48, 60, 72, 84, 96 ]
  where
  guide pitch =
    let pct = pitchToY pitch / riverH * 100.0
    in
      [ HH.div [ style $ "position:absolute;left:0;right:0;top:" <> show pct
            <> "%;height:1px;background:#ffffff12" ] []
      , HH.div [ style $ "position:absolute;left:7px;top:calc(" <> show pct
            <> "% - 7px);font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#ffffff3a" ]
          [ HH.text ("C" <> show (pitch / 12 - 1)) ]
      ]

noteBar :: forall m. Number -> NoteEvent -> H.ComponentHTML Action () m
noteBar now n =
  let
    elapsedMs = (now - n.fireUnixMicros) / 1000.0
    x = riverW - elapsedMs * pxPerMs - 10.0
  in
    svgEl "rect"
      [ svgAttr "x" (show x)
      , svgAttr "y" (show (pitchToY n.pitch))
      , svgAttr "width" "9", svgAttr "height" "5", svgAttr "rx" "2"
      , svgAttr "fill" (headColor n.headIdx)
      , svgAttr "opacity" (show (max 0.12 (1.0 - elapsedMs / 7000.0)))
      ] []
