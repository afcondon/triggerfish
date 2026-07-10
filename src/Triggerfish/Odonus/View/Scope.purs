-- | SCOPE panel — the hero panel on the far left: a scrolling note river,
-- | octave gridlines + labels under the stretched note SVG. Notes emit at the
-- | right edge and flow left, fading with age.
module Triggerfish.Odonus.View.Scope (scopePanel) where

import Prelude

import Data.Array (concatMap, filter, length)
import Data.Int (toNumber)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Grid.Types (Action(..), Mark, NoteEvent, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (clampI, headColor, style, svgAttr, svgEl)
import Triggerfish.Odonus.Logbook (noteCount)

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
scopePanel :: forall m. State -> H.ComponentHTML Action Slots m
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
              ( map (markLine s.nowMicros) (visibleMarks s.nowMicros s.logbook.marks)
                  <> map (noteBar s.nowMicros) s.notes )
          , logbookOverlay s
          ]
    )

-- | The always-on logbook readout + mark button, floated top-left over the
-- | river. There's no arm — the rig is always capturing; the "● logging" dot
-- | just confirms it. "◆ mark" flags the current instant (a gold line on the
-- | river); the count shows captured notes and flags this session.
logbookOverlay :: forall m. State -> H.ComponentHTML Action Slots m
logbookOverlay s =
  HH.div
    [ style $ "position:absolute;top:10px;left:10px;display:flex;align-items:center;gap:9px;"
        <> "padding:5px 9px;border-radius:8px;background:#ffffff0d;backdrop-filter:blur(2px);"
        <> "border:1px solid #ffffff14;font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#c9c4b4" ]
    [ HH.span [ style "display:flex;align-items:center;gap:4px" ]
        [ HH.span [ style "width:7px;height:7px;border-radius:50%;background:#c65a4a;box-shadow:0 0 5px #c65a4a" ] []
        , HH.text "logging" ]
    , HH.span [ style "opacity:0.7" ]
        [ HH.text (show (noteCount s.logbook) <> " notes · " <> show (length s.logbook.marks) <> " ◆") ]
    , HH.button
        [ HE.onClick \_ -> MarkNow
        , style $ "padding:2px 9px;border-radius:6px;cursor:pointer;font-family:Georgia,serif;font-size:10px;"
            <> "color:#e8c14a;border:1px solid #e8c14a55;background:#e8c14a1a" ]
        [ HH.text "◆ mark" ]
    ]

-- | Marks recent enough to still be on-screen (within the river's fade span).
visibleMarks :: Number -> Array Mark -> Array Mark
visibleMarks now = filter (\m -> (now - m.atMicros) / 1000.0 * pxPerMs < riverW)

-- | A flagged instant as a full-height gold line, positioned like a note by age.
markLine :: forall m. Number -> Mark -> H.ComponentHTML Action Slots m
markLine now m =
  let x = riverW - (now - m.atMicros) / 1000.0 * pxPerMs - 10.0
  in
    svgEl "rect"
      [ svgAttr "x" (show x), svgAttr "y" "0"
      , svgAttr "width" "1.5", svgAttr "height" (show riverH)
      , svgAttr "fill" "#e8c14a", svgAttr "opacity" "0.5"
      ] []

-- | Faint horizontal line + a "C4"-style label at each octave C (HTML, so the
-- | text isn't stretched by the scope's preserveAspectRatio=none).
octaveGuides :: forall m. Array (H.ComponentHTML Action Slots m)
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

noteBar :: forall m. Number -> NoteEvent -> H.ComponentHTML Action Slots m
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
