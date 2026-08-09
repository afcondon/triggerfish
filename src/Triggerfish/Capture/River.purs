-- | `Triggerfish.Capture.River` — the LIVE note river, machine-agnostic. The second
-- | of the two capture surfaces, and the one people actually watch while playing:
-- |
-- |   * **River** (this module) — a CONSTANT time→space scale (`pxPerMs`). Notes are
-- |     emitted at one edge and flow toward the other at a fixed speed, fading with
-- |     age. Nothing rescales as the session grows, so the motion is smooth. Redraw
-- |     it off a frame timer and it reads like a chart recorder.
-- |   * **`Capture.View.capturePanel`** — the WHOLE-SESSION fit (tMin..tMax squeezed
-- |     into the box). Right for REPLAY, where you want the entire take at once to
-- |     pick a phrase out of it; wrong for LIVE, where every new note rescales the
-- |     picture and the roll appears to lurch.
-- |
-- | Generalised from `Odonus.View.Scope` (now a thin adapter over this) when Vetula
-- | needed the same river running the other way (AC, 2026-08-05). `Flow` is the only
-- | parameter that differs: Odonus emits at the right edge and flows left; Vetula's
-- | surface sits to the RIGHT of its voices, so it emits at the left edge — next to
-- | the voice that played the note — and flows right.
-- |
-- | Pure and polymorphic in the host's `action` (nothing here is clickable): the
-- | host wraps it with its own overlays and gestures.
module Triggerfish.Capture.River
  ( Flow(..)
  , RiverWiring
  , RiverState
  , riverPanel
  , windowMicros
  ) where

import Prelude

import Data.Array (concatMap, filter)
import Data.Int (toNumber)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Triggerfish.Capture.Types (Mark)
import Triggerfish.Clips (NoteEvent)
import Halogen.Widgets.Svg (svgAttr, svgEl)

-- | Which way the river runs. `FlowLeft` = emitted at the RIGHT edge, ageing
-- | leftward (Odonus). `FlowRight` = emitted at the LEFT edge, ageing rightward
-- | (Vetula, whose river sits to the right of the voices that feed it).
data Flow = FlowLeft | FlowRight

derive instance eqFlow :: Eq Flow

type RiverWiring =
  { flow :: Flow
  , headColor :: Int -> String   -- colour a note by its source voice/channel
  }

-- | What the river reads: the current instant (in the same time base the notes were
-- | stamped in), the recent notes, and the flagged instants.
type RiverState =
  { nowMicros :: Number
  , notes :: Array NoteEvent
  , marks :: Array Mark
  }

-- viewBox units. `preserveAspectRatio none` stretches these to the container, which
-- is exactly what we want for the PITCH axis (use all the height there is) — but it
-- means a note's mark is only a stable size because the TIME axis has a fixed scale
-- and a fixed viewBox width. That's the whole reason the live surface can't be the
-- fit-to-session renderer: there, `tlW` covers an ever-growing span, so the marks
-- shrink toward slivers as you play.
riverW :: Number
riverW = 380.0

riverH :: Number
riverH = 520.0

-- | The river's scale: 0.05 viewBox units per millisecond ⇒ `riverW` spans ~7.6s.
pxPerMs :: Number
pxPerMs = 0.05

-- | A note has faded out by here. Hosts prune their recent-note array to this span.
fadeMs :: Number
fadeMs = 7000.0

-- | The same span in micros, for hosts pruning on a frame tick.
windowMicros :: Number
windowMicros = fadeMs * 1000.0

-- | The leading edge a note is emitted from, inset a little so it isn't clipped.
emitEdge :: Flow -> Number
emitEdge = case _ of
  FlowLeft -> riverW - 10.0
  FlowRight -> 4.0

-- | Where something of a given age sits along the time axis, in viewBox units.
xAt :: Flow -> Number -> Number
xAt flow elapsedMs = case flow of
  FlowLeft -> emitEdge FlowLeft - elapsedMs * pxPerMs
  FlowRight -> emitEdge FlowRight + elapsedMs * pxPerMs

pitchToY :: Int -> Number
pitchToY pitch = riverH * (1.0 - (toNumber (clamp 24 96 pitch) - 24.0) / 72.0)

style :: forall r i. String -> HP.IProp r i
style = HP.attr (HH.AttrName "style")

-- | The river: octave guides (HTML, so the labels aren't stretched by
-- | `preserveAspectRatio=none`), then the notes and marks in one stretched SVG.
-- | Fills its parent — give it a positioned container.
riverPanel :: forall action slots m. RiverWiring -> RiverState -> H.ComponentHTML action slots m
riverPanel w r =
  HH.div
    -- the gradient brightens toward the emit edge, so the eye is drawn to where
    -- notes are appearing rather than to where they're dying.
    [ style $ "position:absolute;inset:0;overflow:hidden;background:radial-gradient(140% 100% at "
        <> (case w.flow of
              FlowLeft -> "100%"
              FlowRight -> "0%")
        <> " 50%,#15140f,#0b0a07)" ]
    ( octaveGuides
        <>
          [ svgEl "svg"
              [ svgAttr "width" "100%", svgAttr "height" "100%"
              , svgAttr "viewBox" ("0 0 " <> show riverW <> " " <> show riverH)
              , svgAttr "preserveAspectRatio" "none"
              , style "position:absolute;inset:0" ]
              ( map (markLine w r.nowMicros) (visibleMarks r.nowMicros r.marks)
                  <> map (noteBar w r.nowMicros) r.notes )
          ]
    )

-- | Marks recent enough to still be on-screen.
visibleMarks :: Number -> Array Mark -> Array Mark
visibleMarks now = filter (\m -> (now - m.atMicros) / 1000.0 * pxPerMs < riverW)

-- | A flagged instant as a full-height gold line, placed by age like a note.
markLine :: forall action slots m. RiverWiring -> Number -> Mark -> H.ComponentHTML action slots m
markLine w now m =
  svgEl "rect"
    [ svgAttr "x" (show (xAt w.flow ((now - m.atMicros) / 1000.0)))
    , svgAttr "y" "0"
    , svgAttr "width" "1.5", svgAttr "height" (show riverH)
    , svgAttr "fill" "#e8c14a", svgAttr "opacity" "0.5"
    ] []

-- | One note: a rounded bar at its pitch, fading out over `fadeMs`.
noteBar :: forall action slots m. RiverWiring -> Number -> NoteEvent -> H.ComponentHTML action slots m
noteBar w now n =
  let elapsedMs = (now - n.fireUnixMicros) / 1000.0
  in
    svgEl "rect"
      [ svgAttr "x" (show (xAt w.flow elapsedMs))
      , svgAttr "y" (show (pitchToY n.pitch))
      , svgAttr "width" "9", svgAttr "height" "5", svgAttr "rx" "2"
      , svgAttr "fill" (w.headColor n.headIdx)
      , svgAttr "opacity" (show (max 0.12 (1.0 - elapsedMs / fadeMs)))
      ] []

-- | Faint horizontal line + a "C4"-style label at each octave C. HTML, so the text
-- | isn't distorted by the SVG's `preserveAspectRatio=none`.
octaveGuides :: forall action slots m. Array (H.ComponentHTML action slots m)
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
