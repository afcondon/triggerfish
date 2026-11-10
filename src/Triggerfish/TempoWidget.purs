-- | **The tempo, the same on every page** (AC, 2026-11-10): − the number +,
-- | "bpm", the beats of the bar lit in turn, and whose tempo it is: Link's,
-- | through Diaphus, or the free run every page keeps (`Triggerfish.Tempo`).
-- |
-- | Drawn by the machines' shell in its bar and by the dashboard, from a
-- | `Reading` taken off the page's own clock. Without `Handlers` it is read
-- | only, for a page that never sets the tempo. The Link peer count is not
-- | here: it is the dashboard's alone.
module Triggerfish.TempoWidget
  ( Reading
  , Handlers
  , reading
  , view
  ) where

import Prelude

import Binnacle.Clock (ClockReading)
import Data.Array ((..))
import Data.Int (floor, round)
import Data.Maybe (Maybe(..), isNothing)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Tempo as Tempo
import Triggerfish.Ui.Style (style)

-- | What the widget draws: the tempo, whether Link sets it, which beat of
-- | the bar it is and how many beats the bar has. Kept to what is drawn, so
-- | a page writes it to state only when one of these changes.
type Reading = { tempo :: Number, locked :: Boolean, beat :: Int, beats :: Int }

-- | What a press or a typed tempo becomes on the page that draws it.
type Handlers i = { bump :: Number -> i, set :: String -> i }

-- | From the clock: Link's tempo when locked, else the free tempo the page
-- | keeps (the clock's own free run may not have heard of it yet).
reading :: Number -> ClockReading -> Reading
reading free r =
  let
    beats = clamp 1 8 (round r.quantum)
  in
    { tempo: Tempo.clampTempo (if r.locked then r.tempo else free)
    , locked: r.locked
    , beat: clamp 0 (beats - 1) (floor r.phase)
    , beats
    }

view :: forall w i. Maybe (Handlers i) -> Reading -> HH.HTML w i
view handlers r =
  HH.span
    [ HP.title (if isNothing handlers then "the rig's tempo" else Tempo.hotkeyHelp)
    , style "display:inline-flex;align-items:center;gap:5px;flex:0 0 auto;font-family:'SF Mono',Menlo,monospace;color:#5a564b"
    ]
    ( number <> [ HH.span [ style "font-size:10px;letter-spacing:0.06em" ] [ HH.text "bpm" ], dots, source ] )
  where
  number = case handlers of
    Nothing -> [ HH.span [ style numStyle ] [ HH.text (Tempo.showTempo r.tempo) ] ]
    Just h ->
      [ step h "−" (-1.0) "tempo −1 (⌥−; with ⇧, −5)"
      , HH.input
          [ HP.type_ HP.InputNumber
          , HP.attr (HH.AttrName "step") "1", HP.attr (HH.AttrName "min") "20", HP.attr (HH.AttrName "max") "300"
          , HP.value (Tempo.showTempo r.tempo)
          , HE.onValueChange h.set
          , HP.attr (HH.AttrName "aria-label") "Tempo, beats a minute"
          , HP.class_ (HH.ClassName "tf-tempo-num")
          , style (numStyle <> "width:3.3em;border:none;border-bottom:1px solid #00000030;background:transparent;padding:0 2px;outline:none")
          ]
      , step h "+" 1.0 "tempo +1 (⌥=; with ⇧, +5)"
      ]
  numStyle = "font-size:15px;color:#1c1a12;font-variant-numeric:tabular-nums;text-align:right;"
  step h label d tip =
    HH.button
      [ HE.onClick \_ -> h.bump d, HP.title tip
      , style "width:20px;height:20px;padding:0;border:1px solid #00000026;border-radius:4px;background:transparent;color:#1c1a12;cursor:pointer;font:13px/1 'SF Mono',Menlo,monospace"
      ]
      [ HH.text label ]
  -- the beats of the bar, the one now lit; the downbeat a little larger
  dots =
    HH.span [ style "display:inline-flex;align-items:center;gap:3px;margin-left:4px", HP.attr (HH.AttrName "aria-hidden") "true" ]
      ( (0 .. (r.beats - 1)) <#> \i ->
          let
            size = if i == 0 then 7 else 5
            fill
              | i == r.beat = if r.locked then "#3f8f5a" else "#1c1a12"
              | otherwise = "#00000022"
          in
            HH.span [ style ("display:inline-block;border-radius:50%;width:" <> show size <> "px;height:" <> show size <> "px;background:" <> fill) ] []
      )
  source =
    HH.span
      [ HP.title
          ( if r.locked then "Locked to Link: Diaphus's anchor sets the beat."
            else "Free-running: no Link anchor has reached this page; it keeps its own time."
          )
      , style ("font-size:10px;letter-spacing:0.06em;" <> if r.locked then "color:#3f8f5a" else "color:#8a8576")
      ]
      [ HH.text (if r.locked then "Link" else "free") ]

