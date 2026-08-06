-- | Triggerfish.Balistes.View.Trig — the TIDAL (POLYTRIG) tab: the lane-spanning
-- | ROUTES editor (middle column) and the eight named jacks as a grid (pattern
-- | column). Relocated from Selene — every jack lands on the drums MIDI channel;
-- | CV/gate targets stay on Selene. Pure renderers over `State`.
module Triggerfish.Balistes.View.Trig
  ( routeStrip
  , trigJacks
  ) where

import Prelude

import Data.Array (mapWithIndex, range, (!!))
import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Grid.Widgets (style)
import Triggerfish.Tidal.Lane as Lane
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Types (Action(..), State)
import Triggerfish.Balistes.Widgets (stepBtn)

-- The middle column for the SELENE DRUMS tab: the lane-spanning ROUTES editor
-- (each route is a mini-notation string whose atoms fire jacks by name), plus a
-- short legend. Jacks live in the PATTERN column to the right.
-- The routes, as a HORIZONTAL strip riding in the TIDAL band header. Was a 260px
-- ROUTES column whose top third was a paragraph explaining what a route is; the
-- explanation is in the ⓘ help now and the fields are where the jacks are.
routeStrip :: forall m. State -> H.ComponentHTML Action () m
routeStrip s =
  HH.div [ style "display:flex;align-items:center;gap:6px;flex-wrap:wrap" ]
    ( mapWithIndex routeLine s.trig.routes
        <> [ HH.button
               [ HE.onClick \_ -> AddRoute
               , style $ "padding:3px 10px;border:1px dashed #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:Georgia,serif;font-size:10px;color:#6a6657;background:#00000006" ]
               [ HH.text "+ route" ] ] )

-- One editable route line: a text field + a remove button.
routeLine :: forall m. Int -> String -> H.ComponentHTML Action () m
routeLine i src =
  HH.div [ style "display:flex;align-items:center;gap:6px" ]
    [ HH.input
        [ HP.value src
        , HP.placeholder "bd sn cp sn"
        , HE.onValueInput (SetRoute i)
        , style $ "flex:1 1 auto;min-width:0;padding:5px 8px;border:1px solid #a8a392;border-radius:5px;"
            <> "background:#f3f1e8;font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#1c1a12" ]
    , HH.button
        [ HE.onClick \_ -> RemoveRoute i
        , style $ "flex:0 0 auto;width:22px;height:26px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:12px;color:#8a3120;background:#efece1" ]
        [ HH.text "×" ]
    ]

-- The eight named jacks, FOUR across and two down (was two across and four down,
-- which needed a tall narrow column). Each card keeps its width; they just
-- re-flow, so the band is ~170px tall instead of ~390.
trigJacks :: forall m. State -> H.ComponentHTML Action () m
trigJacks s =
  HH.div
    [ style "display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:8px;width:100%" ]
    (mapWithIndex trigJackCell s.trig.jacks)

-- One POLYTRIG jack: name + note steppers on top, a source input, then a step
-- figure following the source's meter (faint "↳ route" when the source is empty).
trigJackCell :: forall m. Int -> M.TrigSlot -> H.ComponentHTML Action () m
trigJackCell i sl =
  HH.div
    [ style $ "padding:8px 9px;border-radius:7px;background:#ffffff55;border:1px solid #00000012;"
        <> "display:flex;flex-direction:column;gap:6px;min-width:0" ]
    [ HH.div [ style "display:flex;align-items:center;gap:6px" ]
        [ HH.input
            [ HP.value sl.name
            , HE.onValueInput (SetJackName i)
            , style $ "flex:1 1 auto;min-width:0;padding:3px 6px;border:1px solid #a8a392;border-radius:4px;"
                <> "background:#f3f1e8;font-family:Georgia,serif;font-size:12px;color:#1c1a12" ]
        , stepBtn "−" (SetJackNote i (-1))
        , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;width:30px;text-align:center" ]
            [ HH.text ("♪" <> show sl.note) ]
        , stepBtn "+" (SetJackNote i 1)
        ]
    , HH.input
        [ HP.value sl.source
        , HP.placeholder "(routed)"
        , HE.onValueInput (SetJackSource i)
        , style $ "padding:4px 7px;border:1px solid #a8a392;border-radius:4px;background:#f3f1e8;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#1c1a12;width:100%;box-sizing:border-box" ]
    , trigStepFigure sl.source
    ]

-- A linear step row lit at the source's onset cells (HTML so it fills width).
-- Adapted from Selene's stepFigure; the trig accent is a steel-blue.
trigStepFigure :: forall m. String -> H.ComponentHTML Action () m
trigStepFigure src =
  let
    m = Lane.meterOf src
    mask = Lane.cellMaskOf src
    trigAccent = "#3f6f8a"
    stepDiv k =
      let on = fromMaybe false (mask !! k)
      in
        HH.div
          [ style $ "flex:1 1 0;min-width:0;height:14px;border-radius:2px;"
              <> (if on then "background:" <> trigAccent
                  else "background:#00000008;border:1px solid " <> trigAccent <> "44;box-sizing:border-box") ]
          []
  in
    HH.div [ style "display:flex;gap:2px;width:100%;height:14px;align-items:center" ]
      (map stepDiv (range 0 (m - 1)))
