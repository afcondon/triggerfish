-- | **The sentence: the scene in words**, as reef's `Reef.Conspicillum.Sentence`
-- | says it. Every number is a `Value`: drawn with the catalogue's own
-- | readout, draggable, and marked with its knob's number when a knob is on it.
-- | The rules follow as a table.
module Conspicillum.View.Sentence (sentence) where

import Prelude

import Data.Array (elemIndex, null)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Reef.Conspicillum.Cloud (Spec)
import Reef.Conspicillum.Parameter (Parameter, format, read)
import Reef.Conspicillum.Sentence (Fragment(..), RuleRow, Sentence)
import Web.Event.Event (Event, EventType(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

sentence
  :: forall w i
   . { said :: Array Sentence
     , rows :: Array RuleRow
     , spec :: Spec
     , knobs :: Array Parameter
     , highlighted :: Maybe Parameter
     , problem :: Maybe String
     , grab :: Parameter -> Event -> i
     , hover :: Maybe Parameter -> i
     , nudge :: Parameter -> KeyboardEvent -> i
     }
  -> HH.HTML w i
sentence r =
  HH.div [ HP.class_ (HH.ClassName "sentence") ]
    ( (r.said <#> \s -> HH.p_ (s <#> fragment))
        <> (if null r.rows then [] else [ rules ])
        <> case r.problem of
          Just p -> [ HH.p [ HP.class_ (HH.ClassName "note") ] [ HH.text ("The engine refused this scene: " <> p) ] ]
          Nothing -> []
    )
  where
  fragment = case _ of
    Words w -> HH.text w
    Value p -> value p
    SetName n -> HH.span [ HP.class_ (HH.ClassName "set") ] [ HH.text n ]
    Code c -> HH.code_ [ HH.text c ]
    Aside a -> HH.span [ HP.class_ (HH.ClassName "note") ] [ HH.text a ]

  value p =
    let
      knob = elemIndex p r.knobs
      classes = "num" <> (if knob /= Nothing then " bound" else "") <> (if r.highlighted == Just p then " hl" else "")
    in
      HH.span_
        ( [ HH.span
              [ HP.class_ (HH.ClassName classes)
              , HP.tabIndex 0
              , HE.handler (EventType "pointerdown") (r.grab p)
              , HE.onMouseEnter \_ -> r.hover (Just p)
              , HE.onMouseLeave \_ -> r.hover Nothing
              , HE.onKeyDown (r.nudge p)
              ]
              [ HH.text (format p (read p r.spec)) ]
          ]
            <> case knob of
              Just k -> [ HH.sup_ [ HH.text (show (k + 1)) ] ]
              Nothing -> []
        )

  rules =
    HH.table [ HP.class_ (HH.ClassName "rules") ]
      ( [ HH.tr_ [ HH.th_ [ HH.text "which grains" ], HH.th_ [ HH.text "what" ], HH.th_ [ HH.text "by" ] ] ]
          <> (r.rows <#> \row -> HH.tr_
                [ HH.td_ [ HH.text row.which ]
                , HH.td [ HP.class_ (HH.ClassName "op") ] [ HH.text row.what ]
                , HH.td [ HP.class_ (HH.ClassName "amt") ] [ HH.text row.amount ]
                ])
      )
