-- | The VOICES modal (docs/kb/plans/odonus-voices.md): a Harmonia voicing and a
-- | rule per head, behind the bar's VOICES chip so the simple SPREAD stays the
-- | face of the page. Every edit is the strategy's text (`Reef.Odonus.Voices`)
-- | sent as one `SetVoices` input, so the page and the rig parse the same words.
module Triggerfish.Odonus.View.Voices (voicesModal, voicesEdit) where

import Prelude

import Data.Array (mapWithIndex, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Reef.Odonus (Odonus, renderHead)
import Reef.Odonus.Voices (Rule(..), Voices, printVoices, ruleWire, rulesOf, voicingIx, voicingKinds, voicingName, voicingOf)
import Reef.Scale (rootName)
import Triggerfish.Odonus.Grid.Types (Action(..), Slots)
import Triggerfish.Odonus.Grid.Widgets (engrave, headColor, roman, style)

-- | The strategy shown: the live one, or every head on its melody.
current :: Odonus -> Voices
current odo = fromMaybe { voicing: 0, rules: [] } odo.voices

-- | One edit to the shown strategy, as the text to send.
voicesEdit :: Odonus -> (Voices -> Voices) -> String
voicesEdit odo f = printVoices (f (current odo))

setRule :: Int -> Rule -> Voices -> Voices
setRule i r v =
  let rules = map ruleWire (rulesOf v)
  in v { rules = fromMaybe rules (updateAt i (ruleWire r) rules) }

voicesModal :: forall m. Odonus -> H.ComponentHTML Action Slots m
voicesModal odo =
  let
    v = current odo
    on = case odo.voices of
      Just _ -> true
      Nothing -> false
    kind = voicingOf v.voicing
    send f = SetVoicesText (voicesEdit odo f)
  in
    HH.div [ style "position:fixed;inset:0;z-index:50;display:flex;align-items:center;justify-content:center" ]
      [ HH.div
          [ HE.onClick \_ -> ToggleVoicesModal
          , style "position:absolute;inset:0;background:rgba(30,28,22,0.28)" ]
          []
      , HH.div
          [ style $ "position:relative;width:440px;max-width:calc(100vw - 32px);padding:16px 18px;border-radius:10px;"
              <> "background:#e9e5d8;border:1px solid #a8a392;box-shadow:0 10px 30px rgba(0,0,0,0.25);font-family:Georgia,serif;color:#3f3c33" ]
          [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;margin-bottom:4px" ]
              [ HH.span [ style $ engrave <> ";font-size:12px" ] [ HH.text "VOICES" ]
              , HH.span [ style "font-size:11px;opacity:0.7" ]
                  [ HH.text (if on then "on" else "off: every head plays its own melody") ]
              ]
          , HH.div [ style "font-size:11px;opacity:0.75;margin-bottom:12px;line-height:1.35" ]
              [ HH.text "Head I's tune, harmonised: a voicing of the sounding chord (or of the scale on head I's note), and what each head plays from it." ]
          , label "VOICING"
          , HH.div [ style "display:flex;gap:4px;margin-bottom:12px" ]
              (map (\k -> pill (voicingName k) (on && k == kind) (send _ { voicing = voicingIx k })) voicingKinds)
          , label "HEADS"
          , HH.div [ style "display:flex;flex-direction:column;gap:6px;margin-bottom:14px" ]
              (mapWithIndex (headRow odo send) (rulesOf v))
          , HH.div [ style "display:flex;justify-content:space-between;align-items:center" ]
              [ pill "OFF" false (SetVoicesText "off")
              , pill "DONE" false ToggleVoicesModal
              ]
          ]
      ]
  where
  label t = HH.div [ style $ engrave <> ";font-size:8px;opacity:0.8;margin-bottom:4px" ] [ HH.text t ]

headRow :: forall m. Odonus -> ((Voices -> Voices) -> Action) -> Int -> Rule -> H.ComponentHTML Action Slots m
headRow odo send i rule =
  HH.div [ style "display:flex;align-items:center;gap:6px" ]
    [ HH.span [ style $ "width:26px;font-weight:bold;font-size:13px;color:" <> headColor i ] [ HH.text (roman i) ]
    , HH.div [ style "display:flex;gap:3px;flex:0 0 auto" ]
        [ pill "melody" (rule == Melody) (send (setRule i Melody))
        , pill "voice" (isVoice rule) (send (setRule i (Voice (numberOf rule 1))))
        , pill "lead" (isLead rule) (send (setRule i (Lead (numberOf rule 2))))
        ]
    , HH.div [ style "display:flex;align-items:center;gap:3px;width:92px;justify-content:center" ] case rule of
        Voice k -> stepper (show k) (send (setRule i (Voice (max 1 (k - 1))))) (send (setRule i (Voice (min 8 (k + 1)))))
        Lead n -> stepper (signed n) (send (setRule i (Lead (max (-14) (n - 1))))) (send (setRule i (Lead (min 14 (n + 1)))))
        Melody -> []
    , HH.span [ style "flex:1;text-align:right;font-family:'SF Mono',Menlo,monospace;font-size:11px" ]
        [ HH.text (nowPlaying odo i) ]
    ]
  where
  isVoice = case _ of
    Voice _ -> true
    _ -> false
  isLead = case _ of
    Lead _ -> true
    _ -> false
  numberOf r d = case r of
    Voice k -> k
    Lead n -> n
    Melody -> d
  signed n = if n > 0 then "+" <> show n else show n

-- | The note a head would play now, at its current cell.
nowPlaying :: Odonus -> Int -> String
nowPlaying odo i = case odo.heads !! i of
  Just hd | Just c <- odo.cells !! hd.cursor ->
    let p = renderHead odo i hd c in rootName (((p `mod` 12) + 12) `mod` 12) <> show (p / 12 - 1)
  _ -> ""

stepper :: forall m. String -> Action -> Action -> Array (H.ComponentHTML Action Slots m)
stepper txt down up =
  [ pill "−" false down
  , HH.span [ style "min-width:24px;text-align:center;font-family:'SF Mono',Menlo,monospace;font-size:12px" ] [ HH.text txt ]
  , pill "+" false up
  ]

pill :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action Slots m
pill txt lit act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:3px 7px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;font-family:Georgia,serif;font-size:11px;"
        <> (if lit then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12"
                   else "background:linear-gradient(#efece1,#ddd9cb);color:#3f3c33") ]
    [ HH.text txt ]
