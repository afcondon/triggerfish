-- | Odonus's stage controls, as a card at the top-left of the river (AC,
-- | 2026-10-05). They were a secondary nav under the shell's bar, which also
-- | carried the harmonic context; that moved to the top of the panels it
-- | describes (`Odonus.View.Key.quantPieces`), and the bar went, giving its
-- | height back to the panels.
-- |
-- | The stage tabs, then the controls shared by both stages (◆ mark, the
-- | counts, clear), then whether the heads' routes reach their ports. The card
-- | floats over whichever surface is on the left, PERFORM's river or REVIEW's.
module Triggerfish.Odonus.View.Nav (riverControls) where

import Prelude

import Data.Array (concatMap, length, null, (..))
import Data.String (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Grid.Types (Action(..), Stage(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, style)
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Web as Web
import Triggerfish.Odonus.Logbook (noteCount)

riverControls :: forall m. State -> H.ComponentHTML Action Slots m
riverControls s =
  HH.div
    [ style $ "position:absolute;top:10px;left:10px;z-index:20;display:flex;flex-direction:column;align-items:flex-start;gap:6px;"
        <> "padding:7px 9px;border-radius:9px;background:#cdc7b6ee;box-shadow:0 2px 10px #00000055;font-family:Georgia,serif" ]
    [ stageTabs s
    , HH.div [ style "display:flex;align-items:center;gap:8px" ] (captureControls s)
    , routingReadout s
    ]

-- | Where this machine's heads are going, and whether they can get there.
-- |
-- | Replaces the VOICES button: routing is edited on the dashboard now, so
-- | there is nothing here to open — but the FACT still belongs in view,
-- | because a leg pointing at an absent port produces silence, and silence reads
-- | as a musical decision. Counting the dead legs turns it into something you can
-- | see without playing a note.
routingReadout :: forall m. State -> H.ComponentHTML Action Slots m
routingReadout s =
  -- not the rig's, and no route it can reach (the web app; a browser with no
  -- Web MIDI, or none of its routes' ports): it sounds in the browser's own
  -- voice, so say that, not that routes it never takes are dead (AC,
  -- 2026-10-27: "makes it look like maybe it's not going to work")
  if s.sounding /= Rig && (null s.outs || length dead == length legs) then
      HH.span [ style "display:flex;align-items:baseline;gap:8px" ]
        ( [ HH.span
              [ HP.title "Odonus plays in the browser's own voice"
              , style $ engrave <> ";font-size:8px;letter-spacing:0.1em;color:#6a6558" ]
              [ HH.text "\x266a IN THE BROWSER" ] ]
          -- on the web, MIDI is the visitor's to choose (the browser asks them)
          <> (if not Web.onTheWeb then []
              else if null s.outs then [ toggle "PLAY THROUGH MIDI" "play through a MIDI port instead (your browser will ask)" true ]
              else [ toggle "BROWSER" "no route reaches a port here: back to the browser's voice, and stop asking MIDI" false ]) )
  else
    HH.span [ style "display:flex;align-items:baseline;gap:8px" ]
      ( [ HH.span
        [ HP.title (if broken
            then joinWith " \x00b7 " (map (\d -> RM.destLabel d.dest <> " \x2014 " <> RM.reachNote d.why) dead)
            else joinWith " \x00b7 " (map (\l -> RM.destLabel l.dest) legs))
        , style $ engrave <> ";font-size:8px;letter-spacing:0.1em;"
            <> (if broken then "color:#8c2f1c" else "color:#6a6558") ]
        [ HH.text (if broken
            then show (length dead) <> "/" <> show (length legs) <> " ROUTES DEAD"
            else show (length legs) <> " ROUTES") ] ]
        <> (if Web.onTheWeb then [ toggle "BROWSER" "back to the browser's own voice; no MIDI is sent" false ] else []) )
  where
  toggle label tip on =
    HH.button
      [ HP.title tip, HE.onClick \_ -> UseMidi on
      , style $ engrave <> ";font-size:8px;letter-spacing:0.1em;color:#3f3c33;background:#e9e4d6;border:1px solid #00000026;border-radius:4px;padding:1px 6px;cursor:pointer" ]
      [ HH.text label ]
  ports = { found: RO.portNames s.outs, rigUp: true }
  dead = concatMap (\h -> RM.unreachable ports s.routing (RM.SOdonusHead h)) (0 .. 3)
  legs = concatMap (\h -> RM.liveLegsFor s.routing (RM.SOdonusHead h)) (0 .. 3)
  broken = not (null dead)

-- | The stage tabs. Two on Odonus (Vetula's three minus HUNT — Vetula owns the
-- | harmony, so there is nothing here to hunt), styled identically so the control
-- | is recognisably the same control on both machines.
stageTabs :: forall m. State -> H.ComponentHTML Action Slots m
stageTabs s =
  HH.div
    [ style $ "display:flex;flex:0 0 auto;border:1px solid #00000026;border-radius:6px;"
        <> "overflow:hidden;box-shadow:0 1px 2px #0000001a" ]
    [ tab Perform "PERFORM" "the instrument — scope, playheads, grid, parameters"
    , tab Review "REVIEW" "the whole take — cherry-pick a phrase into the clip library"
    ]
  where
  tab target label tip =
    let active = s.stage == target
    in HH.button
        [ HP.title tip
        , HE.onClick \_ -> SetStage target
        , style $ "padding:5px 14px;border:none;cursor:pointer;font-family:Georgia,serif;font-size:11px;letter-spacing:0.12em;"
            <> (if active then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12;font-weight:600"
                          else "background:linear-gradient(#e9e5d9,#dcd8c9);color:#5a564b") ]
        [ HH.text label ]

-- | ◆ mark and the running counts — the stage controls, shared by PERFORM and
-- | REVIEW and deliberately identical in both. There is no arm: the rig is always
-- | capturing, so marking is the only gesture. The counts double as the "yes, it
-- | is recording" confirmation that the scope's dot used to give.
captureControls :: forall m. State -> Array (H.ComponentHTML Action Slots m)
captureControls s =
  [ HH.button
      [ HE.onClick \_ -> MarkNow
      , HP.title "flag this instant as a good bit (works in either stage)"
      , style $ "display:flex;align-items:center;gap:7px;padding:3px 12px;border-radius:6px;cursor:pointer;"
          <> "border:1px solid #8a7a3a55;background:#e8c14a1a;color:#6a5820;font-size:11px;white-space:nowrap" ]
      [ HH.text "◆ mark" ]
  , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#6a6558;white-space:nowrap" ]
      [ HH.text (show (noteCount s.logbook) <> " notes · " <> show (length s.logbook.marks) <> " ◆") ]
  , HH.button
      [ HE.onClick \_ -> ClearLog
      , HP.title "clear the Review surface: its notes, marks and loops (on the rig too: odonus $ clear)"
      , style $ "padding:3px 10px;border-radius:6px;cursor:pointer;border:1px solid #00000018;"
          <> "background:transparent;color:#8a8576;font-size:10px;white-space:nowrap" ]
      [ HH.text "clear" ]
  ]
