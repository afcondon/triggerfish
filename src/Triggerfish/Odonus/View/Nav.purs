-- | Odonus's SECONDARY NAV — the thin bar under the shell's machine switcher.
-- |
-- | It carries the SAME shape as Vetula's (`Vetula.App.contextBar`), because the
-- | two machines answer the same three questions and a player shouldn't have to
-- | learn two layouts (AC, 2026-08-06):
-- |
-- |   LEFT   the stage tabs, hard left under the shell's transport, then ONLY
-- |          the controls that mean something in the stage you're in.
-- |   RIGHT  housekeeping (scenes), then the harmonic context — which sits in the
-- |          same column as the shell's pitch set above it and Vetula's key/scale
-- |          on Vetula's own bar, because all three show the same thing at
-- |          different removes: Vetula sets it, the shell states it rig-wide,
-- |          Odonus reports the slice it's quantising to.
-- |
-- | Two groups never move (tabs, harmonic context); only the middle-left changes
-- | with the stage. That's the whole locality argument, and it's why ◆ mark is
-- | HERE rather than on the surface: it belongs to both stages, so putting it on
-- | either surface would move it under you when you switched to look at what you
-- | just marked. (It used to be in two places at once — this nav *and* the
-- | scope's `● logging` overlay, with the same counts. One home now.)
module Triggerfish.Odonus.View.Nav (navBar) where

import Prelude

import Data.Array (length, null)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Grid.Types (Action(..), Stage(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (style)
import Triggerfish.Odonus.Logbook (noteCount)
import Triggerfish.Odonus.View.Key (contextStrip)
import Triggerfish.Odonus.View.Scenes (sceneMenuBody)

navBar :: forall m. State -> H.ComponentHTML Action Slots m
navBar s =
  HH.div
    [ style $ "position:relative;z-index:20;display:flex;align-items:center;gap:12px;"
        <> "padding:5px 14px;border-bottom:1px solid #00000014;"
        <> "background:linear-gradient(#cdc7b6,#c4bead);font-family:Georgia,serif" ]
    ( [ stageTabs s ]
        <> captureControls s
        <> [ HH.div [ style "flex:1 1 auto;min-width:8px" ] []
           , sceneMenu s
           , divider
           , contextStrip s
           ]
    )

divider :: forall m. H.ComponentHTML Action Slots m
divider = HH.div [ style "width:1px;height:20px;background:#00000018" ] []

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
  ]

-- | Scenes behind a menu: capture and recall are housekeeping, so they shouldn't
-- | hold width open all the time.
sceneMenu :: forall m. State -> H.ComponentHTML Action Slots m
sceneMenu s =
  HH.div [ style "position:relative" ]
    ( [ HH.button
          [ HE.onClick \_ -> ToggleSceneMenu
          , HP.title "capture and recall named settings"
          , style $ "padding:3px 11px;border-radius:6px;cursor:pointer;border:1px solid #00000022;font-size:11px;white-space:nowrap;"
              <> (if s.navScenes then "background:#3a352a;color:#f2eee2" else "background:#efece1;color:#3f3c33") ]
          [ HH.text ("scenes" <> (if null s.scenes then "" else " · " <> show (length s.scenes))) ]
      ]
        <> (if s.navScenes then [ menuPanel ] else [])
    )
  where
  menuPanel =
    HH.div
      [ style $ "position:absolute;top:calc(100% + 6px);right:0;z-index:30;width:300px;max-height:60vh;overflow-y:auto;"
          <> "padding:12px 14px;border-radius:9px;border:1px solid #a8a392;"
          <> "background:linear-gradient(#f6f2e8,#efe9db);box-shadow:0 14px 40px #00000044" ]
      -- no header of our own: `sceneMenuBody` opens with its own SCENES · SONG rule.
      (sceneMenuBody s)
