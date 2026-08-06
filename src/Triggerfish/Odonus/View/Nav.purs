-- | Odonus's SECONDARY NAV — the thin bar under the shell's machine switcher, the
-- | same idea Vetula has had (AC, 2026-08-06). It exists because the right-hand KEY
-- | column had become a dumping ground: a read-only harmonic readout, the scene
-- | machinery, and a logbook counter, together eating a fifth of the width while
-- | PARAMETERS beside it needed a scrollbar to reach LEN.
-- |
-- | What lives here and why:
-- |
-- |   * **harmonic context** — a readout, not a control, so it belongs in the chrome
-- |     rather than in a panel. A live player wants to glance at it.
-- |   * **scenes** — capture/recall is session housekeeping, not part of the
-- |     instrument; behind a menu so it costs no width until you want it.
-- |   * **◆ mark** and the **full-surface toggle** — the two capture gestures, side
-- |     by side. Mark works in either view; the toggle only changes how much room
-- |     the capture surface gets. Neither touches the transport.
module Triggerfish.Odonus.View.Nav (navBar) where

import Prelude

import Data.Array (length, null)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Grid.Types (Action(..), OdonusView(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (style)
import Triggerfish.Odonus.Logbook (noteCount)
import Triggerfish.Odonus.View.Key (contextStrip)
import Triggerfish.Odonus.View.Scenes (sceneMenuBody)

-- | The bar. Context on the left (it's what you read), gestures on the right (what
-- | you press), scenes tucked behind a menu between them.
navBar :: forall m. State -> H.ComponentHTML Action Slots m
navBar s =
  HH.div
    [ style $ "position:relative;z-index:20;display:flex;align-items:center;gap:16px;"
        <> "padding:5px 14px;border-bottom:1px solid #00000014;"
        <> "background:linear-gradient(#cdc7b6,#c4bead);font-family:Georgia,serif" ]
    [ contextStrip s
    , HH.div [ style "flex:1 1 auto" ] []
    , sceneMenu s
    , markBtn s
    , viewToggle s
    ]

-- | ◆ mark — flag the current instant on the capture roll. Deliberately available
-- | in BOTH views: the whole reason to sit in the instrument is to catch a good bit
-- | as it goes past, and having to expand the surface first would lose it.
markBtn :: forall m. State -> H.ComponentHTML Action Slots m
markBtn s =
  HH.button
    [ HE.onClick \_ -> MarkNow
    , HP.title "flag this instant as a good bit (works in either view)"
    , style $ "display:flex;align-items:center;gap:7px;padding:3px 11px;border-radius:6px;cursor:pointer;"
        <> "border:1px solid #8a7a3a55;background:#e8c14a1a;color:#6a5820;font-size:11px;white-space:nowrap" ]
    [ HH.text "◆ mark"
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;opacity:0.65" ]
        [ HH.text (show (noteCount s.logbook) <> "n · " <> show (length s.logbook.marks) <> "◆") ]
    ]

-- | The surface-size toggle. NOT a transport control — the generator runs either
-- | way — so it reads as one button that expands and collapses, rather than two
-- | mode tabs implying you've left something behind.
viewToggle :: forall m. State -> H.ComponentHTML Action Slots m
viewToggle s =
  let full = s.view == VFull
  in HH.button
      [ HE.onClick \_ -> SetView (if full then VPanels else VFull)
      , HP.title (if full then "back to the instrument (the roll keeps capturing)"
                          else "give the capture surface the whole window (the instrument keeps playing)")
      , style $ "padding:3px 11px;border-radius:6px;cursor:pointer;border:1px solid #00000022;font-size:11px;white-space:nowrap;"
          <> (if full then "background:#3a352a;color:#f2eee2" else "background:#efece1;color:#3f3c33") ]
      [ HH.text (if full then "⛶ full ✓" else "⛶ full") ]

-- | Scenes behind a menu: the button, and the panel that drops out of it. Capture
-- | and recall are housekeeping — they shouldn't hold width open all the time.
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
