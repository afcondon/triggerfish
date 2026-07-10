-- | SCENES section — save whole settings, then sequence them into a song. Since
-- | the KEY/SCENES pane merge (#139) this is a *body* (an array of rows) stacked
-- | below the KEY controls in one column, not its own `panelShell` — so it leads
-- | with its own divider + section label instead of a panel header.
module Triggerfish.Odonus.View.Scenes (scenesBody, sceneName) where

import Prelude

import Data.Array (length, mapWithIndex, null)
import Data.Int (round)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action(..), Scene, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, stepBtn, style)

-- | Auto-name a captured scene by its position + its scale.
sceneName :: State -> String
sceneName s = show (length s.scenes + 1) <> " · " <> Scale.scaleName (M.scaleOf s.odo)

-- | RIG cluster at the top of Scenes: the rig/runtime one-shots (Push the whole
-- | patch to the BEAM, Hush the reef voice, Pin the shared seed for reproducible
-- | runs) grouped away from the per-song scene controls below. Pin seed shows the
-- | live seed; it rides the reef-sim handoff so pin-then-Push starts both runtimes
-- | identically.
rigCluster :: forall m. State -> H.ComponentHTML Action Slots m
rigCluster s =
  HH.div [ style "margin-bottom:11px;padding-bottom:10px;border-bottom:1px solid #00000014" ]
    [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.85;display:block;margin-bottom:5px" ]
        [ HH.text "RIG" ]
    -- Control-surface Phase 2/refinement: no manual push OR hush here — in ATLANTIS
    -- the shell hands off automatically and edits stream live; the global "Hush rig"
    -- lives in the top nav (shown only in ATLANTIS). Only the shared seed stays.
    , HH.div [ style "display:flex;align-items:center;gap:6px" ]
        [ HH.button
            [ HE.onClick \_ -> ReseedTo 1
            , style $ "flex:0 0 auto;padding:4px 8px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
                <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:10px;color:#3f3c33" ]
            [ HH.text "⚑ Pin seed" ]
        , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:8px;color:#5a564b" ]
            [ HH.text ("seed " <> show (round s.genSeed)) ]
        ]
    ]

-- | The SCENES rows, to stack below KEY in the merged pane. Leads with a section
-- | divider so it reads as its own block within the shared column.
scenesBody :: forall m. State -> Array (H.ComponentHTML Action Slots m)
scenesBody s =
    [ sceneDivider
    , rigCluster s
    , HH.input
        [ HP.value s.sceneNameInput
        , HE.onValueInput SetSceneName
        , HP.placeholder "name this setting…"
        , style $ "width:100%;box-sizing:border-box;padding:6px 8px;margin-bottom:6px;border:1px solid #a8a392;"
            <> "border-radius:6px;background:#f4f1e8;font-family:Georgia,serif;font-size:11px;color:#1c1a12" ]
    , HH.button
        [ HE.onClick \_ -> CaptureScene
        , style $ "width:100%;padding:7px;margin-bottom:10px;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
            <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:12px;color:#3f3c33" ]
        [ HH.text "＋ Capture current" ]
    , HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:6px" ]
        [ HH.button
            [ HE.onClick \_ -> ToggleChain
            , style $ "padding:5px 10px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;font-family:Georgia,serif;font-size:11px;color:#3f3c33;background:"
                <> (if s.chain then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
            [ HH.text (if s.chain then "■ Chain" else "▶ Chain") ]
        , HH.div [ style "display:flex;align-items:center;gap:5px" ]
            [ stepBtn "‹" (BumpBars (-1))
            , HH.span [ style $ engrave <> ";font-size:9px;min-width:48px;text-align:center" ]
                [ HH.text (show s.barsPerScene <> " bar" <> (if s.barsPerScene == 1 then "" else "s")) ]
            , stepBtn "›" (BumpBars 1)
            ]
        ]
    , HH.div [ style "display:flex;flex-direction:column;gap:5px;margin-top:8px" ]
        ( if null s.scenes
            then [ HH.div [ style $ engrave <> ";font-size:8px;color:#888273;margin-top:6px" ]
                     [ HH.text "capture a few settings, then chain them" ] ]
            else mapWithIndex (sceneChip s) s.scenes )
    ]

-- | The divider + "SCENES" label that opens the scenes block within the merged
-- | KEY pane (the visual seam where KEY's pitch controls end and the song
-- | machinery begins).
sceneDivider :: forall m. H.ComponentHTML Action Slots m
sceneDivider =
  HH.div
    [ style $ "margin:14px 0 10px;padding-top:11px;border-top:1px solid #00000022;"
        <> "display:flex;align-items:baseline;justify-content:space-between" ]
    [ HH.span [ style $ engrave <> ";font-size:14px;letter-spacing:0.16em;color:#3f3c33" ] [ HH.text "SCENES" ]
    , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ] [ HH.text "Song" ]
    ]

sceneChip :: forall m. State -> Int -> Scene -> H.ComponentHTML Action Slots m
sceneChip s i sc =
  let active = s.chain && s.sceneIx == i
  in
    HH.div
      [ style $ "display:flex;align-items:center;gap:6px;padding:6px 8px;border-radius:7px;cursor:pointer;"
          <> "background:#cbc6b6;box-shadow:0 0 0 1px " <> (if active then "#b5832b" else "#00000018")
          <> (if active then ";outline:2px solid #b5832b66" else "") ]
      [ HH.div
          [ HE.onClick \_ -> RecallScene i
          , HP.title "recall the whole scene — its own key/progression too"
          , style "flex:1;font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33" ]
          [ HH.text sc.name ]
      , HH.span
          [ HE.onClick \_ -> RecallGesture i
          , HP.title "recall the gesture in the CURRENT key — same lick, live harmony"
          , style $ "font-family:Georgia,serif;font-size:9px;letter-spacing:0.04em;color:#5a7a58;"
              <> "padding:1px 5px;border-radius:5px;border:1px solid #5a7a5844;background:#5a7a5814" ]
          [ HH.text "in key" ]
      , HH.span
          [ HE.onClick \_ -> DeleteScene i
          , style "font-family:Georgia,serif;font-size:11px;color:#a06048;padding:0 3px" ]
          [ HH.text "×" ]
      ]
