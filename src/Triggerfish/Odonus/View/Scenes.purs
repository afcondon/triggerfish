-- | SCENES panel — save whole settings, then sequence them into a song.
module Triggerfish.Odonus.View.Scenes (scenesPanel, sceneName) where

import Prelude

import Data.Array (length, mapWithIndex, null)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action(..), Scene, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, panelShell, stepBtn, style)

-- | Auto-name a captured scene by its position + its scale.
sceneName :: State -> String
sceneName s = show (length s.scenes + 1) <> " · " <> Scale.scaleName (M.scaleOf s.odo)

scenesPanel :: forall m. State -> H.ComponentHTML Action () m
scenesPanel s =
  panelShell s.collapsed "SCENES" "Song" "flex:0 1 198px;min-width:min-content"
    [ HH.button
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

sceneChip :: forall m. State -> Int -> Scene -> H.ComponentHTML Action () m
sceneChip s i sc =
  let active = s.chain && s.sceneIx == i
  in
    HH.div
      [ style $ "display:flex;align-items:center;gap:6px;padding:6px 8px;border-radius:7px;cursor:pointer;"
          <> "background:#cbc6b6;box-shadow:0 0 0 1px " <> (if active then "#b5832b" else "#00000018")
          <> (if active then ";outline:2px solid #b5832b66" else "") ]
      [ HH.div
          [ HE.onClick \_ -> RecallScene i
          , style "flex:1;font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33" ]
          [ HH.text sc.name ]
      , HH.span
          [ HE.onClick \_ -> DeleteScene i
          , style "font-family:Georgia,serif;font-size:11px;color:#a06048;padding:0 3px" ]
          [ HH.text "×" ]
      ]
