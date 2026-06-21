-- | Shared view helpers used by two or more Odonus panels: the `style`
-- | attribute helper, the `engrave` style string, SVG constructors, the
-- | `panelShell` chrome, button/row primitives, the per-cell `cellChrome`,
-- | and the small formatting helpers. Held below the panel modules in the DAG.
module Triggerfish.Odonus.Grid.Widgets
  ( style
  , svgEl
  , svgAttr
  , engrave
  , clampI
  , headColor
  , roman
  , dirName
  , speedRatio
  , signed
  , octLabel
  , romanNum
  , panelShell
  , tabBtn
  , stepBtn
  , stepperRow
  , labelledRow
  , miniKnob
  , headAt
  , cellChrome
  ) where

import Prelude

import Data.Array (findIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Odonus.Grid.Types (Action(..), KnobTarget, targetRange)

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")

svgEl :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr n v = HP.attr (HH.AttrName n) v

headColor :: Int -> String
headColor h = case h `mod` 4 of
  0 -> "#2f5fb0"
  1 -> "#b0492f"
  2 -> "#2f8a5c"
  _ -> "#b07a2f"

roman :: Int -> String
roman = case _ of
  0 -> "I"
  1 -> "II"
  2 -> "III"
  _ -> "IV"

dirName :: Int -> String
dirName = case _ of
  0 -> "FWD"
  1 -> "BCK"
  _ -> "PEND"

speedRatio :: Int -> String
speedRatio ix = maybe "1.0" show (M.speedTable !! ix) <> "×"

signed :: Int -> String
signed n = if n > 0 then "+" <> show n else show n

engrave :: String
engrave = "font-family:Georgia,'Times New Roman',serif;letter-spacing:0.12em;text-transform:uppercase;color:#5a564b"

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

-- | A pale Hainbach control panel: engraved header + body, full viewport height.
panelShell
  :: forall m
   . String -> String -> String
  -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panelShell label sub widthCss body =
  HH.div
    [ style $ widthCss <> ";height:100vh;box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
        <> "padding:18px 14px;display:flex;flex-direction:column" ]
    ( [ HH.div
          [ style $ engrave <> ";font-size:11px;display:flex;justify-content:space-between;"
              <> "align-items:baseline;margin-bottom:14px;border-bottom:1px solid #00000018;padding-bottom:6px" ]
          [ HH.span [ style "font-size:14px;letter-spacing:0.16em;color:#3f3c33" ] [ HH.text label ]
          , HH.span [ style "font-size:8px" ] [ HH.text sub ]
          ]
      ] <> body )

-- | A label over a row of tab buttons (OCTAVE / SCALAR TRANSP, Xynthesizr-style).
labelledRow :: forall m. String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
labelledRow lbl btns =
  HH.div [ style "margin:8px 0" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;margin-bottom:4px" ] [ HH.text lbl ]
    , HH.div [ style "display:flex;gap:3px" ] btns
    ]

tabBtn :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
tabBtn label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:1;padding:5px 0;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:10px;color:" <> (if active then "#1c1a12" else "#3f3c33")
        <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

octLabel :: Int -> String
octLabel n = if n > 0 then "+" <> show n else show n

romanNum :: Int -> String
romanNum i = fromMaybe (show (i + 1))
  ([ "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" ] !! i)

stepperRow :: forall m. String -> String -> Action -> Action -> H.ComponentHTML Action () m
stepperRow lbl val decA incA =
  HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin:8px 0" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text lbl ]
    , HH.div [ style "display:flex;align-items:center;gap:6px" ]
        [ stepBtn "‹" decA
        , HH.span
            [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;min-width:78px;text-align:center" ]
            [ HH.text val ]
        , stepBtn "›" incA
        ]
    ]

stepBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
stepBtn glyph act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "width:22px;height:22px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "background:linear-gradient(#efece1,#ddd9cb);font-family:Georgia,serif;font-size:13px;color:#3f3c33;line-height:1" ]
    [ HH.text glyph ]

headAt :: M.Odonus -> Int -> Maybe { idx :: Int, mute :: Boolean }
headAt o i = case findIndex (\hd -> hd.cursor == i) o.heads of
  Just idx -> Just { idx, mute: maybe false _.mute (o.heads !! idx) }
  Nothing -> Nothing

-- | The per-cell chrome shared by every small multiple: pale pad +
-- | head-presence ring (bright = a playhead is here, dim = a muted one).
cellChrome :: M.Odonus -> Int -> String
cellChrome odo i =
  let
    mh = headAt odo i
    ring = maybe "#a79f86" (\r -> headColor r.idx) mh
    glow = case mh of
      Just r -> if r.mute then ",0 0 0 2px " <> ring <> "33" else ",0 0 0 3px " <> ring <> "66"
      Nothing -> ""
  in
    "background:#cbc6b6;border-radius:7px;box-shadow:0 0 0 1px " <> ring <> glow

miniKnob :: forall m. KnobTarget -> Int -> String -> String -> String -> H.ComponentHTML Action () m
miniKnob target val color topLabel valText =
  let r = targetRange target
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:46px" ]
      [ HH.span [ style $ engrave <> ";font-size:8px;margin-bottom:1px" ] [ HH.text topLabel ]
      , HH.div [ style "width:38px;height:38px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color, lo: r.lo, hi: r.hi, value: val, ticks: 0 } (KnobDown target val) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#3f3c33;margin-top:1px" ]
          [ HH.text valText ]
      ]
