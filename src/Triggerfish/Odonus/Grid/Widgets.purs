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
  , genRow
  ) where

import Prelude

import Data.Array (elem, find, findIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Model as M
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Odonus.Grid.Types
  ( Action(..), KnobTarget(..), State, GenKind, targetRange, genLabel, genSub, periodOf )

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
-- | A pale Hainbach control panel. Collapsible: its header is the toggle
-- | (click to fold the panel to a thin tab); when its label is in `collapsed`
-- | it renders as that tab instead, freeing its width for the open panels.
panelShell
  :: forall m
   . Array String -> String -> String -> String
  -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panelShell collapsed label sub widthCss body =
  if elem label collapsed then panelTab label
  else
    HH.div
      [ style $ widthCss <> ";height:calc(100vh - var(--tf-bar));box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
          <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
          <> "padding:18px 14px;display:flex;flex-direction:column" ]
      ( [ HH.div
            [ HE.onClick \_ -> CollapsePanel label
            , style $ engrave <> ";font-size:11px;display:flex;justify-content:space-between;"
                <> "align-items:baseline;margin-bottom:14px;border-bottom:1px solid #00000018;"
                <> "padding-bottom:6px;cursor:pointer;user-select:none" ]
            [ HH.span [ style "font-size:14px;letter-spacing:0.16em;color:#3f3c33" ] [ HH.text label ]
            , HH.span [ style "font-size:8px;display:flex;gap:7px;align-items:baseline" ]
                [ HH.span_ [ HH.text sub ]
                , HH.span [ style "opacity:0.45;font-size:11px" ] [ HH.text "–" ]
                ]
            ]
        ] <> body )

-- | A collapsed panel: a thin full-height tab with the rotated label; click to
-- | reopen. The freed width flows to the open panels and the scope.
panelTab :: forall m. String -> H.ComponentHTML Action () m
panelTab label =
  HH.div
    [ HE.onClick \_ -> ExpandPanel label
    , style $ "flex:0 0 30px;min-width:30px;height:calc(100vh - var(--tf-bar));box-sizing:border-box;cursor:pointer;"
        <> "display:flex;align-items:center;justify-content:center;user-select:none;"
        <> "background:linear-gradient(#d2cec0,#c5c0b1);border-left:1px solid #b3ae9c" ]
    [ HH.span
        [ style $ engrave <> ";font-size:11px;letter-spacing:0.14em;color:#3f3c33;"
            <> "transform:rotate(-90deg);white-space:nowrap;display:inline-block" ]
        [ HH.text label ] ]

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

-- ---------------------------------------------------------------------------
-- Generator row — the LED enable + label + mutation depth + firing period, one
-- per random source. Shared by the NOTES pane (its GNotes header) and the
-- PARAMETERS pane (the top rows + each grid card's header), so it lives here.
-- The row is header-only; callers add any extras (Marbles pad, a grid) beneath.
-- ---------------------------------------------------------------------------

genRow :: forall m. State -> GenKind -> H.ComponentHTML Action () m
genRow s kind =
  let
    src = find (\g -> g.kind == kind) s.gen
    on = maybe false _.on src
    rate = maybe 90 _.rate src
    amt = maybe 30 _.amt src
  in
    HH.div [ style "display:flex;align-items:center;gap:7px" ]
      [ led on kind
      , HH.div [ style "flex:1;min-width:0" ]
          [ HH.div [ style $ engrave <> ";font-size:11px;color:#3f3c33;line-height:1.1" ]
              [ HH.text (genLabel kind) ]
          , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.65;letter-spacing:0.06em" ]
              [ HH.text (genSub kind) ]
          ]
      , amtNumber kind amt on
      , freqNumber kind rate on
      ]

-- | A round source-enable lamp. Click toggles; debounced in the handler so the
-- | doubled re-render dispatch can't cancel the flip.
led :: forall m. Boolean -> GenKind -> H.ComponentHTML Action () m
led on kind =
  HH.div
    [ HE.onClick \_ -> ToggleGen kind
    , style $ "width:15px;height:15px;border-radius:50%;cursor:pointer;flex:0 0 auto;"
        <> "border:1px solid #a8a392;box-shadow:inset 0 1px 1px #00000022;background:"
        <> (if on then "radial-gradient(circle at 35% 30%, #f0c25a, #b5832b)" else "#c4bfb0") ]
    []

-- | The mutation-depth number (how MUCH each change is), dragged vertically.
amtNumber :: forall m. GenKind -> Int -> Boolean -> H.ComponentHTML Action () m
amtNumber kind amt on =
  HH.div
    [ HE.onMouseDown \_ -> KnobDown (GenAmt kind) amt
    , style "display:flex;flex-direction:column;align-items:flex-end;cursor:ns-resize;min-width:32px;user-select:none" ]
    [ HH.span
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:12px;line-height:1;color:"
            <> (if on then "#5a564b" else "#a9a497") ]
        [ HH.text (show amt <> "%") ]
    , HH.span [ style $ engrave <> ";font-size:7px;opacity:0.55;margin-top:1px" ]
        [ HH.text "depth" ]
    ]

-- | The bare period number, dragged vertically (up = rarer). Reuses the knob
-- | drag infra via the GenRate target; renders as a plain number, no dial.
freqNumber :: forall m. GenKind -> Int -> Boolean -> H.ComponentHTML Action () m
freqNumber kind rate on =
  HH.div
    [ HE.onMouseDown \_ -> KnobDown (GenRate kind) rate
    , style "display:flex;flex-direction:column;align-items:flex-end;cursor:ns-resize;min-width:52px;user-select:none" ]
    [ HH.span
        [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:17px;line-height:1;font-weight:600;color:"
            <> (if on then "#7a3b1f" else "#9a9588") ]
        [ HH.text (show (periodOf rate)) ]
    , HH.span [ style $ engrave <> ";font-size:7px;opacity:0.6;margin-top:1px" ]
        [ HH.text "1 / N steps" ]
    ]
