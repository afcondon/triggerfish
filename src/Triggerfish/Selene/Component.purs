-- | Triggerfish.Selene.Component — the polysignal rack, as a stack of
-- | destinations. Each destination is a group of eight signals (one generator
-- | kind) bound to a physical target; you add destinations in groups of eight
-- | and configure each in place.
-- |
-- | Increment 1 (this): the visual language. Every slot is drawn, not formed —
-- | LFOs as scaled, log-frequency waveforms; Euclids as step-rings with k/n in
-- | the centre; clocks + notes as number lists. Read-only: the viz reflects the
-- | model. Editing the numbers comes next, in the SOURCE pane (then hover +
-- | arrow-keys directly on these elements). No output/scheduling yet — Selene
-- | drives CV/gate via es9-daemon (and, for MIDI targets, the Midi path).
module Triggerfish.Selene.Component (component) where

import Prelude

import Data.Array (filter, length, mapWithIndex, range)
import Data.Int (round, toNumber)
import Data.Number (cos, pi, sin) as Num
import Data.String.Common (joinWith)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Odonus.Grid.Widgets (engrave, style, svgAttr, svgEl)
import Triggerfish.Selene.Model as M
import Triggerfish.Selene.Source as Source
import Triggerfish.SourceQuery (Query(..))
import Data.Maybe (Maybe(..))

-- ---------------------------------------------------------------------------
-- State / Actions
-- ---------------------------------------------------------------------------

-- | The SOURCE document is the authority for the rack: `bal` is its parsed
-- | projection, kept in sync on every edit so the visualisations reflect it.
type State = { sel :: M.Selene, doc :: String }

data Action
  = AddDest M.GenKind         -- append a template block (comment-safe)
  | SetDoc String             -- the whole editable document, verbatim

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        let doc = Source.printRack M.defaultSelene
        in { sel: Source.parseRack doc, doc }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

-- | Answer the shell's TIDAL-tab query with the verbatim rack document. Selene
-- | has no clock yet (its CV output path is unbuilt), so it ignores SyncFree.
handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply s.doc))
  SyncFree _ _ next -> pure (Just next)

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  -- append a fresh block to the document (so existing comments survive), then
  -- re-derive the rack from the new text.
  AddDest k -> H.modify_ \s ->
    let block = Source.printDest { target: M.defaultTargetFor k, range: M.Bipolar5V, bank: M.freshBank k }
        doc = s.doc <> "\n\n" <> block
    in s { doc = doc, sel = Source.parseRack doc }
  SetDoc doc -> H.modify_ \s -> s { doc = doc, sel = Source.parseRack doc }

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

accent :: String
accent = "#3f6f8a"   -- steel-blue, Selene's electric accent

ink :: String
ink = "#2b2922"

-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;inset:0;display:flex;align-items:stretch;overflow:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ rackPanel s
    , sourcePanel s
    ]

panel :: forall m. String -> String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panel label widthCss body =
  HH.div
    [ style $ widthCss <> ";height:100vh;box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
        <> "padding:18px 16px;display:flex;flex-direction:column" ]
    ( [ HH.div
          [ style $ engrave <> ";font-size:14px;letter-spacing:0.16em;color:#3f3c33;"
              <> "margin-bottom:14px;border-bottom:1px solid #00000018;padding-bottom:6px" ]
          [ HH.text label ]
      ] <> body )

-- ---------------------------------------------------------------------------
-- The rack: a stack of destinations + the add bar
-- ---------------------------------------------------------------------------

rackPanel :: forall m. State -> H.ComponentHTML Action () m
rackPanel s =
  panel "SELENE · DESTINATIONS" "flex:1 1 auto;min-width:0"
    ( mapWithIndex destinationRow s.sel.destinations
        <> [ addBar, footNote ]
    )

addBar :: forall m. H.ComponentHTML Action () m
addBar =
  HH.div [ style "display:flex;align-items:center;gap:8px;margin-top:14px" ]
    ( [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.6;margin-right:2px" ] [ HH.text "ADD 8 →" ] ]
        <> map addButton M.allKinds
    )

addButton :: forall m. M.GenKind -> H.ComponentHTML Action () m
addButton k =
  HH.button
    [ HE.onClick \_ -> AddDest k
    , style $ "padding:6px 11px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.08em;color:#3f3c33;"
        <> "background:linear-gradient(#efece1,#ddd9cb)" ]
    [ HH.text (M.kindLabel k) ]

footNote :: forall m. H.ComponentHTML Action () m
footNote =
  HH.div [ style $ engrave <> ";font-size:8px;opacity:0.45;margin-top:14px;line-height:1.6" ]
    [ HH.text "EACH DESTINATION = 8 SIGNALS → 8 JACKS. EDIT THE NUMBERS — AND RE-PATCH / REMOVE BLOCKS — IN THE SOURCE PANE. -- MUTES A SLOT." ]

-- One destination: a target/header strip on the left, eight visualised slots.
destinationRow :: forall m. Int -> M.Destination -> H.ComponentHTML Action () m
destinationRow i d =
  HH.div
    [ style $ "display:flex;align-items:stretch;gap:12px;padding:11px 12px;margin-bottom:10px;border-radius:8px;"
        <> "background:#00000008;border:1px solid #00000012" ]
    [ destHeader i d
    , HH.div [ style "display:flex;flex-wrap:wrap;gap:7px;align-items:center;flex:1 1 auto" ]
        (slotViews d.bank)
    ]

destHeader :: forall m. Int -> M.Destination -> H.ComponentHTML Action () m
destHeader _ d =
  HH.div [ style "display:flex;flex-direction:column;gap:5px;flex:0 0 132px;justify-content:center" ]
    [ HH.div [ style $ engrave <> ";font-size:10px;letter-spacing:0.1em;color:" <> ink ]
        [ HH.text (M.kindLabel (M.bankKind d.bank)) ]
    , HH.div
        [ style $ "padding:4px 8px;border-radius:5px;text-align:left;display:inline-block;align-self:flex-start;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#efece1;background:" <> accent ]
        [ HH.text (M.targetLabel d.target) ]
    , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.45" ]
        [ HH.text ("→ " <> M.targetWire d.target) ]
    ]

-- ---------------------------------------------------------------------------
-- Per-kind slot visualisations
-- ---------------------------------------------------------------------------

slotViews :: forall m. M.GenBank -> Array (H.ComponentHTML Action () m)
slotViews = case _ of
  M.GLfo slots -> map lfoCell slots
  M.GEuclid slots -> map euclidRing slots
  M.GClock slots -> mapWithIndex clockNumber slots
  M.GNote slots -> map noteCell slots

-- --- POLYLFO: a scaled waveform, 0V baseline, log-frequency, rate label ------

lfoCell :: forall m. M.ModSlot -> H.ComponentHTML Action () m
lfoCell sl =
  let
    w = 84.0
    h = 50.0
    mid = h / 2.0
    -- normalise the summed wave into the cell (90% of half-height)
    span = max 1.0 (abs sl.level + sl.sin + sl.sqr + sl.tri + abs sl.saw)
    samples = 48
    pt k =
      let
        u = toNumber k / toNumber samples
        x = u * w
        y = mid - (M.lfoValue sl u / span) * (mid * 0.9)
      in
        show (round2 x) <> "," <> show (round2 y)
    poly = joinWith " " (map pt (range 0 samples))
    hint = lfoShapeHint sl
  in
    cellBox 90.0
      [ svgEl "svg"
          [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h), svgAttr "width" "100%"
          , svgAttr "height" (show h), svgAttr "style" "display:block;overflow:visible" ]
          [ svgEl "line"
              [ svgAttr "x1" "0", svgAttr "y1" (show mid), svgAttr "x2" (show w), svgAttr "y2" (show mid)
              , svgAttr "stroke" "#00000022", svgAttr "stroke-width" "0.8", svgAttr "stroke-dasharray" "2 2" ] []
          , svgEl "polyline"
              [ svgAttr "points" poly, svgAttr "fill" "none", svgAttr "stroke" accent
              , svgAttr "stroke-width" "1.6", svgAttr "stroke-linejoin" "round" ] []
          ]
      , cellCaption (fmt2 sl.rate <> " Hz" <> hint)
      ]

-- which non-drawn shapes are present, as a tiny tag
lfoShapeHint :: M.ModSlot -> String
lfoShapeHint sl =
  let tags = filter (_ /= "") [ if sl.rnd > 0.0 then "+rnd" else "", if sl.nse > 0.0 then "+nse" else "" ]
  in if length tags == 0 then "" else " " <> joinWith "" tags

-- --- POLYEUCLID: a ring of step-dots with k / n in the centre ----------------

euclidRing :: forall m. M.EuclidSlot -> H.ComponentHTML Action () m
euclidRing sl =
  let
    sz = 64.0
    c = sz / 2.0
    r = c - 8.0
    bits = M.euclidBits sl
    n = length bits
    dotFor k on =
      let
        ang = (toNumber k / toNumber (max 1 n)) * 2.0 * pi - pi / 2.0
        dx = c + r * cos ang
        dy = c + r * sin ang
        rad = if on then 3.4 else 2.0
      in
        svgEl "circle"
          [ svgAttr "cx" (show (round2 dx)), svgAttr "cy" (show (round2 dy)), svgAttr "r" (show rad)
          , svgAttr "fill" (if on then accent else "none")
          , svgAttr "stroke" accent, svgAttr "stroke-width" (if on then "0" else "1") ] []
  in
    cellBox 70.0
      [ svgEl "svg"
          [ svgAttr "viewBox" ("0 0 " <> show sz <> " " <> show sz), svgAttr "width" "100%"
          , svgAttr "height" (show sz), svgAttr "style" "display:block" ]
          ( mapWithIndex dotFor bits
              <> [ svgEl "text"
                     [ svgAttr "x" (show c), svgAttr "y" (show (c + 4.0)), svgAttr "text-anchor" "middle"
                     , svgAttr "fill" ink, svgAttr "font-family" "'SF Mono',Menlo,monospace"
                     , svgAttr "font-size" "13" ]
                     [ HH.text (show sl.beats <> "/" <> show sl.steps) ]
                 ]
          )
      ]

-- --- POLYCLOCK: a list of division numbers -----------------------------------

clockNumber :: forall m. Int -> M.ClockSlot -> H.ComponentHTML Action () m
clockNumber _ sl =
  cellBox 58.0
    [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:18px;color:" <> ink <> ";text-align:center" ]
        [ HH.text ("×" <> show sl.multiplier) ]
    , cellCaption (M.clockBaseLabel sl.base <> " · " <> show sl.pulseWidth <> "%")
    ]

-- --- POLYNOTE: a list of notes -----------------------------------------------

noteCell :: forall m. M.PresetNoteSlot -> H.ComponentHTML Action () m
noteCell sl =
  cellBox 58.0
    [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:16px;color:" <> ink <> ";text-align:center" ]
        [ HH.text (M.noteName sl.note) ]
    , cellCaption ("midi " <> show sl.note)
    ]

-- ---------------------------------------------------------------------------
-- Cell chrome
-- ---------------------------------------------------------------------------

cellBox :: forall m. Number -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
cellBox widthPx body =
  HH.div
    [ style $ "width:" <> show (round widthPx) <> "px;flex:0 0 auto;padding:5px 4px;border-radius:6px;"
        <> "background:#ffffff55;border:1px solid #00000010;display:flex;flex-direction:column;gap:2px;align-items:center" ]
    body

cellCaption :: forall m. String -> H.ComponentHTML Action () m
cellCaption t =
  HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55;text-align:center" ] [ HH.text t ]

-- ---------------------------------------------------------------------------
-- Source panel — the growing-spec eDSL, one block per destination (read-only)
-- ---------------------------------------------------------------------------

sourcePanel :: forall m. State -> H.ComponentHTML Action () m
sourcePanel s =
  panel "SOURCE" "flex:0 0 340px"
    [ HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:6px" ]
        [ HH.text "THE RACK · EDIT THE NUMBERS · -- MUTES A SLOT" ]
    , HH.textarea
        [ HP.value s.doc
        , HE.onValueInput SetDoc
        , HP.spellcheck false
        , style $ "flex:1 1 auto;min-height:420px;resize:none;box-sizing:border-box;"
            <> "padding:9px 10px;border:1px solid #a8a392;border-radius:6px;background:#f4f1e8;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;line-height:1.5;color:#2b2922;"
            <> "white-space:pre;overflow:auto;outline:none" ]
    ]

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

pi :: Number
pi = Num.pi

cos :: Number -> Number
cos = Num.cos

sin :: Number -> Number
sin = Num.sin

abs :: Number -> Number
abs x = if x < 0.0 then -x else x

round2 :: Number -> Number
round2 x = toNumber (round (x * 100.0)) / 100.0

fmt2 :: Number -> String
fmt2 x = show (toNumber (round (x * 100.0)) / 100.0)
