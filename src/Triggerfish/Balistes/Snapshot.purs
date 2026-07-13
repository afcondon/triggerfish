-- | Triggerfish.Balistes.Snapshot — the snapshot-bank + sequence pane (the
-- | control-space machinery): capture a control point into a slot, recall it
-- | instantly, and lay a path the playhead walks to morph the kit (Grids-style
-- | song structure).
-- |
-- | Today these are the Mutable-only views over `Model`'s `snapshots`/`sequence`.
-- | They are the seat of the tri-snapshot rework (#182/#199): this module is
-- | where the bank becomes a 3-brain `TriSnapshot` bank and grows the persistent
-- | rail. Pure renderers — capture/recall touch `HalogenM` and live in
-- | `Balistes.Component`.
module Triggerfish.Balistes.Snapshot
  ( snapshotSection
  , snapshotSlot
  , sequenceSection
  , seqCell
  ) where

import Prelude

import Data.Array (length, range, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Widgets (engrave, style, svgAttr, svgEl)
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Types (Action(..), State)
import Triggerfish.Balistes.Widgets (armBtn, flatBtn, stepBtn)

-- The snapshot bank: capture the whole control point (X/Y + densities +
-- randomness + open + push) into a slot, recall it instantly. Records the
-- two-handed gestures a single mouse can't (kick up while snare down). Each
-- filled slot shows a mini X/Y dot so the bank reads as a constellation of
-- points in control space.
snapshotSection :: forall m. State -> H.ComponentHTML Action () m
snapshotSection s =
  HH.div_
    [ HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:7px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "SNAPSHOTS" ]
        , HH.button
            [ HE.onClick \_ -> ToggleCap
            , style $ "padding:4px 12px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;letter-spacing:0.08em;"
                <> (if s.capArm then "color:#fbeae7;background:linear-gradient(#b23b28,#9a3120)"
                    else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
            [ HH.text (if s.capArm then "● ARMED" else "CAPTURE") ]
        ]
    , HH.div [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:6px;max-width:200px" ]
        (map (snapshotSlot s) (range 0 (M.snapshotCount - 1)))
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;line-height:1.5;margin-top:8px" ]
        [ HH.text (if s.capArm then "ARMED — CLICK A SLOT TO STORE THE CURRENT KIT." else "CLICK CAPTURE THEN A SLOT TO STORE · CLICK A SLOT TO RECALL · SHIFT-CLICK TO CLEAR.") ]
    ]

-- One snapshot slot: a mini X/Y pad. Filled shows the captured cursor as a dot;
-- empty is a faint outline with its index.
snapshotSlot :: forall m. State -> Int -> H.ComponentHTML Action () m
snapshotSlot s i =
  let
    msnap = M.snapshotAt s.bal i
    filled = case msnap of
      Just _ -> true
      Nothing -> false
    body = case msnap of
      Just snap ->
        [ svgEl "svg"
            [ svgAttr "viewBox" "0 0 100 100", svgAttr "width" "30", svgAttr "height" "30"
            , svgAttr "style" "display:block" ]
            [ svgEl "circle"
                [ svgAttr "cx" (show (toNumber snap.x / 255.0 * 100.0))
                , svgAttr "cy" (show ((1.0 - toNumber snap.y / 255.0) * 100.0))
                , svgAttr "r" "13", svgAttr "fill" "#1c1a12" ] []
            ]
        ]
      Nothing ->
        [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.45" ] [ HH.text (show (i + 1)) ] ]
  in
    HH.div
      [ HE.onClick \e -> SlotClick i (ME.shiftKey e)
      , style $ "width:32px;height:32px;border-radius:5px;cursor:pointer;display:flex;"
          <> "align-items:center;justify-content:center;box-sizing:border-box;"
          <> (if filled then "border:1px solid #a8a392;background:#cfcabb"
              else "border:1px dashed #b3ae9c;background:#00000006") ]
      body

-- The snapshot sequence: a path of slot references the playhead walks, each
-- held `seqBars` bars; advancing recalls that snapshot, morphing the kit. Build
-- it with SEQ+ (then click snapshots in order); play it with ▸.
sequenceSection :: forall m. State -> H.ComponentHTML Action () m
sequenceSection s =
  let
    seq = s.bal.sequence
    n = length seq
  in
    HH.div_
      [ HH.div [ style "display:flex;align-items:center;gap:8px;margin-bottom:8px" ]
          [ HH.span [ style $ engrave <> ";font-size:9px;flex:0 0 auto" ] [ HH.text "SEQUENCE" ]
          , armBtn (if s.seqEnabled then "❚❚ STOP" else "▸ PLAY") s.seqEnabled ToggleSeq
          , armBtn (if s.seqArm then "● BUILD" else "SEQ +") s.seqArm ToggleSeqBuild
          , HH.div [ style "display:flex;align-items:center;gap:4px;margin-left:6px" ]
              [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ] [ HH.text "BARS/STEP" ]
              , stepBtn "−" (SeqBarsDelta (-1))
              , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;width:14px;text-align:center" ] [ HH.text (show s.bal.seqBars) ]
              , stepBtn "+" (SeqBarsDelta 1)
              ]
          , flatBtn "CLEAR" ClearSeq
          ]
      , if n == 0 then
          HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;line-height:1.6" ]
            [ HH.text (if s.seqArm then "ARMED — CLICK SNAPSHOTS IN ORDER TO LAY THE PATH." else "PRESS SEQ + THEN CLICK SNAPSHOTS TO LAY A PATH; ▸ PLAYS IT, MORPHING THE KIT EACH STEP.") ]
        else
          HH.div [ style "display:flex;flex-wrap:wrap;gap:5px" ]
            (map (seqCell s) (range 0 (n - 1)))
      ]

-- One step of the sequence lane: the snapshot index, highlighted on the playhead.
seqCell :: forall m. State -> Int -> H.ComponentHTML Action () m
seqCell s p =
  let
    slot = fromMaybe 0 (s.bal.sequence !! p)
    here = s.seqEnabled && p == s.seqPos
  in
    HH.div
      [ style $ "width:26px;height:26px;border-radius:5px;display:flex;align-items:center;justify-content:center;"
          <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;box-sizing:border-box;"
          <> (if here then "background:#1c1a12;color:#efece1;border:1px solid #1c1a12"
              else "background:#cfcabb;color:#3f3c33;border:1px solid #a8a392") ]
      [ HH.text (show (slot + 1)) ]
