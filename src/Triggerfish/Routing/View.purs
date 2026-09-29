-- | The router's rows for one source: its name and an add menu, then a line per
-- | destination it fans out to, with that destination's fields, its trim, and
-- | whether it can be reached.
-- |
-- | Drawn the same wherever a router appears (Triggerfish's ⌥1, Balistes on its
-- | own page). The page supplies the table and the facts about the room, and
-- | turns each `Edit` into a change of its own table.
module Triggerfish.Routing.View
  ( Env
  , sourceRows
  , destKind
  ) where

import Prelude

import Data.Array (any, elem, find, mapWithIndex, take, (:))
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Routing.Edit (Edit(..))
import Triggerfish.Routing.Model as RM
import Triggerfish.SampleSets (SampleSet)
import Triggerfish.Ui.Style (style)

-- | What the rows are drawn from, and what they raise.
-- |
-- | - `ports`: the MIDI outputs that exist, the only ones a leg may be pointed at;
-- | - `rigUp`: whether the rig is connected, which decides what a rig-only leg can reach;
-- | - `onEdit`: a change to the table;
-- | - `onAudition`: hear this destination now, through the rig.
type Env i =
  { table :: RM.Table
  , ports :: Array String
  , rigUp :: Boolean
  , sampleSets :: Array SampleSet
  , onEdit :: Edit -> i
  , onAudition :: RM.Destination -> i
  }

-- | One source: its name, then a line per destination it fans out to.
sourceRows :: forall w i. Env i -> RM.Source -> HH.HTML w i
sourceRows env src =
  -- `min-width:0` and a wrapping leg keep a long leg (a sample leg has nine
  -- controls) inside its own column. Overflowing, it ran under the next column,
  -- which then took its clicks: the ✕ of a sample leg could not be reached.
  HH.div [ style "display:flex;flex-direction:column;gap:2px;margin-bottom:7px;min-width:0" ]
    ( [ HH.div [ style "display:flex;align-items:baseline;gap:8px" ]
          [ HH.span [ style "font-size:12px;color:#2a271e;min-width:96px" ]
              [ HH.text (rowLabel src) ]
          , addControl env src
          ]
      ] <> mapWithIndex (legRow env src) (RM.legsFor env.table src) )

-- | Drum lanes read better as their kit name + note than as an index.
rowLabel :: RM.Source -> String
rowLabel = case _ of
  RM.SDrumLane i -> P.laneName i <> "  " <> show (P.laneNote i)
  other -> RM.sourceLabel other

-- | A leg: on/off, what it is, its editable numbers, its trim, and whether it can
-- | actually be reached. The reach column is the point of the whole panel — a
-- | route to a port that isn't there makes exactly as much sound as no route.
legRow :: forall w i. Env i -> RM.Source -> Int -> RM.Leg -> HH.HTML w i
legRow env src i leg =
  let reach = RM.reachOf { found: env.ports, rigUp: env.rigUp } leg.dest
      dead = reach /= RM.Reachable
      dim = if leg.on then "1" else "0.4"
  in HH.div
       [ style $ "display:flex;flex-wrap:wrap;align-items:center;gap:5px;margin-left:14px;opacity:" <> dim ]
       ( [ HH.span
             [ HE.onClick \_ -> env.onEdit (ToggleLeg src i)
             , HP.title (if leg.on then "mute this destination (keeps it)" else "unmute")
             , style $ "cursor:pointer;font-size:11px;width:14px;color:" <> (if leg.on then "#3a6a4a" else "#a09a88") ]
             [ HH.text (if leg.on then "●" else "○") ]
         , HH.span [ style "font-size:10px;color:#6a6558;width:52px" ] [ HH.text (destKind leg.dest) ]
         ] <> destFields env src i leg.dest <>
         [ numBox 40 (fmtOffset leg.offsetMs) (env.onEdit <<< SetOffset src i) "ms trim — the flam killer when doubling"
         , HH.span
             [ HE.onClick \_ -> env.onEdit (RemoveLeg src i)
             , HP.title "remove this destination"
             , style "cursor:pointer;color:#b09a86;font-size:11px;padding:0 3px" ]
             [ HH.text "\x2715" ]
         , HH.span
             [ style $ "font-size:9px;font-family:'SF Mono',Menlo,monospace;"
                 <> (if dead then "color:#b0492f" else "color:#7a9a7a") ]
             [ HH.text (if dead then RM.reachNote reach else "ok") ]
         ] )

destKind :: RM.Destination -> String
destKind = case _ of
  RM.DMidi _ -> "midi"
  RM.DFh2Env _ -> "fh2 env"
  RM.DFh2Gate _ -> "fh2 gate"
  RM.DEs9Gate _ -> "es9 gate"
  RM.DEs9Cv _ -> "es9 cv"
  RM.DPoly _ -> "poly"
  RM.DContinuo _ -> "continuo"
  RM.DRample d -> "rample v" <> show d.voice
  RM.DRamplePoly _ -> "rample x4"
  RM.DSample _ -> "sample"

-- | The editable numbers of a destination, which differ per device because the
-- | devices differ. An FH-2 gate shows BOTH its selector note and its jack, since
-- | neither is meaningful without the other.
destFields :: forall w i. Env i -> RM.Source -> Int -> RM.Destination -> Array (HH.HTML w i)
destFields env src i = case _ of
  RM.DMidi d ->
    [ portSelect env src i d.port
    , field 30 (show d.channel) "channel" "MIDI channel 1-16" ]
  RM.DFh2Env d -> [ field 30 (show d.slot) "slot" "polyenv slot 1-8" ]
  RM.DFh2Gate d ->
    [ field 34 (show d.note) "note" "note the trigger MCV matches on"
    , field 30 (show d.jack) "jack" "FHX-8GT jack 1-8" ]
  RM.DEs9Gate d ->
    [ field 26 (show d.block) "block" "gate block"
    , field 26 (show d.jack) "jack" "jack 1-8" ]
  RM.DEs9Cv d -> [ field 30 (show d.bus) "bus" "CV bus" ]
  RM.DPoly d -> [ HH.span [ HP.class_ (HH.ClassName "rt-fixed") ] [ HH.text (RM.destLabel (RM.DPoly d)) ] ]
  RM.DContinuo d -> [ field 30 (show d.channel) "channel" "channel 1-16" ]
  -- Played by SuperDirt on the rig, so it sounds in Rig mode only.
  RM.DSample d ->
    [ setSelect env src i d.set
    , field 26 (show d.n) "n" ("sample in the set, 0-" <> show (samplesIn d.set - 1))
    , field 26 (show d.begin) "begin" "window start, % of the sample"
    , field 26 (show d.end) "end" "window end, % of the sample"
    , HH.span
        [ HE.onClick \_ -> env.onEdit (SetField src i "reverse" (if d.reverse then "0" else "1"))
        , HP.title "play the window backwards"
        , style $ "cursor:pointer;font-size:10px;padding:0 3px;color:" <> (if d.reverse then "#3a6a4a" else "#a09a88") ]
        [ HH.text "rev" ]
    , field 30 (show d.gain) "gain" "gain, %"
    , field 22 (show d.chop) "chop" "chop: slices of the window across the step, 1-16"
    , HH.span
        [ HE.onClick \_ -> env.onAudition (RM.DSample d)
        , HP.title "hear it now (through the rig)"
        , style "cursor:pointer;font-size:11px;color:#3a6a4a;padding:0 3px" ]
        [ HH.text "\x25B6" ]
    ]
  -- The card's own facts are editable because they belong to the CARD, not to
  -- the module: another card sliced differently plays from the same route.
  RM.DRample d ->
    [ portSelect env src i d.port
    , field 26 (show d.channel) "channel" "MIDI channel 1-16"
    , field 26 (show d.voice) "voice" "Rample voice 1-4"
    , field 30 (show d.trigger) "trigger" "trigger note (SETTINGS > SPx)"
    , field 30 (show d.slots) "slots" "SLICER division of the card"
    , field 30 (show d.pitchOfSlot0) "pitchOfSlot0" "MIDI note of slice 0"
    , field 26 (show d.settleMs) "settleMs" "ms the start-point CC leads the note" ]
  -- No settle box: the allocator's own measured 40 ms governs the whole
  -- module, so it belongs to `Reef.Voices.rample`, not to this route.
  RM.DRamplePoly d ->
    [ portSelect env src i d.port
    , field 26 (show d.channel) "channel" "MIDI channel 1-16"
    , field 30 (show d.slots) "slots" "SLICER division of the card"
    , field 30 (show d.pitchOfSlot0) "pitchOfSlot0" "MIDI note of slice 0"
    ] <> mapWithIndex
      (\k t -> field 26 (show t) ("trig" <> show (k + 1))
                ("voice " <> show (k + 1) <> " trigger note (SETTINGS > SP" <> show (k + 1) <> ")"))
      (take 4 (d.triggers <> [ 60, 61, 62, 63 ]))
  where
  field w v name tip = numBox w v (env.onEdit <<< SetField src i name) tip
  samplesIn set = maybe 0 _.samples (find (\x -> x.name == set) env.sampleSets)

-- | Only ports that EXIST are offerable, so a route can't be typed at a device
-- | that isn't plugged in. (An already-routed name that has since vanished stays
-- | selected and shows dead, rather than being silently rewritten.)
portSelect :: forall w i. Env i -> RM.Source -> Int -> String -> HH.HTML w i
portSelect env src i cur =
  HH.select
    [ HE.onValueChange (env.onEdit <<< SetPort src i)
    , style "font-size:10px;max-width:118px;padding:1px 2px;border-radius:3px;border:1px solid #cdbb96;background:#fffdf8" ]
    (map (\n -> HH.option [ HP.value n, HP.selected (n == cur) ] [ HH.text n ])
      (if elem cur env.ports then env.ports else cur : env.ports))

setSelect :: forall w i. Env i -> RM.Source -> Int -> String -> HH.HTML w i
setSelect env src i cur =
  HH.select
    [ HE.onValueChange (env.onEdit <<< SetSampleSet src i)
    , style "font-size:10px;max-width:150px;padding:1px 2px;border-radius:3px;border:1px solid #cdbb96;background:#fffdf8" ]
    (map (\x -> HH.option [ HP.value x.name, HP.selected (x.name == cur) ] [ HH.text x.name ])
      (if any (\x -> x.name == cur) env.sampleSets then env.sampleSets else { name: cur, samples: 0 } : env.sampleSets))

numBox :: forall w i. Int -> String -> (String -> i) -> String -> HH.HTML w i
numBox w v f tip =
  HH.input
    [ HP.value v, HE.onValueInput f, HP.title tip
    , style $ "width:" <> show w <> "px;font-family:'SF Mono',Menlo,monospace;font-size:10px;"
        <> "padding:1px 3px;border-radius:3px;border:1px solid #cdbb96;background:#fffdf8;text-align:center" ]

fmtOffset :: Number -> String
fmtOffset n = if n == 0.0 then "0" else show n

addControl :: forall w i. Env i -> RM.Source -> HH.HTML w i
addControl env src =
  HH.select
    [ HE.onValueChange (env.onEdit <<< AddLeg src)
    , style "font-size:10px;padding:1px 3px;border-radius:3px;border:1px solid #d8cdb8;background:#faf7f0;color:#6a655a" ]
    ( [ HH.option [ HP.value "", HP.selected true ] [ HH.text "+ add" ] ]
        <> map (\(Tuple v l) -> HH.option [ HP.value v ] [ HH.text l ])
             [ Tuple "midi" "MIDI", Tuple "fh2env" "FH-2 envelope", Tuple "fh2gate" "FH-2 gate"
             , Tuple "es9gate" "ES-9 gate", Tuple "es9cv" "ES-9 CV"
             , Tuple "poly-saich" "Saïch (poly)"
             , Tuple "poly-saich-sorted" "Saïch (poly, bass on voice 1)"
             , Tuple "poly-rings" "Rings (poly mode)"
             , Tuple "continuo" "continuo"
             , Tuple "rample-1" "Rample voice 1"
             , Tuple "rample-2" "Rample voice 2"
             , Tuple "rample-3" "Rample voice 3"
             , Tuple "rample-4" "Rample voice 4"
             , Tuple "rample-poly" "Rample (4 voices, allocated)"
             , Tuple "sample" "Sample (SuperDirt, Rig mode)" ] )
