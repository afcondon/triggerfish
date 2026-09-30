-- | The router, as a ledger: a row per leg, every kind of destination on one
-- | grid, drawn with the quiet controls of `halogen-widgets`.
-- |
-- | The columns: source (on its first leg's row), destination (the family mark,
-- | which is also the leg's on/off switch), port, channel, the kind's own
-- | values, trim, and status with the row tools. Each kind's fields come from
-- | `Routing.Kinds`, so a kind is described once and drawn everywhere.
-- |
-- | Drawn the same wherever a router appears (the dashboard, Triggerfish's ⌥1,
-- | Balistes on its own page). The page supplies the table and the facts about
-- | the room, and turns each `Edit` into a change of its own table.
-- |
-- | The page must load the library stylesheet (`halogen-widgets.css`, the
-- | `hw-quiet-*` and `hw-ledger-*` rules) and define the family tokens
-- | `--f-midi`, `--f-cv`, `--f-rample`, `--f-sample`, `--f-continuo`.
module Triggerfish.Routing.View
  ( Env
  , sourceRows
  , key
  , destKind
  ) where

import Prelude

import Data.Array (concatMap, filter, length, mapWithIndex, null)
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Ledger as L
import Halogen.Widgets.Quiet as Q
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Fish as Fish
import Triggerfish.Routing.Edit (Edit(..))
import Triggerfish.Routing.Kinds as K
import Triggerfish.Routing.Model as RM
import Triggerfish.SampleSets (SampleSet)

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

-- | These sources as one ledger, with no headings.
sourceRows :: forall w i. Env i -> Array RM.Source -> HH.HTML w i
sourceRows env srcs = L.ledger layout (concatMap (rowsFor env) srcs)

-- | The key to the destination marks: each family, with how many legs of these
-- | sources reach it.
key :: forall w i. Env i -> Array RM.Source -> HH.HTML w i
key env srcs =
  Q.key (map entry K.families)
  where
  legs = concatMap (RM.legsFor env.table) srcs
  entry f =
    { name: K.familyName f
    , hue: K.familyHue f
    , count: length (filter (\l -> K.familyOf l.dest == f) legs)
    }

-- | The mock-up's columns. Status may hold a fault as long as "nothing sends
-- | here yet", so it grows to fit rather than running under the page edge.
layout :: L.LedgerConfig
layout =
  { columns:
      [ { head: "Source", track: "9.5em", align: Q.Start }
      , { head: "Destination", track: "11.5em", align: Q.Start }
      , { head: "Port", track: "11em", align: Q.Start }
      , { head: "Ch", track: "3.2em", align: Q.End }
      , { head: "", track: "minmax(16em, 1fr)", align: Q.Start }
      , { head: "Trim ms", track: "4.4em", align: Q.End }
      , { head: "", track: "minmax(7.5em, max-content)", align: Q.Start }
      ]
  , minWidth: "860px"
  }

-- | One source: a row per leg, its name on the first, then the "+ destination"
-- | line. A source with no legs still gets its row, so it can be given one.
rowsFor :: forall w i. Env i -> RM.Source -> Array (L.Row w i)
rowsFor env src =
  ( if null legs then
      [ L.Entry { first: true, off: false, cells: [ sourceName src, Q.note "no destination" ] } ]
    else mapWithIndex (legRow env src) legs
  )
    <> [ L.Add { from: 2, content: addControl env src } ]
  where
  legs = RM.legsFor env.table src

-- | A source's name, and what it is, on its first row, after its machine's fish.
-- | The fish makes every row name its machine, so the table needs no group
-- | headings and can later be sorted by any column. (A page that has not
-- | installed the fish sprite shows only the name.)
sourceName :: forall w i. RM.Source -> HH.HTML w i
sourceName src =
  HH.span [ HP.style "display:inline-flex;align-items:baseline;gap:8px" ]
    [ HH.span [ HP.style "flex:none;width:22px;align-self:center;display:inline-flex" ]
        [ Fish.icon "tf-fish-row" (Fish.ofSource src) ]
    , nameOf src
    ]

nameOf :: forall w i. RM.Source -> HH.HTML w i
nameOf src = L.name case src of
  RM.SOdonusHead h -> { name: RM.sourceLabel src, sub: "head " <> show (h + 1) }
  RM.SDrumLane i -> { name: P.laneName i, sub: "drum lane · " <> show (P.laneNote i) }
  RM.SVetulaVoice "" -> { name: "Vetula", sub: "default voice" }
  RM.SVetulaVoice nm -> { name: nm, sub: "Vetula voice" }
  RM.SSeleneBank a -> { name: a, sub: "Selene bank" }

-- | A leg: its mark (and switch), port, channel, values, trim, and whether it
-- | can actually be reached. The reach is the point of the whole panel: a route
-- | to a port that isn't there makes exactly as much sound as no route, so it
-- | shows, in the danger colour, and nothing shows when all is well.
legRow :: forall w i. Env i -> RM.Source -> Int -> RM.Leg -> L.Row w i
legRow env src i leg =
  L.Entry
    { first: i == 0
    , off: not leg.on
    , cells:
        [ if i == 0 then sourceName src else HH.text ""
        , Q.mark
            { name: K.familyName kind.family
            , detail: kind.detail
            , hue: K.familyHue kind.family
            , on: leg.on
            , onToggle: edit (ToggleLeg src i)
            }
        , case kind.port of
            K.Choosable p ->
              Q.select { value: p, options: env.ports, label: "MIDI port", onChange: edit <<< SetPort src i }
            K.Fixed name -> HH.text name
        , maybe (HH.text "")
            (\ch -> number 2 (show ch) "MIDI channel 1-16" (edit <<< SetField src i "channel"))
            kind.channel
        , L.values (map value kind.values)
        , number 4 (fmtOffset leg.offsetMs) "trim, ms: the flam killer when doubling" (edit <<< SetOffset src i)
        , HH.span_
            ( [ if reach == RM.Reachable then HH.text "" else Q.fault (RM.reachNote reach) ]
                <> audition
                <> [ Q.tool { glyph: "✕", label: "remove this destination", onClick: edit (RemoveLeg src i) } ]
            )
        ]
    }
  where
  kind = K.describe leg.dest
  reach = RM.reachOf { found: env.ports, rigUp: env.rigUp } leg.dest
  edit = env.onEdit
  audition = case leg.dest of
    RM.DSample _ -> [ Q.tool { glyph: "▷", label: "hear it now (through the rig)", onClick: env.onAudition leg.dest } ]
    _ -> []
  value v = Q.labelled v.label (concatMap (control v.tip) v.controls)
  control tip = case _ of
    K.Number n ->
      [ number n.width (show n.value) tip (edit <<< SetField src i n.field) ]
        <> maybe [] (\s -> [ Q.note s ]) n.note
    K.SampleSet set ->
      [ Q.select { value: set, options: map _.name env.sampleSets, label: tip, onChange: edit <<< SetSampleSet src i } ]
    K.Switch s ->
      [ Q.toggle
          { on: s.on
          , text: if s.on then "on" else "off"
          , label: tip
          , onToggle: edit (SetField src i s.field (if s.on then "0" else "1"))
          }
      ]
    K.Shown s -> [ HH.text s ]

number :: forall w i. Int -> String -> String -> (String -> i) -> HH.HTML w i
number width value label onChange =
  Q.number { value, width, align: Q.End, label, onChange, disabled: false }

fmtOffset :: Number -> String
fmtOffset n = if n == 0.0 then "0" else show n

-- | "+ destination": a faint line that opens the kinds, grouped by family.
addControl :: forall w i. Env i -> RM.Source -> HH.HTML w i
addControl env src =
  Q.choose
    { prompt: "+ destination"
    , label: "add a destination to " <> RM.sourceLabel src
    , groups: map group K.families
    , onChoose: env.onEdit <<< AddLeg src
    }
  where
  group f =
    { name: K.familyName f
    , options: map (\a -> { value: a.value, label: a.label }) (filter (\a -> a.family == f) K.addable)
    }

-- | A destination's family and kind, in words: `CV/gate fh2 env`.
destKind :: RM.Destination -> String
destKind d =
  let k = K.describe d
  in K.familyName k.family <> (if k.detail == "" then "" else " " <> k.detail)
