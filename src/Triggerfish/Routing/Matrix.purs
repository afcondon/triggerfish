-- | **The router's matrices** (docs/kb/plans/matrix-router.md): the routing
-- | table drawn AUM-style, one grid for the voices' notes and one for the drum
-- | lanes' hits.
-- |
-- | What a column shares is said once, in its head (a port, the FH-2's gates);
-- | a cell carries only what varies (a channel, a jack, an envelope); the row
-- | says whose. So nothing is printed sixteen times. A drum MIDI column whose
-- | legs all share a channel names it in the head, and its cells are dots.
-- |
-- | Every change is one of the router's own edits (`Routing.Edit`), so the
-- | matrix and the ledger change the table the same way. An empty cell adds a
-- | leg with sensible defaults; a filled one opens its details under the grid
-- | (step the value, trim, on/off, remove).
-- |
-- | A destination with settings of its own has a **sheet**, opened from its
-- | column head, as tapping a node in AUM opens it: a Rample's slicing and each
-- | voice's trigger; each sample voice's set and window. The grid says where;
-- | the sheet says what the thing at the end of the wire is set to.
module Triggerfish.Routing.Matrix
  ( Grid(..)
  , Pick
  , Env
  , view
  , gridOf
  , columnOf
  ) where

import Prelude

import Data.Array (concatMap, filter, find, length, mapMaybe, mapWithIndex, nub, null, snoc)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), contains)
import Data.String as Str
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Routing.Edit (Edit(..))
import Triggerfish.Routing.Model as RM
import Triggerfish.SampleSets (SampleSet)

data Grid = Notes | Drums

derive instance Eq Grid

-- | A cell picked for its details: whose, and which column.
type Pick = { source :: RM.Source, col :: String }

type Env i =
  { table :: RM.Table
  , ports :: Array String
  -- Vetula's cards' channels: a card's address is its channel, set in its line,
  -- so these rows are shown, not edited here
  , cards :: Array Int
  , sampleSets :: Array SampleSet
  , pick :: Maybe Pick
  , focus :: Maybe String -- a column to light, when opened from the chart
  , sheet :: Maybe String -- a column whose settings sheet is open
  , onEdits :: Array Edit -> i
  , onPick :: Maybe Pick -> i
  , onSheet :: Maybe String -> i
  , onAudition :: RM.Destination -> i
  , onGrid :: Grid -> i
  , onClose :: i
  }

-- | The grid a machine's routes are in.
gridOf :: String -> Grid
gridOf m = if m == "balistes" then Drums else Notes

-- ---------------------------------------------------------------------------
-- Columns
-- ---------------------------------------------------------------------------

type Col =
  { key :: String
  , group :: String
  , name :: String
  , sub :: String
  , fam :: String
  , missing :: Boolean
  -- the kind `Routing.Edit.AddLeg` adds for this column, and its port
  , addKind :: Maybe String
  , port :: Maybe String
  }

-- | The port a leg names, as the port that exists (a table may hold a needle,
-- | "IAC", for "IAC Driver Tidal"); the needle itself when none matches.
portOf :: Array String -> String -> String
portOf ports needle = fromMaybe needle (find (contains (Pattern needle)) ports)

-- | The column a destination belongs to.
columnOf :: Array String -> RM.Destination -> String
columnOf ports = case _ of
  RM.DMidi d -> "midi:" <> portOf ports d.port
  RM.DContinuo _ -> "continuo"
  RM.DFh2Env _ -> "fh2env"
  RM.DFh2Gate _ -> "fh2gate"
  RM.DEs9Cv _ -> "es9cv"
  RM.DEs9Gate _ -> "es9gate"
  RM.DRample d -> "rample:" <> portOf ports d.port
  RM.DRamplePoly d -> "rample:" <> portOf ports d.port
  RM.DPoly d -> "poly:" <> RM.instrumentLabel d.inst
  RM.DSample _ -> "sample"

-- | The field a cell's value steps, and the value.
valueOf :: RM.Destination -> Maybe { field :: String, value :: Int }
valueOf = case _ of
  RM.DMidi d -> Just { field: "channel", value: d.channel }
  RM.DContinuo d -> Just { field: "channel", value: d.channel }
  RM.DFh2Env d -> Just { field: "slot", value: d.slot }
  RM.DFh2Gate d -> Just { field: "jack", value: d.jack }
  RM.DEs9Cv d -> Just { field: "bus", value: d.bus }
  RM.DEs9Gate d -> Just { field: "jack", value: d.jack }
  RM.DRample d -> Just { field: "voice", value: d.voice }
  _ -> Nothing

sources :: Grid -> RM.Table -> Array RM.Source
sources grid tbl = case grid of
  Drums -> map RM.SDrumLane (Array.range 0 15)
  Notes -> map RM.SOdonusHead (Array.range 0 3) <> filter isSelene (map _.source tbl)
  where
  isSelene = case _ of
    RM.SSeleneBank _ -> true
    _ -> false

-- | The grid's columns: every port that exists, the fixed rig destinations a
-- | grid of this kind uses, and any destination a leg names that is not among
-- | them (drawn as missing when its port is absent).
columns :: forall i. Grid -> Env i -> Array Col
columns grid env = Array.sortWith (\c -> fromMaybe 9 (Array.elemIndex c.group groupOrder)) (fixed <> extra)
  where
  groupOrder = [ "MIDI ports", "Hosted", "Modular", "Samplers", "Other" ]
  midi p = { key: "midi:" <> p, group: "MIDI ports", name: if p == "" then "no port chosen" else p, sub: if p `Array.elem` env.ports then "" else "not on this computer"
           , fam: "midi", missing: not (p `Array.elem` env.ports), addKind: Just "midi", port: Just p }
  one key group name sub fam addKind = { key, group, name, sub, fam, missing: false, addKind, port: Nothing }
  rampleCols = map (\p -> { key: "rample:" <> p, group: "Samplers", name: p, sub: (if contains (Pattern "Rample") p then "" else "Rample"), fam: "rample", missing: not (p `Array.elem` env.ports), addKind: Just "rample-1", port: Just p })
                 (filter (contains (Pattern "Rample")) env.ports)
  -- A port with a column of its own kind (the FH-2's envelopes and gates,
  -- Continuo, a Rample) is not offered again as a plain MIDI port; a leg that
  -- does name it plainly still gets its column, among the extras.
  plain p = not (Array.any (\k -> contains (Pattern k) (Str.toLower p)) [ "rample", "fh-2", "continuo" ])
  fixed = map midi (filter plain env.ports) <> case grid of
    Notes ->
      [ one "continuo" "Hosted" "Continuo" "piano" "continuo" (Just "continuo")
      , one "fh2env" "Modular" "FH-2 envelopes" "" "cv" (Just "fh2env")
      , one "es9cv" "Modular" "ES-9 CV" "calibrated" "cv" (Just "es9cv")
      ] <> rampleCols
    Drums ->
      [ one "fh2gate" "Modular" "FH-2 gates" "QuadDrum" "cv" (Just "fh2gate") ] <> rampleCols
        <> [ one "sample" "Samplers" "Sample voices" "SuperDirt" "sample" (Just "sample") ]
  known = map _.key fixed
  used = nub (concatMap (\src -> map (columnOf env.ports <<< _.dest) (RM.legsFor env.table src)) (sources grid env.table))
  extra = mapMaybe extraCol (filter (\k -> not (k `Array.elem` known)) used)
  extraCol k = case Array.uncons (splitKey k) of
    Just { head: "midi", tail: [ p ] } -> Just (midi p)
    Just { head: "rample", tail: [ p ] } -> Just { key: k, group: "Samplers", name: p, sub: (if contains (Pattern "Rample") p then "" else "Rample"), fam: "rample", missing: not (p `Array.elem` env.ports), addKind: Just "rample-1", port: Just p }
    Just { head: "poly", tail: [ i ] } -> Just (one k "Modular" i "poly" "cv" Nothing)
    Just { head: "es9gate" } -> Just (one k "Modular" "ES-9 gates" "" "cv" Nothing)
    Just { head: "fh2env" } -> Just (one k "Modular" "FH-2 envelopes" "" "cv" Nothing)
    Just { head: "continuo" } -> Just (one k "Hosted" "Continuo" "piano" "continuo" Nothing)
    _ -> Just (one k "Other" k "" "midi" Nothing)
  splitKey k = case Str.indexOf (Pattern ":") k of
    Just i -> [ Str.take i k, Str.drop (i + 1) k ]
    Nothing -> [ k ]

-- ---------------------------------------------------------------------------
-- Cells
-- ---------------------------------------------------------------------------

-- | The legs of a source in a column, with their index among the source's legs.
legsIn :: forall i. Env i -> RM.Source -> String -> Array { i :: Int, leg :: RM.Leg }
legsIn env src key =
  filter (\x -> columnOf env.ports x.leg.dest == key) (mapWithIndex (\i leg -> { i, leg }) (RM.legsFor env.table src))

-- | A drum MIDI column's shared channel, when every leg in it has the same one.
sharedChannel :: forall i. Env i -> Array RM.Source -> String -> Maybe Int
sharedChannel env srcs key =
  case nub (concatMap (\src -> mapMaybe (\x -> _.value <$> valueOf x.leg.dest) (legsIn env src key)) srcs) of
    [ ch ] -> Just ch
    _ -> Nothing

-- | The edits that add a leg for this source in this column, with a starting
-- | value: a head on its own channel and envelope, a drum lane on channel 10.
-- | `n` is the index the new leg will have: the source's leg count.
addEdits :: Grid -> RM.Source -> Col -> Int -> Array Edit
addEdits grid src col n = case col.addKind of
  Nothing -> []
  Just kind ->
    [ AddLeg src kind ]
      <> maybe [] (\p -> [ SetPort src n p ]) col.port
      <> case kind of
           "midi" -> [ SetField src n "channel" (show start) ]
           "continuo" -> [ SetField src n "channel" (show start) ]
           "fh2env" -> [ SetField src n "slot" (show own) ]
           _ -> []
  where
  start = if grid == Drums then 10 else own
  own = case src of
    RM.SOdonusHead h -> h + 1
    _ -> 1

view :: forall w i. Grid -> Env i -> HH.HTML w i
view grid env =
  HH.div [ cls "matrix-modal", HP.attr (HH.AttrName "role") "dialog" ]
    [ HH.div [ cls "matrix-backdrop", HE.onClick \_ -> env.onClose ] []
    , HH.div [ cls "matrix-card" ]
        [ HH.div [ cls "matrix-top" ]
            [ HH.div [ cls "matrix-switch" ]
                [ switch Notes "Notes" "the voices' streams · cell = channel or envelope"
                , switch Drums "Drums" "each lane's hits · cell = jack or voice; • = the lane's own note"
                ]
            , HH.button [ cls "matrix-close", HE.onClick \_ -> env.onClose, HP.title "Close (Esc)" ] [ HH.text "×" ]
            ]
        , HH.div [ cls "matrix-scroll" ] [ table ]
        , details
        ]
    ]
  where
  srcs = sources grid env.table
  cols = columns grid env
  switch g label hint =
    HH.button [ cls ("matrix-tab" <> if g == grid then " on" else ""), HE.onClick \_ -> env.onGrid g, HP.title hint ] [ HH.text label ]
  groups = Array.foldl (\acc c -> case Array.unsnoc acc of
                           Just { init, last } | last.name == c.group -> snoc init last { span = last.span + 1 }
                           _ -> snoc acc { name: c.group, span: 1 }) [] cols
  table =
    HH.table [ cls "m" ]
      [ HH.thead_
          [ HH.tr_ ([ HH.th_ [] ] <> map (\g -> HH.th [ cls "group", HP.colSpan g.span ] [ HH.text g.name ]) groups)
          , HH.tr_ ([ HH.th_ [] ] <> map colHead cols)
          ]
      , HH.tbody_ (map row srcs <> if grid == Notes then map cardRow env.cards else [])
      ]
  colHead c =
    HH.th ([ cls ("col" <> (if c.missing then " missing" else "") <> (if env.focus == Just c.key || env.sheet == Just c.key then " focus" else "") <> (if hasSheet c.key then " has-sheet" else ""))
           , HP.title (if c.missing then c.name <> " is not on this computer" else if hasSheet c.key then c.name <> ": open its settings" else c.name) ]
           <> (if hasSheet c.key then [ HE.onClick \_ -> env.onSheet (if env.sheet == Just c.key then Nothing else Just c.key) ] else []))
      [ HH.span [ cls "lab" ]
          [ HH.b_ [ HH.text c.name ]
          , HH.small_ [ HH.text (headSub c) ]
          ]
      , HH.span [ cls ("fam " <> c.fam) ] []
      ]
  headSub c = case grid, sharedChannel env srcs c.key of
    Drums, Just ch | Str.take 5 c.key == "midi:" -> joinSub c.sub ("ch " <> show ch)
    _, _ -> c.sub
  joinSub a b = if a == "" then b else a <> " · " <> b
  row src =
    HH.tr [ cls (if (_.source <$> env.pick) == Just src then "picked" else "") ]
      ([ HH.th [ cls "row" ] (rowLabel src) ] <> map (cell src) cols)
  rowLabel = case _ of
    RM.SDrumLane i -> [ HH.text (P.laneName i), HH.small_ [ HH.text (show (P.laneNote i)) ] ]
    src -> [ HH.text (RM.sourceLabel src) ]
  cell src c =
    let
      here = legsIn env src c.key
      picked = env.pick == Just { source: src, col: c.key }
      shared = if grid == Drums then sharedChannel env srcs c.key else Nothing
      mark x =
        let off = if x.leg.on then "" else " off"
        in case valueOf x.leg.dest of
             Just v | not (isMidi x.leg.dest && shared == Just v.value) -> HH.span [ cls ("v " <> c.fam <> off) ] [ HH.text (show v.value) ]
             _ -> HH.span [ cls ("dot " <> c.fam <> off) ] []
    in
      HH.td
        [ cls ((if picked then "picked" else "") <> (if env.focus == Just c.key then " focus" else ""))
        , HP.title (whose src <> " → " <> c.name)
        , HE.onClick \_ ->
            if not (null here) then env.onPick (if picked then Nothing else Just { source: src, col: c.key })
            else case addEdits grid src c (length (RM.legsFor env.table src)) of
              [] -> env.onPick Nothing
              es -> env.onEdits es
        ]
        (map mark here)
  cardRow ch =
    HH.tr [ cls "card" ]
      ([ HH.th [ cls "row" ] [ HH.text ("Vetula ch " <> show ch), HH.small_ [ HH.text "card" ] ] ]
        <> map (\c -> HH.td [ HP.title "a card's channel is set in its line" ]
                  (if isJust (Array.find (_ == c.key) (map (\p -> "midi:" <> p) (filter (contains (Pattern "IAC")) env.ports))) then [ HH.span [ cls "v midi ghost" ] [ HH.text (show ch) ] ] else []))
               cols)
  details = case env.sheet of
    Just key -> sheet key
    Nothing -> pickDetails
  pickDetails = case env.pick of
    Nothing -> HH.p [ cls "matrix-hint" ] [ HH.text "Click an empty cell to send there; a filled one for its details. A Rample's slicing and a sample's window are set in the router on Triggerfish's and Balistes's own pages." ]
    Just pk ->
      let here = legsIn env pk.source pk.col
      in HH.div [ cls "matrix-details" ]
           ([ HH.div [ cls "who" ] [ HH.text (whose pk.source <> " → " <> maybe pk.col _.name (find (\c -> c.key == pk.col) cols)) ] ]
             <> map (legDetail pk.source) here
             <> [ HH.button [ cls "add", HE.onClick \_ -> maybe (env.onPick Nothing) (\c -> env.onEdits (addEdits grid pk.source c (length (RM.legsFor env.table pk.source)))) (find (\c -> c.key == pk.col) cols) ] [ HH.text "+ another" ] ])
  legDetail src x =
    HH.div [ cls ("leg" <> if x.leg.on then "" else " off") ]
      ( case valueOf x.leg.dest of
          Just v ->
            [ HH.span [ cls "field" ] [ HH.text v.field ]
            , HH.button [ HE.onClick \_ -> env.onEdits [ SetField src x.i v.field (show (v.value - 1)) ] ] [ HH.text "−" ]
            , HH.span [ cls "val" ] [ HH.text (show v.value) ]
            , HH.button [ HE.onClick \_ -> env.onEdits [ SetField src x.i v.field (show (v.value + 1)) ] ] [ HH.text "+" ]
            ]
          Nothing -> [ HH.span [ cls "field" ] [ HH.text "details in the ledger" ] ]
        <>
          [ HH.label_
              [ HH.text "trim "
              , HH.input [ HP.value (show x.leg.offsetMs), HE.onValueChange \t -> env.onEdits [ SetOffset src x.i t ] ]
              , HH.text " ms"
              ]
          , HH.button [ HE.onClick \_ -> env.onEdits [ ToggleLeg src x.i ] ] [ HH.text (if x.leg.on then "on" else "off") ]
          , HH.button [ cls "remove", HE.onClick \_ -> env.onEdits [ RemoveLeg src x.i ] ] [ HH.text "remove" ]
          ]
      )
  whose = case _ of
    RM.SDrumLane i -> P.laneName i
    src -> RM.sourceLabel src
  -- ── sheets ────────────────────────────────────────────────────────────────
  sheet key =
    let
      rows = concatMap (\src -> map (\x -> { src, i: x.i, leg: x.leg }) (legsIn env src key)) srcs
      title = maybe key _.name (find (\c -> c.key == key) cols)
    in
      HH.div [ cls "matrix-sheet" ]
        [ HH.div [ cls "who" ]
            [ HH.text (title <> (if Str.take 7 key == "rample:" && not (contains (Pattern "Rample") title) then " · Rample" else ""))
            , HH.button [ cls "sheet-close", HE.onClick \_ -> env.onSheet Nothing ] [ HH.text "done" ]
            ]
        , if null rows then HH.p [ cls "matrix-hint" ] [ HH.text "Nothing is routed here yet: add it in the grid, then set it here." ]
          else if key == "sample" then sampleSheet rows
          else rampleSheet rows
        ]
  rampleSheet rows =
    HH.div_
      [ HH.p [ cls "matrix-hint" ] [ HH.text "What the card is set up for: which note fires each voice (SETTINGS > SPx), how the card is sliced, the note of slice 0, and how far ahead the slice control lands." ]
      , HH.table [ cls "sheet" ]
          [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ]) [ "", "voice", "trigger", "slices", "slice 0", "settle ms", "ch" ]) ]
          , HH.tbody_ (map rampleRow rows)
          ]
      ]
  rampleRow r = case r.leg.dest of
    RM.DRample d ->
      HH.tr_
        [ HH.th_ [ HH.text (whose r.src) ]
        , num r "voice" d.voice, num r "trigger" d.trigger, num r "slots" d.slots
        , num r "pitchOfSlot0" d.pitchOfSlot0, num r "settleMs" d.settleMs, num r "channel" d.channel ]
    RM.DRamplePoly d ->
      HH.tr_
        [ HH.th_ [ HH.text (whose r.src) ]
        , HH.td_ [ HH.text "poly" ]
        , HH.td_ (mapWithIndex (\k t -> numIn r ("trig" <> show (k + 1)) t) d.triggers)
        , num r "slots" d.slots, num r "pitchOfSlot0" d.pitchOfSlot0, HH.td_ [], num r "channel" d.channel ]
    _ -> HH.tr_ []
  sampleSheet rows =
    HH.table [ cls "sheet" ]
      [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ]) [ "", "set", "sample", "from %", "to %", "reverse", "gain %", "chop", "" ]) ]
      , HH.tbody_ (map sampleRow rows)
      ]
  sampleRow r = case r.leg.dest of
    RM.DSample d ->
      HH.tr_
        [ HH.th_ [ HH.text (whose r.src) ]
        , HH.td_
            [ HH.select [ HE.onValueChange \v -> env.onEdits [ SetSampleSet r.src r.i v ] ]
                (map (\set -> HH.option [ HP.value set.name, HP.selected (set.name == d.set) ] [ HH.text set.name ])
                   (if Array.any (\x -> x.name == d.set) env.sampleSets then env.sampleSets else snoc env.sampleSets { name: d.set, samples: 0 }))
            ]
        , num r "n" d.n, num r "begin" d.begin, num r "end" d.end
        , HH.td_ [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked d.reverse, HE.onChecked \b -> env.onEdits [ SetField r.src r.i "reverse" (if b then "1" else "0") ] ] ]
        , num r "gain" d.gain, num r "chop" d.chop
        , HH.td_ [ HH.button [ HE.onClick \_ -> env.onAudition r.leg.dest, HP.title "hear it now, through the rig" ] [ HH.text "▶" ] ]
        ]
    _ -> HH.tr_ []
  num r field v = HH.td_ [ numIn r field v ]
  numIn r field v =
    HH.input [ cls "num", HP.value (show v), HE.onValueChange \t -> env.onEdits [ SetField r.src r.i field t ] ]
  hasSheet key = key == "sample" || Str.take 7 key == "rample:"
  isMidi = case _ of
    RM.DMidi _ -> true
    _ -> false

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
