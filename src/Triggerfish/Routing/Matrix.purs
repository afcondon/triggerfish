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
-- | voice's trigger; each sample voice's set and window; and, for a drum MIDI
-- | port, the note each lane sends (Drum Rack pads, GM, or one per lane). The grid says where;
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
import Data.Foldable (maximum, minimum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String (Pattern(..), contains)
import Data.String as Str
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Reef.Balistes.Pattern as P
import Triggerfish.Continuo as Continuo
import Triggerfish.Routing.Edit (Edit(..))
import Triggerfish.Routing.Model as RM
import Triggerfish.SampleSets (SampleSet)

-- | `Hosted` is Continuo's own grid (AC, 2026-11-13): the same voices as
-- | `Notes`, against only the instruments Continuo hosts, with its mixer
-- | under it. `Notes` then says no more of Continuo than who goes there.
data Grid = Notes | Drums | Hosted

derive instance Eq Grid

-- | A cell picked for its details: whose, and which column.
type Pick = { source :: RM.Source, col :: String }

type Env i =
  { table :: RM.Table
  , ports :: Array String
  -- what Continuo is hosting, as it says; none when it does not answer
  , continuo :: Array Continuo.Layer
  -- Vetula's cards' channels: each card is a row (`RM.cardSource`), routed
  -- like a head; its address, the channel, is set in its line
  , cards :: Array Int
  , sampleSets :: Array SampleSet
  , pick :: Maybe Pick
  , focus :: Maybe String -- a column to light, when opened from the chart
  , sheet :: Maybe String -- a column whose settings sheet is open
  , fold :: Boolean -- drum lanes routed alike shown as one row
  , onEdits :: Array Edit -> i
  , onPick :: Maybe Pick -> i
  , onSheet :: Maybe String -> i
  , onAudition :: RM.Destination -> i
  -- an instrument in Continuo (1 is the first) and its new level or pan
  , onMix :: Int -> Continuo.Mix -> i
  , onGrid :: Grid -> i
  , onFold :: Boolean -> i
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
  -- the value a new leg here starts with, when the column fixes it: an
  -- instrument in Continuo is a channel
  , start :: Maybe Int
  }

-- | The port a leg names, as the port that exists (a table may hold a needle,
-- | "IAC", for "IAC Driver Tidal"); the needle itself when none matches.
portOf :: Array String -> String -> String
portOf ports needle = fromMaybe needle (find (contains (Pattern needle)) ports)

-- | The column a destination belongs to. A Continuo leg belongs to the
-- | instrument on its channel when every instrument has a channel of its own
-- | (`Continuo.split`), and to the column of legs nothing plays when no
-- | instrument is on that channel. Otherwise Continuo is one column and the
-- | channel is the cell's value, as it always was.
columnOf :: Array Continuo.Layer -> Array String -> RM.Destination -> String
columnOf hosted ports = case _ of
  RM.DMidi d -> "midi:" <> portOf ports d.port
  RM.DContinuo d
    | not (Continuo.split hosted) -> "continuo"
    | d.channel `Array.elem` Continuo.channels hosted -> "continuo:" <> show d.channel
    | otherwise -> continuoUnheard
  RM.DFh2Env _ -> "fh2env"
  RM.DFh2Gate _ -> "fh2gate"
  RM.DEs9Cv _ -> "es9cv"
  RM.DFh2Voice _ -> "fh2voice"
  RM.DEs9Gate _ -> "es9gate"
  RM.DRample d -> "rample:" <> portOf ports d.port
  RM.DRamplePoly d -> "rample:" <> portOf ports d.port
  RM.DPoly d -> "poly:" <> RM.instrumentLabel d.inst
  RM.DSample _ -> "sample"

-- | The column a destination is drawn in, in this grid. The notes' grid
-- | gathers every Continuo leg into one column that says only that the voice
-- | goes there; which instrument is `Hosted`'s to say.
keyIn :: forall i. Grid -> Env i -> RM.Destination -> String
keyIn grid env dest = case grid, dest of
  Notes, RM.DContinuo _ -> continuoAll
  _, _ -> columnOf env.continuo env.ports dest

-- | The notes' one column for Continuo.
continuoAll :: String
continuoAll = "continuo-all"

-- | The column of Continuo legs on a channel no instrument listens on.
continuoUnheard :: String
continuoUnheard = "continuo:unheard"

-- | Whether a column is one instrument in Continuo, where a leg is a tick.
isInstrument :: String -> Boolean
isInstrument key = Str.take 9 key == "continuo:" && key /= continuoUnheard

-- | The field a cell's value steps, and the value.
valueOf :: RM.Destination -> Maybe { field :: String, value :: Int }
valueOf = case _ of
  RM.DMidi d -> Just { field: "channel", value: d.channel }
  RM.DContinuo d -> Just { field: "channel", value: d.channel }
  RM.DFh2Env d -> Just { field: "slot", value: d.slot }
  RM.DFh2Gate d -> Just { field: "jack", value: d.jack }
  RM.DEs9Cv d -> Just { field: "jack", value: d.jack }
  RM.DFh2Voice d -> Just { field: "channel", value: d.channel }
  RM.DEs9Gate d -> Just { field: "jack", value: d.jack }
  RM.DRample d -> Just { field: "voice", value: d.voice }
  _ -> Nothing

sources :: Grid -> RM.Table -> Array RM.Source
sources grid tbl = case grid of
  Drums -> map RM.SDrumLane (Array.range 0 15)
  _ -> map RM.SOdonusHead (Array.range 0 3) <> filter isSelene (map _.source tbl)
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
           , fam: "midi", missing: not (p `Array.elem` env.ports), addKind: Just "midi", port: Just p, start: Nothing }
  one key group name sub fam addKind = { key, group, name, sub, fam, missing: false, addKind, port: Nothing, start: Nothing }
  -- Continuo: a column for each instrument it hosts, when each has a channel
  -- of its own; one column, the channel in the cell, when it does not.
  continuoCols
    | Continuo.split env.continuo = Continuo.channels env.continuo <#> \ch ->
        (one ("continuo:" <> show ch) "Hosted" (Continuo.instrumentOn env.continuo ch) "Continuo" "continuo" (Just "continuo")) { start = Just ch }
    | otherwise = [ one "continuo" "Hosted" "Continuo" "piano" "continuo" (Just "continuo") ]
  rampleCols = map (\p -> { key: "rample:" <> p, group: "Samplers", name: p, sub: (if contains (Pattern "Rample") p then "" else "Rample"), fam: "rample", missing: not (p `Array.elem` env.ports), addKind: Just "rample-1", port: Just p, start: Nothing })
                 (filter (contains (Pattern "Rample")) env.ports)
  -- A port with a column of its own kind (the FH-2's envelopes and gates,
  -- Continuo, a Rample) is not offered again as a plain MIDI port; a leg that
  -- does name it plainly still gets its column, among the extras.
  plain p = not (Array.any (\k -> contains (Pattern k) (Str.toLower p)) [ "rample", "fh-2", "continuo" ])
  fixed = if grid == Hosted then continuoCols else map midi (filter plain env.ports) <> case grid of
    Hosted -> []
    Notes ->
      [ one continuoAll "Hosted" "Continuo" "open" "continuo" Nothing
      , one "fh2env" "Modular" "FH-2 envelopes" "" "cv" (Just "fh2env")
      , one "es9cv" "Modular" "ES-9 CV" "calibrated" "cv" (Just "es9cv")
      , one "fh2voice" "Modular" "FH-2 voices" "by channel" "cv" (Just "fh2voice")
      ] <> rampleCols
    Drums ->
      [ one "fh2gate" "Modular" "FH-2 gates" "QuadDrum" "cv" (Just "fh2gate") ] <> rampleCols
        <> [ one "sample" "Samplers" "Sample voices" "SuperDirt" "sample" (Just "sample") ]
  known = map _.key fixed
  used = filter here (nub (concatMap (\src -> map (keyIn grid env <<< _.dest) (RM.legsFor env.table src)) (sources grid env.table <> (if grid == Drums then [] else map RM.cardSource env.cards))))
  -- Continuo's grid draws Continuo's columns and no others
  here k = grid /= Hosted || Str.take 8 k == "continuo"
  extra = mapMaybe extraCol (filter (\k -> not (k `Array.elem` known)) used)
  extraCol k = case Array.uncons (splitKey k) of
    Just { head: "midi", tail: [ p ] } -> Just (midi p)
    Just { head: "rample", tail: [ p ] } -> Just { key: k, group: "Samplers", name: p, sub: (if contains (Pattern "Rample") p then "" else "Rample"), fam: "rample", missing: not (p `Array.elem` env.ports), addKind: Just "rample-1", port: Just p, start: Nothing }
    Just { head: "poly", tail: [ i ] } -> Just (one k "Modular" i "poly" "cv" Nothing)
    Just { head: "es9gate" } -> Just (one k "Modular" "ES-9 gates" "" "cv" Nothing)
    Just { head: "fh2env" } -> Just (one k "Modular" "FH-2 envelopes" "" "cv" Nothing)
    -- legs on a channel no instrument listens on: shown, so they can be
    -- seen and stopped, and not offered
    Just { head: "continuo" } | k == continuoUnheard -> Just ((one k "Hosted" "Continuo" "no instrument" "continuo" Nothing) { missing = true })
    Just { head: "continuo" } -> Just (one k "Hosted" "Continuo" "piano" "continuo" Nothing)
    _ -> Just (one k "Other" k "" "midi" Nothing)
  splitKey k = case Str.indexOf (Pattern ":") k of
    Just i -> [ Str.take i k, Str.drop (i + 1) k ]
    Nothing -> [ k ]

-- ---------------------------------------------------------------------------
-- Cells
-- ---------------------------------------------------------------------------

-- | The legs of a source in a column, with their index among the source's legs.
legsIn :: forall i. Grid -> Env i -> RM.Source -> String -> Array { i :: Int, leg :: RM.Leg }
legsIn grid env src key =
  filter (\x -> keyIn grid env x.leg.dest == key) (mapWithIndex (\i leg -> { i, leg }) legs)
  where
  -- a card with no row yet plays its default, so show that
  legs = case RM.cardChannelOf src of
    Just ch -> RM.cardLegs env.ports env.table ch
    Nothing -> RM.legsFor env.table src

-- | A drum MIDI column's shared channel, when every leg in it has the same one.
sharedChannel :: forall i. Grid -> Env i -> Array RM.Source -> String -> Maybe Int
sharedChannel grid env srcs key =
  case nub (concatMap (\src -> mapMaybe (\x -> _.value <$> valueOf x.leg.dest) (legsIn grid env src key)) srcs) of
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
           "continuo" -> [ SetField src n "channel" (show (fromMaybe start col.start)) ]
           "fh2env" -> [ SetField src n "slot" (show own) ]
           _ -> []
  where
  start = if grid == Drums then 10 else own
  own = case src of
    RM.SOdonusHead h -> h + 1
    other -> fromMaybe 1 (RM.cardChannelOf other)

view :: forall w i. Grid -> Env i -> HH.HTML w i
view grid env =
  HH.div [ cls "matrix-modal", HP.attr (HH.AttrName "role") "dialog" ]
    [ HH.div [ cls "matrix-backdrop", HE.onClick \_ -> env.onClose ] []
    , HH.div [ cls "matrix-card" ]
        [ HH.div [ cls "matrix-top" ]
            [ HH.div [ cls "matrix-switch" ]
                [ switch Notes "Notes" "the voices' streams · cell = channel or envelope"
                , switch Drums "Drums" "each lane's hits · cell = jack or voice; • = the lane's own note"
                , switch Hosted "Continuo" "the instruments Continuo hosts · a dot plays a voice on one; their levels and pans below"
                ]
            , if grid == Drums then
                HH.label [ cls "matrix-fold", HP.title "show lanes routed exactly alike as one row; a click in it routes them all" ]
                  [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked env.fold, HE.onChecked env.onFold ]
                  , HH.text " fold alike"
                  ]
              else HH.text ""
            , HH.button [ cls "matrix-close", HE.onClick \_ -> env.onClose, HP.title "Close (Esc)" ] [ HH.text "×" ]
            ]
        , HH.div [ cls "matrix-scroll" ] [ table ]
        , details
        ]
    ]
  where
  srcs = sources grid env.table <> (if grid == Drums then [] else map RM.cardSource env.cards)
  -- rows: one source each, or with folding, every drum lane routed exactly
  -- alike (same legs in every column) as one row, where the first of them stood
  rows
    | grid == Drums && env.fold =
        Array.foldl (\acc src -> case Array.findIndex (\g -> maybe false (\h -> sig h == sig src) (Array.head g)) acc of
                       Just k -> fromMaybe acc (Array.modifyAt k (_ `snoc` src) acc)
                       Nothing -> snoc acc [ src ]) [] srcs
    | otherwise = map pure srcs
  sig src = map (\c -> map (\x -> { dest: x.leg.dest, on: x.leg.on, offsetMs: x.leg.offsetMs }) (legsIn grid env src c.key)) cols
  cols = columns grid env
  switch g label hint =
    HH.button [ cls ("matrix-tab" <> if g == grid then " on" else ""), HE.onClick \_ -> env.onGrid g, HP.title hint ] [ HH.text label ]
  -- the chart opens the matrix on "continuo": light every instrument's column
  focused c = env.focus == Just c.key || (env.focus == Just "continuo" && Str.take 9 c.key == "continuo:")
  groups = Array.foldl (\acc c -> case Array.unsnoc acc of
                           Just { init, last } | last.name == c.group -> snoc init last { span = last.span + 1 }
                           _ -> snoc acc { name: c.group, span: 1 }) [] cols
  table =
    HH.table [ cls "m" ]
      [ HH.thead_
          [ HH.tr_ ([ HH.th_ [] ] <> map (\g -> HH.th [ cls "group", HP.colSpan g.span ] [ HH.text g.name ]) groups)
          , HH.tr_ ([ HH.th_ [] ] <> map colHead cols)
          ]
      , HH.tbody_ (map row rows)
      ]
  colHead c =
    HH.th ([ cls ("col" <> (if c.missing then " missing" else "") <> (if focused c || env.sheet == Just c.key then " focus" else "") <> (if hasSheet c.key then " has-sheet" else ""))
           , HP.title (if c.key == continuoAll then "Continuo: open its instruments and mixer" else if c.key == continuoUnheard then "sent to Continuo on a channel no instrument listens on: silent" else if c.missing then c.name <> " is not on this computer" else if hasSheet c.key then c.name <> ": open its settings" else c.name) ]
           <> (if c.key == continuoAll then [ HE.onClick \_ -> env.onGrid Hosted ]
               else if hasSheet c.key then [ HE.onClick \_ -> env.onSheet (if env.sheet == Just c.key then Nothing else Just c.key) ] else []))
      [ HH.span [ cls "lab" ]
          [ HH.b_ [ HH.text c.name ]
          , HH.small_ [ HH.text (headSub c) ]
          ]
      , HH.span [ cls ("fam " <> c.fam) ] []
      ]
  headSub c = case grid, sharedChannel grid env srcs c.key of
    Drums, Just ch | isMidiKey c.key -> joinSub c.sub ("ch " <> show ch <> " · " <> noteMapName c.key)
    Drums, Nothing | isMidiKey c.key -> joinSub c.sub (noteMapName c.key)
    _, _ -> c.sub
  -- what a drum MIDI column's lanes send, as one word when they agree on one
  noteMapName key =
    let ls = concatMap (\src -> map (\x -> { src, leg: x.leg }) (legsIn grid env src key)) srcs
    in if null ls then ""
       else if Array.all (\x -> isPad x.src x.leg) ls then "pads"
       else if Array.all (\x -> x.leg.note < 0) ls then "GM"
       else "custom notes"
  isPad src leg = case src of
    RM.SDrumLane i -> leg.note == RM.drumPad i
    _ -> false
  joinSub a b = if a == "" then b else a <> " · " <> b
  row group =
    HH.tr [ cls ((if maybe false (\pk -> Array.elem pk.source group) env.pick then "picked" else "") <> (if length group > 1 then " folded" else "")) ]
      ([ HH.th [ cls "row" ] (rowLabel group) ] <> map (cell group) cols)
  rowLabel group = case group of
    [ RM.SDrumLane i ] -> [ HH.text (laneLabel i) ]
    [ src ] -> [ HH.text (RM.sourceLabel src) ]
    _ -> [ HH.text (Str.joinWith " · " (map whose group)) ]
  cell group c =
    let
      src = fromMaybe (RM.SDrumLane 0) (Array.head group)
      here = legsIn grid env src c.key
      picked = maybe false (\pk -> pk.col == c.key && Array.elem pk.source group) env.pick
      shared = if grid == Drums then sharedChannel grid env srcs c.key else Nothing
      mark x =
        let off = if x.leg.on then "" else " off"
        in case valueOf x.leg.dest of
             -- a drum lane's MIDI leg: the note it sends, which is the pad it plays
             Just v | grid == Drums && isMidi x.leg.dest ->
               HH.span [ cls ("v " <> c.fam <> off), HP.title (noteTitle src x.leg <> (if shared == Just v.value then "" else " · ch " <> show v.value)) ]
                 [ HH.text (foldedNotes x) ]
             -- an instrument in Continuo: the column says which, so a leg is a tick
             Just _ | isInstrument c.key || c.key == continuoAll -> HH.span [ cls ("dot " <> c.fam <> off) ] []
             Just v | not (isMidi x.leg.dest && shared == Just v.value) -> HH.span [ cls ("v " <> c.fam <> off) ] [ HH.text (show v.value) ]
             _ -> HH.span [ cls ("dot " <> c.fam <> off) ] []
      -- a folded row's lanes play their own pads: say the run they cover
      foldedNotes x =
        let ns = concatMap (\s -> map (sentNote s <<< _.leg) (legsIn grid env s c.key)) group
        in case minimum ns, maximum ns of
             Just lo, Just hi | lo /= hi -> show lo <> "–" <> show hi
             _, _ -> show (sentNote src x.leg)
    in
      -- One click either way (AC, 2026-10-29: adding took one click and
      -- removing two, "reminiscent of dark patterns"): an empty cell sends
      -- there, a filled one's ✕ stops it, and the cell itself opens its
      -- details. The ✕ is the cell's sibling, not its child, so the cell's
      -- own click never fires with it.
      HH.td
        [ cls ((if picked then "picked" else "") <> (if focused c then " focus" else "") <> (if null here then "" else " filled")) ]
        ( [ HH.div
              [ cls "hit"
              , HP.title (whose src <> " → " <> c.name)
              , HE.onClick \_ ->
                  -- the notes' Continuo column only says who goes there:
                  -- which instrument is chosen in Continuo's own grid
                  if c.key == continuoAll then env.onGrid Hosted
                  else if not (null here) then env.onPick (if picked then Nothing else Just { source: src, col: c.key })
                  else case concatMap (\s -> addEdits grid s c (length (RM.legsFor env.table s))) group of
                    [] -> env.onPick Nothing
                    es -> env.onEdits es
              ]
              (map mark here) ]
            <> (if null here then [] else
                  [ HH.button
                      [ cls "unroute", HP.title ("stop sending " <> whose src <> " to " <> c.name)
                      , HE.onClick \_ -> env.onEdits (removals group c.key) ]
                      [ HH.text "\x00d7" ] ]) )
  -- every leg of the row (or folded rows) in that column, the highest index
  -- first so the ones before it keep theirs
  removals group key = group # concatMap \s ->
    map (\x -> RemoveLeg s x.i) (Array.reverse (Array.sortWith _.i (legsIn grid env s key)))
  details = case grid, env.sheet of
    -- Continuo's grid always shows its mixer: every instrument, side by side
    Hosted, _ -> HH.div [ cls "matrix-sheet" ] [ mixer ]
    _, Just key -> sheet key
    _, Nothing -> pickDetails
  pickDetails = case env.pick of
    Nothing -> HH.p [ cls "matrix-hint" ] [ HH.text "Click an empty cell to send there, its × to stop, the cell for its details. A Rample's slicing and a sample's window are set in the router on Triggerfish's and Balistes's own pages." ]
    Just pk ->
      let here = legsIn grid env pk.source pk.col
      in HH.div [ cls "matrix-details" ]
           ([ HH.div [ cls "who" ] [ HH.text (whose pk.source <> " → " <> maybe pk.col _.name (find (\c -> c.key == pk.col) cols)) ] ]
             <> map (legDetail pk.source) here
             <> (if isInstrument pk.col then [] else [ HH.button [ cls "add", HE.onClick \_ -> maybe (env.onPick Nothing) (\c -> env.onEdits (addEdits grid pk.source c (length (RM.legsFor env.table pk.source)))) (find (\c -> c.key == pk.col) cols) ] [ HH.text "+ another" ] ]))
  legDetail src x =
    HH.div [ cls ("leg" <> if x.leg.on then "" else " off") ]
      ( ( if grid == Drums && isMidi x.leg.dest then
            [ HH.span [ cls "field" ] [ HH.text "note" ]
            , HH.button [ HE.onClick \_ -> env.onEdits [ SetNote src x.i (show (sentNote src x.leg - 1)) ] ] [ HH.text "−" ]
            , HH.span [ cls "val", HP.title (noteTitle src x.leg) ] [ HH.text (show (sentNote src x.leg) <> " " <> noteName (sentNote src x.leg)) ]
            , HH.button [ HE.onClick \_ -> env.onEdits [ SetNote src x.i (show (sentNote src x.leg + 1)) ] ] [ HH.text "+" ]
            ]
          else [] ) <>
        case valueOf x.leg.dest of
          -- the channel of an instrument is not the player's to step: it is
          -- the column
          Just _ | isInstrument (columnOf env.continuo env.ports x.leg.dest) -> []
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
    RM.SDrumLane i -> laneLabel i
    src -> RM.sourceLabel src
  -- ── sheets ────────────────────────────────────────────────────────────────
  sheet key =
    let
      sheetRows = concatMap (\src -> map (\x -> { src, i: x.i, leg: x.leg }) (legsIn grid env src key)) srcs
      title = maybe key _.name (find (\c -> c.key == key) cols)
    in
      HH.div [ cls "matrix-sheet" ]
        [ HH.div [ cls "who" ]
            [ HH.text (title <> (if Str.take 7 key == "rample:" && not (contains (Pattern "Rample") title) then " · Rample" else ""))
            , HH.button [ cls "sheet-close", HE.onClick \_ -> env.onSheet Nothing ] [ HH.text "done" ]
            ]
        , if null sheetRows then HH.p [ cls "matrix-hint" ] [ HH.text "Nothing is routed here yet: add it in the grid, then set it here." ]
          else if key == "sample" then sampleSheet sheetRows
          else if isMidiKey key then notesSheet sheetRows
          else rampleSheet sheetRows
        ]
  -- An instrument's place in Continuo's mix: its level and its pan. Several
  -- instruments layered on one channel each have their own.
  mixer =
    let
      here = mapWithIndex (\i layer -> { n: i + 1, layer }) env.continuo
      slider lo hi step value title change =
        HH.input [ HP.type_ HP.InputRange, HP.min lo, HP.max hi, HP.step (HP.Step step), HP.value (show value), HP.title title
                 , HE.onValueInput \t -> maybe (env.onPick env.pick) change (Number.fromString t) ]
      mixRow x =
        HH.tr_
          [ HH.th_ [ HH.text x.layer.name ]
          , HH.td_ [ slider (-40.0) 6.0 0.5 x.layer.levelDb "level, in dB: 0 is the instrument's own" (env.onMix x.n <<< Continuo.Level) ]
          , HH.td_ [ HH.text (levelText x.layer.levelDb) ]
          , HH.td_ [ slider (-1.0) 1.0 0.05 x.layer.pan "pan: left to right" (env.onMix x.n <<< Continuo.Pan) ]
          , HH.td_ [ HH.text (panText x.layer.pan) ]
          , HH.td_ [ HH.button [ HE.onClick \_ -> env.onMix x.n (Continuo.Level 0.0) ] [ HH.text "0 dB" ]
                   , HH.button [ HE.onClick \_ -> env.onMix x.n (Continuo.Pan 0.0) ] [ HH.text "centre" ] ]
          ]
    in
      if null here then HH.p [ cls "matrix-hint" ] [ HH.text "Continuo is not answering: start it from the chart, and its instruments appear here." ]
      else HH.div_
        [ HH.p [ cls "matrix-hint" ] [ HH.text "Click an empty cell to play a voice on an instrument, its × to stop. Below, where each instrument sits in Continuo's mix; it lasts until Continuo restarts." ]
        , HH.table [ cls "sheet" ]
            [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ]) [ "instrument", "level", "", "pan", "", "" ]) ]
            , HH.tbody_ (map mixRow here)
            ]
        ]
  levelText db = (if db > 0.0 then "+" else "") <> show (Number.round (db * 10.0) / 10.0) <> " dB"
  panText p
    | p < -0.02 = "L " <> show (Int.round (negate p * 100.0))
    | p > 0.02 = "R " <> show (Int.round (p * 100.0))
    | otherwise = "centre"
  -- The note each lane sends this port: a Drum Rack's pads (lane n, pad n),
  -- GM, or set lane by lane.
  notesSheet legs =
    HH.div_
      [ HH.p [ cls "matrix-hint" ] [ HH.text "The note each lane sends to this port. Pads: lane 1 plays the Drum Rack's first pad (C1, 36), lane 16 its last (D#2, 51). GM: the General MIDI drum note. Or set any lane's own." ]
      , HH.div [ cls "presets" ]
          [ HH.button [ HE.onClick \_ -> env.onEdits (map (\r -> SetNote r.src r.i (show (padOf r.src))) legs), HP.title RM.padsLabel ] [ HH.text "Drum Rack pads" ]
          , HH.button [ HE.onClick \_ -> env.onEdits (map (\r -> SetNote r.src r.i "") legs), HP.title "each lane's General MIDI drum note" ] [ HH.text "GM" ]
          ]
      , HH.table [ cls "sheet" ]
          [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ]) [ "lane", "note", "" ]) ]
          , HH.tbody_ (map noteRow legs)
          ]
      ]
  noteRow r =
    HH.tr_
      [ HH.th_ [ HH.text (whose r.src) ]
      , HH.td_ [ HH.input [ cls "num", HP.value (show (sentNote r.src r.leg)), HE.onValueChange \t -> env.onEdits [ SetNote r.src r.i t ] ] ]
      , HH.td_ [ HH.text (noteName (sentNote r.src r.leg) <> (if r.leg.note < 0 then " · GM" else if isPad r.src r.leg then " · pad " <> show (sentNote r.src r.leg - 35) else "")) ]
      ]
  padOf = case _ of
    RM.SDrumLane i -> RM.drumPad i
    _ -> 36
  rampleSheet legs =
    HH.div_
      [ HH.p [ cls "matrix-hint" ] [ HH.text "What the card is set up for: which note fires each voice (SETTINGS > SPx), how the card is sliced, the note of slice 0, and how far ahead the slice control lands." ]
      , HH.table [ cls "sheet" ]
          [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ]) [ "", "voice", "trigger", "slices", "slice 0", "settle ms", "ch" ]) ]
          , HH.tbody_ (map rampleRow legs)
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
  sampleSheet legs =
    HH.table [ cls "sheet" ]
      [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ]) [ "", "set", "sample", "from %", "to %", "reverse", "gain %", "chop", "" ]) ]
      , HH.tbody_ (map sampleRow legs)
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
  hasSheet key = key == "sample" || Str.take 7 key == "rample:" || (grid == Drums && isMidiKey key)
  isMidi = case _ of
    RM.DMidi _ -> true
    _ -> false

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

-- | A drum lane as the matrix names it: its number, 1..16, and its drum.
laneLabel :: Int -> String
laneLabel i = show (i + 1) <> " " <> P.laneName i

-- | The note a source's leg sends: its own, or the lane's GM note.
sentNote :: RM.Source -> RM.Leg -> Int
sentNote src leg
  | leg.note >= 0 = leg.note
  | otherwise = case src of
      RM.SDrumLane i -> P.laneNote i
      _ -> 36

noteTitle :: RM.Source -> RM.Leg -> String
noteTitle src leg = "note " <> show (sentNote src leg) <> " (" <> noteName (sentNote src leg) <> " in Ableton)"

-- | A note as Ableton names it: C1 = 36, middle C = C3.
noteName :: Int -> String
noteName n = fromMaybe "?" (names Array.!! (n `mod` 12)) <> show (n / 12 - 2)
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

isMidiKey :: String -> Boolean
isMidiKey key = Str.take 5 key == "midi:"
