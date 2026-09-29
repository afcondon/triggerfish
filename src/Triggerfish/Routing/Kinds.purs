-- | Every kind of destination, as data: its family, its kind label, and its
-- | fields.
-- |
-- | The router draws every kind from this one description (`Routing.View`), so
-- | a new `RM.Destination` constructor is a new case here and nowhere else in
-- | the view. The cases are exhaustive on purpose: a kind added to the model
-- | fails to compile here rather than showing up in the router with no fields.
-- |
-- | **Families** are the variable that matters most when reading a routing
-- | table (where does this come out: a MIDI port, a CV jack, the Rample, a
-- | sample, the piano?), so they get the row's hue and its mark. The specific
-- | kind is the muted detail beside the family name.
module Triggerfish.Routing.Kinds
  ( Family(..)
  , families
  , familyOf
  , familyName
  , familyHue
  , Kind
  , Port(..)
  , Value
  , Control(..)
  , describe
  , Addable
  , addable
  ) where

import Prelude

import Data.Array (mapWithIndex, take)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Triggerfish.Routing.Model as RM
import Triggerfish.Selene.Model (noteName)

-- | Where a leg comes out, as a player would sort it.
data Family = FMidi | FCvGate | FRample | FSample | FContinuo

derive instance eqFamily :: Eq Family

-- | In the order the key shows them.
families :: Array Family
families = [ FMidi, FCvGate, FRample, FSample, FContinuo ]

familyOf :: RM.Destination -> Family
familyOf = case _ of
  RM.DMidi _ -> FMidi
  RM.DFh2Env _ -> FCvGate
  RM.DFh2Gate _ -> FCvGate
  RM.DEs9Gate _ -> FCvGate
  RM.DEs9Cv _ -> FCvGate
  RM.DPoly _ -> FCvGate
  RM.DRample _ -> FRample
  RM.DRamplePoly _ -> FRample
  RM.DSample _ -> FSample
  RM.DContinuo _ -> FContinuo

familyName :: Family -> String
familyName = case _ of
  FMidi -> "MIDI"
  FCvGate -> "CV/gate"
  FRample -> "Rample"
  FSample -> "Sample"
  FContinuo -> "Continuo"

-- | The family's colour, as a CSS token the page defines (light and dark):
-- | `--f-midi`, `--f-cv`, `--f-rample`, `--f-sample`, `--f-continuo`.
familyHue :: Family -> String
familyHue f = "var(--f-" <> slug <> ")"
  where
  slug = case f of
    FMidi -> "midi"
    FCvGate -> "cv"
    FRample -> "rample"
    FSample -> "sample"
    FContinuo -> "continuo"

-- | A destination as the router draws it.
-- |
-- | - `detail`: the specific kind within the family (`fh2 env`, `×4 poly`), or
-- |   `""` where the family says it all;
-- | - `port`: a port the leg may be pointed at, or the fixed device it names;
-- | - `channel`: its MIDI channel, if it has one (the field is `"channel"`);
-- | - `values`: the kind's own values, each under its label.
type Kind =
  { family :: Family
  , detail :: String
  , port :: Port
  , channel :: Maybe Int
  , values :: Array Value
  }

-- | A choosable MIDI port (edited with `SetPort`), or a device the kind names by
-- | construction (the FH-2, the ES-9, SuperDirt), which is shown and not edited.
data Port = Choosable String | Fixed String

-- | One labelled group of controls, e.g. `SLOT 0  36 C2` or
-- | `TRIGGERS 60 61 62 63`. `tip` says what the value means.
type Value = { label :: String, tip :: String, controls :: Array Control }

-- | A single control, and how an edit to it reaches the table.
-- |
-- | - `Number`: an integer field, edited by `RM.setDestField field`. `width` is
-- |   in characters; `note` is the note name shown beside a field that is a
-- |   pitch (slot 0 = 36 reads `36 C2`).
-- | - `SampleSet`: the sample set, edited with `SetSampleSet`.
-- | - `Switch`: a 0/1 field shown as a word (`rev`), edited with
-- |   `RM.setDestField field`.
-- | - `Shown`: a fact of the kind that is not editable here.
data Control
  = Number { field :: String, value :: Int, width :: Int, note :: Maybe String }
  | SampleSet String
  | Switch { field :: String, on :: Boolean }
  | Shown String

describe :: RM.Destination -> Kind
describe d = case d of
  RM.DMidi x ->
    { family: FMidi, detail: "", port: Choosable x.port, channel: Just x.channel, values: [] }
  RM.DFh2Env x ->
    fixed "fh2 env"
      [ one "slot" "polyenv slot 1-8" "slot" x.slot 1 ]
  RM.DFh2Gate x ->
    fixed "fh2 gate"
      [ one "note" "the note the trigger MCV matches on" "note" x.note 3
      , one "jack" "FHX-8GT jack 1-8" "jack" x.jack 1
      ]
  RM.DEs9Gate x ->
    fixed "es9 gate"
      [ one "block" "gate block 0-7" "block" x.block 1
      , one "jack" "jack 1-8" "jack" x.jack 1
      ]
  RM.DEs9Cv x ->
    fixed "es9 cv" [ one "bus" "CV bus 1-16" "bus" x.bus 2 ]
  -- The allocator picks the jack, so nothing here is editable: the buses are
  -- how the module is patched (`RM.polyJacks`), shown so they can be checked.
  RM.DPoly x ->
    let js = RM.polyJacks x.inst
    in fixed (RM.instrumentLabel x.inst <> " poly")
         ( [ { label: "voices", tip: "ES-9 buses the voices are on", controls: [ Shown (joinWith " " (map show js.voiceBuses)) ] }
           , { label: js.ctrlLabel, tip: "ES-9 bus of the " <> js.ctrlLabel <> " jack", controls: [ Shown (show js.ctrlBus) ] }
           ]
             <> (if x.sortByPitch then [ { label: "order", tip: "the lowest note is always on voice 1", controls: [ Shown "bass on 1" ] } ] else [])
         )
  RM.DContinuo x ->
    { family: FContinuo, detail: "", port: Fixed "continuo", channel: Just x.channel, values: [] }
  -- The card's own facts are editable because they belong to the CARD, not to
  -- the module: another card sliced differently plays from the same route.
  RM.DRample x ->
    { family: FRample
    , detail: "voice " <> show x.voice
    , port: Choosable x.port
    , channel: Just x.channel
    , values:
        [ one "voice" "Rample voice 1-4" "voice" x.voice 1
        , one "trigger" "trigger note (SETTINGS > SPx)" "trigger" x.trigger 3
        , one "slots" "SLICER division of the card" "slots" x.slots 3
        , slot0 x.pitchOfSlot0
        , one "settle ms" "ms the start-point CC leads the note" "settleMs" x.settleMs 3
        ]
    }
  -- No settle: the allocator's own measured 40 ms governs the whole module, so
  -- it belongs to `Reef.Voices.rample`, not to this route.
  RM.DRamplePoly x ->
    { family: FRample
    , detail: "×4 poly"
    , port: Choosable x.port
    , channel: Just x.channel
    , values:
        [ one "slots" "SLICER division of the card" "slots" x.slots 3
        , slot0 x.pitchOfSlot0
        , { label: "triggers"
          , tip: "each voice's trigger note (SETTINGS > SP1-4)"
          , controls: mapWithIndex (\k t -> num ("trig" <> show (k + 1)) t 3)
              (take 4 (x.triggers <> [ 60, 61, 62, 63 ]))
          }
        ]
    }
  -- Played by SuperDirt on the rig, so it sounds in Rig mode only.
  RM.DSample x ->
    { family: FSample
    , detail: ""
    , port: Fixed "SuperDirt"
    , channel: Nothing
    , values:
        [ { label: "set", tip: "the sample set (a SuperDirt bank)", controls: [ SampleSet x.set ] }
        , one "n" "sample in the set" "n" x.n 3
        , one "begin" "window start, % of the sample" "begin" x.begin 3
        , one "end" "window end, % of the sample" "end" x.end 3
        , { label: "rev", tip: "play the window backwards", controls: [ Switch { field: "reverse", on: x.reverse } ] }
        , one "gain" "gain, %" "gain" x.gain 3
        , one "chop" "slices of the window across the step, 1-16" "chop" x.chop 2
        ]
    }
  where
  fixed detail values =
    { family: familyOf d, detail, port: Fixed (RM.deviceLabel (RM.destDevice d)), channel: Nothing, values }
  num field value width = Number { field, value, width, note: Nothing }
  one label tip field value width = { label, tip, controls: [ num field value width ] }
  slot0 p =
    { label: "slot 0", tip: "MIDI note of slice 0"
    , controls: [ Number { field: "pitchOfSlot0", value: p, width: 3, note: Just (noteName p) } ] }

-- | A kind of leg that can be added: the key `Routing.Edit.newDest` takes, what
-- | the menu calls it, and the family it is listed under.
type Addable = { value :: String, label :: String, family :: Family }

addable :: Array Addable
addable =
  [ { value: "midi", label: "MIDI", family: FMidi }
  , { value: "fh2env", label: "FH-2 envelope", family: FCvGate }
  , { value: "fh2gate", label: "FH-2 gate", family: FCvGate }
  , { value: "es9gate", label: "ES-9 gate", family: FCvGate }
  , { value: "es9cv", label: "ES-9 CV", family: FCvGate }
  , { value: "poly-saich", label: "Saïch (poly)", family: FCvGate }
  , { value: "poly-saich-sorted", label: "Saïch (poly, bass on voice 1)", family: FCvGate }
  , { value: "poly-rings", label: "Rings (poly mode)", family: FCvGate }
  , { value: "rample-1", label: "Rample voice 1", family: FRample }
  , { value: "rample-2", label: "Rample voice 2", family: FRample }
  , { value: "rample-3", label: "Rample voice 3", family: FRample }
  , { value: "rample-4", label: "Rample voice 4", family: FRample }
  , { value: "rample-poly", label: "Rample (4 voices, allocated)", family: FRample }
  , { value: "sample", label: "Sample (SuperDirt, Rig mode)", family: FSample }
  , { value: "continuo", label: "Continuo", family: FContinuo }
  ]
