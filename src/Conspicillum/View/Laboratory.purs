-- | **The laboratory**: every control, where the module has four knobs.
-- |
-- | The same scene as the module, not a copy: what is turned here is heard and
-- | drawn there. The numbers are generated from reef's parameter catalogue, a
-- | section at a time, so a parameter added to reef appears here with its
-- | range, taper, readout and help and nothing to write. Only what is not a
-- | number has its own control: the onsets, the tape's structure, the rules,
-- | the progression.
module Conspicillum.View.Laboratory
  ( Callbacks
  , laboratory
  ) where

import Prelude

import Data.Array (catMaybes, deleteAt, elem, filter, length, mapWithIndex, modifyAt, null, snoc)
import Data.Either (hush)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Reef.Conspicillum.Cloud (ResonatorFollow(..), Spec, WarpMode(..))
import Reef.Conspicillum.Decimal (trimmed)
import Reef.Conspicillum.Harmonic (nameOfChord)
import Reef.Conspicillum.Notation (OnsetLayout(..), chordList, euclid, even, onsetLayout, opNames, stepTable, stepsText)
import Reef.Conspicillum.Parameter (Parameter, Section(..), allParameters, denormalise, describe, engaged, format, normalise, read, sectionName)
import Reef.Conspicillum.Preset (Preset)
import Reef.Conspicillum.Protocol (WireRule, fromWireRule, toWireRule)

type Callbacks i =
  { editSpec :: (Spec -> Spec) -> i
  , setParameter :: Parameter -> Number -> i
  , pin :: Parameter -> i
  , nothing :: i
  }

laboratory
  :: forall w i
   . Callbacks i
  -> { preset :: Preset, scene :: String }
  -> HH.HTML w i
laboratory on { preset, scene } =
  HH.div [ cls "lab" ]
    [ HH.div [ cls "labgrid" ]
        ( [ onsets ]
            <> (sections <#> numbers)
            <> [ tape, rules, progression, query ]
        )
    , HH.details [ cls "wire" ]
        [ HH.summary_ [ HH.text "the scene, as it goes to the rig" ]
        , HH.pre_ [ HH.text scene ]
        ]
    ]
  where
  spec = preset.line.spec
  pinned = preset.line.knobs <#> _.parameter

  cls :: forall r j. String -> HP.IProp (class :: String | r) j
  cls = HP.class_ <<< HH.ClassName

  panel :: String -> String -> Array (HH.HTML w i) -> HH.HTML w i
  panel title about body =
    HH.section [ cls "labpanel" ]
      ([ HH.h2_ [ HH.text title ] ] <> (if about == "" then [] else [ HH.p [ cls "why" ] [ HH.text about ] ]) <> body)

  row :: String -> Array (HH.HTML w i) -> HH.HTML w i
  row label controls = HH.div [ cls "labrow" ] ([ HH.label_ [ HH.text label ] ] <> controls)

  -- ── the numbers, from the catalogue ────────────────────────────────────────

  sections = [ Grain, ReadHead, Walk, Swing, Voice, Sends, GrainEffects, CloudEffects ]

  numbers section =
    panel (capitalise (sectionName section)) ""
      (filter (\p -> (describe p).section == section) allParameters <#> number)

  number p =
    let
      d = describe p
      value = read p spec
      isPinned = elem p pinned
    in
      HH.div [ cls ("labrow" <> (if engaged p spec then "" else " idle")), HP.title d.about ]
        [ HH.label_ [ HH.text d.label ]
        , HH.input
            [ HP.type_ HP.InputRange
            , HP.min 0.0
            , HP.max 1.0
            , HP.step (HP.Step 0.001)
            , HP.value (show (normalise p value))
            , HE.onValueInput \s -> maybe on.nothing (on.setParameter p <<< denormalise p) (Number.fromString s)
            ]
        , HH.output_ [ HH.text (format p value) ]
        , HH.button
            [ cls (if isPinned then "pin on" else "pin")
            , HP.title (if isPinned then "on the module's panel: click to take it off" else "put on the module's panel as a knob")
            , HP.disabled (not isPinned && length pinned >= 4)
            , HE.onClick \_ -> on.pin p
            ]
            [ HH.text (if isPinned then "●" else "○") ]
        ]

  -- ── onsets ─────────────────────────────────────────────────────────────────

  onsets =
    let
      layout = onsetLayout spec.onsets
      mode = case layout of
        EvenGrains _ -> "even"
        Euclidean _ _ -> "euclid"
        Placed _ -> "manual"
      setOnsets os = on.editSpec _ { onsets = os }
    in
      panel "Onsets" "When grains fall in the bar. Each is a grain with an ordinal, which is what a rule counts."
        ( [ row "shape"
              [ HH.select
                  [ HE.onValueChange \m -> case m of
                      "even" -> setOnsets (even (max 1 (length spec.onsets)))
                      "euclid" -> setOnsets (euclid 5 16)
                      _ -> setOnsets spec.onsets
                  ]
                  [ option "even" "even — n a bar" mode, option "euclid" "euclid (k, n)" mode, option "manual" "placed by hand" mode ]
              ]
          ]
            <> case layout of
              EvenGrains k -> [ row "grains" [ integer k 1 64 \v -> setOnsets (even v) ] ]
              Euclidean k n ->
                [ row "hits" [ integer k 1 n \v -> setOnsets (euclid v n) ]
                , row "steps" [ integer n 2 32 \v -> setOnsets (euclid (min k v) v) ]
                ]
              Placed os ->
                [ row "positions"
                    [ text (intercalate ", " (os <#> trimmed 4)) "0, 0.25, 0.5, 0.75" \s ->
                        let
                          parsed = catMaybes (String.split (String.Pattern ",") s <#> (Number.fromString <<< String.trim))
                        in
                          setOnsets (filter (\x -> x >= 0.0 && x < 1.0) parsed)
                    ]
                ]
            <> [ HH.div [ cls "onsetline" ] (spec.onsets <#> \x -> HH.i [ HP.style ("left:" <> trimmed 2 (x * 100.0) <> "%") ] []) ]
        )

  -- ── the tape's structure ───────────────────────────────────────────────────

  tape =
    panel "Tape and time" "A tape can be several bars; each cycle reads the next, or the order says which. The step table is Sector's jumps: one token a step, ~ stays, 5 jumps to step 5, 5?0.4 does so with probability 0.4."
      [ row "bars" [ integer spec.tape.bars 1 64 \v -> on.editSpec _ { tape { bars = v } } ]
      , row "bar order" [ text (ints spec.tape.order) "0 1 1 0" \s -> on.editSpec _ { tape { order = parseInts s } } ]
      , row "bar samples" [ text (ints spec.tape.samples) "4 7 5 0" \s -> on.editSpec _ { tape { samples = parseInts s } } ]
      , row "step table"
          [ text (if null spec.steps.to then "" else stepsText spec.steps) "~ ~ 5?0.4 ~" \s ->
              maybe on.nothing (\t -> on.editSpec _ { steps = t }) (hush (stepTable s))
          ]
      , row "warp"
          [ HH.select [ HE.onValueChange \m -> on.editSpec _ { warp { mode = warpOf m } } ]
              [ option "gap" "gap" (warpName spec.warp.mode), option "repitch" "repitch" (warpName spec.warp.mode), option "leak" "leak" (warpName spec.warp.mode) ]
          ]
      , row "walk grid"
          [ HH.select [ HE.onValueChange \m -> maybe on.nothing (\g -> on.editSpec _ { walk { grid = g } }) (Int.fromString m) ]
              ([ 32, 16, 8, 4, 2 ] <#> \g -> option (show g) (gridName g) (show spec.walk.grid))
          ]
      , row "swing on"
          [ HH.select [ HE.onValueChange \m -> maybe on.nothing (\g -> on.editSpec _ { swing { grid = g } }) (Int.fromString m) ]
              ([ 16, 8 ] <#> \g -> option (show g) (gridName g) (show spec.swing.grid))
          ]
      , row "orbit" [ integer spec.chain.orbit 0 11 \v -> on.editSpec _ { chain { orbit = v } } ]
      ]

  -- ── rules ──────────────────────────────────────────────────────────────────

  rules =
    panel "Rules" "A grain is an event, so it has an ordinal, and a rule can count it. Speed, gain and length multiply; the effects set."
      ( (mapWithIndex ruleRow (spec.rules <#> toWireRule))
          <>
            [ HH.button
                [ cls "add"
                , HE.onClick \_ -> on.editSpec \s -> s { rules = snoc s.rules (fromWireRule { when: 1, everyN: 3, everyK: 0, chance: 0.5, op: 0, amount: -1.0, values: [], step: 0 }) }
                ]
                [ HH.text "+ rule" ]
            ]
      )

  ruleRow i r =
    let
      edit f = on.editSpec \s -> s { rules = fromMaybe s.rules (modifyAt i (\old -> fromWireRule (f (toWireRule old))) s.rules) }
    in
      HH.div [ cls "rule" ]
        [ HH.select [ HE.onValueChange \m -> maybe on.nothing (\w -> edit _ { when = w }) (Int.fromString m) ]
            ([ Tuple' 0 "always", Tuple' 1 "every", Tuple' 2 "chance", Tuple' 3 "hit" ] <#> \(Tuple' v l) -> option (show v) l (show r.when))
        , if r.when == 3 then
            HH.select [ HE.onValueChange \m -> maybe on.nothing (\k -> edit _ { everyN = k }) (Int.fromString m) ]
              ([ Tuple' 0 "kick", Tuple' 1 "snare", Tuple' 2 "hat" ] <#> \(Tuple' v l) -> option (show v) l (show r.everyN))
          else small (show r.everyN) (r.when /= 1) "n" \s -> maybe on.nothing (\v -> edit _ { everyN = v }) (Int.fromString s)
        , small (show r.everyK) (r.when /= 1) "from" \s -> maybe on.nothing (\v -> edit _ { everyK = v }) (Int.fromString s)
        , small (trimmed 3 r.chance) (r.when < 2) (if r.when == 3 then "threshold" else "chance") \s -> maybe on.nothing (\v -> edit _ { chance = v }) (Number.fromString s)
        , HH.select [ HE.onValueChange \m -> maybe on.nothing (\o -> edit _ { op = o }) (Int.fromString m) ]
            (mapWithIndex (\j name -> option (show j) name (show r.op)) opNames)
        , small (trimmed 3 r.amount) (not (null r.values)) "amount" \s -> maybe on.nothing (\v -> edit _ { amount = v }) (Number.fromString s)
        , HH.input
            [ cls "values"
            , HP.value (valuesText r)
            , HP.placeholder "values"
            , HP.title "a sequence instead of the amount: \"36 36 39 31\" steps a hit at a time, \"<36 36 39 31>\" a bar at a time"
            , HE.onValueChange \s -> edit \w -> w { values = numbersIn s, step = if String.contains (String.Pattern "<") s then 1 else 0 }
            ]
        , HH.button [ cls "kill", HP.title "remove", HE.onClick \_ -> on.editSpec \s -> s { rules = fromMaybe s.rules (deleteAt i s.rules) } ] [ HH.text "×" ]
        ]

  valuesText :: WireRule -> String
  valuesText r
    | null r.values = ""
    | r.step == 1 = "<" <> intercalate " " (r.values <#> trimmed 3) <> ">"
    | otherwise = intercalate " " (r.values <#> trimmed 3)

  -- ── the progression ────────────────────────────────────────────────────────

  progression =
    let
      p = spec.progression
    in
      panel "Progression" "A chord a cycle. The cloud keeps to samples that fit it, and the resonators can follow it."
        [ row "chords"
            [ text (intercalate " " (p.chords <#> \c -> fromMaybe "?" (nameOfChord c))) "Am F C G" \s ->
                maybe on.nothing (\cs -> on.editSpec _ { progression { chords = cs } }) (hush (chordList s))
            ]
        , row "fit at least" [ decimal p.minimumFit \v -> on.editSpec _ { progression { minimumFit = v } } ]
        , row "strength" [ decimal p.strength \v -> on.editSpec _ { progression { strength = v } } ]
        , row "resonators"
            [ HH.select [ HE.onValueChange \m -> on.editSpec _ { progression { follow = followOf m } } ]
                [ option "off" "use their own pitch" (followName p.follow)
                , option "root" "follow the root" (followName p.follow)
                , option "bass" "follow the bass" (followName p.follow)
                , option "tones" "play the chord's tones" (followName p.follow)
                ]
            ]
        ]

  query =
    let
      q = preset.query
      parts = catMaybes
        [ if null q.clauses then Nothing else Just (show (length q.clauses) <> (if length q.clauses == 1 then " clause" else " clauses"))
        , q.weighting <#> const "a weighting"
        , q.harmonic <#> const "a harmonic target"
        ]
    in
      panel "Query" "Which samples the cloud draws from. A preset carries its query beside its line."
        [ HH.p [ cls "hint" ] [ HH.text (if null parts then "None: every sample in the material." else "This preset's query has " <> intercalate ", " parts <> ".") ] ]

  -- ── controls ───────────────────────────────────────────────────────────────

  option value label current = HH.option [ HP.value value, HP.selected (value == current) ] [ HH.text label ]

  integer value low high set =
    HH.input
      [ HP.type_ HP.InputNumber
      , HP.value (show value)
      , HP.min (Int.toNumber low)
      , HP.max (Int.toNumber high)
      , HE.onValueChange \s -> maybe on.nothing (set <<< clamp low high) (Int.fromString s)
      ]

  decimal value set =
    -- Text, not a number field: a number field shows 0.5 as "0,5" in some
    -- locales, and the line never does.
    HH.input
      [ HP.value (trimmed 3 value)
      , HE.onValueChange \s -> maybe on.nothing set (Number.fromString s)
      ]

  text value placeholder set =
    HH.input [ HP.value value, HP.placeholder placeholder, HE.onValueChange set ]

  small value disabled title set =
    HH.input [ cls "small", HP.value value, HP.disabled disabled, HP.title title, HE.onValueChange set ]

data Tuple' = Tuple' Int String

ints :: Array Int -> String
ints = intercalate " " <<< map show

parseInts :: String -> Array Int
parseInts s = catMaybes (words s <#> Int.fromString)

numbersIn :: String -> Array Number
numbersIn s = catMaybes (words (replace "<" " " (replace ">" " " (replace "," " " s))) <#> Number.fromString)

words :: String -> Array String
words = filter (_ /= "") <<< String.split (String.Pattern " ")

replace :: String -> String -> String -> String
replace from to = String.replaceAll (String.Pattern from) (String.Replacement to)

warpName :: WarpMode -> String
warpName = case _ of
  Repitch -> "repitch"
  Gap -> "gap"
  Leak -> "leak"

warpOf :: String -> WarpMode
warpOf = case _ of
  "repitch" -> Repitch
  "leak" -> Leak
  _ -> Gap

followName :: ResonatorFollow -> String
followName = case _ of
  NoFollow -> "off"
  FollowRoot -> "root"
  FollowBass -> "bass"
  FollowChordTones -> "tones"

followOf :: String -> ResonatorFollow
followOf = case _ of
  "root" -> FollowRoot
  "bass" -> FollowBass
  "tones" -> FollowChordTones
  _ -> NoFollow

gridName :: Int -> String
gridName = case _ of
  32 -> "32nds"
  16 -> "16ths"
  8 -> "8ths"
  4 -> "beats"
  2 -> "halves"
  g -> show g

capitalise :: String -> String
capitalise s = String.toUpper (String.take 1 s) <> String.drop 1 s
