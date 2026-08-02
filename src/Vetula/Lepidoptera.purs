-- | `Vetula.Lepidoptera` — the Perform surface as one transferable document.
-- |
-- | Where `Vetula.Tidal` serialises a single *progression* (one caught-chord path
-- | as `note "<…>"`), this serialises the whole **Perform surface**: the set of
-- | parallel voices, each a source-reference + a mini-notation head + a transform
-- | stack + a terminal. That makes a live Perform configuration a named, saveable
-- | *scene* — the Amphora-storable "the file is the music" — and the atom a future
-- | macro/arrangement layer sequences (verse / chorus / head-fugue).
-- |
-- | The document shape (Tier 2 of the convergence — see the Lepidoptera memory):
-- |
-- |   -- vetula perform · C major · 3 voices
-- |   source A "<[c5,e5,g5] [a4,c5,e5] [d5,f5,a5] [g4,b4,d5]>"
-- |   source B "<[c5,e5,g5,b5,d6] …>"
-- |   ch1 A "0 1 2 3" # voice open # strum 14
-- |   ch2 B "0 1 2 3" # arpup 4 # every 4
-- |   ch5 - "" # transpose 12 # out rig
-- |
-- | Two principles from the design conversation are baked in:
-- |
-- |   * **Sources are plural and named; sharing is a coincidence of reference.**
-- |     A voice names a source; two voices that name the same one share harmony,
-- |     and re-harmonising = editing one table entry. Distinct/inline sources give
-- |     the "different chords per voice" cases (plain vs substitution sibling,
-- |     different keys). "Set all at once" is a repoint, never a baseline share.
-- |   * **One grammar across the ecosystem.** The `"head" # verb arg` lane is
-- |     parsed by `Triggerfish.Macro.parseLane` verbatim (the arrangement layer's
-- |     grammar). Vetula owns only the *vocabulary* — the `(verb,arg) -> PerfFx`
-- |     table — which is the right seam. This forced the (b)-tasteful choice:
-- |     arp's direction fuses into the verb (`arpup`/`arpdown`/`arpupdown`) so
-- |     every layer is a single-arg mod, and `every N` is a trailing mod that
-- |     binds to the preceding layer. `out <term>` / `mute` ride the same grammar.
-- |
-- | Total + lenient (Selene `Source.purs` discipline): unknown mods drop, partial
-- | ones fall back to defaults, values clamp to the chip-nudge ranges. Aim:
-- |   parsePerform (performSource doc) == doc  (modulo trim + source-name canon).
-- |
-- | NOTE (follow-up): this imports the Perform types from `Vetula.App`. Wiring a
-- | save button *into* App will need App to import this — a cycle — so first
-- | extract the pure Perform types (PerfFx/Layer/PerfTerm/…) into a shared module.
-- | Deferred deliberately; tonight is additive and leaves App untouched.
module Vetula.Lepidoptera
  ( NamedSource
  , VoiceEntry
  , PerfDoc
  , performSource
  , parsePerform
  , docFromBoxes
  , roundTrips
  ) where

import Prelude

import Data.Array (drop, filter, find, index, length, mapMaybe, mapWithIndex, null, snoc, updateAt, (!!))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.String.Common (joinWith, split, trim)
import Triggerfish.Macro (Arg(..), Form(..), parseLane, tokenize)
import Vetula.App (ArpDir(..), Layer, PerfBox, PerfFx(..), PerfSel(..), PerfTerm(..), VoiceShape(..), When(..), mkLayer, parseVoiceShape, printArpDir, printVoiceShape, termShort)
import Vetula.Tidal (parseProgression, tidalNoteName)

-- | A named chord set the voices reference. `chords` are note-lists in *stored
-- | order* (NOT sorted — voicing order drives arp direction, so it must survive).
type NamedSource = { name :: String, chords :: Array (Array Int) }

-- | One voice of the surface — the serialisable core of a `PerfBox` (drop the
-- | live-trace provenance / glyph; keep the harmonic content by reference).
type VoiceEntry =
  { channel :: Int
  , source  :: Maybe String   -- name into the source table; Nothing = sourceless
  , seqText :: String         -- the mini-notation head (indices into the source)
  , stack   :: Array Layer    -- the transform stack (fx + when)
  , term    :: PerfTerm       -- terminal sink
  , muted   :: Boolean
  }

-- | A whole Perform surface as one document.
type PerfDoc =
  { key     :: String
  , sources :: Array NamedSource
  , voices  :: Array VoiceEntry
  }

-- ============================================================================
-- Print
-- ============================================================================

performSource :: PerfDoc -> String
performSource doc = joinWith "\n" (header <> map printSource doc.sources <> map printVoice doc.voices)
  where
  header = [ "-- vetula perform · " <> doc.key <> " · " <> show (length doc.voices) <> " voices" ]

printSource :: NamedSource -> String
printSource s = "source " <> s.name <> " " <> quote ("<" <> joinWith " " (map bracket s.chords) <> ">")
  where
  bracket c = "[" <> joinWith "," (map tidalNoteName c) <> "]"

printVoice :: VoiceEntry -> String
printVoice v = joinWith " " ([ "ch" <> show v.channel, srcTok, quote v.seqText ] <> hashed directives)
  where
  srcTok = fromMaybe "-" v.source
  directives = bind v.stack layerDirs <> termDir <> muteDir
  termDir = case v.term of
    TMidi -> []
    t -> [ "out " <> termShort t ]
  muteDir = if v.muted then [ "mute" ] else []
  -- interleave a `#` before each directive: [d1,d2] -> ["#",d1,"#",d2]
  hashed ds = bind ds \d -> [ "#", d ]

-- | A layer prints to its fx directive, plus an `every N` directive when gated.
layerDirs :: Layer -> Array String
layerDirs lyr = [ fxDir lyr.fx ] <> whenDir lyr.when

whenDir :: When -> Array String
whenDir = case _ of
  Always -> []
  Every n -> [ "every " <> show n ]

-- | The single-arg mod vocabulary. Arp's direction is FUSED into the verb so the
-- | mod stays `verb arg` (Macro's grammar is strictly one arg). Use plain `show`
-- | (a leading '+' breaks `Int.fromString`).
fxDir :: PerfFx -> String
fxDir = case _ of
  Transpose n -> "transpose " <> show n
  Octave n -> "oct " <> show n
  Rate n -> "rate " <> show n
  Voice shape -> "voice " <> printVoiceShape shape
  Select (High n) -> "top " <> show n
  Select (Low n) -> "bottom " <> show n
  Arpg dir r -> "arp" <> printArpDir dir <> " " <> show r
  Strum ms -> "strum " <> show ms

quote :: String -> String
quote x = "\"" <> x <> "\""

-- ============================================================================
-- Parse — total + lenient
-- ============================================================================

parsePerform :: String -> PerfDoc
parsePerform text =
  { key: findKey lines
  , sources: mapMaybe parseSourceLine kept
  , voices: mapMaybe parseVoiceLine kept
  }
  where
  lines = map trim (split (Pattern "\n") text)
  kept = filter (\l -> l /= "" && not (isComment l)) lines
  isComment l = stripPrefix (Pattern "--") l /= Nothing

-- | Pull the key out of the `-- vetula perform · KEY · N voices` header.
findKey :: Array String -> String
findKey lines = fromMaybe "" (find isHeader lines >>= keyOf)
  where
  isHeader l = stripPrefix (Pattern "-- vetula perform") l /= Nothing
  keyOf l = map trim (index (split (Pattern "·") l) 1)

parseSourceLine :: String -> Maybe NamedSource
parseSourceLine l = case tokenize l of
  toks | (toks !! 0) == Just "source" ->
    case toks !! 1 of
      Just name -> Just { name, chords: parseProgression l }
      Nothing -> Nothing
  _ -> Nothing

parseVoiceLine :: String -> Maybe VoiceEntry
parseVoiceLine l =
  let toks = tokenize l
  in case toks !! 0 >>= parseCh of
       Nothing -> Nothing
       Just channel ->
         let src = case toks !! 1 of
               Just "-" -> Nothing
               Just s -> Just s
               Nothing -> Nothing
             laneStr = joinWith " " (drop 2 toks)
             folded = foldMods (firstStepMods laneStr)
         in Just
              { channel
              , source: src
              , seqText: firstStepHead laneStr
              , stack: folded.stack
              , term: folded.term
              , muted: folded.muted
              }

parseCh :: String -> Maybe Int
parseCh t = stripPrefix (Pattern "ch") t >>= Int.fromString

-- The head (mini-notation) of the lane's first (and, for a Perform voice, only)
-- step. Empty when the lane is empty or the head is a rest/alt (kept lenient).
firstStepHead :: String -> String
firstStepHead laneStr = case parseLane laneStr of
  steps -> case steps !! 0 of
    Just step -> formHead step.form
    Nothing -> ""

firstStepMods :: String -> Array { verb :: String, arg :: Arg }
firstStepMods laneStr = case (parseLane laneStr) !! 0 of
  Just step -> step.mods
  Nothing -> []

formHead :: Form -> String
formHead = case _ of
  FName n -> n
  FRest -> "~"
  FAlt inner -> "<" <> joinWith " " inner <> ">"

-- | Fold the step's mods into (stack, term, muted). `every` binds to the layer
-- | most recently pushed; `out`/`mute` set the terminal/mute; a recognised fx
-- | verb pushes a layer; anything else drops (lenient).
foldMods :: Array { verb :: String, arg :: Arg } -> { stack :: Array Layer, term :: PerfTerm, muted :: Boolean }
foldMods = foldl step { stack: [], term: TMidi, muted: false }
  where
  step acc m =
    let arg = argStr m.arg
    in case m.verb of
         "every" -> acc { stack = setLastWhen (Every (max 1 (argInt 2 arg))) acc.stack }
         "out" -> acc { term = parseTerm arg }
         "mute" -> acc { muted = true }
         _ -> case fxOfVerb m.verb arg of
                Just fx -> acc { stack = snoc acc.stack (mkLayer fx) }
                Nothing -> acc

setLastWhen :: When -> Array Layer -> Array Layer
setLastWhen w stack =
  let n = length stack
  in case stack !! (n - 1) of
       Just lyr -> fromMaybe stack (updateAt (n - 1) (lyr { when = w }) stack)
       Nothing -> stack

fxOfVerb :: String -> String -> Maybe PerfFx
fxOfVerb verb arg = case verb of
  "transpose" -> Just (Transpose (clamp (-24) 24 (argInt 0 arg)))
  "trans" -> Just (Transpose (clamp (-24) 24 (argInt 0 arg)))
  "oct" -> Just (Octave (clamp (-4) 4 (argInt 0 arg)))
  "octave" -> Just (Octave (clamp (-4) 4 (argInt 0 arg)))
  "8ve" -> Just (Octave (clamp (-4) 4 (argInt 0 arg)))
  "rate" -> Just (Rate (clamp (-8) 8 (argInt 2 arg)))
  "voice" -> Just (Voice (fromMaybe Open (parseVoiceShape arg)))
  "top" -> Just (Select (High (clamp 1 6 (argInt 1 arg))))
  "bottom" -> Just (Select (Low (clamp 1 6 (argInt 1 arg))))
  "arpup" -> Just (Arpg ArpUp (clamp 1 16 (argInt 4 arg)))
  "arpdown" -> Just (Arpg ArpDown (clamp 1 16 (argInt 4 arg)))
  "arpupdown" -> Just (Arpg ArpUpDown (clamp 1 16 (argInt 4 arg)))
  "strum" -> Just (Strum (clamp 0 80 (argInt 14 arg)))
  _ -> Nothing

parseTerm :: String -> PerfTerm
parseTerm = case _ of
  "rig" -> TRig
  "odo" -> TOdo
  _ -> TMidi

argStr :: Arg -> String
argStr = case _ of
  Lit s -> s
  AltArg xs -> fromMaybe "" (xs !! 0)

-- | One integer token, lenient: strip a leading '+' (which `fromString` rejects),
-- | fall back to `def` on anything non-numeric.
argInt :: Int -> String -> Int
argInt def s = fromMaybe def (Int.fromString (fromMaybe s (stripPrefix (Pattern "+") s)))

-- ============================================================================
-- Build a document from live boxes (the App-facing entry; the inverse — minting
-- SavedSeqs with synthesised events/glyphs — lives App-side and is deferred).
-- ============================================================================

docFromBoxes :: String -> Array PerfBox -> PerfDoc
docFromBoxes key boxes =
  { key
  , sources: named
  , voices: map toVoice boxes
  }
  where
  -- distinct non-empty chord sets across all boxes, in first-seen order → named
  distinct = foldl (\acc c -> if null c || contains acc c then acc else snoc acc c) [] (map boxChords boxes)
  named = mapWithIndex (\i c -> { name: srcName i, chords: c }) distinct
  contains acc c = isJustArr (find (_ == c) acc)
  toVoice box =
    let c = boxChords box
    in { channel: box.channel
       , source: if null c then Nothing else map _.name (find (\ns -> ns.chords == c) named)
       , seqText: trim box.seqText
       , stack: box.stack
       , term: box.term
       , muted: box.muted
       }

boxChords :: PerfBox -> Array (Array Int)
boxChords box = maybe [] (\s -> map _.notes s.events) box.seq

isJustArr :: forall a. Maybe a -> Boolean
isJustArr = case _ of
  Just _ -> true
  Nothing -> false

-- Source names: A…Z, then S27, S28, … (rarely reached — few voices).
srcName :: Int -> String
srcName i = fromMaybe ("S" <> show i) (index letters i)
  where
  letters =
    [ "A","B","C","D","E","F","G","H","I","J","K","L","M"
    , "N","O","P","Q","R","S","T","U","V","W","X","Y","Z" ]

-- ============================================================================
-- Round-trip self-check (type-checks that Eq is available; run in a harness).
-- ============================================================================

-- | The reconciliation invariant, as a Boolean over a document. `performSource`
-- | then `parsePerform` should return the same document (source names are already
-- | canonical A…Z; seqText is trimmed on the way in). Not run at build time —
-- | drop tokens on the surface and eyeball, or wire a test harness.
roundTrips :: PerfDoc -> Boolean
roundTrips doc = parsePerform (performSource doc) == doc
