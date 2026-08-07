-- | `Triggerfish.Balistes.TriSnapshot` — a snapshot that can hold the playing state
-- | of ANY of the three Balistes brains, so one snapshot bank sequences Grids,
-- | Grids and Tidal states intermingled (the macro-tidal surface). See
-- | `docs/DESIGN-tri-snapshot.md`.
-- |
-- | Today's snapshot is Grids-only (`Triggerfish.Balistes.Model.Snapshot`, the
-- | Grids control point). This generalises it: one constructor per brain, each
-- | carrying the whole restorable artefact — not a reference — so a snapshot
-- | survives library edits and can be pushed to the rig verbatim (the same
-- | discipline the reef handoffs use: send the whole FixedPattern / TrigKit).
-- |
-- | This module is the pure MODEL foundation (slice 1): the type + brain tags +
-- | pure describe/badge helpers. Capture and recall touch component `State` /
-- | `HalogenM`, so they live in `Triggerfish.Balistes.Component` (slices 2–3).
module Triggerfish.Balistes.TriSnapshot
  ( TriSnapshot(..)
  , Brain(..)
  , brainOf
  , brainBadge
  , brainLabel
  , describeTri
  , printTri
  , parseTri
  , rhythmContent
  , rhythmOfContent
  ) where

import Prelude

import Data.Array (drop, elemIndex, filter, length, mapMaybe, take, uncons)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith, split, trim)
import Data.String.Pattern (Pattern(..))
import Triggerfish.Balistes.Lepidoptera (parsePattern, printPattern)
import Triggerfish.Balistes.Model (Snapshot, TrigBank)
import Triggerfish.Balistes.Pattern (FixedPattern, usedLanes)

-- | A captured playing-state, tagged by which brain took it.
-- |
-- |   • `TSGrids` — the Grids control point (the existing `Snapshot`: X/Y +
-- |     densities + randomness + open + push). The Grids engine regenerates its
-- |     pattern from this, so it's the natural restorable unit.
-- |   • `TSFixed` — a whole RYTM rhythm (the rich `FixedPattern`, self-contained,
-- |     NOT a `library !! i` index — it must survive library reordering).
-- |   • `TSTrig` — a whole TIDAL POLYTRIG rack (`TrigBank`: named jacks + routes).
data TriSnapshot
  = TSGrids Snapshot
  | TSFixed FixedPattern
  | TSTrig TrigBank

derive instance eqTriSnapshot :: Eq TriSnapshot

-- | The three brains, as a small closed tag (used for badges / tab-switching on
-- | recall). Mirrors `Component.Active` minus the AFixed library index — a
-- | snapshot carries its own pattern, so the index is irrelevant.
data Brain = BGrids | BFixed | BTrig

derive instance eqBrain :: Eq Brain

-- | Which brain a snapshot belongs to (recall switches the active tab to this).
brainOf :: TriSnapshot -> Brain
brainOf = case _ of
  TSGrids _ -> BGrids
  TSFixed _ -> BFixed
  TSTrig _ -> BTrig

-- | The one-letter engraved badge shown on a filled slot. DISPLAY ONLY — this is
-- | not the wire tag (see `printTri`), which is frozen at M/G/T for stored data.
brainBadge :: Brain -> String
brainBadge = case _ of
  BGrids -> "G"
  BFixed -> "R"
  BTrig -> "T"

-- | The brain's display name, for tooltips / the sequence rail.
brainLabel :: Brain -> String
brainLabel = case _ of
  BGrids -> "GRIDS"
  BFixed -> "RYTM"
  BTrig -> "TIDAL"

-- | A compact human description of a snapshot's contents (for a slot's title
-- | attribute): the brain + a size cue (used-lane count for a rhythm, jack count
-- | for a rack, the X/Y cursor for a Grids point).
describeTri :: TriSnapshot -> String
describeTri = case _ of
  TSGrids snap -> "GRIDS · x" <> show snap.x <> " y" <> show snap.y
  TSFixed pat -> "RYTM · " <> show (length (usedLanes pat)) <> " lanes"
  TSTrig rack -> "TIDAL · " <> show (length rack.jacks) <> " jacks"

-- ---------------------------------------------------------------------------
-- Text codec (slice 5) — the transferable, localStorage-friendly form.
--
-- A snapshot serialises as `<tag-line>\n<payload>`: a one-char brain tag (the
-- same M/G/T badges) picks the constructor, the rest is TEXT — the Lepidoptera
-- "save the rendering, not bespoke structure" rule. `TSFixed` reuses the eDSL
-- (`printPattern`, so a slot drops straight into Calypso); `TSGrids`/`TSTrig`
-- get compact line forms. Parse is total + lenient (`Nothing` on malformed
-- input → the Store drops the slot), mirroring `parsePattern`.
-- ---------------------------------------------------------------------------

-- | **The M/G/T tags are FROZEN and no longer match the brains' names.** They
-- | date from when `BGrids` was displayed as "MUTABLE" and `BFixed` as "GRIDS";
-- | the 2026-08-06 rename made those GRIDS and RYTM (see `brainLabel`), but these
-- | are the on-disk discriminator for every snapshot already in localStorage and
-- | in Amphora. Renaming them to G/R/T would silently fail to parse every stored
-- | slot — `parseTri` is lenient, so the Store would just drop them. Leave them.
printTri :: TriSnapshot -> String
printTri = case _ of
  TSGrids s -> "M\n" <> printSnap s
  TSFixed p -> "G\n" <> printPattern p
  TSTrig r -> "T\n" <> printTrig r

parseTri :: String -> Maybe TriSnapshot
parseTri text = case uncons (split (Pattern "\n") text) of
  Just { head, tail } ->
    let body = joinWith "\n" tail
    in case trim head of
      "M" -> TSGrids <$> parseSnap body
      "G" -> TSFixed <$> parsePattern body
      "T" -> TSTrig <$> parseTrig body
      _ -> Nothing
  Nothing -> Nothing

-- The Grids control point: seven space-joined ints, then ` | ` and the push
-- overlay (a variable-length int array).
printSnap :: Snapshot -> String
printSnap s =
  joinWith " " (map show [ s.x, s.y, s.densBd, s.densSd, s.densHh, s.randomness, s.open ])
    <> " | " <> joinWith " " (map show s.push)

parseSnap :: String -> Maybe Snapshot
parseSnap body = case split (Pattern " | ") body of
  [ headPart, pushPart ] -> case mapMaybe fromString (words headPart) of
    [ x, y, densBd, densSd, densHh, randomness, open ] ->
      Just
        { x, y, densBd, densSd, densHh, randomness, open
        , push: mapMaybe fromString (words pushPart)
        }
    _ -> Nothing
  _ -> Nothing

-- The POLYTRIG rack: one tab-delimited jack per line (`name<TAB>note<TAB>src`,
-- so a Tidal source's spaces survive), a `--routes--` marker, then one route
-- per line. Tabs never occur in mini-notation, so they're a safe field split.
printTrig :: TrigBank -> String
printTrig tb =
  joinWith "\n" (map jackLine tb.jacks)
    <> "\n--routes--"
    <> joinWith "" (map (\r -> "\n" <> r) tb.routes)
  where
  jackLine j = j.name <> "\t" <> show j.note <> "\t" <> j.source

parseTrig :: String -> Maybe TrigBank
parseTrig body =
  let ls = split (Pattern "\n") body
  in case elemIndex "--routes--" ls of
    Just i -> Just
      { jacks: mapMaybe parseJack (take i ls)
      , routes: filter (_ /= "") (drop (i + 1) ls)
      }
    Nothing -> Just { jacks: mapMaybe parseJack ls, routes: [] }
  where
  parseJack line = case split (Pattern "\t") line of
    [ nm, noteS, src ] -> fromString noteS <#> \note -> { name: nm, note, source: src }
    _ -> Nothing

-- Split on spaces, dropping the empty runs a leading/collapsed space would make.
words :: String -> Array String
words = filter (_ /= "") <<< split (Pattern " ")

-- ---------------------------------------------------------------------------
-- The rhythm name seam
-- ---------------------------------------------------------------------------
--
-- A rhythm's canonical text embeds its name (`balistesPattern "lo house 110" 32`),
-- but the BANK carries names in its envelope. Storing both would give one rhythm
-- two names that drift on rename, so bank content is NAME-STRIPPED and the name is
-- injected back whenever a pattern is handed out — to the editor, to Amphora, to
-- Calypso. The envelope name is the single source of truth.
--
-- Two consequences, both wanted: the glyph fingerprints the SOUND (renaming no
-- longer changes the content hash, so "identical state => identical glyph" is at
-- last true for rhythms as the bank always claimed), and there is one rename path
-- for all three brains.
--
-- Verified against all 14 published rhythms: parse -> strip -> print -> parse ->
-- re-inject reproduces the original text byte-for-byte.

-- | A rhythm's bank content: the `TSFixed` wire tag plus its name-stripped text.
-- | NB the tag is the frozen `G`, which means RYTM, not GRIDS (see printTri).
rhythmContent :: FixedPattern -> String
rhythmContent p = "G\n" <> printPattern (p { name = "" })

-- | Recover a rhythm from bank content with `name` injected from the envelope.
-- | `Nothing` when the content belongs to another brain or does not parse. Uses
-- | the same tag-line-then-payload split as `parseTri`, so the two agree.
rhythmOfContent :: String -> String -> Maybe FixedPattern
rhythmOfContent name content = case uncons (split (Pattern "\n") content) of
  Just { head, tail } | trim head == "G" ->
    (\p -> p { name = name }) <$> parsePattern (joinWith "\n" tail)
  _ -> Nothing
