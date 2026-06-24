-- | Triggerfish.Balistes.Source — the round-trip between module state and a
-- | purerl-tidal `balistes { … }` cell. Two halves, split by who authors them:
-- |
-- |   • `headerText` — the REFLECTIVE half. Everything authored by direct
-- |     manipulation (X/Y/densities/randomness from the pad+knobs, push from the
-- |     groove knobs, ratchets from Alt-drag, clicks from tapping a pad lane).
-- |     Rendered read-only above the editor; it is never parsed back, so a knob
-- |     drag can reflow it freely without ever touching what you've typed.
-- |   • `bodyText` / `parseBody` — the EDITABLE half. The typed lane sources and
-- |     routing patterns, line-oriented so commenting a line (`--`) mutes it.
-- |     `bodyText` prints the model; `parseBody` reads an edited document back
-- |     into the model. The textarea stores your text verbatim (we never
-- |     reformat on input), so this is the instrument's primary surface.
-- |
-- | Document grammar (one statement per line):
-- |   <label>  "<mini-notation>"     -- set lane <label>'s source
-- |   route    "<mini-notation>"     -- a routing pattern (atoms route by name)
-- |   -- anything                    -- a comment / muted line
-- | A lane not mentioned (or commented out) has no source this cycle.
module Triggerfish.Balistes.Source
  ( headerText
  , bodyText
  , parseBody
  , starterDoc
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, filter, mapMaybe, null, range, replicate)
import Data.Foldable (any, foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.Common (joinWith, toLower)
import Data.Tuple (Tuple(..))
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Tidal as Tidal

-- ---------------------------------------------------------------------------
-- Header — the reflective half (read-only; never parsed back)
-- ---------------------------------------------------------------------------

headerText :: M.Balistes -> String
headerText b =
  joinWith "\n" (filter (_ /= "") [ configBlock b, grooveBlock b, ratchetBlock b, clickBlock b ])

-- The faithful firmware config — what the BEAM `balistes` cell already takes.
configBlock :: M.Balistes -> String
configBlock b =
  "balistes \"kit\" iac 10 $ balistesConfig\n"
    <> "  { x          = pure " <> show b.x <> "\n"
    <> "  , y          = pure " <> show b.y <> "\n"
    <> "  , fillBd     = pure " <> show b.densBd <> "\n"
    <> "  , fillSd     = pure " <> show b.densSd <> "\n"
    <> "  , fillHh     = pure " <> show b.densHh <> "\n"
    <> "  , randomness = pure " <> show b.randomness <> "\n"
    <> "  }"

signed :: Int -> String
signed n = if n > 0 then "+" <> show n else show n

-- Per-lane timing push (Triggerfish extension).
grooveBlock :: M.Balistes -> String
grooveBlock b =
  let bd = M.pushOf 0 b
      sd = M.pushOf 1 b
      hh = M.pushOf 2 b
  in
    if bd == 0 && sd == 0 && hh == 0 then ""
    else "\n-- groove\n"
      <> "push  bd " <> signed bd <> "  sd " <> signed sd <> "  hh " <> signed hh <> "   -- ms"

-- Active ratchets on the three Grids lanes.
ratchetBlock :: M.Balistes -> String
ratchetBlock b =
  let
    forLane lane = mapMaybe
      ( \step ->
          let n = M.ratchetAt b lane step
          in if n > 1 then Just (toLower (M.instName lane) <> ":" <> show step <> "×" <> show n) else Nothing
      )
      (range 0 31)
    rs = concatMap forLane [ 0, 1, 2 ]
  in
    if null rs then "" else "\n-- ratchets\nratchet  " <> joinWith "  " rs

-- The clicked overlays (tapped pads), reflected as mini-notation at each lane's
-- derived meter. Direct-manipulation-authored, so it shows here, not in the
-- editable body.
clickBlock :: M.Balistes -> String
clickBlock b =
  let
    laneLine i =
      let clk = M.padClicks b i
      in if any identity clk
         then Just (padR 4 (toLower (M.padName b i)) <> "\"" <> clicksMini clk <> "\"")
         else Nothing
    ls = mapMaybe laneLine (range 0 (M.padCount b - 1))
  in
    if null ls then "" else "\n-- clicks (tapped)\n" <> joinWith "\n" ls

-- A clicked overlay as a flat mini-notation string at its own meter.
clicksMini :: Array Boolean -> String
clicksMini = joinWith "" <<< map (\c -> if c then "x" else "·")

-- ---------------------------------------------------------------------------
-- Body — the editable half (round-trips through parseBody)
-- ---------------------------------------------------------------------------

-- | Print the model's typed sources + routing patterns as an editable document.
-- | Only used to seed/reformat; the live textarea holds the user's verbatim text.
bodyText :: M.Balistes -> String
bodyText b =
  joinWith "\n" (filter (_ /= "") [ padBody b, routeBody b ])

padBody :: M.Balistes -> String
padBody b =
  let
    laneLine i =
      let src = String.trim (M.padSource b i)
      in if src == "" then Nothing
         else Just (padR 4 (toLower (M.padName b i)) <> "\"" <> src <> "\"")
    ls = mapMaybe laneLine (range 0 (M.padCount b - 1))
  in
    if null ls then "" else "-- pads\n" <> joinWith "\n" ls

routeBody :: M.Balistes -> String
routeBody b =
  let
    ls = mapMaybe
      (\src -> let t = String.trim src in if t == "" then Nothing else Just ("route  \"" <> t <> "\""))
      b.routes
  in
    if null ls then "" else "-- routes\n" <> joinWith "\n" ls

-- | Read an edited document back into the model: set the typed source of every
-- | named lane, collect the routing patterns, and clear any lane the document
-- | no longer mentions. Clicks, labels, knobs and ratchets are untouched.
parseBody :: String -> M.Balistes -> M.Balistes
parseBody doc b0 =
  let
    active = filter (\l -> not (isComment l) && String.trim l /= "")
      (String.split (String.Pattern "\n") doc)
    classified = map (classify b0) active
    routes = catMaybes (map routeOf classified)
    laneAssigns = catMaybes (map laneOf classified)
    -- start from every lane source cleared, then re-apply what the doc names
    cleared = foldl (\b i -> Tidal.setLaneSource i "" b) b0 (range 0 (M.padCount b0 - 1))
    withRoutes = M.setRoutes routes cleared
  in
    foldl (\b (Tuple i src) -> Tidal.setLaneSource i src b) withRoutes laneAssigns

-- One classified line of the document.
data DocLine = LRoute String | LLane Int String | LIgnore

classify :: M.Balistes -> String -> DocLine
classify b line =
  let
    { before, after } = splitFirstToken (String.trim line)
    val = stripQuotes (String.trim after)
  in
    if toLower before == "route" then LRoute val
    else case Tidal.lookupLane b before of
      Just i -> LLane i val
      Nothing -> LIgnore

routeOf :: DocLine -> Maybe String
routeOf = case _ of
  LRoute v -> Just v
  _ -> Nothing

laneOf :: DocLine -> Maybe (Tuple Int String)
laneOf = case _ of
  LLane i v -> Just (Tuple i v)
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- The starter document the textarea opens with — an inviting, self-documenting
-- demonstration of the lane-source / route / comment-toggle idiom.
-- ---------------------------------------------------------------------------

starterDoc :: String
starterDoc =
  joinWith "\n"
    [ "-- BALISTES · lane sources (Tidal mini-notation)"
    , "-- comment a line out (--) to mute it, uncomment to play."
    , ""
    , "bd   \"bd*4\""
    , "sn   \"~ sn ~ sn\""
    , "ch   \"ch*8\""
    , "-- cp   \"~ ~ cp ~\""
    , "-- lt   \"lt(3,8)\""
    , ""
    , "-- routing: one mini-notation across lanes, by name"
    , "-- route  \"oh ch oh ch\""
    ]

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

isComment :: String -> Boolean
isComment l = isJust (String.stripPrefix (String.Pattern "--") (String.trim l))

-- Split a line into its first whitespace-delimited token and the remainder.
splitFirstToken :: String -> { before :: String, after :: String }
splitFirstToken s =
  case String.indexOf (String.Pattern " ") s of
    Just idx -> { before: String.take idx s, after: String.drop (idx + 1) s }
    Nothing -> { before: s, after: "" }

-- Drop one pair of surrounding double-quotes, if present.
stripQuotes :: String -> String
stripQuotes s =
  let s1 = fromMaybe s (String.stripPrefix (String.Pattern "\"") s)
  in fromMaybe s1 (String.stripSuffix (String.Pattern "\"") s1)

padR :: Int -> String -> String
padR n s = if String.length s >= n then s <> " " else s <> joinWith "" (replicate (n - String.length s) " ")
