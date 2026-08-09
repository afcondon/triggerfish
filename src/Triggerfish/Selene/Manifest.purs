-- | `Triggerfish.Selene.Manifest` — what a music app declares it can emit from.
-- |
-- | Today `Routing.Model.Source` is a **closed ADT** naming Triggerfish's four
-- | machines, which is fine while Triggerfish is the only app and useless the
-- | moment it is not. A manifest is the open form: an app publishes the sources
-- | it offers, and the routing table is built against that rather than against a
-- | type. See `docs/DESIGN-selene-companion.md`.
-- |
-- | ## Why it declares capability, not just identity
-- |
-- | A manifest of names gets you a table. A manifest of *shapes* — this source
-- | needs `{pitch, gate}`, that one only `{gate}` — lets "this layout can drive
-- | Odonus fully and Balistes partially" be **computed** rather than eyeballed,
-- | since a voice group's capabilities are exactly what `Layout.capabilities`
-- | returns and the match is a subset test.
-- |
-- | ## Liveness, and the three standings
-- |
-- | A claim on a jack means nothing without knowing whether anyone is still there
-- | to honour it. Three cases that currently look identical in the backward view:
-- |
-- |   * **Live** — an app that is up declares this source.
-- |   * **Stale** — an app declared it, but has not been heard from since. Its
-- |     rows still sit in the table occupying jacks. Not a conflict; not free
-- |     either. Unreachable until there is more than one app, but the shape has
-- |     to admit it or retrofitting means changing the wire format.
-- |   * **Unknown** — no manifest declares it at all. **This one bites today.**
-- |     The routing table persists to `localStorage` keyed by `sourceKey`, so a
-- |     row for a Vetula voice that has since been deleted, or a Selene alias
-- |     that is gone, keeps its jack forever and nothing says so.
-- |
-- | The distinction that matters is not live-versus-dead, it is **claimed by
-- | something that exists** versus **claimed by a memory**.
module Triggerfish.Selene.Manifest
  ( Entry
  , Manifest
  , Standing(..)
  , standingNote
  , standingOf
  , declares
  , triggerfishManifest
  , nowMs
  ) where

import Prelude

import Data.Array (find, (..))
import Data.Int (round)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)

import Triggerfish.Routing.Model as RM

-- | One thing an app can emit from.
type Entry =
  { key :: String              -- ^ `RM.sourceKey` — a wire format; renaming orphans routing
  , label :: String
  , requires :: Array String   -- ^ roles it needs to be played properly
  }

type Manifest =
  { app :: String
  , seenAt :: Number           -- ^ epoch ms of the last heartbeat
  , sources :: Array Entry
  }

data Standing
  = Live String                -- ^ app name
  | Stale String Number        -- ^ app name, ms since last heard from
  | Unknown

derive instance eqStanding :: Eq Standing
derive instance ordStanding :: Ord Standing

standingNote :: Standing -> String
standingNote = case _ of
  Live app -> app
  Stale app ms -> app <> " — not heard from for " <> show (round (ms / 1000.0)) <> "s"
  Unknown -> "no app declares this source"

-- | An app not heard from for this long is presumed gone. Generous on purpose: a
-- | wrongly-stale row invites you to reassign a jack somebody is still playing,
-- | which is worse than a briefly-late one.
staleAfterMs :: Number
staleAfterMs = 15000.0

-- | Does any manifest declare this source key?
declares :: Array Manifest -> String -> Boolean
declares ms k = isJust (find (\m -> isJust (find (\e -> e.key == k) m.sources)) ms)

standingOf :: Number -> Array Manifest -> String -> Standing
standingOf now ms k =
  case find (\m -> isJust (find (\e -> e.key == k) m.sources)) ms of
    Nothing -> Unknown
    Just m ->
      let ago = now - m.seenAt
      in if ago > staleAfterMs then Stale m.app ago else Live m.app

-- ---------------------------------------------------------------------------
-- Triggerfish's own
-- ---------------------------------------------------------------------------

-- | Built from live app state rather than declared statically, which is the
-- | whole point: `vetulaNames` and `seleneAliases` change while the app runs, and
-- | a source that has gone away must stop being declared or "Unknown" never
-- | fires.
-- |
-- | The `requires` sets are the honest shapes. Note Vetula asks for `pitch` and
-- | `gate` like Odonus but is **polyphonic** — it emits chords — which is
-- | invisible here because a manifest says what a source needs, not how many at
-- | once. Allocation is the layout's business (`Layout.Allocation`), and the two
-- | meet when a source is bound to a voice group.
triggerfishManifest
  :: { vetulaNames :: Array String, seleneAliases :: Array String, seenAt :: Number }
  -> Manifest
triggerfishManifest st =
  { app: "triggerfish"
  , seenAt: st.seenAt
  , sources: odonus <> drums <> vetula <> selene
  }
  where
  entry src requires = { key: RM.sourceKey src, label: RM.sourceLabel src, requires }

  odonus = map (\h -> entry (RM.SOdonusHead h) [ "pitch", "gate" ]) (0 .. 3)
  drums = map (\i -> entry (RM.SDrumLane i) [ "gate" ]) (0 .. 15)
  vetula = map (\nm -> entry (RM.SVetulaVoice nm) [ "pitch", "gate" ]) st.vetulaNames
  -- A Selene bank is autonomous — the FH-2 or ES-9 generates it from a config we
  -- pushed, and nothing plays it. So it requires nothing of a voice group; it IS
  -- one, from the other side.
  selene = map (\a -> entry (RM.SSeleneBank a) []) st.seleneAliases

foreign import nowMs :: Effect Number
