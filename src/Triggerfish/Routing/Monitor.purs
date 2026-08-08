-- | `Triggerfish.Routing.Monitor` — what the MIDI ports are ACTUALLY carrying.
-- |
-- | The routing table says where output is *meant* to go. This says where notes
-- | are *observed* going. Keeping both, side by side, is the point: this rig's
-- | recurring failure is a surface that reports healthy while a load-bearing
-- | thing is dead, and the only cure is evidence that doesn't come from the same
-- | place as the claim.
-- |
-- | It taps `MIDIOutput.prototype.send`, so it sees every note the page emits —
-- | including from code paths that never consulted the routing table. Traffic the
-- | table cannot account for is the most useful thing here: it is the answer to
-- | "something is sending it MIDI but I can't see what".
module Triggerfish.Routing.Monitor
  ( Row
  , install
  , read
  , clear
  , matches
  , hitsFor
  , unaccounted
  ) where

import Prelude

import Data.Array (filter, find)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), contains)
import Effect (Effect)

import Triggerfish.Routing.Model (Source, Table, Wire, liveLegsFor, wireOf)

-- | One observed (port, channel, note) triple. `agoMs` is how long since the last
-- | hit, which is what makes "this route was live a moment ago" distinguishable
-- | from "this route carried something once, an hour back".
type Row =
  { port :: String
  , channel :: Int
  , note :: Int
  , hits :: Int
  -- Note-OFFS, counted separately. `hits` far exceeding `offs` means notes are
  -- being started and never ended — which on an FH-2 envelope reads as "it never
  -- comes back down", indistinguishable at the rack from a sustain problem.
  , offs :: Int
  , velMin :: Int
  , velMax :: Int
  , agoMs :: Number
  }

foreign import installImpl :: Effect Unit
foreign import readImpl :: Effect (Array Row)
foreign import clearImpl :: Effect Unit

-- | Install the tap. Idempotent, and a no-op where WebMIDI is absent.
install :: Effect Unit
install = installImpl

read :: Effect (Array Row)
read = readImpl

clear :: Effect Unit
clear = clearImpl

-- | Whether an observed row is traffic for this wire.
-- |
-- | Port is matched by SUBSTRING, because a route names a needle ("IAC") while
-- | the tap records the real port ("IAC Driver Tidal") — the same rule
-- | `findOutput` uses, so the monitor's idea of "this route" cannot drift from
-- | the emit path's.
-- |
-- | Note is compared only when the destination pins one. An FH-2 gate selects BY
-- | note, so its note is part of its identity; a plain MIDI destination carries
-- | whatever the source played, so requiring a note match there would report
-- | every route as silent.
matches :: Wire -> Row -> Boolean
matches w r =
  contains (Pattern w.port) r.port
    && r.channel == w.channel
    && case w.noteOverride of
      Nothing -> true
      Just n -> r.note == n

-- | Total observed hits attributable to one source's live legs.
hitsFor :: Array Row -> Table -> Source -> Int
hitsFor rows tbl src =
  sum (map legHits (liveLegsFor tbl src))
  where
  legHits l = case wireOf l.dest of
    Nothing -> 0
    Just w -> sum (map _.hits (filter (matches w) rows))

-- | Observed traffic that NO live leg in the table explains.
-- |
-- | The diagnostic that matters most. A row here is either a machine emitting
-- | outside the router (a bug this refactor was meant to remove), a stale route
-- | still firing, or something else on the box holding the port open. All three
-- | are invisible without it, and the third has cost this rig whole evenings.
unaccounted :: Array Row -> Table -> Array Source -> Array Row
unaccounted rows tbl srcs = filter unexplained rows
  where
  wires = do
    src <- srcs
    l <- liveLegsFor tbl src
    case wireOf l.dest of
      Nothing -> []
      Just w -> [ w ]
  unexplained r = isNothing (find (\w -> matches w r) wires)
