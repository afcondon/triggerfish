-- | Triggerfish.Balistes.Store — localStorage persistence for the fixed-rhythm
-- | library. Triggerfish is a browser instrument, so the patterns you author
-- | live in the browser; this serialises the library to a plain-JSON form
-- | (records/arrays/ints — `TrigCond` flattened to two ints) and round-trips it
-- | through `window.localStorage`. Lossy/corrupt reads degrade to `Nothing`, so
-- | the app falls back to the bundled patterns rather than crashing.
module Triggerfish.Balistes.Store
  ( saveLibrary
  , loadLibrary
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Balistes.Pattern (Cell, FixedPattern, TrigCond(..))

storeKey :: String
storeKey = "triggerfish.balistes.library.v1"

-- The on-disk shapes: plain JS objects (cond → cx/cy, CAlways = 0/0).
type SCell = { vel :: Int, prob :: Int, cx :: Int, cy :: Int, ratchet :: Int }
type SPattern = { name :: String, steps :: Int, notes :: Array Int, grid :: Array (Array SCell) }

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable (Array SPattern))
foreign import _stringify :: Array SPattern -> String

toSCell :: Cell -> SCell
toSCell c = case c.cond of
  CAlways -> { vel: c.vel, prob: c.prob, cx: 0, cy: 0, ratchet: c.ratchet }
  CEvery x y -> { vel: c.vel, prob: c.prob, cx: x, cy: y, ratchet: c.ratchet }

fromSCell :: SCell -> Cell
fromSCell s =
  { vel: s.vel
  , prob: s.prob
  , ratchet: s.ratchet
  , cond: if s.cy <= 0 then CAlways else CEvery s.cx s.cy
  }

toSPattern :: FixedPattern -> SPattern
toSPattern p = { name: p.name, steps: p.steps, notes: p.notes, grid: map (map toSCell) p.grid }

fromSPattern :: SPattern -> FixedPattern
fromSPattern s = { name: s.name, steps: s.steps, notes: s.notes, grid: map (map fromSCell) s.grid }

-- | Persist the whole library (best-effort — failures are swallowed in the FFI).
saveLibrary :: Array FixedPattern -> Effect Unit
saveLibrary lib = _save storeKey (_stringify (map toSPattern lib))

-- | Load the stored library, or `Nothing` if absent / unparseable.
loadLibrary :: Effect (Maybe (Array FixedPattern))
loadLibrary = do
  m <- _load storeKey
  pure (map (map fromSPattern) (toMaybe m))
