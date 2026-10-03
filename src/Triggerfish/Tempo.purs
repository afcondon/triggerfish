-- | **The tempo, one for every page.** It is set on the dashboard, or from any
-- | page by the tempo hotkeys, and nowhere else.
-- |
-- | Two places hold it, for the two ways a page keeps time:
-- |
-- | - with the rig up, Link's session: setting the tempo sends the rig `bpm N`,
-- |   which Architeuthis passes to Diaphus (`/link/set-tempo`), and every
-- |   page's clock follows the anchor that comes back, as do the Link peers;
-- | - with no rig (Solo off the rig, the static site), the free-run tempo each
-- |   page's clock falls back to, kept in this browser's storage so every tab
-- |   runs at the same one and a reload keeps it.
-- |
-- | Setting it writes both, so the two never disagree once the rig is up.
module Triggerfish.Tempo
  ( load
  , save
  , onChange
  , set
  , current
  , Bump
  , hotkey
  , hotkeyHelp
  , clampTempo
  , showTempo
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Int as Int
import Data.Number (round)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Binnacle (Binnacle)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Transport as Transport
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

storeKey :: String
storeKey = "triggerfish.tempo.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable { bpm :: Number })
foreign import _onChange :: String -> Effect Unit -> Effect Unit

-- | The free-run tempo this browser last set, if any.
load :: Effect (Maybe Number)
load = map (\s -> clampTempo s.bpm) <<< toMaybe <$> _load storeKey

save :: Number -> Effect Unit
save bpm = _save storeKey ("{\"bpm\":" <> show (clampTempo bpm) <> "}")

-- | Run `callback` when ANOTHER tab of this origin sets the tempo.
onChange :: Effect Unit -> Effect Unit
onChange = _onChange storeKey

-- | Tempi are kept to a tenth of a beat a minute, from 20 to 300.
clampTempo :: Number -> Number
clampTempo n = round (clamp 20.0 300.0 n * 10.0) / 10.0

-- | A tempo as people read it: whole beats plainly, else to a tenth.
showTempo :: Number -> String
showTempo n =
  let t = round (n * 10.0) / 10.0
  in if t == round t then show (Int.round t) else toStringWith (fixed 1) t

-- | Set the tempo: stored for this browser's pages, and sent to the rig, if
-- | it is connected, for Link.
set :: Binnacle -> Number -> Effect Unit
set bin n = do
  let bpm = clampTempo n
  save bpm
  up <- Transport.isConnected (Binnacle.socket bin)
  when up $ Transport.send (Binnacle.socket bin) ("bpm " <> showTempo bpm)

-- | The tempo being kept: Link's once the clock is anchored, else `free`,
-- | the free-run tempo (the clock's own is only what it was made with).
current :: Binnacle -> Number -> Effect Number
current bin free = do
  r <- Clock.read (Binnacle.clock bin)
  pure (if r.locked then r.tempo else free)

-- | A tempo hotkey: how far it moves the tempo.
type Bump = Number

-- | The tempo hotkeys, the same on every page: ⌥− and ⌥= move it by one, with
-- | ⇧ by five. Read by the key's position, since on a Mac Option
-- | turns `-` into `–`; and with Option held, so a page that plays its keys
-- | (Vetula) never hears them as notes.
hotkey :: KeyboardEvent -> Maybe Bump
hotkey ke
  | not (KE.altKey ke) || KE.metaKey ke || KE.ctrlKey ke = Nothing
  | otherwise =
      let step = if KE.shiftKey ke then 5.0 else 1.0
      in case KE.code ke of
        "Minus" -> Just (negate step)
        "Equal" -> Just step
        _ -> Nothing

hotkeyHelp :: String
hotkeyHelp = "⌥− / ⌥= tempo −1 / +1 (with ⇧, ±5)"
