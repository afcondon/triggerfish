-- | **The tab bus**: how the dashboard and the machines' pages talk, tab to tab.
-- |
-- | Every Atlantis page is served from one origin (`:3023`), so they can share a
-- | `BroadcastChannel`. It needs no rig, so it works in Solo as well as Atlantis.
-- |
-- | - A machine's page announces its state (`State`) whenever it changes and on
-- |   every tick of its shell, so a dashboard that stops hearing from a machine
-- |   knows its tab has closed.
-- | - The dashboard sends commands: play or stop one machine, Panic, and
-- |   `Hello`, which asks every page to announce itself now.
-- |
-- | Commands only, never audio or timing: each machine still keeps its own time.
-- | The mode (Solo or Atlantis) is not on the bus; it is a stored preference, and
-- | pages follow it through `Transport.Store.onChange`.
-- |
-- | Machines are named by their stage slot (`Triggerfish.Stage.slotOf`).
module Triggerfish.TabBus
  ( Bus
  , Msg(..)
  , MachineState
  , open
  , post
  , onMessage
  , sayGoodbye
  ) where

import Prelude

import Data.Either (hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Simple.JSON (readJSON, writeJSON)

foreign import data Bus :: Type

foreign import _open :: String -> Effect Bus
foreign import _post :: Bus -> String -> Effect Unit
foreign import _onMessage :: Bus -> (String -> Effect Unit) -> Effect Unit
foreign import _onPageHide :: Effect Unit -> Effect Unit

-- | What a machine's page says about its machine.
type MachineState =
  { machine :: String
  , alias :: Maybe String
  , edited :: Boolean
  , playing :: Boolean
  }

data Msg
  = State MachineState
  | Play String
  | Stop String
  | Panic
  | Hello
  -- | A machine's page is closing. Silence is not enough to tell: a browser
  -- | slows a background tab's timers to once a minute, so a quiet tab is
  -- | usually still there.
  | Bye String

-- | The wire shape: a tag and whichever fields it needs.
type Wire =
  { t :: String
  , machine :: Nullable String
  , alias :: Nullable String
  , edited :: Boolean
  , playing :: Boolean
  }

open :: Effect Bus
open = _open "atlantis"

post :: Bus -> Msg -> Effect Unit
post bus = _post bus <<< encode

-- | Every message another tab posts. A message this build cannot read is
-- | dropped, so an older and a newer page can share the bus.
onMessage :: Bus -> (Msg -> Effect Unit) -> Effect Unit
onMessage bus cb = _onMessage bus \text -> case decode text of
  Just m -> cb m
  Nothing -> pure unit

encode :: Msg -> String
encode = writeJSON <<< case _ of
  State s -> wire "state" (Just s.machine) s.alias s.edited s.playing
  Play m -> wire "play" (Just m) Nothing false false
  Stop m -> wire "stop" (Just m) Nothing false false
  Panic -> wire "panic" Nothing Nothing false false
  Hello -> wire "hello" Nothing Nothing false false
  Bye m -> wire "bye" (Just m) Nothing false false
  where
  wire t machine alias edited playing =
    { t, machine: toNullable machine, alias: toNullable alias, edited, playing } :: Wire

decode :: String -> Maybe Msg
decode text = do
  w <- hush (readJSON text :: _ Wire)
  case w.t, toMaybe w.machine of
    "state", Just m -> Just (State { machine: m, alias: toMaybe w.alias, edited: w.edited, playing: w.playing })
    "play", Just m -> Just (Play m)
    "stop", Just m -> Just (Stop m)
    "panic", _ -> Just Panic
    "hello", _ -> Just Hello
    "bye", Just m -> Just (Bye m)
    _, _ -> Nothing

-- | Say `Bye` for each of these machines when the page goes away (closed,
-- | reloaded or navigated off), so the dashboard knows at once.
sayGoodbye :: Bus -> Array String -> Effect Unit
sayGoodbye bus slots = _onPageHide (for_ slots (post bus <<< Bye))
