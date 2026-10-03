-- | **What feeds Odonus's harmony inputs, as the rig resolved it.** The rig
-- | applies the harmony routes to its own Odonus voice (`odonus_feeds`) and
-- | publishes the result on the stage as `odonus/feeds`
-- | (`Reef.Route.printFeeds`): a scale, Vetula's key, a card's chords as a
-- | harmony pattern. A page that plays Odonus itself (Solo) has no rig voice
-- | to follow, so it reads that object and applies the same `feedInputs`.
-- | Reading it here, rather than resolving the routes in the page, keeps the
-- | one resolution on the rig, and a card read with Tidal out of the browser.
module Triggerfish.Odonus.Feeds
  ( feedsKey
  , subscribeLine
  , readFeeds
  , routesKey
  , readRoutes
  ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), stripPrefix)
import Foreign.Object (Object)
import Foreign.Object as Object
import Reef.Route (Feed(..), Feeds, parseFeeds)
import Simple.JSON (readJSON)

feedsKey :: String
feedsKey = "odonus/feeds"

-- | The harmony routes as written (`Reef.Route`), which a mark keeps beside
-- | what they resolved to.
routesKey :: String
routesKey = "routing/harmony"

subscribeLine :: String
subscribeLine = "stage-text-subscribe"

-- | The feeds a rig frame states: from the whole table (on subscribing) or a
-- | write of `odonus/feeds`. No object, or a deleted one, is nothing fed;
-- | `Nothing` is a frame that says nothing about the feeds.
readFeeds :: String -> Maybe Feeds
readFeeds msg = case stripPrefix (Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    pure (fromText (_.text <$> Object.lookup feedsKey table))
  Nothing -> do
    json <- stripPrefix (Pattern "stage-text ") msg
    w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
    if w.key == feedsKey then Just (fromText (toMaybe w.text)) else Nothing
  where
  unfed = { grid: Unfed, out: Unfed }
  fromText = maybe unfed (\t -> maybe unfed identity (hush (parseFeeds t)))

-- | The routes' text a rig frame states: from the whole table, or a write of
-- | `routing/harmony` (`Just Nothing`: none). `Nothing`: says nothing of them.
readRoutes :: String -> Maybe (Maybe String)
readRoutes msg = case stripPrefix (Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    pure (_.text <$> Object.lookup routesKey table)
  Nothing -> do
    json <- stripPrefix (Pattern "stage-text ") msg
    w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
    if w.key == routesKey then Just (toMaybe w.text) else Nothing
