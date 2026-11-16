-- | `Triggerfish.Clips.Kept` — **the clip library, kept in Amphora.**
-- |
-- | A clip used to live in this browser's localStorage and nowhere else: one
-- | browser, no copy, and nothing the rig could read. Now each clip is its own
-- | piece of Amphora content (immutable, addressed by its hash, so a voice or
-- | the rig can name a clip by that hash and fetch exactly it), and the
-- | library is an index of them (`Triggerfish.Library`, ref
-- | `triggerfish/clips`): one entry a clip, with its hash and the things a
-- | person edits (name, tags, notes).
-- |
-- | The browser's copy (`Clips.Store`) stays, as the copy a page opens with
-- | and works from when the store is down. This module brings the two
-- | together: a clip here and not there is sent; one there and not here is
-- | fetched; one deleted or renamed in another browser is deleted or renamed
-- | here.
-- |
-- | A page's `save` is read as its edits to what it was last handed (`read`),
-- | never as the whole library, so a clip another page or browser added
-- | meanwhile is kept, not dropped.
module Triggerfish.Clips.Kept
  ( Kept
  , Entry
  , open
  , read
  , save
  , hashOf
  ) where

import Prelude

import Amphora.Api as Api
import Data.Array (any, catMaybes, filter, find, foldl, length, notElem)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (readJSON, writeJSON)
import Triggerfish.Amphora as Amphora
import Triggerfish.Clips (MidiClip)
import Triggerfish.Clips.Codec as Codec
import Triggerfish.Clips.Store as Store
import Triggerfish.Library as Library

-- | A clip in the index: where its content is, and what can change without
-- | the content changing.
type Entry =
  { id :: String
  , hash :: String
  , name :: String
  , tags :: Array String
  , notes :: String
  , source :: String
  , capturedMicros :: Number
  , lenMicros :: Number
  , heads :: Int
  , bpm :: Nullable Number
  , key :: Nullable String
  }

-- | What this browser knows of a clip's content in Amphora: its hash, and a
-- | mark of the notes it was made from (so re-cut notes are sent again).
type Sent = { hash :: String, mark :: String }

newtype Kept = Kept
  { shelf :: Library.Shelf Entry
  , handed :: Ref (Array String)     -- the ids the page was last given
  , busy :: Ref Boolean
  , again :: Ref Boolean
  , changed :: Effect Unit
  }

indexDoc :: Library.Doc Entry
indexDoc =
  { ref: "triggerfish/clips"
  , kind: "triggerfish-clip-index"
  , idOf: _.id
  , withId: \id e -> e { id = id }
  , encode: writeJSON
  , decode: hush <<< readJSON
  , same: \a b -> a.id == b.id
  , first: pure []
  , tidy: identity
  }

sentKey :: String
sentKey = "triggerfish.clips.sent"

readSent :: Effect (Object Sent)
readSent = (fromMaybe Object.empty <<< (_ >>= hush <<< readJSON) <<< toMaybe) <$> loadText sentKey

writeSent :: Object Sent -> Effect Unit
writeSent = saveText sentKey <<< writeJSON

markOf :: MidiClip -> String
markOf c = show (length c.events) <> ":" <> show c.lenMicros

-- | A clip's hash in Amphora, once it has been sent: how a voice or the rig
-- | names it.
hashOf :: MidiClip -> Effect (Maybe String)
hashOf c = (map _.hash <<< Object.lookup c.id) <$> readSent

entryOf :: String -> MidiClip -> Entry
entryOf hash c =
  { id: c.id, hash, name: c.name, tags: c.tags, notes: c.notes, source: c.source
  , capturedMicros: c.capturedMicros, lenMicros: c.lenMicros, heads: c.heads
  , bpm: toNullable c.bpm, key: toNullable c.key }

-- | A clip with what its entry says of it (the entry is where edits are kept).
dressed :: Entry -> MidiClip -> MidiClip
dressed e c = c { id = e.id, name = e.name, tags = e.tags, notes = e.notes }

-- | No two clips with one id: the second and later take a suffix. Two clips
-- | cut from one mark used to share theirs.
distinct :: Array MidiClip -> Array MidiClip
distinct = _.out <<< foldl step { seen: [], out: [] }
  where
  step acc c =
    let id = free acc.seen c.id 2
    in { seen: acc.seen <> [ id ], out: acc.out <> [ c { id = id } ] }
  free seen id n
    | notElem id seen = id
    | notElem (id <> "-" <> show n) seen = id <> "-" <> show n
    | otherwise = free seen id (n + 1)

-- | Open the library: the clips as this browser has them, at once; then
-- | `changed` whenever Amphora's library made them differ (the page then
-- | `read`s).
open :: Effect Unit -> Effect { kept :: Kept, clips :: Array MidiClip }
open changed = do
  stored <- Store.loadClips
  let clips = distinct stored
  when (map _.id clips /= map _.id stored) (Store.saveClips clips)
  handed <- Ref.new (map _.id clips)
  busy <- Ref.new false
  again <- Ref.new false
  -- the index tells us when another browser changed it; `kept` is tied below
  self <- Ref.new Nothing
  opened <- Library.open indexDoc [] (Ref.read self >>= case _ of
    Just k -> launchAff_ (settle k)
    Nothing -> pure unit)
  let kept = Kept { shelf: opened.shelf, handed, busy, again, changed }
  Ref.write (Just kept) self
  launchAff_ (settle kept)
  pure { kept, clips }

-- | The clips now, for a page told they `changed`.
read :: Kept -> Effect (Array MidiClip)
read (Kept k) = do
  clips <- Store.loadClips
  Ref.write (map _.id clips) k.handed
  pure clips

-- | The page's clips, as its edits to what it was last handed: a clip it no
-- | longer has is deleted, one it has is kept as it now is, and any the page
-- | was never handed stay. Returns the library with that done.
save :: Kept -> Array MidiClip -> Effect (Array MidiClip)
save kept@(Kept k) mine = do
  handed <- Ref.read k.handed
  stored <- Store.loadClips
  let unseen = filter (\c -> notElem c.id handed && not (any (\m -> m.id == c.id) mine)) stored
      clips = distinct (mine <> unseen)
  Store.saveClips clips
  Ref.write (map _.id clips) k.handed
  launchAff_ (settle kept)
  pure clips

-- | Bring this browser and Amphora together. One at a time; a call while one
-- | runs runs again after.
settle :: Kept -> Aff Unit
settle kept@(Kept k) = do
  running <- liftEffect (Ref.read k.busy)
  if running then liftEffect (Ref.write true k.again)
  else do
    liftEffect (Ref.write true k.busy *> Ref.write false k.again)
    before <- liftEffect Store.loadClips
    -- 1. every clip here has its content there
    sent0 <- liftEffect readSent
    fresh <- catMaybes <$> traverse (send sent0) before
    let sent1 = foldl (\o s -> Object.insert s.id { hash: s.hash, mark: s.mark } o) sent0 fresh
    liftEffect (writeSent sent1)
    -- 2. the index, with this browser's edits made to it
    let mine = catMaybes (map (\c -> (\s -> entryOf s.hash c) <$> Object.lookup c.id sent1) before)
    index <- liftEffect (Library.put k.shelf mine)
    -- 3. what the index has and this browser does not
    let missing = filter (\e -> not (any (\c -> c.id == e.id) before)) index
    fetched <- catMaybes <$> traverse fetch missing
    -- 4. made on the clips as they are now (the page may have saved meanwhile)
    changed <- liftEffect do
      now <- Store.loadClips
      let inIndex id = any (\e -> e.id == id) index
          -- sent once, and no longer in the index: deleted in another browser
          gone c = isJust (Object.lookup c.id sent1) && not (inIndex c.id)
          stay = map (\c -> maybe' c (\e -> dressed e c) (find (\e -> e.id == c.id) index)) (filter (not <<< gone) now)
          added = filter (\f -> not (any (\n -> n.id == f.clip.id) now)) fetched
          next = stay <> map _.clip added
          forgotten = foldl (\o c -> if gone c then Object.delete c.id o else o) sent1 now
          sent2 = foldl (\o f -> Object.insert f.clip.id { hash: f.hash, mark: markOf f.clip } o) forgotten added
          differs = map Codec.encode next /= map Codec.encode now
      writeSent sent2
      when differs (Store.saveClips next)
      pure differs
    liftEffect do
      Ref.write false k.busy
      when changed k.changed
    rerun <- liftEffect (Ref.read k.again)
    when rerun (settle kept)
  where
  maybe' d f = case _ of
    Just x -> f x
    Nothing -> d
  -- a clip's content, put there if it is not, or if its notes changed
  send sent c = case Object.lookup c.id sent of
    Just s | s.mark == markOf c -> pure Nothing
    _ -> Amphora.ask Api.putContent { kind: "triggerfish-clip", payload: Codec.encode c } <#> case _ of
      Right { hash } -> Just { id: c.id, hash, mark: markOf c }
      Left _ -> Nothing
  fetch e = Amphora.ask Api.getContent { hash: e.hash } <#> case _ of
    Right { payload } -> (\c -> { clip: dressed e c, hash: e.hash }) <$> Codec.decode payload
    Left _ -> Nothing

foreign import loadText :: String -> Effect (Nullable String)
foreign import saveText :: String -> String -> Effect Unit
