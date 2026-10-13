-- | Triggerfish.Library — what a machine's Library drawer holds, kept in
-- | Amphora, so it is the same in every browser and on every reload.
-- |
-- | Each collection is one Amphora document (`kind`), the current one being
-- | where the ref `ref` points (`triggerfish/balistes/library`). A change is
-- | kept as new content and the ref moved to it, compare-and-swap, as
-- | Conspicillum keeps its library (`Conspicillum.Library`): when another
-- | page moved the ref first, the change is made again on top of theirs. A
-- | change travels as what it did to items, by their ids (`Change`), never as
-- | a whole library, so neither page's change is lost.
-- |
-- | The browser keeps a copy (localStorage) of what Amphora last said and of
-- | the page's library, so a page opens at once and works with the store
-- | down: what differs between the two is a change not yet kept, sent the
-- | next time the store answers. A page asks again whenever it comes back
-- | into view, so another browser's changes arrive.
-- |
-- | The old per-item collections (`balistes-grid`, `odonus-scene`, …) are
-- | read once, to start the document when there is none, and never again,
-- | so what is deleted or renamed stays so.
module Triggerfish.Library
  ( Doc
  , Shelf
  , open
  , put
  , current
  ) where

import Prelude

import Data.Array (any, catMaybes, cons, filter, find, findIndex, foldl, insertAt, mapWithIndex, notElem, null, snoc, updateAt, (!!))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Simple.JSON (readJSON, writeJSON)
import Triggerfish.Amphora as Amphora

-- | One collection: where it is kept, how its items are told apart and
-- | written, and what it started from before it was kept here.
type Doc a =
  { ref :: String
  , kind :: String
  , idOf :: a -> String               -- "" until it has one
  , withId :: String -> a -> a
  , encode :: Array a -> String
  , decode :: String -> Maybe (Array a)
  , same :: a -> a -> Boolean         -- one thing kept twice, before ids (by name)
  , first :: Aff (Array a)            -- the old collection, read once
  -- What every page would make of a library (a preset's word): made here, as
  -- items come in, so no page sends it as an edit of its own, made to its
  -- older view, over another page's.
  , tidy :: Array a -> Array a
  }

type Held a = { hash :: Maybe String, items :: Array a }

data Change a = Put (Maybe String) a | Drop String

-- | What turned `old` into `new`: each item gone, and each new or changed
-- | one with the item it follows.
changes :: forall a. Doc a -> Array a -> Array a -> Array (Change a)
changes doc old new =
  map Drop (filter (\id -> notElem id (map doc.idOf new)) (map doc.idOf old))
    <> catMaybes (mapWithIndex one new)
  where
  one i x = case find (\o -> doc.idOf o == doc.idOf x) old of
    Just o | doc.encode [ o ] == doc.encode [ x ] -> Nothing
    _ -> Just (Put (doc.idOf <$> new !! (i - 1)) x)

-- | Make the changes to a library: an item changed where it is, a new one
-- | after the item it followed (first, or last when that one is gone).
makeChanges :: forall a. Doc a -> Array (Change a) -> Array a -> Array a
makeChanges doc cs items = foldl one items cs
  where
  one xs = case _ of
    Drop id -> filter (\x -> doc.idOf x /= id) xs
    Put after x -> case findIndex (\y -> doc.idOf y == doc.idOf x) xs of
      Just i -> fromMaybe xs (updateAt i x xs)
      Nothing -> case after of
        Nothing -> cons x xs
        Just a -> case findIndex (\y -> doc.idOf y == a) xs of
          Just j -> fromMaybe (snoc xs x) (insertAt (j + 1) x xs)
          Nothing -> snoc xs x

-- | The library the ref points at, `Nothing` if none was kept yet.
load :: forall a. Doc a -> Aff (Either String (Maybe (Held a)))
load doc = Amphora.exchange "GET" ("/refs/" <> doc.ref) Nothing >>= case _ of
  Left problem -> pure (Left problem)
  Right { status: 404 } -> pure (Right Nothing)
  Right { status: 200, body } | Right ({ hash } :: { hash :: String }) <- readJSON body ->
    map (\items -> Just { hash: Just hash, items: doc.tidy items }) <$> contentOf doc hash
  Right r -> pure (Left (doc.ref <> ": " <> show r.status <> " " <> r.body))

contentOf :: forall a. Doc a -> String -> Aff (Either String (Array a))
contentOf doc hash = Amphora.exchange "GET" ("/content/" <> hash) Nothing <#> case _ of
  Right { status: 200, body }
    | Right ({ payload } :: { payload :: String }) <- readJSON body
    , Just items <- doc.decode payload -> Right items
  Right r -> Left (hash <> ": " <> show r.status <> " " <> r.body)
  Left problem -> Left problem

-- | Make the changes to the library as it stands at `at`, keep the result and
-- | move the ref to it; when the ref has moved on, make them again to the
-- | library it now points at.
keep :: forall a. Doc a -> Held a -> Array (Change a) -> Aff (Either String (Held a))
keep doc = go 5
  where
  go tries at cs
    | tries <= 0 = pure (Left (doc.ref <> " kept changing under this page"))
    | otherwise = do
        let next = doc.tidy (makeChanges doc cs at.items)
        saved <- Amphora.exchange "POST" "/content" (Just (writeJSON { kind: doc.kind, payload: doc.encode next }))
        case saved of
          Right { status, body }
            | status < 300, Right ({ hash } :: { hash :: String }) <- readJSON body ->
                if at.hash == Just hash then pure (Right { hash: Just hash, items: next })
                else do
                  moved <- Amphora.exchange "POST" ("/refs/" <> doc.ref) (Just (writeJSON { to: hash, expected: toNullable at.hash }))
                  case moved of
                    Right { status: 200 } -> pure (Right { hash: Just hash, items: next })
                    Right { status: 409, body: theirs }
                      | Right ({ current } :: { current :: Nullable { hash :: String } }) <- readJSON theirs ->
                          case toMaybe current of
                            Nothing -> go (tries - 1) { hash: Nothing, items: [] } cs
                            Just now -> contentOf doc now.hash >>= case _ of
                              Left problem -> pure (Left problem)
                              Right items -> go (tries - 1) { hash: Just now.hash, items: doc.tidy items } cs
                    Right r -> pure (Left (doc.ref <> ": " <> show r.status <> " " <> r.body))
                    Left problem -> pure (Left problem)
          Right r -> pure (Left ("/content: " <> show r.status <> " " <> r.body))
          Left problem -> pure (Left problem)

-- ---------------------------------------------------------------------------
-- A page's shelf
-- ---------------------------------------------------------------------------

-- | A page's hold on one collection.
newtype Shelf a = Shelf
  { doc :: Doc a
  , state :: Ref (St a)
  , changed :: Effect Unit
  }

-- | `base` is what Amphora last said (`known` once it has said anything to
-- | this browser); `local` is the library here, which differs from `base` by
-- | the changes not yet kept; `handed` is what the page was last given. A
-- | page's library is `handed` plus its own edits, so what it puts is read
-- | as those edits and nothing else: another page's change that arrived
-- | since is kept, not undone.
type St a =
  { base :: Held a
  , known :: Boolean
  , local :: Array a
  , handed :: Array a
  , busy :: Boolean
  , again :: Boolean
  }

-- | Open a collection: the library as this browser last had it (or, the
-- | first time, `legacy`, what it kept before Amphora did) at once; then
-- | `changed` whenever Amphora's differs, when the page reads it (`current`).
open :: forall a. Doc a -> Array a -> Effect Unit -> Effect { shelf :: Shelf a, items :: Array a }
open doc legacy changed = do
  cached <- readCache doc
  let st = case cached of
        Just c -> { base: c.base, known: true, local: c.local, handed: [], busy: false, again: false }
        Nothing -> { base: { hash: Nothing, items: [] }, known: false, local: legacy, handed: [], busy: false, again: false }
  local <- withIds doc (doc.tidy st.local)
  state <- Ref.new st { local = local, handed = local }
  let shelf = Shelf { doc, state, changed }
  launchAff_ (sync shelf)
  onShown (launchAff_ (sync shelf))
  pure { shelf, items: local }

-- | The page's library now, as its edits to what it was last handed. Returns
-- | the library with them made (an id on every item, and any other page's
-- | change that arrived meanwhile): the page shows that. Sends the edits.
put :: forall a. Shelf a -> Array a -> Effect (Array a)
put shelf@(Shelf sh) items = do
  edited <- withIds sh.doc items
  st <- Ref.modify (\s -> let local = sh.doc.tidy (makeChanges sh.doc (changes sh.doc s.handed edited) s.local) in s { local = local, handed = local }) sh.state
  writeCache sh.doc st
  launchAff_ (sync shelf)
  pure st.local

-- | The library as it is here now, for a page told it `changed`.
current :: forall a. Shelf a -> Effect (Array a)
current (Shelf sh) = _.local <$> Ref.modify (\s -> s { handed = s.local }) sh.state

-- | Bring this page and Amphora together: send what this page changed, take
-- | what others did. One at a time; a call while one runs runs again after.
sync :: forall a. Shelf a -> Aff Unit
sync shelf@(Shelf sh) = do
  st <- liftEffect (Ref.read sh.state)
  if st.busy then liftEffect (Ref.modify_ (_ { again = true }) sh.state)
  else do
    liftEffect (Ref.modify_ (_ { busy = true, again = false }) sh.state)
    load sh.doc >>= case _ of
      Left _ -> pure unit
      Right held -> do
        let sent = st.local
        plan <- case held of
          -- none yet: start it from this page's library and the old collection
          Nothing -> do
            old <- either (const []) identity <$> attempt sh.doc.first
            olds <- liftEffect (withIds sh.doc (filter (\o -> not (any (sh.doc.same o) sent)) old))
            pure { at: { hash: Nothing, items: [] }, cs: changes sh.doc [] (sh.doc.tidy (sent <> olds)) }
          Just at
            | st.known -> pure { at, cs: changes sh.doc st.base.items sent }
            -- this browser's first word from Amphora: add what it kept and
            -- Amphora hasn't
            | otherwise -> pure { at, cs: changes sh.doc [] (filter (\x -> not (any (sh.doc.same x) at.items)) sent) }
        kept <- if null plan.cs && isJust held then pure (Right plan.at) else keep sh.doc plan.at plan.cs
        case kept of
          Left _ -> pure unit
          Right now -> liftEffect do
            latest <- Ref.read sh.state
            -- what the page changed while this was out, made on what was kept
            let local = sh.doc.tidy (makeChanges sh.doc (changes sh.doc sent latest.local) now.items)
            st' <- Ref.modify (_ { base = now, known = true, local = local }) sh.state
            writeCache sh.doc st'
            -- the page shows what it was handed: tell it when that is not this
            when (sh.doc.encode local /= sh.doc.encode st'.handed) sh.changed
    after <- liftEffect (Ref.modify (_ { busy = false }) sh.state)
    when after.again (sync shelf)

withIds :: forall a. Doc a -> Array a -> Effect (Array a)
withIds doc = traverse \x -> if doc.idOf x == "" then (\id -> doc.withId id x) <$> freshId else pure x

-- ---------------------------------------------------------------------------
-- The browser's copy
-- ---------------------------------------------------------------------------

type Copy = { base :: { hash :: Nullable String, items :: String }, local :: String }

cacheKey :: forall a. Doc a -> String
cacheKey doc = "triggerfish.library." <> doc.ref

readCache :: forall a. Doc a -> Effect (Maybe { base :: Held a, local :: Array a })
readCache doc = do
  text <- loadCopy (cacheKey doc)
  pure do
    c :: Copy <- either (const Nothing) Just (readJSON =<< pure (fromMaybe "" (toMaybe text)))
    base <- doc.decode c.base.items
    local <- doc.decode c.local
    pure { base: { hash: toMaybe c.base.hash, items: base }, local }

writeCache :: forall a. Doc a -> St a -> Effect Unit
writeCache doc st = saveCopy (cacheKey doc)
  (writeJSON ({ base: { hash: toNullable st.base.hash, items: doc.encode st.base.items }, local: doc.encode st.local } :: Copy))

foreign import freshId :: Effect String
foreign import loadCopy :: String -> Effect (Nullable String)
foreign import saveCopy :: String -> String -> Effect Unit
-- | Run this whenever the page comes back into view.
foreign import onShown :: Effect Unit -> Effect Unit
