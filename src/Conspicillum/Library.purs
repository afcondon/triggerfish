-- | **The library: how presets are organised**, apart from the presets.
-- |
-- | A preset is referred to by a stable reference: a factory preset by its
-- | name in reef (`reef:Tape`), a kept one by its hash (`kept:…`). Over those
-- | the library holds
-- |
-- | - a **category** for any preset that is not in its bank's,
-- | - **tags**, freely,
-- | - **collections**: named, ordered lists, for a set, a piece, a session;
-- | - which collection is **on the module**, whose presets the module lists
-- |   and steps through. None means all of them.
-- |
-- | It is one document in Amphora, kind `conspicillum-library`, and the
-- | current one is where the ref `conspicillum/library` points. A change is
-- | kept as new content and the ref moved to it, compare-and-swap, so every
-- | earlier library is still there, in the ref's moves. When another page
-- | moved the ref first, the change is made again on top of theirs: changes
-- | travel as edits (`Library -> Library`), never as whole libraries, so
-- | neither page's change is lost.
module Conspicillum.Library
  ( Library
  , Collection
  , empty
  , Edit
  , load
  , keep
  , categoryOf
  , tagsOf
  , setCategory
  , addTag
  , removeTag
  , collectionNamed
  , addCollection
  , renameCollection
  , removeCollection
  , toggleMember
  , move
  , renameReference
  , allTags
  ) where

import Prelude

import Conspicillum.Http as Http
import Data.Array (delete, deleteAt, elem, filter, find, findIndex, insertAt, length, nub, snoc, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String as String
import Effect.Aff (Aff)
import Simple.JSON (E, readJSON, writeJSON)

type Collection = { name :: String, presets :: Array String }

type Library =
  { categories :: Array { preset :: String, category :: String }
  , tags :: Array { preset :: String, tags :: Array String }
  , collections :: Array Collection
  , onModule :: Maybe String
  }

empty :: Library
empty = { categories: [], tags: [], collections: [], onModule: Nothing }

-- ── reading it ───────────────────────────────────────────────────────────────

categoryOf :: Library -> String -> String -> String
categoryOf library preset bank = maybe bank _.category (find (\c -> c.preset == preset) library.categories)

tagsOf :: Library -> String -> Array String
tagsOf library preset = maybe [] _.tags (find (\t -> t.preset == preset) library.tags)

collectionNamed :: Library -> String -> Maybe Collection
collectionNamed library name = find (\c -> c.name == name) library.collections

allTags :: Library -> Array String
allTags library = sort (nub (library.tags >>= _.tags))

-- ── changing it ──────────────────────────────────────────────────────────────

-- | A category the same as the bank's is no override at all.
setCategory :: String -> String -> String -> Library -> Library
setCategory preset bank category library =
  let
    rest = filter (\c -> c.preset /= preset) library.categories
    named = String.trim category
  in
    library { categories = if named == "" || named == bank then rest else snoc rest { preset, category: named } }

addTag :: String -> String -> Library -> Library
addTag preset tag library =
  let
    t = String.toLower (String.trim tag)
    current = tagsOf library preset
  in
    if t == "" || elem t current then library
    else withTags preset (snoc current t) library

removeTag :: String -> String -> Library -> Library
removeTag preset tag library = withTags preset (delete tag (tagsOf library preset)) library

withTags :: String -> Array String -> Library -> Library
withTags preset tags library =
  let rest = filter (\t -> t.preset /= preset) library.tags
  in library { tags = if tags == [] then rest else snoc rest { preset, tags } }

addCollection :: String -> Library -> Library
addCollection name library
  | String.trim name == "" || collectionNamed library (String.trim name) /= Nothing = library
  | otherwise = library { collections = snoc library.collections { name: String.trim name, presets: [] } }

renameCollection :: String -> String -> Library -> Library
renameCollection from to library
  | String.trim to == "" || collectionNamed library (String.trim to) /= Nothing = library
  | otherwise = library
      { collections = library.collections <#> \c -> if c.name == from then c { name = String.trim to } else c
      , onModule = library.onModule <#> \m -> if m == from then String.trim to else m
      }

removeCollection :: String -> Library -> Library
removeCollection name library = library
  { collections = filter (\c -> c.name /= name) library.collections
  , onModule = if library.onModule == Just name then Nothing else library.onModule
  }

toggleMember :: String -> String -> Library -> Library
toggleMember collection preset = onCollection collection \c ->
  if elem preset c.presets then c { presets = delete preset c.presets } else c { presets = snoc c.presets preset }

-- | Move a preset up (-1) or down (+1) its collection.
move :: String -> String -> Int -> Library -> Library
move collection preset by = onCollection collection \c -> fromMaybe c do
  at <- findIndex (_ == preset) c.presets
  without <- deleteAt at c.presets
  moved <- insertAt (clamp 0 (length without) (at + by)) preset without
  pure c { presets = moved }

onCollection :: String -> (Collection -> Collection) -> Library -> Library
onCollection name f library = library { collections = library.collections <#> \c -> if c.name == name then f c else c }

-- | A kept preset replaced by a new version keeps its place everywhere.
renameReference :: String -> String -> Library -> Library
renameReference from to library =
  let swap r = if r == from then to else r
  in
    library
      { categories = library.categories <#> \c -> c { preset = swap c.preset }
      , tags = library.tags <#> \t -> t { preset = swap t.preset }
      , collections = library.collections <#> \c -> c { presets = swap <$> c.presets }
      }

-- ── keeping it ───────────────────────────────────────────────────────────────

-- | A change to the library, as the user made it.
type Edit = Library -> Library

kind :: String
kind = "conspicillum-library"

base :: String -> String
base host = "http://" <> host <> ":3024"

ref :: String -> String
ref host = base host <> "/refs/conspicillum/library"

-- | The current library and its hash, or the empty one if none was kept yet.
load :: String -> Aff (Either String { library :: Library, hash :: Maybe String })
load host = do
  got <- Http.exchange "GET" (ref host) Nothing
  case got of
    Left problem -> pure (Left problem)
    Right { status: 404 } -> pure (Right { library: empty, hash: Nothing })
    Right { status: 200, body } -> case decode (readJSON :: String -> E { hash :: String }) body of
      Left problem -> pure (Left problem)
      Right { hash } -> map (\library -> { library, hash: Just hash }) <$> contentOf host hash
    Right { status, body } -> pure (Left (ref host <> ": " <> show status <> " " <> body))

contentOf :: String -> String -> Aff (Either String Library)
contentOf host hash = do
  got <- Http.get (base host <> "/content/" <> hash)
  pure (got >>= decode (readJSON :: String -> E { payload :: String }) >>= \c -> decode (readJSON :: String -> E Library) c.payload)

-- | Make the edit to the library as it stands at `expected`, keep the result
-- | and move the ref to it. If the ref has moved on, make the edit again to
-- | the library it now points at: a few times, then give up and say so.
-- | Returns the library as kept, which includes any other page's changes.
keep :: String -> { hash :: Maybe String, library :: Library } -> Edit -> Aff (Either String { library :: Library, hash :: Maybe String })
keep host = attempt 5
  where
  attempt tries at edit
    | tries <= 0 = pure (Left "the library kept changing under this page; reload to see it")
    | otherwise = do
        let next = edit at.library
        saved <- Http.post (base host <> "/content") (writeJSON { kind, payload: writeJSON next })
        case saved >>= decode (readJSON :: String -> E { hash :: String }) of
          Left problem -> pure (Left problem)
          Right { hash } | at.hash == Just hash -> pure (Right { library: next, hash: Just hash })
          Right { hash } -> do
            moved <- Http.exchange "POST" (ref host) (Just (writeJSON { to: hash, expected: toNullable at.hash }))
            case moved of
              Left problem -> pure (Left problem)
              Right { status: 200 } -> pure (Right { library: next, hash: Just hash })
              Right { status: 409, body } -> case decode (readJSON :: String -> E { current :: Nullable { hash :: String } }) body of
                Left problem -> pure (Left problem)
                Right { current } -> case toMaybe current of
                  Nothing -> attempt (tries - 1) { hash: Nothing, library: empty } edit
                  Just now -> contentOf host now.hash >>= case _ of
                    Left problem -> pure (Left problem)
                    Right library -> attempt (tries - 1) { hash: Just now.hash, library } edit
              Right { status, body } -> pure (Left (ref host <> ": " <> show status <> " " <> body))

decode :: forall a e. Show e => (String -> Either e a) -> String -> Either String a
decode f text = case f text of
  Left e -> Left (show e)
  Right a -> Right a
