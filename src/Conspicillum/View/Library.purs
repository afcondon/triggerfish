-- | **The library tab**: every preset, factory and kept, as a table to search,
-- | sort, categorise and tag, and the collections that pick which of them the
-- | module shows, in what order.
-- |
-- | A component of its own: what is being browsed (the collection, the
-- | search, the sort, a half-typed tag) is this tab's business. The library
-- | itself belongs to the page, which keeps it; every change goes up as a new
-- | library, and auditioning a preset goes up as its reference.
module Conspicillum.View.Library
  ( PresetRow
  , Input
  , Output(..)
  , component
  ) where

import Prelude

import Conspicillum.Library (Edit, Library, addCollection, addTag, allTags, categoryOf, collectionNamed, move, removeCollection, removeTag, renameCollection, setCategory, tagsOf, toggleMember)
import Data.Foldable (for_)
import Data.Array (elem, filter, findIndex, length, nub, null, sort, sortBy)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- | A preset as the table shows it. `ref` is the library's reference to it.
type PresetRow =
  { ref :: String
  , name :: String
  , about :: String
  , bank :: String
  , set :: String
  , kept :: Boolean
  , playable :: Boolean
  }

type Input = { rows :: Array PresetRow, library :: Library, current :: Maybe String }

data Output
  = Audition String
  | Change Edit

data Sort = InOrder | ByName | ByCategory | ByMaterial

derive instance eqSort :: Eq Sort

type State =
  { rows :: Array PresetRow
  , library :: Library
  , current :: Maybe String
  -- | The collection being looked at; `Nothing` is all presets.
  , selected :: Maybe String
  -- | Show only the selected collection's presets, in its order.
  , membersOnly :: Boolean
  , search :: String
  , tag :: Maybe String
  , category :: Maybe String
  , sorting :: Sort
  , newCollection :: String
  , renaming :: Maybe String
  , tagDraft :: Maybe { ref :: String, text :: String }
  }

data Action
  = Receive Input
  | Select (Maybe String)
  | MembersOnly Boolean
  | Search String
  | FilterTag (Maybe String)
  | FilterCategory (Maybe String)
  | SortBy Sort
  | NewCollectionInput String
  | CreateCollection
  | StartRename
  | RenameInput String
  | FinishRename
  | RemoveSelected
  | PutOnModule (Maybe String)
  | Toggle String
  | Move String Int
  | Category String String String
  | TagInput String String
  | TagKey String KeyboardEvent.KeyboardEvent
  | Untag String String
  | Play String

component :: forall q m. MonadAff m => H.Component q Input Output m
component = H.mkComponent
  { initialState: \i ->
      { rows: i.rows
      , library: i.library
      , current: i.current
      , selected: i.library.onModule
      , membersOnly: isJust i.library.onModule
      , search: ""
      , tag: Nothing
      , category: Nothing
      , sorting: if isJust i.library.onModule then InOrder else ByCategory
      , newCollection: ""
      , renaming: Nothing
      , tagDraft: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
  }

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

-- ── what is shown ────────────────────────────────────────────────────────────

shown :: State -> Array PresetRow
shown st =
  let
    members = maybe [] _.presets (st.selected >>= collectionNamed st.library)
    inCollection r = elem r.ref members
    matches r =
      (not st.membersOnly || isNothing' st.selected || inCollection r)
        && maybe true (\t -> elem t (tagsOf st.library r.ref)) st.tag
        && maybe true (\c -> categoryOf st.library r.ref r.bank == c) st.category
        && searched r
    needle = String.toLower (String.trim st.search)
    searched r =
      needle == ""
        || String.contains (String.Pattern needle) (String.toLower (r.name <> " " <> r.about <> " " <> r.set <> " " <> categoryOf st.library r.ref r.bank <> " " <> String.joinWith " " (tagsOf st.library r.ref)))
    kept = filter matches st.rows
    position r = fromMaybe 100000 (findIndex (_ == r.ref) members)
  in
    case st.sorting of
      InOrder -> if isNothing' st.selected then kept else sortBy (comparing position) kept
      ByName -> sortBy (comparing (String.toLower <<< _.name)) kept
      ByCategory -> sortBy (comparing \r -> categoryOf st.library r.ref r.bank) kept
      ByMaterial -> sortBy (comparing _.set) kept
  where
  isNothing' = maybe true (const false)

categories :: State -> Array String
categories st = sort (nub (st.rows <#> \r -> categoryOf st.library r.ref r.bank))

-- ── rendering ────────────────────────────────────────────────────────────────

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ cls "library" ]
    [ collectionsBar
    , HH.div [ cls "libfilters" ]
        [ HH.input [ cls "search", HP.placeholder "search names, notes, sets, tags", HP.value st.search, HE.onValueInput Search ]
        , HH.select [ HE.onValueChange \v -> FilterCategory (if v == "" then Nothing else Just v) ]
            ([ HH.option [ HP.value "", HP.selected (st.category == Nothing) ] [ HH.text "every category" ] ]
              <> (categories st <#> \c -> HH.option [ HP.value c, HP.selected (st.category == Just c) ] [ HH.text c ]))
        , HH.select [ HE.onValueChange sortOf ]
            [ sortOption "order" "in the collection's order" InOrder
            , sortOption "name" "by name" ByName
            , sortOption "category" "by category" ByCategory
            , sortOption "material" "by material" ByMaterial
            ]
        , HH.span [ cls "sp" ] []
        , HH.span [ cls "count" ] [ HH.text (show (length rows) <> " of " <> show (length st.rows)) ]
        ]
    , if null (allTags st.library) then HH.text ""
      else
        HH.div [ cls "tagfilter" ]
          ( [ HH.span [ cls "lab" ] [ HH.text "tags" ] ]
              <> (allTags st.library <#> \t ->
                HH.button [ cls (if st.tag == Just t then "chip on" else "chip"), HE.onClick \_ -> FilterTag (if st.tag == Just t then Nothing else Just t) ] [ HH.text t ])
          )
    , HH.table [ cls "libtable" ]
        ( [ HH.tr_
              ( (if st.selected == Nothing then [] else [ HH.th [ HP.title "in this collection" ] [ HH.text "in" ] ])
                  <> [ HH.th_ [ HH.text "preset" ], HH.th_ [ HH.text "category" ], HH.th_ [ HH.text "tags" ], HH.th_ [ HH.text "material" ], HH.th_ [] ]
                  <> (if ordering then [ HH.th_ [ HH.text "order" ] ] else [])
              )
          ]
            <> (rows <#> row)
        )
    , HH.datalist [ HP.id "categories" ] (categories st <#> \c -> HH.option [ HP.value c ] [])
    ]
  where
  rows = shown st
  members = maybe [] _.presets (st.selected >>= collectionNamed st.library)
  ordering = st.selected /= Nothing && st.sorting == InOrder && st.membersOnly

  sortOption value label s = HH.option [ HP.value value, HP.selected (st.sorting == s) ] [ HH.text label ]
  sortOf = case _ of
    "name" -> SortBy ByName
    "category" -> SortBy ByCategory
    "material" -> SortBy ByMaterial
    _ -> SortBy InOrder

  collectionsBar =
    HH.div [ cls "collections" ]
      ( [ HH.span [ cls "lab" ] [ HH.text "collections" ]
        , pill Nothing "all presets"
        ]
          <> (st.library.collections <#> \c -> pill (Just c.name) (c.name <> " · " <> show (length c.presets)))
          <>
            [ HH.input
                [ cls "newcollection", HP.placeholder "new collection", HP.value st.newCollection
                , HE.onValueInput NewCollectionInput
                , HE.onKeyDown \k -> if KeyboardEvent.key k == "Enter" then CreateCollection else NewCollectionInput st.newCollection
                ]
            ]
          <> [ HH.div [ cls "collectionctl" ] selectedControls ]
      )

  pill name label =
    HH.button
      [ cls ("pill" <> (if st.selected == name then " on" else "") <> (if st.library.onModule == name then " module" else ""))
      , HP.title (if st.library.onModule == name then "the presets drawer shows this" else "")
      , HE.onClick \_ -> Select name
      ]
      [ HH.text ((if st.library.onModule == name then "▸ " else "") <> label) ]

  selectedControls = case st.selected of
    Nothing ->
      [ HH.button [ cls (if st.library.onModule == Nothing then "on" else ""), HE.onClick \_ -> PutOnModule Nothing ]
          [ HH.text (if st.library.onModule == Nothing then "the drawer shows all presets" else "show all presets in the drawer") ]
      ]
    Just name ->
      [ HH.button [ cls (if st.library.onModule == Just name then "on" else ""), HE.onClick \_ -> PutOnModule (Just name) ]
          [ HH.text (if st.library.onModule == Just name then "in the drawer" else "put in the drawer") ]
      , HH.label [ cls "check" ]
          [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked st.membersOnly, HE.onChecked MembersOnly ]
          , HH.text "only its presets"
          ]
      ]
        <> case st.renaming of
          Just draft ->
            [ HH.input [ HP.value draft, HE.onValueInput RenameInput, HE.onKeyDown \k -> if KeyboardEvent.key k == "Enter" then FinishRename else RenameInput draft ]
            , HH.button [ HE.onClick \_ -> FinishRename ] [ HH.text "rename" ]
            ]
          Nothing ->
            [ HH.button [ HE.onClick \_ -> StartRename ] [ HH.text "rename" ]
            , HH.button [ HP.title "the presets stay; only the collection goes", HE.onClick \_ -> RemoveSelected ] [ HH.text "remove" ]
            ]

  row r =
    let
      isMember = elem r.ref members
      category = categoryOf st.library r.ref r.bank
      tags = tagsOf st.library r.ref
    in
      HH.tr [ cls ((if st.current == Just r.ref then "current" else "") <> (if r.playable then "" else " unplayable")) ]
        ( ( case st.selected of
              Nothing -> []
              Just _ -> [ HH.td_ [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked isMember, HE.onChecked \_ -> Toggle r.ref ] ] ]
          )
            <>
              [ HH.td [ cls "pname" ]
                  [ HH.button [ cls "name", HP.title (if r.playable then "play it" else "its material is not here"), HP.disabled (not r.playable), HE.onClick \_ -> Play r.ref ] [ HH.text r.name ]
                  , HH.div [ cls "about" ] [ HH.text r.about ]
                  ]
              , HH.td_
                  [ HH.input
                      [ cls "category", HP.value category, HP.attr (HH.AttrName "list") "categories"
                      , HP.title ("its bank is " <> r.bank <> "; clear it to go back")
                      , HE.onValueChange (Category r.ref r.bank)
                      ]
                  ]
              , HH.td [ cls "tags" ]
                  ( (tags <#> \t -> HH.span [ cls "chip" ] [ HH.text t, HH.button [ HE.onClick \_ -> Untag r.ref t ] [ HH.text "×" ] ])
                      <>
                        [ HH.input
                            [ cls "newtag", HP.placeholder "+ tag"
                            , HP.value (maybe "" (\d -> if d.ref == r.ref then d.text else "") st.tagDraft)
                            , HE.onValueInput (TagInput r.ref)
                            , HE.onKeyDown (TagKey r.ref)
                            ]
                        ]
                  )
              , HH.td [ cls "set" ] [ HH.text r.set ]
              , HH.td [ cls "kind" ] [ HH.text (if r.kept then "kept" else "factory") ]
              ]
            <>
              ( if ordering then
                  [ HH.td [ cls "order" ]
                      [ HH.button [ HE.onClick \_ -> Move r.ref (-1) ] [ HH.text "↑" ]
                      , HH.button [ HE.onClick \_ -> Move r.ref 1 ] [ HH.text "↓" ]
                      ]
                  ]
                else []
              )
        )

-- ── handling ─────────────────────────────────────────────────────────────────

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive i -> H.modify_ \st ->
    st
      { rows = i.rows
      , library = i.library
      , current = i.current
      -- A collection that is gone cannot stay selected.
      , selected = st.selected >>= \s -> map _.name (collectionNamed i.library s)
      }

  Select name -> H.modify_ \st ->
    let
      has = maybe false (not <<< null <<< _.presets) (name >>= collectionNamed st.library)
    in
      st { selected = name, membersOnly = has, sorting = if has then InOrder else st.sorting, renaming = Nothing }

  MembersOnly on -> H.modify_ _ { membersOnly = on }
  Search s -> H.modify_ _ { search = s }
  FilterTag t -> H.modify_ _ { tag = t }
  FilterCategory c -> H.modify_ _ { category = c }
  SortBy s -> H.modify_ _ { sorting = s }
  NewCollectionInput s -> H.modify_ _ { newCollection = s }

  CreateCollection -> do
    st <- H.get
    let name = String.trim st.newCollection
    unless (name == "") do
      change (addCollection name)
      H.modify_ _ { newCollection = "", selected = Just name, membersOnly = false }

  StartRename -> H.modify_ \st -> st { renaming = st.selected }
  RenameInput s -> H.modify_ _ { renaming = Just s }

  FinishRename -> do
    st <- H.get
    case st.selected, st.renaming of
      Just from, Just to -> do
        change (renameCollection from to)
        H.modify_ _ { selected = Just (String.trim to), renaming = Nothing }
      _, _ -> H.modify_ _ { renaming = Nothing }

  RemoveSelected -> do
    st <- H.get
    for_ st.selected \name -> do
      change (removeCollection name)
      H.modify_ _ { selected = Nothing }

  PutOnModule name -> change _ { onModule = name }

  Toggle ref -> do
    st <- H.get
    for_ st.selected \name -> change (toggleMember name ref)

  Move ref by -> do
    st <- H.get
    for_ st.selected \name -> change (move name ref by)

  Category ref bank category -> change (setCategory ref bank category)

  TagInput ref text -> H.modify_ _ { tagDraft = Just { ref, text } }

  TagKey ref key -> when (KeyboardEvent.key key == "Enter") do
    st <- H.get
    for_ st.tagDraft \d -> when (d.ref == ref) do
      change (addTag ref d.text)
      H.modify_ _ { tagDraft = Nothing }

  Untag ref tag -> change (removeTag ref tag)

  Play ref -> H.raise (Audition ref)
  where
  change edit = do
    H.modify_ \st -> st { library = edit st.library }
    H.raise (Change edit)
