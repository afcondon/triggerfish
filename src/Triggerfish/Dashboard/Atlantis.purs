-- | **The Atlantis page**: the rig's daemons, as Bosun supervises them
-- | (`docs/kb/plans/dashboard.md`, "The app's pages": 1e, 3a–3c).
-- |
-- | Grouped by **branch**, not listed: the trunk (Architeuthis and Diaphus)
-- | every rig path needs, then the branches that are independent of each
-- | other, each saying what it serves. A daemon has a lamp, its state and a
-- | restart; the group is raised and lowered as one.
-- |
-- | Bosun's "ok" means it took the request, not that a process moved (twice
-- | measured: `bosun-supervise-orphans`). So a restart is shown as asked until
-- | `/state` shows the service move, and as "nothing moved" if it does not.
module Triggerfish.Dashboard.Atlantis
  ( Asked
  , Outcome(..)
  , Handlers
  , outcome
  , view
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, find, null)
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Bosun (Health, Lamp(..), Service, lampOf)
import Triggerfish.DeepStar (Check)

-- | A restart asked of Bosun: when, and the service as it was.
type Asked = { service :: String, at :: Number, before :: Maybe Service }

data Outcome = Waiting | Moved | NothingMoved

derive instance Eq Outcome

-- | How long a restart may take before "nothing moved" is the honest answer.
patienceMs :: Number
patienceMs = 20000.0

outcome :: Number -> Maybe Health -> Asked -> Outcome
outcome now h a = case a.before, find (\s -> s.id == a.service) (maybe [] _.services h) of
  Just b, Just s | s.restarts /= b.restarts || s.since /= b.since -> Moved
  _, _ | now - a.at > patienceMs -> NothingMoved
  _, _ -> Waiting

type Handlers i =
  { restart :: String -> i
  , group :: String -> i        -- "up" or "down"
  , confirmDown :: Boolean -> i
  }

type Branch = { name :: String, serves :: String, ids :: Array String }

branches :: Array Branch
branches =
  [ { name: "The trunk", serves: "every rig path: the engine, and the MIDI it sends on the beat", ids: [ "architeuthis", "diaphus" ] }
  , { name: "Samples", serves: "Conspicillum, sample legs, Limulus's sounds", ids: [ "superdirt" ] }
  , { name: "ES-9", serves: "Selene's ES-9 banks, poly legs, Quadrat's strikes", ids: [ "es9-daemon" ] }
  , { name: "FH-2", serves: "configures the FH-2: Selene's FH-2 banks, the drum gates. Notes reach it over MIDI without these", ids: [ "fh2-daemon", "fh2-drumkit" ] }
  , { name: "Piano", serves: "piano legs, Vetula's audition", ids: [ "continuo" ] }
  , { name: "Sampling", serves: "Quadrat and the Friend: sets, takes, harvests, the CV relay", ids: [ "friends-of-itajara", "itajara" ] }
  , { name: "Pages and stores", serves: "the pages themselves, the artefact store, the rig doctor", ids: [ "triggerfish-frontend", "conspicillum-frontend", "limulus", "amphora", "deepstar" ] }
  ]

view
  :: forall w i
   . Handlers i
  -> { now :: Number, health :: Maybe Health, asked :: Array Asked, confirmingDown :: Boolean, doctor :: Maybe (Array Check) }
  -> HH.HTML w i
view on st = case st.health of
  Nothing ->
    HH.section [ cls "atl" ]
      [ HH.h2_ [ HH.text "Atlantis" ]
      , HH.p [ cls "atl-none" ]
          [ HH.text "Bosun is out of reach (:3994), so the rig's daemons cannot be shown or started from here. "
          , HH.text "Start the Atlantis group's supervisor, and this page fills in."
          ]
      ]
  Just h ->
    HH.section [ cls "atl" ]
      ( [ HH.div [ cls "atl-head" ]
            [ HH.h2_ [ HH.text "Atlantis" ]
            , HH.span [ cls ("atl-phase " <> h.phase) ] [ HH.text (phaseText h) ]
            , HH.span [ cls "spacer" ] []
            , groupButtons h
            ]
        ]
          <> map (branch h) (branches <> others h)
      )
  where
  phaseText h
    | h.desired == "up" = "raised: Bosun keeps these running"
    | otherwise = "held: nothing is started or kept up"

  groupButtons h
    | h.desired /= "up" = HH.button [ cls "btn", HE.onClick \_ -> on.group "up" ] [ HH.text "Raise the rig" ]
    | st.confirmingDown =
        HH.span [ cls "atl-confirm" ]
          [ HH.text "Stop every daemon here? "
          , HH.button [ cls "btn panic", HE.onClick \_ -> on.group "down" ] [ HH.text "Lower the rig" ]
          , HH.button [ cls "btn", HE.onClick \_ -> on.confirmDown false ] [ HH.text "Keep it up" ]
          ]
    | otherwise = HH.button [ cls "btn", HE.onClick \_ -> on.confirmDown true ] [ HH.text "Lower the rig…" ]

  -- Services Bosun has that no branch names, so nothing it runs is hidden.
  others h =
    let
      named = concatMap _.ids branches
      rest = map _.id (filter (\s -> not (s.id `elem` named)) h.services)
    in if null rest then [] else [ { name: "Other", serves: "in the group, not yet placed on a branch", ids: rest } ]

  branch h b =
    let svcs = filter (\s -> s.id `elem` b.ids) h.services
    in
      if null svcs then HH.text ""
      else
        HH.div [ cls "atl-branch" ]
          [ HH.div [ cls "atl-bhead" ] [ HH.h3_ [ HH.text b.name ], HH.span [ cls "atl-serves" ] [ HH.text b.serves ] ]
          , HH.div [ cls "atl-rows" ] (deviceRows b <> map (row h) svcs)
          ]

  -- The hardware itself, from the rig doctor (DeepStar), above the daemons
  -- that drive it: Bosun can say a daemon runs, not that its device is
  -- there. Today the ES-9's presence on the USB bus.
  deviceRows b = case b.name of
    "ES-9" -> case st.doctor >>= find (\c -> c.name == "ES-9 present") of
      Just c ->
        [ HH.div [ cls "atl-row device" ]
            [ HH.span [ cls ("lamp" <> statusCls c.status) ] [ HH.i_ [], HH.text "the ES-9" ]
            , HH.span [ cls "atl-state" ] [ HH.text (if c.status == "ok" then "on the USB bus" else if c.status == "down" then "not on the USB bus" else c.status) ]
            , HH.span [ cls "atl-restarts" ] []
            , HH.span [ cls "atl-asked" ] [ HH.text c.detail ]
            , HH.span [] []
            ]
        ]
      Nothing -> [ HH.div [ cls "atl-row device" ] [ HH.span [ cls "lamp" ] [ HH.i_ [], HH.text "the ES-9" ], HH.span [ cls "atl-state" ] [ HH.text "unknown: DeepStar (:3027) is out of reach" ] ] ]
    _ -> []
  statusCls = case _ of
    "ok" -> " live"
    "down" -> " dead"
    _ -> " coming"
  -- the doctor's word on a daemon's control socket, where it has one
  socketNote s = case st.doctor >>= find (\c -> c.name == s.id && c.status == "down") of
    Just _ -> " · its socket does not answer"
    Nothing -> ""

  row h s =
    HH.div [ cls "atl-row" ]
      [ HH.span [ cls ("lamp" <> lampCls (lampOf s)) ] [ HH.i_ [], HH.text s.id ]
      , HH.span [ cls "atl-state" ] [ HH.text (s.state <> (if s.gaveUp then ", gave up" else "") <> socketNote s) ]
      , HH.span [ cls "atl-restarts" ] [ HH.text (if s.restarts == 0 then "" else show s.restarts <> " restarts") ]
      , HH.span [ cls "atl-asked" ] [ HH.text (askedText s) ]
      , HH.button
          [ cls "btn small", HP.disabled (h.desired /= "up" || waiting s)
          , HP.title (restartTip s)
          , HE.onClick \_ -> on.restart s.id
          ]
          [ HH.text "Restart" ]
      ]

  lastAsk s = find (\a -> a.service == s.id) st.asked
  waiting s = maybe false (\a -> outcome st.now st.health a == Waiting) (lastAsk s)
  askedText s = case lastAsk s of
    Nothing -> ""
    Just a -> case outcome st.now st.health a of
      Waiting -> "restarting…"
      Moved -> "restarted"
      NothingMoved -> "Bosun said yes, but nothing moved"

  restartTip s = case s.id of
    "architeuthis" -> "Every page loses the rig for a few seconds; the rig's loops and marks are lost."
    "diaphus" -> "The rig's MIDI stops while it restarts. macOS may ask again for Local Network permission."
    "fh2-daemon" -> "Power the FH-2 on first: with it unplugged this gives up again after five tries."
    _ -> "Ask Bosun to restart " <> s.id <> "."

  lampCls = case _ of
    Up -> " live"
    Coming -> " coming"
    Down -> " dead"

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
