-- | Triggerfish shell. Triggerfish is a rack of direct-manipulation
-- | instruments over the BEAM-native modules; this root holds a small
-- | instrument selector and shows one at a time.
-- |
-- | All four instruments stay MOUNTED at once — the selector only toggles which
-- | is VISIBLE (`display:none` for the rest). So each module keeps playing when
-- | you switch away: start Odonus, switch to Balistes and start it too, and
-- | they run together as a four-module rig jam (each on its own MIDI channel /
-- | CV, so no conflict). With the rig's Link clock all four phase-lock to the
-- | same anchor; in free-run (no rig) they each run at 120 from their own mount
-- | time and can drift, so a tight multi-module jam wants the rig.
module Triggerfish.Main where

import Prelude

import Data.Const (Const)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Triggerfish.Odonus.Grid as Odonus
import Triggerfish.Balistes.Component as Balistes
import Triggerfish.Selene.Component as Selene
import Vetula.App as Vetula

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI root unit body

data Which = Odo | Bal | Sel | Vet

derive instance Eq Which

data RAction = Pick Which

type RState = { which :: Which }

type Slots =
  ( odo :: H.Slot (Const Void) Void Unit
  , bal :: H.Slot (Const Void) Void Unit
  , sel :: H.Slot (Const Void) Void Unit
  , vet :: H.Slot (Const Void) Void Unit
  )

_odo :: Proxy "odo"
_odo = Proxy

_bal :: Proxy "bal"
_bal = Proxy

_sel :: Proxy "sel"
_sel = Proxy

_vet :: Proxy "vet"
_vet = Proxy

root :: forall q i o m. MonadAff m => H.Component q i o m
root =
  H.mkComponent
    { initialState: \_ -> { which: Bal }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction :: forall o m. RAction -> H.HalogenM RState RAction Slots o m Unit
handleAction (Pick w) = H.modify_ _ { which = w }

render :: forall m. MonadAff m => RState -> H.ComponentHTML RAction Slots m
render st =
  HH.div_
    [ switchBar st
    -- All four are always in the tree (hence always mounted + running); the
    -- active one is shown, the rest are display:none but keep playing.
    , pane (st.which == Odo) (HH.slot_ _odo unit Odonus.component unit)
    , pane (st.which == Bal) (HH.slot_ _bal unit Balistes.component unit)
    , pane (st.which == Sel) (HH.slot_ _sel unit Selene.component unit)
    , pane (st.which == Vet) (HH.slot_ _vet unit Vetula.component unit)
    ]

-- A mounted-but-maybe-hidden pane. `display:none` keeps the component alive
-- (and its scheduler/MIDI running) while removing it from layout.
pane :: forall m. Boolean -> H.ComponentHTML RAction Slots m -> H.ComponentHTML RAction Slots m
pane visible content =
  HH.div [ style (if visible then "" else "display:none") ] [ content ]

-- A small floating selector, top-right, in the Hainbach idiom.
switchBar :: forall m. RState -> H.ComponentHTML RAction Slots m
switchBar st =
  HH.div
    [ style $ "position:fixed;top:10px;right:14px;z-index:50;display:flex;gap:0;"
        <> "border:1px solid #00000033;border-radius:7px;overflow:hidden;"
        <> "box-shadow:0 1px 4px #0000002a;font-family:Georgia,serif" ]
    [ seg "ODONUS" (st.which == Odo) (Pick Odo)
    , seg "BALISTES" (st.which == Bal) (Pick Bal)
    , seg "SELENE" (st.which == Sel) (Pick Sel)
    , seg "VETULA" (st.which == Vet) (Pick Vet)
    ]

seg :: forall m. String -> Boolean -> RAction -> H.ComponentHTML RAction Slots m
seg label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:6px 14px;border:0;cursor:pointer;font-size:11px;letter-spacing:0.12em;"
        <> "text-transform:uppercase;color:" <> (if active then "#1c1a12" else "#5a564b")
        <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#e9e5d9,#dcd8c9)") ]
    [ HH.text label ]

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")
