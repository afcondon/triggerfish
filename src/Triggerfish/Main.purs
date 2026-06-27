-- | Triggerfish shell. Triggerfish is now a rack of direct-manipulation
-- | instruments over the BEAM-native modules, not just Odonus; this root holds
-- | a small instrument selector and mounts one at a time. Only the active
-- | instrument is mounted, so only one clock/MIDI path runs (switching
-- | unmounts the other and stops its scheduler).
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
    , case st.which of
        Odo -> HH.slot_ _odo unit Odonus.component unit
        Bal -> HH.slot_ _bal unit Balistes.component unit
        Sel -> HH.slot_ _sel unit Selene.component unit
        Vet -> HH.slot_ _vet unit Vetula.component unit
    ]

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
