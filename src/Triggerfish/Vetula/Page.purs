-- | Vetula, made to fit the one-machine shell (`Triggerfish.Standalone`).
-- |
-- | Vetula speaks its own query type, and has no continuous frame loop, so in
-- | the Triggerfish page the shell polled it: for its preset chip, and in
-- | Atlantis for its rig payload, re-pushed once an edit settles. This wrapper
-- | does that polling itself and answers the shell's `SourceQuery` by
-- | translating it.
-- |
-- | What passes from Vetula to Odonus (its key, a card's chords) goes by the
-- | harmony routes on the rig (`odonus_feeds`), not from tab to tab.
module Triggerfish.Vetula.Page
  ( Output(..)
  , component
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Triggerfish.Glyph (ChipView)
import Triggerfish.SourceQuery as SQ
import Triggerfish.Transport (Sounding(..))
import Type.Proxy (Proxy(..))
import Vetula.App as Vetula

data Output
  = Chip (Maybe ChipView)
  | Armed Boolean

type State =
  { sounding :: Sounding
  , chip :: Maybe (Maybe ChipView)
  -- The rig payload as last seen and as last sent: a change is re-pushed once
  -- it has held still for one poll, so a drag sends once, after it stops.
  , brushPrev :: String
  , brushSent :: String
  -- Vetula answers nothing until its lattice is built (tens of seconds), so
  -- the poll runs forked, and only one at a time.
  , busy :: Maybe (Ref Boolean)
  }

data Action
  = Init
  | Poll
  | FromVetula Vetula.Output

type Slots = (vet :: H.Slot Vetula.SourceQuery Vetula.Output Unit)

_vet :: Proxy "vet"
_vet = Proxy

component :: forall i. H.Component SQ.Query i Output Aff
component = H.mkComponent
  { initialState: \_ ->
      { sounding: Silent, chip: Nothing
      , brushPrev: "", brushSent: "", busy: Nothing }
  , render: \_ -> HH.slot _vet unit Vetula.component unit FromVetula
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init
      }
  }

type M = H.HalogenM State Action Slots Output Aff

handleAction :: Action -> M Unit
handleAction = case _ of
  Init -> do
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    gate <- liftEffect (Ref.new false)
    H.modify_ _ { busy = Just gate }
    void $ liftEffect $ setInterval 100 (HS.notify listener Poll)

  Poll -> do
    st <- H.get
    for_ st.busy \gate -> do
      busy <- liftEffect (Ref.read gate)
      unless busy do
        liftEffect (Ref.write true gate)
        void $ H.fork do
          poll
          liftEffect (Ref.write false gate)

  FromVetula out -> case out of
    Vetula.ArmChanged on -> H.raise (Armed on)
    Vetula.StageChanged _ -> pure unit

-- | Every write is guarded on a change: this runs ten times a second.
poll :: M Unit
poll = do
  mchip <- H.query _vet unit (Vetula.AskChip identity)
  for_ mchip \cv -> do
    st <- H.get
    when (st.chip /= Just cv) do
      H.modify_ _ { chip = Just cv }
      H.raise (Chip cv)
  -- Vetula has no incremental rig path: a settled change to its payload is
  -- pushed again, and only while it is the rig that sounds it.
  st <- H.get
  when (st.sounding == Rig) do
    msig <- H.query _vet unit (Vetula.AskBrushSig identity)
    for_ msig \sig -> do
      when (sig == st.brushPrev && sig /= st.brushSent) do
        void $ H.query _vet unit (Vetula.SetSounding Rig unit)
        H.modify_ _ { brushSent = sig }
      when (sig /= st.brushPrev) (H.modify_ _ { brushPrev = sig })

-- | The shell's queries, in Vetula's terms. What Vetula has no counterpart for
-- | (lanes, a clock, the routing table, a pitch set to follow) is unanswered.
handleQuery :: forall a. SQ.Query a -> M (Maybe a)
handleQuery = case _ of
  SQ.AskSource k -> H.query _vet unit (Vetula.AskSource k)
  SQ.SetStagePath segs a -> H.query _vet unit (Vetula.SetStagePath segs a)
  SQ.SyncFree t0 tempo a -> H.query _vet unit (Vetula.SyncFree t0 tempo a)
  SQ.SetSounding s a -> do
    H.modify_ _ { sounding = s }
    H.query _vet unit (Vetula.SetSounding s a)
  SQ.AskSounding k -> H.query _vet unit (Vetula.AskSounding k)
  SQ.AskLibrary k -> H.query _vet unit (Vetula.AskLibrary k)
  SQ.LoadEntry i a -> H.query _vet unit (Vetula.LoadEntry i a)
  SQ.ImportText t k -> H.query _vet unit (Vetula.ImportText t k)
  SQ.Capture a -> H.query _vet unit (Vetula.Capture a)
  SQ.AskBank k -> H.query _vet unit (Vetula.AskBank k)
  SQ.RecallSlot i a -> H.query _vet unit (Vetula.RecallSlot i a)
  SQ.StarSlot i a -> H.query _vet unit (Vetula.StarSlot i a)
  SQ.DeleteSlot i a -> H.query _vet unit (Vetula.DeleteSlot i a)
  SQ.PutLane _ _ _ -> pure Nothing
  SQ.PutSource _ _ -> pure Nothing
  SQ.AskClock _ -> pure Nothing
  SQ.SetRouting _ _ -> pure Nothing
