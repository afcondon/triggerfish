-- | `Triggerfish.Selene.Backward` — routing read from the jack.
-- |
-- | The forward router asks *"I am editing Odonus head II, where does it come
-- | out?"* This asks the question you actually hold standing at the rack:
-- |
-- |   **what is supposed to be driving this jack, and is anything?**
-- |
-- | See `docs/DESIGN-routing-backward.md`. Two reasons it is the stronger view:
-- |
-- | **Contention only exists on the output side.** Two sources can want one jack;
-- | one source wanting two jacks is the feature, not a conflict. So every
-- | conflict is invisible in the forward view *by construction* — you would have
-- | to hold four rows in your head and notice two of them naming the same
-- | hardware. That is exactly how the FH-2 MCV collision shipped.
-- |
-- | **Free jacks are a first-class answer.** "Which outputs are unspoken for?" is
-- | asked constantly while patching, and the forward view cannot express it at
-- | all — it requires mentally subtracting every source's legs from the rig's
-- | inventory. Here it is the rows with an empty claim.
-- |
-- | ## Three kinds of fact, kept apart
-- |
-- | The columns are not decoration; keeping them distinct is the whole value.
-- |
-- |   * **claimed** — what the routing table says should drive it.
-- |   * **layout** — what the rig is *configured* to receive there. Declared
-- |     independently of the table, so a disagreement between the two columns is
-- |     itself a finding: a source aimed at a jack the hardware is not set up to
-- |     drive, or a configured voice nobody is playing.
-- |   * **traffic** — what the monitor actually observed. Declared and observed
-- |     fail independently: a route can be perfectly reachable and never carry a
-- |     note because the machine never armed, and from the rack those look
-- |     identical.
-- |
-- | ## What this deliberately does NOT show
-- |
-- | **Device liveness.** With the modular switched off the FH-2's USB port still
-- | enumerates, so every FH-2 route reads `ok` — `Reach = Reachable` only ever
-- | meant "a port with that name exists". The honest test is a `device-status`
-- | round trip on `~/.fh2/control.sock`, which a browser cannot open. Showing a
-- | confident row for a powered-down module would rebuild, deliberately and in
-- | colour, the exact failure this view exists to remove. So liveness waits for
-- | Selene's API rather than being faked here.
module Triggerfish.Selene.Backward
  ( Row
  , Traffic
  , rows
  , conflicted
  , panel
  ) where

import Prelude

import Data.Array (any, filter, length, null, nub, sortWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String.Common (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Triggerfish.Rig (RigConfig, rigOutputs)
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Monitor as Mon
import Triggerfish.Selene.Layout (Assignment, Layout, Output, outputKey)
import Triggerfish.Selene.Layout as Layout

type Traffic = { hits :: Int, offs :: Int, recent :: Boolean }

type Row =
  { output :: Output
  , claimedBy :: Array String        -- ^ source labels; more than one is a conflict
  , kinds :: Array String            -- ^ what kind of signal each claimant sends
  , layout :: Maybe Assignment       -- ^ what the rig is configured to receive
  , traffic :: Maybe Traffic
  }

-- | One row per physical output the rig has — claimed or not.
rows :: RigConfig -> Layout -> RM.Table -> Array Mon.Row -> Array Row
rows cfg lay tbl obs = map build (rigOutputs cfg)
  where
  -- Every live leg, paired with the jack it lands on. Legs that are not a jack
  -- at all (MIDI, continuo) simply never match a row, which is right: a channel
  -- is not scarce the way a jack is and does not belong in this table.
  placed = do
    route <- tbl
    leg <- filter _.on route.legs
    case RM.outputOf leg.dest of
      Nothing -> []
      Just o -> [ { output: o, source: route.source, dest: leg.dest } ]

  build o =
    let here = filter (\p -> p.output == o) placed
        wires = do
          p <- here
          case RM.wireOf p.dest of
            Nothing -> []
            Just w -> [ w ]
        seen = filter (\r -> any (\w -> Mon.matches w r) wires) obs
    in
      { output: o
      , claimedBy: nub (map (\p -> RM.sourceLabel p.source) here)
      , kinds: nub (map (\p -> RM.destShortLabel p.dest) here)
      , layout: Layout.assignmentAt lay o
      , traffic:
          if null wires then Nothing
          else Just
            { hits: sum (map _.hits seen)
            , offs: sum (map _.offs seen)
            , recent: any (\r -> r.agoMs < 2000.0) seen
            }
      }

-- | Rows wanted by more than one source. Worth showing, not blocking: two
-- | sources on one jack is usually a mistake and occasionally a deliberate OR.
conflicted :: Array Row -> Array Row
conflicted = filter (\r -> length r.claimedBy > 1)

-- ---------------------------------------------------------------------------
-- The view
-- ---------------------------------------------------------------------------

-- | Read-only. Editing at the jack — assigning and unassigning a source at the
-- | point of contention — is the gesture that motivated the whole view, but it
-- | wants the rows to have settled first.
panel :: forall w i. Array Row -> HH.HTML w i
panel rs =
  HH.div [ sty "display:flex;flex-wrap:wrap;gap:14px 26px;align-items:flex-start" ]
    (map block (banksOf rs))
  where
  banksOf xs = nub (map (\r -> r.output.device <> "/" <> r.output.bank) xs)

  block b =
    HH.div [ sty "flex:1 1 300px;min-width:280px;max-width:420px" ]
      [ HH.div
          [ sty $ engrave <> ";font-size:10px;padding-bottom:3px;margin-bottom:5px;"
              <> "border-bottom:1px solid #00000018" ]
          [ HH.text b ]
      , HH.div [ sty "display:flex;flex-direction:column" ]
          (map row (sortWith (\r -> r.output.slot)
                      (filter (\r -> r.output.device <> "/" <> r.output.bank == b) rs)))
      ]

  row r =
    HH.div
      [ HP.title (outputKey r.output)
      , sty $ "display:grid;grid-template-columns:34px 1fr 96px 62px;gap:8px;"
          <> "align-items:baseline;padding:2px 0;font-size:11px;"
          <> (if free r then "opacity:0.45" else "") ]
      [ HH.span [ sty mono ] [ HH.text (show (r.output.slot + 1)) ]
      , claim r
      , HH.span [ sty $ mono <> ";font-size:9px;color:#6a6558" ]
          [ HH.text (maybe "" (\a -> a.group <> " v" <> show a.voice <> " · " <> a.role) r.layout) ]
      , traffic r
      ]

  -- A conflict is the one thing here that must not be quiet.
  claim r
    | null r.claimedBy =
        HH.span [ sty "color:#a79f86;font-size:10px" ] [ HH.text "—" ]
    | length r.claimedBy > 1 =
        HH.span [ sty "color:#b0492f;font-weight:600" ]
          [ HH.text (joinWith "  ·  " r.claimedBy <> "  ⚠") ]
    | otherwise =
        HH.span [ sty "color:#3f3c33" ]
          [ HH.text (joinWith "  ·  " r.claimedBy)
          , HH.span [ sty "color:#8a8474;font-size:9px" ]
              [ HH.text ("  " <> joinWith " " r.kinds) ]
          ]

  -- Declared and observed fail independently, so a claimed jack with no traffic
  -- is said out loud rather than left blank — that is a real and common fault
  -- (the machine never armed), and it is invisible from the rack.
  traffic r = case r.traffic of
    Nothing -> HH.span_ []
    Just t
      | t.hits == 0 ->
          HH.span [ sty $ mono <> ";font-size:9px;color:#b0a690"
                  , HP.title "claimed, but no notes have gone there" ]
            [ HH.text "silent" ]
      | otherwise ->
          HH.span
            [ sty $ mono <> ";font-size:9px;color:"
                <> (if t.hits - t.offs > 2 then "#b0492f"
                    else if t.recent then "#2f8a5c" else "#9a9284")
            , HP.title (show t.hits <> " on / " <> show t.offs <> " off") ]
            [ HH.text ((if t.recent then "● " else "") <> show t.hits) ]

  free r = null r.claimedBy && isNothing r.layout
  mono = "font-family:'SF Mono',Menlo,monospace"
  engrave = "font-family:Georgia,serif;letter-spacing:0.12em;text-transform:uppercase;color:#5a564b"

-- | Local rather than imported from `Odonus.Grid.Widgets`, which is where the
-- | app's `style` currently lives: Selene must not depend on Odonus, and this is
-- | precisely the primitive the `Triggerfish.Ui.Style` extraction is meant to
-- | give a neutral home. Needs its own signature — inside a `where` it is
-- | monomorphised to one property row and every other use fails to unify.
sty :: forall r i. String -> HH.IProp r i
sty = HP.attr (HH.AttrName "style")
