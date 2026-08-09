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
  , Claimant
  , Traffic
  , rows
  , conflicted
  , panel
  ) where

import Prelude

import Data.Array (any, filter, intersperse, length, null, nub, sortWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String.Common (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Triggerfish.Rig (RigConfig, rigOutputs)
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Monitor as Mon
import Triggerfish.Selene.Manifest as Man
import Triggerfish.Selene.Layout (Assignment, Layout, Output, outputKey)
import Triggerfish.Selene.Layout as Layout

type Traffic = { hits :: Int, offs :: Int, recent :: Boolean }

-- | One claimant of an output. `on` is the routing table's mute flag, and
-- | carrying muted claimants here rather than dropping them is deliberate.
-- |
-- | **Muting is how a clash gets resolved, so a muted claim has to stay
-- | visible.** Delete the losing leg and you lose the record of which machine
-- | wanted the jack, leaving you to hunt for it the next time you wonder why
-- | that voice is silent. Absence is unreadable; a ghost is readable — it says
-- | "this was decided" rather than "this never existed".
type Claimant =
  { label :: String
  , kind :: String
  , on :: Boolean
  -- | Whether anything still exists to honour this claim. A muted claimant and a
  -- | claimant whose source is gone both look like "not driving this jack", and
  -- | they are entirely different facts: one is a decision, the other is a
  -- | leftover. See `Selene.Manifest`.
  , standing :: Man.Standing
  }

type Row =
  { output :: Output
  , claimedBy :: Array Claimant      -- ^ more than one LIVE claimant is a conflict
  , layout :: Maybe Assignment       -- ^ what the rig is configured to receive
  -- | The VCO this jack reaches, by its Amphora `vco-calibrations` label. Only
  -- | meaningful on a `pitch` output, and its absence there is a real finding: an
  -- | uncorrected analogue VCO does not track 1 V/oct (the Tona measured 1.007
  -- | rising to 1.030 across its range), so an uncalibrated pitch jack plays
  -- | progressively out of tune with everything else rather than obviously wrong.
  , vco :: Maybe String
  , traffic :: Maybe Traffic
  }

-- | One row per physical output the rig has — claimed or not.
rows :: RigConfig -> Layout -> Array Man.Manifest -> Number -> RM.Table -> Array Mon.Row -> Array Row
rows cfg lay mans now tbl obs = map build (rigOutputs cfg)
  where
  -- Every live leg, paired with the jack it lands on. Legs that are not a jack
  -- at all (MIDI, continuo) simply never match a row, which is right: a channel
  -- is not scarce the way a jack is and does not belong in this table.
  -- Muted legs are INCLUDED — see `Claimant`. They are excluded from the
  -- conflict count and from traffic (a muted leg emits nothing), but they still
  -- render, because they are the memory of a decision.
  placed = do
    route <- tbl
    leg <- route.legs
    case RM.outputOf leg.dest of
      Nothing -> []
      Just o -> [ { output: o, source: route.source, dest: leg.dest, on: leg.on } ]

  build o =
    let here = filter (\p -> p.output == o) placed
        wires = do
          p <- filter _.on here
          case RM.wireOf p.dest of
            Nothing -> []
            Just w -> [ w ]
        seen = filter (\r -> any (\w -> Mon.matches w r) wires) obs
    in
      { output: o
      , claimedBy: nub (map (\p ->
          { label: RM.sourceLabel p.source
          , kind: RM.destShortLabel p.dest
          , on: p.on
          , standing: Man.standingOf now mans (RM.sourceKey p.source)
          }) here)
      , layout: Layout.assignmentAt lay o
      , vco: do
          a <- Layout.assignmentAt lay o
          g <- Layout.groupNamed lay a.group
          if a.role == "pitch" then g.vco else Nothing
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
-- | Only LIVE claimants contend. A muted one has already lost the argument and
-- | is being kept as a note-to-self, not as a competitor.
conflicted :: Array Row -> Array Row
conflicted = filter (\r -> length (filter _.on r.claimedBy) > 1)

-- ---------------------------------------------------------------------------
-- The view
-- ---------------------------------------------------------------------------

-- | Read-only. Editing at the jack — assigning and unassigning a source at the
-- | point of contention — is the gesture that motivated the whole view, but it
-- | wants the rows to have settled first.
panel :: forall w i. Array Layout.Problem -> Array Row -> HH.HTML w i
panel probs rs =
  HH.div_
    [ resources
    , HH.div [ sty "display:flex;flex-wrap:wrap;gap:14px 26px;align-items:flex-start" ]
        (map block (banksOf rs))
    ]
  where
  banksOf xs = nub (map (\r -> r.output.device <> "/" <> r.output.bank) xs)

  -- Contention on things that are NOT jacks — MCVs, above all. The jack table
  -- below is structurally blind to these: two MCVs can collide while their
  -- outputs sit on different hardware, which is precisely the FH-2 case that
  -- had to be found by reading allocation code. So it goes at the top, where
  -- the table cannot quietly imply everything is fine.
  resources =
    if null probs then HH.div_ []
    else
      HH.div
        [ sty $ "margin-bottom:18px;padding:9px 12px;border-left:3px solid #b0492f;"
            <> "background:#b0492f11;display:flex;flex-direction:column;gap:3px" ]
        ( [ HH.div [ sty $ engrave <> ";font-size:9px;color:#b0492f;margin-bottom:2px" ]
              [ HH.text "contended resources — not visible in the table below" ] ]
            <> map (\p -> HH.div [ sty "font-size:10px;color:#7a3a28;line-height:1.45" ]
                            [ HH.text (Layout.problemNote p) ]) probs )

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
      -- Both numbers, because they disagree by one and each is right in its own
      -- frame: the row is the JACK, which the FH-2 manual numbers from 1, while
      -- the address carries the SLOT, which `apply-drumkit` numbers from 0.
      -- Showing only the address next to a 1-based row number reads as a
      -- contradiction — and this is the exact off-by-one that already shipped
      -- once today.
      [ HP.title ("jack " <> show (r.output.slot + 1) <> "  ·  " <> outputKey r.output)
      , sty $ "display:grid;grid-template-columns:34px 1fr 96px 62px;gap:8px;"
          <> "align-items:baseline;padding:2px 0;font-size:11px;"
          <> (if free r then "opacity:0.45" else "") ]
      [ HH.span [ sty mono ] [ HH.text (show (r.output.slot + 1)) ]
      , claim r
      , HH.span [ sty $ mono <> ";font-size:9px;color:#6a6558" ]
          ( [ HH.text (maybe "" (\a -> a.group <> " v" <> show a.voice <> " · " <> a.role) r.layout) ]
              <> calib r )
      , traffic r
      ]

  -- A conflict is the one thing here that must not be quiet. A muted claimant is
  -- struck through and dimmed rather than hidden: it is what remains of a
  -- decision, and it is the answer to "which machine used to have this?"
  claim r =
    let live = filter _.on r.claimedBy
    in if null r.claimedBy
      then HH.span [ sty "color:#a79f86;font-size:10px" ] [ HH.text "—" ]
      else HH.span
             [ sty (if length live > 1 then "color:#b0492f;font-weight:600" else "color:#3f3c33") ]
             ( intersperse (HH.span [ sty "color:#a79f86" ] [ HH.text "  ·  " ])
                 (map one r.claimedBy)
                 <> (if length live > 1 then [ HH.text "  ⚠" ] else []) )

  one c =
    HH.span
      [ sty (if c.on then "" else "opacity:0.4;text-decoration:line-through")
      , HP.title ((if c.on then c.kind else c.kind <> " — muted, so it is not driving this jack")
            <> "  ·  " <> Man.standingNote c.standing) ]
      ( [ HH.text c.label
        , HH.span [ sty "color:#8a8474;font-size:9px;text-decoration:none" ]
            [ HH.text ("  " <> c.kind) ]
        ] <> orphan c )

  -- A claim nobody stands behind. Marked, not hidden: it is holding a jack, and
  -- the reason you cannot find the machine is that there is no longer one.
  orphan c = case c.standing of
    Man.Unknown ->
      [ HH.span [ sty "color:#b0492f;font-size:9px;text-decoration:none"
                , HP.title "no app declares this source — a leftover row holding a jack" ]
          [ HH.text "  ✗ orphaned" ] ]
    Man.Stale app _ ->
      [ HH.span [ sty "color:#a8762f;font-size:9px;text-decoration:none"
                , HP.title (Man.standingNote c.standing) ]
          [ HH.text ("  ◌ " <> app <> " not responding") ] ]
    Man.Live _ -> []

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

  -- A pitch jack says which VCO it is corrected for; one with no table says so,
  -- because uncalibrated reads as drifting-out-of-tune rather than as broken.
  calib r = case r.layout of
    Just a | a.role == "pitch" -> case r.vco of
      Just v -> [ HH.span [ sty "color:#2f8a5c" ] [ HH.text ("  ✓ " <> v) ] ]
      Nothing -> [ HH.span [ sty "color:#b0492f"
                           , HP.title "no calibration table — this VCO will drift sharp or flat across its range" ]
                     [ HH.text "  uncalibrated" ] ]
    _ -> []

  free r = null (filter _.on r.claimedBy) && isNothing r.layout
  mono = "font-family:'SF Mono',Menlo,monospace"
  engrave = "font-family:Georgia,serif;letter-spacing:0.12em;text-transform:uppercase;color:#5a564b"

-- | Local rather than imported from `Odonus.Grid.Widgets`, which is where the
-- | app's `style` currently lives: Selene must not depend on Odonus, and this is
-- | precisely the primitive the `Triggerfish.Ui.Style` extraction is meant to
-- | give a neutral home. Needs its own signature — inside a `where` it is
-- | monomorphised to one property row and every other use fails to unify.
sty :: forall r i. String -> HH.IProp r i
sty = HP.attr (HH.AttrName "style")
