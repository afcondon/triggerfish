-- | The READ-ONLY harmonic-context display: what Odonus's grid quantises to and
-- | what its output snaps to. Since 2026-10-02 the harmony routes decide where
-- | each comes from (`routing/harmony`, the dashboard's Harmony matrix: a scale,
-- | Vetula's key, a Vetula card's chords, a harmony pattern); Vetula is one
-- | source among them, no longer the authority. This shows the result.
-- |
-- | Since 2026-10-01 the chords arrive as a Tidal pattern, the harmony
-- | (`odonus $ harmony "..."`, set by Vetula or typed in Limulus): the strip
-- | shows the pattern beside the scale, and rings the chord it gives this step.
-- |
-- | Was the KEY *panel* — a whole right-hand column carrying this plus SCENES plus
-- | LOGBOOK. Retired 2026-08-06 (AC): the column cost a fifth of the width while
-- | PARAMETERS next to it needed a scrollbar. This is now a compact strip in
-- | Odonus's secondary nav (`Odonus.View.Nav`), which is where a live player wants
-- | it anyway — glanceable, not a panel to read.
module Triggerfish.Odonus.View.Key (outputPiece, gridPiece) where

import Reef.Vetula.VoiceName (voiceLetter)
import Prelude

import Data.Array (mapWithIndex, range, take)
import Data.Either (hush)
import Data.Int as Int
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Scale (scaleTypes)
import Triggerfish.Odonus.View.Progression (gridChords, outChords)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (toUpper)
import Reef.Route as Route
import Halogen as H
import Halogen.HTML as HH
import Reef.PitchSet (PitchSet(..))
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, style)

-- | Odonus's two quantisations, each opening the panel it acts on, read right
-- | to left as the notes travel (AC, 2026-10-05): the grid's cells are mapped onto
-- | the GRID set, the playheads shift them, the OUTPUT set snaps them, and they
-- | leave through the river. Each piece: its label, the set, and where the set
-- | comes from. A readout, not a control: routes are patched on the dashboard's
-- | chart, or set by a line in Limulus.
-- |
-- |   GRID    the scale a cell's value is mapped onto (q1, `odonus.grid`)
-- |   OUTPUT  the set each note snaps to past its head's offset (q2,
-- |           `odonus.out`): a chord, an output scale, or none
-- | OUTPUT QUANTISATION, opening PLAYHEADS.
outputPiece :: forall m. State -> H.ComponentHTML Action Slots m
outputPiece s = (quantPieces s Route.OdonusOut)

-- | GRID QUANTISATION, opening NOTES.
gridPiece :: forall m. State -> H.ComponentHTML Action Slots m
gridPiece s = (quantPieces s Route.OdonusGrid)

-- | One piece. Each is built only for its own input: PureScript is strict, so
-- | a record of both, built twice, cost each render four of these.
quantPieces :: forall m. State -> Route.Input -> H.ComponentHTML Action Slots m
quantPieces s = case _ of
  Route.OdonusOut -> piece "Output quantisation" (outBody unit <> strip (if isJust s.odo.harmony then outChords s else []))
      "What each note snaps to at the end, past its head's offset: a chord colours the melody without changing its shape; chromatic leaves each note as it is. Odonus's own choice, unless a route feeds odonus.out"
  Route.OdonusGrid -> piece "Grid quantisation" (gridBody unit <> strip (if isJust s.odo.gridHarmony then gridChords s else []))
      "What a cell's value is mapped onto: the melody's shape. Odonus's own scale, unless a route feeds odonus.grid (a chord's arpeggios, Vetula's key, a scale)"
  where
  ctx = contextInfo s.odo
  routes = fromMaybe [] (s.routesText >>= hush <<< Route.parse)
  routed i = isJust (Route.sourceOf i routes)
  fromVetula i = case Route.sourceOf i routes of
    Just Route.VetulaKey -> true
    Just (Route.VetulaVoice _) -> true
    _ -> false
  outChromatic = map _.pattern s.odo.outScale == Just "chromatic"
  -- Grid: a route says where it comes from; a line names what it set; else
  -- Odonus's own scale, chosen here (the taster's only harmony, in Solo)
  gridBody _
    -- fed by Vetula: where it comes from is the name, and the chords below
    -- say what it holds (the pattern itself is the rig's business)
    | fromVetula Route.OdonusGrid = [ named (provenance Route.OdonusGrid s) ]
    | routed Route.OdonusGrid = [ named gridName, from (provenance Route.OdonusGrid s) ]
    | isJust s.odo.gridHarmony || isJust s.odo.scalePattern = [ named gridName, from "a line in Limulus" ]
    | otherwise = [ rootSelect, scaleSelect ]
  gridName = case s.odo.gridHarmony, s.odo.scalePattern of
    Just h, _ -> "arpeggios of harmony \"" <> h <> "\""
    _, Just sp -> "scale \"" <> sp <> "\""
    _, _ -> Scale.rootName ctx.rootPc <> " " <> ctx.name
  -- Output: a route, a line, or Odonus's own choice: the grid's set, or
  -- chromatic (AC: null quantisation at both)
  outBody _
    | fromVetula Route.OdonusOut = [ named (provenance Route.OdonusOut s) ]
    | routed Route.OdonusOut = [ named outName, from (provenance Route.OdonusOut s) ]
    | isJust s.odo.harmony || (isJust s.odo.outScale && not outChromatic) = [ named outName, from "a line in Limulus" ]
    | otherwise = [ outToggle ]
  outName = case s.odo.outScale, s.odo.harmony of
    Just o, _ | o.pattern == "chromatic" -> "chromatic"
    Just o, _ -> "scale \"" <> o.pattern <> "\" on " <> Scale.rootName o.root
    _, Just h -> "harmony \"" <> h <> "\""
    _, _ -> "the grid\x2019s set"
  -- the progression ahead as names, the chord in force now lit
  strip = case _ of
    [] -> []
    cs ->
      [ HH.div [ style "flex-basis:100%;display:flex;flex-wrap:wrap;gap:3px;margin-top:2px" ]
          (mapWithIndex (\i c -> HH.span
              [ style $ "font-family:Georgia,serif;font-size:12px;padding:0 6px;border-radius:4px;white-space:nowrap;"
                  <> (if i == 0 then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12" else "background:#00000010;color:#5a564b")
              , HH.attr (HH.AttrName "title") (if i == 0 then "the chord in force now" else "coming") ]
              [ HH.text c.name ]) (take 8 cs))
      ]
  -- one line at most, cut with … (a long pattern widened the whole panel); the
  -- whole of it on hover
  named t = HH.span [ style "font-family:Georgia,serif;font-size:13px;color:#2a271e;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;min-width:0;max-width:20em", HH.attr (HH.AttrName "title") t ] [ HH.text t ]
  from t = HH.span [ style "font-family:Georgia,serif;font-style:italic;font-size:12px;color:#6a5820;white-space:nowrap" ] [ HH.text ("\x00b7 " <> t) ]
  rootSelect =
    HH.select
      [ HE.onValueChange \v -> SetRoot (fromMaybe ctx.rootPc (Int.fromString v)), selectStyle "width:52px" ]
      (map (\pc -> HH.option [ HP.value (show pc), HP.selected (pc == ctx.rootPc) ] [ HH.text (Scale.rootName pc) ]) (range 0 11))
  scaleSelect =
    HH.select
      [ HE.onValueChange PickScale, selectStyle "max-width:170px" ]
      (map (\t -> HH.option [ HP.value t.name, HP.selected (t.name == ctx.name) ] [ HH.text t.name ]) scaleTypes)
  selectStyle w = style $ "font-family:Georgia,serif;font-size:13px;color:#2a271e;background:#efece1;border:1px solid #00000026;border-radius:4px;padding:1px 3px;" <> w
  outToggle =
    HH.div [ style "display:flex;border:1px solid #00000026;border-radius:5px;overflow:hidden" ]
      [ seg (not outChromatic) "the grid\x2019s set" (SetOutChromatic false)
      , seg outChromatic "chromatic" (SetOutChromatic true)
      ]
  seg on label act =
    HH.button
      [ HE.onClick \_ -> act
      , style $ "border:none;padding:2px 9px;cursor:pointer;font-family:Georgia,serif;font-size:12px;"
          <> (if on then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12" else "background:#efece1;color:#5a564b") ]
      [ HH.text label ]
  piece label body tip =
    HH.div [ style "display:flex;align-items:center;gap:8px;min-width:0;margin:-6px 0 14px;padding-bottom:10px;border-bottom:1px solid #00000018", HH.attr (HH.AttrName "title") tip ]
      [ HH.span [ style "font-family:Georgia,serif;font-size:18px;color:#7a6a3a;line-height:1" ] [ HH.text "\x2190" ]
      , HH.div [ style "display:flex;flex-direction:column;gap:3px;min-width:0" ]
          [ HH.span [ style $ engrave <> ";font-size:8px;letter-spacing:0.16em;color:#5a4a1f;white-space:nowrap" ]
              [ HH.text (toUpper label) ]
          , HH.div [ style "display:flex;align-items:center;gap:6px;min-width:0;flex-wrap:wrap" ] body
          ]
      ]

-- | Where an input's set comes from: its harmony route if it has one (saying
-- | so when a Vetula source has nothing behind it, so the input falls back to
-- | Odonus's own scale), else a line that set it, else Odonus's own.
provenance :: Route.Input -> State -> String
provenance input s = case Route.sourceOf input routes of
  Just Route.VetulaKey -> vetula "Vetula\x2019s key"
  Just (Route.VetulaVoice n) -> vetula ("Vetula voice " <> voiceLetter n)
  Just (Route.Scale _) -> "a scale route"
  Just (Route.Harmony _) -> "a harmony route"
  Nothing -> case input of
    Route.OdonusGrid
      | isJust s.odo.scalePattern || isJust s.odo.gridHarmony -> "a line in Limulus"
      | otherwise -> "Odonus\x2019s own scale"
    Route.OdonusOut
      | isJust s.odo.harmony || isJust s.odo.outScale -> "a line in Limulus"
      | otherwise -> "no route"
  where
  routes = fromMaybe [] (s.routesText >>= hush <<< Route.parse)
  fed = case input of
    Route.OdonusGrid -> s.feedsSeen.grid
    Route.OdonusOut -> s.feedsSeen.out
  vetula name = if fed == Route.Unfed then name <> ", silent: Odonus\x2019s own" else name

-- | Extract the display facts from the effective pitch set. `root` is a MIDI note;
-- | its pitch class is the scale root, and each interval mapped over it gives the
-- | lit pitch classes. `recogniseScale` names the interval shape.
contextInfo :: M.Odonus -> { rootPc :: Int, pcs :: Array Int, name :: String }
contextInfo odo = case M.effectivePitchSet odo of
  PitchSet ps ->
    { rootPc: mod ps.root 12
    , pcs: map (\iv -> mod (ps.root + iv) 12) ps.offsets
    , name: Scale.recogniseScale ps.offsets
    }
