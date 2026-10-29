-- | **The strip: the material.** The sound laid out left to right, in the
-- | colours the ring and the cloud borrow, so colour has one meaning on the
-- | panel: where in the material. Under it, the sound's own envelope, and
-- | under that each grain's read span, laned so none hides another.
-- |
-- | **The loupe** is one segment, the one being read, at full width: its
-- | envelope, each grain's read window over it, and for a read head that
-- | stands still, the band that position and spray let a grain start in. It is
-- | what shows position as a musical fact, a chord on a progression tape, the
-- | attack or the tail inside a hit.
module Conspicillum.View.Strip
  ( strip
  , stripBackdrop
  , stripHeight
  , LoupeInput
  , loupe
  , loupeBackdrop
  , loupeView
  , loupeSource
  ) where

import Prelude

import Data.Array (filter, find, index, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Conspicillum.Corpora (SampleSet)
import Conspicillum.Plan (Grain, Plan, stripWidth)
import Conspicillum.View.Svg (attribute, classes, element, number)
import Conspicillum.Waveform (Source, loudest, outline, sources, stretch)
import Halogen.HTML as HH
import Reef.Conspicillum.Cloud (Spec)
import Reef.Conspicillum.Display (Material, colourAt, colourCss, segments)

left :: Number
left = 8.0

across :: Number -> Number
across x = left + x * stripWidth

-- The strip's rows, top to bottom.
colourTop = 14.0 :: Number
colourHeight = 26.0 :: Number
waveCentre = 53.0 :: Number
waveHalf = 9.0 :: Number
laneTop = 66.0 :: Number
laneHeight = 5.0 :: Number
lanePitch = 7.0 :: Number

stripHeight :: Int -> Number
stripHeight lanes = laneTop + toNumber lanes * lanePitch + 4.0

-- | The grain sounding at `phase`, if one is, and how far through it.
sounding :: Plan -> Number -> Maybe { grain :: Grain, through :: Number }
sounding plan phase =
  find (\g -> phase >= g.from && phase < g.to) plan.grains <#> \g ->
    { grain: g, through: min 1.0 ((phase - g.from) / g.sounding) }

-- | What does not move: the boxes in their colours, their labels, and the
-- | sound under them. Drawn once per scene, not once per frame.
stripBackdrop :: forall w i. Material -> SampleSet -> Array (HH.HTML w i)
stripBackdrop material set =
  [ element "defs" [] (mapWithIndex gradient segs) ]
    <> mapWithIndex box segs
    <> labels
    <> waveform
  where
  segs = segments material
  count = length segs

  gradient k _ =
    element "linearGradient" [ attribute "id" ("sg" <> show k) ]
      ([ 0.0, 0.25, 0.5, 0.75, 1.0 ] <#> \f ->
        element "stop" [ number "offset" f, attribute "stop-color" (colourCss (colourAt material { segment: k, within: f, along: 0.0 })) ] [])

  box k g =
    element "rect"
      [ classes "seg"
      , number "x" (across g.from), number "y" colourTop
      , number "width" (boxWidth g), number "height" colourHeight
      , attribute "fill" ("url(#sg" <> show k <> ")")
      ] []

  boxWidth g = max 1.0 ((g.to - g.from) * stripWidth - (if count > 1 then min 2.0 ((g.to - g.from) * stripWidth * 0.15) else 0.0))

  labels =
    if count > 32 then []
    else segs # filter (\g -> g.label /= "") <#> \g ->
      element "text" [ classes "seglab", number "x" (across ((g.from + g.to) / 2.0)), number "y" 10.0 ] [ HH.text g.label ]

  -- Each box's stretch of sound, drawn against the loudest in view.
  waveform =
    let
      stretches = sources material <#> stretch set
      peak = loudest stretches
    in
      [ element "line" [ classes "wavecentre", number "x1" left, number "y1" waveCentre, number "x2" (left + stripWidth), number "y2" waveCentre ] [] ]
        <> mapWithIndex
          ( \k bins -> case index segs k of
              Just g -> element "path"
                [ classes "wave"
                , attribute "d" (outline { x0: across g.from, x1: across g.to, centre: waveCentre, half: waveHalf, peak } bins)
                ] []
              Nothing -> HH.text ""
          )
          stretches

-- | The strip, with what moves drawn over its backdrop: the box being read,
-- | each grain's read span, and the read head.
strip
  :: forall w i
   . { material :: Material, plan :: Plan, phase :: Number, playing :: Boolean, lanes :: Int, backdrop :: HH.HTML w i }
  -> HH.HTML w i
strip { material, plan, phase, playing, lanes, backdrop } =
  element "svg"
    [ attribute "id" "strip", attribute "viewBox" ("0 0 800 " <> show height), attribute "preserveAspectRatio" "none"
    , attribute "style" ("height:" <> show height <> "px"), classes (if playing then "" else "idle")
    ]
    ( [ backdrop ]
        <> reading
        <> (plan.grains <#> lane)
        <> [ head ]
    )
  where
  height = stripHeight lanes
  segs = segments material
  now = sounding plan phase

  reading = case now >>= \s -> index segs s.grain.reads.from.segment of
    Just g ->
      [ element "rect"
          [ classes "seg on", number "x" (across g.from), number "y" colourTop
          , number "width" (max 1.0 ((g.to - g.from) * stripWidth - (if length segs > 1 then min 2.0 ((g.to - g.from) * stripWidth * 0.15) else 0.0)))
          , number "height" colourHeight, attribute "fill" "none"
          ] []
      ]
    Nothing -> []

  lane g =
    let
      on = phase >= g.from && phase < g.to
    in
      element "rect"
        [ classes (if on then "lane on" else "lane")
        , number "x" (across g.reads.from.along), number "y" (laneTop + toNumber g.lane * lanePitch)
        , number "width" (max 2.0 ((g.reads.to.along - g.reads.from.along) * stripWidth - 1.0)), number "height" laneHeight
        , attribute "fill" g.colour
        ] []

  head =
    let
      x = case now of
        Just s -> across (s.grain.reads.from.along + (s.grain.reads.to.along - s.grain.reads.from.along) * s.through)
        Nothing -> -9.0
    in
      element "line" [ classes "rhead", number "x1" x, number "y1" (colourTop - 3.0), number "x2" x, number "y2" (height - 2.0) ] []

-- ── the loupe ────────────────────────────────────────────────────────────────

loupeCentre = 34.0 :: Number
loupeHalf = 24.0 :: Number

type LoupeInput = { material :: Material, set :: SampleSet, spec :: Spec, segment :: Int }

-- | The segment the loupe is on, as a source, and how long it lasts.
loupeSource :: LoupeInput -> { source :: Source, seconds :: Number }
loupeSource { material, set, segment } =
  let
    source = fromMaybe { sample: 0, from: 0.0, to: 1.0 } (index (sources material) segment)
    whole = maybe 1.0 _.secs (find (\s -> s.index == source.sample) set.corpus.samples)
  in
    { source, seconds: whole * (source.to - source.from) }

-- | Where a grain may start, when the read head stands still, and how long it
-- | then reads, as fractions of the segment: `position` of the way into the
-- | room a grain leaves, give or take half the spray. As reef's `grainAt`.
reach :: LoupeInput -> Maybe { earliest :: Number, at :: Number, latest :: Number }
reach input@{ set, spec }
  | spec.cloud.follow /= 0.0 = Nothing
  | otherwise =
      let
        { source } = loupeSource input
        secs = maybe 1.0 _.secs (find (\s -> s.index == source.sample) set.corpus.samples)
        reads = if secs <= 0.0 then 1.0 else min 1.0 (spec.cloud.sustain / secs)
        room = 1.0 - reads
        at = spec.cloud.position * room
        local x = (x - source.from) / (source.to - source.from)
      in
        Just
          { earliest: local (clamp 0.0 room (at - spec.cloud.spray / 2.0))
          , at: local at
          , latest: local (clamp 0.0 room (at + spec.cloud.spray / 2.0) + reads)
          }

-- | What the loupe magnifies, as a fraction of its segment. A read head that
-- | moves (a tape) is shown its whole bar. One that stands still is shown the
-- | band a grain may start in, twice over and at least a quarter of a second,
-- | so the attack or the tail a grain reads is big enough to see.
loupeView :: LoupeInput -> { from :: Number, to :: Number }
loupeView input = case reach input of
  Nothing -> { from: 0.0, to: 1.0 }
  Just r ->
    let
      { seconds } = loupeSource input
      span = min 1.0 (max (2.0 * (r.latest - r.earliest)) (if seconds <= 0.0 then 1.0 else 0.25 / seconds))
      centre = (r.earliest + r.latest) / 2.0
      from = clamp 0.0 (1.0 - span) (centre - span / 2.0)
    in
      { from, to: from + span }

-- | What does not move in the loupe: the band a grain may start in, and the
-- | sound, cut to the view.
loupeBackdrop :: forall w i. LoupeInput -> Array (HH.HTML w i)
loupeBackdrop input@{ set } =
  band
    <> [ element "line" [ classes "wavecentre", number "x1" left, number "y1" loupeCentre, number "x2" (left + stripWidth), number "y2" loupeCentre ] []
       , element "path" [ classes "wave loupewave", attribute "d" (outline { x0: left, x1: left + stripWidth, centre: loupeCentre, half: loupeHalf, peak: loudest [ whole ] } bins) ] []
       ]
  where
  { source } = loupeSource input
  view = loupeView input
  -- The view as a stretch of the sample, so the envelope is cut to it; drawn
  -- against the whole segment's peak, so zooming in does not make a tail loud.
  onSample x = source.from + x * (source.to - source.from)
  bins = stretch set { sample: source.sample, from: onSample view.from, to: onSample view.to }
  whole = stretch set source
  seen x = across ((x - view.from) / (view.to - view.from))

  band = case reach input of
    Nothing -> []
    Just r ->
      [ element "rect"
          [ classes "reach", number "x" (seen r.earliest), number "y" 2.0
          , number "width" (max 1.0 (seen r.latest - seen r.earliest)), number "height" 64.0
          ] []
      , element "line" [ classes "position", number "x1" (seen r.at), number "y1" 0.0, number "x2" (seen r.at), number "y2" 68.0 ] []
      ]

loupe
  :: forall w i
   . { material :: Material, set :: SampleSet, plan :: Plan, phase :: Number, spec :: Spec, segment :: Int, backdrop :: HH.HTML w i }
  -> HH.HTML w i
loupe { material, set, plan, phase, spec, segment, backdrop } =
  element "svg" [ attribute "id" "loupe", attribute "viewBox" "0 0 800 68", attribute "preserveAspectRatio" "none" ]
    ( [ backdrop ] <> (ours <#> window) <> [ head ] )
  where
  input = { material, set, spec, segment }
  view = loupeView input
  seen x = across ((x - view.from) / (view.to - view.from))
  visible from to = to > view.from && from < view.to
  ours = filter (\g -> g.reads.from.segment == segment && visible g.reads.from.within (endOf g)) plan.grains
  endOf g = if g.reads.to.segment == segment then g.reads.to.within else 1.0
  now = sounding plan phase

  -- A grain's window, from where it starts reading for as long as it reads;
  -- cut at the edges of the view.
  window g =
    let
      x0 = seen (max view.from g.reads.from.within)
      x1 = seen (min view.to (endOf g))
      on = phase >= g.from && phase < g.to
    in
      element "rect"
        [ classes (if on then "window on" else "window")
        , number "x" x0, number "y" (loupeCentre - loupeHalf - 3.0)
        , number "width" (max 2.0 (x1 - x0)), number "height" (2.0 * loupeHalf + 6.0)
        , attribute "fill" g.colour
        ] []

  head = case now of
    Just s | s.grain.reads.from.segment == segment ->
      let
        r = s.grain.reads
        at = r.from.within + (endOf s.grain - r.from.within) * s.through
        x = seen at
      in
        if at < view.from || at > view.to then HH.text ""
        else element "line" [ classes "rhead", number "x1" x, number "y1" 4.0, number "x2" x, number "y2" 64.0 ] []
    _ -> HH.text ""
