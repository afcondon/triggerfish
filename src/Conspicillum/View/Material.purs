-- | **The material chooser**: which set a scene plays, and which of its
-- | samples, or all of them. A preset names its material, but most of what a
-- | preset does is a treatment that would work on anything, so the material
-- | can be held while presets change under it.
module Conspicillum.View.Material (material) where

import Prelude

import Conspicillum.Corpora (Corpora, SampleSet, envelopeOf)
import Conspicillum.View.Svg (attribute, classes, element)
import Conspicillum.Waveform (loudest, outline)
import Data.Array (length, nub, null)
import Data.Int (round, toNumber)
import Data.Number (sqrt)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Reef.Conspicillum.Decimal (fixed)

material
  :: forall w i
   . { corpora :: Corpora
     , set :: SampleSet
     , sample :: Maybe Int
     , held :: Boolean
     , chooseSet :: String -> i
     , chooseSample :: Maybe Int -> i
     , hold :: Boolean -> i
     }
  -> HH.HTML w i
material r =
  HH.div [ cls "material" ]
    [ HH.div [ cls "materialrow" ]
        [ HH.span [ cls "materiallab" ] [ HH.text "material" ]
        , HH.select [ HE.onValueChange r.chooseSet ]
            (r.corpora <#> \s ->
              HH.option [ HP.value s.corpus.name, HP.selected (s.corpus.name == r.set.corpus.name) ]
                [ HH.text (s.corpus.name <> " — " <> describeSet s) ])
        , HH.button
            [ cls (if r.held then "hold on" else "hold")
            , HP.title "hold this material while presets change"
            , HE.onClick \_ -> r.hold (not r.held)
            ]
            [ HH.text (if r.held then "held" else "hold") ]
        ]
    , if r.set.tape /= Nothing then HH.text ""
      else
        HH.div [ cls "tiles" ]
          ( [ HH.button [ cls (if r.sample == Nothing then "tile all on" else "tile all"), HE.onClick \_ -> r.chooseSample Nothing ]
                [ HH.span [ cls "tn" ] [ HH.text "all" ] ]
            ]
              <> (r.set.corpus.samples <#> tile)
          )
    ]
  where
  cls :: forall rr ii. String -> HP.IProp (class :: String | rr) ii
  cls = HP.class_ <<< HH.ClassName

  -- On a square-root scale: a tile is too small for a long tail to show
  -- otherwise, and what a tile is for is telling samples apart.
  shaped index = envelopeOf r.set index <#> \v -> round (sqrt (toNumber v / 255.0) * 255.0)
  peak = loudest (r.set.corpus.samples <#> \s -> shaped s.index)

  tile s =
    HH.button
      [ cls (if r.sample == Just s.index then "tile on" else "tile")
      , HP.title (fixed 2 s.secs <> " s" <> (if null s.notes then "" else " · " <> chord s.notes))
      , HE.onClick \_ -> r.chooseSample (Just s.index)
      ]
      [ element "svg" [ attribute "viewBox" "0 0 40 16", attribute "preserveAspectRatio" "none" ]
          [ element "path" [ classes "wave", attribute "d" (outline { x0: 0.0, x1: 40.0, centre: 8.0, half: 7.5, peak } (shaped s.index)) ] [] ]
      , HH.span [ cls "tn" ] [ HH.text (show s.index) ]
      , HH.span [ cls "tc" ] [ HH.text (if null s.notes then "" else chord s.notes) ]
      ]

describeSet :: SampleSet -> String
describeSet s = case s.tape of
  Just t -> show t.bars <> "-bar tape"
  Nothing -> case length s.corpus.samples of
    1 -> "1 sample"
    n -> show n <> " samples"

chord :: Array Int -> String
chord notes = intercalate " " (nub (notes <#> \m -> pitchName (((m `mod` 12) + 12) `mod` 12)))

pitchName :: Int -> String
pitchName = case _ of
  0 -> "C"
  1 -> "C♯"
  2 -> "D"
  3 -> "E♭"
  4 -> "E"
  5 -> "F"
  6 -> "F♯"
  7 -> "G"
  8 -> "A♭"
  9 -> "A"
  10 -> "B♭"
  _ -> "B"
