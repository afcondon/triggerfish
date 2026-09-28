-- | The Quadrat sample sets SuperDirt has loaded, by name and size, for the
-- | routing table's sample destination to choose from. Read from the corpora
-- | file Triggerfish already serves (`build-corpora.py` writes it): a set's
-- | directory is a SuperDirt bank, so its name is the `s` to play.
module Triggerfish.SampleSets (SampleSet, load) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

type SampleSet = { name :: String, samples :: Int }

foreign import loadImpl :: String -> (Array SampleSet -> Effect Unit) -> Effect Unit

-- | Every set, or none if the file cannot be read: the table then offers no
-- | sets, which says what is wrong more plainly than an error would.
load :: Aff (Array SampleSet)
load = makeAff \done -> do
  loadImpl "conspicillum-corpora.json" (done <<< Right)
  pure nonCanceler
