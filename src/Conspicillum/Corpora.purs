-- | **The material: Quadrat's sets, as the page reads them.**
-- |
-- | `conspicillum-corpora.json` is written by triggerfish's
-- | `audio/build-corpora.py` from `~/.itajara/quadrat`. Each sample carries the
-- | facts the engine grains by (a reef `Grainable`) and, for the page alone,
-- | its peak envelope. The file is read twice with two record types, so the
-- | corpus the rig is sent is exactly reef's shape and the envelopes never
-- | travel.
module Conspicillum.Corpora
  ( Tape
  , SampleSet
  , Corpora
  , load
  , setNamed
  , corpusFor
  , materialFor
  , envelopeOf
  , withMaterial
  , sampleChoice
  ) where

import Prelude

import Data.Array (any, filter, find, head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (error)
import Effect (Effect)
import Reef.Conspicillum.Corpus (Corpus, Grainable)
import Reef.Conspicillum.Display (Material, materialOf)
import Reef.Conspicillum.Notation (Line)
import Simple.JSON (E, readJSON)

-- | What a tape knows about itself: how many bars, at what tempo.
type Tape = { bars :: Int, bpm :: Number }

type SampleSet =
  { corpus :: Corpus
  , tape :: Maybe Tape
  -- | Peak |x| per bin, 0..255 of full scale, by sample index.
  , envelopes :: Array { index :: Int, envelope :: Array Int }
  }

type Corpora = Array SampleSet

type Facts = { sets :: Array { name :: String, samples :: Array Grainable, tape :: Maybe Tape } }
type Envelopes = { sets :: Array { name :: String, samples :: Array { index :: Int, envelope :: Array Int } } }

foreign import fetchText :: String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit

load :: String -> Aff (Either String Corpora)
load url = do
  text <- makeAff \done -> do
    fetchText url (done <<< Right) (done <<< Left <<< error)
    pure nonCanceler
  pure (decode text)

decode :: String -> Either String Corpora
decode text = do
  facts <- lmap (\e -> "the corpora do not read as sets: " <> show e) (readJSON text :: E Facts)
  shapes <- lmap (\e -> "the corpora have no envelopes: " <> show e) (readJSON text :: E Envelopes)
  pure $ facts.sets <#> \s ->
    { corpus: { name: s.name, samples: s.samples }
    , tape: s.tape
    , envelopes: maybe [] _.samples (find (\e -> e.name == s.name) shapes.sets)
    }

setNamed :: Corpora -> String -> Maybe SampleSet
setNamed corpora name = find (\s -> s.corpus.name == name) corpora

-- | The corpus a line plays: the whole set, or the one sample it names. What
-- | goes to the rig, and what `cycleOf` is asked about, so they agree.
corpusFor :: SampleSet -> Line -> Corpus
corpusFor set line = case line.n of
  Nothing -> set.corpus
  Just k -> set.corpus { samples = filter (\s -> s.index == k) set.corpus.samples }

materialFor :: SampleSet -> Line -> Material
materialFor set line = materialOf
  { line
  , samples: set.corpus.samples <#> \s -> { index: s.index, seconds: s.secs }
  , tape: set.tape
  }

envelopeOf :: SampleSet -> Int -> Array Int
envelopeOf set index = maybe [] _.envelope (find (\e -> e.index == index) set.envelopes)

-- | A line played on other material. A tape says how many bars it is, so a
-- | line moved onto one reads it whole and takes its bars; the rest of the
-- | scene is untouched.
withMaterial :: SampleSet -> Maybe Int -> Line -> Line
withMaterial set n line = case set.tape of
  Just t -> line { set = set.corpus.name, n = Nothing, spec = line.spec { tape = line.spec.tape { bars = t.bars } } }
  Nothing -> line { set = set.corpus.name, n = n }

-- | Which sample a line should play on moving to this set: the same one if it
-- | has it, else its first, or still the whole set if that is what it played.
sampleChoice :: SampleSet -> Maybe Int -> Maybe Int
sampleChoice set = case _ of
  Nothing -> Nothing
  Just k
    | any (\s -> s.index == k) set.corpus.samples -> Just k
    | otherwise -> map _.index (head set.corpus.samples)
