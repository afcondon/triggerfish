-- | `Vetula.Vary` — **the Pads lens pointed at one chord instead of at a key.**
-- |
-- | Same widget, different generator, which is the second time that move has
-- | paid: the Banks grid asks *which chord comes next*, this one holds a chord
-- | still and asks *what else is this chord*. Rows are `Drift` — how far the
-- | pitch-class content may travel, from pure revoicing out to substitution —
-- | and columns are `Density`, how widely it may be spread and doubled. The two
-- | axes are Progressions' freedom and complexity one level down, which is where
-- | they came from and why the layout can be identical.
-- |
-- | Everything musical lives in `Harmonia.Vary`. This module is the same thin
-- | plumbing `Vetula.Spread` is: a register and a chord out of a `ChordNode`, the
-- | seeded cells, and voicings back into nodes with a name the recogniser gives
-- | them — because a `Swapped` candidate is genuinely no longer the chord you
-- | started from, and calling it by the old name would be a lie the ear would
-- | catch before the eye did.
module Vetula.Vary
  ( Cell
  , grid
  , gridIn
  , varyRows
  , varyCols
  ) where

import Prelude

import Data.Array (drop, elemIndex, filter, head, length, mapWithIndex, nub, sort)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe)

import Harmonia.Chord (Chord(..), Key)
import Harmonia.Recognise (best, candidateName, observeWithBass)
import Harmonia.Vary (Density, Drift, Variation, densities, drifts, variations)
import Harmonia.Voicing (voicingMidi)
import Harmonia.Walk (seed)
import Vetula.Harmony (ChordNode, bassMidi, scaleSet)
import Vetula.Spread (openFor)

-- | Sixteen candidates, four across — the Midifighter tile the Banks grid uses,
-- | kept here so the two lenses are the same gesture.
varyRows :: Int
varyRows = 4

varyCols :: Int
varyCols = 4

type Cell =
  { drift :: Drift
  , density :: Density
  , chords :: Array ChordNode
  }

-- | Nine cells in reading order: rows are drift, columns are density.
-- |
-- | A cell may come back with fewer than sixteen, and that is the design rather
-- | than a shortfall — `Harmonia.Vary` keeps only distinct voicings, so the close
-- | end of the held row exhausts its neighbourhood and says so by stopping.
grid :: Key -> ChordNode -> Int -> Array Cell
grid = gridIn densities

-- | The cells of some densities only, the same cells `grid` gives (each is
-- | seeded by its own place): a panel showing one density need not pay for
-- | three, and each costs about a hundred milliseconds.
gridIn :: Array Density -> Key -> ChordNode -> Int -> Array Cell
gridIn dns key src roll = do
  d <- drifts
  dn <- dns
  let
    vs = variations (openFor src) (rootedOf src) d dn (seed (cellSeed roll d dn)) 16
    base = 30000 + cellIx d dn * 100
    scl = scaleSet key
  pure
    { drift: d
    , density: dn
    , chords: mapWithIndex (\i v -> nodeOf scl src (base + i) v) vs
    }

-- | The chord to vary, as Harmonia wants it. The ROOT here, not the bass:
-- | `applySpread` builds its stack upward from the root and the inversions come
-- | from rotating that stack afterwards, so handing it the current bass would
-- | quietly re-root every candidate on whatever inversion happened to be showing.
rootedOf :: ChordNode -> { root :: Int, chord :: Chord }
rootedOf c = { root: mod c.root 12, chord: Chord (sort (nub (map (\p -> mod p 12) c.pcs))) }

-- | Seeds are addresses, not events: roll 7 always gives roll 7, in every cell.
cellSeed :: Int -> Drift -> Density -> Int
cellSeed roll d dn = roll * 7919 + cellIx d dn * 37 + 1

cellIx :: Drift -> Density -> Int
cellIx d dn = fromMaybe 0 (elemIndex d drifts) * 3 + fromMaybe 0 (elemIndex dn densities)

-- | A candidate as a node. Built by updating the source, so everything the grid
-- | does not decide — kind, anchor, the chord's role — is carried rather than
-- | invented; `parentId` records where it came from, which is what the pool
-- | needs if you catch one.
nodeOf :: Array Int -> ChordNode -> Int -> Variation -> ChordNode
nodeOf scl src nid v =
  let
    midi = sort (voicingMidi v.voicing)
    b = fromMaybe (bassMidi src) (head midi)
    Chord cpcs = v.chord
    ps = sort (nub (map (\p -> mod p 12) cpcs))
  in
    src
      { id = nid
      , parentId = Just src.id
      , bassPc = mod b 12
      , bassOct = b / 12
      , pcs = ps
      , voicing = drop 1 midi
      , label = nameFor (mod b 12) ps src.label
      , pinned = false
      , outside = length (filter (\p -> not (elem p scl)) ps)
      , isCentre = false
      }

-- | What to call it. The recogniser reads the sounding set with its bass known,
-- | so a rotation is named as a slash chord and a swapped tone gets the name it
-- | has actually become. Falling back to the source's name keeps a pad labelled
-- | rather than blank when nothing in the vocabulary fits.
nameFor :: Int -> Array Int -> String -> String
nameFor b ps fallback = fromMaybe fallback (map candidateName (best (observeWithBass b ps)))
