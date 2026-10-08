-- | `Vetula.Pads` — **the chord space as nine banks of sixteen.**
-- |
-- | The banks view, and a deliberately different principle from the lattice,
-- | the fifths and the tonnetz, which all place a chord by its own pitch
-- | content: its root, its notes, how far it stacks. This arranges them by
-- | ABSOLUTE ADDRESS — where a chord sits in the
-- | Progressions-family generator's two axes, measured from home and not from
-- | whatever you last clicked.
-- |
-- | That is why it is additive rather than a rival. It answers a question the
-- | others cannot ("what is out there at this distance, in this vocabulary?")
-- | and it is bad at the one they are good at ("what is near this chord?").
-- |
-- | ## The settings are the layout
-- |
-- | `Harmonia.Freedom` and `Harmonia.Palette` are two independent axes, and the
-- | usual way to expose two axes is two dials. Here they are the two axes of
-- | the SCREEN instead: you do not set freedom to 3 and complexity to Medium
-- | and then look at the result, you reach for the cell. Nine cells of sixteen
-- | pads, 144 chords, out of a total vocabulary of 480 — so this is not a
-- | lottery ticket, it is thirty percent of everything, laid out.
-- |
-- | The grouping is three and three rather than the full six freedoms by five
-- | levels, and that is not only about screen space. The full grid has dead
-- | cells: `Basic` contributes two chord types and the outermost ring is one
-- | wedge, so its cell would hold two chords and a shuffle button over two
-- | chords is a lie. Grouped, the smallest cell draws from twenty-four and the
-- | largest from a hundred and five, so **every cell genuinely shuffles** —
-- | least at top-left and most at bottom-right, which is the right gradient.
-- |
-- | ## Why a cell is a walk and not a sample
-- |
-- | Sixteen chords drawn at random from a cell would be sixteen unrelated
-- | chords. Sixteen chords from `Harmonia.Progression` are consecutive steps of
-- | a seeded walk, so **any row of four you play across is already music**. It
-- | costs nothing — the walk is the generator we have — and it is the whole
-- | difference between a reference chart and something you can mess about with.
-- |
-- | The consequence to be honest about: because both axes are cumulative (a
-- | freedom window contains the smaller ones; a complexity level contains the
-- | simpler ones), the cells NEST rather than partition, and a chord can appear
-- | in more than one cell. What differs between cells is the walk's character —
-- | how far it roams and how richly it colours — which is what you would
-- | actually hear. The alternative reading, where each cell shows only what its
-- | own ring and level CONTRIBUTE, gives 144 distinct chords and a truer map;
-- | `Harmonia.Freedom.ring` and `Harmonia.Palette.atLevel` are exported for
-- | exactly that, and it would be a toggle here rather than a rewrite.
-- |
-- | ## Shuffle
-- |
-- | One seed for the whole grid, fanned out to nine by cell index, so a shuffle
-- | re-rolls every cell at once and the grid as a whole is reproducible from a
-- | single number. Each cell is a `Harmonia.Progression.Spec` and could be saved
-- | as one.
module Vetula.Pads
  ( Reach(..)
  , reaches
  , reachLabel
  , reachFreedom
  , Colour(..)
  , colours
  , colourLabel
  , colourLevel
  , Cell
  , cellSpec
  , cellNodes
  , grid
  , padRows
  , padCols
  ) where

import Prelude

import Data.Array (drop, filter, head, length, mapWithIndex, nub)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Harmonia.Anchor (Anchor(..))
import Harmonia.Chord (Chord(..), Key, Mode(..))
import Harmonia.Freedom (Freedom, freedom)
import Harmonia.OpenVoicing (defaults) as OV
import Harmonia.Palette (Level(..), typeSuffix)
import Harmonia.Progression (Spec, Voiced, progression, spec)
import Harmonia.Voicing (voicingMidi)
import Harmonia.Walk (seed)
import Vetula.Harmony (ChordNode, Kind(Voiced), noteName, scaleSet)

-- | Pads per cell: four by four, the Midifighter tile. Sixteen is also the mean
-- | cell size of the ungrouped grid — 480 chords over six rings and five levels
-- | is exactly sixteen — which is why the tile fits the model rather than just
-- | the hardware.
padRows :: Int
padRows = 4

padCols :: Int
padCols = 4

-- | **How far from home the walk may roam** — the row axis, `Harmonia.Freedom`
-- | grouped in pairs of rings. Cumulative, so `Far` contains `Near`.
data Reach = Near | Mid | Far

derive instance eqReach :: Eq Reach

reaches :: Array Reach
reaches = [ Near, Mid, Far ]

reachLabel :: Reach -> String
reachLabel = case _ of
  Near -> "near"
  Mid -> "mid"
  Far -> "far"

-- | Rings 0–1, 2–3, 4–5 as the freedom that admits them: ±2, ±4, ±6 on the
-- | circle of fifths. `Far` is the tritone and therefore everything.
reachFreedom :: Reach -> Freedom
reachFreedom = case _ of
  Near -> freedom 1
  Mid -> freedom 3
  Far -> freedom 5

-- | **How richly the walk may colour** — the column axis, `Harmonia.Palette`
-- | levels grouped. Cumulative in the same way: `Extended` contains plain
-- | triads, which is the source's own rule and the reason a set generated at
-- | the top still holds simple chords.
data Colour = Plain | Coloured | Extended

derive instance eqColour :: Eq Colour

colours :: Array Colour
colours = [ Plain, Coloured, Extended ]

colourLabel :: Colour -> String
colourLabel = case _ of
  Plain -> "plain"
  Coloured -> "coloured"
  Extended -> "extended"

-- | Basic+Low, Medium, High+Extreme — 8, 19 and 40 chord types cumulatively.
colourLevel :: Colour -> Level
colourLevel = case _ of
  Plain -> Low
  Coloured -> Medium
  Extended -> Extreme

-- | One bank: its place in the grid, the spec that generates it, and the
-- | sixteen chords that spec yields.
type Cell =
  { reach :: Reach
  , colour :: Colour
  , spec :: Spec
  , chords :: Array ChordNode
  }

-- | Is the key's mode a minor one? `Harmonia.Freedom` only needs the third,
-- | because only the third decides which side of the wheel home sits on.
minorMode :: Mode -> Boolean
minorMode = case _ of
  Aeolian -> true
  Dorian -> true
  Phrygian -> true
  Locrian -> true
  HarmonicMinor -> true
  MelodicMinor -> true
  _ -> false

-- | The spec behind one cell. `duplicates` is off so the walk prefers a chord
-- | it has not used yet — sixteen pads showing the same chord twice would waste
-- | the bank.
cellSpec :: Key -> Int -> Reach -> Colour -> Spec
cellSpec key roll r c =
  spec
    { tonic: mod key.tonic 12, minor: minorMode key.mode }
    (reachFreedom r)
    (colourLevel c)
    false
    (seed (cellSeed roll r c))
    (padRows * padCols)

-- | Fan one shuffle number out to nine, so the whole grid is reproducible from
-- | a single Int and no two cells walk the same path.
cellSeed :: Int -> Reach -> Colour -> Int
cellSeed roll r c = roll * 9973 + reachIx r * 31 + colourIx c * 7 + 1
  where
  reachIx = case _ of
    Near -> 0
    Mid -> 1
    Far -> 2
  colourIx = case _ of
    Plain -> 0
    Coloured -> 1
    Extended -> 2

-- | A generated chord as a pool node. The voicing arrives from
-- | `Harmonia.OpenVoicing` with its bass already pinned to the root, so the
-- | split here is the same one every other lens makes: lowest note becomes
-- | `bassPc`, the rest become `voicing`, and `playNotes` re-grounds the bass an
-- | octave below.
nodeOf :: Array Int -> Int -> Voiced -> ChordNode
nodeOf scl nid v =
  let
    midi = voicingMidi v.voicing
    Chord pcs = v.chord
    ps = nub (map (\p -> mod p 12) pcs)
  in
    { id: nid
    , parentId: Nothing
    , root: mod v.root 12
    , bassPc: mod (fromMaybe v.root (head midi)) 12
    , bassOct: 3
    , pcs: ps
    , voicing: drop 1 midi
    , kind: Voiced
    , label: noteName (mod v.root 12) <> typeSuffix v.chordType
    , pinned: false
    , outside: outsideCount scl ps
    -- The pads are laid out by the surface on a fixed grid, so there is no
    -- computed position to carry.
    , targetX: 0.0
    , targetY: 0.0
    , isCentre: false
    , anchor: Free
    }

-- | Non-scale tones, the same count the other lenses colour their glyphs by.
outsideCount :: Array Int -> Array Int -> Int
outsideCount scl ps = length (filter (\p -> not (elem p scl)) ps)

-- | Nine cells in reading order: rows are reach, columns are colour.
grid :: Key -> Int -> Array Cell
grid key roll = do
  r <- reaches
  c <- colours
  let
    sp = cellSpec key roll r c
    scl = scaleSet key
    base = 20000 + cellSeedIx r c * 100
  pure
    { reach: r
    , colour: c
    , spec: sp
    , chords: mapWithIndex (\i v -> nodeOf scl (base + i) v) (progression OV.defaults sp)
    }

cellSeedIx :: Reach -> Colour -> Int
cellSeedIx r c = rIx r * 3 + cIx c
  where
  rIx = case _ of
    Near -> 0
    Mid -> 1
    Far -> 2
  cIx = case _ of
    Plain -> 0
    Coloured -> 1
    Extended -> 2

-- | Sixteen chords of one cell, for a caller that only wants the pads.
cellNodes :: Cell -> Array ChordNode
cellNodes = _.chords
