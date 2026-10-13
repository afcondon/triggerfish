-- | Triggerfish.Balistes.Pattern — the fixed rhythm (now `Reef.Balistes.Pattern`,
-- | so the rig reads the same type off the stage) plus the starter rhythms the
-- | app ships.
module Triggerfish.Balistes.Pattern
  ( module Reef.Balistes.Pattern
  , bundledPatterns
  , houseLoTempo110
  , genreStarters
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Reef.Balistes.Pattern

-- | The starter rhythms shipped with the app — the OFFLINE FALLBACK. When
-- | Amphora is reachable Balistes sources its library from the store
-- | (`balistes-grid` collection); these are what it falls back to when the
-- | store is down or empty, and they double as the seed content the store is
-- | populated from (via `scripts/seed-balistes.mjs`, which serialises each with
-- | this module's own Lepidoptera printer — one source of truth).
bundledPatterns :: Array FixedPattern
bundledPatterns = [ houseLoTempo110 ] <> genreStarters

-- | Genre starter kits — one archetypal beat per dance genre, each named with
-- | its characteristic tempo (`"<genre> <bpm>"`, matching `houseLoTempo110`).
-- | Authored from genre convention (four-on-the-floor house, boom-bap, dembow,
-- | two-step, …), NOT transcribed from any copyrighted pattern book — the
-- | defining rhythmic skeleton of a genre is common musical practice. One bar
-- | of sixteenths (16 steps) unless the genre wants two. Velocities: 120 accent,
-- | 100 normal, 64 ghost. Grow this freely (Pocket Operations imports,
-- | hand-entered favourites) — every addition seeds the store the same way.
genreStarters :: Array FixedPattern
genreStarters =
  [ p16 "house 122"
      [ Tuple 0 [ 120,0,0,0, 120,0,0,0, 120,0,0,0, 120,0,0,0 ]   -- BD four-on-floor
      , Tuple 2 [ 0,0,0,0, 120,0,0,0, 0,0,0,0, 120,0,0,0 ]        -- CP backbeat
      , Tuple 4 [ 64,0,64,0, 64,0,64,0, 64,0,64,0, 64,0,64,0 ]    -- CH straight 8ths
      , Tuple 6 [ 0,0,100,0, 0,0,100,0, 0,0,100,0, 0,0,100,0 ]    -- OH offbeat (the house lift)
      ]
  , p16 "techno 130"
      [ Tuple 0 [ 120,0,0,0, 120,0,0,0, 120,0,0,0, 120,0,0,0 ]    -- BD four-on-floor
      , Tuple 2 [ 0,0,0,0, 64,0,0,0, 0,0,0,0, 64,0,0,0 ]          -- CP ghost backbeat
      , Tuple 4 [ 0,0,100,0, 0,0,100,0, 0,0,100,0, 0,0,100,0 ]    -- CH insistent offbeat
      , Tuple 6 [ 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,110,0 ]          -- OH bar-end open
      ]
  , p16 "trance 138"
      [ Tuple 0 [ 120,0,0,0, 120,0,0,0, 120,0,0,0, 120,0,0,0 ]    -- BD four-on-floor
      , Tuple 2 [ 0,0,0,0, 120,0,0,0, 0,0,0,0, 120,0,0,0 ]        -- CP backbeat
      , Tuple 4 [ 60,0,60,0, 60,0,60,0, 60,0,60,0, 60,0,60,0 ]    -- CH 8ths
      , Tuple 6 [ 0,0,100,0, 0,0,100,0, 0,0,100,0, 0,0,100,0 ]    -- OH offbeat
      ]
  , p16 "disco 120"
      [ Tuple 0 [ 120,0,0,0, 120,0,0,0, 120,0,0,0, 120,0,0,0 ]    -- BD four-on-floor
      , Tuple 1 [ 0,0,0,0, 120,0,0,0, 0,0,0,0, 120,0,0,0 ]        -- SD backbeat
      , Tuple 4 [ 60,0,60,0, 60,0,60,0, 60,0,60,0, 60,0,60,0 ]    -- CH 8ths
      , Tuple 6 [ 0,0,100,0, 0,0,100,0, 0,0,100,0, 0,0,100,0 ]    -- OH offbeat
      ]
  , p16 "boom bap 90"
      [ Tuple 0 [ 120,0,0,0, 0,0,0,0, 110,0,0,110, 0,0,0,0 ]      -- BD 1, 3, a-of-3
      , Tuple 1 [ 0,0,0,0, 120,0,0,0, 0,0,64,0, 120,0,0,0 ]       -- SD backbeat + ghost
      , Tuple 4 [ 80,0,80,0, 80,0,80,0, 80,0,80,0, 80,0,80,0 ]    -- CH 8ths (apply Dilla push for swing)
      ]
  , p16 "trap 140"
      [ Tuple 0 [ 120,0,0,0, 0,0,110,0, 0,0,110,0, 0,0,0,0 ]      -- BD syncopated 808
      , Tuple 1 [ 0,0,0,0, 0,0,0,0, 120,0,0,0, 0,0,0,0 ]          -- SD half-time (beat 3)
      , Tuple 2 [ 0,0,0,0, 0,0,0,0, 120,0,0,0, 0,0,0,0 ]          -- CP with snare
      , Tuple 4 [ 90,0,90,0, 90,0,90,0, 90,0,90,0, 90,90,90,90 ]  -- CH rolling hats + bar-end roll
      ]
  , p16 "dnb 174"
      [ Tuple 0 [ 120,0,0,0, 0,0,0,0, 0,0,110,0, 0,0,0,0 ]        -- BD 1 + and-of-3
      , Tuple 1 [ 0,0,0,0, 120,0,0,0, 0,0,0,0, 120,0,0,64 ]       -- SD two-step backbeat + ghost
      , Tuple 4 [ 0,0,90,0, 0,0,90,0, 0,0,90,0, 0,0,90,0 ]        -- CH offbeat
      ]
  , p16 "breakbeat 130"
      [ Tuple 0 [ 120,0,0,0, 0,0,0,0, 0,0,110,0, 0,0,0,0 ]        -- BD 1 + and-of-3
      , Tuple 1 [ 0,0,0,0, 120,0,0,64, 0,0,0,0, 120,0,0,64 ]      -- SD backbeat + ghosts
      , Tuple 4 [ 0,0,90,0, 0,0,90,0, 0,0,90,0, 0,0,90,0 ]        -- CH offbeat
      ]
  , p16 "2-step 135"
      [ Tuple 0 [ 120,0,0,0, 0,0,0,0, 0,0,110,0, 0,0,0,0 ]        -- BD 1 + and-of-3
      , Tuple 1 [ 0,0,0,0, 120,0,0,0, 0,0,0,0, 120,0,0,0 ]        -- SD backbeat
      , Tuple 2 [ 0,0,0,0, 120,0,0,0, 0,0,0,0, 120,0,0,0 ]        -- CP with snare
      , Tuple 4 [ 90,0,0,90, 0,0,90,0, 0,0,90,0, 0,90,0,0 ]       -- CH skippy shuffle
      , Tuple 6 [ 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,100,0 ]          -- OH bar-end
      ]
  , p16 "dembow 95"
      [ Tuple 0 [ 120,0,0,0, 120,0,0,0, 120,0,0,0, 120,0,0,0 ]    -- BD four-on-floor
      , Tuple 1 [ 0,0,0,110, 0,0,110,0, 0,0,0,110, 0,0,110,0 ]    -- SD dembow (boom-ch-boom-chick)
      , Tuple 4 [ 60,0,60,0, 60,0,60,0, 60,0,60,0, 60,0,60,0 ]    -- CH 8ths
      ]
  , p16 "funk 100"
      [ Tuple 0 [ 120,0,0,100, 0,0,0,0, 0,0,100,0, 0,0,0,0 ]      -- BD syncopated
      , Tuple 1 [ 0,0,0,0, 120,0,0,64, 0,0,0,0, 120,0,64,0 ]      -- SD backbeat + ghosts
      , Tuple 4 [ 80,60,60,60, 80,60,60,60, 80,60,60,60, 80,60,60,60 ] -- CH 16ths
      ]
  , p16 "dubstep 140"
      [ Tuple 0 [ 120,0,0,0, 0,0,0,0, 0,0,110,0, 0,0,0,0 ]        -- BD 1 + and-of-3
      , Tuple 1 [ 0,0,0,0, 0,0,0,0, 120,0,0,0, 0,0,0,0 ]          -- SD half-time (beat 3)
      , Tuple 4 [ 0,0,80,0, 0,0,80,0, 0,0,80,0, 0,0,80,0 ]        -- CH sparse offbeat
      ]
  ]
  where
  p16 name rows = { name, steps: 16, notes: defaultNotes, grid: buildGrid 16 rows }

-- | "Lo-tempo house, 110 bpm" — 2 bars (32 sixteenths), six voices: four-on-the-
-- | floor kick with ghost pushes, clap backbeat, off-beat open hats, a driving
-- | accented ride, one tom and a few crashes as the book's arrangement garnish.
houseLoTempo110 :: FixedPattern
houseLoTempo110 =
  { name: "lo house 110"
  , steps: 32
  , notes: defaultNotes
  , grid: buildGrid 32
      [ Tuple 0 [ 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 98, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 98, 0 ] -- BD
      , Tuple 2 [ 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0 ] -- CP
      , Tuple 6 [ 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0 ] -- OH
      , Tuple 9 [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] -- HT
      , Tuple 10 [ 127, 100, 127, 0, 127, 0, 127, 82, 127, 127, 127, 0, 127, 0, 127, 127, 127, 127, 127, 0, 127, 0, 127, 127, 127, 127, 127, 0, 127, 0, 127, 127 ] -- RD
      , Tuple 12 [ 0, 0, 0, 98, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0 ] -- CR
      ]
  }
