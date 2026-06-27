-- | `Vetula.Generate` — the candidate-chord generator behind the Lattice's
-- | "pick mode". From the chords the user shift-selected in the progression, it
-- | proposes a cloud of plausible chords:
-- |
-- |   * Prepend / Append — a plausible chord before the first / after the last,
-- |     voice-led from that anchor;
-- |   * Transition — chords that sit BETWEEN two adjacent anchors A and B, by
-- |     voice-leading (no key needed) — the cross-family connector;
-- |   * Substitute — non-diatonic but plausible replacements for one middle
-- |     chord, sharing tones with it.
-- |
-- | Every candidate is voice-led from its anchor (so it's smooth to play),
-- | scored, blended with an `adventure` dial (0 = smoothest, 1 = most striking),
-- | and positioned on two meaningful axes — x = voice-leading distance (or which
-- | anchor it leans toward, for Transition), y = how far outside the scale.
module Vetula.Generate
  ( GenMode(..)
  , generateCandidates
  ) where

import Prelude

import Data.Array (drop, filter, head, index, length, mapWithIndex, nub, nubByEq, sort, sortBy, take, zipWith, (..))
import Data.Foldable (elem, foldl, maximum, minimum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (sqrt)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Vetula.Theory (Chord(..), Key)
import Vetula.Theory.Voicing (Voicing(..), voiceLead, voicingMidi)
import Vetula.Harmony (ChordNode, Kind(..), noteName, scaleSet)
import Vetula.Relate (roughness)

data GenMode = Prepend | Append | Transition | Substitute

-- | The chord vocabulary the cloud draws from: seven common qualities on every
-- | one of the twelve roots. Deliberately small + tonal for now.
type Template = { sfx :: String, ivs :: Array Int }

qualities :: Array Template
qualities =
  [ { sfx: "", ivs: [ 0, 4, 7 ] }
  , { sfx: "m", ivs: [ 0, 3, 7 ] }
  , { sfx: "dim", ivs: [ 0, 3, 6 ] }
  , { sfx: "7", ivs: [ 0, 4, 7, 10 ] }
  , { sfx: "maj7", ivs: [ 0, 4, 7, 11 ] }
  , { sfx: "m7", ivs: [ 0, 3, 7, 10 ] }
  , { sfx: "ø7", ivs: [ 0, 3, 6, 10 ] }
  ]

type Cand = { root :: Int, sfx :: String, pcs :: Array Int }

pool :: Array Cand
pool = do
  r <- 0 .. 11
  q <- qualities
  pure { root: r, sfx: q.sfx, pcs: nub (map (\iv -> mod (r + iv) 12) q.ivs) }

-- | Circular pitch-class distance (semitones, 0..6).
pcDist :: Int -> Int -> Int
pcDist a b = let d = mod (abs (a - b)) 12 in min d (12 - d)

-- | Voice-leading distance between two pitch-class sets: average nearest-tone
-- | motion. Small = smooth.
pcVLDist :: Array Int -> Array Int -> Number
pcVLDist as bs
  | length as == 0 || length bs == 0 = 0.0
  | otherwise = sum (map (\x -> toNumber (fromMaybe 6 (minimum (map (pcDist x) bs)))) as) / toNumber (length as)

commonTones :: Array Int -> Array Int -> Int
commonTones as bs = length (filter (\x -> elem x bs) as)

clampN :: Number -> Number -> Number -> Number
clampN lo hi x = max lo (min hi x)

-- small deterministic jitter so equal-axis candidates don't fully overlap
jitter :: Int -> Number -> Number
jitter i amp = (toNumber (mod (i * 7) 5) - 2.0) * amp

-- | Generate the candidate cloud. `anchors` is the selected chord(s): one for
-- | Prepend/Append/Substitute, two ([A, B]) for Transition. `startId` numbers the
-- | fresh nodes; `adventure` ∈ [0,1] tilts from smoothest to most striking.
generateCandidates :: GenMode -> Array ChordNode -> Key -> Number -> Int -> Array ChordNode
generateCandidates mode anchors key adventure startId =
  let scl = scaleSet key
      aPcs = maybe [] _.pcs (head anchors)
      bPcs = maybe aPcs _.pcs (index anchors 1)
      -- voice-lead from the anchor's MIDDLE-register voicing (not playNotes, whose
      -- low octave-3 bass would drag candidates below the glyph's staff)
      anchorPlay = maybe [] _.voicing (head anchors)

      smoothOf c = case mode of
        Transition -> pcVLDist c.pcs aPcs + pcVLDist c.pcs bPcs
        _ -> pcVLDist c.pcs aPcs

      outsideOf c = length (filter (\p -> not (elem p scl)) c.pcs)

      keep c =
        c.pcs /= aPcs && c.pcs /= bPcs &&
        (case mode of
          Substitute -> outsideOf c > 0 && commonTones c.pcs aPcs >= 1
          _ -> true)

      scored = map (\c -> { c, smooth: smoothOf c, outside: outsideOf c, striking: roughness c.pcs })
                 (filter keep pool)

      rankOf s = s.smooth * (1.0 - adventure) - (toNumber s.outside * 0.6 + s.striking * 2.5) * adventure

      ranked = take 14 (nubByEq (\a b -> a.c.pcs == b.c.pcs)
                          (sortBy (\x y -> compare (rankOf x) (rankOf y)) scored))
      maxSmooth = fromMaybe 1.0 (maximum (map _.smooth ranked))

      placeX i s = case mode of
        Transition -> clampN (-280.0) 280.0 ((pcVLDist s.c.pcs bPcs - pcVLDist s.c.pcs aPcs) * 70.0 + jitter i 22.0)
        _ -> clampN (-280.0) 280.0 (-260.0 + s.smooth / (max 0.1 maxSmooth) * 520.0 + jitter i 22.0)
      placeY i s = clampN (-200.0) 200.0 (90.0 - toNumber s.outside * 60.0 + jitter (i + 2) 26.0)

      -- voiced notes + a collision radius + an initial placement per candidate;
      -- then relax positions so the discs stop overlapping (the cloud is static,
      -- so we de-overlap here instead of with a force).
      prelim = mapWithIndex
        (\i s ->
          let midi = sort (voicingMidi (voiceLead (Voicing anchorPlay) (Chord s.c.pcs)))
              span = toNumber (fromMaybe 0 (maximum midi) - fromMaybe 0 (minimum midi))
          in { i, s, midi, r: min 40.0 (12.0 + span * 0.7), x0: placeX i s, y0: placeY i s })
        ranked
      relaxed = relax 60 (map (\p -> { x: p.x0, y: p.y0, r: p.r }) prelim)

      build p pos =
        let bp = mod (fromMaybe 60 (head p.midi)) 12
        in { id: startId + p.i, parentId: Nothing, root: p.s.c.root, bassPc: bp
           , pcs: nub p.s.c.pcs, voicing: drop 1 p.midi, kind: Voiced
           , label: noteName p.s.c.root <> p.s.c.sfx, pinned: false, outside: p.s.outside
           , targetX: pos.x, targetY: pos.y, isCentre: false }
  in zipWith build prelim relaxed

-- | Push overlapping discs apart over a few iterations (a static stand-in for a
-- | collision force, since the candidate cloud isn't simulated).
relax :: Int -> Array { x :: Number, y :: Number, r :: Number } -> Array { x :: Number, y :: Number, r :: Number }
relax iters ps0 = go iters ps0
  where
  go n ps
    | n <= 0 = ps
    | otherwise = go (n - 1) (mapWithIndex (nudge ps) ps)
  nudge ps i pi =
    let d = foldl
          (\acc (Tuple j pj) ->
            if i == j then acc
            else
              let dx = pi.x - pj.x
                  dy = pi.y - pj.y
                  dist = sqrt (dx * dx + dy * dy)
                  minD = pi.r + pj.r + 7.0
              in if dist < minD && dist > 0.001
                   then { x: acc.x + dx / dist * (minD - dist) * 0.5, y: acc.y + dy / dist * (minD - dist) * 0.5 }
                   else acc)
          { x: 0.0, y: 0.0 }
          (mapWithIndex Tuple ps)
    in pi { x = clampN (-300.0) 300.0 (pi.x + d.x), y = clampN (-220.0) 210.0 (pi.y + d.y) }
