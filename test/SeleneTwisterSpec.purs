-- | The Selene page's Twister surfaces (`Triggerfish.Selene.Twister`).
module Test.SeleneTwisterSpec (runSeleneTwisterTests) where

import Prelude

import Data.Array (all, catMaybes, filter, length, mapWithIndex, nub, (!!), (..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Selene.Model as M
import Triggerfish.Selene.Twister as Tw

-- | The page's arithmetic stubbed: the tests are about layout and painting,
-- | not about the clock ladder or the LFO's shape view.
ops :: Tw.Ops
ops =
  { clockSpeeds: 9
  , clockSpeed: const 2
  , setClockSpeed: \_ sl -> sl
  , lfoShape: const 0
  , lfoAmp: const 0.8
  , setLfoShape: \_ _ sl -> sl
  }

-- | One destination of each kind, in this order.
kinds :: Array M.GenKind
kinds = [ M.KEnv, M.KLfo, M.KEuclid, M.KClock, M.KNote ]

rack :: M.Selene
rack =
  { destinations: map
      (\k -> { target: M.defaultTargetFor k, jacks: M.allJacks, range: Nothing, bank: M.freshBank k, retrig: Nothing, muted: false, mutedSlots: [], midi: [] })
      kinds
  }

at :: Int -> Boolean -> Array (Maybe Tw.Control)
at d open = Tw.surface ops rack { sel: Just { dest: d, slot: 0 }, open, rows: drawn }

-- | The order the page draws them in, which is not the rack's.
drawn :: Array Int
drawn = [ 2, 0, 4, 1, 3 ]

-- | Which of the sixteen encoders are lit.
litOf :: Array (Maybe Tw.Control) -> Array Int
litOf cs = catMaybes (mapWithIndex (\i c -> if isJust c then Just i else Nothing) cs)

firstGoto :: Array Tw.Act -> Maybe { slot :: Int, dest :: Int, open :: Boolean }
firstGoto = case _ of
  [ Tw.Goto s open ] -> Just { slot: s.slot, dest: s.dest, open }
  _ -> Nothing

runSeleneTwisterTests :: Effect Unit
runSeleneTwisterTests = do
  let surfaces = map (\d -> litOf (at d true)) (0 .. 4)
  assert' "every kind's surface is lit in its own pattern" $
    length (nub surfaces) == length kinds
  assert' "the envelope's pattern is ADSR over depth, velocity, time" $
    surfaces !! 0 == Just [ 0, 1, 2, 3, 8, 9, 10, 12, 13, 14, 15 ]
  assert' "the overview lights the eight signals and the bottom row" $
    litOf (at 1 false) == [ 0, 1, 2, 3, 4, 5, 6, 7, 12, 13, 14, 15 ]
  assert' "the bottom row is the same on every surface" $
    all (\cs -> all (\i -> isJust (join (cs !! i))) [ 12, 13, 14, 15 ])
      (map (\d -> at d true) (0 .. 4) <> map (\d -> at d false) (0 .. 4))

  -- Navigation
  let row = join (at 0 true !! 12)
      col = join (at 0 true !! 13)
  assert' "turning ROW leaves the entered signal, and walks the drawn order" $
    (row <#> \c -> firstGoto (c.turn 127)) == Just (Just { slot: 0, dest: 3, open: false })
      && (row <#> \c -> firstGoto (c.turn 0)) == Just (Just { slot: 0, dest: 2, open: false })
  assert' "ROW's ring sits at the row's place on screen, not in the rack" $
    (row <#> _.pos) == Just 32
  assert' "turning COLUMN walks the row and stays entered" $
    (col <#> \c -> firstGoto (c.turn 127)) == Just (Just { slot: 7, dest: 0, open: true })
  assert' "pushing COLUMN leaves, and in the overview enters" $
    (col <#> \c -> firstGoto c.press) == Just (Just { slot: 0, dest: 0, open: false })
      && (join (at 0 false !! 13) <#> \c -> firstGoto c.press) == Just (Just { slot: 0, dest: 0, open: true })

  -- Every control's own ring means its own value, so nothing is edited or
  -- repainted just by being shown.
  let everything = catMaybes (join (map (\d -> at d true <> at d false) (0 .. 4)))
  assert' "a painted ring means its value" $ all (\c -> c.means c.pos) everything
  assert' "turning to where the ring already is edits nothing" $
    all (\c -> length (c.turn c.pos) == 0) everything

  -- Painting: the first frame writes, the second has nothing to say.
  let cs = at 0 true
      first = Tw.paint cs { heard: Map.empty, lit: Map.empty }
      second = Tw.paint cs { heard: Map.empty, lit: first.lit }
  assert' "the first frame lights all sixteen" $ Map.size first.lit == 16
  assert' "an unchanged page writes nothing" $ length second.writes == 0
  assert' "a dark encoder is dimmed, not left showing" $
    length (filter isLightOff first.writes) == 16 - length (litOf cs)
  log "PASS - Selene Twister: one pattern per kind, navigation, rings that keep still"
  where
  isLightOff = case _ of
    Tw.Light _ false -> true
    _ -> false
