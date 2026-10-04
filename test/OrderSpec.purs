-- | **The order within a column, from the wiring** (`Triggerfish.Flow.Order`).
-- |
-- | One case drawn from the chart (AC's screenshot, 2026-10-04: the Rample
-- | fed from the first port, drawn second), and random wirings, which must
-- | never come out crossing more than the fixed list did.
module Test.OrderSpec (runOrderTests) where

import Prelude

import Data.Array as Array
import Data.Array (all, concatMap, filter, foldl, length, mapWithIndex, nub, range, sort, sortWith, (!!))
import Data.Int (toNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Flow (Column(..), Flow, Link, Node, Signal(..), nodeRank)
import Triggerfish.Flow.Order (Order, crossings, orderOf)

check :: String -> Boolean -> Effect Unit
check name ok = do
  assert' ("order: " <> name) ok
  log ("  ✓ " <> name)

node :: String -> Column -> String -> Node
node id column name = { id, column, name, note: "", machine: Nothing }

wire :: String -> String -> Link
wire from to = { from, to, signal: Midi, machine: "x", streams: 1, broken: 0, wires: [], notes: [], waiting: 0, control: false, bone: false }

-- | The fixed list's order: what the chart drew before.
fixedOrder :: Flow -> Order
fixedOrder f = Map.fromFoldable (concatMap (\c -> mapWithIndex (\i nd -> Tuple nd.id i) (sortWith nodeRank (filter (\nd -> nd.column == c) f.nodes))) (nub (map _.column f.nodes)))

-- | The screenshot: four ports and the FH-2 out of the engine; the Rample
-- | on the first port, Ableton on the two IAC buses, the modular on the FH-2.
screenshot :: Flow
screenshot =
  { nodes:
      [ node "engine" Engine "Architeuthis", node "sets" Engine "Sample sets"
      , node "port:din" Interface "AUDIO4c DIN", node "port:usb2" Interface "AUDIO4c USB2"
      , node "port:iac" Interface "IAC", node "port:tidal" Interface "IAC Driver Tidal", node "fh2" Interface "FH-2"
      , node "ableton" Instrument "Ableton", node "rample" Instrument "Rample", node "d-dirt" Instrument "SuperDirt"
      , node "modular" Instrument "The modular", node "inst:usb2" Instrument "AUDIO4c USB2"
      , node "ears" Heard "Your ears"
      ]
  , links:
      map (wire "engine") [ "port:din", "port:usb2", "port:iac", "port:tidal", "fh2", "d-dirt" ]
        <> [ wire "sets" "d-dirt", wire "port:din" "rample", wire "port:usb2" "inst:usb2", wire "port:iac" "ableton"
           , wire "port:tidal" "ableton", wire "fh2" "modular" ]
        <> map (\i -> wire i "ears") [ "ableton", "rample", "d-dirt", "modular", "inst:usb2" ]
  }

-- | A small linear congruential generator, so a failing wiring can be named
-- | by its seed.
next :: Int -> Int
next s = s * 1103515245 + 12345  -- wraps at 32 bits, as Int does

-- | `k` numbers below `n`, and the seed after them.
draws :: Int -> Int -> Int -> Tuple (Array Int) Int
draws k n s0 = foldl (\(Tuple xs s) _ -> let s' = next s in Tuple (xs <> [ ((s' / 65536) `mod` 32768) `mod` n ]) s') (Tuple [] s0) (range 1 k)

-- | A random rig: one or two engine nodes, up to three rig outs, two to six
-- | interfaces and instruments, each fed from one to three nodes of the
-- | column before, and every instrument heard.
randomFlow :: Int -> Flow
randomFlow seed =
  let
    Tuple sizes s1 = draws 4 5 seed
    count i lo = lo + fromMaybe 0 (sizes !! i)
    col c prefix k = map (\i -> node (prefix <> show i) c (prefix <> show i)) (range 1 k)
    engines = col Engine "e" (1 + (count 0 0) `mod` 2)
    outs = col RigOut "o" ((count 1 0) `mod` 4)
    ifaces = col Interface "i" (count 2 2)
    insts = col Instrument "n" (count 3 2)
    layers = filter (\l -> length l > 0) [ engines, outs, ifaces, insts ]
    feed (Tuple acc s) (Tuple prev cur) =
      foldl (\(Tuple a s') nd ->
        let
          Tuple ks s'' = draws 3 (length prev) s'
          Tuple m s3 = draws 1 3 s''
          srcs = nub (map (\k -> fromMaybe nd (prev !! k)) (sort (filter (const true) (Array.take (1 + fromMaybe 0 (m !! 0)) ks))))
        in Tuple (a <> map (\p -> wire p.id nd.id) srcs) s3) (Tuple acc s) cur
    pairs = Array.zip layers (Array.drop 1 layers)
    Tuple links _ = foldl feed (Tuple [] s1) pairs
  in
    { nodes: concatMap identity layers <> [ node "ears" Heard "Your ears" ]
    , links: links <> map (\nd -> wire nd.id "ears") insts
    }

-- | Every order of a list.
perms :: forall a. Array a -> Array (Array a)
perms xs
  | Array.null xs = [ [] ]
  | otherwise = concatMap (\i -> case xs !! i, Array.deleteAt i xs of
      Just x, Just rest -> map (Array.cons x) (perms rest)
      _, _ -> []) (range 0 (length xs - 1))

-- | The fewest crossings any order of the wired columns can give, by
-- | trying them all, column by column: with lines only between neighbouring
-- | columns, the crossings are a sum over neighbouring pairs, so the best
-- | for each order of a column, given the best up to the column before, is
-- | enough (dynamic programming over the chain). The engine's column keeps
-- | its fixed order, as `orderOf` does.
optimum :: Flow -> Int
optimum f = fromMaybe 0 (Array.head (sort run))
  where
  cols = Array.sort (nub (map _.column (filter (\nd -> nd.column /= Heard) f.nodes)))
  idsIn c = map _.id (sortWith nodeRank (filter (\nd -> nd.column == c) f.nodes))
  layers = cols <#> \c -> if c == Engine then [ idsIn c ] else perms (idsIn c)
  ls = filter (not <<< _.bone) f.links
  -- the crossings between one order of a column and one of the next
  pairCost pa pb =
    let
      posA = Map.fromFoldable (mapWithIndex (\i id -> Tuple id i) pa)
      posB = Map.fromFoldable (mapWithIndex (\i id -> Tuple id i) pb)
      between = ls # Array.mapMaybe \l -> case Map.lookup l.from posA, Map.lookup l.to posB of
        Just a, Just b -> Just (Tuple a b)
        _, _ -> Nothing
      idx = mapWithIndex Tuple between
    in
      length (concatMap (\(Tuple i (Tuple a1 b1)) -> filter (\(Tuple j (Tuple a2 b2)) -> j > i && a1 /= a2 && b1 /= b2 && (a1 - a2) * (b1 - b2) < 0) idx) idx)
  -- the fewest crossings ending in each order of the latest column
  run = Array.foldl (\acc k ->
      let
        prev = fromMaybe [] (layers !! (k - 1))
        cur = fromMaybe [] (layers !! k)
      in
        cur <#> \pb -> fromMaybe 0 (Array.head (sort (mapWithIndex (\i pa -> fromMaybe 0 (acc !! i) + pairCost pa pb) prev))))
    (map (const 0) (fromMaybe [] (layers !! 0))) (range 1 (length layers - 1))

runOrderTests :: Effect Unit
runOrderTests = do
  log "\n════ Flow.Order: the order within a column, from the wiring ════"
  let ord = orderOf screenshot
  let at id = fromMaybe 99 (Map.lookup id ord)
  log ("  screenshot: " <> show (crossings screenshot (fixedOrder screenshot)) <> " crossings drawn in the fixed order, " <> show (crossings screenshot ord) <> " now")
  check "the Rample, fed from the first port, comes above Ableton"
    (at "rample" < at "ableton")
  check "the modular, fed from the FH-2 at the foot, stays at the foot"
    (at "modular" == 4)
  check "the screenshot's wiring is drawn with no crossings"
    (crossings screenshot ord == 0)

  let seeds = range 1 300
  let results = seeds <#> \seed -> let f = randomFlow seed in { seed, f, before: crossings f (fixedOrder f), after: crossings f (orderOf f) }
  let total g = foldl (+) 0 (map g results)
  log ("  random: " <> show (length results) <> " wirings, " <> show (total _.before) <> " crossings in the fixed order, "
    <> show (total _.after) <> " now; " <> show (length (filter (\r -> r.after < r.before) results)) <> " improved, "
    <> show (length (filter (\r -> r.after == 0) results)) <> " with none left")
  check "no random wiring crosses more than the fixed order drew it"
    (all (\r -> r.after <= r.before) results)
  check "every column's order is a permutation of its places"
    ( results # all \r ->
        let
          o = orderOf r.f
          cols = nub (map _.column r.f.nodes)
        in cols # all \c ->
          let ids = map _.id (filter (\nd -> nd.column == c) r.f.nodes)
          in sort (map (\id -> fromMaybe (-1) (Map.lookup id o)) ids) == range 0 (length ids - 1)
    )
  -- how far from the best any order could do: worked out exactly where
  -- the columns are small enough to try every order (the first hundred
  -- seeds; all three hundred take a minute). Measured 2026-10-04 over all
  -- of them: optimal in 152 of 184, 409 crossings against a best of 352,
  -- never more than 4 off, which is why the chart does not search.
  let small = filter (\r -> r.seed <= 100 && all (\c -> length (filter (\nd -> nd.column == c) r.f.nodes) <= 5) [ RigOut, Interface, Instrument ]) results
  let gaps = small <#> \r -> { r, best: optimum r.f }
  let sumOf g = foldl (+) 0 (map g gaps)
  log ("  exact, " <> show (length gaps) <> " wirings small enough: best possible " <> show (sumOf _.best) <> " crossings in all, heuristic "
    <> show (sumOf _.r.after) <> "; optimal in " <> show (length (filter (\g -> g.r.after == g.best) gaps))
    <> ", worst gap " <> show (foldl max 0 (map (\g -> g.r.after - g.best) gaps)))
  check "the heuristic never beats the exact optimum (the measure is sound)"
    (all (\g -> g.best <= g.r.after) gaps)
  check "the order is the same each time it is worked out"
    (all (\r -> orderOf r.f == orderOf r.f) results)
  log ("  (mean " <> show (Int.round (toNumber (total _.before) / 3.0)) <> " → " <> show (Int.round (toNumber (total _.after) / 3.0)) <> " crossings a hundred wirings)")
