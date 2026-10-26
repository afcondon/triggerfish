-- | The Selene page on a Midifighter Twister: what each encoder does, and
-- | what it shows.
-- |
-- | Layer 2 (the device's bank 3), so the Odonus grid keeps banks 1 and 2 and
-- | both pages can be open at once. The device is Helm's (`Helm.Twister`);
-- | this is the binding, pure, and the component only carries it out.
-- |
-- | ## Two surfaces and a fixed bottom row
-- |
-- | The bottom row is the same everywhere: ROW (which destination), COLUMN
-- | (which of its eight signals), MUTE, APPLY. What the top twelve do depends
-- | on whether a signal has been **entered**:
-- |
-- | - **Overview**: the top two rows are the destination's eight signals,
-- |   each on its main field, for roughing out a whole row at once. Turning
-- |   one moves COLUMN to it, so the page outlines what the hand is on, and
-- |   pushing one enters it.
-- | - **A signal entered** (push COLUMN; push again to leave): the top rows
-- |   become that kind's own surface. Turning COLUMN walks the row and stays
-- |   entered, since every signal on a row is the same kind: that is how a
-- |   bank of eight gets set quickly. Turning ROW leaves, because the next row
-- |   may be another kind (AC, 2026-10-17: push COLUMN again to go in).
-- |
-- | ## Each kind looks like itself
-- |
-- | Deliberately *not* one layout reused: every kind gets the fewest controls
-- | it needs, placed in its own shape, in its own colour, with everything
-- | else dark, so which kind is under the hand is legible before reading
-- | anything (AC). On the top twelve:
-- |
-- | ```
-- | envelope    LFO         euclid      clock       note
-- | A D S R     . . . .     B . . .     S . . W     . . . .
-- | . . . .     . R P .     N . . .     . . . .     . O . .
-- | D V T .     . W A .     R . . .     . . . .     . S . .
-- | ```
-- |
-- | Envelope: attack, decay, sustain, release over depth, velocity depth,
-- | time range. LFO: rate, phase over waveform, amplitude. Euclid: beats,
-- | steps, rate. Clock: speed (the page's ÷4…×16 ladder) and pulse width.
-- | Note: octave over semitone. These are the fields the page's arrow keys
-- | reach; the rest stay in the SOURCE pane.
-- |
-- | ## Rings are painted from the model
-- |
-- | As on Odonus (`Triggerfish.Odonus.Twister`), and with its one exception:
-- | an encoder whose last reported position already *means* the value is left
-- | alone, or a field with few values (an euclid's steps, a clock's nine
-- | speeds) would snap back under a slow hand on every click.
module Triggerfish.Selene.Twister
  ( layer
  , Cursor
  , Act(..)
  , Control
  , Ops
  , surface
  , Look
  , Write(..)
  , paint
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, elem, elemIndex, length, mapWithIndex, range, replicate, (!!))
import Data.Int (round, toNumber)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (exp, log)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Helm.Twister as HT
import Triggerfish.Selene.Model as M

-- | The device layer this page owns.
layer :: Int
layer = 2

-- | Where the hand is: the page's own selection, whether it is entered, and
-- | the destinations in the order the page draws them, which is what ROW
-- | walks. That is not the rack's own order: the page lists the rig's outputs
-- | in the rig's order, then anything off it (`Component.rackRows`).
type Cursor = { sel :: Maybe { dest :: Int, slot :: Int }, open :: Boolean, rows :: Array Int }

-- | What a gesture asks the page to do.
data Act
  = Edit Int (M.GenBank -> M.GenBank)
  | Goto { dest :: Int, slot :: Int } Boolean
  | Mute Int
  | Apply

-- | One encoder: where its ring should sit, its colour, whether a device
-- | position already says its value, and what turning or pushing it does.
type Control =
  { pos :: Int
  , hue :: Int
  , means :: Int -> Boolean
  , turn :: Int -> Array Act
  , press :: Array Act
  }

-- | The page's own slot arithmetic, passed in rather than copied: the clock's
-- | speed ladder and the LFO's dominant-shape view live in
-- | `Triggerfish.Selene.Component`, and the Twister must step exactly as the
-- | arrow keys do.
type Ops =
  { clockSpeeds :: Int
  , clockSpeed :: M.ClockSlot -> Int
  , setClockSpeed :: Int -> M.ClockSlot -> M.ClockSlot
  , lfoShape :: M.ModSlot -> Int
  , lfoAmp :: M.ModSlot -> Number
  , setLfoShape :: Int -> Number -> M.ModSlot -> M.ModSlot
  }

-- Colours: PWYF's wheel positions (`Data.Looper.Twister.hue`), so the two
-- apps speak one palette. Approximate, as they are there.
red :: Int
red = 78

orange :: Int
orange = 68

yellow :: Int
yellow = 58

green :: Int
green = 40

teal :: Int
teal = 30

blue :: Int
blue = 12

violet :: Int
violet = 100

hueOf :: M.GenKind -> Int
hueOf = case _ of
  M.KEnv -> orange
  M.KVoice -> red
  M.KLfo -> teal
  M.KLfoT -> teal
  M.KEuclid -> green
  M.KClock -> yellow
  M.KNote -> violet
  M.KThrow -> orange
  M.KVolt -> pink

pink :: Int
pink = 112

-- | The sixteen encoders, top-left first; `Nothing` is dark.
surface :: Ops -> M.Selene -> Cursor -> Array (Maybe Control)
surface ops sel cur = top <> bottom
  where
  dests = sel.destinations
  here = fromMaybe { dest: fromMaybe 0 (cur.rows !! 0), slot: 0 } cur.sel
  dest = dests !! here.dest
  rowAt = fromMaybe 0 (elemIndex here.dest cur.rows)
  hue = maybe blue (hueOf <<< M.bankKind <<< _.bank) dest
  slotsOf i = maybe M.slotCount (M.bankSize <<< _.bank) (dests !! i)

  bottom =
    [ Just (stepped (length cur.rows - 1) rowAt hue \r ->
        let to = fromMaybe here.dest (cur.rows !! r)
        -- the same slot where the new row has it: slot 7 of a whole bank is
        -- slot 3 of a row of four, not a slot that isn't there
        in Goto { dest: to, slot: min here.slot (slotsOf to - 1) } false)
    -- a row of four steps over four
    , Just ((stepped (maybe M.slotCount (M.bankSize <<< _.bank) dest - 1) here.slot hue \s -> Goto { dest: here.dest, slot: s } cur.open)
        { press = [ Goto here (not cur.open) ] })
    , dest <#> \d -> button (if d.muted then red else blue) (if d.muted then 127 else 0) (Mute here.dest)
    , Just (button blue 0 Apply)
    ]

  top = case dest of
    Nothing -> dark 12
    Just d
      | cur.open -> detail ops (M.voltRange d) here.dest here.slot d.bank
      | otherwise -> overview ops (M.voltRange d) here.dest d.bank <> dark 4

-- | Eight signals on their main fields.
overview :: Ops -> M.OutputRange -> Int -> M.GenBank -> Array (Maybe Control)
overview ops outRange d bank = map (\j -> follow j <$> main j) (range 0 (M.slotCount - 1))
  where
  follow j c = c
    { turn = \p -> case c.turn p of
        [] -> []
        acts -> acts <> [ Goto { dest: d, slot: j } false ]
    , press = [ Goto { dest: d, slot: j } true ]
    }
  main j = case bank of
    M.GLfo xs -> xs !! j <#> \sl -> lfoRate (put M.GLfo xs j) sl
    M.GLfoT xs -> xs !! j <#> \sl -> lfoTSpeed (put M.GLfoT xs j) sl
    M.GEnv xs -> xs !! j <#> \sl -> int 0 127 sl.depth orange (put M.GEnv xs j <<< sl { depth = _ })
    M.GEuclid xs -> xs !! j <#> \sl -> int 0 sl.steps sl.beats green (put M.GEuclid xs j <<< sl { beats = _ })
    M.GClock xs -> xs !! j <#> \sl -> clockSpeed ops (put M.GClock xs j) sl
    M.GNote xs -> xs !! j <#> \sl -> int 0 127 sl.note violet (put M.GNote xs j <<< sl { note = _ })
    -- a voice's channel: what plays it
    M.GVoice vb -> vb.voices !! j <#> \v -> int 1 16 v.channel red (put (\vs -> M.GVoice vb { voices = vs }) vb.voices j <<< v { channel = _ })
    -- a throw's lengths are written, not turned: dark
    M.GThrow _ -> Nothing
    -- a voltage across the bank's range, as its slider
    M.GVolt xs -> xs !! j <#> \sl -> volts outRange (put M.GVolt xs j) sl
  put :: forall a. (Array a -> M.GenBank) -> Array a -> Int -> a -> Act
  put mk xs j x = Edit d (const (mk (setAt j xs x)))

-- | One signal's own surface: the twelve top encoders, mostly dark.
detail :: Ops -> M.OutputRange -> Int -> Int -> M.GenBank -> Array (Maybe Control)
detail ops outRange d j bank = fromMaybe (dark 12) case bank of
  M.GEnv xs -> xs !! j <#> \sl ->
    let f v set = int 0 127 v orange (put M.GEnv xs <<< set)
    in place [ 0, 1, 2, 3, 8, 9, 10 ]
         [ f sl.attack sl { attack = _ }
         , f sl.decay sl { decay = _ }
         , f sl.sustain sl { sustain = _ }
         , f sl.release sl { release = _ }
         , f sl.depth sl { depth = _ }
         , f sl.velDepth sl { velDepth = _ }
         , int 0 7 sl.timeRange orange (put M.GEnv xs <<< sl { timeRange = _ })
         ]
  M.GLfo xs -> xs !! j <#> \sl ->
    let p = put M.GLfo xs
    in place [ 5, 6, 9, 10 ]
         [ lfoRate p sl
         , phase p sl
         , int 0 5 (ops.lfoShape sl) teal \k -> p (ops.setLfoShape k (max 0.1 (ops.lfoAmp sl)) sl)
         , amp ops p sl
         ]
  M.GLfoT xs -> xs !! j <#> \sl ->
    let p = put M.GLfoT xs
    in place [ 5, 6, 9, 10 ]
         [ lfoTSpeed p sl
         , phase p sl
         , int 0 5 (ops.lfoShape sl) teal \k -> p (ops.setLfoShape k (max 0.1 (ops.lfoAmp sl)) sl)
         , amp ops p sl
         ]
  M.GEuclid xs -> xs !! j <#> \sl ->
    let p = put M.GEuclid xs
    in place [ 0, 4, 5, 8 ]
         [ int 0 sl.steps sl.beats green (p <<< sl { beats = _ })
         , int 1 M.maxEuclidSteps sl.steps green \n -> p (sl { steps = n, beats = min sl.beats n, rotation = sl.rotation `mod` n })
         , int 0 (max 0 (sl.steps - 1)) sl.rotation green (p <<< sl { rotation = _ })
         , int 1 16 sl.rate green (p <<< sl { rate = _ })
         ]
  M.GClock xs -> xs !! j <#> \sl ->
    let p = put M.GClock xs
    in place [ 0, 3 ]
         [ clockSpeed ops p sl
         , int 1 99 sl.pulseWidth yellow (p <<< sl { pulseWidth = _ })
         ]
  -- a voice: its channel, and its envelope's when the layout has one
  M.GVoice vb -> vb.voices !! j <#> \v ->
    let pv = put (\vs -> M.GVoice vb { voices = vs }) vb.voices
        f x set = int 0 127 x orange (\n -> pv (v { env = set n }))
    in if elem M.VEnvelope vb.outputs
      then place [ 0, 4, 5, 6, 7 ]
             [ int 1 16 v.channel red (\c -> pv (v { channel = c }))
             , f v.env.attack (\n -> v.env { attack = n })
             , f v.env.decay (\n -> v.env { decay = n })
             , f v.env.sustain (\n -> v.env { sustain = n })
             , f v.env.release (\n -> v.env { release = n })
             ]
      else place [ 0 ] [ int 1 16 v.channel red (\c -> pv (v { channel = c })) ]
  M.GNote xs -> xs !! j <#> \sl ->
    let p = put M.GNote xs
        oct = sl.note / 12
        semi = sl.note `mod` 12
    in place [ 5, 9 ]
         [ int 0 10 oct violet \o -> p (sl { note = clamp 0 127 (o * 12 + semi) })
         , int 0 11 semi violet \s -> p (sl { note = clamp 0 127 (oct * 12 + s) })
         ]
  M.GThrow _ -> Nothing
  -- whole volts over hundredths, as a note's octave over its semitone, so a
  -- CV is dialled in coarse then fine
  M.GVolt xs -> xs !! j <#> \sl ->
    let
      p = put M.GVolt xs
      span = M.rangeVolts outRange
      cents = round (sl.volts * 100.0)
      whole = Int.floor (toNumber cents / 100.0)
      hundredths = cents - whole * 100
      lo = Int.floor span.lo
      hi = Int.floor span.hi
      at w h = p (sl { volts = toNumber (w * 100 + h) / 100.0 })
    in place [ 5, 9 ]
         [ int lo hi (clamp lo hi whole) pink \w -> at w hundredths
         , int 0 99 hundredths pink \h -> at whole h
         ]
  where
  put :: forall a. (Array a -> M.GenBank) -> Array a -> a -> Act
  put mk xs x = Edit d (const (mk (setAt j xs x)))

-- Controls ------------------------------------------------------------------

-- | An integer field over `lo..hi`, spread across the whole turn.
int :: Int -> Int -> Int -> Int -> (Int -> Act) -> Control
int lo hi v hue set =
  { pos: posOf lo hi v
  , hue
  , means: \p -> scale lo hi p == v
  , turn: \p -> let v' = scale lo hi p in if v' == v then [] else [ set v' ]
  , press: []
  }

-- | A choice among `0..hi`, for navigation.
stepped :: Int -> Int -> Int -> (Int -> Act) -> Control
stepped hi = int 0 (max 0 hi)

-- | A push-only control; turns do nothing.
button :: Int -> Int -> Act -> Control
button hue pos act = { pos, hue, means: const true, turn: const [], press: [ act ] }

-- | A continuous field through a mapping onto 0..1. A position means the
-- | value within a click either side, since the rack prints three decimals
-- | and the round trip can move it by one.
continuous :: (Number -> Number) -> (Number -> Number) -> Number -> Int -> (Number -> Act) -> Control
continuous to from v hue set =
  let here = clamp 0 127 (round (to v * 127.0))
  in { pos: here
     , hue
     , means: \p -> abs (p - here) <= 1
     , turn: \p -> if p == here then [] else [ set (from (toNumber p / 127.0)) ]
     , press: []
     }

-- | LFO rate, 0.01–50 Hz (the arrows' range) on a log scale, so a turn
-- | spends as long under 1 Hz as over it.
lfoRate :: (M.ModSlot -> Act) -> M.ModSlot -> Control
lfoRate p sl = continuous to from sl.rate teal \r -> p (sl { rate = r })
  where
  lo = 0.01
  hi = 50.0
  to r = (log (clamp lo hi r) - log lo) / (log hi - log lo)
  from u = exp (log lo + u * (log hi - log lo))

-- | A POLYLFOT slot's period, stepped along the ladder, slow to fast.
lfoTSpeed :: (M.ModSlot -> Act) -> M.ModSlot -> Control
lfoTSpeed p sl = int 0 (length M.lfoTLadder - 1) (M.lfoTRung sl) teal \i -> p (M.onLfoTRung i sl)

phase :: (M.ModSlot -> Act) -> M.ModSlot -> Control
phase p sl = continuous identity (min 0.999) sl.phase teal \v -> p (sl { phase = v })

-- | The dominant shape's amplitude, 0–2, as the page's ↑/↓ moves it.
amp :: Ops -> (M.ModSlot -> Act) -> M.ModSlot -> Control
amp ops p sl = continuous (_ / 2.0) (_ * 2.0) (ops.lfoAmp sl) teal \a ->
  p (ops.setLfoShape (ops.lfoShape sl) a sl)

-- | A voltage across a range, to the hundredth of a volt.
volts :: M.OutputRange -> (M.VoltSlot -> Act) -> M.VoltSlot -> Control
volts outRange p sl = continuous to from sl.volts pink \v -> p (sl { volts = toNumber (round (v * 100.0)) / 100.0 })
  where
  span = M.rangeVolts outRange
  to v = (M.clampNum span.lo span.hi v - span.lo) / (span.hi - span.lo)
  from u = span.lo + u * (span.hi - span.lo)

clockSpeed :: Ops -> (M.ClockSlot -> Act) -> M.ClockSlot -> Control
clockSpeed ops p sl = int 0 (ops.clockSpeeds - 1) (ops.clockSpeed sl) yellow \i -> p (ops.setClockSpeed i sl)

-- Painting ------------------------------------------------------------------

-- | What an encoder last showed: dark, or a ring and a colour.
type Look = Maybe { pos :: Int, hue :: Int }

data Write
  = Ring HT.Knob Int
  | Colour HT.Knob Int
  | Light HT.Knob Boolean

-- | The writes this frame needs, and the looks they leave, keyed by encoder
-- | index. `heard` is the device's last reported position, `lit` what was
-- | last written; an encoder with no entry in `lit` is unknown, and written.
paint
  :: Array (Maybe Control)
  -> { heard :: Map Int Int, lit :: Map Int Look }
  -> { writes :: Array Write, lit :: Map Int Look }
paint controls st =
  { writes: concatMap _.writes changed
  , lit: Map.union (Map.fromFoldable (map (\c -> Tuple c.i c.look) changed)) st.lit
  }
  where
  changed = catMaybes (mapWithIndex one controls)
  one i c =
    let k = { layer, index: i }
        want = c <#> \ctl ->
          { pos: case Map.lookup i st.heard of
              Just p | ctl.means p -> p
              _ -> ctl.pos
          , hue: ctl.hue
          }
        before = Map.lookup i st.lit
    in if before == Just want then Nothing
       else Just { i, look: want, writes: diff k before want }
  diff k before now = case before, now of
    -- Ring and colour to zero as well, so a dark encoder still reads as
    -- unused if `Helm.Twister.ringBrightness` turns out to do nothing.
    _, Nothing -> [ Light k false, Ring k 0, Colour k 0 ]
    Just (Just w), Just n ->
      (if w.pos /= n.pos then [ Ring k n.pos ] else [])
        <> (if w.hue /= n.hue then [ Colour k n.hue ] else [])
    _, Just n -> [ Light k true, Ring k n.pos, Colour k n.hue ]

-- Helpers -------------------------------------------------------------------

setAt :: forall a. Int -> Array a -> a -> Array a
setAt j xs x = mapWithIndex (\i y -> if i == j then x else y) xs

dark :: forall a. Int -> Array (Maybe a)
dark n = replicate n Nothing

-- | Controls at the given top-twelve positions, the rest dark.
place :: Array Int -> Array Control -> Array (Maybe Control)
place at cs = map (\i -> elemIndex i at >>= (cs !! _)) (range 0 11)

posOf :: Int -> Int -> Int -> Int
posOf lo hi v
  | hi <= lo = 0
  | otherwise = clamp 0 127 (round (toNumber (v - lo) / toNumber (hi - lo) * 127.0))

scale :: Int -> Int -> Int -> Int
scale lo hi p = lo + round (toNumber p / 127.0 * toNumber (hi - lo))
