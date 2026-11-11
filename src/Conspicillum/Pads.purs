-- | **The step gate on the Midifighter 3D's pads.**
-- |
-- | Sixteen arcade pads, one a step of Sector's table (`Reef.Conspicillum.Cloud.Steps`):
-- | a press lights its step, so that step may jump, and a second press puts
-- | it out. A side button lets the gate go, and the table jumps as written
-- | again. Latched rather than held, because the engine fixes a whole bar
-- | ahead of time: a pad held down would count only at the moment the next
-- | bar is worked out, which is not a moment a hand can feel.
-- |
-- | Pad `k` is step `k`, counted as the device counts: bottom-left first,
-- | rows upward. A grid of 8 uses the bottom two rows. Bank 1 only.
-- |
-- | **What the device was measured to do** (`docs/kb/plans/helm.md`,
-- | 2026-11-07 and 2026-11-09): a pad sends a note on channel 3, 36
-- | bottom-left to 51 top-right on bank 1; the side buttons send notes 20–25
-- | on channel 4. A note-on sent back on channel 3 at a pad's note sets its
-- | light, velocity the colour: 0 off, 13–120 a hue wheel from red to pink.
-- |
-- | A lit pad wears the colour of its slice on the ring, so the pad and the
-- | ring always agree about which part of the bar is which.
-- |
-- | **On a Sampler preset the pads are the sampler instead** (`Conspicillum.Take`):
-- | pad `k` strikes sample `k` now, and the side buttons record, clear and
-- | lengthen a loop of those hits. Which a press means is the page's choice,
-- | so a press here is only which control it was.
module Conspicillum.Pads
  ( Press(..)
  , parse
  , toggle
  , lights
  , samplerLights
  , paintSampler
  , velocityOfHue
  , open
  , paint
  ) where

import Prelude

import Data.Array (index, length, range, replicate, updateAt)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Helm.WebMIDI (MIDIOutput, PortMatch(..))
import Helm.WebMIDI as Midi

-- | A pad (0 to 15), or a side button by its note (20 to 25).
data Press
  = Pad Int
  | Side Int

derive instance eqPress :: Eq Press

-- | The 3D's port, as the hub names it.
device :: String
device = "Midi Fighter 3D"

firstPad :: Int
firstPad = 36

pads :: Int
pads = 16

-- | A message from the 3D, if it is a press this page answers. The pads'
-- | releases and their channel-4 echoes are ignored.
parse :: Array Int -> Maybe Press
parse = case _ of
  [ 0x92, note, velocity ] | velocity > 0 && note >= firstPad && note < firstPad + pads -> Just (Pad (note - firstPad))
  [ 0x93, note, velocity ] | velocity > 0 && note >= 20 && note <= 25 -> Just (Side note)
  _ -> Nothing

-- | Light or put out step `k`'s pad. A gate is made, all out, by the first
-- | press, and is re-made at the grid's size if the preset's grid changed.
-- | A pad beyond the grid does nothing.
toggle :: Int -> Int -> Array Boolean -> Array Boolean
toggle grid k gate
  | k < 0 || k >= grid = gate
  | otherwise =
      let
        sized = if length gate == grid then gate else replicate grid false
      in
        fromMaybe sized (updateAt k (not (fromMaybe false (index sized k))) sized)

-- | The note-on to send each pad: the slice's colour when lit, off otherwise.
-- | The ring's hue for a one-bar tape runs from 0° to 300° along the bar
-- | (`Reef.Conspicillum.Display.colourAt`), and step `k` starts at `k/grid` of it.
lights :: Int -> Array Boolean -> Array (Array Int)
lights grid gate = range 0 (pads - 1) <#> \k ->
  let
    lit = k < grid && fromMaybe false (index gate k)
    hue = toNumber k / toNumber (max 1 grid) * 300.0
  in
    [ 0x92, firstPad + k, if lit then velocityOfHue hue else 0 ]

-- | The sampler's pads: each in its own colour along the hue wheel, so a
-- | sample is found by colour; all red while recording.
samplerLights :: Boolean -> Array (Array Int)
samplerLights recording = range 0 (pads - 1) <#> \k ->
  [ 0x92, firstPad + k, if recording then velocityOfHue 0.0 else velocityOfHue (toNumber k / toNumber pads * 300.0) ]

-- | A hue in degrees as the 3D's velocity, by the colours AC read off the
-- | pads on 2026-11-09. Between them, straight lines: right about which
-- | colour is which, approximate about the shade.
velocityOfHue :: Number -> Int
velocityOfHue hue = round (between table)
  where
  table =
    [ { h: 0.0, v: 16.0 }, { h: 30.0, v: 28.0 }, { h: 60.0, v: 40.0 }, { h: 120.0, v: 64.0 }
    , { h: 180.0, v: 80.0 }, { h: 240.0, v: 90.0 }, { h: 270.0, v: 104.0 }, { h: 300.0, v: 114.0 }
    , { h: 330.0, v: 120.0 }
    ]
  h = max 0.0 (min 330.0 hue)
  between t = fromMaybe 120.0 do
    i <- firstAbove t 0
    a <- index t (i - 1)
    b <- index t i
    pure (a.v + (b.v - a.v) * (h - a.h) / (b.h - a.h))
  firstAbove t i = case index t i of
    Nothing -> Nothing
    Just e -> if e.h >= h && i > 0 then Just i else firstAbove t (i + 1)

-- | Reach the 3D through Helm's hub: its presses to `onPress`, and its output
-- | (to light the pads) to `onReady`, claimed so no other page paints over
-- | it. If the hub or the device is missing, `onReady` gets `Nothing` and the
-- | page carries on without pads.
open :: { onPress :: Press -> Effect Unit, onReady :: Maybe MIDIOutput -> Effect Unit } -> Effect Unit
open { onPress, onReady } =
  Midi.requestHubAccess { url: "ws://127.0.0.1:3039", client: "conspicillum", waitMs: 3000.0 } case _ of
    Left _ -> onReady Nothing
    Right access -> do
      minput <- Midi.findInput access (Exactly device)
      for_ minput \input -> void (Midi.onMessage input \m -> for_ (parse m) onPress)
      moutput <- Midi.findOutput access (Exactly device)
      for_ moutput (Midi.claimOutput access)
      onReady moutput

-- | Light the pads for a gate.
paint :: MIDIOutput -> Int -> Array Boolean -> Effect Unit
paint output grid gate = for_ (lights grid gate) (Midi.send output)

-- | Light the pads as the sampler.
paintSampler :: MIDIOutput -> Boolean -> Effect Unit
paintSampler output recording = for_ (samplerLights recording) (Midi.send output)
