-- | **The browser's own voice**: an FM electric piano in Web Audio, so Vetula
-- | sounds with nothing installed (the zero-install taster) and wherever no
-- | MIDI port answers. AC, 2026-10-06: "electric piano alone might cover us for
-- | MVP in the browser". A strings pad can join it later as a second voice.
-- |
-- | The audio context is made on the first note, which is always in answer to
-- | a click or a key, as browsers require.
module Vetula.Voice (Note, play) where

import Prelude

import Effect (Effect)

-- | One note: MIDI number, velocity (0–127), when (ms from now) and how long.
type Note = { note :: Int, velocity :: Int, delayMs :: Number, durMs :: Number }

play :: Note -> Effect Unit
play n = playNote_ n.note n.velocity n.delayMs n.durMs

foreign import playNote_ :: Int -> Int -> Number -> Number -> Effect Unit
