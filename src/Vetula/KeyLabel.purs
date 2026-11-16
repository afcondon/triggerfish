-- | A key as Vetula labels it ("C mixo. ♭6", "D major"), and the way back.
-- | A kept progression carries its key only as this label, so whatever reads
-- | the library (Melichthys, for its ground) reads the key through here.
module Vetula.KeyLabel (groupLabel, modeShort, parseKeyLabel) where

import Prelude

import Data.Array (find, range)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits as SCU
import Harmonia.Chord (Key, Mode(..))
import Vetula.Harmony (noteName)

-- | The key/scale a chord is gathered under — the bubblepack it joins.
groupLabel :: Key -> String
groupLabel key = noteName key.tonic <> " " <> modeShort key.mode

modeShort :: Mode -> String
modeShort = case _ of
  Ionian -> "major"
  Aeolian -> "minor"
  Dorian -> "dorian"
  Phrygian -> "phrygian"
  Lydian -> "lydian"
  Mixolydian -> "mixolydian"
  Locrian -> "locrian"
  HarmonicMinor -> "harm. minor"
  MelodicMinor -> "mel. minor"
  LocrianNat6 -> "locrian ♮6"
  IonianSharp5 -> "ionian ♯5"
  DorianSharp4 -> "dorian ♯4"
  PhrygianDominant -> "phryg. dom."
  LydianSharp2 -> "lydian ♯2"
  Ultralocrian -> "ultralocrian"
  DorianFlat2 -> "dorian ♭2"
  LydianAugmented -> "lydian aug."
  LydianDominant -> "lydian dom."
  MixolydianFlat6 -> "mixo. ♭6"
  LocrianNat2 -> "locrian ♮2"
  Altered -> "altered"
  Custom _ -> "custom"

-- | Every mode with a name of its own (all but `Custom`).
namedModes :: Array Mode
namedModes =
  [ Ionian, Aeolian, Dorian, Phrygian, Lydian, Mixolydian, Locrian, HarmonicMinor, MelodicMinor
  , LocrianNat6, IonianSharp5, DorianSharp4, PhrygianDominant, LydianSharp2, Ultralocrian
  , DorianFlat2, LydianAugmented, LydianDominant, MixolydianFlat6, LocrianNat2, Altered ]

-- | Inverse of `groupLabel`: parse a stored key label ("F# phryg. dom.", "C major")
-- | back into a Key. The note name is the first word; the rest is a `modeShort`
-- | value (which can itself contain spaces, so split at the FIRST space only). Used
-- | when loading a saved progression so its key becomes the live harmonic context.
parseKeyLabel :: String -> Maybe Key
parseKeyLabel lbl = case SCU.indexOf (Pattern " ") lbl of
  Nothing -> Nothing
  Just ix -> do
    let noteTok = SCU.take ix lbl
        modeTok = SCU.drop (ix + 1) lbl
    tonic <- find (\pc -> noteName pc == noteTok) (range 0 11)
    mode <- find (\m -> modeShort m == modeTok) namedModes
    pure { tonic, mode }
