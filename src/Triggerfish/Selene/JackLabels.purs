-- | Triggerfish.Selene.JackLabels — a name written on a jack: "QD bd",
-- | "BIA trig", "Plaits V/oct" (docs/kb/plans/atlantis-crs.md, CR-6).
-- |
-- | A label belongs to the JACK, not to the bank on it: it says where the
-- | cable goes, and the cable stays when a Euclid on the jack becomes an LFO.
-- | So it is keyed by the jack (`fh2_1:5`), and outlives every bank.
-- |
-- | Free text, and possibly out of date. AC, 2026-10-22: a stale label beats
-- | no label when you are reading a patch. Keeping it true is the player's, as
-- | it is for tape on a panel; nothing here checks it.
-- |
-- | Kept in Amphora (`Triggerfish.Library`), so every page and browser sees
-- | the same names. The patch proper is Acropora's (`Selene.Patch.Types`,
-- | keyed by outlet); when a label can come from a module spec there, this is
-- | the field it fills.
module Triggerfish.Selene.JackLabels
  ( JackLabel
  , doc
  , jackKey
  , labelAt
  , setLabel
  , loadShown
  , saveShown
  , focusField
  ) where

import Prelude

import Data.Array (filter, find, snoc)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.String (trim)
import Effect (Effect)
import Simple.JSON (readJSON, writeJSON)
import Triggerfish.Library as Library
import Triggerfish.Selene.Model as M
import Web.HTML.HTMLElement (HTMLElement)

-- | One jack's label. `jack` is the bank's wire name and the jack's number
-- | on it, 1-based as the panel prints it: `fh2_1:5`, `es9gt0:3`, `midi1:2`.
type JackLabel = { id :: String, jack :: String, label :: String }

doc :: Library.Doc JackLabel
doc =
  { ref: "triggerfish/selene/jack-labels"
  , kind: "triggerfish-selene-jack-labels"
  , idOf: _.id
  , withId: \id l -> l { id = id }
  , encode: writeJSON
  , decode: hush <<< readJSON
  , same: \a b -> a.jack == b.jack
  , first: pure []
  , tidy: identity
  }

-- | A jack by its bank and its number on the panel (1-based).
jackKey :: M.Target -> Int -> String
jackKey target n = M.targetWire target <> ":" <> show n

labelAt :: Array JackLabel -> String -> String
labelAt labels jack = maybe "" _.label (find (\l -> l.jack == jack) labels)

-- | Name a jack; an empty name takes the label off.
setLabel :: String -> String -> Array JackLabel -> Array JackLabel
setLabel jack text labels = case find (\l -> l.jack == jack) labels of
  _ | name == "" -> filter (\l -> l.jack /= jack) labels
  Just _ -> map (\l -> if l.jack == jack then l { label = name } else l) labels
  Nothing -> snoc labels { id: "", jack, label: name }
  where
  name = trim text

-- | Whether this browser draws the labels: they cost a line under every
-- | output, so they are off until asked for. A viewer's preference, kept in
-- | this browser only.
foreign import loadShown :: Effect Boolean
foreign import saveShown :: Boolean -> Effect Unit

-- | Put the cursor in a label just opened, its text selected to retype.
foreign import focusField :: HTMLElement -> Effect Unit
