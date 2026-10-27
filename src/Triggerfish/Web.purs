-- | **The web build** (AC, 2026-10-27: "anticipate and mitigate problems the
-- | web-user might encounter"). trigger-fish.app serves Vetula and Odonus to
-- | people with no Atlantis install, from the same bundles the rig serves; the
-- | page says which it is (`window.TRIGGERFISH_WEB`, written by `make static`).
-- | On the web a page reaches for nothing on the visitor's own machine
-- | unasked: no rig socket (which would retry forever, and which a browser
-- | may meet with a local-network prompt), no Amphora, and no Web MIDI until
-- | the visitor chooses to play through MIDI.
module Triggerfish.Web
  ( onTheWeb
  , rigUrl
  , midiChosen
  , chooseMidi
  ) where

import Prelude

import Effect (Effect)

-- | Whether this page is the web build.
foreign import onTheWeb :: Boolean

-- | The rig's socket, as this page should dial it: none on the web (an empty
-- | URL is a Binnacle that never dials).
rigUrl :: String -> String
rigUrl url = if onTheWeb then "" else url

-- | Whether the visitor chose to play through MIDI (kept in this browser).
foreign import midiChosen :: Effect Boolean

foreign import chooseMidi :: Boolean -> Effect Unit
