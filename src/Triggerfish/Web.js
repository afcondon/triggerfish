// Triggerfish.Web — whether this page is the web build (trigger-fish.app).
//
// Declared, not inferred: `make static` writes `window.TRIGGERFISH_WEB = true`
// into the page's head, before the bundle, and nothing else sets it. The same
// bundle runs on the rig (localhost:3023), where the flag is absent.
export const onTheWeb =
  typeof window !== "undefined" && window.TRIGGERFISH_WEB === true;

// The web visitor's choice of MIDI (Odonus's readout), kept in this browser.
const MIDI_KEY = "triggerfish.web.midi";

export const midiChosen = () => {
  try { return window.localStorage.getItem(MIDI_KEY) === "on"; } catch (_) { return false; }
};

export const chooseMidi = (on) => () => {
  try { window.localStorage.setItem(MIDI_KEY, on ? "on" : "off"); } catch (_) { /* private mode */ }
};
