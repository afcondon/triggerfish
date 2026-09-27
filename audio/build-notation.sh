#!/bin/sh
# Bundle reef's notation (and the codec it needs) for conspicillum.html, and
# cycleOf for module.html, and the presets for every page (reef-presets.js).
# Run after changing Reef.Conspicillum.Notation, with reef built (spago build).
# Minified, so class names are gone: the page asks isRight / isNothing,
# never constructor.name.
set -e
here=$(cd "$(dirname "$0")" && pwd)
reef=$(cd "$here/../../../reef" && pwd)
dir=$(mktemp -d "${TMPDIR:-/tmp}/notation.XXXXXX")
cat > "$dir/entry.js" <<JS
import { Right } from "$reef/output/Data.Either/index.js";
import { Nothing } from "$reef/output/Data.Maybe/index.js";
export { parse, print } from "$reef/output/Reef.Conspicillum.Notation/index.js";
export { decodeScene, encodeScene } from "$reef/output/Reef.Conspicillum.Protocol/index.js";
export { Just, Nothing } from "$reef/output/Data.Maybe/index.js";
// For module.html: the circle is drawn by the engine's own cycle, not a lookalike.
export { cycleOf, noFx, noChain } from "$reef/output/Reef.Conspicillum.Cloud/index.js";
// reef's model of the module, for the A/B against the JS prototype
// (audio/model-ab.mjs, and module.html?model=reef): what it draws and says.
export { materialOf, materialName, segments, locate, colourAt, colourCss, wedges, lanes } from "$reef/output/Reef.Conspicillum.Display/index.js";
export { sentences, plainText, ruleRows, Words, Value, SetName, Code, Aside } from "$reef/output/Reef.Conspicillum.Sentence/index.js";
export { describe, format, read, set, normalise, denormalise, identifier, fromIdentifier, notationTerm, allParameters, engaged, Exponential } from "$reef/output/Reef.Conspicillum.Parameter/index.js";
// The presets, from reef (the only copy): as plain wire data, for reef-presets.js.
import { presets as resolvedPresets } from "$reef/output/Reef.Conspicillum.Presets/index.js";
import { presetOnWire } from "$reef/output/Reef.Conspicillum.Preset/index.js";
export const presetsOnWire = (() => {
  if (!(resolvedPresets instanceof Right)) throw new Error("reef presets do not resolve: " + resolvedPresets.value0);
  return resolvedPresets.value0.map(presetOnWire);
})();
export const isRight = (e) => e instanceof Right;
export const isNothing = (m) => m instanceof Nothing;
JS
esbuild "$dir/entry.js" --bundle --format=esm --minify --outfile="$here/../public/notation.js" --log-level=warning
rm -rf "$dir"
