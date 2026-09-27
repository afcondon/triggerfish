#!/bin/sh
# Bundle reef's notation (and the codec it needs) for conspicillum.html, and
# cycleOf for module.html.
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
export const isRight = (e) => e instanceof Right;
export const isNothing = (m) => m instanceof Nothing;
JS
esbuild "$dir/entry.js" --bundle --format=esm --minify --outfile="$here/../public/notation.js" --log-level=warning
rm -rf "$dir"
