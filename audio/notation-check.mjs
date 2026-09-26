// Every page preset through the notation and back: preset -> scene -> line
// -> scene, compared on the wire. Prints each preset as its line.
//   node notation-check.mjs          all, quietly; exit 1 on any mismatch
//   node notation-check.mjs -v       and print every line
import { presets, fromPreset } from "./scenes.mjs";
const R = "../../../reef/output/";
const { decodeScene, encodeScene } = await import(R + "Reef.Conspicillum.Protocol/index.js");
const { parse, print } = await import(R + "Reef.Conspicillum.Notation/index.js");
const { Just, Nothing } = await import(R + "Data.Maybe/index.js");
const WARP = { repitch: 0, gap: 1, leak: 2 };
// A sequence's own amount is never read (setAmount replaces it).
const norm = (json) => { const s = JSON.parse(json);
  s.spec.rules.forEach(r => { if (r.values.length) r.amount = 0; }); return JSON.stringify(s); };
let bad = 0;
for (const P of presets()) {
  const sc = fromPreset(P);
  sc.spec.warp = { ratio: 1, mode: WARP[P.warp || "gap"] };
  const d = decodeScene(JSON.stringify(sc));
  if (!d.value0 || d.constructor.name !== "Right") { console.log("DECODE", P.name); bad++; continue; }
  const scene = d.value0;
  const line = { set: P.set, n: P.whole ? Nothing.value : Just.create(P.n), seed: scene.seed, spec: scene.spec };
  const text = print(line);
  const back = parse(text);
  if (back.constructor.name !== "Right") { console.log("PARSE", P.name, back.value0, "\n  ", text); bad++; continue; }
  const l2 = back.value0;
  const same = norm(encodeScene({ ...scene, spec: l2.spec, seed: l2.seed })) === norm(encodeScene(scene))
    && l2.set === P.set && (P.whole ? l2.n.constructor.name === "Nothing" : l2.n.value0 === P.n);
  if (!same) { bad++; console.log("DIFFERS", P.name, "\n  ", text); }
  else if (process.argv.includes("-v")) console.log(`${P.name.padEnd(28)} ${text}`);
}
console.log(`${presets().length - bad} of ${presets().length} presets round-trip`);
process.exit(bad ? 1 : 0);
