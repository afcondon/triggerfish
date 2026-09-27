// One-shot: write reef's Reef.Conspicillum.Presets from the workshop's preset
// literals in conspicillum.html. After it has run, the PureScript is the source
// of truth and the pages read presets from reef; this stays only as the record
// of how the first copy was made.
//
//   node audio/generate-presets.mjs
//
// Each scene becomes its canonical line, printed by reef itself (through the
// notation bundle), so the text is exactly what `parse` reads back. The seed
// is the workshop's default, 12345, which every preset was auditioned with.
import { readFileSync, writeFileSync } from "node:fs";
import { presets, fromPreset, SETS } from "./scenes.mjs";

const here = (p) => new URL(p, import.meta.url);
const N = await import(here("../public/notation.js").href);
const OUT = here("../../../reef/src/Reef/Conspicillum/Presets.purs");

const html = readFileSync(here("../public/conspicillum.html"), "utf8");
const at = html.indexOf("const DEFAULT_KNOBS = {");
const DEFAULT_KNOBS = new Function(html.slice(at, html.indexOf("\n};", at) + 3) + "\nreturn DEFAULT_KNOBS;")();

const BANK = { "": "Early", found: "Found", poly: "Polychords", sector: "Sector", fix: "Fix", dub: "Dub",
  swing: "Swung", harm: "Harmony", env: "Envelopes", fx: "Effects", rsn: "Resonators", prog: "Progressions" };
const KNOB = { sustain: "GrainLength", position: "ReadPosition", spray: "Spray", follow: "TapeFollow",
  wjump: "WalkJump", whold: "WalkHold", whome: "WalkHome", wreach: "WalkReach",
  swplay: "PlaySwing", swtape: "TapeSwing", speed: "Speed", gain: "Gain", pan: "Pan", accel: "Accelerate",
  sendA: "(SendLevel SendA)", sendB: "(SendLevel SendB)" };
const FOLLOW = { root: "FollowRoot", bass: "FollowBass", tones: "FollowChordTones" };
const AXIS = ["APeak", "ARms", "AZcr", "ATilt", "ADecay", "ASecs"];
const CMP = ["Lt", "Lte", "Gt", "Gte"];

const str = (s) => JSON.stringify(s);
const num = (x) => { const t = Number.isInteger(x) ? x.toFixed(1) : String(x); return x < 0 ? `(${t})` : t; };
const axis = (a) => a.kind === 6 ? `(ACell ${a.at})` : a.kind === 7 ? `(AParam ${str(a.param)})` : AXIS[a.kind];

function lineOf(P) {
  const d = N.decodeScene(JSON.stringify(fromPreset(P, 12345)));
  if (!N.isRight(d)) throw new Error(`${P.name}: ${d.value0}`);
  const n = P.whole ? N.Nothing.value : N.Just.create(P.n);
  return N.print({ set: P.set, n, seed: d.value0.seed, spec: d.value0.spec });
}

// The knobs the workshop would draw: the preset's own, else its bank's, else
// what the module falls back to — a followed tape gets its rate and walk, a
// cloud its grain and position.
function knobsOf(P) {
  const set = SETS.find(s => s.name === P.set);
  const tapeish = (set && set.tape) || (P.cloud.follow ?? 0) !== 0;
  const fallback = tapeish
    ? [["follow", "tape rate"], ["wjump", "chaos"], ["whold", "repeat"], ["sendB", "space"]]
    : [["sustain", "grain"], ["position", "position"], ["spray", "spray"], ["sendB", "space"]];
  const ks = (P.knobs || DEFAULT_KNOBS[P.tag] || fallback).map(k => Array.isArray(k) ? k : [k.id, k.label]);
  return ks.map(([id, label]) => {
    if (!KNOB[id]) throw new Error(`${P.name}: no parameter for knob ${id}`);
    return `knob ${KNOB[id]} ${str(label)}`;
  });
}

function queryOf(P) {
  const q = P.q || {};
  const clauses = (q.clauses || []).map(c => `{ axis: ${axis(c.axis)}, cmp: ${CMP[c.cmp]}, value: ${num(c.value)} }`);
  const w = q.weighting;
  const weighting = w ? `Just { axis: ${axis(w.axis)}, toward: ${w.toward === 1 ? "Low" : "High"}, strength: ${num(w.strength)} }` : "Nothing";
  if (!clauses.length && !w) return "emptyQuery";
  return `emptyQuery { clauses = ${clauses.length ? `[ ${clauses.join(", ")} ]` : "[]"}, weighting = ${weighting} }`;
}

function progressionOf(P) {
  if (!P.prog) return "Nothing";
  const h = (P.q && P.q.harmonic) || { minFit: 0.5, strength: 0.9 };
  return `Just\n        { chords: [ ${P.prog.map(str).join(", ")} ]\n        , minimumFit: ${num(h.minFit)}\n        , strength: ${num(h.strength)}\n`
    + `        , resonatorFollows: ${FOLLOW[P.follow] || "NoFollow"}\n        }`;
}

const entries = presets().map(P => `{ name: ${str(P.name)}
    , bank: ${BANK[P.tag || ""]}
    , about: ${str(P.blurb || "")}
    , line: ${str(lineOf(P))}
    , query: ${queryOf(P)}
    , progression: ${progressionOf(P)}
    , knobs: [ ${knobsOf(P).join(", ")} ]
    }`);

const src = `-- | **The presets**, as data: ${entries.length} scenes, each its line, what the
-- | line cannot carry, and the knobs worth playing.
-- |
-- | First written on 2026-09-27 by triggerfish's \`audio/generate-presets.mjs\`
-- | from the workshop's preset literals, which were auditioned at the rig one by
-- | one. **This file is now the source of truth**: edit presets here, not in the
-- | workshop page. Every line is resolved on both runtimes by the conformance
-- | suite (\`conspicillumPresetRun\`).
module Reef.Conspicillum.Presets
  ( presetSources
  , presets
  ) where

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Reef.Conspicillum.Corpus (Axis(..), Cmp(..), Toward(..), emptyQuery)
import Reef.Conspicillum.Parameter (Parameter(..), SendBus(..))
import Reef.Conspicillum.Preset (Bank(..), Preset, PresetSource, ResonatorFollow(..), knob, resolve)

-- | Every preset resolved, or the first that will not: never a silent drop.
presets :: Either String (Array Preset)
presets = traverse resolve presetSources

presetSources :: Array PresetSource
presetSources =
  [ ${entries.join("\n  , ")}
  ]
`;
writeFileSync(OUT, src);
console.log(`wrote ${entries.length} presets to ${OUT.pathname}`);
