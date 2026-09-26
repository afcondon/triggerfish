// Conspicillum scenes from the page's presets, for anything that is not the
// page: the CLI (sector.mjs), the notation round-trip, a future module UI.
// Presets are read out of conspicillum.html, so there is one copy.
import { readFileSync } from "node:fs";
const here = (p) => new URL(p, import.meta.url);
const { noFx, noChain } = await import(here("../../../reef/output/Reef.Conspicillum.Cloud/index.js").href);

export const OPS = ["speed","gain","length","pan","accelerate","shape","crush","coarse","lpf","hpf","bpf","res",
  "vowel","pshift","tremolo","phaser","genv","gtilt","gplat","atk","hold","rel","curve",
  "rsnpitch","rsndecay","rsnbright","rsnmix","rsnmodel","shift","ratchet","send"];

export const SETS = JSON.parse(readFileSync(here("../public/conspicillum-corpora.json"))).sets;
export const corpusOf = (name, whole, n) => {
  const set = SETS.find(s => s.name === name);
  if (!set) throw new Error(`no corpus ${name} — rebuild with audio/build-corpora.py`);
  return { name, samples: whole ? set.samples : set.samples.filter(s => s.index === n) };
};

export function presets() {
  const h = readFileSync(here("../public/conspicillum.html"), "utf8");
  const start = h.indexOf("const LC = ");
  const open = h.indexOf("const PRESETS = [", start);
  const close = h.indexOf("\n];", open);
  return new Function(h.slice(start, close + 3) + "\nreturn PRESETS;")();
}

export function onsetsOf(on) {
  if (on.mode === "even") return Array.from({ length: on.count }, (_, i) => i / on.count);
  if (on.mode === "euclid") {
    const out = [];
    for (let i = 0; i < on.n; i++)
      if (Math.floor(i * on.k / on.n) !== Math.floor((i - 1) * on.k / on.n)) out.push(i / on.n);
    return out;
  }
  return on.manual.split(",").map(s => parseFloat(s)).filter(x => !isNaN(x) && x >= 0 && x < 1)
    .sort((a, b) => a - b);
}

// A projected tape (project-tape.py) says how many bars it is.
export const tapeOf = (name) => (SETS.find(s => s.name === name) || {}).tape || {};

export const noWalk = { jump: 0, hold: 0, home: 0, grid: 16, reach: 0 };
// "~ ~ 5?0.4 ~ 2" -> Sector's per-step table; the token count is the grid.
export function stepsOf(text) {
  const toks = (text || "").trim().split(/\s+/).filter(Boolean);
  if (!toks.length) return { grid: 16, to: [], p: [] };
  return { grid: toks.length,
           to: toks.map(t => { const n = parseInt(t.split("?")[0]); return isNaN(n) ? -1 : n; }),
           p: toks.map(t => { const q = parseFloat(t.split("?")[1]); return isNaN(q) ? 1 : q; }) };
}
// Send A is orbit 10 (outputs 3/4), send B orbit 11 (5/6): dry chains, so the
// effect can be an Ableton return. See superdirt-daemon.scd, SUPERDIRT_OUTPUTS.
export const sends = (a = 0.8, b = 0.8) => [
  { chain: { ...noChain, orbit: 10 }, level: a },
  { chain: { ...noChain, orbit: 11 }, level: b }];
export const noQuery = { clauses: [], weighting: [], harmonic: [] };

export function fromPreset(P, seed = 1) {
  return {
    corpus: corpusOf(P.set, !!P.whole, P.n),
    query: { clauses: (P.q && P.q.clauses) || [], weighting: (P.q && P.q.weighting) ? [P.q.weighting] : [], harmonic: [] },
    spec: {
      onsets: onsetsOf(P.on),
      cloud: { follow: 0, ...P.cloud },
      walk: { ...noWalk, ...(P.walk || {}) },
      swing: { tape: 0.5, play: 0.5, grid: 16, ...(P.swing || {}) },
      tape: { bars: 1, order: [], samples: [], ...(P.tape || {}) },
      steps: stepsOf(P.steps),
      rules: (P.rules || []).map(r => ({ when: r.when, everyN: r.everyN, everyK: r.everyK,
        chance: r.chance, op: r.op, amount: r.amount, values: r.values || [], step: r.step || 0 })),
      speed: P.v.speed, gain: P.v.gain, pan: P.v.pan, accelerate: P.v.accel,
      fx: { ...noFx, ...(P.fx || {}) },
      chain: { ...noChain, ...(P.chain || {}) },
      sends: sends(P.sends && P.sends.a, P.sends && P.sends.b),
      warp: { ratio: 1, mode: 1 },
    },
    seed,
  };
}
