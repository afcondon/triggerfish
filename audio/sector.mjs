// Push a Conspicillum scene at purerl-tidal from the command line — no page.
//
//   node sector.mjs whole                 fd-beat-bar as one grain (the reference)
//   node sector.mjs tape                  16 sixteenths that follow the tape: identical
//   node sector.mjs tape jump=0.2 grid=8 reach=3 hold=0.1 home=0.1
//   node sector.mjs tape position=0.25 rules='[[16,5,"shift",-0.0625],["p",0.2,"speed",-1]]'
//   node sector.mjs list                  the page's presets
//   node sector.mjs preset Breakdown      a page preset, exactly as its button pushes it
//   node sector.mjs stop
//
// A rule is [n, k, op, amount] (every n-th grain from k) or ["p", chance, op,
// amount]. Presets are read out of conspicillum.html, so there is one copy.
import { readFileSync } from "node:fs";
const here = (p) => new URL(p, import.meta.url);
const { noFx, noChain } = await import(here("../../../reef/output/Reef.Conspicillum.Cloud/index.js").href);

const OPS = ["speed","gain","length","pan","accelerate","shape","crush","coarse","lpf","hpf","bpf","res",
  "vowel","pshift","tremolo","phaser","genv","gtilt","gplat","atk","hold","rel","curve",
  "rsnpitch","rsndecay","rsnbright","rsnmix","rsnmodel","shift","ratchet"];

const [mode = "tape", ...kv] = process.argv.slice(2);
const o = Object.fromEntries(kv.filter(s => s.includes("=")).map(s => {
  const [k, v] = s.split("="); return [k, JSON.parse(v)];
}));

const SETS = JSON.parse(readFileSync(here("../public/conspicillum-corpora.json"))).sets;
const corpusOf = (name, whole, n) => {
  const set = SETS.find(s => s.name === name);
  if (!set) throw new Error(`no corpus ${name} — rebuild with audio/build-corpora.py`);
  return { name, samples: whole ? set.samples : set.samples.filter(s => s.index === n) };
};

function presets() {
  const h = readFileSync(here("../public/conspicillum.html"), "utf8");
  const start = h.indexOf("const LC = ");
  const open = h.indexOf("const PRESETS = [", start);
  const close = h.indexOf("\n];", open);
  return new Function(h.slice(start, close + 3) + "\nreturn PRESETS;")();
}

function onsetsOf(on) {
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

const noWalk = { jump: 0, hold: 0, home: 0, grid: 16, reach: 0 };
const noQuery = { clauses: [], weighting: [], harmonic: [] };

function fromPreset(P) {
  return {
    corpus: corpusOf(P.set, !!P.whole, P.n),
    query: { clauses: (P.q && P.q.clauses) || [], weighting: (P.q && P.q.weighting) ? [P.q.weighting] : [], harmonic: [] },
    spec: {
      onsets: onsetsOf(P.on),
      cloud: { follow: 0, ...P.cloud },
      walk: { ...noWalk, ...(P.walk || {}) },
      rules: P.rules || [],
      speed: P.v.speed, gain: P.v.gain, pan: P.v.pan, accelerate: P.v.accel,
      fx: { ...noFx, ...(P.fx || {}) },
      chain: { ...noChain, ...(P.chain || {}) },
    },
    seed: o.seed ?? 1,
  };
}

function tape() {
  const count = o.count ?? 16;
  const rules = (o.rules ?? []).map(([a, b, op, amount]) => {
    const opi = typeof op === "string" ? OPS.indexOf(op) : op;
    if (opi < 0) throw new Error(`unknown op ${op}`);
    return a === "p" ? { when: 2, everyN: 0, everyK: 0, chance: b, op: opi, amount }
                     : { when: 1, everyN: a, everyK: b, chance: 0, op: opi, amount };
  });
  const whole = mode === "whole";
  return {
    corpus: corpusOf(o.set ?? "fd-beat-bar", true, 0),
    query: noQuery,
    spec: {
      onsets: whole ? [0] : Array.from({ length: count }, (_, i) => i / count),
      cloud: whole ? { sustain: 2.0, position: 0, spray: 0, follow: 0 }
                   : { sustain: o.sustain ?? 2.0 / count, position: o.position ?? 0,
                       spray: o.spray ?? 0, follow: o.follow ?? 1 },
      walk: whole ? noWalk : { jump: o.jump ?? 0, hold: o.hold ?? 0, home: o.home ?? 0,
                               grid: o.grid ?? 16, reach: o.reach ?? 0 },
      rules,
      speed: o.speed ?? 1, gain: o.gain ?? 1, pan: 0.5, accelerate: 0,
      fx: noFx, chain: noChain,
    },
    seed: o.seed ?? 1,
  };
}

if (mode === "list") {
  for (const P of presets()) console.log(`${(P.tag || "").padEnd(7)} ${P.name}`);
  process.exit(0);
}

let frame;
if (mode === "stop") frame = "conspicillum-stop";
else if (mode === "preset") {
  const name = process.argv.slice(3).filter(s => !s.includes("=")).join(" ");
  const P = presets().find(p => p.name.toLowerCase() === name.toLowerCase());
  if (!P) { console.error(`no preset "${name}" — try: node sector.mjs list`); process.exit(1); }
  frame = "conspicillum-scene " + JSON.stringify(fromPreset(P));
} else frame = "conspicillum-scene " + JSON.stringify(tape());

const ws = new WebSocket("ws://localhost:3012/ws");
ws.onmessage = (e) => { if (!String(e.data).startsWith("anchor")) console.log("<", String(e.data).slice(0, 200)); };
ws.onopen = () => { ws.send(frame); setTimeout(() => ws.close(), 600); };
