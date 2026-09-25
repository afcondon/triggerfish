// Push a Conspicillum scene at purerl-tidal from the command line — no page.
//   node sector.mjs whole            the bar as one grain (the reference)
//   node sector.mjs tape             16 sixteenths, follow 1: should sound identical
//   node sector.mjs tape position=0.25 rules='[[16,5,"shift",-0.0625],["p",0.2,"speed",-1]]'
//   node sector.mjs stop
const { noFx, noChain } = await import(new URL("../../../reef/output/Reef.Conspicillum.Cloud/index.js", import.meta.url).href);
const [mode = "tape", ...kv] = process.argv.slice(2);
const o = Object.fromEntries(kv.map(s => { const [k, v] = s.split("="); return [k, JSON.parse(v)]; }));
const corpus = { name: "fd-beat-bar", samples: [{ index: 0, secs: 2.0, peak: 0.637933, rms: 0.077551,
  zcr: 4087.5, tilt: 0.202982, decay: 2.0, cell: [], params: [], notes: [] }] };
const count = o.count ?? 16;
const base = mode === "whole"
  ? { onsets: [0], cloud: { sustain: 2.0, position: 0, spray: 0, follow: 0 } }
  : { onsets: Array.from({ length: count }, (_, i) => i / count),
      cloud: { sustain: o.sustain ?? 2.0 / count, position: o.position ?? 0, spray: o.spray ?? 0, follow: o.follow ?? 1 } };
const OPS = ["speed","gain","length","pan","accelerate","shape","crush","coarse","lpf","hpf","bpf","res",
  "vowel","pshift","tremolo","phaser","genv","gtilt","gplat","atk","hold","rel","curve",
  "rsnpitch","rsndecay","rsnbright","rsnmix","rsnmodel","shift"];
// a rule is [n, k, op, amount] (every n from k) or ["p", chance, op, amount]
const rules = (o.rules ?? []).map(([a, b, op, amount]) => {
  const opi = typeof op === "string" ? OPS.indexOf(op) : op;
  return a === "p" ? { when: 2, everyN: 0, everyK: 0, chance: b, op: opi, amount }
                   : { when: 1, everyN: a, everyK: b, chance: 0, op: opi, amount };
});
const scene = { corpus, query: { clauses: [], weighting: [], harmonic: [] },
  spec: { ...base, rules, speed: o.speed ?? 1, gain: o.gain ?? 1, pan: 0.5, accelerate: 0, fx: noFx, chain: noChain },
  seed: o.seed ?? 1 };
const ws = new WebSocket("ws://localhost:3012/ws");
ws.onmessage = (e) => { if (!String(e.data).startsWith("anchor")) console.log("<", String(e.data).slice(0, 200)); };
ws.onopen = () => {
  ws.send(mode === "stop" ? "conspicillum-stop" : "conspicillum-scene " + JSON.stringify(scene));
  setTimeout(() => ws.close(), 600);
};
