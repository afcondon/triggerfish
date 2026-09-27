// **The presets come from reef.** `Reef.Conspicillum.Presets` is the only copy
// (since 2026-09-27); the bundle carries them as plain wire data
// (`presetsOnWire`), and this turns each into the preset object the pages have
// always used: the shape `applyP` reads in the workshop, `wireOf` in the
// module, and `fromPreset` in audio/scenes.mjs.
//
// Pure: pass it the loaded notation bundle.

// Knobs travel by the catalogue's stable identifier; the pages still name
// controls by their element ids.
const KNOB_ID = {
  grainLength: "sustain", readPosition: "position", spray: "spray", tapeFollow: "follow",
  walkJump: "wjump", walkHold: "whold", walkHome: "whome", walkReach: "wreach",
  playSwing: "swplay", tapeSwing: "swtape", speed: "speed", gain: "gain", pan: "pan",
  accelerate: "accel", sendLevelA: "sendA", sendLevelB: "sendB",
};
// An effect knob is the effect's control id: grain.x is fx_<wire name>, and so
// on; none of the presets uses one yet, so they pass through by identifier.
const knobId = (identifier) => KNOB_ID[identifier] || identifier;
// Bank names back to the workshop's short tags, which its buttons show.
const TAG = { Early: "", Found: "found", Polychords: "poly", Sector: "sector", Fix: "fix", Dub: "dub",
  Swing: "swing", Harmony: "harm", Envelope: "env", Effects: "fx", Resonator: "rsn", Progression: "prog" };
const FOLLOW = ["off", "root", "bass", "tones"];

function onsetsOf(os) {
  const n = os.length;
  const near = (a, b) => Math.abs(a - b) < 1e-9;
  if (os.every((x, k) => near(x, k / n))) return { mode: "even", count: n };
  for (let steps = 1; steps <= 64; steps++) for (let k = 1; k <= steps; k++) {
    const e = [];
    for (let i = 0; i < steps; i++) if (Math.floor(i * k / steps) !== Math.floor((i - 1) * k / steps)) e.push(i / steps);
    if (e.length === n && e.every((x, j) => near(x, os[j]))) return { mode: "euclid", k, n: steps };
  }
  return { mode: "manual", manual: os.join(",") };
}

function chordNameOf(chords, c) {
  return Object.keys(chords || {}).find(k => chords[k].root === c.root && chords[k].bass === c.bass
    && chords[k].pcs.join() === c.pcs.join());
}

// `chords` is the page's own name -> {pcs, root, bass} table, to name a
// progression's chords; without it, a progression keeps its chord records.
export function presetsFromReef(N, chords) {
  return N.presetsOnWire.map(o => {
    const w = o.spec;
    const valtext = (r) => !r.values.length ? ""
      : r.step === 1 ? "<" + r.values.join(" ") + ">" : r.values.join(" ");
    const q = o.query;
    const prog = w.progression.chords.map(c => chordNameOf(chords, c) || c);
    return {
      name: o.name, tag: TAG[o.bank] ?? o.bank, bank: o.bank, blurb: o.about, line: o.line,
      set: o.set, n: o.whole ? 0 : o.sample, whole: o.whole, seed: o.seed,
      cloud: w.cloud, walk: w.walk, swing: w.swing, tape: w.tape,
      steps: w.steps.to.map((t, k) => (t < 0 ? "~" : String(t)) + ((w.steps.p[k] ?? 1) === 1 ? "" : "?" + w.steps.p[k])).join(" "),
      warp: ["repitch", "gap", "leak"][w.warp.mode] || "gap",
      on: onsetsOf(w.onsets),
      v: { speed: w.speed, gain: w.gain, pan: w.pan, accel: w.accelerate },
      rules: w.rules.map(r => ({ ...r, valtext: valtext(r) })),
      fx: w.fx, chain: w.chain,
      sends: { a: (w.sends[0] || {}).level ?? 0.8, b: (w.sends[1] || {}).level ?? 0.8 },
      q: {
        clauses: q.clauses,
        weighting: q.weighting[0],
        harmonic: prog.length ? { minFit: w.progression.minimumFit, strength: w.progression.strength } : undefined,
      },
      prog: prog.length ? prog : undefined,
      follow: prog.length ? FOLLOW[w.progression.follow] : undefined,
      knobs: o.knobs.map(k => [knobId(k.parameter), k.label]),
    };
  });
}
