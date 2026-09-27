// **A/B: the module's JS prototype model against reef's typed one.**
//
//   node audio/model-ab.mjs            # summary, with the first differences
//   node audio/model-ab.mjs --all      # every difference
//
// Same presets, same cycles, same corpora, two models:
//   A  the prototype's own functions, lifted out of public/module.html and
//      run against a stub DOM (the preset literals, wireOf, materialOf,
//      drawCircle's wedges and lanes, sentenceHTML);
//   B  reef, through the notation bundle (presetsOnWire, cycleOf, Display,
//      Sentence).
// Rendering is not in it: both sides stop at numbers and text, so a
// difference is a difference in the model and nothing else.
import { readFileSync, writeFileSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const here = (p) => new URL(p, import.meta.url);
const PUBLIC = here("../public/").pathname;
const all = process.argv.includes("--all");
const CYCLES = [0, 1, 2, 3];

// ── B: reef ────────────────────────────────────────────────────────────────
const N = await import(here("../public/notation.js").href);
const SETS = JSON.parse(readFileSync(PUBLIC + "conspicillum-corpora.json", "utf8")).sets;

// ── A: the prototype, out of its page ──────────────────────────────────────
function loadPrototype(search = "") {
  const html = readFileSync(PUBLIC + "module.html", "utf8");
  let js = html.split('<script type="module">')[1].split("</script>")[0];
  js = js.replace('import * as N from "./notation.js";', `import * as N from ${JSON.stringify(PUBLIC + "notation.js")};`)
    .replace('import("./reef-presets.js")', `import(${JSON.stringify(PUBLIC + "reef-presets.js")})`);
  const stub = `
import { readFileSync } from "node:fs";
const mk = () => ({ innerHTML: "", textContent: "", value: "", style: {}, dataset: {},
  classList: { toggle() {}, add() {}, remove() {} }, addEventListener() {}, setAttribute() {},
  querySelectorAll() { return []; }, querySelector() { return mk(); }, appendChild() {}, remove() {}, focus() {}, blur() {} });
const els = {};
globalThis.document = { getElementById: (id) => els[id] || (els[id] = mk()), createElementNS: () => mk(),
  querySelectorAll: () => [], addEventListener() {}, activeElement: null };
globalThis.location = { hostname: "localhost", search: ${JSON.stringify(search)} };
globalThis.WebSocket = class { send() {} };
globalThis.localStorage = { getItem() { return null; }, setItem() {} };
globalThis.requestAnimationFrame = () => {};
globalThis.fetch = async (f) => ({ text: async () => readFileSync(${JSON.stringify(PUBLIC)} + f, "utf8"),
  json: async () => JSON.parse(readFileSync(${JSON.stringify(PUBLIC)} + f, "utf8")) });
`;
  const tail = `
export const ready = new Promise(r => { const t = setInterval(() => { if (PRESETS.length && SETS.length && P) { clearInterval(t); r(); } }, 10); });
export const prototype = { recall, drawCircle, sentenceHTML, reefSentenceHTML, ruleRow,
  state: () => ({ P, PRESETS, MAT, GR, KNOBS, wire, sceneErr, REEF }) };
`;
  const dir = mkdtempSync(join(tmpdir(), "model-ab-"));
  const file = join(dir, "prototype.mjs");
  writeFileSync(file, stub + js + tail);
  return import(file);
}
const A = await loadPrototype();
await A.ready;
const proto = A.prototype;

// ── helpers ────────────────────────────────────────────────────────────────
const close = (a, b, eps = 1e-9) => Math.abs(a - b) <= eps;
const lineOfWire = (wire, set, whole, n) => {
  const d = N.decodeScene(JSON.stringify(wire));
  if (!N.isRight(d)) return "UNDECODABLE: " + d.value0;
  return N.print({ set, n: whole ? N.Nothing.value : N.Just.create(n), seed: d.value0.seed, spec: d.value0.spec });
};
const text = (html) => html
  .replace(/<sup>.*?<\/sup>/g, "")          // the knob numbers, which reef leaves to its renderer
  .replace(/<table[\s\S]*?<\/table>/g, "")  // rules are compared as rows, below
  .replace(/<\/p><p>/g, " ").replace(/<[^>]+>/g, "")
  .replace(/&amp;/g, "&").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"')
  .replace(/\s+/g, " ").trim();
// Word-level diff (LCS), returning the replaced stretches as [a, b] pairs:
// "lpf 800 Hz" against "low-pass 800 Hz" is one pair, ["lpf", "low-pass"].
function substitutions(a, b) {
  const x = a.split(" "), y = b.split(" ");
  const L = Array.from({ length: x.length + 1 }, () => new Array(y.length + 1).fill(0));
  for (let i = x.length - 1; i >= 0; i--) for (let j = y.length - 1; j >= 0; j--)
    L[i][j] = x[i] === y[j] ? L[i + 1][j + 1] + 1 : Math.max(L[i + 1][j], L[i][j + 1]);
  const out = []; let i = 0, j = 0, da = [], db = [];
  const flush = () => { if (da.length || db.length) out.push([da.join(" "), db.join(" ")]); da = []; db = []; };
  while (i < x.length || j < y.length) {
    if (i < x.length && j < y.length && x[i] === y[j]) { flush(); i++; j++; }
    else if (j < y.length && (i >= x.length || L[i][j + 1] >= L[i + 1][j])) db.push(y[j++]);
    else da.push(x[i++]);
  }
  flush();
  return out;
}
// Numbers inside a substitution are the same number read differently, so a
// pair is tallied by its shape: "dly time 0.310" -> "delay time 310 ms" is
// "dly time # -> delay time # ms".
const shape = (t) => t.replace(/-?\d+(\.\d+)?/g, "#");
const phrases = {};   // check -> Map(shape pair -> { count, example })
function tallyPhrases(check, a, b) {
  const m = phrases[check] || (phrases[check] = new Map());
  for (const [pa, pb] of substitutions(a, b)) {
    const key = shape(pa) + "  →  " + shape(pb);
    const e = m.get(key) || { count: 0, example: `${pa}  →  ${pb}` };
    e.count++; m.set(key, e);
  }
}

const cellsOf = (rowHtml) => [...rowHtml.matchAll(/<td[^>]*>(.*?)<\/td>/g)].map(m => text(m[1]));

// ── the comparison ─────────────────────────────────────────────────────────
const checks = {};   // name -> { same, total, diffs: [] }
const tally = (name, same, detail) => {
  const c = checks[name] || (checks[name] = { same: 0, total: 0, diffs: [] });
  c.total++;
  if (same) c.same++; else c.diffs.push(detail);
};

const B = N.presetsOnWire;
const protoPresets = proto.state().PRESETS;

B.forEach((o, i) => {
  const name = o.name;
  proto.recall(i);
  const s = proto.state();
  tally("preset order", s.P.name === name, `${i}: ${s.P.name} vs ${name}`);

  // The scene each side would send.
  tally("scene", lineOfWire(s.wire, s.P.set, s.P.whole, s.P.n) === o.line,
    `${name}\n      A ${lineOfWire(s.wire, s.P.set, s.P.whole, s.P.n)}\n      B ${o.line}`);

  // reef's view of the same preset.
  const set = SETS.find(x => x.name === o.set);
  const line = N.parse(o.line).value0;
  const material = N.materialOf({
    line,
    samples: set.samples.map(x => ({ index: x.index, seconds: x.secs })),
    tape: set.tape ? N.Just.create({ bars: set.tape.bars, bpm: set.tape.bpm }) : N.Nothing.value,
  });
  const segs = N.segments(material);
  const knobs = o.knobs.map(k => N.fromIdentifier(k.parameter).value0);

  // What it is made of.
  tally("material name", s.MAT.name === N.materialName(material), `${name}: A "${s.MAT.name}" B "${N.materialName(material)}"`);
  tally("segments", s.MAT.segs.length === segs.length
      && s.MAT.segs.every((g, k) => close(g.x0, segs[k].from) && close(g.x1, segs[k].to)),
    `${name}: A ${s.MAT.segs.length} [${s.MAT.segs.slice(0, 3).map(g => g.x0.toFixed(4)).join(" ")}…] B ${segs.length} [${segs.slice(0, 3).map(g => g.from.toFixed(4)).join(" ")}…]`);

  // What it says.
  const sa = text(proto.sentenceHTML(s.P));
  const sb = N.plainText(line.spec)(N.sentences({ line, material, knobs }));
  tally("sentence", sa === sb, `${name}\n      A ${sa}\n      B ${sb}`);
  if (sa !== sb) tallyPhrases("sentence", sa, sb);
  const ra = (s.P.rules || []).map(r => cellsOf(proto.ruleRow(r)).join(" / "));
  const rb = N.ruleRows(line.spec.rules).map(r => [r.which, r.what, r.amount].join(" / "));
  tally("rule rows", JSON.stringify(ra) === JSON.stringify(rb), `${name}\n      A ${ra.join(" | ")}\n      B ${rb.join(" | ")}`);
  if (JSON.stringify(ra) !== JSON.stringify(rb)) tallyPhrases("rule rows", ra.join(" | "), rb.join(" | "));

  // What it draws, cycle by cycle.
  const d = N.decodeScene(JSON.stringify({
    corpus: { name: o.set, samples: o.whole ? set.samples : set.samples.filter(x => x.index === o.sample) },
    query: o.query, spec: o.spec, seed: o.seed })).value0;
  for (const c of CYCLES) {
    proto.drawCircle(c);
    const GR = proto.state().GR;
    const emits = N.cycleOf(d.corpus)(d.query)(d.spec)(d.seed)(c);
    const ws = N.wedges({ cycleSeconds: 2, orbit: (o.spec.chain && o.spec.chain.orbit) || 0 })(emits);
    const spans = ws.map(w => ({ from: N.locate(material)(w.emit.n)(w.emit.begin).along, to: N.locate(material)(w.emit.n)(w.emit.end).along }));
    const ls = N.lanes(2 / 784)(spans);
    tally("grains", GR.length === ws.length, `${name} c${c}: A ${GR.length} B ${ws.length}`);
    const n = Math.min(GR.length, ws.length);
    for (let k = 0; k < n; k++) {
      const a = GR[k], w = ws[k], at = N.locate(material)(w.emit.n)(w.emit.begin);
      tally("grain emit", a.e.n === w.emit.n && close(a.e.at, w.emit.at) && close(a.e.begin, w.emit.begin) && close(a.e.end, w.emit.end),
        `${name} c${c} g${k}: A n${a.e.n}@${a.e.at} ${a.e.begin}-${a.e.end} B n${w.emit.n}@${w.emit.at} ${w.emit.begin}-${w.emit.end}`);
      tally("wedge", close(a.at, w.from) && close(a.u1, w.to), `${name} c${c} g${k}: A ${a.at}-${a.u1} B ${w.from}-${w.to}`);
      tally("read start", close(a.x0, at.along), `${name} c${c} g${k}: A ${a.x0} B ${at.along}`);
      // The end of a read that runs to the end of the material is clamped
      // inside it, and the two models clamp by different amounts (the
      // prototype 1e-5, reef 1e-9): the same place, told apart only there.
      const endGap = Math.abs(a.x1 - spans[k].to);
      tally("read end", endGap <= 1e-9 || (endGap <= 2e-5 && spans[k].to > 0.9999),
        `${name} c${c} g${k}: A ${a.x1} B ${spans[k].to}`);
      tally("colour", a.col === N.colourCss(N.colourAt(material)(at)), `${name} c${c} g${k}: A ${a.col} B ${N.colourCss(N.colourAt(material)(at))}`);
      tally("lane", a.lane === ls[k], `${name} c${c} g${k}: A ${a.lane} B ${ls[k]}`);
    }
  }
});

// ── the page with ?model=reef ──────────────────────────────────────────────
// The same page, switched to reef's model, must show exactly what reef says:
// its presets, its sentences and rule rows, its wedges, colours and lanes.
const R = await loadPrototype("?model=reef");
await R.ready;
const page = R.prototype;
tally("page(reef) switched", page.state().REEF === true, "the switch did not take");
B.forEach((o, i) => {
  const name = o.name;
  page.recall(i);
  const s = page.state();
  tally("page(reef) preset", s.P.name === name && s.P.line === o.line, `${i}: ${s.P.name} vs ${name}`);
  tally("page(reef) scene", lineOfWire(s.wire, s.P.set, s.P.whole, s.P.n) === o.line,
    `${name}\n      page ${lineOfWire(s.wire, s.P.set, s.P.whole, s.P.n)}\n      reef ${o.line}`);
  const set = SETS.find(x => x.name === o.set);
  const line = N.parse(o.line).value0;
  const material = N.materialOf({ line, samples: set.samples.map(x => ({ index: x.index, seconds: x.secs })),
    tape: set.tape ? N.Just.create({ bars: set.tape.bars, bpm: set.tape.bpm }) : N.Nothing.value });
  const knobs = o.knobs.map(k => N.fromIdentifier(k.parameter).value0);
  const shown = text(page.reefSentenceHTML(s.P));
  const said = N.plainText(line.spec)(N.sentences({ line, material, knobs }));
  tally("page(reef) sentence", shown === said, `${name}\n      page ${shown}\n      reef ${said}`);
  const d = N.decodeScene(JSON.stringify({
    corpus: { name: o.set, samples: o.whole ? set.samples : set.samples.filter(x => x.index === o.sample) },
    query: o.query, spec: o.spec, seed: o.seed })).value0;
  for (const c of CYCLES) {
    page.drawCircle(c);
    const GR = page.state().GR;
    const ws = N.wedges({ cycleSeconds: 2, orbit: (o.spec.chain && o.spec.chain.orbit) || 0 })(N.cycleOf(d.corpus)(d.query)(d.spec)(d.seed)(c));
    const spans = ws.map(w => ({ from: N.locate(material)(w.emit.n)(w.emit.begin).along, to: N.locate(material)(w.emit.n)(w.emit.end).along }));
    const ls = N.lanes(2 / 784)(spans);
    const same = GR.length === ws.length && GR.every((g, k) => close(g.u1, ws[k].to) && g.lane === ls[k]
      && close(g.x0, spans[k].from) && close(g.x1, spans[k].to)
      && g.col === N.colourCss(N.colourAt(material)(N.locate(material)(ws[k].emit.n)(ws[k].emit.begin))));
    tally("page(reef) drawing", same, `${name} c${c}`);
  }
});

// ── report ─────────────────────────────────────────────────────────────────
const width = Math.max(...Object.keys(checks).map(k => k.length));
console.log(`A/B over ${B.length} presets × ${CYCLES.length} cycles: the prototype (A) against reef (B)\n`);
for (const [k, c] of Object.entries(checks)) {
  const mark = c.same === c.total ? "same" : `${c.total - c.same} differ`;
  console.log(`  ${k.padEnd(width)}  ${String(c.same).padStart(5)} / ${String(c.total).padEnd(5)} ${mark}`);
}
for (const [check, m] of Object.entries(phrases)) {
  const rows = [...m.entries()].sort((p, q) => q[1].count - p[1].count);
  console.log(`\n── ${check}: what changed, word by word (${rows.length} distinct substitutions)`);
  for (const [, e] of (all ? rows : rows.slice(0, 40))) console.log(`   ${String(e.count).padStart(4)}×  ${e.example}`);
  if (!all && rows.length > 40) console.log(`   … ${rows.length - 40} more (--all)`);
}
for (const [k, c] of Object.entries(checks)) {
  if (!c.diffs.length || phrases[k]) continue;
  console.log(`\n── ${k}: ${c.diffs.length} difference${c.diffs.length === 1 ? "" : "s"}`);
  for (const dline of (all ? c.diffs : c.diffs.slice(0, 4))) console.log("   " + dline);
  if (!all && c.diffs.length > 4) console.log(`   … ${c.diffs.length - 4} more (--all)`);
}
