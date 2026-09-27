#!/usr/bin/env node
// grain-probe — how many grains per second can this rig actually sound?
//
// C1 of `docs/CONSPICILLUM-DESIGN.md`. A grain cloud is not a synth setting in
// SuperDirt; it is one `/dirt/play` per grain. So the instrument's density
// control has a real ceiling somewhere, set by machinery nobody here has
// measured, and the surface should not be drawn until the number is known.
//
// WHAT IT MEASURES, and why it is not obvious which thing breaks first. The
// chain from here to sound has three stages that can each give out:
//
//   this probe  →  sclang  →  scsynth
//                  ^^^^^^     ^^^^^^^
//                  interpreted SuperCollider, single-threaded, GC'd. It runs
//                  the whole DirtEvent module chain PER GRAIN before a single
//                  synth message is sent. This is the stage most likely to
//                  fail first and the one nothing reports on.
//                             |
//                             the DSP graph. Fails loudly ("too many nodes",
//                             "alloc failed") and is instrumented: /status
//                             gives node count and CPU for the asking.
//
// So the probe reads scsynth's own `/status` as its instrument, and infers
// sclang's health from the gap between the grains it SENT and the synths that
// actually appeared. A cloud that is dropping grains in the language layer
// looks, from the outside, exactly like a cloud that is quieter than you asked
// for — which is the same silent-failure shape as everything else in C0.
//
// IT SENDS TIMETAGGED BUNDLES ON ONE SOCKET, deliberately: that is the
// `reef_stellatus_voice.erl` technique (Stellatus, since retired; reef_conspicillum_voice.erl keeps it), and it is the path a real cloud would
// take. The generic `Tidal/OSC.erl sendDirtAfter` path — plain message, one
// spawned process and one fresh UDP socket PER EVENT — is not measured here
// because no cloud should ever use it. See the design doc, finding 3.
//
// Output is whatever scsynth was booted onto (BlackHole on this rig), so this
// is silent unless something is monitoring that device.
//
//   node audio/grain-probe.mjs --selftest        # bundle bytes, sends nothing
//   node audio/grain-probe.mjs                   # the sweep
//   node audio/grain-probe.mjs --window          # again, with a grain envelope
//   node audio/grain-probe.mjs --set brass --sustain 0.08
//
// Zero dependencies (node built-ins only), per the house rule for anything in
// this directory.

import dgram from 'node:dgram';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';

const DIRT_PORT = Number(process.env.SUPERDIRT_PORT ?? 57120);
const DIRT_HOST = process.env.SUPERDIRT_HOST ?? '127.0.0.1';
const SCSYNTH_PORT = Number(process.env.SCSYNTH_PORT ?? 57110);
const SETS_DIR = process.env.QUADRAT_SETS_DIR
  ?? path.join(os.homedir(), '.itajara/quadrat/samples');

const argv = process.argv.slice(2);
const flag = (name) => argv.includes(`--${name}`);
const opt = (name, dflt) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : dflt;
};

const SET = opt('set', 'longform-0914-125645');
const SUSTAIN = Number(opt('sustain', 0.05));   // grain length, seconds
const SECS = Number(opt('secs', 6));            // per density step
const WINDOW = flag('window');                  // set `tilt` → +1 synth/grain
const LOOKAHEAD_MS = Number(opt('lookahead', 200));
const DENSITIES = (opt('densities', '25,50,100,200,400,800'))
  .split(',').map(Number);

// ── OSC 1.0 encoding ───────────────────────────────────────────────────────
const NTP_EPOCH_OFFSET = 2208988800;

function oscString(s) {
  const b = Buffer.from(String(s), 'ascii');
  return Buffer.concat([b, Buffer.alloc(4 - (b.length % 4))]); // pad ≥1 null
}
const oscFloat = (f) => { const b = Buffer.alloc(4); b.writeFloatBE(f, 0); return b; };
const oscInt = (i) => { const b = Buffer.alloc(4); b.writeInt32BE(i | 0, 0); return b; };

function encodeMsg(addr, params) {
  let tags = ',';
  const parts = [];
  for (const [k, v] of Object.entries(params)) {
    tags += 's'; parts.push(oscString(k));
    if (typeof v === 'number') { tags += 'f'; parts.push(oscFloat(v)); }
    else { tags += 's'; parts.push(oscString(v)); }
  }
  return Buffer.concat([oscString(addr), oscString(tags), ...parts]);
}

// Bundle one message at an absolute wall time. SuperDirt's sclang side honours
// the timetag and schedules the sound there, so sending ahead lands it on time
// instead of late-on-receipt. Same NTP arithmetic as reef_conspicillum_voice.erl.
function bundle(whenUnixMs, msg) {
  const secs = Math.floor(whenUnixMs / 1000) + NTP_EPOCH_OFFSET;
  const frac = Math.floor(((whenUnixMs % 1000) / 1000) * 4294967296);
  const tt = Buffer.alloc(8);
  tt.writeUInt32BE(secs >>> 0, 0);
  tt.writeUInt32BE(frac >>> 0, 4);
  const el = Buffer.alloc(4);
  el.writeInt32BE(msg.length, 0);
  return Buffer.concat([oscString('#bundle'), tt, el, msg]);
}

// ── the grain ──────────────────────────────────────────────────────────────
// A grain's begin/end window must be as long IN THE SOURCE as `sustain` is in
// time, or the grain is transposed: SuperDirt sweeps begin→end over sustain,
// so the effective rate is (end-begin)*fileSecs/sustain. Read the real
// duration out of the Quadrat set.json rather than guessing it.
function sourceSeconds(setName) {
  const p = path.join(SETS_DIR, setName, 'set.json');
  try {
    const d = JSON.parse(fs.readFileSync(p, 'utf8'));
    const s0 = d.samples?.[0];
    if (s0) return { secs: s0.end - s0.start, n: d.samples.length };
  } catch (e) {
    console.error(`!! cannot read ${p}: ${e.message}`);
    console.error('   (is the set name right? ls ' + SETS_DIR + ')');
    process.exit(1);
  }
  process.exit(1);
}

const { secs: SRC_SECS, n: SRC_N } = sourceSeconds(SET);
const GRAIN_FRAC = SUSTAIN / SRC_SECS;   // window width as a fraction of the file

function grainParams(cps) {
  const begin = Math.random() * (1 - GRAIN_FRAC);
  const p = {
    s: SET,
    orbit: 0,
    cps,
    cycle: 0,
    delta: SUSTAIN,
    n: Math.floor(Math.random() * SRC_N),
    begin,
    end: begin + GRAIN_FRAC,
    sustain: SUSTAIN,
    speed: 1,
    gain: 0.4,
  };
  // The grain envelope is gated on `tilt` and costs a SECOND synth per grain
  // (grenvelo). Measuring with and without is the point of the flag: if the
  // ceiling is nodes, the window halves the achievable density.
  if (WINDOW) { p.tilt = 0.5; p.plat = 0.2; p.curve = -3; }
  return p;
}

if (flag('selftest')) {
  const msg = encodeMsg('/dirt/play', grainParams(0.5));
  const b = bundle(Date.now() + 200, msg);
  console.log(`set          ${SET}  (${SRC_N} sample(s), ${SRC_SECS.toFixed(2)}s each)`);
  console.log(`grain        sustain ${SUSTAIN}s  = ${(GRAIN_FRAC * 100).toFixed(4)}% of the source`);
  console.log(`window       ${WINDOW ? 'tilt/plat/curve set (2 synths per grain)' : 'off (1 synth per grain)'}`);
  console.log(`/dirt/play   ${msg.length} bytes, multiple of 4: ${msg.length % 4 === 0}`);
  console.log(`#bundle      ${b.length} bytes, multiple of 4: ${b.length % 4 === 0}`);
  console.log(b.toString('hex').replace(/(..)/g, '$1 ').trim());
  process.exit(0);
}

// ── scsynth /status ────────────────────────────────────────────────────────
// Reply is ,iiiiiffdd : unused, ugens, synths, groups, synthdefs, avgCPU,
// peakCPU, nominalSR, actualSR.
const status = dgram.createSocket('udp4');
let latest = null;

status.on('message', (buf) => {
  let i = 0;
  const readStr = () => {
    const end = buf.indexOf(0, i);
    const s = buf.toString('ascii', i, end);
    i = end + (4 - (end % 4));
    return s;
  };
  const addr = readStr();
  if (addr !== '/status.reply') return;
  readStr(); // type tags
  const ints = [];
  for (let k = 0; k < 5; k++) { ints.push(buf.readInt32BE(i)); i += 4; }
  const avgCPU = buf.readFloatBE(i); i += 4;
  const peakCPU = buf.readFloatBE(i); i += 4;
  latest = { ugens: ints[1], synths: ints[2], groups: ints[3], avgCPU, peakCPU };
});

const askStatus = () => {
  const m = Buffer.concat([oscString('/status'), oscString(',')]);
  status.send(m, SCSYNTH_PORT, DIRT_HOST);
};

// ── the sweep ──────────────────────────────────────────────────────────────
const udp = dgram.createSocket('udp4');
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function baseline() {
  latest = null;
  for (let k = 0; k < 6 && !latest; k++) { askStatus(); await sleep(150); }
  if (!latest) {
    console.error(`!! no /status.reply from scsynth on ${SCSYNTH_PORT} — is the rig up?`);
    console.error('   lsof -nP -iUDP:57110');
    process.exit(1);
  }
  return { ...latest };
}

async function runDensity(d, base) {
  const total = Math.round(d * SECS);
  const t0 = Date.now() + LOOKAHEAD_MS;
  const cps = 0.5;
  let sent = 0, sendErrs = 0;
  const peak = { synths: 0, ugens: 0, avgCPU: 0, peakCPU: 0 };
  const samples = [];

  const poll = setInterval(() => {
    askStatus();
    if (latest) {
      samples.push(latest.synths);
      peak.synths = Math.max(peak.synths, latest.synths);
      peak.ugens = Math.max(peak.ugens, latest.ugens);
      peak.avgCPU = Math.max(peak.avgCPU, latest.avgCPU);
      peak.peakCPU = Math.max(peak.peakCPU, latest.peakCPU);
    }
  }, 100);

  // Lookahead scheduler: every tick, send every grain due within the window.
  // Dumping all of them at once would measure the UDP receive buffer, not the
  // cloud — the pacing IS the experiment.
  let next = 0;
  const TICK = 25;
  while (next < total) {
    const horizon = Date.now() + LOOKAHEAD_MS;
    while (next < total) {
      const due = t0 + (next / d) * 1000;
      if (due > horizon) break;
      const b = bundle(due, encodeMsg('/dirt/play', grainParams(cps)));
      udp.send(b, DIRT_PORT, DIRT_HOST, (e) => { if (e) sendErrs++; });
      sent++; next++;
    }
    await sleep(TICK);
  }
  // Let the tail sound and the nodes free before reading the verdict.
  await sleep(LOOKAHEAD_MS + SUSTAIN * 1000 + 600);
  clearInterval(poll);

  const observed = Math.max(0, peak.synths - base.synths);
  return { d, sent, sendErrs, observed, peak, samples };
}

console.log(`grain-probe — ${SET} (${SRC_N} sample(s), ${SRC_SECS.toFixed(2)}s)`);
console.log(`  grain ${SUSTAIN}s${WINDOW ? ' + envelope (grenvelo)' : ''}, `
  + `${SECS}s per step, lookahead ${LOOKAHEAD_MS}ms`);
console.log(`  → SuperDirt ${DIRT_HOST}:${DIRT_PORT}, scsynth status ${SCSYNTH_PORT}\n`);

const base = await baseline();
console.log(`idle: ${base.synths} synths, ${base.ugens} ugens, `
  + `avgCPU ${base.avgCPU.toFixed(1)}%\n`);

// **The ceiling is where the line stops being straight** — and the honest way
// to see that needs no reference point at all. How many synths one
// `/dirt/play` becomes is SuperDirt's business (the modules that fire depend
// on which params are set), so instead of predicting a concurrency we report
// the SLOPE at each step: concurrent synths per 100 grains/sec. While every
// grain sounds, that number is flat. When it falls away, grains are being
// discarded — and if node count and CPU are nowhere near their limits when it
// does, the loss is in the language layer, which is the one stage in the chain
// that reports nothing.
console.log('  dens   sent   synths   per 100/s   CPU avg/peak    verdict');
console.log('  ────   ────   ──────   ─────────   ────────────    ───────');

const rows = [];
let bestSlope = 0;
for (const d of DENSITIES) {
  const r = await runDensity(d, base);
  r.slope = (r.observed / r.d) * 100;
  bestSlope = Math.max(bestSlope, r.slope);
  rows.push(r);
  r.ratio = bestSlope > 0 ? r.slope / bestSlope : 1;
  const verdict =
    r.sendErrs > 0 ? `${r.sendErrs} SEND ERRORS`
    : r.ratio >= 0.8 ? 'ok'
    : r.ratio >= 0.5 ? 'LAGGING'
    : 'DROPPING';
  console.log(
    `  ${String(r.d).padStart(4)}  ${String(r.sent).padStart(5)}   `
    + `${String(r.observed).padStart(6)}   ${r.slope.toFixed(1).padStart(7)}   `
    + `${r.peak.avgCPU.toFixed(1).padStart(5)}/${r.peak.peakCPU.toFixed(1).padEnd(6)}   ${verdict}`
  );
  await sleep(1200);
}

const good = rows.filter((r) => r.ratio >= 0.8 && r.sendErrs === 0);
const ceiling = good.length ? good[good.length - 1].d : 0;
const topSynths = Math.max(...rows.map((r) => r.peak.synths));
const topCPU = Math.max(...rows.map((r) => r.peak.avgCPU));
console.log(`\n  a grain costs ${(bestSlope / 100 / SUSTAIN).toFixed(2)} concurrent synths`
  + ` (its own, plus whatever SuperDirt adds)`);
console.log(`  clean to: ${ceiling} grains/sec`
  + (ceiling === DENSITIES[DENSITIES.length - 1]
      ? ' — the sweep never broke it, raise --densities' : ''));
console.log(`  peaks: ${topSynths} synths of maxNodes 4096 (${(topSynths / 4096 * 100).toFixed(0)}%),`
  + ` CPU ${topCPU.toFixed(0)}%`);
console.log('\n  If the slope fell while nodes and CPU stayed low, the limit is sclang — it runs');
console.log('  the whole DirtEvent chain per grain — and no scsynth setting will move it.');

udp.close();
status.close();
