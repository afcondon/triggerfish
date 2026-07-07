"use strict";

// Browser-side onset detection for Stellatus buffer import.
//
// fetch(url) → decodeAudioData → mono mixdown → an energy-flux onset detection
// function (half-wave-rectified log-energy difference) → adaptive peak picking
// with a minimum inter-onset interval. Decoding runs on an OfflineAudioContext
// so we never open the speakers: this is analysis, not playback.
//
// Output record: { onsets :: [Number], wave :: [Number], dur :: Number }.
// `onsets` are normalised [0,1) transient positions (sorted, always includes 0);
// `wave` is a 168-bucket abs-peak envelope for the ring.

export const detectImpl = (url) => (sensitivity) => (onError) => (onSuccess) => () => {
  const done = (r) => onSuccess(r)();
  const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();
  try {
    const OAC = window.OfflineAudioContext || window.webkitOfflineAudioContext;
    const ctx = new OAC(1, 1, 44100);
    fetch(url)
      .then((r) => {
        if (!r.ok) throw new Error("fetch " + url + " → HTTP " + r.status);
        return r.arrayBuffer();
      })
      .then((buf) => ctx.decodeAudioData(buf))
      .then((audio) => done(analyze(audio, clamp01(sensitivity))))
      .catch(fail);
  } catch (e) {
    fail(e);
  }
};

const clamp01 = (x) => (x < 0 ? 0 : x > 1 ? 1 : x);

function analyze(audio, sensitivity) {
  const sr = audio.sampleRate;
  const n = audio.length;
  const chs = audio.numberOfChannels;

  // mono mixdown
  const mono = new Float32Array(n);
  for (let c = 0; c < chs; c++) {
    const d = audio.getChannelData(c);
    for (let i = 0; i < n; i++) mono[i] += d[i] / chs;
  }
  const dur = n / sr;

  // onset detection function: framed RMS, log-compressed, half-wave-rectified
  // first difference. Percussive attacks show up as sharp positive jumps.
  const hop = 256;
  const win = 512;
  const nf = Math.max(1, Math.floor((n - win) / hop) + 1);
  const odf = new Float32Array(nf);
  let prev = 0;
  for (let f = 0; f < nf; f++) {
    const start = f * hop;
    let e = 0;
    for (let i = 0; i < win; i++) {
      const s = mono[start + i];
      e += s * s;
    }
    const le = Math.log(1 + 1000 * Math.sqrt(e / win));
    const diff = le - prev;
    odf[f] = diff > 0 ? diff : 0;
    prev = le;
  }

  // normalise ODF to [0,1]
  let mx = 0;
  for (let f = 0; f < nf; f++) if (odf[f] > mx) mx = odf[f];
  if (mx > 0) for (let f = 0; f < nf; f++) odf[f] /= mx;

  // adaptive peak picking: local-mean threshold + a delta that sensitivity
  // shrinks, a minimum inter-onset interval, and a strict local maximum.
  const w = 8; // frames each side for the local mean
  const minIOI = Math.max(1, Math.floor((0.045 * sr) / hop)); // ~45ms
  const delta = 0.02 + 0.10 * (1 - sensitivity); // higher sensitivity → lower bar
  const onsets = [0]; // the loop start is always a slice boundary
  let last = -minIOI;
  for (let f = 1; f < nf - 1; f++) {
    let m = 0;
    let cnt = 0;
    const lo = Math.max(0, f - w);
    const hi = Math.min(nf - 1, f + w);
    for (let k = lo; k <= hi; k++) {
      m += odf[k];
      cnt++;
    }
    m /= cnt;
    if (
      odf[f] > m + delta &&
      odf[f] >= odf[f - 1] &&
      odf[f] > odf[f + 1] &&
      f - last >= minIOI
    ) {
      const t = (f * hop + win / 2) / n;
      if (t > 0.012 && t < 0.995) onsets.push(t);
      last = f;
    }
  }

  // 168-bucket abs-peak waveform envelope for the ring
  const BUCKETS = 168;
  const wave = new Array(BUCKETS);
  const bs = Math.max(1, Math.floor(n / BUCKETS));
  for (let b = 0; b < BUCKETS; b++) {
    let peak = 0;
    const s0 = b * bs;
    for (let i = 0; i < bs; i++) {
      const a = Math.abs(mono[s0 + i] || 0);
      if (a > peak) peak = a;
    }
    wave[b] = peak;
  }

  // Classify each slice (window between consecutive transients) by timbre — no
  // FFT: a one-pole low-pass energy ratio (how bass-dominated) plus zero-crossing
  // rate (how bright). 0 = kick/low, 1 = snare/mid, 2 = hat/high. Rough but enough
  // to colour the ring and, later, seed the jump matrix by hit type.
  const classes = [];
  for (let i = 0; i < onsets.length; i++) {
    const a = Math.floor(onsets[i] * n);
    const b = Math.floor((i + 1 < onsets.length ? onsets[i + 1] : 1.0) * n);
    classes.push(classify(mono, a, Math.min(b, n), sr));
  }

  return { onsets, wave, dur, classes };
}

function classify(mono, s0, s1, sr) {
  const full = s1 - s0;
  if (full <= 0) return 1;
  // Analyse only the ATTACK (first ~40ms), not the ringing decay tail — the tail
  // is high-frequency noise that would read as "hat" for every hit. The attack
  // transient is where kick/snare/hat identity lives.
  const win = Math.min(full, Math.max(1, Math.floor(0.04 * sr)));
  const end = s0 + win;
  // one-pole low-pass at ~180 Hz
  const rc = 1 / (2 * Math.PI * 180);
  const dt = 1 / sr;
  const alpha = dt / (rc + dt);
  let lp = 0, eLow = 0, eTot = 0, zc = 0, prev = 0;
  for (let i = s0; i < end; i++) {
    const x = mono[i];
    lp = lp + alpha * (x - lp);
    eLow += lp * lp;
    eTot += x * x;
    if ((x >= 0) !== (prev >= 0)) zc++;
    prev = x;
  }
  const lowRatio = eTot > 0 ? eLow / eTot : 0;
  const zcr = zc / win;
  if (lowRatio > 0.45) return 0;  // bass-dominated attack → kick
  if (zcr > 0.14) return 2;       // bright, sizzly attack → hat
  return 1;                       // broadband mid attack → snare
}
