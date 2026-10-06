// An FM electric piano: a sine carrier, a sine modulator at the same
// frequency whose depth falls away after the strike (bright attack, mellow
// body), and a brief high "tine" modulator for the bell at the front of a
// Rhodes-like note. Through one compressor so chords don't clip.
let ctx = null;
let master = null;

const ensure = () => {
  if (!ctx) {
    const AC = window.AudioContext || window.webkitAudioContext;
    ctx = new AC();
    const comp = ctx.createDynamicsCompressor();
    comp.threshold.value = -18;
    comp.ratio.value = 4;
    master = ctx.createGain();
    master.gain.value = 0.5;
    master.connect(comp);
    comp.connect(ctx.destination);
  }
  if (ctx.state === "suspended") ctx.resume();
  return ctx;
};

export const playNote_ = (note) => (velocity) => (delayMs) => (durMs) => () => {
  const c = ensure();
  const t0 = c.currentTime + Math.max(0, delayMs) / 1000 + 0.01;
  const hold = Math.max(0.05, durMs / 1000);
  const end = t0 + hold;
  const release = 0.35;
  const f = 440 * Math.pow(2, (note - 69) / 12);
  const v = Math.max(0, Math.min(1, velocity / 127));
  // quieter up high, as a real tine piano is
  const scale = Math.min(1, Math.max(0.35, 1.25 - (note - 48) / 60));

  const car = c.createOscillator();
  car.frequency.value = f;

  const mod = c.createOscillator();
  mod.frequency.value = f;
  const modDepth = c.createGain();
  modDepth.gain.setValueAtTime(f * (0.9 + 2.2 * v), t0);
  modDepth.gain.exponentialRampToValueAtTime(f * 0.18, t0 + 0.7);
  mod.connect(modDepth);
  modDepth.connect(car.frequency);

  const tine = c.createOscillator();
  tine.frequency.value = f * 14;
  const tineDepth = c.createGain();
  tineDepth.gain.setValueAtTime(f * 0.9 * v, t0);
  tineDepth.gain.exponentialRampToValueAtTime(0.01, t0 + 0.08);
  tine.connect(tineDepth);
  tineDepth.connect(car.frequency);

  const amp = c.createGain();
  const peak = 0.22 * (0.35 + 0.65 * v) * scale;
  amp.gain.setValueAtTime(0.0001, t0);
  amp.gain.exponentialRampToValueAtTime(peak, t0 + 0.006);
  amp.gain.exponentialRampToValueAtTime(peak * 0.3, t0 + Math.min(hold, 1.6));
  amp.gain.setValueAtTime(peak * 0.3, end);
  amp.gain.exponentialRampToValueAtTime(0.0001, end + release);
  car.connect(amp);
  amp.connect(master);

  for (const o of [car, mod, tine]) {
    o.start(t0);
    o.stop(end + release + 0.05);
  }
};
