"""Score what each slice of a take sounds like: kick, snare, hat, 0..1 each.

    python3 score-hits.py fd-beat-bar              # sixteenths at 120 bpm
    python3 score-hits.py some-set --slices 32     # or say how many

Writes `hits: {kick, snare, hat, slices, method}` into every sample of
~/.itajara/quadrat/samples/<set>/set.json, and prints the table so the
scores can be argued with. Rebuild the corpora afterwards (build-corpora.py);
Conspicillum's `Hit kind threshold` rules read them, at the slice under each
grain's read head.

This is four band energies over the first 50 ms of each slice — the hit, not
its tail — and three formulas. It is not a drum transcriber and does not try
to be. A kick with a click scores a little hat; a clap scores as snare; a
ghost note scores low in everything. They are SCORES rather than labels so
the rule chooses its threshold, and a low threshold is how a snare rule
starts catching the clap: an error you can play.
"""

import argparse, json, os, subprocess
import numpy as np

SETS = os.path.expanduser("~/.itajara/quadrat/samples")
SR = 48000
METHOD = "bands-50ms-v1"


def mono(path):
    raw = subprocess.run(["ffmpeg", "-v", "error", "-i", path, "-ac", "1", "-ar", str(SR),
                          "-f", "f32le", "-"], capture_output=True, check=True).stdout
    return np.frombuffer(raw, dtype=np.float32).astype(float)


def band(seg, lo, hi):
    spec = np.abs(np.fft.rfft(seg * np.hanning(len(seg)))) ** 2
    f = np.fft.rfftfreq(len(seg), 1 / SR)
    return spec[(f >= lo) & (f < hi)].sum()


def norm(v):
    m = max(v) if len(v) else 0
    return [x / m if m > 0 else 0.0 for x in v]


def score(x, slices):
    size = len(x) / slices
    head = int(0.05 * SR)
    rows = []
    for k in range(slices):
        a = int(round(k * size))
        seg = x[a:a + min(head, int(size))]
        if len(seg) < 64:
            seg = np.pad(seg, (0, 64 - len(seg)))
        rows.append([band(seg, 30, 150), band(seg, 150, 1000),
                     band(seg, 1500, 6000), band(seg, 7000, 16000)])
    low, body, noise, air = (norm(c) for c in zip(*rows))
    kick = low
    snare = [np.sqrt(b * n) * (1 - 0.7 * l) for l, b, n in zip(low, body, noise)]
    hat = [a * (1 - b) * (1 - 0.7 * l) for l, b, a in zip(low, body, air)]
    r3 = lambda v: [round(float(s), 3) for s in norm(v)]
    return {"kick": r3(kick), "snare": r3(snare), "hat": r3(hat)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("set")
    ap.add_argument("--slices", type=int, default=0,
                    help="slices per sample (default: sixteenths at --bpm)")
    ap.add_argument("--bpm", type=float, default=120.0)
    a = ap.parse_args()
    d = os.path.join(SETS, a.set)
    sj = json.load(open(os.path.join(d, "set.json")))
    for s in sj["samples"]:
        x = mono(os.path.join(d, s["file"]))
        secs = len(x) / SR
        n = a.slices or max(1, round(secs / (60.0 / a.bpm / 4)))
        h = score(x, n)
        s["hits"] = {**h, "slices": n, "method": METHOD}
        print(f"{s['file']}: {n} slices")
        print("   k  kick snare  hat")
        for k in range(n):
            tag = max(("kick", "snare", "hat"), key=lambda t: h[t][k])
            best = h[tag][k]
            print(f"  {k:2d}  {h['kick'][k]:.2f}  {h['snare'][k]:.2f}  {h['hat'][k]:.2f}"
                  + (f"   {tag}" if best >= 0.5 else ""))
    json.dump(sj, open(os.path.join(d, "set.json"), "w"), indent=2)
    print(f"written: {os.path.join(d, 'set.json')}")


if __name__ == "__main__":
    main()
