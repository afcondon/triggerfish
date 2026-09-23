#!/usr/bin/env python3
"""Turn any audio file into a Quadrat set, and so into a Conspicillum bank.

Conspicillum reads `~/.itajara/quadrat/samples/<set>/set.json`, and SuperDirt
loads the same directory as a bank — the directory layout IS the bank. So the
only thing standing between a file on a drive and the instrument is a
`set.json`, and everything the rig has played so far got one by being recorded
through Quadrat. This is for material that was not: an archive pull, a
Morphagene reel, a lecture.

**The measurements come from `msm onset`, not from here**, and that is the
whole point. `peak`, `rms`, `zcr` and `tilt` are what Conspicillum's selector
filters and weights on, so a set measured by different arithmetic would put its
samples on axes that do not mean what the other sets' axes mean — the filter
would still work and would quietly be comparing two different things. One
binary measures everything.

`decay` is the exception and is computed here, because Quadrat reads it off an
envelope the Itajara daemon drew and there is no daemon in this path. The rule
is Quadrat's (`Main.purs settledFor`): bucket the take, take the threshold as
three times the take's OWN quietest bucket — never an absolute level, because
the ES-9's DC-coupled inputs read -32.6 dBFS as silence — and the decay is how
far into the region the last bucket above that threshold sits.

Usage:
  make-set.py <file> <set-name> [--as hits|chords|break|passage|ambient]
                                [--whole] [--min-secs N] [--limit N]

  --whole   one sample, the entire file. For scanning rather than selecting:
            `position` becomes a scrub over the whole duration.
  default   divide at detected onsets, one sample per phrase.
"""

import argparse, array, json, os, subprocess, sys, tempfile, time

MSM = os.path.expanduser("~/.cargo/bin/msm")   # NOT the one on PATH: that is a
                                               # stale copy and cargo build does
                                               # not update it.
SETS = os.path.expanduser("~/.itajara/quadrat/samples")


def run_json(args):
    out = subprocess.run(args, capture_output=True, text=True)
    if out.returncode != 0:
        sys.exit(f"msm failed: {out.stderr.strip() or out.returncode}")
    return json.loads(out.stdout)


def onsets(path, material):
    """Every onset msm can see, as times in seconds."""
    out = subprocess.run([MSM, "onset", "--as", material, "--all", path],
                         capture_output=True, text=True)
    times = []
    for line in out.stdout.splitlines():
        parts = line.split()
        # "      3     4.573s" — and NOT "  95 onsets", which has the same
        # shape and parsed as a time until it did not.
        if len(parts) == 2 and parts[0].isdigit() and parts[1].endswith("s"):
            try:
                times.append(float(parts[1][:-1]))
            except ValueError:
                pass
    return times


def envelope(path, buckets):
    """Max |sample| per bucket, via ffmpeg — mono, native rate, 16-bit.

    Read at full rate rather than downsampled: a resample low-passes, and an
    envelope built from a low-passed signal understates every transient, which
    is exactly the part a decay measurement turns on.
    """
    p = subprocess.run(
        ["ffmpeg", "-v", "error", "-i", path, "-ac", "1", "-f", "s16le", "-"],
        capture_output=True)
    a = array.array("h")
    a.frombytes(p.stdout[: len(p.stdout) // 2 * 2])
    if not len(a):
        sys.exit("ffmpeg produced no audio")
    per = max(1, len(a) // buckets)
    return [max(abs(v) for v in a[i:i + per]) for i in range(0, len(a), per)], len(a)


def decays(env, total_secs, regions):
    """Quadrat's rule, reimplemented: see the module docstring."""
    n = len(env)
    quietest = min(env)
    thr = 3 * max(1, quietest)
    loudest = max(1, max(env))
    out = []
    for r in regions:
        span = max(0.001, r["end"] - r["start"])
        lo = min(n - 1, int(n * r["start"] / total_secs))
        hi = max(lo + 1, min(n, int(n * r["end"] / total_secs)))
        inside = env[lo:hi]
        last = -1
        for i, v in enumerate(inside):
            if v > thr:
                last = i
        m = max(1, len(inside))
        out.append({
            "decay": 0.0 if last < 0 else span * (last + 1) / m,
            "floor": quietest / loudest,
        })
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("file")
    ap.add_argument("name")
    ap.add_argument("--as", dest="material", default="ambient",
                    choices=["hits", "chords", "break", "passage", "ambient"])
    ap.add_argument("--whole", action="store_true")
    ap.add_argument("--min-secs", type=float, default=0.35,
                    help="drop regions shorter than this (default 0.35)")
    ap.add_argument("--limit", type=int, default=0,
                    help="keep at most N samples, longest first")
    a = ap.parse_args()

    src = os.path.abspath(a.file)
    if not os.path.exists(src):
        sys.exit(f"no such file: {src}")

    probe = run_json([MSM, "onset", "--json", "--as", "ambient", src])
    total = probe["secs"]
    print(f"{os.path.basename(src)}  {total:.1f}s  {probe['sampleRate']} Hz")

    if a.whole:
        regions = [{"start": 0.0, "end": total}]
    else:
        ts = onsets(src, a.material)
        if not ts:
            sys.exit("no onsets found — try --as break, or --whole")
        bounds = ts + [total]
        regions = [{"start": bounds[i], "end": bounds[i + 1]}
                   for i in range(len(ts))]
        # A region shorter than a syllable is not a fragment anybody wanted;
        # it is the detector twitching. Dropped here rather than in the
        # surface, because a set is meant to be playable as stored.
        kept = [r for r in regions if r["end"] - r["start"] >= a.min_secs]
        print(f"  {len(regions)} regions, {len(regions) - len(kept)} shorter "
              f"than {a.min_secs}s dropped")
        regions = kept
        if a.limit and len(regions) > a.limit:
            regions = sorted(sorted(regions,
                                    key=lambda r: r["start"] - r["end"])[:a.limit],
                             key=lambda r: r["start"])
            print(f"  kept the {a.limit} longest")
    if not regions:
        sys.exit("nothing left to cut")

    out = os.path.join(SETS, a.name)
    os.makedirs(out, exist_ok=True)
    # NOT inside `out`: `msm cut --overwrite` clears the output directory
    # first, and a regions file living there is deleted by the command that
    # was about to read it. Measured, once.
    rj = os.path.join(tempfile.gettempdir(), f"regions-{a.name}-{os.getpid()}.json")
    json.dump(regions, open(rj, "w"))

    cut = subprocess.run([MSM, "cut", "--regions", rj, "--out", out,
                          "--name", a.name, "--stereo", "--overwrite", src],
                         capture_output=True, text=True)
    if cut.returncode != 0:
        sys.exit(f"msm cut failed: {cut.stderr.strip()}")
    print("  " + cut.stdout.strip())

    meas = run_json([MSM, "onset", "--json", "--regions", rj, src])["regions"]
    env, _ = envelope(src, 4000)
    dec = decays(env, total, regions)

    samples = []
    for i, (r, m, d) in enumerate(zip(regions, meas, dec), start=1):
        samples.append({
            "file": f"{a.name}-{i:02d}.wav",
            "cell": [],
            "start": 0.0,
            "end": round(r["end"] - r["start"], 6),
            "peak": m["peak"], "rms": m["rms"], "zcr": m["zcr"],
            "tilt": m["tilt"],
            "decay": round(d["decay"], 6), "floor": round(d["floor"], 6),
            # No MIDI was listened to and no parameter was swept: this material
            # was found, not made. Empty is the honest value, and the surface
            # already renders it as "no recorded notes" rather than guessing.
            "means": [], "notes": [],
        })

    doc = {
        "version": 1, "name": a.name, "take": a.name,
        "made": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
        "kind": "found", "stereo": True, "sliced": not a.whole,
        "slots": 0, "slotSecs": 0, "voltsPerLevel": 10, "spec": None,
        "listened": {"source": os.path.basename(src)},
        "schedule": [],
    }
    doc["samples"] = samples
    json.dump(doc, open(os.path.join(out, "set.json"), "w"), indent=1)
    os.remove(rj)

    secs = [s["end"] for s in samples]
    print(f"  wrote {out}/set.json — {len(samples)} samples, "
          f"{min(secs):.2f}..{max(secs):.2f}s each")
    print("  now: audio/build-corpora.py, and restart SuperDirt to load the bank")


if __name__ == "__main__":
    main()
