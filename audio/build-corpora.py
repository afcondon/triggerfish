"""Bake every Quadrat set into one static corpora file the pump page can fetch.

The browser cannot read ~/.itajara, and Triggerfish's frontend is a plain
static server with no API. So the corpora are generated into `public/` and
served as a file. Re-run after recording a new set; nothing watches.

The field mapping is deliberately the SAME as conspicillum-scene.py's
`corpus()` — one shape reaching the engine, whether a scene came from the
command line or from the page. `index` is the SuperDirt `n`, so samples with
no recorded notes are kept rather than filtered: dropping one here would
renumber every sample after it and the cloud would play different audio than
it named.
"""

import glob
import json, os, struct, sys

import numpy as np

SETS = os.path.expanduser("~/.itajara/quadrat/samples")
# Tapes (project-tape.py): kept takes projected whole, set.json-shaped, with the
# tape's meaning under "tape". Listed after the sets.
TAPES = os.path.expanduser("~/.itajara/quadrat/tapes")
OUT = os.path.join(os.path.dirname(__file__), "..", "public",
                   "conspicillum-corpora.json")


# The waveform under the strip: each sample's peak envelope, |x| per bin as
# 0..255 of full scale, so relative loudness between samples survives. About
# a hundred bins a second, at least 64 so a short hit still has a shape, and at
# most 800 so a one-bar tape fills the strip's 784 pixels and no more.
BINS_PER_SECOND = 100
BINS_MIN, BINS_MAX = 64, 800


def read_wav(path):
    """Mono float frames from a WAV, PCM or IEEE float. The wave module refuses
    format 3, which is what the tapes are written as."""
    with open(path, "rb") as f:
        data = f.read()
    if data[:4] != b"RIFF" or data[8:12] != b"WAVE":
        raise ValueError("not a RIFF/WAVE file")
    fmt, frames, at = None, None, 12
    while at + 8 <= len(data):
        chunk, size = data[at:at + 4], struct.unpack("<I", data[at + 4:at + 8])[0]
        body = data[at + 8:at + 8 + size]
        if chunk == b"fmt ":
            tag, channels, _rate, _, _, bits = struct.unpack("<HHIIHH", body[:16])
            if tag == 0xFFFE:  # EXTENSIBLE: the real tag leads the subformat GUID
                tag = struct.unpack("<H", body[24:26])[0]
            fmt = (tag, channels, bits)
        elif chunk == b"data":
            frames = body
        at += 8 + size + (size & 1)
    if fmt is None or frames is None:
        raise ValueError("no fmt or data chunk")
    tag, channels, bits = fmt
    if tag == 3 and bits == 32:
        x = np.frombuffer(frames, dtype="<f4").astype(np.float64)
    elif tag == 1 and bits == 16:
        x = np.frombuffer(frames, dtype="<i2") / 32768.0
    elif tag == 1 and bits == 24:
        b = np.frombuffer(frames[:len(frames) // 3 * 3], dtype=np.uint8).reshape(-1, 3)
        i = (b[:, 0].astype(np.int32) | (b[:, 1].astype(np.int32) << 8) | (b[:, 2].astype(np.int32) << 16))
        x = np.where(i >= 1 << 23, i - (1 << 24), i) / 8388608.0
    elif tag == 1 and bits == 32:
        x = np.frombuffer(frames, dtype="<i4") / 2147483648.0
    else:
        raise ValueError(f"unsupported WAV format {tag}/{bits} bits")
    n = len(x) // channels
    return np.abs(x[:n * channels].reshape(n, channels)).max(axis=1)


def envelope(path, secs):
    magnitude = read_wav(path)
    bins = int(min(BINS_MAX, max(BINS_MIN, round(secs * BINS_PER_SECOND))))
    if len(magnitude) == 0:
        return [0] * bins
    edges = np.linspace(0, len(magnitude), bins + 1).astype(int)
    peaks = [float(magnitude[a:max(b, a + 1)].max()) if a < len(magnitude) else 0.0
             for a, b in zip(edges[:-1], edges[1:])]
    return [int(round(min(1.0, p) * 255)) for p in peaks]


def corpus(name, root=SETS):
    d = json.load(open(os.path.join(root, name, "set.json")))
    samples = []
    for i, s in enumerate(d.get("samples") or []):
        samples.append({
            "index": i,
            "secs": round(s["end"] - s["start"], 6),
            "peak": s["peak"], "rms": s["rms"], "zcr": s["zcr"],
            "tilt": s["tilt"], "decay": s["decay"],
            "cell": s.get("cell") or [],
            "params": [{"name": m["name"], "level": m["level"]}
                       for m in (s.get("means") or [])],
            "notes": s.get("notes") or [],
            # score-hits.py's kick/snare/hat per slice; empty = never scored.
            "hits": {k: (s.get("hits") or {}).get(k, []) for k in ("kick", "snare", "hat")},
            "envelope": envelope(os.path.join(root, name, s["file"]), s["end"] - s["start"]),
        })
    c = {"name": name, "samples": samples}
    # A tape carries what it means as one — bars, tempo, swing — so the page
    # can set itself up to play it; the scene sent to the rig never sees it.
    if d.get("tape"):
        c["tape"] = d["tape"]
    return c


def main():
    sets = []
    orphans = []
    for nm in sorted(os.listdir(SETS)):
        p = os.path.join(SETS, nm, "set.json")
        if not os.path.exists(p):
            # A directory of audio with no set.json is not nothing — it is a
            # take that never became a set, and it is INVISIBLE to the surface
            # while looking perfectly present on disk. Saying so costs one line
            # and is the difference between "that set isn't in the dropdown"
            # being a mystery and being a fact.
            wavs = len(glob.glob(os.path.join(SETS, nm, "*.wav")))
            if wavs:
                orphans.append((nm, wavs))
            continue
        try:
            c = corpus(nm)
        except Exception as e:
            print(f"  skipped {nm}: {e}", file=sys.stderr)
            continue
        if not c["samples"]:
            print(f"  skipped {nm}: set.json lists no samples", file=sys.stderr)
            continue
        sets.append(c)
        n = len(c["samples"])
        withnotes = sum(1 for s in c["samples"] if s["notes"])
        dim = len(c["samples"][0]["cell"])
        print(f"  {nm:34s} {n:3d} samples, {withnotes:3d} with notes, cell dim {dim}")

    if os.path.isdir(TAPES):
        for nm in sorted(os.listdir(TAPES)):
            if os.path.exists(os.path.join(TAPES, nm, "set.json")):
                c = corpus(nm, TAPES)
                sets.append(c)
                t = c.get("tape", {})
                print(f"  {nm:34s} tape: {t.get('bars')} bars at {t.get('bpm')} bpm")

    out = os.path.normpath(OUT)
    json.dump({"sets": sets}, open(out, "w"))
    if orphans:
        print("\naudio with no set.json — present on disk, absent from the surface:")
        for nm, n in orphans:
            print(f"  {nm:<36}{n} wav{'s' if n != 1 else ''}")

    print(f"\nwritten: {out}  ({os.path.getsize(out)} bytes, {len(sets)} sets)")


if __name__ == "__main__":
    main()
