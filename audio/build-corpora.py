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
import json, os, sys

SETS = os.path.expanduser("~/.itajara/quadrat/samples")
# Tapes (project-tape.py): kept takes projected whole, set.json-shaped, with the
# tape's meaning under "tape". Listed after the sets.
TAPES = os.path.expanduser("~/.itajara/quadrat/tapes")
OUT = os.path.join(os.path.dirname(__file__), "..", "public",
                   "conspicillum-corpora.json")


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
