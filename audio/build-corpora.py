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

import json, os, sys

SETS = os.path.expanduser("~/.itajara/quadrat/samples")
OUT = os.path.join(os.path.dirname(__file__), "..", "public",
                   "conspicillum-corpora.json")


def corpus(name):
    d = json.load(open(os.path.join(SETS, name, "set.json")))
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
        })
    return {"name": name, "samples": samples}


def main():
    sets = []
    for nm in sorted(os.listdir(SETS)):
        p = os.path.join(SETS, nm, "set.json")
        if not os.path.exists(p):
            continue
        try:
            c = corpus(nm)
        except Exception as e:
            print(f"  skipped {nm}: {e}", file=sys.stderr)
            continue
        if not c["samples"]:
            continue
        sets.append(c)
        n = len(c["samples"])
        withnotes = sum(1 for s in c["samples"] if s["notes"])
        dim = len(c["samples"][0]["cell"])
        print(f"  {nm:34s} {n:3d} samples, {withnotes:3d} with notes, cell dim {dim}")

    out = os.path.normpath(OUT)
    json.dump({"sets": sets}, open(out, "w"))
    print(f"\nwritten: {out}  ({os.path.getsize(out)} bytes, {len(sets)} sets)")


if __name__ == "__main__":
    main()
