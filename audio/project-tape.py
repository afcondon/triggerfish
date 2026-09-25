#!/usr/bin/env python3
"""Project a kept take as a Conspicillum TAPE: the whole take, unsplit.

    project-tape.py bars-0924-220301              # bpm 120, bars from the length
    project-tape.py some-take --bpm 96 --swing 0.6
    project-tape.py --all-bars                    # every take kept as `bars`
    project-tape.py --all                         # ... and every take with a tape.json

Quadrat keeps the take (~/.itajara/takes/<name>/) and the division into
samples (set.json) as one thing; the cut WAVs are one PROJECTION of it (a
SuperDirt bank of cuts, a Rample card). This is another: the take as one
sample, with what it means as a tape — its tempo, its bars, its swing, and
what each sixteenth sounds like — so the grain engine can play it in order,
walk it, swap its bars, and swing it.

Writes, and never cuts or copies audio (a single-layer take is LINKED):

  ~/.itajara/takes/<name>/tape.json            the tape's meaning, kept with
                                               the take it describes
  ~/.itajara/quadrat/tapes/<name>-tape/        the projection: a SuperDirt bank
      <name>-tape-01.wav -> the take's audio   and a set.json the corpus reads
      set.json

The `-tape` suffix is load-bearing: a set is named after its take, and SuperDirt
loads sets after takes, so a tape bank called <name> would be replaced by the
set's cuts. Then run build-corpora.py and restart SuperDirt.

Tempo is not recorded by takes made before 2026-09-25, so it is DECLARED here
(--bpm, default 120: the rig's Link tempo for everything kept so far) and the
file says so ("bpmFrom": "declared" or "assumed").
"""

import argparse, importlib.util, json, os, sys, time
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
TAKES = os.path.expanduser(os.environ.get("ITAJARA_TAKES_DIR", "~/.itajara/takes"))
SETS = os.path.expanduser(os.environ.get("QUADRAT_SETS_DIR", "~/.itajara/quadrat/samples"))
TAPES = os.path.expanduser(os.environ.get("QUADRAT_TAPES_DIR", "~/.itajara/quadrat/tapes"))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


makeset = load("makeset", "make-set.py")      # msm measurement, decay: one arithmetic
hits = load("scorehits", "score-hits.py")     # kick/snare/hat per slice


def project(name, bpm, bpm_from, beats, swing, bars_arg):
    tdir = os.path.join(TAKES, name)
    take = json.load(open(os.path.join(tdir, "take.json")))
    # What the take already says about itself wins over an assumption: Quadrat
    # writes tape.json from Link when it keeps a bars take (since 2026-09-25).
    # A --bpm on the command line is a declaration and wins over both.
    known = os.path.join(tdir, "tape.json")
    if bpm_from == "assumed" and os.path.exists(known):
        k = json.load(open(known))
        if k.get("bpmFrom") in ("link", "declared"):
            bpm, bpm_from = float(k["bpm"]), k["bpmFrom"]
            beats = int(k.get("beats", beats))
            bars_arg = bars_arg or int(k.get("bars", 0))
            swing = k.get("swing", swing) if swing == 0.5 else swing
    layers = take.get("layers") or []
    if len(layers) != 1:
        print(f"  skip {name}: {len(layers)} layers — a tape is one take, mix it first")
        return None
    src = os.path.join(tdir, layers[0]["file"])
    secs = take["loopSecs"]
    bar = beats * 60.0 / bpm
    raw = secs / bar
    bars = bars_arg or max(1, round(raw))
    if not bars_arg and abs(raw - bars) > 0.05:
        print(f"  note {name}: {secs:.3f} s is {raw:.2f} bars at {bpm:g} bpm — "
              f"not bar-length; read as {bars}")

    meaning = {"version": 1, "bpm": bpm, "bpmFrom": bpm_from, "beats": beats,
               "bars": bars, "swing": swing, "secs": secs}

    out = os.path.join(TAPES, f"{name}-tape")
    os.makedirs(out, exist_ok=True)
    wav = os.path.join(out, f"{name}-tape-01.wav")
    if os.path.lexists(wav):
        os.remove(wav)
    os.symlink(src, wav)

    region = [{"start": 0.0, "end": secs}]
    rj = os.path.join(out, ".regions.json")
    json.dump(region, open(rj, "w"))
    meas = makeset.run_json([makeset.MSM, "onset", "--json", "--regions", rj, src])["regions"][0]
    os.remove(rj)
    env, _ = makeset.envelope(src, 4000)
    dec = makeset.decays(env, secs, region)[0]

    x = hits.mono(src)
    slices = bars * beats * 4
    h = hits.score(x, slices, swing)

    doc = {
        "version": 1, "name": f"{name}-tape", "take": name,
        "made": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
        "kind": "tape", "stereo": layers[0].get("channels", 2) > 1, "sliced": False,
        "slots": 0, "slotSecs": 0, "voltsPerLevel": 10, "spec": None,
        "tape": meaning,
        "listened": {"source": layers[0]["file"]},
        "schedule": [],
        "samples": [{
            "file": os.path.basename(wav), "cell": [], "start": 0.0, "end": round(secs, 6),
            "peak": meas["peak"], "rms": meas["rms"], "zcr": meas["zcr"], "tilt": meas["tilt"],
            "decay": round(dec["decay"], 6), "floor": round(dec["floor"], 6),
            "means": [], "notes": [],
            "hits": {**h, "slices": slices, "method": hits.METHOD, "swing": swing},
        }],
    }
    json.dump(doc, open(os.path.join(out, "set.json"), "w"), indent=1)
    json.dump(meaning, open(os.path.join(tdir, "tape.json"), "w"), indent=1)
    kicks = sum(1 for v in h["kick"] if v >= 0.5)
    print(f"  {name}-tape: {bars} bar{'s' if bars != 1 else ''} at {bpm:g} bpm "
          f"({bpm_from}), swing {swing:g}, {slices} slices, {kicks} kick-like")
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("take", nargs="?")
    ap.add_argument("--bpm", type=float)
    ap.add_argument("--beats", type=int, default=4)
    ap.add_argument("--bars", type=int, default=0)
    ap.add_argument("--swing", type=float, default=0.5)
    ap.add_argument("--all-bars", action="store_true",
                    help="every take whose set was kept as `bars`")
    ap.add_argument("--all", action="store_true",
                    help="every take that says what it is as a tape (tape.json), plus --all-bars")
    a = ap.parse_args()
    bpm, frm = (a.bpm, "declared") if a.bpm else (120.0, "assumed")
    names = []
    if a.all_bars or a.all:
        for d in sorted(os.listdir(SETS)):
            p = os.path.join(SETS, d, "set.json")
            if os.path.exists(p):
                j = json.load(open(p))
                if j.get("kind") == "bars" and os.path.isdir(os.path.join(TAKES, j.get("take", ""))):
                    names.append(j["take"])
    if a.all:
        for d in sorted(os.listdir(TAKES)):
            if os.path.exists(os.path.join(TAKES, d, "tape.json")) and d not in names:
                names.append(d)
    if a.all or a.all_bars:
        pass
    elif a.take:
        names = [a.take]
    else:
        sys.exit("name a take, or --all-bars")
    for n in names:
        project(n, bpm, frm, a.beats, a.swing, a.bars)
    print("now: audio/build-corpora.py, and restart SuperDirt to load the banks")


if __name__ == "__main__":
    main()
