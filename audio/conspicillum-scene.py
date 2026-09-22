#!/usr/bin/env python3
"""Build a Conspicillum scene from a real Quadrat set, and push it to the rig.

A scene is the WHOLE definition of a cloud — corpus, query, cloud spec, seed —
and it crosses the wire ONCE. After that the BEAM is the sole audio authority
(`reef_conspicillum_voice`) and any surface is a pure visualizer recomputing
the same pure function. That is why this script is allowed to be a script: it
is the frontend's job, done in fifty lines, and nothing about the rig depends
on it being a nice one.

    python3 conspicillum-scene.py --set chord-hits-0916-185508 --chord Dm --print
    python3 conspicillum-scene.py --set chord-hits-0916-185508 --chord Dm --push

Set names: anchor-car is chord-hits-0916-185508, leaf-cloud is
chord-hits-0915-232247 (and 0915-230332, which carries the same chords through
a different patch — see Marginalia 289 note 687).
"""
import argparse, json, os, sys, urllib.request

SETS = os.path.expanduser("~/.itajara/quadrat/samples")
WS = "ws://127.0.0.1:3012/ws"

# Pitch classes, root and bass — root given explicitly because a realised
# Harmonia chord is SORTED and its root cannot be recovered from the set.
# The vocabulary these sets actually contain, read off their recorded notes.
# They are NOT common-practice material: anchor-car and leaf-cloud sit around
# E / B / F# / G and are full of minor-major sevenths, augmenteds and
# half-diminisheds. Asking them for a D minor would be asking for something
# neither of them has, and the harmonic cut would correctly return nothing —
# which reads as a broken instrument rather than as an honest answer.
#
# Roots are explicit because a realised Harmonia chord is SORTED and its root
# cannot be recovered from the set.
CHORDS = {
    "Bm":      {"pcs": [11, 2, 6],     "root": 11, "bass": 11},  # anchor-car 1, leaf-cloud 20
    "EmMaj7":  {"pcs": [4, 7, 11, 3],  "root": 4,  "bass": 4},   # anchor-car 0, leaf-cloud 17/18
    "F#aug":   {"pcs": [6, 10, 2],     "root": 6,  "bass": 6},   # anchor-car 6, leaf-cloud 21
    "F#m7b5":  {"pcs": [6, 9, 0, 4],   "root": 6,  "bass": 6},   # anchor-car 10
    "G":       {"pcs": [7, 11, 2],     "root": 7,  "bass": 7},   # anchor-car 8/11/12
    "D":       {"pcs": [2, 6, 9],      "root": 2,  "bass": 2},   # leaf-cloud 14
    "E":       {"pcs": [4, 8, 11],     "root": 4,  "bass": 4},   # leaf-cloud 0
    "Dm":      {"pcs": [2, 5, 9],      "root": 2,  "bass": 2},   # in NEITHER set — the honest-empty case
    "any":     None,
}



def corpus_of(name):
    """A Quadrat set as a Conspicillum corpus.

    Samples with no recorded notes are kept rather than filtered: `fit` scores
    them zero, so the harmonic cut removes them and says why. Dropping them
    here would renumber `index` — and `index` IS the SuperDirt `n`, so the
    cloud would play different audio than it named.
    """
    d = json.load(open(os.path.join(SETS, name, "set.json")))
    samples = []
    for i, s in enumerate(d["samples"]):
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


def scene(setname, chord, onsets, sustain, seed, minfit, strength, every):
    harmonic = []
    if CHORDS.get(chord):
        harmonic = [{"target": CHORDS[chord], "minFit": minfit, "strength": strength}]
    rules = []
    if every:
        # Every Nth grain reversed — the figure no hardware granulator can
        # express, because there a grain has no identity to count.
        rules.append({"when": 1, "everyN": every, "everyK": 0,
                      "chance": 0.0, "op": 0, "amount": -1.0})
    return {
        "corpus": corpus_of(setname),
        "query": {"clauses": [], "weighting": [], "harmonic": harmonic},
        "spec": {
            "onsets": onsets,
            "cloud": {"sustain": sustain, "position": 0.35, "spray": 0.5},
            "rules": rules,
            "speed": 1.0, "gain": 0.85, "pan": 0.5, "accelerate": 0.0,
        },
        "seed": seed,
    }


def even(n):
    return [round(i / n, 6) for i in range(n)]


def push(text):
    """Send one verb over the rig WebSocket. Raw frames, no dependencies."""
    import base64, socket, struct
    host, port = "127.0.0.1", 3012
    s = socket.create_connection((host, port), timeout=10)
    k = base64.b64encode(os.urandom(16)).decode()
    s.send((f"GET /ws HTTP/1.1\r\nHost: {host}:{port}\r\nUpgrade: websocket\r\n"
            f"Connection: Upgrade\r\nSec-WebSocket-Key: {k}\r\n"
            f"Sec-WebSocket-Version: 13\r\n\r\n").encode())
    buf = b""
    while b"\r\n\r\n" not in buf:
        buf += s.recv(4096)
    payload = text.encode()
    hdr = b"\x81"
    n = len(payload)
    mask = os.urandom(4)
    if n < 126:
        hdr += bytes([0x80 | n])
    elif n < 65536:
        hdr += bytes([0x80 | 126]) + struct.pack(">H", n)
    else:
        hdr += bytes([0x80 | 127]) + struct.pack(">Q", n)
    masked = bytes(b ^ mask[i % 4] for i, b in enumerate(payload))
    s.send(hdr + mask + masked)
    s.settimeout(6)
    try:
        return s.recv(4096)[:200]
    except Exception:
        return b"(no reply)"
    finally:
        s.close()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--set", default="chord-hits-0916-185508")
    ap.add_argument("--chord", default="Bm", choices=sorted(CHORDS))
    ap.add_argument("--grains", type=int, default=16, help="onsets per cycle")
    ap.add_argument("--sustain", type=float, default=0.12)
    ap.add_argument("--seed", type=int, default=12345)
    ap.add_argument("--min-fit", type=float, default=0.5)
    ap.add_argument("--strength", type=float, default=0.9)
    ap.add_argument("--every", type=int, default=0, help="reverse every Nth grain")
    ap.add_argument("--print", action="store_true")
    ap.add_argument("--push", action="store_true")
    ap.add_argument("--stop", action="store_true")
    a = ap.parse_args()

    if a.stop:
        print(push("conspicillum-stop"))
        return

    sc = scene(a.set, a.chord, even(a.grains), a.sustain, a.seed,
               a.min_fit, a.strength, a.every)
    js = json.dumps(sc, separators=(",", ":"))

    n = len(sc["corpus"]["samples"])
    voiced = sum(1 for s in sc["corpus"]["samples"] if s["notes"])
    print(f"{a.set}: {n} samples, {voiced} with notes | chord {a.chord} "
          f"| {a.grains} grains/cycle of {a.sustain}s | scene {len(js)} bytes",
          file=sys.stderr)

    if a.print:
        print(js)
    if a.push:
        print(push("conspicillum-scene " + js))


if __name__ == "__main__":
    main()
