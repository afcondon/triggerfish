# PLAN — Selene → modular output (ES-9 / FH-2)

*Design, 2026-07-09. Task #142. Selene is now the CV/gate rack (POLYTRIG left
for Balistes, #141); this plan gives it an output path to the hardware. Cross-repo:
es9-daemon, fh2-config daemon, purerl-tidal (the WS bridge), Triggerfish (codec +
push UI).*

## The two findings that shape everything

1. **The bridge already exists — it's the rig WebSocket.** The browser never
   raw-OSCs modular. It sends verb-prefixed text frames to the Atlantis rig WS
   (`ws://127.0.0.1:3012/ws` = the purerl-tidal BEAM app), which owns every
   OSC/unix-socket client. Same path Odonus/Balistes/Stellatus already use. So
   there is **no new bridge daemon** — just one new verb + a unix-socket relay
   handler on the BEAM. This resolves the "browser can't reach modular" open
   question from PLAN-midi-routing.md.

2. **Selene is install-once config, not per-tick emit.** es9-daemon already hosts
   the polysignal generators (polylfo / polyclock / polyeuclid / polypresetnote),
   Link-locked to link-spike's `/link/anchor`. The browser configures once; the
   daemon generates the CV autonomously. No streaming, no Step-handler emit — a
   fundamentally simpler shape than the per-step MIDI voices.

## The shared wire: `apply-polysignal`

Both daemons take the **identical** command over a unix socket, line-protocol,
newline-terminated:

```
apply-polysignal <one-line-json>
→ OK apply-polysignal <alias> (family=… bank=… slots=8)
→ ERR apply-polysignal <alias>: capability — bank gt0 is Gate-only; polylfo needs Cv
```

- **es9** → `~/.es9/control.sock`, banks `main` (ES-9 jacks 1–8), `cv0` (ESX-8CV),
  `gt0` (ES-5 gates). `cv1-6`/`gt1-7` are unmapped today → return an ERR.
- **fh2** → `~/.fh2/control.sock`, banks `main`, `cv0-6`, `gt0-7`.

Envelope JSON:

```json
{ "bank": "main", "family": "polylfo", "alias": "selene:main",
  "outputRange": "bipolar5v", "slots": [ {…}, …8 ] }
```

The slot field names **already match `Triggerfish.Selene.Model` verbatim** — that
was the original design intent ("copied from Tidal.Selene to port onto the
apply-polysignal wire shape"):

| family | slot fields |
|---|---|
| `polylfo` | `rate phase level sin sqr tri saw rnd nse` |
| `polyclock` | `base`(wire token) `multiplier pulseWidth phase` |
| `polyeuclid` | `beats steps rate accentRate` (accentRate ignored by es9) |
| `polypresetnote` | `note` |

The model already has `clockBaseToWire` (→ `"quarter"`/`"8t"`/…) and `rangeToWire`
(→ `"bipolar5v"`/…), the only two non-obvious tokens.

## Topology

```
Triggerfish Selene (browser)
  │  Binnacle WS → verb:  selene-apply <es9|fh2> <envelope-json>
  ▼
purerl-tidal BEAM (Handler.erl)   ← generalize the existing fh2_daemon_call
  │  writes "apply-polysignal <json>\n", reads OK/ERR
  ├─► ~/.es9/control.sock   → ES-9 main / ES-5 gt0 / ESX-8CV cv0
  └─► ~/.fh2/control.sock   → FH-2 banks
  │  relays reply → verb:  selene-reply <es9|fh2> <bank> <OK/ERR …>
  ▼
Selene shows per-destination OK / claim-conflict / ERR
```

Mappings the browser owns:

- **Target → (socket, bank):** `ES9Main→(es9,main)`, `ES9Gt n→(es9,gt{n})`,
  `ES9Cv n→(es9,cv{n})`, `FH2 n→(fh2,<bank-for-n>)`. **Midi / Virtual are out of
  scope** for #142 — greyed as "not a modular target."
- **GenBank → family:** `GLfo→polylfo`, `GClock→polyclock`, `GEuclid→polyeuclid`,
  `GNote→polypresetnote`.

## Push model — explicit "Apply → rig" (decided 2026-07-09)

A button pushes all es9/fh2 destinations on demand and shows the per-bank reply
(OK / claim-eviction / ERR). Chosen over live auto-push because the daemon's claim
/ eviction semantics mean you want to *see* a conflict before forcing it. Live
debounced auto-push (with `apply-polysignal!` force) is a later refinement (S4).

Selene stays frontend-only (no BEAM voice); this is a config side-channel over the
rig WS, so it only does anything when Atlantis + the daemons are up. Standalone,
Selene remains the visual editor — honest and consistent.

## Build slices

- **S1 — BEAM relay** (`Tidal/WebSocket/Handler.erl`): add the `selene-apply
  <socket> <json>` verb; generalize `fh2_daemon_call` into a
  `daemon_call(SocketPath, Line) -> OK/ERR`; relay the reply back as
  `selene-reply`. Testable with a hand-sent WS frame → daemon `OK`, before any
  browser work. **purerl FFI rebuild caveat:** if the handler edit lands in an
  `.erl` whose module name ≠ filename, `make erl-quick` won't refresh the loaded
  beam — use `make erl` / full `spago build` (see the FFI-rebuild-trap memory).
- **S2 — browser codec** (`Triggerfish.Selene.Wire`): pure
  `destinationEnvelope :: Destination -> Maybe { socket :: String, bank :: String,
  json :: String }` (Nothing for Midi/Virtual). Reuses the model +
  `clockBaseToWire`/`rangeToWire`. Unit-testable, no I/O.
- **S3 — push + UI**: an "Apply → rig" action in the Selene transport strip that
  sends one `selene-apply` per es9/fh2 destination over the Binnacle socket, and a
  `selene-reply` handler surfacing OK/claim/ERR per bank. Build + bundle.
- **S4 — polish**: debounced auto-push on edit; `apply-polysignal!` force toggle;
  `release-claim` on rack switch / clear; grey the non-modular rows.

## Risks / notes

- **Rig-only.** Needs es9-daemon + fh2 daemon running; standalone Selene stays
  visual-only. Modular output inherently requires the hardware, so this is fine.
- **Bank coverage.** es9 supports only `main`/`cv0`/`gt0` today; offer just those
  or surface the ERR. Confirm fh2 bank tokens against `fh2-config/PolyBank.purs`.
- **Claims / aliases.** Use a stable alias per (socket,bank), e.g. `selene:main`,
  so a re-push replaces cleanly; if same-alias re-apply conflicts, S4's
  `apply-polysignal!` force + `release-claim` handle it.
- **Conformance untouched.** No reef/BEAM voice, no goldens — this is a config
  relay, not a scheduled voice.
