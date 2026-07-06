# Stellatus audio path — SuperDirt via the OSC bridge

Stellatus is **rig-only** (no browser audio, by design).

> **STATUS (2026-07-06): the shipping BEAM path has LANDED.** Stellatus now runs
> BEAM-authoritative: the browser pushes a `stellatus-scene <json>` over the rig
> WebSocket and `reef_stellatus_voice` (purerl-tidal) generates + emits
> `/dirt/play` to SuperDirt, Link-locked and OSC-bundle-timetagged. The browser
> is a pure visualizer. **`stellatus-bridge.mjs` below is now RETIRED** — it was
> the dev-audition scaffold (browser → HTTP → bridge → UDP OSC → SuperDirt), kept
> here only for reference. `superdirt-daemon.scd` / `boot-superdirt.sh` are still
> live: SuperDirt itself is the sound engine either way.

For the record, the retired dev-audition route was:
**browser → HTTP → bridge → UDP OSC → SuperDirt**. Two small daemons, both
Bosun-ready.

```
  Stellatus (browser)          stellatus-bridge.mjs         superdirt-daemon.scd
  ┌──────────────────┐  POST   ┌───────────────────┐  OSC   ┌──────────────────┐
  │ ◉ SEND  → fire() │ ──────► │ :57130 /play      │ ─────► │ SuperDirt :57135 │
  │ per fired slice  │  JSON   │  → /dirt/play      │  UDP   │  scsynth + samples│
  └──────────────────┘         └───────────────────┘        └──────────────────┘
```

## Boot (two terminals, or background them)

**1 — SuperDirt** (boots scsynth, loads Dirt-Samples; ~10-20s; grabs the default
audio output). Wait for `STELLATUS-SUPERDIRT READY on port 57135`:

```sh
cd .../triggerfish
./audio/boot-superdirt.sh
# or:  SUPERDIRT_PORT=57135 /Applications/SuperCollider.app/Contents/MacOS/sclang audio/superdirt-daemon.scd
```

**2 — the bridge** (no audio, no deps — node built-ins only):

```sh
node audio/stellatus-bridge.mjs
# GET http://127.0.0.1:57130/health  → { ok, sent, superdirt }
```

**3 — in the browser**: open STELLATUS, hit **▶ RUN** then **◉ SEND**. Each slice
the walk lands on POSTs one `/dirt/play` (s, n, begin, end, speed, gain, orbit,
cps). Edit the `place`/`slice` line and it re-places live; `⟳ SHAKE` re-rolls the
warps + walk.

Ports: SuperDirt on **57135** (NOT 57120 — es9-daemon owns that on the MBP),
bridge intake on **57130**. Override via `SUPERDIRT_PORT` / `BRIDGE_PORT`.

## Verify the pipe without the browser

```sh
node audio/stellatus-bridge.mjs --selftest          # dumps the OSC bytes for one hit
curl -X POST 127.0.0.1:57130/play -H 'Content-Type: application/json' \
  -d '{"s":"808bd","n":3,"speed":1,"begin":0,"end":1,"gain":1,"orbit":0,"cps":0.5}'
# → you should hear an 808 kick if SuperDirt is up
```

## Bosun / Quartermaster — what's ready and the gaps

Both daemons already satisfy the `bosun-daemon` skill's contract:

- **port-from-env** — `SUPERDIRT_PORT`, `BRIDGE_PORT`.
- **readiness** — SuperDirt prints READY + binds UDP (`lsof -nP -iUDP:57135`);
  the bridge answers `GET /health`.
- **drain-on-signal** — `boot-superdirt.sh` traps TERM/INT and kills the process
  group (so scsynth doesn't orphan); the bridge closes cleanly on SIGTERM.
- **prebuilt artifact** — no build step; the bridge has no dependencies.

**Documented gaps (for a future Bosun/Quartermaster levelling pass):**

0. **Ports should be ALLOCATED BY BOSUN, not hardcoded** (AC, 2026-07-06 — the
   canonical mechanism). The right flow: a service *programmatically requests* a
   port from Bosun, which allocates it and writes the row into
   `ShapedSteer/bosun/registry/fleet.json` (the single source of truth,
   denormalised from Marginalia by the chair-server; `resolvePort` / `writeFleet`
   already live in `chair-server/src/Bosun/ChairServer/IO.purs`). Every consumer
   — the `.scd` (via env), the bridge (via env), the browser (its bridge URL) —
   then reads its port from that allocation instead of a literal. Current state
   is a **stopgap hardcode** (I picked 57135 after finding link-spike on 57122).
   **Two concrete gaps:** (a) no clean "request-a-port-and-record-it" call is
   wired for a shell script / node daemon to hit at boot — needs a Bosun
   endpoint (or CLI) + the two consumers reading it back; (b) the **SC-side
   `startup.scd` gotcha**: sclang runs the user's global
   `Platform.userAppSupportDir/startup.scd` FIRST, which auto-boots SuperDirt on
   the default **57120** and wins the race over our daemon's env port. So a
   Bosun-allocated port only "sticks" if either our daemon reliably overrides
   (stop-and-restart-on-our-port — flaky here) OR the global startup.scd stops
   auto-booting SuperDirt (leave it to our daemon) OR we simply adopt 57120 as
   SuperDirt's recorded allocation. Pragmatic near-term: **record 57120
   (SuperDirt, already bound + SC convention) and 57130 (bridge) in fleet.json**,
   and have the bridge + browser read the SuperDirt port from there.

1. **superdirt is now supervised (DONE); a fleet.json row is optional.** `superdirt`
   is a stage-0 service in `ShapedSteer/bosun/fixtures/atlantis/compose.yml` (boots
   `boot-superdirt.sh`, :57120), raised under `bosun supervise` and GREEN in the
   Chair. **Finding: the Chair draws supervised nodes from the group's compose +
   `/state`, NOT from fleet.json** — so no fleet.json row is needed to *see* it (the
   earlier assumption here was wrong; fleet.json is the `bosun serve` router's
   registry). A Marginalia/fleet row is still worth adding for documentation +
   port-collision-avoidance, but it isn't what makes the node appear. The dev
   `stellatus-bridge` is retired and needs no row.
2. **Quartermaster host pre-flight** doesn't yet check for SuperCollider /
   Dirt-Samples / the SuperDirt+Vowel quarks. Add a `verify` check:
   `sclang` present, `~/Library/Application Support/SuperCollider/downloaded-quarks/{SuperDirt,Dirt-Samples}`
   exist.
3. **Bosun group (DONE for superdirt).** `superdirt` is in the Atlantis group
   (`:3994`) alongside the other rig services. The scsynth-child pgid teardown is
   the one non-standard bit (handled in `boot-superdirt.sh`; Bosun's process
   executor should model it as a process-group, per the supervision-substrate
   steer). **Finding: `bosun supervise`'s INITIAL bring-up FORCE-RESTARTS the whole
   group** (`kill -pgid` then relaunch), it does NOT adopt already-running
   processes — so restarting the supervisor to add a service bounces the live rig.
   Use `bosun supervise --held` (boot down, raise deliberately from the Chair) to
   add/roll a service without a full-group bounce.
4. **es9-daemon co-existence** — SuperDirt on 57135 avoids es9-daemon's 57120,
   but both open CoreAudio; on the rig, confirm scsynth uses an output device
   that doesn't fight es9-daemon's ES-9 claim (likely fine — different devices).
