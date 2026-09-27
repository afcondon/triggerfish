# The rig's SuperDirt

SuperDirt is the rig's sample engine. It is played BEAM-authoritatively: a page
pushes a whole scene over the rig WebSocket, and a purerl-tidal voice emits
`/dirt/play` to SuperDirt, Link-locked and OSC-bundle-timetagged. The browser
only draws.

```
  Conspicillum (browser)    purerl-tidal :3012                 SuperDirt :57120
  ┌────────────────────┐    ┌──────────────────────────┐  OSC  ┌──────────────────┐
  │ conspicillum-scene │ ─► │ reef_conspicillum_voice  │ ────► │ scsynth + samples│
  │ (one push a change)│ WS │ cycleOf, per cycle       │  UDP  │ BlackHole 16ch   │
  └────────────────────┘    └──────────────────────────┘       └──────────────────┘
```

Its outputs, with `SUPERDIRT_DEVICE=BlackHole` and `SUPERDIRT_OUTPUTS=6`, are
1/2 dry, 3/4 send A (orbit 10), 5/6 send B (orbit 11); they are heard through
Ableton.

`superdirt-daemon.scd` and `boot-superdirt.sh` boot it headless; Bosun
supervises it in the Atlantis group (`superdirt`, stage 0). Stellatus, the ring
re-sequencer that first played SuperDirt from here, and its HTTP-to-OSC dev
bridge were retired on 2026-09-27; Conspicillum replaced them.

## Bosun / Quartermaster — what's ready and the gaps

The SuperDirt daemon already satisfies the `bosun-daemon` skill's contract:

- **port-from-env** — `SUPERDIRT_PORT`.
- **readiness** — SuperDirt prints READY + binds UDP (`lsof -nP -iUDP:57120`).
- **drain-on-signal** — `boot-superdirt.sh` traps TERM/INT and kills the process
  group (so scsynth doesn't orphan).
- **prebuilt artifact** — no build step.

**Documented gaps (for a future Bosun/Quartermaster levelling pass):**

0. **Ports should be ALLOCATED BY BOSUN, not hardcoded** (AC, 2026-07-06 — the
   canonical mechanism). The right flow: a service *programmatically requests* a
   port from Bosun, which allocates it and writes the row into
   `ShapedSteer/bosun/registry/fleet.json` (the single source of truth,
   denormalised from Marginalia by the chair-server; `resolvePort` / `writeFleet`
   already live in `chair-server/src/Bosun/ChairServer/IO.purs`). Every consumer
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
   port-collision-avoidance, but it isn't what makes the node appear.
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
