# Spec — Workstream C: the real-SuperDirt OSC output path (purerl-tidal)

*Hand-off spec, 2026-06-29. A self-contained work order for a Claude session
**scoped to `purerl-tidal`** (music/live-coding/purerl-tidal). Parent context:
`triggerfish/docs/PLAN-lepidoptera-routing-superdirt.md` (workstream C) and
`PRESETS-AND-THE-EDSL.md`. The Triggerfish webapp side (routing UI, the SuperDirt
instrument) is **out of scope here** — this is only the BEAM-side capability that
makes real SuperDirt audio reachable.*

## Goal

Give purerl-tidal a **real-SuperDirt audio output path**: a pattern routed to the
SuperDirt alias emits full Dirt-protocol OSC (`/dirt/play` with the param bag) to
a running SuperCollider/SuperDirt instance — alongside, not replacing, the
existing es9-daemon CV/gate path.

## What already exists (recon, 2026-06-29 — verify before building)

- purerl-tidal **owns OSC-out**: `tidal_dispatcher.erl` is a gen_server owning OSC
  + MIDI bridge sockets; it opens an `OscClient` against `gatePort` (default
  `57120`) via `tidal_oSC@foreign:startClient` when gate output is enabled, and
  "sends OSC out the bridgeClient."
- But it's a **singleton client → `127.0.0.1:57120` = es9-daemon** (`CvRouter
  "127.0.0.1" 57120` in `src/Studio.purs`; mirrored in `src/Calypso/Prelude.purs`).
  The **router alias is currently informational** ("all OSC goes through the
  singleton OSCClient opened against the default host:port"); the code itself
  flags the fix as a planned **"PR 2c.2 — per-alias OSCClient map."**
- **`s` is reduced to typed MIDI** today: `tidal_session_walker.erl:309` /
  `Studio.purs:91` — "Tidal/SuperDirt per-orbit `s`-keyed lookup, *ported to typed
  MIDI*." So **no real-SuperDirt audio path exists yet.**
- **es9-daemon is the OSC *receiver*** on 57120 (a Dirt-target impersonator
  driving CV/gate); it is **not** a SuperDirt sender. No new daemon is needed —
  SuperDirt is its own SuperCollider server.

## Tasks

### C1 — Per-alias OSC client map

Replace the singleton `OscClient` with a map keyed by **router alias**, so
different aliases reach different OSC endpoints. This is the already-pencilled
"PR 2c.2." Reference: `tidal_dispatcher.erl` (the `oscClient` field in state,
opened ~L186-197), `Studio.purs` `CvRouter`, `Calypso/Prelude.purs` `CvRouter`.

**Alias vocabulary (THE SEAM — fix these names; the Triggerfish routing layer
[workstream B] will set them):**
- `es9` → `127.0.0.1:57120` (existing CV/gate path, unchanged).
- `superdirt` → a real SuperDirt instance (host:port from C3).

A pattern/orbit declares its alias; the dispatcher routes to that alias's client.
Keep `es9` the default so nothing regresses.

### C2 — Emit full Dirt-protocol messages

For the `superdirt` alias, emit standard SuperDirt OSC — **not** the typed-MIDI
reduction. A `/dirt/play` message carrying the flat param bag: at minimum
`cps`, `cycle`, `delta`, `s`, `orbit`, plus whatever the event sets (`n`, `gain`,
`pan`, `cutoff`, `speed`, `begin`, `end`, …). Match the wire format upstream Tidal
uses (see `Sound.Tidal.Stream`'s OSC map / SuperDirt's message handler) so a stock
SuperDirt responds unmodified. The event already carries these params in the typed
model — this task is the **alias-conditional encode**: es9 alias → current CV/gate
encode; superdirt alias → `/dirt/play` bundle at the event's scheduled time.

### C3 — Deconflict port 57120

es9-daemon already binds `57120` on the MBP; SuperDirt also defaults to it — they
cannot coexist on one host. Decide (and document) the port layout:
- **Recommended:** SuperDirt stays on the conventional `57120`; **es9-daemon moves**
  (e.g. `57130`) — it's our own daemon, and every external Tidal tool assumes
  SuperDirt on 57120. Update the `es9` alias target (C1) + es9-daemon's listen
  port + any link-spike/launchd config that references it.
- *Alternative:* SuperDirt on a non-default port, es9-daemon unchanged — fewer
  ripples, but breaks the 57120 convention for any stock tooling.

Pick one, wire the `superdirt`/`es9` alias targets to match, and note the choice.

## The interface contract (what B will rely on)

- Alias names are **`es9`** and **`superdirt`** (extensible later).
- A source/orbit selects its alias; default `es9`.
- How the alias is carried in the eval/control message is yours to define **but
  must be a stable, documented field** — Triggerfish's routing layer will set it.
  Document the exact shape in this file when done so workstream B can match it.

## Gotchas (read before editing)

- **The purerl FFI rebuild trap.** `tidal_oSC@foreign` and friends: editing an FFI
  `.erl` whose module name ≠ filename does **not** reach the loaded beam via
  `make erl-quick` — only `make erl` (or `spago build` refreshing the output-erl
  copy) does. Symptom: your new OSC behaviour silently doesn't take. (See the
  project's `reference_purerl_ffi_rebuild_trap` note.)
- Calypso rides the same dispatcher — don't break the `es9` path or the singleton
  assumption Calypso currently depends on; the per-alias map must default to the
  old behaviour.

## Definition of done

- `spago build` + `make erl` clean.
- The `superdirt` alias emits well-formed `/dirt/play` OSC (verify the bytes /
  log the message even without audio).
- The `es9` path is byte-unchanged (no regression for Calypso / the CV rig).
- A **documented manual test recipe** for AC's rig: boot SuperDirt on its port,
  route a one-shot pattern to the `superdirt` alias, hear a sample. **Audio
  confirmation is AC's rig test** — a sandbox can't make sound; don't block "done"
  on it, but leave the recipe.
- The **interface-contract field** (how alias is selected) documented here for
  workstream B.

## Resolution (2026-06-29) — what was built

Implemented in **purerl-tidal**. `spago build` + `make erl` clean; the
`/dirt/play` wire format was byte-verified (see recipe); the `es9` OSC
output is unchanged (same FFI senders, same bytes — only its *port*
moved, by design).

### The interface-contract field (what workstream B sets)

The alias is carried as a **`PrimAction` constructor**, exactly like every
other emit kind (Gate / CV / MidiNote / …). A SuperDirt-routed source is a
binding whose action list contains:

```
Dirt { alias :: String, orbit :: Int }
```

- `alias` — the OSC client to reach. **`superdirt`** by default (opened at
  boot → real SuperDirt). `es9` is the CV/gate path. Extensible: any alias
  declared via `registerCvRouter` resolves; unknown aliases fall back to
  the `es9` client (so nothing silently drops).
- `orbit` — the SuperDirt orbit (output bus / FX chain).

It is set two ways, both stable and documented:

1. **Text / WS verb (the seam B will emit):**
   `bind <name> dirt <orbit> [<alias>]`
   e.g. `bind sd dirt 0` (alias defaults to `superdirt`),
   `bind sd dirt 2 superdirt` (explicit). Parsed by
   `Tidal.Binding.parseAction`; mirrored client-side in
   `tidal-protocol`'s `TidalProtocol.Binding` (parse + print round-trip)
   so editors (tidal-cli, browser) speak it too.
2. **Endpoint declaration (multi-rig / remote SuperDirt):**
   `registerCvRouter { alias, host, port }` — opens (or replaces) the OSC
   client for that alias at runtime via
   `tidal_dispatcher:register_osc_router/3`. `es9` (→ `127.0.0.1:57130`)
   and `superdirt` (→ `127.0.0.1:57120`) are opened at boot from app-env
   (`gateHost`/`gatePort`, `superDirtHost`/`superDirtPort`,
   `superDirtEnabled`); declaring an endpoint is only needed for extra
   targets.

The per-event param bag (`n`, `gain`, `pan`, `cutoff`, `speed`, `begin`,
`end`, `shape`, …) rides through unchanged via the existing
`#`-join → `Tidal.Sound.soundParams` path; the token is the SuperDirt `s`.
`cps`/`cycle`/`delta` are threaded per-event by `Tidal.Voice` (reserved
`_cps`/`_cycle`/`_delta` param keys, stripped before encode).

### C3 — port layout (chosen + wired)

SuperDirt keeps the conventional **57120**; **es9-daemon moved to 57130**.
Edits: es9-daemon `OSC_PORT` (+ README), link-spike `ES9_DAEMON_ADDR`,
purerl-tidal `gatePort` default + `Studio.cvRouter` + `Calypso.Prelude`
docstring. The `es9` alias and SuperDirt alias targets match.

### Manual test recipe (AC's rig — audio confirmation)

A sandbox can't make sound; this is the rig test.

1. **Boot SuperDirt** on the conventional port in SuperCollider:
   ```supercollider
   SuperDirt.start;   // listens on 57120 by default
   ```
   (es9-daemon, if running, is now on 57130 — no conflict.)
2. **Start purerl-tidal** (`deepstar up`, or the usual boot). At boot it
   opens the `superdirt` client → `127.0.0.1:57120`.
3. **Connect** a client: `wscat -c ws://localhost:3012/ws` (or Calypso).
4. **Route a one-shot to SuperDirt:**
   ```
   bind sd dirt 0
   sd "bd*4"
   ```
   You should hear the `bd` sample, 4×/cycle, on orbit 0.
5. **Exercise the param bag:**
   ```
   sd "bd sn hh sn" # gain "1 0.7 0.5 0.8" # n "0 1 2 0" # cutoff "800 1200"
   ```
6. **Confirm the es9 path still works** (no regression):
   `kick "bd*4"` still fires the ES-9 gate (now via 57130).

Wire-format check without audio (what the sandbox ran): a probe captured
the datagram and decoded `address=/dirt/play`, typetag
`,sssisfsfsfsf…`, args `s "bd" orbit 2 cps 0.5 cycle 12.0 delta 0.25
gain … n … cutoff …` — the stock SuperDirt key/value param bag.

## Out of scope

- The Triggerfish routing UI / capability switch (workstream B, the webapp
  session).
- The SuperDirt *instrument* (workstream D).
- Lepidoptera / preset format (workstream A).
