# Plan — a portable package of virtual modules (engine + protocol), one source of truth

*Design, 2026-06-30. Resolves the architecture surfaced while planning the
Triggerfish→purerl-tidal connection: the instrument engines exist in **three**
drifted copies, and "purely declarative data down the wire" only stays coherent
if both ends share one definition. This is the [harmonia](../../../harmonia)
pattern one level up — a pure-PureScript library compiled to **both** the JS
frontend and the Erlang engine — applied to the instrument engines themselves.
Phase 0 (this doc) precedes any BEAM surgery.*

## The problem, grounded

Each virtual module exists as **three** implementations that have drifted:

| | Odonus | Balistes | Selene |
|---|---|---|---|
| **Triggerfish (PS/JS)** — the rich model + UI | `Triggerfish.Odonus.Model` (632 LOC; `renderCell`, `step`, **`stepEmit`**) | `Triggerfish.Balistes.*` | `Triggerfish.Selene.*` |
| **purerl-tidal (Erlang)** — hand-rolled BEAM engine | `odonus_engine.erl` + `odonus_voice.erl` | `balistes_*` + `balistes_tables.erl` | `selene_pattern_voice_sup.erl` |
| **purerl-tidal (PS→Erlang)** — partial shared logic already live | `tidal_odonus@ps` (`buildControlMap`, `evaluateParamsAtControls`) | — | — |

The drift is concrete on Odonus: the BEAM config knows only
`notes/skip/gate/glide` + `vel`/`dur_ms`; the **frontend** has, and the BEAM
**lacks**, per-cell `ratchet`/`dur`/`vel`, Euclidean (`pulses`/`esteps`),
polymeter (`offset`/`len`), the `ChordSeq`/McMullen quantiser, and
`dist`/`degShift`. "A lot of stuff in Triggerfish that isn't in the purerl-tidal
version."

**The decisive enabler:** `Triggerfish.Odonus.Model` already expresses the
engine as **pure functions** — `renderCell :: Odonus -> Head -> Cell -> Int`,
`step :: Odonus -> Odonus`, and the crown jewel
**`stepEmit :: Odonus -> { odo, fired :: Array Fired }`** (advance one step, here
is what fired). And `odonus_voice.erl` **already calls** `tidal_odonus@ps` — so
purerl-tidal is *already* running PureScript-compiled-to-Erlang for Odonus. We
are not introducing a mechanism; we are extending a live one from "some control
logic" to "the whole engine."

## The idea: one definition, two runtimes

A **portable package of virtual modules** — the pure engine core **and** the
protocol to drive them — that every consumer depends on:

- **Triggerfish** (JS backend) imports it for the editor + local preview.
- **purerl-tidal** (Erlang backend) compiles it via purerl and runs it in the
  voice gen_servers.
- **Calypso** (JS backend) — *eventually* (AC, 2026-06-30: "not right now"). The
  text-editor sibling will adopt the same `Reef.Protocol` + data path, so a cell
  and a Triggerfish knob drive the same typed protocol into the same BEAM voice.
  That a *third* consumer is already foreseen is the argument for the protocol
  living in the package, not in any one app.

A virtual module's *behaviour* becomes **one pure definition**; the editor and
the BEAM are two **runtimes** of it. Parity stops being a sync chore and becomes
**structural** — there is one `stepEmit`; drift is impossible because there is
nothing to drift from. This is the runtime-spine principle
([[project_runtime_spine_build_layer]] / [[project_three_dag_unification]])
proven on the instruments, and the anti-vendoring lesson of harmonia repeated
one layer up: harmonia is the *theory* layer; this package is the *engine* layer
that sits on it.

## Package shape

**One package, a module per instrument** (AC, 2026-06-30). Working name
**`reef`** *(placeholder — the habitat the triggerfish modules live in; AC to
bless; leans into the nautical theme of [[feedback_naming_at_scale]])*.

```
Reef.Odonus      -- Config, Runtime, stepEmit, renderCell, serialization
Reef.Balistes    -- (Phase 2)
Reef.Selene      -- (Phase 3)
Reef.Protocol    -- the wire vocabulary shared by sender + receiver
```

- **Depends on `harmonia`** (the theory layer — `realize`, McMullen, voicing).
  Clean stack: `reef` (engines) → `harmonia` (theory) → Prelude/Data.
- Pure `Prelude`/`Data.*` + `harmonia`, no `Effect`, no FFI — so it compiles
  under JS **and** purerl, exactly as harmonia does (proven this week).
- Consumed via the harmonia-proven mechanism: git-tag / path `extraPackages` in
  each workspace (resolves against each one's own package set).

## The two halves of the package

### 1. The engines (pure)

Per instrument, the **clean config/runtime split** — the rigor that makes
"declarative data down the wire" real rather than aspirational:

```purescript
-- Declarative. Serializable. THIS is what travels down the wire (Lepidoptera).
type OdonusConfig = { cells, heads-config, rootPc, scaleIvls, dist,
                      octaveShift, degShift, gatePct, chord-config }

-- Engine-held. Never sent — cursors, accumulators, phase.
type OdonusRuntime = { cursors, seqPositions, accumulators, chordIx, chordPhase }

stepEmit :: OdonusConfig -> OdonusRuntime -> { runtime :: OdonusRuntime, fired :: Array Fired }
```

Today `Odonus` mixes the two (e.g. `Head` carries config `transp`/`len`/`pulses`
*and* runtime `cursor`/`seqPos`/`accumulator`; `ChordSeq` carries config `period`
*and* runtime `ix`/`phase`). The Lepidoptera work already carves config out for
serialization (`capturePatch` excludes runtime fields); the extraction
**formalizes that split into the types**, which is what lets the wire carry only
`Config` and the engine own `Runtime`.

`Fired` is the emit vocabulary (note, velocity, gate length, ratchet, channel) —
backend-neutral; the BEAM turns it into MIDI/OSC, the frontend can render or
preview it.

### 2. The protocol (`Reef.Protocol`)

"The protocol to update them all in one place." One definition, shared by the
**sender** (Triggerfish) and the **receiver** (purerl-tidal `*_voice`):

- **Config push** — the declarative `…Config` for a named voice (the
  Lepidoptera form). "Here is the whole Odonus."
- **Live control** — the incremental update vocabulary (a knob twist →
  `set-control odonus.modN.<idx>`; mute; nav-mode; etc.), today an ad-hoc text
  protocol in `Handler.erl`. Promoting it into typed `Reef.Protocol` means the
  frontend can't send a control the engine doesn't understand, and vice versa —
  the wire is type-checked at both ends from one source.

Codec lives here too (the `…Config` print/parse = today's Lepidoptera), so the
on-wire form has a single canonical definition.

## How the consumers change

**purerl-tidal** — the voice gen_server becomes a thin **impure shell**:
- keeps clock subscription, MIDI/OSC emit, the control bus, supervision;
- calls `Reef.Odonus.stepEmit@ps` for the actual advance + fired events;
- **`odonus_engine.erl` is retired** (its logic now lives once, in `Reef.Odonus`);
- `tidal_odonus@ps` folds into `Reef.Odonus` (it was a down-payment on this).

**Triggerfish** — `Odonus.Model` becomes a thin layer over `Reef.Odonus` (or
re-exports it); the UI is unchanged; it gains nothing to maintain and loses the
private engine copy. It sends `Reef.Protocol` messages over the browser-direct WS
(the transport decided in the connection plan).

## Parity strategy

Parity is achieved **by construction**, not by diffing: once both ends call the
same `stepEmit`, the only way to add a feature is to add it to `Reef`, and both
runtimes get it. The remaining engine-specific work in purerl-tidal is the
**shell** (does the gen_server thread `Runtime` correctly, emit `Fired`
faithfully), not the musical logic.

## Phased rollout (rig-aware)

1. **Phase 0 — this doc.** Agree the boundary + shape before BEAM surgery.
2. **Phase 1 — Odonus, end-to-end** (the proof):
   - Create `reef` (`Reef.Odonus` + `Reef.Protocol`), `depends on harmonia`.
   - Lift `Odonus.Model`'s pure core in, **split Config/Runtime**, keep
     `stepEmit` as the contract. Port the existing model tests as the golden.
   - Triggerfish consumes `Reef.Odonus`; build + bundle; UI unchanged.
   - `odonus_voice.erl` calls `Reef.Odonus.stepEmit@ps`; retire
     `odonus_engine.erl`; **verify the rig still plays** (the careful step).
3. **Phase 2 — Balistes**, **Phase 3 — Selene**: same template; each retires a
   hand-rolled `.erl` engine and adds a `Reef.*` module.

## Open decisions

- **Name** (`reef`? — AC's call).
- **`stepEmit` signature for the gen_server**: `Config -> Runtime -> {Runtime,
  Fired}` is clean, but confirm it threads naturally through the BEAM's
  per-step `emit_step` and the lookahead loop (the gen_server holds `Runtime`
  between casts).
- **Protocol wire format**: the live-control half is text today (`Handler.erl`
  prefixed verbs). Keep text (typed *constructors* that render to the existing
  verbs, so the BEAM parser is untouched at first) vs. move to JSON. Lean: typed
  constructors → existing text verbs, so Phase 1 doesn't also rewrite the wire.
- **Config/Runtime boundary per instrument** — Odonus is mapped above; Balistes
  and Selene need the same pass in their phases.
