# Rig issues — found 2026-08-07, first session against the full Atlantis group

The working list from the first run of Triggerfish against the real modular rig
with the whole Atlantis group up (`MILESTONE-2026-08-07-rig.md` is the milestone
this session tested). Eleven issues; two fixed on the day.

Numbering is stable — refer to issues by number, they do not get renumbered as
items are closed.

## The shape most of these share

Nine of the eleven are the same failure mode, and it is worth naming because it
is what made the session expensive:

> **The rig reports healthy while a load-bearing thing is dead.**

`state` says `voices:[]` while a voice is emitting. `--silent` reports success
while LFOs keep running. `release-claim` returns `OK` having sent nothing to the
hardware. link-spike sits at zero peers with no error, free-running at 120 bpm.
Triggerfish comes up in Solo and every publish goes nowhere. In each case a
*positive* signal was returned for a thing that had not happened.

The lesson for the pre-flight checklist (below): **a check that cannot observe
something must report UNKNOWN, never OK.** Absence of evidence got reported as
evidence of absence repeatedly today, and each time it sent the search in the
wrong direction.

---

## Fixed on the day

### #9 — Triggerfish boots into Solo after every reload — FIXED

A good default for a webapp in isolation, wrong for a member of the Atlantis
fleet: a reload silently re-routes everything into the browser, publishes go
nowhere, and the rig looks dead. Cost a substantial detour before it was spotted.

**Fix:** `Triggerfish.Transport.Store` — localStorage persistence of the
authority `Mode`, saved on `SetMode`, loaded at Initialize. Serialised as a
tagged string so an unknown tag from another build degrades to the Solo default
rather than decoding into the wrong authority.

**Deliberately not persisted: `armed`.** `mode` is a *preference* — the user's
choice about how this browser relates to the rig, which nothing on the backend
can contradict. `armed` is a *claim about what is running*, and its truth lives
in the BEAM. Restoring it across a reload — especially across a BEAM restart,
which wipes every voice — would have the UI assert something false. That is the
same desync that makes a dead rig look live. It also cannot be reconciled today,
because per #1 `state` cannot see four of six voice trees, so there is nothing
trustworthy to check a restored `armed` against. Revisit only after #1.

This matches the discipline already in the sibling stores, which restore the
scene grid and macro lanes but never `sceneRun` / `macroOn`.

### #10 — Balistes did not advance at all in Atlantis — FIXED

`Step` was gated in its entirety on `sounding == Local`, so in Atlantis nothing
ran: no playhead, no model advance, no flash. `Triggerfish.Transport`'s own
docstring states the intent — *"Atlantis — the RIG is authoritative; the frontend
is muted but keeps its schedulers running (lockstep animation)"*. Muted and
stopped are different things and the code did the second.

**Fix:** the gate moved from wrapping the whole handler to wrapping only the
three `for_ st.midiOut` emit blocks. `playStep`, `flash`, `bal` and
`nextModelStep` now advance in both modes; only MIDI output is muted in Atlantis.

Note the consequence, which is intended: the browser now co-simulates the Grids
model alongside the BEAM voice, so `PushBalistes` stamps a `nextModelStep`
derived from a model that has been running in parallel. That is what lockstep
handoff requires; if the two ever drift, the handoff carries the drift.

Confirmed on the rig — playhead advances silently in Atlantis, and stop works.

---

## Fixed later the same day

### #12 — The rig link dies after 30 minutes and nothing notices — FIXED

Found when AC returned after an hour away to a rig that was playing and would
not answer Triggerfish at all, PANIC included. The same symptom as most mornings.

purerl-tidal's cowboy handler sets `idle_timeout => 1800000` (`Handler.erl:28`),
and cowboy counts idle from the last frame it RECEIVED — the anchors it streams
to the browser do not reset it, only traffic the other way does. So a browser
left untouched for half an hour is hung up on. Overnight blows past it every time.

What made it invisible rather than merely annoying:

  * `Binnacle.purs` had `onClose: pure unit` — the close handler did nothing at
    all. No reconnect, no flag, no notice.
  * `Transport.js`'s `send` is `if (ws.readyState === 1) ws.send(msg)` — a send
    on a closed socket is a silent no-op.

So the UI went on looking alive while controlling nothing, which is this
document's opening theme in its purest form: a positive signal returned for
something that did not happen. It also explains why a BEAM restart had been
costing a tab reload all day — every socket died with it and none came back.

**Fixed** in three layers:

  * `Socket` became a DURABLE HANDLE owning a mutable inner WebSocket, re-dialling
    with exponential backoff to a 15s cap. `Socket` is an opaque foreign type, so
    this took zero call-site changes and every machine on Binnacle gets it.
    `onOpen` runs on every successful connect, so `clock-subscribe` is re-sent and
    the clock returns by itself.
  * A keepalive: `state` every 10 minutes against the 30-minute timeout, so the
    drop should not happen at all. Reconnect makes it survivable; the keepalive
    means the reconnect window is not where the PANIC press lands.
  * `Transport.isConnected`, polled on SyncTick and drawn as a red tilted
    DISCONNECTED stamp across the SOLO/ATLANTIS toggle — in both modes, since
    every rig verb rides that socket and someone flipping INTO Atlantis should see
    the link is dead before handing over authority.

Sends are still silent no-ops while down. The gap is now seconds and visible
rather than permanent and invisible.

## Open

### #1 — Four of six voice trees are unstoppable and invisible — THE BIG ONE

`purerl_tidal_sup` starts six voice supervisors. `hush` reaches two of them plus
five `reef_*` singletons:

| supervisor | reached by `hush`? | visible to `state`? |
|---|---|---|
| `tidal_voice_sup` | yes | yes (via dispatcher) |
| `odonus_voice_sup` | yes | no |
| `balistes_voice_sup` | **no** | no |
| `repetitor_voice_sup` | **no** | no |
| `virtual_selene_voice_sup` | **no** | no |
| `selene_pattern_voice_sup` | **no** | no |

`balistes_voice_sup:stop_voice/1` exists, is exported, and has **zero callers**
anywhere in the codebase. `balistes_voice` handles exactly two casts —
`{compute_until, Window}` and `{set_config, Cfg}` — so it has no stop or hush to
receive even if one were sent. Meanwhile `tidal_clock:181` broadcasts step
windows to `balistes_voice_sup` every tick, so the voice keeps playing.

Independently, `tidal_state_pub` gathers from only `tidal_clock` and
`tidal_dispatcher`. So `voices:[]` means "the dispatcher holds no bindings", not
"nothing is playing" — the two got conflated during the session and sent the
investigation to Ableton and SuperCollider.

**Consequence:** a voice can emit forever, unreachable by every UI action and by
`hush`, killable only by restarting the BEAM, while the diagnostic reports clean.
Observed live: channel-10 drum notes with `voices:[]`, stopped only by bouncing
purerl-tidal.

**Still unknown:** which tree the stuck voice was actually in. It was NOT
Balistes-as-routed (none of Balistes' three machines were connected to the FH-2;
the router assigns ch 10 and the FH-2 filters ch 10 in hardware, which is a
separate point worth remembering). A minimal repro would name it.

**Fix shape:** wire `balistes-stop` to the existing `stop_voice`; extend `hush`
to sweep all six trees via `which_voices()` + `stop_voice` (these voices have no
hush cast to receive); add the four trees to `tidal_state_pub` so an emitting
voice is visible.

### #2 — Deleting Selene channels and publishing releases nothing

Publish is additive-only: an empty publish sends no release, so previously
installed claims keep running. They are persisted to `~/.es9/claims.json` and
restored across daemon restarts, so bouncing the daemon does not clear them
either. Three were live during the session — `selene:es9:main`, `selene:es9:gt0`,
`selene:es9:cv0`, 24 generator slots — surviving every UI action.

Selene polysignals are *autonomous*: once applied they run inside es9-daemon's
audio callback or the FH-2's own clock/LFO engine. Nothing in the transport is
driving them, so stopping the transport cannot stop them.

### #3 — The FH-2's `release-claim` silences nothing

`Daemon.purs:434` rewrites the claim table and persists it, then returns. It
never calls `applyTransform`, never encodes, never sends SysEx. es9-daemon's
implementation calls `.deactivate()` on every destination the owner held, so
there releasing *is* silencing. Same verb name, same wire form — the Rust
comment even says "matches fh2-daemon" — and opposite effect on hardware.

### #4 — `--silent` is not silent

It sends `silent-baseline-config.syx` **paired with `working-preset.syx`**.
Clocks live in `Config.clocks` and do die; LFOs live in `Preset.lfos` and get a
*working* preset loaded over them. There is no silent preset fixture in
`fixtures/` at all. So a command documented as "nothing on any jack across main
+ both expander banks" leaves the LFOs running.

### #5 — Neither daemon has a silence/panic verb

es9-daemon's control socket has six verbs: `ping`, `version`, `bpm`,
`list-claims`, `release-claim`, `apply-polysignal`. No stop, silence or panic.

The machinery already exists on the FH-2 side: `silenceOtherFamilies "" bank`
(`PolyBank.purs:215`) runs *every* family's silencer when passed `""` as the
active family, and the macro path at `Daemon.purs:570` already uses it exactly
that way. It just is not exposed as a verb.

`scripts/rig-panic.mjs` (in this repo) is the stopgap written during the session:
enumerate-and-release on the ES-9, silence + `reload` + restore-drums on the FH-2.
**It predates the discovery of #4 and #5, so it does not fully silence the FH-2.**
Its one durable lesson is documented in the script: `reload` after `--silent` is
mandatory, not hygiene — `--silent` writes the hardware from a separate process,
leaving the daemon's cached Config holding the noisy state, so the next apply
pushes the noise straight back.

### #6 — `fh2-drumkit` never co-restarts with `fh2-daemon`

64 restarts against 0 during the session. The Atlantis compose comment claims the
member is "D-E5-co-restarted (re-applied) whenever the daemon bounces". It is not.
The drum breakout is applied once at boot and never again, so once the FH-2 loses
its trigger MCVs nothing puts them back — Balistes drums go silent while ch-10
notes still visibly arrive at the FH-2.

**Durable workaround, independent of the bug:**
`node scripts/apply-drum-breakout.mjs --save` persists the four MCVs to the FH-2's
own flash, so they survive a powercycle and stop depending on Bosun or the daemon
lifecycle at all.

### #7 — fh2-daemon has a silent-exit restart loop

`Daemon.purs:297`: if the boot config dump fails to decode, it logs
`✗ initial config decode failed` and falls out of the `else` branch **without ever
binding the socket**. The process then runs out of work and exits; Bosun restarts
it. 64 times over the session, still climbing at the end, succeeding
intermittently.

Two defects: the failure path should retry the dump rather than exit, and
`nohup >` truncates `/tmp/bosun-apply-fh2-daemon.log` on every restart, so the
evidence of *why* it failed is destroyed each cycle.

### #8 — `MacroTick` churns `PushLane` while stopped

`Main.purs`: `handleAction PushLane` sits outside the `when (st.macroOn && ...)`
guard, so a child query fires every 120 ms regardless. Harmless, but it makes the
comment four lines above it — "MacroTick is a no-op while the sequencer is
stopped" — false, and a false comment about exactly this is what a future
debugging session will trust.

### #11 — link-spike cannot survive Ableton restarting

Its `rusty_link` session goes stale when the peer disappears and never recovers:
zero peers indefinitely, no error, free-running at 120 bpm — so the rig plays on,
silently unsynced. It had been up 2.5 hours across an Ableton quit/restart and
never re-paired; a fresh process found Live in under a second and immediately
tracked its tempo (121 → 131 → 137 → 139 bpm).

Compounding it: `[peers]` is logged **only on change**, so a wedged session looks
byte-identical in the log to a healthy idle one.

Confirmed alongside: bouncing link-spike via Bosun's Chair does **not** cascade to
purerl-tidal, so it is a cheap recovery move mid-session — voices survive.

---

## Dead ends — recorded so they are not re-derived

**`basegate = output - 1` is not an off-by-one.** It looks like one (the McvSpec
comment says FHX-8GT is 65-128, and `set-drum-trig 0 36 65 10` writes 64), but it
is empirically verified — see `Main.purs:428`: *"verified empirically: basegate=65
produced output 66 / jack 2, so wire = output - 1 just like the `base` field"*.

**The Link failure was not a macOS permissions problem.** Local Network / TCC was
the initial hypothesis and it was wrong: the binary had not changed, macOS had not
been updated, and a plain restart fixed it. It was #11, a stale session.

---

## Toward the pre-flight checklist

The session's other conclusion: **a flat checklist is not enough.** The failures
are interrelated, so a list of independent checks lights up red everywhere
downstream of the real break, and Solo mode (#9) invalidates every rig-side check
beneath it while looking like a rig fault.

What is wanted is a **precondition DAG over the signal path**, where each check
declares what it depends on and the runner reports the topmost failure as FAIL
and everything beneath it as MASKED — one root cause instead of a cascade:

```
Link peer ─┐
           ├─ link-spike anchor emit ─ BEAM :57121 bound ─ BEAM clock
free-run  ─┘                                                  │
                                    voice exists in right tree ┤
                                          Solo vs Atlantis ────┤
                                              MIDI/CV emit ────┤
                                    FH-2 port live (device-status)
                                    device config armed (MCVs)
                                          physical / expander
```

Plus the coverage rule from the top of this document: every check declares what
it can observe, and one that cannot observe something reports UNKNOWN. DeepStar
must not derive "the rig is idle" from an instrument that structurally cannot see
most of it.

Every node above is a check that was run by hand during this session, so the
pre-flight already has its content:

- anchor port 57121 bound (`lsof`)
- link-spike peers > 0 when Live is expected
- FH-2 port live — `device-status` round-trip (this verb already exists and is
  documented as the preflight handshake)
- drum MCVs armed — MCV 0-3 `enable=1`, `vt=1`, note-filtered
- es9 / fh2 claims empty when nothing should be playing (`list-claims`)
- Triggerfish not in Solo
- BEAM voice trees empty — **blocked on #1**, since `state` cannot currently see
  four of the six
