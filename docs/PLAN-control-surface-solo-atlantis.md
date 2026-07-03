# Plan — Control Surface: SOLO ⟷ ATLANTIS

## The problem (AC, system architect, 2026-07-03)

The control surface has grown past what can be held in one head — the architect's
included. Today a user faces: **four push-to-backend buttons** (push Odonus, push
Balistes, push Vetula, → BRUSH), in-app playback controls, the Ableton transport,
and the **interacting hidden state** between all of them. Diagnosing "why isn't
this playing / tracking?" means reasoning about invisible couplings (a rig restart
silently drops `reef_voice`; Odonus's source isn't live-synced; browser co-sim vs
rig voice; brush vs perf; 0/1 channel index).

**Goal:** collapse this toward **one authority-mode toggle plus transparent
auto-sync**, and make every remaining hidden state either explicit or eliminated.

## The core principle: one AUTHORITY per mode

The confusion comes from frontend and backend being *ambiguously, sometimes* both
authoritative. Make it binary and explicit — two modes, and the mode names *who is
in charge of sound and animation*:

- **SOLO** — the **frontend** is the authority. It runs the engines locally and
  plays **direct to a MIDI sink (Ableton) via Web MIDI**, animating from its own
  sim. Triggerfish as a standalone playable instrument (Odonus, Balistes) and
  Vetula as a standalone harmonic explorer. No backend involved.
- **ATLANTIS** — the **backend (rig)** is the authority. The rig runs the engines
  and makes the sound (MIDI/OSC via link-spike / es9); the frontend **animates in
  lockstep and emits no sound of its own**. Triggerfish as a frontend to the
  Atlantis system — the animations on screen are, in effect, *played from the
  backend* to match the sound coming from the backend.

**One big top-nav toggle flips the authority.** That flip is the entire mental
model. Crucially, in ATLANTIS the frontend is *explicitly a synced view, not a
second independent player* — which dissolves the "two Odonuses" (browser co-sim vs
rig voice) confusion at the root.

## The top nav — make mode + harmonic context ALWAYS visible

The current centre label `TRIGGERFISH · ODONUS` is **redundant** with the
instrument switcher on the right (which already shows the selected instrument).
Reclaim that space for the two things that should be visible *at all times, in
every pane*:

```
┌───────────┬───────────────────────────────────────────┬────────────────────────────────┐
│ ▶ PLAY    │   Triggerfish   SOLO│ATLANTIS   o-oo---o-o-o │ ODONUS BALISTES SELENE VETULA … │
└───────────┴───────────────────────────────────────────┴────────────────────────────────┘
     ↑                    ↑              ↑                              ↑
 transport         mode toggle    harmonic-context strip        instrument switcher
                 (+ live state)   (progression + playhead)
```

- **Mode segment** — the `SOLO ⟷ ATLANTIS` toggle *and* indicator in one. The mode
  is never hidden state; you always see which authority you're in.
- **Harmonic-context strip** — a compact glyph of the current progression with a
  playhead (`o-oo---o-o-o`: chords + dwell/skip, active chord marked). It is **live
  in every pane**, so wherever you are, you see the progression stepping. This is
  the lockstep palette animation surfaced globally — the one thing everyone shares.

## Lockstep co-sim IS "played from the backend"

In ATLANTIS the frontend does not run an *independent* thing; it runs the **same
deterministic sim in lockstep, muted**. Because it is byte-identical to the rig,
*what you see == what you hear from the backend*. So "animations played from the
backend" is satisfied with snappy local rendering and **no per-frame wire**. This
is the model already proven in `reef/docs/PLAN-lockstep-cosimulation.md` (the
Standalone vs Rig-attached authority table); SOLO/ATLANTIS is that model given a
name and a button.

### The animation boundary IS the palette/brush boundary

The one subtlety (AC): the **Tidal-pattern brush runs only on the rig** — we
deliberately kept Tidal out of the browser. So the animation splits exactly on the
palette/brush line (`reef/docs/PLAN-vetula-palette-brush.md`):

- **Palette (definition) — SEEN, in lockstep.** Progression, dwell/skip timeline,
  which chord is active, voice-leading discs, Odonus/Balistes playheads. All
  pure-of-pulse, the frontend's own data → animates with **zero backend events**.
- **Brush (realization) — HEARD, backend-only.** The individual arp/pattern
  note-onsets are produced by the rig's Tidal engine, which the frontend can't
  reproduce. So you **hear the arp but don't see its individual notes fire** — and
  that's fine, because what you need to see is the *progression*, which you do.

- **Future option (reserved, not built):** a thin **fired-notes event stream** from
  the rig could drive a note-level *overlay* — event-driven, imperceptible latency
  (as with Odonus), layered on top of the lockstep palette animation. Design the
  event channel to *allow* it; don't build it now.

So within ATLANTIS: **lockstep for everything the frontend owns (the palette + the
co-sim engines); heard-only for the brush's note realization; event-driven overlay
reserved for arp notes later.**

## Transparent auto-resync — the frontend always knows WHEN

Transparency is only tolerable if the sync is *maintained* so the user never has to
know it's there. It can be, because **the frontend is the source of the definition,
so it always knows when the definition changed.**

- In ATLANTIS, any change to a backend-authoritative definition (progression,
  voicings, dwell, Odonus/Balistes config, seed, tempo, Odonus source) marks the
  state **dirty → auto-sync, debounced**. The user never presses "push."
- **Two mechanisms already exist and auto-choose:**
  - structural change → **phase-aligned full re-push** (`reef-sim-at`,
    `vetula-voicings`)
  - incremental live edit → **tick-tagged input** (`reef-input`), applied in
    lockstep
- Both are **glitchless**: the engines are pure functions of the absolute pulse and
  pushes phase-align — seamless live re-voice is already proven on the rig.
- Entering ATLANTIS does **one initial full sync** of everything.

## One concession to visibility: a passive SYNC LIGHT (not a button)

Transparency ≠ invisibility. A small indicator — **in-sync / syncing /
disconnected** — gives *awareness without action*. The user never *acts* on sync,
but can *see* it's healthy, which is what makes an automatic system trustworthy
rather than spooky. This is the antidote to "transparent but frustrating."

## Auto-heal — this is what kills the invisible-state traps

Each trap observed while building the brush maps to a fix here:

| Trap (today) | Fix (this design) |
|---|---|
| Rig restart / WS drop silently loses `reef_voice`; nothing plays | Frontend detects the drop (heartbeat) and **auto-resyncs everything on reconnect** — restart self-heals |
| Odonus source (scale vs Vetula) not live-synced | It's part of the definition → changing it **auto-resyncs**; no manual re-push |
| Browser co-sim vs rig voice ("two Odonuses") | ATLANTIS makes the frontend a *synced muted view* — one perceived voice |
| Brush (`vetula-voicings`) vs Perf (`vetula-perf`) parallel paths | **Retire `vetula-perf`**; the brush is the one Vetula→rig path |
| MIDI channel 0-index (browser) vs 1-index (link-spike) | Normalize once at the boundary; **UI is 1-indexed everywhere** |

## Playback collapses too

The clock is Link, and the engines play whenever Link runs (pure-of-pulse). So
play/stop collapses to **follow Link (the Ableton transport) + one mute**. Drop the
per-instrument arm/play as separate transports. `▶ PLAY` in the nav is the single
transport affordance (and in ATLANTIS it simply reflects/controls Link).

## Granularity — later, and never as a new top-level button

Start **global**: one mode for the whole app. Per-instrument on-rig (e.g. Vetula on
the rig while Balistes stays SOLO) is a *secondary* control introduced later — a
settings panel or a small per-instrument toggle that defaults to "follow global."
The primary affordance stays **one button**.

## What collapses — before / after

**Before:** `▶ PLAY` · push-Odonus · push-Balistes · push-Vetula · `→ BRUSH` ·
per-instrument arm/play · Ableton transport · redundant centre label · several
hidden couplings.

**After:** `▶ PLAY` (follows Link) · **`SOLO ⟷ ATLANTIS`** toggle · a passive
**sync light** · the centre label repurposed as the **live harmonic-context strip**.
Four push buttons → gone. `vetula-perf` → retired.

## Decisions locked (2026-07-03)

- Names: **SOLO ⟷ ATLANTIS**.
- ATLANTIS animation source: **lockstep co-sim** (built, proven) — not a pure
  event-driven view.
- **Global mode** now; per-instrument granularity deferred.
- Animation boundary = **palette/brush** (see the progression, hear the brush);
  fired-notes arp overlay reserved as a future option.
- Top-nav centre: **mode toggle + live harmonic-context strip** (kills the
  redundant `TRIGGERFISH · <instrument>` label).

## Implementation sketch (phases)

1. **Mode + nav.** Add a global `Mode = Solo | Atlantis` to Triggerfish shell
   state; render the top-nav centre strip (mode toggle + harmonic-context glyph),
   removing the redundant label. Wire authority: SOLO → local Web MIDI on; ATLANTIS
   → local MIDI muted, rig authoritative.
2. **Auto-resync.** Dirty-tracking on backend-authoritative definitions; debounced
   auto-sync in ATLANTIS (structural re-push vs tick-tagged input, auto-chosen).
   **Remove the four push buttons.**
3. **Sync light + heartbeat + auto-heal.** WS heartbeat; on reconnect, full
   resync. Passive in-sync/syncing/disconnected indicator.
4. **One Vetula path.** Retire `vetula-perf` / `reef_vetula_voice`; the brush
   (`vetula-voicings` / `reef_vetula_brush`) is the sole Vetula→rig path.
5. **Playback collapse.** Follow Link + single mute; drop per-instrument
   transports.
6. **Later:** per-instrument granularity; fired-notes event overlay for arp-note
   animation; configurable Vetula audition channel (task #71).

## Related

- `reef/docs/PLAN-lockstep-cosimulation.md` — the Standalone/Rig-attached authority
  model this names and buttons.
- `reef/docs/PLAN-vetula-palette-brush.md` — the palette/brush boundary that the
  animation boundary follows.
- Memory `feedback_unify_control_surface` — the traps this design kills.
