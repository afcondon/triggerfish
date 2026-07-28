# A unified scene / arrangement modal across all panels

*Design note, 2026-07-28. Forward-looking — the organizing target for an
upcoming workflow design session, NOT a build plan. Builds directly on
[`PRESETS-AND-THE-EDSL.md`](PRESETS-AND-THE-EDSL.md) (the spine) and comes out of
returning to the Balistes ARRANGE work after ~2 weeks away and finding it
confusing — the honest usability signal that the shape wants a rethink, not a
patch.*

## The vision (AC, 2026-07-28)

> There's really a lot to be said for making all the scene — and possibly
> arrangement — setting into a **modal that operates on all panels**. Structure
> it to match the panels: **Odonus, Balistes, Selene, Vetula, Sufflamen,
> Stellatus**. Then the **Tidal pane is purely for seeing the source.**

One surface, identical everywhere, for capturing / naming / recalling / (maybe)
sequencing the state of each machine — instead of the per-machine, per-shape
notions that grew independently. The Tidal page stops being a place you *edit*
and becomes a read-only mirror of the eDSL the modal and panels produce.

This is the same principle `PRESETS-AND-THE-EDSL.md` already fixed:
**consistency does not mean identical machinery everywhere — it means the
differences live in *what an item is*, never in *how it's named, saved, loaded,
or shipped*.** The modal is the concrete home for that "how."

## The central workflow tension (the thing the session must resolve)

> The workflow needs to be thought out carefully, especially between **fast and
> easy creation of presets** and **slower creation that forces naming.**

Two creation speeds, both legitimate, in tension:

- **Fast / performance-time** — capture the current state into a slot *now*, no
  dialog, no name, keep playing. This is today's Balistes CAPTURE→slot and
  Odonus SCENES. Anonymous, positional, disposable.
- **Deliberate / authoring-time** — promote a configuration into a *named*
  library item you'll reach for again, ship to Calypso, or share. Forces a name;
  earns permanence.

The `PRESETS-AND-THE-EDSL.md` cut names these precisely: **scenes/snapshots**
(the fast, "how it's set now", for recall + sequencing) vs **library items**
(the deliberate, authored *content*, "what to play"). The modal has to make both
first-class *and* make the promotion path from one to the other obvious —
without making the fast path slow. That balance is the design problem.

## Where we are today — the fragmentation to unify

Each machine grew its own answer, which is exactly what the modal collapses:

| Machine | Scene/preset today | Persistence |
|---|---|---|
| **Balistes** | ARRANGE rail: tri-snapshot bank (M/G/T) + sequence (`Snapshot.purs`, `TriSnapshot.purs`) | ✅ localStorage, **eDSL-text** (`Store.purs` v3, 2026-07-27) |
| **Odonus** | `View/Scenes.purs` (SCENES) | ✅ `Store.purs` |
| **Selene** | rack library | ✅ `Store.purs` (eDSL doc `{name, doc}`) |
| **Vetula** | non-persistent "library" | ✗ |
| **Sufflamen** | — | ✗ |
| **Stellatus** | — | ✗ |

So: three different scene notions, three that have none, and one (Balistes) that
also carries a *sequence* on top. The modal is where these converge onto one
vocabulary and one storage discipline.

### Completed this pass (2026-07-27 → 28)

- **Balistes ARRANGE persistence (slice 5)** — `TriSnapshot` gained a text codec
  (`printTri`/`parseTri`, brain-tagged; `TSFixed` reuses Lepidoptera
  `printPattern`), and `Store` grew to a v3 envelope `{library, bank, sequence,
  seqBars}` saved on every rail edit, restored on init (stopped). **This moved
  Balistes onto the eDSL-text discipline** the presets note asked for — closing
  the "serialises to custom JSON, not the eDSL" divergence it flagged.
- **Recall highlight fix** — a recalled GRIDS snapshot now lights the library
  chip matching by name (playback still from the frozen `scratchFixed`), so the
  switcher agrees with what's sounding.

## Two live bugs/smells that are really design symptoms

1. **Stop doesn't stop — two transports.** The ARRANGE rail has *two* gates:
   **Run** (the voice, `sounding`: Silent/Local/Rig) and **▸PLAY/❚❚STOP** (the
   sequence, `seqEnabled`). `advanceSeq` needs both; ❚❚STOP halts the *advance*
   but the voice keeps looping the last-recalled snapshot, so you must *also*
   kill Run to silence it. No relabeling fixes this — the modal must decide **what
   "stop" means for an arrangement** (one gate or two; does stopping the sequence
   silence the voice, freeze on the current scene, or return to a base state?).

2. **Cross-instrument `Widgets` smell.** `Balistes.Widgets` reaches into
   `Odonus.Grid.Widgets` — a shared scene surface wants a *shared* widget layer,
   not one instrument importing another's.

## Open questions for the design session

1. **Transport / playback model** — what Run vs Play/Stop mean once scenes are
   universal; whether arrangement (sequencing scenes) is per-machine or a global
   layer above all machines.
2. **The surface** — modal vs rail; is it one modal with a section per machine
   (matching the panels), or a modal scoped to the active machine? How does it
   subsume Odonus SCENES and the Balistes ARRANGE rail without regressing them?
3. **The fast↔named workflow** (the crux above) — the gesture vocabulary for
   anonymous capture, for promotion-to-named, and for making both obvious.
4. **Tidal pane → read-only source** — confirm it becomes a pure eDSL mirror;
   what that removes from the per-machine panels.

## Reading list for the session

- [`PRESETS-AND-THE-EDSL.md`](PRESETS-AND-THE-EDSL.md) — **the spine**: library-item
  vs scene, eDSL as the one format, "consistency ≠ identical machinery".
- [`BALISTES-SELENE-RETHINK.md`](BALISTES-SELENE-RETHINK.md) — one-idea-per-instrument
  decomposition (context for what each panel *is*).
- [`DESIGN-macro-tidal.md`](DESIGN-macro-tidal.md), [`DESIGN-tri-snapshot.md`](DESIGN-tri-snapshot.md)
  — the arrangement-as-song surface Balistes already prototypes.
- Per-machine prior art: `Odonus/View/Scenes.purs`, `Balistes/Snapshot.purs`,
  the three `Store.purs`.
