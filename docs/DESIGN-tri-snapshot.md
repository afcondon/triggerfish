# Tri-snapshot — one snapshot taker across all three Balistes tabs

**Marginalia #182 (model) + #199 (UI). Status: design + model foundation.**

## The ask (AC, 2026-07-12)

> The primary thing I want to do with the UI is to make the snapshot taker
> work for all three types (Mutable, Grids, Tidal) so that we can freely
> sequence them intermingled using the Macro Tidal composition idea. I think
> this means making one unchanging pane that manages the snapshots in all
> three views.

Today the snapshot bank is **Mutable-only**. A snapshot captures the Grids
control point (X/Y + densities + randomness + open + push), lives *inside*
`Balistes` (the Grids model), renders *inside* the Grids pattern panel, and the
sequence is replayed only in the `AGrids` branch of the `Step` loop. Switch to
the GRIDS (AFixed) or TIDAL (ASelene) tab and the whole snapshot machinery is
gone.

The goal: **one persistent pane**, the same in all three tabs, whose slots can
hold a snapshot of *whichever* brain was active when it was taken — and whose
sequence can march through Mutable, Grids and Tidal snapshots **intermingled**.
That is the macro-tidal surface: arrange whole drum-machine states as a song.

## What a snapshot must now capture

A snapshot has to restore the full *playing* state of one brain, so recall can
switch the active tab and hand off to the rig. One constructor per brain:

```purescript
-- Triggerfish.Balistes.TriSnapshot
data TriSnapshot
  = TSGrids Snapshot            -- the Mutable control point (today's Snapshot)
  | TSFixed P.FixedPattern      -- the whole rhythm (self-contained, not a library index)
  | TSTrig M.TrigBank           -- the whole POLYTRIG rack (jacks + routes)
```

Design choices:

- **Store the artefact, not a reference.** `TSFixed` carries the whole
  `FixedPattern`, `TSTrig` the whole `TrigBank` — not a `library !! i` index.
  A snapshot must survive library edits and reordering (the same discipline the
  reef pushes already use: `encodeFixed`/`encodeTrigKit` send the whole thing).
  `TSGrids` keeps today's `Snapshot` (knob state) — the Grids engine
  regenerates its pattern from that, so it's the natural restorable unit.
- **Mode is in the constructor.** Recall reads the constructor to know which tab
  to switch to and which handoff verb to push.

## Where the state lives

Snapshots span brains now, so they no longer belong to `Balistes` (the Grids
model). Move them to component `State`:

```purescript
-- was: bal.snapshots :: Array (Maybe Snapshot), bal.sequence, bal.seqBars
, snapshots :: Array (Maybe TriSnapshot)   -- the bank (State, not bal)
, sequence  :: Array Int                    -- path of slot indices
, seqBars   :: Int
```

Removing `snapshots`/`sequence`/`seqBars` from `Balistes` also simplifies the
`BalSim` handoff (they were already frontend-only; `balSimOf` never sent them).

## Capture

`captureTri :: State -> Maybe TriSnapshot` reads the active brain:

```
AGrids     -> Just (TSGrids (M.captureSnapshot s.bal))
AFixed i   -> TSFixed <$> (s.library !! i)          -- the live rhythm
ASelene    -> Just (TSTrig s.trig)
```

## Recall — switch tab + restore + (Rig) hand off

`recallTri` is the heart of the change. It sets `active` to the snapshot's
brain, restores that brain's state, and — when rig-authoritative — pushes the
matching handoff so the rig follows:

```
TSGrids snap -> active := AGrids; bal := applySnapshot snap bal; pushHandoff
TSFixed pat  -> active := AFixed <ephemeral>; play pat; balistes-fixed push
TSTrig  rack -> active := ASelene; trig := rack; balistes-trig push (pushTrig)
```

The rig side is **already ready**: `reef_balistes_voice` swaps mode in place on
any of `balistes-sim-at` (→ grids), `balistes-fixed` (→ `set_pattern`),
`balistes-trig` (→ `set_kit`). So a mid-sequence Mutable→Tidal→Grids march is
just three pushes; the one running voice re-modes each time. No restart, no gap.

Open question for AC: **where does a recalled `TSFixed` live in the library?**
Two options —
  (a) *ephemeral*: play the snapshot's pattern without adding a library entry
      (a transient `AFixed` backed by a scratch slot). Cleanest for sequencing.
  (b) *materialise*: append/replace it into the library and select it. Pollutes
      the library during playback.
Recommend (a): a snapshot is a frozen artefact; playing it shouldn't mutate the
library. Needs a small `activeFixedPattern :: Maybe FixedPattern` escape hatch
on State so `AFixed` can be backed by a snapshot rather than a library index —
OR generalise `active` to `AFixed (Either Int FixedPattern)`. TBD with AC.

## Playback — mode-agnostic sequence advance

Today the advance lives in the `AGrids` step branch. Lift it to the top of
`Step` (before the per-mode `case`), so the sequence marches regardless of the
visible tab:

```
Step tick -> do
  advance the sequence if a boundary elapsed → recallTri the new slot
  then the existing per-mode emit runs on the (possibly just-switched) brain
```

The boundary math (`bar`, `seqBars`, `seqStartBar`, `seqPos`) is unchanged; only
its location and what it recalls (a `TriSnapshot`, not a Grids slot) change.

## UI — the persistent pane

- Render `snapshotSection` for **all three** `active` values, not just Grids —
  lift it out of the Grids pattern panel into a slot the shell always shows
  (candidate: a strip along the bottom of the transport panel, or a fixed
  right-hand rail present in every tab).
- Each filled slot shows a **brain badge** (M / G / T) + a mode-appropriate
  glyph: the mini X/Y pad for `TSGrids` (today's rendering), a tiny lane
  heatmap for `TSFixed`, a jack-count chip for `TSTrig`.
- CAPTURE / SEQ-BUILD / PLAY / bars-per-step controls unchanged in behaviour;
  they just operate on the `State` bank now.
- Hainbach × Rams: keep the restrained panel; the badge is a small engraved
  letter, not a coloured pill. (Cosmetic pass is the tail of #199.)

## Persistence

The bank + sequence persist to localStorage alongside the pattern library
(`Triggerfish.Balistes.Store`). `TriSnapshot` needs a JSON codec — simple-json
generic on the three-constructor ADT (or reuse the reef codecs for the Fixed /
Trig payloads).

## Slices

1. **Model** — `Triggerfish.Balistes.TriSnapshot` (the ADT + `captureTri` shape
   + pure `applyTri`), no wiring yet. *(this slice: foundation only)*
2. **State move** — bank/sequence/seqBars from `bal` → `State`; delete the Grids
   `Snapshot` bank fields; fix `defaultBalistes`/`balSimOf`.
3. **Recall + playback** — `recallTri` (tab-switch + restore + rig push); lift
   the sequence advance to the top of `Step`.
4. **UI** — persistent pane across all tabs + per-brain slot rendering + badge.
5. **Persist** — codec + Store round-trip.
6. **Cosmetic** — Hainbach/Rams pass over the pane (folds into the broader #199
   three-tab UI pass).

Slices 3 and 4 are the ones that want AC's eye (the recall-switches-tab feel;
the pane's home in the layout). 1–2 are low-regret and can land first.
