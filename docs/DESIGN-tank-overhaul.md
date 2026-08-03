# The Tank overhaul — the chyron as the one editing surface

*Design spec, 2026-08-03. Converged with AC over a design conversation. Supersedes
the collect-to-tank half of `DESIGN-vetula-chyron-redesign.md`. Retires the Tank
panel, `CatchChord`/catch-to-tank, the standalone Preview button, and task #10
(progression-building UX). Timing is explicitly deferred to its own session (§8).*

---

## 1. The finding that drives everything

The audition **chyron already collects every chord you play**, which makes the
**Tank redundant as a collector**. But the Tank is *not* purely redundant, and the
reason is the crux of this whole design:

**Explore and Revoice both need a per-chord harmonic *reading* — a `{ voicing,
anchor }` — that the chyron currently throws away.** A `ChyronEvent` today is
`{ pcs, notes, label, at }`; a `Specimen` (the Tank's unit) is `{ voicing, bass,
label, provenance, anchor }`. `specToNode` literally comments "carry the tank
reading back onto the surface." So the Tank's surviving value is that it *preserves
the reading* the chyron drops — which is exactly why the current flow catches to
the Tank *before* exploring.

**The reading must be per-chord, not per-progression.** You can audition a chord in
one modal centre and the next in an entirely different scale, and both land in the
same buffer. A progression can legitimately span scales. If the reading lived at the
buffer level it would be a lie the moment two scales meet. So each chord
self-describes: its `voicing` and `anchor` travel *with it*.

**The fix is therefore: enrich the event.** Carry `{ voicing, anchor }` per chord on
the `ChyronEvent`, and — because a `SavedSeq`'s events *are* `ChyronEvent`s —
enriching the event enriches the saved two-glyph token **for free**. Once the reading
travels with the chord, the Tank has nothing left to hold, and it is deleted.

## 2. Target architecture (the spine)

- **Enrich the event** with per-chord `{ voicing, anchor }` (§7) — the foundation
  everything else rests on.
- **The chyron becomes the one working *and* editing surface** (§3): collect ·
  select · explore · reorder · unbundle-into · rebundle. It can **expand into an
  "edit mode"** for revoicing/reordering (§3.5).
- **Explore re-points its seeds** from the Tank to the chyron *selection* (§4). The
  engine is *already multi-seed* — no new engine work.
- **The revoice panel stays a per-chord fine-tuning drill-in** (§5), invoked on one
  chord from the chyron — NOT a second arranger.
- **The Tank panel is deleted** (§9), freeing the top-right float-card slot.
- **Timing is its own later session** (§8).

## 3. The chyron as the one surface

Today the chyron is a thin capture ticker. It grows into the single surface for the
whole compositional loop:

- **Collect** — unchanged: every audition auto-logs (the tape).
- **Select** — click-to-select a chord; **shift-click extends the range from the
  previous selection** (Mac text-editing semantics). This makes the buffer a proper
  selectable list.
- **Explore around the selection** — §4.
- **Reorder** — drag-and-drop within the buffer.
- **Unbundle** — drag (or otherwise route) a saved two-glyph token *into* the chyron
  to re-edit it (§6). This *replaces* the working buffer ("open replaces the working
  document"); warn/save-first if there's unsaved capture.
- **Rebundle** — bundle the buffer (or a selection) into a **new** two-glyph token
  (§6).

### 3.5 Edit mode (AC, 2026-08-03)

When arranging (reorder / revoice / multi-select), the thin capture ticker can
**expand into a fuller "edit mode" view** — same surface, more room — rather than
forcing the work into a one-line strip. This keeps a *consistency of focus* (you
never leave the chyron to arrange) and is a natural use of the **lower strip freed by
dropping the bottom voice bar** (ties into the MIDI-flow chyron, #18). Not mandatory,
but the preferred shape.

## 4. Explore — a re-point, not a rebuild

Explore (`LensGenerate`) is **already multi-seed**: hitting explore with N chords in
the Tank blooms N neighbourhoods, one relative-ring per seed, with a `shake` re-roll
"around each tank seed". (Confirmed live — four tank chords → four exploded rings.)

So "explore around a selection of chyron chords" is **just changing the seed source**
from Tank specimens to the chyron selection. Each selected chord seeds its own
neighbourhood (`specToNode` → `focusId` → `spawn Extend`, per seed); `shake` carries
over unchanged. Newly explored chords **append to the chyron buffer** (you're growing
it; reorder afterwards).

## 5. The revoice panel — per-chord fine-tuning

The existing `revoiceModal` stays a **per-chord** tool: invoke it on one selected
chord to do the delicate voicing work (the ladders). It is explicitly **not** promoted
to a whole-progression editor — arranging (reorder / select / explore) is the
chyron's job, so there is exactly **one** arranger. The revoice panel is the leaf of
the tree, reached from the chyron.

## 6. Bundle / unbundle, and the re-hash worry

A two-glyph token is content-addressed (its glyph is a hash of its chords). Editing
changes the content → a **new** glyph. This is fine *and* non-confusing **in the
unbundle model**: you explicitly *unbundled* the old token to get into the chyron, so
rebundling naturally mints a **new** token. The old one is already out of the working
buffer — it either sits untouched in the store (delete if you want) or was never
saved. There is no silent mutation. Additive, always; no overwrite issues.

## 7. The enrichment (step one — the foundation)

- **`ChyronEvent`** gains the per-chord reading: `voicing` (the voiced notes — note
  `notes` may already serve) and `anchor` (the harmonic reading). Possibly the scale
  context if `anchor` doesn't fully capture it. Populated at the two audition
  choke-points (`playChord` / `playSpecimen`), which have the source chord's reading
  in hand.
- **`SavedSeq`** inherits it automatically (its events are `ChyronEvent`s), so saved
  two-glyph tokens carry the reading — "persist it into the glyph-marked
  progressions", as AC put it.
- **Lepidoptera / recall** (`Vetula.Lepidoptera`, and `mkSavedSeq` / `boxSpec` in
  `App`) currently serialise sources as **note-lists only** (`[c5,e5,g5]` brackets).
  A **recalled** token therefore has notes but no reading. Decision needed: either
  **persist the reading into the scene document** too (extend the source grammar), or
  accept **anchor re-derivation** from pcs+key on recall. This is the one remaining
  place the per-chord reading could still be dropped.
- **Verification is by ear at the rig**: the test is "does exploring from a chord I
  just played land on the *right* neighbourhood?" This is why enrichment is done at
  the desk, not blind.

## 8. Timing — deferred to a dedicated session

Human timing is where the expressivity lives (the Autechre/Chopin difference), so it
gets its own session rather than a bolt-on. Two anchors for that session:

- **The chyron is a *tape* until you touch it, then a *list*.** The `at` timestamps
  are the performance; once you select-and-reorder you are *arranging*, and
  arrangement is *order*, not timestamps. Reordering a list never scrambles
  timestamps because timestamps stopped being the timing model. (Explored chords have
  no `at` at all — which confirms the buffer is a list, not a tape, the moment you
  explore into it.)
- **Quant-layer-first (AC).** A progression with no recorded human timing gets a
  **mandatory, non-deletable quantisation layer** — timing must come from *somewhere*,
  and if it isn't recorded human timing it's the grid. This is **already half-built**:
  the Perform box's mini-notation *is* that quant grid. The session's job is to make
  the quant layer a first-class element of a voice, with recorded human timing as an
  *optional richer layer on top* — having our cake and eating it.

## 9. What this deletes

- **The Tank panel** (the `TANK & PROGRESSION` float-card's Tank half) — frees the
  top-right slot.
- **`CatchChord` / catch-to-tank** (and `CatchTriad` / `CatchNode`) — the chyron is
  the collector.
- **The standalone Preview button** — "play" lives where you audition (direct
  audition, and the progression plays from the editing surface). Keep `previewChan`
  (the audition *destination channel* — a separate concept).
- **Task #10** (progression-building UX) — this *is* its resolution.

## 10. Execution sequence

1. **Enrich `ChyronEvent`** with per-chord `{ voicing, anchor }`; populate at the
   audition choke-points; confirm `SavedSeq` carries it. Verify by ear (explore lands
   right). ← *the first commit; everything rests on it.*
2. **Chyron selection** — click / shift-click range.
3. **Explore re-point** — seed from the chyron selection instead of the Tank; append
   results.
4. **Reorder** — drag-and-drop in the chyron; **edit-mode** expansion (§3.5).
5. **Unbundle / rebundle** — token ↔ chyron; new glyph on rebundle.
6. **Delete the Tank panel, catch-to-tank, Preview.**
7. **Recall/Lepidoptera reading** — decide persist-vs-re-derive (§7).
8. **Timing session** — later (§8).
