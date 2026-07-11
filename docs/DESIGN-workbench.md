# The Workbench — go-to shelf, preview, and the transform (mutation) heart

The TIDAL page is the **Workbench**: a curated shelf of saved setups feeding a
bench where one is picked, previewed, transformed, and committed back to its
instrument. It is the read/recall surface over [Amphora](../../amphora), the
content-addressed artefact store — the counterpart to the per-editor *save*
paths (Odonus scenes, Selene racks, Vetula progressions, Balistes patterns).

The guiding idea (from the design conversation, 2026-07-11): **it is a
preview-first mutation workbench, not a filing cabinet.** The primary verbs are
*audition* and *transform*, not *tag* and *search*. Curation is the point —
save-everything is cheap substrate, but the surface that matters is a small wall
of go-to setups (the "Suzanne Ciani has had the same notes in her sequencer
since the 1970s" model).

## Built (Triggerfish `main`, 2026-07-11)

- **S1 — layout.** `Triggerfish.Main.tidalView` reorganised into shelf (left) +
  bench (right); the raw-source aggregate demoted to a slide-out drawer
  (`⟨ source ⟩`) so the workbench owns the canvas. `commit → editor` reuses the
  existing `LoadFromLib` path. Commits `52e83a3`.
- **S2 — ★ go-to tier.** A cross-instrument `triggerfish-goto` favourite
  collection. The shelf leads with the ★ Go-to wall; the full per-instrument
  archive sits behind a `▸ dig` toggle. Star = publish content + favourite into
  `triggerfish-goto`; unstar = remove the favourite. **Membership is matched by
  payload equality** — content-addressing means same payload ⇒ same hash, so no
  hashing is needed in the shell and commit stays on its existing path. Commit
  `bb58ba0`.
- **S3 — preview (local audition).** Per-row `▶`/`■` toggle; several instruments
  can audition at once (hear a *combination*), one setup per instrument; header
  `■ stop all previews (N)`. Preview loads the setup into its editor and sounds
  **only that one instrument locally — the rig and the others are untouched.**
  Commit `d098176`.
- **Transport de-tangle (shipped with S3).** Preview is folded into the pure
  transport derivation: `soundingOf :: Mode → Set Which → Set Which → Which →
  Sounding` gained a `previewing` set, so a preview forces `Local` as *part of*
  the model rather than an out-of-band write. The old `SyncTick` poll-and-
  reconcile reverse edge was deleted; Vetula now raises an `ArmChanged` event on
  self play/stop/unload and the shell updates `armed` directly. Sounding is now
  one-directional (shell state → instruments). Commit `d098176`.

## S4 — Transforms: the mutation heart (NOT YET BUILT — own branch)

Deferred to a dedicated session on a feature branch (`workbench-transforms`).
This is the high-value part: **lift a saved idea into a context it was never
written for** — "how would this Odonus line sound at ¼ speed with F#2 in the
bass and a breakbeat under it."

### Core concept: a transform is a morphism

Applying a transform is `Content → Content` **plus** a recorded edge
`morphism(from: sourceHash, to: newHash, kind: "speed×½", params: …)`. Amphora
already has the `morphism` relation; this makes it load-bearing. The store then
*remembers* that the ¼-speed version derived from the original — the derivation
graph is exactly the Hylograph "frontispiece" from the Amphora design. You are
not editing the original; you are growing a family of variations off it.

A transform operates on the **recipe** (the eDSL / Tidal text), not by re-editing
the instrument — which is what makes it cheap. Two implementation levels:

1. *Structured (preferred):* parse → transform the typed model → print, reusing
   each editor's Lepidoptera parse/print. Principled, per-kind.
2. *Text-level:* manipulate the eDSL string. Generic but fragile; avoid.

### The transforms

| Control | Meaning | Mechanism | Applies to |
|---|---|---|---|
| **speed** `½ ¾ 1 2` | rate multiplier | Odonus `stepDiv`; `fast`/`slow` on a Tidal pattern; Balistes pattern length | ~everything (most universal — do first) |
| **transpose** `±semitones` | pitch shift | rides Odonus's injected `realize` (index-space → free on the recipe); shift Vetula chords | pitched voices |
| **bass** `F#2` | set / re-foot the bass | Vetula-flavoured | Vetula, harmony |
| **scale** `swap →` | re-quantise to a different scale | the same injected-realize seam — "same shape in lydian" | Odonus, Vetula |
| **layer +** `breakbeat ▾` | add a layer from the **snippet** world | drop a Balistes break under an Odonus line | cross-instrument (→ assembling a set by ear) |

The transpose/scale transforms lean on the **injected-realize architecture**
(Odonus works in index-space; `realize :: Index → Pitch` is reconstructed from a
serialized `PitchSet` that travels as per-voice data — see the reef
quantisation-realize design). So they are transforms *on the recipe*, not edits.

### The live-then-capture model (the key UX decision)

Transforms apply in two moments:

- **Live** — turn a transform *while previewing* and hear it change immediately
  (nothing stored). Exploratory; matches "wonder how that'd sound."
- **Capture** — a **"keep this variant"** button freezes the current transformed
  state as new content + a morphism edge back to the source. Live for
  exploration; capture for the keepers (which can then be starred / folded into a
  set).

### Micro-slices (resume order)

- **4a** — one transform (**speed**) applied *live* to the running preview, end to
  end, for one or two kinds. Proves the loop: pick → preview → turn speed → hear
  it change. No storage yet.
- **4b** — **"keep this variant"** → new content + `morphism` edge in Amphora.
- **4c** — add **transpose** / **scale** (the realize-seam ones), then **bass**,
  then the **snippet-layer** (needs the snippet world — see below).

## Related / dependent surfaces (from the same conversation)

The one recall *service* (Amphora) fronts (at least) three interaction
archetypes — the workbench is only the first:

- **Setups / go-to's (the workbench)** — whole-instrument scenes + rig-sets.
  Preview-first, transform, curated wall. *This document.*
- **Parts bin** — Selene 8-output configs: browse a labelled palette → click →
  install to 8 outs of ES-9 / FH-2. **No preview** (CV structure, not an idea to
  audition). A grab-and-fit palette, not a workbench.
- **Snippets** — small Tidal patterns, cross-instrument, composable. Their own
  world: the "breakbeat" the workbench's `layer +` drops in, the pattern pasted
  into Vetula, the fill in Balistes. The `layer +` transform (4c) depends on
  this existing.

### Normalized rig-set (separate slice, also deferred)

Save *all* editors at once as one recallable snapshot — but **normalized**: a
`rig-set` is content whose payload is a record of **references** (one content
hash per instrument + loose scalars like tempo/routing), NOT a blob of everyone's
state. Because content is immutable and deduped, a set shares storage with every
individual save, sets themselves dedup, and "this set but with a different drum
pattern" is a near-identical hash — one field changed. Recall = fetch each
referenced hash, load into its editor. Morphism edges (`set → member`) light up
the graph.

## Pointers

- Amphora store + client: `music/live-coding/amphora`, shared client
  `Triggerfish.Amphora` (`fetchCollection` / `publish` / `unpublish`).
- Shelf/bench/preview: `Triggerfish.Main` (`tidalView`, `shelfPanel`,
  `benchPanel`, `entryRow`, `PreviewEntry`/`StopPreview`, `goToCollection`).
- Transport model: `Triggerfish.Transport` (`soundingOf` with the `previewing`
  set) + `docs/DESIGN-transport-misu.md`.
