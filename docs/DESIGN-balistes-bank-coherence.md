# Balistes banks — one artefact model across the three brains

**Status: design. Prompted by AC, 2026-08-07, after the first rig session.**

> "i wonder whether redundant preset buttons, one per machine would actually be
> a good idea, there's no way to name the presets for the other two machines, i
> notice. all a bit disjoint. and the fact that you have to publish the Rytm but
> the others make live changes is also a bit weird."

Three complaints, one cause. This is the plan for the coherent version.

## Read this first: the G-means-RYTM trap

`printTri` writes the brain tag as **`M` / `G` / `T`** — frozen on-disk
discriminators dating from when the brains were called MUTABLE / GRIDS / TIDAL.
The 2026-08-06 rename made them GRIDS / RYTM / TIDAL, and `brainBadge` displays
**`G` / `R` / `T`**.

So **stored `G` = RYTM, displayed `G` = GRIDS.** They are different alphabets
that share letters.

Any UI that shows a brain letter must go
`p.content → parseTri → brainOf → brainBadge`, and must never read the leading
character of the stored text. Doing the naive thing yields a letter that is
confidently wrong for two of the three machines. The tags cannot be renamed —
`parseTri` is lenient, so changing them would silently drop every snapshot
already in localStorage and Amphora rather than failing loudly.

## What exists today

Two collections, and the asymmetry between them is the whole problem.

**`library :: Array P.FixedPattern`** — RYTM rhythms.
- Named (`SetPatternName`), editable in place (`modLibAt s.fixedSel`).
- Persisted on every edit (`persistLib = persist *> repushFixed`).
- Merged from Amphora on load (`mergeByName`), published back with
  `PublishActive`.
- Rendered as chips in the presets modal's **RHYTHMS** section.

**`presets :: Array Preset`** — tri-snapshots of *any* brain, stored as text.
- `Preset = { content :: String, name :: Maybe String, starred :: Boolean }`,
  where `content` is `printTri` output, so the brain is recoverable.
- Glyph alias derived from content (`glyphOf p.content`), identical state ⇒
  identical glyph.
- Captured with the `c` hotkey. Starrable, deletable, recallable.
- **`name` can never be set — no rename action exists anywhere.**
- Rendered in the **SNAPSHOTS** section with no brain badge.

So RYTM has a first-class named, editable, publishable artefact type. GRIDS and
TIDAL have only anonymous captures. Every one of AC's three complaints falls out
of that single asymmetry:

| complaint | cause |
|---|---|
| can't see which machine a preset is from | `brainBadge` exists, brain is in `content`, `snapshotRow` just doesn't render it |
| can't name the other two machines' presets | `Preset.name` exists with no setter; RYTM's naming lives in the *other* collection |
| only RYTM has PUBLISH | Amphora write-back is wired to `library`, which is RYTM-only |

## The spine: the codebase already names the right idea

`Triggerfish.Preset` says:

```purescript
-- | The human label: the name if promoted, else the glyph alias.
```

**Promoted.** The model is already articulated in a comment and nowhere else:

> An anonymous, glyph-identified **capture** becomes a named **artefact** by
> being named. Naming is promotion.

That is the coherent spine, and it costs nothing to adopt because the data
already supports it — `name :: Maybe String` is exactly a promotion flag.

Restated as the target model:

- **One bank.** Every entry is a brain-tagged tri-snapshot.
- **Anonymous entries are captures.** Cheap, `c`-hotkey, identified by their
  content glyph. This is the macro-tidal sequencing material.
- **Named entries are artefacts.** They are what you curate, star, share, and
  publish to Amphora.
- **The brain badge is how you read the bank.** G / R / T does real work once one
  list holds all three.
- **`library` is the special case that got there first** — it is precisely "named
  artefacts whose brain is R", and should fold in rather than persist as a
  parallel system.

## Answering the two open questions directly

**"Redundant preset buttons, one per machine — good idea?"** No. One bank, with
the badge and a brain filter. Per-machine buttons would re-create the current
disjointness in the UI after removing it from the model, and they fight the
macro-tidal purpose of the bank, which is explicitly to sequence brains
*intermingled* (`DESIGN-tri-snapshot.md`). The band you are on can bias the
default filter; it should not partition the collection.

**"Publish for RYTM but the others are live — weird?"** Yes, and it disappears.
PUBLISH stops being a RYTM verb and becomes "share this named artefact", offered
on any named entry of any brain. Note the current button is Amphora write-back,
*not* a rig push — the naming makes it read as a transport concept, which is part
of why it feels odd. Rename it to something that says what it does (SHARE, or
PUBLISH TO STORE).

The deeper reason RYTM feels different is real and should be preserved: a rhythm
is a thing you *edit*, while a Grids point is a thing you *land on*. Under the
target model that is not a difference in kind — selecting any named artefact
loads it into its band, and edits write back to it. `scratchFixed` (the ephemeral
recalled snapshot) stays as the "recalled but not adopted" state, which is what
keeps recall non-destructive.

## Slices

Ordered low-regret first. Each is independently shippable and independently
testable on the rig.

### 1 — Show the brain badge (cosmetic, no model change)

`snapshotRow` renders star + glyphs + label + delete. Add the badge in front of
the glyph pair, via `parseTri → brainOf → brainBadge`. Small engraved letter, not
a coloured pill (Hainbach × Rams — `DESIGN-tri-snapshot.md` says the same).

Ships the "see at a glance which machine it's from" ask on its own. **Mind the
G-means-RYTM trap above.**

### 2 — Rename presets (closes the dead `name` field)

A rename action on a bank row, mirroring `SetPatternName` for rhythms. This is
also the promotion primitive, so it is worth doing before anything depends on
promotion.

Once it exists, `presetLabel`'s "name if promoted, else glyph alias" becomes true
in practice rather than aspirational, and the label a row shows starts carrying
the distinction.

### 3 — Distinguish captures from artefacts in the bank UI

With naming possible, split the SNAPSHOTS list visually: named artefacts first
(or in their own group), anonymous captures after. Star already exists and is
adjacent to this — decide whether starred-and-anonymous is a state worth keeping
or whether starring should imply promotion.

### 4 — PUBLISH any named artefact

Generalise `PublishActive` from "the selected rhythm" to "this named bank entry".
Needs a decision on the Amphora side about content types — today the store holds
patterns as balistes-grid favourites; a Grids point and a TrigBank are different
payloads. `printTri` already gives one canonical text per brain, so the store can
hold `content` verbatim and tag it by brain, which is the smallest change.

### 5 — Fold `library` into the bank

The structural one, and the only one that should wait for AC's eye. RYTM rhythms
become named R-brain entries; the RHYTHMS section becomes a filtered view rather
than a separate collection; `fixedSel` points into the filtered set.

Watch for:
- **Amphora `mergeByName`** currently merges rhythms only, keyed by name. Folding
  in means merging across brains — name collisions between a Grids point and a
  rhythm are now possible, so the key must include the brain.
- **Store envelope goes to v5.** v4 is `{ library, presets }`; v3 was a fixed
  bank of `printTri` texts. Migration must fold v4's `library` into the bank as
  named R entries, and must be tested against a real localStorage payload —
  `parseTri`'s leniency means a bad migration silently loses everything rather
  than erroring.
- **`fixedSel` semantics.** It indexes `library` today. Against a filtered view it
  needs to be a stable identifier, not a position, or reordering breaks the
  selection. The same reasoning `DESIGN-tri-snapshot.md` used for storing the
  artefact rather than a `library !! i` index applies here.

## Not in scope, but adjacent

- `patternChips` highlights on `s.active == AFixed i` — the *sounding* brain —
  so with GRIDS live no chip appears selected even though `fixedSel` points at
  one. Should reflect selection, or show both states distinctly (outline =
  selected, fill = sounding). Slice 1 or 3 is the natural moment.
- `SelectPattern` does not close the presets modal, while its sibling
  `RecallPreset` does, with the stated rationale "you picked a thing, you want to
  see it land on the bands". Same modal, two behaviours.
- `cellStrip` renders `""` when nothing is selected — the milestone's "RYTM's
  cell strip has no prompt when nothing is selected, an invisible feature".
