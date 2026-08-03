# Vetula progression-building — findings & redesign directions

*Design note, 2026-07-30. Marginalia #240 (Triggerfish), branch `macro-tidal`.
Written before a compaction; this is the grounding for a redesign discussion
(task #10). The trigger: verifying Vetula's preset chip required INJECTING a
library entry because there was no obvious way to build a progression by hand.*

## The core problem

Vetula is a rich harmonic **explorer** (Tonnetz lattice, chord families,
voicings, the pitch ladder, the TANK of caught chords, lenses:
keyboard/pad-grid/fifths/tonnetz/lattices/grow) **and** it has a **progression**
(the `path` — the ordered chords you perform). But **the bridge between
exploring and composing is missing**: there is no direct gesture to grow a
progression by picking chords on the primary (Tonnetz) surface — the one thing
the on-screen hint ("Shift-click chords on the lattice to grow a progression
here") literally tells you to do.

## How the `path` is actually built today (4 ways; none is "click the lattice")

Every mutation of `st.path` in `src/Vetula/App.purs`:

1. **`PathPick pid`** (handler at ~line 1107) — append a lattice chord to the
   path, **auto-bridged**: it runs `Path.shortestPath` over the Tonnetz web from
   the last path chord to the clicked one and inserts the intervening
   voice-leading chords; clicking the last chord again clears the path. This is
   clearly the *intended* progression-builder — **but it has NO UI binding.**
   Orphaned code.
2. **Lattice shift-click is bound to catch-to-TANK instead** — `CatchTriad` /
   `CatchNode` / `CatchChord` (onClick handlers at ~lines 3460, 3634, 3735,
   4448: plain = audition/play, shift = catch). So the primary surface builds
   the tank, not the path. This is what displaced `PathPick`.
3. **Generate flow** — shift-click existing progression **rows** (`StepClick i
   shift`, ~1287) selects 1–2 positions into `genSel`; the "grow" lens generates
   candidates; **`PickCandidate cid`** (~1308) inserts / substitutes / prepends /
   appends one into the path. Needs an existing path to select positions within.
4. **`LoadProg i`** (~1327, load a library entry) or **`LoadSource`** (paste
   Tidal `note "<[..] [..]>"` text and press Load). `parseProgression` +
   `importChord` (in `Vetula/Tidal.purs`) turn text → note-lists → chords.

## The two disconnected collections

- **TANK** (`st.tank :: Array Specimen`) — caught chords; state comment says it
  is "the durable, unordered collection … will feed the Stage + Sequences."
  Clicking a tank specimen (`seedChord`) injects it into the pool as a centre
  chord for **lattice exploration** — NOT into the path. So the tank is
  currently a **dead-end** w.r.t. composing a progression.
- **PATH** (`st.path :: Array Int`) — the progression (chord ids, in order).

Gather (tank) and arrange (path) never connect.

## Redesign directions (react to these)

- **A — Un-orphan `PathPick` (minimal restore):** re-bind lattice shift-click (or
  a dedicated modifier, or a "compose" mode) to append-to-path with the
  auto-bridging; move tank-catch to its own gesture (the `k` key already
  catches). Smallest fix; restores the intended flow.
- **B — Tank *is* the palette, path is arranged from it (most coherent):** catch
  chords freely (explore), then a gesture (drag / click-in-order) lays tank
  chords into the path. Makes the two collections one workflow: gather → arrange,
  and gives the tank a real job.
- **C — Explicit "compose mode":** a mode where every chord click builds the path
  (auto-bridged), distinct from an "explore mode" where clicks audition/catch.
  Removes the modifier ambiguity.
- **D — Something more dramatic** — AC is open to rethinking the whole metaphor
  (said so 2026-07-30 and earlier).

Claude's instinct: **B** (tank-as-palette) is the most coherent; **A** is the
cheap restore. Open question for AC: *what progression-building experience do you
want when you sit down with Vetula?* — answer that first, then pick the shape.

## Context-panel redesign — three views + a color-chord overlay (2026-07-31)

AC's design pass on the CONTEXT control (the left floating card). Agreed shape;
this is the plan to **implement after the VST audition + a compact**.

### The three views (down from four lenses)

Drop `keyboard` — it is only a root-picker subset of `fifths`. Make `fifths`
interactive and that retires it. What remains, each a way of laying out the SAME
material:

1. **Circle of fifths** (now interactive) — the "extensions" home. Always shows
   the color-chords (McMullen / Butler / Stock) and the **borrowed-scale** chords
   sitting on the ring. The manual `e`/`s` extension toggles go away — the
   color-chords *are* the extensions (they recapitulated the lattice anyway).
2. **Tonnetz** — diatonic triads of the primary scale by third-relations. Triads
   only; its job is triad relationships + **stacking** (below).
3. **Voice-leading lattice** (this is the name for the current unnamed "lattice")
   — chords by smooth voice-leading distance. Can carry the borrow + color-chord
   layers too.

### The key reframe: palettes are a color OVERLAY, not a mode

Today McMullen/Butler/Stock and the borrow scale are *selectors that change what's
shown*, and they connect to nothing on the geometry. Instead they become an
**always-on annotation layer** painted onto the views. This fixes every
disconnection listed in the findings: the borrow scale and the chord-sets stop
being inert dropdowns and become colored chords *on the view*. Palette chips
become **show/hide toggles** for each color layer, not a mode switch.

### Decisions (AC, 2026-07-31)

1. **Which layers on which view:** the color layers are available on BOTH the
   circle of fifths and the voice-leading lattice, each toggleable; the view only
   changes the spatial arrangement. **Tonnetz stays triads-only.** (strong agree)
2. **Triad stacking (tonnetz):** shift-click accumulates triads into a stack.
   Edge-adjacent triangles (sharing two notes) fold into 7ths/9ths, BUT stacking
   is **freeform** — you can also pick odd combinations, e.g. a D-minor triad in
   the bass and an F-major triad an octave up (a cross-register polychord). So the
   stack is an ordered set of (triad, register) picks, not only adjacent merges.
3. **Colors:** each chord-set gets its OWN distinct color — borrowed = one hue,
   McMullen = another, Butler = another, Stock = another, diatonic = the base.
   A legend. (NOT sharing the tonnetz outside-distance ramp.)

### Unified gesture

Across all three views: **click = catch to tank**; **shift-click = stack/extend**
where it means something (tonnetz especially). Consistent with the existing `k`
catch.

### Implementation sequence (autonomous run — commit each step, verify in browser)

Each step is independently buildable/committable; do them in order and stop to
flag only a genuine fork. The betweening-flavor decision (cadence vs
voice-leading) is NOT here — it waits on the VST audition.

1. **Drop `keyboard`, make `fifths` interactive.** Remove `LensKeyboard` from
   `geometryLenses`; wire click-to-catch on the circle-of-fifths surface (today
   non-interactive). Rename the `lattices` lens label → **voice-leading lattice**.
   Smallest first step; leaves 3 views.
2. **Palette → color-overlay model.** State gains a set of *active color layers*
   (diatonic / borrowed / McMullen / Butler / Stock), each with a fixed hue.
   Palette chips flip from mode-select to show/hide toggles. No render yet.
3. **Render the color layers on fifths + VL-lattice.** Generate each layer's
   chords from Harmonia for the current key (+ the borrow scale for the borrowed
   layer) and paint them as colored tokens on those two surfaces. Tonnetz stays
   triads-only. A small legend.
4. **Unified catch gesture.** click = catch to tank; shift-click = stack/extend,
   consistent across the three views (folds in the existing `k` catch).
5. **Tonnetz triad stacking.** shift-click accumulates triads into an ordered
   (triad, register) stack — edge-adjacent → 7ths/9ths, but freeform too
   (cross-register polychords, e.g. Dm bass + F maj an octave up). The stack
   catches to the tank as one `Anchor`.

### Built (2026-07-31) — all five steps done, verified in-browser

Commits `cc08677` (1) · `0a52dad` (2) · `70937fd`+`3ed2835` (3) · `1cbecf5`
(4) · `d548946` (5) on `macro-tidal`.

- **Step 1** — `keyboard` dropped from `geometryLenses`; `lattices` relabelled
  **voice-leading lattice**; the circle-of-fifths root markers are clickable
  (plain = audition the root's `triadOn` triad, shift = catch).
- **Step 2** — `ColorLayer` (diatonic/borrowed/McMullen/Butler/Stock), each a
  fixed hue; `colorLayers :: Set ColorLayer`; `ToggleLayer`; PALETTE chips are
  show/hide toggles with hue swatches (they double as the legend).
- **Step 3** — color chords are **de-duplicated by content** (`mergedLayerChords`)
  and each shown ONCE as a real chord glyph BADGED by source (`layerBadges`, one
  hue pip per set): notes-on-stave on the fifths corona (`colorGlyphAt`), native
  chromatic polygon on the VL-lattice ribbon (`latColorRibbon`). *(This is AC's
  revision of the first cut, which drew plain dots per-source.)*
- **Step 4** — those glyphs are catchable (`colorHit`: hover-preview, click =
  audition, shift = catch). The **BORROW picker only shows when the borrowed
  layer is engaged** (AC), sitting under the PALETTE toggles.
- **Step 5** — Tonnetz **triad stacking**: **alt-click** accumulates triads into
  `tonnetzStack` (toggle), plain/shift unchanged (audition/catch); stacked
  triangles get a violet wash + pick-order badge (`tonStackMark`); a `stackBar`
  HUD names the triads + the pc-union polychord and offers **catch as anchor**
  (`CommitStack` → one tank Specimen) / **clear**. Edge-adjacent triads fold into
  7ths/9ths for free (F + Am → C E F A = Fmaj7, verified).

**Gesture model (AC decision, 2026-07-31):** keep **plain = audition, shift =
catch**, and use **alt = stack** — NOT the doc's original "click = catch /
shift = stack" inversion (rejected: it would have cost one-click auditioning
across every surface).

**Follow-ups left open:**
- **Cross-register polychords** — step 5 stacks in one register (the pc-union).
  Dm-bass + F-maj-an-octave-up needs a register field per stack entry + a gesture
  to bump octave (e.g. alt+shift-click raises register). Deferred.
- **VL-lattice true integration** — the color layers ride a ribbon across the top
  rather than being placed into the web by voice-leading distance. A deeper pass
  would weave them in. Deferred.
- **Diatonic-on-fifths redundancy** — the diatonic corona ring duplicates the
  wheel's own wedge/markers; fine but could be suppressed.

### Open implementation questions (resolve during build)

- Voice-leading lattice node content vs tonnetz — keep them genuinely distinct
  (third-relations vs voice-leading distance), not two skins of one graph.
- Where the color-chord sets come from in Harmonia (the palette generators) and
  how a "borrowed" chord is tagged for its color.
- Stack → tank product: a stacked polychord caught to the tank as one `Anchor`?
- Fifths interactivity: click a root vs click a color-chord token on the ring.

## Related

- The Vetula preset **chip** (capture/recall of the progression source) already
  works — see the session's `Roll the unified Preset chip to Vetula` commit. This
  note is only about **building** a progression, which the chip doesn't address.
- Vetula's LIBRARY (auto-capture stack) is the named tier, kept until the
  between-sessions modal exists (see `docs/DESIGN-scene-modal.md`).
