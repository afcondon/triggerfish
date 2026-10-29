# Conspicillum in the shell: a playing test

2026-10-29. Conspicillum moved into Triggerfish today, as a page in the
machines' shell (`src/Conspicillum`, `src/Triggerfish/Conspicillum/Main.purs`),
at the same address, `:3023/conspicillum/`. Claude checked what a headless
browser can check: the views, the drawer, keep, rename and delete, → Limulus,
and Play and Stop reaching the rig, at gain 0. **None of it has been heard.**
Mark each row ✓ or ✗, with a note, and file anything wrong as a CR in
`docs/kb/plans/atlantis-crs.md` under Conspicillum.

## What changed, so you know what to look for

- **The bar is the shell's**: ▶ Play / ■ Stop, the three views
  (module · laboratory · library), "→ Limulus", ⓘ, Panic. The PLAY button
  on the instrument is gone; space plays, as on every page.
- **The presets are in the shell's drawer** (`b`): kept presets under
  "Kept", then the factory's by category; or the collection chosen in the
  library. **keep** (or `c`) keeps the scene at once as "Name · 21:14";
  double-click a kept one to rename it; hover it for **delete**. The
  "keep… / off the shelf" links under the name are gone.
- **The knobs sit under the sentence**, beside the ring, so the whole face
  fits a laptop screen. With both drawers open the face narrows, then
  stacks.
- **Limulus** opens from "→ Limulus" in Solo too, since Conspicillum only
  plays on the rig. Ctrl+` toggles it, as on every page.
- **A welcome card** on the first visit, and from ⓘ.
- The library's test tags (`foo`, `bar`, `test-tag…`) are gone; the earlier
  library is still in the ref's moves.

## The rows

| # | Do | Expect | ✓/✗ |
|---|---|---|---|
| 1 | Open `/conspicillum/` fresh | the welcome card; start closes it and it does not come back | |
| 2 | ▶ Play on the bar | grains, and the ring's hand turning; the dashboard's Conspicillum chip lit | |
| 3 | ← → | the presets step, and the sound follows on the next bar | |
| 4 | Drag a number in the sentence; turn a knob | heard as you drag; "edited · revert" appears | |
| 5 | **keep** in the drawer, then rename it, then delete it | the row appears under Kept as "Name · time", renames in place, goes on the second click | |
| 6 | Material: another set, a sample, **Hold**, then step presets | held material stays while the treatment changes | |
| 7 | laboratory: move a slider; pin one | heard; the pin puts it on the module as a knob | |
| 8 | library: make a collection, put it in the drawer | the drawer lists that collection only, in its order | |
| 9 | → Limulus | Limulus opens with `conspicillum $ …`; editing and evaluating it plays the new line | |
| 10 | Reload while playing | the page comes back to the same preset, playing, with ■ Stop on the bar | |
| 11 | Two tabs of it | both show the same scene; Stop in one stops both | |
| 12 | Panic (bar, or the dashboard's) | silence, and the bar says ▶ Play | |
| 13 | `conspicillum $ hush` from Limulus | silence, and the bar says ▶ Play | |
| 14 | A small window, both drawers open | nothing cut off; the face stacks and scrolls | |

## What might go wrong

- **The dashboard shows "edited" as never.** The shell announces the page
  on the tab bus now, and it takes "edited" from a preset chip, which
  Conspicillum does not have. Its own announcement used to say so.
- **The first Play of a set** after a SuperDirt restart has to ask for its
  bank (lazy loading since 2026-10-11): the first bar may be silent.
- A page opening adopts what the rig has loaded, even when stopped, over
  the preset its link names (`#12`). That is the stage's rule, older than
  this move; say if it surprises you.
