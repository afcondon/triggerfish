# Vetula — unified surface: states, objects & workflows

A short working note to mark up together. Goal: decide the **states and transitions**
of the unified page deliberately, instead of one bug at a time. Written just after
retiring the Hunt/Perform width-flip (the first-pick-collapses-the-lattice wart).

## Where we are now (post-collapse)

One surface, always visible:

```
┌ topNav: Vetula · KEY · SCALE · [family] · drops · borrow ······ MIDI · ⓘ ┐
├──────────────────────────────────────────┬───────────────────────────────┤
│ POOL — the lattice / chord cloud          │ RAIL (accordion, multi-open)  │
│   pick chords, summon families, borrow,   │  ▾ Progression  (the path)    │
│   drop exterior sets; hover+v → revoice   │  ▸ Library      (snapshots)   │
│                                           │  ▸ Voices       (playheads)   │
│   [ revoice = DOM modal over everything ] │                               │
└──────────────────────────────────────────┴───────────────────────────────┘
```

- **The path IS the progression** (Slice 4a). Picking chords in the lattice grows
  `path`; the Progression rail section shows it live. No load-a-copy step.
- **`focus` state is dormant** — `Focus`, `SetFocus`, `poolSpine`, `focusTab` remain in
  the code but nothing reads `focus` for layout. Retained on purpose: a *deliberate*
  perform-collapse may return, but on an explicit trigger, not an auto-flip.

## The objects (the "pieces we juggle")

| Object | What it is | Owns | Emits |
|---|---|---|---|
| **Pool / lattice** | the hunting ground — families, extensions, borrow, drops | `chords`, `nodes`, sim `handle`, `focusedFamily` | PathPick, SummonRoot, OpenRevoice, DropSet, Hover |
| **Progression** | the live `path` over pool chords + its playback | `path` | PathPick (grow/clear), PlayPath, StepClick |
| **Revoice modal** | within-chord surgery for one chord (ladder, swatches, bass) | `revoicing`, `selected`, `drag`, `favorites` | drag, SlashBass, PickVoicing, Tab/↑↓/f |
| **Library** | auto-captured + kept snapshots of past paths | `library`, `lastCapIdx/Sig` | restore-into-path, keep/drop |
| **Voices** | per-voice Tidal read-heads that realise the path to MIDI/Odonus | `voices`, `tempo`, `previewChan` | Add/Remove/Commit voice, Cycle dest/renderer/artic |

## The workflows (user journeys)

1. **Hunt → build.** Set key/scale → pick chords in the lattice → `path` grows,
   Progression section fills. *(now works with the lattice staying open)*
2. **Revoice.** Hover a chord + `v` → modal → octave-drag / Tab / bass-slash / `f` keep
   → close. The chord's voicing changes in place; it stays in the path.
3. **Perform.** Add voices in the Voices section → commit read-head + ♪ patterns →
   hit ▶ → the path plays through the voices to the rig.
4. **Snapshot / restore.** A path auto-captures to the Library on source change; `keep`
   promotes ◦→★; clicking a Library entry restores it *into* the path (non-destructive
   because the current path was already captured).
5. **Clear / start over.** `c` or click-the-last-chord empties the path; the next pick
   opens a fresh capture session.

## Open transition questions (to decide together)

1. **Is there a "perform" mode at all, or one surface forever?** If a collapse returns,
   what triggers it — ▶ (playing), or an explicit "focus the rail" control? (Leaning:
   explicit, never automatic.)
2. **Play/stop as a state.** `playing` exists. Should the surface *reflect* it (e.g.
   the sight-reader ribbon #74, dim the pool, lock edits)? What's legal to edit mid-play?
3. **Revoice source.** Today revoice opens from a **pool** chord (hover+v). Should it
   also open from a **Progression step** (revoice step 3 of the path directly)? That's
   the `OpenRevoice` "source" question (#88 follow-on).
4. **Restore semantics.** Clicking a Library entry — replace the path, or append? And
   does restoring re-arm the voices that were saved with it (ties into #90, persistable
   voice-sets decoupled from progressions)?
5. **Empty-state affordance.** With no `path` and an empty lattice, what does the surface
   invite? (The old "blank canvas" problem the Hunt/Perform split was trying to solve.)
6. **Voice-set ↔ progression coupling (#90).** Is a *scene* = progression × voice-set?
   One Library or two? Where do voices live when the path is cleared?

## Open: retire the top nav → a "Scale" accordion pane (parked, thinking)

Proposal: drop the top bar entirely; move its harmonic-context controls into a new
**Scale** pane at the top of the rail accordion, so the rail reads top-to-bottom as the
workflow order — **Scale → Progression → Library → Voices**. The pane header shows the
current setting as its subtitle ("Scale · C major"), so context stays visible when
collapsed *and* the lattice gets full window height.

- **KEY · SCALE · BORROW · drop-sets** → the Scale pane (these are "set the world" controls).
- **MIDI chip** → the Voices pane (it's an output concern).
- **ⓘ help** → a small floating button over the surface. **Title** → gone.

**The snag = the contextual family picker** (per-family mode reflavour). Confirmed it's a
genuinely different thing from Borrow:
- *Borrow* = same home tonic, parallel mode → chromatic chords on home degrees (the one AC
  actually uses; his intuition "borrowing from another scale on the same key" is exactly this).
- *Family reflavour* = a family rooted on a **different** tonic gets its **own** mode
  (a C-major and an F♯-Phrygian family coexisting) — a secondary-key-area move.

Borrow already auto-tags its chords with their source scale (`familyScale` write in
`BorrowFrom`), so the manual family picker is **only** for hand-reflavouring a summoned
family — rare, and theory-exposing (a mode dropdown per region), which cuts against
Vetula's gesture-led ethos. **AC's steer (parked):** the goal is *not to prescribe outside
chords in the pool* — the family picker is too prominent for a specialty feature. Options
on return: (a) keep the `familyScale` *mechanism* (borrow needs it, coexisting-mode regions
are nice) but **retire the manual picker from the UI**; (b) rethink it later as a *gesture*
rather than a dropdown; (c) drop the capability. Leaning (a) for now → nothing to rehome,
nav removal is clean.

## Resolved during this pass

- **Octave-drag identity.** Two fixes: (1) `text-selection` during drag was suppressed
  (`user-select:none` on the modal svg); (2) the drag no longer re-sorts the voicing —
  the array **slot is the voice's identity**, so a move/double keeps its place. (3) The
  ladder dot colour now tracks **pitch class** (one muted hue per chromatic tone, 12
  colours) rather than array slot — a voice keeps its colour through octave-drag,
  re-sort, and doubling. The bass wears a dark rim so the foot still reads.

## Known bugs parked against this redesign

- **Dead CSS** — `rv-backdrop` / `rv-drawer` / `rv-title` / `rv-hint` are unused since the
  modal moved to the shared widget; sweep when we touch the stylesheet.
