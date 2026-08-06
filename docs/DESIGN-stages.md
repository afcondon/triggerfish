# Stages — the one mode axis, shared across machines

**AC + Claude, 2026-08-06.** Supersedes the per-machine mode types
(`Vetula.App.View` + `captureView`, `Odonus.Grid.Types.OdonusView`).

## The problem it fixed

Two separate messes that compounded.

**1. The user's count and the type's count disagreed.** AC, describing Vetula:
*"vetula is complex because it has three forms — 1 harmony exploration, itself
four views AND 2 Perform and Replay."* Three. The type said

```purescript
data View = Browse Viewtype | Perform     -- two
-- plus, nested inside Perform:
data CaptureView = CapLive | CapReplay    -- a layout flag
```

— two modes, one of which had a layout switch. When the player's count of the
modes and the type's count of the modes disagree, the type is wrong; the player
is the one using it.

**2. The nav was scope-blind.** Vetula's `contextBar` was one flat row of ten
slots regardless of mode:

| always | Hunt only | mode |
|---|---|---|
| session, key, scale, midi chip, ⓘ | family, palettes, borrow, lens ▾, shake ⟳, reset view | Perform button |

Six of ten were dead weight while performing — present, lit, clickable, meaning
nothing. Meanwhile the one control that *was* live in Perform (the LIVE/REPLAY
switch) wasn't in the bar at all: it floated absolutely-positioned over the
surface, because the bar had no notion of stage to hang it on. Exactly inverted.

The tell was the lens dropdown. While Perform was up it displayed
`browseOr lastBrowse view` — a *remembered* projection presented as the current
one, because there was no honest value for it to show.

## The concept

One object, three stages, in the order material flows through them:

```
HUNT  ──→  PERFORM  ──→  REVIEW  ──┐
  ↑         harmonic material      │
  └─── voiced into notes ──────────┘
             lifted into clips
```

- **HUNT** — hunt harmonic space through one of four projections (fifths /
  tonnetz / lattice / explore). Produces the tank and the progression.
- **PERFORM** — player boxes voicing that material live, capture river alongside.
  Produces notes.
- **REVIEW** — the whole-session roll, cherry-pick a phrase. Produces clips.

```purescript
-- Vetula
data Stage = Hunt Viewtype | Perform | Review
-- Odonus
data Stage = Perform | Review
```

**Odonus is Vetula minus HUNT** — Vetula is the harmonic authority, so Odonus has
nothing to hunt. That is also why Odonus's toggle never had a name: a binary
doesn't need one, which is how it ended up reading `⛶ full` while Vetula called
the same surface REPLAY.

### A stage is what you are LOOKING AT, not what is running

The generator keeps generating and the voices keep sounding in every stage — you
can hunt chords while the boxes loop. The transport is orthogonal and lives in
the shell's top nav. Hence the division:

> **Top nav owns what's running. The machine's secondary nav owns what you're
> looking at.**

Two questions, two bars, no overlap. This is also why the shell's PLAY and BPM
moved hard left beside the wordmark the same day.

## Naming

Candidates for the third stage were *tickertape*, *log*, *history*, *replay*.

- **tickertape** names the wrong surface — the LIVE river already *is* the
  tickertape, and it lives in PERFORM.
- **log** / **history** say *archive*: passive, a record of what happened. But
  you don't go there to read it, you go there to lift phrases out of it. It's a
  work surface.
- **replay** claimed a transport distinction the control never made (the
  generator runs either way), which is what got it renamed to `⛶ full` on Odonus
  in the first place — honest about being a size, but then nameless.

**REVIEW** — a verb, in the same mood as HUNT and PERFORM, three stages of one
pipeline, and it works unchanged on both machines.

## The nav rule that falls out

```
LEFT   stage tabs (under the shell's transport), then ONLY the controls
       that mean something in the stage you're in
RIGHT  housekeeping, then the harmonic column
```

```
TOP     TRIGGERFISH [▶PLAY] BPM [machine tabs] ········ [pitch set] [SOLO|ATLANTIS]
VETULA  [HUNT│PERFORM│REVIEW] «stage controls» ········ session ▾ │ [key][scale] │ midi ⓘ
ODONUS       [PERFORM│REVIEW] «stage controls» ········ scenes ▾  │ [◀Vetula C Phryg ▦]
```

**Two groups never move** — the stage tabs and the harmonic context. Only the
middle-left group changes with the stage, so the changing region is one
contiguous block you learn to expect rather than a row that rearranges under you.

**The harmonic column** is the right edge of all three bars, because all three
show the same thing at different removes: Vetula *sets* it (editable), the shell
*states* it rig-wide, Odonus *reports* the slice it quantises to. They stack.

**Stage controls, by stage:**

| stage | controls |
|---|---|
| HUNT | lens ▾ · shake ⟳ · family · palettes · borrow · reset view |
| PERFORM | ◆ mark · counts · clear |
| REVIEW | ◆ mark · counts · clear |

PERFORM and REVIEW share theirs *deliberately and identically*: a control that
belongs to two stages belongs to the chrome, not to either surface. Putting ◆
mark on a surface would move it under you exactly when you switched stage to look
at what you just marked. This retired both Vetula's `captureHeader` and the
duplicate ◆ mark in Odonus's scope overlay (which had been showing the same
counts as the nav, in two places at once). The overlay keeps only the `● logging`
dot — the one thing that is about the river rather than about the session.

## What the collapse also fixed

- **Leaving REVIEW by any route now hushes the region preview.** `SetStage`
  absorbed `SetCaptureView`, so escaping sideways into HUNT stops a looping
  preview. Under the split you could leave via Browse and it kept ringing — the
  "known wrinkle" in DESIGN-capture-surface.md.
- **The lens dropdown never lies.** It's a HUNT control, so it only exists while
  hunting, where its value is the truth.
- **Dead weight removed with it**: `Focus` (written, never read), `RailSection` +
  `railOpen` (initialised and toggled with no renderer), `poolSpine` and
  `focusTab` (defined, never called).

## Not done

- **Odonus scenes as genre templates.** AC: *"scenes in Odonus could be very very
  different, almost templates for types of music."* Odonus deliberately gets no
  *sessions* — a Vetula session accumulates material (tank, progression, boxes);
  Odonus's state is one grid, so there is nothing for a session to hold. The
  scene menu now sits in the same housekeeping slot as Vetula's session menu;
  what goes *in* it is an open content question.
- **PER CELL still overflows** by one knob row in the params panel.
