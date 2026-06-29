# Presets, patterns, and the eDSL as the one format

*Design note, 2026-06-29. A synthesis pass on how Triggerfish thinks about
saved state across all four instruments — written to fix intention and
consistency before any code changes. This is **not a build plan**; it's the
organizing principle the eventual plan should serve.*

## Why now

Persistence landed for Balistes (localStorage). That surfaced a question bigger
than Balistes: the four instruments each handle "saved state" differently and
inconsistently, and several things AC noticed are really one gap —

- Odonus' per-instrument Tidal SOURCE pane is redundant now that the shell has a
  unified Tidal page.
- Selene has no presets at all.
- Vetula has a non-persistent "library."
- Balistes' new persistence serialises to **custom JSON**, not to the eDSL the
  SOURCE panes already speak — a quiet divergence.
- The new drum patterns: a fifth virtual module, or just more of Balistes?
  (Resolved: **extend Balistes** — the Pattern family already made Grids one
  member and fixed rhythms another; no fifth module.)

## The clarifying cut: two things are both called "presets"

- **Library items** — authored *content*, the what-to-play: a Balistes rhythm, a
  Vetula progression, a Selene rack. Built once, named, reached for.
- **Scenes / snapshots** — a captured *performance configuration* for recall and
  sequencing: Odonus' SCENES, Balistes' Grids snapshots. The how-it's-set-now.

Both want naming and persistence, so they feel like one thing — but which an
instrument needs depends on whether its *content* and its *state* are the same
object. For Odonus and Selene they essentially are (the grid / the rack *is* the
state). For Balistes they differ (a Grids snapshot is a point in a generative
field; a fixed rhythm is a literal grid) — which is exactly why the Pattern bank
can hold both. So **consistency does not mean identical machinery everywhere; it
means the differences live in *what an item is*, never in *how it's named,
saved, loaded, or shipped*.**

A clean line worth holding: a preset captures **authored config, not transient
runtime** (playhead, clock, RNG cursor are excluded — Balistes' `Snapshot`
already does this). The record is the settled intent, not the live moment.

## The representation spine

The deep object is the **PureScript record**. Both JSON and the eDSL text are
just *renderings* of it. So "round-trip JSON → Tidal amounts to the same thing"
is exactly right: the record is the truth, the eDSL text is its **canonical
serialization**, and the Balistes JSON was a shortcut that should *converge onto*
the eDSL printer/parser rather than persist as a parallel format.

This collapses the fork that looked load-bearing a moment ago. "Structured-state
as truth" and "document-as-authority" are **not two options** — they are the
same thing the instant the print/parse is faithful. Selene already proves it: its
rack doc is the authority *and* it is merely `sel` rendered to text and parsed
back.

The expressiveness worry also dissolves. We are **not** targeting Alex's
mini-notation as the save format, where per-cell probability/condition or the
Grids morph would fall off the edge. We target the **extended eDSL** in which the
virtual machines are first-class records — `odonusWith {…}`, the
`balistes "kit" … $ balistesConfig {…}` the SOURCE pane already prints, Selene's
rack, the vendored `Tidal.Vetula`. Once the records *are* the format,
prob/cond/ratchet aren't squeezed into mini-notation — they're just fields on a
`Tidal.Balistes` record. The move for the new drum patterns is therefore the
plain one: **add the `Tidal.Balistes` records and print/parse them as
`balistesWith {…}`** (or by extending the existing `balistes` form), making them
peers of everything else.

## The invariant (the organizing principle)

> Every saveable thing is a `Tidal.<VM>` record with a faithful print/parse;
> the library stores those values; the Tidal page renders and edits them;
> purerl-tidal consumes them.

The record is the noun; print/parse is the verb; the library, the wire, and the
BEAM are just places the noun travels. Differences between instruments become
*which records*, never *how it's saved*.

## "Not Tidal compatible" — and the two constraints that make that fine

The dialect has **two registers**, and the distinction is the same
delegated-vs-streamed line as PolyEuclid-vs-PolyTrig, one level up:

| register | example | who can consume it |
|---|---|---|
| **mini-notation** | `"bd sn cp sn"`, `x(3,8)` | universal — OG Tidal, Strudel, anyone |
| **VM records** | `balistesWith {…}`, `odonusWith {…}` | only things that *implement that VM* — purerl-tidal + the Triggerfish family |

AC is content for the overall notation to be **"not Tidal compatible"** — i.e.
the `…With {…}` records mean nothing to upstream Tidal — **provided two things
hold:**

1. **The mini-notation *within* it stays OG-Tidal-compatible.** The `"bd sn cp
   sn"` strings embedded in our records parse and mean the same as in Alex's
   Tidal, so anyone can lift those fragments straight into an OG Tidal project.
2. **The whole thing is valid PureScript eDSL** — a real `…With { … }` value,
   not an ad-hoc text format.

**Refinement (AC, 2026-06-29) — use mini-notation only where it compresses a lot
and reveals structure.** Mini-notation is the right register for `bd*4`,
`x(3,8)`: a short string that makes a repetition or Euclidean structure *legible*.
It is the *wrong* register for a dense per-cell grid with arbitrary velocities —
chaining a rhythm string against a parallel `# gain "0.77 0.9 …"` string is
*harder* to read, not easier (a human can't visually line the two up), and it
isn't lossless. So a Balistes fixed rhythm serialises as a **fully-structured
record** (lanes, hits, per-cell overlay as fields), not as mini-notation.
Constraint (1) is about keeping embedded mini-notation compatible *where it
genuinely appears*, not about forcing everything into it.

Hold those two and the portability story is honest and useful: people extract
mini-notation bits for their own Tidal sets, and we share whole presets with
other eDSL-based apps — **Calypso today, maybe others later** — because they
speak the same `Tidal.*` records. "Portable" then means *portable within
purerl-tidal's world, degrading cleanly to plain mini-notation where they
overlap* — never a promise to a Strudel user that a `balistesWith` record can't
keep.

## What this asks of each instrument

- **Selene** — essentially there (doc ⟷ `sel`, `parseRack` total). Needs only the
  library/persist layer wrapped around what it already round-trips. Cheapest win.
- **Balistes** — has the records for Grids; needs the new `Tidal.Balistes`
  records for fixed rhythms + the per-cell overlay (vel/prob/cond/ratchet), a
  faithful `balistesWith` print/parse, and the localStorage Store **converged
  onto that rendering** instead of bespoke JSON.
- **Odonus** — has the printer (the SOURCE pane, "the growing spec"); needs the
  **parser** side for true round-trip + persist. Its redundant per-instrument
  source pane demotes once the Tidal page owns source.
- **Vetula** — needs its eDSL form (its chord/pitch sets and progressions as
  `Tidal.*` values) and the same persistence, replacing the in-memory library.

It is fine for the dialect to run **ahead of the BEAM**: the app has always been
the design surface, the spec leading and the engine catching up. A `balistesWith`
field can exist in the eDSL before `balistes_engine` implements it.

## What it folds together

One gap, not five. The SOURCE pane stops being read-only scaffolding and becomes
the **preset editor**; the library is just a named collection of these values;
persistence stores the rendering; shipping to the BEAM is that same rendering
crossing the wire. Odonus' redundant pane, Selene's missing presets, Vetula's
ephemeral library, Balistes' JSON — all of them are "we *print* the eDSL but
don't yet *round-trip and store* it, everywhere, the same way."

## Decisions captured (for the eventual plan to honour)

- **Drum patterns = extend Balistes**, not a fifth module.
- **Independent loading** — per-instrument loadable preset sets, not one
  monolithic session blob; that's how you actually work (drop a rhythm into
  Balistes without disturbing Odonus) and it lets you mix a Vetula progression +
  a Balistes pattern + an Odonus scene freely.
- **Two altitudes, one library.** Quick per-instrument recall (the switcher, the
  scene buttons) is a *performance* gesture and stays in the instrument;
  organising / naming / exporting / importing is a *management* gesture and
  belongs on the **Tidal page**, which becomes the cross-instrument **library
  manager** as well as the source surface. Same library, two windows.
- **The Tidal page absorbs the per-instrument source panes' job** (resolving the
  Odonus redundancy).

## Still open (deliberately not decided here)

- Whether **scenes** and **content libraries** are one mechanism or two kept
  distinct (lean: distinct roles, shared save/name/serialize plumbing).
- The exact **grammar for the per-cell overlay** in `balistesWith` (vel/prob/
  cond/ratchet) — must satisfy constraint (1): the embedded patterns stay clean
  mini-notation; the overlay rides as record fields, not as bespoke string
  syntax.
- A **name** for the dialect. It wants one eventually — the fact that we keep
  reaching for "Tidal, but…" is the tell that it has become its own object.

*No build order yet — this note exists to make the plan, when we write it, serve
the invariant rather than patch the symptoms.*
