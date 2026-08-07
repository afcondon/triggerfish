# Balistes beds — the middle level of beat composition

**Status: design. AC, 2026-08-07, after the bank fold.**

> "i also feel like Balistes would be great for building up several variations of
> patterns and mixing grid like beats with pattern beats to get interesting
> rhythm beds in their own right. And those assemblies are like blocks in
> themselves. […] beats are different and have at least three levels of
> composition."

## Three levels, of which two exist

| level | what it is | today |
|---|---|---|
| 1 · **beat** | one Grids state, one Rytm rhythm, one Tidal rack | ✅ an artefact in the bank |
| 2 · **bed** | a sequence of beats that reads as one rhythmic idea | ❌ **not an artefact** |
| 3 · **arrangement** | whole-machine states across the rack, over time | ✅ a macro-tidal lane |

## The gap is reuse, not expressiveness

Worth being exact, because it changes what to build. AC's example — *"four bars of
this beat from Grids, then a bar of this Rytm pattern, then every second time this
mad fill from this tidal pattern"* — **is already expressible today**, at
bars-per-step 1, on the ⌥3 page:

```
g g g g r <~ f>
```

The lane grammar has per-cycle alternation `<a b>`, rests `~`, and `# verb`
stacks; the rack lane already sequences all three brains. So there is no missing
notation.

What is missing is that the result is **not a thing**. A bed lives as a string in
the shell's `macroLanes`, one per machine. It cannot be named, banked, glyphed,
starred, or dropped into another lane. A rhythm bed you spent twenty minutes
getting right is a text field, not an artefact — and it is edited on a different
page from the beats that compose it.

So: **reuse and locality**, not expressiveness.

## The move: a bed is a bank entry

`Triggerfish.Balistes.Component`'s ASSEMBLE docstring warns against exactly this
feature —

> *"Deliberately NOT a new chaining mechanism… A private Balistes chainer would
> have been a second, weaker sequencer that didn't compose with the other
> machines' lanes."*

— and the warning is right about the failure mode while pointing at its own cure.
The objection is to a chainer **that does not compose**. Make a bed an artefact
and it composes: level 3 addresses a bed by name, so beds fold upward as atoms
instead of duplicating the mechanism sideways.

Concretely, a fourth constructor beside the three brains:

```purescript
data TriSnapshot
  = TSGrids Snapshot
  | TSFixed FixedPattern
  | TSTrig  TrigBank
  | TSBed   { lane :: String, bars :: Int }   -- badge "A"
```

It then inherits, for free, everything the bank fold just built: one collection,
naming-as-promotion, star, the G/R/T (now G/R/T/A) badge, the brain filter,
save-to-Amphora, and a content-derived glyph. A bed gets a 2-glyph identity like
every other artefact, which is what makes "jam on the little 2-glyph beats"
possible at the bed level too.

**Wire tag:** a NEW letter, not a reuse. The stored tags are the frozen `M`/`G`/`T`
(where `G` means RYTM); pick something unused — `A` — and note that display and
wire agree for this one only by luck. `parseTri` is lenient, so an unknown tag
silently drops the slot; that is the failure to watch for on the first load after
this ships.

## Decisions taken (AC)

**Nesting: truncate.** When level 3 gives a bed fewer bars than it needs, the bed
is cut off mid-sequence rather than looping or holding. Alignment strategies are
deferred — worth revisiting once beds are actually being played against each
other, not before.

**A bed arms the machine.** *"If you play a pattern-of-patterns it's just as if
you were driving that with the mouse yourself."* So a bed is a user proxy, and
selecting a beat within it does exactly what clicking would: switch the sounding
brain and arm.

That has a consequence worth stating plainly, because it already exists at level 3
and will now exist at level 2 as well:

> **Anything that arms will fight `■ STOP`.** `applyLaneCell` arms on every
> `Load`, and `MacroTick` only acts on step boundaries — so today, stopping mid-bar
> appears to work and then spontaneously undoes itself at the next boundary.

If a driver arms, then a global stop must also stop the driver, or stop does not
mean stop. **Proposal: `■ STOP` clears `macroOn` (and any running bed) as well as
`armed`.** That is one line and it makes the master control honest. Without it,
every level added makes stop less trustworthy. (Cf. the same class of fault
throughout `RIG-ISSUES-2026-08-07.md`: a control that reports success while
something else quietly re-asserts.)

## The audition / assemble switch

AC: *"a switch that determines whether hitting a 2glyph auditions it or puts it in
the assembly, not unlike the hunt/perform distinction in Vetula."*

This is the affordance that is missing today, and it is what makes bed-building
feel like playing rather than typing. One mode toggle on the Balistes surface
governs what clicking a glyph does:

- **AUDITION** — clicking a beat plays it now. You are jamming, hunting for what
  works. (Roughly what clicking does today.)
- **ASSEMBLE** — clicking a beat appends its token to the bed under construction.
  You are writing, and the glyphs are your alphabet.

Same gesture, two intents, one visible switch — the Vetula HUNT/PERFORM shape.
Note this also removes the need to type glyph aliases by hand, which is the thing
that makes lanes cryptic to author today.

The switch belongs on the Balistes surface next to the bands, not inside the
preset modal: the point is to click beats while hearing them, and a modal covers
the bands.

## What has to be built

1. **`TSBed` in `TriSnapshot`** — constructor, badge, `describeTri`, `printTri` /
   `parseTri` arms. Pure model, no wiring; the bank picks it up for free.
2. **A bed clock in Balistes.** Balistes already has a Binnacle clock and a bar
   count, so a local stepper is small. It steps its own lane and applies each token
   locally (switch sounding brain + arm). Truncate at the boundary.
3. **The audition / assemble switch** + click routing on the bank list and the
   band chips.
4. **Bed editing in ASSEMBLE** — the panel stops being a window onto the shell's
   level-3 lane and becomes the editor for the *bed under construction*, with
   BANK-this-bed as its commit action. This is the change that resolves the
   panel's current identity problem: it looks like level 2 and is wired to level 3.
5. **Level 3 addresses beds by name.** Depends on the name-resolution change
   already identified: `recallAlias` matches only `it.alias`, so the quoted names
   the lane placeholder advertises (`"lo house 110" <"trap 140" ~>`) parse and then
   fail to resolve. Beds make this urgent — nobody will address a bed by glyph pair.

## Order

3 → 2 → 1 → 4 → 5 is the order that keeps something playable at every step: get
the jamming affordance first (it is useful with no beds at all), then the local
clock (beds playable but not yet savable), then make them artefacts, then move the
panel, then let the rack address them.

**Before any of it: hear a level-3 lane.** The mechanism is built and has never
been confirmed by ear (`MILESTONE-2026-08-07-rig.md`). Beds reuse the same
recall-and-arm path, so if it misbehaves under a clock, that is worth knowing
before a second sequencer is built on top of it.
