# A unified scene / arrangement modal across all panels

*Design note, 2026-07-28. Forward-looking — the organizing target for an
upcoming workflow design session, NOT a build plan. Builds directly on
[`PRESETS-AND-THE-EDSL.md`](PRESETS-AND-THE-EDSL.md) (the spine) and comes out of
returning to the Balistes ARRANGE work after ~2 weeks away and finding it
confusing — the honest usability signal that the shape wants a rethink, not a
patch.*

## The vision (AC, 2026-07-28)

> There's really a lot to be said for making all the scene — and possibly
> arrangement — setting into a **modal that operates on all panels**. Structure
> it to match the panels: **Odonus, Balistes, Selene, Vetula, Sufflamen,
> Stellatus**. Then the **Tidal pane is purely for seeing the source.**

One surface, identical everywhere, for capturing / naming / recalling / (maybe)
sequencing the state of each machine — instead of the per-machine, per-shape
notions that grew independently. The Tidal page stops being a place you *edit*
and becomes a read-only mirror of the eDSL the modal and panels produce.

This is the same principle `PRESETS-AND-THE-EDSL.md` already fixed:
**consistency does not mean identical machinery everywhere — it means the
differences live in *what an item is*, never in *how it's named, saved, loaded,
or shipped*.** The modal is the concrete home for that "how."

## The central workflow tension (the thing the session must resolve)

> The workflow needs to be thought out carefully, especially between **fast and
> easy creation of presets** and **slower creation that forces naming.**

Two creation speeds, both legitimate, in tension:

- **Fast / performance-time** — capture the current state into a slot *now*, no
  dialog, no name, keep playing. This is today's Balistes CAPTURE→slot and
  Odonus SCENES. Anonymous, positional, disposable.
- **Deliberate / authoring-time** — promote a configuration into a *named*
  library item you'll reach for again, ship to Calypso, or share. Forces a name;
  earns permanence.

The `PRESETS-AND-THE-EDSL.md` cut names these precisely: **scenes/snapshots**
(the fast, "how it's set now", for recall + sequencing) vs **library items**
(the deliberate, authored *content*, "what to play"). The modal has to make both
first-class *and* make the promotion path from one to the other obvious —
without making the fast path slow. That balance is the design problem.

## Resolved direction (2026-07-28): the glyph substrate

The design session (AC × Claude, 2026-07-28) resolved the tension not by
*balancing* fast against named but by **removing the choice from capture time
entirely**. The mechanism is an auto-generated **glyph** — identity without a
name.

### The move

The fast↔named tension only exists if identity must come from a *name*. It
doesn't. Give every capture an auto-assigned pictographic glyph and you get a
handle that is recallable and sequenceable with **zero flow-break** at capture:

- **Capture is always fast + anonymous + glyphed.** One gesture, no dialog, ever.
- **Naming is always later, optional, deliberate** — it lives in the modal,
  where slow is fine. Naming becomes *promotion to a library item*.

Most captures never get named and don't need to — the glyph carries enough
identity to recall and sequence them *before* anyone names them. The glyph is
the bridge that makes anonymous captures first-class. This is the direct
`PRESETS-AND-THE-EDSL.md` cut: scene (glyphed, fast) → library item (named,
deliberate), with the promotion path being "give this glyph a human alias."

### How glyphs are made

- **Content-hash, not a sequential deck.** Derive the glyph from a hash of the
  pattern's canonical eDSL text (slice 5 already gives this — `printTri` /
  `printPattern`). Identical states get identical glyphs (free dedup —
  recapturing the same thing doesn't litter the bank); a real edit avalanches to
  a visibly different glyph (the honest "this changed" signal). No visual family
  resemblance between cousins — accepted, because *changed should look changed*.
- **Machine ≠ content.** Hue comes from the machine (all Balistes one hue, all
  Vetula another — the "four green then one red" read across a row); the icon(s)
  come from the content hash.
- **Pairs, not singles.** A ~60-icon deck gives ~3600 ordered pairs, so hash
  collisions effectively never happen, and absurd pairs ("owl · bomb", "star ·
  ambulance") are *more* memorable than single icons — real bizarre-imagery /
  method-of-loci mnemonics. The pair is one hyphen-joined token (`owl-bomb`, no
  internal space) so it stays a single mini-notation event.
- **Picture + alias, always.** A deck entry is `{ icon, alias, hue }`. The alias
  (`owl-bomb`) is the typeable/text form; the icon is the rendered form. This is
  load-bearing for the macro-Tidal path (you can't type a picture).

### The identity chip — one widget, four jobs

A glyph chip sits in the **same spot on every machine**, doing four jobs at once:

1. **Current identity** — "you are playing ⟨owl · bomb⟩ right now."
2. **Dirty indicator** — nudge a knob and it **ghosts** (the old glyph stays
   faintly visible = "based on owl·bomb, modified, unsaved"), so you always know
   what to get *back* to. With content-hashing the chip can even preview the
   *prospective* new glyph faintly: "who you'd be if you captured now."
3. **Recall menu** — click it → dropdown of this machine's banked glyphs.
4. **Capture target** — the thing a fresh capture mints.

**This chip also fixes the two-transport Stop bug** (see below). That bug is
really *missing feedback* — nothing told you the voice was still parked on the
last state. Once the chip always tells the truth, "why is it still sounding
after Stop?" stops being a surprise: the chip shows the glyph you're parked on.
The clean reframe is that two *orthogonal* axes had been mashed into two on/off
gates:

- **Destination** (routing): Silent / Local / Rig — legitimately separate.
- **Content**: nothing / held-glyph / running-macro — three positions of *one*
  control per machine.

Stop = "content → freeze on the current glyph" (visible in the chip), with
silence being an explicit destination choice. Groovebox-freeze over Tidal-silence
precisely *because* the chip makes the freeze legible.

### The tab bar becomes a six-machine status board

The chips need not hide inside panes. Carry each machine's glyph-pair **in its
tab**: the tab bar stops being a pane-switcher and becomes rig mission-control.
Each tab shows machine-name + current glyph-pair, tinted the machine's hue, in
three states mirroring the content axis:

- **held** — solid glyph-pair (parked on a banked state)
- **diverged** — ghosted glyph-pair (divergence, visible rig-wide)
- **empty** — no glyph (nothing captured)

So at a glance, without visiting anything: "Balistes and Vetula are in unsaved
territory; the other four are on known glyphs." Disambiguation: clicking the
**label** switches pane; clicking the **glyph/caret** opens that machine's recall
dropdown *in place*. Six dropdowns, no pane changes.

### `:`+Tab completion — the typeability keystone

The macro-Tidal arrangement is mini-notation over glyph aliases:

```
balistes  "owl-bomb star-ambulance owl-bomb ~"
vetula    "moon-key <sun-key rain-key>"
```

inheriting `[]` `<>` `*` `~` `,` `!` — repeats, alternation, subdivision, rests,
polymeter — for free. Each token is a *whole-machine-state recall* occupying the
mini-notation "sample name" slot. This is coherent in a way it wouldn't be in a
DAW: the rig is already Tidal/BEAM-native, so an arrangement-of-glyphs is the
same kind of object as the patterns *inside* each machine — Tidal all the way up.

Typeability is solved by an **emoji-picker gesture**: type `:` then letters →
completion popup over that machine's bank (scoped — inside `balistes "…"` you see
only Balistes' glyphs); **Tab** accepts. Because nothing transforms until an
explicit Tab, there is **no parser collision to worry about** — `bd:3` with no
Tab is left verbatim (the only residual is that Tabbing after `bd:3` might
*offer* a glyph you then ignore). `:` is a completion trigger, not reserved
syntax.

**What Tab writes (decided): a canonical token rendered as a glyph decoration,
NOT a literal glyph codepoint.** Tab inserts `:owl-bomb:` (or `owl-bomb`) as
plain text; a CodeMirror atomic *decoration* paints it as the machine-hued
glyph-pair, cursor stepping over it as one unit. It *looks* substituted — same
delight — but the buffer stays plain, portable text. This protects the
Lepidoptera transferable-text invariant the whole persistence layer rests on
(FontAwesome glyphs are private-use codepoints tied to the font — a literal
glyph in the buffer would become tofu anywhere the font isn't loaded, breaking
the "drops straight into Calypso / shares as text" property). The glyph is a
*lens over the text*, never the text itself.

**Payoff — the pictographic mirror.** The read-only Tidal pane renders these
shortcodes as their real coloured glyphs inline, so the arrangement isn't cryptic
source — it's a **pictographic score**, rows of little machine-hued emblems. The
"distinct from a groovebox" pitch made literal.

**Named ≠ glyphed fork dissolves.** A name is just a nicer shortcode.
`:owl-bomb:` is the auto-minted alias; promoting it in the modal renames the
shortcode to `:funk-100:`, which still renders its glyph beside the word. Named
and glyphed are the same object with a better alias.

### The modal is purely between-sessions

Live, you need nothing but the capture hotkey + the tab-bar chips. The modal
(the per-panel-structured surface this note opened with) is browse-the-grid /
rename / promote-to-named / delete-junk / arrange. It is allowed to be slow and
heavy because it is *never in the performance path*. That is the clean split.

### Staging — substrate first

The glyph/chip/bank layer is **beneath and orthogonal** to the Session-view-vs-
macro-Tidal arrangement choice. It resolves capture-tension, recall, *and* the
transport-feedback bug on its own — so build it first and fork the arrangement
style on top of it without betting the foundation on either:

1. **Substrate on one machine (Balistes — its bank already exists):**
   content-hash glyphs (icon + alias + machine-hue), the identity chip in a fixed
   spot, capture-hotkey + ghost-on-mutation + click-to-recall. Validates the
   whole thesis: does the auto-glyph give recall-without-naming, and does the
   chip kill the Stop confusion?
2. **Roll the chip to the tab bar** across all six machines (the status board).
3. **Arrangement layer on top** — prototype macro-Tidal (`:`+Tab, decorated
   mirror). Back off to an Ableton-style global-scene row (recall a tuple of
   glyphs across all machines at once) if per-machine polymeter proves
   unreadable live. Both sit on the same glyph substrate, so it is not either/or
   — a global-scene row can even sit *above* the per-machine macros.

**Honest risks flagged in session:** (a) per-machine macros at different cycle
lengths are glorious in theory and can be unreadable live — the main thing that
would push back toward Session-view predictability; (b) pure per-machine macro
gives no song-like global downbeat, hence the optional global-scene row above.

### Parking lot — REPLAY loops as glyphable clips (NOT first pass)

The Odonus **REPLAY** tab (#151) already lifts "good bits" out of the logbook:
you loop a gold band and `⧉ clip` it into the CLIPS strip — a captured, replayable
loop region. Those clips are *another well of recallable, sequenceable
material*, distinct from the three brain-captures. Long-term they should fold
into the same glyph substrate: a REPLAY clip is just a fourth kind of captured
artefact behind an Odonus glyph, so a macro-pattern could sequence a looped good-
bit exactly like a Grids rhythm or a chord scene. **Explicitly deferred past the
first pass** (AC, 2026-07-28) — get the three-brain glyph substrate + chip +
arrangement working first, then generalise capture to include REPLAY clips.

## Where we are today — the fragmentation to unify

Each machine grew its own answer, which is exactly what the modal collapses:

| Machine | Scene/preset today | Persistence |
|---|---|---|
| **Balistes** | ARRANGE rail: tri-snapshot bank (M/G/T) + sequence (`Snapshot.purs`, `TriSnapshot.purs`) | ✅ localStorage, **eDSL-text** (`Store.purs` v3, 2026-07-27) |
| **Odonus** | `View/Scenes.purs` (SCENES) | ✅ `Store.purs` |
| **Selene** | rack library | ✅ `Store.purs` (eDSL doc `{name, doc}`) |
| **Vetula** | non-persistent "library" | ✗ |
| **Sufflamen** | — | ✗ |
| **Stellatus** | — | ✗ |

So: three different scene notions, three that have none, and one (Balistes) that
also carries a *sequence* on top. The modal is where these converge onto one
vocabulary and one storage discipline.

### Completed this pass (2026-07-27 → 28)

- **Balistes ARRANGE persistence (slice 5)** — `TriSnapshot` gained a text codec
  (`printTri`/`parseTri`, brain-tagged; `TSFixed` reuses Lepidoptera
  `printPattern`), and `Store` grew to a v3 envelope `{library, bank, sequence,
  seqBars}` saved on every rail edit, restored on init (stopped). **This moved
  Balistes onto the eDSL-text discipline** the presets note asked for — closing
  the "serialises to custom JSON, not the eDSL" divergence it flagged.
- **Recall highlight fix** — a recalled GRIDS snapshot now lights the library
  chip matching by name (playback still from the frozen `scratchFixed`), so the
  switcher agrees with what's sounding.

## Two live bugs/smells that are really design symptoms

1. **Stop doesn't stop — two transports.** The ARRANGE rail has *two* gates:
   **Run** (the voice, `sounding`: Silent/Local/Rig) and **▸PLAY/❚❚STOP** (the
   sequence, `seqEnabled`). `advanceSeq` needs both; ❚❚STOP halts the *advance*
   but the voice keeps looping the last-recalled snapshot, so you must *also*
   kill Run to silence it. No relabeling fixes this — the modal must decide **what
   "stop" means for an arrangement** (one gate or two; does stopping the sequence
   silence the voice, freeze on the current scene, or return to a base state?).

2. **Cross-instrument `Widgets` smell.** `Balistes.Widgets` reaches into
   `Odonus.Grid.Widgets` — a shared scene surface wants a *shared* widget layer,
   not one instrument importing another's.

## Open questions — resolved 2026-07-28

The design session answered these; see **Resolved direction** above. In short:

1. **Transport / playback model** — **resolved.** Two orthogonal axes
   (destination: Silent/Local/Rig; content: nothing/held-glyph/running-macro),
   not two on/off gates. Stop = freeze on the current glyph, made legible by the
   identity chip. Arrangement can be per-machine (macro-Tidal, polymetric) with
   an optional global-scene row above — build the substrate first, fork the
   arrangement style later.
2. **The surface** — **resolved (direction).** The always-present surface is the
   **tab-bar status board** (six glyph chips + in-place recall dropdowns); the
   *modal* is the between-sessions catalog (name / promote / delete / arrange),
   never in the performance path. It subsumes Odonus SCENES and the Balistes
   ARRANGE rail by making every machine's bank a glyph bank behind one chip.
3. **The fast↔named workflow** (the crux) — **resolved.** The choice is *removed
   from capture time*: capture is always fast + anonymous + glyphed; naming is a
   later, optional promotion in the modal. The glyph is identity-without-a-name.
4. **Tidal pane → read-only source** — **resolved.** It becomes the
   **pictographic mirror**: shortcodes rendered as machine-hued glyphs inline. It
   removes preset/scene *editing* chrome from the per-machine panels (that moves
   to the chip + modal), leaving the panels for sound-shaping only.

Remaining to settle *at build time*, not design time: the exact icon deck and
hash→glyph mapping; the capture-hotkey binding; and (post-substrate) the
macro-Tidal-vs-global-scene arrangement fork with its live-legibility risks.

## Reading list for the session

- [`PRESETS-AND-THE-EDSL.md`](PRESETS-AND-THE-EDSL.md) — **the spine**: library-item
  vs scene, eDSL as the one format, "consistency ≠ identical machinery".
- [`BALISTES-SELENE-RETHINK.md`](BALISTES-SELENE-RETHINK.md) — one-idea-per-instrument
  decomposition (context for what each panel *is*).
- [`DESIGN-macro-tidal.md`](DESIGN-macro-tidal.md), [`DESIGN-tri-snapshot.md`](DESIGN-tri-snapshot.md)
  — the arrangement-as-song surface Balistes already prototypes.
- Per-machine prior art: `Odonus/View/Scenes.purs`, `Balistes/Snapshot.purs`,
  the three `Store.purs`.
