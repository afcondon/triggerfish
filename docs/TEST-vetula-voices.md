# Vetula voices: a listening test of the ways to play

2026-10-29. Each fragment in Vetula's **Ways to play a voice** has been
checked by reef's parser and the rig's card player, and none has been heard.
This sheet is for hearing them. Mark each row ✓ or ✗, with a note, and file
anything wrong as a CR in `docs/kb/plans/atlantis-crs.md`.

## Setup

- Progression: `book-spa-cloud` (7 chords, one a bar, on the stage). It also
  feeds Odonus's grid today (`odonus/feeds`), which C uses.
- Play each line from Limulus as one voice, e.g.
  `P $ vetula "book-spa-cloud" # arpup 8`, or drag the fragment onto a voice tile.
  Both reach the rig's card player (`Tidal/Vetula/Card.purs`).
- One voice at a time unless the row says otherwise; `hush` between rows.
- A drum loop from Balistes helps hear where the bar is.

How the line is read (from `Reef.Vetula.Lepidoptera`):

- The words apply **in order, left to right**.
- `every` and `prob` attach to the word **just before them**. With nothing
  before them they are dropped, and nothing tells you.
- Ranges clamp quietly: `arpup` takes 1–16, and `strum` takes 0–80 ms.
- `prob` tosses one coin **a bar**, not one a note.

## A. Each word alone

| # | Line after `vetula "book-spa-cloud"` | You should hear | ✓/✗ |
|---|---|---|---|
| A0 | (nothing) | each chord struck once a bar, held, 7 bars round | |
| A1 | `# arpup 8` | each chord as 8 notes a bar, low to high, repeating | |
| A2 | `# arpdown 8` | the same, high to low | |
| A3 | `# arpupdown 8` | up then down. The code repeats the top and bottom notes at the turn (see below) | |
| A4 | `# strum 30` | each chord rolled upward, 30 ms a note, so a 5-note chord takes 120 ms | |
| A5 | `# oct 1` / `# oct -1` | A0 an octave up / down | |
| A6 | `# trans 5` | A0 a fourth up | |
| A7 | `# top 1` | only the highest note of each chord: a melody | |
| A8 | `# bottom 1` | only the lowest: a bass line (f3 g3 b3 d3 ef3 af3 f3) | |
| A9 | `# slow 2` | a chord every two bars; the round takes 14 bars | |
| A10 | `# fast 2` | two chords a bar; the round takes 3½ bars | |
| A11 | `# mute` | silence, and the voice stays on the page as muted | |
| A12 | `# out odo` | silence from the voice; Odonus's output quantised to the chord | |

## B. Words together: order matters

| # | Line | You should hear | ✓/✗ |
|---|---|---|---|
| B1 | `# bottom 1 # oct -1` | A8 an octave lower | |
| B2 | `# arpup 8 # slow 2` | the arp stretched: 4 notes a bar, a chord every two bars | |
| B3 | `# slow 2 # arpup 8` | 8 notes a bar, a chord every two bars (16 notes per chord) | |
| B4 | `# top 2 # arpupdown 4` | the top two notes, alternating | |
| B5 | `# strum 30 # oct 1` | A4 an octave up | |
| B6 | `# strum 10 # strum 60` | only the second strum, 60 ms (the last one wins) | |
| B7 | `# fast 2 # arpup 4` | two chords a bar, each as 2 notes (4 a bar) | |

## C. When: `every` and `prob`

| # | Line | You should hear | ✓/✗ |
|---|---|---|---|
| C1 | `# arpup 8 # every 2` | arp on bars 0, 2, 4…, plain chords on the others. The round is 7 bars, so the arped chords change on each pass | |
| C2 | `# oct 1 # every 3` | every third bar an octave up | |
| C3 | `# arpup 8 # prob 0.5` | about half the bars arped, chosen at random, and the same bars again next time round? (The coin is seeded by bar number. Find out whether that is what you want) | |
| C4 | `# every 2 # arpup 8` | `every` comes first, so it is **dropped**: arp on every bar. Is this a snag? | |
| C5 | `# arpup 8 # every 2 # oct 1 # every 3` | each `every` gates only its own word | |

## D. Patterned arguments (not in the dictionary)

The parser treats an argument as a pattern when it has a space, `<`, `[`, `~`,
`*` or `(` in it.

| # | Line | You should hear | ✓/✗ |
|---|---|---|---|
| D1 | `# trans "<0 5>"` | alternate passes a fourth up. Is it per bar or per pass? Write down which | |
| D2 | `# oct "<0 1 -1>"` | the octave moves bar by bar | |
| D3 | `# top "<1 2 3>"` | a growing top voice | |

## E. Several voices and Odonus

| # | Setup | You should hear | ✓/✗ |
|---|---|---|---|
| E1 | P `# bottom 1 # oct -1`, Q `# top 1 # arpup 4`, R plain | bass, melody and pads locked together, the same chord in all three | |
| E2 | E1, then change one chord in Vetula's score | all three voices follow on the next pass, with no re-evaluation | |
| E3 | S `# out odo`, Odonus playing | Odonus's notes follow each chord. Then load the patch `Odonus-10-32` (`locks: "out"`): Odonus ignores the voice, and the Dashboard draws the route held off | |
| E4 | E1, then `hush` one voice | only that voice stops | |

## What I expect might go wrong

These are guesses from reading the code, not findings:

- **C4:** a leading `every`/`prob` vanishes silently. CR-4's rule says the
  tool must not rewrite the line, so the fix is a mark or a message, not an
  edit.
- **C3:** "half the time" may surprise you, because it is half the *bars*,
  and the same ones each pass.
- **A3, confirmed in the code:** `arpupdown` repeats its turning notes.
  reef's `arpOrder` is `ns <> reverse ns`, so a chord c e g plays
  c e g g e c. Tidal's `updown` is `init up ++ init down`, so it plays
  c e g e. By the rule "err toward upstream-Tidal compat", ours is probably
  the one that's wrong. Listen first.
- `arpup` goes in the chord's **stored** order, not sorted (reef keeps
  voicing order on purpose). The stored chords here go up, so you won't hear
  the difference in this test.
- **B2/B3:** the dictionary says "eight notes a bar". That is only true when
  `arpup` comes after `slow`.
