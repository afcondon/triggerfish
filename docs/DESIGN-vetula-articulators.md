# Vetula articulators — harmonia-backed arrangement, patternable by Tidal

Status: design. Successor thread to Slice 3 (playheads as Tidal, shipped). Depends
on nothing built here yet; wants `harmonia`'s voice-leading engine
(`enumerateVoicings` / `voiceLead`) and reef's segment-clock seam (`PerfClock`).

## Where this starts

Slice 3 made a voice's **read-head** a Tidal pattern of chord indices (which chord,
when). But *how* a chord is sounded is still a fixed enum — `Block | Arp | Strummed`
— hardcoded as the terminal step of the realiser:

```
Axis-A pattern (which chord, when) → chord's notes → RENDERER (block/arp/strum) → MIDI
```

`Strummed` is a smell: it's a bespoke special case (compare to the previous chord,
keep common tones, sustain across the forward run) baked into `reef`'s
`renderVoiceMidiAt`. Andrew's steer: if we special-case Strum, do it **principledly**
— as one member of a family of functions that fold along the chord list, some looking
less-locally than adjacent pairs.

## The reframe: articulator = alphabet supplier; pattern = indices into it

The unlock: an articulator stops being a *terminal renderer* and becomes an
**alphabet supplier**. For each chord it returns an **ordered, labelled list of
notes** (with role / lifetime flags). The note-pattern (Axis B) is then just
**indices into that list** — exactly how Tidal's `n "0 2 4" # scale "major"` works:
`scale` supplies the pitch alphabet, `n` indexes it. Here **harmonia supplies the
alphabet** (the voice-led notes) and the pattern indexes it. That is, literally,
"harmonia feeds Tidal."

Today Strum's output is *not* interpreted by Tidal — it's finished MIDI, the last
step, with no pattern downstream. The recording we remember is "Strum with the
trivial pattern (fire everything on the onset)". The reframe puts a pattern *between*
the articulator and the MIDI.

### Worked example (the payoff)

ii–V–I in C. Ask harmonia's voice-leader for 4 smooth parts → per-chord alphabet
`[bass, v1, v2, top]`:

| chord | 0 bass | 1 | 2 | 3 top |
|---|---|---|---|---|
| Dm7   | D2 | F3 | A3 | C4 |
| G7    | G2 | F3 | B3 | D4 |
| Cmaj7 | C2 | E3 | G3 | B3 |

(F held Dm7→G7 as a common tone; lines move by step — that's the voice-leading.)
Same alphabet, different Axis-B patterns = different instruments:

- `3` → top voice only: **C4 D4 B3** — a melody.
- `0` → bass only: **D2 G2 C2** — a bass line.
- `0 1 2 3` → arp up (a harp); `3 2 1 0` → down.
- `0 3` → outer voices (shell/duet).
- `0 ~ ~ 3` → bass on beat 1, melody on beat 4.
- `3*4` → melody note pulsing.

And each Vetula **voice** has its own Axis-B pattern over the *same* alphabet, so you
stack them — bass block on `0`, melody arp on `3`, inner voices off-beat on `1 2` —
**one voice-led alphabet, a whole arrangement carved out of it by patterns.**

## The four-layer stack

Arranging, factored into composable knobs (Andrew's "cowboy chords → little
symphony"):

| layer | decides | whose job | statefulness |
|---|---|---|---|
| 0. **Reharmonize** (Axis A material) | *which chords* — substitutions, added tensions, passing chords | harmonia | transform on the chord loop |
| 1. **Voice** | per chord, the concrete notes (drop-2, open/close, extensions) | harmonia | joint over the loop (voice-leading) |
| 2. **Articulate** | the note **alphabet** + roles/lifetimes | harmonia | **fold over the known loop** |
| 3. **Pattern** (Axis B) | order / rhythm — indices into the alphabet | Tidal | stateless per cycle |

Slice 3 built the read-head (a distinct Axis-A *timing* pattern: which chord, when).
Layers 1–2 are harmonia's; layer 3 is the note-pattern that indexes the alphabet.
Reharmonization (layer 0) is a separate, later concern — it changes the chords
themselves, upstream of everything here.

## Articulators are folds over a KNOWN finite loop

The enabling fact: Vetula's progression is a **known finite loop**, so an articulator
can scan the *whole* chord list — impossible in open-ended Tidal (a function of time
that can't see ahead), which is exactly why Strum isn't a Tidal primitive. Locality
is a **parameter**, tagging each articulator by its reach:

- **Local** (index only) — `block` (all notes), `thicken` (melody + parallel 3rds/6ths, drop-2 spread).
- **Backward pairwise** — `strum` (entering notes; common tones ring).
- **Windowed** — `pedal` / oblique motion (a tone common to a run of chords holds).
- **Global** — `voiceLeadN` (the N smoothest connecting lines), `featureVoice k` (solo the tenor / top line out of the voice-leading), `counterMelody` (invent a new line in contrary motion, constrained to chord tones).
- **Forward** — `walkingBass` (fill between one chord's root and the next with passing/approach tones).

Strum sits near the bottom of a tall ladder. `strumSustain` already walks *forward*
through the run a note persists — the general fold is latent in the code, just
hardwired.

## Strum re-expressed

Alphabet labels become `held` (common tones ringing) vs `entering` (new notes). Then
patterns do what MIDI-recording Strum can't: `entering: 0 1 2` arps the new notes in;
`~ 0 1` delays them (a suspension); solo the entering top note as a moving line. The
old behaviour = "fire all `entering` on the onset" — the trivial pattern.

## The one knot: pitch vs lifetime

Some articulators (strum-legato, pedal, walking-bass) are partly about **when / how
long**, not just pitch. So an `Alphabet` entry carries a **role / lifetime flag**
(e.g. `sustains-until-it-leaves-the-chord`) beside its pitch. The pattern indexes
pitches; the flag governs ringing when the pattern doesn't re-fire a note. This is
the interesting design decision to nail in the `Alphabet` type — it's tie-able, not a
blocker.

## Why it fits Vetula's spirit

None of this exposes theory. You pick "voice-lead 4 parts", "feature the tenor",
"walking bass" as a **gesture**, apply a pattern, and it sounds like an arrangement —
the voice-leading engine is the hidden coherence layer. Cowboy chords in, symphony
out, no lecture. (Compositional, not theory-exposing — Vetula's founding stance.)

## Build plan

1. **`Alphabet` type + articulator interface** — `articulate :: Array VChord -> Int
   -> Alphabet`, where `Alphabet` = ordered notes + role/lifetime flags. Settle the
   lifetime flag here.
2. **Harmonia articulators** — `block`, `voiceLeadN` (wraps existing
   `enumerateVoicings`/`voiceLead`), `featureVoice k`, `strum` (held/entering).
   Later: `walkingBass`, `pedal`, `counterMelody`, `thicken`. Pure; compiles to node
   + BEAM.
3. **Reef wiring** — realiser becomes: Axis-A pattern → chord index → `articulate` →
   alphabet → **Axis-B pattern indexes the alphabet** → MIDI. Same byte-identical
   `PerfClock` seam, one level finer (index-into-notes nested under
   index-into-chords). Retire `Block | Arp | Strummed` into (articulator ×
   note-pattern); Block = all-notes × stack, Arp = all-notes × sequence, Strum =
   held/entering × onset.
4. **UI** — per voice: an articulator picker (the gesture) + the Axis-B note-pattern
   box (alongside Slice 3's read-head box). Two pattern boxes + one dropdown.

## Sequencing vs the other slices

- Slice 3 (read-head as Tidal) — **done**.
- Slice 3½ — Axis B note-patterns with the fixed `block/arp` set (no harmonia yet).
  A smaller step that proves the alphabet-indexing wiring with the trivial
  all-notes alphabet before harmonia articulators land.
- This note — the full articulator family, harmonia-backed. Build 3½ first (proves
  the seam), then layer articulators in.
- Slice 4 (unify Lab + Performance) is orthogonal layout work; independent.

## Open questions

- The `Alphabet` lifetime/role vocabulary (the pitch-vs-timing knot).
- How the articulator picker and two pattern boxes present without clutter (ties into
  Slice 4's surface).
- Reharmonization (layer 0) — a whole separate design; deferred.
