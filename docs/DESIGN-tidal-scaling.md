# Scaling up Tidal — one grammar at three scales (micro voices, macro arrangement, song)

*Design note, 2026-08-03; decisions resolved with AC 2026-08-04. Companion to
`DESIGN-tank-overhaul.md`. Frames how much of TidalCycles Vetula's Perform cards can
hold, how the answer unifies them with the shell's `Triggerfish.Macro` arrangement layer,
and how the same grammar recurses one level further into **song structure**. No code yet —
this is the grammar we converge on before building.*

---

## 1. The finding that drives everything

Triggerfish already runs Tidal at **multiple scales**, and they are converging on the *same
grammar* — a mini-notation of atoms, then `# verb arg` layers:

- **Micro — the Vetula Perform card.** A pattern over **chord-indices within one voice**,
  transformed by a stack of harmonic verbs:
  `0 1 2 3 # voice open # arp up 4`. Atoms are *chords*; verbs are *voicing / pitch* moves.
- **Macro — `Triggerfish.Macro`.** A lane pattern over **saved forms (glyph-tokens)
  across the whole rig**, transformed by per-instrument verbs:
  `"contemplative" # scale <"F# lydian" "G major">`. Atoms are *forms*; verbs are
  *whatever each instrument decides they mean*.
- **Song — the macro layer recursing (§9).** A lane over **named sections** — each section
  a saved macro-state — so `verse chorus bridge verse pre-chorus chorus chorus outro` is the
  *same* grammar one scale up. Atoms are *sections*; the level above Tidal that makes it
  usable for composed music (AC's decision 5).

Same shape at every scale. This is why "how much Tidal can we hold?" has one answer instead
of fifty — the ceiling is a property of the shared grammar, not of any one surface, and the
grammar is **self-similar**: expressivity comes from *fractal* patterns (patterns of
patterns of patterns), not from a growing pile of verbs.

## 2. Where the headroom actually is (the micro ceiling)

Two independent axes, at very different maturity:

### 2.1 The sequence axis is already rich

The engine under `src/Tidal/**` (AST · Parse · Pattern · Eval · Notation · Chords) already
gives the head segment: rests `~`, groups `[a b]`, alternation `<a b>`, euclid `a(3,8)`,
replication `a!3`, speed `a*2` / `a/2`, degrade `a?`. A large fraction of Tidal
mini-notation is already reachable in the sequence part of a card.

### 2.2 The argument axis is the frontier — and the lever is NOT more verbs

Today each Perform layer takes a **scalar** argument. `parsePerfFx` reads `transpose 7`
with `tokInt` — one integer. In real Tidal **everything is a pattern, including the
arguments**: `# transpose "0 7 <5 3>"`.

The moment a layer's argument can itself be a mini-notation **pattern**, you get
time-varying transposition, alternating voicings, euclidean gating of a verb — an
expressivity explosion **with no new verbs and no new engine**, because it reuses the
`<>` / sequence machinery already parsed by `src/Tidal`.

**The tell: `Macro.purs` already did this.** Its modifier argument type is
`Arg = Lit String | AltArg (Array String)` — the macro args are *already* per-cycle
alternation-valued. The arrangement layer is **ahead of the voice card** on exactly the
axis that matters. So "scale up Tidal (micro)" is precisely: **bring pattern-valued
arguments to the Perform layers**, catching the card up to what the lane already does.

### 2.3 More verbs is the *second* lever, not the first

Beyond scalar→pattern args, Tidal offers structural transforms we do not expose: `rev`,
`off` / `superimpose`, `iter`, `palindrome`, `chunk`, `jux`, `sometimesBy`. Worth having,
but *breadth*. Depth (pattern args) multiplies what the eight verbs we already have can
say, and it is the move that unifies the two scales — so it comes first. **One exception
jumps the queue: `slow` — see §2.4.**

### 2.4 The most-missing verb is `slow` — and it exposes a time-base assumption (AC)

The single most-wanted verb is **`slow`**. You almost inevitably want a harmonic
progression to unfold **much slower than a bar** — unless you are chasing bebop. Two
forces make this the norm, not the exception:

- **Conditioning Odonus.** When a Vetula progression is *conditioning* Odonus (setting its
  harmonic frame), Odonus is running constant variations underneath that **need time to be
  appreciated**. A chord change every bar strobes past that motion; a chord held over 4–8
  bars lets the variation speak.
- **Ambient.** Very slow progressions with **very sparse arpeggios** are a whole register
  the current one-bar assumption can't reach.

Three consequences for the grammar:

1. **`slow N` becomes a first-class verb.** It exists today only *encoded as negative
   `rate`* (`rate -4` = 4× slower, clamped to −8). That is unintuitive and undiscoverable —
   nobody reaches for "negative rate" to mean "hold this longer." Promote `slow N`
   (and `fast N`) to Tidal-canonical verbs; a card should say `# slow 4`, not `# rate -4`.
2. **Rename/retire our `rate`.** In real Tidal `rate` is **sample playback speed** (a pitch
   control), *not* time-stretch — our signed `rate` collides with that meaning. Time belongs
   to `slow`/`fast`; if a pitch/speed control is wanted later it can take the `rate` name
   honestly. **We can make a clean break: there are no saved scenes to preserve** (AC,
   2026-08-04), so `rate` can just become `slow`/`fast` outright. *If* a transitional
   `rate ±n → slow/fast` alias is kept for a while, it carries a `-- TEMPORARY: rate is
   changing, do not depend on this` comment so it isn't mistaken for the real grammar.
3. **The range must go large.** `slow` wants to reach **8 · 16 · 32 · 64** bars, not cap at
   8. The one-bar cycle is the *sequence*'s home; `slow` is precisely how a progression
   stops being a one-bar loop and becomes an arc. (decision 7: does `slow`'s cap come
   off entirely, and does `boxUsesSeq`/the bar-grid need to know a box is multi-bar?)

`slow` is also a clean early win: it is a *time* transform on the whole box, independent of
the pattern-arg work, so it can land first (or alongside step 1) and be felt immediately.

## 3. The unifying grammar — micro == macro

Once **both** layers are `atoms # verb pattern-arg`, micro and macro are the **same
interpreter at two scales**, differing only in what atoms and verbs *resolve to*:

| | atoms resolve to | verbs resolve to | evaluated arg feeds |
|---|---|---|---|
| **micro** (Vetula card) | chord indices → voicings | voicing / pitch moves | `applyLayer` on a `Voicing` |
| **macro** (shell lane) | form names → saved setups | per-instrument commands | the instrument's verb handler |

The shared core is **verb + pattern-arg**, where the pattern-arg is parsed by the *same*
mini-notation parser as the head sequence and **sampled per cycle** to yield the scalar (or
small structure) the verb consumes. This is the Tidal way (`# speed "1 2"`), and it is the
foundation that lets Vetula wire into Macro Tidal as a first-class instrument rather than a
divergent dialect.

## 4. Proposed grammar (the thing to converge on)

### 4.1 A layer is `verb arg? gate?`

- **verb** — a keyword: `transpose · oct · slow · fast · voice · top · bottom · arp · strum`
  (micro; `slow`/`fast` replace the old signed `rate`, §2.4); `scale · bass · …` (macro,
  per-instrument).
- **arg(s)** — a verb has **N positional slots** (`transpose` 1, `arp` 2), each slot a
  **literal** (`7`, `open`, `up`) **or a quoted pattern** (`"0 7 <5 3>"`). See §4.4 for how
  slots + patterns coexist. Quoting is how a multi-token / alternating arg stays one field —
  the same device `Macro.purs` uses to quote multi-word form names.
- **gate** — an **explicit, named** clause over an **extensible predicate vocabulary** (see
  §4.5 / decision 2). *Not* an implicit trailing peel: AC's queasiness about magic
  `every N` (§4.3) is right — a gate is absent by default and, when present, always written
  out, so the parser never has to guess where the arg ends and the gate begins.

### 4.2 Canonical text (round-trip)

`printLayer` renders a literal arg bare and a pattern arg quoted:
`transpose 7` · `transpose "0 7 <5 3>"` · `arp up 4 every 2`. The pipeline stays
`seq # layer # layer`, `#`-split (mini-notation and quoted args never contain `#`). The
reconciliation invariant is unchanged in spirit:
`parsePipeline (printPipeline box) == { seqText, stack }` — but `stack` layers now carry a
`PatternArg`, not only an `Int`.

### 4.3 The one parser subtlety

`parseLayer` currently tokenizes on spaces and peels a trailing `every N` off the last two
tokens. A quoted pattern arg contains spaces and `<>`, so the tokenizer must treat a
`"…"`-delimited span as a single token. With the gate made **explicit** (§4.5) the fragile
trailing-peel disappears; the tokenizer's only job is quote-aware splitting. That is the
whole grammar cost of pattern args; everything downstream (evaluate the pattern per cycle,
feed the verb) reuses `src/Tidal`.

### 4.4 One uniform `PatternArg`, positional slots (decision 3, RESOLVED → B, CONFIRMED)

*Confirmed with AC 2026-08-04 after a side-by-side: the surface syntax is identical under
typed-slots (A) and uniform-string (B); B wins on live leniency (bad token → default, not a
dropped layer), one parse path, and literal micro==macro (it IS the `Macro.purs` arg model).
A's only real edge — typed editor affordances — is recovered by letting a verb declare its
slot KINDS for the UI only, not for parsing.*


Verbs take different arg *types* — `transpose`→Int, `voice`→Shape, `arp`→Dir+Int. To make
them all patternable without a type per verb, **one `PatternArg` = a mini-notation pattern
over string atoms**, and each verb **interprets** the sampled string at apply-time
(`parseVoiceShape "open"`, `fromString "7"`, `parseArpDir "up"`; invalid → the verb's
default, Selene-lenient). `voice <open drop2>` and `transpose "0 7"` then parse *identically*
— which is exactly `Macro.purs`'s domain-agnostic string-arg model, so micro and macro
become **literally the same code**.

Multi-arg verbs use **positional slots**, each its own `PatternArg`:
`arp <up down> <4 8>` (direction pattern, rate pattern). A verb declares its slot count; the
tokenizer fills slots left-to-right, a `"…"`/`<…>` span counting as one slot. This keeps
`arp <up down> 4` unambiguous (slot 1 patterned, slot 2 literal).

*Cost of B:* a bad token defaults instead of failing at parse time. For a live instrument
that is the right trade (you hear the default, fix it, move on) and it matches the existing
lenient parsers. *Decision 1* (thin `PatternArg` wrapper vs reusing the engine `Pattern`)
rides on top of this and is provisional — collapse to the engine type if the wrapper fights
it.

### 4.5 The gate as an explicit, extensible predicate (decision 2, RESOLVED)

The gate is a **named clause with an open predicate vocabulary**, never an implicit peel:

- trivial built-ins: `every N` (each N-th cycle), `prob P` (probability), the default is *no
  gate*;
- open to real predicates over the transport/world — `fullMoon`, `afterBar 16`,
  `everyOtherChorus` — resolved as small PureScript functions `Context -> Cycle -> Boolean`.

The guardrail (AC): **this must not tax the simple language.** A layer with no gate reads
exactly as today; the predicate slot only appears when you write it. So the ambient-piece
dream ("plays differently on the full moon") is reachable *without* making `# arp up 4` one
character more complex. Syntactic shape TBD (a leading marker like `? fullMoon`, or a
keyworded ` when fullMoon`) — pick whatever keeps the gate visually distinct from the value
args so §4.3's tokenizer stays trivial.

## 5. What each level needs

### 5.1 Micro (Vetula card)

- `PerfFx` args generalise from `Int` to a `PatternArg` (a literal or a parsed mini-notation
  pattern over ints / shape-tokens).
- `parsePerfFx` / `printPerfFx` gain the quoted-pattern branch; `parseLayer` gets
  quote-aware tokenizing.
- `applyLayer` samples the arg pattern at the current cycle before applying the verb.
- The chip UI stays the second view: a pattern-valued arg shows as a compact glyph (e.g.
  `⟨0 7⟩`) with the text hatch as the full form — the two-views-of-one-thing rule holds.

### 5.2 Macro (shell lane)

Already has the parser (`tokenize` / `parseLane` / `resolveStep`) and a per-machine lane UI
(task #12). Open frontier: the **modal sequencer** (task #9), the **deferred grammar**
(weights `@`, replication `*`, multiple lanes, nested `<>`), and **per-instrument verb
interpretation** (the parser is deliberately domain-agnostic — each instrument must say what
its verbs mean). Vetula becoming a macro instrument = its verbs (`scale`, voicing moves)
resolving against a loaded form.

## 6. Decisions (resolved with AC, 2026-08-04)

1. **Arg representation — thin wrapper, provisionally.** A thin `PatternArg = Lit … | Pat …`,
   parsed once, sampled per cycle (keeps `applyLayer` pure, round-trip cheap). AC: "could be
   premature optimisation" — so treat it as provisional; if the wrapper fights `src/Tidal`'s
   `Pattern`, collapse onto the engine type. Rides on top of decision 3.
2. **Gate — explicit and extensible (§4.5).** Not folded into the value pattern, not an
   implicit peel: a named clause over an open predicate vocabulary (`every N`, `prob P`, and
   real PureScript predicates like `fullMoon`). AC wants the expressivity (ambient pieces
   that play differently on the full moon) **but not at the cost of the simple language** —
   so a gate is absent by default and always explicit when present.
3. **Shape-valued patterns — one uniform string `PatternArg` (§4.4).** `voice <open drop2>`
   and `transpose "0 7"` parse identically; the verb interprets the sampled string, invalid →
   default. Multi-arg verbs use positional slots. This *is* the `Macro.purs` model, so it
   makes micro == macro literal.
4. **Quoting — quote-only-when-needed.** Bare `7`, quoted `"0 7"`. The printer decides by
   whether a slot is a single literal.
5. **How far to chase Tidal — both, in order.** Pattern-args (depth) first; the verb-breadth
   set (§2.3) as a later à-la-carte slice. **And keep going up:** the real target is the
   *level above Tidal* — a song's structure as tidy as
   `verse chorus bridge verse pre-chorus chorus chorus outro` (§9). Composed music, not just
   loops.
6. **Complexity budget — small language, right primitives.** Depth (richer args) not width
   (a wall of verbs); `learn-the-chrome ≈ learn-Tidal`. AC: "if the primitives are right the
   language can be small and still very expressive." Every added verb must earn its chip
   (`slow` earns it on sight).
7. **`slow`'s ceiling — no cap (but no BigInt).** `slow N` is unbounded within a plain `Int`
   (AC: "no cap at all, but we're not going to need BigInt"). Sub-question **SETTLED**
   (2026-08-04, by reading the scheduler): `scheduleBox` queries `boxPattern` one cycle at a
   time over `[c, c+1)`, with `c` the *absolute* bar count (`tick.index / 16`). The engine's
   `slow`/`fast` map arcs correctly, so a `slow 64` pattern samples cleanly across 64 bars
   with continuous phase — **`slow` composes on top of the existing one-bar scheduling; no
   bar-grid change, no cap.** The old `−8..8` clamp on `Rate` was arbitrary.

## 7. Execution sequence

0. ✅ **`slow` / `fast` as first-class time verbs** (§2.4). Done — `slow N`/`fast N` replace
   signed `rate`, clean break, no cap. Plus (unplanned, but on-theme): arp rebuilt as a
   genuine pattern transform (`arpeggiate`) + onset-guarded sink so it composes with `slow`.
1. ✅ **Quote-aware `parseLayer`** + a `PatternArg` type (`Lit`/`Pat`); printer round-trips
   literal vs quoted (`tokensQ` keeps quotes, `mkArg`, `printArg`). Reconciliation invariant
   holds (test harness green).
2. ✅ **`applyLayer` samples the arg pattern per cycle** — `withSampledArg` (engine); the
   value verbs (`transpose`, `oct`, `voice`, `top`, `bottom`) now take pattern args
   (`transpose "0 7 <5 3>"`), sampled at each chord's onset. Arp also has `arp "0 1 2"` (its
   own `arpWith`, since it explodes time). *Still literal-only: `slow`/`fast` and arp's
   dir/rate slots — pattern-valued time verbs are semantically fiddlier, deferred.*
3. ◐ **Uniform string args + positional slots** — value verbs done (each one `PatternArg`);
   the multi-slot case (`arp <up down> <4 8>`) still pending (arp keeps the `Arpg` dir+rate
   form alongside the `ArpP` figure). Decision 3 / §4.4.
4. ◐ **Explicit extensible gate** (§4.5) — done as a small ADT: `every N`, `prob P`
   (deterministic per-cycle hash), `afterbar N` (build-up), each a pure `cycle → Bool`
   via the engine's `whenCycle`/`cycleRand`. The gate is an explicit trailing named
   clause (safe now args are quoted). *Still ahead: the OPEN predicate registry / world
   context (fullMoon) — needs a `Context` threaded to `applyLayer`.*
5. **Chip view for pattern args** — the compact glyph + text-hatch pairing.
6. ◐ **Converge micro and macro** — factor the shared `verb + pattern-arg` core so
   `Triggerfish.Macro` and the Vetula pipeline share one evaluator; Vetula becomes a macro
   instrument. *Structural half done (2026-08-04):* `Triggerfish.PatternArg` now owns the ONE
   arg type (`Lit`/`Pat`), the quote+angle-aware tokenizer, `printArg`/`glyphArg`/`mkArg`, and
   the macro-scale per-cycle `sampleArg` (verbatim Macro's old `resolveArg`). `Macro.Arg`
   collapsed onto it (`AltArg <a b>` → `Pat "<a b>"`, multi-word alternatives preserved);
   Vetula's card, the Lepidoptera document, and the arrangement lane now parse ONE grammar —
   `voice <open drop2>` / `transpose "0 7"` round-trip through `Macro.parseLane`. No behaviour
   change (round-trip tests green; the macro `sampleArg` path is byte-identical). Vetula's
   per-event Tidal-depth projection (`argEval`) stays App-side so `Macro` keeps no engine dep.
   *Behavioural half still ahead:* macro-lane verbs (`# transpose`, `# voice`) actually driving
   Vetula state — needs new component queries + a Vetula verb interpreter.
7. **Macro frontier** — modal sequencer (#9), deferred grammar (`@` / `*` / multi-lane),
   per-instrument verb tables. Lands on the unified foundation.
8. **The song level (§9)** — sections as named macro-states, a song lane over section names.
   The level above Tidal; reachable once the macro frontier stands.

## 8. What this is NOT

- Not a rewrite of the mini-notation engine — `src/Tidal` already has the `Pattern` type and
  parser; this exposes it to *arguments*.
- Not a promotion of the chip UI to a second language — chips stay the discoverable view of
  the one text pipeline.
- Not the macro sequencer itself — that is task #9, which this note sets the foundation for
  but does not do.

## 9. The level above Tidal — songs as macro-of-macro (AC's decision 5)

The goal beyond loops: **composed music**, where a whole song's structure is as tidy as

    verse chorus bridge verse pre-chorus chorus chorus outro

The architecture already contains this — it is the macro layer **recursing one scale**, no
new mechanism:

- A **section** (verse, chorus, bridge) is a **named macro-state**: a saved bundle of the
  per-instrument lanes across the whole rig (close to what a "scene" already is in the scene
  grid, #11). Give it a name.
- A **song** is a **macro lane whose atoms are section names**, read by the *same*
  `atoms # verb arg` grammar: `verse chorus bridge …`.

Because it is the same grammar, song forms inherit **all of mini-notation for free**:

    verse chorus!2 bridge          -- chorus twice
    <verse chorus> outro           -- alternate the opener each pass
    intro [verse chorus] outro     -- verse+chorus share a span

…and the layer verbs apply at song scale too — `# key <C G>` to lift a section, `# slow 2`
to stretch one. Three scales — notes, forms, sections — **one self-similar language**. This
is the payoff of getting the primitives right (decision 6): the small grammar, aimed one
level up, *is* a composition tool.

**What it needs** (later — after the macro frontier, §7.7): a way to **name and save a
macro-state as a section** (likely folds into the scene grid / the between-sessions modal,
#16), and a **song lane** surface (one lane above the per-instrument lanes). Verbs at this
scale (`key`, `repeat`, `slow`) are a small per-section interpreter — the same domain-agnostic
`(verb, arg)` resolution `Macro.purs` already yields. No new engine; the recursion is the
feature.
