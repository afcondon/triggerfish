# The macro layer is text — structure-as-code for the whole rig

*Design note, 2026-08-04 (AC + Claude). Companion to `DESIGN-tidal-scaling.md` — this
supersedes the **front-end** direction of that note's steps 6–8. The grammar work
(steps 0–4) and the structural convergence (step 6a, `Triggerfish.PatternArg`) stand;
what changes is the surface the macro/song scale is authored on. Prompted by AC's
realization at the frontier: "the structure of a piece — a generative multi-week Eno
installation or Shostakovich's 15th Quartet — is best expressed in text."*

---

## 1. The realization that reframes the frontier

Direct manipulation and text are not competitors — they serve different **objects**:

- **Direct manipulation earns its keep when the object is a *thing you're exploring*** —
  a chord, a voicing. Spatial, tactile, "what sounds good *here*?" This is the Vetula
  micro card, and chips are genuinely right for it. Micro stays chips.
- **Text earns its keep when the object is *structure over time*** — a form, a process, a
  score. Sequential, relational, "how does this *unfold*?" You do not drag widgets to
  compose a quartet's architecture; you *write* it. An Eno installation is a *rule
  system*; Shostakovich's 15th is an *architecture*. Both are structure, not objects.

The line falls almost exactly on the micro/macro seam established in
`DESIGN-tidal-scaling.md`: **things → chips, structure → text.** The chrome was never
going to scale up to the song level. So the macro and song scales (that note's §5.2 and
§9) are authored in **one live-coding text buffer**, not a chip surface.

**The convergence work is what makes this credible.** A text macro layer is not "invent a
language" — the language exists and is byte-stable: `Triggerfish.PatternArg` +
`Triggerfish.Macro.parseLane` + the per-machine Lepidoptera serialisers. `print == parse`.
The text buffer is the *same grammar, written by hand* instead of *derived from chips*.
Step 6a built the substrate that lets text stand on its own.

## 2. The vessel — the macro-Tidal modal becomes the environment (AC)

The **`MTidalSeq` modal** (⌥3, "Sequencing — tidal", `Main.macroPanel`, task #9) is the
vessel. It stops being "a sequencer that arranges glyphs in per-machine lanes" and becomes
"the place you write the piece." Same modal, evolved from chrome-sequencer into a text
live-coding environment. Not a new pane, not a detour through Calypso (§5) — the surface
already earmarked for this scale, growing into what it was reaching toward.

Today `macroPanel` renders one `<input>` mini-notation lane per machine over glyph-tokens
(task #12). Tomorrow it renders (at minimum) a text buffer whose grammar is a superset of
those lanes — the lanes become one *view* of the text, not a parallel authoring system.

## 3. What already exists — the substrate is ~80% there

This pivot is close because three of the pieces are built:

**(a) Named machine-states are already text.** Each machine serialises to a valid-shaped
`Tidal.*` eDSL value — a named, transferable "the file is the music" record:

```
vetulaScene    "lush"  { key: …, sources: [ … ], voices: [ … ] }   -- Vetula.Lepidoptera
odonusPatch    "rain"  { … gen matrix, marbles, quantize … }        -- Odonus.Lepidoptera
balistesPattern "clave" 16 [ lane …, lane … ]                       -- Balistes.Lepidoptera
```

Each module's header states the intent explicitly: *"valid-shaped `Tidal.*` eDSL, so it
drops into Calypso and ships to purerl-tidal."* A **section** is just a *named grouping of
these already-textual states*. (Selene + the Tidal voice still need Lepidoptera modules —
§9 open items.)

**(b) A machine-agnostic arrangement grammar.** `Triggerfish.Macro` (`tokenize` /
`parseLane` / `resolveStep`) already parses `atoms # verb arg` lanes into `(verb, arg)`
pairs and resolves them per macro-cycle, domain-agnostic — "one grammar across the
ecosystem." As of step 6a its argument type is the shared `PatternArg`, identical to the
Vetula card's.

**(c) The section idea already exists in chrome.** `Triggerfish.Scenes` — the scene grid
(columns = scenes, rows = machines, task #11) — *is* "sections as named rig-states,"
rendered as glyphs. The text surface is that grid's **dual**: the same object, written.

What's missing is small by comparison: **group per-machine states into a named section, a
song lane over section names, a buffer to write them in, and a runtime to load a section +
apply its transforms** (§7 — most of which the near-built Vetula overlay engine already
covers).

## 4. The grammar — sections and a song lane, one self-similar language

The buffer is the Lepidoptera/Macro grammar with two additions, both *groupings*, no new
mechanism (this realises `DESIGN-tidal-scaling.md` §9):

```
-- a SECTION groups a named state (+ live transforms) per machine
section verse {
  vetula:  "lush"  # slow 8 # voice <open drop2>
  odonus:  "rain"  # sparse 0.3
  balistes: ~                      -- silent this section
}

section chorus {
  vetula:  "lush"  # transpose 5 # arp "0 2 4"
  odonus:  "downpour"
}

-- the SONG lane sequences section names — the same mini-notation, one scale up
song: verse chorus bridge verse chorus!2 outro
```

Because it is the same grammar, the song lane inherits **all of mini-notation for free**
(`DESIGN-tidal-scaling.md` §9): `chorus!2` (repeat), `<verse chorus>` (alternate each
pass), `[verse chorus]` (share a span), and layer verbs at song scale (`# slow 2` to
stretch a section, `# key <C G>` to lift one). Three scales — notes, forms, sections — one
self-similar language.

**Section body lines are exactly the macro lanes we already have** (`vetula: "lush" #
voice open` is a `Macro.parseLane` step targeting Vetula). So the section is a thin
`{ machine → lane }` map; the song lane is a `parseLane` over section names. Both parse
with machinery that exists.

## 5. Where it lives — Triggerfish, not Calypso (survey-backed)

A survey of Calypso (`music/live-coding/calypso`, 2026-08-04) settles the fork:

- Calypso is **strongly wedded to purerl-tidal specifically**: a single-WebSocket funnel
  (`ws://localhost:3012/ws`), a `.tiderl` grammar built around that engine's
  devices/bindings/cues, and a **live-loop editing model with no composed-structure layer
  built** — its `# Structure` / `section` / `piece` grammar is *reserved and unimplemented*,
  and its scene/shared-state design docs are explicitly parked ("undecided").
- Calypso shares **zero PureScript modules** with Triggerfish and has no Lepidoptera code.
- **Triggerfish already owns the pieces** a whole-rig structure surface needs: the Scenes
  arrangement grid, the machine-agnostic Macro grammar, and the Lepidoptera save-language
  spanning the machines. Calypso *lacks* exactly the layer we're building.

**So the text-macro environment grows from Triggerfish's own Lepidoptera/Macro/Scenes
stack, inside the `MTidalSeq` modal.** Not a Calypso mode.

**Calypso's status (AC, 2026-08-04):** ~90% likely to be **abandoned or radically
overhauled once Triggerfish is done** — "but it is a nice design and I'd like to mine it for
ideas later." So the earlier "two peers converge on one grammar" framing is *off*:
**Triggerfish's grammar leads**, and `.tiderl` is a **design quarry**, not a compatibility
target. Its reserved structure forms (`section verse = arrange […]`, `piece = […]`) and its
`tag`/`byTag` conditional-dispatch scheme are worth *reading for ideas* when we fix syntax
(§9 D3) — but Triggerfish is free to choose the grammar that fits the rig, unconstrained by
Calypso back-compat.

## 6. Compose vs launch — the fate of the grid and lanes (split confirmed, AC)

AC: "I'm not sure where the more traditional sequencer fits in, or if we use it" →
"completely sound analysis — structure vs live-launch." **The split is confirmed**; what
stays open is only the sub-choice in D1 (does the launch grid survive, or does AC perform
by typing?). The principled split is **Ableton's own**, which the rig already lives inside:

- **Arrangement / composition** — *writing* the piece's structure over time. Inherently
  textual (§1). → the text modal. **The per-machine lanes (#12) as a composition tool are
  subsumed here** — they are chrome-for-structure, exactly what text replaces.
- **Session / launch** — *performing* live: jumping between sections, muting a machine,
  re-triggering on the fly. A **grid genuinely earns its place** here; clip-launching is a
  real-time gesture text is bad at.

**Proposed resolution (needs AC's call — D1):**
- The **text modal is canonical** — compose structure here.
- The **scene grid (#11) survives only as a live-*launch* surface** (Ableton Session view,
  not Arrangement), and becomes a *generated view* of the text — the "two views of one
  thing" rule (§8), one scale up. If AC performs by *typing*, the grid retires; if by
  *triggering*, it stays as a launch pad the text mints.
- The **per-machine lanes (#12) are absorbed** into the text buffer as one view.

This is not "keep or kill" — it is "demote the grid to launch-only, subsume the lanes."

## 7. How text drives the rig — the runtime

Parsing is done (§3b). Driving needs one thing per machine: a **verb interpreter** that
turns resolved `(verb, arg)` mods into that machine's state changes. For Vetula this is the
step-6 behavioural work, *already scoped and nearly built*:

- The shell resolves a section's line to `Load "lush" [ {voice, "open"}, {transpose, "0 7"} ]`
  (existing `resolveStep`).
- It recalls the named state (`recallAlias` → `RecallSlot`), then hands the resolved mods
  to the machine, which interprets them in **its own vocabulary** (`Vetula.fxOfVerb`
  produces `PerfFx`; applied as a live overlay folded over every box's pattern). This keeps
  the parser domain-agnostic and each machine the authority on what its verbs mean — the
  Lepidoptera seam ("Vetula owns only the vocabulary").
- `# scale` stays the rig-global harmonic verb it is today (`SetRestingScale`).

So the backend for "Vetula as a text-macro instrument" is the *same* overlay engine we
paused before this note — it is needed identically whether the front end is text or chips.
Other machines (Odonus, Selene) each grow their own `fxOfVerb`-style interpreter as their
verbs are defined; until then their section lines just recall a named state (no transform),
which already works.

**Section/song runtime:** a section transition loads every machine's line at a bar-quantised
boundary (today's `MacroTick`/`applyLaneCell`, generalised from per-machine lanes to a
section map). The song lane advances the *section pointer* on its own (slower) clock —
`resolveStep` over section names, one scale up.

## 8. Relationship to the chips — one rule, three scales

The "two views of one thing" rule (chrome + text hatch, established for the Vetula card)
now holds at **every** scale:

| scale | object | authoring surface | second view |
|---|---|---|---|
| micro | a chord / voicing | **chips** (direct manipulation) | text hatch |
| macro | a machine's lane | **text** (the section body) | lane chrome (generated) |
| song | the piece's form | **text** (the song lane) | launch grid (generated, §6) |

Micro flips (chips primary, text secondary); macro/song settle the other way (text primary,
chrome a generated view). Same principle, the primacy following §1's things-vs-structure
line. The text is always the truth; any surviving chrome is a projection of it.

## 9. Open decisions (for AC)

- **D1 — the grid's fate** (§6): retire it, or keep it as a launch-only Session view that
  the text generates? Conditions how much chrome survives.
- **D2 — buffer granularity:** one buffer for the whole piece (sections + song together), or
  a section-list + a separate song lane? (The example in §4 is one buffer; simplest to
  start.)
- **D3 — mine `.tiderl` for ideas** (§5): Calypso is likely abandoned/overhauled (not a
  compatibility target), so Triggerfish's grammar leads. Still worth a short cross-read of
  `calypso/docs/tiderl-format-design-2026-05-14.md` (its `section`/`piece` + `tag`/`byTag`
  design) to *steal the good parts* before fixing our syntax — a quarry, not a constraint.
- **D4 — editor widget:** plain `<textarea>` to start (fast, ships now), or CodeMirror from
  the outset (task #15 already wants CodeMirror for the lanes; Calypso uses it)? Recommend
  textarea first — prove the loop, then upgrade.
- **D5 — persistence:** the buffer is a Lepidoptera document; where does it save (Amphora
  library entry? a `piece.lepi` file? the between-sessions modal, task #16)?

## 10. Execution sketch (once decisions land)

0. ✅ *(done, step 6a)* one shared `verb + pattern-arg` grammar.
1. **Vetula verb interpreter** (§7) — the paused overlay engine: `fxOfVerb` → live overlay
   folded over every box. Makes a *single* macro line audibly drive Vetula. The one piece
   that needs by-ear validation; unblocks everything above it. (This is the concrete next
   build whenever we resume code.)
2. **A text buffer in `MTidalSeq`** parsing macro lines (§3b) and driving the rig live — the
   thinnest end-to-end proof (`vetula: … ` line → sound). Textarea (D4).
3. **Sections** — group per-machine lines under `section <name> { … }`; a section transition
   loads all machines at a boundary (generalise `applyLaneCell`).
4. **The song lane** — `parseLane` over section names on the slower clock (§4). The piece
   plays itself.
5. **Resolve the grid** (D1) — retire, or regenerate it as a launch view of the buffer.
6. **Round-trip + persistence** (D5) — the buffer is a Lepidoptera document; save/recall it;
   converge its grammar with `.tiderl` (D3).

The recursion is the feature: get the primitives right (small grammar, right objects) and
the same language aimed one level up *is* the composition tool.
