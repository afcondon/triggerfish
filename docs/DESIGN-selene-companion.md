# Selene — extracting the modular companion

**Status: design. AC + Claude, 2026-08-09, from a testing pass on the unified
router.**

This consolidates four threads that arrived within an hour of each other while
testing, because they turn out to be one thread. In order of appearance they
were: re-routing sixteen Balistes lanes is tedious; recall of good module
mappings would be high leverage; the router should probably be its own app; and
Selene has always been the odd one out. The last of those is the answer to the
first three.

Read `DESIGN-routing-backward.md` first — the output-backward view is the
substrate for everything here, and this document assumes it rather than
restating it.

## The move

> "i'm sort of thinking that we should split out the whole routing business as a
> separate app somehow, could be running in another tab, Calypso could also be
> using it… i'd try to get all the modular specific stuff in there, including
> scans of the faceplates and so on."

> "if you think of Selene's envelopes as downstream of regular Triggerfish
> emissions… i think Selene itself moves wholesale. It's always been a bit of an
> odd one out. Perhaps that suggests that we name the new piece Selene and just
> extract and grow it into the modular companion."

**Selene becomes the modular companion app.** It takes the routing table, the rig
model, device configuration, the output-backward view, and everything that knows
what a QuadDrum is. Triggerfish keeps the sequencers and becomes a pure MIDI
instrument.

### Why Selene moves wholesale

Firing an envelope looks like a coupling between Selene and Odonus, and isn't.
It decomposes into three concerns with three homes:

| concern | owner |
|---|---|
| what envelope 2 **is** | Selene |
| that head II **reaches** it | the routing table (Selene) |
| **sending the note** | Triggerfish |

Only the middle one is shared, and it is shared as data rather than as a call.
The envelope is downstream of an ordinary emission: Triggerfish sends a note to a
port and a channel, exactly as it would to any synth, and the FH-2's
configuration — which Selene wrote — turns it into a contour. Triggerfish never
needs to know an envelope exists.

### The name was already telling us

Balistes, Odonus, Sufflamen, Vetula and Stellatus are all triggerfish. Selene is
a jack — a different family. It has been in the water but not in the family
since it was named, which is a fair description of a thing that was never a peer
of the machines it sat beside in the tab bar.

## Triggerfish becomes a pure MIDI app

> "but Triggerfish could then be a completely pure MIDI app (maybe?) with an
> ADD-ON modular affordance"

**It nearly is one already, at the wire.** `Routing.Model.wireOf` reduces every
destination Triggerfish can currently reach to a MIDI note on a port: an FH-2
envelope is a note on channel N of the FH-2's port; an FH-2 gate is a note on
channel 10 whose *pitch* is a selector. Only the two ES-9 kinds return `Nothing`,
and they have no emit path at all today.

So the emission layer needs no change whatsoever to become "pure MIDI" — it
already is. What is modular is not the sending but the **meaning**: knowing that
FH-2 port channel 3 *is* envelope 3. Meaning is precisely what moves.

**The test for whether this is an add-on or a dismemberment:** does Triggerfish
still make sense with the modular part removed? Odonus, Balistes and Vetula
driving MIDI synths is a complete instrument, and it should run with a default
table of channels 1–16 and no Selene in sight. **If the MIDI app ever needs the
companion in order to make a sound, the line has been drawn in the wrong place.**

A side effect worth wanting on purpose: a pure-MIDI Triggerfish is the first part
of this that is **shareable**. Nobody else has this ES-9/FH-2/QuadDrum rack;
everybody has MIDI.

## What cannot move, and why that is fine

Two things are stuck in the emitting page, and being exact about why is what
decides the whole architecture:

- **WebMIDI is per-page.** A second tab gets its own `MIDIAccess`. It cannot emit
  on the first tab's behalf.
- **`Routing.Monitor` is a monkeypatch** on *that page's*
  `MIDIOutput.prototype.send`. It cannot observe another tab's traffic.

And decisively: **browsers throttle timers in background tabs**, so whichever tab
emits must be the one being looked at. That rules out the tempting design where
the routing app owns the wire and music apps send it abstract events.

### So the boundary is drawn at cold versus hot

Everything Selene wants changes at **human speed** — the table, the rig
inventory, module profiles, faceplate scans, recipes, the backward view. None of
it is in the note path. Emission stays with the music app. Traffic crosses back
as a **low-rate aggregate rather than an event stream**: `Routing.Monitor`
already accumulates hits, offs, velocity range and recency per destination, so
publishing that a few times a second fills the backward view's traffic column and
puts nothing real-time across the boundary.

That seam is not invented for this. `Routing.Model`'s own header already asserts
it: *"everything here is placement, and deliberately NOT part of any machine's
musical model."* The app split follows a line the code already draws, which is
the best evidence available that it is the right line.

## Declared outputs — the manifest

> "i think the music making apps would have to declare their outputs for this
> putative new app to base the mapping on"

This is the key move. Today `Routing.Model.Source` is a **closed ADT** naming
Triggerfish's four machines, which cannot serve Calypso or anything else. It
becomes an open, published list.

Half the discipline already exists: `sourceKey` is there, and its comment already
says *"this is a wire format — changing one orphans that row's saved routing."*

**A manifest entry should declare capability, not just identity.**

```
{ key: "odonus.head.1"
, label: "Odonus II"
, app: "triggerfish"
, requires: ["pitch", "gate"]      -- what this source needs to be played properly
}
```

A names-only manifest gets you the table. A manifest of *shapes* gets you "this
recipe can drive Odonus fully and Balistes partially", which is the difference
between rendering a mapping and computing one. It is the same `requires :: Set
String` idea the four-layer destination family already uses.

## Voices, not jacks

> "using QuadDrum with pitch and envelope CV — 4 trigger outputs and four pitch
> CV outputs and four mod CV outputs would make the QuadDrum a very capable four
> voice synth and a good match for Odonus. But also 4 QuadDrum triggers and 4
> Rample triggers and 8 mod CV triggers would be pretty powerful too… 4 voices
> for Saich, 1 voice and 4 LFOs or envelopes for Plaits etc etc."

Every one of those is a **voice architecture**, not a list of outputs:

| recipe | shape |
|---|---|
| QuadDrum as a 4-voice synth | 4 × {gate, pitch, mod} |
| QuadDrum + Rample + mod | 8 × {gate} + 8 modulation destinations |
| Plaits | 1 × {gate, pitch, level} + 4 modulation destinations |

And the machines have shapes too — Odonus is four heads needing `{pitch, gate}`,
Balistes is sixteen lanes needing `{gate}`. **"A good match for Odonus" is a
shape comparison AC is already doing in his head**, and it is the operation worth
making the software do.

This also subsumes the bulk-routing problem below: a bank of *voices* handles
both the pitched and the trigger-only case, where a bank of *jacks* needs
special-casing for each.

It is worth noting that the FH-2 already thinks in these terms. Its
`apply-drumkit` verb declares voices as `gateBank`/`gateSlot` +
`pitchBank`/`pitchSlot`, and `drumVoiceSpec` configures one MCV to emit a gate on
one jack and a 1V/oct pitch CV on another. Its comment describes the canonical
layout exactly: *"the gate jack is on the FHX-8GT (jacks 65..128) and the pitch
jack is on the main panel (1..8)."* The voice abstraction is not being imposed on
the hardware; it is being read off it.

## Banks and bindings — the bulk-routing problem

> "how tedious it would be to change the routing of all 16 channels of Balistes
> from MIDI to FH2… whatever we choose should be hardware independent (if user
> has a different MIDI-to-CV module it should handle that) [and] not 100%
> hard-wired (should still be possible to send the Kick only to MIDI or
> something)."

The tedium is not that there are sixteen edits. It is that **one fact about the
rig has to be spelled out sixteen times**. The table lost the intent, and
re-typing it is the interest payment. So express the regularity rather than
automate the typing.

A **bank** is a named, ordered set of destinations — which is what makes it
hardware-independent by construction. Nothing in the concept knows about Expert
Sleepers.

Banks are not all 1:1 with lanes, and that asymmetry is real hardware:

- an **FH-2 gate bank** is sixteen destinations for sixteen lanes — one jack each;
- a **MIDI drumkit** is *one* destination for sixteen lanes — one channel, lanes
  distinguished by note, exactly as a drum machine expects.

So a bank carries how it spreads: `PerLane (Array Destination)` or `Shared
Destination`. That single distinction makes "all drums to the FH-2" and "all
drums to MIDI ch10" the same gesture over different hardware.

**On not being hard-wired**, a binding is a default a lane can depart from in the
two ways that matter: a lane can **add** a leg (kick to the FH-2 *and* to MIDI —
the doubling case, which will want a calibration offset so it does not flam), or
**replace** its inherited leg entirely (kick to MIDI only). Both survive
re-binding the other fifteen, which a bulk-edit button cannot offer: re-running
it would clobber the exception every time.

### The discipline that keeps this from being a second truth

The binding lives **inside** the table, and there is exactly one

```purescript
resolve :: Table -> Array (Source /\ Array Leg)
```

Everything downstream — emission, `claims`, the backward view — consumes only
resolved legs and never learns that bindings exist. **The binding is a
compression of the table, not a parallel table.** If a second caller ever peeks
at bindings directly, the design has failed.

It sharpens the backward view too. A conflict currently reads "two claimants on
jack 3"; with bindings it can read "the drum binding wants jack 3 and so does
Odonus II's envelope" — naming the *policy* in collision rather than two rows
that happen to collide, which is usually the level you want to resolve at.

## Labels as references, and recipes as artefacts

> "perhaps the intended destinations of each output could be labelled and would
> act as a guide when repatching"

Yes — and make the label a **reference**, not a string. `ES-9 CV 3 → QuadDrum ·
voice 2 · pitch` reads identically as text but additionally gets you:

- **checkability** — a gate output assigned to a v/oct input is an error the
  software can name;
- **the inverse question** — "which of QuadDrum's inputs are still unpatched?",
  the exact mirror of the free-jack question the backward view already makes
  first-class;
- **collision** — two recipes both wanting ES-9 CV 3 go through the same `claims`
  machinery as everything else.

The cost is a small table of module input surfaces. It should be **data in
Amphora, not code**, since it grows every time a module is bought. Note that msm
and the printable recipe cards know about these modules already, but they know
about *samples and technique*, not input surfaces — so this is probably genuinely
new data. Worth checking before writing it rather than after.

**One structural decision cannot be deferred**, because retrofitting it is
expensive: labels must live in a **named, loadable recipe**, not as annotations
scattered on rows. If they are per-row properties of current state, you get a
guide for the patch you are in and can never say "load the QuadDrum four-voice
setup" — and recall is the entire point. Same promotion idiom as Selene's racks:
build it, name it, it joins the library. That costs nothing to honour while the
labels are still dumb strings.

## Storage — Amphora for artefacts, an API for the live table

> "As for storage, let's use Amphora in common?"

Yes, with one division.

**Amphora is right for artefacts.** It is a content-addressed store — hash,
label, payload — and its own header says callers treat unreachability as *"fall
back to the local library, never fatal."* That is library semantics, and it fits
everything that should be recallable: recipes, module profiles, faceplates,
racks, envelope shapes.

**Amphora is wrong for the current routing table.** That is mutable, single-writer,
many-reader, and wrong if stale in a way an artefact never is. It also cannot
live in either app's `localStorage` — which is where it lives today — because the
apps are different origins (`:3023` and `:3061`), and `BroadcastChannel` will not
cross that either.

### Which means Selene wants the Calypso shape

A frontend plus a small API. And that turns out to solve a problem which
currently has **no answer at all**.

`DESIGN-routing-backward.md` calls device liveness "step 0, the cheapest item
here, a few lines". That is wrong, and the reason is instructive: Triggerfish has
no path to the daemons whatsoever. `device-status` lives on
`~/.fh2/control.sock`; a browser cannot open a Unix socket; Triggerfish's only
rig channel is the BEAM WebSocket on `:3012`.

**Selene's API is the missing piece.** It is the one component that legitimately
needs to reach fh2-daemon and es9-daemon, because it owns device configuration.
Publishing a polysignal, applying a drumkit, polling liveness, greying a dark
device's block in the backward view — all of it lands on the same server, and
none of it belongs in a music app.

So the split is not tidying. **It is the thing that makes the backward view's
step 0 buildable.**

## Adjacent: replaying voices one at a time

> "one of the very nice features of this system would be the ability to replay
> four voice parts one voice at a time to a mono-synth, or even to replay just
> one voice but record it with different params and possibly envelopes or LFOs"

Recorded here because it is the mirror image of the recipe idea: **recipes trade
jacks for polyphony in space; this trades takes for polyphony in time.** Same
shortage, opposite solution — which is decent evidence that the voice is the
right unit for both.

Most of the machinery exists. `Leg` already carries `on :: Boolean`, so "play
head II only" is a predicate over the table, not new state. A **pass list** is an
ordered sequence of (mute mask, scene) advanced on a bar boundary — and the scene
per pass is what gives the second half: the same voice recorded four times with
different envelopes, different LFO depth, different Selene bank. Odonus has
scenes; Selene has banks; a pass is a pointer at those plus a mask.

Structurally that is a sequencer of sequencer states, which is the piece-scale
direction purerl-tidal has been circling, reached from the performance end rather
than the language end.

**The honest constraint is that Triggerfish does not record** — Ableton or
LoopyPro does. So it splits into a part it owns (advancing passes deterministically
on a musical boundary, recalling the scene, counting in) and a part it must
delegate (punch-in, take management). The seam: Triggerfish should **emit the
boundary** — a marker note, a CC, a Link-anchored bar count — rather than drive
the DAW's transport, so any recorder can follow. That also makes it testable with
no DAW at all: the passes are correct if the boundaries land where they should,
which the monitor can already observe.

## Requirements from the other applications

> "could we make Selene work with something that was very much more traditional
> TidalCycles? like say we made a super simple Tidal live-coding editor… Clearly
> we could also make much simpler apps like 'just Grids' or 'a 303 bass clone'
> too"

A Tidal editor is the sharpest available test, because Triggerfish's fixed
machines hide assumptions it breaks immediately. Five things fall out, four of
them real gaps in the model as designed above.

**1. Polyphony and voice allocation.** Odonus heads are monophonic by
construction — one cursor, one note. A Tidal `d1` playing a chord is not. So a
destination must be able to say *"I am an N-voice group; allocate across me"*,
and `Destination` has no field for it. The hardware already does: `McvSpec`
carries `voices :: Int` and `stride :: Int`, so one FH-2 MCV can voice-steal
across a run of outputs. **This is the biggest thing a Tidal editor needs that
Triggerfish never asks for, and it is much cheaper to design in now than to
retrofit once recipes exist.**

**2. The capability vocabulary must be open.** A 303 voice is `{gate, pitch,
accent, slide}`; Plaits is `{gate, pitch, level, timbre, morph, harmonics}`. So
`requires :: Set String`, never a closed `data Capability` — consistent with what
the four-layer destination family already settled on. The 303 adds a wrinkle
worth not flattening: *slide* is portamento, a property of how the destination
**behaves**, not of the note. Some roles are "how", not "what".

**3. Manifests are live, not declared at load.** Tidal's `p "bassline" $ …`
creates named sources at runtime. So an app republishes as it goes, and the table
must **tolerate sources that vanish rather than garbage-collecting their
routing** — the name will come back tomorrow and should find its jack still
assigned. Half-true already, since `sourceKey` is a wire format and a missing key
falls back to default.

**4. The contract is JSON, not a PureScript module.** This decides whether Selene
is reusable or merely Triggerfish's sidecar. A "traditional Tidal editor" might
be Calypso, or vim plus tidal-cli, or purerl-tidal itself — which emits from
Erlang on the BEAM, not WebMIDI. If reading the table requires importing
`Routing.Model`, then "any app" means "any Halogen app". **Selene serves the
table over HTTP as plain JSON and has no opinion about who emits.**

**5. Inline pattern params select the SOURCE, never the destination.** Tidal can
express routing in the pattern text — `# orbit 3`, `# midichan 5` — which
competes directly with an external table. That is the two-truths failure this
whole effort exists to remove. The rule: `# orbit 3` is an *identity* claim,
meaning "this event comes from orbit 3", and Selene alone decides where orbit 3
lands. It preserves single-truth and matches how SuperDirt users already think,
since an orbit is a bus you route rather than a destination in itself.

### The leverage, stated plainly

"Just Grids" is a manifest of three lanes and nothing else — no FH-2 knowledge,
no MCV allocation, no rig model. **Selene turns "make a sequencer for my modular"
from a rig-integration problem into a pattern-generation problem.** A
single-purpose app becomes a weekend project instead of a subsystem that has to
learn the hardware. That is a larger argument for the split than any amount of
tidiness.

### Non-goal: controls to parameters

Selene routes **emissions to physical outputs**. It must refuse to route
**controls to parameters** — the Twister sending CC 5 to Odonus's spread knob is
the controller layer, it belongs with the music app, and its lifetime is a
performance rather than a patch. Letting it in makes Selene "everything about the
rig", which is unbounded and will eventually swallow the machines it was
extracted from.

## Decomposition and multi-machine jamming

> "decomposing the Triggerfish app could be very cool given that we have Link via
> link-spike — one way i could see this working is to have them separate but
> another would be to have the machines EXCLUSIVE to somebodies computer (just
> disabling in all the others) making any configuration of live jamming possible
> tho Vetula harmonic context to a remote app would be a challenge."

Once machines are separable and the rig knowledge lives elsewhere, the machines
can live on **different computers**, sharing tempo and phase over Ableton Link
(`link-spike`, UDP 20808). Any allocation of machines to players becomes
possible.

**Exclusivity is a claim, and claims already exist.** es9-daemon has capability
and overlap checks with `!` eviction; fh2-config has `PortClaim`;
`Routing.Model.claims` reports output contention. Machine ownership is the same
shape one level up, on the **source** side rather than the destination side:

> **the manifest becomes a claim.** An app publishes "I claim `odonus.head.0..3`";
> Selene grants it, or refuses because another machine holds it. A refused
> machine renders disabled rather than absent, so everyone can see who has it.

That falls out of machinery already designed rather than being new, which is
some evidence it is the right framing.

### The Vetula problem, and how to dodge it

Vetula supplies **harmonic context** — key, chord, scale — that other machines
read when deciding notes. Split across computers, Odonus needs Vetula's context
*at note time*. That is not cold data, so it cannot ride on Selene, and Link
carries tempo and phase only, not arbitrary payloads.

**The dodge: distribute the progression, not the chord.** If the harmony is a
progression on a timeline, every machine can derive the current chord *locally*
from Link's phase. The cold thing (the progression) is shared through the usual
channels; the hot thing (which chord is now) becomes a local computation that
needs no network at all — and is therefore exactly as tight as Link's phase lock,
which is already good enough to trigger with.

**This is the same trick as the routing table**: move the hot thing to a local
derivation of a cold thing. It is also the same trick as the pass-list idea
above, which advances on Link phase rather than on a message.

It does not cover everything. A **live-played** Vetula — someone changing chords
by hand, ad hoc — is genuinely hot and needs a real-time channel. purerl-tidal's
live-control bus (ETS-backed, `set-control` → `live "name"`) is the obvious
carrier, since it already exists and already crosses process boundaries. So:

| harmony mode | distribution |
|---|---|
| scheduled progression | derive locally from Link phase — free, no network |
| live-played | live-control bus, and accept the latency |

Worth designing the scheduled case first: it is free, it covers most jamming, and
having it working sets the bar the live case has to justify itself against.

## Sequencing

The ordering matters because the early steps are useful whether or not the split
ever happens.

1. **Build the output-backward view inside Triggerfish**, read-only, from `Rig` ×
   `claims` × monitor. Every input already exists in the browser. This is not
   wasted work if the split happens later — **it is the prototype of the new app,
   built where the data currently is.**
2. **Free-text patch labels** on its rows. Cheap, immediately useful, and the
   fastest way to learn what the labels actually want to say.
3. **Extract Selene** — frontend plus API, table moved off `localStorage`,
   manifests published by Triggerfish, traffic aggregate published back.
4. **Daemon proxying in Selene's API** — liveness first, then polysignal publish
   and `apply-drumkit`, which currently run from a shell script.
5. **Promote labels to references**, once fifteen or twenty exist and their real
   shape is visible. The backward-view doc's own warning applies to itself here:
   design the encoding against a real populated table, not a mock.
6. **Banks and bindings**, then recipes as Amphora artefacts.
7. **Faceplates**, whenever. Pure reference data, zero coupling, and the biggest
   single payoff for having a whole window instead of a modal.

## Hazards

- **Two apps must now agree for the rig to play.** Today you open one thing. That
  is a real regression in a performance, and the mitigation is that Selene's data
  is *cold*: a stale table still works, where a stale emitter would not. Guard it
  by keeping Triggerfish's default table sane enough to be useful with Selene
  absent — which is the same test as the pure-MIDI one above.
- **Single writer.** Selene edits the table; music apps subscribe read-only and
  never write. Two editors with a network between them is the bank-coherence
  problem with extra latency, and `DESIGN-routing-backward.md` already ends with
  exactly this warning.
- **The manifest is a wire format.** It inherits `sourceKey`'s existing hazard —
  rename a key and you silently orphan a user's saved routing.
- **Do not let Selene grow a second opinion about admission.** The daemons
  already own it (es9-daemon has capability and overlap checks with `!` eviction,
  fh2-config has `PortClaim`). `claims` reports; it must not arbitrate. The
  output-range bug is the standing lesson: the client that duplicates a policy
  silently overrides it.
- **Faceplate scans are the fun part and the least valuable.** They are also the
  easiest to spend a weekend on. Sequence them last on purpose.
- **Polyphony is a `Destination` change.** Retrofitting `voices`/allocation after
  recipes are artefacts in Amphora means migrating stored data. Design it in
  before step 6 even if nothing uses it yet.
- **Multi-machine is a claim system, and claim systems fail closed badly.** If
  Selene is unreachable, a machine that cannot confirm its claim must still play
  — degrade to "assume I own what I owned last time", never to silence. The
  cold-data principle again: stale is fine, absent is not.
