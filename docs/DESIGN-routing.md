# Routing — one table, sources to destinations

**Status: design. AC, 2026-08-08, after the Odonus envelope work.**

> "i think at this point we should make the routing in the Routing Modal (Alt-1)
> editable and there might be an argument for merging our newly created voices
> modal from Odonus into that more general router, no?"

Yes, and the argument is stronger than tidiness: the VOICES modal built an hour
earlier is a **second routing surface**, which is the same fault the bank fold
spent yesterday removing. Two places to say where a machine's output goes will
drift exactly as two places to name a rhythm did.

## What the router already is

`channelMapPanel` (⌥1) is a per-machine table, and it is closer to right than it
looks — it already has editable destinations, just not everywhere:

| machine | rows | editable |
|---|---|---|
| Odonus | four heads | no — `fixedEntry "ch 1"` … |
| Balistes | `kit` | no — `fixedEntry "ch 10"` |
| Selene | one per declared destination | **yes** — cascading device+bank select |
| Vetula | default + named voices | **yes** — channel input per name |
| Sufflamen, Stellatus | — | tbd |

So "make it editable" is not new machinery. Selene's cascade is the pattern; the
other columns are rows nobody has finished.

## The structural change: a source has SEVERAL destinations

This is what forces a model rather than a patch, and it is the bug that stopped
the envelope test working.

Odonus head I needs to send:

- its **musical note** to `IAC Driver Tidal`, channel 1 — so Ableton plays it
- its **envelope trigger** to the **FH-2**, channel 1 — so polyenv fires

One source, two devices. Odonus opens exactly one WebMIDI output
(`midiPortName = "IAC"`), so everything it emits goes down that one port and
nothing ever reaches the FH-2. The VOICES modal correctly assigns *which
envelope*, and then the note goes to the wrong device.

Every machine has the same latent problem — each hardcodes a single port name —
so this is not an Odonus quirk.

**The model:**

> **source → a SET of destinations, and a destination's shape depends on its
> device.**

```
Source        = { machine :: Which, sub :: Maybe String }   -- head I, voice "pad", kit
Destination   = MidiDest  { port :: String, channel :: Int }
              | Fh2Env    { slots :: Array Int }            -- polyenv 1..8
              | Fhx8Gt    { note :: Int, jack :: Int }      -- note-filtered trigger MCV
              | Es9Cv     { pitchBus :: Int, trigBus :: Maybe Int }
              | Continuo  { channel :: Int }
              | ClaimNone                                    -- declared, unrouted
```

Same idea as `Roles` in fh2-config: a source declares what it needs, placement is
config, and a rig change is data rather than a rewrite. It is also what makes
AC's "in future perhaps the other way around — FH-2 sending the pitch and ES-9
the envelopes" a config edit rather than a refactor.

## What each column becomes

- **Odonus** — four head rows. Each carries a MIDI destination (port + channel)
  and, when an FH-2 is present, an `Fh2Env` with the envelope pips the VOICES
  modal currently owns. Later an `Es9Cv` for the calibrated pitch route that
  `reef_voice` already runs.
- **Balistes** — `kit` gains the same treatment; ch 10 stops being a constant.

  And it should be **one row per drum lane, not one row for the kit**. The
  assignment that actually matters on this rig is *which drum fires which
  FHX-8GT jack*, and today that lives as a hardcoded four-row table in
  `fh2-config/scripts/apply-drum-breakout.mjs`:

  ```
  BD note 36 → jack 1     HH note 42 → jack 3
  SD note 38 → jack 2     CP note 39 → jack 4
  ```

  — a constant, in another repo, applied out-of-band by a script, which per
  issue #6 never re-applies when the daemon bounces. That is the same fact the
  router exists to hold, in the worst possible place for it. Moving a drum to a
  different gate is currently a code edit; it should be a row.

  Note this is a **different destination kind** from Odonus's: the drum's note
  number is a *selector* the FH-2's note-filtered trigger MCVs match on, not a
  pitch. So `Fhx8Gt { note :: Int, jack :: Int }` beside `MidiDest`, and the
  router owns both halves of the pair — which is the point, since they are only
  correct relative to each other.
- **Selene** — unchanged. Its cascade is already the shape everything else is
  growing towards.
- **Vetula** — named voices already edit a channel; they gain a port so
  `continuo` versus IAC stops being decided in code.

## The VOICES modal folds in and is deleted

It ships today because it unblocked the envelope test, but it is explicitly
temporary. `VoiceCfg { envs :: Array Int }` in `Odonus.Grid.Types` becomes the
`Fh2Env` destination of the head's row in the router, and both the modal and its
nav button go. Nothing should be left offering a second answer to "where does
this go".

Note the state stays where it is in one respect: routing is **rig-facing
placement**, so it belongs in Triggerfish, not in `Reef.Odonus.Head` — that
record co-simulates with the BEAM and carries musical facts only. Moving routing
into the shell's router keeps that split and improves it, since the shell already
owns `routing` and `audition`.

## Persistence, and who is authoritative

The shell already persists some of this (`routing :: Map String Int`, `audition`)
and Selene's targets live in its own doc. Consolidating means one store for the
whole table.

Keep the discipline established yesterday: **persist preferences, never claims
about running state.** A route is a preference — the user's decision about where a
machine's output goes, which nothing on the backend can contradict — so it
persists. What is currently *playing* through that route does not.

## Sequencing

1. ~~**Multi-destination for Odonus heads.**~~ **DONE 2026-08-08** (`639b30d`).
   A head's musical note keeps going to its MIDI port; its envelope trigger goes
   to the FH-2's own port, matched on `"FH-2"`. No fallback to the note port when
   absent — envelope notes on the IAC bus would trigger Ableton synths, worse
   than silence. A missing port is shown: the VOICES nav button goes red with
   `NO FH-2` when envelopes are assigned but the port isn't there, and the modal
   states both destinations.

   Two things this turned up that the note didn't predict:

   - `extraEnvChannels` elided the head's own channel on the grounds that its
     note already fired that envelope. Only true while notes and envelopes shared
     a port. Now `envChannels`, and it elides nothing.
   - **Selene was overriding the FH-2's own output-range policy**, so envelopes
     came up bipolar and idled at -5V. `FH2.PolyBank.familyDefaultRange` already
     owns that policy and is consulted only when `outputRange` is absent;
     Selene sent it unconditionally, hardcoded to `Bipolar5V`. Fixed by Selene
     stating no opinion (`range :: Maybe OutputRange`, default `Nothing`) rather
     than by copying the policy across. A range can be pinned per block with an
     optional header token — `env fh2_0 unipolar8v`.

   Still unheard: whether velocity actually changes an envelope. That is the
   next thing to do at the rig, not more code.
2. **Make the Odonus and Balistes rows editable** — port and channel, reusing
   Selene's cascade rather than inventing a control.
3. **Fold the envelope pips into the Odonus rows**; delete the VOICES modal and
   its nav button.
4. **One store for the table**, replacing the scattered `routing` / `audition` /
   Selene-doc split.
5. **Harmonise the BEAM.** `reef_voice:start_json(Json, 12, 0.25)` hardcodes base
   channel 12 while Triggerfish uses `1 + h`, so Solo and Atlantis emit the same
   head on different channels. Whatever the router says should be what both
   runtimes use — AC has confirmed the 12 was debugging residue and the Ableton
   session can be reconfigured.

## Watch for

- **A missing MIDI port must be visible.** `findOutput` returning `Nothing`
  silently disables a whole destination. The router is the natural place to show
  a port as present or missing, the same way the DISCONNECTED stamp shows the rig
  link.
- **Channel collisions become possible** once channels are editable — two sources
  on one channel is legal and sometimes wanted (layering), but wants showing
  rather than discovering by ear.
- **polyenv occupies channels 1–8 on the FH-2 by construction** (envelope N
  listens on channel N, not configurable), so the router must not offer an FH-2
  MIDI destination on 1–8 that means something different from "fire envelope N".
