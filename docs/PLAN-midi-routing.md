# PLAN — MIDI output routing: identity → destination

*Design + build plan, 2026-07-08. Pairs with the three-drum-tabs section of
`BALISTES-SELENE-RETHINK.md`. Cross-repo: reef (wire), purerl-tidal (`.erl`
voices), Triggerfish (config page + emitters), link-spike (the wire convention).*

## The goal

Put **both runtimes on the same MIDI channels.** Today the browser and the BEAM
emit on *deliberately different* channels — an A/B safety margin from when the
byte-identical sync was still being proven (`reef_balistes_voice.erl` literally
says *"browser on 10, rig on 11 for A/B"*). Lockstep co-simulation is now proven,
so the offset has done its job. Collapsing onto one channel per voice lets a
**standard Ableton project template** receive either runtime interchangeably.

## The model — identity ≠ destination

Channel is **not** an instrument-page concern. Every voice/source has an
**identity**; a central **routing table on the Tidal config page** binds
identity → destination; the instrument pages stop carrying channel numbers.

This is already latent in the codebase: Odonus follows a Vetula voice by **id**,
not channel; reef tags notes by **voiceOrd**, not channel. This plan makes that
the whole model.

- A **destination is heterogeneous**:
  `Destination = MidiChannel Int | Es9Bus … | Fh2Out … | Osc …`
- A source can **fan out to several destinations at once** (e.g. drums → Ch 10
  *and* an ES-9 trigger bus). This dissolves the "duplicate the drum lane on two
  pages" problem: one source, many destinations — not one lane copied twice.
- **Browser owns the table** (Option B). Values flow to the BEAM via the existing
  config/Perf push. The browser is the single source of truth; the `.erl` voices
  stop hardcoding channels.

## The default map = the Ableton template

Because the common case is "mostly default," the default channel map and the
Ableton template's track routing are the **same artifact**. Pinning one pins the
other. Channels shown as the **hardware** sees them (1–16):

| Ch | Source (default) | Ableton track |
|----|------------------|---------------|
| 1–4 | Odonus I–IV (fixed) | 4 gen-melody tracks |
| 5 | Vetula (unnamed default) | harmony |
| 6–8 | Vetula *named* voices (specialised) | spare harmony |
| 10 | Drums (Balistes, all three tabs) | drum rack |
| 9, 11–16 | free / future | — |

Everyday footprint: **6 channels** (1–4, 5, 10). Everything else is off-MIDI:
Selene → modular (FH-2/ES-9), Stellatus + Sufflamen → OSC only.

### Per-instrument: current → target

| Instrument | Browser now | BEAM now | Target (both) |
|---|---|---|---|
| Odonus I–IV | 1,2,3,4 (`headIdx`) | 13–16 (base 12 + head) | **1–4** |
| Balistes | 10 (hardcoded) | 11 (`reef_balistes_voice`) | **10** |
| Vetula →MIDI | per-voice UI, default 1–4 | `[8,9,16]` hardcoded | **5** default; **named→6–8** via table |
| Selene | 1-indexed config | — | **modular only** (FH-2/ES-9); no MIDI ch |
| Stellatus / Sufflamen | OSC | OSC | **OSC only** |

### Vetula loses its channel field

Vetula output voices are **Odo or MIDI**. A MIDI voice gets an **optional name**;
unnamed → the single default Vetula channel (5); named → looked up in the config
table (6–8…). The MIDI-channel picker leaves the Vetula page entirely. This
matches "Vetula is about *progressions*, not *where they go*."

## Two boundary facts to get right

1. **Index convention.** link-spike's `/midi/note/at` takes `channel(i, 1-16)`
   and does `channel_1idx.saturating_sub(1) & 0x0F` (`main.rs:37,170`) —
   **1-indexed**. The browser's `Binnacle.Midi.js` masks `channel & 0x0f` on a
   **0-indexed** value. The routing table should hold **one canonical convention**
   (recommend hardware 1–16, matching link-spike and Ableton), converting to
   0-indexed at exactly **one** browser-side boundary (the WebMIDI emit).

2. **Channel ∉ the conformance digest — the key de-risk.** reef's byte-identical
   proof formats *which notes at which ticks* (`cursor:seqPos:transp:…`), **not
   channel**. Channel is a routing decoration applied at emit. So the channel map
   can travel as **out-of-band config** (set on the voice at spawn / config push)
   **without touching the goldens**. Most BEAM voices already take a `Channel`
   start param (`reef_balistes_voice`, `reef_voice`, `reef_vetula_brush`); only
   `reef_vetula_voice`'s hardcoded `rig_channels() = [8,9,16]` needs replacing.

## Build order (Half A)

- **A1 — Pin the default map** as a shared data module (also *is* the Ableton
  template spec). Establish the canonical 1–16 convention + the single conversion
  boundary. No behaviour change yet.
- **A2 — Browser routing table** on the Tidal config page: `identity →
  [Destination]`, seeded with the defaults. Emitters (Odonus, Vetula, Balistes)
  source channel from the table by identity; Vetula sheds its channel field;
  Selene voices bind to FH-2/ES-9. **Browser-only, no conformance risk —
  immediately testable by ear.**
- **A3 — BEAM un-hardcode.** Replace `reef_vetula_voice`'s `[8,9,16]` with a
  channel list carried in the config push (out-of-band → goldens untouched).
  Verify conformance still 25/25. Both runtimes now read the same map → **same
  channels**.

(Half B — the Balistes three-tab restructure — is in `BALISTES-SELENE-RETHINK.md`.)

## Open question — browser → modular fan-out

Fan-out to modular (drums → Ch 10 **and** an ES-9 bus) is trivial on the rig:
the BEAM already OSCs es9-daemon / link-spike directly. But the **browser cannot
raw-OSC modular** — it would need a WS/HTTP→OSC bridge. So `Destination`s of the
`Es9Bus`/`Fh2Out` kind may be **rig-only** initially; the browser honours the
`MidiChannel` rows and ignores (or greys) the modular rows until a bridge exists.
The type is designed for the full fan-out; the browser's *reach* lags. Decide the
bridge separately (candidate: a small OSC-forwarder the browser already needs for
Stellatus/Sufflamen SuperDirt).
