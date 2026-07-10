# MidiFighter Twister → Odonus map

The Twister drives the Odonus surface. Switch **hardware banks** with the side
buttons: **Bank 1 = the cells**, **Bank 2 = the four voices**. The 16 endless
encoders sit in a 4×4 grid; positions are numbered **row-major, top-left = 0**:

```
  0  1  2  3
  4  5  6  7
  8  9 10 11
 12 13 14 15
```

Encoders run in **absolute** mode (rotate = CC 0–127 on MIDI ch 1; push = ch 2).
Banks are distinguished by CC range: **Bank 1 = CC 0–15, Bank 2 = CC 16–31.**
The status bar shows the active Bank-1 grid, e.g. `TWISTER ▸ NOTE`.

---

# Bank 1 — the cells

## PUSH — select the active grid

| Push encoder | Selects grid |
|---|---|
| 0 | **NOTE** |
| 1 | **LEN** |
| 2 | **RATCHET** |
| 3 | **VEL** |
| 4 | **GATE** |
| 5 | **SKIP** |
| 6 | **GLIDE** |
| 7 | **MACRO** |
| 8–15 | *(unused — reserved)* |

## ROTATE — edit the active grid

- **Value grids** — `NOTE · LEN · RATCHET · VEL`: each encoder sets **that cell's**
  value (encoder *n* ↔ cell *n*), scaled across the field's range.
- **Boolean grids** — `GATE · SKIP · GLIDE`: each encoder sets **that cell's** flag.
  Turn **right (past halfway) = on**, left = off.
- **MACRO grid** — the 16 encoders drive the Notes-pane globals, not the cells:

```
 octave    degree     marbles-X   marbles-Y
 gen-on*   depth      rate        roll
 step-div  gate%      swing       humanise
   —         —          —           —
```

  \* **gen-on** toggles the **NOTES** generator source (right = on), not a global
  freeze. **roll** re-rolls all cell notes once per turn (debounced). **depth** /
  **rate** are the NOTES generator's amount / rate.

---

# Bank 2 — the voices

Each **row is a voice** (head 0–3, top to bottom); the four columns are the most
crucial per-voice controls:

```
 voice 0:  pattern  euclid-k  euclid-n  transpose
 voice 1:  pattern  euclid-k  euclid-n  transpose
 voice 2:  pattern  euclid-k  euclid-n  transpose
 voice 3:  pattern  euclid-k  euclid-n  transpose
```

- **pattern** — scans the access pattern (Rows / Serpentine / Columns / Spiral /
  Diagonal); the encoder position picks the pattern (it cycles to it).
- **euclid-k** — the *k* (pulses) in E(k, n), 0–16.
- **euclid-n** — the *n* (steps) in E(k, n), 1–16.
- **transpose** — per-voice interval, −24..+24 semitones.

Pushes on Bank 2 are unmapped for now (candidate: mute the voice).

---

## Notes

- **Immediate feel** — Twister edits apply to local state the instant they arrive
  (not quantised to the model-step grid), and broadcast to the rig when on ATLANTIS.
- **Pickup jump** — encoders are absolute and don't yet know a grid's current
  values, so the first turn after switching grids snaps to the encoder's physical
  position. Driving the Twister's LED rings back from state (Twister-as-output) is
  the planned fix.
- Banks 2–4 are earmarked for the per-voice Euclid grid, the macro/Balistes bank,
  and Selene polysignals respectively.
