# Control Surface — State-Machinery Loose Threads

Status as of the milestone commit (2026-07-03). The SOLO⟷ATLANTIS control
surface (Phases 1, 2, 2.5 + refinements) works and reads much more clearly, but
the state that drives it **grew by exploratory programming**, not by design. This
doc names the loose threads so the **next session can do a dedicated audit**:
model the transport/authority as a proper state machine and make the illegal
states unrepresentable.

> **RESOLVED 2026-07-03** — the transport was re-modelled per
> `docs/DESIGN-transport-misu.md`. Shell state is now `mode :: Mode` +
> `armed :: Set Which`; each machine holds one `Sounding` (Silent | Local |
> Rig) derived by `soundingOf`. Killed: the vestigial `master` gate,
> `rigOn`, `playing`, the asymmetric arm records, `reconcileRig`, and the
> dead `Hush*` actions. The six illegal states in the table below are now
> unrepresentable. Audition policy settled (AC): audible when STOPPED,
> silent-ok when MUTED → gates on `sounding /= Rig`. **Still open:** the
> channel model (audit item 7), and the fully-MISU child-output path for
> self-disarm (the SyncTick observe poll remains). Builds + bundles clean;
> **rig-test pending.**

## The state we actually have

Transport/authority is currently a scatter of booleans across the shell and four
instruments:

- **Shell (`Triggerfish.Main.RState`)**
  - `mode :: Mode` (Solo | Atlantis) — the one clean ADT.
  - `playing :: Boolean` — repurposed to mean "anything armed" (button label only).
  - `armed :: { odo, bal, sel, vet :: Boolean }` — the per-tab mirror.
  - `rigOn :: { odo, bal, vet :: Boolean }` — which rig voices are running (ATLANTIS).
  - `brushSent, brushPrev :: String` — the Vetula signature-diff heuristic.
- **Each instrument**
  - `master :: Boolean` — now **vestigial** (broadcast `true` once on Init, never
    flipped). The old master-gate.
  - `running` (Odonus/Balistes/Selene) / `armed` (Vetula) — the real arm flag.
  - `audible :: Boolean` — local-MIDI gate; `audible == false` ⟺ ATLANTIS ⟺ "rig
    is authoritative", so it's **overloaded** (mutes local AND opens the rig-send
    gate, read as `onRig = not audible`).
  - Balistes also has `pushed :: Boolean` — another stateful latch.

That's `mode × playing × armed{4} × rigOn{3} × master{4} × audible{4} × pushed`
… a large space, most of which is illegal.

## Illegal / incoherent states currently possible

- `master = false` anywhere (never intended now — the gate is vestigial).
- `rigOn.x = true` while `mode = Solo` (rig voice believed running with no
  authority). Only avoided by discipline, not by types.
- `audible = true` on a rig-backed instrument while `mode = Atlantis` (would
  double the sound). Avoided only by ordering `broadcastAudible` correctly.
- Shell `armed.x` disagreeing with the instrument's real `running` (two sources of
  truth; reconciled every 1.5 s by SyncTick, not by construction).
- `rigOn` shape `{odo,bal,vet}` vs `armed` shape `{odo,bal,sel,vet}` — asymmetric
  by hand (Selene has no rig voice), easy to desync.
- `playing` (= anyArmed) can drift from the actual `armed` record if any writer
  forgets to recompute it.

## Known odd behaviours (observed, tolerated for now)

- **rigOn drift on a mid-ATLANTIS rig restart.** The shell mirror thinks voices
  run when they died; a re-arm won't restart them until a SOLO→ATLANTIS
  round-trip (the "implicit resync"). Self-heals, but only via that round-trip.
- **Selene is a hybrid.** No rig voice, so it's frontend-authoritative in *both*
  modes (never muted). Correct given today's backend, but it means "ATLANTIS =
  rig authoritative" has an exception. Its POLYTRIG drums also overlap Balistes
  (both ch 10) — see task #75 (fold onto Balistes).
- **Channel duality.** Balistes plays ch 10 (frontend) vs ch 11 (rig); Selene ch
  10. The UI is 0-indexed in places (voice `ch`, preview `ch`) — the promised
  "1-indexed everywhere" normalisation is not done.
- **Dead code.** `PushVetula` / `PushBrush` (Vetula) and `HushRig` / `HushBalistes`
  (Odonus/Balistes) actions are now dormant (no button, no dispatch); the
  `vetula-perf` path is retired but not deleted.
- **SetArm reuses ToggleRun/ToggleArm** guarded by `when (b /= current)` — a
  toggle pretending to be a setter. Works, but it's a smell.
- **Optimistic + reconciled arm** — ArmTab writes the mirror optimistically then
  SyncTick reconciles from `AskArmed`. Two writers, eventual consistency.

## The audit (next session)

1. **Model the transport as one type.** Likely a per-instrument `Transport` ADT
   and a top-level authority that makes `mode`, arm, and rig-running one coherent
   value — e.g. a machine can be `Silent | LocalPlaying | RigPlaying`, and the
   mode + arm determine which, with no independent `master`/`audible`/`rigOn`
   booleans to contradict each other.
2. **Kill the vestigial `master` gate** (or fold it into the arm concept).
3. **One source of truth for arm.** Either the shell owns it (instruments are
   dumb views) or the instruments own it (shell always queries) — not both.
4. **Make `onRig`/`audible` a *derived* function of `(mode, hasRigVoice)`**, not a
   stored, separately-broadcast flag. Selene's exception then falls out of
   `hasRigVoice = false` rather than being special-cased.
5. **Unify the arm/rigOn record shapes**, or index by a `Which`-keyed map so the
   Selene asymmetry can't cause a desync.
6. **Delete the dead paths** (`vetula-perf`, dormant Push*/Hush* actions) —
   Phase 4.
7. **Channel model** — 1-indexed everywhere at the UI boundary; a single place
   that maps instrument → (local ch, rig ch).

Cross-refs: `docs/PLAN-control-surface-solo-atlantis.md` (the design),
`reef/docs/PLAN-lockstep-cosimulation.md` (the authority model), Marginalia #240
(Triggerfish), the "Make Illegal States Unrepresentable" discipline.
