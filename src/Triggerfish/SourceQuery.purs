-- | The queries every instrument answers for the shell:
-- |
-- |   * `AskSource` — "hand me your current source as a string" (the TIDAL tab
-- |     aggregate; pull-based, asked when the tab opens / on refresh).
-- |   * `SyncFree start tempo` — "adopt this shared free-run baseline (start
-- |     micros, BPM)", so the modules share one downbeat with no rig (the
-- |     laptop-only analogue of all locking to one Link anchor). Modules with
-- |     no clock (Selene's skeleton) ignore it.
-- |   * `SetMaster on` — the shell's master transport. A module SOUNDS only when
-- |     `master && armed`: its own run button is now a sticky ARM/cue toggle, and
-- |     this query tells it whether the master PLAY is engaged. So arming a
-- |     stopped rack is silent; master PLAY starts every armed module together on
-- |     the shared downbeat; toggling arm mid-play drops a module in/out live.
-- |     Modules with no transport (Selene) ignore it.
-- |   * `SetAudible on` — the SOLO/ATLANTIS authority gate (control-surface
-- |     consolidation). A module emits local Web-MIDI only when `audible`; in
-- |     ATLANTIS the rig is authoritative and the frontend is muted (`audible =
-- |     false`) but its scheduler/animation KEEPS RUNNING (lockstep co-sim). This
-- |     is orthogonal to `master`: master is the transport (playing at all),
-- |     audible is who makes the sound (local vs rig).
-- |   * `FeedVoiceChords` — the LIVE Vetula→Odonus follow bridge. The shell polls
-- |     Vetula ~100ms for each Odonus-bound performance voice's current block
-- |     chord (`{ id, pcs }`, where `id` is the voice's channel reused as an
-- |     Odonus id) and pushes the set here; Odonus's KEY pane selects one (or
-- |     zero) to snap its output to. Modules without a chord quantiser ignore it.
-- |
-- |   * `AskLibrary` — "hand me your saved presets as `{name, text}`", where
-- |     `text` is each entry rendered to its Lepidoptera eDSL (the transferable
-- |     form). The TIDAL page's cross-instrument LIBRARY MANAGER (A5) gathers
-- |     these from all four. An instrument with no named collection returns its
-- |     single live value (Odonus → its live patch).
-- |   * `LoadEntry i` — "make saved entry `i` the active / live one" (the manager's
-- |     LOAD button; the in-instrument switcher does the same at the other altitude).
-- |   * `ImportText txt` — "if this eDSL text is one of MINE, add it to my library
-- |     and answer `true`". The manager broadcasts a paste to all four; the one
-- |     whose parser (or signature) recognises it accepts — auto-routing by form.
-- |
-- | (Vetula, vendored from its own standalone app, defines its own structurally
-- | similar source query — see Vetula.App.SourceQuery — with its own SetMaster
-- | and a mirror of these three library constructors. The `{name, text}` shape is
-- | a STRUCTURAL record, so the two query types need share no nominal type.)
module Triggerfish.SourceQuery (Query(..)) where

import Triggerfish.Transport (Sounding)

data Query a
  = AskSource (String -> a)
  | SyncFree Number Number a
  | FeedChords (Array (Array Int)) a
  | FeedVoiceChords (Array { id :: Int, pcs :: Array Int }) a
  -- The ONE transport query (control-surface MISU refactor — see
  -- docs/DESIGN-transport-misu.md). It replaces the old scatter of SetMaster /
  -- SetAudible / SetArm / SyncToRig / StopRig: the shell derives each machine's
  -- `Sounding` (Silent | Local | Rig) from (mode, armed) and pushes it here. The
  -- instrument stores just this value and edge-detects transitions itself —
  -- entering `Rig` hands off to the backend voice (re-issuing `Rig` re-voices),
  -- leaving `Rig` stops it, `Local` plays local Web-MIDI, `Silent` is stopped.
  -- Instruments with no rig voice (Selene) simply never receive `Rig`.
  | SetSounding Sounding a
  -- Observe: report the machine's EFFECTIVE sounding so the shell can reconcile
  -- its `armed` set (a machine can self-disarm, e.g. Vetula unloading) and render
  -- the switcher dot. `Silent` ⇒ not armed.
  | AskSounding (Sounding -> a)
  | AskLibrary (Array { name :: String, text :: String } -> a)
  | LoadEntry Int a
  | ImportText String (Boolean -> a)
  -- macro-tidal harmonic authority: install the rig's resting harmonic context
  -- (root pc + intervals) as the pitch-quantisation set. Pushed by the shell from
  -- Vetula (the single harmonic authority); Odonus realises through it. Instruments
  -- with no quantiser ignore it.
  | SetContextPitchSet Int (Array Int) a
  -- The shell's CAPTURE hotkey (same key on every pane): "bank your current
  -- playing-state as a preset (mint its glyph) and park your identity on it".
  -- Routed to the active machine. Machines without a capture/glyph notion ignore it.
  | Capture a
  -- The status-board chip's recall menu: "hand me your banked presets as
  -- { slot, alias }" (the shell renders each via glyphFromAlias) and "recall slot i"
  -- (switch to + restore that preset). Machines without a bank answer [] / ignore.
  | AskBank (Array { slot :: Int, alias :: String } -> a)
  | RecallSlot Int a
