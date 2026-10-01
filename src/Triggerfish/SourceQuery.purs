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

import Data.Maybe (Maybe)
import Triggerfish.Routing.Model (Table)
import Triggerfish.Transport (Sounding)

data Query a
  = AskSource (String -> a)
  -- URL routing (`Triggerfish.Route`): "adopt the stage named by these path
  -- segments". The shell carries the segments opaquely — each machine owns its
  -- own stage vocabulary and parses them itself. A machine with no stage axis
  -- (Balistes, Selene today) no-ops; unrecognised segments are IGNORED rather
  -- than guessed at, so a stale link switches machine and leaves the stage alone.
  | SetStagePath (Array String) a
  -- macro-tidal: push this machine's arrangement lane down, so a machine can
  -- show and edit its own lane in place instead of only on the rack-wide TIDAL
  -- page. `text` is the lane source, `readout` its live current-token label.
  -- The machine raises its edits back up; the shell stays the owner (the lane is
  -- rack state, persisted with the others), so this is a mirror, not a handoff.
  | PutLane String String a
  -- The write mirror of `AskSource`: "replace your active source with this
  -- string, verbatim". Lets a second surface (the routing modal's Selene
  -- column) edit the same document the machine's own tab shows — the doc is the
  -- authority, so both surfaces stay in sync through it. Machines whose source
  -- the shell never rewrites just no-op.
  | PutSource String a
  | SyncFree Number Number a
  -- Report the machine's live clock for the shell's system-BPM readout: the
  -- tempo it's currently running at and whether it's Link-LOCKED (rig anchor
  -- present, so the shell's free-run baseline is overridden). Clock-less machines
  -- answer their last-known / default.
  | AskClock ({ tempo :: Number, locked :: Boolean } -> a)
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
  -- (root pc + intervals) as the pitch-quantisation set, and the chords as a
  -- Tidal note pattern (`odonus $ harmony "..."`; Nothing for none). Pushed by
  -- the shell from Vetula (the single harmonic authority); Odonus realises
  -- through it. Instruments with no quantiser ignore it.
  | SetContextPitchSet Int (Array Int) (Maybe String) a
  -- The shell's CAPTURE hotkey (same key on every pane): "bank your current
  -- playing-state as a preset (mint its glyph) and park your identity on it".
  -- Routed to the active machine. Machines without a capture/glyph notion ignore it.
  | Capture a
  -- The status-board chip's recall menu: "hand me your banked presets" (the shell
  -- renders each via glyphFromAlias; `name` "" = anonymous), "recall slot i", and
  -- the curate verbs — toggle a star, delete a preset. Machines without a bank
  -- answer [] / ignore.
  -- The unified ROUTING TABLE (docs/DESIGN-routing.md). The shell owns it — it
  -- is rack-wide, spans machines, and persists — and pushes it down here on load
  -- and after every edit in the ⌥1 router. A machine stores it and resolves its
  -- own legs at emit time.
  --
  -- Pushed rather than each machine reading the store, so an edit takes effect on
  -- the next note instead of the next reload, and so there is exactly one writer.
  | SetRouting Table a
  | AskBank (Array { slot :: Int, alias :: String, name :: String, starred :: Boolean } -> a)
  | RecallSlot Int a
  | StarSlot Int a
  | DeleteSlot Int a
