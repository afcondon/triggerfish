#!/usr/bin/env bash
# Boot the headless SuperDirt daemon for Stellatus.
#
# Port-from-env (SUPERDIRT_PORT, default 57135 — NOT 57120, which es9-daemon
# owns). Drain-on-signal: trap TERM/INT and kill the whole process group so the
# scsynth child sclang spawns doesn't orphan (the one real wrinkle the
# bosun-daemon skill calls out). A Bosun `supervise` wrapper wants exactly this.
set -euo pipefail

SCLANG="${SCLANG:-/Applications/SuperCollider.app/Contents/MacOS/sclang}"
export SUPERDIRT_PORT="${SUPERDIRT_PORT:-57135}"
# scsynth output device (see superdirt-daemon.scd). Under Bosun the compose env
# supplies this (SUPERDIRT_DEVICE: "BlackHole 2ch") now that Bosun shell-quotes env
# values (project 227 note #398). This `:-` fallback covers manual/standalone
# launches. Override by exporting SUPERDIRT_DEVICE; set to empty to hear SuperDirt
# on the system default output directly.
export SUPERDIRT_DEVICE="${SUPERDIRT_DEVICE:-BlackHole 2ch}"
DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ ! -x "$SCLANG" ]]; then
  echo "sclang not found at $SCLANG — set SCLANG=/path/to/sclang" >&2
  exit 1
fi

# Singleton guard. Two sclang running superdirt-daemon.scd fight over SuperDirt's
# OSC port (57120): the loser boots into a deaf, CPU-burning, audio-device-holding
# zombie. This actually happened (2026-07-30 — a midnight cron/manual boot landed
# atop a days-old one). If one is already alive, this boot is a no-op success
# (idempotent — safe for a repeated manual/cron invocation); set FORCE=1 to
# override deliberately. A Bosun `supervise` wrapper should be the SOLE launcher,
# so under supervision this path shouldn't trigger.
EXISTING="$(pgrep -f 'sclang .*superdirt-daemon\.scd' || true)"
if [[ -n "$EXISTING" && "${FORCE:-0}" != "1" ]]; then
  echo "SuperDirt already running (sclang pid(s): ${EXISTING//$'\n'/ }) — not launching a second." >&2
  echo "  kill it first, or set FORCE=1 to override." >&2
  exit 0
fi

echo "booting SuperDirt on UDP $SUPERDIRT_PORT (sclang $SCLANG) …"

# Run sclang in its own process group so we can tear the whole tree down.
set -m
"$SCLANG" "$DIR/superdirt-daemon.scd" &
SC_PID=$!

cleanup() {
  echo "draining SuperDirt (pgid kill) …"
  kill -TERM -- "-$SC_PID" 2>/dev/null || kill -TERM "$SC_PID" 2>/dev/null || true
}
trap cleanup TERM INT EXIT

wait "$SC_PID"
