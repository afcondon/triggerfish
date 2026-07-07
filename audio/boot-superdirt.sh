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
