#!/usr/bin/env bash
# Boot the headless SuperDirt daemon for Stellatus.
#
# Port-from-env (SUPERDIRT_PORT, default 57135 — NOT 57120, which es9-daemon
# owns). Drain-on-signal: trap TERM/INT and tear the whole process group down,
# WAITING for it to actually die, so the scsynth child sclang spawns can't
# orphan (the one real wrinkle the bosun-daemon skill calls out). A Bosun
# `supervise` wrapper wants exactly this.
set -euo pipefail

SCLANG="${SCLANG:-/Applications/SuperCollider.app/Contents/MacOS/sclang}"
export SUPERDIRT_PORT="${SUPERDIRT_PORT:-57135}"
# scsynth output device (see superdirt-daemon.scd). Under Bosun the compose env
# supplies this (SUPERDIRT_DEVICE: "BlackHole 2ch") now that Bosun shell-quotes env
# values (project 227 note #398). This `:-` fallback covers manual/standalone
# launches. Override by exporting SUPERDIRT_DEVICE; set to empty to hear SuperDirt
# on the system default output directly.
export SUPERDIRT_DEVICE="${SUPERDIRT_DEVICE:-BlackHole 2ch}"
# Which sample banks to load: core (default) | full | lazy. See the header of
# superdirt-daemon.scd for what each costs. `core` exists because loading all
# 217 Dirt-Samples banks was ~935 MB — the rig's largest single memory holder.
# Prefer the superdirt-core.sh / superdirt-full.sh wrappers to setting this by
# hand; they are the two documented ways in.
export SUPERDIRT_SAMPLES="${SUPERDIRT_SAMPLES:-core}"
# Tells the user's ~/Library/Application Support/SuperCollider/startup.scd to
# keep its hands off: that file boots its OWN SuperDirt and reads ALL 217
# Dirt-Samples banks (444 MB) on every sclang launch, which for a long time was
# where most of this rig's scsynth footprint actually came from — the daemon's
# own curated load was landing on top of it. The guard leaves interactive IDE
# sessions untouched.
export SUPERDIRT_HEADLESS=1
DIR="$(cd "$(dirname "$0")" && pwd)"

# How long to give a TERM'd tree before escalating to KILL.
GRACE="${SUPERDIRT_GRACE:-10}"

if [[ ! -x "$SCLANG" ]]; then
  echo "sclang not found at $SCLANG — set SCLANG=/path/to/sclang" >&2
  exit 1
fi

# --- teardown ---------------------------------------------------------------
# reap: TERM a pid (or a process group, passed as "-PGID") and then WAIT for it
# to actually go, escalating to KILL if it won't.
#
# The old cleanup() fired one `kill -TERM` and let the script exit immediately.
# That is how scsynth orphaned: if sclang hung or crashed *during its own
# teardown* — exactly what the three SIGSEGVs on 2026-08-05 (12:39, 12:40,
# 13:18) looked like, the 13:18 report showing this wrapper as an already-
# "Exited process" while sclang was still dying — the wrapper was gone while
# the tree was still up, and scsynth (~1 GB as configured back then) was left
# holding the audio device forever. Bosun then launched a fresh pair on top of
# the corpse. Reaping properly is the entire reason this wrapper exists.
reap() {
  local target="$1" label="$2" waited=0
  kill -TERM "$target" 2>/dev/null || true
  while kill -0 "$target" 2>/dev/null; do
    if (( waited >= GRACE )); then
      echo "  $label still alive after ${GRACE}s — escalating to KILL" >&2
      kill -KILL "$target" 2>/dev/null || true
      sleep 1
      break
    fi
    sleep 1
    waited=$((waited + 1))
  done
}

SC_PID=""       # the sclang we launched (also its process-group id, via set -m)
SCSYNTH_PID=""  # its scsynth child, recorded while healthy so we can reap it
                # directly even after sclang dies and it reparents to launchd
ADOPTED_PID=""  # a healthy incumbent we attached to rather than double-booting
CLEANED=0

cleanup() {
  if [[ "$CLEANED" == "1" ]]; then      # TERM-then-EXIT would run this twice
    return 0
  fi
  CLEANED=1
  echo "draining SuperDirt …"
  if [[ -n "$SC_PID" ]]; then
    reap "-$SC_PID" "sclang process group"   # negative pid = the whole group
  fi
  if [[ -n "$ADOPTED_PID" ]]; then
    reap "$ADOPTED_PID" "adopted sclang"
  fi
  # Belt and braces: scsynth is sclang's child, so the group TERM above should
  # have taken it. If it somehow survived (already reparented, or wedged in the
  # audio driver) take it out by the pid we recorded — never by a name match,
  # which would also hit a SuperCollider the user has open in the IDE.
  if [[ -n "$SCSYNTH_PID" ]] && kill -0 "$SCSYNTH_PID" 2>/dev/null; then
    echo "  scsynth $SCSYNTH_PID survived the group teardown — reaping directly" >&2
    reap "$SCSYNTH_PID" "scsynth"
  fi
}

# Who, if anyone, is already on our UDP port? (Used to tell a working incumbent
# from a deaf leftover.)
port_holders() {
  lsof -nP -iUDP:"$SUPERDIRT_PORT" -t 2>/dev/null || true
}

# Does this pid hold ANY UDP socket? A SuperDirt that is serving *some* port is
# somebody's working daemon — just not ours — and must not be reaped. Only one
# holding nothing at all is the deaf zombie we're here to clear.
holds_any_udp() {
  [[ -n "$(lsof -nP -iUDP -a -p "$1" -t 2>/dev/null || true)" ]]
}

# --- incumbent handling -----------------------------------------------------
# Two sclang running superdirt-daemon.scd fight over the SuperDirt OSC port: the
# loser boots into a deaf, audio-device-holding zombie. So we never start a
# second one. But the old guard's response — `exit 0` — was wrong under Bosun:
# a wrapper that exits reads to `supervise` as "service down", so Bosun restarts
# it, the guard bails again, and you get a restart loop that never once reaps
# the leftover it is bailing out for. Instead:
#
#   incumbent is SERVING the port -> attach to it and stay resident, so Bosun
#                                    sees the service as up and leaves it alone
#   incumbent is NOT serving      -> it is a leftover/deaf zombie; reap it and
#                                    boot a fresh one in its place
#   FORCE=1                       -> always reap and boot fresh
EXISTING="$(pgrep -f 'sclang .*superdirt-daemon\.scd' || true)"
if [[ -n "$EXISTING" && "${FORCE:-0}" != "1" ]]; then
  HOLDERS="$(port_holders)"
  HEALTHY=""
  STALE=""
  for pid in $EXISTING; do
    if [[ " $HOLDERS " == *" $pid "* ]]; then
      HEALTHY="$pid"                    # serving OUR port — the incumbent
    elif holds_any_udp "$pid"; then
      # Serving a different port: a deliberate second daemon (another rig, a
      # test on another port). Not ours to touch — the old guard bailed out on
      # it, and reaping it would be strictly worse than that.
      echo "note: sclang $pid is serving another UDP port — leaving it alone." >&2
    else
      STALE="$STALE $pid"               # holding nothing: the deaf zombie
    fi
  done

  if [[ -n "$HEALTHY" ]]; then
    echo "SuperDirt already serving UDP $SUPERDIRT_PORT (sclang $HEALTHY) — attaching, not double-booting." >&2
    ADOPTED_PID="$HEALTHY"
    trap cleanup TERM INT EXIT
    # Stay resident for as long as the incumbent lives: Bosun's process probe
    # reads THIS script, so exiting here would be a false "down". Not `wait` —
    # the incumbent is not our child.
    while kill -0 "$ADOPTED_PID" 2>/dev/null; do
      sleep 5
    done
    echo "adopted SuperDirt (sclang $ADOPTED_PID) exited — so are we." >&2
    exit 1   # non-zero: the service really did go down; let the supervisor act
  fi

  if [[ -n "$STALE" ]]; then
    echo "found sclang ($STALE ) holding no UDP port — deaf leftover, reaping." >&2
    for pid in $STALE; do
      reap "$pid" "stale sclang $pid"
    done
  fi
fi

echo "booting SuperDirt on UDP $SUPERDIRT_PORT, samples=$SUPERDIRT_SAMPLES (sclang $SCLANG) …"

# Run sclang in its own process group so we can tear the whole tree down.
set -m
"$SCLANG" "$DIR/superdirt-daemon.scd" &
SC_PID=$!
set +m

# Arm the trap immediately — before the discovery loop below — so a TERM
# arriving during boot still drains the tree instead of orphaning it.
trap cleanup TERM INT EXIT

# Record the scsynth child while everything is healthy, so cleanup can reap it
# by pid even if sclang later dies and it reparents to launchd. scsynth appears
# a second or two into the boot; give it a bounded window and don't care if it
# never shows (a failed boot has nothing to reap).
for _ in $(seq 1 30); do
  kill -0 "$SC_PID" 2>/dev/null || break
  SCSYNTH_PID="$(pgrep -P "$SC_PID" -x scsynth 2>/dev/null | head -1 || true)"
  if [[ -n "$SCSYNTH_PID" ]]; then
    echo "  scsynth child is pid $SCSYNTH_PID"
    break
  fi
  sleep 1
done

# Don't let `set -e` skip the trap's logging when sclang exits non-zero (a
# SIGSEGV lands here as 139) — report it, then fall through to cleanup.
set +e
wait "$SC_PID"
RC=$?
set -e
if (( RC != 0 )); then
  echo "sclang exited with status $RC" >&2
fi
exit "$RC"
