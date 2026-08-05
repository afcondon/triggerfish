#!/usr/bin/env bash
# Boot SuperDirt with the CORE sample banks — the default, and the one to run
# under Bosun and in performance.
#
# ~9 MB of samples instead of 394 MB, everything read before the first beat, so
# no first-hit read latency. The bank list lives in superdirt-daemon.scd
# (`coreBanks`); add to it there when a kit name you want stays silent.
#
# The other path is superdirt-full.sh (all 217 banks, ~935 MB) for when you're
# digging through the library for sounds.
set -euo pipefail
exec env SUPERDIRT_SAMPLES=core "$(cd "$(dirname "$0")" && pwd)/boot-superdirt.sh" "$@"
