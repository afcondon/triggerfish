#!/usr/bin/env bash
# Boot SuperDirt with ALL 217 Dirt-Samples banks — the sound-hunting path.
#
# Costs ~935 MB of scsynth footprint (394 MB on disk, ~2x as float32 buffers).
# That is a real bite out of 16 GB, so prefer superdirt-core.sh for performance
# and keep this for browsing. Once you know which banks you want, add them to
# `coreBanks` in superdirt-daemon.scd and go back to core.
#
# SUPERDIRT_SAMPLES=lazy is a third option not given its own script: all 217
# banks registered header-only and read on first use, so every name resolves and
# it stays small — but the read is async, so the FIRST hit of each sample is
# lost. Good for browsing, wrong for a take.
set -euo pipefail
exec env SUPERDIRT_SAMPLES=full "$(cd "$(dirname "$0")" && pwd)/boot-superdirt.sh" "$@"
