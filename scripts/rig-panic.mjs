// rig-panic.mjs — stop everything the polysignal daemons are generating.
//
// The problem it solves: Selene polysignals are AUTONOMOUS. Once applied they
// run inside es9-daemon's audio callback (ES-9) or the FH-2's own clock/LFO
// engine — nothing in the transport is driving them, so stopping Balistes,
// stopping the transport, or deleting the channels in Triggerfish and
// re-publishing does not stop them. Deleting channels publishes an empty set,
// and an empty set sends no release, so the old claims keep running. They are
// also persisted (~/.es9/claims.json, ~/.fh2/claims.json) and restored across
// daemon restarts, which is why a bounce doesn't clear them either.
//
// This is the kill switch. Two devices, two different mechanisms:
//
//   ES-9  — `release-claim <kind> <name>` per live claim. es9-daemon's
//           implementation calls .deactivate() on every destination the owner
//           held, so releasing IS silencing. Enumerate via `list-claims`.
//
//   FH-2  — `release-claim` is BOOKKEEPING ONLY (Daemon.purs:434 rewrites the
//           claim table and persists it; it never encodes or sends SysEx), so
//           releasing does NOT silence the hardware. The kill is the `--silent`
//           baseline config instead: every MCV disabled, clocks at Type=None,
//           LFOs idle, nothing on any jack across main + both expanders.
//
//           Then `reload`, which is NOT optional: --silent writes the hardware
//           from a separate process, leaving the daemon's cached Config still
//           holding the noisy state. The next apply-anything builds on that
//           stale cache and pushes the noise straight back. reload re-reads the
//           device so cache and hardware agree.
//
//           Finally re-apply the drum breakout, because --silent wiped the
//           FHX-8GT trigger MCVs along with everything else.
//
// Usage:
//   node scripts/rig-panic.mjs            # kill everything, restore drums
//   node scripts/rig-panic.mjs --bare     # kill everything, leave drums off
//   node scripts/rig-panic.mjs --es9-only # ES-9 generators only, don't touch FH-2
//
// Eventual home is DeepStar (the rig doctor) alongside the pre-flight checks;
// it lives here for now because Triggerfish is what leaks the claims.

import net from "node:net";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";

const FH2_REPO = "/Users/afc/work/afc-work/music/expert-sleepers/fh2-config";
const ES9_SOCK = path.join(os.homedir(), ".es9", "control.sock");
const FH2_SOCK = path.join(os.homedir(), ".fh2", "control.sock");

const args = process.argv.slice(2);
const BARE = args.includes("--bare");
const ES9_ONLY = args.includes("--es9-only");

// One newline-framed verb, one newline-terminated reply. Note this WAITS for
// the reply — `nc -U` does not reliably do so, and a dropped `reload` reply is
// exactly the failure that silently restores the noise (see above).
function send(sock, cmd, timeoutMs = 5000) {
  return new Promise((resolve, reject) => {
    const s = net.createConnection(sock, () => s.write(cmd + "\n"));
    const timer = setTimeout(() => {
      s.destroy();
      reject(new Error(`timeout after ${timeoutMs}ms`));
    }, timeoutMs);
    s.setEncoding("utf8");
    let buf = "";
    s.on("data", (chunk) => {
      buf += chunk;
      const i = buf.indexOf("\n");
      if (i !== -1) {
        clearTimeout(timer);
        s.end();
        resolve(buf.slice(0, i));
      }
    });
    s.on("error", (e) => { clearTimeout(timer); reject(e); });
  });
}

let failed = false;
const fail = (msg) => { failed = true; console.log(`  ✗ ${msg}`); };

// --- ES-9 -------------------------------------------------------------------
// Enumerate then release. Releasing deactivates the generator slots, so this
// both drops the claim and stops the sound.

console.log("rig-panic: ES-9 generators");
try {
  const reply = await send(ES9_SOCK, "list-claims");
  if (!reply.startsWith("OK")) {
    fail(`list-claims: ${reply}`);
  } else {
    const { claims = [] } = JSON.parse(reply.slice(3));
    if (claims.length === 0) {
      console.log("  · no live claims");
    }
    for (const c of claims) {
      const { kind, name } = c.owner;
      const r = await send(ES9_SOCK, `release-claim ${kind} ${name}`);
      if (r.startsWith("OK")) console.log(`  ✓ ${r}`);
      else fail(`release ${kind} ${name}: ${r}`);
    }
  }
} catch (e) {
  fail(`ES-9 daemon unreachable (${e.message}) — is es9-daemon up?`);
}

// --- FH-2 -------------------------------------------------------------------

if (ES9_ONLY) {
  console.log("rig-panic: FH-2 skipped (--es9-only)");
} else {
  console.log("rig-panic: FH-2 clocks/LFOs");

  // 1. Silence the hardware. Separate process, opens its own MIDI port.
  try {
    execFileSync("node", ["run-daemon.mjs", "--silent"], {
      cwd: FH2_REPO, stdio: "pipe",
    });
    console.log("  ✓ silent baseline sent (all MCVs off, clocks None, LFOs idle)");
  } catch (e) {
    fail(`silent baseline: ${e.message}`);
  }

  // 2. Re-sync the daemon's cache. Skipping this is what puts the noise back.
  try {
    const r = await send(FH2_SOCK, "reload");
    if (r.startsWith("OK")) console.log(`  ✓ daemon cache re-synced — ${r}`);
    else fail(`reload: ${r} (cache is STALE — do not apply anything until fixed)`);
  } catch (e) {
    fail(`reload: ${e.message} (cache is STALE — do not apply anything until fixed)`);
  }

  // 3. Drop the FH-2 claim rows too, so the claim table matches the silence.
  //    Bookkeeping only — the silencing already happened in step 1.
  try {
    const reply = await send(FH2_SOCK, "list-claims");
    if (reply.startsWith("OK")) {
      const { claims = [] } = JSON.parse(reply.slice(3));
      for (const c of claims) {
        const { kind, name } = c.owner;
        await send(FH2_SOCK, `release-claim ${kind} ${name}`);
        console.log(`  ✓ claim dropped: ${kind} ${name}`);
      }
    }
  } catch {
    // fh2-daemon has no list-claims verb in some builds; the silence already
    // landed, so a stale claim row is cosmetic. Not worth failing the panic.
    console.log("  · claim table not enumerable (cosmetic — hardware is silent)");
  }

  // 4. Drums back, unless --bare. --silent wiped the FHX-8GT trigger MCVs.
  if (BARE) {
    console.log("  · drums left off (--bare)");
  } else {
    try {
      execFileSync("node", ["scripts/apply-drum-breakout.mjs"], {
        cwd: FH2_REPO, stdio: "pipe",
      });
      console.log("  ✓ drum breakout re-applied (BD/SD/HH/CP → FHX-8GT 1-4)");
    } catch (e) {
      fail(`drum breakout: ${e.message}`);
    }
  }
}

console.log(failed ? "rig-panic: FINISHED WITH ERRORS (see above)" : "rig-panic: quiet");
process.exit(failed ? 1 : 0);
