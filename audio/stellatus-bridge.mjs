// Stellatus → SuperDirt OSC bridge.
//
// The browser can't send UDP OSC, so Stellatus POSTs each fired slice here as
// JSON and this daemon relays it to SuperDirt as a `/dirt/play` OSC message.
// Node built-ins only (http + dgram) — no npm install, no dependency (keeps the
// rig's runtime surface minimal). Dependency-free, hand-rolled OSC encoder.
//
// This is the interface daemon AC signed off on: a small, supervisable Atlantis
// component. It is Bosun-ready — port-from-env, a /health readiness endpoint,
// and it drains cleanly on SIGTERM. See audio/README.md for the Bosun gaps.
//
//   BRIDGE_PORT     HTTP port the browser POSTs to      (default 57130)
//   SUPERDIRT_PORT  UDP port SuperDirt listens on       (default 57135)
//   SUPERDIRT_HOST  where SuperDirt runs                (default 127.0.0.1)
//
// Run:  node audio/stellatus-bridge.mjs
// Test: node audio/stellatus-bridge.mjs --selftest   (dumps the OSC bytes for one hit)

import http from 'node:http';
import dgram from 'node:dgram';

const BRIDGE_PORT = Number(process.env.BRIDGE_PORT ?? 57130);
const SUPERDIRT_PORT = Number(process.env.SUPERDIRT_PORT ?? 57135);
const SUPERDIRT_HOST = process.env.SUPERDIRT_HOST ?? '127.0.0.1';

// ── OSC encoding (spec 1.0) ────────────────────────────────────────────────
// An OSC message = padded address string + padded type-tag string + args.
function oscString(s) {
  const b = Buffer.from(String(s), 'ascii');
  const pad = 4 - (b.length % 4); // always ≥1 (the null terminator)
  return Buffer.concat([b, Buffer.alloc(pad)]);
}
function oscFloat(f) { const b = Buffer.alloc(4); b.writeFloatBE(f, 0); return b; }
function oscInt(i) { const b = Buffer.alloc(4); b.writeInt32BE(i | 0, 0); return b; }

// SuperDirt's /dirt/play takes a flat [key1, val1, key2, val2, …] arg list.
function encodeDirtPlay(params) {
  let tags = ',';
  const parts = [];
  for (const [k, v] of Object.entries(params)) {
    tags += 's';
    parts.push(oscString(k));
    if (typeof v === 'number') { tags += 'f'; parts.push(oscFloat(v)); }
    else { tags += 's'; parts.push(oscString(v)); }
  }
  return Buffer.concat([oscString('/dirt/play'), oscString(tags), ...parts]);
}

// ── Self-test: print the encoded bytes for one representative hit ───────────
if (process.argv.includes('--selftest')) {
  const msg = encodeDirtPlay({ s: '808bd', n: 3, speed: 1, begin: 0, end: 1, gain: 0.9, orbit: 0, cps: 0.5 });
  console.log(`OSC /dirt/play — ${msg.length} bytes (must be multiple of 4: ${msg.length % 4 === 0})`);
  console.log(msg.toString('hex').replace(/(..)/g, '$1 ').trim());
  process.exit(0);
}

// ── UDP sender ──────────────────────────────────────────────────────────────
const udp = dgram.createSocket('udp4');
let sent = 0;

function play(params) {
  const msg = encodeDirtPlay(params);
  udp.send(msg, SUPERDIRT_PORT, SUPERDIRT_HOST);
  sent++;
}

// ── HTTP intake ───────────────────────────────────────────────────────────
const cors = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'POST, GET, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type',
};

const server = http.createServer((req, res) => {
  if (req.method === 'OPTIONS') { res.writeHead(204, cors); return res.end(); }

  if (req.method === 'GET' && req.url === '/health') {
    res.writeHead(200, { ...cors, 'Content-Type': 'application/json' });
    return res.end(JSON.stringify({ ok: true, sent, superdirt: `${SUPERDIRT_HOST}:${SUPERDIRT_PORT}` }));
  }

  if (req.method === 'POST' && req.url === '/play') {
    let body = '';
    req.on('data', (c) => { body += c; if (body.length > 1e5) req.destroy(); });
    req.on('end', () => {
      try {
        const params = JSON.parse(body);
        play(params);
        res.writeHead(204, cors);
        res.end();
      } catch (e) {
        res.writeHead(400, cors);
        res.end(String(e));
      }
    });
    return;
  }

  res.writeHead(404, cors);
  res.end();
});

server.listen(BRIDGE_PORT, () => {
  console.log(`stellatus-bridge: POST http://127.0.0.1:${BRIDGE_PORT}/play  →  OSC ${SUPERDIRT_HOST}:${SUPERDIRT_PORT}/dirt/play`);
  console.log('(waiting for Stellatus. GET /health for status.)');
});

// Drain on signal (Bosun-ready).
for (const sig of ['SIGTERM', 'SIGINT']) {
  process.on(sig, () => { console.log(`\n${sig} — closing bridge (${sent} hits relayed).`); server.close(); udp.close(); process.exit(0); });
}
