// Fire-and-forget POST of one Stellatus event to the OSC bridge
// (audio/stellatus-bridge.mjs), which relays it to SuperDirt as /dirt/play.
// Deliberately does not await — the ring animation must not block on the network.
export const fireImpl = (url) => (json) => () => {
  try {
    fetch(url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: json,
      keepalive: true,
    }).catch(() => {});
  } catch (_e) {
    /* bridge down — stay silent, keep animating */
  }
};
