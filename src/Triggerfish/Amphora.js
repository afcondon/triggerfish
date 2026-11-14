// Triggerfish.Amphora: where the store is. Everything else about talking to
// Amphora is PureScript over its described operations (Amphora.Api).
//
// The base URL follows the page host so it works at localhost or
// andrews-mac-mini; Amphora listens on :3024 with permissive CORS.
//
// The web build (trigger-fish.app, `window.TRIGGERFISH_WEB`) has no Amphora:
// null, so that every request fails at once without touching the network,
// the library is this browser's copy and nothing asks after the visitor's
// machine (Triggerfish.Web, 2026-10-27).
export const storeBase = () => {
  if (typeof window === "undefined") return "http://localhost:3024";
  if (window.TRIGGERFISH_WEB === true) return null;
  if (window.AMPHORA_BASE) return window.AMPHORA_BASE;
  const host = window.location && window.location.hostname ? window.location.hostname : "localhost";
  const proto = window.location && window.location.protocol === "https:" ? "https:" : "http:";
  return proto + "//" + host + ":3024";
};
