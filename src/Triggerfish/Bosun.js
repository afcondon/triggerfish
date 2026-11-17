// Bosun's supervisor for the Atlantis group, on :3994 of the page's own host.
const base = () => "http://" + (window.location.hostname || "localhost") + ":3994";

// The document as text, or null when Bosun is out of reach. Reading it is
// PureScript's job (Bosun.Protocol.State), not this file's.
export const stateTextImpl = (done) => () => {
  fetch(base() + "/state", { cache: "no-store" })
    .then((r) => (r.ok ? r.text() : null))
    .then((text) => done(text)())
    .catch(() => done(null)());
};

export const controlImpl = (verb) => (service) => (done) => () => {
  const q = service ? "?service=" + encodeURIComponent(service) : "";
  fetch(base() + "/control/" + verb + q, { method: "POST" })
    .then((r) => done(r.ok)())
    .catch(() => done(false)());
};
