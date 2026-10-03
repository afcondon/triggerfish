// Bosun's supervisor for the Atlantis group, on :3994 of the page's own host.
const base = () => "http://" + (window.location.hostname || "localhost") + ":3994";

export const stateImpl = (done) => () => {
  fetch(base() + "/state", { cache: "no-store" })
    .then((r) => (r.ok ? r.json() : null))
    .then((d) => {
      if (!d || !d.services) return done(null)();
      const sup = d.supervision || {};
      const services = Object.keys(d.services).sort().map((id) => {
        const s = sup[id] || {};
        return { id, state: String(d.services[id]), restarts: s.restarts | 0, gaveUp: !!s.gaveUp };
      });
      done({ desired: String(d.desired || ""), phase: String(d.phase || ""), services })();
    })
    .catch(() => done(null)());
};

export const controlImpl = (verb) => (service) => (done) => () => {
  const q = service ? "?service=" + encodeURIComponent(service) : "";
  fetch(base() + "/control/" + verb + q, { method: "POST" })
    .then((r) => done(r.ok)())
    .catch(() => done(false)());
};
