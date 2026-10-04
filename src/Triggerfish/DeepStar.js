// DeepStar, the rig doctor, on :3027 of the page's own host.
export const doctorImpl = (done) => () => {
  fetch("http://" + (window.location.hostname || "localhost") + ":3027/doctor", { cache: "no-store" })
    .then((r) => (r.ok ? r.json() : null))
    .then((d) => done(d && Array.isArray(d.checks)
      ? d.checks.map((c) => ({ name: String(c.name), status: String(c.status), detail: String(c.detail || "") }))
      : null)())
    .catch(() => done(null)());
};
