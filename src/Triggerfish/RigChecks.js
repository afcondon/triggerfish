// The rig's checks, from the page's own origin (see RigChecks.purs).
export const checksImpl = (done) => () => {
  fetch("/api/rig/checks", { cache: "no-store" })
    .then((r) => (r.ok ? r.json() : null))
    .then((d) => done(d && Array.isArray(d.checks)
      ? d.checks.map((c) => ({ name: String(c.name), status: String(c.status), detail: String(c.detail || "") }))
      : null)())
    .catch(() => done(null)());
};
