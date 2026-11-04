// `no-cache`: the corpora change whenever a tape or set is made, and the
// server sends them with no Cache-Control, so a browser would otherwise
// keep an old list by its own heuristic.
export const fetchText = (url) => (ok) => (fail) => () => {
  fetch(url, { cache: "no-cache" })
    .then((r) => (r.ok ? r.text() : Promise.reject(new Error(url + ": " + r.status))))
    .then((t) => ok(t)(), (e) => fail(String(e && e.message ? e.message : e))());
};
