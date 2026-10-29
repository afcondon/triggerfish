export const fetchText = (url) => (ok) => (fail) => () => {
  fetch(url)
    .then((r) => (r.ok ? r.text() : Promise.reject(new Error(url + ": " + r.status))))
    .then((t) => ok(t)(), (e) => fail(String(e && e.message ? e.message : e))());
};
