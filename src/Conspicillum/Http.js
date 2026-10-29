export const requestImpl = (method) => (url) => (body) => (ok) => (fail) => () => {
  const init = { method };
  if (body !== null) { init.body = body; init.headers = { "Content-Type": "application/json" }; }
  fetch(url, init)
    .then((r) => r.text().then((t) => (r.ok ? ok(t)() : fail(url + ": " + r.status + " " + t)())))
    .catch((e) => fail(url + ": " + String(e && e.message ? e.message : e))());
};

export const exchangeImpl = (method) => (url) => (body) => (ok) => (fail) => () => {
  const init = { method };
  if (body !== null) { init.body = body; init.headers = { "Content-Type": "application/json" }; }
  fetch(url, init)
    .then((r) => r.text().then((t) => ok({ status: r.status, body: t })()))
    .catch((e) => fail(url + ": " + String(e && e.message ? e.message : e))());
};
