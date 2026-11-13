const read = (d) =>
  (d.layers || []).map((l) => ({ name: String(l.name), channel: l.channel | 0, levelDb: Number(l.levelDb) || 0, pan: Number(l.pan) || 0 }));

// `no-cache`: what Continuo hosts changes when an instrument is loaded.
export const layersImpl = (url) => (done) => () => {
  fetch(url, { cache: "no-cache" })
    .then((r) => (r.ok ? r.json() : { layers: [] }))
    .then(read)
    .catch(() => [])
    .then((layers) => done(layers)());
};

// Set one instrument's level or pan. Answers with the layers as they now
// are, or none when it did not take (the caller keeps what it had).
export const mixImpl = (url) => (body) => (done) => () => {
  fetch(url, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(body) })
    .then((r) => (r.ok ? r.json() : { layers: [] }))
    .then(read)
    .catch(() => [])
    .then((layers) => done(layers)());
};
