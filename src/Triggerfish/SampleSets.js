// `no-cache`, as in Conspicillum/Corpora.js: the list changes as sets are made.
export const loadImpl = (url) => (done) => () => {
  fetch(url, { cache: "no-cache" })
    .then((r) => (r.ok ? r.json() : { sets: [] }))
    .then((d) => (d.sets || []).map((s) => ({ name: s.name, samples: (s.samples || []).length })))
    .catch(() => [])
    .then((sets) => done(sets)());
};
