export const loadImpl = (url) => (done) => () => {
  fetch(url)
    .then((r) => (r.ok ? r.json() : { sets: [] }))
    .then((d) => (d.sets || []).map((s) => ({ name: s.name, samples: (s.samples || []).length })))
    .catch(() => [])
    .then((sets) => done(sets)());
};
