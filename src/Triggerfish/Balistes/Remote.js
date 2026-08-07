// Amphora read client (browser). Base URL is derived from the page host so it
// works whether Triggerfish is opened at localhost or andrews-mac-mini; Amphora
// listens on :3024 and sends permissive CORS.

const amphoraBase = () => {
  if (typeof window !== "undefined" && window.AMPHORA_BASE) return window.AMPHORA_BASE;
  const host =
    typeof window !== "undefined" && window.location && window.location.hostname
      ? window.location.hostname
      : "localhost";
  const proto =
    typeof window !== "undefined" && window.location && window.location.protocol === "https:"
      ? "https:"
      : "http:";
  return proto + "//" + host + ":3024";
};

// fetchCollectionImpl(collection)(onError)(onSuccess)() :: Effect Unit
// Resolves an Array of content payloads (the Lepidoptera strings) that are
// favourited into `collection`. Does the N+1 fetch here so PureScript stays pure.
export const fetchCollectionImpl = (collection) => (onError) => (onSuccess) => () => {
  const base = amphoraBase();
  const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();
  const done = (payloads) => onSuccess(payloads)();

  // Fail fast when Amphora (:3024) is down, instead of hanging on the browser's
  // ~30s default — otherwise this blocks Balistes' Initialize. AbortError → Left.
  const getJSON = (url) => {
    const ctrl = new AbortController();
    const timer = setTimeout(() => ctrl.abort(), 2500);
    return fetch(url, { signal: ctrl.signal })
      .then((r) => {
        if (!r.ok) throw new Error("GET " + url + " → HTTP " + r.status);
        return r.json();
      })
      .finally(() => clearTimeout(timer));
  };

  getJSON(base + "/favorites?collection=" + encodeURIComponent(collection))
    .then((favs) => {
      const hashes = (favs || []).map((f) => f.contentHash).filter(Boolean);
      return Promise.all(
        hashes.map((h) =>
          getJSON(base + "/content/" + h).then((c) => (c && c.payload) || null)
        )
      );
    })
    .then((payloads) => done(payloads.filter((p) => typeof p === "string")))
    .catch(fail);
};

// "house 122" → { genre: "house", bpm: 122 }. Tempo is the trailing integer.
const parseName = (name) => {
  const m = name.match(/^(.*?)\s+(\d+)\s*$/);
  return m ? { genre: m[1].trim(), bpm: Number(m[2]) } : { genre: name, bpm: null };
};

// publishPatternImpl(payload)(name)(source)(onError)(onSuccess)() :: Effect Unit
// content (dedup by hash) → label (genre + bpm tags, guarded) → favourite into
// balistes-grid (guarded). Resolves the content hash. Mirrors the seed script.
export const publishPatternImpl =
  (payload) => (name) => (source) => (onError) => (onSuccess) => () => {
    const base = amphoraBase();
    const COLLECTION = "balistes-grid";
    const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();

    const j = (method, path, body) =>
      fetch(base + path, {
        method,
        headers: { "Content-Type": "application/json" },
        body: body === undefined ? undefined : JSON.stringify(body),
      }).then(async (r) => {
        if (!r.ok) throw new Error(method + " " + path + " → HTTP " + r.status + ": " + (await r.text()));
        return r.json();
      });

    (async () => {
      const { hash } = await j("POST", "/content", { kind: "balistes-pattern", payload });

      const { genre, bpm } = parseName(name);
      const tags = [genre];
      if (bpm != null) tags.push("bpm:" + bpm);
      const labels = await j("GET", "/labels?hash=" + hash);
      if (!labels.some((l) => l.name === name)) {
        await j("POST", "/labels", { contentHash: hash, name, source, tags });
      }

      const favs = await j("GET", "/favorites?collection=" + COLLECTION);
      if (!favs.some((f) => f.contentHash === hash)) {
        await j("POST", "/favorites", { contentHash: hash, collection: COLLECTION });
      }
      return hash;
    })()
      .then((hash) => onSuccess(hash)())
      .catch(fail);
  };

// publishSnapshotImpl(payload)(name)(brain)(source)(onError)(onSuccess)()
//
// The BANK's write-back, as opposed to publishPatternImpl above which is the
// RYTM library's. Same content → label → favourite shape, but a different kind
// and a different collection, because the payloads are not interchangeable:
//
//   balistes-pattern / balistes-grid  — Lepidoptera FixedPattern text. fetchLibrary
//                                       parses EVERY payload in that collection with
//                                       parsePattern and silently drops failures, so
//                                       a Grids point posted there would vanish on
//                                       next load rather than error.
//   balistes-snapshot / balistes-bank — printTri text for ANY brain (M/G/T tagged),
//                                       read back with parseTri.
//
// The two paths merge when the library folds into the bank (slice 5 of
// docs/DESIGN-balistes-bank-coherence.md); until then balistes-grid stays the
// rhythm path, untouched.
//
// `brain` is the DISPLAY letter (G/R/T) and rides along as a tag so the store can
// be filtered by machine without parsing every payload. NB it is not the leading
// character of the payload — those are the frozen M/G/T wire tags, where stored
// "G" means RYTM. See Triggerfish.Balistes.TriSnapshot.
export const publishSnapshotImpl =
  (payload) => (name) => (brain) => (source) => (onError) => (onSuccess) => () => {
    const base = amphoraBase();
    const COLLECTION = "balistes-bank";
    const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();

    const j = (method, path, body) =>
      fetch(base + path, {
        method,
        headers: { "Content-Type": "application/json" },
        body: body === undefined ? undefined : JSON.stringify(body),
      }).then(async (r) => {
        if (!r.ok) throw new Error(method + " " + path + " → HTTP " + r.status + ": " + (await r.text()));
        return r.json();
      });

    (async () => {
      const { hash } = await j("POST", "/content", { kind: "balistes-snapshot", payload });

      // Content-addressed, so saving the same state twice collapses to one row.
      // The label is guarded the same way: re-saving under the same name is a
      // no-op, but the same content CAN carry several names over time.
      const labels = await j("GET", "/labels?hash=" + hash);
      if (!labels.some((l) => l.name === name)) {
        await j("POST", "/labels", { contentHash: hash, name, source, tags: ["brain:" + brain] });
      }

      const favs = await j("GET", "/favorites?collection=" + COLLECTION);
      if (!favs.some((f) => f.contentHash === hash)) {
        await j("POST", "/favorites", { contentHash: hash, collection: COLLECTION });
      }
      return hash;
    })()
      .then((hash) => onSuccess(hash)())
      .catch(fail);
  };
