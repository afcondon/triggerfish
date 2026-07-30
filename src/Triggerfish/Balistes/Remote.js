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
