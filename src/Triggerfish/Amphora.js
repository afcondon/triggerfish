// Triggerfish.Amphora — the shared Amphora artefact-store client (browser).
//
// One content-addressed read/write dance, reused by every editor whose library
// is a named collection of eDSL / Tidal text (Odonus scenes, Selene racks,
// Vetula progressions; Balistes has its own name-in-payload variant). The name
// and any extra metadata ride on the LABEL, not the payload, so this works
// whether or not the eDSL embeds a name.
//
// Base URL follows the page host so it works at localhost or andrews-mac-mini;
// Amphora listens on :3024 with permissive CORS.

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

const getJSON = (url) =>
  fetch(url).then((r) => {
    if (!r.ok) throw new Error("GET " + url + " → HTTP " + r.status);
    return r.json();
  });

const sendJSON = (base, method, path, body) =>
  fetch(base + path, {
    method,
    headers: { "Content-Type": "application/json" },
    body: body === undefined ? undefined : JSON.stringify(body),
  }).then(async (r) => {
    if (!r.ok) throw new Error(method + " " + path + " → HTTP " + r.status + ": " + (await r.text()));
    return r.status === 204 ? null : r.json();
  });

// fetchCollectionImpl(collection)(onError)(onSuccess)() :: Effect Unit
// Resolves an Array of { hash, name, payload, tags }. Joins favourites → content
// (payload) and favourites → first label (name + tags) so PureScript stays pure.
export const fetchCollectionImpl = (collection) => (onError) => (onSuccess) => () => {
  const base = amphoraBase();
  const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();

  getJSON(base + "/favorites?collection=" + encodeURIComponent(collection))
    .then((favs) => {
      const hashes = (favs || []).map((f) => f.contentHash).filter(Boolean);
      return Promise.all(
        hashes.map((h) =>
          Promise.all([
            getJSON(base + "/content/" + h).then((c) => (c && c.payload) || null),
            getJSON(base + "/labels?hash=" + h).then((ls) => (ls && ls[0]) || null),
          ]).then(([payload, label]) => {
            if (typeof payload !== "string") return null;
            return {
              hash: h,
              name: (label && label.name) || "",
              payload,
              tags: (label && label.tags) || [],
            };
          })
        )
      );
    })
    .then((items) => onSuccess(items.filter(Boolean))())
    .catch(fail);
};

// publishImpl(spec)(onError)(onSuccess)() :: Effect Unit
// spec = { kind, collection, name, source, payload, tags }.
// content (dedup by hash) → label (guarded on name) → favourite (guarded).
// Resolves the content hash.
export const publishImpl = (spec) => (onError) => (onSuccess) => () => {
  const base = amphoraBase();
  const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();

  (async () => {
    const { hash } = await sendJSON(base, "POST", "/content", {
      kind: spec.kind,
      payload: spec.payload,
    });

    const labels = await getJSON(base + "/labels?hash=" + hash);
    if (!labels.some((l) => l.name === spec.name)) {
      await sendJSON(base, "POST", "/labels", {
        contentHash: hash,
        name: spec.name,
        source: spec.source,
        tags: spec.tags,
      });
    }

    const favs = await getJSON(base + "/favorites?collection=" + encodeURIComponent(spec.collection));
    if (!favs.some((f) => f.contentHash === hash)) {
      await sendJSON(base, "POST", "/favorites", {
        contentHash: hash,
        collection: spec.collection,
      });
    }
    return hash;
  })()
    .then((hash) => onSuccess(hash)())
    .catch(fail);
};

// unpublishImpl(collection)(hash)(onError)(onSuccess)() :: Effect Unit
// DELETE the favourite; content + labels stay addressable.
export const unpublishImpl = (collection) => (hash) => (onError) => (onSuccess) => () => {
  const base = amphoraBase();
  const fail = (e) => onError(e instanceof Error ? e : new Error(String(e)))();
  sendJSON(
    base,
    "DELETE",
    "/favorites?hash=" + encodeURIComponent(hash) + "&collection=" + encodeURIComponent(collection)
  )
    .then(() => onSuccess()())
    .catch(fail);
};
