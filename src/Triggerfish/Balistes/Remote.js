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

  const getJSON = (url) =>
    fetch(url).then((r) => {
      if (!r.ok) throw new Error("GET " + url + " → HTTP " + r.status);
      return r.json();
    });

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
