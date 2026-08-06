// Hash-fragment plumbing for Triggerfish.Route.
//
// `history.replaceState`, NOT `location.hash = …`: the shell rewrites the hash
// every time you change machine or stage, and assigning to `location.hash`
// pushes a history entry each time. Within a minute of playing you'd have a back
// button holding fifty steps of your own navigation. replaceState keeps the URL
// current (copyable, reloadable) while leaving history alone.

export const readHashImpl = () => {
  if (typeof window === "undefined" || !window.location) return "";
  // strip the leading '#'
  return (window.location.hash || "").replace(/^#/, "");
};

export const writeHashImpl = (hash) => () => {
  if (typeof window === "undefined" || !window.history) return;
  const url = window.location.pathname + window.location.search + (hash ? "#" + hash : "");
  window.history.replaceState(null, "", url);
};

// Fires only for hash changes the app did NOT make — a pasted URL, a bookmark, a
// manual edit in the address bar. `writeHashImpl` uses replaceState, which does
// not emit hashchange, so the app can never hear its own writes and there is no
// write→event→write loop to guard against.
export const onHashChangeImpl = (cb) => () => {
  if (typeof window === "undefined") return;
  window.addEventListener("hashchange", () => {
    cb((window.location.hash || "").replace(/^#/, ""))();
  });
};
