// One BroadcastChannel per page, shared by every tab of this origin.
export const _open = (name) => () =>
  typeof BroadcastChannel === "undefined" ? null : new BroadcastChannel(name);

export const _post = (ch) => (text) => () => {
  if (ch) ch.postMessage(text);
};

export const _onMessage = (ch) => (cb) => () => {
  if (ch) ch.addEventListener("message", (e) => {
    if (typeof e.data === "string") cb(e.data)();
  });
};

// pagehide, not unload: it also fires when the page goes into the back/forward
// cache, and it is the one browsers still deliver reliably.
export const _onPageHide = (act) => () => {
  window.addEventListener("pagehide", () => act());
};
