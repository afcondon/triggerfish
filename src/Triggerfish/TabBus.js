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
