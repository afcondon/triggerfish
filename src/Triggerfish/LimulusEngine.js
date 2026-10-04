// Limulus's engine, in the origin's shared storage (Limulus reads the same key).
const KEY = "atlantis/limulus-engine";
export const load = () => { try { return localStorage.getItem(KEY) || "architeuthis"; } catch (_) { return "architeuthis"; } };
export const save = (v) => () => { try { localStorage.setItem(KEY, v); } catch (_) {} };
export const onChange = (cb) => () => {
  window.addEventListener("storage", (e) => { if (e.key === KEY && e.newValue) cb(e.newValue)(); });
};
