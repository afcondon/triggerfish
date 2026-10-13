// Triggerfish.Library — ids, the browser's copy, and "shown again".

export const freshId = () =>
  typeof crypto !== "undefined" && crypto.randomUUID
    ? crypto.randomUUID()
    : Date.now().toString(36) + Math.random().toString(36).slice(2);

export const loadCopy = (key) => () => {
  try {
    return window.localStorage.getItem(key);
  } catch (_) {
    return null;
  }
};

export const saveCopy = (key) => (text) => () => {
  try {
    window.localStorage.setItem(key, text);
  } catch (_) {
    // quota or private mode: Amphora still has it
  }
};

export const onShown = (act) => () => {
  if (typeof window === "undefined") return;
  window.addEventListener("focus", () => act());
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") act();
  });
};
