"use strict";

export const _save = (key) => (s) => () => {
  try {
    window.localStorage.setItem(key, s);
  } catch (e) {
    /* private mode / quota / no storage — best-effort */
  }
};

export const _load = (key) => () => {
  try {
    const s = window.localStorage.getItem(key);
    return s == null ? null : s;
  } catch (e) {
    return null;
  }
};
