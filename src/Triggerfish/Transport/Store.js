"use strict";

export const _save = (key) => (json) => () => {
  try {
    window.localStorage.setItem(key, json);
  } catch (e) {
    /* private mode / quota / no storage — best-effort */
  }
};

export const _load = (key) => () => {
  try {
    const s = window.localStorage.getItem(key);
    if (s == null) return null;
    return JSON.parse(s);
  } catch (e) {
    return null;
  }
};

export const _stringify = (x) => JSON.stringify(x);

export const _onChange = (key) => (callback) => () => {
  window.addEventListener("storage", (e) => {
    if (e.key === key) callback();
  });
};
