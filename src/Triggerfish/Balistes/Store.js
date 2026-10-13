"use strict";

export const _load = (key) => () => {
  try {
    const s = window.localStorage.getItem(key);
    if (s == null) return null;
    return JSON.parse(s);
  } catch (e) {
    return null;
  }
};

