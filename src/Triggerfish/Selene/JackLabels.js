"use strict";

const KEY = "triggerfish.selene.labels.shown";

export const loadShown = () => {
  try {
    return window.localStorage.getItem(KEY) === "1";
  } catch (e) {
    return false;
  }
};

export const saveShown = (on) => () => {
  try {
    window.localStorage.setItem(KEY, on ? "1" : "0");
  } catch (e) {
    /* private mode / no storage: the default (off) is fine */
  }
};
