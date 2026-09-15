"use strict";

export const _stringify = (x) => JSON.stringify(x);

export const _parse = (s) => {
  try {
    const v = JSON.parse(s);
    // A clip without events is not a clip; better a null than a record whose
    // `events` is undefined and blows up two layers down.
    return v && Array.isArray(v.events) ? v : null;
  } catch (e) {
    return null;
  }
};
