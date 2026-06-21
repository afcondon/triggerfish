"use strict";

// Normalised [0,1]×[0,1] position of a mouse event within the element with
// the given id. x grows right, y grows down, clamped to the rails. We look the
// element up by id (not ev.currentTarget) because the handler runs async, after
// dispatch, when currentTarget is null — but clientX/clientY persist.
// EffectFn3: uncurried, returns the value directly (NO `() =>` thunk —
// that was the bug: runEffectFn3 got the thunk back as the result, so .x/.y
// were undefined and the puck's CSS became left:NaN% → top-left).
export const padNormImpl = (id, cx, cy) => {
  const el = document.getElementById(id);
  if (!el) return { x: 0.5, y: 0.5 };
  const r = el.getBoundingClientRect();
  const c = (v) => Math.max(0, Math.min(1, v));
  return {
    x: r.width > 0 ? c((cx - r.left) / r.width) : 0.5,
    y: r.height > 0 ? c((cy - r.top) / r.height) : 0.5,
  };
};
