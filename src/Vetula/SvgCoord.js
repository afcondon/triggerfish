// The SVG an event's handler sits on, or (for a container that hosts an SVG
// drawn outside Halogen, such as Explore's field) the SVG inside it.
const svgOf = (ev) => {
  const el = ev.currentTarget;
  if (el && el.viewBox && el.viewBox.baseVal) return el;
  return el && el.querySelector ? el.querySelector(":scope > svg") : null;
};

export const svgYFromEvent = (ev) => () => {
  const el = svgOf(ev);
  if (!el || !el.viewBox || !el.viewBox.baseVal) return 0.0;
  const rect = el.getBoundingClientRect();
  if (rect.height === 0) return 0.0;
  const vb = el.viewBox.baseVal;
  return vb.y + ((ev.clientY - rect.top) / rect.height) * vb.height;
};

export const isFormField = (ev) => () => {
  const t = ev.target;
  if (!t) return false;
  const tag = (t.tagName || "").toUpperCase();
  return tag === "INPUT" || tag === "TEXTAREA" || t.isContentEditable === true;
};

// display:none yields a 0×0 bounding box (works for SVG, which has no reliable
// offsetParent). Absent surface counts as hidden too.
export const surfaceHidden = () => {
  const el = document.querySelector(".vetula-surface");
  if (!el) return true;
  const r = el.getBoundingClientRect();
  return r.width === 0 && r.height === 0;
};

export const svgXFromEvent = (ev) => () => {
  const el = svgOf(ev);
  if (!el || !el.viewBox || !el.viewBox.baseVal) return 0.0;
  const rect = el.getBoundingClientRect();
  if (rect.width === 0) return 0.0;
  const vb = el.viewBox.baseVal;
  return vb.x + ((ev.clientX - rect.left) / rect.width) * vb.width;
};
