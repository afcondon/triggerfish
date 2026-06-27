export const svgYFromEvent = (ev) => () => {
  const el = ev.currentTarget;
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

export const svgXFromEvent = (ev) => () => {
  const el = ev.currentTarget;
  if (!el || !el.viewBox || !el.viewBox.baseVal) return 0.0;
  const rect = el.getBoundingClientRect();
  if (rect.width === 0) return 0.0;
  const vb = el.viewBox.baseVal;
  return vb.x + ((ev.clientX - rect.left) / rect.width) * vb.width;
};
