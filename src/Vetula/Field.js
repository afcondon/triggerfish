const SVG_NS = "http://www.w3.org/2000/svg";

// The field's own <svg>, made once per container. Halogen renders the
// container empty and never touches its children. True when it was made now.
export const ensureRoot_ = (sel) => (css) => () => {
  const host = document.querySelector(sel);
  if (!host) return false;
  if (host.querySelector(":scope > svg")) return false;
  const style = document.createElement("style");
  style.textContent = css;
  host.appendChild(style);
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("class", "vetula-surface");
  svg.setAttribute("viewBox", "-440 -300 880 600");
  svg.setAttribute("style", "max-width: none; touch-action: none; width: 100%; height: 100%; display: block;");
  host.appendChild(svg);
  return true;
};

export const setViewBox_ = (sel) => (vb) => () => {
  const svg = document.querySelector(sel + " > svg");
  if (svg) svg.setAttribute("viewBox", vb);
};

export const setAttr_ = (el) => (name) => (value) => () => el.setAttribute(name, value);

// The key HATS gave an element of a keyed fold.
export const hatsKey_ = (el) => () => el.getAttribute("data-hats-key") || "";

export const hostPresent_ = (sel) => () => document.querySelector(sel) !== null;
export const rootPresent_ = (sel) => () => document.querySelector(sel + " > svg") !== null;
