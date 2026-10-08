// The Limulus panel asks to close (Escape inside it): a message from the
// panel's own frame, on this page's origin.
export const limulusAskedClose = (e) =>
  e.origin === window.location.origin && !!e.data && e.data.limulus === "close";

// Give the panel's frame the keyboard (its window's focus also has Limulus
// take the latest buffer), or take it back.
export const focusFrame = (el) => () => { if (el && el.contentWindow) el.contentWindow.focus(); };
export const focusSelf = () => window.focus();

// Where the Limulus panel stands. A page marks the region it gives Limulus
// with `data-limulus-dock` (one element or several, taken together); the
// panel covers that region's columns, from under the bar (or the region's
// top, if lower) to the window's foot (or its bottom, if higher). A dock
// marked "always" keeps the panel open while it is on the page. A marker
// "open" places nothing: it asks for the ordinary drawer to open when it
// appears (and to close again when it goes, if it was what opened it), so a
// page can bring Limulus out without taking it over (Vetula's score). No
// dock: the panel stands on the right, as the bar's CSS says.
// The place goes into CSS variables on the root, so Halogen's own render of
// the panel's style never fights it.
export const watchDocks = (onAlways) => (onWant) => () => {
  const root = document.documentElement;
  let always = null, queued = false, want = null;
  const vars = ["--lim-left", "--lim-right", "--lim-top", "--lim-width", "--lim-height", "--lim-shadow", "--lim-edge"];
  // A dock marked data-limulus-frame="flush" is part of the page: no window
  // chrome on the panel, and the panel's Limulus is told so.
  let flush = null;
  const tellFrame = () => {
    const f = document.querySelector('iframe[title="Limulus"]');
    if (f && f.contentWindow) f.contentWindow.postMessage({ limulusFrame: flush ? "flush" : "window" }, location.origin);
  };
  const place = () => {
    queued = false;
    const marks = [...document.querySelectorAll("[data-limulus-dock]")];
    const nowWant = marks.some((d) => d.dataset.limulusDock === "open");
    if (nowWant !== want) { const first = want === null; want = nowWant; if (!(first && !nowWant)) onWant(nowWant)(); }
    const docks = marks.filter((d) => d.dataset.limulusDock !== "open");
    const nowAlways = docks.some((d) => d.dataset.limulusDock === "always");
    if (nowAlways !== always) { always = nowAlways; onAlways(nowAlways)(); }
    const nowFlush = docks.some((d) => d.dataset.limulusFrame === "flush");
    if (nowFlush !== flush) { flush = nowFlush; tellFrame(); }
    const f = document.querySelector('iframe[title="Limulus"]');
    if (f && !f.dataset.told) { f.dataset.told = "1"; f.addEventListener("load", tellFrame); }
    const rects = docks.map((d) => d.getBoundingClientRect()).filter((r) => r.width > 0 && r.height > 0);
    if (rects.length === 0) { vars.forEach((v) => root.style.removeProperty(v)); return; }
    const bar = parseFloat(getComputedStyle(root).getPropertyValue("--tf-bar")) || 44;
    const left = Math.min(...rects.map((r) => r.left));
    const right = Math.max(...rects.map((r) => r.right));
    const top = Math.max(bar, Math.min(...rects.map((r) => r.top)));
    const bottom = Math.min(window.innerHeight, Math.max(...rects.map((r) => r.bottom)));
    root.style.setProperty("--lim-left", left + "px");
    root.style.setProperty("--lim-right", "auto");
    root.style.setProperty("--lim-top", top + "px");
    root.style.setProperty("--lim-width", (right - left) + "px");
    root.style.setProperty("--lim-height", Math.max(120, bottom - top) + "px");
    if (flush) { root.style.setProperty("--lim-shadow", "none"); root.style.setProperty("--lim-edge", "none"); }
    else { root.style.removeProperty("--lim-shadow"); root.style.removeProperty("--lim-edge"); }
  };
  const queue = () => { if (!queued) { queued = true; requestAnimationFrame(place); } };
  window.addEventListener("resize", queue);
  window.addEventListener("scroll", queue, true);
  new MutationObserver(queue).observe(document.body, { childList: true, subtree: true, attributes: true, attributeFilter: ["data-limulus-dock", "style"] });
  queue();
};

// The browser drawer's width on this page, a view preference kept per page
// in the browser's storage. A page always opens with the drawer closed (AC,
// 2026-10-05: open blank, to encourage experimenting rather than presets),
// so only the width is taken from what was kept.
export const loadDrawer = (key) => () => {
  try {
    const o = JSON.parse(localStorage.getItem(key) || "null");
    return { open: false, width: o && typeof o.width === "number" ? o.width : 280 };
  } catch (_) { return { open: false, width: 280 }; }
};
export const saveDrawer = (key) => (o) => () => {
  try { localStorage.setItem(key, JSON.stringify(o)); } catch (_) {}
};

// What is being dragged from the page now (Selene's rack rebus), for the
// drawer to judge while the drag is over it.
export const currentDrag = () => window.__tfDrag || "";
export const allowDrop = (e) => () => { e.preventDefault(); };
export const dropText = (e) => () => { e.preventDefault(); return e.dataTransfer ? e.dataTransfer.getData("text/plain") : ""; };

// A name being edited starts selected, so typing replaces it.
export const selectAll = (el) => () => { el.focus(); if (el.select) el.select(); };

// A drawer's kept width, or `fallback` when none was kept.
export const loadWidth = (key) => (fallback) => () => {
  try {
    const o = JSON.parse(localStorage.getItem(key) || "null");
    return o && typeof o.width === "number" ? o.width : fallback;
  } catch (_) { return fallback; }
};

// A row dragged from the browser drawer carries its text (a Selene module's
// line), for a drop target on the page to read.
// A progression (`vetula-progression <name>`) carries its own type, which
// Limulus reads to repoint a voice's block, and as plain text just its name
// quoted, which drops into any line as a source.
const carry = (dt, text) => {
  const m = /^vetula-progression (.+)$/.exec(text);
  if (m) {
    dt.setData("application/x-vetula-progression", m[1]);
    dt.setData("text/plain", '"' + m[1] + '"');
  } else dt.setData("text/plain", text);
  dt.effectAllowed = "copy";
};
export const setDragText = (e) => (text) => () => {
  if (e.dataTransfer) carry(e.dataTransfer, text);
  // what is being dragged, for a target to judge while the drag is over it
  // (the drag's own data cannot be read until the drop); gone when it ends
  window.__tfDrag = text;
  if (e.target) e.target.addEventListener("dragend", () => { window.__tfDrag = ""; }, { once: true });
};
