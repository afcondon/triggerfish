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
// marked "always" keeps the panel open while it is on the page (Vetula's
// Perform). No dock: the panel stands on the right, as the bar's CSS says.
// The place goes into CSS variables on the root, so Halogen's own render of
// the panel's style never fights it.
export const watchDocks = (onAlways) => () => {
  const root = document.documentElement;
  let always = null, queued = false;
  const vars = ["--lim-left", "--lim-right", "--lim-top", "--lim-width", "--lim-height"];
  const place = () => {
    queued = false;
    const docks = [...document.querySelectorAll("[data-limulus-dock]")];
    const nowAlways = docks.some((d) => d.dataset.limulusDock === "always");
    if (nowAlways !== always) { always = nowAlways; onAlways(nowAlways)(); }
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
  };
  const queue = () => { if (!queued) { queued = true; requestAnimationFrame(place); } };
  window.addEventListener("resize", queue);
  window.addEventListener("scroll", queue, true);
  new MutationObserver(queue).observe(document.body, { childList: true, subtree: true, attributes: true, attributeFilter: ["data-limulus-dock", "style"] });
  queue();
};
