// A ghost fish is a real link (`data-peek`), so the browser's own cmd-click
// (ctrl-click elsewhere) opens its machine in a background tab: that is the
// only background open a browser allows, since a click made by the page,
// modifiers and all, now opens in front. A plain click on one is swallowed
// here, so looking at a fish never moves you to a new tab.
export const install = () => {
  if (window.__peekInstalled) return;
  window.__peekInstalled = true;
  document.addEventListener("click", (e) => {
    const el = e.target instanceof Element ? e.target.closest("[data-peek]") : null;
    if (el && !(e.metaKey || e.ctrlKey || e.shiftKey)) e.preventDefault();
  }, true);
};
