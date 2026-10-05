// Dropping a module from the browser drawer on a bank: let the drop land,
// and read what the drag carried (the module's line, as text).
export const allowDrop = (e) => () => { e.preventDefault(); if (e.dataTransfer) e.dataTransfer.dropEffect = "copy"; };
// What is being dragged now (Triggerfish.Standalone's drawer sets it), "" if
// nothing from the drawer.
export const currentDrag = () => window.__tfDrag || "";
// Option (Alt) held at the drop: merge rather than replace.
export const altHeld = (e) => !!e.altKey;
export const dropText = (e) => () => {
  e.preventDefault();
  return e.dataTransfer ? e.dataTransfer.getData("text/plain") : "";
};
// Start a drag from the page (the rack's rebus) that the drawer can judge
// while it is over it, as the drawer's own rows do.
export const startDrag = (e) => (text) => () => {
  if (e.dataTransfer) { e.dataTransfer.setData("text/plain", text); e.dataTransfer.effectAllowed = "copy"; }
  window.__tfDrag = text;
  if (e.target) e.target.addEventListener("dragend", () => { window.__tfDrag = ""; }, { once: true });
};
