// Dropping a module from the browser drawer on a bank: let the drop land,
// and read what the drag carried (the module's line, as text).
export const allowDrop = (e) => () => { e.preventDefault(); if (e.dataTransfer) e.dataTransfer.dropEffect = "copy"; };
// What is being dragged now (Triggerfish.Standalone's drawer sets it), "" if
// nothing from the drawer.
export const currentDrag = () => window.__tfDrag || "";
// Option (Alt) held at the drop: merge rather than replace.
export const altHeld = (e) => !!e.altKey;
// The progression a drop carries (its name), "" if it carries none.
export const dropProgression = (e) => () => {
  e.preventDefault();
  return e.dataTransfer ? e.dataTransfer.getData("application/x-vetula-progression") : "";
};
export const dropText = (e) => () => {
  e.preventDefault();
  return e.dataTransfer ? e.dataTransfer.getData("text/plain") : "";
};
// Start a drag from the page (the rack's rebus) that the drawer can judge
// while it is over it, as the drawer's own rows do.
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
export const startDrag = (e) => (text) => () => {
  if (e.dataTransfer) carry(e.dataTransfer, text);
  window.__tfDrag = text;
  if (e.target) e.target.addEventListener("dragend", () => { window.__tfDrag = ""; }, { once: true });
};
