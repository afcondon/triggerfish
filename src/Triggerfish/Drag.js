// Dragging a saved thing out of a machine's drawer (or off its page), and
// dropping it: one place, for every machine.
//
// A drag's text is `<kind> <payload>` when the thing is one of the kinds
// below, which every drop target can recognise by its own type
// (`application/x-<kind>`) and Limulus turns into a line; any other text is
// carried as it is (a Selene module's line, the rack's rebus `keep:rack`).
// Each kind also says what it is as plain text, for a drop anywhere else.
const kinds = {
  // a Vetula progression: its name, quoted (a source anywhere in a line)
  "vetula-progression": (name) => '"' + name + '"',
  // a Balistes beat, an Odonus patch: the word, bare (it goes in a pattern)
  "balistes-beat": (word) => word,
  "odonus-patch": (word) => word,
  // a Selene module: its line
  "selene-module": (line) => line,
};

const carry = (dt, text) => {
  const at = text.indexOf(" ");
  const kind = at < 0 ? "" : text.slice(0, at);
  const plain = kinds[kind];
  if (plain) {
    const payload = text.slice(at + 1);
    dt.setData("application/x-" + kind, payload);
    dt.setData("text/plain", plain(payload));
  } else dt.setData("text/plain", text);
  dt.effectAllowed = "copy";
};

export const startDrag = (e) => (text) => () => {
  if (e.dataTransfer) carry(e.dataTransfer, text);
  // what is being dragged, for a target to judge while the drag is over it
  // (the drag's own data cannot be read until the drop); gone when it ends
  window.__tfDrag = text;
  if (e.target) e.target.addEventListener("dragend", () => { window.__tfDrag = ""; }, { once: true });
};

export const currentDrag = () => window.__tfDrag || "";

export const allowDrop = (e) => () => { e.preventDefault(); if (e.dataTransfer) e.dataTransfer.dropEffect = "copy"; };

export const altHeld = (e) => !!e.altKey;

export const dropText = (e) => () => {
  e.preventDefault();
  return e.dataTransfer ? e.dataTransfer.getData("text/plain") : "";
};

export const dropOf = (kind) => (e) => () => {
  e.preventDefault();
  return e.dataTransfer ? e.dataTransfer.getData("application/x-" + kind) : "";
};
