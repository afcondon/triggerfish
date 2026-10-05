// Dropping a module from the browser drawer on a bank: let the drop land,
// and read what the drag carried (the module's line, as text).
export const allowDrop = (e) => () => { e.preventDefault(); if (e.dataTransfer) e.dataTransfer.dropEffect = "copy"; };
export const dropText = (e) => () => {
  e.preventDefault();
  return e.dataTransfer ? e.dataTransfer.getData("text/plain") : "";
};
