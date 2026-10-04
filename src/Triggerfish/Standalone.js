// The Limulus panel asks to close (Escape inside it): a message from the
// panel's own frame, on this page's origin.
export const limulusAskedClose = (e) =>
  e.origin === window.location.origin && !!e.data && e.data.limulus === "close";

// Give the panel's frame the keyboard (its window's focus also has Limulus
// take the latest buffer), or take it back.
export const focusFrame = (el) => () => { if (el && el.contentWindow) el.contentWindow.focus(); };
export const focusSelf = () => window.focus();
