// Open a machine's page in a background tab, so the dashboard stays in front.
// A page cannot ask for a background tab directly; a click with the platform's
// "open in background" modifier, made while handling the user's own click, is
// what browsers honour. Where one does not, it opens in front, as a link would.
export const openInBackground = (href) => () => {
  const a = document.createElement("a");
  a.href = href;
  a.rel = "noopener";
  a.style.display = "none";
  document.body.appendChild(a);
  const mac = /Mac|iPhone|iPad/.test(navigator.platform);
  a.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, view: window, metaKey: mac, ctrlKey: !mac }));
  a.remove();
};
