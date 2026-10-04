// Open a page in a background tab from a plain click on anything carrying
// `data-bg-open="<href>"`. A page cannot ask for a background tab directly;
// browsers honour the platform's "open in background" modifier on a click
// made while handling the user's own click. That must happen inside the
// click itself, so this is one document listener, installed once, rather
// than a Halogen handler (which may run a moment later).
export const install = () => {
  if (window.__bgOpenInstalled) return;
  window.__bgOpenInstalled = true;
  document.addEventListener("click", (e) => {
    const el = e.target instanceof Element ? e.target.closest("[data-bg-open]") : null;
    if (!el) return;
    e.preventDefault();
    const a = document.createElement("a");
    a.href = el.getAttribute("data-bg-open");
    a.target = "_blank";
    a.rel = "noopener";
    a.style.display = "none";
    document.body.appendChild(a);
    const mac = /Mac|iPhone|iPad/.test(navigator.platform);
    a.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, view: window, metaKey: mac, ctrlKey: !mac }));
    a.remove();
  }, true);
};
