// A `storage` event fires in every OTHER tab of this origin when one writes a
// key, which is how an edit in Triggerfish's router reaches this page live.
export const onStorage = (key) => (callback) => () => {
  window.addEventListener("storage", (e) => {
    if (e.key === key) callback();
  });
};
