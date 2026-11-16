// Triggerfish.Clips.Kept — what this browser knows it has sent.

export const loadText = (key) => () => {
  try {
    return window.localStorage.getItem(key);
  } catch (_) {
    return null;
  }
};

export const saveText = (key) => (text) => () => {
  try {
    window.localStorage.setItem(key, text);
  } catch (_) {
    // quota or private mode: Amphora still has it
  }
};
