export const typingIn = (event) => {
  const t = event.target;
  const tag = t && t.tagName;
  return tag === "INPUT" || tag === "SELECT" || tag === "TEXTAREA" || (t && t.isContentEditable === true);
};
