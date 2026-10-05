// The time now as HH:MM, local: a capture's name until it is renamed.
export const stamp = () => {
  const d = new Date();
  return String(d.getHours()).padStart(2, "0") + ":" + String(d.getMinutes()).padStart(2, "0");
};
