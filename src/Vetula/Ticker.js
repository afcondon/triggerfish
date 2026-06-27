export const startTicker = (ms) => (cb) => () => {
  const id = setInterval(() => { cb(); }, ms);
  return () => { clearInterval(id); };
};
