export const onFrames = (each) => () => {
  let running = true;
  const frame = (t) => { if (!running) return; each(t)(); requestAnimationFrame(frame); };
  requestAnimationFrame(frame);
  return () => { running = false; };
};
