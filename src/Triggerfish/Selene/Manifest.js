// Wall-clock epoch ms. `performance.now()` would be steadier but is
// page-relative, and a manifest's `seenAt` has to be comparable across
// processes once apps other than this one publish them.
export const nowMs = () => Date.now();
