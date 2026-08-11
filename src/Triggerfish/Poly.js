// A calibration payload is the canonical JSON DeepStar writes: a table object
// whose `points` are { volts, hz, ... }. Only those two fields are read here —
// spread, agreement and provenance belong to whoever is judging the sweep, not
// to whoever is playing through it.
//
// Returns [] rather than throwing on anything unexpected. A malformed table
// means that voice falls back to nominal 1 V/oct, which is audibly wrong but
// still plays; throwing would take the whole instrument down mid-performance
// for one bad artefact.
export const parsePoints = (payload) => {
  try {
    const t = JSON.parse(payload);
    const pts = Array.isArray(t) ? t : t && t.points;
    if (!Array.isArray(pts)) return [];
    return pts
      .filter((p) => p && typeof p.volts === "number" && typeof p.hz === "number")
      .map((p) => ({ volts: p.volts, hz: p.hz }));
  } catch (e) {
    return [];
  }
};
