// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
const store = (rt, s, index, value) => {
  const i = Number(rt.coerce("int", index));
  if (s instanceof rt.TextArray) {
    if (i < 0 || i >= s.data.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (typeof value !== 'string') rt.fail("E_ARRAY_TYPE", "Text arrays contain text values.");
    s.data[i] = value;
  } else if (s instanceof rt.PArray) {
    const n = s.data.length / s.width;
    if (i < 0 || i >= n) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (s.data instanceof BigInt64Array) {
      if (typeof value !== 'number' && typeof value !== 'bigint' && typeof value !== 'boolean')
        rt.fail("E_ARRAY_TYPE", "Packed integer arrays contain integer values.");
      s.data[i] = rt.coerce("int", value);
      return;
    }
    const comps = value instanceof Float64Array ? value : Array(s.width).fill(rt.float(value));
    if (comps.length !== s.width) rt.fail("E_ARRAY_TYPE", "Packed array element has the wrong vector width.");
    for (let j = 0; j < s.width; j++)
      s.data[i * s.width + j] = Number.isFinite(comps[j]) ? comps[j] : rt.fail("E_NONFINITE", "array produced a nonfinite value.");
  } else {
    if (i < 0 || i >= s.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    s[i] = value;
  }
};
export function frame(input, state, rt) {
  K ??= [-1e3, 0n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; {
        const f5 = rt.coerce("int", l4), e5 = rt.coerce("int", l0), n5 = e5 > f5 ? e5 - f5 : 0n;
        if (n5 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l5 = n5;
        at = 6; l8 = rt.sequence_build(l5);
        for (let i5 = 0, c5 = Number(n5); i5 < c5; i5++) {
          const j5 = BigInt(i5); l6 = j5; l7 = f5 + j5;
          at = 7; store(rt, l8, l6, l7);
        }
      }
      at = 8; l9 = rt.sequence_length(l8);
      at = 9; l3 = l9;
    } else {
      at = 10; l10 = K[2];
      at = 11; {
        const f11 = rt.coerce("int", l10), e11 = rt.coerce("int", l0), n11 = e11 > f11 ? e11 - f11 : 0n;
        if (n11 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l11 = n11;
        at = 12; l14 = rt.sequence_build(l11);
        for (let i11 = 0, c11 = Number(n11); i11 < c11; i11++) {
          const j11 = BigInt(i11); l12 = j11; l13 = f11 + j11;
          at = 13; store(rt, l14, l12, l13);
        }
      }
      at = 14; l15 = rt.sequence_length(l14);
      at = 15; l3 = l15;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
