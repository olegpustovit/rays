// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
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
  K ??= [-1000, 0n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.round(l0);
      at = 5; l5 = rt.int(l4);
      at = 6; l6 = K[1];
      at = 7; {
        const f7 = rt.coerce("int", l6), e7 = rt.coerce("int", l5), n7 = e7 > f7 ? e7 - f7 : 0n;
        if (n7 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l7 = n7;
        at = 8; l10 = rt.sequence_build(l7);
        for (let i7 = 0, c7 = Number(n7); i7 < c7; i7++) {
          const j7 = BigInt(i7); l8 = j7; l9 = f7 + j7;
          at = 9; store(rt, l10, l8, l9);
        }
      }
      at = 10; l3 = l10;
    } else {
      at = 11; l11 = rt.round(l0);
      at = 12; l12 = rt.int(l11);
      at = 13; l13 = K[2];
      at = 14; {
        const f14 = rt.coerce("int", l13), e14 = rt.coerce("int", l12), n14 = e14 > f14 ? e14 - f14 : 0n;
        if (n14 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l14 = n14;
        at = 15; l17 = rt.sequence_build(l14);
        for (let i14 = 0, c14 = Number(n14); i14 < c14; i14++) {
          const j14 = BigInt(i14); l15 = j14; l16 = f14 + j14;
          at = 16; store(rt, l17, l15, l16);
        }
      }
      at = 17; l3 = l17;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
