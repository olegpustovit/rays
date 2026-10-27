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
  K ??= [-1e3, 2, 3, 2, 3];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.sub(l0, l4);
      at = 6; l6 = K[2];
      at = 7; l7 = rt.add(l0, l6);
      at = 8; {
        const f8 = rt.coerce("int", l5), e8 = rt.coerce("int", l7), n8 = e8 > f8 ? e8 - f8 : 0n;
        if (n8 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l8 = n8;
        at = 9; l11 = rt.sequence_build(l8);
        for (let i8 = 0, c8 = Number(n8); i8 < c8; i8++) {
          const j8 = BigInt(i8); l9 = j8; l10 = f8 + j8;
          at = 10; store(rt, l11, l9, l10);
        }
      }
      at = 11; l3 = l11;
    } else {
      at = 12; l12 = K[3];
      at = 13; l13 = rt.sub(l0, l12);
      at = 14; l14 = K[4];
      at = 15; l15 = rt.add(l0, l14);
      at = 16; {
        const f16 = rt.coerce("int", l13), e16 = rt.coerce("int", l15), n16 = e16 > f16 ? e16 - f16 : 0n;
        if (n16 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l16 = n16;
        at = 17; l19 = rt.sequence_build(l16);
        for (let i16 = 0, c16 = Number(n16); i16 < c16; i16++) {
          const j16 = BigInt(i16); l17 = j16; l18 = f16 + j16;
          at = 18; store(rt, l19, l17, l18);
        }
      }
      at = 19; l3 = l19;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
