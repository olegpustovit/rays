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
  K ??= [-1e3, 1n, 2n, 1n, 1n, 2n, 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = K[2];
      at = 6; l6 = rt.sequence_concat([l0], [l4], [l5]);
      at = 7; l7 = rt.sequence_length(l6);
      at = 8; l8 = rt.sequence_build(l7);
      at = 9; {
        const s9 = l6, c9 = Array.isArray(s9) ? s9.length : Number(rt.array_count(s9));
        if (c9 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i9 = 0; i9 < c9; i9++) {
          l9 = BigInt(i9); l10 = Array.isArray(s9) ? s9[i9] : rt.array_nth(s9, i9);
          at = 10; l11 = K[3];
          at = 11; l12 = rt.sub(l7, l11);
          at = 12; l13 = rt.sub(l12, l9);
          at = 13; store(rt, l8, l13, l10);
        }
      }
      at = 14; l3 = l8;
    } else {
      at = 15; l14 = K[4];
      at = 16; l15 = K[5];
      at = 17; l16 = rt.sequence_concat([l0], [l14], [l15]);
      at = 18; l17 = rt.sequence_length(l16);
      at = 19; l18 = rt.sequence_build(l17);
      at = 20; {
        const s20 = l16, c20 = Array.isArray(s20) ? s20.length : Number(rt.array_count(s20));
        if (c20 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i20 = 0; i20 < c20; i20++) {
          l19 = BigInt(i20); l20 = Array.isArray(s20) ? s20[i20] : rt.array_nth(s20, i20);
          at = 21; l21 = K[6];
          at = 22; l22 = rt.sub(l17, l21);
          at = 23; l23 = rt.sub(l22, l19);
          at = 24; store(rt, l18, l23, l20);
        }
      }
      at = 25; l3 = l18;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
