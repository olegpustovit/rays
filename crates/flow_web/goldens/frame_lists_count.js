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
  let l16, l17, l18, l19;
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
      at = 10; l11 = rt.sequence_length(l10);
      at = 11; l3 = l11;
    } else {
      at = 12; l12 = rt.round(l0);
      at = 13; l13 = rt.int(l12);
      at = 14; l14 = K[2];
      at = 15; {
        const f15 = rt.coerce("int", l14), e15 = rt.coerce("int", l13), n15 = e15 > f15 ? e15 - f15 : 0n;
        if (n15 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l15 = n15;
        at = 16; l18 = rt.sequence_build(l15);
        for (let i15 = 0, c15 = Number(n15); i15 < c15; i15++) {
          const j15 = BigInt(i15); l16 = j15; l17 = f15 + j15;
          at = 17; store(rt, l18, l16, l17);
        }
      }
      at = 18; l19 = rt.sequence_length(l18);
      at = 19; l3 = l19;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
