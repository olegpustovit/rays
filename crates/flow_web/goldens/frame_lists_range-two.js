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
  K ??= [-1000, 2, 3, 2, 3];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.sub(l0, l4);
      at = 6; l6 = rt.round(l5);
      at = 7; l7 = rt.int(l6);
      at = 8; l8 = K[2];
      at = 9; l9 = rt.add(l0, l8);
      at = 10; l10 = rt.round(l9);
      at = 11; l11 = rt.int(l10);
      at = 12; {
        const f12 = rt.coerce("int", l7), e12 = rt.coerce("int", l11), n12 = e12 > f12 ? e12 - f12 : 0n;
        if (n12 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l12 = n12;
        at = 13; l15 = rt.sequence_build(l12);
        for (let i12 = 0, c12 = Number(n12); i12 < c12; i12++) {
          const j12 = BigInt(i12); l13 = j12; l14 = f12 + j12;
          at = 14; store(rt, l15, l13, l14);
        }
      }
      at = 15; l3 = l15;
    } else {
      at = 16; l16 = K[3];
      at = 17; l17 = rt.sub(l0, l16);
      at = 18; l18 = rt.round(l17);
      at = 19; l19 = rt.int(l18);
      at = 20; l20 = K[4];
      at = 21; l21 = rt.add(l0, l20);
      at = 22; l22 = rt.round(l21);
      at = 23; l23 = rt.int(l22);
      at = 24; {
        const f24 = rt.coerce("int", l19), e24 = rt.coerce("int", l23), n24 = e24 > f24 ? e24 - f24 : 0n;
        if (n24 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l24 = n24;
        at = 25; l27 = rt.sequence_build(l24);
        for (let i24 = 0, c24 = Number(n24); i24 < c24; i24++) {
          const j24 = BigInt(i24); l25 = j24; l26 = f24 + j24;
          at = 26; store(rt, l27, l25, l26);
        }
      }
      at = 27; l3 = l27;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
