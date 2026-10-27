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
  K ??= [-1e3, 5000n, 3n, 0n, 0n, 0n, 5000n, 3n, 0n, 0n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41;
  let at = 0;
  const calls = new Int32Array(2);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l4 = rt.int(l0);
      at = 6; l5 = K[1];
      at = 7; l6 = rt.add(l5, l4);
      at = 8; l7 = rt.array_range(l6);
      at = 9; l8 = rt.array_count(l7);
      at = 10; l9 = K[2];
      at = 11; l10 = rt.sequence_build(l8, l9);
      at = 12; l11 = K[3];
      at = 13; l12 = l11;
      at = 14; l13 = K[4];
      at = 15; {
        const f15 = rt.coerce("int", l13), e15 = rt.coerce("int", l8), n15 = e15 > f15 ? e15 - f15 : 0n;
        if (n15 > 18014398509481983n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l14 = n15;
        for (let i15 = 0n, c15 = n15; i15 < c15; i15++) {
          const j15 = i15; l15 = j15; l16 = f15 + j15;
          at = 16; l17 = rt.array_nth(l7, l16);
          at = 17; l18 = BigInt(calls[0]++);
          at = 18; l19 = rt.int(l17);
          at = 19; l20 = rt.mul(l17, l0);
          at = 20; l21 = K[5];
          at = 21; l22 = Float64Array.of(rt.float(l19), rt.float(l20), rt.float(l21));
          at = 22; store(rt, l10, l16, l22);
        }
      }
      at = 23; l3 = l10;
    } else {
      at = 24; calls[1] = 0;
      at = 25; l23 = rt.int(l0);
      at = 26; l24 = K[6];
      at = 27; l25 = rt.add(l24, l23);
      at = 28; l26 = rt.array_range(l25);
      at = 29; l27 = rt.array_count(l26);
      at = 30; l28 = K[7];
      at = 31; l29 = rt.sequence_build(l27, l28);
      at = 32; l30 = K[8];
      at = 33; l31 = l30;
      at = 34; l32 = K[9];
      at = 35; {
        const f35 = rt.coerce("int", l32), e35 = rt.coerce("int", l27), n35 = e35 > f35 ? e35 - f35 : 0n;
        if (n35 > 18014398509481983n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l33 = n35;
        for (let i35 = 0n, c35 = n35; i35 < c35; i35++) {
          const j35 = i35; l34 = j35; l35 = f35 + j35;
          at = 36; l36 = rt.array_nth(l26, l35);
          at = 37; l37 = BigInt(calls[1]++);
          at = 38; l38 = rt.int(l36);
          at = 39; l39 = rt.mul(l36, l0);
          at = 40; l40 = K[10];
          at = 41; l41 = Float64Array.of(rt.float(l38), rt.float(l39), rt.float(l40));
          at = 42; store(rt, l29, l35, l41);
        }
      }
      at = 43; l3 = l29;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
