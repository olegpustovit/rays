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
  K ??= [-1e3, 5000n, 1n, 0n, 0n, 10, 1n, 0n, 0n, 0n, 5000n, 1n, 0n, 0n, 10, 1n, 0n, 0n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57;
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
          at = 19; l20 = K[5];
          at = 20; l21 = rt.mul(l0, l20);
          at = 21; l22 = rt.int(l21);
          at = 22; l23 = rt.gt(l19, l22);
          at = 23; if (rt.truthy(l23)) {
            at = 24; store(rt, l10, l12, l17);
            at = 25; l25 = K[6];
            at = 26; l26 = rt.add(l12, l25);
            at = 27; l12 = l26;
            at = 28; l27 = K[7];
            at = 29; l24 = l27;
          } else {
            at = 30; l28 = K[8];
            at = 31; l24 = l28;
          }
        }
      }
      at = 32; l29 = K[9];
      at = 33; l30 = rt.array_slice(l10, l29, l12);
      at = 34; l3 = l30;
    } else {
      at = 35; calls[1] = 0;
      at = 36; l31 = rt.int(l0);
      at = 37; l32 = K[10];
      at = 38; l33 = rt.add(l32, l31);
      at = 39; l34 = rt.array_range(l33);
      at = 40; l35 = rt.array_count(l34);
      at = 41; l36 = K[11];
      at = 42; l37 = rt.sequence_build(l35, l36);
      at = 43; l38 = K[12];
      at = 44; l39 = l38;
      at = 45; l40 = K[13];
      at = 46; {
        const f46 = rt.coerce("int", l40), e46 = rt.coerce("int", l35), n46 = e46 > f46 ? e46 - f46 : 0n;
        if (n46 > 18014398509481983n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l41 = n46;
        for (let i46 = 0n, c46 = n46; i46 < c46; i46++) {
          const j46 = i46; l42 = j46; l43 = f46 + j46;
          at = 47; l44 = rt.array_nth(l34, l43);
          at = 48; l45 = BigInt(calls[1]++);
          at = 49; l46 = rt.int(l44);
          at = 50; l47 = K[14];
          at = 51; l48 = rt.mul(l0, l47);
          at = 52; l49 = rt.int(l48);
          at = 53; l50 = rt.gt(l46, l49);
          at = 54; if (rt.truthy(l50)) {
            at = 55; store(rt, l37, l39, l44);
            at = 56; l52 = K[15];
            at = 57; l53 = rt.add(l39, l52);
            at = 58; l39 = l53;
            at = 59; l54 = K[16];
            at = 60; l51 = l54;
          } else {
            at = 61; l55 = K[17];
            at = 62; l51 = l55;
          }
        }
      }
      at = 63; l56 = K[18];
      at = 64; l57 = rt.array_slice(l37, l56, l39);
      at = 65; l3 = l57;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
