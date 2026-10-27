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
  K ??= [-1e3, 8n, 0n, 0n, 8n, [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n], 1n, 0n, 0n, 0n, 8n, 0n, 0n, 8n, [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n], 1n, 0n, 0n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43;
  let at = 0;
  const calls = new Int32Array(2);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l4 = K[1];
      at = 6; l5 = rt.sequence_build(l4);
      at = 7; l6 = K[2];
      at = 8; l7 = l6;
      at = 9; l8 = K[3];
      at = 10; l9 = K[4];
      at = 11; {
        const f11 = rt.coerce("int", l8), e11 = rt.coerce("int", l9), n11 = e11 > f11 ? e11 - f11 : 0n;
        if (n11 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l10 = n11;
        for (let i11 = 0, c11 = Number(n11); i11 < c11; i11++) {
          const j11 = BigInt(i11); l11 = j11; l12 = f11 + j11;
          at = 12; l13 = K[5];
          at = 13; l14 = rt.sequence_get(l13, l12);
          at = 14; l15 = BigInt(calls[0]++);
          at = 15; l16 = rt.gt(l14, l0);
          at = 16; if (rt.truthy(l16)) {
            at = 17; store(rt, l5, l7, l14);
            at = 18; l18 = K[6];
            at = 19; l19 = rt.add(l7, l18);
            at = 20; l7 = l19;
            at = 21; l20 = K[7];
            at = 22; l17 = l20;
          } else {
            at = 23; l21 = K[8];
            at = 24; l17 = l21;
          }
        }
      }
      at = 25; l22 = K[9];
      at = 26; l23 = rt.sequence_slice(l5, l22, l7);
      at = 27; l3 = l23;
    } else {
      at = 28; calls[1] = 0;
      at = 29; l24 = K[10];
      at = 30; l25 = rt.sequence_build(l24);
      at = 31; l26 = K[11];
      at = 32; l27 = l26;
      at = 33; l28 = K[12];
      at = 34; l29 = K[13];
      at = 35; {
        const f35 = rt.coerce("int", l28), e35 = rt.coerce("int", l29), n35 = e35 > f35 ? e35 - f35 : 0n;
        if (n35 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l30 = n35;
        for (let i35 = 0, c35 = Number(n35); i35 < c35; i35++) {
          const j35 = BigInt(i35); l31 = j35; l32 = f35 + j35;
          at = 36; l33 = K[14];
          at = 37; l34 = rt.sequence_get(l33, l32);
          at = 38; l35 = BigInt(calls[1]++);
          at = 39; l36 = rt.gt(l34, l0);
          at = 40; if (rt.truthy(l36)) {
            at = 41; store(rt, l25, l27, l34);
            at = 42; l38 = K[15];
            at = 43; l39 = rt.add(l27, l38);
            at = 44; l27 = l39;
            at = 45; l40 = K[16];
            at = 46; l37 = l40;
          } else {
            at = 47; l41 = K[17];
            at = 48; l37 = l41;
          }
        }
      }
      at = 49; l42 = K[18];
      at = 50; l43 = rt.sequence_slice(l25, l42, l27);
      at = 51; l3 = l43;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
