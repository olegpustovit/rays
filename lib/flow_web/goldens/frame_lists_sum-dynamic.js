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
  K ??= [-1e3, 0n, 0n, 0n, 0n, 4096n, 0n, 0n, 1n, 1n, 0n, 0n, 0n, 0n, 4096n, 0n, 0n, 1n, 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57;
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
      at = 8; l9 = K[2];
      at = 9; l10 = l9;
      at = 10; l11 = K[3];
      at = 11; l12 = l11;
      at = 12; l13 = K[4];
      at = 13; l14 = l13;
      at = 14; {
        const s14 = l8, c14 = Array.isArray(s14) ? s14.length : Number(rt.array_count(s14));
        if (c14 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i14 = 0; i14 < c14; i14++) {
          l15 = BigInt(i14); l16 = Array.isArray(s14) ? s14[i14] : rt.array_nth(s14, i14);
          at = 15; l17 = K[5];
          at = 16; l18 = rt.ge(l10, l17);
          at = 17; if (rt.truthy(l18)) {
            at = 18; l20 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 19; l19 = l20;
          } else {
            at = 20; l21 = K[6];
            at = 21; l19 = l21;
          }
          at = 22; l22 = rt.mul(l16, l0);
          at = 23; l23 = K[7];
          at = 24; l24 = rt.eq(l12, l23);
          at = 25; if (rt.truthy(l24)) {
            at = 26; l25 = l22;
          } else {
            at = 27; l26 = rt.add(l14, l22);
            at = 28; l25 = l26;
          }
          at = 29; l14 = l25;
          at = 30; l27 = K[8];
          at = 31; l28 = rt.add(l12, l27);
          at = 32; l12 = l28;
          at = 33; l29 = K[9];
          at = 34; l30 = rt.add(l10, l29);
          at = 35; l10 = l30;
        }
      }
      at = 36; l3 = l14;
    } else {
      at = 37; l31 = K[10];
      at = 38; {
        const f38 = rt.coerce("int", l31), e38 = rt.coerce("int", l0), n38 = e38 > f38 ? e38 - f38 : 0n;
        if (n38 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l32 = n38;
        at = 39; l35 = rt.sequence_build(l32);
        for (let i38 = 0, c38 = Number(n38); i38 < c38; i38++) {
          const j38 = BigInt(i38); l33 = j38; l34 = f38 + j38;
          at = 40; store(rt, l35, l33, l34);
        }
      }
      at = 41; l36 = K[11];
      at = 42; l37 = l36;
      at = 43; l38 = K[12];
      at = 44; l39 = l38;
      at = 45; l40 = K[13];
      at = 46; l41 = l40;
      at = 47; {
        const s47 = l35, c47 = Array.isArray(s47) ? s47.length : Number(rt.array_count(s47));
        if (c47 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i47 = 0; i47 < c47; i47++) {
          l42 = BigInt(i47); l43 = Array.isArray(s47) ? s47[i47] : rt.array_nth(s47, i47);
          at = 48; l44 = K[14];
          at = 49; l45 = rt.ge(l37, l44);
          at = 50; if (rt.truthy(l45)) {
            at = 51; l47 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 52; l46 = l47;
          } else {
            at = 53; l48 = K[15];
            at = 54; l46 = l48;
          }
          at = 55; l49 = rt.mul(l43, l0);
          at = 56; l50 = K[16];
          at = 57; l51 = rt.eq(l39, l50);
          at = 58; if (rt.truthy(l51)) {
            at = 59; l52 = l49;
          } else {
            at = 60; l53 = rt.add(l41, l49);
            at = 61; l52 = l53;
          }
          at = 62; l41 = l52;
          at = 63; l54 = K[17];
          at = 64; l55 = rt.add(l39, l54);
          at = 65; l39 = l55;
          at = 66; l56 = K[18];
          at = 67; l57 = rt.add(l37, l56);
          at = 68; l37 = l57;
        }
      }
      at = 69; l3 = l41;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
