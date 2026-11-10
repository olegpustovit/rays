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
  K ??= [-1000, 0n, 0n, 0n, 0n, 4096n, 0n, 0n, 1n, 1n, 0n, 0n, 0n, 0n, 4096n, 0n, 0n, 1n, 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61;
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
      at = 10; l11 = K[2];
      at = 11; l12 = l11;
      at = 12; l13 = K[3];
      at = 13; l14 = l13;
      at = 14; l15 = K[4];
      at = 15; l16 = l15;
      at = 16; {
        const s16 = l10, c16 = Array.isArray(s16) ? s16.length : Number(rt.array_count(s16));
        if (c16 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i16 = 0; i16 < c16; i16++) {
          l17 = BigInt(i16); l18 = Array.isArray(s16) ? s16[i16] : rt.array_nth(s16, i16);
          at = 17; l19 = K[5];
          at = 18; l20 = rt.ge(l12, l19);
          at = 19; if (rt.truthy(l20)) {
            at = 20; l22 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 21; l21 = l22;
          } else {
            at = 22; l23 = K[6];
            at = 23; l21 = l23;
          }
          at = 24; l24 = rt.mul(l18, l0);
          at = 25; l25 = K[7];
          at = 26; l26 = rt.eq(l14, l25);
          at = 27; if (rt.truthy(l26)) {
            at = 28; l27 = l24;
          } else {
            at = 29; l28 = rt.add(l16, l24);
            at = 30; l27 = l28;
          }
          at = 31; l16 = l27;
          at = 32; l29 = K[8];
          at = 33; l30 = rt.add(l14, l29);
          at = 34; l14 = l30;
          at = 35; l31 = K[9];
          at = 36; l32 = rt.add(l12, l31);
          at = 37; l12 = l32;
        }
      }
      at = 38; l3 = l16;
    } else {
      at = 39; l33 = rt.round(l0);
      at = 40; l34 = rt.int(l33);
      at = 41; l35 = K[10];
      at = 42; {
        const f42 = rt.coerce("int", l35), e42 = rt.coerce("int", l34), n42 = e42 > f42 ? e42 - f42 : 0n;
        if (n42 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l36 = n42;
        at = 43; l39 = rt.sequence_build(l36);
        for (let i42 = 0, c42 = Number(n42); i42 < c42; i42++) {
          const j42 = BigInt(i42); l37 = j42; l38 = f42 + j42;
          at = 44; store(rt, l39, l37, l38);
        }
      }
      at = 45; l40 = K[11];
      at = 46; l41 = l40;
      at = 47; l42 = K[12];
      at = 48; l43 = l42;
      at = 49; l44 = K[13];
      at = 50; l45 = l44;
      at = 51; {
        const s51 = l39, c51 = Array.isArray(s51) ? s51.length : Number(rt.array_count(s51));
        if (c51 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i51 = 0; i51 < c51; i51++) {
          l46 = BigInt(i51); l47 = Array.isArray(s51) ? s51[i51] : rt.array_nth(s51, i51);
          at = 52; l48 = K[14];
          at = 53; l49 = rt.ge(l41, l48);
          at = 54; if (rt.truthy(l49)) {
            at = 55; l51 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 56; l50 = l51;
          } else {
            at = 57; l52 = K[15];
            at = 58; l50 = l52;
          }
          at = 59; l53 = rt.mul(l47, l0);
          at = 60; l54 = K[16];
          at = 61; l55 = rt.eq(l43, l54);
          at = 62; if (rt.truthy(l55)) {
            at = 63; l56 = l53;
          } else {
            at = 64; l57 = rt.add(l45, l53);
            at = 65; l56 = l57;
          }
          at = 66; l45 = l56;
          at = 67; l58 = K[17];
          at = 68; l59 = rt.add(l43, l58);
          at = 69; l43 = l59;
          at = 70; l60 = K[18];
          at = 71; l61 = rt.add(l41, l60);
          at = 72; l41 = l61;
        }
      }
      at = 73; l3 = l45;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
