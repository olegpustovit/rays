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
  K ??= [-1e3, 0n, 0n, 4096n, [0n, 1n, 2n], 0n, 4096n, 0n, 10n, 1n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n, 2n], 0n, 4096n, 0n, 10n, 1n, 1n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = l4;
      at = 6; l6 = K[2];
      at = 7; l7 = l6;
      at = 8; l8 = K[3];
      at = 9; l9 = rt.sequence_build(l8);
      at = 10; l10 = K[4];
      at = 11; {
        const s11 = l10, c11 = Array.isArray(s11) ? s11.length : Number(rt.array_count(s11));
        if (c11 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i11 = 0; i11 < c11; i11++) {
          l11 = BigInt(i11); l12 = Array.isArray(s11) ? s11[i11] : rt.array_nth(s11, i11);
          at = 12; l13 = K[5];
          at = 13; {
            const f13 = rt.coerce("int", l13), e13 = rt.coerce("int", l12), n13 = e13 > f13 ? e13 - f13 : 0n;
            if (n13 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
            l14 = n13;
            at = 14; l17 = rt.sequence_build(l14);
            for (let i13 = 0, c13 = Number(n13); i13 < c13; i13++) {
              const j13 = BigInt(i13); l15 = j13; l16 = f13 + j13;
              at = 15; store(rt, l17, l15, l16);
            }
          }
          at = 16; {
            const s16 = l17, c16 = Array.isArray(s16) ? s16.length : Number(rt.array_count(s16));
            if (c16 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
            for (let i16 = 0; i16 < c16; i16++) {
              l18 = BigInt(i16); l19 = Array.isArray(s16) ? s16[i16] : rt.array_nth(s16, i16);
              at = 17; l20 = K[6];
              at = 18; l21 = rt.ge(l5, l20);
              at = 19; if (rt.truthy(l21)) {
                at = 20; l23 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
                at = 21; l22 = l23;
              } else {
                at = 22; l24 = K[7];
                at = 23; l22 = l24;
              }
              at = 24; l25 = K[8];
              at = 25; l26 = rt.mul(l25, l12);
              at = 26; l27 = rt.add(l26, l19);
              at = 27; l28 = rt.add(l27, l0);
              at = 28; store(rt, l9, l7, l28);
              at = 29; l29 = K[9];
              at = 30; l30 = rt.add(l7, l29);
              at = 31; l7 = l30;
              at = 32; l31 = K[10];
              at = 33; l32 = rt.add(l5, l31);
              at = 34; l5 = l32;
            }
          }
        }
      }
      at = 35; l33 = K[11];
      at = 36; l34 = rt.sequence_slice(l9, l33, l7);
      at = 37; l35 = rt.sequence_concat(l34);
      at = 38; l3 = l35;
    } else {
      at = 39; l36 = K[12];
      at = 40; l37 = l36;
      at = 41; l38 = K[13];
      at = 42; l39 = l38;
      at = 43; l40 = K[14];
      at = 44; l41 = rt.sequence_build(l40);
      at = 45; l42 = K[15];
      at = 46; {
        const s46 = l42, c46 = Array.isArray(s46) ? s46.length : Number(rt.array_count(s46));
        if (c46 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i46 = 0; i46 < c46; i46++) {
          l43 = BigInt(i46); l44 = Array.isArray(s46) ? s46[i46] : rt.array_nth(s46, i46);
          at = 47; l45 = K[16];
          at = 48; {
            const f48 = rt.coerce("int", l45), e48 = rt.coerce("int", l44), n48 = e48 > f48 ? e48 - f48 : 0n;
            if (n48 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
            l46 = n48;
            at = 49; l49 = rt.sequence_build(l46);
            for (let i48 = 0, c48 = Number(n48); i48 < c48; i48++) {
              const j48 = BigInt(i48); l47 = j48; l48 = f48 + j48;
              at = 50; store(rt, l49, l47, l48);
            }
          }
          at = 51; {
            const s51 = l49, c51 = Array.isArray(s51) ? s51.length : Number(rt.array_count(s51));
            if (c51 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
            for (let i51 = 0; i51 < c51; i51++) {
              l50 = BigInt(i51); l51 = Array.isArray(s51) ? s51[i51] : rt.array_nth(s51, i51);
              at = 52; l52 = K[17];
              at = 53; l53 = rt.ge(l37, l52);
              at = 54; if (rt.truthy(l53)) {
                at = 55; l55 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
                at = 56; l54 = l55;
              } else {
                at = 57; l56 = K[18];
                at = 58; l54 = l56;
              }
              at = 59; l57 = K[19];
              at = 60; l58 = rt.mul(l57, l44);
              at = 61; l59 = rt.add(l58, l51);
              at = 62; l60 = rt.add(l59, l0);
              at = 63; store(rt, l41, l39, l60);
              at = 64; l61 = K[20];
              at = 65; l62 = rt.add(l39, l61);
              at = 66; l39 = l62;
              at = 67; l63 = K[21];
              at = 68; l64 = rt.add(l37, l63);
              at = 69; l37 = l64;
            }
          }
        }
      }
      at = 70; l65 = K[22];
      at = 71; l66 = rt.sequence_slice(l41, l65, l39);
      at = 72; l67 = rt.sequence_concat(l66);
      at = 73; l3 = l67;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
