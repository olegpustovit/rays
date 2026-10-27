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
  K ??= [-1e3, 0n, 0n, 4096n, [0n, 1n], [0n, 1n, 2n], 4096n, 0n, 4n, true, false, 0n, 10n, 1n, 0n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n], [0n, 1n, 2n], 4096n, 0n, 4n, true, false, 0n, 10n, 1n, 0n, 1n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77;
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
            const s13 = l13, c13 = Array.isArray(s13) ? s13.length : Number(rt.array_count(s13));
            if (c13 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
            for (let i13 = 0; i13 < c13; i13++) {
              l14 = BigInt(i13); l15 = Array.isArray(s13) ? s13[i13] : rt.array_nth(s13, i13);
              at = 14; l16 = K[6];
              at = 15; l17 = rt.ge(l5, l16);
              at = 16; if (rt.truthy(l17)) {
                at = 17; l19 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
                at = 18; l18 = l19;
              } else {
                at = 19; l20 = K[7];
                at = 20; l18 = l20;
              }
              at = 21; l21 = K[8];
              at = 22; l22 = rt.eq(l5, l21);
              at = 23; l23 = K[9];
              at = 24; l24 = rt.and(l23, l22);
              at = 25; l25 = K[10];
              at = 26; l26 = rt.or(l25, l24);
              at = 27; if (rt.truthy(l26)) {
                at = 28; l28 = K[11];
                at = 29; l27 = l28;
              } else {
                at = 30; l29 = K[12];
                at = 31; l30 = rt.mul(l29, l12);
                at = 32; l31 = rt.add(l30, l15);
                at = 33; l32 = rt.add(l31, l0);
                at = 34; store(rt, l9, l7, l32);
                at = 35; l33 = K[13];
                at = 36; l34 = rt.add(l7, l33);
                at = 37; l7 = l34;
                at = 38; l35 = K[14];
                at = 39; l27 = l35;
              }
              at = 40; l36 = K[15];
              at = 41; l37 = rt.add(l5, l36);
              at = 42; l5 = l37;
            }
          }
        }
      }
      at = 43; l38 = K[16];
      at = 44; l39 = rt.sequence_slice(l9, l38, l7);
      at = 45; l40 = rt.sequence_concat(l39);
      at = 46; l3 = l40;
    } else {
      at = 47; l41 = K[17];
      at = 48; l42 = l41;
      at = 49; l43 = K[18];
      at = 50; l44 = l43;
      at = 51; l45 = K[19];
      at = 52; l46 = rt.sequence_build(l45);
      at = 53; l47 = K[20];
      at = 54; {
        const s54 = l47, c54 = Array.isArray(s54) ? s54.length : Number(rt.array_count(s54));
        if (c54 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i54 = 0; i54 < c54; i54++) {
          l48 = BigInt(i54); l49 = Array.isArray(s54) ? s54[i54] : rt.array_nth(s54, i54);
          at = 55; l50 = K[21];
          at = 56; {
            const s56 = l50, c56 = Array.isArray(s56) ? s56.length : Number(rt.array_count(s56));
            if (c56 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
            for (let i56 = 0; i56 < c56; i56++) {
              l51 = BigInt(i56); l52 = Array.isArray(s56) ? s56[i56] : rt.array_nth(s56, i56);
              at = 57; l53 = K[22];
              at = 58; l54 = rt.ge(l42, l53);
              at = 59; if (rt.truthy(l54)) {
                at = 60; l56 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
                at = 61; l55 = l56;
              } else {
                at = 62; l57 = K[23];
                at = 63; l55 = l57;
              }
              at = 64; l58 = K[24];
              at = 65; l59 = rt.eq(l42, l58);
              at = 66; l60 = K[25];
              at = 67; l61 = rt.and(l60, l59);
              at = 68; l62 = K[26];
              at = 69; l63 = rt.or(l62, l61);
              at = 70; if (rt.truthy(l63)) {
                at = 71; l65 = K[27];
                at = 72; l64 = l65;
              } else {
                at = 73; l66 = K[28];
                at = 74; l67 = rt.mul(l66, l49);
                at = 75; l68 = rt.add(l67, l52);
                at = 76; l69 = rt.add(l68, l0);
                at = 77; store(rt, l46, l44, l69);
                at = 78; l70 = K[29];
                at = 79; l71 = rt.add(l44, l70);
                at = 80; l44 = l71;
                at = 81; l72 = K[30];
                at = 82; l64 = l72;
              }
              at = 83; l73 = K[31];
              at = 84; l74 = rt.add(l42, l73);
              at = 85; l42 = l74;
            }
          }
        }
      }
      at = 86; l75 = K[32];
      at = 87; l76 = rt.sequence_slice(l46, l75, l44);
      at = 88; l77 = rt.sequence_concat(l76);
      at = 89; l3 = l77;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
