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
  K ??= [-1e3, 0n, 0n, 0n, 4096n, [0n, 1n, 2n, 3n], 4096n, 0n, 1n, 1n, 0n, 0n, 0n, 0n, 4096n, [0n, 1n, 2n, 3n], 4096n, 0n, 1n, 1n, 0n];
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
      at = 9; l9 = l8;
      at = 10; l10 = K[4];
      at = 11; l11 = rt.sequence_build(l10);
      at = 12; l12 = K[5];
      at = 13; {
        const s13 = l12, c13 = Array.isArray(s13) ? s13.length : Number(rt.array_count(s13));
        if (c13 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i13 = 0; i13 < c13; i13++) {
          l13 = BigInt(i13); l14 = Array.isArray(s13) ? s13[i13] : rt.array_nth(s13, i13);
          at = 14; l15 = K[6];
          at = 15; l16 = rt.ge(l7, l15);
          at = 16; if (rt.truthy(l16)) {
            at = 17; l18 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 18; l17 = l18;
          } else {
            at = 19; l19 = K[7];
            at = 20; l17 = l19;
          }
          at = 21; l20 = rt.add(l5, l0);
          at = 22; l21 = rt.coerce("float", l20);
          at = 23; l22 = rt.equal(l20, l21);
          at = 24; if (rt.truthy(l22)) {
            at = 25; l23 = l20;
          } else {
            at = 26; l24 = rt.coerce("float", l5);
            at = 27; l25 = rt.equal(l5, l24);
            at = 28; if (rt.truthy(l25)) {
              at = 29; l27 = rt.coerce("float", l20);
              at = 30; l26 = l27;
            } else {
              at = 31; l28 = rt.coerce("int", l20);
              at = 32; l26 = l28;
            }
            at = 33; l23 = l26;
          }
          at = 34; l5 = l23;
          at = 35; store(rt, l11, l9, l5);
          at = 36; l29 = K[8];
          at = 37; l30 = rt.add(l9, l29);
          at = 38; l9 = l30;
          at = 39; l31 = K[9];
          at = 40; l32 = rt.add(l7, l31);
          at = 41; l7 = l32;
        }
      }
      at = 42; l33 = K[10];
      at = 43; l34 = rt.sequence_slice(l11, l33, l9);
      at = 44; l35 = rt.sequence_concat(l34);
      at = 45; l3 = l35;
    } else {
      at = 46; l36 = K[11];
      at = 47; l37 = l36;
      at = 48; l38 = K[12];
      at = 49; l39 = l38;
      at = 50; l40 = K[13];
      at = 51; l41 = l40;
      at = 52; l42 = K[14];
      at = 53; l43 = rt.sequence_build(l42);
      at = 54; l44 = K[15];
      at = 55; {
        const s55 = l44, c55 = Array.isArray(s55) ? s55.length : Number(rt.array_count(s55));
        if (c55 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i55 = 0; i55 < c55; i55++) {
          l45 = BigInt(i55); l46 = Array.isArray(s55) ? s55[i55] : rt.array_nth(s55, i55);
          at = 56; l47 = K[16];
          at = 57; l48 = rt.ge(l39, l47);
          at = 58; if (rt.truthy(l48)) {
            at = 59; l50 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 60; l49 = l50;
          } else {
            at = 61; l51 = K[17];
            at = 62; l49 = l51;
          }
          at = 63; l52 = rt.add(l37, l0);
          at = 64; l53 = rt.coerce("float", l52);
          at = 65; l54 = rt.equal(l52, l53);
          at = 66; if (rt.truthy(l54)) {
            at = 67; l55 = l52;
          } else {
            at = 68; l56 = rt.coerce("float", l37);
            at = 69; l57 = rt.equal(l37, l56);
            at = 70; if (rt.truthy(l57)) {
              at = 71; l59 = rt.coerce("float", l52);
              at = 72; l58 = l59;
            } else {
              at = 73; l60 = rt.coerce("int", l52);
              at = 74; l58 = l60;
            }
            at = 75; l55 = l58;
          }
          at = 76; l37 = l55;
          at = 77; store(rt, l43, l41, l37);
          at = 78; l61 = K[18];
          at = 79; l62 = rt.add(l41, l61);
          at = 80; l41 = l62;
          at = 81; l63 = K[19];
          at = 82; l64 = rt.add(l39, l63);
          at = 83; l39 = l64;
        }
      }
      at = 84; l65 = K[20];
      at = 85; l66 = rt.sequence_slice(l43, l65, l41);
      at = 86; l67 = rt.sequence_concat(l66);
      at = 87; l3 = l67;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
