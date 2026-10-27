// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
const M = new Map();
export function frame(input, state, rt) {
  K ??= [0n, 1n, 1n, 1n, 2n, 1n, 3n, 1n, 0n, 0n, 0n, 4096n, 0n, 2n, 0n, 1n, 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.mul(l1, l0);
    at = 3; l3 = K[1];
    at = 4; l4 = rt.add(l2, l3);
    at = 5; l5 = K[2];
    at = 6; l6 = rt.mul(l5, l0);
    at = 7; l7 = K[3];
    at = 8; l8 = rt.add(l6, l7);
    at = 9; l9 = K[4];
    at = 10; l10 = rt.mul(l9, l0);
    at = 11; l11 = K[5];
    at = 12; l12 = rt.add(l10, l11);
    at = 13; l13 = K[6];
    at = 14; l14 = rt.mul(l13, l0);
    at = 15; l15 = K[7];
    at = 16; l16 = rt.add(l14, l15);
    at = 17; l17 = rt.sequence_concat([l4], [l8], [l12], [l16]);
    at = 18; l18 = K[8];
    at = 19; l19 = l18;
    at = 20; l20 = K[9];
    at = 21; l21 = l20;
    at = 22; l22 = K[10];
    at = 23; l23 = l22;
    { const k24 = [l17], h24 = rt.recall(M, 24, k24);
    if (h24) [l19, l21, l23, l24, l25, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40] = h24; else {
      at = 24; {
        const s24 = l17, c24 = Array.isArray(s24) ? s24.length : Number(rt.array_count(s24));
        if (c24 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i24 = 0; i24 < c24; i24++) {
          l24 = BigInt(i24); l25 = Array.isArray(s24) ? s24[i24] : rt.array_nth(s24, i24);
          at = 25; l26 = K[11];
          at = 26; l27 = rt.ge(l19, l26);
          at = 27; if (rt.truthy(l27)) {
            at = 28; l29 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 29; l28 = l29;
          } else {
            at = 30; l30 = K[12];
            at = 31; l28 = l30;
          }
          at = 32; l31 = K[13];
          at = 33; l32 = rt.mul(l25, l31);
          at = 34; l33 = K[14];
          at = 35; l34 = rt.eq(l21, l33);
          at = 36; if (rt.truthy(l34)) {
            at = 37; l35 = l32;
          } else {
            at = 38; l36 = rt.add(l23, l32);
            at = 39; l35 = l36;
          }
          at = 40; l23 = l35;
          at = 41; l37 = K[15];
          at = 42; l38 = rt.add(l21, l37);
          at = 43; l21 = l38;
          at = 44; l39 = K[16];
          at = 45; l40 = rt.add(l19, l39);
          at = 46; l19 = l40;
        }
      }
    rt.keep(M, 24, k24, [l19, l21, l23, l24, l25, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40]); } }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l23;
}
