// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
const M = new Map();
export function frame(input, state, rt) {
  K ??= [0n, 1n, 1n, 1n, 2n, 1n, 3n, 1n, 0n, 0n, 0n, 4096n, 0n, 2n, 0n, 1n, 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.mul(l1, l0);
    at = 3; l3 = K[1];
    at = 4; l4 = rt.add(l2, l3);
    at = 5; l5 = input.t;
    at = 6; l6 = K[2];
    at = 7; l7 = rt.mul(l6, l5);
    at = 8; l8 = K[3];
    at = 9; l9 = rt.add(l7, l8);
    at = 10; l10 = input.t;
    at = 11; l11 = K[4];
    at = 12; l12 = rt.mul(l11, l10);
    at = 13; l13 = K[5];
    at = 14; l14 = rt.add(l12, l13);
    at = 15; l15 = input.t;
    at = 16; l16 = K[6];
    at = 17; l17 = rt.mul(l16, l15);
    at = 18; l18 = K[7];
    at = 19; l19 = rt.add(l17, l18);
    at = 20; l20 = rt.sequence_concat([l4], [l9], [l14], [l19]);
    at = 21; l21 = K[8];
    at = 22; l22 = l21;
    at = 23; l23 = K[9];
    at = 24; l24 = l23;
    at = 25; l25 = K[10];
    at = 26; l26 = l25;
    { const k27 = [l20], h27 = rt.recall(M, 27, k27);
    if (h27) [l22, l24, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43] = h27; else {
      at = 27; {
        const s27 = l20, c27 = Array.isArray(s27) ? s27.length : Number(rt.array_count(s27));
        if (c27 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i27 = 0; i27 < c27; i27++) {
          l27 = BigInt(i27); l28 = Array.isArray(s27) ? s27[i27] : rt.array_nth(s27, i27);
          at = 28; l29 = K[11];
          at = 29; l30 = rt.ge(l22, l29);
          at = 30; if (rt.truthy(l30)) {
            at = 31; l32 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 32; l31 = l32;
          } else {
            at = 33; l33 = K[12];
            at = 34; l31 = l33;
          }
          at = 35; l34 = K[13];
          at = 36; l35 = rt.mul(l28, l34);
          at = 37; l36 = K[14];
          at = 38; l37 = rt.eq(l24, l36);
          at = 39; if (rt.truthy(l37)) {
            at = 40; l38 = l35;
          } else {
            at = 41; l39 = rt.add(l26, l35);
            at = 42; l38 = l39;
          }
          at = 43; l26 = l38;
          at = 44; l40 = K[15];
          at = 45; l41 = rt.add(l24, l40);
          at = 46; l24 = l41;
          at = 47; l42 = K[16];
          at = 48; l43 = rt.add(l22, l42);
          at = 49; l22 = l43;
        }
      }
    rt.keep(M, 27, k27, [l22, l24, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43]); } }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l26;
}
