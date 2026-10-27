// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0, 0n, 3n, 0, 0n, 3n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44;
  let at = 0;
  const calls = new Int32Array(2);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l3 = K[1];
      at = 6; l4 = l3;
      at = 7; l5 = K[2];
      at = 8; l6 = K[3];
      at = 9; {
        const f9 = rt.coerce("int", l5), e9 = rt.coerce("int", l6), n9 = e9 > f9 ? e9 - f9 : 0n;
        if (n9 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l7 = n9;
        for (let i9 = 0n, c9 = n9; i9 < c9; i9++) {
          const j9 = i9; l8 = j9; l9 = f9 + j9;
          at = 10; l10 = BigInt(calls[0]++);
          at = 11; l11 = rt.add(l4, l0);
          at = 12; l12 = rt.coerce("float", l4);
          at = 13; l13 = rt.equal(l4, l12);
          at = 14; if (rt.truthy(l13)) {
            at = 15; l15 = rt.coerce("float", l11);
            at = 16; l14 = l15;
          } else {
            at = 17; l16 = rt.coerce("int", l11);
            at = 18; l14 = l16;
          }
          at = 19; l17 = rt.coerce("float", l4);
          at = 20; l18 = rt.equal(l4, l17);
          at = 21; if (rt.truthy(l18)) {
            at = 22; l20 = rt.coerce("float", l14);
            at = 23; l19 = l20;
          } else {
            at = 24; l21 = rt.coerce("int", l14);
            at = 25; l19 = l21;
          }
          at = 26; l22 = l19;
          at = 27; l4 = l22;
        }
      }
      at = 52; l43 = l4;
    } else {
      at = 28; calls[1] = 0;
      at = 29; l23 = K[4];
      at = 30; l24 = l23;
      at = 31; l25 = K[5];
      at = 32; l26 = K[6];
      at = 33; {
        const f33 = rt.coerce("int", l25), e33 = rt.coerce("int", l26), n33 = e33 > f33 ? e33 - f33 : 0n;
        if (n33 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l27 = n33;
        for (let i33 = 0n, c33 = n33; i33 < c33; i33++) {
          const j33 = i33; l28 = j33; l29 = f33 + j33;
          at = 34; l30 = BigInt(calls[1]++);
          at = 35; l31 = rt.add(l24, l0);
          at = 36; l32 = rt.coerce("float", l24);
          at = 37; l33 = rt.equal(l24, l32);
          at = 38; if (rt.truthy(l33)) {
            at = 39; l35 = rt.coerce("float", l31);
            at = 40; l34 = l35;
          } else {
            at = 41; l36 = rt.coerce("int", l31);
            at = 42; l34 = l36;
          }
          at = 43; l37 = rt.coerce("float", l24);
          at = 44; l38 = rt.equal(l24, l37);
          at = 45; if (rt.truthy(l38)) {
            at = 46; l40 = rt.coerce("float", l34);
            at = 47; l39 = l40;
          } else {
            at = 48; l41 = rt.coerce("int", l34);
            at = 49; l39 = l41;
          }
          at = 50; l42 = l39;
          at = 51; l24 = l42;
        }
      }
      at = 53; l43 = l24;
    }
    at = 54; l44 = new Map([["n", l43]]);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l44;
}
