// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0n, 0n, 3n, 0n, 0n, 3n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37;
  let at = 0;
  const calls = new Int32Array(2);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l4 = K[1];
      at = 6; l5 = l4;
      at = 7; l6 = K[2];
      at = 8; l7 = K[3];
      at = 9; {
        const f9 = rt.coerce("int", l6), e9 = rt.coerce("int", l7), n9 = e9 > f9 ? e9 - f9 : 0n;
        if (n9 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l8 = n9;
        for (let i9 = 0n, c9 = n9; i9 < c9; i9++) {
          const j9 = i9; l9 = j9; l10 = f9 + j9;
          at = 10; l11 = BigInt(calls[0]++);
          at = 11; l12 = rt.add(l5, l0);
          at = 12; l13 = rt.coerce("float", l12);
          at = 13; l14 = rt.equal(l12, l13);
          at = 14; if (rt.truthy(l14)) {
            at = 15; l15 = l12;
          } else {
            at = 16; l16 = rt.coerce("float", l5);
            at = 17; l17 = rt.equal(l5, l16);
            at = 18; if (rt.truthy(l17)) {
              at = 19; l19 = rt.coerce("float", l12);
              at = 20; l18 = l19;
            } else {
              at = 21; l20 = rt.coerce("int", l12);
              at = 22; l18 = l20;
            }
            at = 23; l15 = l18;
          }
          at = 24; l5 = l15;
        }
      }
      at = 25; l3 = l5;
    } else {
      at = 26; calls[1] = 0;
      at = 27; l21 = K[4];
      at = 28; l22 = l21;
      at = 29; l23 = K[5];
      at = 30; l24 = K[6];
      at = 31; {
        const f31 = rt.coerce("int", l23), e31 = rt.coerce("int", l24), n31 = e31 > f31 ? e31 - f31 : 0n;
        if (n31 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l25 = n31;
        for (let i31 = 0n, c31 = n31; i31 < c31; i31++) {
          const j31 = i31; l26 = j31; l27 = f31 + j31;
          at = 32; l28 = BigInt(calls[1]++);
          at = 33; l29 = rt.add(l22, l0);
          at = 34; l30 = rt.coerce("float", l29);
          at = 35; l31 = rt.equal(l29, l30);
          at = 36; if (rt.truthy(l31)) {
            at = 37; l32 = l29;
          } else {
            at = 38; l33 = rt.coerce("float", l22);
            at = 39; l34 = rt.equal(l22, l33);
            at = 40; if (rt.truthy(l34)) {
              at = 41; l36 = rt.coerce("float", l29);
              at = 42; l35 = l36;
            } else {
              at = 43; l37 = rt.coerce("int", l29);
              at = 44; l35 = l37;
            }
            at = 45; l32 = l35;
          }
          at = 46; l22 = l32;
        }
      }
      at = 47; l3 = l22;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
