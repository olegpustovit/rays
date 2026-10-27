// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0, 4500n, 0n, 1, 0, 4500n, 0n, 1];
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
      at = 6; l5 = l4;
      at = 7; l6 = rt.int(l0);
      at = 8; l7 = K[2];
      at = 9; l8 = rt.add(l7, l6);
      at = 10; l9 = K[3];
      at = 11; {
        const f11 = rt.coerce("int", l9), e11 = rt.coerce("int", l8), n11 = e11 > f11 ? e11 - f11 : 0n;
        if (n11 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l10 = n11;
        for (let i11 = 0n, c11 = n11; i11 < c11; i11++) {
          const j11 = i11; l11 = j11; l12 = f11 + j11;
          at = 12; l13 = BigInt(calls[0]++);
          at = 13; l14 = K[4];
          at = 14; l15 = rt.add(l5, l14);
          at = 15; l16 = rt.coerce("float", l15);
          at = 16; l17 = rt.equal(l15, l16);
          at = 17; if (rt.truthy(l17)) {
            at = 18; l18 = l15;
          } else {
            at = 19; l19 = rt.coerce("float", l5);
            at = 20; l20 = rt.equal(l5, l19);
            at = 21; if (rt.truthy(l20)) {
              at = 22; l22 = rt.coerce("float", l15);
              at = 23; l21 = l22;
            } else {
              at = 24; l23 = rt.coerce("int", l15);
              at = 25; l21 = l23;
            }
            at = 26; l18 = l21;
          }
          at = 27; l5 = l18;
        }
      }
      at = 28; l3 = l5;
    } else {
      at = 29; calls[1] = 0;
      at = 30; l24 = K[5];
      at = 31; l25 = l24;
      at = 32; l26 = rt.int(l0);
      at = 33; l27 = K[6];
      at = 34; l28 = rt.add(l27, l26);
      at = 35; l29 = K[7];
      at = 36; {
        const f36 = rt.coerce("int", l29), e36 = rt.coerce("int", l28), n36 = e36 > f36 ? e36 - f36 : 0n;
        if (n36 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l30 = n36;
        for (let i36 = 0n, c36 = n36; i36 < c36; i36++) {
          const j36 = i36; l31 = j36; l32 = f36 + j36;
          at = 37; l33 = BigInt(calls[1]++);
          at = 38; l34 = K[8];
          at = 39; l35 = rt.add(l25, l34);
          at = 40; l36 = rt.coerce("float", l35);
          at = 41; l37 = rt.equal(l35, l36);
          at = 42; if (rt.truthy(l37)) {
            at = 43; l38 = l35;
          } else {
            at = 44; l39 = rt.coerce("float", l25);
            at = 45; l40 = rt.equal(l25, l39);
            at = 46; if (rt.truthy(l40)) {
              at = 47; l42 = rt.coerce("float", l35);
              at = 48; l41 = l42;
            } else {
              at = 49; l43 = rt.coerce("int", l35);
              at = 50; l41 = l43;
            }
            at = 51; l38 = l41;
          }
          at = 52; l25 = l38;
        }
      }
      at = 53; l3 = l25;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
