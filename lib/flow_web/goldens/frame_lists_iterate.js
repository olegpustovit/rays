// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0, 3, 0n, 0, 3, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41;
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
      at = 8; l7 = rt.add(l6, l0);
      at = 9; l8 = rt.int(l7);
      at = 10; l9 = K[3];
      at = 11; {
        const f11 = rt.coerce("int", l9), e11 = rt.coerce("int", l8), n11 = e11 > f11 ? e11 - f11 : 0n;
        if (n11 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l10 = n11;
        for (let i11 = 0n, c11 = n11; i11 < c11; i11++) {
          const j11 = i11; l11 = j11; l12 = f11 + j11;
          at = 12; l13 = BigInt(calls[0]++);
          at = 13; l14 = rt.add(l5, l0);
          at = 14; l15 = rt.coerce("float", l14);
          at = 15; l16 = rt.equal(l14, l15);
          at = 16; if (rt.truthy(l16)) {
            at = 17; l17 = l14;
          } else {
            at = 18; l18 = rt.coerce("float", l5);
            at = 19; l19 = rt.equal(l5, l18);
            at = 20; if (rt.truthy(l19)) {
              at = 21; l21 = rt.coerce("float", l14);
              at = 22; l20 = l21;
            } else {
              at = 23; l22 = rt.coerce("int", l14);
              at = 24; l20 = l22;
            }
            at = 25; l17 = l20;
          }
          at = 26; l5 = l17;
        }
      }
      at = 27; l3 = l5;
    } else {
      at = 28; calls[1] = 0;
      at = 29; l23 = K[4];
      at = 30; l24 = l23;
      at = 31; l25 = K[5];
      at = 32; l26 = rt.add(l25, l0);
      at = 33; l27 = rt.int(l26);
      at = 34; l28 = K[6];
      at = 35; {
        const f35 = rt.coerce("int", l28), e35 = rt.coerce("int", l27), n35 = e35 > f35 ? e35 - f35 : 0n;
        if (n35 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l29 = n35;
        for (let i35 = 0n, c35 = n35; i35 < c35; i35++) {
          const j35 = i35; l30 = j35; l31 = f35 + j35;
          at = 36; l32 = BigInt(calls[1]++);
          at = 37; l33 = rt.add(l24, l0);
          at = 38; l34 = rt.coerce("float", l33);
          at = 39; l35 = rt.equal(l33, l34);
          at = 40; if (rt.truthy(l35)) {
            at = 41; l36 = l33;
          } else {
            at = 42; l37 = rt.coerce("float", l24);
            at = 43; l38 = rt.equal(l24, l37);
            at = 44; if (rt.truthy(l38)) {
              at = 45; l40 = rt.coerce("float", l33);
              at = 46; l39 = l40;
            } else {
              at = 47; l41 = rt.coerce("int", l33);
              at = 48; l39 = l41;
            }
            at = 49; l36 = l39;
          }
          at = 50; l24 = l36;
        }
      }
      at = 51; l3 = l24;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
