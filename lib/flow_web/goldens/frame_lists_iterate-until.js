// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0, false, 0n, 8n, 2, 0, false, 0n, 8n, 2];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49;
  let at = 0;
  const calls = new Int32Array(4);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l4 = K[1];
      at = 6; l5 = l4;
      at = 7; calls[1] = 0;
      at = 8; l6 = K[2];
      at = 9; l7 = l6;
      at = 10; l8 = K[3];
      at = 11; l9 = K[4];
      at = 12; {
        const f12 = rt.coerce("int", l8), e12 = rt.coerce("int", l9), n12 = e12 > f12 ? e12 - f12 : 0n;
        if (n12 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l10 = n12;
        for (let i12 = 0n, c12 = n12; i12 < c12 && !rt.truthy(l7); i12++) {
          const j12 = i12; l11 = j12; l12 = f12 + j12;
          at = 13; l13 = BigInt(calls[0]++);
          at = 14; l14 = rt.add(l5, l0);
          at = 15; l15 = rt.coerce("float", l14);
          at = 16; l16 = rt.equal(l14, l15);
          at = 17; if (rt.truthy(l16)) {
            at = 18; l17 = l14;
          } else {
            at = 19; l18 = rt.coerce("float", l5);
            at = 20; l19 = rt.equal(l5, l18);
            at = 21; if (rt.truthy(l19)) {
              at = 22; l21 = rt.coerce("float", l14);
              at = 23; l20 = l21;
            } else {
              at = 24; l22 = rt.coerce("int", l14);
              at = 25; l20 = l22;
            }
            at = 26; l17 = l20;
          }
          at = 27; l23 = l17;
          at = 28; l24 = BigInt(calls[1]++);
          at = 29; l25 = K[5];
          at = 30; l26 = rt.gt(l23, l25);
          at = 31; l7 = l26;
          at = 32; l5 = l23;
        }
      }
      at = 33; l3 = l5;
    } else {
      at = 34; calls[2] = 0;
      at = 35; l27 = K[6];
      at = 36; l28 = l27;
      at = 37; calls[3] = 0;
      at = 38; l29 = K[7];
      at = 39; l30 = l29;
      at = 40; l31 = K[8];
      at = 41; l32 = K[9];
      at = 42; {
        const f42 = rt.coerce("int", l31), e42 = rt.coerce("int", l32), n42 = e42 > f42 ? e42 - f42 : 0n;
        if (n42 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l33 = n42;
        for (let i42 = 0n, c42 = n42; i42 < c42 && !rt.truthy(l30); i42++) {
          const j42 = i42; l34 = j42; l35 = f42 + j42;
          at = 43; l36 = BigInt(calls[2]++);
          at = 44; l37 = rt.add(l28, l0);
          at = 45; l38 = rt.coerce("float", l37);
          at = 46; l39 = rt.equal(l37, l38);
          at = 47; if (rt.truthy(l39)) {
            at = 48; l40 = l37;
          } else {
            at = 49; l41 = rt.coerce("float", l28);
            at = 50; l42 = rt.equal(l28, l41);
            at = 51; if (rt.truthy(l42)) {
              at = 52; l44 = rt.coerce("float", l37);
              at = 53; l43 = l44;
            } else {
              at = 54; l45 = rt.coerce("int", l37);
              at = 55; l43 = l45;
            }
            at = 56; l40 = l43;
          }
          at = 57; l46 = l40;
          at = 58; l47 = BigInt(calls[3]++);
          at = 59; l48 = K[10];
          at = 60; l49 = rt.gt(l46, l48);
          at = 61; l30 = l49;
          at = 62; l28 = l46;
        }
      }
      at = 63; l3 = l28;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
