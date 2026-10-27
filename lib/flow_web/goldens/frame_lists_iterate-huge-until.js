// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, false, 0n, 4611686018427387903n, 2, 1, 2, false, 0n, 4611686018427387903n, 2, 1, 2];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61;
  let at = 0;
  const calls = new Int32Array(4);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l4 = l0;
      at = 6; calls[1] = 0;
      at = 7; l5 = K[1];
      at = 8; l6 = l5;
      at = 9; l7 = K[2];
      at = 10; l8 = K[3];
      at = 11; {
        const f11 = rt.coerce("int", l7), e11 = rt.coerce("int", l8), n11 = e11 > f11 ? e11 - f11 : 0n;
        if (n11 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l9 = n11;
        for (let i11 = 0n, c11 = n11; i11 < c11 && !rt.truthy(l6); i11++) {
          const j11 = i11; l10 = j11; l11 = f11 + j11;
          at = 12; l12 = BigInt(calls[0]++);
          at = 13; l13 = K[4];
          at = 14; l14 = rt.add(l0, l13);
          at = 15; l15 = rt.ge(l4, l14);
          at = 16; if (rt.truthy(l15)) {
            at = 17; l17 = rt.fail("E_PAST_STOP", "body ran after stop");
            at = 18; l16 = l17;
          } else {
            at = 19; l18 = K[5];
            at = 20; l19 = rt.add(l4, l18);
            at = 21; l16 = l19;
          }
          at = 22; l20 = rt.coerce("float", l16);
          at = 23; l21 = rt.equal(l16, l20);
          at = 24; if (rt.truthy(l21)) {
            at = 25; l22 = l16;
          } else {
            at = 26; l23 = rt.coerce("float", l4);
            at = 27; l24 = rt.equal(l4, l23);
            at = 28; if (rt.truthy(l24)) {
              at = 29; l26 = rt.coerce("float", l16);
              at = 30; l25 = l26;
            } else {
              at = 31; l27 = rt.coerce("int", l16);
              at = 32; l25 = l27;
            }
            at = 33; l22 = l25;
          }
          at = 34; l28 = l22;
          at = 35; l29 = BigInt(calls[1]++);
          at = 36; l30 = K[6];
          at = 37; l31 = rt.add(l0, l30);
          at = 38; l32 = rt.ge(l28, l31);
          at = 39; l6 = l32;
          at = 40; l4 = l28;
        }
      }
      at = 41; l3 = l4;
    } else {
      at = 42; calls[2] = 0;
      at = 43; l33 = l0;
      at = 44; calls[3] = 0;
      at = 45; l34 = K[7];
      at = 46; l35 = l34;
      at = 47; l36 = K[8];
      at = 48; l37 = K[9];
      at = 49; {
        const f49 = rt.coerce("int", l36), e49 = rt.coerce("int", l37), n49 = e49 > f49 ? e49 - f49 : 0n;
        if (n49 > 4611686018427387903n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l38 = n49;
        for (let i49 = 0n, c49 = n49; i49 < c49 && !rt.truthy(l35); i49++) {
          const j49 = i49; l39 = j49; l40 = f49 + j49;
          at = 50; l41 = BigInt(calls[2]++);
          at = 51; l42 = K[10];
          at = 52; l43 = rt.add(l0, l42);
          at = 53; l44 = rt.ge(l33, l43);
          at = 54; if (rt.truthy(l44)) {
            at = 55; l46 = rt.fail("E_PAST_STOP", "body ran after stop");
            at = 56; l45 = l46;
          } else {
            at = 57; l47 = K[11];
            at = 58; l48 = rt.add(l33, l47);
            at = 59; l45 = l48;
          }
          at = 60; l49 = rt.coerce("float", l45);
          at = 61; l50 = rt.equal(l45, l49);
          at = 62; if (rt.truthy(l50)) {
            at = 63; l51 = l45;
          } else {
            at = 64; l52 = rt.coerce("float", l33);
            at = 65; l53 = rt.equal(l33, l52);
            at = 66; if (rt.truthy(l53)) {
              at = 67; l55 = rt.coerce("float", l45);
              at = 68; l54 = l55;
            } else {
              at = 69; l56 = rt.coerce("int", l45);
              at = 70; l54 = l56;
            }
            at = 71; l51 = l54;
          }
          at = 72; l57 = l51;
          at = 73; l58 = BigInt(calls[3]++);
          at = 74; l59 = K[12];
          at = 75; l60 = rt.add(l0, l59);
          at = 76; l61 = rt.ge(l57, l60);
          at = 77; l35 = l61;
          at = 78; l33 = l57;
        }
      }
      at = 79; l3 = l33;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
