// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [-1000, 0n, 0n, 3n, 3n, [0n, 1n, 2n], 0n, 0n, 0n, 3n, 3n, [0n, 1n, 2n], 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.round(l0);
      at = 5; l5 = rt.int(l4);
      at = 6; l6 = K[1];
      at = 7; l7 = rt.int_lt(l5, l6);
      at = 8; if (rt.truthy(l7)) {
        at = 9; l9 = K[2];
        at = 10; l8 = l9;
      } else {
        at = 11; l8 = l5;
      }
      at = 12; l10 = K[3];
      at = 13; l11 = rt.int_gt(l8, l10);
      at = 14; if (rt.truthy(l11)) {
        at = 15; l13 = K[4];
        at = 16; l12 = l13;
      } else {
        at = 17; l12 = l8;
      }
      at = 18; l14 = K[5];
      at = 19; l15 = K[6];
      at = 20; l16 = rt.sequence_slice(l14, l15, l12);
      at = 21; l3 = l16;
    } else {
      at = 22; l17 = rt.round(l0);
      at = 23; l18 = rt.int(l17);
      at = 24; l19 = K[7];
      at = 25; l20 = rt.int_lt(l18, l19);
      at = 26; if (rt.truthy(l20)) {
        at = 27; l22 = K[8];
        at = 28; l21 = l22;
      } else {
        at = 29; l21 = l18;
      }
      at = 30; l23 = K[9];
      at = 31; l24 = rt.int_gt(l21, l23);
      at = 32; if (rt.truthy(l24)) {
        at = 33; l26 = K[10];
        at = 34; l25 = l26;
      } else {
        at = 35; l25 = l21;
      }
      at = 36; l27 = K[11];
      at = 37; l28 = K[12];
      at = 38; l29 = rt.sequence_slice(l27, l28, l25);
      at = 39; l3 = l29;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
