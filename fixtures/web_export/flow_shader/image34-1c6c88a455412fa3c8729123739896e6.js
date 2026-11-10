// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [360n, 240n, 0.9, 0.12, 1, 1.3, 0.12, 1n, 0.19, 0.7, 0.06, 0.5, 0.27];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; l1 = K[1];
    at = 2; l2 = input.t;
    at = 3; l3 = K[2];
    at = 4; l4 = rt.mul(l2, l3);
    at = 5; l5 = rt.sin(l4);
    at = 6; l6 = K[3];
    at = 7; l7 = rt.mul(l6, l5);
    at = 8; l8 = K[4];
    at = 9; l9 = rt.add(l8, l7);
    at = 10; l10 = K[5];
    at = 11; l11 = rt.mul(l2, l10);
    at = 12; l12 = rt.sin(l11);
    at = 13; l13 = K[6];
    at = 14; l14 = rt.mul(l13, l12);
    at = 15; l15 = K[7];
    at = 16; l16 = rt.add(l15, l14);
    at = 17; l17 = K[8];
    at = 18; l18 = rt.mul(l17, l16);
    at = 19; l19 = K[9];
    at = 20; l20 = rt.mul(l2, l19);
    at = 21; l21 = rt.sin(l20);
    at = 22; l22 = K[10];
    at = 23; l23 = rt.mul(l22, l21);
    at = 24; l24 = K[11];
    at = 25; l25 = rt.add(l24, l23);
    at = 26; l26 = K[12];
    at = 27; l27 = rt.mul(l26, l16);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [l0, l1, l9, l18, l25, l27];
}

export const regions = [];
