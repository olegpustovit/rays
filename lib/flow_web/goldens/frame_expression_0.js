// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [0n, 1n, 1n, 1n, 3n, 3n, 2n, 5n, 5n, 3n, 7n, 7n, 4n, 9n, 9n, 5n, 11n, 11n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.add(l0, l1);
    at = 3; l3 = K[1];
    at = 4; l4 = rt.mul(l2, l3);
    at = 5; l5 = rt.sin(l4);
    at = 6; l6 = K[2];
    at = 7; l7 = rt.div(l5, l6);
    at = 8; l8 = K[3];
    at = 9; l9 = rt.add(l0, l8);
    at = 10; l10 = K[4];
    at = 11; l11 = rt.mul(l9, l10);
    at = 12; l12 = rt.sin(l11);
    at = 13; l13 = K[5];
    at = 14; l14 = rt.div(l12, l13);
    at = 15; l15 = K[6];
    at = 16; l16 = rt.add(l0, l15);
    at = 17; l17 = K[7];
    at = 18; l18 = rt.mul(l16, l17);
    at = 19; l19 = rt.sin(l18);
    at = 20; l20 = K[8];
    at = 21; l21 = rt.div(l19, l20);
    at = 22; l22 = K[9];
    at = 23; l23 = rt.add(l0, l22);
    at = 24; l24 = K[10];
    at = 25; l25 = rt.mul(l23, l24);
    at = 26; l26 = rt.sin(l25);
    at = 27; l27 = K[11];
    at = 28; l28 = rt.div(l26, l27);
    at = 29; l29 = K[12];
    at = 30; l30 = rt.add(l0, l29);
    at = 31; l31 = K[13];
    at = 32; l32 = rt.mul(l30, l31);
    at = 33; l33 = rt.sin(l32);
    at = 34; l34 = K[14];
    at = 35; l35 = rt.div(l33, l34);
    at = 36; l36 = K[15];
    at = 37; l37 = rt.add(l0, l36);
    at = 38; l38 = K[16];
    at = 39; l39 = rt.mul(l37, l38);
    at = 40; l40 = rt.sin(l39);
    at = 41; l41 = K[17];
    at = 42; l42 = rt.div(l40, l41);
    at = 43; l43 = rt.add(l7, l14);
    at = 44; l44 = rt.add(l43, l21);
    at = 45; l45 = rt.add(l44, l28);
    at = 46; l46 = rt.add(l45, l35);
    at = 47; l47 = rt.add(l46, l42);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l47;
}
