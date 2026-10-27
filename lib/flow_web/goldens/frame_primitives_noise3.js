// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0.5, 1.25, 7n, 3n, 0.5, 1.25, 7n, 3n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.mul(l0, l4);
      at = 6; l6 = K[2];
      at = 7; l7 = Float64Array.of(rt.float(l0), rt.float(l5), rt.float(l6));
      at = 8; l8 = K[3];
      at = 9; l9 = K[4];
      at = 10; l10 = rt.noise3(l7, l8, l9);
      at = 11; l3 = l10;
    } else {
      at = 12; l11 = K[5];
      at = 13; l12 = rt.mul(l0, l11);
      at = 14; l13 = K[6];
      at = 15; l14 = Float64Array.of(rt.float(l0), rt.float(l12), rt.float(l13));
      at = 16; l15 = K[7];
      at = 17; l16 = K[8];
      at = 18; l17 = rt.noise3(l14, l15, l16);
      at = 19; l3 = l17;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
