// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 2n, new rt.PArray(1, Float64Array.of(0, 0)), 2n, new rt.PArray(1, Float64Array.of(0, 0))];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.int(l0);
      at = 5; l5 = K[1];
      at = 6; l6 = rt.add(l5, l4);
      at = 7; l7 = rt.array_range(l6);
      at = 8; l8 = K[2];
      at = 9; l9 = rt.array_concat(l7, l8);
      at = 10; l3 = l9;
    } else {
      at = 11; l10 = rt.int(l0);
      at = 12; l11 = K[3];
      at = 13; l12 = rt.add(l11, l10);
      at = 14; l13 = rt.array_range(l12);
      at = 15; l14 = K[4];
      at = 16; l15 = rt.array_concat(l13, l14);
      at = 17; l3 = l15;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
