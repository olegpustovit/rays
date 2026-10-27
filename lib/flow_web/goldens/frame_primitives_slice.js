// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, new rt.PArray(1, Float64Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)), 4n, new rt.PArray(1, Float64Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)), 4n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.int(l0);
      at = 5; l5 = K[1];
      at = 6; l6 = K[2];
      at = 7; l7 = rt.array_slice(l5, l4, l6);
      at = 8; l3 = l7;
    } else {
      at = 9; l8 = rt.int(l0);
      at = 10; l9 = K[3];
      at = 11; l10 = K[4];
      at = 12; l11 = rt.array_slice(l9, l8, l10);
      at = 13; l3 = l11;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
