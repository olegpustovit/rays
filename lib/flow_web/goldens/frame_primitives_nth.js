// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 2n, new rt.PArray(1, Float64Array.of(0, 1, 2, 3, 4, 5, 6, 7)), 2n, new rt.PArray(1, Float64Array.of(0, 1, 2, 3, 4, 5, 6, 7))];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.int(l0);
      at = 5; l5 = K[1];
      at = 6; l6 = rt.add(l5, l4);
      at = 7; l7 = K[2];
      at = 8; l8 = rt.array_nth(l7, l6);
      at = 9; l3 = l8;
    } else {
      at = 10; l9 = rt.int(l0);
      at = 11; l10 = K[3];
      at = 12; l11 = rt.add(l10, l9);
      at = 13; l12 = K[4];
      at = 14; l13 = rt.array_nth(l12, l11);
      at = 15; l3 = l13;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
