// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, [1n, 2n, 3n], [1n, 2n, 3n]];
  let l0, l1, l2, l3, l4, l5, l6, l7;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.sequence_get(l4, l0);
      at = 6; l3 = l5;
    } else {
      at = 7; l6 = K[2];
      at = 8; l7 = rt.sequence_get(l6, l0);
      at = 9; l3 = l7;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
