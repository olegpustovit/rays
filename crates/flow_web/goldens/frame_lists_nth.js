// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [-1000, [1n, 2n, 3n], [1n, 2n, 3n]];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.int(l0);
      at = 5; l5 = K[1];
      at = 6; l6 = rt.sequence_get(l5, l4);
      at = 7; l3 = l6;
    } else {
      at = 8; l7 = rt.int(l0);
      at = 9; l8 = K[2];
      at = 10; l9 = rt.sequence_get(l8, l7);
      at = 11; l3 = l9;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
