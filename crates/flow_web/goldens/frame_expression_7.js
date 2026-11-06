// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [0.5];
  let l0, l1, l2, l3, l4, l5;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = rt.add(l0, l0);
    at = 2; l2 = rt.add(l1, l1);
    at = 3; l3 = rt.add(l2, l2);
    at = 4; l4 = K[0];
    at = 5; l5 = rt.mul(l3, l4);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l5;
}
