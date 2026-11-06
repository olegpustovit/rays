// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [2n];
  let l0, l1, l2;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.mul(l1, l0);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l2;
}
