// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [256n, 256n, 1n, 0.2];
  let l0, l1, l2, l3, l4, l5, l6, l7;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; l1 = K[1];
    at = 2; l2 = input.t;
    at = 3; l3 = rt.sin(l2);
    at = 4; l4 = K[2];
    at = 5; l5 = rt.add(l4, l3);
    at = 6; l6 = K[3];
    at = 7; l7 = rt.mul(l6, l5);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [l0, l1, l7];
}

export const regions = [];
