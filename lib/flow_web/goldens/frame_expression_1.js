// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [6n, 0.7];
  let l0, l1, l2, l3, l4, l5;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.mul(l0, l1);
    at = 3; l3 = K[1];
    at = 4; l4 = rt.mod(l0, l3);
    at = 5; l5 = rt.add(l2, l4);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l5;
}
