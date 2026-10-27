// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [0.2, 2n, 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = rt.sin(l0);
    at = 2; l2 = K[0];
    at = 3; l3 = rt.gt(l1, l2);
    at = 4; if (rt.truthy(l3)) {
      at = 5; l5 = K[1];
      at = 6; l6 = rt.pow(l0, l5);
      at = 7; l4 = l6;
    } else {
      at = 8; l7 = K[2];
      at = 9; l8 = rt.sub(l7, l0);
      at = 10; l4 = l8;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l4;
}
