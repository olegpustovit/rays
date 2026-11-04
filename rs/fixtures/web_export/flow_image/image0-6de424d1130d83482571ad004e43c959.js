// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [256n, 256n, 0.03, 17n];
  let l0, l1, l2, l3;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; l1 = K[1];
    at = 2; l2 = K[2];
    at = 3; l3 = K[3];
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [l0, l1, l2, l3];
}

export const regions = [];
