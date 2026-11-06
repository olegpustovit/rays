// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [360n, 240n];
  let l0, l1;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; l1 = K[1];
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [l0, l1];
}

export const regions = [];
