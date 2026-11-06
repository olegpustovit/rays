// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
export function frame(input, state, rt) {
  let l0;
  let at = 0;
  try {
    at = 0; l0 = input.t;
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l0;
}
