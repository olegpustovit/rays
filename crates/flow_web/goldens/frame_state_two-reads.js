// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const cell = (state, slot, key, stamp) => {
  const cells = state[slot] ??= new Map();
  let c = cells.get(key);
  if (c === undefined) cells.set(key, c = { value: undefined, pending: undefined, stamp });
  else if (c.stamp !== stamp) {
    if (c.pending !== undefined) c.value = c.pending;
    c.pending = undefined; c.stamp = stamp;
  }
  return c;
};
export function frame(input, state, rt) {
  K ??= [0n, 1n];
  let l0, l1, l2, l3, l4, l5, l6;
  let at = 0;
  try {
    at = 1; { const c1 = cell(state, 0, "", input.frame);
      if (c1.value !== undefined) l1 = c1.value; else {
        at = 0; l0 = K[0];
        l1 = l0;
      } }
    at = 2; { const c2 = cell(state, 0, "", input.frame); l2 = c2.pending !== undefined; l3 = l2 ? c2.pending : null; }
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = l3;
    } else {
      at = 5; l5 = K[1];
      at = 6; l6 = rt.add(l1, l5);
      at = 7; cell(state, 0, "", input.frame).pending = l6;
      at = 8; l4 = l6;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l4;
}
