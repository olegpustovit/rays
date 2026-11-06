// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
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
  K ??= [0n, 1, 1n, 1.5];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21;
  let at = 0;
  try {
    at = 3; { const c3 = cell(state, 0, "", input.frame);
      if (c3.value !== undefined) l3 = c3.value; else {
        at = 0; l0 = K[0];
        at = 1; l1 = K[1];
        at = 2; l2 = new Map([["n", l0], ["m", l1]]);
        l3 = l2;
      } }
    at = 4; { const c4 = cell(state, 0, "", input.frame); l4 = c4.pending !== undefined; l5 = l4 ? c4.pending : null; }
    at = 5; if (rt.truthy(l4)) {
      at = 6; l6 = l5;
    } else {
      at = 7; l7 = field(rt, l3, "n");
      at = 8; l8 = K[2];
      at = 9; l9 = rt.add(l7, l8);
      at = 10; l10 = field(rt, l3, "n");
      at = 11; l11 = field(rt, l3, "m");
      at = 12; l12 = field(rt, l3, "m");
      at = 13; l13 = K[3];
      at = 14; l14 = rt.mul(l12, l13);
      at = 15; l15 = rt.coerce("float", l11);
      at = 16; l16 = rt.equal(l11, l15);
      at = 17; if (rt.truthy(l16)) {
        at = 18; l18 = rt.coerce("float", l14);
        at = 19; l17 = l18;
      } else {
        at = 20; l19 = rt.coerce("int", l14);
        at = 21; l17 = l19;
      }
      at = 22; l20 = rt.coerce("float", l17);
      at = 23; l21 = new Map([["n", l9], ["m", l20]]);
      at = 24; cell(state, 0, "", input.frame).pending = l21;
      at = 25; l6 = l21;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l6;
}
