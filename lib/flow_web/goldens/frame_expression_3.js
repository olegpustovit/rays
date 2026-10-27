// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
export function frame(input, state, rt) {
  K ??= [2n, 1n, 3n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.mul(l0, l1);
    at = 3; l3 = K[1];
    at = 4; l4 = rt.add(l0, l3);
    at = 5; l5 = K[2];
    at = 6; l6 = rt.min(l0, l5);
    at = 7; l7 = Float64Array.of(rt.float(l2), rt.float(l4), rt.float(l6));
    at = 8; l8 = field(rt, l7, "x");
    at = 9; l9 = field(rt, l7, "y");
    at = 10; l10 = rt.add(l8, l9);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l10;
}
