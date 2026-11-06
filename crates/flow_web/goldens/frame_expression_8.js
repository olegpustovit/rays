// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
export function frame(input, state, rt) {
  K ??= [0n, 1n, 0.25, 1, 1, 2, 3];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = K[1];
    at = 3; l3 = Float64Array.of(rt.float(l0), rt.float(l1), rt.float(l2));
    at = 4; l4 = K[2];
    at = 5; l5 = rt.float(l4);
    at = 6; l6 = K[3];
    at = 7; l7 = rt.sub(l6, l5);
    at = 8; l8 = field(rt, l3, "x");
    at = 9; l9 = K[4];
    at = 10; l10 = rt.mul(l9, l7);
    at = 11; l11 = rt.mul(l8, l5);
    at = 12; l12 = rt.add(l10, l11);
    at = 13; l13 = field(rt, l3, "y");
    at = 14; l14 = K[5];
    at = 15; l15 = rt.mul(l14, l7);
    at = 16; l16 = rt.mul(l13, l5);
    at = 17; l17 = rt.add(l15, l16);
    at = 18; l18 = field(rt, l3, "z");
    at = 19; l19 = K[6];
    at = 20; l20 = rt.mul(l19, l7);
    at = 21; l21 = rt.mul(l18, l5);
    at = 22; l22 = rt.add(l20, l21);
    at = 23; l23 = Float64Array.of(rt.float(l12), rt.float(l17), rt.float(l22));
    at = 24; l24 = field(rt, l23, "z");
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l24;
}
