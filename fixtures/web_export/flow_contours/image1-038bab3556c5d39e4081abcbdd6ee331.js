// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [12.5, 12.5, 0.02];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10;
  let at = 0;
  try {
    at = 0; l0 = BigInt(input.size[0]);
    at = 1; l1 = rt.float(l0);
    at = 2; l2 = BigInt(input.size[1]);
    at = 3; l3 = rt.float(l2);
    at = 4; l4 = K[0];
    at = 5; l5 = rt.div(l0, l4);
    at = 6; l6 = K[1];
    at = 7; l7 = rt.div(l2, l6);
    at = 8; l8 = input.t;
    at = 9; l9 = K[2];
    at = 10; l10 = rt.mul(l8, l9);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [l1, l3, l5, l1, l7, l10];
}

export const regions = [];
