// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
export function frame(input, state, rt) {
  K ??= [0.05, 12.5, 12.5, 0.02, 48, 24];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12;
  let at = 0;
  try {
    at = 0; l0 = BigInt(input.size[0]);
    at = 1; l1 = BigInt(input.size[1]);
    at = 2; l2 = K[0];
    at = 3; l3 = K[1];
    at = 4; l4 = rt.div(l0, l3);
    at = 5; l5 = rt.float(l0);
    at = 6; l6 = K[2];
    at = 7; l7 = rt.div(l1, l6);
    at = 8; l8 = input.t;
    at = 9; l9 = K[3];
    at = 10; l10 = rt.mul(l8, l9);
    at = 11; l11 = K[4];
    at = 12; l12 = K[5];
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [l0, l1, l2, l4, l5, l7, l10, l11, l12];
}

export const regions = [];
