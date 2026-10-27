// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [0, 0, 1, 2, 0.5];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.lt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.sub(l4, l0);
      at = 6; l3 = l5;
    } else {
      at = 7; l6 = K[2];
      at = 8; l7 = rt.gt(l0, l6);
      at = 9; if (rt.truthy(l7)) {
        at = 10; l9 = K[3];
        at = 11; l10 = rt.mul(l0, l9);
        at = 12; l8 = l10;
      } else {
        at = 13; l11 = K[4];
        at = 14; l12 = rt.add(l0, l11);
        at = 15; l8 = l12;
      }
      at = 16; l3 = l8;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
