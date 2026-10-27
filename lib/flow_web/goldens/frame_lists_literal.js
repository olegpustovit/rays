// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, -1e3, 1n, 2, [], -1e3, 1n, 2, []];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.gt(l0, l4);
      at = 6; if (rt.truthy(l5)) {
        at = 7; l7 = K[2];
        at = 8; l8 = K[3];
        at = 9; l9 = rt.sequence_concat([l0], [l7], [l8]);
        at = 10; l6 = l9;
      } else {
        at = 11; l10 = K[4];
        at = 12; l6 = l10;
      }
      at = 13; l3 = l6;
    } else {
      at = 14; l11 = K[5];
      at = 15; l12 = rt.gt(l0, l11);
      at = 16; if (rt.truthy(l12)) {
        at = 17; l14 = K[6];
        at = 18; l15 = K[7];
        at = 19; l16 = rt.sequence_concat([l0], [l14], [l15]);
        at = 20; l13 = l16;
      } else {
        at = 21; l17 = K[8];
        at = 22; l13 = l17;
      }
      at = 23; l3 = l13;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
