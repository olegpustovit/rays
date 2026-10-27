// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [0, 0n, 1n, 3n, 0n, 0n, 1n, 2n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.add(l0, l4);
      at = 6; l6 = K[2];
      at = 7; l7 = rt.add(l0, l6);
      at = 8; l8 = rt.sequence_concat([l5], [l7]);
      at = 9; l9 = rt.sequence_length(l8);
      at = 10; l10 = K[3];
      at = 11; l11 = rt.lt(l9, l10);
      at = 12; if (rt.truthy(l11)) {
        at = 13; l13 = rt.fail("E_PATTERN", "Not enough elements to destructure.");
        at = 14; l12 = l13;
      } else {
        at = 15; l14 = K[4];
        at = 16; l12 = l14;
      }
      at = 17; l15 = K[5];
      at = 18; l16 = rt.sequence_get(l8, l15);
      at = 19; l17 = K[6];
      at = 20; l18 = rt.sequence_get(l8, l17);
      at = 21; l19 = K[7];
      at = 22; l20 = rt.sequence_get(l8, l19);
      at = 23; l21 = rt.add(l16, l18);
      at = 24; l3 = l21;
    } else {
      at = 25; l3 = l0;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
