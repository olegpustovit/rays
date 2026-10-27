// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0n, 3n, [0n, 1n, 2n], 0n, 0n, 3n, [0n, 1n, 2n], 0n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = rt.float(l0);
      at = 5; l5 = rt.coerce("int", l4);
      at = 6; l6 = K[1];
      at = 7; l7 = rt.max(l6, l5);
      at = 8; l8 = K[2];
      at = 9; l9 = rt.min(l7, l8);
      at = 10; l10 = K[3];
      at = 11; l11 = K[4];
      at = 12; l12 = rt.sequence_slice(l10, l11, l9);
      at = 13; l3 = l12;
    } else {
      at = 14; l13 = rt.float(l0);
      at = 15; l14 = rt.coerce("int", l13);
      at = 16; l15 = K[5];
      at = 17; l16 = rt.max(l15, l14);
      at = 18; l17 = K[6];
      at = 19; l18 = rt.min(l16, l17);
      at = 20; l19 = K[7];
      at = 21; l20 = K[8];
      at = 22; l21 = rt.sequence_slice(l19, l20, l18);
      at = 23; l3 = l21;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
