// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [0, 2, 1];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11;
  let at = 0;
  const calls = new Int32Array(1);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = rt.mul(l0, l4);
      at = 6; calls[0] = 0;
      at = 7; l6 = BigInt(calls[0]++);
      at = 8; l7 = rt.add(l0, l5);
      at = 9; l8 = BigInt(calls[0]++);
      at = 10; l9 = K[2];
      at = 11; l10 = rt.add(l9, l5);
      at = 12; l11 = rt.add(l7, l10);
      at = 13; l3 = l11;
    } else {
      at = 14; l3 = l0;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
