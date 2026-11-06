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
  K ??= [0n, 0n, "a", 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25;
  let at = 0;
  const calls = new Int32Array(1);
  try {
    at = 1; { const c1 = cell(state, 0, "", input.frame);
      if (c1.value !== undefined) l1 = c1.value; else {
        at = 0; l0 = K[0];
        l1 = l0;
      } }
    at = 2; { const c2 = cell(state, 0, "", input.frame); l2 = c2.pending !== undefined; l3 = l2 ? c2.pending : null; }
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = l3;
    } else {
      at = 5; calls[0] = 0;
      at = 6; l5 = K[1];
      at = 7; l6 = l5;
      at = 8; l7 = rt.frameValue(input);
      at = 9; l8 = field(rt, l7, "events");
      at = 10; {
        const s10 = l8, c10 = Array.isArray(s10) ? s10.length : Number(rt.array_count(s10));
        if (c10 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i10 = 0; i10 < c10; i10++) {
          l9 = BigInt(i10); l10 = Array.isArray(s10) ? s10[i10] : rt.array_nth(s10, i10);
          at = 11; l11 = BigInt(calls[0]++);
          at = 12; l12 = field(rt, l10, "key");
          at = 13; l13 = K[2];
          at = 14; l14 = rt.equal(l12, l13);
          at = 15; if (rt.truthy(l14)) {
            at = 16; l16 = K[3];
            at = 17; l17 = rt.add(l6, l16);
            at = 18; l15 = l17;
          } else {
            at = 19; l15 = l6;
          }
          at = 20; l18 = rt.coerce("float", l15);
          at = 21; l19 = rt.equal(l15, l18);
          at = 22; if (rt.truthy(l19)) {
            at = 23; l20 = l15;
          } else {
            at = 24; l21 = rt.coerce("float", l6);
            at = 25; l22 = rt.equal(l6, l21);
            at = 26; if (rt.truthy(l22)) {
              at = 27; l24 = rt.coerce("float", l15);
              at = 28; l23 = l24;
            } else {
              at = 29; l23 = l15;
            }
            at = 30; l20 = l23;
          }
          at = 31; l6 = l20;
        }
      }
      at = 32; l25 = rt.add(l1, l6);
      at = 33; cell(state, 0, "", input.frame).pending = l25;
      at = 34; l4 = l25;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l4;
}
