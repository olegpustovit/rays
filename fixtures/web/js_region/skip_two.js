function region(inputs, uniforms, outputs, start, stop) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  const wrap = x => BigInt.asIntN(64, x);
  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? BigInt(Math.trunc(x)) : fail('E_NONFINITE', name + ' produced a nonfinite value.');
  const divide = (a, b, mod) => b === 0n ? fail('E_RANGE', 'Integer division by zero.') : wrap(mod ? a % b : a / b);
  const shift = (a, b, right) => b < 0n || b > 63n ? fail('E_RANGE', 'Integer shift count must be within [0, 63].') : wrap(right ? BigInt.asUintN(64, a) >> b : a << b);
  const in0 = inputs[0];
  const out0 = outputs[0];
  const skip = [0, 700];
  let s = 0;
  for (let hi = skip.length; s < hi;) {
    const mid = (s + hi) >> 1;
    if (skip[mid] < start) s = mid + 1; else hi = mid;
  }
  for (let i = start; i < stop; i++) {
    if (s < skip.length && skip[s] === i) { s++; continue; }
    const o = i - s;
    const j0 = i;
    let r0;
    r0 = Number(in0[j0 * 1 + 0]);
    if (!Number.isFinite(r0)) return 1;
    let r1;
    r1 = uniforms[0][1];
    if (!Number.isFinite(r1)) return 1;
    let r2;
    r2 = r0 * r1;
    if (!Number.isFinite(r2)) return 1;
    if (!Number.isFinite(r2)) return 1;
    out0[o * 1 + 0] = r2;
  }
  return 0;
}
