function region(inputs, uniforms, outputs, start, stop) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  const wrap = x => BigInt.asIntN(63, x);
  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? BigInt(Math.trunc(x)) : fail('E_NONFINITE', name + ' produced a nonfinite value.');
  const divide = (a, b, mod) => b === 0n ? fail('E_RANGE', 'Integer division by zero.') : wrap(mod ? a % b : a / b);
  const shift = (a, b, right) => b < 0n || b > 62n ? fail('E_RANGE', 'Integer shift count must be within [0, 62].') : wrap(right ? BigInt.asUintN(63, a) >> b : a << b);
  const in0 = inputs[0];
  const out0 = outputs[0];
  for (let i = start; i < stop; i++) {
    const o = i;
    const j0 = i;
    let r0;
    r0 = Number(in0[j0 * 1 + 0]);
    if (!Number.isFinite(r0)) return 1;
    let r1;
    r1 = uniforms[0][1];
    if (!Number.isFinite(r1)) return 1;
    let r2;
    r2 = (r0 < r1 ? 1 : 0);
    let r3;
    if (r2 !== 0) {
    r3 = 1;
    }
    let r4;
    if (r2 !== 0) {
    r4 = r0 + r3;
    if (!Number.isFinite(r4)) return 1;
    }
    let r5;
    if (r2 === 0) {
    r5 = 1;
    }
    let r6;
    if (r2 === 0) {
    r6 = r0 - r5;
    if (!Number.isFinite(r6)) return 1;
    }
    let r7;
    r7 = r2 !== 0 ? r4 : r6;
    if (!Number.isFinite(r7)) return 1;
    out0[o * 1 + 0] = r7;
  }
  return 0;
}
