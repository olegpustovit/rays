function region(inputs, uniforms, outputs, start, stop) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  const wrap = x => BigInt.asIntN(64, x);
  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? BigInt(Math.trunc(x)) : fail('E_NONFINITE', name + ' produced a nonfinite value.');
  const divide = (a, b, mod) => b === 0n ? fail('E_RANGE', 'Integer division by zero.') : wrap(mod ? a % b : a / b);
  const shift = (a, b, right) => b < 0n || b > 63n ? fail('E_RANGE', 'Integer shift count must be within [0, 63].') : wrap(right ? BigInt.asUintN(64, a) >> b : a << b);
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
    r2 = 0n;
    let r3;
    r3 = 2n;
    let r4;
    let r5;
    const length4 = r3 - r2;
    if (length4 > 4096n) fail('E_ITER_BOUND', `range ${r2}‥${r3} exceeds 4,096 iterations.`);
    r4 = r2;
    r5 = r1;
    for (; r4 < r3;) {
    let r6;
    r6 = 1;
    let r7;
    r7 = r0 + r6;
    if (!Number.isFinite(r7)) return 1;
    let r8;
    r8 = r5 + r7;
    if (!Number.isFinite(r8)) return 1;
    const next4_0 = r8;
    r5 = next4_0;
    r4++;
    }
    let r10;
    r10 = r5;
    let r11;
    r11 = 1;
    let r12;
    r12 = r0 + r11;
    if (!Number.isFinite(r12)) return 1;
    let r13;
    r13 = r10 + r12;
    if (!Number.isFinite(r13)) return 1;
    if (!Number.isFinite(r13)) return 1;
    out0[o * 1 + 0] = r13;
  }
  return 0;
}
