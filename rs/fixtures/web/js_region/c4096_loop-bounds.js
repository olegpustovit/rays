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
    r2 = -2n;
    let r3;
    r3 = convert("int", Math.trunc(r0));
    let r4;
    r4 = 5n;
    let r5;
    r5 = divide(r3, r4, true);
    let r6;
    let r7;
    const length6 = r5 - r2;
    if (length6 > 4096n) fail('E_ITER_BOUND', `range ${r2}‥${r5} exceeds 4,096 iterations.`);
    r6 = r2;
    r7 = r1;
    for (; r6 < r5;) {
    let r8;
    r8 = 0.5;
    let r9;
    r9 = Number(r6);
    if (!Number.isFinite(r9)) return 1;
    let r10;
    r10 = r9 * r8;
    if (!Number.isFinite(r10)) return 1;
    let r11;
    r11 = r7 + r10;
    if (!Number.isFinite(r11)) return 1;
    const next6_0 = r11;
    r7 = next6_0;
    r6++;
    }
    let r13;
    r13 = r7;
    if (!Number.isFinite(r13)) return 1;
    out0[o * 1 + 0] = r13;
  }
  return 0;
}
