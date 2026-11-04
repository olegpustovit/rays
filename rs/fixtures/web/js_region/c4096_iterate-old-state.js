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
    r1 = 1;
    let r2;
    r2 = uniforms[0][2];
    if (!Number.isFinite(r2)) return 1;
    let r3;
    r3 = 4611686018427387903n;
    let r4;
    r4 = 0n;
    let r5;
    let r6;
    let r7;
    r5 = r4;
    r6 = r1;
    r7 = r2;
    for (; r5 < r3;) {
    let r8;
    r8 = 0;
    let r9;
    r9 = r7 + r0;
    if (!Number.isFinite(r9)) return 1;
    let r10;
    r10 = (r6 === 0 ? 1 : 0);
    let r11;
    r11 = (r10 === 0 ? 1 : 0);
    const stop5 = r11 !== 0;
    const next5_0 = r8;
    const next5_1 = r9;
    r6 = next5_0;
    r7 = next5_1;
    if (stop5) break;
    r5++;
    }
    let r13;
    r13 = r6;
    let r14;
    r14 = r7;
    if (!Number.isFinite(r14)) return 1;
    out0[o * 1 + 0] = r14;
  }
  return 0;
}
