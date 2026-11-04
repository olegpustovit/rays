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
    r2 = r0 + r1;
    if (!Number.isFinite(r2)) return 1;
    let r3;
    r3 = 3n;
    let r4;
    r4 = 0n;
    let r5;
    let r6;
    r5 = r4;
    r6 = r2;
    for (; r5 < r3;) {
    let r7;
    r7 = 1;
    let r8;
    r8 = r6 + r7;
    if (!Number.isFinite(r8)) return 1;
    const next5_0 = r8;
    r6 = next5_0;
    r5++;
    }
    let r10;
    r10 = r6;
    let r11;
    r11 = (r10 > r1 ? 1 : 0);
    let r12;
    if (r11 !== 0) {
    r12 = 2;
    }
    let r13;
    if (r11 !== 0) {
    r13 = r10 * r12;
    if (!Number.isFinite(r13)) return 1;
    }
    let r14;
    if (r11 === 0) {
    r14 = uniforms[0][14];
    if (!Number.isFinite(r14)) return 1;
    }
    let r15;
    r15 = r11 !== 0 ? r13 : r14;
    if (!Number.isFinite(r15)) return 1;
    out0[o * 1 + 0] = r15;
  }
  return 0;
}
