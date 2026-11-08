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
    r1 = convert("int", Math.trunc(r0));
    let r2;
    r2 = uniforms[0][2];
    let r3;
    r3 = wrap(r1 + r2);
    let r4;
    r4 = 2n;
    let r5;
    r5 = 0n;
    let r6;
    let r7;
    r6 = r5;
    r7 = r3;
    for (; r6 < r4;) {
    let r8;
    r8 = 4611686018427387903n;
    let r9;
    r9 = wrap(r7 + r8);
    const next6_0 = r9;
    r7 = next6_0;
    r6++;
    }
    let r11;
    r11 = r7;
    let r12;
    r12 = 3n;
    let r13;
    let r14;
    r13 = r5;
    r14 = r11;
    for (; r13 < r12;) {
    let r15;
    r15 = uniforms[0][15];
    let r16;
    r16 = wrap(r14 ^ r15);
    const next13_0 = r16;
    r14 = next13_0;
    r13++;
    }
    let r18;
    r18 = r14;
    let r19;
    r19 = Number(r18);
    if (!Number.isFinite(r19)) return 1;
    if (!Number.isFinite(r19)) return 1;
    out0[o * 1 + 0] = r19;
  }
  return 0;
}
