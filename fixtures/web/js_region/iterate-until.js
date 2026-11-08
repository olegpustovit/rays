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
    r2 = r0 + r1;
    if (!Number.isFinite(r2)) return 1;
    let r3;
    r3 = 4611686018427387903n;
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
    let r9;
    r9 = uniforms[0][9];
    if (!Number.isFinite(r9)) return 1;
    let r10;
    r10 = r0 + r9;
    if (!Number.isFinite(r10)) return 1;
    let r11;
    r11 = 3;
    let r12;
    r12 = r10 + r11;
    if (!Number.isFinite(r12)) return 1;
    let r13;
    r13 = (r8 >= r12 ? 1 : 0);
    let r14;
    r14 = (r13 === 0 ? 1 : 0);
    let r15;
    r15 = (r14 === 0 ? 1 : 0);
    const stop5 = r15 !== 0;
    const next5_0 = r8;
    r6 = next5_0;
    if (stop5) break;
    r5++;
    }
    let r17;
    r17 = r6;
    if (!Number.isFinite(r17)) return 1;
    out0[o * 1 + 0] = r17;
  }
  return 0;
}
