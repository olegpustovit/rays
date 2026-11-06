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
    r2 = 1;
    let r3;
    r3 = 0;
    let r4;
    r4 = 1n;
    let r5;
    r5 = 2n;
    let r6;
    r6 = 0n;
    let r7;
    r7 = convert("int", Math.trunc(r0));
    let r8;
    r8 = 5n;
    let r9;
    r9 = divide(r7, r8, true);
    let r10;
    let r11;
    let r12;
    let r13;
    let r14;
    let r15;
    const length10 = r9 - r6;
    if (length10 > 4096n) fail('E_ITER_BOUND', `range ${r6}‥${r9} exceeds 4,096 iterations.`);
    r10 = r6;
    r11 = r4;
    r12 = r5;
    r13 = r1;
    r14 = r2;
    r15 = r3;
    for (; r10 < r9;) {
    let r16;
    r16 = 1;
    let r17;
    r17 = r13 + r16;
    if (!Number.isFinite(r17)) return 1;
    let r18;
    r18 = (r15 === 0 ? 1 : 0);
    let r19;
    r19 = (r18 === 0 ? 1 : 0);
    let r20;
    r20 = (r19 === 0 ? 1 : 0);
    const next10_0 = r12;
    const next10_1 = r11;
    const next10_2 = r14;
    const next10_3 = r17;
    const next10_4 = r20;
    r11 = next10_0;
    r12 = next10_1;
    r13 = next10_2;
    r14 = next10_3;
    r15 = next10_4;
    r10++;
    }
    let r22;
    r22 = r11;
    let r23;
    r23 = r12;
    let r24;
    r24 = r13;
    let r25;
    r25 = r14;
    let r26;
    r26 = r15;
    let r27;
    r27 = Number(r22);
    if (!Number.isFinite(r27)) return 1;
    let r28;
    r28 = Number(r23);
    if (!Number.isFinite(r28)) return 1;
    let r29;
    if (r26 !== 0) {
    r29 = 1;
    }
    let r30;
    if (r26 === 0) {
    r30 = 0;
    }
    let r31;
    r31 = r26 !== 0 ? r29 : r30;
    if (!Number.isFinite(r27)) return 1;
    out0[o * 4 + 0] = r27;
    if (!Number.isFinite(r28)) return 1;
    out0[o * 4 + 1] = r28;
    if (!Number.isFinite(r24)) return 1;
    out0[o * 4 + 2] = r24;
    if (!Number.isFinite(r31)) return 1;
    out0[o * 4 + 3] = r31;
  }
  return 0;
}
