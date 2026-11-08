function region(inputs, uniforms, outputs, start, stop, outputBase = 0) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  const wrap = x => BigInt.asIntN(64, x);
  const text = value => typeof value === 'string' ? value : fail('E_ARRAY_TYPE', 'Kernel input changed its packed element type.');
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
    r2 = 1;
    let r3;
    r3 = 0;
    let r4;
    r4 = "start";
    let r5;
    r5 = 1n;
    let r6;
    r6 = 2n;
    let r7;
    r7 = 0n;
    let r8;
    r8 = convert("int", Math.trunc(r0));
    let r9;
    r9 = 5n;
    let r10;
    r10 = divide(r8, r9, true);
    let r11;
    let r12;
    let r13;
    let r14;
    let r15;
    let r16;
    let r17;
    const length11 = r10 - r7;
    if (length11 > 4096n) fail('E_ITER_BOUND', `range ${r7}‥${r10} exceeds 4,096 iterations.`);
    r11 = r7;
    r12 = r5;
    r13 = r6;
    r14 = r1;
    r15 = r2;
    r16 = r3;
    r17 = r4;
    for (; r11 < r10;) {
    let r18;
    r18 = 1;
    let r19;
    r19 = r14 + r18;
    if (!Number.isFinite(r19)) return 1;
    let r20;
    r20 = (r16 === 0 ? 1 : 0);
    let r21;
    if (r16 !== 0) {
    r21 = "a";
    }
    let r22;
    if (r16 === 0) {
    r22 = "b";
    }
    let r23;
    r23 = "b";
    let r24;
    r24 = "a";
    let r25;
    r25 = (r16 !== 0 ? r24 : r23);
    let r26;
    r26 = (r20 === 0 ? 1 : 0);
    let r27;
    r27 = (r26 === 0 ? 1 : 0);
    const next11_0 = r13;
    const next11_1 = r12;
    const next11_2 = r15;
    const next11_3 = r19;
    const next11_4 = r27;
    const next11_5 = r25;
    r12 = next11_0;
    r13 = next11_1;
    r14 = next11_2;
    r15 = next11_3;
    r16 = next11_4;
    r17 = next11_5;
    r11++;
    }
    let r29;
    r29 = r12;
    let r30;
    r30 = r13;
    let r31;
    r31 = r14;
    let r32;
    r32 = r15;
    let r33;
    r33 = r16;
    let r34;
    r34 = r17;
    let r35;
    r35 = Number(r29);
    if (!Number.isFinite(r35)) return 1;
    let r36;
    r36 = Number(r30);
    if (!Number.isFinite(r36)) return 1;
    let r37;
    if (r33 !== 0) {
    r37 = 1;
    }
    let r38;
    if (r33 === 0) {
    r38 = 0;
    }
    let r39;
    r39 = r33 !== 0 ? r37 : r38;
    if (!Number.isFinite(r35)) return 1;
    out0[o * 4 + 0] = r35;
    if (!Number.isFinite(r36)) return 1;
    out0[o * 4 + 1] = r36;
    if (!Number.isFinite(r31)) return 1;
    out0[o * 4 + 2] = r31;
    if (!Number.isFinite(r39)) return 1;
    out0[o * 4 + 3] = r39;
  }
  return 0;
}
