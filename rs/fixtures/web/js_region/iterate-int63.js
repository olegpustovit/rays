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
    r2 = 0;
    let r3;
    r3 = 1n;
    let r4;
    r4 = 0n;
    let r5;
    let r6;
    let r7;
    let r8;
    let r9;
    r5 = r4;
    r6 = r1;
    r7 = r2;
    r8 = r2;
    r9 = r2;
    for (; r5 < r3;) {
    let r10;
    r10 = convert("int", Math.trunc(r0));
    let r11;
    r11 = uniforms[0][11];
    let r12;
    r12 = wrap(r10 + r11);
    let r13;
    r13 = 4n;
    let r14;
    r14 = divide(r12, r13, true);
    let r15;
    r15 = 1n;
    let r16;
    r16 = wrap(r14 + r15);
    let r17;
    r17 = 2n;
    let r18;
    r18 = wrap(r16 * r17);
    let r19;
    r19 = divide(r12, r15, true);
    let r20;
    r20 = 62n;
    let r21;
    r21 = wrap(r20 + r19);
    let r22;
    r22 = shift(r15, r21, false);
    let r23;
    r23 = 4611686018427387903n;
    let r24;
    r24 = wrap(r23 + r16);
    let r25;
    r25 = 0n;
    let r26;
    r26 = (r24 < r25 ? 1 : 0);
    if (!Number.isFinite(r26)) return 1;
    let r27;
    if (r26 !== 0) {
    r27 = 1;
    }
    let r28;
    if (r26 === 0) {
    r28 = 0;
    }
    let r29;
    r29 = r26 !== 0 ? r27 : r28;
    let r30;
    r30 = wrap(r23 * r18);
    let r31;
    r31 = wrap(r25 - r18);
    let r32;
    r32 = (r30 === r31 ? 1 : 0);
    if (!Number.isFinite(r32)) return 1;
    let r33;
    if (r32 !== 0) {
    r33 = 1;
    }
    let r34;
    if (r32 === 0) {
    r34 = 0;
    }
    let r35;
    r35 = r32 !== 0 ? r33 : r34;
    let r36;
    r36 = wrap(r25 - r16);
    let r37;
    r37 = shift(r36, r20, true);
    let r38;
    r38 = (r37 === r15 ? 1 : 0);
    if (!Number.isFinite(r38)) return 1;
    let r39;
    if (r38 !== 0) {
    r39 = 1;
    }
    let r40;
    if (r38 === 0) {
    r40 = 0;
    }
    let r41;
    r41 = r38 !== 0 ? r39 : r40;
    let r42;
    r42 = r22 < 0n ? wrap(-r22) : r22;
    let r43;
    r43 = (r42 < r25 ? 1 : 0);
    if (!Number.isFinite(r43)) return 1;
    let r44;
    r44 = -1n;
    let r45;
    r45 = divide(r22, r44, false);
    let r46;
    r46 = (r45 === r22 ? 1 : 0);
    if (!Number.isFinite(r46)) return 1;
    let r47;
    r47 = (r43 !== 0 && r46 !== 0 ? 1 : 0);
    let r48;
    if (r47 !== 0) {
    r48 = 1;
    }
    let r49;
    if (r47 === 0) {
    r49 = 0;
    }
    let r50;
    r50 = r47 !== 0 ? r48 : r49;
    const next5_0 = r29;
    const next5_1 = r35;
    const next5_2 = r41;
    const next5_3 = r50;
    r6 = next5_0;
    r7 = next5_1;
    r8 = next5_2;
    r9 = next5_3;
    r5++;
    }
    let r52;
    r52 = r6;
    let r53;
    r53 = r7;
    let r54;
    r54 = r8;
    let r55;
    r55 = r9;
    if (!Number.isFinite(r52)) return 1;
    out0[o * 4 + 0] = r52;
    if (!Number.isFinite(r53)) return 1;
    out0[o * 4 + 1] = r53;
    if (!Number.isFinite(r54)) return 1;
    out0[o * 4 + 2] = r54;
    if (!Number.isFinite(r55)) return 1;
    out0[o * 4 + 3] = r55;
  }
  return 0;
}
