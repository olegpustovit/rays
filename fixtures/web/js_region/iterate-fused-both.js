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
    r2 = (r0 > r1 ? 1 : 0);
    let r3;
    if (r2 !== 0) {
    r3 = uniforms[0][3];
    if (!Number.isFinite(r3)) return 1;
    }
    let r4;
    if (r2 !== 0) {
    r4 = 0n;
    }
    let r5;
    if (r2 !== 0) {
    r5 = 2n;
    }
    let r6;
    let r7;
    if (r2 !== 0) {
    const length6 = r5 - r4;
    if (length6 > 4096n) fail('E_ITER_BOUND', `range ${r4}‥${r5} exceeds 4,096 iterations.`);
    r6 = r4;
    r7 = r3;
    for (; r6 < r5;) {
    let r8;
    if (r2 !== 0) {
    r8 = r7 + r0;
    if (!Number.isFinite(r8)) return 1;
    }
    const next6_0 = r8;
    r7 = next6_0;
    r6++;
    }
    }
    let r10;
    if (r2 !== 0) {
    r10 = r7;
    }
    let r11;
    r11 = r2 !== 0 ? r10 : r0;
    let r12;
    r12 = (r11 > r1 ? 1 : 0);
    let r13;
    if (r12 !== 0) {
    r13 = 4611686018427387903n;
    }
    let r14;
    if (r12 !== 0) {
    r14 = 0n;
    }
    let r15;
    let r16;
    if (r12 !== 0) {
    r15 = r14;
    r16 = r11;
    for (; r15 < r13;) {
    let r17;
    if (r12 !== 0) {
    r17 = 1;
    }
    let r18;
    if (r12 !== 0) {
    r18 = r16 + r17;
    if (!Number.isFinite(r18)) return 1;
    }
    let r19;
    if (r12 !== 0) {
    r19 = 2;
    }
    let r20;
    if (r12 !== 0) {
    r20 = r11 + r19;
    if (!Number.isFinite(r20)) return 1;
    }
    let r21;
    if (r12 !== 0) {
    r21 = (r18 >= r20 ? 1 : 0);
    }
    let r22;
    if (r12 !== 0) {
    r22 = (r21 === 0 ? 1 : 0);
    }
    let r23;
    if (r12 !== 0) {
    r23 = (r22 === 0 ? 1 : 0);
    }
    const stop15 = r23 !== 0;
    const next15_0 = r18;
    r16 = next15_0;
    if (stop15) break;
    r15++;
    }
    }
    let r25;
    if (r12 !== 0) {
    r25 = r16;
    }
    let r26;
    if (r12 === 0) {
    r26 = uniforms[0][26];
    if (!Number.isFinite(r26)) return 1;
    }
    let r27;
    r27 = r12 !== 0 ? r25 : r26;
    if (!Number.isFinite(r27)) return 1;
    out0[o * 1 + 0] = r27;
  }
  return 0;
}
