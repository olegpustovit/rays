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
    r1 = convert("int", Math.trunc(r0));
    let r2;
    r2 = 2654435761n;
    let r3;
    r3 = wrap(r1 * r2);
    let r4;
    r4 = 5000000000000n;
    let r5;
    r5 = (r3 < r4 ? 1 : 0);
    if (!Number.isFinite(r5)) return 1;
    let r6;
    if (r5 !== 0) {
    r6 = convert("int", Math.trunc(r0));
    }
    let r7;
    if (r5 !== 0) {
    r7 = 3n;
    }
    let r8;
    if (r5 !== 0) {
    r8 = wrap(r6 * r7);
    }
    let r9;
    if (r5 === 0) {
    r9 = convert("int", Math.trunc(r0));
    }
    let r10;
    if (r5 === 0) {
    r10 = 0n;
    }
    let r11;
    if (r5 === 0) {
    r11 = wrap(r10 - r9);
    }
    let r12;
    r12 = r5 !== 0 ? r8 : r11;
    out0[o * 1 + 0] = r12;
  }
  return 0;
}
