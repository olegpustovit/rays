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
    r4 = 37n;
    let r5;
    r5 = shift(r3, r4, true);
    let r6;
    r6 = 40n;
    let r7;
    r7 = shift(r1, r6, false);
    let r8;
    r8 = 38n;
    let r9;
    r9 = shift(r7, r8, true);
    let r10;
    r10 = wrap(r5 + r9);
    out0[o * 1 + 0] = r10;
  }
  return 0;
}
