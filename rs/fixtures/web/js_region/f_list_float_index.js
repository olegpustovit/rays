function region(inputs, uniforms, outputs, start, stop) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  const wrap = x => BigInt.asIntN(63, x);
  const tables = new Map();
  const table = (slot, integer, width) => { if (tables.has(slot)) return tables.get(slot); let data = uniforms[slot];
    const text = integer === 'text';
    if (text ? !Array.isArray(data) : !(data instanceof (integer ? BigInt64Array : Float64Array))) fail('E_ARRAY_TYPE', 'Kernel lookup table changed its packed element type.');
    if (text && !data.every(x => typeof x === 'string')) fail('E_ARRAY_TYPE', 'Text arrays contain text values.');
    if (data.length % width !== 0) fail('E_ARRAY_TYPE', `Packed vec${width} storage has ${['','','two','three','four'][width]} coordinates per element.`);
    if (!integer) for (const x of data) if (!Number.isFinite(x)) fail('E_NONFINITE', 'array input produced a nonfinite value.');
    tables.set(slot, data); return data; };
  const row = (array, index, width) => { if (index < 0n || index > 18014398509481983n) fail('E_ARRAY_RANGE', 'array/nth needs a non-negative whole length.');
    if (index >= BigInt(array.length / width)) fail('E_ARRAY_RANGE', 'Array index is outside its length.'); return Number(index); };
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
    if (!Number.isFinite(r0)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r1;
    r1 = table(0, false, 1);
    let r2;
    r2 = convert("int", Math.trunc(r0));
    let r3;
    r3 = 3n;
    let r4;
    r4 = divide(r2, r3, true);
    let r5;
    r5 = Number(r4);
    if (!Number.isFinite(r5)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r6;
    r6 = 0;
    let r7;
    r7 = (r5 >= r6 ? 1 : 0);
    let r8;
    r8 = 3;
    let r9;
    r9 = (r5 < r8 ? 1 : 0);
    let r10;
    r10 = 1;
    let r11;
    r11 = r10 === 0 ? 0 : ((r5 % r10) + r10) % r10;
    if (!Number.isFinite(r11)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r12;
    r12 = (r11 === r6 ? 1 : 0);
    let r13;
    r13 = (r7 !== 0 && r9 !== 0 ? 1 : 0);
    let r14;
    r14 = (r13 !== 0 && r12 !== 0 ? 1 : 0);
    let r15;
    if (r14 !== 0) {
    r15 = convert("int", Math.trunc(r5));
    }
    let r16;
    if (r14 === 0) {
    r16 = fail("E_LIST_RANGE", "nth index is out of range or is not a whole number.");
    }
    let r17;
    r17 = r14 !== 0 ? r15 : r16;
    let r18;
    if (r14 !== 0) {
    r18 = r1[row(r1, r17, 1) * 1 + 0];
    if (!Number.isFinite(r18)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    }
    let r19;
    r19 = uniforms[1][19];
    if (!Number.isFinite(r19)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r20;
    r20 = r18 + r19;
    if (!Number.isFinite(r20)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    if (!Number.isFinite(r20)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    out0[o * 1 + 0] = r20;
  }
  return 0;
}
