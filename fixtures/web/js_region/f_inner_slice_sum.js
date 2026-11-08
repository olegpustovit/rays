function region(inputs, uniforms, outputs, start, stop) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  const wrap = x => BigInt.asIntN(64, x);
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
  const shift = (a, b, right) => b < 0n || b > 63n ? fail('E_RANGE', 'Integer shift count must be within [0, 63].') : wrap(right ? BigInt.asUintN(64, a) >> b : a << b);
  const in0 = inputs[0];
  const out0 = outputs[0];
  for (let i = start; i < stop; i++) {
    const o = i;
    const j0 = i;
    let r0;
    r0 = in0[j0];
    let r1;
    r1 = table(0, true, 1);
    let r2;
    r2 = 61n;
    let r3;
    r3 = divide(r0, r2, true);
    let r4;
    r4 = uniforms[1][0];
    let r5;
    r5 = 0n;
    let r6;
    r6 = (r3 >= r5 ? 1 : 0);
    if (!Number.isFinite(r6)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r7;
    r7 = 18014398509481983n;
    let r8;
    r8 = (r3 <= r7 ? 1 : 0);
    if (!Number.isFinite(r8)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r9;
    r9 = (r6 !== 0 && r8 !== 0 ? 1 : 0);
    let r10;
    if (r9 === 0) {
    r10 = fail("E_ARRAY_RANGE", "array/slice needs a non-negative whole length.");
    }
    let r11;
    r11 = r9 !== 0 ? r3 : r10;
    let r12;
    r12 = wrap(r4 - r11);
    let r13;
    r13 = 3n;
    let r14;
    r14 = (r13 <= r12 ? 1 : 0);
    if (!Number.isFinite(r14)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r15;
    r15 = (r11 <= r4 ? 1 : 0);
    if (!Number.isFinite(r15)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r16;
    r16 = (r15 !== 0 && r14 !== 0 ? 1 : 0);
    let r17;
    if (r16 === 0) {
    r17 = fail("E_ARRAY_RANGE", "An array slice must be within its source.");
    }
    let r18;
    r18 = r16 !== 0 ? r5 : r17;
    let r19;
    r19 = wrap(r11 + r5);
    let r20;
    if (r16 !== 0) {
    r20 = r1[row(r1, r19, 1)];
    }
    let r21;
    r21 = 1n;
    let r22;
    r22 = wrap(r11 + r21);
    let r23;
    if (r16 !== 0) {
    r23 = r1[row(r1, r22, 1)];
    }
    let r24;
    r24 = 2n;
    let r25;
    r25 = wrap(r11 + r24);
    let r26;
    if (r16 !== 0) {
    r26 = r1[row(r1, r25, 1)];
    }
    let r27;
    r27 = wrap(r18 + r20);
    let r28;
    r28 = wrap(r27 + r23);
    let r29;
    r29 = wrap(r28 + r26);
    let r30;
    r30 = uniforms[2][30];
    let r31;
    r31 = Number(r29);
    if (!Number.isFinite(r31)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r32;
    r32 = Number(r30);
    if (!Number.isFinite(r32)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r33;
    r33 = r31 + r32;
    if (!Number.isFinite(r33)) fail('E_NONFINITE', 'packed produced a nonfinite value.');
    let r34;
    r34 = convert("+", Math.floor(r33 + 0.5));
    out0[o * 1 + 0] = r34;
  }
  return 0;
}
