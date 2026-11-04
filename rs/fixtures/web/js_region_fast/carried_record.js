function region(inputs, uniforms, outputs, start, stop, outputBase = 0) {
  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };
  let RH = 0;
  const P32 = 4294967296;
  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };
  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };
  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);
  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };
  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };
  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;
    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);
    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };
  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };
  const div64 = (ah, al, bh, bl, mod) => {
    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');
    const a = ah * P32 + al, b = bh * P32 + bl;
    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));
    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };
  const shift64 = (ah, al, b, right) => {
    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');
    if (b === 0) { RH = ah; return al; }
    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }
    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }
    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };
  const text = value => typeof value === 'string' ? value : fail('E_ARRAY_TYPE', 'Kernel input changed its packed element type.');
  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');
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
    let r5h = 0, r5l = 0;
    r5h = 0; r5l = 1;
    let r6h = 0, r6l = 0;
    r6h = 0; r6l = 2;
    let r7h = 0, r7l = 0;
    r7h = 0; r7l = 0;
    let r8h = 0, r8l = 0;
    r8l = split(convert("int", Math.trunc(r0))); r8h = RH;
    let r9h = 0, r9l = 0;
    r9h = 0; r9l = 5;
    let r10h = 0, r10l = 0;
    r10l = div64(r8h, r8l, r9h, r9l, true); r10h = RH; r10h = (r10h << 1) >> 1;
    let r11h = 0, r11l = 0;
    let r12h = 0, r12l = 0;
    let r13h = 0, r13l = 0;
    let r14;
    let r15;
    let r16;
    let r17;
    const length11 = big(r10h, r10l) - big(r7h, r7l);
    if (length11 > 4096n) fail('E_ITER_BOUND', `range ${big(r7h, r7l)}‥${big(r10h, r10l)} exceeds 4,096 iterations.`);
    r11h = r7h; r11l = r7l;
    r12h = r5h; r12l = r5l;
    r13h = r6h; r13l = r6l;
    r14 = r1;
    r15 = r2;
    r16 = r3;
    r17 = r4;
    for (; (r11h < r10h || (r11h === r10h && r11l < r10l));) {
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
    const next11h0 = r13h, next11l0 = r13l;
    const next11h1 = r12h, next11l1 = r12l;
    const next11_2 = r15;
    const next11_3 = r19;
    const next11_4 = r27;
    const next11_5 = r25;
    r12h = next11h0; r12l = next11l0;
    r13h = next11h1; r13l = next11l1;
    r14 = next11_2;
    r15 = next11_3;
    r16 = next11_4;
    r17 = next11_5;
    r11l = add64(r11h, r11l, 0, 1); r11h = RH;
    }
    let r29h = 0, r29l = 0;
    r29h = r12h; r29l = r12l;
    let r30h = 0, r30l = 0;
    r30h = r13h; r30l = r13l;
    let r31;
    r31 = r14;
    let r32;
    r32 = r15;
    let r33;
    r33 = r16;
    let r34;
    r34 = r17;
    let r35;
    r35 = (r29h * 4294967296 + r29l);
    if (!Number.isFinite(r35)) return 1;
    let r36;
    r36 = (r30h * 4294967296 + r30l);
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
