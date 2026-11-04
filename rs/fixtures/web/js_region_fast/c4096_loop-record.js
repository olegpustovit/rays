function region(inputs, uniforms, outputs, start, stop) {
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
    let r4h = 0, r4l = 0;
    r4h = 0; r4l = 1;
    let r5h = 0, r5l = 0;
    r5h = 0; r5l = 2;
    let r6h = 0, r6l = 0;
    r6h = 0; r6l = 0;
    let r7h = 0, r7l = 0;
    r7l = split(convert("int", Math.trunc(r0))); r7h = RH;
    let r8h = 0, r8l = 0;
    r8h = 0; r8l = 5;
    let r9h = 0, r9l = 0;
    r9l = div64(r7h, r7l, r8h, r8l, true); r9h = RH; r9h = (r9h << 1) >> 1;
    let r10h = 0, r10l = 0;
    let r11h = 0, r11l = 0;
    let r12h = 0, r12l = 0;
    let r13;
    let r14;
    let r15;
    const length10 = big(r9h, r9l) - big(r6h, r6l);
    if (length10 > 4096n) fail('E_ITER_BOUND', `range ${big(r6h, r6l)}‥${big(r9h, r9l)} exceeds 4,096 iterations.`);
    r10h = r6h; r10l = r6l;
    r11h = r4h; r11l = r4l;
    r12h = r5h; r12l = r5l;
    r13 = r1;
    r14 = r2;
    r15 = r3;
    for (; (r10h < r9h || (r10h === r9h && r10l < r9l));) {
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
    const next10h0 = r12h, next10l0 = r12l;
    const next10h1 = r11h, next10l1 = r11l;
    const next10_2 = r14;
    const next10_3 = r17;
    const next10_4 = r20;
    r11h = next10h0; r11l = next10l0;
    r12h = next10h1; r12l = next10l1;
    r13 = next10_2;
    r14 = next10_3;
    r15 = next10_4;
    r10l = add64(r10h, r10l, 0, 1); r10h = RH;
    }
    let r22h = 0, r22l = 0;
    r22h = r11h; r22l = r11l;
    let r23h = 0, r23l = 0;
    r23h = r12h; r23l = r12l;
    let r24;
    r24 = r13;
    let r25;
    r25 = r14;
    let r26;
    r26 = r15;
    let r27;
    r27 = (r22h * 4294967296 + r22l);
    if (!Number.isFinite(r27)) return 1;
    let r28;
    r28 = (r23h * 4294967296 + r23l);
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
