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
    let r1h = 0, r1l = 0;
    r1l = split(convert("int", Math.trunc(r0))); r1h = RH;
    let r2h = 0, r2l = 0;
    r2h = 0; r2l = 4294967295;
    let r3h = 0, r3l = 0;
    r3h = r1h & r2h; r3l = (r1l & r2l) >>> 0; r3h = (r3h << 1) >> 1;
    let r4h = 0, r4l = 0;
    r4h = 0; r4l = 0;
    let r5h = 0, r5l = 0;
    r5h = 0; r5l = 3;
    let r6h = 0, r6l = 0;
    let r7h = 0, r7l = 0;
    let r8h = 0, r8l = 0;
    const length6 = big(r5h, r5l) - big(r4h, r4l);
    if (length6 > 4096n) fail('E_ITER_BOUND', `range ${big(r4h, r4l)}‥${big(r5h, r5l)} exceeds 4,096 iterations.`);
    r6h = r4h; r6l = r4l;
    r7h = r3h; r7l = r3l;
    r8h = r4h; r8l = r4l;
    for (; (r6h < r5h || (r6h === r5h && r6l < r5l));) {
    let r9h = 0, r9l = 0;
    r9h = 0; r9l = 747796405;
    let r10h = 0, r10l = 0;
    r10l = mul64(r7h, r7l, r9h, r9l); r10h = RH; r10h = (r10h << 1) >> 1;
    let r11h = 0, r11l = 0;
    r11h = 0; r11l = 2891336453;
    let r12h = 0, r12l = 0;
    r12l = add64(r10h, r10l, r11h, r11l); r12h = RH; r12h = (r12h << 1) >> 1;
    let r13h = 0, r13l = 0;
    r13h = 0; r13l = 4294967295;
    let r14h = 0, r14l = 0;
    r14h = r12h & r13h; r14l = (r12l & r13l) >>> 0; r14h = (r14h << 1) >> 1;
    let r15h = 0, r15l = 0;
    r15h = 0; r15l = 28;
    let r16h = 0, r16l = 0;
    r16l = shift64(r14h, r14l, (r15h * 4294967296 + r15l), true); r16h = RH; r16h = (r16h << 1) >> 1;
    let r17h = 0, r17l = 0;
    r17h = 0; r17l = 4;
    let r18h = 0, r18l = 0;
    r18l = add64(r16h, r16l, r17h, r17l); r18h = RH; r18h = (r18h << 1) >> 1;
    let r19h = 0, r19l = 0;
    r19l = shift64(r14h, r14l, (r18h * 4294967296 + r18l), true); r19h = RH; r19h = (r19h << 1) >> 1;
    let r20h = 0, r20l = 0;
    r20h = r19h ^ r14h; r20l = (r19l ^ r14l) >>> 0; r20h = (r20h << 1) >> 1;
    let r21h = 0, r21l = 0;
    r21h = 0; r21l = 277803737;
    let r22h = 0, r22l = 0;
    r22l = mul64(r20h, r20l, r21h, r21l); r22h = RH; r22h = (r22h << 1) >> 1;
    let r23h = 0, r23l = 0;
    r23h = r22h & r13h; r23l = (r22l & r13l) >>> 0; r23h = (r23h << 1) >> 1;
    let r24h = 0, r24l = 0;
    r24h = 0; r24l = 22;
    let r25h = 0, r25l = 0;
    r25l = shift64(r23h, r23l, (r24h * 4294967296 + r24l), true); r25h = RH; r25h = (r25h << 1) >> 1;
    let r26h = 0, r26l = 0;
    r26h = r25h ^ r23h; r26l = (r25l ^ r23l) >>> 0; r26h = (r26h << 1) >> 1;
    let r27h = 0, r27l = 0;
    r27h = 0; r27l = 65535;
    let r28h = 0, r28l = 0;
    r28h = r26h & r27h; r28l = (r26l & r27l) >>> 0; r28h = (r28h << 1) >> 1;
    const next6h0 = r14h, next6l0 = r14l;
    const next6h1 = r28h, next6l1 = r28l;
    r7h = next6h0; r7l = next6l0;
    r8h = next6h1; r8l = next6l1;
    r6l = add64(r6h, r6l, 0, 1); r6h = RH;
    }
    let r30h = 0, r30l = 0;
    r30h = r7h; r30l = r7l;
    let r31h = 0, r31l = 0;
    r31h = r8h; r31l = r8l;
    let r32;
    r32 = (r31h * 4294967296 + r31l);
    if (!Number.isFinite(r32)) return 1;
    let r33;
    r33 = uniforms[0][33];
    if (!Number.isFinite(r33)) return 1;
    let r34;
    r34 = r32 + r33;
    if (!Number.isFinite(r34)) return 1;
    if (!Number.isFinite(r34)) return 1;
    out0[o * 1 + 0] = r34;
  }
  return 0;
}
