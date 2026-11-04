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
    r2 = (r0 > r1 ? 1 : 0);
    let r3;
    if (r2 !== 0) {
    r3 = uniforms[0][3];
    if (!Number.isFinite(r3)) return 1;
    }
    let r4h = 0, r4l = 0;
    if (r2 !== 0) {
    r4h = 0; r4l = 0;
    }
    let r5h = 0, r5l = 0;
    if (r2 !== 0) {
    r5h = 0; r5l = 2;
    }
    let r6h = 0, r6l = 0;
    let r7;
    if (r2 !== 0) {
    const length6 = big(r5h, r5l) - big(r4h, r4l);
    if (length6 > 4096n) fail('E_ITER_BOUND', `range ${big(r4h, r4l)}‥${big(r5h, r5l)} exceeds 4,096 iterations.`);
    r6h = r4h; r6l = r4l;
    r7 = r3;
    for (; (r6h < r5h || (r6h === r5h && r6l < r5l));) {
    let r8;
    if (r2 !== 0) {
    r8 = r7 + r0;
    if (!Number.isFinite(r8)) return 1;
    }
    const next6_0 = r8;
    r7 = next6_0;
    r6l = add64(r6h, r6l, 0, 1); r6h = RH;
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
    let r13h = 0, r13l = 0;
    if (r12 !== 0) {
    r13h = 1073741823; r13l = 4294967295;
    }
    let r14h = 0, r14l = 0;
    if (r12 !== 0) {
    r14h = 0; r14l = 0;
    }
    let r15h = 0, r15l = 0;
    let r16;
    if (r12 !== 0) {
    r15h = r14h; r15l = r14l;
    r16 = r11;
    for (; (r15h < r13h || (r15h === r13h && r15l < r13l));) {
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
    r15l = add64(r15h, r15l, 0, 1); r15h = RH;
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
