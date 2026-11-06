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
    let r2h = 0, r2l = 0;
    r2h = -1; r2l = 4294967294;
    let r3h = 0, r3l = 0;
    r3l = split(convert("int", Math.trunc(r0))); r3h = RH;
    let r4h = 0, r4l = 0;
    r4h = 0; r4l = 5;
    let r5h = 0, r5l = 0;
    r5l = div64(r3h, r3l, r4h, r4l, true); r5h = RH; r5h = (r5h << 1) >> 1;
    let r6h = 0, r6l = 0;
    let r7;
    const length6 = big(r5h, r5l) - big(r2h, r2l);
    if (length6 > 4096n) fail('E_ITER_BOUND', `range ${big(r2h, r2l)}‥${big(r5h, r5l)} exceeds 4,096 iterations.`);
    r6h = r2h; r6l = r2l;
    r7 = r1;
    for (; (r6h < r5h || (r6h === r5h && r6l < r5l));) {
    let r8;
    r8 = 0.5;
    let r9;
    r9 = (r6h * 4294967296 + r6l);
    if (!Number.isFinite(r9)) return 1;
    let r10;
    r10 = r9 * r8;
    if (!Number.isFinite(r10)) return 1;
    let r11;
    r11 = r7 + r10;
    if (!Number.isFinite(r11)) return 1;
    const next6_0 = r11;
    r7 = next6_0;
    r6l = add64(r6h, r6l, 0, 1); r6h = RH;
    }
    let r13;
    r13 = r7;
    if (!Number.isFinite(r13)) return 1;
    out0[o * 1 + 0] = r13;
  }
  return 0;
}
