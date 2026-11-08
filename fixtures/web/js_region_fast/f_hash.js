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
    if (b < 0 || b > 63) fail('E_RANGE', 'Integer shift count must be within [0, 63].');
    if (b === 0) { RH = ah; return al; }
    if (right) { if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }
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
    r2h = 0; r2l = 73856093;
    let r3h = 0, r3l = 0;
    r3l = mul64(r1h, r1l, r2h, r2l); r3h = RH;
    let r4h = 0, r4l = 0;
    r4l = mul64(r1h, r1l, r1h, r1l); r4h = RH;
    let r5h = 0, r5l = 0;
    r5h = 0; r5l = 19349663;
    let r6h = 0, r6l = 0;
    r6l = mul64(r4h, r4l, r5h, r5l); r6h = RH;
    let r7h = 0, r7l = 0;
    r7h = r3h ^ r6h; r7l = (r3l ^ r6l) >>> 0;
    let r8h = 0, r8l = 0;
    r8h = 1073741823; r8l = 4294967295;
    let r9h = 0, r9l = 0;
    r9h = r7h & r8h; r9l = (r7l & r8l) >>> 0;
    let r10h = 0, r10l = 0;
    r10h = 0; r10l = 8;
    let r11h = 0, r11l = 0;
    r11l = div64(r9h, r9l, r10h, r10l, true); r11h = RH;
    out0[o * 1 + 0] = big(r11h, r11l);
  }
  return 0;
}
