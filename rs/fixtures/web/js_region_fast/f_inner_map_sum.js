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
    let r0h = 0, r0l = 0;
    r0l = (typeof (in0[j0]) === 'bigint' ? unbig(in0[j0]) : split(in0[j0])); r0h = RH;
    let r1h = 0, r1l = 0;
    r1h = 0; r1l = 0;
    let r2;
    r2 = (r0h * 4294967296 + r0l);
    if (!Number.isFinite(r2)) return 1;
    let r3;
    r3 = 0;
    let r4;
    r4 = r2 + r3;
    if (!Number.isFinite(r4)) return 1;
    let r5h = 0, r5l = 0;
    r5l = split(convert("+", Math.floor(r4 + 0.5))); r5h = RH;
    let r6h = 0, r6l = 0;
    r6h = 0; r6l = 1;
    let r7;
    r7 = 1;
    let r8;
    r8 = r2 + r7;
    if (!Number.isFinite(r8)) return 1;
    let r9h = 0, r9l = 0;
    r9l = split(convert("+", Math.floor(r8 + 0.5))); r9h = RH;
    let r10h = 0, r10l = 0;
    r10h = 0; r10l = 2;
    let r11;
    r11 = 2;
    let r12;
    r12 = r2 + r11;
    if (!Number.isFinite(r12)) return 1;
    let r13h = 0, r13l = 0;
    r13l = split(convert("+", Math.floor(r12 + 0.5))); r13h = RH;
    let r14h = 0, r14l = 0;
    r14l = add64(r1h, r1l, r5h, r5l); r14h = RH; r14h = (r14h << 1) >> 1;
    let r15h = 0, r15l = 0;
    r15l = add64(r14h, r14l, r9h, r9l); r15h = RH; r15h = (r15h << 1) >> 1;
    let r16h = 0, r16l = 0;
    r16l = add64(r15h, r15l, r13h, r13l); r16h = RH; r16h = (r16h << 1) >> 1;
    let r17h = 0, r17l = 0;
    r17l = (typeof (uniforms[0][17]) === 'bigint' ? unbig(uniforms[0][17]) : split(uniforms[0][17])); r17h = RH;
    let r18;
    r18 = (r16h * 4294967296 + r16l);
    if (!Number.isFinite(r18)) return 1;
    let r19;
    r19 = (r17h * 4294967296 + r17l);
    if (!Number.isFinite(r19)) return 1;
    let r20;
    r20 = r18 + r19;
    if (!Number.isFinite(r20)) return 1;
    let r21h = 0, r21l = 0;
    r21l = split(convert("+", Math.floor(r20 + 0.5))); r21h = RH;
    out0[o * 1 + 0] = big(r21h, r21l);
  }
  return 0;
}
