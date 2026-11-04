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
    r2 = 0;
    let r3h = 0, r3l = 0;
    r3h = 0; r3l = 1;
    let r4h = 0, r4l = 0;
    r4h = 0; r4l = 0;
    let r5h = 0, r5l = 0;
    let r6;
    let r7;
    let r8;
    let r9;
    r5h = r4h; r5l = r4l;
    r6 = r1;
    r7 = r2;
    r8 = r2;
    r9 = r2;
    for (; (r5h < r3h || (r5h === r3h && r5l < r3l));) {
    let r10h = 0, r10l = 0;
    r10l = split(convert("int", Math.trunc(r0))); r10h = RH;
    let r11h = 0, r11l = 0;
    r11l = (typeof (uniforms[0][11]) === 'bigint' ? unbig(uniforms[0][11]) : split(uniforms[0][11])); r11h = RH;
    let r12h = 0, r12l = 0;
    r12l = add64(r10h, r10l, r11h, r11l); r12h = RH; r12h = (r12h << 1) >> 1;
    let r13h = 0, r13l = 0;
    r13h = 0; r13l = 4;
    let r14h = 0, r14l = 0;
    r14l = div64(r12h, r12l, r13h, r13l, true); r14h = RH; r14h = (r14h << 1) >> 1;
    let r15h = 0, r15l = 0;
    r15h = 0; r15l = 1;
    let r16h = 0, r16l = 0;
    r16l = add64(r14h, r14l, r15h, r15l); r16h = RH; r16h = (r16h << 1) >> 1;
    let r17h = 0, r17l = 0;
    r17h = 0; r17l = 2;
    let r18h = 0, r18l = 0;
    r18l = mul64(r16h, r16l, r17h, r17l); r18h = RH; r18h = (r18h << 1) >> 1;
    let r19h = 0, r19l = 0;
    r19l = div64(r12h, r12l, r15h, r15l, true); r19h = RH; r19h = (r19h << 1) >> 1;
    let r20h = 0, r20l = 0;
    r20h = 0; r20l = 62;
    let r21h = 0, r21l = 0;
    r21l = add64(r20h, r20l, r19h, r19l); r21h = RH; r21h = (r21h << 1) >> 1;
    let r22h = 0, r22l = 0;
    r22l = shift64(r15h, r15l, (r21h * 4294967296 + r21l), false); r22h = RH; r22h = (r22h << 1) >> 1;
    let r23h = 0, r23l = 0;
    r23h = 1073741823; r23l = 4294967295;
    let r24h = 0, r24l = 0;
    r24l = add64(r23h, r23l, r16h, r16l); r24h = RH; r24h = (r24h << 1) >> 1;
    let r25h = 0, r25l = 0;
    r25h = 0; r25l = 0;
    let r26;
    r26 = ((r24h < r25h || (r24h === r25h && r24l < r25l)) ? 1 : 0);
    if (!Number.isFinite(r26)) return 1;
    let r27;
    if (r26 !== 0) {
    r27 = 1;
    }
    let r28;
    if (r26 === 0) {
    r28 = 0;
    }
    let r29;
    r29 = r26 !== 0 ? r27 : r28;
    let r30h = 0, r30l = 0;
    r30l = mul64(r23h, r23l, r18h, r18l); r30h = RH; r30h = (r30h << 1) >> 1;
    let r31h = 0, r31l = 0;
    r31l = sub64(r25h, r25l, r18h, r18l); r31h = RH; r31h = (r31h << 1) >> 1;
    let r32;
    r32 = ((r30h === r31h && r30l === r31l) ? 1 : 0);
    if (!Number.isFinite(r32)) return 1;
    let r33;
    if (r32 !== 0) {
    r33 = 1;
    }
    let r34;
    if (r32 === 0) {
    r34 = 0;
    }
    let r35;
    r35 = r32 !== 0 ? r33 : r34;
    let r36h = 0, r36l = 0;
    r36l = sub64(r25h, r25l, r16h, r16l); r36h = RH; r36h = (r36h << 1) >> 1;
    let r37h = 0, r37l = 0;
    r37l = shift64(r36h, r36l, (r20h * 4294967296 + r20l), true); r37h = RH; r37h = (r37h << 1) >> 1;
    let r38;
    r38 = ((r37h === r15h && r37l === r15l) ? 1 : 0);
    if (!Number.isFinite(r38)) return 1;
    let r39;
    if (r38 !== 0) {
    r39 = 1;
    }
    let r40;
    if (r38 === 0) {
    r40 = 0;
    }
    let r41;
    r41 = r38 !== 0 ? r39 : r40;
    let r42h = 0, r42l = 0;
    if (r22h < 0) { r42l = sub64(0, 0, r22h, r22l); r42h = RH; } else { r42h = r22h; r42l = r22l; } r42h = (r42h << 1) >> 1;
    let r43;
    r43 = ((r42h < r25h || (r42h === r25h && r42l < r25l)) ? 1 : 0);
    if (!Number.isFinite(r43)) return 1;
    let r44h = 0, r44l = 0;
    r44h = -1; r44l = 4294967295;
    let r45h = 0, r45l = 0;
    r45l = div64(r22h, r22l, r44h, r44l, false); r45h = RH; r45h = (r45h << 1) >> 1;
    let r46;
    r46 = ((r45h === r22h && r45l === r22l) ? 1 : 0);
    if (!Number.isFinite(r46)) return 1;
    let r47;
    r47 = (r43 !== 0 && r46 !== 0 ? 1 : 0);
    let r48;
    if (r47 !== 0) {
    r48 = 1;
    }
    let r49;
    if (r47 === 0) {
    r49 = 0;
    }
    let r50;
    r50 = r47 !== 0 ? r48 : r49;
    const next5_0 = r29;
    const next5_1 = r35;
    const next5_2 = r41;
    const next5_3 = r50;
    r6 = next5_0;
    r7 = next5_1;
    r8 = next5_2;
    r9 = next5_3;
    r5l = add64(r5h, r5l, 0, 1); r5h = RH;
    }
    let r52;
    r52 = r6;
    let r53;
    r53 = r7;
    let r54;
    r54 = r8;
    let r55;
    r55 = r9;
    if (!Number.isFinite(r52)) return 1;
    out0[o * 4 + 0] = r52;
    if (!Number.isFinite(r53)) return 1;
    out0[o * 4 + 1] = r53;
    if (!Number.isFinite(r54)) return 1;
    out0[o * 4 + 2] = r54;
    if (!Number.isFinite(r55)) return 1;
    out0[o * 4 + 3] = r55;
  }
  return 0;
}
