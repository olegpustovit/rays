// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const M = new Map();
const cell = (state, slot, key, stamp) => {
  const cells = state[slot] ??= new Map();
  let c = cells.get(key);
  if (c === undefined) cells.set(key, c = { value: undefined, pending: undefined, stamp });
  else if (c.stamp !== stamp) {
    if (c.pending !== undefined) c.value = c.pending;
    c.pending = undefined; c.stamp = stamp;
  }
  return c;
};
const num = (rt, v) => typeof v === "number" ? v : typeof v === "bigint" ? Number(v)
  : typeof v === "boolean" ? +v : rt.fail("E_TYPE", "Expected a number.");
const comps = (rt, v) => v instanceof Float64Array && v.length === 3 ? v : (n => [n, n, n])(num(rt, v));
const toInt = (rt, x) => Number.isFinite(x) && x < 2 ** 62 && x >= -(2 ** 62) ? Math.trunc(x)
  : rt.fail("E_DRAW_RANGE", "A drawing coordinate must fit an integer.");
const point = (rt, v) => { const c = comps(rt, v); return [toInt(rt, c[0]), toInt(rt, c[1])]; };
const pair = (rt, v) => { const c = comps(rt, v); return [c[0], c[1]]; };
const rgba = (rt, v) => {
  const c = rt.color_parse(v);
  return (Number(c.get("r")) << 24 | Number(c.get("g")) << 16 | Number(c.get("b")) << 8 | Number(c.get("a"))) | 0;
};
const packed3 = (rt, v) => v instanceof rt.PArray && v.width === 3;
const ink = (rt, v) => packed3(rt, v) ? -1 : rgba(rt, v);
const emit = (rt, entry) => {
  if (rt.marks.length >= 1048576) rt.fail("E_DRAW_RANGE", "A drawing has too many entries.");
  rt.marks.push(entry);
};
const emitAll = (rt, entries) => { for (const e of entries) emit(rt, e); };
const buffer = (data, count, width) => ({ buffer: data, count, width });
const items = (rt, v) => v instanceof rt.PArray ? Array.from({ length: Number(rt.array_count(v)) }, (_, i) => rt.array_nth(v, i))
  : Array.isArray(v) ? v : rt.fail("E_TYPE", "Expected an array.");
const arg = (rt, a, k) => a[k] ?? rt.fail("E_DRAW_INPUT", "draw needs " + k + ".");
const colourAttr = (rt, count, v) => {
  if (!packed3(rt, v)) return { constant: rgba(rt, v) };
  if (v.data.length !== 3 * count) rt.fail("E_DRAW_LENGTH", "Colors and positions have different lengths.");
  return buffer(v.data, count, 3);
};
const checkColours = (rt, n, a) => {
  for (const k of ["fill", "stroke", "color"])
    if (packed3(rt, a[k]) && a[k].data.length !== 3 * n) rt.fail("E_DRAW_LENGTH", "Colors and positions have different lengths.");
};
const pointsAttr = (rt, v) => {
  if (v instanceof rt.PArray && (v.width === 2 || v.width === 3)) {
    const n = v.data.length / v.width;
    return [n, buffer(v.data, n, v.width)];
  }
  const xs = items(rt, v), data = new Float64Array(2 * xs.length);
  xs.forEach((x, i) => { const p = pair(rt, x); data[2 * i] = p[0]; data[2 * i + 1] = p[1]; });
  return [xs.length, buffer(data, xs.length, 2)];
};
const markSet = (rt, batch, kind, a) => {
  const style = n => [a.fill === undefined ? { constant: 0 } : colourAttr(rt, n, a.fill),
    a.stroke === undefined ? { constant: 0 } : colourAttr(rt, n, a.stroke)];
  const where = key => batch ? pointsAttr(rt, arg(rt, a, key)) : [1, { constant: pair(rt, arg(rt, a, key)) }];
  const one = v => ({ constant: pair(rt, v) });
  const width = { constant: 1 };
  if (kind === "circle") {
    const [n, position] = where(batch ? "positions" : "at");
    const r = arg(rt, a, "radius");
    let radius;
    if (!batch) radius = { constant: num(rt, r) };
    else if (r instanceof rt.PArray && r.width === 1) {
      if (r.data.length !== n) rt.fail("E_DRAW_LENGTH", "Radii and positions have different lengths.");
      radius = buffer(r.data, n, 1);
    } else if (typeof r === "bigint" || typeof r === "number") radius = { constant: num(rt, r) };
    else rt.fail("E_TYPE", "Radius is a number or packed float array.");
    checkColours(rt, n, a);
    const [fill, stroke] = style(n);
    emit(rt, { type: "mark_set", kind, count: n, position, radius, fill, stroke, stroke_width: width });
  } else if (kind === "rect") {
    const [n, position] = where(batch ? "positions" : "at");
    const s = arg(rt, a, batch ? "sizes" : "size");
    let size;
    if (!batch) size = one(s);
    else if (packed3(rt, s)) {
      if (s.data.length !== 3 * n) rt.fail("E_DRAW_LENGTH", "Sizes and positions have different lengths.");
      size = buffer(s.data, n, 3);
    } else if (s instanceof Float64Array && s.length === 3) size = one(s);
    else rt.fail("E_TYPE", "Sizes are a vec3 or packed vec3 array.");
    checkColours(rt, n, a);
    const [fill, stroke] = style(n);
    emit(rt, { type: "mark_set", kind, count: n, position, size, fill, stroke, stroke_width: width });
  } else {
    const [n, position] = where("from");
    let end;
    if (batch) {
      const [m, e] = pointsAttr(rt, arg(rt, a, "to"));
      if (m !== n) rt.fail("E_DRAW_LENGTH", "Line endpoints have different lengths.");
      end = e;
    } else end = one(arg(rt, a, "to"));
    checkColours(rt, n, a);
    emit(rt, { type: "mark_set", kind, count: n, position, end, stroke: colourAttr(rt, n, arg(rt, a, "color")),
      stroke_width: { constant: num(rt, arg(rt, a, "width")) } });
  }
};
const optColour = (rt, v) => v === undefined ? null : rgba(rt, v);
const pathOf = (rt, kind, a) => {
  if (kind === "polygon") emitAll(rt, rt.polygon(items(rt, arg(rt, a, "points")).map(p => floatPoint(rt, p)), optColour(rt, a.fill), optColour(rt, a.stroke)));
  else if (kind === "polyline") emitAll(rt, rt.polyline(items(rt, arg(rt, a, "points")).map(p => floatPoint(rt, p)), ink(rt, arg(rt, a, "color"))));
  else {
    const [w, h] = floatPoint(rt, arg(rt, a, "size")), [x, y] = floatPoint(rt, arg(rt, a, "at"));
    const radius = Math.max(0, Math.min(num(rt, arg(rt, a, "radius")), Math.min(Math.abs(w), Math.abs(h)) / 2));
    emit(rt, { type: "push_transform", xx: 1, xy: 0, yx: 0, yy: 1, tx: x, ty: y });
    emitAll(rt, rt.rounded_rect(w, h, radius, optColour(rt, a.fill), optColour(rt, a.stroke)));
    emit(rt, { type: "pop_transform" });
  }
};
const transform = (rt, kind, v) => {
  if (kind === "translate") {
    const [x, y] = point(rt, v);
    emit(rt, { type: "push_transform", xx: 1, xy: 0, yx: 0, yy: 1, tx: x, ty: y });
  } else if (kind === "rotate") {
    const a = num(rt, v);
    if (!Number.isFinite(a)) rt.fail("E_NONFINITE", "angle produced a nonfinite value.");
    emit(rt, { type: "push_transform", xx: Math.cos(a), xy: Math.sin(a), yx: -Math.sin(a), yy: Math.cos(a), tx: 0, ty: 0 });
  } else {
    const [x, y] = v instanceof Float64Array && v.length === 3 ? [v[0], v[1]] : (n => [n, n])(num(rt, v));
    if (!Number.isFinite(x) || !Number.isFinite(y)) rt.fail("E_NONFINITE", "scale produced a nonfinite value.");
    emit(rt, { type: "push_transform", xx: x, xy: 0, yx: 0, yy: y, tx: 0, ty: 0 });
  }
};
const B0 = {source:"function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(uniforms[0][0]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = 4;\n    let r3;\n    r3 = r0 * r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 0.01;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r1 + r5;\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = Math.sin(r6);\n    if (!Number.isFinite(r7)) return 1;\n    let r8;\n    r8 = 200;\n    let r9;\n    r9 = r8 * r7;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = 250;\n    let r11;\n    r11 = r10 + r9;\n    if (!Number.isFinite(r11)) return 1;\n    let r12h = 0, r12l = 0;\n    r12l = split(convert(\"int\", Math.trunc(r11))); r12h = RH;\n    let r13;\n    r13 = (r12h * 4294967296 + r12l);\n    if (!Number.isFinite(r13)) return 1;\n    let r14;\n    r14 = 0;\n    if (!Number.isFinite(r13)) return 1;\n    out0[o * 3 + 0] = r13;\n    if (!Number.isFinite(r0)) return 1;\n    out0[o * 3 + 1] = r0;\n    if (!Number.isFinite(r14)) return 1;\n    out0[o * 3 + 2] = r14;\n  }\n  return 0;\n}\n",argumentTypes:["float"],sources:[{type:{array: "float"},source:"// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.\nlet K;\nfunction frame(input, state, rt) {\n  K ??= [new rt.PArray(1, Float64Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 496, 497, 498, 499, 500))];\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = K[0];\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],captures:[{type:"float",source:"// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(0);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],product:false,skip:[],frame:[],length:15,width:3,integer:false,text:false,frameValue:false,arrayLimit:18014398509481983n,countLimit:4611686018427387903n,kernel:"f0",shapes:{sources:[['host']],uniforms:[['arg',0]]}};
const B1 = {source:"function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(uniforms[0][0]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = 4;\n    let r3;\n    r3 = r0 * r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 0.0037;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r1 + r5;\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = Math.sin(r6);\n    if (!Number.isFinite(r7)) return 1;\n    let r8;\n    r8 = 40;\n    let r9;\n    r9 = r8 * r7;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = 50;\n    let r11;\n    r11 = r10 + r9;\n    if (!Number.isFinite(r11)) return 1;\n    let r12h = 0, r12l = 0;\n    r12l = split(convert(\"int\", Math.trunc(r11))); r12h = RH;\n    out0[o * 1 + 0] = big(r12h, r12l);\n  }\n  return 0;\n}\n",argumentTypes:["float"],sources:[{type:{array: "float"},source:"// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.\nlet K;\nfunction frame(input, state, rt) {\n  K ??= [new rt.PArray(1, Float64Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 496, 497, 498, 499, 500))];\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = K[0];\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],captures:[{type:"float",source:"// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(0);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],product:false,skip:[],frame:[],length:13,width:1,integer:true,text:false,frameValue:false,arrayLimit:18014398509481983n,countLimit:4611686018427387903n,kernel:"f1",shapes:{sources:[['host']],uniforms:[['arg',0]]}};
export async function frame(input, state, rt) {
  K ??= ["#000000", 2n, 2n, 0n, 1.5707963267948966, 2n, 0n, 2n, 0n, 0n, 2, 0, new rt.PArray(3, new Float64Array(rt.data, 0, 1503))];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = BigInt(input.size[0]);
    at = 3; l2 = K[1];
    at = 4; l3 = rt.div(l1, l2);
    at = 5; l4 = BigInt(input.size[1]);
    at = 6; l5 = K[2];
    at = 7; l6 = rt.div(l4, l5);
    at = 8; l7 = K[3];
    at = 9; l8 = Float64Array.of(rt.float(l3), rt.float(l6), rt.float(l7));
    at = 10; transform(rt, "translate", l8);
    at = 11; l9 = K[4];
    at = 12; transform(rt, "rotate", l9);
    at = 13; l10 = K[5];
    at = 14; l11 = rt.div(l1, l10);
    at = 15; l12 = K[6];
    at = 16; l13 = rt.sub(l12, l11);
    at = 17; l14 = K[7];
    at = 18; l15 = rt.div(l4, l14);
    at = 19; l16 = K[8];
    at = 20; l17 = rt.sub(l16, l15);
    at = 21; l18 = K[9];
    at = 22; l19 = Float64Array.of(rt.float(l13), rt.float(l17), rt.float(l18));
    at = 23; transform(rt, "translate", l19);
    at = 24; l20 = K[10];
    at = 25; transform(rt, "scale", l20);
    at = 27; { const c27 = cell(state, 0, "", input.frame);
      if (c27.value !== undefined) l22 = c27.value; else {
        at = 26; l21 = K[11];
        l22 = l21;
      } }
    at = 28; { const c28 = cell(state, 0, "", input.frame); l23 = c28.pending !== undefined; l24 = l23 ? c28.pending : null; }
    at = 29; if (rt.truthy(l23)) {
      at = 30; l25 = l24;
    } else {
      at = 31; l26 = input.dt;
      at = 32; l27 = rt.add(l22, l26);
      at = 33; l28 = rt.coerce("float", l27);
      at = 34; cell(state, 0, "", input.frame).pending = l28;
      at = 35; l25 = l28;
    }
    { const k36 = [l25], h36 = rt.recall(M, 36, k36);
    if (h36) [l29] = h36; else {
      at = 36; l29 = await rt.gpuInvoke(0, [l25], B0);
    rt.keep(M, 36, k36, [l29]); } }
    { const k37 = [l25], h37 = rt.recall(M, 37, k37);
    if (h37) [l30] = h37; else {
      at = 37; l30 = await rt.gpuInvoke(1, [l25], B1);
    rt.keep(M, 37, k37, [l30]); } }
    at = 38; l31 = K[12];
    at = 39; markSet(rt, true, "circle", {positions: l29, radius: l30, fill: l31});
    at = 40; emit(rt, {type: "pop_transform"});
    at = 41; emit(rt, {type: "pop_transform"});
    at = 42; emit(rt, {type: "pop_transform"});
    at = 43; emit(rt, {type: "pop_transform"});
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [{"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(uniforms[0][0]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = 4;\n    let r3;\n    r3 = r0 * r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 0.01;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r1 + r5;\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = Math.sin(r6);\n    if (!Number.isFinite(r7)) return 1;\n    let r8;\n    r8 = 200;\n    let r9;\n    r9 = r8 * r7;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = 250;\n    let r11;\n    r11 = r10 + r9;\n    if (!Number.isFinite(r11)) return 1;\n    let r12h = 0, r12l = 0;\n    r12l = split(convert(\"int\", Math.trunc(r11))); r12h = RH;\n    let r13;\n    r13 = (r12h * 4294967296 + r12l);\n    if (!Number.isFinite(r13)) return 1;\n    let r14;\n    r14 = 0;\n    if (!Number.isFinite(r13)) return 1;\n    out0[o * 3 + 0] = r13;\n    if (!Number.isFinite(r0)) return 1;\n    out0[o * 3 + 1] = r0;\n    if (!Number.isFinite(r14)) return 1;\n    out0[o * 3 + 2] = r14;\n  }\n  return 0;\n}\n", "kernel": null},
  {"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(uniforms[0][0]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = 4;\n    let r3;\n    r3 = r0 * r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 0.0037;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r1 + r5;\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = Math.sin(r6);\n    if (!Number.isFinite(r7)) return 1;\n    let r8;\n    r8 = 40;\n    let r9;\n    r9 = r8 * r7;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = 50;\n    let r11;\n    r11 = r10 + r9;\n    if (!Number.isFinite(r11)) return 1;\n    let r12h = 0, r12l = 0;\n    r12l = split(convert(\"int\", Math.trunc(r11))); r12h = RH;\n    out0[o * 1 + 0] = big(r12h, r12l);\n  }\n  return 0;\n}\n", "kernel": null}];
