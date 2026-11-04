// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
const M = new Map();
const store = (rt, s, index, value) => {
  const i = Number(rt.coerce("int", index));
  if (s instanceof rt.TextArray) {
    if (i < 0 || i >= s.data.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (typeof value !== 'string') rt.fail("E_ARRAY_TYPE", "Text arrays contain text values.");
    s.data[i] = value;
  } else if (s instanceof rt.PArray) {
    const n = s.data.length / s.width;
    if (i < 0 || i >= n) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (s.data instanceof BigInt64Array) {
      if (typeof value !== 'number' && typeof value !== 'bigint' && typeof value !== 'boolean')
        rt.fail("E_ARRAY_TYPE", "Packed integer arrays contain integer values.");
      s.data[i] = rt.coerce("int", value);
      return;
    }
    const comps = value instanceof Float64Array ? value : Array(s.width).fill(rt.float(value));
    if (comps.length !== s.width) rt.fail("E_ARRAY_TYPE", "Packed array element has the wrong vector width.");
    for (let j = 0; j < s.width; j++)
      s.data[i * s.width + j] = Number.isFinite(comps[j]) ? comps[j] : rt.fail("E_NONFINITE", "array produced a nonfinite value.");
  } else {
    if (i < 0 || i >= s.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    s[i] = value;
  }
};
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
const B0 = {source:"function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1h = 0, r1l = 0;\n    r1l = (typeof (uniforms[0][0]) === 'bigint' ? unbig(uniforms[0][0]) : split(uniforms[0][0])); r1h = RH;\n    let r2;\n    r2 = (r1h * 4294967296 + r1l);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = r2 === 0 ? 0 : ((r0 % r2) + r2) % r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 10;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r2 === 0 ? 0 : r0 / r2;\n    if (!Number.isFinite(r6)) return 1;\n    let r7h = 0, r7l = 0;\n    r7l = split(convert(\"floor\", Math.floor(r6))); r7h = RH;\n    let r8;\n    r8 = (r7h * 4294967296 + r7l);\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r8 * r4;\n    if (!Number.isFinite(r9)) return 1;\n    let r10h = 0, r10l = 0;\n    r10l = split(convert(\"*\", Math.floor(r9 + 0.5))); r10h = RH;\n    let r11;\n    r11 = (r10h * 4294967296 + r10l);\n    if (!Number.isFinite(r11)) return 1;\n    let r12;\n    r12 = 0;\n    if (!Number.isFinite(r5)) return 1;\n    out0[o * 3 + 0] = r5;\n    if (!Number.isFinite(r11)) return 1;\n    out0[o * 3 + 1] = r11;\n    if (!Number.isFinite(r12)) return 1;\n    out0[o * 3 + 2] = r12;\n  }\n  return 0;\n}\n",argumentTypes:["int",{array: "float"}],sources:[{type:{array: "float"},source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(1);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],captures:[{type:"int",source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(0);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],product:false,skip:[],frame:[],length:13,width:3,integer:false,text:false,frameValue:false,arrayLimit:18014398509481983n,countLimit:4611686018427387903n,kernel:"f0",shapes:{sources:[['arg',1]],uniforms:[['arg',0]]}};
const B1 = {source:"function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const noise = (...args) => { try { return noise3(...args); } catch (e) { if (e?.code === 'E_NONFINITE') return NaN; throw e; } };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1h = 0, r1l = 0;\n    r1l = (typeof (uniforms[0][0]) === 'bigint' ? unbig(uniforms[0][0]) : split(uniforms[0][0])); r1h = RH;\n    let r2;\n    r2 = (r1h * 4294967296 + r1l);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = r2 === 0 ? 0 : ((r0 % r2) + r2) % r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 0.045;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r2 === 0 ? 0 : r0 / r2;\n    if (!Number.isFinite(r6)) return 1;\n    let r7h = 0, r7l = 0;\n    r7l = split(convert(\"floor\", Math.floor(r6))); r7h = RH;\n    let r8;\n    r8 = (r7h * 4294967296 + r7l);\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r8 * r4;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = Number(uniforms[1][0]);\n    if (!Number.isFinite(r10)) return 1;\n    let r11;\n    r11 = noise(Float64Array.of(r5, r9, r10), 2026n, 5n, 2, 0.5);\n    if (!Number.isFinite(r11)) return 1;\n    if (!Number.isFinite(r11)) return 1;\n    out0[o * 1 + 0] = r11;\n  }\n  return 0;\n}\n",argumentTypes:["int",{array: "float"},"float"],sources:[{type:{array: "float"},source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(1);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],captures:[{type:"int",source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(0);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"},{type:"float",source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(2);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],product:false,skip:[],frame:[],length:12,width:1,integer:false,text:false,frameValue:false,arrayLimit:18014398509481983n,countLimit:4611686018427387903n,kernel:"f1",shapes:{sources:[['arg',1]],uniforms:[['arg',0],['arg',2]]}};
export async function frame(input, state, rt) {
  K ??= ["#101426", 10n, 10n, Float64Array.of(10, 10, 0), 0, 0.15, 3n, 0n, 0n, 1, 0, 4, 3n, 1, 0, 1, 0n, 16, 36, 0.5, 255, 20, 74, 0.5, 255, 38, 114, 0.5, 255, 1n, 36, 60, 0.5, 255, 74, 166, 0.5, 255, 114, 166, 0.5, 255, 2n, 60, 242, 0.5, 255, 166, 209, 0.5, 255, 166, 107, 0.5, 255, 242, 242, 0.5, 255, 209, 100, 0.5, 255, 107, 68, 0.5, 255];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79;
  let l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95;
  let l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111;
  let l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127;
  let l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143;
  let l144, l145, l146, l147, l148, l149, l150, l151, l152, l153, l154, l155, l156, l157, l158, l159;
  let l160, l161, l162, l163, l164, l165, l166, l167, l168, l169, l170, l171, l172, l173, l174, l175;
  let l176, l177, l178, l179, l180, l181, l182, l183, l184, l185, l186, l187, l188, l189, l190, l191;
  let l192, l193, l194, l195, l196, l197;
  let at = 0;
  const calls = new Int32Array(1);
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = BigInt(input.size[0]);
    at = 3; l2 = K[1];
    at = 4; l3 = rt.div(l1, l2);
    at = 5; l4 = rt.ceil(l3);
    at = 6; l5 = BigInt(input.size[1]);
    at = 7; l6 = K[2];
    at = 8; l7 = rt.div(l5, l6);
    at = 9; l8 = rt.ceil(l7);
    at = 10; l9 = rt.mul(l4, l8);
    at = 11; l10 = rt.array_range(l9);
    { const k12 = [l4, l10], h12 = rt.recall(M, 12, k12);
    if (h12) [l11] = h12; else {
      at = 12; l11 = await rt.gpuInvoke(0, [l4, l10], B0);
    rt.keep(M, 12, k12, [l11]); } }
    at = 13; l12 = K[3];
    at = 15; { const c15 = cell(state, 0, "", input.frame);
      if (c15.value !== undefined) l14 = c15.value; else {
        at = 14; l13 = K[4];
        l14 = l13;
      } }
    at = 16; { const c16 = cell(state, 0, "", input.frame); l15 = c16.pending !== undefined; l16 = l15 ? c16.pending : null; }
    at = 17; if (rt.truthy(l15)) {
      at = 18; l17 = l16;
    } else {
      at = 19; l18 = input.dt;
      at = 20; l19 = K[5];
      at = 21; l20 = rt.mul(l18, l19);
      at = 22; l21 = rt.add(l14, l20);
      at = 23; l22 = rt.coerce("float", l21);
      at = 24; cell(state, 0, "", input.frame).pending = l22;
      at = 25; l17 = l22;
    }
    { const k26 = [l4, l10, l17], h26 = rt.recall(M, 26, k26);
    if (h26) [l23] = h26; else {
      at = 26; l23 = await rt.invoke(1, [l4, l10, l17], B1);
    rt.keep(M, 26, k26, [l23]); } }
    at = 27; calls[0] = 0;
    at = 28; l24 = rt.array_count(l23);
    at = 29; l25 = K[6];
    at = 30; l26 = rt.sequence_build(l24, l25);
    at = 31; l27 = K[7];
    at = 32; l28 = l27;
    at = 33; l29 = K[8];
    { const k34 = [l23, l24, l29], h34 = rt.recall(M, 34, k34);
    if (h34) [l26, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47, l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63, l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79, l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95, l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111, l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127, l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143, l144, l145, l146, l147, l148, l149, l150, l151, l152, l153, l154, l155, l156, l157, l158, l159, l160, l161, l162, l163, l164, l165, l166, l167, l168, l169, l170, l171, l172, l173, l174, l175, l176, l177, l178, l179, l180, l181, l182, l183, l184, l185, l186, l187, l188, l189, l190, l191, l192, l193, l194, l195, l196, l197] = h34; else {
      at = 34; {
        const f34 = rt.coerce("int", l29), e34 = rt.coerce("int", l24), n34 = e34 > f34 ? e34 - f34 : 0n;
        if (n34 > 18014398509481983n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l30 = n34;
        for (let i34 = 0n, c34 = n34; i34 < c34; i34++) {
          const j34 = i34; l31 = j34; l32 = f34 + j34;
          at = 35; l33 = rt.array_nth(l23, l32);
          at = 36; l34 = BigInt(calls[0]++);
          at = 37; l35 = rt.float(l33);
          at = 38; l36 = K[9];
          at = 39; l37 = rt.min(l36, l35);
          at = 40; l38 = K[10];
          at = 41; l39 = rt.max(l38, l37);
          at = 42; l40 = K[11];
          at = 43; l41 = rt.mul(l39, l40);
          at = 44; l42 = rt.int(l41);
          at = 45; l43 = K[12];
          at = 46; l44 = rt.min(l43, l42);
          at = 47; l45 = rt.float(l44);
          at = 48; l46 = rt.sub(l41, l45);
          at = 49; l47 = K[13];
          at = 50; l48 = rt.min(l47, l46);
          at = 51; l49 = K[14];
          at = 52; l50 = rt.max(l49, l48);
          at = 53; l51 = K[15];
          at = 54; l52 = rt.sub(l51, l50);
          at = 55; l53 = K[16];
          at = 56; l54 = rt.eq(l44, l53);
          at = 57; if (rt.truthy(l54)) {
            at = 58; l56 = K[17];
            at = 59; l57 = rt.mul(l56, l52);
            at = 60; l58 = K[18];
            at = 61; l59 = rt.mul(l58, l50);
            at = 62; l60 = rt.add(l57, l59);
            at = 63; l61 = K[19];
            at = 64; l62 = rt.add(l60, l61);
            at = 65; l63 = rt.int(l62);
            at = 66; l64 = rt.float(l63);
            at = 67; l65 = K[20];
            at = 68; l66 = rt.div(l64, l65);
            at = 69; l67 = K[21];
            at = 70; l68 = rt.mul(l67, l52);
            at = 71; l69 = K[22];
            at = 72; l70 = rt.mul(l69, l50);
            at = 73; l71 = rt.add(l68, l70);
            at = 74; l72 = K[23];
            at = 75; l73 = rt.add(l71, l72);
            at = 76; l74 = rt.int(l73);
            at = 77; l75 = rt.float(l74);
            at = 78; l76 = K[24];
            at = 79; l77 = rt.div(l75, l76);
            at = 80; l78 = K[25];
            at = 81; l79 = rt.mul(l78, l52);
            at = 82; l80 = K[26];
            at = 83; l81 = rt.mul(l80, l50);
            at = 84; l82 = rt.add(l79, l81);
            at = 85; l83 = K[27];
            at = 86; l84 = rt.add(l82, l83);
            at = 87; l85 = rt.int(l84);
            at = 88; l86 = rt.float(l85);
            at = 89; l87 = K[28];
            at = 90; l88 = rt.div(l86, l87);
            at = 91; l89 = Float64Array.of(rt.float(l66), rt.float(l77), rt.float(l88));
            at = 92; l55 = l89;
          } else {
            at = 93; l90 = K[29];
            at = 94; l91 = rt.eq(l44, l90);
            at = 95; if (rt.truthy(l91)) {
              at = 96; l93 = K[30];
              at = 97; l94 = rt.mul(l93, l52);
              at = 98; l95 = K[31];
              at = 99; l96 = rt.mul(l95, l50);
              at = 100; l97 = rt.add(l94, l96);
              at = 101; l98 = K[32];
              at = 102; l99 = rt.add(l97, l98);
              at = 103; l100 = rt.int(l99);
              at = 104; l101 = rt.float(l100);
              at = 105; l102 = K[33];
              at = 106; l103 = rt.div(l101, l102);
              at = 107; l104 = K[34];
              at = 108; l105 = rt.mul(l104, l52);
              at = 109; l106 = K[35];
              at = 110; l107 = rt.mul(l106, l50);
              at = 111; l108 = rt.add(l105, l107);
              at = 112; l109 = K[36];
              at = 113; l110 = rt.add(l108, l109);
              at = 114; l111 = rt.int(l110);
              at = 115; l112 = rt.float(l111);
              at = 116; l113 = K[37];
              at = 117; l114 = rt.div(l112, l113);
              at = 118; l115 = K[38];
              at = 119; l116 = rt.mul(l115, l52);
              at = 120; l117 = K[39];
              at = 121; l118 = rt.mul(l117, l50);
              at = 122; l119 = rt.add(l116, l118);
              at = 123; l120 = K[40];
              at = 124; l121 = rt.add(l119, l120);
              at = 125; l122 = rt.int(l121);
              at = 126; l123 = rt.float(l122);
              at = 127; l124 = K[41];
              at = 128; l125 = rt.div(l123, l124);
              at = 129; l126 = Float64Array.of(rt.float(l103), rt.float(l114), rt.float(l125));
              at = 130; l92 = l126;
            } else {
              at = 131; l127 = K[42];
              at = 132; l128 = rt.eq(l44, l127);
              at = 133; if (rt.truthy(l128)) {
                at = 134; l130 = K[43];
                at = 135; l131 = rt.mul(l130, l52);
                at = 136; l132 = K[44];
                at = 137; l133 = rt.mul(l132, l50);
                at = 138; l134 = rt.add(l131, l133);
                at = 139; l135 = K[45];
                at = 140; l136 = rt.add(l134, l135);
                at = 141; l137 = rt.int(l136);
                at = 142; l138 = rt.float(l137);
                at = 143; l139 = K[46];
                at = 144; l140 = rt.div(l138, l139);
                at = 145; l141 = K[47];
                at = 146; l142 = rt.mul(l141, l52);
                at = 147; l143 = K[48];
                at = 148; l144 = rt.mul(l143, l50);
                at = 149; l145 = rt.add(l142, l144);
                at = 150; l146 = K[49];
                at = 151; l147 = rt.add(l145, l146);
                at = 152; l148 = rt.int(l147);
                at = 153; l149 = rt.float(l148);
                at = 154; l150 = K[50];
                at = 155; l151 = rt.div(l149, l150);
                at = 156; l152 = K[51];
                at = 157; l153 = rt.mul(l152, l52);
                at = 158; l154 = K[52];
                at = 159; l155 = rt.mul(l154, l50);
                at = 160; l156 = rt.add(l153, l155);
                at = 161; l157 = K[53];
                at = 162; l158 = rt.add(l156, l157);
                at = 163; l159 = rt.int(l158);
                at = 164; l160 = rt.float(l159);
                at = 165; l161 = K[54];
                at = 166; l162 = rt.div(l160, l161);
                at = 167; l163 = Float64Array.of(rt.float(l140), rt.float(l151), rt.float(l162));
                at = 168; l129 = l163;
              } else {
                at = 169; l164 = K[55];
                at = 170; l165 = rt.mul(l164, l52);
                at = 171; l166 = K[56];
                at = 172; l167 = rt.mul(l166, l50);
                at = 173; l168 = rt.add(l165, l167);
                at = 174; l169 = K[57];
                at = 175; l170 = rt.add(l168, l169);
                at = 176; l171 = rt.int(l170);
                at = 177; l172 = rt.float(l171);
                at = 178; l173 = K[58];
                at = 179; l174 = rt.div(l172, l173);
                at = 180; l175 = K[59];
                at = 181; l176 = rt.mul(l175, l52);
                at = 182; l177 = K[60];
                at = 183; l178 = rt.mul(l177, l50);
                at = 184; l179 = rt.add(l176, l178);
                at = 185; l180 = K[61];
                at = 186; l181 = rt.add(l179, l180);
                at = 187; l182 = rt.int(l181);
                at = 188; l183 = rt.float(l182);
                at = 189; l184 = K[62];
                at = 190; l185 = rt.div(l183, l184);
                at = 191; l186 = K[63];
                at = 192; l187 = rt.mul(l186, l52);
                at = 193; l188 = K[64];
                at = 194; l189 = rt.mul(l188, l50);
                at = 195; l190 = rt.add(l187, l189);
                at = 196; l191 = K[65];
                at = 197; l192 = rt.add(l190, l191);
                at = 198; l193 = rt.int(l192);
                at = 199; l194 = rt.float(l193);
                at = 200; l195 = K[66];
                at = 201; l196 = rt.div(l194, l195);
                at = 202; l197 = Float64Array.of(rt.float(l174), rt.float(l185), rt.float(l196));
                at = 203; l129 = l197;
              }
              at = 204; l92 = l129;
            }
            at = 205; l55 = l92;
          }
          at = 206; store(rt, l26, l32, l55);
        }
      }
    rt.keep(M, 34, k34, [l26, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47, l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63, l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79, l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95, l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111, l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127, l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143, l144, l145, l146, l147, l148, l149, l150, l151, l152, l153, l154, l155, l156, l157, l158, l159, l160, l161, l162, l163, l164, l165, l166, l167, l168, l169, l170, l171, l172, l173, l174, l175, l176, l177, l178, l179, l180, l181, l182, l183, l184, l185, l186, l187, l188, l189, l190, l191, l192, l193, l194, l195, l196, l197]); } }
    at = 207; markSet(rt, true, "rect", {positions: l11, sizes: l12, fill: l26});
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [{"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1h = 0, r1l = 0;\n    r1l = (typeof (uniforms[0][0]) === 'bigint' ? unbig(uniforms[0][0]) : split(uniforms[0][0])); r1h = RH;\n    let r2;\n    r2 = (r1h * 4294967296 + r1l);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = r2 === 0 ? 0 : ((r0 % r2) + r2) % r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 10;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r2 === 0 ? 0 : r0 / r2;\n    if (!Number.isFinite(r6)) return 1;\n    let r7h = 0, r7l = 0;\n    r7l = split(convert(\"floor\", Math.floor(r6))); r7h = RH;\n    let r8;\n    r8 = (r7h * 4294967296 + r7l);\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r8 * r4;\n    if (!Number.isFinite(r9)) return 1;\n    let r10h = 0, r10l = 0;\n    r10l = split(convert(\"*\", Math.floor(r9 + 0.5))); r10h = RH;\n    let r11;\n    r11 = (r10h * 4294967296 + r10l);\n    if (!Number.isFinite(r11)) return 1;\n    let r12;\n    r12 = 0;\n    if (!Number.isFinite(r5)) return 1;\n    out0[o * 3 + 0] = r5;\n    if (!Number.isFinite(r11)) return 1;\n    out0[o * 3 + 1] = r11;\n    if (!Number.isFinite(r12)) return 1;\n    out0[o * 3 + 2] = r12;\n  }\n  return 0;\n}\n", "kernel": null},
  {"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const noise = (...args) => { try { return noise3(...args); } catch (e) { if (e?.code === 'E_NONFINITE') return NaN; throw e; } };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1h = 0, r1l = 0;\n    r1l = (typeof (uniforms[0][0]) === 'bigint' ? unbig(uniforms[0][0]) : split(uniforms[0][0])); r1h = RH;\n    let r2;\n    r2 = (r1h * 4294967296 + r1l);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = r2 === 0 ? 0 : ((r0 % r2) + r2) % r2;\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = 0.045;\n    let r5;\n    r5 = r3 * r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = r2 === 0 ? 0 : r0 / r2;\n    if (!Number.isFinite(r6)) return 1;\n    let r7h = 0, r7l = 0;\n    r7l = split(convert(\"floor\", Math.floor(r6))); r7h = RH;\n    let r8;\n    r8 = (r7h * 4294967296 + r7l);\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r8 * r4;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = Number(uniforms[1][0]);\n    if (!Number.isFinite(r10)) return 1;\n    let r11;\n    r11 = noise(Float64Array.of(r5, r9, r10), 2026n, 5n, 2, 0.5);\n    if (!Number.isFinite(r11)) return 1;\n    if (!Number.isFinite(r11)) return 1;\n    out0[o * 1 + 0] = r11;\n  }\n  return 0;\n}\n", "kernel": null}];
