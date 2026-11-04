// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
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
const B0 = {source:"function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const in1 = inputs[1];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    const j1 = i;\n    let r0;\n    r0 = Number(in0[j0 * 3 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(in0[j0 * 3 + 1]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = Number(in0[j0 * 3 + 2]);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = Number(in1[j1 * 3 + 0]);\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = Number(in1[j1 * 3 + 1]);\n    if (!Number.isFinite(r4)) return 1;\n    let r5;\n    r5 = Number(in1[j1 * 3 + 2]);\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = uniforms[0][6];\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = r3 * r6;\n    if (!Number.isFinite(r7)) return 1;\n    let r8;\n    r8 = r4 * r6;\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r5 * r6;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = r0 + r7;\n    if (!Number.isFinite(r10)) return 1;\n    let r11;\n    r11 = r1 + r8;\n    if (!Number.isFinite(r11)) return 1;\n    let r12;\n    r12 = r2 + r9;\n    if (!Number.isFinite(r12)) return 1;\n    if (!Number.isFinite(r10)) return 1;\n    out0[o * 3 + 0] = r10;\n    if (!Number.isFinite(r11)) return 1;\n    out0[o * 3 + 1] = r11;\n    if (!Number.isFinite(r12)) return 1;\n    out0[o * 3 + 2] = r12;\n  }\n  return 0;\n}\n",argumentTypes:[{array: "vec3"},{array: "vec3"}],sources:[{type:{array: "vec3"},source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(0);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"},{type:{array: "vec3"},source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(1);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],captures:[],product:false,skip:[],frame:[[6,"frame/dt",false]],length:13,width:3,integer:false,text:false,frameValue:false,arrayLimit:18014398509481983n,countLimit:4611686018427387903n,kernel:"f0",shapes:{sources:[['opaque',[]],['opaque',[]]],uniforms:[]}};
const B1 = {source:"function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const in1 = inputs[1];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    const j1 = i;\n    let r0;\n    r0 = Number(in0[j0 * 3 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(in0[j0 * 3 + 1]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = Number(in0[j0 * 3 + 2]);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = Number(in1[j1 * 3 + 0]);\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = Number(in1[j1 * 3 + 1]);\n    if (!Number.isFinite(r4)) return 1;\n    let r5;\n    r5 = Number(in1[j1 * 3 + 2]);\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = 0;\n    let r7;\n    r7 = (r0 < r6 ? 1 : 0);\n    let r8h = 0, r8l = 0;\n    r8l = (typeof (uniforms[0][8]) === 'bigint' ? unbig(uniforms[0][8]) : split(uniforms[0][8])); r8h = RH;\n    let r9;\n    r9 = (r8h * 4294967296 + r8l);\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = (r0 >= r9 ? 1 : 0);\n    let r11;\n    r11 = (r7 !== 0 || r10 !== 0 ? 1 : 0);\n    let r12;\n    if (r11 !== 0) {\n    r12 = 0;\n    }\n    let r13;\n    if (r11 !== 0) {\n    r13 = r12 - r3;\n    if (!Number.isFinite(r13)) return 1;\n    }\n    let r14;\n    r14 = r11 !== 0 ? r13 : r3;\n    let r15;\n    r15 = (r1 < r6 ? 1 : 0);\n    let r16h = 0, r16l = 0;\n    r16l = (typeof (uniforms[0][16]) === 'bigint' ? unbig(uniforms[0][16]) : split(uniforms[0][16])); r16h = RH;\n    let r17;\n    r17 = (r16h * 4294967296 + r16l);\n    if (!Number.isFinite(r17)) return 1;\n    let r18;\n    r18 = (r1 >= r17 ? 1 : 0);\n    let r19;\n    r19 = (r15 !== 0 || r18 !== 0 ? 1 : 0);\n    let r20;\n    if (r19 !== 0) {\n    r20 = 0;\n    }\n    let r21;\n    if (r19 !== 0) {\n    r21 = r20 - r4;\n    if (!Number.isFinite(r21)) return 1;\n    }\n    let r22;\n    r22 = r19 !== 0 ? r21 : r4;\n    if (!Number.isFinite(r14)) return 1;\n    out0[o * 3 + 0] = r14;\n    if (!Number.isFinite(r22)) return 1;\n    out0[o * 3 + 1] = r22;\n    if (!Number.isFinite(r6)) return 1;\n    out0[o * 3 + 2] = r6;\n  }\n  return 0;\n}\n",argumentTypes:[{array: "vec3"},{array: "vec3"},{array: "vec3"}],sources:[{type:{array: "vec3"},source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(0);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"},{type:{array: "vec3"},source:"// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\nfunction frame(input, state, rt) {\n  let l0;\n  let at = 0;\n  try {\n    at = 0; l0 = rt.coreArgument(2);\n  } catch (e) {\n    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n    throw e;\n  }\n  return l0;\n}\n"}],captures:[],product:false,skip:[],frame:[[8,"frame/width",true],[16,"frame/height",true]],length:23,width:3,integer:false,text:false,frameValue:false,arrayLimit:18014398509481983n,countLimit:4611686018427387903n,kernel:"f1",shapes:{sources:[['arg',0],['opaque',[]]],uniforms:[]}};
export async function frame(input, state, rt) {
  K ??= ["#080a10", new rt.PArray(3, new Float64Array(rt.data, 0, 30000)), new rt.PArray(3, new Float64Array(rt.data, 240000, 30000)), "#00ffff"];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 5; { const c5 = cell(state, 0, "", input.frame);
      if (c5.value !== undefined) l4 = c5.value; else {
        at = 2; l1 = K[1];
        at = 3; l2 = K[2];
        at = 4; l3 = new Map([["position", l1], ["velocity", l2]]);
        l4 = l3;
      } }
    at = 6; { const c6 = cell(state, 0, "", input.frame); l5 = c6.pending !== undefined; l6 = l5 ? c6.pending : null; }
    at = 7; if (rt.truthy(l5)) {
      at = 8; l7 = l6;
    } else {
      at = 9; l8 = field(rt, l4, "position");
      at = 10; l9 = field(rt, l4, "velocity");
      at = 11; l10 = await rt.invoke(0, [l8, l9], B0);
      at = 12; l11 = field(rt, l4, "position");
      at = 13; l12 = field(rt, l4, "velocity");
      at = 14; l13 = await rt.invoke(1, [l10, l11, l12], B1);
      at = 15; l14 = new Map([["position", l10], ["velocity", l13]]);
      at = 16; cell(state, 0, "", input.frame).pending = l14;
      at = 17; l7 = l14;
    }
    at = 18; l15 = field(rt, l7, "position");
    at = 19; l16 = K[3];
    at = 20; { const [count, position] = pointsAttr(rt, l15); emit(rt, {type: "mark_set", kind: "point", count, position, fill: ink(rt, l16)}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [{"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const in1 = inputs[1];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    const j1 = i;\n    let r0;\n    r0 = Number(in0[j0 * 3 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(in0[j0 * 3 + 1]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = Number(in0[j0 * 3 + 2]);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = Number(in1[j1 * 3 + 0]);\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = Number(in1[j1 * 3 + 1]);\n    if (!Number.isFinite(r4)) return 1;\n    let r5;\n    r5 = Number(in1[j1 * 3 + 2]);\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = uniforms[0][6];\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = r3 * r6;\n    if (!Number.isFinite(r7)) return 1;\n    let r8;\n    r8 = r4 * r6;\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r5 * r6;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = r0 + r7;\n    if (!Number.isFinite(r10)) return 1;\n    let r11;\n    r11 = r1 + r8;\n    if (!Number.isFinite(r11)) return 1;\n    let r12;\n    r12 = r2 + r9;\n    if (!Number.isFinite(r12)) return 1;\n    if (!Number.isFinite(r10)) return 1;\n    out0[o * 3 + 0] = r10;\n    if (!Number.isFinite(r11)) return 1;\n    out0[o * 3 + 1] = r11;\n    if (!Number.isFinite(r12)) return 1;\n    out0[o * 3 + 2] = r12;\n  }\n  return 0;\n}\n", "kernel": null},
  {"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const in1 = inputs[1];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    const j1 = i;\n    let r0;\n    r0 = Number(in0[j0 * 3 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = Number(in0[j0 * 3 + 1]);\n    if (!Number.isFinite(r1)) return 1;\n    let r2;\n    r2 = Number(in0[j0 * 3 + 2]);\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = Number(in1[j1 * 3 + 0]);\n    if (!Number.isFinite(r3)) return 1;\n    let r4;\n    r4 = Number(in1[j1 * 3 + 1]);\n    if (!Number.isFinite(r4)) return 1;\n    let r5;\n    r5 = Number(in1[j1 * 3 + 2]);\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = 0;\n    let r7;\n    r7 = (r0 < r6 ? 1 : 0);\n    let r8h = 0, r8l = 0;\n    r8l = (typeof (uniforms[0][8]) === 'bigint' ? unbig(uniforms[0][8]) : split(uniforms[0][8])); r8h = RH;\n    let r9;\n    r9 = (r8h * 4294967296 + r8l);\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = (r0 >= r9 ? 1 : 0);\n    let r11;\n    r11 = (r7 !== 0 || r10 !== 0 ? 1 : 0);\n    let r12;\n    if (r11 !== 0) {\n    r12 = 0;\n    }\n    let r13;\n    if (r11 !== 0) {\n    r13 = r12 - r3;\n    if (!Number.isFinite(r13)) return 1;\n    }\n    let r14;\n    r14 = r11 !== 0 ? r13 : r3;\n    let r15;\n    r15 = (r1 < r6 ? 1 : 0);\n    let r16h = 0, r16l = 0;\n    r16l = (typeof (uniforms[0][16]) === 'bigint' ? unbig(uniforms[0][16]) : split(uniforms[0][16])); r16h = RH;\n    let r17;\n    r17 = (r16h * 4294967296 + r16l);\n    if (!Number.isFinite(r17)) return 1;\n    let r18;\n    r18 = (r1 >= r17 ? 1 : 0);\n    let r19;\n    r19 = (r15 !== 0 || r18 !== 0 ? 1 : 0);\n    let r20;\n    if (r19 !== 0) {\n    r20 = 0;\n    }\n    let r21;\n    if (r19 !== 0) {\n    r21 = r20 - r4;\n    if (!Number.isFinite(r21)) return 1;\n    }\n    let r22;\n    r22 = r19 !== 0 ? r21 : r4;\n    if (!Number.isFinite(r14)) return 1;\n    out0[o * 3 + 0] = r14;\n    if (!Number.isFinite(r22)) return 1;\n    out0[o * 3 + 1] = r22;\n    if (!Number.isFinite(r6)) return 1;\n    out0[o * 3 + 2] = r6;\n  }\n  return 0;\n}\n", "kernel": null}];
