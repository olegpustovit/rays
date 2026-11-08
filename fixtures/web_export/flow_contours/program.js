import * as image1 from "./image1-ae431d8b9813f868fe0bf1a015a967db.js";
import * as image3 from "./image3-54309263036ee60ca94c84f12c38c4f0.js";
import * as image5 from "./image5-54309263036ee60ca94c84f12c38c4f0.js";
import * as image7 from "./image7-54309263036ee60ca94c84f12c38c4f0.js";
import * as image9 from "./image9-54309263036ee60ca94c84f12c38c4f0.js";
import * as image11 from "./image11-54309263036ee60ca94c84f12c38c4f0.js";
// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
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
export async function frame(input, state, rt) {
  K ??= ["#0f0f11", Float64Array.of(0, 0, 0), 1, 0, Float64Array.of(0, 0, 0), 1, 0, Float64Array.of(0, 0, 0), 1, 0, Float64Array.of(0, 0, 0), 1, 0, Float64Array.of(0, 0, 0), 1, 0, Float64Array.of(0, 0, 0), 1, 0];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = K[1];
    at = 3; l2 = K[2];
    at = 4; l3 = K[3];
    at = 5; { const resource = await rt.image(1); const [x, y] = point(rt, l1), scale = num(rt, l2), angle = num(rt, l3); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 6; l4 = K[4];
    at = 7; l5 = K[5];
    at = 8; l6 = K[6];
    at = 9; { const resource = await rt.image(3); const [x, y] = point(rt, l4), scale = num(rt, l5), angle = num(rt, l6); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 10; l7 = K[7];
    at = 11; l8 = K[8];
    at = 12; l9 = K[9];
    at = 13; { const resource = await rt.image(5); const [x, y] = point(rt, l7), scale = num(rt, l8), angle = num(rt, l9); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 14; l10 = K[10];
    at = 15; l11 = K[11];
    at = 16; l12 = K[12];
    at = 17; { const resource = await rt.image(7); const [x, y] = point(rt, l10), scale = num(rt, l11), angle = num(rt, l12); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 18; l13 = K[13];
    at = 19; l14 = K[14];
    at = 20; l15 = K[15];
    at = 21; { const resource = await rt.image(9); const [x, y] = point(rt, l13), scale = num(rt, l14), angle = num(rt, l15); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 22; l16 = K[16];
    at = 23; l17 = K[17];
    at = 24; l18 = K[18];
    at = 25; { const resource = await rt.image(11); const [x, y] = point(rt, l16), scale = num(rt, l17), angle = num(rt, l18); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
export const images = {'1': {kind: 'map', kernel: 'image1', args: image1, frame: [], animated: true},
  '3': {kind: 'map', kernel: 'image3', args: image3, frame: [], animated: true},
  '5': {kind: 'map', kernel: 'image5', args: image5, frame: [], animated: true},
  '7': {kind: 'map', kernel: 'image7', args: image7, frame: [], animated: true},
  '9': {kind: 'map', kernel: 'image9', args: image9, frame: [], animated: true},
  '11': {kind: 'map', kernel: 'image11', args: image11, frame: [], animated: true}};
