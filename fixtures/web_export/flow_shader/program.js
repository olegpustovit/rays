import * as image1 from "./image1-998c2f3399b6f986a423e4f8a735b89c.js";
import * as image3 from "./image3-659ed3bf4f808ee13cdbe58276b68ab5.js";
import * as image5 from "./image5-41b9399e56ac9939554c28ba647ffdb8.js";
import * as image7 from "./image7-659ed3bf4f808ee13cdbe58276b68ab5.js";
import * as image10 from "./image10-998c2f3399b6f986a423e4f8a735b89c.js";
import * as image12 from "./image12-db19aaa8e6fa73cd6944fd024131ae51.js";
import * as image14 from "./image14-db19aaa8e6fa73cd6944fd024131ae51.js";
import * as image16 from "./image16-db19aaa8e6fa73cd6944fd024131ae51.js";
import * as image19 from "./image19-998c2f3399b6f986a423e4f8a735b89c.js";
import * as image21 from "./image21-5315e6fb690f7f672d4e88eb5e1c4699.js";
import * as image23 from "./image23-5315e6fb690f7f672d4e88eb5e1c4699.js";
import * as image25 from "./image25-5315e6fb690f7f672d4e88eb5e1c4699.js";
import * as image28 from "./image28-998c2f3399b6f986a423e4f8a735b89c.js";
import * as image30 from "./image30-1c6c88a455412fa3c8729123739896e6.js";
import * as image32 from "./image32-1c6c88a455412fa3c8729123739896e6.js";
import * as image34 from "./image34-1c6c88a455412fa3c8729123739896e6.js";
import * as image37 from "./image37-998c2f3399b6f986a423e4f8a735b89c.js";
import * as image39 from "./image39-8d241b97f9b371bfe94dfdf185c14466.js";
import * as image41 from "./image41-f9d1b410e5eeb25dd25e97541ebdbd4c.js";
import * as image43 from "./image43-8d241b97f9b371bfe94dfdf185c14466.js";
import * as image46 from "./image46-8d241b97f9b371bfe94dfdf185c14466.js";
import * as image49 from "./image49-998c2f3399b6f986a423e4f8a735b89c.js";
import * as image51 from "./image51-207b3fcc28f70e80d804c8b750692768.js";
import * as image53 from "./image53-207b3fcc28f70e80d804c8b750692768.js";
import * as image55 from "./image55-207b3fcc28f70e80d804c8b750692768.js";
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
  K ??= ["#0b0b0e", Float64Array.of(8, 8, 0), 1, 0, Float64Array.of(8, 8, 0), 1, 0, Float64Array.of(8, 8, 0), 1, 0, Float64Array.of(8, 8, 0), 1, 0, Float64Array.of(372, 8, 0), 1, 0, Float64Array.of(372, 8, 0), 1, 0, Float64Array.of(372, 8, 0), 1, 0, Float64Array.of(372, 8, 0), 1, 0, Float64Array.of(8, 256, 0), 1, 0, Float64Array.of(8, 256, 0), 1, 0, Float64Array.of(8, 256, 0), 1, 0, Float64Array.of(8, 256, 0), 1, 0, Float64Array.of(372, 256, 0), 1, 0, Float64Array.of(372, 256, 0), 1, 0, Float64Array.of(372, 256, 0), 1, 0, Float64Array.of(372, 256, 0), 1, 0, Float64Array.of(8, 504, 0), 1, 0, Float64Array.of(8, 504, 0), 1, 0, Float64Array.of(8, 504, 0), 1, 0, Float64Array.of(8, 504, 0), 1, 0, Float64Array.of(8, 504, 0), 1, 0, Float64Array.of(372, 504, 0), 1, 0, Float64Array.of(372, 504, 0), 1, 0, Float64Array.of(372, 504, 0), 1, 0, Float64Array.of(372, 504, 0), 1, 0];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75;
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
    at = 21; { const resource = await rt.image(10); const [x, y] = point(rt, l13), scale = num(rt, l14), angle = num(rt, l15); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 22; l16 = K[16];
    at = 23; l17 = K[17];
    at = 24; l18 = K[18];
    at = 25; { const resource = await rt.image(12); const [x, y] = point(rt, l16), scale = num(rt, l17), angle = num(rt, l18); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 26; l19 = K[19];
    at = 27; l20 = K[20];
    at = 28; l21 = K[21];
    at = 29; { const resource = await rt.image(14); const [x, y] = point(rt, l19), scale = num(rt, l20), angle = num(rt, l21); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 30; l22 = K[22];
    at = 31; l23 = K[23];
    at = 32; l24 = K[24];
    at = 33; { const resource = await rt.image(16); const [x, y] = point(rt, l22), scale = num(rt, l23), angle = num(rt, l24); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 34; l25 = K[25];
    at = 35; l26 = K[26];
    at = 36; l27 = K[27];
    at = 37; { const resource = await rt.image(19); const [x, y] = point(rt, l25), scale = num(rt, l26), angle = num(rt, l27); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 38; l28 = K[28];
    at = 39; l29 = K[29];
    at = 40; l30 = K[30];
    at = 41; { const resource = await rt.image(21); const [x, y] = point(rt, l28), scale = num(rt, l29), angle = num(rt, l30); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 42; l31 = K[31];
    at = 43; l32 = K[32];
    at = 44; l33 = K[33];
    at = 45; { const resource = await rt.image(23); const [x, y] = point(rt, l31), scale = num(rt, l32), angle = num(rt, l33); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 46; l34 = K[34];
    at = 47; l35 = K[35];
    at = 48; l36 = K[36];
    at = 49; { const resource = await rt.image(25); const [x, y] = point(rt, l34), scale = num(rt, l35), angle = num(rt, l36); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 50; l37 = K[37];
    at = 51; l38 = K[38];
    at = 52; l39 = K[39];
    at = 53; { const resource = await rt.image(28); const [x, y] = point(rt, l37), scale = num(rt, l38), angle = num(rt, l39); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 54; l40 = K[40];
    at = 55; l41 = K[41];
    at = 56; l42 = K[42];
    at = 57; { const resource = await rt.image(30); const [x, y] = point(rt, l40), scale = num(rt, l41), angle = num(rt, l42); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 58; l43 = K[43];
    at = 59; l44 = K[44];
    at = 60; l45 = K[45];
    at = 61; { const resource = await rt.image(32); const [x, y] = point(rt, l43), scale = num(rt, l44), angle = num(rt, l45); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 62; l46 = K[46];
    at = 63; l47 = K[47];
    at = 64; l48 = K[48];
    at = 65; { const resource = await rt.image(34); const [x, y] = point(rt, l46), scale = num(rt, l47), angle = num(rt, l48); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 66; l49 = K[49];
    at = 67; l50 = K[50];
    at = 68; l51 = K[51];
    at = 69; { const resource = await rt.image(37); const [x, y] = point(rt, l49), scale = num(rt, l50), angle = num(rt, l51); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 70; l52 = K[52];
    at = 71; l53 = K[53];
    at = 72; l54 = K[54];
    at = 73; { const resource = await rt.image(39); const [x, y] = point(rt, l52), scale = num(rt, l53), angle = num(rt, l54); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 74; l55 = K[55];
    at = 75; l56 = K[56];
    at = 76; l57 = K[57];
    at = 77; { const resource = await rt.image(41); const [x, y] = point(rt, l55), scale = num(rt, l56), angle = num(rt, l57); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 78; l58 = K[58];
    at = 79; l59 = K[59];
    at = 80; l60 = K[60];
    at = 81; { const resource = await rt.image(43); const [x, y] = point(rt, l58), scale = num(rt, l59), angle = num(rt, l60); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 82; l61 = K[61];
    at = 83; l62 = K[62];
    at = 84; l63 = K[63];
    at = 85; { const resource = await rt.image(46); const [x, y] = point(rt, l61), scale = num(rt, l62), angle = num(rt, l63); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 86; l64 = K[64];
    at = 87; l65 = K[65];
    at = 88; l66 = K[66];
    at = 89; { const resource = await rt.image(49); const [x, y] = point(rt, l64), scale = num(rt, l65), angle = num(rt, l66); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 90; l67 = K[67];
    at = 91; l68 = K[68];
    at = 92; l69 = K[69];
    at = 93; { const resource = await rt.image(51); const [x, y] = point(rt, l67), scale = num(rt, l68), angle = num(rt, l69); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 94; l70 = K[70];
    at = 95; l71 = K[71];
    at = 96; l72 = K[72];
    at = 97; { const resource = await rt.image(53); const [x, y] = point(rt, l70), scale = num(rt, l71), angle = num(rt, l72); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
    at = 98; l73 = K[73];
    at = 99; l74 = K[74];
    at = 100; l75 = K[75];
    at = 101; { const resource = await rt.image(55); const [x, y] = point(rt, l73), scale = num(rt, l74), angle = num(rt, l75); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= 0) rt.fail("E_DRAW_RANGE", "Scene.image: invalid transform"); emit(rt, {type: "image", resource, x, y, scale, angle}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
export const images = {'1': {kind: 'map', kernel: 'image1', args: image1, frame: [], animated: false},
  '3': {kind: 'map', kernel: 'image3', args: image3, frame: [], animated: true},
  '5': {kind: 'map', kernel: 'image5', args: image5, frame: [], animated: true},
  '7': {kind: 'map', kernel: 'image7', args: image7, frame: [], animated: true},
  '10': {kind: 'map', kernel: 'image10', args: image10, frame: [], animated: false},
  '12': {kind: 'map', kernel: 'image12', args: image12, frame: [], animated: true},
  '14': {kind: 'map', kernel: 'image14', args: image14, frame: [], animated: true},
  '16': {kind: 'map', kernel: 'image16', args: image16, frame: [], animated: true},
  '19': {kind: 'map', kernel: 'image19', args: image19, frame: [], animated: false},
  '21': {kind: 'map', kernel: 'image21', args: image21, frame: [], animated: true},
  '23': {kind: 'map', kernel: 'image23', args: image23, frame: [], animated: true},
  '25': {kind: 'map', kernel: 'image25', args: image25, frame: [], animated: true},
  '28': {kind: 'map', kernel: 'image28', args: image28, frame: [], animated: false},
  '30': {kind: 'map', kernel: 'image30', args: image30, frame: [], animated: true},
  '32': {kind: 'map', kernel: 'image32', args: image32, frame: [], animated: true},
  '34': {kind: 'map', kernel: 'image34', args: image34, frame: [], animated: true},
  '37': {kind: 'map', kernel: 'image37', args: image37, frame: [], animated: false},
  '39': {kind: 'map', kernel: 'image39', args: image39, frame: [], animated: true},
  '41': {kind: 'map', kernel: 'image41', args: image41, frame: [], animated: true},
  '43': {kind: 'map', kernel: 'image43', args: image43, frame: [], animated: true},
  '46': {kind: 'map', kernel: 'image46', args: image46, frame: [], animated: true},
  '49': {kind: 'map', kernel: 'image49', args: image49, frame: [], animated: false},
  '51': {kind: 'map', kernel: 'image51', args: image51, frame: [], animated: true},
  '53': {kind: 'map', kernel: 'image53', args: image53, frame: [], animated: true},
  '55': {kind: 'map', kernel: 'image55', args: image55, frame: [], animated: true}};
