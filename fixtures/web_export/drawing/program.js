// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const floatPoint = (rt, v) => { const c = comps(rt, v);
  if (![c[0], c[1]].every(x => Number.isFinite(x) && Math.abs(x) <= 3.4028234663852886e38))
    rt.fail("E_DRAW_RANGE", "A drawing coordinate must be finite and fit a 32-bit float.");
  return [c[0], c[1]];
};
const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
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
export function frame(input, state, rt) {
  K ??= ["#0b1020", [], false, 0n, 10n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 10n, "Drag to draw", false, false, 0n, 0n, 4096n, 0n, 0n, "pointer-pressed", "left", 92n, 8n, 70n, 8n, 42n, 0n, [], 0n, 0n, [], 76n, 138n, 8n, 42n, 1n, 0n, 0n, 1n, 0n, 0n, 144n, 206n, 8n, 42n, 215n, 245n, 8n, 42n, 0, 0n, 253n, 283n, 8n, 42n, 1, 1n, 291n, 321n, 8n, 42n, 2, 2n, 329n, 359n, 8n, 42n, 3, 3n, 8n, 44n, 50n, 84n, 2n, 2n, 52n, 88n, 50n, 84n, 2n, 32n, false, false, false, false, false, false, false, false, "Saved _out/rays-drawing.png", true, false, "Undid the latest stroke", false, "Canvas cleared", 1n, true, "Drawing", "pointer-moved", 4096n, 0n, 1n, 1n, 4n, 1n, "pointer-released", "left", 4096n, 0n, 1n, 1n, 4n, 1n, 128n, 0n, 0n, 128n, 0n, 0n, 0n, false, "Drag to draw", "pointer-cancelled", "left", "focus-lost", 128n, 0n, 0n, 128n, 0n, 0n, 0n, false, "Drag to draw", "key-pressed", "c", "C", 0n, [], 0n, 0n, [], "z", "Z", 1n, 0n, 0n, 1n, 0n, 0n, "[", 2n, 2n, "]", 2n, 32n, "s", "S", "Saved _out/rays-drawing.png", true, false, "Undid the latest stroke", false, "Canvas cleared", 0n, 0n, 1n, 1n, 127n, 127n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 126n, 126n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 125n, 125n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 124n, 124n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 123n, 123n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 122n, 122n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 121n, 121n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 120n, 120n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 119n, 119n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 118n, 118n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 117n, 117n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 116n, 116n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 115n, 115n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 114n, 114n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 113n, 113n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 112n, 112n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 111n, 111n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 110n, 110n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 109n, 109n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 108n, 108n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 107n, 107n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 106n, 106n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 105n, 105n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 104n, 104n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 103n, 103n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 102n, 102n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 101n, 101n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 100n, 100n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 99n, 99n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 98n, 98n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 97n, 97n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 96n, 96n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 95n, 95n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 94n, 94n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 93n, 93n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 92n, 92n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 91n, 91n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 90n, 90n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 89n, 89n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 88n, 88n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 87n, 87n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 86n, 86n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 85n, 85n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 84n, 84n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 83n, 83n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 82n, 82n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 81n, 81n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 80n, 80n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 79n, 79n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 78n, 78n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 77n, 77n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 76n, 76n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 75n, 75n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 74n, 74n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 73n, 73n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 72n, 72n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 71n, 71n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 70n, 70n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 69n, 69n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 68n, 68n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 67n, 67n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 66n, 66n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 65n, 65n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 64n, 64n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 63n, 63n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 62n, 62n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 61n, 61n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 60n, 60n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 59n, 59n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 58n, 58n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 57n, 57n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 56n, 56n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 55n, 55n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 54n, 54n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 53n, 53n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 52n, 52n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 51n, 51n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 50n, 50n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 49n, 49n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 48n, 48n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 47n, 47n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 46n, 46n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 45n, 45n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 44n, 44n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 43n, 43n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 42n, 42n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 41n, 41n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 40n, 40n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 39n, 39n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 38n, 38n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 37n, 37n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 36n, 36n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 35n, 35n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 34n, 34n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 33n, 33n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 32n, 32n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 31n, 31n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 30n, 30n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 29n, 29n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 28n, 28n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 27n, 27n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 26n, 26n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 25n, 25n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 24n, 24n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 23n, 23n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 22n, 22n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 21n, 21n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 20n, 20n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 19n, 19n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 18n, 18n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 17n, 17n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 16n, 16n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 15n, 15n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 14n, 14n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 13n, 13n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 12n, 12n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 11n, 11n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 10n, 10n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 9n, 9n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 8n, 8n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 7n, 7n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 6n, 6n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 5n, 5n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 4n, 4n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 3n, 3n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 2n, 2n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 1n, 1n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, Float64Array.of(0, 0, 0), 92n, 0n, [0.03529411764705882, 0.054901960784313725, 0.09803921568627451, 0.9607843137254902], Float64Array.of(8, 8, 0), Float64Array.of(62, 34, 0), 6, "#1f2937", "#475569", Float64Array.of(20, 16, 0), "Clear", 14, Float64Array.of(1, 1, 1), Float64Array.of(76, 8, 0), Float64Array.of(62, 34, 0), 6, "#1f2937", "#475569", Float64Array.of(88, 16, 0), "Undo", 14, Float64Array.of(1, 1, 1), Float64Array.of(144, 8, 0), Float64Array.of(62, 34, 0), 6, "#1f2937", "#475569", Float64Array.of(156, 16, 0), "Save", 14, Float64Array.of(1, 1, 1), Float64Array.of(230, 25, 0), 0n, 12n, 9n, "#22d3ee", "#ffffff", Float64Array.of(268, 25, 0), 1n, 12n, 9n, "#fb7185", "#ffffff", Float64Array.of(306, 25, 0), 2n, 12n, 9n, "#fbbf24", "#ffffff", Float64Array.of(344, 25, 0), 3n, 12n, 9n, "#a78bfa", "#ffffff", Float64Array.of(8, 50, 0), Float64Array.of(36, 34, 0), 6, "#1f2937", "#475569", Float64Array.of(20, 58, 0), "−", 14, Float64Array.of(1, 1, 1), Float64Array.of(52, 50, 0), Float64Array.of(36, 34, 0), 6, "#1f2937", "#475569", Float64Array.of(64, 58, 0), "+", 14, Float64Array.of(1, 1, 1), Float64Array.of(112, 67, 0), 2n, 1n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], Float64Array.of(136, 58, 0), " px · draw · S save", 13, Float64Array.of(1, 1, 1), 26n, 12n, 0n, 13, [0.8862745098039215, 0.9098039215686274, 0.9411764705882353, 0.7450980392156863]];
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
  let l192, l193, l194, l195, l196, l197, l198, l199, l200, l201, l202, l203, l204, l205, l206, l207;
  let l208, l209, l210, l211, l212, l213, l214, l215, l216, l217, l218, l219, l220, l221, l222, l223;
  let l224, l225, l226, l227, l228, l229, l230, l231, l232, l233, l234, l235, l236, l237, l238, l239;
  let l240, l241, l242, l243, l244, l245, l246, l247, l248, l249, l250, l251, l252, l253, l254, l255;
  let l256, l257, l258, l259, l260, l261, l262, l263, l264, l265, l266, l267, l268, l269, l270, l271;
  let l272, l273, l274, l275, l276, l277, l278, l279, l280, l281, l282, l283, l284, l285, l286, l287;
  let l288, l289, l290, l291, l292, l293, l294, l295, l296, l297, l298, l299, l300, l301, l302, l303;
  let l304, l305, l306, l307, l308, l309, l310, l311, l312, l313, l314, l315, l316, l317, l318, l319;
  let l320, l321, l322, l323, l324, l325, l326, l327, l328, l329, l330, l331, l332, l333, l334, l335;
  let l336, l337, l338, l339, l340, l341, l342, l343, l344, l345, l346, l347, l348, l349, l350, l351;
  let l352, l353, l354, l355, l356, l357, l358, l359, l360, l361, l362, l363, l364, l365, l366, l367;
  let l368, l369, l370, l371, l372, l373, l374, l375, l376, l377, l378, l379, l380, l381, l382, l383;
  let l384, l385, l386, l387, l388, l389, l390, l391, l392, l393, l394, l395, l396, l397, l398, l399;
  let l400, l401, l402, l403, l404, l405, l406, l407, l408, l409, l410, l411, l412, l413, l414, l415;
  let l416, l417, l418, l419, l420, l421, l422, l423, l424, l425, l426, l427, l428, l429, l430, l431;
  let l432, l433, l434, l435, l436, l437, l438, l439, l440, l441, l442, l443, l444, l445, l446, l447;
  let l448, l449, l450, l451, l452, l453, l454, l455, l456, l457, l458, l459, l460, l461, l462, l463;
  let l464, l465, l466, l467, l468, l469, l470, l471, l472, l473, l474, l475, l476, l477, l478, l479;
  let l480, l481, l482, l483, l484, l485, l486, l487, l488, l489, l490, l491, l492, l493, l494, l495;
  let l496, l497, l498, l499, l500, l501, l502, l503, l504, l505, l506, l507, l508, l509, l510, l511;
  let l512, l513, l514, l515, l516, l517, l518, l519, l520, l521, l522, l523, l524, l525, l526, l527;
  let l528, l529, l530, l531, l532, l533, l534, l535, l536, l537, l538, l539, l540, l541, l542, l543;
  let l544, l545, l546, l547, l548, l549, l550, l551, l552, l553, l554, l555, l556, l557, l558, l559;
  let l560, l561, l562, l563, l564, l565, l566, l567, l568, l569, l570, l571, l572, l573, l574, l575;
  let l576, l577, l578, l579, l580, l581, l582, l583, l584, l585, l586, l587, l588, l589, l590, l591;
  let l592, l593, l594, l595, l596, l597, l598, l599, l600, l601, l602, l603, l604, l605, l606, l607;
  let l608, l609, l610, l611, l612, l613, l614, l615, l616, l617, l618, l619, l620, l621, l622, l623;
  let l624, l625, l626, l627, l628, l629, l630, l631, l632, l633, l634, l635, l636, l637, l638, l639;
  let l640, l641, l642, l643, l644, l645, l646, l647, l648, l649, l650, l651, l652, l653, l654, l655;
  let l656, l657, l658, l659, l660, l661, l662, l663, l664, l665, l666, l667, l668, l669, l670, l671;
  let l672, l673, l674, l675, l676, l677, l678, l679, l680, l681, l682, l683, l684, l685, l686, l687;
  let l688, l689, l690, l691, l692, l693, l694, l695, l696, l697, l698, l699, l700, l701, l702, l703;
  let l704, l705, l706, l707, l708, l709, l710, l711, l712, l713, l714, l715, l716, l717, l718, l719;
  let l720, l721, l722, l723, l724, l725, l726, l727, l728, l729, l730, l731, l732, l733, l734, l735;
  let l736, l737, l738, l739, l740, l741, l742, l743, l744, l745, l746, l747, l748, l749, l750, l751;
  let l752, l753, l754, l755, l756, l757, l758, l759, l760, l761, l762, l763, l764, l765, l766, l767;
  let l768, l769, l770, l771, l772, l773, l774, l775, l776, l777, l778, l779, l780, l781, l782, l783;
  let l784, l785, l786, l787, l788, l789, l790, l791, l792, l793, l794, l795, l796, l797, l798, l799;
  let l800, l801, l802, l803, l804, l805, l806, l807, l808, l809, l810, l811, l812, l813, l814, l815;
  let l816, l817, l818, l819, l820, l821, l822, l823, l824, l825, l826, l827, l828, l829, l830, l831;
  let l832, l833, l834, l835, l836, l837, l838, l839, l840, l841, l842, l843, l844, l845, l846, l847;
  let l848, l849, l850, l851, l852, l853, l854, l855, l856, l857, l858, l859, l860, l861, l862, l863;
  let l864, l865, l866, l867, l868, l869, l870, l871, l872, l873, l874, l875, l876, l877, l878, l879;
  let l880, l881, l882, l883, l884, l885, l886, l887, l888, l889, l890, l891, l892, l893, l894, l895;
  let l896, l897, l898, l899, l900, l901, l902, l903, l904, l905, l906, l907, l908, l909, l910, l911;
  let l912, l913, l914, l915, l916, l917, l918, l919, l920, l921, l922, l923, l924, l925, l926, l927;
  let l928, l929, l930, l931, l932, l933, l934, l935, l936, l937, l938, l939, l940, l941, l942, l943;
  let l944, l945, l946, l947, l948, l949, l950, l951, l952, l953, l954, l955, l956, l957, l958, l959;
  let l960, l961, l962, l963, l964, l965, l966, l967, l968, l969, l970, l971, l972, l973, l974, l975;
  let l976, l977, l978, l979, l980, l981, l982, l983, l984, l985, l986, l987, l988, l989, l990, l991;
  let l992, l993, l994, l995, l996, l997, l998, l999, l1000, l1001, l1002, l1003, l1004, l1005, l1006, l1007;
  let l1008, l1009, l1010, l1011, l1012, l1013, l1014, l1015, l1016, l1017, l1018, l1019, l1020, l1021, l1022, l1023;
  let l1024, l1025, l1026, l1027, l1028, l1029, l1030, l1031, l1032, l1033, l1034, l1035, l1036, l1037, l1038, l1039;
  let l1040, l1041, l1042, l1043, l1044, l1045, l1046, l1047, l1048, l1049, l1050, l1051, l1052, l1053, l1054, l1055;
  let l1056, l1057, l1058, l1059, l1060, l1061, l1062, l1063, l1064, l1065, l1066, l1067, l1068, l1069, l1070, l1071;
  let l1072, l1073, l1074, l1075, l1076, l1077, l1078, l1079, l1080, l1081, l1082, l1083, l1084, l1085, l1086, l1087;
  let l1088, l1089, l1090, l1091, l1092, l1093, l1094, l1095, l1096, l1097, l1098, l1099, l1100, l1101, l1102, l1103;
  let l1104, l1105, l1106, l1107, l1108, l1109, l1110, l1111, l1112, l1113, l1114, l1115, l1116, l1117, l1118, l1119;
  let l1120, l1121, l1122, l1123, l1124, l1125, l1126, l1127, l1128, l1129, l1130, l1131, l1132, l1133, l1134, l1135;
  let l1136, l1137, l1138, l1139, l1140, l1141, l1142, l1143, l1144, l1145, l1146, l1147, l1148, l1149, l1150, l1151;
  let l1152, l1153, l1154, l1155, l1156, l1157, l1158, l1159, l1160, l1161, l1162, l1163, l1164, l1165, l1166, l1167;
  let l1168, l1169, l1170, l1171, l1172, l1173, l1174, l1175, l1176, l1177, l1178, l1179, l1180, l1181, l1182, l1183;
  let l1184, l1185, l1186, l1187, l1188, l1189, l1190, l1191, l1192, l1193, l1194, l1195, l1196, l1197, l1198, l1199;
  let l1200, l1201, l1202, l1203, l1204, l1205, l1206, l1207, l1208, l1209, l1210, l1211, l1212, l1213, l1214, l1215;
  let l1216, l1217, l1218, l1219, l1220, l1221, l1222, l1223, l1224, l1225, l1226, l1227, l1228, l1229, l1230, l1231;
  let l1232, l1233, l1234, l1235, l1236, l1237, l1238, l1239, l1240, l1241, l1242, l1243, l1244, l1245, l1246, l1247;
  let l1248, l1249, l1250, l1251, l1252, l1253, l1254, l1255, l1256, l1257, l1258, l1259, l1260, l1261, l1262, l1263;
  let l1264, l1265, l1266, l1267, l1268, l1269, l1270, l1271, l1272, l1273, l1274, l1275, l1276, l1277, l1278, l1279;
  let l1280, l1281, l1282, l1283, l1284, l1285, l1286, l1287, l1288, l1289, l1290, l1291, l1292, l1293, l1294, l1295;
  let l1296, l1297, l1298, l1299, l1300, l1301, l1302, l1303, l1304, l1305, l1306, l1307, l1308, l1309, l1310, l1311;
  let l1312, l1313, l1314, l1315, l1316, l1317, l1318, l1319, l1320, l1321, l1322, l1323, l1324, l1325, l1326, l1327;
  let l1328, l1329, l1330, l1331, l1332, l1333, l1334, l1335, l1336, l1337, l1338, l1339, l1340, l1341, l1342, l1343;
  let l1344, l1345, l1346, l1347, l1348, l1349, l1350, l1351, l1352, l1353, l1354, l1355, l1356, l1357, l1358, l1359;
  let l1360, l1361, l1362, l1363, l1364, l1365, l1366, l1367, l1368, l1369, l1370, l1371, l1372, l1373, l1374, l1375;
  let l1376, l1377, l1378, l1379, l1380, l1381, l1382, l1383, l1384, l1385, l1386, l1387, l1388, l1389, l1390, l1391;
  let l1392, l1393, l1394, l1395, l1396, l1397, l1398, l1399, l1400, l1401, l1402, l1403, l1404, l1405, l1406, l1407;
  let l1408, l1409, l1410, l1411, l1412, l1413, l1414, l1415, l1416, l1417, l1418, l1419, l1420, l1421, l1422, l1423;
  let l1424, l1425, l1426, l1427, l1428, l1429, l1430, l1431, l1432, l1433, l1434, l1435, l1436, l1437, l1438, l1439;
  let l1440, l1441, l1442, l1443, l1444, l1445, l1446, l1447, l1448, l1449, l1450, l1451, l1452, l1453, l1454, l1455;
  let l1456, l1457, l1458, l1459, l1460, l1461, l1462, l1463, l1464, l1465, l1466, l1467, l1468, l1469, l1470, l1471;
  let l1472, l1473, l1474, l1475, l1476, l1477, l1478, l1479, l1480, l1481, l1482, l1483, l1484, l1485, l1486, l1487;
  let l1488, l1489, l1490, l1491, l1492, l1493, l1494, l1495, l1496, l1497, l1498, l1499, l1500, l1501, l1502, l1503;
  let l1504, l1505, l1506, l1507, l1508, l1509, l1510, l1511, l1512, l1513, l1514, l1515, l1516, l1517, l1518, l1519;
  let l1520, l1521, l1522, l1523, l1524, l1525, l1526, l1527, l1528, l1529, l1530, l1531, l1532, l1533, l1534, l1535;
  let l1536, l1537, l1538, l1539, l1540, l1541, l1542, l1543, l1544, l1545, l1546, l1547, l1548, l1549, l1550, l1551;
  let l1552, l1553, l1554, l1555, l1556, l1557, l1558, l1559, l1560, l1561, l1562, l1563, l1564, l1565, l1566, l1567;
  let l1568, l1569, l1570, l1571, l1572, l1573, l1574, l1575, l1576, l1577, l1578, l1579, l1580, l1581, l1582, l1583;
  let l1584, l1585, l1586, l1587, l1588, l1589, l1590, l1591, l1592, l1593, l1594, l1595, l1596, l1597, l1598, l1599;
  let l1600, l1601, l1602, l1603, l1604, l1605, l1606, l1607, l1608, l1609, l1610, l1611, l1612, l1613, l1614, l1615;
  let l1616, l1617, l1618, l1619, l1620, l1621, l1622, l1623, l1624, l1625, l1626, l1627, l1628, l1629, l1630, l1631;
  let l1632, l1633, l1634, l1635, l1636, l1637, l1638, l1639, l1640, l1641, l1642, l1643, l1644, l1645, l1646, l1647;
  let l1648, l1649, l1650, l1651, l1652, l1653, l1654, l1655, l1656, l1657, l1658, l1659, l1660, l1661, l1662, l1663;
  let l1664, l1665, l1666, l1667, l1668, l1669, l1670, l1671, l1672, l1673, l1674, l1675, l1676, l1677, l1678, l1679;
  let l1680, l1681, l1682, l1683, l1684, l1685, l1686, l1687, l1688, l1689, l1690, l1691, l1692, l1693, l1694, l1695;
  let l1696, l1697, l1698, l1699, l1700, l1701, l1702, l1703, l1704, l1705, l1706, l1707, l1708, l1709, l1710, l1711;
  let l1712, l1713, l1714, l1715, l1716, l1717, l1718, l1719, l1720, l1721, l1722, l1723, l1724, l1725, l1726, l1727;
  let l1728, l1729, l1730, l1731, l1732, l1733, l1734, l1735, l1736, l1737, l1738, l1739, l1740, l1741, l1742, l1743;
  let l1744, l1745, l1746, l1747, l1748, l1749, l1750, l1751, l1752, l1753, l1754, l1755, l1756, l1757, l1758, l1759;
  let l1760, l1761, l1762, l1763, l1764, l1765, l1766, l1767, l1768, l1769, l1770, l1771, l1772, l1773, l1774, l1775;
  let l1776, l1777, l1778, l1779, l1780, l1781, l1782, l1783, l1784, l1785, l1786, l1787, l1788, l1789, l1790, l1791;
  let l1792, l1793, l1794, l1795, l1796, l1797, l1798, l1799, l1800, l1801, l1802, l1803, l1804, l1805, l1806, l1807;
  let l1808, l1809, l1810, l1811, l1812, l1813, l1814, l1815, l1816, l1817, l1818, l1819, l1820, l1821, l1822, l1823;
  let l1824, l1825, l1826, l1827, l1828, l1829, l1830, l1831, l1832, l1833, l1834, l1835, l1836, l1837, l1838, l1839;
  let l1840, l1841, l1842, l1843, l1844, l1845, l1846, l1847, l1848, l1849, l1850, l1851, l1852, l1853, l1854, l1855;
  let l1856, l1857, l1858, l1859, l1860, l1861, l1862, l1863, l1864, l1865, l1866, l1867, l1868, l1869, l1870, l1871;
  let l1872, l1873, l1874, l1875, l1876, l1877, l1878, l1879, l1880, l1881, l1882, l1883, l1884, l1885, l1886, l1887;
  let l1888, l1889, l1890, l1891, l1892, l1893, l1894, l1895, l1896, l1897, l1898, l1899, l1900, l1901, l1902, l1903;
  let l1904, l1905, l1906, l1907, l1908, l1909, l1910, l1911, l1912, l1913, l1914, l1915, l1916, l1917, l1918, l1919;
  let l1920, l1921, l1922, l1923, l1924, l1925, l1926, l1927, l1928, l1929, l1930, l1931, l1932, l1933, l1934, l1935;
  let l1936, l1937, l1938, l1939, l1940, l1941, l1942, l1943, l1944, l1945, l1946, l1947, l1948, l1949, l1950, l1951;
  let l1952, l1953, l1954, l1955, l1956, l1957, l1958, l1959, l1960, l1961, l1962, l1963, l1964, l1965, l1966, l1967;
  let l1968, l1969, l1970, l1971, l1972, l1973, l1974, l1975, l1976, l1977, l1978, l1979, l1980, l1981, l1982, l1983;
  let l1984, l1985, l1986, l1987, l1988, l1989, l1990, l1991, l1992, l1993, l1994, l1995, l1996, l1997, l1998, l1999;
  let l2000, l2001, l2002, l2003, l2004, l2005, l2006, l2007, l2008, l2009, l2010, l2011, l2012, l2013, l2014, l2015;
  let l2016, l2017, l2018, l2019, l2020, l2021, l2022, l2023, l2024, l2025, l2026, l2027, l2028, l2029, l2030, l2031;
  let l2032, l2033, l2034, l2035, l2036, l2037, l2038, l2039, l2040, l2041, l2042, l2043, l2044, l2045, l2046, l2047;
  let l2048, l2049, l2050, l2051, l2052, l2053, l2054, l2055, l2056, l2057, l2058, l2059, l2060, l2061, l2062, l2063;
  let l2064, l2065, l2066, l2067, l2068, l2069, l2070, l2071, l2072, l2073, l2074, l2075, l2076, l2077, l2078, l2079;
  let l2080, l2081, l2082, l2083, l2084, l2085, l2086, l2087, l2088, l2089, l2090, l2091, l2092, l2093, l2094, l2095;
  let l2096, l2097, l2098, l2099, l2100, l2101, l2102, l2103, l2104, l2105, l2106, l2107, l2108, l2109, l2110, l2111;
  let l2112, l2113, l2114, l2115, l2116, l2117, l2118, l2119, l2120, l2121, l2122, l2123, l2124, l2125, l2126, l2127;
  let l2128, l2129, l2130, l2131, l2132, l2133, l2134, l2135, l2136, l2137, l2138, l2139, l2140, l2141, l2142, l2143;
  let l2144, l2145, l2146, l2147, l2148, l2149, l2150, l2151, l2152, l2153, l2154, l2155, l2156, l2157, l2158, l2159;
  let l2160, l2161, l2162, l2163, l2164, l2165, l2166, l2167, l2168, l2169, l2170, l2171, l2172, l2173, l2174, l2175;
  let l2176, l2177, l2178, l2179, l2180, l2181, l2182, l2183, l2184, l2185, l2186, l2187, l2188, l2189, l2190, l2191;
  let l2192, l2193, l2194, l2195, l2196, l2197, l2198, l2199, l2200, l2201, l2202, l2203, l2204, l2205, l2206, l2207;
  let l2208, l2209, l2210, l2211, l2212, l2213, l2214, l2215, l2216, l2217, l2218, l2219, l2220, l2221, l2222, l2223;
  let l2224, l2225, l2226, l2227, l2228, l2229, l2230, l2231, l2232, l2233, l2234, l2235, l2236, l2237, l2238, l2239;
  let l2240, l2241, l2242, l2243, l2244, l2245, l2246, l2247, l2248, l2249, l2250, l2251, l2252, l2253, l2254, l2255;
  let l2256, l2257, l2258, l2259, l2260, l2261, l2262, l2263, l2264, l2265, l2266, l2267, l2268, l2269, l2270, l2271;
  let l2272, l2273, l2274, l2275, l2276, l2277, l2278, l2279, l2280, l2281, l2282, l2283, l2284, l2285, l2286, l2287;
  let l2288, l2289, l2290, l2291, l2292, l2293, l2294, l2295, l2296, l2297, l2298, l2299, l2300, l2301, l2302, l2303;
  let l2304, l2305, l2306, l2307, l2308, l2309, l2310, l2311, l2312, l2313, l2314, l2315, l2316, l2317, l2318, l2319;
  let l2320, l2321, l2322, l2323, l2324, l2325, l2326, l2327, l2328, l2329, l2330, l2331, l2332, l2333, l2334, l2335;
  let l2336, l2337, l2338, l2339, l2340, l2341, l2342, l2343, l2344, l2345, l2346, l2347, l2348, l2349, l2350, l2351;
  let l2352, l2353, l2354, l2355, l2356, l2357, l2358, l2359, l2360, l2361, l2362, l2363, l2364, l2365, l2366, l2367;
  let l2368, l2369, l2370, l2371, l2372, l2373, l2374, l2375, l2376, l2377, l2378, l2379, l2380, l2381, l2382, l2383;
  let l2384, l2385, l2386, l2387, l2388, l2389, l2390, l2391, l2392, l2393, l2394, l2395, l2396, l2397, l2398, l2399;
  let l2400, l2401, l2402, l2403, l2404, l2405, l2406, l2407, l2408, l2409, l2410, l2411, l2412, l2413, l2414, l2415;
  let l2416, l2417, l2418, l2419, l2420, l2421, l2422, l2423, l2424, l2425, l2426, l2427, l2428, l2429, l2430, l2431;
  let l2432, l2433, l2434, l2435, l2436, l2437, l2438, l2439, l2440, l2441, l2442, l2443, l2444, l2445, l2446, l2447;
  let l2448, l2449, l2450, l2451, l2452, l2453, l2454, l2455, l2456, l2457, l2458, l2459, l2460, l2461, l2462, l2463;
  let l2464, l2465, l2466, l2467, l2468, l2469, l2470, l2471, l2472, l2473, l2474, l2475, l2476, l2477, l2478, l2479;
  let l2480, l2481, l2482, l2483, l2484, l2485, l2486, l2487, l2488, l2489, l2490, l2491, l2492, l2493, l2494, l2495;
  let l2496, l2497, l2498, l2499, l2500, l2501, l2502, l2503, l2504, l2505, l2506, l2507, l2508, l2509, l2510, l2511;
  let l2512, l2513, l2514, l2515, l2516, l2517, l2518, l2519, l2520, l2521, l2522, l2523, l2524, l2525, l2526, l2527;
  let l2528, l2529, l2530, l2531, l2532, l2533, l2534, l2535, l2536, l2537, l2538, l2539, l2540, l2541, l2542, l2543;
  let l2544, l2545, l2546, l2547, l2548, l2549, l2550, l2551, l2552, l2553, l2554, l2555, l2556, l2557, l2558, l2559;
  let l2560, l2561, l2562, l2563, l2564, l2565, l2566, l2567, l2568, l2569, l2570, l2571, l2572, l2573, l2574, l2575;
  let l2576, l2577, l2578, l2579, l2580, l2581, l2582, l2583, l2584, l2585, l2586, l2587, l2588, l2589, l2590, l2591;
  let l2592, l2593, l2594, l2595, l2596, l2597, l2598, l2599, l2600, l2601, l2602, l2603, l2604, l2605, l2606, l2607;
  let l2608, l2609, l2610, l2611, l2612, l2613, l2614, l2615, l2616, l2617, l2618, l2619, l2620, l2621, l2622, l2623;
  let l2624, l2625, l2626, l2627, l2628, l2629, l2630, l2631, l2632, l2633, l2634, l2635, l2636, l2637, l2638, l2639;
  let l2640, l2641, l2642, l2643, l2644, l2645, l2646, l2647, l2648, l2649, l2650, l2651, l2652, l2653, l2654, l2655;
  let l2656, l2657, l2658, l2659, l2660, l2661, l2662, l2663, l2664, l2665, l2666, l2667, l2668, l2669, l2670, l2671;
  let l2672, l2673, l2674, l2675, l2676, l2677, l2678, l2679, l2680, l2681, l2682, l2683, l2684, l2685, l2686, l2687;
  let l2688, l2689, l2690, l2691, l2692, l2693, l2694, l2695, l2696, l2697, l2698, l2699, l2700, l2701, l2702, l2703;
  let l2704, l2705, l2706, l2707, l2708, l2709, l2710, l2711, l2712, l2713, l2714, l2715, l2716, l2717, l2718, l2719;
  let l2720, l2721, l2722, l2723, l2724, l2725, l2726, l2727, l2728, l2729, l2730, l2731, l2732, l2733, l2734, l2735;
  let l2736, l2737, l2738, l2739, l2740, l2741, l2742, l2743, l2744, l2745, l2746, l2747, l2748, l2749, l2750, l2751;
  let l2752, l2753, l2754, l2755, l2756, l2757, l2758, l2759, l2760, l2761, l2762, l2763, l2764, l2765, l2766, l2767;
  let l2768, l2769, l2770, l2771, l2772, l2773, l2774, l2775, l2776, l2777, l2778, l2779, l2780, l2781, l2782, l2783;
  let l2784, l2785, l2786, l2787, l2788, l2789, l2790, l2791, l2792, l2793, l2794, l2795, l2796, l2797, l2798, l2799;
  let l2800, l2801, l2802, l2803, l2804, l2805, l2806, l2807, l2808, l2809, l2810, l2811, l2812, l2813, l2814, l2815;
  let l2816, l2817, l2818, l2819, l2820, l2821, l2822, l2823, l2824, l2825, l2826, l2827, l2828, l2829, l2830, l2831;
  let l2832, l2833, l2834, l2835, l2836, l2837, l2838, l2839, l2840, l2841, l2842, l2843, l2844, l2845, l2846, l2847;
  let l2848, l2849, l2850, l2851, l2852, l2853, l2854, l2855, l2856, l2857, l2858, l2859, l2860, l2861, l2862, l2863;
  let l2864, l2865, l2866, l2867, l2868, l2869, l2870, l2871, l2872, l2873, l2874, l2875, l2876, l2877, l2878, l2879;
  let l2880, l2881, l2882, l2883, l2884, l2885, l2886, l2887, l2888, l2889, l2890, l2891, l2892, l2893, l2894, l2895;
  let l2896, l2897, l2898, l2899, l2900, l2901, l2902, l2903, l2904, l2905, l2906, l2907, l2908, l2909, l2910, l2911;
  let l2912, l2913, l2914, l2915, l2916, l2917, l2918, l2919, l2920, l2921, l2922, l2923, l2924, l2925, l2926, l2927;
  let l2928, l2929, l2930, l2931, l2932, l2933, l2934, l2935, l2936, l2937, l2938, l2939, l2940, l2941, l2942, l2943;
  let l2944, l2945, l2946, l2947, l2948, l2949, l2950, l2951, l2952, l2953, l2954, l2955, l2956, l2957, l2958, l2959;
  let l2960, l2961, l2962, l2963, l2964, l2965, l2966, l2967, l2968, l2969, l2970, l2971, l2972, l2973, l2974, l2975;
  let l2976, l2977, l2978, l2979, l2980, l2981, l2982, l2983, l2984, l2985, l2986, l2987, l2988, l2989, l2990, l2991;
  let l2992, l2993, l2994, l2995, l2996, l2997, l2998, l2999, l3000, l3001, l3002, l3003, l3004, l3005, l3006, l3007;
  let l3008, l3009, l3010, l3011, l3012, l3013, l3014, l3015, l3016, l3017, l3018, l3019, l3020, l3021, l3022, l3023;
  let l3024, l3025, l3026, l3027, l3028, l3029, l3030, l3031, l3032, l3033, l3034, l3035, l3036, l3037, l3038, l3039;
  let l3040, l3041, l3042, l3043, l3044, l3045, l3046, l3047, l3048, l3049, l3050, l3051, l3052, l3053, l3054, l3055;
  let l3056, l3057, l3058, l3059, l3060, l3061, l3062, l3063, l3064, l3065, l3066, l3067, l3068, l3069, l3070, l3071;
  let l3072, l3073, l3074, l3075, l3076, l3077, l3078, l3079, l3080, l3081, l3082, l3083, l3084, l3085, l3086, l3087;
  let l3088, l3089, l3090, l3091, l3092, l3093, l3094, l3095, l3096, l3097, l3098, l3099, l3100, l3101, l3102, l3103;
  let l3104, l3105, l3106, l3107, l3108, l3109, l3110, l3111, l3112, l3113, l3114, l3115, l3116, l3117, l3118, l3119;
  let l3120, l3121, l3122, l3123, l3124, l3125, l3126, l3127, l3128, l3129, l3130, l3131, l3132, l3133, l3134, l3135;
  let l3136, l3137, l3138, l3139, l3140, l3141, l3142, l3143, l3144, l3145, l3146, l3147, l3148, l3149, l3150, l3151;
  let l3152, l3153, l3154, l3155, l3156, l3157, l3158, l3159, l3160, l3161, l3162, l3163, l3164, l3165, l3166, l3167;
  let l3168, l3169, l3170, l3171, l3172, l3173, l3174, l3175, l3176, l3177, l3178, l3179, l3180, l3181, l3182, l3183;
  let l3184, l3185, l3186, l3187, l3188, l3189, l3190, l3191, l3192, l3193, l3194, l3195, l3196, l3197, l3198, l3199;
  let l3200, l3201, l3202, l3203, l3204, l3205, l3206, l3207, l3208, l3209, l3210, l3211, l3212, l3213, l3214, l3215;
  let l3216, l3217, l3218, l3219, l3220, l3221, l3222, l3223, l3224, l3225, l3226, l3227, l3228, l3229, l3230, l3231;
  let l3232, l3233, l3234, l3235, l3236, l3237, l3238, l3239, l3240, l3241, l3242, l3243, l3244, l3245, l3246, l3247;
  let l3248, l3249, l3250, l3251, l3252, l3253, l3254, l3255, l3256, l3257, l3258, l3259, l3260, l3261, l3262, l3263;
  let l3264, l3265, l3266, l3267, l3268, l3269, l3270, l3271, l3272, l3273, l3274, l3275, l3276, l3277, l3278, l3279;
  let l3280, l3281, l3282, l3283, l3284, l3285, l3286, l3287, l3288, l3289, l3290, l3291, l3292, l3293, l3294, l3295;
  let l3296, l3297, l3298, l3299, l3300, l3301, l3302, l3303, l3304, l3305, l3306, l3307, l3308, l3309, l3310, l3311;
  let l3312, l3313, l3314, l3315, l3316, l3317, l3318, l3319, l3320, l3321, l3322, l3323, l3324, l3325, l3326, l3327;
  let l3328, l3329, l3330, l3331, l3332, l3333, l3334, l3335, l3336, l3337, l3338, l3339, l3340, l3341, l3342, l3343;
  let l3344, l3345, l3346, l3347, l3348, l3349, l3350, l3351, l3352, l3353, l3354, l3355, l3356, l3357, l3358, l3359;
  let l3360, l3361, l3362, l3363, l3364, l3365, l3366, l3367, l3368, l3369, l3370, l3371, l3372, l3373, l3374, l3375;
  let l3376, l3377, l3378, l3379, l3380, l3381, l3382, l3383, l3384, l3385, l3386, l3387, l3388, l3389, l3390, l3391;
  let l3392, l3393, l3394, l3395, l3396, l3397, l3398, l3399, l3400, l3401, l3402, l3403, l3404, l3405, l3406, l3407;
  let l3408, l3409, l3410, l3411, l3412, l3413, l3414, l3415, l3416, l3417, l3418, l3419, l3420, l3421, l3422, l3423;
  let l3424, l3425, l3426, l3427, l3428, l3429, l3430, l3431, l3432, l3433, l3434, l3435, l3436, l3437, l3438, l3439;
  let l3440, l3441, l3442, l3443, l3444, l3445, l3446, l3447, l3448, l3449, l3450, l3451, l3452, l3453, l3454, l3455;
  let l3456, l3457, l3458, l3459, l3460, l3461, l3462, l3463, l3464, l3465, l3466, l3467, l3468, l3469, l3470, l3471;
  let l3472, l3473, l3474, l3475, l3476, l3477, l3478, l3479, l3480, l3481, l3482, l3483, l3484, l3485, l3486, l3487;
  let l3488, l3489, l3490, l3491, l3492, l3493, l3494, l3495, l3496, l3497, l3498, l3499, l3500, l3501, l3502, l3503;
  let l3504, l3505, l3506, l3507, l3508, l3509, l3510, l3511, l3512, l3513, l3514, l3515, l3516, l3517, l3518, l3519;
  let l3520, l3521, l3522, l3523, l3524, l3525, l3526, l3527, l3528, l3529, l3530, l3531, l3532, l3533, l3534, l3535;
  let l3536, l3537, l3538, l3539, l3540, l3541, l3542, l3543, l3544, l3545, l3546, l3547, l3548, l3549, l3550, l3551;
  let l3552, l3553, l3554, l3555, l3556, l3557, l3558, l3559, l3560, l3561, l3562, l3563, l3564, l3565, l3566, l3567;
  let l3568, l3569, l3570, l3571, l3572, l3573, l3574, l3575, l3576, l3577, l3578, l3579, l3580, l3581, l3582, l3583;
  let l3584, l3585, l3586, l3587, l3588, l3589, l3590, l3591, l3592, l3593, l3594, l3595, l3596, l3597, l3598, l3599;
  let l3600, l3601, l3602, l3603, l3604, l3605, l3606, l3607, l3608, l3609, l3610, l3611, l3612, l3613, l3614, l3615;
  let l3616, l3617, l3618, l3619, l3620, l3621, l3622, l3623, l3624, l3625, l3626, l3627, l3628, l3629, l3630, l3631;
  let l3632, l3633, l3634, l3635, l3636, l3637, l3638, l3639, l3640, l3641, l3642, l3643, l3644, l3645, l3646, l3647;
  let l3648, l3649, l3650, l3651, l3652, l3653, l3654, l3655, l3656, l3657, l3658, l3659, l3660, l3661, l3662, l3663;
  let l3664, l3665, l3666, l3667, l3668, l3669, l3670, l3671, l3672, l3673, l3674, l3675, l3676, l3677, l3678, l3679;
  let l3680, l3681, l3682, l3683, l3684, l3685, l3686, l3687, l3688, l3689, l3690, l3691, l3692, l3693, l3694, l3695;
  let l3696, l3697, l3698, l3699, l3700, l3701, l3702, l3703, l3704, l3705, l3706, l3707, l3708, l3709, l3710, l3711;
  let l3712, l3713, l3714, l3715, l3716, l3717, l3718, l3719, l3720, l3721, l3722, l3723, l3724, l3725, l3726, l3727;
  let l3728, l3729, l3730, l3731, l3732, l3733, l3734, l3735, l3736, l3737, l3738, l3739, l3740, l3741, l3742, l3743;
  let l3744, l3745, l3746, l3747, l3748, l3749, l3750, l3751, l3752, l3753, l3754, l3755, l3756, l3757, l3758, l3759;
  let l3760, l3761, l3762, l3763, l3764, l3765, l3766, l3767, l3768, l3769, l3770, l3771, l3772, l3773, l3774, l3775;
  let l3776, l3777, l3778, l3779, l3780, l3781, l3782, l3783, l3784, l3785, l3786, l3787, l3788, l3789, l3790, l3791;
  let l3792, l3793, l3794, l3795, l3796, l3797, l3798, l3799, l3800, l3801, l3802, l3803, l3804, l3805, l3806, l3807;
  let l3808, l3809, l3810, l3811, l3812, l3813, l3814, l3815, l3816, l3817, l3818, l3819, l3820, l3821, l3822, l3823;
  let l3824, l3825, l3826, l3827, l3828, l3829, l3830, l3831, l3832, l3833, l3834, l3835, l3836, l3837, l3838, l3839;
  let l3840, l3841, l3842, l3843, l3844, l3845, l3846, l3847, l3848, l3849, l3850, l3851, l3852, l3853, l3854, l3855;
  let l3856, l3857, l3858, l3859, l3860, l3861, l3862, l3863, l3864, l3865, l3866, l3867, l3868, l3869, l3870, l3871;
  let l3872, l3873, l3874, l3875, l3876, l3877, l3878, l3879, l3880, l3881, l3882, l3883, l3884, l3885, l3886, l3887;
  let l3888, l3889, l3890, l3891, l3892, l3893, l3894, l3895, l3896, l3897, l3898, l3899, l3900, l3901, l3902, l3903;
  let l3904, l3905, l3906, l3907, l3908, l3909, l3910, l3911, l3912, l3913, l3914, l3915, l3916, l3917, l3918, l3919;
  let l3920, l3921, l3922, l3923, l3924, l3925, l3926, l3927, l3928, l3929, l3930, l3931, l3932, l3933, l3934, l3935;
  let l3936, l3937, l3938, l3939, l3940, l3941, l3942, l3943, l3944, l3945, l3946, l3947, l3948, l3949, l3950, l3951;
  let l3952, l3953, l3954, l3955, l3956, l3957, l3958, l3959, l3960, l3961, l3962, l3963, l3964, l3965, l3966, l3967;
  let l3968, l3969, l3970, l3971, l3972, l3973, l3974, l3975, l3976, l3977, l3978, l3979, l3980, l3981, l3982, l3983;
  let l3984, l3985, l3986, l3987, l3988, l3989, l3990, l3991, l3992, l3993, l3994, l3995, l3996, l3997, l3998, l3999;
  let l4000, l4001, l4002, l4003, l4004, l4005, l4006, l4007, l4008, l4009, l4010, l4011, l4012, l4013, l4014, l4015;
  let l4016, l4017, l4018, l4019, l4020, l4021, l4022, l4023, l4024, l4025, l4026, l4027, l4028, l4029, l4030, l4031;
  let l4032, l4033, l4034, l4035, l4036, l4037, l4038, l4039, l4040, l4041, l4042, l4043, l4044, l4045, l4046, l4047;
  let l4048, l4049, l4050, l4051, l4052, l4053, l4054, l4055, l4056, l4057, l4058, l4059, l4060, l4061, l4062, l4063;
  let l4064, l4065, l4066, l4067, l4068, l4069, l4070, l4071, l4072, l4073, l4074, l4075, l4076, l4077, l4078, l4079;
  let l4080, l4081, l4082, l4083, l4084, l4085, l4086, l4087, l4088, l4089, l4090, l4091, l4092, l4093, l4094, l4095;
  let l4096, l4097, l4098, l4099, l4100, l4101, l4102, l4103, l4104, l4105, l4106, l4107, l4108, l4109, l4110, l4111;
  let l4112, l4113, l4114, l4115, l4116, l4117, l4118, l4119, l4120, l4121, l4122, l4123, l4124, l4125, l4126, l4127;
  let l4128, l4129, l4130, l4131, l4132, l4133, l4134, l4135, l4136, l4137, l4138, l4139, l4140, l4141, l4142, l4143;
  let l4144, l4145, l4146, l4147, l4148, l4149, l4150, l4151, l4152, l4153, l4154, l4155, l4156, l4157, l4158, l4159;
  let l4160, l4161, l4162, l4163, l4164, l4165, l4166, l4167, l4168, l4169, l4170, l4171, l4172, l4173, l4174, l4175;
  let l4176, l4177, l4178, l4179, l4180, l4181, l4182, l4183, l4184, l4185, l4186, l4187, l4188, l4189, l4190, l4191;
  let l4192, l4193, l4194, l4195, l4196, l4197, l4198, l4199, l4200, l4201, l4202, l4203, l4204, l4205, l4206, l4207;
  let l4208, l4209, l4210, l4211, l4212, l4213, l4214, l4215, l4216, l4217, l4218, l4219, l4220, l4221, l4222, l4223;
  let l4224, l4225, l4226, l4227, l4228, l4229, l4230, l4231, l4232, l4233, l4234, l4235, l4236, l4237, l4238, l4239;
  let l4240, l4241, l4242, l4243, l4244, l4245, l4246, l4247, l4248, l4249, l4250, l4251, l4252, l4253, l4254, l4255;
  let l4256, l4257, l4258, l4259, l4260, l4261, l4262, l4263, l4264, l4265, l4266, l4267, l4268, l4269, l4270, l4271;
  let l4272, l4273, l4274, l4275, l4276, l4277, l4278, l4279, l4280, l4281, l4282, l4283, l4284, l4285, l4286, l4287;
  let l4288, l4289, l4290, l4291, l4292, l4293, l4294, l4295, l4296, l4297, l4298, l4299, l4300, l4301, l4302, l4303;
  let l4304, l4305, l4306, l4307, l4308, l4309, l4310, l4311, l4312, l4313, l4314, l4315, l4316, l4317, l4318, l4319;
  let l4320, l4321, l4322, l4323, l4324, l4325, l4326, l4327, l4328, l4329, l4330, l4331, l4332, l4333, l4334, l4335;
  let l4336, l4337, l4338, l4339, l4340, l4341, l4342, l4343, l4344, l4345, l4346, l4347, l4348, l4349, l4350, l4351;
  let l4352, l4353, l4354, l4355, l4356, l4357, l4358, l4359, l4360, l4361, l4362, l4363, l4364, l4365, l4366, l4367;
  let l4368, l4369, l4370, l4371, l4372, l4373, l4374, l4375, l4376, l4377, l4378, l4379, l4380, l4381, l4382, l4383;
  let l4384, l4385, l4386, l4387, l4388, l4389, l4390, l4391, l4392, l4393, l4394, l4395, l4396, l4397, l4398, l4399;
  let l4400, l4401, l4402, l4403, l4404, l4405, l4406, l4407, l4408, l4409, l4410, l4411, l4412, l4413, l4414, l4415;
  let l4416, l4417, l4418, l4419, l4420, l4421, l4422, l4423, l4424, l4425, l4426, l4427, l4428, l4429, l4430, l4431;
  let l4432, l4433, l4434, l4435, l4436, l4437, l4438, l4439, l4440, l4441, l4442, l4443, l4444, l4445, l4446, l4447;
  let l4448, l4449, l4450, l4451, l4452, l4453, l4454, l4455, l4456, l4457, l4458, l4459, l4460, l4461, l4462, l4463;
  let l4464, l4465, l4466, l4467, l4468, l4469, l4470, l4471, l4472, l4473, l4474, l4475, l4476, l4477, l4478, l4479;
  let l4480, l4481, l4482, l4483, l4484, l4485, l4486, l4487, l4488, l4489, l4490, l4491, l4492, l4493, l4494, l4495;
  let l4496, l4497, l4498, l4499, l4500, l4501, l4502, l4503, l4504, l4505, l4506, l4507, l4508, l4509, l4510, l4511;
  let l4512, l4513, l4514, l4515, l4516, l4517, l4518, l4519, l4520, l4521, l4522, l4523, l4524, l4525, l4526, l4527;
  let l4528, l4529, l4530, l4531, l4532, l4533, l4534, l4535, l4536, l4537, l4538, l4539, l4540, l4541, l4542, l4543;
  let l4544, l4545, l4546, l4547, l4548, l4549, l4550, l4551, l4552, l4553, l4554, l4555, l4556, l4557, l4558, l4559;
  let l4560, l4561, l4562, l4563, l4564, l4565, l4566, l4567, l4568, l4569, l4570, l4571, l4572, l4573, l4574, l4575;
  let l4576, l4577, l4578, l4579, l4580, l4581, l4582, l4583, l4584, l4585, l4586, l4587, l4588, l4589, l4590, l4591;
  let l4592, l4593, l4594, l4595, l4596, l4597, l4598, l4599, l4600, l4601, l4602, l4603, l4604, l4605, l4606, l4607;
  let l4608, l4609, l4610, l4611, l4612, l4613, l4614, l4615, l4616, l4617, l4618, l4619, l4620, l4621, l4622, l4623;
  let l4624, l4625, l4626, l4627, l4628, l4629, l4630, l4631, l4632, l4633, l4634, l4635, l4636, l4637, l4638, l4639;
  let l4640, l4641, l4642, l4643, l4644, l4645, l4646, l4647, l4648, l4649, l4650, l4651, l4652, l4653, l4654, l4655;
  let l4656, l4657, l4658, l4659, l4660, l4661, l4662, l4663, l4664, l4665, l4666, l4667, l4668, l4669, l4670, l4671;
  let l4672, l4673, l4674, l4675, l4676, l4677, l4678, l4679, l4680, l4681, l4682, l4683, l4684, l4685, l4686, l4687;
  let l4688, l4689, l4690, l4691, l4692, l4693, l4694, l4695, l4696, l4697, l4698, l4699, l4700, l4701, l4702, l4703;
  let l4704, l4705, l4706, l4707, l4708, l4709, l4710, l4711, l4712, l4713, l4714, l4715, l4716, l4717, l4718, l4719;
  let l4720, l4721, l4722, l4723, l4724, l4725, l4726, l4727, l4728, l4729, l4730, l4731, l4732, l4733, l4734, l4735;
  let l4736, l4737, l4738, l4739, l4740, l4741, l4742, l4743, l4744, l4745, l4746, l4747, l4748, l4749, l4750, l4751;
  let l4752, l4753, l4754, l4755, l4756, l4757, l4758, l4759, l4760, l4761, l4762, l4763, l4764, l4765, l4766, l4767;
  let l4768, l4769, l4770, l4771, l4772, l4773, l4774, l4775, l4776, l4777, l4778, l4779, l4780, l4781, l4782, l4783;
  let l4784, l4785, l4786, l4787, l4788, l4789, l4790, l4791, l4792, l4793, l4794, l4795, l4796, l4797, l4798, l4799;
  let l4800, l4801, l4802, l4803, l4804, l4805, l4806, l4807, l4808, l4809, l4810, l4811, l4812, l4813, l4814, l4815;
  let l4816, l4817, l4818, l4819, l4820, l4821, l4822, l4823, l4824, l4825, l4826, l4827, l4828, l4829, l4830, l4831;
  let l4832, l4833, l4834, l4835, l4836, l4837, l4838, l4839, l4840, l4841, l4842, l4843, l4844, l4845, l4846, l4847;
  let l4848, l4849, l4850, l4851, l4852, l4853, l4854, l4855, l4856, l4857, l4858, l4859, l4860, l4861, l4862, l4863;
  let l4864, l4865, l4866, l4867, l4868, l4869, l4870, l4871, l4872, l4873, l4874, l4875, l4876, l4877, l4878, l4879;
  let l4880, l4881, l4882, l4883, l4884, l4885, l4886, l4887, l4888, l4889, l4890, l4891, l4892, l4893, l4894, l4895;
  let l4896, l4897, l4898, l4899, l4900, l4901, l4902, l4903, l4904, l4905, l4906, l4907, l4908, l4909, l4910, l4911;
  let l4912, l4913, l4914, l4915, l4916, l4917, l4918, l4919, l4920, l4921, l4922, l4923, l4924, l4925, l4926, l4927;
  let l4928, l4929, l4930, l4931, l4932, l4933, l4934, l4935, l4936, l4937, l4938, l4939, l4940, l4941, l4942, l4943;
  let l4944, l4945, l4946, l4947, l4948, l4949, l4950, l4951, l4952, l4953, l4954, l4955, l4956, l4957, l4958, l4959;
  let l4960, l4961, l4962, l4963, l4964, l4965, l4966, l4967, l4968, l4969, l4970, l4971, l4972, l4973, l4974, l4975;
  let l4976, l4977, l4978, l4979, l4980, l4981, l4982, l4983, l4984, l4985, l4986, l4987, l4988, l4989, l4990, l4991;
  let l4992, l4993, l4994, l4995, l4996, l4997, l4998, l4999, l5000, l5001, l5002, l5003, l5004, l5005, l5006, l5007;
  let l5008, l5009, l5010, l5011, l5012, l5013, l5014, l5015, l5016, l5017, l5018, l5019, l5020, l5021, l5022, l5023;
  let l5024, l5025, l5026, l5027, l5028, l5029, l5030, l5031, l5032, l5033, l5034, l5035, l5036, l5037, l5038, l5039;
  let l5040, l5041, l5042, l5043, l5044, l5045, l5046, l5047, l5048, l5049, l5050, l5051, l5052, l5053, l5054, l5055;
  let l5056, l5057, l5058, l5059, l5060, l5061, l5062, l5063, l5064, l5065, l5066, l5067, l5068, l5069, l5070, l5071;
  let l5072, l5073, l5074, l5075, l5076, l5077, l5078, l5079, l5080, l5081, l5082, l5083, l5084, l5085, l5086, l5087;
  let l5088, l5089, l5090, l5091, l5092, l5093, l5094, l5095, l5096, l5097, l5098, l5099, l5100, l5101, l5102, l5103;
  let l5104, l5105, l5106, l5107, l5108, l5109, l5110, l5111, l5112, l5113, l5114, l5115, l5116, l5117, l5118, l5119;
  let l5120, l5121, l5122, l5123, l5124, l5125, l5126, l5127, l5128, l5129, l5130, l5131, l5132, l5133, l5134, l5135;
  let l5136, l5137, l5138, l5139, l5140, l5141, l5142, l5143, l5144, l5145, l5146, l5147, l5148, l5149, l5150, l5151;
  let l5152, l5153, l5154, l5155, l5156, l5157, l5158, l5159, l5160, l5161, l5162, l5163, l5164, l5165, l5166, l5167;
  let l5168, l5169, l5170, l5171, l5172, l5173, l5174, l5175, l5176, l5177, l5178, l5179, l5180, l5181, l5182, l5183;
  let l5184, l5185, l5186, l5187, l5188, l5189, l5190, l5191, l5192, l5193, l5194, l5195, l5196, l5197, l5198, l5199;
  let l5200, l5201, l5202, l5203, l5204, l5205, l5206, l5207, l5208, l5209, l5210, l5211, l5212, l5213, l5214, l5215;
  let l5216, l5217, l5218, l5219, l5220, l5221, l5222, l5223, l5224, l5225, l5226, l5227, l5228, l5229, l5230, l5231;
  let l5232, l5233, l5234, l5235, l5236, l5237, l5238, l5239, l5240, l5241, l5242, l5243, l5244, l5245, l5246, l5247;
  let l5248, l5249, l5250, l5251, l5252, l5253, l5254, l5255, l5256, l5257, l5258, l5259, l5260, l5261, l5262, l5263;
  let l5264, l5265, l5266, l5267, l5268, l5269, l5270, l5271, l5272, l5273, l5274, l5275, l5276, l5277, l5278, l5279;
  let l5280, l5281, l5282, l5283, l5284, l5285, l5286, l5287, l5288, l5289, l5290, l5291, l5292, l5293, l5294, l5295;
  let l5296, l5297, l5298, l5299, l5300, l5301, l5302, l5303, l5304, l5305, l5306, l5307, l5308, l5309, l5310, l5311;
  let l5312, l5313, l5314, l5315, l5316, l5317, l5318, l5319, l5320, l5321, l5322, l5323, l5324, l5325, l5326, l5327;
  let l5328, l5329, l5330, l5331, l5332, l5333, l5334, l5335, l5336, l5337, l5338, l5339, l5340, l5341, l5342, l5343;
  let l5344, l5345, l5346, l5347, l5348, l5349, l5350, l5351, l5352, l5353, l5354, l5355, l5356, l5357, l5358, l5359;
  let l5360, l5361, l5362, l5363, l5364, l5365, l5366, l5367, l5368, l5369, l5370, l5371, l5372, l5373, l5374, l5375;
  let l5376, l5377, l5378, l5379, l5380, l5381, l5382, l5383, l5384, l5385, l5386, l5387, l5388, l5389, l5390, l5391;
  let l5392, l5393, l5394, l5395, l5396, l5397, l5398, l5399, l5400, l5401, l5402, l5403, l5404, l5405, l5406, l5407;
  let l5408, l5409, l5410, l5411, l5412, l5413, l5414, l5415, l5416, l5417, l5418, l5419, l5420, l5421, l5422, l5423;
  let l5424, l5425, l5426, l5427, l5428, l5429, l5430, l5431, l5432, l5433, l5434, l5435, l5436, l5437, l5438, l5439;
  let l5440, l5441, l5442, l5443, l5444, l5445, l5446, l5447, l5448, l5449, l5450, l5451, l5452, l5453, l5454, l5455;
  let l5456, l5457, l5458, l5459, l5460, l5461, l5462, l5463, l5464, l5465, l5466, l5467, l5468, l5469, l5470, l5471;
  let l5472, l5473, l5474, l5475, l5476, l5477, l5478, l5479, l5480, l5481, l5482, l5483, l5484, l5485, l5486, l5487;
  let l5488, l5489, l5490, l5491, l5492, l5493, l5494, l5495, l5496, l5497, l5498, l5499, l5500, l5501, l5502, l5503;
  let l5504, l5505, l5506, l5507, l5508, l5509, l5510, l5511, l5512, l5513, l5514, l5515, l5516, l5517, l5518, l5519;
  let l5520, l5521, l5522, l5523, l5524, l5525, l5526, l5527, l5528, l5529, l5530, l5531, l5532, l5533, l5534, l5535;
  let l5536, l5537, l5538, l5539, l5540, l5541, l5542, l5543, l5544, l5545, l5546, l5547, l5548, l5549, l5550, l5551;
  let l5552, l5553, l5554, l5555, l5556, l5557, l5558, l5559, l5560, l5561, l5562, l5563, l5564, l5565, l5566, l5567;
  let l5568, l5569, l5570, l5571, l5572, l5573, l5574, l5575, l5576, l5577, l5578, l5579, l5580, l5581, l5582, l5583;
  let l5584, l5585, l5586, l5587, l5588, l5589, l5590, l5591, l5592, l5593, l5594, l5595, l5596, l5597, l5598, l5599;
  let l5600, l5601, l5602, l5603, l5604, l5605, l5606, l5607, l5608, l5609, l5610, l5611, l5612, l5613, l5614, l5615;
  let l5616, l5617, l5618, l5619, l5620, l5621, l5622, l5623, l5624, l5625, l5626, l5627, l5628, l5629, l5630, l5631;
  let l5632, l5633, l5634, l5635, l5636, l5637, l5638, l5639, l5640, l5641, l5642, l5643, l5644, l5645, l5646, l5647;
  let l5648, l5649, l5650, l5651, l5652, l5653, l5654, l5655, l5656, l5657, l5658, l5659, l5660, l5661, l5662, l5663;
  let l5664, l5665, l5666, l5667, l5668, l5669, l5670, l5671, l5672, l5673, l5674, l5675, l5676, l5677, l5678, l5679;
  let l5680, l5681, l5682, l5683, l5684, l5685, l5686, l5687, l5688, l5689, l5690, l5691, l5692, l5693, l5694, l5695;
  let l5696, l5697, l5698, l5699, l5700, l5701, l5702, l5703, l5704, l5705, l5706, l5707, l5708, l5709, l5710, l5711;
  let l5712, l5713, l5714, l5715, l5716, l5717, l5718, l5719, l5720, l5721, l5722, l5723, l5724, l5725, l5726, l5727;
  let l5728, l5729, l5730, l5731, l5732, l5733, l5734, l5735, l5736, l5737, l5738, l5739, l5740, l5741, l5742, l5743;
  let l5744, l5745, l5746, l5747, l5748, l5749, l5750, l5751, l5752, l5753, l5754, l5755, l5756, l5757, l5758, l5759;
  let l5760, l5761, l5762, l5763, l5764, l5765, l5766, l5767, l5768, l5769, l5770, l5771, l5772, l5773, l5774, l5775;
  let l5776, l5777, l5778, l5779, l5780, l5781, l5782, l5783, l5784, l5785, l5786, l5787, l5788, l5789, l5790, l5791;
  let l5792, l5793, l5794, l5795, l5796, l5797, l5798, l5799, l5800, l5801, l5802, l5803, l5804, l5805, l5806, l5807;
  let l5808, l5809, l5810, l5811, l5812, l5813, l5814, l5815, l5816, l5817, l5818, l5819, l5820, l5821, l5822, l5823;
  let l5824, l5825, l5826, l5827, l5828, l5829, l5830, l5831, l5832, l5833, l5834, l5835, l5836, l5837, l5838, l5839;
  let l5840, l5841, l5842, l5843, l5844, l5845, l5846, l5847, l5848, l5849, l5850, l5851, l5852, l5853, l5854, l5855;
  let l5856, l5857, l5858, l5859, l5860, l5861, l5862, l5863, l5864, l5865, l5866, l5867, l5868, l5869, l5870, l5871;
  let l5872, l5873, l5874, l5875, l5876, l5877, l5878, l5879, l5880, l5881, l5882, l5883, l5884, l5885, l5886, l5887;
  let l5888, l5889, l5890, l5891, l5892, l5893, l5894, l5895, l5896, l5897, l5898, l5899, l5900, l5901, l5902, l5903;
  let l5904, l5905, l5906, l5907, l5908, l5909, l5910, l5911, l5912, l5913, l5914, l5915, l5916, l5917, l5918, l5919;
  let l5920, l5921, l5922, l5923, l5924, l5925, l5926, l5927, l5928, l5929, l5930, l5931, l5932, l5933, l5934, l5935;
  let l5936, l5937, l5938, l5939, l5940, l5941, l5942, l5943, l5944, l5945, l5946, l5947, l5948, l5949, l5950, l5951;
  let l5952, l5953, l5954, l5955, l5956, l5957, l5958, l5959, l5960, l5961, l5962, l5963, l5964, l5965, l5966, l5967;
  let l5968, l5969, l5970, l5971, l5972, l5973, l5974, l5975, l5976, l5977, l5978, l5979, l5980, l5981, l5982, l5983;
  let l5984, l5985, l5986, l5987, l5988, l5989, l5990, l5991, l5992, l5993, l5994, l5995, l5996, l5997, l5998, l5999;
  let l6000, l6001, l6002, l6003, l6004, l6005, l6006, l6007, l6008, l6009, l6010, l6011, l6012, l6013, l6014, l6015;
  let l6016, l6017, l6018, l6019, l6020, l6021, l6022, l6023, l6024, l6025, l6026, l6027, l6028, l6029, l6030, l6031;
  let l6032, l6033, l6034, l6035, l6036, l6037, l6038, l6039, l6040, l6041, l6042, l6043, l6044, l6045, l6046, l6047;
  let l6048, l6049, l6050, l6051, l6052, l6053, l6054, l6055, l6056, l6057, l6058, l6059, l6060, l6061, l6062, l6063;
  let l6064, l6065, l6066, l6067, l6068, l6069, l6070, l6071, l6072, l6073, l6074, l6075, l6076, l6077, l6078, l6079;
  let l6080, l6081, l6082, l6083, l6084, l6085, l6086, l6087, l6088, l6089, l6090, l6091, l6092, l6093, l6094, l6095;
  let l6096, l6097, l6098, l6099, l6100, l6101, l6102, l6103, l6104, l6105, l6106, l6107, l6108, l6109, l6110, l6111;
  let l6112, l6113, l6114, l6115, l6116, l6117, l6118, l6119, l6120, l6121, l6122, l6123, l6124, l6125, l6126, l6127;
  let l6128, l6129, l6130, l6131, l6132, l6133, l6134, l6135, l6136, l6137, l6138, l6139, l6140, l6141, l6142, l6143;
  let l6144, l6145, l6146, l6147, l6148, l6149, l6150, l6151, l6152, l6153, l6154, l6155, l6156, l6157, l6158, l6159;
  let l6160, l6161, l6162, l6163, l6164, l6165, l6166, l6167, l6168, l6169, l6170, l6171, l6172, l6173, l6174, l6175;
  let l6176, l6177, l6178, l6179, l6180, l6181, l6182, l6183, l6184, l6185, l6186, l6187, l6188, l6189, l6190, l6191;
  let l6192, l6193, l6194, l6195, l6196, l6197, l6198, l6199, l6200, l6201, l6202, l6203, l6204, l6205, l6206, l6207;
  let l6208, l6209, l6210, l6211, l6212, l6213, l6214, l6215, l6216, l6217, l6218, l6219, l6220, l6221, l6222, l6223;
  let l6224, l6225, l6226, l6227, l6228, l6229, l6230, l6231, l6232, l6233, l6234, l6235, l6236, l6237, l6238, l6239;
  let l6240, l6241, l6242, l6243, l6244, l6245, l6246, l6247, l6248, l6249, l6250, l6251, l6252, l6253, l6254, l6255;
  let l6256, l6257, l6258, l6259, l6260, l6261, l6262, l6263, l6264, l6265, l6266, l6267, l6268, l6269, l6270, l6271;
  let l6272, l6273, l6274, l6275, l6276, l6277, l6278, l6279, l6280, l6281, l6282, l6283, l6284, l6285, l6286, l6287;
  let l6288, l6289, l6290, l6291, l6292, l6293, l6294, l6295, l6296, l6297, l6298, l6299, l6300, l6301, l6302, l6303;
  let l6304, l6305, l6306, l6307, l6308, l6309, l6310, l6311, l6312, l6313, l6314, l6315, l6316, l6317, l6318, l6319;
  let l6320, l6321, l6322, l6323, l6324, l6325, l6326, l6327, l6328, l6329, l6330, l6331, l6332, l6333, l6334, l6335;
  let l6336, l6337, l6338, l6339, l6340, l6341, l6342, l6343, l6344, l6345, l6346, l6347, l6348, l6349, l6350, l6351;
  let l6352, l6353, l6354, l6355, l6356, l6357, l6358, l6359, l6360, l6361, l6362, l6363, l6364, l6365, l6366, l6367;
  let l6368, l6369, l6370, l6371, l6372, l6373, l6374, l6375, l6376, l6377, l6378, l6379, l6380, l6381, l6382, l6383;
  let l6384, l6385, l6386, l6387, l6388, l6389, l6390, l6391, l6392, l6393, l6394, l6395, l6396, l6397, l6398, l6399;
  let l6400, l6401, l6402, l6403, l6404, l6405, l6406, l6407, l6408, l6409, l6410, l6411, l6412, l6413, l6414, l6415;
  let l6416, l6417, l6418, l6419, l6420, l6421, l6422, l6423, l6424, l6425, l6426, l6427, l6428, l6429, l6430, l6431;
  let l6432, l6433, l6434, l6435, l6436, l6437, l6438, l6439, l6440, l6441, l6442, l6443, l6444, l6445, l6446, l6447;
  let l6448, l6449, l6450, l6451, l6452, l6453, l6454, l6455, l6456, l6457, l6458, l6459, l6460, l6461, l6462, l6463;
  let l6464, l6465, l6466, l6467, l6468, l6469, l6470, l6471, l6472, l6473, l6474, l6475, l6476, l6477, l6478, l6479;
  let l6480, l6481, l6482, l6483, l6484, l6485, l6486, l6487, l6488, l6489, l6490, l6491, l6492, l6493, l6494, l6495;
  let l6496, l6497, l6498, l6499, l6500, l6501, l6502, l6503, l6504, l6505, l6506, l6507, l6508, l6509, l6510, l6511;
  let l6512, l6513, l6514, l6515, l6516, l6517, l6518, l6519, l6520, l6521, l6522, l6523, l6524, l6525, l6526, l6527;
  let l6528, l6529, l6530, l6531, l6532, l6533, l6534, l6535, l6536, l6537, l6538, l6539, l6540, l6541, l6542, l6543;
  let l6544, l6545, l6546, l6547, l6548, l6549, l6550, l6551, l6552, l6553, l6554, l6555, l6556, l6557, l6558, l6559;
  let l6560, l6561, l6562, l6563, l6564, l6565, l6566, l6567, l6568, l6569, l6570, l6571, l6572, l6573, l6574, l6575;
  let l6576, l6577, l6578, l6579, l6580, l6581, l6582, l6583, l6584, l6585, l6586, l6587, l6588, l6589, l6590, l6591;
  let l6592, l6593, l6594, l6595, l6596, l6597, l6598, l6599, l6600, l6601, l6602, l6603, l6604, l6605, l6606, l6607;
  let l6608, l6609, l6610, l6611, l6612, l6613, l6614, l6615, l6616, l6617, l6618, l6619, l6620, l6621, l6622, l6623;
  let l6624, l6625, l6626, l6627, l6628, l6629, l6630, l6631, l6632, l6633, l6634, l6635, l6636, l6637, l6638, l6639;
  let l6640, l6641, l6642, l6643, l6644, l6645, l6646, l6647, l6648, l6649, l6650, l6651, l6652, l6653, l6654, l6655;
  let l6656, l6657, l6658, l6659, l6660, l6661, l6662, l6663, l6664, l6665, l6666, l6667, l6668, l6669, l6670, l6671;
  let l6672, l6673, l6674, l6675, l6676, l6677, l6678, l6679, l6680, l6681, l6682, l6683, l6684, l6685, l6686, l6687;
  let l6688, l6689, l6690, l6691, l6692, l6693, l6694, l6695, l6696, l6697, l6698, l6699, l6700, l6701, l6702, l6703;
  let l6704, l6705, l6706, l6707, l6708, l6709, l6710, l6711, l6712, l6713, l6714, l6715, l6716, l6717, l6718, l6719;
  let l6720, l6721, l6722, l6723, l6724, l6725, l6726, l6727, l6728, l6729, l6730, l6731, l6732, l6733, l6734, l6735;
  let l6736, l6737, l6738, l6739, l6740, l6741, l6742, l6743, l6744, l6745, l6746, l6747, l6748, l6749, l6750, l6751;
  let l6752, l6753, l6754, l6755, l6756, l6757, l6758, l6759, l6760, l6761, l6762, l6763, l6764, l6765, l6766, l6767;
  let l6768, l6769, l6770, l6771, l6772, l6773, l6774, l6775, l6776, l6777, l6778, l6779, l6780, l6781, l6782, l6783;
  let l6784, l6785, l6786, l6787, l6788, l6789, l6790, l6791, l6792, l6793, l6794, l6795, l6796, l6797, l6798, l6799;
  let l6800, l6801, l6802, l6803, l6804, l6805, l6806, l6807, l6808, l6809, l6810, l6811, l6812, l6813, l6814, l6815;
  let l6816, l6817, l6818, l6819, l6820, l6821, l6822, l6823, l6824, l6825, l6826, l6827, l6828, l6829, l6830, l6831;
  let l6832, l6833, l6834, l6835, l6836, l6837, l6838, l6839, l6840, l6841, l6842, l6843, l6844, l6845, l6846, l6847;
  let l6848, l6849, l6850, l6851, l6852, l6853, l6854, l6855, l6856, l6857, l6858, l6859, l6860, l6861, l6862, l6863;
  let l6864, l6865, l6866, l6867, l6868, l6869, l6870, l6871, l6872, l6873, l6874, l6875, l6876, l6877, l6878, l6879;
  let l6880, l6881, l6882, l6883, l6884, l6885, l6886, l6887, l6888, l6889, l6890, l6891, l6892, l6893, l6894, l6895;
  let l6896, l6897, l6898, l6899, l6900, l6901, l6902, l6903, l6904, l6905, l6906, l6907, l6908, l6909, l6910, l6911;
  let l6912, l6913, l6914, l6915, l6916, l6917, l6918, l6919, l6920, l6921, l6922, l6923, l6924, l6925, l6926, l6927;
  let l6928, l6929, l6930, l6931, l6932, l6933, l6934, l6935, l6936, l6937, l6938, l6939, l6940, l6941, l6942, l6943;
  let l6944, l6945, l6946, l6947, l6948, l6949, l6950, l6951, l6952, l6953, l6954, l6955, l6956, l6957, l6958, l6959;
  let l6960, l6961, l6962, l6963, l6964, l6965, l6966, l6967, l6968, l6969, l6970, l6971, l6972, l6973, l6974, l6975;
  let l6976, l6977, l6978, l6979, l6980, l6981, l6982, l6983, l6984, l6985, l6986, l6987, l6988, l6989, l6990, l6991;
  let l6992, l6993, l6994, l6995, l6996, l6997, l6998, l6999, l7000, l7001, l7002, l7003, l7004, l7005, l7006, l7007;
  let l7008, l7009, l7010, l7011, l7012, l7013, l7014, l7015, l7016, l7017, l7018, l7019, l7020, l7021, l7022, l7023;
  let l7024, l7025, l7026, l7027, l7028, l7029, l7030, l7031, l7032, l7033, l7034, l7035, l7036, l7037, l7038, l7039;
  let l7040, l7041, l7042, l7043, l7044, l7045, l7046, l7047, l7048, l7049, l7050, l7051, l7052, l7053, l7054, l7055;
  let l7056, l7057, l7058, l7059, l7060, l7061, l7062, l7063, l7064, l7065, l7066, l7067, l7068, l7069, l7070, l7071;
  let l7072, l7073, l7074, l7075, l7076, l7077, l7078, l7079, l7080, l7081, l7082, l7083, l7084, l7085, l7086, l7087;
  let l7088, l7089, l7090, l7091, l7092, l7093, l7094, l7095, l7096, l7097, l7098, l7099, l7100, l7101, l7102, l7103;
  let l7104, l7105, l7106, l7107, l7108, l7109, l7110, l7111, l7112, l7113, l7114, l7115, l7116, l7117, l7118, l7119;
  let l7120, l7121, l7122, l7123, l7124, l7125, l7126, l7127, l7128, l7129, l7130, l7131, l7132, l7133, l7134, l7135;
  let l7136, l7137, l7138, l7139, l7140, l7141, l7142, l7143, l7144, l7145, l7146, l7147, l7148, l7149, l7150, l7151;
  let l7152, l7153, l7154, l7155, l7156, l7157, l7158, l7159, l7160, l7161, l7162, l7163, l7164, l7165, l7166, l7167;
  let l7168, l7169, l7170, l7171, l7172, l7173, l7174, l7175, l7176, l7177, l7178, l7179, l7180, l7181, l7182, l7183;
  let l7184, l7185, l7186, l7187, l7188, l7189, l7190, l7191, l7192, l7193, l7194, l7195, l7196, l7197, l7198, l7199;
  let l7200, l7201, l7202, l7203, l7204, l7205, l7206, l7207, l7208, l7209, l7210, l7211, l7212, l7213, l7214, l7215;
  let l7216, l7217, l7218, l7219, l7220, l7221, l7222, l7223, l7224, l7225, l7226, l7227, l7228, l7229, l7230, l7231;
  let l7232, l7233, l7234, l7235, l7236, l7237, l7238, l7239, l7240, l7241, l7242, l7243, l7244, l7245, l7246, l7247;
  let l7248, l7249, l7250, l7251, l7252, l7253, l7254, l7255, l7256, l7257, l7258, l7259, l7260, l7261, l7262, l7263;
  let l7264, l7265, l7266, l7267, l7268, l7269, l7270, l7271, l7272, l7273, l7274, l7275, l7276, l7277, l7278, l7279;
  let l7280, l7281, l7282, l7283, l7284, l7285, l7286, l7287, l7288, l7289, l7290, l7291, l7292, l7293, l7294, l7295;
  let l7296, l7297, l7298, l7299, l7300, l7301, l7302, l7303, l7304, l7305, l7306, l7307, l7308, l7309, l7310, l7311;
  let l7312, l7313, l7314, l7315, l7316, l7317, l7318, l7319, l7320, l7321, l7322, l7323, l7324, l7325, l7326, l7327;
  let l7328, l7329, l7330, l7331, l7332, l7333, l7334, l7335, l7336, l7337, l7338, l7339, l7340, l7341, l7342, l7343;
  let l7344, l7345, l7346, l7347, l7348, l7349, l7350, l7351, l7352, l7353, l7354, l7355, l7356, l7357, l7358, l7359;
  let l7360, l7361, l7362, l7363, l7364, l7365, l7366, l7367, l7368, l7369, l7370, l7371, l7372, l7373, l7374, l7375;
  let l7376, l7377, l7378, l7379, l7380, l7381, l7382, l7383, l7384, l7385, l7386, l7387, l7388, l7389, l7390, l7391;
  let l7392, l7393, l7394, l7395, l7396, l7397, l7398, l7399, l7400, l7401, l7402, l7403, l7404, l7405, l7406, l7407;
  let l7408, l7409, l7410, l7411, l7412, l7413, l7414, l7415, l7416, l7417, l7418, l7419, l7420, l7421, l7422, l7423;
  let l7424, l7425, l7426, l7427, l7428, l7429, l7430, l7431, l7432, l7433, l7434, l7435, l7436, l7437, l7438, l7439;
  let l7440, l7441, l7442, l7443, l7444, l7445, l7446, l7447, l7448, l7449, l7450, l7451, l7452, l7453, l7454, l7455;
  let l7456, l7457, l7458, l7459, l7460, l7461, l7462, l7463, l7464, l7465, l7466, l7467, l7468, l7469, l7470, l7471;
  let l7472, l7473, l7474, l7475, l7476, l7477, l7478, l7479, l7480, l7481, l7482, l7483, l7484, l7485, l7486, l7487;
  let l7488, l7489, l7490, l7491, l7492, l7493, l7494, l7495, l7496, l7497, l7498, l7499, l7500, l7501, l7502, l7503;
  let l7504, l7505, l7506, l7507, l7508, l7509, l7510, l7511, l7512, l7513, l7514, l7515, l7516, l7517, l7518, l7519;
  let l7520, l7521, l7522, l7523, l7524, l7525, l7526, l7527, l7528, l7529, l7530, l7531, l7532, l7533, l7534, l7535;
  let l7536, l7537, l7538, l7539, l7540, l7541, l7542, l7543, l7544, l7545, l7546, l7547, l7548, l7549, l7550, l7551;
  let l7552, l7553, l7554, l7555, l7556, l7557, l7558, l7559, l7560, l7561, l7562, l7563, l7564, l7565, l7566, l7567;
  let l7568, l7569, l7570, l7571, l7572, l7573, l7574, l7575, l7576, l7577, l7578, l7579, l7580, l7581, l7582, l7583;
  let l7584, l7585, l7586, l7587, l7588, l7589, l7590, l7591, l7592, l7593, l7594, l7595, l7596, l7597, l7598, l7599;
  let l7600, l7601, l7602, l7603, l7604, l7605, l7606, l7607, l7608, l7609, l7610, l7611, l7612, l7613, l7614, l7615;
  let l7616, l7617, l7618, l7619, l7620, l7621, l7622, l7623, l7624, l7625, l7626, l7627, l7628, l7629, l7630, l7631;
  let l7632, l7633, l7634, l7635, l7636, l7637, l7638, l7639, l7640, l7641, l7642, l7643, l7644, l7645, l7646, l7647;
  let l7648, l7649, l7650, l7651, l7652, l7653, l7654, l7655, l7656, l7657, l7658, l7659, l7660, l7661, l7662, l7663;
  let l7664, l7665, l7666, l7667, l7668, l7669, l7670, l7671, l7672, l7673, l7674, l7675, l7676, l7677, l7678, l7679;
  let l7680, l7681, l7682, l7683, l7684, l7685, l7686, l7687, l7688, l7689, l7690, l7691, l7692, l7693, l7694, l7695;
  let l7696, l7697, l7698, l7699, l7700, l7701, l7702, l7703, l7704, l7705, l7706, l7707, l7708, l7709, l7710, l7711;
  let l7712, l7713, l7714, l7715, l7716, l7717, l7718, l7719, l7720, l7721, l7722, l7723, l7724, l7725, l7726, l7727;
  let l7728, l7729, l7730, l7731, l7732, l7733, l7734, l7735, l7736, l7737, l7738, l7739, l7740, l7741, l7742, l7743;
  let l7744, l7745, l7746, l7747, l7748, l7749, l7750, l7751, l7752, l7753, l7754, l7755, l7756, l7757, l7758, l7759;
  let l7760, l7761, l7762, l7763, l7764, l7765, l7766, l7767, l7768, l7769, l7770, l7771, l7772, l7773, l7774, l7775;
  let l7776, l7777, l7778, l7779, l7780, l7781, l7782, l7783, l7784, l7785, l7786, l7787, l7788, l7789, l7790, l7791;
  let l7792, l7793, l7794, l7795, l7796, l7797, l7798, l7799, l7800, l7801, l7802, l7803, l7804, l7805, l7806, l7807;
  let l7808, l7809, l7810, l7811, l7812, l7813, l7814, l7815, l7816, l7817, l7818, l7819, l7820, l7821, l7822, l7823;
  let l7824, l7825, l7826, l7827, l7828, l7829, l7830, l7831, l7832, l7833, l7834, l7835, l7836, l7837, l7838, l7839;
  let l7840, l7841, l7842, l7843, l7844, l7845, l7846, l7847, l7848, l7849, l7850, l7851, l7852, l7853, l7854, l7855;
  let l7856, l7857, l7858, l7859, l7860, l7861, l7862, l7863, l7864, l7865, l7866, l7867, l7868, l7869, l7870, l7871;
  let l7872, l7873, l7874, l7875, l7876, l7877, l7878, l7879, l7880, l7881, l7882, l7883, l7884, l7885, l7886, l7887;
  let l7888, l7889, l7890, l7891, l7892, l7893, l7894, l7895, l7896, l7897, l7898, l7899, l7900, l7901, l7902, l7903;
  let l7904, l7905, l7906, l7907, l7908, l7909, l7910, l7911, l7912, l7913, l7914, l7915, l7916, l7917, l7918, l7919;
  let l7920, l7921, l7922, l7923, l7924, l7925, l7926, l7927, l7928, l7929, l7930, l7931, l7932, l7933, l7934, l7935;
  let l7936, l7937, l7938, l7939, l7940, l7941, l7942, l7943, l7944, l7945, l7946, l7947, l7948, l7949, l7950, l7951;
  let l7952, l7953, l7954, l7955, l7956, l7957, l7958, l7959, l7960, l7961, l7962, l7963, l7964, l7965, l7966, l7967;
  let l7968, l7969, l7970, l7971, l7972, l7973, l7974, l7975, l7976, l7977, l7978, l7979, l7980, l7981, l7982, l7983;
  let l7984, l7985, l7986, l7987, l7988, l7989, l7990, l7991, l7992, l7993, l7994, l7995, l7996, l7997, l7998, l7999;
  let l8000, l8001, l8002, l8003, l8004, l8005, l8006, l8007, l8008, l8009, l8010, l8011, l8012, l8013, l8014, l8015;
  let l8016, l8017, l8018, l8019, l8020, l8021, l8022, l8023, l8024, l8025, l8026, l8027, l8028, l8029, l8030, l8031;
  let l8032, l8033, l8034, l8035, l8036, l8037, l8038, l8039, l8040, l8041, l8042, l8043, l8044, l8045, l8046, l8047;
  let l8048, l8049, l8050, l8051, l8052, l8053, l8054, l8055, l8056, l8057, l8058, l8059, l8060, l8061, l8062, l8063;
  let l8064, l8065, l8066, l8067, l8068, l8069, l8070, l8071, l8072, l8073, l8074, l8075, l8076, l8077, l8078, l8079;
  let l8080, l8081, l8082, l8083, l8084, l8085, l8086, l8087, l8088, l8089, l8090, l8091, l8092, l8093, l8094, l8095;
  let l8096, l8097, l8098, l8099, l8100, l8101, l8102, l8103, l8104, l8105, l8106, l8107, l8108, l8109, l8110, l8111;
  let l8112, l8113, l8114, l8115, l8116, l8117, l8118, l8119, l8120, l8121, l8122, l8123, l8124, l8125, l8126, l8127;
  let l8128, l8129, l8130, l8131, l8132, l8133, l8134, l8135, l8136, l8137, l8138, l8139, l8140, l8141, l8142, l8143;
  let l8144, l8145, l8146, l8147, l8148, l8149, l8150, l8151, l8152, l8153, l8154, l8155, l8156, l8157, l8158, l8159;
  let l8160, l8161, l8162, l8163, l8164, l8165, l8166, l8167, l8168, l8169, l8170, l8171, l8172, l8173, l8174, l8175;
  let l8176, l8177, l8178, l8179, l8180, l8181, l8182, l8183, l8184, l8185, l8186, l8187, l8188, l8189, l8190, l8191;
  let l8192, l8193, l8194, l8195, l8196, l8197, l8198, l8199, l8200, l8201, l8202, l8203, l8204, l8205, l8206, l8207;
  let l8208, l8209, l8210, l8211, l8212, l8213, l8214, l8215, l8216, l8217, l8218, l8219, l8220, l8221, l8222, l8223;
  let l8224, l8225, l8226, l8227, l8228, l8229, l8230, l8231, l8232, l8233, l8234, l8235, l8236, l8237, l8238, l8239;
  let l8240, l8241, l8242, l8243, l8244, l8245, l8246, l8247, l8248, l8249, l8250, l8251, l8252, l8253, l8254, l8255;
  let l8256, l8257, l8258, l8259, l8260, l8261, l8262, l8263, l8264, l8265, l8266, l8267, l8268, l8269, l8270, l8271;
  let l8272, l8273, l8274, l8275, l8276, l8277, l8278, l8279, l8280, l8281, l8282, l8283, l8284, l8285, l8286, l8287;
  let l8288, l8289, l8290, l8291, l8292, l8293, l8294, l8295, l8296, l8297, l8298, l8299, l8300, l8301, l8302, l8303;
  let l8304, l8305, l8306, l8307, l8308, l8309, l8310, l8311, l8312, l8313, l8314, l8315, l8316, l8317, l8318, l8319;
  let l8320, l8321, l8322, l8323, l8324, l8325, l8326, l8327, l8328, l8329, l8330, l8331, l8332, l8333, l8334, l8335;
  let l8336, l8337, l8338, l8339, l8340, l8341, l8342, l8343, l8344, l8345, l8346, l8347, l8348, l8349, l8350, l8351;
  let l8352, l8353, l8354, l8355, l8356, l8357, l8358, l8359, l8360, l8361, l8362, l8363, l8364, l8365, l8366, l8367;
  let l8368, l8369, l8370, l8371, l8372, l8373, l8374, l8375, l8376, l8377, l8378, l8379, l8380, l8381, l8382, l8383;
  let l8384, l8385, l8386, l8387, l8388, l8389, l8390, l8391, l8392, l8393, l8394, l8395, l8396, l8397, l8398, l8399;
  let l8400, l8401, l8402, l8403, l8404, l8405, l8406, l8407, l8408, l8409, l8410, l8411, l8412, l8413, l8414, l8415;
  let l8416, l8417, l8418, l8419, l8420, l8421, l8422, l8423, l8424, l8425, l8426, l8427, l8428, l8429, l8430, l8431;
  let l8432, l8433, l8434, l8435, l8436, l8437, l8438, l8439, l8440, l8441, l8442, l8443, l8444, l8445, l8446, l8447;
  let l8448, l8449, l8450, l8451, l8452, l8453, l8454, l8455, l8456, l8457, l8458, l8459, l8460, l8461, l8462, l8463;
  let l8464, l8465, l8466, l8467, l8468, l8469, l8470, l8471, l8472, l8473, l8474, l8475, l8476, l8477, l8478, l8479;
  let l8480, l8481, l8482, l8483, l8484, l8485, l8486, l8487, l8488, l8489, l8490, l8491, l8492, l8493, l8494, l8495;
  let l8496, l8497, l8498, l8499, l8500, l8501, l8502, l8503, l8504, l8505, l8506, l8507, l8508, l8509, l8510, l8511;
  let l8512, l8513, l8514, l8515, l8516, l8517, l8518, l8519, l8520, l8521, l8522, l8523, l8524, l8525, l8526, l8527;
  let l8528, l8529, l8530, l8531, l8532, l8533, l8534, l8535, l8536, l8537, l8538, l8539, l8540, l8541, l8542, l8543;
  let l8544, l8545, l8546, l8547, l8548, l8549, l8550, l8551, l8552, l8553, l8554, l8555, l8556, l8557, l8558, l8559;
  let l8560, l8561, l8562, l8563, l8564, l8565, l8566, l8567, l8568, l8569, l8570, l8571, l8572, l8573, l8574, l8575;
  let l8576, l8577, l8578, l8579, l8580, l8581, l8582, l8583, l8584, l8585, l8586, l8587, l8588, l8589, l8590, l8591;
  let l8592, l8593, l8594, l8595, l8596, l8597, l8598, l8599, l8600, l8601, l8602, l8603, l8604, l8605, l8606, l8607;
  let l8608, l8609, l8610, l8611, l8612, l8613, l8614, l8615, l8616, l8617, l8618, l8619, l8620, l8621, l8622, l8623;
  let l8624, l8625, l8626, l8627, l8628, l8629, l8630, l8631, l8632, l8633, l8634, l8635, l8636, l8637, l8638, l8639;
  let l8640, l8641, l8642, l8643, l8644, l8645, l8646, l8647, l8648, l8649, l8650, l8651, l8652, l8653, l8654, l8655;
  let l8656, l8657, l8658, l8659, l8660, l8661, l8662, l8663, l8664, l8665, l8666, l8667, l8668, l8669, l8670, l8671;
  let l8672, l8673, l8674, l8675, l8676, l8677, l8678, l8679, l8680, l8681, l8682, l8683, l8684, l8685, l8686, l8687;
  let l8688, l8689, l8690, l8691, l8692, l8693, l8694, l8695, l8696, l8697, l8698, l8699, l8700, l8701, l8702, l8703;
  let l8704, l8705, l8706, l8707, l8708, l8709, l8710, l8711, l8712, l8713, l8714, l8715, l8716, l8717, l8718, l8719;
  let l8720, l8721, l8722, l8723, l8724, l8725, l8726, l8727, l8728, l8729, l8730, l8731, l8732, l8733, l8734, l8735;
  let l8736, l8737, l8738, l8739, l8740, l8741, l8742, l8743, l8744, l8745, l8746, l8747, l8748, l8749, l8750, l8751;
  let l8752, l8753, l8754, l8755, l8756, l8757, l8758, l8759, l8760, l8761, l8762, l8763, l8764, l8765, l8766, l8767;
  let l8768, l8769, l8770, l8771, l8772, l8773, l8774, l8775, l8776, l8777, l8778, l8779, l8780, l8781, l8782, l8783;
  let l8784, l8785, l8786, l8787, l8788, l8789, l8790, l8791, l8792, l8793, l8794, l8795, l8796, l8797, l8798, l8799;
  let l8800, l8801, l8802, l8803, l8804, l8805, l8806, l8807, l8808, l8809, l8810, l8811, l8812, l8813, l8814, l8815;
  let l8816, l8817, l8818, l8819, l8820, l8821, l8822, l8823, l8824, l8825, l8826, l8827, l8828, l8829, l8830, l8831;
  let l8832, l8833, l8834, l8835, l8836, l8837, l8838, l8839, l8840, l8841, l8842, l8843, l8844, l8845, l8846, l8847;
  let l8848, l8849, l8850, l8851, l8852, l8853, l8854, l8855, l8856, l8857, l8858, l8859, l8860, l8861, l8862, l8863;
  let l8864, l8865, l8866, l8867, l8868, l8869, l8870, l8871, l8872, l8873, l8874, l8875, l8876, l8877, l8878, l8879;
  let l8880, l8881, l8882, l8883, l8884, l8885, l8886, l8887, l8888, l8889, l8890, l8891, l8892, l8893, l8894, l8895;
  let l8896, l8897, l8898, l8899, l8900, l8901, l8902, l8903, l8904, l8905, l8906, l8907, l8908, l8909, l8910, l8911;
  let l8912, l8913, l8914, l8915, l8916, l8917, l8918, l8919, l8920, l8921, l8922, l8923, l8924, l8925, l8926, l8927;
  let l8928, l8929, l8930, l8931, l8932, l8933, l8934, l8935, l8936, l8937, l8938, l8939, l8940, l8941, l8942, l8943;
  let l8944, l8945, l8946, l8947, l8948, l8949, l8950, l8951, l8952, l8953, l8954, l8955, l8956, l8957, l8958, l8959;
  let l8960, l8961, l8962, l8963, l8964, l8965, l8966, l8967, l8968, l8969, l8970, l8971, l8972, l8973, l8974, l8975;
  let l8976, l8977, l8978, l8979, l8980, l8981, l8982, l8983, l8984, l8985, l8986, l8987, l8988, l8989, l8990, l8991;
  let l8992, l8993, l8994, l8995, l8996, l8997, l8998, l8999, l9000, l9001, l9002, l9003, l9004, l9005, l9006, l9007;
  let l9008, l9009, l9010, l9011, l9012, l9013, l9014, l9015, l9016, l9017, l9018, l9019, l9020, l9021, l9022, l9023;
  let l9024, l9025, l9026, l9027, l9028, l9029, l9030, l9031, l9032, l9033, l9034, l9035, l9036, l9037, l9038, l9039;
  let l9040, l9041, l9042, l9043, l9044, l9045, l9046, l9047, l9048, l9049, l9050, l9051, l9052, l9053, l9054, l9055;
  let l9056, l9057, l9058, l9059, l9060, l9061, l9062, l9063, l9064, l9065, l9066, l9067, l9068, l9069, l9070, l9071;
  let l9072, l9073, l9074, l9075, l9076, l9077, l9078, l9079, l9080, l9081, l9082, l9083, l9084, l9085, l9086, l9087;
  let l9088, l9089, l9090, l9091, l9092, l9093, l9094, l9095, l9096, l9097, l9098, l9099, l9100, l9101, l9102, l9103;
  let l9104, l9105, l9106, l9107, l9108, l9109, l9110, l9111, l9112, l9113, l9114, l9115, l9116, l9117, l9118, l9119;
  let l9120, l9121, l9122, l9123, l9124, l9125, l9126, l9127, l9128, l9129, l9130, l9131, l9132, l9133, l9134, l9135;
  let l9136, l9137, l9138, l9139, l9140, l9141, l9142, l9143, l9144, l9145, l9146, l9147, l9148, l9149, l9150, l9151;
  let l9152, l9153, l9154, l9155, l9156, l9157, l9158, l9159, l9160, l9161, l9162, l9163, l9164, l9165, l9166, l9167;
  let l9168, l9169, l9170, l9171, l9172, l9173, l9174, l9175, l9176, l9177, l9178, l9179, l9180, l9181, l9182, l9183;
  let l9184, l9185, l9186, l9187, l9188, l9189, l9190, l9191, l9192, l9193, l9194, l9195, l9196, l9197, l9198, l9199;
  let l9200, l9201, l9202, l9203, l9204, l9205, l9206, l9207, l9208, l9209, l9210, l9211, l9212, l9213, l9214, l9215;
  let l9216, l9217, l9218, l9219, l9220, l9221, l9222, l9223, l9224, l9225, l9226, l9227, l9228, l9229, l9230, l9231;
  let l9232, l9233, l9234, l9235, l9236, l9237, l9238, l9239, l9240, l9241, l9242, l9243, l9244, l9245, l9246, l9247;
  let l9248, l9249, l9250, l9251, l9252, l9253, l9254, l9255, l9256, l9257, l9258, l9259, l9260, l9261, l9262, l9263;
  let l9264, l9265, l9266, l9267, l9268, l9269, l9270, l9271, l9272, l9273, l9274, l9275, l9276, l9277, l9278, l9279;
  let l9280, l9281, l9282, l9283, l9284, l9285, l9286, l9287, l9288, l9289, l9290, l9291, l9292, l9293, l9294, l9295;
  let l9296, l9297, l9298, l9299, l9300, l9301, l9302, l9303, l9304, l9305, l9306, l9307, l9308, l9309, l9310, l9311;
  let l9312, l9313, l9314, l9315, l9316, l9317, l9318, l9319, l9320, l9321, l9322, l9323, l9324, l9325, l9326, l9327;
  let l9328, l9329, l9330, l9331, l9332, l9333, l9334, l9335, l9336, l9337, l9338, l9339, l9340, l9341, l9342, l9343;
  let l9344, l9345, l9346, l9347, l9348, l9349, l9350, l9351, l9352, l9353, l9354, l9355, l9356, l9357, l9358, l9359;
  let l9360, l9361, l9362, l9363, l9364, l9365, l9366, l9367, l9368, l9369, l9370, l9371, l9372, l9373, l9374, l9375;
  let l9376, l9377, l9378, l9379, l9380, l9381, l9382, l9383, l9384, l9385, l9386, l9387, l9388, l9389, l9390, l9391;
  let l9392, l9393, l9394, l9395, l9396, l9397, l9398, l9399, l9400, l9401, l9402, l9403, l9404, l9405, l9406, l9407;
  let l9408, l9409, l9410, l9411, l9412, l9413, l9414, l9415, l9416, l9417, l9418, l9419, l9420, l9421, l9422, l9423;
  let l9424, l9425, l9426, l9427, l9428, l9429, l9430, l9431, l9432, l9433, l9434, l9435, l9436, l9437, l9438, l9439;
  let l9440, l9441, l9442, l9443, l9444, l9445, l9446, l9447, l9448, l9449, l9450, l9451, l9452, l9453, l9454, l9455;
  let l9456, l9457, l9458, l9459, l9460, l9461, l9462, l9463, l9464, l9465, l9466, l9467, l9468, l9469, l9470, l9471;
  let l9472, l9473, l9474, l9475, l9476, l9477, l9478, l9479, l9480, l9481, l9482, l9483, l9484, l9485, l9486, l9487;
  let l9488, l9489, l9490, l9491, l9492, l9493, l9494, l9495, l9496, l9497, l9498, l9499, l9500, l9501, l9502, l9503;
  let l9504, l9505, l9506, l9507, l9508, l9509, l9510, l9511, l9512, l9513, l9514, l9515, l9516, l9517, l9518, l9519;
  let l9520, l9521, l9522, l9523, l9524, l9525, l9526, l9527, l9528, l9529, l9530, l9531, l9532, l9533, l9534, l9535;
  let l9536, l9537, l9538, l9539, l9540, l9541, l9542, l9543, l9544, l9545, l9546, l9547, l9548, l9549, l9550, l9551;
  let l9552, l9553, l9554, l9555, l9556, l9557, l9558, l9559, l9560, l9561, l9562, l9563, l9564, l9565;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 13; { const c13 = cell(state, 0, "", input.tick);
      if (c13.value !== undefined) l12 = c13.value; else {
        at = 2; l1 = K[1];
        at = 3; l2 = K[2];
        at = 4; l3 = K[3];
        at = 5; l4 = K[4];
        at = 6; l5 = K[5];
        at = 7; l6 = new Map([["color", l3], ["width", l4], ["points", l5]]);
        at = 8; l7 = K[6];
        at = 9; l8 = K[7];
        at = 10; l9 = K[8];
        at = 11; l10 = K[9];
        at = 12; l11 = new Map([["strokes", l1], ["active", l2], ["current", l6], ["color", l7], ["width", l8], ["status", l9], ["save", l10]]);
        l12 = l11;
      } }
    at = 14; { const c14 = cell(state, 0, "", input.tick); l13 = c14.pending !== undefined; l14 = l13 ? c14.pending : null; }
    at = 15; if (rt.truthy(l13)) {
      at = 16; l15 = l14;
    } else {
      at = 17; l16 = field(rt, l12, "strokes");
      at = 18; l17 = field(rt, l12, "active");
      at = 19; l18 = field(rt, l12, "current");
      at = 20; l19 = field(rt, l12, "color");
      at = 21; l20 = field(rt, l12, "width");
      at = 22; l21 = field(rt, l12, "status");
      at = 23; l22 = field(rt, l12, "save");
      at = 24; l23 = l16;
      at = 25; l24 = l17;
      at = 26; l25 = l18;
      at = 27; l26 = l19;
      at = 28; l27 = l20;
      at = 29; l28 = l21;
      at = 30; l29 = K[10];
      at = 31; l30 = l29;
      at = 32; l31 = K[11];
      at = 33; l32 = l31;
      at = 34; l33 = K[12];
      at = 35; l34 = l33;
      at = 36; l35 = rt.frameValue(input);
      at = 37; l36 = field(rt, l35, "events");
      at = 38; {
        const s38 = l36, c38 = Array.isArray(s38) ? s38.length : Number(rt.array_count(s38));
        if (c38 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i38 = 0; i38 < c38; i38++) {
          l37 = BigInt(i38); l38 = Array.isArray(s38) ? s38[i38] : rt.array_nth(s38, i38);
          at = 39; l39 = K[13];
          at = 40; l40 = rt.ge(l32, l39);
          at = 41; if (rt.truthy(l40)) {
            at = 42; l42 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 43; l41 = l42;
          } else {
            at = 44; l43 = K[14];
            at = 45; l41 = l43;
          }
          at = 46; l44 = field(rt, l38, "position");
          at = 47; l45 = field(rt, l44, "x");
          at = 48; l46 = rt.int(l45);
          at = 49; l47 = field(rt, l38, "position");
          at = 50; l48 = field(rt, l47, "y");
          at = 51; l49 = rt.int(l48);
          at = 52; l50 = K[15];
          at = 53; l51 = Float64Array.of(rt.float(l46), rt.float(l49), rt.float(l50));
          at = 54; l52 = field(rt, l38, "kind");
          at = 55; l53 = K[16];
          at = 56; l54 = rt.equal(l52, l53);
          at = 57; l55 = field(rt, l38, "button");
          at = 58; l56 = K[17];
          at = 59; l57 = rt.equal(l55, l56);
          at = 60; l58 = rt.and(l54, l57);
          at = 61; if (rt.truthy(l58)) {
            at = 62; l59 = field(rt, l51, "y");
            at = 63; l60 = K[18];
            at = 64; l61 = rt.lt(l59, l60);
            at = 65; if (rt.truthy(l61)) {
              at = 66; l62 = field(rt, l51, "x");
              at = 67; l63 = K[19];
              at = 68; l64 = rt.ge(l62, l63);
              at = 69; l65 = field(rt, l51, "x");
              at = 70; l66 = K[20];
              at = 71; l67 = rt.lt(l65, l66);
              at = 72; l68 = rt.and(l64, l67);
              at = 73; l69 = field(rt, l51, "y");
              at = 74; l70 = K[21];
              at = 75; l71 = rt.ge(l69, l70);
              at = 76; l72 = field(rt, l51, "y");
              at = 77; l73 = K[22];
              at = 78; l74 = rt.lt(l72, l73);
              at = 79; l75 = rt.and(l71, l74);
              at = 80; l76 = rt.and(l68, l75);
              at = 81; if (rt.truthy(l76)) {
                at = 82; l77 = rt.sequence_length(l23);
                at = 83; l78 = K[23];
                at = 84; l79 = rt.eq(l77, l78);
                at = 85; if (rt.truthy(l79)) {
                  at = 86; l81 = K[24];
                  at = 87; l80 = l81;
                } else {
                  at = 88; l82 = K[25];
                  at = 89; l83 = rt.sequence_get(l23, l82);
                  at = 90; l84 = K[26];
                  at = 91; l85 = rt.sequence_build(l84);
                  at = 92; l86 = K[27];
                  at = 93; {
                    const s93 = l86, c93 = Array.isArray(s93) ? s93.length : Number(rt.array_count(s93));
                    if (c93 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                    for (let i93 = 0; i93 < c93; i93++) {
                      l87 = BigInt(i93); l88 = Array.isArray(s93) ? s93[i93] : rt.array_nth(s93, i93);
                      at = 94; store(rt, l85, l87, l88);
                    }
                  }
                  at = 95; l80 = l85;
                }
                at = 407; l326 = l80;
                at = 409; l328 = K[90];
                at = 410; l327 = l328;
                at = 412; l329 = l26;
                at = 414; l330 = l27;
                at = 416; l332 = K[91];
                at = 417; l331 = l332;
                at = 419; l333 = l30;
              } else {
                at = 96; l89 = field(rt, l51, "x");
                at = 97; l90 = K[28];
                at = 98; l91 = rt.ge(l89, l90);
                at = 99; l92 = field(rt, l51, "x");
                at = 100; l93 = K[29];
                at = 101; l94 = rt.lt(l92, l93);
                at = 102; l95 = rt.and(l91, l94);
                at = 103; l96 = field(rt, l51, "y");
                at = 104; l97 = K[30];
                at = 105; l98 = rt.ge(l96, l97);
                at = 106; l99 = field(rt, l51, "y");
                at = 107; l100 = K[31];
                at = 108; l101 = rt.lt(l99, l100);
                at = 109; l102 = rt.and(l98, l101);
                at = 110; l103 = rt.and(l95, l102);
                at = 111; if (rt.truthy(l103)) {
                  at = 112; l104 = rt.sequence_length(l23);
                  at = 113; l105 = K[32];
                  at = 114; l106 = K[33];
                  at = 115; l107 = rt.int_lt(l105, l106);
                  at = 116; if (rt.truthy(l107)) {
                    at = 117; l109 = K[34];
                    at = 118; l108 = l109;
                  } else {
                    at = 119; l110 = K[35];
                    at = 120; l108 = l110;
                  }
                  at = 121; l111 = rt.int_gt(l108, l104);
                  at = 122; if (rt.truthy(l111)) {
                    at = 123; l112 = l104;
                  } else {
                    at = 124; l112 = l108;
                  }
                  at = 125; l113 = rt.int_sub(l104, l112);
                  at = 126; l114 = rt.sequence_slice(l23, l112, l113);
                  at = 127; l115 = rt.sequence_length(l23);
                  at = 128; l116 = K[36];
                  at = 129; l117 = rt.eq(l115, l116);
                  at = 130; if (rt.truthy(l117)) {
                    at = 131; l118 = l114;
                  } else {
                    at = 132; l119 = K[37];
                    at = 133; l120 = rt.sequence_get(l23, l119);
                    at = 134; l121 = rt.sequence_length(l114);
                    at = 135; l122 = rt.sequence_build(l121);
                    at = 136; {
                      const s136 = l114, c136 = Array.isArray(s136) ? s136.length : Number(rt.array_count(s136));
                      if (c136 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                      for (let i136 = 0; i136 < c136; i136++) {
                        l123 = BigInt(i136); l124 = Array.isArray(s136) ? s136[i136] : rt.array_nth(s136, i136);
                        at = 137; store(rt, l122, l123, l124);
                      }
                    }
                    at = 138; l118 = l122;
                  }
                  at = 393; l318 = l118;
                  at = 395; l320 = K[88];
                  at = 396; l319 = l320;
                  at = 398; l321 = l26;
                  at = 400; l322 = l27;
                  at = 402; l324 = K[89];
                  at = 403; l323 = l324;
                  at = 405; l325 = l30;
                } else {
                  at = 139; l125 = field(rt, l51, "x");
                  at = 140; l126 = K[38];
                  at = 141; l127 = rt.ge(l125, l126);
                  at = 142; l128 = field(rt, l51, "x");
                  at = 143; l129 = K[39];
                  at = 144; l130 = rt.lt(l128, l129);
                  at = 145; l131 = rt.and(l127, l130);
                  at = 146; l132 = field(rt, l51, "y");
                  at = 147; l133 = K[40];
                  at = 148; l134 = rt.ge(l132, l133);
                  at = 149; l135 = field(rt, l51, "y");
                  at = 150; l136 = K[41];
                  at = 151; l137 = rt.lt(l135, l136);
                  at = 152; l138 = rt.and(l134, l137);
                  at = 153; l139 = rt.and(l131, l138);
                  at = 154; if (rt.truthy(l139)) {
                    at = 378; l309 = l23;
                    at = 380; l311 = K[85];
                    at = 381; l310 = l311;
                    at = 383; l312 = l26;
                    at = 385; l313 = l27;
                    at = 387; l315 = K[86];
                    at = 388; l314 = l315;
                    at = 390; l317 = K[87];
                    at = 391; l316 = l317;
                  } else {
                    at = 155; l140 = field(rt, l51, "x");
                    at = 156; l141 = K[42];
                    at = 157; l142 = rt.ge(l140, l141);
                    at = 158; l143 = field(rt, l51, "x");
                    at = 159; l144 = K[43];
                    at = 160; l145 = rt.lt(l143, l144);
                    at = 161; l146 = rt.and(l142, l145);
                    at = 162; l147 = field(rt, l51, "y");
                    at = 163; l148 = K[44];
                    at = 164; l149 = rt.ge(l147, l148);
                    at = 165; l150 = field(rt, l51, "y");
                    at = 166; l151 = K[45];
                    at = 167; l152 = rt.lt(l150, l151);
                    at = 168; l153 = rt.and(l149, l152);
                    at = 169; l154 = rt.and(l146, l153);
                    at = 170; if (rt.truthy(l154)) {
                      at = 171; l155 = rt.coerce("float", l26);
                      at = 172; l156 = rt.equal(l26, l155);
                      at = 173; if (rt.truthy(l156)) {
                        at = 174; l158 = K[46];
                        at = 175; l157 = l158;
                      } else {
                        at = 176; l159 = K[47];
                        at = 177; l157 = l159;
                      }
                      at = 365; l302 = l23;
                      at = 367; l304 = K[84];
                      at = 368; l303 = l304;
                      at = 370; l305 = l157;
                      at = 372; l306 = l27;
                      at = 374; l307 = l28;
                      at = 376; l308 = l30;
                    } else {
                      at = 178; l160 = field(rt, l51, "x");
                      at = 179; l161 = K[48];
                      at = 180; l162 = rt.ge(l160, l161);
                      at = 181; l163 = field(rt, l51, "x");
                      at = 182; l164 = K[49];
                      at = 183; l165 = rt.lt(l163, l164);
                      at = 184; l166 = rt.and(l162, l165);
                      at = 185; l167 = field(rt, l51, "y");
                      at = 186; l168 = K[50];
                      at = 187; l169 = rt.ge(l167, l168);
                      at = 188; l170 = field(rt, l51, "y");
                      at = 189; l171 = K[51];
                      at = 190; l172 = rt.lt(l170, l171);
                      at = 191; l173 = rt.and(l169, l172);
                      at = 192; l174 = rt.and(l166, l173);
                      at = 193; if (rt.truthy(l174)) {
                        at = 194; l175 = rt.coerce("float", l26);
                        at = 195; l176 = rt.equal(l26, l175);
                        at = 196; if (rt.truthy(l176)) {
                          at = 197; l178 = K[52];
                          at = 198; l177 = l178;
                        } else {
                          at = 199; l179 = K[53];
                          at = 200; l177 = l179;
                        }
                        at = 352; l295 = l23;
                        at = 354; l297 = K[83];
                        at = 355; l296 = l297;
                        at = 357; l298 = l177;
                        at = 359; l299 = l27;
                        at = 361; l300 = l28;
                        at = 363; l301 = l30;
                      } else {
                        at = 201; l180 = field(rt, l51, "x");
                        at = 202; l181 = K[54];
                        at = 203; l182 = rt.ge(l180, l181);
                        at = 204; l183 = field(rt, l51, "x");
                        at = 205; l184 = K[55];
                        at = 206; l185 = rt.lt(l183, l184);
                        at = 207; l186 = rt.and(l182, l185);
                        at = 208; l187 = field(rt, l51, "y");
                        at = 209; l188 = K[56];
                        at = 210; l189 = rt.ge(l187, l188);
                        at = 211; l190 = field(rt, l51, "y");
                        at = 212; l191 = K[57];
                        at = 213; l192 = rt.lt(l190, l191);
                        at = 214; l193 = rt.and(l189, l192);
                        at = 215; l194 = rt.and(l186, l193);
                        at = 216; if (rt.truthy(l194)) {
                          at = 217; l195 = rt.coerce("float", l26);
                          at = 218; l196 = rt.equal(l26, l195);
                          at = 219; if (rt.truthy(l196)) {
                            at = 220; l198 = K[58];
                            at = 221; l197 = l198;
                          } else {
                            at = 222; l199 = K[59];
                            at = 223; l197 = l199;
                          }
                          at = 339; l288 = l23;
                          at = 341; l290 = K[82];
                          at = 342; l289 = l290;
                          at = 344; l291 = l197;
                          at = 346; l292 = l27;
                          at = 348; l293 = l28;
                          at = 350; l294 = l30;
                        } else {
                          at = 224; l200 = field(rt, l51, "x");
                          at = 225; l201 = K[60];
                          at = 226; l202 = rt.ge(l200, l201);
                          at = 227; l203 = field(rt, l51, "x");
                          at = 228; l204 = K[61];
                          at = 229; l205 = rt.lt(l203, l204);
                          at = 230; l206 = rt.and(l202, l205);
                          at = 231; l207 = field(rt, l51, "y");
                          at = 232; l208 = K[62];
                          at = 233; l209 = rt.ge(l207, l208);
                          at = 234; l210 = field(rt, l51, "y");
                          at = 235; l211 = K[63];
                          at = 236; l212 = rt.lt(l210, l211);
                          at = 237; l213 = rt.and(l209, l212);
                          at = 238; l214 = rt.and(l206, l213);
                          at = 239; if (rt.truthy(l214)) {
                            at = 240; l215 = rt.coerce("float", l26);
                            at = 241; l216 = rt.equal(l26, l215);
                            at = 242; if (rt.truthy(l216)) {
                              at = 243; l218 = K[64];
                              at = 244; l217 = l218;
                            } else {
                              at = 245; l219 = K[65];
                              at = 246; l217 = l219;
                            }
                            at = 326; l281 = l23;
                            at = 328; l283 = K[81];
                            at = 329; l282 = l283;
                            at = 331; l284 = l217;
                            at = 333; l285 = l27;
                            at = 335; l286 = l28;
                            at = 337; l287 = l30;
                          } else {
                            at = 247; l220 = field(rt, l51, "x");
                            at = 248; l221 = K[66];
                            at = 249; l222 = rt.ge(l220, l221);
                            at = 250; l223 = field(rt, l51, "x");
                            at = 251; l224 = K[67];
                            at = 252; l225 = rt.lt(l223, l224);
                            at = 253; l226 = rt.and(l222, l225);
                            at = 254; l227 = field(rt, l51, "y");
                            at = 255; l228 = K[68];
                            at = 256; l229 = rt.ge(l227, l228);
                            at = 257; l230 = field(rt, l51, "y");
                            at = 258; l231 = K[69];
                            at = 259; l232 = rt.lt(l230, l231);
                            at = 260; l233 = rt.and(l229, l232);
                            at = 261; l234 = rt.and(l226, l233);
                            at = 262; if (rt.truthy(l234)) {
                              at = 263; l235 = K[70];
                              at = 264; l236 = rt.sub(l27, l235);
                              at = 265; l237 = K[71];
                              at = 266; l238 = rt.max(l237, l236);
                              at = 267; l239 = rt.coerce("float", l27);
                              at = 268; l240 = rt.equal(l27, l239);
                              at = 269; if (rt.truthy(l240)) {
                                at = 270; l242 = rt.coerce("float", l238);
                                at = 271; l241 = l242;
                              } else {
                                at = 272; l241 = l238;
                              }
                              at = 313; l274 = l23;
                              at = 315; l276 = K[80];
                              at = 316; l275 = l276;
                              at = 318; l277 = l26;
                              at = 320; l278 = l241;
                              at = 322; l279 = l28;
                              at = 324; l280 = l30;
                            } else {
                              at = 273; l243 = field(rt, l51, "x");
                              at = 274; l244 = K[72];
                              at = 275; l245 = rt.ge(l243, l244);
                              at = 276; l246 = field(rt, l51, "x");
                              at = 277; l247 = K[73];
                              at = 278; l248 = rt.lt(l246, l247);
                              at = 279; l249 = rt.and(l245, l248);
                              at = 280; l250 = field(rt, l51, "y");
                              at = 281; l251 = K[74];
                              at = 282; l252 = rt.ge(l250, l251);
                              at = 283; l253 = field(rt, l51, "y");
                              at = 284; l254 = K[75];
                              at = 285; l255 = rt.lt(l253, l254);
                              at = 286; l256 = rt.and(l252, l255);
                              at = 287; l257 = rt.and(l249, l256);
                              at = 288; if (rt.truthy(l257)) {
                                at = 289; l258 = K[76];
                                at = 290; l259 = rt.add(l27, l258);
                                at = 291; l260 = K[77];
                                at = 292; l261 = rt.min(l260, l259);
                                at = 293; l262 = rt.coerce("float", l27);
                                at = 294; l263 = rt.equal(l27, l262);
                                at = 295; if (rt.truthy(l263)) {
                                  at = 296; l265 = rt.coerce("float", l261);
                                  at = 297; l264 = l265;
                                } else {
                                  at = 298; l264 = l261;
                                }
                                at = 299; l266 = l23;
                                at = 301; l268 = K[78];
                                at = 302; l267 = l268;
                                at = 305; l270 = l26;
                                at = 307; l271 = l264;
                                at = 309; l272 = l28;
                                at = 311; l273 = l30;
                              } else {
                                at = 300; l266 = l23;
                                at = 303; l269 = K[79];
                                at = 304; l267 = l269;
                                at = 306; l270 = l26;
                                at = 308; l271 = l27;
                                at = 310; l272 = l28;
                                at = 312; l273 = l30;
                              }
                              at = 314; l274 = l266;
                              at = 317; l275 = l267;
                              at = 319; l277 = l270;
                              at = 321; l278 = l271;
                              at = 323; l279 = l272;
                              at = 325; l280 = l273;
                            }
                            at = 327; l281 = l274;
                            at = 330; l282 = l275;
                            at = 332; l284 = l277;
                            at = 334; l285 = l278;
                            at = 336; l286 = l279;
                            at = 338; l287 = l280;
                          }
                          at = 340; l288 = l281;
                          at = 343; l289 = l282;
                          at = 345; l291 = l284;
                          at = 347; l292 = l285;
                          at = 349; l293 = l286;
                          at = 351; l294 = l287;
                        }
                        at = 353; l295 = l288;
                        at = 356; l296 = l289;
                        at = 358; l298 = l291;
                        at = 360; l299 = l292;
                        at = 362; l300 = l293;
                        at = 364; l301 = l294;
                      }
                      at = 366; l302 = l295;
                      at = 369; l303 = l296;
                      at = 371; l305 = l298;
                      at = 373; l306 = l299;
                      at = 375; l307 = l300;
                      at = 377; l308 = l301;
                    }
                    at = 379; l309 = l302;
                    at = 382; l310 = l303;
                    at = 384; l312 = l305;
                    at = 386; l313 = l306;
                    at = 389; l314 = l307;
                    at = 392; l316 = l308;
                  }
                  at = 394; l318 = l309;
                  at = 397; l319 = l310;
                  at = 399; l321 = l312;
                  at = 401; l322 = l313;
                  at = 404; l323 = l314;
                  at = 406; l325 = l316;
                }
                at = 408; l326 = l318;
                at = 411; l327 = l319;
                at = 413; l329 = l321;
                at = 415; l330 = l322;
                at = 418; l331 = l323;
                at = 420; l333 = l325;
              }
              at = 428; l341 = l326;
              at = 430; l342 = l327;
              at = 433; l344 = l329;
              at = 435; l345 = l330;
              at = 437; l346 = l331;
              at = 440; l348 = l333;
            } else {
              at = 421; l334 = K[92];
              at = 422; l335 = rt.array_vec3(l334, l51);
              at = 423; l336 = field(rt, l25, "color");
              at = 424; l337 = rt.coerce("int", l26);
              at = 425; l338 = field(rt, l25, "width");
              at = 426; l339 = rt.coerce("int", l27);
              at = 427; l340 = field(rt, l25, "points");
              at = 429; l341 = l23;
              at = 431; l343 = K[93];
              at = 432; l342 = l343;
              at = 434; l344 = l26;
              at = 436; l345 = l27;
              at = 438; l347 = K[94];
              at = 439; l346 = l347;
              at = 441; l348 = l30;
            }
            at = 920; l695 = l341;
            at = 922; l696 = l342;
            at = 924; l697 = l344;
            at = 926; l698 = l345;
            at = 928; l699 = l346;
            at = 930; l700 = l348;
          } else {
            at = 442; l349 = field(rt, l38, "kind");
            at = 443; l350 = K[95];
            at = 444; l351 = rt.equal(l349, l350);
            at = 445; if (rt.truthy(l351)) {
              at = 446; if (rt.truthy(l24)) {
                at = 447; l352 = field(rt, l25, "points");
                at = 448; l353 = rt.array_count(l352);
                at = 449; l354 = K[96];
                at = 450; l355 = rt.ge(l353, l354);
                at = 451; if (rt.truthy(l355)) {
                } else {
                  at = 452; l356 = K[97];
                  at = 453; l357 = rt.eq(l353, l356);
                  at = 454; if (rt.truthy(l357)) {
                    at = 455; l358 = K[98];
                    at = 456; l359 = rt.array_vec3(l358, l51);
                    at = 457; l360 = field(rt, l25, "color");
                    at = 458; l361 = field(rt, l25, "width");
                    at = 459; l362 = field(rt, l25, "points");
                  } else {
                    at = 460; l363 = field(rt, l25, "points");
                    at = 461; l364 = K[99];
                    at = 462; l365 = rt.sub(l353, l364);
                    at = 463; l366 = rt.array_nth(l363, l365);
                    at = 464; l367 = field(rt, l51, "x");
                    at = 465; l368 = field(rt, l366, "x");
                    at = 466; l369 = rt.sub(l367, l368);
                    at = 467; l370 = field(rt, l51, "y");
                    at = 468; l371 = field(rt, l366, "y");
                    at = 469; l372 = rt.sub(l370, l371);
                    at = 470; l373 = rt.mul(l369, l369);
                    at = 471; l374 = rt.mul(l372, l372);
                    at = 472; l375 = rt.add(l373, l374);
                    at = 473; l376 = K[100];
                    at = 474; l377 = rt.lt(l375, l376);
                    at = 475; if (rt.truthy(l377)) {
                    } else {
                      at = 476; l378 = field(rt, l25, "points");
                      at = 477; l379 = K[101];
                      at = 478; l380 = rt.array_vec3(l379, l51);
                      at = 479; l381 = rt.array_concat(l378, l380);
                      at = 480; l382 = field(rt, l25, "color");
                      at = 481; l383 = field(rt, l25, "width");
                      at = 482; l384 = field(rt, l25, "points");
                    }
                  }
                }
                at = 483; if (rt.truthy(l355)) {
                  at = 484; l385 = field(rt, l25, "color");
                  at = 485; l386 = field(rt, l25, "width");
                  at = 486; l387 = field(rt, l25, "points");
                  at = 487; l388 = field(rt, l25, "color");
                  at = 488; l389 = field(rt, l25, "width");
                  at = 489; l390 = field(rt, l25, "points");
                  at = 516; l409 = l385;
                  at = 518; l410 = l386;
                  at = 520; l411 = l387;
                } else {
                  at = 490; if (rt.truthy(l357)) {
                    at = 491; l391 = field(rt, l25, "color");
                    at = 492; l392 = field(rt, l25, "width");
                    at = 493; l393 = field(rt, l25, "points");
                    at = 510; l406 = l360;
                    at = 512; l407 = l361;
                    at = 514; l408 = l359;
                  } else {
                    at = 494; if (rt.truthy(l377)) {
                      at = 495; l394 = field(rt, l25, "color");
                      at = 496; l395 = field(rt, l25, "width");
                      at = 497; l396 = field(rt, l25, "points");
                      at = 498; l397 = field(rt, l25, "color");
                      at = 499; l398 = field(rt, l25, "width");
                      at = 500; l399 = field(rt, l25, "points");
                      at = 504; l403 = l394;
                      at = 506; l404 = l395;
                      at = 508; l405 = l396;
                    } else {
                      at = 501; l400 = field(rt, l25, "color");
                      at = 502; l401 = field(rt, l25, "width");
                      at = 503; l402 = field(rt, l25, "points");
                      at = 505; l403 = l382;
                      at = 507; l404 = l383;
                      at = 509; l405 = l381;
                    }
                    at = 511; l406 = l403;
                    at = 513; l407 = l404;
                    at = 515; l408 = l405;
                  }
                  at = 517; l409 = l406;
                  at = 519; l410 = l407;
                  at = 521; l411 = l408;
                }
                at = 522; l412 = l23;
                at = 524; l413 = l24;
                at = 526; l414 = l26;
                at = 528; l415 = l27;
                at = 530; l416 = l28;
                at = 532; l417 = l30;
              } else {
                at = 523; l412 = l23;
                at = 525; l413 = l24;
                at = 527; l414 = l26;
                at = 529; l415 = l27;
                at = 531; l416 = l28;
                at = 533; l417 = l30;
              }
              at = 908; l689 = l412;
              at = 910; l690 = l413;
              at = 912; l691 = l414;
              at = 914; l692 = l415;
              at = 916; l693 = l416;
              at = 918; l694 = l417;
            } else {
              at = 534; l418 = field(rt, l38, "kind");
              at = 535; l419 = K[102];
              at = 536; l420 = rt.equal(l418, l419);
              at = 537; l421 = field(rt, l38, "button");
              at = 538; l422 = K[103];
              at = 539; l423 = rt.equal(l421, l422);
              at = 540; l424 = rt.and(l420, l423);
              at = 541; if (rt.truthy(l424)) {
                at = 542; l425 = rt.coerce({list: {record: [["color", "int"], ["width", "int"], ["points", {array: "vec3"}]]}}, l23);
                at = 543; l426 = rt.coerce("int", l26);
                at = 544; l427 = rt.coerce("int", l27);
                at = 545; if (rt.truthy(l24)) {
                  at = 546; l428 = field(rt, l25, "points");
                  at = 547; l429 = rt.array_count(l428);
                  at = 548; l430 = K[104];
                  at = 549; l431 = rt.ge(l429, l430);
                  at = 550; if (rt.truthy(l431)) {
                  } else {
                    at = 551; l432 = K[105];
                    at = 552; l433 = rt.eq(l429, l432);
                    at = 553; if (rt.truthy(l433)) {
                      at = 554; l434 = K[106];
                      at = 555; l435 = rt.array_vec3(l434, l51);
                      at = 556; l436 = field(rt, l25, "color");
                      at = 557; l437 = field(rt, l25, "width");
                      at = 558; l438 = field(rt, l25, "points");
                    } else {
                      at = 559; l439 = field(rt, l25, "points");
                      at = 560; l440 = K[107];
                      at = 561; l441 = rt.sub(l429, l440);
                      at = 562; l442 = rt.array_nth(l439, l441);
                      at = 563; l443 = field(rt, l51, "x");
                      at = 564; l444 = field(rt, l442, "x");
                      at = 565; l445 = rt.sub(l443, l444);
                      at = 566; l446 = field(rt, l51, "y");
                      at = 567; l447 = field(rt, l442, "y");
                      at = 568; l448 = rt.sub(l446, l447);
                      at = 569; l449 = rt.mul(l445, l445);
                      at = 570; l450 = rt.mul(l448, l448);
                      at = 571; l451 = rt.add(l449, l450);
                      at = 572; l452 = K[108];
                      at = 573; l453 = rt.lt(l451, l452);
                      at = 574; if (rt.truthy(l453)) {
                      } else {
                        at = 575; l454 = field(rt, l25, "points");
                        at = 576; l455 = K[109];
                        at = 577; l456 = rt.array_vec3(l455, l51);
                        at = 578; l457 = rt.array_concat(l454, l456);
                        at = 579; l458 = field(rt, l25, "color");
                        at = 580; l459 = field(rt, l25, "width");
                        at = 581; l460 = field(rt, l25, "points");
                      }
                    }
                  }
                  at = 582; if (rt.truthy(l431)) {
                    at = 583; l461 = l25;
                  } else {
                    at = 584; if (rt.truthy(l433)) {
                      at = 585; l463 = new Map([["color", l436], ["width", l437], ["points", l435]]);
                      at = 586; l462 = l463;
                    } else {
                      at = 587; if (rt.truthy(l453)) {
                        at = 588; l464 = l25;
                      } else {
                        at = 589; l465 = new Map([["color", l458], ["width", l459], ["points", l457]]);
                        at = 590; l464 = l465;
                      }
                      at = 591; l462 = l464;
                    }
                    at = 592; l461 = l462;
                  }
                  at = 593; l466 = rt.sequence_concat([l461]);
                  at = 594; l467 = rt.sequence_concat(l466, l425);
                  at = 595; l468 = rt.sequence_length(l467);
                  at = 596; l469 = K[110];
                  at = 597; l470 = K[111];
                  at = 598; l471 = rt.int_lt(l469, l470);
                  at = 599; if (rt.truthy(l471)) {
                    at = 600; l473 = K[112];
                    at = 601; l472 = l473;
                  } else {
                    at = 602; l474 = K[113];
                    at = 603; l472 = l474;
                  }
                  at = 604; l475 = rt.int_gt(l472, l468);
                  at = 605; if (rt.truthy(l475)) {
                    at = 606; l476 = l468;
                  } else {
                    at = 607; l476 = l472;
                  }
                  at = 608; l477 = K[114];
                  at = 609; l478 = rt.sequence_slice(l467, l477, l476);
                  at = 610; l479 = rt.sequence_length(l425);
                  at = 611; l480 = K[115];
                  at = 612; l481 = rt.eq(l479, l480);
                  at = 613; if (rt.truthy(l481)) {
                    at = 614; l482 = l478;
                  } else {
                    at = 615; l483 = K[116];
                    at = 616; l484 = rt.sequence_get(l425, l483);
                    at = 617; l485 = rt.sequence_length(l478);
                    at = 618; l486 = rt.sequence_build(l485);
                    at = 619; {
                      const s619 = l478, c619 = Array.isArray(s619) ? s619.length : Number(rt.array_count(s619));
                      if (c619 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                      for (let i619 = 0; i619 < c619; i619++) {
                        l487 = BigInt(i619); l488 = Array.isArray(s619) ? s619[i619] : rt.array_nth(s619, i619);
                        at = 620; l489 = field(rt, l488, "color");
                        at = 621; l490 = field(rt, l488, "width");
                        at = 622; l491 = field(rt, l488, "points");
                        at = 623; l492 = field(rt, l484, "color");
                        at = 624; l493 = field(rt, l484, "width");
                        at = 625; l494 = field(rt, l484, "points");
                        at = 626; l495 = new Map([["color", l489], ["width", l490], ["points", l491]]);
                        at = 627; store(rt, l486, l487, l495);
                      }
                    }
                    at = 628; l482 = l486;
                  }
                  at = 629; l496 = l482;
                  at = 631; l498 = K[117];
                  at = 632; l497 = l498;
                  at = 634; l499 = l426;
                  at = 636; l500 = l427;
                  at = 638; l502 = K[118];
                  at = 639; l501 = l502;
                  at = 641; l503 = l30;
                } else {
                  at = 630; l496 = l425;
                  at = 633; l497 = l24;
                  at = 635; l499 = l426;
                  at = 637; l500 = l427;
                  at = 640; l501 = l28;
                  at = 642; l503 = l30;
                }
                at = 896; l683 = l496;
                at = 898; l684 = l497;
                at = 900; l685 = l499;
                at = 902; l686 = l500;
                at = 904; l687 = l501;
                at = 906; l688 = l503;
              } else {
                at = 643; l504 = field(rt, l38, "kind");
                at = 644; l505 = K[119];
                at = 645; l506 = rt.equal(l504, l505);
                at = 646; l507 = field(rt, l38, "button");
                at = 647; l508 = K[120];
                at = 648; l509 = rt.equal(l507, l508);
                at = 649; l510 = rt.and(l506, l509);
                at = 650; l511 = field(rt, l38, "kind");
                at = 651; l512 = K[121];
                at = 652; l513 = rt.equal(l511, l512);
                at = 653; l514 = rt.or(l510, l513);
                at = 654; if (rt.truthy(l514)) {
                  at = 655; l515 = rt.coerce({list: {record: [["color", "int"], ["width", "int"], ["points", {array: "vec3"}]]}}, l23);
                  at = 656; l516 = rt.coerce("int", l26);
                  at = 657; l517 = rt.coerce("int", l27);
                  at = 658; if (rt.truthy(l24)) {
                    at = 659; l518 = rt.sequence_concat([l25]);
                    at = 660; l519 = rt.sequence_concat(l518, l515);
                    at = 661; l520 = rt.sequence_length(l519);
                    at = 662; l521 = K[122];
                    at = 663; l522 = K[123];
                    at = 664; l523 = rt.int_lt(l521, l522);
                    at = 665; if (rt.truthy(l523)) {
                      at = 666; l525 = K[124];
                      at = 667; l524 = l525;
                    } else {
                      at = 668; l526 = K[125];
                      at = 669; l524 = l526;
                    }
                    at = 670; l527 = rt.int_gt(l524, l520);
                    at = 671; if (rt.truthy(l527)) {
                      at = 672; l528 = l520;
                    } else {
                      at = 673; l528 = l524;
                    }
                    at = 674; l529 = K[126];
                    at = 675; l530 = rt.sequence_slice(l519, l529, l528);
                    at = 676; l531 = rt.sequence_length(l515);
                    at = 677; l532 = K[127];
                    at = 678; l533 = rt.eq(l531, l532);
                    at = 679; if (rt.truthy(l533)) {
                      at = 680; l534 = l530;
                    } else {
                      at = 681; l535 = K[128];
                      at = 682; l536 = rt.sequence_get(l515, l535);
                      at = 683; l537 = rt.sequence_length(l530);
                      at = 684; l538 = rt.sequence_build(l537);
                      at = 685; {
                        const s685 = l530, c685 = Array.isArray(s685) ? s685.length : Number(rt.array_count(s685));
                        if (c685 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                        for (let i685 = 0; i685 < c685; i685++) {
                          l539 = BigInt(i685); l540 = Array.isArray(s685) ? s685[i685] : rt.array_nth(s685, i685);
                          at = 686; l541 = field(rt, l540, "color");
                          at = 687; l542 = field(rt, l540, "width");
                          at = 688; l543 = field(rt, l540, "points");
                          at = 689; l544 = field(rt, l536, "color");
                          at = 690; l545 = field(rt, l536, "width");
                          at = 691; l546 = field(rt, l536, "points");
                          at = 692; l547 = new Map([["color", l541], ["width", l542], ["points", l543]]);
                          at = 693; store(rt, l538, l539, l547);
                        }
                      }
                      at = 694; l534 = l538;
                    }
                    at = 695; l548 = l534;
                    at = 697; l550 = K[129];
                    at = 698; l549 = l550;
                    at = 700; l551 = l516;
                    at = 702; l552 = l517;
                    at = 704; l554 = K[130];
                    at = 705; l553 = l554;
                    at = 707; l555 = l30;
                  } else {
                    at = 696; l548 = l515;
                    at = 699; l549 = l24;
                    at = 701; l551 = l516;
                    at = 703; l552 = l517;
                    at = 706; l553 = l28;
                    at = 708; l555 = l30;
                  }
                  at = 884; l677 = l548;
                  at = 886; l678 = l549;
                  at = 888; l679 = l551;
                  at = 890; l680 = l552;
                  at = 892; l681 = l553;
                  at = 894; l682 = l555;
                } else {
                  at = 709; l556 = field(rt, l38, "kind");
                  at = 710; l557 = K[131];
                  at = 711; l558 = rt.equal(l556, l557);
                  at = 712; if (rt.truthy(l558)) {
                    at = 713; l559 = field(rt, l38, "key");
                    at = 714; l560 = K[132];
                    at = 715; l561 = rt.equal(l559, l560);
                    at = 716; l562 = field(rt, l38, "key");
                    at = 717; l563 = K[133];
                    at = 718; l564 = rt.equal(l562, l563);
                    at = 719; l565 = rt.or(l561, l564);
                    at = 720; if (rt.truthy(l565)) {
                      at = 721; l566 = rt.sequence_length(l23);
                      at = 722; l567 = K[134];
                      at = 723; l568 = rt.eq(l566, l567);
                      at = 724; if (rt.truthy(l568)) {
                        at = 725; l570 = K[135];
                        at = 726; l569 = l570;
                      } else {
                        at = 727; l571 = K[136];
                        at = 728; l572 = rt.sequence_get(l23, l571);
                        at = 729; l573 = K[137];
                        at = 730; l574 = rt.sequence_build(l573);
                        at = 731; l575 = K[138];
                        at = 732; {
                          const s732 = l575, c732 = Array.isArray(s732) ? s732.length : Number(rt.array_count(s732));
                          if (c732 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                          for (let i732 = 0; i732 < c732; i732++) {
                            l576 = BigInt(i732); l577 = Array.isArray(s732) ? s732[i732] : rt.array_nth(s732, i732);
                            at = 733; store(rt, l574, l576, l577);
                          }
                        }
                        at = 734; l569 = l574;
                      }
                      at = 858; l663 = l569;
                      at = 860; l665 = K[159];
                      at = 861; l664 = l665;
                      at = 863; l666 = l26;
                      at = 865; l667 = l27;
                      at = 867; l669 = K[160];
                      at = 868; l668 = l669;
                      at = 870; l670 = l30;
                    } else {
                      at = 735; l578 = field(rt, l38, "key");
                      at = 736; l579 = K[139];
                      at = 737; l580 = rt.equal(l578, l579);
                      at = 738; l581 = field(rt, l38, "key");
                      at = 739; l582 = K[140];
                      at = 740; l583 = rt.equal(l581, l582);
                      at = 741; l584 = rt.or(l580, l583);
                      at = 742; if (rt.truthy(l584)) {
                        at = 743; l585 = rt.sequence_length(l23);
                        at = 744; l586 = K[141];
                        at = 745; l587 = K[142];
                        at = 746; l588 = rt.int_lt(l586, l587);
                        at = 747; if (rt.truthy(l588)) {
                          at = 748; l590 = K[143];
                          at = 749; l589 = l590;
                        } else {
                          at = 750; l591 = K[144];
                          at = 751; l589 = l591;
                        }
                        at = 752; l592 = rt.int_gt(l589, l585);
                        at = 753; if (rt.truthy(l592)) {
                          at = 754; l593 = l585;
                        } else {
                          at = 755; l593 = l589;
                        }
                        at = 756; l594 = rt.int_sub(l585, l593);
                        at = 757; l595 = rt.sequence_slice(l23, l593, l594);
                        at = 758; l596 = rt.sequence_length(l23);
                        at = 759; l597 = K[145];
                        at = 760; l598 = rt.eq(l596, l597);
                        at = 761; if (rt.truthy(l598)) {
                          at = 762; l599 = l595;
                        } else {
                          at = 763; l600 = K[146];
                          at = 764; l601 = rt.sequence_get(l23, l600);
                          at = 765; l602 = rt.sequence_length(l595);
                          at = 766; l603 = rt.sequence_build(l602);
                          at = 767; {
                            const s767 = l595, c767 = Array.isArray(s767) ? s767.length : Number(rt.array_count(s767));
                            if (c767 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                            for (let i767 = 0; i767 < c767; i767++) {
                              l604 = BigInt(i767); l605 = Array.isArray(s767) ? s767[i767] : rt.array_nth(s767, i767);
                              at = 768; store(rt, l603, l604, l605);
                            }
                          }
                          at = 769; l599 = l603;
                        }
                        at = 844; l655 = l599;
                        at = 846; l657 = K[157];
                        at = 847; l656 = l657;
                        at = 849; l658 = l26;
                        at = 851; l659 = l27;
                        at = 853; l661 = K[158];
                        at = 854; l660 = l661;
                        at = 856; l662 = l30;
                      } else {
                        at = 770; l606 = field(rt, l38, "key");
                        at = 771; l607 = K[147];
                        at = 772; l608 = rt.equal(l606, l607);
                        at = 773; if (rt.truthy(l608)) {
                          at = 774; l609 = K[148];
                          at = 775; l610 = rt.sub(l27, l609);
                          at = 776; l611 = K[149];
                          at = 777; l612 = rt.max(l611, l610);
                          at = 778; l613 = rt.coerce("float", l27);
                          at = 779; l614 = rt.equal(l27, l613);
                          at = 780; if (rt.truthy(l614)) {
                            at = 781; l616 = rt.coerce("float", l612);
                            at = 782; l615 = l616;
                          } else {
                            at = 783; l615 = l612;
                          }
                          at = 832; l649 = l23;
                          at = 834; l650 = l24;
                          at = 836; l651 = l26;
                          at = 838; l652 = l615;
                          at = 840; l653 = l28;
                          at = 842; l654 = l30;
                        } else {
                          at = 784; l617 = field(rt, l38, "key");
                          at = 785; l618 = K[150];
                          at = 786; l619 = rt.equal(l617, l618);
                          at = 787; if (rt.truthy(l619)) {
                            at = 788; l620 = K[151];
                            at = 789; l621 = rt.add(l27, l620);
                            at = 790; l622 = K[152];
                            at = 791; l623 = rt.min(l622, l621);
                            at = 792; l624 = rt.coerce("float", l27);
                            at = 793; l625 = rt.equal(l27, l624);
                            at = 794; if (rt.truthy(l625)) {
                              at = 795; l627 = rt.coerce("float", l623);
                              at = 796; l626 = l627;
                            } else {
                              at = 797; l626 = l623;
                            }
                            at = 820; l643 = l23;
                            at = 822; l644 = l24;
                            at = 824; l645 = l26;
                            at = 826; l646 = l626;
                            at = 828; l647 = l28;
                            at = 830; l648 = l30;
                          } else {
                            at = 798; l628 = field(rt, l38, "key");
                            at = 799; l629 = K[153];
                            at = 800; l630 = rt.equal(l628, l629);
                            at = 801; l631 = field(rt, l38, "key");
                            at = 802; l632 = K[154];
                            at = 803; l633 = rt.equal(l631, l632);
                            at = 804; l634 = rt.or(l630, l633);
                            at = 805; if (rt.truthy(l634)) {
                              at = 806; l635 = l23;
                              at = 808; l636 = l24;
                              at = 810; l637 = l26;
                              at = 812; l638 = l27;
                              at = 814; l640 = K[155];
                              at = 815; l639 = l640;
                              at = 817; l642 = K[156];
                              at = 818; l641 = l642;
                            } else {
                              at = 807; l635 = l23;
                              at = 809; l636 = l24;
                              at = 811; l637 = l26;
                              at = 813; l638 = l27;
                              at = 816; l639 = l28;
                              at = 819; l641 = l30;
                            }
                            at = 821; l643 = l635;
                            at = 823; l644 = l636;
                            at = 825; l645 = l637;
                            at = 827; l646 = l638;
                            at = 829; l647 = l639;
                            at = 831; l648 = l641;
                          }
                          at = 833; l649 = l643;
                          at = 835; l650 = l644;
                          at = 837; l651 = l645;
                          at = 839; l652 = l646;
                          at = 841; l653 = l647;
                          at = 843; l654 = l648;
                        }
                        at = 845; l655 = l649;
                        at = 848; l656 = l650;
                        at = 850; l658 = l651;
                        at = 852; l659 = l652;
                        at = 855; l660 = l653;
                        at = 857; l662 = l654;
                      }
                      at = 859; l663 = l655;
                      at = 862; l664 = l656;
                      at = 864; l666 = l658;
                      at = 866; l667 = l659;
                      at = 869; l668 = l660;
                      at = 871; l670 = l662;
                    }
                    at = 872; l671 = l663;
                    at = 874; l672 = l664;
                    at = 876; l673 = l666;
                    at = 878; l674 = l667;
                    at = 880; l675 = l668;
                    at = 882; l676 = l670;
                  } else {
                    at = 873; l671 = l23;
                    at = 875; l672 = l24;
                    at = 877; l673 = l26;
                    at = 879; l674 = l27;
                    at = 881; l675 = l28;
                    at = 883; l676 = l30;
                  }
                  at = 885; l677 = l671;
                  at = 887; l678 = l672;
                  at = 889; l679 = l673;
                  at = 891; l680 = l674;
                  at = 893; l681 = l675;
                  at = 895; l682 = l676;
                }
                at = 897; l683 = l677;
                at = 899; l684 = l678;
                at = 901; l685 = l679;
                at = 903; l686 = l680;
                at = 905; l687 = l681;
                at = 907; l688 = l682;
              }
              at = 909; l689 = l683;
              at = 911; l690 = l684;
              at = 913; l691 = l685;
              at = 915; l692 = l686;
              at = 917; l693 = l687;
              at = 919; l694 = l688;
            }
            at = 921; l695 = l689;
            at = 923; l696 = l690;
            at = 925; l697 = l691;
            at = 927; l698 = l692;
            at = 929; l699 = l693;
            at = 931; l700 = l694;
          }
          at = 932; l701 = rt.sequence_length(l23);
          at = 933; l702 = K[161];
          at = 934; l703 = rt.eq(l701, l702);
          at = 935; if (rt.truthy(l703)) {
            at = 936; l704 = l695;
          } else {
            at = 937; l705 = K[162];
            at = 938; l706 = rt.sequence_get(l23, l705);
            at = 939; l707 = rt.sequence_length(l695);
            at = 940; l708 = rt.sequence_build(l707);
            at = 941; {
              const s941 = l695, c941 = Array.isArray(s941) ? s941.length : Number(rt.array_count(s941));
              if (c941 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              for (let i941 = 0; i941 < c941; i941++) {
                l709 = BigInt(i941); l710 = Array.isArray(s941) ? s941[i941] : rt.array_nth(s941, i941);
                at = 942; store(rt, l708, l709, l710);
              }
            }
            at = 943; l704 = l708;
          }
          at = 944; if (rt.truthy(l58)) {
            at = 945; if (rt.truthy(l61)) {
              at = 946; if (rt.truthy(l76)) {
                at = 947; l711 = field(rt, l25, "color");
                at = 948; l712 = field(rt, l25, "width");
                at = 949; l713 = field(rt, l25, "points");
                at = 950; l714 = field(rt, l25, "color");
                at = 951; l715 = field(rt, l25, "width");
                at = 952; l716 = field(rt, l25, "points");
                at = 1063; l795 = l711;
                at = 1065; l796 = l712;
                at = 1067; l797 = l713;
              } else {
                at = 953; if (rt.truthy(l103)) {
                  at = 954; l717 = field(rt, l25, "color");
                  at = 955; l718 = field(rt, l25, "width");
                  at = 956; l719 = field(rt, l25, "points");
                  at = 957; l720 = field(rt, l25, "color");
                  at = 958; l721 = field(rt, l25, "width");
                  at = 959; l722 = field(rt, l25, "points");
                  at = 1057; l792 = l717;
                  at = 1059; l793 = l718;
                  at = 1061; l794 = l719;
                } else {
                  at = 960; if (rt.truthy(l139)) {
                    at = 961; l723 = field(rt, l25, "color");
                    at = 962; l724 = field(rt, l25, "width");
                    at = 963; l725 = field(rt, l25, "points");
                    at = 964; l726 = field(rt, l25, "color");
                    at = 965; l727 = field(rt, l25, "width");
                    at = 966; l728 = field(rt, l25, "points");
                    at = 1051; l789 = l723;
                    at = 1053; l790 = l724;
                    at = 1055; l791 = l725;
                  } else {
                    at = 967; if (rt.truthy(l154)) {
                      at = 968; l729 = field(rt, l25, "color");
                      at = 969; l730 = field(rt, l25, "width");
                      at = 970; l731 = field(rt, l25, "points");
                      at = 971; l732 = field(rt, l25, "color");
                      at = 972; l733 = field(rt, l25, "width");
                      at = 973; l734 = field(rt, l25, "points");
                      at = 1045; l786 = l729;
                      at = 1047; l787 = l730;
                      at = 1049; l788 = l731;
                    } else {
                      at = 974; if (rt.truthy(l174)) {
                        at = 975; l735 = field(rt, l25, "color");
                        at = 976; l736 = field(rt, l25, "width");
                        at = 977; l737 = field(rt, l25, "points");
                        at = 978; l738 = field(rt, l25, "color");
                        at = 979; l739 = field(rt, l25, "width");
                        at = 980; l740 = field(rt, l25, "points");
                        at = 1039; l783 = l735;
                        at = 1041; l784 = l736;
                        at = 1043; l785 = l737;
                      } else {
                        at = 981; if (rt.truthy(l194)) {
                          at = 982; l741 = field(rt, l25, "color");
                          at = 983; l742 = field(rt, l25, "width");
                          at = 984; l743 = field(rt, l25, "points");
                          at = 985; l744 = field(rt, l25, "color");
                          at = 986; l745 = field(rt, l25, "width");
                          at = 987; l746 = field(rt, l25, "points");
                          at = 1033; l780 = l741;
                          at = 1035; l781 = l742;
                          at = 1037; l782 = l743;
                        } else {
                          at = 988; if (rt.truthy(l214)) {
                            at = 989; l747 = field(rt, l25, "color");
                            at = 990; l748 = field(rt, l25, "width");
                            at = 991; l749 = field(rt, l25, "points");
                            at = 992; l750 = field(rt, l25, "color");
                            at = 993; l751 = field(rt, l25, "width");
                            at = 994; l752 = field(rt, l25, "points");
                            at = 1027; l777 = l747;
                            at = 1029; l778 = l748;
                            at = 1031; l779 = l749;
                          } else {
                            at = 995; if (rt.truthy(l234)) {
                              at = 996; l753 = field(rt, l25, "color");
                              at = 997; l754 = field(rt, l25, "width");
                              at = 998; l755 = field(rt, l25, "points");
                              at = 999; l756 = field(rt, l25, "color");
                              at = 1000; l757 = field(rt, l25, "width");
                              at = 1001; l758 = field(rt, l25, "points");
                              at = 1021; l774 = l753;
                              at = 1023; l775 = l754;
                              at = 1025; l776 = l755;
                            } else {
                              at = 1002; if (rt.truthy(l257)) {
                                at = 1003; l759 = field(rt, l25, "color");
                                at = 1004; l760 = field(rt, l25, "width");
                                at = 1005; l761 = field(rt, l25, "points");
                                at = 1006; l762 = field(rt, l25, "color");
                                at = 1007; l763 = field(rt, l25, "width");
                                at = 1008; l764 = field(rt, l25, "points");
                                at = 1015; l771 = l759;
                                at = 1017; l772 = l760;
                                at = 1019; l773 = l761;
                              } else {
                                at = 1009; l765 = field(rt, l25, "color");
                                at = 1010; l766 = field(rt, l25, "width");
                                at = 1011; l767 = field(rt, l25, "points");
                                at = 1012; l768 = field(rt, l25, "color");
                                at = 1013; l769 = field(rt, l25, "width");
                                at = 1014; l770 = field(rt, l25, "points");
                                at = 1016; l771 = l765;
                                at = 1018; l772 = l766;
                                at = 1020; l773 = l767;
                              }
                              at = 1022; l774 = l771;
                              at = 1024; l775 = l772;
                              at = 1026; l776 = l773;
                            }
                            at = 1028; l777 = l774;
                            at = 1030; l778 = l775;
                            at = 1032; l779 = l776;
                          }
                          at = 1034; l780 = l777;
                          at = 1036; l781 = l778;
                          at = 1038; l782 = l779;
                        }
                        at = 1040; l783 = l780;
                        at = 1042; l784 = l781;
                        at = 1044; l785 = l782;
                      }
                      at = 1046; l786 = l783;
                      at = 1048; l787 = l784;
                      at = 1050; l788 = l785;
                    }
                    at = 1052; l789 = l786;
                    at = 1054; l790 = l787;
                    at = 1056; l791 = l788;
                  }
                  at = 1058; l792 = l789;
                  at = 1060; l793 = l790;
                  at = 1062; l794 = l791;
                }
                at = 1064; l795 = l792;
                at = 1066; l796 = l793;
                at = 1068; l797 = l794;
              }
              at = 1072; l801 = l795;
              at = 1074; l802 = l796;
              at = 1076; l803 = l797;
            } else {
              at = 1069; l798 = field(rt, l25, "color");
              at = 1070; l799 = field(rt, l25, "width");
              at = 1071; l800 = field(rt, l25, "points");
              at = 1073; l801 = l337;
              at = 1075; l802 = l339;
              at = 1077; l803 = l335;
            }
            at = 1237; l915 = l801;
            at = 1239; l916 = l802;
            at = 1241; l917 = l803;
          } else {
            at = 1078; if (rt.truthy(l351)) {
              at = 1079; if (rt.truthy(l24)) {
                at = 1080; l804 = field(rt, l25, "color");
                at = 1081; l805 = field(rt, l25, "width");
                at = 1082; l806 = field(rt, l25, "points");
                at = 1089; l813 = l409;
                at = 1091; l814 = l410;
                at = 1093; l815 = l411;
              } else {
                at = 1083; l807 = field(rt, l25, "color");
                at = 1084; l808 = field(rt, l25, "width");
                at = 1085; l809 = field(rt, l25, "points");
                at = 1086; l810 = field(rt, l25, "color");
                at = 1087; l811 = field(rt, l25, "width");
                at = 1088; l812 = field(rt, l25, "points");
                at = 1090; l813 = l807;
                at = 1092; l814 = l808;
                at = 1094; l815 = l809;
              }
              at = 1231; l912 = l813;
              at = 1233; l913 = l814;
              at = 1235; l914 = l815;
            } else {
              at = 1095; if (rt.truthy(l424)) {
                at = 1096; if (rt.truthy(l24)) {
                  at = 1097; l816 = field(rt, l25, "color");
                  at = 1098; l817 = field(rt, l25, "width");
                  at = 1099; l818 = field(rt, l25, "points");
                  at = 1100; l819 = field(rt, l25, "color");
                  at = 1101; l820 = field(rt, l25, "width");
                  at = 1102; l821 = field(rt, l25, "points");
                  at = 1109; l828 = l816;
                  at = 1111; l829 = l817;
                  at = 1113; l830 = l818;
                } else {
                  at = 1103; l822 = field(rt, l25, "color");
                  at = 1104; l823 = field(rt, l25, "width");
                  at = 1105; l824 = field(rt, l25, "points");
                  at = 1106; l825 = field(rt, l25, "color");
                  at = 1107; l826 = field(rt, l25, "width");
                  at = 1108; l827 = field(rt, l25, "points");
                  at = 1110; l828 = l822;
                  at = 1112; l829 = l823;
                  at = 1114; l830 = l824;
                }
                at = 1225; l909 = l828;
                at = 1227; l910 = l829;
                at = 1229; l911 = l830;
              } else {
                at = 1115; if (rt.truthy(l514)) {
                  at = 1116; if (rt.truthy(l24)) {
                    at = 1117; l831 = field(rt, l25, "color");
                    at = 1118; l832 = field(rt, l25, "width");
                    at = 1119; l833 = field(rt, l25, "points");
                    at = 1120; l834 = field(rt, l25, "color");
                    at = 1121; l835 = field(rt, l25, "width");
                    at = 1122; l836 = field(rt, l25, "points");
                    at = 1129; l843 = l831;
                    at = 1131; l844 = l832;
                    at = 1133; l845 = l833;
                  } else {
                    at = 1123; l837 = field(rt, l25, "color");
                    at = 1124; l838 = field(rt, l25, "width");
                    at = 1125; l839 = field(rt, l25, "points");
                    at = 1126; l840 = field(rt, l25, "color");
                    at = 1127; l841 = field(rt, l25, "width");
                    at = 1128; l842 = field(rt, l25, "points");
                    at = 1130; l843 = l837;
                    at = 1132; l844 = l838;
                    at = 1134; l845 = l839;
                  }
                  at = 1219; l906 = l843;
                  at = 1221; l907 = l844;
                  at = 1223; l908 = l845;
                } else {
                  at = 1135; if (rt.truthy(l558)) {
                    at = 1136; if (rt.truthy(l565)) {
                      at = 1137; l846 = field(rt, l25, "color");
                      at = 1138; l847 = field(rt, l25, "width");
                      at = 1139; l848 = field(rt, l25, "points");
                      at = 1140; l849 = field(rt, l25, "color");
                      at = 1141; l850 = field(rt, l25, "width");
                      at = 1142; l851 = field(rt, l25, "points");
                      at = 1201; l894 = l846;
                      at = 1203; l895 = l847;
                      at = 1205; l896 = l848;
                    } else {
                      at = 1143; if (rt.truthy(l584)) {
                        at = 1144; l852 = field(rt, l25, "color");
                        at = 1145; l853 = field(rt, l25, "width");
                        at = 1146; l854 = field(rt, l25, "points");
                        at = 1147; l855 = field(rt, l25, "color");
                        at = 1148; l856 = field(rt, l25, "width");
                        at = 1149; l857 = field(rt, l25, "points");
                        at = 1195; l891 = l852;
                        at = 1197; l892 = l853;
                        at = 1199; l893 = l854;
                      } else {
                        at = 1150; if (rt.truthy(l608)) {
                          at = 1151; l858 = field(rt, l25, "color");
                          at = 1152; l859 = field(rt, l25, "width");
                          at = 1153; l860 = field(rt, l25, "points");
                          at = 1154; l861 = field(rt, l25, "color");
                          at = 1155; l862 = field(rt, l25, "width");
                          at = 1156; l863 = field(rt, l25, "points");
                          at = 1189; l888 = l858;
                          at = 1191; l889 = l859;
                          at = 1193; l890 = l860;
                        } else {
                          at = 1157; if (rt.truthy(l619)) {
                            at = 1158; l864 = field(rt, l25, "color");
                            at = 1159; l865 = field(rt, l25, "width");
                            at = 1160; l866 = field(rt, l25, "points");
                            at = 1161; l867 = field(rt, l25, "color");
                            at = 1162; l868 = field(rt, l25, "width");
                            at = 1163; l869 = field(rt, l25, "points");
                            at = 1183; l885 = l864;
                            at = 1185; l886 = l865;
                            at = 1187; l887 = l866;
                          } else {
                            at = 1164; if (rt.truthy(l634)) {
                              at = 1165; l870 = field(rt, l25, "color");
                              at = 1166; l871 = field(rt, l25, "width");
                              at = 1167; l872 = field(rt, l25, "points");
                              at = 1168; l873 = field(rt, l25, "color");
                              at = 1169; l874 = field(rt, l25, "width");
                              at = 1170; l875 = field(rt, l25, "points");
                              at = 1177; l882 = l870;
                              at = 1179; l883 = l871;
                              at = 1181; l884 = l872;
                            } else {
                              at = 1171; l876 = field(rt, l25, "color");
                              at = 1172; l877 = field(rt, l25, "width");
                              at = 1173; l878 = field(rt, l25, "points");
                              at = 1174; l879 = field(rt, l25, "color");
                              at = 1175; l880 = field(rt, l25, "width");
                              at = 1176; l881 = field(rt, l25, "points");
                              at = 1178; l882 = l876;
                              at = 1180; l883 = l877;
                              at = 1182; l884 = l878;
                            }
                            at = 1184; l885 = l882;
                            at = 1186; l886 = l883;
                            at = 1188; l887 = l884;
                          }
                          at = 1190; l888 = l885;
                          at = 1192; l889 = l886;
                          at = 1194; l890 = l887;
                        }
                        at = 1196; l891 = l888;
                        at = 1198; l892 = l889;
                        at = 1200; l893 = l890;
                      }
                      at = 1202; l894 = l891;
                      at = 1204; l895 = l892;
                      at = 1206; l896 = l893;
                    }
                    at = 1213; l903 = l894;
                    at = 1215; l904 = l895;
                    at = 1217; l905 = l896;
                  } else {
                    at = 1207; l897 = field(rt, l25, "color");
                    at = 1208; l898 = field(rt, l25, "width");
                    at = 1209; l899 = field(rt, l25, "points");
                    at = 1210; l900 = field(rt, l25, "color");
                    at = 1211; l901 = field(rt, l25, "width");
                    at = 1212; l902 = field(rt, l25, "points");
                    at = 1214; l903 = l897;
                    at = 1216; l904 = l898;
                    at = 1218; l905 = l899;
                  }
                  at = 1220; l906 = l903;
                  at = 1222; l907 = l904;
                  at = 1224; l908 = l905;
                }
                at = 1226; l909 = l906;
                at = 1228; l910 = l907;
                at = 1230; l911 = l908;
              }
              at = 1232; l912 = l909;
              at = 1234; l913 = l910;
              at = 1236; l914 = l911;
            }
            at = 1238; l915 = l912;
            at = 1240; l916 = l913;
            at = 1242; l917 = l914;
          }
          at = 1243; l918 = rt.coerce("float", l26);
          at = 1244; l919 = rt.equal(l26, l918);
          at = 1245; if (rt.truthy(l919)) {
            at = 1246; l921 = rt.coerce("float", l697);
            at = 1247; l920 = l921;
          } else {
            at = 1248; l922 = rt.coerce("int", l697);
            at = 1249; l920 = l922;
          }
          at = 1250; l923 = rt.coerce("float", l27);
          at = 1251; l924 = rt.equal(l27, l923);
          at = 1252; if (rt.truthy(l924)) {
            at = 1253; l926 = rt.coerce("float", l698);
            at = 1254; l925 = l926;
          } else {
            at = 1255; l927 = rt.coerce("int", l698);
            at = 1256; l925 = l927;
          }
          at = 1257; l928 = l704;
          at = 1258; l929 = l696;
          at = 1259; l930 = l915;
          at = 1260; l931 = l916;
          at = 1261; l932 = l917;
          at = 1262; l933 = l920;
          at = 1263; l934 = l925;
          at = 1264; l935 = l699;
          at = 1265; l936 = l700;
          at = 1266; l23 = l928;
          at = 1267; l24 = l929;
          at = 1268; l937 = new Map([["color", l930], ["width", l931], ["points", l932]]);
          at = 1269; l25 = l937;
          at = 1270; l26 = l933;
          at = 1271; l27 = l934;
          at = 1272; l28 = l935;
          at = 1273; l30 = l936;
          at = 1274; l938 = K[163];
          at = 1275; l939 = rt.add(l34, l938);
          at = 1276; l34 = l939;
          at = 1277; l940 = K[164];
          at = 1278; l941 = rt.add(l32, l940);
          at = 1279; l32 = l941;
        }
      }
      at = 1280; l942 = rt.coerce("int", l26);
      at = 1281; l943 = rt.coerce("int", l27);
      at = 1282; l944 = new Map([["strokes", l23], ["active", l24], ["current", l25], ["color", l942], ["width", l943], ["status", l28], ["save", l30]]);
      at = 1283; cell(state, 0, "", input.tick).pending = l944;
      at = 1284; l15 = l944;
    }
    at = 1285; l945 = field(rt, l15, "strokes");
    at = 1286; l946 = rt.sequence_length(l945);
    at = 1287; l947 = K[165];
    at = 1288; l948 = rt.lt(l947, l946);
    at = 1289; if (rt.truthy(l948)) {
      at = 1290; l949 = field(rt, l15, "strokes");
      at = 1291; l950 = K[166];
      at = 1292; l951 = rt.sequence_get(l949, l950);
    }
    at = 1293; if (rt.truthy(l948)) {
      at = 1294; l953 = field(rt, l951, "points");
      at = 1295; l952 = l953;
    } else {
      at = 1296; l954 = K[167];
      at = 1297; l952 = l954;
    }
    at = 1298; l955 = rt.array_count(l952);
    at = 1299; l956 = K[168];
    at = 1300; l957 = rt.gt(l955, l956);
    at = 1301; if (rt.truthy(l957)) {
      at = 1302; if (rt.truthy(l948)) {
        at = 1303; l960 = field(rt, l951, "points");
        at = 1304; l959 = l960;
      } else {
        at = 1305; l961 = K[169];
        at = 1306; l959 = l961;
      }
      at = 1307; l962 = K[170];
      at = 1308; l963 = rt.array_nth(l959, l962);
      at = 1309; l958 = l963;
    } else {
      at = 1310; l964 = K[171];
      at = 1311; l958 = l964;
    }
    at = 1312; if (rt.truthy(l948)) {
      at = 1313; l966 = field(rt, l951, "width");
      at = 1314; l965 = l966;
    } else {
      at = 1315; l967 = K[172];
      at = 1316; l965 = l967;
    }
    at = 1317; l968 = K[173];
    at = 1318; l969 = rt.int_div(l965, l968);
    at = 1319; l970 = K[174];
    at = 1320; l971 = rt.max(l970, l969);
    at = 1321; l972 = K[175];
    at = 1322; l973 = rt.gt(l955, l972);
    at = 1323; if (rt.truthy(l973)) {
      at = 1324; if (rt.truthy(l948)) {
        at = 1325; l976 = field(rt, l951, "color");
        at = 1326; l975 = l976;
      } else {
        at = 1327; l977 = K[176];
        at = 1328; l975 = l977;
      }
      at = 1329; l978 = K[177];
      at = 1330; l979 = rt.sequence_get(l978, l975);
      at = 1331; l974 = l979;
    } else {
      at = 1332; l980 = K[178];
      at = 1333; l974 = l980;
    }
    at = 1334; markSet(rt, false, "circle", {at: l958, radius: l971, fill: l974});
    at = 1335; if (rt.truthy(l948)) {
      at = 1336; l982 = field(rt, l951, "points");
      at = 1337; l981 = l982;
    } else {
      at = 1338; l983 = K[179];
      at = 1339; l981 = l983;
    }
    at = 1340; l984 = K[180];
    at = 1341; l985 = rt.sub(l955, l984);
    at = 1342; l986 = K[181];
    at = 1343; l987 = rt.max(l986, l985);
    at = 1344; l988 = K[182];
    at = 1345; l989 = rt.array_slice(l981, l988, l987);
    at = 1346; if (rt.truthy(l948)) {
      at = 1347; l991 = field(rt, l951, "points");
      at = 1348; l990 = l991;
    } else {
      at = 1349; l992 = K[183];
      at = 1350; l990 = l992;
    }
    at = 1351; l993 = K[184];
    at = 1352; l994 = rt.gt(l955, l993);
    at = 1353; if (rt.truthy(l994)) {
      at = 1354; l996 = K[185];
      at = 1355; l995 = l996;
    } else {
      at = 1356; l997 = K[186];
      at = 1357; l995 = l997;
    }
    at = 1358; l998 = K[187];
    at = 1359; l999 = rt.sub(l955, l998);
    at = 1360; l1000 = K[188];
    at = 1361; l1001 = rt.max(l1000, l999);
    at = 1362; l1002 = rt.array_slice(l990, l995, l1001);
    at = 1363; if (rt.truthy(l948)) {
      at = 1364; l1004 = field(rt, l951, "color");
      at = 1365; l1003 = l1004;
    } else {
      at = 1366; l1005 = K[189];
      at = 1367; l1003 = l1005;
    }
    at = 1368; l1006 = K[190];
    at = 1369; l1007 = rt.sequence_get(l1006, l1003);
    at = 1370; if (rt.truthy(l948)) {
      at = 1371; l1009 = field(rt, l951, "width");
      at = 1372; l1008 = l1009;
    } else {
      at = 1373; l1010 = K[191];
      at = 1374; l1008 = l1010;
    }
    at = 1375; markSet(rt, true, "line", {from: l989, to: l1002, color: l1007, width: l1008});
    at = 1376; l1011 = field(rt, l15, "strokes");
    at = 1377; l1012 = rt.sequence_length(l1011);
    at = 1378; l1013 = K[192];
    at = 1379; l1014 = rt.lt(l1013, l1012);
    at = 1380; if (rt.truthy(l1014)) {
      at = 1381; l1015 = field(rt, l15, "strokes");
      at = 1382; l1016 = K[193];
      at = 1383; l1017 = rt.sequence_get(l1015, l1016);
    }
    at = 1384; if (rt.truthy(l1014)) {
      at = 1385; l1019 = field(rt, l1017, "points");
      at = 1386; l1018 = l1019;
    } else {
      at = 1387; l1020 = K[194];
      at = 1388; l1018 = l1020;
    }
    at = 1389; l1021 = rt.array_count(l1018);
    at = 1390; l1022 = K[195];
    at = 1391; l1023 = rt.gt(l1021, l1022);
    at = 1392; if (rt.truthy(l1023)) {
      at = 1393; if (rt.truthy(l1014)) {
        at = 1394; l1026 = field(rt, l1017, "points");
        at = 1395; l1025 = l1026;
      } else {
        at = 1396; l1027 = K[196];
        at = 1397; l1025 = l1027;
      }
      at = 1398; l1028 = K[197];
      at = 1399; l1029 = rt.array_nth(l1025, l1028);
      at = 1400; l1024 = l1029;
    } else {
      at = 1401; l1030 = K[198];
      at = 1402; l1024 = l1030;
    }
    at = 1403; if (rt.truthy(l1014)) {
      at = 1404; l1032 = field(rt, l1017, "width");
      at = 1405; l1031 = l1032;
    } else {
      at = 1406; l1033 = K[199];
      at = 1407; l1031 = l1033;
    }
    at = 1408; l1034 = K[200];
    at = 1409; l1035 = rt.int_div(l1031, l1034);
    at = 1410; l1036 = K[201];
    at = 1411; l1037 = rt.max(l1036, l1035);
    at = 1412; l1038 = K[202];
    at = 1413; l1039 = rt.gt(l1021, l1038);
    at = 1414; if (rt.truthy(l1039)) {
      at = 1415; if (rt.truthy(l1014)) {
        at = 1416; l1042 = field(rt, l1017, "color");
        at = 1417; l1041 = l1042;
      } else {
        at = 1418; l1043 = K[203];
        at = 1419; l1041 = l1043;
      }
      at = 1420; l1044 = K[204];
      at = 1421; l1045 = rt.sequence_get(l1044, l1041);
      at = 1422; l1040 = l1045;
    } else {
      at = 1423; l1046 = K[205];
      at = 1424; l1040 = l1046;
    }
    at = 1425; markSet(rt, false, "circle", {at: l1024, radius: l1037, fill: l1040});
    at = 1426; if (rt.truthy(l1014)) {
      at = 1427; l1048 = field(rt, l1017, "points");
      at = 1428; l1047 = l1048;
    } else {
      at = 1429; l1049 = K[206];
      at = 1430; l1047 = l1049;
    }
    at = 1431; l1050 = K[207];
    at = 1432; l1051 = rt.sub(l1021, l1050);
    at = 1433; l1052 = K[208];
    at = 1434; l1053 = rt.max(l1052, l1051);
    at = 1435; l1054 = K[209];
    at = 1436; l1055 = rt.array_slice(l1047, l1054, l1053);
    at = 1437; if (rt.truthy(l1014)) {
      at = 1438; l1057 = field(rt, l1017, "points");
      at = 1439; l1056 = l1057;
    } else {
      at = 1440; l1058 = K[210];
      at = 1441; l1056 = l1058;
    }
    at = 1442; l1059 = K[211];
    at = 1443; l1060 = rt.gt(l1021, l1059);
    at = 1444; if (rt.truthy(l1060)) {
      at = 1445; l1062 = K[212];
      at = 1446; l1061 = l1062;
    } else {
      at = 1447; l1063 = K[213];
      at = 1448; l1061 = l1063;
    }
    at = 1449; l1064 = K[214];
    at = 1450; l1065 = rt.sub(l1021, l1064);
    at = 1451; l1066 = K[215];
    at = 1452; l1067 = rt.max(l1066, l1065);
    at = 1453; l1068 = rt.array_slice(l1056, l1061, l1067);
    at = 1454; if (rt.truthy(l1014)) {
      at = 1455; l1070 = field(rt, l1017, "color");
      at = 1456; l1069 = l1070;
    } else {
      at = 1457; l1071 = K[216];
      at = 1458; l1069 = l1071;
    }
    at = 1459; l1072 = K[217];
    at = 1460; l1073 = rt.sequence_get(l1072, l1069);
    at = 1461; if (rt.truthy(l1014)) {
      at = 1462; l1075 = field(rt, l1017, "width");
      at = 1463; l1074 = l1075;
    } else {
      at = 1464; l1076 = K[218];
      at = 1465; l1074 = l1076;
    }
    at = 1466; markSet(rt, true, "line", {from: l1055, to: l1068, color: l1073, width: l1074});
    at = 1467; l1077 = field(rt, l15, "strokes");
    at = 1468; l1078 = rt.sequence_length(l1077);
    at = 1469; l1079 = K[219];
    at = 1470; l1080 = rt.lt(l1079, l1078);
    at = 1471; if (rt.truthy(l1080)) {
      at = 1472; l1081 = field(rt, l15, "strokes");
      at = 1473; l1082 = K[220];
      at = 1474; l1083 = rt.sequence_get(l1081, l1082);
    }
    at = 1475; if (rt.truthy(l1080)) {
      at = 1476; l1085 = field(rt, l1083, "points");
      at = 1477; l1084 = l1085;
    } else {
      at = 1478; l1086 = K[221];
      at = 1479; l1084 = l1086;
    }
    at = 1480; l1087 = rt.array_count(l1084);
    at = 1481; l1088 = K[222];
    at = 1482; l1089 = rt.gt(l1087, l1088);
    at = 1483; if (rt.truthy(l1089)) {
      at = 1484; if (rt.truthy(l1080)) {
        at = 1485; l1092 = field(rt, l1083, "points");
        at = 1486; l1091 = l1092;
      } else {
        at = 1487; l1093 = K[223];
        at = 1488; l1091 = l1093;
      }
      at = 1489; l1094 = K[224];
      at = 1490; l1095 = rt.array_nth(l1091, l1094);
      at = 1491; l1090 = l1095;
    } else {
      at = 1492; l1096 = K[225];
      at = 1493; l1090 = l1096;
    }
    at = 1494; if (rt.truthy(l1080)) {
      at = 1495; l1098 = field(rt, l1083, "width");
      at = 1496; l1097 = l1098;
    } else {
      at = 1497; l1099 = K[226];
      at = 1498; l1097 = l1099;
    }
    at = 1499; l1100 = K[227];
    at = 1500; l1101 = rt.int_div(l1097, l1100);
    at = 1501; l1102 = K[228];
    at = 1502; l1103 = rt.max(l1102, l1101);
    at = 1503; l1104 = K[229];
    at = 1504; l1105 = rt.gt(l1087, l1104);
    at = 1505; if (rt.truthy(l1105)) {
      at = 1506; if (rt.truthy(l1080)) {
        at = 1507; l1108 = field(rt, l1083, "color");
        at = 1508; l1107 = l1108;
      } else {
        at = 1509; l1109 = K[230];
        at = 1510; l1107 = l1109;
      }
      at = 1511; l1110 = K[231];
      at = 1512; l1111 = rt.sequence_get(l1110, l1107);
      at = 1513; l1106 = l1111;
    } else {
      at = 1514; l1112 = K[232];
      at = 1515; l1106 = l1112;
    }
    at = 1516; markSet(rt, false, "circle", {at: l1090, radius: l1103, fill: l1106});
    at = 1517; if (rt.truthy(l1080)) {
      at = 1518; l1114 = field(rt, l1083, "points");
      at = 1519; l1113 = l1114;
    } else {
      at = 1520; l1115 = K[233];
      at = 1521; l1113 = l1115;
    }
    at = 1522; l1116 = K[234];
    at = 1523; l1117 = rt.sub(l1087, l1116);
    at = 1524; l1118 = K[235];
    at = 1525; l1119 = rt.max(l1118, l1117);
    at = 1526; l1120 = K[236];
    at = 1527; l1121 = rt.array_slice(l1113, l1120, l1119);
    at = 1528; if (rt.truthy(l1080)) {
      at = 1529; l1123 = field(rt, l1083, "points");
      at = 1530; l1122 = l1123;
    } else {
      at = 1531; l1124 = K[237];
      at = 1532; l1122 = l1124;
    }
    at = 1533; l1125 = K[238];
    at = 1534; l1126 = rt.gt(l1087, l1125);
    at = 1535; if (rt.truthy(l1126)) {
      at = 1536; l1128 = K[239];
      at = 1537; l1127 = l1128;
    } else {
      at = 1538; l1129 = K[240];
      at = 1539; l1127 = l1129;
    }
    at = 1540; l1130 = K[241];
    at = 1541; l1131 = rt.sub(l1087, l1130);
    at = 1542; l1132 = K[242];
    at = 1543; l1133 = rt.max(l1132, l1131);
    at = 1544; l1134 = rt.array_slice(l1122, l1127, l1133);
    at = 1545; if (rt.truthy(l1080)) {
      at = 1546; l1136 = field(rt, l1083, "color");
      at = 1547; l1135 = l1136;
    } else {
      at = 1548; l1137 = K[243];
      at = 1549; l1135 = l1137;
    }
    at = 1550; l1138 = K[244];
    at = 1551; l1139 = rt.sequence_get(l1138, l1135);
    at = 1552; if (rt.truthy(l1080)) {
      at = 1553; l1141 = field(rt, l1083, "width");
      at = 1554; l1140 = l1141;
    } else {
      at = 1555; l1142 = K[245];
      at = 1556; l1140 = l1142;
    }
    at = 1557; markSet(rt, true, "line", {from: l1121, to: l1134, color: l1139, width: l1140});
    at = 1558; l1143 = field(rt, l15, "strokes");
    at = 1559; l1144 = rt.sequence_length(l1143);
    at = 1560; l1145 = K[246];
    at = 1561; l1146 = rt.lt(l1145, l1144);
    at = 1562; if (rt.truthy(l1146)) {
      at = 1563; l1147 = field(rt, l15, "strokes");
      at = 1564; l1148 = K[247];
      at = 1565; l1149 = rt.sequence_get(l1147, l1148);
    }
    at = 1566; if (rt.truthy(l1146)) {
      at = 1567; l1151 = field(rt, l1149, "points");
      at = 1568; l1150 = l1151;
    } else {
      at = 1569; l1152 = K[248];
      at = 1570; l1150 = l1152;
    }
    at = 1571; l1153 = rt.array_count(l1150);
    at = 1572; l1154 = K[249];
    at = 1573; l1155 = rt.gt(l1153, l1154);
    at = 1574; if (rt.truthy(l1155)) {
      at = 1575; if (rt.truthy(l1146)) {
        at = 1576; l1158 = field(rt, l1149, "points");
        at = 1577; l1157 = l1158;
      } else {
        at = 1578; l1159 = K[250];
        at = 1579; l1157 = l1159;
      }
      at = 1580; l1160 = K[251];
      at = 1581; l1161 = rt.array_nth(l1157, l1160);
      at = 1582; l1156 = l1161;
    } else {
      at = 1583; l1162 = K[252];
      at = 1584; l1156 = l1162;
    }
    at = 1585; if (rt.truthy(l1146)) {
      at = 1586; l1164 = field(rt, l1149, "width");
      at = 1587; l1163 = l1164;
    } else {
      at = 1588; l1165 = K[253];
      at = 1589; l1163 = l1165;
    }
    at = 1590; l1166 = K[254];
    at = 1591; l1167 = rt.int_div(l1163, l1166);
    at = 1592; l1168 = K[255];
    at = 1593; l1169 = rt.max(l1168, l1167);
    at = 1594; l1170 = K[256];
    at = 1595; l1171 = rt.gt(l1153, l1170);
    at = 1596; if (rt.truthy(l1171)) {
      at = 1597; if (rt.truthy(l1146)) {
        at = 1598; l1174 = field(rt, l1149, "color");
        at = 1599; l1173 = l1174;
      } else {
        at = 1600; l1175 = K[257];
        at = 1601; l1173 = l1175;
      }
      at = 1602; l1176 = K[258];
      at = 1603; l1177 = rt.sequence_get(l1176, l1173);
      at = 1604; l1172 = l1177;
    } else {
      at = 1605; l1178 = K[259];
      at = 1606; l1172 = l1178;
    }
    at = 1607; markSet(rt, false, "circle", {at: l1156, radius: l1169, fill: l1172});
    at = 1608; if (rt.truthy(l1146)) {
      at = 1609; l1180 = field(rt, l1149, "points");
      at = 1610; l1179 = l1180;
    } else {
      at = 1611; l1181 = K[260];
      at = 1612; l1179 = l1181;
    }
    at = 1613; l1182 = K[261];
    at = 1614; l1183 = rt.sub(l1153, l1182);
    at = 1615; l1184 = K[262];
    at = 1616; l1185 = rt.max(l1184, l1183);
    at = 1617; l1186 = K[263];
    at = 1618; l1187 = rt.array_slice(l1179, l1186, l1185);
    at = 1619; if (rt.truthy(l1146)) {
      at = 1620; l1189 = field(rt, l1149, "points");
      at = 1621; l1188 = l1189;
    } else {
      at = 1622; l1190 = K[264];
      at = 1623; l1188 = l1190;
    }
    at = 1624; l1191 = K[265];
    at = 1625; l1192 = rt.gt(l1153, l1191);
    at = 1626; if (rt.truthy(l1192)) {
      at = 1627; l1194 = K[266];
      at = 1628; l1193 = l1194;
    } else {
      at = 1629; l1195 = K[267];
      at = 1630; l1193 = l1195;
    }
    at = 1631; l1196 = K[268];
    at = 1632; l1197 = rt.sub(l1153, l1196);
    at = 1633; l1198 = K[269];
    at = 1634; l1199 = rt.max(l1198, l1197);
    at = 1635; l1200 = rt.array_slice(l1188, l1193, l1199);
    at = 1636; if (rt.truthy(l1146)) {
      at = 1637; l1202 = field(rt, l1149, "color");
      at = 1638; l1201 = l1202;
    } else {
      at = 1639; l1203 = K[270];
      at = 1640; l1201 = l1203;
    }
    at = 1641; l1204 = K[271];
    at = 1642; l1205 = rt.sequence_get(l1204, l1201);
    at = 1643; if (rt.truthy(l1146)) {
      at = 1644; l1207 = field(rt, l1149, "width");
      at = 1645; l1206 = l1207;
    } else {
      at = 1646; l1208 = K[272];
      at = 1647; l1206 = l1208;
    }
    at = 1648; markSet(rt, true, "line", {from: l1187, to: l1200, color: l1205, width: l1206});
    at = 1649; l1209 = field(rt, l15, "strokes");
    at = 1650; l1210 = rt.sequence_length(l1209);
    at = 1651; l1211 = K[273];
    at = 1652; l1212 = rt.lt(l1211, l1210);
    at = 1653; if (rt.truthy(l1212)) {
      at = 1654; l1213 = field(rt, l15, "strokes");
      at = 1655; l1214 = K[274];
      at = 1656; l1215 = rt.sequence_get(l1213, l1214);
    }
    at = 1657; if (rt.truthy(l1212)) {
      at = 1658; l1217 = field(rt, l1215, "points");
      at = 1659; l1216 = l1217;
    } else {
      at = 1660; l1218 = K[275];
      at = 1661; l1216 = l1218;
    }
    at = 1662; l1219 = rt.array_count(l1216);
    at = 1663; l1220 = K[276];
    at = 1664; l1221 = rt.gt(l1219, l1220);
    at = 1665; if (rt.truthy(l1221)) {
      at = 1666; if (rt.truthy(l1212)) {
        at = 1667; l1224 = field(rt, l1215, "points");
        at = 1668; l1223 = l1224;
      } else {
        at = 1669; l1225 = K[277];
        at = 1670; l1223 = l1225;
      }
      at = 1671; l1226 = K[278];
      at = 1672; l1227 = rt.array_nth(l1223, l1226);
      at = 1673; l1222 = l1227;
    } else {
      at = 1674; l1228 = K[279];
      at = 1675; l1222 = l1228;
    }
    at = 1676; if (rt.truthy(l1212)) {
      at = 1677; l1230 = field(rt, l1215, "width");
      at = 1678; l1229 = l1230;
    } else {
      at = 1679; l1231 = K[280];
      at = 1680; l1229 = l1231;
    }
    at = 1681; l1232 = K[281];
    at = 1682; l1233 = rt.int_div(l1229, l1232);
    at = 1683; l1234 = K[282];
    at = 1684; l1235 = rt.max(l1234, l1233);
    at = 1685; l1236 = K[283];
    at = 1686; l1237 = rt.gt(l1219, l1236);
    at = 1687; if (rt.truthy(l1237)) {
      at = 1688; if (rt.truthy(l1212)) {
        at = 1689; l1240 = field(rt, l1215, "color");
        at = 1690; l1239 = l1240;
      } else {
        at = 1691; l1241 = K[284];
        at = 1692; l1239 = l1241;
      }
      at = 1693; l1242 = K[285];
      at = 1694; l1243 = rt.sequence_get(l1242, l1239);
      at = 1695; l1238 = l1243;
    } else {
      at = 1696; l1244 = K[286];
      at = 1697; l1238 = l1244;
    }
    at = 1698; markSet(rt, false, "circle", {at: l1222, radius: l1235, fill: l1238});
    at = 1699; if (rt.truthy(l1212)) {
      at = 1700; l1246 = field(rt, l1215, "points");
      at = 1701; l1245 = l1246;
    } else {
      at = 1702; l1247 = K[287];
      at = 1703; l1245 = l1247;
    }
    at = 1704; l1248 = K[288];
    at = 1705; l1249 = rt.sub(l1219, l1248);
    at = 1706; l1250 = K[289];
    at = 1707; l1251 = rt.max(l1250, l1249);
    at = 1708; l1252 = K[290];
    at = 1709; l1253 = rt.array_slice(l1245, l1252, l1251);
    at = 1710; if (rt.truthy(l1212)) {
      at = 1711; l1255 = field(rt, l1215, "points");
      at = 1712; l1254 = l1255;
    } else {
      at = 1713; l1256 = K[291];
      at = 1714; l1254 = l1256;
    }
    at = 1715; l1257 = K[292];
    at = 1716; l1258 = rt.gt(l1219, l1257);
    at = 1717; if (rt.truthy(l1258)) {
      at = 1718; l1260 = K[293];
      at = 1719; l1259 = l1260;
    } else {
      at = 1720; l1261 = K[294];
      at = 1721; l1259 = l1261;
    }
    at = 1722; l1262 = K[295];
    at = 1723; l1263 = rt.sub(l1219, l1262);
    at = 1724; l1264 = K[296];
    at = 1725; l1265 = rt.max(l1264, l1263);
    at = 1726; l1266 = rt.array_slice(l1254, l1259, l1265);
    at = 1727; if (rt.truthy(l1212)) {
      at = 1728; l1268 = field(rt, l1215, "color");
      at = 1729; l1267 = l1268;
    } else {
      at = 1730; l1269 = K[297];
      at = 1731; l1267 = l1269;
    }
    at = 1732; l1270 = K[298];
    at = 1733; l1271 = rt.sequence_get(l1270, l1267);
    at = 1734; if (rt.truthy(l1212)) {
      at = 1735; l1273 = field(rt, l1215, "width");
      at = 1736; l1272 = l1273;
    } else {
      at = 1737; l1274 = K[299];
      at = 1738; l1272 = l1274;
    }
    at = 1739; markSet(rt, true, "line", {from: l1253, to: l1266, color: l1271, width: l1272});
    at = 1740; l1275 = field(rt, l15, "strokes");
    at = 1741; l1276 = rt.sequence_length(l1275);
    at = 1742; l1277 = K[300];
    at = 1743; l1278 = rt.lt(l1277, l1276);
    at = 1744; if (rt.truthy(l1278)) {
      at = 1745; l1279 = field(rt, l15, "strokes");
      at = 1746; l1280 = K[301];
      at = 1747; l1281 = rt.sequence_get(l1279, l1280);
    }
    at = 1748; if (rt.truthy(l1278)) {
      at = 1749; l1283 = field(rt, l1281, "points");
      at = 1750; l1282 = l1283;
    } else {
      at = 1751; l1284 = K[302];
      at = 1752; l1282 = l1284;
    }
    at = 1753; l1285 = rt.array_count(l1282);
    at = 1754; l1286 = K[303];
    at = 1755; l1287 = rt.gt(l1285, l1286);
    at = 1756; if (rt.truthy(l1287)) {
      at = 1757; if (rt.truthy(l1278)) {
        at = 1758; l1290 = field(rt, l1281, "points");
        at = 1759; l1289 = l1290;
      } else {
        at = 1760; l1291 = K[304];
        at = 1761; l1289 = l1291;
      }
      at = 1762; l1292 = K[305];
      at = 1763; l1293 = rt.array_nth(l1289, l1292);
      at = 1764; l1288 = l1293;
    } else {
      at = 1765; l1294 = K[306];
      at = 1766; l1288 = l1294;
    }
    at = 1767; if (rt.truthy(l1278)) {
      at = 1768; l1296 = field(rt, l1281, "width");
      at = 1769; l1295 = l1296;
    } else {
      at = 1770; l1297 = K[307];
      at = 1771; l1295 = l1297;
    }
    at = 1772; l1298 = K[308];
    at = 1773; l1299 = rt.int_div(l1295, l1298);
    at = 1774; l1300 = K[309];
    at = 1775; l1301 = rt.max(l1300, l1299);
    at = 1776; l1302 = K[310];
    at = 1777; l1303 = rt.gt(l1285, l1302);
    at = 1778; if (rt.truthy(l1303)) {
      at = 1779; if (rt.truthy(l1278)) {
        at = 1780; l1306 = field(rt, l1281, "color");
        at = 1781; l1305 = l1306;
      } else {
        at = 1782; l1307 = K[311];
        at = 1783; l1305 = l1307;
      }
      at = 1784; l1308 = K[312];
      at = 1785; l1309 = rt.sequence_get(l1308, l1305);
      at = 1786; l1304 = l1309;
    } else {
      at = 1787; l1310 = K[313];
      at = 1788; l1304 = l1310;
    }
    at = 1789; markSet(rt, false, "circle", {at: l1288, radius: l1301, fill: l1304});
    at = 1790; if (rt.truthy(l1278)) {
      at = 1791; l1312 = field(rt, l1281, "points");
      at = 1792; l1311 = l1312;
    } else {
      at = 1793; l1313 = K[314];
      at = 1794; l1311 = l1313;
    }
    at = 1795; l1314 = K[315];
    at = 1796; l1315 = rt.sub(l1285, l1314);
    at = 1797; l1316 = K[316];
    at = 1798; l1317 = rt.max(l1316, l1315);
    at = 1799; l1318 = K[317];
    at = 1800; l1319 = rt.array_slice(l1311, l1318, l1317);
    at = 1801; if (rt.truthy(l1278)) {
      at = 1802; l1321 = field(rt, l1281, "points");
      at = 1803; l1320 = l1321;
    } else {
      at = 1804; l1322 = K[318];
      at = 1805; l1320 = l1322;
    }
    at = 1806; l1323 = K[319];
    at = 1807; l1324 = rt.gt(l1285, l1323);
    at = 1808; if (rt.truthy(l1324)) {
      at = 1809; l1326 = K[320];
      at = 1810; l1325 = l1326;
    } else {
      at = 1811; l1327 = K[321];
      at = 1812; l1325 = l1327;
    }
    at = 1813; l1328 = K[322];
    at = 1814; l1329 = rt.sub(l1285, l1328);
    at = 1815; l1330 = K[323];
    at = 1816; l1331 = rt.max(l1330, l1329);
    at = 1817; l1332 = rt.array_slice(l1320, l1325, l1331);
    at = 1818; if (rt.truthy(l1278)) {
      at = 1819; l1334 = field(rt, l1281, "color");
      at = 1820; l1333 = l1334;
    } else {
      at = 1821; l1335 = K[324];
      at = 1822; l1333 = l1335;
    }
    at = 1823; l1336 = K[325];
    at = 1824; l1337 = rt.sequence_get(l1336, l1333);
    at = 1825; if (rt.truthy(l1278)) {
      at = 1826; l1339 = field(rt, l1281, "width");
      at = 1827; l1338 = l1339;
    } else {
      at = 1828; l1340 = K[326];
      at = 1829; l1338 = l1340;
    }
    at = 1830; markSet(rt, true, "line", {from: l1319, to: l1332, color: l1337, width: l1338});
    at = 1831; l1341 = field(rt, l15, "strokes");
    at = 1832; l1342 = rt.sequence_length(l1341);
    at = 1833; l1343 = K[327];
    at = 1834; l1344 = rt.lt(l1343, l1342);
    at = 1835; if (rt.truthy(l1344)) {
      at = 1836; l1345 = field(rt, l15, "strokes");
      at = 1837; l1346 = K[328];
      at = 1838; l1347 = rt.sequence_get(l1345, l1346);
    }
    at = 1839; if (rt.truthy(l1344)) {
      at = 1840; l1349 = field(rt, l1347, "points");
      at = 1841; l1348 = l1349;
    } else {
      at = 1842; l1350 = K[329];
      at = 1843; l1348 = l1350;
    }
    at = 1844; l1351 = rt.array_count(l1348);
    at = 1845; l1352 = K[330];
    at = 1846; l1353 = rt.gt(l1351, l1352);
    at = 1847; if (rt.truthy(l1353)) {
      at = 1848; if (rt.truthy(l1344)) {
        at = 1849; l1356 = field(rt, l1347, "points");
        at = 1850; l1355 = l1356;
      } else {
        at = 1851; l1357 = K[331];
        at = 1852; l1355 = l1357;
      }
      at = 1853; l1358 = K[332];
      at = 1854; l1359 = rt.array_nth(l1355, l1358);
      at = 1855; l1354 = l1359;
    } else {
      at = 1856; l1360 = K[333];
      at = 1857; l1354 = l1360;
    }
    at = 1858; if (rt.truthy(l1344)) {
      at = 1859; l1362 = field(rt, l1347, "width");
      at = 1860; l1361 = l1362;
    } else {
      at = 1861; l1363 = K[334];
      at = 1862; l1361 = l1363;
    }
    at = 1863; l1364 = K[335];
    at = 1864; l1365 = rt.int_div(l1361, l1364);
    at = 1865; l1366 = K[336];
    at = 1866; l1367 = rt.max(l1366, l1365);
    at = 1867; l1368 = K[337];
    at = 1868; l1369 = rt.gt(l1351, l1368);
    at = 1869; if (rt.truthy(l1369)) {
      at = 1870; if (rt.truthy(l1344)) {
        at = 1871; l1372 = field(rt, l1347, "color");
        at = 1872; l1371 = l1372;
      } else {
        at = 1873; l1373 = K[338];
        at = 1874; l1371 = l1373;
      }
      at = 1875; l1374 = K[339];
      at = 1876; l1375 = rt.sequence_get(l1374, l1371);
      at = 1877; l1370 = l1375;
    } else {
      at = 1878; l1376 = K[340];
      at = 1879; l1370 = l1376;
    }
    at = 1880; markSet(rt, false, "circle", {at: l1354, radius: l1367, fill: l1370});
    at = 1881; if (rt.truthy(l1344)) {
      at = 1882; l1378 = field(rt, l1347, "points");
      at = 1883; l1377 = l1378;
    } else {
      at = 1884; l1379 = K[341];
      at = 1885; l1377 = l1379;
    }
    at = 1886; l1380 = K[342];
    at = 1887; l1381 = rt.sub(l1351, l1380);
    at = 1888; l1382 = K[343];
    at = 1889; l1383 = rt.max(l1382, l1381);
    at = 1890; l1384 = K[344];
    at = 1891; l1385 = rt.array_slice(l1377, l1384, l1383);
    at = 1892; if (rt.truthy(l1344)) {
      at = 1893; l1387 = field(rt, l1347, "points");
      at = 1894; l1386 = l1387;
    } else {
      at = 1895; l1388 = K[345];
      at = 1896; l1386 = l1388;
    }
    at = 1897; l1389 = K[346];
    at = 1898; l1390 = rt.gt(l1351, l1389);
    at = 1899; if (rt.truthy(l1390)) {
      at = 1900; l1392 = K[347];
      at = 1901; l1391 = l1392;
    } else {
      at = 1902; l1393 = K[348];
      at = 1903; l1391 = l1393;
    }
    at = 1904; l1394 = K[349];
    at = 1905; l1395 = rt.sub(l1351, l1394);
    at = 1906; l1396 = K[350];
    at = 1907; l1397 = rt.max(l1396, l1395);
    at = 1908; l1398 = rt.array_slice(l1386, l1391, l1397);
    at = 1909; if (rt.truthy(l1344)) {
      at = 1910; l1400 = field(rt, l1347, "color");
      at = 1911; l1399 = l1400;
    } else {
      at = 1912; l1401 = K[351];
      at = 1913; l1399 = l1401;
    }
    at = 1914; l1402 = K[352];
    at = 1915; l1403 = rt.sequence_get(l1402, l1399);
    at = 1916; if (rt.truthy(l1344)) {
      at = 1917; l1405 = field(rt, l1347, "width");
      at = 1918; l1404 = l1405;
    } else {
      at = 1919; l1406 = K[353];
      at = 1920; l1404 = l1406;
    }
    at = 1921; markSet(rt, true, "line", {from: l1385, to: l1398, color: l1403, width: l1404});
    at = 1922; l1407 = field(rt, l15, "strokes");
    at = 1923; l1408 = rt.sequence_length(l1407);
    at = 1924; l1409 = K[354];
    at = 1925; l1410 = rt.lt(l1409, l1408);
    at = 1926; if (rt.truthy(l1410)) {
      at = 1927; l1411 = field(rt, l15, "strokes");
      at = 1928; l1412 = K[355];
      at = 1929; l1413 = rt.sequence_get(l1411, l1412);
    }
    at = 1930; if (rt.truthy(l1410)) {
      at = 1931; l1415 = field(rt, l1413, "points");
      at = 1932; l1414 = l1415;
    } else {
      at = 1933; l1416 = K[356];
      at = 1934; l1414 = l1416;
    }
    at = 1935; l1417 = rt.array_count(l1414);
    at = 1936; l1418 = K[357];
    at = 1937; l1419 = rt.gt(l1417, l1418);
    at = 1938; if (rt.truthy(l1419)) {
      at = 1939; if (rt.truthy(l1410)) {
        at = 1940; l1422 = field(rt, l1413, "points");
        at = 1941; l1421 = l1422;
      } else {
        at = 1942; l1423 = K[358];
        at = 1943; l1421 = l1423;
      }
      at = 1944; l1424 = K[359];
      at = 1945; l1425 = rt.array_nth(l1421, l1424);
      at = 1946; l1420 = l1425;
    } else {
      at = 1947; l1426 = K[360];
      at = 1948; l1420 = l1426;
    }
    at = 1949; if (rt.truthy(l1410)) {
      at = 1950; l1428 = field(rt, l1413, "width");
      at = 1951; l1427 = l1428;
    } else {
      at = 1952; l1429 = K[361];
      at = 1953; l1427 = l1429;
    }
    at = 1954; l1430 = K[362];
    at = 1955; l1431 = rt.int_div(l1427, l1430);
    at = 1956; l1432 = K[363];
    at = 1957; l1433 = rt.max(l1432, l1431);
    at = 1958; l1434 = K[364];
    at = 1959; l1435 = rt.gt(l1417, l1434);
    at = 1960; if (rt.truthy(l1435)) {
      at = 1961; if (rt.truthy(l1410)) {
        at = 1962; l1438 = field(rt, l1413, "color");
        at = 1963; l1437 = l1438;
      } else {
        at = 1964; l1439 = K[365];
        at = 1965; l1437 = l1439;
      }
      at = 1966; l1440 = K[366];
      at = 1967; l1441 = rt.sequence_get(l1440, l1437);
      at = 1968; l1436 = l1441;
    } else {
      at = 1969; l1442 = K[367];
      at = 1970; l1436 = l1442;
    }
    at = 1971; markSet(rt, false, "circle", {at: l1420, radius: l1433, fill: l1436});
    at = 1972; if (rt.truthy(l1410)) {
      at = 1973; l1444 = field(rt, l1413, "points");
      at = 1974; l1443 = l1444;
    } else {
      at = 1975; l1445 = K[368];
      at = 1976; l1443 = l1445;
    }
    at = 1977; l1446 = K[369];
    at = 1978; l1447 = rt.sub(l1417, l1446);
    at = 1979; l1448 = K[370];
    at = 1980; l1449 = rt.max(l1448, l1447);
    at = 1981; l1450 = K[371];
    at = 1982; l1451 = rt.array_slice(l1443, l1450, l1449);
    at = 1983; if (rt.truthy(l1410)) {
      at = 1984; l1453 = field(rt, l1413, "points");
      at = 1985; l1452 = l1453;
    } else {
      at = 1986; l1454 = K[372];
      at = 1987; l1452 = l1454;
    }
    at = 1988; l1455 = K[373];
    at = 1989; l1456 = rt.gt(l1417, l1455);
    at = 1990; if (rt.truthy(l1456)) {
      at = 1991; l1458 = K[374];
      at = 1992; l1457 = l1458;
    } else {
      at = 1993; l1459 = K[375];
      at = 1994; l1457 = l1459;
    }
    at = 1995; l1460 = K[376];
    at = 1996; l1461 = rt.sub(l1417, l1460);
    at = 1997; l1462 = K[377];
    at = 1998; l1463 = rt.max(l1462, l1461);
    at = 1999; l1464 = rt.array_slice(l1452, l1457, l1463);
    at = 2000; if (rt.truthy(l1410)) {
      at = 2001; l1466 = field(rt, l1413, "color");
      at = 2002; l1465 = l1466;
    } else {
      at = 2003; l1467 = K[378];
      at = 2004; l1465 = l1467;
    }
    at = 2005; l1468 = K[379];
    at = 2006; l1469 = rt.sequence_get(l1468, l1465);
    at = 2007; if (rt.truthy(l1410)) {
      at = 2008; l1471 = field(rt, l1413, "width");
      at = 2009; l1470 = l1471;
    } else {
      at = 2010; l1472 = K[380];
      at = 2011; l1470 = l1472;
    }
    at = 2012; markSet(rt, true, "line", {from: l1451, to: l1464, color: l1469, width: l1470});
    at = 2013; l1473 = field(rt, l15, "strokes");
    at = 2014; l1474 = rt.sequence_length(l1473);
    at = 2015; l1475 = K[381];
    at = 2016; l1476 = rt.lt(l1475, l1474);
    at = 2017; if (rt.truthy(l1476)) {
      at = 2018; l1477 = field(rt, l15, "strokes");
      at = 2019; l1478 = K[382];
      at = 2020; l1479 = rt.sequence_get(l1477, l1478);
    }
    at = 2021; if (rt.truthy(l1476)) {
      at = 2022; l1481 = field(rt, l1479, "points");
      at = 2023; l1480 = l1481;
    } else {
      at = 2024; l1482 = K[383];
      at = 2025; l1480 = l1482;
    }
    at = 2026; l1483 = rt.array_count(l1480);
    at = 2027; l1484 = K[384];
    at = 2028; l1485 = rt.gt(l1483, l1484);
    at = 2029; if (rt.truthy(l1485)) {
      at = 2030; if (rt.truthy(l1476)) {
        at = 2031; l1488 = field(rt, l1479, "points");
        at = 2032; l1487 = l1488;
      } else {
        at = 2033; l1489 = K[385];
        at = 2034; l1487 = l1489;
      }
      at = 2035; l1490 = K[386];
      at = 2036; l1491 = rt.array_nth(l1487, l1490);
      at = 2037; l1486 = l1491;
    } else {
      at = 2038; l1492 = K[387];
      at = 2039; l1486 = l1492;
    }
    at = 2040; if (rt.truthy(l1476)) {
      at = 2041; l1494 = field(rt, l1479, "width");
      at = 2042; l1493 = l1494;
    } else {
      at = 2043; l1495 = K[388];
      at = 2044; l1493 = l1495;
    }
    at = 2045; l1496 = K[389];
    at = 2046; l1497 = rt.int_div(l1493, l1496);
    at = 2047; l1498 = K[390];
    at = 2048; l1499 = rt.max(l1498, l1497);
    at = 2049; l1500 = K[391];
    at = 2050; l1501 = rt.gt(l1483, l1500);
    at = 2051; if (rt.truthy(l1501)) {
      at = 2052; if (rt.truthy(l1476)) {
        at = 2053; l1504 = field(rt, l1479, "color");
        at = 2054; l1503 = l1504;
      } else {
        at = 2055; l1505 = K[392];
        at = 2056; l1503 = l1505;
      }
      at = 2057; l1506 = K[393];
      at = 2058; l1507 = rt.sequence_get(l1506, l1503);
      at = 2059; l1502 = l1507;
    } else {
      at = 2060; l1508 = K[394];
      at = 2061; l1502 = l1508;
    }
    at = 2062; markSet(rt, false, "circle", {at: l1486, radius: l1499, fill: l1502});
    at = 2063; if (rt.truthy(l1476)) {
      at = 2064; l1510 = field(rt, l1479, "points");
      at = 2065; l1509 = l1510;
    } else {
      at = 2066; l1511 = K[395];
      at = 2067; l1509 = l1511;
    }
    at = 2068; l1512 = K[396];
    at = 2069; l1513 = rt.sub(l1483, l1512);
    at = 2070; l1514 = K[397];
    at = 2071; l1515 = rt.max(l1514, l1513);
    at = 2072; l1516 = K[398];
    at = 2073; l1517 = rt.array_slice(l1509, l1516, l1515);
    at = 2074; if (rt.truthy(l1476)) {
      at = 2075; l1519 = field(rt, l1479, "points");
      at = 2076; l1518 = l1519;
    } else {
      at = 2077; l1520 = K[399];
      at = 2078; l1518 = l1520;
    }
    at = 2079; l1521 = K[400];
    at = 2080; l1522 = rt.gt(l1483, l1521);
    at = 2081; if (rt.truthy(l1522)) {
      at = 2082; l1524 = K[401];
      at = 2083; l1523 = l1524;
    } else {
      at = 2084; l1525 = K[402];
      at = 2085; l1523 = l1525;
    }
    at = 2086; l1526 = K[403];
    at = 2087; l1527 = rt.sub(l1483, l1526);
    at = 2088; l1528 = K[404];
    at = 2089; l1529 = rt.max(l1528, l1527);
    at = 2090; l1530 = rt.array_slice(l1518, l1523, l1529);
    at = 2091; if (rt.truthy(l1476)) {
      at = 2092; l1532 = field(rt, l1479, "color");
      at = 2093; l1531 = l1532;
    } else {
      at = 2094; l1533 = K[405];
      at = 2095; l1531 = l1533;
    }
    at = 2096; l1534 = K[406];
    at = 2097; l1535 = rt.sequence_get(l1534, l1531);
    at = 2098; if (rt.truthy(l1476)) {
      at = 2099; l1537 = field(rt, l1479, "width");
      at = 2100; l1536 = l1537;
    } else {
      at = 2101; l1538 = K[407];
      at = 2102; l1536 = l1538;
    }
    at = 2103; markSet(rt, true, "line", {from: l1517, to: l1530, color: l1535, width: l1536});
    at = 2104; l1539 = field(rt, l15, "strokes");
    at = 2105; l1540 = rt.sequence_length(l1539);
    at = 2106; l1541 = K[408];
    at = 2107; l1542 = rt.lt(l1541, l1540);
    at = 2108; if (rt.truthy(l1542)) {
      at = 2109; l1543 = field(rt, l15, "strokes");
      at = 2110; l1544 = K[409];
      at = 2111; l1545 = rt.sequence_get(l1543, l1544);
    }
    at = 2112; if (rt.truthy(l1542)) {
      at = 2113; l1547 = field(rt, l1545, "points");
      at = 2114; l1546 = l1547;
    } else {
      at = 2115; l1548 = K[410];
      at = 2116; l1546 = l1548;
    }
    at = 2117; l1549 = rt.array_count(l1546);
    at = 2118; l1550 = K[411];
    at = 2119; l1551 = rt.gt(l1549, l1550);
    at = 2120; if (rt.truthy(l1551)) {
      at = 2121; if (rt.truthy(l1542)) {
        at = 2122; l1554 = field(rt, l1545, "points");
        at = 2123; l1553 = l1554;
      } else {
        at = 2124; l1555 = K[412];
        at = 2125; l1553 = l1555;
      }
      at = 2126; l1556 = K[413];
      at = 2127; l1557 = rt.array_nth(l1553, l1556);
      at = 2128; l1552 = l1557;
    } else {
      at = 2129; l1558 = K[414];
      at = 2130; l1552 = l1558;
    }
    at = 2131; if (rt.truthy(l1542)) {
      at = 2132; l1560 = field(rt, l1545, "width");
      at = 2133; l1559 = l1560;
    } else {
      at = 2134; l1561 = K[415];
      at = 2135; l1559 = l1561;
    }
    at = 2136; l1562 = K[416];
    at = 2137; l1563 = rt.int_div(l1559, l1562);
    at = 2138; l1564 = K[417];
    at = 2139; l1565 = rt.max(l1564, l1563);
    at = 2140; l1566 = K[418];
    at = 2141; l1567 = rt.gt(l1549, l1566);
    at = 2142; if (rt.truthy(l1567)) {
      at = 2143; if (rt.truthy(l1542)) {
        at = 2144; l1570 = field(rt, l1545, "color");
        at = 2145; l1569 = l1570;
      } else {
        at = 2146; l1571 = K[419];
        at = 2147; l1569 = l1571;
      }
      at = 2148; l1572 = K[420];
      at = 2149; l1573 = rt.sequence_get(l1572, l1569);
      at = 2150; l1568 = l1573;
    } else {
      at = 2151; l1574 = K[421];
      at = 2152; l1568 = l1574;
    }
    at = 2153; markSet(rt, false, "circle", {at: l1552, radius: l1565, fill: l1568});
    at = 2154; if (rt.truthy(l1542)) {
      at = 2155; l1576 = field(rt, l1545, "points");
      at = 2156; l1575 = l1576;
    } else {
      at = 2157; l1577 = K[422];
      at = 2158; l1575 = l1577;
    }
    at = 2159; l1578 = K[423];
    at = 2160; l1579 = rt.sub(l1549, l1578);
    at = 2161; l1580 = K[424];
    at = 2162; l1581 = rt.max(l1580, l1579);
    at = 2163; l1582 = K[425];
    at = 2164; l1583 = rt.array_slice(l1575, l1582, l1581);
    at = 2165; if (rt.truthy(l1542)) {
      at = 2166; l1585 = field(rt, l1545, "points");
      at = 2167; l1584 = l1585;
    } else {
      at = 2168; l1586 = K[426];
      at = 2169; l1584 = l1586;
    }
    at = 2170; l1587 = K[427];
    at = 2171; l1588 = rt.gt(l1549, l1587);
    at = 2172; if (rt.truthy(l1588)) {
      at = 2173; l1590 = K[428];
      at = 2174; l1589 = l1590;
    } else {
      at = 2175; l1591 = K[429];
      at = 2176; l1589 = l1591;
    }
    at = 2177; l1592 = K[430];
    at = 2178; l1593 = rt.sub(l1549, l1592);
    at = 2179; l1594 = K[431];
    at = 2180; l1595 = rt.max(l1594, l1593);
    at = 2181; l1596 = rt.array_slice(l1584, l1589, l1595);
    at = 2182; if (rt.truthy(l1542)) {
      at = 2183; l1598 = field(rt, l1545, "color");
      at = 2184; l1597 = l1598;
    } else {
      at = 2185; l1599 = K[432];
      at = 2186; l1597 = l1599;
    }
    at = 2187; l1600 = K[433];
    at = 2188; l1601 = rt.sequence_get(l1600, l1597);
    at = 2189; if (rt.truthy(l1542)) {
      at = 2190; l1603 = field(rt, l1545, "width");
      at = 2191; l1602 = l1603;
    } else {
      at = 2192; l1604 = K[434];
      at = 2193; l1602 = l1604;
    }
    at = 2194; markSet(rt, true, "line", {from: l1583, to: l1596, color: l1601, width: l1602});
    at = 2195; l1605 = field(rt, l15, "strokes");
    at = 2196; l1606 = rt.sequence_length(l1605);
    at = 2197; l1607 = K[435];
    at = 2198; l1608 = rt.lt(l1607, l1606);
    at = 2199; if (rt.truthy(l1608)) {
      at = 2200; l1609 = field(rt, l15, "strokes");
      at = 2201; l1610 = K[436];
      at = 2202; l1611 = rt.sequence_get(l1609, l1610);
    }
    at = 2203; if (rt.truthy(l1608)) {
      at = 2204; l1613 = field(rt, l1611, "points");
      at = 2205; l1612 = l1613;
    } else {
      at = 2206; l1614 = K[437];
      at = 2207; l1612 = l1614;
    }
    at = 2208; l1615 = rt.array_count(l1612);
    at = 2209; l1616 = K[438];
    at = 2210; l1617 = rt.gt(l1615, l1616);
    at = 2211; if (rt.truthy(l1617)) {
      at = 2212; if (rt.truthy(l1608)) {
        at = 2213; l1620 = field(rt, l1611, "points");
        at = 2214; l1619 = l1620;
      } else {
        at = 2215; l1621 = K[439];
        at = 2216; l1619 = l1621;
      }
      at = 2217; l1622 = K[440];
      at = 2218; l1623 = rt.array_nth(l1619, l1622);
      at = 2219; l1618 = l1623;
    } else {
      at = 2220; l1624 = K[441];
      at = 2221; l1618 = l1624;
    }
    at = 2222; if (rt.truthy(l1608)) {
      at = 2223; l1626 = field(rt, l1611, "width");
      at = 2224; l1625 = l1626;
    } else {
      at = 2225; l1627 = K[442];
      at = 2226; l1625 = l1627;
    }
    at = 2227; l1628 = K[443];
    at = 2228; l1629 = rt.int_div(l1625, l1628);
    at = 2229; l1630 = K[444];
    at = 2230; l1631 = rt.max(l1630, l1629);
    at = 2231; l1632 = K[445];
    at = 2232; l1633 = rt.gt(l1615, l1632);
    at = 2233; if (rt.truthy(l1633)) {
      at = 2234; if (rt.truthy(l1608)) {
        at = 2235; l1636 = field(rt, l1611, "color");
        at = 2236; l1635 = l1636;
      } else {
        at = 2237; l1637 = K[446];
        at = 2238; l1635 = l1637;
      }
      at = 2239; l1638 = K[447];
      at = 2240; l1639 = rt.sequence_get(l1638, l1635);
      at = 2241; l1634 = l1639;
    } else {
      at = 2242; l1640 = K[448];
      at = 2243; l1634 = l1640;
    }
    at = 2244; markSet(rt, false, "circle", {at: l1618, radius: l1631, fill: l1634});
    at = 2245; if (rt.truthy(l1608)) {
      at = 2246; l1642 = field(rt, l1611, "points");
      at = 2247; l1641 = l1642;
    } else {
      at = 2248; l1643 = K[449];
      at = 2249; l1641 = l1643;
    }
    at = 2250; l1644 = K[450];
    at = 2251; l1645 = rt.sub(l1615, l1644);
    at = 2252; l1646 = K[451];
    at = 2253; l1647 = rt.max(l1646, l1645);
    at = 2254; l1648 = K[452];
    at = 2255; l1649 = rt.array_slice(l1641, l1648, l1647);
    at = 2256; if (rt.truthy(l1608)) {
      at = 2257; l1651 = field(rt, l1611, "points");
      at = 2258; l1650 = l1651;
    } else {
      at = 2259; l1652 = K[453];
      at = 2260; l1650 = l1652;
    }
    at = 2261; l1653 = K[454];
    at = 2262; l1654 = rt.gt(l1615, l1653);
    at = 2263; if (rt.truthy(l1654)) {
      at = 2264; l1656 = K[455];
      at = 2265; l1655 = l1656;
    } else {
      at = 2266; l1657 = K[456];
      at = 2267; l1655 = l1657;
    }
    at = 2268; l1658 = K[457];
    at = 2269; l1659 = rt.sub(l1615, l1658);
    at = 2270; l1660 = K[458];
    at = 2271; l1661 = rt.max(l1660, l1659);
    at = 2272; l1662 = rt.array_slice(l1650, l1655, l1661);
    at = 2273; if (rt.truthy(l1608)) {
      at = 2274; l1664 = field(rt, l1611, "color");
      at = 2275; l1663 = l1664;
    } else {
      at = 2276; l1665 = K[459];
      at = 2277; l1663 = l1665;
    }
    at = 2278; l1666 = K[460];
    at = 2279; l1667 = rt.sequence_get(l1666, l1663);
    at = 2280; if (rt.truthy(l1608)) {
      at = 2281; l1669 = field(rt, l1611, "width");
      at = 2282; l1668 = l1669;
    } else {
      at = 2283; l1670 = K[461];
      at = 2284; l1668 = l1670;
    }
    at = 2285; markSet(rt, true, "line", {from: l1649, to: l1662, color: l1667, width: l1668});
    at = 2286; l1671 = field(rt, l15, "strokes");
    at = 2287; l1672 = rt.sequence_length(l1671);
    at = 2288; l1673 = K[462];
    at = 2289; l1674 = rt.lt(l1673, l1672);
    at = 2290; if (rt.truthy(l1674)) {
      at = 2291; l1675 = field(rt, l15, "strokes");
      at = 2292; l1676 = K[463];
      at = 2293; l1677 = rt.sequence_get(l1675, l1676);
    }
    at = 2294; if (rt.truthy(l1674)) {
      at = 2295; l1679 = field(rt, l1677, "points");
      at = 2296; l1678 = l1679;
    } else {
      at = 2297; l1680 = K[464];
      at = 2298; l1678 = l1680;
    }
    at = 2299; l1681 = rt.array_count(l1678);
    at = 2300; l1682 = K[465];
    at = 2301; l1683 = rt.gt(l1681, l1682);
    at = 2302; if (rt.truthy(l1683)) {
      at = 2303; if (rt.truthy(l1674)) {
        at = 2304; l1686 = field(rt, l1677, "points");
        at = 2305; l1685 = l1686;
      } else {
        at = 2306; l1687 = K[466];
        at = 2307; l1685 = l1687;
      }
      at = 2308; l1688 = K[467];
      at = 2309; l1689 = rt.array_nth(l1685, l1688);
      at = 2310; l1684 = l1689;
    } else {
      at = 2311; l1690 = K[468];
      at = 2312; l1684 = l1690;
    }
    at = 2313; if (rt.truthy(l1674)) {
      at = 2314; l1692 = field(rt, l1677, "width");
      at = 2315; l1691 = l1692;
    } else {
      at = 2316; l1693 = K[469];
      at = 2317; l1691 = l1693;
    }
    at = 2318; l1694 = K[470];
    at = 2319; l1695 = rt.int_div(l1691, l1694);
    at = 2320; l1696 = K[471];
    at = 2321; l1697 = rt.max(l1696, l1695);
    at = 2322; l1698 = K[472];
    at = 2323; l1699 = rt.gt(l1681, l1698);
    at = 2324; if (rt.truthy(l1699)) {
      at = 2325; if (rt.truthy(l1674)) {
        at = 2326; l1702 = field(rt, l1677, "color");
        at = 2327; l1701 = l1702;
      } else {
        at = 2328; l1703 = K[473];
        at = 2329; l1701 = l1703;
      }
      at = 2330; l1704 = K[474];
      at = 2331; l1705 = rt.sequence_get(l1704, l1701);
      at = 2332; l1700 = l1705;
    } else {
      at = 2333; l1706 = K[475];
      at = 2334; l1700 = l1706;
    }
    at = 2335; markSet(rt, false, "circle", {at: l1684, radius: l1697, fill: l1700});
    at = 2336; if (rt.truthy(l1674)) {
      at = 2337; l1708 = field(rt, l1677, "points");
      at = 2338; l1707 = l1708;
    } else {
      at = 2339; l1709 = K[476];
      at = 2340; l1707 = l1709;
    }
    at = 2341; l1710 = K[477];
    at = 2342; l1711 = rt.sub(l1681, l1710);
    at = 2343; l1712 = K[478];
    at = 2344; l1713 = rt.max(l1712, l1711);
    at = 2345; l1714 = K[479];
    at = 2346; l1715 = rt.array_slice(l1707, l1714, l1713);
    at = 2347; if (rt.truthy(l1674)) {
      at = 2348; l1717 = field(rt, l1677, "points");
      at = 2349; l1716 = l1717;
    } else {
      at = 2350; l1718 = K[480];
      at = 2351; l1716 = l1718;
    }
    at = 2352; l1719 = K[481];
    at = 2353; l1720 = rt.gt(l1681, l1719);
    at = 2354; if (rt.truthy(l1720)) {
      at = 2355; l1722 = K[482];
      at = 2356; l1721 = l1722;
    } else {
      at = 2357; l1723 = K[483];
      at = 2358; l1721 = l1723;
    }
    at = 2359; l1724 = K[484];
    at = 2360; l1725 = rt.sub(l1681, l1724);
    at = 2361; l1726 = K[485];
    at = 2362; l1727 = rt.max(l1726, l1725);
    at = 2363; l1728 = rt.array_slice(l1716, l1721, l1727);
    at = 2364; if (rt.truthy(l1674)) {
      at = 2365; l1730 = field(rt, l1677, "color");
      at = 2366; l1729 = l1730;
    } else {
      at = 2367; l1731 = K[486];
      at = 2368; l1729 = l1731;
    }
    at = 2369; l1732 = K[487];
    at = 2370; l1733 = rt.sequence_get(l1732, l1729);
    at = 2371; if (rt.truthy(l1674)) {
      at = 2372; l1735 = field(rt, l1677, "width");
      at = 2373; l1734 = l1735;
    } else {
      at = 2374; l1736 = K[488];
      at = 2375; l1734 = l1736;
    }
    at = 2376; markSet(rt, true, "line", {from: l1715, to: l1728, color: l1733, width: l1734});
    at = 2377; l1737 = field(rt, l15, "strokes");
    at = 2378; l1738 = rt.sequence_length(l1737);
    at = 2379; l1739 = K[489];
    at = 2380; l1740 = rt.lt(l1739, l1738);
    at = 2381; if (rt.truthy(l1740)) {
      at = 2382; l1741 = field(rt, l15, "strokes");
      at = 2383; l1742 = K[490];
      at = 2384; l1743 = rt.sequence_get(l1741, l1742);
    }
    at = 2385; if (rt.truthy(l1740)) {
      at = 2386; l1745 = field(rt, l1743, "points");
      at = 2387; l1744 = l1745;
    } else {
      at = 2388; l1746 = K[491];
      at = 2389; l1744 = l1746;
    }
    at = 2390; l1747 = rt.array_count(l1744);
    at = 2391; l1748 = K[492];
    at = 2392; l1749 = rt.gt(l1747, l1748);
    at = 2393; if (rt.truthy(l1749)) {
      at = 2394; if (rt.truthy(l1740)) {
        at = 2395; l1752 = field(rt, l1743, "points");
        at = 2396; l1751 = l1752;
      } else {
        at = 2397; l1753 = K[493];
        at = 2398; l1751 = l1753;
      }
      at = 2399; l1754 = K[494];
      at = 2400; l1755 = rt.array_nth(l1751, l1754);
      at = 2401; l1750 = l1755;
    } else {
      at = 2402; l1756 = K[495];
      at = 2403; l1750 = l1756;
    }
    at = 2404; if (rt.truthy(l1740)) {
      at = 2405; l1758 = field(rt, l1743, "width");
      at = 2406; l1757 = l1758;
    } else {
      at = 2407; l1759 = K[496];
      at = 2408; l1757 = l1759;
    }
    at = 2409; l1760 = K[497];
    at = 2410; l1761 = rt.int_div(l1757, l1760);
    at = 2411; l1762 = K[498];
    at = 2412; l1763 = rt.max(l1762, l1761);
    at = 2413; l1764 = K[499];
    at = 2414; l1765 = rt.gt(l1747, l1764);
    at = 2415; if (rt.truthy(l1765)) {
      at = 2416; if (rt.truthy(l1740)) {
        at = 2417; l1768 = field(rt, l1743, "color");
        at = 2418; l1767 = l1768;
      } else {
        at = 2419; l1769 = K[500];
        at = 2420; l1767 = l1769;
      }
      at = 2421; l1770 = K[501];
      at = 2422; l1771 = rt.sequence_get(l1770, l1767);
      at = 2423; l1766 = l1771;
    } else {
      at = 2424; l1772 = K[502];
      at = 2425; l1766 = l1772;
    }
    at = 2426; markSet(rt, false, "circle", {at: l1750, radius: l1763, fill: l1766});
    at = 2427; if (rt.truthy(l1740)) {
      at = 2428; l1774 = field(rt, l1743, "points");
      at = 2429; l1773 = l1774;
    } else {
      at = 2430; l1775 = K[503];
      at = 2431; l1773 = l1775;
    }
    at = 2432; l1776 = K[504];
    at = 2433; l1777 = rt.sub(l1747, l1776);
    at = 2434; l1778 = K[505];
    at = 2435; l1779 = rt.max(l1778, l1777);
    at = 2436; l1780 = K[506];
    at = 2437; l1781 = rt.array_slice(l1773, l1780, l1779);
    at = 2438; if (rt.truthy(l1740)) {
      at = 2439; l1783 = field(rt, l1743, "points");
      at = 2440; l1782 = l1783;
    } else {
      at = 2441; l1784 = K[507];
      at = 2442; l1782 = l1784;
    }
    at = 2443; l1785 = K[508];
    at = 2444; l1786 = rt.gt(l1747, l1785);
    at = 2445; if (rt.truthy(l1786)) {
      at = 2446; l1788 = K[509];
      at = 2447; l1787 = l1788;
    } else {
      at = 2448; l1789 = K[510];
      at = 2449; l1787 = l1789;
    }
    at = 2450; l1790 = K[511];
    at = 2451; l1791 = rt.sub(l1747, l1790);
    at = 2452; l1792 = K[512];
    at = 2453; l1793 = rt.max(l1792, l1791);
    at = 2454; l1794 = rt.array_slice(l1782, l1787, l1793);
    at = 2455; if (rt.truthy(l1740)) {
      at = 2456; l1796 = field(rt, l1743, "color");
      at = 2457; l1795 = l1796;
    } else {
      at = 2458; l1797 = K[513];
      at = 2459; l1795 = l1797;
    }
    at = 2460; l1798 = K[514];
    at = 2461; l1799 = rt.sequence_get(l1798, l1795);
    at = 2462; if (rt.truthy(l1740)) {
      at = 2463; l1801 = field(rt, l1743, "width");
      at = 2464; l1800 = l1801;
    } else {
      at = 2465; l1802 = K[515];
      at = 2466; l1800 = l1802;
    }
    at = 2467; markSet(rt, true, "line", {from: l1781, to: l1794, color: l1799, width: l1800});
    at = 2468; l1803 = field(rt, l15, "strokes");
    at = 2469; l1804 = rt.sequence_length(l1803);
    at = 2470; l1805 = K[516];
    at = 2471; l1806 = rt.lt(l1805, l1804);
    at = 2472; if (rt.truthy(l1806)) {
      at = 2473; l1807 = field(rt, l15, "strokes");
      at = 2474; l1808 = K[517];
      at = 2475; l1809 = rt.sequence_get(l1807, l1808);
    }
    at = 2476; if (rt.truthy(l1806)) {
      at = 2477; l1811 = field(rt, l1809, "points");
      at = 2478; l1810 = l1811;
    } else {
      at = 2479; l1812 = K[518];
      at = 2480; l1810 = l1812;
    }
    at = 2481; l1813 = rt.array_count(l1810);
    at = 2482; l1814 = K[519];
    at = 2483; l1815 = rt.gt(l1813, l1814);
    at = 2484; if (rt.truthy(l1815)) {
      at = 2485; if (rt.truthy(l1806)) {
        at = 2486; l1818 = field(rt, l1809, "points");
        at = 2487; l1817 = l1818;
      } else {
        at = 2488; l1819 = K[520];
        at = 2489; l1817 = l1819;
      }
      at = 2490; l1820 = K[521];
      at = 2491; l1821 = rt.array_nth(l1817, l1820);
      at = 2492; l1816 = l1821;
    } else {
      at = 2493; l1822 = K[522];
      at = 2494; l1816 = l1822;
    }
    at = 2495; if (rt.truthy(l1806)) {
      at = 2496; l1824 = field(rt, l1809, "width");
      at = 2497; l1823 = l1824;
    } else {
      at = 2498; l1825 = K[523];
      at = 2499; l1823 = l1825;
    }
    at = 2500; l1826 = K[524];
    at = 2501; l1827 = rt.int_div(l1823, l1826);
    at = 2502; l1828 = K[525];
    at = 2503; l1829 = rt.max(l1828, l1827);
    at = 2504; l1830 = K[526];
    at = 2505; l1831 = rt.gt(l1813, l1830);
    at = 2506; if (rt.truthy(l1831)) {
      at = 2507; if (rt.truthy(l1806)) {
        at = 2508; l1834 = field(rt, l1809, "color");
        at = 2509; l1833 = l1834;
      } else {
        at = 2510; l1835 = K[527];
        at = 2511; l1833 = l1835;
      }
      at = 2512; l1836 = K[528];
      at = 2513; l1837 = rt.sequence_get(l1836, l1833);
      at = 2514; l1832 = l1837;
    } else {
      at = 2515; l1838 = K[529];
      at = 2516; l1832 = l1838;
    }
    at = 2517; markSet(rt, false, "circle", {at: l1816, radius: l1829, fill: l1832});
    at = 2518; if (rt.truthy(l1806)) {
      at = 2519; l1840 = field(rt, l1809, "points");
      at = 2520; l1839 = l1840;
    } else {
      at = 2521; l1841 = K[530];
      at = 2522; l1839 = l1841;
    }
    at = 2523; l1842 = K[531];
    at = 2524; l1843 = rt.sub(l1813, l1842);
    at = 2525; l1844 = K[532];
    at = 2526; l1845 = rt.max(l1844, l1843);
    at = 2527; l1846 = K[533];
    at = 2528; l1847 = rt.array_slice(l1839, l1846, l1845);
    at = 2529; if (rt.truthy(l1806)) {
      at = 2530; l1849 = field(rt, l1809, "points");
      at = 2531; l1848 = l1849;
    } else {
      at = 2532; l1850 = K[534];
      at = 2533; l1848 = l1850;
    }
    at = 2534; l1851 = K[535];
    at = 2535; l1852 = rt.gt(l1813, l1851);
    at = 2536; if (rt.truthy(l1852)) {
      at = 2537; l1854 = K[536];
      at = 2538; l1853 = l1854;
    } else {
      at = 2539; l1855 = K[537];
      at = 2540; l1853 = l1855;
    }
    at = 2541; l1856 = K[538];
    at = 2542; l1857 = rt.sub(l1813, l1856);
    at = 2543; l1858 = K[539];
    at = 2544; l1859 = rt.max(l1858, l1857);
    at = 2545; l1860 = rt.array_slice(l1848, l1853, l1859);
    at = 2546; if (rt.truthy(l1806)) {
      at = 2547; l1862 = field(rt, l1809, "color");
      at = 2548; l1861 = l1862;
    } else {
      at = 2549; l1863 = K[540];
      at = 2550; l1861 = l1863;
    }
    at = 2551; l1864 = K[541];
    at = 2552; l1865 = rt.sequence_get(l1864, l1861);
    at = 2553; if (rt.truthy(l1806)) {
      at = 2554; l1867 = field(rt, l1809, "width");
      at = 2555; l1866 = l1867;
    } else {
      at = 2556; l1868 = K[542];
      at = 2557; l1866 = l1868;
    }
    at = 2558; markSet(rt, true, "line", {from: l1847, to: l1860, color: l1865, width: l1866});
    at = 2559; l1869 = field(rt, l15, "strokes");
    at = 2560; l1870 = rt.sequence_length(l1869);
    at = 2561; l1871 = K[543];
    at = 2562; l1872 = rt.lt(l1871, l1870);
    at = 2563; if (rt.truthy(l1872)) {
      at = 2564; l1873 = field(rt, l15, "strokes");
      at = 2565; l1874 = K[544];
      at = 2566; l1875 = rt.sequence_get(l1873, l1874);
    }
    at = 2567; if (rt.truthy(l1872)) {
      at = 2568; l1877 = field(rt, l1875, "points");
      at = 2569; l1876 = l1877;
    } else {
      at = 2570; l1878 = K[545];
      at = 2571; l1876 = l1878;
    }
    at = 2572; l1879 = rt.array_count(l1876);
    at = 2573; l1880 = K[546];
    at = 2574; l1881 = rt.gt(l1879, l1880);
    at = 2575; if (rt.truthy(l1881)) {
      at = 2576; if (rt.truthy(l1872)) {
        at = 2577; l1884 = field(rt, l1875, "points");
        at = 2578; l1883 = l1884;
      } else {
        at = 2579; l1885 = K[547];
        at = 2580; l1883 = l1885;
      }
      at = 2581; l1886 = K[548];
      at = 2582; l1887 = rt.array_nth(l1883, l1886);
      at = 2583; l1882 = l1887;
    } else {
      at = 2584; l1888 = K[549];
      at = 2585; l1882 = l1888;
    }
    at = 2586; if (rt.truthy(l1872)) {
      at = 2587; l1890 = field(rt, l1875, "width");
      at = 2588; l1889 = l1890;
    } else {
      at = 2589; l1891 = K[550];
      at = 2590; l1889 = l1891;
    }
    at = 2591; l1892 = K[551];
    at = 2592; l1893 = rt.int_div(l1889, l1892);
    at = 2593; l1894 = K[552];
    at = 2594; l1895 = rt.max(l1894, l1893);
    at = 2595; l1896 = K[553];
    at = 2596; l1897 = rt.gt(l1879, l1896);
    at = 2597; if (rt.truthy(l1897)) {
      at = 2598; if (rt.truthy(l1872)) {
        at = 2599; l1900 = field(rt, l1875, "color");
        at = 2600; l1899 = l1900;
      } else {
        at = 2601; l1901 = K[554];
        at = 2602; l1899 = l1901;
      }
      at = 2603; l1902 = K[555];
      at = 2604; l1903 = rt.sequence_get(l1902, l1899);
      at = 2605; l1898 = l1903;
    } else {
      at = 2606; l1904 = K[556];
      at = 2607; l1898 = l1904;
    }
    at = 2608; markSet(rt, false, "circle", {at: l1882, radius: l1895, fill: l1898});
    at = 2609; if (rt.truthy(l1872)) {
      at = 2610; l1906 = field(rt, l1875, "points");
      at = 2611; l1905 = l1906;
    } else {
      at = 2612; l1907 = K[557];
      at = 2613; l1905 = l1907;
    }
    at = 2614; l1908 = K[558];
    at = 2615; l1909 = rt.sub(l1879, l1908);
    at = 2616; l1910 = K[559];
    at = 2617; l1911 = rt.max(l1910, l1909);
    at = 2618; l1912 = K[560];
    at = 2619; l1913 = rt.array_slice(l1905, l1912, l1911);
    at = 2620; if (rt.truthy(l1872)) {
      at = 2621; l1915 = field(rt, l1875, "points");
      at = 2622; l1914 = l1915;
    } else {
      at = 2623; l1916 = K[561];
      at = 2624; l1914 = l1916;
    }
    at = 2625; l1917 = K[562];
    at = 2626; l1918 = rt.gt(l1879, l1917);
    at = 2627; if (rt.truthy(l1918)) {
      at = 2628; l1920 = K[563];
      at = 2629; l1919 = l1920;
    } else {
      at = 2630; l1921 = K[564];
      at = 2631; l1919 = l1921;
    }
    at = 2632; l1922 = K[565];
    at = 2633; l1923 = rt.sub(l1879, l1922);
    at = 2634; l1924 = K[566];
    at = 2635; l1925 = rt.max(l1924, l1923);
    at = 2636; l1926 = rt.array_slice(l1914, l1919, l1925);
    at = 2637; if (rt.truthy(l1872)) {
      at = 2638; l1928 = field(rt, l1875, "color");
      at = 2639; l1927 = l1928;
    } else {
      at = 2640; l1929 = K[567];
      at = 2641; l1927 = l1929;
    }
    at = 2642; l1930 = K[568];
    at = 2643; l1931 = rt.sequence_get(l1930, l1927);
    at = 2644; if (rt.truthy(l1872)) {
      at = 2645; l1933 = field(rt, l1875, "width");
      at = 2646; l1932 = l1933;
    } else {
      at = 2647; l1934 = K[569];
      at = 2648; l1932 = l1934;
    }
    at = 2649; markSet(rt, true, "line", {from: l1913, to: l1926, color: l1931, width: l1932});
    at = 2650; l1935 = field(rt, l15, "strokes");
    at = 2651; l1936 = rt.sequence_length(l1935);
    at = 2652; l1937 = K[570];
    at = 2653; l1938 = rt.lt(l1937, l1936);
    at = 2654; if (rt.truthy(l1938)) {
      at = 2655; l1939 = field(rt, l15, "strokes");
      at = 2656; l1940 = K[571];
      at = 2657; l1941 = rt.sequence_get(l1939, l1940);
    }
    at = 2658; if (rt.truthy(l1938)) {
      at = 2659; l1943 = field(rt, l1941, "points");
      at = 2660; l1942 = l1943;
    } else {
      at = 2661; l1944 = K[572];
      at = 2662; l1942 = l1944;
    }
    at = 2663; l1945 = rt.array_count(l1942);
    at = 2664; l1946 = K[573];
    at = 2665; l1947 = rt.gt(l1945, l1946);
    at = 2666; if (rt.truthy(l1947)) {
      at = 2667; if (rt.truthy(l1938)) {
        at = 2668; l1950 = field(rt, l1941, "points");
        at = 2669; l1949 = l1950;
      } else {
        at = 2670; l1951 = K[574];
        at = 2671; l1949 = l1951;
      }
      at = 2672; l1952 = K[575];
      at = 2673; l1953 = rt.array_nth(l1949, l1952);
      at = 2674; l1948 = l1953;
    } else {
      at = 2675; l1954 = K[576];
      at = 2676; l1948 = l1954;
    }
    at = 2677; if (rt.truthy(l1938)) {
      at = 2678; l1956 = field(rt, l1941, "width");
      at = 2679; l1955 = l1956;
    } else {
      at = 2680; l1957 = K[577];
      at = 2681; l1955 = l1957;
    }
    at = 2682; l1958 = K[578];
    at = 2683; l1959 = rt.int_div(l1955, l1958);
    at = 2684; l1960 = K[579];
    at = 2685; l1961 = rt.max(l1960, l1959);
    at = 2686; l1962 = K[580];
    at = 2687; l1963 = rt.gt(l1945, l1962);
    at = 2688; if (rt.truthy(l1963)) {
      at = 2689; if (rt.truthy(l1938)) {
        at = 2690; l1966 = field(rt, l1941, "color");
        at = 2691; l1965 = l1966;
      } else {
        at = 2692; l1967 = K[581];
        at = 2693; l1965 = l1967;
      }
      at = 2694; l1968 = K[582];
      at = 2695; l1969 = rt.sequence_get(l1968, l1965);
      at = 2696; l1964 = l1969;
    } else {
      at = 2697; l1970 = K[583];
      at = 2698; l1964 = l1970;
    }
    at = 2699; markSet(rt, false, "circle", {at: l1948, radius: l1961, fill: l1964});
    at = 2700; if (rt.truthy(l1938)) {
      at = 2701; l1972 = field(rt, l1941, "points");
      at = 2702; l1971 = l1972;
    } else {
      at = 2703; l1973 = K[584];
      at = 2704; l1971 = l1973;
    }
    at = 2705; l1974 = K[585];
    at = 2706; l1975 = rt.sub(l1945, l1974);
    at = 2707; l1976 = K[586];
    at = 2708; l1977 = rt.max(l1976, l1975);
    at = 2709; l1978 = K[587];
    at = 2710; l1979 = rt.array_slice(l1971, l1978, l1977);
    at = 2711; if (rt.truthy(l1938)) {
      at = 2712; l1981 = field(rt, l1941, "points");
      at = 2713; l1980 = l1981;
    } else {
      at = 2714; l1982 = K[588];
      at = 2715; l1980 = l1982;
    }
    at = 2716; l1983 = K[589];
    at = 2717; l1984 = rt.gt(l1945, l1983);
    at = 2718; if (rt.truthy(l1984)) {
      at = 2719; l1986 = K[590];
      at = 2720; l1985 = l1986;
    } else {
      at = 2721; l1987 = K[591];
      at = 2722; l1985 = l1987;
    }
    at = 2723; l1988 = K[592];
    at = 2724; l1989 = rt.sub(l1945, l1988);
    at = 2725; l1990 = K[593];
    at = 2726; l1991 = rt.max(l1990, l1989);
    at = 2727; l1992 = rt.array_slice(l1980, l1985, l1991);
    at = 2728; if (rt.truthy(l1938)) {
      at = 2729; l1994 = field(rt, l1941, "color");
      at = 2730; l1993 = l1994;
    } else {
      at = 2731; l1995 = K[594];
      at = 2732; l1993 = l1995;
    }
    at = 2733; l1996 = K[595];
    at = 2734; l1997 = rt.sequence_get(l1996, l1993);
    at = 2735; if (rt.truthy(l1938)) {
      at = 2736; l1999 = field(rt, l1941, "width");
      at = 2737; l1998 = l1999;
    } else {
      at = 2738; l2000 = K[596];
      at = 2739; l1998 = l2000;
    }
    at = 2740; markSet(rt, true, "line", {from: l1979, to: l1992, color: l1997, width: l1998});
    at = 2741; l2001 = field(rt, l15, "strokes");
    at = 2742; l2002 = rt.sequence_length(l2001);
    at = 2743; l2003 = K[597];
    at = 2744; l2004 = rt.lt(l2003, l2002);
    at = 2745; if (rt.truthy(l2004)) {
      at = 2746; l2005 = field(rt, l15, "strokes");
      at = 2747; l2006 = K[598];
      at = 2748; l2007 = rt.sequence_get(l2005, l2006);
    }
    at = 2749; if (rt.truthy(l2004)) {
      at = 2750; l2009 = field(rt, l2007, "points");
      at = 2751; l2008 = l2009;
    } else {
      at = 2752; l2010 = K[599];
      at = 2753; l2008 = l2010;
    }
    at = 2754; l2011 = rt.array_count(l2008);
    at = 2755; l2012 = K[600];
    at = 2756; l2013 = rt.gt(l2011, l2012);
    at = 2757; if (rt.truthy(l2013)) {
      at = 2758; if (rt.truthy(l2004)) {
        at = 2759; l2016 = field(rt, l2007, "points");
        at = 2760; l2015 = l2016;
      } else {
        at = 2761; l2017 = K[601];
        at = 2762; l2015 = l2017;
      }
      at = 2763; l2018 = K[602];
      at = 2764; l2019 = rt.array_nth(l2015, l2018);
      at = 2765; l2014 = l2019;
    } else {
      at = 2766; l2020 = K[603];
      at = 2767; l2014 = l2020;
    }
    at = 2768; if (rt.truthy(l2004)) {
      at = 2769; l2022 = field(rt, l2007, "width");
      at = 2770; l2021 = l2022;
    } else {
      at = 2771; l2023 = K[604];
      at = 2772; l2021 = l2023;
    }
    at = 2773; l2024 = K[605];
    at = 2774; l2025 = rt.int_div(l2021, l2024);
    at = 2775; l2026 = K[606];
    at = 2776; l2027 = rt.max(l2026, l2025);
    at = 2777; l2028 = K[607];
    at = 2778; l2029 = rt.gt(l2011, l2028);
    at = 2779; if (rt.truthy(l2029)) {
      at = 2780; if (rt.truthy(l2004)) {
        at = 2781; l2032 = field(rt, l2007, "color");
        at = 2782; l2031 = l2032;
      } else {
        at = 2783; l2033 = K[608];
        at = 2784; l2031 = l2033;
      }
      at = 2785; l2034 = K[609];
      at = 2786; l2035 = rt.sequence_get(l2034, l2031);
      at = 2787; l2030 = l2035;
    } else {
      at = 2788; l2036 = K[610];
      at = 2789; l2030 = l2036;
    }
    at = 2790; markSet(rt, false, "circle", {at: l2014, radius: l2027, fill: l2030});
    at = 2791; if (rt.truthy(l2004)) {
      at = 2792; l2038 = field(rt, l2007, "points");
      at = 2793; l2037 = l2038;
    } else {
      at = 2794; l2039 = K[611];
      at = 2795; l2037 = l2039;
    }
    at = 2796; l2040 = K[612];
    at = 2797; l2041 = rt.sub(l2011, l2040);
    at = 2798; l2042 = K[613];
    at = 2799; l2043 = rt.max(l2042, l2041);
    at = 2800; l2044 = K[614];
    at = 2801; l2045 = rt.array_slice(l2037, l2044, l2043);
    at = 2802; if (rt.truthy(l2004)) {
      at = 2803; l2047 = field(rt, l2007, "points");
      at = 2804; l2046 = l2047;
    } else {
      at = 2805; l2048 = K[615];
      at = 2806; l2046 = l2048;
    }
    at = 2807; l2049 = K[616];
    at = 2808; l2050 = rt.gt(l2011, l2049);
    at = 2809; if (rt.truthy(l2050)) {
      at = 2810; l2052 = K[617];
      at = 2811; l2051 = l2052;
    } else {
      at = 2812; l2053 = K[618];
      at = 2813; l2051 = l2053;
    }
    at = 2814; l2054 = K[619];
    at = 2815; l2055 = rt.sub(l2011, l2054);
    at = 2816; l2056 = K[620];
    at = 2817; l2057 = rt.max(l2056, l2055);
    at = 2818; l2058 = rt.array_slice(l2046, l2051, l2057);
    at = 2819; if (rt.truthy(l2004)) {
      at = 2820; l2060 = field(rt, l2007, "color");
      at = 2821; l2059 = l2060;
    } else {
      at = 2822; l2061 = K[621];
      at = 2823; l2059 = l2061;
    }
    at = 2824; l2062 = K[622];
    at = 2825; l2063 = rt.sequence_get(l2062, l2059);
    at = 2826; if (rt.truthy(l2004)) {
      at = 2827; l2065 = field(rt, l2007, "width");
      at = 2828; l2064 = l2065;
    } else {
      at = 2829; l2066 = K[623];
      at = 2830; l2064 = l2066;
    }
    at = 2831; markSet(rt, true, "line", {from: l2045, to: l2058, color: l2063, width: l2064});
    at = 2832; l2067 = field(rt, l15, "strokes");
    at = 2833; l2068 = rt.sequence_length(l2067);
    at = 2834; l2069 = K[624];
    at = 2835; l2070 = rt.lt(l2069, l2068);
    at = 2836; if (rt.truthy(l2070)) {
      at = 2837; l2071 = field(rt, l15, "strokes");
      at = 2838; l2072 = K[625];
      at = 2839; l2073 = rt.sequence_get(l2071, l2072);
    }
    at = 2840; if (rt.truthy(l2070)) {
      at = 2841; l2075 = field(rt, l2073, "points");
      at = 2842; l2074 = l2075;
    } else {
      at = 2843; l2076 = K[626];
      at = 2844; l2074 = l2076;
    }
    at = 2845; l2077 = rt.array_count(l2074);
    at = 2846; l2078 = K[627];
    at = 2847; l2079 = rt.gt(l2077, l2078);
    at = 2848; if (rt.truthy(l2079)) {
      at = 2849; if (rt.truthy(l2070)) {
        at = 2850; l2082 = field(rt, l2073, "points");
        at = 2851; l2081 = l2082;
      } else {
        at = 2852; l2083 = K[628];
        at = 2853; l2081 = l2083;
      }
      at = 2854; l2084 = K[629];
      at = 2855; l2085 = rt.array_nth(l2081, l2084);
      at = 2856; l2080 = l2085;
    } else {
      at = 2857; l2086 = K[630];
      at = 2858; l2080 = l2086;
    }
    at = 2859; if (rt.truthy(l2070)) {
      at = 2860; l2088 = field(rt, l2073, "width");
      at = 2861; l2087 = l2088;
    } else {
      at = 2862; l2089 = K[631];
      at = 2863; l2087 = l2089;
    }
    at = 2864; l2090 = K[632];
    at = 2865; l2091 = rt.int_div(l2087, l2090);
    at = 2866; l2092 = K[633];
    at = 2867; l2093 = rt.max(l2092, l2091);
    at = 2868; l2094 = K[634];
    at = 2869; l2095 = rt.gt(l2077, l2094);
    at = 2870; if (rt.truthy(l2095)) {
      at = 2871; if (rt.truthy(l2070)) {
        at = 2872; l2098 = field(rt, l2073, "color");
        at = 2873; l2097 = l2098;
      } else {
        at = 2874; l2099 = K[635];
        at = 2875; l2097 = l2099;
      }
      at = 2876; l2100 = K[636];
      at = 2877; l2101 = rt.sequence_get(l2100, l2097);
      at = 2878; l2096 = l2101;
    } else {
      at = 2879; l2102 = K[637];
      at = 2880; l2096 = l2102;
    }
    at = 2881; markSet(rt, false, "circle", {at: l2080, radius: l2093, fill: l2096});
    at = 2882; if (rt.truthy(l2070)) {
      at = 2883; l2104 = field(rt, l2073, "points");
      at = 2884; l2103 = l2104;
    } else {
      at = 2885; l2105 = K[638];
      at = 2886; l2103 = l2105;
    }
    at = 2887; l2106 = K[639];
    at = 2888; l2107 = rt.sub(l2077, l2106);
    at = 2889; l2108 = K[640];
    at = 2890; l2109 = rt.max(l2108, l2107);
    at = 2891; l2110 = K[641];
    at = 2892; l2111 = rt.array_slice(l2103, l2110, l2109);
    at = 2893; if (rt.truthy(l2070)) {
      at = 2894; l2113 = field(rt, l2073, "points");
      at = 2895; l2112 = l2113;
    } else {
      at = 2896; l2114 = K[642];
      at = 2897; l2112 = l2114;
    }
    at = 2898; l2115 = K[643];
    at = 2899; l2116 = rt.gt(l2077, l2115);
    at = 2900; if (rt.truthy(l2116)) {
      at = 2901; l2118 = K[644];
      at = 2902; l2117 = l2118;
    } else {
      at = 2903; l2119 = K[645];
      at = 2904; l2117 = l2119;
    }
    at = 2905; l2120 = K[646];
    at = 2906; l2121 = rt.sub(l2077, l2120);
    at = 2907; l2122 = K[647];
    at = 2908; l2123 = rt.max(l2122, l2121);
    at = 2909; l2124 = rt.array_slice(l2112, l2117, l2123);
    at = 2910; if (rt.truthy(l2070)) {
      at = 2911; l2126 = field(rt, l2073, "color");
      at = 2912; l2125 = l2126;
    } else {
      at = 2913; l2127 = K[648];
      at = 2914; l2125 = l2127;
    }
    at = 2915; l2128 = K[649];
    at = 2916; l2129 = rt.sequence_get(l2128, l2125);
    at = 2917; if (rt.truthy(l2070)) {
      at = 2918; l2131 = field(rt, l2073, "width");
      at = 2919; l2130 = l2131;
    } else {
      at = 2920; l2132 = K[650];
      at = 2921; l2130 = l2132;
    }
    at = 2922; markSet(rt, true, "line", {from: l2111, to: l2124, color: l2129, width: l2130});
    at = 2923; l2133 = field(rt, l15, "strokes");
    at = 2924; l2134 = rt.sequence_length(l2133);
    at = 2925; l2135 = K[651];
    at = 2926; l2136 = rt.lt(l2135, l2134);
    at = 2927; if (rt.truthy(l2136)) {
      at = 2928; l2137 = field(rt, l15, "strokes");
      at = 2929; l2138 = K[652];
      at = 2930; l2139 = rt.sequence_get(l2137, l2138);
    }
    at = 2931; if (rt.truthy(l2136)) {
      at = 2932; l2141 = field(rt, l2139, "points");
      at = 2933; l2140 = l2141;
    } else {
      at = 2934; l2142 = K[653];
      at = 2935; l2140 = l2142;
    }
    at = 2936; l2143 = rt.array_count(l2140);
    at = 2937; l2144 = K[654];
    at = 2938; l2145 = rt.gt(l2143, l2144);
    at = 2939; if (rt.truthy(l2145)) {
      at = 2940; if (rt.truthy(l2136)) {
        at = 2941; l2148 = field(rt, l2139, "points");
        at = 2942; l2147 = l2148;
      } else {
        at = 2943; l2149 = K[655];
        at = 2944; l2147 = l2149;
      }
      at = 2945; l2150 = K[656];
      at = 2946; l2151 = rt.array_nth(l2147, l2150);
      at = 2947; l2146 = l2151;
    } else {
      at = 2948; l2152 = K[657];
      at = 2949; l2146 = l2152;
    }
    at = 2950; if (rt.truthy(l2136)) {
      at = 2951; l2154 = field(rt, l2139, "width");
      at = 2952; l2153 = l2154;
    } else {
      at = 2953; l2155 = K[658];
      at = 2954; l2153 = l2155;
    }
    at = 2955; l2156 = K[659];
    at = 2956; l2157 = rt.int_div(l2153, l2156);
    at = 2957; l2158 = K[660];
    at = 2958; l2159 = rt.max(l2158, l2157);
    at = 2959; l2160 = K[661];
    at = 2960; l2161 = rt.gt(l2143, l2160);
    at = 2961; if (rt.truthy(l2161)) {
      at = 2962; if (rt.truthy(l2136)) {
        at = 2963; l2164 = field(rt, l2139, "color");
        at = 2964; l2163 = l2164;
      } else {
        at = 2965; l2165 = K[662];
        at = 2966; l2163 = l2165;
      }
      at = 2967; l2166 = K[663];
      at = 2968; l2167 = rt.sequence_get(l2166, l2163);
      at = 2969; l2162 = l2167;
    } else {
      at = 2970; l2168 = K[664];
      at = 2971; l2162 = l2168;
    }
    at = 2972; markSet(rt, false, "circle", {at: l2146, radius: l2159, fill: l2162});
    at = 2973; if (rt.truthy(l2136)) {
      at = 2974; l2170 = field(rt, l2139, "points");
      at = 2975; l2169 = l2170;
    } else {
      at = 2976; l2171 = K[665];
      at = 2977; l2169 = l2171;
    }
    at = 2978; l2172 = K[666];
    at = 2979; l2173 = rt.sub(l2143, l2172);
    at = 2980; l2174 = K[667];
    at = 2981; l2175 = rt.max(l2174, l2173);
    at = 2982; l2176 = K[668];
    at = 2983; l2177 = rt.array_slice(l2169, l2176, l2175);
    at = 2984; if (rt.truthy(l2136)) {
      at = 2985; l2179 = field(rt, l2139, "points");
      at = 2986; l2178 = l2179;
    } else {
      at = 2987; l2180 = K[669];
      at = 2988; l2178 = l2180;
    }
    at = 2989; l2181 = K[670];
    at = 2990; l2182 = rt.gt(l2143, l2181);
    at = 2991; if (rt.truthy(l2182)) {
      at = 2992; l2184 = K[671];
      at = 2993; l2183 = l2184;
    } else {
      at = 2994; l2185 = K[672];
      at = 2995; l2183 = l2185;
    }
    at = 2996; l2186 = K[673];
    at = 2997; l2187 = rt.sub(l2143, l2186);
    at = 2998; l2188 = K[674];
    at = 2999; l2189 = rt.max(l2188, l2187);
    at = 3000; l2190 = rt.array_slice(l2178, l2183, l2189);
    at = 3001; if (rt.truthy(l2136)) {
      at = 3002; l2192 = field(rt, l2139, "color");
      at = 3003; l2191 = l2192;
    } else {
      at = 3004; l2193 = K[675];
      at = 3005; l2191 = l2193;
    }
    at = 3006; l2194 = K[676];
    at = 3007; l2195 = rt.sequence_get(l2194, l2191);
    at = 3008; if (rt.truthy(l2136)) {
      at = 3009; l2197 = field(rt, l2139, "width");
      at = 3010; l2196 = l2197;
    } else {
      at = 3011; l2198 = K[677];
      at = 3012; l2196 = l2198;
    }
    at = 3013; markSet(rt, true, "line", {from: l2177, to: l2190, color: l2195, width: l2196});
    at = 3014; l2199 = field(rt, l15, "strokes");
    at = 3015; l2200 = rt.sequence_length(l2199);
    at = 3016; l2201 = K[678];
    at = 3017; l2202 = rt.lt(l2201, l2200);
    at = 3018; if (rt.truthy(l2202)) {
      at = 3019; l2203 = field(rt, l15, "strokes");
      at = 3020; l2204 = K[679];
      at = 3021; l2205 = rt.sequence_get(l2203, l2204);
    }
    at = 3022; if (rt.truthy(l2202)) {
      at = 3023; l2207 = field(rt, l2205, "points");
      at = 3024; l2206 = l2207;
    } else {
      at = 3025; l2208 = K[680];
      at = 3026; l2206 = l2208;
    }
    at = 3027; l2209 = rt.array_count(l2206);
    at = 3028; l2210 = K[681];
    at = 3029; l2211 = rt.gt(l2209, l2210);
    at = 3030; if (rt.truthy(l2211)) {
      at = 3031; if (rt.truthy(l2202)) {
        at = 3032; l2214 = field(rt, l2205, "points");
        at = 3033; l2213 = l2214;
      } else {
        at = 3034; l2215 = K[682];
        at = 3035; l2213 = l2215;
      }
      at = 3036; l2216 = K[683];
      at = 3037; l2217 = rt.array_nth(l2213, l2216);
      at = 3038; l2212 = l2217;
    } else {
      at = 3039; l2218 = K[684];
      at = 3040; l2212 = l2218;
    }
    at = 3041; if (rt.truthy(l2202)) {
      at = 3042; l2220 = field(rt, l2205, "width");
      at = 3043; l2219 = l2220;
    } else {
      at = 3044; l2221 = K[685];
      at = 3045; l2219 = l2221;
    }
    at = 3046; l2222 = K[686];
    at = 3047; l2223 = rt.int_div(l2219, l2222);
    at = 3048; l2224 = K[687];
    at = 3049; l2225 = rt.max(l2224, l2223);
    at = 3050; l2226 = K[688];
    at = 3051; l2227 = rt.gt(l2209, l2226);
    at = 3052; if (rt.truthy(l2227)) {
      at = 3053; if (rt.truthy(l2202)) {
        at = 3054; l2230 = field(rt, l2205, "color");
        at = 3055; l2229 = l2230;
      } else {
        at = 3056; l2231 = K[689];
        at = 3057; l2229 = l2231;
      }
      at = 3058; l2232 = K[690];
      at = 3059; l2233 = rt.sequence_get(l2232, l2229);
      at = 3060; l2228 = l2233;
    } else {
      at = 3061; l2234 = K[691];
      at = 3062; l2228 = l2234;
    }
    at = 3063; markSet(rt, false, "circle", {at: l2212, radius: l2225, fill: l2228});
    at = 3064; if (rt.truthy(l2202)) {
      at = 3065; l2236 = field(rt, l2205, "points");
      at = 3066; l2235 = l2236;
    } else {
      at = 3067; l2237 = K[692];
      at = 3068; l2235 = l2237;
    }
    at = 3069; l2238 = K[693];
    at = 3070; l2239 = rt.sub(l2209, l2238);
    at = 3071; l2240 = K[694];
    at = 3072; l2241 = rt.max(l2240, l2239);
    at = 3073; l2242 = K[695];
    at = 3074; l2243 = rt.array_slice(l2235, l2242, l2241);
    at = 3075; if (rt.truthy(l2202)) {
      at = 3076; l2245 = field(rt, l2205, "points");
      at = 3077; l2244 = l2245;
    } else {
      at = 3078; l2246 = K[696];
      at = 3079; l2244 = l2246;
    }
    at = 3080; l2247 = K[697];
    at = 3081; l2248 = rt.gt(l2209, l2247);
    at = 3082; if (rt.truthy(l2248)) {
      at = 3083; l2250 = K[698];
      at = 3084; l2249 = l2250;
    } else {
      at = 3085; l2251 = K[699];
      at = 3086; l2249 = l2251;
    }
    at = 3087; l2252 = K[700];
    at = 3088; l2253 = rt.sub(l2209, l2252);
    at = 3089; l2254 = K[701];
    at = 3090; l2255 = rt.max(l2254, l2253);
    at = 3091; l2256 = rt.array_slice(l2244, l2249, l2255);
    at = 3092; if (rt.truthy(l2202)) {
      at = 3093; l2258 = field(rt, l2205, "color");
      at = 3094; l2257 = l2258;
    } else {
      at = 3095; l2259 = K[702];
      at = 3096; l2257 = l2259;
    }
    at = 3097; l2260 = K[703];
    at = 3098; l2261 = rt.sequence_get(l2260, l2257);
    at = 3099; if (rt.truthy(l2202)) {
      at = 3100; l2263 = field(rt, l2205, "width");
      at = 3101; l2262 = l2263;
    } else {
      at = 3102; l2264 = K[704];
      at = 3103; l2262 = l2264;
    }
    at = 3104; markSet(rt, true, "line", {from: l2243, to: l2256, color: l2261, width: l2262});
    at = 3105; l2265 = field(rt, l15, "strokes");
    at = 3106; l2266 = rt.sequence_length(l2265);
    at = 3107; l2267 = K[705];
    at = 3108; l2268 = rt.lt(l2267, l2266);
    at = 3109; if (rt.truthy(l2268)) {
      at = 3110; l2269 = field(rt, l15, "strokes");
      at = 3111; l2270 = K[706];
      at = 3112; l2271 = rt.sequence_get(l2269, l2270);
    }
    at = 3113; if (rt.truthy(l2268)) {
      at = 3114; l2273 = field(rt, l2271, "points");
      at = 3115; l2272 = l2273;
    } else {
      at = 3116; l2274 = K[707];
      at = 3117; l2272 = l2274;
    }
    at = 3118; l2275 = rt.array_count(l2272);
    at = 3119; l2276 = K[708];
    at = 3120; l2277 = rt.gt(l2275, l2276);
    at = 3121; if (rt.truthy(l2277)) {
      at = 3122; if (rt.truthy(l2268)) {
        at = 3123; l2280 = field(rt, l2271, "points");
        at = 3124; l2279 = l2280;
      } else {
        at = 3125; l2281 = K[709];
        at = 3126; l2279 = l2281;
      }
      at = 3127; l2282 = K[710];
      at = 3128; l2283 = rt.array_nth(l2279, l2282);
      at = 3129; l2278 = l2283;
    } else {
      at = 3130; l2284 = K[711];
      at = 3131; l2278 = l2284;
    }
    at = 3132; if (rt.truthy(l2268)) {
      at = 3133; l2286 = field(rt, l2271, "width");
      at = 3134; l2285 = l2286;
    } else {
      at = 3135; l2287 = K[712];
      at = 3136; l2285 = l2287;
    }
    at = 3137; l2288 = K[713];
    at = 3138; l2289 = rt.int_div(l2285, l2288);
    at = 3139; l2290 = K[714];
    at = 3140; l2291 = rt.max(l2290, l2289);
    at = 3141; l2292 = K[715];
    at = 3142; l2293 = rt.gt(l2275, l2292);
    at = 3143; if (rt.truthy(l2293)) {
      at = 3144; if (rt.truthy(l2268)) {
        at = 3145; l2296 = field(rt, l2271, "color");
        at = 3146; l2295 = l2296;
      } else {
        at = 3147; l2297 = K[716];
        at = 3148; l2295 = l2297;
      }
      at = 3149; l2298 = K[717];
      at = 3150; l2299 = rt.sequence_get(l2298, l2295);
      at = 3151; l2294 = l2299;
    } else {
      at = 3152; l2300 = K[718];
      at = 3153; l2294 = l2300;
    }
    at = 3154; markSet(rt, false, "circle", {at: l2278, radius: l2291, fill: l2294});
    at = 3155; if (rt.truthy(l2268)) {
      at = 3156; l2302 = field(rt, l2271, "points");
      at = 3157; l2301 = l2302;
    } else {
      at = 3158; l2303 = K[719];
      at = 3159; l2301 = l2303;
    }
    at = 3160; l2304 = K[720];
    at = 3161; l2305 = rt.sub(l2275, l2304);
    at = 3162; l2306 = K[721];
    at = 3163; l2307 = rt.max(l2306, l2305);
    at = 3164; l2308 = K[722];
    at = 3165; l2309 = rt.array_slice(l2301, l2308, l2307);
    at = 3166; if (rt.truthy(l2268)) {
      at = 3167; l2311 = field(rt, l2271, "points");
      at = 3168; l2310 = l2311;
    } else {
      at = 3169; l2312 = K[723];
      at = 3170; l2310 = l2312;
    }
    at = 3171; l2313 = K[724];
    at = 3172; l2314 = rt.gt(l2275, l2313);
    at = 3173; if (rt.truthy(l2314)) {
      at = 3174; l2316 = K[725];
      at = 3175; l2315 = l2316;
    } else {
      at = 3176; l2317 = K[726];
      at = 3177; l2315 = l2317;
    }
    at = 3178; l2318 = K[727];
    at = 3179; l2319 = rt.sub(l2275, l2318);
    at = 3180; l2320 = K[728];
    at = 3181; l2321 = rt.max(l2320, l2319);
    at = 3182; l2322 = rt.array_slice(l2310, l2315, l2321);
    at = 3183; if (rt.truthy(l2268)) {
      at = 3184; l2324 = field(rt, l2271, "color");
      at = 3185; l2323 = l2324;
    } else {
      at = 3186; l2325 = K[729];
      at = 3187; l2323 = l2325;
    }
    at = 3188; l2326 = K[730];
    at = 3189; l2327 = rt.sequence_get(l2326, l2323);
    at = 3190; if (rt.truthy(l2268)) {
      at = 3191; l2329 = field(rt, l2271, "width");
      at = 3192; l2328 = l2329;
    } else {
      at = 3193; l2330 = K[731];
      at = 3194; l2328 = l2330;
    }
    at = 3195; markSet(rt, true, "line", {from: l2309, to: l2322, color: l2327, width: l2328});
    at = 3196; l2331 = field(rt, l15, "strokes");
    at = 3197; l2332 = rt.sequence_length(l2331);
    at = 3198; l2333 = K[732];
    at = 3199; l2334 = rt.lt(l2333, l2332);
    at = 3200; if (rt.truthy(l2334)) {
      at = 3201; l2335 = field(rt, l15, "strokes");
      at = 3202; l2336 = K[733];
      at = 3203; l2337 = rt.sequence_get(l2335, l2336);
    }
    at = 3204; if (rt.truthy(l2334)) {
      at = 3205; l2339 = field(rt, l2337, "points");
      at = 3206; l2338 = l2339;
    } else {
      at = 3207; l2340 = K[734];
      at = 3208; l2338 = l2340;
    }
    at = 3209; l2341 = rt.array_count(l2338);
    at = 3210; l2342 = K[735];
    at = 3211; l2343 = rt.gt(l2341, l2342);
    at = 3212; if (rt.truthy(l2343)) {
      at = 3213; if (rt.truthy(l2334)) {
        at = 3214; l2346 = field(rt, l2337, "points");
        at = 3215; l2345 = l2346;
      } else {
        at = 3216; l2347 = K[736];
        at = 3217; l2345 = l2347;
      }
      at = 3218; l2348 = K[737];
      at = 3219; l2349 = rt.array_nth(l2345, l2348);
      at = 3220; l2344 = l2349;
    } else {
      at = 3221; l2350 = K[738];
      at = 3222; l2344 = l2350;
    }
    at = 3223; if (rt.truthy(l2334)) {
      at = 3224; l2352 = field(rt, l2337, "width");
      at = 3225; l2351 = l2352;
    } else {
      at = 3226; l2353 = K[739];
      at = 3227; l2351 = l2353;
    }
    at = 3228; l2354 = K[740];
    at = 3229; l2355 = rt.int_div(l2351, l2354);
    at = 3230; l2356 = K[741];
    at = 3231; l2357 = rt.max(l2356, l2355);
    at = 3232; l2358 = K[742];
    at = 3233; l2359 = rt.gt(l2341, l2358);
    at = 3234; if (rt.truthy(l2359)) {
      at = 3235; if (rt.truthy(l2334)) {
        at = 3236; l2362 = field(rt, l2337, "color");
        at = 3237; l2361 = l2362;
      } else {
        at = 3238; l2363 = K[743];
        at = 3239; l2361 = l2363;
      }
      at = 3240; l2364 = K[744];
      at = 3241; l2365 = rt.sequence_get(l2364, l2361);
      at = 3242; l2360 = l2365;
    } else {
      at = 3243; l2366 = K[745];
      at = 3244; l2360 = l2366;
    }
    at = 3245; markSet(rt, false, "circle", {at: l2344, radius: l2357, fill: l2360});
    at = 3246; if (rt.truthy(l2334)) {
      at = 3247; l2368 = field(rt, l2337, "points");
      at = 3248; l2367 = l2368;
    } else {
      at = 3249; l2369 = K[746];
      at = 3250; l2367 = l2369;
    }
    at = 3251; l2370 = K[747];
    at = 3252; l2371 = rt.sub(l2341, l2370);
    at = 3253; l2372 = K[748];
    at = 3254; l2373 = rt.max(l2372, l2371);
    at = 3255; l2374 = K[749];
    at = 3256; l2375 = rt.array_slice(l2367, l2374, l2373);
    at = 3257; if (rt.truthy(l2334)) {
      at = 3258; l2377 = field(rt, l2337, "points");
      at = 3259; l2376 = l2377;
    } else {
      at = 3260; l2378 = K[750];
      at = 3261; l2376 = l2378;
    }
    at = 3262; l2379 = K[751];
    at = 3263; l2380 = rt.gt(l2341, l2379);
    at = 3264; if (rt.truthy(l2380)) {
      at = 3265; l2382 = K[752];
      at = 3266; l2381 = l2382;
    } else {
      at = 3267; l2383 = K[753];
      at = 3268; l2381 = l2383;
    }
    at = 3269; l2384 = K[754];
    at = 3270; l2385 = rt.sub(l2341, l2384);
    at = 3271; l2386 = K[755];
    at = 3272; l2387 = rt.max(l2386, l2385);
    at = 3273; l2388 = rt.array_slice(l2376, l2381, l2387);
    at = 3274; if (rt.truthy(l2334)) {
      at = 3275; l2390 = field(rt, l2337, "color");
      at = 3276; l2389 = l2390;
    } else {
      at = 3277; l2391 = K[756];
      at = 3278; l2389 = l2391;
    }
    at = 3279; l2392 = K[757];
    at = 3280; l2393 = rt.sequence_get(l2392, l2389);
    at = 3281; if (rt.truthy(l2334)) {
      at = 3282; l2395 = field(rt, l2337, "width");
      at = 3283; l2394 = l2395;
    } else {
      at = 3284; l2396 = K[758];
      at = 3285; l2394 = l2396;
    }
    at = 3286; markSet(rt, true, "line", {from: l2375, to: l2388, color: l2393, width: l2394});
    at = 3287; l2397 = field(rt, l15, "strokes");
    at = 3288; l2398 = rt.sequence_length(l2397);
    at = 3289; l2399 = K[759];
    at = 3290; l2400 = rt.lt(l2399, l2398);
    at = 3291; if (rt.truthy(l2400)) {
      at = 3292; l2401 = field(rt, l15, "strokes");
      at = 3293; l2402 = K[760];
      at = 3294; l2403 = rt.sequence_get(l2401, l2402);
    }
    at = 3295; if (rt.truthy(l2400)) {
      at = 3296; l2405 = field(rt, l2403, "points");
      at = 3297; l2404 = l2405;
    } else {
      at = 3298; l2406 = K[761];
      at = 3299; l2404 = l2406;
    }
    at = 3300; l2407 = rt.array_count(l2404);
    at = 3301; l2408 = K[762];
    at = 3302; l2409 = rt.gt(l2407, l2408);
    at = 3303; if (rt.truthy(l2409)) {
      at = 3304; if (rt.truthy(l2400)) {
        at = 3305; l2412 = field(rt, l2403, "points");
        at = 3306; l2411 = l2412;
      } else {
        at = 3307; l2413 = K[763];
        at = 3308; l2411 = l2413;
      }
      at = 3309; l2414 = K[764];
      at = 3310; l2415 = rt.array_nth(l2411, l2414);
      at = 3311; l2410 = l2415;
    } else {
      at = 3312; l2416 = K[765];
      at = 3313; l2410 = l2416;
    }
    at = 3314; if (rt.truthy(l2400)) {
      at = 3315; l2418 = field(rt, l2403, "width");
      at = 3316; l2417 = l2418;
    } else {
      at = 3317; l2419 = K[766];
      at = 3318; l2417 = l2419;
    }
    at = 3319; l2420 = K[767];
    at = 3320; l2421 = rt.int_div(l2417, l2420);
    at = 3321; l2422 = K[768];
    at = 3322; l2423 = rt.max(l2422, l2421);
    at = 3323; l2424 = K[769];
    at = 3324; l2425 = rt.gt(l2407, l2424);
    at = 3325; if (rt.truthy(l2425)) {
      at = 3326; if (rt.truthy(l2400)) {
        at = 3327; l2428 = field(rt, l2403, "color");
        at = 3328; l2427 = l2428;
      } else {
        at = 3329; l2429 = K[770];
        at = 3330; l2427 = l2429;
      }
      at = 3331; l2430 = K[771];
      at = 3332; l2431 = rt.sequence_get(l2430, l2427);
      at = 3333; l2426 = l2431;
    } else {
      at = 3334; l2432 = K[772];
      at = 3335; l2426 = l2432;
    }
    at = 3336; markSet(rt, false, "circle", {at: l2410, radius: l2423, fill: l2426});
    at = 3337; if (rt.truthy(l2400)) {
      at = 3338; l2434 = field(rt, l2403, "points");
      at = 3339; l2433 = l2434;
    } else {
      at = 3340; l2435 = K[773];
      at = 3341; l2433 = l2435;
    }
    at = 3342; l2436 = K[774];
    at = 3343; l2437 = rt.sub(l2407, l2436);
    at = 3344; l2438 = K[775];
    at = 3345; l2439 = rt.max(l2438, l2437);
    at = 3346; l2440 = K[776];
    at = 3347; l2441 = rt.array_slice(l2433, l2440, l2439);
    at = 3348; if (rt.truthy(l2400)) {
      at = 3349; l2443 = field(rt, l2403, "points");
      at = 3350; l2442 = l2443;
    } else {
      at = 3351; l2444 = K[777];
      at = 3352; l2442 = l2444;
    }
    at = 3353; l2445 = K[778];
    at = 3354; l2446 = rt.gt(l2407, l2445);
    at = 3355; if (rt.truthy(l2446)) {
      at = 3356; l2448 = K[779];
      at = 3357; l2447 = l2448;
    } else {
      at = 3358; l2449 = K[780];
      at = 3359; l2447 = l2449;
    }
    at = 3360; l2450 = K[781];
    at = 3361; l2451 = rt.sub(l2407, l2450);
    at = 3362; l2452 = K[782];
    at = 3363; l2453 = rt.max(l2452, l2451);
    at = 3364; l2454 = rt.array_slice(l2442, l2447, l2453);
    at = 3365; if (rt.truthy(l2400)) {
      at = 3366; l2456 = field(rt, l2403, "color");
      at = 3367; l2455 = l2456;
    } else {
      at = 3368; l2457 = K[783];
      at = 3369; l2455 = l2457;
    }
    at = 3370; l2458 = K[784];
    at = 3371; l2459 = rt.sequence_get(l2458, l2455);
    at = 3372; if (rt.truthy(l2400)) {
      at = 3373; l2461 = field(rt, l2403, "width");
      at = 3374; l2460 = l2461;
    } else {
      at = 3375; l2462 = K[785];
      at = 3376; l2460 = l2462;
    }
    at = 3377; markSet(rt, true, "line", {from: l2441, to: l2454, color: l2459, width: l2460});
    at = 3378; l2463 = field(rt, l15, "strokes");
    at = 3379; l2464 = rt.sequence_length(l2463);
    at = 3380; l2465 = K[786];
    at = 3381; l2466 = rt.lt(l2465, l2464);
    at = 3382; if (rt.truthy(l2466)) {
      at = 3383; l2467 = field(rt, l15, "strokes");
      at = 3384; l2468 = K[787];
      at = 3385; l2469 = rt.sequence_get(l2467, l2468);
    }
    at = 3386; if (rt.truthy(l2466)) {
      at = 3387; l2471 = field(rt, l2469, "points");
      at = 3388; l2470 = l2471;
    } else {
      at = 3389; l2472 = K[788];
      at = 3390; l2470 = l2472;
    }
    at = 3391; l2473 = rt.array_count(l2470);
    at = 3392; l2474 = K[789];
    at = 3393; l2475 = rt.gt(l2473, l2474);
    at = 3394; if (rt.truthy(l2475)) {
      at = 3395; if (rt.truthy(l2466)) {
        at = 3396; l2478 = field(rt, l2469, "points");
        at = 3397; l2477 = l2478;
      } else {
        at = 3398; l2479 = K[790];
        at = 3399; l2477 = l2479;
      }
      at = 3400; l2480 = K[791];
      at = 3401; l2481 = rt.array_nth(l2477, l2480);
      at = 3402; l2476 = l2481;
    } else {
      at = 3403; l2482 = K[792];
      at = 3404; l2476 = l2482;
    }
    at = 3405; if (rt.truthy(l2466)) {
      at = 3406; l2484 = field(rt, l2469, "width");
      at = 3407; l2483 = l2484;
    } else {
      at = 3408; l2485 = K[793];
      at = 3409; l2483 = l2485;
    }
    at = 3410; l2486 = K[794];
    at = 3411; l2487 = rt.int_div(l2483, l2486);
    at = 3412; l2488 = K[795];
    at = 3413; l2489 = rt.max(l2488, l2487);
    at = 3414; l2490 = K[796];
    at = 3415; l2491 = rt.gt(l2473, l2490);
    at = 3416; if (rt.truthy(l2491)) {
      at = 3417; if (rt.truthy(l2466)) {
        at = 3418; l2494 = field(rt, l2469, "color");
        at = 3419; l2493 = l2494;
      } else {
        at = 3420; l2495 = K[797];
        at = 3421; l2493 = l2495;
      }
      at = 3422; l2496 = K[798];
      at = 3423; l2497 = rt.sequence_get(l2496, l2493);
      at = 3424; l2492 = l2497;
    } else {
      at = 3425; l2498 = K[799];
      at = 3426; l2492 = l2498;
    }
    at = 3427; markSet(rt, false, "circle", {at: l2476, radius: l2489, fill: l2492});
    at = 3428; if (rt.truthy(l2466)) {
      at = 3429; l2500 = field(rt, l2469, "points");
      at = 3430; l2499 = l2500;
    } else {
      at = 3431; l2501 = K[800];
      at = 3432; l2499 = l2501;
    }
    at = 3433; l2502 = K[801];
    at = 3434; l2503 = rt.sub(l2473, l2502);
    at = 3435; l2504 = K[802];
    at = 3436; l2505 = rt.max(l2504, l2503);
    at = 3437; l2506 = K[803];
    at = 3438; l2507 = rt.array_slice(l2499, l2506, l2505);
    at = 3439; if (rt.truthy(l2466)) {
      at = 3440; l2509 = field(rt, l2469, "points");
      at = 3441; l2508 = l2509;
    } else {
      at = 3442; l2510 = K[804];
      at = 3443; l2508 = l2510;
    }
    at = 3444; l2511 = K[805];
    at = 3445; l2512 = rt.gt(l2473, l2511);
    at = 3446; if (rt.truthy(l2512)) {
      at = 3447; l2514 = K[806];
      at = 3448; l2513 = l2514;
    } else {
      at = 3449; l2515 = K[807];
      at = 3450; l2513 = l2515;
    }
    at = 3451; l2516 = K[808];
    at = 3452; l2517 = rt.sub(l2473, l2516);
    at = 3453; l2518 = K[809];
    at = 3454; l2519 = rt.max(l2518, l2517);
    at = 3455; l2520 = rt.array_slice(l2508, l2513, l2519);
    at = 3456; if (rt.truthy(l2466)) {
      at = 3457; l2522 = field(rt, l2469, "color");
      at = 3458; l2521 = l2522;
    } else {
      at = 3459; l2523 = K[810];
      at = 3460; l2521 = l2523;
    }
    at = 3461; l2524 = K[811];
    at = 3462; l2525 = rt.sequence_get(l2524, l2521);
    at = 3463; if (rt.truthy(l2466)) {
      at = 3464; l2527 = field(rt, l2469, "width");
      at = 3465; l2526 = l2527;
    } else {
      at = 3466; l2528 = K[812];
      at = 3467; l2526 = l2528;
    }
    at = 3468; markSet(rt, true, "line", {from: l2507, to: l2520, color: l2525, width: l2526});
    at = 3469; l2529 = field(rt, l15, "strokes");
    at = 3470; l2530 = rt.sequence_length(l2529);
    at = 3471; l2531 = K[813];
    at = 3472; l2532 = rt.lt(l2531, l2530);
    at = 3473; if (rt.truthy(l2532)) {
      at = 3474; l2533 = field(rt, l15, "strokes");
      at = 3475; l2534 = K[814];
      at = 3476; l2535 = rt.sequence_get(l2533, l2534);
    }
    at = 3477; if (rt.truthy(l2532)) {
      at = 3478; l2537 = field(rt, l2535, "points");
      at = 3479; l2536 = l2537;
    } else {
      at = 3480; l2538 = K[815];
      at = 3481; l2536 = l2538;
    }
    at = 3482; l2539 = rt.array_count(l2536);
    at = 3483; l2540 = K[816];
    at = 3484; l2541 = rt.gt(l2539, l2540);
    at = 3485; if (rt.truthy(l2541)) {
      at = 3486; if (rt.truthy(l2532)) {
        at = 3487; l2544 = field(rt, l2535, "points");
        at = 3488; l2543 = l2544;
      } else {
        at = 3489; l2545 = K[817];
        at = 3490; l2543 = l2545;
      }
      at = 3491; l2546 = K[818];
      at = 3492; l2547 = rt.array_nth(l2543, l2546);
      at = 3493; l2542 = l2547;
    } else {
      at = 3494; l2548 = K[819];
      at = 3495; l2542 = l2548;
    }
    at = 3496; if (rt.truthy(l2532)) {
      at = 3497; l2550 = field(rt, l2535, "width");
      at = 3498; l2549 = l2550;
    } else {
      at = 3499; l2551 = K[820];
      at = 3500; l2549 = l2551;
    }
    at = 3501; l2552 = K[821];
    at = 3502; l2553 = rt.int_div(l2549, l2552);
    at = 3503; l2554 = K[822];
    at = 3504; l2555 = rt.max(l2554, l2553);
    at = 3505; l2556 = K[823];
    at = 3506; l2557 = rt.gt(l2539, l2556);
    at = 3507; if (rt.truthy(l2557)) {
      at = 3508; if (rt.truthy(l2532)) {
        at = 3509; l2560 = field(rt, l2535, "color");
        at = 3510; l2559 = l2560;
      } else {
        at = 3511; l2561 = K[824];
        at = 3512; l2559 = l2561;
      }
      at = 3513; l2562 = K[825];
      at = 3514; l2563 = rt.sequence_get(l2562, l2559);
      at = 3515; l2558 = l2563;
    } else {
      at = 3516; l2564 = K[826];
      at = 3517; l2558 = l2564;
    }
    at = 3518; markSet(rt, false, "circle", {at: l2542, radius: l2555, fill: l2558});
    at = 3519; if (rt.truthy(l2532)) {
      at = 3520; l2566 = field(rt, l2535, "points");
      at = 3521; l2565 = l2566;
    } else {
      at = 3522; l2567 = K[827];
      at = 3523; l2565 = l2567;
    }
    at = 3524; l2568 = K[828];
    at = 3525; l2569 = rt.sub(l2539, l2568);
    at = 3526; l2570 = K[829];
    at = 3527; l2571 = rt.max(l2570, l2569);
    at = 3528; l2572 = K[830];
    at = 3529; l2573 = rt.array_slice(l2565, l2572, l2571);
    at = 3530; if (rt.truthy(l2532)) {
      at = 3531; l2575 = field(rt, l2535, "points");
      at = 3532; l2574 = l2575;
    } else {
      at = 3533; l2576 = K[831];
      at = 3534; l2574 = l2576;
    }
    at = 3535; l2577 = K[832];
    at = 3536; l2578 = rt.gt(l2539, l2577);
    at = 3537; if (rt.truthy(l2578)) {
      at = 3538; l2580 = K[833];
      at = 3539; l2579 = l2580;
    } else {
      at = 3540; l2581 = K[834];
      at = 3541; l2579 = l2581;
    }
    at = 3542; l2582 = K[835];
    at = 3543; l2583 = rt.sub(l2539, l2582);
    at = 3544; l2584 = K[836];
    at = 3545; l2585 = rt.max(l2584, l2583);
    at = 3546; l2586 = rt.array_slice(l2574, l2579, l2585);
    at = 3547; if (rt.truthy(l2532)) {
      at = 3548; l2588 = field(rt, l2535, "color");
      at = 3549; l2587 = l2588;
    } else {
      at = 3550; l2589 = K[837];
      at = 3551; l2587 = l2589;
    }
    at = 3552; l2590 = K[838];
    at = 3553; l2591 = rt.sequence_get(l2590, l2587);
    at = 3554; if (rt.truthy(l2532)) {
      at = 3555; l2593 = field(rt, l2535, "width");
      at = 3556; l2592 = l2593;
    } else {
      at = 3557; l2594 = K[839];
      at = 3558; l2592 = l2594;
    }
    at = 3559; markSet(rt, true, "line", {from: l2573, to: l2586, color: l2591, width: l2592});
    at = 3560; l2595 = field(rt, l15, "strokes");
    at = 3561; l2596 = rt.sequence_length(l2595);
    at = 3562; l2597 = K[840];
    at = 3563; l2598 = rt.lt(l2597, l2596);
    at = 3564; if (rt.truthy(l2598)) {
      at = 3565; l2599 = field(rt, l15, "strokes");
      at = 3566; l2600 = K[841];
      at = 3567; l2601 = rt.sequence_get(l2599, l2600);
    }
    at = 3568; if (rt.truthy(l2598)) {
      at = 3569; l2603 = field(rt, l2601, "points");
      at = 3570; l2602 = l2603;
    } else {
      at = 3571; l2604 = K[842];
      at = 3572; l2602 = l2604;
    }
    at = 3573; l2605 = rt.array_count(l2602);
    at = 3574; l2606 = K[843];
    at = 3575; l2607 = rt.gt(l2605, l2606);
    at = 3576; if (rt.truthy(l2607)) {
      at = 3577; if (rt.truthy(l2598)) {
        at = 3578; l2610 = field(rt, l2601, "points");
        at = 3579; l2609 = l2610;
      } else {
        at = 3580; l2611 = K[844];
        at = 3581; l2609 = l2611;
      }
      at = 3582; l2612 = K[845];
      at = 3583; l2613 = rt.array_nth(l2609, l2612);
      at = 3584; l2608 = l2613;
    } else {
      at = 3585; l2614 = K[846];
      at = 3586; l2608 = l2614;
    }
    at = 3587; if (rt.truthy(l2598)) {
      at = 3588; l2616 = field(rt, l2601, "width");
      at = 3589; l2615 = l2616;
    } else {
      at = 3590; l2617 = K[847];
      at = 3591; l2615 = l2617;
    }
    at = 3592; l2618 = K[848];
    at = 3593; l2619 = rt.int_div(l2615, l2618);
    at = 3594; l2620 = K[849];
    at = 3595; l2621 = rt.max(l2620, l2619);
    at = 3596; l2622 = K[850];
    at = 3597; l2623 = rt.gt(l2605, l2622);
    at = 3598; if (rt.truthy(l2623)) {
      at = 3599; if (rt.truthy(l2598)) {
        at = 3600; l2626 = field(rt, l2601, "color");
        at = 3601; l2625 = l2626;
      } else {
        at = 3602; l2627 = K[851];
        at = 3603; l2625 = l2627;
      }
      at = 3604; l2628 = K[852];
      at = 3605; l2629 = rt.sequence_get(l2628, l2625);
      at = 3606; l2624 = l2629;
    } else {
      at = 3607; l2630 = K[853];
      at = 3608; l2624 = l2630;
    }
    at = 3609; markSet(rt, false, "circle", {at: l2608, radius: l2621, fill: l2624});
    at = 3610; if (rt.truthy(l2598)) {
      at = 3611; l2632 = field(rt, l2601, "points");
      at = 3612; l2631 = l2632;
    } else {
      at = 3613; l2633 = K[854];
      at = 3614; l2631 = l2633;
    }
    at = 3615; l2634 = K[855];
    at = 3616; l2635 = rt.sub(l2605, l2634);
    at = 3617; l2636 = K[856];
    at = 3618; l2637 = rt.max(l2636, l2635);
    at = 3619; l2638 = K[857];
    at = 3620; l2639 = rt.array_slice(l2631, l2638, l2637);
    at = 3621; if (rt.truthy(l2598)) {
      at = 3622; l2641 = field(rt, l2601, "points");
      at = 3623; l2640 = l2641;
    } else {
      at = 3624; l2642 = K[858];
      at = 3625; l2640 = l2642;
    }
    at = 3626; l2643 = K[859];
    at = 3627; l2644 = rt.gt(l2605, l2643);
    at = 3628; if (rt.truthy(l2644)) {
      at = 3629; l2646 = K[860];
      at = 3630; l2645 = l2646;
    } else {
      at = 3631; l2647 = K[861];
      at = 3632; l2645 = l2647;
    }
    at = 3633; l2648 = K[862];
    at = 3634; l2649 = rt.sub(l2605, l2648);
    at = 3635; l2650 = K[863];
    at = 3636; l2651 = rt.max(l2650, l2649);
    at = 3637; l2652 = rt.array_slice(l2640, l2645, l2651);
    at = 3638; if (rt.truthy(l2598)) {
      at = 3639; l2654 = field(rt, l2601, "color");
      at = 3640; l2653 = l2654;
    } else {
      at = 3641; l2655 = K[864];
      at = 3642; l2653 = l2655;
    }
    at = 3643; l2656 = K[865];
    at = 3644; l2657 = rt.sequence_get(l2656, l2653);
    at = 3645; if (rt.truthy(l2598)) {
      at = 3646; l2659 = field(rt, l2601, "width");
      at = 3647; l2658 = l2659;
    } else {
      at = 3648; l2660 = K[866];
      at = 3649; l2658 = l2660;
    }
    at = 3650; markSet(rt, true, "line", {from: l2639, to: l2652, color: l2657, width: l2658});
    at = 3651; l2661 = field(rt, l15, "strokes");
    at = 3652; l2662 = rt.sequence_length(l2661);
    at = 3653; l2663 = K[867];
    at = 3654; l2664 = rt.lt(l2663, l2662);
    at = 3655; if (rt.truthy(l2664)) {
      at = 3656; l2665 = field(rt, l15, "strokes");
      at = 3657; l2666 = K[868];
      at = 3658; l2667 = rt.sequence_get(l2665, l2666);
    }
    at = 3659; if (rt.truthy(l2664)) {
      at = 3660; l2669 = field(rt, l2667, "points");
      at = 3661; l2668 = l2669;
    } else {
      at = 3662; l2670 = K[869];
      at = 3663; l2668 = l2670;
    }
    at = 3664; l2671 = rt.array_count(l2668);
    at = 3665; l2672 = K[870];
    at = 3666; l2673 = rt.gt(l2671, l2672);
    at = 3667; if (rt.truthy(l2673)) {
      at = 3668; if (rt.truthy(l2664)) {
        at = 3669; l2676 = field(rt, l2667, "points");
        at = 3670; l2675 = l2676;
      } else {
        at = 3671; l2677 = K[871];
        at = 3672; l2675 = l2677;
      }
      at = 3673; l2678 = K[872];
      at = 3674; l2679 = rt.array_nth(l2675, l2678);
      at = 3675; l2674 = l2679;
    } else {
      at = 3676; l2680 = K[873];
      at = 3677; l2674 = l2680;
    }
    at = 3678; if (rt.truthy(l2664)) {
      at = 3679; l2682 = field(rt, l2667, "width");
      at = 3680; l2681 = l2682;
    } else {
      at = 3681; l2683 = K[874];
      at = 3682; l2681 = l2683;
    }
    at = 3683; l2684 = K[875];
    at = 3684; l2685 = rt.int_div(l2681, l2684);
    at = 3685; l2686 = K[876];
    at = 3686; l2687 = rt.max(l2686, l2685);
    at = 3687; l2688 = K[877];
    at = 3688; l2689 = rt.gt(l2671, l2688);
    at = 3689; if (rt.truthy(l2689)) {
      at = 3690; if (rt.truthy(l2664)) {
        at = 3691; l2692 = field(rt, l2667, "color");
        at = 3692; l2691 = l2692;
      } else {
        at = 3693; l2693 = K[878];
        at = 3694; l2691 = l2693;
      }
      at = 3695; l2694 = K[879];
      at = 3696; l2695 = rt.sequence_get(l2694, l2691);
      at = 3697; l2690 = l2695;
    } else {
      at = 3698; l2696 = K[880];
      at = 3699; l2690 = l2696;
    }
    at = 3700; markSet(rt, false, "circle", {at: l2674, radius: l2687, fill: l2690});
    at = 3701; if (rt.truthy(l2664)) {
      at = 3702; l2698 = field(rt, l2667, "points");
      at = 3703; l2697 = l2698;
    } else {
      at = 3704; l2699 = K[881];
      at = 3705; l2697 = l2699;
    }
    at = 3706; l2700 = K[882];
    at = 3707; l2701 = rt.sub(l2671, l2700);
    at = 3708; l2702 = K[883];
    at = 3709; l2703 = rt.max(l2702, l2701);
    at = 3710; l2704 = K[884];
    at = 3711; l2705 = rt.array_slice(l2697, l2704, l2703);
    at = 3712; if (rt.truthy(l2664)) {
      at = 3713; l2707 = field(rt, l2667, "points");
      at = 3714; l2706 = l2707;
    } else {
      at = 3715; l2708 = K[885];
      at = 3716; l2706 = l2708;
    }
    at = 3717; l2709 = K[886];
    at = 3718; l2710 = rt.gt(l2671, l2709);
    at = 3719; if (rt.truthy(l2710)) {
      at = 3720; l2712 = K[887];
      at = 3721; l2711 = l2712;
    } else {
      at = 3722; l2713 = K[888];
      at = 3723; l2711 = l2713;
    }
    at = 3724; l2714 = K[889];
    at = 3725; l2715 = rt.sub(l2671, l2714);
    at = 3726; l2716 = K[890];
    at = 3727; l2717 = rt.max(l2716, l2715);
    at = 3728; l2718 = rt.array_slice(l2706, l2711, l2717);
    at = 3729; if (rt.truthy(l2664)) {
      at = 3730; l2720 = field(rt, l2667, "color");
      at = 3731; l2719 = l2720;
    } else {
      at = 3732; l2721 = K[891];
      at = 3733; l2719 = l2721;
    }
    at = 3734; l2722 = K[892];
    at = 3735; l2723 = rt.sequence_get(l2722, l2719);
    at = 3736; if (rt.truthy(l2664)) {
      at = 3737; l2725 = field(rt, l2667, "width");
      at = 3738; l2724 = l2725;
    } else {
      at = 3739; l2726 = K[893];
      at = 3740; l2724 = l2726;
    }
    at = 3741; markSet(rt, true, "line", {from: l2705, to: l2718, color: l2723, width: l2724});
    at = 3742; l2727 = field(rt, l15, "strokes");
    at = 3743; l2728 = rt.sequence_length(l2727);
    at = 3744; l2729 = K[894];
    at = 3745; l2730 = rt.lt(l2729, l2728);
    at = 3746; if (rt.truthy(l2730)) {
      at = 3747; l2731 = field(rt, l15, "strokes");
      at = 3748; l2732 = K[895];
      at = 3749; l2733 = rt.sequence_get(l2731, l2732);
    }
    at = 3750; if (rt.truthy(l2730)) {
      at = 3751; l2735 = field(rt, l2733, "points");
      at = 3752; l2734 = l2735;
    } else {
      at = 3753; l2736 = K[896];
      at = 3754; l2734 = l2736;
    }
    at = 3755; l2737 = rt.array_count(l2734);
    at = 3756; l2738 = K[897];
    at = 3757; l2739 = rt.gt(l2737, l2738);
    at = 3758; if (rt.truthy(l2739)) {
      at = 3759; if (rt.truthy(l2730)) {
        at = 3760; l2742 = field(rt, l2733, "points");
        at = 3761; l2741 = l2742;
      } else {
        at = 3762; l2743 = K[898];
        at = 3763; l2741 = l2743;
      }
      at = 3764; l2744 = K[899];
      at = 3765; l2745 = rt.array_nth(l2741, l2744);
      at = 3766; l2740 = l2745;
    } else {
      at = 3767; l2746 = K[900];
      at = 3768; l2740 = l2746;
    }
    at = 3769; if (rt.truthy(l2730)) {
      at = 3770; l2748 = field(rt, l2733, "width");
      at = 3771; l2747 = l2748;
    } else {
      at = 3772; l2749 = K[901];
      at = 3773; l2747 = l2749;
    }
    at = 3774; l2750 = K[902];
    at = 3775; l2751 = rt.int_div(l2747, l2750);
    at = 3776; l2752 = K[903];
    at = 3777; l2753 = rt.max(l2752, l2751);
    at = 3778; l2754 = K[904];
    at = 3779; l2755 = rt.gt(l2737, l2754);
    at = 3780; if (rt.truthy(l2755)) {
      at = 3781; if (rt.truthy(l2730)) {
        at = 3782; l2758 = field(rt, l2733, "color");
        at = 3783; l2757 = l2758;
      } else {
        at = 3784; l2759 = K[905];
        at = 3785; l2757 = l2759;
      }
      at = 3786; l2760 = K[906];
      at = 3787; l2761 = rt.sequence_get(l2760, l2757);
      at = 3788; l2756 = l2761;
    } else {
      at = 3789; l2762 = K[907];
      at = 3790; l2756 = l2762;
    }
    at = 3791; markSet(rt, false, "circle", {at: l2740, radius: l2753, fill: l2756});
    at = 3792; if (rt.truthy(l2730)) {
      at = 3793; l2764 = field(rt, l2733, "points");
      at = 3794; l2763 = l2764;
    } else {
      at = 3795; l2765 = K[908];
      at = 3796; l2763 = l2765;
    }
    at = 3797; l2766 = K[909];
    at = 3798; l2767 = rt.sub(l2737, l2766);
    at = 3799; l2768 = K[910];
    at = 3800; l2769 = rt.max(l2768, l2767);
    at = 3801; l2770 = K[911];
    at = 3802; l2771 = rt.array_slice(l2763, l2770, l2769);
    at = 3803; if (rt.truthy(l2730)) {
      at = 3804; l2773 = field(rt, l2733, "points");
      at = 3805; l2772 = l2773;
    } else {
      at = 3806; l2774 = K[912];
      at = 3807; l2772 = l2774;
    }
    at = 3808; l2775 = K[913];
    at = 3809; l2776 = rt.gt(l2737, l2775);
    at = 3810; if (rt.truthy(l2776)) {
      at = 3811; l2778 = K[914];
      at = 3812; l2777 = l2778;
    } else {
      at = 3813; l2779 = K[915];
      at = 3814; l2777 = l2779;
    }
    at = 3815; l2780 = K[916];
    at = 3816; l2781 = rt.sub(l2737, l2780);
    at = 3817; l2782 = K[917];
    at = 3818; l2783 = rt.max(l2782, l2781);
    at = 3819; l2784 = rt.array_slice(l2772, l2777, l2783);
    at = 3820; if (rt.truthy(l2730)) {
      at = 3821; l2786 = field(rt, l2733, "color");
      at = 3822; l2785 = l2786;
    } else {
      at = 3823; l2787 = K[918];
      at = 3824; l2785 = l2787;
    }
    at = 3825; l2788 = K[919];
    at = 3826; l2789 = rt.sequence_get(l2788, l2785);
    at = 3827; if (rt.truthy(l2730)) {
      at = 3828; l2791 = field(rt, l2733, "width");
      at = 3829; l2790 = l2791;
    } else {
      at = 3830; l2792 = K[920];
      at = 3831; l2790 = l2792;
    }
    at = 3832; markSet(rt, true, "line", {from: l2771, to: l2784, color: l2789, width: l2790});
    at = 3833; l2793 = field(rt, l15, "strokes");
    at = 3834; l2794 = rt.sequence_length(l2793);
    at = 3835; l2795 = K[921];
    at = 3836; l2796 = rt.lt(l2795, l2794);
    at = 3837; if (rt.truthy(l2796)) {
      at = 3838; l2797 = field(rt, l15, "strokes");
      at = 3839; l2798 = K[922];
      at = 3840; l2799 = rt.sequence_get(l2797, l2798);
    }
    at = 3841; if (rt.truthy(l2796)) {
      at = 3842; l2801 = field(rt, l2799, "points");
      at = 3843; l2800 = l2801;
    } else {
      at = 3844; l2802 = K[923];
      at = 3845; l2800 = l2802;
    }
    at = 3846; l2803 = rt.array_count(l2800);
    at = 3847; l2804 = K[924];
    at = 3848; l2805 = rt.gt(l2803, l2804);
    at = 3849; if (rt.truthy(l2805)) {
      at = 3850; if (rt.truthy(l2796)) {
        at = 3851; l2808 = field(rt, l2799, "points");
        at = 3852; l2807 = l2808;
      } else {
        at = 3853; l2809 = K[925];
        at = 3854; l2807 = l2809;
      }
      at = 3855; l2810 = K[926];
      at = 3856; l2811 = rt.array_nth(l2807, l2810);
      at = 3857; l2806 = l2811;
    } else {
      at = 3858; l2812 = K[927];
      at = 3859; l2806 = l2812;
    }
    at = 3860; if (rt.truthy(l2796)) {
      at = 3861; l2814 = field(rt, l2799, "width");
      at = 3862; l2813 = l2814;
    } else {
      at = 3863; l2815 = K[928];
      at = 3864; l2813 = l2815;
    }
    at = 3865; l2816 = K[929];
    at = 3866; l2817 = rt.int_div(l2813, l2816);
    at = 3867; l2818 = K[930];
    at = 3868; l2819 = rt.max(l2818, l2817);
    at = 3869; l2820 = K[931];
    at = 3870; l2821 = rt.gt(l2803, l2820);
    at = 3871; if (rt.truthy(l2821)) {
      at = 3872; if (rt.truthy(l2796)) {
        at = 3873; l2824 = field(rt, l2799, "color");
        at = 3874; l2823 = l2824;
      } else {
        at = 3875; l2825 = K[932];
        at = 3876; l2823 = l2825;
      }
      at = 3877; l2826 = K[933];
      at = 3878; l2827 = rt.sequence_get(l2826, l2823);
      at = 3879; l2822 = l2827;
    } else {
      at = 3880; l2828 = K[934];
      at = 3881; l2822 = l2828;
    }
    at = 3882; markSet(rt, false, "circle", {at: l2806, radius: l2819, fill: l2822});
    at = 3883; if (rt.truthy(l2796)) {
      at = 3884; l2830 = field(rt, l2799, "points");
      at = 3885; l2829 = l2830;
    } else {
      at = 3886; l2831 = K[935];
      at = 3887; l2829 = l2831;
    }
    at = 3888; l2832 = K[936];
    at = 3889; l2833 = rt.sub(l2803, l2832);
    at = 3890; l2834 = K[937];
    at = 3891; l2835 = rt.max(l2834, l2833);
    at = 3892; l2836 = K[938];
    at = 3893; l2837 = rt.array_slice(l2829, l2836, l2835);
    at = 3894; if (rt.truthy(l2796)) {
      at = 3895; l2839 = field(rt, l2799, "points");
      at = 3896; l2838 = l2839;
    } else {
      at = 3897; l2840 = K[939];
      at = 3898; l2838 = l2840;
    }
    at = 3899; l2841 = K[940];
    at = 3900; l2842 = rt.gt(l2803, l2841);
    at = 3901; if (rt.truthy(l2842)) {
      at = 3902; l2844 = K[941];
      at = 3903; l2843 = l2844;
    } else {
      at = 3904; l2845 = K[942];
      at = 3905; l2843 = l2845;
    }
    at = 3906; l2846 = K[943];
    at = 3907; l2847 = rt.sub(l2803, l2846);
    at = 3908; l2848 = K[944];
    at = 3909; l2849 = rt.max(l2848, l2847);
    at = 3910; l2850 = rt.array_slice(l2838, l2843, l2849);
    at = 3911; if (rt.truthy(l2796)) {
      at = 3912; l2852 = field(rt, l2799, "color");
      at = 3913; l2851 = l2852;
    } else {
      at = 3914; l2853 = K[945];
      at = 3915; l2851 = l2853;
    }
    at = 3916; l2854 = K[946];
    at = 3917; l2855 = rt.sequence_get(l2854, l2851);
    at = 3918; if (rt.truthy(l2796)) {
      at = 3919; l2857 = field(rt, l2799, "width");
      at = 3920; l2856 = l2857;
    } else {
      at = 3921; l2858 = K[947];
      at = 3922; l2856 = l2858;
    }
    at = 3923; markSet(rt, true, "line", {from: l2837, to: l2850, color: l2855, width: l2856});
    at = 3924; l2859 = field(rt, l15, "strokes");
    at = 3925; l2860 = rt.sequence_length(l2859);
    at = 3926; l2861 = K[948];
    at = 3927; l2862 = rt.lt(l2861, l2860);
    at = 3928; if (rt.truthy(l2862)) {
      at = 3929; l2863 = field(rt, l15, "strokes");
      at = 3930; l2864 = K[949];
      at = 3931; l2865 = rt.sequence_get(l2863, l2864);
    }
    at = 3932; if (rt.truthy(l2862)) {
      at = 3933; l2867 = field(rt, l2865, "points");
      at = 3934; l2866 = l2867;
    } else {
      at = 3935; l2868 = K[950];
      at = 3936; l2866 = l2868;
    }
    at = 3937; l2869 = rt.array_count(l2866);
    at = 3938; l2870 = K[951];
    at = 3939; l2871 = rt.gt(l2869, l2870);
    at = 3940; if (rt.truthy(l2871)) {
      at = 3941; if (rt.truthy(l2862)) {
        at = 3942; l2874 = field(rt, l2865, "points");
        at = 3943; l2873 = l2874;
      } else {
        at = 3944; l2875 = K[952];
        at = 3945; l2873 = l2875;
      }
      at = 3946; l2876 = K[953];
      at = 3947; l2877 = rt.array_nth(l2873, l2876);
      at = 3948; l2872 = l2877;
    } else {
      at = 3949; l2878 = K[954];
      at = 3950; l2872 = l2878;
    }
    at = 3951; if (rt.truthy(l2862)) {
      at = 3952; l2880 = field(rt, l2865, "width");
      at = 3953; l2879 = l2880;
    } else {
      at = 3954; l2881 = K[955];
      at = 3955; l2879 = l2881;
    }
    at = 3956; l2882 = K[956];
    at = 3957; l2883 = rt.int_div(l2879, l2882);
    at = 3958; l2884 = K[957];
    at = 3959; l2885 = rt.max(l2884, l2883);
    at = 3960; l2886 = K[958];
    at = 3961; l2887 = rt.gt(l2869, l2886);
    at = 3962; if (rt.truthy(l2887)) {
      at = 3963; if (rt.truthy(l2862)) {
        at = 3964; l2890 = field(rt, l2865, "color");
        at = 3965; l2889 = l2890;
      } else {
        at = 3966; l2891 = K[959];
        at = 3967; l2889 = l2891;
      }
      at = 3968; l2892 = K[960];
      at = 3969; l2893 = rt.sequence_get(l2892, l2889);
      at = 3970; l2888 = l2893;
    } else {
      at = 3971; l2894 = K[961];
      at = 3972; l2888 = l2894;
    }
    at = 3973; markSet(rt, false, "circle", {at: l2872, radius: l2885, fill: l2888});
    at = 3974; if (rt.truthy(l2862)) {
      at = 3975; l2896 = field(rt, l2865, "points");
      at = 3976; l2895 = l2896;
    } else {
      at = 3977; l2897 = K[962];
      at = 3978; l2895 = l2897;
    }
    at = 3979; l2898 = K[963];
    at = 3980; l2899 = rt.sub(l2869, l2898);
    at = 3981; l2900 = K[964];
    at = 3982; l2901 = rt.max(l2900, l2899);
    at = 3983; l2902 = K[965];
    at = 3984; l2903 = rt.array_slice(l2895, l2902, l2901);
    at = 3985; if (rt.truthy(l2862)) {
      at = 3986; l2905 = field(rt, l2865, "points");
      at = 3987; l2904 = l2905;
    } else {
      at = 3988; l2906 = K[966];
      at = 3989; l2904 = l2906;
    }
    at = 3990; l2907 = K[967];
    at = 3991; l2908 = rt.gt(l2869, l2907);
    at = 3992; if (rt.truthy(l2908)) {
      at = 3993; l2910 = K[968];
      at = 3994; l2909 = l2910;
    } else {
      at = 3995; l2911 = K[969];
      at = 3996; l2909 = l2911;
    }
    at = 3997; l2912 = K[970];
    at = 3998; l2913 = rt.sub(l2869, l2912);
    at = 3999; l2914 = K[971];
    at = 4000; l2915 = rt.max(l2914, l2913);
    at = 4001; l2916 = rt.array_slice(l2904, l2909, l2915);
    at = 4002; if (rt.truthy(l2862)) {
      at = 4003; l2918 = field(rt, l2865, "color");
      at = 4004; l2917 = l2918;
    } else {
      at = 4005; l2919 = K[972];
      at = 4006; l2917 = l2919;
    }
    at = 4007; l2920 = K[973];
    at = 4008; l2921 = rt.sequence_get(l2920, l2917);
    at = 4009; if (rt.truthy(l2862)) {
      at = 4010; l2923 = field(rt, l2865, "width");
      at = 4011; l2922 = l2923;
    } else {
      at = 4012; l2924 = K[974];
      at = 4013; l2922 = l2924;
    }
    at = 4014; markSet(rt, true, "line", {from: l2903, to: l2916, color: l2921, width: l2922});
    at = 4015; l2925 = field(rt, l15, "strokes");
    at = 4016; l2926 = rt.sequence_length(l2925);
    at = 4017; l2927 = K[975];
    at = 4018; l2928 = rt.lt(l2927, l2926);
    at = 4019; if (rt.truthy(l2928)) {
      at = 4020; l2929 = field(rt, l15, "strokes");
      at = 4021; l2930 = K[976];
      at = 4022; l2931 = rt.sequence_get(l2929, l2930);
    }
    at = 4023; if (rt.truthy(l2928)) {
      at = 4024; l2933 = field(rt, l2931, "points");
      at = 4025; l2932 = l2933;
    } else {
      at = 4026; l2934 = K[977];
      at = 4027; l2932 = l2934;
    }
    at = 4028; l2935 = rt.array_count(l2932);
    at = 4029; l2936 = K[978];
    at = 4030; l2937 = rt.gt(l2935, l2936);
    at = 4031; if (rt.truthy(l2937)) {
      at = 4032; if (rt.truthy(l2928)) {
        at = 4033; l2940 = field(rt, l2931, "points");
        at = 4034; l2939 = l2940;
      } else {
        at = 4035; l2941 = K[979];
        at = 4036; l2939 = l2941;
      }
      at = 4037; l2942 = K[980];
      at = 4038; l2943 = rt.array_nth(l2939, l2942);
      at = 4039; l2938 = l2943;
    } else {
      at = 4040; l2944 = K[981];
      at = 4041; l2938 = l2944;
    }
    at = 4042; if (rt.truthy(l2928)) {
      at = 4043; l2946 = field(rt, l2931, "width");
      at = 4044; l2945 = l2946;
    } else {
      at = 4045; l2947 = K[982];
      at = 4046; l2945 = l2947;
    }
    at = 4047; l2948 = K[983];
    at = 4048; l2949 = rt.int_div(l2945, l2948);
    at = 4049; l2950 = K[984];
    at = 4050; l2951 = rt.max(l2950, l2949);
    at = 4051; l2952 = K[985];
    at = 4052; l2953 = rt.gt(l2935, l2952);
    at = 4053; if (rt.truthy(l2953)) {
      at = 4054; if (rt.truthy(l2928)) {
        at = 4055; l2956 = field(rt, l2931, "color");
        at = 4056; l2955 = l2956;
      } else {
        at = 4057; l2957 = K[986];
        at = 4058; l2955 = l2957;
      }
      at = 4059; l2958 = K[987];
      at = 4060; l2959 = rt.sequence_get(l2958, l2955);
      at = 4061; l2954 = l2959;
    } else {
      at = 4062; l2960 = K[988];
      at = 4063; l2954 = l2960;
    }
    at = 4064; markSet(rt, false, "circle", {at: l2938, radius: l2951, fill: l2954});
    at = 4065; if (rt.truthy(l2928)) {
      at = 4066; l2962 = field(rt, l2931, "points");
      at = 4067; l2961 = l2962;
    } else {
      at = 4068; l2963 = K[989];
      at = 4069; l2961 = l2963;
    }
    at = 4070; l2964 = K[990];
    at = 4071; l2965 = rt.sub(l2935, l2964);
    at = 4072; l2966 = K[991];
    at = 4073; l2967 = rt.max(l2966, l2965);
    at = 4074; l2968 = K[992];
    at = 4075; l2969 = rt.array_slice(l2961, l2968, l2967);
    at = 4076; if (rt.truthy(l2928)) {
      at = 4077; l2971 = field(rt, l2931, "points");
      at = 4078; l2970 = l2971;
    } else {
      at = 4079; l2972 = K[993];
      at = 4080; l2970 = l2972;
    }
    at = 4081; l2973 = K[994];
    at = 4082; l2974 = rt.gt(l2935, l2973);
    at = 4083; if (rt.truthy(l2974)) {
      at = 4084; l2976 = K[995];
      at = 4085; l2975 = l2976;
    } else {
      at = 4086; l2977 = K[996];
      at = 4087; l2975 = l2977;
    }
    at = 4088; l2978 = K[997];
    at = 4089; l2979 = rt.sub(l2935, l2978);
    at = 4090; l2980 = K[998];
    at = 4091; l2981 = rt.max(l2980, l2979);
    at = 4092; l2982 = rt.array_slice(l2970, l2975, l2981);
    at = 4093; if (rt.truthy(l2928)) {
      at = 4094; l2984 = field(rt, l2931, "color");
      at = 4095; l2983 = l2984;
    } else {
      at = 4096; l2985 = K[999];
      at = 4097; l2983 = l2985;
    }
    at = 4098; l2986 = K[1000];
    at = 4099; l2987 = rt.sequence_get(l2986, l2983);
    at = 4100; if (rt.truthy(l2928)) {
      at = 4101; l2989 = field(rt, l2931, "width");
      at = 4102; l2988 = l2989;
    } else {
      at = 4103; l2990 = K[1001];
      at = 4104; l2988 = l2990;
    }
    at = 4105; markSet(rt, true, "line", {from: l2969, to: l2982, color: l2987, width: l2988});
    at = 4106; l2991 = field(rt, l15, "strokes");
    at = 4107; l2992 = rt.sequence_length(l2991);
    at = 4108; l2993 = K[1002];
    at = 4109; l2994 = rt.lt(l2993, l2992);
    at = 4110; if (rt.truthy(l2994)) {
      at = 4111; l2995 = field(rt, l15, "strokes");
      at = 4112; l2996 = K[1003];
      at = 4113; l2997 = rt.sequence_get(l2995, l2996);
    }
    at = 4114; if (rt.truthy(l2994)) {
      at = 4115; l2999 = field(rt, l2997, "points");
      at = 4116; l2998 = l2999;
    } else {
      at = 4117; l3000 = K[1004];
      at = 4118; l2998 = l3000;
    }
    at = 4119; l3001 = rt.array_count(l2998);
    at = 4120; l3002 = K[1005];
    at = 4121; l3003 = rt.gt(l3001, l3002);
    at = 4122; if (rt.truthy(l3003)) {
      at = 4123; if (rt.truthy(l2994)) {
        at = 4124; l3006 = field(rt, l2997, "points");
        at = 4125; l3005 = l3006;
      } else {
        at = 4126; l3007 = K[1006];
        at = 4127; l3005 = l3007;
      }
      at = 4128; l3008 = K[1007];
      at = 4129; l3009 = rt.array_nth(l3005, l3008);
      at = 4130; l3004 = l3009;
    } else {
      at = 4131; l3010 = K[1008];
      at = 4132; l3004 = l3010;
    }
    at = 4133; if (rt.truthy(l2994)) {
      at = 4134; l3012 = field(rt, l2997, "width");
      at = 4135; l3011 = l3012;
    } else {
      at = 4136; l3013 = K[1009];
      at = 4137; l3011 = l3013;
    }
    at = 4138; l3014 = K[1010];
    at = 4139; l3015 = rt.int_div(l3011, l3014);
    at = 4140; l3016 = K[1011];
    at = 4141; l3017 = rt.max(l3016, l3015);
    at = 4142; l3018 = K[1012];
    at = 4143; l3019 = rt.gt(l3001, l3018);
    at = 4144; if (rt.truthy(l3019)) {
      at = 4145; if (rt.truthy(l2994)) {
        at = 4146; l3022 = field(rt, l2997, "color");
        at = 4147; l3021 = l3022;
      } else {
        at = 4148; l3023 = K[1013];
        at = 4149; l3021 = l3023;
      }
      at = 4150; l3024 = K[1014];
      at = 4151; l3025 = rt.sequence_get(l3024, l3021);
      at = 4152; l3020 = l3025;
    } else {
      at = 4153; l3026 = K[1015];
      at = 4154; l3020 = l3026;
    }
    at = 4155; markSet(rt, false, "circle", {at: l3004, radius: l3017, fill: l3020});
    at = 4156; if (rt.truthy(l2994)) {
      at = 4157; l3028 = field(rt, l2997, "points");
      at = 4158; l3027 = l3028;
    } else {
      at = 4159; l3029 = K[1016];
      at = 4160; l3027 = l3029;
    }
    at = 4161; l3030 = K[1017];
    at = 4162; l3031 = rt.sub(l3001, l3030);
    at = 4163; l3032 = K[1018];
    at = 4164; l3033 = rt.max(l3032, l3031);
    at = 4165; l3034 = K[1019];
    at = 4166; l3035 = rt.array_slice(l3027, l3034, l3033);
    at = 4167; if (rt.truthy(l2994)) {
      at = 4168; l3037 = field(rt, l2997, "points");
      at = 4169; l3036 = l3037;
    } else {
      at = 4170; l3038 = K[1020];
      at = 4171; l3036 = l3038;
    }
    at = 4172; l3039 = K[1021];
    at = 4173; l3040 = rt.gt(l3001, l3039);
    at = 4174; if (rt.truthy(l3040)) {
      at = 4175; l3042 = K[1022];
      at = 4176; l3041 = l3042;
    } else {
      at = 4177; l3043 = K[1023];
      at = 4178; l3041 = l3043;
    }
    at = 4179; l3044 = K[1024];
    at = 4180; l3045 = rt.sub(l3001, l3044);
    at = 4181; l3046 = K[1025];
    at = 4182; l3047 = rt.max(l3046, l3045);
    at = 4183; l3048 = rt.array_slice(l3036, l3041, l3047);
    at = 4184; if (rt.truthy(l2994)) {
      at = 4185; l3050 = field(rt, l2997, "color");
      at = 4186; l3049 = l3050;
    } else {
      at = 4187; l3051 = K[1026];
      at = 4188; l3049 = l3051;
    }
    at = 4189; l3052 = K[1027];
    at = 4190; l3053 = rt.sequence_get(l3052, l3049);
    at = 4191; if (rt.truthy(l2994)) {
      at = 4192; l3055 = field(rt, l2997, "width");
      at = 4193; l3054 = l3055;
    } else {
      at = 4194; l3056 = K[1028];
      at = 4195; l3054 = l3056;
    }
    at = 4196; markSet(rt, true, "line", {from: l3035, to: l3048, color: l3053, width: l3054});
    at = 4197; l3057 = field(rt, l15, "strokes");
    at = 4198; l3058 = rt.sequence_length(l3057);
    at = 4199; l3059 = K[1029];
    at = 4200; l3060 = rt.lt(l3059, l3058);
    at = 4201; if (rt.truthy(l3060)) {
      at = 4202; l3061 = field(rt, l15, "strokes");
      at = 4203; l3062 = K[1030];
      at = 4204; l3063 = rt.sequence_get(l3061, l3062);
    }
    at = 4205; if (rt.truthy(l3060)) {
      at = 4206; l3065 = field(rt, l3063, "points");
      at = 4207; l3064 = l3065;
    } else {
      at = 4208; l3066 = K[1031];
      at = 4209; l3064 = l3066;
    }
    at = 4210; l3067 = rt.array_count(l3064);
    at = 4211; l3068 = K[1032];
    at = 4212; l3069 = rt.gt(l3067, l3068);
    at = 4213; if (rt.truthy(l3069)) {
      at = 4214; if (rt.truthy(l3060)) {
        at = 4215; l3072 = field(rt, l3063, "points");
        at = 4216; l3071 = l3072;
      } else {
        at = 4217; l3073 = K[1033];
        at = 4218; l3071 = l3073;
      }
      at = 4219; l3074 = K[1034];
      at = 4220; l3075 = rt.array_nth(l3071, l3074);
      at = 4221; l3070 = l3075;
    } else {
      at = 4222; l3076 = K[1035];
      at = 4223; l3070 = l3076;
    }
    at = 4224; if (rt.truthy(l3060)) {
      at = 4225; l3078 = field(rt, l3063, "width");
      at = 4226; l3077 = l3078;
    } else {
      at = 4227; l3079 = K[1036];
      at = 4228; l3077 = l3079;
    }
    at = 4229; l3080 = K[1037];
    at = 4230; l3081 = rt.int_div(l3077, l3080);
    at = 4231; l3082 = K[1038];
    at = 4232; l3083 = rt.max(l3082, l3081);
    at = 4233; l3084 = K[1039];
    at = 4234; l3085 = rt.gt(l3067, l3084);
    at = 4235; if (rt.truthy(l3085)) {
      at = 4236; if (rt.truthy(l3060)) {
        at = 4237; l3088 = field(rt, l3063, "color");
        at = 4238; l3087 = l3088;
      } else {
        at = 4239; l3089 = K[1040];
        at = 4240; l3087 = l3089;
      }
      at = 4241; l3090 = K[1041];
      at = 4242; l3091 = rt.sequence_get(l3090, l3087);
      at = 4243; l3086 = l3091;
    } else {
      at = 4244; l3092 = K[1042];
      at = 4245; l3086 = l3092;
    }
    at = 4246; markSet(rt, false, "circle", {at: l3070, radius: l3083, fill: l3086});
    at = 4247; if (rt.truthy(l3060)) {
      at = 4248; l3094 = field(rt, l3063, "points");
      at = 4249; l3093 = l3094;
    } else {
      at = 4250; l3095 = K[1043];
      at = 4251; l3093 = l3095;
    }
    at = 4252; l3096 = K[1044];
    at = 4253; l3097 = rt.sub(l3067, l3096);
    at = 4254; l3098 = K[1045];
    at = 4255; l3099 = rt.max(l3098, l3097);
    at = 4256; l3100 = K[1046];
    at = 4257; l3101 = rt.array_slice(l3093, l3100, l3099);
    at = 4258; if (rt.truthy(l3060)) {
      at = 4259; l3103 = field(rt, l3063, "points");
      at = 4260; l3102 = l3103;
    } else {
      at = 4261; l3104 = K[1047];
      at = 4262; l3102 = l3104;
    }
    at = 4263; l3105 = K[1048];
    at = 4264; l3106 = rt.gt(l3067, l3105);
    at = 4265; if (rt.truthy(l3106)) {
      at = 4266; l3108 = K[1049];
      at = 4267; l3107 = l3108;
    } else {
      at = 4268; l3109 = K[1050];
      at = 4269; l3107 = l3109;
    }
    at = 4270; l3110 = K[1051];
    at = 4271; l3111 = rt.sub(l3067, l3110);
    at = 4272; l3112 = K[1052];
    at = 4273; l3113 = rt.max(l3112, l3111);
    at = 4274; l3114 = rt.array_slice(l3102, l3107, l3113);
    at = 4275; if (rt.truthy(l3060)) {
      at = 4276; l3116 = field(rt, l3063, "color");
      at = 4277; l3115 = l3116;
    } else {
      at = 4278; l3117 = K[1053];
      at = 4279; l3115 = l3117;
    }
    at = 4280; l3118 = K[1054];
    at = 4281; l3119 = rt.sequence_get(l3118, l3115);
    at = 4282; if (rt.truthy(l3060)) {
      at = 4283; l3121 = field(rt, l3063, "width");
      at = 4284; l3120 = l3121;
    } else {
      at = 4285; l3122 = K[1055];
      at = 4286; l3120 = l3122;
    }
    at = 4287; markSet(rt, true, "line", {from: l3101, to: l3114, color: l3119, width: l3120});
    at = 4288; l3123 = field(rt, l15, "strokes");
    at = 4289; l3124 = rt.sequence_length(l3123);
    at = 4290; l3125 = K[1056];
    at = 4291; l3126 = rt.lt(l3125, l3124);
    at = 4292; if (rt.truthy(l3126)) {
      at = 4293; l3127 = field(rt, l15, "strokes");
      at = 4294; l3128 = K[1057];
      at = 4295; l3129 = rt.sequence_get(l3127, l3128);
    }
    at = 4296; if (rt.truthy(l3126)) {
      at = 4297; l3131 = field(rt, l3129, "points");
      at = 4298; l3130 = l3131;
    } else {
      at = 4299; l3132 = K[1058];
      at = 4300; l3130 = l3132;
    }
    at = 4301; l3133 = rt.array_count(l3130);
    at = 4302; l3134 = K[1059];
    at = 4303; l3135 = rt.gt(l3133, l3134);
    at = 4304; if (rt.truthy(l3135)) {
      at = 4305; if (rt.truthy(l3126)) {
        at = 4306; l3138 = field(rt, l3129, "points");
        at = 4307; l3137 = l3138;
      } else {
        at = 4308; l3139 = K[1060];
        at = 4309; l3137 = l3139;
      }
      at = 4310; l3140 = K[1061];
      at = 4311; l3141 = rt.array_nth(l3137, l3140);
      at = 4312; l3136 = l3141;
    } else {
      at = 4313; l3142 = K[1062];
      at = 4314; l3136 = l3142;
    }
    at = 4315; if (rt.truthy(l3126)) {
      at = 4316; l3144 = field(rt, l3129, "width");
      at = 4317; l3143 = l3144;
    } else {
      at = 4318; l3145 = K[1063];
      at = 4319; l3143 = l3145;
    }
    at = 4320; l3146 = K[1064];
    at = 4321; l3147 = rt.int_div(l3143, l3146);
    at = 4322; l3148 = K[1065];
    at = 4323; l3149 = rt.max(l3148, l3147);
    at = 4324; l3150 = K[1066];
    at = 4325; l3151 = rt.gt(l3133, l3150);
    at = 4326; if (rt.truthy(l3151)) {
      at = 4327; if (rt.truthy(l3126)) {
        at = 4328; l3154 = field(rt, l3129, "color");
        at = 4329; l3153 = l3154;
      } else {
        at = 4330; l3155 = K[1067];
        at = 4331; l3153 = l3155;
      }
      at = 4332; l3156 = K[1068];
      at = 4333; l3157 = rt.sequence_get(l3156, l3153);
      at = 4334; l3152 = l3157;
    } else {
      at = 4335; l3158 = K[1069];
      at = 4336; l3152 = l3158;
    }
    at = 4337; markSet(rt, false, "circle", {at: l3136, radius: l3149, fill: l3152});
    at = 4338; if (rt.truthy(l3126)) {
      at = 4339; l3160 = field(rt, l3129, "points");
      at = 4340; l3159 = l3160;
    } else {
      at = 4341; l3161 = K[1070];
      at = 4342; l3159 = l3161;
    }
    at = 4343; l3162 = K[1071];
    at = 4344; l3163 = rt.sub(l3133, l3162);
    at = 4345; l3164 = K[1072];
    at = 4346; l3165 = rt.max(l3164, l3163);
    at = 4347; l3166 = K[1073];
    at = 4348; l3167 = rt.array_slice(l3159, l3166, l3165);
    at = 4349; if (rt.truthy(l3126)) {
      at = 4350; l3169 = field(rt, l3129, "points");
      at = 4351; l3168 = l3169;
    } else {
      at = 4352; l3170 = K[1074];
      at = 4353; l3168 = l3170;
    }
    at = 4354; l3171 = K[1075];
    at = 4355; l3172 = rt.gt(l3133, l3171);
    at = 4356; if (rt.truthy(l3172)) {
      at = 4357; l3174 = K[1076];
      at = 4358; l3173 = l3174;
    } else {
      at = 4359; l3175 = K[1077];
      at = 4360; l3173 = l3175;
    }
    at = 4361; l3176 = K[1078];
    at = 4362; l3177 = rt.sub(l3133, l3176);
    at = 4363; l3178 = K[1079];
    at = 4364; l3179 = rt.max(l3178, l3177);
    at = 4365; l3180 = rt.array_slice(l3168, l3173, l3179);
    at = 4366; if (rt.truthy(l3126)) {
      at = 4367; l3182 = field(rt, l3129, "color");
      at = 4368; l3181 = l3182;
    } else {
      at = 4369; l3183 = K[1080];
      at = 4370; l3181 = l3183;
    }
    at = 4371; l3184 = K[1081];
    at = 4372; l3185 = rt.sequence_get(l3184, l3181);
    at = 4373; if (rt.truthy(l3126)) {
      at = 4374; l3187 = field(rt, l3129, "width");
      at = 4375; l3186 = l3187;
    } else {
      at = 4376; l3188 = K[1082];
      at = 4377; l3186 = l3188;
    }
    at = 4378; markSet(rt, true, "line", {from: l3167, to: l3180, color: l3185, width: l3186});
    at = 4379; l3189 = field(rt, l15, "strokes");
    at = 4380; l3190 = rt.sequence_length(l3189);
    at = 4381; l3191 = K[1083];
    at = 4382; l3192 = rt.lt(l3191, l3190);
    at = 4383; if (rt.truthy(l3192)) {
      at = 4384; l3193 = field(rt, l15, "strokes");
      at = 4385; l3194 = K[1084];
      at = 4386; l3195 = rt.sequence_get(l3193, l3194);
    }
    at = 4387; if (rt.truthy(l3192)) {
      at = 4388; l3197 = field(rt, l3195, "points");
      at = 4389; l3196 = l3197;
    } else {
      at = 4390; l3198 = K[1085];
      at = 4391; l3196 = l3198;
    }
    at = 4392; l3199 = rt.array_count(l3196);
    at = 4393; l3200 = K[1086];
    at = 4394; l3201 = rt.gt(l3199, l3200);
    at = 4395; if (rt.truthy(l3201)) {
      at = 4396; if (rt.truthy(l3192)) {
        at = 4397; l3204 = field(rt, l3195, "points");
        at = 4398; l3203 = l3204;
      } else {
        at = 4399; l3205 = K[1087];
        at = 4400; l3203 = l3205;
      }
      at = 4401; l3206 = K[1088];
      at = 4402; l3207 = rt.array_nth(l3203, l3206);
      at = 4403; l3202 = l3207;
    } else {
      at = 4404; l3208 = K[1089];
      at = 4405; l3202 = l3208;
    }
    at = 4406; if (rt.truthy(l3192)) {
      at = 4407; l3210 = field(rt, l3195, "width");
      at = 4408; l3209 = l3210;
    } else {
      at = 4409; l3211 = K[1090];
      at = 4410; l3209 = l3211;
    }
    at = 4411; l3212 = K[1091];
    at = 4412; l3213 = rt.int_div(l3209, l3212);
    at = 4413; l3214 = K[1092];
    at = 4414; l3215 = rt.max(l3214, l3213);
    at = 4415; l3216 = K[1093];
    at = 4416; l3217 = rt.gt(l3199, l3216);
    at = 4417; if (rt.truthy(l3217)) {
      at = 4418; if (rt.truthy(l3192)) {
        at = 4419; l3220 = field(rt, l3195, "color");
        at = 4420; l3219 = l3220;
      } else {
        at = 4421; l3221 = K[1094];
        at = 4422; l3219 = l3221;
      }
      at = 4423; l3222 = K[1095];
      at = 4424; l3223 = rt.sequence_get(l3222, l3219);
      at = 4425; l3218 = l3223;
    } else {
      at = 4426; l3224 = K[1096];
      at = 4427; l3218 = l3224;
    }
    at = 4428; markSet(rt, false, "circle", {at: l3202, radius: l3215, fill: l3218});
    at = 4429; if (rt.truthy(l3192)) {
      at = 4430; l3226 = field(rt, l3195, "points");
      at = 4431; l3225 = l3226;
    } else {
      at = 4432; l3227 = K[1097];
      at = 4433; l3225 = l3227;
    }
    at = 4434; l3228 = K[1098];
    at = 4435; l3229 = rt.sub(l3199, l3228);
    at = 4436; l3230 = K[1099];
    at = 4437; l3231 = rt.max(l3230, l3229);
    at = 4438; l3232 = K[1100];
    at = 4439; l3233 = rt.array_slice(l3225, l3232, l3231);
    at = 4440; if (rt.truthy(l3192)) {
      at = 4441; l3235 = field(rt, l3195, "points");
      at = 4442; l3234 = l3235;
    } else {
      at = 4443; l3236 = K[1101];
      at = 4444; l3234 = l3236;
    }
    at = 4445; l3237 = K[1102];
    at = 4446; l3238 = rt.gt(l3199, l3237);
    at = 4447; if (rt.truthy(l3238)) {
      at = 4448; l3240 = K[1103];
      at = 4449; l3239 = l3240;
    } else {
      at = 4450; l3241 = K[1104];
      at = 4451; l3239 = l3241;
    }
    at = 4452; l3242 = K[1105];
    at = 4453; l3243 = rt.sub(l3199, l3242);
    at = 4454; l3244 = K[1106];
    at = 4455; l3245 = rt.max(l3244, l3243);
    at = 4456; l3246 = rt.array_slice(l3234, l3239, l3245);
    at = 4457; if (rt.truthy(l3192)) {
      at = 4458; l3248 = field(rt, l3195, "color");
      at = 4459; l3247 = l3248;
    } else {
      at = 4460; l3249 = K[1107];
      at = 4461; l3247 = l3249;
    }
    at = 4462; l3250 = K[1108];
    at = 4463; l3251 = rt.sequence_get(l3250, l3247);
    at = 4464; if (rt.truthy(l3192)) {
      at = 4465; l3253 = field(rt, l3195, "width");
      at = 4466; l3252 = l3253;
    } else {
      at = 4467; l3254 = K[1109];
      at = 4468; l3252 = l3254;
    }
    at = 4469; markSet(rt, true, "line", {from: l3233, to: l3246, color: l3251, width: l3252});
    at = 4470; l3255 = field(rt, l15, "strokes");
    at = 4471; l3256 = rt.sequence_length(l3255);
    at = 4472; l3257 = K[1110];
    at = 4473; l3258 = rt.lt(l3257, l3256);
    at = 4474; if (rt.truthy(l3258)) {
      at = 4475; l3259 = field(rt, l15, "strokes");
      at = 4476; l3260 = K[1111];
      at = 4477; l3261 = rt.sequence_get(l3259, l3260);
    }
    at = 4478; if (rt.truthy(l3258)) {
      at = 4479; l3263 = field(rt, l3261, "points");
      at = 4480; l3262 = l3263;
    } else {
      at = 4481; l3264 = K[1112];
      at = 4482; l3262 = l3264;
    }
    at = 4483; l3265 = rt.array_count(l3262);
    at = 4484; l3266 = K[1113];
    at = 4485; l3267 = rt.gt(l3265, l3266);
    at = 4486; if (rt.truthy(l3267)) {
      at = 4487; if (rt.truthy(l3258)) {
        at = 4488; l3270 = field(rt, l3261, "points");
        at = 4489; l3269 = l3270;
      } else {
        at = 4490; l3271 = K[1114];
        at = 4491; l3269 = l3271;
      }
      at = 4492; l3272 = K[1115];
      at = 4493; l3273 = rt.array_nth(l3269, l3272);
      at = 4494; l3268 = l3273;
    } else {
      at = 4495; l3274 = K[1116];
      at = 4496; l3268 = l3274;
    }
    at = 4497; if (rt.truthy(l3258)) {
      at = 4498; l3276 = field(rt, l3261, "width");
      at = 4499; l3275 = l3276;
    } else {
      at = 4500; l3277 = K[1117];
      at = 4501; l3275 = l3277;
    }
    at = 4502; l3278 = K[1118];
    at = 4503; l3279 = rt.int_div(l3275, l3278);
    at = 4504; l3280 = K[1119];
    at = 4505; l3281 = rt.max(l3280, l3279);
    at = 4506; l3282 = K[1120];
    at = 4507; l3283 = rt.gt(l3265, l3282);
    at = 4508; if (rt.truthy(l3283)) {
      at = 4509; if (rt.truthy(l3258)) {
        at = 4510; l3286 = field(rt, l3261, "color");
        at = 4511; l3285 = l3286;
      } else {
        at = 4512; l3287 = K[1121];
        at = 4513; l3285 = l3287;
      }
      at = 4514; l3288 = K[1122];
      at = 4515; l3289 = rt.sequence_get(l3288, l3285);
      at = 4516; l3284 = l3289;
    } else {
      at = 4517; l3290 = K[1123];
      at = 4518; l3284 = l3290;
    }
    at = 4519; markSet(rt, false, "circle", {at: l3268, radius: l3281, fill: l3284});
    at = 4520; if (rt.truthy(l3258)) {
      at = 4521; l3292 = field(rt, l3261, "points");
      at = 4522; l3291 = l3292;
    } else {
      at = 4523; l3293 = K[1124];
      at = 4524; l3291 = l3293;
    }
    at = 4525; l3294 = K[1125];
    at = 4526; l3295 = rt.sub(l3265, l3294);
    at = 4527; l3296 = K[1126];
    at = 4528; l3297 = rt.max(l3296, l3295);
    at = 4529; l3298 = K[1127];
    at = 4530; l3299 = rt.array_slice(l3291, l3298, l3297);
    at = 4531; if (rt.truthy(l3258)) {
      at = 4532; l3301 = field(rt, l3261, "points");
      at = 4533; l3300 = l3301;
    } else {
      at = 4534; l3302 = K[1128];
      at = 4535; l3300 = l3302;
    }
    at = 4536; l3303 = K[1129];
    at = 4537; l3304 = rt.gt(l3265, l3303);
    at = 4538; if (rt.truthy(l3304)) {
      at = 4539; l3306 = K[1130];
      at = 4540; l3305 = l3306;
    } else {
      at = 4541; l3307 = K[1131];
      at = 4542; l3305 = l3307;
    }
    at = 4543; l3308 = K[1132];
    at = 4544; l3309 = rt.sub(l3265, l3308);
    at = 4545; l3310 = K[1133];
    at = 4546; l3311 = rt.max(l3310, l3309);
    at = 4547; l3312 = rt.array_slice(l3300, l3305, l3311);
    at = 4548; if (rt.truthy(l3258)) {
      at = 4549; l3314 = field(rt, l3261, "color");
      at = 4550; l3313 = l3314;
    } else {
      at = 4551; l3315 = K[1134];
      at = 4552; l3313 = l3315;
    }
    at = 4553; l3316 = K[1135];
    at = 4554; l3317 = rt.sequence_get(l3316, l3313);
    at = 4555; if (rt.truthy(l3258)) {
      at = 4556; l3319 = field(rt, l3261, "width");
      at = 4557; l3318 = l3319;
    } else {
      at = 4558; l3320 = K[1136];
      at = 4559; l3318 = l3320;
    }
    at = 4560; markSet(rt, true, "line", {from: l3299, to: l3312, color: l3317, width: l3318});
    at = 4561; l3321 = field(rt, l15, "strokes");
    at = 4562; l3322 = rt.sequence_length(l3321);
    at = 4563; l3323 = K[1137];
    at = 4564; l3324 = rt.lt(l3323, l3322);
    at = 4565; if (rt.truthy(l3324)) {
      at = 4566; l3325 = field(rt, l15, "strokes");
      at = 4567; l3326 = K[1138];
      at = 4568; l3327 = rt.sequence_get(l3325, l3326);
    }
    at = 4569; if (rt.truthy(l3324)) {
      at = 4570; l3329 = field(rt, l3327, "points");
      at = 4571; l3328 = l3329;
    } else {
      at = 4572; l3330 = K[1139];
      at = 4573; l3328 = l3330;
    }
    at = 4574; l3331 = rt.array_count(l3328);
    at = 4575; l3332 = K[1140];
    at = 4576; l3333 = rt.gt(l3331, l3332);
    at = 4577; if (rt.truthy(l3333)) {
      at = 4578; if (rt.truthy(l3324)) {
        at = 4579; l3336 = field(rt, l3327, "points");
        at = 4580; l3335 = l3336;
      } else {
        at = 4581; l3337 = K[1141];
        at = 4582; l3335 = l3337;
      }
      at = 4583; l3338 = K[1142];
      at = 4584; l3339 = rt.array_nth(l3335, l3338);
      at = 4585; l3334 = l3339;
    } else {
      at = 4586; l3340 = K[1143];
      at = 4587; l3334 = l3340;
    }
    at = 4588; if (rt.truthy(l3324)) {
      at = 4589; l3342 = field(rt, l3327, "width");
      at = 4590; l3341 = l3342;
    } else {
      at = 4591; l3343 = K[1144];
      at = 4592; l3341 = l3343;
    }
    at = 4593; l3344 = K[1145];
    at = 4594; l3345 = rt.int_div(l3341, l3344);
    at = 4595; l3346 = K[1146];
    at = 4596; l3347 = rt.max(l3346, l3345);
    at = 4597; l3348 = K[1147];
    at = 4598; l3349 = rt.gt(l3331, l3348);
    at = 4599; if (rt.truthy(l3349)) {
      at = 4600; if (rt.truthy(l3324)) {
        at = 4601; l3352 = field(rt, l3327, "color");
        at = 4602; l3351 = l3352;
      } else {
        at = 4603; l3353 = K[1148];
        at = 4604; l3351 = l3353;
      }
      at = 4605; l3354 = K[1149];
      at = 4606; l3355 = rt.sequence_get(l3354, l3351);
      at = 4607; l3350 = l3355;
    } else {
      at = 4608; l3356 = K[1150];
      at = 4609; l3350 = l3356;
    }
    at = 4610; markSet(rt, false, "circle", {at: l3334, radius: l3347, fill: l3350});
    at = 4611; if (rt.truthy(l3324)) {
      at = 4612; l3358 = field(rt, l3327, "points");
      at = 4613; l3357 = l3358;
    } else {
      at = 4614; l3359 = K[1151];
      at = 4615; l3357 = l3359;
    }
    at = 4616; l3360 = K[1152];
    at = 4617; l3361 = rt.sub(l3331, l3360);
    at = 4618; l3362 = K[1153];
    at = 4619; l3363 = rt.max(l3362, l3361);
    at = 4620; l3364 = K[1154];
    at = 4621; l3365 = rt.array_slice(l3357, l3364, l3363);
    at = 4622; if (rt.truthy(l3324)) {
      at = 4623; l3367 = field(rt, l3327, "points");
      at = 4624; l3366 = l3367;
    } else {
      at = 4625; l3368 = K[1155];
      at = 4626; l3366 = l3368;
    }
    at = 4627; l3369 = K[1156];
    at = 4628; l3370 = rt.gt(l3331, l3369);
    at = 4629; if (rt.truthy(l3370)) {
      at = 4630; l3372 = K[1157];
      at = 4631; l3371 = l3372;
    } else {
      at = 4632; l3373 = K[1158];
      at = 4633; l3371 = l3373;
    }
    at = 4634; l3374 = K[1159];
    at = 4635; l3375 = rt.sub(l3331, l3374);
    at = 4636; l3376 = K[1160];
    at = 4637; l3377 = rt.max(l3376, l3375);
    at = 4638; l3378 = rt.array_slice(l3366, l3371, l3377);
    at = 4639; if (rt.truthy(l3324)) {
      at = 4640; l3380 = field(rt, l3327, "color");
      at = 4641; l3379 = l3380;
    } else {
      at = 4642; l3381 = K[1161];
      at = 4643; l3379 = l3381;
    }
    at = 4644; l3382 = K[1162];
    at = 4645; l3383 = rt.sequence_get(l3382, l3379);
    at = 4646; if (rt.truthy(l3324)) {
      at = 4647; l3385 = field(rt, l3327, "width");
      at = 4648; l3384 = l3385;
    } else {
      at = 4649; l3386 = K[1163];
      at = 4650; l3384 = l3386;
    }
    at = 4651; markSet(rt, true, "line", {from: l3365, to: l3378, color: l3383, width: l3384});
    at = 4652; l3387 = field(rt, l15, "strokes");
    at = 4653; l3388 = rt.sequence_length(l3387);
    at = 4654; l3389 = K[1164];
    at = 4655; l3390 = rt.lt(l3389, l3388);
    at = 4656; if (rt.truthy(l3390)) {
      at = 4657; l3391 = field(rt, l15, "strokes");
      at = 4658; l3392 = K[1165];
      at = 4659; l3393 = rt.sequence_get(l3391, l3392);
    }
    at = 4660; if (rt.truthy(l3390)) {
      at = 4661; l3395 = field(rt, l3393, "points");
      at = 4662; l3394 = l3395;
    } else {
      at = 4663; l3396 = K[1166];
      at = 4664; l3394 = l3396;
    }
    at = 4665; l3397 = rt.array_count(l3394);
    at = 4666; l3398 = K[1167];
    at = 4667; l3399 = rt.gt(l3397, l3398);
    at = 4668; if (rt.truthy(l3399)) {
      at = 4669; if (rt.truthy(l3390)) {
        at = 4670; l3402 = field(rt, l3393, "points");
        at = 4671; l3401 = l3402;
      } else {
        at = 4672; l3403 = K[1168];
        at = 4673; l3401 = l3403;
      }
      at = 4674; l3404 = K[1169];
      at = 4675; l3405 = rt.array_nth(l3401, l3404);
      at = 4676; l3400 = l3405;
    } else {
      at = 4677; l3406 = K[1170];
      at = 4678; l3400 = l3406;
    }
    at = 4679; if (rt.truthy(l3390)) {
      at = 4680; l3408 = field(rt, l3393, "width");
      at = 4681; l3407 = l3408;
    } else {
      at = 4682; l3409 = K[1171];
      at = 4683; l3407 = l3409;
    }
    at = 4684; l3410 = K[1172];
    at = 4685; l3411 = rt.int_div(l3407, l3410);
    at = 4686; l3412 = K[1173];
    at = 4687; l3413 = rt.max(l3412, l3411);
    at = 4688; l3414 = K[1174];
    at = 4689; l3415 = rt.gt(l3397, l3414);
    at = 4690; if (rt.truthy(l3415)) {
      at = 4691; if (rt.truthy(l3390)) {
        at = 4692; l3418 = field(rt, l3393, "color");
        at = 4693; l3417 = l3418;
      } else {
        at = 4694; l3419 = K[1175];
        at = 4695; l3417 = l3419;
      }
      at = 4696; l3420 = K[1176];
      at = 4697; l3421 = rt.sequence_get(l3420, l3417);
      at = 4698; l3416 = l3421;
    } else {
      at = 4699; l3422 = K[1177];
      at = 4700; l3416 = l3422;
    }
    at = 4701; markSet(rt, false, "circle", {at: l3400, radius: l3413, fill: l3416});
    at = 4702; if (rt.truthy(l3390)) {
      at = 4703; l3424 = field(rt, l3393, "points");
      at = 4704; l3423 = l3424;
    } else {
      at = 4705; l3425 = K[1178];
      at = 4706; l3423 = l3425;
    }
    at = 4707; l3426 = K[1179];
    at = 4708; l3427 = rt.sub(l3397, l3426);
    at = 4709; l3428 = K[1180];
    at = 4710; l3429 = rt.max(l3428, l3427);
    at = 4711; l3430 = K[1181];
    at = 4712; l3431 = rt.array_slice(l3423, l3430, l3429);
    at = 4713; if (rt.truthy(l3390)) {
      at = 4714; l3433 = field(rt, l3393, "points");
      at = 4715; l3432 = l3433;
    } else {
      at = 4716; l3434 = K[1182];
      at = 4717; l3432 = l3434;
    }
    at = 4718; l3435 = K[1183];
    at = 4719; l3436 = rt.gt(l3397, l3435);
    at = 4720; if (rt.truthy(l3436)) {
      at = 4721; l3438 = K[1184];
      at = 4722; l3437 = l3438;
    } else {
      at = 4723; l3439 = K[1185];
      at = 4724; l3437 = l3439;
    }
    at = 4725; l3440 = K[1186];
    at = 4726; l3441 = rt.sub(l3397, l3440);
    at = 4727; l3442 = K[1187];
    at = 4728; l3443 = rt.max(l3442, l3441);
    at = 4729; l3444 = rt.array_slice(l3432, l3437, l3443);
    at = 4730; if (rt.truthy(l3390)) {
      at = 4731; l3446 = field(rt, l3393, "color");
      at = 4732; l3445 = l3446;
    } else {
      at = 4733; l3447 = K[1188];
      at = 4734; l3445 = l3447;
    }
    at = 4735; l3448 = K[1189];
    at = 4736; l3449 = rt.sequence_get(l3448, l3445);
    at = 4737; if (rt.truthy(l3390)) {
      at = 4738; l3451 = field(rt, l3393, "width");
      at = 4739; l3450 = l3451;
    } else {
      at = 4740; l3452 = K[1190];
      at = 4741; l3450 = l3452;
    }
    at = 4742; markSet(rt, true, "line", {from: l3431, to: l3444, color: l3449, width: l3450});
    at = 4743; l3453 = field(rt, l15, "strokes");
    at = 4744; l3454 = rt.sequence_length(l3453);
    at = 4745; l3455 = K[1191];
    at = 4746; l3456 = rt.lt(l3455, l3454);
    at = 4747; if (rt.truthy(l3456)) {
      at = 4748; l3457 = field(rt, l15, "strokes");
      at = 4749; l3458 = K[1192];
      at = 4750; l3459 = rt.sequence_get(l3457, l3458);
    }
    at = 4751; if (rt.truthy(l3456)) {
      at = 4752; l3461 = field(rt, l3459, "points");
      at = 4753; l3460 = l3461;
    } else {
      at = 4754; l3462 = K[1193];
      at = 4755; l3460 = l3462;
    }
    at = 4756; l3463 = rt.array_count(l3460);
    at = 4757; l3464 = K[1194];
    at = 4758; l3465 = rt.gt(l3463, l3464);
    at = 4759; if (rt.truthy(l3465)) {
      at = 4760; if (rt.truthy(l3456)) {
        at = 4761; l3468 = field(rt, l3459, "points");
        at = 4762; l3467 = l3468;
      } else {
        at = 4763; l3469 = K[1195];
        at = 4764; l3467 = l3469;
      }
      at = 4765; l3470 = K[1196];
      at = 4766; l3471 = rt.array_nth(l3467, l3470);
      at = 4767; l3466 = l3471;
    } else {
      at = 4768; l3472 = K[1197];
      at = 4769; l3466 = l3472;
    }
    at = 4770; if (rt.truthy(l3456)) {
      at = 4771; l3474 = field(rt, l3459, "width");
      at = 4772; l3473 = l3474;
    } else {
      at = 4773; l3475 = K[1198];
      at = 4774; l3473 = l3475;
    }
    at = 4775; l3476 = K[1199];
    at = 4776; l3477 = rt.int_div(l3473, l3476);
    at = 4777; l3478 = K[1200];
    at = 4778; l3479 = rt.max(l3478, l3477);
    at = 4779; l3480 = K[1201];
    at = 4780; l3481 = rt.gt(l3463, l3480);
    at = 4781; if (rt.truthy(l3481)) {
      at = 4782; if (rt.truthy(l3456)) {
        at = 4783; l3484 = field(rt, l3459, "color");
        at = 4784; l3483 = l3484;
      } else {
        at = 4785; l3485 = K[1202];
        at = 4786; l3483 = l3485;
      }
      at = 4787; l3486 = K[1203];
      at = 4788; l3487 = rt.sequence_get(l3486, l3483);
      at = 4789; l3482 = l3487;
    } else {
      at = 4790; l3488 = K[1204];
      at = 4791; l3482 = l3488;
    }
    at = 4792; markSet(rt, false, "circle", {at: l3466, radius: l3479, fill: l3482});
    at = 4793; if (rt.truthy(l3456)) {
      at = 4794; l3490 = field(rt, l3459, "points");
      at = 4795; l3489 = l3490;
    } else {
      at = 4796; l3491 = K[1205];
      at = 4797; l3489 = l3491;
    }
    at = 4798; l3492 = K[1206];
    at = 4799; l3493 = rt.sub(l3463, l3492);
    at = 4800; l3494 = K[1207];
    at = 4801; l3495 = rt.max(l3494, l3493);
    at = 4802; l3496 = K[1208];
    at = 4803; l3497 = rt.array_slice(l3489, l3496, l3495);
    at = 4804; if (rt.truthy(l3456)) {
      at = 4805; l3499 = field(rt, l3459, "points");
      at = 4806; l3498 = l3499;
    } else {
      at = 4807; l3500 = K[1209];
      at = 4808; l3498 = l3500;
    }
    at = 4809; l3501 = K[1210];
    at = 4810; l3502 = rt.gt(l3463, l3501);
    at = 4811; if (rt.truthy(l3502)) {
      at = 4812; l3504 = K[1211];
      at = 4813; l3503 = l3504;
    } else {
      at = 4814; l3505 = K[1212];
      at = 4815; l3503 = l3505;
    }
    at = 4816; l3506 = K[1213];
    at = 4817; l3507 = rt.sub(l3463, l3506);
    at = 4818; l3508 = K[1214];
    at = 4819; l3509 = rt.max(l3508, l3507);
    at = 4820; l3510 = rt.array_slice(l3498, l3503, l3509);
    at = 4821; if (rt.truthy(l3456)) {
      at = 4822; l3512 = field(rt, l3459, "color");
      at = 4823; l3511 = l3512;
    } else {
      at = 4824; l3513 = K[1215];
      at = 4825; l3511 = l3513;
    }
    at = 4826; l3514 = K[1216];
    at = 4827; l3515 = rt.sequence_get(l3514, l3511);
    at = 4828; if (rt.truthy(l3456)) {
      at = 4829; l3517 = field(rt, l3459, "width");
      at = 4830; l3516 = l3517;
    } else {
      at = 4831; l3518 = K[1217];
      at = 4832; l3516 = l3518;
    }
    at = 4833; markSet(rt, true, "line", {from: l3497, to: l3510, color: l3515, width: l3516});
    at = 4834; l3519 = field(rt, l15, "strokes");
    at = 4835; l3520 = rt.sequence_length(l3519);
    at = 4836; l3521 = K[1218];
    at = 4837; l3522 = rt.lt(l3521, l3520);
    at = 4838; if (rt.truthy(l3522)) {
      at = 4839; l3523 = field(rt, l15, "strokes");
      at = 4840; l3524 = K[1219];
      at = 4841; l3525 = rt.sequence_get(l3523, l3524);
    }
    at = 4842; if (rt.truthy(l3522)) {
      at = 4843; l3527 = field(rt, l3525, "points");
      at = 4844; l3526 = l3527;
    } else {
      at = 4845; l3528 = K[1220];
      at = 4846; l3526 = l3528;
    }
    at = 4847; l3529 = rt.array_count(l3526);
    at = 4848; l3530 = K[1221];
    at = 4849; l3531 = rt.gt(l3529, l3530);
    at = 4850; if (rt.truthy(l3531)) {
      at = 4851; if (rt.truthy(l3522)) {
        at = 4852; l3534 = field(rt, l3525, "points");
        at = 4853; l3533 = l3534;
      } else {
        at = 4854; l3535 = K[1222];
        at = 4855; l3533 = l3535;
      }
      at = 4856; l3536 = K[1223];
      at = 4857; l3537 = rt.array_nth(l3533, l3536);
      at = 4858; l3532 = l3537;
    } else {
      at = 4859; l3538 = K[1224];
      at = 4860; l3532 = l3538;
    }
    at = 4861; if (rt.truthy(l3522)) {
      at = 4862; l3540 = field(rt, l3525, "width");
      at = 4863; l3539 = l3540;
    } else {
      at = 4864; l3541 = K[1225];
      at = 4865; l3539 = l3541;
    }
    at = 4866; l3542 = K[1226];
    at = 4867; l3543 = rt.int_div(l3539, l3542);
    at = 4868; l3544 = K[1227];
    at = 4869; l3545 = rt.max(l3544, l3543);
    at = 4870; l3546 = K[1228];
    at = 4871; l3547 = rt.gt(l3529, l3546);
    at = 4872; if (rt.truthy(l3547)) {
      at = 4873; if (rt.truthy(l3522)) {
        at = 4874; l3550 = field(rt, l3525, "color");
        at = 4875; l3549 = l3550;
      } else {
        at = 4876; l3551 = K[1229];
        at = 4877; l3549 = l3551;
      }
      at = 4878; l3552 = K[1230];
      at = 4879; l3553 = rt.sequence_get(l3552, l3549);
      at = 4880; l3548 = l3553;
    } else {
      at = 4881; l3554 = K[1231];
      at = 4882; l3548 = l3554;
    }
    at = 4883; markSet(rt, false, "circle", {at: l3532, radius: l3545, fill: l3548});
    at = 4884; if (rt.truthy(l3522)) {
      at = 4885; l3556 = field(rt, l3525, "points");
      at = 4886; l3555 = l3556;
    } else {
      at = 4887; l3557 = K[1232];
      at = 4888; l3555 = l3557;
    }
    at = 4889; l3558 = K[1233];
    at = 4890; l3559 = rt.sub(l3529, l3558);
    at = 4891; l3560 = K[1234];
    at = 4892; l3561 = rt.max(l3560, l3559);
    at = 4893; l3562 = K[1235];
    at = 4894; l3563 = rt.array_slice(l3555, l3562, l3561);
    at = 4895; if (rt.truthy(l3522)) {
      at = 4896; l3565 = field(rt, l3525, "points");
      at = 4897; l3564 = l3565;
    } else {
      at = 4898; l3566 = K[1236];
      at = 4899; l3564 = l3566;
    }
    at = 4900; l3567 = K[1237];
    at = 4901; l3568 = rt.gt(l3529, l3567);
    at = 4902; if (rt.truthy(l3568)) {
      at = 4903; l3570 = K[1238];
      at = 4904; l3569 = l3570;
    } else {
      at = 4905; l3571 = K[1239];
      at = 4906; l3569 = l3571;
    }
    at = 4907; l3572 = K[1240];
    at = 4908; l3573 = rt.sub(l3529, l3572);
    at = 4909; l3574 = K[1241];
    at = 4910; l3575 = rt.max(l3574, l3573);
    at = 4911; l3576 = rt.array_slice(l3564, l3569, l3575);
    at = 4912; if (rt.truthy(l3522)) {
      at = 4913; l3578 = field(rt, l3525, "color");
      at = 4914; l3577 = l3578;
    } else {
      at = 4915; l3579 = K[1242];
      at = 4916; l3577 = l3579;
    }
    at = 4917; l3580 = K[1243];
    at = 4918; l3581 = rt.sequence_get(l3580, l3577);
    at = 4919; if (rt.truthy(l3522)) {
      at = 4920; l3583 = field(rt, l3525, "width");
      at = 4921; l3582 = l3583;
    } else {
      at = 4922; l3584 = K[1244];
      at = 4923; l3582 = l3584;
    }
    at = 4924; markSet(rt, true, "line", {from: l3563, to: l3576, color: l3581, width: l3582});
    at = 4925; l3585 = field(rt, l15, "strokes");
    at = 4926; l3586 = rt.sequence_length(l3585);
    at = 4927; l3587 = K[1245];
    at = 4928; l3588 = rt.lt(l3587, l3586);
    at = 4929; if (rt.truthy(l3588)) {
      at = 4930; l3589 = field(rt, l15, "strokes");
      at = 4931; l3590 = K[1246];
      at = 4932; l3591 = rt.sequence_get(l3589, l3590);
    }
    at = 4933; if (rt.truthy(l3588)) {
      at = 4934; l3593 = field(rt, l3591, "points");
      at = 4935; l3592 = l3593;
    } else {
      at = 4936; l3594 = K[1247];
      at = 4937; l3592 = l3594;
    }
    at = 4938; l3595 = rt.array_count(l3592);
    at = 4939; l3596 = K[1248];
    at = 4940; l3597 = rt.gt(l3595, l3596);
    at = 4941; if (rt.truthy(l3597)) {
      at = 4942; if (rt.truthy(l3588)) {
        at = 4943; l3600 = field(rt, l3591, "points");
        at = 4944; l3599 = l3600;
      } else {
        at = 4945; l3601 = K[1249];
        at = 4946; l3599 = l3601;
      }
      at = 4947; l3602 = K[1250];
      at = 4948; l3603 = rt.array_nth(l3599, l3602);
      at = 4949; l3598 = l3603;
    } else {
      at = 4950; l3604 = K[1251];
      at = 4951; l3598 = l3604;
    }
    at = 4952; if (rt.truthy(l3588)) {
      at = 4953; l3606 = field(rt, l3591, "width");
      at = 4954; l3605 = l3606;
    } else {
      at = 4955; l3607 = K[1252];
      at = 4956; l3605 = l3607;
    }
    at = 4957; l3608 = K[1253];
    at = 4958; l3609 = rt.int_div(l3605, l3608);
    at = 4959; l3610 = K[1254];
    at = 4960; l3611 = rt.max(l3610, l3609);
    at = 4961; l3612 = K[1255];
    at = 4962; l3613 = rt.gt(l3595, l3612);
    at = 4963; if (rt.truthy(l3613)) {
      at = 4964; if (rt.truthy(l3588)) {
        at = 4965; l3616 = field(rt, l3591, "color");
        at = 4966; l3615 = l3616;
      } else {
        at = 4967; l3617 = K[1256];
        at = 4968; l3615 = l3617;
      }
      at = 4969; l3618 = K[1257];
      at = 4970; l3619 = rt.sequence_get(l3618, l3615);
      at = 4971; l3614 = l3619;
    } else {
      at = 4972; l3620 = K[1258];
      at = 4973; l3614 = l3620;
    }
    at = 4974; markSet(rt, false, "circle", {at: l3598, radius: l3611, fill: l3614});
    at = 4975; if (rt.truthy(l3588)) {
      at = 4976; l3622 = field(rt, l3591, "points");
      at = 4977; l3621 = l3622;
    } else {
      at = 4978; l3623 = K[1259];
      at = 4979; l3621 = l3623;
    }
    at = 4980; l3624 = K[1260];
    at = 4981; l3625 = rt.sub(l3595, l3624);
    at = 4982; l3626 = K[1261];
    at = 4983; l3627 = rt.max(l3626, l3625);
    at = 4984; l3628 = K[1262];
    at = 4985; l3629 = rt.array_slice(l3621, l3628, l3627);
    at = 4986; if (rt.truthy(l3588)) {
      at = 4987; l3631 = field(rt, l3591, "points");
      at = 4988; l3630 = l3631;
    } else {
      at = 4989; l3632 = K[1263];
      at = 4990; l3630 = l3632;
    }
    at = 4991; l3633 = K[1264];
    at = 4992; l3634 = rt.gt(l3595, l3633);
    at = 4993; if (rt.truthy(l3634)) {
      at = 4994; l3636 = K[1265];
      at = 4995; l3635 = l3636;
    } else {
      at = 4996; l3637 = K[1266];
      at = 4997; l3635 = l3637;
    }
    at = 4998; l3638 = K[1267];
    at = 4999; l3639 = rt.sub(l3595, l3638);
    at = 5000; l3640 = K[1268];
    at = 5001; l3641 = rt.max(l3640, l3639);
    at = 5002; l3642 = rt.array_slice(l3630, l3635, l3641);
    at = 5003; if (rt.truthy(l3588)) {
      at = 5004; l3644 = field(rt, l3591, "color");
      at = 5005; l3643 = l3644;
    } else {
      at = 5006; l3645 = K[1269];
      at = 5007; l3643 = l3645;
    }
    at = 5008; l3646 = K[1270];
    at = 5009; l3647 = rt.sequence_get(l3646, l3643);
    at = 5010; if (rt.truthy(l3588)) {
      at = 5011; l3649 = field(rt, l3591, "width");
      at = 5012; l3648 = l3649;
    } else {
      at = 5013; l3650 = K[1271];
      at = 5014; l3648 = l3650;
    }
    at = 5015; markSet(rt, true, "line", {from: l3629, to: l3642, color: l3647, width: l3648});
    at = 5016; l3651 = field(rt, l15, "strokes");
    at = 5017; l3652 = rt.sequence_length(l3651);
    at = 5018; l3653 = K[1272];
    at = 5019; l3654 = rt.lt(l3653, l3652);
    at = 5020; if (rt.truthy(l3654)) {
      at = 5021; l3655 = field(rt, l15, "strokes");
      at = 5022; l3656 = K[1273];
      at = 5023; l3657 = rt.sequence_get(l3655, l3656);
    }
    at = 5024; if (rt.truthy(l3654)) {
      at = 5025; l3659 = field(rt, l3657, "points");
      at = 5026; l3658 = l3659;
    } else {
      at = 5027; l3660 = K[1274];
      at = 5028; l3658 = l3660;
    }
    at = 5029; l3661 = rt.array_count(l3658);
    at = 5030; l3662 = K[1275];
    at = 5031; l3663 = rt.gt(l3661, l3662);
    at = 5032; if (rt.truthy(l3663)) {
      at = 5033; if (rt.truthy(l3654)) {
        at = 5034; l3666 = field(rt, l3657, "points");
        at = 5035; l3665 = l3666;
      } else {
        at = 5036; l3667 = K[1276];
        at = 5037; l3665 = l3667;
      }
      at = 5038; l3668 = K[1277];
      at = 5039; l3669 = rt.array_nth(l3665, l3668);
      at = 5040; l3664 = l3669;
    } else {
      at = 5041; l3670 = K[1278];
      at = 5042; l3664 = l3670;
    }
    at = 5043; if (rt.truthy(l3654)) {
      at = 5044; l3672 = field(rt, l3657, "width");
      at = 5045; l3671 = l3672;
    } else {
      at = 5046; l3673 = K[1279];
      at = 5047; l3671 = l3673;
    }
    at = 5048; l3674 = K[1280];
    at = 5049; l3675 = rt.int_div(l3671, l3674);
    at = 5050; l3676 = K[1281];
    at = 5051; l3677 = rt.max(l3676, l3675);
    at = 5052; l3678 = K[1282];
    at = 5053; l3679 = rt.gt(l3661, l3678);
    at = 5054; if (rt.truthy(l3679)) {
      at = 5055; if (rt.truthy(l3654)) {
        at = 5056; l3682 = field(rt, l3657, "color");
        at = 5057; l3681 = l3682;
      } else {
        at = 5058; l3683 = K[1283];
        at = 5059; l3681 = l3683;
      }
      at = 5060; l3684 = K[1284];
      at = 5061; l3685 = rt.sequence_get(l3684, l3681);
      at = 5062; l3680 = l3685;
    } else {
      at = 5063; l3686 = K[1285];
      at = 5064; l3680 = l3686;
    }
    at = 5065; markSet(rt, false, "circle", {at: l3664, radius: l3677, fill: l3680});
    at = 5066; if (rt.truthy(l3654)) {
      at = 5067; l3688 = field(rt, l3657, "points");
      at = 5068; l3687 = l3688;
    } else {
      at = 5069; l3689 = K[1286];
      at = 5070; l3687 = l3689;
    }
    at = 5071; l3690 = K[1287];
    at = 5072; l3691 = rt.sub(l3661, l3690);
    at = 5073; l3692 = K[1288];
    at = 5074; l3693 = rt.max(l3692, l3691);
    at = 5075; l3694 = K[1289];
    at = 5076; l3695 = rt.array_slice(l3687, l3694, l3693);
    at = 5077; if (rt.truthy(l3654)) {
      at = 5078; l3697 = field(rt, l3657, "points");
      at = 5079; l3696 = l3697;
    } else {
      at = 5080; l3698 = K[1290];
      at = 5081; l3696 = l3698;
    }
    at = 5082; l3699 = K[1291];
    at = 5083; l3700 = rt.gt(l3661, l3699);
    at = 5084; if (rt.truthy(l3700)) {
      at = 5085; l3702 = K[1292];
      at = 5086; l3701 = l3702;
    } else {
      at = 5087; l3703 = K[1293];
      at = 5088; l3701 = l3703;
    }
    at = 5089; l3704 = K[1294];
    at = 5090; l3705 = rt.sub(l3661, l3704);
    at = 5091; l3706 = K[1295];
    at = 5092; l3707 = rt.max(l3706, l3705);
    at = 5093; l3708 = rt.array_slice(l3696, l3701, l3707);
    at = 5094; if (rt.truthy(l3654)) {
      at = 5095; l3710 = field(rt, l3657, "color");
      at = 5096; l3709 = l3710;
    } else {
      at = 5097; l3711 = K[1296];
      at = 5098; l3709 = l3711;
    }
    at = 5099; l3712 = K[1297];
    at = 5100; l3713 = rt.sequence_get(l3712, l3709);
    at = 5101; if (rt.truthy(l3654)) {
      at = 5102; l3715 = field(rt, l3657, "width");
      at = 5103; l3714 = l3715;
    } else {
      at = 5104; l3716 = K[1298];
      at = 5105; l3714 = l3716;
    }
    at = 5106; markSet(rt, true, "line", {from: l3695, to: l3708, color: l3713, width: l3714});
    at = 5107; l3717 = field(rt, l15, "strokes");
    at = 5108; l3718 = rt.sequence_length(l3717);
    at = 5109; l3719 = K[1299];
    at = 5110; l3720 = rt.lt(l3719, l3718);
    at = 5111; if (rt.truthy(l3720)) {
      at = 5112; l3721 = field(rt, l15, "strokes");
      at = 5113; l3722 = K[1300];
      at = 5114; l3723 = rt.sequence_get(l3721, l3722);
    }
    at = 5115; if (rt.truthy(l3720)) {
      at = 5116; l3725 = field(rt, l3723, "points");
      at = 5117; l3724 = l3725;
    } else {
      at = 5118; l3726 = K[1301];
      at = 5119; l3724 = l3726;
    }
    at = 5120; l3727 = rt.array_count(l3724);
    at = 5121; l3728 = K[1302];
    at = 5122; l3729 = rt.gt(l3727, l3728);
    at = 5123; if (rt.truthy(l3729)) {
      at = 5124; if (rt.truthy(l3720)) {
        at = 5125; l3732 = field(rt, l3723, "points");
        at = 5126; l3731 = l3732;
      } else {
        at = 5127; l3733 = K[1303];
        at = 5128; l3731 = l3733;
      }
      at = 5129; l3734 = K[1304];
      at = 5130; l3735 = rt.array_nth(l3731, l3734);
      at = 5131; l3730 = l3735;
    } else {
      at = 5132; l3736 = K[1305];
      at = 5133; l3730 = l3736;
    }
    at = 5134; if (rt.truthy(l3720)) {
      at = 5135; l3738 = field(rt, l3723, "width");
      at = 5136; l3737 = l3738;
    } else {
      at = 5137; l3739 = K[1306];
      at = 5138; l3737 = l3739;
    }
    at = 5139; l3740 = K[1307];
    at = 5140; l3741 = rt.int_div(l3737, l3740);
    at = 5141; l3742 = K[1308];
    at = 5142; l3743 = rt.max(l3742, l3741);
    at = 5143; l3744 = K[1309];
    at = 5144; l3745 = rt.gt(l3727, l3744);
    at = 5145; if (rt.truthy(l3745)) {
      at = 5146; if (rt.truthy(l3720)) {
        at = 5147; l3748 = field(rt, l3723, "color");
        at = 5148; l3747 = l3748;
      } else {
        at = 5149; l3749 = K[1310];
        at = 5150; l3747 = l3749;
      }
      at = 5151; l3750 = K[1311];
      at = 5152; l3751 = rt.sequence_get(l3750, l3747);
      at = 5153; l3746 = l3751;
    } else {
      at = 5154; l3752 = K[1312];
      at = 5155; l3746 = l3752;
    }
    at = 5156; markSet(rt, false, "circle", {at: l3730, radius: l3743, fill: l3746});
    at = 5157; if (rt.truthy(l3720)) {
      at = 5158; l3754 = field(rt, l3723, "points");
      at = 5159; l3753 = l3754;
    } else {
      at = 5160; l3755 = K[1313];
      at = 5161; l3753 = l3755;
    }
    at = 5162; l3756 = K[1314];
    at = 5163; l3757 = rt.sub(l3727, l3756);
    at = 5164; l3758 = K[1315];
    at = 5165; l3759 = rt.max(l3758, l3757);
    at = 5166; l3760 = K[1316];
    at = 5167; l3761 = rt.array_slice(l3753, l3760, l3759);
    at = 5168; if (rt.truthy(l3720)) {
      at = 5169; l3763 = field(rt, l3723, "points");
      at = 5170; l3762 = l3763;
    } else {
      at = 5171; l3764 = K[1317];
      at = 5172; l3762 = l3764;
    }
    at = 5173; l3765 = K[1318];
    at = 5174; l3766 = rt.gt(l3727, l3765);
    at = 5175; if (rt.truthy(l3766)) {
      at = 5176; l3768 = K[1319];
      at = 5177; l3767 = l3768;
    } else {
      at = 5178; l3769 = K[1320];
      at = 5179; l3767 = l3769;
    }
    at = 5180; l3770 = K[1321];
    at = 5181; l3771 = rt.sub(l3727, l3770);
    at = 5182; l3772 = K[1322];
    at = 5183; l3773 = rt.max(l3772, l3771);
    at = 5184; l3774 = rt.array_slice(l3762, l3767, l3773);
    at = 5185; if (rt.truthy(l3720)) {
      at = 5186; l3776 = field(rt, l3723, "color");
      at = 5187; l3775 = l3776;
    } else {
      at = 5188; l3777 = K[1323];
      at = 5189; l3775 = l3777;
    }
    at = 5190; l3778 = K[1324];
    at = 5191; l3779 = rt.sequence_get(l3778, l3775);
    at = 5192; if (rt.truthy(l3720)) {
      at = 5193; l3781 = field(rt, l3723, "width");
      at = 5194; l3780 = l3781;
    } else {
      at = 5195; l3782 = K[1325];
      at = 5196; l3780 = l3782;
    }
    at = 5197; markSet(rt, true, "line", {from: l3761, to: l3774, color: l3779, width: l3780});
    at = 5198; l3783 = field(rt, l15, "strokes");
    at = 5199; l3784 = rt.sequence_length(l3783);
    at = 5200; l3785 = K[1326];
    at = 5201; l3786 = rt.lt(l3785, l3784);
    at = 5202; if (rt.truthy(l3786)) {
      at = 5203; l3787 = field(rt, l15, "strokes");
      at = 5204; l3788 = K[1327];
      at = 5205; l3789 = rt.sequence_get(l3787, l3788);
    }
    at = 5206; if (rt.truthy(l3786)) {
      at = 5207; l3791 = field(rt, l3789, "points");
      at = 5208; l3790 = l3791;
    } else {
      at = 5209; l3792 = K[1328];
      at = 5210; l3790 = l3792;
    }
    at = 5211; l3793 = rt.array_count(l3790);
    at = 5212; l3794 = K[1329];
    at = 5213; l3795 = rt.gt(l3793, l3794);
    at = 5214; if (rt.truthy(l3795)) {
      at = 5215; if (rt.truthy(l3786)) {
        at = 5216; l3798 = field(rt, l3789, "points");
        at = 5217; l3797 = l3798;
      } else {
        at = 5218; l3799 = K[1330];
        at = 5219; l3797 = l3799;
      }
      at = 5220; l3800 = K[1331];
      at = 5221; l3801 = rt.array_nth(l3797, l3800);
      at = 5222; l3796 = l3801;
    } else {
      at = 5223; l3802 = K[1332];
      at = 5224; l3796 = l3802;
    }
    at = 5225; if (rt.truthy(l3786)) {
      at = 5226; l3804 = field(rt, l3789, "width");
      at = 5227; l3803 = l3804;
    } else {
      at = 5228; l3805 = K[1333];
      at = 5229; l3803 = l3805;
    }
    at = 5230; l3806 = K[1334];
    at = 5231; l3807 = rt.int_div(l3803, l3806);
    at = 5232; l3808 = K[1335];
    at = 5233; l3809 = rt.max(l3808, l3807);
    at = 5234; l3810 = K[1336];
    at = 5235; l3811 = rt.gt(l3793, l3810);
    at = 5236; if (rt.truthy(l3811)) {
      at = 5237; if (rt.truthy(l3786)) {
        at = 5238; l3814 = field(rt, l3789, "color");
        at = 5239; l3813 = l3814;
      } else {
        at = 5240; l3815 = K[1337];
        at = 5241; l3813 = l3815;
      }
      at = 5242; l3816 = K[1338];
      at = 5243; l3817 = rt.sequence_get(l3816, l3813);
      at = 5244; l3812 = l3817;
    } else {
      at = 5245; l3818 = K[1339];
      at = 5246; l3812 = l3818;
    }
    at = 5247; markSet(rt, false, "circle", {at: l3796, radius: l3809, fill: l3812});
    at = 5248; if (rt.truthy(l3786)) {
      at = 5249; l3820 = field(rt, l3789, "points");
      at = 5250; l3819 = l3820;
    } else {
      at = 5251; l3821 = K[1340];
      at = 5252; l3819 = l3821;
    }
    at = 5253; l3822 = K[1341];
    at = 5254; l3823 = rt.sub(l3793, l3822);
    at = 5255; l3824 = K[1342];
    at = 5256; l3825 = rt.max(l3824, l3823);
    at = 5257; l3826 = K[1343];
    at = 5258; l3827 = rt.array_slice(l3819, l3826, l3825);
    at = 5259; if (rt.truthy(l3786)) {
      at = 5260; l3829 = field(rt, l3789, "points");
      at = 5261; l3828 = l3829;
    } else {
      at = 5262; l3830 = K[1344];
      at = 5263; l3828 = l3830;
    }
    at = 5264; l3831 = K[1345];
    at = 5265; l3832 = rt.gt(l3793, l3831);
    at = 5266; if (rt.truthy(l3832)) {
      at = 5267; l3834 = K[1346];
      at = 5268; l3833 = l3834;
    } else {
      at = 5269; l3835 = K[1347];
      at = 5270; l3833 = l3835;
    }
    at = 5271; l3836 = K[1348];
    at = 5272; l3837 = rt.sub(l3793, l3836);
    at = 5273; l3838 = K[1349];
    at = 5274; l3839 = rt.max(l3838, l3837);
    at = 5275; l3840 = rt.array_slice(l3828, l3833, l3839);
    at = 5276; if (rt.truthy(l3786)) {
      at = 5277; l3842 = field(rt, l3789, "color");
      at = 5278; l3841 = l3842;
    } else {
      at = 5279; l3843 = K[1350];
      at = 5280; l3841 = l3843;
    }
    at = 5281; l3844 = K[1351];
    at = 5282; l3845 = rt.sequence_get(l3844, l3841);
    at = 5283; if (rt.truthy(l3786)) {
      at = 5284; l3847 = field(rt, l3789, "width");
      at = 5285; l3846 = l3847;
    } else {
      at = 5286; l3848 = K[1352];
      at = 5287; l3846 = l3848;
    }
    at = 5288; markSet(rt, true, "line", {from: l3827, to: l3840, color: l3845, width: l3846});
    at = 5289; l3849 = field(rt, l15, "strokes");
    at = 5290; l3850 = rt.sequence_length(l3849);
    at = 5291; l3851 = K[1353];
    at = 5292; l3852 = rt.lt(l3851, l3850);
    at = 5293; if (rt.truthy(l3852)) {
      at = 5294; l3853 = field(rt, l15, "strokes");
      at = 5295; l3854 = K[1354];
      at = 5296; l3855 = rt.sequence_get(l3853, l3854);
    }
    at = 5297; if (rt.truthy(l3852)) {
      at = 5298; l3857 = field(rt, l3855, "points");
      at = 5299; l3856 = l3857;
    } else {
      at = 5300; l3858 = K[1355];
      at = 5301; l3856 = l3858;
    }
    at = 5302; l3859 = rt.array_count(l3856);
    at = 5303; l3860 = K[1356];
    at = 5304; l3861 = rt.gt(l3859, l3860);
    at = 5305; if (rt.truthy(l3861)) {
      at = 5306; if (rt.truthy(l3852)) {
        at = 5307; l3864 = field(rt, l3855, "points");
        at = 5308; l3863 = l3864;
      } else {
        at = 5309; l3865 = K[1357];
        at = 5310; l3863 = l3865;
      }
      at = 5311; l3866 = K[1358];
      at = 5312; l3867 = rt.array_nth(l3863, l3866);
      at = 5313; l3862 = l3867;
    } else {
      at = 5314; l3868 = K[1359];
      at = 5315; l3862 = l3868;
    }
    at = 5316; if (rt.truthy(l3852)) {
      at = 5317; l3870 = field(rt, l3855, "width");
      at = 5318; l3869 = l3870;
    } else {
      at = 5319; l3871 = K[1360];
      at = 5320; l3869 = l3871;
    }
    at = 5321; l3872 = K[1361];
    at = 5322; l3873 = rt.int_div(l3869, l3872);
    at = 5323; l3874 = K[1362];
    at = 5324; l3875 = rt.max(l3874, l3873);
    at = 5325; l3876 = K[1363];
    at = 5326; l3877 = rt.gt(l3859, l3876);
    at = 5327; if (rt.truthy(l3877)) {
      at = 5328; if (rt.truthy(l3852)) {
        at = 5329; l3880 = field(rt, l3855, "color");
        at = 5330; l3879 = l3880;
      } else {
        at = 5331; l3881 = K[1364];
        at = 5332; l3879 = l3881;
      }
      at = 5333; l3882 = K[1365];
      at = 5334; l3883 = rt.sequence_get(l3882, l3879);
      at = 5335; l3878 = l3883;
    } else {
      at = 5336; l3884 = K[1366];
      at = 5337; l3878 = l3884;
    }
    at = 5338; markSet(rt, false, "circle", {at: l3862, radius: l3875, fill: l3878});
    at = 5339; if (rt.truthy(l3852)) {
      at = 5340; l3886 = field(rt, l3855, "points");
      at = 5341; l3885 = l3886;
    } else {
      at = 5342; l3887 = K[1367];
      at = 5343; l3885 = l3887;
    }
    at = 5344; l3888 = K[1368];
    at = 5345; l3889 = rt.sub(l3859, l3888);
    at = 5346; l3890 = K[1369];
    at = 5347; l3891 = rt.max(l3890, l3889);
    at = 5348; l3892 = K[1370];
    at = 5349; l3893 = rt.array_slice(l3885, l3892, l3891);
    at = 5350; if (rt.truthy(l3852)) {
      at = 5351; l3895 = field(rt, l3855, "points");
      at = 5352; l3894 = l3895;
    } else {
      at = 5353; l3896 = K[1371];
      at = 5354; l3894 = l3896;
    }
    at = 5355; l3897 = K[1372];
    at = 5356; l3898 = rt.gt(l3859, l3897);
    at = 5357; if (rt.truthy(l3898)) {
      at = 5358; l3900 = K[1373];
      at = 5359; l3899 = l3900;
    } else {
      at = 5360; l3901 = K[1374];
      at = 5361; l3899 = l3901;
    }
    at = 5362; l3902 = K[1375];
    at = 5363; l3903 = rt.sub(l3859, l3902);
    at = 5364; l3904 = K[1376];
    at = 5365; l3905 = rt.max(l3904, l3903);
    at = 5366; l3906 = rt.array_slice(l3894, l3899, l3905);
    at = 5367; if (rt.truthy(l3852)) {
      at = 5368; l3908 = field(rt, l3855, "color");
      at = 5369; l3907 = l3908;
    } else {
      at = 5370; l3909 = K[1377];
      at = 5371; l3907 = l3909;
    }
    at = 5372; l3910 = K[1378];
    at = 5373; l3911 = rt.sequence_get(l3910, l3907);
    at = 5374; if (rt.truthy(l3852)) {
      at = 5375; l3913 = field(rt, l3855, "width");
      at = 5376; l3912 = l3913;
    } else {
      at = 5377; l3914 = K[1379];
      at = 5378; l3912 = l3914;
    }
    at = 5379; markSet(rt, true, "line", {from: l3893, to: l3906, color: l3911, width: l3912});
    at = 5380; l3915 = field(rt, l15, "strokes");
    at = 5381; l3916 = rt.sequence_length(l3915);
    at = 5382; l3917 = K[1380];
    at = 5383; l3918 = rt.lt(l3917, l3916);
    at = 5384; if (rt.truthy(l3918)) {
      at = 5385; l3919 = field(rt, l15, "strokes");
      at = 5386; l3920 = K[1381];
      at = 5387; l3921 = rt.sequence_get(l3919, l3920);
    }
    at = 5388; if (rt.truthy(l3918)) {
      at = 5389; l3923 = field(rt, l3921, "points");
      at = 5390; l3922 = l3923;
    } else {
      at = 5391; l3924 = K[1382];
      at = 5392; l3922 = l3924;
    }
    at = 5393; l3925 = rt.array_count(l3922);
    at = 5394; l3926 = K[1383];
    at = 5395; l3927 = rt.gt(l3925, l3926);
    at = 5396; if (rt.truthy(l3927)) {
      at = 5397; if (rt.truthy(l3918)) {
        at = 5398; l3930 = field(rt, l3921, "points");
        at = 5399; l3929 = l3930;
      } else {
        at = 5400; l3931 = K[1384];
        at = 5401; l3929 = l3931;
      }
      at = 5402; l3932 = K[1385];
      at = 5403; l3933 = rt.array_nth(l3929, l3932);
      at = 5404; l3928 = l3933;
    } else {
      at = 5405; l3934 = K[1386];
      at = 5406; l3928 = l3934;
    }
    at = 5407; if (rt.truthy(l3918)) {
      at = 5408; l3936 = field(rt, l3921, "width");
      at = 5409; l3935 = l3936;
    } else {
      at = 5410; l3937 = K[1387];
      at = 5411; l3935 = l3937;
    }
    at = 5412; l3938 = K[1388];
    at = 5413; l3939 = rt.int_div(l3935, l3938);
    at = 5414; l3940 = K[1389];
    at = 5415; l3941 = rt.max(l3940, l3939);
    at = 5416; l3942 = K[1390];
    at = 5417; l3943 = rt.gt(l3925, l3942);
    at = 5418; if (rt.truthy(l3943)) {
      at = 5419; if (rt.truthy(l3918)) {
        at = 5420; l3946 = field(rt, l3921, "color");
        at = 5421; l3945 = l3946;
      } else {
        at = 5422; l3947 = K[1391];
        at = 5423; l3945 = l3947;
      }
      at = 5424; l3948 = K[1392];
      at = 5425; l3949 = rt.sequence_get(l3948, l3945);
      at = 5426; l3944 = l3949;
    } else {
      at = 5427; l3950 = K[1393];
      at = 5428; l3944 = l3950;
    }
    at = 5429; markSet(rt, false, "circle", {at: l3928, radius: l3941, fill: l3944});
    at = 5430; if (rt.truthy(l3918)) {
      at = 5431; l3952 = field(rt, l3921, "points");
      at = 5432; l3951 = l3952;
    } else {
      at = 5433; l3953 = K[1394];
      at = 5434; l3951 = l3953;
    }
    at = 5435; l3954 = K[1395];
    at = 5436; l3955 = rt.sub(l3925, l3954);
    at = 5437; l3956 = K[1396];
    at = 5438; l3957 = rt.max(l3956, l3955);
    at = 5439; l3958 = K[1397];
    at = 5440; l3959 = rt.array_slice(l3951, l3958, l3957);
    at = 5441; if (rt.truthy(l3918)) {
      at = 5442; l3961 = field(rt, l3921, "points");
      at = 5443; l3960 = l3961;
    } else {
      at = 5444; l3962 = K[1398];
      at = 5445; l3960 = l3962;
    }
    at = 5446; l3963 = K[1399];
    at = 5447; l3964 = rt.gt(l3925, l3963);
    at = 5448; if (rt.truthy(l3964)) {
      at = 5449; l3966 = K[1400];
      at = 5450; l3965 = l3966;
    } else {
      at = 5451; l3967 = K[1401];
      at = 5452; l3965 = l3967;
    }
    at = 5453; l3968 = K[1402];
    at = 5454; l3969 = rt.sub(l3925, l3968);
    at = 5455; l3970 = K[1403];
    at = 5456; l3971 = rt.max(l3970, l3969);
    at = 5457; l3972 = rt.array_slice(l3960, l3965, l3971);
    at = 5458; if (rt.truthy(l3918)) {
      at = 5459; l3974 = field(rt, l3921, "color");
      at = 5460; l3973 = l3974;
    } else {
      at = 5461; l3975 = K[1404];
      at = 5462; l3973 = l3975;
    }
    at = 5463; l3976 = K[1405];
    at = 5464; l3977 = rt.sequence_get(l3976, l3973);
    at = 5465; if (rt.truthy(l3918)) {
      at = 5466; l3979 = field(rt, l3921, "width");
      at = 5467; l3978 = l3979;
    } else {
      at = 5468; l3980 = K[1406];
      at = 5469; l3978 = l3980;
    }
    at = 5470; markSet(rt, true, "line", {from: l3959, to: l3972, color: l3977, width: l3978});
    at = 5471; l3981 = field(rt, l15, "strokes");
    at = 5472; l3982 = rt.sequence_length(l3981);
    at = 5473; l3983 = K[1407];
    at = 5474; l3984 = rt.lt(l3983, l3982);
    at = 5475; if (rt.truthy(l3984)) {
      at = 5476; l3985 = field(rt, l15, "strokes");
      at = 5477; l3986 = K[1408];
      at = 5478; l3987 = rt.sequence_get(l3985, l3986);
    }
    at = 5479; if (rt.truthy(l3984)) {
      at = 5480; l3989 = field(rt, l3987, "points");
      at = 5481; l3988 = l3989;
    } else {
      at = 5482; l3990 = K[1409];
      at = 5483; l3988 = l3990;
    }
    at = 5484; l3991 = rt.array_count(l3988);
    at = 5485; l3992 = K[1410];
    at = 5486; l3993 = rt.gt(l3991, l3992);
    at = 5487; if (rt.truthy(l3993)) {
      at = 5488; if (rt.truthy(l3984)) {
        at = 5489; l3996 = field(rt, l3987, "points");
        at = 5490; l3995 = l3996;
      } else {
        at = 5491; l3997 = K[1411];
        at = 5492; l3995 = l3997;
      }
      at = 5493; l3998 = K[1412];
      at = 5494; l3999 = rt.array_nth(l3995, l3998);
      at = 5495; l3994 = l3999;
    } else {
      at = 5496; l4000 = K[1413];
      at = 5497; l3994 = l4000;
    }
    at = 5498; if (rt.truthy(l3984)) {
      at = 5499; l4002 = field(rt, l3987, "width");
      at = 5500; l4001 = l4002;
    } else {
      at = 5501; l4003 = K[1414];
      at = 5502; l4001 = l4003;
    }
    at = 5503; l4004 = K[1415];
    at = 5504; l4005 = rt.int_div(l4001, l4004);
    at = 5505; l4006 = K[1416];
    at = 5506; l4007 = rt.max(l4006, l4005);
    at = 5507; l4008 = K[1417];
    at = 5508; l4009 = rt.gt(l3991, l4008);
    at = 5509; if (rt.truthy(l4009)) {
      at = 5510; if (rt.truthy(l3984)) {
        at = 5511; l4012 = field(rt, l3987, "color");
        at = 5512; l4011 = l4012;
      } else {
        at = 5513; l4013 = K[1418];
        at = 5514; l4011 = l4013;
      }
      at = 5515; l4014 = K[1419];
      at = 5516; l4015 = rt.sequence_get(l4014, l4011);
      at = 5517; l4010 = l4015;
    } else {
      at = 5518; l4016 = K[1420];
      at = 5519; l4010 = l4016;
    }
    at = 5520; markSet(rt, false, "circle", {at: l3994, radius: l4007, fill: l4010});
    at = 5521; if (rt.truthy(l3984)) {
      at = 5522; l4018 = field(rt, l3987, "points");
      at = 5523; l4017 = l4018;
    } else {
      at = 5524; l4019 = K[1421];
      at = 5525; l4017 = l4019;
    }
    at = 5526; l4020 = K[1422];
    at = 5527; l4021 = rt.sub(l3991, l4020);
    at = 5528; l4022 = K[1423];
    at = 5529; l4023 = rt.max(l4022, l4021);
    at = 5530; l4024 = K[1424];
    at = 5531; l4025 = rt.array_slice(l4017, l4024, l4023);
    at = 5532; if (rt.truthy(l3984)) {
      at = 5533; l4027 = field(rt, l3987, "points");
      at = 5534; l4026 = l4027;
    } else {
      at = 5535; l4028 = K[1425];
      at = 5536; l4026 = l4028;
    }
    at = 5537; l4029 = K[1426];
    at = 5538; l4030 = rt.gt(l3991, l4029);
    at = 5539; if (rt.truthy(l4030)) {
      at = 5540; l4032 = K[1427];
      at = 5541; l4031 = l4032;
    } else {
      at = 5542; l4033 = K[1428];
      at = 5543; l4031 = l4033;
    }
    at = 5544; l4034 = K[1429];
    at = 5545; l4035 = rt.sub(l3991, l4034);
    at = 5546; l4036 = K[1430];
    at = 5547; l4037 = rt.max(l4036, l4035);
    at = 5548; l4038 = rt.array_slice(l4026, l4031, l4037);
    at = 5549; if (rt.truthy(l3984)) {
      at = 5550; l4040 = field(rt, l3987, "color");
      at = 5551; l4039 = l4040;
    } else {
      at = 5552; l4041 = K[1431];
      at = 5553; l4039 = l4041;
    }
    at = 5554; l4042 = K[1432];
    at = 5555; l4043 = rt.sequence_get(l4042, l4039);
    at = 5556; if (rt.truthy(l3984)) {
      at = 5557; l4045 = field(rt, l3987, "width");
      at = 5558; l4044 = l4045;
    } else {
      at = 5559; l4046 = K[1433];
      at = 5560; l4044 = l4046;
    }
    at = 5561; markSet(rt, true, "line", {from: l4025, to: l4038, color: l4043, width: l4044});
    at = 5562; l4047 = field(rt, l15, "strokes");
    at = 5563; l4048 = rt.sequence_length(l4047);
    at = 5564; l4049 = K[1434];
    at = 5565; l4050 = rt.lt(l4049, l4048);
    at = 5566; if (rt.truthy(l4050)) {
      at = 5567; l4051 = field(rt, l15, "strokes");
      at = 5568; l4052 = K[1435];
      at = 5569; l4053 = rt.sequence_get(l4051, l4052);
    }
    at = 5570; if (rt.truthy(l4050)) {
      at = 5571; l4055 = field(rt, l4053, "points");
      at = 5572; l4054 = l4055;
    } else {
      at = 5573; l4056 = K[1436];
      at = 5574; l4054 = l4056;
    }
    at = 5575; l4057 = rt.array_count(l4054);
    at = 5576; l4058 = K[1437];
    at = 5577; l4059 = rt.gt(l4057, l4058);
    at = 5578; if (rt.truthy(l4059)) {
      at = 5579; if (rt.truthy(l4050)) {
        at = 5580; l4062 = field(rt, l4053, "points");
        at = 5581; l4061 = l4062;
      } else {
        at = 5582; l4063 = K[1438];
        at = 5583; l4061 = l4063;
      }
      at = 5584; l4064 = K[1439];
      at = 5585; l4065 = rt.array_nth(l4061, l4064);
      at = 5586; l4060 = l4065;
    } else {
      at = 5587; l4066 = K[1440];
      at = 5588; l4060 = l4066;
    }
    at = 5589; if (rt.truthy(l4050)) {
      at = 5590; l4068 = field(rt, l4053, "width");
      at = 5591; l4067 = l4068;
    } else {
      at = 5592; l4069 = K[1441];
      at = 5593; l4067 = l4069;
    }
    at = 5594; l4070 = K[1442];
    at = 5595; l4071 = rt.int_div(l4067, l4070);
    at = 5596; l4072 = K[1443];
    at = 5597; l4073 = rt.max(l4072, l4071);
    at = 5598; l4074 = K[1444];
    at = 5599; l4075 = rt.gt(l4057, l4074);
    at = 5600; if (rt.truthy(l4075)) {
      at = 5601; if (rt.truthy(l4050)) {
        at = 5602; l4078 = field(rt, l4053, "color");
        at = 5603; l4077 = l4078;
      } else {
        at = 5604; l4079 = K[1445];
        at = 5605; l4077 = l4079;
      }
      at = 5606; l4080 = K[1446];
      at = 5607; l4081 = rt.sequence_get(l4080, l4077);
      at = 5608; l4076 = l4081;
    } else {
      at = 5609; l4082 = K[1447];
      at = 5610; l4076 = l4082;
    }
    at = 5611; markSet(rt, false, "circle", {at: l4060, radius: l4073, fill: l4076});
    at = 5612; if (rt.truthy(l4050)) {
      at = 5613; l4084 = field(rt, l4053, "points");
      at = 5614; l4083 = l4084;
    } else {
      at = 5615; l4085 = K[1448];
      at = 5616; l4083 = l4085;
    }
    at = 5617; l4086 = K[1449];
    at = 5618; l4087 = rt.sub(l4057, l4086);
    at = 5619; l4088 = K[1450];
    at = 5620; l4089 = rt.max(l4088, l4087);
    at = 5621; l4090 = K[1451];
    at = 5622; l4091 = rt.array_slice(l4083, l4090, l4089);
    at = 5623; if (rt.truthy(l4050)) {
      at = 5624; l4093 = field(rt, l4053, "points");
      at = 5625; l4092 = l4093;
    } else {
      at = 5626; l4094 = K[1452];
      at = 5627; l4092 = l4094;
    }
    at = 5628; l4095 = K[1453];
    at = 5629; l4096 = rt.gt(l4057, l4095);
    at = 5630; if (rt.truthy(l4096)) {
      at = 5631; l4098 = K[1454];
      at = 5632; l4097 = l4098;
    } else {
      at = 5633; l4099 = K[1455];
      at = 5634; l4097 = l4099;
    }
    at = 5635; l4100 = K[1456];
    at = 5636; l4101 = rt.sub(l4057, l4100);
    at = 5637; l4102 = K[1457];
    at = 5638; l4103 = rt.max(l4102, l4101);
    at = 5639; l4104 = rt.array_slice(l4092, l4097, l4103);
    at = 5640; if (rt.truthy(l4050)) {
      at = 5641; l4106 = field(rt, l4053, "color");
      at = 5642; l4105 = l4106;
    } else {
      at = 5643; l4107 = K[1458];
      at = 5644; l4105 = l4107;
    }
    at = 5645; l4108 = K[1459];
    at = 5646; l4109 = rt.sequence_get(l4108, l4105);
    at = 5647; if (rt.truthy(l4050)) {
      at = 5648; l4111 = field(rt, l4053, "width");
      at = 5649; l4110 = l4111;
    } else {
      at = 5650; l4112 = K[1460];
      at = 5651; l4110 = l4112;
    }
    at = 5652; markSet(rt, true, "line", {from: l4091, to: l4104, color: l4109, width: l4110});
    at = 5653; l4113 = field(rt, l15, "strokes");
    at = 5654; l4114 = rt.sequence_length(l4113);
    at = 5655; l4115 = K[1461];
    at = 5656; l4116 = rt.lt(l4115, l4114);
    at = 5657; if (rt.truthy(l4116)) {
      at = 5658; l4117 = field(rt, l15, "strokes");
      at = 5659; l4118 = K[1462];
      at = 5660; l4119 = rt.sequence_get(l4117, l4118);
    }
    at = 5661; if (rt.truthy(l4116)) {
      at = 5662; l4121 = field(rt, l4119, "points");
      at = 5663; l4120 = l4121;
    } else {
      at = 5664; l4122 = K[1463];
      at = 5665; l4120 = l4122;
    }
    at = 5666; l4123 = rt.array_count(l4120);
    at = 5667; l4124 = K[1464];
    at = 5668; l4125 = rt.gt(l4123, l4124);
    at = 5669; if (rt.truthy(l4125)) {
      at = 5670; if (rt.truthy(l4116)) {
        at = 5671; l4128 = field(rt, l4119, "points");
        at = 5672; l4127 = l4128;
      } else {
        at = 5673; l4129 = K[1465];
        at = 5674; l4127 = l4129;
      }
      at = 5675; l4130 = K[1466];
      at = 5676; l4131 = rt.array_nth(l4127, l4130);
      at = 5677; l4126 = l4131;
    } else {
      at = 5678; l4132 = K[1467];
      at = 5679; l4126 = l4132;
    }
    at = 5680; if (rt.truthy(l4116)) {
      at = 5681; l4134 = field(rt, l4119, "width");
      at = 5682; l4133 = l4134;
    } else {
      at = 5683; l4135 = K[1468];
      at = 5684; l4133 = l4135;
    }
    at = 5685; l4136 = K[1469];
    at = 5686; l4137 = rt.int_div(l4133, l4136);
    at = 5687; l4138 = K[1470];
    at = 5688; l4139 = rt.max(l4138, l4137);
    at = 5689; l4140 = K[1471];
    at = 5690; l4141 = rt.gt(l4123, l4140);
    at = 5691; if (rt.truthy(l4141)) {
      at = 5692; if (rt.truthy(l4116)) {
        at = 5693; l4144 = field(rt, l4119, "color");
        at = 5694; l4143 = l4144;
      } else {
        at = 5695; l4145 = K[1472];
        at = 5696; l4143 = l4145;
      }
      at = 5697; l4146 = K[1473];
      at = 5698; l4147 = rt.sequence_get(l4146, l4143);
      at = 5699; l4142 = l4147;
    } else {
      at = 5700; l4148 = K[1474];
      at = 5701; l4142 = l4148;
    }
    at = 5702; markSet(rt, false, "circle", {at: l4126, radius: l4139, fill: l4142});
    at = 5703; if (rt.truthy(l4116)) {
      at = 5704; l4150 = field(rt, l4119, "points");
      at = 5705; l4149 = l4150;
    } else {
      at = 5706; l4151 = K[1475];
      at = 5707; l4149 = l4151;
    }
    at = 5708; l4152 = K[1476];
    at = 5709; l4153 = rt.sub(l4123, l4152);
    at = 5710; l4154 = K[1477];
    at = 5711; l4155 = rt.max(l4154, l4153);
    at = 5712; l4156 = K[1478];
    at = 5713; l4157 = rt.array_slice(l4149, l4156, l4155);
    at = 5714; if (rt.truthy(l4116)) {
      at = 5715; l4159 = field(rt, l4119, "points");
      at = 5716; l4158 = l4159;
    } else {
      at = 5717; l4160 = K[1479];
      at = 5718; l4158 = l4160;
    }
    at = 5719; l4161 = K[1480];
    at = 5720; l4162 = rt.gt(l4123, l4161);
    at = 5721; if (rt.truthy(l4162)) {
      at = 5722; l4164 = K[1481];
      at = 5723; l4163 = l4164;
    } else {
      at = 5724; l4165 = K[1482];
      at = 5725; l4163 = l4165;
    }
    at = 5726; l4166 = K[1483];
    at = 5727; l4167 = rt.sub(l4123, l4166);
    at = 5728; l4168 = K[1484];
    at = 5729; l4169 = rt.max(l4168, l4167);
    at = 5730; l4170 = rt.array_slice(l4158, l4163, l4169);
    at = 5731; if (rt.truthy(l4116)) {
      at = 5732; l4172 = field(rt, l4119, "color");
      at = 5733; l4171 = l4172;
    } else {
      at = 5734; l4173 = K[1485];
      at = 5735; l4171 = l4173;
    }
    at = 5736; l4174 = K[1486];
    at = 5737; l4175 = rt.sequence_get(l4174, l4171);
    at = 5738; if (rt.truthy(l4116)) {
      at = 5739; l4177 = field(rt, l4119, "width");
      at = 5740; l4176 = l4177;
    } else {
      at = 5741; l4178 = K[1487];
      at = 5742; l4176 = l4178;
    }
    at = 5743; markSet(rt, true, "line", {from: l4157, to: l4170, color: l4175, width: l4176});
    at = 5744; l4179 = field(rt, l15, "strokes");
    at = 5745; l4180 = rt.sequence_length(l4179);
    at = 5746; l4181 = K[1488];
    at = 5747; l4182 = rt.lt(l4181, l4180);
    at = 5748; if (rt.truthy(l4182)) {
      at = 5749; l4183 = field(rt, l15, "strokes");
      at = 5750; l4184 = K[1489];
      at = 5751; l4185 = rt.sequence_get(l4183, l4184);
    }
    at = 5752; if (rt.truthy(l4182)) {
      at = 5753; l4187 = field(rt, l4185, "points");
      at = 5754; l4186 = l4187;
    } else {
      at = 5755; l4188 = K[1490];
      at = 5756; l4186 = l4188;
    }
    at = 5757; l4189 = rt.array_count(l4186);
    at = 5758; l4190 = K[1491];
    at = 5759; l4191 = rt.gt(l4189, l4190);
    at = 5760; if (rt.truthy(l4191)) {
      at = 5761; if (rt.truthy(l4182)) {
        at = 5762; l4194 = field(rt, l4185, "points");
        at = 5763; l4193 = l4194;
      } else {
        at = 5764; l4195 = K[1492];
        at = 5765; l4193 = l4195;
      }
      at = 5766; l4196 = K[1493];
      at = 5767; l4197 = rt.array_nth(l4193, l4196);
      at = 5768; l4192 = l4197;
    } else {
      at = 5769; l4198 = K[1494];
      at = 5770; l4192 = l4198;
    }
    at = 5771; if (rt.truthy(l4182)) {
      at = 5772; l4200 = field(rt, l4185, "width");
      at = 5773; l4199 = l4200;
    } else {
      at = 5774; l4201 = K[1495];
      at = 5775; l4199 = l4201;
    }
    at = 5776; l4202 = K[1496];
    at = 5777; l4203 = rt.int_div(l4199, l4202);
    at = 5778; l4204 = K[1497];
    at = 5779; l4205 = rt.max(l4204, l4203);
    at = 5780; l4206 = K[1498];
    at = 5781; l4207 = rt.gt(l4189, l4206);
    at = 5782; if (rt.truthy(l4207)) {
      at = 5783; if (rt.truthy(l4182)) {
        at = 5784; l4210 = field(rt, l4185, "color");
        at = 5785; l4209 = l4210;
      } else {
        at = 5786; l4211 = K[1499];
        at = 5787; l4209 = l4211;
      }
      at = 5788; l4212 = K[1500];
      at = 5789; l4213 = rt.sequence_get(l4212, l4209);
      at = 5790; l4208 = l4213;
    } else {
      at = 5791; l4214 = K[1501];
      at = 5792; l4208 = l4214;
    }
    at = 5793; markSet(rt, false, "circle", {at: l4192, radius: l4205, fill: l4208});
    at = 5794; if (rt.truthy(l4182)) {
      at = 5795; l4216 = field(rt, l4185, "points");
      at = 5796; l4215 = l4216;
    } else {
      at = 5797; l4217 = K[1502];
      at = 5798; l4215 = l4217;
    }
    at = 5799; l4218 = K[1503];
    at = 5800; l4219 = rt.sub(l4189, l4218);
    at = 5801; l4220 = K[1504];
    at = 5802; l4221 = rt.max(l4220, l4219);
    at = 5803; l4222 = K[1505];
    at = 5804; l4223 = rt.array_slice(l4215, l4222, l4221);
    at = 5805; if (rt.truthy(l4182)) {
      at = 5806; l4225 = field(rt, l4185, "points");
      at = 5807; l4224 = l4225;
    } else {
      at = 5808; l4226 = K[1506];
      at = 5809; l4224 = l4226;
    }
    at = 5810; l4227 = K[1507];
    at = 5811; l4228 = rt.gt(l4189, l4227);
    at = 5812; if (rt.truthy(l4228)) {
      at = 5813; l4230 = K[1508];
      at = 5814; l4229 = l4230;
    } else {
      at = 5815; l4231 = K[1509];
      at = 5816; l4229 = l4231;
    }
    at = 5817; l4232 = K[1510];
    at = 5818; l4233 = rt.sub(l4189, l4232);
    at = 5819; l4234 = K[1511];
    at = 5820; l4235 = rt.max(l4234, l4233);
    at = 5821; l4236 = rt.array_slice(l4224, l4229, l4235);
    at = 5822; if (rt.truthy(l4182)) {
      at = 5823; l4238 = field(rt, l4185, "color");
      at = 5824; l4237 = l4238;
    } else {
      at = 5825; l4239 = K[1512];
      at = 5826; l4237 = l4239;
    }
    at = 5827; l4240 = K[1513];
    at = 5828; l4241 = rt.sequence_get(l4240, l4237);
    at = 5829; if (rt.truthy(l4182)) {
      at = 5830; l4243 = field(rt, l4185, "width");
      at = 5831; l4242 = l4243;
    } else {
      at = 5832; l4244 = K[1514];
      at = 5833; l4242 = l4244;
    }
    at = 5834; markSet(rt, true, "line", {from: l4223, to: l4236, color: l4241, width: l4242});
    at = 5835; l4245 = field(rt, l15, "strokes");
    at = 5836; l4246 = rt.sequence_length(l4245);
    at = 5837; l4247 = K[1515];
    at = 5838; l4248 = rt.lt(l4247, l4246);
    at = 5839; if (rt.truthy(l4248)) {
      at = 5840; l4249 = field(rt, l15, "strokes");
      at = 5841; l4250 = K[1516];
      at = 5842; l4251 = rt.sequence_get(l4249, l4250);
    }
    at = 5843; if (rt.truthy(l4248)) {
      at = 5844; l4253 = field(rt, l4251, "points");
      at = 5845; l4252 = l4253;
    } else {
      at = 5846; l4254 = K[1517];
      at = 5847; l4252 = l4254;
    }
    at = 5848; l4255 = rt.array_count(l4252);
    at = 5849; l4256 = K[1518];
    at = 5850; l4257 = rt.gt(l4255, l4256);
    at = 5851; if (rt.truthy(l4257)) {
      at = 5852; if (rt.truthy(l4248)) {
        at = 5853; l4260 = field(rt, l4251, "points");
        at = 5854; l4259 = l4260;
      } else {
        at = 5855; l4261 = K[1519];
        at = 5856; l4259 = l4261;
      }
      at = 5857; l4262 = K[1520];
      at = 5858; l4263 = rt.array_nth(l4259, l4262);
      at = 5859; l4258 = l4263;
    } else {
      at = 5860; l4264 = K[1521];
      at = 5861; l4258 = l4264;
    }
    at = 5862; if (rt.truthy(l4248)) {
      at = 5863; l4266 = field(rt, l4251, "width");
      at = 5864; l4265 = l4266;
    } else {
      at = 5865; l4267 = K[1522];
      at = 5866; l4265 = l4267;
    }
    at = 5867; l4268 = K[1523];
    at = 5868; l4269 = rt.int_div(l4265, l4268);
    at = 5869; l4270 = K[1524];
    at = 5870; l4271 = rt.max(l4270, l4269);
    at = 5871; l4272 = K[1525];
    at = 5872; l4273 = rt.gt(l4255, l4272);
    at = 5873; if (rt.truthy(l4273)) {
      at = 5874; if (rt.truthy(l4248)) {
        at = 5875; l4276 = field(rt, l4251, "color");
        at = 5876; l4275 = l4276;
      } else {
        at = 5877; l4277 = K[1526];
        at = 5878; l4275 = l4277;
      }
      at = 5879; l4278 = K[1527];
      at = 5880; l4279 = rt.sequence_get(l4278, l4275);
      at = 5881; l4274 = l4279;
    } else {
      at = 5882; l4280 = K[1528];
      at = 5883; l4274 = l4280;
    }
    at = 5884; markSet(rt, false, "circle", {at: l4258, radius: l4271, fill: l4274});
    at = 5885; if (rt.truthy(l4248)) {
      at = 5886; l4282 = field(rt, l4251, "points");
      at = 5887; l4281 = l4282;
    } else {
      at = 5888; l4283 = K[1529];
      at = 5889; l4281 = l4283;
    }
    at = 5890; l4284 = K[1530];
    at = 5891; l4285 = rt.sub(l4255, l4284);
    at = 5892; l4286 = K[1531];
    at = 5893; l4287 = rt.max(l4286, l4285);
    at = 5894; l4288 = K[1532];
    at = 5895; l4289 = rt.array_slice(l4281, l4288, l4287);
    at = 5896; if (rt.truthy(l4248)) {
      at = 5897; l4291 = field(rt, l4251, "points");
      at = 5898; l4290 = l4291;
    } else {
      at = 5899; l4292 = K[1533];
      at = 5900; l4290 = l4292;
    }
    at = 5901; l4293 = K[1534];
    at = 5902; l4294 = rt.gt(l4255, l4293);
    at = 5903; if (rt.truthy(l4294)) {
      at = 5904; l4296 = K[1535];
      at = 5905; l4295 = l4296;
    } else {
      at = 5906; l4297 = K[1536];
      at = 5907; l4295 = l4297;
    }
    at = 5908; l4298 = K[1537];
    at = 5909; l4299 = rt.sub(l4255, l4298);
    at = 5910; l4300 = K[1538];
    at = 5911; l4301 = rt.max(l4300, l4299);
    at = 5912; l4302 = rt.array_slice(l4290, l4295, l4301);
    at = 5913; if (rt.truthy(l4248)) {
      at = 5914; l4304 = field(rt, l4251, "color");
      at = 5915; l4303 = l4304;
    } else {
      at = 5916; l4305 = K[1539];
      at = 5917; l4303 = l4305;
    }
    at = 5918; l4306 = K[1540];
    at = 5919; l4307 = rt.sequence_get(l4306, l4303);
    at = 5920; if (rt.truthy(l4248)) {
      at = 5921; l4309 = field(rt, l4251, "width");
      at = 5922; l4308 = l4309;
    } else {
      at = 5923; l4310 = K[1541];
      at = 5924; l4308 = l4310;
    }
    at = 5925; markSet(rt, true, "line", {from: l4289, to: l4302, color: l4307, width: l4308});
    at = 5926; l4311 = field(rt, l15, "strokes");
    at = 5927; l4312 = rt.sequence_length(l4311);
    at = 5928; l4313 = K[1542];
    at = 5929; l4314 = rt.lt(l4313, l4312);
    at = 5930; if (rt.truthy(l4314)) {
      at = 5931; l4315 = field(rt, l15, "strokes");
      at = 5932; l4316 = K[1543];
      at = 5933; l4317 = rt.sequence_get(l4315, l4316);
    }
    at = 5934; if (rt.truthy(l4314)) {
      at = 5935; l4319 = field(rt, l4317, "points");
      at = 5936; l4318 = l4319;
    } else {
      at = 5937; l4320 = K[1544];
      at = 5938; l4318 = l4320;
    }
    at = 5939; l4321 = rt.array_count(l4318);
    at = 5940; l4322 = K[1545];
    at = 5941; l4323 = rt.gt(l4321, l4322);
    at = 5942; if (rt.truthy(l4323)) {
      at = 5943; if (rt.truthy(l4314)) {
        at = 5944; l4326 = field(rt, l4317, "points");
        at = 5945; l4325 = l4326;
      } else {
        at = 5946; l4327 = K[1546];
        at = 5947; l4325 = l4327;
      }
      at = 5948; l4328 = K[1547];
      at = 5949; l4329 = rt.array_nth(l4325, l4328);
      at = 5950; l4324 = l4329;
    } else {
      at = 5951; l4330 = K[1548];
      at = 5952; l4324 = l4330;
    }
    at = 5953; if (rt.truthy(l4314)) {
      at = 5954; l4332 = field(rt, l4317, "width");
      at = 5955; l4331 = l4332;
    } else {
      at = 5956; l4333 = K[1549];
      at = 5957; l4331 = l4333;
    }
    at = 5958; l4334 = K[1550];
    at = 5959; l4335 = rt.int_div(l4331, l4334);
    at = 5960; l4336 = K[1551];
    at = 5961; l4337 = rt.max(l4336, l4335);
    at = 5962; l4338 = K[1552];
    at = 5963; l4339 = rt.gt(l4321, l4338);
    at = 5964; if (rt.truthy(l4339)) {
      at = 5965; if (rt.truthy(l4314)) {
        at = 5966; l4342 = field(rt, l4317, "color");
        at = 5967; l4341 = l4342;
      } else {
        at = 5968; l4343 = K[1553];
        at = 5969; l4341 = l4343;
      }
      at = 5970; l4344 = K[1554];
      at = 5971; l4345 = rt.sequence_get(l4344, l4341);
      at = 5972; l4340 = l4345;
    } else {
      at = 5973; l4346 = K[1555];
      at = 5974; l4340 = l4346;
    }
    at = 5975; markSet(rt, false, "circle", {at: l4324, radius: l4337, fill: l4340});
    at = 5976; if (rt.truthy(l4314)) {
      at = 5977; l4348 = field(rt, l4317, "points");
      at = 5978; l4347 = l4348;
    } else {
      at = 5979; l4349 = K[1556];
      at = 5980; l4347 = l4349;
    }
    at = 5981; l4350 = K[1557];
    at = 5982; l4351 = rt.sub(l4321, l4350);
    at = 5983; l4352 = K[1558];
    at = 5984; l4353 = rt.max(l4352, l4351);
    at = 5985; l4354 = K[1559];
    at = 5986; l4355 = rt.array_slice(l4347, l4354, l4353);
    at = 5987; if (rt.truthy(l4314)) {
      at = 5988; l4357 = field(rt, l4317, "points");
      at = 5989; l4356 = l4357;
    } else {
      at = 5990; l4358 = K[1560];
      at = 5991; l4356 = l4358;
    }
    at = 5992; l4359 = K[1561];
    at = 5993; l4360 = rt.gt(l4321, l4359);
    at = 5994; if (rt.truthy(l4360)) {
      at = 5995; l4362 = K[1562];
      at = 5996; l4361 = l4362;
    } else {
      at = 5997; l4363 = K[1563];
      at = 5998; l4361 = l4363;
    }
    at = 5999; l4364 = K[1564];
    at = 6000; l4365 = rt.sub(l4321, l4364);
    at = 6001; l4366 = K[1565];
    at = 6002; l4367 = rt.max(l4366, l4365);
    at = 6003; l4368 = rt.array_slice(l4356, l4361, l4367);
    at = 6004; if (rt.truthy(l4314)) {
      at = 6005; l4370 = field(rt, l4317, "color");
      at = 6006; l4369 = l4370;
    } else {
      at = 6007; l4371 = K[1566];
      at = 6008; l4369 = l4371;
    }
    at = 6009; l4372 = K[1567];
    at = 6010; l4373 = rt.sequence_get(l4372, l4369);
    at = 6011; if (rt.truthy(l4314)) {
      at = 6012; l4375 = field(rt, l4317, "width");
      at = 6013; l4374 = l4375;
    } else {
      at = 6014; l4376 = K[1568];
      at = 6015; l4374 = l4376;
    }
    at = 6016; markSet(rt, true, "line", {from: l4355, to: l4368, color: l4373, width: l4374});
    at = 6017; l4377 = field(rt, l15, "strokes");
    at = 6018; l4378 = rt.sequence_length(l4377);
    at = 6019; l4379 = K[1569];
    at = 6020; l4380 = rt.lt(l4379, l4378);
    at = 6021; if (rt.truthy(l4380)) {
      at = 6022; l4381 = field(rt, l15, "strokes");
      at = 6023; l4382 = K[1570];
      at = 6024; l4383 = rt.sequence_get(l4381, l4382);
    }
    at = 6025; if (rt.truthy(l4380)) {
      at = 6026; l4385 = field(rt, l4383, "points");
      at = 6027; l4384 = l4385;
    } else {
      at = 6028; l4386 = K[1571];
      at = 6029; l4384 = l4386;
    }
    at = 6030; l4387 = rt.array_count(l4384);
    at = 6031; l4388 = K[1572];
    at = 6032; l4389 = rt.gt(l4387, l4388);
    at = 6033; if (rt.truthy(l4389)) {
      at = 6034; if (rt.truthy(l4380)) {
        at = 6035; l4392 = field(rt, l4383, "points");
        at = 6036; l4391 = l4392;
      } else {
        at = 6037; l4393 = K[1573];
        at = 6038; l4391 = l4393;
      }
      at = 6039; l4394 = K[1574];
      at = 6040; l4395 = rt.array_nth(l4391, l4394);
      at = 6041; l4390 = l4395;
    } else {
      at = 6042; l4396 = K[1575];
      at = 6043; l4390 = l4396;
    }
    at = 6044; if (rt.truthy(l4380)) {
      at = 6045; l4398 = field(rt, l4383, "width");
      at = 6046; l4397 = l4398;
    } else {
      at = 6047; l4399 = K[1576];
      at = 6048; l4397 = l4399;
    }
    at = 6049; l4400 = K[1577];
    at = 6050; l4401 = rt.int_div(l4397, l4400);
    at = 6051; l4402 = K[1578];
    at = 6052; l4403 = rt.max(l4402, l4401);
    at = 6053; l4404 = K[1579];
    at = 6054; l4405 = rt.gt(l4387, l4404);
    at = 6055; if (rt.truthy(l4405)) {
      at = 6056; if (rt.truthy(l4380)) {
        at = 6057; l4408 = field(rt, l4383, "color");
        at = 6058; l4407 = l4408;
      } else {
        at = 6059; l4409 = K[1580];
        at = 6060; l4407 = l4409;
      }
      at = 6061; l4410 = K[1581];
      at = 6062; l4411 = rt.sequence_get(l4410, l4407);
      at = 6063; l4406 = l4411;
    } else {
      at = 6064; l4412 = K[1582];
      at = 6065; l4406 = l4412;
    }
    at = 6066; markSet(rt, false, "circle", {at: l4390, radius: l4403, fill: l4406});
    at = 6067; if (rt.truthy(l4380)) {
      at = 6068; l4414 = field(rt, l4383, "points");
      at = 6069; l4413 = l4414;
    } else {
      at = 6070; l4415 = K[1583];
      at = 6071; l4413 = l4415;
    }
    at = 6072; l4416 = K[1584];
    at = 6073; l4417 = rt.sub(l4387, l4416);
    at = 6074; l4418 = K[1585];
    at = 6075; l4419 = rt.max(l4418, l4417);
    at = 6076; l4420 = K[1586];
    at = 6077; l4421 = rt.array_slice(l4413, l4420, l4419);
    at = 6078; if (rt.truthy(l4380)) {
      at = 6079; l4423 = field(rt, l4383, "points");
      at = 6080; l4422 = l4423;
    } else {
      at = 6081; l4424 = K[1587];
      at = 6082; l4422 = l4424;
    }
    at = 6083; l4425 = K[1588];
    at = 6084; l4426 = rt.gt(l4387, l4425);
    at = 6085; if (rt.truthy(l4426)) {
      at = 6086; l4428 = K[1589];
      at = 6087; l4427 = l4428;
    } else {
      at = 6088; l4429 = K[1590];
      at = 6089; l4427 = l4429;
    }
    at = 6090; l4430 = K[1591];
    at = 6091; l4431 = rt.sub(l4387, l4430);
    at = 6092; l4432 = K[1592];
    at = 6093; l4433 = rt.max(l4432, l4431);
    at = 6094; l4434 = rt.array_slice(l4422, l4427, l4433);
    at = 6095; if (rt.truthy(l4380)) {
      at = 6096; l4436 = field(rt, l4383, "color");
      at = 6097; l4435 = l4436;
    } else {
      at = 6098; l4437 = K[1593];
      at = 6099; l4435 = l4437;
    }
    at = 6100; l4438 = K[1594];
    at = 6101; l4439 = rt.sequence_get(l4438, l4435);
    at = 6102; if (rt.truthy(l4380)) {
      at = 6103; l4441 = field(rt, l4383, "width");
      at = 6104; l4440 = l4441;
    } else {
      at = 6105; l4442 = K[1595];
      at = 6106; l4440 = l4442;
    }
    at = 6107; markSet(rt, true, "line", {from: l4421, to: l4434, color: l4439, width: l4440});
    at = 6108; l4443 = field(rt, l15, "strokes");
    at = 6109; l4444 = rt.sequence_length(l4443);
    at = 6110; l4445 = K[1596];
    at = 6111; l4446 = rt.lt(l4445, l4444);
    at = 6112; if (rt.truthy(l4446)) {
      at = 6113; l4447 = field(rt, l15, "strokes");
      at = 6114; l4448 = K[1597];
      at = 6115; l4449 = rt.sequence_get(l4447, l4448);
    }
    at = 6116; if (rt.truthy(l4446)) {
      at = 6117; l4451 = field(rt, l4449, "points");
      at = 6118; l4450 = l4451;
    } else {
      at = 6119; l4452 = K[1598];
      at = 6120; l4450 = l4452;
    }
    at = 6121; l4453 = rt.array_count(l4450);
    at = 6122; l4454 = K[1599];
    at = 6123; l4455 = rt.gt(l4453, l4454);
    at = 6124; if (rt.truthy(l4455)) {
      at = 6125; if (rt.truthy(l4446)) {
        at = 6126; l4458 = field(rt, l4449, "points");
        at = 6127; l4457 = l4458;
      } else {
        at = 6128; l4459 = K[1600];
        at = 6129; l4457 = l4459;
      }
      at = 6130; l4460 = K[1601];
      at = 6131; l4461 = rt.array_nth(l4457, l4460);
      at = 6132; l4456 = l4461;
    } else {
      at = 6133; l4462 = K[1602];
      at = 6134; l4456 = l4462;
    }
    at = 6135; if (rt.truthy(l4446)) {
      at = 6136; l4464 = field(rt, l4449, "width");
      at = 6137; l4463 = l4464;
    } else {
      at = 6138; l4465 = K[1603];
      at = 6139; l4463 = l4465;
    }
    at = 6140; l4466 = K[1604];
    at = 6141; l4467 = rt.int_div(l4463, l4466);
    at = 6142; l4468 = K[1605];
    at = 6143; l4469 = rt.max(l4468, l4467);
    at = 6144; l4470 = K[1606];
    at = 6145; l4471 = rt.gt(l4453, l4470);
    at = 6146; if (rt.truthy(l4471)) {
      at = 6147; if (rt.truthy(l4446)) {
        at = 6148; l4474 = field(rt, l4449, "color");
        at = 6149; l4473 = l4474;
      } else {
        at = 6150; l4475 = K[1607];
        at = 6151; l4473 = l4475;
      }
      at = 6152; l4476 = K[1608];
      at = 6153; l4477 = rt.sequence_get(l4476, l4473);
      at = 6154; l4472 = l4477;
    } else {
      at = 6155; l4478 = K[1609];
      at = 6156; l4472 = l4478;
    }
    at = 6157; markSet(rt, false, "circle", {at: l4456, radius: l4469, fill: l4472});
    at = 6158; if (rt.truthy(l4446)) {
      at = 6159; l4480 = field(rt, l4449, "points");
      at = 6160; l4479 = l4480;
    } else {
      at = 6161; l4481 = K[1610];
      at = 6162; l4479 = l4481;
    }
    at = 6163; l4482 = K[1611];
    at = 6164; l4483 = rt.sub(l4453, l4482);
    at = 6165; l4484 = K[1612];
    at = 6166; l4485 = rt.max(l4484, l4483);
    at = 6167; l4486 = K[1613];
    at = 6168; l4487 = rt.array_slice(l4479, l4486, l4485);
    at = 6169; if (rt.truthy(l4446)) {
      at = 6170; l4489 = field(rt, l4449, "points");
      at = 6171; l4488 = l4489;
    } else {
      at = 6172; l4490 = K[1614];
      at = 6173; l4488 = l4490;
    }
    at = 6174; l4491 = K[1615];
    at = 6175; l4492 = rt.gt(l4453, l4491);
    at = 6176; if (rt.truthy(l4492)) {
      at = 6177; l4494 = K[1616];
      at = 6178; l4493 = l4494;
    } else {
      at = 6179; l4495 = K[1617];
      at = 6180; l4493 = l4495;
    }
    at = 6181; l4496 = K[1618];
    at = 6182; l4497 = rt.sub(l4453, l4496);
    at = 6183; l4498 = K[1619];
    at = 6184; l4499 = rt.max(l4498, l4497);
    at = 6185; l4500 = rt.array_slice(l4488, l4493, l4499);
    at = 6186; if (rt.truthy(l4446)) {
      at = 6187; l4502 = field(rt, l4449, "color");
      at = 6188; l4501 = l4502;
    } else {
      at = 6189; l4503 = K[1620];
      at = 6190; l4501 = l4503;
    }
    at = 6191; l4504 = K[1621];
    at = 6192; l4505 = rt.sequence_get(l4504, l4501);
    at = 6193; if (rt.truthy(l4446)) {
      at = 6194; l4507 = field(rt, l4449, "width");
      at = 6195; l4506 = l4507;
    } else {
      at = 6196; l4508 = K[1622];
      at = 6197; l4506 = l4508;
    }
    at = 6198; markSet(rt, true, "line", {from: l4487, to: l4500, color: l4505, width: l4506});
    at = 6199; l4509 = field(rt, l15, "strokes");
    at = 6200; l4510 = rt.sequence_length(l4509);
    at = 6201; l4511 = K[1623];
    at = 6202; l4512 = rt.lt(l4511, l4510);
    at = 6203; if (rt.truthy(l4512)) {
      at = 6204; l4513 = field(rt, l15, "strokes");
      at = 6205; l4514 = K[1624];
      at = 6206; l4515 = rt.sequence_get(l4513, l4514);
    }
    at = 6207; if (rt.truthy(l4512)) {
      at = 6208; l4517 = field(rt, l4515, "points");
      at = 6209; l4516 = l4517;
    } else {
      at = 6210; l4518 = K[1625];
      at = 6211; l4516 = l4518;
    }
    at = 6212; l4519 = rt.array_count(l4516);
    at = 6213; l4520 = K[1626];
    at = 6214; l4521 = rt.gt(l4519, l4520);
    at = 6215; if (rt.truthy(l4521)) {
      at = 6216; if (rt.truthy(l4512)) {
        at = 6217; l4524 = field(rt, l4515, "points");
        at = 6218; l4523 = l4524;
      } else {
        at = 6219; l4525 = K[1627];
        at = 6220; l4523 = l4525;
      }
      at = 6221; l4526 = K[1628];
      at = 6222; l4527 = rt.array_nth(l4523, l4526);
      at = 6223; l4522 = l4527;
    } else {
      at = 6224; l4528 = K[1629];
      at = 6225; l4522 = l4528;
    }
    at = 6226; if (rt.truthy(l4512)) {
      at = 6227; l4530 = field(rt, l4515, "width");
      at = 6228; l4529 = l4530;
    } else {
      at = 6229; l4531 = K[1630];
      at = 6230; l4529 = l4531;
    }
    at = 6231; l4532 = K[1631];
    at = 6232; l4533 = rt.int_div(l4529, l4532);
    at = 6233; l4534 = K[1632];
    at = 6234; l4535 = rt.max(l4534, l4533);
    at = 6235; l4536 = K[1633];
    at = 6236; l4537 = rt.gt(l4519, l4536);
    at = 6237; if (rt.truthy(l4537)) {
      at = 6238; if (rt.truthy(l4512)) {
        at = 6239; l4540 = field(rt, l4515, "color");
        at = 6240; l4539 = l4540;
      } else {
        at = 6241; l4541 = K[1634];
        at = 6242; l4539 = l4541;
      }
      at = 6243; l4542 = K[1635];
      at = 6244; l4543 = rt.sequence_get(l4542, l4539);
      at = 6245; l4538 = l4543;
    } else {
      at = 6246; l4544 = K[1636];
      at = 6247; l4538 = l4544;
    }
    at = 6248; markSet(rt, false, "circle", {at: l4522, radius: l4535, fill: l4538});
    at = 6249; if (rt.truthy(l4512)) {
      at = 6250; l4546 = field(rt, l4515, "points");
      at = 6251; l4545 = l4546;
    } else {
      at = 6252; l4547 = K[1637];
      at = 6253; l4545 = l4547;
    }
    at = 6254; l4548 = K[1638];
    at = 6255; l4549 = rt.sub(l4519, l4548);
    at = 6256; l4550 = K[1639];
    at = 6257; l4551 = rt.max(l4550, l4549);
    at = 6258; l4552 = K[1640];
    at = 6259; l4553 = rt.array_slice(l4545, l4552, l4551);
    at = 6260; if (rt.truthy(l4512)) {
      at = 6261; l4555 = field(rt, l4515, "points");
      at = 6262; l4554 = l4555;
    } else {
      at = 6263; l4556 = K[1641];
      at = 6264; l4554 = l4556;
    }
    at = 6265; l4557 = K[1642];
    at = 6266; l4558 = rt.gt(l4519, l4557);
    at = 6267; if (rt.truthy(l4558)) {
      at = 6268; l4560 = K[1643];
      at = 6269; l4559 = l4560;
    } else {
      at = 6270; l4561 = K[1644];
      at = 6271; l4559 = l4561;
    }
    at = 6272; l4562 = K[1645];
    at = 6273; l4563 = rt.sub(l4519, l4562);
    at = 6274; l4564 = K[1646];
    at = 6275; l4565 = rt.max(l4564, l4563);
    at = 6276; l4566 = rt.array_slice(l4554, l4559, l4565);
    at = 6277; if (rt.truthy(l4512)) {
      at = 6278; l4568 = field(rt, l4515, "color");
      at = 6279; l4567 = l4568;
    } else {
      at = 6280; l4569 = K[1647];
      at = 6281; l4567 = l4569;
    }
    at = 6282; l4570 = K[1648];
    at = 6283; l4571 = rt.sequence_get(l4570, l4567);
    at = 6284; if (rt.truthy(l4512)) {
      at = 6285; l4573 = field(rt, l4515, "width");
      at = 6286; l4572 = l4573;
    } else {
      at = 6287; l4574 = K[1649];
      at = 6288; l4572 = l4574;
    }
    at = 6289; markSet(rt, true, "line", {from: l4553, to: l4566, color: l4571, width: l4572});
    at = 6290; l4575 = field(rt, l15, "strokes");
    at = 6291; l4576 = rt.sequence_length(l4575);
    at = 6292; l4577 = K[1650];
    at = 6293; l4578 = rt.lt(l4577, l4576);
    at = 6294; if (rt.truthy(l4578)) {
      at = 6295; l4579 = field(rt, l15, "strokes");
      at = 6296; l4580 = K[1651];
      at = 6297; l4581 = rt.sequence_get(l4579, l4580);
    }
    at = 6298; if (rt.truthy(l4578)) {
      at = 6299; l4583 = field(rt, l4581, "points");
      at = 6300; l4582 = l4583;
    } else {
      at = 6301; l4584 = K[1652];
      at = 6302; l4582 = l4584;
    }
    at = 6303; l4585 = rt.array_count(l4582);
    at = 6304; l4586 = K[1653];
    at = 6305; l4587 = rt.gt(l4585, l4586);
    at = 6306; if (rt.truthy(l4587)) {
      at = 6307; if (rt.truthy(l4578)) {
        at = 6308; l4590 = field(rt, l4581, "points");
        at = 6309; l4589 = l4590;
      } else {
        at = 6310; l4591 = K[1654];
        at = 6311; l4589 = l4591;
      }
      at = 6312; l4592 = K[1655];
      at = 6313; l4593 = rt.array_nth(l4589, l4592);
      at = 6314; l4588 = l4593;
    } else {
      at = 6315; l4594 = K[1656];
      at = 6316; l4588 = l4594;
    }
    at = 6317; if (rt.truthy(l4578)) {
      at = 6318; l4596 = field(rt, l4581, "width");
      at = 6319; l4595 = l4596;
    } else {
      at = 6320; l4597 = K[1657];
      at = 6321; l4595 = l4597;
    }
    at = 6322; l4598 = K[1658];
    at = 6323; l4599 = rt.int_div(l4595, l4598);
    at = 6324; l4600 = K[1659];
    at = 6325; l4601 = rt.max(l4600, l4599);
    at = 6326; l4602 = K[1660];
    at = 6327; l4603 = rt.gt(l4585, l4602);
    at = 6328; if (rt.truthy(l4603)) {
      at = 6329; if (rt.truthy(l4578)) {
        at = 6330; l4606 = field(rt, l4581, "color");
        at = 6331; l4605 = l4606;
      } else {
        at = 6332; l4607 = K[1661];
        at = 6333; l4605 = l4607;
      }
      at = 6334; l4608 = K[1662];
      at = 6335; l4609 = rt.sequence_get(l4608, l4605);
      at = 6336; l4604 = l4609;
    } else {
      at = 6337; l4610 = K[1663];
      at = 6338; l4604 = l4610;
    }
    at = 6339; markSet(rt, false, "circle", {at: l4588, radius: l4601, fill: l4604});
    at = 6340; if (rt.truthy(l4578)) {
      at = 6341; l4612 = field(rt, l4581, "points");
      at = 6342; l4611 = l4612;
    } else {
      at = 6343; l4613 = K[1664];
      at = 6344; l4611 = l4613;
    }
    at = 6345; l4614 = K[1665];
    at = 6346; l4615 = rt.sub(l4585, l4614);
    at = 6347; l4616 = K[1666];
    at = 6348; l4617 = rt.max(l4616, l4615);
    at = 6349; l4618 = K[1667];
    at = 6350; l4619 = rt.array_slice(l4611, l4618, l4617);
    at = 6351; if (rt.truthy(l4578)) {
      at = 6352; l4621 = field(rt, l4581, "points");
      at = 6353; l4620 = l4621;
    } else {
      at = 6354; l4622 = K[1668];
      at = 6355; l4620 = l4622;
    }
    at = 6356; l4623 = K[1669];
    at = 6357; l4624 = rt.gt(l4585, l4623);
    at = 6358; if (rt.truthy(l4624)) {
      at = 6359; l4626 = K[1670];
      at = 6360; l4625 = l4626;
    } else {
      at = 6361; l4627 = K[1671];
      at = 6362; l4625 = l4627;
    }
    at = 6363; l4628 = K[1672];
    at = 6364; l4629 = rt.sub(l4585, l4628);
    at = 6365; l4630 = K[1673];
    at = 6366; l4631 = rt.max(l4630, l4629);
    at = 6367; l4632 = rt.array_slice(l4620, l4625, l4631);
    at = 6368; if (rt.truthy(l4578)) {
      at = 6369; l4634 = field(rt, l4581, "color");
      at = 6370; l4633 = l4634;
    } else {
      at = 6371; l4635 = K[1674];
      at = 6372; l4633 = l4635;
    }
    at = 6373; l4636 = K[1675];
    at = 6374; l4637 = rt.sequence_get(l4636, l4633);
    at = 6375; if (rt.truthy(l4578)) {
      at = 6376; l4639 = field(rt, l4581, "width");
      at = 6377; l4638 = l4639;
    } else {
      at = 6378; l4640 = K[1676];
      at = 6379; l4638 = l4640;
    }
    at = 6380; markSet(rt, true, "line", {from: l4619, to: l4632, color: l4637, width: l4638});
    at = 6381; l4641 = field(rt, l15, "strokes");
    at = 6382; l4642 = rt.sequence_length(l4641);
    at = 6383; l4643 = K[1677];
    at = 6384; l4644 = rt.lt(l4643, l4642);
    at = 6385; if (rt.truthy(l4644)) {
      at = 6386; l4645 = field(rt, l15, "strokes");
      at = 6387; l4646 = K[1678];
      at = 6388; l4647 = rt.sequence_get(l4645, l4646);
    }
    at = 6389; if (rt.truthy(l4644)) {
      at = 6390; l4649 = field(rt, l4647, "points");
      at = 6391; l4648 = l4649;
    } else {
      at = 6392; l4650 = K[1679];
      at = 6393; l4648 = l4650;
    }
    at = 6394; l4651 = rt.array_count(l4648);
    at = 6395; l4652 = K[1680];
    at = 6396; l4653 = rt.gt(l4651, l4652);
    at = 6397; if (rt.truthy(l4653)) {
      at = 6398; if (rt.truthy(l4644)) {
        at = 6399; l4656 = field(rt, l4647, "points");
        at = 6400; l4655 = l4656;
      } else {
        at = 6401; l4657 = K[1681];
        at = 6402; l4655 = l4657;
      }
      at = 6403; l4658 = K[1682];
      at = 6404; l4659 = rt.array_nth(l4655, l4658);
      at = 6405; l4654 = l4659;
    } else {
      at = 6406; l4660 = K[1683];
      at = 6407; l4654 = l4660;
    }
    at = 6408; if (rt.truthy(l4644)) {
      at = 6409; l4662 = field(rt, l4647, "width");
      at = 6410; l4661 = l4662;
    } else {
      at = 6411; l4663 = K[1684];
      at = 6412; l4661 = l4663;
    }
    at = 6413; l4664 = K[1685];
    at = 6414; l4665 = rt.int_div(l4661, l4664);
    at = 6415; l4666 = K[1686];
    at = 6416; l4667 = rt.max(l4666, l4665);
    at = 6417; l4668 = K[1687];
    at = 6418; l4669 = rt.gt(l4651, l4668);
    at = 6419; if (rt.truthy(l4669)) {
      at = 6420; if (rt.truthy(l4644)) {
        at = 6421; l4672 = field(rt, l4647, "color");
        at = 6422; l4671 = l4672;
      } else {
        at = 6423; l4673 = K[1688];
        at = 6424; l4671 = l4673;
      }
      at = 6425; l4674 = K[1689];
      at = 6426; l4675 = rt.sequence_get(l4674, l4671);
      at = 6427; l4670 = l4675;
    } else {
      at = 6428; l4676 = K[1690];
      at = 6429; l4670 = l4676;
    }
    at = 6430; markSet(rt, false, "circle", {at: l4654, radius: l4667, fill: l4670});
    at = 6431; if (rt.truthy(l4644)) {
      at = 6432; l4678 = field(rt, l4647, "points");
      at = 6433; l4677 = l4678;
    } else {
      at = 6434; l4679 = K[1691];
      at = 6435; l4677 = l4679;
    }
    at = 6436; l4680 = K[1692];
    at = 6437; l4681 = rt.sub(l4651, l4680);
    at = 6438; l4682 = K[1693];
    at = 6439; l4683 = rt.max(l4682, l4681);
    at = 6440; l4684 = K[1694];
    at = 6441; l4685 = rt.array_slice(l4677, l4684, l4683);
    at = 6442; if (rt.truthy(l4644)) {
      at = 6443; l4687 = field(rt, l4647, "points");
      at = 6444; l4686 = l4687;
    } else {
      at = 6445; l4688 = K[1695];
      at = 6446; l4686 = l4688;
    }
    at = 6447; l4689 = K[1696];
    at = 6448; l4690 = rt.gt(l4651, l4689);
    at = 6449; if (rt.truthy(l4690)) {
      at = 6450; l4692 = K[1697];
      at = 6451; l4691 = l4692;
    } else {
      at = 6452; l4693 = K[1698];
      at = 6453; l4691 = l4693;
    }
    at = 6454; l4694 = K[1699];
    at = 6455; l4695 = rt.sub(l4651, l4694);
    at = 6456; l4696 = K[1700];
    at = 6457; l4697 = rt.max(l4696, l4695);
    at = 6458; l4698 = rt.array_slice(l4686, l4691, l4697);
    at = 6459; if (rt.truthy(l4644)) {
      at = 6460; l4700 = field(rt, l4647, "color");
      at = 6461; l4699 = l4700;
    } else {
      at = 6462; l4701 = K[1701];
      at = 6463; l4699 = l4701;
    }
    at = 6464; l4702 = K[1702];
    at = 6465; l4703 = rt.sequence_get(l4702, l4699);
    at = 6466; if (rt.truthy(l4644)) {
      at = 6467; l4705 = field(rt, l4647, "width");
      at = 6468; l4704 = l4705;
    } else {
      at = 6469; l4706 = K[1703];
      at = 6470; l4704 = l4706;
    }
    at = 6471; markSet(rt, true, "line", {from: l4685, to: l4698, color: l4703, width: l4704});
    at = 6472; l4707 = field(rt, l15, "strokes");
    at = 6473; l4708 = rt.sequence_length(l4707);
    at = 6474; l4709 = K[1704];
    at = 6475; l4710 = rt.lt(l4709, l4708);
    at = 6476; if (rt.truthy(l4710)) {
      at = 6477; l4711 = field(rt, l15, "strokes");
      at = 6478; l4712 = K[1705];
      at = 6479; l4713 = rt.sequence_get(l4711, l4712);
    }
    at = 6480; if (rt.truthy(l4710)) {
      at = 6481; l4715 = field(rt, l4713, "points");
      at = 6482; l4714 = l4715;
    } else {
      at = 6483; l4716 = K[1706];
      at = 6484; l4714 = l4716;
    }
    at = 6485; l4717 = rt.array_count(l4714);
    at = 6486; l4718 = K[1707];
    at = 6487; l4719 = rt.gt(l4717, l4718);
    at = 6488; if (rt.truthy(l4719)) {
      at = 6489; if (rt.truthy(l4710)) {
        at = 6490; l4722 = field(rt, l4713, "points");
        at = 6491; l4721 = l4722;
      } else {
        at = 6492; l4723 = K[1708];
        at = 6493; l4721 = l4723;
      }
      at = 6494; l4724 = K[1709];
      at = 6495; l4725 = rt.array_nth(l4721, l4724);
      at = 6496; l4720 = l4725;
    } else {
      at = 6497; l4726 = K[1710];
      at = 6498; l4720 = l4726;
    }
    at = 6499; if (rt.truthy(l4710)) {
      at = 6500; l4728 = field(rt, l4713, "width");
      at = 6501; l4727 = l4728;
    } else {
      at = 6502; l4729 = K[1711];
      at = 6503; l4727 = l4729;
    }
    at = 6504; l4730 = K[1712];
    at = 6505; l4731 = rt.int_div(l4727, l4730);
    at = 6506; l4732 = K[1713];
    at = 6507; l4733 = rt.max(l4732, l4731);
    at = 6508; l4734 = K[1714];
    at = 6509; l4735 = rt.gt(l4717, l4734);
    at = 6510; if (rt.truthy(l4735)) {
      at = 6511; if (rt.truthy(l4710)) {
        at = 6512; l4738 = field(rt, l4713, "color");
        at = 6513; l4737 = l4738;
      } else {
        at = 6514; l4739 = K[1715];
        at = 6515; l4737 = l4739;
      }
      at = 6516; l4740 = K[1716];
      at = 6517; l4741 = rt.sequence_get(l4740, l4737);
      at = 6518; l4736 = l4741;
    } else {
      at = 6519; l4742 = K[1717];
      at = 6520; l4736 = l4742;
    }
    at = 6521; markSet(rt, false, "circle", {at: l4720, radius: l4733, fill: l4736});
    at = 6522; if (rt.truthy(l4710)) {
      at = 6523; l4744 = field(rt, l4713, "points");
      at = 6524; l4743 = l4744;
    } else {
      at = 6525; l4745 = K[1718];
      at = 6526; l4743 = l4745;
    }
    at = 6527; l4746 = K[1719];
    at = 6528; l4747 = rt.sub(l4717, l4746);
    at = 6529; l4748 = K[1720];
    at = 6530; l4749 = rt.max(l4748, l4747);
    at = 6531; l4750 = K[1721];
    at = 6532; l4751 = rt.array_slice(l4743, l4750, l4749);
    at = 6533; if (rt.truthy(l4710)) {
      at = 6534; l4753 = field(rt, l4713, "points");
      at = 6535; l4752 = l4753;
    } else {
      at = 6536; l4754 = K[1722];
      at = 6537; l4752 = l4754;
    }
    at = 6538; l4755 = K[1723];
    at = 6539; l4756 = rt.gt(l4717, l4755);
    at = 6540; if (rt.truthy(l4756)) {
      at = 6541; l4758 = K[1724];
      at = 6542; l4757 = l4758;
    } else {
      at = 6543; l4759 = K[1725];
      at = 6544; l4757 = l4759;
    }
    at = 6545; l4760 = K[1726];
    at = 6546; l4761 = rt.sub(l4717, l4760);
    at = 6547; l4762 = K[1727];
    at = 6548; l4763 = rt.max(l4762, l4761);
    at = 6549; l4764 = rt.array_slice(l4752, l4757, l4763);
    at = 6550; if (rt.truthy(l4710)) {
      at = 6551; l4766 = field(rt, l4713, "color");
      at = 6552; l4765 = l4766;
    } else {
      at = 6553; l4767 = K[1728];
      at = 6554; l4765 = l4767;
    }
    at = 6555; l4768 = K[1729];
    at = 6556; l4769 = rt.sequence_get(l4768, l4765);
    at = 6557; if (rt.truthy(l4710)) {
      at = 6558; l4771 = field(rt, l4713, "width");
      at = 6559; l4770 = l4771;
    } else {
      at = 6560; l4772 = K[1730];
      at = 6561; l4770 = l4772;
    }
    at = 6562; markSet(rt, true, "line", {from: l4751, to: l4764, color: l4769, width: l4770});
    at = 6563; l4773 = field(rt, l15, "strokes");
    at = 6564; l4774 = rt.sequence_length(l4773);
    at = 6565; l4775 = K[1731];
    at = 6566; l4776 = rt.lt(l4775, l4774);
    at = 6567; if (rt.truthy(l4776)) {
      at = 6568; l4777 = field(rt, l15, "strokes");
      at = 6569; l4778 = K[1732];
      at = 6570; l4779 = rt.sequence_get(l4777, l4778);
    }
    at = 6571; if (rt.truthy(l4776)) {
      at = 6572; l4781 = field(rt, l4779, "points");
      at = 6573; l4780 = l4781;
    } else {
      at = 6574; l4782 = K[1733];
      at = 6575; l4780 = l4782;
    }
    at = 6576; l4783 = rt.array_count(l4780);
    at = 6577; l4784 = K[1734];
    at = 6578; l4785 = rt.gt(l4783, l4784);
    at = 6579; if (rt.truthy(l4785)) {
      at = 6580; if (rt.truthy(l4776)) {
        at = 6581; l4788 = field(rt, l4779, "points");
        at = 6582; l4787 = l4788;
      } else {
        at = 6583; l4789 = K[1735];
        at = 6584; l4787 = l4789;
      }
      at = 6585; l4790 = K[1736];
      at = 6586; l4791 = rt.array_nth(l4787, l4790);
      at = 6587; l4786 = l4791;
    } else {
      at = 6588; l4792 = K[1737];
      at = 6589; l4786 = l4792;
    }
    at = 6590; if (rt.truthy(l4776)) {
      at = 6591; l4794 = field(rt, l4779, "width");
      at = 6592; l4793 = l4794;
    } else {
      at = 6593; l4795 = K[1738];
      at = 6594; l4793 = l4795;
    }
    at = 6595; l4796 = K[1739];
    at = 6596; l4797 = rt.int_div(l4793, l4796);
    at = 6597; l4798 = K[1740];
    at = 6598; l4799 = rt.max(l4798, l4797);
    at = 6599; l4800 = K[1741];
    at = 6600; l4801 = rt.gt(l4783, l4800);
    at = 6601; if (rt.truthy(l4801)) {
      at = 6602; if (rt.truthy(l4776)) {
        at = 6603; l4804 = field(rt, l4779, "color");
        at = 6604; l4803 = l4804;
      } else {
        at = 6605; l4805 = K[1742];
        at = 6606; l4803 = l4805;
      }
      at = 6607; l4806 = K[1743];
      at = 6608; l4807 = rt.sequence_get(l4806, l4803);
      at = 6609; l4802 = l4807;
    } else {
      at = 6610; l4808 = K[1744];
      at = 6611; l4802 = l4808;
    }
    at = 6612; markSet(rt, false, "circle", {at: l4786, radius: l4799, fill: l4802});
    at = 6613; if (rt.truthy(l4776)) {
      at = 6614; l4810 = field(rt, l4779, "points");
      at = 6615; l4809 = l4810;
    } else {
      at = 6616; l4811 = K[1745];
      at = 6617; l4809 = l4811;
    }
    at = 6618; l4812 = K[1746];
    at = 6619; l4813 = rt.sub(l4783, l4812);
    at = 6620; l4814 = K[1747];
    at = 6621; l4815 = rt.max(l4814, l4813);
    at = 6622; l4816 = K[1748];
    at = 6623; l4817 = rt.array_slice(l4809, l4816, l4815);
    at = 6624; if (rt.truthy(l4776)) {
      at = 6625; l4819 = field(rt, l4779, "points");
      at = 6626; l4818 = l4819;
    } else {
      at = 6627; l4820 = K[1749];
      at = 6628; l4818 = l4820;
    }
    at = 6629; l4821 = K[1750];
    at = 6630; l4822 = rt.gt(l4783, l4821);
    at = 6631; if (rt.truthy(l4822)) {
      at = 6632; l4824 = K[1751];
      at = 6633; l4823 = l4824;
    } else {
      at = 6634; l4825 = K[1752];
      at = 6635; l4823 = l4825;
    }
    at = 6636; l4826 = K[1753];
    at = 6637; l4827 = rt.sub(l4783, l4826);
    at = 6638; l4828 = K[1754];
    at = 6639; l4829 = rt.max(l4828, l4827);
    at = 6640; l4830 = rt.array_slice(l4818, l4823, l4829);
    at = 6641; if (rt.truthy(l4776)) {
      at = 6642; l4832 = field(rt, l4779, "color");
      at = 6643; l4831 = l4832;
    } else {
      at = 6644; l4833 = K[1755];
      at = 6645; l4831 = l4833;
    }
    at = 6646; l4834 = K[1756];
    at = 6647; l4835 = rt.sequence_get(l4834, l4831);
    at = 6648; if (rt.truthy(l4776)) {
      at = 6649; l4837 = field(rt, l4779, "width");
      at = 6650; l4836 = l4837;
    } else {
      at = 6651; l4838 = K[1757];
      at = 6652; l4836 = l4838;
    }
    at = 6653; markSet(rt, true, "line", {from: l4817, to: l4830, color: l4835, width: l4836});
    at = 6654; l4839 = field(rt, l15, "strokes");
    at = 6655; l4840 = rt.sequence_length(l4839);
    at = 6656; l4841 = K[1758];
    at = 6657; l4842 = rt.lt(l4841, l4840);
    at = 6658; if (rt.truthy(l4842)) {
      at = 6659; l4843 = field(rt, l15, "strokes");
      at = 6660; l4844 = K[1759];
      at = 6661; l4845 = rt.sequence_get(l4843, l4844);
    }
    at = 6662; if (rt.truthy(l4842)) {
      at = 6663; l4847 = field(rt, l4845, "points");
      at = 6664; l4846 = l4847;
    } else {
      at = 6665; l4848 = K[1760];
      at = 6666; l4846 = l4848;
    }
    at = 6667; l4849 = rt.array_count(l4846);
    at = 6668; l4850 = K[1761];
    at = 6669; l4851 = rt.gt(l4849, l4850);
    at = 6670; if (rt.truthy(l4851)) {
      at = 6671; if (rt.truthy(l4842)) {
        at = 6672; l4854 = field(rt, l4845, "points");
        at = 6673; l4853 = l4854;
      } else {
        at = 6674; l4855 = K[1762];
        at = 6675; l4853 = l4855;
      }
      at = 6676; l4856 = K[1763];
      at = 6677; l4857 = rt.array_nth(l4853, l4856);
      at = 6678; l4852 = l4857;
    } else {
      at = 6679; l4858 = K[1764];
      at = 6680; l4852 = l4858;
    }
    at = 6681; if (rt.truthy(l4842)) {
      at = 6682; l4860 = field(rt, l4845, "width");
      at = 6683; l4859 = l4860;
    } else {
      at = 6684; l4861 = K[1765];
      at = 6685; l4859 = l4861;
    }
    at = 6686; l4862 = K[1766];
    at = 6687; l4863 = rt.int_div(l4859, l4862);
    at = 6688; l4864 = K[1767];
    at = 6689; l4865 = rt.max(l4864, l4863);
    at = 6690; l4866 = K[1768];
    at = 6691; l4867 = rt.gt(l4849, l4866);
    at = 6692; if (rt.truthy(l4867)) {
      at = 6693; if (rt.truthy(l4842)) {
        at = 6694; l4870 = field(rt, l4845, "color");
        at = 6695; l4869 = l4870;
      } else {
        at = 6696; l4871 = K[1769];
        at = 6697; l4869 = l4871;
      }
      at = 6698; l4872 = K[1770];
      at = 6699; l4873 = rt.sequence_get(l4872, l4869);
      at = 6700; l4868 = l4873;
    } else {
      at = 6701; l4874 = K[1771];
      at = 6702; l4868 = l4874;
    }
    at = 6703; markSet(rt, false, "circle", {at: l4852, radius: l4865, fill: l4868});
    at = 6704; if (rt.truthy(l4842)) {
      at = 6705; l4876 = field(rt, l4845, "points");
      at = 6706; l4875 = l4876;
    } else {
      at = 6707; l4877 = K[1772];
      at = 6708; l4875 = l4877;
    }
    at = 6709; l4878 = K[1773];
    at = 6710; l4879 = rt.sub(l4849, l4878);
    at = 6711; l4880 = K[1774];
    at = 6712; l4881 = rt.max(l4880, l4879);
    at = 6713; l4882 = K[1775];
    at = 6714; l4883 = rt.array_slice(l4875, l4882, l4881);
    at = 6715; if (rt.truthy(l4842)) {
      at = 6716; l4885 = field(rt, l4845, "points");
      at = 6717; l4884 = l4885;
    } else {
      at = 6718; l4886 = K[1776];
      at = 6719; l4884 = l4886;
    }
    at = 6720; l4887 = K[1777];
    at = 6721; l4888 = rt.gt(l4849, l4887);
    at = 6722; if (rt.truthy(l4888)) {
      at = 6723; l4890 = K[1778];
      at = 6724; l4889 = l4890;
    } else {
      at = 6725; l4891 = K[1779];
      at = 6726; l4889 = l4891;
    }
    at = 6727; l4892 = K[1780];
    at = 6728; l4893 = rt.sub(l4849, l4892);
    at = 6729; l4894 = K[1781];
    at = 6730; l4895 = rt.max(l4894, l4893);
    at = 6731; l4896 = rt.array_slice(l4884, l4889, l4895);
    at = 6732; if (rt.truthy(l4842)) {
      at = 6733; l4898 = field(rt, l4845, "color");
      at = 6734; l4897 = l4898;
    } else {
      at = 6735; l4899 = K[1782];
      at = 6736; l4897 = l4899;
    }
    at = 6737; l4900 = K[1783];
    at = 6738; l4901 = rt.sequence_get(l4900, l4897);
    at = 6739; if (rt.truthy(l4842)) {
      at = 6740; l4903 = field(rt, l4845, "width");
      at = 6741; l4902 = l4903;
    } else {
      at = 6742; l4904 = K[1784];
      at = 6743; l4902 = l4904;
    }
    at = 6744; markSet(rt, true, "line", {from: l4883, to: l4896, color: l4901, width: l4902});
    at = 6745; l4905 = field(rt, l15, "strokes");
    at = 6746; l4906 = rt.sequence_length(l4905);
    at = 6747; l4907 = K[1785];
    at = 6748; l4908 = rt.lt(l4907, l4906);
    at = 6749; if (rt.truthy(l4908)) {
      at = 6750; l4909 = field(rt, l15, "strokes");
      at = 6751; l4910 = K[1786];
      at = 6752; l4911 = rt.sequence_get(l4909, l4910);
    }
    at = 6753; if (rt.truthy(l4908)) {
      at = 6754; l4913 = field(rt, l4911, "points");
      at = 6755; l4912 = l4913;
    } else {
      at = 6756; l4914 = K[1787];
      at = 6757; l4912 = l4914;
    }
    at = 6758; l4915 = rt.array_count(l4912);
    at = 6759; l4916 = K[1788];
    at = 6760; l4917 = rt.gt(l4915, l4916);
    at = 6761; if (rt.truthy(l4917)) {
      at = 6762; if (rt.truthy(l4908)) {
        at = 6763; l4920 = field(rt, l4911, "points");
        at = 6764; l4919 = l4920;
      } else {
        at = 6765; l4921 = K[1789];
        at = 6766; l4919 = l4921;
      }
      at = 6767; l4922 = K[1790];
      at = 6768; l4923 = rt.array_nth(l4919, l4922);
      at = 6769; l4918 = l4923;
    } else {
      at = 6770; l4924 = K[1791];
      at = 6771; l4918 = l4924;
    }
    at = 6772; if (rt.truthy(l4908)) {
      at = 6773; l4926 = field(rt, l4911, "width");
      at = 6774; l4925 = l4926;
    } else {
      at = 6775; l4927 = K[1792];
      at = 6776; l4925 = l4927;
    }
    at = 6777; l4928 = K[1793];
    at = 6778; l4929 = rt.int_div(l4925, l4928);
    at = 6779; l4930 = K[1794];
    at = 6780; l4931 = rt.max(l4930, l4929);
    at = 6781; l4932 = K[1795];
    at = 6782; l4933 = rt.gt(l4915, l4932);
    at = 6783; if (rt.truthy(l4933)) {
      at = 6784; if (rt.truthy(l4908)) {
        at = 6785; l4936 = field(rt, l4911, "color");
        at = 6786; l4935 = l4936;
      } else {
        at = 6787; l4937 = K[1796];
        at = 6788; l4935 = l4937;
      }
      at = 6789; l4938 = K[1797];
      at = 6790; l4939 = rt.sequence_get(l4938, l4935);
      at = 6791; l4934 = l4939;
    } else {
      at = 6792; l4940 = K[1798];
      at = 6793; l4934 = l4940;
    }
    at = 6794; markSet(rt, false, "circle", {at: l4918, radius: l4931, fill: l4934});
    at = 6795; if (rt.truthy(l4908)) {
      at = 6796; l4942 = field(rt, l4911, "points");
      at = 6797; l4941 = l4942;
    } else {
      at = 6798; l4943 = K[1799];
      at = 6799; l4941 = l4943;
    }
    at = 6800; l4944 = K[1800];
    at = 6801; l4945 = rt.sub(l4915, l4944);
    at = 6802; l4946 = K[1801];
    at = 6803; l4947 = rt.max(l4946, l4945);
    at = 6804; l4948 = K[1802];
    at = 6805; l4949 = rt.array_slice(l4941, l4948, l4947);
    at = 6806; if (rt.truthy(l4908)) {
      at = 6807; l4951 = field(rt, l4911, "points");
      at = 6808; l4950 = l4951;
    } else {
      at = 6809; l4952 = K[1803];
      at = 6810; l4950 = l4952;
    }
    at = 6811; l4953 = K[1804];
    at = 6812; l4954 = rt.gt(l4915, l4953);
    at = 6813; if (rt.truthy(l4954)) {
      at = 6814; l4956 = K[1805];
      at = 6815; l4955 = l4956;
    } else {
      at = 6816; l4957 = K[1806];
      at = 6817; l4955 = l4957;
    }
    at = 6818; l4958 = K[1807];
    at = 6819; l4959 = rt.sub(l4915, l4958);
    at = 6820; l4960 = K[1808];
    at = 6821; l4961 = rt.max(l4960, l4959);
    at = 6822; l4962 = rt.array_slice(l4950, l4955, l4961);
    at = 6823; if (rt.truthy(l4908)) {
      at = 6824; l4964 = field(rt, l4911, "color");
      at = 6825; l4963 = l4964;
    } else {
      at = 6826; l4965 = K[1809];
      at = 6827; l4963 = l4965;
    }
    at = 6828; l4966 = K[1810];
    at = 6829; l4967 = rt.sequence_get(l4966, l4963);
    at = 6830; if (rt.truthy(l4908)) {
      at = 6831; l4969 = field(rt, l4911, "width");
      at = 6832; l4968 = l4969;
    } else {
      at = 6833; l4970 = K[1811];
      at = 6834; l4968 = l4970;
    }
    at = 6835; markSet(rt, true, "line", {from: l4949, to: l4962, color: l4967, width: l4968});
    at = 6836; l4971 = field(rt, l15, "strokes");
    at = 6837; l4972 = rt.sequence_length(l4971);
    at = 6838; l4973 = K[1812];
    at = 6839; l4974 = rt.lt(l4973, l4972);
    at = 6840; if (rt.truthy(l4974)) {
      at = 6841; l4975 = field(rt, l15, "strokes");
      at = 6842; l4976 = K[1813];
      at = 6843; l4977 = rt.sequence_get(l4975, l4976);
    }
    at = 6844; if (rt.truthy(l4974)) {
      at = 6845; l4979 = field(rt, l4977, "points");
      at = 6846; l4978 = l4979;
    } else {
      at = 6847; l4980 = K[1814];
      at = 6848; l4978 = l4980;
    }
    at = 6849; l4981 = rt.array_count(l4978);
    at = 6850; l4982 = K[1815];
    at = 6851; l4983 = rt.gt(l4981, l4982);
    at = 6852; if (rt.truthy(l4983)) {
      at = 6853; if (rt.truthy(l4974)) {
        at = 6854; l4986 = field(rt, l4977, "points");
        at = 6855; l4985 = l4986;
      } else {
        at = 6856; l4987 = K[1816];
        at = 6857; l4985 = l4987;
      }
      at = 6858; l4988 = K[1817];
      at = 6859; l4989 = rt.array_nth(l4985, l4988);
      at = 6860; l4984 = l4989;
    } else {
      at = 6861; l4990 = K[1818];
      at = 6862; l4984 = l4990;
    }
    at = 6863; if (rt.truthy(l4974)) {
      at = 6864; l4992 = field(rt, l4977, "width");
      at = 6865; l4991 = l4992;
    } else {
      at = 6866; l4993 = K[1819];
      at = 6867; l4991 = l4993;
    }
    at = 6868; l4994 = K[1820];
    at = 6869; l4995 = rt.int_div(l4991, l4994);
    at = 6870; l4996 = K[1821];
    at = 6871; l4997 = rt.max(l4996, l4995);
    at = 6872; l4998 = K[1822];
    at = 6873; l4999 = rt.gt(l4981, l4998);
    at = 6874; if (rt.truthy(l4999)) {
      at = 6875; if (rt.truthy(l4974)) {
        at = 6876; l5002 = field(rt, l4977, "color");
        at = 6877; l5001 = l5002;
      } else {
        at = 6878; l5003 = K[1823];
        at = 6879; l5001 = l5003;
      }
      at = 6880; l5004 = K[1824];
      at = 6881; l5005 = rt.sequence_get(l5004, l5001);
      at = 6882; l5000 = l5005;
    } else {
      at = 6883; l5006 = K[1825];
      at = 6884; l5000 = l5006;
    }
    at = 6885; markSet(rt, false, "circle", {at: l4984, radius: l4997, fill: l5000});
    at = 6886; if (rt.truthy(l4974)) {
      at = 6887; l5008 = field(rt, l4977, "points");
      at = 6888; l5007 = l5008;
    } else {
      at = 6889; l5009 = K[1826];
      at = 6890; l5007 = l5009;
    }
    at = 6891; l5010 = K[1827];
    at = 6892; l5011 = rt.sub(l4981, l5010);
    at = 6893; l5012 = K[1828];
    at = 6894; l5013 = rt.max(l5012, l5011);
    at = 6895; l5014 = K[1829];
    at = 6896; l5015 = rt.array_slice(l5007, l5014, l5013);
    at = 6897; if (rt.truthy(l4974)) {
      at = 6898; l5017 = field(rt, l4977, "points");
      at = 6899; l5016 = l5017;
    } else {
      at = 6900; l5018 = K[1830];
      at = 6901; l5016 = l5018;
    }
    at = 6902; l5019 = K[1831];
    at = 6903; l5020 = rt.gt(l4981, l5019);
    at = 6904; if (rt.truthy(l5020)) {
      at = 6905; l5022 = K[1832];
      at = 6906; l5021 = l5022;
    } else {
      at = 6907; l5023 = K[1833];
      at = 6908; l5021 = l5023;
    }
    at = 6909; l5024 = K[1834];
    at = 6910; l5025 = rt.sub(l4981, l5024);
    at = 6911; l5026 = K[1835];
    at = 6912; l5027 = rt.max(l5026, l5025);
    at = 6913; l5028 = rt.array_slice(l5016, l5021, l5027);
    at = 6914; if (rt.truthy(l4974)) {
      at = 6915; l5030 = field(rt, l4977, "color");
      at = 6916; l5029 = l5030;
    } else {
      at = 6917; l5031 = K[1836];
      at = 6918; l5029 = l5031;
    }
    at = 6919; l5032 = K[1837];
    at = 6920; l5033 = rt.sequence_get(l5032, l5029);
    at = 6921; if (rt.truthy(l4974)) {
      at = 6922; l5035 = field(rt, l4977, "width");
      at = 6923; l5034 = l5035;
    } else {
      at = 6924; l5036 = K[1838];
      at = 6925; l5034 = l5036;
    }
    at = 6926; markSet(rt, true, "line", {from: l5015, to: l5028, color: l5033, width: l5034});
    at = 6927; l5037 = field(rt, l15, "strokes");
    at = 6928; l5038 = rt.sequence_length(l5037);
    at = 6929; l5039 = K[1839];
    at = 6930; l5040 = rt.lt(l5039, l5038);
    at = 6931; if (rt.truthy(l5040)) {
      at = 6932; l5041 = field(rt, l15, "strokes");
      at = 6933; l5042 = K[1840];
      at = 6934; l5043 = rt.sequence_get(l5041, l5042);
    }
    at = 6935; if (rt.truthy(l5040)) {
      at = 6936; l5045 = field(rt, l5043, "points");
      at = 6937; l5044 = l5045;
    } else {
      at = 6938; l5046 = K[1841];
      at = 6939; l5044 = l5046;
    }
    at = 6940; l5047 = rt.array_count(l5044);
    at = 6941; l5048 = K[1842];
    at = 6942; l5049 = rt.gt(l5047, l5048);
    at = 6943; if (rt.truthy(l5049)) {
      at = 6944; if (rt.truthy(l5040)) {
        at = 6945; l5052 = field(rt, l5043, "points");
        at = 6946; l5051 = l5052;
      } else {
        at = 6947; l5053 = K[1843];
        at = 6948; l5051 = l5053;
      }
      at = 6949; l5054 = K[1844];
      at = 6950; l5055 = rt.array_nth(l5051, l5054);
      at = 6951; l5050 = l5055;
    } else {
      at = 6952; l5056 = K[1845];
      at = 6953; l5050 = l5056;
    }
    at = 6954; if (rt.truthy(l5040)) {
      at = 6955; l5058 = field(rt, l5043, "width");
      at = 6956; l5057 = l5058;
    } else {
      at = 6957; l5059 = K[1846];
      at = 6958; l5057 = l5059;
    }
    at = 6959; l5060 = K[1847];
    at = 6960; l5061 = rt.int_div(l5057, l5060);
    at = 6961; l5062 = K[1848];
    at = 6962; l5063 = rt.max(l5062, l5061);
    at = 6963; l5064 = K[1849];
    at = 6964; l5065 = rt.gt(l5047, l5064);
    at = 6965; if (rt.truthy(l5065)) {
      at = 6966; if (rt.truthy(l5040)) {
        at = 6967; l5068 = field(rt, l5043, "color");
        at = 6968; l5067 = l5068;
      } else {
        at = 6969; l5069 = K[1850];
        at = 6970; l5067 = l5069;
      }
      at = 6971; l5070 = K[1851];
      at = 6972; l5071 = rt.sequence_get(l5070, l5067);
      at = 6973; l5066 = l5071;
    } else {
      at = 6974; l5072 = K[1852];
      at = 6975; l5066 = l5072;
    }
    at = 6976; markSet(rt, false, "circle", {at: l5050, radius: l5063, fill: l5066});
    at = 6977; if (rt.truthy(l5040)) {
      at = 6978; l5074 = field(rt, l5043, "points");
      at = 6979; l5073 = l5074;
    } else {
      at = 6980; l5075 = K[1853];
      at = 6981; l5073 = l5075;
    }
    at = 6982; l5076 = K[1854];
    at = 6983; l5077 = rt.sub(l5047, l5076);
    at = 6984; l5078 = K[1855];
    at = 6985; l5079 = rt.max(l5078, l5077);
    at = 6986; l5080 = K[1856];
    at = 6987; l5081 = rt.array_slice(l5073, l5080, l5079);
    at = 6988; if (rt.truthy(l5040)) {
      at = 6989; l5083 = field(rt, l5043, "points");
      at = 6990; l5082 = l5083;
    } else {
      at = 6991; l5084 = K[1857];
      at = 6992; l5082 = l5084;
    }
    at = 6993; l5085 = K[1858];
    at = 6994; l5086 = rt.gt(l5047, l5085);
    at = 6995; if (rt.truthy(l5086)) {
      at = 6996; l5088 = K[1859];
      at = 6997; l5087 = l5088;
    } else {
      at = 6998; l5089 = K[1860];
      at = 6999; l5087 = l5089;
    }
    at = 7000; l5090 = K[1861];
    at = 7001; l5091 = rt.sub(l5047, l5090);
    at = 7002; l5092 = K[1862];
    at = 7003; l5093 = rt.max(l5092, l5091);
    at = 7004; l5094 = rt.array_slice(l5082, l5087, l5093);
    at = 7005; if (rt.truthy(l5040)) {
      at = 7006; l5096 = field(rt, l5043, "color");
      at = 7007; l5095 = l5096;
    } else {
      at = 7008; l5097 = K[1863];
      at = 7009; l5095 = l5097;
    }
    at = 7010; l5098 = K[1864];
    at = 7011; l5099 = rt.sequence_get(l5098, l5095);
    at = 7012; if (rt.truthy(l5040)) {
      at = 7013; l5101 = field(rt, l5043, "width");
      at = 7014; l5100 = l5101;
    } else {
      at = 7015; l5102 = K[1865];
      at = 7016; l5100 = l5102;
    }
    at = 7017; markSet(rt, true, "line", {from: l5081, to: l5094, color: l5099, width: l5100});
    at = 7018; l5103 = field(rt, l15, "strokes");
    at = 7019; l5104 = rt.sequence_length(l5103);
    at = 7020; l5105 = K[1866];
    at = 7021; l5106 = rt.lt(l5105, l5104);
    at = 7022; if (rt.truthy(l5106)) {
      at = 7023; l5107 = field(rt, l15, "strokes");
      at = 7024; l5108 = K[1867];
      at = 7025; l5109 = rt.sequence_get(l5107, l5108);
    }
    at = 7026; if (rt.truthy(l5106)) {
      at = 7027; l5111 = field(rt, l5109, "points");
      at = 7028; l5110 = l5111;
    } else {
      at = 7029; l5112 = K[1868];
      at = 7030; l5110 = l5112;
    }
    at = 7031; l5113 = rt.array_count(l5110);
    at = 7032; l5114 = K[1869];
    at = 7033; l5115 = rt.gt(l5113, l5114);
    at = 7034; if (rt.truthy(l5115)) {
      at = 7035; if (rt.truthy(l5106)) {
        at = 7036; l5118 = field(rt, l5109, "points");
        at = 7037; l5117 = l5118;
      } else {
        at = 7038; l5119 = K[1870];
        at = 7039; l5117 = l5119;
      }
      at = 7040; l5120 = K[1871];
      at = 7041; l5121 = rt.array_nth(l5117, l5120);
      at = 7042; l5116 = l5121;
    } else {
      at = 7043; l5122 = K[1872];
      at = 7044; l5116 = l5122;
    }
    at = 7045; if (rt.truthy(l5106)) {
      at = 7046; l5124 = field(rt, l5109, "width");
      at = 7047; l5123 = l5124;
    } else {
      at = 7048; l5125 = K[1873];
      at = 7049; l5123 = l5125;
    }
    at = 7050; l5126 = K[1874];
    at = 7051; l5127 = rt.int_div(l5123, l5126);
    at = 7052; l5128 = K[1875];
    at = 7053; l5129 = rt.max(l5128, l5127);
    at = 7054; l5130 = K[1876];
    at = 7055; l5131 = rt.gt(l5113, l5130);
    at = 7056; if (rt.truthy(l5131)) {
      at = 7057; if (rt.truthy(l5106)) {
        at = 7058; l5134 = field(rt, l5109, "color");
        at = 7059; l5133 = l5134;
      } else {
        at = 7060; l5135 = K[1877];
        at = 7061; l5133 = l5135;
      }
      at = 7062; l5136 = K[1878];
      at = 7063; l5137 = rt.sequence_get(l5136, l5133);
      at = 7064; l5132 = l5137;
    } else {
      at = 7065; l5138 = K[1879];
      at = 7066; l5132 = l5138;
    }
    at = 7067; markSet(rt, false, "circle", {at: l5116, radius: l5129, fill: l5132});
    at = 7068; if (rt.truthy(l5106)) {
      at = 7069; l5140 = field(rt, l5109, "points");
      at = 7070; l5139 = l5140;
    } else {
      at = 7071; l5141 = K[1880];
      at = 7072; l5139 = l5141;
    }
    at = 7073; l5142 = K[1881];
    at = 7074; l5143 = rt.sub(l5113, l5142);
    at = 7075; l5144 = K[1882];
    at = 7076; l5145 = rt.max(l5144, l5143);
    at = 7077; l5146 = K[1883];
    at = 7078; l5147 = rt.array_slice(l5139, l5146, l5145);
    at = 7079; if (rt.truthy(l5106)) {
      at = 7080; l5149 = field(rt, l5109, "points");
      at = 7081; l5148 = l5149;
    } else {
      at = 7082; l5150 = K[1884];
      at = 7083; l5148 = l5150;
    }
    at = 7084; l5151 = K[1885];
    at = 7085; l5152 = rt.gt(l5113, l5151);
    at = 7086; if (rt.truthy(l5152)) {
      at = 7087; l5154 = K[1886];
      at = 7088; l5153 = l5154;
    } else {
      at = 7089; l5155 = K[1887];
      at = 7090; l5153 = l5155;
    }
    at = 7091; l5156 = K[1888];
    at = 7092; l5157 = rt.sub(l5113, l5156);
    at = 7093; l5158 = K[1889];
    at = 7094; l5159 = rt.max(l5158, l5157);
    at = 7095; l5160 = rt.array_slice(l5148, l5153, l5159);
    at = 7096; if (rt.truthy(l5106)) {
      at = 7097; l5162 = field(rt, l5109, "color");
      at = 7098; l5161 = l5162;
    } else {
      at = 7099; l5163 = K[1890];
      at = 7100; l5161 = l5163;
    }
    at = 7101; l5164 = K[1891];
    at = 7102; l5165 = rt.sequence_get(l5164, l5161);
    at = 7103; if (rt.truthy(l5106)) {
      at = 7104; l5167 = field(rt, l5109, "width");
      at = 7105; l5166 = l5167;
    } else {
      at = 7106; l5168 = K[1892];
      at = 7107; l5166 = l5168;
    }
    at = 7108; markSet(rt, true, "line", {from: l5147, to: l5160, color: l5165, width: l5166});
    at = 7109; l5169 = field(rt, l15, "strokes");
    at = 7110; l5170 = rt.sequence_length(l5169);
    at = 7111; l5171 = K[1893];
    at = 7112; l5172 = rt.lt(l5171, l5170);
    at = 7113; if (rt.truthy(l5172)) {
      at = 7114; l5173 = field(rt, l15, "strokes");
      at = 7115; l5174 = K[1894];
      at = 7116; l5175 = rt.sequence_get(l5173, l5174);
    }
    at = 7117; if (rt.truthy(l5172)) {
      at = 7118; l5177 = field(rt, l5175, "points");
      at = 7119; l5176 = l5177;
    } else {
      at = 7120; l5178 = K[1895];
      at = 7121; l5176 = l5178;
    }
    at = 7122; l5179 = rt.array_count(l5176);
    at = 7123; l5180 = K[1896];
    at = 7124; l5181 = rt.gt(l5179, l5180);
    at = 7125; if (rt.truthy(l5181)) {
      at = 7126; if (rt.truthy(l5172)) {
        at = 7127; l5184 = field(rt, l5175, "points");
        at = 7128; l5183 = l5184;
      } else {
        at = 7129; l5185 = K[1897];
        at = 7130; l5183 = l5185;
      }
      at = 7131; l5186 = K[1898];
      at = 7132; l5187 = rt.array_nth(l5183, l5186);
      at = 7133; l5182 = l5187;
    } else {
      at = 7134; l5188 = K[1899];
      at = 7135; l5182 = l5188;
    }
    at = 7136; if (rt.truthy(l5172)) {
      at = 7137; l5190 = field(rt, l5175, "width");
      at = 7138; l5189 = l5190;
    } else {
      at = 7139; l5191 = K[1900];
      at = 7140; l5189 = l5191;
    }
    at = 7141; l5192 = K[1901];
    at = 7142; l5193 = rt.int_div(l5189, l5192);
    at = 7143; l5194 = K[1902];
    at = 7144; l5195 = rt.max(l5194, l5193);
    at = 7145; l5196 = K[1903];
    at = 7146; l5197 = rt.gt(l5179, l5196);
    at = 7147; if (rt.truthy(l5197)) {
      at = 7148; if (rt.truthy(l5172)) {
        at = 7149; l5200 = field(rt, l5175, "color");
        at = 7150; l5199 = l5200;
      } else {
        at = 7151; l5201 = K[1904];
        at = 7152; l5199 = l5201;
      }
      at = 7153; l5202 = K[1905];
      at = 7154; l5203 = rt.sequence_get(l5202, l5199);
      at = 7155; l5198 = l5203;
    } else {
      at = 7156; l5204 = K[1906];
      at = 7157; l5198 = l5204;
    }
    at = 7158; markSet(rt, false, "circle", {at: l5182, radius: l5195, fill: l5198});
    at = 7159; if (rt.truthy(l5172)) {
      at = 7160; l5206 = field(rt, l5175, "points");
      at = 7161; l5205 = l5206;
    } else {
      at = 7162; l5207 = K[1907];
      at = 7163; l5205 = l5207;
    }
    at = 7164; l5208 = K[1908];
    at = 7165; l5209 = rt.sub(l5179, l5208);
    at = 7166; l5210 = K[1909];
    at = 7167; l5211 = rt.max(l5210, l5209);
    at = 7168; l5212 = K[1910];
    at = 7169; l5213 = rt.array_slice(l5205, l5212, l5211);
    at = 7170; if (rt.truthy(l5172)) {
      at = 7171; l5215 = field(rt, l5175, "points");
      at = 7172; l5214 = l5215;
    } else {
      at = 7173; l5216 = K[1911];
      at = 7174; l5214 = l5216;
    }
    at = 7175; l5217 = K[1912];
    at = 7176; l5218 = rt.gt(l5179, l5217);
    at = 7177; if (rt.truthy(l5218)) {
      at = 7178; l5220 = K[1913];
      at = 7179; l5219 = l5220;
    } else {
      at = 7180; l5221 = K[1914];
      at = 7181; l5219 = l5221;
    }
    at = 7182; l5222 = K[1915];
    at = 7183; l5223 = rt.sub(l5179, l5222);
    at = 7184; l5224 = K[1916];
    at = 7185; l5225 = rt.max(l5224, l5223);
    at = 7186; l5226 = rt.array_slice(l5214, l5219, l5225);
    at = 7187; if (rt.truthy(l5172)) {
      at = 7188; l5228 = field(rt, l5175, "color");
      at = 7189; l5227 = l5228;
    } else {
      at = 7190; l5229 = K[1917];
      at = 7191; l5227 = l5229;
    }
    at = 7192; l5230 = K[1918];
    at = 7193; l5231 = rt.sequence_get(l5230, l5227);
    at = 7194; if (rt.truthy(l5172)) {
      at = 7195; l5233 = field(rt, l5175, "width");
      at = 7196; l5232 = l5233;
    } else {
      at = 7197; l5234 = K[1919];
      at = 7198; l5232 = l5234;
    }
    at = 7199; markSet(rt, true, "line", {from: l5213, to: l5226, color: l5231, width: l5232});
    at = 7200; l5235 = field(rt, l15, "strokes");
    at = 7201; l5236 = rt.sequence_length(l5235);
    at = 7202; l5237 = K[1920];
    at = 7203; l5238 = rt.lt(l5237, l5236);
    at = 7204; if (rt.truthy(l5238)) {
      at = 7205; l5239 = field(rt, l15, "strokes");
      at = 7206; l5240 = K[1921];
      at = 7207; l5241 = rt.sequence_get(l5239, l5240);
    }
    at = 7208; if (rt.truthy(l5238)) {
      at = 7209; l5243 = field(rt, l5241, "points");
      at = 7210; l5242 = l5243;
    } else {
      at = 7211; l5244 = K[1922];
      at = 7212; l5242 = l5244;
    }
    at = 7213; l5245 = rt.array_count(l5242);
    at = 7214; l5246 = K[1923];
    at = 7215; l5247 = rt.gt(l5245, l5246);
    at = 7216; if (rt.truthy(l5247)) {
      at = 7217; if (rt.truthy(l5238)) {
        at = 7218; l5250 = field(rt, l5241, "points");
        at = 7219; l5249 = l5250;
      } else {
        at = 7220; l5251 = K[1924];
        at = 7221; l5249 = l5251;
      }
      at = 7222; l5252 = K[1925];
      at = 7223; l5253 = rt.array_nth(l5249, l5252);
      at = 7224; l5248 = l5253;
    } else {
      at = 7225; l5254 = K[1926];
      at = 7226; l5248 = l5254;
    }
    at = 7227; if (rt.truthy(l5238)) {
      at = 7228; l5256 = field(rt, l5241, "width");
      at = 7229; l5255 = l5256;
    } else {
      at = 7230; l5257 = K[1927];
      at = 7231; l5255 = l5257;
    }
    at = 7232; l5258 = K[1928];
    at = 7233; l5259 = rt.int_div(l5255, l5258);
    at = 7234; l5260 = K[1929];
    at = 7235; l5261 = rt.max(l5260, l5259);
    at = 7236; l5262 = K[1930];
    at = 7237; l5263 = rt.gt(l5245, l5262);
    at = 7238; if (rt.truthy(l5263)) {
      at = 7239; if (rt.truthy(l5238)) {
        at = 7240; l5266 = field(rt, l5241, "color");
        at = 7241; l5265 = l5266;
      } else {
        at = 7242; l5267 = K[1931];
        at = 7243; l5265 = l5267;
      }
      at = 7244; l5268 = K[1932];
      at = 7245; l5269 = rt.sequence_get(l5268, l5265);
      at = 7246; l5264 = l5269;
    } else {
      at = 7247; l5270 = K[1933];
      at = 7248; l5264 = l5270;
    }
    at = 7249; markSet(rt, false, "circle", {at: l5248, radius: l5261, fill: l5264});
    at = 7250; if (rt.truthy(l5238)) {
      at = 7251; l5272 = field(rt, l5241, "points");
      at = 7252; l5271 = l5272;
    } else {
      at = 7253; l5273 = K[1934];
      at = 7254; l5271 = l5273;
    }
    at = 7255; l5274 = K[1935];
    at = 7256; l5275 = rt.sub(l5245, l5274);
    at = 7257; l5276 = K[1936];
    at = 7258; l5277 = rt.max(l5276, l5275);
    at = 7259; l5278 = K[1937];
    at = 7260; l5279 = rt.array_slice(l5271, l5278, l5277);
    at = 7261; if (rt.truthy(l5238)) {
      at = 7262; l5281 = field(rt, l5241, "points");
      at = 7263; l5280 = l5281;
    } else {
      at = 7264; l5282 = K[1938];
      at = 7265; l5280 = l5282;
    }
    at = 7266; l5283 = K[1939];
    at = 7267; l5284 = rt.gt(l5245, l5283);
    at = 7268; if (rt.truthy(l5284)) {
      at = 7269; l5286 = K[1940];
      at = 7270; l5285 = l5286;
    } else {
      at = 7271; l5287 = K[1941];
      at = 7272; l5285 = l5287;
    }
    at = 7273; l5288 = K[1942];
    at = 7274; l5289 = rt.sub(l5245, l5288);
    at = 7275; l5290 = K[1943];
    at = 7276; l5291 = rt.max(l5290, l5289);
    at = 7277; l5292 = rt.array_slice(l5280, l5285, l5291);
    at = 7278; if (rt.truthy(l5238)) {
      at = 7279; l5294 = field(rt, l5241, "color");
      at = 7280; l5293 = l5294;
    } else {
      at = 7281; l5295 = K[1944];
      at = 7282; l5293 = l5295;
    }
    at = 7283; l5296 = K[1945];
    at = 7284; l5297 = rt.sequence_get(l5296, l5293);
    at = 7285; if (rt.truthy(l5238)) {
      at = 7286; l5299 = field(rt, l5241, "width");
      at = 7287; l5298 = l5299;
    } else {
      at = 7288; l5300 = K[1946];
      at = 7289; l5298 = l5300;
    }
    at = 7290; markSet(rt, true, "line", {from: l5279, to: l5292, color: l5297, width: l5298});
    at = 7291; l5301 = field(rt, l15, "strokes");
    at = 7292; l5302 = rt.sequence_length(l5301);
    at = 7293; l5303 = K[1947];
    at = 7294; l5304 = rt.lt(l5303, l5302);
    at = 7295; if (rt.truthy(l5304)) {
      at = 7296; l5305 = field(rt, l15, "strokes");
      at = 7297; l5306 = K[1948];
      at = 7298; l5307 = rt.sequence_get(l5305, l5306);
    }
    at = 7299; if (rt.truthy(l5304)) {
      at = 7300; l5309 = field(rt, l5307, "points");
      at = 7301; l5308 = l5309;
    } else {
      at = 7302; l5310 = K[1949];
      at = 7303; l5308 = l5310;
    }
    at = 7304; l5311 = rt.array_count(l5308);
    at = 7305; l5312 = K[1950];
    at = 7306; l5313 = rt.gt(l5311, l5312);
    at = 7307; if (rt.truthy(l5313)) {
      at = 7308; if (rt.truthy(l5304)) {
        at = 7309; l5316 = field(rt, l5307, "points");
        at = 7310; l5315 = l5316;
      } else {
        at = 7311; l5317 = K[1951];
        at = 7312; l5315 = l5317;
      }
      at = 7313; l5318 = K[1952];
      at = 7314; l5319 = rt.array_nth(l5315, l5318);
      at = 7315; l5314 = l5319;
    } else {
      at = 7316; l5320 = K[1953];
      at = 7317; l5314 = l5320;
    }
    at = 7318; if (rt.truthy(l5304)) {
      at = 7319; l5322 = field(rt, l5307, "width");
      at = 7320; l5321 = l5322;
    } else {
      at = 7321; l5323 = K[1954];
      at = 7322; l5321 = l5323;
    }
    at = 7323; l5324 = K[1955];
    at = 7324; l5325 = rt.int_div(l5321, l5324);
    at = 7325; l5326 = K[1956];
    at = 7326; l5327 = rt.max(l5326, l5325);
    at = 7327; l5328 = K[1957];
    at = 7328; l5329 = rt.gt(l5311, l5328);
    at = 7329; if (rt.truthy(l5329)) {
      at = 7330; if (rt.truthy(l5304)) {
        at = 7331; l5332 = field(rt, l5307, "color");
        at = 7332; l5331 = l5332;
      } else {
        at = 7333; l5333 = K[1958];
        at = 7334; l5331 = l5333;
      }
      at = 7335; l5334 = K[1959];
      at = 7336; l5335 = rt.sequence_get(l5334, l5331);
      at = 7337; l5330 = l5335;
    } else {
      at = 7338; l5336 = K[1960];
      at = 7339; l5330 = l5336;
    }
    at = 7340; markSet(rt, false, "circle", {at: l5314, radius: l5327, fill: l5330});
    at = 7341; if (rt.truthy(l5304)) {
      at = 7342; l5338 = field(rt, l5307, "points");
      at = 7343; l5337 = l5338;
    } else {
      at = 7344; l5339 = K[1961];
      at = 7345; l5337 = l5339;
    }
    at = 7346; l5340 = K[1962];
    at = 7347; l5341 = rt.sub(l5311, l5340);
    at = 7348; l5342 = K[1963];
    at = 7349; l5343 = rt.max(l5342, l5341);
    at = 7350; l5344 = K[1964];
    at = 7351; l5345 = rt.array_slice(l5337, l5344, l5343);
    at = 7352; if (rt.truthy(l5304)) {
      at = 7353; l5347 = field(rt, l5307, "points");
      at = 7354; l5346 = l5347;
    } else {
      at = 7355; l5348 = K[1965];
      at = 7356; l5346 = l5348;
    }
    at = 7357; l5349 = K[1966];
    at = 7358; l5350 = rt.gt(l5311, l5349);
    at = 7359; if (rt.truthy(l5350)) {
      at = 7360; l5352 = K[1967];
      at = 7361; l5351 = l5352;
    } else {
      at = 7362; l5353 = K[1968];
      at = 7363; l5351 = l5353;
    }
    at = 7364; l5354 = K[1969];
    at = 7365; l5355 = rt.sub(l5311, l5354);
    at = 7366; l5356 = K[1970];
    at = 7367; l5357 = rt.max(l5356, l5355);
    at = 7368; l5358 = rt.array_slice(l5346, l5351, l5357);
    at = 7369; if (rt.truthy(l5304)) {
      at = 7370; l5360 = field(rt, l5307, "color");
      at = 7371; l5359 = l5360;
    } else {
      at = 7372; l5361 = K[1971];
      at = 7373; l5359 = l5361;
    }
    at = 7374; l5362 = K[1972];
    at = 7375; l5363 = rt.sequence_get(l5362, l5359);
    at = 7376; if (rt.truthy(l5304)) {
      at = 7377; l5365 = field(rt, l5307, "width");
      at = 7378; l5364 = l5365;
    } else {
      at = 7379; l5366 = K[1973];
      at = 7380; l5364 = l5366;
    }
    at = 7381; markSet(rt, true, "line", {from: l5345, to: l5358, color: l5363, width: l5364});
    at = 7382; l5367 = field(rt, l15, "strokes");
    at = 7383; l5368 = rt.sequence_length(l5367);
    at = 7384; l5369 = K[1974];
    at = 7385; l5370 = rt.lt(l5369, l5368);
    at = 7386; if (rt.truthy(l5370)) {
      at = 7387; l5371 = field(rt, l15, "strokes");
      at = 7388; l5372 = K[1975];
      at = 7389; l5373 = rt.sequence_get(l5371, l5372);
    }
    at = 7390; if (rt.truthy(l5370)) {
      at = 7391; l5375 = field(rt, l5373, "points");
      at = 7392; l5374 = l5375;
    } else {
      at = 7393; l5376 = K[1976];
      at = 7394; l5374 = l5376;
    }
    at = 7395; l5377 = rt.array_count(l5374);
    at = 7396; l5378 = K[1977];
    at = 7397; l5379 = rt.gt(l5377, l5378);
    at = 7398; if (rt.truthy(l5379)) {
      at = 7399; if (rt.truthy(l5370)) {
        at = 7400; l5382 = field(rt, l5373, "points");
        at = 7401; l5381 = l5382;
      } else {
        at = 7402; l5383 = K[1978];
        at = 7403; l5381 = l5383;
      }
      at = 7404; l5384 = K[1979];
      at = 7405; l5385 = rt.array_nth(l5381, l5384);
      at = 7406; l5380 = l5385;
    } else {
      at = 7407; l5386 = K[1980];
      at = 7408; l5380 = l5386;
    }
    at = 7409; if (rt.truthy(l5370)) {
      at = 7410; l5388 = field(rt, l5373, "width");
      at = 7411; l5387 = l5388;
    } else {
      at = 7412; l5389 = K[1981];
      at = 7413; l5387 = l5389;
    }
    at = 7414; l5390 = K[1982];
    at = 7415; l5391 = rt.int_div(l5387, l5390);
    at = 7416; l5392 = K[1983];
    at = 7417; l5393 = rt.max(l5392, l5391);
    at = 7418; l5394 = K[1984];
    at = 7419; l5395 = rt.gt(l5377, l5394);
    at = 7420; if (rt.truthy(l5395)) {
      at = 7421; if (rt.truthy(l5370)) {
        at = 7422; l5398 = field(rt, l5373, "color");
        at = 7423; l5397 = l5398;
      } else {
        at = 7424; l5399 = K[1985];
        at = 7425; l5397 = l5399;
      }
      at = 7426; l5400 = K[1986];
      at = 7427; l5401 = rt.sequence_get(l5400, l5397);
      at = 7428; l5396 = l5401;
    } else {
      at = 7429; l5402 = K[1987];
      at = 7430; l5396 = l5402;
    }
    at = 7431; markSet(rt, false, "circle", {at: l5380, radius: l5393, fill: l5396});
    at = 7432; if (rt.truthy(l5370)) {
      at = 7433; l5404 = field(rt, l5373, "points");
      at = 7434; l5403 = l5404;
    } else {
      at = 7435; l5405 = K[1988];
      at = 7436; l5403 = l5405;
    }
    at = 7437; l5406 = K[1989];
    at = 7438; l5407 = rt.sub(l5377, l5406);
    at = 7439; l5408 = K[1990];
    at = 7440; l5409 = rt.max(l5408, l5407);
    at = 7441; l5410 = K[1991];
    at = 7442; l5411 = rt.array_slice(l5403, l5410, l5409);
    at = 7443; if (rt.truthy(l5370)) {
      at = 7444; l5413 = field(rt, l5373, "points");
      at = 7445; l5412 = l5413;
    } else {
      at = 7446; l5414 = K[1992];
      at = 7447; l5412 = l5414;
    }
    at = 7448; l5415 = K[1993];
    at = 7449; l5416 = rt.gt(l5377, l5415);
    at = 7450; if (rt.truthy(l5416)) {
      at = 7451; l5418 = K[1994];
      at = 7452; l5417 = l5418;
    } else {
      at = 7453; l5419 = K[1995];
      at = 7454; l5417 = l5419;
    }
    at = 7455; l5420 = K[1996];
    at = 7456; l5421 = rt.sub(l5377, l5420);
    at = 7457; l5422 = K[1997];
    at = 7458; l5423 = rt.max(l5422, l5421);
    at = 7459; l5424 = rt.array_slice(l5412, l5417, l5423);
    at = 7460; if (rt.truthy(l5370)) {
      at = 7461; l5426 = field(rt, l5373, "color");
      at = 7462; l5425 = l5426;
    } else {
      at = 7463; l5427 = K[1998];
      at = 7464; l5425 = l5427;
    }
    at = 7465; l5428 = K[1999];
    at = 7466; l5429 = rt.sequence_get(l5428, l5425);
    at = 7467; if (rt.truthy(l5370)) {
      at = 7468; l5431 = field(rt, l5373, "width");
      at = 7469; l5430 = l5431;
    } else {
      at = 7470; l5432 = K[2000];
      at = 7471; l5430 = l5432;
    }
    at = 7472; markSet(rt, true, "line", {from: l5411, to: l5424, color: l5429, width: l5430});
    at = 7473; l5433 = field(rt, l15, "strokes");
    at = 7474; l5434 = rt.sequence_length(l5433);
    at = 7475; l5435 = K[2001];
    at = 7476; l5436 = rt.lt(l5435, l5434);
    at = 7477; if (rt.truthy(l5436)) {
      at = 7478; l5437 = field(rt, l15, "strokes");
      at = 7479; l5438 = K[2002];
      at = 7480; l5439 = rt.sequence_get(l5437, l5438);
    }
    at = 7481; if (rt.truthy(l5436)) {
      at = 7482; l5441 = field(rt, l5439, "points");
      at = 7483; l5440 = l5441;
    } else {
      at = 7484; l5442 = K[2003];
      at = 7485; l5440 = l5442;
    }
    at = 7486; l5443 = rt.array_count(l5440);
    at = 7487; l5444 = K[2004];
    at = 7488; l5445 = rt.gt(l5443, l5444);
    at = 7489; if (rt.truthy(l5445)) {
      at = 7490; if (rt.truthy(l5436)) {
        at = 7491; l5448 = field(rt, l5439, "points");
        at = 7492; l5447 = l5448;
      } else {
        at = 7493; l5449 = K[2005];
        at = 7494; l5447 = l5449;
      }
      at = 7495; l5450 = K[2006];
      at = 7496; l5451 = rt.array_nth(l5447, l5450);
      at = 7497; l5446 = l5451;
    } else {
      at = 7498; l5452 = K[2007];
      at = 7499; l5446 = l5452;
    }
    at = 7500; if (rt.truthy(l5436)) {
      at = 7501; l5454 = field(rt, l5439, "width");
      at = 7502; l5453 = l5454;
    } else {
      at = 7503; l5455 = K[2008];
      at = 7504; l5453 = l5455;
    }
    at = 7505; l5456 = K[2009];
    at = 7506; l5457 = rt.int_div(l5453, l5456);
    at = 7507; l5458 = K[2010];
    at = 7508; l5459 = rt.max(l5458, l5457);
    at = 7509; l5460 = K[2011];
    at = 7510; l5461 = rt.gt(l5443, l5460);
    at = 7511; if (rt.truthy(l5461)) {
      at = 7512; if (rt.truthy(l5436)) {
        at = 7513; l5464 = field(rt, l5439, "color");
        at = 7514; l5463 = l5464;
      } else {
        at = 7515; l5465 = K[2012];
        at = 7516; l5463 = l5465;
      }
      at = 7517; l5466 = K[2013];
      at = 7518; l5467 = rt.sequence_get(l5466, l5463);
      at = 7519; l5462 = l5467;
    } else {
      at = 7520; l5468 = K[2014];
      at = 7521; l5462 = l5468;
    }
    at = 7522; markSet(rt, false, "circle", {at: l5446, radius: l5459, fill: l5462});
    at = 7523; if (rt.truthy(l5436)) {
      at = 7524; l5470 = field(rt, l5439, "points");
      at = 7525; l5469 = l5470;
    } else {
      at = 7526; l5471 = K[2015];
      at = 7527; l5469 = l5471;
    }
    at = 7528; l5472 = K[2016];
    at = 7529; l5473 = rt.sub(l5443, l5472);
    at = 7530; l5474 = K[2017];
    at = 7531; l5475 = rt.max(l5474, l5473);
    at = 7532; l5476 = K[2018];
    at = 7533; l5477 = rt.array_slice(l5469, l5476, l5475);
    at = 7534; if (rt.truthy(l5436)) {
      at = 7535; l5479 = field(rt, l5439, "points");
      at = 7536; l5478 = l5479;
    } else {
      at = 7537; l5480 = K[2019];
      at = 7538; l5478 = l5480;
    }
    at = 7539; l5481 = K[2020];
    at = 7540; l5482 = rt.gt(l5443, l5481);
    at = 7541; if (rt.truthy(l5482)) {
      at = 7542; l5484 = K[2021];
      at = 7543; l5483 = l5484;
    } else {
      at = 7544; l5485 = K[2022];
      at = 7545; l5483 = l5485;
    }
    at = 7546; l5486 = K[2023];
    at = 7547; l5487 = rt.sub(l5443, l5486);
    at = 7548; l5488 = K[2024];
    at = 7549; l5489 = rt.max(l5488, l5487);
    at = 7550; l5490 = rt.array_slice(l5478, l5483, l5489);
    at = 7551; if (rt.truthy(l5436)) {
      at = 7552; l5492 = field(rt, l5439, "color");
      at = 7553; l5491 = l5492;
    } else {
      at = 7554; l5493 = K[2025];
      at = 7555; l5491 = l5493;
    }
    at = 7556; l5494 = K[2026];
    at = 7557; l5495 = rt.sequence_get(l5494, l5491);
    at = 7558; if (rt.truthy(l5436)) {
      at = 7559; l5497 = field(rt, l5439, "width");
      at = 7560; l5496 = l5497;
    } else {
      at = 7561; l5498 = K[2027];
      at = 7562; l5496 = l5498;
    }
    at = 7563; markSet(rt, true, "line", {from: l5477, to: l5490, color: l5495, width: l5496});
    at = 7564; l5499 = field(rt, l15, "strokes");
    at = 7565; l5500 = rt.sequence_length(l5499);
    at = 7566; l5501 = K[2028];
    at = 7567; l5502 = rt.lt(l5501, l5500);
    at = 7568; if (rt.truthy(l5502)) {
      at = 7569; l5503 = field(rt, l15, "strokes");
      at = 7570; l5504 = K[2029];
      at = 7571; l5505 = rt.sequence_get(l5503, l5504);
    }
    at = 7572; if (rt.truthy(l5502)) {
      at = 7573; l5507 = field(rt, l5505, "points");
      at = 7574; l5506 = l5507;
    } else {
      at = 7575; l5508 = K[2030];
      at = 7576; l5506 = l5508;
    }
    at = 7577; l5509 = rt.array_count(l5506);
    at = 7578; l5510 = K[2031];
    at = 7579; l5511 = rt.gt(l5509, l5510);
    at = 7580; if (rt.truthy(l5511)) {
      at = 7581; if (rt.truthy(l5502)) {
        at = 7582; l5514 = field(rt, l5505, "points");
        at = 7583; l5513 = l5514;
      } else {
        at = 7584; l5515 = K[2032];
        at = 7585; l5513 = l5515;
      }
      at = 7586; l5516 = K[2033];
      at = 7587; l5517 = rt.array_nth(l5513, l5516);
      at = 7588; l5512 = l5517;
    } else {
      at = 7589; l5518 = K[2034];
      at = 7590; l5512 = l5518;
    }
    at = 7591; if (rt.truthy(l5502)) {
      at = 7592; l5520 = field(rt, l5505, "width");
      at = 7593; l5519 = l5520;
    } else {
      at = 7594; l5521 = K[2035];
      at = 7595; l5519 = l5521;
    }
    at = 7596; l5522 = K[2036];
    at = 7597; l5523 = rt.int_div(l5519, l5522);
    at = 7598; l5524 = K[2037];
    at = 7599; l5525 = rt.max(l5524, l5523);
    at = 7600; l5526 = K[2038];
    at = 7601; l5527 = rt.gt(l5509, l5526);
    at = 7602; if (rt.truthy(l5527)) {
      at = 7603; if (rt.truthy(l5502)) {
        at = 7604; l5530 = field(rt, l5505, "color");
        at = 7605; l5529 = l5530;
      } else {
        at = 7606; l5531 = K[2039];
        at = 7607; l5529 = l5531;
      }
      at = 7608; l5532 = K[2040];
      at = 7609; l5533 = rt.sequence_get(l5532, l5529);
      at = 7610; l5528 = l5533;
    } else {
      at = 7611; l5534 = K[2041];
      at = 7612; l5528 = l5534;
    }
    at = 7613; markSet(rt, false, "circle", {at: l5512, radius: l5525, fill: l5528});
    at = 7614; if (rt.truthy(l5502)) {
      at = 7615; l5536 = field(rt, l5505, "points");
      at = 7616; l5535 = l5536;
    } else {
      at = 7617; l5537 = K[2042];
      at = 7618; l5535 = l5537;
    }
    at = 7619; l5538 = K[2043];
    at = 7620; l5539 = rt.sub(l5509, l5538);
    at = 7621; l5540 = K[2044];
    at = 7622; l5541 = rt.max(l5540, l5539);
    at = 7623; l5542 = K[2045];
    at = 7624; l5543 = rt.array_slice(l5535, l5542, l5541);
    at = 7625; if (rt.truthy(l5502)) {
      at = 7626; l5545 = field(rt, l5505, "points");
      at = 7627; l5544 = l5545;
    } else {
      at = 7628; l5546 = K[2046];
      at = 7629; l5544 = l5546;
    }
    at = 7630; l5547 = K[2047];
    at = 7631; l5548 = rt.gt(l5509, l5547);
    at = 7632; if (rt.truthy(l5548)) {
      at = 7633; l5550 = K[2048];
      at = 7634; l5549 = l5550;
    } else {
      at = 7635; l5551 = K[2049];
      at = 7636; l5549 = l5551;
    }
    at = 7637; l5552 = K[2050];
    at = 7638; l5553 = rt.sub(l5509, l5552);
    at = 7639; l5554 = K[2051];
    at = 7640; l5555 = rt.max(l5554, l5553);
    at = 7641; l5556 = rt.array_slice(l5544, l5549, l5555);
    at = 7642; if (rt.truthy(l5502)) {
      at = 7643; l5558 = field(rt, l5505, "color");
      at = 7644; l5557 = l5558;
    } else {
      at = 7645; l5559 = K[2052];
      at = 7646; l5557 = l5559;
    }
    at = 7647; l5560 = K[2053];
    at = 7648; l5561 = rt.sequence_get(l5560, l5557);
    at = 7649; if (rt.truthy(l5502)) {
      at = 7650; l5563 = field(rt, l5505, "width");
      at = 7651; l5562 = l5563;
    } else {
      at = 7652; l5564 = K[2054];
      at = 7653; l5562 = l5564;
    }
    at = 7654; markSet(rt, true, "line", {from: l5543, to: l5556, color: l5561, width: l5562});
    at = 7655; l5565 = field(rt, l15, "strokes");
    at = 7656; l5566 = rt.sequence_length(l5565);
    at = 7657; l5567 = K[2055];
    at = 7658; l5568 = rt.lt(l5567, l5566);
    at = 7659; if (rt.truthy(l5568)) {
      at = 7660; l5569 = field(rt, l15, "strokes");
      at = 7661; l5570 = K[2056];
      at = 7662; l5571 = rt.sequence_get(l5569, l5570);
    }
    at = 7663; if (rt.truthy(l5568)) {
      at = 7664; l5573 = field(rt, l5571, "points");
      at = 7665; l5572 = l5573;
    } else {
      at = 7666; l5574 = K[2057];
      at = 7667; l5572 = l5574;
    }
    at = 7668; l5575 = rt.array_count(l5572);
    at = 7669; l5576 = K[2058];
    at = 7670; l5577 = rt.gt(l5575, l5576);
    at = 7671; if (rt.truthy(l5577)) {
      at = 7672; if (rt.truthy(l5568)) {
        at = 7673; l5580 = field(rt, l5571, "points");
        at = 7674; l5579 = l5580;
      } else {
        at = 7675; l5581 = K[2059];
        at = 7676; l5579 = l5581;
      }
      at = 7677; l5582 = K[2060];
      at = 7678; l5583 = rt.array_nth(l5579, l5582);
      at = 7679; l5578 = l5583;
    } else {
      at = 7680; l5584 = K[2061];
      at = 7681; l5578 = l5584;
    }
    at = 7682; if (rt.truthy(l5568)) {
      at = 7683; l5586 = field(rt, l5571, "width");
      at = 7684; l5585 = l5586;
    } else {
      at = 7685; l5587 = K[2062];
      at = 7686; l5585 = l5587;
    }
    at = 7687; l5588 = K[2063];
    at = 7688; l5589 = rt.int_div(l5585, l5588);
    at = 7689; l5590 = K[2064];
    at = 7690; l5591 = rt.max(l5590, l5589);
    at = 7691; l5592 = K[2065];
    at = 7692; l5593 = rt.gt(l5575, l5592);
    at = 7693; if (rt.truthy(l5593)) {
      at = 7694; if (rt.truthy(l5568)) {
        at = 7695; l5596 = field(rt, l5571, "color");
        at = 7696; l5595 = l5596;
      } else {
        at = 7697; l5597 = K[2066];
        at = 7698; l5595 = l5597;
      }
      at = 7699; l5598 = K[2067];
      at = 7700; l5599 = rt.sequence_get(l5598, l5595);
      at = 7701; l5594 = l5599;
    } else {
      at = 7702; l5600 = K[2068];
      at = 7703; l5594 = l5600;
    }
    at = 7704; markSet(rt, false, "circle", {at: l5578, radius: l5591, fill: l5594});
    at = 7705; if (rt.truthy(l5568)) {
      at = 7706; l5602 = field(rt, l5571, "points");
      at = 7707; l5601 = l5602;
    } else {
      at = 7708; l5603 = K[2069];
      at = 7709; l5601 = l5603;
    }
    at = 7710; l5604 = K[2070];
    at = 7711; l5605 = rt.sub(l5575, l5604);
    at = 7712; l5606 = K[2071];
    at = 7713; l5607 = rt.max(l5606, l5605);
    at = 7714; l5608 = K[2072];
    at = 7715; l5609 = rt.array_slice(l5601, l5608, l5607);
    at = 7716; if (rt.truthy(l5568)) {
      at = 7717; l5611 = field(rt, l5571, "points");
      at = 7718; l5610 = l5611;
    } else {
      at = 7719; l5612 = K[2073];
      at = 7720; l5610 = l5612;
    }
    at = 7721; l5613 = K[2074];
    at = 7722; l5614 = rt.gt(l5575, l5613);
    at = 7723; if (rt.truthy(l5614)) {
      at = 7724; l5616 = K[2075];
      at = 7725; l5615 = l5616;
    } else {
      at = 7726; l5617 = K[2076];
      at = 7727; l5615 = l5617;
    }
    at = 7728; l5618 = K[2077];
    at = 7729; l5619 = rt.sub(l5575, l5618);
    at = 7730; l5620 = K[2078];
    at = 7731; l5621 = rt.max(l5620, l5619);
    at = 7732; l5622 = rt.array_slice(l5610, l5615, l5621);
    at = 7733; if (rt.truthy(l5568)) {
      at = 7734; l5624 = field(rt, l5571, "color");
      at = 7735; l5623 = l5624;
    } else {
      at = 7736; l5625 = K[2079];
      at = 7737; l5623 = l5625;
    }
    at = 7738; l5626 = K[2080];
    at = 7739; l5627 = rt.sequence_get(l5626, l5623);
    at = 7740; if (rt.truthy(l5568)) {
      at = 7741; l5629 = field(rt, l5571, "width");
      at = 7742; l5628 = l5629;
    } else {
      at = 7743; l5630 = K[2081];
      at = 7744; l5628 = l5630;
    }
    at = 7745; markSet(rt, true, "line", {from: l5609, to: l5622, color: l5627, width: l5628});
    at = 7746; l5631 = field(rt, l15, "strokes");
    at = 7747; l5632 = rt.sequence_length(l5631);
    at = 7748; l5633 = K[2082];
    at = 7749; l5634 = rt.lt(l5633, l5632);
    at = 7750; if (rt.truthy(l5634)) {
      at = 7751; l5635 = field(rt, l15, "strokes");
      at = 7752; l5636 = K[2083];
      at = 7753; l5637 = rt.sequence_get(l5635, l5636);
    }
    at = 7754; if (rt.truthy(l5634)) {
      at = 7755; l5639 = field(rt, l5637, "points");
      at = 7756; l5638 = l5639;
    } else {
      at = 7757; l5640 = K[2084];
      at = 7758; l5638 = l5640;
    }
    at = 7759; l5641 = rt.array_count(l5638);
    at = 7760; l5642 = K[2085];
    at = 7761; l5643 = rt.gt(l5641, l5642);
    at = 7762; if (rt.truthy(l5643)) {
      at = 7763; if (rt.truthy(l5634)) {
        at = 7764; l5646 = field(rt, l5637, "points");
        at = 7765; l5645 = l5646;
      } else {
        at = 7766; l5647 = K[2086];
        at = 7767; l5645 = l5647;
      }
      at = 7768; l5648 = K[2087];
      at = 7769; l5649 = rt.array_nth(l5645, l5648);
      at = 7770; l5644 = l5649;
    } else {
      at = 7771; l5650 = K[2088];
      at = 7772; l5644 = l5650;
    }
    at = 7773; if (rt.truthy(l5634)) {
      at = 7774; l5652 = field(rt, l5637, "width");
      at = 7775; l5651 = l5652;
    } else {
      at = 7776; l5653 = K[2089];
      at = 7777; l5651 = l5653;
    }
    at = 7778; l5654 = K[2090];
    at = 7779; l5655 = rt.int_div(l5651, l5654);
    at = 7780; l5656 = K[2091];
    at = 7781; l5657 = rt.max(l5656, l5655);
    at = 7782; l5658 = K[2092];
    at = 7783; l5659 = rt.gt(l5641, l5658);
    at = 7784; if (rt.truthy(l5659)) {
      at = 7785; if (rt.truthy(l5634)) {
        at = 7786; l5662 = field(rt, l5637, "color");
        at = 7787; l5661 = l5662;
      } else {
        at = 7788; l5663 = K[2093];
        at = 7789; l5661 = l5663;
      }
      at = 7790; l5664 = K[2094];
      at = 7791; l5665 = rt.sequence_get(l5664, l5661);
      at = 7792; l5660 = l5665;
    } else {
      at = 7793; l5666 = K[2095];
      at = 7794; l5660 = l5666;
    }
    at = 7795; markSet(rt, false, "circle", {at: l5644, radius: l5657, fill: l5660});
    at = 7796; if (rt.truthy(l5634)) {
      at = 7797; l5668 = field(rt, l5637, "points");
      at = 7798; l5667 = l5668;
    } else {
      at = 7799; l5669 = K[2096];
      at = 7800; l5667 = l5669;
    }
    at = 7801; l5670 = K[2097];
    at = 7802; l5671 = rt.sub(l5641, l5670);
    at = 7803; l5672 = K[2098];
    at = 7804; l5673 = rt.max(l5672, l5671);
    at = 7805; l5674 = K[2099];
    at = 7806; l5675 = rt.array_slice(l5667, l5674, l5673);
    at = 7807; if (rt.truthy(l5634)) {
      at = 7808; l5677 = field(rt, l5637, "points");
      at = 7809; l5676 = l5677;
    } else {
      at = 7810; l5678 = K[2100];
      at = 7811; l5676 = l5678;
    }
    at = 7812; l5679 = K[2101];
    at = 7813; l5680 = rt.gt(l5641, l5679);
    at = 7814; if (rt.truthy(l5680)) {
      at = 7815; l5682 = K[2102];
      at = 7816; l5681 = l5682;
    } else {
      at = 7817; l5683 = K[2103];
      at = 7818; l5681 = l5683;
    }
    at = 7819; l5684 = K[2104];
    at = 7820; l5685 = rt.sub(l5641, l5684);
    at = 7821; l5686 = K[2105];
    at = 7822; l5687 = rt.max(l5686, l5685);
    at = 7823; l5688 = rt.array_slice(l5676, l5681, l5687);
    at = 7824; if (rt.truthy(l5634)) {
      at = 7825; l5690 = field(rt, l5637, "color");
      at = 7826; l5689 = l5690;
    } else {
      at = 7827; l5691 = K[2106];
      at = 7828; l5689 = l5691;
    }
    at = 7829; l5692 = K[2107];
    at = 7830; l5693 = rt.sequence_get(l5692, l5689);
    at = 7831; if (rt.truthy(l5634)) {
      at = 7832; l5695 = field(rt, l5637, "width");
      at = 7833; l5694 = l5695;
    } else {
      at = 7834; l5696 = K[2108];
      at = 7835; l5694 = l5696;
    }
    at = 7836; markSet(rt, true, "line", {from: l5675, to: l5688, color: l5693, width: l5694});
    at = 7837; l5697 = field(rt, l15, "strokes");
    at = 7838; l5698 = rt.sequence_length(l5697);
    at = 7839; l5699 = K[2109];
    at = 7840; l5700 = rt.lt(l5699, l5698);
    at = 7841; if (rt.truthy(l5700)) {
      at = 7842; l5701 = field(rt, l15, "strokes");
      at = 7843; l5702 = K[2110];
      at = 7844; l5703 = rt.sequence_get(l5701, l5702);
    }
    at = 7845; if (rt.truthy(l5700)) {
      at = 7846; l5705 = field(rt, l5703, "points");
      at = 7847; l5704 = l5705;
    } else {
      at = 7848; l5706 = K[2111];
      at = 7849; l5704 = l5706;
    }
    at = 7850; l5707 = rt.array_count(l5704);
    at = 7851; l5708 = K[2112];
    at = 7852; l5709 = rt.gt(l5707, l5708);
    at = 7853; if (rt.truthy(l5709)) {
      at = 7854; if (rt.truthy(l5700)) {
        at = 7855; l5712 = field(rt, l5703, "points");
        at = 7856; l5711 = l5712;
      } else {
        at = 7857; l5713 = K[2113];
        at = 7858; l5711 = l5713;
      }
      at = 7859; l5714 = K[2114];
      at = 7860; l5715 = rt.array_nth(l5711, l5714);
      at = 7861; l5710 = l5715;
    } else {
      at = 7862; l5716 = K[2115];
      at = 7863; l5710 = l5716;
    }
    at = 7864; if (rt.truthy(l5700)) {
      at = 7865; l5718 = field(rt, l5703, "width");
      at = 7866; l5717 = l5718;
    } else {
      at = 7867; l5719 = K[2116];
      at = 7868; l5717 = l5719;
    }
    at = 7869; l5720 = K[2117];
    at = 7870; l5721 = rt.int_div(l5717, l5720);
    at = 7871; l5722 = K[2118];
    at = 7872; l5723 = rt.max(l5722, l5721);
    at = 7873; l5724 = K[2119];
    at = 7874; l5725 = rt.gt(l5707, l5724);
    at = 7875; if (rt.truthy(l5725)) {
      at = 7876; if (rt.truthy(l5700)) {
        at = 7877; l5728 = field(rt, l5703, "color");
        at = 7878; l5727 = l5728;
      } else {
        at = 7879; l5729 = K[2120];
        at = 7880; l5727 = l5729;
      }
      at = 7881; l5730 = K[2121];
      at = 7882; l5731 = rt.sequence_get(l5730, l5727);
      at = 7883; l5726 = l5731;
    } else {
      at = 7884; l5732 = K[2122];
      at = 7885; l5726 = l5732;
    }
    at = 7886; markSet(rt, false, "circle", {at: l5710, radius: l5723, fill: l5726});
    at = 7887; if (rt.truthy(l5700)) {
      at = 7888; l5734 = field(rt, l5703, "points");
      at = 7889; l5733 = l5734;
    } else {
      at = 7890; l5735 = K[2123];
      at = 7891; l5733 = l5735;
    }
    at = 7892; l5736 = K[2124];
    at = 7893; l5737 = rt.sub(l5707, l5736);
    at = 7894; l5738 = K[2125];
    at = 7895; l5739 = rt.max(l5738, l5737);
    at = 7896; l5740 = K[2126];
    at = 7897; l5741 = rt.array_slice(l5733, l5740, l5739);
    at = 7898; if (rt.truthy(l5700)) {
      at = 7899; l5743 = field(rt, l5703, "points");
      at = 7900; l5742 = l5743;
    } else {
      at = 7901; l5744 = K[2127];
      at = 7902; l5742 = l5744;
    }
    at = 7903; l5745 = K[2128];
    at = 7904; l5746 = rt.gt(l5707, l5745);
    at = 7905; if (rt.truthy(l5746)) {
      at = 7906; l5748 = K[2129];
      at = 7907; l5747 = l5748;
    } else {
      at = 7908; l5749 = K[2130];
      at = 7909; l5747 = l5749;
    }
    at = 7910; l5750 = K[2131];
    at = 7911; l5751 = rt.sub(l5707, l5750);
    at = 7912; l5752 = K[2132];
    at = 7913; l5753 = rt.max(l5752, l5751);
    at = 7914; l5754 = rt.array_slice(l5742, l5747, l5753);
    at = 7915; if (rt.truthy(l5700)) {
      at = 7916; l5756 = field(rt, l5703, "color");
      at = 7917; l5755 = l5756;
    } else {
      at = 7918; l5757 = K[2133];
      at = 7919; l5755 = l5757;
    }
    at = 7920; l5758 = K[2134];
    at = 7921; l5759 = rt.sequence_get(l5758, l5755);
    at = 7922; if (rt.truthy(l5700)) {
      at = 7923; l5761 = field(rt, l5703, "width");
      at = 7924; l5760 = l5761;
    } else {
      at = 7925; l5762 = K[2135];
      at = 7926; l5760 = l5762;
    }
    at = 7927; markSet(rt, true, "line", {from: l5741, to: l5754, color: l5759, width: l5760});
    at = 7928; l5763 = field(rt, l15, "strokes");
    at = 7929; l5764 = rt.sequence_length(l5763);
    at = 7930; l5765 = K[2136];
    at = 7931; l5766 = rt.lt(l5765, l5764);
    at = 7932; if (rt.truthy(l5766)) {
      at = 7933; l5767 = field(rt, l15, "strokes");
      at = 7934; l5768 = K[2137];
      at = 7935; l5769 = rt.sequence_get(l5767, l5768);
    }
    at = 7936; if (rt.truthy(l5766)) {
      at = 7937; l5771 = field(rt, l5769, "points");
      at = 7938; l5770 = l5771;
    } else {
      at = 7939; l5772 = K[2138];
      at = 7940; l5770 = l5772;
    }
    at = 7941; l5773 = rt.array_count(l5770);
    at = 7942; l5774 = K[2139];
    at = 7943; l5775 = rt.gt(l5773, l5774);
    at = 7944; if (rt.truthy(l5775)) {
      at = 7945; if (rt.truthy(l5766)) {
        at = 7946; l5778 = field(rt, l5769, "points");
        at = 7947; l5777 = l5778;
      } else {
        at = 7948; l5779 = K[2140];
        at = 7949; l5777 = l5779;
      }
      at = 7950; l5780 = K[2141];
      at = 7951; l5781 = rt.array_nth(l5777, l5780);
      at = 7952; l5776 = l5781;
    } else {
      at = 7953; l5782 = K[2142];
      at = 7954; l5776 = l5782;
    }
    at = 7955; if (rt.truthy(l5766)) {
      at = 7956; l5784 = field(rt, l5769, "width");
      at = 7957; l5783 = l5784;
    } else {
      at = 7958; l5785 = K[2143];
      at = 7959; l5783 = l5785;
    }
    at = 7960; l5786 = K[2144];
    at = 7961; l5787 = rt.int_div(l5783, l5786);
    at = 7962; l5788 = K[2145];
    at = 7963; l5789 = rt.max(l5788, l5787);
    at = 7964; l5790 = K[2146];
    at = 7965; l5791 = rt.gt(l5773, l5790);
    at = 7966; if (rt.truthy(l5791)) {
      at = 7967; if (rt.truthy(l5766)) {
        at = 7968; l5794 = field(rt, l5769, "color");
        at = 7969; l5793 = l5794;
      } else {
        at = 7970; l5795 = K[2147];
        at = 7971; l5793 = l5795;
      }
      at = 7972; l5796 = K[2148];
      at = 7973; l5797 = rt.sequence_get(l5796, l5793);
      at = 7974; l5792 = l5797;
    } else {
      at = 7975; l5798 = K[2149];
      at = 7976; l5792 = l5798;
    }
    at = 7977; markSet(rt, false, "circle", {at: l5776, radius: l5789, fill: l5792});
    at = 7978; if (rt.truthy(l5766)) {
      at = 7979; l5800 = field(rt, l5769, "points");
      at = 7980; l5799 = l5800;
    } else {
      at = 7981; l5801 = K[2150];
      at = 7982; l5799 = l5801;
    }
    at = 7983; l5802 = K[2151];
    at = 7984; l5803 = rt.sub(l5773, l5802);
    at = 7985; l5804 = K[2152];
    at = 7986; l5805 = rt.max(l5804, l5803);
    at = 7987; l5806 = K[2153];
    at = 7988; l5807 = rt.array_slice(l5799, l5806, l5805);
    at = 7989; if (rt.truthy(l5766)) {
      at = 7990; l5809 = field(rt, l5769, "points");
      at = 7991; l5808 = l5809;
    } else {
      at = 7992; l5810 = K[2154];
      at = 7993; l5808 = l5810;
    }
    at = 7994; l5811 = K[2155];
    at = 7995; l5812 = rt.gt(l5773, l5811);
    at = 7996; if (rt.truthy(l5812)) {
      at = 7997; l5814 = K[2156];
      at = 7998; l5813 = l5814;
    } else {
      at = 7999; l5815 = K[2157];
      at = 8000; l5813 = l5815;
    }
    at = 8001; l5816 = K[2158];
    at = 8002; l5817 = rt.sub(l5773, l5816);
    at = 8003; l5818 = K[2159];
    at = 8004; l5819 = rt.max(l5818, l5817);
    at = 8005; l5820 = rt.array_slice(l5808, l5813, l5819);
    at = 8006; if (rt.truthy(l5766)) {
      at = 8007; l5822 = field(rt, l5769, "color");
      at = 8008; l5821 = l5822;
    } else {
      at = 8009; l5823 = K[2160];
      at = 8010; l5821 = l5823;
    }
    at = 8011; l5824 = K[2161];
    at = 8012; l5825 = rt.sequence_get(l5824, l5821);
    at = 8013; if (rt.truthy(l5766)) {
      at = 8014; l5827 = field(rt, l5769, "width");
      at = 8015; l5826 = l5827;
    } else {
      at = 8016; l5828 = K[2162];
      at = 8017; l5826 = l5828;
    }
    at = 8018; markSet(rt, true, "line", {from: l5807, to: l5820, color: l5825, width: l5826});
    at = 8019; l5829 = field(rt, l15, "strokes");
    at = 8020; l5830 = rt.sequence_length(l5829);
    at = 8021; l5831 = K[2163];
    at = 8022; l5832 = rt.lt(l5831, l5830);
    at = 8023; if (rt.truthy(l5832)) {
      at = 8024; l5833 = field(rt, l15, "strokes");
      at = 8025; l5834 = K[2164];
      at = 8026; l5835 = rt.sequence_get(l5833, l5834);
    }
    at = 8027; if (rt.truthy(l5832)) {
      at = 8028; l5837 = field(rt, l5835, "points");
      at = 8029; l5836 = l5837;
    } else {
      at = 8030; l5838 = K[2165];
      at = 8031; l5836 = l5838;
    }
    at = 8032; l5839 = rt.array_count(l5836);
    at = 8033; l5840 = K[2166];
    at = 8034; l5841 = rt.gt(l5839, l5840);
    at = 8035; if (rt.truthy(l5841)) {
      at = 8036; if (rt.truthy(l5832)) {
        at = 8037; l5844 = field(rt, l5835, "points");
        at = 8038; l5843 = l5844;
      } else {
        at = 8039; l5845 = K[2167];
        at = 8040; l5843 = l5845;
      }
      at = 8041; l5846 = K[2168];
      at = 8042; l5847 = rt.array_nth(l5843, l5846);
      at = 8043; l5842 = l5847;
    } else {
      at = 8044; l5848 = K[2169];
      at = 8045; l5842 = l5848;
    }
    at = 8046; if (rt.truthy(l5832)) {
      at = 8047; l5850 = field(rt, l5835, "width");
      at = 8048; l5849 = l5850;
    } else {
      at = 8049; l5851 = K[2170];
      at = 8050; l5849 = l5851;
    }
    at = 8051; l5852 = K[2171];
    at = 8052; l5853 = rt.int_div(l5849, l5852);
    at = 8053; l5854 = K[2172];
    at = 8054; l5855 = rt.max(l5854, l5853);
    at = 8055; l5856 = K[2173];
    at = 8056; l5857 = rt.gt(l5839, l5856);
    at = 8057; if (rt.truthy(l5857)) {
      at = 8058; if (rt.truthy(l5832)) {
        at = 8059; l5860 = field(rt, l5835, "color");
        at = 8060; l5859 = l5860;
      } else {
        at = 8061; l5861 = K[2174];
        at = 8062; l5859 = l5861;
      }
      at = 8063; l5862 = K[2175];
      at = 8064; l5863 = rt.sequence_get(l5862, l5859);
      at = 8065; l5858 = l5863;
    } else {
      at = 8066; l5864 = K[2176];
      at = 8067; l5858 = l5864;
    }
    at = 8068; markSet(rt, false, "circle", {at: l5842, radius: l5855, fill: l5858});
    at = 8069; if (rt.truthy(l5832)) {
      at = 8070; l5866 = field(rt, l5835, "points");
      at = 8071; l5865 = l5866;
    } else {
      at = 8072; l5867 = K[2177];
      at = 8073; l5865 = l5867;
    }
    at = 8074; l5868 = K[2178];
    at = 8075; l5869 = rt.sub(l5839, l5868);
    at = 8076; l5870 = K[2179];
    at = 8077; l5871 = rt.max(l5870, l5869);
    at = 8078; l5872 = K[2180];
    at = 8079; l5873 = rt.array_slice(l5865, l5872, l5871);
    at = 8080; if (rt.truthy(l5832)) {
      at = 8081; l5875 = field(rt, l5835, "points");
      at = 8082; l5874 = l5875;
    } else {
      at = 8083; l5876 = K[2181];
      at = 8084; l5874 = l5876;
    }
    at = 8085; l5877 = K[2182];
    at = 8086; l5878 = rt.gt(l5839, l5877);
    at = 8087; if (rt.truthy(l5878)) {
      at = 8088; l5880 = K[2183];
      at = 8089; l5879 = l5880;
    } else {
      at = 8090; l5881 = K[2184];
      at = 8091; l5879 = l5881;
    }
    at = 8092; l5882 = K[2185];
    at = 8093; l5883 = rt.sub(l5839, l5882);
    at = 8094; l5884 = K[2186];
    at = 8095; l5885 = rt.max(l5884, l5883);
    at = 8096; l5886 = rt.array_slice(l5874, l5879, l5885);
    at = 8097; if (rt.truthy(l5832)) {
      at = 8098; l5888 = field(rt, l5835, "color");
      at = 8099; l5887 = l5888;
    } else {
      at = 8100; l5889 = K[2187];
      at = 8101; l5887 = l5889;
    }
    at = 8102; l5890 = K[2188];
    at = 8103; l5891 = rt.sequence_get(l5890, l5887);
    at = 8104; if (rt.truthy(l5832)) {
      at = 8105; l5893 = field(rt, l5835, "width");
      at = 8106; l5892 = l5893;
    } else {
      at = 8107; l5894 = K[2189];
      at = 8108; l5892 = l5894;
    }
    at = 8109; markSet(rt, true, "line", {from: l5873, to: l5886, color: l5891, width: l5892});
    at = 8110; l5895 = field(rt, l15, "strokes");
    at = 8111; l5896 = rt.sequence_length(l5895);
    at = 8112; l5897 = K[2190];
    at = 8113; l5898 = rt.lt(l5897, l5896);
    at = 8114; if (rt.truthy(l5898)) {
      at = 8115; l5899 = field(rt, l15, "strokes");
      at = 8116; l5900 = K[2191];
      at = 8117; l5901 = rt.sequence_get(l5899, l5900);
    }
    at = 8118; if (rt.truthy(l5898)) {
      at = 8119; l5903 = field(rt, l5901, "points");
      at = 8120; l5902 = l5903;
    } else {
      at = 8121; l5904 = K[2192];
      at = 8122; l5902 = l5904;
    }
    at = 8123; l5905 = rt.array_count(l5902);
    at = 8124; l5906 = K[2193];
    at = 8125; l5907 = rt.gt(l5905, l5906);
    at = 8126; if (rt.truthy(l5907)) {
      at = 8127; if (rt.truthy(l5898)) {
        at = 8128; l5910 = field(rt, l5901, "points");
        at = 8129; l5909 = l5910;
      } else {
        at = 8130; l5911 = K[2194];
        at = 8131; l5909 = l5911;
      }
      at = 8132; l5912 = K[2195];
      at = 8133; l5913 = rt.array_nth(l5909, l5912);
      at = 8134; l5908 = l5913;
    } else {
      at = 8135; l5914 = K[2196];
      at = 8136; l5908 = l5914;
    }
    at = 8137; if (rt.truthy(l5898)) {
      at = 8138; l5916 = field(rt, l5901, "width");
      at = 8139; l5915 = l5916;
    } else {
      at = 8140; l5917 = K[2197];
      at = 8141; l5915 = l5917;
    }
    at = 8142; l5918 = K[2198];
    at = 8143; l5919 = rt.int_div(l5915, l5918);
    at = 8144; l5920 = K[2199];
    at = 8145; l5921 = rt.max(l5920, l5919);
    at = 8146; l5922 = K[2200];
    at = 8147; l5923 = rt.gt(l5905, l5922);
    at = 8148; if (rt.truthy(l5923)) {
      at = 8149; if (rt.truthy(l5898)) {
        at = 8150; l5926 = field(rt, l5901, "color");
        at = 8151; l5925 = l5926;
      } else {
        at = 8152; l5927 = K[2201];
        at = 8153; l5925 = l5927;
      }
      at = 8154; l5928 = K[2202];
      at = 8155; l5929 = rt.sequence_get(l5928, l5925);
      at = 8156; l5924 = l5929;
    } else {
      at = 8157; l5930 = K[2203];
      at = 8158; l5924 = l5930;
    }
    at = 8159; markSet(rt, false, "circle", {at: l5908, radius: l5921, fill: l5924});
    at = 8160; if (rt.truthy(l5898)) {
      at = 8161; l5932 = field(rt, l5901, "points");
      at = 8162; l5931 = l5932;
    } else {
      at = 8163; l5933 = K[2204];
      at = 8164; l5931 = l5933;
    }
    at = 8165; l5934 = K[2205];
    at = 8166; l5935 = rt.sub(l5905, l5934);
    at = 8167; l5936 = K[2206];
    at = 8168; l5937 = rt.max(l5936, l5935);
    at = 8169; l5938 = K[2207];
    at = 8170; l5939 = rt.array_slice(l5931, l5938, l5937);
    at = 8171; if (rt.truthy(l5898)) {
      at = 8172; l5941 = field(rt, l5901, "points");
      at = 8173; l5940 = l5941;
    } else {
      at = 8174; l5942 = K[2208];
      at = 8175; l5940 = l5942;
    }
    at = 8176; l5943 = K[2209];
    at = 8177; l5944 = rt.gt(l5905, l5943);
    at = 8178; if (rt.truthy(l5944)) {
      at = 8179; l5946 = K[2210];
      at = 8180; l5945 = l5946;
    } else {
      at = 8181; l5947 = K[2211];
      at = 8182; l5945 = l5947;
    }
    at = 8183; l5948 = K[2212];
    at = 8184; l5949 = rt.sub(l5905, l5948);
    at = 8185; l5950 = K[2213];
    at = 8186; l5951 = rt.max(l5950, l5949);
    at = 8187; l5952 = rt.array_slice(l5940, l5945, l5951);
    at = 8188; if (rt.truthy(l5898)) {
      at = 8189; l5954 = field(rt, l5901, "color");
      at = 8190; l5953 = l5954;
    } else {
      at = 8191; l5955 = K[2214];
      at = 8192; l5953 = l5955;
    }
    at = 8193; l5956 = K[2215];
    at = 8194; l5957 = rt.sequence_get(l5956, l5953);
    at = 8195; if (rt.truthy(l5898)) {
      at = 8196; l5959 = field(rt, l5901, "width");
      at = 8197; l5958 = l5959;
    } else {
      at = 8198; l5960 = K[2216];
      at = 8199; l5958 = l5960;
    }
    at = 8200; markSet(rt, true, "line", {from: l5939, to: l5952, color: l5957, width: l5958});
    at = 8201; l5961 = field(rt, l15, "strokes");
    at = 8202; l5962 = rt.sequence_length(l5961);
    at = 8203; l5963 = K[2217];
    at = 8204; l5964 = rt.lt(l5963, l5962);
    at = 8205; if (rt.truthy(l5964)) {
      at = 8206; l5965 = field(rt, l15, "strokes");
      at = 8207; l5966 = K[2218];
      at = 8208; l5967 = rt.sequence_get(l5965, l5966);
    }
    at = 8209; if (rt.truthy(l5964)) {
      at = 8210; l5969 = field(rt, l5967, "points");
      at = 8211; l5968 = l5969;
    } else {
      at = 8212; l5970 = K[2219];
      at = 8213; l5968 = l5970;
    }
    at = 8214; l5971 = rt.array_count(l5968);
    at = 8215; l5972 = K[2220];
    at = 8216; l5973 = rt.gt(l5971, l5972);
    at = 8217; if (rt.truthy(l5973)) {
      at = 8218; if (rt.truthy(l5964)) {
        at = 8219; l5976 = field(rt, l5967, "points");
        at = 8220; l5975 = l5976;
      } else {
        at = 8221; l5977 = K[2221];
        at = 8222; l5975 = l5977;
      }
      at = 8223; l5978 = K[2222];
      at = 8224; l5979 = rt.array_nth(l5975, l5978);
      at = 8225; l5974 = l5979;
    } else {
      at = 8226; l5980 = K[2223];
      at = 8227; l5974 = l5980;
    }
    at = 8228; if (rt.truthy(l5964)) {
      at = 8229; l5982 = field(rt, l5967, "width");
      at = 8230; l5981 = l5982;
    } else {
      at = 8231; l5983 = K[2224];
      at = 8232; l5981 = l5983;
    }
    at = 8233; l5984 = K[2225];
    at = 8234; l5985 = rt.int_div(l5981, l5984);
    at = 8235; l5986 = K[2226];
    at = 8236; l5987 = rt.max(l5986, l5985);
    at = 8237; l5988 = K[2227];
    at = 8238; l5989 = rt.gt(l5971, l5988);
    at = 8239; if (rt.truthy(l5989)) {
      at = 8240; if (rt.truthy(l5964)) {
        at = 8241; l5992 = field(rt, l5967, "color");
        at = 8242; l5991 = l5992;
      } else {
        at = 8243; l5993 = K[2228];
        at = 8244; l5991 = l5993;
      }
      at = 8245; l5994 = K[2229];
      at = 8246; l5995 = rt.sequence_get(l5994, l5991);
      at = 8247; l5990 = l5995;
    } else {
      at = 8248; l5996 = K[2230];
      at = 8249; l5990 = l5996;
    }
    at = 8250; markSet(rt, false, "circle", {at: l5974, radius: l5987, fill: l5990});
    at = 8251; if (rt.truthy(l5964)) {
      at = 8252; l5998 = field(rt, l5967, "points");
      at = 8253; l5997 = l5998;
    } else {
      at = 8254; l5999 = K[2231];
      at = 8255; l5997 = l5999;
    }
    at = 8256; l6000 = K[2232];
    at = 8257; l6001 = rt.sub(l5971, l6000);
    at = 8258; l6002 = K[2233];
    at = 8259; l6003 = rt.max(l6002, l6001);
    at = 8260; l6004 = K[2234];
    at = 8261; l6005 = rt.array_slice(l5997, l6004, l6003);
    at = 8262; if (rt.truthy(l5964)) {
      at = 8263; l6007 = field(rt, l5967, "points");
      at = 8264; l6006 = l6007;
    } else {
      at = 8265; l6008 = K[2235];
      at = 8266; l6006 = l6008;
    }
    at = 8267; l6009 = K[2236];
    at = 8268; l6010 = rt.gt(l5971, l6009);
    at = 8269; if (rt.truthy(l6010)) {
      at = 8270; l6012 = K[2237];
      at = 8271; l6011 = l6012;
    } else {
      at = 8272; l6013 = K[2238];
      at = 8273; l6011 = l6013;
    }
    at = 8274; l6014 = K[2239];
    at = 8275; l6015 = rt.sub(l5971, l6014);
    at = 8276; l6016 = K[2240];
    at = 8277; l6017 = rt.max(l6016, l6015);
    at = 8278; l6018 = rt.array_slice(l6006, l6011, l6017);
    at = 8279; if (rt.truthy(l5964)) {
      at = 8280; l6020 = field(rt, l5967, "color");
      at = 8281; l6019 = l6020;
    } else {
      at = 8282; l6021 = K[2241];
      at = 8283; l6019 = l6021;
    }
    at = 8284; l6022 = K[2242];
    at = 8285; l6023 = rt.sequence_get(l6022, l6019);
    at = 8286; if (rt.truthy(l5964)) {
      at = 8287; l6025 = field(rt, l5967, "width");
      at = 8288; l6024 = l6025;
    } else {
      at = 8289; l6026 = K[2243];
      at = 8290; l6024 = l6026;
    }
    at = 8291; markSet(rt, true, "line", {from: l6005, to: l6018, color: l6023, width: l6024});
    at = 8292; l6027 = field(rt, l15, "strokes");
    at = 8293; l6028 = rt.sequence_length(l6027);
    at = 8294; l6029 = K[2244];
    at = 8295; l6030 = rt.lt(l6029, l6028);
    at = 8296; if (rt.truthy(l6030)) {
      at = 8297; l6031 = field(rt, l15, "strokes");
      at = 8298; l6032 = K[2245];
      at = 8299; l6033 = rt.sequence_get(l6031, l6032);
    }
    at = 8300; if (rt.truthy(l6030)) {
      at = 8301; l6035 = field(rt, l6033, "points");
      at = 8302; l6034 = l6035;
    } else {
      at = 8303; l6036 = K[2246];
      at = 8304; l6034 = l6036;
    }
    at = 8305; l6037 = rt.array_count(l6034);
    at = 8306; l6038 = K[2247];
    at = 8307; l6039 = rt.gt(l6037, l6038);
    at = 8308; if (rt.truthy(l6039)) {
      at = 8309; if (rt.truthy(l6030)) {
        at = 8310; l6042 = field(rt, l6033, "points");
        at = 8311; l6041 = l6042;
      } else {
        at = 8312; l6043 = K[2248];
        at = 8313; l6041 = l6043;
      }
      at = 8314; l6044 = K[2249];
      at = 8315; l6045 = rt.array_nth(l6041, l6044);
      at = 8316; l6040 = l6045;
    } else {
      at = 8317; l6046 = K[2250];
      at = 8318; l6040 = l6046;
    }
    at = 8319; if (rt.truthy(l6030)) {
      at = 8320; l6048 = field(rt, l6033, "width");
      at = 8321; l6047 = l6048;
    } else {
      at = 8322; l6049 = K[2251];
      at = 8323; l6047 = l6049;
    }
    at = 8324; l6050 = K[2252];
    at = 8325; l6051 = rt.int_div(l6047, l6050);
    at = 8326; l6052 = K[2253];
    at = 8327; l6053 = rt.max(l6052, l6051);
    at = 8328; l6054 = K[2254];
    at = 8329; l6055 = rt.gt(l6037, l6054);
    at = 8330; if (rt.truthy(l6055)) {
      at = 8331; if (rt.truthy(l6030)) {
        at = 8332; l6058 = field(rt, l6033, "color");
        at = 8333; l6057 = l6058;
      } else {
        at = 8334; l6059 = K[2255];
        at = 8335; l6057 = l6059;
      }
      at = 8336; l6060 = K[2256];
      at = 8337; l6061 = rt.sequence_get(l6060, l6057);
      at = 8338; l6056 = l6061;
    } else {
      at = 8339; l6062 = K[2257];
      at = 8340; l6056 = l6062;
    }
    at = 8341; markSet(rt, false, "circle", {at: l6040, radius: l6053, fill: l6056});
    at = 8342; if (rt.truthy(l6030)) {
      at = 8343; l6064 = field(rt, l6033, "points");
      at = 8344; l6063 = l6064;
    } else {
      at = 8345; l6065 = K[2258];
      at = 8346; l6063 = l6065;
    }
    at = 8347; l6066 = K[2259];
    at = 8348; l6067 = rt.sub(l6037, l6066);
    at = 8349; l6068 = K[2260];
    at = 8350; l6069 = rt.max(l6068, l6067);
    at = 8351; l6070 = K[2261];
    at = 8352; l6071 = rt.array_slice(l6063, l6070, l6069);
    at = 8353; if (rt.truthy(l6030)) {
      at = 8354; l6073 = field(rt, l6033, "points");
      at = 8355; l6072 = l6073;
    } else {
      at = 8356; l6074 = K[2262];
      at = 8357; l6072 = l6074;
    }
    at = 8358; l6075 = K[2263];
    at = 8359; l6076 = rt.gt(l6037, l6075);
    at = 8360; if (rt.truthy(l6076)) {
      at = 8361; l6078 = K[2264];
      at = 8362; l6077 = l6078;
    } else {
      at = 8363; l6079 = K[2265];
      at = 8364; l6077 = l6079;
    }
    at = 8365; l6080 = K[2266];
    at = 8366; l6081 = rt.sub(l6037, l6080);
    at = 8367; l6082 = K[2267];
    at = 8368; l6083 = rt.max(l6082, l6081);
    at = 8369; l6084 = rt.array_slice(l6072, l6077, l6083);
    at = 8370; if (rt.truthy(l6030)) {
      at = 8371; l6086 = field(rt, l6033, "color");
      at = 8372; l6085 = l6086;
    } else {
      at = 8373; l6087 = K[2268];
      at = 8374; l6085 = l6087;
    }
    at = 8375; l6088 = K[2269];
    at = 8376; l6089 = rt.sequence_get(l6088, l6085);
    at = 8377; if (rt.truthy(l6030)) {
      at = 8378; l6091 = field(rt, l6033, "width");
      at = 8379; l6090 = l6091;
    } else {
      at = 8380; l6092 = K[2270];
      at = 8381; l6090 = l6092;
    }
    at = 8382; markSet(rt, true, "line", {from: l6071, to: l6084, color: l6089, width: l6090});
    at = 8383; l6093 = field(rt, l15, "strokes");
    at = 8384; l6094 = rt.sequence_length(l6093);
    at = 8385; l6095 = K[2271];
    at = 8386; l6096 = rt.lt(l6095, l6094);
    at = 8387; if (rt.truthy(l6096)) {
      at = 8388; l6097 = field(rt, l15, "strokes");
      at = 8389; l6098 = K[2272];
      at = 8390; l6099 = rt.sequence_get(l6097, l6098);
    }
    at = 8391; if (rt.truthy(l6096)) {
      at = 8392; l6101 = field(rt, l6099, "points");
      at = 8393; l6100 = l6101;
    } else {
      at = 8394; l6102 = K[2273];
      at = 8395; l6100 = l6102;
    }
    at = 8396; l6103 = rt.array_count(l6100);
    at = 8397; l6104 = K[2274];
    at = 8398; l6105 = rt.gt(l6103, l6104);
    at = 8399; if (rt.truthy(l6105)) {
      at = 8400; if (rt.truthy(l6096)) {
        at = 8401; l6108 = field(rt, l6099, "points");
        at = 8402; l6107 = l6108;
      } else {
        at = 8403; l6109 = K[2275];
        at = 8404; l6107 = l6109;
      }
      at = 8405; l6110 = K[2276];
      at = 8406; l6111 = rt.array_nth(l6107, l6110);
      at = 8407; l6106 = l6111;
    } else {
      at = 8408; l6112 = K[2277];
      at = 8409; l6106 = l6112;
    }
    at = 8410; if (rt.truthy(l6096)) {
      at = 8411; l6114 = field(rt, l6099, "width");
      at = 8412; l6113 = l6114;
    } else {
      at = 8413; l6115 = K[2278];
      at = 8414; l6113 = l6115;
    }
    at = 8415; l6116 = K[2279];
    at = 8416; l6117 = rt.int_div(l6113, l6116);
    at = 8417; l6118 = K[2280];
    at = 8418; l6119 = rt.max(l6118, l6117);
    at = 8419; l6120 = K[2281];
    at = 8420; l6121 = rt.gt(l6103, l6120);
    at = 8421; if (rt.truthy(l6121)) {
      at = 8422; if (rt.truthy(l6096)) {
        at = 8423; l6124 = field(rt, l6099, "color");
        at = 8424; l6123 = l6124;
      } else {
        at = 8425; l6125 = K[2282];
        at = 8426; l6123 = l6125;
      }
      at = 8427; l6126 = K[2283];
      at = 8428; l6127 = rt.sequence_get(l6126, l6123);
      at = 8429; l6122 = l6127;
    } else {
      at = 8430; l6128 = K[2284];
      at = 8431; l6122 = l6128;
    }
    at = 8432; markSet(rt, false, "circle", {at: l6106, radius: l6119, fill: l6122});
    at = 8433; if (rt.truthy(l6096)) {
      at = 8434; l6130 = field(rt, l6099, "points");
      at = 8435; l6129 = l6130;
    } else {
      at = 8436; l6131 = K[2285];
      at = 8437; l6129 = l6131;
    }
    at = 8438; l6132 = K[2286];
    at = 8439; l6133 = rt.sub(l6103, l6132);
    at = 8440; l6134 = K[2287];
    at = 8441; l6135 = rt.max(l6134, l6133);
    at = 8442; l6136 = K[2288];
    at = 8443; l6137 = rt.array_slice(l6129, l6136, l6135);
    at = 8444; if (rt.truthy(l6096)) {
      at = 8445; l6139 = field(rt, l6099, "points");
      at = 8446; l6138 = l6139;
    } else {
      at = 8447; l6140 = K[2289];
      at = 8448; l6138 = l6140;
    }
    at = 8449; l6141 = K[2290];
    at = 8450; l6142 = rt.gt(l6103, l6141);
    at = 8451; if (rt.truthy(l6142)) {
      at = 8452; l6144 = K[2291];
      at = 8453; l6143 = l6144;
    } else {
      at = 8454; l6145 = K[2292];
      at = 8455; l6143 = l6145;
    }
    at = 8456; l6146 = K[2293];
    at = 8457; l6147 = rt.sub(l6103, l6146);
    at = 8458; l6148 = K[2294];
    at = 8459; l6149 = rt.max(l6148, l6147);
    at = 8460; l6150 = rt.array_slice(l6138, l6143, l6149);
    at = 8461; if (rt.truthy(l6096)) {
      at = 8462; l6152 = field(rt, l6099, "color");
      at = 8463; l6151 = l6152;
    } else {
      at = 8464; l6153 = K[2295];
      at = 8465; l6151 = l6153;
    }
    at = 8466; l6154 = K[2296];
    at = 8467; l6155 = rt.sequence_get(l6154, l6151);
    at = 8468; if (rt.truthy(l6096)) {
      at = 8469; l6157 = field(rt, l6099, "width");
      at = 8470; l6156 = l6157;
    } else {
      at = 8471; l6158 = K[2297];
      at = 8472; l6156 = l6158;
    }
    at = 8473; markSet(rt, true, "line", {from: l6137, to: l6150, color: l6155, width: l6156});
    at = 8474; l6159 = field(rt, l15, "strokes");
    at = 8475; l6160 = rt.sequence_length(l6159);
    at = 8476; l6161 = K[2298];
    at = 8477; l6162 = rt.lt(l6161, l6160);
    at = 8478; if (rt.truthy(l6162)) {
      at = 8479; l6163 = field(rt, l15, "strokes");
      at = 8480; l6164 = K[2299];
      at = 8481; l6165 = rt.sequence_get(l6163, l6164);
    }
    at = 8482; if (rt.truthy(l6162)) {
      at = 8483; l6167 = field(rt, l6165, "points");
      at = 8484; l6166 = l6167;
    } else {
      at = 8485; l6168 = K[2300];
      at = 8486; l6166 = l6168;
    }
    at = 8487; l6169 = rt.array_count(l6166);
    at = 8488; l6170 = K[2301];
    at = 8489; l6171 = rt.gt(l6169, l6170);
    at = 8490; if (rt.truthy(l6171)) {
      at = 8491; if (rt.truthy(l6162)) {
        at = 8492; l6174 = field(rt, l6165, "points");
        at = 8493; l6173 = l6174;
      } else {
        at = 8494; l6175 = K[2302];
        at = 8495; l6173 = l6175;
      }
      at = 8496; l6176 = K[2303];
      at = 8497; l6177 = rt.array_nth(l6173, l6176);
      at = 8498; l6172 = l6177;
    } else {
      at = 8499; l6178 = K[2304];
      at = 8500; l6172 = l6178;
    }
    at = 8501; if (rt.truthy(l6162)) {
      at = 8502; l6180 = field(rt, l6165, "width");
      at = 8503; l6179 = l6180;
    } else {
      at = 8504; l6181 = K[2305];
      at = 8505; l6179 = l6181;
    }
    at = 8506; l6182 = K[2306];
    at = 8507; l6183 = rt.int_div(l6179, l6182);
    at = 8508; l6184 = K[2307];
    at = 8509; l6185 = rt.max(l6184, l6183);
    at = 8510; l6186 = K[2308];
    at = 8511; l6187 = rt.gt(l6169, l6186);
    at = 8512; if (rt.truthy(l6187)) {
      at = 8513; if (rt.truthy(l6162)) {
        at = 8514; l6190 = field(rt, l6165, "color");
        at = 8515; l6189 = l6190;
      } else {
        at = 8516; l6191 = K[2309];
        at = 8517; l6189 = l6191;
      }
      at = 8518; l6192 = K[2310];
      at = 8519; l6193 = rt.sequence_get(l6192, l6189);
      at = 8520; l6188 = l6193;
    } else {
      at = 8521; l6194 = K[2311];
      at = 8522; l6188 = l6194;
    }
    at = 8523; markSet(rt, false, "circle", {at: l6172, radius: l6185, fill: l6188});
    at = 8524; if (rt.truthy(l6162)) {
      at = 8525; l6196 = field(rt, l6165, "points");
      at = 8526; l6195 = l6196;
    } else {
      at = 8527; l6197 = K[2312];
      at = 8528; l6195 = l6197;
    }
    at = 8529; l6198 = K[2313];
    at = 8530; l6199 = rt.sub(l6169, l6198);
    at = 8531; l6200 = K[2314];
    at = 8532; l6201 = rt.max(l6200, l6199);
    at = 8533; l6202 = K[2315];
    at = 8534; l6203 = rt.array_slice(l6195, l6202, l6201);
    at = 8535; if (rt.truthy(l6162)) {
      at = 8536; l6205 = field(rt, l6165, "points");
      at = 8537; l6204 = l6205;
    } else {
      at = 8538; l6206 = K[2316];
      at = 8539; l6204 = l6206;
    }
    at = 8540; l6207 = K[2317];
    at = 8541; l6208 = rt.gt(l6169, l6207);
    at = 8542; if (rt.truthy(l6208)) {
      at = 8543; l6210 = K[2318];
      at = 8544; l6209 = l6210;
    } else {
      at = 8545; l6211 = K[2319];
      at = 8546; l6209 = l6211;
    }
    at = 8547; l6212 = K[2320];
    at = 8548; l6213 = rt.sub(l6169, l6212);
    at = 8549; l6214 = K[2321];
    at = 8550; l6215 = rt.max(l6214, l6213);
    at = 8551; l6216 = rt.array_slice(l6204, l6209, l6215);
    at = 8552; if (rt.truthy(l6162)) {
      at = 8553; l6218 = field(rt, l6165, "color");
      at = 8554; l6217 = l6218;
    } else {
      at = 8555; l6219 = K[2322];
      at = 8556; l6217 = l6219;
    }
    at = 8557; l6220 = K[2323];
    at = 8558; l6221 = rt.sequence_get(l6220, l6217);
    at = 8559; if (rt.truthy(l6162)) {
      at = 8560; l6223 = field(rt, l6165, "width");
      at = 8561; l6222 = l6223;
    } else {
      at = 8562; l6224 = K[2324];
      at = 8563; l6222 = l6224;
    }
    at = 8564; markSet(rt, true, "line", {from: l6203, to: l6216, color: l6221, width: l6222});
    at = 8565; l6225 = field(rt, l15, "strokes");
    at = 8566; l6226 = rt.sequence_length(l6225);
    at = 8567; l6227 = K[2325];
    at = 8568; l6228 = rt.lt(l6227, l6226);
    at = 8569; if (rt.truthy(l6228)) {
      at = 8570; l6229 = field(rt, l15, "strokes");
      at = 8571; l6230 = K[2326];
      at = 8572; l6231 = rt.sequence_get(l6229, l6230);
    }
    at = 8573; if (rt.truthy(l6228)) {
      at = 8574; l6233 = field(rt, l6231, "points");
      at = 8575; l6232 = l6233;
    } else {
      at = 8576; l6234 = K[2327];
      at = 8577; l6232 = l6234;
    }
    at = 8578; l6235 = rt.array_count(l6232);
    at = 8579; l6236 = K[2328];
    at = 8580; l6237 = rt.gt(l6235, l6236);
    at = 8581; if (rt.truthy(l6237)) {
      at = 8582; if (rt.truthy(l6228)) {
        at = 8583; l6240 = field(rt, l6231, "points");
        at = 8584; l6239 = l6240;
      } else {
        at = 8585; l6241 = K[2329];
        at = 8586; l6239 = l6241;
      }
      at = 8587; l6242 = K[2330];
      at = 8588; l6243 = rt.array_nth(l6239, l6242);
      at = 8589; l6238 = l6243;
    } else {
      at = 8590; l6244 = K[2331];
      at = 8591; l6238 = l6244;
    }
    at = 8592; if (rt.truthy(l6228)) {
      at = 8593; l6246 = field(rt, l6231, "width");
      at = 8594; l6245 = l6246;
    } else {
      at = 8595; l6247 = K[2332];
      at = 8596; l6245 = l6247;
    }
    at = 8597; l6248 = K[2333];
    at = 8598; l6249 = rt.int_div(l6245, l6248);
    at = 8599; l6250 = K[2334];
    at = 8600; l6251 = rt.max(l6250, l6249);
    at = 8601; l6252 = K[2335];
    at = 8602; l6253 = rt.gt(l6235, l6252);
    at = 8603; if (rt.truthy(l6253)) {
      at = 8604; if (rt.truthy(l6228)) {
        at = 8605; l6256 = field(rt, l6231, "color");
        at = 8606; l6255 = l6256;
      } else {
        at = 8607; l6257 = K[2336];
        at = 8608; l6255 = l6257;
      }
      at = 8609; l6258 = K[2337];
      at = 8610; l6259 = rt.sequence_get(l6258, l6255);
      at = 8611; l6254 = l6259;
    } else {
      at = 8612; l6260 = K[2338];
      at = 8613; l6254 = l6260;
    }
    at = 8614; markSet(rt, false, "circle", {at: l6238, radius: l6251, fill: l6254});
    at = 8615; if (rt.truthy(l6228)) {
      at = 8616; l6262 = field(rt, l6231, "points");
      at = 8617; l6261 = l6262;
    } else {
      at = 8618; l6263 = K[2339];
      at = 8619; l6261 = l6263;
    }
    at = 8620; l6264 = K[2340];
    at = 8621; l6265 = rt.sub(l6235, l6264);
    at = 8622; l6266 = K[2341];
    at = 8623; l6267 = rt.max(l6266, l6265);
    at = 8624; l6268 = K[2342];
    at = 8625; l6269 = rt.array_slice(l6261, l6268, l6267);
    at = 8626; if (rt.truthy(l6228)) {
      at = 8627; l6271 = field(rt, l6231, "points");
      at = 8628; l6270 = l6271;
    } else {
      at = 8629; l6272 = K[2343];
      at = 8630; l6270 = l6272;
    }
    at = 8631; l6273 = K[2344];
    at = 8632; l6274 = rt.gt(l6235, l6273);
    at = 8633; if (rt.truthy(l6274)) {
      at = 8634; l6276 = K[2345];
      at = 8635; l6275 = l6276;
    } else {
      at = 8636; l6277 = K[2346];
      at = 8637; l6275 = l6277;
    }
    at = 8638; l6278 = K[2347];
    at = 8639; l6279 = rt.sub(l6235, l6278);
    at = 8640; l6280 = K[2348];
    at = 8641; l6281 = rt.max(l6280, l6279);
    at = 8642; l6282 = rt.array_slice(l6270, l6275, l6281);
    at = 8643; if (rt.truthy(l6228)) {
      at = 8644; l6284 = field(rt, l6231, "color");
      at = 8645; l6283 = l6284;
    } else {
      at = 8646; l6285 = K[2349];
      at = 8647; l6283 = l6285;
    }
    at = 8648; l6286 = K[2350];
    at = 8649; l6287 = rt.sequence_get(l6286, l6283);
    at = 8650; if (rt.truthy(l6228)) {
      at = 8651; l6289 = field(rt, l6231, "width");
      at = 8652; l6288 = l6289;
    } else {
      at = 8653; l6290 = K[2351];
      at = 8654; l6288 = l6290;
    }
    at = 8655; markSet(rt, true, "line", {from: l6269, to: l6282, color: l6287, width: l6288});
    at = 8656; l6291 = field(rt, l15, "strokes");
    at = 8657; l6292 = rt.sequence_length(l6291);
    at = 8658; l6293 = K[2352];
    at = 8659; l6294 = rt.lt(l6293, l6292);
    at = 8660; if (rt.truthy(l6294)) {
      at = 8661; l6295 = field(rt, l15, "strokes");
      at = 8662; l6296 = K[2353];
      at = 8663; l6297 = rt.sequence_get(l6295, l6296);
    }
    at = 8664; if (rt.truthy(l6294)) {
      at = 8665; l6299 = field(rt, l6297, "points");
      at = 8666; l6298 = l6299;
    } else {
      at = 8667; l6300 = K[2354];
      at = 8668; l6298 = l6300;
    }
    at = 8669; l6301 = rt.array_count(l6298);
    at = 8670; l6302 = K[2355];
    at = 8671; l6303 = rt.gt(l6301, l6302);
    at = 8672; if (rt.truthy(l6303)) {
      at = 8673; if (rt.truthy(l6294)) {
        at = 8674; l6306 = field(rt, l6297, "points");
        at = 8675; l6305 = l6306;
      } else {
        at = 8676; l6307 = K[2356];
        at = 8677; l6305 = l6307;
      }
      at = 8678; l6308 = K[2357];
      at = 8679; l6309 = rt.array_nth(l6305, l6308);
      at = 8680; l6304 = l6309;
    } else {
      at = 8681; l6310 = K[2358];
      at = 8682; l6304 = l6310;
    }
    at = 8683; if (rt.truthy(l6294)) {
      at = 8684; l6312 = field(rt, l6297, "width");
      at = 8685; l6311 = l6312;
    } else {
      at = 8686; l6313 = K[2359];
      at = 8687; l6311 = l6313;
    }
    at = 8688; l6314 = K[2360];
    at = 8689; l6315 = rt.int_div(l6311, l6314);
    at = 8690; l6316 = K[2361];
    at = 8691; l6317 = rt.max(l6316, l6315);
    at = 8692; l6318 = K[2362];
    at = 8693; l6319 = rt.gt(l6301, l6318);
    at = 8694; if (rt.truthy(l6319)) {
      at = 8695; if (rt.truthy(l6294)) {
        at = 8696; l6322 = field(rt, l6297, "color");
        at = 8697; l6321 = l6322;
      } else {
        at = 8698; l6323 = K[2363];
        at = 8699; l6321 = l6323;
      }
      at = 8700; l6324 = K[2364];
      at = 8701; l6325 = rt.sequence_get(l6324, l6321);
      at = 8702; l6320 = l6325;
    } else {
      at = 8703; l6326 = K[2365];
      at = 8704; l6320 = l6326;
    }
    at = 8705; markSet(rt, false, "circle", {at: l6304, radius: l6317, fill: l6320});
    at = 8706; if (rt.truthy(l6294)) {
      at = 8707; l6328 = field(rt, l6297, "points");
      at = 8708; l6327 = l6328;
    } else {
      at = 8709; l6329 = K[2366];
      at = 8710; l6327 = l6329;
    }
    at = 8711; l6330 = K[2367];
    at = 8712; l6331 = rt.sub(l6301, l6330);
    at = 8713; l6332 = K[2368];
    at = 8714; l6333 = rt.max(l6332, l6331);
    at = 8715; l6334 = K[2369];
    at = 8716; l6335 = rt.array_slice(l6327, l6334, l6333);
    at = 8717; if (rt.truthy(l6294)) {
      at = 8718; l6337 = field(rt, l6297, "points");
      at = 8719; l6336 = l6337;
    } else {
      at = 8720; l6338 = K[2370];
      at = 8721; l6336 = l6338;
    }
    at = 8722; l6339 = K[2371];
    at = 8723; l6340 = rt.gt(l6301, l6339);
    at = 8724; if (rt.truthy(l6340)) {
      at = 8725; l6342 = K[2372];
      at = 8726; l6341 = l6342;
    } else {
      at = 8727; l6343 = K[2373];
      at = 8728; l6341 = l6343;
    }
    at = 8729; l6344 = K[2374];
    at = 8730; l6345 = rt.sub(l6301, l6344);
    at = 8731; l6346 = K[2375];
    at = 8732; l6347 = rt.max(l6346, l6345);
    at = 8733; l6348 = rt.array_slice(l6336, l6341, l6347);
    at = 8734; if (rt.truthy(l6294)) {
      at = 8735; l6350 = field(rt, l6297, "color");
      at = 8736; l6349 = l6350;
    } else {
      at = 8737; l6351 = K[2376];
      at = 8738; l6349 = l6351;
    }
    at = 8739; l6352 = K[2377];
    at = 8740; l6353 = rt.sequence_get(l6352, l6349);
    at = 8741; if (rt.truthy(l6294)) {
      at = 8742; l6355 = field(rt, l6297, "width");
      at = 8743; l6354 = l6355;
    } else {
      at = 8744; l6356 = K[2378];
      at = 8745; l6354 = l6356;
    }
    at = 8746; markSet(rt, true, "line", {from: l6335, to: l6348, color: l6353, width: l6354});
    at = 8747; l6357 = field(rt, l15, "strokes");
    at = 8748; l6358 = rt.sequence_length(l6357);
    at = 8749; l6359 = K[2379];
    at = 8750; l6360 = rt.lt(l6359, l6358);
    at = 8751; if (rt.truthy(l6360)) {
      at = 8752; l6361 = field(rt, l15, "strokes");
      at = 8753; l6362 = K[2380];
      at = 8754; l6363 = rt.sequence_get(l6361, l6362);
    }
    at = 8755; if (rt.truthy(l6360)) {
      at = 8756; l6365 = field(rt, l6363, "points");
      at = 8757; l6364 = l6365;
    } else {
      at = 8758; l6366 = K[2381];
      at = 8759; l6364 = l6366;
    }
    at = 8760; l6367 = rt.array_count(l6364);
    at = 8761; l6368 = K[2382];
    at = 8762; l6369 = rt.gt(l6367, l6368);
    at = 8763; if (rt.truthy(l6369)) {
      at = 8764; if (rt.truthy(l6360)) {
        at = 8765; l6372 = field(rt, l6363, "points");
        at = 8766; l6371 = l6372;
      } else {
        at = 8767; l6373 = K[2383];
        at = 8768; l6371 = l6373;
      }
      at = 8769; l6374 = K[2384];
      at = 8770; l6375 = rt.array_nth(l6371, l6374);
      at = 8771; l6370 = l6375;
    } else {
      at = 8772; l6376 = K[2385];
      at = 8773; l6370 = l6376;
    }
    at = 8774; if (rt.truthy(l6360)) {
      at = 8775; l6378 = field(rt, l6363, "width");
      at = 8776; l6377 = l6378;
    } else {
      at = 8777; l6379 = K[2386];
      at = 8778; l6377 = l6379;
    }
    at = 8779; l6380 = K[2387];
    at = 8780; l6381 = rt.int_div(l6377, l6380);
    at = 8781; l6382 = K[2388];
    at = 8782; l6383 = rt.max(l6382, l6381);
    at = 8783; l6384 = K[2389];
    at = 8784; l6385 = rt.gt(l6367, l6384);
    at = 8785; if (rt.truthy(l6385)) {
      at = 8786; if (rt.truthy(l6360)) {
        at = 8787; l6388 = field(rt, l6363, "color");
        at = 8788; l6387 = l6388;
      } else {
        at = 8789; l6389 = K[2390];
        at = 8790; l6387 = l6389;
      }
      at = 8791; l6390 = K[2391];
      at = 8792; l6391 = rt.sequence_get(l6390, l6387);
      at = 8793; l6386 = l6391;
    } else {
      at = 8794; l6392 = K[2392];
      at = 8795; l6386 = l6392;
    }
    at = 8796; markSet(rt, false, "circle", {at: l6370, radius: l6383, fill: l6386});
    at = 8797; if (rt.truthy(l6360)) {
      at = 8798; l6394 = field(rt, l6363, "points");
      at = 8799; l6393 = l6394;
    } else {
      at = 8800; l6395 = K[2393];
      at = 8801; l6393 = l6395;
    }
    at = 8802; l6396 = K[2394];
    at = 8803; l6397 = rt.sub(l6367, l6396);
    at = 8804; l6398 = K[2395];
    at = 8805; l6399 = rt.max(l6398, l6397);
    at = 8806; l6400 = K[2396];
    at = 8807; l6401 = rt.array_slice(l6393, l6400, l6399);
    at = 8808; if (rt.truthy(l6360)) {
      at = 8809; l6403 = field(rt, l6363, "points");
      at = 8810; l6402 = l6403;
    } else {
      at = 8811; l6404 = K[2397];
      at = 8812; l6402 = l6404;
    }
    at = 8813; l6405 = K[2398];
    at = 8814; l6406 = rt.gt(l6367, l6405);
    at = 8815; if (rt.truthy(l6406)) {
      at = 8816; l6408 = K[2399];
      at = 8817; l6407 = l6408;
    } else {
      at = 8818; l6409 = K[2400];
      at = 8819; l6407 = l6409;
    }
    at = 8820; l6410 = K[2401];
    at = 8821; l6411 = rt.sub(l6367, l6410);
    at = 8822; l6412 = K[2402];
    at = 8823; l6413 = rt.max(l6412, l6411);
    at = 8824; l6414 = rt.array_slice(l6402, l6407, l6413);
    at = 8825; if (rt.truthy(l6360)) {
      at = 8826; l6416 = field(rt, l6363, "color");
      at = 8827; l6415 = l6416;
    } else {
      at = 8828; l6417 = K[2403];
      at = 8829; l6415 = l6417;
    }
    at = 8830; l6418 = K[2404];
    at = 8831; l6419 = rt.sequence_get(l6418, l6415);
    at = 8832; if (rt.truthy(l6360)) {
      at = 8833; l6421 = field(rt, l6363, "width");
      at = 8834; l6420 = l6421;
    } else {
      at = 8835; l6422 = K[2405];
      at = 8836; l6420 = l6422;
    }
    at = 8837; markSet(rt, true, "line", {from: l6401, to: l6414, color: l6419, width: l6420});
    at = 8838; l6423 = field(rt, l15, "strokes");
    at = 8839; l6424 = rt.sequence_length(l6423);
    at = 8840; l6425 = K[2406];
    at = 8841; l6426 = rt.lt(l6425, l6424);
    at = 8842; if (rt.truthy(l6426)) {
      at = 8843; l6427 = field(rt, l15, "strokes");
      at = 8844; l6428 = K[2407];
      at = 8845; l6429 = rt.sequence_get(l6427, l6428);
    }
    at = 8846; if (rt.truthy(l6426)) {
      at = 8847; l6431 = field(rt, l6429, "points");
      at = 8848; l6430 = l6431;
    } else {
      at = 8849; l6432 = K[2408];
      at = 8850; l6430 = l6432;
    }
    at = 8851; l6433 = rt.array_count(l6430);
    at = 8852; l6434 = K[2409];
    at = 8853; l6435 = rt.gt(l6433, l6434);
    at = 8854; if (rt.truthy(l6435)) {
      at = 8855; if (rt.truthy(l6426)) {
        at = 8856; l6438 = field(rt, l6429, "points");
        at = 8857; l6437 = l6438;
      } else {
        at = 8858; l6439 = K[2410];
        at = 8859; l6437 = l6439;
      }
      at = 8860; l6440 = K[2411];
      at = 8861; l6441 = rt.array_nth(l6437, l6440);
      at = 8862; l6436 = l6441;
    } else {
      at = 8863; l6442 = K[2412];
      at = 8864; l6436 = l6442;
    }
    at = 8865; if (rt.truthy(l6426)) {
      at = 8866; l6444 = field(rt, l6429, "width");
      at = 8867; l6443 = l6444;
    } else {
      at = 8868; l6445 = K[2413];
      at = 8869; l6443 = l6445;
    }
    at = 8870; l6446 = K[2414];
    at = 8871; l6447 = rt.int_div(l6443, l6446);
    at = 8872; l6448 = K[2415];
    at = 8873; l6449 = rt.max(l6448, l6447);
    at = 8874; l6450 = K[2416];
    at = 8875; l6451 = rt.gt(l6433, l6450);
    at = 8876; if (rt.truthy(l6451)) {
      at = 8877; if (rt.truthy(l6426)) {
        at = 8878; l6454 = field(rt, l6429, "color");
        at = 8879; l6453 = l6454;
      } else {
        at = 8880; l6455 = K[2417];
        at = 8881; l6453 = l6455;
      }
      at = 8882; l6456 = K[2418];
      at = 8883; l6457 = rt.sequence_get(l6456, l6453);
      at = 8884; l6452 = l6457;
    } else {
      at = 8885; l6458 = K[2419];
      at = 8886; l6452 = l6458;
    }
    at = 8887; markSet(rt, false, "circle", {at: l6436, radius: l6449, fill: l6452});
    at = 8888; if (rt.truthy(l6426)) {
      at = 8889; l6460 = field(rt, l6429, "points");
      at = 8890; l6459 = l6460;
    } else {
      at = 8891; l6461 = K[2420];
      at = 8892; l6459 = l6461;
    }
    at = 8893; l6462 = K[2421];
    at = 8894; l6463 = rt.sub(l6433, l6462);
    at = 8895; l6464 = K[2422];
    at = 8896; l6465 = rt.max(l6464, l6463);
    at = 8897; l6466 = K[2423];
    at = 8898; l6467 = rt.array_slice(l6459, l6466, l6465);
    at = 8899; if (rt.truthy(l6426)) {
      at = 8900; l6469 = field(rt, l6429, "points");
      at = 8901; l6468 = l6469;
    } else {
      at = 8902; l6470 = K[2424];
      at = 8903; l6468 = l6470;
    }
    at = 8904; l6471 = K[2425];
    at = 8905; l6472 = rt.gt(l6433, l6471);
    at = 8906; if (rt.truthy(l6472)) {
      at = 8907; l6474 = K[2426];
      at = 8908; l6473 = l6474;
    } else {
      at = 8909; l6475 = K[2427];
      at = 8910; l6473 = l6475;
    }
    at = 8911; l6476 = K[2428];
    at = 8912; l6477 = rt.sub(l6433, l6476);
    at = 8913; l6478 = K[2429];
    at = 8914; l6479 = rt.max(l6478, l6477);
    at = 8915; l6480 = rt.array_slice(l6468, l6473, l6479);
    at = 8916; if (rt.truthy(l6426)) {
      at = 8917; l6482 = field(rt, l6429, "color");
      at = 8918; l6481 = l6482;
    } else {
      at = 8919; l6483 = K[2430];
      at = 8920; l6481 = l6483;
    }
    at = 8921; l6484 = K[2431];
    at = 8922; l6485 = rt.sequence_get(l6484, l6481);
    at = 8923; if (rt.truthy(l6426)) {
      at = 8924; l6487 = field(rt, l6429, "width");
      at = 8925; l6486 = l6487;
    } else {
      at = 8926; l6488 = K[2432];
      at = 8927; l6486 = l6488;
    }
    at = 8928; markSet(rt, true, "line", {from: l6467, to: l6480, color: l6485, width: l6486});
    at = 8929; l6489 = field(rt, l15, "strokes");
    at = 8930; l6490 = rt.sequence_length(l6489);
    at = 8931; l6491 = K[2433];
    at = 8932; l6492 = rt.lt(l6491, l6490);
    at = 8933; if (rt.truthy(l6492)) {
      at = 8934; l6493 = field(rt, l15, "strokes");
      at = 8935; l6494 = K[2434];
      at = 8936; l6495 = rt.sequence_get(l6493, l6494);
    }
    at = 8937; if (rt.truthy(l6492)) {
      at = 8938; l6497 = field(rt, l6495, "points");
      at = 8939; l6496 = l6497;
    } else {
      at = 8940; l6498 = K[2435];
      at = 8941; l6496 = l6498;
    }
    at = 8942; l6499 = rt.array_count(l6496);
    at = 8943; l6500 = K[2436];
    at = 8944; l6501 = rt.gt(l6499, l6500);
    at = 8945; if (rt.truthy(l6501)) {
      at = 8946; if (rt.truthy(l6492)) {
        at = 8947; l6504 = field(rt, l6495, "points");
        at = 8948; l6503 = l6504;
      } else {
        at = 8949; l6505 = K[2437];
        at = 8950; l6503 = l6505;
      }
      at = 8951; l6506 = K[2438];
      at = 8952; l6507 = rt.array_nth(l6503, l6506);
      at = 8953; l6502 = l6507;
    } else {
      at = 8954; l6508 = K[2439];
      at = 8955; l6502 = l6508;
    }
    at = 8956; if (rt.truthy(l6492)) {
      at = 8957; l6510 = field(rt, l6495, "width");
      at = 8958; l6509 = l6510;
    } else {
      at = 8959; l6511 = K[2440];
      at = 8960; l6509 = l6511;
    }
    at = 8961; l6512 = K[2441];
    at = 8962; l6513 = rt.int_div(l6509, l6512);
    at = 8963; l6514 = K[2442];
    at = 8964; l6515 = rt.max(l6514, l6513);
    at = 8965; l6516 = K[2443];
    at = 8966; l6517 = rt.gt(l6499, l6516);
    at = 8967; if (rt.truthy(l6517)) {
      at = 8968; if (rt.truthy(l6492)) {
        at = 8969; l6520 = field(rt, l6495, "color");
        at = 8970; l6519 = l6520;
      } else {
        at = 8971; l6521 = K[2444];
        at = 8972; l6519 = l6521;
      }
      at = 8973; l6522 = K[2445];
      at = 8974; l6523 = rt.sequence_get(l6522, l6519);
      at = 8975; l6518 = l6523;
    } else {
      at = 8976; l6524 = K[2446];
      at = 8977; l6518 = l6524;
    }
    at = 8978; markSet(rt, false, "circle", {at: l6502, radius: l6515, fill: l6518});
    at = 8979; if (rt.truthy(l6492)) {
      at = 8980; l6526 = field(rt, l6495, "points");
      at = 8981; l6525 = l6526;
    } else {
      at = 8982; l6527 = K[2447];
      at = 8983; l6525 = l6527;
    }
    at = 8984; l6528 = K[2448];
    at = 8985; l6529 = rt.sub(l6499, l6528);
    at = 8986; l6530 = K[2449];
    at = 8987; l6531 = rt.max(l6530, l6529);
    at = 8988; l6532 = K[2450];
    at = 8989; l6533 = rt.array_slice(l6525, l6532, l6531);
    at = 8990; if (rt.truthy(l6492)) {
      at = 8991; l6535 = field(rt, l6495, "points");
      at = 8992; l6534 = l6535;
    } else {
      at = 8993; l6536 = K[2451];
      at = 8994; l6534 = l6536;
    }
    at = 8995; l6537 = K[2452];
    at = 8996; l6538 = rt.gt(l6499, l6537);
    at = 8997; if (rt.truthy(l6538)) {
      at = 8998; l6540 = K[2453];
      at = 8999; l6539 = l6540;
    } else {
      at = 9000; l6541 = K[2454];
      at = 9001; l6539 = l6541;
    }
    at = 9002; l6542 = K[2455];
    at = 9003; l6543 = rt.sub(l6499, l6542);
    at = 9004; l6544 = K[2456];
    at = 9005; l6545 = rt.max(l6544, l6543);
    at = 9006; l6546 = rt.array_slice(l6534, l6539, l6545);
    at = 9007; if (rt.truthy(l6492)) {
      at = 9008; l6548 = field(rt, l6495, "color");
      at = 9009; l6547 = l6548;
    } else {
      at = 9010; l6549 = K[2457];
      at = 9011; l6547 = l6549;
    }
    at = 9012; l6550 = K[2458];
    at = 9013; l6551 = rt.sequence_get(l6550, l6547);
    at = 9014; if (rt.truthy(l6492)) {
      at = 9015; l6553 = field(rt, l6495, "width");
      at = 9016; l6552 = l6553;
    } else {
      at = 9017; l6554 = K[2459];
      at = 9018; l6552 = l6554;
    }
    at = 9019; markSet(rt, true, "line", {from: l6533, to: l6546, color: l6551, width: l6552});
    at = 9020; l6555 = field(rt, l15, "strokes");
    at = 9021; l6556 = rt.sequence_length(l6555);
    at = 9022; l6557 = K[2460];
    at = 9023; l6558 = rt.lt(l6557, l6556);
    at = 9024; if (rt.truthy(l6558)) {
      at = 9025; l6559 = field(rt, l15, "strokes");
      at = 9026; l6560 = K[2461];
      at = 9027; l6561 = rt.sequence_get(l6559, l6560);
    }
    at = 9028; if (rt.truthy(l6558)) {
      at = 9029; l6563 = field(rt, l6561, "points");
      at = 9030; l6562 = l6563;
    } else {
      at = 9031; l6564 = K[2462];
      at = 9032; l6562 = l6564;
    }
    at = 9033; l6565 = rt.array_count(l6562);
    at = 9034; l6566 = K[2463];
    at = 9035; l6567 = rt.gt(l6565, l6566);
    at = 9036; if (rt.truthy(l6567)) {
      at = 9037; if (rt.truthy(l6558)) {
        at = 9038; l6570 = field(rt, l6561, "points");
        at = 9039; l6569 = l6570;
      } else {
        at = 9040; l6571 = K[2464];
        at = 9041; l6569 = l6571;
      }
      at = 9042; l6572 = K[2465];
      at = 9043; l6573 = rt.array_nth(l6569, l6572);
      at = 9044; l6568 = l6573;
    } else {
      at = 9045; l6574 = K[2466];
      at = 9046; l6568 = l6574;
    }
    at = 9047; if (rt.truthy(l6558)) {
      at = 9048; l6576 = field(rt, l6561, "width");
      at = 9049; l6575 = l6576;
    } else {
      at = 9050; l6577 = K[2467];
      at = 9051; l6575 = l6577;
    }
    at = 9052; l6578 = K[2468];
    at = 9053; l6579 = rt.int_div(l6575, l6578);
    at = 9054; l6580 = K[2469];
    at = 9055; l6581 = rt.max(l6580, l6579);
    at = 9056; l6582 = K[2470];
    at = 9057; l6583 = rt.gt(l6565, l6582);
    at = 9058; if (rt.truthy(l6583)) {
      at = 9059; if (rt.truthy(l6558)) {
        at = 9060; l6586 = field(rt, l6561, "color");
        at = 9061; l6585 = l6586;
      } else {
        at = 9062; l6587 = K[2471];
        at = 9063; l6585 = l6587;
      }
      at = 9064; l6588 = K[2472];
      at = 9065; l6589 = rt.sequence_get(l6588, l6585);
      at = 9066; l6584 = l6589;
    } else {
      at = 9067; l6590 = K[2473];
      at = 9068; l6584 = l6590;
    }
    at = 9069; markSet(rt, false, "circle", {at: l6568, radius: l6581, fill: l6584});
    at = 9070; if (rt.truthy(l6558)) {
      at = 9071; l6592 = field(rt, l6561, "points");
      at = 9072; l6591 = l6592;
    } else {
      at = 9073; l6593 = K[2474];
      at = 9074; l6591 = l6593;
    }
    at = 9075; l6594 = K[2475];
    at = 9076; l6595 = rt.sub(l6565, l6594);
    at = 9077; l6596 = K[2476];
    at = 9078; l6597 = rt.max(l6596, l6595);
    at = 9079; l6598 = K[2477];
    at = 9080; l6599 = rt.array_slice(l6591, l6598, l6597);
    at = 9081; if (rt.truthy(l6558)) {
      at = 9082; l6601 = field(rt, l6561, "points");
      at = 9083; l6600 = l6601;
    } else {
      at = 9084; l6602 = K[2478];
      at = 9085; l6600 = l6602;
    }
    at = 9086; l6603 = K[2479];
    at = 9087; l6604 = rt.gt(l6565, l6603);
    at = 9088; if (rt.truthy(l6604)) {
      at = 9089; l6606 = K[2480];
      at = 9090; l6605 = l6606;
    } else {
      at = 9091; l6607 = K[2481];
      at = 9092; l6605 = l6607;
    }
    at = 9093; l6608 = K[2482];
    at = 9094; l6609 = rt.sub(l6565, l6608);
    at = 9095; l6610 = K[2483];
    at = 9096; l6611 = rt.max(l6610, l6609);
    at = 9097; l6612 = rt.array_slice(l6600, l6605, l6611);
    at = 9098; if (rt.truthy(l6558)) {
      at = 9099; l6614 = field(rt, l6561, "color");
      at = 9100; l6613 = l6614;
    } else {
      at = 9101; l6615 = K[2484];
      at = 9102; l6613 = l6615;
    }
    at = 9103; l6616 = K[2485];
    at = 9104; l6617 = rt.sequence_get(l6616, l6613);
    at = 9105; if (rt.truthy(l6558)) {
      at = 9106; l6619 = field(rt, l6561, "width");
      at = 9107; l6618 = l6619;
    } else {
      at = 9108; l6620 = K[2486];
      at = 9109; l6618 = l6620;
    }
    at = 9110; markSet(rt, true, "line", {from: l6599, to: l6612, color: l6617, width: l6618});
    at = 9111; l6621 = field(rt, l15, "strokes");
    at = 9112; l6622 = rt.sequence_length(l6621);
    at = 9113; l6623 = K[2487];
    at = 9114; l6624 = rt.lt(l6623, l6622);
    at = 9115; if (rt.truthy(l6624)) {
      at = 9116; l6625 = field(rt, l15, "strokes");
      at = 9117; l6626 = K[2488];
      at = 9118; l6627 = rt.sequence_get(l6625, l6626);
    }
    at = 9119; if (rt.truthy(l6624)) {
      at = 9120; l6629 = field(rt, l6627, "points");
      at = 9121; l6628 = l6629;
    } else {
      at = 9122; l6630 = K[2489];
      at = 9123; l6628 = l6630;
    }
    at = 9124; l6631 = rt.array_count(l6628);
    at = 9125; l6632 = K[2490];
    at = 9126; l6633 = rt.gt(l6631, l6632);
    at = 9127; if (rt.truthy(l6633)) {
      at = 9128; if (rt.truthy(l6624)) {
        at = 9129; l6636 = field(rt, l6627, "points");
        at = 9130; l6635 = l6636;
      } else {
        at = 9131; l6637 = K[2491];
        at = 9132; l6635 = l6637;
      }
      at = 9133; l6638 = K[2492];
      at = 9134; l6639 = rt.array_nth(l6635, l6638);
      at = 9135; l6634 = l6639;
    } else {
      at = 9136; l6640 = K[2493];
      at = 9137; l6634 = l6640;
    }
    at = 9138; if (rt.truthy(l6624)) {
      at = 9139; l6642 = field(rt, l6627, "width");
      at = 9140; l6641 = l6642;
    } else {
      at = 9141; l6643 = K[2494];
      at = 9142; l6641 = l6643;
    }
    at = 9143; l6644 = K[2495];
    at = 9144; l6645 = rt.int_div(l6641, l6644);
    at = 9145; l6646 = K[2496];
    at = 9146; l6647 = rt.max(l6646, l6645);
    at = 9147; l6648 = K[2497];
    at = 9148; l6649 = rt.gt(l6631, l6648);
    at = 9149; if (rt.truthy(l6649)) {
      at = 9150; if (rt.truthy(l6624)) {
        at = 9151; l6652 = field(rt, l6627, "color");
        at = 9152; l6651 = l6652;
      } else {
        at = 9153; l6653 = K[2498];
        at = 9154; l6651 = l6653;
      }
      at = 9155; l6654 = K[2499];
      at = 9156; l6655 = rt.sequence_get(l6654, l6651);
      at = 9157; l6650 = l6655;
    } else {
      at = 9158; l6656 = K[2500];
      at = 9159; l6650 = l6656;
    }
    at = 9160; markSet(rt, false, "circle", {at: l6634, radius: l6647, fill: l6650});
    at = 9161; if (rt.truthy(l6624)) {
      at = 9162; l6658 = field(rt, l6627, "points");
      at = 9163; l6657 = l6658;
    } else {
      at = 9164; l6659 = K[2501];
      at = 9165; l6657 = l6659;
    }
    at = 9166; l6660 = K[2502];
    at = 9167; l6661 = rt.sub(l6631, l6660);
    at = 9168; l6662 = K[2503];
    at = 9169; l6663 = rt.max(l6662, l6661);
    at = 9170; l6664 = K[2504];
    at = 9171; l6665 = rt.array_slice(l6657, l6664, l6663);
    at = 9172; if (rt.truthy(l6624)) {
      at = 9173; l6667 = field(rt, l6627, "points");
      at = 9174; l6666 = l6667;
    } else {
      at = 9175; l6668 = K[2505];
      at = 9176; l6666 = l6668;
    }
    at = 9177; l6669 = K[2506];
    at = 9178; l6670 = rt.gt(l6631, l6669);
    at = 9179; if (rt.truthy(l6670)) {
      at = 9180; l6672 = K[2507];
      at = 9181; l6671 = l6672;
    } else {
      at = 9182; l6673 = K[2508];
      at = 9183; l6671 = l6673;
    }
    at = 9184; l6674 = K[2509];
    at = 9185; l6675 = rt.sub(l6631, l6674);
    at = 9186; l6676 = K[2510];
    at = 9187; l6677 = rt.max(l6676, l6675);
    at = 9188; l6678 = rt.array_slice(l6666, l6671, l6677);
    at = 9189; if (rt.truthy(l6624)) {
      at = 9190; l6680 = field(rt, l6627, "color");
      at = 9191; l6679 = l6680;
    } else {
      at = 9192; l6681 = K[2511];
      at = 9193; l6679 = l6681;
    }
    at = 9194; l6682 = K[2512];
    at = 9195; l6683 = rt.sequence_get(l6682, l6679);
    at = 9196; if (rt.truthy(l6624)) {
      at = 9197; l6685 = field(rt, l6627, "width");
      at = 9198; l6684 = l6685;
    } else {
      at = 9199; l6686 = K[2513];
      at = 9200; l6684 = l6686;
    }
    at = 9201; markSet(rt, true, "line", {from: l6665, to: l6678, color: l6683, width: l6684});
    at = 9202; l6687 = field(rt, l15, "strokes");
    at = 9203; l6688 = rt.sequence_length(l6687);
    at = 9204; l6689 = K[2514];
    at = 9205; l6690 = rt.lt(l6689, l6688);
    at = 9206; if (rt.truthy(l6690)) {
      at = 9207; l6691 = field(rt, l15, "strokes");
      at = 9208; l6692 = K[2515];
      at = 9209; l6693 = rt.sequence_get(l6691, l6692);
    }
    at = 9210; if (rt.truthy(l6690)) {
      at = 9211; l6695 = field(rt, l6693, "points");
      at = 9212; l6694 = l6695;
    } else {
      at = 9213; l6696 = K[2516];
      at = 9214; l6694 = l6696;
    }
    at = 9215; l6697 = rt.array_count(l6694);
    at = 9216; l6698 = K[2517];
    at = 9217; l6699 = rt.gt(l6697, l6698);
    at = 9218; if (rt.truthy(l6699)) {
      at = 9219; if (rt.truthy(l6690)) {
        at = 9220; l6702 = field(rt, l6693, "points");
        at = 9221; l6701 = l6702;
      } else {
        at = 9222; l6703 = K[2518];
        at = 9223; l6701 = l6703;
      }
      at = 9224; l6704 = K[2519];
      at = 9225; l6705 = rt.array_nth(l6701, l6704);
      at = 9226; l6700 = l6705;
    } else {
      at = 9227; l6706 = K[2520];
      at = 9228; l6700 = l6706;
    }
    at = 9229; if (rt.truthy(l6690)) {
      at = 9230; l6708 = field(rt, l6693, "width");
      at = 9231; l6707 = l6708;
    } else {
      at = 9232; l6709 = K[2521];
      at = 9233; l6707 = l6709;
    }
    at = 9234; l6710 = K[2522];
    at = 9235; l6711 = rt.int_div(l6707, l6710);
    at = 9236; l6712 = K[2523];
    at = 9237; l6713 = rt.max(l6712, l6711);
    at = 9238; l6714 = K[2524];
    at = 9239; l6715 = rt.gt(l6697, l6714);
    at = 9240; if (rt.truthy(l6715)) {
      at = 9241; if (rt.truthy(l6690)) {
        at = 9242; l6718 = field(rt, l6693, "color");
        at = 9243; l6717 = l6718;
      } else {
        at = 9244; l6719 = K[2525];
        at = 9245; l6717 = l6719;
      }
      at = 9246; l6720 = K[2526];
      at = 9247; l6721 = rt.sequence_get(l6720, l6717);
      at = 9248; l6716 = l6721;
    } else {
      at = 9249; l6722 = K[2527];
      at = 9250; l6716 = l6722;
    }
    at = 9251; markSet(rt, false, "circle", {at: l6700, radius: l6713, fill: l6716});
    at = 9252; if (rt.truthy(l6690)) {
      at = 9253; l6724 = field(rt, l6693, "points");
      at = 9254; l6723 = l6724;
    } else {
      at = 9255; l6725 = K[2528];
      at = 9256; l6723 = l6725;
    }
    at = 9257; l6726 = K[2529];
    at = 9258; l6727 = rt.sub(l6697, l6726);
    at = 9259; l6728 = K[2530];
    at = 9260; l6729 = rt.max(l6728, l6727);
    at = 9261; l6730 = K[2531];
    at = 9262; l6731 = rt.array_slice(l6723, l6730, l6729);
    at = 9263; if (rt.truthy(l6690)) {
      at = 9264; l6733 = field(rt, l6693, "points");
      at = 9265; l6732 = l6733;
    } else {
      at = 9266; l6734 = K[2532];
      at = 9267; l6732 = l6734;
    }
    at = 9268; l6735 = K[2533];
    at = 9269; l6736 = rt.gt(l6697, l6735);
    at = 9270; if (rt.truthy(l6736)) {
      at = 9271; l6738 = K[2534];
      at = 9272; l6737 = l6738;
    } else {
      at = 9273; l6739 = K[2535];
      at = 9274; l6737 = l6739;
    }
    at = 9275; l6740 = K[2536];
    at = 9276; l6741 = rt.sub(l6697, l6740);
    at = 9277; l6742 = K[2537];
    at = 9278; l6743 = rt.max(l6742, l6741);
    at = 9279; l6744 = rt.array_slice(l6732, l6737, l6743);
    at = 9280; if (rt.truthy(l6690)) {
      at = 9281; l6746 = field(rt, l6693, "color");
      at = 9282; l6745 = l6746;
    } else {
      at = 9283; l6747 = K[2538];
      at = 9284; l6745 = l6747;
    }
    at = 9285; l6748 = K[2539];
    at = 9286; l6749 = rt.sequence_get(l6748, l6745);
    at = 9287; if (rt.truthy(l6690)) {
      at = 9288; l6751 = field(rt, l6693, "width");
      at = 9289; l6750 = l6751;
    } else {
      at = 9290; l6752 = K[2540];
      at = 9291; l6750 = l6752;
    }
    at = 9292; markSet(rt, true, "line", {from: l6731, to: l6744, color: l6749, width: l6750});
    at = 9293; l6753 = field(rt, l15, "strokes");
    at = 9294; l6754 = rt.sequence_length(l6753);
    at = 9295; l6755 = K[2541];
    at = 9296; l6756 = rt.lt(l6755, l6754);
    at = 9297; if (rt.truthy(l6756)) {
      at = 9298; l6757 = field(rt, l15, "strokes");
      at = 9299; l6758 = K[2542];
      at = 9300; l6759 = rt.sequence_get(l6757, l6758);
    }
    at = 9301; if (rt.truthy(l6756)) {
      at = 9302; l6761 = field(rt, l6759, "points");
      at = 9303; l6760 = l6761;
    } else {
      at = 9304; l6762 = K[2543];
      at = 9305; l6760 = l6762;
    }
    at = 9306; l6763 = rt.array_count(l6760);
    at = 9307; l6764 = K[2544];
    at = 9308; l6765 = rt.gt(l6763, l6764);
    at = 9309; if (rt.truthy(l6765)) {
      at = 9310; if (rt.truthy(l6756)) {
        at = 9311; l6768 = field(rt, l6759, "points");
        at = 9312; l6767 = l6768;
      } else {
        at = 9313; l6769 = K[2545];
        at = 9314; l6767 = l6769;
      }
      at = 9315; l6770 = K[2546];
      at = 9316; l6771 = rt.array_nth(l6767, l6770);
      at = 9317; l6766 = l6771;
    } else {
      at = 9318; l6772 = K[2547];
      at = 9319; l6766 = l6772;
    }
    at = 9320; if (rt.truthy(l6756)) {
      at = 9321; l6774 = field(rt, l6759, "width");
      at = 9322; l6773 = l6774;
    } else {
      at = 9323; l6775 = K[2548];
      at = 9324; l6773 = l6775;
    }
    at = 9325; l6776 = K[2549];
    at = 9326; l6777 = rt.int_div(l6773, l6776);
    at = 9327; l6778 = K[2550];
    at = 9328; l6779 = rt.max(l6778, l6777);
    at = 9329; l6780 = K[2551];
    at = 9330; l6781 = rt.gt(l6763, l6780);
    at = 9331; if (rt.truthy(l6781)) {
      at = 9332; if (rt.truthy(l6756)) {
        at = 9333; l6784 = field(rt, l6759, "color");
        at = 9334; l6783 = l6784;
      } else {
        at = 9335; l6785 = K[2552];
        at = 9336; l6783 = l6785;
      }
      at = 9337; l6786 = K[2553];
      at = 9338; l6787 = rt.sequence_get(l6786, l6783);
      at = 9339; l6782 = l6787;
    } else {
      at = 9340; l6788 = K[2554];
      at = 9341; l6782 = l6788;
    }
    at = 9342; markSet(rt, false, "circle", {at: l6766, radius: l6779, fill: l6782});
    at = 9343; if (rt.truthy(l6756)) {
      at = 9344; l6790 = field(rt, l6759, "points");
      at = 9345; l6789 = l6790;
    } else {
      at = 9346; l6791 = K[2555];
      at = 9347; l6789 = l6791;
    }
    at = 9348; l6792 = K[2556];
    at = 9349; l6793 = rt.sub(l6763, l6792);
    at = 9350; l6794 = K[2557];
    at = 9351; l6795 = rt.max(l6794, l6793);
    at = 9352; l6796 = K[2558];
    at = 9353; l6797 = rt.array_slice(l6789, l6796, l6795);
    at = 9354; if (rt.truthy(l6756)) {
      at = 9355; l6799 = field(rt, l6759, "points");
      at = 9356; l6798 = l6799;
    } else {
      at = 9357; l6800 = K[2559];
      at = 9358; l6798 = l6800;
    }
    at = 9359; l6801 = K[2560];
    at = 9360; l6802 = rt.gt(l6763, l6801);
    at = 9361; if (rt.truthy(l6802)) {
      at = 9362; l6804 = K[2561];
      at = 9363; l6803 = l6804;
    } else {
      at = 9364; l6805 = K[2562];
      at = 9365; l6803 = l6805;
    }
    at = 9366; l6806 = K[2563];
    at = 9367; l6807 = rt.sub(l6763, l6806);
    at = 9368; l6808 = K[2564];
    at = 9369; l6809 = rt.max(l6808, l6807);
    at = 9370; l6810 = rt.array_slice(l6798, l6803, l6809);
    at = 9371; if (rt.truthy(l6756)) {
      at = 9372; l6812 = field(rt, l6759, "color");
      at = 9373; l6811 = l6812;
    } else {
      at = 9374; l6813 = K[2565];
      at = 9375; l6811 = l6813;
    }
    at = 9376; l6814 = K[2566];
    at = 9377; l6815 = rt.sequence_get(l6814, l6811);
    at = 9378; if (rt.truthy(l6756)) {
      at = 9379; l6817 = field(rt, l6759, "width");
      at = 9380; l6816 = l6817;
    } else {
      at = 9381; l6818 = K[2567];
      at = 9382; l6816 = l6818;
    }
    at = 9383; markSet(rt, true, "line", {from: l6797, to: l6810, color: l6815, width: l6816});
    at = 9384; l6819 = field(rt, l15, "strokes");
    at = 9385; l6820 = rt.sequence_length(l6819);
    at = 9386; l6821 = K[2568];
    at = 9387; l6822 = rt.lt(l6821, l6820);
    at = 9388; if (rt.truthy(l6822)) {
      at = 9389; l6823 = field(rt, l15, "strokes");
      at = 9390; l6824 = K[2569];
      at = 9391; l6825 = rt.sequence_get(l6823, l6824);
    }
    at = 9392; if (rt.truthy(l6822)) {
      at = 9393; l6827 = field(rt, l6825, "points");
      at = 9394; l6826 = l6827;
    } else {
      at = 9395; l6828 = K[2570];
      at = 9396; l6826 = l6828;
    }
    at = 9397; l6829 = rt.array_count(l6826);
    at = 9398; l6830 = K[2571];
    at = 9399; l6831 = rt.gt(l6829, l6830);
    at = 9400; if (rt.truthy(l6831)) {
      at = 9401; if (rt.truthy(l6822)) {
        at = 9402; l6834 = field(rt, l6825, "points");
        at = 9403; l6833 = l6834;
      } else {
        at = 9404; l6835 = K[2572];
        at = 9405; l6833 = l6835;
      }
      at = 9406; l6836 = K[2573];
      at = 9407; l6837 = rt.array_nth(l6833, l6836);
      at = 9408; l6832 = l6837;
    } else {
      at = 9409; l6838 = K[2574];
      at = 9410; l6832 = l6838;
    }
    at = 9411; if (rt.truthy(l6822)) {
      at = 9412; l6840 = field(rt, l6825, "width");
      at = 9413; l6839 = l6840;
    } else {
      at = 9414; l6841 = K[2575];
      at = 9415; l6839 = l6841;
    }
    at = 9416; l6842 = K[2576];
    at = 9417; l6843 = rt.int_div(l6839, l6842);
    at = 9418; l6844 = K[2577];
    at = 9419; l6845 = rt.max(l6844, l6843);
    at = 9420; l6846 = K[2578];
    at = 9421; l6847 = rt.gt(l6829, l6846);
    at = 9422; if (rt.truthy(l6847)) {
      at = 9423; if (rt.truthy(l6822)) {
        at = 9424; l6850 = field(rt, l6825, "color");
        at = 9425; l6849 = l6850;
      } else {
        at = 9426; l6851 = K[2579];
        at = 9427; l6849 = l6851;
      }
      at = 9428; l6852 = K[2580];
      at = 9429; l6853 = rt.sequence_get(l6852, l6849);
      at = 9430; l6848 = l6853;
    } else {
      at = 9431; l6854 = K[2581];
      at = 9432; l6848 = l6854;
    }
    at = 9433; markSet(rt, false, "circle", {at: l6832, radius: l6845, fill: l6848});
    at = 9434; if (rt.truthy(l6822)) {
      at = 9435; l6856 = field(rt, l6825, "points");
      at = 9436; l6855 = l6856;
    } else {
      at = 9437; l6857 = K[2582];
      at = 9438; l6855 = l6857;
    }
    at = 9439; l6858 = K[2583];
    at = 9440; l6859 = rt.sub(l6829, l6858);
    at = 9441; l6860 = K[2584];
    at = 9442; l6861 = rt.max(l6860, l6859);
    at = 9443; l6862 = K[2585];
    at = 9444; l6863 = rt.array_slice(l6855, l6862, l6861);
    at = 9445; if (rt.truthy(l6822)) {
      at = 9446; l6865 = field(rt, l6825, "points");
      at = 9447; l6864 = l6865;
    } else {
      at = 9448; l6866 = K[2586];
      at = 9449; l6864 = l6866;
    }
    at = 9450; l6867 = K[2587];
    at = 9451; l6868 = rt.gt(l6829, l6867);
    at = 9452; if (rt.truthy(l6868)) {
      at = 9453; l6870 = K[2588];
      at = 9454; l6869 = l6870;
    } else {
      at = 9455; l6871 = K[2589];
      at = 9456; l6869 = l6871;
    }
    at = 9457; l6872 = K[2590];
    at = 9458; l6873 = rt.sub(l6829, l6872);
    at = 9459; l6874 = K[2591];
    at = 9460; l6875 = rt.max(l6874, l6873);
    at = 9461; l6876 = rt.array_slice(l6864, l6869, l6875);
    at = 9462; if (rt.truthy(l6822)) {
      at = 9463; l6878 = field(rt, l6825, "color");
      at = 9464; l6877 = l6878;
    } else {
      at = 9465; l6879 = K[2592];
      at = 9466; l6877 = l6879;
    }
    at = 9467; l6880 = K[2593];
    at = 9468; l6881 = rt.sequence_get(l6880, l6877);
    at = 9469; if (rt.truthy(l6822)) {
      at = 9470; l6883 = field(rt, l6825, "width");
      at = 9471; l6882 = l6883;
    } else {
      at = 9472; l6884 = K[2594];
      at = 9473; l6882 = l6884;
    }
    at = 9474; markSet(rt, true, "line", {from: l6863, to: l6876, color: l6881, width: l6882});
    at = 9475; l6885 = field(rt, l15, "strokes");
    at = 9476; l6886 = rt.sequence_length(l6885);
    at = 9477; l6887 = K[2595];
    at = 9478; l6888 = rt.lt(l6887, l6886);
    at = 9479; if (rt.truthy(l6888)) {
      at = 9480; l6889 = field(rt, l15, "strokes");
      at = 9481; l6890 = K[2596];
      at = 9482; l6891 = rt.sequence_get(l6889, l6890);
    }
    at = 9483; if (rt.truthy(l6888)) {
      at = 9484; l6893 = field(rt, l6891, "points");
      at = 9485; l6892 = l6893;
    } else {
      at = 9486; l6894 = K[2597];
      at = 9487; l6892 = l6894;
    }
    at = 9488; l6895 = rt.array_count(l6892);
    at = 9489; l6896 = K[2598];
    at = 9490; l6897 = rt.gt(l6895, l6896);
    at = 9491; if (rt.truthy(l6897)) {
      at = 9492; if (rt.truthy(l6888)) {
        at = 9493; l6900 = field(rt, l6891, "points");
        at = 9494; l6899 = l6900;
      } else {
        at = 9495; l6901 = K[2599];
        at = 9496; l6899 = l6901;
      }
      at = 9497; l6902 = K[2600];
      at = 9498; l6903 = rt.array_nth(l6899, l6902);
      at = 9499; l6898 = l6903;
    } else {
      at = 9500; l6904 = K[2601];
      at = 9501; l6898 = l6904;
    }
    at = 9502; if (rt.truthy(l6888)) {
      at = 9503; l6906 = field(rt, l6891, "width");
      at = 9504; l6905 = l6906;
    } else {
      at = 9505; l6907 = K[2602];
      at = 9506; l6905 = l6907;
    }
    at = 9507; l6908 = K[2603];
    at = 9508; l6909 = rt.int_div(l6905, l6908);
    at = 9509; l6910 = K[2604];
    at = 9510; l6911 = rt.max(l6910, l6909);
    at = 9511; l6912 = K[2605];
    at = 9512; l6913 = rt.gt(l6895, l6912);
    at = 9513; if (rt.truthy(l6913)) {
      at = 9514; if (rt.truthy(l6888)) {
        at = 9515; l6916 = field(rt, l6891, "color");
        at = 9516; l6915 = l6916;
      } else {
        at = 9517; l6917 = K[2606];
        at = 9518; l6915 = l6917;
      }
      at = 9519; l6918 = K[2607];
      at = 9520; l6919 = rt.sequence_get(l6918, l6915);
      at = 9521; l6914 = l6919;
    } else {
      at = 9522; l6920 = K[2608];
      at = 9523; l6914 = l6920;
    }
    at = 9524; markSet(rt, false, "circle", {at: l6898, radius: l6911, fill: l6914});
    at = 9525; if (rt.truthy(l6888)) {
      at = 9526; l6922 = field(rt, l6891, "points");
      at = 9527; l6921 = l6922;
    } else {
      at = 9528; l6923 = K[2609];
      at = 9529; l6921 = l6923;
    }
    at = 9530; l6924 = K[2610];
    at = 9531; l6925 = rt.sub(l6895, l6924);
    at = 9532; l6926 = K[2611];
    at = 9533; l6927 = rt.max(l6926, l6925);
    at = 9534; l6928 = K[2612];
    at = 9535; l6929 = rt.array_slice(l6921, l6928, l6927);
    at = 9536; if (rt.truthy(l6888)) {
      at = 9537; l6931 = field(rt, l6891, "points");
      at = 9538; l6930 = l6931;
    } else {
      at = 9539; l6932 = K[2613];
      at = 9540; l6930 = l6932;
    }
    at = 9541; l6933 = K[2614];
    at = 9542; l6934 = rt.gt(l6895, l6933);
    at = 9543; if (rt.truthy(l6934)) {
      at = 9544; l6936 = K[2615];
      at = 9545; l6935 = l6936;
    } else {
      at = 9546; l6937 = K[2616];
      at = 9547; l6935 = l6937;
    }
    at = 9548; l6938 = K[2617];
    at = 9549; l6939 = rt.sub(l6895, l6938);
    at = 9550; l6940 = K[2618];
    at = 9551; l6941 = rt.max(l6940, l6939);
    at = 9552; l6942 = rt.array_slice(l6930, l6935, l6941);
    at = 9553; if (rt.truthy(l6888)) {
      at = 9554; l6944 = field(rt, l6891, "color");
      at = 9555; l6943 = l6944;
    } else {
      at = 9556; l6945 = K[2619];
      at = 9557; l6943 = l6945;
    }
    at = 9558; l6946 = K[2620];
    at = 9559; l6947 = rt.sequence_get(l6946, l6943);
    at = 9560; if (rt.truthy(l6888)) {
      at = 9561; l6949 = field(rt, l6891, "width");
      at = 9562; l6948 = l6949;
    } else {
      at = 9563; l6950 = K[2621];
      at = 9564; l6948 = l6950;
    }
    at = 9565; markSet(rt, true, "line", {from: l6929, to: l6942, color: l6947, width: l6948});
    at = 9566; l6951 = field(rt, l15, "strokes");
    at = 9567; l6952 = rt.sequence_length(l6951);
    at = 9568; l6953 = K[2622];
    at = 9569; l6954 = rt.lt(l6953, l6952);
    at = 9570; if (rt.truthy(l6954)) {
      at = 9571; l6955 = field(rt, l15, "strokes");
      at = 9572; l6956 = K[2623];
      at = 9573; l6957 = rt.sequence_get(l6955, l6956);
    }
    at = 9574; if (rt.truthy(l6954)) {
      at = 9575; l6959 = field(rt, l6957, "points");
      at = 9576; l6958 = l6959;
    } else {
      at = 9577; l6960 = K[2624];
      at = 9578; l6958 = l6960;
    }
    at = 9579; l6961 = rt.array_count(l6958);
    at = 9580; l6962 = K[2625];
    at = 9581; l6963 = rt.gt(l6961, l6962);
    at = 9582; if (rt.truthy(l6963)) {
      at = 9583; if (rt.truthy(l6954)) {
        at = 9584; l6966 = field(rt, l6957, "points");
        at = 9585; l6965 = l6966;
      } else {
        at = 9586; l6967 = K[2626];
        at = 9587; l6965 = l6967;
      }
      at = 9588; l6968 = K[2627];
      at = 9589; l6969 = rt.array_nth(l6965, l6968);
      at = 9590; l6964 = l6969;
    } else {
      at = 9591; l6970 = K[2628];
      at = 9592; l6964 = l6970;
    }
    at = 9593; if (rt.truthy(l6954)) {
      at = 9594; l6972 = field(rt, l6957, "width");
      at = 9595; l6971 = l6972;
    } else {
      at = 9596; l6973 = K[2629];
      at = 9597; l6971 = l6973;
    }
    at = 9598; l6974 = K[2630];
    at = 9599; l6975 = rt.int_div(l6971, l6974);
    at = 9600; l6976 = K[2631];
    at = 9601; l6977 = rt.max(l6976, l6975);
    at = 9602; l6978 = K[2632];
    at = 9603; l6979 = rt.gt(l6961, l6978);
    at = 9604; if (rt.truthy(l6979)) {
      at = 9605; if (rt.truthy(l6954)) {
        at = 9606; l6982 = field(rt, l6957, "color");
        at = 9607; l6981 = l6982;
      } else {
        at = 9608; l6983 = K[2633];
        at = 9609; l6981 = l6983;
      }
      at = 9610; l6984 = K[2634];
      at = 9611; l6985 = rt.sequence_get(l6984, l6981);
      at = 9612; l6980 = l6985;
    } else {
      at = 9613; l6986 = K[2635];
      at = 9614; l6980 = l6986;
    }
    at = 9615; markSet(rt, false, "circle", {at: l6964, radius: l6977, fill: l6980});
    at = 9616; if (rt.truthy(l6954)) {
      at = 9617; l6988 = field(rt, l6957, "points");
      at = 9618; l6987 = l6988;
    } else {
      at = 9619; l6989 = K[2636];
      at = 9620; l6987 = l6989;
    }
    at = 9621; l6990 = K[2637];
    at = 9622; l6991 = rt.sub(l6961, l6990);
    at = 9623; l6992 = K[2638];
    at = 9624; l6993 = rt.max(l6992, l6991);
    at = 9625; l6994 = K[2639];
    at = 9626; l6995 = rt.array_slice(l6987, l6994, l6993);
    at = 9627; if (rt.truthy(l6954)) {
      at = 9628; l6997 = field(rt, l6957, "points");
      at = 9629; l6996 = l6997;
    } else {
      at = 9630; l6998 = K[2640];
      at = 9631; l6996 = l6998;
    }
    at = 9632; l6999 = K[2641];
    at = 9633; l7000 = rt.gt(l6961, l6999);
    at = 9634; if (rt.truthy(l7000)) {
      at = 9635; l7002 = K[2642];
      at = 9636; l7001 = l7002;
    } else {
      at = 9637; l7003 = K[2643];
      at = 9638; l7001 = l7003;
    }
    at = 9639; l7004 = K[2644];
    at = 9640; l7005 = rt.sub(l6961, l7004);
    at = 9641; l7006 = K[2645];
    at = 9642; l7007 = rt.max(l7006, l7005);
    at = 9643; l7008 = rt.array_slice(l6996, l7001, l7007);
    at = 9644; if (rt.truthy(l6954)) {
      at = 9645; l7010 = field(rt, l6957, "color");
      at = 9646; l7009 = l7010;
    } else {
      at = 9647; l7011 = K[2646];
      at = 9648; l7009 = l7011;
    }
    at = 9649; l7012 = K[2647];
    at = 9650; l7013 = rt.sequence_get(l7012, l7009);
    at = 9651; if (rt.truthy(l6954)) {
      at = 9652; l7015 = field(rt, l6957, "width");
      at = 9653; l7014 = l7015;
    } else {
      at = 9654; l7016 = K[2648];
      at = 9655; l7014 = l7016;
    }
    at = 9656; markSet(rt, true, "line", {from: l6995, to: l7008, color: l7013, width: l7014});
    at = 9657; l7017 = field(rt, l15, "strokes");
    at = 9658; l7018 = rt.sequence_length(l7017);
    at = 9659; l7019 = K[2649];
    at = 9660; l7020 = rt.lt(l7019, l7018);
    at = 9661; if (rt.truthy(l7020)) {
      at = 9662; l7021 = field(rt, l15, "strokes");
      at = 9663; l7022 = K[2650];
      at = 9664; l7023 = rt.sequence_get(l7021, l7022);
    }
    at = 9665; if (rt.truthy(l7020)) {
      at = 9666; l7025 = field(rt, l7023, "points");
      at = 9667; l7024 = l7025;
    } else {
      at = 9668; l7026 = K[2651];
      at = 9669; l7024 = l7026;
    }
    at = 9670; l7027 = rt.array_count(l7024);
    at = 9671; l7028 = K[2652];
    at = 9672; l7029 = rt.gt(l7027, l7028);
    at = 9673; if (rt.truthy(l7029)) {
      at = 9674; if (rt.truthy(l7020)) {
        at = 9675; l7032 = field(rt, l7023, "points");
        at = 9676; l7031 = l7032;
      } else {
        at = 9677; l7033 = K[2653];
        at = 9678; l7031 = l7033;
      }
      at = 9679; l7034 = K[2654];
      at = 9680; l7035 = rt.array_nth(l7031, l7034);
      at = 9681; l7030 = l7035;
    } else {
      at = 9682; l7036 = K[2655];
      at = 9683; l7030 = l7036;
    }
    at = 9684; if (rt.truthy(l7020)) {
      at = 9685; l7038 = field(rt, l7023, "width");
      at = 9686; l7037 = l7038;
    } else {
      at = 9687; l7039 = K[2656];
      at = 9688; l7037 = l7039;
    }
    at = 9689; l7040 = K[2657];
    at = 9690; l7041 = rt.int_div(l7037, l7040);
    at = 9691; l7042 = K[2658];
    at = 9692; l7043 = rt.max(l7042, l7041);
    at = 9693; l7044 = K[2659];
    at = 9694; l7045 = rt.gt(l7027, l7044);
    at = 9695; if (rt.truthy(l7045)) {
      at = 9696; if (rt.truthy(l7020)) {
        at = 9697; l7048 = field(rt, l7023, "color");
        at = 9698; l7047 = l7048;
      } else {
        at = 9699; l7049 = K[2660];
        at = 9700; l7047 = l7049;
      }
      at = 9701; l7050 = K[2661];
      at = 9702; l7051 = rt.sequence_get(l7050, l7047);
      at = 9703; l7046 = l7051;
    } else {
      at = 9704; l7052 = K[2662];
      at = 9705; l7046 = l7052;
    }
    at = 9706; markSet(rt, false, "circle", {at: l7030, radius: l7043, fill: l7046});
    at = 9707; if (rt.truthy(l7020)) {
      at = 9708; l7054 = field(rt, l7023, "points");
      at = 9709; l7053 = l7054;
    } else {
      at = 9710; l7055 = K[2663];
      at = 9711; l7053 = l7055;
    }
    at = 9712; l7056 = K[2664];
    at = 9713; l7057 = rt.sub(l7027, l7056);
    at = 9714; l7058 = K[2665];
    at = 9715; l7059 = rt.max(l7058, l7057);
    at = 9716; l7060 = K[2666];
    at = 9717; l7061 = rt.array_slice(l7053, l7060, l7059);
    at = 9718; if (rt.truthy(l7020)) {
      at = 9719; l7063 = field(rt, l7023, "points");
      at = 9720; l7062 = l7063;
    } else {
      at = 9721; l7064 = K[2667];
      at = 9722; l7062 = l7064;
    }
    at = 9723; l7065 = K[2668];
    at = 9724; l7066 = rt.gt(l7027, l7065);
    at = 9725; if (rt.truthy(l7066)) {
      at = 9726; l7068 = K[2669];
      at = 9727; l7067 = l7068;
    } else {
      at = 9728; l7069 = K[2670];
      at = 9729; l7067 = l7069;
    }
    at = 9730; l7070 = K[2671];
    at = 9731; l7071 = rt.sub(l7027, l7070);
    at = 9732; l7072 = K[2672];
    at = 9733; l7073 = rt.max(l7072, l7071);
    at = 9734; l7074 = rt.array_slice(l7062, l7067, l7073);
    at = 9735; if (rt.truthy(l7020)) {
      at = 9736; l7076 = field(rt, l7023, "color");
      at = 9737; l7075 = l7076;
    } else {
      at = 9738; l7077 = K[2673];
      at = 9739; l7075 = l7077;
    }
    at = 9740; l7078 = K[2674];
    at = 9741; l7079 = rt.sequence_get(l7078, l7075);
    at = 9742; if (rt.truthy(l7020)) {
      at = 9743; l7081 = field(rt, l7023, "width");
      at = 9744; l7080 = l7081;
    } else {
      at = 9745; l7082 = K[2675];
      at = 9746; l7080 = l7082;
    }
    at = 9747; markSet(rt, true, "line", {from: l7061, to: l7074, color: l7079, width: l7080});
    at = 9748; l7083 = field(rt, l15, "strokes");
    at = 9749; l7084 = rt.sequence_length(l7083);
    at = 9750; l7085 = K[2676];
    at = 9751; l7086 = rt.lt(l7085, l7084);
    at = 9752; if (rt.truthy(l7086)) {
      at = 9753; l7087 = field(rt, l15, "strokes");
      at = 9754; l7088 = K[2677];
      at = 9755; l7089 = rt.sequence_get(l7087, l7088);
    }
    at = 9756; if (rt.truthy(l7086)) {
      at = 9757; l7091 = field(rt, l7089, "points");
      at = 9758; l7090 = l7091;
    } else {
      at = 9759; l7092 = K[2678];
      at = 9760; l7090 = l7092;
    }
    at = 9761; l7093 = rt.array_count(l7090);
    at = 9762; l7094 = K[2679];
    at = 9763; l7095 = rt.gt(l7093, l7094);
    at = 9764; if (rt.truthy(l7095)) {
      at = 9765; if (rt.truthy(l7086)) {
        at = 9766; l7098 = field(rt, l7089, "points");
        at = 9767; l7097 = l7098;
      } else {
        at = 9768; l7099 = K[2680];
        at = 9769; l7097 = l7099;
      }
      at = 9770; l7100 = K[2681];
      at = 9771; l7101 = rt.array_nth(l7097, l7100);
      at = 9772; l7096 = l7101;
    } else {
      at = 9773; l7102 = K[2682];
      at = 9774; l7096 = l7102;
    }
    at = 9775; if (rt.truthy(l7086)) {
      at = 9776; l7104 = field(rt, l7089, "width");
      at = 9777; l7103 = l7104;
    } else {
      at = 9778; l7105 = K[2683];
      at = 9779; l7103 = l7105;
    }
    at = 9780; l7106 = K[2684];
    at = 9781; l7107 = rt.int_div(l7103, l7106);
    at = 9782; l7108 = K[2685];
    at = 9783; l7109 = rt.max(l7108, l7107);
    at = 9784; l7110 = K[2686];
    at = 9785; l7111 = rt.gt(l7093, l7110);
    at = 9786; if (rt.truthy(l7111)) {
      at = 9787; if (rt.truthy(l7086)) {
        at = 9788; l7114 = field(rt, l7089, "color");
        at = 9789; l7113 = l7114;
      } else {
        at = 9790; l7115 = K[2687];
        at = 9791; l7113 = l7115;
      }
      at = 9792; l7116 = K[2688];
      at = 9793; l7117 = rt.sequence_get(l7116, l7113);
      at = 9794; l7112 = l7117;
    } else {
      at = 9795; l7118 = K[2689];
      at = 9796; l7112 = l7118;
    }
    at = 9797; markSet(rt, false, "circle", {at: l7096, radius: l7109, fill: l7112});
    at = 9798; if (rt.truthy(l7086)) {
      at = 9799; l7120 = field(rt, l7089, "points");
      at = 9800; l7119 = l7120;
    } else {
      at = 9801; l7121 = K[2690];
      at = 9802; l7119 = l7121;
    }
    at = 9803; l7122 = K[2691];
    at = 9804; l7123 = rt.sub(l7093, l7122);
    at = 9805; l7124 = K[2692];
    at = 9806; l7125 = rt.max(l7124, l7123);
    at = 9807; l7126 = K[2693];
    at = 9808; l7127 = rt.array_slice(l7119, l7126, l7125);
    at = 9809; if (rt.truthy(l7086)) {
      at = 9810; l7129 = field(rt, l7089, "points");
      at = 9811; l7128 = l7129;
    } else {
      at = 9812; l7130 = K[2694];
      at = 9813; l7128 = l7130;
    }
    at = 9814; l7131 = K[2695];
    at = 9815; l7132 = rt.gt(l7093, l7131);
    at = 9816; if (rt.truthy(l7132)) {
      at = 9817; l7134 = K[2696];
      at = 9818; l7133 = l7134;
    } else {
      at = 9819; l7135 = K[2697];
      at = 9820; l7133 = l7135;
    }
    at = 9821; l7136 = K[2698];
    at = 9822; l7137 = rt.sub(l7093, l7136);
    at = 9823; l7138 = K[2699];
    at = 9824; l7139 = rt.max(l7138, l7137);
    at = 9825; l7140 = rt.array_slice(l7128, l7133, l7139);
    at = 9826; if (rt.truthy(l7086)) {
      at = 9827; l7142 = field(rt, l7089, "color");
      at = 9828; l7141 = l7142;
    } else {
      at = 9829; l7143 = K[2700];
      at = 9830; l7141 = l7143;
    }
    at = 9831; l7144 = K[2701];
    at = 9832; l7145 = rt.sequence_get(l7144, l7141);
    at = 9833; if (rt.truthy(l7086)) {
      at = 9834; l7147 = field(rt, l7089, "width");
      at = 9835; l7146 = l7147;
    } else {
      at = 9836; l7148 = K[2702];
      at = 9837; l7146 = l7148;
    }
    at = 9838; markSet(rt, true, "line", {from: l7127, to: l7140, color: l7145, width: l7146});
    at = 9839; l7149 = field(rt, l15, "strokes");
    at = 9840; l7150 = rt.sequence_length(l7149);
    at = 9841; l7151 = K[2703];
    at = 9842; l7152 = rt.lt(l7151, l7150);
    at = 9843; if (rt.truthy(l7152)) {
      at = 9844; l7153 = field(rt, l15, "strokes");
      at = 9845; l7154 = K[2704];
      at = 9846; l7155 = rt.sequence_get(l7153, l7154);
    }
    at = 9847; if (rt.truthy(l7152)) {
      at = 9848; l7157 = field(rt, l7155, "points");
      at = 9849; l7156 = l7157;
    } else {
      at = 9850; l7158 = K[2705];
      at = 9851; l7156 = l7158;
    }
    at = 9852; l7159 = rt.array_count(l7156);
    at = 9853; l7160 = K[2706];
    at = 9854; l7161 = rt.gt(l7159, l7160);
    at = 9855; if (rt.truthy(l7161)) {
      at = 9856; if (rt.truthy(l7152)) {
        at = 9857; l7164 = field(rt, l7155, "points");
        at = 9858; l7163 = l7164;
      } else {
        at = 9859; l7165 = K[2707];
        at = 9860; l7163 = l7165;
      }
      at = 9861; l7166 = K[2708];
      at = 9862; l7167 = rt.array_nth(l7163, l7166);
      at = 9863; l7162 = l7167;
    } else {
      at = 9864; l7168 = K[2709];
      at = 9865; l7162 = l7168;
    }
    at = 9866; if (rt.truthy(l7152)) {
      at = 9867; l7170 = field(rt, l7155, "width");
      at = 9868; l7169 = l7170;
    } else {
      at = 9869; l7171 = K[2710];
      at = 9870; l7169 = l7171;
    }
    at = 9871; l7172 = K[2711];
    at = 9872; l7173 = rt.int_div(l7169, l7172);
    at = 9873; l7174 = K[2712];
    at = 9874; l7175 = rt.max(l7174, l7173);
    at = 9875; l7176 = K[2713];
    at = 9876; l7177 = rt.gt(l7159, l7176);
    at = 9877; if (rt.truthy(l7177)) {
      at = 9878; if (rt.truthy(l7152)) {
        at = 9879; l7180 = field(rt, l7155, "color");
        at = 9880; l7179 = l7180;
      } else {
        at = 9881; l7181 = K[2714];
        at = 9882; l7179 = l7181;
      }
      at = 9883; l7182 = K[2715];
      at = 9884; l7183 = rt.sequence_get(l7182, l7179);
      at = 9885; l7178 = l7183;
    } else {
      at = 9886; l7184 = K[2716];
      at = 9887; l7178 = l7184;
    }
    at = 9888; markSet(rt, false, "circle", {at: l7162, radius: l7175, fill: l7178});
    at = 9889; if (rt.truthy(l7152)) {
      at = 9890; l7186 = field(rt, l7155, "points");
      at = 9891; l7185 = l7186;
    } else {
      at = 9892; l7187 = K[2717];
      at = 9893; l7185 = l7187;
    }
    at = 9894; l7188 = K[2718];
    at = 9895; l7189 = rt.sub(l7159, l7188);
    at = 9896; l7190 = K[2719];
    at = 9897; l7191 = rt.max(l7190, l7189);
    at = 9898; l7192 = K[2720];
    at = 9899; l7193 = rt.array_slice(l7185, l7192, l7191);
    at = 9900; if (rt.truthy(l7152)) {
      at = 9901; l7195 = field(rt, l7155, "points");
      at = 9902; l7194 = l7195;
    } else {
      at = 9903; l7196 = K[2721];
      at = 9904; l7194 = l7196;
    }
    at = 9905; l7197 = K[2722];
    at = 9906; l7198 = rt.gt(l7159, l7197);
    at = 9907; if (rt.truthy(l7198)) {
      at = 9908; l7200 = K[2723];
      at = 9909; l7199 = l7200;
    } else {
      at = 9910; l7201 = K[2724];
      at = 9911; l7199 = l7201;
    }
    at = 9912; l7202 = K[2725];
    at = 9913; l7203 = rt.sub(l7159, l7202);
    at = 9914; l7204 = K[2726];
    at = 9915; l7205 = rt.max(l7204, l7203);
    at = 9916; l7206 = rt.array_slice(l7194, l7199, l7205);
    at = 9917; if (rt.truthy(l7152)) {
      at = 9918; l7208 = field(rt, l7155, "color");
      at = 9919; l7207 = l7208;
    } else {
      at = 9920; l7209 = K[2727];
      at = 9921; l7207 = l7209;
    }
    at = 9922; l7210 = K[2728];
    at = 9923; l7211 = rt.sequence_get(l7210, l7207);
    at = 9924; if (rt.truthy(l7152)) {
      at = 9925; l7213 = field(rt, l7155, "width");
      at = 9926; l7212 = l7213;
    } else {
      at = 9927; l7214 = K[2729];
      at = 9928; l7212 = l7214;
    }
    at = 9929; markSet(rt, true, "line", {from: l7193, to: l7206, color: l7211, width: l7212});
    at = 9930; l7215 = field(rt, l15, "strokes");
    at = 9931; l7216 = rt.sequence_length(l7215);
    at = 9932; l7217 = K[2730];
    at = 9933; l7218 = rt.lt(l7217, l7216);
    at = 9934; if (rt.truthy(l7218)) {
      at = 9935; l7219 = field(rt, l15, "strokes");
      at = 9936; l7220 = K[2731];
      at = 9937; l7221 = rt.sequence_get(l7219, l7220);
    }
    at = 9938; if (rt.truthy(l7218)) {
      at = 9939; l7223 = field(rt, l7221, "points");
      at = 9940; l7222 = l7223;
    } else {
      at = 9941; l7224 = K[2732];
      at = 9942; l7222 = l7224;
    }
    at = 9943; l7225 = rt.array_count(l7222);
    at = 9944; l7226 = K[2733];
    at = 9945; l7227 = rt.gt(l7225, l7226);
    at = 9946; if (rt.truthy(l7227)) {
      at = 9947; if (rt.truthy(l7218)) {
        at = 9948; l7230 = field(rt, l7221, "points");
        at = 9949; l7229 = l7230;
      } else {
        at = 9950; l7231 = K[2734];
        at = 9951; l7229 = l7231;
      }
      at = 9952; l7232 = K[2735];
      at = 9953; l7233 = rt.array_nth(l7229, l7232);
      at = 9954; l7228 = l7233;
    } else {
      at = 9955; l7234 = K[2736];
      at = 9956; l7228 = l7234;
    }
    at = 9957; if (rt.truthy(l7218)) {
      at = 9958; l7236 = field(rt, l7221, "width");
      at = 9959; l7235 = l7236;
    } else {
      at = 9960; l7237 = K[2737];
      at = 9961; l7235 = l7237;
    }
    at = 9962; l7238 = K[2738];
    at = 9963; l7239 = rt.int_div(l7235, l7238);
    at = 9964; l7240 = K[2739];
    at = 9965; l7241 = rt.max(l7240, l7239);
    at = 9966; l7242 = K[2740];
    at = 9967; l7243 = rt.gt(l7225, l7242);
    at = 9968; if (rt.truthy(l7243)) {
      at = 9969; if (rt.truthy(l7218)) {
        at = 9970; l7246 = field(rt, l7221, "color");
        at = 9971; l7245 = l7246;
      } else {
        at = 9972; l7247 = K[2741];
        at = 9973; l7245 = l7247;
      }
      at = 9974; l7248 = K[2742];
      at = 9975; l7249 = rt.sequence_get(l7248, l7245);
      at = 9976; l7244 = l7249;
    } else {
      at = 9977; l7250 = K[2743];
      at = 9978; l7244 = l7250;
    }
    at = 9979; markSet(rt, false, "circle", {at: l7228, radius: l7241, fill: l7244});
    at = 9980; if (rt.truthy(l7218)) {
      at = 9981; l7252 = field(rt, l7221, "points");
      at = 9982; l7251 = l7252;
    } else {
      at = 9983; l7253 = K[2744];
      at = 9984; l7251 = l7253;
    }
    at = 9985; l7254 = K[2745];
    at = 9986; l7255 = rt.sub(l7225, l7254);
    at = 9987; l7256 = K[2746];
    at = 9988; l7257 = rt.max(l7256, l7255);
    at = 9989; l7258 = K[2747];
    at = 9990; l7259 = rt.array_slice(l7251, l7258, l7257);
    at = 9991; if (rt.truthy(l7218)) {
      at = 9992; l7261 = field(rt, l7221, "points");
      at = 9993; l7260 = l7261;
    } else {
      at = 9994; l7262 = K[2748];
      at = 9995; l7260 = l7262;
    }
    at = 9996; l7263 = K[2749];
    at = 9997; l7264 = rt.gt(l7225, l7263);
    at = 9998; if (rt.truthy(l7264)) {
      at = 9999; l7266 = K[2750];
      at = 10000; l7265 = l7266;
    } else {
      at = 10001; l7267 = K[2751];
      at = 10002; l7265 = l7267;
    }
    at = 10003; l7268 = K[2752];
    at = 10004; l7269 = rt.sub(l7225, l7268);
    at = 10005; l7270 = K[2753];
    at = 10006; l7271 = rt.max(l7270, l7269);
    at = 10007; l7272 = rt.array_slice(l7260, l7265, l7271);
    at = 10008; if (rt.truthy(l7218)) {
      at = 10009; l7274 = field(rt, l7221, "color");
      at = 10010; l7273 = l7274;
    } else {
      at = 10011; l7275 = K[2754];
      at = 10012; l7273 = l7275;
    }
    at = 10013; l7276 = K[2755];
    at = 10014; l7277 = rt.sequence_get(l7276, l7273);
    at = 10015; if (rt.truthy(l7218)) {
      at = 10016; l7279 = field(rt, l7221, "width");
      at = 10017; l7278 = l7279;
    } else {
      at = 10018; l7280 = K[2756];
      at = 10019; l7278 = l7280;
    }
    at = 10020; markSet(rt, true, "line", {from: l7259, to: l7272, color: l7277, width: l7278});
    at = 10021; l7281 = field(rt, l15, "strokes");
    at = 10022; l7282 = rt.sequence_length(l7281);
    at = 10023; l7283 = K[2757];
    at = 10024; l7284 = rt.lt(l7283, l7282);
    at = 10025; if (rt.truthy(l7284)) {
      at = 10026; l7285 = field(rt, l15, "strokes");
      at = 10027; l7286 = K[2758];
      at = 10028; l7287 = rt.sequence_get(l7285, l7286);
    }
    at = 10029; if (rt.truthy(l7284)) {
      at = 10030; l7289 = field(rt, l7287, "points");
      at = 10031; l7288 = l7289;
    } else {
      at = 10032; l7290 = K[2759];
      at = 10033; l7288 = l7290;
    }
    at = 10034; l7291 = rt.array_count(l7288);
    at = 10035; l7292 = K[2760];
    at = 10036; l7293 = rt.gt(l7291, l7292);
    at = 10037; if (rt.truthy(l7293)) {
      at = 10038; if (rt.truthy(l7284)) {
        at = 10039; l7296 = field(rt, l7287, "points");
        at = 10040; l7295 = l7296;
      } else {
        at = 10041; l7297 = K[2761];
        at = 10042; l7295 = l7297;
      }
      at = 10043; l7298 = K[2762];
      at = 10044; l7299 = rt.array_nth(l7295, l7298);
      at = 10045; l7294 = l7299;
    } else {
      at = 10046; l7300 = K[2763];
      at = 10047; l7294 = l7300;
    }
    at = 10048; if (rt.truthy(l7284)) {
      at = 10049; l7302 = field(rt, l7287, "width");
      at = 10050; l7301 = l7302;
    } else {
      at = 10051; l7303 = K[2764];
      at = 10052; l7301 = l7303;
    }
    at = 10053; l7304 = K[2765];
    at = 10054; l7305 = rt.int_div(l7301, l7304);
    at = 10055; l7306 = K[2766];
    at = 10056; l7307 = rt.max(l7306, l7305);
    at = 10057; l7308 = K[2767];
    at = 10058; l7309 = rt.gt(l7291, l7308);
    at = 10059; if (rt.truthy(l7309)) {
      at = 10060; if (rt.truthy(l7284)) {
        at = 10061; l7312 = field(rt, l7287, "color");
        at = 10062; l7311 = l7312;
      } else {
        at = 10063; l7313 = K[2768];
        at = 10064; l7311 = l7313;
      }
      at = 10065; l7314 = K[2769];
      at = 10066; l7315 = rt.sequence_get(l7314, l7311);
      at = 10067; l7310 = l7315;
    } else {
      at = 10068; l7316 = K[2770];
      at = 10069; l7310 = l7316;
    }
    at = 10070; markSet(rt, false, "circle", {at: l7294, radius: l7307, fill: l7310});
    at = 10071; if (rt.truthy(l7284)) {
      at = 10072; l7318 = field(rt, l7287, "points");
      at = 10073; l7317 = l7318;
    } else {
      at = 10074; l7319 = K[2771];
      at = 10075; l7317 = l7319;
    }
    at = 10076; l7320 = K[2772];
    at = 10077; l7321 = rt.sub(l7291, l7320);
    at = 10078; l7322 = K[2773];
    at = 10079; l7323 = rt.max(l7322, l7321);
    at = 10080; l7324 = K[2774];
    at = 10081; l7325 = rt.array_slice(l7317, l7324, l7323);
    at = 10082; if (rt.truthy(l7284)) {
      at = 10083; l7327 = field(rt, l7287, "points");
      at = 10084; l7326 = l7327;
    } else {
      at = 10085; l7328 = K[2775];
      at = 10086; l7326 = l7328;
    }
    at = 10087; l7329 = K[2776];
    at = 10088; l7330 = rt.gt(l7291, l7329);
    at = 10089; if (rt.truthy(l7330)) {
      at = 10090; l7332 = K[2777];
      at = 10091; l7331 = l7332;
    } else {
      at = 10092; l7333 = K[2778];
      at = 10093; l7331 = l7333;
    }
    at = 10094; l7334 = K[2779];
    at = 10095; l7335 = rt.sub(l7291, l7334);
    at = 10096; l7336 = K[2780];
    at = 10097; l7337 = rt.max(l7336, l7335);
    at = 10098; l7338 = rt.array_slice(l7326, l7331, l7337);
    at = 10099; if (rt.truthy(l7284)) {
      at = 10100; l7340 = field(rt, l7287, "color");
      at = 10101; l7339 = l7340;
    } else {
      at = 10102; l7341 = K[2781];
      at = 10103; l7339 = l7341;
    }
    at = 10104; l7342 = K[2782];
    at = 10105; l7343 = rt.sequence_get(l7342, l7339);
    at = 10106; if (rt.truthy(l7284)) {
      at = 10107; l7345 = field(rt, l7287, "width");
      at = 10108; l7344 = l7345;
    } else {
      at = 10109; l7346 = K[2783];
      at = 10110; l7344 = l7346;
    }
    at = 10111; markSet(rt, true, "line", {from: l7325, to: l7338, color: l7343, width: l7344});
    at = 10112; l7347 = field(rt, l15, "strokes");
    at = 10113; l7348 = rt.sequence_length(l7347);
    at = 10114; l7349 = K[2784];
    at = 10115; l7350 = rt.lt(l7349, l7348);
    at = 10116; if (rt.truthy(l7350)) {
      at = 10117; l7351 = field(rt, l15, "strokes");
      at = 10118; l7352 = K[2785];
      at = 10119; l7353 = rt.sequence_get(l7351, l7352);
    }
    at = 10120; if (rt.truthy(l7350)) {
      at = 10121; l7355 = field(rt, l7353, "points");
      at = 10122; l7354 = l7355;
    } else {
      at = 10123; l7356 = K[2786];
      at = 10124; l7354 = l7356;
    }
    at = 10125; l7357 = rt.array_count(l7354);
    at = 10126; l7358 = K[2787];
    at = 10127; l7359 = rt.gt(l7357, l7358);
    at = 10128; if (rt.truthy(l7359)) {
      at = 10129; if (rt.truthy(l7350)) {
        at = 10130; l7362 = field(rt, l7353, "points");
        at = 10131; l7361 = l7362;
      } else {
        at = 10132; l7363 = K[2788];
        at = 10133; l7361 = l7363;
      }
      at = 10134; l7364 = K[2789];
      at = 10135; l7365 = rt.array_nth(l7361, l7364);
      at = 10136; l7360 = l7365;
    } else {
      at = 10137; l7366 = K[2790];
      at = 10138; l7360 = l7366;
    }
    at = 10139; if (rt.truthy(l7350)) {
      at = 10140; l7368 = field(rt, l7353, "width");
      at = 10141; l7367 = l7368;
    } else {
      at = 10142; l7369 = K[2791];
      at = 10143; l7367 = l7369;
    }
    at = 10144; l7370 = K[2792];
    at = 10145; l7371 = rt.int_div(l7367, l7370);
    at = 10146; l7372 = K[2793];
    at = 10147; l7373 = rt.max(l7372, l7371);
    at = 10148; l7374 = K[2794];
    at = 10149; l7375 = rt.gt(l7357, l7374);
    at = 10150; if (rt.truthy(l7375)) {
      at = 10151; if (rt.truthy(l7350)) {
        at = 10152; l7378 = field(rt, l7353, "color");
        at = 10153; l7377 = l7378;
      } else {
        at = 10154; l7379 = K[2795];
        at = 10155; l7377 = l7379;
      }
      at = 10156; l7380 = K[2796];
      at = 10157; l7381 = rt.sequence_get(l7380, l7377);
      at = 10158; l7376 = l7381;
    } else {
      at = 10159; l7382 = K[2797];
      at = 10160; l7376 = l7382;
    }
    at = 10161; markSet(rt, false, "circle", {at: l7360, radius: l7373, fill: l7376});
    at = 10162; if (rt.truthy(l7350)) {
      at = 10163; l7384 = field(rt, l7353, "points");
      at = 10164; l7383 = l7384;
    } else {
      at = 10165; l7385 = K[2798];
      at = 10166; l7383 = l7385;
    }
    at = 10167; l7386 = K[2799];
    at = 10168; l7387 = rt.sub(l7357, l7386);
    at = 10169; l7388 = K[2800];
    at = 10170; l7389 = rt.max(l7388, l7387);
    at = 10171; l7390 = K[2801];
    at = 10172; l7391 = rt.array_slice(l7383, l7390, l7389);
    at = 10173; if (rt.truthy(l7350)) {
      at = 10174; l7393 = field(rt, l7353, "points");
      at = 10175; l7392 = l7393;
    } else {
      at = 10176; l7394 = K[2802];
      at = 10177; l7392 = l7394;
    }
    at = 10178; l7395 = K[2803];
    at = 10179; l7396 = rt.gt(l7357, l7395);
    at = 10180; if (rt.truthy(l7396)) {
      at = 10181; l7398 = K[2804];
      at = 10182; l7397 = l7398;
    } else {
      at = 10183; l7399 = K[2805];
      at = 10184; l7397 = l7399;
    }
    at = 10185; l7400 = K[2806];
    at = 10186; l7401 = rt.sub(l7357, l7400);
    at = 10187; l7402 = K[2807];
    at = 10188; l7403 = rt.max(l7402, l7401);
    at = 10189; l7404 = rt.array_slice(l7392, l7397, l7403);
    at = 10190; if (rt.truthy(l7350)) {
      at = 10191; l7406 = field(rt, l7353, "color");
      at = 10192; l7405 = l7406;
    } else {
      at = 10193; l7407 = K[2808];
      at = 10194; l7405 = l7407;
    }
    at = 10195; l7408 = K[2809];
    at = 10196; l7409 = rt.sequence_get(l7408, l7405);
    at = 10197; if (rt.truthy(l7350)) {
      at = 10198; l7411 = field(rt, l7353, "width");
      at = 10199; l7410 = l7411;
    } else {
      at = 10200; l7412 = K[2810];
      at = 10201; l7410 = l7412;
    }
    at = 10202; markSet(rt, true, "line", {from: l7391, to: l7404, color: l7409, width: l7410});
    at = 10203; l7413 = field(rt, l15, "strokes");
    at = 10204; l7414 = rt.sequence_length(l7413);
    at = 10205; l7415 = K[2811];
    at = 10206; l7416 = rt.lt(l7415, l7414);
    at = 10207; if (rt.truthy(l7416)) {
      at = 10208; l7417 = field(rt, l15, "strokes");
      at = 10209; l7418 = K[2812];
      at = 10210; l7419 = rt.sequence_get(l7417, l7418);
    }
    at = 10211; if (rt.truthy(l7416)) {
      at = 10212; l7421 = field(rt, l7419, "points");
      at = 10213; l7420 = l7421;
    } else {
      at = 10214; l7422 = K[2813];
      at = 10215; l7420 = l7422;
    }
    at = 10216; l7423 = rt.array_count(l7420);
    at = 10217; l7424 = K[2814];
    at = 10218; l7425 = rt.gt(l7423, l7424);
    at = 10219; if (rt.truthy(l7425)) {
      at = 10220; if (rt.truthy(l7416)) {
        at = 10221; l7428 = field(rt, l7419, "points");
        at = 10222; l7427 = l7428;
      } else {
        at = 10223; l7429 = K[2815];
        at = 10224; l7427 = l7429;
      }
      at = 10225; l7430 = K[2816];
      at = 10226; l7431 = rt.array_nth(l7427, l7430);
      at = 10227; l7426 = l7431;
    } else {
      at = 10228; l7432 = K[2817];
      at = 10229; l7426 = l7432;
    }
    at = 10230; if (rt.truthy(l7416)) {
      at = 10231; l7434 = field(rt, l7419, "width");
      at = 10232; l7433 = l7434;
    } else {
      at = 10233; l7435 = K[2818];
      at = 10234; l7433 = l7435;
    }
    at = 10235; l7436 = K[2819];
    at = 10236; l7437 = rt.int_div(l7433, l7436);
    at = 10237; l7438 = K[2820];
    at = 10238; l7439 = rt.max(l7438, l7437);
    at = 10239; l7440 = K[2821];
    at = 10240; l7441 = rt.gt(l7423, l7440);
    at = 10241; if (rt.truthy(l7441)) {
      at = 10242; if (rt.truthy(l7416)) {
        at = 10243; l7444 = field(rt, l7419, "color");
        at = 10244; l7443 = l7444;
      } else {
        at = 10245; l7445 = K[2822];
        at = 10246; l7443 = l7445;
      }
      at = 10247; l7446 = K[2823];
      at = 10248; l7447 = rt.sequence_get(l7446, l7443);
      at = 10249; l7442 = l7447;
    } else {
      at = 10250; l7448 = K[2824];
      at = 10251; l7442 = l7448;
    }
    at = 10252; markSet(rt, false, "circle", {at: l7426, radius: l7439, fill: l7442});
    at = 10253; if (rt.truthy(l7416)) {
      at = 10254; l7450 = field(rt, l7419, "points");
      at = 10255; l7449 = l7450;
    } else {
      at = 10256; l7451 = K[2825];
      at = 10257; l7449 = l7451;
    }
    at = 10258; l7452 = K[2826];
    at = 10259; l7453 = rt.sub(l7423, l7452);
    at = 10260; l7454 = K[2827];
    at = 10261; l7455 = rt.max(l7454, l7453);
    at = 10262; l7456 = K[2828];
    at = 10263; l7457 = rt.array_slice(l7449, l7456, l7455);
    at = 10264; if (rt.truthy(l7416)) {
      at = 10265; l7459 = field(rt, l7419, "points");
      at = 10266; l7458 = l7459;
    } else {
      at = 10267; l7460 = K[2829];
      at = 10268; l7458 = l7460;
    }
    at = 10269; l7461 = K[2830];
    at = 10270; l7462 = rt.gt(l7423, l7461);
    at = 10271; if (rt.truthy(l7462)) {
      at = 10272; l7464 = K[2831];
      at = 10273; l7463 = l7464;
    } else {
      at = 10274; l7465 = K[2832];
      at = 10275; l7463 = l7465;
    }
    at = 10276; l7466 = K[2833];
    at = 10277; l7467 = rt.sub(l7423, l7466);
    at = 10278; l7468 = K[2834];
    at = 10279; l7469 = rt.max(l7468, l7467);
    at = 10280; l7470 = rt.array_slice(l7458, l7463, l7469);
    at = 10281; if (rt.truthy(l7416)) {
      at = 10282; l7472 = field(rt, l7419, "color");
      at = 10283; l7471 = l7472;
    } else {
      at = 10284; l7473 = K[2835];
      at = 10285; l7471 = l7473;
    }
    at = 10286; l7474 = K[2836];
    at = 10287; l7475 = rt.sequence_get(l7474, l7471);
    at = 10288; if (rt.truthy(l7416)) {
      at = 10289; l7477 = field(rt, l7419, "width");
      at = 10290; l7476 = l7477;
    } else {
      at = 10291; l7478 = K[2837];
      at = 10292; l7476 = l7478;
    }
    at = 10293; markSet(rt, true, "line", {from: l7457, to: l7470, color: l7475, width: l7476});
    at = 10294; l7479 = field(rt, l15, "strokes");
    at = 10295; l7480 = rt.sequence_length(l7479);
    at = 10296; l7481 = K[2838];
    at = 10297; l7482 = rt.lt(l7481, l7480);
    at = 10298; if (rt.truthy(l7482)) {
      at = 10299; l7483 = field(rt, l15, "strokes");
      at = 10300; l7484 = K[2839];
      at = 10301; l7485 = rt.sequence_get(l7483, l7484);
    }
    at = 10302; if (rt.truthy(l7482)) {
      at = 10303; l7487 = field(rt, l7485, "points");
      at = 10304; l7486 = l7487;
    } else {
      at = 10305; l7488 = K[2840];
      at = 10306; l7486 = l7488;
    }
    at = 10307; l7489 = rt.array_count(l7486);
    at = 10308; l7490 = K[2841];
    at = 10309; l7491 = rt.gt(l7489, l7490);
    at = 10310; if (rt.truthy(l7491)) {
      at = 10311; if (rt.truthy(l7482)) {
        at = 10312; l7494 = field(rt, l7485, "points");
        at = 10313; l7493 = l7494;
      } else {
        at = 10314; l7495 = K[2842];
        at = 10315; l7493 = l7495;
      }
      at = 10316; l7496 = K[2843];
      at = 10317; l7497 = rt.array_nth(l7493, l7496);
      at = 10318; l7492 = l7497;
    } else {
      at = 10319; l7498 = K[2844];
      at = 10320; l7492 = l7498;
    }
    at = 10321; if (rt.truthy(l7482)) {
      at = 10322; l7500 = field(rt, l7485, "width");
      at = 10323; l7499 = l7500;
    } else {
      at = 10324; l7501 = K[2845];
      at = 10325; l7499 = l7501;
    }
    at = 10326; l7502 = K[2846];
    at = 10327; l7503 = rt.int_div(l7499, l7502);
    at = 10328; l7504 = K[2847];
    at = 10329; l7505 = rt.max(l7504, l7503);
    at = 10330; l7506 = K[2848];
    at = 10331; l7507 = rt.gt(l7489, l7506);
    at = 10332; if (rt.truthy(l7507)) {
      at = 10333; if (rt.truthy(l7482)) {
        at = 10334; l7510 = field(rt, l7485, "color");
        at = 10335; l7509 = l7510;
      } else {
        at = 10336; l7511 = K[2849];
        at = 10337; l7509 = l7511;
      }
      at = 10338; l7512 = K[2850];
      at = 10339; l7513 = rt.sequence_get(l7512, l7509);
      at = 10340; l7508 = l7513;
    } else {
      at = 10341; l7514 = K[2851];
      at = 10342; l7508 = l7514;
    }
    at = 10343; markSet(rt, false, "circle", {at: l7492, radius: l7505, fill: l7508});
    at = 10344; if (rt.truthy(l7482)) {
      at = 10345; l7516 = field(rt, l7485, "points");
      at = 10346; l7515 = l7516;
    } else {
      at = 10347; l7517 = K[2852];
      at = 10348; l7515 = l7517;
    }
    at = 10349; l7518 = K[2853];
    at = 10350; l7519 = rt.sub(l7489, l7518);
    at = 10351; l7520 = K[2854];
    at = 10352; l7521 = rt.max(l7520, l7519);
    at = 10353; l7522 = K[2855];
    at = 10354; l7523 = rt.array_slice(l7515, l7522, l7521);
    at = 10355; if (rt.truthy(l7482)) {
      at = 10356; l7525 = field(rt, l7485, "points");
      at = 10357; l7524 = l7525;
    } else {
      at = 10358; l7526 = K[2856];
      at = 10359; l7524 = l7526;
    }
    at = 10360; l7527 = K[2857];
    at = 10361; l7528 = rt.gt(l7489, l7527);
    at = 10362; if (rt.truthy(l7528)) {
      at = 10363; l7530 = K[2858];
      at = 10364; l7529 = l7530;
    } else {
      at = 10365; l7531 = K[2859];
      at = 10366; l7529 = l7531;
    }
    at = 10367; l7532 = K[2860];
    at = 10368; l7533 = rt.sub(l7489, l7532);
    at = 10369; l7534 = K[2861];
    at = 10370; l7535 = rt.max(l7534, l7533);
    at = 10371; l7536 = rt.array_slice(l7524, l7529, l7535);
    at = 10372; if (rt.truthy(l7482)) {
      at = 10373; l7538 = field(rt, l7485, "color");
      at = 10374; l7537 = l7538;
    } else {
      at = 10375; l7539 = K[2862];
      at = 10376; l7537 = l7539;
    }
    at = 10377; l7540 = K[2863];
    at = 10378; l7541 = rt.sequence_get(l7540, l7537);
    at = 10379; if (rt.truthy(l7482)) {
      at = 10380; l7543 = field(rt, l7485, "width");
      at = 10381; l7542 = l7543;
    } else {
      at = 10382; l7544 = K[2864];
      at = 10383; l7542 = l7544;
    }
    at = 10384; markSet(rt, true, "line", {from: l7523, to: l7536, color: l7541, width: l7542});
    at = 10385; l7545 = field(rt, l15, "strokes");
    at = 10386; l7546 = rt.sequence_length(l7545);
    at = 10387; l7547 = K[2865];
    at = 10388; l7548 = rt.lt(l7547, l7546);
    at = 10389; if (rt.truthy(l7548)) {
      at = 10390; l7549 = field(rt, l15, "strokes");
      at = 10391; l7550 = K[2866];
      at = 10392; l7551 = rt.sequence_get(l7549, l7550);
    }
    at = 10393; if (rt.truthy(l7548)) {
      at = 10394; l7553 = field(rt, l7551, "points");
      at = 10395; l7552 = l7553;
    } else {
      at = 10396; l7554 = K[2867];
      at = 10397; l7552 = l7554;
    }
    at = 10398; l7555 = rt.array_count(l7552);
    at = 10399; l7556 = K[2868];
    at = 10400; l7557 = rt.gt(l7555, l7556);
    at = 10401; if (rt.truthy(l7557)) {
      at = 10402; if (rt.truthy(l7548)) {
        at = 10403; l7560 = field(rt, l7551, "points");
        at = 10404; l7559 = l7560;
      } else {
        at = 10405; l7561 = K[2869];
        at = 10406; l7559 = l7561;
      }
      at = 10407; l7562 = K[2870];
      at = 10408; l7563 = rt.array_nth(l7559, l7562);
      at = 10409; l7558 = l7563;
    } else {
      at = 10410; l7564 = K[2871];
      at = 10411; l7558 = l7564;
    }
    at = 10412; if (rt.truthy(l7548)) {
      at = 10413; l7566 = field(rt, l7551, "width");
      at = 10414; l7565 = l7566;
    } else {
      at = 10415; l7567 = K[2872];
      at = 10416; l7565 = l7567;
    }
    at = 10417; l7568 = K[2873];
    at = 10418; l7569 = rt.int_div(l7565, l7568);
    at = 10419; l7570 = K[2874];
    at = 10420; l7571 = rt.max(l7570, l7569);
    at = 10421; l7572 = K[2875];
    at = 10422; l7573 = rt.gt(l7555, l7572);
    at = 10423; if (rt.truthy(l7573)) {
      at = 10424; if (rt.truthy(l7548)) {
        at = 10425; l7576 = field(rt, l7551, "color");
        at = 10426; l7575 = l7576;
      } else {
        at = 10427; l7577 = K[2876];
        at = 10428; l7575 = l7577;
      }
      at = 10429; l7578 = K[2877];
      at = 10430; l7579 = rt.sequence_get(l7578, l7575);
      at = 10431; l7574 = l7579;
    } else {
      at = 10432; l7580 = K[2878];
      at = 10433; l7574 = l7580;
    }
    at = 10434; markSet(rt, false, "circle", {at: l7558, radius: l7571, fill: l7574});
    at = 10435; if (rt.truthy(l7548)) {
      at = 10436; l7582 = field(rt, l7551, "points");
      at = 10437; l7581 = l7582;
    } else {
      at = 10438; l7583 = K[2879];
      at = 10439; l7581 = l7583;
    }
    at = 10440; l7584 = K[2880];
    at = 10441; l7585 = rt.sub(l7555, l7584);
    at = 10442; l7586 = K[2881];
    at = 10443; l7587 = rt.max(l7586, l7585);
    at = 10444; l7588 = K[2882];
    at = 10445; l7589 = rt.array_slice(l7581, l7588, l7587);
    at = 10446; if (rt.truthy(l7548)) {
      at = 10447; l7591 = field(rt, l7551, "points");
      at = 10448; l7590 = l7591;
    } else {
      at = 10449; l7592 = K[2883];
      at = 10450; l7590 = l7592;
    }
    at = 10451; l7593 = K[2884];
    at = 10452; l7594 = rt.gt(l7555, l7593);
    at = 10453; if (rt.truthy(l7594)) {
      at = 10454; l7596 = K[2885];
      at = 10455; l7595 = l7596;
    } else {
      at = 10456; l7597 = K[2886];
      at = 10457; l7595 = l7597;
    }
    at = 10458; l7598 = K[2887];
    at = 10459; l7599 = rt.sub(l7555, l7598);
    at = 10460; l7600 = K[2888];
    at = 10461; l7601 = rt.max(l7600, l7599);
    at = 10462; l7602 = rt.array_slice(l7590, l7595, l7601);
    at = 10463; if (rt.truthy(l7548)) {
      at = 10464; l7604 = field(rt, l7551, "color");
      at = 10465; l7603 = l7604;
    } else {
      at = 10466; l7605 = K[2889];
      at = 10467; l7603 = l7605;
    }
    at = 10468; l7606 = K[2890];
    at = 10469; l7607 = rt.sequence_get(l7606, l7603);
    at = 10470; if (rt.truthy(l7548)) {
      at = 10471; l7609 = field(rt, l7551, "width");
      at = 10472; l7608 = l7609;
    } else {
      at = 10473; l7610 = K[2891];
      at = 10474; l7608 = l7610;
    }
    at = 10475; markSet(rt, true, "line", {from: l7589, to: l7602, color: l7607, width: l7608});
    at = 10476; l7611 = field(rt, l15, "strokes");
    at = 10477; l7612 = rt.sequence_length(l7611);
    at = 10478; l7613 = K[2892];
    at = 10479; l7614 = rt.lt(l7613, l7612);
    at = 10480; if (rt.truthy(l7614)) {
      at = 10481; l7615 = field(rt, l15, "strokes");
      at = 10482; l7616 = K[2893];
      at = 10483; l7617 = rt.sequence_get(l7615, l7616);
    }
    at = 10484; if (rt.truthy(l7614)) {
      at = 10485; l7619 = field(rt, l7617, "points");
      at = 10486; l7618 = l7619;
    } else {
      at = 10487; l7620 = K[2894];
      at = 10488; l7618 = l7620;
    }
    at = 10489; l7621 = rt.array_count(l7618);
    at = 10490; l7622 = K[2895];
    at = 10491; l7623 = rt.gt(l7621, l7622);
    at = 10492; if (rt.truthy(l7623)) {
      at = 10493; if (rt.truthy(l7614)) {
        at = 10494; l7626 = field(rt, l7617, "points");
        at = 10495; l7625 = l7626;
      } else {
        at = 10496; l7627 = K[2896];
        at = 10497; l7625 = l7627;
      }
      at = 10498; l7628 = K[2897];
      at = 10499; l7629 = rt.array_nth(l7625, l7628);
      at = 10500; l7624 = l7629;
    } else {
      at = 10501; l7630 = K[2898];
      at = 10502; l7624 = l7630;
    }
    at = 10503; if (rt.truthy(l7614)) {
      at = 10504; l7632 = field(rt, l7617, "width");
      at = 10505; l7631 = l7632;
    } else {
      at = 10506; l7633 = K[2899];
      at = 10507; l7631 = l7633;
    }
    at = 10508; l7634 = K[2900];
    at = 10509; l7635 = rt.int_div(l7631, l7634);
    at = 10510; l7636 = K[2901];
    at = 10511; l7637 = rt.max(l7636, l7635);
    at = 10512; l7638 = K[2902];
    at = 10513; l7639 = rt.gt(l7621, l7638);
    at = 10514; if (rt.truthy(l7639)) {
      at = 10515; if (rt.truthy(l7614)) {
        at = 10516; l7642 = field(rt, l7617, "color");
        at = 10517; l7641 = l7642;
      } else {
        at = 10518; l7643 = K[2903];
        at = 10519; l7641 = l7643;
      }
      at = 10520; l7644 = K[2904];
      at = 10521; l7645 = rt.sequence_get(l7644, l7641);
      at = 10522; l7640 = l7645;
    } else {
      at = 10523; l7646 = K[2905];
      at = 10524; l7640 = l7646;
    }
    at = 10525; markSet(rt, false, "circle", {at: l7624, radius: l7637, fill: l7640});
    at = 10526; if (rt.truthy(l7614)) {
      at = 10527; l7648 = field(rt, l7617, "points");
      at = 10528; l7647 = l7648;
    } else {
      at = 10529; l7649 = K[2906];
      at = 10530; l7647 = l7649;
    }
    at = 10531; l7650 = K[2907];
    at = 10532; l7651 = rt.sub(l7621, l7650);
    at = 10533; l7652 = K[2908];
    at = 10534; l7653 = rt.max(l7652, l7651);
    at = 10535; l7654 = K[2909];
    at = 10536; l7655 = rt.array_slice(l7647, l7654, l7653);
    at = 10537; if (rt.truthy(l7614)) {
      at = 10538; l7657 = field(rt, l7617, "points");
      at = 10539; l7656 = l7657;
    } else {
      at = 10540; l7658 = K[2910];
      at = 10541; l7656 = l7658;
    }
    at = 10542; l7659 = K[2911];
    at = 10543; l7660 = rt.gt(l7621, l7659);
    at = 10544; if (rt.truthy(l7660)) {
      at = 10545; l7662 = K[2912];
      at = 10546; l7661 = l7662;
    } else {
      at = 10547; l7663 = K[2913];
      at = 10548; l7661 = l7663;
    }
    at = 10549; l7664 = K[2914];
    at = 10550; l7665 = rt.sub(l7621, l7664);
    at = 10551; l7666 = K[2915];
    at = 10552; l7667 = rt.max(l7666, l7665);
    at = 10553; l7668 = rt.array_slice(l7656, l7661, l7667);
    at = 10554; if (rt.truthy(l7614)) {
      at = 10555; l7670 = field(rt, l7617, "color");
      at = 10556; l7669 = l7670;
    } else {
      at = 10557; l7671 = K[2916];
      at = 10558; l7669 = l7671;
    }
    at = 10559; l7672 = K[2917];
    at = 10560; l7673 = rt.sequence_get(l7672, l7669);
    at = 10561; if (rt.truthy(l7614)) {
      at = 10562; l7675 = field(rt, l7617, "width");
      at = 10563; l7674 = l7675;
    } else {
      at = 10564; l7676 = K[2918];
      at = 10565; l7674 = l7676;
    }
    at = 10566; markSet(rt, true, "line", {from: l7655, to: l7668, color: l7673, width: l7674});
    at = 10567; l7677 = field(rt, l15, "strokes");
    at = 10568; l7678 = rt.sequence_length(l7677);
    at = 10569; l7679 = K[2919];
    at = 10570; l7680 = rt.lt(l7679, l7678);
    at = 10571; if (rt.truthy(l7680)) {
      at = 10572; l7681 = field(rt, l15, "strokes");
      at = 10573; l7682 = K[2920];
      at = 10574; l7683 = rt.sequence_get(l7681, l7682);
    }
    at = 10575; if (rt.truthy(l7680)) {
      at = 10576; l7685 = field(rt, l7683, "points");
      at = 10577; l7684 = l7685;
    } else {
      at = 10578; l7686 = K[2921];
      at = 10579; l7684 = l7686;
    }
    at = 10580; l7687 = rt.array_count(l7684);
    at = 10581; l7688 = K[2922];
    at = 10582; l7689 = rt.gt(l7687, l7688);
    at = 10583; if (rt.truthy(l7689)) {
      at = 10584; if (rt.truthy(l7680)) {
        at = 10585; l7692 = field(rt, l7683, "points");
        at = 10586; l7691 = l7692;
      } else {
        at = 10587; l7693 = K[2923];
        at = 10588; l7691 = l7693;
      }
      at = 10589; l7694 = K[2924];
      at = 10590; l7695 = rt.array_nth(l7691, l7694);
      at = 10591; l7690 = l7695;
    } else {
      at = 10592; l7696 = K[2925];
      at = 10593; l7690 = l7696;
    }
    at = 10594; if (rt.truthy(l7680)) {
      at = 10595; l7698 = field(rt, l7683, "width");
      at = 10596; l7697 = l7698;
    } else {
      at = 10597; l7699 = K[2926];
      at = 10598; l7697 = l7699;
    }
    at = 10599; l7700 = K[2927];
    at = 10600; l7701 = rt.int_div(l7697, l7700);
    at = 10601; l7702 = K[2928];
    at = 10602; l7703 = rt.max(l7702, l7701);
    at = 10603; l7704 = K[2929];
    at = 10604; l7705 = rt.gt(l7687, l7704);
    at = 10605; if (rt.truthy(l7705)) {
      at = 10606; if (rt.truthy(l7680)) {
        at = 10607; l7708 = field(rt, l7683, "color");
        at = 10608; l7707 = l7708;
      } else {
        at = 10609; l7709 = K[2930];
        at = 10610; l7707 = l7709;
      }
      at = 10611; l7710 = K[2931];
      at = 10612; l7711 = rt.sequence_get(l7710, l7707);
      at = 10613; l7706 = l7711;
    } else {
      at = 10614; l7712 = K[2932];
      at = 10615; l7706 = l7712;
    }
    at = 10616; markSet(rt, false, "circle", {at: l7690, radius: l7703, fill: l7706});
    at = 10617; if (rt.truthy(l7680)) {
      at = 10618; l7714 = field(rt, l7683, "points");
      at = 10619; l7713 = l7714;
    } else {
      at = 10620; l7715 = K[2933];
      at = 10621; l7713 = l7715;
    }
    at = 10622; l7716 = K[2934];
    at = 10623; l7717 = rt.sub(l7687, l7716);
    at = 10624; l7718 = K[2935];
    at = 10625; l7719 = rt.max(l7718, l7717);
    at = 10626; l7720 = K[2936];
    at = 10627; l7721 = rt.array_slice(l7713, l7720, l7719);
    at = 10628; if (rt.truthy(l7680)) {
      at = 10629; l7723 = field(rt, l7683, "points");
      at = 10630; l7722 = l7723;
    } else {
      at = 10631; l7724 = K[2937];
      at = 10632; l7722 = l7724;
    }
    at = 10633; l7725 = K[2938];
    at = 10634; l7726 = rt.gt(l7687, l7725);
    at = 10635; if (rt.truthy(l7726)) {
      at = 10636; l7728 = K[2939];
      at = 10637; l7727 = l7728;
    } else {
      at = 10638; l7729 = K[2940];
      at = 10639; l7727 = l7729;
    }
    at = 10640; l7730 = K[2941];
    at = 10641; l7731 = rt.sub(l7687, l7730);
    at = 10642; l7732 = K[2942];
    at = 10643; l7733 = rt.max(l7732, l7731);
    at = 10644; l7734 = rt.array_slice(l7722, l7727, l7733);
    at = 10645; if (rt.truthy(l7680)) {
      at = 10646; l7736 = field(rt, l7683, "color");
      at = 10647; l7735 = l7736;
    } else {
      at = 10648; l7737 = K[2943];
      at = 10649; l7735 = l7737;
    }
    at = 10650; l7738 = K[2944];
    at = 10651; l7739 = rt.sequence_get(l7738, l7735);
    at = 10652; if (rt.truthy(l7680)) {
      at = 10653; l7741 = field(rt, l7683, "width");
      at = 10654; l7740 = l7741;
    } else {
      at = 10655; l7742 = K[2945];
      at = 10656; l7740 = l7742;
    }
    at = 10657; markSet(rt, true, "line", {from: l7721, to: l7734, color: l7739, width: l7740});
    at = 10658; l7743 = field(rt, l15, "strokes");
    at = 10659; l7744 = rt.sequence_length(l7743);
    at = 10660; l7745 = K[2946];
    at = 10661; l7746 = rt.lt(l7745, l7744);
    at = 10662; if (rt.truthy(l7746)) {
      at = 10663; l7747 = field(rt, l15, "strokes");
      at = 10664; l7748 = K[2947];
      at = 10665; l7749 = rt.sequence_get(l7747, l7748);
    }
    at = 10666; if (rt.truthy(l7746)) {
      at = 10667; l7751 = field(rt, l7749, "points");
      at = 10668; l7750 = l7751;
    } else {
      at = 10669; l7752 = K[2948];
      at = 10670; l7750 = l7752;
    }
    at = 10671; l7753 = rt.array_count(l7750);
    at = 10672; l7754 = K[2949];
    at = 10673; l7755 = rt.gt(l7753, l7754);
    at = 10674; if (rt.truthy(l7755)) {
      at = 10675; if (rt.truthy(l7746)) {
        at = 10676; l7758 = field(rt, l7749, "points");
        at = 10677; l7757 = l7758;
      } else {
        at = 10678; l7759 = K[2950];
        at = 10679; l7757 = l7759;
      }
      at = 10680; l7760 = K[2951];
      at = 10681; l7761 = rt.array_nth(l7757, l7760);
      at = 10682; l7756 = l7761;
    } else {
      at = 10683; l7762 = K[2952];
      at = 10684; l7756 = l7762;
    }
    at = 10685; if (rt.truthy(l7746)) {
      at = 10686; l7764 = field(rt, l7749, "width");
      at = 10687; l7763 = l7764;
    } else {
      at = 10688; l7765 = K[2953];
      at = 10689; l7763 = l7765;
    }
    at = 10690; l7766 = K[2954];
    at = 10691; l7767 = rt.int_div(l7763, l7766);
    at = 10692; l7768 = K[2955];
    at = 10693; l7769 = rt.max(l7768, l7767);
    at = 10694; l7770 = K[2956];
    at = 10695; l7771 = rt.gt(l7753, l7770);
    at = 10696; if (rt.truthy(l7771)) {
      at = 10697; if (rt.truthy(l7746)) {
        at = 10698; l7774 = field(rt, l7749, "color");
        at = 10699; l7773 = l7774;
      } else {
        at = 10700; l7775 = K[2957];
        at = 10701; l7773 = l7775;
      }
      at = 10702; l7776 = K[2958];
      at = 10703; l7777 = rt.sequence_get(l7776, l7773);
      at = 10704; l7772 = l7777;
    } else {
      at = 10705; l7778 = K[2959];
      at = 10706; l7772 = l7778;
    }
    at = 10707; markSet(rt, false, "circle", {at: l7756, radius: l7769, fill: l7772});
    at = 10708; if (rt.truthy(l7746)) {
      at = 10709; l7780 = field(rt, l7749, "points");
      at = 10710; l7779 = l7780;
    } else {
      at = 10711; l7781 = K[2960];
      at = 10712; l7779 = l7781;
    }
    at = 10713; l7782 = K[2961];
    at = 10714; l7783 = rt.sub(l7753, l7782);
    at = 10715; l7784 = K[2962];
    at = 10716; l7785 = rt.max(l7784, l7783);
    at = 10717; l7786 = K[2963];
    at = 10718; l7787 = rt.array_slice(l7779, l7786, l7785);
    at = 10719; if (rt.truthy(l7746)) {
      at = 10720; l7789 = field(rt, l7749, "points");
      at = 10721; l7788 = l7789;
    } else {
      at = 10722; l7790 = K[2964];
      at = 10723; l7788 = l7790;
    }
    at = 10724; l7791 = K[2965];
    at = 10725; l7792 = rt.gt(l7753, l7791);
    at = 10726; if (rt.truthy(l7792)) {
      at = 10727; l7794 = K[2966];
      at = 10728; l7793 = l7794;
    } else {
      at = 10729; l7795 = K[2967];
      at = 10730; l7793 = l7795;
    }
    at = 10731; l7796 = K[2968];
    at = 10732; l7797 = rt.sub(l7753, l7796);
    at = 10733; l7798 = K[2969];
    at = 10734; l7799 = rt.max(l7798, l7797);
    at = 10735; l7800 = rt.array_slice(l7788, l7793, l7799);
    at = 10736; if (rt.truthy(l7746)) {
      at = 10737; l7802 = field(rt, l7749, "color");
      at = 10738; l7801 = l7802;
    } else {
      at = 10739; l7803 = K[2970];
      at = 10740; l7801 = l7803;
    }
    at = 10741; l7804 = K[2971];
    at = 10742; l7805 = rt.sequence_get(l7804, l7801);
    at = 10743; if (rt.truthy(l7746)) {
      at = 10744; l7807 = field(rt, l7749, "width");
      at = 10745; l7806 = l7807;
    } else {
      at = 10746; l7808 = K[2972];
      at = 10747; l7806 = l7808;
    }
    at = 10748; markSet(rt, true, "line", {from: l7787, to: l7800, color: l7805, width: l7806});
    at = 10749; l7809 = field(rt, l15, "strokes");
    at = 10750; l7810 = rt.sequence_length(l7809);
    at = 10751; l7811 = K[2973];
    at = 10752; l7812 = rt.lt(l7811, l7810);
    at = 10753; if (rt.truthy(l7812)) {
      at = 10754; l7813 = field(rt, l15, "strokes");
      at = 10755; l7814 = K[2974];
      at = 10756; l7815 = rt.sequence_get(l7813, l7814);
    }
    at = 10757; if (rt.truthy(l7812)) {
      at = 10758; l7817 = field(rt, l7815, "points");
      at = 10759; l7816 = l7817;
    } else {
      at = 10760; l7818 = K[2975];
      at = 10761; l7816 = l7818;
    }
    at = 10762; l7819 = rt.array_count(l7816);
    at = 10763; l7820 = K[2976];
    at = 10764; l7821 = rt.gt(l7819, l7820);
    at = 10765; if (rt.truthy(l7821)) {
      at = 10766; if (rt.truthy(l7812)) {
        at = 10767; l7824 = field(rt, l7815, "points");
        at = 10768; l7823 = l7824;
      } else {
        at = 10769; l7825 = K[2977];
        at = 10770; l7823 = l7825;
      }
      at = 10771; l7826 = K[2978];
      at = 10772; l7827 = rt.array_nth(l7823, l7826);
      at = 10773; l7822 = l7827;
    } else {
      at = 10774; l7828 = K[2979];
      at = 10775; l7822 = l7828;
    }
    at = 10776; if (rt.truthy(l7812)) {
      at = 10777; l7830 = field(rt, l7815, "width");
      at = 10778; l7829 = l7830;
    } else {
      at = 10779; l7831 = K[2980];
      at = 10780; l7829 = l7831;
    }
    at = 10781; l7832 = K[2981];
    at = 10782; l7833 = rt.int_div(l7829, l7832);
    at = 10783; l7834 = K[2982];
    at = 10784; l7835 = rt.max(l7834, l7833);
    at = 10785; l7836 = K[2983];
    at = 10786; l7837 = rt.gt(l7819, l7836);
    at = 10787; if (rt.truthy(l7837)) {
      at = 10788; if (rt.truthy(l7812)) {
        at = 10789; l7840 = field(rt, l7815, "color");
        at = 10790; l7839 = l7840;
      } else {
        at = 10791; l7841 = K[2984];
        at = 10792; l7839 = l7841;
      }
      at = 10793; l7842 = K[2985];
      at = 10794; l7843 = rt.sequence_get(l7842, l7839);
      at = 10795; l7838 = l7843;
    } else {
      at = 10796; l7844 = K[2986];
      at = 10797; l7838 = l7844;
    }
    at = 10798; markSet(rt, false, "circle", {at: l7822, radius: l7835, fill: l7838});
    at = 10799; if (rt.truthy(l7812)) {
      at = 10800; l7846 = field(rt, l7815, "points");
      at = 10801; l7845 = l7846;
    } else {
      at = 10802; l7847 = K[2987];
      at = 10803; l7845 = l7847;
    }
    at = 10804; l7848 = K[2988];
    at = 10805; l7849 = rt.sub(l7819, l7848);
    at = 10806; l7850 = K[2989];
    at = 10807; l7851 = rt.max(l7850, l7849);
    at = 10808; l7852 = K[2990];
    at = 10809; l7853 = rt.array_slice(l7845, l7852, l7851);
    at = 10810; if (rt.truthy(l7812)) {
      at = 10811; l7855 = field(rt, l7815, "points");
      at = 10812; l7854 = l7855;
    } else {
      at = 10813; l7856 = K[2991];
      at = 10814; l7854 = l7856;
    }
    at = 10815; l7857 = K[2992];
    at = 10816; l7858 = rt.gt(l7819, l7857);
    at = 10817; if (rt.truthy(l7858)) {
      at = 10818; l7860 = K[2993];
      at = 10819; l7859 = l7860;
    } else {
      at = 10820; l7861 = K[2994];
      at = 10821; l7859 = l7861;
    }
    at = 10822; l7862 = K[2995];
    at = 10823; l7863 = rt.sub(l7819, l7862);
    at = 10824; l7864 = K[2996];
    at = 10825; l7865 = rt.max(l7864, l7863);
    at = 10826; l7866 = rt.array_slice(l7854, l7859, l7865);
    at = 10827; if (rt.truthy(l7812)) {
      at = 10828; l7868 = field(rt, l7815, "color");
      at = 10829; l7867 = l7868;
    } else {
      at = 10830; l7869 = K[2997];
      at = 10831; l7867 = l7869;
    }
    at = 10832; l7870 = K[2998];
    at = 10833; l7871 = rt.sequence_get(l7870, l7867);
    at = 10834; if (rt.truthy(l7812)) {
      at = 10835; l7873 = field(rt, l7815, "width");
      at = 10836; l7872 = l7873;
    } else {
      at = 10837; l7874 = K[2999];
      at = 10838; l7872 = l7874;
    }
    at = 10839; markSet(rt, true, "line", {from: l7853, to: l7866, color: l7871, width: l7872});
    at = 10840; l7875 = field(rt, l15, "strokes");
    at = 10841; l7876 = rt.sequence_length(l7875);
    at = 10842; l7877 = K[3000];
    at = 10843; l7878 = rt.lt(l7877, l7876);
    at = 10844; if (rt.truthy(l7878)) {
      at = 10845; l7879 = field(rt, l15, "strokes");
      at = 10846; l7880 = K[3001];
      at = 10847; l7881 = rt.sequence_get(l7879, l7880);
    }
    at = 10848; if (rt.truthy(l7878)) {
      at = 10849; l7883 = field(rt, l7881, "points");
      at = 10850; l7882 = l7883;
    } else {
      at = 10851; l7884 = K[3002];
      at = 10852; l7882 = l7884;
    }
    at = 10853; l7885 = rt.array_count(l7882);
    at = 10854; l7886 = K[3003];
    at = 10855; l7887 = rt.gt(l7885, l7886);
    at = 10856; if (rt.truthy(l7887)) {
      at = 10857; if (rt.truthy(l7878)) {
        at = 10858; l7890 = field(rt, l7881, "points");
        at = 10859; l7889 = l7890;
      } else {
        at = 10860; l7891 = K[3004];
        at = 10861; l7889 = l7891;
      }
      at = 10862; l7892 = K[3005];
      at = 10863; l7893 = rt.array_nth(l7889, l7892);
      at = 10864; l7888 = l7893;
    } else {
      at = 10865; l7894 = K[3006];
      at = 10866; l7888 = l7894;
    }
    at = 10867; if (rt.truthy(l7878)) {
      at = 10868; l7896 = field(rt, l7881, "width");
      at = 10869; l7895 = l7896;
    } else {
      at = 10870; l7897 = K[3007];
      at = 10871; l7895 = l7897;
    }
    at = 10872; l7898 = K[3008];
    at = 10873; l7899 = rt.int_div(l7895, l7898);
    at = 10874; l7900 = K[3009];
    at = 10875; l7901 = rt.max(l7900, l7899);
    at = 10876; l7902 = K[3010];
    at = 10877; l7903 = rt.gt(l7885, l7902);
    at = 10878; if (rt.truthy(l7903)) {
      at = 10879; if (rt.truthy(l7878)) {
        at = 10880; l7906 = field(rt, l7881, "color");
        at = 10881; l7905 = l7906;
      } else {
        at = 10882; l7907 = K[3011];
        at = 10883; l7905 = l7907;
      }
      at = 10884; l7908 = K[3012];
      at = 10885; l7909 = rt.sequence_get(l7908, l7905);
      at = 10886; l7904 = l7909;
    } else {
      at = 10887; l7910 = K[3013];
      at = 10888; l7904 = l7910;
    }
    at = 10889; markSet(rt, false, "circle", {at: l7888, radius: l7901, fill: l7904});
    at = 10890; if (rt.truthy(l7878)) {
      at = 10891; l7912 = field(rt, l7881, "points");
      at = 10892; l7911 = l7912;
    } else {
      at = 10893; l7913 = K[3014];
      at = 10894; l7911 = l7913;
    }
    at = 10895; l7914 = K[3015];
    at = 10896; l7915 = rt.sub(l7885, l7914);
    at = 10897; l7916 = K[3016];
    at = 10898; l7917 = rt.max(l7916, l7915);
    at = 10899; l7918 = K[3017];
    at = 10900; l7919 = rt.array_slice(l7911, l7918, l7917);
    at = 10901; if (rt.truthy(l7878)) {
      at = 10902; l7921 = field(rt, l7881, "points");
      at = 10903; l7920 = l7921;
    } else {
      at = 10904; l7922 = K[3018];
      at = 10905; l7920 = l7922;
    }
    at = 10906; l7923 = K[3019];
    at = 10907; l7924 = rt.gt(l7885, l7923);
    at = 10908; if (rt.truthy(l7924)) {
      at = 10909; l7926 = K[3020];
      at = 10910; l7925 = l7926;
    } else {
      at = 10911; l7927 = K[3021];
      at = 10912; l7925 = l7927;
    }
    at = 10913; l7928 = K[3022];
    at = 10914; l7929 = rt.sub(l7885, l7928);
    at = 10915; l7930 = K[3023];
    at = 10916; l7931 = rt.max(l7930, l7929);
    at = 10917; l7932 = rt.array_slice(l7920, l7925, l7931);
    at = 10918; if (rt.truthy(l7878)) {
      at = 10919; l7934 = field(rt, l7881, "color");
      at = 10920; l7933 = l7934;
    } else {
      at = 10921; l7935 = K[3024];
      at = 10922; l7933 = l7935;
    }
    at = 10923; l7936 = K[3025];
    at = 10924; l7937 = rt.sequence_get(l7936, l7933);
    at = 10925; if (rt.truthy(l7878)) {
      at = 10926; l7939 = field(rt, l7881, "width");
      at = 10927; l7938 = l7939;
    } else {
      at = 10928; l7940 = K[3026];
      at = 10929; l7938 = l7940;
    }
    at = 10930; markSet(rt, true, "line", {from: l7919, to: l7932, color: l7937, width: l7938});
    at = 10931; l7941 = field(rt, l15, "strokes");
    at = 10932; l7942 = rt.sequence_length(l7941);
    at = 10933; l7943 = K[3027];
    at = 10934; l7944 = rt.lt(l7943, l7942);
    at = 10935; if (rt.truthy(l7944)) {
      at = 10936; l7945 = field(rt, l15, "strokes");
      at = 10937; l7946 = K[3028];
      at = 10938; l7947 = rt.sequence_get(l7945, l7946);
    }
    at = 10939; if (rt.truthy(l7944)) {
      at = 10940; l7949 = field(rt, l7947, "points");
      at = 10941; l7948 = l7949;
    } else {
      at = 10942; l7950 = K[3029];
      at = 10943; l7948 = l7950;
    }
    at = 10944; l7951 = rt.array_count(l7948);
    at = 10945; l7952 = K[3030];
    at = 10946; l7953 = rt.gt(l7951, l7952);
    at = 10947; if (rt.truthy(l7953)) {
      at = 10948; if (rt.truthy(l7944)) {
        at = 10949; l7956 = field(rt, l7947, "points");
        at = 10950; l7955 = l7956;
      } else {
        at = 10951; l7957 = K[3031];
        at = 10952; l7955 = l7957;
      }
      at = 10953; l7958 = K[3032];
      at = 10954; l7959 = rt.array_nth(l7955, l7958);
      at = 10955; l7954 = l7959;
    } else {
      at = 10956; l7960 = K[3033];
      at = 10957; l7954 = l7960;
    }
    at = 10958; if (rt.truthy(l7944)) {
      at = 10959; l7962 = field(rt, l7947, "width");
      at = 10960; l7961 = l7962;
    } else {
      at = 10961; l7963 = K[3034];
      at = 10962; l7961 = l7963;
    }
    at = 10963; l7964 = K[3035];
    at = 10964; l7965 = rt.int_div(l7961, l7964);
    at = 10965; l7966 = K[3036];
    at = 10966; l7967 = rt.max(l7966, l7965);
    at = 10967; l7968 = K[3037];
    at = 10968; l7969 = rt.gt(l7951, l7968);
    at = 10969; if (rt.truthy(l7969)) {
      at = 10970; if (rt.truthy(l7944)) {
        at = 10971; l7972 = field(rt, l7947, "color");
        at = 10972; l7971 = l7972;
      } else {
        at = 10973; l7973 = K[3038];
        at = 10974; l7971 = l7973;
      }
      at = 10975; l7974 = K[3039];
      at = 10976; l7975 = rt.sequence_get(l7974, l7971);
      at = 10977; l7970 = l7975;
    } else {
      at = 10978; l7976 = K[3040];
      at = 10979; l7970 = l7976;
    }
    at = 10980; markSet(rt, false, "circle", {at: l7954, radius: l7967, fill: l7970});
    at = 10981; if (rt.truthy(l7944)) {
      at = 10982; l7978 = field(rt, l7947, "points");
      at = 10983; l7977 = l7978;
    } else {
      at = 10984; l7979 = K[3041];
      at = 10985; l7977 = l7979;
    }
    at = 10986; l7980 = K[3042];
    at = 10987; l7981 = rt.sub(l7951, l7980);
    at = 10988; l7982 = K[3043];
    at = 10989; l7983 = rt.max(l7982, l7981);
    at = 10990; l7984 = K[3044];
    at = 10991; l7985 = rt.array_slice(l7977, l7984, l7983);
    at = 10992; if (rt.truthy(l7944)) {
      at = 10993; l7987 = field(rt, l7947, "points");
      at = 10994; l7986 = l7987;
    } else {
      at = 10995; l7988 = K[3045];
      at = 10996; l7986 = l7988;
    }
    at = 10997; l7989 = K[3046];
    at = 10998; l7990 = rt.gt(l7951, l7989);
    at = 10999; if (rt.truthy(l7990)) {
      at = 11000; l7992 = K[3047];
      at = 11001; l7991 = l7992;
    } else {
      at = 11002; l7993 = K[3048];
      at = 11003; l7991 = l7993;
    }
    at = 11004; l7994 = K[3049];
    at = 11005; l7995 = rt.sub(l7951, l7994);
    at = 11006; l7996 = K[3050];
    at = 11007; l7997 = rt.max(l7996, l7995);
    at = 11008; l7998 = rt.array_slice(l7986, l7991, l7997);
    at = 11009; if (rt.truthy(l7944)) {
      at = 11010; l8000 = field(rt, l7947, "color");
      at = 11011; l7999 = l8000;
    } else {
      at = 11012; l8001 = K[3051];
      at = 11013; l7999 = l8001;
    }
    at = 11014; l8002 = K[3052];
    at = 11015; l8003 = rt.sequence_get(l8002, l7999);
    at = 11016; if (rt.truthy(l7944)) {
      at = 11017; l8005 = field(rt, l7947, "width");
      at = 11018; l8004 = l8005;
    } else {
      at = 11019; l8006 = K[3053];
      at = 11020; l8004 = l8006;
    }
    at = 11021; markSet(rt, true, "line", {from: l7985, to: l7998, color: l8003, width: l8004});
    at = 11022; l8007 = field(rt, l15, "strokes");
    at = 11023; l8008 = rt.sequence_length(l8007);
    at = 11024; l8009 = K[3054];
    at = 11025; l8010 = rt.lt(l8009, l8008);
    at = 11026; if (rt.truthy(l8010)) {
      at = 11027; l8011 = field(rt, l15, "strokes");
      at = 11028; l8012 = K[3055];
      at = 11029; l8013 = rt.sequence_get(l8011, l8012);
    }
    at = 11030; if (rt.truthy(l8010)) {
      at = 11031; l8015 = field(rt, l8013, "points");
      at = 11032; l8014 = l8015;
    } else {
      at = 11033; l8016 = K[3056];
      at = 11034; l8014 = l8016;
    }
    at = 11035; l8017 = rt.array_count(l8014);
    at = 11036; l8018 = K[3057];
    at = 11037; l8019 = rt.gt(l8017, l8018);
    at = 11038; if (rt.truthy(l8019)) {
      at = 11039; if (rt.truthy(l8010)) {
        at = 11040; l8022 = field(rt, l8013, "points");
        at = 11041; l8021 = l8022;
      } else {
        at = 11042; l8023 = K[3058];
        at = 11043; l8021 = l8023;
      }
      at = 11044; l8024 = K[3059];
      at = 11045; l8025 = rt.array_nth(l8021, l8024);
      at = 11046; l8020 = l8025;
    } else {
      at = 11047; l8026 = K[3060];
      at = 11048; l8020 = l8026;
    }
    at = 11049; if (rt.truthy(l8010)) {
      at = 11050; l8028 = field(rt, l8013, "width");
      at = 11051; l8027 = l8028;
    } else {
      at = 11052; l8029 = K[3061];
      at = 11053; l8027 = l8029;
    }
    at = 11054; l8030 = K[3062];
    at = 11055; l8031 = rt.int_div(l8027, l8030);
    at = 11056; l8032 = K[3063];
    at = 11057; l8033 = rt.max(l8032, l8031);
    at = 11058; l8034 = K[3064];
    at = 11059; l8035 = rt.gt(l8017, l8034);
    at = 11060; if (rt.truthy(l8035)) {
      at = 11061; if (rt.truthy(l8010)) {
        at = 11062; l8038 = field(rt, l8013, "color");
        at = 11063; l8037 = l8038;
      } else {
        at = 11064; l8039 = K[3065];
        at = 11065; l8037 = l8039;
      }
      at = 11066; l8040 = K[3066];
      at = 11067; l8041 = rt.sequence_get(l8040, l8037);
      at = 11068; l8036 = l8041;
    } else {
      at = 11069; l8042 = K[3067];
      at = 11070; l8036 = l8042;
    }
    at = 11071; markSet(rt, false, "circle", {at: l8020, radius: l8033, fill: l8036});
    at = 11072; if (rt.truthy(l8010)) {
      at = 11073; l8044 = field(rt, l8013, "points");
      at = 11074; l8043 = l8044;
    } else {
      at = 11075; l8045 = K[3068];
      at = 11076; l8043 = l8045;
    }
    at = 11077; l8046 = K[3069];
    at = 11078; l8047 = rt.sub(l8017, l8046);
    at = 11079; l8048 = K[3070];
    at = 11080; l8049 = rt.max(l8048, l8047);
    at = 11081; l8050 = K[3071];
    at = 11082; l8051 = rt.array_slice(l8043, l8050, l8049);
    at = 11083; if (rt.truthy(l8010)) {
      at = 11084; l8053 = field(rt, l8013, "points");
      at = 11085; l8052 = l8053;
    } else {
      at = 11086; l8054 = K[3072];
      at = 11087; l8052 = l8054;
    }
    at = 11088; l8055 = K[3073];
    at = 11089; l8056 = rt.gt(l8017, l8055);
    at = 11090; if (rt.truthy(l8056)) {
      at = 11091; l8058 = K[3074];
      at = 11092; l8057 = l8058;
    } else {
      at = 11093; l8059 = K[3075];
      at = 11094; l8057 = l8059;
    }
    at = 11095; l8060 = K[3076];
    at = 11096; l8061 = rt.sub(l8017, l8060);
    at = 11097; l8062 = K[3077];
    at = 11098; l8063 = rt.max(l8062, l8061);
    at = 11099; l8064 = rt.array_slice(l8052, l8057, l8063);
    at = 11100; if (rt.truthy(l8010)) {
      at = 11101; l8066 = field(rt, l8013, "color");
      at = 11102; l8065 = l8066;
    } else {
      at = 11103; l8067 = K[3078];
      at = 11104; l8065 = l8067;
    }
    at = 11105; l8068 = K[3079];
    at = 11106; l8069 = rt.sequence_get(l8068, l8065);
    at = 11107; if (rt.truthy(l8010)) {
      at = 11108; l8071 = field(rt, l8013, "width");
      at = 11109; l8070 = l8071;
    } else {
      at = 11110; l8072 = K[3080];
      at = 11111; l8070 = l8072;
    }
    at = 11112; markSet(rt, true, "line", {from: l8051, to: l8064, color: l8069, width: l8070});
    at = 11113; l8073 = field(rt, l15, "strokes");
    at = 11114; l8074 = rt.sequence_length(l8073);
    at = 11115; l8075 = K[3081];
    at = 11116; l8076 = rt.lt(l8075, l8074);
    at = 11117; if (rt.truthy(l8076)) {
      at = 11118; l8077 = field(rt, l15, "strokes");
      at = 11119; l8078 = K[3082];
      at = 11120; l8079 = rt.sequence_get(l8077, l8078);
    }
    at = 11121; if (rt.truthy(l8076)) {
      at = 11122; l8081 = field(rt, l8079, "points");
      at = 11123; l8080 = l8081;
    } else {
      at = 11124; l8082 = K[3083];
      at = 11125; l8080 = l8082;
    }
    at = 11126; l8083 = rt.array_count(l8080);
    at = 11127; l8084 = K[3084];
    at = 11128; l8085 = rt.gt(l8083, l8084);
    at = 11129; if (rt.truthy(l8085)) {
      at = 11130; if (rt.truthy(l8076)) {
        at = 11131; l8088 = field(rt, l8079, "points");
        at = 11132; l8087 = l8088;
      } else {
        at = 11133; l8089 = K[3085];
        at = 11134; l8087 = l8089;
      }
      at = 11135; l8090 = K[3086];
      at = 11136; l8091 = rt.array_nth(l8087, l8090);
      at = 11137; l8086 = l8091;
    } else {
      at = 11138; l8092 = K[3087];
      at = 11139; l8086 = l8092;
    }
    at = 11140; if (rt.truthy(l8076)) {
      at = 11141; l8094 = field(rt, l8079, "width");
      at = 11142; l8093 = l8094;
    } else {
      at = 11143; l8095 = K[3088];
      at = 11144; l8093 = l8095;
    }
    at = 11145; l8096 = K[3089];
    at = 11146; l8097 = rt.int_div(l8093, l8096);
    at = 11147; l8098 = K[3090];
    at = 11148; l8099 = rt.max(l8098, l8097);
    at = 11149; l8100 = K[3091];
    at = 11150; l8101 = rt.gt(l8083, l8100);
    at = 11151; if (rt.truthy(l8101)) {
      at = 11152; if (rt.truthy(l8076)) {
        at = 11153; l8104 = field(rt, l8079, "color");
        at = 11154; l8103 = l8104;
      } else {
        at = 11155; l8105 = K[3092];
        at = 11156; l8103 = l8105;
      }
      at = 11157; l8106 = K[3093];
      at = 11158; l8107 = rt.sequence_get(l8106, l8103);
      at = 11159; l8102 = l8107;
    } else {
      at = 11160; l8108 = K[3094];
      at = 11161; l8102 = l8108;
    }
    at = 11162; markSet(rt, false, "circle", {at: l8086, radius: l8099, fill: l8102});
    at = 11163; if (rt.truthy(l8076)) {
      at = 11164; l8110 = field(rt, l8079, "points");
      at = 11165; l8109 = l8110;
    } else {
      at = 11166; l8111 = K[3095];
      at = 11167; l8109 = l8111;
    }
    at = 11168; l8112 = K[3096];
    at = 11169; l8113 = rt.sub(l8083, l8112);
    at = 11170; l8114 = K[3097];
    at = 11171; l8115 = rt.max(l8114, l8113);
    at = 11172; l8116 = K[3098];
    at = 11173; l8117 = rt.array_slice(l8109, l8116, l8115);
    at = 11174; if (rt.truthy(l8076)) {
      at = 11175; l8119 = field(rt, l8079, "points");
      at = 11176; l8118 = l8119;
    } else {
      at = 11177; l8120 = K[3099];
      at = 11178; l8118 = l8120;
    }
    at = 11179; l8121 = K[3100];
    at = 11180; l8122 = rt.gt(l8083, l8121);
    at = 11181; if (rt.truthy(l8122)) {
      at = 11182; l8124 = K[3101];
      at = 11183; l8123 = l8124;
    } else {
      at = 11184; l8125 = K[3102];
      at = 11185; l8123 = l8125;
    }
    at = 11186; l8126 = K[3103];
    at = 11187; l8127 = rt.sub(l8083, l8126);
    at = 11188; l8128 = K[3104];
    at = 11189; l8129 = rt.max(l8128, l8127);
    at = 11190; l8130 = rt.array_slice(l8118, l8123, l8129);
    at = 11191; if (rt.truthy(l8076)) {
      at = 11192; l8132 = field(rt, l8079, "color");
      at = 11193; l8131 = l8132;
    } else {
      at = 11194; l8133 = K[3105];
      at = 11195; l8131 = l8133;
    }
    at = 11196; l8134 = K[3106];
    at = 11197; l8135 = rt.sequence_get(l8134, l8131);
    at = 11198; if (rt.truthy(l8076)) {
      at = 11199; l8137 = field(rt, l8079, "width");
      at = 11200; l8136 = l8137;
    } else {
      at = 11201; l8138 = K[3107];
      at = 11202; l8136 = l8138;
    }
    at = 11203; markSet(rt, true, "line", {from: l8117, to: l8130, color: l8135, width: l8136});
    at = 11204; l8139 = field(rt, l15, "strokes");
    at = 11205; l8140 = rt.sequence_length(l8139);
    at = 11206; l8141 = K[3108];
    at = 11207; l8142 = rt.lt(l8141, l8140);
    at = 11208; if (rt.truthy(l8142)) {
      at = 11209; l8143 = field(rt, l15, "strokes");
      at = 11210; l8144 = K[3109];
      at = 11211; l8145 = rt.sequence_get(l8143, l8144);
    }
    at = 11212; if (rt.truthy(l8142)) {
      at = 11213; l8147 = field(rt, l8145, "points");
      at = 11214; l8146 = l8147;
    } else {
      at = 11215; l8148 = K[3110];
      at = 11216; l8146 = l8148;
    }
    at = 11217; l8149 = rt.array_count(l8146);
    at = 11218; l8150 = K[3111];
    at = 11219; l8151 = rt.gt(l8149, l8150);
    at = 11220; if (rt.truthy(l8151)) {
      at = 11221; if (rt.truthy(l8142)) {
        at = 11222; l8154 = field(rt, l8145, "points");
        at = 11223; l8153 = l8154;
      } else {
        at = 11224; l8155 = K[3112];
        at = 11225; l8153 = l8155;
      }
      at = 11226; l8156 = K[3113];
      at = 11227; l8157 = rt.array_nth(l8153, l8156);
      at = 11228; l8152 = l8157;
    } else {
      at = 11229; l8158 = K[3114];
      at = 11230; l8152 = l8158;
    }
    at = 11231; if (rt.truthy(l8142)) {
      at = 11232; l8160 = field(rt, l8145, "width");
      at = 11233; l8159 = l8160;
    } else {
      at = 11234; l8161 = K[3115];
      at = 11235; l8159 = l8161;
    }
    at = 11236; l8162 = K[3116];
    at = 11237; l8163 = rt.int_div(l8159, l8162);
    at = 11238; l8164 = K[3117];
    at = 11239; l8165 = rt.max(l8164, l8163);
    at = 11240; l8166 = K[3118];
    at = 11241; l8167 = rt.gt(l8149, l8166);
    at = 11242; if (rt.truthy(l8167)) {
      at = 11243; if (rt.truthy(l8142)) {
        at = 11244; l8170 = field(rt, l8145, "color");
        at = 11245; l8169 = l8170;
      } else {
        at = 11246; l8171 = K[3119];
        at = 11247; l8169 = l8171;
      }
      at = 11248; l8172 = K[3120];
      at = 11249; l8173 = rt.sequence_get(l8172, l8169);
      at = 11250; l8168 = l8173;
    } else {
      at = 11251; l8174 = K[3121];
      at = 11252; l8168 = l8174;
    }
    at = 11253; markSet(rt, false, "circle", {at: l8152, radius: l8165, fill: l8168});
    at = 11254; if (rt.truthy(l8142)) {
      at = 11255; l8176 = field(rt, l8145, "points");
      at = 11256; l8175 = l8176;
    } else {
      at = 11257; l8177 = K[3122];
      at = 11258; l8175 = l8177;
    }
    at = 11259; l8178 = K[3123];
    at = 11260; l8179 = rt.sub(l8149, l8178);
    at = 11261; l8180 = K[3124];
    at = 11262; l8181 = rt.max(l8180, l8179);
    at = 11263; l8182 = K[3125];
    at = 11264; l8183 = rt.array_slice(l8175, l8182, l8181);
    at = 11265; if (rt.truthy(l8142)) {
      at = 11266; l8185 = field(rt, l8145, "points");
      at = 11267; l8184 = l8185;
    } else {
      at = 11268; l8186 = K[3126];
      at = 11269; l8184 = l8186;
    }
    at = 11270; l8187 = K[3127];
    at = 11271; l8188 = rt.gt(l8149, l8187);
    at = 11272; if (rt.truthy(l8188)) {
      at = 11273; l8190 = K[3128];
      at = 11274; l8189 = l8190;
    } else {
      at = 11275; l8191 = K[3129];
      at = 11276; l8189 = l8191;
    }
    at = 11277; l8192 = K[3130];
    at = 11278; l8193 = rt.sub(l8149, l8192);
    at = 11279; l8194 = K[3131];
    at = 11280; l8195 = rt.max(l8194, l8193);
    at = 11281; l8196 = rt.array_slice(l8184, l8189, l8195);
    at = 11282; if (rt.truthy(l8142)) {
      at = 11283; l8198 = field(rt, l8145, "color");
      at = 11284; l8197 = l8198;
    } else {
      at = 11285; l8199 = K[3132];
      at = 11286; l8197 = l8199;
    }
    at = 11287; l8200 = K[3133];
    at = 11288; l8201 = rt.sequence_get(l8200, l8197);
    at = 11289; if (rt.truthy(l8142)) {
      at = 11290; l8203 = field(rt, l8145, "width");
      at = 11291; l8202 = l8203;
    } else {
      at = 11292; l8204 = K[3134];
      at = 11293; l8202 = l8204;
    }
    at = 11294; markSet(rt, true, "line", {from: l8183, to: l8196, color: l8201, width: l8202});
    at = 11295; l8205 = field(rt, l15, "strokes");
    at = 11296; l8206 = rt.sequence_length(l8205);
    at = 11297; l8207 = K[3135];
    at = 11298; l8208 = rt.lt(l8207, l8206);
    at = 11299; if (rt.truthy(l8208)) {
      at = 11300; l8209 = field(rt, l15, "strokes");
      at = 11301; l8210 = K[3136];
      at = 11302; l8211 = rt.sequence_get(l8209, l8210);
    }
    at = 11303; if (rt.truthy(l8208)) {
      at = 11304; l8213 = field(rt, l8211, "points");
      at = 11305; l8212 = l8213;
    } else {
      at = 11306; l8214 = K[3137];
      at = 11307; l8212 = l8214;
    }
    at = 11308; l8215 = rt.array_count(l8212);
    at = 11309; l8216 = K[3138];
    at = 11310; l8217 = rt.gt(l8215, l8216);
    at = 11311; if (rt.truthy(l8217)) {
      at = 11312; if (rt.truthy(l8208)) {
        at = 11313; l8220 = field(rt, l8211, "points");
        at = 11314; l8219 = l8220;
      } else {
        at = 11315; l8221 = K[3139];
        at = 11316; l8219 = l8221;
      }
      at = 11317; l8222 = K[3140];
      at = 11318; l8223 = rt.array_nth(l8219, l8222);
      at = 11319; l8218 = l8223;
    } else {
      at = 11320; l8224 = K[3141];
      at = 11321; l8218 = l8224;
    }
    at = 11322; if (rt.truthy(l8208)) {
      at = 11323; l8226 = field(rt, l8211, "width");
      at = 11324; l8225 = l8226;
    } else {
      at = 11325; l8227 = K[3142];
      at = 11326; l8225 = l8227;
    }
    at = 11327; l8228 = K[3143];
    at = 11328; l8229 = rt.int_div(l8225, l8228);
    at = 11329; l8230 = K[3144];
    at = 11330; l8231 = rt.max(l8230, l8229);
    at = 11331; l8232 = K[3145];
    at = 11332; l8233 = rt.gt(l8215, l8232);
    at = 11333; if (rt.truthy(l8233)) {
      at = 11334; if (rt.truthy(l8208)) {
        at = 11335; l8236 = field(rt, l8211, "color");
        at = 11336; l8235 = l8236;
      } else {
        at = 11337; l8237 = K[3146];
        at = 11338; l8235 = l8237;
      }
      at = 11339; l8238 = K[3147];
      at = 11340; l8239 = rt.sequence_get(l8238, l8235);
      at = 11341; l8234 = l8239;
    } else {
      at = 11342; l8240 = K[3148];
      at = 11343; l8234 = l8240;
    }
    at = 11344; markSet(rt, false, "circle", {at: l8218, radius: l8231, fill: l8234});
    at = 11345; if (rt.truthy(l8208)) {
      at = 11346; l8242 = field(rt, l8211, "points");
      at = 11347; l8241 = l8242;
    } else {
      at = 11348; l8243 = K[3149];
      at = 11349; l8241 = l8243;
    }
    at = 11350; l8244 = K[3150];
    at = 11351; l8245 = rt.sub(l8215, l8244);
    at = 11352; l8246 = K[3151];
    at = 11353; l8247 = rt.max(l8246, l8245);
    at = 11354; l8248 = K[3152];
    at = 11355; l8249 = rt.array_slice(l8241, l8248, l8247);
    at = 11356; if (rt.truthy(l8208)) {
      at = 11357; l8251 = field(rt, l8211, "points");
      at = 11358; l8250 = l8251;
    } else {
      at = 11359; l8252 = K[3153];
      at = 11360; l8250 = l8252;
    }
    at = 11361; l8253 = K[3154];
    at = 11362; l8254 = rt.gt(l8215, l8253);
    at = 11363; if (rt.truthy(l8254)) {
      at = 11364; l8256 = K[3155];
      at = 11365; l8255 = l8256;
    } else {
      at = 11366; l8257 = K[3156];
      at = 11367; l8255 = l8257;
    }
    at = 11368; l8258 = K[3157];
    at = 11369; l8259 = rt.sub(l8215, l8258);
    at = 11370; l8260 = K[3158];
    at = 11371; l8261 = rt.max(l8260, l8259);
    at = 11372; l8262 = rt.array_slice(l8250, l8255, l8261);
    at = 11373; if (rt.truthy(l8208)) {
      at = 11374; l8264 = field(rt, l8211, "color");
      at = 11375; l8263 = l8264;
    } else {
      at = 11376; l8265 = K[3159];
      at = 11377; l8263 = l8265;
    }
    at = 11378; l8266 = K[3160];
    at = 11379; l8267 = rt.sequence_get(l8266, l8263);
    at = 11380; if (rt.truthy(l8208)) {
      at = 11381; l8269 = field(rt, l8211, "width");
      at = 11382; l8268 = l8269;
    } else {
      at = 11383; l8270 = K[3161];
      at = 11384; l8268 = l8270;
    }
    at = 11385; markSet(rt, true, "line", {from: l8249, to: l8262, color: l8267, width: l8268});
    at = 11386; l8271 = field(rt, l15, "strokes");
    at = 11387; l8272 = rt.sequence_length(l8271);
    at = 11388; l8273 = K[3162];
    at = 11389; l8274 = rt.lt(l8273, l8272);
    at = 11390; if (rt.truthy(l8274)) {
      at = 11391; l8275 = field(rt, l15, "strokes");
      at = 11392; l8276 = K[3163];
      at = 11393; l8277 = rt.sequence_get(l8275, l8276);
    }
    at = 11394; if (rt.truthy(l8274)) {
      at = 11395; l8279 = field(rt, l8277, "points");
      at = 11396; l8278 = l8279;
    } else {
      at = 11397; l8280 = K[3164];
      at = 11398; l8278 = l8280;
    }
    at = 11399; l8281 = rt.array_count(l8278);
    at = 11400; l8282 = K[3165];
    at = 11401; l8283 = rt.gt(l8281, l8282);
    at = 11402; if (rt.truthy(l8283)) {
      at = 11403; if (rt.truthy(l8274)) {
        at = 11404; l8286 = field(rt, l8277, "points");
        at = 11405; l8285 = l8286;
      } else {
        at = 11406; l8287 = K[3166];
        at = 11407; l8285 = l8287;
      }
      at = 11408; l8288 = K[3167];
      at = 11409; l8289 = rt.array_nth(l8285, l8288);
      at = 11410; l8284 = l8289;
    } else {
      at = 11411; l8290 = K[3168];
      at = 11412; l8284 = l8290;
    }
    at = 11413; if (rt.truthy(l8274)) {
      at = 11414; l8292 = field(rt, l8277, "width");
      at = 11415; l8291 = l8292;
    } else {
      at = 11416; l8293 = K[3169];
      at = 11417; l8291 = l8293;
    }
    at = 11418; l8294 = K[3170];
    at = 11419; l8295 = rt.int_div(l8291, l8294);
    at = 11420; l8296 = K[3171];
    at = 11421; l8297 = rt.max(l8296, l8295);
    at = 11422; l8298 = K[3172];
    at = 11423; l8299 = rt.gt(l8281, l8298);
    at = 11424; if (rt.truthy(l8299)) {
      at = 11425; if (rt.truthy(l8274)) {
        at = 11426; l8302 = field(rt, l8277, "color");
        at = 11427; l8301 = l8302;
      } else {
        at = 11428; l8303 = K[3173];
        at = 11429; l8301 = l8303;
      }
      at = 11430; l8304 = K[3174];
      at = 11431; l8305 = rt.sequence_get(l8304, l8301);
      at = 11432; l8300 = l8305;
    } else {
      at = 11433; l8306 = K[3175];
      at = 11434; l8300 = l8306;
    }
    at = 11435; markSet(rt, false, "circle", {at: l8284, radius: l8297, fill: l8300});
    at = 11436; if (rt.truthy(l8274)) {
      at = 11437; l8308 = field(rt, l8277, "points");
      at = 11438; l8307 = l8308;
    } else {
      at = 11439; l8309 = K[3176];
      at = 11440; l8307 = l8309;
    }
    at = 11441; l8310 = K[3177];
    at = 11442; l8311 = rt.sub(l8281, l8310);
    at = 11443; l8312 = K[3178];
    at = 11444; l8313 = rt.max(l8312, l8311);
    at = 11445; l8314 = K[3179];
    at = 11446; l8315 = rt.array_slice(l8307, l8314, l8313);
    at = 11447; if (rt.truthy(l8274)) {
      at = 11448; l8317 = field(rt, l8277, "points");
      at = 11449; l8316 = l8317;
    } else {
      at = 11450; l8318 = K[3180];
      at = 11451; l8316 = l8318;
    }
    at = 11452; l8319 = K[3181];
    at = 11453; l8320 = rt.gt(l8281, l8319);
    at = 11454; if (rt.truthy(l8320)) {
      at = 11455; l8322 = K[3182];
      at = 11456; l8321 = l8322;
    } else {
      at = 11457; l8323 = K[3183];
      at = 11458; l8321 = l8323;
    }
    at = 11459; l8324 = K[3184];
    at = 11460; l8325 = rt.sub(l8281, l8324);
    at = 11461; l8326 = K[3185];
    at = 11462; l8327 = rt.max(l8326, l8325);
    at = 11463; l8328 = rt.array_slice(l8316, l8321, l8327);
    at = 11464; if (rt.truthy(l8274)) {
      at = 11465; l8330 = field(rt, l8277, "color");
      at = 11466; l8329 = l8330;
    } else {
      at = 11467; l8331 = K[3186];
      at = 11468; l8329 = l8331;
    }
    at = 11469; l8332 = K[3187];
    at = 11470; l8333 = rt.sequence_get(l8332, l8329);
    at = 11471; if (rt.truthy(l8274)) {
      at = 11472; l8335 = field(rt, l8277, "width");
      at = 11473; l8334 = l8335;
    } else {
      at = 11474; l8336 = K[3188];
      at = 11475; l8334 = l8336;
    }
    at = 11476; markSet(rt, true, "line", {from: l8315, to: l8328, color: l8333, width: l8334});
    at = 11477; l8337 = field(rt, l15, "strokes");
    at = 11478; l8338 = rt.sequence_length(l8337);
    at = 11479; l8339 = K[3189];
    at = 11480; l8340 = rt.lt(l8339, l8338);
    at = 11481; if (rt.truthy(l8340)) {
      at = 11482; l8341 = field(rt, l15, "strokes");
      at = 11483; l8342 = K[3190];
      at = 11484; l8343 = rt.sequence_get(l8341, l8342);
    }
    at = 11485; if (rt.truthy(l8340)) {
      at = 11486; l8345 = field(rt, l8343, "points");
      at = 11487; l8344 = l8345;
    } else {
      at = 11488; l8346 = K[3191];
      at = 11489; l8344 = l8346;
    }
    at = 11490; l8347 = rt.array_count(l8344);
    at = 11491; l8348 = K[3192];
    at = 11492; l8349 = rt.gt(l8347, l8348);
    at = 11493; if (rt.truthy(l8349)) {
      at = 11494; if (rt.truthy(l8340)) {
        at = 11495; l8352 = field(rt, l8343, "points");
        at = 11496; l8351 = l8352;
      } else {
        at = 11497; l8353 = K[3193];
        at = 11498; l8351 = l8353;
      }
      at = 11499; l8354 = K[3194];
      at = 11500; l8355 = rt.array_nth(l8351, l8354);
      at = 11501; l8350 = l8355;
    } else {
      at = 11502; l8356 = K[3195];
      at = 11503; l8350 = l8356;
    }
    at = 11504; if (rt.truthy(l8340)) {
      at = 11505; l8358 = field(rt, l8343, "width");
      at = 11506; l8357 = l8358;
    } else {
      at = 11507; l8359 = K[3196];
      at = 11508; l8357 = l8359;
    }
    at = 11509; l8360 = K[3197];
    at = 11510; l8361 = rt.int_div(l8357, l8360);
    at = 11511; l8362 = K[3198];
    at = 11512; l8363 = rt.max(l8362, l8361);
    at = 11513; l8364 = K[3199];
    at = 11514; l8365 = rt.gt(l8347, l8364);
    at = 11515; if (rt.truthy(l8365)) {
      at = 11516; if (rt.truthy(l8340)) {
        at = 11517; l8368 = field(rt, l8343, "color");
        at = 11518; l8367 = l8368;
      } else {
        at = 11519; l8369 = K[3200];
        at = 11520; l8367 = l8369;
      }
      at = 11521; l8370 = K[3201];
      at = 11522; l8371 = rt.sequence_get(l8370, l8367);
      at = 11523; l8366 = l8371;
    } else {
      at = 11524; l8372 = K[3202];
      at = 11525; l8366 = l8372;
    }
    at = 11526; markSet(rt, false, "circle", {at: l8350, radius: l8363, fill: l8366});
    at = 11527; if (rt.truthy(l8340)) {
      at = 11528; l8374 = field(rt, l8343, "points");
      at = 11529; l8373 = l8374;
    } else {
      at = 11530; l8375 = K[3203];
      at = 11531; l8373 = l8375;
    }
    at = 11532; l8376 = K[3204];
    at = 11533; l8377 = rt.sub(l8347, l8376);
    at = 11534; l8378 = K[3205];
    at = 11535; l8379 = rt.max(l8378, l8377);
    at = 11536; l8380 = K[3206];
    at = 11537; l8381 = rt.array_slice(l8373, l8380, l8379);
    at = 11538; if (rt.truthy(l8340)) {
      at = 11539; l8383 = field(rt, l8343, "points");
      at = 11540; l8382 = l8383;
    } else {
      at = 11541; l8384 = K[3207];
      at = 11542; l8382 = l8384;
    }
    at = 11543; l8385 = K[3208];
    at = 11544; l8386 = rt.gt(l8347, l8385);
    at = 11545; if (rt.truthy(l8386)) {
      at = 11546; l8388 = K[3209];
      at = 11547; l8387 = l8388;
    } else {
      at = 11548; l8389 = K[3210];
      at = 11549; l8387 = l8389;
    }
    at = 11550; l8390 = K[3211];
    at = 11551; l8391 = rt.sub(l8347, l8390);
    at = 11552; l8392 = K[3212];
    at = 11553; l8393 = rt.max(l8392, l8391);
    at = 11554; l8394 = rt.array_slice(l8382, l8387, l8393);
    at = 11555; if (rt.truthy(l8340)) {
      at = 11556; l8396 = field(rt, l8343, "color");
      at = 11557; l8395 = l8396;
    } else {
      at = 11558; l8397 = K[3213];
      at = 11559; l8395 = l8397;
    }
    at = 11560; l8398 = K[3214];
    at = 11561; l8399 = rt.sequence_get(l8398, l8395);
    at = 11562; if (rt.truthy(l8340)) {
      at = 11563; l8401 = field(rt, l8343, "width");
      at = 11564; l8400 = l8401;
    } else {
      at = 11565; l8402 = K[3215];
      at = 11566; l8400 = l8402;
    }
    at = 11567; markSet(rt, true, "line", {from: l8381, to: l8394, color: l8399, width: l8400});
    at = 11568; l8403 = field(rt, l15, "strokes");
    at = 11569; l8404 = rt.sequence_length(l8403);
    at = 11570; l8405 = K[3216];
    at = 11571; l8406 = rt.lt(l8405, l8404);
    at = 11572; if (rt.truthy(l8406)) {
      at = 11573; l8407 = field(rt, l15, "strokes");
      at = 11574; l8408 = K[3217];
      at = 11575; l8409 = rt.sequence_get(l8407, l8408);
    }
    at = 11576; if (rt.truthy(l8406)) {
      at = 11577; l8411 = field(rt, l8409, "points");
      at = 11578; l8410 = l8411;
    } else {
      at = 11579; l8412 = K[3218];
      at = 11580; l8410 = l8412;
    }
    at = 11581; l8413 = rt.array_count(l8410);
    at = 11582; l8414 = K[3219];
    at = 11583; l8415 = rt.gt(l8413, l8414);
    at = 11584; if (rt.truthy(l8415)) {
      at = 11585; if (rt.truthy(l8406)) {
        at = 11586; l8418 = field(rt, l8409, "points");
        at = 11587; l8417 = l8418;
      } else {
        at = 11588; l8419 = K[3220];
        at = 11589; l8417 = l8419;
      }
      at = 11590; l8420 = K[3221];
      at = 11591; l8421 = rt.array_nth(l8417, l8420);
      at = 11592; l8416 = l8421;
    } else {
      at = 11593; l8422 = K[3222];
      at = 11594; l8416 = l8422;
    }
    at = 11595; if (rt.truthy(l8406)) {
      at = 11596; l8424 = field(rt, l8409, "width");
      at = 11597; l8423 = l8424;
    } else {
      at = 11598; l8425 = K[3223];
      at = 11599; l8423 = l8425;
    }
    at = 11600; l8426 = K[3224];
    at = 11601; l8427 = rt.int_div(l8423, l8426);
    at = 11602; l8428 = K[3225];
    at = 11603; l8429 = rt.max(l8428, l8427);
    at = 11604; l8430 = K[3226];
    at = 11605; l8431 = rt.gt(l8413, l8430);
    at = 11606; if (rt.truthy(l8431)) {
      at = 11607; if (rt.truthy(l8406)) {
        at = 11608; l8434 = field(rt, l8409, "color");
        at = 11609; l8433 = l8434;
      } else {
        at = 11610; l8435 = K[3227];
        at = 11611; l8433 = l8435;
      }
      at = 11612; l8436 = K[3228];
      at = 11613; l8437 = rt.sequence_get(l8436, l8433);
      at = 11614; l8432 = l8437;
    } else {
      at = 11615; l8438 = K[3229];
      at = 11616; l8432 = l8438;
    }
    at = 11617; markSet(rt, false, "circle", {at: l8416, radius: l8429, fill: l8432});
    at = 11618; if (rt.truthy(l8406)) {
      at = 11619; l8440 = field(rt, l8409, "points");
      at = 11620; l8439 = l8440;
    } else {
      at = 11621; l8441 = K[3230];
      at = 11622; l8439 = l8441;
    }
    at = 11623; l8442 = K[3231];
    at = 11624; l8443 = rt.sub(l8413, l8442);
    at = 11625; l8444 = K[3232];
    at = 11626; l8445 = rt.max(l8444, l8443);
    at = 11627; l8446 = K[3233];
    at = 11628; l8447 = rt.array_slice(l8439, l8446, l8445);
    at = 11629; if (rt.truthy(l8406)) {
      at = 11630; l8449 = field(rt, l8409, "points");
      at = 11631; l8448 = l8449;
    } else {
      at = 11632; l8450 = K[3234];
      at = 11633; l8448 = l8450;
    }
    at = 11634; l8451 = K[3235];
    at = 11635; l8452 = rt.gt(l8413, l8451);
    at = 11636; if (rt.truthy(l8452)) {
      at = 11637; l8454 = K[3236];
      at = 11638; l8453 = l8454;
    } else {
      at = 11639; l8455 = K[3237];
      at = 11640; l8453 = l8455;
    }
    at = 11641; l8456 = K[3238];
    at = 11642; l8457 = rt.sub(l8413, l8456);
    at = 11643; l8458 = K[3239];
    at = 11644; l8459 = rt.max(l8458, l8457);
    at = 11645; l8460 = rt.array_slice(l8448, l8453, l8459);
    at = 11646; if (rt.truthy(l8406)) {
      at = 11647; l8462 = field(rt, l8409, "color");
      at = 11648; l8461 = l8462;
    } else {
      at = 11649; l8463 = K[3240];
      at = 11650; l8461 = l8463;
    }
    at = 11651; l8464 = K[3241];
    at = 11652; l8465 = rt.sequence_get(l8464, l8461);
    at = 11653; if (rt.truthy(l8406)) {
      at = 11654; l8467 = field(rt, l8409, "width");
      at = 11655; l8466 = l8467;
    } else {
      at = 11656; l8468 = K[3242];
      at = 11657; l8466 = l8468;
    }
    at = 11658; markSet(rt, true, "line", {from: l8447, to: l8460, color: l8465, width: l8466});
    at = 11659; l8469 = field(rt, l15, "strokes");
    at = 11660; l8470 = rt.sequence_length(l8469);
    at = 11661; l8471 = K[3243];
    at = 11662; l8472 = rt.lt(l8471, l8470);
    at = 11663; if (rt.truthy(l8472)) {
      at = 11664; l8473 = field(rt, l15, "strokes");
      at = 11665; l8474 = K[3244];
      at = 11666; l8475 = rt.sequence_get(l8473, l8474);
    }
    at = 11667; if (rt.truthy(l8472)) {
      at = 11668; l8477 = field(rt, l8475, "points");
      at = 11669; l8476 = l8477;
    } else {
      at = 11670; l8478 = K[3245];
      at = 11671; l8476 = l8478;
    }
    at = 11672; l8479 = rt.array_count(l8476);
    at = 11673; l8480 = K[3246];
    at = 11674; l8481 = rt.gt(l8479, l8480);
    at = 11675; if (rt.truthy(l8481)) {
      at = 11676; if (rt.truthy(l8472)) {
        at = 11677; l8484 = field(rt, l8475, "points");
        at = 11678; l8483 = l8484;
      } else {
        at = 11679; l8485 = K[3247];
        at = 11680; l8483 = l8485;
      }
      at = 11681; l8486 = K[3248];
      at = 11682; l8487 = rt.array_nth(l8483, l8486);
      at = 11683; l8482 = l8487;
    } else {
      at = 11684; l8488 = K[3249];
      at = 11685; l8482 = l8488;
    }
    at = 11686; if (rt.truthy(l8472)) {
      at = 11687; l8490 = field(rt, l8475, "width");
      at = 11688; l8489 = l8490;
    } else {
      at = 11689; l8491 = K[3250];
      at = 11690; l8489 = l8491;
    }
    at = 11691; l8492 = K[3251];
    at = 11692; l8493 = rt.int_div(l8489, l8492);
    at = 11693; l8494 = K[3252];
    at = 11694; l8495 = rt.max(l8494, l8493);
    at = 11695; l8496 = K[3253];
    at = 11696; l8497 = rt.gt(l8479, l8496);
    at = 11697; if (rt.truthy(l8497)) {
      at = 11698; if (rt.truthy(l8472)) {
        at = 11699; l8500 = field(rt, l8475, "color");
        at = 11700; l8499 = l8500;
      } else {
        at = 11701; l8501 = K[3254];
        at = 11702; l8499 = l8501;
      }
      at = 11703; l8502 = K[3255];
      at = 11704; l8503 = rt.sequence_get(l8502, l8499);
      at = 11705; l8498 = l8503;
    } else {
      at = 11706; l8504 = K[3256];
      at = 11707; l8498 = l8504;
    }
    at = 11708; markSet(rt, false, "circle", {at: l8482, radius: l8495, fill: l8498});
    at = 11709; if (rt.truthy(l8472)) {
      at = 11710; l8506 = field(rt, l8475, "points");
      at = 11711; l8505 = l8506;
    } else {
      at = 11712; l8507 = K[3257];
      at = 11713; l8505 = l8507;
    }
    at = 11714; l8508 = K[3258];
    at = 11715; l8509 = rt.sub(l8479, l8508);
    at = 11716; l8510 = K[3259];
    at = 11717; l8511 = rt.max(l8510, l8509);
    at = 11718; l8512 = K[3260];
    at = 11719; l8513 = rt.array_slice(l8505, l8512, l8511);
    at = 11720; if (rt.truthy(l8472)) {
      at = 11721; l8515 = field(rt, l8475, "points");
      at = 11722; l8514 = l8515;
    } else {
      at = 11723; l8516 = K[3261];
      at = 11724; l8514 = l8516;
    }
    at = 11725; l8517 = K[3262];
    at = 11726; l8518 = rt.gt(l8479, l8517);
    at = 11727; if (rt.truthy(l8518)) {
      at = 11728; l8520 = K[3263];
      at = 11729; l8519 = l8520;
    } else {
      at = 11730; l8521 = K[3264];
      at = 11731; l8519 = l8521;
    }
    at = 11732; l8522 = K[3265];
    at = 11733; l8523 = rt.sub(l8479, l8522);
    at = 11734; l8524 = K[3266];
    at = 11735; l8525 = rt.max(l8524, l8523);
    at = 11736; l8526 = rt.array_slice(l8514, l8519, l8525);
    at = 11737; if (rt.truthy(l8472)) {
      at = 11738; l8528 = field(rt, l8475, "color");
      at = 11739; l8527 = l8528;
    } else {
      at = 11740; l8529 = K[3267];
      at = 11741; l8527 = l8529;
    }
    at = 11742; l8530 = K[3268];
    at = 11743; l8531 = rt.sequence_get(l8530, l8527);
    at = 11744; if (rt.truthy(l8472)) {
      at = 11745; l8533 = field(rt, l8475, "width");
      at = 11746; l8532 = l8533;
    } else {
      at = 11747; l8534 = K[3269];
      at = 11748; l8532 = l8534;
    }
    at = 11749; markSet(rt, true, "line", {from: l8513, to: l8526, color: l8531, width: l8532});
    at = 11750; l8535 = field(rt, l15, "strokes");
    at = 11751; l8536 = rt.sequence_length(l8535);
    at = 11752; l8537 = K[3270];
    at = 11753; l8538 = rt.lt(l8537, l8536);
    at = 11754; if (rt.truthy(l8538)) {
      at = 11755; l8539 = field(rt, l15, "strokes");
      at = 11756; l8540 = K[3271];
      at = 11757; l8541 = rt.sequence_get(l8539, l8540);
    }
    at = 11758; if (rt.truthy(l8538)) {
      at = 11759; l8543 = field(rt, l8541, "points");
      at = 11760; l8542 = l8543;
    } else {
      at = 11761; l8544 = K[3272];
      at = 11762; l8542 = l8544;
    }
    at = 11763; l8545 = rt.array_count(l8542);
    at = 11764; l8546 = K[3273];
    at = 11765; l8547 = rt.gt(l8545, l8546);
    at = 11766; if (rt.truthy(l8547)) {
      at = 11767; if (rt.truthy(l8538)) {
        at = 11768; l8550 = field(rt, l8541, "points");
        at = 11769; l8549 = l8550;
      } else {
        at = 11770; l8551 = K[3274];
        at = 11771; l8549 = l8551;
      }
      at = 11772; l8552 = K[3275];
      at = 11773; l8553 = rt.array_nth(l8549, l8552);
      at = 11774; l8548 = l8553;
    } else {
      at = 11775; l8554 = K[3276];
      at = 11776; l8548 = l8554;
    }
    at = 11777; if (rt.truthy(l8538)) {
      at = 11778; l8556 = field(rt, l8541, "width");
      at = 11779; l8555 = l8556;
    } else {
      at = 11780; l8557 = K[3277];
      at = 11781; l8555 = l8557;
    }
    at = 11782; l8558 = K[3278];
    at = 11783; l8559 = rt.int_div(l8555, l8558);
    at = 11784; l8560 = K[3279];
    at = 11785; l8561 = rt.max(l8560, l8559);
    at = 11786; l8562 = K[3280];
    at = 11787; l8563 = rt.gt(l8545, l8562);
    at = 11788; if (rt.truthy(l8563)) {
      at = 11789; if (rt.truthy(l8538)) {
        at = 11790; l8566 = field(rt, l8541, "color");
        at = 11791; l8565 = l8566;
      } else {
        at = 11792; l8567 = K[3281];
        at = 11793; l8565 = l8567;
      }
      at = 11794; l8568 = K[3282];
      at = 11795; l8569 = rt.sequence_get(l8568, l8565);
      at = 11796; l8564 = l8569;
    } else {
      at = 11797; l8570 = K[3283];
      at = 11798; l8564 = l8570;
    }
    at = 11799; markSet(rt, false, "circle", {at: l8548, radius: l8561, fill: l8564});
    at = 11800; if (rt.truthy(l8538)) {
      at = 11801; l8572 = field(rt, l8541, "points");
      at = 11802; l8571 = l8572;
    } else {
      at = 11803; l8573 = K[3284];
      at = 11804; l8571 = l8573;
    }
    at = 11805; l8574 = K[3285];
    at = 11806; l8575 = rt.sub(l8545, l8574);
    at = 11807; l8576 = K[3286];
    at = 11808; l8577 = rt.max(l8576, l8575);
    at = 11809; l8578 = K[3287];
    at = 11810; l8579 = rt.array_slice(l8571, l8578, l8577);
    at = 11811; if (rt.truthy(l8538)) {
      at = 11812; l8581 = field(rt, l8541, "points");
      at = 11813; l8580 = l8581;
    } else {
      at = 11814; l8582 = K[3288];
      at = 11815; l8580 = l8582;
    }
    at = 11816; l8583 = K[3289];
    at = 11817; l8584 = rt.gt(l8545, l8583);
    at = 11818; if (rt.truthy(l8584)) {
      at = 11819; l8586 = K[3290];
      at = 11820; l8585 = l8586;
    } else {
      at = 11821; l8587 = K[3291];
      at = 11822; l8585 = l8587;
    }
    at = 11823; l8588 = K[3292];
    at = 11824; l8589 = rt.sub(l8545, l8588);
    at = 11825; l8590 = K[3293];
    at = 11826; l8591 = rt.max(l8590, l8589);
    at = 11827; l8592 = rt.array_slice(l8580, l8585, l8591);
    at = 11828; if (rt.truthy(l8538)) {
      at = 11829; l8594 = field(rt, l8541, "color");
      at = 11830; l8593 = l8594;
    } else {
      at = 11831; l8595 = K[3294];
      at = 11832; l8593 = l8595;
    }
    at = 11833; l8596 = K[3295];
    at = 11834; l8597 = rt.sequence_get(l8596, l8593);
    at = 11835; if (rt.truthy(l8538)) {
      at = 11836; l8599 = field(rt, l8541, "width");
      at = 11837; l8598 = l8599;
    } else {
      at = 11838; l8600 = K[3296];
      at = 11839; l8598 = l8600;
    }
    at = 11840; markSet(rt, true, "line", {from: l8579, to: l8592, color: l8597, width: l8598});
    at = 11841; l8601 = field(rt, l15, "strokes");
    at = 11842; l8602 = rt.sequence_length(l8601);
    at = 11843; l8603 = K[3297];
    at = 11844; l8604 = rt.lt(l8603, l8602);
    at = 11845; if (rt.truthy(l8604)) {
      at = 11846; l8605 = field(rt, l15, "strokes");
      at = 11847; l8606 = K[3298];
      at = 11848; l8607 = rt.sequence_get(l8605, l8606);
    }
    at = 11849; if (rt.truthy(l8604)) {
      at = 11850; l8609 = field(rt, l8607, "points");
      at = 11851; l8608 = l8609;
    } else {
      at = 11852; l8610 = K[3299];
      at = 11853; l8608 = l8610;
    }
    at = 11854; l8611 = rt.array_count(l8608);
    at = 11855; l8612 = K[3300];
    at = 11856; l8613 = rt.gt(l8611, l8612);
    at = 11857; if (rt.truthy(l8613)) {
      at = 11858; if (rt.truthy(l8604)) {
        at = 11859; l8616 = field(rt, l8607, "points");
        at = 11860; l8615 = l8616;
      } else {
        at = 11861; l8617 = K[3301];
        at = 11862; l8615 = l8617;
      }
      at = 11863; l8618 = K[3302];
      at = 11864; l8619 = rt.array_nth(l8615, l8618);
      at = 11865; l8614 = l8619;
    } else {
      at = 11866; l8620 = K[3303];
      at = 11867; l8614 = l8620;
    }
    at = 11868; if (rt.truthy(l8604)) {
      at = 11869; l8622 = field(rt, l8607, "width");
      at = 11870; l8621 = l8622;
    } else {
      at = 11871; l8623 = K[3304];
      at = 11872; l8621 = l8623;
    }
    at = 11873; l8624 = K[3305];
    at = 11874; l8625 = rt.int_div(l8621, l8624);
    at = 11875; l8626 = K[3306];
    at = 11876; l8627 = rt.max(l8626, l8625);
    at = 11877; l8628 = K[3307];
    at = 11878; l8629 = rt.gt(l8611, l8628);
    at = 11879; if (rt.truthy(l8629)) {
      at = 11880; if (rt.truthy(l8604)) {
        at = 11881; l8632 = field(rt, l8607, "color");
        at = 11882; l8631 = l8632;
      } else {
        at = 11883; l8633 = K[3308];
        at = 11884; l8631 = l8633;
      }
      at = 11885; l8634 = K[3309];
      at = 11886; l8635 = rt.sequence_get(l8634, l8631);
      at = 11887; l8630 = l8635;
    } else {
      at = 11888; l8636 = K[3310];
      at = 11889; l8630 = l8636;
    }
    at = 11890; markSet(rt, false, "circle", {at: l8614, radius: l8627, fill: l8630});
    at = 11891; if (rt.truthy(l8604)) {
      at = 11892; l8638 = field(rt, l8607, "points");
      at = 11893; l8637 = l8638;
    } else {
      at = 11894; l8639 = K[3311];
      at = 11895; l8637 = l8639;
    }
    at = 11896; l8640 = K[3312];
    at = 11897; l8641 = rt.sub(l8611, l8640);
    at = 11898; l8642 = K[3313];
    at = 11899; l8643 = rt.max(l8642, l8641);
    at = 11900; l8644 = K[3314];
    at = 11901; l8645 = rt.array_slice(l8637, l8644, l8643);
    at = 11902; if (rt.truthy(l8604)) {
      at = 11903; l8647 = field(rt, l8607, "points");
      at = 11904; l8646 = l8647;
    } else {
      at = 11905; l8648 = K[3315];
      at = 11906; l8646 = l8648;
    }
    at = 11907; l8649 = K[3316];
    at = 11908; l8650 = rt.gt(l8611, l8649);
    at = 11909; if (rt.truthy(l8650)) {
      at = 11910; l8652 = K[3317];
      at = 11911; l8651 = l8652;
    } else {
      at = 11912; l8653 = K[3318];
      at = 11913; l8651 = l8653;
    }
    at = 11914; l8654 = K[3319];
    at = 11915; l8655 = rt.sub(l8611, l8654);
    at = 11916; l8656 = K[3320];
    at = 11917; l8657 = rt.max(l8656, l8655);
    at = 11918; l8658 = rt.array_slice(l8646, l8651, l8657);
    at = 11919; if (rt.truthy(l8604)) {
      at = 11920; l8660 = field(rt, l8607, "color");
      at = 11921; l8659 = l8660;
    } else {
      at = 11922; l8661 = K[3321];
      at = 11923; l8659 = l8661;
    }
    at = 11924; l8662 = K[3322];
    at = 11925; l8663 = rt.sequence_get(l8662, l8659);
    at = 11926; if (rt.truthy(l8604)) {
      at = 11927; l8665 = field(rt, l8607, "width");
      at = 11928; l8664 = l8665;
    } else {
      at = 11929; l8666 = K[3323];
      at = 11930; l8664 = l8666;
    }
    at = 11931; markSet(rt, true, "line", {from: l8645, to: l8658, color: l8663, width: l8664});
    at = 11932; l8667 = field(rt, l15, "strokes");
    at = 11933; l8668 = rt.sequence_length(l8667);
    at = 11934; l8669 = K[3324];
    at = 11935; l8670 = rt.lt(l8669, l8668);
    at = 11936; if (rt.truthy(l8670)) {
      at = 11937; l8671 = field(rt, l15, "strokes");
      at = 11938; l8672 = K[3325];
      at = 11939; l8673 = rt.sequence_get(l8671, l8672);
    }
    at = 11940; if (rt.truthy(l8670)) {
      at = 11941; l8675 = field(rt, l8673, "points");
      at = 11942; l8674 = l8675;
    } else {
      at = 11943; l8676 = K[3326];
      at = 11944; l8674 = l8676;
    }
    at = 11945; l8677 = rt.array_count(l8674);
    at = 11946; l8678 = K[3327];
    at = 11947; l8679 = rt.gt(l8677, l8678);
    at = 11948; if (rt.truthy(l8679)) {
      at = 11949; if (rt.truthy(l8670)) {
        at = 11950; l8682 = field(rt, l8673, "points");
        at = 11951; l8681 = l8682;
      } else {
        at = 11952; l8683 = K[3328];
        at = 11953; l8681 = l8683;
      }
      at = 11954; l8684 = K[3329];
      at = 11955; l8685 = rt.array_nth(l8681, l8684);
      at = 11956; l8680 = l8685;
    } else {
      at = 11957; l8686 = K[3330];
      at = 11958; l8680 = l8686;
    }
    at = 11959; if (rt.truthy(l8670)) {
      at = 11960; l8688 = field(rt, l8673, "width");
      at = 11961; l8687 = l8688;
    } else {
      at = 11962; l8689 = K[3331];
      at = 11963; l8687 = l8689;
    }
    at = 11964; l8690 = K[3332];
    at = 11965; l8691 = rt.int_div(l8687, l8690);
    at = 11966; l8692 = K[3333];
    at = 11967; l8693 = rt.max(l8692, l8691);
    at = 11968; l8694 = K[3334];
    at = 11969; l8695 = rt.gt(l8677, l8694);
    at = 11970; if (rt.truthy(l8695)) {
      at = 11971; if (rt.truthy(l8670)) {
        at = 11972; l8698 = field(rt, l8673, "color");
        at = 11973; l8697 = l8698;
      } else {
        at = 11974; l8699 = K[3335];
        at = 11975; l8697 = l8699;
      }
      at = 11976; l8700 = K[3336];
      at = 11977; l8701 = rt.sequence_get(l8700, l8697);
      at = 11978; l8696 = l8701;
    } else {
      at = 11979; l8702 = K[3337];
      at = 11980; l8696 = l8702;
    }
    at = 11981; markSet(rt, false, "circle", {at: l8680, radius: l8693, fill: l8696});
    at = 11982; if (rt.truthy(l8670)) {
      at = 11983; l8704 = field(rt, l8673, "points");
      at = 11984; l8703 = l8704;
    } else {
      at = 11985; l8705 = K[3338];
      at = 11986; l8703 = l8705;
    }
    at = 11987; l8706 = K[3339];
    at = 11988; l8707 = rt.sub(l8677, l8706);
    at = 11989; l8708 = K[3340];
    at = 11990; l8709 = rt.max(l8708, l8707);
    at = 11991; l8710 = K[3341];
    at = 11992; l8711 = rt.array_slice(l8703, l8710, l8709);
    at = 11993; if (rt.truthy(l8670)) {
      at = 11994; l8713 = field(rt, l8673, "points");
      at = 11995; l8712 = l8713;
    } else {
      at = 11996; l8714 = K[3342];
      at = 11997; l8712 = l8714;
    }
    at = 11998; l8715 = K[3343];
    at = 11999; l8716 = rt.gt(l8677, l8715);
    at = 12000; if (rt.truthy(l8716)) {
      at = 12001; l8718 = K[3344];
      at = 12002; l8717 = l8718;
    } else {
      at = 12003; l8719 = K[3345];
      at = 12004; l8717 = l8719;
    }
    at = 12005; l8720 = K[3346];
    at = 12006; l8721 = rt.sub(l8677, l8720);
    at = 12007; l8722 = K[3347];
    at = 12008; l8723 = rt.max(l8722, l8721);
    at = 12009; l8724 = rt.array_slice(l8712, l8717, l8723);
    at = 12010; if (rt.truthy(l8670)) {
      at = 12011; l8726 = field(rt, l8673, "color");
      at = 12012; l8725 = l8726;
    } else {
      at = 12013; l8727 = K[3348];
      at = 12014; l8725 = l8727;
    }
    at = 12015; l8728 = K[3349];
    at = 12016; l8729 = rt.sequence_get(l8728, l8725);
    at = 12017; if (rt.truthy(l8670)) {
      at = 12018; l8731 = field(rt, l8673, "width");
      at = 12019; l8730 = l8731;
    } else {
      at = 12020; l8732 = K[3350];
      at = 12021; l8730 = l8732;
    }
    at = 12022; markSet(rt, true, "line", {from: l8711, to: l8724, color: l8729, width: l8730});
    at = 12023; l8733 = field(rt, l15, "strokes");
    at = 12024; l8734 = rt.sequence_length(l8733);
    at = 12025; l8735 = K[3351];
    at = 12026; l8736 = rt.lt(l8735, l8734);
    at = 12027; if (rt.truthy(l8736)) {
      at = 12028; l8737 = field(rt, l15, "strokes");
      at = 12029; l8738 = K[3352];
      at = 12030; l8739 = rt.sequence_get(l8737, l8738);
    }
    at = 12031; if (rt.truthy(l8736)) {
      at = 12032; l8741 = field(rt, l8739, "points");
      at = 12033; l8740 = l8741;
    } else {
      at = 12034; l8742 = K[3353];
      at = 12035; l8740 = l8742;
    }
    at = 12036; l8743 = rt.array_count(l8740);
    at = 12037; l8744 = K[3354];
    at = 12038; l8745 = rt.gt(l8743, l8744);
    at = 12039; if (rt.truthy(l8745)) {
      at = 12040; if (rt.truthy(l8736)) {
        at = 12041; l8748 = field(rt, l8739, "points");
        at = 12042; l8747 = l8748;
      } else {
        at = 12043; l8749 = K[3355];
        at = 12044; l8747 = l8749;
      }
      at = 12045; l8750 = K[3356];
      at = 12046; l8751 = rt.array_nth(l8747, l8750);
      at = 12047; l8746 = l8751;
    } else {
      at = 12048; l8752 = K[3357];
      at = 12049; l8746 = l8752;
    }
    at = 12050; if (rt.truthy(l8736)) {
      at = 12051; l8754 = field(rt, l8739, "width");
      at = 12052; l8753 = l8754;
    } else {
      at = 12053; l8755 = K[3358];
      at = 12054; l8753 = l8755;
    }
    at = 12055; l8756 = K[3359];
    at = 12056; l8757 = rt.int_div(l8753, l8756);
    at = 12057; l8758 = K[3360];
    at = 12058; l8759 = rt.max(l8758, l8757);
    at = 12059; l8760 = K[3361];
    at = 12060; l8761 = rt.gt(l8743, l8760);
    at = 12061; if (rt.truthy(l8761)) {
      at = 12062; if (rt.truthy(l8736)) {
        at = 12063; l8764 = field(rt, l8739, "color");
        at = 12064; l8763 = l8764;
      } else {
        at = 12065; l8765 = K[3362];
        at = 12066; l8763 = l8765;
      }
      at = 12067; l8766 = K[3363];
      at = 12068; l8767 = rt.sequence_get(l8766, l8763);
      at = 12069; l8762 = l8767;
    } else {
      at = 12070; l8768 = K[3364];
      at = 12071; l8762 = l8768;
    }
    at = 12072; markSet(rt, false, "circle", {at: l8746, radius: l8759, fill: l8762});
    at = 12073; if (rt.truthy(l8736)) {
      at = 12074; l8770 = field(rt, l8739, "points");
      at = 12075; l8769 = l8770;
    } else {
      at = 12076; l8771 = K[3365];
      at = 12077; l8769 = l8771;
    }
    at = 12078; l8772 = K[3366];
    at = 12079; l8773 = rt.sub(l8743, l8772);
    at = 12080; l8774 = K[3367];
    at = 12081; l8775 = rt.max(l8774, l8773);
    at = 12082; l8776 = K[3368];
    at = 12083; l8777 = rt.array_slice(l8769, l8776, l8775);
    at = 12084; if (rt.truthy(l8736)) {
      at = 12085; l8779 = field(rt, l8739, "points");
      at = 12086; l8778 = l8779;
    } else {
      at = 12087; l8780 = K[3369];
      at = 12088; l8778 = l8780;
    }
    at = 12089; l8781 = K[3370];
    at = 12090; l8782 = rt.gt(l8743, l8781);
    at = 12091; if (rt.truthy(l8782)) {
      at = 12092; l8784 = K[3371];
      at = 12093; l8783 = l8784;
    } else {
      at = 12094; l8785 = K[3372];
      at = 12095; l8783 = l8785;
    }
    at = 12096; l8786 = K[3373];
    at = 12097; l8787 = rt.sub(l8743, l8786);
    at = 12098; l8788 = K[3374];
    at = 12099; l8789 = rt.max(l8788, l8787);
    at = 12100; l8790 = rt.array_slice(l8778, l8783, l8789);
    at = 12101; if (rt.truthy(l8736)) {
      at = 12102; l8792 = field(rt, l8739, "color");
      at = 12103; l8791 = l8792;
    } else {
      at = 12104; l8793 = K[3375];
      at = 12105; l8791 = l8793;
    }
    at = 12106; l8794 = K[3376];
    at = 12107; l8795 = rt.sequence_get(l8794, l8791);
    at = 12108; if (rt.truthy(l8736)) {
      at = 12109; l8797 = field(rt, l8739, "width");
      at = 12110; l8796 = l8797;
    } else {
      at = 12111; l8798 = K[3377];
      at = 12112; l8796 = l8798;
    }
    at = 12113; markSet(rt, true, "line", {from: l8777, to: l8790, color: l8795, width: l8796});
    at = 12114; l8799 = field(rt, l15, "strokes");
    at = 12115; l8800 = rt.sequence_length(l8799);
    at = 12116; l8801 = K[3378];
    at = 12117; l8802 = rt.lt(l8801, l8800);
    at = 12118; if (rt.truthy(l8802)) {
      at = 12119; l8803 = field(rt, l15, "strokes");
      at = 12120; l8804 = K[3379];
      at = 12121; l8805 = rt.sequence_get(l8803, l8804);
    }
    at = 12122; if (rt.truthy(l8802)) {
      at = 12123; l8807 = field(rt, l8805, "points");
      at = 12124; l8806 = l8807;
    } else {
      at = 12125; l8808 = K[3380];
      at = 12126; l8806 = l8808;
    }
    at = 12127; l8809 = rt.array_count(l8806);
    at = 12128; l8810 = K[3381];
    at = 12129; l8811 = rt.gt(l8809, l8810);
    at = 12130; if (rt.truthy(l8811)) {
      at = 12131; if (rt.truthy(l8802)) {
        at = 12132; l8814 = field(rt, l8805, "points");
        at = 12133; l8813 = l8814;
      } else {
        at = 12134; l8815 = K[3382];
        at = 12135; l8813 = l8815;
      }
      at = 12136; l8816 = K[3383];
      at = 12137; l8817 = rt.array_nth(l8813, l8816);
      at = 12138; l8812 = l8817;
    } else {
      at = 12139; l8818 = K[3384];
      at = 12140; l8812 = l8818;
    }
    at = 12141; if (rt.truthy(l8802)) {
      at = 12142; l8820 = field(rt, l8805, "width");
      at = 12143; l8819 = l8820;
    } else {
      at = 12144; l8821 = K[3385];
      at = 12145; l8819 = l8821;
    }
    at = 12146; l8822 = K[3386];
    at = 12147; l8823 = rt.int_div(l8819, l8822);
    at = 12148; l8824 = K[3387];
    at = 12149; l8825 = rt.max(l8824, l8823);
    at = 12150; l8826 = K[3388];
    at = 12151; l8827 = rt.gt(l8809, l8826);
    at = 12152; if (rt.truthy(l8827)) {
      at = 12153; if (rt.truthy(l8802)) {
        at = 12154; l8830 = field(rt, l8805, "color");
        at = 12155; l8829 = l8830;
      } else {
        at = 12156; l8831 = K[3389];
        at = 12157; l8829 = l8831;
      }
      at = 12158; l8832 = K[3390];
      at = 12159; l8833 = rt.sequence_get(l8832, l8829);
      at = 12160; l8828 = l8833;
    } else {
      at = 12161; l8834 = K[3391];
      at = 12162; l8828 = l8834;
    }
    at = 12163; markSet(rt, false, "circle", {at: l8812, radius: l8825, fill: l8828});
    at = 12164; if (rt.truthy(l8802)) {
      at = 12165; l8836 = field(rt, l8805, "points");
      at = 12166; l8835 = l8836;
    } else {
      at = 12167; l8837 = K[3392];
      at = 12168; l8835 = l8837;
    }
    at = 12169; l8838 = K[3393];
    at = 12170; l8839 = rt.sub(l8809, l8838);
    at = 12171; l8840 = K[3394];
    at = 12172; l8841 = rt.max(l8840, l8839);
    at = 12173; l8842 = K[3395];
    at = 12174; l8843 = rt.array_slice(l8835, l8842, l8841);
    at = 12175; if (rt.truthy(l8802)) {
      at = 12176; l8845 = field(rt, l8805, "points");
      at = 12177; l8844 = l8845;
    } else {
      at = 12178; l8846 = K[3396];
      at = 12179; l8844 = l8846;
    }
    at = 12180; l8847 = K[3397];
    at = 12181; l8848 = rt.gt(l8809, l8847);
    at = 12182; if (rt.truthy(l8848)) {
      at = 12183; l8850 = K[3398];
      at = 12184; l8849 = l8850;
    } else {
      at = 12185; l8851 = K[3399];
      at = 12186; l8849 = l8851;
    }
    at = 12187; l8852 = K[3400];
    at = 12188; l8853 = rt.sub(l8809, l8852);
    at = 12189; l8854 = K[3401];
    at = 12190; l8855 = rt.max(l8854, l8853);
    at = 12191; l8856 = rt.array_slice(l8844, l8849, l8855);
    at = 12192; if (rt.truthy(l8802)) {
      at = 12193; l8858 = field(rt, l8805, "color");
      at = 12194; l8857 = l8858;
    } else {
      at = 12195; l8859 = K[3402];
      at = 12196; l8857 = l8859;
    }
    at = 12197; l8860 = K[3403];
    at = 12198; l8861 = rt.sequence_get(l8860, l8857);
    at = 12199; if (rt.truthy(l8802)) {
      at = 12200; l8863 = field(rt, l8805, "width");
      at = 12201; l8862 = l8863;
    } else {
      at = 12202; l8864 = K[3404];
      at = 12203; l8862 = l8864;
    }
    at = 12204; markSet(rt, true, "line", {from: l8843, to: l8856, color: l8861, width: l8862});
    at = 12205; l8865 = field(rt, l15, "strokes");
    at = 12206; l8866 = rt.sequence_length(l8865);
    at = 12207; l8867 = K[3405];
    at = 12208; l8868 = rt.lt(l8867, l8866);
    at = 12209; if (rt.truthy(l8868)) {
      at = 12210; l8869 = field(rt, l15, "strokes");
      at = 12211; l8870 = K[3406];
      at = 12212; l8871 = rt.sequence_get(l8869, l8870);
    }
    at = 12213; if (rt.truthy(l8868)) {
      at = 12214; l8873 = field(rt, l8871, "points");
      at = 12215; l8872 = l8873;
    } else {
      at = 12216; l8874 = K[3407];
      at = 12217; l8872 = l8874;
    }
    at = 12218; l8875 = rt.array_count(l8872);
    at = 12219; l8876 = K[3408];
    at = 12220; l8877 = rt.gt(l8875, l8876);
    at = 12221; if (rt.truthy(l8877)) {
      at = 12222; if (rt.truthy(l8868)) {
        at = 12223; l8880 = field(rt, l8871, "points");
        at = 12224; l8879 = l8880;
      } else {
        at = 12225; l8881 = K[3409];
        at = 12226; l8879 = l8881;
      }
      at = 12227; l8882 = K[3410];
      at = 12228; l8883 = rt.array_nth(l8879, l8882);
      at = 12229; l8878 = l8883;
    } else {
      at = 12230; l8884 = K[3411];
      at = 12231; l8878 = l8884;
    }
    at = 12232; if (rt.truthy(l8868)) {
      at = 12233; l8886 = field(rt, l8871, "width");
      at = 12234; l8885 = l8886;
    } else {
      at = 12235; l8887 = K[3412];
      at = 12236; l8885 = l8887;
    }
    at = 12237; l8888 = K[3413];
    at = 12238; l8889 = rt.int_div(l8885, l8888);
    at = 12239; l8890 = K[3414];
    at = 12240; l8891 = rt.max(l8890, l8889);
    at = 12241; l8892 = K[3415];
    at = 12242; l8893 = rt.gt(l8875, l8892);
    at = 12243; if (rt.truthy(l8893)) {
      at = 12244; if (rt.truthy(l8868)) {
        at = 12245; l8896 = field(rt, l8871, "color");
        at = 12246; l8895 = l8896;
      } else {
        at = 12247; l8897 = K[3416];
        at = 12248; l8895 = l8897;
      }
      at = 12249; l8898 = K[3417];
      at = 12250; l8899 = rt.sequence_get(l8898, l8895);
      at = 12251; l8894 = l8899;
    } else {
      at = 12252; l8900 = K[3418];
      at = 12253; l8894 = l8900;
    }
    at = 12254; markSet(rt, false, "circle", {at: l8878, radius: l8891, fill: l8894});
    at = 12255; if (rt.truthy(l8868)) {
      at = 12256; l8902 = field(rt, l8871, "points");
      at = 12257; l8901 = l8902;
    } else {
      at = 12258; l8903 = K[3419];
      at = 12259; l8901 = l8903;
    }
    at = 12260; l8904 = K[3420];
    at = 12261; l8905 = rt.sub(l8875, l8904);
    at = 12262; l8906 = K[3421];
    at = 12263; l8907 = rt.max(l8906, l8905);
    at = 12264; l8908 = K[3422];
    at = 12265; l8909 = rt.array_slice(l8901, l8908, l8907);
    at = 12266; if (rt.truthy(l8868)) {
      at = 12267; l8911 = field(rt, l8871, "points");
      at = 12268; l8910 = l8911;
    } else {
      at = 12269; l8912 = K[3423];
      at = 12270; l8910 = l8912;
    }
    at = 12271; l8913 = K[3424];
    at = 12272; l8914 = rt.gt(l8875, l8913);
    at = 12273; if (rt.truthy(l8914)) {
      at = 12274; l8916 = K[3425];
      at = 12275; l8915 = l8916;
    } else {
      at = 12276; l8917 = K[3426];
      at = 12277; l8915 = l8917;
    }
    at = 12278; l8918 = K[3427];
    at = 12279; l8919 = rt.sub(l8875, l8918);
    at = 12280; l8920 = K[3428];
    at = 12281; l8921 = rt.max(l8920, l8919);
    at = 12282; l8922 = rt.array_slice(l8910, l8915, l8921);
    at = 12283; if (rt.truthy(l8868)) {
      at = 12284; l8924 = field(rt, l8871, "color");
      at = 12285; l8923 = l8924;
    } else {
      at = 12286; l8925 = K[3429];
      at = 12287; l8923 = l8925;
    }
    at = 12288; l8926 = K[3430];
    at = 12289; l8927 = rt.sequence_get(l8926, l8923);
    at = 12290; if (rt.truthy(l8868)) {
      at = 12291; l8929 = field(rt, l8871, "width");
      at = 12292; l8928 = l8929;
    } else {
      at = 12293; l8930 = K[3431];
      at = 12294; l8928 = l8930;
    }
    at = 12295; markSet(rt, true, "line", {from: l8909, to: l8922, color: l8927, width: l8928});
    at = 12296; l8931 = field(rt, l15, "strokes");
    at = 12297; l8932 = rt.sequence_length(l8931);
    at = 12298; l8933 = K[3432];
    at = 12299; l8934 = rt.lt(l8933, l8932);
    at = 12300; if (rt.truthy(l8934)) {
      at = 12301; l8935 = field(rt, l15, "strokes");
      at = 12302; l8936 = K[3433];
      at = 12303; l8937 = rt.sequence_get(l8935, l8936);
    }
    at = 12304; if (rt.truthy(l8934)) {
      at = 12305; l8939 = field(rt, l8937, "points");
      at = 12306; l8938 = l8939;
    } else {
      at = 12307; l8940 = K[3434];
      at = 12308; l8938 = l8940;
    }
    at = 12309; l8941 = rt.array_count(l8938);
    at = 12310; l8942 = K[3435];
    at = 12311; l8943 = rt.gt(l8941, l8942);
    at = 12312; if (rt.truthy(l8943)) {
      at = 12313; if (rt.truthy(l8934)) {
        at = 12314; l8946 = field(rt, l8937, "points");
        at = 12315; l8945 = l8946;
      } else {
        at = 12316; l8947 = K[3436];
        at = 12317; l8945 = l8947;
      }
      at = 12318; l8948 = K[3437];
      at = 12319; l8949 = rt.array_nth(l8945, l8948);
      at = 12320; l8944 = l8949;
    } else {
      at = 12321; l8950 = K[3438];
      at = 12322; l8944 = l8950;
    }
    at = 12323; if (rt.truthy(l8934)) {
      at = 12324; l8952 = field(rt, l8937, "width");
      at = 12325; l8951 = l8952;
    } else {
      at = 12326; l8953 = K[3439];
      at = 12327; l8951 = l8953;
    }
    at = 12328; l8954 = K[3440];
    at = 12329; l8955 = rt.int_div(l8951, l8954);
    at = 12330; l8956 = K[3441];
    at = 12331; l8957 = rt.max(l8956, l8955);
    at = 12332; l8958 = K[3442];
    at = 12333; l8959 = rt.gt(l8941, l8958);
    at = 12334; if (rt.truthy(l8959)) {
      at = 12335; if (rt.truthy(l8934)) {
        at = 12336; l8962 = field(rt, l8937, "color");
        at = 12337; l8961 = l8962;
      } else {
        at = 12338; l8963 = K[3443];
        at = 12339; l8961 = l8963;
      }
      at = 12340; l8964 = K[3444];
      at = 12341; l8965 = rt.sequence_get(l8964, l8961);
      at = 12342; l8960 = l8965;
    } else {
      at = 12343; l8966 = K[3445];
      at = 12344; l8960 = l8966;
    }
    at = 12345; markSet(rt, false, "circle", {at: l8944, radius: l8957, fill: l8960});
    at = 12346; if (rt.truthy(l8934)) {
      at = 12347; l8968 = field(rt, l8937, "points");
      at = 12348; l8967 = l8968;
    } else {
      at = 12349; l8969 = K[3446];
      at = 12350; l8967 = l8969;
    }
    at = 12351; l8970 = K[3447];
    at = 12352; l8971 = rt.sub(l8941, l8970);
    at = 12353; l8972 = K[3448];
    at = 12354; l8973 = rt.max(l8972, l8971);
    at = 12355; l8974 = K[3449];
    at = 12356; l8975 = rt.array_slice(l8967, l8974, l8973);
    at = 12357; if (rt.truthy(l8934)) {
      at = 12358; l8977 = field(rt, l8937, "points");
      at = 12359; l8976 = l8977;
    } else {
      at = 12360; l8978 = K[3450];
      at = 12361; l8976 = l8978;
    }
    at = 12362; l8979 = K[3451];
    at = 12363; l8980 = rt.gt(l8941, l8979);
    at = 12364; if (rt.truthy(l8980)) {
      at = 12365; l8982 = K[3452];
      at = 12366; l8981 = l8982;
    } else {
      at = 12367; l8983 = K[3453];
      at = 12368; l8981 = l8983;
    }
    at = 12369; l8984 = K[3454];
    at = 12370; l8985 = rt.sub(l8941, l8984);
    at = 12371; l8986 = K[3455];
    at = 12372; l8987 = rt.max(l8986, l8985);
    at = 12373; l8988 = rt.array_slice(l8976, l8981, l8987);
    at = 12374; if (rt.truthy(l8934)) {
      at = 12375; l8990 = field(rt, l8937, "color");
      at = 12376; l8989 = l8990;
    } else {
      at = 12377; l8991 = K[3456];
      at = 12378; l8989 = l8991;
    }
    at = 12379; l8992 = K[3457];
    at = 12380; l8993 = rt.sequence_get(l8992, l8989);
    at = 12381; if (rt.truthy(l8934)) {
      at = 12382; l8995 = field(rt, l8937, "width");
      at = 12383; l8994 = l8995;
    } else {
      at = 12384; l8996 = K[3458];
      at = 12385; l8994 = l8996;
    }
    at = 12386; markSet(rt, true, "line", {from: l8975, to: l8988, color: l8993, width: l8994});
    at = 12387; l8997 = field(rt, l15, "strokes");
    at = 12388; l8998 = rt.sequence_length(l8997);
    at = 12389; l8999 = K[3459];
    at = 12390; l9000 = rt.lt(l8999, l8998);
    at = 12391; if (rt.truthy(l9000)) {
      at = 12392; l9001 = field(rt, l15, "strokes");
      at = 12393; l9002 = K[3460];
      at = 12394; l9003 = rt.sequence_get(l9001, l9002);
    }
    at = 12395; if (rt.truthy(l9000)) {
      at = 12396; l9005 = field(rt, l9003, "points");
      at = 12397; l9004 = l9005;
    } else {
      at = 12398; l9006 = K[3461];
      at = 12399; l9004 = l9006;
    }
    at = 12400; l9007 = rt.array_count(l9004);
    at = 12401; l9008 = K[3462];
    at = 12402; l9009 = rt.gt(l9007, l9008);
    at = 12403; if (rt.truthy(l9009)) {
      at = 12404; if (rt.truthy(l9000)) {
        at = 12405; l9012 = field(rt, l9003, "points");
        at = 12406; l9011 = l9012;
      } else {
        at = 12407; l9013 = K[3463];
        at = 12408; l9011 = l9013;
      }
      at = 12409; l9014 = K[3464];
      at = 12410; l9015 = rt.array_nth(l9011, l9014);
      at = 12411; l9010 = l9015;
    } else {
      at = 12412; l9016 = K[3465];
      at = 12413; l9010 = l9016;
    }
    at = 12414; if (rt.truthy(l9000)) {
      at = 12415; l9018 = field(rt, l9003, "width");
      at = 12416; l9017 = l9018;
    } else {
      at = 12417; l9019 = K[3466];
      at = 12418; l9017 = l9019;
    }
    at = 12419; l9020 = K[3467];
    at = 12420; l9021 = rt.int_div(l9017, l9020);
    at = 12421; l9022 = K[3468];
    at = 12422; l9023 = rt.max(l9022, l9021);
    at = 12423; l9024 = K[3469];
    at = 12424; l9025 = rt.gt(l9007, l9024);
    at = 12425; if (rt.truthy(l9025)) {
      at = 12426; if (rt.truthy(l9000)) {
        at = 12427; l9028 = field(rt, l9003, "color");
        at = 12428; l9027 = l9028;
      } else {
        at = 12429; l9029 = K[3470];
        at = 12430; l9027 = l9029;
      }
      at = 12431; l9030 = K[3471];
      at = 12432; l9031 = rt.sequence_get(l9030, l9027);
      at = 12433; l9026 = l9031;
    } else {
      at = 12434; l9032 = K[3472];
      at = 12435; l9026 = l9032;
    }
    at = 12436; markSet(rt, false, "circle", {at: l9010, radius: l9023, fill: l9026});
    at = 12437; if (rt.truthy(l9000)) {
      at = 12438; l9034 = field(rt, l9003, "points");
      at = 12439; l9033 = l9034;
    } else {
      at = 12440; l9035 = K[3473];
      at = 12441; l9033 = l9035;
    }
    at = 12442; l9036 = K[3474];
    at = 12443; l9037 = rt.sub(l9007, l9036);
    at = 12444; l9038 = K[3475];
    at = 12445; l9039 = rt.max(l9038, l9037);
    at = 12446; l9040 = K[3476];
    at = 12447; l9041 = rt.array_slice(l9033, l9040, l9039);
    at = 12448; if (rt.truthy(l9000)) {
      at = 12449; l9043 = field(rt, l9003, "points");
      at = 12450; l9042 = l9043;
    } else {
      at = 12451; l9044 = K[3477];
      at = 12452; l9042 = l9044;
    }
    at = 12453; l9045 = K[3478];
    at = 12454; l9046 = rt.gt(l9007, l9045);
    at = 12455; if (rt.truthy(l9046)) {
      at = 12456; l9048 = K[3479];
      at = 12457; l9047 = l9048;
    } else {
      at = 12458; l9049 = K[3480];
      at = 12459; l9047 = l9049;
    }
    at = 12460; l9050 = K[3481];
    at = 12461; l9051 = rt.sub(l9007, l9050);
    at = 12462; l9052 = K[3482];
    at = 12463; l9053 = rt.max(l9052, l9051);
    at = 12464; l9054 = rt.array_slice(l9042, l9047, l9053);
    at = 12465; if (rt.truthy(l9000)) {
      at = 12466; l9056 = field(rt, l9003, "color");
      at = 12467; l9055 = l9056;
    } else {
      at = 12468; l9057 = K[3483];
      at = 12469; l9055 = l9057;
    }
    at = 12470; l9058 = K[3484];
    at = 12471; l9059 = rt.sequence_get(l9058, l9055);
    at = 12472; if (rt.truthy(l9000)) {
      at = 12473; l9061 = field(rt, l9003, "width");
      at = 12474; l9060 = l9061;
    } else {
      at = 12475; l9062 = K[3485];
      at = 12476; l9060 = l9062;
    }
    at = 12477; markSet(rt, true, "line", {from: l9041, to: l9054, color: l9059, width: l9060});
    at = 12478; l9063 = field(rt, l15, "strokes");
    at = 12479; l9064 = rt.sequence_length(l9063);
    at = 12480; l9065 = K[3486];
    at = 12481; l9066 = rt.lt(l9065, l9064);
    at = 12482; if (rt.truthy(l9066)) {
      at = 12483; l9067 = field(rt, l15, "strokes");
      at = 12484; l9068 = K[3487];
      at = 12485; l9069 = rt.sequence_get(l9067, l9068);
    }
    at = 12486; if (rt.truthy(l9066)) {
      at = 12487; l9071 = field(rt, l9069, "points");
      at = 12488; l9070 = l9071;
    } else {
      at = 12489; l9072 = K[3488];
      at = 12490; l9070 = l9072;
    }
    at = 12491; l9073 = rt.array_count(l9070);
    at = 12492; l9074 = K[3489];
    at = 12493; l9075 = rt.gt(l9073, l9074);
    at = 12494; if (rt.truthy(l9075)) {
      at = 12495; if (rt.truthy(l9066)) {
        at = 12496; l9078 = field(rt, l9069, "points");
        at = 12497; l9077 = l9078;
      } else {
        at = 12498; l9079 = K[3490];
        at = 12499; l9077 = l9079;
      }
      at = 12500; l9080 = K[3491];
      at = 12501; l9081 = rt.array_nth(l9077, l9080);
      at = 12502; l9076 = l9081;
    } else {
      at = 12503; l9082 = K[3492];
      at = 12504; l9076 = l9082;
    }
    at = 12505; if (rt.truthy(l9066)) {
      at = 12506; l9084 = field(rt, l9069, "width");
      at = 12507; l9083 = l9084;
    } else {
      at = 12508; l9085 = K[3493];
      at = 12509; l9083 = l9085;
    }
    at = 12510; l9086 = K[3494];
    at = 12511; l9087 = rt.int_div(l9083, l9086);
    at = 12512; l9088 = K[3495];
    at = 12513; l9089 = rt.max(l9088, l9087);
    at = 12514; l9090 = K[3496];
    at = 12515; l9091 = rt.gt(l9073, l9090);
    at = 12516; if (rt.truthy(l9091)) {
      at = 12517; if (rt.truthy(l9066)) {
        at = 12518; l9094 = field(rt, l9069, "color");
        at = 12519; l9093 = l9094;
      } else {
        at = 12520; l9095 = K[3497];
        at = 12521; l9093 = l9095;
      }
      at = 12522; l9096 = K[3498];
      at = 12523; l9097 = rt.sequence_get(l9096, l9093);
      at = 12524; l9092 = l9097;
    } else {
      at = 12525; l9098 = K[3499];
      at = 12526; l9092 = l9098;
    }
    at = 12527; markSet(rt, false, "circle", {at: l9076, radius: l9089, fill: l9092});
    at = 12528; if (rt.truthy(l9066)) {
      at = 12529; l9100 = field(rt, l9069, "points");
      at = 12530; l9099 = l9100;
    } else {
      at = 12531; l9101 = K[3500];
      at = 12532; l9099 = l9101;
    }
    at = 12533; l9102 = K[3501];
    at = 12534; l9103 = rt.sub(l9073, l9102);
    at = 12535; l9104 = K[3502];
    at = 12536; l9105 = rt.max(l9104, l9103);
    at = 12537; l9106 = K[3503];
    at = 12538; l9107 = rt.array_slice(l9099, l9106, l9105);
    at = 12539; if (rt.truthy(l9066)) {
      at = 12540; l9109 = field(rt, l9069, "points");
      at = 12541; l9108 = l9109;
    } else {
      at = 12542; l9110 = K[3504];
      at = 12543; l9108 = l9110;
    }
    at = 12544; l9111 = K[3505];
    at = 12545; l9112 = rt.gt(l9073, l9111);
    at = 12546; if (rt.truthy(l9112)) {
      at = 12547; l9114 = K[3506];
      at = 12548; l9113 = l9114;
    } else {
      at = 12549; l9115 = K[3507];
      at = 12550; l9113 = l9115;
    }
    at = 12551; l9116 = K[3508];
    at = 12552; l9117 = rt.sub(l9073, l9116);
    at = 12553; l9118 = K[3509];
    at = 12554; l9119 = rt.max(l9118, l9117);
    at = 12555; l9120 = rt.array_slice(l9108, l9113, l9119);
    at = 12556; if (rt.truthy(l9066)) {
      at = 12557; l9122 = field(rt, l9069, "color");
      at = 12558; l9121 = l9122;
    } else {
      at = 12559; l9123 = K[3510];
      at = 12560; l9121 = l9123;
    }
    at = 12561; l9124 = K[3511];
    at = 12562; l9125 = rt.sequence_get(l9124, l9121);
    at = 12563; if (rt.truthy(l9066)) {
      at = 12564; l9127 = field(rt, l9069, "width");
      at = 12565; l9126 = l9127;
    } else {
      at = 12566; l9128 = K[3512];
      at = 12567; l9126 = l9128;
    }
    at = 12568; markSet(rt, true, "line", {from: l9107, to: l9120, color: l9125, width: l9126});
    at = 12569; l9129 = field(rt, l15, "strokes");
    at = 12570; l9130 = rt.sequence_length(l9129);
    at = 12571; l9131 = K[3513];
    at = 12572; l9132 = rt.lt(l9131, l9130);
    at = 12573; if (rt.truthy(l9132)) {
      at = 12574; l9133 = field(rt, l15, "strokes");
      at = 12575; l9134 = K[3514];
      at = 12576; l9135 = rt.sequence_get(l9133, l9134);
    }
    at = 12577; if (rt.truthy(l9132)) {
      at = 12578; l9137 = field(rt, l9135, "points");
      at = 12579; l9136 = l9137;
    } else {
      at = 12580; l9138 = K[3515];
      at = 12581; l9136 = l9138;
    }
    at = 12582; l9139 = rt.array_count(l9136);
    at = 12583; l9140 = K[3516];
    at = 12584; l9141 = rt.gt(l9139, l9140);
    at = 12585; if (rt.truthy(l9141)) {
      at = 12586; if (rt.truthy(l9132)) {
        at = 12587; l9144 = field(rt, l9135, "points");
        at = 12588; l9143 = l9144;
      } else {
        at = 12589; l9145 = K[3517];
        at = 12590; l9143 = l9145;
      }
      at = 12591; l9146 = K[3518];
      at = 12592; l9147 = rt.array_nth(l9143, l9146);
      at = 12593; l9142 = l9147;
    } else {
      at = 12594; l9148 = K[3519];
      at = 12595; l9142 = l9148;
    }
    at = 12596; if (rt.truthy(l9132)) {
      at = 12597; l9150 = field(rt, l9135, "width");
      at = 12598; l9149 = l9150;
    } else {
      at = 12599; l9151 = K[3520];
      at = 12600; l9149 = l9151;
    }
    at = 12601; l9152 = K[3521];
    at = 12602; l9153 = rt.int_div(l9149, l9152);
    at = 12603; l9154 = K[3522];
    at = 12604; l9155 = rt.max(l9154, l9153);
    at = 12605; l9156 = K[3523];
    at = 12606; l9157 = rt.gt(l9139, l9156);
    at = 12607; if (rt.truthy(l9157)) {
      at = 12608; if (rt.truthy(l9132)) {
        at = 12609; l9160 = field(rt, l9135, "color");
        at = 12610; l9159 = l9160;
      } else {
        at = 12611; l9161 = K[3524];
        at = 12612; l9159 = l9161;
      }
      at = 12613; l9162 = K[3525];
      at = 12614; l9163 = rt.sequence_get(l9162, l9159);
      at = 12615; l9158 = l9163;
    } else {
      at = 12616; l9164 = K[3526];
      at = 12617; l9158 = l9164;
    }
    at = 12618; markSet(rt, false, "circle", {at: l9142, radius: l9155, fill: l9158});
    at = 12619; if (rt.truthy(l9132)) {
      at = 12620; l9166 = field(rt, l9135, "points");
      at = 12621; l9165 = l9166;
    } else {
      at = 12622; l9167 = K[3527];
      at = 12623; l9165 = l9167;
    }
    at = 12624; l9168 = K[3528];
    at = 12625; l9169 = rt.sub(l9139, l9168);
    at = 12626; l9170 = K[3529];
    at = 12627; l9171 = rt.max(l9170, l9169);
    at = 12628; l9172 = K[3530];
    at = 12629; l9173 = rt.array_slice(l9165, l9172, l9171);
    at = 12630; if (rt.truthy(l9132)) {
      at = 12631; l9175 = field(rt, l9135, "points");
      at = 12632; l9174 = l9175;
    } else {
      at = 12633; l9176 = K[3531];
      at = 12634; l9174 = l9176;
    }
    at = 12635; l9177 = K[3532];
    at = 12636; l9178 = rt.gt(l9139, l9177);
    at = 12637; if (rt.truthy(l9178)) {
      at = 12638; l9180 = K[3533];
      at = 12639; l9179 = l9180;
    } else {
      at = 12640; l9181 = K[3534];
      at = 12641; l9179 = l9181;
    }
    at = 12642; l9182 = K[3535];
    at = 12643; l9183 = rt.sub(l9139, l9182);
    at = 12644; l9184 = K[3536];
    at = 12645; l9185 = rt.max(l9184, l9183);
    at = 12646; l9186 = rt.array_slice(l9174, l9179, l9185);
    at = 12647; if (rt.truthy(l9132)) {
      at = 12648; l9188 = field(rt, l9135, "color");
      at = 12649; l9187 = l9188;
    } else {
      at = 12650; l9189 = K[3537];
      at = 12651; l9187 = l9189;
    }
    at = 12652; l9190 = K[3538];
    at = 12653; l9191 = rt.sequence_get(l9190, l9187);
    at = 12654; if (rt.truthy(l9132)) {
      at = 12655; l9193 = field(rt, l9135, "width");
      at = 12656; l9192 = l9193;
    } else {
      at = 12657; l9194 = K[3539];
      at = 12658; l9192 = l9194;
    }
    at = 12659; markSet(rt, true, "line", {from: l9173, to: l9186, color: l9191, width: l9192});
    at = 12660; l9195 = field(rt, l15, "strokes");
    at = 12661; l9196 = rt.sequence_length(l9195);
    at = 12662; l9197 = K[3540];
    at = 12663; l9198 = rt.lt(l9197, l9196);
    at = 12664; if (rt.truthy(l9198)) {
      at = 12665; l9199 = field(rt, l15, "strokes");
      at = 12666; l9200 = K[3541];
      at = 12667; l9201 = rt.sequence_get(l9199, l9200);
    }
    at = 12668; if (rt.truthy(l9198)) {
      at = 12669; l9203 = field(rt, l9201, "points");
      at = 12670; l9202 = l9203;
    } else {
      at = 12671; l9204 = K[3542];
      at = 12672; l9202 = l9204;
    }
    at = 12673; l9205 = rt.array_count(l9202);
    at = 12674; l9206 = K[3543];
    at = 12675; l9207 = rt.gt(l9205, l9206);
    at = 12676; if (rt.truthy(l9207)) {
      at = 12677; if (rt.truthy(l9198)) {
        at = 12678; l9210 = field(rt, l9201, "points");
        at = 12679; l9209 = l9210;
      } else {
        at = 12680; l9211 = K[3544];
        at = 12681; l9209 = l9211;
      }
      at = 12682; l9212 = K[3545];
      at = 12683; l9213 = rt.array_nth(l9209, l9212);
      at = 12684; l9208 = l9213;
    } else {
      at = 12685; l9214 = K[3546];
      at = 12686; l9208 = l9214;
    }
    at = 12687; if (rt.truthy(l9198)) {
      at = 12688; l9216 = field(rt, l9201, "width");
      at = 12689; l9215 = l9216;
    } else {
      at = 12690; l9217 = K[3547];
      at = 12691; l9215 = l9217;
    }
    at = 12692; l9218 = K[3548];
    at = 12693; l9219 = rt.int_div(l9215, l9218);
    at = 12694; l9220 = K[3549];
    at = 12695; l9221 = rt.max(l9220, l9219);
    at = 12696; l9222 = K[3550];
    at = 12697; l9223 = rt.gt(l9205, l9222);
    at = 12698; if (rt.truthy(l9223)) {
      at = 12699; if (rt.truthy(l9198)) {
        at = 12700; l9226 = field(rt, l9201, "color");
        at = 12701; l9225 = l9226;
      } else {
        at = 12702; l9227 = K[3551];
        at = 12703; l9225 = l9227;
      }
      at = 12704; l9228 = K[3552];
      at = 12705; l9229 = rt.sequence_get(l9228, l9225);
      at = 12706; l9224 = l9229;
    } else {
      at = 12707; l9230 = K[3553];
      at = 12708; l9224 = l9230;
    }
    at = 12709; markSet(rt, false, "circle", {at: l9208, radius: l9221, fill: l9224});
    at = 12710; if (rt.truthy(l9198)) {
      at = 12711; l9232 = field(rt, l9201, "points");
      at = 12712; l9231 = l9232;
    } else {
      at = 12713; l9233 = K[3554];
      at = 12714; l9231 = l9233;
    }
    at = 12715; l9234 = K[3555];
    at = 12716; l9235 = rt.sub(l9205, l9234);
    at = 12717; l9236 = K[3556];
    at = 12718; l9237 = rt.max(l9236, l9235);
    at = 12719; l9238 = K[3557];
    at = 12720; l9239 = rt.array_slice(l9231, l9238, l9237);
    at = 12721; if (rt.truthy(l9198)) {
      at = 12722; l9241 = field(rt, l9201, "points");
      at = 12723; l9240 = l9241;
    } else {
      at = 12724; l9242 = K[3558];
      at = 12725; l9240 = l9242;
    }
    at = 12726; l9243 = K[3559];
    at = 12727; l9244 = rt.gt(l9205, l9243);
    at = 12728; if (rt.truthy(l9244)) {
      at = 12729; l9246 = K[3560];
      at = 12730; l9245 = l9246;
    } else {
      at = 12731; l9247 = K[3561];
      at = 12732; l9245 = l9247;
    }
    at = 12733; l9248 = K[3562];
    at = 12734; l9249 = rt.sub(l9205, l9248);
    at = 12735; l9250 = K[3563];
    at = 12736; l9251 = rt.max(l9250, l9249);
    at = 12737; l9252 = rt.array_slice(l9240, l9245, l9251);
    at = 12738; if (rt.truthy(l9198)) {
      at = 12739; l9254 = field(rt, l9201, "color");
      at = 12740; l9253 = l9254;
    } else {
      at = 12741; l9255 = K[3564];
      at = 12742; l9253 = l9255;
    }
    at = 12743; l9256 = K[3565];
    at = 12744; l9257 = rt.sequence_get(l9256, l9253);
    at = 12745; if (rt.truthy(l9198)) {
      at = 12746; l9259 = field(rt, l9201, "width");
      at = 12747; l9258 = l9259;
    } else {
      at = 12748; l9260 = K[3566];
      at = 12749; l9258 = l9260;
    }
    at = 12750; markSet(rt, true, "line", {from: l9239, to: l9252, color: l9257, width: l9258});
    at = 12751; l9261 = field(rt, l15, "strokes");
    at = 12752; l9262 = rt.sequence_length(l9261);
    at = 12753; l9263 = K[3567];
    at = 12754; l9264 = rt.lt(l9263, l9262);
    at = 12755; if (rt.truthy(l9264)) {
      at = 12756; l9265 = field(rt, l15, "strokes");
      at = 12757; l9266 = K[3568];
      at = 12758; l9267 = rt.sequence_get(l9265, l9266);
    }
    at = 12759; if (rt.truthy(l9264)) {
      at = 12760; l9269 = field(rt, l9267, "points");
      at = 12761; l9268 = l9269;
    } else {
      at = 12762; l9270 = K[3569];
      at = 12763; l9268 = l9270;
    }
    at = 12764; l9271 = rt.array_count(l9268);
    at = 12765; l9272 = K[3570];
    at = 12766; l9273 = rt.gt(l9271, l9272);
    at = 12767; if (rt.truthy(l9273)) {
      at = 12768; if (rt.truthy(l9264)) {
        at = 12769; l9276 = field(rt, l9267, "points");
        at = 12770; l9275 = l9276;
      } else {
        at = 12771; l9277 = K[3571];
        at = 12772; l9275 = l9277;
      }
      at = 12773; l9278 = K[3572];
      at = 12774; l9279 = rt.array_nth(l9275, l9278);
      at = 12775; l9274 = l9279;
    } else {
      at = 12776; l9280 = K[3573];
      at = 12777; l9274 = l9280;
    }
    at = 12778; if (rt.truthy(l9264)) {
      at = 12779; l9282 = field(rt, l9267, "width");
      at = 12780; l9281 = l9282;
    } else {
      at = 12781; l9283 = K[3574];
      at = 12782; l9281 = l9283;
    }
    at = 12783; l9284 = K[3575];
    at = 12784; l9285 = rt.int_div(l9281, l9284);
    at = 12785; l9286 = K[3576];
    at = 12786; l9287 = rt.max(l9286, l9285);
    at = 12787; l9288 = K[3577];
    at = 12788; l9289 = rt.gt(l9271, l9288);
    at = 12789; if (rt.truthy(l9289)) {
      at = 12790; if (rt.truthy(l9264)) {
        at = 12791; l9292 = field(rt, l9267, "color");
        at = 12792; l9291 = l9292;
      } else {
        at = 12793; l9293 = K[3578];
        at = 12794; l9291 = l9293;
      }
      at = 12795; l9294 = K[3579];
      at = 12796; l9295 = rt.sequence_get(l9294, l9291);
      at = 12797; l9290 = l9295;
    } else {
      at = 12798; l9296 = K[3580];
      at = 12799; l9290 = l9296;
    }
    at = 12800; markSet(rt, false, "circle", {at: l9274, radius: l9287, fill: l9290});
    at = 12801; if (rt.truthy(l9264)) {
      at = 12802; l9298 = field(rt, l9267, "points");
      at = 12803; l9297 = l9298;
    } else {
      at = 12804; l9299 = K[3581];
      at = 12805; l9297 = l9299;
    }
    at = 12806; l9300 = K[3582];
    at = 12807; l9301 = rt.sub(l9271, l9300);
    at = 12808; l9302 = K[3583];
    at = 12809; l9303 = rt.max(l9302, l9301);
    at = 12810; l9304 = K[3584];
    at = 12811; l9305 = rt.array_slice(l9297, l9304, l9303);
    at = 12812; if (rt.truthy(l9264)) {
      at = 12813; l9307 = field(rt, l9267, "points");
      at = 12814; l9306 = l9307;
    } else {
      at = 12815; l9308 = K[3585];
      at = 12816; l9306 = l9308;
    }
    at = 12817; l9309 = K[3586];
    at = 12818; l9310 = rt.gt(l9271, l9309);
    at = 12819; if (rt.truthy(l9310)) {
      at = 12820; l9312 = K[3587];
      at = 12821; l9311 = l9312;
    } else {
      at = 12822; l9313 = K[3588];
      at = 12823; l9311 = l9313;
    }
    at = 12824; l9314 = K[3589];
    at = 12825; l9315 = rt.sub(l9271, l9314);
    at = 12826; l9316 = K[3590];
    at = 12827; l9317 = rt.max(l9316, l9315);
    at = 12828; l9318 = rt.array_slice(l9306, l9311, l9317);
    at = 12829; if (rt.truthy(l9264)) {
      at = 12830; l9320 = field(rt, l9267, "color");
      at = 12831; l9319 = l9320;
    } else {
      at = 12832; l9321 = K[3591];
      at = 12833; l9319 = l9321;
    }
    at = 12834; l9322 = K[3592];
    at = 12835; l9323 = rt.sequence_get(l9322, l9319);
    at = 12836; if (rt.truthy(l9264)) {
      at = 12837; l9325 = field(rt, l9267, "width");
      at = 12838; l9324 = l9325;
    } else {
      at = 12839; l9326 = K[3593];
      at = 12840; l9324 = l9326;
    }
    at = 12841; markSet(rt, true, "line", {from: l9305, to: l9318, color: l9323, width: l9324});
    at = 12842; l9327 = field(rt, l15, "strokes");
    at = 12843; l9328 = rt.sequence_length(l9327);
    at = 12844; l9329 = K[3594];
    at = 12845; l9330 = rt.lt(l9329, l9328);
    at = 12846; if (rt.truthy(l9330)) {
      at = 12847; l9331 = field(rt, l15, "strokes");
      at = 12848; l9332 = K[3595];
      at = 12849; l9333 = rt.sequence_get(l9331, l9332);
    }
    at = 12850; if (rt.truthy(l9330)) {
      at = 12851; l9335 = field(rt, l9333, "points");
      at = 12852; l9334 = l9335;
    } else {
      at = 12853; l9336 = K[3596];
      at = 12854; l9334 = l9336;
    }
    at = 12855; l9337 = rt.array_count(l9334);
    at = 12856; l9338 = K[3597];
    at = 12857; l9339 = rt.gt(l9337, l9338);
    at = 12858; if (rt.truthy(l9339)) {
      at = 12859; if (rt.truthy(l9330)) {
        at = 12860; l9342 = field(rt, l9333, "points");
        at = 12861; l9341 = l9342;
      } else {
        at = 12862; l9343 = K[3598];
        at = 12863; l9341 = l9343;
      }
      at = 12864; l9344 = K[3599];
      at = 12865; l9345 = rt.array_nth(l9341, l9344);
      at = 12866; l9340 = l9345;
    } else {
      at = 12867; l9346 = K[3600];
      at = 12868; l9340 = l9346;
    }
    at = 12869; if (rt.truthy(l9330)) {
      at = 12870; l9348 = field(rt, l9333, "width");
      at = 12871; l9347 = l9348;
    } else {
      at = 12872; l9349 = K[3601];
      at = 12873; l9347 = l9349;
    }
    at = 12874; l9350 = K[3602];
    at = 12875; l9351 = rt.int_div(l9347, l9350);
    at = 12876; l9352 = K[3603];
    at = 12877; l9353 = rt.max(l9352, l9351);
    at = 12878; l9354 = K[3604];
    at = 12879; l9355 = rt.gt(l9337, l9354);
    at = 12880; if (rt.truthy(l9355)) {
      at = 12881; if (rt.truthy(l9330)) {
        at = 12882; l9358 = field(rt, l9333, "color");
        at = 12883; l9357 = l9358;
      } else {
        at = 12884; l9359 = K[3605];
        at = 12885; l9357 = l9359;
      }
      at = 12886; l9360 = K[3606];
      at = 12887; l9361 = rt.sequence_get(l9360, l9357);
      at = 12888; l9356 = l9361;
    } else {
      at = 12889; l9362 = K[3607];
      at = 12890; l9356 = l9362;
    }
    at = 12891; markSet(rt, false, "circle", {at: l9340, radius: l9353, fill: l9356});
    at = 12892; if (rt.truthy(l9330)) {
      at = 12893; l9364 = field(rt, l9333, "points");
      at = 12894; l9363 = l9364;
    } else {
      at = 12895; l9365 = K[3608];
      at = 12896; l9363 = l9365;
    }
    at = 12897; l9366 = K[3609];
    at = 12898; l9367 = rt.sub(l9337, l9366);
    at = 12899; l9368 = K[3610];
    at = 12900; l9369 = rt.max(l9368, l9367);
    at = 12901; l9370 = K[3611];
    at = 12902; l9371 = rt.array_slice(l9363, l9370, l9369);
    at = 12903; if (rt.truthy(l9330)) {
      at = 12904; l9373 = field(rt, l9333, "points");
      at = 12905; l9372 = l9373;
    } else {
      at = 12906; l9374 = K[3612];
      at = 12907; l9372 = l9374;
    }
    at = 12908; l9375 = K[3613];
    at = 12909; l9376 = rt.gt(l9337, l9375);
    at = 12910; if (rt.truthy(l9376)) {
      at = 12911; l9378 = K[3614];
      at = 12912; l9377 = l9378;
    } else {
      at = 12913; l9379 = K[3615];
      at = 12914; l9377 = l9379;
    }
    at = 12915; l9380 = K[3616];
    at = 12916; l9381 = rt.sub(l9337, l9380);
    at = 12917; l9382 = K[3617];
    at = 12918; l9383 = rt.max(l9382, l9381);
    at = 12919; l9384 = rt.array_slice(l9372, l9377, l9383);
    at = 12920; if (rt.truthy(l9330)) {
      at = 12921; l9386 = field(rt, l9333, "color");
      at = 12922; l9385 = l9386;
    } else {
      at = 12923; l9387 = K[3618];
      at = 12924; l9385 = l9387;
    }
    at = 12925; l9388 = K[3619];
    at = 12926; l9389 = rt.sequence_get(l9388, l9385);
    at = 12927; if (rt.truthy(l9330)) {
      at = 12928; l9391 = field(rt, l9333, "width");
      at = 12929; l9390 = l9391;
    } else {
      at = 12930; l9392 = K[3620];
      at = 12931; l9390 = l9392;
    }
    at = 12932; markSet(rt, true, "line", {from: l9371, to: l9384, color: l9389, width: l9390});
    at = 12933; l9393 = field(rt, l15, "active");
    at = 12934; if (rt.truthy(l9393)) {
      at = 12935; l9394 = field(rt, l15, "current");
    }
    at = 12936; if (rt.truthy(l9393)) {
      at = 12937; l9396 = field(rt, l9394, "points");
      at = 12938; l9395 = l9396;
    } else {
      at = 12939; l9397 = K[3621];
      at = 12940; l9395 = l9397;
    }
    at = 12941; l9398 = rt.array_count(l9395);
    at = 12942; l9399 = K[3622];
    at = 12943; l9400 = rt.gt(l9398, l9399);
    at = 12944; if (rt.truthy(l9400)) {
      at = 12945; if (rt.truthy(l9393)) {
        at = 12946; l9403 = field(rt, l9394, "points");
        at = 12947; l9402 = l9403;
      } else {
        at = 12948; l9404 = K[3623];
        at = 12949; l9402 = l9404;
      }
      at = 12950; l9405 = K[3624];
      at = 12951; l9406 = rt.array_nth(l9402, l9405);
      at = 12952; l9401 = l9406;
    } else {
      at = 12953; l9407 = K[3625];
      at = 12954; l9401 = l9407;
    }
    at = 12955; if (rt.truthy(l9393)) {
      at = 12956; l9409 = field(rt, l9394, "width");
      at = 12957; l9408 = l9409;
    } else {
      at = 12958; l9410 = K[3626];
      at = 12959; l9408 = l9410;
    }
    at = 12960; l9411 = K[3627];
    at = 12961; l9412 = rt.int_div(l9408, l9411);
    at = 12962; l9413 = K[3628];
    at = 12963; l9414 = rt.max(l9413, l9412);
    at = 12964; l9415 = K[3629];
    at = 12965; l9416 = rt.gt(l9398, l9415);
    at = 12966; if (rt.truthy(l9416)) {
      at = 12967; if (rt.truthy(l9393)) {
        at = 12968; l9419 = field(rt, l9394, "color");
        at = 12969; l9418 = l9419;
      } else {
        at = 12970; l9420 = K[3630];
        at = 12971; l9418 = l9420;
      }
      at = 12972; l9421 = K[3631];
      at = 12973; l9422 = rt.sequence_get(l9421, l9418);
      at = 12974; l9417 = l9422;
    } else {
      at = 12975; l9423 = K[3632];
      at = 12976; l9417 = l9423;
    }
    at = 12977; markSet(rt, false, "circle", {at: l9401, radius: l9414, fill: l9417});
    at = 12978; if (rt.truthy(l9393)) {
      at = 12979; l9425 = field(rt, l9394, "points");
      at = 12980; l9424 = l9425;
    } else {
      at = 12981; l9426 = K[3633];
      at = 12982; l9424 = l9426;
    }
    at = 12983; l9427 = K[3634];
    at = 12984; l9428 = rt.sub(l9398, l9427);
    at = 12985; l9429 = K[3635];
    at = 12986; l9430 = rt.max(l9429, l9428);
    at = 12987; l9431 = K[3636];
    at = 12988; l9432 = rt.array_slice(l9424, l9431, l9430);
    at = 12989; if (rt.truthy(l9393)) {
      at = 12990; l9434 = field(rt, l9394, "points");
      at = 12991; l9433 = l9434;
    } else {
      at = 12992; l9435 = K[3637];
      at = 12993; l9433 = l9435;
    }
    at = 12994; l9436 = K[3638];
    at = 12995; l9437 = rt.gt(l9398, l9436);
    at = 12996; if (rt.truthy(l9437)) {
      at = 12997; l9439 = K[3639];
      at = 12998; l9438 = l9439;
    } else {
      at = 12999; l9440 = K[3640];
      at = 13000; l9438 = l9440;
    }
    at = 13001; l9441 = K[3641];
    at = 13002; l9442 = rt.sub(l9398, l9441);
    at = 13003; l9443 = K[3642];
    at = 13004; l9444 = rt.max(l9443, l9442);
    at = 13005; l9445 = rt.array_slice(l9433, l9438, l9444);
    at = 13006; if (rt.truthy(l9393)) {
      at = 13007; l9447 = field(rt, l9394, "color");
      at = 13008; l9446 = l9447;
    } else {
      at = 13009; l9448 = K[3643];
      at = 13010; l9446 = l9448;
    }
    at = 13011; l9449 = K[3644];
    at = 13012; l9450 = rt.sequence_get(l9449, l9446);
    at = 13013; if (rt.truthy(l9393)) {
      at = 13014; l9452 = field(rt, l9394, "width");
      at = 13015; l9451 = l9452;
    } else {
      at = 13016; l9453 = K[3645];
      at = 13017; l9451 = l9453;
    }
    at = 13018; markSet(rt, true, "line", {from: l9432, to: l9445, color: l9450, width: l9451});
    at = 13019; l9454 = K[3646];
    at = 13020; l9455 = BigInt(input.size[0]);
    at = 13021; l9456 = K[3647];
    at = 13022; l9457 = K[3648];
    at = 13023; l9458 = Float64Array.of(rt.float(l9455), rt.float(l9456), rt.float(l9457));
    at = 13024; l9459 = K[3649];
    at = 13025; markSet(rt, false, "rect", {at: l9454, size: l9458, fill: l9459});
    at = 13026; l9460 = K[3650];
    at = 13027; l9461 = K[3651];
    at = 13028; l9462 = K[3652];
    at = 13029; l9463 = K[3653];
    at = 13030; l9464 = K[3654];
    at = 13031; pathOf(rt, "rounded_rect", {at: l9460, size: l9461, radius: l9462, fill: l9463, stroke: l9464});
    at = 13032; l9465 = K[3655];
    at = 13033; l9466 = K[3656];
    at = 13034; l9467 = K[3657];
    at = 13035; l9468 = K[3658];
    at = 13036; if (typeof l9466 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9466 !== "") { const [x, y] = point(rt, l9465); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9467)), color: ink(rt, l9468), text: l9466}); }
    at = 13037; l9469 = K[3659];
    at = 13038; l9470 = K[3660];
    at = 13039; l9471 = K[3661];
    at = 13040; l9472 = K[3662];
    at = 13041; l9473 = K[3663];
    at = 13042; pathOf(rt, "rounded_rect", {at: l9469, size: l9470, radius: l9471, fill: l9472, stroke: l9473});
    at = 13043; l9474 = K[3664];
    at = 13044; l9475 = K[3665];
    at = 13045; l9476 = K[3666];
    at = 13046; l9477 = K[3667];
    at = 13047; if (typeof l9475 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9475 !== "") { const [x, y] = point(rt, l9474); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9476)), color: ink(rt, l9477), text: l9475}); }
    at = 13048; l9478 = K[3668];
    at = 13049; l9479 = K[3669];
    at = 13050; l9480 = K[3670];
    at = 13051; l9481 = K[3671];
    at = 13052; l9482 = K[3672];
    at = 13053; pathOf(rt, "rounded_rect", {at: l9478, size: l9479, radius: l9480, fill: l9481, stroke: l9482});
    at = 13054; l9483 = K[3673];
    at = 13055; l9484 = K[3674];
    at = 13056; l9485 = K[3675];
    at = 13057; l9486 = K[3676];
    at = 13058; if (typeof l9484 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9484 !== "") { const [x, y] = point(rt, l9483); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9485)), color: ink(rt, l9486), text: l9484}); }
    at = 13059; l9487 = K[3677];
    at = 13060; l9488 = field(rt, l15, "color");
    at = 13061; l9489 = K[3678];
    at = 13062; l9490 = rt.eq(l9489, l9488);
    at = 13063; if (rt.truthy(l9490)) {
      at = 13064; l9492 = K[3679];
      at = 13065; l9491 = l9492;
    } else {
      at = 13066; l9493 = K[3680];
      at = 13067; l9491 = l9493;
    }
    at = 13068; l9494 = K[3681];
    at = 13069; l9495 = K[3682];
    at = 13070; markSet(rt, false, "circle", {at: l9487, radius: l9491, fill: l9494, stroke: l9495});
    at = 13071; l9496 = K[3683];
    at = 13072; l9497 = field(rt, l15, "color");
    at = 13073; l9498 = K[3684];
    at = 13074; l9499 = rt.eq(l9498, l9497);
    at = 13075; if (rt.truthy(l9499)) {
      at = 13076; l9501 = K[3685];
      at = 13077; l9500 = l9501;
    } else {
      at = 13078; l9502 = K[3686];
      at = 13079; l9500 = l9502;
    }
    at = 13080; l9503 = K[3687];
    at = 13081; l9504 = K[3688];
    at = 13082; markSet(rt, false, "circle", {at: l9496, radius: l9500, fill: l9503, stroke: l9504});
    at = 13083; l9505 = K[3689];
    at = 13084; l9506 = field(rt, l15, "color");
    at = 13085; l9507 = K[3690];
    at = 13086; l9508 = rt.eq(l9507, l9506);
    at = 13087; if (rt.truthy(l9508)) {
      at = 13088; l9510 = K[3691];
      at = 13089; l9509 = l9510;
    } else {
      at = 13090; l9511 = K[3692];
      at = 13091; l9509 = l9511;
    }
    at = 13092; l9512 = K[3693];
    at = 13093; l9513 = K[3694];
    at = 13094; markSet(rt, false, "circle", {at: l9505, radius: l9509, fill: l9512, stroke: l9513});
    at = 13095; l9514 = K[3695];
    at = 13096; l9515 = field(rt, l15, "color");
    at = 13097; l9516 = K[3696];
    at = 13098; l9517 = rt.eq(l9516, l9515);
    at = 13099; if (rt.truthy(l9517)) {
      at = 13100; l9519 = K[3697];
      at = 13101; l9518 = l9519;
    } else {
      at = 13102; l9520 = K[3698];
      at = 13103; l9518 = l9520;
    }
    at = 13104; l9521 = K[3699];
    at = 13105; l9522 = K[3700];
    at = 13106; markSet(rt, false, "circle", {at: l9514, radius: l9518, fill: l9521, stroke: l9522});
    at = 13107; l9523 = K[3701];
    at = 13108; l9524 = K[3702];
    at = 13109; l9525 = K[3703];
    at = 13110; l9526 = K[3704];
    at = 13111; l9527 = K[3705];
    at = 13112; pathOf(rt, "rounded_rect", {at: l9523, size: l9524, radius: l9525, fill: l9526, stroke: l9527});
    at = 13113; l9528 = K[3706];
    at = 13114; l9529 = K[3707];
    at = 13115; l9530 = K[3708];
    at = 13116; l9531 = K[3709];
    at = 13117; if (typeof l9529 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9529 !== "") { const [x, y] = point(rt, l9528); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9530)), color: ink(rt, l9531), text: l9529}); }
    at = 13118; l9532 = K[3710];
    at = 13119; l9533 = K[3711];
    at = 13120; l9534 = K[3712];
    at = 13121; l9535 = K[3713];
    at = 13122; l9536 = K[3714];
    at = 13123; pathOf(rt, "rounded_rect", {at: l9532, size: l9533, radius: l9534, fill: l9535, stroke: l9536});
    at = 13124; l9537 = K[3715];
    at = 13125; l9538 = K[3716];
    at = 13126; l9539 = K[3717];
    at = 13127; l9540 = K[3718];
    at = 13128; if (typeof l9538 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9538 !== "") { const [x, y] = point(rt, l9537); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9539)), color: ink(rt, l9540), text: l9538}); }
    at = 13129; l9541 = K[3719];
    at = 13130; l9542 = field(rt, l15, "width");
    at = 13131; l9543 = K[3720];
    at = 13132; l9544 = rt.int_div(l9542, l9543);
    at = 13133; l9545 = K[3721];
    at = 13134; l9546 = rt.max(l9545, l9544);
    at = 13135; l9547 = field(rt, l15, "color");
    at = 13136; l9548 = K[3722];
    at = 13137; l9549 = rt.sequence_get(l9548, l9547);
    at = 13138; markSet(rt, false, "circle", {at: l9541, radius: l9546, fill: l9549});
    at = 13139; l9550 = K[3723];
    at = 13140; l9551 = field(rt, l15, "width");
    at = 13141; l9552 = rt.show(l9551);
    at = 13142; l9553 = K[3724];
    at = 13143; l9554 = rt.text_concat(l9552, l9553);
    at = 13144; l9555 = K[3725];
    at = 13145; l9556 = K[3726];
    at = 13146; if (typeof l9554 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9554 !== "") { const [x, y] = point(rt, l9550); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9555)), color: ink(rt, l9556), text: l9554}); }
    at = 13147; l9557 = BigInt(input.size[1]);
    at = 13148; l9558 = K[3727];
    at = 13149; l9559 = rt.sub(l9557, l9558);
    at = 13150; l9560 = K[3728];
    at = 13151; l9561 = K[3729];
    at = 13152; l9562 = Float64Array.of(rt.float(l9560), rt.float(l9559), rt.float(l9561));
    at = 13153; l9563 = field(rt, l15, "status");
    at = 13154; l9564 = K[3730];
    at = 13155; l9565 = K[3731];
    at = 13156; if (typeof l9563 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9563 !== "") { const [x, y] = point(rt, l9562); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9564)), color: ink(rt, l9565), text: l9563}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
