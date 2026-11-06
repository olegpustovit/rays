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
  K ??= ["#0b1020", [], false, 0n, 10n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 10n, "Drag to draw", false, false, 0n, 0n, 4096n, 0n, 0n, "pointer-pressed", "left", 92n, 8n, 70n, 8n, 42n, 0n, [], 0n, 0n, [], 76n, 138n, 8n, 42n, 0n, 1n, 0n, 0n, 144n, 206n, 8n, 42n, 215n, 245n, 8n, 42n, 0, 0n, 253n, 283n, 8n, 42n, 1, 1n, 291n, 321n, 8n, 42n, 2, 2n, 329n, 359n, 8n, 42n, 3, 3n, 8n, 44n, 50n, 84n, 2n, 2n, 52n, 88n, 50n, 84n, 2n, 32n, false, false, false, false, false, false, false, false, "Saved _out/rays-drawing.png", true, false, "Undid the latest stroke", false, "Canvas cleared", 1n, true, "Drawing", "pointer-moved", 4096n, 0n, 1n, 1n, 4n, 1n, "pointer-released", "left", 4096n, 0n, 1n, 1n, 4n, 1n, 128n, 0n, 0n, 0n, 0n, false, "Drag to draw", "pointer-cancelled", "left", "focus-lost", 128n, 0n, 0n, 0n, 0n, false, "Drag to draw", "key-pressed", "c", "C", 0n, [], 0n, 0n, [], "z", "Z", 0n, 1n, 0n, 0n, "[", 2n, 2n, "]", 2n, 32n, "s", "S", "Saved _out/rays-drawing.png", true, false, "Undid the latest stroke", false, "Canvas cleared", 0n, 0n, 1n, 1n, 127n, 127n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 126n, 126n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 125n, 125n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 124n, 124n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 123n, 123n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 122n, 122n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 121n, 121n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 120n, 120n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 119n, 119n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 118n, 118n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 117n, 117n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 116n, 116n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 115n, 115n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 114n, 114n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 113n, 113n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 112n, 112n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 111n, 111n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 110n, 110n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 109n, 109n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 108n, 108n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 107n, 107n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 106n, 106n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 105n, 105n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 104n, 104n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 103n, 103n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 102n, 102n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 101n, 101n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 100n, 100n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 99n, 99n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 98n, 98n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 97n, 97n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 96n, 96n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 95n, 95n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 94n, 94n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 93n, 93n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 92n, 92n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 91n, 91n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 90n, 90n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 89n, 89n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 88n, 88n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 87n, 87n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 86n, 86n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 85n, 85n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 84n, 84n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 83n, 83n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 82n, 82n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 81n, 81n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 80n, 80n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 79n, 79n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 78n, 78n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 77n, 77n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 76n, 76n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 75n, 75n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 74n, 74n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 73n, 73n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 72n, 72n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 71n, 71n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 70n, 70n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 69n, 69n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 68n, 68n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 67n, 67n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 66n, 66n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 65n, 65n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 64n, 64n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 63n, 63n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 62n, 62n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 61n, 61n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 60n, 60n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 59n, 59n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 58n, 58n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 57n, 57n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 56n, 56n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 55n, 55n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 54n, 54n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 53n, 53n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 52n, 52n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 51n, 51n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 50n, 50n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 49n, 49n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 48n, 48n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 47n, 47n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 46n, 46n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 45n, 45n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 44n, 44n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 43n, 43n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 42n, 42n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 41n, 41n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 40n, 40n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 39n, 39n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 38n, 38n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 37n, 37n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 36n, 36n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 35n, 35n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 34n, 34n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 33n, 33n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 32n, 32n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 31n, 31n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 30n, 30n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 29n, 29n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 28n, 28n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 27n, 27n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 26n, 26n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 25n, 25n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 24n, 24n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 23n, 23n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 22n, 22n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 21n, 21n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 20n, 20n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 19n, 19n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 18n, 18n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 17n, 17n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 16n, 16n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 15n, 15n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 14n, 14n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 13n, 13n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 12n, 12n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 11n, 11n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 10n, 10n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 9n, 9n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 8n, 8n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 7n, 7n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 6n, 6n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 5n, 5n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 4n, 4n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 3n, 3n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 2n, 2n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 1n, 1n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, Float64Array.of(0, 0, 0), 10n, 2n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], "#00000000", new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 1n, 0n, 0n, new rt.PArray(3, new Float64Array(rt.data, 0, 0)), 0n, 1n, 0n, 1n, 0n, 0n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], 10n, Float64Array.of(0, 0, 0), 92n, 0n, [0.03529411764705882, 0.054901960784313725, 0.09803921568627451, 0.9607843137254902], Float64Array.of(8, 8, 0), Float64Array.of(62, 34, 0), 6n, "#1f2937", "#475569", Float64Array.of(20, 16, 0), "Clear", 14n, Float64Array.of(1, 1, 1), Float64Array.of(76, 8, 0), Float64Array.of(62, 34, 0), 6n, "#1f2937", "#475569", Float64Array.of(88, 16, 0), "Undo", 14n, Float64Array.of(1, 1, 1), Float64Array.of(144, 8, 0), Float64Array.of(62, 34, 0), 6n, "#1f2937", "#475569", Float64Array.of(156, 16, 0), "Save", 14n, Float64Array.of(1, 1, 1), Float64Array.of(230, 25, 0), 0n, 12n, 9n, "#22d3ee", "#ffffff", Float64Array.of(268, 25, 0), 1n, 12n, 9n, "#fb7185", "#ffffff", Float64Array.of(306, 25, 0), 2n, 12n, 9n, "#fbbf24", "#ffffff", Float64Array.of(344, 25, 0), 3n, 12n, 9n, "#a78bfa", "#ffffff", Float64Array.of(8, 50, 0), Float64Array.of(36, 34, 0), 6n, "#1f2937", "#475569", Float64Array.of(20, 58, 0), "−", 14n, Float64Array.of(1, 1, 1), Float64Array.of(52, 50, 0), Float64Array.of(36, 34, 0), 6n, "#1f2937", "#475569", Float64Array.of(64, 58, 0), "+", 14n, Float64Array.of(1, 1, 1), Float64Array.of(112, 67, 0), 2n, 1n, ["#22d3ee", "#fb7185", "#fbbf24", "#a78bfa"], Float64Array.of(136, 58, 0), " px · draw · S save", 13n, Float64Array.of(1, 1, 1), 26n, 12n, 0n, 13n, [0.8862745098039215, 0.9098039215686274, 0.9411764705882353, 0.7450980392156863]];
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
  let l9552, l9553;
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
                at = 399; l322 = l80;
                at = 401; l324 = K[88];
                at = 402; l323 = l324;
                at = 404; l325 = l26;
                at = 406; l326 = l27;
                at = 408; l328 = K[89];
                at = 409; l327 = l328;
                at = 411; l329 = l30;
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
                  at = 115; l107 = rt.max(l105, l106);
                  at = 116; l108 = rt.min(l107, l104);
                  at = 117; l109 = rt.sub(l104, l108);
                  at = 118; l110 = rt.sequence_slice(l23, l108, l109);
                  at = 119; l111 = rt.sequence_length(l23);
                  at = 120; l112 = K[34];
                  at = 121; l113 = rt.eq(l111, l112);
                  at = 122; if (rt.truthy(l113)) {
                    at = 123; l114 = l110;
                  } else {
                    at = 124; l115 = K[35];
                    at = 125; l116 = rt.sequence_get(l23, l115);
                    at = 126; l117 = rt.sequence_length(l110);
                    at = 127; l118 = rt.sequence_build(l117);
                    at = 128; {
                      const s128 = l110, c128 = Array.isArray(s128) ? s128.length : Number(rt.array_count(s128));
                      if (c128 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                      for (let i128 = 0; i128 < c128; i128++) {
                        l119 = BigInt(i128); l120 = Array.isArray(s128) ? s128[i128] : rt.array_nth(s128, i128);
                        at = 129; store(rt, l118, l119, l120);
                      }
                    }
                    at = 130; l114 = l118;
                  }
                  at = 385; l314 = l114;
                  at = 387; l316 = K[86];
                  at = 388; l315 = l316;
                  at = 390; l317 = l26;
                  at = 392; l318 = l27;
                  at = 394; l320 = K[87];
                  at = 395; l319 = l320;
                  at = 397; l321 = l30;
                } else {
                  at = 131; l121 = field(rt, l51, "x");
                  at = 132; l122 = K[36];
                  at = 133; l123 = rt.ge(l121, l122);
                  at = 134; l124 = field(rt, l51, "x");
                  at = 135; l125 = K[37];
                  at = 136; l126 = rt.lt(l124, l125);
                  at = 137; l127 = rt.and(l123, l126);
                  at = 138; l128 = field(rt, l51, "y");
                  at = 139; l129 = K[38];
                  at = 140; l130 = rt.ge(l128, l129);
                  at = 141; l131 = field(rt, l51, "y");
                  at = 142; l132 = K[39];
                  at = 143; l133 = rt.lt(l131, l132);
                  at = 144; l134 = rt.and(l130, l133);
                  at = 145; l135 = rt.and(l127, l134);
                  at = 146; if (rt.truthy(l135)) {
                    at = 370; l305 = l23;
                    at = 372; l307 = K[83];
                    at = 373; l306 = l307;
                    at = 375; l308 = l26;
                    at = 377; l309 = l27;
                    at = 379; l311 = K[84];
                    at = 380; l310 = l311;
                    at = 382; l313 = K[85];
                    at = 383; l312 = l313;
                  } else {
                    at = 147; l136 = field(rt, l51, "x");
                    at = 148; l137 = K[40];
                    at = 149; l138 = rt.ge(l136, l137);
                    at = 150; l139 = field(rt, l51, "x");
                    at = 151; l140 = K[41];
                    at = 152; l141 = rt.lt(l139, l140);
                    at = 153; l142 = rt.and(l138, l141);
                    at = 154; l143 = field(rt, l51, "y");
                    at = 155; l144 = K[42];
                    at = 156; l145 = rt.ge(l143, l144);
                    at = 157; l146 = field(rt, l51, "y");
                    at = 158; l147 = K[43];
                    at = 159; l148 = rt.lt(l146, l147);
                    at = 160; l149 = rt.and(l145, l148);
                    at = 161; l150 = rt.and(l142, l149);
                    at = 162; if (rt.truthy(l150)) {
                      at = 163; l151 = rt.coerce("float", l26);
                      at = 164; l152 = rt.equal(l26, l151);
                      at = 165; if (rt.truthy(l152)) {
                        at = 166; l154 = K[44];
                        at = 167; l153 = l154;
                      } else {
                        at = 168; l155 = K[45];
                        at = 169; l153 = l155;
                      }
                      at = 357; l298 = l23;
                      at = 359; l300 = K[82];
                      at = 360; l299 = l300;
                      at = 362; l301 = l153;
                      at = 364; l302 = l27;
                      at = 366; l303 = l28;
                      at = 368; l304 = l30;
                    } else {
                      at = 170; l156 = field(rt, l51, "x");
                      at = 171; l157 = K[46];
                      at = 172; l158 = rt.ge(l156, l157);
                      at = 173; l159 = field(rt, l51, "x");
                      at = 174; l160 = K[47];
                      at = 175; l161 = rt.lt(l159, l160);
                      at = 176; l162 = rt.and(l158, l161);
                      at = 177; l163 = field(rt, l51, "y");
                      at = 178; l164 = K[48];
                      at = 179; l165 = rt.ge(l163, l164);
                      at = 180; l166 = field(rt, l51, "y");
                      at = 181; l167 = K[49];
                      at = 182; l168 = rt.lt(l166, l167);
                      at = 183; l169 = rt.and(l165, l168);
                      at = 184; l170 = rt.and(l162, l169);
                      at = 185; if (rt.truthy(l170)) {
                        at = 186; l171 = rt.coerce("float", l26);
                        at = 187; l172 = rt.equal(l26, l171);
                        at = 188; if (rt.truthy(l172)) {
                          at = 189; l174 = K[50];
                          at = 190; l173 = l174;
                        } else {
                          at = 191; l175 = K[51];
                          at = 192; l173 = l175;
                        }
                        at = 344; l291 = l23;
                        at = 346; l293 = K[81];
                        at = 347; l292 = l293;
                        at = 349; l294 = l173;
                        at = 351; l295 = l27;
                        at = 353; l296 = l28;
                        at = 355; l297 = l30;
                      } else {
                        at = 193; l176 = field(rt, l51, "x");
                        at = 194; l177 = K[52];
                        at = 195; l178 = rt.ge(l176, l177);
                        at = 196; l179 = field(rt, l51, "x");
                        at = 197; l180 = K[53];
                        at = 198; l181 = rt.lt(l179, l180);
                        at = 199; l182 = rt.and(l178, l181);
                        at = 200; l183 = field(rt, l51, "y");
                        at = 201; l184 = K[54];
                        at = 202; l185 = rt.ge(l183, l184);
                        at = 203; l186 = field(rt, l51, "y");
                        at = 204; l187 = K[55];
                        at = 205; l188 = rt.lt(l186, l187);
                        at = 206; l189 = rt.and(l185, l188);
                        at = 207; l190 = rt.and(l182, l189);
                        at = 208; if (rt.truthy(l190)) {
                          at = 209; l191 = rt.coerce("float", l26);
                          at = 210; l192 = rt.equal(l26, l191);
                          at = 211; if (rt.truthy(l192)) {
                            at = 212; l194 = K[56];
                            at = 213; l193 = l194;
                          } else {
                            at = 214; l195 = K[57];
                            at = 215; l193 = l195;
                          }
                          at = 331; l284 = l23;
                          at = 333; l286 = K[80];
                          at = 334; l285 = l286;
                          at = 336; l287 = l193;
                          at = 338; l288 = l27;
                          at = 340; l289 = l28;
                          at = 342; l290 = l30;
                        } else {
                          at = 216; l196 = field(rt, l51, "x");
                          at = 217; l197 = K[58];
                          at = 218; l198 = rt.ge(l196, l197);
                          at = 219; l199 = field(rt, l51, "x");
                          at = 220; l200 = K[59];
                          at = 221; l201 = rt.lt(l199, l200);
                          at = 222; l202 = rt.and(l198, l201);
                          at = 223; l203 = field(rt, l51, "y");
                          at = 224; l204 = K[60];
                          at = 225; l205 = rt.ge(l203, l204);
                          at = 226; l206 = field(rt, l51, "y");
                          at = 227; l207 = K[61];
                          at = 228; l208 = rt.lt(l206, l207);
                          at = 229; l209 = rt.and(l205, l208);
                          at = 230; l210 = rt.and(l202, l209);
                          at = 231; if (rt.truthy(l210)) {
                            at = 232; l211 = rt.coerce("float", l26);
                            at = 233; l212 = rt.equal(l26, l211);
                            at = 234; if (rt.truthy(l212)) {
                              at = 235; l214 = K[62];
                              at = 236; l213 = l214;
                            } else {
                              at = 237; l215 = K[63];
                              at = 238; l213 = l215;
                            }
                            at = 318; l277 = l23;
                            at = 320; l279 = K[79];
                            at = 321; l278 = l279;
                            at = 323; l280 = l213;
                            at = 325; l281 = l27;
                            at = 327; l282 = l28;
                            at = 329; l283 = l30;
                          } else {
                            at = 239; l216 = field(rt, l51, "x");
                            at = 240; l217 = K[64];
                            at = 241; l218 = rt.ge(l216, l217);
                            at = 242; l219 = field(rt, l51, "x");
                            at = 243; l220 = K[65];
                            at = 244; l221 = rt.lt(l219, l220);
                            at = 245; l222 = rt.and(l218, l221);
                            at = 246; l223 = field(rt, l51, "y");
                            at = 247; l224 = K[66];
                            at = 248; l225 = rt.ge(l223, l224);
                            at = 249; l226 = field(rt, l51, "y");
                            at = 250; l227 = K[67];
                            at = 251; l228 = rt.lt(l226, l227);
                            at = 252; l229 = rt.and(l225, l228);
                            at = 253; l230 = rt.and(l222, l229);
                            at = 254; if (rt.truthy(l230)) {
                              at = 255; l231 = K[68];
                              at = 256; l232 = rt.sub(l27, l231);
                              at = 257; l233 = K[69];
                              at = 258; l234 = rt.max(l233, l232);
                              at = 259; l235 = rt.coerce("float", l27);
                              at = 260; l236 = rt.equal(l27, l235);
                              at = 261; if (rt.truthy(l236)) {
                                at = 262; l238 = rt.coerce("float", l234);
                                at = 263; l237 = l238;
                              } else {
                                at = 264; l237 = l234;
                              }
                              at = 305; l270 = l23;
                              at = 307; l272 = K[78];
                              at = 308; l271 = l272;
                              at = 310; l273 = l26;
                              at = 312; l274 = l237;
                              at = 314; l275 = l28;
                              at = 316; l276 = l30;
                            } else {
                              at = 265; l239 = field(rt, l51, "x");
                              at = 266; l240 = K[70];
                              at = 267; l241 = rt.ge(l239, l240);
                              at = 268; l242 = field(rt, l51, "x");
                              at = 269; l243 = K[71];
                              at = 270; l244 = rt.lt(l242, l243);
                              at = 271; l245 = rt.and(l241, l244);
                              at = 272; l246 = field(rt, l51, "y");
                              at = 273; l247 = K[72];
                              at = 274; l248 = rt.ge(l246, l247);
                              at = 275; l249 = field(rt, l51, "y");
                              at = 276; l250 = K[73];
                              at = 277; l251 = rt.lt(l249, l250);
                              at = 278; l252 = rt.and(l248, l251);
                              at = 279; l253 = rt.and(l245, l252);
                              at = 280; if (rt.truthy(l253)) {
                                at = 281; l254 = K[74];
                                at = 282; l255 = rt.add(l27, l254);
                                at = 283; l256 = K[75];
                                at = 284; l257 = rt.min(l256, l255);
                                at = 285; l258 = rt.coerce("float", l27);
                                at = 286; l259 = rt.equal(l27, l258);
                                at = 287; if (rt.truthy(l259)) {
                                  at = 288; l261 = rt.coerce("float", l257);
                                  at = 289; l260 = l261;
                                } else {
                                  at = 290; l260 = l257;
                                }
                                at = 291; l262 = l23;
                                at = 293; l264 = K[76];
                                at = 294; l263 = l264;
                                at = 297; l266 = l26;
                                at = 299; l267 = l260;
                                at = 301; l268 = l28;
                                at = 303; l269 = l30;
                              } else {
                                at = 292; l262 = l23;
                                at = 295; l265 = K[77];
                                at = 296; l263 = l265;
                                at = 298; l266 = l26;
                                at = 300; l267 = l27;
                                at = 302; l268 = l28;
                                at = 304; l269 = l30;
                              }
                              at = 306; l270 = l262;
                              at = 309; l271 = l263;
                              at = 311; l273 = l266;
                              at = 313; l274 = l267;
                              at = 315; l275 = l268;
                              at = 317; l276 = l269;
                            }
                            at = 319; l277 = l270;
                            at = 322; l278 = l271;
                            at = 324; l280 = l273;
                            at = 326; l281 = l274;
                            at = 328; l282 = l275;
                            at = 330; l283 = l276;
                          }
                          at = 332; l284 = l277;
                          at = 335; l285 = l278;
                          at = 337; l287 = l280;
                          at = 339; l288 = l281;
                          at = 341; l289 = l282;
                          at = 343; l290 = l283;
                        }
                        at = 345; l291 = l284;
                        at = 348; l292 = l285;
                        at = 350; l294 = l287;
                        at = 352; l295 = l288;
                        at = 354; l296 = l289;
                        at = 356; l297 = l290;
                      }
                      at = 358; l298 = l291;
                      at = 361; l299 = l292;
                      at = 363; l301 = l294;
                      at = 365; l302 = l295;
                      at = 367; l303 = l296;
                      at = 369; l304 = l297;
                    }
                    at = 371; l305 = l298;
                    at = 374; l306 = l299;
                    at = 376; l308 = l301;
                    at = 378; l309 = l302;
                    at = 381; l310 = l303;
                    at = 384; l312 = l304;
                  }
                  at = 386; l314 = l305;
                  at = 389; l315 = l306;
                  at = 391; l317 = l308;
                  at = 393; l318 = l309;
                  at = 396; l319 = l310;
                  at = 398; l321 = l312;
                }
                at = 400; l322 = l314;
                at = 403; l323 = l315;
                at = 405; l325 = l317;
                at = 407; l326 = l318;
                at = 410; l327 = l319;
                at = 412; l329 = l321;
              }
              at = 420; l337 = l322;
              at = 422; l338 = l323;
              at = 425; l340 = l325;
              at = 427; l341 = l326;
              at = 429; l342 = l327;
              at = 432; l344 = l329;
            } else {
              at = 413; l330 = K[90];
              at = 414; l331 = rt.array_vec3(l330, l51);
              at = 415; l332 = field(rt, l25, "color");
              at = 416; l333 = rt.coerce("int", l26);
              at = 417; l334 = field(rt, l25, "width");
              at = 418; l335 = rt.coerce("int", l27);
              at = 419; l336 = field(rt, l25, "points");
              at = 421; l337 = l23;
              at = 423; l339 = K[91];
              at = 424; l338 = l339;
              at = 426; l340 = l26;
              at = 428; l341 = l27;
              at = 430; l343 = K[92];
              at = 431; l342 = l343;
              at = 433; l344 = l30;
            }
            at = 892; l683 = l337;
            at = 894; l684 = l338;
            at = 896; l685 = l340;
            at = 898; l686 = l341;
            at = 900; l687 = l342;
            at = 902; l688 = l344;
          } else {
            at = 434; l345 = field(rt, l38, "kind");
            at = 435; l346 = K[93];
            at = 436; l347 = rt.equal(l345, l346);
            at = 437; if (rt.truthy(l347)) {
              at = 438; if (rt.truthy(l24)) {
                at = 439; l348 = field(rt, l25, "points");
                at = 440; l349 = rt.array_count(l348);
                at = 441; l350 = K[94];
                at = 442; l351 = rt.ge(l349, l350);
                at = 443; if (rt.truthy(l351)) {
                } else {
                  at = 444; l352 = K[95];
                  at = 445; l353 = rt.eq(l349, l352);
                  at = 446; if (rt.truthy(l353)) {
                    at = 447; l354 = K[96];
                    at = 448; l355 = rt.array_vec3(l354, l51);
                    at = 449; l356 = field(rt, l25, "color");
                    at = 450; l357 = field(rt, l25, "width");
                    at = 451; l358 = field(rt, l25, "points");
                  } else {
                    at = 452; l359 = field(rt, l25, "points");
                    at = 453; l360 = K[97];
                    at = 454; l361 = rt.sub(l349, l360);
                    at = 455; l362 = rt.array_nth(l359, l361);
                    at = 456; l363 = field(rt, l51, "x");
                    at = 457; l364 = field(rt, l362, "x");
                    at = 458; l365 = rt.sub(l363, l364);
                    at = 459; l366 = field(rt, l51, "y");
                    at = 460; l367 = field(rt, l362, "y");
                    at = 461; l368 = rt.sub(l366, l367);
                    at = 462; l369 = rt.mul(l365, l365);
                    at = 463; l370 = rt.mul(l368, l368);
                    at = 464; l371 = rt.add(l369, l370);
                    at = 465; l372 = K[98];
                    at = 466; l373 = rt.lt(l371, l372);
                    at = 467; if (rt.truthy(l373)) {
                    } else {
                      at = 468; l374 = field(rt, l25, "points");
                      at = 469; l375 = K[99];
                      at = 470; l376 = rt.array_vec3(l375, l51);
                      at = 471; l377 = rt.array_concat(l374, l376);
                      at = 472; l378 = field(rt, l25, "color");
                      at = 473; l379 = field(rt, l25, "width");
                      at = 474; l380 = field(rt, l25, "points");
                    }
                  }
                }
                at = 475; if (rt.truthy(l351)) {
                  at = 476; l381 = field(rt, l25, "color");
                  at = 477; l382 = field(rt, l25, "width");
                  at = 478; l383 = field(rt, l25, "points");
                  at = 479; l384 = field(rt, l25, "color");
                  at = 480; l385 = field(rt, l25, "width");
                  at = 481; l386 = field(rt, l25, "points");
                  at = 508; l405 = l381;
                  at = 510; l406 = l382;
                  at = 512; l407 = l383;
                } else {
                  at = 482; if (rt.truthy(l353)) {
                    at = 483; l387 = field(rt, l25, "color");
                    at = 484; l388 = field(rt, l25, "width");
                    at = 485; l389 = field(rt, l25, "points");
                    at = 502; l402 = l356;
                    at = 504; l403 = l357;
                    at = 506; l404 = l355;
                  } else {
                    at = 486; if (rt.truthy(l373)) {
                      at = 487; l390 = field(rt, l25, "color");
                      at = 488; l391 = field(rt, l25, "width");
                      at = 489; l392 = field(rt, l25, "points");
                      at = 490; l393 = field(rt, l25, "color");
                      at = 491; l394 = field(rt, l25, "width");
                      at = 492; l395 = field(rt, l25, "points");
                      at = 496; l399 = l390;
                      at = 498; l400 = l391;
                      at = 500; l401 = l392;
                    } else {
                      at = 493; l396 = field(rt, l25, "color");
                      at = 494; l397 = field(rt, l25, "width");
                      at = 495; l398 = field(rt, l25, "points");
                      at = 497; l399 = l378;
                      at = 499; l400 = l379;
                      at = 501; l401 = l377;
                    }
                    at = 503; l402 = l399;
                    at = 505; l403 = l400;
                    at = 507; l404 = l401;
                  }
                  at = 509; l405 = l402;
                  at = 511; l406 = l403;
                  at = 513; l407 = l404;
                }
                at = 514; l408 = l23;
                at = 516; l409 = l24;
                at = 518; l410 = l26;
                at = 520; l411 = l27;
                at = 522; l412 = l28;
                at = 524; l413 = l30;
              } else {
                at = 515; l408 = l23;
                at = 517; l409 = l24;
                at = 519; l410 = l26;
                at = 521; l411 = l27;
                at = 523; l412 = l28;
                at = 525; l413 = l30;
              }
              at = 880; l677 = l408;
              at = 882; l678 = l409;
              at = 884; l679 = l410;
              at = 886; l680 = l411;
              at = 888; l681 = l412;
              at = 890; l682 = l413;
            } else {
              at = 526; l414 = field(rt, l38, "kind");
              at = 527; l415 = K[100];
              at = 528; l416 = rt.equal(l414, l415);
              at = 529; l417 = field(rt, l38, "button");
              at = 530; l418 = K[101];
              at = 531; l419 = rt.equal(l417, l418);
              at = 532; l420 = rt.and(l416, l419);
              at = 533; if (rt.truthy(l420)) {
                at = 534; l421 = rt.coerce({list: {record: [["color", "int"], ["width", "int"], ["points", {array: "vec3"}]]}}, l23);
                at = 535; l422 = rt.coerce("int", l26);
                at = 536; l423 = rt.coerce("int", l27);
                at = 537; if (rt.truthy(l24)) {
                  at = 538; l424 = field(rt, l25, "points");
                  at = 539; l425 = rt.array_count(l424);
                  at = 540; l426 = K[102];
                  at = 541; l427 = rt.ge(l425, l426);
                  at = 542; if (rt.truthy(l427)) {
                  } else {
                    at = 543; l428 = K[103];
                    at = 544; l429 = rt.eq(l425, l428);
                    at = 545; if (rt.truthy(l429)) {
                      at = 546; l430 = K[104];
                      at = 547; l431 = rt.array_vec3(l430, l51);
                      at = 548; l432 = field(rt, l25, "color");
                      at = 549; l433 = field(rt, l25, "width");
                      at = 550; l434 = field(rt, l25, "points");
                    } else {
                      at = 551; l435 = field(rt, l25, "points");
                      at = 552; l436 = K[105];
                      at = 553; l437 = rt.sub(l425, l436);
                      at = 554; l438 = rt.array_nth(l435, l437);
                      at = 555; l439 = field(rt, l51, "x");
                      at = 556; l440 = field(rt, l438, "x");
                      at = 557; l441 = rt.sub(l439, l440);
                      at = 558; l442 = field(rt, l51, "y");
                      at = 559; l443 = field(rt, l438, "y");
                      at = 560; l444 = rt.sub(l442, l443);
                      at = 561; l445 = rt.mul(l441, l441);
                      at = 562; l446 = rt.mul(l444, l444);
                      at = 563; l447 = rt.add(l445, l446);
                      at = 564; l448 = K[106];
                      at = 565; l449 = rt.lt(l447, l448);
                      at = 566; if (rt.truthy(l449)) {
                      } else {
                        at = 567; l450 = field(rt, l25, "points");
                        at = 568; l451 = K[107];
                        at = 569; l452 = rt.array_vec3(l451, l51);
                        at = 570; l453 = rt.array_concat(l450, l452);
                        at = 571; l454 = field(rt, l25, "color");
                        at = 572; l455 = field(rt, l25, "width");
                        at = 573; l456 = field(rt, l25, "points");
                      }
                    }
                  }
                  at = 574; if (rt.truthy(l427)) {
                    at = 575; l457 = l25;
                  } else {
                    at = 576; if (rt.truthy(l429)) {
                      at = 577; l459 = new Map([["color", l432], ["width", l433], ["points", l431]]);
                      at = 578; l458 = l459;
                    } else {
                      at = 579; if (rt.truthy(l449)) {
                        at = 580; l460 = l25;
                      } else {
                        at = 581; l461 = new Map([["color", l454], ["width", l455], ["points", l453]]);
                        at = 582; l460 = l461;
                      }
                      at = 583; l458 = l460;
                    }
                    at = 584; l457 = l458;
                  }
                  at = 585; l462 = rt.sequence_concat([l457]);
                  at = 586; l463 = rt.sequence_concat(l462, l421);
                  at = 587; l464 = rt.sequence_length(l463);
                  at = 588; l465 = K[108];
                  at = 589; l466 = rt.float(l465);
                  at = 590; l467 = rt.coerce("int", l466);
                  at = 591; l468 = K[109];
                  at = 592; l469 = rt.max(l468, l467);
                  at = 593; l470 = rt.min(l469, l464);
                  at = 594; l471 = K[110];
                  at = 595; l472 = rt.sequence_slice(l463, l471, l470);
                  at = 596; l473 = rt.sequence_length(l421);
                  at = 597; l474 = K[111];
                  at = 598; l475 = rt.eq(l473, l474);
                  at = 599; if (rt.truthy(l475)) {
                    at = 600; l476 = l472;
                  } else {
                    at = 601; l477 = K[112];
                    at = 602; l478 = rt.sequence_get(l421, l477);
                    at = 603; l479 = rt.sequence_length(l472);
                    at = 604; l480 = rt.sequence_build(l479);
                    at = 605; {
                      const s605 = l472, c605 = Array.isArray(s605) ? s605.length : Number(rt.array_count(s605));
                      if (c605 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                      for (let i605 = 0; i605 < c605; i605++) {
                        l481 = BigInt(i605); l482 = Array.isArray(s605) ? s605[i605] : rt.array_nth(s605, i605);
                        at = 606; l483 = field(rt, l482, "color");
                        at = 607; l484 = field(rt, l482, "width");
                        at = 608; l485 = field(rt, l482, "points");
                        at = 609; l486 = field(rt, l478, "color");
                        at = 610; l487 = field(rt, l478, "width");
                        at = 611; l488 = field(rt, l478, "points");
                        at = 612; l489 = new Map([["color", l483], ["width", l484], ["points", l485]]);
                        at = 613; store(rt, l480, l481, l489);
                      }
                    }
                    at = 614; l476 = l480;
                  }
                  at = 615; l490 = l476;
                  at = 617; l492 = K[113];
                  at = 618; l491 = l492;
                  at = 620; l493 = l422;
                  at = 622; l494 = l423;
                  at = 624; l496 = K[114];
                  at = 625; l495 = l496;
                  at = 627; l497 = l30;
                } else {
                  at = 616; l490 = l421;
                  at = 619; l491 = l24;
                  at = 621; l493 = l422;
                  at = 623; l494 = l423;
                  at = 626; l495 = l28;
                  at = 628; l497 = l30;
                }
                at = 868; l671 = l490;
                at = 870; l672 = l491;
                at = 872; l673 = l493;
                at = 874; l674 = l494;
                at = 876; l675 = l495;
                at = 878; l676 = l497;
              } else {
                at = 629; l498 = field(rt, l38, "kind");
                at = 630; l499 = K[115];
                at = 631; l500 = rt.equal(l498, l499);
                at = 632; l501 = field(rt, l38, "button");
                at = 633; l502 = K[116];
                at = 634; l503 = rt.equal(l501, l502);
                at = 635; l504 = rt.and(l500, l503);
                at = 636; l505 = field(rt, l38, "kind");
                at = 637; l506 = K[117];
                at = 638; l507 = rt.equal(l505, l506);
                at = 639; l508 = rt.or(l504, l507);
                at = 640; if (rt.truthy(l508)) {
                  at = 641; l509 = rt.coerce({list: {record: [["color", "int"], ["width", "int"], ["points", {array: "vec3"}]]}}, l23);
                  at = 642; l510 = rt.coerce("int", l26);
                  at = 643; l511 = rt.coerce("int", l27);
                  at = 644; if (rt.truthy(l24)) {
                    at = 645; l512 = rt.sequence_concat([l25]);
                    at = 646; l513 = rt.sequence_concat(l512, l509);
                    at = 647; l514 = rt.sequence_length(l513);
                    at = 648; l515 = K[118];
                    at = 649; l516 = rt.float(l515);
                    at = 650; l517 = rt.coerce("int", l516);
                    at = 651; l518 = K[119];
                    at = 652; l519 = rt.max(l518, l517);
                    at = 653; l520 = rt.min(l519, l514);
                    at = 654; l521 = K[120];
                    at = 655; l522 = rt.sequence_slice(l513, l521, l520);
                    at = 656; l523 = rt.sequence_length(l509);
                    at = 657; l524 = K[121];
                    at = 658; l525 = rt.eq(l523, l524);
                    at = 659; if (rt.truthy(l525)) {
                      at = 660; l526 = l522;
                    } else {
                      at = 661; l527 = K[122];
                      at = 662; l528 = rt.sequence_get(l509, l527);
                      at = 663; l529 = rt.sequence_length(l522);
                      at = 664; l530 = rt.sequence_build(l529);
                      at = 665; {
                        const s665 = l522, c665 = Array.isArray(s665) ? s665.length : Number(rt.array_count(s665));
                        if (c665 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                        for (let i665 = 0; i665 < c665; i665++) {
                          l531 = BigInt(i665); l532 = Array.isArray(s665) ? s665[i665] : rt.array_nth(s665, i665);
                          at = 666; l533 = field(rt, l532, "color");
                          at = 667; l534 = field(rt, l532, "width");
                          at = 668; l535 = field(rt, l532, "points");
                          at = 669; l536 = field(rt, l528, "color");
                          at = 670; l537 = field(rt, l528, "width");
                          at = 671; l538 = field(rt, l528, "points");
                          at = 672; l539 = new Map([["color", l533], ["width", l534], ["points", l535]]);
                          at = 673; store(rt, l530, l531, l539);
                        }
                      }
                      at = 674; l526 = l530;
                    }
                    at = 675; l540 = l526;
                    at = 677; l542 = K[123];
                    at = 678; l541 = l542;
                    at = 680; l543 = l510;
                    at = 682; l544 = l511;
                    at = 684; l546 = K[124];
                    at = 685; l545 = l546;
                    at = 687; l547 = l30;
                  } else {
                    at = 676; l540 = l509;
                    at = 679; l541 = l24;
                    at = 681; l543 = l510;
                    at = 683; l544 = l511;
                    at = 686; l545 = l28;
                    at = 688; l547 = l30;
                  }
                  at = 856; l665 = l540;
                  at = 858; l666 = l541;
                  at = 860; l667 = l543;
                  at = 862; l668 = l544;
                  at = 864; l669 = l545;
                  at = 866; l670 = l547;
                } else {
                  at = 689; l548 = field(rt, l38, "kind");
                  at = 690; l549 = K[125];
                  at = 691; l550 = rt.equal(l548, l549);
                  at = 692; if (rt.truthy(l550)) {
                    at = 693; l551 = field(rt, l38, "key");
                    at = 694; l552 = K[126];
                    at = 695; l553 = rt.equal(l551, l552);
                    at = 696; l554 = field(rt, l38, "key");
                    at = 697; l555 = K[127];
                    at = 698; l556 = rt.equal(l554, l555);
                    at = 699; l557 = rt.or(l553, l556);
                    at = 700; if (rt.truthy(l557)) {
                      at = 701; l558 = rt.sequence_length(l23);
                      at = 702; l559 = K[128];
                      at = 703; l560 = rt.eq(l558, l559);
                      at = 704; if (rt.truthy(l560)) {
                        at = 705; l562 = K[129];
                        at = 706; l561 = l562;
                      } else {
                        at = 707; l563 = K[130];
                        at = 708; l564 = rt.sequence_get(l23, l563);
                        at = 709; l565 = K[131];
                        at = 710; l566 = rt.sequence_build(l565);
                        at = 711; l567 = K[132];
                        at = 712; {
                          const s712 = l567, c712 = Array.isArray(s712) ? s712.length : Number(rt.array_count(s712));
                          if (c712 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                          for (let i712 = 0; i712 < c712; i712++) {
                            l568 = BigInt(i712); l569 = Array.isArray(s712) ? s712[i712] : rt.array_nth(s712, i712);
                            at = 713; store(rt, l566, l568, l569);
                          }
                        }
                        at = 714; l561 = l566;
                      }
                      at = 830; l651 = l561;
                      at = 832; l653 = K[151];
                      at = 833; l652 = l653;
                      at = 835; l654 = l26;
                      at = 837; l655 = l27;
                      at = 839; l657 = K[152];
                      at = 840; l656 = l657;
                      at = 842; l658 = l30;
                    } else {
                      at = 715; l570 = field(rt, l38, "key");
                      at = 716; l571 = K[133];
                      at = 717; l572 = rt.equal(l570, l571);
                      at = 718; l573 = field(rt, l38, "key");
                      at = 719; l574 = K[134];
                      at = 720; l575 = rt.equal(l573, l574);
                      at = 721; l576 = rt.or(l572, l575);
                      at = 722; if (rt.truthy(l576)) {
                        at = 723; l577 = rt.sequence_length(l23);
                        at = 724; l578 = K[135];
                        at = 725; l579 = K[136];
                        at = 726; l580 = rt.max(l578, l579);
                        at = 727; l581 = rt.min(l580, l577);
                        at = 728; l582 = rt.sub(l577, l581);
                        at = 729; l583 = rt.sequence_slice(l23, l581, l582);
                        at = 730; l584 = rt.sequence_length(l23);
                        at = 731; l585 = K[137];
                        at = 732; l586 = rt.eq(l584, l585);
                        at = 733; if (rt.truthy(l586)) {
                          at = 734; l587 = l583;
                        } else {
                          at = 735; l588 = K[138];
                          at = 736; l589 = rt.sequence_get(l23, l588);
                          at = 737; l590 = rt.sequence_length(l583);
                          at = 738; l591 = rt.sequence_build(l590);
                          at = 739; {
                            const s739 = l583, c739 = Array.isArray(s739) ? s739.length : Number(rt.array_count(s739));
                            if (c739 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                            for (let i739 = 0; i739 < c739; i739++) {
                              l592 = BigInt(i739); l593 = Array.isArray(s739) ? s739[i739] : rt.array_nth(s739, i739);
                              at = 740; store(rt, l591, l592, l593);
                            }
                          }
                          at = 741; l587 = l591;
                        }
                        at = 816; l643 = l587;
                        at = 818; l645 = K[149];
                        at = 819; l644 = l645;
                        at = 821; l646 = l26;
                        at = 823; l647 = l27;
                        at = 825; l649 = K[150];
                        at = 826; l648 = l649;
                        at = 828; l650 = l30;
                      } else {
                        at = 742; l594 = field(rt, l38, "key");
                        at = 743; l595 = K[139];
                        at = 744; l596 = rt.equal(l594, l595);
                        at = 745; if (rt.truthy(l596)) {
                          at = 746; l597 = K[140];
                          at = 747; l598 = rt.sub(l27, l597);
                          at = 748; l599 = K[141];
                          at = 749; l600 = rt.max(l599, l598);
                          at = 750; l601 = rt.coerce("float", l27);
                          at = 751; l602 = rt.equal(l27, l601);
                          at = 752; if (rt.truthy(l602)) {
                            at = 753; l604 = rt.coerce("float", l600);
                            at = 754; l603 = l604;
                          } else {
                            at = 755; l603 = l600;
                          }
                          at = 804; l637 = l23;
                          at = 806; l638 = l24;
                          at = 808; l639 = l26;
                          at = 810; l640 = l603;
                          at = 812; l641 = l28;
                          at = 814; l642 = l30;
                        } else {
                          at = 756; l605 = field(rt, l38, "key");
                          at = 757; l606 = K[142];
                          at = 758; l607 = rt.equal(l605, l606);
                          at = 759; if (rt.truthy(l607)) {
                            at = 760; l608 = K[143];
                            at = 761; l609 = rt.add(l27, l608);
                            at = 762; l610 = K[144];
                            at = 763; l611 = rt.min(l610, l609);
                            at = 764; l612 = rt.coerce("float", l27);
                            at = 765; l613 = rt.equal(l27, l612);
                            at = 766; if (rt.truthy(l613)) {
                              at = 767; l615 = rt.coerce("float", l611);
                              at = 768; l614 = l615;
                            } else {
                              at = 769; l614 = l611;
                            }
                            at = 792; l631 = l23;
                            at = 794; l632 = l24;
                            at = 796; l633 = l26;
                            at = 798; l634 = l614;
                            at = 800; l635 = l28;
                            at = 802; l636 = l30;
                          } else {
                            at = 770; l616 = field(rt, l38, "key");
                            at = 771; l617 = K[145];
                            at = 772; l618 = rt.equal(l616, l617);
                            at = 773; l619 = field(rt, l38, "key");
                            at = 774; l620 = K[146];
                            at = 775; l621 = rt.equal(l619, l620);
                            at = 776; l622 = rt.or(l618, l621);
                            at = 777; if (rt.truthy(l622)) {
                              at = 778; l623 = l23;
                              at = 780; l624 = l24;
                              at = 782; l625 = l26;
                              at = 784; l626 = l27;
                              at = 786; l628 = K[147];
                              at = 787; l627 = l628;
                              at = 789; l630 = K[148];
                              at = 790; l629 = l630;
                            } else {
                              at = 779; l623 = l23;
                              at = 781; l624 = l24;
                              at = 783; l625 = l26;
                              at = 785; l626 = l27;
                              at = 788; l627 = l28;
                              at = 791; l629 = l30;
                            }
                            at = 793; l631 = l623;
                            at = 795; l632 = l624;
                            at = 797; l633 = l625;
                            at = 799; l634 = l626;
                            at = 801; l635 = l627;
                            at = 803; l636 = l629;
                          }
                          at = 805; l637 = l631;
                          at = 807; l638 = l632;
                          at = 809; l639 = l633;
                          at = 811; l640 = l634;
                          at = 813; l641 = l635;
                          at = 815; l642 = l636;
                        }
                        at = 817; l643 = l637;
                        at = 820; l644 = l638;
                        at = 822; l646 = l639;
                        at = 824; l647 = l640;
                        at = 827; l648 = l641;
                        at = 829; l650 = l642;
                      }
                      at = 831; l651 = l643;
                      at = 834; l652 = l644;
                      at = 836; l654 = l646;
                      at = 838; l655 = l647;
                      at = 841; l656 = l648;
                      at = 843; l658 = l650;
                    }
                    at = 844; l659 = l651;
                    at = 846; l660 = l652;
                    at = 848; l661 = l654;
                    at = 850; l662 = l655;
                    at = 852; l663 = l656;
                    at = 854; l664 = l658;
                  } else {
                    at = 845; l659 = l23;
                    at = 847; l660 = l24;
                    at = 849; l661 = l26;
                    at = 851; l662 = l27;
                    at = 853; l663 = l28;
                    at = 855; l664 = l30;
                  }
                  at = 857; l665 = l659;
                  at = 859; l666 = l660;
                  at = 861; l667 = l661;
                  at = 863; l668 = l662;
                  at = 865; l669 = l663;
                  at = 867; l670 = l664;
                }
                at = 869; l671 = l665;
                at = 871; l672 = l666;
                at = 873; l673 = l667;
                at = 875; l674 = l668;
                at = 877; l675 = l669;
                at = 879; l676 = l670;
              }
              at = 881; l677 = l671;
              at = 883; l678 = l672;
              at = 885; l679 = l673;
              at = 887; l680 = l674;
              at = 889; l681 = l675;
              at = 891; l682 = l676;
            }
            at = 893; l683 = l677;
            at = 895; l684 = l678;
            at = 897; l685 = l679;
            at = 899; l686 = l680;
            at = 901; l687 = l681;
            at = 903; l688 = l682;
          }
          at = 904; l689 = rt.sequence_length(l23);
          at = 905; l690 = K[153];
          at = 906; l691 = rt.eq(l689, l690);
          at = 907; if (rt.truthy(l691)) {
            at = 908; l692 = l683;
          } else {
            at = 909; l693 = K[154];
            at = 910; l694 = rt.sequence_get(l23, l693);
            at = 911; l695 = rt.sequence_length(l683);
            at = 912; l696 = rt.sequence_build(l695);
            at = 913; {
              const s913 = l683, c913 = Array.isArray(s913) ? s913.length : Number(rt.array_count(s913));
              if (c913 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              for (let i913 = 0; i913 < c913; i913++) {
                l697 = BigInt(i913); l698 = Array.isArray(s913) ? s913[i913] : rt.array_nth(s913, i913);
                at = 914; store(rt, l696, l697, l698);
              }
            }
            at = 915; l692 = l696;
          }
          at = 916; if (rt.truthy(l58)) {
            at = 917; if (rt.truthy(l61)) {
              at = 918; if (rt.truthy(l76)) {
                at = 919; l699 = field(rt, l25, "color");
                at = 920; l700 = field(rt, l25, "width");
                at = 921; l701 = field(rt, l25, "points");
                at = 922; l702 = field(rt, l25, "color");
                at = 923; l703 = field(rt, l25, "width");
                at = 924; l704 = field(rt, l25, "points");
                at = 1035; l783 = l699;
                at = 1037; l784 = l700;
                at = 1039; l785 = l701;
              } else {
                at = 925; if (rt.truthy(l103)) {
                  at = 926; l705 = field(rt, l25, "color");
                  at = 927; l706 = field(rt, l25, "width");
                  at = 928; l707 = field(rt, l25, "points");
                  at = 929; l708 = field(rt, l25, "color");
                  at = 930; l709 = field(rt, l25, "width");
                  at = 931; l710 = field(rt, l25, "points");
                  at = 1029; l780 = l705;
                  at = 1031; l781 = l706;
                  at = 1033; l782 = l707;
                } else {
                  at = 932; if (rt.truthy(l135)) {
                    at = 933; l711 = field(rt, l25, "color");
                    at = 934; l712 = field(rt, l25, "width");
                    at = 935; l713 = field(rt, l25, "points");
                    at = 936; l714 = field(rt, l25, "color");
                    at = 937; l715 = field(rt, l25, "width");
                    at = 938; l716 = field(rt, l25, "points");
                    at = 1023; l777 = l711;
                    at = 1025; l778 = l712;
                    at = 1027; l779 = l713;
                  } else {
                    at = 939; if (rt.truthy(l150)) {
                      at = 940; l717 = field(rt, l25, "color");
                      at = 941; l718 = field(rt, l25, "width");
                      at = 942; l719 = field(rt, l25, "points");
                      at = 943; l720 = field(rt, l25, "color");
                      at = 944; l721 = field(rt, l25, "width");
                      at = 945; l722 = field(rt, l25, "points");
                      at = 1017; l774 = l717;
                      at = 1019; l775 = l718;
                      at = 1021; l776 = l719;
                    } else {
                      at = 946; if (rt.truthy(l170)) {
                        at = 947; l723 = field(rt, l25, "color");
                        at = 948; l724 = field(rt, l25, "width");
                        at = 949; l725 = field(rt, l25, "points");
                        at = 950; l726 = field(rt, l25, "color");
                        at = 951; l727 = field(rt, l25, "width");
                        at = 952; l728 = field(rt, l25, "points");
                        at = 1011; l771 = l723;
                        at = 1013; l772 = l724;
                        at = 1015; l773 = l725;
                      } else {
                        at = 953; if (rt.truthy(l190)) {
                          at = 954; l729 = field(rt, l25, "color");
                          at = 955; l730 = field(rt, l25, "width");
                          at = 956; l731 = field(rt, l25, "points");
                          at = 957; l732 = field(rt, l25, "color");
                          at = 958; l733 = field(rt, l25, "width");
                          at = 959; l734 = field(rt, l25, "points");
                          at = 1005; l768 = l729;
                          at = 1007; l769 = l730;
                          at = 1009; l770 = l731;
                        } else {
                          at = 960; if (rt.truthy(l210)) {
                            at = 961; l735 = field(rt, l25, "color");
                            at = 962; l736 = field(rt, l25, "width");
                            at = 963; l737 = field(rt, l25, "points");
                            at = 964; l738 = field(rt, l25, "color");
                            at = 965; l739 = field(rt, l25, "width");
                            at = 966; l740 = field(rt, l25, "points");
                            at = 999; l765 = l735;
                            at = 1001; l766 = l736;
                            at = 1003; l767 = l737;
                          } else {
                            at = 967; if (rt.truthy(l230)) {
                              at = 968; l741 = field(rt, l25, "color");
                              at = 969; l742 = field(rt, l25, "width");
                              at = 970; l743 = field(rt, l25, "points");
                              at = 971; l744 = field(rt, l25, "color");
                              at = 972; l745 = field(rt, l25, "width");
                              at = 973; l746 = field(rt, l25, "points");
                              at = 993; l762 = l741;
                              at = 995; l763 = l742;
                              at = 997; l764 = l743;
                            } else {
                              at = 974; if (rt.truthy(l253)) {
                                at = 975; l747 = field(rt, l25, "color");
                                at = 976; l748 = field(rt, l25, "width");
                                at = 977; l749 = field(rt, l25, "points");
                                at = 978; l750 = field(rt, l25, "color");
                                at = 979; l751 = field(rt, l25, "width");
                                at = 980; l752 = field(rt, l25, "points");
                                at = 987; l759 = l747;
                                at = 989; l760 = l748;
                                at = 991; l761 = l749;
                              } else {
                                at = 981; l753 = field(rt, l25, "color");
                                at = 982; l754 = field(rt, l25, "width");
                                at = 983; l755 = field(rt, l25, "points");
                                at = 984; l756 = field(rt, l25, "color");
                                at = 985; l757 = field(rt, l25, "width");
                                at = 986; l758 = field(rt, l25, "points");
                                at = 988; l759 = l753;
                                at = 990; l760 = l754;
                                at = 992; l761 = l755;
                              }
                              at = 994; l762 = l759;
                              at = 996; l763 = l760;
                              at = 998; l764 = l761;
                            }
                            at = 1000; l765 = l762;
                            at = 1002; l766 = l763;
                            at = 1004; l767 = l764;
                          }
                          at = 1006; l768 = l765;
                          at = 1008; l769 = l766;
                          at = 1010; l770 = l767;
                        }
                        at = 1012; l771 = l768;
                        at = 1014; l772 = l769;
                        at = 1016; l773 = l770;
                      }
                      at = 1018; l774 = l771;
                      at = 1020; l775 = l772;
                      at = 1022; l776 = l773;
                    }
                    at = 1024; l777 = l774;
                    at = 1026; l778 = l775;
                    at = 1028; l779 = l776;
                  }
                  at = 1030; l780 = l777;
                  at = 1032; l781 = l778;
                  at = 1034; l782 = l779;
                }
                at = 1036; l783 = l780;
                at = 1038; l784 = l781;
                at = 1040; l785 = l782;
              }
              at = 1044; l789 = l783;
              at = 1046; l790 = l784;
              at = 1048; l791 = l785;
            } else {
              at = 1041; l786 = field(rt, l25, "color");
              at = 1042; l787 = field(rt, l25, "width");
              at = 1043; l788 = field(rt, l25, "points");
              at = 1045; l789 = l333;
              at = 1047; l790 = l335;
              at = 1049; l791 = l331;
            }
            at = 1209; l903 = l789;
            at = 1211; l904 = l790;
            at = 1213; l905 = l791;
          } else {
            at = 1050; if (rt.truthy(l347)) {
              at = 1051; if (rt.truthy(l24)) {
                at = 1052; l792 = field(rt, l25, "color");
                at = 1053; l793 = field(rt, l25, "width");
                at = 1054; l794 = field(rt, l25, "points");
                at = 1061; l801 = l405;
                at = 1063; l802 = l406;
                at = 1065; l803 = l407;
              } else {
                at = 1055; l795 = field(rt, l25, "color");
                at = 1056; l796 = field(rt, l25, "width");
                at = 1057; l797 = field(rt, l25, "points");
                at = 1058; l798 = field(rt, l25, "color");
                at = 1059; l799 = field(rt, l25, "width");
                at = 1060; l800 = field(rt, l25, "points");
                at = 1062; l801 = l795;
                at = 1064; l802 = l796;
                at = 1066; l803 = l797;
              }
              at = 1203; l900 = l801;
              at = 1205; l901 = l802;
              at = 1207; l902 = l803;
            } else {
              at = 1067; if (rt.truthy(l420)) {
                at = 1068; if (rt.truthy(l24)) {
                  at = 1069; l804 = field(rt, l25, "color");
                  at = 1070; l805 = field(rt, l25, "width");
                  at = 1071; l806 = field(rt, l25, "points");
                  at = 1072; l807 = field(rt, l25, "color");
                  at = 1073; l808 = field(rt, l25, "width");
                  at = 1074; l809 = field(rt, l25, "points");
                  at = 1081; l816 = l804;
                  at = 1083; l817 = l805;
                  at = 1085; l818 = l806;
                } else {
                  at = 1075; l810 = field(rt, l25, "color");
                  at = 1076; l811 = field(rt, l25, "width");
                  at = 1077; l812 = field(rt, l25, "points");
                  at = 1078; l813 = field(rt, l25, "color");
                  at = 1079; l814 = field(rt, l25, "width");
                  at = 1080; l815 = field(rt, l25, "points");
                  at = 1082; l816 = l810;
                  at = 1084; l817 = l811;
                  at = 1086; l818 = l812;
                }
                at = 1197; l897 = l816;
                at = 1199; l898 = l817;
                at = 1201; l899 = l818;
              } else {
                at = 1087; if (rt.truthy(l508)) {
                  at = 1088; if (rt.truthy(l24)) {
                    at = 1089; l819 = field(rt, l25, "color");
                    at = 1090; l820 = field(rt, l25, "width");
                    at = 1091; l821 = field(rt, l25, "points");
                    at = 1092; l822 = field(rt, l25, "color");
                    at = 1093; l823 = field(rt, l25, "width");
                    at = 1094; l824 = field(rt, l25, "points");
                    at = 1101; l831 = l819;
                    at = 1103; l832 = l820;
                    at = 1105; l833 = l821;
                  } else {
                    at = 1095; l825 = field(rt, l25, "color");
                    at = 1096; l826 = field(rt, l25, "width");
                    at = 1097; l827 = field(rt, l25, "points");
                    at = 1098; l828 = field(rt, l25, "color");
                    at = 1099; l829 = field(rt, l25, "width");
                    at = 1100; l830 = field(rt, l25, "points");
                    at = 1102; l831 = l825;
                    at = 1104; l832 = l826;
                    at = 1106; l833 = l827;
                  }
                  at = 1191; l894 = l831;
                  at = 1193; l895 = l832;
                  at = 1195; l896 = l833;
                } else {
                  at = 1107; if (rt.truthy(l550)) {
                    at = 1108; if (rt.truthy(l557)) {
                      at = 1109; l834 = field(rt, l25, "color");
                      at = 1110; l835 = field(rt, l25, "width");
                      at = 1111; l836 = field(rt, l25, "points");
                      at = 1112; l837 = field(rt, l25, "color");
                      at = 1113; l838 = field(rt, l25, "width");
                      at = 1114; l839 = field(rt, l25, "points");
                      at = 1173; l882 = l834;
                      at = 1175; l883 = l835;
                      at = 1177; l884 = l836;
                    } else {
                      at = 1115; if (rt.truthy(l576)) {
                        at = 1116; l840 = field(rt, l25, "color");
                        at = 1117; l841 = field(rt, l25, "width");
                        at = 1118; l842 = field(rt, l25, "points");
                        at = 1119; l843 = field(rt, l25, "color");
                        at = 1120; l844 = field(rt, l25, "width");
                        at = 1121; l845 = field(rt, l25, "points");
                        at = 1167; l879 = l840;
                        at = 1169; l880 = l841;
                        at = 1171; l881 = l842;
                      } else {
                        at = 1122; if (rt.truthy(l596)) {
                          at = 1123; l846 = field(rt, l25, "color");
                          at = 1124; l847 = field(rt, l25, "width");
                          at = 1125; l848 = field(rt, l25, "points");
                          at = 1126; l849 = field(rt, l25, "color");
                          at = 1127; l850 = field(rt, l25, "width");
                          at = 1128; l851 = field(rt, l25, "points");
                          at = 1161; l876 = l846;
                          at = 1163; l877 = l847;
                          at = 1165; l878 = l848;
                        } else {
                          at = 1129; if (rt.truthy(l607)) {
                            at = 1130; l852 = field(rt, l25, "color");
                            at = 1131; l853 = field(rt, l25, "width");
                            at = 1132; l854 = field(rt, l25, "points");
                            at = 1133; l855 = field(rt, l25, "color");
                            at = 1134; l856 = field(rt, l25, "width");
                            at = 1135; l857 = field(rt, l25, "points");
                            at = 1155; l873 = l852;
                            at = 1157; l874 = l853;
                            at = 1159; l875 = l854;
                          } else {
                            at = 1136; if (rt.truthy(l622)) {
                              at = 1137; l858 = field(rt, l25, "color");
                              at = 1138; l859 = field(rt, l25, "width");
                              at = 1139; l860 = field(rt, l25, "points");
                              at = 1140; l861 = field(rt, l25, "color");
                              at = 1141; l862 = field(rt, l25, "width");
                              at = 1142; l863 = field(rt, l25, "points");
                              at = 1149; l870 = l858;
                              at = 1151; l871 = l859;
                              at = 1153; l872 = l860;
                            } else {
                              at = 1143; l864 = field(rt, l25, "color");
                              at = 1144; l865 = field(rt, l25, "width");
                              at = 1145; l866 = field(rt, l25, "points");
                              at = 1146; l867 = field(rt, l25, "color");
                              at = 1147; l868 = field(rt, l25, "width");
                              at = 1148; l869 = field(rt, l25, "points");
                              at = 1150; l870 = l864;
                              at = 1152; l871 = l865;
                              at = 1154; l872 = l866;
                            }
                            at = 1156; l873 = l870;
                            at = 1158; l874 = l871;
                            at = 1160; l875 = l872;
                          }
                          at = 1162; l876 = l873;
                          at = 1164; l877 = l874;
                          at = 1166; l878 = l875;
                        }
                        at = 1168; l879 = l876;
                        at = 1170; l880 = l877;
                        at = 1172; l881 = l878;
                      }
                      at = 1174; l882 = l879;
                      at = 1176; l883 = l880;
                      at = 1178; l884 = l881;
                    }
                    at = 1185; l891 = l882;
                    at = 1187; l892 = l883;
                    at = 1189; l893 = l884;
                  } else {
                    at = 1179; l885 = field(rt, l25, "color");
                    at = 1180; l886 = field(rt, l25, "width");
                    at = 1181; l887 = field(rt, l25, "points");
                    at = 1182; l888 = field(rt, l25, "color");
                    at = 1183; l889 = field(rt, l25, "width");
                    at = 1184; l890 = field(rt, l25, "points");
                    at = 1186; l891 = l885;
                    at = 1188; l892 = l886;
                    at = 1190; l893 = l887;
                  }
                  at = 1192; l894 = l891;
                  at = 1194; l895 = l892;
                  at = 1196; l896 = l893;
                }
                at = 1198; l897 = l894;
                at = 1200; l898 = l895;
                at = 1202; l899 = l896;
              }
              at = 1204; l900 = l897;
              at = 1206; l901 = l898;
              at = 1208; l902 = l899;
            }
            at = 1210; l903 = l900;
            at = 1212; l904 = l901;
            at = 1214; l905 = l902;
          }
          at = 1215; l906 = rt.coerce("float", l26);
          at = 1216; l907 = rt.equal(l26, l906);
          at = 1217; if (rt.truthy(l907)) {
            at = 1218; l909 = rt.coerce("float", l685);
            at = 1219; l908 = l909;
          } else {
            at = 1220; l910 = rt.coerce("int", l685);
            at = 1221; l908 = l910;
          }
          at = 1222; l911 = rt.coerce("float", l27);
          at = 1223; l912 = rt.equal(l27, l911);
          at = 1224; if (rt.truthy(l912)) {
            at = 1225; l914 = rt.coerce("float", l686);
            at = 1226; l913 = l914;
          } else {
            at = 1227; l915 = rt.coerce("int", l686);
            at = 1228; l913 = l915;
          }
          at = 1229; l916 = l692;
          at = 1230; l917 = l684;
          at = 1231; l918 = l903;
          at = 1232; l919 = l904;
          at = 1233; l920 = l905;
          at = 1234; l921 = l908;
          at = 1235; l922 = l913;
          at = 1236; l923 = l687;
          at = 1237; l924 = l688;
          at = 1238; l23 = l916;
          at = 1239; l24 = l917;
          at = 1240; l925 = new Map([["color", l918], ["width", l919], ["points", l920]]);
          at = 1241; l25 = l925;
          at = 1242; l26 = l921;
          at = 1243; l27 = l922;
          at = 1244; l28 = l923;
          at = 1245; l30 = l924;
          at = 1246; l926 = K[155];
          at = 1247; l927 = rt.add(l34, l926);
          at = 1248; l34 = l927;
          at = 1249; l928 = K[156];
          at = 1250; l929 = rt.add(l32, l928);
          at = 1251; l32 = l929;
        }
      }
      at = 1252; l930 = rt.coerce("int", l26);
      at = 1253; l931 = rt.coerce("int", l27);
      at = 1254; l932 = new Map([["strokes", l23], ["active", l24], ["current", l25], ["color", l930], ["width", l931], ["status", l28], ["save", l30]]);
      at = 1255; cell(state, 0, "", input.tick).pending = l932;
      at = 1256; l15 = l932;
    }
    at = 1257; l933 = field(rt, l15, "strokes");
    at = 1258; l934 = rt.sequence_length(l933);
    at = 1259; l935 = K[157];
    at = 1260; l936 = rt.lt(l935, l934);
    at = 1261; if (rt.truthy(l936)) {
      at = 1262; l937 = field(rt, l15, "strokes");
      at = 1263; l938 = K[158];
      at = 1264; l939 = rt.sequence_get(l937, l938);
    }
    at = 1265; if (rt.truthy(l936)) {
      at = 1266; l941 = field(rt, l939, "points");
      at = 1267; l940 = l941;
    } else {
      at = 1268; l942 = K[159];
      at = 1269; l940 = l942;
    }
    at = 1270; l943 = rt.array_count(l940);
    at = 1271; l944 = K[160];
    at = 1272; l945 = rt.gt(l943, l944);
    at = 1273; if (rt.truthy(l945)) {
      at = 1274; if (rt.truthy(l936)) {
        at = 1275; l948 = field(rt, l939, "points");
        at = 1276; l947 = l948;
      } else {
        at = 1277; l949 = K[161];
        at = 1278; l947 = l949;
      }
      at = 1279; l950 = K[162];
      at = 1280; l951 = rt.array_nth(l947, l950);
      at = 1281; l946 = l951;
    } else {
      at = 1282; l952 = K[163];
      at = 1283; l946 = l952;
    }
    at = 1284; if (rt.truthy(l936)) {
      at = 1285; l954 = field(rt, l939, "width");
      at = 1286; l953 = l954;
    } else {
      at = 1287; l955 = K[164];
      at = 1288; l953 = l955;
    }
    at = 1289; l956 = K[165];
    at = 1290; l957 = rt.int_div(l953, l956);
    at = 1291; l958 = K[166];
    at = 1292; l959 = rt.max(l958, l957);
    at = 1293; l960 = K[167];
    at = 1294; l961 = rt.gt(l943, l960);
    at = 1295; if (rt.truthy(l961)) {
      at = 1296; if (rt.truthy(l936)) {
        at = 1297; l964 = field(rt, l939, "color");
        at = 1298; l963 = l964;
      } else {
        at = 1299; l965 = K[168];
        at = 1300; l963 = l965;
      }
      at = 1301; l966 = K[169];
      at = 1302; l967 = rt.sequence_get(l966, l963);
      at = 1303; l962 = l967;
    } else {
      at = 1304; l968 = K[170];
      at = 1305; l962 = l968;
    }
    at = 1306; markSet(rt, false, "circle", {at: l946, radius: l959, fill: l962});
    at = 1307; if (rt.truthy(l936)) {
      at = 1308; l970 = field(rt, l939, "points");
      at = 1309; l969 = l970;
    } else {
      at = 1310; l971 = K[171];
      at = 1311; l969 = l971;
    }
    at = 1312; l972 = K[172];
    at = 1313; l973 = rt.sub(l943, l972);
    at = 1314; l974 = K[173];
    at = 1315; l975 = rt.max(l974, l973);
    at = 1316; l976 = K[174];
    at = 1317; l977 = rt.array_slice(l969, l976, l975);
    at = 1318; if (rt.truthy(l936)) {
      at = 1319; l979 = field(rt, l939, "points");
      at = 1320; l978 = l979;
    } else {
      at = 1321; l980 = K[175];
      at = 1322; l978 = l980;
    }
    at = 1323; l981 = K[176];
    at = 1324; l982 = rt.gt(l943, l981);
    at = 1325; if (rt.truthy(l982)) {
      at = 1326; l984 = K[177];
      at = 1327; l983 = l984;
    } else {
      at = 1328; l985 = K[178];
      at = 1329; l983 = l985;
    }
    at = 1330; l986 = K[179];
    at = 1331; l987 = rt.sub(l943, l986);
    at = 1332; l988 = K[180];
    at = 1333; l989 = rt.max(l988, l987);
    at = 1334; l990 = rt.array_slice(l978, l983, l989);
    at = 1335; if (rt.truthy(l936)) {
      at = 1336; l992 = field(rt, l939, "color");
      at = 1337; l991 = l992;
    } else {
      at = 1338; l993 = K[181];
      at = 1339; l991 = l993;
    }
    at = 1340; l994 = K[182];
    at = 1341; l995 = rt.sequence_get(l994, l991);
    at = 1342; if (rt.truthy(l936)) {
      at = 1343; l997 = field(rt, l939, "width");
      at = 1344; l996 = l997;
    } else {
      at = 1345; l998 = K[183];
      at = 1346; l996 = l998;
    }
    at = 1347; markSet(rt, true, "line", {from: l977, to: l990, color: l995, width: l996});
    at = 1348; l999 = field(rt, l15, "strokes");
    at = 1349; l1000 = rt.sequence_length(l999);
    at = 1350; l1001 = K[184];
    at = 1351; l1002 = rt.lt(l1001, l1000);
    at = 1352; if (rt.truthy(l1002)) {
      at = 1353; l1003 = field(rt, l15, "strokes");
      at = 1354; l1004 = K[185];
      at = 1355; l1005 = rt.sequence_get(l1003, l1004);
    }
    at = 1356; if (rt.truthy(l1002)) {
      at = 1357; l1007 = field(rt, l1005, "points");
      at = 1358; l1006 = l1007;
    } else {
      at = 1359; l1008 = K[186];
      at = 1360; l1006 = l1008;
    }
    at = 1361; l1009 = rt.array_count(l1006);
    at = 1362; l1010 = K[187];
    at = 1363; l1011 = rt.gt(l1009, l1010);
    at = 1364; if (rt.truthy(l1011)) {
      at = 1365; if (rt.truthy(l1002)) {
        at = 1366; l1014 = field(rt, l1005, "points");
        at = 1367; l1013 = l1014;
      } else {
        at = 1368; l1015 = K[188];
        at = 1369; l1013 = l1015;
      }
      at = 1370; l1016 = K[189];
      at = 1371; l1017 = rt.array_nth(l1013, l1016);
      at = 1372; l1012 = l1017;
    } else {
      at = 1373; l1018 = K[190];
      at = 1374; l1012 = l1018;
    }
    at = 1375; if (rt.truthy(l1002)) {
      at = 1376; l1020 = field(rt, l1005, "width");
      at = 1377; l1019 = l1020;
    } else {
      at = 1378; l1021 = K[191];
      at = 1379; l1019 = l1021;
    }
    at = 1380; l1022 = K[192];
    at = 1381; l1023 = rt.int_div(l1019, l1022);
    at = 1382; l1024 = K[193];
    at = 1383; l1025 = rt.max(l1024, l1023);
    at = 1384; l1026 = K[194];
    at = 1385; l1027 = rt.gt(l1009, l1026);
    at = 1386; if (rt.truthy(l1027)) {
      at = 1387; if (rt.truthy(l1002)) {
        at = 1388; l1030 = field(rt, l1005, "color");
        at = 1389; l1029 = l1030;
      } else {
        at = 1390; l1031 = K[195];
        at = 1391; l1029 = l1031;
      }
      at = 1392; l1032 = K[196];
      at = 1393; l1033 = rt.sequence_get(l1032, l1029);
      at = 1394; l1028 = l1033;
    } else {
      at = 1395; l1034 = K[197];
      at = 1396; l1028 = l1034;
    }
    at = 1397; markSet(rt, false, "circle", {at: l1012, radius: l1025, fill: l1028});
    at = 1398; if (rt.truthy(l1002)) {
      at = 1399; l1036 = field(rt, l1005, "points");
      at = 1400; l1035 = l1036;
    } else {
      at = 1401; l1037 = K[198];
      at = 1402; l1035 = l1037;
    }
    at = 1403; l1038 = K[199];
    at = 1404; l1039 = rt.sub(l1009, l1038);
    at = 1405; l1040 = K[200];
    at = 1406; l1041 = rt.max(l1040, l1039);
    at = 1407; l1042 = K[201];
    at = 1408; l1043 = rt.array_slice(l1035, l1042, l1041);
    at = 1409; if (rt.truthy(l1002)) {
      at = 1410; l1045 = field(rt, l1005, "points");
      at = 1411; l1044 = l1045;
    } else {
      at = 1412; l1046 = K[202];
      at = 1413; l1044 = l1046;
    }
    at = 1414; l1047 = K[203];
    at = 1415; l1048 = rt.gt(l1009, l1047);
    at = 1416; if (rt.truthy(l1048)) {
      at = 1417; l1050 = K[204];
      at = 1418; l1049 = l1050;
    } else {
      at = 1419; l1051 = K[205];
      at = 1420; l1049 = l1051;
    }
    at = 1421; l1052 = K[206];
    at = 1422; l1053 = rt.sub(l1009, l1052);
    at = 1423; l1054 = K[207];
    at = 1424; l1055 = rt.max(l1054, l1053);
    at = 1425; l1056 = rt.array_slice(l1044, l1049, l1055);
    at = 1426; if (rt.truthy(l1002)) {
      at = 1427; l1058 = field(rt, l1005, "color");
      at = 1428; l1057 = l1058;
    } else {
      at = 1429; l1059 = K[208];
      at = 1430; l1057 = l1059;
    }
    at = 1431; l1060 = K[209];
    at = 1432; l1061 = rt.sequence_get(l1060, l1057);
    at = 1433; if (rt.truthy(l1002)) {
      at = 1434; l1063 = field(rt, l1005, "width");
      at = 1435; l1062 = l1063;
    } else {
      at = 1436; l1064 = K[210];
      at = 1437; l1062 = l1064;
    }
    at = 1438; markSet(rt, true, "line", {from: l1043, to: l1056, color: l1061, width: l1062});
    at = 1439; l1065 = field(rt, l15, "strokes");
    at = 1440; l1066 = rt.sequence_length(l1065);
    at = 1441; l1067 = K[211];
    at = 1442; l1068 = rt.lt(l1067, l1066);
    at = 1443; if (rt.truthy(l1068)) {
      at = 1444; l1069 = field(rt, l15, "strokes");
      at = 1445; l1070 = K[212];
      at = 1446; l1071 = rt.sequence_get(l1069, l1070);
    }
    at = 1447; if (rt.truthy(l1068)) {
      at = 1448; l1073 = field(rt, l1071, "points");
      at = 1449; l1072 = l1073;
    } else {
      at = 1450; l1074 = K[213];
      at = 1451; l1072 = l1074;
    }
    at = 1452; l1075 = rt.array_count(l1072);
    at = 1453; l1076 = K[214];
    at = 1454; l1077 = rt.gt(l1075, l1076);
    at = 1455; if (rt.truthy(l1077)) {
      at = 1456; if (rt.truthy(l1068)) {
        at = 1457; l1080 = field(rt, l1071, "points");
        at = 1458; l1079 = l1080;
      } else {
        at = 1459; l1081 = K[215];
        at = 1460; l1079 = l1081;
      }
      at = 1461; l1082 = K[216];
      at = 1462; l1083 = rt.array_nth(l1079, l1082);
      at = 1463; l1078 = l1083;
    } else {
      at = 1464; l1084 = K[217];
      at = 1465; l1078 = l1084;
    }
    at = 1466; if (rt.truthy(l1068)) {
      at = 1467; l1086 = field(rt, l1071, "width");
      at = 1468; l1085 = l1086;
    } else {
      at = 1469; l1087 = K[218];
      at = 1470; l1085 = l1087;
    }
    at = 1471; l1088 = K[219];
    at = 1472; l1089 = rt.int_div(l1085, l1088);
    at = 1473; l1090 = K[220];
    at = 1474; l1091 = rt.max(l1090, l1089);
    at = 1475; l1092 = K[221];
    at = 1476; l1093 = rt.gt(l1075, l1092);
    at = 1477; if (rt.truthy(l1093)) {
      at = 1478; if (rt.truthy(l1068)) {
        at = 1479; l1096 = field(rt, l1071, "color");
        at = 1480; l1095 = l1096;
      } else {
        at = 1481; l1097 = K[222];
        at = 1482; l1095 = l1097;
      }
      at = 1483; l1098 = K[223];
      at = 1484; l1099 = rt.sequence_get(l1098, l1095);
      at = 1485; l1094 = l1099;
    } else {
      at = 1486; l1100 = K[224];
      at = 1487; l1094 = l1100;
    }
    at = 1488; markSet(rt, false, "circle", {at: l1078, radius: l1091, fill: l1094});
    at = 1489; if (rt.truthy(l1068)) {
      at = 1490; l1102 = field(rt, l1071, "points");
      at = 1491; l1101 = l1102;
    } else {
      at = 1492; l1103 = K[225];
      at = 1493; l1101 = l1103;
    }
    at = 1494; l1104 = K[226];
    at = 1495; l1105 = rt.sub(l1075, l1104);
    at = 1496; l1106 = K[227];
    at = 1497; l1107 = rt.max(l1106, l1105);
    at = 1498; l1108 = K[228];
    at = 1499; l1109 = rt.array_slice(l1101, l1108, l1107);
    at = 1500; if (rt.truthy(l1068)) {
      at = 1501; l1111 = field(rt, l1071, "points");
      at = 1502; l1110 = l1111;
    } else {
      at = 1503; l1112 = K[229];
      at = 1504; l1110 = l1112;
    }
    at = 1505; l1113 = K[230];
    at = 1506; l1114 = rt.gt(l1075, l1113);
    at = 1507; if (rt.truthy(l1114)) {
      at = 1508; l1116 = K[231];
      at = 1509; l1115 = l1116;
    } else {
      at = 1510; l1117 = K[232];
      at = 1511; l1115 = l1117;
    }
    at = 1512; l1118 = K[233];
    at = 1513; l1119 = rt.sub(l1075, l1118);
    at = 1514; l1120 = K[234];
    at = 1515; l1121 = rt.max(l1120, l1119);
    at = 1516; l1122 = rt.array_slice(l1110, l1115, l1121);
    at = 1517; if (rt.truthy(l1068)) {
      at = 1518; l1124 = field(rt, l1071, "color");
      at = 1519; l1123 = l1124;
    } else {
      at = 1520; l1125 = K[235];
      at = 1521; l1123 = l1125;
    }
    at = 1522; l1126 = K[236];
    at = 1523; l1127 = rt.sequence_get(l1126, l1123);
    at = 1524; if (rt.truthy(l1068)) {
      at = 1525; l1129 = field(rt, l1071, "width");
      at = 1526; l1128 = l1129;
    } else {
      at = 1527; l1130 = K[237];
      at = 1528; l1128 = l1130;
    }
    at = 1529; markSet(rt, true, "line", {from: l1109, to: l1122, color: l1127, width: l1128});
    at = 1530; l1131 = field(rt, l15, "strokes");
    at = 1531; l1132 = rt.sequence_length(l1131);
    at = 1532; l1133 = K[238];
    at = 1533; l1134 = rt.lt(l1133, l1132);
    at = 1534; if (rt.truthy(l1134)) {
      at = 1535; l1135 = field(rt, l15, "strokes");
      at = 1536; l1136 = K[239];
      at = 1537; l1137 = rt.sequence_get(l1135, l1136);
    }
    at = 1538; if (rt.truthy(l1134)) {
      at = 1539; l1139 = field(rt, l1137, "points");
      at = 1540; l1138 = l1139;
    } else {
      at = 1541; l1140 = K[240];
      at = 1542; l1138 = l1140;
    }
    at = 1543; l1141 = rt.array_count(l1138);
    at = 1544; l1142 = K[241];
    at = 1545; l1143 = rt.gt(l1141, l1142);
    at = 1546; if (rt.truthy(l1143)) {
      at = 1547; if (rt.truthy(l1134)) {
        at = 1548; l1146 = field(rt, l1137, "points");
        at = 1549; l1145 = l1146;
      } else {
        at = 1550; l1147 = K[242];
        at = 1551; l1145 = l1147;
      }
      at = 1552; l1148 = K[243];
      at = 1553; l1149 = rt.array_nth(l1145, l1148);
      at = 1554; l1144 = l1149;
    } else {
      at = 1555; l1150 = K[244];
      at = 1556; l1144 = l1150;
    }
    at = 1557; if (rt.truthy(l1134)) {
      at = 1558; l1152 = field(rt, l1137, "width");
      at = 1559; l1151 = l1152;
    } else {
      at = 1560; l1153 = K[245];
      at = 1561; l1151 = l1153;
    }
    at = 1562; l1154 = K[246];
    at = 1563; l1155 = rt.int_div(l1151, l1154);
    at = 1564; l1156 = K[247];
    at = 1565; l1157 = rt.max(l1156, l1155);
    at = 1566; l1158 = K[248];
    at = 1567; l1159 = rt.gt(l1141, l1158);
    at = 1568; if (rt.truthy(l1159)) {
      at = 1569; if (rt.truthy(l1134)) {
        at = 1570; l1162 = field(rt, l1137, "color");
        at = 1571; l1161 = l1162;
      } else {
        at = 1572; l1163 = K[249];
        at = 1573; l1161 = l1163;
      }
      at = 1574; l1164 = K[250];
      at = 1575; l1165 = rt.sequence_get(l1164, l1161);
      at = 1576; l1160 = l1165;
    } else {
      at = 1577; l1166 = K[251];
      at = 1578; l1160 = l1166;
    }
    at = 1579; markSet(rt, false, "circle", {at: l1144, radius: l1157, fill: l1160});
    at = 1580; if (rt.truthy(l1134)) {
      at = 1581; l1168 = field(rt, l1137, "points");
      at = 1582; l1167 = l1168;
    } else {
      at = 1583; l1169 = K[252];
      at = 1584; l1167 = l1169;
    }
    at = 1585; l1170 = K[253];
    at = 1586; l1171 = rt.sub(l1141, l1170);
    at = 1587; l1172 = K[254];
    at = 1588; l1173 = rt.max(l1172, l1171);
    at = 1589; l1174 = K[255];
    at = 1590; l1175 = rt.array_slice(l1167, l1174, l1173);
    at = 1591; if (rt.truthy(l1134)) {
      at = 1592; l1177 = field(rt, l1137, "points");
      at = 1593; l1176 = l1177;
    } else {
      at = 1594; l1178 = K[256];
      at = 1595; l1176 = l1178;
    }
    at = 1596; l1179 = K[257];
    at = 1597; l1180 = rt.gt(l1141, l1179);
    at = 1598; if (rt.truthy(l1180)) {
      at = 1599; l1182 = K[258];
      at = 1600; l1181 = l1182;
    } else {
      at = 1601; l1183 = K[259];
      at = 1602; l1181 = l1183;
    }
    at = 1603; l1184 = K[260];
    at = 1604; l1185 = rt.sub(l1141, l1184);
    at = 1605; l1186 = K[261];
    at = 1606; l1187 = rt.max(l1186, l1185);
    at = 1607; l1188 = rt.array_slice(l1176, l1181, l1187);
    at = 1608; if (rt.truthy(l1134)) {
      at = 1609; l1190 = field(rt, l1137, "color");
      at = 1610; l1189 = l1190;
    } else {
      at = 1611; l1191 = K[262];
      at = 1612; l1189 = l1191;
    }
    at = 1613; l1192 = K[263];
    at = 1614; l1193 = rt.sequence_get(l1192, l1189);
    at = 1615; if (rt.truthy(l1134)) {
      at = 1616; l1195 = field(rt, l1137, "width");
      at = 1617; l1194 = l1195;
    } else {
      at = 1618; l1196 = K[264];
      at = 1619; l1194 = l1196;
    }
    at = 1620; markSet(rt, true, "line", {from: l1175, to: l1188, color: l1193, width: l1194});
    at = 1621; l1197 = field(rt, l15, "strokes");
    at = 1622; l1198 = rt.sequence_length(l1197);
    at = 1623; l1199 = K[265];
    at = 1624; l1200 = rt.lt(l1199, l1198);
    at = 1625; if (rt.truthy(l1200)) {
      at = 1626; l1201 = field(rt, l15, "strokes");
      at = 1627; l1202 = K[266];
      at = 1628; l1203 = rt.sequence_get(l1201, l1202);
    }
    at = 1629; if (rt.truthy(l1200)) {
      at = 1630; l1205 = field(rt, l1203, "points");
      at = 1631; l1204 = l1205;
    } else {
      at = 1632; l1206 = K[267];
      at = 1633; l1204 = l1206;
    }
    at = 1634; l1207 = rt.array_count(l1204);
    at = 1635; l1208 = K[268];
    at = 1636; l1209 = rt.gt(l1207, l1208);
    at = 1637; if (rt.truthy(l1209)) {
      at = 1638; if (rt.truthy(l1200)) {
        at = 1639; l1212 = field(rt, l1203, "points");
        at = 1640; l1211 = l1212;
      } else {
        at = 1641; l1213 = K[269];
        at = 1642; l1211 = l1213;
      }
      at = 1643; l1214 = K[270];
      at = 1644; l1215 = rt.array_nth(l1211, l1214);
      at = 1645; l1210 = l1215;
    } else {
      at = 1646; l1216 = K[271];
      at = 1647; l1210 = l1216;
    }
    at = 1648; if (rt.truthy(l1200)) {
      at = 1649; l1218 = field(rt, l1203, "width");
      at = 1650; l1217 = l1218;
    } else {
      at = 1651; l1219 = K[272];
      at = 1652; l1217 = l1219;
    }
    at = 1653; l1220 = K[273];
    at = 1654; l1221 = rt.int_div(l1217, l1220);
    at = 1655; l1222 = K[274];
    at = 1656; l1223 = rt.max(l1222, l1221);
    at = 1657; l1224 = K[275];
    at = 1658; l1225 = rt.gt(l1207, l1224);
    at = 1659; if (rt.truthy(l1225)) {
      at = 1660; if (rt.truthy(l1200)) {
        at = 1661; l1228 = field(rt, l1203, "color");
        at = 1662; l1227 = l1228;
      } else {
        at = 1663; l1229 = K[276];
        at = 1664; l1227 = l1229;
      }
      at = 1665; l1230 = K[277];
      at = 1666; l1231 = rt.sequence_get(l1230, l1227);
      at = 1667; l1226 = l1231;
    } else {
      at = 1668; l1232 = K[278];
      at = 1669; l1226 = l1232;
    }
    at = 1670; markSet(rt, false, "circle", {at: l1210, radius: l1223, fill: l1226});
    at = 1671; if (rt.truthy(l1200)) {
      at = 1672; l1234 = field(rt, l1203, "points");
      at = 1673; l1233 = l1234;
    } else {
      at = 1674; l1235 = K[279];
      at = 1675; l1233 = l1235;
    }
    at = 1676; l1236 = K[280];
    at = 1677; l1237 = rt.sub(l1207, l1236);
    at = 1678; l1238 = K[281];
    at = 1679; l1239 = rt.max(l1238, l1237);
    at = 1680; l1240 = K[282];
    at = 1681; l1241 = rt.array_slice(l1233, l1240, l1239);
    at = 1682; if (rt.truthy(l1200)) {
      at = 1683; l1243 = field(rt, l1203, "points");
      at = 1684; l1242 = l1243;
    } else {
      at = 1685; l1244 = K[283];
      at = 1686; l1242 = l1244;
    }
    at = 1687; l1245 = K[284];
    at = 1688; l1246 = rt.gt(l1207, l1245);
    at = 1689; if (rt.truthy(l1246)) {
      at = 1690; l1248 = K[285];
      at = 1691; l1247 = l1248;
    } else {
      at = 1692; l1249 = K[286];
      at = 1693; l1247 = l1249;
    }
    at = 1694; l1250 = K[287];
    at = 1695; l1251 = rt.sub(l1207, l1250);
    at = 1696; l1252 = K[288];
    at = 1697; l1253 = rt.max(l1252, l1251);
    at = 1698; l1254 = rt.array_slice(l1242, l1247, l1253);
    at = 1699; if (rt.truthy(l1200)) {
      at = 1700; l1256 = field(rt, l1203, "color");
      at = 1701; l1255 = l1256;
    } else {
      at = 1702; l1257 = K[289];
      at = 1703; l1255 = l1257;
    }
    at = 1704; l1258 = K[290];
    at = 1705; l1259 = rt.sequence_get(l1258, l1255);
    at = 1706; if (rt.truthy(l1200)) {
      at = 1707; l1261 = field(rt, l1203, "width");
      at = 1708; l1260 = l1261;
    } else {
      at = 1709; l1262 = K[291];
      at = 1710; l1260 = l1262;
    }
    at = 1711; markSet(rt, true, "line", {from: l1241, to: l1254, color: l1259, width: l1260});
    at = 1712; l1263 = field(rt, l15, "strokes");
    at = 1713; l1264 = rt.sequence_length(l1263);
    at = 1714; l1265 = K[292];
    at = 1715; l1266 = rt.lt(l1265, l1264);
    at = 1716; if (rt.truthy(l1266)) {
      at = 1717; l1267 = field(rt, l15, "strokes");
      at = 1718; l1268 = K[293];
      at = 1719; l1269 = rt.sequence_get(l1267, l1268);
    }
    at = 1720; if (rt.truthy(l1266)) {
      at = 1721; l1271 = field(rt, l1269, "points");
      at = 1722; l1270 = l1271;
    } else {
      at = 1723; l1272 = K[294];
      at = 1724; l1270 = l1272;
    }
    at = 1725; l1273 = rt.array_count(l1270);
    at = 1726; l1274 = K[295];
    at = 1727; l1275 = rt.gt(l1273, l1274);
    at = 1728; if (rt.truthy(l1275)) {
      at = 1729; if (rt.truthy(l1266)) {
        at = 1730; l1278 = field(rt, l1269, "points");
        at = 1731; l1277 = l1278;
      } else {
        at = 1732; l1279 = K[296];
        at = 1733; l1277 = l1279;
      }
      at = 1734; l1280 = K[297];
      at = 1735; l1281 = rt.array_nth(l1277, l1280);
      at = 1736; l1276 = l1281;
    } else {
      at = 1737; l1282 = K[298];
      at = 1738; l1276 = l1282;
    }
    at = 1739; if (rt.truthy(l1266)) {
      at = 1740; l1284 = field(rt, l1269, "width");
      at = 1741; l1283 = l1284;
    } else {
      at = 1742; l1285 = K[299];
      at = 1743; l1283 = l1285;
    }
    at = 1744; l1286 = K[300];
    at = 1745; l1287 = rt.int_div(l1283, l1286);
    at = 1746; l1288 = K[301];
    at = 1747; l1289 = rt.max(l1288, l1287);
    at = 1748; l1290 = K[302];
    at = 1749; l1291 = rt.gt(l1273, l1290);
    at = 1750; if (rt.truthy(l1291)) {
      at = 1751; if (rt.truthy(l1266)) {
        at = 1752; l1294 = field(rt, l1269, "color");
        at = 1753; l1293 = l1294;
      } else {
        at = 1754; l1295 = K[303];
        at = 1755; l1293 = l1295;
      }
      at = 1756; l1296 = K[304];
      at = 1757; l1297 = rt.sequence_get(l1296, l1293);
      at = 1758; l1292 = l1297;
    } else {
      at = 1759; l1298 = K[305];
      at = 1760; l1292 = l1298;
    }
    at = 1761; markSet(rt, false, "circle", {at: l1276, radius: l1289, fill: l1292});
    at = 1762; if (rt.truthy(l1266)) {
      at = 1763; l1300 = field(rt, l1269, "points");
      at = 1764; l1299 = l1300;
    } else {
      at = 1765; l1301 = K[306];
      at = 1766; l1299 = l1301;
    }
    at = 1767; l1302 = K[307];
    at = 1768; l1303 = rt.sub(l1273, l1302);
    at = 1769; l1304 = K[308];
    at = 1770; l1305 = rt.max(l1304, l1303);
    at = 1771; l1306 = K[309];
    at = 1772; l1307 = rt.array_slice(l1299, l1306, l1305);
    at = 1773; if (rt.truthy(l1266)) {
      at = 1774; l1309 = field(rt, l1269, "points");
      at = 1775; l1308 = l1309;
    } else {
      at = 1776; l1310 = K[310];
      at = 1777; l1308 = l1310;
    }
    at = 1778; l1311 = K[311];
    at = 1779; l1312 = rt.gt(l1273, l1311);
    at = 1780; if (rt.truthy(l1312)) {
      at = 1781; l1314 = K[312];
      at = 1782; l1313 = l1314;
    } else {
      at = 1783; l1315 = K[313];
      at = 1784; l1313 = l1315;
    }
    at = 1785; l1316 = K[314];
    at = 1786; l1317 = rt.sub(l1273, l1316);
    at = 1787; l1318 = K[315];
    at = 1788; l1319 = rt.max(l1318, l1317);
    at = 1789; l1320 = rt.array_slice(l1308, l1313, l1319);
    at = 1790; if (rt.truthy(l1266)) {
      at = 1791; l1322 = field(rt, l1269, "color");
      at = 1792; l1321 = l1322;
    } else {
      at = 1793; l1323 = K[316];
      at = 1794; l1321 = l1323;
    }
    at = 1795; l1324 = K[317];
    at = 1796; l1325 = rt.sequence_get(l1324, l1321);
    at = 1797; if (rt.truthy(l1266)) {
      at = 1798; l1327 = field(rt, l1269, "width");
      at = 1799; l1326 = l1327;
    } else {
      at = 1800; l1328 = K[318];
      at = 1801; l1326 = l1328;
    }
    at = 1802; markSet(rt, true, "line", {from: l1307, to: l1320, color: l1325, width: l1326});
    at = 1803; l1329 = field(rt, l15, "strokes");
    at = 1804; l1330 = rt.sequence_length(l1329);
    at = 1805; l1331 = K[319];
    at = 1806; l1332 = rt.lt(l1331, l1330);
    at = 1807; if (rt.truthy(l1332)) {
      at = 1808; l1333 = field(rt, l15, "strokes");
      at = 1809; l1334 = K[320];
      at = 1810; l1335 = rt.sequence_get(l1333, l1334);
    }
    at = 1811; if (rt.truthy(l1332)) {
      at = 1812; l1337 = field(rt, l1335, "points");
      at = 1813; l1336 = l1337;
    } else {
      at = 1814; l1338 = K[321];
      at = 1815; l1336 = l1338;
    }
    at = 1816; l1339 = rt.array_count(l1336);
    at = 1817; l1340 = K[322];
    at = 1818; l1341 = rt.gt(l1339, l1340);
    at = 1819; if (rt.truthy(l1341)) {
      at = 1820; if (rt.truthy(l1332)) {
        at = 1821; l1344 = field(rt, l1335, "points");
        at = 1822; l1343 = l1344;
      } else {
        at = 1823; l1345 = K[323];
        at = 1824; l1343 = l1345;
      }
      at = 1825; l1346 = K[324];
      at = 1826; l1347 = rt.array_nth(l1343, l1346);
      at = 1827; l1342 = l1347;
    } else {
      at = 1828; l1348 = K[325];
      at = 1829; l1342 = l1348;
    }
    at = 1830; if (rt.truthy(l1332)) {
      at = 1831; l1350 = field(rt, l1335, "width");
      at = 1832; l1349 = l1350;
    } else {
      at = 1833; l1351 = K[326];
      at = 1834; l1349 = l1351;
    }
    at = 1835; l1352 = K[327];
    at = 1836; l1353 = rt.int_div(l1349, l1352);
    at = 1837; l1354 = K[328];
    at = 1838; l1355 = rt.max(l1354, l1353);
    at = 1839; l1356 = K[329];
    at = 1840; l1357 = rt.gt(l1339, l1356);
    at = 1841; if (rt.truthy(l1357)) {
      at = 1842; if (rt.truthy(l1332)) {
        at = 1843; l1360 = field(rt, l1335, "color");
        at = 1844; l1359 = l1360;
      } else {
        at = 1845; l1361 = K[330];
        at = 1846; l1359 = l1361;
      }
      at = 1847; l1362 = K[331];
      at = 1848; l1363 = rt.sequence_get(l1362, l1359);
      at = 1849; l1358 = l1363;
    } else {
      at = 1850; l1364 = K[332];
      at = 1851; l1358 = l1364;
    }
    at = 1852; markSet(rt, false, "circle", {at: l1342, radius: l1355, fill: l1358});
    at = 1853; if (rt.truthy(l1332)) {
      at = 1854; l1366 = field(rt, l1335, "points");
      at = 1855; l1365 = l1366;
    } else {
      at = 1856; l1367 = K[333];
      at = 1857; l1365 = l1367;
    }
    at = 1858; l1368 = K[334];
    at = 1859; l1369 = rt.sub(l1339, l1368);
    at = 1860; l1370 = K[335];
    at = 1861; l1371 = rt.max(l1370, l1369);
    at = 1862; l1372 = K[336];
    at = 1863; l1373 = rt.array_slice(l1365, l1372, l1371);
    at = 1864; if (rt.truthy(l1332)) {
      at = 1865; l1375 = field(rt, l1335, "points");
      at = 1866; l1374 = l1375;
    } else {
      at = 1867; l1376 = K[337];
      at = 1868; l1374 = l1376;
    }
    at = 1869; l1377 = K[338];
    at = 1870; l1378 = rt.gt(l1339, l1377);
    at = 1871; if (rt.truthy(l1378)) {
      at = 1872; l1380 = K[339];
      at = 1873; l1379 = l1380;
    } else {
      at = 1874; l1381 = K[340];
      at = 1875; l1379 = l1381;
    }
    at = 1876; l1382 = K[341];
    at = 1877; l1383 = rt.sub(l1339, l1382);
    at = 1878; l1384 = K[342];
    at = 1879; l1385 = rt.max(l1384, l1383);
    at = 1880; l1386 = rt.array_slice(l1374, l1379, l1385);
    at = 1881; if (rt.truthy(l1332)) {
      at = 1882; l1388 = field(rt, l1335, "color");
      at = 1883; l1387 = l1388;
    } else {
      at = 1884; l1389 = K[343];
      at = 1885; l1387 = l1389;
    }
    at = 1886; l1390 = K[344];
    at = 1887; l1391 = rt.sequence_get(l1390, l1387);
    at = 1888; if (rt.truthy(l1332)) {
      at = 1889; l1393 = field(rt, l1335, "width");
      at = 1890; l1392 = l1393;
    } else {
      at = 1891; l1394 = K[345];
      at = 1892; l1392 = l1394;
    }
    at = 1893; markSet(rt, true, "line", {from: l1373, to: l1386, color: l1391, width: l1392});
    at = 1894; l1395 = field(rt, l15, "strokes");
    at = 1895; l1396 = rt.sequence_length(l1395);
    at = 1896; l1397 = K[346];
    at = 1897; l1398 = rt.lt(l1397, l1396);
    at = 1898; if (rt.truthy(l1398)) {
      at = 1899; l1399 = field(rt, l15, "strokes");
      at = 1900; l1400 = K[347];
      at = 1901; l1401 = rt.sequence_get(l1399, l1400);
    }
    at = 1902; if (rt.truthy(l1398)) {
      at = 1903; l1403 = field(rt, l1401, "points");
      at = 1904; l1402 = l1403;
    } else {
      at = 1905; l1404 = K[348];
      at = 1906; l1402 = l1404;
    }
    at = 1907; l1405 = rt.array_count(l1402);
    at = 1908; l1406 = K[349];
    at = 1909; l1407 = rt.gt(l1405, l1406);
    at = 1910; if (rt.truthy(l1407)) {
      at = 1911; if (rt.truthy(l1398)) {
        at = 1912; l1410 = field(rt, l1401, "points");
        at = 1913; l1409 = l1410;
      } else {
        at = 1914; l1411 = K[350];
        at = 1915; l1409 = l1411;
      }
      at = 1916; l1412 = K[351];
      at = 1917; l1413 = rt.array_nth(l1409, l1412);
      at = 1918; l1408 = l1413;
    } else {
      at = 1919; l1414 = K[352];
      at = 1920; l1408 = l1414;
    }
    at = 1921; if (rt.truthy(l1398)) {
      at = 1922; l1416 = field(rt, l1401, "width");
      at = 1923; l1415 = l1416;
    } else {
      at = 1924; l1417 = K[353];
      at = 1925; l1415 = l1417;
    }
    at = 1926; l1418 = K[354];
    at = 1927; l1419 = rt.int_div(l1415, l1418);
    at = 1928; l1420 = K[355];
    at = 1929; l1421 = rt.max(l1420, l1419);
    at = 1930; l1422 = K[356];
    at = 1931; l1423 = rt.gt(l1405, l1422);
    at = 1932; if (rt.truthy(l1423)) {
      at = 1933; if (rt.truthy(l1398)) {
        at = 1934; l1426 = field(rt, l1401, "color");
        at = 1935; l1425 = l1426;
      } else {
        at = 1936; l1427 = K[357];
        at = 1937; l1425 = l1427;
      }
      at = 1938; l1428 = K[358];
      at = 1939; l1429 = rt.sequence_get(l1428, l1425);
      at = 1940; l1424 = l1429;
    } else {
      at = 1941; l1430 = K[359];
      at = 1942; l1424 = l1430;
    }
    at = 1943; markSet(rt, false, "circle", {at: l1408, radius: l1421, fill: l1424});
    at = 1944; if (rt.truthy(l1398)) {
      at = 1945; l1432 = field(rt, l1401, "points");
      at = 1946; l1431 = l1432;
    } else {
      at = 1947; l1433 = K[360];
      at = 1948; l1431 = l1433;
    }
    at = 1949; l1434 = K[361];
    at = 1950; l1435 = rt.sub(l1405, l1434);
    at = 1951; l1436 = K[362];
    at = 1952; l1437 = rt.max(l1436, l1435);
    at = 1953; l1438 = K[363];
    at = 1954; l1439 = rt.array_slice(l1431, l1438, l1437);
    at = 1955; if (rt.truthy(l1398)) {
      at = 1956; l1441 = field(rt, l1401, "points");
      at = 1957; l1440 = l1441;
    } else {
      at = 1958; l1442 = K[364];
      at = 1959; l1440 = l1442;
    }
    at = 1960; l1443 = K[365];
    at = 1961; l1444 = rt.gt(l1405, l1443);
    at = 1962; if (rt.truthy(l1444)) {
      at = 1963; l1446 = K[366];
      at = 1964; l1445 = l1446;
    } else {
      at = 1965; l1447 = K[367];
      at = 1966; l1445 = l1447;
    }
    at = 1967; l1448 = K[368];
    at = 1968; l1449 = rt.sub(l1405, l1448);
    at = 1969; l1450 = K[369];
    at = 1970; l1451 = rt.max(l1450, l1449);
    at = 1971; l1452 = rt.array_slice(l1440, l1445, l1451);
    at = 1972; if (rt.truthy(l1398)) {
      at = 1973; l1454 = field(rt, l1401, "color");
      at = 1974; l1453 = l1454;
    } else {
      at = 1975; l1455 = K[370];
      at = 1976; l1453 = l1455;
    }
    at = 1977; l1456 = K[371];
    at = 1978; l1457 = rt.sequence_get(l1456, l1453);
    at = 1979; if (rt.truthy(l1398)) {
      at = 1980; l1459 = field(rt, l1401, "width");
      at = 1981; l1458 = l1459;
    } else {
      at = 1982; l1460 = K[372];
      at = 1983; l1458 = l1460;
    }
    at = 1984; markSet(rt, true, "line", {from: l1439, to: l1452, color: l1457, width: l1458});
    at = 1985; l1461 = field(rt, l15, "strokes");
    at = 1986; l1462 = rt.sequence_length(l1461);
    at = 1987; l1463 = K[373];
    at = 1988; l1464 = rt.lt(l1463, l1462);
    at = 1989; if (rt.truthy(l1464)) {
      at = 1990; l1465 = field(rt, l15, "strokes");
      at = 1991; l1466 = K[374];
      at = 1992; l1467 = rt.sequence_get(l1465, l1466);
    }
    at = 1993; if (rt.truthy(l1464)) {
      at = 1994; l1469 = field(rt, l1467, "points");
      at = 1995; l1468 = l1469;
    } else {
      at = 1996; l1470 = K[375];
      at = 1997; l1468 = l1470;
    }
    at = 1998; l1471 = rt.array_count(l1468);
    at = 1999; l1472 = K[376];
    at = 2000; l1473 = rt.gt(l1471, l1472);
    at = 2001; if (rt.truthy(l1473)) {
      at = 2002; if (rt.truthy(l1464)) {
        at = 2003; l1476 = field(rt, l1467, "points");
        at = 2004; l1475 = l1476;
      } else {
        at = 2005; l1477 = K[377];
        at = 2006; l1475 = l1477;
      }
      at = 2007; l1478 = K[378];
      at = 2008; l1479 = rt.array_nth(l1475, l1478);
      at = 2009; l1474 = l1479;
    } else {
      at = 2010; l1480 = K[379];
      at = 2011; l1474 = l1480;
    }
    at = 2012; if (rt.truthy(l1464)) {
      at = 2013; l1482 = field(rt, l1467, "width");
      at = 2014; l1481 = l1482;
    } else {
      at = 2015; l1483 = K[380];
      at = 2016; l1481 = l1483;
    }
    at = 2017; l1484 = K[381];
    at = 2018; l1485 = rt.int_div(l1481, l1484);
    at = 2019; l1486 = K[382];
    at = 2020; l1487 = rt.max(l1486, l1485);
    at = 2021; l1488 = K[383];
    at = 2022; l1489 = rt.gt(l1471, l1488);
    at = 2023; if (rt.truthy(l1489)) {
      at = 2024; if (rt.truthy(l1464)) {
        at = 2025; l1492 = field(rt, l1467, "color");
        at = 2026; l1491 = l1492;
      } else {
        at = 2027; l1493 = K[384];
        at = 2028; l1491 = l1493;
      }
      at = 2029; l1494 = K[385];
      at = 2030; l1495 = rt.sequence_get(l1494, l1491);
      at = 2031; l1490 = l1495;
    } else {
      at = 2032; l1496 = K[386];
      at = 2033; l1490 = l1496;
    }
    at = 2034; markSet(rt, false, "circle", {at: l1474, radius: l1487, fill: l1490});
    at = 2035; if (rt.truthy(l1464)) {
      at = 2036; l1498 = field(rt, l1467, "points");
      at = 2037; l1497 = l1498;
    } else {
      at = 2038; l1499 = K[387];
      at = 2039; l1497 = l1499;
    }
    at = 2040; l1500 = K[388];
    at = 2041; l1501 = rt.sub(l1471, l1500);
    at = 2042; l1502 = K[389];
    at = 2043; l1503 = rt.max(l1502, l1501);
    at = 2044; l1504 = K[390];
    at = 2045; l1505 = rt.array_slice(l1497, l1504, l1503);
    at = 2046; if (rt.truthy(l1464)) {
      at = 2047; l1507 = field(rt, l1467, "points");
      at = 2048; l1506 = l1507;
    } else {
      at = 2049; l1508 = K[391];
      at = 2050; l1506 = l1508;
    }
    at = 2051; l1509 = K[392];
    at = 2052; l1510 = rt.gt(l1471, l1509);
    at = 2053; if (rt.truthy(l1510)) {
      at = 2054; l1512 = K[393];
      at = 2055; l1511 = l1512;
    } else {
      at = 2056; l1513 = K[394];
      at = 2057; l1511 = l1513;
    }
    at = 2058; l1514 = K[395];
    at = 2059; l1515 = rt.sub(l1471, l1514);
    at = 2060; l1516 = K[396];
    at = 2061; l1517 = rt.max(l1516, l1515);
    at = 2062; l1518 = rt.array_slice(l1506, l1511, l1517);
    at = 2063; if (rt.truthy(l1464)) {
      at = 2064; l1520 = field(rt, l1467, "color");
      at = 2065; l1519 = l1520;
    } else {
      at = 2066; l1521 = K[397];
      at = 2067; l1519 = l1521;
    }
    at = 2068; l1522 = K[398];
    at = 2069; l1523 = rt.sequence_get(l1522, l1519);
    at = 2070; if (rt.truthy(l1464)) {
      at = 2071; l1525 = field(rt, l1467, "width");
      at = 2072; l1524 = l1525;
    } else {
      at = 2073; l1526 = K[399];
      at = 2074; l1524 = l1526;
    }
    at = 2075; markSet(rt, true, "line", {from: l1505, to: l1518, color: l1523, width: l1524});
    at = 2076; l1527 = field(rt, l15, "strokes");
    at = 2077; l1528 = rt.sequence_length(l1527);
    at = 2078; l1529 = K[400];
    at = 2079; l1530 = rt.lt(l1529, l1528);
    at = 2080; if (rt.truthy(l1530)) {
      at = 2081; l1531 = field(rt, l15, "strokes");
      at = 2082; l1532 = K[401];
      at = 2083; l1533 = rt.sequence_get(l1531, l1532);
    }
    at = 2084; if (rt.truthy(l1530)) {
      at = 2085; l1535 = field(rt, l1533, "points");
      at = 2086; l1534 = l1535;
    } else {
      at = 2087; l1536 = K[402];
      at = 2088; l1534 = l1536;
    }
    at = 2089; l1537 = rt.array_count(l1534);
    at = 2090; l1538 = K[403];
    at = 2091; l1539 = rt.gt(l1537, l1538);
    at = 2092; if (rt.truthy(l1539)) {
      at = 2093; if (rt.truthy(l1530)) {
        at = 2094; l1542 = field(rt, l1533, "points");
        at = 2095; l1541 = l1542;
      } else {
        at = 2096; l1543 = K[404];
        at = 2097; l1541 = l1543;
      }
      at = 2098; l1544 = K[405];
      at = 2099; l1545 = rt.array_nth(l1541, l1544);
      at = 2100; l1540 = l1545;
    } else {
      at = 2101; l1546 = K[406];
      at = 2102; l1540 = l1546;
    }
    at = 2103; if (rt.truthy(l1530)) {
      at = 2104; l1548 = field(rt, l1533, "width");
      at = 2105; l1547 = l1548;
    } else {
      at = 2106; l1549 = K[407];
      at = 2107; l1547 = l1549;
    }
    at = 2108; l1550 = K[408];
    at = 2109; l1551 = rt.int_div(l1547, l1550);
    at = 2110; l1552 = K[409];
    at = 2111; l1553 = rt.max(l1552, l1551);
    at = 2112; l1554 = K[410];
    at = 2113; l1555 = rt.gt(l1537, l1554);
    at = 2114; if (rt.truthy(l1555)) {
      at = 2115; if (rt.truthy(l1530)) {
        at = 2116; l1558 = field(rt, l1533, "color");
        at = 2117; l1557 = l1558;
      } else {
        at = 2118; l1559 = K[411];
        at = 2119; l1557 = l1559;
      }
      at = 2120; l1560 = K[412];
      at = 2121; l1561 = rt.sequence_get(l1560, l1557);
      at = 2122; l1556 = l1561;
    } else {
      at = 2123; l1562 = K[413];
      at = 2124; l1556 = l1562;
    }
    at = 2125; markSet(rt, false, "circle", {at: l1540, radius: l1553, fill: l1556});
    at = 2126; if (rt.truthy(l1530)) {
      at = 2127; l1564 = field(rt, l1533, "points");
      at = 2128; l1563 = l1564;
    } else {
      at = 2129; l1565 = K[414];
      at = 2130; l1563 = l1565;
    }
    at = 2131; l1566 = K[415];
    at = 2132; l1567 = rt.sub(l1537, l1566);
    at = 2133; l1568 = K[416];
    at = 2134; l1569 = rt.max(l1568, l1567);
    at = 2135; l1570 = K[417];
    at = 2136; l1571 = rt.array_slice(l1563, l1570, l1569);
    at = 2137; if (rt.truthy(l1530)) {
      at = 2138; l1573 = field(rt, l1533, "points");
      at = 2139; l1572 = l1573;
    } else {
      at = 2140; l1574 = K[418];
      at = 2141; l1572 = l1574;
    }
    at = 2142; l1575 = K[419];
    at = 2143; l1576 = rt.gt(l1537, l1575);
    at = 2144; if (rt.truthy(l1576)) {
      at = 2145; l1578 = K[420];
      at = 2146; l1577 = l1578;
    } else {
      at = 2147; l1579 = K[421];
      at = 2148; l1577 = l1579;
    }
    at = 2149; l1580 = K[422];
    at = 2150; l1581 = rt.sub(l1537, l1580);
    at = 2151; l1582 = K[423];
    at = 2152; l1583 = rt.max(l1582, l1581);
    at = 2153; l1584 = rt.array_slice(l1572, l1577, l1583);
    at = 2154; if (rt.truthy(l1530)) {
      at = 2155; l1586 = field(rt, l1533, "color");
      at = 2156; l1585 = l1586;
    } else {
      at = 2157; l1587 = K[424];
      at = 2158; l1585 = l1587;
    }
    at = 2159; l1588 = K[425];
    at = 2160; l1589 = rt.sequence_get(l1588, l1585);
    at = 2161; if (rt.truthy(l1530)) {
      at = 2162; l1591 = field(rt, l1533, "width");
      at = 2163; l1590 = l1591;
    } else {
      at = 2164; l1592 = K[426];
      at = 2165; l1590 = l1592;
    }
    at = 2166; markSet(rt, true, "line", {from: l1571, to: l1584, color: l1589, width: l1590});
    at = 2167; l1593 = field(rt, l15, "strokes");
    at = 2168; l1594 = rt.sequence_length(l1593);
    at = 2169; l1595 = K[427];
    at = 2170; l1596 = rt.lt(l1595, l1594);
    at = 2171; if (rt.truthy(l1596)) {
      at = 2172; l1597 = field(rt, l15, "strokes");
      at = 2173; l1598 = K[428];
      at = 2174; l1599 = rt.sequence_get(l1597, l1598);
    }
    at = 2175; if (rt.truthy(l1596)) {
      at = 2176; l1601 = field(rt, l1599, "points");
      at = 2177; l1600 = l1601;
    } else {
      at = 2178; l1602 = K[429];
      at = 2179; l1600 = l1602;
    }
    at = 2180; l1603 = rt.array_count(l1600);
    at = 2181; l1604 = K[430];
    at = 2182; l1605 = rt.gt(l1603, l1604);
    at = 2183; if (rt.truthy(l1605)) {
      at = 2184; if (rt.truthy(l1596)) {
        at = 2185; l1608 = field(rt, l1599, "points");
        at = 2186; l1607 = l1608;
      } else {
        at = 2187; l1609 = K[431];
        at = 2188; l1607 = l1609;
      }
      at = 2189; l1610 = K[432];
      at = 2190; l1611 = rt.array_nth(l1607, l1610);
      at = 2191; l1606 = l1611;
    } else {
      at = 2192; l1612 = K[433];
      at = 2193; l1606 = l1612;
    }
    at = 2194; if (rt.truthy(l1596)) {
      at = 2195; l1614 = field(rt, l1599, "width");
      at = 2196; l1613 = l1614;
    } else {
      at = 2197; l1615 = K[434];
      at = 2198; l1613 = l1615;
    }
    at = 2199; l1616 = K[435];
    at = 2200; l1617 = rt.int_div(l1613, l1616);
    at = 2201; l1618 = K[436];
    at = 2202; l1619 = rt.max(l1618, l1617);
    at = 2203; l1620 = K[437];
    at = 2204; l1621 = rt.gt(l1603, l1620);
    at = 2205; if (rt.truthy(l1621)) {
      at = 2206; if (rt.truthy(l1596)) {
        at = 2207; l1624 = field(rt, l1599, "color");
        at = 2208; l1623 = l1624;
      } else {
        at = 2209; l1625 = K[438];
        at = 2210; l1623 = l1625;
      }
      at = 2211; l1626 = K[439];
      at = 2212; l1627 = rt.sequence_get(l1626, l1623);
      at = 2213; l1622 = l1627;
    } else {
      at = 2214; l1628 = K[440];
      at = 2215; l1622 = l1628;
    }
    at = 2216; markSet(rt, false, "circle", {at: l1606, radius: l1619, fill: l1622});
    at = 2217; if (rt.truthy(l1596)) {
      at = 2218; l1630 = field(rt, l1599, "points");
      at = 2219; l1629 = l1630;
    } else {
      at = 2220; l1631 = K[441];
      at = 2221; l1629 = l1631;
    }
    at = 2222; l1632 = K[442];
    at = 2223; l1633 = rt.sub(l1603, l1632);
    at = 2224; l1634 = K[443];
    at = 2225; l1635 = rt.max(l1634, l1633);
    at = 2226; l1636 = K[444];
    at = 2227; l1637 = rt.array_slice(l1629, l1636, l1635);
    at = 2228; if (rt.truthy(l1596)) {
      at = 2229; l1639 = field(rt, l1599, "points");
      at = 2230; l1638 = l1639;
    } else {
      at = 2231; l1640 = K[445];
      at = 2232; l1638 = l1640;
    }
    at = 2233; l1641 = K[446];
    at = 2234; l1642 = rt.gt(l1603, l1641);
    at = 2235; if (rt.truthy(l1642)) {
      at = 2236; l1644 = K[447];
      at = 2237; l1643 = l1644;
    } else {
      at = 2238; l1645 = K[448];
      at = 2239; l1643 = l1645;
    }
    at = 2240; l1646 = K[449];
    at = 2241; l1647 = rt.sub(l1603, l1646);
    at = 2242; l1648 = K[450];
    at = 2243; l1649 = rt.max(l1648, l1647);
    at = 2244; l1650 = rt.array_slice(l1638, l1643, l1649);
    at = 2245; if (rt.truthy(l1596)) {
      at = 2246; l1652 = field(rt, l1599, "color");
      at = 2247; l1651 = l1652;
    } else {
      at = 2248; l1653 = K[451];
      at = 2249; l1651 = l1653;
    }
    at = 2250; l1654 = K[452];
    at = 2251; l1655 = rt.sequence_get(l1654, l1651);
    at = 2252; if (rt.truthy(l1596)) {
      at = 2253; l1657 = field(rt, l1599, "width");
      at = 2254; l1656 = l1657;
    } else {
      at = 2255; l1658 = K[453];
      at = 2256; l1656 = l1658;
    }
    at = 2257; markSet(rt, true, "line", {from: l1637, to: l1650, color: l1655, width: l1656});
    at = 2258; l1659 = field(rt, l15, "strokes");
    at = 2259; l1660 = rt.sequence_length(l1659);
    at = 2260; l1661 = K[454];
    at = 2261; l1662 = rt.lt(l1661, l1660);
    at = 2262; if (rt.truthy(l1662)) {
      at = 2263; l1663 = field(rt, l15, "strokes");
      at = 2264; l1664 = K[455];
      at = 2265; l1665 = rt.sequence_get(l1663, l1664);
    }
    at = 2266; if (rt.truthy(l1662)) {
      at = 2267; l1667 = field(rt, l1665, "points");
      at = 2268; l1666 = l1667;
    } else {
      at = 2269; l1668 = K[456];
      at = 2270; l1666 = l1668;
    }
    at = 2271; l1669 = rt.array_count(l1666);
    at = 2272; l1670 = K[457];
    at = 2273; l1671 = rt.gt(l1669, l1670);
    at = 2274; if (rt.truthy(l1671)) {
      at = 2275; if (rt.truthy(l1662)) {
        at = 2276; l1674 = field(rt, l1665, "points");
        at = 2277; l1673 = l1674;
      } else {
        at = 2278; l1675 = K[458];
        at = 2279; l1673 = l1675;
      }
      at = 2280; l1676 = K[459];
      at = 2281; l1677 = rt.array_nth(l1673, l1676);
      at = 2282; l1672 = l1677;
    } else {
      at = 2283; l1678 = K[460];
      at = 2284; l1672 = l1678;
    }
    at = 2285; if (rt.truthy(l1662)) {
      at = 2286; l1680 = field(rt, l1665, "width");
      at = 2287; l1679 = l1680;
    } else {
      at = 2288; l1681 = K[461];
      at = 2289; l1679 = l1681;
    }
    at = 2290; l1682 = K[462];
    at = 2291; l1683 = rt.int_div(l1679, l1682);
    at = 2292; l1684 = K[463];
    at = 2293; l1685 = rt.max(l1684, l1683);
    at = 2294; l1686 = K[464];
    at = 2295; l1687 = rt.gt(l1669, l1686);
    at = 2296; if (rt.truthy(l1687)) {
      at = 2297; if (rt.truthy(l1662)) {
        at = 2298; l1690 = field(rt, l1665, "color");
        at = 2299; l1689 = l1690;
      } else {
        at = 2300; l1691 = K[465];
        at = 2301; l1689 = l1691;
      }
      at = 2302; l1692 = K[466];
      at = 2303; l1693 = rt.sequence_get(l1692, l1689);
      at = 2304; l1688 = l1693;
    } else {
      at = 2305; l1694 = K[467];
      at = 2306; l1688 = l1694;
    }
    at = 2307; markSet(rt, false, "circle", {at: l1672, radius: l1685, fill: l1688});
    at = 2308; if (rt.truthy(l1662)) {
      at = 2309; l1696 = field(rt, l1665, "points");
      at = 2310; l1695 = l1696;
    } else {
      at = 2311; l1697 = K[468];
      at = 2312; l1695 = l1697;
    }
    at = 2313; l1698 = K[469];
    at = 2314; l1699 = rt.sub(l1669, l1698);
    at = 2315; l1700 = K[470];
    at = 2316; l1701 = rt.max(l1700, l1699);
    at = 2317; l1702 = K[471];
    at = 2318; l1703 = rt.array_slice(l1695, l1702, l1701);
    at = 2319; if (rt.truthy(l1662)) {
      at = 2320; l1705 = field(rt, l1665, "points");
      at = 2321; l1704 = l1705;
    } else {
      at = 2322; l1706 = K[472];
      at = 2323; l1704 = l1706;
    }
    at = 2324; l1707 = K[473];
    at = 2325; l1708 = rt.gt(l1669, l1707);
    at = 2326; if (rt.truthy(l1708)) {
      at = 2327; l1710 = K[474];
      at = 2328; l1709 = l1710;
    } else {
      at = 2329; l1711 = K[475];
      at = 2330; l1709 = l1711;
    }
    at = 2331; l1712 = K[476];
    at = 2332; l1713 = rt.sub(l1669, l1712);
    at = 2333; l1714 = K[477];
    at = 2334; l1715 = rt.max(l1714, l1713);
    at = 2335; l1716 = rt.array_slice(l1704, l1709, l1715);
    at = 2336; if (rt.truthy(l1662)) {
      at = 2337; l1718 = field(rt, l1665, "color");
      at = 2338; l1717 = l1718;
    } else {
      at = 2339; l1719 = K[478];
      at = 2340; l1717 = l1719;
    }
    at = 2341; l1720 = K[479];
    at = 2342; l1721 = rt.sequence_get(l1720, l1717);
    at = 2343; if (rt.truthy(l1662)) {
      at = 2344; l1723 = field(rt, l1665, "width");
      at = 2345; l1722 = l1723;
    } else {
      at = 2346; l1724 = K[480];
      at = 2347; l1722 = l1724;
    }
    at = 2348; markSet(rt, true, "line", {from: l1703, to: l1716, color: l1721, width: l1722});
    at = 2349; l1725 = field(rt, l15, "strokes");
    at = 2350; l1726 = rt.sequence_length(l1725);
    at = 2351; l1727 = K[481];
    at = 2352; l1728 = rt.lt(l1727, l1726);
    at = 2353; if (rt.truthy(l1728)) {
      at = 2354; l1729 = field(rt, l15, "strokes");
      at = 2355; l1730 = K[482];
      at = 2356; l1731 = rt.sequence_get(l1729, l1730);
    }
    at = 2357; if (rt.truthy(l1728)) {
      at = 2358; l1733 = field(rt, l1731, "points");
      at = 2359; l1732 = l1733;
    } else {
      at = 2360; l1734 = K[483];
      at = 2361; l1732 = l1734;
    }
    at = 2362; l1735 = rt.array_count(l1732);
    at = 2363; l1736 = K[484];
    at = 2364; l1737 = rt.gt(l1735, l1736);
    at = 2365; if (rt.truthy(l1737)) {
      at = 2366; if (rt.truthy(l1728)) {
        at = 2367; l1740 = field(rt, l1731, "points");
        at = 2368; l1739 = l1740;
      } else {
        at = 2369; l1741 = K[485];
        at = 2370; l1739 = l1741;
      }
      at = 2371; l1742 = K[486];
      at = 2372; l1743 = rt.array_nth(l1739, l1742);
      at = 2373; l1738 = l1743;
    } else {
      at = 2374; l1744 = K[487];
      at = 2375; l1738 = l1744;
    }
    at = 2376; if (rt.truthy(l1728)) {
      at = 2377; l1746 = field(rt, l1731, "width");
      at = 2378; l1745 = l1746;
    } else {
      at = 2379; l1747 = K[488];
      at = 2380; l1745 = l1747;
    }
    at = 2381; l1748 = K[489];
    at = 2382; l1749 = rt.int_div(l1745, l1748);
    at = 2383; l1750 = K[490];
    at = 2384; l1751 = rt.max(l1750, l1749);
    at = 2385; l1752 = K[491];
    at = 2386; l1753 = rt.gt(l1735, l1752);
    at = 2387; if (rt.truthy(l1753)) {
      at = 2388; if (rt.truthy(l1728)) {
        at = 2389; l1756 = field(rt, l1731, "color");
        at = 2390; l1755 = l1756;
      } else {
        at = 2391; l1757 = K[492];
        at = 2392; l1755 = l1757;
      }
      at = 2393; l1758 = K[493];
      at = 2394; l1759 = rt.sequence_get(l1758, l1755);
      at = 2395; l1754 = l1759;
    } else {
      at = 2396; l1760 = K[494];
      at = 2397; l1754 = l1760;
    }
    at = 2398; markSet(rt, false, "circle", {at: l1738, radius: l1751, fill: l1754});
    at = 2399; if (rt.truthy(l1728)) {
      at = 2400; l1762 = field(rt, l1731, "points");
      at = 2401; l1761 = l1762;
    } else {
      at = 2402; l1763 = K[495];
      at = 2403; l1761 = l1763;
    }
    at = 2404; l1764 = K[496];
    at = 2405; l1765 = rt.sub(l1735, l1764);
    at = 2406; l1766 = K[497];
    at = 2407; l1767 = rt.max(l1766, l1765);
    at = 2408; l1768 = K[498];
    at = 2409; l1769 = rt.array_slice(l1761, l1768, l1767);
    at = 2410; if (rt.truthy(l1728)) {
      at = 2411; l1771 = field(rt, l1731, "points");
      at = 2412; l1770 = l1771;
    } else {
      at = 2413; l1772 = K[499];
      at = 2414; l1770 = l1772;
    }
    at = 2415; l1773 = K[500];
    at = 2416; l1774 = rt.gt(l1735, l1773);
    at = 2417; if (rt.truthy(l1774)) {
      at = 2418; l1776 = K[501];
      at = 2419; l1775 = l1776;
    } else {
      at = 2420; l1777 = K[502];
      at = 2421; l1775 = l1777;
    }
    at = 2422; l1778 = K[503];
    at = 2423; l1779 = rt.sub(l1735, l1778);
    at = 2424; l1780 = K[504];
    at = 2425; l1781 = rt.max(l1780, l1779);
    at = 2426; l1782 = rt.array_slice(l1770, l1775, l1781);
    at = 2427; if (rt.truthy(l1728)) {
      at = 2428; l1784 = field(rt, l1731, "color");
      at = 2429; l1783 = l1784;
    } else {
      at = 2430; l1785 = K[505];
      at = 2431; l1783 = l1785;
    }
    at = 2432; l1786 = K[506];
    at = 2433; l1787 = rt.sequence_get(l1786, l1783);
    at = 2434; if (rt.truthy(l1728)) {
      at = 2435; l1789 = field(rt, l1731, "width");
      at = 2436; l1788 = l1789;
    } else {
      at = 2437; l1790 = K[507];
      at = 2438; l1788 = l1790;
    }
    at = 2439; markSet(rt, true, "line", {from: l1769, to: l1782, color: l1787, width: l1788});
    at = 2440; l1791 = field(rt, l15, "strokes");
    at = 2441; l1792 = rt.sequence_length(l1791);
    at = 2442; l1793 = K[508];
    at = 2443; l1794 = rt.lt(l1793, l1792);
    at = 2444; if (rt.truthy(l1794)) {
      at = 2445; l1795 = field(rt, l15, "strokes");
      at = 2446; l1796 = K[509];
      at = 2447; l1797 = rt.sequence_get(l1795, l1796);
    }
    at = 2448; if (rt.truthy(l1794)) {
      at = 2449; l1799 = field(rt, l1797, "points");
      at = 2450; l1798 = l1799;
    } else {
      at = 2451; l1800 = K[510];
      at = 2452; l1798 = l1800;
    }
    at = 2453; l1801 = rt.array_count(l1798);
    at = 2454; l1802 = K[511];
    at = 2455; l1803 = rt.gt(l1801, l1802);
    at = 2456; if (rt.truthy(l1803)) {
      at = 2457; if (rt.truthy(l1794)) {
        at = 2458; l1806 = field(rt, l1797, "points");
        at = 2459; l1805 = l1806;
      } else {
        at = 2460; l1807 = K[512];
        at = 2461; l1805 = l1807;
      }
      at = 2462; l1808 = K[513];
      at = 2463; l1809 = rt.array_nth(l1805, l1808);
      at = 2464; l1804 = l1809;
    } else {
      at = 2465; l1810 = K[514];
      at = 2466; l1804 = l1810;
    }
    at = 2467; if (rt.truthy(l1794)) {
      at = 2468; l1812 = field(rt, l1797, "width");
      at = 2469; l1811 = l1812;
    } else {
      at = 2470; l1813 = K[515];
      at = 2471; l1811 = l1813;
    }
    at = 2472; l1814 = K[516];
    at = 2473; l1815 = rt.int_div(l1811, l1814);
    at = 2474; l1816 = K[517];
    at = 2475; l1817 = rt.max(l1816, l1815);
    at = 2476; l1818 = K[518];
    at = 2477; l1819 = rt.gt(l1801, l1818);
    at = 2478; if (rt.truthy(l1819)) {
      at = 2479; if (rt.truthy(l1794)) {
        at = 2480; l1822 = field(rt, l1797, "color");
        at = 2481; l1821 = l1822;
      } else {
        at = 2482; l1823 = K[519];
        at = 2483; l1821 = l1823;
      }
      at = 2484; l1824 = K[520];
      at = 2485; l1825 = rt.sequence_get(l1824, l1821);
      at = 2486; l1820 = l1825;
    } else {
      at = 2487; l1826 = K[521];
      at = 2488; l1820 = l1826;
    }
    at = 2489; markSet(rt, false, "circle", {at: l1804, radius: l1817, fill: l1820});
    at = 2490; if (rt.truthy(l1794)) {
      at = 2491; l1828 = field(rt, l1797, "points");
      at = 2492; l1827 = l1828;
    } else {
      at = 2493; l1829 = K[522];
      at = 2494; l1827 = l1829;
    }
    at = 2495; l1830 = K[523];
    at = 2496; l1831 = rt.sub(l1801, l1830);
    at = 2497; l1832 = K[524];
    at = 2498; l1833 = rt.max(l1832, l1831);
    at = 2499; l1834 = K[525];
    at = 2500; l1835 = rt.array_slice(l1827, l1834, l1833);
    at = 2501; if (rt.truthy(l1794)) {
      at = 2502; l1837 = field(rt, l1797, "points");
      at = 2503; l1836 = l1837;
    } else {
      at = 2504; l1838 = K[526];
      at = 2505; l1836 = l1838;
    }
    at = 2506; l1839 = K[527];
    at = 2507; l1840 = rt.gt(l1801, l1839);
    at = 2508; if (rt.truthy(l1840)) {
      at = 2509; l1842 = K[528];
      at = 2510; l1841 = l1842;
    } else {
      at = 2511; l1843 = K[529];
      at = 2512; l1841 = l1843;
    }
    at = 2513; l1844 = K[530];
    at = 2514; l1845 = rt.sub(l1801, l1844);
    at = 2515; l1846 = K[531];
    at = 2516; l1847 = rt.max(l1846, l1845);
    at = 2517; l1848 = rt.array_slice(l1836, l1841, l1847);
    at = 2518; if (rt.truthy(l1794)) {
      at = 2519; l1850 = field(rt, l1797, "color");
      at = 2520; l1849 = l1850;
    } else {
      at = 2521; l1851 = K[532];
      at = 2522; l1849 = l1851;
    }
    at = 2523; l1852 = K[533];
    at = 2524; l1853 = rt.sequence_get(l1852, l1849);
    at = 2525; if (rt.truthy(l1794)) {
      at = 2526; l1855 = field(rt, l1797, "width");
      at = 2527; l1854 = l1855;
    } else {
      at = 2528; l1856 = K[534];
      at = 2529; l1854 = l1856;
    }
    at = 2530; markSet(rt, true, "line", {from: l1835, to: l1848, color: l1853, width: l1854});
    at = 2531; l1857 = field(rt, l15, "strokes");
    at = 2532; l1858 = rt.sequence_length(l1857);
    at = 2533; l1859 = K[535];
    at = 2534; l1860 = rt.lt(l1859, l1858);
    at = 2535; if (rt.truthy(l1860)) {
      at = 2536; l1861 = field(rt, l15, "strokes");
      at = 2537; l1862 = K[536];
      at = 2538; l1863 = rt.sequence_get(l1861, l1862);
    }
    at = 2539; if (rt.truthy(l1860)) {
      at = 2540; l1865 = field(rt, l1863, "points");
      at = 2541; l1864 = l1865;
    } else {
      at = 2542; l1866 = K[537];
      at = 2543; l1864 = l1866;
    }
    at = 2544; l1867 = rt.array_count(l1864);
    at = 2545; l1868 = K[538];
    at = 2546; l1869 = rt.gt(l1867, l1868);
    at = 2547; if (rt.truthy(l1869)) {
      at = 2548; if (rt.truthy(l1860)) {
        at = 2549; l1872 = field(rt, l1863, "points");
        at = 2550; l1871 = l1872;
      } else {
        at = 2551; l1873 = K[539];
        at = 2552; l1871 = l1873;
      }
      at = 2553; l1874 = K[540];
      at = 2554; l1875 = rt.array_nth(l1871, l1874);
      at = 2555; l1870 = l1875;
    } else {
      at = 2556; l1876 = K[541];
      at = 2557; l1870 = l1876;
    }
    at = 2558; if (rt.truthy(l1860)) {
      at = 2559; l1878 = field(rt, l1863, "width");
      at = 2560; l1877 = l1878;
    } else {
      at = 2561; l1879 = K[542];
      at = 2562; l1877 = l1879;
    }
    at = 2563; l1880 = K[543];
    at = 2564; l1881 = rt.int_div(l1877, l1880);
    at = 2565; l1882 = K[544];
    at = 2566; l1883 = rt.max(l1882, l1881);
    at = 2567; l1884 = K[545];
    at = 2568; l1885 = rt.gt(l1867, l1884);
    at = 2569; if (rt.truthy(l1885)) {
      at = 2570; if (rt.truthy(l1860)) {
        at = 2571; l1888 = field(rt, l1863, "color");
        at = 2572; l1887 = l1888;
      } else {
        at = 2573; l1889 = K[546];
        at = 2574; l1887 = l1889;
      }
      at = 2575; l1890 = K[547];
      at = 2576; l1891 = rt.sequence_get(l1890, l1887);
      at = 2577; l1886 = l1891;
    } else {
      at = 2578; l1892 = K[548];
      at = 2579; l1886 = l1892;
    }
    at = 2580; markSet(rt, false, "circle", {at: l1870, radius: l1883, fill: l1886});
    at = 2581; if (rt.truthy(l1860)) {
      at = 2582; l1894 = field(rt, l1863, "points");
      at = 2583; l1893 = l1894;
    } else {
      at = 2584; l1895 = K[549];
      at = 2585; l1893 = l1895;
    }
    at = 2586; l1896 = K[550];
    at = 2587; l1897 = rt.sub(l1867, l1896);
    at = 2588; l1898 = K[551];
    at = 2589; l1899 = rt.max(l1898, l1897);
    at = 2590; l1900 = K[552];
    at = 2591; l1901 = rt.array_slice(l1893, l1900, l1899);
    at = 2592; if (rt.truthy(l1860)) {
      at = 2593; l1903 = field(rt, l1863, "points");
      at = 2594; l1902 = l1903;
    } else {
      at = 2595; l1904 = K[553];
      at = 2596; l1902 = l1904;
    }
    at = 2597; l1905 = K[554];
    at = 2598; l1906 = rt.gt(l1867, l1905);
    at = 2599; if (rt.truthy(l1906)) {
      at = 2600; l1908 = K[555];
      at = 2601; l1907 = l1908;
    } else {
      at = 2602; l1909 = K[556];
      at = 2603; l1907 = l1909;
    }
    at = 2604; l1910 = K[557];
    at = 2605; l1911 = rt.sub(l1867, l1910);
    at = 2606; l1912 = K[558];
    at = 2607; l1913 = rt.max(l1912, l1911);
    at = 2608; l1914 = rt.array_slice(l1902, l1907, l1913);
    at = 2609; if (rt.truthy(l1860)) {
      at = 2610; l1916 = field(rt, l1863, "color");
      at = 2611; l1915 = l1916;
    } else {
      at = 2612; l1917 = K[559];
      at = 2613; l1915 = l1917;
    }
    at = 2614; l1918 = K[560];
    at = 2615; l1919 = rt.sequence_get(l1918, l1915);
    at = 2616; if (rt.truthy(l1860)) {
      at = 2617; l1921 = field(rt, l1863, "width");
      at = 2618; l1920 = l1921;
    } else {
      at = 2619; l1922 = K[561];
      at = 2620; l1920 = l1922;
    }
    at = 2621; markSet(rt, true, "line", {from: l1901, to: l1914, color: l1919, width: l1920});
    at = 2622; l1923 = field(rt, l15, "strokes");
    at = 2623; l1924 = rt.sequence_length(l1923);
    at = 2624; l1925 = K[562];
    at = 2625; l1926 = rt.lt(l1925, l1924);
    at = 2626; if (rt.truthy(l1926)) {
      at = 2627; l1927 = field(rt, l15, "strokes");
      at = 2628; l1928 = K[563];
      at = 2629; l1929 = rt.sequence_get(l1927, l1928);
    }
    at = 2630; if (rt.truthy(l1926)) {
      at = 2631; l1931 = field(rt, l1929, "points");
      at = 2632; l1930 = l1931;
    } else {
      at = 2633; l1932 = K[564];
      at = 2634; l1930 = l1932;
    }
    at = 2635; l1933 = rt.array_count(l1930);
    at = 2636; l1934 = K[565];
    at = 2637; l1935 = rt.gt(l1933, l1934);
    at = 2638; if (rt.truthy(l1935)) {
      at = 2639; if (rt.truthy(l1926)) {
        at = 2640; l1938 = field(rt, l1929, "points");
        at = 2641; l1937 = l1938;
      } else {
        at = 2642; l1939 = K[566];
        at = 2643; l1937 = l1939;
      }
      at = 2644; l1940 = K[567];
      at = 2645; l1941 = rt.array_nth(l1937, l1940);
      at = 2646; l1936 = l1941;
    } else {
      at = 2647; l1942 = K[568];
      at = 2648; l1936 = l1942;
    }
    at = 2649; if (rt.truthy(l1926)) {
      at = 2650; l1944 = field(rt, l1929, "width");
      at = 2651; l1943 = l1944;
    } else {
      at = 2652; l1945 = K[569];
      at = 2653; l1943 = l1945;
    }
    at = 2654; l1946 = K[570];
    at = 2655; l1947 = rt.int_div(l1943, l1946);
    at = 2656; l1948 = K[571];
    at = 2657; l1949 = rt.max(l1948, l1947);
    at = 2658; l1950 = K[572];
    at = 2659; l1951 = rt.gt(l1933, l1950);
    at = 2660; if (rt.truthy(l1951)) {
      at = 2661; if (rt.truthy(l1926)) {
        at = 2662; l1954 = field(rt, l1929, "color");
        at = 2663; l1953 = l1954;
      } else {
        at = 2664; l1955 = K[573];
        at = 2665; l1953 = l1955;
      }
      at = 2666; l1956 = K[574];
      at = 2667; l1957 = rt.sequence_get(l1956, l1953);
      at = 2668; l1952 = l1957;
    } else {
      at = 2669; l1958 = K[575];
      at = 2670; l1952 = l1958;
    }
    at = 2671; markSet(rt, false, "circle", {at: l1936, radius: l1949, fill: l1952});
    at = 2672; if (rt.truthy(l1926)) {
      at = 2673; l1960 = field(rt, l1929, "points");
      at = 2674; l1959 = l1960;
    } else {
      at = 2675; l1961 = K[576];
      at = 2676; l1959 = l1961;
    }
    at = 2677; l1962 = K[577];
    at = 2678; l1963 = rt.sub(l1933, l1962);
    at = 2679; l1964 = K[578];
    at = 2680; l1965 = rt.max(l1964, l1963);
    at = 2681; l1966 = K[579];
    at = 2682; l1967 = rt.array_slice(l1959, l1966, l1965);
    at = 2683; if (rt.truthy(l1926)) {
      at = 2684; l1969 = field(rt, l1929, "points");
      at = 2685; l1968 = l1969;
    } else {
      at = 2686; l1970 = K[580];
      at = 2687; l1968 = l1970;
    }
    at = 2688; l1971 = K[581];
    at = 2689; l1972 = rt.gt(l1933, l1971);
    at = 2690; if (rt.truthy(l1972)) {
      at = 2691; l1974 = K[582];
      at = 2692; l1973 = l1974;
    } else {
      at = 2693; l1975 = K[583];
      at = 2694; l1973 = l1975;
    }
    at = 2695; l1976 = K[584];
    at = 2696; l1977 = rt.sub(l1933, l1976);
    at = 2697; l1978 = K[585];
    at = 2698; l1979 = rt.max(l1978, l1977);
    at = 2699; l1980 = rt.array_slice(l1968, l1973, l1979);
    at = 2700; if (rt.truthy(l1926)) {
      at = 2701; l1982 = field(rt, l1929, "color");
      at = 2702; l1981 = l1982;
    } else {
      at = 2703; l1983 = K[586];
      at = 2704; l1981 = l1983;
    }
    at = 2705; l1984 = K[587];
    at = 2706; l1985 = rt.sequence_get(l1984, l1981);
    at = 2707; if (rt.truthy(l1926)) {
      at = 2708; l1987 = field(rt, l1929, "width");
      at = 2709; l1986 = l1987;
    } else {
      at = 2710; l1988 = K[588];
      at = 2711; l1986 = l1988;
    }
    at = 2712; markSet(rt, true, "line", {from: l1967, to: l1980, color: l1985, width: l1986});
    at = 2713; l1989 = field(rt, l15, "strokes");
    at = 2714; l1990 = rt.sequence_length(l1989);
    at = 2715; l1991 = K[589];
    at = 2716; l1992 = rt.lt(l1991, l1990);
    at = 2717; if (rt.truthy(l1992)) {
      at = 2718; l1993 = field(rt, l15, "strokes");
      at = 2719; l1994 = K[590];
      at = 2720; l1995 = rt.sequence_get(l1993, l1994);
    }
    at = 2721; if (rt.truthy(l1992)) {
      at = 2722; l1997 = field(rt, l1995, "points");
      at = 2723; l1996 = l1997;
    } else {
      at = 2724; l1998 = K[591];
      at = 2725; l1996 = l1998;
    }
    at = 2726; l1999 = rt.array_count(l1996);
    at = 2727; l2000 = K[592];
    at = 2728; l2001 = rt.gt(l1999, l2000);
    at = 2729; if (rt.truthy(l2001)) {
      at = 2730; if (rt.truthy(l1992)) {
        at = 2731; l2004 = field(rt, l1995, "points");
        at = 2732; l2003 = l2004;
      } else {
        at = 2733; l2005 = K[593];
        at = 2734; l2003 = l2005;
      }
      at = 2735; l2006 = K[594];
      at = 2736; l2007 = rt.array_nth(l2003, l2006);
      at = 2737; l2002 = l2007;
    } else {
      at = 2738; l2008 = K[595];
      at = 2739; l2002 = l2008;
    }
    at = 2740; if (rt.truthy(l1992)) {
      at = 2741; l2010 = field(rt, l1995, "width");
      at = 2742; l2009 = l2010;
    } else {
      at = 2743; l2011 = K[596];
      at = 2744; l2009 = l2011;
    }
    at = 2745; l2012 = K[597];
    at = 2746; l2013 = rt.int_div(l2009, l2012);
    at = 2747; l2014 = K[598];
    at = 2748; l2015 = rt.max(l2014, l2013);
    at = 2749; l2016 = K[599];
    at = 2750; l2017 = rt.gt(l1999, l2016);
    at = 2751; if (rt.truthy(l2017)) {
      at = 2752; if (rt.truthy(l1992)) {
        at = 2753; l2020 = field(rt, l1995, "color");
        at = 2754; l2019 = l2020;
      } else {
        at = 2755; l2021 = K[600];
        at = 2756; l2019 = l2021;
      }
      at = 2757; l2022 = K[601];
      at = 2758; l2023 = rt.sequence_get(l2022, l2019);
      at = 2759; l2018 = l2023;
    } else {
      at = 2760; l2024 = K[602];
      at = 2761; l2018 = l2024;
    }
    at = 2762; markSet(rt, false, "circle", {at: l2002, radius: l2015, fill: l2018});
    at = 2763; if (rt.truthy(l1992)) {
      at = 2764; l2026 = field(rt, l1995, "points");
      at = 2765; l2025 = l2026;
    } else {
      at = 2766; l2027 = K[603];
      at = 2767; l2025 = l2027;
    }
    at = 2768; l2028 = K[604];
    at = 2769; l2029 = rt.sub(l1999, l2028);
    at = 2770; l2030 = K[605];
    at = 2771; l2031 = rt.max(l2030, l2029);
    at = 2772; l2032 = K[606];
    at = 2773; l2033 = rt.array_slice(l2025, l2032, l2031);
    at = 2774; if (rt.truthy(l1992)) {
      at = 2775; l2035 = field(rt, l1995, "points");
      at = 2776; l2034 = l2035;
    } else {
      at = 2777; l2036 = K[607];
      at = 2778; l2034 = l2036;
    }
    at = 2779; l2037 = K[608];
    at = 2780; l2038 = rt.gt(l1999, l2037);
    at = 2781; if (rt.truthy(l2038)) {
      at = 2782; l2040 = K[609];
      at = 2783; l2039 = l2040;
    } else {
      at = 2784; l2041 = K[610];
      at = 2785; l2039 = l2041;
    }
    at = 2786; l2042 = K[611];
    at = 2787; l2043 = rt.sub(l1999, l2042);
    at = 2788; l2044 = K[612];
    at = 2789; l2045 = rt.max(l2044, l2043);
    at = 2790; l2046 = rt.array_slice(l2034, l2039, l2045);
    at = 2791; if (rt.truthy(l1992)) {
      at = 2792; l2048 = field(rt, l1995, "color");
      at = 2793; l2047 = l2048;
    } else {
      at = 2794; l2049 = K[613];
      at = 2795; l2047 = l2049;
    }
    at = 2796; l2050 = K[614];
    at = 2797; l2051 = rt.sequence_get(l2050, l2047);
    at = 2798; if (rt.truthy(l1992)) {
      at = 2799; l2053 = field(rt, l1995, "width");
      at = 2800; l2052 = l2053;
    } else {
      at = 2801; l2054 = K[615];
      at = 2802; l2052 = l2054;
    }
    at = 2803; markSet(rt, true, "line", {from: l2033, to: l2046, color: l2051, width: l2052});
    at = 2804; l2055 = field(rt, l15, "strokes");
    at = 2805; l2056 = rt.sequence_length(l2055);
    at = 2806; l2057 = K[616];
    at = 2807; l2058 = rt.lt(l2057, l2056);
    at = 2808; if (rt.truthy(l2058)) {
      at = 2809; l2059 = field(rt, l15, "strokes");
      at = 2810; l2060 = K[617];
      at = 2811; l2061 = rt.sequence_get(l2059, l2060);
    }
    at = 2812; if (rt.truthy(l2058)) {
      at = 2813; l2063 = field(rt, l2061, "points");
      at = 2814; l2062 = l2063;
    } else {
      at = 2815; l2064 = K[618];
      at = 2816; l2062 = l2064;
    }
    at = 2817; l2065 = rt.array_count(l2062);
    at = 2818; l2066 = K[619];
    at = 2819; l2067 = rt.gt(l2065, l2066);
    at = 2820; if (rt.truthy(l2067)) {
      at = 2821; if (rt.truthy(l2058)) {
        at = 2822; l2070 = field(rt, l2061, "points");
        at = 2823; l2069 = l2070;
      } else {
        at = 2824; l2071 = K[620];
        at = 2825; l2069 = l2071;
      }
      at = 2826; l2072 = K[621];
      at = 2827; l2073 = rt.array_nth(l2069, l2072);
      at = 2828; l2068 = l2073;
    } else {
      at = 2829; l2074 = K[622];
      at = 2830; l2068 = l2074;
    }
    at = 2831; if (rt.truthy(l2058)) {
      at = 2832; l2076 = field(rt, l2061, "width");
      at = 2833; l2075 = l2076;
    } else {
      at = 2834; l2077 = K[623];
      at = 2835; l2075 = l2077;
    }
    at = 2836; l2078 = K[624];
    at = 2837; l2079 = rt.int_div(l2075, l2078);
    at = 2838; l2080 = K[625];
    at = 2839; l2081 = rt.max(l2080, l2079);
    at = 2840; l2082 = K[626];
    at = 2841; l2083 = rt.gt(l2065, l2082);
    at = 2842; if (rt.truthy(l2083)) {
      at = 2843; if (rt.truthy(l2058)) {
        at = 2844; l2086 = field(rt, l2061, "color");
        at = 2845; l2085 = l2086;
      } else {
        at = 2846; l2087 = K[627];
        at = 2847; l2085 = l2087;
      }
      at = 2848; l2088 = K[628];
      at = 2849; l2089 = rt.sequence_get(l2088, l2085);
      at = 2850; l2084 = l2089;
    } else {
      at = 2851; l2090 = K[629];
      at = 2852; l2084 = l2090;
    }
    at = 2853; markSet(rt, false, "circle", {at: l2068, radius: l2081, fill: l2084});
    at = 2854; if (rt.truthy(l2058)) {
      at = 2855; l2092 = field(rt, l2061, "points");
      at = 2856; l2091 = l2092;
    } else {
      at = 2857; l2093 = K[630];
      at = 2858; l2091 = l2093;
    }
    at = 2859; l2094 = K[631];
    at = 2860; l2095 = rt.sub(l2065, l2094);
    at = 2861; l2096 = K[632];
    at = 2862; l2097 = rt.max(l2096, l2095);
    at = 2863; l2098 = K[633];
    at = 2864; l2099 = rt.array_slice(l2091, l2098, l2097);
    at = 2865; if (rt.truthy(l2058)) {
      at = 2866; l2101 = field(rt, l2061, "points");
      at = 2867; l2100 = l2101;
    } else {
      at = 2868; l2102 = K[634];
      at = 2869; l2100 = l2102;
    }
    at = 2870; l2103 = K[635];
    at = 2871; l2104 = rt.gt(l2065, l2103);
    at = 2872; if (rt.truthy(l2104)) {
      at = 2873; l2106 = K[636];
      at = 2874; l2105 = l2106;
    } else {
      at = 2875; l2107 = K[637];
      at = 2876; l2105 = l2107;
    }
    at = 2877; l2108 = K[638];
    at = 2878; l2109 = rt.sub(l2065, l2108);
    at = 2879; l2110 = K[639];
    at = 2880; l2111 = rt.max(l2110, l2109);
    at = 2881; l2112 = rt.array_slice(l2100, l2105, l2111);
    at = 2882; if (rt.truthy(l2058)) {
      at = 2883; l2114 = field(rt, l2061, "color");
      at = 2884; l2113 = l2114;
    } else {
      at = 2885; l2115 = K[640];
      at = 2886; l2113 = l2115;
    }
    at = 2887; l2116 = K[641];
    at = 2888; l2117 = rt.sequence_get(l2116, l2113);
    at = 2889; if (rt.truthy(l2058)) {
      at = 2890; l2119 = field(rt, l2061, "width");
      at = 2891; l2118 = l2119;
    } else {
      at = 2892; l2120 = K[642];
      at = 2893; l2118 = l2120;
    }
    at = 2894; markSet(rt, true, "line", {from: l2099, to: l2112, color: l2117, width: l2118});
    at = 2895; l2121 = field(rt, l15, "strokes");
    at = 2896; l2122 = rt.sequence_length(l2121);
    at = 2897; l2123 = K[643];
    at = 2898; l2124 = rt.lt(l2123, l2122);
    at = 2899; if (rt.truthy(l2124)) {
      at = 2900; l2125 = field(rt, l15, "strokes");
      at = 2901; l2126 = K[644];
      at = 2902; l2127 = rt.sequence_get(l2125, l2126);
    }
    at = 2903; if (rt.truthy(l2124)) {
      at = 2904; l2129 = field(rt, l2127, "points");
      at = 2905; l2128 = l2129;
    } else {
      at = 2906; l2130 = K[645];
      at = 2907; l2128 = l2130;
    }
    at = 2908; l2131 = rt.array_count(l2128);
    at = 2909; l2132 = K[646];
    at = 2910; l2133 = rt.gt(l2131, l2132);
    at = 2911; if (rt.truthy(l2133)) {
      at = 2912; if (rt.truthy(l2124)) {
        at = 2913; l2136 = field(rt, l2127, "points");
        at = 2914; l2135 = l2136;
      } else {
        at = 2915; l2137 = K[647];
        at = 2916; l2135 = l2137;
      }
      at = 2917; l2138 = K[648];
      at = 2918; l2139 = rt.array_nth(l2135, l2138);
      at = 2919; l2134 = l2139;
    } else {
      at = 2920; l2140 = K[649];
      at = 2921; l2134 = l2140;
    }
    at = 2922; if (rt.truthy(l2124)) {
      at = 2923; l2142 = field(rt, l2127, "width");
      at = 2924; l2141 = l2142;
    } else {
      at = 2925; l2143 = K[650];
      at = 2926; l2141 = l2143;
    }
    at = 2927; l2144 = K[651];
    at = 2928; l2145 = rt.int_div(l2141, l2144);
    at = 2929; l2146 = K[652];
    at = 2930; l2147 = rt.max(l2146, l2145);
    at = 2931; l2148 = K[653];
    at = 2932; l2149 = rt.gt(l2131, l2148);
    at = 2933; if (rt.truthy(l2149)) {
      at = 2934; if (rt.truthy(l2124)) {
        at = 2935; l2152 = field(rt, l2127, "color");
        at = 2936; l2151 = l2152;
      } else {
        at = 2937; l2153 = K[654];
        at = 2938; l2151 = l2153;
      }
      at = 2939; l2154 = K[655];
      at = 2940; l2155 = rt.sequence_get(l2154, l2151);
      at = 2941; l2150 = l2155;
    } else {
      at = 2942; l2156 = K[656];
      at = 2943; l2150 = l2156;
    }
    at = 2944; markSet(rt, false, "circle", {at: l2134, radius: l2147, fill: l2150});
    at = 2945; if (rt.truthy(l2124)) {
      at = 2946; l2158 = field(rt, l2127, "points");
      at = 2947; l2157 = l2158;
    } else {
      at = 2948; l2159 = K[657];
      at = 2949; l2157 = l2159;
    }
    at = 2950; l2160 = K[658];
    at = 2951; l2161 = rt.sub(l2131, l2160);
    at = 2952; l2162 = K[659];
    at = 2953; l2163 = rt.max(l2162, l2161);
    at = 2954; l2164 = K[660];
    at = 2955; l2165 = rt.array_slice(l2157, l2164, l2163);
    at = 2956; if (rt.truthy(l2124)) {
      at = 2957; l2167 = field(rt, l2127, "points");
      at = 2958; l2166 = l2167;
    } else {
      at = 2959; l2168 = K[661];
      at = 2960; l2166 = l2168;
    }
    at = 2961; l2169 = K[662];
    at = 2962; l2170 = rt.gt(l2131, l2169);
    at = 2963; if (rt.truthy(l2170)) {
      at = 2964; l2172 = K[663];
      at = 2965; l2171 = l2172;
    } else {
      at = 2966; l2173 = K[664];
      at = 2967; l2171 = l2173;
    }
    at = 2968; l2174 = K[665];
    at = 2969; l2175 = rt.sub(l2131, l2174);
    at = 2970; l2176 = K[666];
    at = 2971; l2177 = rt.max(l2176, l2175);
    at = 2972; l2178 = rt.array_slice(l2166, l2171, l2177);
    at = 2973; if (rt.truthy(l2124)) {
      at = 2974; l2180 = field(rt, l2127, "color");
      at = 2975; l2179 = l2180;
    } else {
      at = 2976; l2181 = K[667];
      at = 2977; l2179 = l2181;
    }
    at = 2978; l2182 = K[668];
    at = 2979; l2183 = rt.sequence_get(l2182, l2179);
    at = 2980; if (rt.truthy(l2124)) {
      at = 2981; l2185 = field(rt, l2127, "width");
      at = 2982; l2184 = l2185;
    } else {
      at = 2983; l2186 = K[669];
      at = 2984; l2184 = l2186;
    }
    at = 2985; markSet(rt, true, "line", {from: l2165, to: l2178, color: l2183, width: l2184});
    at = 2986; l2187 = field(rt, l15, "strokes");
    at = 2987; l2188 = rt.sequence_length(l2187);
    at = 2988; l2189 = K[670];
    at = 2989; l2190 = rt.lt(l2189, l2188);
    at = 2990; if (rt.truthy(l2190)) {
      at = 2991; l2191 = field(rt, l15, "strokes");
      at = 2992; l2192 = K[671];
      at = 2993; l2193 = rt.sequence_get(l2191, l2192);
    }
    at = 2994; if (rt.truthy(l2190)) {
      at = 2995; l2195 = field(rt, l2193, "points");
      at = 2996; l2194 = l2195;
    } else {
      at = 2997; l2196 = K[672];
      at = 2998; l2194 = l2196;
    }
    at = 2999; l2197 = rt.array_count(l2194);
    at = 3000; l2198 = K[673];
    at = 3001; l2199 = rt.gt(l2197, l2198);
    at = 3002; if (rt.truthy(l2199)) {
      at = 3003; if (rt.truthy(l2190)) {
        at = 3004; l2202 = field(rt, l2193, "points");
        at = 3005; l2201 = l2202;
      } else {
        at = 3006; l2203 = K[674];
        at = 3007; l2201 = l2203;
      }
      at = 3008; l2204 = K[675];
      at = 3009; l2205 = rt.array_nth(l2201, l2204);
      at = 3010; l2200 = l2205;
    } else {
      at = 3011; l2206 = K[676];
      at = 3012; l2200 = l2206;
    }
    at = 3013; if (rt.truthy(l2190)) {
      at = 3014; l2208 = field(rt, l2193, "width");
      at = 3015; l2207 = l2208;
    } else {
      at = 3016; l2209 = K[677];
      at = 3017; l2207 = l2209;
    }
    at = 3018; l2210 = K[678];
    at = 3019; l2211 = rt.int_div(l2207, l2210);
    at = 3020; l2212 = K[679];
    at = 3021; l2213 = rt.max(l2212, l2211);
    at = 3022; l2214 = K[680];
    at = 3023; l2215 = rt.gt(l2197, l2214);
    at = 3024; if (rt.truthy(l2215)) {
      at = 3025; if (rt.truthy(l2190)) {
        at = 3026; l2218 = field(rt, l2193, "color");
        at = 3027; l2217 = l2218;
      } else {
        at = 3028; l2219 = K[681];
        at = 3029; l2217 = l2219;
      }
      at = 3030; l2220 = K[682];
      at = 3031; l2221 = rt.sequence_get(l2220, l2217);
      at = 3032; l2216 = l2221;
    } else {
      at = 3033; l2222 = K[683];
      at = 3034; l2216 = l2222;
    }
    at = 3035; markSet(rt, false, "circle", {at: l2200, radius: l2213, fill: l2216});
    at = 3036; if (rt.truthy(l2190)) {
      at = 3037; l2224 = field(rt, l2193, "points");
      at = 3038; l2223 = l2224;
    } else {
      at = 3039; l2225 = K[684];
      at = 3040; l2223 = l2225;
    }
    at = 3041; l2226 = K[685];
    at = 3042; l2227 = rt.sub(l2197, l2226);
    at = 3043; l2228 = K[686];
    at = 3044; l2229 = rt.max(l2228, l2227);
    at = 3045; l2230 = K[687];
    at = 3046; l2231 = rt.array_slice(l2223, l2230, l2229);
    at = 3047; if (rt.truthy(l2190)) {
      at = 3048; l2233 = field(rt, l2193, "points");
      at = 3049; l2232 = l2233;
    } else {
      at = 3050; l2234 = K[688];
      at = 3051; l2232 = l2234;
    }
    at = 3052; l2235 = K[689];
    at = 3053; l2236 = rt.gt(l2197, l2235);
    at = 3054; if (rt.truthy(l2236)) {
      at = 3055; l2238 = K[690];
      at = 3056; l2237 = l2238;
    } else {
      at = 3057; l2239 = K[691];
      at = 3058; l2237 = l2239;
    }
    at = 3059; l2240 = K[692];
    at = 3060; l2241 = rt.sub(l2197, l2240);
    at = 3061; l2242 = K[693];
    at = 3062; l2243 = rt.max(l2242, l2241);
    at = 3063; l2244 = rt.array_slice(l2232, l2237, l2243);
    at = 3064; if (rt.truthy(l2190)) {
      at = 3065; l2246 = field(rt, l2193, "color");
      at = 3066; l2245 = l2246;
    } else {
      at = 3067; l2247 = K[694];
      at = 3068; l2245 = l2247;
    }
    at = 3069; l2248 = K[695];
    at = 3070; l2249 = rt.sequence_get(l2248, l2245);
    at = 3071; if (rt.truthy(l2190)) {
      at = 3072; l2251 = field(rt, l2193, "width");
      at = 3073; l2250 = l2251;
    } else {
      at = 3074; l2252 = K[696];
      at = 3075; l2250 = l2252;
    }
    at = 3076; markSet(rt, true, "line", {from: l2231, to: l2244, color: l2249, width: l2250});
    at = 3077; l2253 = field(rt, l15, "strokes");
    at = 3078; l2254 = rt.sequence_length(l2253);
    at = 3079; l2255 = K[697];
    at = 3080; l2256 = rt.lt(l2255, l2254);
    at = 3081; if (rt.truthy(l2256)) {
      at = 3082; l2257 = field(rt, l15, "strokes");
      at = 3083; l2258 = K[698];
      at = 3084; l2259 = rt.sequence_get(l2257, l2258);
    }
    at = 3085; if (rt.truthy(l2256)) {
      at = 3086; l2261 = field(rt, l2259, "points");
      at = 3087; l2260 = l2261;
    } else {
      at = 3088; l2262 = K[699];
      at = 3089; l2260 = l2262;
    }
    at = 3090; l2263 = rt.array_count(l2260);
    at = 3091; l2264 = K[700];
    at = 3092; l2265 = rt.gt(l2263, l2264);
    at = 3093; if (rt.truthy(l2265)) {
      at = 3094; if (rt.truthy(l2256)) {
        at = 3095; l2268 = field(rt, l2259, "points");
        at = 3096; l2267 = l2268;
      } else {
        at = 3097; l2269 = K[701];
        at = 3098; l2267 = l2269;
      }
      at = 3099; l2270 = K[702];
      at = 3100; l2271 = rt.array_nth(l2267, l2270);
      at = 3101; l2266 = l2271;
    } else {
      at = 3102; l2272 = K[703];
      at = 3103; l2266 = l2272;
    }
    at = 3104; if (rt.truthy(l2256)) {
      at = 3105; l2274 = field(rt, l2259, "width");
      at = 3106; l2273 = l2274;
    } else {
      at = 3107; l2275 = K[704];
      at = 3108; l2273 = l2275;
    }
    at = 3109; l2276 = K[705];
    at = 3110; l2277 = rt.int_div(l2273, l2276);
    at = 3111; l2278 = K[706];
    at = 3112; l2279 = rt.max(l2278, l2277);
    at = 3113; l2280 = K[707];
    at = 3114; l2281 = rt.gt(l2263, l2280);
    at = 3115; if (rt.truthy(l2281)) {
      at = 3116; if (rt.truthy(l2256)) {
        at = 3117; l2284 = field(rt, l2259, "color");
        at = 3118; l2283 = l2284;
      } else {
        at = 3119; l2285 = K[708];
        at = 3120; l2283 = l2285;
      }
      at = 3121; l2286 = K[709];
      at = 3122; l2287 = rt.sequence_get(l2286, l2283);
      at = 3123; l2282 = l2287;
    } else {
      at = 3124; l2288 = K[710];
      at = 3125; l2282 = l2288;
    }
    at = 3126; markSet(rt, false, "circle", {at: l2266, radius: l2279, fill: l2282});
    at = 3127; if (rt.truthy(l2256)) {
      at = 3128; l2290 = field(rt, l2259, "points");
      at = 3129; l2289 = l2290;
    } else {
      at = 3130; l2291 = K[711];
      at = 3131; l2289 = l2291;
    }
    at = 3132; l2292 = K[712];
    at = 3133; l2293 = rt.sub(l2263, l2292);
    at = 3134; l2294 = K[713];
    at = 3135; l2295 = rt.max(l2294, l2293);
    at = 3136; l2296 = K[714];
    at = 3137; l2297 = rt.array_slice(l2289, l2296, l2295);
    at = 3138; if (rt.truthy(l2256)) {
      at = 3139; l2299 = field(rt, l2259, "points");
      at = 3140; l2298 = l2299;
    } else {
      at = 3141; l2300 = K[715];
      at = 3142; l2298 = l2300;
    }
    at = 3143; l2301 = K[716];
    at = 3144; l2302 = rt.gt(l2263, l2301);
    at = 3145; if (rt.truthy(l2302)) {
      at = 3146; l2304 = K[717];
      at = 3147; l2303 = l2304;
    } else {
      at = 3148; l2305 = K[718];
      at = 3149; l2303 = l2305;
    }
    at = 3150; l2306 = K[719];
    at = 3151; l2307 = rt.sub(l2263, l2306);
    at = 3152; l2308 = K[720];
    at = 3153; l2309 = rt.max(l2308, l2307);
    at = 3154; l2310 = rt.array_slice(l2298, l2303, l2309);
    at = 3155; if (rt.truthy(l2256)) {
      at = 3156; l2312 = field(rt, l2259, "color");
      at = 3157; l2311 = l2312;
    } else {
      at = 3158; l2313 = K[721];
      at = 3159; l2311 = l2313;
    }
    at = 3160; l2314 = K[722];
    at = 3161; l2315 = rt.sequence_get(l2314, l2311);
    at = 3162; if (rt.truthy(l2256)) {
      at = 3163; l2317 = field(rt, l2259, "width");
      at = 3164; l2316 = l2317;
    } else {
      at = 3165; l2318 = K[723];
      at = 3166; l2316 = l2318;
    }
    at = 3167; markSet(rt, true, "line", {from: l2297, to: l2310, color: l2315, width: l2316});
    at = 3168; l2319 = field(rt, l15, "strokes");
    at = 3169; l2320 = rt.sequence_length(l2319);
    at = 3170; l2321 = K[724];
    at = 3171; l2322 = rt.lt(l2321, l2320);
    at = 3172; if (rt.truthy(l2322)) {
      at = 3173; l2323 = field(rt, l15, "strokes");
      at = 3174; l2324 = K[725];
      at = 3175; l2325 = rt.sequence_get(l2323, l2324);
    }
    at = 3176; if (rt.truthy(l2322)) {
      at = 3177; l2327 = field(rt, l2325, "points");
      at = 3178; l2326 = l2327;
    } else {
      at = 3179; l2328 = K[726];
      at = 3180; l2326 = l2328;
    }
    at = 3181; l2329 = rt.array_count(l2326);
    at = 3182; l2330 = K[727];
    at = 3183; l2331 = rt.gt(l2329, l2330);
    at = 3184; if (rt.truthy(l2331)) {
      at = 3185; if (rt.truthy(l2322)) {
        at = 3186; l2334 = field(rt, l2325, "points");
        at = 3187; l2333 = l2334;
      } else {
        at = 3188; l2335 = K[728];
        at = 3189; l2333 = l2335;
      }
      at = 3190; l2336 = K[729];
      at = 3191; l2337 = rt.array_nth(l2333, l2336);
      at = 3192; l2332 = l2337;
    } else {
      at = 3193; l2338 = K[730];
      at = 3194; l2332 = l2338;
    }
    at = 3195; if (rt.truthy(l2322)) {
      at = 3196; l2340 = field(rt, l2325, "width");
      at = 3197; l2339 = l2340;
    } else {
      at = 3198; l2341 = K[731];
      at = 3199; l2339 = l2341;
    }
    at = 3200; l2342 = K[732];
    at = 3201; l2343 = rt.int_div(l2339, l2342);
    at = 3202; l2344 = K[733];
    at = 3203; l2345 = rt.max(l2344, l2343);
    at = 3204; l2346 = K[734];
    at = 3205; l2347 = rt.gt(l2329, l2346);
    at = 3206; if (rt.truthy(l2347)) {
      at = 3207; if (rt.truthy(l2322)) {
        at = 3208; l2350 = field(rt, l2325, "color");
        at = 3209; l2349 = l2350;
      } else {
        at = 3210; l2351 = K[735];
        at = 3211; l2349 = l2351;
      }
      at = 3212; l2352 = K[736];
      at = 3213; l2353 = rt.sequence_get(l2352, l2349);
      at = 3214; l2348 = l2353;
    } else {
      at = 3215; l2354 = K[737];
      at = 3216; l2348 = l2354;
    }
    at = 3217; markSet(rt, false, "circle", {at: l2332, radius: l2345, fill: l2348});
    at = 3218; if (rt.truthy(l2322)) {
      at = 3219; l2356 = field(rt, l2325, "points");
      at = 3220; l2355 = l2356;
    } else {
      at = 3221; l2357 = K[738];
      at = 3222; l2355 = l2357;
    }
    at = 3223; l2358 = K[739];
    at = 3224; l2359 = rt.sub(l2329, l2358);
    at = 3225; l2360 = K[740];
    at = 3226; l2361 = rt.max(l2360, l2359);
    at = 3227; l2362 = K[741];
    at = 3228; l2363 = rt.array_slice(l2355, l2362, l2361);
    at = 3229; if (rt.truthy(l2322)) {
      at = 3230; l2365 = field(rt, l2325, "points");
      at = 3231; l2364 = l2365;
    } else {
      at = 3232; l2366 = K[742];
      at = 3233; l2364 = l2366;
    }
    at = 3234; l2367 = K[743];
    at = 3235; l2368 = rt.gt(l2329, l2367);
    at = 3236; if (rt.truthy(l2368)) {
      at = 3237; l2370 = K[744];
      at = 3238; l2369 = l2370;
    } else {
      at = 3239; l2371 = K[745];
      at = 3240; l2369 = l2371;
    }
    at = 3241; l2372 = K[746];
    at = 3242; l2373 = rt.sub(l2329, l2372);
    at = 3243; l2374 = K[747];
    at = 3244; l2375 = rt.max(l2374, l2373);
    at = 3245; l2376 = rt.array_slice(l2364, l2369, l2375);
    at = 3246; if (rt.truthy(l2322)) {
      at = 3247; l2378 = field(rt, l2325, "color");
      at = 3248; l2377 = l2378;
    } else {
      at = 3249; l2379 = K[748];
      at = 3250; l2377 = l2379;
    }
    at = 3251; l2380 = K[749];
    at = 3252; l2381 = rt.sequence_get(l2380, l2377);
    at = 3253; if (rt.truthy(l2322)) {
      at = 3254; l2383 = field(rt, l2325, "width");
      at = 3255; l2382 = l2383;
    } else {
      at = 3256; l2384 = K[750];
      at = 3257; l2382 = l2384;
    }
    at = 3258; markSet(rt, true, "line", {from: l2363, to: l2376, color: l2381, width: l2382});
    at = 3259; l2385 = field(rt, l15, "strokes");
    at = 3260; l2386 = rt.sequence_length(l2385);
    at = 3261; l2387 = K[751];
    at = 3262; l2388 = rt.lt(l2387, l2386);
    at = 3263; if (rt.truthy(l2388)) {
      at = 3264; l2389 = field(rt, l15, "strokes");
      at = 3265; l2390 = K[752];
      at = 3266; l2391 = rt.sequence_get(l2389, l2390);
    }
    at = 3267; if (rt.truthy(l2388)) {
      at = 3268; l2393 = field(rt, l2391, "points");
      at = 3269; l2392 = l2393;
    } else {
      at = 3270; l2394 = K[753];
      at = 3271; l2392 = l2394;
    }
    at = 3272; l2395 = rt.array_count(l2392);
    at = 3273; l2396 = K[754];
    at = 3274; l2397 = rt.gt(l2395, l2396);
    at = 3275; if (rt.truthy(l2397)) {
      at = 3276; if (rt.truthy(l2388)) {
        at = 3277; l2400 = field(rt, l2391, "points");
        at = 3278; l2399 = l2400;
      } else {
        at = 3279; l2401 = K[755];
        at = 3280; l2399 = l2401;
      }
      at = 3281; l2402 = K[756];
      at = 3282; l2403 = rt.array_nth(l2399, l2402);
      at = 3283; l2398 = l2403;
    } else {
      at = 3284; l2404 = K[757];
      at = 3285; l2398 = l2404;
    }
    at = 3286; if (rt.truthy(l2388)) {
      at = 3287; l2406 = field(rt, l2391, "width");
      at = 3288; l2405 = l2406;
    } else {
      at = 3289; l2407 = K[758];
      at = 3290; l2405 = l2407;
    }
    at = 3291; l2408 = K[759];
    at = 3292; l2409 = rt.int_div(l2405, l2408);
    at = 3293; l2410 = K[760];
    at = 3294; l2411 = rt.max(l2410, l2409);
    at = 3295; l2412 = K[761];
    at = 3296; l2413 = rt.gt(l2395, l2412);
    at = 3297; if (rt.truthy(l2413)) {
      at = 3298; if (rt.truthy(l2388)) {
        at = 3299; l2416 = field(rt, l2391, "color");
        at = 3300; l2415 = l2416;
      } else {
        at = 3301; l2417 = K[762];
        at = 3302; l2415 = l2417;
      }
      at = 3303; l2418 = K[763];
      at = 3304; l2419 = rt.sequence_get(l2418, l2415);
      at = 3305; l2414 = l2419;
    } else {
      at = 3306; l2420 = K[764];
      at = 3307; l2414 = l2420;
    }
    at = 3308; markSet(rt, false, "circle", {at: l2398, radius: l2411, fill: l2414});
    at = 3309; if (rt.truthy(l2388)) {
      at = 3310; l2422 = field(rt, l2391, "points");
      at = 3311; l2421 = l2422;
    } else {
      at = 3312; l2423 = K[765];
      at = 3313; l2421 = l2423;
    }
    at = 3314; l2424 = K[766];
    at = 3315; l2425 = rt.sub(l2395, l2424);
    at = 3316; l2426 = K[767];
    at = 3317; l2427 = rt.max(l2426, l2425);
    at = 3318; l2428 = K[768];
    at = 3319; l2429 = rt.array_slice(l2421, l2428, l2427);
    at = 3320; if (rt.truthy(l2388)) {
      at = 3321; l2431 = field(rt, l2391, "points");
      at = 3322; l2430 = l2431;
    } else {
      at = 3323; l2432 = K[769];
      at = 3324; l2430 = l2432;
    }
    at = 3325; l2433 = K[770];
    at = 3326; l2434 = rt.gt(l2395, l2433);
    at = 3327; if (rt.truthy(l2434)) {
      at = 3328; l2436 = K[771];
      at = 3329; l2435 = l2436;
    } else {
      at = 3330; l2437 = K[772];
      at = 3331; l2435 = l2437;
    }
    at = 3332; l2438 = K[773];
    at = 3333; l2439 = rt.sub(l2395, l2438);
    at = 3334; l2440 = K[774];
    at = 3335; l2441 = rt.max(l2440, l2439);
    at = 3336; l2442 = rt.array_slice(l2430, l2435, l2441);
    at = 3337; if (rt.truthy(l2388)) {
      at = 3338; l2444 = field(rt, l2391, "color");
      at = 3339; l2443 = l2444;
    } else {
      at = 3340; l2445 = K[775];
      at = 3341; l2443 = l2445;
    }
    at = 3342; l2446 = K[776];
    at = 3343; l2447 = rt.sequence_get(l2446, l2443);
    at = 3344; if (rt.truthy(l2388)) {
      at = 3345; l2449 = field(rt, l2391, "width");
      at = 3346; l2448 = l2449;
    } else {
      at = 3347; l2450 = K[777];
      at = 3348; l2448 = l2450;
    }
    at = 3349; markSet(rt, true, "line", {from: l2429, to: l2442, color: l2447, width: l2448});
    at = 3350; l2451 = field(rt, l15, "strokes");
    at = 3351; l2452 = rt.sequence_length(l2451);
    at = 3352; l2453 = K[778];
    at = 3353; l2454 = rt.lt(l2453, l2452);
    at = 3354; if (rt.truthy(l2454)) {
      at = 3355; l2455 = field(rt, l15, "strokes");
      at = 3356; l2456 = K[779];
      at = 3357; l2457 = rt.sequence_get(l2455, l2456);
    }
    at = 3358; if (rt.truthy(l2454)) {
      at = 3359; l2459 = field(rt, l2457, "points");
      at = 3360; l2458 = l2459;
    } else {
      at = 3361; l2460 = K[780];
      at = 3362; l2458 = l2460;
    }
    at = 3363; l2461 = rt.array_count(l2458);
    at = 3364; l2462 = K[781];
    at = 3365; l2463 = rt.gt(l2461, l2462);
    at = 3366; if (rt.truthy(l2463)) {
      at = 3367; if (rt.truthy(l2454)) {
        at = 3368; l2466 = field(rt, l2457, "points");
        at = 3369; l2465 = l2466;
      } else {
        at = 3370; l2467 = K[782];
        at = 3371; l2465 = l2467;
      }
      at = 3372; l2468 = K[783];
      at = 3373; l2469 = rt.array_nth(l2465, l2468);
      at = 3374; l2464 = l2469;
    } else {
      at = 3375; l2470 = K[784];
      at = 3376; l2464 = l2470;
    }
    at = 3377; if (rt.truthy(l2454)) {
      at = 3378; l2472 = field(rt, l2457, "width");
      at = 3379; l2471 = l2472;
    } else {
      at = 3380; l2473 = K[785];
      at = 3381; l2471 = l2473;
    }
    at = 3382; l2474 = K[786];
    at = 3383; l2475 = rt.int_div(l2471, l2474);
    at = 3384; l2476 = K[787];
    at = 3385; l2477 = rt.max(l2476, l2475);
    at = 3386; l2478 = K[788];
    at = 3387; l2479 = rt.gt(l2461, l2478);
    at = 3388; if (rt.truthy(l2479)) {
      at = 3389; if (rt.truthy(l2454)) {
        at = 3390; l2482 = field(rt, l2457, "color");
        at = 3391; l2481 = l2482;
      } else {
        at = 3392; l2483 = K[789];
        at = 3393; l2481 = l2483;
      }
      at = 3394; l2484 = K[790];
      at = 3395; l2485 = rt.sequence_get(l2484, l2481);
      at = 3396; l2480 = l2485;
    } else {
      at = 3397; l2486 = K[791];
      at = 3398; l2480 = l2486;
    }
    at = 3399; markSet(rt, false, "circle", {at: l2464, radius: l2477, fill: l2480});
    at = 3400; if (rt.truthy(l2454)) {
      at = 3401; l2488 = field(rt, l2457, "points");
      at = 3402; l2487 = l2488;
    } else {
      at = 3403; l2489 = K[792];
      at = 3404; l2487 = l2489;
    }
    at = 3405; l2490 = K[793];
    at = 3406; l2491 = rt.sub(l2461, l2490);
    at = 3407; l2492 = K[794];
    at = 3408; l2493 = rt.max(l2492, l2491);
    at = 3409; l2494 = K[795];
    at = 3410; l2495 = rt.array_slice(l2487, l2494, l2493);
    at = 3411; if (rt.truthy(l2454)) {
      at = 3412; l2497 = field(rt, l2457, "points");
      at = 3413; l2496 = l2497;
    } else {
      at = 3414; l2498 = K[796];
      at = 3415; l2496 = l2498;
    }
    at = 3416; l2499 = K[797];
    at = 3417; l2500 = rt.gt(l2461, l2499);
    at = 3418; if (rt.truthy(l2500)) {
      at = 3419; l2502 = K[798];
      at = 3420; l2501 = l2502;
    } else {
      at = 3421; l2503 = K[799];
      at = 3422; l2501 = l2503;
    }
    at = 3423; l2504 = K[800];
    at = 3424; l2505 = rt.sub(l2461, l2504);
    at = 3425; l2506 = K[801];
    at = 3426; l2507 = rt.max(l2506, l2505);
    at = 3427; l2508 = rt.array_slice(l2496, l2501, l2507);
    at = 3428; if (rt.truthy(l2454)) {
      at = 3429; l2510 = field(rt, l2457, "color");
      at = 3430; l2509 = l2510;
    } else {
      at = 3431; l2511 = K[802];
      at = 3432; l2509 = l2511;
    }
    at = 3433; l2512 = K[803];
    at = 3434; l2513 = rt.sequence_get(l2512, l2509);
    at = 3435; if (rt.truthy(l2454)) {
      at = 3436; l2515 = field(rt, l2457, "width");
      at = 3437; l2514 = l2515;
    } else {
      at = 3438; l2516 = K[804];
      at = 3439; l2514 = l2516;
    }
    at = 3440; markSet(rt, true, "line", {from: l2495, to: l2508, color: l2513, width: l2514});
    at = 3441; l2517 = field(rt, l15, "strokes");
    at = 3442; l2518 = rt.sequence_length(l2517);
    at = 3443; l2519 = K[805];
    at = 3444; l2520 = rt.lt(l2519, l2518);
    at = 3445; if (rt.truthy(l2520)) {
      at = 3446; l2521 = field(rt, l15, "strokes");
      at = 3447; l2522 = K[806];
      at = 3448; l2523 = rt.sequence_get(l2521, l2522);
    }
    at = 3449; if (rt.truthy(l2520)) {
      at = 3450; l2525 = field(rt, l2523, "points");
      at = 3451; l2524 = l2525;
    } else {
      at = 3452; l2526 = K[807];
      at = 3453; l2524 = l2526;
    }
    at = 3454; l2527 = rt.array_count(l2524);
    at = 3455; l2528 = K[808];
    at = 3456; l2529 = rt.gt(l2527, l2528);
    at = 3457; if (rt.truthy(l2529)) {
      at = 3458; if (rt.truthy(l2520)) {
        at = 3459; l2532 = field(rt, l2523, "points");
        at = 3460; l2531 = l2532;
      } else {
        at = 3461; l2533 = K[809];
        at = 3462; l2531 = l2533;
      }
      at = 3463; l2534 = K[810];
      at = 3464; l2535 = rt.array_nth(l2531, l2534);
      at = 3465; l2530 = l2535;
    } else {
      at = 3466; l2536 = K[811];
      at = 3467; l2530 = l2536;
    }
    at = 3468; if (rt.truthy(l2520)) {
      at = 3469; l2538 = field(rt, l2523, "width");
      at = 3470; l2537 = l2538;
    } else {
      at = 3471; l2539 = K[812];
      at = 3472; l2537 = l2539;
    }
    at = 3473; l2540 = K[813];
    at = 3474; l2541 = rt.int_div(l2537, l2540);
    at = 3475; l2542 = K[814];
    at = 3476; l2543 = rt.max(l2542, l2541);
    at = 3477; l2544 = K[815];
    at = 3478; l2545 = rt.gt(l2527, l2544);
    at = 3479; if (rt.truthy(l2545)) {
      at = 3480; if (rt.truthy(l2520)) {
        at = 3481; l2548 = field(rt, l2523, "color");
        at = 3482; l2547 = l2548;
      } else {
        at = 3483; l2549 = K[816];
        at = 3484; l2547 = l2549;
      }
      at = 3485; l2550 = K[817];
      at = 3486; l2551 = rt.sequence_get(l2550, l2547);
      at = 3487; l2546 = l2551;
    } else {
      at = 3488; l2552 = K[818];
      at = 3489; l2546 = l2552;
    }
    at = 3490; markSet(rt, false, "circle", {at: l2530, radius: l2543, fill: l2546});
    at = 3491; if (rt.truthy(l2520)) {
      at = 3492; l2554 = field(rt, l2523, "points");
      at = 3493; l2553 = l2554;
    } else {
      at = 3494; l2555 = K[819];
      at = 3495; l2553 = l2555;
    }
    at = 3496; l2556 = K[820];
    at = 3497; l2557 = rt.sub(l2527, l2556);
    at = 3498; l2558 = K[821];
    at = 3499; l2559 = rt.max(l2558, l2557);
    at = 3500; l2560 = K[822];
    at = 3501; l2561 = rt.array_slice(l2553, l2560, l2559);
    at = 3502; if (rt.truthy(l2520)) {
      at = 3503; l2563 = field(rt, l2523, "points");
      at = 3504; l2562 = l2563;
    } else {
      at = 3505; l2564 = K[823];
      at = 3506; l2562 = l2564;
    }
    at = 3507; l2565 = K[824];
    at = 3508; l2566 = rt.gt(l2527, l2565);
    at = 3509; if (rt.truthy(l2566)) {
      at = 3510; l2568 = K[825];
      at = 3511; l2567 = l2568;
    } else {
      at = 3512; l2569 = K[826];
      at = 3513; l2567 = l2569;
    }
    at = 3514; l2570 = K[827];
    at = 3515; l2571 = rt.sub(l2527, l2570);
    at = 3516; l2572 = K[828];
    at = 3517; l2573 = rt.max(l2572, l2571);
    at = 3518; l2574 = rt.array_slice(l2562, l2567, l2573);
    at = 3519; if (rt.truthy(l2520)) {
      at = 3520; l2576 = field(rt, l2523, "color");
      at = 3521; l2575 = l2576;
    } else {
      at = 3522; l2577 = K[829];
      at = 3523; l2575 = l2577;
    }
    at = 3524; l2578 = K[830];
    at = 3525; l2579 = rt.sequence_get(l2578, l2575);
    at = 3526; if (rt.truthy(l2520)) {
      at = 3527; l2581 = field(rt, l2523, "width");
      at = 3528; l2580 = l2581;
    } else {
      at = 3529; l2582 = K[831];
      at = 3530; l2580 = l2582;
    }
    at = 3531; markSet(rt, true, "line", {from: l2561, to: l2574, color: l2579, width: l2580});
    at = 3532; l2583 = field(rt, l15, "strokes");
    at = 3533; l2584 = rt.sequence_length(l2583);
    at = 3534; l2585 = K[832];
    at = 3535; l2586 = rt.lt(l2585, l2584);
    at = 3536; if (rt.truthy(l2586)) {
      at = 3537; l2587 = field(rt, l15, "strokes");
      at = 3538; l2588 = K[833];
      at = 3539; l2589 = rt.sequence_get(l2587, l2588);
    }
    at = 3540; if (rt.truthy(l2586)) {
      at = 3541; l2591 = field(rt, l2589, "points");
      at = 3542; l2590 = l2591;
    } else {
      at = 3543; l2592 = K[834];
      at = 3544; l2590 = l2592;
    }
    at = 3545; l2593 = rt.array_count(l2590);
    at = 3546; l2594 = K[835];
    at = 3547; l2595 = rt.gt(l2593, l2594);
    at = 3548; if (rt.truthy(l2595)) {
      at = 3549; if (rt.truthy(l2586)) {
        at = 3550; l2598 = field(rt, l2589, "points");
        at = 3551; l2597 = l2598;
      } else {
        at = 3552; l2599 = K[836];
        at = 3553; l2597 = l2599;
      }
      at = 3554; l2600 = K[837];
      at = 3555; l2601 = rt.array_nth(l2597, l2600);
      at = 3556; l2596 = l2601;
    } else {
      at = 3557; l2602 = K[838];
      at = 3558; l2596 = l2602;
    }
    at = 3559; if (rt.truthy(l2586)) {
      at = 3560; l2604 = field(rt, l2589, "width");
      at = 3561; l2603 = l2604;
    } else {
      at = 3562; l2605 = K[839];
      at = 3563; l2603 = l2605;
    }
    at = 3564; l2606 = K[840];
    at = 3565; l2607 = rt.int_div(l2603, l2606);
    at = 3566; l2608 = K[841];
    at = 3567; l2609 = rt.max(l2608, l2607);
    at = 3568; l2610 = K[842];
    at = 3569; l2611 = rt.gt(l2593, l2610);
    at = 3570; if (rt.truthy(l2611)) {
      at = 3571; if (rt.truthy(l2586)) {
        at = 3572; l2614 = field(rt, l2589, "color");
        at = 3573; l2613 = l2614;
      } else {
        at = 3574; l2615 = K[843];
        at = 3575; l2613 = l2615;
      }
      at = 3576; l2616 = K[844];
      at = 3577; l2617 = rt.sequence_get(l2616, l2613);
      at = 3578; l2612 = l2617;
    } else {
      at = 3579; l2618 = K[845];
      at = 3580; l2612 = l2618;
    }
    at = 3581; markSet(rt, false, "circle", {at: l2596, radius: l2609, fill: l2612});
    at = 3582; if (rt.truthy(l2586)) {
      at = 3583; l2620 = field(rt, l2589, "points");
      at = 3584; l2619 = l2620;
    } else {
      at = 3585; l2621 = K[846];
      at = 3586; l2619 = l2621;
    }
    at = 3587; l2622 = K[847];
    at = 3588; l2623 = rt.sub(l2593, l2622);
    at = 3589; l2624 = K[848];
    at = 3590; l2625 = rt.max(l2624, l2623);
    at = 3591; l2626 = K[849];
    at = 3592; l2627 = rt.array_slice(l2619, l2626, l2625);
    at = 3593; if (rt.truthy(l2586)) {
      at = 3594; l2629 = field(rt, l2589, "points");
      at = 3595; l2628 = l2629;
    } else {
      at = 3596; l2630 = K[850];
      at = 3597; l2628 = l2630;
    }
    at = 3598; l2631 = K[851];
    at = 3599; l2632 = rt.gt(l2593, l2631);
    at = 3600; if (rt.truthy(l2632)) {
      at = 3601; l2634 = K[852];
      at = 3602; l2633 = l2634;
    } else {
      at = 3603; l2635 = K[853];
      at = 3604; l2633 = l2635;
    }
    at = 3605; l2636 = K[854];
    at = 3606; l2637 = rt.sub(l2593, l2636);
    at = 3607; l2638 = K[855];
    at = 3608; l2639 = rt.max(l2638, l2637);
    at = 3609; l2640 = rt.array_slice(l2628, l2633, l2639);
    at = 3610; if (rt.truthy(l2586)) {
      at = 3611; l2642 = field(rt, l2589, "color");
      at = 3612; l2641 = l2642;
    } else {
      at = 3613; l2643 = K[856];
      at = 3614; l2641 = l2643;
    }
    at = 3615; l2644 = K[857];
    at = 3616; l2645 = rt.sequence_get(l2644, l2641);
    at = 3617; if (rt.truthy(l2586)) {
      at = 3618; l2647 = field(rt, l2589, "width");
      at = 3619; l2646 = l2647;
    } else {
      at = 3620; l2648 = K[858];
      at = 3621; l2646 = l2648;
    }
    at = 3622; markSet(rt, true, "line", {from: l2627, to: l2640, color: l2645, width: l2646});
    at = 3623; l2649 = field(rt, l15, "strokes");
    at = 3624; l2650 = rt.sequence_length(l2649);
    at = 3625; l2651 = K[859];
    at = 3626; l2652 = rt.lt(l2651, l2650);
    at = 3627; if (rt.truthy(l2652)) {
      at = 3628; l2653 = field(rt, l15, "strokes");
      at = 3629; l2654 = K[860];
      at = 3630; l2655 = rt.sequence_get(l2653, l2654);
    }
    at = 3631; if (rt.truthy(l2652)) {
      at = 3632; l2657 = field(rt, l2655, "points");
      at = 3633; l2656 = l2657;
    } else {
      at = 3634; l2658 = K[861];
      at = 3635; l2656 = l2658;
    }
    at = 3636; l2659 = rt.array_count(l2656);
    at = 3637; l2660 = K[862];
    at = 3638; l2661 = rt.gt(l2659, l2660);
    at = 3639; if (rt.truthy(l2661)) {
      at = 3640; if (rt.truthy(l2652)) {
        at = 3641; l2664 = field(rt, l2655, "points");
        at = 3642; l2663 = l2664;
      } else {
        at = 3643; l2665 = K[863];
        at = 3644; l2663 = l2665;
      }
      at = 3645; l2666 = K[864];
      at = 3646; l2667 = rt.array_nth(l2663, l2666);
      at = 3647; l2662 = l2667;
    } else {
      at = 3648; l2668 = K[865];
      at = 3649; l2662 = l2668;
    }
    at = 3650; if (rt.truthy(l2652)) {
      at = 3651; l2670 = field(rt, l2655, "width");
      at = 3652; l2669 = l2670;
    } else {
      at = 3653; l2671 = K[866];
      at = 3654; l2669 = l2671;
    }
    at = 3655; l2672 = K[867];
    at = 3656; l2673 = rt.int_div(l2669, l2672);
    at = 3657; l2674 = K[868];
    at = 3658; l2675 = rt.max(l2674, l2673);
    at = 3659; l2676 = K[869];
    at = 3660; l2677 = rt.gt(l2659, l2676);
    at = 3661; if (rt.truthy(l2677)) {
      at = 3662; if (rt.truthy(l2652)) {
        at = 3663; l2680 = field(rt, l2655, "color");
        at = 3664; l2679 = l2680;
      } else {
        at = 3665; l2681 = K[870];
        at = 3666; l2679 = l2681;
      }
      at = 3667; l2682 = K[871];
      at = 3668; l2683 = rt.sequence_get(l2682, l2679);
      at = 3669; l2678 = l2683;
    } else {
      at = 3670; l2684 = K[872];
      at = 3671; l2678 = l2684;
    }
    at = 3672; markSet(rt, false, "circle", {at: l2662, radius: l2675, fill: l2678});
    at = 3673; if (rt.truthy(l2652)) {
      at = 3674; l2686 = field(rt, l2655, "points");
      at = 3675; l2685 = l2686;
    } else {
      at = 3676; l2687 = K[873];
      at = 3677; l2685 = l2687;
    }
    at = 3678; l2688 = K[874];
    at = 3679; l2689 = rt.sub(l2659, l2688);
    at = 3680; l2690 = K[875];
    at = 3681; l2691 = rt.max(l2690, l2689);
    at = 3682; l2692 = K[876];
    at = 3683; l2693 = rt.array_slice(l2685, l2692, l2691);
    at = 3684; if (rt.truthy(l2652)) {
      at = 3685; l2695 = field(rt, l2655, "points");
      at = 3686; l2694 = l2695;
    } else {
      at = 3687; l2696 = K[877];
      at = 3688; l2694 = l2696;
    }
    at = 3689; l2697 = K[878];
    at = 3690; l2698 = rt.gt(l2659, l2697);
    at = 3691; if (rt.truthy(l2698)) {
      at = 3692; l2700 = K[879];
      at = 3693; l2699 = l2700;
    } else {
      at = 3694; l2701 = K[880];
      at = 3695; l2699 = l2701;
    }
    at = 3696; l2702 = K[881];
    at = 3697; l2703 = rt.sub(l2659, l2702);
    at = 3698; l2704 = K[882];
    at = 3699; l2705 = rt.max(l2704, l2703);
    at = 3700; l2706 = rt.array_slice(l2694, l2699, l2705);
    at = 3701; if (rt.truthy(l2652)) {
      at = 3702; l2708 = field(rt, l2655, "color");
      at = 3703; l2707 = l2708;
    } else {
      at = 3704; l2709 = K[883];
      at = 3705; l2707 = l2709;
    }
    at = 3706; l2710 = K[884];
    at = 3707; l2711 = rt.sequence_get(l2710, l2707);
    at = 3708; if (rt.truthy(l2652)) {
      at = 3709; l2713 = field(rt, l2655, "width");
      at = 3710; l2712 = l2713;
    } else {
      at = 3711; l2714 = K[885];
      at = 3712; l2712 = l2714;
    }
    at = 3713; markSet(rt, true, "line", {from: l2693, to: l2706, color: l2711, width: l2712});
    at = 3714; l2715 = field(rt, l15, "strokes");
    at = 3715; l2716 = rt.sequence_length(l2715);
    at = 3716; l2717 = K[886];
    at = 3717; l2718 = rt.lt(l2717, l2716);
    at = 3718; if (rt.truthy(l2718)) {
      at = 3719; l2719 = field(rt, l15, "strokes");
      at = 3720; l2720 = K[887];
      at = 3721; l2721 = rt.sequence_get(l2719, l2720);
    }
    at = 3722; if (rt.truthy(l2718)) {
      at = 3723; l2723 = field(rt, l2721, "points");
      at = 3724; l2722 = l2723;
    } else {
      at = 3725; l2724 = K[888];
      at = 3726; l2722 = l2724;
    }
    at = 3727; l2725 = rt.array_count(l2722);
    at = 3728; l2726 = K[889];
    at = 3729; l2727 = rt.gt(l2725, l2726);
    at = 3730; if (rt.truthy(l2727)) {
      at = 3731; if (rt.truthy(l2718)) {
        at = 3732; l2730 = field(rt, l2721, "points");
        at = 3733; l2729 = l2730;
      } else {
        at = 3734; l2731 = K[890];
        at = 3735; l2729 = l2731;
      }
      at = 3736; l2732 = K[891];
      at = 3737; l2733 = rt.array_nth(l2729, l2732);
      at = 3738; l2728 = l2733;
    } else {
      at = 3739; l2734 = K[892];
      at = 3740; l2728 = l2734;
    }
    at = 3741; if (rt.truthy(l2718)) {
      at = 3742; l2736 = field(rt, l2721, "width");
      at = 3743; l2735 = l2736;
    } else {
      at = 3744; l2737 = K[893];
      at = 3745; l2735 = l2737;
    }
    at = 3746; l2738 = K[894];
    at = 3747; l2739 = rt.int_div(l2735, l2738);
    at = 3748; l2740 = K[895];
    at = 3749; l2741 = rt.max(l2740, l2739);
    at = 3750; l2742 = K[896];
    at = 3751; l2743 = rt.gt(l2725, l2742);
    at = 3752; if (rt.truthy(l2743)) {
      at = 3753; if (rt.truthy(l2718)) {
        at = 3754; l2746 = field(rt, l2721, "color");
        at = 3755; l2745 = l2746;
      } else {
        at = 3756; l2747 = K[897];
        at = 3757; l2745 = l2747;
      }
      at = 3758; l2748 = K[898];
      at = 3759; l2749 = rt.sequence_get(l2748, l2745);
      at = 3760; l2744 = l2749;
    } else {
      at = 3761; l2750 = K[899];
      at = 3762; l2744 = l2750;
    }
    at = 3763; markSet(rt, false, "circle", {at: l2728, radius: l2741, fill: l2744});
    at = 3764; if (rt.truthy(l2718)) {
      at = 3765; l2752 = field(rt, l2721, "points");
      at = 3766; l2751 = l2752;
    } else {
      at = 3767; l2753 = K[900];
      at = 3768; l2751 = l2753;
    }
    at = 3769; l2754 = K[901];
    at = 3770; l2755 = rt.sub(l2725, l2754);
    at = 3771; l2756 = K[902];
    at = 3772; l2757 = rt.max(l2756, l2755);
    at = 3773; l2758 = K[903];
    at = 3774; l2759 = rt.array_slice(l2751, l2758, l2757);
    at = 3775; if (rt.truthy(l2718)) {
      at = 3776; l2761 = field(rt, l2721, "points");
      at = 3777; l2760 = l2761;
    } else {
      at = 3778; l2762 = K[904];
      at = 3779; l2760 = l2762;
    }
    at = 3780; l2763 = K[905];
    at = 3781; l2764 = rt.gt(l2725, l2763);
    at = 3782; if (rt.truthy(l2764)) {
      at = 3783; l2766 = K[906];
      at = 3784; l2765 = l2766;
    } else {
      at = 3785; l2767 = K[907];
      at = 3786; l2765 = l2767;
    }
    at = 3787; l2768 = K[908];
    at = 3788; l2769 = rt.sub(l2725, l2768);
    at = 3789; l2770 = K[909];
    at = 3790; l2771 = rt.max(l2770, l2769);
    at = 3791; l2772 = rt.array_slice(l2760, l2765, l2771);
    at = 3792; if (rt.truthy(l2718)) {
      at = 3793; l2774 = field(rt, l2721, "color");
      at = 3794; l2773 = l2774;
    } else {
      at = 3795; l2775 = K[910];
      at = 3796; l2773 = l2775;
    }
    at = 3797; l2776 = K[911];
    at = 3798; l2777 = rt.sequence_get(l2776, l2773);
    at = 3799; if (rt.truthy(l2718)) {
      at = 3800; l2779 = field(rt, l2721, "width");
      at = 3801; l2778 = l2779;
    } else {
      at = 3802; l2780 = K[912];
      at = 3803; l2778 = l2780;
    }
    at = 3804; markSet(rt, true, "line", {from: l2759, to: l2772, color: l2777, width: l2778});
    at = 3805; l2781 = field(rt, l15, "strokes");
    at = 3806; l2782 = rt.sequence_length(l2781);
    at = 3807; l2783 = K[913];
    at = 3808; l2784 = rt.lt(l2783, l2782);
    at = 3809; if (rt.truthy(l2784)) {
      at = 3810; l2785 = field(rt, l15, "strokes");
      at = 3811; l2786 = K[914];
      at = 3812; l2787 = rt.sequence_get(l2785, l2786);
    }
    at = 3813; if (rt.truthy(l2784)) {
      at = 3814; l2789 = field(rt, l2787, "points");
      at = 3815; l2788 = l2789;
    } else {
      at = 3816; l2790 = K[915];
      at = 3817; l2788 = l2790;
    }
    at = 3818; l2791 = rt.array_count(l2788);
    at = 3819; l2792 = K[916];
    at = 3820; l2793 = rt.gt(l2791, l2792);
    at = 3821; if (rt.truthy(l2793)) {
      at = 3822; if (rt.truthy(l2784)) {
        at = 3823; l2796 = field(rt, l2787, "points");
        at = 3824; l2795 = l2796;
      } else {
        at = 3825; l2797 = K[917];
        at = 3826; l2795 = l2797;
      }
      at = 3827; l2798 = K[918];
      at = 3828; l2799 = rt.array_nth(l2795, l2798);
      at = 3829; l2794 = l2799;
    } else {
      at = 3830; l2800 = K[919];
      at = 3831; l2794 = l2800;
    }
    at = 3832; if (rt.truthy(l2784)) {
      at = 3833; l2802 = field(rt, l2787, "width");
      at = 3834; l2801 = l2802;
    } else {
      at = 3835; l2803 = K[920];
      at = 3836; l2801 = l2803;
    }
    at = 3837; l2804 = K[921];
    at = 3838; l2805 = rt.int_div(l2801, l2804);
    at = 3839; l2806 = K[922];
    at = 3840; l2807 = rt.max(l2806, l2805);
    at = 3841; l2808 = K[923];
    at = 3842; l2809 = rt.gt(l2791, l2808);
    at = 3843; if (rt.truthy(l2809)) {
      at = 3844; if (rt.truthy(l2784)) {
        at = 3845; l2812 = field(rt, l2787, "color");
        at = 3846; l2811 = l2812;
      } else {
        at = 3847; l2813 = K[924];
        at = 3848; l2811 = l2813;
      }
      at = 3849; l2814 = K[925];
      at = 3850; l2815 = rt.sequence_get(l2814, l2811);
      at = 3851; l2810 = l2815;
    } else {
      at = 3852; l2816 = K[926];
      at = 3853; l2810 = l2816;
    }
    at = 3854; markSet(rt, false, "circle", {at: l2794, radius: l2807, fill: l2810});
    at = 3855; if (rt.truthy(l2784)) {
      at = 3856; l2818 = field(rt, l2787, "points");
      at = 3857; l2817 = l2818;
    } else {
      at = 3858; l2819 = K[927];
      at = 3859; l2817 = l2819;
    }
    at = 3860; l2820 = K[928];
    at = 3861; l2821 = rt.sub(l2791, l2820);
    at = 3862; l2822 = K[929];
    at = 3863; l2823 = rt.max(l2822, l2821);
    at = 3864; l2824 = K[930];
    at = 3865; l2825 = rt.array_slice(l2817, l2824, l2823);
    at = 3866; if (rt.truthy(l2784)) {
      at = 3867; l2827 = field(rt, l2787, "points");
      at = 3868; l2826 = l2827;
    } else {
      at = 3869; l2828 = K[931];
      at = 3870; l2826 = l2828;
    }
    at = 3871; l2829 = K[932];
    at = 3872; l2830 = rt.gt(l2791, l2829);
    at = 3873; if (rt.truthy(l2830)) {
      at = 3874; l2832 = K[933];
      at = 3875; l2831 = l2832;
    } else {
      at = 3876; l2833 = K[934];
      at = 3877; l2831 = l2833;
    }
    at = 3878; l2834 = K[935];
    at = 3879; l2835 = rt.sub(l2791, l2834);
    at = 3880; l2836 = K[936];
    at = 3881; l2837 = rt.max(l2836, l2835);
    at = 3882; l2838 = rt.array_slice(l2826, l2831, l2837);
    at = 3883; if (rt.truthy(l2784)) {
      at = 3884; l2840 = field(rt, l2787, "color");
      at = 3885; l2839 = l2840;
    } else {
      at = 3886; l2841 = K[937];
      at = 3887; l2839 = l2841;
    }
    at = 3888; l2842 = K[938];
    at = 3889; l2843 = rt.sequence_get(l2842, l2839);
    at = 3890; if (rt.truthy(l2784)) {
      at = 3891; l2845 = field(rt, l2787, "width");
      at = 3892; l2844 = l2845;
    } else {
      at = 3893; l2846 = K[939];
      at = 3894; l2844 = l2846;
    }
    at = 3895; markSet(rt, true, "line", {from: l2825, to: l2838, color: l2843, width: l2844});
    at = 3896; l2847 = field(rt, l15, "strokes");
    at = 3897; l2848 = rt.sequence_length(l2847);
    at = 3898; l2849 = K[940];
    at = 3899; l2850 = rt.lt(l2849, l2848);
    at = 3900; if (rt.truthy(l2850)) {
      at = 3901; l2851 = field(rt, l15, "strokes");
      at = 3902; l2852 = K[941];
      at = 3903; l2853 = rt.sequence_get(l2851, l2852);
    }
    at = 3904; if (rt.truthy(l2850)) {
      at = 3905; l2855 = field(rt, l2853, "points");
      at = 3906; l2854 = l2855;
    } else {
      at = 3907; l2856 = K[942];
      at = 3908; l2854 = l2856;
    }
    at = 3909; l2857 = rt.array_count(l2854);
    at = 3910; l2858 = K[943];
    at = 3911; l2859 = rt.gt(l2857, l2858);
    at = 3912; if (rt.truthy(l2859)) {
      at = 3913; if (rt.truthy(l2850)) {
        at = 3914; l2862 = field(rt, l2853, "points");
        at = 3915; l2861 = l2862;
      } else {
        at = 3916; l2863 = K[944];
        at = 3917; l2861 = l2863;
      }
      at = 3918; l2864 = K[945];
      at = 3919; l2865 = rt.array_nth(l2861, l2864);
      at = 3920; l2860 = l2865;
    } else {
      at = 3921; l2866 = K[946];
      at = 3922; l2860 = l2866;
    }
    at = 3923; if (rt.truthy(l2850)) {
      at = 3924; l2868 = field(rt, l2853, "width");
      at = 3925; l2867 = l2868;
    } else {
      at = 3926; l2869 = K[947];
      at = 3927; l2867 = l2869;
    }
    at = 3928; l2870 = K[948];
    at = 3929; l2871 = rt.int_div(l2867, l2870);
    at = 3930; l2872 = K[949];
    at = 3931; l2873 = rt.max(l2872, l2871);
    at = 3932; l2874 = K[950];
    at = 3933; l2875 = rt.gt(l2857, l2874);
    at = 3934; if (rt.truthy(l2875)) {
      at = 3935; if (rt.truthy(l2850)) {
        at = 3936; l2878 = field(rt, l2853, "color");
        at = 3937; l2877 = l2878;
      } else {
        at = 3938; l2879 = K[951];
        at = 3939; l2877 = l2879;
      }
      at = 3940; l2880 = K[952];
      at = 3941; l2881 = rt.sequence_get(l2880, l2877);
      at = 3942; l2876 = l2881;
    } else {
      at = 3943; l2882 = K[953];
      at = 3944; l2876 = l2882;
    }
    at = 3945; markSet(rt, false, "circle", {at: l2860, radius: l2873, fill: l2876});
    at = 3946; if (rt.truthy(l2850)) {
      at = 3947; l2884 = field(rt, l2853, "points");
      at = 3948; l2883 = l2884;
    } else {
      at = 3949; l2885 = K[954];
      at = 3950; l2883 = l2885;
    }
    at = 3951; l2886 = K[955];
    at = 3952; l2887 = rt.sub(l2857, l2886);
    at = 3953; l2888 = K[956];
    at = 3954; l2889 = rt.max(l2888, l2887);
    at = 3955; l2890 = K[957];
    at = 3956; l2891 = rt.array_slice(l2883, l2890, l2889);
    at = 3957; if (rt.truthy(l2850)) {
      at = 3958; l2893 = field(rt, l2853, "points");
      at = 3959; l2892 = l2893;
    } else {
      at = 3960; l2894 = K[958];
      at = 3961; l2892 = l2894;
    }
    at = 3962; l2895 = K[959];
    at = 3963; l2896 = rt.gt(l2857, l2895);
    at = 3964; if (rt.truthy(l2896)) {
      at = 3965; l2898 = K[960];
      at = 3966; l2897 = l2898;
    } else {
      at = 3967; l2899 = K[961];
      at = 3968; l2897 = l2899;
    }
    at = 3969; l2900 = K[962];
    at = 3970; l2901 = rt.sub(l2857, l2900);
    at = 3971; l2902 = K[963];
    at = 3972; l2903 = rt.max(l2902, l2901);
    at = 3973; l2904 = rt.array_slice(l2892, l2897, l2903);
    at = 3974; if (rt.truthy(l2850)) {
      at = 3975; l2906 = field(rt, l2853, "color");
      at = 3976; l2905 = l2906;
    } else {
      at = 3977; l2907 = K[964];
      at = 3978; l2905 = l2907;
    }
    at = 3979; l2908 = K[965];
    at = 3980; l2909 = rt.sequence_get(l2908, l2905);
    at = 3981; if (rt.truthy(l2850)) {
      at = 3982; l2911 = field(rt, l2853, "width");
      at = 3983; l2910 = l2911;
    } else {
      at = 3984; l2912 = K[966];
      at = 3985; l2910 = l2912;
    }
    at = 3986; markSet(rt, true, "line", {from: l2891, to: l2904, color: l2909, width: l2910});
    at = 3987; l2913 = field(rt, l15, "strokes");
    at = 3988; l2914 = rt.sequence_length(l2913);
    at = 3989; l2915 = K[967];
    at = 3990; l2916 = rt.lt(l2915, l2914);
    at = 3991; if (rt.truthy(l2916)) {
      at = 3992; l2917 = field(rt, l15, "strokes");
      at = 3993; l2918 = K[968];
      at = 3994; l2919 = rt.sequence_get(l2917, l2918);
    }
    at = 3995; if (rt.truthy(l2916)) {
      at = 3996; l2921 = field(rt, l2919, "points");
      at = 3997; l2920 = l2921;
    } else {
      at = 3998; l2922 = K[969];
      at = 3999; l2920 = l2922;
    }
    at = 4000; l2923 = rt.array_count(l2920);
    at = 4001; l2924 = K[970];
    at = 4002; l2925 = rt.gt(l2923, l2924);
    at = 4003; if (rt.truthy(l2925)) {
      at = 4004; if (rt.truthy(l2916)) {
        at = 4005; l2928 = field(rt, l2919, "points");
        at = 4006; l2927 = l2928;
      } else {
        at = 4007; l2929 = K[971];
        at = 4008; l2927 = l2929;
      }
      at = 4009; l2930 = K[972];
      at = 4010; l2931 = rt.array_nth(l2927, l2930);
      at = 4011; l2926 = l2931;
    } else {
      at = 4012; l2932 = K[973];
      at = 4013; l2926 = l2932;
    }
    at = 4014; if (rt.truthy(l2916)) {
      at = 4015; l2934 = field(rt, l2919, "width");
      at = 4016; l2933 = l2934;
    } else {
      at = 4017; l2935 = K[974];
      at = 4018; l2933 = l2935;
    }
    at = 4019; l2936 = K[975];
    at = 4020; l2937 = rt.int_div(l2933, l2936);
    at = 4021; l2938 = K[976];
    at = 4022; l2939 = rt.max(l2938, l2937);
    at = 4023; l2940 = K[977];
    at = 4024; l2941 = rt.gt(l2923, l2940);
    at = 4025; if (rt.truthy(l2941)) {
      at = 4026; if (rt.truthy(l2916)) {
        at = 4027; l2944 = field(rt, l2919, "color");
        at = 4028; l2943 = l2944;
      } else {
        at = 4029; l2945 = K[978];
        at = 4030; l2943 = l2945;
      }
      at = 4031; l2946 = K[979];
      at = 4032; l2947 = rt.sequence_get(l2946, l2943);
      at = 4033; l2942 = l2947;
    } else {
      at = 4034; l2948 = K[980];
      at = 4035; l2942 = l2948;
    }
    at = 4036; markSet(rt, false, "circle", {at: l2926, radius: l2939, fill: l2942});
    at = 4037; if (rt.truthy(l2916)) {
      at = 4038; l2950 = field(rt, l2919, "points");
      at = 4039; l2949 = l2950;
    } else {
      at = 4040; l2951 = K[981];
      at = 4041; l2949 = l2951;
    }
    at = 4042; l2952 = K[982];
    at = 4043; l2953 = rt.sub(l2923, l2952);
    at = 4044; l2954 = K[983];
    at = 4045; l2955 = rt.max(l2954, l2953);
    at = 4046; l2956 = K[984];
    at = 4047; l2957 = rt.array_slice(l2949, l2956, l2955);
    at = 4048; if (rt.truthy(l2916)) {
      at = 4049; l2959 = field(rt, l2919, "points");
      at = 4050; l2958 = l2959;
    } else {
      at = 4051; l2960 = K[985];
      at = 4052; l2958 = l2960;
    }
    at = 4053; l2961 = K[986];
    at = 4054; l2962 = rt.gt(l2923, l2961);
    at = 4055; if (rt.truthy(l2962)) {
      at = 4056; l2964 = K[987];
      at = 4057; l2963 = l2964;
    } else {
      at = 4058; l2965 = K[988];
      at = 4059; l2963 = l2965;
    }
    at = 4060; l2966 = K[989];
    at = 4061; l2967 = rt.sub(l2923, l2966);
    at = 4062; l2968 = K[990];
    at = 4063; l2969 = rt.max(l2968, l2967);
    at = 4064; l2970 = rt.array_slice(l2958, l2963, l2969);
    at = 4065; if (rt.truthy(l2916)) {
      at = 4066; l2972 = field(rt, l2919, "color");
      at = 4067; l2971 = l2972;
    } else {
      at = 4068; l2973 = K[991];
      at = 4069; l2971 = l2973;
    }
    at = 4070; l2974 = K[992];
    at = 4071; l2975 = rt.sequence_get(l2974, l2971);
    at = 4072; if (rt.truthy(l2916)) {
      at = 4073; l2977 = field(rt, l2919, "width");
      at = 4074; l2976 = l2977;
    } else {
      at = 4075; l2978 = K[993];
      at = 4076; l2976 = l2978;
    }
    at = 4077; markSet(rt, true, "line", {from: l2957, to: l2970, color: l2975, width: l2976});
    at = 4078; l2979 = field(rt, l15, "strokes");
    at = 4079; l2980 = rt.sequence_length(l2979);
    at = 4080; l2981 = K[994];
    at = 4081; l2982 = rt.lt(l2981, l2980);
    at = 4082; if (rt.truthy(l2982)) {
      at = 4083; l2983 = field(rt, l15, "strokes");
      at = 4084; l2984 = K[995];
      at = 4085; l2985 = rt.sequence_get(l2983, l2984);
    }
    at = 4086; if (rt.truthy(l2982)) {
      at = 4087; l2987 = field(rt, l2985, "points");
      at = 4088; l2986 = l2987;
    } else {
      at = 4089; l2988 = K[996];
      at = 4090; l2986 = l2988;
    }
    at = 4091; l2989 = rt.array_count(l2986);
    at = 4092; l2990 = K[997];
    at = 4093; l2991 = rt.gt(l2989, l2990);
    at = 4094; if (rt.truthy(l2991)) {
      at = 4095; if (rt.truthy(l2982)) {
        at = 4096; l2994 = field(rt, l2985, "points");
        at = 4097; l2993 = l2994;
      } else {
        at = 4098; l2995 = K[998];
        at = 4099; l2993 = l2995;
      }
      at = 4100; l2996 = K[999];
      at = 4101; l2997 = rt.array_nth(l2993, l2996);
      at = 4102; l2992 = l2997;
    } else {
      at = 4103; l2998 = K[1000];
      at = 4104; l2992 = l2998;
    }
    at = 4105; if (rt.truthy(l2982)) {
      at = 4106; l3000 = field(rt, l2985, "width");
      at = 4107; l2999 = l3000;
    } else {
      at = 4108; l3001 = K[1001];
      at = 4109; l2999 = l3001;
    }
    at = 4110; l3002 = K[1002];
    at = 4111; l3003 = rt.int_div(l2999, l3002);
    at = 4112; l3004 = K[1003];
    at = 4113; l3005 = rt.max(l3004, l3003);
    at = 4114; l3006 = K[1004];
    at = 4115; l3007 = rt.gt(l2989, l3006);
    at = 4116; if (rt.truthy(l3007)) {
      at = 4117; if (rt.truthy(l2982)) {
        at = 4118; l3010 = field(rt, l2985, "color");
        at = 4119; l3009 = l3010;
      } else {
        at = 4120; l3011 = K[1005];
        at = 4121; l3009 = l3011;
      }
      at = 4122; l3012 = K[1006];
      at = 4123; l3013 = rt.sequence_get(l3012, l3009);
      at = 4124; l3008 = l3013;
    } else {
      at = 4125; l3014 = K[1007];
      at = 4126; l3008 = l3014;
    }
    at = 4127; markSet(rt, false, "circle", {at: l2992, radius: l3005, fill: l3008});
    at = 4128; if (rt.truthy(l2982)) {
      at = 4129; l3016 = field(rt, l2985, "points");
      at = 4130; l3015 = l3016;
    } else {
      at = 4131; l3017 = K[1008];
      at = 4132; l3015 = l3017;
    }
    at = 4133; l3018 = K[1009];
    at = 4134; l3019 = rt.sub(l2989, l3018);
    at = 4135; l3020 = K[1010];
    at = 4136; l3021 = rt.max(l3020, l3019);
    at = 4137; l3022 = K[1011];
    at = 4138; l3023 = rt.array_slice(l3015, l3022, l3021);
    at = 4139; if (rt.truthy(l2982)) {
      at = 4140; l3025 = field(rt, l2985, "points");
      at = 4141; l3024 = l3025;
    } else {
      at = 4142; l3026 = K[1012];
      at = 4143; l3024 = l3026;
    }
    at = 4144; l3027 = K[1013];
    at = 4145; l3028 = rt.gt(l2989, l3027);
    at = 4146; if (rt.truthy(l3028)) {
      at = 4147; l3030 = K[1014];
      at = 4148; l3029 = l3030;
    } else {
      at = 4149; l3031 = K[1015];
      at = 4150; l3029 = l3031;
    }
    at = 4151; l3032 = K[1016];
    at = 4152; l3033 = rt.sub(l2989, l3032);
    at = 4153; l3034 = K[1017];
    at = 4154; l3035 = rt.max(l3034, l3033);
    at = 4155; l3036 = rt.array_slice(l3024, l3029, l3035);
    at = 4156; if (rt.truthy(l2982)) {
      at = 4157; l3038 = field(rt, l2985, "color");
      at = 4158; l3037 = l3038;
    } else {
      at = 4159; l3039 = K[1018];
      at = 4160; l3037 = l3039;
    }
    at = 4161; l3040 = K[1019];
    at = 4162; l3041 = rt.sequence_get(l3040, l3037);
    at = 4163; if (rt.truthy(l2982)) {
      at = 4164; l3043 = field(rt, l2985, "width");
      at = 4165; l3042 = l3043;
    } else {
      at = 4166; l3044 = K[1020];
      at = 4167; l3042 = l3044;
    }
    at = 4168; markSet(rt, true, "line", {from: l3023, to: l3036, color: l3041, width: l3042});
    at = 4169; l3045 = field(rt, l15, "strokes");
    at = 4170; l3046 = rt.sequence_length(l3045);
    at = 4171; l3047 = K[1021];
    at = 4172; l3048 = rt.lt(l3047, l3046);
    at = 4173; if (rt.truthy(l3048)) {
      at = 4174; l3049 = field(rt, l15, "strokes");
      at = 4175; l3050 = K[1022];
      at = 4176; l3051 = rt.sequence_get(l3049, l3050);
    }
    at = 4177; if (rt.truthy(l3048)) {
      at = 4178; l3053 = field(rt, l3051, "points");
      at = 4179; l3052 = l3053;
    } else {
      at = 4180; l3054 = K[1023];
      at = 4181; l3052 = l3054;
    }
    at = 4182; l3055 = rt.array_count(l3052);
    at = 4183; l3056 = K[1024];
    at = 4184; l3057 = rt.gt(l3055, l3056);
    at = 4185; if (rt.truthy(l3057)) {
      at = 4186; if (rt.truthy(l3048)) {
        at = 4187; l3060 = field(rt, l3051, "points");
        at = 4188; l3059 = l3060;
      } else {
        at = 4189; l3061 = K[1025];
        at = 4190; l3059 = l3061;
      }
      at = 4191; l3062 = K[1026];
      at = 4192; l3063 = rt.array_nth(l3059, l3062);
      at = 4193; l3058 = l3063;
    } else {
      at = 4194; l3064 = K[1027];
      at = 4195; l3058 = l3064;
    }
    at = 4196; if (rt.truthy(l3048)) {
      at = 4197; l3066 = field(rt, l3051, "width");
      at = 4198; l3065 = l3066;
    } else {
      at = 4199; l3067 = K[1028];
      at = 4200; l3065 = l3067;
    }
    at = 4201; l3068 = K[1029];
    at = 4202; l3069 = rt.int_div(l3065, l3068);
    at = 4203; l3070 = K[1030];
    at = 4204; l3071 = rt.max(l3070, l3069);
    at = 4205; l3072 = K[1031];
    at = 4206; l3073 = rt.gt(l3055, l3072);
    at = 4207; if (rt.truthy(l3073)) {
      at = 4208; if (rt.truthy(l3048)) {
        at = 4209; l3076 = field(rt, l3051, "color");
        at = 4210; l3075 = l3076;
      } else {
        at = 4211; l3077 = K[1032];
        at = 4212; l3075 = l3077;
      }
      at = 4213; l3078 = K[1033];
      at = 4214; l3079 = rt.sequence_get(l3078, l3075);
      at = 4215; l3074 = l3079;
    } else {
      at = 4216; l3080 = K[1034];
      at = 4217; l3074 = l3080;
    }
    at = 4218; markSet(rt, false, "circle", {at: l3058, radius: l3071, fill: l3074});
    at = 4219; if (rt.truthy(l3048)) {
      at = 4220; l3082 = field(rt, l3051, "points");
      at = 4221; l3081 = l3082;
    } else {
      at = 4222; l3083 = K[1035];
      at = 4223; l3081 = l3083;
    }
    at = 4224; l3084 = K[1036];
    at = 4225; l3085 = rt.sub(l3055, l3084);
    at = 4226; l3086 = K[1037];
    at = 4227; l3087 = rt.max(l3086, l3085);
    at = 4228; l3088 = K[1038];
    at = 4229; l3089 = rt.array_slice(l3081, l3088, l3087);
    at = 4230; if (rt.truthy(l3048)) {
      at = 4231; l3091 = field(rt, l3051, "points");
      at = 4232; l3090 = l3091;
    } else {
      at = 4233; l3092 = K[1039];
      at = 4234; l3090 = l3092;
    }
    at = 4235; l3093 = K[1040];
    at = 4236; l3094 = rt.gt(l3055, l3093);
    at = 4237; if (rt.truthy(l3094)) {
      at = 4238; l3096 = K[1041];
      at = 4239; l3095 = l3096;
    } else {
      at = 4240; l3097 = K[1042];
      at = 4241; l3095 = l3097;
    }
    at = 4242; l3098 = K[1043];
    at = 4243; l3099 = rt.sub(l3055, l3098);
    at = 4244; l3100 = K[1044];
    at = 4245; l3101 = rt.max(l3100, l3099);
    at = 4246; l3102 = rt.array_slice(l3090, l3095, l3101);
    at = 4247; if (rt.truthy(l3048)) {
      at = 4248; l3104 = field(rt, l3051, "color");
      at = 4249; l3103 = l3104;
    } else {
      at = 4250; l3105 = K[1045];
      at = 4251; l3103 = l3105;
    }
    at = 4252; l3106 = K[1046];
    at = 4253; l3107 = rt.sequence_get(l3106, l3103);
    at = 4254; if (rt.truthy(l3048)) {
      at = 4255; l3109 = field(rt, l3051, "width");
      at = 4256; l3108 = l3109;
    } else {
      at = 4257; l3110 = K[1047];
      at = 4258; l3108 = l3110;
    }
    at = 4259; markSet(rt, true, "line", {from: l3089, to: l3102, color: l3107, width: l3108});
    at = 4260; l3111 = field(rt, l15, "strokes");
    at = 4261; l3112 = rt.sequence_length(l3111);
    at = 4262; l3113 = K[1048];
    at = 4263; l3114 = rt.lt(l3113, l3112);
    at = 4264; if (rt.truthy(l3114)) {
      at = 4265; l3115 = field(rt, l15, "strokes");
      at = 4266; l3116 = K[1049];
      at = 4267; l3117 = rt.sequence_get(l3115, l3116);
    }
    at = 4268; if (rt.truthy(l3114)) {
      at = 4269; l3119 = field(rt, l3117, "points");
      at = 4270; l3118 = l3119;
    } else {
      at = 4271; l3120 = K[1050];
      at = 4272; l3118 = l3120;
    }
    at = 4273; l3121 = rt.array_count(l3118);
    at = 4274; l3122 = K[1051];
    at = 4275; l3123 = rt.gt(l3121, l3122);
    at = 4276; if (rt.truthy(l3123)) {
      at = 4277; if (rt.truthy(l3114)) {
        at = 4278; l3126 = field(rt, l3117, "points");
        at = 4279; l3125 = l3126;
      } else {
        at = 4280; l3127 = K[1052];
        at = 4281; l3125 = l3127;
      }
      at = 4282; l3128 = K[1053];
      at = 4283; l3129 = rt.array_nth(l3125, l3128);
      at = 4284; l3124 = l3129;
    } else {
      at = 4285; l3130 = K[1054];
      at = 4286; l3124 = l3130;
    }
    at = 4287; if (rt.truthy(l3114)) {
      at = 4288; l3132 = field(rt, l3117, "width");
      at = 4289; l3131 = l3132;
    } else {
      at = 4290; l3133 = K[1055];
      at = 4291; l3131 = l3133;
    }
    at = 4292; l3134 = K[1056];
    at = 4293; l3135 = rt.int_div(l3131, l3134);
    at = 4294; l3136 = K[1057];
    at = 4295; l3137 = rt.max(l3136, l3135);
    at = 4296; l3138 = K[1058];
    at = 4297; l3139 = rt.gt(l3121, l3138);
    at = 4298; if (rt.truthy(l3139)) {
      at = 4299; if (rt.truthy(l3114)) {
        at = 4300; l3142 = field(rt, l3117, "color");
        at = 4301; l3141 = l3142;
      } else {
        at = 4302; l3143 = K[1059];
        at = 4303; l3141 = l3143;
      }
      at = 4304; l3144 = K[1060];
      at = 4305; l3145 = rt.sequence_get(l3144, l3141);
      at = 4306; l3140 = l3145;
    } else {
      at = 4307; l3146 = K[1061];
      at = 4308; l3140 = l3146;
    }
    at = 4309; markSet(rt, false, "circle", {at: l3124, radius: l3137, fill: l3140});
    at = 4310; if (rt.truthy(l3114)) {
      at = 4311; l3148 = field(rt, l3117, "points");
      at = 4312; l3147 = l3148;
    } else {
      at = 4313; l3149 = K[1062];
      at = 4314; l3147 = l3149;
    }
    at = 4315; l3150 = K[1063];
    at = 4316; l3151 = rt.sub(l3121, l3150);
    at = 4317; l3152 = K[1064];
    at = 4318; l3153 = rt.max(l3152, l3151);
    at = 4319; l3154 = K[1065];
    at = 4320; l3155 = rt.array_slice(l3147, l3154, l3153);
    at = 4321; if (rt.truthy(l3114)) {
      at = 4322; l3157 = field(rt, l3117, "points");
      at = 4323; l3156 = l3157;
    } else {
      at = 4324; l3158 = K[1066];
      at = 4325; l3156 = l3158;
    }
    at = 4326; l3159 = K[1067];
    at = 4327; l3160 = rt.gt(l3121, l3159);
    at = 4328; if (rt.truthy(l3160)) {
      at = 4329; l3162 = K[1068];
      at = 4330; l3161 = l3162;
    } else {
      at = 4331; l3163 = K[1069];
      at = 4332; l3161 = l3163;
    }
    at = 4333; l3164 = K[1070];
    at = 4334; l3165 = rt.sub(l3121, l3164);
    at = 4335; l3166 = K[1071];
    at = 4336; l3167 = rt.max(l3166, l3165);
    at = 4337; l3168 = rt.array_slice(l3156, l3161, l3167);
    at = 4338; if (rt.truthy(l3114)) {
      at = 4339; l3170 = field(rt, l3117, "color");
      at = 4340; l3169 = l3170;
    } else {
      at = 4341; l3171 = K[1072];
      at = 4342; l3169 = l3171;
    }
    at = 4343; l3172 = K[1073];
    at = 4344; l3173 = rt.sequence_get(l3172, l3169);
    at = 4345; if (rt.truthy(l3114)) {
      at = 4346; l3175 = field(rt, l3117, "width");
      at = 4347; l3174 = l3175;
    } else {
      at = 4348; l3176 = K[1074];
      at = 4349; l3174 = l3176;
    }
    at = 4350; markSet(rt, true, "line", {from: l3155, to: l3168, color: l3173, width: l3174});
    at = 4351; l3177 = field(rt, l15, "strokes");
    at = 4352; l3178 = rt.sequence_length(l3177);
    at = 4353; l3179 = K[1075];
    at = 4354; l3180 = rt.lt(l3179, l3178);
    at = 4355; if (rt.truthy(l3180)) {
      at = 4356; l3181 = field(rt, l15, "strokes");
      at = 4357; l3182 = K[1076];
      at = 4358; l3183 = rt.sequence_get(l3181, l3182);
    }
    at = 4359; if (rt.truthy(l3180)) {
      at = 4360; l3185 = field(rt, l3183, "points");
      at = 4361; l3184 = l3185;
    } else {
      at = 4362; l3186 = K[1077];
      at = 4363; l3184 = l3186;
    }
    at = 4364; l3187 = rt.array_count(l3184);
    at = 4365; l3188 = K[1078];
    at = 4366; l3189 = rt.gt(l3187, l3188);
    at = 4367; if (rt.truthy(l3189)) {
      at = 4368; if (rt.truthy(l3180)) {
        at = 4369; l3192 = field(rt, l3183, "points");
        at = 4370; l3191 = l3192;
      } else {
        at = 4371; l3193 = K[1079];
        at = 4372; l3191 = l3193;
      }
      at = 4373; l3194 = K[1080];
      at = 4374; l3195 = rt.array_nth(l3191, l3194);
      at = 4375; l3190 = l3195;
    } else {
      at = 4376; l3196 = K[1081];
      at = 4377; l3190 = l3196;
    }
    at = 4378; if (rt.truthy(l3180)) {
      at = 4379; l3198 = field(rt, l3183, "width");
      at = 4380; l3197 = l3198;
    } else {
      at = 4381; l3199 = K[1082];
      at = 4382; l3197 = l3199;
    }
    at = 4383; l3200 = K[1083];
    at = 4384; l3201 = rt.int_div(l3197, l3200);
    at = 4385; l3202 = K[1084];
    at = 4386; l3203 = rt.max(l3202, l3201);
    at = 4387; l3204 = K[1085];
    at = 4388; l3205 = rt.gt(l3187, l3204);
    at = 4389; if (rt.truthy(l3205)) {
      at = 4390; if (rt.truthy(l3180)) {
        at = 4391; l3208 = field(rt, l3183, "color");
        at = 4392; l3207 = l3208;
      } else {
        at = 4393; l3209 = K[1086];
        at = 4394; l3207 = l3209;
      }
      at = 4395; l3210 = K[1087];
      at = 4396; l3211 = rt.sequence_get(l3210, l3207);
      at = 4397; l3206 = l3211;
    } else {
      at = 4398; l3212 = K[1088];
      at = 4399; l3206 = l3212;
    }
    at = 4400; markSet(rt, false, "circle", {at: l3190, radius: l3203, fill: l3206});
    at = 4401; if (rt.truthy(l3180)) {
      at = 4402; l3214 = field(rt, l3183, "points");
      at = 4403; l3213 = l3214;
    } else {
      at = 4404; l3215 = K[1089];
      at = 4405; l3213 = l3215;
    }
    at = 4406; l3216 = K[1090];
    at = 4407; l3217 = rt.sub(l3187, l3216);
    at = 4408; l3218 = K[1091];
    at = 4409; l3219 = rt.max(l3218, l3217);
    at = 4410; l3220 = K[1092];
    at = 4411; l3221 = rt.array_slice(l3213, l3220, l3219);
    at = 4412; if (rt.truthy(l3180)) {
      at = 4413; l3223 = field(rt, l3183, "points");
      at = 4414; l3222 = l3223;
    } else {
      at = 4415; l3224 = K[1093];
      at = 4416; l3222 = l3224;
    }
    at = 4417; l3225 = K[1094];
    at = 4418; l3226 = rt.gt(l3187, l3225);
    at = 4419; if (rt.truthy(l3226)) {
      at = 4420; l3228 = K[1095];
      at = 4421; l3227 = l3228;
    } else {
      at = 4422; l3229 = K[1096];
      at = 4423; l3227 = l3229;
    }
    at = 4424; l3230 = K[1097];
    at = 4425; l3231 = rt.sub(l3187, l3230);
    at = 4426; l3232 = K[1098];
    at = 4427; l3233 = rt.max(l3232, l3231);
    at = 4428; l3234 = rt.array_slice(l3222, l3227, l3233);
    at = 4429; if (rt.truthy(l3180)) {
      at = 4430; l3236 = field(rt, l3183, "color");
      at = 4431; l3235 = l3236;
    } else {
      at = 4432; l3237 = K[1099];
      at = 4433; l3235 = l3237;
    }
    at = 4434; l3238 = K[1100];
    at = 4435; l3239 = rt.sequence_get(l3238, l3235);
    at = 4436; if (rt.truthy(l3180)) {
      at = 4437; l3241 = field(rt, l3183, "width");
      at = 4438; l3240 = l3241;
    } else {
      at = 4439; l3242 = K[1101];
      at = 4440; l3240 = l3242;
    }
    at = 4441; markSet(rt, true, "line", {from: l3221, to: l3234, color: l3239, width: l3240});
    at = 4442; l3243 = field(rt, l15, "strokes");
    at = 4443; l3244 = rt.sequence_length(l3243);
    at = 4444; l3245 = K[1102];
    at = 4445; l3246 = rt.lt(l3245, l3244);
    at = 4446; if (rt.truthy(l3246)) {
      at = 4447; l3247 = field(rt, l15, "strokes");
      at = 4448; l3248 = K[1103];
      at = 4449; l3249 = rt.sequence_get(l3247, l3248);
    }
    at = 4450; if (rt.truthy(l3246)) {
      at = 4451; l3251 = field(rt, l3249, "points");
      at = 4452; l3250 = l3251;
    } else {
      at = 4453; l3252 = K[1104];
      at = 4454; l3250 = l3252;
    }
    at = 4455; l3253 = rt.array_count(l3250);
    at = 4456; l3254 = K[1105];
    at = 4457; l3255 = rt.gt(l3253, l3254);
    at = 4458; if (rt.truthy(l3255)) {
      at = 4459; if (rt.truthy(l3246)) {
        at = 4460; l3258 = field(rt, l3249, "points");
        at = 4461; l3257 = l3258;
      } else {
        at = 4462; l3259 = K[1106];
        at = 4463; l3257 = l3259;
      }
      at = 4464; l3260 = K[1107];
      at = 4465; l3261 = rt.array_nth(l3257, l3260);
      at = 4466; l3256 = l3261;
    } else {
      at = 4467; l3262 = K[1108];
      at = 4468; l3256 = l3262;
    }
    at = 4469; if (rt.truthy(l3246)) {
      at = 4470; l3264 = field(rt, l3249, "width");
      at = 4471; l3263 = l3264;
    } else {
      at = 4472; l3265 = K[1109];
      at = 4473; l3263 = l3265;
    }
    at = 4474; l3266 = K[1110];
    at = 4475; l3267 = rt.int_div(l3263, l3266);
    at = 4476; l3268 = K[1111];
    at = 4477; l3269 = rt.max(l3268, l3267);
    at = 4478; l3270 = K[1112];
    at = 4479; l3271 = rt.gt(l3253, l3270);
    at = 4480; if (rt.truthy(l3271)) {
      at = 4481; if (rt.truthy(l3246)) {
        at = 4482; l3274 = field(rt, l3249, "color");
        at = 4483; l3273 = l3274;
      } else {
        at = 4484; l3275 = K[1113];
        at = 4485; l3273 = l3275;
      }
      at = 4486; l3276 = K[1114];
      at = 4487; l3277 = rt.sequence_get(l3276, l3273);
      at = 4488; l3272 = l3277;
    } else {
      at = 4489; l3278 = K[1115];
      at = 4490; l3272 = l3278;
    }
    at = 4491; markSet(rt, false, "circle", {at: l3256, radius: l3269, fill: l3272});
    at = 4492; if (rt.truthy(l3246)) {
      at = 4493; l3280 = field(rt, l3249, "points");
      at = 4494; l3279 = l3280;
    } else {
      at = 4495; l3281 = K[1116];
      at = 4496; l3279 = l3281;
    }
    at = 4497; l3282 = K[1117];
    at = 4498; l3283 = rt.sub(l3253, l3282);
    at = 4499; l3284 = K[1118];
    at = 4500; l3285 = rt.max(l3284, l3283);
    at = 4501; l3286 = K[1119];
    at = 4502; l3287 = rt.array_slice(l3279, l3286, l3285);
    at = 4503; if (rt.truthy(l3246)) {
      at = 4504; l3289 = field(rt, l3249, "points");
      at = 4505; l3288 = l3289;
    } else {
      at = 4506; l3290 = K[1120];
      at = 4507; l3288 = l3290;
    }
    at = 4508; l3291 = K[1121];
    at = 4509; l3292 = rt.gt(l3253, l3291);
    at = 4510; if (rt.truthy(l3292)) {
      at = 4511; l3294 = K[1122];
      at = 4512; l3293 = l3294;
    } else {
      at = 4513; l3295 = K[1123];
      at = 4514; l3293 = l3295;
    }
    at = 4515; l3296 = K[1124];
    at = 4516; l3297 = rt.sub(l3253, l3296);
    at = 4517; l3298 = K[1125];
    at = 4518; l3299 = rt.max(l3298, l3297);
    at = 4519; l3300 = rt.array_slice(l3288, l3293, l3299);
    at = 4520; if (rt.truthy(l3246)) {
      at = 4521; l3302 = field(rt, l3249, "color");
      at = 4522; l3301 = l3302;
    } else {
      at = 4523; l3303 = K[1126];
      at = 4524; l3301 = l3303;
    }
    at = 4525; l3304 = K[1127];
    at = 4526; l3305 = rt.sequence_get(l3304, l3301);
    at = 4527; if (rt.truthy(l3246)) {
      at = 4528; l3307 = field(rt, l3249, "width");
      at = 4529; l3306 = l3307;
    } else {
      at = 4530; l3308 = K[1128];
      at = 4531; l3306 = l3308;
    }
    at = 4532; markSet(rt, true, "line", {from: l3287, to: l3300, color: l3305, width: l3306});
    at = 4533; l3309 = field(rt, l15, "strokes");
    at = 4534; l3310 = rt.sequence_length(l3309);
    at = 4535; l3311 = K[1129];
    at = 4536; l3312 = rt.lt(l3311, l3310);
    at = 4537; if (rt.truthy(l3312)) {
      at = 4538; l3313 = field(rt, l15, "strokes");
      at = 4539; l3314 = K[1130];
      at = 4540; l3315 = rt.sequence_get(l3313, l3314);
    }
    at = 4541; if (rt.truthy(l3312)) {
      at = 4542; l3317 = field(rt, l3315, "points");
      at = 4543; l3316 = l3317;
    } else {
      at = 4544; l3318 = K[1131];
      at = 4545; l3316 = l3318;
    }
    at = 4546; l3319 = rt.array_count(l3316);
    at = 4547; l3320 = K[1132];
    at = 4548; l3321 = rt.gt(l3319, l3320);
    at = 4549; if (rt.truthy(l3321)) {
      at = 4550; if (rt.truthy(l3312)) {
        at = 4551; l3324 = field(rt, l3315, "points");
        at = 4552; l3323 = l3324;
      } else {
        at = 4553; l3325 = K[1133];
        at = 4554; l3323 = l3325;
      }
      at = 4555; l3326 = K[1134];
      at = 4556; l3327 = rt.array_nth(l3323, l3326);
      at = 4557; l3322 = l3327;
    } else {
      at = 4558; l3328 = K[1135];
      at = 4559; l3322 = l3328;
    }
    at = 4560; if (rt.truthy(l3312)) {
      at = 4561; l3330 = field(rt, l3315, "width");
      at = 4562; l3329 = l3330;
    } else {
      at = 4563; l3331 = K[1136];
      at = 4564; l3329 = l3331;
    }
    at = 4565; l3332 = K[1137];
    at = 4566; l3333 = rt.int_div(l3329, l3332);
    at = 4567; l3334 = K[1138];
    at = 4568; l3335 = rt.max(l3334, l3333);
    at = 4569; l3336 = K[1139];
    at = 4570; l3337 = rt.gt(l3319, l3336);
    at = 4571; if (rt.truthy(l3337)) {
      at = 4572; if (rt.truthy(l3312)) {
        at = 4573; l3340 = field(rt, l3315, "color");
        at = 4574; l3339 = l3340;
      } else {
        at = 4575; l3341 = K[1140];
        at = 4576; l3339 = l3341;
      }
      at = 4577; l3342 = K[1141];
      at = 4578; l3343 = rt.sequence_get(l3342, l3339);
      at = 4579; l3338 = l3343;
    } else {
      at = 4580; l3344 = K[1142];
      at = 4581; l3338 = l3344;
    }
    at = 4582; markSet(rt, false, "circle", {at: l3322, radius: l3335, fill: l3338});
    at = 4583; if (rt.truthy(l3312)) {
      at = 4584; l3346 = field(rt, l3315, "points");
      at = 4585; l3345 = l3346;
    } else {
      at = 4586; l3347 = K[1143];
      at = 4587; l3345 = l3347;
    }
    at = 4588; l3348 = K[1144];
    at = 4589; l3349 = rt.sub(l3319, l3348);
    at = 4590; l3350 = K[1145];
    at = 4591; l3351 = rt.max(l3350, l3349);
    at = 4592; l3352 = K[1146];
    at = 4593; l3353 = rt.array_slice(l3345, l3352, l3351);
    at = 4594; if (rt.truthy(l3312)) {
      at = 4595; l3355 = field(rt, l3315, "points");
      at = 4596; l3354 = l3355;
    } else {
      at = 4597; l3356 = K[1147];
      at = 4598; l3354 = l3356;
    }
    at = 4599; l3357 = K[1148];
    at = 4600; l3358 = rt.gt(l3319, l3357);
    at = 4601; if (rt.truthy(l3358)) {
      at = 4602; l3360 = K[1149];
      at = 4603; l3359 = l3360;
    } else {
      at = 4604; l3361 = K[1150];
      at = 4605; l3359 = l3361;
    }
    at = 4606; l3362 = K[1151];
    at = 4607; l3363 = rt.sub(l3319, l3362);
    at = 4608; l3364 = K[1152];
    at = 4609; l3365 = rt.max(l3364, l3363);
    at = 4610; l3366 = rt.array_slice(l3354, l3359, l3365);
    at = 4611; if (rt.truthy(l3312)) {
      at = 4612; l3368 = field(rt, l3315, "color");
      at = 4613; l3367 = l3368;
    } else {
      at = 4614; l3369 = K[1153];
      at = 4615; l3367 = l3369;
    }
    at = 4616; l3370 = K[1154];
    at = 4617; l3371 = rt.sequence_get(l3370, l3367);
    at = 4618; if (rt.truthy(l3312)) {
      at = 4619; l3373 = field(rt, l3315, "width");
      at = 4620; l3372 = l3373;
    } else {
      at = 4621; l3374 = K[1155];
      at = 4622; l3372 = l3374;
    }
    at = 4623; markSet(rt, true, "line", {from: l3353, to: l3366, color: l3371, width: l3372});
    at = 4624; l3375 = field(rt, l15, "strokes");
    at = 4625; l3376 = rt.sequence_length(l3375);
    at = 4626; l3377 = K[1156];
    at = 4627; l3378 = rt.lt(l3377, l3376);
    at = 4628; if (rt.truthy(l3378)) {
      at = 4629; l3379 = field(rt, l15, "strokes");
      at = 4630; l3380 = K[1157];
      at = 4631; l3381 = rt.sequence_get(l3379, l3380);
    }
    at = 4632; if (rt.truthy(l3378)) {
      at = 4633; l3383 = field(rt, l3381, "points");
      at = 4634; l3382 = l3383;
    } else {
      at = 4635; l3384 = K[1158];
      at = 4636; l3382 = l3384;
    }
    at = 4637; l3385 = rt.array_count(l3382);
    at = 4638; l3386 = K[1159];
    at = 4639; l3387 = rt.gt(l3385, l3386);
    at = 4640; if (rt.truthy(l3387)) {
      at = 4641; if (rt.truthy(l3378)) {
        at = 4642; l3390 = field(rt, l3381, "points");
        at = 4643; l3389 = l3390;
      } else {
        at = 4644; l3391 = K[1160];
        at = 4645; l3389 = l3391;
      }
      at = 4646; l3392 = K[1161];
      at = 4647; l3393 = rt.array_nth(l3389, l3392);
      at = 4648; l3388 = l3393;
    } else {
      at = 4649; l3394 = K[1162];
      at = 4650; l3388 = l3394;
    }
    at = 4651; if (rt.truthy(l3378)) {
      at = 4652; l3396 = field(rt, l3381, "width");
      at = 4653; l3395 = l3396;
    } else {
      at = 4654; l3397 = K[1163];
      at = 4655; l3395 = l3397;
    }
    at = 4656; l3398 = K[1164];
    at = 4657; l3399 = rt.int_div(l3395, l3398);
    at = 4658; l3400 = K[1165];
    at = 4659; l3401 = rt.max(l3400, l3399);
    at = 4660; l3402 = K[1166];
    at = 4661; l3403 = rt.gt(l3385, l3402);
    at = 4662; if (rt.truthy(l3403)) {
      at = 4663; if (rt.truthy(l3378)) {
        at = 4664; l3406 = field(rt, l3381, "color");
        at = 4665; l3405 = l3406;
      } else {
        at = 4666; l3407 = K[1167];
        at = 4667; l3405 = l3407;
      }
      at = 4668; l3408 = K[1168];
      at = 4669; l3409 = rt.sequence_get(l3408, l3405);
      at = 4670; l3404 = l3409;
    } else {
      at = 4671; l3410 = K[1169];
      at = 4672; l3404 = l3410;
    }
    at = 4673; markSet(rt, false, "circle", {at: l3388, radius: l3401, fill: l3404});
    at = 4674; if (rt.truthy(l3378)) {
      at = 4675; l3412 = field(rt, l3381, "points");
      at = 4676; l3411 = l3412;
    } else {
      at = 4677; l3413 = K[1170];
      at = 4678; l3411 = l3413;
    }
    at = 4679; l3414 = K[1171];
    at = 4680; l3415 = rt.sub(l3385, l3414);
    at = 4681; l3416 = K[1172];
    at = 4682; l3417 = rt.max(l3416, l3415);
    at = 4683; l3418 = K[1173];
    at = 4684; l3419 = rt.array_slice(l3411, l3418, l3417);
    at = 4685; if (rt.truthy(l3378)) {
      at = 4686; l3421 = field(rt, l3381, "points");
      at = 4687; l3420 = l3421;
    } else {
      at = 4688; l3422 = K[1174];
      at = 4689; l3420 = l3422;
    }
    at = 4690; l3423 = K[1175];
    at = 4691; l3424 = rt.gt(l3385, l3423);
    at = 4692; if (rt.truthy(l3424)) {
      at = 4693; l3426 = K[1176];
      at = 4694; l3425 = l3426;
    } else {
      at = 4695; l3427 = K[1177];
      at = 4696; l3425 = l3427;
    }
    at = 4697; l3428 = K[1178];
    at = 4698; l3429 = rt.sub(l3385, l3428);
    at = 4699; l3430 = K[1179];
    at = 4700; l3431 = rt.max(l3430, l3429);
    at = 4701; l3432 = rt.array_slice(l3420, l3425, l3431);
    at = 4702; if (rt.truthy(l3378)) {
      at = 4703; l3434 = field(rt, l3381, "color");
      at = 4704; l3433 = l3434;
    } else {
      at = 4705; l3435 = K[1180];
      at = 4706; l3433 = l3435;
    }
    at = 4707; l3436 = K[1181];
    at = 4708; l3437 = rt.sequence_get(l3436, l3433);
    at = 4709; if (rt.truthy(l3378)) {
      at = 4710; l3439 = field(rt, l3381, "width");
      at = 4711; l3438 = l3439;
    } else {
      at = 4712; l3440 = K[1182];
      at = 4713; l3438 = l3440;
    }
    at = 4714; markSet(rt, true, "line", {from: l3419, to: l3432, color: l3437, width: l3438});
    at = 4715; l3441 = field(rt, l15, "strokes");
    at = 4716; l3442 = rt.sequence_length(l3441);
    at = 4717; l3443 = K[1183];
    at = 4718; l3444 = rt.lt(l3443, l3442);
    at = 4719; if (rt.truthy(l3444)) {
      at = 4720; l3445 = field(rt, l15, "strokes");
      at = 4721; l3446 = K[1184];
      at = 4722; l3447 = rt.sequence_get(l3445, l3446);
    }
    at = 4723; if (rt.truthy(l3444)) {
      at = 4724; l3449 = field(rt, l3447, "points");
      at = 4725; l3448 = l3449;
    } else {
      at = 4726; l3450 = K[1185];
      at = 4727; l3448 = l3450;
    }
    at = 4728; l3451 = rt.array_count(l3448);
    at = 4729; l3452 = K[1186];
    at = 4730; l3453 = rt.gt(l3451, l3452);
    at = 4731; if (rt.truthy(l3453)) {
      at = 4732; if (rt.truthy(l3444)) {
        at = 4733; l3456 = field(rt, l3447, "points");
        at = 4734; l3455 = l3456;
      } else {
        at = 4735; l3457 = K[1187];
        at = 4736; l3455 = l3457;
      }
      at = 4737; l3458 = K[1188];
      at = 4738; l3459 = rt.array_nth(l3455, l3458);
      at = 4739; l3454 = l3459;
    } else {
      at = 4740; l3460 = K[1189];
      at = 4741; l3454 = l3460;
    }
    at = 4742; if (rt.truthy(l3444)) {
      at = 4743; l3462 = field(rt, l3447, "width");
      at = 4744; l3461 = l3462;
    } else {
      at = 4745; l3463 = K[1190];
      at = 4746; l3461 = l3463;
    }
    at = 4747; l3464 = K[1191];
    at = 4748; l3465 = rt.int_div(l3461, l3464);
    at = 4749; l3466 = K[1192];
    at = 4750; l3467 = rt.max(l3466, l3465);
    at = 4751; l3468 = K[1193];
    at = 4752; l3469 = rt.gt(l3451, l3468);
    at = 4753; if (rt.truthy(l3469)) {
      at = 4754; if (rt.truthy(l3444)) {
        at = 4755; l3472 = field(rt, l3447, "color");
        at = 4756; l3471 = l3472;
      } else {
        at = 4757; l3473 = K[1194];
        at = 4758; l3471 = l3473;
      }
      at = 4759; l3474 = K[1195];
      at = 4760; l3475 = rt.sequence_get(l3474, l3471);
      at = 4761; l3470 = l3475;
    } else {
      at = 4762; l3476 = K[1196];
      at = 4763; l3470 = l3476;
    }
    at = 4764; markSet(rt, false, "circle", {at: l3454, radius: l3467, fill: l3470});
    at = 4765; if (rt.truthy(l3444)) {
      at = 4766; l3478 = field(rt, l3447, "points");
      at = 4767; l3477 = l3478;
    } else {
      at = 4768; l3479 = K[1197];
      at = 4769; l3477 = l3479;
    }
    at = 4770; l3480 = K[1198];
    at = 4771; l3481 = rt.sub(l3451, l3480);
    at = 4772; l3482 = K[1199];
    at = 4773; l3483 = rt.max(l3482, l3481);
    at = 4774; l3484 = K[1200];
    at = 4775; l3485 = rt.array_slice(l3477, l3484, l3483);
    at = 4776; if (rt.truthy(l3444)) {
      at = 4777; l3487 = field(rt, l3447, "points");
      at = 4778; l3486 = l3487;
    } else {
      at = 4779; l3488 = K[1201];
      at = 4780; l3486 = l3488;
    }
    at = 4781; l3489 = K[1202];
    at = 4782; l3490 = rt.gt(l3451, l3489);
    at = 4783; if (rt.truthy(l3490)) {
      at = 4784; l3492 = K[1203];
      at = 4785; l3491 = l3492;
    } else {
      at = 4786; l3493 = K[1204];
      at = 4787; l3491 = l3493;
    }
    at = 4788; l3494 = K[1205];
    at = 4789; l3495 = rt.sub(l3451, l3494);
    at = 4790; l3496 = K[1206];
    at = 4791; l3497 = rt.max(l3496, l3495);
    at = 4792; l3498 = rt.array_slice(l3486, l3491, l3497);
    at = 4793; if (rt.truthy(l3444)) {
      at = 4794; l3500 = field(rt, l3447, "color");
      at = 4795; l3499 = l3500;
    } else {
      at = 4796; l3501 = K[1207];
      at = 4797; l3499 = l3501;
    }
    at = 4798; l3502 = K[1208];
    at = 4799; l3503 = rt.sequence_get(l3502, l3499);
    at = 4800; if (rt.truthy(l3444)) {
      at = 4801; l3505 = field(rt, l3447, "width");
      at = 4802; l3504 = l3505;
    } else {
      at = 4803; l3506 = K[1209];
      at = 4804; l3504 = l3506;
    }
    at = 4805; markSet(rt, true, "line", {from: l3485, to: l3498, color: l3503, width: l3504});
    at = 4806; l3507 = field(rt, l15, "strokes");
    at = 4807; l3508 = rt.sequence_length(l3507);
    at = 4808; l3509 = K[1210];
    at = 4809; l3510 = rt.lt(l3509, l3508);
    at = 4810; if (rt.truthy(l3510)) {
      at = 4811; l3511 = field(rt, l15, "strokes");
      at = 4812; l3512 = K[1211];
      at = 4813; l3513 = rt.sequence_get(l3511, l3512);
    }
    at = 4814; if (rt.truthy(l3510)) {
      at = 4815; l3515 = field(rt, l3513, "points");
      at = 4816; l3514 = l3515;
    } else {
      at = 4817; l3516 = K[1212];
      at = 4818; l3514 = l3516;
    }
    at = 4819; l3517 = rt.array_count(l3514);
    at = 4820; l3518 = K[1213];
    at = 4821; l3519 = rt.gt(l3517, l3518);
    at = 4822; if (rt.truthy(l3519)) {
      at = 4823; if (rt.truthy(l3510)) {
        at = 4824; l3522 = field(rt, l3513, "points");
        at = 4825; l3521 = l3522;
      } else {
        at = 4826; l3523 = K[1214];
        at = 4827; l3521 = l3523;
      }
      at = 4828; l3524 = K[1215];
      at = 4829; l3525 = rt.array_nth(l3521, l3524);
      at = 4830; l3520 = l3525;
    } else {
      at = 4831; l3526 = K[1216];
      at = 4832; l3520 = l3526;
    }
    at = 4833; if (rt.truthy(l3510)) {
      at = 4834; l3528 = field(rt, l3513, "width");
      at = 4835; l3527 = l3528;
    } else {
      at = 4836; l3529 = K[1217];
      at = 4837; l3527 = l3529;
    }
    at = 4838; l3530 = K[1218];
    at = 4839; l3531 = rt.int_div(l3527, l3530);
    at = 4840; l3532 = K[1219];
    at = 4841; l3533 = rt.max(l3532, l3531);
    at = 4842; l3534 = K[1220];
    at = 4843; l3535 = rt.gt(l3517, l3534);
    at = 4844; if (rt.truthy(l3535)) {
      at = 4845; if (rt.truthy(l3510)) {
        at = 4846; l3538 = field(rt, l3513, "color");
        at = 4847; l3537 = l3538;
      } else {
        at = 4848; l3539 = K[1221];
        at = 4849; l3537 = l3539;
      }
      at = 4850; l3540 = K[1222];
      at = 4851; l3541 = rt.sequence_get(l3540, l3537);
      at = 4852; l3536 = l3541;
    } else {
      at = 4853; l3542 = K[1223];
      at = 4854; l3536 = l3542;
    }
    at = 4855; markSet(rt, false, "circle", {at: l3520, radius: l3533, fill: l3536});
    at = 4856; if (rt.truthy(l3510)) {
      at = 4857; l3544 = field(rt, l3513, "points");
      at = 4858; l3543 = l3544;
    } else {
      at = 4859; l3545 = K[1224];
      at = 4860; l3543 = l3545;
    }
    at = 4861; l3546 = K[1225];
    at = 4862; l3547 = rt.sub(l3517, l3546);
    at = 4863; l3548 = K[1226];
    at = 4864; l3549 = rt.max(l3548, l3547);
    at = 4865; l3550 = K[1227];
    at = 4866; l3551 = rt.array_slice(l3543, l3550, l3549);
    at = 4867; if (rt.truthy(l3510)) {
      at = 4868; l3553 = field(rt, l3513, "points");
      at = 4869; l3552 = l3553;
    } else {
      at = 4870; l3554 = K[1228];
      at = 4871; l3552 = l3554;
    }
    at = 4872; l3555 = K[1229];
    at = 4873; l3556 = rt.gt(l3517, l3555);
    at = 4874; if (rt.truthy(l3556)) {
      at = 4875; l3558 = K[1230];
      at = 4876; l3557 = l3558;
    } else {
      at = 4877; l3559 = K[1231];
      at = 4878; l3557 = l3559;
    }
    at = 4879; l3560 = K[1232];
    at = 4880; l3561 = rt.sub(l3517, l3560);
    at = 4881; l3562 = K[1233];
    at = 4882; l3563 = rt.max(l3562, l3561);
    at = 4883; l3564 = rt.array_slice(l3552, l3557, l3563);
    at = 4884; if (rt.truthy(l3510)) {
      at = 4885; l3566 = field(rt, l3513, "color");
      at = 4886; l3565 = l3566;
    } else {
      at = 4887; l3567 = K[1234];
      at = 4888; l3565 = l3567;
    }
    at = 4889; l3568 = K[1235];
    at = 4890; l3569 = rt.sequence_get(l3568, l3565);
    at = 4891; if (rt.truthy(l3510)) {
      at = 4892; l3571 = field(rt, l3513, "width");
      at = 4893; l3570 = l3571;
    } else {
      at = 4894; l3572 = K[1236];
      at = 4895; l3570 = l3572;
    }
    at = 4896; markSet(rt, true, "line", {from: l3551, to: l3564, color: l3569, width: l3570});
    at = 4897; l3573 = field(rt, l15, "strokes");
    at = 4898; l3574 = rt.sequence_length(l3573);
    at = 4899; l3575 = K[1237];
    at = 4900; l3576 = rt.lt(l3575, l3574);
    at = 4901; if (rt.truthy(l3576)) {
      at = 4902; l3577 = field(rt, l15, "strokes");
      at = 4903; l3578 = K[1238];
      at = 4904; l3579 = rt.sequence_get(l3577, l3578);
    }
    at = 4905; if (rt.truthy(l3576)) {
      at = 4906; l3581 = field(rt, l3579, "points");
      at = 4907; l3580 = l3581;
    } else {
      at = 4908; l3582 = K[1239];
      at = 4909; l3580 = l3582;
    }
    at = 4910; l3583 = rt.array_count(l3580);
    at = 4911; l3584 = K[1240];
    at = 4912; l3585 = rt.gt(l3583, l3584);
    at = 4913; if (rt.truthy(l3585)) {
      at = 4914; if (rt.truthy(l3576)) {
        at = 4915; l3588 = field(rt, l3579, "points");
        at = 4916; l3587 = l3588;
      } else {
        at = 4917; l3589 = K[1241];
        at = 4918; l3587 = l3589;
      }
      at = 4919; l3590 = K[1242];
      at = 4920; l3591 = rt.array_nth(l3587, l3590);
      at = 4921; l3586 = l3591;
    } else {
      at = 4922; l3592 = K[1243];
      at = 4923; l3586 = l3592;
    }
    at = 4924; if (rt.truthy(l3576)) {
      at = 4925; l3594 = field(rt, l3579, "width");
      at = 4926; l3593 = l3594;
    } else {
      at = 4927; l3595 = K[1244];
      at = 4928; l3593 = l3595;
    }
    at = 4929; l3596 = K[1245];
    at = 4930; l3597 = rt.int_div(l3593, l3596);
    at = 4931; l3598 = K[1246];
    at = 4932; l3599 = rt.max(l3598, l3597);
    at = 4933; l3600 = K[1247];
    at = 4934; l3601 = rt.gt(l3583, l3600);
    at = 4935; if (rt.truthy(l3601)) {
      at = 4936; if (rt.truthy(l3576)) {
        at = 4937; l3604 = field(rt, l3579, "color");
        at = 4938; l3603 = l3604;
      } else {
        at = 4939; l3605 = K[1248];
        at = 4940; l3603 = l3605;
      }
      at = 4941; l3606 = K[1249];
      at = 4942; l3607 = rt.sequence_get(l3606, l3603);
      at = 4943; l3602 = l3607;
    } else {
      at = 4944; l3608 = K[1250];
      at = 4945; l3602 = l3608;
    }
    at = 4946; markSet(rt, false, "circle", {at: l3586, radius: l3599, fill: l3602});
    at = 4947; if (rt.truthy(l3576)) {
      at = 4948; l3610 = field(rt, l3579, "points");
      at = 4949; l3609 = l3610;
    } else {
      at = 4950; l3611 = K[1251];
      at = 4951; l3609 = l3611;
    }
    at = 4952; l3612 = K[1252];
    at = 4953; l3613 = rt.sub(l3583, l3612);
    at = 4954; l3614 = K[1253];
    at = 4955; l3615 = rt.max(l3614, l3613);
    at = 4956; l3616 = K[1254];
    at = 4957; l3617 = rt.array_slice(l3609, l3616, l3615);
    at = 4958; if (rt.truthy(l3576)) {
      at = 4959; l3619 = field(rt, l3579, "points");
      at = 4960; l3618 = l3619;
    } else {
      at = 4961; l3620 = K[1255];
      at = 4962; l3618 = l3620;
    }
    at = 4963; l3621 = K[1256];
    at = 4964; l3622 = rt.gt(l3583, l3621);
    at = 4965; if (rt.truthy(l3622)) {
      at = 4966; l3624 = K[1257];
      at = 4967; l3623 = l3624;
    } else {
      at = 4968; l3625 = K[1258];
      at = 4969; l3623 = l3625;
    }
    at = 4970; l3626 = K[1259];
    at = 4971; l3627 = rt.sub(l3583, l3626);
    at = 4972; l3628 = K[1260];
    at = 4973; l3629 = rt.max(l3628, l3627);
    at = 4974; l3630 = rt.array_slice(l3618, l3623, l3629);
    at = 4975; if (rt.truthy(l3576)) {
      at = 4976; l3632 = field(rt, l3579, "color");
      at = 4977; l3631 = l3632;
    } else {
      at = 4978; l3633 = K[1261];
      at = 4979; l3631 = l3633;
    }
    at = 4980; l3634 = K[1262];
    at = 4981; l3635 = rt.sequence_get(l3634, l3631);
    at = 4982; if (rt.truthy(l3576)) {
      at = 4983; l3637 = field(rt, l3579, "width");
      at = 4984; l3636 = l3637;
    } else {
      at = 4985; l3638 = K[1263];
      at = 4986; l3636 = l3638;
    }
    at = 4987; markSet(rt, true, "line", {from: l3617, to: l3630, color: l3635, width: l3636});
    at = 4988; l3639 = field(rt, l15, "strokes");
    at = 4989; l3640 = rt.sequence_length(l3639);
    at = 4990; l3641 = K[1264];
    at = 4991; l3642 = rt.lt(l3641, l3640);
    at = 4992; if (rt.truthy(l3642)) {
      at = 4993; l3643 = field(rt, l15, "strokes");
      at = 4994; l3644 = K[1265];
      at = 4995; l3645 = rt.sequence_get(l3643, l3644);
    }
    at = 4996; if (rt.truthy(l3642)) {
      at = 4997; l3647 = field(rt, l3645, "points");
      at = 4998; l3646 = l3647;
    } else {
      at = 4999; l3648 = K[1266];
      at = 5000; l3646 = l3648;
    }
    at = 5001; l3649 = rt.array_count(l3646);
    at = 5002; l3650 = K[1267];
    at = 5003; l3651 = rt.gt(l3649, l3650);
    at = 5004; if (rt.truthy(l3651)) {
      at = 5005; if (rt.truthy(l3642)) {
        at = 5006; l3654 = field(rt, l3645, "points");
        at = 5007; l3653 = l3654;
      } else {
        at = 5008; l3655 = K[1268];
        at = 5009; l3653 = l3655;
      }
      at = 5010; l3656 = K[1269];
      at = 5011; l3657 = rt.array_nth(l3653, l3656);
      at = 5012; l3652 = l3657;
    } else {
      at = 5013; l3658 = K[1270];
      at = 5014; l3652 = l3658;
    }
    at = 5015; if (rt.truthy(l3642)) {
      at = 5016; l3660 = field(rt, l3645, "width");
      at = 5017; l3659 = l3660;
    } else {
      at = 5018; l3661 = K[1271];
      at = 5019; l3659 = l3661;
    }
    at = 5020; l3662 = K[1272];
    at = 5021; l3663 = rt.int_div(l3659, l3662);
    at = 5022; l3664 = K[1273];
    at = 5023; l3665 = rt.max(l3664, l3663);
    at = 5024; l3666 = K[1274];
    at = 5025; l3667 = rt.gt(l3649, l3666);
    at = 5026; if (rt.truthy(l3667)) {
      at = 5027; if (rt.truthy(l3642)) {
        at = 5028; l3670 = field(rt, l3645, "color");
        at = 5029; l3669 = l3670;
      } else {
        at = 5030; l3671 = K[1275];
        at = 5031; l3669 = l3671;
      }
      at = 5032; l3672 = K[1276];
      at = 5033; l3673 = rt.sequence_get(l3672, l3669);
      at = 5034; l3668 = l3673;
    } else {
      at = 5035; l3674 = K[1277];
      at = 5036; l3668 = l3674;
    }
    at = 5037; markSet(rt, false, "circle", {at: l3652, radius: l3665, fill: l3668});
    at = 5038; if (rt.truthy(l3642)) {
      at = 5039; l3676 = field(rt, l3645, "points");
      at = 5040; l3675 = l3676;
    } else {
      at = 5041; l3677 = K[1278];
      at = 5042; l3675 = l3677;
    }
    at = 5043; l3678 = K[1279];
    at = 5044; l3679 = rt.sub(l3649, l3678);
    at = 5045; l3680 = K[1280];
    at = 5046; l3681 = rt.max(l3680, l3679);
    at = 5047; l3682 = K[1281];
    at = 5048; l3683 = rt.array_slice(l3675, l3682, l3681);
    at = 5049; if (rt.truthy(l3642)) {
      at = 5050; l3685 = field(rt, l3645, "points");
      at = 5051; l3684 = l3685;
    } else {
      at = 5052; l3686 = K[1282];
      at = 5053; l3684 = l3686;
    }
    at = 5054; l3687 = K[1283];
    at = 5055; l3688 = rt.gt(l3649, l3687);
    at = 5056; if (rt.truthy(l3688)) {
      at = 5057; l3690 = K[1284];
      at = 5058; l3689 = l3690;
    } else {
      at = 5059; l3691 = K[1285];
      at = 5060; l3689 = l3691;
    }
    at = 5061; l3692 = K[1286];
    at = 5062; l3693 = rt.sub(l3649, l3692);
    at = 5063; l3694 = K[1287];
    at = 5064; l3695 = rt.max(l3694, l3693);
    at = 5065; l3696 = rt.array_slice(l3684, l3689, l3695);
    at = 5066; if (rt.truthy(l3642)) {
      at = 5067; l3698 = field(rt, l3645, "color");
      at = 5068; l3697 = l3698;
    } else {
      at = 5069; l3699 = K[1288];
      at = 5070; l3697 = l3699;
    }
    at = 5071; l3700 = K[1289];
    at = 5072; l3701 = rt.sequence_get(l3700, l3697);
    at = 5073; if (rt.truthy(l3642)) {
      at = 5074; l3703 = field(rt, l3645, "width");
      at = 5075; l3702 = l3703;
    } else {
      at = 5076; l3704 = K[1290];
      at = 5077; l3702 = l3704;
    }
    at = 5078; markSet(rt, true, "line", {from: l3683, to: l3696, color: l3701, width: l3702});
    at = 5079; l3705 = field(rt, l15, "strokes");
    at = 5080; l3706 = rt.sequence_length(l3705);
    at = 5081; l3707 = K[1291];
    at = 5082; l3708 = rt.lt(l3707, l3706);
    at = 5083; if (rt.truthy(l3708)) {
      at = 5084; l3709 = field(rt, l15, "strokes");
      at = 5085; l3710 = K[1292];
      at = 5086; l3711 = rt.sequence_get(l3709, l3710);
    }
    at = 5087; if (rt.truthy(l3708)) {
      at = 5088; l3713 = field(rt, l3711, "points");
      at = 5089; l3712 = l3713;
    } else {
      at = 5090; l3714 = K[1293];
      at = 5091; l3712 = l3714;
    }
    at = 5092; l3715 = rt.array_count(l3712);
    at = 5093; l3716 = K[1294];
    at = 5094; l3717 = rt.gt(l3715, l3716);
    at = 5095; if (rt.truthy(l3717)) {
      at = 5096; if (rt.truthy(l3708)) {
        at = 5097; l3720 = field(rt, l3711, "points");
        at = 5098; l3719 = l3720;
      } else {
        at = 5099; l3721 = K[1295];
        at = 5100; l3719 = l3721;
      }
      at = 5101; l3722 = K[1296];
      at = 5102; l3723 = rt.array_nth(l3719, l3722);
      at = 5103; l3718 = l3723;
    } else {
      at = 5104; l3724 = K[1297];
      at = 5105; l3718 = l3724;
    }
    at = 5106; if (rt.truthy(l3708)) {
      at = 5107; l3726 = field(rt, l3711, "width");
      at = 5108; l3725 = l3726;
    } else {
      at = 5109; l3727 = K[1298];
      at = 5110; l3725 = l3727;
    }
    at = 5111; l3728 = K[1299];
    at = 5112; l3729 = rt.int_div(l3725, l3728);
    at = 5113; l3730 = K[1300];
    at = 5114; l3731 = rt.max(l3730, l3729);
    at = 5115; l3732 = K[1301];
    at = 5116; l3733 = rt.gt(l3715, l3732);
    at = 5117; if (rt.truthy(l3733)) {
      at = 5118; if (rt.truthy(l3708)) {
        at = 5119; l3736 = field(rt, l3711, "color");
        at = 5120; l3735 = l3736;
      } else {
        at = 5121; l3737 = K[1302];
        at = 5122; l3735 = l3737;
      }
      at = 5123; l3738 = K[1303];
      at = 5124; l3739 = rt.sequence_get(l3738, l3735);
      at = 5125; l3734 = l3739;
    } else {
      at = 5126; l3740 = K[1304];
      at = 5127; l3734 = l3740;
    }
    at = 5128; markSet(rt, false, "circle", {at: l3718, radius: l3731, fill: l3734});
    at = 5129; if (rt.truthy(l3708)) {
      at = 5130; l3742 = field(rt, l3711, "points");
      at = 5131; l3741 = l3742;
    } else {
      at = 5132; l3743 = K[1305];
      at = 5133; l3741 = l3743;
    }
    at = 5134; l3744 = K[1306];
    at = 5135; l3745 = rt.sub(l3715, l3744);
    at = 5136; l3746 = K[1307];
    at = 5137; l3747 = rt.max(l3746, l3745);
    at = 5138; l3748 = K[1308];
    at = 5139; l3749 = rt.array_slice(l3741, l3748, l3747);
    at = 5140; if (rt.truthy(l3708)) {
      at = 5141; l3751 = field(rt, l3711, "points");
      at = 5142; l3750 = l3751;
    } else {
      at = 5143; l3752 = K[1309];
      at = 5144; l3750 = l3752;
    }
    at = 5145; l3753 = K[1310];
    at = 5146; l3754 = rt.gt(l3715, l3753);
    at = 5147; if (rt.truthy(l3754)) {
      at = 5148; l3756 = K[1311];
      at = 5149; l3755 = l3756;
    } else {
      at = 5150; l3757 = K[1312];
      at = 5151; l3755 = l3757;
    }
    at = 5152; l3758 = K[1313];
    at = 5153; l3759 = rt.sub(l3715, l3758);
    at = 5154; l3760 = K[1314];
    at = 5155; l3761 = rt.max(l3760, l3759);
    at = 5156; l3762 = rt.array_slice(l3750, l3755, l3761);
    at = 5157; if (rt.truthy(l3708)) {
      at = 5158; l3764 = field(rt, l3711, "color");
      at = 5159; l3763 = l3764;
    } else {
      at = 5160; l3765 = K[1315];
      at = 5161; l3763 = l3765;
    }
    at = 5162; l3766 = K[1316];
    at = 5163; l3767 = rt.sequence_get(l3766, l3763);
    at = 5164; if (rt.truthy(l3708)) {
      at = 5165; l3769 = field(rt, l3711, "width");
      at = 5166; l3768 = l3769;
    } else {
      at = 5167; l3770 = K[1317];
      at = 5168; l3768 = l3770;
    }
    at = 5169; markSet(rt, true, "line", {from: l3749, to: l3762, color: l3767, width: l3768});
    at = 5170; l3771 = field(rt, l15, "strokes");
    at = 5171; l3772 = rt.sequence_length(l3771);
    at = 5172; l3773 = K[1318];
    at = 5173; l3774 = rt.lt(l3773, l3772);
    at = 5174; if (rt.truthy(l3774)) {
      at = 5175; l3775 = field(rt, l15, "strokes");
      at = 5176; l3776 = K[1319];
      at = 5177; l3777 = rt.sequence_get(l3775, l3776);
    }
    at = 5178; if (rt.truthy(l3774)) {
      at = 5179; l3779 = field(rt, l3777, "points");
      at = 5180; l3778 = l3779;
    } else {
      at = 5181; l3780 = K[1320];
      at = 5182; l3778 = l3780;
    }
    at = 5183; l3781 = rt.array_count(l3778);
    at = 5184; l3782 = K[1321];
    at = 5185; l3783 = rt.gt(l3781, l3782);
    at = 5186; if (rt.truthy(l3783)) {
      at = 5187; if (rt.truthy(l3774)) {
        at = 5188; l3786 = field(rt, l3777, "points");
        at = 5189; l3785 = l3786;
      } else {
        at = 5190; l3787 = K[1322];
        at = 5191; l3785 = l3787;
      }
      at = 5192; l3788 = K[1323];
      at = 5193; l3789 = rt.array_nth(l3785, l3788);
      at = 5194; l3784 = l3789;
    } else {
      at = 5195; l3790 = K[1324];
      at = 5196; l3784 = l3790;
    }
    at = 5197; if (rt.truthy(l3774)) {
      at = 5198; l3792 = field(rt, l3777, "width");
      at = 5199; l3791 = l3792;
    } else {
      at = 5200; l3793 = K[1325];
      at = 5201; l3791 = l3793;
    }
    at = 5202; l3794 = K[1326];
    at = 5203; l3795 = rt.int_div(l3791, l3794);
    at = 5204; l3796 = K[1327];
    at = 5205; l3797 = rt.max(l3796, l3795);
    at = 5206; l3798 = K[1328];
    at = 5207; l3799 = rt.gt(l3781, l3798);
    at = 5208; if (rt.truthy(l3799)) {
      at = 5209; if (rt.truthy(l3774)) {
        at = 5210; l3802 = field(rt, l3777, "color");
        at = 5211; l3801 = l3802;
      } else {
        at = 5212; l3803 = K[1329];
        at = 5213; l3801 = l3803;
      }
      at = 5214; l3804 = K[1330];
      at = 5215; l3805 = rt.sequence_get(l3804, l3801);
      at = 5216; l3800 = l3805;
    } else {
      at = 5217; l3806 = K[1331];
      at = 5218; l3800 = l3806;
    }
    at = 5219; markSet(rt, false, "circle", {at: l3784, radius: l3797, fill: l3800});
    at = 5220; if (rt.truthy(l3774)) {
      at = 5221; l3808 = field(rt, l3777, "points");
      at = 5222; l3807 = l3808;
    } else {
      at = 5223; l3809 = K[1332];
      at = 5224; l3807 = l3809;
    }
    at = 5225; l3810 = K[1333];
    at = 5226; l3811 = rt.sub(l3781, l3810);
    at = 5227; l3812 = K[1334];
    at = 5228; l3813 = rt.max(l3812, l3811);
    at = 5229; l3814 = K[1335];
    at = 5230; l3815 = rt.array_slice(l3807, l3814, l3813);
    at = 5231; if (rt.truthy(l3774)) {
      at = 5232; l3817 = field(rt, l3777, "points");
      at = 5233; l3816 = l3817;
    } else {
      at = 5234; l3818 = K[1336];
      at = 5235; l3816 = l3818;
    }
    at = 5236; l3819 = K[1337];
    at = 5237; l3820 = rt.gt(l3781, l3819);
    at = 5238; if (rt.truthy(l3820)) {
      at = 5239; l3822 = K[1338];
      at = 5240; l3821 = l3822;
    } else {
      at = 5241; l3823 = K[1339];
      at = 5242; l3821 = l3823;
    }
    at = 5243; l3824 = K[1340];
    at = 5244; l3825 = rt.sub(l3781, l3824);
    at = 5245; l3826 = K[1341];
    at = 5246; l3827 = rt.max(l3826, l3825);
    at = 5247; l3828 = rt.array_slice(l3816, l3821, l3827);
    at = 5248; if (rt.truthy(l3774)) {
      at = 5249; l3830 = field(rt, l3777, "color");
      at = 5250; l3829 = l3830;
    } else {
      at = 5251; l3831 = K[1342];
      at = 5252; l3829 = l3831;
    }
    at = 5253; l3832 = K[1343];
    at = 5254; l3833 = rt.sequence_get(l3832, l3829);
    at = 5255; if (rt.truthy(l3774)) {
      at = 5256; l3835 = field(rt, l3777, "width");
      at = 5257; l3834 = l3835;
    } else {
      at = 5258; l3836 = K[1344];
      at = 5259; l3834 = l3836;
    }
    at = 5260; markSet(rt, true, "line", {from: l3815, to: l3828, color: l3833, width: l3834});
    at = 5261; l3837 = field(rt, l15, "strokes");
    at = 5262; l3838 = rt.sequence_length(l3837);
    at = 5263; l3839 = K[1345];
    at = 5264; l3840 = rt.lt(l3839, l3838);
    at = 5265; if (rt.truthy(l3840)) {
      at = 5266; l3841 = field(rt, l15, "strokes");
      at = 5267; l3842 = K[1346];
      at = 5268; l3843 = rt.sequence_get(l3841, l3842);
    }
    at = 5269; if (rt.truthy(l3840)) {
      at = 5270; l3845 = field(rt, l3843, "points");
      at = 5271; l3844 = l3845;
    } else {
      at = 5272; l3846 = K[1347];
      at = 5273; l3844 = l3846;
    }
    at = 5274; l3847 = rt.array_count(l3844);
    at = 5275; l3848 = K[1348];
    at = 5276; l3849 = rt.gt(l3847, l3848);
    at = 5277; if (rt.truthy(l3849)) {
      at = 5278; if (rt.truthy(l3840)) {
        at = 5279; l3852 = field(rt, l3843, "points");
        at = 5280; l3851 = l3852;
      } else {
        at = 5281; l3853 = K[1349];
        at = 5282; l3851 = l3853;
      }
      at = 5283; l3854 = K[1350];
      at = 5284; l3855 = rt.array_nth(l3851, l3854);
      at = 5285; l3850 = l3855;
    } else {
      at = 5286; l3856 = K[1351];
      at = 5287; l3850 = l3856;
    }
    at = 5288; if (rt.truthy(l3840)) {
      at = 5289; l3858 = field(rt, l3843, "width");
      at = 5290; l3857 = l3858;
    } else {
      at = 5291; l3859 = K[1352];
      at = 5292; l3857 = l3859;
    }
    at = 5293; l3860 = K[1353];
    at = 5294; l3861 = rt.int_div(l3857, l3860);
    at = 5295; l3862 = K[1354];
    at = 5296; l3863 = rt.max(l3862, l3861);
    at = 5297; l3864 = K[1355];
    at = 5298; l3865 = rt.gt(l3847, l3864);
    at = 5299; if (rt.truthy(l3865)) {
      at = 5300; if (rt.truthy(l3840)) {
        at = 5301; l3868 = field(rt, l3843, "color");
        at = 5302; l3867 = l3868;
      } else {
        at = 5303; l3869 = K[1356];
        at = 5304; l3867 = l3869;
      }
      at = 5305; l3870 = K[1357];
      at = 5306; l3871 = rt.sequence_get(l3870, l3867);
      at = 5307; l3866 = l3871;
    } else {
      at = 5308; l3872 = K[1358];
      at = 5309; l3866 = l3872;
    }
    at = 5310; markSet(rt, false, "circle", {at: l3850, radius: l3863, fill: l3866});
    at = 5311; if (rt.truthy(l3840)) {
      at = 5312; l3874 = field(rt, l3843, "points");
      at = 5313; l3873 = l3874;
    } else {
      at = 5314; l3875 = K[1359];
      at = 5315; l3873 = l3875;
    }
    at = 5316; l3876 = K[1360];
    at = 5317; l3877 = rt.sub(l3847, l3876);
    at = 5318; l3878 = K[1361];
    at = 5319; l3879 = rt.max(l3878, l3877);
    at = 5320; l3880 = K[1362];
    at = 5321; l3881 = rt.array_slice(l3873, l3880, l3879);
    at = 5322; if (rt.truthy(l3840)) {
      at = 5323; l3883 = field(rt, l3843, "points");
      at = 5324; l3882 = l3883;
    } else {
      at = 5325; l3884 = K[1363];
      at = 5326; l3882 = l3884;
    }
    at = 5327; l3885 = K[1364];
    at = 5328; l3886 = rt.gt(l3847, l3885);
    at = 5329; if (rt.truthy(l3886)) {
      at = 5330; l3888 = K[1365];
      at = 5331; l3887 = l3888;
    } else {
      at = 5332; l3889 = K[1366];
      at = 5333; l3887 = l3889;
    }
    at = 5334; l3890 = K[1367];
    at = 5335; l3891 = rt.sub(l3847, l3890);
    at = 5336; l3892 = K[1368];
    at = 5337; l3893 = rt.max(l3892, l3891);
    at = 5338; l3894 = rt.array_slice(l3882, l3887, l3893);
    at = 5339; if (rt.truthy(l3840)) {
      at = 5340; l3896 = field(rt, l3843, "color");
      at = 5341; l3895 = l3896;
    } else {
      at = 5342; l3897 = K[1369];
      at = 5343; l3895 = l3897;
    }
    at = 5344; l3898 = K[1370];
    at = 5345; l3899 = rt.sequence_get(l3898, l3895);
    at = 5346; if (rt.truthy(l3840)) {
      at = 5347; l3901 = field(rt, l3843, "width");
      at = 5348; l3900 = l3901;
    } else {
      at = 5349; l3902 = K[1371];
      at = 5350; l3900 = l3902;
    }
    at = 5351; markSet(rt, true, "line", {from: l3881, to: l3894, color: l3899, width: l3900});
    at = 5352; l3903 = field(rt, l15, "strokes");
    at = 5353; l3904 = rt.sequence_length(l3903);
    at = 5354; l3905 = K[1372];
    at = 5355; l3906 = rt.lt(l3905, l3904);
    at = 5356; if (rt.truthy(l3906)) {
      at = 5357; l3907 = field(rt, l15, "strokes");
      at = 5358; l3908 = K[1373];
      at = 5359; l3909 = rt.sequence_get(l3907, l3908);
    }
    at = 5360; if (rt.truthy(l3906)) {
      at = 5361; l3911 = field(rt, l3909, "points");
      at = 5362; l3910 = l3911;
    } else {
      at = 5363; l3912 = K[1374];
      at = 5364; l3910 = l3912;
    }
    at = 5365; l3913 = rt.array_count(l3910);
    at = 5366; l3914 = K[1375];
    at = 5367; l3915 = rt.gt(l3913, l3914);
    at = 5368; if (rt.truthy(l3915)) {
      at = 5369; if (rt.truthy(l3906)) {
        at = 5370; l3918 = field(rt, l3909, "points");
        at = 5371; l3917 = l3918;
      } else {
        at = 5372; l3919 = K[1376];
        at = 5373; l3917 = l3919;
      }
      at = 5374; l3920 = K[1377];
      at = 5375; l3921 = rt.array_nth(l3917, l3920);
      at = 5376; l3916 = l3921;
    } else {
      at = 5377; l3922 = K[1378];
      at = 5378; l3916 = l3922;
    }
    at = 5379; if (rt.truthy(l3906)) {
      at = 5380; l3924 = field(rt, l3909, "width");
      at = 5381; l3923 = l3924;
    } else {
      at = 5382; l3925 = K[1379];
      at = 5383; l3923 = l3925;
    }
    at = 5384; l3926 = K[1380];
    at = 5385; l3927 = rt.int_div(l3923, l3926);
    at = 5386; l3928 = K[1381];
    at = 5387; l3929 = rt.max(l3928, l3927);
    at = 5388; l3930 = K[1382];
    at = 5389; l3931 = rt.gt(l3913, l3930);
    at = 5390; if (rt.truthy(l3931)) {
      at = 5391; if (rt.truthy(l3906)) {
        at = 5392; l3934 = field(rt, l3909, "color");
        at = 5393; l3933 = l3934;
      } else {
        at = 5394; l3935 = K[1383];
        at = 5395; l3933 = l3935;
      }
      at = 5396; l3936 = K[1384];
      at = 5397; l3937 = rt.sequence_get(l3936, l3933);
      at = 5398; l3932 = l3937;
    } else {
      at = 5399; l3938 = K[1385];
      at = 5400; l3932 = l3938;
    }
    at = 5401; markSet(rt, false, "circle", {at: l3916, radius: l3929, fill: l3932});
    at = 5402; if (rt.truthy(l3906)) {
      at = 5403; l3940 = field(rt, l3909, "points");
      at = 5404; l3939 = l3940;
    } else {
      at = 5405; l3941 = K[1386];
      at = 5406; l3939 = l3941;
    }
    at = 5407; l3942 = K[1387];
    at = 5408; l3943 = rt.sub(l3913, l3942);
    at = 5409; l3944 = K[1388];
    at = 5410; l3945 = rt.max(l3944, l3943);
    at = 5411; l3946 = K[1389];
    at = 5412; l3947 = rt.array_slice(l3939, l3946, l3945);
    at = 5413; if (rt.truthy(l3906)) {
      at = 5414; l3949 = field(rt, l3909, "points");
      at = 5415; l3948 = l3949;
    } else {
      at = 5416; l3950 = K[1390];
      at = 5417; l3948 = l3950;
    }
    at = 5418; l3951 = K[1391];
    at = 5419; l3952 = rt.gt(l3913, l3951);
    at = 5420; if (rt.truthy(l3952)) {
      at = 5421; l3954 = K[1392];
      at = 5422; l3953 = l3954;
    } else {
      at = 5423; l3955 = K[1393];
      at = 5424; l3953 = l3955;
    }
    at = 5425; l3956 = K[1394];
    at = 5426; l3957 = rt.sub(l3913, l3956);
    at = 5427; l3958 = K[1395];
    at = 5428; l3959 = rt.max(l3958, l3957);
    at = 5429; l3960 = rt.array_slice(l3948, l3953, l3959);
    at = 5430; if (rt.truthy(l3906)) {
      at = 5431; l3962 = field(rt, l3909, "color");
      at = 5432; l3961 = l3962;
    } else {
      at = 5433; l3963 = K[1396];
      at = 5434; l3961 = l3963;
    }
    at = 5435; l3964 = K[1397];
    at = 5436; l3965 = rt.sequence_get(l3964, l3961);
    at = 5437; if (rt.truthy(l3906)) {
      at = 5438; l3967 = field(rt, l3909, "width");
      at = 5439; l3966 = l3967;
    } else {
      at = 5440; l3968 = K[1398];
      at = 5441; l3966 = l3968;
    }
    at = 5442; markSet(rt, true, "line", {from: l3947, to: l3960, color: l3965, width: l3966});
    at = 5443; l3969 = field(rt, l15, "strokes");
    at = 5444; l3970 = rt.sequence_length(l3969);
    at = 5445; l3971 = K[1399];
    at = 5446; l3972 = rt.lt(l3971, l3970);
    at = 5447; if (rt.truthy(l3972)) {
      at = 5448; l3973 = field(rt, l15, "strokes");
      at = 5449; l3974 = K[1400];
      at = 5450; l3975 = rt.sequence_get(l3973, l3974);
    }
    at = 5451; if (rt.truthy(l3972)) {
      at = 5452; l3977 = field(rt, l3975, "points");
      at = 5453; l3976 = l3977;
    } else {
      at = 5454; l3978 = K[1401];
      at = 5455; l3976 = l3978;
    }
    at = 5456; l3979 = rt.array_count(l3976);
    at = 5457; l3980 = K[1402];
    at = 5458; l3981 = rt.gt(l3979, l3980);
    at = 5459; if (rt.truthy(l3981)) {
      at = 5460; if (rt.truthy(l3972)) {
        at = 5461; l3984 = field(rt, l3975, "points");
        at = 5462; l3983 = l3984;
      } else {
        at = 5463; l3985 = K[1403];
        at = 5464; l3983 = l3985;
      }
      at = 5465; l3986 = K[1404];
      at = 5466; l3987 = rt.array_nth(l3983, l3986);
      at = 5467; l3982 = l3987;
    } else {
      at = 5468; l3988 = K[1405];
      at = 5469; l3982 = l3988;
    }
    at = 5470; if (rt.truthy(l3972)) {
      at = 5471; l3990 = field(rt, l3975, "width");
      at = 5472; l3989 = l3990;
    } else {
      at = 5473; l3991 = K[1406];
      at = 5474; l3989 = l3991;
    }
    at = 5475; l3992 = K[1407];
    at = 5476; l3993 = rt.int_div(l3989, l3992);
    at = 5477; l3994 = K[1408];
    at = 5478; l3995 = rt.max(l3994, l3993);
    at = 5479; l3996 = K[1409];
    at = 5480; l3997 = rt.gt(l3979, l3996);
    at = 5481; if (rt.truthy(l3997)) {
      at = 5482; if (rt.truthy(l3972)) {
        at = 5483; l4000 = field(rt, l3975, "color");
        at = 5484; l3999 = l4000;
      } else {
        at = 5485; l4001 = K[1410];
        at = 5486; l3999 = l4001;
      }
      at = 5487; l4002 = K[1411];
      at = 5488; l4003 = rt.sequence_get(l4002, l3999);
      at = 5489; l3998 = l4003;
    } else {
      at = 5490; l4004 = K[1412];
      at = 5491; l3998 = l4004;
    }
    at = 5492; markSet(rt, false, "circle", {at: l3982, radius: l3995, fill: l3998});
    at = 5493; if (rt.truthy(l3972)) {
      at = 5494; l4006 = field(rt, l3975, "points");
      at = 5495; l4005 = l4006;
    } else {
      at = 5496; l4007 = K[1413];
      at = 5497; l4005 = l4007;
    }
    at = 5498; l4008 = K[1414];
    at = 5499; l4009 = rt.sub(l3979, l4008);
    at = 5500; l4010 = K[1415];
    at = 5501; l4011 = rt.max(l4010, l4009);
    at = 5502; l4012 = K[1416];
    at = 5503; l4013 = rt.array_slice(l4005, l4012, l4011);
    at = 5504; if (rt.truthy(l3972)) {
      at = 5505; l4015 = field(rt, l3975, "points");
      at = 5506; l4014 = l4015;
    } else {
      at = 5507; l4016 = K[1417];
      at = 5508; l4014 = l4016;
    }
    at = 5509; l4017 = K[1418];
    at = 5510; l4018 = rt.gt(l3979, l4017);
    at = 5511; if (rt.truthy(l4018)) {
      at = 5512; l4020 = K[1419];
      at = 5513; l4019 = l4020;
    } else {
      at = 5514; l4021 = K[1420];
      at = 5515; l4019 = l4021;
    }
    at = 5516; l4022 = K[1421];
    at = 5517; l4023 = rt.sub(l3979, l4022);
    at = 5518; l4024 = K[1422];
    at = 5519; l4025 = rt.max(l4024, l4023);
    at = 5520; l4026 = rt.array_slice(l4014, l4019, l4025);
    at = 5521; if (rt.truthy(l3972)) {
      at = 5522; l4028 = field(rt, l3975, "color");
      at = 5523; l4027 = l4028;
    } else {
      at = 5524; l4029 = K[1423];
      at = 5525; l4027 = l4029;
    }
    at = 5526; l4030 = K[1424];
    at = 5527; l4031 = rt.sequence_get(l4030, l4027);
    at = 5528; if (rt.truthy(l3972)) {
      at = 5529; l4033 = field(rt, l3975, "width");
      at = 5530; l4032 = l4033;
    } else {
      at = 5531; l4034 = K[1425];
      at = 5532; l4032 = l4034;
    }
    at = 5533; markSet(rt, true, "line", {from: l4013, to: l4026, color: l4031, width: l4032});
    at = 5534; l4035 = field(rt, l15, "strokes");
    at = 5535; l4036 = rt.sequence_length(l4035);
    at = 5536; l4037 = K[1426];
    at = 5537; l4038 = rt.lt(l4037, l4036);
    at = 5538; if (rt.truthy(l4038)) {
      at = 5539; l4039 = field(rt, l15, "strokes");
      at = 5540; l4040 = K[1427];
      at = 5541; l4041 = rt.sequence_get(l4039, l4040);
    }
    at = 5542; if (rt.truthy(l4038)) {
      at = 5543; l4043 = field(rt, l4041, "points");
      at = 5544; l4042 = l4043;
    } else {
      at = 5545; l4044 = K[1428];
      at = 5546; l4042 = l4044;
    }
    at = 5547; l4045 = rt.array_count(l4042);
    at = 5548; l4046 = K[1429];
    at = 5549; l4047 = rt.gt(l4045, l4046);
    at = 5550; if (rt.truthy(l4047)) {
      at = 5551; if (rt.truthy(l4038)) {
        at = 5552; l4050 = field(rt, l4041, "points");
        at = 5553; l4049 = l4050;
      } else {
        at = 5554; l4051 = K[1430];
        at = 5555; l4049 = l4051;
      }
      at = 5556; l4052 = K[1431];
      at = 5557; l4053 = rt.array_nth(l4049, l4052);
      at = 5558; l4048 = l4053;
    } else {
      at = 5559; l4054 = K[1432];
      at = 5560; l4048 = l4054;
    }
    at = 5561; if (rt.truthy(l4038)) {
      at = 5562; l4056 = field(rt, l4041, "width");
      at = 5563; l4055 = l4056;
    } else {
      at = 5564; l4057 = K[1433];
      at = 5565; l4055 = l4057;
    }
    at = 5566; l4058 = K[1434];
    at = 5567; l4059 = rt.int_div(l4055, l4058);
    at = 5568; l4060 = K[1435];
    at = 5569; l4061 = rt.max(l4060, l4059);
    at = 5570; l4062 = K[1436];
    at = 5571; l4063 = rt.gt(l4045, l4062);
    at = 5572; if (rt.truthy(l4063)) {
      at = 5573; if (rt.truthy(l4038)) {
        at = 5574; l4066 = field(rt, l4041, "color");
        at = 5575; l4065 = l4066;
      } else {
        at = 5576; l4067 = K[1437];
        at = 5577; l4065 = l4067;
      }
      at = 5578; l4068 = K[1438];
      at = 5579; l4069 = rt.sequence_get(l4068, l4065);
      at = 5580; l4064 = l4069;
    } else {
      at = 5581; l4070 = K[1439];
      at = 5582; l4064 = l4070;
    }
    at = 5583; markSet(rt, false, "circle", {at: l4048, radius: l4061, fill: l4064});
    at = 5584; if (rt.truthy(l4038)) {
      at = 5585; l4072 = field(rt, l4041, "points");
      at = 5586; l4071 = l4072;
    } else {
      at = 5587; l4073 = K[1440];
      at = 5588; l4071 = l4073;
    }
    at = 5589; l4074 = K[1441];
    at = 5590; l4075 = rt.sub(l4045, l4074);
    at = 5591; l4076 = K[1442];
    at = 5592; l4077 = rt.max(l4076, l4075);
    at = 5593; l4078 = K[1443];
    at = 5594; l4079 = rt.array_slice(l4071, l4078, l4077);
    at = 5595; if (rt.truthy(l4038)) {
      at = 5596; l4081 = field(rt, l4041, "points");
      at = 5597; l4080 = l4081;
    } else {
      at = 5598; l4082 = K[1444];
      at = 5599; l4080 = l4082;
    }
    at = 5600; l4083 = K[1445];
    at = 5601; l4084 = rt.gt(l4045, l4083);
    at = 5602; if (rt.truthy(l4084)) {
      at = 5603; l4086 = K[1446];
      at = 5604; l4085 = l4086;
    } else {
      at = 5605; l4087 = K[1447];
      at = 5606; l4085 = l4087;
    }
    at = 5607; l4088 = K[1448];
    at = 5608; l4089 = rt.sub(l4045, l4088);
    at = 5609; l4090 = K[1449];
    at = 5610; l4091 = rt.max(l4090, l4089);
    at = 5611; l4092 = rt.array_slice(l4080, l4085, l4091);
    at = 5612; if (rt.truthy(l4038)) {
      at = 5613; l4094 = field(rt, l4041, "color");
      at = 5614; l4093 = l4094;
    } else {
      at = 5615; l4095 = K[1450];
      at = 5616; l4093 = l4095;
    }
    at = 5617; l4096 = K[1451];
    at = 5618; l4097 = rt.sequence_get(l4096, l4093);
    at = 5619; if (rt.truthy(l4038)) {
      at = 5620; l4099 = field(rt, l4041, "width");
      at = 5621; l4098 = l4099;
    } else {
      at = 5622; l4100 = K[1452];
      at = 5623; l4098 = l4100;
    }
    at = 5624; markSet(rt, true, "line", {from: l4079, to: l4092, color: l4097, width: l4098});
    at = 5625; l4101 = field(rt, l15, "strokes");
    at = 5626; l4102 = rt.sequence_length(l4101);
    at = 5627; l4103 = K[1453];
    at = 5628; l4104 = rt.lt(l4103, l4102);
    at = 5629; if (rt.truthy(l4104)) {
      at = 5630; l4105 = field(rt, l15, "strokes");
      at = 5631; l4106 = K[1454];
      at = 5632; l4107 = rt.sequence_get(l4105, l4106);
    }
    at = 5633; if (rt.truthy(l4104)) {
      at = 5634; l4109 = field(rt, l4107, "points");
      at = 5635; l4108 = l4109;
    } else {
      at = 5636; l4110 = K[1455];
      at = 5637; l4108 = l4110;
    }
    at = 5638; l4111 = rt.array_count(l4108);
    at = 5639; l4112 = K[1456];
    at = 5640; l4113 = rt.gt(l4111, l4112);
    at = 5641; if (rt.truthy(l4113)) {
      at = 5642; if (rt.truthy(l4104)) {
        at = 5643; l4116 = field(rt, l4107, "points");
        at = 5644; l4115 = l4116;
      } else {
        at = 5645; l4117 = K[1457];
        at = 5646; l4115 = l4117;
      }
      at = 5647; l4118 = K[1458];
      at = 5648; l4119 = rt.array_nth(l4115, l4118);
      at = 5649; l4114 = l4119;
    } else {
      at = 5650; l4120 = K[1459];
      at = 5651; l4114 = l4120;
    }
    at = 5652; if (rt.truthy(l4104)) {
      at = 5653; l4122 = field(rt, l4107, "width");
      at = 5654; l4121 = l4122;
    } else {
      at = 5655; l4123 = K[1460];
      at = 5656; l4121 = l4123;
    }
    at = 5657; l4124 = K[1461];
    at = 5658; l4125 = rt.int_div(l4121, l4124);
    at = 5659; l4126 = K[1462];
    at = 5660; l4127 = rt.max(l4126, l4125);
    at = 5661; l4128 = K[1463];
    at = 5662; l4129 = rt.gt(l4111, l4128);
    at = 5663; if (rt.truthy(l4129)) {
      at = 5664; if (rt.truthy(l4104)) {
        at = 5665; l4132 = field(rt, l4107, "color");
        at = 5666; l4131 = l4132;
      } else {
        at = 5667; l4133 = K[1464];
        at = 5668; l4131 = l4133;
      }
      at = 5669; l4134 = K[1465];
      at = 5670; l4135 = rt.sequence_get(l4134, l4131);
      at = 5671; l4130 = l4135;
    } else {
      at = 5672; l4136 = K[1466];
      at = 5673; l4130 = l4136;
    }
    at = 5674; markSet(rt, false, "circle", {at: l4114, radius: l4127, fill: l4130});
    at = 5675; if (rt.truthy(l4104)) {
      at = 5676; l4138 = field(rt, l4107, "points");
      at = 5677; l4137 = l4138;
    } else {
      at = 5678; l4139 = K[1467];
      at = 5679; l4137 = l4139;
    }
    at = 5680; l4140 = K[1468];
    at = 5681; l4141 = rt.sub(l4111, l4140);
    at = 5682; l4142 = K[1469];
    at = 5683; l4143 = rt.max(l4142, l4141);
    at = 5684; l4144 = K[1470];
    at = 5685; l4145 = rt.array_slice(l4137, l4144, l4143);
    at = 5686; if (rt.truthy(l4104)) {
      at = 5687; l4147 = field(rt, l4107, "points");
      at = 5688; l4146 = l4147;
    } else {
      at = 5689; l4148 = K[1471];
      at = 5690; l4146 = l4148;
    }
    at = 5691; l4149 = K[1472];
    at = 5692; l4150 = rt.gt(l4111, l4149);
    at = 5693; if (rt.truthy(l4150)) {
      at = 5694; l4152 = K[1473];
      at = 5695; l4151 = l4152;
    } else {
      at = 5696; l4153 = K[1474];
      at = 5697; l4151 = l4153;
    }
    at = 5698; l4154 = K[1475];
    at = 5699; l4155 = rt.sub(l4111, l4154);
    at = 5700; l4156 = K[1476];
    at = 5701; l4157 = rt.max(l4156, l4155);
    at = 5702; l4158 = rt.array_slice(l4146, l4151, l4157);
    at = 5703; if (rt.truthy(l4104)) {
      at = 5704; l4160 = field(rt, l4107, "color");
      at = 5705; l4159 = l4160;
    } else {
      at = 5706; l4161 = K[1477];
      at = 5707; l4159 = l4161;
    }
    at = 5708; l4162 = K[1478];
    at = 5709; l4163 = rt.sequence_get(l4162, l4159);
    at = 5710; if (rt.truthy(l4104)) {
      at = 5711; l4165 = field(rt, l4107, "width");
      at = 5712; l4164 = l4165;
    } else {
      at = 5713; l4166 = K[1479];
      at = 5714; l4164 = l4166;
    }
    at = 5715; markSet(rt, true, "line", {from: l4145, to: l4158, color: l4163, width: l4164});
    at = 5716; l4167 = field(rt, l15, "strokes");
    at = 5717; l4168 = rt.sequence_length(l4167);
    at = 5718; l4169 = K[1480];
    at = 5719; l4170 = rt.lt(l4169, l4168);
    at = 5720; if (rt.truthy(l4170)) {
      at = 5721; l4171 = field(rt, l15, "strokes");
      at = 5722; l4172 = K[1481];
      at = 5723; l4173 = rt.sequence_get(l4171, l4172);
    }
    at = 5724; if (rt.truthy(l4170)) {
      at = 5725; l4175 = field(rt, l4173, "points");
      at = 5726; l4174 = l4175;
    } else {
      at = 5727; l4176 = K[1482];
      at = 5728; l4174 = l4176;
    }
    at = 5729; l4177 = rt.array_count(l4174);
    at = 5730; l4178 = K[1483];
    at = 5731; l4179 = rt.gt(l4177, l4178);
    at = 5732; if (rt.truthy(l4179)) {
      at = 5733; if (rt.truthy(l4170)) {
        at = 5734; l4182 = field(rt, l4173, "points");
        at = 5735; l4181 = l4182;
      } else {
        at = 5736; l4183 = K[1484];
        at = 5737; l4181 = l4183;
      }
      at = 5738; l4184 = K[1485];
      at = 5739; l4185 = rt.array_nth(l4181, l4184);
      at = 5740; l4180 = l4185;
    } else {
      at = 5741; l4186 = K[1486];
      at = 5742; l4180 = l4186;
    }
    at = 5743; if (rt.truthy(l4170)) {
      at = 5744; l4188 = field(rt, l4173, "width");
      at = 5745; l4187 = l4188;
    } else {
      at = 5746; l4189 = K[1487];
      at = 5747; l4187 = l4189;
    }
    at = 5748; l4190 = K[1488];
    at = 5749; l4191 = rt.int_div(l4187, l4190);
    at = 5750; l4192 = K[1489];
    at = 5751; l4193 = rt.max(l4192, l4191);
    at = 5752; l4194 = K[1490];
    at = 5753; l4195 = rt.gt(l4177, l4194);
    at = 5754; if (rt.truthy(l4195)) {
      at = 5755; if (rt.truthy(l4170)) {
        at = 5756; l4198 = field(rt, l4173, "color");
        at = 5757; l4197 = l4198;
      } else {
        at = 5758; l4199 = K[1491];
        at = 5759; l4197 = l4199;
      }
      at = 5760; l4200 = K[1492];
      at = 5761; l4201 = rt.sequence_get(l4200, l4197);
      at = 5762; l4196 = l4201;
    } else {
      at = 5763; l4202 = K[1493];
      at = 5764; l4196 = l4202;
    }
    at = 5765; markSet(rt, false, "circle", {at: l4180, radius: l4193, fill: l4196});
    at = 5766; if (rt.truthy(l4170)) {
      at = 5767; l4204 = field(rt, l4173, "points");
      at = 5768; l4203 = l4204;
    } else {
      at = 5769; l4205 = K[1494];
      at = 5770; l4203 = l4205;
    }
    at = 5771; l4206 = K[1495];
    at = 5772; l4207 = rt.sub(l4177, l4206);
    at = 5773; l4208 = K[1496];
    at = 5774; l4209 = rt.max(l4208, l4207);
    at = 5775; l4210 = K[1497];
    at = 5776; l4211 = rt.array_slice(l4203, l4210, l4209);
    at = 5777; if (rt.truthy(l4170)) {
      at = 5778; l4213 = field(rt, l4173, "points");
      at = 5779; l4212 = l4213;
    } else {
      at = 5780; l4214 = K[1498];
      at = 5781; l4212 = l4214;
    }
    at = 5782; l4215 = K[1499];
    at = 5783; l4216 = rt.gt(l4177, l4215);
    at = 5784; if (rt.truthy(l4216)) {
      at = 5785; l4218 = K[1500];
      at = 5786; l4217 = l4218;
    } else {
      at = 5787; l4219 = K[1501];
      at = 5788; l4217 = l4219;
    }
    at = 5789; l4220 = K[1502];
    at = 5790; l4221 = rt.sub(l4177, l4220);
    at = 5791; l4222 = K[1503];
    at = 5792; l4223 = rt.max(l4222, l4221);
    at = 5793; l4224 = rt.array_slice(l4212, l4217, l4223);
    at = 5794; if (rt.truthy(l4170)) {
      at = 5795; l4226 = field(rt, l4173, "color");
      at = 5796; l4225 = l4226;
    } else {
      at = 5797; l4227 = K[1504];
      at = 5798; l4225 = l4227;
    }
    at = 5799; l4228 = K[1505];
    at = 5800; l4229 = rt.sequence_get(l4228, l4225);
    at = 5801; if (rt.truthy(l4170)) {
      at = 5802; l4231 = field(rt, l4173, "width");
      at = 5803; l4230 = l4231;
    } else {
      at = 5804; l4232 = K[1506];
      at = 5805; l4230 = l4232;
    }
    at = 5806; markSet(rt, true, "line", {from: l4211, to: l4224, color: l4229, width: l4230});
    at = 5807; l4233 = field(rt, l15, "strokes");
    at = 5808; l4234 = rt.sequence_length(l4233);
    at = 5809; l4235 = K[1507];
    at = 5810; l4236 = rt.lt(l4235, l4234);
    at = 5811; if (rt.truthy(l4236)) {
      at = 5812; l4237 = field(rt, l15, "strokes");
      at = 5813; l4238 = K[1508];
      at = 5814; l4239 = rt.sequence_get(l4237, l4238);
    }
    at = 5815; if (rt.truthy(l4236)) {
      at = 5816; l4241 = field(rt, l4239, "points");
      at = 5817; l4240 = l4241;
    } else {
      at = 5818; l4242 = K[1509];
      at = 5819; l4240 = l4242;
    }
    at = 5820; l4243 = rt.array_count(l4240);
    at = 5821; l4244 = K[1510];
    at = 5822; l4245 = rt.gt(l4243, l4244);
    at = 5823; if (rt.truthy(l4245)) {
      at = 5824; if (rt.truthy(l4236)) {
        at = 5825; l4248 = field(rt, l4239, "points");
        at = 5826; l4247 = l4248;
      } else {
        at = 5827; l4249 = K[1511];
        at = 5828; l4247 = l4249;
      }
      at = 5829; l4250 = K[1512];
      at = 5830; l4251 = rt.array_nth(l4247, l4250);
      at = 5831; l4246 = l4251;
    } else {
      at = 5832; l4252 = K[1513];
      at = 5833; l4246 = l4252;
    }
    at = 5834; if (rt.truthy(l4236)) {
      at = 5835; l4254 = field(rt, l4239, "width");
      at = 5836; l4253 = l4254;
    } else {
      at = 5837; l4255 = K[1514];
      at = 5838; l4253 = l4255;
    }
    at = 5839; l4256 = K[1515];
    at = 5840; l4257 = rt.int_div(l4253, l4256);
    at = 5841; l4258 = K[1516];
    at = 5842; l4259 = rt.max(l4258, l4257);
    at = 5843; l4260 = K[1517];
    at = 5844; l4261 = rt.gt(l4243, l4260);
    at = 5845; if (rt.truthy(l4261)) {
      at = 5846; if (rt.truthy(l4236)) {
        at = 5847; l4264 = field(rt, l4239, "color");
        at = 5848; l4263 = l4264;
      } else {
        at = 5849; l4265 = K[1518];
        at = 5850; l4263 = l4265;
      }
      at = 5851; l4266 = K[1519];
      at = 5852; l4267 = rt.sequence_get(l4266, l4263);
      at = 5853; l4262 = l4267;
    } else {
      at = 5854; l4268 = K[1520];
      at = 5855; l4262 = l4268;
    }
    at = 5856; markSet(rt, false, "circle", {at: l4246, radius: l4259, fill: l4262});
    at = 5857; if (rt.truthy(l4236)) {
      at = 5858; l4270 = field(rt, l4239, "points");
      at = 5859; l4269 = l4270;
    } else {
      at = 5860; l4271 = K[1521];
      at = 5861; l4269 = l4271;
    }
    at = 5862; l4272 = K[1522];
    at = 5863; l4273 = rt.sub(l4243, l4272);
    at = 5864; l4274 = K[1523];
    at = 5865; l4275 = rt.max(l4274, l4273);
    at = 5866; l4276 = K[1524];
    at = 5867; l4277 = rt.array_slice(l4269, l4276, l4275);
    at = 5868; if (rt.truthy(l4236)) {
      at = 5869; l4279 = field(rt, l4239, "points");
      at = 5870; l4278 = l4279;
    } else {
      at = 5871; l4280 = K[1525];
      at = 5872; l4278 = l4280;
    }
    at = 5873; l4281 = K[1526];
    at = 5874; l4282 = rt.gt(l4243, l4281);
    at = 5875; if (rt.truthy(l4282)) {
      at = 5876; l4284 = K[1527];
      at = 5877; l4283 = l4284;
    } else {
      at = 5878; l4285 = K[1528];
      at = 5879; l4283 = l4285;
    }
    at = 5880; l4286 = K[1529];
    at = 5881; l4287 = rt.sub(l4243, l4286);
    at = 5882; l4288 = K[1530];
    at = 5883; l4289 = rt.max(l4288, l4287);
    at = 5884; l4290 = rt.array_slice(l4278, l4283, l4289);
    at = 5885; if (rt.truthy(l4236)) {
      at = 5886; l4292 = field(rt, l4239, "color");
      at = 5887; l4291 = l4292;
    } else {
      at = 5888; l4293 = K[1531];
      at = 5889; l4291 = l4293;
    }
    at = 5890; l4294 = K[1532];
    at = 5891; l4295 = rt.sequence_get(l4294, l4291);
    at = 5892; if (rt.truthy(l4236)) {
      at = 5893; l4297 = field(rt, l4239, "width");
      at = 5894; l4296 = l4297;
    } else {
      at = 5895; l4298 = K[1533];
      at = 5896; l4296 = l4298;
    }
    at = 5897; markSet(rt, true, "line", {from: l4277, to: l4290, color: l4295, width: l4296});
    at = 5898; l4299 = field(rt, l15, "strokes");
    at = 5899; l4300 = rt.sequence_length(l4299);
    at = 5900; l4301 = K[1534];
    at = 5901; l4302 = rt.lt(l4301, l4300);
    at = 5902; if (rt.truthy(l4302)) {
      at = 5903; l4303 = field(rt, l15, "strokes");
      at = 5904; l4304 = K[1535];
      at = 5905; l4305 = rt.sequence_get(l4303, l4304);
    }
    at = 5906; if (rt.truthy(l4302)) {
      at = 5907; l4307 = field(rt, l4305, "points");
      at = 5908; l4306 = l4307;
    } else {
      at = 5909; l4308 = K[1536];
      at = 5910; l4306 = l4308;
    }
    at = 5911; l4309 = rt.array_count(l4306);
    at = 5912; l4310 = K[1537];
    at = 5913; l4311 = rt.gt(l4309, l4310);
    at = 5914; if (rt.truthy(l4311)) {
      at = 5915; if (rt.truthy(l4302)) {
        at = 5916; l4314 = field(rt, l4305, "points");
        at = 5917; l4313 = l4314;
      } else {
        at = 5918; l4315 = K[1538];
        at = 5919; l4313 = l4315;
      }
      at = 5920; l4316 = K[1539];
      at = 5921; l4317 = rt.array_nth(l4313, l4316);
      at = 5922; l4312 = l4317;
    } else {
      at = 5923; l4318 = K[1540];
      at = 5924; l4312 = l4318;
    }
    at = 5925; if (rt.truthy(l4302)) {
      at = 5926; l4320 = field(rt, l4305, "width");
      at = 5927; l4319 = l4320;
    } else {
      at = 5928; l4321 = K[1541];
      at = 5929; l4319 = l4321;
    }
    at = 5930; l4322 = K[1542];
    at = 5931; l4323 = rt.int_div(l4319, l4322);
    at = 5932; l4324 = K[1543];
    at = 5933; l4325 = rt.max(l4324, l4323);
    at = 5934; l4326 = K[1544];
    at = 5935; l4327 = rt.gt(l4309, l4326);
    at = 5936; if (rt.truthy(l4327)) {
      at = 5937; if (rt.truthy(l4302)) {
        at = 5938; l4330 = field(rt, l4305, "color");
        at = 5939; l4329 = l4330;
      } else {
        at = 5940; l4331 = K[1545];
        at = 5941; l4329 = l4331;
      }
      at = 5942; l4332 = K[1546];
      at = 5943; l4333 = rt.sequence_get(l4332, l4329);
      at = 5944; l4328 = l4333;
    } else {
      at = 5945; l4334 = K[1547];
      at = 5946; l4328 = l4334;
    }
    at = 5947; markSet(rt, false, "circle", {at: l4312, radius: l4325, fill: l4328});
    at = 5948; if (rt.truthy(l4302)) {
      at = 5949; l4336 = field(rt, l4305, "points");
      at = 5950; l4335 = l4336;
    } else {
      at = 5951; l4337 = K[1548];
      at = 5952; l4335 = l4337;
    }
    at = 5953; l4338 = K[1549];
    at = 5954; l4339 = rt.sub(l4309, l4338);
    at = 5955; l4340 = K[1550];
    at = 5956; l4341 = rt.max(l4340, l4339);
    at = 5957; l4342 = K[1551];
    at = 5958; l4343 = rt.array_slice(l4335, l4342, l4341);
    at = 5959; if (rt.truthy(l4302)) {
      at = 5960; l4345 = field(rt, l4305, "points");
      at = 5961; l4344 = l4345;
    } else {
      at = 5962; l4346 = K[1552];
      at = 5963; l4344 = l4346;
    }
    at = 5964; l4347 = K[1553];
    at = 5965; l4348 = rt.gt(l4309, l4347);
    at = 5966; if (rt.truthy(l4348)) {
      at = 5967; l4350 = K[1554];
      at = 5968; l4349 = l4350;
    } else {
      at = 5969; l4351 = K[1555];
      at = 5970; l4349 = l4351;
    }
    at = 5971; l4352 = K[1556];
    at = 5972; l4353 = rt.sub(l4309, l4352);
    at = 5973; l4354 = K[1557];
    at = 5974; l4355 = rt.max(l4354, l4353);
    at = 5975; l4356 = rt.array_slice(l4344, l4349, l4355);
    at = 5976; if (rt.truthy(l4302)) {
      at = 5977; l4358 = field(rt, l4305, "color");
      at = 5978; l4357 = l4358;
    } else {
      at = 5979; l4359 = K[1558];
      at = 5980; l4357 = l4359;
    }
    at = 5981; l4360 = K[1559];
    at = 5982; l4361 = rt.sequence_get(l4360, l4357);
    at = 5983; if (rt.truthy(l4302)) {
      at = 5984; l4363 = field(rt, l4305, "width");
      at = 5985; l4362 = l4363;
    } else {
      at = 5986; l4364 = K[1560];
      at = 5987; l4362 = l4364;
    }
    at = 5988; markSet(rt, true, "line", {from: l4343, to: l4356, color: l4361, width: l4362});
    at = 5989; l4365 = field(rt, l15, "strokes");
    at = 5990; l4366 = rt.sequence_length(l4365);
    at = 5991; l4367 = K[1561];
    at = 5992; l4368 = rt.lt(l4367, l4366);
    at = 5993; if (rt.truthy(l4368)) {
      at = 5994; l4369 = field(rt, l15, "strokes");
      at = 5995; l4370 = K[1562];
      at = 5996; l4371 = rt.sequence_get(l4369, l4370);
    }
    at = 5997; if (rt.truthy(l4368)) {
      at = 5998; l4373 = field(rt, l4371, "points");
      at = 5999; l4372 = l4373;
    } else {
      at = 6000; l4374 = K[1563];
      at = 6001; l4372 = l4374;
    }
    at = 6002; l4375 = rt.array_count(l4372);
    at = 6003; l4376 = K[1564];
    at = 6004; l4377 = rt.gt(l4375, l4376);
    at = 6005; if (rt.truthy(l4377)) {
      at = 6006; if (rt.truthy(l4368)) {
        at = 6007; l4380 = field(rt, l4371, "points");
        at = 6008; l4379 = l4380;
      } else {
        at = 6009; l4381 = K[1565];
        at = 6010; l4379 = l4381;
      }
      at = 6011; l4382 = K[1566];
      at = 6012; l4383 = rt.array_nth(l4379, l4382);
      at = 6013; l4378 = l4383;
    } else {
      at = 6014; l4384 = K[1567];
      at = 6015; l4378 = l4384;
    }
    at = 6016; if (rt.truthy(l4368)) {
      at = 6017; l4386 = field(rt, l4371, "width");
      at = 6018; l4385 = l4386;
    } else {
      at = 6019; l4387 = K[1568];
      at = 6020; l4385 = l4387;
    }
    at = 6021; l4388 = K[1569];
    at = 6022; l4389 = rt.int_div(l4385, l4388);
    at = 6023; l4390 = K[1570];
    at = 6024; l4391 = rt.max(l4390, l4389);
    at = 6025; l4392 = K[1571];
    at = 6026; l4393 = rt.gt(l4375, l4392);
    at = 6027; if (rt.truthy(l4393)) {
      at = 6028; if (rt.truthy(l4368)) {
        at = 6029; l4396 = field(rt, l4371, "color");
        at = 6030; l4395 = l4396;
      } else {
        at = 6031; l4397 = K[1572];
        at = 6032; l4395 = l4397;
      }
      at = 6033; l4398 = K[1573];
      at = 6034; l4399 = rt.sequence_get(l4398, l4395);
      at = 6035; l4394 = l4399;
    } else {
      at = 6036; l4400 = K[1574];
      at = 6037; l4394 = l4400;
    }
    at = 6038; markSet(rt, false, "circle", {at: l4378, radius: l4391, fill: l4394});
    at = 6039; if (rt.truthy(l4368)) {
      at = 6040; l4402 = field(rt, l4371, "points");
      at = 6041; l4401 = l4402;
    } else {
      at = 6042; l4403 = K[1575];
      at = 6043; l4401 = l4403;
    }
    at = 6044; l4404 = K[1576];
    at = 6045; l4405 = rt.sub(l4375, l4404);
    at = 6046; l4406 = K[1577];
    at = 6047; l4407 = rt.max(l4406, l4405);
    at = 6048; l4408 = K[1578];
    at = 6049; l4409 = rt.array_slice(l4401, l4408, l4407);
    at = 6050; if (rt.truthy(l4368)) {
      at = 6051; l4411 = field(rt, l4371, "points");
      at = 6052; l4410 = l4411;
    } else {
      at = 6053; l4412 = K[1579];
      at = 6054; l4410 = l4412;
    }
    at = 6055; l4413 = K[1580];
    at = 6056; l4414 = rt.gt(l4375, l4413);
    at = 6057; if (rt.truthy(l4414)) {
      at = 6058; l4416 = K[1581];
      at = 6059; l4415 = l4416;
    } else {
      at = 6060; l4417 = K[1582];
      at = 6061; l4415 = l4417;
    }
    at = 6062; l4418 = K[1583];
    at = 6063; l4419 = rt.sub(l4375, l4418);
    at = 6064; l4420 = K[1584];
    at = 6065; l4421 = rt.max(l4420, l4419);
    at = 6066; l4422 = rt.array_slice(l4410, l4415, l4421);
    at = 6067; if (rt.truthy(l4368)) {
      at = 6068; l4424 = field(rt, l4371, "color");
      at = 6069; l4423 = l4424;
    } else {
      at = 6070; l4425 = K[1585];
      at = 6071; l4423 = l4425;
    }
    at = 6072; l4426 = K[1586];
    at = 6073; l4427 = rt.sequence_get(l4426, l4423);
    at = 6074; if (rt.truthy(l4368)) {
      at = 6075; l4429 = field(rt, l4371, "width");
      at = 6076; l4428 = l4429;
    } else {
      at = 6077; l4430 = K[1587];
      at = 6078; l4428 = l4430;
    }
    at = 6079; markSet(rt, true, "line", {from: l4409, to: l4422, color: l4427, width: l4428});
    at = 6080; l4431 = field(rt, l15, "strokes");
    at = 6081; l4432 = rt.sequence_length(l4431);
    at = 6082; l4433 = K[1588];
    at = 6083; l4434 = rt.lt(l4433, l4432);
    at = 6084; if (rt.truthy(l4434)) {
      at = 6085; l4435 = field(rt, l15, "strokes");
      at = 6086; l4436 = K[1589];
      at = 6087; l4437 = rt.sequence_get(l4435, l4436);
    }
    at = 6088; if (rt.truthy(l4434)) {
      at = 6089; l4439 = field(rt, l4437, "points");
      at = 6090; l4438 = l4439;
    } else {
      at = 6091; l4440 = K[1590];
      at = 6092; l4438 = l4440;
    }
    at = 6093; l4441 = rt.array_count(l4438);
    at = 6094; l4442 = K[1591];
    at = 6095; l4443 = rt.gt(l4441, l4442);
    at = 6096; if (rt.truthy(l4443)) {
      at = 6097; if (rt.truthy(l4434)) {
        at = 6098; l4446 = field(rt, l4437, "points");
        at = 6099; l4445 = l4446;
      } else {
        at = 6100; l4447 = K[1592];
        at = 6101; l4445 = l4447;
      }
      at = 6102; l4448 = K[1593];
      at = 6103; l4449 = rt.array_nth(l4445, l4448);
      at = 6104; l4444 = l4449;
    } else {
      at = 6105; l4450 = K[1594];
      at = 6106; l4444 = l4450;
    }
    at = 6107; if (rt.truthy(l4434)) {
      at = 6108; l4452 = field(rt, l4437, "width");
      at = 6109; l4451 = l4452;
    } else {
      at = 6110; l4453 = K[1595];
      at = 6111; l4451 = l4453;
    }
    at = 6112; l4454 = K[1596];
    at = 6113; l4455 = rt.int_div(l4451, l4454);
    at = 6114; l4456 = K[1597];
    at = 6115; l4457 = rt.max(l4456, l4455);
    at = 6116; l4458 = K[1598];
    at = 6117; l4459 = rt.gt(l4441, l4458);
    at = 6118; if (rt.truthy(l4459)) {
      at = 6119; if (rt.truthy(l4434)) {
        at = 6120; l4462 = field(rt, l4437, "color");
        at = 6121; l4461 = l4462;
      } else {
        at = 6122; l4463 = K[1599];
        at = 6123; l4461 = l4463;
      }
      at = 6124; l4464 = K[1600];
      at = 6125; l4465 = rt.sequence_get(l4464, l4461);
      at = 6126; l4460 = l4465;
    } else {
      at = 6127; l4466 = K[1601];
      at = 6128; l4460 = l4466;
    }
    at = 6129; markSet(rt, false, "circle", {at: l4444, radius: l4457, fill: l4460});
    at = 6130; if (rt.truthy(l4434)) {
      at = 6131; l4468 = field(rt, l4437, "points");
      at = 6132; l4467 = l4468;
    } else {
      at = 6133; l4469 = K[1602];
      at = 6134; l4467 = l4469;
    }
    at = 6135; l4470 = K[1603];
    at = 6136; l4471 = rt.sub(l4441, l4470);
    at = 6137; l4472 = K[1604];
    at = 6138; l4473 = rt.max(l4472, l4471);
    at = 6139; l4474 = K[1605];
    at = 6140; l4475 = rt.array_slice(l4467, l4474, l4473);
    at = 6141; if (rt.truthy(l4434)) {
      at = 6142; l4477 = field(rt, l4437, "points");
      at = 6143; l4476 = l4477;
    } else {
      at = 6144; l4478 = K[1606];
      at = 6145; l4476 = l4478;
    }
    at = 6146; l4479 = K[1607];
    at = 6147; l4480 = rt.gt(l4441, l4479);
    at = 6148; if (rt.truthy(l4480)) {
      at = 6149; l4482 = K[1608];
      at = 6150; l4481 = l4482;
    } else {
      at = 6151; l4483 = K[1609];
      at = 6152; l4481 = l4483;
    }
    at = 6153; l4484 = K[1610];
    at = 6154; l4485 = rt.sub(l4441, l4484);
    at = 6155; l4486 = K[1611];
    at = 6156; l4487 = rt.max(l4486, l4485);
    at = 6157; l4488 = rt.array_slice(l4476, l4481, l4487);
    at = 6158; if (rt.truthy(l4434)) {
      at = 6159; l4490 = field(rt, l4437, "color");
      at = 6160; l4489 = l4490;
    } else {
      at = 6161; l4491 = K[1612];
      at = 6162; l4489 = l4491;
    }
    at = 6163; l4492 = K[1613];
    at = 6164; l4493 = rt.sequence_get(l4492, l4489);
    at = 6165; if (rt.truthy(l4434)) {
      at = 6166; l4495 = field(rt, l4437, "width");
      at = 6167; l4494 = l4495;
    } else {
      at = 6168; l4496 = K[1614];
      at = 6169; l4494 = l4496;
    }
    at = 6170; markSet(rt, true, "line", {from: l4475, to: l4488, color: l4493, width: l4494});
    at = 6171; l4497 = field(rt, l15, "strokes");
    at = 6172; l4498 = rt.sequence_length(l4497);
    at = 6173; l4499 = K[1615];
    at = 6174; l4500 = rt.lt(l4499, l4498);
    at = 6175; if (rt.truthy(l4500)) {
      at = 6176; l4501 = field(rt, l15, "strokes");
      at = 6177; l4502 = K[1616];
      at = 6178; l4503 = rt.sequence_get(l4501, l4502);
    }
    at = 6179; if (rt.truthy(l4500)) {
      at = 6180; l4505 = field(rt, l4503, "points");
      at = 6181; l4504 = l4505;
    } else {
      at = 6182; l4506 = K[1617];
      at = 6183; l4504 = l4506;
    }
    at = 6184; l4507 = rt.array_count(l4504);
    at = 6185; l4508 = K[1618];
    at = 6186; l4509 = rt.gt(l4507, l4508);
    at = 6187; if (rt.truthy(l4509)) {
      at = 6188; if (rt.truthy(l4500)) {
        at = 6189; l4512 = field(rt, l4503, "points");
        at = 6190; l4511 = l4512;
      } else {
        at = 6191; l4513 = K[1619];
        at = 6192; l4511 = l4513;
      }
      at = 6193; l4514 = K[1620];
      at = 6194; l4515 = rt.array_nth(l4511, l4514);
      at = 6195; l4510 = l4515;
    } else {
      at = 6196; l4516 = K[1621];
      at = 6197; l4510 = l4516;
    }
    at = 6198; if (rt.truthy(l4500)) {
      at = 6199; l4518 = field(rt, l4503, "width");
      at = 6200; l4517 = l4518;
    } else {
      at = 6201; l4519 = K[1622];
      at = 6202; l4517 = l4519;
    }
    at = 6203; l4520 = K[1623];
    at = 6204; l4521 = rt.int_div(l4517, l4520);
    at = 6205; l4522 = K[1624];
    at = 6206; l4523 = rt.max(l4522, l4521);
    at = 6207; l4524 = K[1625];
    at = 6208; l4525 = rt.gt(l4507, l4524);
    at = 6209; if (rt.truthy(l4525)) {
      at = 6210; if (rt.truthy(l4500)) {
        at = 6211; l4528 = field(rt, l4503, "color");
        at = 6212; l4527 = l4528;
      } else {
        at = 6213; l4529 = K[1626];
        at = 6214; l4527 = l4529;
      }
      at = 6215; l4530 = K[1627];
      at = 6216; l4531 = rt.sequence_get(l4530, l4527);
      at = 6217; l4526 = l4531;
    } else {
      at = 6218; l4532 = K[1628];
      at = 6219; l4526 = l4532;
    }
    at = 6220; markSet(rt, false, "circle", {at: l4510, radius: l4523, fill: l4526});
    at = 6221; if (rt.truthy(l4500)) {
      at = 6222; l4534 = field(rt, l4503, "points");
      at = 6223; l4533 = l4534;
    } else {
      at = 6224; l4535 = K[1629];
      at = 6225; l4533 = l4535;
    }
    at = 6226; l4536 = K[1630];
    at = 6227; l4537 = rt.sub(l4507, l4536);
    at = 6228; l4538 = K[1631];
    at = 6229; l4539 = rt.max(l4538, l4537);
    at = 6230; l4540 = K[1632];
    at = 6231; l4541 = rt.array_slice(l4533, l4540, l4539);
    at = 6232; if (rt.truthy(l4500)) {
      at = 6233; l4543 = field(rt, l4503, "points");
      at = 6234; l4542 = l4543;
    } else {
      at = 6235; l4544 = K[1633];
      at = 6236; l4542 = l4544;
    }
    at = 6237; l4545 = K[1634];
    at = 6238; l4546 = rt.gt(l4507, l4545);
    at = 6239; if (rt.truthy(l4546)) {
      at = 6240; l4548 = K[1635];
      at = 6241; l4547 = l4548;
    } else {
      at = 6242; l4549 = K[1636];
      at = 6243; l4547 = l4549;
    }
    at = 6244; l4550 = K[1637];
    at = 6245; l4551 = rt.sub(l4507, l4550);
    at = 6246; l4552 = K[1638];
    at = 6247; l4553 = rt.max(l4552, l4551);
    at = 6248; l4554 = rt.array_slice(l4542, l4547, l4553);
    at = 6249; if (rt.truthy(l4500)) {
      at = 6250; l4556 = field(rt, l4503, "color");
      at = 6251; l4555 = l4556;
    } else {
      at = 6252; l4557 = K[1639];
      at = 6253; l4555 = l4557;
    }
    at = 6254; l4558 = K[1640];
    at = 6255; l4559 = rt.sequence_get(l4558, l4555);
    at = 6256; if (rt.truthy(l4500)) {
      at = 6257; l4561 = field(rt, l4503, "width");
      at = 6258; l4560 = l4561;
    } else {
      at = 6259; l4562 = K[1641];
      at = 6260; l4560 = l4562;
    }
    at = 6261; markSet(rt, true, "line", {from: l4541, to: l4554, color: l4559, width: l4560});
    at = 6262; l4563 = field(rt, l15, "strokes");
    at = 6263; l4564 = rt.sequence_length(l4563);
    at = 6264; l4565 = K[1642];
    at = 6265; l4566 = rt.lt(l4565, l4564);
    at = 6266; if (rt.truthy(l4566)) {
      at = 6267; l4567 = field(rt, l15, "strokes");
      at = 6268; l4568 = K[1643];
      at = 6269; l4569 = rt.sequence_get(l4567, l4568);
    }
    at = 6270; if (rt.truthy(l4566)) {
      at = 6271; l4571 = field(rt, l4569, "points");
      at = 6272; l4570 = l4571;
    } else {
      at = 6273; l4572 = K[1644];
      at = 6274; l4570 = l4572;
    }
    at = 6275; l4573 = rt.array_count(l4570);
    at = 6276; l4574 = K[1645];
    at = 6277; l4575 = rt.gt(l4573, l4574);
    at = 6278; if (rt.truthy(l4575)) {
      at = 6279; if (rt.truthy(l4566)) {
        at = 6280; l4578 = field(rt, l4569, "points");
        at = 6281; l4577 = l4578;
      } else {
        at = 6282; l4579 = K[1646];
        at = 6283; l4577 = l4579;
      }
      at = 6284; l4580 = K[1647];
      at = 6285; l4581 = rt.array_nth(l4577, l4580);
      at = 6286; l4576 = l4581;
    } else {
      at = 6287; l4582 = K[1648];
      at = 6288; l4576 = l4582;
    }
    at = 6289; if (rt.truthy(l4566)) {
      at = 6290; l4584 = field(rt, l4569, "width");
      at = 6291; l4583 = l4584;
    } else {
      at = 6292; l4585 = K[1649];
      at = 6293; l4583 = l4585;
    }
    at = 6294; l4586 = K[1650];
    at = 6295; l4587 = rt.int_div(l4583, l4586);
    at = 6296; l4588 = K[1651];
    at = 6297; l4589 = rt.max(l4588, l4587);
    at = 6298; l4590 = K[1652];
    at = 6299; l4591 = rt.gt(l4573, l4590);
    at = 6300; if (rt.truthy(l4591)) {
      at = 6301; if (rt.truthy(l4566)) {
        at = 6302; l4594 = field(rt, l4569, "color");
        at = 6303; l4593 = l4594;
      } else {
        at = 6304; l4595 = K[1653];
        at = 6305; l4593 = l4595;
      }
      at = 6306; l4596 = K[1654];
      at = 6307; l4597 = rt.sequence_get(l4596, l4593);
      at = 6308; l4592 = l4597;
    } else {
      at = 6309; l4598 = K[1655];
      at = 6310; l4592 = l4598;
    }
    at = 6311; markSet(rt, false, "circle", {at: l4576, radius: l4589, fill: l4592});
    at = 6312; if (rt.truthy(l4566)) {
      at = 6313; l4600 = field(rt, l4569, "points");
      at = 6314; l4599 = l4600;
    } else {
      at = 6315; l4601 = K[1656];
      at = 6316; l4599 = l4601;
    }
    at = 6317; l4602 = K[1657];
    at = 6318; l4603 = rt.sub(l4573, l4602);
    at = 6319; l4604 = K[1658];
    at = 6320; l4605 = rt.max(l4604, l4603);
    at = 6321; l4606 = K[1659];
    at = 6322; l4607 = rt.array_slice(l4599, l4606, l4605);
    at = 6323; if (rt.truthy(l4566)) {
      at = 6324; l4609 = field(rt, l4569, "points");
      at = 6325; l4608 = l4609;
    } else {
      at = 6326; l4610 = K[1660];
      at = 6327; l4608 = l4610;
    }
    at = 6328; l4611 = K[1661];
    at = 6329; l4612 = rt.gt(l4573, l4611);
    at = 6330; if (rt.truthy(l4612)) {
      at = 6331; l4614 = K[1662];
      at = 6332; l4613 = l4614;
    } else {
      at = 6333; l4615 = K[1663];
      at = 6334; l4613 = l4615;
    }
    at = 6335; l4616 = K[1664];
    at = 6336; l4617 = rt.sub(l4573, l4616);
    at = 6337; l4618 = K[1665];
    at = 6338; l4619 = rt.max(l4618, l4617);
    at = 6339; l4620 = rt.array_slice(l4608, l4613, l4619);
    at = 6340; if (rt.truthy(l4566)) {
      at = 6341; l4622 = field(rt, l4569, "color");
      at = 6342; l4621 = l4622;
    } else {
      at = 6343; l4623 = K[1666];
      at = 6344; l4621 = l4623;
    }
    at = 6345; l4624 = K[1667];
    at = 6346; l4625 = rt.sequence_get(l4624, l4621);
    at = 6347; if (rt.truthy(l4566)) {
      at = 6348; l4627 = field(rt, l4569, "width");
      at = 6349; l4626 = l4627;
    } else {
      at = 6350; l4628 = K[1668];
      at = 6351; l4626 = l4628;
    }
    at = 6352; markSet(rt, true, "line", {from: l4607, to: l4620, color: l4625, width: l4626});
    at = 6353; l4629 = field(rt, l15, "strokes");
    at = 6354; l4630 = rt.sequence_length(l4629);
    at = 6355; l4631 = K[1669];
    at = 6356; l4632 = rt.lt(l4631, l4630);
    at = 6357; if (rt.truthy(l4632)) {
      at = 6358; l4633 = field(rt, l15, "strokes");
      at = 6359; l4634 = K[1670];
      at = 6360; l4635 = rt.sequence_get(l4633, l4634);
    }
    at = 6361; if (rt.truthy(l4632)) {
      at = 6362; l4637 = field(rt, l4635, "points");
      at = 6363; l4636 = l4637;
    } else {
      at = 6364; l4638 = K[1671];
      at = 6365; l4636 = l4638;
    }
    at = 6366; l4639 = rt.array_count(l4636);
    at = 6367; l4640 = K[1672];
    at = 6368; l4641 = rt.gt(l4639, l4640);
    at = 6369; if (rt.truthy(l4641)) {
      at = 6370; if (rt.truthy(l4632)) {
        at = 6371; l4644 = field(rt, l4635, "points");
        at = 6372; l4643 = l4644;
      } else {
        at = 6373; l4645 = K[1673];
        at = 6374; l4643 = l4645;
      }
      at = 6375; l4646 = K[1674];
      at = 6376; l4647 = rt.array_nth(l4643, l4646);
      at = 6377; l4642 = l4647;
    } else {
      at = 6378; l4648 = K[1675];
      at = 6379; l4642 = l4648;
    }
    at = 6380; if (rt.truthy(l4632)) {
      at = 6381; l4650 = field(rt, l4635, "width");
      at = 6382; l4649 = l4650;
    } else {
      at = 6383; l4651 = K[1676];
      at = 6384; l4649 = l4651;
    }
    at = 6385; l4652 = K[1677];
    at = 6386; l4653 = rt.int_div(l4649, l4652);
    at = 6387; l4654 = K[1678];
    at = 6388; l4655 = rt.max(l4654, l4653);
    at = 6389; l4656 = K[1679];
    at = 6390; l4657 = rt.gt(l4639, l4656);
    at = 6391; if (rt.truthy(l4657)) {
      at = 6392; if (rt.truthy(l4632)) {
        at = 6393; l4660 = field(rt, l4635, "color");
        at = 6394; l4659 = l4660;
      } else {
        at = 6395; l4661 = K[1680];
        at = 6396; l4659 = l4661;
      }
      at = 6397; l4662 = K[1681];
      at = 6398; l4663 = rt.sequence_get(l4662, l4659);
      at = 6399; l4658 = l4663;
    } else {
      at = 6400; l4664 = K[1682];
      at = 6401; l4658 = l4664;
    }
    at = 6402; markSet(rt, false, "circle", {at: l4642, radius: l4655, fill: l4658});
    at = 6403; if (rt.truthy(l4632)) {
      at = 6404; l4666 = field(rt, l4635, "points");
      at = 6405; l4665 = l4666;
    } else {
      at = 6406; l4667 = K[1683];
      at = 6407; l4665 = l4667;
    }
    at = 6408; l4668 = K[1684];
    at = 6409; l4669 = rt.sub(l4639, l4668);
    at = 6410; l4670 = K[1685];
    at = 6411; l4671 = rt.max(l4670, l4669);
    at = 6412; l4672 = K[1686];
    at = 6413; l4673 = rt.array_slice(l4665, l4672, l4671);
    at = 6414; if (rt.truthy(l4632)) {
      at = 6415; l4675 = field(rt, l4635, "points");
      at = 6416; l4674 = l4675;
    } else {
      at = 6417; l4676 = K[1687];
      at = 6418; l4674 = l4676;
    }
    at = 6419; l4677 = K[1688];
    at = 6420; l4678 = rt.gt(l4639, l4677);
    at = 6421; if (rt.truthy(l4678)) {
      at = 6422; l4680 = K[1689];
      at = 6423; l4679 = l4680;
    } else {
      at = 6424; l4681 = K[1690];
      at = 6425; l4679 = l4681;
    }
    at = 6426; l4682 = K[1691];
    at = 6427; l4683 = rt.sub(l4639, l4682);
    at = 6428; l4684 = K[1692];
    at = 6429; l4685 = rt.max(l4684, l4683);
    at = 6430; l4686 = rt.array_slice(l4674, l4679, l4685);
    at = 6431; if (rt.truthy(l4632)) {
      at = 6432; l4688 = field(rt, l4635, "color");
      at = 6433; l4687 = l4688;
    } else {
      at = 6434; l4689 = K[1693];
      at = 6435; l4687 = l4689;
    }
    at = 6436; l4690 = K[1694];
    at = 6437; l4691 = rt.sequence_get(l4690, l4687);
    at = 6438; if (rt.truthy(l4632)) {
      at = 6439; l4693 = field(rt, l4635, "width");
      at = 6440; l4692 = l4693;
    } else {
      at = 6441; l4694 = K[1695];
      at = 6442; l4692 = l4694;
    }
    at = 6443; markSet(rt, true, "line", {from: l4673, to: l4686, color: l4691, width: l4692});
    at = 6444; l4695 = field(rt, l15, "strokes");
    at = 6445; l4696 = rt.sequence_length(l4695);
    at = 6446; l4697 = K[1696];
    at = 6447; l4698 = rt.lt(l4697, l4696);
    at = 6448; if (rt.truthy(l4698)) {
      at = 6449; l4699 = field(rt, l15, "strokes");
      at = 6450; l4700 = K[1697];
      at = 6451; l4701 = rt.sequence_get(l4699, l4700);
    }
    at = 6452; if (rt.truthy(l4698)) {
      at = 6453; l4703 = field(rt, l4701, "points");
      at = 6454; l4702 = l4703;
    } else {
      at = 6455; l4704 = K[1698];
      at = 6456; l4702 = l4704;
    }
    at = 6457; l4705 = rt.array_count(l4702);
    at = 6458; l4706 = K[1699];
    at = 6459; l4707 = rt.gt(l4705, l4706);
    at = 6460; if (rt.truthy(l4707)) {
      at = 6461; if (rt.truthy(l4698)) {
        at = 6462; l4710 = field(rt, l4701, "points");
        at = 6463; l4709 = l4710;
      } else {
        at = 6464; l4711 = K[1700];
        at = 6465; l4709 = l4711;
      }
      at = 6466; l4712 = K[1701];
      at = 6467; l4713 = rt.array_nth(l4709, l4712);
      at = 6468; l4708 = l4713;
    } else {
      at = 6469; l4714 = K[1702];
      at = 6470; l4708 = l4714;
    }
    at = 6471; if (rt.truthy(l4698)) {
      at = 6472; l4716 = field(rt, l4701, "width");
      at = 6473; l4715 = l4716;
    } else {
      at = 6474; l4717 = K[1703];
      at = 6475; l4715 = l4717;
    }
    at = 6476; l4718 = K[1704];
    at = 6477; l4719 = rt.int_div(l4715, l4718);
    at = 6478; l4720 = K[1705];
    at = 6479; l4721 = rt.max(l4720, l4719);
    at = 6480; l4722 = K[1706];
    at = 6481; l4723 = rt.gt(l4705, l4722);
    at = 6482; if (rt.truthy(l4723)) {
      at = 6483; if (rt.truthy(l4698)) {
        at = 6484; l4726 = field(rt, l4701, "color");
        at = 6485; l4725 = l4726;
      } else {
        at = 6486; l4727 = K[1707];
        at = 6487; l4725 = l4727;
      }
      at = 6488; l4728 = K[1708];
      at = 6489; l4729 = rt.sequence_get(l4728, l4725);
      at = 6490; l4724 = l4729;
    } else {
      at = 6491; l4730 = K[1709];
      at = 6492; l4724 = l4730;
    }
    at = 6493; markSet(rt, false, "circle", {at: l4708, radius: l4721, fill: l4724});
    at = 6494; if (rt.truthy(l4698)) {
      at = 6495; l4732 = field(rt, l4701, "points");
      at = 6496; l4731 = l4732;
    } else {
      at = 6497; l4733 = K[1710];
      at = 6498; l4731 = l4733;
    }
    at = 6499; l4734 = K[1711];
    at = 6500; l4735 = rt.sub(l4705, l4734);
    at = 6501; l4736 = K[1712];
    at = 6502; l4737 = rt.max(l4736, l4735);
    at = 6503; l4738 = K[1713];
    at = 6504; l4739 = rt.array_slice(l4731, l4738, l4737);
    at = 6505; if (rt.truthy(l4698)) {
      at = 6506; l4741 = field(rt, l4701, "points");
      at = 6507; l4740 = l4741;
    } else {
      at = 6508; l4742 = K[1714];
      at = 6509; l4740 = l4742;
    }
    at = 6510; l4743 = K[1715];
    at = 6511; l4744 = rt.gt(l4705, l4743);
    at = 6512; if (rt.truthy(l4744)) {
      at = 6513; l4746 = K[1716];
      at = 6514; l4745 = l4746;
    } else {
      at = 6515; l4747 = K[1717];
      at = 6516; l4745 = l4747;
    }
    at = 6517; l4748 = K[1718];
    at = 6518; l4749 = rt.sub(l4705, l4748);
    at = 6519; l4750 = K[1719];
    at = 6520; l4751 = rt.max(l4750, l4749);
    at = 6521; l4752 = rt.array_slice(l4740, l4745, l4751);
    at = 6522; if (rt.truthy(l4698)) {
      at = 6523; l4754 = field(rt, l4701, "color");
      at = 6524; l4753 = l4754;
    } else {
      at = 6525; l4755 = K[1720];
      at = 6526; l4753 = l4755;
    }
    at = 6527; l4756 = K[1721];
    at = 6528; l4757 = rt.sequence_get(l4756, l4753);
    at = 6529; if (rt.truthy(l4698)) {
      at = 6530; l4759 = field(rt, l4701, "width");
      at = 6531; l4758 = l4759;
    } else {
      at = 6532; l4760 = K[1722];
      at = 6533; l4758 = l4760;
    }
    at = 6534; markSet(rt, true, "line", {from: l4739, to: l4752, color: l4757, width: l4758});
    at = 6535; l4761 = field(rt, l15, "strokes");
    at = 6536; l4762 = rt.sequence_length(l4761);
    at = 6537; l4763 = K[1723];
    at = 6538; l4764 = rt.lt(l4763, l4762);
    at = 6539; if (rt.truthy(l4764)) {
      at = 6540; l4765 = field(rt, l15, "strokes");
      at = 6541; l4766 = K[1724];
      at = 6542; l4767 = rt.sequence_get(l4765, l4766);
    }
    at = 6543; if (rt.truthy(l4764)) {
      at = 6544; l4769 = field(rt, l4767, "points");
      at = 6545; l4768 = l4769;
    } else {
      at = 6546; l4770 = K[1725];
      at = 6547; l4768 = l4770;
    }
    at = 6548; l4771 = rt.array_count(l4768);
    at = 6549; l4772 = K[1726];
    at = 6550; l4773 = rt.gt(l4771, l4772);
    at = 6551; if (rt.truthy(l4773)) {
      at = 6552; if (rt.truthy(l4764)) {
        at = 6553; l4776 = field(rt, l4767, "points");
        at = 6554; l4775 = l4776;
      } else {
        at = 6555; l4777 = K[1727];
        at = 6556; l4775 = l4777;
      }
      at = 6557; l4778 = K[1728];
      at = 6558; l4779 = rt.array_nth(l4775, l4778);
      at = 6559; l4774 = l4779;
    } else {
      at = 6560; l4780 = K[1729];
      at = 6561; l4774 = l4780;
    }
    at = 6562; if (rt.truthy(l4764)) {
      at = 6563; l4782 = field(rt, l4767, "width");
      at = 6564; l4781 = l4782;
    } else {
      at = 6565; l4783 = K[1730];
      at = 6566; l4781 = l4783;
    }
    at = 6567; l4784 = K[1731];
    at = 6568; l4785 = rt.int_div(l4781, l4784);
    at = 6569; l4786 = K[1732];
    at = 6570; l4787 = rt.max(l4786, l4785);
    at = 6571; l4788 = K[1733];
    at = 6572; l4789 = rt.gt(l4771, l4788);
    at = 6573; if (rt.truthy(l4789)) {
      at = 6574; if (rt.truthy(l4764)) {
        at = 6575; l4792 = field(rt, l4767, "color");
        at = 6576; l4791 = l4792;
      } else {
        at = 6577; l4793 = K[1734];
        at = 6578; l4791 = l4793;
      }
      at = 6579; l4794 = K[1735];
      at = 6580; l4795 = rt.sequence_get(l4794, l4791);
      at = 6581; l4790 = l4795;
    } else {
      at = 6582; l4796 = K[1736];
      at = 6583; l4790 = l4796;
    }
    at = 6584; markSet(rt, false, "circle", {at: l4774, radius: l4787, fill: l4790});
    at = 6585; if (rt.truthy(l4764)) {
      at = 6586; l4798 = field(rt, l4767, "points");
      at = 6587; l4797 = l4798;
    } else {
      at = 6588; l4799 = K[1737];
      at = 6589; l4797 = l4799;
    }
    at = 6590; l4800 = K[1738];
    at = 6591; l4801 = rt.sub(l4771, l4800);
    at = 6592; l4802 = K[1739];
    at = 6593; l4803 = rt.max(l4802, l4801);
    at = 6594; l4804 = K[1740];
    at = 6595; l4805 = rt.array_slice(l4797, l4804, l4803);
    at = 6596; if (rt.truthy(l4764)) {
      at = 6597; l4807 = field(rt, l4767, "points");
      at = 6598; l4806 = l4807;
    } else {
      at = 6599; l4808 = K[1741];
      at = 6600; l4806 = l4808;
    }
    at = 6601; l4809 = K[1742];
    at = 6602; l4810 = rt.gt(l4771, l4809);
    at = 6603; if (rt.truthy(l4810)) {
      at = 6604; l4812 = K[1743];
      at = 6605; l4811 = l4812;
    } else {
      at = 6606; l4813 = K[1744];
      at = 6607; l4811 = l4813;
    }
    at = 6608; l4814 = K[1745];
    at = 6609; l4815 = rt.sub(l4771, l4814);
    at = 6610; l4816 = K[1746];
    at = 6611; l4817 = rt.max(l4816, l4815);
    at = 6612; l4818 = rt.array_slice(l4806, l4811, l4817);
    at = 6613; if (rt.truthy(l4764)) {
      at = 6614; l4820 = field(rt, l4767, "color");
      at = 6615; l4819 = l4820;
    } else {
      at = 6616; l4821 = K[1747];
      at = 6617; l4819 = l4821;
    }
    at = 6618; l4822 = K[1748];
    at = 6619; l4823 = rt.sequence_get(l4822, l4819);
    at = 6620; if (rt.truthy(l4764)) {
      at = 6621; l4825 = field(rt, l4767, "width");
      at = 6622; l4824 = l4825;
    } else {
      at = 6623; l4826 = K[1749];
      at = 6624; l4824 = l4826;
    }
    at = 6625; markSet(rt, true, "line", {from: l4805, to: l4818, color: l4823, width: l4824});
    at = 6626; l4827 = field(rt, l15, "strokes");
    at = 6627; l4828 = rt.sequence_length(l4827);
    at = 6628; l4829 = K[1750];
    at = 6629; l4830 = rt.lt(l4829, l4828);
    at = 6630; if (rt.truthy(l4830)) {
      at = 6631; l4831 = field(rt, l15, "strokes");
      at = 6632; l4832 = K[1751];
      at = 6633; l4833 = rt.sequence_get(l4831, l4832);
    }
    at = 6634; if (rt.truthy(l4830)) {
      at = 6635; l4835 = field(rt, l4833, "points");
      at = 6636; l4834 = l4835;
    } else {
      at = 6637; l4836 = K[1752];
      at = 6638; l4834 = l4836;
    }
    at = 6639; l4837 = rt.array_count(l4834);
    at = 6640; l4838 = K[1753];
    at = 6641; l4839 = rt.gt(l4837, l4838);
    at = 6642; if (rt.truthy(l4839)) {
      at = 6643; if (rt.truthy(l4830)) {
        at = 6644; l4842 = field(rt, l4833, "points");
        at = 6645; l4841 = l4842;
      } else {
        at = 6646; l4843 = K[1754];
        at = 6647; l4841 = l4843;
      }
      at = 6648; l4844 = K[1755];
      at = 6649; l4845 = rt.array_nth(l4841, l4844);
      at = 6650; l4840 = l4845;
    } else {
      at = 6651; l4846 = K[1756];
      at = 6652; l4840 = l4846;
    }
    at = 6653; if (rt.truthy(l4830)) {
      at = 6654; l4848 = field(rt, l4833, "width");
      at = 6655; l4847 = l4848;
    } else {
      at = 6656; l4849 = K[1757];
      at = 6657; l4847 = l4849;
    }
    at = 6658; l4850 = K[1758];
    at = 6659; l4851 = rt.int_div(l4847, l4850);
    at = 6660; l4852 = K[1759];
    at = 6661; l4853 = rt.max(l4852, l4851);
    at = 6662; l4854 = K[1760];
    at = 6663; l4855 = rt.gt(l4837, l4854);
    at = 6664; if (rt.truthy(l4855)) {
      at = 6665; if (rt.truthy(l4830)) {
        at = 6666; l4858 = field(rt, l4833, "color");
        at = 6667; l4857 = l4858;
      } else {
        at = 6668; l4859 = K[1761];
        at = 6669; l4857 = l4859;
      }
      at = 6670; l4860 = K[1762];
      at = 6671; l4861 = rt.sequence_get(l4860, l4857);
      at = 6672; l4856 = l4861;
    } else {
      at = 6673; l4862 = K[1763];
      at = 6674; l4856 = l4862;
    }
    at = 6675; markSet(rt, false, "circle", {at: l4840, radius: l4853, fill: l4856});
    at = 6676; if (rt.truthy(l4830)) {
      at = 6677; l4864 = field(rt, l4833, "points");
      at = 6678; l4863 = l4864;
    } else {
      at = 6679; l4865 = K[1764];
      at = 6680; l4863 = l4865;
    }
    at = 6681; l4866 = K[1765];
    at = 6682; l4867 = rt.sub(l4837, l4866);
    at = 6683; l4868 = K[1766];
    at = 6684; l4869 = rt.max(l4868, l4867);
    at = 6685; l4870 = K[1767];
    at = 6686; l4871 = rt.array_slice(l4863, l4870, l4869);
    at = 6687; if (rt.truthy(l4830)) {
      at = 6688; l4873 = field(rt, l4833, "points");
      at = 6689; l4872 = l4873;
    } else {
      at = 6690; l4874 = K[1768];
      at = 6691; l4872 = l4874;
    }
    at = 6692; l4875 = K[1769];
    at = 6693; l4876 = rt.gt(l4837, l4875);
    at = 6694; if (rt.truthy(l4876)) {
      at = 6695; l4878 = K[1770];
      at = 6696; l4877 = l4878;
    } else {
      at = 6697; l4879 = K[1771];
      at = 6698; l4877 = l4879;
    }
    at = 6699; l4880 = K[1772];
    at = 6700; l4881 = rt.sub(l4837, l4880);
    at = 6701; l4882 = K[1773];
    at = 6702; l4883 = rt.max(l4882, l4881);
    at = 6703; l4884 = rt.array_slice(l4872, l4877, l4883);
    at = 6704; if (rt.truthy(l4830)) {
      at = 6705; l4886 = field(rt, l4833, "color");
      at = 6706; l4885 = l4886;
    } else {
      at = 6707; l4887 = K[1774];
      at = 6708; l4885 = l4887;
    }
    at = 6709; l4888 = K[1775];
    at = 6710; l4889 = rt.sequence_get(l4888, l4885);
    at = 6711; if (rt.truthy(l4830)) {
      at = 6712; l4891 = field(rt, l4833, "width");
      at = 6713; l4890 = l4891;
    } else {
      at = 6714; l4892 = K[1776];
      at = 6715; l4890 = l4892;
    }
    at = 6716; markSet(rt, true, "line", {from: l4871, to: l4884, color: l4889, width: l4890});
    at = 6717; l4893 = field(rt, l15, "strokes");
    at = 6718; l4894 = rt.sequence_length(l4893);
    at = 6719; l4895 = K[1777];
    at = 6720; l4896 = rt.lt(l4895, l4894);
    at = 6721; if (rt.truthy(l4896)) {
      at = 6722; l4897 = field(rt, l15, "strokes");
      at = 6723; l4898 = K[1778];
      at = 6724; l4899 = rt.sequence_get(l4897, l4898);
    }
    at = 6725; if (rt.truthy(l4896)) {
      at = 6726; l4901 = field(rt, l4899, "points");
      at = 6727; l4900 = l4901;
    } else {
      at = 6728; l4902 = K[1779];
      at = 6729; l4900 = l4902;
    }
    at = 6730; l4903 = rt.array_count(l4900);
    at = 6731; l4904 = K[1780];
    at = 6732; l4905 = rt.gt(l4903, l4904);
    at = 6733; if (rt.truthy(l4905)) {
      at = 6734; if (rt.truthy(l4896)) {
        at = 6735; l4908 = field(rt, l4899, "points");
        at = 6736; l4907 = l4908;
      } else {
        at = 6737; l4909 = K[1781];
        at = 6738; l4907 = l4909;
      }
      at = 6739; l4910 = K[1782];
      at = 6740; l4911 = rt.array_nth(l4907, l4910);
      at = 6741; l4906 = l4911;
    } else {
      at = 6742; l4912 = K[1783];
      at = 6743; l4906 = l4912;
    }
    at = 6744; if (rt.truthy(l4896)) {
      at = 6745; l4914 = field(rt, l4899, "width");
      at = 6746; l4913 = l4914;
    } else {
      at = 6747; l4915 = K[1784];
      at = 6748; l4913 = l4915;
    }
    at = 6749; l4916 = K[1785];
    at = 6750; l4917 = rt.int_div(l4913, l4916);
    at = 6751; l4918 = K[1786];
    at = 6752; l4919 = rt.max(l4918, l4917);
    at = 6753; l4920 = K[1787];
    at = 6754; l4921 = rt.gt(l4903, l4920);
    at = 6755; if (rt.truthy(l4921)) {
      at = 6756; if (rt.truthy(l4896)) {
        at = 6757; l4924 = field(rt, l4899, "color");
        at = 6758; l4923 = l4924;
      } else {
        at = 6759; l4925 = K[1788];
        at = 6760; l4923 = l4925;
      }
      at = 6761; l4926 = K[1789];
      at = 6762; l4927 = rt.sequence_get(l4926, l4923);
      at = 6763; l4922 = l4927;
    } else {
      at = 6764; l4928 = K[1790];
      at = 6765; l4922 = l4928;
    }
    at = 6766; markSet(rt, false, "circle", {at: l4906, radius: l4919, fill: l4922});
    at = 6767; if (rt.truthy(l4896)) {
      at = 6768; l4930 = field(rt, l4899, "points");
      at = 6769; l4929 = l4930;
    } else {
      at = 6770; l4931 = K[1791];
      at = 6771; l4929 = l4931;
    }
    at = 6772; l4932 = K[1792];
    at = 6773; l4933 = rt.sub(l4903, l4932);
    at = 6774; l4934 = K[1793];
    at = 6775; l4935 = rt.max(l4934, l4933);
    at = 6776; l4936 = K[1794];
    at = 6777; l4937 = rt.array_slice(l4929, l4936, l4935);
    at = 6778; if (rt.truthy(l4896)) {
      at = 6779; l4939 = field(rt, l4899, "points");
      at = 6780; l4938 = l4939;
    } else {
      at = 6781; l4940 = K[1795];
      at = 6782; l4938 = l4940;
    }
    at = 6783; l4941 = K[1796];
    at = 6784; l4942 = rt.gt(l4903, l4941);
    at = 6785; if (rt.truthy(l4942)) {
      at = 6786; l4944 = K[1797];
      at = 6787; l4943 = l4944;
    } else {
      at = 6788; l4945 = K[1798];
      at = 6789; l4943 = l4945;
    }
    at = 6790; l4946 = K[1799];
    at = 6791; l4947 = rt.sub(l4903, l4946);
    at = 6792; l4948 = K[1800];
    at = 6793; l4949 = rt.max(l4948, l4947);
    at = 6794; l4950 = rt.array_slice(l4938, l4943, l4949);
    at = 6795; if (rt.truthy(l4896)) {
      at = 6796; l4952 = field(rt, l4899, "color");
      at = 6797; l4951 = l4952;
    } else {
      at = 6798; l4953 = K[1801];
      at = 6799; l4951 = l4953;
    }
    at = 6800; l4954 = K[1802];
    at = 6801; l4955 = rt.sequence_get(l4954, l4951);
    at = 6802; if (rt.truthy(l4896)) {
      at = 6803; l4957 = field(rt, l4899, "width");
      at = 6804; l4956 = l4957;
    } else {
      at = 6805; l4958 = K[1803];
      at = 6806; l4956 = l4958;
    }
    at = 6807; markSet(rt, true, "line", {from: l4937, to: l4950, color: l4955, width: l4956});
    at = 6808; l4959 = field(rt, l15, "strokes");
    at = 6809; l4960 = rt.sequence_length(l4959);
    at = 6810; l4961 = K[1804];
    at = 6811; l4962 = rt.lt(l4961, l4960);
    at = 6812; if (rt.truthy(l4962)) {
      at = 6813; l4963 = field(rt, l15, "strokes");
      at = 6814; l4964 = K[1805];
      at = 6815; l4965 = rt.sequence_get(l4963, l4964);
    }
    at = 6816; if (rt.truthy(l4962)) {
      at = 6817; l4967 = field(rt, l4965, "points");
      at = 6818; l4966 = l4967;
    } else {
      at = 6819; l4968 = K[1806];
      at = 6820; l4966 = l4968;
    }
    at = 6821; l4969 = rt.array_count(l4966);
    at = 6822; l4970 = K[1807];
    at = 6823; l4971 = rt.gt(l4969, l4970);
    at = 6824; if (rt.truthy(l4971)) {
      at = 6825; if (rt.truthy(l4962)) {
        at = 6826; l4974 = field(rt, l4965, "points");
        at = 6827; l4973 = l4974;
      } else {
        at = 6828; l4975 = K[1808];
        at = 6829; l4973 = l4975;
      }
      at = 6830; l4976 = K[1809];
      at = 6831; l4977 = rt.array_nth(l4973, l4976);
      at = 6832; l4972 = l4977;
    } else {
      at = 6833; l4978 = K[1810];
      at = 6834; l4972 = l4978;
    }
    at = 6835; if (rt.truthy(l4962)) {
      at = 6836; l4980 = field(rt, l4965, "width");
      at = 6837; l4979 = l4980;
    } else {
      at = 6838; l4981 = K[1811];
      at = 6839; l4979 = l4981;
    }
    at = 6840; l4982 = K[1812];
    at = 6841; l4983 = rt.int_div(l4979, l4982);
    at = 6842; l4984 = K[1813];
    at = 6843; l4985 = rt.max(l4984, l4983);
    at = 6844; l4986 = K[1814];
    at = 6845; l4987 = rt.gt(l4969, l4986);
    at = 6846; if (rt.truthy(l4987)) {
      at = 6847; if (rt.truthy(l4962)) {
        at = 6848; l4990 = field(rt, l4965, "color");
        at = 6849; l4989 = l4990;
      } else {
        at = 6850; l4991 = K[1815];
        at = 6851; l4989 = l4991;
      }
      at = 6852; l4992 = K[1816];
      at = 6853; l4993 = rt.sequence_get(l4992, l4989);
      at = 6854; l4988 = l4993;
    } else {
      at = 6855; l4994 = K[1817];
      at = 6856; l4988 = l4994;
    }
    at = 6857; markSet(rt, false, "circle", {at: l4972, radius: l4985, fill: l4988});
    at = 6858; if (rt.truthy(l4962)) {
      at = 6859; l4996 = field(rt, l4965, "points");
      at = 6860; l4995 = l4996;
    } else {
      at = 6861; l4997 = K[1818];
      at = 6862; l4995 = l4997;
    }
    at = 6863; l4998 = K[1819];
    at = 6864; l4999 = rt.sub(l4969, l4998);
    at = 6865; l5000 = K[1820];
    at = 6866; l5001 = rt.max(l5000, l4999);
    at = 6867; l5002 = K[1821];
    at = 6868; l5003 = rt.array_slice(l4995, l5002, l5001);
    at = 6869; if (rt.truthy(l4962)) {
      at = 6870; l5005 = field(rt, l4965, "points");
      at = 6871; l5004 = l5005;
    } else {
      at = 6872; l5006 = K[1822];
      at = 6873; l5004 = l5006;
    }
    at = 6874; l5007 = K[1823];
    at = 6875; l5008 = rt.gt(l4969, l5007);
    at = 6876; if (rt.truthy(l5008)) {
      at = 6877; l5010 = K[1824];
      at = 6878; l5009 = l5010;
    } else {
      at = 6879; l5011 = K[1825];
      at = 6880; l5009 = l5011;
    }
    at = 6881; l5012 = K[1826];
    at = 6882; l5013 = rt.sub(l4969, l5012);
    at = 6883; l5014 = K[1827];
    at = 6884; l5015 = rt.max(l5014, l5013);
    at = 6885; l5016 = rt.array_slice(l5004, l5009, l5015);
    at = 6886; if (rt.truthy(l4962)) {
      at = 6887; l5018 = field(rt, l4965, "color");
      at = 6888; l5017 = l5018;
    } else {
      at = 6889; l5019 = K[1828];
      at = 6890; l5017 = l5019;
    }
    at = 6891; l5020 = K[1829];
    at = 6892; l5021 = rt.sequence_get(l5020, l5017);
    at = 6893; if (rt.truthy(l4962)) {
      at = 6894; l5023 = field(rt, l4965, "width");
      at = 6895; l5022 = l5023;
    } else {
      at = 6896; l5024 = K[1830];
      at = 6897; l5022 = l5024;
    }
    at = 6898; markSet(rt, true, "line", {from: l5003, to: l5016, color: l5021, width: l5022});
    at = 6899; l5025 = field(rt, l15, "strokes");
    at = 6900; l5026 = rt.sequence_length(l5025);
    at = 6901; l5027 = K[1831];
    at = 6902; l5028 = rt.lt(l5027, l5026);
    at = 6903; if (rt.truthy(l5028)) {
      at = 6904; l5029 = field(rt, l15, "strokes");
      at = 6905; l5030 = K[1832];
      at = 6906; l5031 = rt.sequence_get(l5029, l5030);
    }
    at = 6907; if (rt.truthy(l5028)) {
      at = 6908; l5033 = field(rt, l5031, "points");
      at = 6909; l5032 = l5033;
    } else {
      at = 6910; l5034 = K[1833];
      at = 6911; l5032 = l5034;
    }
    at = 6912; l5035 = rt.array_count(l5032);
    at = 6913; l5036 = K[1834];
    at = 6914; l5037 = rt.gt(l5035, l5036);
    at = 6915; if (rt.truthy(l5037)) {
      at = 6916; if (rt.truthy(l5028)) {
        at = 6917; l5040 = field(rt, l5031, "points");
        at = 6918; l5039 = l5040;
      } else {
        at = 6919; l5041 = K[1835];
        at = 6920; l5039 = l5041;
      }
      at = 6921; l5042 = K[1836];
      at = 6922; l5043 = rt.array_nth(l5039, l5042);
      at = 6923; l5038 = l5043;
    } else {
      at = 6924; l5044 = K[1837];
      at = 6925; l5038 = l5044;
    }
    at = 6926; if (rt.truthy(l5028)) {
      at = 6927; l5046 = field(rt, l5031, "width");
      at = 6928; l5045 = l5046;
    } else {
      at = 6929; l5047 = K[1838];
      at = 6930; l5045 = l5047;
    }
    at = 6931; l5048 = K[1839];
    at = 6932; l5049 = rt.int_div(l5045, l5048);
    at = 6933; l5050 = K[1840];
    at = 6934; l5051 = rt.max(l5050, l5049);
    at = 6935; l5052 = K[1841];
    at = 6936; l5053 = rt.gt(l5035, l5052);
    at = 6937; if (rt.truthy(l5053)) {
      at = 6938; if (rt.truthy(l5028)) {
        at = 6939; l5056 = field(rt, l5031, "color");
        at = 6940; l5055 = l5056;
      } else {
        at = 6941; l5057 = K[1842];
        at = 6942; l5055 = l5057;
      }
      at = 6943; l5058 = K[1843];
      at = 6944; l5059 = rt.sequence_get(l5058, l5055);
      at = 6945; l5054 = l5059;
    } else {
      at = 6946; l5060 = K[1844];
      at = 6947; l5054 = l5060;
    }
    at = 6948; markSet(rt, false, "circle", {at: l5038, radius: l5051, fill: l5054});
    at = 6949; if (rt.truthy(l5028)) {
      at = 6950; l5062 = field(rt, l5031, "points");
      at = 6951; l5061 = l5062;
    } else {
      at = 6952; l5063 = K[1845];
      at = 6953; l5061 = l5063;
    }
    at = 6954; l5064 = K[1846];
    at = 6955; l5065 = rt.sub(l5035, l5064);
    at = 6956; l5066 = K[1847];
    at = 6957; l5067 = rt.max(l5066, l5065);
    at = 6958; l5068 = K[1848];
    at = 6959; l5069 = rt.array_slice(l5061, l5068, l5067);
    at = 6960; if (rt.truthy(l5028)) {
      at = 6961; l5071 = field(rt, l5031, "points");
      at = 6962; l5070 = l5071;
    } else {
      at = 6963; l5072 = K[1849];
      at = 6964; l5070 = l5072;
    }
    at = 6965; l5073 = K[1850];
    at = 6966; l5074 = rt.gt(l5035, l5073);
    at = 6967; if (rt.truthy(l5074)) {
      at = 6968; l5076 = K[1851];
      at = 6969; l5075 = l5076;
    } else {
      at = 6970; l5077 = K[1852];
      at = 6971; l5075 = l5077;
    }
    at = 6972; l5078 = K[1853];
    at = 6973; l5079 = rt.sub(l5035, l5078);
    at = 6974; l5080 = K[1854];
    at = 6975; l5081 = rt.max(l5080, l5079);
    at = 6976; l5082 = rt.array_slice(l5070, l5075, l5081);
    at = 6977; if (rt.truthy(l5028)) {
      at = 6978; l5084 = field(rt, l5031, "color");
      at = 6979; l5083 = l5084;
    } else {
      at = 6980; l5085 = K[1855];
      at = 6981; l5083 = l5085;
    }
    at = 6982; l5086 = K[1856];
    at = 6983; l5087 = rt.sequence_get(l5086, l5083);
    at = 6984; if (rt.truthy(l5028)) {
      at = 6985; l5089 = field(rt, l5031, "width");
      at = 6986; l5088 = l5089;
    } else {
      at = 6987; l5090 = K[1857];
      at = 6988; l5088 = l5090;
    }
    at = 6989; markSet(rt, true, "line", {from: l5069, to: l5082, color: l5087, width: l5088});
    at = 6990; l5091 = field(rt, l15, "strokes");
    at = 6991; l5092 = rt.sequence_length(l5091);
    at = 6992; l5093 = K[1858];
    at = 6993; l5094 = rt.lt(l5093, l5092);
    at = 6994; if (rt.truthy(l5094)) {
      at = 6995; l5095 = field(rt, l15, "strokes");
      at = 6996; l5096 = K[1859];
      at = 6997; l5097 = rt.sequence_get(l5095, l5096);
    }
    at = 6998; if (rt.truthy(l5094)) {
      at = 6999; l5099 = field(rt, l5097, "points");
      at = 7000; l5098 = l5099;
    } else {
      at = 7001; l5100 = K[1860];
      at = 7002; l5098 = l5100;
    }
    at = 7003; l5101 = rt.array_count(l5098);
    at = 7004; l5102 = K[1861];
    at = 7005; l5103 = rt.gt(l5101, l5102);
    at = 7006; if (rt.truthy(l5103)) {
      at = 7007; if (rt.truthy(l5094)) {
        at = 7008; l5106 = field(rt, l5097, "points");
        at = 7009; l5105 = l5106;
      } else {
        at = 7010; l5107 = K[1862];
        at = 7011; l5105 = l5107;
      }
      at = 7012; l5108 = K[1863];
      at = 7013; l5109 = rt.array_nth(l5105, l5108);
      at = 7014; l5104 = l5109;
    } else {
      at = 7015; l5110 = K[1864];
      at = 7016; l5104 = l5110;
    }
    at = 7017; if (rt.truthy(l5094)) {
      at = 7018; l5112 = field(rt, l5097, "width");
      at = 7019; l5111 = l5112;
    } else {
      at = 7020; l5113 = K[1865];
      at = 7021; l5111 = l5113;
    }
    at = 7022; l5114 = K[1866];
    at = 7023; l5115 = rt.int_div(l5111, l5114);
    at = 7024; l5116 = K[1867];
    at = 7025; l5117 = rt.max(l5116, l5115);
    at = 7026; l5118 = K[1868];
    at = 7027; l5119 = rt.gt(l5101, l5118);
    at = 7028; if (rt.truthy(l5119)) {
      at = 7029; if (rt.truthy(l5094)) {
        at = 7030; l5122 = field(rt, l5097, "color");
        at = 7031; l5121 = l5122;
      } else {
        at = 7032; l5123 = K[1869];
        at = 7033; l5121 = l5123;
      }
      at = 7034; l5124 = K[1870];
      at = 7035; l5125 = rt.sequence_get(l5124, l5121);
      at = 7036; l5120 = l5125;
    } else {
      at = 7037; l5126 = K[1871];
      at = 7038; l5120 = l5126;
    }
    at = 7039; markSet(rt, false, "circle", {at: l5104, radius: l5117, fill: l5120});
    at = 7040; if (rt.truthy(l5094)) {
      at = 7041; l5128 = field(rt, l5097, "points");
      at = 7042; l5127 = l5128;
    } else {
      at = 7043; l5129 = K[1872];
      at = 7044; l5127 = l5129;
    }
    at = 7045; l5130 = K[1873];
    at = 7046; l5131 = rt.sub(l5101, l5130);
    at = 7047; l5132 = K[1874];
    at = 7048; l5133 = rt.max(l5132, l5131);
    at = 7049; l5134 = K[1875];
    at = 7050; l5135 = rt.array_slice(l5127, l5134, l5133);
    at = 7051; if (rt.truthy(l5094)) {
      at = 7052; l5137 = field(rt, l5097, "points");
      at = 7053; l5136 = l5137;
    } else {
      at = 7054; l5138 = K[1876];
      at = 7055; l5136 = l5138;
    }
    at = 7056; l5139 = K[1877];
    at = 7057; l5140 = rt.gt(l5101, l5139);
    at = 7058; if (rt.truthy(l5140)) {
      at = 7059; l5142 = K[1878];
      at = 7060; l5141 = l5142;
    } else {
      at = 7061; l5143 = K[1879];
      at = 7062; l5141 = l5143;
    }
    at = 7063; l5144 = K[1880];
    at = 7064; l5145 = rt.sub(l5101, l5144);
    at = 7065; l5146 = K[1881];
    at = 7066; l5147 = rt.max(l5146, l5145);
    at = 7067; l5148 = rt.array_slice(l5136, l5141, l5147);
    at = 7068; if (rt.truthy(l5094)) {
      at = 7069; l5150 = field(rt, l5097, "color");
      at = 7070; l5149 = l5150;
    } else {
      at = 7071; l5151 = K[1882];
      at = 7072; l5149 = l5151;
    }
    at = 7073; l5152 = K[1883];
    at = 7074; l5153 = rt.sequence_get(l5152, l5149);
    at = 7075; if (rt.truthy(l5094)) {
      at = 7076; l5155 = field(rt, l5097, "width");
      at = 7077; l5154 = l5155;
    } else {
      at = 7078; l5156 = K[1884];
      at = 7079; l5154 = l5156;
    }
    at = 7080; markSet(rt, true, "line", {from: l5135, to: l5148, color: l5153, width: l5154});
    at = 7081; l5157 = field(rt, l15, "strokes");
    at = 7082; l5158 = rt.sequence_length(l5157);
    at = 7083; l5159 = K[1885];
    at = 7084; l5160 = rt.lt(l5159, l5158);
    at = 7085; if (rt.truthy(l5160)) {
      at = 7086; l5161 = field(rt, l15, "strokes");
      at = 7087; l5162 = K[1886];
      at = 7088; l5163 = rt.sequence_get(l5161, l5162);
    }
    at = 7089; if (rt.truthy(l5160)) {
      at = 7090; l5165 = field(rt, l5163, "points");
      at = 7091; l5164 = l5165;
    } else {
      at = 7092; l5166 = K[1887];
      at = 7093; l5164 = l5166;
    }
    at = 7094; l5167 = rt.array_count(l5164);
    at = 7095; l5168 = K[1888];
    at = 7096; l5169 = rt.gt(l5167, l5168);
    at = 7097; if (rt.truthy(l5169)) {
      at = 7098; if (rt.truthy(l5160)) {
        at = 7099; l5172 = field(rt, l5163, "points");
        at = 7100; l5171 = l5172;
      } else {
        at = 7101; l5173 = K[1889];
        at = 7102; l5171 = l5173;
      }
      at = 7103; l5174 = K[1890];
      at = 7104; l5175 = rt.array_nth(l5171, l5174);
      at = 7105; l5170 = l5175;
    } else {
      at = 7106; l5176 = K[1891];
      at = 7107; l5170 = l5176;
    }
    at = 7108; if (rt.truthy(l5160)) {
      at = 7109; l5178 = field(rt, l5163, "width");
      at = 7110; l5177 = l5178;
    } else {
      at = 7111; l5179 = K[1892];
      at = 7112; l5177 = l5179;
    }
    at = 7113; l5180 = K[1893];
    at = 7114; l5181 = rt.int_div(l5177, l5180);
    at = 7115; l5182 = K[1894];
    at = 7116; l5183 = rt.max(l5182, l5181);
    at = 7117; l5184 = K[1895];
    at = 7118; l5185 = rt.gt(l5167, l5184);
    at = 7119; if (rt.truthy(l5185)) {
      at = 7120; if (rt.truthy(l5160)) {
        at = 7121; l5188 = field(rt, l5163, "color");
        at = 7122; l5187 = l5188;
      } else {
        at = 7123; l5189 = K[1896];
        at = 7124; l5187 = l5189;
      }
      at = 7125; l5190 = K[1897];
      at = 7126; l5191 = rt.sequence_get(l5190, l5187);
      at = 7127; l5186 = l5191;
    } else {
      at = 7128; l5192 = K[1898];
      at = 7129; l5186 = l5192;
    }
    at = 7130; markSet(rt, false, "circle", {at: l5170, radius: l5183, fill: l5186});
    at = 7131; if (rt.truthy(l5160)) {
      at = 7132; l5194 = field(rt, l5163, "points");
      at = 7133; l5193 = l5194;
    } else {
      at = 7134; l5195 = K[1899];
      at = 7135; l5193 = l5195;
    }
    at = 7136; l5196 = K[1900];
    at = 7137; l5197 = rt.sub(l5167, l5196);
    at = 7138; l5198 = K[1901];
    at = 7139; l5199 = rt.max(l5198, l5197);
    at = 7140; l5200 = K[1902];
    at = 7141; l5201 = rt.array_slice(l5193, l5200, l5199);
    at = 7142; if (rt.truthy(l5160)) {
      at = 7143; l5203 = field(rt, l5163, "points");
      at = 7144; l5202 = l5203;
    } else {
      at = 7145; l5204 = K[1903];
      at = 7146; l5202 = l5204;
    }
    at = 7147; l5205 = K[1904];
    at = 7148; l5206 = rt.gt(l5167, l5205);
    at = 7149; if (rt.truthy(l5206)) {
      at = 7150; l5208 = K[1905];
      at = 7151; l5207 = l5208;
    } else {
      at = 7152; l5209 = K[1906];
      at = 7153; l5207 = l5209;
    }
    at = 7154; l5210 = K[1907];
    at = 7155; l5211 = rt.sub(l5167, l5210);
    at = 7156; l5212 = K[1908];
    at = 7157; l5213 = rt.max(l5212, l5211);
    at = 7158; l5214 = rt.array_slice(l5202, l5207, l5213);
    at = 7159; if (rt.truthy(l5160)) {
      at = 7160; l5216 = field(rt, l5163, "color");
      at = 7161; l5215 = l5216;
    } else {
      at = 7162; l5217 = K[1909];
      at = 7163; l5215 = l5217;
    }
    at = 7164; l5218 = K[1910];
    at = 7165; l5219 = rt.sequence_get(l5218, l5215);
    at = 7166; if (rt.truthy(l5160)) {
      at = 7167; l5221 = field(rt, l5163, "width");
      at = 7168; l5220 = l5221;
    } else {
      at = 7169; l5222 = K[1911];
      at = 7170; l5220 = l5222;
    }
    at = 7171; markSet(rt, true, "line", {from: l5201, to: l5214, color: l5219, width: l5220});
    at = 7172; l5223 = field(rt, l15, "strokes");
    at = 7173; l5224 = rt.sequence_length(l5223);
    at = 7174; l5225 = K[1912];
    at = 7175; l5226 = rt.lt(l5225, l5224);
    at = 7176; if (rt.truthy(l5226)) {
      at = 7177; l5227 = field(rt, l15, "strokes");
      at = 7178; l5228 = K[1913];
      at = 7179; l5229 = rt.sequence_get(l5227, l5228);
    }
    at = 7180; if (rt.truthy(l5226)) {
      at = 7181; l5231 = field(rt, l5229, "points");
      at = 7182; l5230 = l5231;
    } else {
      at = 7183; l5232 = K[1914];
      at = 7184; l5230 = l5232;
    }
    at = 7185; l5233 = rt.array_count(l5230);
    at = 7186; l5234 = K[1915];
    at = 7187; l5235 = rt.gt(l5233, l5234);
    at = 7188; if (rt.truthy(l5235)) {
      at = 7189; if (rt.truthy(l5226)) {
        at = 7190; l5238 = field(rt, l5229, "points");
        at = 7191; l5237 = l5238;
      } else {
        at = 7192; l5239 = K[1916];
        at = 7193; l5237 = l5239;
      }
      at = 7194; l5240 = K[1917];
      at = 7195; l5241 = rt.array_nth(l5237, l5240);
      at = 7196; l5236 = l5241;
    } else {
      at = 7197; l5242 = K[1918];
      at = 7198; l5236 = l5242;
    }
    at = 7199; if (rt.truthy(l5226)) {
      at = 7200; l5244 = field(rt, l5229, "width");
      at = 7201; l5243 = l5244;
    } else {
      at = 7202; l5245 = K[1919];
      at = 7203; l5243 = l5245;
    }
    at = 7204; l5246 = K[1920];
    at = 7205; l5247 = rt.int_div(l5243, l5246);
    at = 7206; l5248 = K[1921];
    at = 7207; l5249 = rt.max(l5248, l5247);
    at = 7208; l5250 = K[1922];
    at = 7209; l5251 = rt.gt(l5233, l5250);
    at = 7210; if (rt.truthy(l5251)) {
      at = 7211; if (rt.truthy(l5226)) {
        at = 7212; l5254 = field(rt, l5229, "color");
        at = 7213; l5253 = l5254;
      } else {
        at = 7214; l5255 = K[1923];
        at = 7215; l5253 = l5255;
      }
      at = 7216; l5256 = K[1924];
      at = 7217; l5257 = rt.sequence_get(l5256, l5253);
      at = 7218; l5252 = l5257;
    } else {
      at = 7219; l5258 = K[1925];
      at = 7220; l5252 = l5258;
    }
    at = 7221; markSet(rt, false, "circle", {at: l5236, radius: l5249, fill: l5252});
    at = 7222; if (rt.truthy(l5226)) {
      at = 7223; l5260 = field(rt, l5229, "points");
      at = 7224; l5259 = l5260;
    } else {
      at = 7225; l5261 = K[1926];
      at = 7226; l5259 = l5261;
    }
    at = 7227; l5262 = K[1927];
    at = 7228; l5263 = rt.sub(l5233, l5262);
    at = 7229; l5264 = K[1928];
    at = 7230; l5265 = rt.max(l5264, l5263);
    at = 7231; l5266 = K[1929];
    at = 7232; l5267 = rt.array_slice(l5259, l5266, l5265);
    at = 7233; if (rt.truthy(l5226)) {
      at = 7234; l5269 = field(rt, l5229, "points");
      at = 7235; l5268 = l5269;
    } else {
      at = 7236; l5270 = K[1930];
      at = 7237; l5268 = l5270;
    }
    at = 7238; l5271 = K[1931];
    at = 7239; l5272 = rt.gt(l5233, l5271);
    at = 7240; if (rt.truthy(l5272)) {
      at = 7241; l5274 = K[1932];
      at = 7242; l5273 = l5274;
    } else {
      at = 7243; l5275 = K[1933];
      at = 7244; l5273 = l5275;
    }
    at = 7245; l5276 = K[1934];
    at = 7246; l5277 = rt.sub(l5233, l5276);
    at = 7247; l5278 = K[1935];
    at = 7248; l5279 = rt.max(l5278, l5277);
    at = 7249; l5280 = rt.array_slice(l5268, l5273, l5279);
    at = 7250; if (rt.truthy(l5226)) {
      at = 7251; l5282 = field(rt, l5229, "color");
      at = 7252; l5281 = l5282;
    } else {
      at = 7253; l5283 = K[1936];
      at = 7254; l5281 = l5283;
    }
    at = 7255; l5284 = K[1937];
    at = 7256; l5285 = rt.sequence_get(l5284, l5281);
    at = 7257; if (rt.truthy(l5226)) {
      at = 7258; l5287 = field(rt, l5229, "width");
      at = 7259; l5286 = l5287;
    } else {
      at = 7260; l5288 = K[1938];
      at = 7261; l5286 = l5288;
    }
    at = 7262; markSet(rt, true, "line", {from: l5267, to: l5280, color: l5285, width: l5286});
    at = 7263; l5289 = field(rt, l15, "strokes");
    at = 7264; l5290 = rt.sequence_length(l5289);
    at = 7265; l5291 = K[1939];
    at = 7266; l5292 = rt.lt(l5291, l5290);
    at = 7267; if (rt.truthy(l5292)) {
      at = 7268; l5293 = field(rt, l15, "strokes");
      at = 7269; l5294 = K[1940];
      at = 7270; l5295 = rt.sequence_get(l5293, l5294);
    }
    at = 7271; if (rt.truthy(l5292)) {
      at = 7272; l5297 = field(rt, l5295, "points");
      at = 7273; l5296 = l5297;
    } else {
      at = 7274; l5298 = K[1941];
      at = 7275; l5296 = l5298;
    }
    at = 7276; l5299 = rt.array_count(l5296);
    at = 7277; l5300 = K[1942];
    at = 7278; l5301 = rt.gt(l5299, l5300);
    at = 7279; if (rt.truthy(l5301)) {
      at = 7280; if (rt.truthy(l5292)) {
        at = 7281; l5304 = field(rt, l5295, "points");
        at = 7282; l5303 = l5304;
      } else {
        at = 7283; l5305 = K[1943];
        at = 7284; l5303 = l5305;
      }
      at = 7285; l5306 = K[1944];
      at = 7286; l5307 = rt.array_nth(l5303, l5306);
      at = 7287; l5302 = l5307;
    } else {
      at = 7288; l5308 = K[1945];
      at = 7289; l5302 = l5308;
    }
    at = 7290; if (rt.truthy(l5292)) {
      at = 7291; l5310 = field(rt, l5295, "width");
      at = 7292; l5309 = l5310;
    } else {
      at = 7293; l5311 = K[1946];
      at = 7294; l5309 = l5311;
    }
    at = 7295; l5312 = K[1947];
    at = 7296; l5313 = rt.int_div(l5309, l5312);
    at = 7297; l5314 = K[1948];
    at = 7298; l5315 = rt.max(l5314, l5313);
    at = 7299; l5316 = K[1949];
    at = 7300; l5317 = rt.gt(l5299, l5316);
    at = 7301; if (rt.truthy(l5317)) {
      at = 7302; if (rt.truthy(l5292)) {
        at = 7303; l5320 = field(rt, l5295, "color");
        at = 7304; l5319 = l5320;
      } else {
        at = 7305; l5321 = K[1950];
        at = 7306; l5319 = l5321;
      }
      at = 7307; l5322 = K[1951];
      at = 7308; l5323 = rt.sequence_get(l5322, l5319);
      at = 7309; l5318 = l5323;
    } else {
      at = 7310; l5324 = K[1952];
      at = 7311; l5318 = l5324;
    }
    at = 7312; markSet(rt, false, "circle", {at: l5302, radius: l5315, fill: l5318});
    at = 7313; if (rt.truthy(l5292)) {
      at = 7314; l5326 = field(rt, l5295, "points");
      at = 7315; l5325 = l5326;
    } else {
      at = 7316; l5327 = K[1953];
      at = 7317; l5325 = l5327;
    }
    at = 7318; l5328 = K[1954];
    at = 7319; l5329 = rt.sub(l5299, l5328);
    at = 7320; l5330 = K[1955];
    at = 7321; l5331 = rt.max(l5330, l5329);
    at = 7322; l5332 = K[1956];
    at = 7323; l5333 = rt.array_slice(l5325, l5332, l5331);
    at = 7324; if (rt.truthy(l5292)) {
      at = 7325; l5335 = field(rt, l5295, "points");
      at = 7326; l5334 = l5335;
    } else {
      at = 7327; l5336 = K[1957];
      at = 7328; l5334 = l5336;
    }
    at = 7329; l5337 = K[1958];
    at = 7330; l5338 = rt.gt(l5299, l5337);
    at = 7331; if (rt.truthy(l5338)) {
      at = 7332; l5340 = K[1959];
      at = 7333; l5339 = l5340;
    } else {
      at = 7334; l5341 = K[1960];
      at = 7335; l5339 = l5341;
    }
    at = 7336; l5342 = K[1961];
    at = 7337; l5343 = rt.sub(l5299, l5342);
    at = 7338; l5344 = K[1962];
    at = 7339; l5345 = rt.max(l5344, l5343);
    at = 7340; l5346 = rt.array_slice(l5334, l5339, l5345);
    at = 7341; if (rt.truthy(l5292)) {
      at = 7342; l5348 = field(rt, l5295, "color");
      at = 7343; l5347 = l5348;
    } else {
      at = 7344; l5349 = K[1963];
      at = 7345; l5347 = l5349;
    }
    at = 7346; l5350 = K[1964];
    at = 7347; l5351 = rt.sequence_get(l5350, l5347);
    at = 7348; if (rt.truthy(l5292)) {
      at = 7349; l5353 = field(rt, l5295, "width");
      at = 7350; l5352 = l5353;
    } else {
      at = 7351; l5354 = K[1965];
      at = 7352; l5352 = l5354;
    }
    at = 7353; markSet(rt, true, "line", {from: l5333, to: l5346, color: l5351, width: l5352});
    at = 7354; l5355 = field(rt, l15, "strokes");
    at = 7355; l5356 = rt.sequence_length(l5355);
    at = 7356; l5357 = K[1966];
    at = 7357; l5358 = rt.lt(l5357, l5356);
    at = 7358; if (rt.truthy(l5358)) {
      at = 7359; l5359 = field(rt, l15, "strokes");
      at = 7360; l5360 = K[1967];
      at = 7361; l5361 = rt.sequence_get(l5359, l5360);
    }
    at = 7362; if (rt.truthy(l5358)) {
      at = 7363; l5363 = field(rt, l5361, "points");
      at = 7364; l5362 = l5363;
    } else {
      at = 7365; l5364 = K[1968];
      at = 7366; l5362 = l5364;
    }
    at = 7367; l5365 = rt.array_count(l5362);
    at = 7368; l5366 = K[1969];
    at = 7369; l5367 = rt.gt(l5365, l5366);
    at = 7370; if (rt.truthy(l5367)) {
      at = 7371; if (rt.truthy(l5358)) {
        at = 7372; l5370 = field(rt, l5361, "points");
        at = 7373; l5369 = l5370;
      } else {
        at = 7374; l5371 = K[1970];
        at = 7375; l5369 = l5371;
      }
      at = 7376; l5372 = K[1971];
      at = 7377; l5373 = rt.array_nth(l5369, l5372);
      at = 7378; l5368 = l5373;
    } else {
      at = 7379; l5374 = K[1972];
      at = 7380; l5368 = l5374;
    }
    at = 7381; if (rt.truthy(l5358)) {
      at = 7382; l5376 = field(rt, l5361, "width");
      at = 7383; l5375 = l5376;
    } else {
      at = 7384; l5377 = K[1973];
      at = 7385; l5375 = l5377;
    }
    at = 7386; l5378 = K[1974];
    at = 7387; l5379 = rt.int_div(l5375, l5378);
    at = 7388; l5380 = K[1975];
    at = 7389; l5381 = rt.max(l5380, l5379);
    at = 7390; l5382 = K[1976];
    at = 7391; l5383 = rt.gt(l5365, l5382);
    at = 7392; if (rt.truthy(l5383)) {
      at = 7393; if (rt.truthy(l5358)) {
        at = 7394; l5386 = field(rt, l5361, "color");
        at = 7395; l5385 = l5386;
      } else {
        at = 7396; l5387 = K[1977];
        at = 7397; l5385 = l5387;
      }
      at = 7398; l5388 = K[1978];
      at = 7399; l5389 = rt.sequence_get(l5388, l5385);
      at = 7400; l5384 = l5389;
    } else {
      at = 7401; l5390 = K[1979];
      at = 7402; l5384 = l5390;
    }
    at = 7403; markSet(rt, false, "circle", {at: l5368, radius: l5381, fill: l5384});
    at = 7404; if (rt.truthy(l5358)) {
      at = 7405; l5392 = field(rt, l5361, "points");
      at = 7406; l5391 = l5392;
    } else {
      at = 7407; l5393 = K[1980];
      at = 7408; l5391 = l5393;
    }
    at = 7409; l5394 = K[1981];
    at = 7410; l5395 = rt.sub(l5365, l5394);
    at = 7411; l5396 = K[1982];
    at = 7412; l5397 = rt.max(l5396, l5395);
    at = 7413; l5398 = K[1983];
    at = 7414; l5399 = rt.array_slice(l5391, l5398, l5397);
    at = 7415; if (rt.truthy(l5358)) {
      at = 7416; l5401 = field(rt, l5361, "points");
      at = 7417; l5400 = l5401;
    } else {
      at = 7418; l5402 = K[1984];
      at = 7419; l5400 = l5402;
    }
    at = 7420; l5403 = K[1985];
    at = 7421; l5404 = rt.gt(l5365, l5403);
    at = 7422; if (rt.truthy(l5404)) {
      at = 7423; l5406 = K[1986];
      at = 7424; l5405 = l5406;
    } else {
      at = 7425; l5407 = K[1987];
      at = 7426; l5405 = l5407;
    }
    at = 7427; l5408 = K[1988];
    at = 7428; l5409 = rt.sub(l5365, l5408);
    at = 7429; l5410 = K[1989];
    at = 7430; l5411 = rt.max(l5410, l5409);
    at = 7431; l5412 = rt.array_slice(l5400, l5405, l5411);
    at = 7432; if (rt.truthy(l5358)) {
      at = 7433; l5414 = field(rt, l5361, "color");
      at = 7434; l5413 = l5414;
    } else {
      at = 7435; l5415 = K[1990];
      at = 7436; l5413 = l5415;
    }
    at = 7437; l5416 = K[1991];
    at = 7438; l5417 = rt.sequence_get(l5416, l5413);
    at = 7439; if (rt.truthy(l5358)) {
      at = 7440; l5419 = field(rt, l5361, "width");
      at = 7441; l5418 = l5419;
    } else {
      at = 7442; l5420 = K[1992];
      at = 7443; l5418 = l5420;
    }
    at = 7444; markSet(rt, true, "line", {from: l5399, to: l5412, color: l5417, width: l5418});
    at = 7445; l5421 = field(rt, l15, "strokes");
    at = 7446; l5422 = rt.sequence_length(l5421);
    at = 7447; l5423 = K[1993];
    at = 7448; l5424 = rt.lt(l5423, l5422);
    at = 7449; if (rt.truthy(l5424)) {
      at = 7450; l5425 = field(rt, l15, "strokes");
      at = 7451; l5426 = K[1994];
      at = 7452; l5427 = rt.sequence_get(l5425, l5426);
    }
    at = 7453; if (rt.truthy(l5424)) {
      at = 7454; l5429 = field(rt, l5427, "points");
      at = 7455; l5428 = l5429;
    } else {
      at = 7456; l5430 = K[1995];
      at = 7457; l5428 = l5430;
    }
    at = 7458; l5431 = rt.array_count(l5428);
    at = 7459; l5432 = K[1996];
    at = 7460; l5433 = rt.gt(l5431, l5432);
    at = 7461; if (rt.truthy(l5433)) {
      at = 7462; if (rt.truthy(l5424)) {
        at = 7463; l5436 = field(rt, l5427, "points");
        at = 7464; l5435 = l5436;
      } else {
        at = 7465; l5437 = K[1997];
        at = 7466; l5435 = l5437;
      }
      at = 7467; l5438 = K[1998];
      at = 7468; l5439 = rt.array_nth(l5435, l5438);
      at = 7469; l5434 = l5439;
    } else {
      at = 7470; l5440 = K[1999];
      at = 7471; l5434 = l5440;
    }
    at = 7472; if (rt.truthy(l5424)) {
      at = 7473; l5442 = field(rt, l5427, "width");
      at = 7474; l5441 = l5442;
    } else {
      at = 7475; l5443 = K[2000];
      at = 7476; l5441 = l5443;
    }
    at = 7477; l5444 = K[2001];
    at = 7478; l5445 = rt.int_div(l5441, l5444);
    at = 7479; l5446 = K[2002];
    at = 7480; l5447 = rt.max(l5446, l5445);
    at = 7481; l5448 = K[2003];
    at = 7482; l5449 = rt.gt(l5431, l5448);
    at = 7483; if (rt.truthy(l5449)) {
      at = 7484; if (rt.truthy(l5424)) {
        at = 7485; l5452 = field(rt, l5427, "color");
        at = 7486; l5451 = l5452;
      } else {
        at = 7487; l5453 = K[2004];
        at = 7488; l5451 = l5453;
      }
      at = 7489; l5454 = K[2005];
      at = 7490; l5455 = rt.sequence_get(l5454, l5451);
      at = 7491; l5450 = l5455;
    } else {
      at = 7492; l5456 = K[2006];
      at = 7493; l5450 = l5456;
    }
    at = 7494; markSet(rt, false, "circle", {at: l5434, radius: l5447, fill: l5450});
    at = 7495; if (rt.truthy(l5424)) {
      at = 7496; l5458 = field(rt, l5427, "points");
      at = 7497; l5457 = l5458;
    } else {
      at = 7498; l5459 = K[2007];
      at = 7499; l5457 = l5459;
    }
    at = 7500; l5460 = K[2008];
    at = 7501; l5461 = rt.sub(l5431, l5460);
    at = 7502; l5462 = K[2009];
    at = 7503; l5463 = rt.max(l5462, l5461);
    at = 7504; l5464 = K[2010];
    at = 7505; l5465 = rt.array_slice(l5457, l5464, l5463);
    at = 7506; if (rt.truthy(l5424)) {
      at = 7507; l5467 = field(rt, l5427, "points");
      at = 7508; l5466 = l5467;
    } else {
      at = 7509; l5468 = K[2011];
      at = 7510; l5466 = l5468;
    }
    at = 7511; l5469 = K[2012];
    at = 7512; l5470 = rt.gt(l5431, l5469);
    at = 7513; if (rt.truthy(l5470)) {
      at = 7514; l5472 = K[2013];
      at = 7515; l5471 = l5472;
    } else {
      at = 7516; l5473 = K[2014];
      at = 7517; l5471 = l5473;
    }
    at = 7518; l5474 = K[2015];
    at = 7519; l5475 = rt.sub(l5431, l5474);
    at = 7520; l5476 = K[2016];
    at = 7521; l5477 = rt.max(l5476, l5475);
    at = 7522; l5478 = rt.array_slice(l5466, l5471, l5477);
    at = 7523; if (rt.truthy(l5424)) {
      at = 7524; l5480 = field(rt, l5427, "color");
      at = 7525; l5479 = l5480;
    } else {
      at = 7526; l5481 = K[2017];
      at = 7527; l5479 = l5481;
    }
    at = 7528; l5482 = K[2018];
    at = 7529; l5483 = rt.sequence_get(l5482, l5479);
    at = 7530; if (rt.truthy(l5424)) {
      at = 7531; l5485 = field(rt, l5427, "width");
      at = 7532; l5484 = l5485;
    } else {
      at = 7533; l5486 = K[2019];
      at = 7534; l5484 = l5486;
    }
    at = 7535; markSet(rt, true, "line", {from: l5465, to: l5478, color: l5483, width: l5484});
    at = 7536; l5487 = field(rt, l15, "strokes");
    at = 7537; l5488 = rt.sequence_length(l5487);
    at = 7538; l5489 = K[2020];
    at = 7539; l5490 = rt.lt(l5489, l5488);
    at = 7540; if (rt.truthy(l5490)) {
      at = 7541; l5491 = field(rt, l15, "strokes");
      at = 7542; l5492 = K[2021];
      at = 7543; l5493 = rt.sequence_get(l5491, l5492);
    }
    at = 7544; if (rt.truthy(l5490)) {
      at = 7545; l5495 = field(rt, l5493, "points");
      at = 7546; l5494 = l5495;
    } else {
      at = 7547; l5496 = K[2022];
      at = 7548; l5494 = l5496;
    }
    at = 7549; l5497 = rt.array_count(l5494);
    at = 7550; l5498 = K[2023];
    at = 7551; l5499 = rt.gt(l5497, l5498);
    at = 7552; if (rt.truthy(l5499)) {
      at = 7553; if (rt.truthy(l5490)) {
        at = 7554; l5502 = field(rt, l5493, "points");
        at = 7555; l5501 = l5502;
      } else {
        at = 7556; l5503 = K[2024];
        at = 7557; l5501 = l5503;
      }
      at = 7558; l5504 = K[2025];
      at = 7559; l5505 = rt.array_nth(l5501, l5504);
      at = 7560; l5500 = l5505;
    } else {
      at = 7561; l5506 = K[2026];
      at = 7562; l5500 = l5506;
    }
    at = 7563; if (rt.truthy(l5490)) {
      at = 7564; l5508 = field(rt, l5493, "width");
      at = 7565; l5507 = l5508;
    } else {
      at = 7566; l5509 = K[2027];
      at = 7567; l5507 = l5509;
    }
    at = 7568; l5510 = K[2028];
    at = 7569; l5511 = rt.int_div(l5507, l5510);
    at = 7570; l5512 = K[2029];
    at = 7571; l5513 = rt.max(l5512, l5511);
    at = 7572; l5514 = K[2030];
    at = 7573; l5515 = rt.gt(l5497, l5514);
    at = 7574; if (rt.truthy(l5515)) {
      at = 7575; if (rt.truthy(l5490)) {
        at = 7576; l5518 = field(rt, l5493, "color");
        at = 7577; l5517 = l5518;
      } else {
        at = 7578; l5519 = K[2031];
        at = 7579; l5517 = l5519;
      }
      at = 7580; l5520 = K[2032];
      at = 7581; l5521 = rt.sequence_get(l5520, l5517);
      at = 7582; l5516 = l5521;
    } else {
      at = 7583; l5522 = K[2033];
      at = 7584; l5516 = l5522;
    }
    at = 7585; markSet(rt, false, "circle", {at: l5500, radius: l5513, fill: l5516});
    at = 7586; if (rt.truthy(l5490)) {
      at = 7587; l5524 = field(rt, l5493, "points");
      at = 7588; l5523 = l5524;
    } else {
      at = 7589; l5525 = K[2034];
      at = 7590; l5523 = l5525;
    }
    at = 7591; l5526 = K[2035];
    at = 7592; l5527 = rt.sub(l5497, l5526);
    at = 7593; l5528 = K[2036];
    at = 7594; l5529 = rt.max(l5528, l5527);
    at = 7595; l5530 = K[2037];
    at = 7596; l5531 = rt.array_slice(l5523, l5530, l5529);
    at = 7597; if (rt.truthy(l5490)) {
      at = 7598; l5533 = field(rt, l5493, "points");
      at = 7599; l5532 = l5533;
    } else {
      at = 7600; l5534 = K[2038];
      at = 7601; l5532 = l5534;
    }
    at = 7602; l5535 = K[2039];
    at = 7603; l5536 = rt.gt(l5497, l5535);
    at = 7604; if (rt.truthy(l5536)) {
      at = 7605; l5538 = K[2040];
      at = 7606; l5537 = l5538;
    } else {
      at = 7607; l5539 = K[2041];
      at = 7608; l5537 = l5539;
    }
    at = 7609; l5540 = K[2042];
    at = 7610; l5541 = rt.sub(l5497, l5540);
    at = 7611; l5542 = K[2043];
    at = 7612; l5543 = rt.max(l5542, l5541);
    at = 7613; l5544 = rt.array_slice(l5532, l5537, l5543);
    at = 7614; if (rt.truthy(l5490)) {
      at = 7615; l5546 = field(rt, l5493, "color");
      at = 7616; l5545 = l5546;
    } else {
      at = 7617; l5547 = K[2044];
      at = 7618; l5545 = l5547;
    }
    at = 7619; l5548 = K[2045];
    at = 7620; l5549 = rt.sequence_get(l5548, l5545);
    at = 7621; if (rt.truthy(l5490)) {
      at = 7622; l5551 = field(rt, l5493, "width");
      at = 7623; l5550 = l5551;
    } else {
      at = 7624; l5552 = K[2046];
      at = 7625; l5550 = l5552;
    }
    at = 7626; markSet(rt, true, "line", {from: l5531, to: l5544, color: l5549, width: l5550});
    at = 7627; l5553 = field(rt, l15, "strokes");
    at = 7628; l5554 = rt.sequence_length(l5553);
    at = 7629; l5555 = K[2047];
    at = 7630; l5556 = rt.lt(l5555, l5554);
    at = 7631; if (rt.truthy(l5556)) {
      at = 7632; l5557 = field(rt, l15, "strokes");
      at = 7633; l5558 = K[2048];
      at = 7634; l5559 = rt.sequence_get(l5557, l5558);
    }
    at = 7635; if (rt.truthy(l5556)) {
      at = 7636; l5561 = field(rt, l5559, "points");
      at = 7637; l5560 = l5561;
    } else {
      at = 7638; l5562 = K[2049];
      at = 7639; l5560 = l5562;
    }
    at = 7640; l5563 = rt.array_count(l5560);
    at = 7641; l5564 = K[2050];
    at = 7642; l5565 = rt.gt(l5563, l5564);
    at = 7643; if (rt.truthy(l5565)) {
      at = 7644; if (rt.truthy(l5556)) {
        at = 7645; l5568 = field(rt, l5559, "points");
        at = 7646; l5567 = l5568;
      } else {
        at = 7647; l5569 = K[2051];
        at = 7648; l5567 = l5569;
      }
      at = 7649; l5570 = K[2052];
      at = 7650; l5571 = rt.array_nth(l5567, l5570);
      at = 7651; l5566 = l5571;
    } else {
      at = 7652; l5572 = K[2053];
      at = 7653; l5566 = l5572;
    }
    at = 7654; if (rt.truthy(l5556)) {
      at = 7655; l5574 = field(rt, l5559, "width");
      at = 7656; l5573 = l5574;
    } else {
      at = 7657; l5575 = K[2054];
      at = 7658; l5573 = l5575;
    }
    at = 7659; l5576 = K[2055];
    at = 7660; l5577 = rt.int_div(l5573, l5576);
    at = 7661; l5578 = K[2056];
    at = 7662; l5579 = rt.max(l5578, l5577);
    at = 7663; l5580 = K[2057];
    at = 7664; l5581 = rt.gt(l5563, l5580);
    at = 7665; if (rt.truthy(l5581)) {
      at = 7666; if (rt.truthy(l5556)) {
        at = 7667; l5584 = field(rt, l5559, "color");
        at = 7668; l5583 = l5584;
      } else {
        at = 7669; l5585 = K[2058];
        at = 7670; l5583 = l5585;
      }
      at = 7671; l5586 = K[2059];
      at = 7672; l5587 = rt.sequence_get(l5586, l5583);
      at = 7673; l5582 = l5587;
    } else {
      at = 7674; l5588 = K[2060];
      at = 7675; l5582 = l5588;
    }
    at = 7676; markSet(rt, false, "circle", {at: l5566, radius: l5579, fill: l5582});
    at = 7677; if (rt.truthy(l5556)) {
      at = 7678; l5590 = field(rt, l5559, "points");
      at = 7679; l5589 = l5590;
    } else {
      at = 7680; l5591 = K[2061];
      at = 7681; l5589 = l5591;
    }
    at = 7682; l5592 = K[2062];
    at = 7683; l5593 = rt.sub(l5563, l5592);
    at = 7684; l5594 = K[2063];
    at = 7685; l5595 = rt.max(l5594, l5593);
    at = 7686; l5596 = K[2064];
    at = 7687; l5597 = rt.array_slice(l5589, l5596, l5595);
    at = 7688; if (rt.truthy(l5556)) {
      at = 7689; l5599 = field(rt, l5559, "points");
      at = 7690; l5598 = l5599;
    } else {
      at = 7691; l5600 = K[2065];
      at = 7692; l5598 = l5600;
    }
    at = 7693; l5601 = K[2066];
    at = 7694; l5602 = rt.gt(l5563, l5601);
    at = 7695; if (rt.truthy(l5602)) {
      at = 7696; l5604 = K[2067];
      at = 7697; l5603 = l5604;
    } else {
      at = 7698; l5605 = K[2068];
      at = 7699; l5603 = l5605;
    }
    at = 7700; l5606 = K[2069];
    at = 7701; l5607 = rt.sub(l5563, l5606);
    at = 7702; l5608 = K[2070];
    at = 7703; l5609 = rt.max(l5608, l5607);
    at = 7704; l5610 = rt.array_slice(l5598, l5603, l5609);
    at = 7705; if (rt.truthy(l5556)) {
      at = 7706; l5612 = field(rt, l5559, "color");
      at = 7707; l5611 = l5612;
    } else {
      at = 7708; l5613 = K[2071];
      at = 7709; l5611 = l5613;
    }
    at = 7710; l5614 = K[2072];
    at = 7711; l5615 = rt.sequence_get(l5614, l5611);
    at = 7712; if (rt.truthy(l5556)) {
      at = 7713; l5617 = field(rt, l5559, "width");
      at = 7714; l5616 = l5617;
    } else {
      at = 7715; l5618 = K[2073];
      at = 7716; l5616 = l5618;
    }
    at = 7717; markSet(rt, true, "line", {from: l5597, to: l5610, color: l5615, width: l5616});
    at = 7718; l5619 = field(rt, l15, "strokes");
    at = 7719; l5620 = rt.sequence_length(l5619);
    at = 7720; l5621 = K[2074];
    at = 7721; l5622 = rt.lt(l5621, l5620);
    at = 7722; if (rt.truthy(l5622)) {
      at = 7723; l5623 = field(rt, l15, "strokes");
      at = 7724; l5624 = K[2075];
      at = 7725; l5625 = rt.sequence_get(l5623, l5624);
    }
    at = 7726; if (rt.truthy(l5622)) {
      at = 7727; l5627 = field(rt, l5625, "points");
      at = 7728; l5626 = l5627;
    } else {
      at = 7729; l5628 = K[2076];
      at = 7730; l5626 = l5628;
    }
    at = 7731; l5629 = rt.array_count(l5626);
    at = 7732; l5630 = K[2077];
    at = 7733; l5631 = rt.gt(l5629, l5630);
    at = 7734; if (rt.truthy(l5631)) {
      at = 7735; if (rt.truthy(l5622)) {
        at = 7736; l5634 = field(rt, l5625, "points");
        at = 7737; l5633 = l5634;
      } else {
        at = 7738; l5635 = K[2078];
        at = 7739; l5633 = l5635;
      }
      at = 7740; l5636 = K[2079];
      at = 7741; l5637 = rt.array_nth(l5633, l5636);
      at = 7742; l5632 = l5637;
    } else {
      at = 7743; l5638 = K[2080];
      at = 7744; l5632 = l5638;
    }
    at = 7745; if (rt.truthy(l5622)) {
      at = 7746; l5640 = field(rt, l5625, "width");
      at = 7747; l5639 = l5640;
    } else {
      at = 7748; l5641 = K[2081];
      at = 7749; l5639 = l5641;
    }
    at = 7750; l5642 = K[2082];
    at = 7751; l5643 = rt.int_div(l5639, l5642);
    at = 7752; l5644 = K[2083];
    at = 7753; l5645 = rt.max(l5644, l5643);
    at = 7754; l5646 = K[2084];
    at = 7755; l5647 = rt.gt(l5629, l5646);
    at = 7756; if (rt.truthy(l5647)) {
      at = 7757; if (rt.truthy(l5622)) {
        at = 7758; l5650 = field(rt, l5625, "color");
        at = 7759; l5649 = l5650;
      } else {
        at = 7760; l5651 = K[2085];
        at = 7761; l5649 = l5651;
      }
      at = 7762; l5652 = K[2086];
      at = 7763; l5653 = rt.sequence_get(l5652, l5649);
      at = 7764; l5648 = l5653;
    } else {
      at = 7765; l5654 = K[2087];
      at = 7766; l5648 = l5654;
    }
    at = 7767; markSet(rt, false, "circle", {at: l5632, radius: l5645, fill: l5648});
    at = 7768; if (rt.truthy(l5622)) {
      at = 7769; l5656 = field(rt, l5625, "points");
      at = 7770; l5655 = l5656;
    } else {
      at = 7771; l5657 = K[2088];
      at = 7772; l5655 = l5657;
    }
    at = 7773; l5658 = K[2089];
    at = 7774; l5659 = rt.sub(l5629, l5658);
    at = 7775; l5660 = K[2090];
    at = 7776; l5661 = rt.max(l5660, l5659);
    at = 7777; l5662 = K[2091];
    at = 7778; l5663 = rt.array_slice(l5655, l5662, l5661);
    at = 7779; if (rt.truthy(l5622)) {
      at = 7780; l5665 = field(rt, l5625, "points");
      at = 7781; l5664 = l5665;
    } else {
      at = 7782; l5666 = K[2092];
      at = 7783; l5664 = l5666;
    }
    at = 7784; l5667 = K[2093];
    at = 7785; l5668 = rt.gt(l5629, l5667);
    at = 7786; if (rt.truthy(l5668)) {
      at = 7787; l5670 = K[2094];
      at = 7788; l5669 = l5670;
    } else {
      at = 7789; l5671 = K[2095];
      at = 7790; l5669 = l5671;
    }
    at = 7791; l5672 = K[2096];
    at = 7792; l5673 = rt.sub(l5629, l5672);
    at = 7793; l5674 = K[2097];
    at = 7794; l5675 = rt.max(l5674, l5673);
    at = 7795; l5676 = rt.array_slice(l5664, l5669, l5675);
    at = 7796; if (rt.truthy(l5622)) {
      at = 7797; l5678 = field(rt, l5625, "color");
      at = 7798; l5677 = l5678;
    } else {
      at = 7799; l5679 = K[2098];
      at = 7800; l5677 = l5679;
    }
    at = 7801; l5680 = K[2099];
    at = 7802; l5681 = rt.sequence_get(l5680, l5677);
    at = 7803; if (rt.truthy(l5622)) {
      at = 7804; l5683 = field(rt, l5625, "width");
      at = 7805; l5682 = l5683;
    } else {
      at = 7806; l5684 = K[2100];
      at = 7807; l5682 = l5684;
    }
    at = 7808; markSet(rt, true, "line", {from: l5663, to: l5676, color: l5681, width: l5682});
    at = 7809; l5685 = field(rt, l15, "strokes");
    at = 7810; l5686 = rt.sequence_length(l5685);
    at = 7811; l5687 = K[2101];
    at = 7812; l5688 = rt.lt(l5687, l5686);
    at = 7813; if (rt.truthy(l5688)) {
      at = 7814; l5689 = field(rt, l15, "strokes");
      at = 7815; l5690 = K[2102];
      at = 7816; l5691 = rt.sequence_get(l5689, l5690);
    }
    at = 7817; if (rt.truthy(l5688)) {
      at = 7818; l5693 = field(rt, l5691, "points");
      at = 7819; l5692 = l5693;
    } else {
      at = 7820; l5694 = K[2103];
      at = 7821; l5692 = l5694;
    }
    at = 7822; l5695 = rt.array_count(l5692);
    at = 7823; l5696 = K[2104];
    at = 7824; l5697 = rt.gt(l5695, l5696);
    at = 7825; if (rt.truthy(l5697)) {
      at = 7826; if (rt.truthy(l5688)) {
        at = 7827; l5700 = field(rt, l5691, "points");
        at = 7828; l5699 = l5700;
      } else {
        at = 7829; l5701 = K[2105];
        at = 7830; l5699 = l5701;
      }
      at = 7831; l5702 = K[2106];
      at = 7832; l5703 = rt.array_nth(l5699, l5702);
      at = 7833; l5698 = l5703;
    } else {
      at = 7834; l5704 = K[2107];
      at = 7835; l5698 = l5704;
    }
    at = 7836; if (rt.truthy(l5688)) {
      at = 7837; l5706 = field(rt, l5691, "width");
      at = 7838; l5705 = l5706;
    } else {
      at = 7839; l5707 = K[2108];
      at = 7840; l5705 = l5707;
    }
    at = 7841; l5708 = K[2109];
    at = 7842; l5709 = rt.int_div(l5705, l5708);
    at = 7843; l5710 = K[2110];
    at = 7844; l5711 = rt.max(l5710, l5709);
    at = 7845; l5712 = K[2111];
    at = 7846; l5713 = rt.gt(l5695, l5712);
    at = 7847; if (rt.truthy(l5713)) {
      at = 7848; if (rt.truthy(l5688)) {
        at = 7849; l5716 = field(rt, l5691, "color");
        at = 7850; l5715 = l5716;
      } else {
        at = 7851; l5717 = K[2112];
        at = 7852; l5715 = l5717;
      }
      at = 7853; l5718 = K[2113];
      at = 7854; l5719 = rt.sequence_get(l5718, l5715);
      at = 7855; l5714 = l5719;
    } else {
      at = 7856; l5720 = K[2114];
      at = 7857; l5714 = l5720;
    }
    at = 7858; markSet(rt, false, "circle", {at: l5698, radius: l5711, fill: l5714});
    at = 7859; if (rt.truthy(l5688)) {
      at = 7860; l5722 = field(rt, l5691, "points");
      at = 7861; l5721 = l5722;
    } else {
      at = 7862; l5723 = K[2115];
      at = 7863; l5721 = l5723;
    }
    at = 7864; l5724 = K[2116];
    at = 7865; l5725 = rt.sub(l5695, l5724);
    at = 7866; l5726 = K[2117];
    at = 7867; l5727 = rt.max(l5726, l5725);
    at = 7868; l5728 = K[2118];
    at = 7869; l5729 = rt.array_slice(l5721, l5728, l5727);
    at = 7870; if (rt.truthy(l5688)) {
      at = 7871; l5731 = field(rt, l5691, "points");
      at = 7872; l5730 = l5731;
    } else {
      at = 7873; l5732 = K[2119];
      at = 7874; l5730 = l5732;
    }
    at = 7875; l5733 = K[2120];
    at = 7876; l5734 = rt.gt(l5695, l5733);
    at = 7877; if (rt.truthy(l5734)) {
      at = 7878; l5736 = K[2121];
      at = 7879; l5735 = l5736;
    } else {
      at = 7880; l5737 = K[2122];
      at = 7881; l5735 = l5737;
    }
    at = 7882; l5738 = K[2123];
    at = 7883; l5739 = rt.sub(l5695, l5738);
    at = 7884; l5740 = K[2124];
    at = 7885; l5741 = rt.max(l5740, l5739);
    at = 7886; l5742 = rt.array_slice(l5730, l5735, l5741);
    at = 7887; if (rt.truthy(l5688)) {
      at = 7888; l5744 = field(rt, l5691, "color");
      at = 7889; l5743 = l5744;
    } else {
      at = 7890; l5745 = K[2125];
      at = 7891; l5743 = l5745;
    }
    at = 7892; l5746 = K[2126];
    at = 7893; l5747 = rt.sequence_get(l5746, l5743);
    at = 7894; if (rt.truthy(l5688)) {
      at = 7895; l5749 = field(rt, l5691, "width");
      at = 7896; l5748 = l5749;
    } else {
      at = 7897; l5750 = K[2127];
      at = 7898; l5748 = l5750;
    }
    at = 7899; markSet(rt, true, "line", {from: l5729, to: l5742, color: l5747, width: l5748});
    at = 7900; l5751 = field(rt, l15, "strokes");
    at = 7901; l5752 = rt.sequence_length(l5751);
    at = 7902; l5753 = K[2128];
    at = 7903; l5754 = rt.lt(l5753, l5752);
    at = 7904; if (rt.truthy(l5754)) {
      at = 7905; l5755 = field(rt, l15, "strokes");
      at = 7906; l5756 = K[2129];
      at = 7907; l5757 = rt.sequence_get(l5755, l5756);
    }
    at = 7908; if (rt.truthy(l5754)) {
      at = 7909; l5759 = field(rt, l5757, "points");
      at = 7910; l5758 = l5759;
    } else {
      at = 7911; l5760 = K[2130];
      at = 7912; l5758 = l5760;
    }
    at = 7913; l5761 = rt.array_count(l5758);
    at = 7914; l5762 = K[2131];
    at = 7915; l5763 = rt.gt(l5761, l5762);
    at = 7916; if (rt.truthy(l5763)) {
      at = 7917; if (rt.truthy(l5754)) {
        at = 7918; l5766 = field(rt, l5757, "points");
        at = 7919; l5765 = l5766;
      } else {
        at = 7920; l5767 = K[2132];
        at = 7921; l5765 = l5767;
      }
      at = 7922; l5768 = K[2133];
      at = 7923; l5769 = rt.array_nth(l5765, l5768);
      at = 7924; l5764 = l5769;
    } else {
      at = 7925; l5770 = K[2134];
      at = 7926; l5764 = l5770;
    }
    at = 7927; if (rt.truthy(l5754)) {
      at = 7928; l5772 = field(rt, l5757, "width");
      at = 7929; l5771 = l5772;
    } else {
      at = 7930; l5773 = K[2135];
      at = 7931; l5771 = l5773;
    }
    at = 7932; l5774 = K[2136];
    at = 7933; l5775 = rt.int_div(l5771, l5774);
    at = 7934; l5776 = K[2137];
    at = 7935; l5777 = rt.max(l5776, l5775);
    at = 7936; l5778 = K[2138];
    at = 7937; l5779 = rt.gt(l5761, l5778);
    at = 7938; if (rt.truthy(l5779)) {
      at = 7939; if (rt.truthy(l5754)) {
        at = 7940; l5782 = field(rt, l5757, "color");
        at = 7941; l5781 = l5782;
      } else {
        at = 7942; l5783 = K[2139];
        at = 7943; l5781 = l5783;
      }
      at = 7944; l5784 = K[2140];
      at = 7945; l5785 = rt.sequence_get(l5784, l5781);
      at = 7946; l5780 = l5785;
    } else {
      at = 7947; l5786 = K[2141];
      at = 7948; l5780 = l5786;
    }
    at = 7949; markSet(rt, false, "circle", {at: l5764, radius: l5777, fill: l5780});
    at = 7950; if (rt.truthy(l5754)) {
      at = 7951; l5788 = field(rt, l5757, "points");
      at = 7952; l5787 = l5788;
    } else {
      at = 7953; l5789 = K[2142];
      at = 7954; l5787 = l5789;
    }
    at = 7955; l5790 = K[2143];
    at = 7956; l5791 = rt.sub(l5761, l5790);
    at = 7957; l5792 = K[2144];
    at = 7958; l5793 = rt.max(l5792, l5791);
    at = 7959; l5794 = K[2145];
    at = 7960; l5795 = rt.array_slice(l5787, l5794, l5793);
    at = 7961; if (rt.truthy(l5754)) {
      at = 7962; l5797 = field(rt, l5757, "points");
      at = 7963; l5796 = l5797;
    } else {
      at = 7964; l5798 = K[2146];
      at = 7965; l5796 = l5798;
    }
    at = 7966; l5799 = K[2147];
    at = 7967; l5800 = rt.gt(l5761, l5799);
    at = 7968; if (rt.truthy(l5800)) {
      at = 7969; l5802 = K[2148];
      at = 7970; l5801 = l5802;
    } else {
      at = 7971; l5803 = K[2149];
      at = 7972; l5801 = l5803;
    }
    at = 7973; l5804 = K[2150];
    at = 7974; l5805 = rt.sub(l5761, l5804);
    at = 7975; l5806 = K[2151];
    at = 7976; l5807 = rt.max(l5806, l5805);
    at = 7977; l5808 = rt.array_slice(l5796, l5801, l5807);
    at = 7978; if (rt.truthy(l5754)) {
      at = 7979; l5810 = field(rt, l5757, "color");
      at = 7980; l5809 = l5810;
    } else {
      at = 7981; l5811 = K[2152];
      at = 7982; l5809 = l5811;
    }
    at = 7983; l5812 = K[2153];
    at = 7984; l5813 = rt.sequence_get(l5812, l5809);
    at = 7985; if (rt.truthy(l5754)) {
      at = 7986; l5815 = field(rt, l5757, "width");
      at = 7987; l5814 = l5815;
    } else {
      at = 7988; l5816 = K[2154];
      at = 7989; l5814 = l5816;
    }
    at = 7990; markSet(rt, true, "line", {from: l5795, to: l5808, color: l5813, width: l5814});
    at = 7991; l5817 = field(rt, l15, "strokes");
    at = 7992; l5818 = rt.sequence_length(l5817);
    at = 7993; l5819 = K[2155];
    at = 7994; l5820 = rt.lt(l5819, l5818);
    at = 7995; if (rt.truthy(l5820)) {
      at = 7996; l5821 = field(rt, l15, "strokes");
      at = 7997; l5822 = K[2156];
      at = 7998; l5823 = rt.sequence_get(l5821, l5822);
    }
    at = 7999; if (rt.truthy(l5820)) {
      at = 8000; l5825 = field(rt, l5823, "points");
      at = 8001; l5824 = l5825;
    } else {
      at = 8002; l5826 = K[2157];
      at = 8003; l5824 = l5826;
    }
    at = 8004; l5827 = rt.array_count(l5824);
    at = 8005; l5828 = K[2158];
    at = 8006; l5829 = rt.gt(l5827, l5828);
    at = 8007; if (rt.truthy(l5829)) {
      at = 8008; if (rt.truthy(l5820)) {
        at = 8009; l5832 = field(rt, l5823, "points");
        at = 8010; l5831 = l5832;
      } else {
        at = 8011; l5833 = K[2159];
        at = 8012; l5831 = l5833;
      }
      at = 8013; l5834 = K[2160];
      at = 8014; l5835 = rt.array_nth(l5831, l5834);
      at = 8015; l5830 = l5835;
    } else {
      at = 8016; l5836 = K[2161];
      at = 8017; l5830 = l5836;
    }
    at = 8018; if (rt.truthy(l5820)) {
      at = 8019; l5838 = field(rt, l5823, "width");
      at = 8020; l5837 = l5838;
    } else {
      at = 8021; l5839 = K[2162];
      at = 8022; l5837 = l5839;
    }
    at = 8023; l5840 = K[2163];
    at = 8024; l5841 = rt.int_div(l5837, l5840);
    at = 8025; l5842 = K[2164];
    at = 8026; l5843 = rt.max(l5842, l5841);
    at = 8027; l5844 = K[2165];
    at = 8028; l5845 = rt.gt(l5827, l5844);
    at = 8029; if (rt.truthy(l5845)) {
      at = 8030; if (rt.truthy(l5820)) {
        at = 8031; l5848 = field(rt, l5823, "color");
        at = 8032; l5847 = l5848;
      } else {
        at = 8033; l5849 = K[2166];
        at = 8034; l5847 = l5849;
      }
      at = 8035; l5850 = K[2167];
      at = 8036; l5851 = rt.sequence_get(l5850, l5847);
      at = 8037; l5846 = l5851;
    } else {
      at = 8038; l5852 = K[2168];
      at = 8039; l5846 = l5852;
    }
    at = 8040; markSet(rt, false, "circle", {at: l5830, radius: l5843, fill: l5846});
    at = 8041; if (rt.truthy(l5820)) {
      at = 8042; l5854 = field(rt, l5823, "points");
      at = 8043; l5853 = l5854;
    } else {
      at = 8044; l5855 = K[2169];
      at = 8045; l5853 = l5855;
    }
    at = 8046; l5856 = K[2170];
    at = 8047; l5857 = rt.sub(l5827, l5856);
    at = 8048; l5858 = K[2171];
    at = 8049; l5859 = rt.max(l5858, l5857);
    at = 8050; l5860 = K[2172];
    at = 8051; l5861 = rt.array_slice(l5853, l5860, l5859);
    at = 8052; if (rt.truthy(l5820)) {
      at = 8053; l5863 = field(rt, l5823, "points");
      at = 8054; l5862 = l5863;
    } else {
      at = 8055; l5864 = K[2173];
      at = 8056; l5862 = l5864;
    }
    at = 8057; l5865 = K[2174];
    at = 8058; l5866 = rt.gt(l5827, l5865);
    at = 8059; if (rt.truthy(l5866)) {
      at = 8060; l5868 = K[2175];
      at = 8061; l5867 = l5868;
    } else {
      at = 8062; l5869 = K[2176];
      at = 8063; l5867 = l5869;
    }
    at = 8064; l5870 = K[2177];
    at = 8065; l5871 = rt.sub(l5827, l5870);
    at = 8066; l5872 = K[2178];
    at = 8067; l5873 = rt.max(l5872, l5871);
    at = 8068; l5874 = rt.array_slice(l5862, l5867, l5873);
    at = 8069; if (rt.truthy(l5820)) {
      at = 8070; l5876 = field(rt, l5823, "color");
      at = 8071; l5875 = l5876;
    } else {
      at = 8072; l5877 = K[2179];
      at = 8073; l5875 = l5877;
    }
    at = 8074; l5878 = K[2180];
    at = 8075; l5879 = rt.sequence_get(l5878, l5875);
    at = 8076; if (rt.truthy(l5820)) {
      at = 8077; l5881 = field(rt, l5823, "width");
      at = 8078; l5880 = l5881;
    } else {
      at = 8079; l5882 = K[2181];
      at = 8080; l5880 = l5882;
    }
    at = 8081; markSet(rt, true, "line", {from: l5861, to: l5874, color: l5879, width: l5880});
    at = 8082; l5883 = field(rt, l15, "strokes");
    at = 8083; l5884 = rt.sequence_length(l5883);
    at = 8084; l5885 = K[2182];
    at = 8085; l5886 = rt.lt(l5885, l5884);
    at = 8086; if (rt.truthy(l5886)) {
      at = 8087; l5887 = field(rt, l15, "strokes");
      at = 8088; l5888 = K[2183];
      at = 8089; l5889 = rt.sequence_get(l5887, l5888);
    }
    at = 8090; if (rt.truthy(l5886)) {
      at = 8091; l5891 = field(rt, l5889, "points");
      at = 8092; l5890 = l5891;
    } else {
      at = 8093; l5892 = K[2184];
      at = 8094; l5890 = l5892;
    }
    at = 8095; l5893 = rt.array_count(l5890);
    at = 8096; l5894 = K[2185];
    at = 8097; l5895 = rt.gt(l5893, l5894);
    at = 8098; if (rt.truthy(l5895)) {
      at = 8099; if (rt.truthy(l5886)) {
        at = 8100; l5898 = field(rt, l5889, "points");
        at = 8101; l5897 = l5898;
      } else {
        at = 8102; l5899 = K[2186];
        at = 8103; l5897 = l5899;
      }
      at = 8104; l5900 = K[2187];
      at = 8105; l5901 = rt.array_nth(l5897, l5900);
      at = 8106; l5896 = l5901;
    } else {
      at = 8107; l5902 = K[2188];
      at = 8108; l5896 = l5902;
    }
    at = 8109; if (rt.truthy(l5886)) {
      at = 8110; l5904 = field(rt, l5889, "width");
      at = 8111; l5903 = l5904;
    } else {
      at = 8112; l5905 = K[2189];
      at = 8113; l5903 = l5905;
    }
    at = 8114; l5906 = K[2190];
    at = 8115; l5907 = rt.int_div(l5903, l5906);
    at = 8116; l5908 = K[2191];
    at = 8117; l5909 = rt.max(l5908, l5907);
    at = 8118; l5910 = K[2192];
    at = 8119; l5911 = rt.gt(l5893, l5910);
    at = 8120; if (rt.truthy(l5911)) {
      at = 8121; if (rt.truthy(l5886)) {
        at = 8122; l5914 = field(rt, l5889, "color");
        at = 8123; l5913 = l5914;
      } else {
        at = 8124; l5915 = K[2193];
        at = 8125; l5913 = l5915;
      }
      at = 8126; l5916 = K[2194];
      at = 8127; l5917 = rt.sequence_get(l5916, l5913);
      at = 8128; l5912 = l5917;
    } else {
      at = 8129; l5918 = K[2195];
      at = 8130; l5912 = l5918;
    }
    at = 8131; markSet(rt, false, "circle", {at: l5896, radius: l5909, fill: l5912});
    at = 8132; if (rt.truthy(l5886)) {
      at = 8133; l5920 = field(rt, l5889, "points");
      at = 8134; l5919 = l5920;
    } else {
      at = 8135; l5921 = K[2196];
      at = 8136; l5919 = l5921;
    }
    at = 8137; l5922 = K[2197];
    at = 8138; l5923 = rt.sub(l5893, l5922);
    at = 8139; l5924 = K[2198];
    at = 8140; l5925 = rt.max(l5924, l5923);
    at = 8141; l5926 = K[2199];
    at = 8142; l5927 = rt.array_slice(l5919, l5926, l5925);
    at = 8143; if (rt.truthy(l5886)) {
      at = 8144; l5929 = field(rt, l5889, "points");
      at = 8145; l5928 = l5929;
    } else {
      at = 8146; l5930 = K[2200];
      at = 8147; l5928 = l5930;
    }
    at = 8148; l5931 = K[2201];
    at = 8149; l5932 = rt.gt(l5893, l5931);
    at = 8150; if (rt.truthy(l5932)) {
      at = 8151; l5934 = K[2202];
      at = 8152; l5933 = l5934;
    } else {
      at = 8153; l5935 = K[2203];
      at = 8154; l5933 = l5935;
    }
    at = 8155; l5936 = K[2204];
    at = 8156; l5937 = rt.sub(l5893, l5936);
    at = 8157; l5938 = K[2205];
    at = 8158; l5939 = rt.max(l5938, l5937);
    at = 8159; l5940 = rt.array_slice(l5928, l5933, l5939);
    at = 8160; if (rt.truthy(l5886)) {
      at = 8161; l5942 = field(rt, l5889, "color");
      at = 8162; l5941 = l5942;
    } else {
      at = 8163; l5943 = K[2206];
      at = 8164; l5941 = l5943;
    }
    at = 8165; l5944 = K[2207];
    at = 8166; l5945 = rt.sequence_get(l5944, l5941);
    at = 8167; if (rt.truthy(l5886)) {
      at = 8168; l5947 = field(rt, l5889, "width");
      at = 8169; l5946 = l5947;
    } else {
      at = 8170; l5948 = K[2208];
      at = 8171; l5946 = l5948;
    }
    at = 8172; markSet(rt, true, "line", {from: l5927, to: l5940, color: l5945, width: l5946});
    at = 8173; l5949 = field(rt, l15, "strokes");
    at = 8174; l5950 = rt.sequence_length(l5949);
    at = 8175; l5951 = K[2209];
    at = 8176; l5952 = rt.lt(l5951, l5950);
    at = 8177; if (rt.truthy(l5952)) {
      at = 8178; l5953 = field(rt, l15, "strokes");
      at = 8179; l5954 = K[2210];
      at = 8180; l5955 = rt.sequence_get(l5953, l5954);
    }
    at = 8181; if (rt.truthy(l5952)) {
      at = 8182; l5957 = field(rt, l5955, "points");
      at = 8183; l5956 = l5957;
    } else {
      at = 8184; l5958 = K[2211];
      at = 8185; l5956 = l5958;
    }
    at = 8186; l5959 = rt.array_count(l5956);
    at = 8187; l5960 = K[2212];
    at = 8188; l5961 = rt.gt(l5959, l5960);
    at = 8189; if (rt.truthy(l5961)) {
      at = 8190; if (rt.truthy(l5952)) {
        at = 8191; l5964 = field(rt, l5955, "points");
        at = 8192; l5963 = l5964;
      } else {
        at = 8193; l5965 = K[2213];
        at = 8194; l5963 = l5965;
      }
      at = 8195; l5966 = K[2214];
      at = 8196; l5967 = rt.array_nth(l5963, l5966);
      at = 8197; l5962 = l5967;
    } else {
      at = 8198; l5968 = K[2215];
      at = 8199; l5962 = l5968;
    }
    at = 8200; if (rt.truthy(l5952)) {
      at = 8201; l5970 = field(rt, l5955, "width");
      at = 8202; l5969 = l5970;
    } else {
      at = 8203; l5971 = K[2216];
      at = 8204; l5969 = l5971;
    }
    at = 8205; l5972 = K[2217];
    at = 8206; l5973 = rt.int_div(l5969, l5972);
    at = 8207; l5974 = K[2218];
    at = 8208; l5975 = rt.max(l5974, l5973);
    at = 8209; l5976 = K[2219];
    at = 8210; l5977 = rt.gt(l5959, l5976);
    at = 8211; if (rt.truthy(l5977)) {
      at = 8212; if (rt.truthy(l5952)) {
        at = 8213; l5980 = field(rt, l5955, "color");
        at = 8214; l5979 = l5980;
      } else {
        at = 8215; l5981 = K[2220];
        at = 8216; l5979 = l5981;
      }
      at = 8217; l5982 = K[2221];
      at = 8218; l5983 = rt.sequence_get(l5982, l5979);
      at = 8219; l5978 = l5983;
    } else {
      at = 8220; l5984 = K[2222];
      at = 8221; l5978 = l5984;
    }
    at = 8222; markSet(rt, false, "circle", {at: l5962, radius: l5975, fill: l5978});
    at = 8223; if (rt.truthy(l5952)) {
      at = 8224; l5986 = field(rt, l5955, "points");
      at = 8225; l5985 = l5986;
    } else {
      at = 8226; l5987 = K[2223];
      at = 8227; l5985 = l5987;
    }
    at = 8228; l5988 = K[2224];
    at = 8229; l5989 = rt.sub(l5959, l5988);
    at = 8230; l5990 = K[2225];
    at = 8231; l5991 = rt.max(l5990, l5989);
    at = 8232; l5992 = K[2226];
    at = 8233; l5993 = rt.array_slice(l5985, l5992, l5991);
    at = 8234; if (rt.truthy(l5952)) {
      at = 8235; l5995 = field(rt, l5955, "points");
      at = 8236; l5994 = l5995;
    } else {
      at = 8237; l5996 = K[2227];
      at = 8238; l5994 = l5996;
    }
    at = 8239; l5997 = K[2228];
    at = 8240; l5998 = rt.gt(l5959, l5997);
    at = 8241; if (rt.truthy(l5998)) {
      at = 8242; l6000 = K[2229];
      at = 8243; l5999 = l6000;
    } else {
      at = 8244; l6001 = K[2230];
      at = 8245; l5999 = l6001;
    }
    at = 8246; l6002 = K[2231];
    at = 8247; l6003 = rt.sub(l5959, l6002);
    at = 8248; l6004 = K[2232];
    at = 8249; l6005 = rt.max(l6004, l6003);
    at = 8250; l6006 = rt.array_slice(l5994, l5999, l6005);
    at = 8251; if (rt.truthy(l5952)) {
      at = 8252; l6008 = field(rt, l5955, "color");
      at = 8253; l6007 = l6008;
    } else {
      at = 8254; l6009 = K[2233];
      at = 8255; l6007 = l6009;
    }
    at = 8256; l6010 = K[2234];
    at = 8257; l6011 = rt.sequence_get(l6010, l6007);
    at = 8258; if (rt.truthy(l5952)) {
      at = 8259; l6013 = field(rt, l5955, "width");
      at = 8260; l6012 = l6013;
    } else {
      at = 8261; l6014 = K[2235];
      at = 8262; l6012 = l6014;
    }
    at = 8263; markSet(rt, true, "line", {from: l5993, to: l6006, color: l6011, width: l6012});
    at = 8264; l6015 = field(rt, l15, "strokes");
    at = 8265; l6016 = rt.sequence_length(l6015);
    at = 8266; l6017 = K[2236];
    at = 8267; l6018 = rt.lt(l6017, l6016);
    at = 8268; if (rt.truthy(l6018)) {
      at = 8269; l6019 = field(rt, l15, "strokes");
      at = 8270; l6020 = K[2237];
      at = 8271; l6021 = rt.sequence_get(l6019, l6020);
    }
    at = 8272; if (rt.truthy(l6018)) {
      at = 8273; l6023 = field(rt, l6021, "points");
      at = 8274; l6022 = l6023;
    } else {
      at = 8275; l6024 = K[2238];
      at = 8276; l6022 = l6024;
    }
    at = 8277; l6025 = rt.array_count(l6022);
    at = 8278; l6026 = K[2239];
    at = 8279; l6027 = rt.gt(l6025, l6026);
    at = 8280; if (rt.truthy(l6027)) {
      at = 8281; if (rt.truthy(l6018)) {
        at = 8282; l6030 = field(rt, l6021, "points");
        at = 8283; l6029 = l6030;
      } else {
        at = 8284; l6031 = K[2240];
        at = 8285; l6029 = l6031;
      }
      at = 8286; l6032 = K[2241];
      at = 8287; l6033 = rt.array_nth(l6029, l6032);
      at = 8288; l6028 = l6033;
    } else {
      at = 8289; l6034 = K[2242];
      at = 8290; l6028 = l6034;
    }
    at = 8291; if (rt.truthy(l6018)) {
      at = 8292; l6036 = field(rt, l6021, "width");
      at = 8293; l6035 = l6036;
    } else {
      at = 8294; l6037 = K[2243];
      at = 8295; l6035 = l6037;
    }
    at = 8296; l6038 = K[2244];
    at = 8297; l6039 = rt.int_div(l6035, l6038);
    at = 8298; l6040 = K[2245];
    at = 8299; l6041 = rt.max(l6040, l6039);
    at = 8300; l6042 = K[2246];
    at = 8301; l6043 = rt.gt(l6025, l6042);
    at = 8302; if (rt.truthy(l6043)) {
      at = 8303; if (rt.truthy(l6018)) {
        at = 8304; l6046 = field(rt, l6021, "color");
        at = 8305; l6045 = l6046;
      } else {
        at = 8306; l6047 = K[2247];
        at = 8307; l6045 = l6047;
      }
      at = 8308; l6048 = K[2248];
      at = 8309; l6049 = rt.sequence_get(l6048, l6045);
      at = 8310; l6044 = l6049;
    } else {
      at = 8311; l6050 = K[2249];
      at = 8312; l6044 = l6050;
    }
    at = 8313; markSet(rt, false, "circle", {at: l6028, radius: l6041, fill: l6044});
    at = 8314; if (rt.truthy(l6018)) {
      at = 8315; l6052 = field(rt, l6021, "points");
      at = 8316; l6051 = l6052;
    } else {
      at = 8317; l6053 = K[2250];
      at = 8318; l6051 = l6053;
    }
    at = 8319; l6054 = K[2251];
    at = 8320; l6055 = rt.sub(l6025, l6054);
    at = 8321; l6056 = K[2252];
    at = 8322; l6057 = rt.max(l6056, l6055);
    at = 8323; l6058 = K[2253];
    at = 8324; l6059 = rt.array_slice(l6051, l6058, l6057);
    at = 8325; if (rt.truthy(l6018)) {
      at = 8326; l6061 = field(rt, l6021, "points");
      at = 8327; l6060 = l6061;
    } else {
      at = 8328; l6062 = K[2254];
      at = 8329; l6060 = l6062;
    }
    at = 8330; l6063 = K[2255];
    at = 8331; l6064 = rt.gt(l6025, l6063);
    at = 8332; if (rt.truthy(l6064)) {
      at = 8333; l6066 = K[2256];
      at = 8334; l6065 = l6066;
    } else {
      at = 8335; l6067 = K[2257];
      at = 8336; l6065 = l6067;
    }
    at = 8337; l6068 = K[2258];
    at = 8338; l6069 = rt.sub(l6025, l6068);
    at = 8339; l6070 = K[2259];
    at = 8340; l6071 = rt.max(l6070, l6069);
    at = 8341; l6072 = rt.array_slice(l6060, l6065, l6071);
    at = 8342; if (rt.truthy(l6018)) {
      at = 8343; l6074 = field(rt, l6021, "color");
      at = 8344; l6073 = l6074;
    } else {
      at = 8345; l6075 = K[2260];
      at = 8346; l6073 = l6075;
    }
    at = 8347; l6076 = K[2261];
    at = 8348; l6077 = rt.sequence_get(l6076, l6073);
    at = 8349; if (rt.truthy(l6018)) {
      at = 8350; l6079 = field(rt, l6021, "width");
      at = 8351; l6078 = l6079;
    } else {
      at = 8352; l6080 = K[2262];
      at = 8353; l6078 = l6080;
    }
    at = 8354; markSet(rt, true, "line", {from: l6059, to: l6072, color: l6077, width: l6078});
    at = 8355; l6081 = field(rt, l15, "strokes");
    at = 8356; l6082 = rt.sequence_length(l6081);
    at = 8357; l6083 = K[2263];
    at = 8358; l6084 = rt.lt(l6083, l6082);
    at = 8359; if (rt.truthy(l6084)) {
      at = 8360; l6085 = field(rt, l15, "strokes");
      at = 8361; l6086 = K[2264];
      at = 8362; l6087 = rt.sequence_get(l6085, l6086);
    }
    at = 8363; if (rt.truthy(l6084)) {
      at = 8364; l6089 = field(rt, l6087, "points");
      at = 8365; l6088 = l6089;
    } else {
      at = 8366; l6090 = K[2265];
      at = 8367; l6088 = l6090;
    }
    at = 8368; l6091 = rt.array_count(l6088);
    at = 8369; l6092 = K[2266];
    at = 8370; l6093 = rt.gt(l6091, l6092);
    at = 8371; if (rt.truthy(l6093)) {
      at = 8372; if (rt.truthy(l6084)) {
        at = 8373; l6096 = field(rt, l6087, "points");
        at = 8374; l6095 = l6096;
      } else {
        at = 8375; l6097 = K[2267];
        at = 8376; l6095 = l6097;
      }
      at = 8377; l6098 = K[2268];
      at = 8378; l6099 = rt.array_nth(l6095, l6098);
      at = 8379; l6094 = l6099;
    } else {
      at = 8380; l6100 = K[2269];
      at = 8381; l6094 = l6100;
    }
    at = 8382; if (rt.truthy(l6084)) {
      at = 8383; l6102 = field(rt, l6087, "width");
      at = 8384; l6101 = l6102;
    } else {
      at = 8385; l6103 = K[2270];
      at = 8386; l6101 = l6103;
    }
    at = 8387; l6104 = K[2271];
    at = 8388; l6105 = rt.int_div(l6101, l6104);
    at = 8389; l6106 = K[2272];
    at = 8390; l6107 = rt.max(l6106, l6105);
    at = 8391; l6108 = K[2273];
    at = 8392; l6109 = rt.gt(l6091, l6108);
    at = 8393; if (rt.truthy(l6109)) {
      at = 8394; if (rt.truthy(l6084)) {
        at = 8395; l6112 = field(rt, l6087, "color");
        at = 8396; l6111 = l6112;
      } else {
        at = 8397; l6113 = K[2274];
        at = 8398; l6111 = l6113;
      }
      at = 8399; l6114 = K[2275];
      at = 8400; l6115 = rt.sequence_get(l6114, l6111);
      at = 8401; l6110 = l6115;
    } else {
      at = 8402; l6116 = K[2276];
      at = 8403; l6110 = l6116;
    }
    at = 8404; markSet(rt, false, "circle", {at: l6094, radius: l6107, fill: l6110});
    at = 8405; if (rt.truthy(l6084)) {
      at = 8406; l6118 = field(rt, l6087, "points");
      at = 8407; l6117 = l6118;
    } else {
      at = 8408; l6119 = K[2277];
      at = 8409; l6117 = l6119;
    }
    at = 8410; l6120 = K[2278];
    at = 8411; l6121 = rt.sub(l6091, l6120);
    at = 8412; l6122 = K[2279];
    at = 8413; l6123 = rt.max(l6122, l6121);
    at = 8414; l6124 = K[2280];
    at = 8415; l6125 = rt.array_slice(l6117, l6124, l6123);
    at = 8416; if (rt.truthy(l6084)) {
      at = 8417; l6127 = field(rt, l6087, "points");
      at = 8418; l6126 = l6127;
    } else {
      at = 8419; l6128 = K[2281];
      at = 8420; l6126 = l6128;
    }
    at = 8421; l6129 = K[2282];
    at = 8422; l6130 = rt.gt(l6091, l6129);
    at = 8423; if (rt.truthy(l6130)) {
      at = 8424; l6132 = K[2283];
      at = 8425; l6131 = l6132;
    } else {
      at = 8426; l6133 = K[2284];
      at = 8427; l6131 = l6133;
    }
    at = 8428; l6134 = K[2285];
    at = 8429; l6135 = rt.sub(l6091, l6134);
    at = 8430; l6136 = K[2286];
    at = 8431; l6137 = rt.max(l6136, l6135);
    at = 8432; l6138 = rt.array_slice(l6126, l6131, l6137);
    at = 8433; if (rt.truthy(l6084)) {
      at = 8434; l6140 = field(rt, l6087, "color");
      at = 8435; l6139 = l6140;
    } else {
      at = 8436; l6141 = K[2287];
      at = 8437; l6139 = l6141;
    }
    at = 8438; l6142 = K[2288];
    at = 8439; l6143 = rt.sequence_get(l6142, l6139);
    at = 8440; if (rt.truthy(l6084)) {
      at = 8441; l6145 = field(rt, l6087, "width");
      at = 8442; l6144 = l6145;
    } else {
      at = 8443; l6146 = K[2289];
      at = 8444; l6144 = l6146;
    }
    at = 8445; markSet(rt, true, "line", {from: l6125, to: l6138, color: l6143, width: l6144});
    at = 8446; l6147 = field(rt, l15, "strokes");
    at = 8447; l6148 = rt.sequence_length(l6147);
    at = 8448; l6149 = K[2290];
    at = 8449; l6150 = rt.lt(l6149, l6148);
    at = 8450; if (rt.truthy(l6150)) {
      at = 8451; l6151 = field(rt, l15, "strokes");
      at = 8452; l6152 = K[2291];
      at = 8453; l6153 = rt.sequence_get(l6151, l6152);
    }
    at = 8454; if (rt.truthy(l6150)) {
      at = 8455; l6155 = field(rt, l6153, "points");
      at = 8456; l6154 = l6155;
    } else {
      at = 8457; l6156 = K[2292];
      at = 8458; l6154 = l6156;
    }
    at = 8459; l6157 = rt.array_count(l6154);
    at = 8460; l6158 = K[2293];
    at = 8461; l6159 = rt.gt(l6157, l6158);
    at = 8462; if (rt.truthy(l6159)) {
      at = 8463; if (rt.truthy(l6150)) {
        at = 8464; l6162 = field(rt, l6153, "points");
        at = 8465; l6161 = l6162;
      } else {
        at = 8466; l6163 = K[2294];
        at = 8467; l6161 = l6163;
      }
      at = 8468; l6164 = K[2295];
      at = 8469; l6165 = rt.array_nth(l6161, l6164);
      at = 8470; l6160 = l6165;
    } else {
      at = 8471; l6166 = K[2296];
      at = 8472; l6160 = l6166;
    }
    at = 8473; if (rt.truthy(l6150)) {
      at = 8474; l6168 = field(rt, l6153, "width");
      at = 8475; l6167 = l6168;
    } else {
      at = 8476; l6169 = K[2297];
      at = 8477; l6167 = l6169;
    }
    at = 8478; l6170 = K[2298];
    at = 8479; l6171 = rt.int_div(l6167, l6170);
    at = 8480; l6172 = K[2299];
    at = 8481; l6173 = rt.max(l6172, l6171);
    at = 8482; l6174 = K[2300];
    at = 8483; l6175 = rt.gt(l6157, l6174);
    at = 8484; if (rt.truthy(l6175)) {
      at = 8485; if (rt.truthy(l6150)) {
        at = 8486; l6178 = field(rt, l6153, "color");
        at = 8487; l6177 = l6178;
      } else {
        at = 8488; l6179 = K[2301];
        at = 8489; l6177 = l6179;
      }
      at = 8490; l6180 = K[2302];
      at = 8491; l6181 = rt.sequence_get(l6180, l6177);
      at = 8492; l6176 = l6181;
    } else {
      at = 8493; l6182 = K[2303];
      at = 8494; l6176 = l6182;
    }
    at = 8495; markSet(rt, false, "circle", {at: l6160, radius: l6173, fill: l6176});
    at = 8496; if (rt.truthy(l6150)) {
      at = 8497; l6184 = field(rt, l6153, "points");
      at = 8498; l6183 = l6184;
    } else {
      at = 8499; l6185 = K[2304];
      at = 8500; l6183 = l6185;
    }
    at = 8501; l6186 = K[2305];
    at = 8502; l6187 = rt.sub(l6157, l6186);
    at = 8503; l6188 = K[2306];
    at = 8504; l6189 = rt.max(l6188, l6187);
    at = 8505; l6190 = K[2307];
    at = 8506; l6191 = rt.array_slice(l6183, l6190, l6189);
    at = 8507; if (rt.truthy(l6150)) {
      at = 8508; l6193 = field(rt, l6153, "points");
      at = 8509; l6192 = l6193;
    } else {
      at = 8510; l6194 = K[2308];
      at = 8511; l6192 = l6194;
    }
    at = 8512; l6195 = K[2309];
    at = 8513; l6196 = rt.gt(l6157, l6195);
    at = 8514; if (rt.truthy(l6196)) {
      at = 8515; l6198 = K[2310];
      at = 8516; l6197 = l6198;
    } else {
      at = 8517; l6199 = K[2311];
      at = 8518; l6197 = l6199;
    }
    at = 8519; l6200 = K[2312];
    at = 8520; l6201 = rt.sub(l6157, l6200);
    at = 8521; l6202 = K[2313];
    at = 8522; l6203 = rt.max(l6202, l6201);
    at = 8523; l6204 = rt.array_slice(l6192, l6197, l6203);
    at = 8524; if (rt.truthy(l6150)) {
      at = 8525; l6206 = field(rt, l6153, "color");
      at = 8526; l6205 = l6206;
    } else {
      at = 8527; l6207 = K[2314];
      at = 8528; l6205 = l6207;
    }
    at = 8529; l6208 = K[2315];
    at = 8530; l6209 = rt.sequence_get(l6208, l6205);
    at = 8531; if (rt.truthy(l6150)) {
      at = 8532; l6211 = field(rt, l6153, "width");
      at = 8533; l6210 = l6211;
    } else {
      at = 8534; l6212 = K[2316];
      at = 8535; l6210 = l6212;
    }
    at = 8536; markSet(rt, true, "line", {from: l6191, to: l6204, color: l6209, width: l6210});
    at = 8537; l6213 = field(rt, l15, "strokes");
    at = 8538; l6214 = rt.sequence_length(l6213);
    at = 8539; l6215 = K[2317];
    at = 8540; l6216 = rt.lt(l6215, l6214);
    at = 8541; if (rt.truthy(l6216)) {
      at = 8542; l6217 = field(rt, l15, "strokes");
      at = 8543; l6218 = K[2318];
      at = 8544; l6219 = rt.sequence_get(l6217, l6218);
    }
    at = 8545; if (rt.truthy(l6216)) {
      at = 8546; l6221 = field(rt, l6219, "points");
      at = 8547; l6220 = l6221;
    } else {
      at = 8548; l6222 = K[2319];
      at = 8549; l6220 = l6222;
    }
    at = 8550; l6223 = rt.array_count(l6220);
    at = 8551; l6224 = K[2320];
    at = 8552; l6225 = rt.gt(l6223, l6224);
    at = 8553; if (rt.truthy(l6225)) {
      at = 8554; if (rt.truthy(l6216)) {
        at = 8555; l6228 = field(rt, l6219, "points");
        at = 8556; l6227 = l6228;
      } else {
        at = 8557; l6229 = K[2321];
        at = 8558; l6227 = l6229;
      }
      at = 8559; l6230 = K[2322];
      at = 8560; l6231 = rt.array_nth(l6227, l6230);
      at = 8561; l6226 = l6231;
    } else {
      at = 8562; l6232 = K[2323];
      at = 8563; l6226 = l6232;
    }
    at = 8564; if (rt.truthy(l6216)) {
      at = 8565; l6234 = field(rt, l6219, "width");
      at = 8566; l6233 = l6234;
    } else {
      at = 8567; l6235 = K[2324];
      at = 8568; l6233 = l6235;
    }
    at = 8569; l6236 = K[2325];
    at = 8570; l6237 = rt.int_div(l6233, l6236);
    at = 8571; l6238 = K[2326];
    at = 8572; l6239 = rt.max(l6238, l6237);
    at = 8573; l6240 = K[2327];
    at = 8574; l6241 = rt.gt(l6223, l6240);
    at = 8575; if (rt.truthy(l6241)) {
      at = 8576; if (rt.truthy(l6216)) {
        at = 8577; l6244 = field(rt, l6219, "color");
        at = 8578; l6243 = l6244;
      } else {
        at = 8579; l6245 = K[2328];
        at = 8580; l6243 = l6245;
      }
      at = 8581; l6246 = K[2329];
      at = 8582; l6247 = rt.sequence_get(l6246, l6243);
      at = 8583; l6242 = l6247;
    } else {
      at = 8584; l6248 = K[2330];
      at = 8585; l6242 = l6248;
    }
    at = 8586; markSet(rt, false, "circle", {at: l6226, radius: l6239, fill: l6242});
    at = 8587; if (rt.truthy(l6216)) {
      at = 8588; l6250 = field(rt, l6219, "points");
      at = 8589; l6249 = l6250;
    } else {
      at = 8590; l6251 = K[2331];
      at = 8591; l6249 = l6251;
    }
    at = 8592; l6252 = K[2332];
    at = 8593; l6253 = rt.sub(l6223, l6252);
    at = 8594; l6254 = K[2333];
    at = 8595; l6255 = rt.max(l6254, l6253);
    at = 8596; l6256 = K[2334];
    at = 8597; l6257 = rt.array_slice(l6249, l6256, l6255);
    at = 8598; if (rt.truthy(l6216)) {
      at = 8599; l6259 = field(rt, l6219, "points");
      at = 8600; l6258 = l6259;
    } else {
      at = 8601; l6260 = K[2335];
      at = 8602; l6258 = l6260;
    }
    at = 8603; l6261 = K[2336];
    at = 8604; l6262 = rt.gt(l6223, l6261);
    at = 8605; if (rt.truthy(l6262)) {
      at = 8606; l6264 = K[2337];
      at = 8607; l6263 = l6264;
    } else {
      at = 8608; l6265 = K[2338];
      at = 8609; l6263 = l6265;
    }
    at = 8610; l6266 = K[2339];
    at = 8611; l6267 = rt.sub(l6223, l6266);
    at = 8612; l6268 = K[2340];
    at = 8613; l6269 = rt.max(l6268, l6267);
    at = 8614; l6270 = rt.array_slice(l6258, l6263, l6269);
    at = 8615; if (rt.truthy(l6216)) {
      at = 8616; l6272 = field(rt, l6219, "color");
      at = 8617; l6271 = l6272;
    } else {
      at = 8618; l6273 = K[2341];
      at = 8619; l6271 = l6273;
    }
    at = 8620; l6274 = K[2342];
    at = 8621; l6275 = rt.sequence_get(l6274, l6271);
    at = 8622; if (rt.truthy(l6216)) {
      at = 8623; l6277 = field(rt, l6219, "width");
      at = 8624; l6276 = l6277;
    } else {
      at = 8625; l6278 = K[2343];
      at = 8626; l6276 = l6278;
    }
    at = 8627; markSet(rt, true, "line", {from: l6257, to: l6270, color: l6275, width: l6276});
    at = 8628; l6279 = field(rt, l15, "strokes");
    at = 8629; l6280 = rt.sequence_length(l6279);
    at = 8630; l6281 = K[2344];
    at = 8631; l6282 = rt.lt(l6281, l6280);
    at = 8632; if (rt.truthy(l6282)) {
      at = 8633; l6283 = field(rt, l15, "strokes");
      at = 8634; l6284 = K[2345];
      at = 8635; l6285 = rt.sequence_get(l6283, l6284);
    }
    at = 8636; if (rt.truthy(l6282)) {
      at = 8637; l6287 = field(rt, l6285, "points");
      at = 8638; l6286 = l6287;
    } else {
      at = 8639; l6288 = K[2346];
      at = 8640; l6286 = l6288;
    }
    at = 8641; l6289 = rt.array_count(l6286);
    at = 8642; l6290 = K[2347];
    at = 8643; l6291 = rt.gt(l6289, l6290);
    at = 8644; if (rt.truthy(l6291)) {
      at = 8645; if (rt.truthy(l6282)) {
        at = 8646; l6294 = field(rt, l6285, "points");
        at = 8647; l6293 = l6294;
      } else {
        at = 8648; l6295 = K[2348];
        at = 8649; l6293 = l6295;
      }
      at = 8650; l6296 = K[2349];
      at = 8651; l6297 = rt.array_nth(l6293, l6296);
      at = 8652; l6292 = l6297;
    } else {
      at = 8653; l6298 = K[2350];
      at = 8654; l6292 = l6298;
    }
    at = 8655; if (rt.truthy(l6282)) {
      at = 8656; l6300 = field(rt, l6285, "width");
      at = 8657; l6299 = l6300;
    } else {
      at = 8658; l6301 = K[2351];
      at = 8659; l6299 = l6301;
    }
    at = 8660; l6302 = K[2352];
    at = 8661; l6303 = rt.int_div(l6299, l6302);
    at = 8662; l6304 = K[2353];
    at = 8663; l6305 = rt.max(l6304, l6303);
    at = 8664; l6306 = K[2354];
    at = 8665; l6307 = rt.gt(l6289, l6306);
    at = 8666; if (rt.truthy(l6307)) {
      at = 8667; if (rt.truthy(l6282)) {
        at = 8668; l6310 = field(rt, l6285, "color");
        at = 8669; l6309 = l6310;
      } else {
        at = 8670; l6311 = K[2355];
        at = 8671; l6309 = l6311;
      }
      at = 8672; l6312 = K[2356];
      at = 8673; l6313 = rt.sequence_get(l6312, l6309);
      at = 8674; l6308 = l6313;
    } else {
      at = 8675; l6314 = K[2357];
      at = 8676; l6308 = l6314;
    }
    at = 8677; markSet(rt, false, "circle", {at: l6292, radius: l6305, fill: l6308});
    at = 8678; if (rt.truthy(l6282)) {
      at = 8679; l6316 = field(rt, l6285, "points");
      at = 8680; l6315 = l6316;
    } else {
      at = 8681; l6317 = K[2358];
      at = 8682; l6315 = l6317;
    }
    at = 8683; l6318 = K[2359];
    at = 8684; l6319 = rt.sub(l6289, l6318);
    at = 8685; l6320 = K[2360];
    at = 8686; l6321 = rt.max(l6320, l6319);
    at = 8687; l6322 = K[2361];
    at = 8688; l6323 = rt.array_slice(l6315, l6322, l6321);
    at = 8689; if (rt.truthy(l6282)) {
      at = 8690; l6325 = field(rt, l6285, "points");
      at = 8691; l6324 = l6325;
    } else {
      at = 8692; l6326 = K[2362];
      at = 8693; l6324 = l6326;
    }
    at = 8694; l6327 = K[2363];
    at = 8695; l6328 = rt.gt(l6289, l6327);
    at = 8696; if (rt.truthy(l6328)) {
      at = 8697; l6330 = K[2364];
      at = 8698; l6329 = l6330;
    } else {
      at = 8699; l6331 = K[2365];
      at = 8700; l6329 = l6331;
    }
    at = 8701; l6332 = K[2366];
    at = 8702; l6333 = rt.sub(l6289, l6332);
    at = 8703; l6334 = K[2367];
    at = 8704; l6335 = rt.max(l6334, l6333);
    at = 8705; l6336 = rt.array_slice(l6324, l6329, l6335);
    at = 8706; if (rt.truthy(l6282)) {
      at = 8707; l6338 = field(rt, l6285, "color");
      at = 8708; l6337 = l6338;
    } else {
      at = 8709; l6339 = K[2368];
      at = 8710; l6337 = l6339;
    }
    at = 8711; l6340 = K[2369];
    at = 8712; l6341 = rt.sequence_get(l6340, l6337);
    at = 8713; if (rt.truthy(l6282)) {
      at = 8714; l6343 = field(rt, l6285, "width");
      at = 8715; l6342 = l6343;
    } else {
      at = 8716; l6344 = K[2370];
      at = 8717; l6342 = l6344;
    }
    at = 8718; markSet(rt, true, "line", {from: l6323, to: l6336, color: l6341, width: l6342});
    at = 8719; l6345 = field(rt, l15, "strokes");
    at = 8720; l6346 = rt.sequence_length(l6345);
    at = 8721; l6347 = K[2371];
    at = 8722; l6348 = rt.lt(l6347, l6346);
    at = 8723; if (rt.truthy(l6348)) {
      at = 8724; l6349 = field(rt, l15, "strokes");
      at = 8725; l6350 = K[2372];
      at = 8726; l6351 = rt.sequence_get(l6349, l6350);
    }
    at = 8727; if (rt.truthy(l6348)) {
      at = 8728; l6353 = field(rt, l6351, "points");
      at = 8729; l6352 = l6353;
    } else {
      at = 8730; l6354 = K[2373];
      at = 8731; l6352 = l6354;
    }
    at = 8732; l6355 = rt.array_count(l6352);
    at = 8733; l6356 = K[2374];
    at = 8734; l6357 = rt.gt(l6355, l6356);
    at = 8735; if (rt.truthy(l6357)) {
      at = 8736; if (rt.truthy(l6348)) {
        at = 8737; l6360 = field(rt, l6351, "points");
        at = 8738; l6359 = l6360;
      } else {
        at = 8739; l6361 = K[2375];
        at = 8740; l6359 = l6361;
      }
      at = 8741; l6362 = K[2376];
      at = 8742; l6363 = rt.array_nth(l6359, l6362);
      at = 8743; l6358 = l6363;
    } else {
      at = 8744; l6364 = K[2377];
      at = 8745; l6358 = l6364;
    }
    at = 8746; if (rt.truthy(l6348)) {
      at = 8747; l6366 = field(rt, l6351, "width");
      at = 8748; l6365 = l6366;
    } else {
      at = 8749; l6367 = K[2378];
      at = 8750; l6365 = l6367;
    }
    at = 8751; l6368 = K[2379];
    at = 8752; l6369 = rt.int_div(l6365, l6368);
    at = 8753; l6370 = K[2380];
    at = 8754; l6371 = rt.max(l6370, l6369);
    at = 8755; l6372 = K[2381];
    at = 8756; l6373 = rt.gt(l6355, l6372);
    at = 8757; if (rt.truthy(l6373)) {
      at = 8758; if (rt.truthy(l6348)) {
        at = 8759; l6376 = field(rt, l6351, "color");
        at = 8760; l6375 = l6376;
      } else {
        at = 8761; l6377 = K[2382];
        at = 8762; l6375 = l6377;
      }
      at = 8763; l6378 = K[2383];
      at = 8764; l6379 = rt.sequence_get(l6378, l6375);
      at = 8765; l6374 = l6379;
    } else {
      at = 8766; l6380 = K[2384];
      at = 8767; l6374 = l6380;
    }
    at = 8768; markSet(rt, false, "circle", {at: l6358, radius: l6371, fill: l6374});
    at = 8769; if (rt.truthy(l6348)) {
      at = 8770; l6382 = field(rt, l6351, "points");
      at = 8771; l6381 = l6382;
    } else {
      at = 8772; l6383 = K[2385];
      at = 8773; l6381 = l6383;
    }
    at = 8774; l6384 = K[2386];
    at = 8775; l6385 = rt.sub(l6355, l6384);
    at = 8776; l6386 = K[2387];
    at = 8777; l6387 = rt.max(l6386, l6385);
    at = 8778; l6388 = K[2388];
    at = 8779; l6389 = rt.array_slice(l6381, l6388, l6387);
    at = 8780; if (rt.truthy(l6348)) {
      at = 8781; l6391 = field(rt, l6351, "points");
      at = 8782; l6390 = l6391;
    } else {
      at = 8783; l6392 = K[2389];
      at = 8784; l6390 = l6392;
    }
    at = 8785; l6393 = K[2390];
    at = 8786; l6394 = rt.gt(l6355, l6393);
    at = 8787; if (rt.truthy(l6394)) {
      at = 8788; l6396 = K[2391];
      at = 8789; l6395 = l6396;
    } else {
      at = 8790; l6397 = K[2392];
      at = 8791; l6395 = l6397;
    }
    at = 8792; l6398 = K[2393];
    at = 8793; l6399 = rt.sub(l6355, l6398);
    at = 8794; l6400 = K[2394];
    at = 8795; l6401 = rt.max(l6400, l6399);
    at = 8796; l6402 = rt.array_slice(l6390, l6395, l6401);
    at = 8797; if (rt.truthy(l6348)) {
      at = 8798; l6404 = field(rt, l6351, "color");
      at = 8799; l6403 = l6404;
    } else {
      at = 8800; l6405 = K[2395];
      at = 8801; l6403 = l6405;
    }
    at = 8802; l6406 = K[2396];
    at = 8803; l6407 = rt.sequence_get(l6406, l6403);
    at = 8804; if (rt.truthy(l6348)) {
      at = 8805; l6409 = field(rt, l6351, "width");
      at = 8806; l6408 = l6409;
    } else {
      at = 8807; l6410 = K[2397];
      at = 8808; l6408 = l6410;
    }
    at = 8809; markSet(rt, true, "line", {from: l6389, to: l6402, color: l6407, width: l6408});
    at = 8810; l6411 = field(rt, l15, "strokes");
    at = 8811; l6412 = rt.sequence_length(l6411);
    at = 8812; l6413 = K[2398];
    at = 8813; l6414 = rt.lt(l6413, l6412);
    at = 8814; if (rt.truthy(l6414)) {
      at = 8815; l6415 = field(rt, l15, "strokes");
      at = 8816; l6416 = K[2399];
      at = 8817; l6417 = rt.sequence_get(l6415, l6416);
    }
    at = 8818; if (rt.truthy(l6414)) {
      at = 8819; l6419 = field(rt, l6417, "points");
      at = 8820; l6418 = l6419;
    } else {
      at = 8821; l6420 = K[2400];
      at = 8822; l6418 = l6420;
    }
    at = 8823; l6421 = rt.array_count(l6418);
    at = 8824; l6422 = K[2401];
    at = 8825; l6423 = rt.gt(l6421, l6422);
    at = 8826; if (rt.truthy(l6423)) {
      at = 8827; if (rt.truthy(l6414)) {
        at = 8828; l6426 = field(rt, l6417, "points");
        at = 8829; l6425 = l6426;
      } else {
        at = 8830; l6427 = K[2402];
        at = 8831; l6425 = l6427;
      }
      at = 8832; l6428 = K[2403];
      at = 8833; l6429 = rt.array_nth(l6425, l6428);
      at = 8834; l6424 = l6429;
    } else {
      at = 8835; l6430 = K[2404];
      at = 8836; l6424 = l6430;
    }
    at = 8837; if (rt.truthy(l6414)) {
      at = 8838; l6432 = field(rt, l6417, "width");
      at = 8839; l6431 = l6432;
    } else {
      at = 8840; l6433 = K[2405];
      at = 8841; l6431 = l6433;
    }
    at = 8842; l6434 = K[2406];
    at = 8843; l6435 = rt.int_div(l6431, l6434);
    at = 8844; l6436 = K[2407];
    at = 8845; l6437 = rt.max(l6436, l6435);
    at = 8846; l6438 = K[2408];
    at = 8847; l6439 = rt.gt(l6421, l6438);
    at = 8848; if (rt.truthy(l6439)) {
      at = 8849; if (rt.truthy(l6414)) {
        at = 8850; l6442 = field(rt, l6417, "color");
        at = 8851; l6441 = l6442;
      } else {
        at = 8852; l6443 = K[2409];
        at = 8853; l6441 = l6443;
      }
      at = 8854; l6444 = K[2410];
      at = 8855; l6445 = rt.sequence_get(l6444, l6441);
      at = 8856; l6440 = l6445;
    } else {
      at = 8857; l6446 = K[2411];
      at = 8858; l6440 = l6446;
    }
    at = 8859; markSet(rt, false, "circle", {at: l6424, radius: l6437, fill: l6440});
    at = 8860; if (rt.truthy(l6414)) {
      at = 8861; l6448 = field(rt, l6417, "points");
      at = 8862; l6447 = l6448;
    } else {
      at = 8863; l6449 = K[2412];
      at = 8864; l6447 = l6449;
    }
    at = 8865; l6450 = K[2413];
    at = 8866; l6451 = rt.sub(l6421, l6450);
    at = 8867; l6452 = K[2414];
    at = 8868; l6453 = rt.max(l6452, l6451);
    at = 8869; l6454 = K[2415];
    at = 8870; l6455 = rt.array_slice(l6447, l6454, l6453);
    at = 8871; if (rt.truthy(l6414)) {
      at = 8872; l6457 = field(rt, l6417, "points");
      at = 8873; l6456 = l6457;
    } else {
      at = 8874; l6458 = K[2416];
      at = 8875; l6456 = l6458;
    }
    at = 8876; l6459 = K[2417];
    at = 8877; l6460 = rt.gt(l6421, l6459);
    at = 8878; if (rt.truthy(l6460)) {
      at = 8879; l6462 = K[2418];
      at = 8880; l6461 = l6462;
    } else {
      at = 8881; l6463 = K[2419];
      at = 8882; l6461 = l6463;
    }
    at = 8883; l6464 = K[2420];
    at = 8884; l6465 = rt.sub(l6421, l6464);
    at = 8885; l6466 = K[2421];
    at = 8886; l6467 = rt.max(l6466, l6465);
    at = 8887; l6468 = rt.array_slice(l6456, l6461, l6467);
    at = 8888; if (rt.truthy(l6414)) {
      at = 8889; l6470 = field(rt, l6417, "color");
      at = 8890; l6469 = l6470;
    } else {
      at = 8891; l6471 = K[2422];
      at = 8892; l6469 = l6471;
    }
    at = 8893; l6472 = K[2423];
    at = 8894; l6473 = rt.sequence_get(l6472, l6469);
    at = 8895; if (rt.truthy(l6414)) {
      at = 8896; l6475 = field(rt, l6417, "width");
      at = 8897; l6474 = l6475;
    } else {
      at = 8898; l6476 = K[2424];
      at = 8899; l6474 = l6476;
    }
    at = 8900; markSet(rt, true, "line", {from: l6455, to: l6468, color: l6473, width: l6474});
    at = 8901; l6477 = field(rt, l15, "strokes");
    at = 8902; l6478 = rt.sequence_length(l6477);
    at = 8903; l6479 = K[2425];
    at = 8904; l6480 = rt.lt(l6479, l6478);
    at = 8905; if (rt.truthy(l6480)) {
      at = 8906; l6481 = field(rt, l15, "strokes");
      at = 8907; l6482 = K[2426];
      at = 8908; l6483 = rt.sequence_get(l6481, l6482);
    }
    at = 8909; if (rt.truthy(l6480)) {
      at = 8910; l6485 = field(rt, l6483, "points");
      at = 8911; l6484 = l6485;
    } else {
      at = 8912; l6486 = K[2427];
      at = 8913; l6484 = l6486;
    }
    at = 8914; l6487 = rt.array_count(l6484);
    at = 8915; l6488 = K[2428];
    at = 8916; l6489 = rt.gt(l6487, l6488);
    at = 8917; if (rt.truthy(l6489)) {
      at = 8918; if (rt.truthy(l6480)) {
        at = 8919; l6492 = field(rt, l6483, "points");
        at = 8920; l6491 = l6492;
      } else {
        at = 8921; l6493 = K[2429];
        at = 8922; l6491 = l6493;
      }
      at = 8923; l6494 = K[2430];
      at = 8924; l6495 = rt.array_nth(l6491, l6494);
      at = 8925; l6490 = l6495;
    } else {
      at = 8926; l6496 = K[2431];
      at = 8927; l6490 = l6496;
    }
    at = 8928; if (rt.truthy(l6480)) {
      at = 8929; l6498 = field(rt, l6483, "width");
      at = 8930; l6497 = l6498;
    } else {
      at = 8931; l6499 = K[2432];
      at = 8932; l6497 = l6499;
    }
    at = 8933; l6500 = K[2433];
    at = 8934; l6501 = rt.int_div(l6497, l6500);
    at = 8935; l6502 = K[2434];
    at = 8936; l6503 = rt.max(l6502, l6501);
    at = 8937; l6504 = K[2435];
    at = 8938; l6505 = rt.gt(l6487, l6504);
    at = 8939; if (rt.truthy(l6505)) {
      at = 8940; if (rt.truthy(l6480)) {
        at = 8941; l6508 = field(rt, l6483, "color");
        at = 8942; l6507 = l6508;
      } else {
        at = 8943; l6509 = K[2436];
        at = 8944; l6507 = l6509;
      }
      at = 8945; l6510 = K[2437];
      at = 8946; l6511 = rt.sequence_get(l6510, l6507);
      at = 8947; l6506 = l6511;
    } else {
      at = 8948; l6512 = K[2438];
      at = 8949; l6506 = l6512;
    }
    at = 8950; markSet(rt, false, "circle", {at: l6490, radius: l6503, fill: l6506});
    at = 8951; if (rt.truthy(l6480)) {
      at = 8952; l6514 = field(rt, l6483, "points");
      at = 8953; l6513 = l6514;
    } else {
      at = 8954; l6515 = K[2439];
      at = 8955; l6513 = l6515;
    }
    at = 8956; l6516 = K[2440];
    at = 8957; l6517 = rt.sub(l6487, l6516);
    at = 8958; l6518 = K[2441];
    at = 8959; l6519 = rt.max(l6518, l6517);
    at = 8960; l6520 = K[2442];
    at = 8961; l6521 = rt.array_slice(l6513, l6520, l6519);
    at = 8962; if (rt.truthy(l6480)) {
      at = 8963; l6523 = field(rt, l6483, "points");
      at = 8964; l6522 = l6523;
    } else {
      at = 8965; l6524 = K[2443];
      at = 8966; l6522 = l6524;
    }
    at = 8967; l6525 = K[2444];
    at = 8968; l6526 = rt.gt(l6487, l6525);
    at = 8969; if (rt.truthy(l6526)) {
      at = 8970; l6528 = K[2445];
      at = 8971; l6527 = l6528;
    } else {
      at = 8972; l6529 = K[2446];
      at = 8973; l6527 = l6529;
    }
    at = 8974; l6530 = K[2447];
    at = 8975; l6531 = rt.sub(l6487, l6530);
    at = 8976; l6532 = K[2448];
    at = 8977; l6533 = rt.max(l6532, l6531);
    at = 8978; l6534 = rt.array_slice(l6522, l6527, l6533);
    at = 8979; if (rt.truthy(l6480)) {
      at = 8980; l6536 = field(rt, l6483, "color");
      at = 8981; l6535 = l6536;
    } else {
      at = 8982; l6537 = K[2449];
      at = 8983; l6535 = l6537;
    }
    at = 8984; l6538 = K[2450];
    at = 8985; l6539 = rt.sequence_get(l6538, l6535);
    at = 8986; if (rt.truthy(l6480)) {
      at = 8987; l6541 = field(rt, l6483, "width");
      at = 8988; l6540 = l6541;
    } else {
      at = 8989; l6542 = K[2451];
      at = 8990; l6540 = l6542;
    }
    at = 8991; markSet(rt, true, "line", {from: l6521, to: l6534, color: l6539, width: l6540});
    at = 8992; l6543 = field(rt, l15, "strokes");
    at = 8993; l6544 = rt.sequence_length(l6543);
    at = 8994; l6545 = K[2452];
    at = 8995; l6546 = rt.lt(l6545, l6544);
    at = 8996; if (rt.truthy(l6546)) {
      at = 8997; l6547 = field(rt, l15, "strokes");
      at = 8998; l6548 = K[2453];
      at = 8999; l6549 = rt.sequence_get(l6547, l6548);
    }
    at = 9000; if (rt.truthy(l6546)) {
      at = 9001; l6551 = field(rt, l6549, "points");
      at = 9002; l6550 = l6551;
    } else {
      at = 9003; l6552 = K[2454];
      at = 9004; l6550 = l6552;
    }
    at = 9005; l6553 = rt.array_count(l6550);
    at = 9006; l6554 = K[2455];
    at = 9007; l6555 = rt.gt(l6553, l6554);
    at = 9008; if (rt.truthy(l6555)) {
      at = 9009; if (rt.truthy(l6546)) {
        at = 9010; l6558 = field(rt, l6549, "points");
        at = 9011; l6557 = l6558;
      } else {
        at = 9012; l6559 = K[2456];
        at = 9013; l6557 = l6559;
      }
      at = 9014; l6560 = K[2457];
      at = 9015; l6561 = rt.array_nth(l6557, l6560);
      at = 9016; l6556 = l6561;
    } else {
      at = 9017; l6562 = K[2458];
      at = 9018; l6556 = l6562;
    }
    at = 9019; if (rt.truthy(l6546)) {
      at = 9020; l6564 = field(rt, l6549, "width");
      at = 9021; l6563 = l6564;
    } else {
      at = 9022; l6565 = K[2459];
      at = 9023; l6563 = l6565;
    }
    at = 9024; l6566 = K[2460];
    at = 9025; l6567 = rt.int_div(l6563, l6566);
    at = 9026; l6568 = K[2461];
    at = 9027; l6569 = rt.max(l6568, l6567);
    at = 9028; l6570 = K[2462];
    at = 9029; l6571 = rt.gt(l6553, l6570);
    at = 9030; if (rt.truthy(l6571)) {
      at = 9031; if (rt.truthy(l6546)) {
        at = 9032; l6574 = field(rt, l6549, "color");
        at = 9033; l6573 = l6574;
      } else {
        at = 9034; l6575 = K[2463];
        at = 9035; l6573 = l6575;
      }
      at = 9036; l6576 = K[2464];
      at = 9037; l6577 = rt.sequence_get(l6576, l6573);
      at = 9038; l6572 = l6577;
    } else {
      at = 9039; l6578 = K[2465];
      at = 9040; l6572 = l6578;
    }
    at = 9041; markSet(rt, false, "circle", {at: l6556, radius: l6569, fill: l6572});
    at = 9042; if (rt.truthy(l6546)) {
      at = 9043; l6580 = field(rt, l6549, "points");
      at = 9044; l6579 = l6580;
    } else {
      at = 9045; l6581 = K[2466];
      at = 9046; l6579 = l6581;
    }
    at = 9047; l6582 = K[2467];
    at = 9048; l6583 = rt.sub(l6553, l6582);
    at = 9049; l6584 = K[2468];
    at = 9050; l6585 = rt.max(l6584, l6583);
    at = 9051; l6586 = K[2469];
    at = 9052; l6587 = rt.array_slice(l6579, l6586, l6585);
    at = 9053; if (rt.truthy(l6546)) {
      at = 9054; l6589 = field(rt, l6549, "points");
      at = 9055; l6588 = l6589;
    } else {
      at = 9056; l6590 = K[2470];
      at = 9057; l6588 = l6590;
    }
    at = 9058; l6591 = K[2471];
    at = 9059; l6592 = rt.gt(l6553, l6591);
    at = 9060; if (rt.truthy(l6592)) {
      at = 9061; l6594 = K[2472];
      at = 9062; l6593 = l6594;
    } else {
      at = 9063; l6595 = K[2473];
      at = 9064; l6593 = l6595;
    }
    at = 9065; l6596 = K[2474];
    at = 9066; l6597 = rt.sub(l6553, l6596);
    at = 9067; l6598 = K[2475];
    at = 9068; l6599 = rt.max(l6598, l6597);
    at = 9069; l6600 = rt.array_slice(l6588, l6593, l6599);
    at = 9070; if (rt.truthy(l6546)) {
      at = 9071; l6602 = field(rt, l6549, "color");
      at = 9072; l6601 = l6602;
    } else {
      at = 9073; l6603 = K[2476];
      at = 9074; l6601 = l6603;
    }
    at = 9075; l6604 = K[2477];
    at = 9076; l6605 = rt.sequence_get(l6604, l6601);
    at = 9077; if (rt.truthy(l6546)) {
      at = 9078; l6607 = field(rt, l6549, "width");
      at = 9079; l6606 = l6607;
    } else {
      at = 9080; l6608 = K[2478];
      at = 9081; l6606 = l6608;
    }
    at = 9082; markSet(rt, true, "line", {from: l6587, to: l6600, color: l6605, width: l6606});
    at = 9083; l6609 = field(rt, l15, "strokes");
    at = 9084; l6610 = rt.sequence_length(l6609);
    at = 9085; l6611 = K[2479];
    at = 9086; l6612 = rt.lt(l6611, l6610);
    at = 9087; if (rt.truthy(l6612)) {
      at = 9088; l6613 = field(rt, l15, "strokes");
      at = 9089; l6614 = K[2480];
      at = 9090; l6615 = rt.sequence_get(l6613, l6614);
    }
    at = 9091; if (rt.truthy(l6612)) {
      at = 9092; l6617 = field(rt, l6615, "points");
      at = 9093; l6616 = l6617;
    } else {
      at = 9094; l6618 = K[2481];
      at = 9095; l6616 = l6618;
    }
    at = 9096; l6619 = rt.array_count(l6616);
    at = 9097; l6620 = K[2482];
    at = 9098; l6621 = rt.gt(l6619, l6620);
    at = 9099; if (rt.truthy(l6621)) {
      at = 9100; if (rt.truthy(l6612)) {
        at = 9101; l6624 = field(rt, l6615, "points");
        at = 9102; l6623 = l6624;
      } else {
        at = 9103; l6625 = K[2483];
        at = 9104; l6623 = l6625;
      }
      at = 9105; l6626 = K[2484];
      at = 9106; l6627 = rt.array_nth(l6623, l6626);
      at = 9107; l6622 = l6627;
    } else {
      at = 9108; l6628 = K[2485];
      at = 9109; l6622 = l6628;
    }
    at = 9110; if (rt.truthy(l6612)) {
      at = 9111; l6630 = field(rt, l6615, "width");
      at = 9112; l6629 = l6630;
    } else {
      at = 9113; l6631 = K[2486];
      at = 9114; l6629 = l6631;
    }
    at = 9115; l6632 = K[2487];
    at = 9116; l6633 = rt.int_div(l6629, l6632);
    at = 9117; l6634 = K[2488];
    at = 9118; l6635 = rt.max(l6634, l6633);
    at = 9119; l6636 = K[2489];
    at = 9120; l6637 = rt.gt(l6619, l6636);
    at = 9121; if (rt.truthy(l6637)) {
      at = 9122; if (rt.truthy(l6612)) {
        at = 9123; l6640 = field(rt, l6615, "color");
        at = 9124; l6639 = l6640;
      } else {
        at = 9125; l6641 = K[2490];
        at = 9126; l6639 = l6641;
      }
      at = 9127; l6642 = K[2491];
      at = 9128; l6643 = rt.sequence_get(l6642, l6639);
      at = 9129; l6638 = l6643;
    } else {
      at = 9130; l6644 = K[2492];
      at = 9131; l6638 = l6644;
    }
    at = 9132; markSet(rt, false, "circle", {at: l6622, radius: l6635, fill: l6638});
    at = 9133; if (rt.truthy(l6612)) {
      at = 9134; l6646 = field(rt, l6615, "points");
      at = 9135; l6645 = l6646;
    } else {
      at = 9136; l6647 = K[2493];
      at = 9137; l6645 = l6647;
    }
    at = 9138; l6648 = K[2494];
    at = 9139; l6649 = rt.sub(l6619, l6648);
    at = 9140; l6650 = K[2495];
    at = 9141; l6651 = rt.max(l6650, l6649);
    at = 9142; l6652 = K[2496];
    at = 9143; l6653 = rt.array_slice(l6645, l6652, l6651);
    at = 9144; if (rt.truthy(l6612)) {
      at = 9145; l6655 = field(rt, l6615, "points");
      at = 9146; l6654 = l6655;
    } else {
      at = 9147; l6656 = K[2497];
      at = 9148; l6654 = l6656;
    }
    at = 9149; l6657 = K[2498];
    at = 9150; l6658 = rt.gt(l6619, l6657);
    at = 9151; if (rt.truthy(l6658)) {
      at = 9152; l6660 = K[2499];
      at = 9153; l6659 = l6660;
    } else {
      at = 9154; l6661 = K[2500];
      at = 9155; l6659 = l6661;
    }
    at = 9156; l6662 = K[2501];
    at = 9157; l6663 = rt.sub(l6619, l6662);
    at = 9158; l6664 = K[2502];
    at = 9159; l6665 = rt.max(l6664, l6663);
    at = 9160; l6666 = rt.array_slice(l6654, l6659, l6665);
    at = 9161; if (rt.truthy(l6612)) {
      at = 9162; l6668 = field(rt, l6615, "color");
      at = 9163; l6667 = l6668;
    } else {
      at = 9164; l6669 = K[2503];
      at = 9165; l6667 = l6669;
    }
    at = 9166; l6670 = K[2504];
    at = 9167; l6671 = rt.sequence_get(l6670, l6667);
    at = 9168; if (rt.truthy(l6612)) {
      at = 9169; l6673 = field(rt, l6615, "width");
      at = 9170; l6672 = l6673;
    } else {
      at = 9171; l6674 = K[2505];
      at = 9172; l6672 = l6674;
    }
    at = 9173; markSet(rt, true, "line", {from: l6653, to: l6666, color: l6671, width: l6672});
    at = 9174; l6675 = field(rt, l15, "strokes");
    at = 9175; l6676 = rt.sequence_length(l6675);
    at = 9176; l6677 = K[2506];
    at = 9177; l6678 = rt.lt(l6677, l6676);
    at = 9178; if (rt.truthy(l6678)) {
      at = 9179; l6679 = field(rt, l15, "strokes");
      at = 9180; l6680 = K[2507];
      at = 9181; l6681 = rt.sequence_get(l6679, l6680);
    }
    at = 9182; if (rt.truthy(l6678)) {
      at = 9183; l6683 = field(rt, l6681, "points");
      at = 9184; l6682 = l6683;
    } else {
      at = 9185; l6684 = K[2508];
      at = 9186; l6682 = l6684;
    }
    at = 9187; l6685 = rt.array_count(l6682);
    at = 9188; l6686 = K[2509];
    at = 9189; l6687 = rt.gt(l6685, l6686);
    at = 9190; if (rt.truthy(l6687)) {
      at = 9191; if (rt.truthy(l6678)) {
        at = 9192; l6690 = field(rt, l6681, "points");
        at = 9193; l6689 = l6690;
      } else {
        at = 9194; l6691 = K[2510];
        at = 9195; l6689 = l6691;
      }
      at = 9196; l6692 = K[2511];
      at = 9197; l6693 = rt.array_nth(l6689, l6692);
      at = 9198; l6688 = l6693;
    } else {
      at = 9199; l6694 = K[2512];
      at = 9200; l6688 = l6694;
    }
    at = 9201; if (rt.truthy(l6678)) {
      at = 9202; l6696 = field(rt, l6681, "width");
      at = 9203; l6695 = l6696;
    } else {
      at = 9204; l6697 = K[2513];
      at = 9205; l6695 = l6697;
    }
    at = 9206; l6698 = K[2514];
    at = 9207; l6699 = rt.int_div(l6695, l6698);
    at = 9208; l6700 = K[2515];
    at = 9209; l6701 = rt.max(l6700, l6699);
    at = 9210; l6702 = K[2516];
    at = 9211; l6703 = rt.gt(l6685, l6702);
    at = 9212; if (rt.truthy(l6703)) {
      at = 9213; if (rt.truthy(l6678)) {
        at = 9214; l6706 = field(rt, l6681, "color");
        at = 9215; l6705 = l6706;
      } else {
        at = 9216; l6707 = K[2517];
        at = 9217; l6705 = l6707;
      }
      at = 9218; l6708 = K[2518];
      at = 9219; l6709 = rt.sequence_get(l6708, l6705);
      at = 9220; l6704 = l6709;
    } else {
      at = 9221; l6710 = K[2519];
      at = 9222; l6704 = l6710;
    }
    at = 9223; markSet(rt, false, "circle", {at: l6688, radius: l6701, fill: l6704});
    at = 9224; if (rt.truthy(l6678)) {
      at = 9225; l6712 = field(rt, l6681, "points");
      at = 9226; l6711 = l6712;
    } else {
      at = 9227; l6713 = K[2520];
      at = 9228; l6711 = l6713;
    }
    at = 9229; l6714 = K[2521];
    at = 9230; l6715 = rt.sub(l6685, l6714);
    at = 9231; l6716 = K[2522];
    at = 9232; l6717 = rt.max(l6716, l6715);
    at = 9233; l6718 = K[2523];
    at = 9234; l6719 = rt.array_slice(l6711, l6718, l6717);
    at = 9235; if (rt.truthy(l6678)) {
      at = 9236; l6721 = field(rt, l6681, "points");
      at = 9237; l6720 = l6721;
    } else {
      at = 9238; l6722 = K[2524];
      at = 9239; l6720 = l6722;
    }
    at = 9240; l6723 = K[2525];
    at = 9241; l6724 = rt.gt(l6685, l6723);
    at = 9242; if (rt.truthy(l6724)) {
      at = 9243; l6726 = K[2526];
      at = 9244; l6725 = l6726;
    } else {
      at = 9245; l6727 = K[2527];
      at = 9246; l6725 = l6727;
    }
    at = 9247; l6728 = K[2528];
    at = 9248; l6729 = rt.sub(l6685, l6728);
    at = 9249; l6730 = K[2529];
    at = 9250; l6731 = rt.max(l6730, l6729);
    at = 9251; l6732 = rt.array_slice(l6720, l6725, l6731);
    at = 9252; if (rt.truthy(l6678)) {
      at = 9253; l6734 = field(rt, l6681, "color");
      at = 9254; l6733 = l6734;
    } else {
      at = 9255; l6735 = K[2530];
      at = 9256; l6733 = l6735;
    }
    at = 9257; l6736 = K[2531];
    at = 9258; l6737 = rt.sequence_get(l6736, l6733);
    at = 9259; if (rt.truthy(l6678)) {
      at = 9260; l6739 = field(rt, l6681, "width");
      at = 9261; l6738 = l6739;
    } else {
      at = 9262; l6740 = K[2532];
      at = 9263; l6738 = l6740;
    }
    at = 9264; markSet(rt, true, "line", {from: l6719, to: l6732, color: l6737, width: l6738});
    at = 9265; l6741 = field(rt, l15, "strokes");
    at = 9266; l6742 = rt.sequence_length(l6741);
    at = 9267; l6743 = K[2533];
    at = 9268; l6744 = rt.lt(l6743, l6742);
    at = 9269; if (rt.truthy(l6744)) {
      at = 9270; l6745 = field(rt, l15, "strokes");
      at = 9271; l6746 = K[2534];
      at = 9272; l6747 = rt.sequence_get(l6745, l6746);
    }
    at = 9273; if (rt.truthy(l6744)) {
      at = 9274; l6749 = field(rt, l6747, "points");
      at = 9275; l6748 = l6749;
    } else {
      at = 9276; l6750 = K[2535];
      at = 9277; l6748 = l6750;
    }
    at = 9278; l6751 = rt.array_count(l6748);
    at = 9279; l6752 = K[2536];
    at = 9280; l6753 = rt.gt(l6751, l6752);
    at = 9281; if (rt.truthy(l6753)) {
      at = 9282; if (rt.truthy(l6744)) {
        at = 9283; l6756 = field(rt, l6747, "points");
        at = 9284; l6755 = l6756;
      } else {
        at = 9285; l6757 = K[2537];
        at = 9286; l6755 = l6757;
      }
      at = 9287; l6758 = K[2538];
      at = 9288; l6759 = rt.array_nth(l6755, l6758);
      at = 9289; l6754 = l6759;
    } else {
      at = 9290; l6760 = K[2539];
      at = 9291; l6754 = l6760;
    }
    at = 9292; if (rt.truthy(l6744)) {
      at = 9293; l6762 = field(rt, l6747, "width");
      at = 9294; l6761 = l6762;
    } else {
      at = 9295; l6763 = K[2540];
      at = 9296; l6761 = l6763;
    }
    at = 9297; l6764 = K[2541];
    at = 9298; l6765 = rt.int_div(l6761, l6764);
    at = 9299; l6766 = K[2542];
    at = 9300; l6767 = rt.max(l6766, l6765);
    at = 9301; l6768 = K[2543];
    at = 9302; l6769 = rt.gt(l6751, l6768);
    at = 9303; if (rt.truthy(l6769)) {
      at = 9304; if (rt.truthy(l6744)) {
        at = 9305; l6772 = field(rt, l6747, "color");
        at = 9306; l6771 = l6772;
      } else {
        at = 9307; l6773 = K[2544];
        at = 9308; l6771 = l6773;
      }
      at = 9309; l6774 = K[2545];
      at = 9310; l6775 = rt.sequence_get(l6774, l6771);
      at = 9311; l6770 = l6775;
    } else {
      at = 9312; l6776 = K[2546];
      at = 9313; l6770 = l6776;
    }
    at = 9314; markSet(rt, false, "circle", {at: l6754, radius: l6767, fill: l6770});
    at = 9315; if (rt.truthy(l6744)) {
      at = 9316; l6778 = field(rt, l6747, "points");
      at = 9317; l6777 = l6778;
    } else {
      at = 9318; l6779 = K[2547];
      at = 9319; l6777 = l6779;
    }
    at = 9320; l6780 = K[2548];
    at = 9321; l6781 = rt.sub(l6751, l6780);
    at = 9322; l6782 = K[2549];
    at = 9323; l6783 = rt.max(l6782, l6781);
    at = 9324; l6784 = K[2550];
    at = 9325; l6785 = rt.array_slice(l6777, l6784, l6783);
    at = 9326; if (rt.truthy(l6744)) {
      at = 9327; l6787 = field(rt, l6747, "points");
      at = 9328; l6786 = l6787;
    } else {
      at = 9329; l6788 = K[2551];
      at = 9330; l6786 = l6788;
    }
    at = 9331; l6789 = K[2552];
    at = 9332; l6790 = rt.gt(l6751, l6789);
    at = 9333; if (rt.truthy(l6790)) {
      at = 9334; l6792 = K[2553];
      at = 9335; l6791 = l6792;
    } else {
      at = 9336; l6793 = K[2554];
      at = 9337; l6791 = l6793;
    }
    at = 9338; l6794 = K[2555];
    at = 9339; l6795 = rt.sub(l6751, l6794);
    at = 9340; l6796 = K[2556];
    at = 9341; l6797 = rt.max(l6796, l6795);
    at = 9342; l6798 = rt.array_slice(l6786, l6791, l6797);
    at = 9343; if (rt.truthy(l6744)) {
      at = 9344; l6800 = field(rt, l6747, "color");
      at = 9345; l6799 = l6800;
    } else {
      at = 9346; l6801 = K[2557];
      at = 9347; l6799 = l6801;
    }
    at = 9348; l6802 = K[2558];
    at = 9349; l6803 = rt.sequence_get(l6802, l6799);
    at = 9350; if (rt.truthy(l6744)) {
      at = 9351; l6805 = field(rt, l6747, "width");
      at = 9352; l6804 = l6805;
    } else {
      at = 9353; l6806 = K[2559];
      at = 9354; l6804 = l6806;
    }
    at = 9355; markSet(rt, true, "line", {from: l6785, to: l6798, color: l6803, width: l6804});
    at = 9356; l6807 = field(rt, l15, "strokes");
    at = 9357; l6808 = rt.sequence_length(l6807);
    at = 9358; l6809 = K[2560];
    at = 9359; l6810 = rt.lt(l6809, l6808);
    at = 9360; if (rt.truthy(l6810)) {
      at = 9361; l6811 = field(rt, l15, "strokes");
      at = 9362; l6812 = K[2561];
      at = 9363; l6813 = rt.sequence_get(l6811, l6812);
    }
    at = 9364; if (rt.truthy(l6810)) {
      at = 9365; l6815 = field(rt, l6813, "points");
      at = 9366; l6814 = l6815;
    } else {
      at = 9367; l6816 = K[2562];
      at = 9368; l6814 = l6816;
    }
    at = 9369; l6817 = rt.array_count(l6814);
    at = 9370; l6818 = K[2563];
    at = 9371; l6819 = rt.gt(l6817, l6818);
    at = 9372; if (rt.truthy(l6819)) {
      at = 9373; if (rt.truthy(l6810)) {
        at = 9374; l6822 = field(rt, l6813, "points");
        at = 9375; l6821 = l6822;
      } else {
        at = 9376; l6823 = K[2564];
        at = 9377; l6821 = l6823;
      }
      at = 9378; l6824 = K[2565];
      at = 9379; l6825 = rt.array_nth(l6821, l6824);
      at = 9380; l6820 = l6825;
    } else {
      at = 9381; l6826 = K[2566];
      at = 9382; l6820 = l6826;
    }
    at = 9383; if (rt.truthy(l6810)) {
      at = 9384; l6828 = field(rt, l6813, "width");
      at = 9385; l6827 = l6828;
    } else {
      at = 9386; l6829 = K[2567];
      at = 9387; l6827 = l6829;
    }
    at = 9388; l6830 = K[2568];
    at = 9389; l6831 = rt.int_div(l6827, l6830);
    at = 9390; l6832 = K[2569];
    at = 9391; l6833 = rt.max(l6832, l6831);
    at = 9392; l6834 = K[2570];
    at = 9393; l6835 = rt.gt(l6817, l6834);
    at = 9394; if (rt.truthy(l6835)) {
      at = 9395; if (rt.truthy(l6810)) {
        at = 9396; l6838 = field(rt, l6813, "color");
        at = 9397; l6837 = l6838;
      } else {
        at = 9398; l6839 = K[2571];
        at = 9399; l6837 = l6839;
      }
      at = 9400; l6840 = K[2572];
      at = 9401; l6841 = rt.sequence_get(l6840, l6837);
      at = 9402; l6836 = l6841;
    } else {
      at = 9403; l6842 = K[2573];
      at = 9404; l6836 = l6842;
    }
    at = 9405; markSet(rt, false, "circle", {at: l6820, radius: l6833, fill: l6836});
    at = 9406; if (rt.truthy(l6810)) {
      at = 9407; l6844 = field(rt, l6813, "points");
      at = 9408; l6843 = l6844;
    } else {
      at = 9409; l6845 = K[2574];
      at = 9410; l6843 = l6845;
    }
    at = 9411; l6846 = K[2575];
    at = 9412; l6847 = rt.sub(l6817, l6846);
    at = 9413; l6848 = K[2576];
    at = 9414; l6849 = rt.max(l6848, l6847);
    at = 9415; l6850 = K[2577];
    at = 9416; l6851 = rt.array_slice(l6843, l6850, l6849);
    at = 9417; if (rt.truthy(l6810)) {
      at = 9418; l6853 = field(rt, l6813, "points");
      at = 9419; l6852 = l6853;
    } else {
      at = 9420; l6854 = K[2578];
      at = 9421; l6852 = l6854;
    }
    at = 9422; l6855 = K[2579];
    at = 9423; l6856 = rt.gt(l6817, l6855);
    at = 9424; if (rt.truthy(l6856)) {
      at = 9425; l6858 = K[2580];
      at = 9426; l6857 = l6858;
    } else {
      at = 9427; l6859 = K[2581];
      at = 9428; l6857 = l6859;
    }
    at = 9429; l6860 = K[2582];
    at = 9430; l6861 = rt.sub(l6817, l6860);
    at = 9431; l6862 = K[2583];
    at = 9432; l6863 = rt.max(l6862, l6861);
    at = 9433; l6864 = rt.array_slice(l6852, l6857, l6863);
    at = 9434; if (rt.truthy(l6810)) {
      at = 9435; l6866 = field(rt, l6813, "color");
      at = 9436; l6865 = l6866;
    } else {
      at = 9437; l6867 = K[2584];
      at = 9438; l6865 = l6867;
    }
    at = 9439; l6868 = K[2585];
    at = 9440; l6869 = rt.sequence_get(l6868, l6865);
    at = 9441; if (rt.truthy(l6810)) {
      at = 9442; l6871 = field(rt, l6813, "width");
      at = 9443; l6870 = l6871;
    } else {
      at = 9444; l6872 = K[2586];
      at = 9445; l6870 = l6872;
    }
    at = 9446; markSet(rt, true, "line", {from: l6851, to: l6864, color: l6869, width: l6870});
    at = 9447; l6873 = field(rt, l15, "strokes");
    at = 9448; l6874 = rt.sequence_length(l6873);
    at = 9449; l6875 = K[2587];
    at = 9450; l6876 = rt.lt(l6875, l6874);
    at = 9451; if (rt.truthy(l6876)) {
      at = 9452; l6877 = field(rt, l15, "strokes");
      at = 9453; l6878 = K[2588];
      at = 9454; l6879 = rt.sequence_get(l6877, l6878);
    }
    at = 9455; if (rt.truthy(l6876)) {
      at = 9456; l6881 = field(rt, l6879, "points");
      at = 9457; l6880 = l6881;
    } else {
      at = 9458; l6882 = K[2589];
      at = 9459; l6880 = l6882;
    }
    at = 9460; l6883 = rt.array_count(l6880);
    at = 9461; l6884 = K[2590];
    at = 9462; l6885 = rt.gt(l6883, l6884);
    at = 9463; if (rt.truthy(l6885)) {
      at = 9464; if (rt.truthy(l6876)) {
        at = 9465; l6888 = field(rt, l6879, "points");
        at = 9466; l6887 = l6888;
      } else {
        at = 9467; l6889 = K[2591];
        at = 9468; l6887 = l6889;
      }
      at = 9469; l6890 = K[2592];
      at = 9470; l6891 = rt.array_nth(l6887, l6890);
      at = 9471; l6886 = l6891;
    } else {
      at = 9472; l6892 = K[2593];
      at = 9473; l6886 = l6892;
    }
    at = 9474; if (rt.truthy(l6876)) {
      at = 9475; l6894 = field(rt, l6879, "width");
      at = 9476; l6893 = l6894;
    } else {
      at = 9477; l6895 = K[2594];
      at = 9478; l6893 = l6895;
    }
    at = 9479; l6896 = K[2595];
    at = 9480; l6897 = rt.int_div(l6893, l6896);
    at = 9481; l6898 = K[2596];
    at = 9482; l6899 = rt.max(l6898, l6897);
    at = 9483; l6900 = K[2597];
    at = 9484; l6901 = rt.gt(l6883, l6900);
    at = 9485; if (rt.truthy(l6901)) {
      at = 9486; if (rt.truthy(l6876)) {
        at = 9487; l6904 = field(rt, l6879, "color");
        at = 9488; l6903 = l6904;
      } else {
        at = 9489; l6905 = K[2598];
        at = 9490; l6903 = l6905;
      }
      at = 9491; l6906 = K[2599];
      at = 9492; l6907 = rt.sequence_get(l6906, l6903);
      at = 9493; l6902 = l6907;
    } else {
      at = 9494; l6908 = K[2600];
      at = 9495; l6902 = l6908;
    }
    at = 9496; markSet(rt, false, "circle", {at: l6886, radius: l6899, fill: l6902});
    at = 9497; if (rt.truthy(l6876)) {
      at = 9498; l6910 = field(rt, l6879, "points");
      at = 9499; l6909 = l6910;
    } else {
      at = 9500; l6911 = K[2601];
      at = 9501; l6909 = l6911;
    }
    at = 9502; l6912 = K[2602];
    at = 9503; l6913 = rt.sub(l6883, l6912);
    at = 9504; l6914 = K[2603];
    at = 9505; l6915 = rt.max(l6914, l6913);
    at = 9506; l6916 = K[2604];
    at = 9507; l6917 = rt.array_slice(l6909, l6916, l6915);
    at = 9508; if (rt.truthy(l6876)) {
      at = 9509; l6919 = field(rt, l6879, "points");
      at = 9510; l6918 = l6919;
    } else {
      at = 9511; l6920 = K[2605];
      at = 9512; l6918 = l6920;
    }
    at = 9513; l6921 = K[2606];
    at = 9514; l6922 = rt.gt(l6883, l6921);
    at = 9515; if (rt.truthy(l6922)) {
      at = 9516; l6924 = K[2607];
      at = 9517; l6923 = l6924;
    } else {
      at = 9518; l6925 = K[2608];
      at = 9519; l6923 = l6925;
    }
    at = 9520; l6926 = K[2609];
    at = 9521; l6927 = rt.sub(l6883, l6926);
    at = 9522; l6928 = K[2610];
    at = 9523; l6929 = rt.max(l6928, l6927);
    at = 9524; l6930 = rt.array_slice(l6918, l6923, l6929);
    at = 9525; if (rt.truthy(l6876)) {
      at = 9526; l6932 = field(rt, l6879, "color");
      at = 9527; l6931 = l6932;
    } else {
      at = 9528; l6933 = K[2611];
      at = 9529; l6931 = l6933;
    }
    at = 9530; l6934 = K[2612];
    at = 9531; l6935 = rt.sequence_get(l6934, l6931);
    at = 9532; if (rt.truthy(l6876)) {
      at = 9533; l6937 = field(rt, l6879, "width");
      at = 9534; l6936 = l6937;
    } else {
      at = 9535; l6938 = K[2613];
      at = 9536; l6936 = l6938;
    }
    at = 9537; markSet(rt, true, "line", {from: l6917, to: l6930, color: l6935, width: l6936});
    at = 9538; l6939 = field(rt, l15, "strokes");
    at = 9539; l6940 = rt.sequence_length(l6939);
    at = 9540; l6941 = K[2614];
    at = 9541; l6942 = rt.lt(l6941, l6940);
    at = 9542; if (rt.truthy(l6942)) {
      at = 9543; l6943 = field(rt, l15, "strokes");
      at = 9544; l6944 = K[2615];
      at = 9545; l6945 = rt.sequence_get(l6943, l6944);
    }
    at = 9546; if (rt.truthy(l6942)) {
      at = 9547; l6947 = field(rt, l6945, "points");
      at = 9548; l6946 = l6947;
    } else {
      at = 9549; l6948 = K[2616];
      at = 9550; l6946 = l6948;
    }
    at = 9551; l6949 = rt.array_count(l6946);
    at = 9552; l6950 = K[2617];
    at = 9553; l6951 = rt.gt(l6949, l6950);
    at = 9554; if (rt.truthy(l6951)) {
      at = 9555; if (rt.truthy(l6942)) {
        at = 9556; l6954 = field(rt, l6945, "points");
        at = 9557; l6953 = l6954;
      } else {
        at = 9558; l6955 = K[2618];
        at = 9559; l6953 = l6955;
      }
      at = 9560; l6956 = K[2619];
      at = 9561; l6957 = rt.array_nth(l6953, l6956);
      at = 9562; l6952 = l6957;
    } else {
      at = 9563; l6958 = K[2620];
      at = 9564; l6952 = l6958;
    }
    at = 9565; if (rt.truthy(l6942)) {
      at = 9566; l6960 = field(rt, l6945, "width");
      at = 9567; l6959 = l6960;
    } else {
      at = 9568; l6961 = K[2621];
      at = 9569; l6959 = l6961;
    }
    at = 9570; l6962 = K[2622];
    at = 9571; l6963 = rt.int_div(l6959, l6962);
    at = 9572; l6964 = K[2623];
    at = 9573; l6965 = rt.max(l6964, l6963);
    at = 9574; l6966 = K[2624];
    at = 9575; l6967 = rt.gt(l6949, l6966);
    at = 9576; if (rt.truthy(l6967)) {
      at = 9577; if (rt.truthy(l6942)) {
        at = 9578; l6970 = field(rt, l6945, "color");
        at = 9579; l6969 = l6970;
      } else {
        at = 9580; l6971 = K[2625];
        at = 9581; l6969 = l6971;
      }
      at = 9582; l6972 = K[2626];
      at = 9583; l6973 = rt.sequence_get(l6972, l6969);
      at = 9584; l6968 = l6973;
    } else {
      at = 9585; l6974 = K[2627];
      at = 9586; l6968 = l6974;
    }
    at = 9587; markSet(rt, false, "circle", {at: l6952, radius: l6965, fill: l6968});
    at = 9588; if (rt.truthy(l6942)) {
      at = 9589; l6976 = field(rt, l6945, "points");
      at = 9590; l6975 = l6976;
    } else {
      at = 9591; l6977 = K[2628];
      at = 9592; l6975 = l6977;
    }
    at = 9593; l6978 = K[2629];
    at = 9594; l6979 = rt.sub(l6949, l6978);
    at = 9595; l6980 = K[2630];
    at = 9596; l6981 = rt.max(l6980, l6979);
    at = 9597; l6982 = K[2631];
    at = 9598; l6983 = rt.array_slice(l6975, l6982, l6981);
    at = 9599; if (rt.truthy(l6942)) {
      at = 9600; l6985 = field(rt, l6945, "points");
      at = 9601; l6984 = l6985;
    } else {
      at = 9602; l6986 = K[2632];
      at = 9603; l6984 = l6986;
    }
    at = 9604; l6987 = K[2633];
    at = 9605; l6988 = rt.gt(l6949, l6987);
    at = 9606; if (rt.truthy(l6988)) {
      at = 9607; l6990 = K[2634];
      at = 9608; l6989 = l6990;
    } else {
      at = 9609; l6991 = K[2635];
      at = 9610; l6989 = l6991;
    }
    at = 9611; l6992 = K[2636];
    at = 9612; l6993 = rt.sub(l6949, l6992);
    at = 9613; l6994 = K[2637];
    at = 9614; l6995 = rt.max(l6994, l6993);
    at = 9615; l6996 = rt.array_slice(l6984, l6989, l6995);
    at = 9616; if (rt.truthy(l6942)) {
      at = 9617; l6998 = field(rt, l6945, "color");
      at = 9618; l6997 = l6998;
    } else {
      at = 9619; l6999 = K[2638];
      at = 9620; l6997 = l6999;
    }
    at = 9621; l7000 = K[2639];
    at = 9622; l7001 = rt.sequence_get(l7000, l6997);
    at = 9623; if (rt.truthy(l6942)) {
      at = 9624; l7003 = field(rt, l6945, "width");
      at = 9625; l7002 = l7003;
    } else {
      at = 9626; l7004 = K[2640];
      at = 9627; l7002 = l7004;
    }
    at = 9628; markSet(rt, true, "line", {from: l6983, to: l6996, color: l7001, width: l7002});
    at = 9629; l7005 = field(rt, l15, "strokes");
    at = 9630; l7006 = rt.sequence_length(l7005);
    at = 9631; l7007 = K[2641];
    at = 9632; l7008 = rt.lt(l7007, l7006);
    at = 9633; if (rt.truthy(l7008)) {
      at = 9634; l7009 = field(rt, l15, "strokes");
      at = 9635; l7010 = K[2642];
      at = 9636; l7011 = rt.sequence_get(l7009, l7010);
    }
    at = 9637; if (rt.truthy(l7008)) {
      at = 9638; l7013 = field(rt, l7011, "points");
      at = 9639; l7012 = l7013;
    } else {
      at = 9640; l7014 = K[2643];
      at = 9641; l7012 = l7014;
    }
    at = 9642; l7015 = rt.array_count(l7012);
    at = 9643; l7016 = K[2644];
    at = 9644; l7017 = rt.gt(l7015, l7016);
    at = 9645; if (rt.truthy(l7017)) {
      at = 9646; if (rt.truthy(l7008)) {
        at = 9647; l7020 = field(rt, l7011, "points");
        at = 9648; l7019 = l7020;
      } else {
        at = 9649; l7021 = K[2645];
        at = 9650; l7019 = l7021;
      }
      at = 9651; l7022 = K[2646];
      at = 9652; l7023 = rt.array_nth(l7019, l7022);
      at = 9653; l7018 = l7023;
    } else {
      at = 9654; l7024 = K[2647];
      at = 9655; l7018 = l7024;
    }
    at = 9656; if (rt.truthy(l7008)) {
      at = 9657; l7026 = field(rt, l7011, "width");
      at = 9658; l7025 = l7026;
    } else {
      at = 9659; l7027 = K[2648];
      at = 9660; l7025 = l7027;
    }
    at = 9661; l7028 = K[2649];
    at = 9662; l7029 = rt.int_div(l7025, l7028);
    at = 9663; l7030 = K[2650];
    at = 9664; l7031 = rt.max(l7030, l7029);
    at = 9665; l7032 = K[2651];
    at = 9666; l7033 = rt.gt(l7015, l7032);
    at = 9667; if (rt.truthy(l7033)) {
      at = 9668; if (rt.truthy(l7008)) {
        at = 9669; l7036 = field(rt, l7011, "color");
        at = 9670; l7035 = l7036;
      } else {
        at = 9671; l7037 = K[2652];
        at = 9672; l7035 = l7037;
      }
      at = 9673; l7038 = K[2653];
      at = 9674; l7039 = rt.sequence_get(l7038, l7035);
      at = 9675; l7034 = l7039;
    } else {
      at = 9676; l7040 = K[2654];
      at = 9677; l7034 = l7040;
    }
    at = 9678; markSet(rt, false, "circle", {at: l7018, radius: l7031, fill: l7034});
    at = 9679; if (rt.truthy(l7008)) {
      at = 9680; l7042 = field(rt, l7011, "points");
      at = 9681; l7041 = l7042;
    } else {
      at = 9682; l7043 = K[2655];
      at = 9683; l7041 = l7043;
    }
    at = 9684; l7044 = K[2656];
    at = 9685; l7045 = rt.sub(l7015, l7044);
    at = 9686; l7046 = K[2657];
    at = 9687; l7047 = rt.max(l7046, l7045);
    at = 9688; l7048 = K[2658];
    at = 9689; l7049 = rt.array_slice(l7041, l7048, l7047);
    at = 9690; if (rt.truthy(l7008)) {
      at = 9691; l7051 = field(rt, l7011, "points");
      at = 9692; l7050 = l7051;
    } else {
      at = 9693; l7052 = K[2659];
      at = 9694; l7050 = l7052;
    }
    at = 9695; l7053 = K[2660];
    at = 9696; l7054 = rt.gt(l7015, l7053);
    at = 9697; if (rt.truthy(l7054)) {
      at = 9698; l7056 = K[2661];
      at = 9699; l7055 = l7056;
    } else {
      at = 9700; l7057 = K[2662];
      at = 9701; l7055 = l7057;
    }
    at = 9702; l7058 = K[2663];
    at = 9703; l7059 = rt.sub(l7015, l7058);
    at = 9704; l7060 = K[2664];
    at = 9705; l7061 = rt.max(l7060, l7059);
    at = 9706; l7062 = rt.array_slice(l7050, l7055, l7061);
    at = 9707; if (rt.truthy(l7008)) {
      at = 9708; l7064 = field(rt, l7011, "color");
      at = 9709; l7063 = l7064;
    } else {
      at = 9710; l7065 = K[2665];
      at = 9711; l7063 = l7065;
    }
    at = 9712; l7066 = K[2666];
    at = 9713; l7067 = rt.sequence_get(l7066, l7063);
    at = 9714; if (rt.truthy(l7008)) {
      at = 9715; l7069 = field(rt, l7011, "width");
      at = 9716; l7068 = l7069;
    } else {
      at = 9717; l7070 = K[2667];
      at = 9718; l7068 = l7070;
    }
    at = 9719; markSet(rt, true, "line", {from: l7049, to: l7062, color: l7067, width: l7068});
    at = 9720; l7071 = field(rt, l15, "strokes");
    at = 9721; l7072 = rt.sequence_length(l7071);
    at = 9722; l7073 = K[2668];
    at = 9723; l7074 = rt.lt(l7073, l7072);
    at = 9724; if (rt.truthy(l7074)) {
      at = 9725; l7075 = field(rt, l15, "strokes");
      at = 9726; l7076 = K[2669];
      at = 9727; l7077 = rt.sequence_get(l7075, l7076);
    }
    at = 9728; if (rt.truthy(l7074)) {
      at = 9729; l7079 = field(rt, l7077, "points");
      at = 9730; l7078 = l7079;
    } else {
      at = 9731; l7080 = K[2670];
      at = 9732; l7078 = l7080;
    }
    at = 9733; l7081 = rt.array_count(l7078);
    at = 9734; l7082 = K[2671];
    at = 9735; l7083 = rt.gt(l7081, l7082);
    at = 9736; if (rt.truthy(l7083)) {
      at = 9737; if (rt.truthy(l7074)) {
        at = 9738; l7086 = field(rt, l7077, "points");
        at = 9739; l7085 = l7086;
      } else {
        at = 9740; l7087 = K[2672];
        at = 9741; l7085 = l7087;
      }
      at = 9742; l7088 = K[2673];
      at = 9743; l7089 = rt.array_nth(l7085, l7088);
      at = 9744; l7084 = l7089;
    } else {
      at = 9745; l7090 = K[2674];
      at = 9746; l7084 = l7090;
    }
    at = 9747; if (rt.truthy(l7074)) {
      at = 9748; l7092 = field(rt, l7077, "width");
      at = 9749; l7091 = l7092;
    } else {
      at = 9750; l7093 = K[2675];
      at = 9751; l7091 = l7093;
    }
    at = 9752; l7094 = K[2676];
    at = 9753; l7095 = rt.int_div(l7091, l7094);
    at = 9754; l7096 = K[2677];
    at = 9755; l7097 = rt.max(l7096, l7095);
    at = 9756; l7098 = K[2678];
    at = 9757; l7099 = rt.gt(l7081, l7098);
    at = 9758; if (rt.truthy(l7099)) {
      at = 9759; if (rt.truthy(l7074)) {
        at = 9760; l7102 = field(rt, l7077, "color");
        at = 9761; l7101 = l7102;
      } else {
        at = 9762; l7103 = K[2679];
        at = 9763; l7101 = l7103;
      }
      at = 9764; l7104 = K[2680];
      at = 9765; l7105 = rt.sequence_get(l7104, l7101);
      at = 9766; l7100 = l7105;
    } else {
      at = 9767; l7106 = K[2681];
      at = 9768; l7100 = l7106;
    }
    at = 9769; markSet(rt, false, "circle", {at: l7084, radius: l7097, fill: l7100});
    at = 9770; if (rt.truthy(l7074)) {
      at = 9771; l7108 = field(rt, l7077, "points");
      at = 9772; l7107 = l7108;
    } else {
      at = 9773; l7109 = K[2682];
      at = 9774; l7107 = l7109;
    }
    at = 9775; l7110 = K[2683];
    at = 9776; l7111 = rt.sub(l7081, l7110);
    at = 9777; l7112 = K[2684];
    at = 9778; l7113 = rt.max(l7112, l7111);
    at = 9779; l7114 = K[2685];
    at = 9780; l7115 = rt.array_slice(l7107, l7114, l7113);
    at = 9781; if (rt.truthy(l7074)) {
      at = 9782; l7117 = field(rt, l7077, "points");
      at = 9783; l7116 = l7117;
    } else {
      at = 9784; l7118 = K[2686];
      at = 9785; l7116 = l7118;
    }
    at = 9786; l7119 = K[2687];
    at = 9787; l7120 = rt.gt(l7081, l7119);
    at = 9788; if (rt.truthy(l7120)) {
      at = 9789; l7122 = K[2688];
      at = 9790; l7121 = l7122;
    } else {
      at = 9791; l7123 = K[2689];
      at = 9792; l7121 = l7123;
    }
    at = 9793; l7124 = K[2690];
    at = 9794; l7125 = rt.sub(l7081, l7124);
    at = 9795; l7126 = K[2691];
    at = 9796; l7127 = rt.max(l7126, l7125);
    at = 9797; l7128 = rt.array_slice(l7116, l7121, l7127);
    at = 9798; if (rt.truthy(l7074)) {
      at = 9799; l7130 = field(rt, l7077, "color");
      at = 9800; l7129 = l7130;
    } else {
      at = 9801; l7131 = K[2692];
      at = 9802; l7129 = l7131;
    }
    at = 9803; l7132 = K[2693];
    at = 9804; l7133 = rt.sequence_get(l7132, l7129);
    at = 9805; if (rt.truthy(l7074)) {
      at = 9806; l7135 = field(rt, l7077, "width");
      at = 9807; l7134 = l7135;
    } else {
      at = 9808; l7136 = K[2694];
      at = 9809; l7134 = l7136;
    }
    at = 9810; markSet(rt, true, "line", {from: l7115, to: l7128, color: l7133, width: l7134});
    at = 9811; l7137 = field(rt, l15, "strokes");
    at = 9812; l7138 = rt.sequence_length(l7137);
    at = 9813; l7139 = K[2695];
    at = 9814; l7140 = rt.lt(l7139, l7138);
    at = 9815; if (rt.truthy(l7140)) {
      at = 9816; l7141 = field(rt, l15, "strokes");
      at = 9817; l7142 = K[2696];
      at = 9818; l7143 = rt.sequence_get(l7141, l7142);
    }
    at = 9819; if (rt.truthy(l7140)) {
      at = 9820; l7145 = field(rt, l7143, "points");
      at = 9821; l7144 = l7145;
    } else {
      at = 9822; l7146 = K[2697];
      at = 9823; l7144 = l7146;
    }
    at = 9824; l7147 = rt.array_count(l7144);
    at = 9825; l7148 = K[2698];
    at = 9826; l7149 = rt.gt(l7147, l7148);
    at = 9827; if (rt.truthy(l7149)) {
      at = 9828; if (rt.truthy(l7140)) {
        at = 9829; l7152 = field(rt, l7143, "points");
        at = 9830; l7151 = l7152;
      } else {
        at = 9831; l7153 = K[2699];
        at = 9832; l7151 = l7153;
      }
      at = 9833; l7154 = K[2700];
      at = 9834; l7155 = rt.array_nth(l7151, l7154);
      at = 9835; l7150 = l7155;
    } else {
      at = 9836; l7156 = K[2701];
      at = 9837; l7150 = l7156;
    }
    at = 9838; if (rt.truthy(l7140)) {
      at = 9839; l7158 = field(rt, l7143, "width");
      at = 9840; l7157 = l7158;
    } else {
      at = 9841; l7159 = K[2702];
      at = 9842; l7157 = l7159;
    }
    at = 9843; l7160 = K[2703];
    at = 9844; l7161 = rt.int_div(l7157, l7160);
    at = 9845; l7162 = K[2704];
    at = 9846; l7163 = rt.max(l7162, l7161);
    at = 9847; l7164 = K[2705];
    at = 9848; l7165 = rt.gt(l7147, l7164);
    at = 9849; if (rt.truthy(l7165)) {
      at = 9850; if (rt.truthy(l7140)) {
        at = 9851; l7168 = field(rt, l7143, "color");
        at = 9852; l7167 = l7168;
      } else {
        at = 9853; l7169 = K[2706];
        at = 9854; l7167 = l7169;
      }
      at = 9855; l7170 = K[2707];
      at = 9856; l7171 = rt.sequence_get(l7170, l7167);
      at = 9857; l7166 = l7171;
    } else {
      at = 9858; l7172 = K[2708];
      at = 9859; l7166 = l7172;
    }
    at = 9860; markSet(rt, false, "circle", {at: l7150, radius: l7163, fill: l7166});
    at = 9861; if (rt.truthy(l7140)) {
      at = 9862; l7174 = field(rt, l7143, "points");
      at = 9863; l7173 = l7174;
    } else {
      at = 9864; l7175 = K[2709];
      at = 9865; l7173 = l7175;
    }
    at = 9866; l7176 = K[2710];
    at = 9867; l7177 = rt.sub(l7147, l7176);
    at = 9868; l7178 = K[2711];
    at = 9869; l7179 = rt.max(l7178, l7177);
    at = 9870; l7180 = K[2712];
    at = 9871; l7181 = rt.array_slice(l7173, l7180, l7179);
    at = 9872; if (rt.truthy(l7140)) {
      at = 9873; l7183 = field(rt, l7143, "points");
      at = 9874; l7182 = l7183;
    } else {
      at = 9875; l7184 = K[2713];
      at = 9876; l7182 = l7184;
    }
    at = 9877; l7185 = K[2714];
    at = 9878; l7186 = rt.gt(l7147, l7185);
    at = 9879; if (rt.truthy(l7186)) {
      at = 9880; l7188 = K[2715];
      at = 9881; l7187 = l7188;
    } else {
      at = 9882; l7189 = K[2716];
      at = 9883; l7187 = l7189;
    }
    at = 9884; l7190 = K[2717];
    at = 9885; l7191 = rt.sub(l7147, l7190);
    at = 9886; l7192 = K[2718];
    at = 9887; l7193 = rt.max(l7192, l7191);
    at = 9888; l7194 = rt.array_slice(l7182, l7187, l7193);
    at = 9889; if (rt.truthy(l7140)) {
      at = 9890; l7196 = field(rt, l7143, "color");
      at = 9891; l7195 = l7196;
    } else {
      at = 9892; l7197 = K[2719];
      at = 9893; l7195 = l7197;
    }
    at = 9894; l7198 = K[2720];
    at = 9895; l7199 = rt.sequence_get(l7198, l7195);
    at = 9896; if (rt.truthy(l7140)) {
      at = 9897; l7201 = field(rt, l7143, "width");
      at = 9898; l7200 = l7201;
    } else {
      at = 9899; l7202 = K[2721];
      at = 9900; l7200 = l7202;
    }
    at = 9901; markSet(rt, true, "line", {from: l7181, to: l7194, color: l7199, width: l7200});
    at = 9902; l7203 = field(rt, l15, "strokes");
    at = 9903; l7204 = rt.sequence_length(l7203);
    at = 9904; l7205 = K[2722];
    at = 9905; l7206 = rt.lt(l7205, l7204);
    at = 9906; if (rt.truthy(l7206)) {
      at = 9907; l7207 = field(rt, l15, "strokes");
      at = 9908; l7208 = K[2723];
      at = 9909; l7209 = rt.sequence_get(l7207, l7208);
    }
    at = 9910; if (rt.truthy(l7206)) {
      at = 9911; l7211 = field(rt, l7209, "points");
      at = 9912; l7210 = l7211;
    } else {
      at = 9913; l7212 = K[2724];
      at = 9914; l7210 = l7212;
    }
    at = 9915; l7213 = rt.array_count(l7210);
    at = 9916; l7214 = K[2725];
    at = 9917; l7215 = rt.gt(l7213, l7214);
    at = 9918; if (rt.truthy(l7215)) {
      at = 9919; if (rt.truthy(l7206)) {
        at = 9920; l7218 = field(rt, l7209, "points");
        at = 9921; l7217 = l7218;
      } else {
        at = 9922; l7219 = K[2726];
        at = 9923; l7217 = l7219;
      }
      at = 9924; l7220 = K[2727];
      at = 9925; l7221 = rt.array_nth(l7217, l7220);
      at = 9926; l7216 = l7221;
    } else {
      at = 9927; l7222 = K[2728];
      at = 9928; l7216 = l7222;
    }
    at = 9929; if (rt.truthy(l7206)) {
      at = 9930; l7224 = field(rt, l7209, "width");
      at = 9931; l7223 = l7224;
    } else {
      at = 9932; l7225 = K[2729];
      at = 9933; l7223 = l7225;
    }
    at = 9934; l7226 = K[2730];
    at = 9935; l7227 = rt.int_div(l7223, l7226);
    at = 9936; l7228 = K[2731];
    at = 9937; l7229 = rt.max(l7228, l7227);
    at = 9938; l7230 = K[2732];
    at = 9939; l7231 = rt.gt(l7213, l7230);
    at = 9940; if (rt.truthy(l7231)) {
      at = 9941; if (rt.truthy(l7206)) {
        at = 9942; l7234 = field(rt, l7209, "color");
        at = 9943; l7233 = l7234;
      } else {
        at = 9944; l7235 = K[2733];
        at = 9945; l7233 = l7235;
      }
      at = 9946; l7236 = K[2734];
      at = 9947; l7237 = rt.sequence_get(l7236, l7233);
      at = 9948; l7232 = l7237;
    } else {
      at = 9949; l7238 = K[2735];
      at = 9950; l7232 = l7238;
    }
    at = 9951; markSet(rt, false, "circle", {at: l7216, radius: l7229, fill: l7232});
    at = 9952; if (rt.truthy(l7206)) {
      at = 9953; l7240 = field(rt, l7209, "points");
      at = 9954; l7239 = l7240;
    } else {
      at = 9955; l7241 = K[2736];
      at = 9956; l7239 = l7241;
    }
    at = 9957; l7242 = K[2737];
    at = 9958; l7243 = rt.sub(l7213, l7242);
    at = 9959; l7244 = K[2738];
    at = 9960; l7245 = rt.max(l7244, l7243);
    at = 9961; l7246 = K[2739];
    at = 9962; l7247 = rt.array_slice(l7239, l7246, l7245);
    at = 9963; if (rt.truthy(l7206)) {
      at = 9964; l7249 = field(rt, l7209, "points");
      at = 9965; l7248 = l7249;
    } else {
      at = 9966; l7250 = K[2740];
      at = 9967; l7248 = l7250;
    }
    at = 9968; l7251 = K[2741];
    at = 9969; l7252 = rt.gt(l7213, l7251);
    at = 9970; if (rt.truthy(l7252)) {
      at = 9971; l7254 = K[2742];
      at = 9972; l7253 = l7254;
    } else {
      at = 9973; l7255 = K[2743];
      at = 9974; l7253 = l7255;
    }
    at = 9975; l7256 = K[2744];
    at = 9976; l7257 = rt.sub(l7213, l7256);
    at = 9977; l7258 = K[2745];
    at = 9978; l7259 = rt.max(l7258, l7257);
    at = 9979; l7260 = rt.array_slice(l7248, l7253, l7259);
    at = 9980; if (rt.truthy(l7206)) {
      at = 9981; l7262 = field(rt, l7209, "color");
      at = 9982; l7261 = l7262;
    } else {
      at = 9983; l7263 = K[2746];
      at = 9984; l7261 = l7263;
    }
    at = 9985; l7264 = K[2747];
    at = 9986; l7265 = rt.sequence_get(l7264, l7261);
    at = 9987; if (rt.truthy(l7206)) {
      at = 9988; l7267 = field(rt, l7209, "width");
      at = 9989; l7266 = l7267;
    } else {
      at = 9990; l7268 = K[2748];
      at = 9991; l7266 = l7268;
    }
    at = 9992; markSet(rt, true, "line", {from: l7247, to: l7260, color: l7265, width: l7266});
    at = 9993; l7269 = field(rt, l15, "strokes");
    at = 9994; l7270 = rt.sequence_length(l7269);
    at = 9995; l7271 = K[2749];
    at = 9996; l7272 = rt.lt(l7271, l7270);
    at = 9997; if (rt.truthy(l7272)) {
      at = 9998; l7273 = field(rt, l15, "strokes");
      at = 9999; l7274 = K[2750];
      at = 10000; l7275 = rt.sequence_get(l7273, l7274);
    }
    at = 10001; if (rt.truthy(l7272)) {
      at = 10002; l7277 = field(rt, l7275, "points");
      at = 10003; l7276 = l7277;
    } else {
      at = 10004; l7278 = K[2751];
      at = 10005; l7276 = l7278;
    }
    at = 10006; l7279 = rt.array_count(l7276);
    at = 10007; l7280 = K[2752];
    at = 10008; l7281 = rt.gt(l7279, l7280);
    at = 10009; if (rt.truthy(l7281)) {
      at = 10010; if (rt.truthy(l7272)) {
        at = 10011; l7284 = field(rt, l7275, "points");
        at = 10012; l7283 = l7284;
      } else {
        at = 10013; l7285 = K[2753];
        at = 10014; l7283 = l7285;
      }
      at = 10015; l7286 = K[2754];
      at = 10016; l7287 = rt.array_nth(l7283, l7286);
      at = 10017; l7282 = l7287;
    } else {
      at = 10018; l7288 = K[2755];
      at = 10019; l7282 = l7288;
    }
    at = 10020; if (rt.truthy(l7272)) {
      at = 10021; l7290 = field(rt, l7275, "width");
      at = 10022; l7289 = l7290;
    } else {
      at = 10023; l7291 = K[2756];
      at = 10024; l7289 = l7291;
    }
    at = 10025; l7292 = K[2757];
    at = 10026; l7293 = rt.int_div(l7289, l7292);
    at = 10027; l7294 = K[2758];
    at = 10028; l7295 = rt.max(l7294, l7293);
    at = 10029; l7296 = K[2759];
    at = 10030; l7297 = rt.gt(l7279, l7296);
    at = 10031; if (rt.truthy(l7297)) {
      at = 10032; if (rt.truthy(l7272)) {
        at = 10033; l7300 = field(rt, l7275, "color");
        at = 10034; l7299 = l7300;
      } else {
        at = 10035; l7301 = K[2760];
        at = 10036; l7299 = l7301;
      }
      at = 10037; l7302 = K[2761];
      at = 10038; l7303 = rt.sequence_get(l7302, l7299);
      at = 10039; l7298 = l7303;
    } else {
      at = 10040; l7304 = K[2762];
      at = 10041; l7298 = l7304;
    }
    at = 10042; markSet(rt, false, "circle", {at: l7282, radius: l7295, fill: l7298});
    at = 10043; if (rt.truthy(l7272)) {
      at = 10044; l7306 = field(rt, l7275, "points");
      at = 10045; l7305 = l7306;
    } else {
      at = 10046; l7307 = K[2763];
      at = 10047; l7305 = l7307;
    }
    at = 10048; l7308 = K[2764];
    at = 10049; l7309 = rt.sub(l7279, l7308);
    at = 10050; l7310 = K[2765];
    at = 10051; l7311 = rt.max(l7310, l7309);
    at = 10052; l7312 = K[2766];
    at = 10053; l7313 = rt.array_slice(l7305, l7312, l7311);
    at = 10054; if (rt.truthy(l7272)) {
      at = 10055; l7315 = field(rt, l7275, "points");
      at = 10056; l7314 = l7315;
    } else {
      at = 10057; l7316 = K[2767];
      at = 10058; l7314 = l7316;
    }
    at = 10059; l7317 = K[2768];
    at = 10060; l7318 = rt.gt(l7279, l7317);
    at = 10061; if (rt.truthy(l7318)) {
      at = 10062; l7320 = K[2769];
      at = 10063; l7319 = l7320;
    } else {
      at = 10064; l7321 = K[2770];
      at = 10065; l7319 = l7321;
    }
    at = 10066; l7322 = K[2771];
    at = 10067; l7323 = rt.sub(l7279, l7322);
    at = 10068; l7324 = K[2772];
    at = 10069; l7325 = rt.max(l7324, l7323);
    at = 10070; l7326 = rt.array_slice(l7314, l7319, l7325);
    at = 10071; if (rt.truthy(l7272)) {
      at = 10072; l7328 = field(rt, l7275, "color");
      at = 10073; l7327 = l7328;
    } else {
      at = 10074; l7329 = K[2773];
      at = 10075; l7327 = l7329;
    }
    at = 10076; l7330 = K[2774];
    at = 10077; l7331 = rt.sequence_get(l7330, l7327);
    at = 10078; if (rt.truthy(l7272)) {
      at = 10079; l7333 = field(rt, l7275, "width");
      at = 10080; l7332 = l7333;
    } else {
      at = 10081; l7334 = K[2775];
      at = 10082; l7332 = l7334;
    }
    at = 10083; markSet(rt, true, "line", {from: l7313, to: l7326, color: l7331, width: l7332});
    at = 10084; l7335 = field(rt, l15, "strokes");
    at = 10085; l7336 = rt.sequence_length(l7335);
    at = 10086; l7337 = K[2776];
    at = 10087; l7338 = rt.lt(l7337, l7336);
    at = 10088; if (rt.truthy(l7338)) {
      at = 10089; l7339 = field(rt, l15, "strokes");
      at = 10090; l7340 = K[2777];
      at = 10091; l7341 = rt.sequence_get(l7339, l7340);
    }
    at = 10092; if (rt.truthy(l7338)) {
      at = 10093; l7343 = field(rt, l7341, "points");
      at = 10094; l7342 = l7343;
    } else {
      at = 10095; l7344 = K[2778];
      at = 10096; l7342 = l7344;
    }
    at = 10097; l7345 = rt.array_count(l7342);
    at = 10098; l7346 = K[2779];
    at = 10099; l7347 = rt.gt(l7345, l7346);
    at = 10100; if (rt.truthy(l7347)) {
      at = 10101; if (rt.truthy(l7338)) {
        at = 10102; l7350 = field(rt, l7341, "points");
        at = 10103; l7349 = l7350;
      } else {
        at = 10104; l7351 = K[2780];
        at = 10105; l7349 = l7351;
      }
      at = 10106; l7352 = K[2781];
      at = 10107; l7353 = rt.array_nth(l7349, l7352);
      at = 10108; l7348 = l7353;
    } else {
      at = 10109; l7354 = K[2782];
      at = 10110; l7348 = l7354;
    }
    at = 10111; if (rt.truthy(l7338)) {
      at = 10112; l7356 = field(rt, l7341, "width");
      at = 10113; l7355 = l7356;
    } else {
      at = 10114; l7357 = K[2783];
      at = 10115; l7355 = l7357;
    }
    at = 10116; l7358 = K[2784];
    at = 10117; l7359 = rt.int_div(l7355, l7358);
    at = 10118; l7360 = K[2785];
    at = 10119; l7361 = rt.max(l7360, l7359);
    at = 10120; l7362 = K[2786];
    at = 10121; l7363 = rt.gt(l7345, l7362);
    at = 10122; if (rt.truthy(l7363)) {
      at = 10123; if (rt.truthy(l7338)) {
        at = 10124; l7366 = field(rt, l7341, "color");
        at = 10125; l7365 = l7366;
      } else {
        at = 10126; l7367 = K[2787];
        at = 10127; l7365 = l7367;
      }
      at = 10128; l7368 = K[2788];
      at = 10129; l7369 = rt.sequence_get(l7368, l7365);
      at = 10130; l7364 = l7369;
    } else {
      at = 10131; l7370 = K[2789];
      at = 10132; l7364 = l7370;
    }
    at = 10133; markSet(rt, false, "circle", {at: l7348, radius: l7361, fill: l7364});
    at = 10134; if (rt.truthy(l7338)) {
      at = 10135; l7372 = field(rt, l7341, "points");
      at = 10136; l7371 = l7372;
    } else {
      at = 10137; l7373 = K[2790];
      at = 10138; l7371 = l7373;
    }
    at = 10139; l7374 = K[2791];
    at = 10140; l7375 = rt.sub(l7345, l7374);
    at = 10141; l7376 = K[2792];
    at = 10142; l7377 = rt.max(l7376, l7375);
    at = 10143; l7378 = K[2793];
    at = 10144; l7379 = rt.array_slice(l7371, l7378, l7377);
    at = 10145; if (rt.truthy(l7338)) {
      at = 10146; l7381 = field(rt, l7341, "points");
      at = 10147; l7380 = l7381;
    } else {
      at = 10148; l7382 = K[2794];
      at = 10149; l7380 = l7382;
    }
    at = 10150; l7383 = K[2795];
    at = 10151; l7384 = rt.gt(l7345, l7383);
    at = 10152; if (rt.truthy(l7384)) {
      at = 10153; l7386 = K[2796];
      at = 10154; l7385 = l7386;
    } else {
      at = 10155; l7387 = K[2797];
      at = 10156; l7385 = l7387;
    }
    at = 10157; l7388 = K[2798];
    at = 10158; l7389 = rt.sub(l7345, l7388);
    at = 10159; l7390 = K[2799];
    at = 10160; l7391 = rt.max(l7390, l7389);
    at = 10161; l7392 = rt.array_slice(l7380, l7385, l7391);
    at = 10162; if (rt.truthy(l7338)) {
      at = 10163; l7394 = field(rt, l7341, "color");
      at = 10164; l7393 = l7394;
    } else {
      at = 10165; l7395 = K[2800];
      at = 10166; l7393 = l7395;
    }
    at = 10167; l7396 = K[2801];
    at = 10168; l7397 = rt.sequence_get(l7396, l7393);
    at = 10169; if (rt.truthy(l7338)) {
      at = 10170; l7399 = field(rt, l7341, "width");
      at = 10171; l7398 = l7399;
    } else {
      at = 10172; l7400 = K[2802];
      at = 10173; l7398 = l7400;
    }
    at = 10174; markSet(rt, true, "line", {from: l7379, to: l7392, color: l7397, width: l7398});
    at = 10175; l7401 = field(rt, l15, "strokes");
    at = 10176; l7402 = rt.sequence_length(l7401);
    at = 10177; l7403 = K[2803];
    at = 10178; l7404 = rt.lt(l7403, l7402);
    at = 10179; if (rt.truthy(l7404)) {
      at = 10180; l7405 = field(rt, l15, "strokes");
      at = 10181; l7406 = K[2804];
      at = 10182; l7407 = rt.sequence_get(l7405, l7406);
    }
    at = 10183; if (rt.truthy(l7404)) {
      at = 10184; l7409 = field(rt, l7407, "points");
      at = 10185; l7408 = l7409;
    } else {
      at = 10186; l7410 = K[2805];
      at = 10187; l7408 = l7410;
    }
    at = 10188; l7411 = rt.array_count(l7408);
    at = 10189; l7412 = K[2806];
    at = 10190; l7413 = rt.gt(l7411, l7412);
    at = 10191; if (rt.truthy(l7413)) {
      at = 10192; if (rt.truthy(l7404)) {
        at = 10193; l7416 = field(rt, l7407, "points");
        at = 10194; l7415 = l7416;
      } else {
        at = 10195; l7417 = K[2807];
        at = 10196; l7415 = l7417;
      }
      at = 10197; l7418 = K[2808];
      at = 10198; l7419 = rt.array_nth(l7415, l7418);
      at = 10199; l7414 = l7419;
    } else {
      at = 10200; l7420 = K[2809];
      at = 10201; l7414 = l7420;
    }
    at = 10202; if (rt.truthy(l7404)) {
      at = 10203; l7422 = field(rt, l7407, "width");
      at = 10204; l7421 = l7422;
    } else {
      at = 10205; l7423 = K[2810];
      at = 10206; l7421 = l7423;
    }
    at = 10207; l7424 = K[2811];
    at = 10208; l7425 = rt.int_div(l7421, l7424);
    at = 10209; l7426 = K[2812];
    at = 10210; l7427 = rt.max(l7426, l7425);
    at = 10211; l7428 = K[2813];
    at = 10212; l7429 = rt.gt(l7411, l7428);
    at = 10213; if (rt.truthy(l7429)) {
      at = 10214; if (rt.truthy(l7404)) {
        at = 10215; l7432 = field(rt, l7407, "color");
        at = 10216; l7431 = l7432;
      } else {
        at = 10217; l7433 = K[2814];
        at = 10218; l7431 = l7433;
      }
      at = 10219; l7434 = K[2815];
      at = 10220; l7435 = rt.sequence_get(l7434, l7431);
      at = 10221; l7430 = l7435;
    } else {
      at = 10222; l7436 = K[2816];
      at = 10223; l7430 = l7436;
    }
    at = 10224; markSet(rt, false, "circle", {at: l7414, radius: l7427, fill: l7430});
    at = 10225; if (rt.truthy(l7404)) {
      at = 10226; l7438 = field(rt, l7407, "points");
      at = 10227; l7437 = l7438;
    } else {
      at = 10228; l7439 = K[2817];
      at = 10229; l7437 = l7439;
    }
    at = 10230; l7440 = K[2818];
    at = 10231; l7441 = rt.sub(l7411, l7440);
    at = 10232; l7442 = K[2819];
    at = 10233; l7443 = rt.max(l7442, l7441);
    at = 10234; l7444 = K[2820];
    at = 10235; l7445 = rt.array_slice(l7437, l7444, l7443);
    at = 10236; if (rt.truthy(l7404)) {
      at = 10237; l7447 = field(rt, l7407, "points");
      at = 10238; l7446 = l7447;
    } else {
      at = 10239; l7448 = K[2821];
      at = 10240; l7446 = l7448;
    }
    at = 10241; l7449 = K[2822];
    at = 10242; l7450 = rt.gt(l7411, l7449);
    at = 10243; if (rt.truthy(l7450)) {
      at = 10244; l7452 = K[2823];
      at = 10245; l7451 = l7452;
    } else {
      at = 10246; l7453 = K[2824];
      at = 10247; l7451 = l7453;
    }
    at = 10248; l7454 = K[2825];
    at = 10249; l7455 = rt.sub(l7411, l7454);
    at = 10250; l7456 = K[2826];
    at = 10251; l7457 = rt.max(l7456, l7455);
    at = 10252; l7458 = rt.array_slice(l7446, l7451, l7457);
    at = 10253; if (rt.truthy(l7404)) {
      at = 10254; l7460 = field(rt, l7407, "color");
      at = 10255; l7459 = l7460;
    } else {
      at = 10256; l7461 = K[2827];
      at = 10257; l7459 = l7461;
    }
    at = 10258; l7462 = K[2828];
    at = 10259; l7463 = rt.sequence_get(l7462, l7459);
    at = 10260; if (rt.truthy(l7404)) {
      at = 10261; l7465 = field(rt, l7407, "width");
      at = 10262; l7464 = l7465;
    } else {
      at = 10263; l7466 = K[2829];
      at = 10264; l7464 = l7466;
    }
    at = 10265; markSet(rt, true, "line", {from: l7445, to: l7458, color: l7463, width: l7464});
    at = 10266; l7467 = field(rt, l15, "strokes");
    at = 10267; l7468 = rt.sequence_length(l7467);
    at = 10268; l7469 = K[2830];
    at = 10269; l7470 = rt.lt(l7469, l7468);
    at = 10270; if (rt.truthy(l7470)) {
      at = 10271; l7471 = field(rt, l15, "strokes");
      at = 10272; l7472 = K[2831];
      at = 10273; l7473 = rt.sequence_get(l7471, l7472);
    }
    at = 10274; if (rt.truthy(l7470)) {
      at = 10275; l7475 = field(rt, l7473, "points");
      at = 10276; l7474 = l7475;
    } else {
      at = 10277; l7476 = K[2832];
      at = 10278; l7474 = l7476;
    }
    at = 10279; l7477 = rt.array_count(l7474);
    at = 10280; l7478 = K[2833];
    at = 10281; l7479 = rt.gt(l7477, l7478);
    at = 10282; if (rt.truthy(l7479)) {
      at = 10283; if (rt.truthy(l7470)) {
        at = 10284; l7482 = field(rt, l7473, "points");
        at = 10285; l7481 = l7482;
      } else {
        at = 10286; l7483 = K[2834];
        at = 10287; l7481 = l7483;
      }
      at = 10288; l7484 = K[2835];
      at = 10289; l7485 = rt.array_nth(l7481, l7484);
      at = 10290; l7480 = l7485;
    } else {
      at = 10291; l7486 = K[2836];
      at = 10292; l7480 = l7486;
    }
    at = 10293; if (rt.truthy(l7470)) {
      at = 10294; l7488 = field(rt, l7473, "width");
      at = 10295; l7487 = l7488;
    } else {
      at = 10296; l7489 = K[2837];
      at = 10297; l7487 = l7489;
    }
    at = 10298; l7490 = K[2838];
    at = 10299; l7491 = rt.int_div(l7487, l7490);
    at = 10300; l7492 = K[2839];
    at = 10301; l7493 = rt.max(l7492, l7491);
    at = 10302; l7494 = K[2840];
    at = 10303; l7495 = rt.gt(l7477, l7494);
    at = 10304; if (rt.truthy(l7495)) {
      at = 10305; if (rt.truthy(l7470)) {
        at = 10306; l7498 = field(rt, l7473, "color");
        at = 10307; l7497 = l7498;
      } else {
        at = 10308; l7499 = K[2841];
        at = 10309; l7497 = l7499;
      }
      at = 10310; l7500 = K[2842];
      at = 10311; l7501 = rt.sequence_get(l7500, l7497);
      at = 10312; l7496 = l7501;
    } else {
      at = 10313; l7502 = K[2843];
      at = 10314; l7496 = l7502;
    }
    at = 10315; markSet(rt, false, "circle", {at: l7480, radius: l7493, fill: l7496});
    at = 10316; if (rt.truthy(l7470)) {
      at = 10317; l7504 = field(rt, l7473, "points");
      at = 10318; l7503 = l7504;
    } else {
      at = 10319; l7505 = K[2844];
      at = 10320; l7503 = l7505;
    }
    at = 10321; l7506 = K[2845];
    at = 10322; l7507 = rt.sub(l7477, l7506);
    at = 10323; l7508 = K[2846];
    at = 10324; l7509 = rt.max(l7508, l7507);
    at = 10325; l7510 = K[2847];
    at = 10326; l7511 = rt.array_slice(l7503, l7510, l7509);
    at = 10327; if (rt.truthy(l7470)) {
      at = 10328; l7513 = field(rt, l7473, "points");
      at = 10329; l7512 = l7513;
    } else {
      at = 10330; l7514 = K[2848];
      at = 10331; l7512 = l7514;
    }
    at = 10332; l7515 = K[2849];
    at = 10333; l7516 = rt.gt(l7477, l7515);
    at = 10334; if (rt.truthy(l7516)) {
      at = 10335; l7518 = K[2850];
      at = 10336; l7517 = l7518;
    } else {
      at = 10337; l7519 = K[2851];
      at = 10338; l7517 = l7519;
    }
    at = 10339; l7520 = K[2852];
    at = 10340; l7521 = rt.sub(l7477, l7520);
    at = 10341; l7522 = K[2853];
    at = 10342; l7523 = rt.max(l7522, l7521);
    at = 10343; l7524 = rt.array_slice(l7512, l7517, l7523);
    at = 10344; if (rt.truthy(l7470)) {
      at = 10345; l7526 = field(rt, l7473, "color");
      at = 10346; l7525 = l7526;
    } else {
      at = 10347; l7527 = K[2854];
      at = 10348; l7525 = l7527;
    }
    at = 10349; l7528 = K[2855];
    at = 10350; l7529 = rt.sequence_get(l7528, l7525);
    at = 10351; if (rt.truthy(l7470)) {
      at = 10352; l7531 = field(rt, l7473, "width");
      at = 10353; l7530 = l7531;
    } else {
      at = 10354; l7532 = K[2856];
      at = 10355; l7530 = l7532;
    }
    at = 10356; markSet(rt, true, "line", {from: l7511, to: l7524, color: l7529, width: l7530});
    at = 10357; l7533 = field(rt, l15, "strokes");
    at = 10358; l7534 = rt.sequence_length(l7533);
    at = 10359; l7535 = K[2857];
    at = 10360; l7536 = rt.lt(l7535, l7534);
    at = 10361; if (rt.truthy(l7536)) {
      at = 10362; l7537 = field(rt, l15, "strokes");
      at = 10363; l7538 = K[2858];
      at = 10364; l7539 = rt.sequence_get(l7537, l7538);
    }
    at = 10365; if (rt.truthy(l7536)) {
      at = 10366; l7541 = field(rt, l7539, "points");
      at = 10367; l7540 = l7541;
    } else {
      at = 10368; l7542 = K[2859];
      at = 10369; l7540 = l7542;
    }
    at = 10370; l7543 = rt.array_count(l7540);
    at = 10371; l7544 = K[2860];
    at = 10372; l7545 = rt.gt(l7543, l7544);
    at = 10373; if (rt.truthy(l7545)) {
      at = 10374; if (rt.truthy(l7536)) {
        at = 10375; l7548 = field(rt, l7539, "points");
        at = 10376; l7547 = l7548;
      } else {
        at = 10377; l7549 = K[2861];
        at = 10378; l7547 = l7549;
      }
      at = 10379; l7550 = K[2862];
      at = 10380; l7551 = rt.array_nth(l7547, l7550);
      at = 10381; l7546 = l7551;
    } else {
      at = 10382; l7552 = K[2863];
      at = 10383; l7546 = l7552;
    }
    at = 10384; if (rt.truthy(l7536)) {
      at = 10385; l7554 = field(rt, l7539, "width");
      at = 10386; l7553 = l7554;
    } else {
      at = 10387; l7555 = K[2864];
      at = 10388; l7553 = l7555;
    }
    at = 10389; l7556 = K[2865];
    at = 10390; l7557 = rt.int_div(l7553, l7556);
    at = 10391; l7558 = K[2866];
    at = 10392; l7559 = rt.max(l7558, l7557);
    at = 10393; l7560 = K[2867];
    at = 10394; l7561 = rt.gt(l7543, l7560);
    at = 10395; if (rt.truthy(l7561)) {
      at = 10396; if (rt.truthy(l7536)) {
        at = 10397; l7564 = field(rt, l7539, "color");
        at = 10398; l7563 = l7564;
      } else {
        at = 10399; l7565 = K[2868];
        at = 10400; l7563 = l7565;
      }
      at = 10401; l7566 = K[2869];
      at = 10402; l7567 = rt.sequence_get(l7566, l7563);
      at = 10403; l7562 = l7567;
    } else {
      at = 10404; l7568 = K[2870];
      at = 10405; l7562 = l7568;
    }
    at = 10406; markSet(rt, false, "circle", {at: l7546, radius: l7559, fill: l7562});
    at = 10407; if (rt.truthy(l7536)) {
      at = 10408; l7570 = field(rt, l7539, "points");
      at = 10409; l7569 = l7570;
    } else {
      at = 10410; l7571 = K[2871];
      at = 10411; l7569 = l7571;
    }
    at = 10412; l7572 = K[2872];
    at = 10413; l7573 = rt.sub(l7543, l7572);
    at = 10414; l7574 = K[2873];
    at = 10415; l7575 = rt.max(l7574, l7573);
    at = 10416; l7576 = K[2874];
    at = 10417; l7577 = rt.array_slice(l7569, l7576, l7575);
    at = 10418; if (rt.truthy(l7536)) {
      at = 10419; l7579 = field(rt, l7539, "points");
      at = 10420; l7578 = l7579;
    } else {
      at = 10421; l7580 = K[2875];
      at = 10422; l7578 = l7580;
    }
    at = 10423; l7581 = K[2876];
    at = 10424; l7582 = rt.gt(l7543, l7581);
    at = 10425; if (rt.truthy(l7582)) {
      at = 10426; l7584 = K[2877];
      at = 10427; l7583 = l7584;
    } else {
      at = 10428; l7585 = K[2878];
      at = 10429; l7583 = l7585;
    }
    at = 10430; l7586 = K[2879];
    at = 10431; l7587 = rt.sub(l7543, l7586);
    at = 10432; l7588 = K[2880];
    at = 10433; l7589 = rt.max(l7588, l7587);
    at = 10434; l7590 = rt.array_slice(l7578, l7583, l7589);
    at = 10435; if (rt.truthy(l7536)) {
      at = 10436; l7592 = field(rt, l7539, "color");
      at = 10437; l7591 = l7592;
    } else {
      at = 10438; l7593 = K[2881];
      at = 10439; l7591 = l7593;
    }
    at = 10440; l7594 = K[2882];
    at = 10441; l7595 = rt.sequence_get(l7594, l7591);
    at = 10442; if (rt.truthy(l7536)) {
      at = 10443; l7597 = field(rt, l7539, "width");
      at = 10444; l7596 = l7597;
    } else {
      at = 10445; l7598 = K[2883];
      at = 10446; l7596 = l7598;
    }
    at = 10447; markSet(rt, true, "line", {from: l7577, to: l7590, color: l7595, width: l7596});
    at = 10448; l7599 = field(rt, l15, "strokes");
    at = 10449; l7600 = rt.sequence_length(l7599);
    at = 10450; l7601 = K[2884];
    at = 10451; l7602 = rt.lt(l7601, l7600);
    at = 10452; if (rt.truthy(l7602)) {
      at = 10453; l7603 = field(rt, l15, "strokes");
      at = 10454; l7604 = K[2885];
      at = 10455; l7605 = rt.sequence_get(l7603, l7604);
    }
    at = 10456; if (rt.truthy(l7602)) {
      at = 10457; l7607 = field(rt, l7605, "points");
      at = 10458; l7606 = l7607;
    } else {
      at = 10459; l7608 = K[2886];
      at = 10460; l7606 = l7608;
    }
    at = 10461; l7609 = rt.array_count(l7606);
    at = 10462; l7610 = K[2887];
    at = 10463; l7611 = rt.gt(l7609, l7610);
    at = 10464; if (rt.truthy(l7611)) {
      at = 10465; if (rt.truthy(l7602)) {
        at = 10466; l7614 = field(rt, l7605, "points");
        at = 10467; l7613 = l7614;
      } else {
        at = 10468; l7615 = K[2888];
        at = 10469; l7613 = l7615;
      }
      at = 10470; l7616 = K[2889];
      at = 10471; l7617 = rt.array_nth(l7613, l7616);
      at = 10472; l7612 = l7617;
    } else {
      at = 10473; l7618 = K[2890];
      at = 10474; l7612 = l7618;
    }
    at = 10475; if (rt.truthy(l7602)) {
      at = 10476; l7620 = field(rt, l7605, "width");
      at = 10477; l7619 = l7620;
    } else {
      at = 10478; l7621 = K[2891];
      at = 10479; l7619 = l7621;
    }
    at = 10480; l7622 = K[2892];
    at = 10481; l7623 = rt.int_div(l7619, l7622);
    at = 10482; l7624 = K[2893];
    at = 10483; l7625 = rt.max(l7624, l7623);
    at = 10484; l7626 = K[2894];
    at = 10485; l7627 = rt.gt(l7609, l7626);
    at = 10486; if (rt.truthy(l7627)) {
      at = 10487; if (rt.truthy(l7602)) {
        at = 10488; l7630 = field(rt, l7605, "color");
        at = 10489; l7629 = l7630;
      } else {
        at = 10490; l7631 = K[2895];
        at = 10491; l7629 = l7631;
      }
      at = 10492; l7632 = K[2896];
      at = 10493; l7633 = rt.sequence_get(l7632, l7629);
      at = 10494; l7628 = l7633;
    } else {
      at = 10495; l7634 = K[2897];
      at = 10496; l7628 = l7634;
    }
    at = 10497; markSet(rt, false, "circle", {at: l7612, radius: l7625, fill: l7628});
    at = 10498; if (rt.truthy(l7602)) {
      at = 10499; l7636 = field(rt, l7605, "points");
      at = 10500; l7635 = l7636;
    } else {
      at = 10501; l7637 = K[2898];
      at = 10502; l7635 = l7637;
    }
    at = 10503; l7638 = K[2899];
    at = 10504; l7639 = rt.sub(l7609, l7638);
    at = 10505; l7640 = K[2900];
    at = 10506; l7641 = rt.max(l7640, l7639);
    at = 10507; l7642 = K[2901];
    at = 10508; l7643 = rt.array_slice(l7635, l7642, l7641);
    at = 10509; if (rt.truthy(l7602)) {
      at = 10510; l7645 = field(rt, l7605, "points");
      at = 10511; l7644 = l7645;
    } else {
      at = 10512; l7646 = K[2902];
      at = 10513; l7644 = l7646;
    }
    at = 10514; l7647 = K[2903];
    at = 10515; l7648 = rt.gt(l7609, l7647);
    at = 10516; if (rt.truthy(l7648)) {
      at = 10517; l7650 = K[2904];
      at = 10518; l7649 = l7650;
    } else {
      at = 10519; l7651 = K[2905];
      at = 10520; l7649 = l7651;
    }
    at = 10521; l7652 = K[2906];
    at = 10522; l7653 = rt.sub(l7609, l7652);
    at = 10523; l7654 = K[2907];
    at = 10524; l7655 = rt.max(l7654, l7653);
    at = 10525; l7656 = rt.array_slice(l7644, l7649, l7655);
    at = 10526; if (rt.truthy(l7602)) {
      at = 10527; l7658 = field(rt, l7605, "color");
      at = 10528; l7657 = l7658;
    } else {
      at = 10529; l7659 = K[2908];
      at = 10530; l7657 = l7659;
    }
    at = 10531; l7660 = K[2909];
    at = 10532; l7661 = rt.sequence_get(l7660, l7657);
    at = 10533; if (rt.truthy(l7602)) {
      at = 10534; l7663 = field(rt, l7605, "width");
      at = 10535; l7662 = l7663;
    } else {
      at = 10536; l7664 = K[2910];
      at = 10537; l7662 = l7664;
    }
    at = 10538; markSet(rt, true, "line", {from: l7643, to: l7656, color: l7661, width: l7662});
    at = 10539; l7665 = field(rt, l15, "strokes");
    at = 10540; l7666 = rt.sequence_length(l7665);
    at = 10541; l7667 = K[2911];
    at = 10542; l7668 = rt.lt(l7667, l7666);
    at = 10543; if (rt.truthy(l7668)) {
      at = 10544; l7669 = field(rt, l15, "strokes");
      at = 10545; l7670 = K[2912];
      at = 10546; l7671 = rt.sequence_get(l7669, l7670);
    }
    at = 10547; if (rt.truthy(l7668)) {
      at = 10548; l7673 = field(rt, l7671, "points");
      at = 10549; l7672 = l7673;
    } else {
      at = 10550; l7674 = K[2913];
      at = 10551; l7672 = l7674;
    }
    at = 10552; l7675 = rt.array_count(l7672);
    at = 10553; l7676 = K[2914];
    at = 10554; l7677 = rt.gt(l7675, l7676);
    at = 10555; if (rt.truthy(l7677)) {
      at = 10556; if (rt.truthy(l7668)) {
        at = 10557; l7680 = field(rt, l7671, "points");
        at = 10558; l7679 = l7680;
      } else {
        at = 10559; l7681 = K[2915];
        at = 10560; l7679 = l7681;
      }
      at = 10561; l7682 = K[2916];
      at = 10562; l7683 = rt.array_nth(l7679, l7682);
      at = 10563; l7678 = l7683;
    } else {
      at = 10564; l7684 = K[2917];
      at = 10565; l7678 = l7684;
    }
    at = 10566; if (rt.truthy(l7668)) {
      at = 10567; l7686 = field(rt, l7671, "width");
      at = 10568; l7685 = l7686;
    } else {
      at = 10569; l7687 = K[2918];
      at = 10570; l7685 = l7687;
    }
    at = 10571; l7688 = K[2919];
    at = 10572; l7689 = rt.int_div(l7685, l7688);
    at = 10573; l7690 = K[2920];
    at = 10574; l7691 = rt.max(l7690, l7689);
    at = 10575; l7692 = K[2921];
    at = 10576; l7693 = rt.gt(l7675, l7692);
    at = 10577; if (rt.truthy(l7693)) {
      at = 10578; if (rt.truthy(l7668)) {
        at = 10579; l7696 = field(rt, l7671, "color");
        at = 10580; l7695 = l7696;
      } else {
        at = 10581; l7697 = K[2922];
        at = 10582; l7695 = l7697;
      }
      at = 10583; l7698 = K[2923];
      at = 10584; l7699 = rt.sequence_get(l7698, l7695);
      at = 10585; l7694 = l7699;
    } else {
      at = 10586; l7700 = K[2924];
      at = 10587; l7694 = l7700;
    }
    at = 10588; markSet(rt, false, "circle", {at: l7678, radius: l7691, fill: l7694});
    at = 10589; if (rt.truthy(l7668)) {
      at = 10590; l7702 = field(rt, l7671, "points");
      at = 10591; l7701 = l7702;
    } else {
      at = 10592; l7703 = K[2925];
      at = 10593; l7701 = l7703;
    }
    at = 10594; l7704 = K[2926];
    at = 10595; l7705 = rt.sub(l7675, l7704);
    at = 10596; l7706 = K[2927];
    at = 10597; l7707 = rt.max(l7706, l7705);
    at = 10598; l7708 = K[2928];
    at = 10599; l7709 = rt.array_slice(l7701, l7708, l7707);
    at = 10600; if (rt.truthy(l7668)) {
      at = 10601; l7711 = field(rt, l7671, "points");
      at = 10602; l7710 = l7711;
    } else {
      at = 10603; l7712 = K[2929];
      at = 10604; l7710 = l7712;
    }
    at = 10605; l7713 = K[2930];
    at = 10606; l7714 = rt.gt(l7675, l7713);
    at = 10607; if (rt.truthy(l7714)) {
      at = 10608; l7716 = K[2931];
      at = 10609; l7715 = l7716;
    } else {
      at = 10610; l7717 = K[2932];
      at = 10611; l7715 = l7717;
    }
    at = 10612; l7718 = K[2933];
    at = 10613; l7719 = rt.sub(l7675, l7718);
    at = 10614; l7720 = K[2934];
    at = 10615; l7721 = rt.max(l7720, l7719);
    at = 10616; l7722 = rt.array_slice(l7710, l7715, l7721);
    at = 10617; if (rt.truthy(l7668)) {
      at = 10618; l7724 = field(rt, l7671, "color");
      at = 10619; l7723 = l7724;
    } else {
      at = 10620; l7725 = K[2935];
      at = 10621; l7723 = l7725;
    }
    at = 10622; l7726 = K[2936];
    at = 10623; l7727 = rt.sequence_get(l7726, l7723);
    at = 10624; if (rt.truthy(l7668)) {
      at = 10625; l7729 = field(rt, l7671, "width");
      at = 10626; l7728 = l7729;
    } else {
      at = 10627; l7730 = K[2937];
      at = 10628; l7728 = l7730;
    }
    at = 10629; markSet(rt, true, "line", {from: l7709, to: l7722, color: l7727, width: l7728});
    at = 10630; l7731 = field(rt, l15, "strokes");
    at = 10631; l7732 = rt.sequence_length(l7731);
    at = 10632; l7733 = K[2938];
    at = 10633; l7734 = rt.lt(l7733, l7732);
    at = 10634; if (rt.truthy(l7734)) {
      at = 10635; l7735 = field(rt, l15, "strokes");
      at = 10636; l7736 = K[2939];
      at = 10637; l7737 = rt.sequence_get(l7735, l7736);
    }
    at = 10638; if (rt.truthy(l7734)) {
      at = 10639; l7739 = field(rt, l7737, "points");
      at = 10640; l7738 = l7739;
    } else {
      at = 10641; l7740 = K[2940];
      at = 10642; l7738 = l7740;
    }
    at = 10643; l7741 = rt.array_count(l7738);
    at = 10644; l7742 = K[2941];
    at = 10645; l7743 = rt.gt(l7741, l7742);
    at = 10646; if (rt.truthy(l7743)) {
      at = 10647; if (rt.truthy(l7734)) {
        at = 10648; l7746 = field(rt, l7737, "points");
        at = 10649; l7745 = l7746;
      } else {
        at = 10650; l7747 = K[2942];
        at = 10651; l7745 = l7747;
      }
      at = 10652; l7748 = K[2943];
      at = 10653; l7749 = rt.array_nth(l7745, l7748);
      at = 10654; l7744 = l7749;
    } else {
      at = 10655; l7750 = K[2944];
      at = 10656; l7744 = l7750;
    }
    at = 10657; if (rt.truthy(l7734)) {
      at = 10658; l7752 = field(rt, l7737, "width");
      at = 10659; l7751 = l7752;
    } else {
      at = 10660; l7753 = K[2945];
      at = 10661; l7751 = l7753;
    }
    at = 10662; l7754 = K[2946];
    at = 10663; l7755 = rt.int_div(l7751, l7754);
    at = 10664; l7756 = K[2947];
    at = 10665; l7757 = rt.max(l7756, l7755);
    at = 10666; l7758 = K[2948];
    at = 10667; l7759 = rt.gt(l7741, l7758);
    at = 10668; if (rt.truthy(l7759)) {
      at = 10669; if (rt.truthy(l7734)) {
        at = 10670; l7762 = field(rt, l7737, "color");
        at = 10671; l7761 = l7762;
      } else {
        at = 10672; l7763 = K[2949];
        at = 10673; l7761 = l7763;
      }
      at = 10674; l7764 = K[2950];
      at = 10675; l7765 = rt.sequence_get(l7764, l7761);
      at = 10676; l7760 = l7765;
    } else {
      at = 10677; l7766 = K[2951];
      at = 10678; l7760 = l7766;
    }
    at = 10679; markSet(rt, false, "circle", {at: l7744, radius: l7757, fill: l7760});
    at = 10680; if (rt.truthy(l7734)) {
      at = 10681; l7768 = field(rt, l7737, "points");
      at = 10682; l7767 = l7768;
    } else {
      at = 10683; l7769 = K[2952];
      at = 10684; l7767 = l7769;
    }
    at = 10685; l7770 = K[2953];
    at = 10686; l7771 = rt.sub(l7741, l7770);
    at = 10687; l7772 = K[2954];
    at = 10688; l7773 = rt.max(l7772, l7771);
    at = 10689; l7774 = K[2955];
    at = 10690; l7775 = rt.array_slice(l7767, l7774, l7773);
    at = 10691; if (rt.truthy(l7734)) {
      at = 10692; l7777 = field(rt, l7737, "points");
      at = 10693; l7776 = l7777;
    } else {
      at = 10694; l7778 = K[2956];
      at = 10695; l7776 = l7778;
    }
    at = 10696; l7779 = K[2957];
    at = 10697; l7780 = rt.gt(l7741, l7779);
    at = 10698; if (rt.truthy(l7780)) {
      at = 10699; l7782 = K[2958];
      at = 10700; l7781 = l7782;
    } else {
      at = 10701; l7783 = K[2959];
      at = 10702; l7781 = l7783;
    }
    at = 10703; l7784 = K[2960];
    at = 10704; l7785 = rt.sub(l7741, l7784);
    at = 10705; l7786 = K[2961];
    at = 10706; l7787 = rt.max(l7786, l7785);
    at = 10707; l7788 = rt.array_slice(l7776, l7781, l7787);
    at = 10708; if (rt.truthy(l7734)) {
      at = 10709; l7790 = field(rt, l7737, "color");
      at = 10710; l7789 = l7790;
    } else {
      at = 10711; l7791 = K[2962];
      at = 10712; l7789 = l7791;
    }
    at = 10713; l7792 = K[2963];
    at = 10714; l7793 = rt.sequence_get(l7792, l7789);
    at = 10715; if (rt.truthy(l7734)) {
      at = 10716; l7795 = field(rt, l7737, "width");
      at = 10717; l7794 = l7795;
    } else {
      at = 10718; l7796 = K[2964];
      at = 10719; l7794 = l7796;
    }
    at = 10720; markSet(rt, true, "line", {from: l7775, to: l7788, color: l7793, width: l7794});
    at = 10721; l7797 = field(rt, l15, "strokes");
    at = 10722; l7798 = rt.sequence_length(l7797);
    at = 10723; l7799 = K[2965];
    at = 10724; l7800 = rt.lt(l7799, l7798);
    at = 10725; if (rt.truthy(l7800)) {
      at = 10726; l7801 = field(rt, l15, "strokes");
      at = 10727; l7802 = K[2966];
      at = 10728; l7803 = rt.sequence_get(l7801, l7802);
    }
    at = 10729; if (rt.truthy(l7800)) {
      at = 10730; l7805 = field(rt, l7803, "points");
      at = 10731; l7804 = l7805;
    } else {
      at = 10732; l7806 = K[2967];
      at = 10733; l7804 = l7806;
    }
    at = 10734; l7807 = rt.array_count(l7804);
    at = 10735; l7808 = K[2968];
    at = 10736; l7809 = rt.gt(l7807, l7808);
    at = 10737; if (rt.truthy(l7809)) {
      at = 10738; if (rt.truthy(l7800)) {
        at = 10739; l7812 = field(rt, l7803, "points");
        at = 10740; l7811 = l7812;
      } else {
        at = 10741; l7813 = K[2969];
        at = 10742; l7811 = l7813;
      }
      at = 10743; l7814 = K[2970];
      at = 10744; l7815 = rt.array_nth(l7811, l7814);
      at = 10745; l7810 = l7815;
    } else {
      at = 10746; l7816 = K[2971];
      at = 10747; l7810 = l7816;
    }
    at = 10748; if (rt.truthy(l7800)) {
      at = 10749; l7818 = field(rt, l7803, "width");
      at = 10750; l7817 = l7818;
    } else {
      at = 10751; l7819 = K[2972];
      at = 10752; l7817 = l7819;
    }
    at = 10753; l7820 = K[2973];
    at = 10754; l7821 = rt.int_div(l7817, l7820);
    at = 10755; l7822 = K[2974];
    at = 10756; l7823 = rt.max(l7822, l7821);
    at = 10757; l7824 = K[2975];
    at = 10758; l7825 = rt.gt(l7807, l7824);
    at = 10759; if (rt.truthy(l7825)) {
      at = 10760; if (rt.truthy(l7800)) {
        at = 10761; l7828 = field(rt, l7803, "color");
        at = 10762; l7827 = l7828;
      } else {
        at = 10763; l7829 = K[2976];
        at = 10764; l7827 = l7829;
      }
      at = 10765; l7830 = K[2977];
      at = 10766; l7831 = rt.sequence_get(l7830, l7827);
      at = 10767; l7826 = l7831;
    } else {
      at = 10768; l7832 = K[2978];
      at = 10769; l7826 = l7832;
    }
    at = 10770; markSet(rt, false, "circle", {at: l7810, radius: l7823, fill: l7826});
    at = 10771; if (rt.truthy(l7800)) {
      at = 10772; l7834 = field(rt, l7803, "points");
      at = 10773; l7833 = l7834;
    } else {
      at = 10774; l7835 = K[2979];
      at = 10775; l7833 = l7835;
    }
    at = 10776; l7836 = K[2980];
    at = 10777; l7837 = rt.sub(l7807, l7836);
    at = 10778; l7838 = K[2981];
    at = 10779; l7839 = rt.max(l7838, l7837);
    at = 10780; l7840 = K[2982];
    at = 10781; l7841 = rt.array_slice(l7833, l7840, l7839);
    at = 10782; if (rt.truthy(l7800)) {
      at = 10783; l7843 = field(rt, l7803, "points");
      at = 10784; l7842 = l7843;
    } else {
      at = 10785; l7844 = K[2983];
      at = 10786; l7842 = l7844;
    }
    at = 10787; l7845 = K[2984];
    at = 10788; l7846 = rt.gt(l7807, l7845);
    at = 10789; if (rt.truthy(l7846)) {
      at = 10790; l7848 = K[2985];
      at = 10791; l7847 = l7848;
    } else {
      at = 10792; l7849 = K[2986];
      at = 10793; l7847 = l7849;
    }
    at = 10794; l7850 = K[2987];
    at = 10795; l7851 = rt.sub(l7807, l7850);
    at = 10796; l7852 = K[2988];
    at = 10797; l7853 = rt.max(l7852, l7851);
    at = 10798; l7854 = rt.array_slice(l7842, l7847, l7853);
    at = 10799; if (rt.truthy(l7800)) {
      at = 10800; l7856 = field(rt, l7803, "color");
      at = 10801; l7855 = l7856;
    } else {
      at = 10802; l7857 = K[2989];
      at = 10803; l7855 = l7857;
    }
    at = 10804; l7858 = K[2990];
    at = 10805; l7859 = rt.sequence_get(l7858, l7855);
    at = 10806; if (rt.truthy(l7800)) {
      at = 10807; l7861 = field(rt, l7803, "width");
      at = 10808; l7860 = l7861;
    } else {
      at = 10809; l7862 = K[2991];
      at = 10810; l7860 = l7862;
    }
    at = 10811; markSet(rt, true, "line", {from: l7841, to: l7854, color: l7859, width: l7860});
    at = 10812; l7863 = field(rt, l15, "strokes");
    at = 10813; l7864 = rt.sequence_length(l7863);
    at = 10814; l7865 = K[2992];
    at = 10815; l7866 = rt.lt(l7865, l7864);
    at = 10816; if (rt.truthy(l7866)) {
      at = 10817; l7867 = field(rt, l15, "strokes");
      at = 10818; l7868 = K[2993];
      at = 10819; l7869 = rt.sequence_get(l7867, l7868);
    }
    at = 10820; if (rt.truthy(l7866)) {
      at = 10821; l7871 = field(rt, l7869, "points");
      at = 10822; l7870 = l7871;
    } else {
      at = 10823; l7872 = K[2994];
      at = 10824; l7870 = l7872;
    }
    at = 10825; l7873 = rt.array_count(l7870);
    at = 10826; l7874 = K[2995];
    at = 10827; l7875 = rt.gt(l7873, l7874);
    at = 10828; if (rt.truthy(l7875)) {
      at = 10829; if (rt.truthy(l7866)) {
        at = 10830; l7878 = field(rt, l7869, "points");
        at = 10831; l7877 = l7878;
      } else {
        at = 10832; l7879 = K[2996];
        at = 10833; l7877 = l7879;
      }
      at = 10834; l7880 = K[2997];
      at = 10835; l7881 = rt.array_nth(l7877, l7880);
      at = 10836; l7876 = l7881;
    } else {
      at = 10837; l7882 = K[2998];
      at = 10838; l7876 = l7882;
    }
    at = 10839; if (rt.truthy(l7866)) {
      at = 10840; l7884 = field(rt, l7869, "width");
      at = 10841; l7883 = l7884;
    } else {
      at = 10842; l7885 = K[2999];
      at = 10843; l7883 = l7885;
    }
    at = 10844; l7886 = K[3000];
    at = 10845; l7887 = rt.int_div(l7883, l7886);
    at = 10846; l7888 = K[3001];
    at = 10847; l7889 = rt.max(l7888, l7887);
    at = 10848; l7890 = K[3002];
    at = 10849; l7891 = rt.gt(l7873, l7890);
    at = 10850; if (rt.truthy(l7891)) {
      at = 10851; if (rt.truthy(l7866)) {
        at = 10852; l7894 = field(rt, l7869, "color");
        at = 10853; l7893 = l7894;
      } else {
        at = 10854; l7895 = K[3003];
        at = 10855; l7893 = l7895;
      }
      at = 10856; l7896 = K[3004];
      at = 10857; l7897 = rt.sequence_get(l7896, l7893);
      at = 10858; l7892 = l7897;
    } else {
      at = 10859; l7898 = K[3005];
      at = 10860; l7892 = l7898;
    }
    at = 10861; markSet(rt, false, "circle", {at: l7876, radius: l7889, fill: l7892});
    at = 10862; if (rt.truthy(l7866)) {
      at = 10863; l7900 = field(rt, l7869, "points");
      at = 10864; l7899 = l7900;
    } else {
      at = 10865; l7901 = K[3006];
      at = 10866; l7899 = l7901;
    }
    at = 10867; l7902 = K[3007];
    at = 10868; l7903 = rt.sub(l7873, l7902);
    at = 10869; l7904 = K[3008];
    at = 10870; l7905 = rt.max(l7904, l7903);
    at = 10871; l7906 = K[3009];
    at = 10872; l7907 = rt.array_slice(l7899, l7906, l7905);
    at = 10873; if (rt.truthy(l7866)) {
      at = 10874; l7909 = field(rt, l7869, "points");
      at = 10875; l7908 = l7909;
    } else {
      at = 10876; l7910 = K[3010];
      at = 10877; l7908 = l7910;
    }
    at = 10878; l7911 = K[3011];
    at = 10879; l7912 = rt.gt(l7873, l7911);
    at = 10880; if (rt.truthy(l7912)) {
      at = 10881; l7914 = K[3012];
      at = 10882; l7913 = l7914;
    } else {
      at = 10883; l7915 = K[3013];
      at = 10884; l7913 = l7915;
    }
    at = 10885; l7916 = K[3014];
    at = 10886; l7917 = rt.sub(l7873, l7916);
    at = 10887; l7918 = K[3015];
    at = 10888; l7919 = rt.max(l7918, l7917);
    at = 10889; l7920 = rt.array_slice(l7908, l7913, l7919);
    at = 10890; if (rt.truthy(l7866)) {
      at = 10891; l7922 = field(rt, l7869, "color");
      at = 10892; l7921 = l7922;
    } else {
      at = 10893; l7923 = K[3016];
      at = 10894; l7921 = l7923;
    }
    at = 10895; l7924 = K[3017];
    at = 10896; l7925 = rt.sequence_get(l7924, l7921);
    at = 10897; if (rt.truthy(l7866)) {
      at = 10898; l7927 = field(rt, l7869, "width");
      at = 10899; l7926 = l7927;
    } else {
      at = 10900; l7928 = K[3018];
      at = 10901; l7926 = l7928;
    }
    at = 10902; markSet(rt, true, "line", {from: l7907, to: l7920, color: l7925, width: l7926});
    at = 10903; l7929 = field(rt, l15, "strokes");
    at = 10904; l7930 = rt.sequence_length(l7929);
    at = 10905; l7931 = K[3019];
    at = 10906; l7932 = rt.lt(l7931, l7930);
    at = 10907; if (rt.truthy(l7932)) {
      at = 10908; l7933 = field(rt, l15, "strokes");
      at = 10909; l7934 = K[3020];
      at = 10910; l7935 = rt.sequence_get(l7933, l7934);
    }
    at = 10911; if (rt.truthy(l7932)) {
      at = 10912; l7937 = field(rt, l7935, "points");
      at = 10913; l7936 = l7937;
    } else {
      at = 10914; l7938 = K[3021];
      at = 10915; l7936 = l7938;
    }
    at = 10916; l7939 = rt.array_count(l7936);
    at = 10917; l7940 = K[3022];
    at = 10918; l7941 = rt.gt(l7939, l7940);
    at = 10919; if (rt.truthy(l7941)) {
      at = 10920; if (rt.truthy(l7932)) {
        at = 10921; l7944 = field(rt, l7935, "points");
        at = 10922; l7943 = l7944;
      } else {
        at = 10923; l7945 = K[3023];
        at = 10924; l7943 = l7945;
      }
      at = 10925; l7946 = K[3024];
      at = 10926; l7947 = rt.array_nth(l7943, l7946);
      at = 10927; l7942 = l7947;
    } else {
      at = 10928; l7948 = K[3025];
      at = 10929; l7942 = l7948;
    }
    at = 10930; if (rt.truthy(l7932)) {
      at = 10931; l7950 = field(rt, l7935, "width");
      at = 10932; l7949 = l7950;
    } else {
      at = 10933; l7951 = K[3026];
      at = 10934; l7949 = l7951;
    }
    at = 10935; l7952 = K[3027];
    at = 10936; l7953 = rt.int_div(l7949, l7952);
    at = 10937; l7954 = K[3028];
    at = 10938; l7955 = rt.max(l7954, l7953);
    at = 10939; l7956 = K[3029];
    at = 10940; l7957 = rt.gt(l7939, l7956);
    at = 10941; if (rt.truthy(l7957)) {
      at = 10942; if (rt.truthy(l7932)) {
        at = 10943; l7960 = field(rt, l7935, "color");
        at = 10944; l7959 = l7960;
      } else {
        at = 10945; l7961 = K[3030];
        at = 10946; l7959 = l7961;
      }
      at = 10947; l7962 = K[3031];
      at = 10948; l7963 = rt.sequence_get(l7962, l7959);
      at = 10949; l7958 = l7963;
    } else {
      at = 10950; l7964 = K[3032];
      at = 10951; l7958 = l7964;
    }
    at = 10952; markSet(rt, false, "circle", {at: l7942, radius: l7955, fill: l7958});
    at = 10953; if (rt.truthy(l7932)) {
      at = 10954; l7966 = field(rt, l7935, "points");
      at = 10955; l7965 = l7966;
    } else {
      at = 10956; l7967 = K[3033];
      at = 10957; l7965 = l7967;
    }
    at = 10958; l7968 = K[3034];
    at = 10959; l7969 = rt.sub(l7939, l7968);
    at = 10960; l7970 = K[3035];
    at = 10961; l7971 = rt.max(l7970, l7969);
    at = 10962; l7972 = K[3036];
    at = 10963; l7973 = rt.array_slice(l7965, l7972, l7971);
    at = 10964; if (rt.truthy(l7932)) {
      at = 10965; l7975 = field(rt, l7935, "points");
      at = 10966; l7974 = l7975;
    } else {
      at = 10967; l7976 = K[3037];
      at = 10968; l7974 = l7976;
    }
    at = 10969; l7977 = K[3038];
    at = 10970; l7978 = rt.gt(l7939, l7977);
    at = 10971; if (rt.truthy(l7978)) {
      at = 10972; l7980 = K[3039];
      at = 10973; l7979 = l7980;
    } else {
      at = 10974; l7981 = K[3040];
      at = 10975; l7979 = l7981;
    }
    at = 10976; l7982 = K[3041];
    at = 10977; l7983 = rt.sub(l7939, l7982);
    at = 10978; l7984 = K[3042];
    at = 10979; l7985 = rt.max(l7984, l7983);
    at = 10980; l7986 = rt.array_slice(l7974, l7979, l7985);
    at = 10981; if (rt.truthy(l7932)) {
      at = 10982; l7988 = field(rt, l7935, "color");
      at = 10983; l7987 = l7988;
    } else {
      at = 10984; l7989 = K[3043];
      at = 10985; l7987 = l7989;
    }
    at = 10986; l7990 = K[3044];
    at = 10987; l7991 = rt.sequence_get(l7990, l7987);
    at = 10988; if (rt.truthy(l7932)) {
      at = 10989; l7993 = field(rt, l7935, "width");
      at = 10990; l7992 = l7993;
    } else {
      at = 10991; l7994 = K[3045];
      at = 10992; l7992 = l7994;
    }
    at = 10993; markSet(rt, true, "line", {from: l7973, to: l7986, color: l7991, width: l7992});
    at = 10994; l7995 = field(rt, l15, "strokes");
    at = 10995; l7996 = rt.sequence_length(l7995);
    at = 10996; l7997 = K[3046];
    at = 10997; l7998 = rt.lt(l7997, l7996);
    at = 10998; if (rt.truthy(l7998)) {
      at = 10999; l7999 = field(rt, l15, "strokes");
      at = 11000; l8000 = K[3047];
      at = 11001; l8001 = rt.sequence_get(l7999, l8000);
    }
    at = 11002; if (rt.truthy(l7998)) {
      at = 11003; l8003 = field(rt, l8001, "points");
      at = 11004; l8002 = l8003;
    } else {
      at = 11005; l8004 = K[3048];
      at = 11006; l8002 = l8004;
    }
    at = 11007; l8005 = rt.array_count(l8002);
    at = 11008; l8006 = K[3049];
    at = 11009; l8007 = rt.gt(l8005, l8006);
    at = 11010; if (rt.truthy(l8007)) {
      at = 11011; if (rt.truthy(l7998)) {
        at = 11012; l8010 = field(rt, l8001, "points");
        at = 11013; l8009 = l8010;
      } else {
        at = 11014; l8011 = K[3050];
        at = 11015; l8009 = l8011;
      }
      at = 11016; l8012 = K[3051];
      at = 11017; l8013 = rt.array_nth(l8009, l8012);
      at = 11018; l8008 = l8013;
    } else {
      at = 11019; l8014 = K[3052];
      at = 11020; l8008 = l8014;
    }
    at = 11021; if (rt.truthy(l7998)) {
      at = 11022; l8016 = field(rt, l8001, "width");
      at = 11023; l8015 = l8016;
    } else {
      at = 11024; l8017 = K[3053];
      at = 11025; l8015 = l8017;
    }
    at = 11026; l8018 = K[3054];
    at = 11027; l8019 = rt.int_div(l8015, l8018);
    at = 11028; l8020 = K[3055];
    at = 11029; l8021 = rt.max(l8020, l8019);
    at = 11030; l8022 = K[3056];
    at = 11031; l8023 = rt.gt(l8005, l8022);
    at = 11032; if (rt.truthy(l8023)) {
      at = 11033; if (rt.truthy(l7998)) {
        at = 11034; l8026 = field(rt, l8001, "color");
        at = 11035; l8025 = l8026;
      } else {
        at = 11036; l8027 = K[3057];
        at = 11037; l8025 = l8027;
      }
      at = 11038; l8028 = K[3058];
      at = 11039; l8029 = rt.sequence_get(l8028, l8025);
      at = 11040; l8024 = l8029;
    } else {
      at = 11041; l8030 = K[3059];
      at = 11042; l8024 = l8030;
    }
    at = 11043; markSet(rt, false, "circle", {at: l8008, radius: l8021, fill: l8024});
    at = 11044; if (rt.truthy(l7998)) {
      at = 11045; l8032 = field(rt, l8001, "points");
      at = 11046; l8031 = l8032;
    } else {
      at = 11047; l8033 = K[3060];
      at = 11048; l8031 = l8033;
    }
    at = 11049; l8034 = K[3061];
    at = 11050; l8035 = rt.sub(l8005, l8034);
    at = 11051; l8036 = K[3062];
    at = 11052; l8037 = rt.max(l8036, l8035);
    at = 11053; l8038 = K[3063];
    at = 11054; l8039 = rt.array_slice(l8031, l8038, l8037);
    at = 11055; if (rt.truthy(l7998)) {
      at = 11056; l8041 = field(rt, l8001, "points");
      at = 11057; l8040 = l8041;
    } else {
      at = 11058; l8042 = K[3064];
      at = 11059; l8040 = l8042;
    }
    at = 11060; l8043 = K[3065];
    at = 11061; l8044 = rt.gt(l8005, l8043);
    at = 11062; if (rt.truthy(l8044)) {
      at = 11063; l8046 = K[3066];
      at = 11064; l8045 = l8046;
    } else {
      at = 11065; l8047 = K[3067];
      at = 11066; l8045 = l8047;
    }
    at = 11067; l8048 = K[3068];
    at = 11068; l8049 = rt.sub(l8005, l8048);
    at = 11069; l8050 = K[3069];
    at = 11070; l8051 = rt.max(l8050, l8049);
    at = 11071; l8052 = rt.array_slice(l8040, l8045, l8051);
    at = 11072; if (rt.truthy(l7998)) {
      at = 11073; l8054 = field(rt, l8001, "color");
      at = 11074; l8053 = l8054;
    } else {
      at = 11075; l8055 = K[3070];
      at = 11076; l8053 = l8055;
    }
    at = 11077; l8056 = K[3071];
    at = 11078; l8057 = rt.sequence_get(l8056, l8053);
    at = 11079; if (rt.truthy(l7998)) {
      at = 11080; l8059 = field(rt, l8001, "width");
      at = 11081; l8058 = l8059;
    } else {
      at = 11082; l8060 = K[3072];
      at = 11083; l8058 = l8060;
    }
    at = 11084; markSet(rt, true, "line", {from: l8039, to: l8052, color: l8057, width: l8058});
    at = 11085; l8061 = field(rt, l15, "strokes");
    at = 11086; l8062 = rt.sequence_length(l8061);
    at = 11087; l8063 = K[3073];
    at = 11088; l8064 = rt.lt(l8063, l8062);
    at = 11089; if (rt.truthy(l8064)) {
      at = 11090; l8065 = field(rt, l15, "strokes");
      at = 11091; l8066 = K[3074];
      at = 11092; l8067 = rt.sequence_get(l8065, l8066);
    }
    at = 11093; if (rt.truthy(l8064)) {
      at = 11094; l8069 = field(rt, l8067, "points");
      at = 11095; l8068 = l8069;
    } else {
      at = 11096; l8070 = K[3075];
      at = 11097; l8068 = l8070;
    }
    at = 11098; l8071 = rt.array_count(l8068);
    at = 11099; l8072 = K[3076];
    at = 11100; l8073 = rt.gt(l8071, l8072);
    at = 11101; if (rt.truthy(l8073)) {
      at = 11102; if (rt.truthy(l8064)) {
        at = 11103; l8076 = field(rt, l8067, "points");
        at = 11104; l8075 = l8076;
      } else {
        at = 11105; l8077 = K[3077];
        at = 11106; l8075 = l8077;
      }
      at = 11107; l8078 = K[3078];
      at = 11108; l8079 = rt.array_nth(l8075, l8078);
      at = 11109; l8074 = l8079;
    } else {
      at = 11110; l8080 = K[3079];
      at = 11111; l8074 = l8080;
    }
    at = 11112; if (rt.truthy(l8064)) {
      at = 11113; l8082 = field(rt, l8067, "width");
      at = 11114; l8081 = l8082;
    } else {
      at = 11115; l8083 = K[3080];
      at = 11116; l8081 = l8083;
    }
    at = 11117; l8084 = K[3081];
    at = 11118; l8085 = rt.int_div(l8081, l8084);
    at = 11119; l8086 = K[3082];
    at = 11120; l8087 = rt.max(l8086, l8085);
    at = 11121; l8088 = K[3083];
    at = 11122; l8089 = rt.gt(l8071, l8088);
    at = 11123; if (rt.truthy(l8089)) {
      at = 11124; if (rt.truthy(l8064)) {
        at = 11125; l8092 = field(rt, l8067, "color");
        at = 11126; l8091 = l8092;
      } else {
        at = 11127; l8093 = K[3084];
        at = 11128; l8091 = l8093;
      }
      at = 11129; l8094 = K[3085];
      at = 11130; l8095 = rt.sequence_get(l8094, l8091);
      at = 11131; l8090 = l8095;
    } else {
      at = 11132; l8096 = K[3086];
      at = 11133; l8090 = l8096;
    }
    at = 11134; markSet(rt, false, "circle", {at: l8074, radius: l8087, fill: l8090});
    at = 11135; if (rt.truthy(l8064)) {
      at = 11136; l8098 = field(rt, l8067, "points");
      at = 11137; l8097 = l8098;
    } else {
      at = 11138; l8099 = K[3087];
      at = 11139; l8097 = l8099;
    }
    at = 11140; l8100 = K[3088];
    at = 11141; l8101 = rt.sub(l8071, l8100);
    at = 11142; l8102 = K[3089];
    at = 11143; l8103 = rt.max(l8102, l8101);
    at = 11144; l8104 = K[3090];
    at = 11145; l8105 = rt.array_slice(l8097, l8104, l8103);
    at = 11146; if (rt.truthy(l8064)) {
      at = 11147; l8107 = field(rt, l8067, "points");
      at = 11148; l8106 = l8107;
    } else {
      at = 11149; l8108 = K[3091];
      at = 11150; l8106 = l8108;
    }
    at = 11151; l8109 = K[3092];
    at = 11152; l8110 = rt.gt(l8071, l8109);
    at = 11153; if (rt.truthy(l8110)) {
      at = 11154; l8112 = K[3093];
      at = 11155; l8111 = l8112;
    } else {
      at = 11156; l8113 = K[3094];
      at = 11157; l8111 = l8113;
    }
    at = 11158; l8114 = K[3095];
    at = 11159; l8115 = rt.sub(l8071, l8114);
    at = 11160; l8116 = K[3096];
    at = 11161; l8117 = rt.max(l8116, l8115);
    at = 11162; l8118 = rt.array_slice(l8106, l8111, l8117);
    at = 11163; if (rt.truthy(l8064)) {
      at = 11164; l8120 = field(rt, l8067, "color");
      at = 11165; l8119 = l8120;
    } else {
      at = 11166; l8121 = K[3097];
      at = 11167; l8119 = l8121;
    }
    at = 11168; l8122 = K[3098];
    at = 11169; l8123 = rt.sequence_get(l8122, l8119);
    at = 11170; if (rt.truthy(l8064)) {
      at = 11171; l8125 = field(rt, l8067, "width");
      at = 11172; l8124 = l8125;
    } else {
      at = 11173; l8126 = K[3099];
      at = 11174; l8124 = l8126;
    }
    at = 11175; markSet(rt, true, "line", {from: l8105, to: l8118, color: l8123, width: l8124});
    at = 11176; l8127 = field(rt, l15, "strokes");
    at = 11177; l8128 = rt.sequence_length(l8127);
    at = 11178; l8129 = K[3100];
    at = 11179; l8130 = rt.lt(l8129, l8128);
    at = 11180; if (rt.truthy(l8130)) {
      at = 11181; l8131 = field(rt, l15, "strokes");
      at = 11182; l8132 = K[3101];
      at = 11183; l8133 = rt.sequence_get(l8131, l8132);
    }
    at = 11184; if (rt.truthy(l8130)) {
      at = 11185; l8135 = field(rt, l8133, "points");
      at = 11186; l8134 = l8135;
    } else {
      at = 11187; l8136 = K[3102];
      at = 11188; l8134 = l8136;
    }
    at = 11189; l8137 = rt.array_count(l8134);
    at = 11190; l8138 = K[3103];
    at = 11191; l8139 = rt.gt(l8137, l8138);
    at = 11192; if (rt.truthy(l8139)) {
      at = 11193; if (rt.truthy(l8130)) {
        at = 11194; l8142 = field(rt, l8133, "points");
        at = 11195; l8141 = l8142;
      } else {
        at = 11196; l8143 = K[3104];
        at = 11197; l8141 = l8143;
      }
      at = 11198; l8144 = K[3105];
      at = 11199; l8145 = rt.array_nth(l8141, l8144);
      at = 11200; l8140 = l8145;
    } else {
      at = 11201; l8146 = K[3106];
      at = 11202; l8140 = l8146;
    }
    at = 11203; if (rt.truthy(l8130)) {
      at = 11204; l8148 = field(rt, l8133, "width");
      at = 11205; l8147 = l8148;
    } else {
      at = 11206; l8149 = K[3107];
      at = 11207; l8147 = l8149;
    }
    at = 11208; l8150 = K[3108];
    at = 11209; l8151 = rt.int_div(l8147, l8150);
    at = 11210; l8152 = K[3109];
    at = 11211; l8153 = rt.max(l8152, l8151);
    at = 11212; l8154 = K[3110];
    at = 11213; l8155 = rt.gt(l8137, l8154);
    at = 11214; if (rt.truthy(l8155)) {
      at = 11215; if (rt.truthy(l8130)) {
        at = 11216; l8158 = field(rt, l8133, "color");
        at = 11217; l8157 = l8158;
      } else {
        at = 11218; l8159 = K[3111];
        at = 11219; l8157 = l8159;
      }
      at = 11220; l8160 = K[3112];
      at = 11221; l8161 = rt.sequence_get(l8160, l8157);
      at = 11222; l8156 = l8161;
    } else {
      at = 11223; l8162 = K[3113];
      at = 11224; l8156 = l8162;
    }
    at = 11225; markSet(rt, false, "circle", {at: l8140, radius: l8153, fill: l8156});
    at = 11226; if (rt.truthy(l8130)) {
      at = 11227; l8164 = field(rt, l8133, "points");
      at = 11228; l8163 = l8164;
    } else {
      at = 11229; l8165 = K[3114];
      at = 11230; l8163 = l8165;
    }
    at = 11231; l8166 = K[3115];
    at = 11232; l8167 = rt.sub(l8137, l8166);
    at = 11233; l8168 = K[3116];
    at = 11234; l8169 = rt.max(l8168, l8167);
    at = 11235; l8170 = K[3117];
    at = 11236; l8171 = rt.array_slice(l8163, l8170, l8169);
    at = 11237; if (rt.truthy(l8130)) {
      at = 11238; l8173 = field(rt, l8133, "points");
      at = 11239; l8172 = l8173;
    } else {
      at = 11240; l8174 = K[3118];
      at = 11241; l8172 = l8174;
    }
    at = 11242; l8175 = K[3119];
    at = 11243; l8176 = rt.gt(l8137, l8175);
    at = 11244; if (rt.truthy(l8176)) {
      at = 11245; l8178 = K[3120];
      at = 11246; l8177 = l8178;
    } else {
      at = 11247; l8179 = K[3121];
      at = 11248; l8177 = l8179;
    }
    at = 11249; l8180 = K[3122];
    at = 11250; l8181 = rt.sub(l8137, l8180);
    at = 11251; l8182 = K[3123];
    at = 11252; l8183 = rt.max(l8182, l8181);
    at = 11253; l8184 = rt.array_slice(l8172, l8177, l8183);
    at = 11254; if (rt.truthy(l8130)) {
      at = 11255; l8186 = field(rt, l8133, "color");
      at = 11256; l8185 = l8186;
    } else {
      at = 11257; l8187 = K[3124];
      at = 11258; l8185 = l8187;
    }
    at = 11259; l8188 = K[3125];
    at = 11260; l8189 = rt.sequence_get(l8188, l8185);
    at = 11261; if (rt.truthy(l8130)) {
      at = 11262; l8191 = field(rt, l8133, "width");
      at = 11263; l8190 = l8191;
    } else {
      at = 11264; l8192 = K[3126];
      at = 11265; l8190 = l8192;
    }
    at = 11266; markSet(rt, true, "line", {from: l8171, to: l8184, color: l8189, width: l8190});
    at = 11267; l8193 = field(rt, l15, "strokes");
    at = 11268; l8194 = rt.sequence_length(l8193);
    at = 11269; l8195 = K[3127];
    at = 11270; l8196 = rt.lt(l8195, l8194);
    at = 11271; if (rt.truthy(l8196)) {
      at = 11272; l8197 = field(rt, l15, "strokes");
      at = 11273; l8198 = K[3128];
      at = 11274; l8199 = rt.sequence_get(l8197, l8198);
    }
    at = 11275; if (rt.truthy(l8196)) {
      at = 11276; l8201 = field(rt, l8199, "points");
      at = 11277; l8200 = l8201;
    } else {
      at = 11278; l8202 = K[3129];
      at = 11279; l8200 = l8202;
    }
    at = 11280; l8203 = rt.array_count(l8200);
    at = 11281; l8204 = K[3130];
    at = 11282; l8205 = rt.gt(l8203, l8204);
    at = 11283; if (rt.truthy(l8205)) {
      at = 11284; if (rt.truthy(l8196)) {
        at = 11285; l8208 = field(rt, l8199, "points");
        at = 11286; l8207 = l8208;
      } else {
        at = 11287; l8209 = K[3131];
        at = 11288; l8207 = l8209;
      }
      at = 11289; l8210 = K[3132];
      at = 11290; l8211 = rt.array_nth(l8207, l8210);
      at = 11291; l8206 = l8211;
    } else {
      at = 11292; l8212 = K[3133];
      at = 11293; l8206 = l8212;
    }
    at = 11294; if (rt.truthy(l8196)) {
      at = 11295; l8214 = field(rt, l8199, "width");
      at = 11296; l8213 = l8214;
    } else {
      at = 11297; l8215 = K[3134];
      at = 11298; l8213 = l8215;
    }
    at = 11299; l8216 = K[3135];
    at = 11300; l8217 = rt.int_div(l8213, l8216);
    at = 11301; l8218 = K[3136];
    at = 11302; l8219 = rt.max(l8218, l8217);
    at = 11303; l8220 = K[3137];
    at = 11304; l8221 = rt.gt(l8203, l8220);
    at = 11305; if (rt.truthy(l8221)) {
      at = 11306; if (rt.truthy(l8196)) {
        at = 11307; l8224 = field(rt, l8199, "color");
        at = 11308; l8223 = l8224;
      } else {
        at = 11309; l8225 = K[3138];
        at = 11310; l8223 = l8225;
      }
      at = 11311; l8226 = K[3139];
      at = 11312; l8227 = rt.sequence_get(l8226, l8223);
      at = 11313; l8222 = l8227;
    } else {
      at = 11314; l8228 = K[3140];
      at = 11315; l8222 = l8228;
    }
    at = 11316; markSet(rt, false, "circle", {at: l8206, radius: l8219, fill: l8222});
    at = 11317; if (rt.truthy(l8196)) {
      at = 11318; l8230 = field(rt, l8199, "points");
      at = 11319; l8229 = l8230;
    } else {
      at = 11320; l8231 = K[3141];
      at = 11321; l8229 = l8231;
    }
    at = 11322; l8232 = K[3142];
    at = 11323; l8233 = rt.sub(l8203, l8232);
    at = 11324; l8234 = K[3143];
    at = 11325; l8235 = rt.max(l8234, l8233);
    at = 11326; l8236 = K[3144];
    at = 11327; l8237 = rt.array_slice(l8229, l8236, l8235);
    at = 11328; if (rt.truthy(l8196)) {
      at = 11329; l8239 = field(rt, l8199, "points");
      at = 11330; l8238 = l8239;
    } else {
      at = 11331; l8240 = K[3145];
      at = 11332; l8238 = l8240;
    }
    at = 11333; l8241 = K[3146];
    at = 11334; l8242 = rt.gt(l8203, l8241);
    at = 11335; if (rt.truthy(l8242)) {
      at = 11336; l8244 = K[3147];
      at = 11337; l8243 = l8244;
    } else {
      at = 11338; l8245 = K[3148];
      at = 11339; l8243 = l8245;
    }
    at = 11340; l8246 = K[3149];
    at = 11341; l8247 = rt.sub(l8203, l8246);
    at = 11342; l8248 = K[3150];
    at = 11343; l8249 = rt.max(l8248, l8247);
    at = 11344; l8250 = rt.array_slice(l8238, l8243, l8249);
    at = 11345; if (rt.truthy(l8196)) {
      at = 11346; l8252 = field(rt, l8199, "color");
      at = 11347; l8251 = l8252;
    } else {
      at = 11348; l8253 = K[3151];
      at = 11349; l8251 = l8253;
    }
    at = 11350; l8254 = K[3152];
    at = 11351; l8255 = rt.sequence_get(l8254, l8251);
    at = 11352; if (rt.truthy(l8196)) {
      at = 11353; l8257 = field(rt, l8199, "width");
      at = 11354; l8256 = l8257;
    } else {
      at = 11355; l8258 = K[3153];
      at = 11356; l8256 = l8258;
    }
    at = 11357; markSet(rt, true, "line", {from: l8237, to: l8250, color: l8255, width: l8256});
    at = 11358; l8259 = field(rt, l15, "strokes");
    at = 11359; l8260 = rt.sequence_length(l8259);
    at = 11360; l8261 = K[3154];
    at = 11361; l8262 = rt.lt(l8261, l8260);
    at = 11362; if (rt.truthy(l8262)) {
      at = 11363; l8263 = field(rt, l15, "strokes");
      at = 11364; l8264 = K[3155];
      at = 11365; l8265 = rt.sequence_get(l8263, l8264);
    }
    at = 11366; if (rt.truthy(l8262)) {
      at = 11367; l8267 = field(rt, l8265, "points");
      at = 11368; l8266 = l8267;
    } else {
      at = 11369; l8268 = K[3156];
      at = 11370; l8266 = l8268;
    }
    at = 11371; l8269 = rt.array_count(l8266);
    at = 11372; l8270 = K[3157];
    at = 11373; l8271 = rt.gt(l8269, l8270);
    at = 11374; if (rt.truthy(l8271)) {
      at = 11375; if (rt.truthy(l8262)) {
        at = 11376; l8274 = field(rt, l8265, "points");
        at = 11377; l8273 = l8274;
      } else {
        at = 11378; l8275 = K[3158];
        at = 11379; l8273 = l8275;
      }
      at = 11380; l8276 = K[3159];
      at = 11381; l8277 = rt.array_nth(l8273, l8276);
      at = 11382; l8272 = l8277;
    } else {
      at = 11383; l8278 = K[3160];
      at = 11384; l8272 = l8278;
    }
    at = 11385; if (rt.truthy(l8262)) {
      at = 11386; l8280 = field(rt, l8265, "width");
      at = 11387; l8279 = l8280;
    } else {
      at = 11388; l8281 = K[3161];
      at = 11389; l8279 = l8281;
    }
    at = 11390; l8282 = K[3162];
    at = 11391; l8283 = rt.int_div(l8279, l8282);
    at = 11392; l8284 = K[3163];
    at = 11393; l8285 = rt.max(l8284, l8283);
    at = 11394; l8286 = K[3164];
    at = 11395; l8287 = rt.gt(l8269, l8286);
    at = 11396; if (rt.truthy(l8287)) {
      at = 11397; if (rt.truthy(l8262)) {
        at = 11398; l8290 = field(rt, l8265, "color");
        at = 11399; l8289 = l8290;
      } else {
        at = 11400; l8291 = K[3165];
        at = 11401; l8289 = l8291;
      }
      at = 11402; l8292 = K[3166];
      at = 11403; l8293 = rt.sequence_get(l8292, l8289);
      at = 11404; l8288 = l8293;
    } else {
      at = 11405; l8294 = K[3167];
      at = 11406; l8288 = l8294;
    }
    at = 11407; markSet(rt, false, "circle", {at: l8272, radius: l8285, fill: l8288});
    at = 11408; if (rt.truthy(l8262)) {
      at = 11409; l8296 = field(rt, l8265, "points");
      at = 11410; l8295 = l8296;
    } else {
      at = 11411; l8297 = K[3168];
      at = 11412; l8295 = l8297;
    }
    at = 11413; l8298 = K[3169];
    at = 11414; l8299 = rt.sub(l8269, l8298);
    at = 11415; l8300 = K[3170];
    at = 11416; l8301 = rt.max(l8300, l8299);
    at = 11417; l8302 = K[3171];
    at = 11418; l8303 = rt.array_slice(l8295, l8302, l8301);
    at = 11419; if (rt.truthy(l8262)) {
      at = 11420; l8305 = field(rt, l8265, "points");
      at = 11421; l8304 = l8305;
    } else {
      at = 11422; l8306 = K[3172];
      at = 11423; l8304 = l8306;
    }
    at = 11424; l8307 = K[3173];
    at = 11425; l8308 = rt.gt(l8269, l8307);
    at = 11426; if (rt.truthy(l8308)) {
      at = 11427; l8310 = K[3174];
      at = 11428; l8309 = l8310;
    } else {
      at = 11429; l8311 = K[3175];
      at = 11430; l8309 = l8311;
    }
    at = 11431; l8312 = K[3176];
    at = 11432; l8313 = rt.sub(l8269, l8312);
    at = 11433; l8314 = K[3177];
    at = 11434; l8315 = rt.max(l8314, l8313);
    at = 11435; l8316 = rt.array_slice(l8304, l8309, l8315);
    at = 11436; if (rt.truthy(l8262)) {
      at = 11437; l8318 = field(rt, l8265, "color");
      at = 11438; l8317 = l8318;
    } else {
      at = 11439; l8319 = K[3178];
      at = 11440; l8317 = l8319;
    }
    at = 11441; l8320 = K[3179];
    at = 11442; l8321 = rt.sequence_get(l8320, l8317);
    at = 11443; if (rt.truthy(l8262)) {
      at = 11444; l8323 = field(rt, l8265, "width");
      at = 11445; l8322 = l8323;
    } else {
      at = 11446; l8324 = K[3180];
      at = 11447; l8322 = l8324;
    }
    at = 11448; markSet(rt, true, "line", {from: l8303, to: l8316, color: l8321, width: l8322});
    at = 11449; l8325 = field(rt, l15, "strokes");
    at = 11450; l8326 = rt.sequence_length(l8325);
    at = 11451; l8327 = K[3181];
    at = 11452; l8328 = rt.lt(l8327, l8326);
    at = 11453; if (rt.truthy(l8328)) {
      at = 11454; l8329 = field(rt, l15, "strokes");
      at = 11455; l8330 = K[3182];
      at = 11456; l8331 = rt.sequence_get(l8329, l8330);
    }
    at = 11457; if (rt.truthy(l8328)) {
      at = 11458; l8333 = field(rt, l8331, "points");
      at = 11459; l8332 = l8333;
    } else {
      at = 11460; l8334 = K[3183];
      at = 11461; l8332 = l8334;
    }
    at = 11462; l8335 = rt.array_count(l8332);
    at = 11463; l8336 = K[3184];
    at = 11464; l8337 = rt.gt(l8335, l8336);
    at = 11465; if (rt.truthy(l8337)) {
      at = 11466; if (rt.truthy(l8328)) {
        at = 11467; l8340 = field(rt, l8331, "points");
        at = 11468; l8339 = l8340;
      } else {
        at = 11469; l8341 = K[3185];
        at = 11470; l8339 = l8341;
      }
      at = 11471; l8342 = K[3186];
      at = 11472; l8343 = rt.array_nth(l8339, l8342);
      at = 11473; l8338 = l8343;
    } else {
      at = 11474; l8344 = K[3187];
      at = 11475; l8338 = l8344;
    }
    at = 11476; if (rt.truthy(l8328)) {
      at = 11477; l8346 = field(rt, l8331, "width");
      at = 11478; l8345 = l8346;
    } else {
      at = 11479; l8347 = K[3188];
      at = 11480; l8345 = l8347;
    }
    at = 11481; l8348 = K[3189];
    at = 11482; l8349 = rt.int_div(l8345, l8348);
    at = 11483; l8350 = K[3190];
    at = 11484; l8351 = rt.max(l8350, l8349);
    at = 11485; l8352 = K[3191];
    at = 11486; l8353 = rt.gt(l8335, l8352);
    at = 11487; if (rt.truthy(l8353)) {
      at = 11488; if (rt.truthy(l8328)) {
        at = 11489; l8356 = field(rt, l8331, "color");
        at = 11490; l8355 = l8356;
      } else {
        at = 11491; l8357 = K[3192];
        at = 11492; l8355 = l8357;
      }
      at = 11493; l8358 = K[3193];
      at = 11494; l8359 = rt.sequence_get(l8358, l8355);
      at = 11495; l8354 = l8359;
    } else {
      at = 11496; l8360 = K[3194];
      at = 11497; l8354 = l8360;
    }
    at = 11498; markSet(rt, false, "circle", {at: l8338, radius: l8351, fill: l8354});
    at = 11499; if (rt.truthy(l8328)) {
      at = 11500; l8362 = field(rt, l8331, "points");
      at = 11501; l8361 = l8362;
    } else {
      at = 11502; l8363 = K[3195];
      at = 11503; l8361 = l8363;
    }
    at = 11504; l8364 = K[3196];
    at = 11505; l8365 = rt.sub(l8335, l8364);
    at = 11506; l8366 = K[3197];
    at = 11507; l8367 = rt.max(l8366, l8365);
    at = 11508; l8368 = K[3198];
    at = 11509; l8369 = rt.array_slice(l8361, l8368, l8367);
    at = 11510; if (rt.truthy(l8328)) {
      at = 11511; l8371 = field(rt, l8331, "points");
      at = 11512; l8370 = l8371;
    } else {
      at = 11513; l8372 = K[3199];
      at = 11514; l8370 = l8372;
    }
    at = 11515; l8373 = K[3200];
    at = 11516; l8374 = rt.gt(l8335, l8373);
    at = 11517; if (rt.truthy(l8374)) {
      at = 11518; l8376 = K[3201];
      at = 11519; l8375 = l8376;
    } else {
      at = 11520; l8377 = K[3202];
      at = 11521; l8375 = l8377;
    }
    at = 11522; l8378 = K[3203];
    at = 11523; l8379 = rt.sub(l8335, l8378);
    at = 11524; l8380 = K[3204];
    at = 11525; l8381 = rt.max(l8380, l8379);
    at = 11526; l8382 = rt.array_slice(l8370, l8375, l8381);
    at = 11527; if (rt.truthy(l8328)) {
      at = 11528; l8384 = field(rt, l8331, "color");
      at = 11529; l8383 = l8384;
    } else {
      at = 11530; l8385 = K[3205];
      at = 11531; l8383 = l8385;
    }
    at = 11532; l8386 = K[3206];
    at = 11533; l8387 = rt.sequence_get(l8386, l8383);
    at = 11534; if (rt.truthy(l8328)) {
      at = 11535; l8389 = field(rt, l8331, "width");
      at = 11536; l8388 = l8389;
    } else {
      at = 11537; l8390 = K[3207];
      at = 11538; l8388 = l8390;
    }
    at = 11539; markSet(rt, true, "line", {from: l8369, to: l8382, color: l8387, width: l8388});
    at = 11540; l8391 = field(rt, l15, "strokes");
    at = 11541; l8392 = rt.sequence_length(l8391);
    at = 11542; l8393 = K[3208];
    at = 11543; l8394 = rt.lt(l8393, l8392);
    at = 11544; if (rt.truthy(l8394)) {
      at = 11545; l8395 = field(rt, l15, "strokes");
      at = 11546; l8396 = K[3209];
      at = 11547; l8397 = rt.sequence_get(l8395, l8396);
    }
    at = 11548; if (rt.truthy(l8394)) {
      at = 11549; l8399 = field(rt, l8397, "points");
      at = 11550; l8398 = l8399;
    } else {
      at = 11551; l8400 = K[3210];
      at = 11552; l8398 = l8400;
    }
    at = 11553; l8401 = rt.array_count(l8398);
    at = 11554; l8402 = K[3211];
    at = 11555; l8403 = rt.gt(l8401, l8402);
    at = 11556; if (rt.truthy(l8403)) {
      at = 11557; if (rt.truthy(l8394)) {
        at = 11558; l8406 = field(rt, l8397, "points");
        at = 11559; l8405 = l8406;
      } else {
        at = 11560; l8407 = K[3212];
        at = 11561; l8405 = l8407;
      }
      at = 11562; l8408 = K[3213];
      at = 11563; l8409 = rt.array_nth(l8405, l8408);
      at = 11564; l8404 = l8409;
    } else {
      at = 11565; l8410 = K[3214];
      at = 11566; l8404 = l8410;
    }
    at = 11567; if (rt.truthy(l8394)) {
      at = 11568; l8412 = field(rt, l8397, "width");
      at = 11569; l8411 = l8412;
    } else {
      at = 11570; l8413 = K[3215];
      at = 11571; l8411 = l8413;
    }
    at = 11572; l8414 = K[3216];
    at = 11573; l8415 = rt.int_div(l8411, l8414);
    at = 11574; l8416 = K[3217];
    at = 11575; l8417 = rt.max(l8416, l8415);
    at = 11576; l8418 = K[3218];
    at = 11577; l8419 = rt.gt(l8401, l8418);
    at = 11578; if (rt.truthy(l8419)) {
      at = 11579; if (rt.truthy(l8394)) {
        at = 11580; l8422 = field(rt, l8397, "color");
        at = 11581; l8421 = l8422;
      } else {
        at = 11582; l8423 = K[3219];
        at = 11583; l8421 = l8423;
      }
      at = 11584; l8424 = K[3220];
      at = 11585; l8425 = rt.sequence_get(l8424, l8421);
      at = 11586; l8420 = l8425;
    } else {
      at = 11587; l8426 = K[3221];
      at = 11588; l8420 = l8426;
    }
    at = 11589; markSet(rt, false, "circle", {at: l8404, radius: l8417, fill: l8420});
    at = 11590; if (rt.truthy(l8394)) {
      at = 11591; l8428 = field(rt, l8397, "points");
      at = 11592; l8427 = l8428;
    } else {
      at = 11593; l8429 = K[3222];
      at = 11594; l8427 = l8429;
    }
    at = 11595; l8430 = K[3223];
    at = 11596; l8431 = rt.sub(l8401, l8430);
    at = 11597; l8432 = K[3224];
    at = 11598; l8433 = rt.max(l8432, l8431);
    at = 11599; l8434 = K[3225];
    at = 11600; l8435 = rt.array_slice(l8427, l8434, l8433);
    at = 11601; if (rt.truthy(l8394)) {
      at = 11602; l8437 = field(rt, l8397, "points");
      at = 11603; l8436 = l8437;
    } else {
      at = 11604; l8438 = K[3226];
      at = 11605; l8436 = l8438;
    }
    at = 11606; l8439 = K[3227];
    at = 11607; l8440 = rt.gt(l8401, l8439);
    at = 11608; if (rt.truthy(l8440)) {
      at = 11609; l8442 = K[3228];
      at = 11610; l8441 = l8442;
    } else {
      at = 11611; l8443 = K[3229];
      at = 11612; l8441 = l8443;
    }
    at = 11613; l8444 = K[3230];
    at = 11614; l8445 = rt.sub(l8401, l8444);
    at = 11615; l8446 = K[3231];
    at = 11616; l8447 = rt.max(l8446, l8445);
    at = 11617; l8448 = rt.array_slice(l8436, l8441, l8447);
    at = 11618; if (rt.truthy(l8394)) {
      at = 11619; l8450 = field(rt, l8397, "color");
      at = 11620; l8449 = l8450;
    } else {
      at = 11621; l8451 = K[3232];
      at = 11622; l8449 = l8451;
    }
    at = 11623; l8452 = K[3233];
    at = 11624; l8453 = rt.sequence_get(l8452, l8449);
    at = 11625; if (rt.truthy(l8394)) {
      at = 11626; l8455 = field(rt, l8397, "width");
      at = 11627; l8454 = l8455;
    } else {
      at = 11628; l8456 = K[3234];
      at = 11629; l8454 = l8456;
    }
    at = 11630; markSet(rt, true, "line", {from: l8435, to: l8448, color: l8453, width: l8454});
    at = 11631; l8457 = field(rt, l15, "strokes");
    at = 11632; l8458 = rt.sequence_length(l8457);
    at = 11633; l8459 = K[3235];
    at = 11634; l8460 = rt.lt(l8459, l8458);
    at = 11635; if (rt.truthy(l8460)) {
      at = 11636; l8461 = field(rt, l15, "strokes");
      at = 11637; l8462 = K[3236];
      at = 11638; l8463 = rt.sequence_get(l8461, l8462);
    }
    at = 11639; if (rt.truthy(l8460)) {
      at = 11640; l8465 = field(rt, l8463, "points");
      at = 11641; l8464 = l8465;
    } else {
      at = 11642; l8466 = K[3237];
      at = 11643; l8464 = l8466;
    }
    at = 11644; l8467 = rt.array_count(l8464);
    at = 11645; l8468 = K[3238];
    at = 11646; l8469 = rt.gt(l8467, l8468);
    at = 11647; if (rt.truthy(l8469)) {
      at = 11648; if (rt.truthy(l8460)) {
        at = 11649; l8472 = field(rt, l8463, "points");
        at = 11650; l8471 = l8472;
      } else {
        at = 11651; l8473 = K[3239];
        at = 11652; l8471 = l8473;
      }
      at = 11653; l8474 = K[3240];
      at = 11654; l8475 = rt.array_nth(l8471, l8474);
      at = 11655; l8470 = l8475;
    } else {
      at = 11656; l8476 = K[3241];
      at = 11657; l8470 = l8476;
    }
    at = 11658; if (rt.truthy(l8460)) {
      at = 11659; l8478 = field(rt, l8463, "width");
      at = 11660; l8477 = l8478;
    } else {
      at = 11661; l8479 = K[3242];
      at = 11662; l8477 = l8479;
    }
    at = 11663; l8480 = K[3243];
    at = 11664; l8481 = rt.int_div(l8477, l8480);
    at = 11665; l8482 = K[3244];
    at = 11666; l8483 = rt.max(l8482, l8481);
    at = 11667; l8484 = K[3245];
    at = 11668; l8485 = rt.gt(l8467, l8484);
    at = 11669; if (rt.truthy(l8485)) {
      at = 11670; if (rt.truthy(l8460)) {
        at = 11671; l8488 = field(rt, l8463, "color");
        at = 11672; l8487 = l8488;
      } else {
        at = 11673; l8489 = K[3246];
        at = 11674; l8487 = l8489;
      }
      at = 11675; l8490 = K[3247];
      at = 11676; l8491 = rt.sequence_get(l8490, l8487);
      at = 11677; l8486 = l8491;
    } else {
      at = 11678; l8492 = K[3248];
      at = 11679; l8486 = l8492;
    }
    at = 11680; markSet(rt, false, "circle", {at: l8470, radius: l8483, fill: l8486});
    at = 11681; if (rt.truthy(l8460)) {
      at = 11682; l8494 = field(rt, l8463, "points");
      at = 11683; l8493 = l8494;
    } else {
      at = 11684; l8495 = K[3249];
      at = 11685; l8493 = l8495;
    }
    at = 11686; l8496 = K[3250];
    at = 11687; l8497 = rt.sub(l8467, l8496);
    at = 11688; l8498 = K[3251];
    at = 11689; l8499 = rt.max(l8498, l8497);
    at = 11690; l8500 = K[3252];
    at = 11691; l8501 = rt.array_slice(l8493, l8500, l8499);
    at = 11692; if (rt.truthy(l8460)) {
      at = 11693; l8503 = field(rt, l8463, "points");
      at = 11694; l8502 = l8503;
    } else {
      at = 11695; l8504 = K[3253];
      at = 11696; l8502 = l8504;
    }
    at = 11697; l8505 = K[3254];
    at = 11698; l8506 = rt.gt(l8467, l8505);
    at = 11699; if (rt.truthy(l8506)) {
      at = 11700; l8508 = K[3255];
      at = 11701; l8507 = l8508;
    } else {
      at = 11702; l8509 = K[3256];
      at = 11703; l8507 = l8509;
    }
    at = 11704; l8510 = K[3257];
    at = 11705; l8511 = rt.sub(l8467, l8510);
    at = 11706; l8512 = K[3258];
    at = 11707; l8513 = rt.max(l8512, l8511);
    at = 11708; l8514 = rt.array_slice(l8502, l8507, l8513);
    at = 11709; if (rt.truthy(l8460)) {
      at = 11710; l8516 = field(rt, l8463, "color");
      at = 11711; l8515 = l8516;
    } else {
      at = 11712; l8517 = K[3259];
      at = 11713; l8515 = l8517;
    }
    at = 11714; l8518 = K[3260];
    at = 11715; l8519 = rt.sequence_get(l8518, l8515);
    at = 11716; if (rt.truthy(l8460)) {
      at = 11717; l8521 = field(rt, l8463, "width");
      at = 11718; l8520 = l8521;
    } else {
      at = 11719; l8522 = K[3261];
      at = 11720; l8520 = l8522;
    }
    at = 11721; markSet(rt, true, "line", {from: l8501, to: l8514, color: l8519, width: l8520});
    at = 11722; l8523 = field(rt, l15, "strokes");
    at = 11723; l8524 = rt.sequence_length(l8523);
    at = 11724; l8525 = K[3262];
    at = 11725; l8526 = rt.lt(l8525, l8524);
    at = 11726; if (rt.truthy(l8526)) {
      at = 11727; l8527 = field(rt, l15, "strokes");
      at = 11728; l8528 = K[3263];
      at = 11729; l8529 = rt.sequence_get(l8527, l8528);
    }
    at = 11730; if (rt.truthy(l8526)) {
      at = 11731; l8531 = field(rt, l8529, "points");
      at = 11732; l8530 = l8531;
    } else {
      at = 11733; l8532 = K[3264];
      at = 11734; l8530 = l8532;
    }
    at = 11735; l8533 = rt.array_count(l8530);
    at = 11736; l8534 = K[3265];
    at = 11737; l8535 = rt.gt(l8533, l8534);
    at = 11738; if (rt.truthy(l8535)) {
      at = 11739; if (rt.truthy(l8526)) {
        at = 11740; l8538 = field(rt, l8529, "points");
        at = 11741; l8537 = l8538;
      } else {
        at = 11742; l8539 = K[3266];
        at = 11743; l8537 = l8539;
      }
      at = 11744; l8540 = K[3267];
      at = 11745; l8541 = rt.array_nth(l8537, l8540);
      at = 11746; l8536 = l8541;
    } else {
      at = 11747; l8542 = K[3268];
      at = 11748; l8536 = l8542;
    }
    at = 11749; if (rt.truthy(l8526)) {
      at = 11750; l8544 = field(rt, l8529, "width");
      at = 11751; l8543 = l8544;
    } else {
      at = 11752; l8545 = K[3269];
      at = 11753; l8543 = l8545;
    }
    at = 11754; l8546 = K[3270];
    at = 11755; l8547 = rt.int_div(l8543, l8546);
    at = 11756; l8548 = K[3271];
    at = 11757; l8549 = rt.max(l8548, l8547);
    at = 11758; l8550 = K[3272];
    at = 11759; l8551 = rt.gt(l8533, l8550);
    at = 11760; if (rt.truthy(l8551)) {
      at = 11761; if (rt.truthy(l8526)) {
        at = 11762; l8554 = field(rt, l8529, "color");
        at = 11763; l8553 = l8554;
      } else {
        at = 11764; l8555 = K[3273];
        at = 11765; l8553 = l8555;
      }
      at = 11766; l8556 = K[3274];
      at = 11767; l8557 = rt.sequence_get(l8556, l8553);
      at = 11768; l8552 = l8557;
    } else {
      at = 11769; l8558 = K[3275];
      at = 11770; l8552 = l8558;
    }
    at = 11771; markSet(rt, false, "circle", {at: l8536, radius: l8549, fill: l8552});
    at = 11772; if (rt.truthy(l8526)) {
      at = 11773; l8560 = field(rt, l8529, "points");
      at = 11774; l8559 = l8560;
    } else {
      at = 11775; l8561 = K[3276];
      at = 11776; l8559 = l8561;
    }
    at = 11777; l8562 = K[3277];
    at = 11778; l8563 = rt.sub(l8533, l8562);
    at = 11779; l8564 = K[3278];
    at = 11780; l8565 = rt.max(l8564, l8563);
    at = 11781; l8566 = K[3279];
    at = 11782; l8567 = rt.array_slice(l8559, l8566, l8565);
    at = 11783; if (rt.truthy(l8526)) {
      at = 11784; l8569 = field(rt, l8529, "points");
      at = 11785; l8568 = l8569;
    } else {
      at = 11786; l8570 = K[3280];
      at = 11787; l8568 = l8570;
    }
    at = 11788; l8571 = K[3281];
    at = 11789; l8572 = rt.gt(l8533, l8571);
    at = 11790; if (rt.truthy(l8572)) {
      at = 11791; l8574 = K[3282];
      at = 11792; l8573 = l8574;
    } else {
      at = 11793; l8575 = K[3283];
      at = 11794; l8573 = l8575;
    }
    at = 11795; l8576 = K[3284];
    at = 11796; l8577 = rt.sub(l8533, l8576);
    at = 11797; l8578 = K[3285];
    at = 11798; l8579 = rt.max(l8578, l8577);
    at = 11799; l8580 = rt.array_slice(l8568, l8573, l8579);
    at = 11800; if (rt.truthy(l8526)) {
      at = 11801; l8582 = field(rt, l8529, "color");
      at = 11802; l8581 = l8582;
    } else {
      at = 11803; l8583 = K[3286];
      at = 11804; l8581 = l8583;
    }
    at = 11805; l8584 = K[3287];
    at = 11806; l8585 = rt.sequence_get(l8584, l8581);
    at = 11807; if (rt.truthy(l8526)) {
      at = 11808; l8587 = field(rt, l8529, "width");
      at = 11809; l8586 = l8587;
    } else {
      at = 11810; l8588 = K[3288];
      at = 11811; l8586 = l8588;
    }
    at = 11812; markSet(rt, true, "line", {from: l8567, to: l8580, color: l8585, width: l8586});
    at = 11813; l8589 = field(rt, l15, "strokes");
    at = 11814; l8590 = rt.sequence_length(l8589);
    at = 11815; l8591 = K[3289];
    at = 11816; l8592 = rt.lt(l8591, l8590);
    at = 11817; if (rt.truthy(l8592)) {
      at = 11818; l8593 = field(rt, l15, "strokes");
      at = 11819; l8594 = K[3290];
      at = 11820; l8595 = rt.sequence_get(l8593, l8594);
    }
    at = 11821; if (rt.truthy(l8592)) {
      at = 11822; l8597 = field(rt, l8595, "points");
      at = 11823; l8596 = l8597;
    } else {
      at = 11824; l8598 = K[3291];
      at = 11825; l8596 = l8598;
    }
    at = 11826; l8599 = rt.array_count(l8596);
    at = 11827; l8600 = K[3292];
    at = 11828; l8601 = rt.gt(l8599, l8600);
    at = 11829; if (rt.truthy(l8601)) {
      at = 11830; if (rt.truthy(l8592)) {
        at = 11831; l8604 = field(rt, l8595, "points");
        at = 11832; l8603 = l8604;
      } else {
        at = 11833; l8605 = K[3293];
        at = 11834; l8603 = l8605;
      }
      at = 11835; l8606 = K[3294];
      at = 11836; l8607 = rt.array_nth(l8603, l8606);
      at = 11837; l8602 = l8607;
    } else {
      at = 11838; l8608 = K[3295];
      at = 11839; l8602 = l8608;
    }
    at = 11840; if (rt.truthy(l8592)) {
      at = 11841; l8610 = field(rt, l8595, "width");
      at = 11842; l8609 = l8610;
    } else {
      at = 11843; l8611 = K[3296];
      at = 11844; l8609 = l8611;
    }
    at = 11845; l8612 = K[3297];
    at = 11846; l8613 = rt.int_div(l8609, l8612);
    at = 11847; l8614 = K[3298];
    at = 11848; l8615 = rt.max(l8614, l8613);
    at = 11849; l8616 = K[3299];
    at = 11850; l8617 = rt.gt(l8599, l8616);
    at = 11851; if (rt.truthy(l8617)) {
      at = 11852; if (rt.truthy(l8592)) {
        at = 11853; l8620 = field(rt, l8595, "color");
        at = 11854; l8619 = l8620;
      } else {
        at = 11855; l8621 = K[3300];
        at = 11856; l8619 = l8621;
      }
      at = 11857; l8622 = K[3301];
      at = 11858; l8623 = rt.sequence_get(l8622, l8619);
      at = 11859; l8618 = l8623;
    } else {
      at = 11860; l8624 = K[3302];
      at = 11861; l8618 = l8624;
    }
    at = 11862; markSet(rt, false, "circle", {at: l8602, radius: l8615, fill: l8618});
    at = 11863; if (rt.truthy(l8592)) {
      at = 11864; l8626 = field(rt, l8595, "points");
      at = 11865; l8625 = l8626;
    } else {
      at = 11866; l8627 = K[3303];
      at = 11867; l8625 = l8627;
    }
    at = 11868; l8628 = K[3304];
    at = 11869; l8629 = rt.sub(l8599, l8628);
    at = 11870; l8630 = K[3305];
    at = 11871; l8631 = rt.max(l8630, l8629);
    at = 11872; l8632 = K[3306];
    at = 11873; l8633 = rt.array_slice(l8625, l8632, l8631);
    at = 11874; if (rt.truthy(l8592)) {
      at = 11875; l8635 = field(rt, l8595, "points");
      at = 11876; l8634 = l8635;
    } else {
      at = 11877; l8636 = K[3307];
      at = 11878; l8634 = l8636;
    }
    at = 11879; l8637 = K[3308];
    at = 11880; l8638 = rt.gt(l8599, l8637);
    at = 11881; if (rt.truthy(l8638)) {
      at = 11882; l8640 = K[3309];
      at = 11883; l8639 = l8640;
    } else {
      at = 11884; l8641 = K[3310];
      at = 11885; l8639 = l8641;
    }
    at = 11886; l8642 = K[3311];
    at = 11887; l8643 = rt.sub(l8599, l8642);
    at = 11888; l8644 = K[3312];
    at = 11889; l8645 = rt.max(l8644, l8643);
    at = 11890; l8646 = rt.array_slice(l8634, l8639, l8645);
    at = 11891; if (rt.truthy(l8592)) {
      at = 11892; l8648 = field(rt, l8595, "color");
      at = 11893; l8647 = l8648;
    } else {
      at = 11894; l8649 = K[3313];
      at = 11895; l8647 = l8649;
    }
    at = 11896; l8650 = K[3314];
    at = 11897; l8651 = rt.sequence_get(l8650, l8647);
    at = 11898; if (rt.truthy(l8592)) {
      at = 11899; l8653 = field(rt, l8595, "width");
      at = 11900; l8652 = l8653;
    } else {
      at = 11901; l8654 = K[3315];
      at = 11902; l8652 = l8654;
    }
    at = 11903; markSet(rt, true, "line", {from: l8633, to: l8646, color: l8651, width: l8652});
    at = 11904; l8655 = field(rt, l15, "strokes");
    at = 11905; l8656 = rt.sequence_length(l8655);
    at = 11906; l8657 = K[3316];
    at = 11907; l8658 = rt.lt(l8657, l8656);
    at = 11908; if (rt.truthy(l8658)) {
      at = 11909; l8659 = field(rt, l15, "strokes");
      at = 11910; l8660 = K[3317];
      at = 11911; l8661 = rt.sequence_get(l8659, l8660);
    }
    at = 11912; if (rt.truthy(l8658)) {
      at = 11913; l8663 = field(rt, l8661, "points");
      at = 11914; l8662 = l8663;
    } else {
      at = 11915; l8664 = K[3318];
      at = 11916; l8662 = l8664;
    }
    at = 11917; l8665 = rt.array_count(l8662);
    at = 11918; l8666 = K[3319];
    at = 11919; l8667 = rt.gt(l8665, l8666);
    at = 11920; if (rt.truthy(l8667)) {
      at = 11921; if (rt.truthy(l8658)) {
        at = 11922; l8670 = field(rt, l8661, "points");
        at = 11923; l8669 = l8670;
      } else {
        at = 11924; l8671 = K[3320];
        at = 11925; l8669 = l8671;
      }
      at = 11926; l8672 = K[3321];
      at = 11927; l8673 = rt.array_nth(l8669, l8672);
      at = 11928; l8668 = l8673;
    } else {
      at = 11929; l8674 = K[3322];
      at = 11930; l8668 = l8674;
    }
    at = 11931; if (rt.truthy(l8658)) {
      at = 11932; l8676 = field(rt, l8661, "width");
      at = 11933; l8675 = l8676;
    } else {
      at = 11934; l8677 = K[3323];
      at = 11935; l8675 = l8677;
    }
    at = 11936; l8678 = K[3324];
    at = 11937; l8679 = rt.int_div(l8675, l8678);
    at = 11938; l8680 = K[3325];
    at = 11939; l8681 = rt.max(l8680, l8679);
    at = 11940; l8682 = K[3326];
    at = 11941; l8683 = rt.gt(l8665, l8682);
    at = 11942; if (rt.truthy(l8683)) {
      at = 11943; if (rt.truthy(l8658)) {
        at = 11944; l8686 = field(rt, l8661, "color");
        at = 11945; l8685 = l8686;
      } else {
        at = 11946; l8687 = K[3327];
        at = 11947; l8685 = l8687;
      }
      at = 11948; l8688 = K[3328];
      at = 11949; l8689 = rt.sequence_get(l8688, l8685);
      at = 11950; l8684 = l8689;
    } else {
      at = 11951; l8690 = K[3329];
      at = 11952; l8684 = l8690;
    }
    at = 11953; markSet(rt, false, "circle", {at: l8668, radius: l8681, fill: l8684});
    at = 11954; if (rt.truthy(l8658)) {
      at = 11955; l8692 = field(rt, l8661, "points");
      at = 11956; l8691 = l8692;
    } else {
      at = 11957; l8693 = K[3330];
      at = 11958; l8691 = l8693;
    }
    at = 11959; l8694 = K[3331];
    at = 11960; l8695 = rt.sub(l8665, l8694);
    at = 11961; l8696 = K[3332];
    at = 11962; l8697 = rt.max(l8696, l8695);
    at = 11963; l8698 = K[3333];
    at = 11964; l8699 = rt.array_slice(l8691, l8698, l8697);
    at = 11965; if (rt.truthy(l8658)) {
      at = 11966; l8701 = field(rt, l8661, "points");
      at = 11967; l8700 = l8701;
    } else {
      at = 11968; l8702 = K[3334];
      at = 11969; l8700 = l8702;
    }
    at = 11970; l8703 = K[3335];
    at = 11971; l8704 = rt.gt(l8665, l8703);
    at = 11972; if (rt.truthy(l8704)) {
      at = 11973; l8706 = K[3336];
      at = 11974; l8705 = l8706;
    } else {
      at = 11975; l8707 = K[3337];
      at = 11976; l8705 = l8707;
    }
    at = 11977; l8708 = K[3338];
    at = 11978; l8709 = rt.sub(l8665, l8708);
    at = 11979; l8710 = K[3339];
    at = 11980; l8711 = rt.max(l8710, l8709);
    at = 11981; l8712 = rt.array_slice(l8700, l8705, l8711);
    at = 11982; if (rt.truthy(l8658)) {
      at = 11983; l8714 = field(rt, l8661, "color");
      at = 11984; l8713 = l8714;
    } else {
      at = 11985; l8715 = K[3340];
      at = 11986; l8713 = l8715;
    }
    at = 11987; l8716 = K[3341];
    at = 11988; l8717 = rt.sequence_get(l8716, l8713);
    at = 11989; if (rt.truthy(l8658)) {
      at = 11990; l8719 = field(rt, l8661, "width");
      at = 11991; l8718 = l8719;
    } else {
      at = 11992; l8720 = K[3342];
      at = 11993; l8718 = l8720;
    }
    at = 11994; markSet(rt, true, "line", {from: l8699, to: l8712, color: l8717, width: l8718});
    at = 11995; l8721 = field(rt, l15, "strokes");
    at = 11996; l8722 = rt.sequence_length(l8721);
    at = 11997; l8723 = K[3343];
    at = 11998; l8724 = rt.lt(l8723, l8722);
    at = 11999; if (rt.truthy(l8724)) {
      at = 12000; l8725 = field(rt, l15, "strokes");
      at = 12001; l8726 = K[3344];
      at = 12002; l8727 = rt.sequence_get(l8725, l8726);
    }
    at = 12003; if (rt.truthy(l8724)) {
      at = 12004; l8729 = field(rt, l8727, "points");
      at = 12005; l8728 = l8729;
    } else {
      at = 12006; l8730 = K[3345];
      at = 12007; l8728 = l8730;
    }
    at = 12008; l8731 = rt.array_count(l8728);
    at = 12009; l8732 = K[3346];
    at = 12010; l8733 = rt.gt(l8731, l8732);
    at = 12011; if (rt.truthy(l8733)) {
      at = 12012; if (rt.truthy(l8724)) {
        at = 12013; l8736 = field(rt, l8727, "points");
        at = 12014; l8735 = l8736;
      } else {
        at = 12015; l8737 = K[3347];
        at = 12016; l8735 = l8737;
      }
      at = 12017; l8738 = K[3348];
      at = 12018; l8739 = rt.array_nth(l8735, l8738);
      at = 12019; l8734 = l8739;
    } else {
      at = 12020; l8740 = K[3349];
      at = 12021; l8734 = l8740;
    }
    at = 12022; if (rt.truthy(l8724)) {
      at = 12023; l8742 = field(rt, l8727, "width");
      at = 12024; l8741 = l8742;
    } else {
      at = 12025; l8743 = K[3350];
      at = 12026; l8741 = l8743;
    }
    at = 12027; l8744 = K[3351];
    at = 12028; l8745 = rt.int_div(l8741, l8744);
    at = 12029; l8746 = K[3352];
    at = 12030; l8747 = rt.max(l8746, l8745);
    at = 12031; l8748 = K[3353];
    at = 12032; l8749 = rt.gt(l8731, l8748);
    at = 12033; if (rt.truthy(l8749)) {
      at = 12034; if (rt.truthy(l8724)) {
        at = 12035; l8752 = field(rt, l8727, "color");
        at = 12036; l8751 = l8752;
      } else {
        at = 12037; l8753 = K[3354];
        at = 12038; l8751 = l8753;
      }
      at = 12039; l8754 = K[3355];
      at = 12040; l8755 = rt.sequence_get(l8754, l8751);
      at = 12041; l8750 = l8755;
    } else {
      at = 12042; l8756 = K[3356];
      at = 12043; l8750 = l8756;
    }
    at = 12044; markSet(rt, false, "circle", {at: l8734, radius: l8747, fill: l8750});
    at = 12045; if (rt.truthy(l8724)) {
      at = 12046; l8758 = field(rt, l8727, "points");
      at = 12047; l8757 = l8758;
    } else {
      at = 12048; l8759 = K[3357];
      at = 12049; l8757 = l8759;
    }
    at = 12050; l8760 = K[3358];
    at = 12051; l8761 = rt.sub(l8731, l8760);
    at = 12052; l8762 = K[3359];
    at = 12053; l8763 = rt.max(l8762, l8761);
    at = 12054; l8764 = K[3360];
    at = 12055; l8765 = rt.array_slice(l8757, l8764, l8763);
    at = 12056; if (rt.truthy(l8724)) {
      at = 12057; l8767 = field(rt, l8727, "points");
      at = 12058; l8766 = l8767;
    } else {
      at = 12059; l8768 = K[3361];
      at = 12060; l8766 = l8768;
    }
    at = 12061; l8769 = K[3362];
    at = 12062; l8770 = rt.gt(l8731, l8769);
    at = 12063; if (rt.truthy(l8770)) {
      at = 12064; l8772 = K[3363];
      at = 12065; l8771 = l8772;
    } else {
      at = 12066; l8773 = K[3364];
      at = 12067; l8771 = l8773;
    }
    at = 12068; l8774 = K[3365];
    at = 12069; l8775 = rt.sub(l8731, l8774);
    at = 12070; l8776 = K[3366];
    at = 12071; l8777 = rt.max(l8776, l8775);
    at = 12072; l8778 = rt.array_slice(l8766, l8771, l8777);
    at = 12073; if (rt.truthy(l8724)) {
      at = 12074; l8780 = field(rt, l8727, "color");
      at = 12075; l8779 = l8780;
    } else {
      at = 12076; l8781 = K[3367];
      at = 12077; l8779 = l8781;
    }
    at = 12078; l8782 = K[3368];
    at = 12079; l8783 = rt.sequence_get(l8782, l8779);
    at = 12080; if (rt.truthy(l8724)) {
      at = 12081; l8785 = field(rt, l8727, "width");
      at = 12082; l8784 = l8785;
    } else {
      at = 12083; l8786 = K[3369];
      at = 12084; l8784 = l8786;
    }
    at = 12085; markSet(rt, true, "line", {from: l8765, to: l8778, color: l8783, width: l8784});
    at = 12086; l8787 = field(rt, l15, "strokes");
    at = 12087; l8788 = rt.sequence_length(l8787);
    at = 12088; l8789 = K[3370];
    at = 12089; l8790 = rt.lt(l8789, l8788);
    at = 12090; if (rt.truthy(l8790)) {
      at = 12091; l8791 = field(rt, l15, "strokes");
      at = 12092; l8792 = K[3371];
      at = 12093; l8793 = rt.sequence_get(l8791, l8792);
    }
    at = 12094; if (rt.truthy(l8790)) {
      at = 12095; l8795 = field(rt, l8793, "points");
      at = 12096; l8794 = l8795;
    } else {
      at = 12097; l8796 = K[3372];
      at = 12098; l8794 = l8796;
    }
    at = 12099; l8797 = rt.array_count(l8794);
    at = 12100; l8798 = K[3373];
    at = 12101; l8799 = rt.gt(l8797, l8798);
    at = 12102; if (rt.truthy(l8799)) {
      at = 12103; if (rt.truthy(l8790)) {
        at = 12104; l8802 = field(rt, l8793, "points");
        at = 12105; l8801 = l8802;
      } else {
        at = 12106; l8803 = K[3374];
        at = 12107; l8801 = l8803;
      }
      at = 12108; l8804 = K[3375];
      at = 12109; l8805 = rt.array_nth(l8801, l8804);
      at = 12110; l8800 = l8805;
    } else {
      at = 12111; l8806 = K[3376];
      at = 12112; l8800 = l8806;
    }
    at = 12113; if (rt.truthy(l8790)) {
      at = 12114; l8808 = field(rt, l8793, "width");
      at = 12115; l8807 = l8808;
    } else {
      at = 12116; l8809 = K[3377];
      at = 12117; l8807 = l8809;
    }
    at = 12118; l8810 = K[3378];
    at = 12119; l8811 = rt.int_div(l8807, l8810);
    at = 12120; l8812 = K[3379];
    at = 12121; l8813 = rt.max(l8812, l8811);
    at = 12122; l8814 = K[3380];
    at = 12123; l8815 = rt.gt(l8797, l8814);
    at = 12124; if (rt.truthy(l8815)) {
      at = 12125; if (rt.truthy(l8790)) {
        at = 12126; l8818 = field(rt, l8793, "color");
        at = 12127; l8817 = l8818;
      } else {
        at = 12128; l8819 = K[3381];
        at = 12129; l8817 = l8819;
      }
      at = 12130; l8820 = K[3382];
      at = 12131; l8821 = rt.sequence_get(l8820, l8817);
      at = 12132; l8816 = l8821;
    } else {
      at = 12133; l8822 = K[3383];
      at = 12134; l8816 = l8822;
    }
    at = 12135; markSet(rt, false, "circle", {at: l8800, radius: l8813, fill: l8816});
    at = 12136; if (rt.truthy(l8790)) {
      at = 12137; l8824 = field(rt, l8793, "points");
      at = 12138; l8823 = l8824;
    } else {
      at = 12139; l8825 = K[3384];
      at = 12140; l8823 = l8825;
    }
    at = 12141; l8826 = K[3385];
    at = 12142; l8827 = rt.sub(l8797, l8826);
    at = 12143; l8828 = K[3386];
    at = 12144; l8829 = rt.max(l8828, l8827);
    at = 12145; l8830 = K[3387];
    at = 12146; l8831 = rt.array_slice(l8823, l8830, l8829);
    at = 12147; if (rt.truthy(l8790)) {
      at = 12148; l8833 = field(rt, l8793, "points");
      at = 12149; l8832 = l8833;
    } else {
      at = 12150; l8834 = K[3388];
      at = 12151; l8832 = l8834;
    }
    at = 12152; l8835 = K[3389];
    at = 12153; l8836 = rt.gt(l8797, l8835);
    at = 12154; if (rt.truthy(l8836)) {
      at = 12155; l8838 = K[3390];
      at = 12156; l8837 = l8838;
    } else {
      at = 12157; l8839 = K[3391];
      at = 12158; l8837 = l8839;
    }
    at = 12159; l8840 = K[3392];
    at = 12160; l8841 = rt.sub(l8797, l8840);
    at = 12161; l8842 = K[3393];
    at = 12162; l8843 = rt.max(l8842, l8841);
    at = 12163; l8844 = rt.array_slice(l8832, l8837, l8843);
    at = 12164; if (rt.truthy(l8790)) {
      at = 12165; l8846 = field(rt, l8793, "color");
      at = 12166; l8845 = l8846;
    } else {
      at = 12167; l8847 = K[3394];
      at = 12168; l8845 = l8847;
    }
    at = 12169; l8848 = K[3395];
    at = 12170; l8849 = rt.sequence_get(l8848, l8845);
    at = 12171; if (rt.truthy(l8790)) {
      at = 12172; l8851 = field(rt, l8793, "width");
      at = 12173; l8850 = l8851;
    } else {
      at = 12174; l8852 = K[3396];
      at = 12175; l8850 = l8852;
    }
    at = 12176; markSet(rt, true, "line", {from: l8831, to: l8844, color: l8849, width: l8850});
    at = 12177; l8853 = field(rt, l15, "strokes");
    at = 12178; l8854 = rt.sequence_length(l8853);
    at = 12179; l8855 = K[3397];
    at = 12180; l8856 = rt.lt(l8855, l8854);
    at = 12181; if (rt.truthy(l8856)) {
      at = 12182; l8857 = field(rt, l15, "strokes");
      at = 12183; l8858 = K[3398];
      at = 12184; l8859 = rt.sequence_get(l8857, l8858);
    }
    at = 12185; if (rt.truthy(l8856)) {
      at = 12186; l8861 = field(rt, l8859, "points");
      at = 12187; l8860 = l8861;
    } else {
      at = 12188; l8862 = K[3399];
      at = 12189; l8860 = l8862;
    }
    at = 12190; l8863 = rt.array_count(l8860);
    at = 12191; l8864 = K[3400];
    at = 12192; l8865 = rt.gt(l8863, l8864);
    at = 12193; if (rt.truthy(l8865)) {
      at = 12194; if (rt.truthy(l8856)) {
        at = 12195; l8868 = field(rt, l8859, "points");
        at = 12196; l8867 = l8868;
      } else {
        at = 12197; l8869 = K[3401];
        at = 12198; l8867 = l8869;
      }
      at = 12199; l8870 = K[3402];
      at = 12200; l8871 = rt.array_nth(l8867, l8870);
      at = 12201; l8866 = l8871;
    } else {
      at = 12202; l8872 = K[3403];
      at = 12203; l8866 = l8872;
    }
    at = 12204; if (rt.truthy(l8856)) {
      at = 12205; l8874 = field(rt, l8859, "width");
      at = 12206; l8873 = l8874;
    } else {
      at = 12207; l8875 = K[3404];
      at = 12208; l8873 = l8875;
    }
    at = 12209; l8876 = K[3405];
    at = 12210; l8877 = rt.int_div(l8873, l8876);
    at = 12211; l8878 = K[3406];
    at = 12212; l8879 = rt.max(l8878, l8877);
    at = 12213; l8880 = K[3407];
    at = 12214; l8881 = rt.gt(l8863, l8880);
    at = 12215; if (rt.truthy(l8881)) {
      at = 12216; if (rt.truthy(l8856)) {
        at = 12217; l8884 = field(rt, l8859, "color");
        at = 12218; l8883 = l8884;
      } else {
        at = 12219; l8885 = K[3408];
        at = 12220; l8883 = l8885;
      }
      at = 12221; l8886 = K[3409];
      at = 12222; l8887 = rt.sequence_get(l8886, l8883);
      at = 12223; l8882 = l8887;
    } else {
      at = 12224; l8888 = K[3410];
      at = 12225; l8882 = l8888;
    }
    at = 12226; markSet(rt, false, "circle", {at: l8866, radius: l8879, fill: l8882});
    at = 12227; if (rt.truthy(l8856)) {
      at = 12228; l8890 = field(rt, l8859, "points");
      at = 12229; l8889 = l8890;
    } else {
      at = 12230; l8891 = K[3411];
      at = 12231; l8889 = l8891;
    }
    at = 12232; l8892 = K[3412];
    at = 12233; l8893 = rt.sub(l8863, l8892);
    at = 12234; l8894 = K[3413];
    at = 12235; l8895 = rt.max(l8894, l8893);
    at = 12236; l8896 = K[3414];
    at = 12237; l8897 = rt.array_slice(l8889, l8896, l8895);
    at = 12238; if (rt.truthy(l8856)) {
      at = 12239; l8899 = field(rt, l8859, "points");
      at = 12240; l8898 = l8899;
    } else {
      at = 12241; l8900 = K[3415];
      at = 12242; l8898 = l8900;
    }
    at = 12243; l8901 = K[3416];
    at = 12244; l8902 = rt.gt(l8863, l8901);
    at = 12245; if (rt.truthy(l8902)) {
      at = 12246; l8904 = K[3417];
      at = 12247; l8903 = l8904;
    } else {
      at = 12248; l8905 = K[3418];
      at = 12249; l8903 = l8905;
    }
    at = 12250; l8906 = K[3419];
    at = 12251; l8907 = rt.sub(l8863, l8906);
    at = 12252; l8908 = K[3420];
    at = 12253; l8909 = rt.max(l8908, l8907);
    at = 12254; l8910 = rt.array_slice(l8898, l8903, l8909);
    at = 12255; if (rt.truthy(l8856)) {
      at = 12256; l8912 = field(rt, l8859, "color");
      at = 12257; l8911 = l8912;
    } else {
      at = 12258; l8913 = K[3421];
      at = 12259; l8911 = l8913;
    }
    at = 12260; l8914 = K[3422];
    at = 12261; l8915 = rt.sequence_get(l8914, l8911);
    at = 12262; if (rt.truthy(l8856)) {
      at = 12263; l8917 = field(rt, l8859, "width");
      at = 12264; l8916 = l8917;
    } else {
      at = 12265; l8918 = K[3423];
      at = 12266; l8916 = l8918;
    }
    at = 12267; markSet(rt, true, "line", {from: l8897, to: l8910, color: l8915, width: l8916});
    at = 12268; l8919 = field(rt, l15, "strokes");
    at = 12269; l8920 = rt.sequence_length(l8919);
    at = 12270; l8921 = K[3424];
    at = 12271; l8922 = rt.lt(l8921, l8920);
    at = 12272; if (rt.truthy(l8922)) {
      at = 12273; l8923 = field(rt, l15, "strokes");
      at = 12274; l8924 = K[3425];
      at = 12275; l8925 = rt.sequence_get(l8923, l8924);
    }
    at = 12276; if (rt.truthy(l8922)) {
      at = 12277; l8927 = field(rt, l8925, "points");
      at = 12278; l8926 = l8927;
    } else {
      at = 12279; l8928 = K[3426];
      at = 12280; l8926 = l8928;
    }
    at = 12281; l8929 = rt.array_count(l8926);
    at = 12282; l8930 = K[3427];
    at = 12283; l8931 = rt.gt(l8929, l8930);
    at = 12284; if (rt.truthy(l8931)) {
      at = 12285; if (rt.truthy(l8922)) {
        at = 12286; l8934 = field(rt, l8925, "points");
        at = 12287; l8933 = l8934;
      } else {
        at = 12288; l8935 = K[3428];
        at = 12289; l8933 = l8935;
      }
      at = 12290; l8936 = K[3429];
      at = 12291; l8937 = rt.array_nth(l8933, l8936);
      at = 12292; l8932 = l8937;
    } else {
      at = 12293; l8938 = K[3430];
      at = 12294; l8932 = l8938;
    }
    at = 12295; if (rt.truthy(l8922)) {
      at = 12296; l8940 = field(rt, l8925, "width");
      at = 12297; l8939 = l8940;
    } else {
      at = 12298; l8941 = K[3431];
      at = 12299; l8939 = l8941;
    }
    at = 12300; l8942 = K[3432];
    at = 12301; l8943 = rt.int_div(l8939, l8942);
    at = 12302; l8944 = K[3433];
    at = 12303; l8945 = rt.max(l8944, l8943);
    at = 12304; l8946 = K[3434];
    at = 12305; l8947 = rt.gt(l8929, l8946);
    at = 12306; if (rt.truthy(l8947)) {
      at = 12307; if (rt.truthy(l8922)) {
        at = 12308; l8950 = field(rt, l8925, "color");
        at = 12309; l8949 = l8950;
      } else {
        at = 12310; l8951 = K[3435];
        at = 12311; l8949 = l8951;
      }
      at = 12312; l8952 = K[3436];
      at = 12313; l8953 = rt.sequence_get(l8952, l8949);
      at = 12314; l8948 = l8953;
    } else {
      at = 12315; l8954 = K[3437];
      at = 12316; l8948 = l8954;
    }
    at = 12317; markSet(rt, false, "circle", {at: l8932, radius: l8945, fill: l8948});
    at = 12318; if (rt.truthy(l8922)) {
      at = 12319; l8956 = field(rt, l8925, "points");
      at = 12320; l8955 = l8956;
    } else {
      at = 12321; l8957 = K[3438];
      at = 12322; l8955 = l8957;
    }
    at = 12323; l8958 = K[3439];
    at = 12324; l8959 = rt.sub(l8929, l8958);
    at = 12325; l8960 = K[3440];
    at = 12326; l8961 = rt.max(l8960, l8959);
    at = 12327; l8962 = K[3441];
    at = 12328; l8963 = rt.array_slice(l8955, l8962, l8961);
    at = 12329; if (rt.truthy(l8922)) {
      at = 12330; l8965 = field(rt, l8925, "points");
      at = 12331; l8964 = l8965;
    } else {
      at = 12332; l8966 = K[3442];
      at = 12333; l8964 = l8966;
    }
    at = 12334; l8967 = K[3443];
    at = 12335; l8968 = rt.gt(l8929, l8967);
    at = 12336; if (rt.truthy(l8968)) {
      at = 12337; l8970 = K[3444];
      at = 12338; l8969 = l8970;
    } else {
      at = 12339; l8971 = K[3445];
      at = 12340; l8969 = l8971;
    }
    at = 12341; l8972 = K[3446];
    at = 12342; l8973 = rt.sub(l8929, l8972);
    at = 12343; l8974 = K[3447];
    at = 12344; l8975 = rt.max(l8974, l8973);
    at = 12345; l8976 = rt.array_slice(l8964, l8969, l8975);
    at = 12346; if (rt.truthy(l8922)) {
      at = 12347; l8978 = field(rt, l8925, "color");
      at = 12348; l8977 = l8978;
    } else {
      at = 12349; l8979 = K[3448];
      at = 12350; l8977 = l8979;
    }
    at = 12351; l8980 = K[3449];
    at = 12352; l8981 = rt.sequence_get(l8980, l8977);
    at = 12353; if (rt.truthy(l8922)) {
      at = 12354; l8983 = field(rt, l8925, "width");
      at = 12355; l8982 = l8983;
    } else {
      at = 12356; l8984 = K[3450];
      at = 12357; l8982 = l8984;
    }
    at = 12358; markSet(rt, true, "line", {from: l8963, to: l8976, color: l8981, width: l8982});
    at = 12359; l8985 = field(rt, l15, "strokes");
    at = 12360; l8986 = rt.sequence_length(l8985);
    at = 12361; l8987 = K[3451];
    at = 12362; l8988 = rt.lt(l8987, l8986);
    at = 12363; if (rt.truthy(l8988)) {
      at = 12364; l8989 = field(rt, l15, "strokes");
      at = 12365; l8990 = K[3452];
      at = 12366; l8991 = rt.sequence_get(l8989, l8990);
    }
    at = 12367; if (rt.truthy(l8988)) {
      at = 12368; l8993 = field(rt, l8991, "points");
      at = 12369; l8992 = l8993;
    } else {
      at = 12370; l8994 = K[3453];
      at = 12371; l8992 = l8994;
    }
    at = 12372; l8995 = rt.array_count(l8992);
    at = 12373; l8996 = K[3454];
    at = 12374; l8997 = rt.gt(l8995, l8996);
    at = 12375; if (rt.truthy(l8997)) {
      at = 12376; if (rt.truthy(l8988)) {
        at = 12377; l9000 = field(rt, l8991, "points");
        at = 12378; l8999 = l9000;
      } else {
        at = 12379; l9001 = K[3455];
        at = 12380; l8999 = l9001;
      }
      at = 12381; l9002 = K[3456];
      at = 12382; l9003 = rt.array_nth(l8999, l9002);
      at = 12383; l8998 = l9003;
    } else {
      at = 12384; l9004 = K[3457];
      at = 12385; l8998 = l9004;
    }
    at = 12386; if (rt.truthy(l8988)) {
      at = 12387; l9006 = field(rt, l8991, "width");
      at = 12388; l9005 = l9006;
    } else {
      at = 12389; l9007 = K[3458];
      at = 12390; l9005 = l9007;
    }
    at = 12391; l9008 = K[3459];
    at = 12392; l9009 = rt.int_div(l9005, l9008);
    at = 12393; l9010 = K[3460];
    at = 12394; l9011 = rt.max(l9010, l9009);
    at = 12395; l9012 = K[3461];
    at = 12396; l9013 = rt.gt(l8995, l9012);
    at = 12397; if (rt.truthy(l9013)) {
      at = 12398; if (rt.truthy(l8988)) {
        at = 12399; l9016 = field(rt, l8991, "color");
        at = 12400; l9015 = l9016;
      } else {
        at = 12401; l9017 = K[3462];
        at = 12402; l9015 = l9017;
      }
      at = 12403; l9018 = K[3463];
      at = 12404; l9019 = rt.sequence_get(l9018, l9015);
      at = 12405; l9014 = l9019;
    } else {
      at = 12406; l9020 = K[3464];
      at = 12407; l9014 = l9020;
    }
    at = 12408; markSet(rt, false, "circle", {at: l8998, radius: l9011, fill: l9014});
    at = 12409; if (rt.truthy(l8988)) {
      at = 12410; l9022 = field(rt, l8991, "points");
      at = 12411; l9021 = l9022;
    } else {
      at = 12412; l9023 = K[3465];
      at = 12413; l9021 = l9023;
    }
    at = 12414; l9024 = K[3466];
    at = 12415; l9025 = rt.sub(l8995, l9024);
    at = 12416; l9026 = K[3467];
    at = 12417; l9027 = rt.max(l9026, l9025);
    at = 12418; l9028 = K[3468];
    at = 12419; l9029 = rt.array_slice(l9021, l9028, l9027);
    at = 12420; if (rt.truthy(l8988)) {
      at = 12421; l9031 = field(rt, l8991, "points");
      at = 12422; l9030 = l9031;
    } else {
      at = 12423; l9032 = K[3469];
      at = 12424; l9030 = l9032;
    }
    at = 12425; l9033 = K[3470];
    at = 12426; l9034 = rt.gt(l8995, l9033);
    at = 12427; if (rt.truthy(l9034)) {
      at = 12428; l9036 = K[3471];
      at = 12429; l9035 = l9036;
    } else {
      at = 12430; l9037 = K[3472];
      at = 12431; l9035 = l9037;
    }
    at = 12432; l9038 = K[3473];
    at = 12433; l9039 = rt.sub(l8995, l9038);
    at = 12434; l9040 = K[3474];
    at = 12435; l9041 = rt.max(l9040, l9039);
    at = 12436; l9042 = rt.array_slice(l9030, l9035, l9041);
    at = 12437; if (rt.truthy(l8988)) {
      at = 12438; l9044 = field(rt, l8991, "color");
      at = 12439; l9043 = l9044;
    } else {
      at = 12440; l9045 = K[3475];
      at = 12441; l9043 = l9045;
    }
    at = 12442; l9046 = K[3476];
    at = 12443; l9047 = rt.sequence_get(l9046, l9043);
    at = 12444; if (rt.truthy(l8988)) {
      at = 12445; l9049 = field(rt, l8991, "width");
      at = 12446; l9048 = l9049;
    } else {
      at = 12447; l9050 = K[3477];
      at = 12448; l9048 = l9050;
    }
    at = 12449; markSet(rt, true, "line", {from: l9029, to: l9042, color: l9047, width: l9048});
    at = 12450; l9051 = field(rt, l15, "strokes");
    at = 12451; l9052 = rt.sequence_length(l9051);
    at = 12452; l9053 = K[3478];
    at = 12453; l9054 = rt.lt(l9053, l9052);
    at = 12454; if (rt.truthy(l9054)) {
      at = 12455; l9055 = field(rt, l15, "strokes");
      at = 12456; l9056 = K[3479];
      at = 12457; l9057 = rt.sequence_get(l9055, l9056);
    }
    at = 12458; if (rt.truthy(l9054)) {
      at = 12459; l9059 = field(rt, l9057, "points");
      at = 12460; l9058 = l9059;
    } else {
      at = 12461; l9060 = K[3480];
      at = 12462; l9058 = l9060;
    }
    at = 12463; l9061 = rt.array_count(l9058);
    at = 12464; l9062 = K[3481];
    at = 12465; l9063 = rt.gt(l9061, l9062);
    at = 12466; if (rt.truthy(l9063)) {
      at = 12467; if (rt.truthy(l9054)) {
        at = 12468; l9066 = field(rt, l9057, "points");
        at = 12469; l9065 = l9066;
      } else {
        at = 12470; l9067 = K[3482];
        at = 12471; l9065 = l9067;
      }
      at = 12472; l9068 = K[3483];
      at = 12473; l9069 = rt.array_nth(l9065, l9068);
      at = 12474; l9064 = l9069;
    } else {
      at = 12475; l9070 = K[3484];
      at = 12476; l9064 = l9070;
    }
    at = 12477; if (rt.truthy(l9054)) {
      at = 12478; l9072 = field(rt, l9057, "width");
      at = 12479; l9071 = l9072;
    } else {
      at = 12480; l9073 = K[3485];
      at = 12481; l9071 = l9073;
    }
    at = 12482; l9074 = K[3486];
    at = 12483; l9075 = rt.int_div(l9071, l9074);
    at = 12484; l9076 = K[3487];
    at = 12485; l9077 = rt.max(l9076, l9075);
    at = 12486; l9078 = K[3488];
    at = 12487; l9079 = rt.gt(l9061, l9078);
    at = 12488; if (rt.truthy(l9079)) {
      at = 12489; if (rt.truthy(l9054)) {
        at = 12490; l9082 = field(rt, l9057, "color");
        at = 12491; l9081 = l9082;
      } else {
        at = 12492; l9083 = K[3489];
        at = 12493; l9081 = l9083;
      }
      at = 12494; l9084 = K[3490];
      at = 12495; l9085 = rt.sequence_get(l9084, l9081);
      at = 12496; l9080 = l9085;
    } else {
      at = 12497; l9086 = K[3491];
      at = 12498; l9080 = l9086;
    }
    at = 12499; markSet(rt, false, "circle", {at: l9064, radius: l9077, fill: l9080});
    at = 12500; if (rt.truthy(l9054)) {
      at = 12501; l9088 = field(rt, l9057, "points");
      at = 12502; l9087 = l9088;
    } else {
      at = 12503; l9089 = K[3492];
      at = 12504; l9087 = l9089;
    }
    at = 12505; l9090 = K[3493];
    at = 12506; l9091 = rt.sub(l9061, l9090);
    at = 12507; l9092 = K[3494];
    at = 12508; l9093 = rt.max(l9092, l9091);
    at = 12509; l9094 = K[3495];
    at = 12510; l9095 = rt.array_slice(l9087, l9094, l9093);
    at = 12511; if (rt.truthy(l9054)) {
      at = 12512; l9097 = field(rt, l9057, "points");
      at = 12513; l9096 = l9097;
    } else {
      at = 12514; l9098 = K[3496];
      at = 12515; l9096 = l9098;
    }
    at = 12516; l9099 = K[3497];
    at = 12517; l9100 = rt.gt(l9061, l9099);
    at = 12518; if (rt.truthy(l9100)) {
      at = 12519; l9102 = K[3498];
      at = 12520; l9101 = l9102;
    } else {
      at = 12521; l9103 = K[3499];
      at = 12522; l9101 = l9103;
    }
    at = 12523; l9104 = K[3500];
    at = 12524; l9105 = rt.sub(l9061, l9104);
    at = 12525; l9106 = K[3501];
    at = 12526; l9107 = rt.max(l9106, l9105);
    at = 12527; l9108 = rt.array_slice(l9096, l9101, l9107);
    at = 12528; if (rt.truthy(l9054)) {
      at = 12529; l9110 = field(rt, l9057, "color");
      at = 12530; l9109 = l9110;
    } else {
      at = 12531; l9111 = K[3502];
      at = 12532; l9109 = l9111;
    }
    at = 12533; l9112 = K[3503];
    at = 12534; l9113 = rt.sequence_get(l9112, l9109);
    at = 12535; if (rt.truthy(l9054)) {
      at = 12536; l9115 = field(rt, l9057, "width");
      at = 12537; l9114 = l9115;
    } else {
      at = 12538; l9116 = K[3504];
      at = 12539; l9114 = l9116;
    }
    at = 12540; markSet(rt, true, "line", {from: l9095, to: l9108, color: l9113, width: l9114});
    at = 12541; l9117 = field(rt, l15, "strokes");
    at = 12542; l9118 = rt.sequence_length(l9117);
    at = 12543; l9119 = K[3505];
    at = 12544; l9120 = rt.lt(l9119, l9118);
    at = 12545; if (rt.truthy(l9120)) {
      at = 12546; l9121 = field(rt, l15, "strokes");
      at = 12547; l9122 = K[3506];
      at = 12548; l9123 = rt.sequence_get(l9121, l9122);
    }
    at = 12549; if (rt.truthy(l9120)) {
      at = 12550; l9125 = field(rt, l9123, "points");
      at = 12551; l9124 = l9125;
    } else {
      at = 12552; l9126 = K[3507];
      at = 12553; l9124 = l9126;
    }
    at = 12554; l9127 = rt.array_count(l9124);
    at = 12555; l9128 = K[3508];
    at = 12556; l9129 = rt.gt(l9127, l9128);
    at = 12557; if (rt.truthy(l9129)) {
      at = 12558; if (rt.truthy(l9120)) {
        at = 12559; l9132 = field(rt, l9123, "points");
        at = 12560; l9131 = l9132;
      } else {
        at = 12561; l9133 = K[3509];
        at = 12562; l9131 = l9133;
      }
      at = 12563; l9134 = K[3510];
      at = 12564; l9135 = rt.array_nth(l9131, l9134);
      at = 12565; l9130 = l9135;
    } else {
      at = 12566; l9136 = K[3511];
      at = 12567; l9130 = l9136;
    }
    at = 12568; if (rt.truthy(l9120)) {
      at = 12569; l9138 = field(rt, l9123, "width");
      at = 12570; l9137 = l9138;
    } else {
      at = 12571; l9139 = K[3512];
      at = 12572; l9137 = l9139;
    }
    at = 12573; l9140 = K[3513];
    at = 12574; l9141 = rt.int_div(l9137, l9140);
    at = 12575; l9142 = K[3514];
    at = 12576; l9143 = rt.max(l9142, l9141);
    at = 12577; l9144 = K[3515];
    at = 12578; l9145 = rt.gt(l9127, l9144);
    at = 12579; if (rt.truthy(l9145)) {
      at = 12580; if (rt.truthy(l9120)) {
        at = 12581; l9148 = field(rt, l9123, "color");
        at = 12582; l9147 = l9148;
      } else {
        at = 12583; l9149 = K[3516];
        at = 12584; l9147 = l9149;
      }
      at = 12585; l9150 = K[3517];
      at = 12586; l9151 = rt.sequence_get(l9150, l9147);
      at = 12587; l9146 = l9151;
    } else {
      at = 12588; l9152 = K[3518];
      at = 12589; l9146 = l9152;
    }
    at = 12590; markSet(rt, false, "circle", {at: l9130, radius: l9143, fill: l9146});
    at = 12591; if (rt.truthy(l9120)) {
      at = 12592; l9154 = field(rt, l9123, "points");
      at = 12593; l9153 = l9154;
    } else {
      at = 12594; l9155 = K[3519];
      at = 12595; l9153 = l9155;
    }
    at = 12596; l9156 = K[3520];
    at = 12597; l9157 = rt.sub(l9127, l9156);
    at = 12598; l9158 = K[3521];
    at = 12599; l9159 = rt.max(l9158, l9157);
    at = 12600; l9160 = K[3522];
    at = 12601; l9161 = rt.array_slice(l9153, l9160, l9159);
    at = 12602; if (rt.truthy(l9120)) {
      at = 12603; l9163 = field(rt, l9123, "points");
      at = 12604; l9162 = l9163;
    } else {
      at = 12605; l9164 = K[3523];
      at = 12606; l9162 = l9164;
    }
    at = 12607; l9165 = K[3524];
    at = 12608; l9166 = rt.gt(l9127, l9165);
    at = 12609; if (rt.truthy(l9166)) {
      at = 12610; l9168 = K[3525];
      at = 12611; l9167 = l9168;
    } else {
      at = 12612; l9169 = K[3526];
      at = 12613; l9167 = l9169;
    }
    at = 12614; l9170 = K[3527];
    at = 12615; l9171 = rt.sub(l9127, l9170);
    at = 12616; l9172 = K[3528];
    at = 12617; l9173 = rt.max(l9172, l9171);
    at = 12618; l9174 = rt.array_slice(l9162, l9167, l9173);
    at = 12619; if (rt.truthy(l9120)) {
      at = 12620; l9176 = field(rt, l9123, "color");
      at = 12621; l9175 = l9176;
    } else {
      at = 12622; l9177 = K[3529];
      at = 12623; l9175 = l9177;
    }
    at = 12624; l9178 = K[3530];
    at = 12625; l9179 = rt.sequence_get(l9178, l9175);
    at = 12626; if (rt.truthy(l9120)) {
      at = 12627; l9181 = field(rt, l9123, "width");
      at = 12628; l9180 = l9181;
    } else {
      at = 12629; l9182 = K[3531];
      at = 12630; l9180 = l9182;
    }
    at = 12631; markSet(rt, true, "line", {from: l9161, to: l9174, color: l9179, width: l9180});
    at = 12632; l9183 = field(rt, l15, "strokes");
    at = 12633; l9184 = rt.sequence_length(l9183);
    at = 12634; l9185 = K[3532];
    at = 12635; l9186 = rt.lt(l9185, l9184);
    at = 12636; if (rt.truthy(l9186)) {
      at = 12637; l9187 = field(rt, l15, "strokes");
      at = 12638; l9188 = K[3533];
      at = 12639; l9189 = rt.sequence_get(l9187, l9188);
    }
    at = 12640; if (rt.truthy(l9186)) {
      at = 12641; l9191 = field(rt, l9189, "points");
      at = 12642; l9190 = l9191;
    } else {
      at = 12643; l9192 = K[3534];
      at = 12644; l9190 = l9192;
    }
    at = 12645; l9193 = rt.array_count(l9190);
    at = 12646; l9194 = K[3535];
    at = 12647; l9195 = rt.gt(l9193, l9194);
    at = 12648; if (rt.truthy(l9195)) {
      at = 12649; if (rt.truthy(l9186)) {
        at = 12650; l9198 = field(rt, l9189, "points");
        at = 12651; l9197 = l9198;
      } else {
        at = 12652; l9199 = K[3536];
        at = 12653; l9197 = l9199;
      }
      at = 12654; l9200 = K[3537];
      at = 12655; l9201 = rt.array_nth(l9197, l9200);
      at = 12656; l9196 = l9201;
    } else {
      at = 12657; l9202 = K[3538];
      at = 12658; l9196 = l9202;
    }
    at = 12659; if (rt.truthy(l9186)) {
      at = 12660; l9204 = field(rt, l9189, "width");
      at = 12661; l9203 = l9204;
    } else {
      at = 12662; l9205 = K[3539];
      at = 12663; l9203 = l9205;
    }
    at = 12664; l9206 = K[3540];
    at = 12665; l9207 = rt.int_div(l9203, l9206);
    at = 12666; l9208 = K[3541];
    at = 12667; l9209 = rt.max(l9208, l9207);
    at = 12668; l9210 = K[3542];
    at = 12669; l9211 = rt.gt(l9193, l9210);
    at = 12670; if (rt.truthy(l9211)) {
      at = 12671; if (rt.truthy(l9186)) {
        at = 12672; l9214 = field(rt, l9189, "color");
        at = 12673; l9213 = l9214;
      } else {
        at = 12674; l9215 = K[3543];
        at = 12675; l9213 = l9215;
      }
      at = 12676; l9216 = K[3544];
      at = 12677; l9217 = rt.sequence_get(l9216, l9213);
      at = 12678; l9212 = l9217;
    } else {
      at = 12679; l9218 = K[3545];
      at = 12680; l9212 = l9218;
    }
    at = 12681; markSet(rt, false, "circle", {at: l9196, radius: l9209, fill: l9212});
    at = 12682; if (rt.truthy(l9186)) {
      at = 12683; l9220 = field(rt, l9189, "points");
      at = 12684; l9219 = l9220;
    } else {
      at = 12685; l9221 = K[3546];
      at = 12686; l9219 = l9221;
    }
    at = 12687; l9222 = K[3547];
    at = 12688; l9223 = rt.sub(l9193, l9222);
    at = 12689; l9224 = K[3548];
    at = 12690; l9225 = rt.max(l9224, l9223);
    at = 12691; l9226 = K[3549];
    at = 12692; l9227 = rt.array_slice(l9219, l9226, l9225);
    at = 12693; if (rt.truthy(l9186)) {
      at = 12694; l9229 = field(rt, l9189, "points");
      at = 12695; l9228 = l9229;
    } else {
      at = 12696; l9230 = K[3550];
      at = 12697; l9228 = l9230;
    }
    at = 12698; l9231 = K[3551];
    at = 12699; l9232 = rt.gt(l9193, l9231);
    at = 12700; if (rt.truthy(l9232)) {
      at = 12701; l9234 = K[3552];
      at = 12702; l9233 = l9234;
    } else {
      at = 12703; l9235 = K[3553];
      at = 12704; l9233 = l9235;
    }
    at = 12705; l9236 = K[3554];
    at = 12706; l9237 = rt.sub(l9193, l9236);
    at = 12707; l9238 = K[3555];
    at = 12708; l9239 = rt.max(l9238, l9237);
    at = 12709; l9240 = rt.array_slice(l9228, l9233, l9239);
    at = 12710; if (rt.truthy(l9186)) {
      at = 12711; l9242 = field(rt, l9189, "color");
      at = 12712; l9241 = l9242;
    } else {
      at = 12713; l9243 = K[3556];
      at = 12714; l9241 = l9243;
    }
    at = 12715; l9244 = K[3557];
    at = 12716; l9245 = rt.sequence_get(l9244, l9241);
    at = 12717; if (rt.truthy(l9186)) {
      at = 12718; l9247 = field(rt, l9189, "width");
      at = 12719; l9246 = l9247;
    } else {
      at = 12720; l9248 = K[3558];
      at = 12721; l9246 = l9248;
    }
    at = 12722; markSet(rt, true, "line", {from: l9227, to: l9240, color: l9245, width: l9246});
    at = 12723; l9249 = field(rt, l15, "strokes");
    at = 12724; l9250 = rt.sequence_length(l9249);
    at = 12725; l9251 = K[3559];
    at = 12726; l9252 = rt.lt(l9251, l9250);
    at = 12727; if (rt.truthy(l9252)) {
      at = 12728; l9253 = field(rt, l15, "strokes");
      at = 12729; l9254 = K[3560];
      at = 12730; l9255 = rt.sequence_get(l9253, l9254);
    }
    at = 12731; if (rt.truthy(l9252)) {
      at = 12732; l9257 = field(rt, l9255, "points");
      at = 12733; l9256 = l9257;
    } else {
      at = 12734; l9258 = K[3561];
      at = 12735; l9256 = l9258;
    }
    at = 12736; l9259 = rt.array_count(l9256);
    at = 12737; l9260 = K[3562];
    at = 12738; l9261 = rt.gt(l9259, l9260);
    at = 12739; if (rt.truthy(l9261)) {
      at = 12740; if (rt.truthy(l9252)) {
        at = 12741; l9264 = field(rt, l9255, "points");
        at = 12742; l9263 = l9264;
      } else {
        at = 12743; l9265 = K[3563];
        at = 12744; l9263 = l9265;
      }
      at = 12745; l9266 = K[3564];
      at = 12746; l9267 = rt.array_nth(l9263, l9266);
      at = 12747; l9262 = l9267;
    } else {
      at = 12748; l9268 = K[3565];
      at = 12749; l9262 = l9268;
    }
    at = 12750; if (rt.truthy(l9252)) {
      at = 12751; l9270 = field(rt, l9255, "width");
      at = 12752; l9269 = l9270;
    } else {
      at = 12753; l9271 = K[3566];
      at = 12754; l9269 = l9271;
    }
    at = 12755; l9272 = K[3567];
    at = 12756; l9273 = rt.int_div(l9269, l9272);
    at = 12757; l9274 = K[3568];
    at = 12758; l9275 = rt.max(l9274, l9273);
    at = 12759; l9276 = K[3569];
    at = 12760; l9277 = rt.gt(l9259, l9276);
    at = 12761; if (rt.truthy(l9277)) {
      at = 12762; if (rt.truthy(l9252)) {
        at = 12763; l9280 = field(rt, l9255, "color");
        at = 12764; l9279 = l9280;
      } else {
        at = 12765; l9281 = K[3570];
        at = 12766; l9279 = l9281;
      }
      at = 12767; l9282 = K[3571];
      at = 12768; l9283 = rt.sequence_get(l9282, l9279);
      at = 12769; l9278 = l9283;
    } else {
      at = 12770; l9284 = K[3572];
      at = 12771; l9278 = l9284;
    }
    at = 12772; markSet(rt, false, "circle", {at: l9262, radius: l9275, fill: l9278});
    at = 12773; if (rt.truthy(l9252)) {
      at = 12774; l9286 = field(rt, l9255, "points");
      at = 12775; l9285 = l9286;
    } else {
      at = 12776; l9287 = K[3573];
      at = 12777; l9285 = l9287;
    }
    at = 12778; l9288 = K[3574];
    at = 12779; l9289 = rt.sub(l9259, l9288);
    at = 12780; l9290 = K[3575];
    at = 12781; l9291 = rt.max(l9290, l9289);
    at = 12782; l9292 = K[3576];
    at = 12783; l9293 = rt.array_slice(l9285, l9292, l9291);
    at = 12784; if (rt.truthy(l9252)) {
      at = 12785; l9295 = field(rt, l9255, "points");
      at = 12786; l9294 = l9295;
    } else {
      at = 12787; l9296 = K[3577];
      at = 12788; l9294 = l9296;
    }
    at = 12789; l9297 = K[3578];
    at = 12790; l9298 = rt.gt(l9259, l9297);
    at = 12791; if (rt.truthy(l9298)) {
      at = 12792; l9300 = K[3579];
      at = 12793; l9299 = l9300;
    } else {
      at = 12794; l9301 = K[3580];
      at = 12795; l9299 = l9301;
    }
    at = 12796; l9302 = K[3581];
    at = 12797; l9303 = rt.sub(l9259, l9302);
    at = 12798; l9304 = K[3582];
    at = 12799; l9305 = rt.max(l9304, l9303);
    at = 12800; l9306 = rt.array_slice(l9294, l9299, l9305);
    at = 12801; if (rt.truthy(l9252)) {
      at = 12802; l9308 = field(rt, l9255, "color");
      at = 12803; l9307 = l9308;
    } else {
      at = 12804; l9309 = K[3583];
      at = 12805; l9307 = l9309;
    }
    at = 12806; l9310 = K[3584];
    at = 12807; l9311 = rt.sequence_get(l9310, l9307);
    at = 12808; if (rt.truthy(l9252)) {
      at = 12809; l9313 = field(rt, l9255, "width");
      at = 12810; l9312 = l9313;
    } else {
      at = 12811; l9314 = K[3585];
      at = 12812; l9312 = l9314;
    }
    at = 12813; markSet(rt, true, "line", {from: l9293, to: l9306, color: l9311, width: l9312});
    at = 12814; l9315 = field(rt, l15, "strokes");
    at = 12815; l9316 = rt.sequence_length(l9315);
    at = 12816; l9317 = K[3586];
    at = 12817; l9318 = rt.lt(l9317, l9316);
    at = 12818; if (rt.truthy(l9318)) {
      at = 12819; l9319 = field(rt, l15, "strokes");
      at = 12820; l9320 = K[3587];
      at = 12821; l9321 = rt.sequence_get(l9319, l9320);
    }
    at = 12822; if (rt.truthy(l9318)) {
      at = 12823; l9323 = field(rt, l9321, "points");
      at = 12824; l9322 = l9323;
    } else {
      at = 12825; l9324 = K[3588];
      at = 12826; l9322 = l9324;
    }
    at = 12827; l9325 = rt.array_count(l9322);
    at = 12828; l9326 = K[3589];
    at = 12829; l9327 = rt.gt(l9325, l9326);
    at = 12830; if (rt.truthy(l9327)) {
      at = 12831; if (rt.truthy(l9318)) {
        at = 12832; l9330 = field(rt, l9321, "points");
        at = 12833; l9329 = l9330;
      } else {
        at = 12834; l9331 = K[3590];
        at = 12835; l9329 = l9331;
      }
      at = 12836; l9332 = K[3591];
      at = 12837; l9333 = rt.array_nth(l9329, l9332);
      at = 12838; l9328 = l9333;
    } else {
      at = 12839; l9334 = K[3592];
      at = 12840; l9328 = l9334;
    }
    at = 12841; if (rt.truthy(l9318)) {
      at = 12842; l9336 = field(rt, l9321, "width");
      at = 12843; l9335 = l9336;
    } else {
      at = 12844; l9337 = K[3593];
      at = 12845; l9335 = l9337;
    }
    at = 12846; l9338 = K[3594];
    at = 12847; l9339 = rt.int_div(l9335, l9338);
    at = 12848; l9340 = K[3595];
    at = 12849; l9341 = rt.max(l9340, l9339);
    at = 12850; l9342 = K[3596];
    at = 12851; l9343 = rt.gt(l9325, l9342);
    at = 12852; if (rt.truthy(l9343)) {
      at = 12853; if (rt.truthy(l9318)) {
        at = 12854; l9346 = field(rt, l9321, "color");
        at = 12855; l9345 = l9346;
      } else {
        at = 12856; l9347 = K[3597];
        at = 12857; l9345 = l9347;
      }
      at = 12858; l9348 = K[3598];
      at = 12859; l9349 = rt.sequence_get(l9348, l9345);
      at = 12860; l9344 = l9349;
    } else {
      at = 12861; l9350 = K[3599];
      at = 12862; l9344 = l9350;
    }
    at = 12863; markSet(rt, false, "circle", {at: l9328, radius: l9341, fill: l9344});
    at = 12864; if (rt.truthy(l9318)) {
      at = 12865; l9352 = field(rt, l9321, "points");
      at = 12866; l9351 = l9352;
    } else {
      at = 12867; l9353 = K[3600];
      at = 12868; l9351 = l9353;
    }
    at = 12869; l9354 = K[3601];
    at = 12870; l9355 = rt.sub(l9325, l9354);
    at = 12871; l9356 = K[3602];
    at = 12872; l9357 = rt.max(l9356, l9355);
    at = 12873; l9358 = K[3603];
    at = 12874; l9359 = rt.array_slice(l9351, l9358, l9357);
    at = 12875; if (rt.truthy(l9318)) {
      at = 12876; l9361 = field(rt, l9321, "points");
      at = 12877; l9360 = l9361;
    } else {
      at = 12878; l9362 = K[3604];
      at = 12879; l9360 = l9362;
    }
    at = 12880; l9363 = K[3605];
    at = 12881; l9364 = rt.gt(l9325, l9363);
    at = 12882; if (rt.truthy(l9364)) {
      at = 12883; l9366 = K[3606];
      at = 12884; l9365 = l9366;
    } else {
      at = 12885; l9367 = K[3607];
      at = 12886; l9365 = l9367;
    }
    at = 12887; l9368 = K[3608];
    at = 12888; l9369 = rt.sub(l9325, l9368);
    at = 12889; l9370 = K[3609];
    at = 12890; l9371 = rt.max(l9370, l9369);
    at = 12891; l9372 = rt.array_slice(l9360, l9365, l9371);
    at = 12892; if (rt.truthy(l9318)) {
      at = 12893; l9374 = field(rt, l9321, "color");
      at = 12894; l9373 = l9374;
    } else {
      at = 12895; l9375 = K[3610];
      at = 12896; l9373 = l9375;
    }
    at = 12897; l9376 = K[3611];
    at = 12898; l9377 = rt.sequence_get(l9376, l9373);
    at = 12899; if (rt.truthy(l9318)) {
      at = 12900; l9379 = field(rt, l9321, "width");
      at = 12901; l9378 = l9379;
    } else {
      at = 12902; l9380 = K[3612];
      at = 12903; l9378 = l9380;
    }
    at = 12904; markSet(rt, true, "line", {from: l9359, to: l9372, color: l9377, width: l9378});
    at = 12905; l9381 = field(rt, l15, "active");
    at = 12906; if (rt.truthy(l9381)) {
      at = 12907; l9382 = field(rt, l15, "current");
    }
    at = 12908; if (rt.truthy(l9381)) {
      at = 12909; l9384 = field(rt, l9382, "points");
      at = 12910; l9383 = l9384;
    } else {
      at = 12911; l9385 = K[3613];
      at = 12912; l9383 = l9385;
    }
    at = 12913; l9386 = rt.array_count(l9383);
    at = 12914; l9387 = K[3614];
    at = 12915; l9388 = rt.gt(l9386, l9387);
    at = 12916; if (rt.truthy(l9388)) {
      at = 12917; if (rt.truthy(l9381)) {
        at = 12918; l9391 = field(rt, l9382, "points");
        at = 12919; l9390 = l9391;
      } else {
        at = 12920; l9392 = K[3615];
        at = 12921; l9390 = l9392;
      }
      at = 12922; l9393 = K[3616];
      at = 12923; l9394 = rt.array_nth(l9390, l9393);
      at = 12924; l9389 = l9394;
    } else {
      at = 12925; l9395 = K[3617];
      at = 12926; l9389 = l9395;
    }
    at = 12927; if (rt.truthy(l9381)) {
      at = 12928; l9397 = field(rt, l9382, "width");
      at = 12929; l9396 = l9397;
    } else {
      at = 12930; l9398 = K[3618];
      at = 12931; l9396 = l9398;
    }
    at = 12932; l9399 = K[3619];
    at = 12933; l9400 = rt.int_div(l9396, l9399);
    at = 12934; l9401 = K[3620];
    at = 12935; l9402 = rt.max(l9401, l9400);
    at = 12936; l9403 = K[3621];
    at = 12937; l9404 = rt.gt(l9386, l9403);
    at = 12938; if (rt.truthy(l9404)) {
      at = 12939; if (rt.truthy(l9381)) {
        at = 12940; l9407 = field(rt, l9382, "color");
        at = 12941; l9406 = l9407;
      } else {
        at = 12942; l9408 = K[3622];
        at = 12943; l9406 = l9408;
      }
      at = 12944; l9409 = K[3623];
      at = 12945; l9410 = rt.sequence_get(l9409, l9406);
      at = 12946; l9405 = l9410;
    } else {
      at = 12947; l9411 = K[3624];
      at = 12948; l9405 = l9411;
    }
    at = 12949; markSet(rt, false, "circle", {at: l9389, radius: l9402, fill: l9405});
    at = 12950; if (rt.truthy(l9381)) {
      at = 12951; l9413 = field(rt, l9382, "points");
      at = 12952; l9412 = l9413;
    } else {
      at = 12953; l9414 = K[3625];
      at = 12954; l9412 = l9414;
    }
    at = 12955; l9415 = K[3626];
    at = 12956; l9416 = rt.sub(l9386, l9415);
    at = 12957; l9417 = K[3627];
    at = 12958; l9418 = rt.max(l9417, l9416);
    at = 12959; l9419 = K[3628];
    at = 12960; l9420 = rt.array_slice(l9412, l9419, l9418);
    at = 12961; if (rt.truthy(l9381)) {
      at = 12962; l9422 = field(rt, l9382, "points");
      at = 12963; l9421 = l9422;
    } else {
      at = 12964; l9423 = K[3629];
      at = 12965; l9421 = l9423;
    }
    at = 12966; l9424 = K[3630];
    at = 12967; l9425 = rt.gt(l9386, l9424);
    at = 12968; if (rt.truthy(l9425)) {
      at = 12969; l9427 = K[3631];
      at = 12970; l9426 = l9427;
    } else {
      at = 12971; l9428 = K[3632];
      at = 12972; l9426 = l9428;
    }
    at = 12973; l9429 = K[3633];
    at = 12974; l9430 = rt.sub(l9386, l9429);
    at = 12975; l9431 = K[3634];
    at = 12976; l9432 = rt.max(l9431, l9430);
    at = 12977; l9433 = rt.array_slice(l9421, l9426, l9432);
    at = 12978; if (rt.truthy(l9381)) {
      at = 12979; l9435 = field(rt, l9382, "color");
      at = 12980; l9434 = l9435;
    } else {
      at = 12981; l9436 = K[3635];
      at = 12982; l9434 = l9436;
    }
    at = 12983; l9437 = K[3636];
    at = 12984; l9438 = rt.sequence_get(l9437, l9434);
    at = 12985; if (rt.truthy(l9381)) {
      at = 12986; l9440 = field(rt, l9382, "width");
      at = 12987; l9439 = l9440;
    } else {
      at = 12988; l9441 = K[3637];
      at = 12989; l9439 = l9441;
    }
    at = 12990; markSet(rt, true, "line", {from: l9420, to: l9433, color: l9438, width: l9439});
    at = 12991; l9442 = K[3638];
    at = 12992; l9443 = BigInt(input.size[0]);
    at = 12993; l9444 = K[3639];
    at = 12994; l9445 = K[3640];
    at = 12995; l9446 = Float64Array.of(rt.float(l9443), rt.float(l9444), rt.float(l9445));
    at = 12996; l9447 = K[3641];
    at = 12997; markSet(rt, false, "rect", {at: l9442, size: l9446, fill: l9447});
    at = 12998; l9448 = K[3642];
    at = 12999; l9449 = K[3643];
    at = 13000; l9450 = K[3644];
    at = 13001; l9451 = K[3645];
    at = 13002; l9452 = K[3646];
    at = 13003; pathOf(rt, "rounded_rect", {at: l9448, size: l9449, radius: l9450, fill: l9451, stroke: l9452});
    at = 13004; l9453 = K[3647];
    at = 13005; l9454 = K[3648];
    at = 13006; l9455 = K[3649];
    at = 13007; l9456 = K[3650];
    at = 13008; if (typeof l9454 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9454 !== "") { const [x, y] = point(rt, l9453); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9455)), color: ink(rt, l9456), text: l9454}); }
    at = 13009; l9457 = K[3651];
    at = 13010; l9458 = K[3652];
    at = 13011; l9459 = K[3653];
    at = 13012; l9460 = K[3654];
    at = 13013; l9461 = K[3655];
    at = 13014; pathOf(rt, "rounded_rect", {at: l9457, size: l9458, radius: l9459, fill: l9460, stroke: l9461});
    at = 13015; l9462 = K[3656];
    at = 13016; l9463 = K[3657];
    at = 13017; l9464 = K[3658];
    at = 13018; l9465 = K[3659];
    at = 13019; if (typeof l9463 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9463 !== "") { const [x, y] = point(rt, l9462); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9464)), color: ink(rt, l9465), text: l9463}); }
    at = 13020; l9466 = K[3660];
    at = 13021; l9467 = K[3661];
    at = 13022; l9468 = K[3662];
    at = 13023; l9469 = K[3663];
    at = 13024; l9470 = K[3664];
    at = 13025; pathOf(rt, "rounded_rect", {at: l9466, size: l9467, radius: l9468, fill: l9469, stroke: l9470});
    at = 13026; l9471 = K[3665];
    at = 13027; l9472 = K[3666];
    at = 13028; l9473 = K[3667];
    at = 13029; l9474 = K[3668];
    at = 13030; if (typeof l9472 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9472 !== "") { const [x, y] = point(rt, l9471); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9473)), color: ink(rt, l9474), text: l9472}); }
    at = 13031; l9475 = K[3669];
    at = 13032; l9476 = field(rt, l15, "color");
    at = 13033; l9477 = K[3670];
    at = 13034; l9478 = rt.eq(l9477, l9476);
    at = 13035; if (rt.truthy(l9478)) {
      at = 13036; l9480 = K[3671];
      at = 13037; l9479 = l9480;
    } else {
      at = 13038; l9481 = K[3672];
      at = 13039; l9479 = l9481;
    }
    at = 13040; l9482 = K[3673];
    at = 13041; l9483 = K[3674];
    at = 13042; markSet(rt, false, "circle", {at: l9475, radius: l9479, fill: l9482, stroke: l9483});
    at = 13043; l9484 = K[3675];
    at = 13044; l9485 = field(rt, l15, "color");
    at = 13045; l9486 = K[3676];
    at = 13046; l9487 = rt.eq(l9486, l9485);
    at = 13047; if (rt.truthy(l9487)) {
      at = 13048; l9489 = K[3677];
      at = 13049; l9488 = l9489;
    } else {
      at = 13050; l9490 = K[3678];
      at = 13051; l9488 = l9490;
    }
    at = 13052; l9491 = K[3679];
    at = 13053; l9492 = K[3680];
    at = 13054; markSet(rt, false, "circle", {at: l9484, radius: l9488, fill: l9491, stroke: l9492});
    at = 13055; l9493 = K[3681];
    at = 13056; l9494 = field(rt, l15, "color");
    at = 13057; l9495 = K[3682];
    at = 13058; l9496 = rt.eq(l9495, l9494);
    at = 13059; if (rt.truthy(l9496)) {
      at = 13060; l9498 = K[3683];
      at = 13061; l9497 = l9498;
    } else {
      at = 13062; l9499 = K[3684];
      at = 13063; l9497 = l9499;
    }
    at = 13064; l9500 = K[3685];
    at = 13065; l9501 = K[3686];
    at = 13066; markSet(rt, false, "circle", {at: l9493, radius: l9497, fill: l9500, stroke: l9501});
    at = 13067; l9502 = K[3687];
    at = 13068; l9503 = field(rt, l15, "color");
    at = 13069; l9504 = K[3688];
    at = 13070; l9505 = rt.eq(l9504, l9503);
    at = 13071; if (rt.truthy(l9505)) {
      at = 13072; l9507 = K[3689];
      at = 13073; l9506 = l9507;
    } else {
      at = 13074; l9508 = K[3690];
      at = 13075; l9506 = l9508;
    }
    at = 13076; l9509 = K[3691];
    at = 13077; l9510 = K[3692];
    at = 13078; markSet(rt, false, "circle", {at: l9502, radius: l9506, fill: l9509, stroke: l9510});
    at = 13079; l9511 = K[3693];
    at = 13080; l9512 = K[3694];
    at = 13081; l9513 = K[3695];
    at = 13082; l9514 = K[3696];
    at = 13083; l9515 = K[3697];
    at = 13084; pathOf(rt, "rounded_rect", {at: l9511, size: l9512, radius: l9513, fill: l9514, stroke: l9515});
    at = 13085; l9516 = K[3698];
    at = 13086; l9517 = K[3699];
    at = 13087; l9518 = K[3700];
    at = 13088; l9519 = K[3701];
    at = 13089; if (typeof l9517 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9517 !== "") { const [x, y] = point(rt, l9516); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9518)), color: ink(rt, l9519), text: l9517}); }
    at = 13090; l9520 = K[3702];
    at = 13091; l9521 = K[3703];
    at = 13092; l9522 = K[3704];
    at = 13093; l9523 = K[3705];
    at = 13094; l9524 = K[3706];
    at = 13095; pathOf(rt, "rounded_rect", {at: l9520, size: l9521, radius: l9522, fill: l9523, stroke: l9524});
    at = 13096; l9525 = K[3707];
    at = 13097; l9526 = K[3708];
    at = 13098; l9527 = K[3709];
    at = 13099; l9528 = K[3710];
    at = 13100; if (typeof l9526 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9526 !== "") { const [x, y] = point(rt, l9525); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9527)), color: ink(rt, l9528), text: l9526}); }
    at = 13101; l9529 = K[3711];
    at = 13102; l9530 = field(rt, l15, "width");
    at = 13103; l9531 = K[3712];
    at = 13104; l9532 = rt.int_div(l9530, l9531);
    at = 13105; l9533 = K[3713];
    at = 13106; l9534 = rt.max(l9533, l9532);
    at = 13107; l9535 = field(rt, l15, "color");
    at = 13108; l9536 = K[3714];
    at = 13109; l9537 = rt.sequence_get(l9536, l9535);
    at = 13110; markSet(rt, false, "circle", {at: l9529, radius: l9534, fill: l9537});
    at = 13111; l9538 = K[3715];
    at = 13112; l9539 = field(rt, l15, "width");
    at = 13113; l9540 = rt.show(l9539);
    at = 13114; l9541 = K[3716];
    at = 13115; l9542 = rt.text_concat(l9540, l9541);
    at = 13116; l9543 = K[3717];
    at = 13117; l9544 = K[3718];
    at = 13118; if (typeof l9542 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9542 !== "") { const [x, y] = point(rt, l9538); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9543)), color: ink(rt, l9544), text: l9542}); }
    at = 13119; l9545 = BigInt(input.size[1]);
    at = 13120; l9546 = K[3719];
    at = 13121; l9547 = rt.sub(l9545, l9546);
    at = 13122; l9548 = K[3720];
    at = 13123; l9549 = K[3721];
    at = 13124; l9550 = Float64Array.of(rt.float(l9548), rt.float(l9547), rt.float(l9549));
    at = 13125; l9551 = field(rt, l15, "status");
    at = 13126; l9552 = K[3722];
    at = 13127; l9553 = K[3723];
    at = 13128; if (typeof l9551 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l9551 !== "") { const [x, y] = point(rt, l9550); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l9552)), color: ink(rt, l9553), text: l9551}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
