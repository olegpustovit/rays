// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const M = new Map();
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
  K ??= ["#14161c", Float64Array.of(12, 12, 0), "O: open images   S: save   F: folder   drag a file in   pinch to scale", Float64Array.of(1, 1, 1), 16n, 2n, [], false, Float64Array.of(0, 0, 0), 1, 0n, 0n, 4096n, 0n, "dialog-opened", "dialog ", " opened", "file-dropped", "dropped ", "file-dialog", "", "dialog ", " failed: ", "file-dialog", 0n, "dialog ", " cancelled", "", "", 8n, 0n, 0n, 8n, 0n, "file-dialog", "", 0n, 0n, 4096n, 0n, "dialog ", ": ", 8n, 0n, 0n, 8n, 0n, 0n, 0n, 1n, 1n, "file-drag-moved", true, "file-drag-ended", false, "file-drag-moved", "pinched", 4, 0.25, 0n, 0n, 1n, 1n, 80n, 2n, 60n, 0n, 0n, "#5aaaf0", 0n, 10, "#f0b43c", "#00000000", Float64Array.of(12, 200, 0), 1n, 0n, 0n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 218, 0), 1n, 1n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 236, 0), 2n, 2n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 254, 0), 3n, 3n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 272, 0), 4n, 4n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 290, 0), 5n, 5n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 308, 0), 6n, 6n, "", Float64Array.of(1, 1, 1), 16n, Float64Array.of(12, 326, 0), 7n, 7n, "", Float64Array.of(1, 1, 1), 16n];
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
  let l304, l305, l306, l307, l308, l309, l310, l311, l312, l313, l314, l315, l316, l317;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = K[1];
    at = 3; l2 = K[2];
    at = 4; l3 = K[3];
    at = 5; l4 = K[4];
    at = 6; if (typeof l2 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l2 !== "") { const [x, y] = point(rt, l1); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l4)), color: ink(rt, l3), text: l2}); }
    at = 7; l5 = BigInt(input.size[0]);
    at = 8; l6 = K[5];
    at = 9; l7 = rt.int_div(l5, l6);
    at = 15; { const c15 = cell(state, 0, "", input.tick);
      if (c15.value !== undefined) l13 = c15.value; else {
        at = 10; l8 = K[6];
        at = 11; l9 = K[7];
        at = 12; l10 = K[8];
        at = 13; l11 = K[9];
        at = 14; l12 = new Map([["lines", l8], ["dragging", l9], ["position", l10], ["zoom", l11]]);
        l13 = l12;
      } }
    at = 16; { const c16 = cell(state, 0, "", input.tick); l14 = c16.pending !== undefined; l15 = l14 ? c16.pending : null; }
    at = 17; if (rt.truthy(l14)) {
      at = 18; l16 = l15;
    } else {
      at = 19; l17 = l13;
      at = 20; l18 = K[10];
      at = 21; l19 = l18;
      at = 22; l20 = K[11];
      at = 23; l21 = l20;
      at = 24; l22 = rt.frameValue(input);
      at = 25; l23 = field(rt, l22, "events");
      at = 26; {
        const s26 = l23, c26 = Array.isArray(s26) ? s26.length : Number(rt.array_count(s26));
        if (c26 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i26 = 0; i26 < c26; i26++) {
          l24 = BigInt(i26); l25 = Array.isArray(s26) ? s26[i26] : rt.array_nth(s26, i26);
          at = 27; l26 = K[12];
          at = 28; l27 = rt.ge(l19, l26);
          at = 29; if (rt.truthy(l27)) {
            at = 30; l29 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 31; l28 = l29;
          } else {
            at = 32; l30 = K[13];
            at = 33; l28 = l30;
          }
          at = 34; l31 = field(rt, l25, "kind");
          at = 35; l32 = K[14];
          at = 36; l33 = rt.equal(l31, l32);
          at = 37; if (rt.truthy(l33)) {
            at = 38; l35 = field(rt, l25, "id");
            at = 39; l36 = rt.show(l35);
            at = 40; l37 = K[15];
            at = 41; l38 = K[16];
            at = 42; l39 = rt.text_concat(l37, l36, l38);
            at = 43; l34 = l39;
          } else {
            at = 44; l40 = field(rt, l25, "kind");
            at = 45; l41 = K[17];
            at = 46; l42 = rt.equal(l40, l41);
            at = 47; if (rt.truthy(l42)) {
              at = 48; l44 = field(rt, l25, "text");
              at = 49; l45 = rt.show(l44);
              at = 50; l46 = K[18];
              at = 51; l47 = rt.text_concat(l46, l45);
              at = 52; l43 = l47;
            } else {
              at = 53; l48 = field(rt, l25, "kind");
              at = 54; l49 = K[19];
              at = 55; l50 = rt.equal(l48, l49);
              at = 56; l51 = field(rt, l25, "error");
              at = 57; l52 = K[20];
              at = 58; l53 = rt.equal(l51, l52);
              at = 59; l54 = rt.not(l53);
              at = 60; l55 = rt.and(l50, l54);
              at = 61; if (rt.truthy(l55)) {
                at = 62; l57 = field(rt, l25, "id");
                at = 63; l58 = field(rt, l25, "error");
                at = 64; l59 = rt.show(l57);
                at = 65; l60 = rt.show(l58);
                at = 66; l61 = K[21];
                at = 67; l62 = K[22];
                at = 68; l63 = rt.text_concat(l61, l59, l62, l60);
                at = 69; l56 = l63;
              } else {
                at = 70; l64 = field(rt, l25, "kind");
                at = 71; l65 = K[23];
                at = 72; l66 = rt.equal(l64, l65);
                at = 73; l67 = field(rt, l25, "paths");
                at = 74; l68 = rt.sequence_length(l67);
                at = 75; l69 = K[24];
                at = 76; l70 = rt.eq(l68, l69);
                at = 77; l71 = rt.and(l66, l70);
                at = 78; if (rt.truthy(l71)) {
                  at = 79; l73 = field(rt, l25, "id");
                  at = 80; l74 = rt.show(l73);
                  at = 81; l75 = K[25];
                  at = 82; l76 = K[26];
                  at = 83; l77 = rt.text_concat(l75, l74, l76);
                  at = 84; l72 = l77;
                } else {
                  at = 85; l78 = K[27];
                  at = 86; l72 = l78;
                }
                at = 87; l56 = l72;
              }
              at = 88; l43 = l56;
            }
            at = 89; l34 = l43;
          }
          at = 90; l79 = K[28];
          at = 91; l80 = rt.equal(l34, l79);
          at = 92; if (rt.truthy(l80)) {
            at = 93; l82 = field(rt, l17, "lines");
            at = 94; l81 = l82;
          } else {
            at = 95; l83 = rt.sequence_concat([l34]);
            at = 96; l84 = field(rt, l17, "lines");
            at = 97; l85 = rt.sequence_concat(l83, l84);
            at = 98; l86 = rt.sequence_length(l85);
            at = 99; l87 = K[29];
            at = 100; l88 = K[30];
            at = 101; l89 = rt.int_lt(l87, l88);
            at = 102; if (rt.truthy(l89)) {
              at = 103; l91 = K[31];
              at = 104; l90 = l91;
            } else {
              at = 105; l92 = K[32];
              at = 106; l90 = l92;
            }
            at = 107; l93 = rt.int_gt(l90, l86);
            at = 108; if (rt.truthy(l93)) {
              at = 109; l94 = l86;
            } else {
              at = 110; l94 = l90;
            }
            at = 111; l95 = K[33];
            at = 112; l96 = rt.sequence_slice(l85, l95, l94);
            at = 113; l81 = l96;
          }
          at = 114; l97 = field(rt, l25, "kind");
          at = 115; l98 = K[34];
          at = 116; l99 = rt.equal(l97, l98);
          at = 117; l100 = field(rt, l25, "error");
          at = 118; l101 = K[35];
          at = 119; l102 = rt.equal(l100, l101);
          at = 120; l103 = rt.and(l99, l102);
          at = 121; if (rt.truthy(l103)) {
            at = 122; l105 = l81;
            at = 123; l106 = K[36];
            at = 124; l107 = l106;
            at = 125; l108 = K[37];
            at = 126; l109 = l108;
            at = 127; l110 = field(rt, l25, "paths");
            at = 128; {
              const s128 = l110, c128 = Array.isArray(s128) ? s128.length : Number(rt.array_count(s128));
              if (c128 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              for (let i128 = 0; i128 < c128; i128++) {
                l111 = BigInt(i128); l112 = Array.isArray(s128) ? s128[i128] : rt.array_nth(s128, i128);
                at = 129; l113 = K[38];
                at = 130; l114 = rt.ge(l107, l113);
                at = 131; if (rt.truthy(l114)) {
                  at = 132; l116 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
                  at = 133; l115 = l116;
                } else {
                  at = 134; l117 = K[39];
                  at = 135; l115 = l117;
                }
                at = 136; l118 = field(rt, l25, "id");
                at = 137; l119 = rt.show(l118);
                at = 138; l120 = rt.show(l112);
                at = 139; l121 = K[40];
                at = 140; l122 = K[41];
                at = 141; l123 = rt.text_concat(l121, l119, l122, l120);
                at = 142; l124 = rt.sequence_concat([l123]);
                at = 143; l125 = rt.sequence_concat(l124, l105);
                at = 144; l126 = rt.sequence_length(l125);
                at = 145; l127 = K[42];
                at = 146; l128 = K[43];
                at = 147; l129 = rt.int_lt(l127, l128);
                at = 148; if (rt.truthy(l129)) {
                  at = 149; l131 = K[44];
                  at = 150; l130 = l131;
                } else {
                  at = 151; l132 = K[45];
                  at = 152; l130 = l132;
                }
                at = 153; l133 = rt.int_gt(l130, l126);
                at = 154; if (rt.truthy(l133)) {
                  at = 155; l134 = l126;
                } else {
                  at = 156; l134 = l130;
                }
                at = 157; l135 = K[46];
                at = 158; l136 = rt.sequence_slice(l125, l135, l134);
                at = 159; l137 = rt.sequence_length(l105);
                at = 160; l138 = K[47];
                at = 161; l139 = rt.eq(l137, l138);
                at = 162; if (rt.truthy(l139)) {
                  at = 163; l140 = l136;
                } else {
                  at = 164; l141 = K[48];
                  at = 165; l142 = rt.sequence_get(l105, l141);
                  at = 166; l143 = rt.sequence_length(l136);
                  at = 167; l144 = rt.sequence_build(l143);
                  at = 168; {
                    const s168 = l136, c168 = Array.isArray(s168) ? s168.length : Number(rt.array_count(s168));
                    if (c168 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                    for (let i168 = 0; i168 < c168; i168++) {
                      l145 = BigInt(i168); l146 = Array.isArray(s168) ? s168[i168] : rt.array_nth(s168, i168);
                      at = 169; store(rt, l144, l145, l146);
                    }
                  }
                  at = 170; l140 = l144;
                }
                at = 171; l105 = l140;
                at = 172; l147 = K[49];
                at = 173; l148 = rt.add(l109, l147);
                at = 174; l109 = l148;
                at = 175; l149 = K[50];
                at = 176; l150 = rt.add(l107, l149);
                at = 177; l107 = l150;
              }
            }
            at = 178; l104 = l105;
          } else {
            at = 179; l104 = l81;
          }
          at = 180; l151 = field(rt, l25, "kind");
          at = 181; l152 = K[51];
          at = 182; l153 = rt.equal(l151, l152);
          at = 183; if (rt.truthy(l153)) {
            at = 184; l155 = K[52];
            at = 185; l154 = l155;
          } else {
            at = 186; l156 = field(rt, l25, "kind");
            at = 187; l157 = K[53];
            at = 188; l158 = rt.equal(l156, l157);
            at = 189; if (rt.truthy(l158)) {
              at = 190; l160 = K[54];
              at = 191; l159 = l160;
            } else {
              at = 192; l161 = field(rt, l17, "dragging");
              at = 193; l159 = l161;
            }
            at = 194; l154 = l159;
          }
          at = 195; l162 = field(rt, l25, "kind");
          at = 196; l163 = K[55];
          at = 197; l164 = rt.equal(l162, l163);
          at = 198; if (rt.truthy(l164)) {
            at = 199; l166 = field(rt, l25, "position");
            at = 200; l165 = l166;
          } else {
            at = 201; l167 = field(rt, l17, "position");
            at = 202; l165 = l167;
          }
          at = 203; l168 = field(rt, l25, "kind");
          at = 204; l169 = K[56];
          at = 205; l170 = rt.equal(l168, l169);
          at = 206; if (rt.truthy(l170)) {
            at = 207; l172 = field(rt, l17, "zoom");
            at = 208; l173 = field(rt, l25, "scale");
            at = 209; l174 = rt.mul(l172, l173);
            at = 210; l175 = K[57];
            at = 211; l176 = rt.min(l175, l174);
            at = 212; l177 = K[58];
            at = 213; l178 = rt.max(l177, l176);
            at = 214; l171 = l178;
          } else {
            at = 215; l179 = field(rt, l17, "zoom");
            at = 216; l171 = l179;
          }
          at = 217; l180 = field(rt, l17, "lines");
          at = 218; l181 = rt.sequence_length(l180);
          at = 219; l182 = K[59];
          at = 220; l183 = rt.eq(l181, l182);
          at = 221; if (rt.truthy(l183)) {
            at = 222; l184 = l104;
          } else {
            at = 223; l185 = K[60];
            at = 224; l186 = rt.sequence_get(l180, l185);
            at = 225; l187 = rt.sequence_length(l104);
            at = 226; l188 = rt.sequence_build(l187);
            at = 227; {
              const s227 = l104, c227 = Array.isArray(s227) ? s227.length : Number(rt.array_count(s227));
              if (c227 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              for (let i227 = 0; i227 < c227; i227++) {
                l189 = BigInt(i227); l190 = Array.isArray(s227) ? s227[i227] : rt.array_nth(s227, i227);
                at = 228; store(rt, l188, l189, l190);
              }
            }
            at = 229; l184 = l188;
          }
          at = 230; l191 = field(rt, l17, "dragging");
          at = 231; l192 = field(rt, l17, "position");
          at = 232; l193 = field(rt, l17, "zoom");
          at = 233; l194 = rt.coerce("float", l193);
          at = 234; l195 = rt.equal(l193, l194);
          at = 235; if (rt.truthy(l195)) {
            at = 236; l197 = rt.coerce("float", l171);
            at = 237; l196 = l197;
          } else {
            at = 238; l198 = rt.coerce("int", l171);
            at = 239; l196 = l198;
          }
          at = 240; l199 = new Map([["lines", l184], ["dragging", l154], ["position", l165], ["zoom", l196]]);
          at = 241; l17 = l199;
          at = 242; l200 = K[61];
          at = 243; l201 = rt.add(l21, l200);
          at = 244; l21 = l201;
          at = 245; l202 = K[62];
          at = 246; l203 = rt.add(l19, l202);
          at = 247; l19 = l203;
        }
      }
      at = 248; cell(state, 0, "", input.tick).pending = l17;
      at = 249; l16 = l17;
    }
    at = 250; l204 = field(rt, l16, "zoom");
    at = 251; l205 = K[63];
    at = 252; l206 = rt.mul(l205, l204);
    at = 253; l207 = rt.int(l206);
    at = 254; l208 = K[64];
    at = 255; l209 = rt.int_div(l207, l208);
    at = 256; l210 = rt.sub(l7, l209);
    at = 257; l211 = K[65];
    at = 258; l212 = K[66];
    at = 259; l213 = Float64Array.of(rt.float(l210), rt.float(l211), rt.float(l212));
    at = 260; l214 = K[67];
    at = 261; l215 = Float64Array.of(rt.float(l207), rt.float(l207), rt.float(l214));
    at = 262; l216 = K[68];
    at = 263; markSet(rt, false, "rect", {at: l213, size: l215, fill: l216});
    at = 264; l217 = field(rt, l16, "position");
    at = 265; l218 = field(rt, l217, "x");
    at = 266; l219 = rt.int(l218);
    at = 267; l220 = field(rt, l16, "position");
    at = 268; l221 = field(rt, l220, "y");
    at = 269; l222 = rt.int(l221);
    at = 270; l223 = K[69];
    at = 271; l224 = Float64Array.of(rt.float(l219), rt.float(l222), rt.float(l223));
    at = 272; l225 = K[70];
    at = 273; l226 = field(rt, l16, "dragging");
    at = 274; if (rt.truthy(l226)) {
      at = 275; l228 = K[71];
      at = 276; l227 = l228;
    } else {
      at = 277; l229 = K[72];
      at = 278; l227 = l229;
    }
    at = 279; markSet(rt, false, "circle", {at: l224, radius: l225, fill: l227});
    at = 280; l230 = K[73];
    at = 281; l231 = field(rt, l16, "lines");
    at = 282; l232 = rt.sequence_length(l231);
    at = 283; l233 = rt.sequence_build(l232);
    { const k284 = [l231, l232], h284 = rt.recall(M, 284, k284);
    if (h284) [l233, l234, l235, l236, l237, l238] = h284; else {
      at = 284; {
        const s284 = l231, c284 = Array.isArray(s284) ? s284.length : Number(rt.array_count(s284));
        if (c284 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i284 = 0; i284 < c284; i284++) {
          l234 = BigInt(i284); l235 = Array.isArray(s284) ? s284[i284] : rt.array_nth(s284, i284);
          at = 285; l236 = K[74];
          at = 286; l237 = rt.sub(l232, l236);
          at = 287; l238 = rt.sub(l237, l234);
          at = 288; store(rt, l233, l238, l235);
        }
      }
    rt.keep(M, 284, k284, [l233, l234, l235, l236, l237, l238]); } }
    at = 289; l239 = rt.sequence_length(l233);
    at = 290; l240 = K[75];
    at = 291; l241 = rt.lt(l240, l239);
    at = 292; if (rt.truthy(l241)) {
      at = 293; l243 = K[76];
      at = 294; l244 = rt.sequence_get(l233, l243);
      at = 295; l242 = l244;
    } else {
      at = 296; l245 = K[77];
      at = 297; l242 = l245;
    }
    at = 298; l246 = K[78];
    at = 299; l247 = K[79];
    at = 300; if (typeof l242 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l242 !== "") { const [x, y] = point(rt, l230); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l247)), color: ink(rt, l246), text: l242}); }
    at = 301; l248 = K[80];
    at = 302; l249 = rt.sequence_length(l233);
    at = 303; l250 = K[81];
    at = 304; l251 = rt.lt(l250, l249);
    at = 305; if (rt.truthy(l251)) {
      at = 306; l253 = K[82];
      at = 307; l254 = rt.sequence_get(l233, l253);
      at = 308; l252 = l254;
    } else {
      at = 309; l255 = K[83];
      at = 310; l252 = l255;
    }
    at = 311; l256 = K[84];
    at = 312; l257 = K[85];
    at = 313; if (typeof l252 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l252 !== "") { const [x, y] = point(rt, l248); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l257)), color: ink(rt, l256), text: l252}); }
    at = 314; l258 = K[86];
    at = 315; l259 = rt.sequence_length(l233);
    at = 316; l260 = K[87];
    at = 317; l261 = rt.lt(l260, l259);
    at = 318; if (rt.truthy(l261)) {
      at = 319; l263 = K[88];
      at = 320; l264 = rt.sequence_get(l233, l263);
      at = 321; l262 = l264;
    } else {
      at = 322; l265 = K[89];
      at = 323; l262 = l265;
    }
    at = 324; l266 = K[90];
    at = 325; l267 = K[91];
    at = 326; if (typeof l262 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l262 !== "") { const [x, y] = point(rt, l258); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l267)), color: ink(rt, l266), text: l262}); }
    at = 327; l268 = K[92];
    at = 328; l269 = rt.sequence_length(l233);
    at = 329; l270 = K[93];
    at = 330; l271 = rt.lt(l270, l269);
    at = 331; if (rt.truthy(l271)) {
      at = 332; l273 = K[94];
      at = 333; l274 = rt.sequence_get(l233, l273);
      at = 334; l272 = l274;
    } else {
      at = 335; l275 = K[95];
      at = 336; l272 = l275;
    }
    at = 337; l276 = K[96];
    at = 338; l277 = K[97];
    at = 339; if (typeof l272 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l272 !== "") { const [x, y] = point(rt, l268); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l277)), color: ink(rt, l276), text: l272}); }
    at = 340; l278 = K[98];
    at = 341; l279 = rt.sequence_length(l233);
    at = 342; l280 = K[99];
    at = 343; l281 = rt.lt(l280, l279);
    at = 344; if (rt.truthy(l281)) {
      at = 345; l283 = K[100];
      at = 346; l284 = rt.sequence_get(l233, l283);
      at = 347; l282 = l284;
    } else {
      at = 348; l285 = K[101];
      at = 349; l282 = l285;
    }
    at = 350; l286 = K[102];
    at = 351; l287 = K[103];
    at = 352; if (typeof l282 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l282 !== "") { const [x, y] = point(rt, l278); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l287)), color: ink(rt, l286), text: l282}); }
    at = 353; l288 = K[104];
    at = 354; l289 = rt.sequence_length(l233);
    at = 355; l290 = K[105];
    at = 356; l291 = rt.lt(l290, l289);
    at = 357; if (rt.truthy(l291)) {
      at = 358; l293 = K[106];
      at = 359; l294 = rt.sequence_get(l233, l293);
      at = 360; l292 = l294;
    } else {
      at = 361; l295 = K[107];
      at = 362; l292 = l295;
    }
    at = 363; l296 = K[108];
    at = 364; l297 = K[109];
    at = 365; if (typeof l292 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l292 !== "") { const [x, y] = point(rt, l288); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l297)), color: ink(rt, l296), text: l292}); }
    at = 366; l298 = K[110];
    at = 367; l299 = rt.sequence_length(l233);
    at = 368; l300 = K[111];
    at = 369; l301 = rt.lt(l300, l299);
    at = 370; if (rt.truthy(l301)) {
      at = 371; l303 = K[112];
      at = 372; l304 = rt.sequence_get(l233, l303);
      at = 373; l302 = l304;
    } else {
      at = 374; l305 = K[113];
      at = 375; l302 = l305;
    }
    at = 376; l306 = K[114];
    at = 377; l307 = K[115];
    at = 378; if (typeof l302 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l302 !== "") { const [x, y] = point(rt, l298); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l307)), color: ink(rt, l306), text: l302}); }
    at = 379; l308 = K[116];
    at = 380; l309 = rt.sequence_length(l233);
    at = 381; l310 = K[117];
    at = 382; l311 = rt.lt(l310, l309);
    at = 383; if (rt.truthy(l311)) {
      at = 384; l313 = K[118];
      at = 385; l314 = rt.sequence_get(l233, l313);
      at = 386; l312 = l314;
    } else {
      at = 387; l315 = K[119];
      at = 388; l312 = l315;
    }
    at = 389; l316 = K[120];
    at = 390; l317 = K[121];
    at = 391; if (typeof l312 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l312 !== "") { const [x, y] = point(rt, l308); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l317)), color: ink(rt, l316), text: l312}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
