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
  K ??= ["#14161c", Float64Array.of(12, 12, 0), "O: open images   S: save   F: folder   drag a file in   pinch to scale", 16n, Float64Array.of(1, 1, 1), 2n, [], false, Float64Array.of(0, 0, 0), 1, 0n, 0n, 4096n, 0n, "dialog-opened", "dialog ", " opened", "file-dropped", "dropped ", "file-dialog", "", "dialog ", " failed: ", "file-dialog", 0n, "dialog ", " cancelled", "", "", 8n, 0n, 0n, "file-dialog", "", 0n, 0n, 4096n, 0n, "dialog ", ": ", 8n, 0n, 0n, 0n, 0n, 1n, 1n, "file-drag-moved", true, "file-drag-ended", false, "file-drag-moved", "pinched", 4, 0.25, 0n, 0n, 1n, 1n, 80n, 2n, 60n, 0n, 0n, "#5aaaf0", 0n, 10n, "#f0b43c", "#00000000", Float64Array.of(12, 200, 0), 1n, 0n, 0n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 218, 0), 1n, 1n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 236, 0), 2n, 2n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 254, 0), 3n, 3n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 272, 0), 4n, 4n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 290, 0), 5n, 5n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 308, 0), 6n, 6n, "", 16n, Float64Array.of(1, 1, 1), Float64Array.of(12, 326, 0), 7n, 7n, "", 16n, Float64Array.of(1, 1, 1)];
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
  let l304, l305, l306, l307, l308, l309, l310, l311, l312, l313;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = K[1];
    at = 3; l2 = K[2];
    at = 4; l3 = K[3];
    at = 5; l4 = K[4];
    at = 6; if (typeof l2 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l2 !== "") { const [x, y] = point(rt, l1); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l3)), color: ink(rt, l4), text: l2}); }
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
            at = 100; l88 = rt.float(l87);
            at = 101; l89 = rt.coerce("int", l88);
            at = 102; l90 = K[30];
            at = 103; l91 = rt.max(l90, l89);
            at = 104; l92 = rt.min(l91, l86);
            at = 105; l93 = K[31];
            at = 106; l94 = rt.sequence_slice(l85, l93, l92);
            at = 107; l81 = l94;
          }
          at = 108; l95 = field(rt, l25, "kind");
          at = 109; l96 = K[32];
          at = 110; l97 = rt.equal(l95, l96);
          at = 111; l98 = field(rt, l25, "error");
          at = 112; l99 = K[33];
          at = 113; l100 = rt.equal(l98, l99);
          at = 114; l101 = rt.and(l97, l100);
          at = 115; if (rt.truthy(l101)) {
            at = 116; l103 = l81;
            at = 117; l104 = K[34];
            at = 118; l105 = l104;
            at = 119; l106 = K[35];
            at = 120; l107 = l106;
            at = 121; l108 = field(rt, l25, "paths");
            at = 122; {
              const s122 = l108, c122 = Array.isArray(s122) ? s122.length : Number(rt.array_count(s122));
              if (c122 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              for (let i122 = 0; i122 < c122; i122++) {
                l109 = BigInt(i122); l110 = Array.isArray(s122) ? s122[i122] : rt.array_nth(s122, i122);
                at = 123; l111 = K[36];
                at = 124; l112 = rt.ge(l105, l111);
                at = 125; if (rt.truthy(l112)) {
                  at = 126; l114 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
                  at = 127; l113 = l114;
                } else {
                  at = 128; l115 = K[37];
                  at = 129; l113 = l115;
                }
                at = 130; l116 = field(rt, l25, "id");
                at = 131; l117 = rt.show(l116);
                at = 132; l118 = rt.show(l110);
                at = 133; l119 = K[38];
                at = 134; l120 = K[39];
                at = 135; l121 = rt.text_concat(l119, l117, l120, l118);
                at = 136; l122 = rt.sequence_concat([l121]);
                at = 137; l123 = rt.sequence_concat(l122, l103);
                at = 138; l124 = rt.sequence_length(l123);
                at = 139; l125 = K[40];
                at = 140; l126 = rt.float(l125);
                at = 141; l127 = rt.coerce("int", l126);
                at = 142; l128 = K[41];
                at = 143; l129 = rt.max(l128, l127);
                at = 144; l130 = rt.min(l129, l124);
                at = 145; l131 = K[42];
                at = 146; l132 = rt.sequence_slice(l123, l131, l130);
                at = 147; l133 = rt.sequence_length(l103);
                at = 148; l134 = K[43];
                at = 149; l135 = rt.eq(l133, l134);
                at = 150; if (rt.truthy(l135)) {
                  at = 151; l136 = l132;
                } else {
                  at = 152; l137 = K[44];
                  at = 153; l138 = rt.sequence_get(l103, l137);
                  at = 154; l139 = rt.sequence_length(l132);
                  at = 155; l140 = rt.sequence_build(l139);
                  at = 156; {
                    const s156 = l132, c156 = Array.isArray(s156) ? s156.length : Number(rt.array_count(s156));
                    if (c156 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                    for (let i156 = 0; i156 < c156; i156++) {
                      l141 = BigInt(i156); l142 = Array.isArray(s156) ? s156[i156] : rt.array_nth(s156, i156);
                      at = 157; store(rt, l140, l141, l142);
                    }
                  }
                  at = 158; l136 = l140;
                }
                at = 159; l103 = l136;
                at = 160; l143 = K[45];
                at = 161; l144 = rt.add(l107, l143);
                at = 162; l107 = l144;
                at = 163; l145 = K[46];
                at = 164; l146 = rt.add(l105, l145);
                at = 165; l105 = l146;
              }
            }
            at = 166; l102 = l103;
          } else {
            at = 167; l102 = l81;
          }
          at = 168; l147 = field(rt, l25, "kind");
          at = 169; l148 = K[47];
          at = 170; l149 = rt.equal(l147, l148);
          at = 171; if (rt.truthy(l149)) {
            at = 172; l151 = K[48];
            at = 173; l150 = l151;
          } else {
            at = 174; l152 = field(rt, l25, "kind");
            at = 175; l153 = K[49];
            at = 176; l154 = rt.equal(l152, l153);
            at = 177; if (rt.truthy(l154)) {
              at = 178; l156 = K[50];
              at = 179; l155 = l156;
            } else {
              at = 180; l157 = field(rt, l17, "dragging");
              at = 181; l155 = l157;
            }
            at = 182; l150 = l155;
          }
          at = 183; l158 = field(rt, l25, "kind");
          at = 184; l159 = K[51];
          at = 185; l160 = rt.equal(l158, l159);
          at = 186; if (rt.truthy(l160)) {
            at = 187; l162 = field(rt, l25, "position");
            at = 188; l161 = l162;
          } else {
            at = 189; l163 = field(rt, l17, "position");
            at = 190; l161 = l163;
          }
          at = 191; l164 = field(rt, l25, "kind");
          at = 192; l165 = K[52];
          at = 193; l166 = rt.equal(l164, l165);
          at = 194; if (rt.truthy(l166)) {
            at = 195; l168 = field(rt, l17, "zoom");
            at = 196; l169 = field(rt, l25, "scale");
            at = 197; l170 = rt.mul(l168, l169);
            at = 198; l171 = K[53];
            at = 199; l172 = rt.min(l171, l170);
            at = 200; l173 = K[54];
            at = 201; l174 = rt.max(l173, l172);
            at = 202; l167 = l174;
          } else {
            at = 203; l175 = field(rt, l17, "zoom");
            at = 204; l167 = l175;
          }
          at = 205; l176 = field(rt, l17, "lines");
          at = 206; l177 = rt.sequence_length(l176);
          at = 207; l178 = K[55];
          at = 208; l179 = rt.eq(l177, l178);
          at = 209; if (rt.truthy(l179)) {
            at = 210; l180 = l102;
          } else {
            at = 211; l181 = K[56];
            at = 212; l182 = rt.sequence_get(l176, l181);
            at = 213; l183 = rt.sequence_length(l102);
            at = 214; l184 = rt.sequence_build(l183);
            at = 215; {
              const s215 = l102, c215 = Array.isArray(s215) ? s215.length : Number(rt.array_count(s215));
              if (c215 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              for (let i215 = 0; i215 < c215; i215++) {
                l185 = BigInt(i215); l186 = Array.isArray(s215) ? s215[i215] : rt.array_nth(s215, i215);
                at = 216; store(rt, l184, l185, l186);
              }
            }
            at = 217; l180 = l184;
          }
          at = 218; l187 = field(rt, l17, "dragging");
          at = 219; l188 = field(rt, l17, "position");
          at = 220; l189 = field(rt, l17, "zoom");
          at = 221; l190 = rt.coerce("float", l189);
          at = 222; l191 = rt.equal(l189, l190);
          at = 223; if (rt.truthy(l191)) {
            at = 224; l193 = rt.coerce("float", l167);
            at = 225; l192 = l193;
          } else {
            at = 226; l194 = rt.coerce("int", l167);
            at = 227; l192 = l194;
          }
          at = 228; l195 = new Map([["lines", l180], ["dragging", l150], ["position", l161], ["zoom", l192]]);
          at = 229; l17 = l195;
          at = 230; l196 = K[57];
          at = 231; l197 = rt.add(l21, l196);
          at = 232; l21 = l197;
          at = 233; l198 = K[58];
          at = 234; l199 = rt.add(l19, l198);
          at = 235; l19 = l199;
        }
      }
      at = 236; cell(state, 0, "", input.tick).pending = l17;
      at = 237; l16 = l17;
    }
    at = 238; l200 = field(rt, l16, "zoom");
    at = 239; l201 = K[59];
    at = 240; l202 = rt.mul(l201, l200);
    at = 241; l203 = rt.int(l202);
    at = 242; l204 = K[60];
    at = 243; l205 = rt.int_div(l203, l204);
    at = 244; l206 = rt.sub(l7, l205);
    at = 245; l207 = K[61];
    at = 246; l208 = K[62];
    at = 247; l209 = Float64Array.of(rt.float(l206), rt.float(l207), rt.float(l208));
    at = 248; l210 = K[63];
    at = 249; l211 = Float64Array.of(rt.float(l203), rt.float(l203), rt.float(l210));
    at = 250; l212 = K[64];
    at = 251; markSet(rt, false, "rect", {at: l209, size: l211, fill: l212});
    at = 252; l213 = field(rt, l16, "position");
    at = 253; l214 = field(rt, l213, "x");
    at = 254; l215 = rt.int(l214);
    at = 255; l216 = field(rt, l16, "position");
    at = 256; l217 = field(rt, l216, "y");
    at = 257; l218 = rt.int(l217);
    at = 258; l219 = K[65];
    at = 259; l220 = Float64Array.of(rt.float(l215), rt.float(l218), rt.float(l219));
    at = 260; l221 = K[66];
    at = 261; l222 = field(rt, l16, "dragging");
    at = 262; if (rt.truthy(l222)) {
      at = 263; l224 = K[67];
      at = 264; l223 = l224;
    } else {
      at = 265; l225 = K[68];
      at = 266; l223 = l225;
    }
    at = 267; markSet(rt, false, "circle", {at: l220, radius: l221, fill: l223});
    at = 268; l226 = K[69];
    at = 269; l227 = field(rt, l16, "lines");
    at = 270; l228 = rt.sequence_length(l227);
    at = 271; l229 = rt.sequence_build(l228);
    { const k272 = [l227, l228], h272 = rt.recall(M, 272, k272);
    if (h272) [l229, l230, l231, l232, l233, l234] = h272; else {
      at = 272; {
        const s272 = l227, c272 = Array.isArray(s272) ? s272.length : Number(rt.array_count(s272));
        if (c272 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i272 = 0; i272 < c272; i272++) {
          l230 = BigInt(i272); l231 = Array.isArray(s272) ? s272[i272] : rt.array_nth(s272, i272);
          at = 273; l232 = K[70];
          at = 274; l233 = rt.sub(l228, l232);
          at = 275; l234 = rt.sub(l233, l230);
          at = 276; store(rt, l229, l234, l231);
        }
      }
    rt.keep(M, 272, k272, [l229, l230, l231, l232, l233, l234]); } }
    at = 277; l235 = rt.sequence_length(l229);
    at = 278; l236 = K[71];
    at = 279; l237 = rt.lt(l236, l235);
    at = 280; if (rt.truthy(l237)) {
      at = 281; l239 = K[72];
      at = 282; l240 = rt.sequence_get(l229, l239);
      at = 283; l238 = l240;
    } else {
      at = 284; l241 = K[73];
      at = 285; l238 = l241;
    }
    at = 286; l242 = K[74];
    at = 287; l243 = K[75];
    at = 288; if (typeof l238 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l238 !== "") { const [x, y] = point(rt, l226); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l242)), color: ink(rt, l243), text: l238}); }
    at = 289; l244 = K[76];
    at = 290; l245 = rt.sequence_length(l229);
    at = 291; l246 = K[77];
    at = 292; l247 = rt.lt(l246, l245);
    at = 293; if (rt.truthy(l247)) {
      at = 294; l249 = K[78];
      at = 295; l250 = rt.sequence_get(l229, l249);
      at = 296; l248 = l250;
    } else {
      at = 297; l251 = K[79];
      at = 298; l248 = l251;
    }
    at = 299; l252 = K[80];
    at = 300; l253 = K[81];
    at = 301; if (typeof l248 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l248 !== "") { const [x, y] = point(rt, l244); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l252)), color: ink(rt, l253), text: l248}); }
    at = 302; l254 = K[82];
    at = 303; l255 = rt.sequence_length(l229);
    at = 304; l256 = K[83];
    at = 305; l257 = rt.lt(l256, l255);
    at = 306; if (rt.truthy(l257)) {
      at = 307; l259 = K[84];
      at = 308; l260 = rt.sequence_get(l229, l259);
      at = 309; l258 = l260;
    } else {
      at = 310; l261 = K[85];
      at = 311; l258 = l261;
    }
    at = 312; l262 = K[86];
    at = 313; l263 = K[87];
    at = 314; if (typeof l258 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l258 !== "") { const [x, y] = point(rt, l254); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l262)), color: ink(rt, l263), text: l258}); }
    at = 315; l264 = K[88];
    at = 316; l265 = rt.sequence_length(l229);
    at = 317; l266 = K[89];
    at = 318; l267 = rt.lt(l266, l265);
    at = 319; if (rt.truthy(l267)) {
      at = 320; l269 = K[90];
      at = 321; l270 = rt.sequence_get(l229, l269);
      at = 322; l268 = l270;
    } else {
      at = 323; l271 = K[91];
      at = 324; l268 = l271;
    }
    at = 325; l272 = K[92];
    at = 326; l273 = K[93];
    at = 327; if (typeof l268 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l268 !== "") { const [x, y] = point(rt, l264); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l272)), color: ink(rt, l273), text: l268}); }
    at = 328; l274 = K[94];
    at = 329; l275 = rt.sequence_length(l229);
    at = 330; l276 = K[95];
    at = 331; l277 = rt.lt(l276, l275);
    at = 332; if (rt.truthy(l277)) {
      at = 333; l279 = K[96];
      at = 334; l280 = rt.sequence_get(l229, l279);
      at = 335; l278 = l280;
    } else {
      at = 336; l281 = K[97];
      at = 337; l278 = l281;
    }
    at = 338; l282 = K[98];
    at = 339; l283 = K[99];
    at = 340; if (typeof l278 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l278 !== "") { const [x, y] = point(rt, l274); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l282)), color: ink(rt, l283), text: l278}); }
    at = 341; l284 = K[100];
    at = 342; l285 = rt.sequence_length(l229);
    at = 343; l286 = K[101];
    at = 344; l287 = rt.lt(l286, l285);
    at = 345; if (rt.truthy(l287)) {
      at = 346; l289 = K[102];
      at = 347; l290 = rt.sequence_get(l229, l289);
      at = 348; l288 = l290;
    } else {
      at = 349; l291 = K[103];
      at = 350; l288 = l291;
    }
    at = 351; l292 = K[104];
    at = 352; l293 = K[105];
    at = 353; if (typeof l288 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l288 !== "") { const [x, y] = point(rt, l284); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l292)), color: ink(rt, l293), text: l288}); }
    at = 354; l294 = K[106];
    at = 355; l295 = rt.sequence_length(l229);
    at = 356; l296 = K[107];
    at = 357; l297 = rt.lt(l296, l295);
    at = 358; if (rt.truthy(l297)) {
      at = 359; l299 = K[108];
      at = 360; l300 = rt.sequence_get(l229, l299);
      at = 361; l298 = l300;
    } else {
      at = 362; l301 = K[109];
      at = 363; l298 = l301;
    }
    at = 364; l302 = K[110];
    at = 365; l303 = K[111];
    at = 366; if (typeof l298 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l298 !== "") { const [x, y] = point(rt, l294); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l302)), color: ink(rt, l303), text: l298}); }
    at = 367; l304 = K[112];
    at = 368; l305 = rt.sequence_length(l229);
    at = 369; l306 = K[113];
    at = 370; l307 = rt.lt(l306, l305);
    at = 371; if (rt.truthy(l307)) {
      at = 372; l309 = K[114];
      at = 373; l310 = rt.sequence_get(l229, l309);
      at = 374; l308 = l310;
    } else {
      at = 375; l311 = K[115];
      at = 376; l308 = l311;
    }
    at = 377; l312 = K[116];
    at = 378; l313 = K[117];
    at = 379; if (typeof l308 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l308 !== "") { const [x, y] = point(rt, l304); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l312)), color: ink(rt, l313), text: l308}); }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
