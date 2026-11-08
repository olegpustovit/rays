// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
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
export async function frame(input, state, rt) {
  K ??= ["#11151c", 2n, 0, 0, 1, 0, 0, 0n, false, "left", 1, 0, 400, false, "=", 1, 0, false, "-", 1, 0, false, "left", 1, 0, false, "right", 1, 0, false, "up", 1, 0, false, "down", 1, 0, 2n, 0n, 0.6, "#76baca", new rt.PArray(3, new Float64Array(rt.data, 8000000, 3)), new rt.PArray(3, new Float64Array(rt.data, 8000024, 3)), "#8a93a0", 1, new rt.PArray(3, new Float64Array(rt.data, 8000048, 3)), new rt.PArray(3, new Float64Array(rt.data, 8000072, 3)), "#8a93a0", 1, new rt.PArray(3, new Float64Array(rt.data, 8000096, 39)), new rt.PArray(3, new Float64Array(rt.data, 8000408, 39)), "#8a93a0", 1];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79;
  let l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95;
  let l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111;
  let l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127;
  let l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143;
  let l144, l145, l146, l147, l148, l149, l150, l151, l152, l153, l154, l155, l156;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = BigInt(input.size[0]);
    at = 3; l2 = K[1];
    at = 4; l3 = rt.div(l1, l2);
    at = 11; { const c11 = cell(state, 0, "", input.tick);
      if (c11.value !== undefined) l10 = c11.value; else {
        at = 5; l4 = K[2];
        at = 6; l5 = K[3];
        at = 7; l6 = K[4];
        at = 8; l7 = K[5];
        at = 9; l8 = K[6];
        at = 10; l9 = new Map([["x", l4], ["y", l5], ["zoom", l6], ["px", l7], ["py", l8]]);
        l10 = l9;
      } }
    at = 12; { const c12 = cell(state, 0, "", input.tick); l11 = c12.pending !== undefined; l12 = l11 ? c12.pending : null; }
    at = 13; if (rt.truthy(l11)) {
      at = 14; l13 = l12;
    } else {
      at = 15; l14 = input.dt;
      at = 16; l15 = K[7];
      at = 17; l16 = rt.max(l15, l14);
      at = 18; l17 = rt.frameValue(input);
      at = 19; l18 = field(rt, l17, "buttons");
      at = 20; l19 = K[8];
      at = 21; l20 = l19;
      at = 22; {
        const s22 = l18, c22 = Array.isArray(s22) ? s22.length : Number(rt.array_count(s22));
        if (c22 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i22 = 0; i22 < c22; i22++) {
          l21 = BigInt(i22); l22 = Array.isArray(s22) ? s22[i22] : rt.array_nth(s22, i22);
          at = 23; l23 = K[9];
          at = 24; l24 = rt.equal(l22, l23);
          at = 25; l25 = rt.or(l20, l24);
          at = 26; l20 = l25;
        }
      }
      at = 27; if (rt.truthy(l20)) {
        at = 28; l27 = K[10];
        at = 29; l26 = l27;
      } else {
        at = 30; l28 = K[11];
        at = 31; l26 = l28;
      }
      at = 32; l29 = K[12];
      at = 33; l30 = rt.mul(l16, l29);
      at = 34; l31 = field(rt, l10, "zoom");
      at = 35; l32 = rt.frameValue(input);
      at = 36; l33 = field(rt, l32, "keys");
      at = 37; l34 = K[13];
      at = 38; l35 = l34;
      at = 39; {
        const s39 = l33, c39 = Array.isArray(s39) ? s39.length : Number(rt.array_count(s39));
        if (c39 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i39 = 0; i39 < c39; i39++) {
          l36 = BigInt(i39); l37 = Array.isArray(s39) ? s39[i39] : rt.array_nth(s39, i39);
          at = 40; l38 = K[14];
          at = 41; l39 = rt.equal(l37, l38);
          at = 42; l40 = rt.or(l35, l39);
          at = 43; l35 = l40;
        }
      }
      at = 44; if (rt.truthy(l35)) {
        at = 45; l42 = K[15];
        at = 46; l41 = l42;
      } else {
        at = 47; l43 = K[16];
        at = 48; l41 = l43;
      }
      at = 49; l44 = rt.frameValue(input);
      at = 50; l45 = field(rt, l44, "keys");
      at = 51; l46 = K[17];
      at = 52; l47 = l46;
      at = 53; {
        const s53 = l45, c53 = Array.isArray(s53) ? s53.length : Number(rt.array_count(s53));
        if (c53 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i53 = 0; i53 < c53; i53++) {
          l48 = BigInt(i53); l49 = Array.isArray(s53) ? s53[i53] : rt.array_nth(s53, i53);
          at = 54; l50 = K[18];
          at = 55; l51 = rt.equal(l49, l50);
          at = 56; l52 = rt.or(l47, l51);
          at = 57; l47 = l52;
        }
      }
      at = 58; if (rt.truthy(l47)) {
        at = 59; l54 = K[19];
        at = 60; l53 = l54;
      } else {
        at = 61; l55 = K[20];
        at = 62; l53 = l55;
      }
      at = 63; l56 = rt.sub(l41, l53);
      at = 64; l57 = rt.mul(l16, l56);
      at = 65; l58 = rt.exp(l57);
      at = 66; l59 = rt.mul(l31, l58);
      at = 67; l60 = field(rt, l10, "x");
      at = 68; l61 = input.pointer[0];
      at = 69; l62 = field(rt, l10, "px");
      at = 70; l63 = rt.sub(l61, l62);
      at = 71; l64 = rt.mul(l26, l63);
      at = 72; l65 = rt.add(l60, l64);
      at = 73; l66 = rt.frameValue(input);
      at = 74; l67 = field(rt, l66, "keys");
      at = 75; l68 = K[21];
      at = 76; l69 = l68;
      at = 77; {
        const s77 = l67, c77 = Array.isArray(s77) ? s77.length : Number(rt.array_count(s77));
        if (c77 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i77 = 0; i77 < c77; i77++) {
          l70 = BigInt(i77); l71 = Array.isArray(s77) ? s77[i77] : rt.array_nth(s77, i77);
          at = 78; l72 = K[22];
          at = 79; l73 = rt.equal(l71, l72);
          at = 80; l74 = rt.or(l69, l73);
          at = 81; l69 = l74;
        }
      }
      at = 82; if (rt.truthy(l69)) {
        at = 83; l76 = K[23];
        at = 84; l75 = l76;
      } else {
        at = 85; l77 = K[24];
        at = 86; l75 = l77;
      }
      at = 87; l78 = rt.frameValue(input);
      at = 88; l79 = field(rt, l78, "keys");
      at = 89; l80 = K[25];
      at = 90; l81 = l80;
      at = 91; {
        const s91 = l79, c91 = Array.isArray(s91) ? s91.length : Number(rt.array_count(s91));
        if (c91 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i91 = 0; i91 < c91; i91++) {
          l82 = BigInt(i91); l83 = Array.isArray(s91) ? s91[i91] : rt.array_nth(s91, i91);
          at = 92; l84 = K[26];
          at = 93; l85 = rt.equal(l83, l84);
          at = 94; l86 = rt.or(l81, l85);
          at = 95; l81 = l86;
        }
      }
      at = 96; if (rt.truthy(l81)) {
        at = 97; l88 = K[27];
        at = 98; l87 = l88;
      } else {
        at = 99; l89 = K[28];
        at = 100; l87 = l89;
      }
      at = 101; l90 = rt.sub(l75, l87);
      at = 102; l91 = rt.mul(l30, l90);
      at = 103; l92 = rt.add(l65, l91);
      at = 104; l93 = field(rt, l10, "y");
      at = 105; l94 = input.pointer[1];
      at = 106; l95 = field(rt, l10, "py");
      at = 107; l96 = rt.sub(l94, l95);
      at = 108; l97 = rt.mul(l26, l96);
      at = 109; l98 = rt.add(l93, l97);
      at = 110; l99 = rt.frameValue(input);
      at = 111; l100 = field(rt, l99, "keys");
      at = 112; l101 = K[29];
      at = 113; l102 = l101;
      at = 114; {
        const s114 = l100, c114 = Array.isArray(s114) ? s114.length : Number(rt.array_count(s114));
        if (c114 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i114 = 0; i114 < c114; i114++) {
          l103 = BigInt(i114); l104 = Array.isArray(s114) ? s114[i114] : rt.array_nth(s114, i114);
          at = 115; l105 = K[30];
          at = 116; l106 = rt.equal(l104, l105);
          at = 117; l107 = rt.or(l102, l106);
          at = 118; l102 = l107;
        }
      }
      at = 119; if (rt.truthy(l102)) {
        at = 120; l109 = K[31];
        at = 121; l108 = l109;
      } else {
        at = 122; l110 = K[32];
        at = 123; l108 = l110;
      }
      at = 124; l111 = rt.frameValue(input);
      at = 125; l112 = field(rt, l111, "keys");
      at = 126; l113 = K[33];
      at = 127; l114 = l113;
      at = 128; {
        const s128 = l112, c128 = Array.isArray(s128) ? s128.length : Number(rt.array_count(s128));
        if (c128 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i128 = 0; i128 < c128; i128++) {
          l115 = BigInt(i128); l116 = Array.isArray(s128) ? s128[i128] : rt.array_nth(s128, i128);
          at = 129; l117 = K[34];
          at = 130; l118 = rt.equal(l116, l117);
          at = 131; l119 = rt.or(l114, l118);
          at = 132; l114 = l119;
        }
      }
      at = 133; if (rt.truthy(l114)) {
        at = 134; l121 = K[35];
        at = 135; l120 = l121;
      } else {
        at = 136; l122 = K[36];
        at = 137; l120 = l122;
      }
      at = 138; l123 = rt.sub(l108, l120);
      at = 139; l124 = rt.mul(l30, l123);
      at = 140; l125 = rt.add(l98, l124);
      at = 141; l126 = rt.coerce("float", l92);
      at = 142; l127 = rt.coerce("float", l125);
      at = 143; l128 = rt.coerce("float", l59);
      at = 144; l129 = new Map([["x", l126], ["y", l127], ["zoom", l128], ["px", l61], ["py", l94]]);
      at = 145; cell(state, 0, "", input.tick).pending = l129;
      at = 146; l13 = l129;
    }
    at = 147; l130 = field(rt, l13, "x");
    at = 148; l131 = rt.add(l3, l130);
    at = 149; l132 = rt.int(l131);
    at = 150; l133 = BigInt(input.size[1]);
    at = 151; l134 = K[37];
    at = 152; l135 = rt.div(l133, l134);
    at = 153; l136 = field(rt, l13, "y");
    at = 154; l137 = rt.add(l135, l136);
    at = 155; l138 = rt.int(l137);
    at = 156; l139 = K[38];
    at = 157; l140 = Float64Array.of(rt.float(l132), rt.float(l138), rt.float(l139));
    at = 158; transform(rt, "translate", l140);
    at = 159; l141 = field(rt, l13, "zoom");
    at = 160; transform(rt, "scale", l141);
    at = 161; l142 = await rt.gpuArray(0);
    at = 162; l143 = K[39];
    at = 163; l144 = K[40];
    at = 164; markSet(rt, true, "circle", {positions: l142, radius: l143, fill: l144});
    at = 165; l145 = K[41];
    at = 166; l146 = K[42];
    at = 167; l147 = K[43];
    at = 168; l148 = K[44];
    at = 169; markSet(rt, true, "line", {from: l145, to: l146, color: l147, width: l148});
    at = 170; l149 = K[45];
    at = 171; l150 = K[46];
    at = 172; l151 = K[47];
    at = 173; l152 = K[48];
    at = 174; markSet(rt, true, "line", {from: l149, to: l150, color: l151, width: l152});
    at = 175; l153 = K[49];
    at = 176; l154 = K[50];
    at = 177; l155 = K[51];
    at = 178; l156 = K[52];
    at = 179; markSet(rt, true, "line", {from: l153, to: l154, color: l155, width: l156});
    at = 180; emit(rt, {type: "pop_transform"});
    at = 181; emit(rt, {type: "pop_transform"});
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [{"source": "function region(inputs, uniforms, outputs, start, stop) {\n  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n  let RH = 0;\n  const P32 = 4294967296;\n  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };\n  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };\n  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);\n  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };\n  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };\n  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;\n    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);\n    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };\n  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };\n  const div64 = (ah, al, bh, bl, mod) => {\n    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');\n    const a = ah * P32 + al, b = bh * P32 + bl;\n    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));\n    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };\n  const shift64 = (ah, al, b, right) => {\n    if (b < 0 || b > 63) fail('E_RANGE', 'Integer shift count must be within [0, 63].');\n    if (b === 0) { RH = ah; return al; }\n    if (right) { if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }\n    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }\n    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };\n  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n  const in0 = inputs[0];\n  const out0 = outputs[0];\n  for (let i = start; i < stop; i++) {\n    const o = i;\n    const j0 = i;\n    let r0;\n    r0 = Number(in0[j0 * 1 + 0]);\n    if (!Number.isFinite(r0)) return 1;\n    let r1;\n    r1 = 12.9898;\n    let r2;\n    r2 = r0 * r1;\n    if (!Number.isFinite(r2)) return 1;\n    let r3;\n    r3 = 1;\n    let r4;\n    r4 = 78.233;\n    let r5;\n    r5 = r2 + r4;\n    if (!Number.isFinite(r5)) return 1;\n    let r6;\n    r6 = Math.sin(r5);\n    if (!Number.isFinite(r6)) return 1;\n    let r7;\n    r7 = 43758.5453;\n    let r8;\n    r8 = r6 * r7;\n    if (!Number.isFinite(r8)) return 1;\n    let r9;\n    r9 = r3 === 0 ? 0 : ((r8 % r3) + r3) % r3;\n    if (!Number.isFinite(r9)) return 1;\n    let r10;\n    r10 = Math.abs(r9);\n    let r11;\n    r11 = 2;\n    let r12;\n    r12 = 156.466;\n    let r13;\n    r13 = r2 + r12;\n    if (!Number.isFinite(r13)) return 1;\n    let r14;\n    r14 = Math.sin(r13);\n    if (!Number.isFinite(r14)) return 1;\n    let r15;\n    r15 = r14 * r7;\n    if (!Number.isFinite(r15)) return 1;\n    let r16;\n    r16 = r3 === 0 ? 0 : ((r15 % r3) + r3) % r3;\n    if (!Number.isFinite(r16)) return 1;\n    let r17;\n    r17 = Math.abs(r16);\n    let r18;\n    r18 = r10 + r17;\n    if (!Number.isFinite(r18)) return 1;\n    let r19;\n    r19 = r18 - r3;\n    if (!Number.isFinite(r19)) return 1;\n    let r20;\n    r20 = 3;\n    let r21;\n    r21 = 234.699;\n    let r22;\n    r22 = r2 + r21;\n    if (!Number.isFinite(r22)) return 1;\n    let r23;\n    r23 = Math.sin(r22);\n    if (!Number.isFinite(r23)) return 1;\n    let r24;\n    r24 = r23 * r7;\n    if (!Number.isFinite(r24)) return 1;\n    let r25;\n    r25 = r3 === 0 ? 0 : ((r24 % r3) + r3) % r3;\n    if (!Number.isFinite(r25)) return 1;\n    let r26;\n    r26 = Math.abs(r25);\n    let r27;\n    r27 = 0.5;\n    let r28;\n    r28 = r26 - r27;\n    if (!Number.isFinite(r28)) return 1;\n    let r29;\n    r29 = Number(uniforms[0][0]);\n    if (!Number.isFinite(r29)) return 1;\n    let r30;\n    r30 = r19 * r29;\n    if (!Number.isFinite(r30)) return 1;\n    let r31;\n    r31 = 0.1;\n    let r32;\n    r32 = uniforms[1][32];\n    if (!Number.isFinite(r32)) return 1;\n    let r33;\n    r33 = 6;\n    let r34;\n    r34 = r19 * r33;\n    if (!Number.isFinite(r34)) return 1;\n    let r35;\n    r35 = r32 + r34;\n    if (!Number.isFinite(r35)) return 1;\n    let r36;\n    r36 = Math.sin(r35);\n    if (!Number.isFinite(r36)) return 1;\n    let r37;\n    r37 = r31 * r36;\n    if (!Number.isFinite(r37)) return 1;\n    let r38;\n    r38 = r28 + r37;\n    if (!Number.isFinite(r38)) return 1;\n    let r39;\n    r39 = r38 * r29;\n    if (!Number.isFinite(r39)) return 1;\n    let r40;\n    r40 = 0;\n    if (!Number.isFinite(r30)) return 1;\n    out0[o * 3 + 0] = r30;\n    if (!Number.isFinite(r39)) return 1;\n    out0[o * 3 + 1] = r39;\n    if (!Number.isFinite(r40)) return 1;\n    out0[o * 3 + 2] = r40;\n  }\n  return 0;\n}\n", "count": 1000000, "kept": 1000000, "inputs": [[0, 1000000, false]], "uniforms": [[300]], "frame": [[32, "t", false]], "length": 41, "width": 3, "integer": false, "kernel": "k0", "gpu": {"output": 0}}];
export const gpu = {"k0": {"count": 1000000, "inputs": [[0, 1000000, false]], "uniforms": [[300]], "frame": [[32, "t", false]]}};
