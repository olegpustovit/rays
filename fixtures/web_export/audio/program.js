// Rays web export: frame program. frame(input, state, rt); see crates/flow_web/src/js.rs.
let K;
const M = new Map();
const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
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
  K ??= ["#111827", Float64Array.of(40, 60, 0), "Press 1–8 or click a key", Float64Array.of(1, 1, 1), 16n, Float64Array.of(40, 150, 0), Float64Array.of(64, 160, 0), -1n, 0n, 0n, 4096n, 0n, 0n, "key-pressed", "1", 0n, "key-pressed", "2", 1n, "key-pressed", "3", 2n, "key-pressed", "4", 3n, "key-pressed", "5", 4n, "key-pressed", "6", 5n, "key-pressed", "7", 6n, "key-pressed", "8", 7n, "pointer-pressed", "left", 40n, 600n, 150n, 310n, 40n, 70n, 7n, 1n, 1n, 0n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(110, 150, 0), Float64Array.of(64, 160, 0), 1n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(180, 150, 0), Float64Array.of(64, 160, 0), 2n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(250, 150, 0), Float64Array.of(64, 160, 0), 3n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(320, 150, 0), Float64Array.of(64, 160, 0), 4n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(390, 150, 0), Float64Array.of(64, 160, 0), 5n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(460, 150, 0), Float64Array.of(64, 160, 0), 6n, "#22d3ee", "#f8fafc", "#0f172a", Float64Array.of(530, 150, 0), Float64Array.of(64, 160, 0), 7n, "#22d3ee", "#f8fafc", "#0f172a"];
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
  let l192, l193, l194, l195, l196, l197, l198, l199, l200, l201, l202, l203, l204, l205, l206;
  let at = 0;
  try {
    at = 0; l0 = K[0];
    at = 1; { const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], [size[0], size[1]], [0, size[1]]], rgba(rt, l0), null)); }
    at = 2; l1 = K[1];
    at = 3; l2 = K[2];
    at = 4; l3 = K[3];
    at = 5; l4 = K[4];
    at = 6; if (typeof l2 !== "string") rt.fail("E_TYPE", "Drawing text is text."); else if (l2 !== "") { const [x, y] = point(rt, l1); emit(rt, {type: "text", x, y, size: Number(rt.coerce("int", l4)), color: ink(rt, l3), text: l2}); }
    at = 7; l5 = K[5];
    at = 8; l6 = K[6];
    at = 9; l7 = K[7];
    at = 10; l8 = l7;
    at = 11; l9 = K[8];
    at = 12; l10 = l9;
    at = 13; l11 = K[9];
    at = 14; l12 = l11;
    at = 15; l13 = rt.frameValue(input);
    at = 16; l14 = field(rt, l13, "events");
    { const k17 = [l14], h17 = rt.recall(M, 17, k17);
    if (h17) [l8, l10, l12, l15, l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47, l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63, l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79, l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95, l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111, l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127, l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143, l144] = h17; else {
      at = 17; {
        const s17 = l14, c17 = Array.isArray(s17) ? s17.length : Number(rt.array_count(s17));
        if (c17 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i17 = 0; i17 < c17; i17++) {
          l15 = BigInt(i17); l16 = Array.isArray(s17) ? s17[i17] : rt.array_nth(s17, i17);
          at = 18; l17 = K[10];
          at = 19; l18 = rt.ge(l10, l17);
          at = 20; if (rt.truthy(l18)) {
            at = 21; l20 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 22; l19 = l20;
          } else {
            at = 23; l21 = K[11];
            at = 24; l19 = l21;
          }
          at = 25; l22 = K[12];
          at = 26; l23 = rt.ge(l8, l22);
          at = 27; if (rt.truthy(l23)) {
            at = 28; l24 = l8;
          } else {
            at = 29; l25 = field(rt, l16, "kind");
            at = 30; l26 = K[13];
            at = 31; l27 = rt.equal(l25, l26);
            at = 32; l28 = field(rt, l16, "key");
            at = 33; l29 = K[14];
            at = 34; l30 = rt.equal(l28, l29);
            at = 35; l31 = rt.and(l27, l30);
            at = 36; if (rt.truthy(l31)) {
              at = 37; l33 = K[15];
              at = 38; l32 = l33;
            } else {
              at = 39; l34 = field(rt, l16, "kind");
              at = 40; l35 = K[16];
              at = 41; l36 = rt.equal(l34, l35);
              at = 42; l37 = field(rt, l16, "key");
              at = 43; l38 = K[17];
              at = 44; l39 = rt.equal(l37, l38);
              at = 45; l40 = rt.and(l36, l39);
              at = 46; if (rt.truthy(l40)) {
                at = 47; l42 = K[18];
                at = 48; l41 = l42;
              } else {
                at = 49; l43 = field(rt, l16, "kind");
                at = 50; l44 = K[19];
                at = 51; l45 = rt.equal(l43, l44);
                at = 52; l46 = field(rt, l16, "key");
                at = 53; l47 = K[20];
                at = 54; l48 = rt.equal(l46, l47);
                at = 55; l49 = rt.and(l45, l48);
                at = 56; if (rt.truthy(l49)) {
                  at = 57; l51 = K[21];
                  at = 58; l50 = l51;
                } else {
                  at = 59; l52 = field(rt, l16, "kind");
                  at = 60; l53 = K[22];
                  at = 61; l54 = rt.equal(l52, l53);
                  at = 62; l55 = field(rt, l16, "key");
                  at = 63; l56 = K[23];
                  at = 64; l57 = rt.equal(l55, l56);
                  at = 65; l58 = rt.and(l54, l57);
                  at = 66; if (rt.truthy(l58)) {
                    at = 67; l60 = K[24];
                    at = 68; l59 = l60;
                  } else {
                    at = 69; l61 = field(rt, l16, "kind");
                    at = 70; l62 = K[25];
                    at = 71; l63 = rt.equal(l61, l62);
                    at = 72; l64 = field(rt, l16, "key");
                    at = 73; l65 = K[26];
                    at = 74; l66 = rt.equal(l64, l65);
                    at = 75; l67 = rt.and(l63, l66);
                    at = 76; if (rt.truthy(l67)) {
                      at = 77; l69 = K[27];
                      at = 78; l68 = l69;
                    } else {
                      at = 79; l70 = field(rt, l16, "kind");
                      at = 80; l71 = K[28];
                      at = 81; l72 = rt.equal(l70, l71);
                      at = 82; l73 = field(rt, l16, "key");
                      at = 83; l74 = K[29];
                      at = 84; l75 = rt.equal(l73, l74);
                      at = 85; l76 = rt.and(l72, l75);
                      at = 86; if (rt.truthy(l76)) {
                        at = 87; l78 = K[30];
                        at = 88; l77 = l78;
                      } else {
                        at = 89; l79 = field(rt, l16, "kind");
                        at = 90; l80 = K[31];
                        at = 91; l81 = rt.equal(l79, l80);
                        at = 92; l82 = field(rt, l16, "key");
                        at = 93; l83 = K[32];
                        at = 94; l84 = rt.equal(l82, l83);
                        at = 95; l85 = rt.and(l81, l84);
                        at = 96; if (rt.truthy(l85)) {
                          at = 97; l87 = K[33];
                          at = 98; l86 = l87;
                        } else {
                          at = 99; l88 = field(rt, l16, "kind");
                          at = 100; l89 = K[34];
                          at = 101; l90 = rt.equal(l88, l89);
                          at = 102; l91 = field(rt, l16, "key");
                          at = 103; l92 = K[35];
                          at = 104; l93 = rt.equal(l91, l92);
                          at = 105; l94 = rt.and(l90, l93);
                          at = 106; if (rt.truthy(l94)) {
                            at = 107; l96 = K[36];
                            at = 108; l95 = l96;
                          } else {
                            at = 109; l97 = field(rt, l16, "kind");
                            at = 110; l98 = K[37];
                            at = 111; l99 = rt.equal(l97, l98);
                            at = 112; l100 = field(rt, l16, "button");
                            at = 113; l101 = K[38];
                            at = 114; l102 = rt.equal(l100, l101);
                            at = 115; l103 = rt.and(l99, l102);
                            at = 116; l104 = field(rt, l16, "position");
                            at = 117; l105 = field(rt, l104, "x");
                            at = 118; l106 = K[39];
                            at = 119; l107 = rt.ge(l105, l106);
                            at = 120; l108 = field(rt, l16, "position");
                            at = 121; l109 = field(rt, l108, "x");
                            at = 122; l110 = K[40];
                            at = 123; l111 = rt.lt(l109, l110);
                            at = 124; l112 = rt.and(l107, l111);
                            at = 125; l113 = field(rt, l16, "position");
                            at = 126; l114 = field(rt, l113, "y");
                            at = 127; l115 = K[41];
                            at = 128; l116 = rt.ge(l114, l115);
                            at = 129; l117 = field(rt, l16, "position");
                            at = 130; l118 = field(rt, l117, "y");
                            at = 131; l119 = K[42];
                            at = 132; l120 = rt.lt(l118, l119);
                            at = 133; l121 = rt.and(l116, l120);
                            at = 134; l122 = rt.and(l112, l121);
                            at = 135; l123 = rt.and(l103, l122);
                            at = 136; if (rt.truthy(l123)) {
                              at = 137; l125 = field(rt, l16, "position");
                              at = 138; l126 = field(rt, l125, "x");
                              at = 139; l127 = K[43];
                              at = 140; l128 = rt.sub(l126, l127);
                              at = 141; l129 = K[44];
                              at = 142; l130 = rt.div(l128, l129);
                              at = 143; l131 = rt.int(l130);
                              at = 144; l132 = K[45];
                              at = 145; l133 = rt.min(l132, l131);
                              at = 146; l124 = l133;
                            } else {
                              at = 147; l124 = l8;
                            }
                            at = 148; l95 = l124;
                          }
                          at = 149; l86 = l95;
                        }
                        at = 150; l77 = l86;
                      }
                      at = 151; l68 = l77;
                    }
                    at = 152; l59 = l68;
                  }
                  at = 153; l50 = l59;
                }
                at = 154; l41 = l50;
              }
              at = 155; l32 = l41;
            }
            at = 156; l24 = l32;
          }
          at = 157; l134 = rt.coerce("float", l24);
          at = 158; l135 = rt.equal(l24, l134);
          at = 159; if (rt.truthy(l135)) {
            at = 160; l136 = l24;
          } else {
            at = 161; l137 = rt.coerce("float", l8);
            at = 162; l138 = rt.equal(l8, l137);
            at = 163; if (rt.truthy(l138)) {
              at = 164; l140 = rt.coerce("float", l24);
              at = 165; l139 = l140;
            } else {
              at = 166; l139 = l24;
            }
            at = 167; l136 = l139;
          }
          at = 168; l8 = l136;
          at = 169; l141 = K[46];
          at = 170; l142 = rt.add(l12, l141);
          at = 171; l12 = l142;
          at = 172; l143 = K[47];
          at = 173; l144 = rt.add(l10, l143);
          at = 174; l10 = l144;
        }
      }
    rt.keep(M, 17, k17, [l8, l10, l12, l15, l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47, l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63, l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79, l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95, l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111, l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127, l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143, l144]); } }
    at = 175; l145 = K[48];
    at = 176; l146 = rt.eq(l8, l145);
    at = 177; if (rt.truthy(l146)) {
      at = 178; l148 = K[49];
      at = 179; l147 = l148;
    } else {
      at = 180; l149 = K[50];
      at = 181; l147 = l149;
    }
    at = 182; l150 = K[51];
    at = 183; markSet(rt, false, "rect", {at: l5, size: l6, fill: l147, stroke: l150});
    at = 184; l151 = K[52];
    at = 185; l152 = K[53];
    at = 186; l153 = K[54];
    at = 187; l154 = rt.eq(l8, l153);
    at = 188; if (rt.truthy(l154)) {
      at = 189; l156 = K[55];
      at = 190; l155 = l156;
    } else {
      at = 191; l157 = K[56];
      at = 192; l155 = l157;
    }
    at = 193; l158 = K[57];
    at = 194; markSet(rt, false, "rect", {at: l151, size: l152, fill: l155, stroke: l158});
    at = 195; l159 = K[58];
    at = 196; l160 = K[59];
    at = 197; l161 = K[60];
    at = 198; l162 = rt.eq(l8, l161);
    at = 199; if (rt.truthy(l162)) {
      at = 200; l164 = K[61];
      at = 201; l163 = l164;
    } else {
      at = 202; l165 = K[62];
      at = 203; l163 = l165;
    }
    at = 204; l166 = K[63];
    at = 205; markSet(rt, false, "rect", {at: l159, size: l160, fill: l163, stroke: l166});
    at = 206; l167 = K[64];
    at = 207; l168 = K[65];
    at = 208; l169 = K[66];
    at = 209; l170 = rt.eq(l8, l169);
    at = 210; if (rt.truthy(l170)) {
      at = 211; l172 = K[67];
      at = 212; l171 = l172;
    } else {
      at = 213; l173 = K[68];
      at = 214; l171 = l173;
    }
    at = 215; l174 = K[69];
    at = 216; markSet(rt, false, "rect", {at: l167, size: l168, fill: l171, stroke: l174});
    at = 217; l175 = K[70];
    at = 218; l176 = K[71];
    at = 219; l177 = K[72];
    at = 220; l178 = rt.eq(l8, l177);
    at = 221; if (rt.truthy(l178)) {
      at = 222; l180 = K[73];
      at = 223; l179 = l180;
    } else {
      at = 224; l181 = K[74];
      at = 225; l179 = l181;
    }
    at = 226; l182 = K[75];
    at = 227; markSet(rt, false, "rect", {at: l175, size: l176, fill: l179, stroke: l182});
    at = 228; l183 = K[76];
    at = 229; l184 = K[77];
    at = 230; l185 = K[78];
    at = 231; l186 = rt.eq(l8, l185);
    at = 232; if (rt.truthy(l186)) {
      at = 233; l188 = K[79];
      at = 234; l187 = l188;
    } else {
      at = 235; l189 = K[80];
      at = 236; l187 = l189;
    }
    at = 237; l190 = K[81];
    at = 238; markSet(rt, false, "rect", {at: l183, size: l184, fill: l187, stroke: l190});
    at = 239; l191 = K[82];
    at = 240; l192 = K[83];
    at = 241; l193 = K[84];
    at = 242; l194 = rt.eq(l8, l193);
    at = 243; if (rt.truthy(l194)) {
      at = 244; l196 = K[85];
      at = 245; l195 = l196;
    } else {
      at = 246; l197 = K[86];
      at = 247; l195 = l197;
    }
    at = 248; l198 = K[87];
    at = 249; markSet(rt, false, "rect", {at: l191, size: l192, fill: l195, stroke: l198});
    at = 250; l199 = K[88];
    at = 251; l200 = K[89];
    at = 252; l201 = K[90];
    at = 253; l202 = rt.eq(l8, l201);
    at = 254; if (rt.truthy(l202)) {
      at = 255; l204 = K[91];
      at = 256; l203 = l204;
    } else {
      at = 257; l205 = K[92];
      at = 258; l203 = l205;
    }
    at = 259; l206 = K[93];
    at = 260; markSet(rt, false, "rect", {at: l199, size: l200, fill: l203, stroke: l206});
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return [];
}

export const regions = [];
