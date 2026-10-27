// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
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
export function frame(input, state, rt) {
  K ??= [-1e3, 0n, 0n, 4096n, [0n, 1n], 4096n, 0n, 0n, 1n, true, 0n, false, 0n, 2n, true, 1n, 0n, 0n, 1n, 0n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n], 4096n, 0n, 1n, 1n, true, 0n, false, 1n, 2n, true, 1n, 0n, 10n, 1n, 0n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n], 4096n, 0n, 2n, 1n, true, 0n, false, 2n, 2n, true, 1n, 0n, 20n, 1n, 0n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n], 4096n, 0n, 0n, 1n, true, 0n, false, 0n, 2n, true, 1n, 0n, 0n, 1n, 0n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n], 4096n, 0n, 1n, 1n, true, 0n, false, 1n, 2n, true, 1n, 0n, 10n, 1n, 0n, 1n, 0n, 0n, 0n, 4096n, [0n, 1n], 4096n, 0n, 2n, 1n, true, 0n, false, 2n, 2n, true, 1n, 0n, 20n, 1n, 0n, 1n, 0n];
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
  let l272, l273, l274, l275, l276, l277, l278, l279, l280, l281;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l4 = K[1];
      at = 5; l5 = l4;
      at = 6; l6 = K[2];
      at = 7; l7 = l6;
      at = 8; l8 = K[3];
      at = 9; l9 = rt.sequence_build(l8);
      at = 10; l10 = K[4];
      at = 11; {
        const s11 = l10, c11 = Array.isArray(s11) ? s11.length : Number(rt.array_count(s11));
        if (c11 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i11 = 0; i11 < c11; i11++) {
          l11 = BigInt(i11); l12 = Array.isArray(s11) ? s11[i11] : rt.array_nth(s11, i11);
          at = 12; l13 = K[5];
          at = 13; l14 = rt.ge(l5, l13);
          at = 14; if (rt.truthy(l14)) {
            at = 15; l16 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 16; l15 = l16;
          } else {
            at = 17; l17 = K[6];
            at = 18; l15 = l17;
          }
          at = 19; l18 = K[7];
          at = 20; l19 = K[8];
          at = 21; l20 = rt.eq(l18, l19);
          at = 22; l21 = K[9];
          at = 23; l22 = rt.and(l21, l20);
          at = 24; l23 = K[10];
          at = 25; l24 = rt.eq(l5, l23);
          at = 26; l25 = rt.and(l22, l24);
          at = 27; l26 = K[11];
          at = 28; l27 = rt.or(l26, l25);
          at = 29; l28 = K[12];
          at = 30; l29 = K[13];
          at = 31; l30 = rt.eq(l28, l29);
          at = 32; l31 = K[14];
          at = 33; l32 = rt.and(l31, l30);
          at = 34; l33 = K[15];
          at = 35; l34 = rt.eq(l5, l33);
          at = 36; l35 = rt.and(l32, l34);
          at = 37; l36 = rt.or(l27, l35);
          at = 38; if (rt.truthy(l36)) {
            at = 39; l38 = K[16];
            at = 40; l37 = l38;
          } else {
            at = 41; l39 = K[17];
            at = 42; l40 = rt.add(l39, l12);
            at = 43; l41 = rt.add(l40, l0);
            at = 44; store(rt, l9, l7, l41);
            at = 45; l42 = K[18];
            at = 46; l43 = rt.add(l7, l42);
            at = 47; l7 = l43;
            at = 48; l44 = K[19];
            at = 49; l37 = l44;
          }
          at = 50; l45 = K[20];
          at = 51; l46 = rt.add(l5, l45);
          at = 52; l5 = l46;
        }
      }
      at = 53; l47 = K[21];
      at = 54; l48 = rt.sequence_slice(l9, l47, l7);
      at = 55; l49 = rt.sequence_concat(l48);
      at = 56; l50 = K[22];
      at = 57; l51 = l50;
      at = 58; l52 = K[23];
      at = 59; l53 = l52;
      at = 60; l54 = K[24];
      at = 61; l55 = rt.sequence_build(l54);
      at = 62; l56 = K[25];
      at = 63; {
        const s63 = l56, c63 = Array.isArray(s63) ? s63.length : Number(rt.array_count(s63));
        if (c63 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i63 = 0; i63 < c63; i63++) {
          l57 = BigInt(i63); l58 = Array.isArray(s63) ? s63[i63] : rt.array_nth(s63, i63);
          at = 64; l59 = K[26];
          at = 65; l60 = rt.ge(l51, l59);
          at = 66; if (rt.truthy(l60)) {
            at = 67; l62 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 68; l61 = l62;
          } else {
            at = 69; l63 = K[27];
            at = 70; l61 = l63;
          }
          at = 71; l64 = K[28];
          at = 72; l65 = K[29];
          at = 73; l66 = rt.eq(l64, l65);
          at = 74; l67 = K[30];
          at = 75; l68 = rt.and(l67, l66);
          at = 76; l69 = K[31];
          at = 77; l70 = rt.eq(l51, l69);
          at = 78; l71 = rt.and(l68, l70);
          at = 79; l72 = K[32];
          at = 80; l73 = rt.or(l72, l71);
          at = 81; l74 = K[33];
          at = 82; l75 = K[34];
          at = 83; l76 = rt.eq(l74, l75);
          at = 84; l77 = K[35];
          at = 85; l78 = rt.and(l77, l76);
          at = 86; l79 = K[36];
          at = 87; l80 = rt.eq(l51, l79);
          at = 88; l81 = rt.and(l78, l80);
          at = 89; l82 = rt.or(l73, l81);
          at = 90; if (rt.truthy(l82)) {
            at = 91; l84 = K[37];
            at = 92; l83 = l84;
          } else {
            at = 93; l85 = K[38];
            at = 94; l86 = rt.add(l85, l58);
            at = 95; l87 = rt.add(l86, l0);
            at = 96; store(rt, l55, l53, l87);
            at = 97; l88 = K[39];
            at = 98; l89 = rt.add(l53, l88);
            at = 99; l53 = l89;
            at = 100; l90 = K[40];
            at = 101; l83 = l90;
          }
          at = 102; l91 = K[41];
          at = 103; l92 = rt.add(l51, l91);
          at = 104; l51 = l92;
        }
      }
      at = 105; l93 = K[42];
      at = 106; l94 = rt.sequence_slice(l55, l93, l53);
      at = 107; l95 = rt.sequence_concat(l94);
      at = 108; l96 = K[43];
      at = 109; l97 = l96;
      at = 110; l98 = K[44];
      at = 111; l99 = l98;
      at = 112; l100 = K[45];
      at = 113; l101 = rt.sequence_build(l100);
      at = 114; l102 = K[46];
      at = 115; {
        const s115 = l102, c115 = Array.isArray(s115) ? s115.length : Number(rt.array_count(s115));
        if (c115 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i115 = 0; i115 < c115; i115++) {
          l103 = BigInt(i115); l104 = Array.isArray(s115) ? s115[i115] : rt.array_nth(s115, i115);
          at = 116; l105 = K[47];
          at = 117; l106 = rt.ge(l97, l105);
          at = 118; if (rt.truthy(l106)) {
            at = 119; l108 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 120; l107 = l108;
          } else {
            at = 121; l109 = K[48];
            at = 122; l107 = l109;
          }
          at = 123; l110 = K[49];
          at = 124; l111 = K[50];
          at = 125; l112 = rt.eq(l110, l111);
          at = 126; l113 = K[51];
          at = 127; l114 = rt.and(l113, l112);
          at = 128; l115 = K[52];
          at = 129; l116 = rt.eq(l97, l115);
          at = 130; l117 = rt.and(l114, l116);
          at = 131; l118 = K[53];
          at = 132; l119 = rt.or(l118, l117);
          at = 133; l120 = K[54];
          at = 134; l121 = K[55];
          at = 135; l122 = rt.eq(l120, l121);
          at = 136; l123 = K[56];
          at = 137; l124 = rt.and(l123, l122);
          at = 138; l125 = K[57];
          at = 139; l126 = rt.eq(l97, l125);
          at = 140; l127 = rt.and(l124, l126);
          at = 141; l128 = rt.or(l119, l127);
          at = 142; if (rt.truthy(l128)) {
            at = 143; l130 = K[58];
            at = 144; l129 = l130;
          } else {
            at = 145; l131 = K[59];
            at = 146; l132 = rt.add(l131, l104);
            at = 147; l133 = rt.add(l132, l0);
            at = 148; store(rt, l101, l99, l133);
            at = 149; l134 = K[60];
            at = 150; l135 = rt.add(l99, l134);
            at = 151; l99 = l135;
            at = 152; l136 = K[61];
            at = 153; l129 = l136;
          }
          at = 154; l137 = K[62];
          at = 155; l138 = rt.add(l97, l137);
          at = 156; l97 = l138;
        }
      }
      at = 157; l139 = K[63];
      at = 158; l140 = rt.sequence_slice(l101, l139, l99);
      at = 159; l141 = rt.sequence_concat(l140);
      at = 160; l142 = rt.sequence_concat([l49], [l95], [l141]);
      at = 161; l3 = l142;
    } else {
      at = 162; l143 = K[64];
      at = 163; l144 = l143;
      at = 164; l145 = K[65];
      at = 165; l146 = l145;
      at = 166; l147 = K[66];
      at = 167; l148 = rt.sequence_build(l147);
      at = 168; l149 = K[67];
      at = 169; {
        const s169 = l149, c169 = Array.isArray(s169) ? s169.length : Number(rt.array_count(s169));
        if (c169 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i169 = 0; i169 < c169; i169++) {
          l150 = BigInt(i169); l151 = Array.isArray(s169) ? s169[i169] : rt.array_nth(s169, i169);
          at = 170; l152 = K[68];
          at = 171; l153 = rt.ge(l144, l152);
          at = 172; if (rt.truthy(l153)) {
            at = 173; l155 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 174; l154 = l155;
          } else {
            at = 175; l156 = K[69];
            at = 176; l154 = l156;
          }
          at = 177; l157 = K[70];
          at = 178; l158 = K[71];
          at = 179; l159 = rt.eq(l157, l158);
          at = 180; l160 = K[72];
          at = 181; l161 = rt.and(l160, l159);
          at = 182; l162 = K[73];
          at = 183; l163 = rt.eq(l144, l162);
          at = 184; l164 = rt.and(l161, l163);
          at = 185; l165 = K[74];
          at = 186; l166 = rt.or(l165, l164);
          at = 187; l167 = K[75];
          at = 188; l168 = K[76];
          at = 189; l169 = rt.eq(l167, l168);
          at = 190; l170 = K[77];
          at = 191; l171 = rt.and(l170, l169);
          at = 192; l172 = K[78];
          at = 193; l173 = rt.eq(l144, l172);
          at = 194; l174 = rt.and(l171, l173);
          at = 195; l175 = rt.or(l166, l174);
          at = 196; if (rt.truthy(l175)) {
            at = 197; l177 = K[79];
            at = 198; l176 = l177;
          } else {
            at = 199; l178 = K[80];
            at = 200; l179 = rt.add(l178, l151);
            at = 201; l180 = rt.add(l179, l0);
            at = 202; store(rt, l148, l146, l180);
            at = 203; l181 = K[81];
            at = 204; l182 = rt.add(l146, l181);
            at = 205; l146 = l182;
            at = 206; l183 = K[82];
            at = 207; l176 = l183;
          }
          at = 208; l184 = K[83];
          at = 209; l185 = rt.add(l144, l184);
          at = 210; l144 = l185;
        }
      }
      at = 211; l186 = K[84];
      at = 212; l187 = rt.sequence_slice(l148, l186, l146);
      at = 213; l188 = rt.sequence_concat(l187);
      at = 214; l189 = K[85];
      at = 215; l190 = l189;
      at = 216; l191 = K[86];
      at = 217; l192 = l191;
      at = 218; l193 = K[87];
      at = 219; l194 = rt.sequence_build(l193);
      at = 220; l195 = K[88];
      at = 221; {
        const s221 = l195, c221 = Array.isArray(s221) ? s221.length : Number(rt.array_count(s221));
        if (c221 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i221 = 0; i221 < c221; i221++) {
          l196 = BigInt(i221); l197 = Array.isArray(s221) ? s221[i221] : rt.array_nth(s221, i221);
          at = 222; l198 = K[89];
          at = 223; l199 = rt.ge(l190, l198);
          at = 224; if (rt.truthy(l199)) {
            at = 225; l201 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 226; l200 = l201;
          } else {
            at = 227; l202 = K[90];
            at = 228; l200 = l202;
          }
          at = 229; l203 = K[91];
          at = 230; l204 = K[92];
          at = 231; l205 = rt.eq(l203, l204);
          at = 232; l206 = K[93];
          at = 233; l207 = rt.and(l206, l205);
          at = 234; l208 = K[94];
          at = 235; l209 = rt.eq(l190, l208);
          at = 236; l210 = rt.and(l207, l209);
          at = 237; l211 = K[95];
          at = 238; l212 = rt.or(l211, l210);
          at = 239; l213 = K[96];
          at = 240; l214 = K[97];
          at = 241; l215 = rt.eq(l213, l214);
          at = 242; l216 = K[98];
          at = 243; l217 = rt.and(l216, l215);
          at = 244; l218 = K[99];
          at = 245; l219 = rt.eq(l190, l218);
          at = 246; l220 = rt.and(l217, l219);
          at = 247; l221 = rt.or(l212, l220);
          at = 248; if (rt.truthy(l221)) {
            at = 249; l223 = K[100];
            at = 250; l222 = l223;
          } else {
            at = 251; l224 = K[101];
            at = 252; l225 = rt.add(l224, l197);
            at = 253; l226 = rt.add(l225, l0);
            at = 254; store(rt, l194, l192, l226);
            at = 255; l227 = K[102];
            at = 256; l228 = rt.add(l192, l227);
            at = 257; l192 = l228;
            at = 258; l229 = K[103];
            at = 259; l222 = l229;
          }
          at = 260; l230 = K[104];
          at = 261; l231 = rt.add(l190, l230);
          at = 262; l190 = l231;
        }
      }
      at = 263; l232 = K[105];
      at = 264; l233 = rt.sequence_slice(l194, l232, l192);
      at = 265; l234 = rt.sequence_concat(l233);
      at = 266; l235 = K[106];
      at = 267; l236 = l235;
      at = 268; l237 = K[107];
      at = 269; l238 = l237;
      at = 270; l239 = K[108];
      at = 271; l240 = rt.sequence_build(l239);
      at = 272; l241 = K[109];
      at = 273; {
        const s273 = l241, c273 = Array.isArray(s273) ? s273.length : Number(rt.array_count(s273));
        if (c273 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i273 = 0; i273 < c273; i273++) {
          l242 = BigInt(i273); l243 = Array.isArray(s273) ? s273[i273] : rt.array_nth(s273, i273);
          at = 274; l244 = K[110];
          at = 275; l245 = rt.ge(l236, l244);
          at = 276; if (rt.truthy(l245)) {
            at = 277; l247 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 278; l246 = l247;
          } else {
            at = 279; l248 = K[111];
            at = 280; l246 = l248;
          }
          at = 281; l249 = K[112];
          at = 282; l250 = K[113];
          at = 283; l251 = rt.eq(l249, l250);
          at = 284; l252 = K[114];
          at = 285; l253 = rt.and(l252, l251);
          at = 286; l254 = K[115];
          at = 287; l255 = rt.eq(l236, l254);
          at = 288; l256 = rt.and(l253, l255);
          at = 289; l257 = K[116];
          at = 290; l258 = rt.or(l257, l256);
          at = 291; l259 = K[117];
          at = 292; l260 = K[118];
          at = 293; l261 = rt.eq(l259, l260);
          at = 294; l262 = K[119];
          at = 295; l263 = rt.and(l262, l261);
          at = 296; l264 = K[120];
          at = 297; l265 = rt.eq(l236, l264);
          at = 298; l266 = rt.and(l263, l265);
          at = 299; l267 = rt.or(l258, l266);
          at = 300; if (rt.truthy(l267)) {
            at = 301; l269 = K[121];
            at = 302; l268 = l269;
          } else {
            at = 303; l270 = K[122];
            at = 304; l271 = rt.add(l270, l243);
            at = 305; l272 = rt.add(l271, l0);
            at = 306; store(rt, l240, l238, l272);
            at = 307; l273 = K[123];
            at = 308; l274 = rt.add(l238, l273);
            at = 309; l238 = l274;
            at = 310; l275 = K[124];
            at = 311; l268 = l275;
          }
          at = 312; l276 = K[125];
          at = 313; l277 = rt.add(l236, l276);
          at = 314; l236 = l277;
        }
      }
      at = 315; l278 = K[126];
      at = 316; l279 = rt.sequence_slice(l240, l278, l238);
      at = 317; l280 = rt.sequence_concat(l279);
      at = 318; l281 = rt.sequence_concat([l188], [l234], [l280]);
      at = 319; l3 = l281;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
