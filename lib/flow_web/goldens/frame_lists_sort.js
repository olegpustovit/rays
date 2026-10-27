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
  K ??= [-1e3, 12n, 12n, 12n, [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 11n], [1n, 2n, 4n, 8n, 16n, 32n, 64n, 128n, 256n, 512n, 1024n, 2048n], 12n, 2n, 12n, 1n, 0n, 12n, 12n, true, false, 1n, 0n, 1n, 0n, 0n, 0n, 12n, 0n, 12n, [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 11n], 12n, 12n, 12n, [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 11n], [1n, 2n, 4n, 8n, 16n, 32n, 64n, 128n, 256n, 512n, 1024n, 2048n], 12n, 2n, 12n, 1n, 0n, 12n, 12n, true, false, 1n, 0n, 1n, 0n, 0n, 0n, 12n, 0n, 12n, [0n, 1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 11n]];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79;
  let l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91, l92, l93, l94, l95;
  let l96, l97, l98, l99, l100, l101, l102, l103, l104, l105, l106, l107, l108, l109, l110, l111;
  let l112, l113, l114, l115, l116, l117, l118, l119, l120, l121, l122, l123, l124, l125, l126, l127;
  let l128, l129, l130, l131, l132, l133, l134, l135, l136, l137, l138, l139, l140, l141, l142, l143;
  let l144, l145, l146, l147, l148, l149, l150, l151, l152, l153, l154, l155;
  let at = 0;
  const calls = new Int32Array(2);
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; calls[0] = 0;
      at = 5; l4 = K[1];
      at = 6; l5 = rt.sequence_build(l4);
      at = 7; l6 = K[2];
      at = 8; l7 = rt.sequence_build(l6);
      at = 9; l8 = l7;
      at = 10; l9 = K[3];
      at = 11; l10 = rt.sequence_build(l9);
      at = 12; l11 = l10;
      at = 13; l12 = K[4];
      at = 14; {
        const s14 = l12, c14 = Array.isArray(s14) ? s14.length : Number(rt.array_count(s14));
        if (c14 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i14 = 0; i14 < c14; i14++) {
          l13 = BigInt(i14); l14 = Array.isArray(s14) ? s14[i14] : rt.array_nth(s14, i14);
          at = 15; l15 = BigInt(calls[0]++);
          at = 16; l16 = rt.sub(l0, l14);
          at = 17; l17 = rt.float(l16);
          at = 18; store(rt, l5, l13, l17);
          at = 19; store(rt, l8, l13, l13);
        }
      }
      at = 20; l18 = K[5];
      at = 21; {
        const s21 = l18, c21 = Array.isArray(s21) ? s21.length : Number(rt.array_count(s21));
        if (c21 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i21 = 0; i21 < c21; i21++) {
          l19 = BigInt(i21); l20 = Array.isArray(s21) ? s21[i21] : rt.array_nth(s21, i21);
          at = 22; l21 = K[6];
          at = 23; l22 = rt.lt(l20, l21);
          at = 24; if (rt.truthy(l22)) {
            at = 25; l24 = K[7];
            at = 26; l25 = rt.mul(l20, l24);
            at = 27; l26 = K[8];
            at = 28; l27 = rt.add(l26, l25);
            at = 29; l28 = K[9];
            at = 30; l29 = rt.sub(l27, l28);
            at = 31; l30 = rt.int_div(l29, l25);
            at = 32; l31 = K[10];
            at = 33; {
              const f33 = rt.coerce("int", l31), e33 = rt.coerce("int", l30), n33 = e33 > f33 ? e33 - f33 : 0n;
              if (n33 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              l32 = n33;
              for (let i33 = 0, c33 = Number(n33); i33 < c33; i33++) {
                const j33 = BigInt(i33); l33 = j33; l34 = f33 + j33;
                at = 34; l35 = rt.mul(l34, l25);
                at = 35; l36 = rt.add(l35, l20);
                at = 36; l37 = K[11];
                at = 37; l38 = rt.min(l36, l37);
                at = 38; l39 = rt.add(l35, l25);
                at = 39; l40 = K[12];
                at = 40; l41 = rt.min(l39, l40);
                at = 41; l42 = l35;
                at = 42; l43 = l38;
                at = 43; {
                  const f43 = rt.coerce("int", l35), e43 = rt.coerce("int", l41), n43 = e43 > f43 ? e43 - f43 : 0n;
                  if (n43 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                  l44 = n43;
                  for (let i43 = 0, c43 = Number(n43); i43 < c43; i43++) {
                    const j43 = BigInt(i43); l45 = j43; l46 = f43 + j43;
                    at = 44; l47 = rt.lt(l42, l38);
                    at = 45; if (rt.truthy(l47)) {
                      at = 46; l49 = rt.lt(l43, l41);
                      at = 47; if (rt.truthy(l49)) {
                        at = 48; l51 = rt.sequence_get(l8, l42);
                        at = 49; l52 = rt.sequence_get(l8, l43);
                        at = 50; l53 = rt.sequence_get(l5, l51);
                        at = 51; l54 = rt.sequence_get(l5, l52);
                        at = 52; l55 = rt.le(l53, l54);
                        at = 53; l50 = l55;
                      } else {
                        at = 54; l56 = K[13];
                        at = 55; l50 = l56;
                      }
                      at = 56; l48 = l50;
                    } else {
                      at = 57; l57 = K[14];
                      at = 58; l48 = l57;
                    }
                    at = 59; if (rt.truthy(l48)) {
                      at = 60; l59 = rt.sequence_get(l8, l42);
                      at = 61; store(rt, l11, l46, l59);
                      at = 62; l60 = K[15];
                      at = 63; l61 = rt.add(l42, l60);
                      at = 64; l42 = l61;
                      at = 65; l62 = K[16];
                      at = 66; l58 = l62;
                    } else {
                      at = 67; l63 = rt.sequence_get(l8, l43);
                      at = 68; store(rt, l11, l46, l63);
                      at = 69; l64 = K[17];
                      at = 70; l65 = rt.add(l43, l64);
                      at = 71; l43 = l65;
                      at = 72; l66 = K[18];
                      at = 73; l58 = l66;
                    }
                  }
                }
              }
            }
            at = 74; l67 = l8;
            at = 75; l8 = l11;
            at = 76; l11 = l67;
            at = 77; l68 = K[19];
            at = 78; l23 = l68;
          } else {
            at = 79; l69 = K[20];
            at = 80; l23 = l69;
          }
        }
      }
      at = 81; l70 = K[21];
      at = 82; l71 = rt.sequence_build(l70);
      at = 83; l72 = K[22];
      at = 84; l73 = K[23];
      at = 85; {
        const f85 = rt.coerce("int", l72), e85 = rt.coerce("int", l73), n85 = e85 > f85 ? e85 - f85 : 0n;
        if (n85 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l74 = n85;
        for (let i85 = 0, c85 = Number(n85); i85 < c85; i85++) {
          const j85 = BigInt(i85); l75 = j85; l76 = f85 + j85;
          at = 86; l77 = rt.sequence_get(l8, l76);
          at = 87; l78 = K[24];
          at = 88; l79 = rt.sequence_get(l78, l77);
          at = 89; store(rt, l71, l76, l79);
        }
      }
      at = 90; l3 = l71;
    } else {
      at = 91; calls[1] = 0;
      at = 92; l80 = K[25];
      at = 93; l81 = rt.sequence_build(l80);
      at = 94; l82 = K[26];
      at = 95; l83 = rt.sequence_build(l82);
      at = 96; l84 = l83;
      at = 97; l85 = K[27];
      at = 98; l86 = rt.sequence_build(l85);
      at = 99; l87 = l86;
      at = 100; l88 = K[28];
      at = 101; {
        const s101 = l88, c101 = Array.isArray(s101) ? s101.length : Number(rt.array_count(s101));
        if (c101 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i101 = 0; i101 < c101; i101++) {
          l89 = BigInt(i101); l90 = Array.isArray(s101) ? s101[i101] : rt.array_nth(s101, i101);
          at = 102; l91 = BigInt(calls[1]++);
          at = 103; l92 = rt.sub(l0, l90);
          at = 104; l93 = rt.float(l92);
          at = 105; store(rt, l81, l89, l93);
          at = 106; store(rt, l84, l89, l89);
        }
      }
      at = 107; l94 = K[29];
      at = 108; {
        const s108 = l94, c108 = Array.isArray(s108) ? s108.length : Number(rt.array_count(s108));
        if (c108 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i108 = 0; i108 < c108; i108++) {
          l95 = BigInt(i108); l96 = Array.isArray(s108) ? s108[i108] : rt.array_nth(s108, i108);
          at = 109; l97 = K[30];
          at = 110; l98 = rt.lt(l96, l97);
          at = 111; if (rt.truthy(l98)) {
            at = 112; l100 = K[31];
            at = 113; l101 = rt.mul(l96, l100);
            at = 114; l102 = K[32];
            at = 115; l103 = rt.add(l102, l101);
            at = 116; l104 = K[33];
            at = 117; l105 = rt.sub(l103, l104);
            at = 118; l106 = rt.int_div(l105, l101);
            at = 119; l107 = K[34];
            at = 120; {
              const f120 = rt.coerce("int", l107), e120 = rt.coerce("int", l106), n120 = e120 > f120 ? e120 - f120 : 0n;
              if (n120 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
              l108 = n120;
              for (let i120 = 0, c120 = Number(n120); i120 < c120; i120++) {
                const j120 = BigInt(i120); l109 = j120; l110 = f120 + j120;
                at = 121; l111 = rt.mul(l110, l101);
                at = 122; l112 = rt.add(l111, l96);
                at = 123; l113 = K[35];
                at = 124; l114 = rt.min(l112, l113);
                at = 125; l115 = rt.add(l111, l101);
                at = 126; l116 = K[36];
                at = 127; l117 = rt.min(l115, l116);
                at = 128; l118 = l111;
                at = 129; l119 = l114;
                at = 130; {
                  const f130 = rt.coerce("int", l111), e130 = rt.coerce("int", l117), n130 = e130 > f130 ? e130 - f130 : 0n;
                  if (n130 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
                  l120 = n130;
                  for (let i130 = 0, c130 = Number(n130); i130 < c130; i130++) {
                    const j130 = BigInt(i130); l121 = j130; l122 = f130 + j130;
                    at = 131; l123 = rt.lt(l118, l114);
                    at = 132; if (rt.truthy(l123)) {
                      at = 133; l125 = rt.lt(l119, l117);
                      at = 134; if (rt.truthy(l125)) {
                        at = 135; l127 = rt.sequence_get(l84, l118);
                        at = 136; l128 = rt.sequence_get(l84, l119);
                        at = 137; l129 = rt.sequence_get(l81, l127);
                        at = 138; l130 = rt.sequence_get(l81, l128);
                        at = 139; l131 = rt.le(l129, l130);
                        at = 140; l126 = l131;
                      } else {
                        at = 141; l132 = K[37];
                        at = 142; l126 = l132;
                      }
                      at = 143; l124 = l126;
                    } else {
                      at = 144; l133 = K[38];
                      at = 145; l124 = l133;
                    }
                    at = 146; if (rt.truthy(l124)) {
                      at = 147; l135 = rt.sequence_get(l84, l118);
                      at = 148; store(rt, l87, l122, l135);
                      at = 149; l136 = K[39];
                      at = 150; l137 = rt.add(l118, l136);
                      at = 151; l118 = l137;
                      at = 152; l138 = K[40];
                      at = 153; l134 = l138;
                    } else {
                      at = 154; l139 = rt.sequence_get(l84, l119);
                      at = 155; store(rt, l87, l122, l139);
                      at = 156; l140 = K[41];
                      at = 157; l141 = rt.add(l119, l140);
                      at = 158; l119 = l141;
                      at = 159; l142 = K[42];
                      at = 160; l134 = l142;
                    }
                  }
                }
              }
            }
            at = 161; l143 = l84;
            at = 162; l84 = l87;
            at = 163; l87 = l143;
            at = 164; l144 = K[43];
            at = 165; l99 = l144;
          } else {
            at = 166; l145 = K[44];
            at = 167; l99 = l145;
          }
        }
      }
      at = 168; l146 = K[45];
      at = 169; l147 = rt.sequence_build(l146);
      at = 170; l148 = K[46];
      at = 171; l149 = K[47];
      at = 172; {
        const f172 = rt.coerce("int", l148), e172 = rt.coerce("int", l149), n172 = e172 > f172 ? e172 - f172 : 0n;
        if (n172 > 4096n) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        l150 = n172;
        for (let i172 = 0, c172 = Number(n172); i172 < c172; i172++) {
          const j172 = BigInt(i172); l151 = j172; l152 = f172 + j172;
          at = 173; l153 = rt.sequence_get(l84, l152);
          at = 174; l154 = K[48];
          at = 175; l155 = rt.sequence_get(l154, l153);
          at = 176; store(rt, l147, l152, l155);
        }
      }
      at = 177; l3 = l147;
    }
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l3;
}
