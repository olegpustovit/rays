// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
let K;
export function frame(input, state, rt) {
  K ??= [-1e3, 0, 1, 0n, 0n, [0n, 1n, 2n, 3n], 4096n, 0n, 1n, 1n, 0, 1, 0n, 0n, [0n, 1n, 2n, 3n], 4096n, 0n, 1n, 1n];
  let l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;
  let l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26, l27, l28, l29, l30, l31;
  let l32, l33, l34, l35, l36, l37, l38, l39, l40, l41, l42, l43, l44, l45, l46, l47;
  let l48, l49, l50, l51, l52, l53, l54, l55, l56, l57, l58, l59, l60, l61, l62, l63;
  let l64, l65, l66, l67, l68, l69, l70, l71, l72, l73, l74, l75, l76, l77, l78, l79;
  let l80, l81, l82, l83, l84, l85, l86, l87, l88, l89, l90, l91;
  let at = 0;
  try {
    at = 0; l0 = input.t;
    at = 1; l1 = K[0];
    at = 2; l2 = rt.gt(l0, l1);
    at = 3; if (rt.truthy(l2)) {
      at = 4; l3 = K[1];
      at = 5; l4 = l3;
      at = 6; l5 = K[2];
      at = 7; l6 = l5;
      at = 8; l7 = K[3];
      at = 9; l8 = l7;
      at = 10; l9 = K[4];
      at = 11; l10 = l9;
      at = 12; l11 = K[5];
      at = 13; {
        const s13 = l11, c13 = Array.isArray(s13) ? s13.length : Number(rt.array_count(s13));
        if (c13 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i13 = 0; i13 < c13; i13++) {
          l12 = BigInt(i13); l13 = Array.isArray(s13) ? s13[i13] : rt.array_nth(s13, i13);
          at = 14; l14 = K[6];
          at = 15; l15 = rt.ge(l8, l14);
          at = 16; if (rt.truthy(l15)) {
            at = 17; l17 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 18; l16 = l17;
          } else {
            at = 19; l18 = K[7];
            at = 20; l16 = l18;
          }
          at = 21; l19 = rt.add(l4, l0);
          at = 22; l20 = rt.coerce("float", l4);
          at = 23; l21 = rt.equal(l4, l20);
          at = 24; if (rt.truthy(l21)) {
            at = 25; l23 = rt.coerce("float", l19);
            at = 26; l22 = l23;
          } else {
            at = 27; l24 = rt.coerce("int", l19);
            at = 28; l22 = l24;
          }
          at = 29; l25 = rt.coerce("float", l6);
          at = 30; l26 = rt.equal(l6, l25);
          at = 31; if (rt.truthy(l26)) {
            at = 32; l28 = rt.coerce("float", l4);
            at = 33; l27 = l28;
          } else {
            at = 34; l29 = rt.coerce("int", l4);
            at = 35; l27 = l29;
          }
          at = 36; l30 = rt.coerce("float", l4);
          at = 37; l31 = rt.equal(l4, l30);
          at = 38; if (rt.truthy(l31)) {
            at = 39; l33 = rt.coerce("float", l22);
            at = 40; l32 = l33;
          } else {
            at = 41; l34 = rt.coerce("int", l22);
            at = 42; l32 = l34;
          }
          at = 43; l35 = rt.coerce("float", l6);
          at = 44; l36 = rt.equal(l6, l35);
          at = 45; if (rt.truthy(l36)) {
            at = 46; l38 = rt.coerce("float", l27);
            at = 47; l37 = l38;
          } else {
            at = 48; l39 = rt.coerce("int", l27);
            at = 49; l37 = l39;
          }
          at = 50; l40 = l32;
          at = 51; l41 = l37;
          at = 52; l4 = l40;
          at = 53; l6 = l41;
          at = 54; l42 = K[8];
          at = 55; l43 = rt.add(l10, l42);
          at = 56; l10 = l43;
          at = 57; l44 = K[9];
          at = 58; l45 = rt.add(l8, l44);
          at = 59; l8 = l45;
        }
      }
      at = 116; l89 = l4;
      at = 118; l90 = l6;
    } else {
      at = 60; l46 = K[10];
      at = 61; l47 = l46;
      at = 62; l48 = K[11];
      at = 63; l49 = l48;
      at = 64; l50 = K[12];
      at = 65; l51 = l50;
      at = 66; l52 = K[13];
      at = 67; l53 = l52;
      at = 68; l54 = K[14];
      at = 69; {
        const s69 = l54, c69 = Array.isArray(s69) ? s69.length : Number(rt.array_count(s69));
        if (c69 > 4096) rt.fail("E_ITER_BOUND", "Core iteration exceeds its bound.");
        for (let i69 = 0; i69 < c69; i69++) {
          l55 = BigInt(i69); l56 = Array.isArray(s69) ? s69[i69] : rt.array_nth(s69, i69);
          at = 70; l57 = K[15];
          at = 71; l58 = rt.ge(l51, l57);
          at = 72; if (rt.truthy(l58)) {
            at = 73; l60 = rt.fail("E_ITER_BOUND", "Loop runs more than 4,096 iterations.");
            at = 74; l59 = l60;
          } else {
            at = 75; l61 = K[16];
            at = 76; l59 = l61;
          }
          at = 77; l62 = rt.add(l47, l0);
          at = 78; l63 = rt.coerce("float", l47);
          at = 79; l64 = rt.equal(l47, l63);
          at = 80; if (rt.truthy(l64)) {
            at = 81; l66 = rt.coerce("float", l62);
            at = 82; l65 = l66;
          } else {
            at = 83; l67 = rt.coerce("int", l62);
            at = 84; l65 = l67;
          }
          at = 85; l68 = rt.coerce("float", l49);
          at = 86; l69 = rt.equal(l49, l68);
          at = 87; if (rt.truthy(l69)) {
            at = 88; l71 = rt.coerce("float", l47);
            at = 89; l70 = l71;
          } else {
            at = 90; l72 = rt.coerce("int", l47);
            at = 91; l70 = l72;
          }
          at = 92; l73 = rt.coerce("float", l47);
          at = 93; l74 = rt.equal(l47, l73);
          at = 94; if (rt.truthy(l74)) {
            at = 95; l76 = rt.coerce("float", l65);
            at = 96; l75 = l76;
          } else {
            at = 97; l77 = rt.coerce("int", l65);
            at = 98; l75 = l77;
          }
          at = 99; l78 = rt.coerce("float", l49);
          at = 100; l79 = rt.equal(l49, l78);
          at = 101; if (rt.truthy(l79)) {
            at = 102; l81 = rt.coerce("float", l70);
            at = 103; l80 = l81;
          } else {
            at = 104; l82 = rt.coerce("int", l70);
            at = 105; l80 = l82;
          }
          at = 106; l83 = l75;
          at = 107; l84 = l80;
          at = 108; l47 = l83;
          at = 109; l49 = l84;
          at = 110; l85 = K[17];
          at = 111; l86 = rt.add(l53, l85);
          at = 112; l53 = l86;
          at = 113; l87 = K[18];
          at = 114; l88 = rt.add(l51, l87);
          at = 115; l51 = l88;
        }
      }
      at = 117; l89 = l47;
      at = 119; l90 = l49;
    }
    at = 120; l91 = new Map([["x", l89], ["y", l90]]);
  } catch (e) {
    if (e !== null && typeof e === "object" && e.statement === undefined) e.statement = at;
    throw e;
  }
  return l91;
}
