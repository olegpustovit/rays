@group(0) @binding(7) var texture7: texture_2d<f32>;
@group(0) @binding(8) var sampler8: sampler;
@group(0) @binding(0) var<storage, read> buffer0: array<u32>;
@group(0) @binding(1) var<storage, read> buffer1: array<f32>;
@group(0) @binding(2) var<storage, read> buffer2: array<u32>;
@group(0) @binding(3) var<storage, read> buffer3: array<u32>;
@group(0) @binding(4) var<storage, read> buffer4: array<f32>;
@group(0) @binding(5) var<storage, read> buffer5: array<u32>;
@group(0) @binding(6) var<storage, read> buffer6: array<f32>;
const quad_corner = array<u32, 6>(0u, 1u, 2u, 0u, 2u, 3u);
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
fn i64_sub(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { return vec2<u32>(a.x - b.x, a.y - b.y - select(0u, 1u, a.x < b.x)); }
fn mulhi32(a: u32, b: u32) -> u32 {
  let a0 = a & 0xffffu; let a1 = a >> 16u; let b0 = b & 0xffffu; let b1 = b >> 16u;
  let m1 = a1 * b0 + ((a0 * b0) >> 16u);
  let m2 = a0 * b1 + (m1 & 0xffffu);
  return a1 * b1 + (m1 >> 16u) + (m2 >> 16u);
}
fn i64_mul(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { return vec2<u32>(a.x * b.x, mulhi32(a.x, b.x) + a.x * b.y + a.y * b.x); }
fn i64_neg(a: vec2<u32>) -> bool { return bitcast<i32>(a.y) < 0; }
fn i64_lt(a: vec2<u32>, b: vec2<u32>) -> bool { let x = bitcast<i32>(a.y); let y = bitcast<i32>(b.y); return x < y || (x == y && a.x < b.x); }
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
fn i64_abs(a: vec2<u32>) -> vec2<u32> { return select(a, i64_sub(vec2<u32>(0u, 0u), a), i64_neg(a)); }
fn i64_to_f(a: vec2<u32>) -> f32 {
  if ((a.y == 0u && a.x < 0x80000000u) || (a.y == 0xffffffffu && a.x >= 0x80000000u)) { return f32(bitcast<i32>(a.x)); }
  let m = select(a, vec2<u32>(0u - a.x, 0u - a.y - select(0u, 1u, a.x != 0u)), bitcast<i32>(a.y) < 0);
  return select(1.0, -1.0, bitcast<i32>(a.y) < 0) * (f32(m.y) * 4294967296.0 + f32(m.x));
}
fn i64_from_f(x: f32) -> vec2<u32> {
  if (abs(x) < 2147483648.0) { let n = i32(x); return vec2<u32>(bitcast<u32>(n), select(0u, 0xffffffffu, n < 0)); }
  let h = floor(x / 4294967296.0);
  return vec2<u32>(u32(x - h * 4294967296.0), bitcast<u32>(i32(h)));
}
fn i64_shl(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> {
  let n = b.x & 63u;
  if (n == 0u) { return a; }
  if (n >= 32u) { return vec2<u32>(0u, a.x << (n - 32u)); }
  return vec2<u32>(a.x << n, (a.y << n) | (a.x >> (32u - n)));
}
fn i64_shr(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> {
  let n = b.x & 63u;
  if (n == 0u) { return a; }
  if (n >= 32u) { return vec2<u32>(a.y >> (n - 32u), 0u); }
  return vec2<u32>((a.x >> n) | (a.y << (32u - n)), a.y >> n);
}
fn i64_fmod(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> {
  let r = i64_divmod(a, b, true);
  if ((r.x != 0u || r.y != 0u) && (i64_neg(r) != i64_neg(b))) { return i64_add(r, b); }
  return r;
}
fn i64_small(a: vec2<u32>) -> bool { return (a.y == 0u && a.x < 0x80000000u) || (a.y == 0xffffffffu && a.x >= 0x80000000u); }
fn i64_divmod(a: vec2<u32>, b: vec2<u32>, rem: bool) -> vec2<u32> {
  if (b.x == 0u && b.y == 0u) { return vec2<u32>(0u, 0u); }
  if (i64_small(a) && i64_small(b) && !(a.x == 0x80000000u && b.x == 0xffffffffu)) {
    let x = bitcast<i32>(a.x); let y = bitcast<i32>(b.x);
    let q = select(x / y, x % y, rem);
    return vec2<u32>(bitcast<u32>(q), select(0u, 0xffffffffu, q < 0));
  }
  let ua = i64_abs(a); let ub = i64_abs(b);
  var q = vec2<u32>(0u, 0u); var r = vec2<u32>(0u, 0u);
  for (var k = 63; k >= 0; k--) {
    r = vec2<u32>(r.x << 1u, (r.y << 1u) | (r.x >> 31u));
    let bit = select((ua.x >> u32(k)) & 1u, (ua.y >> u32(k - 32)) & 1u, k >= 32);
    r.x = r.x | bit;
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
struct marks_vertex_out {
  @builtin(position) position: vec4<f32>,
  @location(0) color: vec4<f32>,
  @location(1) color2: vec4<f32>,
  @location(2) local: vec2<f32>,
  @location(3) @interpolate(flat) box: vec4<f32>,
  @location(4) @interpolate(flat) params: vec4<f32>,
  @location(5) @interpolate(flat) extra: vec4<f32>,
}
@vertex
fn marks_vertex(@builtin(vertex_index) vid: u32, @builtin(instance_index) iid: u32) -> marks_vertex_out {
  var sb: u32 = 0u;
  var o: marks_vertex_out;
  let r8: u32 = 4u;
  let r11: u32 = 1u;
  let r13: u32 = 2u;
  let r16: f32 = bitcast<f32>(0x3f800000u);
  let r17: f32 = bitcast<f32>(0x0u);
  let r20: f32 = bitcast<f32>(0x3f800000u);
  let r21: f32 = bitcast<f32>(0x0u);
  let r23: u32 = 0u;
  let r32: u32 = 5u;
  let r38: u32 = 7u;
  var r41: vec2<u32> = vec2<u32>(0u, 0u);
  var r42: vec2<u32> = vec2<u32>(0u, 0u);
  var r43: f32 = 0.0;
  let r44: f32 = bitcast<f32>(0x0u);
  var r45: f32 = 0.0;
  let r46: f32 = bitcast<f32>(0x3f800000u);
  var r47: f32 = 0.0;
  let r48: f32 = bitcast<f32>(0x437f0000u);
  var r49: f32 = 0.0;
  let r50: f32 = bitcast<f32>(0x3f000000u);
  var r51: f32 = 0.0;
  var r52: vec2<u32> = vec2<u32>(0u, 0u);
  let r53: u32 = 24u;
  var r54: vec2<u32> = vec2<u32>(0u, 0u);
  let r55: u32 = 1u;
  var r56: u32 = 0u;
  var r57: vec2<u32> = vec2<u32>(0u, 0u);
  var r58: f32 = 0.0;
  var r59: f32 = 0.0;
  var r60: f32 = 0.0;
  var r61: f32 = 0.0;
  var r62: f32 = 0.0;
  var r63: vec2<u32> = vec2<u32>(0u, 0u);
  let r64: u32 = 16u;
  var r65: vec2<u32> = vec2<u32>(0u, 0u);
  var r66: vec2<u32> = vec2<u32>(0u, 0u);
  let r67: u32 = 2u;
  var r68: u32 = 0u;
  var r69: vec2<u32> = vec2<u32>(0u, 0u);
  var r70: f32 = 0.0;
  var r71: f32 = 0.0;
  var r72: f32 = 0.0;
  var r73: f32 = 0.0;
  var r74: f32 = 0.0;
  var r75: vec2<u32> = vec2<u32>(0u, 0u);
  let r76: u32 = 8u;
  var r77: vec2<u32> = vec2<u32>(0u, 0u);
  let r78: u32 = 255u;
  var r79: vec2<u32> = vec2<u32>(0u, 0u);
  var r80: vec2<u32> = vec2<u32>(0u, 0u);
  var r83: f32 = 0.0;
  let r84: u32 = 1u;
  var r85: u32 = 0u;
  var r86: f32 = 0.0;
  var r87: f32 = 0.0;
  var r88: f32 = 0.0;
  var r89: f32 = 0.0;
  var r90: f32 = 0.0;
  var r91: f32 = 0.0;
  var r92: f32 = 0.0;
  let r93: f32 = bitcast<f32>(0x358637bdu);
  var r94: f32 = 0.0;
  var r95: f32 = 0.0;
  var r96: f32 = 0.0;
  let r97: f32 = bitcast<f32>(0x3f800000u);
  let r98: f32 = bitcast<f32>(0x0u);
  var r99: f32 = 0.0;
  var r100: f32 = 0.0;
  let r101: f32 = bitcast<f32>(0x0u);
  var r102: f32 = 0.0;
  let r103: f32 = bitcast<f32>(0x3f000000u);
  var r104: f32 = 0.0;
  let r105: f32 = bitcast<f32>(0x3fc00000u);
  var r106: f32 = 0.0;
  var r107: f32 = 0.0;
  var r108: f32 = 0.0;
  var r109: f32 = 0.0;
  var r110: f32 = 0.0;
  var r111: f32 = 0.0;
  var r112: f32 = 0.0;
  var r113: f32 = 0.0;
  var r114: f32 = 0.0;
  var r115: f32 = 0.0;
  var r116: f32 = 0.0;
  var r117: f32 = 0.0;
  var r118: f32 = 0.0;
  let r119: f32 = bitcast<f32>(0x40000000u);
  var r120: f32 = 0.0;
  let r121: f32 = bitcast<f32>(0x3f800000u);
  var r122: f32 = 0.0;
  var r123: f32 = 0.0;
  var r124: f32 = 0.0;
  var r125: f32 = 0.0;
  var r126: f32 = 0.0;
  var r127: f32 = 0.0;
  let r128: u32 = 0u;
  var r129: f32 = 0.0;
  var r130: f32 = 0.0;
  var r131: f32 = 0.0;
  var r132: f32 = 0.0;
  var r133: f32 = 0.0;
  let r134: u32 = 2u;
  var r135: f32 = 0.0;
  var r136: f32 = 0.0;
  let r137: u32 = 3u;
  var r138: f32 = 0.0;
  var r139: f32 = 0.0;
  let r140: u32 = 4u;
  var r141: f32 = 0.0;
  var r142: f32 = 0.0;
  var r143: f32 = 0.0;
  let r144: u32 = 5u;
  var r145: f32 = 0.0;
  var r146: f32 = 0.0;
  let r147: u32 = 24u;
  var r148: vec2<u32> = vec2<u32>(0u, 0u);
  let r149: u32 = 255u;
  var r150: u32 = 0u;
  var r151: f32 = 0.0;
  let r152: f32 = bitcast<f32>(0x437f0000u);
  var r153: f32 = 0.0;
  let r154: u32 = 16u;
  var r155: vec2<u32> = vec2<u32>(0u, 0u);
  var r156: u32 = 0u;
  var r157: f32 = 0.0;
  var r158: f32 = 0.0;
  let r159: u32 = 8u;
  var r160: vec2<u32> = vec2<u32>(0u, 0u);
  var r161: u32 = 0u;
  var r162: f32 = 0.0;
  var r163: f32 = 0.0;
  var r164: u32 = 0u;
  var r165: f32 = 0.0;
  var r166: f32 = 0.0;
  let r167: f32 = bitcast<f32>(0x40a00000u);
  let r168: u32 = 4u;
  var r169: f32 = 0.0;
  var r170: f32 = 0.0;
  var r171: f32 = 0.0;
  var r172: f32 = 0.0;
  var r173: f32 = 0.0;
  var r174: f32 = 0.0;
  var r175: f32 = 0.0;
  var r176: f32 = 0.0;
  let r177: u32 = 1u;
  var r178: u32 = 0u;
  var r179: f32 = 0.0;
  var r180: f32 = 0.0;
  var r181: f32 = 0.0;
  var r182: f32 = 0.0;
  var r183: f32 = 0.0;
  var r184: f32 = 0.0;
  let r185: u32 = 0u;
  var r186: f32 = 0.0;
  let r187: f32 = bitcast<f32>(0x0u);
  var r188: f32 = 0.0;
  let r189: f32 = bitcast<f32>(0x3f800000u);
  let r190: f32 = bitcast<f32>(0x3f000000u);
  var r191: f32 = 0.0;
  var r192: f32 = 0.0;
  var r193: f32 = 0.0;
  var r194: f32 = 0.0;
  var r195: f32 = 0.0;
  var r196: f32 = 0.0;
  var r197: f32 = 0.0;
  var r198: f32 = 0.0;
  var r199: f32 = 0.0;
  var r200: f32 = 0.0;
  var r201: f32 = 0.0;
  var r202: f32 = 0.0;
  var r203: f32 = 0.0;
  var r204: f32 = 0.0;
  let r205: u32 = 1u;
  var r206: f32 = 0.0;
  var r207: f32 = 0.0;
  var r208: f32 = 0.0;
  let r209: u32 = 2u;
  var r210: f32 = 0.0;
  var r211: f32 = 0.0;
  let r212: u32 = 3u;
  var r213: f32 = 0.0;
  var r214: f32 = 0.0;
  var r215: f32 = 0.0;
  var r216: f32 = 0.0;
  var r217: f32 = 0.0;
  let r218: u32 = 5u;
  var r219: f32 = 0.0;
  var r220: f32 = 0.0;
  let r221: f32 = bitcast<f32>(0x0u);
  var r222: vec2<u32> = vec2<u32>(0u, 0u);
  var r223: u32 = 0u;
  let r224: u32 = 6u;
  var r225: vec2<u32> = vec2<u32>(0u, 0u);
  var r226: f32 = 0.0;
  var r227: vec2<u32> = vec2<u32>(0u, 0u);
  var r228: vec2<u32> = vec2<u32>(0u, 0u);
  var r229: f32 = 0.0;
  let r230: f32 = bitcast<f32>(0x0u);
  var r231: f32 = 0.0;
  let r232: f32 = bitcast<f32>(0x3f800000u);
  var r233: f32 = 0.0;
  let r234: f32 = bitcast<f32>(0x437f0000u);
  var r235: f32 = 0.0;
  let r236: f32 = bitcast<f32>(0x3f000000u);
  var r237: f32 = 0.0;
  var r238: vec2<u32> = vec2<u32>(0u, 0u);
  let r239: u32 = 24u;
  var r240: vec2<u32> = vec2<u32>(0u, 0u);
  let r241: u32 = 1u;
  var r242: u32 = 0u;
  var r243: vec2<u32> = vec2<u32>(0u, 0u);
  var r244: f32 = 0.0;
  var r245: f32 = 0.0;
  var r246: f32 = 0.0;
  var r247: f32 = 0.0;
  var r248: f32 = 0.0;
  var r249: vec2<u32> = vec2<u32>(0u, 0u);
  let r250: u32 = 16u;
  var r251: vec2<u32> = vec2<u32>(0u, 0u);
  var r252: vec2<u32> = vec2<u32>(0u, 0u);
  let r253: u32 = 2u;
  var r254: u32 = 0u;
  var r255: vec2<u32> = vec2<u32>(0u, 0u);
  var r256: f32 = 0.0;
  var r257: f32 = 0.0;
  var r258: f32 = 0.0;
  var r259: f32 = 0.0;
  var r260: f32 = 0.0;
  var r261: vec2<u32> = vec2<u32>(0u, 0u);
  let r262: u32 = 8u;
  var r263: vec2<u32> = vec2<u32>(0u, 0u);
  let r264: u32 = 255u;
  var r265: vec2<u32> = vec2<u32>(0u, 0u);
  var r266: vec2<u32> = vec2<u32>(0u, 0u);
  var r267: vec2<u32> = vec2<u32>(0u, 0u);
  let r268: u32 = 24u;
  var r269: vec2<u32> = vec2<u32>(0u, 0u);
  let r270: u32 = 255u;
  var r271: u32 = 0u;
  var r272: f32 = 0.0;
  let r273: f32 = bitcast<f32>(0x437f0000u);
  var r274: f32 = 0.0;
  let r275: u32 = 16u;
  var r276: vec2<u32> = vec2<u32>(0u, 0u);
  var r277: u32 = 0u;
  var r278: f32 = 0.0;
  var r279: f32 = 0.0;
  let r280: u32 = 8u;
  var r281: vec2<u32> = vec2<u32>(0u, 0u);
  var r282: u32 = 0u;
  var r283: f32 = 0.0;
  var r284: f32 = 0.0;
  var r285: u32 = 0u;
  var r286: f32 = 0.0;
  var r287: f32 = 0.0;
  var r288: vec2<u32> = vec2<u32>(0u, 0u);
  var r289: u32 = 0u;
  var r290: f32 = 0.0;
  var r291: f32 = 0.0;
  var r292: vec2<u32> = vec2<u32>(0u, 0u);
  var r293: u32 = 0u;
  var r294: f32 = 0.0;
  var r295: f32 = 0.0;
  var r296: vec2<u32> = vec2<u32>(0u, 0u);
  var r297: u32 = 0u;
  var r298: f32 = 0.0;
  var r299: f32 = 0.0;
  var r300: u32 = 0u;
  var r301: f32 = 0.0;
  var r302: f32 = 0.0;
  var r303: f32 = 0.0;
  let r0: vec2<u32> = vec2<u32>(iid * 4u + quad_corner[vid], 0u);
  let r9: vec2<u32> = i64_divmod(r0, vec2<u32>(r8, 0u), false);
  if (all(vec2<u32>(r8, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r10: vec2<u32> = i64_divmod(r0, vec2<u32>(r8, 0u), true);
  if (all(vec2<u32>(r8, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r12: f32 = select(0.0, 1.0, all(r10 == vec2<u32>(r11, 0u)));
  let r14: f32 = select(0.0, 1.0, all(r10 == vec2<u32>(r13, 0u)));
  let r15: f32 = select(0.0, 1.0, (r12 != 0.0 || r14 != 0.0));
  let r18: f32 = select(r17, r16, r15 != 0.0);
  let r19: f32 = select(0.0, 1.0, !i64_lt(r10, vec2<u32>(r13, 0u)));
  let r22: f32 = select(r21, r20, r19 != 0.0);
  let r24: vec2<u32> = vec2<u32>(buffer5[r23 * 1u + 0u], 0u);
  let r25: vec2<u32> = vec2<u32>(buffer5[r11 * 1u + 0u], 0u);
  let r26: u32 = ((r9).x * (r25).x);
    let r27: vec2<u32> = vec2<u32>(buffer5[r13 * 1u + 0u], 0u);
  let r28: u32 = ((r9).x * (r27).x);
    let r29: f32 = bitcast<f32>(buffer0[r26 * 1u + 0u]);
  let r30: u32 = (r26 + r11);
    let r31: f32 = bitcast<f32>(buffer0[r30 * 1u + 0u]);
  let r33: vec2<u32> = vec2<u32>(buffer5[r32 * 1u + 0u], 0u);
  let r34: u32 = ((r9).x * (r33).x);
    let r35: f32 = buffer4[r34 * 1u + 0u];
  let r36: vec2<u32> = vec2<u32>(buffer5[r8 * 1u + 0u], 0u);
  let r37: u32 = ((r9).x * (r36).x);
    let r39: vec2<u32> = vec2<u32>(buffer5[r38 * 1u + 0u], 0u);
  let r40: f32 = select(0.0, 1.0, all(r39 == vec2<u32>(r23, 0u)));
  if (r40 != 0.0) {
    r41 = vec2<u32>(buffer3[r37 * 1u + 0u], 0u);
  }
  if (r40 == 0.0) {
    r42 = vec2<u32>(buffer3[r37 * 1u + 0u], 0u);
    r43 = bitcast<f32>((r42).x);
    r45 = max(r43, r44);
    r47 = min(r45, r46);
    r49 = (r47 * r48);
    r51 = (r49 + r50);
    r52 = i64_from_f(floor(r51));
    r54 = i64_shl(r52, vec2<u32>(r53, 0u));
    if (vec2<u32>(r53, 0u).y != 0u || vec2<u32>(r53, 0u).x > 63u) { sb |= 4u; }
    r56 = (r37 + r55);
        r57 = vec2<u32>(buffer3[r56 * 1u + 0u], 0u);
    r58 = bitcast<f32>((r57).x);
    r59 = max(r58, r44);
    r60 = min(r59, r46);
    r61 = (r60 * r48);
    r62 = (r61 + r50);
    r63 = i64_from_f(floor(r62));
    r65 = i64_shl(r63, vec2<u32>(r64, 0u));
    if (vec2<u32>(r64, 0u).y != 0u || vec2<u32>(r64, 0u).x > 63u) { sb |= 4u; }
    r66 = (r54 | r65);
        r68 = (r37 + r67);
        r69 = vec2<u32>(buffer3[r68 * 1u + 0u], 0u);
    r70 = bitcast<f32>((r69).x);
    r71 = max(r70, r44);
    r72 = min(r71, r46);
    r73 = (r72 * r48);
    r74 = (r73 + r50);
    r75 = i64_from_f(floor(r74));
    r77 = i64_shl(r75, vec2<u32>(r76, 0u));
    if (vec2<u32>(r76, 0u).y != 0u || vec2<u32>(r76, 0u).x > 63u) { sb |= 4u; }
    r79 = (r77 | vec2<u32>(r78, 0u));
        r80 = (r66 | r79);
      }
  let r81: vec2<u32> = select(r80, r41, r40 != 0.0);
  let r82: f32 = select(0.0, 1.0, all(r24 == vec2<u32>(r32, 0u)));
  if (r82 != 0.0) {
    r83 = buffer1[r28 * 1u + 0u];
    r85 = (r28 + r84);
        r86 = buffer1[r85 * 1u + 0u];
    r87 = (r83 - r29);
    r88 = (r86 - r31);
    r89 = (r87 * r87);
    r90 = (r88 * r88);
    r91 = (r89 + r90);
    r92 = sqrt(abs(r91));
    r94 = select(0.0, 1.0, (r92 > r93));
    if (r94 != 0.0) {
      r95 = select(r87 / r92, 0.0, r92 == 0.0);
      r96 = select(r88 / r92, 0.0, r92 == 0.0);
    }
    r99 = select(r97, r95, r94 != 0.0);
    r100 = select(r98, r96, r94 != 0.0);
    r102 = (r101 - r100);
    r104 = (r35 * r103);
    r106 = (r104 + r105);
    r107 = (r99 * r106);
    r108 = (r100 * r106);
    r109 = (r29 - r107);
    r110 = (r31 - r108);
    r111 = (r83 + r107);
    r112 = (r86 + r108);
    r113 = (r111 - r109);
    r114 = (r112 - r110);
    r115 = (r113 * r18);
    r116 = (r114 * r18);
    r117 = (r109 + r115);
    r118 = (r110 + r116);
    r120 = (r22 * r119);
    r122 = (r120 - r121);
    r123 = (r122 * r106);
    r124 = (r102 * r123);
    r125 = (r99 * r123);
    r126 = (r117 + r124);
    r127 = (r118 + r125);
    r129 = buffer6[r128 * 1u + 0u];
    r130 = (r129 * r126);
    r131 = buffer6[r84 * 1u + 0u];
    r132 = (r131 * r127);
    r133 = (r130 + r132);
    r135 = buffer6[r134 * 1u + 0u];
    r136 = (r133 + r135);
    r138 = buffer6[r137 * 1u + 0u];
    r139 = (r138 * r126);
    r141 = buffer6[r140 * 1u + 0u];
    r142 = (r141 * r127);
    r143 = (r139 + r142);
    r145 = buffer6[r144 * 1u + 0u];
    r146 = (r143 + r145);
    r148 = i64_shr(r81, vec2<u32>(r147, 0u));
    if (vec2<u32>(r147, 0u).y != 0u || vec2<u32>(r147, 0u).x > 63u) { sb |= 4u; }
    r150 = ((r148).x & r149);
        r151 = i64_to_f(vec2<u32>(r150, 0u));
    r153 = select(r151 / r152, 0.0, r152 == 0.0);
    r155 = i64_shr(r81, vec2<u32>(r154, 0u));
    if (vec2<u32>(r154, 0u).y != 0u || vec2<u32>(r154, 0u).x > 63u) { sb |= 4u; }
    r156 = ((r155).x & r149);
        r157 = i64_to_f(vec2<u32>(r156, 0u));
    r158 = select(r157 / r152, 0.0, r152 == 0.0);
    r160 = i64_shr(r81, vec2<u32>(r159, 0u));
    if (vec2<u32>(r159, 0u).y != 0u || vec2<u32>(r159, 0u).x > 63u) { sb |= 4u; }
    r161 = ((r160).x & r149);
        r162 = i64_to_f(vec2<u32>(r161, 0u));
    r163 = select(r162 / r152, 0.0, r152 == 0.0);
    r164 = ((r81).x & r149);
        r165 = i64_to_f(vec2<u32>(r164, 0u));
    r166 = select(r165 / r152, 0.0, r152 == 0.0);
  }
  if (r82 == 0.0) {
    r169 = select(0.0, 1.0, all(r24 == vec2<u32>(r168, 0u)));
    if (r169 != 0.0) {
      r170 = buffer1[r28 * 1u + 0u];
      r171 = (r29 - r170);
      r172 = (r31 - r170);
      r173 = (r29 + r170);
      r174 = (r31 + r170);
    }
    if (r169 == 0.0) {
      r175 = buffer1[r28 * 1u + 0u];
      r176 = (r29 + r175);
      r178 = (r28 + r177);
            r179 = buffer1[r178 * 1u + 0u];
      r180 = (r31 + r179);
    }
    r181 = select(r29, r171, r169 != 0.0);
    r182 = select(r31, r172, r169 != 0.0);
    r183 = select(r176, r173, r169 != 0.0);
    r184 = select(r180, r174, r169 != 0.0);
    r186 = select(0.0, 1.0, all(r81 == vec2<u32>(r185, 0u)));
    r188 = select(r35, r187, r186 != 0.0);
    r191 = (r188 * r190);
    r192 = (r189 + r191);
    r193 = (r181 - r192);
    r194 = (r182 - r192);
    r195 = (r183 + r192);
    r196 = (r184 + r192);
    r197 = (r195 - r193);
    r198 = (r197 * r18);
    r199 = (r193 + r198);
    r200 = (r196 - r194);
    r201 = (r200 * r22);
    r202 = (r194 + r201);
    r203 = buffer6[r185 * 1u + 0u];
    r204 = (r203 * r199);
    r206 = buffer6[r205 * 1u + 0u];
    r207 = (r206 * r202);
    r208 = (r204 + r207);
    r210 = buffer6[r209 * 1u + 0u];
    r211 = (r208 + r210);
    r213 = buffer6[r212 * 1u + 0u];
    r214 = (r213 * r199);
    r215 = buffer6[r168 * 1u + 0u];
    r216 = (r215 * r202);
    r217 = (r214 + r216);
    r219 = buffer6[r218 * 1u + 0u];
    r220 = (r217 + r219);
    r222 = vec2<u32>(buffer5[r212 * 1u + 0u], 0u);
    r223 = ((r9).x * (r222).x);
        r225 = vec2<u32>(buffer5[r224 * 1u + 0u], 0u);
    r226 = select(0.0, 1.0, all(r225 == vec2<u32>(r185, 0u)));
    if (r226 != 0.0) {
      r227 = vec2<u32>(buffer2[r223 * 1u + 0u], 0u);
    }
    if (r226 == 0.0) {
      r228 = vec2<u32>(buffer2[r223 * 1u + 0u], 0u);
      r229 = bitcast<f32>((r228).x);
      r231 = max(r229, r230);
      r233 = min(r231, r232);
      r235 = (r233 * r234);
      r237 = (r235 + r236);
      r238 = i64_from_f(floor(r237));
      r240 = i64_shl(r238, vec2<u32>(r239, 0u));
      if (vec2<u32>(r239, 0u).y != 0u || vec2<u32>(r239, 0u).x > 63u) { sb |= 4u; }
      r242 = (r223 + r241);
            r243 = vec2<u32>(buffer2[r242 * 1u + 0u], 0u);
      r244 = bitcast<f32>((r243).x);
      r245 = max(r244, r230);
      r246 = min(r245, r232);
      r247 = (r246 * r234);
      r248 = (r247 + r236);
      r249 = i64_from_f(floor(r248));
      r251 = i64_shl(r249, vec2<u32>(r250, 0u));
      if (vec2<u32>(r250, 0u).y != 0u || vec2<u32>(r250, 0u).x > 63u) { sb |= 4u; }
      r252 = (r240 | r251);
            r254 = (r223 + r253);
            r255 = vec2<u32>(buffer2[r254 * 1u + 0u], 0u);
      r256 = bitcast<f32>((r255).x);
      r257 = max(r256, r230);
      r258 = min(r257, r232);
      r259 = (r258 * r234);
      r260 = (r259 + r236);
      r261 = i64_from_f(floor(r260));
      r263 = i64_shl(r261, vec2<u32>(r262, 0u));
      if (vec2<u32>(r262, 0u).y != 0u || vec2<u32>(r262, 0u).x > 63u) { sb |= 4u; }
      r265 = (r263 | vec2<u32>(r264, 0u));
            r266 = (r252 | r265);
          }
    r267 = select(r266, r227, r226 != 0.0);
    r269 = i64_shr(r267, vec2<u32>(r268, 0u));
    if (vec2<u32>(r268, 0u).y != 0u || vec2<u32>(r268, 0u).x > 63u) { sb |= 4u; }
    r271 = ((r269).x & r270);
        r272 = i64_to_f(vec2<u32>(r271, 0u));
    r274 = select(r272 / r273, 0.0, r273 == 0.0);
    r276 = i64_shr(r267, vec2<u32>(r275, 0u));
    if (vec2<u32>(r275, 0u).y != 0u || vec2<u32>(r275, 0u).x > 63u) { sb |= 4u; }
    r277 = ((r276).x & r270);
        r278 = i64_to_f(vec2<u32>(r277, 0u));
    r279 = select(r278 / r273, 0.0, r273 == 0.0);
    r281 = i64_shr(r267, vec2<u32>(r280, 0u));
    if (vec2<u32>(r280, 0u).y != 0u || vec2<u32>(r280, 0u).x > 63u) { sb |= 4u; }
    r282 = ((r281).x & r270);
        r283 = i64_to_f(vec2<u32>(r282, 0u));
    r284 = select(r283 / r273, 0.0, r273 == 0.0);
    r285 = ((r267).x & r270);
        r286 = i64_to_f(vec2<u32>(r285, 0u));
    r287 = select(r286 / r273, 0.0, r273 == 0.0);
    r288 = i64_shr(r81, vec2<u32>(r268, 0u));
    if (vec2<u32>(r268, 0u).y != 0u || vec2<u32>(r268, 0u).x > 63u) { sb |= 4u; }
    r289 = ((r288).x & r270);
        r290 = i64_to_f(vec2<u32>(r289, 0u));
    r291 = select(r290 / r273, 0.0, r273 == 0.0);
    r292 = i64_shr(r81, vec2<u32>(r275, 0u));
    if (vec2<u32>(r275, 0u).y != 0u || vec2<u32>(r275, 0u).x > 63u) { sb |= 4u; }
    r293 = ((r292).x & r270);
        r294 = i64_to_f(vec2<u32>(r293, 0u));
    r295 = select(r294 / r273, 0.0, r273 == 0.0);
    r296 = i64_shr(r81, vec2<u32>(r280, 0u));
    if (vec2<u32>(r280, 0u).y != 0u || vec2<u32>(r280, 0u).x > 63u) { sb |= 4u; }
    r297 = ((r296).x & r270);
        r298 = i64_to_f(vec2<u32>(r297, 0u));
    r299 = select(r298 / r273, 0.0, r273 == 0.0);
    r300 = ((r81).x & r270);
        r301 = i64_to_f(vec2<u32>(r300, 0u));
    r302 = select(r301 / r273, 0.0, r273 == 0.0);
    r303 = i64_to_f(r24);
  }
  let r304: f32 = select(r211, r136, r82 != 0.0);
  let r305: f32 = select(r220, r146, r82 != 0.0);
  let r306: f32 = select(r221, r101, r82 != 0.0);
  let r307: f32 = select(r189, r121, r82 != 0.0);
  let r308: f32 = select(r274, r153, r82 != 0.0);
  let r309: f32 = select(r279, r158, r82 != 0.0);
  let r310: f32 = select(r284, r163, r82 != 0.0);
  let r311: f32 = select(r287, r166, r82 != 0.0);
  let r312: f32 = select(r291, r101, r82 != 0.0);
  let r313: f32 = select(r295, r101, r82 != 0.0);
  let r314: f32 = select(r299, r101, r82 != 0.0);
  let r315: f32 = select(r302, r101, r82 != 0.0);
  let r316: f32 = select(r199, r126, r82 != 0.0);
  let r317: f32 = select(r202, r127, r82 != 0.0);
  let r318: f32 = select(r181, r101, r82 != 0.0);
  let r319: f32 = select(r182, r101, r82 != 0.0);
  let r320: f32 = select(r183, r101, r82 != 0.0);
  let r321: f32 = select(r184, r101, r82 != 0.0);
  let r322: f32 = select(r188, r35, r82 != 0.0);
  let r323: f32 = select(r303, r167, r82 != 0.0);
  let r324: f32 = select(r221, r29, r82 != 0.0);
  let r325: f32 = select(r221, r31, r82 != 0.0);
  let r326: f32 = select(r221, r83, r82 != 0.0);
  let r327: f32 = select(r221, r86, r82 != 0.0);
  o.position.x = r304;
  o.position.y = r305;
  o.position.z = r306;
  o.position.w = r307;
  o.color.x = r308;
  o.color.y = r309;
  o.color.z = r310;
  o.color.w = r311;
  o.color2.x = r312;
  o.color2.y = r313;
  o.color2.z = r314;
  o.color2.w = r315;
  o.local.x = r316;
  o.local.y = r317;
  o.box.x = r318;
  o.box.y = r319;
  o.box.z = r320;
  o.box.w = r321;
  o.params.x = r306;
  o.params.y = r322;
  o.params.z = r307;
  o.params.w = r323;
  o.extra.x = r324;
  o.extra.y = r325;
  o.extra.z = r326;
  o.extra.w = r327;
  return o;
}
@fragment
fn marks_fragment(v: marks_vertex_out) -> @location(0) vec4<f32> {
  let fw2_0 = fwidth(v.local.x);
  let fw2_1 = fwidth(v.local.y);
  var sb: u32 = 0u;
  var colour: vec4<f32>;
  let r22: f32 = bitcast<f32>(0x40a00000u);
  var r24: f32 = 0.0;
  var r25: f32 = 0.0;
  var r26: f32 = 0.0;
  var r27: f32 = 0.0;
  var r28: f32 = 0.0;
  var r29: f32 = 0.0;
  var r30: f32 = 0.0;
  var r31: f32 = 0.0;
  var r32: f32 = 0.0;
  var r33: f32 = 0.0;
  let r34: f32 = bitcast<f32>(0x3f000000u);
  var r35: f32 = 0.0;
  var r36: f32 = 0.0;
  var r37: f32 = 0.0;
  var r38: f32 = 0.0;
  var r39: f32 = 0.0;
  var r40: f32 = 0.0;
  var r41: f32 = 0.0;
  let r42: f32 = bitcast<f32>(0x0u);
  var r43: f32 = 0.0;
  var r44: f32 = 0.0;
  var r45: f32 = 0.0;
  var r46: f32 = 0.0;
  var r47: f32 = 0.0;
  var r48: f32 = 0.0;
  var r49: f32 = 0.0;
  var r50: f32 = 0.0;
  var r51: f32 = 0.0;
  var r52: f32 = 0.0;
  var r53: f32 = 0.0;
  var r54: f32 = 0.0;
  var r55: f32 = 0.0;
  var r56: f32 = 0.0;
  var r57: f32 = 0.0;
  var r58: f32 = 0.0;
  var r59: f32 = 0.0;
  var r60: f32 = 0.0;
  var r61: f32 = 0.0;
  var r62: f32 = 0.0;
  var r63: f32 = 0.0;
  var r64: f32 = 0.0;
  var r65: f32 = 0.0;
  var r66: f32 = 0.0;
  var r67: f32 = 0.0;
  var r68: f32 = 0.0;
  var r69: f32 = 0.0;
  var r70: f32 = 0.0;
  let r71: f32 = bitcast<f32>(0x38d1b717u);
  var r72: f32 = 0.0;
  let r73: f32 = bitcast<f32>(0x358637bdu);
  var r74: f32 = 0.0;
  var r75: f32 = 0.0;
  var r76: f32 = 0.0;
  var r77: f32 = 0.0;
  let r78: f32 = bitcast<f32>(0x3f800000u);
  var r79: f32 = 0.0;
  var r80: f32 = 0.0;
  var r81: f32 = 0.0;
  var r82: f32 = 0.0;
  let r83: f32 = bitcast<f32>(0x3f000000u);
  var r84: f32 = 0.0;
  var r85: f32 = 0.0;
  var r86: f32 = 0.0;
  var r87: f32 = 0.0;
  var r88: f32 = 0.0;
  var r89: f32 = 0.0;
  let r90: f32 = bitcast<f32>(0x40800000u);
  var r91: f32 = 0.0;
  var r92: f32 = 0.0;
  var r93: f32 = 0.0;
  var r94: f32 = 0.0;
  var r95: f32 = 0.0;
  var r96: f32 = 0.0;
  var r97: f32 = 0.0;
  var r98: f32 = 0.0;
  var r99: f32 = 0.0;
  var r100: f32 = 0.0;
  let r101: f32 = bitcast<f32>(0x0u);
  var r102: f32 = 0.0;
  var r103: f32 = 0.0;
  var r104: f32 = 0.0;
  var r105: f32 = 0.0;
  var r106: f32 = 0.0;
  var r107: f32 = 0.0;
  var r108: f32 = 0.0;
  var r109: f32 = 0.0;
  var r110: f32 = 0.0;
  var r111: f32 = 0.0;
  var r112: f32 = 0.0;
  var r113: f32 = 0.0;
  var r114: f32 = 0.0;
  var r115: f32 = 0.0;
  var r116: f32 = 0.0;
  var r117: f32 = 0.0;
  var r118: f32 = 0.0;
  var r119: f32 = 0.0;
  var r120: f32 = 0.0;
  var r121: f32 = 0.0;
  var r122: f32 = 0.0;
  var r123: f32 = 0.0;
  var r124: f32 = 0.0;
  let r125: f32 = bitcast<f32>(0x3f3504f3u);
  var r126: f32 = 0.0;
  let r127: f32 = bitcast<f32>(0x38d1b717u);
  var r128: f32 = 0.0;
  var r129: f32 = 0.0;
  var r130: f32 = 0.0;
  let r131: f32 = bitcast<f32>(0x0u);
  var r132: f32 = 0.0;
  let r133: f32 = bitcast<f32>(0x3f800000u);
  var r134: f32 = 0.0;
  var r135: f32 = 0.0;
  var r136: f32 = 0.0;
  let r137: f32 = bitcast<f32>(0x3f000000u);
  var r138: f32 = 0.0;
  var r139: f32 = 0.0;
  var r140: f32 = 0.0;
  var r141: f32 = 0.0;
  var r142: f32 = 0.0;
  let r143: f32 = bitcast<f32>(0x0u);
  var r144: f32 = 0.0;
  let r145: f32 = bitcast<f32>(0x3f800000u);
  var r146: f32 = 0.0;
  var r147: f32 = 0.0;
  var r148: f32 = 0.0;
  var r149: f32 = 0.0;
  var r150: f32 = 0.0;
  var r151: f32 = 0.0;
  var r152: f32 = 0.0;
  var r153: f32 = 0.0;
  var r154: f32 = 0.0;
  var r155: f32 = 0.0;
  var r156: f32 = 0.0;
  var r157: f32 = 0.0;
  var r158: f32 = 0.0;
  var r159: f32 = 0.0;
  var r160: f32 = 0.0;
  var r161: f32 = 0.0;
  var r162: f32 = 0.0;
  var r163: f32 = 0.0;
  var r164: f32 = 0.0;
  var r165: f32 = 0.0;
  var r166: f32 = 0.0;
  let r167: f32 = bitcast<f32>(0x0u);
  var r168: f32 = 0.0;
  var r169: f32 = 0.0;
  var r170: f32 = 0.0;
  var r171: f32 = 0.0;
  var r172: f32 = 0.0;
  var r173: f32 = 0.0;
  var r174: f32 = 0.0;
  var r175: f32 = 0.0;
  let r0: f32 = v.color.x;
  let r1: f32 = v.color.y;
  let r2: f32 = v.color.z;
  let r3: f32 = v.color.w;
  let r4: f32 = v.color2.x;
  let r5: f32 = v.color2.y;
  let r6: f32 = v.color2.z;
  let r7: f32 = v.color2.w;
  let r8: f32 = v.local.x;
  let r9: f32 = v.local.y;
  let r10: f32 = v.box.x;
  let r11: f32 = v.box.y;
  let r12: f32 = v.box.z;
  let r13: f32 = v.box.w;
  let r15: f32 = v.params.y;
  let r17: f32 = v.params.w;
  let r18: f32 = v.extra.x;
  let r19: f32 = v.extra.y;
  let r20: f32 = v.extra.z;
  let r21: f32 = v.extra.w;
  let r23: f32 = select(0.0, 1.0, (r17 == r22));
  if (r23 != 0.0) {
    r24 = (r20 - r18);
    r25 = (r21 - r19);
    r26 = (r24 * r24);
    r27 = (r25 * r25);
    r28 = (r26 + r27);
    r29 = sqrt(abs(r28));
    r30 = select(r24 / r29, 0.0, r29 == 0.0);
    r31 = select(r25 / r29, 0.0, r29 == 0.0);
    r32 = (r18 + r20);
    r33 = (r19 + r21);
    r35 = (r32 * r34);
    r36 = (r33 * r34);
    r37 = (r8 - r35);
    r38 = (r9 - r36);
    r39 = (r37 * r30);
    r40 = (r38 * r31);
    r41 = (r39 + r40);
    r43 = (r42 - r31);
    r44 = (r37 * r43);
    r45 = (r38 * r30);
    r46 = (r44 + r45);
    r47 = (r29 * r34);
    r48 = (r15 * r34);
    r49 = abs(r41);
    r50 = (r49 - r47);
    r51 = (r50 + r42);
    r52 = abs(r46);
    r53 = (r52 - r48);
    r54 = (r53 + r42);
    r55 = max(r51, r42);
    r56 = max(r54, r42);
    r57 = (r55 * r55);
    r58 = (r56 * r56);
    r59 = (r57 + r58);
    r60 = sqrt(abs(r59));
    r61 = max(r51, r54);
    r62 = min(r61, r42);
    r63 = (r60 + r62);
    r64 = (r63 - r42);
    r65 = fw2_0;
    r66 = fw2_1;
    r67 = (r65 * r65);
    r68 = (r66 * r66);
    r69 = (r67 + r68);
    r70 = sqrt(abs(r69));
    r72 = max(r70, r71);
    r74 = select(0.0, 1.0, (r29 <= r73));
    r75 = select(r64 / r72, 0.0, r72 == 0.0);
    r76 = (r34 - r75);
    r77 = max(r76, r42);
    r79 = min(r77, r78);
    r80 = (r3 * r79);
  }
  if (r23 == 0.0) {
    r81 = (r10 + r12);
    r82 = (r11 + r13);
    r84 = (r81 * r83);
    r85 = (r82 * r83);
    r86 = (r12 - r10);
    r87 = (r13 - r11);
    r88 = (r86 * r83);
    r89 = (r87 * r83);
    r91 = select(0.0, 1.0, (r17 == r90));
    if (r91 != 0.0) {
      r92 = (r8 - r84);
      r93 = (r9 - r85);
      r94 = (r92 * r92);
      r95 = (r93 * r93);
      r96 = (r94 + r95);
      r97 = sqrt(abs(r96));
      r98 = (r97 - r88);
    }
    if (r91 == 0.0) {
      r99 = (r8 - r84);
      r100 = (r9 - r85);
      r102 = abs(r99);
      r103 = (r102 - r88);
      r104 = (r103 + r101);
      r105 = abs(r100);
      r106 = (r105 - r89);
      r107 = (r106 + r101);
      r108 = max(r104, r101);
      r109 = max(r107, r101);
      r110 = (r108 * r108);
      r111 = (r109 * r109);
      r112 = (r110 + r111);
      r113 = sqrt(abs(r112));
      r114 = max(r104, r107);
      r115 = min(r114, r101);
      r116 = (r113 + r115);
      r117 = (r116 - r101);
    }
    r118 = select(r117, r98, r91 != 0.0);
    r119 = fw2_0;
    r120 = fw2_1;
    r121 = (r119 * r119);
    r122 = (r120 * r120);
    r123 = (r121 + r122);
    r124 = sqrt(abs(r123));
    r126 = (r124 * r125);
    r128 = max(r126, r127);
    r129 = select(r118 / r128, 0.0, r128 == 0.0);
    r130 = (r83 - r129);
    r132 = max(r130, r131);
    r134 = min(r132, r133);
    r135 = (r3 * r134);
    r136 = select(0.0, 1.0, (r15 <= r131));
    if (r136 == 0.0) {
      r138 = abs(r118);
      r139 = (r15 * r137);
      r140 = (r138 - r139);
      r141 = select(r140 / r128, 0.0, r128 == 0.0);
      r142 = (r137 - r141);
      r144 = max(r142, r143);
      r146 = min(r144, r145);
      r147 = (r7 * r146);
      r148 = (r145 - r147);
      r149 = (r135 * r148);
      r150 = (r147 + r149);
      r151 = (r4 * r147);
      r152 = (r5 * r147);
      r153 = (r6 * r147);
      r154 = (r0 * r135);
      r155 = (r1 * r135);
      r156 = (r2 * r135);
      r157 = (r154 * r148);
      r158 = (r155 * r148);
      r159 = (r156 * r148);
      r160 = (r151 + r157);
      r161 = (r152 + r158);
      r162 = (r153 + r159);
      r163 = select(r160 / r150, 0.0, r150 == 0.0);
      r164 = select(r161 / r150, 0.0, r150 == 0.0);
      r165 = select(r162 / r150, 0.0, r150 == 0.0);
      r166 = select(0.0, 1.0, (r150 <= r143));
      r168 = select(r163, r167, r166 != 0.0);
      r169 = select(r164, r167, r166 != 0.0);
      r170 = select(r165, r167, r166 != 0.0);
      r171 = select(r150, r167, r166 != 0.0);
    }
    r172 = select(r168, r0, r136 != 0.0);
    r173 = select(r169, r1, r136 != 0.0);
    r174 = select(r170, r2, r136 != 0.0);
    r175 = select(r171, r135, r136 != 0.0);
  }
  let r176: f32 = select(r131, r74, r23 != 0.0);
  let r177: f32 = select(r172, r0, r23 != 0.0);
  let r178: f32 = select(r173, r1, r23 != 0.0);
  let r179: f32 = select(r174, r2, r23 != 0.0);
  let r180: f32 = select(r175, r80, r23 != 0.0);
  colour.x = r177;
  colour.y = r178;
  colour.z = r179;
  colour.w = r180;
  if (r176 != 0.0) { discard; }
  return colour;
}
struct geometry_vertex_out {
  @builtin(position) position: vec4<f32>,
  @location(0) @interpolate(flat) ink: vec4<f32>,
}
@vertex
fn geometry_vertex(@builtin(vertex_index) vid: u32, @builtin(instance_index) iid: u32) -> geometry_vertex_out {
  var sb: u32 = 0u;
  var o: geometry_vertex_out;
  let r3: u32 = 6u;
  let r6: u32 = 1u;
  let r8: u32 = 2u;
  let r9: u32 = 3u;
  let r17: u32 = 0u;
  let r27: u32 = 4u;
  let r31: u32 = 5u;
  let r34: f32 = bitcast<f32>(0x0u);
  let r35: f32 = bitcast<f32>(0x3f800000u);
  let r36: u32 = 2u;
  var r37: u32 = 0u;
  var r38: vec2<u32> = vec2<u32>(0u, 0u);
  let r39: u32 = 24u;
  var r40: vec2<u32> = vec2<u32>(0u, 0u);
  let r41: u32 = 255u;
  var r42: u32 = 0u;
  var r43: f32 = 0.0;
  let r44: f32 = bitcast<f32>(0x437f0000u);
  var r45: f32 = 0.0;
  let r46: u32 = 16u;
  var r47: vec2<u32> = vec2<u32>(0u, 0u);
  var r48: u32 = 0u;
  var r49: f32 = 0.0;
  var r50: f32 = 0.0;
  let r51: u32 = 8u;
  var r52: vec2<u32> = vec2<u32>(0u, 0u);
  var r53: u32 = 0u;
  var r54: f32 = 0.0;
  var r55: f32 = 0.0;
  var r56: u32 = 0u;
  var r57: f32 = 0.0;
  var r58: f32 = 0.0;
  let r59: u32 = 8u;
  var r60: f32 = 0.0;
  let r61: u32 = 9u;
  var r62: f32 = 0.0;
  let r63: u32 = 10u;
  var r64: f32 = 0.0;
  let r65: u32 = 11u;
  var r66: f32 = 0.0;
  let r0: vec2<u32> = vec2<u32>(vid, 0u);
  let r4: f32 = buffer6[r3 * 1u + 0u];
  let r5: vec2<u32> = i64_from_f(r4);
  let r7: f32 = select(0.0, 1.0, all(r5 == vec2<u32>(r6, 0u)));
  let r10: u32 = select(r8, r9, r7 != 0.0);
  let r11: u32 = ((r0).x * r10);
    let r12: vec2<u32> = vec2<u32>(buffer0[r11 * 1u + 0u], 0u);
  let r13: f32 = bitcast<f32>((r12).x);
  let r14: u32 = (r11 + r6);
    let r15: vec2<u32> = vec2<u32>(buffer0[r14 * 1u + 0u], 0u);
  let r16: f32 = bitcast<f32>((r15).x);
  let r18: f32 = buffer6[r17 * 1u + 0u];
  let r19: f32 = (r18 * r13);
  let r20: f32 = buffer6[r6 * 1u + 0u];
  let r21: f32 = (r20 * r16);
  let r22: f32 = (r19 + r21);
  let r23: f32 = buffer6[r8 * 1u + 0u];
  let r24: f32 = (r22 + r23);
  let r25: f32 = buffer6[r9 * 1u + 0u];
  let r26: f32 = (r25 * r13);
  let r28: f32 = buffer6[r27 * 1u + 0u];
  let r29: f32 = (r28 * r16);
  let r30: f32 = (r26 + r29);
  let r32: f32 = buffer6[r31 * 1u + 0u];
  let r33: f32 = (r30 + r32);
  if (r7 != 0.0) {
    r37 = (r11 + r36);
        r38 = vec2<u32>(buffer0[r37 * 1u + 0u], 0u);
    r40 = i64_shr(r38, vec2<u32>(r39, 0u));
    if (vec2<u32>(r39, 0u).y != 0u || vec2<u32>(r39, 0u).x > 63u) { sb |= 4u; }
    r42 = ((r40).x & r41);
        r43 = i64_to_f(vec2<u32>(r42, 0u));
    r45 = select(r43 / r44, 0.0, r44 == 0.0);
    r47 = i64_shr(r38, vec2<u32>(r46, 0u));
    if (vec2<u32>(r46, 0u).y != 0u || vec2<u32>(r46, 0u).x > 63u) { sb |= 4u; }
    r48 = ((r47).x & r41);
        r49 = i64_to_f(vec2<u32>(r48, 0u));
    r50 = select(r49 / r44, 0.0, r44 == 0.0);
    r52 = i64_shr(r38, vec2<u32>(r51, 0u));
    if (vec2<u32>(r51, 0u).y != 0u || vec2<u32>(r51, 0u).x > 63u) { sb |= 4u; }
    r53 = ((r52).x & r41);
        r54 = i64_to_f(vec2<u32>(r53, 0u));
    r55 = select(r54 / r44, 0.0, r44 == 0.0);
    r56 = ((r38).x & r41);
        r57 = i64_to_f(vec2<u32>(r56, 0u));
    r58 = select(r57 / r44, 0.0, r44 == 0.0);
  }
  if (r7 == 0.0) {
    r60 = buffer6[r59 * 1u + 0u];
    r62 = buffer6[r61 * 1u + 0u];
    r64 = buffer6[r63 * 1u + 0u];
    r66 = buffer6[r65 * 1u + 0u];
  }
  let r67: f32 = select(r60, r45, r7 != 0.0);
  let r68: f32 = select(r62, r50, r7 != 0.0);
  let r69: f32 = select(r64, r55, r7 != 0.0);
  let r70: f32 = select(r66, r58, r7 != 0.0);
  o.position.x = r24;
  o.position.y = r33;
  o.position.z = r34;
  o.position.w = r35;
  o.ink.x = r67;
  o.ink.y = r68;
  o.ink.z = r69;
  o.ink.w = r70;
  return o;
}
@fragment
fn geometry_fragment(v: geometry_vertex_out) -> @location(0) vec4<f32> {
  var sb: u32 = 0u;
  var colour: vec4<f32>;
  let r0: f32 = v.ink.x;
  let r1: f32 = v.ink.y;
  let r2: f32 = v.ink.z;
  let r3: f32 = v.ink.w;
  colour.x = r0;
  colour.y = r1;
  colour.z = r2;
  colour.w = r3;
  return colour;
}
struct quad_vertex_out {
  @builtin(position) position: vec4<f32>,
  @location(0) uv: vec2<f32>,
  @location(1) @interpolate(flat) ink: vec4<f32>,
}
@vertex
fn quad_vertex(@builtin(vertex_index) vid: u32, @builtin(instance_index) iid: u32) -> quad_vertex_out {
  var sb: u32 = 0u;
  var o: quad_vertex_out;
  let r2: u32 = 4u;
  let r5: u32 = 20u;
  let r7: u32 = 1u;
  let r9: u32 = 2u;
  let r12: f32 = bitcast<f32>(0x3f800000u);
  let r13: f32 = bitcast<f32>(0x0u);
  let r16: f32 = bitcast<f32>(0x3f800000u);
  let r17: f32 = bitcast<f32>(0x0u);
  let r19: u32 = 12u;
  let r22: u32 = 14u;
  let r27: u32 = 13u;
  let r30: u32 = 15u;
  let r35: u32 = 0u;
  let r46: u32 = 3u;
  let r54: u32 = 5u;
  let r58: f32 = bitcast<f32>(0x0u);
  let r59: f32 = bitcast<f32>(0x3f800000u);
  let r60: u32 = 16u;
  let r63: u32 = 18u;
  let r70: u32 = 17u;
  let r73: u32 = 19u;
  let r80: u32 = 8u;
  let r83: u32 = 9u;
  let r86: u32 = 10u;
  let r89: u32 = 11u;
  let r0: vec2<u32> = vec2<u32>(iid * 4u + quad_corner[vid], 0u);
  let r3: vec2<u32> = i64_divmod(r0, vec2<u32>(r2, 0u), true);
  if (all(vec2<u32>(r2, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r4: vec2<u32> = i64_divmod(r0, vec2<u32>(r2, 0u), false);
  if (all(vec2<u32>(r2, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r6: u32 = ((r4).x * r5);
    let r8: f32 = select(0.0, 1.0, all(r3 == vec2<u32>(r7, 0u)));
  let r10: f32 = select(0.0, 1.0, all(r3 == vec2<u32>(r9, 0u)));
  let r11: f32 = select(0.0, 1.0, (r8 != 0.0 || r10 != 0.0));
  let r14: f32 = select(r13, r12, r11 != 0.0);
  let r15: f32 = select(0.0, 1.0, !i64_lt(r3, vec2<u32>(r9, 0u)));
  let r18: f32 = select(r17, r16, r15 != 0.0);
  let r20: u32 = (r6 + r19);
    let r21: f32 = buffer6[r20 * 1u + 0u];
  let r23: u32 = (r6 + r22);
    let r24: f32 = buffer6[r23 * 1u + 0u];
  let r25: f32 = (r24 * r14);
  let r26: f32 = (r21 + r25);
  let r28: u32 = (r6 + r27);
    let r29: f32 = buffer6[r28 * 1u + 0u];
  let r31: u32 = (r6 + r30);
    let r32: f32 = buffer6[r31 * 1u + 0u];
  let r33: f32 = (r32 * r18);
  let r34: f32 = (r29 + r33);
  let r36: u32 = (r6 + r35);
    let r37: f32 = buffer6[r36 * 1u + 0u];
  let r38: f32 = (r37 * r26);
  let r39: u32 = (r6 + r7);
    let r40: f32 = buffer6[r39 * 1u + 0u];
  let r41: f32 = (r40 * r34);
  let r42: f32 = (r38 + r41);
  let r43: u32 = (r6 + r9);
    let r44: f32 = buffer6[r43 * 1u + 0u];
  let r45: f32 = (r42 + r44);
  let r47: u32 = (r6 + r46);
    let r48: f32 = buffer6[r47 * 1u + 0u];
  let r49: f32 = (r48 * r26);
  let r50: u32 = (r6 + r2);
    let r51: f32 = buffer6[r50 * 1u + 0u];
  let r52: f32 = (r51 * r34);
  let r53: f32 = (r49 + r52);
  let r55: u32 = (r6 + r54);
    let r56: f32 = buffer6[r55 * 1u + 0u];
  let r57: f32 = (r53 + r56);
  let r61: u32 = (r6 + r60);
    let r62: f32 = buffer6[r61 * 1u + 0u];
  let r64: u32 = (r6 + r63);
    let r65: f32 = buffer6[r64 * 1u + 0u];
  let r66: f32 = (r59 - r14);
  let r67: f32 = (r62 * r66);
  let r68: f32 = (r65 * r14);
  let r69: f32 = (r67 + r68);
  let r71: u32 = (r6 + r70);
    let r72: f32 = buffer6[r71 * 1u + 0u];
  let r74: u32 = (r6 + r73);
    let r75: f32 = buffer6[r74 * 1u + 0u];
  let r76: f32 = (r59 - r18);
  let r77: f32 = (r72 * r76);
  let r78: f32 = (r75 * r18);
  let r79: f32 = (r77 + r78);
  let r81: u32 = (r6 + r80);
    let r82: f32 = buffer6[r81 * 1u + 0u];
  let r84: u32 = (r6 + r83);
    let r85: f32 = buffer6[r84 * 1u + 0u];
  let r87: u32 = (r6 + r86);
    let r88: f32 = buffer6[r87 * 1u + 0u];
  let r90: u32 = (r6 + r89);
    let r91: f32 = buffer6[r90 * 1u + 0u];
  o.position.x = r45;
  o.position.y = r57;
  o.position.z = r58;
  o.position.w = r59;
  o.uv.x = r69;
  o.uv.y = r79;
  o.ink.x = r82;
  o.ink.y = r85;
  o.ink.z = r88;
  o.ink.w = r91;
  return o;
}
@fragment
fn quad_fragment(v: quad_vertex_out) -> @location(0) vec4<f32> {
  var sb: u32 = 0u;
  var colour: vec4<f32>;
  let r0: f32 = v.ink.x;
  let r1: f32 = v.ink.y;
  let r2: f32 = v.ink.z;
  let r3: f32 = v.ink.w;
  let r4: f32 = v.uv.x;
  let r5: f32 = v.uv.y;
  let r6: f32 = textureSample(texture7, sampler8, vec2<f32>(r4, r5)).x;
  let r7: f32 = textureSample(texture7, sampler8, vec2<f32>(r4, r5)).y;
  let r8: f32 = textureSample(texture7, sampler8, vec2<f32>(r4, r5)).z;
  let r9: f32 = textureSample(texture7, sampler8, vec2<f32>(r4, r5)).w;
  let r10: f32 = (r0 * r6);
  let r11: f32 = (r1 * r7);
  let r12: f32 = (r2 * r8);
  let r13: f32 = (r3 * r9);
  colour.x = r10;
  colour.y = r11;
  colour.z = r12;
  colour.w = r13;
  return colour;
}
