@group(0) @binding(0) var<storage, read> buffer0: array<u32>;
@group(0) @binding(1) var<storage, read> buffer1: array<f32>;
@group(0) @binding(2) var<storage, read> buffer2: array<u32>;
@group(0) @binding(3) var<storage, read> buffer3: array<u32>;
@group(0) @binding(4) var<storage, read> buffer4: array<f32>;
@group(0) @binding(5) var<storage, read> buffer5: array<u32>;
@group(0) @binding(6) var<storage, read> buffer6: array<f32>;
@group(0) @binding(7) var texture7: texture_2d<f32>;
@group(0) @binding(8) var sampler8: sampler;
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
  @location(0) local: vec2<f32>,
  @location(1) @interpolate(flat) color: vec4<f32>,
  @location(2) @interpolate(flat) color2: vec4<f32>,
  @location(3) @interpolate(flat) shape: vec4<f32>,
}
@vertex
fn marks_vertex(@builtin(vertex_index) vid: u32, @builtin(instance_index) iid: u32) -> marks_vertex_out {
  var sb: u32 = 0u;
  var o: marks_vertex_out;
  let r8: u32 = 2u;
  let r10: u32 = 3u;
  let r12: u32 = 1u;
  let r16: f32 = bitcast<f32>(0x3f800000u);
  let r17: f32 = bitcast<f32>(0x0u);
  let r20: f32 = bitcast<f32>(0x3f800000u);
  let r21: f32 = bitcast<f32>(0x0u);
  let r23: u32 = 0u;
  let r32: u32 = 5u;
  let r36: u32 = 4u;
  let r39: u32 = 7u;
  var r42: vec2<u32> = vec2<u32>(0u, 0u);
  var r43: vec2<u32> = vec2<u32>(0u, 0u);
  var r44: f32 = 0.0;
  let r45: f32 = bitcast<f32>(0x0u);
  var r46: f32 = 0.0;
  let r47: f32 = bitcast<f32>(0x3f800000u);
  var r48: f32 = 0.0;
  let r49: f32 = bitcast<f32>(0x437f0000u);
  var r50: f32 = 0.0;
  let r51: f32 = bitcast<f32>(0x3f000000u);
  var r52: f32 = 0.0;
  var r53: vec2<u32> = vec2<u32>(0u, 0u);
  let r54: u32 = 24u;
  var r55: vec2<u32> = vec2<u32>(0u, 0u);
  let r56: u32 = 1u;
  var r57: u32 = 0u;
  var r58: vec2<u32> = vec2<u32>(0u, 0u);
  var r59: f32 = 0.0;
  var r60: f32 = 0.0;
  var r61: f32 = 0.0;
  var r62: f32 = 0.0;
  var r63: f32 = 0.0;
  var r64: vec2<u32> = vec2<u32>(0u, 0u);
  let r65: u32 = 16u;
  var r66: vec2<u32> = vec2<u32>(0u, 0u);
  var r67: vec2<u32> = vec2<u32>(0u, 0u);
  let r68: u32 = 2u;
  var r69: u32 = 0u;
  var r70: vec2<u32> = vec2<u32>(0u, 0u);
  var r71: f32 = 0.0;
  var r72: f32 = 0.0;
  var r73: f32 = 0.0;
  var r74: f32 = 0.0;
  var r75: f32 = 0.0;
  var r76: vec2<u32> = vec2<u32>(0u, 0u);
  let r77: u32 = 8u;
  var r78: vec2<u32> = vec2<u32>(0u, 0u);
  let r79: u32 = 255u;
  var r80: vec2<u32> = vec2<u32>(0u, 0u);
  var r81: vec2<u32> = vec2<u32>(0u, 0u);
  let r83: f32 = bitcast<f32>(0x40000000u);
  let r84: f32 = bitcast<f32>(0x0u);
  let r85: f32 = bitcast<f32>(0x3f800000u);
  var r87: f32 = 0.0;
  let r88: u32 = 1u;
  var r89: u32 = 0u;
  var r90: f32 = 0.0;
  var r91: f32 = 0.0;
  var r92: f32 = 0.0;
  var r93: f32 = 0.0;
  var r94: f32 = 0.0;
  var r95: f32 = 0.0;
  var r96: f32 = 0.0;
  let r97: f32 = bitcast<f32>(0x358637bdu);
  var r98: f32 = 0.0;
  var r99: f32 = 0.0;
  var r100: f32 = 0.0;
  let r101: f32 = bitcast<f32>(0x3f800000u);
  let r102: f32 = bitcast<f32>(0x0u);
  var r103: f32 = 0.0;
  var r104: f32 = 0.0;
  let r105: f32 = bitcast<f32>(0x0u);
  var r106: f32 = 0.0;
  let r107: f32 = bitcast<f32>(0x3f000000u);
  var r108: f32 = 0.0;
  let r109: f32 = bitcast<f32>(0x3fc00000u);
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
  let r123: f32 = bitcast<f32>(0x40000000u);
  var r124: f32 = 0.0;
  let r125: f32 = bitcast<f32>(0x3f800000u);
  var r126: f32 = 0.0;
  var r127: f32 = 0.0;
  var r128: f32 = 0.0;
  var r129: f32 = 0.0;
  var r130: f32 = 0.0;
  var r131: f32 = 0.0;
  let r132: u32 = 0u;
  var r133: f32 = 0.0;
  var r134: f32 = 0.0;
  let r135: u32 = 1u;
  var r136: f32 = 0.0;
  var r137: f32 = 0.0;
  var r138: f32 = 0.0;
  let r139: u32 = 2u;
  var r140: f32 = 0.0;
  var r141: f32 = 0.0;
  let r142: u32 = 3u;
  var r143: f32 = 0.0;
  var r144: f32 = 0.0;
  let r145: u32 = 4u;
  var r146: f32 = 0.0;
  var r147: f32 = 0.0;
  var r148: f32 = 0.0;
  let r149: u32 = 5u;
  var r150: f32 = 0.0;
  var r151: f32 = 0.0;
  let r152: f32 = bitcast<f32>(0x0u);
  let r153: f32 = bitcast<f32>(0x3f800000u);
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
  let r164: u32 = 24u;
  var r165: vec2<u32> = vec2<u32>(0u, 0u);
  let r166: u32 = 255u;
  var r167: u32 = 0u;
  var r168: f32 = 0.0;
  let r169: f32 = bitcast<f32>(0x437f0000u);
  var r170: f32 = 0.0;
  let r171: u32 = 16u;
  var r172: vec2<u32> = vec2<u32>(0u, 0u);
  var r173: u32 = 0u;
  var r174: f32 = 0.0;
  var r175: f32 = 0.0;
  let r176: u32 = 8u;
  var r177: vec2<u32> = vec2<u32>(0u, 0u);
  var r178: u32 = 0u;
  var r179: f32 = 0.0;
  var r180: f32 = 0.0;
  var r181: u32 = 0u;
  var r182: f32 = 0.0;
  var r183: f32 = 0.0;
  var r184: f32 = 0.0;
  let r185: f32 = bitcast<f32>(0x40a00000u);
  let r186: u32 = 4u;
  var r187: f32 = 0.0;
  var r188: f32 = 0.0;
  var r189: f32 = 0.0;
  var r190: f32 = 0.0;
  var r191: f32 = 0.0;
  var r192: f32 = 0.0;
  var r193: f32 = 0.0;
  var r194: f32 = 0.0;
  let r195: u32 = 1u;
  var r196: u32 = 0u;
  var r197: f32 = 0.0;
  var r198: f32 = 0.0;
  var r199: f32 = 0.0;
  var r200: f32 = 0.0;
  var r201: f32 = 0.0;
  var r202: f32 = 0.0;
  let r203: u32 = 0u;
  var r204: f32 = 0.0;
  let r205: f32 = bitcast<f32>(0x0u);
  var r206: f32 = 0.0;
  let r207: f32 = bitcast<f32>(0x3f800000u);
  let r208: f32 = bitcast<f32>(0x3f000000u);
  var r209: f32 = 0.0;
  var r210: f32 = 0.0;
  var r211: f32 = 0.0;
  var r212: f32 = 0.0;
  var r213: f32 = 0.0;
  var r214: f32 = 0.0;
  var r215: f32 = 0.0;
  var r216: f32 = 0.0;
  var r217: f32 = 0.0;
  var r218: f32 = 0.0;
  var r219: f32 = 0.0;
  var r220: f32 = 0.0;
  var r221: f32 = 0.0;
  var r222: f32 = 0.0;
  var r223: f32 = 0.0;
  var r224: f32 = 0.0;
  let r225: f32 = bitcast<f32>(0x0u);
  var r226: f32 = 0.0;
  var r227: f32 = 0.0;
  var r228: f32 = 0.0;
  var r229: f32 = 0.0;
  var r230: f32 = 0.0;
  let r231: u32 = 0u;
  var r232: f32 = 0.0;
  var r233: f32 = 0.0;
  let r234: u32 = 1u;
  var r235: f32 = 0.0;
  var r236: f32 = 0.0;
  var r237: f32 = 0.0;
  let r238: u32 = 2u;
  var r239: f32 = 0.0;
  var r240: f32 = 0.0;
  let r241: u32 = 3u;
  var r242: f32 = 0.0;
  var r243: f32 = 0.0;
  let r244: u32 = 4u;
  var r245: f32 = 0.0;
  var r246: f32 = 0.0;
  var r247: f32 = 0.0;
  let r248: u32 = 5u;
  var r249: f32 = 0.0;
  var r250: f32 = 0.0;
  let r251: f32 = bitcast<f32>(0x0u);
  let r252: f32 = bitcast<f32>(0x3f800000u);
  var r253: f32 = 0.0;
  var r254: f32 = 0.0;
  var r255: f32 = 0.0;
  var r256: f32 = 0.0;
  var r257: f32 = 0.0;
  var r258: f32 = 0.0;
  var r259: f32 = 0.0;
  var r260: f32 = 0.0;
  var r261: f32 = 0.0;
  var r262: f32 = 0.0;
  let r263: u32 = 3u;
  var r264: vec2<u32> = vec2<u32>(0u, 0u);
  var r265: u32 = 0u;
  let r266: u32 = 6u;
  var r267: vec2<u32> = vec2<u32>(0u, 0u);
  var r268: f32 = 0.0;
  var r269: vec2<u32> = vec2<u32>(0u, 0u);
  var r270: vec2<u32> = vec2<u32>(0u, 0u);
  var r271: f32 = 0.0;
  let r272: f32 = bitcast<f32>(0x0u);
  var r273: f32 = 0.0;
  let r274: f32 = bitcast<f32>(0x3f800000u);
  var r275: f32 = 0.0;
  let r276: f32 = bitcast<f32>(0x437f0000u);
  var r277: f32 = 0.0;
  let r278: f32 = bitcast<f32>(0x3f000000u);
  var r279: f32 = 0.0;
  var r280: vec2<u32> = vec2<u32>(0u, 0u);
  let r281: u32 = 24u;
  var r282: vec2<u32> = vec2<u32>(0u, 0u);
  let r283: u32 = 1u;
  var r284: u32 = 0u;
  var r285: vec2<u32> = vec2<u32>(0u, 0u);
  var r286: f32 = 0.0;
  var r287: f32 = 0.0;
  var r288: f32 = 0.0;
  var r289: f32 = 0.0;
  var r290: f32 = 0.0;
  var r291: vec2<u32> = vec2<u32>(0u, 0u);
  let r292: u32 = 16u;
  var r293: vec2<u32> = vec2<u32>(0u, 0u);
  var r294: vec2<u32> = vec2<u32>(0u, 0u);
  let r295: u32 = 2u;
  var r296: u32 = 0u;
  var r297: vec2<u32> = vec2<u32>(0u, 0u);
  var r298: f32 = 0.0;
  var r299: f32 = 0.0;
  var r300: f32 = 0.0;
  var r301: f32 = 0.0;
  var r302: f32 = 0.0;
  var r303: vec2<u32> = vec2<u32>(0u, 0u);
  let r304: u32 = 8u;
  var r305: vec2<u32> = vec2<u32>(0u, 0u);
  let r306: u32 = 255u;
  var r307: vec2<u32> = vec2<u32>(0u, 0u);
  var r308: vec2<u32> = vec2<u32>(0u, 0u);
  var r309: vec2<u32> = vec2<u32>(0u, 0u);
  let r310: u32 = 24u;
  var r311: vec2<u32> = vec2<u32>(0u, 0u);
  let r312: u32 = 255u;
  var r313: u32 = 0u;
  var r314: f32 = 0.0;
  let r315: f32 = bitcast<f32>(0x437f0000u);
  var r316: f32 = 0.0;
  let r317: u32 = 16u;
  var r318: vec2<u32> = vec2<u32>(0u, 0u);
  var r319: u32 = 0u;
  var r320: f32 = 0.0;
  var r321: f32 = 0.0;
  let r322: u32 = 8u;
  var r323: vec2<u32> = vec2<u32>(0u, 0u);
  var r324: u32 = 0u;
  var r325: f32 = 0.0;
  var r326: f32 = 0.0;
  var r327: u32 = 0u;
  var r328: f32 = 0.0;
  var r329: f32 = 0.0;
  var r330: vec2<u32> = vec2<u32>(0u, 0u);
  var r331: u32 = 0u;
  var r332: f32 = 0.0;
  var r333: f32 = 0.0;
  var r334: vec2<u32> = vec2<u32>(0u, 0u);
  var r335: u32 = 0u;
  var r336: f32 = 0.0;
  var r337: f32 = 0.0;
  var r338: vec2<u32> = vec2<u32>(0u, 0u);
  var r339: u32 = 0u;
  var r340: f32 = 0.0;
  var r341: f32 = 0.0;
  var r342: u32 = 0u;
  var r343: f32 = 0.0;
  var r344: f32 = 0.0;
  var r345: f32 = 0.0;
  let r0: vec2<u32> = vec2<u32>(iid * 4u + quad_corner[vid], 0u);
  let r9: vec2<u32> = i64_shr(r0, vec2<u32>(r8, 0u));
  if (vec2<u32>(r8, 0u).y != 0u || vec2<u32>(r8, 0u).x > 63u) { sb |= 4u; }
  let r11: u32 = ((r0).x & r10);
    let r13: f32 = select(0.0, 1.0, all(vec2<u32>(r11, 0u) == vec2<u32>(r12, 0u)));
  let r14: f32 = select(0.0, 1.0, all(vec2<u32>(r11, 0u) == vec2<u32>(r8, 0u)));
  let r15: f32 = select(0.0, 1.0, (r13 != 0.0 || r14 != 0.0));
  let r18: f32 = select(r17, r16, r15 != 0.0);
  let r19: f32 = select(0.0, 1.0, !i64_lt(vec2<u32>(r11, 0u), vec2<u32>(r8, 0u)));
  let r22: f32 = select(r21, r20, r19 != 0.0);
  let r24: vec2<u32> = vec2<u32>(buffer5[r23 * 1u + 0u], 0u);
  let r25: vec2<u32> = vec2<u32>(buffer5[r12 * 1u + 0u], 0u);
  let r26: u32 = ((r9).x * (r25).x);
    let r27: vec2<u32> = vec2<u32>(buffer5[r8 * 1u + 0u], 0u);
  let r28: u32 = ((r9).x * (r27).x);
    let r29: f32 = bitcast<f32>(buffer0[r26 * 1u + 0u]);
  let r30: u32 = (r26 + r12);
    let r31: f32 = bitcast<f32>(buffer0[r30 * 1u + 0u]);
  let r33: vec2<u32> = vec2<u32>(buffer5[r32 * 1u + 0u], 0u);
  let r34: u32 = ((r9).x * (r33).x);
    let r35: f32 = buffer4[r34 * 1u + 0u];
  let r37: vec2<u32> = vec2<u32>(buffer5[r36 * 1u + 0u], 0u);
  let r38: u32 = ((r9).x * (r37).x);
    let r40: vec2<u32> = vec2<u32>(buffer5[r39 * 1u + 0u], 0u);
  let r41: f32 = select(0.0, 1.0, all(r40 == vec2<u32>(r23, 0u)));
  if (r41 != 0.0) {
    r42 = vec2<u32>(buffer3[r38 * 1u + 0u], 0u);
  }
  if (r41 == 0.0) {
    r43 = vec2<u32>(buffer3[r38 * 1u + 0u], 0u);
    r44 = bitcast<f32>((r43).x);
    r46 = max(r44, r45);
    r48 = min(r46, r47);
    r50 = (r48 * r49);
    r52 = (r50 + r51);
    r53 = i64_from_f(floor(r52));
    r55 = i64_shl(r53, vec2<u32>(r54, 0u));
    if (vec2<u32>(r54, 0u).y != 0u || vec2<u32>(r54, 0u).x > 63u) { sb |= 4u; }
    r57 = (r38 + r56);
        r58 = vec2<u32>(buffer3[r57 * 1u + 0u], 0u);
    r59 = bitcast<f32>((r58).x);
    r60 = max(r59, r45);
    r61 = min(r60, r47);
    r62 = (r61 * r49);
    r63 = (r62 + r51);
    r64 = i64_from_f(floor(r63));
    r66 = i64_shl(r64, vec2<u32>(r65, 0u));
    if (vec2<u32>(r65, 0u).y != 0u || vec2<u32>(r65, 0u).x > 63u) { sb |= 4u; }
    r67 = (r55 | r66);
        r69 = (r38 + r68);
        r70 = vec2<u32>(buffer3[r69 * 1u + 0u], 0u);
    r71 = bitcast<f32>((r70).x);
    r72 = max(r71, r45);
    r73 = min(r72, r47);
    r74 = (r73 * r49);
    r75 = (r74 + r51);
    r76 = i64_from_f(floor(r75));
    r78 = i64_shl(r76, vec2<u32>(r77, 0u));
    if (vec2<u32>(r77, 0u).y != 0u || vec2<u32>(r77, 0u).x > 63u) { sb |= 4u; }
    r80 = (r78 | vec2<u32>(r79, 0u));
        r81 = (r67 | r80);
      }
  let r82: vec2<u32> = select(r81, r42, r41 != 0.0);
  let r86: f32 = select(0.0, 1.0, all(r24 == vec2<u32>(r32, 0u)));
  if (r86 != 0.0) {
    r87 = buffer1[r28 * 1u + 0u];
    r89 = (r28 + r88);
        r90 = buffer1[r89 * 1u + 0u];
    r91 = (r87 - r29);
    r92 = (r90 - r31);
    r93 = (r91 * r91);
    r94 = (r92 * r92);
    r95 = (r93 + r94);
    r96 = sqrt(abs(r95));
    r98 = select(0.0, 1.0, (r96 > r97));
    if (r98 != 0.0) {
      r99 = select(r91 / r96, 0.0, r96 == 0.0);
      r100 = select(r92 / r96, 0.0, r96 == 0.0);
    }
    r103 = select(r101, r99, r98 != 0.0);
    r104 = select(r102, r100, r98 != 0.0);
    r106 = (r105 - r104);
    r108 = (r35 * r107);
    r110 = (r108 + r109);
    r111 = (r103 * r110);
    r112 = (r104 * r110);
    r113 = (r29 - r111);
    r114 = (r31 - r112);
    r115 = (r87 + r111);
    r116 = (r90 + r112);
    r117 = (r115 - r113);
    r118 = (r116 - r114);
    r119 = (r117 * r18);
    r120 = (r118 * r18);
    r121 = (r113 + r119);
    r122 = (r114 + r120);
    r124 = (r22 * r123);
    r126 = (r124 - r125);
    r127 = (r126 * r110);
    r128 = (r106 * r127);
    r129 = (r103 * r127);
    r130 = (r121 + r128);
    r131 = (r122 + r129);
    if (r98 != 0.0) {
      r133 = buffer6[r132 * 1u + 0u];
      r134 = (r133 * r130);
      r136 = buffer6[r135 * 1u + 0u];
      r137 = (r136 * r131);
      r138 = (r134 + r137);
      r140 = buffer6[r139 * 1u + 0u];
      r141 = (r138 + r140);
      r143 = buffer6[r142 * 1u + 0u];
      r144 = (r143 * r130);
      r146 = buffer6[r145 * 1u + 0u];
      r147 = (r146 * r131);
      r148 = (r144 + r147);
      r150 = buffer6[r149 * 1u + 0u];
      r151 = (r148 + r150);
    }
    r154 = select(r83, r141, r98 != 0.0);
    r155 = select(r83, r151, r98 != 0.0);
    r156 = select(r84, r152, r98 != 0.0);
    r157 = select(r85, r153, r98 != 0.0);
    r158 = (r29 + r87);
    r159 = (r31 + r90);
    r160 = (r158 * r107);
    r161 = (r159 * r107);
    r162 = (r130 - r160);
    r163 = (r131 - r161);
    r165 = i64_shr(r82, vec2<u32>(r164, 0u));
    if (vec2<u32>(r164, 0u).y != 0u || vec2<u32>(r164, 0u).x > 63u) { sb |= 4u; }
    r167 = ((r165).x & r166);
        r168 = i64_to_f(vec2<u32>(r167, 0u));
    r170 = select(r168 / r169, 0.0, r169 == 0.0);
    r172 = i64_shr(r82, vec2<u32>(r171, 0u));
    if (vec2<u32>(r171, 0u).y != 0u || vec2<u32>(r171, 0u).x > 63u) { sb |= 4u; }
    r173 = ((r172).x & r166);
        r174 = i64_to_f(vec2<u32>(r173, 0u));
    r175 = select(r174 / r169, 0.0, r169 == 0.0);
    r177 = i64_shr(r82, vec2<u32>(r176, 0u));
    if (vec2<u32>(r176, 0u).y != 0u || vec2<u32>(r176, 0u).x > 63u) { sb |= 4u; }
    r178 = ((r177).x & r166);
        r179 = i64_to_f(vec2<u32>(r178, 0u));
    r180 = select(r179 / r169, 0.0, r169 == 0.0);
    r181 = ((r82).x & r166);
        r182 = i64_to_f(vec2<u32>(r181, 0u));
    r183 = select(r182 / r169, 0.0, r169 == 0.0);
    r184 = (r96 * r107);
  }
  if (r86 == 0.0) {
    r187 = select(0.0, 1.0, all(r24 == vec2<u32>(r186, 0u)));
    if (r187 != 0.0) {
      r188 = buffer1[r28 * 1u + 0u];
      r189 = (r29 - r188);
      r190 = (r31 - r188);
      r191 = (r29 + r188);
      r192 = (r31 + r188);
    }
    if (r187 == 0.0) {
      r193 = buffer1[r28 * 1u + 0u];
      r194 = (r29 + r193);
      r196 = (r28 + r195);
            r197 = buffer1[r196 * 1u + 0u];
      r198 = (r31 + r197);
    }
    r199 = select(r29, r189, r187 != 0.0);
    r200 = select(r31, r190, r187 != 0.0);
    r201 = select(r194, r191, r187 != 0.0);
    r202 = select(r198, r192, r187 != 0.0);
    r204 = select(0.0, 1.0, all(r82 == vec2<u32>(r203, 0u)));
    r206 = select(r35, r205, r204 != 0.0);
    r209 = (r206 * r208);
    r210 = (r207 + r209);
    r211 = (r199 - r210);
    r212 = (r200 - r210);
    r213 = (r201 + r210);
    r214 = (r202 + r210);
    r215 = (r213 - r211);
    r216 = (r215 * r18);
    r217 = (r211 + r216);
    r218 = (r214 - r212);
    r219 = (r218 * r22);
    r220 = (r212 + r219);
    r221 = (r201 - r199);
    r222 = (r221 * r208);
    r223 = (r202 - r200);
    r224 = (r223 * r208);
    r226 = select(0.0, 1.0, (r206 > r225));
    r227 = select(0.0, 1.0, (r222 > r225));
    r228 = select(0.0, 1.0, (r224 > r225));
    r229 = select(0.0, 1.0, (r227 != 0.0 && r228 != 0.0));
    r230 = select(0.0, 1.0, (r226 != 0.0 || r229 != 0.0));
    if (r230 != 0.0) {
      r232 = buffer6[r231 * 1u + 0u];
      r233 = (r232 * r217);
      r235 = buffer6[r234 * 1u + 0u];
      r236 = (r235 * r220);
      r237 = (r233 + r236);
      r239 = buffer6[r238 * 1u + 0u];
      r240 = (r237 + r239);
      r242 = buffer6[r241 * 1u + 0u];
      r243 = (r242 * r217);
      r245 = buffer6[r244 * 1u + 0u];
      r246 = (r245 * r220);
      r247 = (r243 + r246);
      r249 = buffer6[r248 * 1u + 0u];
      r250 = (r247 + r249);
    }
    r253 = select(r83, r240, r230 != 0.0);
    r254 = select(r83, r250, r230 != 0.0);
    r255 = select(r84, r251, r230 != 0.0);
    r256 = select(r85, r252, r230 != 0.0);
    r257 = (r199 + r201);
    r258 = (r257 * r208);
    r259 = (r200 + r202);
    r260 = (r259 * r208);
    r261 = (r217 - r258);
    r262 = (r220 - r260);
    r264 = vec2<u32>(buffer5[r263 * 1u + 0u], 0u);
    r265 = ((r9).x * (r264).x);
        r267 = vec2<u32>(buffer5[r266 * 1u + 0u], 0u);
    r268 = select(0.0, 1.0, all(r267 == vec2<u32>(r203, 0u)));
    if (r268 != 0.0) {
      r269 = vec2<u32>(buffer2[r265 * 1u + 0u], 0u);
    }
    if (r268 == 0.0) {
      r270 = vec2<u32>(buffer2[r265 * 1u + 0u], 0u);
      r271 = bitcast<f32>((r270).x);
      r273 = max(r271, r272);
      r275 = min(r273, r274);
      r277 = (r275 * r276);
      r279 = (r277 + r278);
      r280 = i64_from_f(floor(r279));
      r282 = i64_shl(r280, vec2<u32>(r281, 0u));
      if (vec2<u32>(r281, 0u).y != 0u || vec2<u32>(r281, 0u).x > 63u) { sb |= 4u; }
      r284 = (r265 + r283);
            r285 = vec2<u32>(buffer2[r284 * 1u + 0u], 0u);
      r286 = bitcast<f32>((r285).x);
      r287 = max(r286, r272);
      r288 = min(r287, r274);
      r289 = (r288 * r276);
      r290 = (r289 + r278);
      r291 = i64_from_f(floor(r290));
      r293 = i64_shl(r291, vec2<u32>(r292, 0u));
      if (vec2<u32>(r292, 0u).y != 0u || vec2<u32>(r292, 0u).x > 63u) { sb |= 4u; }
      r294 = (r282 | r293);
            r296 = (r265 + r295);
            r297 = vec2<u32>(buffer2[r296 * 1u + 0u], 0u);
      r298 = bitcast<f32>((r297).x);
      r299 = max(r298, r272);
      r300 = min(r299, r274);
      r301 = (r300 * r276);
      r302 = (r301 + r278);
      r303 = i64_from_f(floor(r302));
      r305 = i64_shl(r303, vec2<u32>(r304, 0u));
      if (vec2<u32>(r304, 0u).y != 0u || vec2<u32>(r304, 0u).x > 63u) { sb |= 4u; }
      r307 = (r305 | vec2<u32>(r306, 0u));
            r308 = (r294 | r307);
          }
    r309 = select(r308, r269, r268 != 0.0);
    r311 = i64_shr(r309, vec2<u32>(r310, 0u));
    if (vec2<u32>(r310, 0u).y != 0u || vec2<u32>(r310, 0u).x > 63u) { sb |= 4u; }
    r313 = ((r311).x & r312);
        r314 = i64_to_f(vec2<u32>(r313, 0u));
    r316 = select(r314 / r315, 0.0, r315 == 0.0);
    r318 = i64_shr(r309, vec2<u32>(r317, 0u));
    if (vec2<u32>(r317, 0u).y != 0u || vec2<u32>(r317, 0u).x > 63u) { sb |= 4u; }
    r319 = ((r318).x & r312);
        r320 = i64_to_f(vec2<u32>(r319, 0u));
    r321 = select(r320 / r315, 0.0, r315 == 0.0);
    r323 = i64_shr(r309, vec2<u32>(r322, 0u));
    if (vec2<u32>(r322, 0u).y != 0u || vec2<u32>(r322, 0u).x > 63u) { sb |= 4u; }
    r324 = ((r323).x & r312);
        r325 = i64_to_f(vec2<u32>(r324, 0u));
    r326 = select(r325 / r315, 0.0, r315 == 0.0);
    r327 = ((r309).x & r312);
        r328 = i64_to_f(vec2<u32>(r327, 0u));
    r329 = select(r328 / r315, 0.0, r315 == 0.0);
    r330 = i64_shr(r82, vec2<u32>(r310, 0u));
    if (vec2<u32>(r310, 0u).y != 0u || vec2<u32>(r310, 0u).x > 63u) { sb |= 4u; }
    r331 = ((r330).x & r312);
        r332 = i64_to_f(vec2<u32>(r331, 0u));
    r333 = select(r332 / r315, 0.0, r315 == 0.0);
    r334 = i64_shr(r82, vec2<u32>(r317, 0u));
    if (vec2<u32>(r317, 0u).y != 0u || vec2<u32>(r317, 0u).x > 63u) { sb |= 4u; }
    r335 = ((r334).x & r312);
        r336 = i64_to_f(vec2<u32>(r335, 0u));
    r337 = select(r336 / r315, 0.0, r315 == 0.0);
    r338 = i64_shr(r82, vec2<u32>(r322, 0u));
    if (vec2<u32>(r322, 0u).y != 0u || vec2<u32>(r322, 0u).x > 63u) { sb |= 4u; }
    r339 = ((r338).x & r312);
        r340 = i64_to_f(vec2<u32>(r339, 0u));
    r341 = select(r340 / r315, 0.0, r315 == 0.0);
    r342 = ((r82).x & r312);
        r343 = i64_to_f(vec2<u32>(r342, 0u));
    r344 = select(r343 / r315, 0.0, r315 == 0.0);
    r345 = i64_to_f(r24);
  }
  let r346: f32 = select(r253, r154, r86 != 0.0);
  let r347: f32 = select(r254, r155, r86 != 0.0);
  let r348: f32 = select(r255, r156, r86 != 0.0);
  let r349: f32 = select(r256, r157, r86 != 0.0);
  let r350: f32 = select(r261, r162, r86 != 0.0);
  let r351: f32 = select(r262, r163, r86 != 0.0);
  let r352: f32 = select(r316, r170, r86 != 0.0);
  let r353: f32 = select(r321, r175, r86 != 0.0);
  let r354: f32 = select(r326, r180, r86 != 0.0);
  let r355: f32 = select(r329, r183, r86 != 0.0);
  let r356: f32 = select(r333, r103, r86 != 0.0);
  let r357: f32 = select(r337, r104, r86 != 0.0);
  let r358: f32 = select(r341, r105, r86 != 0.0);
  let r359: f32 = select(r344, r105, r86 != 0.0);
  let r360: f32 = select(r222, r184, r86 != 0.0);
  let r361: f32 = select(r224, r108, r86 != 0.0);
  let r362: f32 = select(r206, r105, r86 != 0.0);
  let r363: f32 = select(r345, r185, r86 != 0.0);
  o.position.x = r346;
  o.position.y = r347;
  o.position.z = r348;
  o.position.w = r349;
  o.local.x = r350;
  o.local.y = r351;
  o.color.x = r352;
  o.color.y = r353;
  o.color.z = r354;
  o.color.w = r355;
  o.color2.x = r356;
  o.color2.y = r357;
  o.color2.z = r358;
  o.color2.w = r359;
  o.shape.x = r360;
  o.shape.y = r361;
  o.shape.z = r362;
  o.shape.w = r363;
  return o;
}
@fragment
fn marks_fragment(v: marks_vertex_out) -> @location(0) vec4<f32> {
  let fw2_0 = fwidth(v.local.x);
  let fw2_1 = fwidth(v.local.y);
  var sb: u32 = 0u;
  var colour: vec4<f32>;
  let r14: f32 = bitcast<f32>(0x40a00000u);
  var r16: f32 = 0.0;
  var r17: f32 = 0.0;
  var r18: f32 = 0.0;
  var r19: f32 = 0.0;
  var r20: f32 = 0.0;
  var r21: f32 = 0.0;
  let r22: f32 = bitcast<f32>(0x0u);
  var r23: f32 = 0.0;
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
  var r34: f32 = 0.0;
  var r35: f32 = 0.0;
  var r36: f32 = 0.0;
  var r37: f32 = 0.0;
  var r38: f32 = 0.0;
  var r39: f32 = 0.0;
  var r40: f32 = 0.0;
  var r41: f32 = 0.0;
  var r42: f32 = 0.0;
  var r43: f32 = 0.0;
  var r44: f32 = 0.0;
  let r45: f32 = bitcast<f32>(0x38d1b717u);
  var r46: f32 = 0.0;
  let r47: f32 = bitcast<f32>(0x3f000000u);
  var r48: f32 = 0.0;
  var r49: f32 = 0.0;
  var r50: f32 = 0.0;
  let r51: f32 = bitcast<f32>(0x3f800000u);
  var r52: f32 = 0.0;
  var r53: f32 = 0.0;
  let r54: f32 = bitcast<f32>(0x40800000u);
  var r55: f32 = 0.0;
  var r56: f32 = 0.0;
  var r57: f32 = 0.0;
  var r58: f32 = 0.0;
  var r59: f32 = 0.0;
  var r60: f32 = 0.0;
  let r61: f32 = bitcast<f32>(0x0u);
  var r62: f32 = 0.0;
  var r63: f32 = 0.0;
  var r64: f32 = 0.0;
  var r65: f32 = 0.0;
  var r66: f32 = 0.0;
  var r67: f32 = 0.0;
  var r68: f32 = 0.0;
  var r69: f32 = 0.0;
  var r70: f32 = 0.0;
  var r71: f32 = 0.0;
  var r72: f32 = 0.0;
  var r73: f32 = 0.0;
  var r74: f32 = 0.0;
  var r75: f32 = 0.0;
  var r76: f32 = 0.0;
  var r77: f32 = 0.0;
  var r78: f32 = 0.0;
  var r79: f32 = 0.0;
  var r80: f32 = 0.0;
  var r81: f32 = 0.0;
  var r82: f32 = 0.0;
  var r83: f32 = 0.0;
  var r84: f32 = 0.0;
  let r85: f32 = bitcast<f32>(0x3f3504f3u);
  var r86: f32 = 0.0;
  let r87: f32 = bitcast<f32>(0x38d1b717u);
  var r88: f32 = 0.0;
  let r89: f32 = bitcast<f32>(0x3f000000u);
  var r90: f32 = 0.0;
  var r91: f32 = 0.0;
  let r92: f32 = bitcast<f32>(0x0u);
  var r93: f32 = 0.0;
  let r94: f32 = bitcast<f32>(0x3f800000u);
  var r95: f32 = 0.0;
  var r96: f32 = 0.0;
  var r97: f32 = 0.0;
  let r98: f32 = bitcast<f32>(0x3f000000u);
  var r99: f32 = 0.0;
  var r100: f32 = 0.0;
  var r101: f32 = 0.0;
  var r102: f32 = 0.0;
  var r103: f32 = 0.0;
  let r104: f32 = bitcast<f32>(0x0u);
  var r105: f32 = 0.0;
  let r106: f32 = bitcast<f32>(0x3f800000u);
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
  var r125: f32 = 0.0;
  var r126: f32 = 0.0;
  var r127: f32 = 0.0;
  let r128: f32 = bitcast<f32>(0x0u);
  var r129: f32 = 0.0;
  var r130: f32 = 0.0;
  var r131: f32 = 0.0;
  var r132: f32 = 0.0;
  var r133: f32 = 0.0;
  var r134: f32 = 0.0;
  var r135: f32 = 0.0;
  var r136: f32 = 0.0;
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
  let r10: f32 = v.shape.x;
  let r11: f32 = v.shape.y;
  let r12: f32 = v.shape.z;
  let r13: f32 = v.shape.w;
  let r15: f32 = select(0.0, 1.0, (r13 == r14));
  if (r15 != 0.0) {
    r16 = (r8 * r4);
    r17 = (r9 * r5);
    r18 = (r16 + r17);
    r19 = (r9 * r4);
    r20 = (r8 * r5);
    r21 = (r19 - r20);
    r23 = abs(r18);
    r24 = (r23 - r10);
    r25 = (r24 + r22);
    r26 = abs(r21);
    r27 = (r26 - r11);
    r28 = (r27 + r22);
    r29 = max(r25, r22);
    r30 = max(r28, r22);
    r31 = (r29 * r29);
    r32 = (r30 * r30);
    r33 = (r31 + r32);
    r34 = sqrt(abs(r33));
    r35 = max(r25, r28);
    r36 = min(r35, r22);
    r37 = (r34 + r36);
    r38 = (r37 - r22);
    r39 = fw2_0;
    r40 = fw2_1;
    r41 = (r39 * r39);
    r42 = (r40 * r40);
    r43 = (r41 + r42);
    r44 = sqrt(abs(r43));
    r46 = max(r44, r45);
    r48 = select(r38 / r46, 0.0, r46 == 0.0);
    r49 = (r47 - r48);
    r50 = max(r49, r22);
    r52 = min(r50, r51);
    r53 = (r3 * r52);
  }
  if (r15 == 0.0) {
    r55 = select(0.0, 1.0, (r13 == r54));
    if (r55 != 0.0) {
      r56 = (r8 * r8);
      r57 = (r9 * r9);
      r58 = (r56 + r57);
      r59 = sqrt(abs(r58));
      r60 = (r59 - r10);
    }
    if (r55 == 0.0) {
      r62 = abs(r8);
      r63 = (r62 - r10);
      r64 = (r63 + r61);
      r65 = abs(r9);
      r66 = (r65 - r11);
      r67 = (r66 + r61);
      r68 = max(r64, r61);
      r69 = max(r67, r61);
      r70 = (r68 * r68);
      r71 = (r69 * r69);
      r72 = (r70 + r71);
      r73 = sqrt(abs(r72));
      r74 = max(r64, r67);
      r75 = min(r74, r61);
      r76 = (r73 + r75);
      r77 = (r76 - r61);
    }
    r78 = select(r77, r60, r55 != 0.0);
    r79 = fw2_0;
    r80 = fw2_1;
    r81 = (r79 * r79);
    r82 = (r80 * r80);
    r83 = (r81 + r82);
    r84 = sqrt(abs(r83));
    r86 = (r84 * r85);
    r88 = max(r86, r87);
    r90 = select(r78 / r88, 0.0, r88 == 0.0);
    r91 = (r89 - r90);
    r93 = max(r91, r92);
    r95 = min(r93, r94);
    r96 = (r3 * r95);
    r97 = select(0.0, 1.0, (r12 <= r92));
    if (r97 == 0.0) {
      r99 = abs(r78);
      r100 = (r12 * r98);
      r101 = (r99 - r100);
      r102 = select(r101 / r88, 0.0, r88 == 0.0);
      r103 = (r98 - r102);
      r105 = max(r103, r104);
      r107 = min(r105, r106);
      r108 = (r7 * r107);
      r109 = (r106 - r108);
      r110 = (r96 * r109);
      r111 = (r108 + r110);
      r112 = (r4 * r108);
      r113 = (r5 * r108);
      r114 = (r6 * r108);
      r115 = (r0 * r96);
      r116 = (r1 * r96);
      r117 = (r2 * r96);
      r118 = (r115 * r109);
      r119 = (r116 * r109);
      r120 = (r117 * r109);
      r121 = (r112 + r118);
      r122 = (r113 + r119);
      r123 = (r114 + r120);
      r124 = select(r121 / r111, 0.0, r111 == 0.0);
      r125 = select(r122 / r111, 0.0, r111 == 0.0);
      r126 = select(r123 / r111, 0.0, r111 == 0.0);
      r127 = select(0.0, 1.0, (r111 <= r104));
      r129 = select(r124, r128, r127 != 0.0);
      r130 = select(r125, r128, r127 != 0.0);
      r131 = select(r126, r128, r127 != 0.0);
      r132 = select(r111, r128, r127 != 0.0);
    }
    r133 = select(r129, r0, r97 != 0.0);
    r134 = select(r130, r1, r97 != 0.0);
    r135 = select(r131, r2, r97 != 0.0);
    r136 = select(r132, r96, r97 != 0.0);
  }
  let r137: f32 = select(r133, r0, r15 != 0.0);
  let r138: f32 = select(r134, r1, r15 != 0.0);
  let r139: f32 = select(r135, r2, r15 != 0.0);
  let r140: f32 = select(r136, r53, r15 != 0.0);
  colour.x = r137;
  colour.y = r138;
  colour.z = r139;
  colour.w = r140;
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
  let r8: u32 = 3u;
  let r9: u32 = 2u;
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
  let r10: u32 = select(r9, r8, r7 != 0.0);
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
  let r23: f32 = buffer6[r9 * 1u + 0u];
  let r24: f32 = (r22 + r23);
  let r25: f32 = buffer6[r8 * 1u + 0u];
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
