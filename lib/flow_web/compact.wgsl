// Generated from compact.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
@group(0) @binding(0) var<storage, read> buffer0: array<f32>;
@group(0) @binding(1) var<storage, read_write> buffer1: array<f32>;
@group(0) @binding(2) var<storage, read_write> buffer2: array<u32>;
@group(0) @binding(3) var<storage, read> buffer3: array<u32>;
@group(0) @binding(4) var<storage, read> buffer4: array<u32>;
@group(0) @binding(5) var<storage, read_write> buffer5: array<u32>;
var<workgroup> scan66: array<u32, 256>;
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
fn i63(a: vec2<u32>) -> vec2<u32> { return vec2<u32>(a.x, bitcast<u32>(bitcast<i32>(a.y << 1u) >> 1u)); }
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
    if (!i64_lt(r, ub) || bitcast<i32>(r.y) < 0) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
@compute @workgroup_size(256)
fn compact_mask(@builtin(local_invocation_index) lid: u32, @builtin(global_invocation_id) gid: vec3<u32>) {
  var sb: u32 = 0u;
  let r7: u32 = 256u;
  let r11: u32 = 2u;
  let r13: u32 = 1073741823u;
  let r15: u32 = 1u;
  let r18: u32 = 0u;
  let r20: u32 = 0u;
  var r21: vec2<u32> = vec2<u32>(0u, 0u);
  let r22: u32 = 1073741823u;
  var r23: u32 = 0u;
  let r24: u32 = 0u;
  var r25: vec2<u32> = vec2<u32>(0u, 0u);
  let r26: u32 = 1073741823u;
  var r27: u32 = 0u;
  var r28: vec2<u32> = vec2<u32>(0u, 0u);
  var r29: u32 = 0u;
  var r30: f32 = 0.0;
  var r31: u32 = 0u;
  let r35: u32 = 255u;
  var r49: f32 = 0.0;
  let r50: f32 = bitcast<f32>(0x0u);
  var r51: f32 = 0.0;
  let r52: u32 = 1u;
  let r53: u32 = 0u;
  var r54: u32 = 0u;
  let r57: u32 = 0u;
  var r58: vec2<u32> = vec2<u32>(0u, 0u);
  var r59: u32 = 0u;
  var r65: u32 = 0u;
  var scan66_before: u32 = 0u;
  var scan66_total: u32 = 0u;
  var r71: u32 = 0u;
  let r72: u32 = 0u;
  var r73: u32 = 0u;
  let r74: u32 = 1u;
  var r75: f32 = 0.0;
  var r76: vec2<u32> = vec2<u32>(0u, 0u);
  var r77: u32 = 0u;
  var r86: u32 = 0u;
  let r87: u32 = 0u;
  var r88: u32 = 0u;
  let r89: u32 = 0u;
  var r90: u32 = 0u;
  var r91: u32 = 0u;
  let r92: u32 = 1u;
  var r93: f32 = 0.0;
  var r94: u32 = 0u;
  var r95: vec2<u32> = vec2<u32>(0u, 0u);
  let r96: u32 = 1073741823u;
  var r97: u32 = 0u;
  var r98: u32 = 0u;
  var r99: f32 = 0.0;
  var r100: u32 = 0u;
  let r101: u32 = 0u;
  var r102: u32 = 0u;
  var r103: u32 = 0u;
  let r0: vec2<u32> = vec2<u32>(gid.x, 0u);
  let r8: vec2<u32> = i63(i64_divmod(r0, vec2<u32>(r7, 0u), false));
  if (all(vec2<u32>(r7, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r9: vec2<u32> = i63(i64_mul(r8, vec2<u32>(r7, 0u)));
    let r10: vec2<u32> = i63(i64_sub(r0, r9));
    let r12: vec2<u32> = vec2<u32>(buffer3[r11 * 1u + 0u], 0u);
  let r14: u32 = ((r12).x & r13);
    let r16: vec2<u32> = vec2<u32>(buffer3[r15 * 1u + 0u], 0u);
  let r17: u32 = ((r16).x & r13);
    let r19: f32 = select(0.0, 1.0, all(vec2<u32>(r17, 0u) == vec2<u32>(r18, 0u)));
  if (r19 != 0.0) {
    r21 = vec2<u32>(buffer3[r20 * 1u + 0u], 0u);
    r23 = ((r21).x & r22);
      }
  if (r19 == 0.0) {
    r25 = vec2<u32>(buffer3[r24 * 1u + 0u], 0u);
    r27 = ((r25).x & r26);
        r28 = vec2<u32>(buffer4[r24 * 1u + 0u], 0u);
    r29 = ((r28).x & r26);
        r30 = select(0.0, 1.0, i64_lt(vec2<u32>(r27, 0u), vec2<u32>(r29, 0u)));
    r31 = select(r29, r27, r30 != 0.0);
  }
  let r32: u32 = select(r31, r23, r19 != 0.0);
  let r33: vec2<u32> = vec2<u32>(buffer3[r18 * 1u + 0u], 0u);
  let r34: u32 = ((r33).x & r13);
    let r36: u32 = (r34 + r35);
    let r37: u32 = select(r36 / r7, 0u, r7 == 0u);
  if (all(vec2<u32>(r7, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r38: u32 = (r32 + r35);
    let r39: u32 = select(r38 / r7, 0u, r7 == 0u);
  if (all(vec2<u32>(r7, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r40: u32 = (r39 + r35);
    let r41: u32 = select(r40 / r7, 0u, r7 == 0u);
  if (all(vec2<u32>(r7, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r42: vec2<u32> = i63(i64_mul(r10, vec2<u32>(r41, 0u)));
    let r43: f32 = select(0.0, 1.0, i64_lt(vec2<u32>(r39, 0u), r42));
  let r44: vec2<u32> = select(r42, vec2<u32>(r39, 0u), r43 != 0.0);
  let r45: vec2<u32> = i63(i64_add(r44, vec2<u32>(r41, 0u)));
    let r46: f32 = select(0.0, 1.0, i64_lt(vec2<u32>(r39, 0u), r45));
  let r47: vec2<u32> = select(r45, vec2<u32>(r39, 0u), r46 != 0.0);
  let r48: f32 = select(0.0, 1.0, i64_lt(r0, vec2<u32>(r32, 0u)));
  if (r48 != 0.0) {
    r49 = buffer0[(r0).x * 1u + 0u];
    r51 = select(0.0, 1.0, (r49 == r50));
    r54 = select(r52, r53, r51 != 0.0);
  }
  let r55: u32 = select(r18, r54, r48 != 0.0);
  let r56: f32 = select(0.0, 1.0, all(vec2<u32>(r14, 0u) == vec2<u32>(r15, 0u)));
  if (r56 != 0.0) {
    let over58 = i64_lt(r44, r47) && i64_lt(vec2<u32>(4096u, 0u), i64_sub(r47, r44));
    if (over58) { sb |= 1u; }
    r59 = r57;
    for (r58 = r44; i64_lt(r58, select(r47, r44, over58)); r58 = i64_add(r58, vec2<u32>(1u, 0u))) {
  var r60: vec2<u32> = vec2<u32>(0u, 0u);
  let r61: u32 = 1073741823u;
  var r62: u32 = 0u;
  var r63: u32 = 0u;
  if (r56 != 0.0) {
    r60 = vec2<u32>(buffer5[(r58).x * 1u + 0u], 0u);
    r62 = ((r60).x & r61);
        r63 = (r59 + r62);
      }
  let next58_0: u32 = r63;
  r59 = next58_0;
    }
    r65 = r59;
  }
  let r66: u32 = select(r55, r65, r56 != 0.0);
  { var v: u32 = r66; scan66[lid] = v; for (var d = 1u; d < 256u; d = d << 1u) { workgroupBarrier(); var w = 0u; if (lid >= d) { w = scan66[lid - d]; } workgroupBarrier(); v = v + w; scan66[lid] = v; } workgroupBarrier(); scan66_before = v - r66; scan66_total = scan66[255u]; }
  let r67: u32 = scan66_total;
  let r68: u32 = scan66_before;
  let r69: f32 = select(0.0, 1.0, all(r10 == vec2<u32>(r18, 0u)));
  let r70: f32 = select(0.0, 1.0, all(vec2<u32>(r14, 0u) == vec2<u32>(r18, 0u)));
  if (r70 != 0.0) {
    if (r69 != 0.0) {
      buffer5[(r8).x] = r67;
      r71 = 1u;
    }
    r73 = select(r72, r71, r69 != 0.0);
  }
  if (r70 == 0.0) {
    r75 = select(0.0, 1.0, all(vec2<u32>(r14, 0u) == vec2<u32>(r74, 0u)));
    if (r75 != 0.0) {
      let over76 = i64_lt(r44, r47) && i64_lt(vec2<u32>(4096u, 0u), i64_sub(r47, r44));
      if (over76) { sb |= 1u; }
      r77 = r68;
      for (r76 = r44; i64_lt(r76, select(r47, r44, over76)); r76 = i64_add(r76, vec2<u32>(1u, 0u))) {
  var r78: vec2<u32> = vec2<u32>(0u, 0u);
  let r79: u32 = 1073741823u;
  var r80: u32 = 0u;
  var r81: u32 = 0u;
  var r82: u32 = 0u;
  var r83: u32 = 0u;
  var r84: u32 = 0u;
  if (r70 == 0.0) {
    if (r75 != 0.0) {
      r78 = vec2<u32>(buffer5[(r76).x * 1u + 0u], 0u);
      r80 = ((r78).x & r79);
            r81 = (r37 + (r76).x);
            buffer5[r81] = r77;
      r82 = 1u;
      r83 = (r80 * r82);
            r84 = (r77 + r83);
          }
  }
  let next76_0: u32 = r84;
  r77 = next76_0;
      }
      r86 = r77;
      if (r69 != 0.0) {
        buffer2[r87] = r67;
        r88 = 1u;
      }
      r90 = select(r89, r88, r69 != 0.0);
      r91 = (r86 + r90);
          }
    if (r75 == 0.0) {
      r93 = select(0.0, 1.0, all(vec2<u32>(r55, 0u) == vec2<u32>(r92, 0u)));
      if (r93 != 0.0) {
        r94 = (r37 + (r8).x);
                r95 = vec2<u32>(buffer5[r94 * 1u + 0u], 0u);
        r97 = ((r95).x & r96);
                r98 = (r97 + r68);
                r99 = i64_to_f(r0);
        buffer1[r98] = r99;
        r100 = 1u;
      }
      r102 = select(r101, r100, r93 != 0.0);
    }
    r103 = select(r102, r91, r75 != 0.0);
  }
  let r104: u32 = select(r103, r73, r70 != 0.0);
}
