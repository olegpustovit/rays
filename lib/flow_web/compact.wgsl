// Generated from compact.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
@group(0) @binding(0) var<storage, read> buffer0: array<f32>;
@group(0) @binding(1) var<storage, read_write> buffer1: array<f32>;
@group(0) @binding(2) var<storage, read_write> buffer2: array<u32>;
@group(0) @binding(3) var<storage, read> buffer3: array<u32>;
@group(0) @binding(4) var<storage, read> buffer4: array<u32>;
var<workgroup> scan56: array<u32, 256>;
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
fn compact_mask(@builtin(local_invocation_index) lid: u32) {
  var sb: u32 = 0u;
  let r6: u32 = 256u;
  let r7: u32 = 1023u;
  let r9: u32 = 1u;
  let r11: u32 = 1073741823u;
  let r13: u32 = 0u;
  let r15: u32 = 0u;
  var r16: vec2<u32> = vec2<u32>(0u, 0u);
  let r17: u32 = 1073741823u;
  var r18: u32 = 0u;
  let r19: u32 = 0u;
  var r20: vec2<u32> = vec2<u32>(0u, 0u);
  let r21: u32 = 1073741823u;
  var r22: u32 = 0u;
  var r23: vec2<u32> = vec2<u32>(0u, 0u);
  var r24: u32 = 0u;
  var r25: f32 = 0.0;
  var r26: u32 = 0u;
  let r28: u32 = 255u;
  let r38: u32 = 0u;
  var r39: u32 = 0u;
  var r40: vec2<u32> = vec2<u32>(0u, 0u);
  var r41: u32 = 0u;
  var r55: u32 = 0u;
  var scan56_before: u32 = 0u;
  var scan56_total: u32 = 0u;
  var r59: u32 = 0u;
  var r60: vec2<u32> = vec2<u32>(0u, 0u);
  var r61: u32 = 0u;
  var r76: u32 = 0u;
  let r79: u32 = 0u;
  var r80: u32 = 0u;
  let r0: vec2<u32> = vec2<u32>(lid, 0u);
  let r8: u32 = ((r0).x & r7);
    let r10: vec2<u32> = vec2<u32>(buffer3[r9 * 1u + 0u], 0u);
  let r12: u32 = ((r10).x & r11);
    let r14: f32 = select(0.0, 1.0, all(vec2<u32>(r12, 0u) == vec2<u32>(r13, 0u)));
  if (r14 != 0.0) {
    r16 = vec2<u32>(buffer3[r15 * 1u + 0u], 0u);
    r18 = ((r16).x & r17);
      }
  if (r14 == 0.0) {
    r20 = vec2<u32>(buffer3[r19 * 1u + 0u], 0u);
    r22 = ((r20).x & r21);
        r23 = vec2<u32>(buffer4[r19 * 1u + 0u], 0u);
    r24 = ((r23).x & r21);
        r25 = select(0.0, 1.0, i64_lt(vec2<u32>(r22, 0u), vec2<u32>(r24, 0u)));
    r26 = select(r24, r22, r25 != 0.0);
  }
  let r27: u32 = select(r26, r18, r14 != 0.0);
  let r29: u32 = (r27 + r28);
    let r30: u32 = select(r29 / r6, 0u, r6 == 0u);
  if (all(vec2<u32>(r6, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r31: u32 = (r8 * r30);
    let r32: f32 = select(0.0, 1.0, i64_lt(vec2<u32>(r27, 0u), vec2<u32>(r31, 0u)));
  let r33: u32 = select(r31, r27, r32 != 0.0);
  let r34: vec2<u32> = i63(i64_add(vec2<u32>(r33, 0u), vec2<u32>(r30, 0u)));
    let r35: f32 = select(0.0, 1.0, i64_lt(vec2<u32>(r27, 0u), r34));
  let r36: vec2<u32> = select(r34, vec2<u32>(r27, 0u), r35 != 0.0);
  let r37: f32 = select(0.0, 1.0, i64_lt(vec2<u32>(r33, 0u), r36));
  if (r37 != 0.0) {
    r40 = vec2<u32>(r33, 0u);
    r41 = r38;
    for (r39 = r38; r39 < r30; r39 = r39 + 1u) {
  let r42: u32 = 1u;
  var r43: vec2<u32> = vec2<u32>(0u, 0u);
  var r44: f32 = 0.0;
  let r45: f32 = bitcast<f32>(0x0u);
  var r46: f32 = 0.0;
  let r47: u32 = 1u;
  var r48: u32 = 0u;
  var r49: u32 = 0u;
  var r50: f32 = 0.0;
  var r51: f32 = 0.0;
  var r52: f32 = 0.0;
  if (r37 != 0.0) {
    r43 = i63(i64_add(r40, vec2<u32>(r42, 0u)));
        r44 = buffer0[(r40).x * 1u + 0u];
    r46 = select(0.0, 1.0, (r44 == r45));
    if (r46 == 0.0) {
      r48 = (r41 + r47);
          }
    r49 = select(r48, r41, r46 != 0.0);
    r50 = select(0.0, 1.0, !i64_lt(r43, r36));
    r51 = select(0.0, 1.0, (r50 == 0.0));
    r52 = select(0.0, 1.0, (r51 == 0.0));
  }
  let stop39 = r52 != 0.0;
  let next39_0: vec2<u32> = r43;
  let next39_1: u32 = r49;
  r40 = next39_0;
  r41 = next39_1;
  if (stop39) { break; }
    }
    r55 = r41;
  }
  let r56: u32 = select(r13, r55, r37 != 0.0);
  { var v: u32 = r56; scan56[lid] = v; for (var d = 1u; d < 256u; d = d << 1u) { workgroupBarrier(); var w = 0u; if (lid >= d) { w = scan56[lid - d]; } workgroupBarrier(); v = v + w; scan56[lid] = v; } workgroupBarrier(); scan56_before = v - r56; scan56_total = scan56[255u]; }
  let r57: u32 = scan56_total;
  let r58: u32 = scan56_before;
  if (r37 != 0.0) {
    r60 = vec2<u32>(r33, 0u);
    r61 = r58;
    for (r59 = r38; r59 < r30; r59 = r59 + 1u) {
  let r62: u32 = 1u;
  var r63: vec2<u32> = vec2<u32>(0u, 0u);
  var r64: f32 = 0.0;
  let r65: f32 = bitcast<f32>(0x0u);
  var r66: f32 = 0.0;
  var r67: f32 = 0.0;
  var r68: u32 = 0u;
  var r69: u32 = 0u;
  var r70: u32 = 0u;
  var r71: f32 = 0.0;
  var r72: f32 = 0.0;
  var r73: f32 = 0.0;
  if (r37 != 0.0) {
    r63 = i63(i64_add(r60, vec2<u32>(r62, 0u)));
        r64 = buffer0[(r60).x * 1u + 0u];
    r66 = select(0.0, 1.0, (r64 == r65));
    if (r66 == 0.0) {
      r67 = i64_to_f(r60);
      buffer1[r61] = r67;
      r68 = 1u;
      r69 = (r61 + r68);
          }
    r70 = select(r69, r61, r66 != 0.0);
    r71 = select(0.0, 1.0, !i64_lt(r63, r36));
    r72 = select(0.0, 1.0, (r71 == 0.0));
    r73 = select(0.0, 1.0, (r72 == 0.0));
  }
  let stop59 = r73 != 0.0;
  let next59_0: vec2<u32> = r63;
  let next59_1: u32 = r70;
  r60 = next59_0;
  r61 = next59_1;
  if (stop59) { break; }
    }
    r76 = r61;
  }
  let r77: u32 = select(r58, r76, r37 != 0.0);
  let r78: f32 = select(0.0, 1.0, all(vec2<u32>(r8, 0u) == vec2<u32>(r13, 0u)));
  if (r78 != 0.0) {
    buffer2[r79] = r57;
    r80 = 1u;
  }
  let r81: u32 = select(r13, r80, r78 != 0.0);
  let r82: u32 = (r77 + r81);
  }
