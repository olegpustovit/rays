// Generated from dispatch.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
@group(0) @binding(0) var<storage, read> buffer0: array<u32>;
@group(0) @binding(1) var<storage, read> buffer1: array<u32>;
@group(0) @binding(2) var<storage, read_write> buffer2: array<u32>;
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
@compute @workgroup_size(1)
fn dispatch_arguments(@builtin(local_invocation_index) lid: u32, @builtin(global_invocation_id) gid: vec3<u32>) {
  var sb: u32 = 0u;
  let r4: u32 = 1u;
  let r5: u32 = 0u;
  let r7: u32 = 1073741823u;
  let r14: u32 = 2u;
  let r20: u32 = 3u;
  let r27: u32 = 1u;
  var r28: f32 = 0.0;
  var r29: u32 = 0u;
  let r30: u32 = 0u;
  var r31: u32 = 0u;
  var r32: u32 = 0u;
  let r33: u32 = 2u;
  var r34: u32 = 0u;
  var r35: u32 = 0u;
  var r36: u32 = 0u;
  let r0: vec2<u32> = vec2<u32>(gid.x, 0u);
  let r6: vec2<u32> = vec2<u32>(buffer1[r5 * 1u + 0u], 0u);
  let r8: u32 = ((r6).x & r7);
    let r9: vec2<u32> = vec2<u32>(buffer0[r5 * 1u + 0u], 0u);
  let r10: u32 = ((r9).x & r7);
    let r11: vec2<u32> = vec2<u32>(buffer1[r4 * 1u + 0u], 0u);
  let r12: u32 = ((r11).x & r7);
    let r13: vec2<u32> = i63(i64_mul(vec2<u32>(r10, 0u), vec2<u32>(r12, 0u)));
    let r15: vec2<u32> = vec2<u32>(buffer1[r14 * 1u + 0u], 0u);
  let r16: u32 = ((r15).x & r7);
    let r17: vec2<u32> = i63(i64_divmod(r13, vec2<u32>(r16, 0u), false));
  if (all(vec2<u32>(r16, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r18: f32 = select(0.0, 1.0, i64_lt(vec2<u32>(r8, 0u), r17));
  let r19: vec2<u32> = select(r17, vec2<u32>(r8, 0u), r18 != 0.0);
  let r21: vec2<u32> = vec2<u32>(buffer1[r20 * 1u + 0u], 0u);
  let r22: u32 = ((r21).x & r7);
    let r23: vec2<u32> = i63(i64_sub(vec2<u32>(r22, 0u), vec2<u32>(r4, 0u)));
    let r24: vec2<u32> = i63(i64_add(r19, r23));
    let r25: vec2<u32> = i63(i64_divmod(r24, vec2<u32>(r22, 0u), false));
  if (all(vec2<u32>(r22, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r26: f32 = select(0.0, 1.0, all(r0 == vec2<u32>(r5, 0u)));
  if (r26 != 0.0) {
    r28 = select(0.0, 1.0, i64_lt(r25, vec2<u32>(r27, 0u)));
    r29 = select((r25).x, r27, r28 != 0.0);
    buffer2[r30] = r29;
    r31 = 1u;
    buffer2[r27] = r27;
    r32 = 1u;
    buffer2[r33] = r27;
    r34 = 1u;
    r35 = (r32 + r34);
        r36 = (r31 + r35);
      }
  let r37: u32 = select(r5, r36, r26 != 0.0);
}
