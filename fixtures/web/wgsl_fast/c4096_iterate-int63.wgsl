kernel_31cf57fb724f65108d9ef596062d4348
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> count_in: array<u32>;
@group(0) @binding(5) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
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
@compute @workgroup_size(64)
fn kernel_31cf57fb724f65108d9ef596062d4348(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].y != 0u) { n = min(n, count_in[0] * uniforms[0].w / uniforms[1].x); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r2: f32 = bitcast<f32>(0x0u);
  let r3: u32 = 1u;
  let r4: u32 = 0u;
  var r5: u32 = 0u;
  var r6: f32 = 0.0;
  var r7: f32 = 0.0;
  var r8: f32 = 0.0;
  var r9: f32 = 0.0;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = bitcast<f32>(uniforms[0].y);
  r6 = r1;
  r7 = r2;
  r8 = r2;
  r9 = r2;
  for (r5 = r4; r5 < r3; r5 = r5 + 1u) {
  let r13: u32 = 4u;
  let r15: u32 = 1u;
  let r17: u32 = 2u;
  let r20: u32 = 63u;
  let r23: vec2<u32> = vec2<u32>(4294967295u, 2147483647u);
  let r25: u32 = 0u;
  let r27: f32 = bitcast<f32>(0x3f800000u);
  let r28: f32 = bitcast<f32>(0x0u);
  let r33: f32 = bitcast<f32>(0x3f800000u);
  let r34: f32 = bitcast<f32>(0x0u);
  let r39: f32 = bitcast<f32>(0x3f800000u);
  let r40: f32 = bitcast<f32>(0x0u);
  let r44: vec2<u32> = vec2<u32>(4294967295u, 4294967295u);
  let r48: f32 = bitcast<f32>(0x3f800000u);
  let r49: f32 = bitcast<f32>(0x0u);
  let r10: vec2<u32> = i64_from_f(r0);
  let r11: vec2<u32> = i64_from_f(bitcast<f32>(uniforms[0].z));
  let r12: vec2<u32> = i64_add(r10, r11);
    let r14: vec2<u32> = i64_divmod(r12, vec2<u32>(r13, 0u), true);
  if (all(vec2<u32>(r13, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r16: vec2<u32> = i64_add(r14, vec2<u32>(r15, 0u));
    let r18: vec2<u32> = i64_mul(r16, vec2<u32>(r17, 0u));
    let r19: vec2<u32> = i64_divmod(r12, vec2<u32>(r15, 0u), true);
  if (all(vec2<u32>(r15, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r21: vec2<u32> = i64_add(vec2<u32>(r20, 0u), r19);
    let r22: vec2<u32> = i64_shl(vec2<u32>(r15, 0u), r21);
  if (r21.y != 0u || r21.x > 63u) { sb |= 4u; }
  let r24: vec2<u32> = i64_add(r23, r16);
    let r26: f32 = select(0.0, 1.0, i64_lt(r24, vec2<u32>(r25, 0u)));
  let r29: f32 = select(r28, r27, r26 != 0.0);
  let r30: vec2<u32> = i64_mul(r23, r18);
    let r31: vec2<u32> = i64_sub(vec2<u32>(r25, 0u), r18);
    let r32: f32 = select(0.0, 1.0, all(r30 == r31));
  let r35: f32 = select(r34, r33, r32 != 0.0);
  let r36: vec2<u32> = i64_sub(vec2<u32>(r25, 0u), r16);
    let r37: vec2<u32> = i64_shr(r36, vec2<u32>(r20, 0u));
  if (vec2<u32>(r20, 0u).y != 0u || vec2<u32>(r20, 0u).x > 63u) { sb |= 4u; }
  let r38: f32 = select(0.0, 1.0, all(r37 == vec2<u32>(r15, 0u)));
  let r41: f32 = select(r40, r39, r38 != 0.0);
  let r42: vec2<u32> = i64_abs(r22);
  let r43: f32 = select(0.0, 1.0, i64_lt(r42, vec2<u32>(r25, 0u)));
  let r45: vec2<u32> = i64_divmod(r22, r44, false);
  if (all(r44 == vec2<u32>(0u))) { sb |= 2u; }
  let r46: f32 = select(0.0, 1.0, all(r45 == r22));
  let r47: f32 = select(0.0, 1.0, (r43 != 0.0 && r46 != 0.0));
  let r50: f32 = select(r49, r48, r47 != 0.0);
  let next5_0: f32 = r29;
  let next5_1: f32 = r35;
  let next5_2: f32 = r41;
  let next5_3: f32 = r50;
  r6 = next5_0;
  r7 = next5_1;
  r8 = next5_2;
  r9 = next5_3;
  }
  let r52: f32 = r6;
  let r53: f32 = r7;
  let r54: f32 = r8;
  let r55: f32 = r9;
  if (!wfinite(r52)) { sb |= 1u; }
  if (!wfinite(r53)) { sb |= 1u; }
  if (!wfinite(r54)) { sb |= 1u; }
  if (!wfinite(r55)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r52;
  outbuf[i * 4u + 1u] = r53;
  outbuf[i * 4u + 2u] = r54;
  outbuf[i * 4u + 3u] = r55;
}
