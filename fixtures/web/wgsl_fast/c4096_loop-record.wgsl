kernel_e0ef77a6935b97bb34df146422f86df5
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> count_in: array<u32>;
@group(0) @binding(5) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
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
@compute @workgroup_size(64)
fn kernel_e0ef77a6935b97bb34df146422f86df5(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].x != 0u) { n = min(n, count_in[0] * uniforms[0].z / uniforms[0].w); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x0u);
  let r4: u32 = 1u;
  let r5: u32 = 2u;
  let r6: u32 = 0u;
  let r8: u32 = 5u;
  var r10: vec2<u32> = vec2<u32>(0u, 0u);
  var r11: u32 = 0u;
  var r12: u32 = 0u;
  var r13: f32 = 0.0;
  var r14: f32 = 0.0;
  var r15: f32 = 0.0;
  let r29: f32 = bitcast<f32>(0x3f800000u);
  let r30: f32 = bitcast<f32>(0x0u);
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = bitcast<f32>(uniforms[0].y);
  let r7: vec2<u32> = i64_from_f(r0);
  let r9: vec2<u32> = i63(i64_divmod(r7, vec2<u32>(r8, 0u), true));
  if (all(vec2<u32>(r8, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let over10 = i64_lt(vec2<u32>(r6, 0u), r9) && i64_lt(vec2<u32>(4096u, 0u), i64_sub(r9, vec2<u32>(r6, 0u)));
  if (over10) { sb |= 1u; }
  r11 = r4;
  r12 = r5;
  r13 = r1;
  r14 = r2;
  r15 = r3;
  for (r10 = vec2<u32>(r6, 0u); i64_lt(r10, select(r9, vec2<u32>(r6, 0u), over10)); r10 = i64_add(r10, vec2<u32>(1u, 0u))) {
  let r16: f32 = bitcast<f32>(0x3f800000u);
  let r17: f32 = (r13 + r16);
  let r18: f32 = select(0.0, 1.0, (r15 == 0.0));
  let r19: f32 = select(0.0, 1.0, (r18 == 0.0));
  let r20: f32 = select(0.0, 1.0, (r19 == 0.0));
  let next10_0: u32 = r12;
  let next10_1: u32 = r11;
  let next10_2: f32 = r14;
  let next10_3: f32 = r17;
  let next10_4: f32 = r20;
  r11 = next10_0;
  r12 = next10_1;
  r13 = next10_2;
  r14 = next10_3;
  r15 = next10_4;
  }
  let r22: u32 = r11;
  let r23: u32 = r12;
  let r24: f32 = r13;
  let r26: f32 = r15;
  let r27: f32 = i64_to_f(vec2<u32>(r22, 0u));
  let r28: f32 = i64_to_f(vec2<u32>(r23, 0u));
  let r31: f32 = select(r30, r29, r26 != 0.0);
  if (!wfinite(r27)) { sb |= 1u; }
  if (!wfinite(r28)) { sb |= 1u; }
  if (!wfinite(r24)) { sb |= 1u; }
  if (!wfinite(r31)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r27;
  outbuf[i * 4u + 1u] = r28;
  outbuf[i * 4u + 2u] = r24;
  outbuf[i * 4u + 3u] = r31;
}
