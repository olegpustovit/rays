kernel_3cb1ec792edd3e04cce554de3c4e566e
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
fn kernel_3cb1ec792edd3e04cce554de3c4e566e(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].y != 0u) { n = min(n, count_in[0] * uniforms[0].w / uniforms[1].x); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r4: f32 = bitcast<f32>(0x0u);
  var r6: vec2<u32> = vec2<u32>(0u, 0u);
  var r7: vec2<u32> = vec2<u32>(0u, 0u);
  var r8: vec2<u32> = vec2<u32>(0u, 0u);
  var r9: f32 = 0.0;
  var r10: f32 = 0.0;
  let r0: vec2<u32> = i64_from_f(input0[i]);
  let r1: f32 = i64_to_f(r0);
  let r2: f32 = bitcast<f32>(uniforms[0].y);
  let r3: f32 = (r1 + r2);
  let r5: f32 = select(0.0, 1.0, (r3 > r4));
  if (r5 != 0.0) {
    r6 = i64_from_f(bitcast<f32>(uniforms[0].z));
    r7 = i63(i64_sub(r6, r6));
        r8 = i63(i64_divmod(r0, r7, true));
    if (all(r7 == vec2<u32>(0u))) { sb |= 2u; }
    r9 = i64_to_f(r8);
  }
  if (r5 == 0.0) {
    r10 = i64_to_f(r0);
  }
  let r11: f32 = select(r10, r9, r5 != 0.0);
  if (!wfinite(r11)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 1u + 0u] = r11;
}
