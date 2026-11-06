// kernel k0
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn wfade(t: f32) -> f32 { return t * t * t * (t * (t * 6.0 - 15.0) + 10.0); }
fn wblend(a: f32, b: f32, t: f32) -> f32 { return a + t * (b - a); }
fn wgrad(hash: i32, x: f32, y: f32, z: f32) -> f32 {
  let h = hash & 15;
  let u = select(y, x, h < 8);
  let v = select(select(z, x, h == 12 || h == 14), y, h < 4);
  return select(-u, u, (h & 1) == 0) + select(-v, v, (h & 2) == 0);
}
fn wnoise(p: vec3<f32>, o: i32) -> f32 {
  if (!(wfinite(p.x) && wfinite(p.y) && wfinite(p.z))) {
    let nan = 0x7fc00000u;
    return bitcast<f32>(nan);
  }
  let base = floor(p);
  let q = p - base;
  let f = vec3<f32>(wfade(q.x), wfade(q.y), wfade(q.z));
  let x = i32(base.x % 256.0) & 255;
  let y = i32(base.y % 256.0) & 255;
  let z = i32(base.z % 256.0) & 255;
  let a = table[o + x] + y;
  let b = table[o + x + 1] + y;
  let aa = table[o + a] + z;
  let ab = table[o + a + 1] + z;
  let ba = table[o + b] + z;
  let bb = table[o + b + 1] + z;
  let x00 = wblend(wgrad(table[o + aa], q.x, q.y, q.z), wgrad(table[o + ba], q.x - 1.0, q.y, q.z), f.x);
  let x10 = wblend(wgrad(table[o + ab], q.x, q.y - 1.0, q.z), wgrad(table[o + bb], q.x - 1.0, q.y - 1.0, q.z), f.x);
  let x01 = wblend(wgrad(table[o + aa + 1], q.x, q.y, q.z - 1.0), wgrad(table[o + ba + 1], q.x - 1.0, q.y, q.z - 1.0), f.x);
  let x11 = wblend(wgrad(table[o + ab + 1], q.x, q.y - 1.0, q.z - 1.0), wgrad(table[o + bb + 1], q.x - 1.0, q.y - 1.0, q.z - 1.0), f.x);
  return clamp((wblend(wblend(x00, x10, f.y), wblend(x01, x11, f.y), f.z) + 1.0) * 0.5, 0.0, 1.0);
}
fn wfbm(p: vec3<f32>, octaves: i32, o: i32) -> f32 {
  var frequency: f32 = 1.0;
  var amplitude: f32 = 1.0;
  var sum: f32 = 0.0;
  var weights: f32 = 0.0;
  for (var octave: i32 = 0; octave < octaves; octave++) {
    sum += wnoise(p * frequency, o) * amplitude;
    weights += amplitude;
    frequency *= 2.0;
    amplitude *= 0.5;
  }
  return select(sum / weights, 0.0, weights == 0.0);
}
@compute @workgroup_size(64)
fn kernel_47b91c697c17da6d1caccc5fba8e45bd(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = bitcast<f32>(0x447a0000u);
  let r2: f32 = select(((r0 % r1) + r1) % r1, 0.0, r1 == 0.0);
  if (!wfinite(r2)) { sb |= 1u; }
  let r3: f32 = (r0 - r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = select(r3 / r1, 0.0, r1 == 0.0);
  if (!wfinite(r4)) { sb |= 1u; }
  let r5: f32 = bitcast<f32>(0x3cf5c28fu);
  let r6: f32 = (r2 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = (r4 * r5);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = wfbm(vec3<f32>(r6, r7, r8), 1, 0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = bitcast<f32>(0x3f4ccccdu);
  let r11: f32 = (r2 * r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = bitcast<f32>(0x3f19999au);
  let r13: f32 = (r4 * r12);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x40000000u);
  let r15: f32 = (r9 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r13 + r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = bitcast<f32>(0x0u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r11;
  outbuf[i * 3u + 1u] = r16;
  outbuf[i * 3u + 2u] = r17;
}
