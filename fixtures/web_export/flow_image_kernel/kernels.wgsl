// kernel image0
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
@compute @workgroup_size(64)
fn kernel_5a006ca0a81c6925792c735495c0885c(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].z));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].w));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r2)) { sb |= 1u; }
  let r3: f32 = (r0 + r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3f000000u);
  let r5: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r3;
  outbuf[i * 4u + 1u] = r1;
  outbuf[i * 4u + 2u] = r4;
  outbuf[i * 4u + 3u] = r5;
}
