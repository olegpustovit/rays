@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
@compute @workgroup_size(64)
fn kernel_b783fe2488765f19579d3e3ea3a2190e(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { atomicOr(&status, 1u); }
  let r1: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r1)) { atomicOr(&status, 1u); }
  let r2: f32 = select(0.0, 1.0, (r0 < r1));
  var r3: f32 = 0.0;
  if (r2 != 0.0) {
    r3 = bitcast<f32>(0x3f800000u);
  }
  var r4: f32 = 0.0;
  if (r2 != 0.0) {
    r4 = (r0 + r3);
    if (!wfinite(r4)) { atomicOr(&status, 1u); }
  }
  var r5: f32 = 0.0;
  if (r2 == 0.0) {
    r5 = bitcast<f32>(0x3f800000u);
  }
  var r6: f32 = 0.0;
  if (r2 == 0.0) {
    r6 = (r0 - r5);
    if (!wfinite(r6)) { atomicOr(&status, 1u); }
  }
  let r7: f32 = select(r6, r4, r2 != 0.0);
  outbuf[i * 1u + 0u] = r7;
}
