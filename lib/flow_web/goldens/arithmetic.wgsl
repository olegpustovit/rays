@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
@compute @workgroup_size(64)
fn kernel_76303ccef5d6f1fd85f1c4eff0c56c27(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { atomicOr(&status, 1u); }
  let r1: f32 = bitcast<f32>(0x40000000u);
  let r2: f32 = (r0 * r1);
  if (!wfinite(r2)) { atomicOr(&status, 1u); }
  let r3: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r3)) { atomicOr(&status, 1u); }
  let r4: f32 = (r2 + r3);
  if (!wfinite(r4)) { atomicOr(&status, 1u); }
  outbuf[i * 1u + 0u] = r4;
}
