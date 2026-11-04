kernel_8f37c7e4f84731ec579e0479b9c5293b
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
@compute @workgroup_size(64)
fn kernel_8f37c7e4f84731ec579e0479b9c5293b(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x40400000u);
  let r3: f32 = select(r0 / r2, 0.0, r2 == 0.0);
  if (!wfinite(r3)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r0;
  outbuf[i * 3u + 1u] = r1;
  outbuf[i * 3u + 2u] = r3;
}
