kernel_3103ab6a62fc51e0000c6475c1d1481e
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
@compute @workgroup_size(64)
fn kernel_3103ab6a62fc51e0000c6475c1d1481e(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  var r10: f32 = 0.0;
  var r11: f32 = 0.0;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = select(0.0, 1.0, (r0 > r1));
  let r3: f32 = bitcast<f32>(0x41000000u);
  let r4: f32 = select(0.0, 1.0, (r0 < r3));
  let r5: f32 = bitcast<f32>(0x43fa0000u);
  let r6: f32 = select(0.0, 1.0, (r0 >= r5));
  let r7: f32 = select(0.0, 1.0, (r6 == 0.0));
  let r8: f32 = select(0.0, 1.0, (r4 != 0.0 || r7 != 0.0));
  let r9: f32 = select(0.0, 1.0, (r2 != 0.0 && r8 != 0.0));
  if (r9 == 0.0) {
    r10 = bitcast<f32>(0x0u);
    r11 = (r10 - r0);
    if (!wfinite(r11)) { sb |= 1u; }
  }
  let r12: f32 = select(r11, r0, r9 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 1u + 0u] = r12;
}
