// kernel k0
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 1>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
@compute @workgroup_size(64)
fn kernel_cd1529a9a3b19964d0f95c395cb5eff7(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = bitcast<f32>(0x414fd639u);
  let r2: f32 = (r0 * r1);
  if (!wfinite(r2)) { sb |= 1u; }
  let r3: f32 = bitcast<f32>(0x3f800000u);
  let r4: f32 = bitcast<f32>(0x429c774cu);
  let r5: f32 = (r2 + r4);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = sin(r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(0x472aee8cu);
  let r8: f32 = (r6 * r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = select(((r8 % r3) + r3) % r3, 0.0, r3 == 0.0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = abs(r9);
  let r12: f32 = bitcast<f32>(0x431c774cu);
  let r13: f32 = (r2 + r12);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = sin(r13);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r14 * r7);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = select(((r15 % r3) + r3) % r3, 0.0, r3 == 0.0);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = abs(r16);
  let r18: f32 = (r10 + r17);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r18 - r3);
  if (!wfinite(r19)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(0x436ab2f2u);
  let r22: f32 = (r2 + r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = sin(r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r23 * r7);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = select(((r24 % r3) + r3) % r3, 0.0, r3 == 0.0);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = abs(r25);
  let r27: f32 = bitcast<f32>(0x3f000000u);
  let r28: f32 = (r26 - r27);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r19 * r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = bitcast<f32>(0x3dcccccdu);
  let r32: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x40c00000u);
  let r34: f32 = (r19 * r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = (r32 + r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = sin(r35);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = (r31 * r36);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = (r28 + r37);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = (r38 * r29);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = bitcast<f32>(0x0u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r30;
  outbuf[i * 3u + 1u] = r39;
  outbuf[i * 3u + 2u] = r40;
}
