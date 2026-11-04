// kernel image1
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
fn kernel_279041b1a34538489fd6088a5f812e75(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].y));
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3ee66666u);
  let r3: f32 = bitcast<f32>(0x3f400000u);
  let r4: f32 = bitcast<f32>(0x3f333333u);
  let r5: f32 = bitcast<f32>(0x3f19999au);
  let r6: f32 = bitcast<f32>(0x3ecccccdu);
  let r7: f32 = bitcast<f32>(0x3f59999au);
  let r8: f32 = (r5 - r2);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r6 - r3);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 - r4);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r1);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 * r1);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r10 * r1);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = (r2 + r11);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r3 + r12);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r4 + r13);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r14;
  outbuf[i * 4u + 1u] = r15;
  outbuf[i * 4u + 2u] = r16;
  outbuf[i * 4u + 3u] = r17;
}

// kernel image3
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_3a15ff44c248e0e6ef7b2cec630e3acd(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3daaaaabu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e4ccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3e99999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3dcccccdu);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3eb33333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x0u);
  let r34: f32 = max(r33, r32);
  let r35: f32 = bitcast<f32>(0x3f666666u);
  let r36: f32 = bitcast<f32>(0x3f4ccccdu);
  let r37: f32 = bitcast<f32>(0x3f333333u);
  let r38: f32 = (r34 * r34);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = (r7 * r27);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r2 - r39);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r38 * r40);
  if (!wfinite(r41)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r35;
  outbuf[i * 4u + 1u] = r36;
  outbuf[i * 4u + 2u] = r37;
  outbuf[i * 4u + 3u] = r41;
}

// kernel image5
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 3>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_ec14dc1c7079f649e2a4d045e2d19a8f(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].w));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].w));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[2].x));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3daaaaabu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e4ccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3e99999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3dcccccdu);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3fe66666u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = bitcast<f32>(0x3f23d70au);
  let r32: f32 = (r30 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r27 * r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = (r2 - r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = bitcast<f32>(0x0u);
  let r37: f32 = max(r36, r35);
  let r38: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = bitcast<f32>(uniforms[1].y);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = bitcast<f32>(uniforms[1].z);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = bitcast<f32>(0xbe4ccccdu);
  let r42: f32 = bitcast<f32>(0xbe99999au);
  let r43: f32 = bitcast<f32>(0x3f666666u);
  let r44: f32 = (r41 * r7);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = (r42 * r7);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = (r43 * r7);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = (r38 + r44);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = (r39 + r45);
  if (!wfinite(r48)) { sb |= 1u; }
  let r49: f32 = (r40 + r46);
  if (!wfinite(r49)) { sb |= 1u; }
  let r50: f32 = (r37 * r37);
  if (!wfinite(r50)) { sb |= 1u; }
  let r51: f32 = bitcast<f32>(0x3eb33333u);
  let r52: f32 = (r7 * r51);
  if (!wfinite(r52)) { sb |= 1u; }
  let r53: f32 = (r2 - r52);
  if (!wfinite(r53)) { sb |= 1u; }
  let r54: f32 = (r50 * r53);
  if (!wfinite(r54)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r47;
  outbuf[i * 4u + 1u] = r48;
  outbuf[i * 4u + 2u] = r49;
  outbuf[i * 4u + 3u] = r54;
}

// kernel image7
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_31e5001e6efce6cfa780795fe6557e20(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3daaaaabu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e4ccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3e99999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3dcccccdu);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x40533333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x0u);
  let r34: f32 = max(r33, r32);
  let r35: f32 = bitcast<f32>(0x3e19999au);
  let r36: f32 = bitcast<f32>(0x3f19999au);
  let r37: f32 = (r34 * r34);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = bitcast<f32>(0x3eb33333u);
  let r39: f32 = (r7 * r38);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r2 - r39);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r37 * r40);
  if (!wfinite(r41)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r22;
  outbuf[i * 4u + 1u] = r35;
  outbuf[i * 4u + 2u] = r36;
  outbuf[i * 4u + 3u] = r41;
}

// kernel image10
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
fn kernel_279041b1a34538489fd6088a5f812e75(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].y));
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3ee66666u);
  let r3: f32 = bitcast<f32>(0x3f400000u);
  let r4: f32 = bitcast<f32>(0x3f333333u);
  let r5: f32 = bitcast<f32>(0x3f19999au);
  let r6: f32 = bitcast<f32>(0x3ecccccdu);
  let r7: f32 = bitcast<f32>(0x3f59999au);
  let r8: f32 = (r5 - r2);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r6 - r3);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 - r4);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r1);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 * r1);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r10 * r1);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = (r2 + r11);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r3 + r12);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r4 + r13);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r14;
  outbuf[i * 4u + 1u] = r15;
  outbuf[i * 4u + 2u] = r16;
  outbuf[i * 4u + 3u] = r17;
}

// kernel image12
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_af26fdd5299dc8fbc6204b901fd29abb(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3d924925u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e19999au);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f59999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3e99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3eb33333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x0u);
  let r34: f32 = max(r33, r32);
  let r35: f32 = bitcast<f32>(0x3f400000u);
  let r36: f32 = bitcast<f32>(0x3f333333u);
  let r37: f32 = (r34 * r34);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = bitcast<f32>(0x3f19999au);
  let r39: f32 = (r7 * r38);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r2 - r39);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r37 * r40);
  if (!wfinite(r41)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r14;
  outbuf[i * 4u + 1u] = r35;
  outbuf[i * 4u + 2u] = r36;
  outbuf[i * 4u + 3u] = r41;
}

// kernel image14
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_c00d92bd671777305712c283cae47cfa(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3d924925u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e19999au);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f59999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3e99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3fe66666u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = bitcast<f32>(0x3f23d70au);
  let r32: f32 = (r30 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r27 * r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = (r2 - r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = bitcast<f32>(0x0u);
  let r37: f32 = max(r36, r35);
  let r38: f32 = bitcast<f32>(0x3ee66666u);
  let r39: f32 = bitcast<f32>(0x3dcccccdu);
  let r40: f32 = bitcast<f32>(0xbdcccccdu);
  let r41: f32 = bitcast<f32>(0xbe99999au);
  let r42: f32 = bitcast<f32>(0x3f333333u);
  let r43: f32 = (r40 * r7);
  if (!wfinite(r43)) { sb |= 1u; }
  let r44: f32 = (r41 * r7);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = (r42 * r7);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = (r2 + r43);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = (r38 + r44);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = (r39 + r45);
  if (!wfinite(r48)) { sb |= 1u; }
  let r49: f32 = (r37 * r37);
  if (!wfinite(r49)) { sb |= 1u; }
  let r50: f32 = bitcast<f32>(0x3f19999au);
  let r51: f32 = (r7 * r50);
  if (!wfinite(r51)) { sb |= 1u; }
  let r52: f32 = (r2 - r51);
  if (!wfinite(r52)) { sb |= 1u; }
  let r53: f32 = (r49 * r52);
  if (!wfinite(r53)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r46;
  outbuf[i * 4u + 1u] = r47;
  outbuf[i * 4u + 2u] = r48;
  outbuf[i * 4u + 3u] = r53;
}

// kernel image16
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_be1ecbf8c7e3db24d2b3cef14bf150b1(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3d924925u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e19999au);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f59999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3e99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x40533333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x0u);
  let r34: f32 = max(r33, r32);
  let r35: f32 = bitcast<f32>(0x3d4ccccdu);
  let r36: f32 = bitcast<f32>(0x3dcccccdu);
  let r37: f32 = bitcast<f32>(0x3ee66666u);
  let r38: f32 = (r34 * r34);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = bitcast<f32>(0x3f19999au);
  let r40: f32 = (r7 * r39);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r2 - r40);
  if (!wfinite(r41)) { sb |= 1u; }
  let r42: f32 = (r38 * r41);
  if (!wfinite(r42)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r35;
  outbuf[i * 4u + 1u] = r36;
  outbuf[i * 4u + 2u] = r37;
  outbuf[i * 4u + 3u] = r42;
}

// kernel image19
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
fn kernel_f71c5a9fa8496fd81b560d110c015f60(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].y));
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3ee66666u);
  let r3: f32 = bitcast<f32>(0x3d4ccccdu);
  let r4: f32 = bitcast<f32>(0x3dcccccdu);
  let r5: f32 = bitcast<f32>(0x3f400000u);
  let r6: f32 = bitcast<f32>(0x3df5c28fu);
  let r7: f32 = (r5 - r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r4 - r3);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r6 - r4);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r1);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r1);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 * r1);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r2 + r10);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = (r3 + r11);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r4 + r12);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r13;
  outbuf[i * 4u + 1u] = r14;
  outbuf[i * 4u + 2u] = r15;
  outbuf[i * 4u + 3u] = r16;
}

// kernel image21
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_77dc76b3f033322ecc7da21a72c9ee41(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3de38e39u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e99999au);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f333333u);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x0u);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3eb33333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = max(r22, r32);
  let r34: f32 = bitcast<f32>(0x3f666666u);
  let r35: f32 = (r33 * r33);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = bitcast<f32>(0x3ecccccdu);
  let r37: f32 = (r7 * r36);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = (r2 - r37);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = (r35 * r38);
  if (!wfinite(r39)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r34;
  outbuf[i * 4u + 1u] = r19;
  outbuf[i * 4u + 2u] = r8;
  outbuf[i * 4u + 3u] = r39;
}

// kernel image23
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_2de2c57bebbce975d1dfdbf4eefac88b(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3de38e39u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e99999au);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f333333u);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x0u);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3fe66666u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = bitcast<f32>(0x3f23d70au);
  let r32: f32 = (r30 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r27 * r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = (r2 - r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = max(r22, r35);
  let r37: f32 = bitcast<f32>(0x3dcccccdu);
  let r38: f32 = bitcast<f32>(0xbdcccccdu);
  let r39: f32 = (r8 * r7);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r38 * r7);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r14 * r7);
  if (!wfinite(r41)) { sb |= 1u; }
  let r42: f32 = (r14 + r39);
  if (!wfinite(r42)) { sb |= 1u; }
  let r43: f32 = (r2 + r40);
  if (!wfinite(r43)) { sb |= 1u; }
  let r44: f32 = (r37 + r41);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = (r36 * r36);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = bitcast<f32>(0x3ecccccdu);
  let r47: f32 = (r7 * r46);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = (r2 - r47);
  if (!wfinite(r48)) { sb |= 1u; }
  let r49: f32 = (r45 * r48);
  if (!wfinite(r49)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r42;
  outbuf[i * 4u + 1u] = r43;
  outbuf[i * 4u + 2u] = r44;
  outbuf[i * 4u + 3u] = r49;
}

// kernel image25
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_fe9f2759488cc720a8fd940c84e74173(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3de38e39u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3e99999au);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f333333u);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x0u);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x40533333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = max(r22, r32);
  let r34: f32 = bitcast<f32>(0x3dcccccdu);
  let r35: f32 = bitcast<f32>(0x3f4ccccdu);
  let r36: f32 = (r33 * r33);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = bitcast<f32>(0x3ecccccdu);
  let r38: f32 = (r7 * r37);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = (r2 - r38);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r36 * r39);
  if (!wfinite(r40)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r34;
  outbuf[i * 4u + 1u] = r19;
  outbuf[i * 4u + 2u] = r35;
  outbuf[i * 4u + 3u] = r40;
}

// kernel image28
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
fn kernel_fc62ede7ccb5b361b48d8fcb5e7e377f(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].y));
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3df5c28fu);
  let r3: f32 = bitcast<f32>(0x3ca3d70au);
  let r4: f32 = bitcast<f32>(0x3e0f5c29u);
  let r5: f32 = bitcast<f32>(0x3d23d70au);
  let r6: f32 = (r4 - r2);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = (r2 - r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r5 - r3);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r6 * r1);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r1);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r1);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r2 + r9);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r2 + r10);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = (r3 + r11);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r12;
  outbuf[i * 4u + 1u] = r13;
  outbuf[i * 4u + 2u] = r14;
  outbuf[i * 4u + 3u] = r15;
}

// kernel image30
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_40a852bd666d7dfabc1ce8fc103019ea(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f19999au);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e124925u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(0x3fd55555u);
  let r8: f32 = wpow(r6, r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = bitcast<f32>(0x3dcccccdu);
  let r10: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 + r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r0 - r12);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = bitcast<f32>(0x3ee66666u);
  let r16: f32 = (r8 * r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = bitcast<f32>(0x3f800000u);
  let r18: f32 = (r17 - r16);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r14 * r18);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = select(r13 / r19, 0.0, r19 == 0.0);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = (r1 - r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x0u);
  let r25: f32 = (r8 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r17 + r25);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r23 * r26);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = select(r22 / r27, 0.0, r27 == 0.0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x3eb33333u);
  let r30: f32 = (r20 * r20);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r28 * r28);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r30 + r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = (r29 * r32);
  if (!wfinite(r33)) { sb |= 1u; }
  let r34: f32 = (r17 - r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = max(r24, r34);
  let r36: f32 = bitcast<f32>(0x3e99999au);
  let r37: f32 = bitcast<f32>(0x3e4ccccdu);
  let r38: f32 = bitcast<f32>(0x3f666666u);
  let r39: f32 = (r35 * r35);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r8 * r38);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r17 - r40);
  if (!wfinite(r41)) { sb |= 1u; }
  let r42: f32 = (r39 * r41);
  if (!wfinite(r42)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r36;
  outbuf[i * 4u + 1u] = r37;
  outbuf[i * 4u + 2u] = r38;
  outbuf[i * 4u + 3u] = r42;
}

// kernel image32
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_e144cfa226f82a1da2fb9ba413db759a(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f19999au);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e124925u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(0x3fd55555u);
  let r8: f32 = wpow(r6, r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = bitcast<f32>(0x3dcccccdu);
  let r10: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 + r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r0 - r12);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = bitcast<f32>(0x3ee66666u);
  let r16: f32 = (r8 * r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = bitcast<f32>(0x3f800000u);
  let r18: f32 = (r17 - r16);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r14 * r18);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = select(r13 / r19, 0.0, r19 == 0.0);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = (r1 - r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x0u);
  let r25: f32 = (r8 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r17 + r25);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r23 * r26);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = select(r22 / r27, 0.0, r27 == 0.0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x3fe66666u);
  let r30: f32 = (r20 * r20);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r28 * r28);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r30 + r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x3f23d70au);
  let r34: f32 = (r32 - r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = abs(r34);
  let r36: f32 = (r29 * r35);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = (r17 - r36);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = max(r24, r37);
  let r39: f32 = bitcast<f32>(0x3eb33333u);
  let r40: f32 = bitcast<f32>(0x3f333333u);
  let r41: f32 = bitcast<f32>(0xbf333333u);
  let r42: f32 = bitcast<f32>(0x3e99999au);
  let r43: f32 = (r41 * r8);
  if (!wfinite(r43)) { sb |= 1u; }
  let r44: f32 = (r24 * r8);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = (r42 * r8);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = (r17 + r43);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = (r39 + r44);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = (r40 + r45);
  if (!wfinite(r48)) { sb |= 1u; }
  let r49: f32 = (r38 * r38);
  if (!wfinite(r49)) { sb |= 1u; }
  let r50: f32 = bitcast<f32>(0x3f666666u);
  let r51: f32 = (r8 * r50);
  if (!wfinite(r51)) { sb |= 1u; }
  let r52: f32 = (r17 - r51);
  if (!wfinite(r52)) { sb |= 1u; }
  let r53: f32 = (r49 * r52);
  if (!wfinite(r53)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r46;
  outbuf[i * 4u + 1u] = r47;
  outbuf[i * 4u + 2u] = r48;
  outbuf[i * 4u + 3u] = r53;
}

// kernel image34
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_39b8490e231201e136985f5c8a13d799(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f19999au);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e124925u);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(0x3fd55555u);
  let r8: f32 = wpow(r6, r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = bitcast<f32>(0x3dcccccdu);
  let r10: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 + r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r0 - r12);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = bitcast<f32>(0x3ee66666u);
  let r16: f32 = (r8 * r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = bitcast<f32>(0x3f800000u);
  let r18: f32 = (r17 - r16);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r14 * r18);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = select(r13 / r19, 0.0, r19 == 0.0);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = (r1 - r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x0u);
  let r25: f32 = (r8 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r17 + r25);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r23 * r26);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = select(r22 / r27, 0.0, r27 == 0.0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x40533333u);
  let r30: f32 = (r20 * r20);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r28 * r28);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r30 + r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = (r29 * r32);
  if (!wfinite(r33)) { sb |= 1u; }
  let r34: f32 = (r17 - r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = max(r24, r34);
  let r36: f32 = bitcast<f32>(0x3e19999au);
  let r37: f32 = bitcast<f32>(0x3e4ccccdu);
  let r38: f32 = bitcast<f32>(0x3f333333u);
  let r39: f32 = (r35 * r35);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = bitcast<f32>(0x3f666666u);
  let r41: f32 = (r8 * r40);
  if (!wfinite(r41)) { sb |= 1u; }
  let r42: f32 = (r17 - r41);
  if (!wfinite(r42)) { sb |= 1u; }
  let r43: f32 = (r39 * r42);
  if (!wfinite(r43)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r36;
  outbuf[i * 4u + 1u] = r37;
  outbuf[i * 4u + 2u] = r38;
  outbuf[i * 4u + 3u] = r43;
}

// kernel image37
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
fn kernel_5ebe970c20ef069e90c2aa3f22475322(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].y));
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3da3d70au);
  let r3: f32 = bitcast<f32>(0x3d4ccccdu);
  let r4: f32 = bitcast<f32>(0x3df5c28fu);
  let r5: f32 = bitcast<f32>(0x3dcccccdu);
  let r6: f32 = (r5 - r2);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = (r3 - r3);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r4 - r4);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r6 * r1);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r1);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r1);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r2 + r9);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r3 + r10);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = (r4 + r11);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r12;
  outbuf[i * 4u + 1u] = r13;
  outbuf[i * 4u + 2u] = r14;
  outbuf[i * 4u + 3u] = r15;
}

// kernel image39
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_f502ce717778189582471eccd81a612c(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3dcccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f19999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3f99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3eb33333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x0u);
  let r34: f32 = max(r33, r32);
  let r35: f32 = (r34 * r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = (r35 * r16);
  if (!wfinite(r36)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r19;
  outbuf[i * 4u + 1u] = r8;
  outbuf[i * 4u + 2u] = r27;
  outbuf[i * 4u + 3u] = r36;
}

// kernel image41
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 3>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_c3f0e3a68bd90d3f557e7afac472b369(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].w));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].w));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[2].x));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3dcccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f19999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3f99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x3fe66666u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = bitcast<f32>(0x3f23d70au);
  let r32: f32 = (r30 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r27 * r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = (r2 - r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = bitcast<f32>(0x0u);
  let r37: f32 = max(r36, r35);
  let r38: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = bitcast<f32>(uniforms[1].y);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = bitcast<f32>(uniforms[1].z);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = bitcast<f32>(0x3f59999au);
  let r42: f32 = bitcast<f32>(0xbf19999au);
  let r43: f32 = (r41 * r7);
  if (!wfinite(r43)) { sb |= 1u; }
  let r44: f32 = (r42 * r7);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = (r38 + r43);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = (r39 + r44);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = (r40 + r44);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = (r37 * r37);
  if (!wfinite(r48)) { sb |= 1u; }
  let r49: f32 = (r48 * r16);
  if (!wfinite(r49)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r45;
  outbuf[i * 4u + 1u] = r46;
  outbuf[i * 4u + 2u] = r47;
  outbuf[i * 4u + 3u] = r49;
}

// kernel image43
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_8ab3cdeeb0c10d7636696d30b1ab18b8(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3dcccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f19999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3f99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = bitcast<f32>(0x40533333u);
  let r28: f32 = (r18 * r18);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r26 * r26);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r28 + r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r27 * r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r2 - r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = bitcast<f32>(0x0u);
  let r34: f32 = max(r33, r32);
  let r35: f32 = bitcast<f32>(0x3f733333u);
  let r36: f32 = bitcast<f32>(0x3f7ae148u);
  let r37: f32 = (r34 * r34);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = (r37 * r16);
  if (!wfinite(r38)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r35;
  outbuf[i * 4u + 1u] = r36;
  outbuf[i * 4u + 2u] = r35;
  outbuf[i * 4u + 3u] = r38;
}

// kernel image46
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_ef04102bda509f9330ee6cf83274e268(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(0x3dcccccdu);
  let r9: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 + r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r0 - r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3f19999au);
  let r15: f32 = (r7 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r2 - r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r13 * r16);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = select(r12 / r17, 0.0, r17 == 0.0);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = bitcast<f32>(0x3f000000u);
  let r20: f32 = (r1 - r19);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x3f99999au);
  let r23: f32 = (r7 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r2 + r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r21 * r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = select(r20 / r25, 0.0, r25 == 0.0);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r18 * r18);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = (r26 * r26);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r27 + r28);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = bitcast<f32>(0x3f8ccccdu);
  let r31: f32 = (r29 - r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = abs(r31);
  let r33: f32 = bitcast<f32>(0x40e00000u);
  let r34: f32 = (r33 * r32);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = (r2 - r34);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = bitcast<f32>(0x0u);
  let r37: f32 = max(r36, r35);
  let r38: f32 = bitcast<f32>(0x3e99999au);
  let r39: f32 = bitcast<f32>(0x3e19999au);
  let r40: f32 = (r37 * r37);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r40 * r16);
  if (!wfinite(r41)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r2;
  outbuf[i * 4u + 1u] = r38;
  outbuf[i * 4u + 2u] = r39;
  outbuf[i * 4u + 3u] = r41;
}

// kernel image49
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
fn kernel_f71c5a9fa8496fd81b560d110c015f60(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[0].y));
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[0].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3ee66666u);
  let r3: f32 = bitcast<f32>(0x3d4ccccdu);
  let r4: f32 = bitcast<f32>(0x3dcccccdu);
  let r5: f32 = bitcast<f32>(0x3f400000u);
  let r6: f32 = bitcast<f32>(0x3df5c28fu);
  let r7: f32 = (r5 - r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r4 - r3);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r6 - r4);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r7 * r1);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r8 * r1);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r9 * r1);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r2 + r10);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = (r3 + r11);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r4 + r12);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = bitcast<f32>(0x3f800000u);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r13;
  outbuf[i * 4u + 1u] = r14;
  outbuf[i * 4u + 2u] = r15;
  outbuf[i * 4u + 3u] = r16;
}

// kernel image51
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_7726b99031fc8563051443f16b5dbb7b(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r7 * r8);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r4 + r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r0 - r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(0x3ee66666u);
  let r14: f32 = (r7 * r13);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r2 - r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 * r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = select(r11 / r16, 0.0, r16 == 0.0);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r1 - r18);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(0x3f4ccccdu);
  let r22: f32 = (r7 * r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = (r2 + r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r20 * r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = select(r19 / r24, 0.0, r24 == 0.0);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = bitcast<f32>(0x3eb33333u);
  let r27: f32 = (r17 * r17);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = (r25 * r25);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r27 + r28);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r26 * r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r2 - r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = bitcast<f32>(0x0u);
  let r33: f32 = max(r32, r31);
  let r34: f32 = bitcast<f32>(0x3f666666u);
  let r35: f32 = bitcast<f32>(0x3f000000u);
  let r36: f32 = bitcast<f32>(0x3e99999au);
  let r37: f32 = (r33 * r33);
  if (!wfinite(r37)) { sb |= 1u; }
  let r38: f32 = bitcast<f32>(0x3f333333u);
  let r39: f32 = (r7 * r38);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r2 - r39);
  if (!wfinite(r40)) { sb |= 1u; }
  let r41: f32 = (r37 * r40);
  if (!wfinite(r41)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r34;
  outbuf[i * 4u + 1u] = r35;
  outbuf[i * 4u + 2u] = r36;
  outbuf[i * 4u + 3u] = r41;
}

// kernel image53
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_52ab5b8b8ab3d47c36cfeac389f806ce(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r7 * r8);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r4 + r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r0 - r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(0x3ee66666u);
  let r14: f32 = (r7 * r13);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r2 - r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 * r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = select(r11 / r16, 0.0, r16 == 0.0);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r1 - r18);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(0x3f4ccccdu);
  let r22: f32 = (r7 * r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = (r2 + r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r20 * r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = select(r19 / r24, 0.0, r24 == 0.0);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = bitcast<f32>(0x3fe66666u);
  let r27: f32 = (r17 * r17);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = (r25 * r25);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r27 + r28);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = bitcast<f32>(0x3f23d70au);
  let r31: f32 = (r29 - r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = abs(r31);
  let r33: f32 = (r26 * r32);
  if (!wfinite(r33)) { sb |= 1u; }
  let r34: f32 = (r2 - r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = bitcast<f32>(0x0u);
  let r36: f32 = max(r35, r34);
  let r37: f32 = bitcast<f32>(0x3f333333u);
  let r38: f32 = bitcast<f32>(0x3dcccccdu);
  let r39: f32 = bitcast<f32>(0xbe4ccccdu);
  let r40: f32 = bitcast<f32>(0x3f000000u);
  let r41: f32 = (r4 * r7);
  if (!wfinite(r41)) { sb |= 1u; }
  let r42: f32 = (r39 * r7);
  if (!wfinite(r42)) { sb |= 1u; }
  let r43: f32 = (r40 * r7);
  if (!wfinite(r43)) { sb |= 1u; }
  let r44: f32 = (r37 + r41);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = (r2 + r42);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = (r38 + r43);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = (r36 * r36);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = (r7 * r37);
  if (!wfinite(r48)) { sb |= 1u; }
  let r49: f32 = (r2 - r48);
  if (!wfinite(r49)) { sb |= 1u; }
  let r50: f32 = (r47 * r49);
  if (!wfinite(r50)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r44;
  outbuf[i * 4u + 1u] = r45;
  outbuf[i * 4u + 2u] = r46;
  outbuf[i * 4u + 3u] = r50;
}

// kernel image55
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
@compute @workgroup_size(64)
fn kernel_c601133f6c93cbc9db25e9bb11cf56c9(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = wpow(r0, r2);
  if (!wfinite(r3)) { sb |= 1u; }
  let r4: f32 = bitcast<f32>(0x3e4ccccdu);
  let r5: f32 = select(((r3 % r4) + r4) % r4, 0.0, r4 == 0.0);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r3 - r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = wpow(r6, r2);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r7 * r8);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r4 + r9);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r0 - r10);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = bitcast<f32>(0x3ee66666u);
  let r14: f32 = (r7 * r13);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = (r2 - r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 * r15);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = select(r11 / r16, 0.0, r16 == 0.0);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r1 - r18);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = bitcast<f32>(0x3f4ccccdu);
  let r22: f32 = (r7 * r21);
  if (!wfinite(r22)) { sb |= 1u; }
  let r23: f32 = (r2 + r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r20 * r23);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = select(r19 / r24, 0.0, r24 == 0.0);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = bitcast<f32>(0x40533333u);
  let r27: f32 = (r17 * r17);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = (r25 * r25);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = (r27 + r28);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = (r26 * r29);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r2 - r30);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = bitcast<f32>(0x0u);
  let r33: f32 = max(r32, r31);
  let r34: f32 = bitcast<f32>(0x3dcccccdu);
  let r35: f32 = bitcast<f32>(0x3f000000u);
  let r36: f32 = (r33 * r33);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = bitcast<f32>(0x3f333333u);
  let r38: f32 = (r7 * r37);
  if (!wfinite(r38)) { sb |= 1u; }
  let r39: f32 = (r2 - r38);
  if (!wfinite(r39)) { sb |= 1u; }
  let r40: f32 = (r36 * r39);
  if (!wfinite(r40)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r34;
  outbuf[i * 4u + 1u] = r35;
  outbuf[i * 4u + 2u] = r21;
  outbuf[i * 4u + 3u] = r40;
}
