// kernel image1
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(3) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
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
fn kernel_bbbf5d92ee45ba28011c58bb4a03e973(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].y));
  var r42: f32 = 0.0;
  var r43: f32 = 0.0;
  var r44: f32 = 0.0;
  var r45: f32 = 0.0;
  var r46: f32 = 0.0;
  var r47: f32 = 0.0;
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].z));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x3f000000u);
  let r4: f32 = bitcast<f32>(0x3d4ccccdu);
  let r5: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r4 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = select(r6 / r7, 0.0, r7 == 0.0);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = (r0 * r5);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = (r9 * r4);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r1 * r11);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r12 * r4);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(uniforms[1].x);
  if (!wfinite(r14)) { sb |= 1u; }
  let r15: f32 = wfbm(vec3<f32>(r10, r13, r14), 2, 0);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r10 + r8);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = wfbm(vec3<f32>(r16, r13, r14), 2, 0);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = (r17 - r15);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = (r13 + r8);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = wfbm(vec3<f32>(r10, r19, r14), 2, 0);
  if (!wfinite(r20)) { sb |= 1u; }
  let r21: f32 = (r20 - r15);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x42400000u);
  let r23: f32 = (r15 * r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x41c00000u);
  let r25: f32 = (r23 - r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r25 + r3);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = select(((r26 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = (r27 - r3);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = abs(r28);
  let r30: f32 = (r18 * r18);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r21 * r21);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r30 + r31);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = sqrt(abs(r32));
  let r34: f32 = (r22 * r33);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = select(r29 / r34, 0.0, r34 == 0.0);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = bitcast<f32>(0x3eb851ecu);
  let r37: f32 = bitcast<f32>(0xbf000000u);
  let r38: f32 = select(0.0, 1.0, (r25 > r37));
  let r39: f32 = bitcast<f32>(0x40900000u);
  let r40: f32 = select(0.0, 1.0, (r25 < r39));
  let r41: f32 = select(0.0, 1.0, (r38 != 0.0 && r40 != 0.0));
  if (r41 != 0.0) {
    r42 = bitcast<f32>(0x3f4ccccdu);
    r43 = (r35 * r42);
    if (!wfinite(r43)) { sb |= 1u; }
    r44 = (r2 - r43);
    if (!wfinite(r44)) { sb |= 1u; }
    r45 = bitcast<f32>(0x0u);
    r46 = max(r45, r44);
  }
  if (r41 == 0.0) {
    r47 = bitcast<f32>(0x0u);
  }
  let r48: f32 = select(r47, r46, r41 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r36;
  outbuf[i * 4u + 1u] = r36;
  outbuf[i * 4u + 2u] = r36;
  outbuf[i * 4u + 3u] = r48;
}

// kernel image3
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(3) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
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
fn kernel_e87630f57fc6d038c7d0cc14c480b040(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  var r58: f32 = 0.0;
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x3f000000u);
  let r4: f32 = bitcast<f32>(0x3ebd70a4u);
  let r5: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r0 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r1 * r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = select(((r6 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = select(((r8 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r6 - r9);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r8 - r10);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r11 + r3);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3d4ccccdu);
  let r15: f32 = (r13 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 + r3);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r16 * r14);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = wfbm(vec3<f32>(r15, r17, r18), 2, 0);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(0x42400000u);
  let r21: f32 = (r19 * r20);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x41c00000u);
  let r23: f32 = (r21 - r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x0u);
  let r25: f32 = (r23 - r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r11 * r4);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r12 * r4);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = wfbm(vec3<f32>(r26, r27, r3), 1, 0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x42740000u);
  let r30: f32 = (r29 * r28);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = select(((r30 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r9 - r3);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r10 - r3);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = abs(r34);
  let r36: f32 = (r25 - r3);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = abs(r36);
  let r38: f32 = bitcast<f32>(0x3ecccccdu);
  let r39: f32 = select(0.0, 1.0, (r37 < r38));
  let r40: f32 = bitcast<f32>(0x3e4ccccdu);
  let r41: f32 = select(0.0, 1.0, (r31 > r40));
  let r42: f32 = select(0.0, 1.0, (r31 < r2));
  let r43: f32 = select(0.0, 1.0, (r41 != 0.0 && r42 != 0.0));
  let r44: f32 = select(0.0, 1.0, (r39 != 0.0 && r43 != 0.0));
  let r45: f32 = (r33 * r2);
  if (!wfinite(r45)) { sb |= 1u; }
  let r46: f32 = max(r45, r35);
  let r47: f32 = (r46 - r24);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = abs(r47);
  let r49: f32 = bitcast<f32>(0x3e99999au);
  let r50: f32 = select(0.0, 1.0, (r48 < r49));
  let r51: f32 = (r33 - r35);
  if (!wfinite(r51)) { sb |= 1u; }
  let r52: f32 = abs(r51);
  let r53: f32 = bitcast<f32>(0x3db851ecu);
  let r54: f32 = select(0.0, 1.0, (r52 < r53));
  let r55: f32 = select(0.0, 1.0, (r50 != 0.0 && r54 != 0.0));
  let r56: f32 = bitcast<f32>(0x3f59999au);
  let r57: f32 = select(0.0, 1.0, (r44 != 0.0 && r55 != 0.0));
  if (r57 == 0.0) {
    r58 = bitcast<f32>(0x0u);
  }
  let r59: f32 = select(r58, r2, r57 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r56;
  outbuf[i * 4u + 1u] = r56;
  outbuf[i * 4u + 2u] = r56;
  outbuf[i * 4u + 3u] = r59;
}

// kernel image5
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(3) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
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
fn kernel_8ed6731c666e62cf1e4563803c556608(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  var r57: f32 = 0.0;
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x3f000000u);
  let r4: f32 = bitcast<f32>(0x3ebd70a4u);
  let r5: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r0 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r1 * r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = select(((r6 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = select(((r8 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r6 - r9);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r8 - r10);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r11 + r3);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3d4ccccdu);
  let r15: f32 = (r13 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 + r3);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r16 * r14);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = wfbm(vec3<f32>(r15, r17, r18), 2, 0);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(0x42400000u);
  let r21: f32 = (r19 * r20);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x41c00000u);
  let r23: f32 = (r21 - r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = (r23 - r2);
  if (!wfinite(r24)) { sb |= 1u; }
  let r25: f32 = (r11 * r4);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r12 * r4);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = wfbm(vec3<f32>(r25, r26, r3), 1, 0);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = bitcast<f32>(0x42740000u);
  let r29: f32 = (r28 * r27);
  if (!wfinite(r29)) { sb |= 1u; }
  let r30: f32 = select(((r29 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = (r9 - r3);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = abs(r31);
  let r33: f32 = (r10 - r3);
  if (!wfinite(r33)) { sb |= 1u; }
  let r34: f32 = abs(r33);
  let r35: f32 = (r24 - r3);
  if (!wfinite(r35)) { sb |= 1u; }
  let r36: f32 = abs(r35);
  let r37: f32 = bitcast<f32>(0x3ecccccdu);
  let r38: f32 = select(0.0, 1.0, (r36 < r37));
  let r39: f32 = bitcast<f32>(0x3e99999au);
  let r40: f32 = select(0.0, 1.0, (r30 > r39));
  let r41: f32 = select(0.0, 1.0, (r30 < r2));
  let r42: f32 = select(0.0, 1.0, (r40 != 0.0 && r41 != 0.0));
  let r43: f32 = select(0.0, 1.0, (r38 != 0.0 && r42 != 0.0));
  let r44: f32 = (r32 * r2);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = max(r44, r34);
  let r46: f32 = bitcast<f32>(0x0u);
  let r47: f32 = (r45 - r46);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = abs(r47);
  let r49: f32 = bitcast<f32>(0x3db851ecu);
  let r50: f32 = select(0.0, 1.0, (r48 < r49));
  let r51: f32 = (r32 - r34);
  if (!wfinite(r51)) { sb |= 1u; }
  let r52: f32 = abs(r51);
  let r53: f32 = select(0.0, 1.0, (r52 < r2));
  let r54: f32 = select(0.0, 1.0, (r50 != 0.0 && r53 != 0.0));
  let r55: f32 = bitcast<f32>(0x3f4ccccdu);
  let r56: f32 = select(0.0, 1.0, (r43 != 0.0 && r54 != 0.0));
  if (r56 == 0.0) {
    r57 = bitcast<f32>(0x0u);
  }
  let r58: f32 = select(r57, r2, r56 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r55;
  outbuf[i * 4u + 1u] = r55;
  outbuf[i * 4u + 2u] = r55;
  outbuf[i * 4u + 3u] = r58;
}

// kernel image7
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(3) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
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
fn kernel_568917e37bd5f093f58aad2fc18d3be5(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  var r59: f32 = 0.0;
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x3f000000u);
  let r4: f32 = bitcast<f32>(0x3ebd70a4u);
  let r5: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r0 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r1 * r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = select(((r6 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = select(((r8 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r6 - r9);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r8 - r10);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r11 + r3);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3d4ccccdu);
  let r15: f32 = (r13 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 + r3);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r16 * r14);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = wfbm(vec3<f32>(r15, r17, r18), 2, 0);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(0x42400000u);
  let r21: f32 = (r19 * r20);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x41c00000u);
  let r23: f32 = (r21 - r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x40000000u);
  let r25: f32 = (r23 - r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r11 * r4);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r12 * r4);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = wfbm(vec3<f32>(r26, r27, r3), 1, 0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x42740000u);
  let r30: f32 = (r29 * r28);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = select(((r30 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r9 - r3);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r10 - r3);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = abs(r34);
  let r36: f32 = (r25 - r3);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = abs(r36);
  let r38: f32 = bitcast<f32>(0x3ecccccdu);
  let r39: f32 = select(0.0, 1.0, (r37 < r38));
  let r40: f32 = bitcast<f32>(0x0u);
  let r41: f32 = select(0.0, 1.0, (r31 > r40));
  let r42: f32 = select(0.0, 1.0, (r31 < r38));
  let r43: f32 = select(0.0, 1.0, (r41 != 0.0 && r42 != 0.0));
  let r44: f32 = select(0.0, 1.0, (r39 != 0.0 && r43 != 0.0));
  let r45: f32 = bitcast<f32>(0x3fb33333u);
  let r46: f32 = (r33 * r45);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = max(r46, r35);
  let r48: f32 = bitcast<f32>(0x3e8a3d71u);
  let r49: f32 = (r47 - r48);
  if (!wfinite(r49)) { sb |= 1u; }
  let r50: f32 = abs(r49);
  let r51: f32 = bitcast<f32>(0x3d8f5c29u);
  let r52: f32 = select(0.0, 1.0, (r50 < r51));
  let r53: f32 = (r33 - r35);
  if (!wfinite(r53)) { sb |= 1u; }
  let r54: f32 = abs(r53);
  let r55: f32 = select(0.0, 1.0, (r54 < r2));
  let r56: f32 = select(0.0, 1.0, (r52 != 0.0 && r55 != 0.0));
  let r57: f32 = bitcast<f32>(0x3f666666u);
  let r58: f32 = select(0.0, 1.0, (r44 != 0.0 && r56 != 0.0));
  if (r58 == 0.0) {
    r59 = bitcast<f32>(0x0u);
  }
  let r60: f32 = select(r59, r2, r58 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r57;
  outbuf[i * 4u + 1u] = r57;
  outbuf[i * 4u + 2u] = r57;
  outbuf[i * 4u + 3u] = r60;
}

// kernel image9
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(3) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
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
fn kernel_55ffd94af2979bf9af6ecfff371d2d95(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  var r59: f32 = 0.0;
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x3f000000u);
  let r4: f32 = bitcast<f32>(0x3ebd70a4u);
  let r5: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r0 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r1 * r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = select(((r6 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = select(((r8 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r6 - r9);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r8 - r10);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r11 + r3);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3d4ccccdu);
  let r15: f32 = (r13 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 + r3);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r16 * r14);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = wfbm(vec3<f32>(r15, r17, r18), 2, 0);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(0x42400000u);
  let r21: f32 = (r19 * r20);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x41c00000u);
  let r23: f32 = (r21 - r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x40000000u);
  let r25: f32 = (r23 - r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r11 * r4);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r12 * r4);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = wfbm(vec3<f32>(r26, r27, r3), 1, 0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x42740000u);
  let r30: f32 = (r29 * r28);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = select(((r30 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r9 - r3);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r10 - r3);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = abs(r34);
  let r36: f32 = (r25 - r3);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = abs(r36);
  let r38: f32 = bitcast<f32>(0x3ecccccdu);
  let r39: f32 = select(0.0, 1.0, (r37 < r38));
  let r40: f32 = select(0.0, 1.0, (r31 > r38));
  let r41: f32 = bitcast<f32>(0x3f19999au);
  let r42: f32 = select(0.0, 1.0, (r31 < r41));
  let r43: f32 = select(0.0, 1.0, (r40 != 0.0 && r42 != 0.0));
  let r44: f32 = select(0.0, 1.0, (r39 != 0.0 && r43 != 0.0));
  let r45: f32 = bitcast<f32>(0x40900000u);
  let r46: f32 = (r33 * r45);
  if (!wfinite(r46)) { sb |= 1u; }
  let r47: f32 = max(r46, r35);
  let r48: f32 = bitcast<f32>(0x0u);
  let r49: f32 = (r47 - r48);
  if (!wfinite(r49)) { sb |= 1u; }
  let r50: f32 = abs(r49);
  let r51: f32 = bitcast<f32>(0x3ea3d70au);
  let r52: f32 = select(0.0, 1.0, (r50 < r51));
  let r53: f32 = (r33 - r35);
  if (!wfinite(r53)) { sb |= 1u; }
  let r54: f32 = abs(r53);
  let r55: f32 = select(0.0, 1.0, (r54 < r2));
  let r56: f32 = select(0.0, 1.0, (r52 != 0.0 && r55 != 0.0));
  let r57: f32 = bitcast<f32>(0x3f666666u);
  let r58: f32 = select(0.0, 1.0, (r44 != 0.0 && r56 != 0.0));
  if (r58 == 0.0) {
    r59 = bitcast<f32>(0x0u);
  }
  let r60: f32 = select(r59, r2, r58 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r57;
  outbuf[i * 4u + 1u] = r57;
  outbuf[i * 4u + 2u] = r57;
  outbuf[i * 4u + 3u] = r60;
}

// kernel image11
@group(0) @binding(0) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(1) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(2) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(3) var<storage, read> table: array<i32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
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
fn kernel_97f8fdaa71b039c77cb1fcdca99fa233(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= uniforms[0].x) { return; }
  var sb: u32 = 0u;
  let image_width = u32(bitcast<f32>(uniforms[1].x));
  var r56: f32 = 0.0;
  let r0: f32 = pixel_uv(i % image_width, bitcast<f32>(uniforms[1].x));
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: f32 = pixel_uv(i / image_width, bitcast<f32>(uniforms[1].y));
  if (!wfinite(r1)) { sb |= 1u; }
  let r2: f32 = bitcast<f32>(0x3f800000u);
  let r3: f32 = bitcast<f32>(0x3f000000u);
  let r4: f32 = bitcast<f32>(0x3ebd70a4u);
  let r5: f32 = bitcast<f32>(uniforms[0].y);
  if (!wfinite(r5)) { sb |= 1u; }
  let r6: f32 = (r0 * r5);
  if (!wfinite(r6)) { sb |= 1u; }
  let r7: f32 = bitcast<f32>(uniforms[0].z);
  if (!wfinite(r7)) { sb |= 1u; }
  let r8: f32 = (r1 * r7);
  if (!wfinite(r8)) { sb |= 1u; }
  let r9: f32 = select(((r6 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r9)) { sb |= 1u; }
  let r10: f32 = select(((r8 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r10)) { sb |= 1u; }
  let r11: f32 = (r6 - r9);
  if (!wfinite(r11)) { sb |= 1u; }
  let r12: f32 = (r8 - r10);
  if (!wfinite(r12)) { sb |= 1u; }
  let r13: f32 = (r11 + r3);
  if (!wfinite(r13)) { sb |= 1u; }
  let r14: f32 = bitcast<f32>(0x3d4ccccdu);
  let r15: f32 = (r13 * r14);
  if (!wfinite(r15)) { sb |= 1u; }
  let r16: f32 = (r12 + r3);
  if (!wfinite(r16)) { sb |= 1u; }
  let r17: f32 = (r16 * r14);
  if (!wfinite(r17)) { sb |= 1u; }
  let r18: f32 = bitcast<f32>(uniforms[0].w);
  if (!wfinite(r18)) { sb |= 1u; }
  let r19: f32 = wfbm(vec3<f32>(r15, r17, r18), 2, 0);
  if (!wfinite(r19)) { sb |= 1u; }
  let r20: f32 = bitcast<f32>(0x42400000u);
  let r21: f32 = (r19 * r20);
  if (!wfinite(r21)) { sb |= 1u; }
  let r22: f32 = bitcast<f32>(0x41c00000u);
  let r23: f32 = (r21 - r22);
  if (!wfinite(r23)) { sb |= 1u; }
  let r24: f32 = bitcast<f32>(0x40400000u);
  let r25: f32 = (r23 - r24);
  if (!wfinite(r25)) { sb |= 1u; }
  let r26: f32 = (r11 * r4);
  if (!wfinite(r26)) { sb |= 1u; }
  let r27: f32 = (r12 * r4);
  if (!wfinite(r27)) { sb |= 1u; }
  let r28: f32 = wfbm(vec3<f32>(r26, r27, r3), 1, 0);
  if (!wfinite(r28)) { sb |= 1u; }
  let r29: f32 = bitcast<f32>(0x42740000u);
  let r30: f32 = (r29 * r28);
  if (!wfinite(r30)) { sb |= 1u; }
  let r31: f32 = select(((r30 % r2) + r2) % r2, 0.0, r2 == 0.0);
  if (!wfinite(r31)) { sb |= 1u; }
  let r32: f32 = (r9 - r3);
  if (!wfinite(r32)) { sb |= 1u; }
  let r33: f32 = abs(r32);
  let r34: f32 = (r10 - r3);
  if (!wfinite(r34)) { sb |= 1u; }
  let r35: f32 = abs(r34);
  let r36: f32 = (r25 - r3);
  if (!wfinite(r36)) { sb |= 1u; }
  let r37: f32 = abs(r36);
  let r38: f32 = bitcast<f32>(0x3ecccccdu);
  let r39: f32 = select(0.0, 1.0, (r37 < r38));
  let r40: f32 = select(0.0, 1.0, (r31 > r3));
  let r41: f32 = select(0.0, 1.0, (r31 < r2));
  let r42: f32 = select(0.0, 1.0, (r40 != 0.0 && r41 != 0.0));
  let r43: f32 = select(0.0, 1.0, (r39 != 0.0 && r42 != 0.0));
  let r44: f32 = (r33 * r2);
  if (!wfinite(r44)) { sb |= 1u; }
  let r45: f32 = max(r44, r35);
  let r46: f32 = bitcast<f32>(0x0u);
  let r47: f32 = (r45 - r46);
  if (!wfinite(r47)) { sb |= 1u; }
  let r48: f32 = abs(r47);
  let r49: f32 = select(0.0, 1.0, (r48 < r2));
  let r50: f32 = (r33 - r35);
  if (!wfinite(r50)) { sb |= 1u; }
  let r51: f32 = abs(r50);
  let r52: f32 = select(0.0, 1.0, (r51 < r2));
  let r53: f32 = select(0.0, 1.0, (r49 != 0.0 && r52 != 0.0));
  let r54: f32 = bitcast<f32>(0x3f733333u);
  let r55: f32 = select(0.0, 1.0, (r43 != 0.0 && r53 != 0.0));
  if (r55 == 0.0) {
    r56 = bitcast<f32>(0x0u);
  }
  let r57: f32 = select(r56, r2, r55 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 4u + 0u] = r54;
  outbuf[i * 4u + 1u] = r54;
  outbuf[i * 4u + 2u] = r54;
  outbuf[i * 4u + 3u] = r57;
}
