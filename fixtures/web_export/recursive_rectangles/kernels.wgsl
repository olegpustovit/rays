// kernel f0
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> table: array<i32>;
@group(0) @binding(5) var<storage, read> gather0: array<f32>;
@group(0) @binding(6) var<storage, read> count_in: array<u32>;
@group(0) @binding(7) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
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
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
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
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
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
fn kernel_00e3af97fdd2c721cbcc37fb541bdc9b(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].z != 0u) { n = min(n, count_in[0] * uniforms[1].x / uniforms[1].y); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r2: u32 = 4u;
  let r4: u32 = 2u;
  let r7: f32 = bitcast<f32>(0x40000000u);
  let r19: f32 = bitcast<f32>(0x3f800000u);
  let r25: f32 = bitcast<f32>(0x40c00000u);
  let r28: f32 = bitcast<f32>(0x3e0f5c29u);
  let r29: f32 = bitcast<f32>(0x3f3851ecu);
  let r30: f32 = bitcast<f32>(0x3f000000u);
  let r33: f32 = bitcast<f32>(0x3c9374bcu);
  let r38: f32 = bitcast<f32>(0x3ebd70a4u);
  let r46: f32 = bitcast<f32>(0x3f1c28f6u);
  let r49: f32 = bitcast<f32>(0x0u);
  let r54: u32 = 1103515245u;
  let r57: f32 = bitcast<f32>(0x4640e400u);
  let r60: u32 = 65535u;
  let r63: f32 = bitcast<f32>(0x477fff00u);
  let r70: f32 = bitcast<f32>(0x40400000u);
  var r80: f32 = 0.0;
  let r81: f32 = bitcast<f32>(0x0u);
  var r82: f32 = 0.0;
  let r83: f32 = bitcast<f32>(0x0u);
  var r84: f32 = 0.0;
  var r85: f32 = 0.0;
  var r86: f32 = 0.0;
  var r87: f32 = 0.0;
  let r88: f32 = bitcast<f32>(0x0u);
  var r89: f32 = 0.0;
  let r90: f32 = bitcast<f32>(0xbf800000u);
  let r91: f32 = bitcast<f32>(0x0u);
  var r92: f32 = 0.0;
  var r93: f32 = 0.0;
  var r94: f32 = 0.0;
  var r95: f32 = 0.0;
  var r96: f32 = 0.0;
  var r97: f32 = 0.0;
  let r98: f32 = bitcast<f32>(0x0u);
  var r99: f32 = 0.0;
  var r100: f32 = 0.0;
  let r101: f32 = bitcast<f32>(0x3f800000u);
  var r102: f32 = 0.0;
  var r103: f32 = 0.0;
  var r104: f32 = 0.0;
  var r105: f32 = 0.0;
  var r106: f32 = 0.0;
  var r107: f32 = 0.0;
  var r108: f32 = 0.0;
  var r109: f32 = 0.0;
  var r110: f32 = 0.0;
  var r111: f32 = 0.0;
  let r112: f32 = bitcast<f32>(0x40000000u);
  var r113: f32 = 0.0;
  var r114: f32 = 0.0;
  var r115: f32 = 0.0;
  let r116: f32 = bitcast<f32>(0x0u);
  var r117: f32 = 0.0;
  var r118: f32 = 0.0;
  var r119: f32 = 0.0;
  var r120: f32 = 0.0;
  var r121: f32 = 0.0;
  var r122: f32 = 0.0;
  var r123: f32 = 0.0;
  let r124: f32 = bitcast<f32>(0x0u);
  var r125: f32 = 0.0;
  var r126: f32 = 0.0;
  var r127: f32 = 0.0;
  var r128: f32 = 0.0;
  var r129: f32 = 0.0;
  var r130: f32 = 0.0;
  let r131: f32 = bitcast<f32>(0x0u);
  var r132: f32 = 0.0;
  var r133: f32 = 0.0;
  var r134: f32 = 0.0;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r1: vec2<u32> = i64_from_f(r0);
  let r3: vec2<u32> = i64_divmod(r1, vec2<u32>(r2, 0u), false);
  if (all(vec2<u32>(r2, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r5: vec2<u32> = i64_divmod(r1, vec2<u32>(r4, 0u), false);
  if (all(vec2<u32>(r4, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r6: f32 = i64_to_f(r5);
  let r8: f32 = i64_to_f(i64_fmod(r5, vec2<u32>(2u, 0u)));
  let r9: vec2<u32> = i64_fmod(r5, vec2<u32>(2u, 0u));
  let r10: f32 = select(((r0 % r7) + r7) % r7, 0.0, r7 == 0.0);
  let r12: f32 = i64_to_f(r3);
  let r13: f32 = i64_to_f(i64_mul(r3, vec2<u32>(2u, 0u)));
  let r14: vec2<u32> = i64_mul(r3, vec2<u32>(2u, 0u));
  let r15: f32 = gather0[u32(clamp(i32(i64_to_f(r14)), 0, max(i32(uniforms[0].w) - 1, 0))) * 3u + 0u];
  let r16: f32 = gather0[u32(clamp(i32(i64_to_f(r14)), 0, max(i32(uniforms[0].w) - 1, 0))) * 3u + 1u];
  let r17: f32 = gather0[u32(clamp(i32(i64_to_f(r14)), 0, max(i32(uniforms[0].w) - 1, 0))) * 3u + 2u];
  let r18: f32 = i64_to_f(r14);
  let r20: f32 = i64_to_f(i64_add(r14, vec2<u32>(1u, 0u)));
  let r21: vec2<u32> = i64_add(r14, vec2<u32>(1u, 0u));
  let r22: f32 = gather0[u32(clamp(i32(i64_to_f(r21)), 0, max(i32(uniforms[0].w) - 1, 0))) * 3u + 0u];
  let r23: f32 = gather0[u32(clamp(i32(i64_to_f(r21)), 0, max(i32(uniforms[0].w) - 1, 0))) * 3u + 1u];
  let r24: f32 = gather0[u32(clamp(i32(i64_to_f(r21)), 0, max(i32(uniforms[0].w) - 1, 0))) * 3u + 2u];
  let r26: f32 = select(0.0, 1.0, (r22 >= r25));
  let r27: f32 = select(0.0, 1.0, (r23 >= r25));
  let r31: f32 = (r22 * r30);
  let r32: f32 = (r15 + r31);
  let r34: f32 = (r32 * r33);
  let r35: f32 = bitcast<f32>(uniforms[0].y);
  let r36: f32 = (r34 + r35);
  let r37: vec2<u32> = i64_from_f(bitcast<f32>(uniforms[0].z));
  let r39: f32 = i64_to_f(r37);
  let r40: f32 = (r39 * r38);
  let r41: f32 = (r36 + r40);
  let r42: f32 = (r23 * r30);
  let r43: f32 = (r16 + r42);
  let r44: f32 = (r43 * r33);
  let r45: f32 = (r44 - r35);
  let r47: f32 = (r39 * r46);
  let r48: f32 = (r45 + r47);
  let r50: f32 = wfbm(vec3<f32>(r41, r48, r49), 1, 0);
  let r51: f32 = (r29 * r50);
  let r52: f32 = (r28 + r51);
  let r53: vec2<u32> = i64_from_f(r17);
  let r55: vec2<u32> = i64_mul(r53, vec2<u32>(r54, 0u));
    let r56: f32 = i64_to_f(r55);
  let r58: f32 = i64_to_f(i64_add(r55, vec2<u32>(12345u, 0u)));
  let r59: vec2<u32> = i64_add(r55, vec2<u32>(12345u, 0u));
  let r61: u32 = ((r59).x & r60);
    let r62: f32 = i64_to_f(vec2<u32>(r61, 0u));
  let r64: f32 = select(r62 / r63, 0.0, r63 == 0.0);
  let r65: f32 = select(0.0, 1.0, (r27 == 0.0));
  let r66: f32 = select(0.0, 1.0, (r64 > r30));
  let r67: f32 = select(0.0, 1.0, (r65 != 0.0 || r66 != 0.0));
  let r68: f32 = select(0.0, 1.0, (r26 != 0.0 && r67 != 0.0));
  let r69: f32 = select(r23, r22, r68 != 0.0);
  let r71: f32 = (r69 - r70);
  let r72: f32 = (r69 * r52);
  let r73: vec2<u32> = i64_from_f(r72);
  let r74: f32 = i64_to_f(r73);
  let r75: f32 = min(r71, r74);
  let r76: f32 = max(r70, r75);
  let r77: vec2<u32> = i64_from_f(r76);
  let r78: f32 = select(0.0, 1.0, (r26 != 0.0 || r27 != 0.0));
  let r79: f32 = select(0.0, 1.0, (r78 == 0.0));
  if (r79 != 0.0) {
    r80 = i64_to_f(r9);
    r82 = select(0.0, 1.0, (r80 == r81));
    if (r82 != 0.0) {
      r84 = select(0.0, 1.0, (r10 == r83));
      r85 = select(r22, r15, r84 != 0.0);
      r86 = select(r23, r16, r84 != 0.0);
      r87 = select(r24, r17, r84 != 0.0);
    }
    if (r82 == 0.0) {
      r89 = select(0.0, 1.0, (r10 == r88));
      r92 = select(r90, r15, r89 != 0.0);
      r93 = select(r90, r16, r89 != 0.0);
      r94 = select(r91, r17, r89 != 0.0);
    }
    r95 = select(r92, r85, r82 != 0.0);
    r96 = select(r93, r86, r82 != 0.0);
    r97 = select(r94, r87, r82 != 0.0);
  }
  if (r79 == 0.0) {
    r99 = select(0.0, 1.0, (r10 == r98));
    if (r99 != 0.0) {
      r100 = i64_to_f(r9);
      r102 = select(0.0, 1.0, (r100 == r101));
      r103 = select(0.0, 1.0, (r68 != 0.0 && r102 != 0.0));
      if (r103 != 0.0) {
        r104 = i64_to_f(r77);
        r105 = (r15 + r104);
      }
      r106 = select(r15, r105, r103 != 0.0);
      r107 = select(0.0, 1.0, (r68 == 0.0));
      r108 = select(0.0, 1.0, (r107 != 0.0 && r102 != 0.0));
      if (r108 != 0.0) {
        r109 = i64_to_f(r77);
        r110 = (r16 + r109);
      }
      r111 = select(r16, r110, r108 != 0.0);
      r113 = (r17 * r112);
      r114 = (r113 + r100);
    }
    if (r99 == 0.0) {
      if (r68 != 0.0) {
        r115 = i64_to_f(r9);
        r117 = select(0.0, 1.0, (r115 == r116));
        if (r117 == 0.0) {
          r118 = i64_to_f(r77);
          r119 = (r22 - r118);
        }
        r120 = i64_to_f(r77);
        r121 = select(r119, r120, r117 != 0.0);
      }
      r122 = select(r22, r121, r68 != 0.0);
      if (r68 == 0.0) {
        r123 = i64_to_f(r9);
        r125 = select(0.0, 1.0, (r123 == r124));
        if (r125 == 0.0) {
          r126 = i64_to_f(r77);
          r127 = (r23 - r126);
        }
        r128 = i64_to_f(r77);
        r129 = select(r127, r128, r125 != 0.0);
      }
      r130 = select(r129, r23, r68 != 0.0);
    }
    r132 = select(r122, r106, r99 != 0.0);
    r133 = select(r130, r111, r99 != 0.0);
    r134 = select(r131, r114, r99 != 0.0);
  }
  let r135: f32 = select(r132, r95, r79 != 0.0);
  let r136: f32 = select(r133, r96, r79 != 0.0);
  let r137: f32 = select(r134, r97, r79 != 0.0);
  if (!wfinite(r135)) { sb |= 1u; }
  if (!wfinite(r136)) { sb |= 1u; }
  if (!wfinite(r137)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r135;
  outbuf[i * 3u + 1u] = r136;
  outbuf[i * 3u + 2u] = r137;
}

// kernel f1
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> gather0: array<f32>;
@group(0) @binding(5) var<storage, read> count_in: array<u32>;
@group(0) @binding(6) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
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
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
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
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
@compute @workgroup_size(64)
fn kernel_c4f1b91418c9180de0a478789da785bd(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].x != 0u) { n = min(n, count_in[0] * uniforms[0].z / uniforms[0].w); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r4: f32 = bitcast<f32>(0x40000000u);
  let r8: f32 = bitcast<f32>(0x3f800000u);
  let r14: f32 = bitcast<f32>(0x0u);
  let r16: u32 = 1u;
  let r17: u32 = 0u;
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r2: vec2<u32> = i64_from_f(r0);
  let r3: f32 = i64_to_f(r2);
  let r5: f32 = i64_to_f(i64_mul(r2, vec2<u32>(2u, 0u)));
  let r6: vec2<u32> = i64_mul(r2, vec2<u32>(2u, 0u));
  let r7: f32 = i64_to_f(r6);
  let r9: f32 = i64_to_f(i64_add(r6, vec2<u32>(1u, 0u)));
  let r10: vec2<u32> = i64_add(r6, vec2<u32>(1u, 0u));
  let r11: f32 = gather0[u32(clamp(i32(i64_to_f(r10)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 0u];
  let r15: f32 = select(0.0, 1.0, (r11 >= r14));
  let r18: u32 = select(r17, r16, r15 != 0.0);
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 1u + 0u] = i64_to_f(vec2<u32>(r18, 0u));
}

// kernel f2
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> gather0: array<f32>;
@group(0) @binding(5) var<storage, read> gather1: array<f32>;
@group(0) @binding(6) var<storage, read> count_in: array<u32>;
@group(0) @binding(7) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
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
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
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
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
@compute @workgroup_size(64)
fn kernel_319c1d7ee2932e2e8462769bf18ee1f7(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].y != 0u) { n = min(n, count_in[0] * uniforms[0].w / uniforms[1].x); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r4: u32 = 2u;
  let r9: f32 = bitcast<f32>(0x40000000u);
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r3: vec2<u32> = i64_from_f(r0);
  let r5: vec2<u32> = i64_divmod(r3, vec2<u32>(r4, 0u), false);
  if (all(vec2<u32>(r4, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r6: f32 = gather1[u32(clamp(i32(i64_to_f(r5)), 0, max(i32(uniforms[0].z) - 1, 0))) * 1u + 0u];
  let r7: vec2<u32> = i64_from_f(r6);
  let r8: f32 = i64_to_f(r7);
  let r10: f32 = i64_to_f(i64_mul(r7, vec2<u32>(2u, 0u)));
  let r11: vec2<u32> = i64_mul(r7, vec2<u32>(2u, 0u));
  let r12: f32 = select(((r0 % r9) + r9) % r9, 0.0, r9 == 0.0);
  let r13: f32 = i64_to_f(r11);
  let r14: f32 = (r13 + r12);
  let r15: vec2<u32> = i64_from_f(r14);
  let r16: f32 = gather0[u32(clamp(i32(i64_to_f(r15)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 0u];
  let r17: f32 = gather0[u32(clamp(i32(i64_to_f(r15)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 1u];
  let r18: f32 = gather0[u32(clamp(i32(i64_to_f(r15)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 2u];
  if (!wfinite(r16)) { sb |= 1u; }
  if (!wfinite(r17)) { sb |= 1u; }
  if (!wfinite(r18)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r16;
  outbuf[i * 3u + 1u] = r17;
  outbuf[i * 3u + 2u] = r18;
}

// kernel f3
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> gather0: array<f32>;
@group(0) @binding(5) var<storage, read> count_in: array<u32>;
@group(0) @binding(6) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
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
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
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
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
@compute @workgroup_size(64)
fn kernel_f6017fdb1e8369072dba8dfa1ebdbe68(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].x != 0u) { n = min(n, count_in[0] * uniforms[0].z / uniforms[0].w); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r4: f32 = bitcast<f32>(0x40000000u);
  let r10: f32 = bitcast<f32>(0x0u);
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r2: vec2<u32> = i64_from_f(r0);
  let r3: f32 = i64_to_f(r2);
  let r5: f32 = i64_to_f(i64_mul(r2, vec2<u32>(2u, 0u)));
  let r6: vec2<u32> = i64_mul(r2, vec2<u32>(2u, 0u));
  let r7: f32 = gather0[u32(clamp(i32(i64_to_f(r6)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 0u];
  let r8: f32 = gather0[u32(clamp(i32(i64_to_f(r6)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 1u];
  if (!wfinite(r7)) { sb |= 1u; }
  if (!wfinite(r8)) { sb |= 1u; }
  if (!wfinite(r10)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r7;
  outbuf[i * 3u + 1u] = r8;
  outbuf[i * 3u + 2u] = r10;
}

// kernel f4
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> gather0: array<f32>;
@group(0) @binding(5) var<storage, read> count_in: array<u32>;
@group(0) @binding(6) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
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
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
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
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
@compute @workgroup_size(64)
fn kernel_515a8739d0b15d52852beec9b941e519(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].x != 0u) { n = min(n, count_in[0] * uniforms[0].z / uniforms[0].w); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r4: f32 = bitcast<f32>(0x40000000u);
  let r8: f32 = bitcast<f32>(0x3f800000u);
  let r18: f32 = bitcast<f32>(0x0u);
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r2: vec2<u32> = i64_from_f(r0);
  let r3: f32 = i64_to_f(r2);
  let r5: f32 = i64_to_f(i64_mul(r2, vec2<u32>(2u, 0u)));
  let r6: vec2<u32> = i64_mul(r2, vec2<u32>(2u, 0u));
  let r7: f32 = i64_to_f(r6);
  let r9: f32 = i64_to_f(i64_add(r6, vec2<u32>(1u, 0u)));
  let r10: vec2<u32> = i64_add(r6, vec2<u32>(1u, 0u));
  let r11: f32 = gather0[u32(clamp(i32(i64_to_f(r10)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 0u];
  let r12: f32 = gather0[u32(clamp(i32(i64_to_f(r10)), 0, max(i32(uniforms[0].y) - 1, 0))) * 3u + 1u];
  let r14: f32 = (r11 - r8);
  let r15: f32 = max(r8, r14);
  let r16: f32 = (r12 - r8);
  let r17: f32 = max(r8, r16);
  if (!wfinite(r15)) { sb |= 1u; }
  if (!wfinite(r17)) { sb |= 1u; }
  if (!wfinite(r18)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r15;
  outbuf[i * 3u + 1u] = r17;
  outbuf[i * 3u + 2u] = r18;
}

// kernel f5
@group(0) @binding(0) var<storage, read> input0: array<f32>;
@group(0) @binding(1) var<storage, read_write> outbuf: array<f32>;
@group(0) @binding(2) var<uniform> uniforms: array<vec4<u32>, 2>;
@group(0) @binding(3) var<storage, read_write> status: atomic<u32>;
@group(0) @binding(4) var<storage, read> gather0: array<f32>;
@group(0) @binding(5) var<storage, read> count_in: array<u32>;
@group(0) @binding(6) var<storage, read_write> count_out: array<u32>;
fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
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
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
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
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
@compute @workgroup_size(64)
fn kernel_352bc1b2544bbd13e437571039ec14d0(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  var n = uniforms[0].x;
  if (uniforms[1].y != 0u) { n = min(n, count_in[0] * uniforms[0].w / uniforms[1].x); }
  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }
  if (i >= n) { return; }
  var sb: u32 = 0u;
  let r4: f32 = bitcast<f32>(0x40000000u);
  let r12: u32 = 73856093u;
  let r15: u32 = 19349663u;
  let r18: vec2<u32> = vec2<u32>(4294967295u, 1073741823u);
  let r21: f32 = bitcast<f32>(0x3f800000u);
  let r24: u32 = 1103515245u;
  let r27: f32 = bitcast<f32>(0x4640e400u);
  let r31: u32 = 8u;
  let r34: f32 = bitcast<f32>(0x48afc800u);
  let r38: f32 = bitcast<f32>(0x447a0000u);
  let r40: u32 = 360000u;
  let r43: f32 = bitcast<f32>(0x40a00000u);
  let r47: f32 = bitcast<f32>(0x0u);
  var r49: f32 = 0.0;
  let r50: f32 = bitcast<f32>(0x41500000u);
  var r51: f32 = 0.0;
  var r52: vec2<u32> = vec2<u32>(0u, 0u);
  var r53: f32 = 0.0;
  let r54: f32 = bitcast<f32>(0x42380000u);
  var r55: f32 = 0.0;
  var r56: vec2<u32> = vec2<u32>(0u, 0u);
  var r57: f32 = 0.0;
  let r58: f32 = bitcast<f32>(0x3f800000u);
  var r59: f32 = 0.0;
  var r60: f32 = 0.0;
  let r61: f32 = bitcast<f32>(0x40800000u);
  var r62: f32 = 0.0;
  var r63: f32 = 0.0;
  var r64: f32 = 0.0;
  let r65: f32 = bitcast<f32>(0x41000000u);
  var r66: f32 = 0.0;
  var r67: vec2<u32> = vec2<u32>(0u, 0u);
  var r68: f32 = 0.0;
  let r69: f32 = bitcast<f32>(0x41400000u);
  var r70: f32 = 0.0;
  var r71: vec2<u32> = vec2<u32>(0u, 0u);
  var r72: f32 = 0.0;
  let r73: f32 = bitcast<f32>(0x3f800000u);
  var r74: f32 = 0.0;
  var r75: f32 = 0.0;
  let r76: f32 = bitcast<f32>(0x40800000u);
  var r77: f32 = 0.0;
  var r78: vec2<u32> = vec2<u32>(0u, 0u);
  var r79: f32 = 0.0;
  let r80: f32 = bitcast<f32>(0x41000000u);
  var r81: f32 = 0.0;
  var r82: vec2<u32> = vec2<u32>(0u, 0u);
  var r83: f32 = 0.0;
  let r84: f32 = bitcast<f32>(0x41400000u);
  var r85: f32 = 0.0;
  var r86: vec2<u32> = vec2<u32>(0u, 0u);
  let r87: f32 = bitcast<f32>(0x43340000u);
  var r88: f32 = 0.0;
  var r89: f32 = 0.0;
  var r90: vec2<u32> = vec2<u32>(0u, 0u);
  var r91: f32 = 0.0;
  let r92: f32 = bitcast<f32>(0x40000000u);
  var r93: f32 = 0.0;
  var r94: f32 = 0.0;
  let r95: f32 = bitcast<f32>(0x40400000u);
  var r96: f32 = 0.0;
  var r97: f32 = 0.0;
  var r98: f32 = 0.0;
  let r99: f32 = bitcast<f32>(0x40e00000u);
  var r100: f32 = 0.0;
  var r101: vec2<u32> = vec2<u32>(0u, 0u);
  var r102: f32 = 0.0;
  var r103: f32 = 0.0;
  var r104: vec2<u32> = vec2<u32>(0u, 0u);
  var r105: f32 = 0.0;
  let r106: f32 = bitcast<f32>(0x40000000u);
  var r107: f32 = 0.0;
  var r108: f32 = 0.0;
  let r109: f32 = bitcast<f32>(0x40c00000u);
  var r110: f32 = 0.0;
  var r111: f32 = 0.0;
  var r112: f32 = 0.0;
  let r113: f32 = bitcast<f32>(0x40400000u);
  var r114: f32 = 0.0;
  var r115: vec2<u32> = vec2<u32>(0u, 0u);
  var r116: f32 = 0.0;
  let r117: f32 = bitcast<f32>(0x40e00000u);
  var r118: f32 = 0.0;
  var r119: vec2<u32> = vec2<u32>(0u, 0u);
  var r120: f32 = 0.0;
  var r121: f32 = 0.0;
  var r122: vec2<u32> = vec2<u32>(0u, 0u);
  let r123: f32 = bitcast<f32>(0x43160000u);
  var r124: f32 = 0.0;
  var r125: f32 = 0.0;
  var r126: vec2<u32> = vec2<u32>(0u, 0u);
  var r127: f32 = 0.0;
  let r128: f32 = bitcast<f32>(0x40000000u);
  var r129: f32 = 0.0;
  var r130: f32 = 0.0;
  let r131: f32 = bitcast<f32>(0x40c00000u);
  var r132: f32 = 0.0;
  var r133: vec2<u32> = vec2<u32>(0u, 0u);
  var r134: f32 = 0.0;
  let r135: f32 = bitcast<f32>(0x40e00000u);
  var r136: f32 = 0.0;
  var r137: vec2<u32> = vec2<u32>(0u, 0u);
  var r138: f32 = 0.0;
  let r139: f32 = bitcast<f32>(0x40800000u);
  var r140: f32 = 0.0;
  var r141: vec2<u32> = vec2<u32>(0u, 0u);
  let r142: f32 = bitcast<f32>(0x43520000u);
  var r143: f32 = 0.0;
  var r144: f32 = 0.0;
  var r145: vec2<u32> = vec2<u32>(0u, 0u);
  var r146: f32 = 0.0;
  let r147: f32 = bitcast<f32>(0x40400000u);
  var r148: f32 = 0.0;
  var r149: f32 = 0.0;
  let r150: f32 = bitcast<f32>(0x40400000u);
  var r151: f32 = 0.0;
  var r152: vec2<u32> = vec2<u32>(0u, 0u);
  var r153: f32 = 0.0;
  let r154: f32 = bitcast<f32>(0x42f00000u);
  var r155: f32 = 0.0;
  var r156: vec2<u32> = vec2<u32>(0u, 0u);
  let r157: u32 = 3u;
  var r158: vec2<u32> = vec2<u32>(0u, 0u);
  var r159: f32 = 0.0;
  let r160: f32 = bitcast<f32>(0x40e00000u);
  var r161: f32 = 0.0;
  var r162: vec2<u32> = vec2<u32>(0u, 0u);
  var r163: f32 = 0.0;
  var r164: f32 = 0.0;
  var r165: f32 = 0.0;
  var r166: vec2<u32> = vec2<u32>(0u, 0u);
  var r167: f32 = 0.0;
  let r168: f32 = bitcast<f32>(0x40800000u);
  var r169: f32 = 0.0;
  var r170: vec2<u32> = vec2<u32>(0u, 0u);
  var r171: f32 = 0.0;
  let r172: f32 = bitcast<f32>(0x42b40000u);
  var r173: f32 = 0.0;
  var r174: vec2<u32> = vec2<u32>(0u, 0u);
  let r175: u32 = 4u;
  var r176: vec2<u32> = vec2<u32>(0u, 0u);
  var r177: f32 = 0.0;
  let r178: f32 = bitcast<f32>(0x41100000u);
  var r179: f32 = 0.0;
  var r180: vec2<u32> = vec2<u32>(0u, 0u);
  var r181: f32 = 0.0;
  var r182: f32 = 0.0;
  var r183: f32 = 0.0;
  var r184: vec2<u32> = vec2<u32>(0u, 0u);
  var r185: vec2<u32> = vec2<u32>(0u, 0u);
  var r186: vec2<u32> = vec2<u32>(0u, 0u);
  var r187: vec2<u32> = vec2<u32>(0u, 0u);
  var r188: vec2<u32> = vec2<u32>(0u, 0u);
  var r189: vec2<u32> = vec2<u32>(0u, 0u);
  var r190: vec2<u32> = vec2<u32>(0u, 0u);
  let r192: u32 = 97u;
  let r195: f32 = bitcast<f32>(0x44610000u);
  let r199: f32 = bitcast<f32>(0x42c80000u);
  let r201: f32 = bitcast<f32>(0x40900000u);
  let r203: f32 = bitcast<f32>(0x3f0f5c29u);
  let r204: u32 = 31u;
  let r207: f32 = bitcast<f32>(0x423c0000u);
  let r215: f32 = bitcast<f32>(0x43820000u);
  let r221: f32 = bitcast<f32>(0x3eae147bu);
  let r222: u32 = 73u;
  let r224: f32 = bitcast<f32>(0x42a60000u);
  let r232: f32 = bitcast<f32>(0x43a50000u);
  let r241: f32 = bitcast<f32>(0x43b40000u);
  let r243: f32 = bitcast<f32>(0x42700000u);
  let r263: f32 = bitcast<f32>(0x40400000u);
  let r265: f32 = bitcast<f32>(0x40800000u);
  let r276: f32 = bitcast<f32>(0x437f0000u);
  let r278: f32 = bitcast<f32>(0x3f000000u);
  let r0: f32 = input0[i * 1u + 0u];
  if (!wfinite(r0)) { sb |= 1u; }
  let r2: vec2<u32> = i64_from_f(r0);
  let r3: f32 = i64_to_f(r2);
  let r5: f32 = i64_to_f(i64_mul(r2, vec2<u32>(2u, 0u)));
  let r6: vec2<u32> = i64_mul(r2, vec2<u32>(2u, 0u));
  let r9: f32 = gather0[u32(clamp(i32(i64_to_f(r6)), 0, max(i32(uniforms[0].z) - 1, 0))) * 3u + 2u];
  let r10: vec2<u32> = i64_from_f(bitcast<f32>(uniforms[0].y));
  let r11: vec2<u32> = i64_from_f(r9);
  let r13: vec2<u32> = i64_mul(r11, vec2<u32>(r12, 0u));
    let r14: vec2<u32> = i64_mul(r11, r11);
    let r16: vec2<u32> = i64_mul(r14, vec2<u32>(r15, 0u));
    let r17: vec2<u32> = (r13 ^ r16);
    let r19: vec2<u32> = (r17 & r18);
    let r20: f32 = i64_to_f(r10);
  let r22: f32 = i64_to_f(i64_add(r10, vec2<u32>(1u, 0u)));
  let r23: vec2<u32> = i64_add(r10, vec2<u32>(1u, 0u));
  let r25: vec2<u32> = i64_mul(r23, vec2<u32>(r24, 0u));
    let r26: f32 = i64_to_f(r25);
  let r28: f32 = i64_to_f(i64_add(r25, vec2<u32>(12345u, 0u)));
  let r29: vec2<u32> = i64_add(r25, vec2<u32>(12345u, 0u));
  let r30: vec2<u32> = (r29 & r18);
    let r32: vec2<u32> = i64_divmod(r19, vec2<u32>(r31, 0u), true);
  if (all(vec2<u32>(r31, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r33: f32 = i64_to_f(r30);
  let r35: f32 = i64_to_f(i64_fmod(r30, vec2<u32>(360000u, 0u)));
  let r36: vec2<u32> = i64_fmod(r30, vec2<u32>(360000u, 0u));
  let r37: f32 = i64_to_f(r36);
  let r39: f32 = select(r37 / r38, 0.0, r38 == 0.0);
  let r41: vec2<u32> = i64_divmod(r30, vec2<u32>(r40, 0u), false);
  if (all(vec2<u32>(r40, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r42: f32 = i64_to_f(r41);
  let r44: f32 = i64_to_f(i64_fmod(r41, vec2<u32>(5u, 0u)));
  let r45: vec2<u32> = i64_fmod(r41, vec2<u32>(5u, 0u));
  let r46: f32 = i64_to_f(r45);
  let r48: f32 = select(0.0, 1.0, (r46 == r47));
  if (r48 != 0.0) {
    r49 = i64_to_f(r32);
    r51 = i64_to_f(i64_mul(r32, vec2<u32>(13u, 0u)));
    r52 = i64_mul(r32, vec2<u32>(13u, 0u));
    r53 = i64_to_f(r52);
    r55 = i64_to_f(i64_sub(r52, vec2<u32>(46u, 0u)));
    r56 = i64_sub(r52, vec2<u32>(46u, 0u));
  }
  if (r48 == 0.0) {
    r57 = i64_to_f(r45);
    r59 = select(0.0, 1.0, (r57 == r58));
    r60 = i64_to_f(r32);
    r62 = select(0.0, 1.0, (r60 < r61));
    r63 = select(0.0, 1.0, (r59 != 0.0 && r62 != 0.0));
    if (r63 != 0.0) {
      r64 = i64_to_f(r32);
      r66 = i64_to_f(i64_mul(r32, vec2<u32>(8u, 0u)));
      r67 = i64_mul(r32, vec2<u32>(8u, 0u));
      r68 = i64_to_f(r67);
      r70 = i64_to_f(i64_sub(r67, vec2<u32>(12u, 0u)));
      r71 = i64_sub(r67, vec2<u32>(12u, 0u));
    }
    if (r63 == 0.0) {
      r72 = i64_to_f(r45);
      r74 = select(0.0, 1.0, (r72 == r73));
      if (r74 != 0.0) {
        r75 = i64_to_f(r32);
        r77 = i64_to_f(i64_sub(r32, vec2<u32>(4u, 0u)));
        r78 = i64_sub(r32, vec2<u32>(4u, 0u));
        r79 = i64_to_f(r78);
        r81 = i64_to_f(i64_mul(r78, vec2<u32>(8u, 0u)));
        r82 = i64_mul(r78, vec2<u32>(8u, 0u));
        r83 = i64_to_f(r82);
        r85 = i64_to_f(i64_sub(r82, vec2<u32>(12u, 0u)));
        r86 = i64_sub(r82, vec2<u32>(12u, 0u));
        r88 = i64_to_f(r86);
        r89 = i64_to_f(i64_add(vec2<u32>(180u, 0u), r86));
        r90 = i64_add(vec2<u32>(180u, 0u), r86);
      }
      if (r74 == 0.0) {
        r91 = i64_to_f(r45);
        r93 = select(0.0, 1.0, (r91 == r92));
        r94 = i64_to_f(r32);
        r96 = select(0.0, 1.0, (r94 < r95));
        r97 = select(0.0, 1.0, (r93 != 0.0 && r96 != 0.0));
        if (r97 != 0.0) {
          r98 = i64_to_f(r32);
          r100 = i64_to_f(i64_mul(r32, vec2<u32>(7u, 0u)));
          r101 = i64_mul(r32, vec2<u32>(7u, 0u));
          r102 = i64_to_f(r101);
          r103 = i64_to_f(i64_sub(r101, vec2<u32>(7u, 0u)));
          r104 = i64_sub(r101, vec2<u32>(7u, 0u));
        }
        if (r97 == 0.0) {
          r105 = i64_to_f(r45);
          r107 = select(0.0, 1.0, (r105 == r106));
          r108 = i64_to_f(r32);
          r110 = select(0.0, 1.0, (r108 < r109));
          r111 = select(0.0, 1.0, (r107 != 0.0 && r110 != 0.0));
          if (r111 != 0.0) {
            r112 = i64_to_f(r32);
            r114 = i64_to_f(i64_sub(r32, vec2<u32>(3u, 0u)));
            r115 = i64_sub(r32, vec2<u32>(3u, 0u));
            r116 = i64_to_f(r115);
            r118 = i64_to_f(i64_mul(r115, vec2<u32>(7u, 0u)));
            r119 = i64_mul(r115, vec2<u32>(7u, 0u));
            r120 = i64_to_f(r119);
            r121 = i64_to_f(i64_sub(r119, vec2<u32>(7u, 0u)));
            r122 = i64_sub(r119, vec2<u32>(7u, 0u));
            r124 = i64_to_f(r122);
            r125 = i64_to_f(i64_add(vec2<u32>(150u, 0u), r122));
            r126 = i64_add(vec2<u32>(150u, 0u), r122);
          }
          if (r111 == 0.0) {
            r127 = i64_to_f(r45);
            r129 = select(0.0, 1.0, (r127 == r128));
            if (r129 != 0.0) {
              r130 = i64_to_f(r32);
              r132 = i64_to_f(i64_sub(r32, vec2<u32>(6u, 0u)));
              r133 = i64_sub(r32, vec2<u32>(6u, 0u));
              r134 = i64_to_f(r133);
              r136 = i64_to_f(i64_mul(r133, vec2<u32>(7u, 0u)));
              r137 = i64_mul(r133, vec2<u32>(7u, 0u));
              r138 = i64_to_f(r137);
              r140 = i64_to_f(i64_sub(r137, vec2<u32>(4u, 0u)));
              r141 = i64_sub(r137, vec2<u32>(4u, 0u));
              r143 = i64_to_f(r141);
              r144 = i64_to_f(i64_add(vec2<u32>(210u, 0u), r141));
              r145 = i64_add(vec2<u32>(210u, 0u), r141);
            }
            if (r129 == 0.0) {
              r146 = i64_to_f(r45);
              r148 = select(0.0, 1.0, (r146 == r147));
              if (r148 != 0.0) {
                r149 = i64_to_f(r32);
                r151 = i64_to_f(i64_fmod(r32, vec2<u32>(3u, 0u)));
                r152 = i64_fmod(r32, vec2<u32>(3u, 0u));
                r153 = i64_to_f(r152);
                r155 = i64_to_f(i64_mul(r152, vec2<u32>(120u, 0u)));
                r156 = i64_mul(r152, vec2<u32>(120u, 0u));
                r158 = i64_divmod(r32, vec2<u32>(r157, 0u), false);
                if (all(vec2<u32>(r157, 0u) == vec2<u32>(0u))) { sb |= 2u; }
                r159 = i64_to_f(r158);
                r161 = i64_to_f(i64_mul(r158, vec2<u32>(7u, 0u)));
                r162 = i64_mul(r158, vec2<u32>(7u, 0u));
                r163 = i64_to_f(r156);
                r164 = i64_to_f(r162);
                r165 = i64_to_f(i64_add(r156, r162));
                r166 = i64_add(r156, r162);
              }
              if (r148 == 0.0) {
                r167 = i64_to_f(r32);
                r169 = i64_to_f(i64_fmod(r32, vec2<u32>(4u, 0u)));
                r170 = i64_fmod(r32, vec2<u32>(4u, 0u));
                r171 = i64_to_f(r170);
                r173 = i64_to_f(i64_mul(r170, vec2<u32>(90u, 0u)));
                r174 = i64_mul(r170, vec2<u32>(90u, 0u));
                r176 = i64_divmod(r32, vec2<u32>(r175, 0u), false);
                if (all(vec2<u32>(r175, 0u) == vec2<u32>(0u))) { sb |= 2u; }
                r177 = i64_to_f(r176);
                r179 = i64_to_f(i64_mul(r176, vec2<u32>(9u, 0u)));
                r180 = i64_mul(r176, vec2<u32>(9u, 0u));
                r181 = i64_to_f(r174);
                r182 = i64_to_f(r180);
                r183 = i64_to_f(i64_add(r174, r180));
                r184 = i64_add(r174, r180);
              }
              r185 = select(r184, r166, r148 != 0.0);
            }
            r186 = select(r185, r145, r129 != 0.0);
          }
          r187 = select(r186, r126, r111 != 0.0);
        }
        r188 = select(r187, r104, r97 != 0.0);
      }
      r189 = select(r188, r90, r74 != 0.0);
    }
    r190 = select(r189, r71, r63 != 0.0);
  }
  let r191: vec2<u32> = select(r190, r56, r48 != 0.0);
  let r193: vec2<u32> = i64_divmod(r19, vec2<u32>(r192, 0u), false);
  if (all(vec2<u32>(r192, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r194: f32 = i64_to_f(r193);
  let r196: f32 = i64_to_f(i64_fmod(r193, vec2<u32>(900u, 0u)));
  let r197: vec2<u32> = i64_fmod(r193, vec2<u32>(900u, 0u));
  let r198: f32 = i64_to_f(r197);
  let r200: f32 = select(r198 / r199, 0.0, r199 == 0.0);
  let r202: f32 = (r200 - r201);
  let r205: vec2<u32> = i64_divmod(r30, vec2<u32>(r204, 0u), false);
  if (all(vec2<u32>(r204, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r206: f32 = i64_to_f(r32);
  let r208: f32 = i64_to_f(i64_mul(r32, vec2<u32>(47u, 0u)));
  let r209: vec2<u32> = i64_mul(r32, vec2<u32>(47u, 0u));
  let r210: f32 = i64_to_f(r205);
  let r211: f32 = i64_to_f(r209);
  let r212: f32 = i64_to_f(i64_add(r205, r209));
  let r213: vec2<u32> = i64_add(r205, r209);
  let r214: f32 = i64_to_f(r213);
  let r216: f32 = i64_to_f(i64_fmod(r213, vec2<u32>(260u, 0u)));
  let r217: vec2<u32> = i64_fmod(r213, vec2<u32>(260u, 0u));
  let r218: f32 = i64_to_f(r217);
  let r219: f32 = select(r218 / r38, 0.0, r38 == 0.0);
  let r220: f32 = (r203 + r219);
  let r223: vec2<u32> = i64_divmod(r30, vec2<u32>(r222, 0u), false);
  if (all(vec2<u32>(r222, 0u) == vec2<u32>(0u))) { sb |= 2u; }
  let r225: f32 = i64_to_f(i64_mul(r32, vec2<u32>(83u, 0u)));
  let r226: vec2<u32> = i64_mul(r32, vec2<u32>(83u, 0u));
  let r227: f32 = i64_to_f(r223);
  let r228: f32 = i64_to_f(r226);
  let r229: f32 = i64_to_f(i64_add(r223, r226));
  let r230: vec2<u32> = i64_add(r223, r226);
  let r231: f32 = i64_to_f(r230);
  let r233: f32 = i64_to_f(i64_fmod(r230, vec2<u32>(330u, 0u)));
  let r234: vec2<u32> = i64_fmod(r230, vec2<u32>(330u, 0u));
  let r235: f32 = i64_to_f(r234);
  let r236: f32 = select(r235 / r38, 0.0, r38 == 0.0);
  let r237: f32 = (r221 + r236);
  let r238: f32 = i64_to_f(r191);
  let r239: f32 = (r39 + r238);
  let r240: f32 = (r239 + r202);
  let r242: f32 = select(((r240 % r241) + r241) % r241, 0.0, r241 == 0.0);
  let r244: f32 = select(r242 / r243, 0.0, r243 == 0.0);
  let r245: f32 = min(r21, r220);
  let r246: f32 = max(r47, r245);
  let r247: f32 = min(r21, r237);
  let r248: f32 = max(r47, r247);
  let r249: f32 = (r4 * r248);
  let r250: f32 = (r249 - r21);
  let r251: f32 = abs(r250);
  let r252: f32 = (r21 - r251);
  let r253: f32 = (r252 * r246);
  let r254: f32 = select(((r244 % r4) + r4) % r4, 0.0, r4 == 0.0);
  let r255: f32 = (r254 - r21);
  let r256: f32 = abs(r255);
  let r257: f32 = (r21 - r256);
  let r258: f32 = (r253 * r257);
  let r259: f32 = select(r253 / r4, 0.0, r4 == 0.0);
  let r260: f32 = (r248 - r259);
  let r261: f32 = select(0.0, 1.0, (r244 < r21));
  let r262: f32 = select(0.0, 1.0, (r244 < r4));
  let r264: f32 = select(0.0, 1.0, (r244 < r263));
  let r266: f32 = select(0.0, 1.0, (r244 < r265));
  let r267: f32 = select(0.0, 1.0, (r244 < r43));
  let r268: f32 = select(r253, r258, r267 != 0.0);
  let r269: f32 = select(r268, r47, r266 != 0.0);
  let r270: f32 = select(r269, r47, r264 != 0.0);
  let r271: f32 = select(r270, r258, r262 != 0.0);
  let r272: f32 = select(r271, r253, r261 != 0.0);
  let r273: f32 = (r272 + r260);
  let r274: f32 = min(r21, r273);
  let r275: f32 = max(r47, r274);
  let r277: f32 = (r275 * r276);
  let r279: f32 = (r277 + r278);
  let r280: vec2<u32> = i64_from_f(r279);
  let r281: f32 = i64_to_f(r280);
  let r282: f32 = select(r281 / r276, 0.0, r276 == 0.0);
  let r283: f32 = select(r47, r47, r267 != 0.0);
  let r284: f32 = select(r283, r258, r266 != 0.0);
  let r285: f32 = select(r284, r253, r264 != 0.0);
  let r286: f32 = select(r285, r253, r262 != 0.0);
  let r287: f32 = select(r286, r258, r261 != 0.0);
  let r288: f32 = (r287 + r260);
  let r289: f32 = min(r21, r288);
  let r290: f32 = max(r47, r289);
  let r291: f32 = (r290 * r276);
  let r292: f32 = (r291 + r278);
  let r293: vec2<u32> = i64_from_f(r292);
  let r294: f32 = i64_to_f(r293);
  let r295: f32 = select(r294 / r276, 0.0, r276 == 0.0);
  let r296: f32 = select(r258, r253, r267 != 0.0);
  let r297: f32 = select(r296, r253, r266 != 0.0);
  let r298: f32 = select(r297, r258, r264 != 0.0);
  let r299: f32 = select(r298, r47, r262 != 0.0);
  let r300: f32 = select(r299, r47, r261 != 0.0);
  let r301: f32 = (r300 + r260);
  let r302: f32 = min(r21, r301);
  let r303: f32 = max(r47, r302);
  let r304: f32 = (r303 * r276);
  let r305: f32 = (r304 + r278);
  let r306: vec2<u32> = i64_from_f(r305);
  let r307: f32 = i64_to_f(r306);
  let r308: f32 = select(r307 / r276, 0.0, r276 == 0.0);
  if (!wfinite(r282)) { sb |= 1u; }
  if (!wfinite(r295)) { sb |= 1u; }
  if (!wfinite(r308)) { sb |= 1u; }
  if (sb != 0u) { atomicOr(&status, sb); }
  outbuf[i * 3u + 0u] = r282;
  outbuf[i * 3u + 1u] = r295;
  outbuf[i * 3u + 2u] = r308;
}
