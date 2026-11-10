#include <metal_stdlib>
using namespace metal;
// pxui_render, Metal. Line-for-line parallel with ui.vert + ui.frag; layout in src/lib.rs.
// Output is straight (non-premultiplied) alpha.
struct Inst { float4 rect; uint4 a; uint4 b; uint4 c; };
struct VOut {
  float4 position [[position]];
  float4 v_pos;
  float4 v_rect [[flat]];
  float4 v_radii [[flat]];
  float4 v_clip [[flat]];
  float4 v_vclip [[flat]];
  float4 v_color [[flat]];
  float4 v_color2 [[flat]];
  float4 v_p0 [[flat]];
  float4 v_p2 [[flat]];
  float4 v_misc [[flat]];
  uint2 v_kf [[flat]];
  float2 v_tp;
};

constant uint RECT = 0u, MASK = 1u, IMAGE = 2u, SHADOW = 3u, LINE = 4u, ARC = 5u, CURVE = 6u;
constant uint INNER = 4u, ROUND = 8u;
constant float TAU = 6.2831853;
constant float2 CORNERS[6] = {float2(0, 0), float2(1, 0), float2(0, 1), float2(0, 1), float2(1, 0), float2(1, 1)};

float2 i16x2(uint w) { return float2(float(int(w << 16) >> 16), float(int(w) >> 16)); }
float2 half2x16(uint w) { return float2(as_type<half2>(w)); }

vertex VOut ui_vertex(uint vid [[vertex_id]], uint iid [[instance_id]],
                      device const Inst* inst [[buffer(0)]], device const float4* g [[buffer(1)]]) {
  VOut o;
  Inst I = inst[iid];
  float2 c = CORNERS[vid];
  uint kind = (I.c.x >> 16) & 255u;
  uint flags = I.c.x >> 24;
  uint view = I.c.y & 65535u;
  float4 va = g[1u + 4u * view];
  float4 vclip = g[2u + 4u * view];
  float4 vm = g[3u + 4u * view];
  float2 piv = g[4u + 4u * view].xy;
  float2 off = va.xy;
  // quad padding in local units: 1 target pixel of anti-aliasing margin even when the view shrinks
  float pad = max(1.0, 1.0 / max(va.w * sqrt(abs(vm.x * vm.w - vm.y * vm.z)), 1e-3));
  float width = half2x16(I.c.x).x;
  float4 r = I.rect + (kind == ARC ? float4(off, 0.0, 0.0) : off.xyxy);
  float4 p0 = (kind == ARC || kind == CURVE) ? float4(as_type<float2>(I.c.zw), 0.0, 0.0) : float4(half2x16(I.c.z), half2x16(I.c.w));
  if (kind == CURVE) p0.xy += off;
  float4 p2 = kind == RECT ? unpack_unorm4x8_to_float(I.b.x) : float4(half2x16(I.b.x), half2x16(I.b.y));
  float h = width * 0.5 + pad;
  float2 p;
  if (kind == LINE) {
    float2 d = r.zw - r.xy;
    float len = length(d);
    float2 dir = len > 1e-6 ? d / len : float2(1.0, 0.0);
    float2 n = float2(-dir.y, dir.x);
    p = mix(r.xy - dir * h, r.zw + dir * h, c.x) + n * ((c.y * 2.0 - 1.0) * h);
  } else {
    float4 box = r;
    if (kind == ARC) box = float4(r.xy - (r.z + h), r.xy + (r.z + h));
    if (kind == CURVE) box = float4(min(min(r.xy, r.zw), p0.xy) - h, max(max(r.xy, r.zw), p0.xy) + h);
    if (kind == RECT || kind == SHADOW) box = r + float4(-pad, -pad, pad, pad);
    if (kind == SHADOW && (flags & INNER) == 0u) box = r + p0.xyxy + float4(-1.0, -1.0, 1.0, 1.0) * (p0.w + 3.0 * p0.z + pad);
    p = mix(box.xy, box.zw, c);
  }
  float2 uv = mix(unpack_unorm2x16_to_float(I.b.x), unpack_unorm2x16_to_float(I.b.y), c);
  float2 t = p - piv;
  t = piv + va.w * float2(vm.x * t.x + vm.y * t.y, vm.z * t.x + vm.w * t.y);
  o.position = float4(t.x / g[0].x * 2.0 - 1.0, 1.0 - t.y / g[0].y * 2.0, 0.0, 1.0);
  o.v_tp = t;
  o.v_pos = float4(p, uv);
  o.v_rect = r;
  o.v_radii = float4(half2x16(I.b.z), half2x16(I.b.w));
  o.v_clip = float4(i16x2(I.a.x), i16x2(I.a.y)) + off.xyxy;
  o.v_vclip = vclip;
  o.v_color = unpack_unorm4x8_to_float(I.a.z);
  o.v_color2 = unpack_unorm4x8_to_float(I.a.w);
  o.v_p0 = p0;
  o.v_p2 = p2;
  o.v_misc = float4(width, half2x16(I.c.y).y, va.z, 0.0);
  o.v_kf = uint2(kind, flags);
  return o;
}

float wrap(float x, float y) { return x - y * floor(x / y); }
float cover(float d, float px) { return clamp(0.5 - d / px, 0.0, 1.0); }
// radii are top-left, top-right, bottom-right, bottom-left (y down)
float sd_rect(float2 p, float4 r, float4 radii) {
  float2 half_ = (r.zw - r.xy) * 0.5;
  float2 q = p - (r.xy + r.zw) * 0.5;
  float rad = q.x < 0.0 ? (q.y < 0.0 ? radii.x : radii.w) : (q.y < 0.0 ? radii.y : radii.z);
  rad = min(rad, min(half_.x, half_.y));
  float2 d = abs(q) - half_ + rad;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0) - rad;
}
float erf_(float x) {
  float s = sign(x), a = abs(x);
  float t = 1.0 + (0.278393 + (0.230389 + 0.078108 * (a * a)) * a) * a;
  t *= t;
  return s - s / (t * t);
}
// coverage of a shape whose signed distance is d, blurred by a gaussian of deviation sigma
float blurred(float d, float sigma) { return 0.5 - 0.5 * erf_(d / (sigma * 1.4142136)); }
// straight-alpha top over bottom
float4 over(float4 top, float4 bottom) {
  float a = top.a + bottom.a * (1.0 - top.a);
  if (a <= 0.0) return float4(0.0);
  return float4((top.rgb * top.a + bottom.rgb * bottom.a * (1.0 - top.a)) / a, a);
}
// premultiplied interpolation of two straight-alpha colours
float4 mix_premul(float4 a, float4 b, float t) {
  float4 m = mix(float4(a.rgb * a.a, a.a), float4(b.rgb * b.a, b.a), t);
  return m.a > 0.0 ? float4(m.rgb / m.a, m.a) : float4(0.0);
}
// signed distance to a stroke at arc length s and offset t from its centre line of length len;
// dash = (on, off, phase) in points, on = 0 is solid
float stroke(float s, float t, float len, float w, float4 dash, bool round_) {
  float lo = 0.0, hi = len;
  float period = dash.x + dash.y;
  if (dash.x > 0.0 && period > 0.0) {
    float m = wrap(s + dash.z, period);
    if (m > dash.x + dash.y * 0.5) m -= period;
    lo = max(s - m, 0.0);
    hi = min(s - m + dash.x, len);
  }
  if (hi < lo) return 1e6;
  float da = max(lo - s, s - hi);
  return round_ ? length(float2(max(da, 0.0), t)) - w * 0.5 : max(da, abs(t) - w * 0.5);
}
// distance to the quadratic bezier a-b-c (Inigo Quilez, MIT)
float sd_bezier(float2 pos, float2 A, float2 B, float2 C) {
  float2 a = B - A, b = A - 2.0 * B + C, c = a * 2.0, d = A - pos;
  if (dot(b, b) < 1e-6) {
    float2 e = C - A;
    return length(d + e * clamp(dot(-d, e) / max(dot(e, e), 1e-12), 0.0, 1.0));
  }
  float kk = 1.0 / dot(b, b);
  float kx = kk * dot(a, b);
  float ky = kk * (2.0 * dot(a, a) + dot(d, b)) / 3.0;
  float kz = kk * dot(d, a);
  float p = ky - kx * kx, p3 = p * p * p;
  float q = kx * (2.0 * kx * kx - 3.0 * ky) + kz;
  float h = q * q + 4.0 * p3;
  float res;
  if (h >= 0.0) {
    h = sqrt(h);
    float2 x = (float2(h, -h) - q) * 0.5;
    float2 uv = sign(x) * pow(abs(x), float2(1.0 / 3.0));
    float t = clamp(uv.x + uv.y - kx, 0.0, 1.0);
    float2 e = d + (c + b * t) * t;
    res = dot(e, e);
  } else {
    float z = sqrt(-p);
    float v = acos(q / (p * z * 2.0)) / 3.0;
    float m = cos(v), n = sin(v) * 1.7320508;
    float3 t = clamp(float3(m + m, -n - m, n - m) * z - kx, 0.0, 1.0);
    float2 e = d + (c + b * t.x) * t.x;
    res = dot(e, e);
    e = d + (c + b * t.y) * t.y;
    res = min(res, dot(e, e));
  }
  return sqrt(res);
}

fragment float4 ui_fragment(VOut in [[stage_in]], texture2d<float> mask_tex [[texture(2)]],
                            texture2d<float> color_tex [[texture(3)]], sampler samp [[sampler(4)]]) {
  float2 p = in.v_pos.xy;
  // points per pixel, rotation invariant (equals the old fwidth form on axis-aligned views)
  float px = max(sqrt(0.5 * (dot(dfdx(p), dfdx(p)) + dot(dfdy(p), dfdy(p)))), 1e-4);
  float pxt = max(sqrt(0.5 * (dot(dfdx(in.v_tp), dfdx(in.v_tp)) + dot(dfdy(in.v_tp), dfdy(in.v_tp)))), 1e-4);
  uint kind = in.v_kf.x, flags = in.v_kf.y;
  float clip_cov = cover(sd_rect(p, in.v_clip, float4(in.v_misc.y)), px) * cover(sd_rect(in.v_tp, in.v_vclip, float4(0.0)), pxt);
  if (clip_cov <= 0.0) discard_fragment();
  float4 col;
  if (kind == MASK) {
    col = float4(in.v_color.rgb, in.v_color.a * mask_tex.sample(samp, in.v_pos.zw, level(0.0)).r);
  } else if (kind == IMAGE) {
    col = in.v_color * color_tex.sample(samp, in.v_pos.zw, level(0.0));
    if (dot(in.v_radii, float4(1.0)) > 0.0) col.a *= cover(sd_rect(p, in.v_rect, in.v_radii), px);
  } else if (kind == RECT) {
    float d = sd_rect(p, in.v_rect, in.v_radii);
    float4 fill = in.v_color;
    uint grad = flags & 3u;
    if (grad != 0u) {
      float2 size = max(in.v_rect.zw - in.v_rect.xy, float2(1e-6));
      float2 q = (p - in.v_rect.xy) / size;
      float t;
      if (grad == 1u) {
        float2 e = in.v_p0.zw - in.v_p0.xy;
        t = dot(q - in.v_p0.xy, e) / max(dot(e, e), 1e-12);
      } else if (grad == 2u) {
        t = length((q - in.v_p0.xy) / max(in.v_p0.zw, float2(1e-6)));
      } else {
        float2 e = (q - in.v_p0.xy) * size;
        t = wrap(atan2(e.y, e.x) / TAU - in.v_p0.z, 1.0);
      }
      fill = mix_premul(in.v_color, in.v_p2, clamp(t, 0.0, 1.0));
    }
    if (in.v_misc.x > 0.0) {
      float inner = d + in.v_misc.x;
      uint sides = flags >> 4u;  // top 1, right 2, bottom 4, left 8; 0 or 15 = all
      if (sides != 0u && sides != 15u) {
        float4 s = float4(float((sides >> 3u) & 1u), float(sides & 1u), -float((sides >> 1u) & 1u), -float((sides >> 2u) & 1u));
        inner = sd_rect(p, in.v_rect + s * in.v_misc.x, max(in.v_radii - in.v_misc.x, 0.0));
      }
      fill = over(float4(in.v_color2.rgb, in.v_color2.a * (1.0 - cover(inner, px))), fill);
    }
    col = float4(fill.rgb, fill.a * cover(d, px));
  } else if (kind == SHADOW) {
    float sigma = sqrt(in.v_p0.z * in.v_p0.z + 0.25 * px * px);
    float4 shape = in.v_rect + in.v_p0.xyxy;
    if ((flags & INNER) == 0u) {
      float d = sd_rect(p, shape + float4(-in.v_p0.w, -in.v_p0.w, in.v_p0.w, in.v_p0.w), max(in.v_radii + in.v_p0.w, 0.0));
      col = float4(in.v_color.rgb, in.v_color.a * blurred(d, sigma));
    } else {
      float d = sd_rect(p, shape + float4(in.v_p0.w, in.v_p0.w, -in.v_p0.w, -in.v_p0.w), max(in.v_radii - in.v_p0.w, 0.0));
      col = float4(in.v_color.rgb, in.v_color.a * cover(sd_rect(p, in.v_rect, in.v_radii), px) * (1.0 - blurred(d, sigma)));
    }
  } else {
    bool round_ = (flags & ROUND) != 0u;
    float w = in.v_misc.x;
    float d;
    if (kind == LINE) {
      float2 e = in.v_rect.zw - in.v_rect.xy;
      float len = length(e);
      float2 dir = len > 1e-6 ? e / len : float2(1.0, 0.0);
      float2 q = p - in.v_rect.xy;
      d = stroke(dot(q, dir), dot(q, float2(-dir.y, dir.x)), len, w, in.v_p2, round_);
    } else if (kind == ARC) {
      float2 q = p - in.v_rect.xy;
      float sweep = abs(in.v_p0.y);
      float ang = atan2(q.y, q.x) - in.v_p0.x;
      if (in.v_p0.y < 0.0) ang = -ang;
      ang = wrap(ang, TAU);
      if (ang > sweep + (TAU - sweep) * 0.5) ang -= TAU;
      d = stroke(ang * in.v_rect.z, length(q) - in.v_rect.z, sweep * in.v_rect.z, w, in.v_p2, round_);
    } else {
      d = sd_bezier(p, in.v_rect.xy, in.v_p0.xy, in.v_rect.zw) - w * 0.5;
    }
    col = float4(in.v_color.rgb, in.v_color.a * cover(d, px));
  }
  return float4(col.rgb, col.a * clip_cov * in.v_misc.z);
}
