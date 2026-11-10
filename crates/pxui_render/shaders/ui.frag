#version 450
#extension GL_EXT_samplerless_texture_functions : require
// pxui_render fragment stage. Line-for-line parallel with ui.metal (fragment half); layout in src/lib.rs.
// Rebuild ui.frag.spv with shaders/build.sh after editing. Output is straight (non-premultiplied) alpha.
layout(set = 0, binding = 2) uniform texture2D mask_tex;
layout(set = 0, binding = 3) uniform texture2D color_tex;
layout(set = 0, binding = 4) uniform sampler samp;
layout(set = 0, binding = 5) uniform texture2D atlas_tex;
layout(location = 0) in vec4 v_pos;
layout(location = 1) flat in vec4 v_rect;
layout(location = 2) flat in vec4 v_radii;
layout(location = 3) flat in vec4 v_clip;
layout(location = 4) flat in vec4 v_vclip;
layout(location = 5) flat in vec4 v_color;
layout(location = 6) flat in vec4 v_color2;
layout(location = 7) flat in vec4 v_p0;
layout(location = 8) flat in vec4 v_p2;
layout(location = 9) flat in vec4 v_misc;
layout(location = 10) flat in uvec2 v_kf;
layout(location = 11) in vec2 v_tp;
layout(location = 0) out vec4 out_color;

const uint RECT = 0u, MASK = 1u, IMAGE = 2u, SHADOW = 3u, LINE = 4u, ARC = 5u, CURVE = 6u;
const uint INNER = 4u, ROUND = 8u;
const float TAU = 6.2831853;

float wrap(float x, float y) { return x - y * floor(x / y); }
float cover(float d, float px) { return clamp(0.5 - d / px, 0.0, 1.0); }
// radii are top-left, top-right, bottom-right, bottom-left (y down)
float sd_rect(vec2 p, vec4 r, vec4 radii) {
  vec2 half_ = (r.zw - r.xy) * 0.5;
  vec2 q = p - (r.xy + r.zw) * 0.5;
  float rad = q.x < 0.0 ? (q.y < 0.0 ? radii.x : radii.w) : (q.y < 0.0 ? radii.y : radii.z);
  rad = min(rad, min(half_.x, half_.y));
  vec2 d = abs(q) - half_ + rad;
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
vec4 over(vec4 top, vec4 bottom) {
  float a = top.a + bottom.a * (1.0 - top.a);
  if (a <= 0.0) return vec4(0.0);
  return vec4((top.rgb * top.a + bottom.rgb * bottom.a * (1.0 - top.a)) / a, a);
}
// premultiplied interpolation of two straight-alpha colours
vec4 mix_premul(vec4 a, vec4 b, float t) {
  vec4 m = mix(vec4(a.rgb * a.a, a.a), vec4(b.rgb * b.a, b.a), t);
  return m.a > 0.0 ? vec4(m.rgb / m.a, m.a) : vec4(0.0);
}
// signed distance to a stroke at arc length s and offset t from its centre line of length len;
// dash = (on, off, phase) in points, on = 0 is solid
float stroke(float s, float t, float len, float w, vec4 dash, bool round_) {
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
  return round_ ? length(vec2(max(da, 0.0), t)) - w * 0.5 : max(da, abs(t) - w * 0.5);
}
// distance to the quadratic bezier a-b-c (Inigo Quilez, MIT)
float sd_bezier(vec2 pos, vec2 A, vec2 B, vec2 C) {
  vec2 a = B - A, b = A - 2.0 * B + C, c = a * 2.0, d = A - pos;
  if (dot(b, b) < 1e-6) {
    vec2 e = C - A;
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
    vec2 x = (vec2(h, -h) - q) * 0.5;
    vec2 uv = sign(x) * pow(abs(x), vec2(1.0 / 3.0));
    float t = clamp(uv.x + uv.y - kx, 0.0, 1.0);
    vec2 e = d + (c + b * t) * t;
    res = dot(e, e);
  } else {
    float z = sqrt(-p);
    float v = acos(q / (p * z * 2.0)) / 3.0;
    float m = cos(v), n = sin(v) * 1.7320508;
    vec3 t = clamp(vec3(m + m, -n - m, n - m) * z - kx, 0.0, 1.0);
    vec2 e = d + (c + b * t.x) * t.x;
    res = dot(e, e);
    e = d + (c + b * t.y) * t.y;
    res = min(res, dot(e, e));
  }
  return sqrt(res);
}

void main() {
  vec2 p = v_pos.xy;
  // points per pixel, rotation invariant (equals the old fwidth form on axis-aligned views)
  float px = max(sqrt(0.5 * (dot(dFdx(p), dFdx(p)) + dot(dFdy(p), dFdy(p)))), 1e-4);
  float pxt = max(sqrt(0.5 * (dot(dFdx(v_tp), dFdx(v_tp)) + dot(dFdy(v_tp), dFdy(v_tp)))), 1e-4);
  uint kind = v_kf.x, flags = v_kf.y & 255u, ramp = v_kf.y >> 8u;
  float clip_cov = cover(sd_rect(p, v_clip, vec4(v_misc.y)), px) * cover(sd_rect(v_tp, v_vclip, vec4(0.0)), pxt);
  if (clip_cov <= 0.0) discard;
  vec4 col;
  if (kind == MASK) {
    col = vec4(v_color.rgb, v_color.a * textureLod(sampler2D(mask_tex, samp), v_pos.zw, 0.0).r);
  } else if (kind == IMAGE) {
    col = v_color * textureLod(sampler2D(color_tex, samp), v_pos.zw, 0.0);
    if (dot(v_radii, vec4(1.0)) > 0.0) col.a *= cover(sd_rect(p, v_rect, v_radii), px);
  } else if (kind == RECT) {
    float d = sd_rect(p, v_rect, v_radii);
    vec4 fill = v_color;
    uint grad = flags & 3u;
    if (grad != 0u) {
      vec2 size = max(v_rect.zw - v_rect.xy, vec2(1e-6));
      vec2 q = (p - v_rect.xy) / size;
      float t;
      if (grad == 1u) {
        vec2 e = v_p0.zw - v_p0.xy;
        t = dot(q - v_p0.xy, e) / max(dot(e, e), 1e-12);
      } else if (grad == 2u) {
        t = length((q - v_p0.xy) / max(v_p0.zw, vec2(1e-6)));
      } else {
        vec2 e = (q - v_p0.xy) * size;
        t = wrap(atan(e.y, e.x) / TAU - v_p0.z, 1.0);
      }
      t = clamp(t, 0.0, 1.0);
      fill = ramp != 0u ? texelFetch(atlas_tex, ivec2(int(t * 255.0 + 0.5), int(ramp) - 1), 0) : mix_premul(v_color, v_p2, t);
    }
    if (v_misc.x > 0.0) {
      float inner = d + v_misc.x;
      uint sides = flags >> 4u;  // top 1, right 2, bottom 4, left 8; 0 or 15 = all
      if (sides != 0u && sides != 15u) {
        vec4 s = vec4(float((sides >> 3u) & 1u), float(sides & 1u), -float((sides >> 1u) & 1u), -float((sides >> 2u) & 1u));
        inner = sd_rect(p, v_rect + s * v_misc.x, max(v_radii - v_misc.x, 0.0));
      }
      fill = over(vec4(v_color2.rgb, v_color2.a * (1.0 - cover(inner, px))), fill);
    }
    col = vec4(fill.rgb, fill.a * cover(d, px));
  } else if (kind == SHADOW) {
    float sigma = sqrt(v_p0.z * v_p0.z + 0.25 * px * px);
    vec4 shape = v_rect + v_p0.xyxy;
    if ((flags & INNER) == 0u) {
      float d = sd_rect(p, shape + vec4(-v_p0.w, -v_p0.w, v_p0.w, v_p0.w), max(v_radii + v_p0.w, 0.0));
      col = vec4(v_color.rgb, v_color.a * blurred(d, sigma));
    } else {
      float d = sd_rect(p, shape + vec4(v_p0.w, v_p0.w, -v_p0.w, -v_p0.w), max(v_radii - v_p0.w, 0.0));
      col = vec4(v_color.rgb, v_color.a * cover(sd_rect(p, v_rect, v_radii), px) * (1.0 - blurred(d, sigma)));
    }
  } else {
    bool round_ = (flags & ROUND) != 0u;
    float w = v_misc.x;
    float d;
    if (kind == LINE) {
      vec2 e = v_rect.zw - v_rect.xy;
      float len = length(e);
      vec2 dir = len > 1e-6 ? e / len : vec2(1.0, 0.0);
      vec2 q = p - v_rect.xy;
      d = stroke(dot(q, dir), dot(q, vec2(-dir.y, dir.x)), len, w, v_p2, round_);
    } else if (kind == ARC) {
      vec2 q = p - v_rect.xy;
      float sweep = abs(v_p0.y);
      float ang = atan(q.y, q.x) - v_p0.x;
      if (v_p0.y < 0.0) ang = -ang;
      ang = wrap(ang, TAU);
      if (ang > sweep + (TAU - sweep) * 0.5) ang -= TAU;
      d = stroke(ang * v_rect.z, length(q) - v_rect.z, sweep * v_rect.z, w, v_p2, round_);
    } else {
      d = sd_bezier(p, v_rect.xy, v_p0.xy, v_rect.zw) - w * 0.5;
    }
    col = vec4(v_color.rgb, v_color.a * cover(d, px));
  }
  out_color = vec4(col.rgb, col.a * clip_cov * v_misc.z);
}
