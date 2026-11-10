#version 450
// pxui_render vertex stage. Line-for-line parallel with ui.metal (vertex half); layout in src/lib.rs.
// Rebuild ui.vert.spv with shaders/build.sh after editing.
struct Inst { vec4 rect; uvec4 a; uvec4 b; uvec4 c; };
layout(std430, set = 0, binding = 0) readonly buffer Instances { Inst inst[]; };
layout(std430, set = 0, binding = 1) readonly buffer Globals { vec4 g[]; };
layout(location = 0) out vec4 v_pos;
layout(location = 1) flat out vec4 v_rect;
layout(location = 2) flat out vec4 v_radii;
layout(location = 3) flat out vec4 v_clip;
layout(location = 4) flat out vec4 v_vclip;
layout(location = 5) flat out vec4 v_color;
layout(location = 6) flat out vec4 v_color2;
layout(location = 7) flat out vec4 v_p0;
layout(location = 8) flat out vec4 v_p2;
layout(location = 9) flat out vec4 v_misc;
layout(location = 10) flat out uvec2 v_kf;

const uint RECT = 0u, MASK = 1u, IMAGE = 2u, SHADOW = 3u, LINE = 4u, ARC = 5u, CURVE = 6u;
const uint INNER = 4u;
const vec2 CORNERS[6] = vec2[6](vec2(0, 0), vec2(1, 0), vec2(0, 1), vec2(0, 1), vec2(1, 0), vec2(1, 1));

vec2 i16x2(uint w) { return vec2(float(int(w << 16) >> 16), float(int(w) >> 16)); }

void main() {
  Inst I = inst[gl_InstanceIndex];
  vec2 c = CORNERS[gl_VertexIndex];
  uint kind = (I.c.x >> 16) & 255u;
  uint flags = I.c.x >> 24;
  uint view = I.c.y & 65535u;
  vec4 va = g[1u + 2u * view];
  vec4 vclip = g[2u + 2u * view];
  vec2 off = va.xy;
  float width = unpackHalf2x16(I.c.x).x;
  vec4 r = I.rect + (kind == ARC ? vec4(off, 0.0, 0.0) : off.xyxy);
  vec4 p0 = (kind == ARC || kind == CURVE) ? vec4(uintBitsToFloat(I.c.zw), 0.0, 0.0) : vec4(unpackHalf2x16(I.c.z), unpackHalf2x16(I.c.w));
  if (kind == CURVE) p0.xy += off;
  vec4 p2 = kind == RECT ? unpackUnorm4x8(I.b.x) : vec4(unpackHalf2x16(I.b.x), unpackHalf2x16(I.b.y));
  float h = width * 0.5 + 1.0;
  vec2 p;
  if (kind == LINE) {
    vec2 d = r.zw - r.xy;
    float len = length(d);
    vec2 dir = len > 1e-6 ? d / len : vec2(1.0, 0.0);
    vec2 n = vec2(-dir.y, dir.x);
    p = mix(r.xy - dir * h, r.zw + dir * h, c.x) + n * ((c.y * 2.0 - 1.0) * h);
  } else {
    vec4 box = r;
    if (kind == ARC) box = vec4(r.xy - (r.z + h), r.xy + (r.z + h));
    if (kind == CURVE) box = vec4(min(min(r.xy, r.zw), p0.xy) - h, max(max(r.xy, r.zw), p0.xy) + h);
    if (kind == RECT || kind == SHADOW) box = r + vec4(-1.0, -1.0, 1.0, 1.0);
    if (kind == SHADOW && (flags & INNER) == 0u) box = r + p0.xyxy + vec4(-1.0, -1.0, 1.0, 1.0) * (p0.w + 3.0 * p0.z + 1.0);
    p = mix(box.xy, box.zw, c);
  }
  vec2 uv = mix(unpackUnorm2x16(I.b.x), unpackUnorm2x16(I.b.y), c);
  gl_Position = vec4(p.x / g[0].x * 2.0 - 1.0, 1.0 - p.y / g[0].y * 2.0, 0.0, 1.0);
  v_pos = vec4(p, uv);
  v_rect = r;
  v_radii = vec4(unpackHalf2x16(I.b.z), unpackHalf2x16(I.b.w));
  v_clip = vec4(i16x2(I.a.x), i16x2(I.a.y)) + off.xyxy;
  v_vclip = vclip;
  v_color = unpackUnorm4x8(I.a.z);
  v_color2 = unpackUnorm4x8(I.a.w);
  v_p0 = p0;
  v_p2 = p2;
  v_misc = vec4(width, unpackHalf2x16(I.c.y).y, va.z, 0.0);
  v_kf = uvec2(kind, flags);
}
