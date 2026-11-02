#version 450
layout(set = 0, binding = 0) readonly buffer Offsets { float offsets[]; };
layout(location = 0) out vec2 uv;
void main() {
  vec2 p[3] = vec2[3](vec2(-1., -1.), vec2(3., -1.), vec2(-1., 3.));
  vec2 q = p[gl_VertexIndex];
  gl_Position = vec4(q + vec2(offsets[0], offsets[1]), 0., 1.);
  uv = (q + 1.) * .5;
}
