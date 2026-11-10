#version 450
// pxui_render backdrop blur, vertex stage: a fullscreen triangle plus one params record. Line-for-line
// parallel with blur.metal. Rebuild blur.vert.spv with shaders/build.sh after editing.
layout(std430, set = 0, binding = 0) readonly buffer Params { vec4 g[]; };
layout(location = 0) out vec2 v_uv;
layout(location = 1) flat out vec4 v_rect;
layout(location = 2) flat out vec4 v_tap;

void main() {
  vec2 uv = vec2(float((gl_VertexIndex << 1) & 2), float(gl_VertexIndex & 2));
  gl_Position = vec4(uv.x * 2.0 - 1.0, 1.0 - uv.y * 2.0, 0.0, 1.0);
  v_uv = uv;
  v_rect = g[0];  // source region in uv: x y w h
  v_tap = g[1];   // uv step per tap x y, sigma in taps, tap radius
}
