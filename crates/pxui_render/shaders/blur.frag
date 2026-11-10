#version 450
// pxui_render backdrop blur, fragment stage: one separable gaussian pass (direction = tap step) with
// linear sampling. Line-for-line parallel with blur.metal. Rebuild blur.frag.spv with shaders/build.sh.
layout(set = 0, binding = 1) uniform texture2D src;
layout(set = 0, binding = 2) uniform sampler samp;
layout(location = 0) in vec2 v_uv;
layout(location = 1) flat in vec4 v_rect;
layout(location = 2) flat in vec4 v_tap;
layout(location = 0) out vec4 out_color;

void main() {
  vec2 base = v_rect.xy + v_uv * v_rect.zw;
  vec4 sum = vec4(0.0);
  float total = 0.0;
  int r = int(v_tap.w);
  for (int k = -r; k <= r; k++) {
    float w = exp(-0.5 * float(k * k) / (v_tap.z * v_tap.z));
    sum += w * textureLod(sampler2D(src, samp), base + v_tap.xy * float(k), 0.0);
    total += w;
  }
  out_color = sum / total;
}
