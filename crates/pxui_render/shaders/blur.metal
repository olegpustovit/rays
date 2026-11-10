#include <metal_stdlib>
using namespace metal;
// pxui_render backdrop blur, Metal. Line-for-line parallel with blur.vert + blur.frag; layout in src/lib.rs.
struct BOut {
  float4 position [[position]];
  float2 uv;
  float4 rect [[flat]];
  float4 tap [[flat]];
};

vertex BOut blur_vertex(uint vid [[vertex_id]], device const float4* g [[buffer(0)]]) {
  BOut o;
  float2 uv = float2(float((vid << 1) & 2u), float(vid & 2u));
  o.position = float4(uv.x * 2.0 - 1.0, 1.0 - uv.y * 2.0, 0.0, 1.0);
  o.uv = uv;
  o.rect = g[0];  // source region in uv: x y w h
  o.tap = g[1];   // uv step per tap x y, sigma in taps, tap radius
  return o;
}

fragment float4 blur_fragment(BOut in [[stage_in]], texture2d<float> src [[texture(1)]], sampler samp [[sampler(2)]]) {
  float2 base = in.rect.xy + in.uv * in.rect.zw;
  float4 sum = float4(0.0);
  float total = 0.0;
  int r = int(in.tap.w);
  for (int k = -r; k <= r; k++) {
    float w = exp(-0.5 * float(k * k) / (in.tap.z * in.tap.z));
    sum += w * src.sample(samp, base + in.tap.xy * float(k), level(0.0));
    total += w;
  }
  return sum / total;
}
