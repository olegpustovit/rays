#include <metal_stdlib>
using namespace metal;
kernel void sample_lod(texture2d<float> image [[texture(0)]], device float4 *out [[buffer(1)]], uint i [[thread_position_in_grid]]) { constexpr sampler s(filter::linear, mip_filter::linear, address::clamp_to_edge); out[i] = image.sample(s, float2(0.3, 0.6), level(float(i) * 0.5)); }
