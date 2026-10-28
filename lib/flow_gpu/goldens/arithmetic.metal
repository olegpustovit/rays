#include <metal_stdlib>
using namespace metal;
kernel void kernel_79035590dedb4f2da1962fef09baa43f(
  device const float* input0 [[buffer(0)]],
  device float* output [[buffer(1)]],
  constant uint* uniforms [[buffer(2)]],
  device atomic_uint* status [[buffer(3)]],
  uint i [[thread_position_in_grid]]) {
  if(i>=uniforms[0]) return;
  uint sb=0u;
  float r0=input0[i*1+0];
  sb|=!isfinite(r0);
  float r2=(r0*(as_type<float>(0x40000000u)));
  sb|=!isfinite(r2);
  float r3=as_type<float>(uniforms[1]);
  sb|=!isfinite(r3);
  float r4=(r2+r3);
  sb|=!isfinite(r4);
  if(sb!=0u||i==0) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);
  output[i*1+0]=r4;
}
