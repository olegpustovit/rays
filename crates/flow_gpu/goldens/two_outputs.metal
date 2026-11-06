#include <metal_stdlib>
using namespace metal;
kernel void kernel_6de127b8aec95c09142b5fa0652047a4(
  device const float* input0 [[buffer(0)]],
  device float* output0 [[buffer(1)]],
  device float* output1 [[buffer(2)]],
  constant uint* uniforms [[buffer(3)]],
  device atomic_uint* status [[buffer(4)]],
  uint i [[thread_position_in_grid]]) {
  if(i>=uniforms[0]) return;
  uint sb=0u;
  float r0=input0[i*1+0];
  sb|=!isfinite(r0);
  float r1=as_type<float>(uniforms[1]);
  sb|=!isfinite(r1);
  float r2=(r0+r1);
  sb|=!isfinite(r2);
  if(sb!=0u||i==0) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);
  output0[i*1+0]=r2;
  output1[i*2+0]=r0;
  output1[i*2+1]=r2;
}
