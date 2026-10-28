#include <metal_stdlib>
using namespace metal;
kernel void kernel_f3cfe748828b50dcd686767ce61fc611(
  device const float* input0 [[buffer(0)]],
  device float* output [[buffer(1)]],
  constant uint* uniforms [[buffer(2)]],
  device atomic_uint* status [[buffer(3)]],
  uint i [[thread_position_in_grid]]) {
  if(i>=uniforms[0]) return;
  if(i==0) atomic_fetch_or_explicit(status,0u,memory_order_relaxed);
  uint sb=0u;
  float r3=as_type<float>(0x3f800000u);
  float r4=0.0f;
  float r5=as_type<float>(0x3f800000u);
  float r6=0.0f;
  float r0=input0[i*1+0];
  sb|=!isfinite(r0);
  float r1=as_type<float>(uniforms[1]);
  sb|=!isfinite(r1);
  float r2=(r0<r1);
  if(r2!=0.0f) {
    r4=(r0+r3);
    sb|=!isfinite(r4);
  }
  if(r2==0.0f) {
    r6=(r0-r5);
    sb|=!isfinite(r6);
  }
  float r7=(r2!=0.0f?r4:r6);
  if(sb!=0u) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);
  output[i*1+0]=r7;
}
