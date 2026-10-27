#include <metal_stdlib>
using namespace metal;
kernel void kernel_e824aaa4c48f3be13b863814ccecd846(
  device const float* input0 [[buffer(0)]],
  device float* output [[buffer(1)]],
  constant uint* uniforms [[buffer(2)]],
  device atomic_uint* status [[buffer(3)]],
  uint i [[thread_position_in_grid]]) {
  if(i>=uniforms[0]) return;
  if(i==0) atomic_fetch_or_explicit(status,0u,memory_order_relaxed);
  float r0=input0[i*1+0];
  if(!isfinite(r0)) atomic_fetch_or_explicit(status,1u,memory_order_relaxed);
  float r1=as_type<float>(uniforms[1]);
  if(!isfinite(r1)) atomic_fetch_or_explicit(status,1u,memory_order_relaxed);
  float r2=(r0<r1);
  float r3=0.0f;
  if(r2!=0.0f) {
    r3=as_type<float>(0x3f800000u);
  }
  float r4=0.0f;
  if(r2!=0.0f) {
    r4=(r0+r3);
    if(!isfinite(r4)) atomic_fetch_or_explicit(status,1u,memory_order_relaxed);
  }
  float r5=0.0f;
  if(r2==0.0f) {
    r5=as_type<float>(0x3f800000u);
  }
  float r6=0.0f;
  if(r2==0.0f) {
    r6=(r0-r5);
    if(!isfinite(r6)) atomic_fetch_or_explicit(status,1u,memory_order_relaxed);
  }
  float r7=(r2!=0.0f?r4:r6);
  output[i*1+0]=r7;
}
