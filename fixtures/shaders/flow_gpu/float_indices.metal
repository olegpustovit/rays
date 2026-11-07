#include <metal_stdlib>
using namespace metal;
kernel void write_float_indices(
    device float* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r4=0u,r8=0u;
  long r0=long(gid);
  float r6=(r0<(long(buffer1[r4*1+0]))?1.0f:0.0f);
  if(r6!=0.0f) {
    r8=(buffer0[uint(r0)]=(float(r0)),1u);
  }
  uint r9=(r6!=0.0f?r8:r4);
  (void)float(r9);
}
