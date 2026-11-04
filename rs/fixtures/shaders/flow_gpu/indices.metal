#include <metal_stdlib>
using namespace metal;
kernel void write_indices(
    device uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r4=0u;
  float r10=0.0f;
  long r12=0;
  uint r14=0u;
  long r0=long(gid);
  float r6=(r0<(long(buffer1[r4*1+0]))?1.0f:0.0f);
  if(r6!=0.0f) {
    r10=((long(buffer1[(1u)*1+0]))==long((0u))?1.0f:0.0f);
    if(r10==0.0f) {
      r12=long(buffer1[(2u)*1+0]);
    }
    r14=(buffer0[uint(r0)]=((r10!=0.0f?uint(r0):uint(r12))),1u);
  }
  uint r15=(r6!=0.0f?r14:r4);
  (void)float(r15);
}
