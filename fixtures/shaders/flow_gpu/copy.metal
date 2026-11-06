#include <metal_stdlib>
using namespace metal;
kernel void copy_words(
    device const uint* buffer0 [[buffer(0)]],
    device uint* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r5=0u,r7=1073741823u,r12=1073741823u,r20=0u;
  long r0=long(gid);
  float r9=(r0<long(((uint((long(buffer2[r5*1+0])))&r7)))?1.0f:0.0f);
  if(r9!=0.0f) {
    r20=(buffer1[((((uint((long(buffer2[(2u)*1+0])))&r12))+uint(r0)))]=uint((long(buffer0[((((uint((long(buffer2[(1u)*1+0])))&r12))+uint(r0)))*1+0]))),1u);
  }
  uint r21=(r9!=0.0f?r20:r5);
  (void)float(r21);
}
