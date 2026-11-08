#include <metal_stdlib>
using namespace metal;
kernel void fill_vectors(
    device uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r4=0u,r13=0u;
  long r0=long(gid);
  float r6=(r0<(long(buffer1[r4*1+0]))?1.0f:0.0f);
  if(r6!=0.0f) {
    long r8=long(buffer1[(1u)*1+0]);
    long r9=(r8==0||r8==-1?0:r0%r8);
    sb|=(r8==0)?2u:0u;
    r13=(buffer0[uint(r0)]=uint((long(buffer1[(((2u)+uint(r9)))*1+0]))),1u);
  }
  uint r14=(r6!=0.0f?r13:r4);
  (void)float(r14);
}
