#include <metal_stdlib>
using namespace metal;
kernel void pack_planes(
    device const float* buffer0 [[buffer(0)]],
    device const float* buffer1 [[buffer(1)]],
    device const float* buffer2 [[buffer(2)]],
    device float* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r7=0u,r10=3u,r14=1u,r18=2u,r23=0u;
  long r0=long(gid);
  float r9=(r0<(long(buffer4[r7*1+0]))?1.0f:0.0f);
  if(r9!=0.0f) {
    uint r11=(uint(r0)*r10);
    uint r13=(buffer3[r11]=(buffer0[r0*1+0]),1u);
    uint r17=(buffer3[((r11+r14))]=(buffer1[r0*1+0]),1u);
    uint r21=(buffer3[((r11+r18))]=(buffer2[r0*1+0]),1u);
    uint r22=(r17+r21);
    r23=(r13+r22);
  }
  uint r24=(r9!=0.0f?r23:r7);
  (void)float(r24);
}
