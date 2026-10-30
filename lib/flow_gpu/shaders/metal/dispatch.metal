// Generated from dispatch.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
kernel void dispatch_arguments(
    device const uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device uint* buffer2 [[buffer(2)]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r4=1u,r5=0u,r7=1073741823u,r27=1u,r36=0u;
  uint r8=(uint((long(buffer1[r5*1+0])))&r7);
  uint r12=(uint((long(buffer1[r4*1+0])))&r7);
  uint r16=(uint((long(buffer1[(2u)*1+0])))&r7);
  long r17=(as_type<long>(ulong((long(r16)==0?0:((as_type<long>(ulong((ulong(long(((uint((long(buffer0[r5*1+0])))&r7))))*ulong(long(r12))))<<1)>>1))/long(r16)))<<1)>>1);
  sb|=(r16==0)?2u:0u;
  uint r22=(uint((long(buffer1[(3u)*1+0])))&r7);
  long r23=(as_type<long>(ulong((ulong(long(r22))-ulong(long(r4))))<<1)>>1);
  long r25=(as_type<long>(ulong((long(r22)==0?0:((as_type<long>(ulong((ulong(((((long(r8)<r17?1.0f:0.0f))!=0.0f?long(r8):r17)))+ulong(r23)))<<1)>>1))/long(r22)))<<1)>>1);
  sb|=(r22==0)?2u:0u;
  float r26=((long(lid))==long(r5)?1.0f:0.0f);
  if(r26!=0.0f) {
    uint r31=(buffer2[(0u)]=((((r25<long(r27)?1.0f:0.0f))!=0.0f?r27:uint(r25))),1u);
    uint r32=(buffer2[r27]=r27,1u);
    uint r34=(buffer2[(2u)]=r27,1u);
    uint r35=(r32+r34);
    r36=(r31+r35);
  }
  uint r37=(r26!=0.0f?r36:r5);
  (void)float(r37);
}
