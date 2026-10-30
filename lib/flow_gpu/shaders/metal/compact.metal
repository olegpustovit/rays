// Generated from compact.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
kernel void compact_mask(
    device const float* buffer0 [[buffer(0)]],
    device float* buffer1 [[buffer(1)]],
    device uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    device uint* buffer5 [[buffer(5)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r7=1024u,r13=1073741823u,r15=1u,r18=0u,r22=1073741823u,r23=0u,r24=0u,r26=1073741823u,r31=0u,r35=1023u,r54=0u;
  long r58=0;
  uint r59=0,r65=0u,scan66_before=0u,scan66_total=0u,r71=0u,r73=0u;
  float r75=0.0f;
  long r76=0;
  uint r77=0,r88=0u,r91=0u;
  float r93=0.0f;
  uint r96=1073741823u,r100=0u,r102=0u,r103=0u;
  long r0=long(gid);
  long r8=(as_type<long>(ulong((long(r7)==0?0:r0/long(r7)))<<1)>>1);
  sb|=(r7==0)?2u:0u;
  long r9=(as_type<long>(ulong((ulong(r8)*ulong(long(r7))))<<1)>>1);
  long r10=(as_type<long>(ulong((ulong(r0)-ulong(r9)))<<1)>>1);
  uint r14=(uint((long(buffer3[(2u)*1+0])))&r13);
  float r19=(long(((uint((long(buffer3[r15*1+0])))&r13)))==long(r18)?1.0f:0.0f);
  if(r19!=0.0f) {
    r23=(uint((long(buffer3[(0u)*1+0])))&r22);
  }
  if(r19==0.0f) {
    uint r27=(uint((long(buffer3[r24*1+0])))&r26);
    uint r29=(uint((long(buffer4[r24*1+0])))&r26);
    r31=(((long(r27)<long(r29)?1.0f:0.0f))!=0.0f?r27:r29);
  }
  uint r32=(r19!=0.0f?r23:r31);
  uint r37=(r7==0u?0u:((((uint((long(buffer3[r18*1+0])))&r13))+r35))/r7);
  sb|=(r7==0)?2u:0u;
  uint r39=(r7==0u?0u:((r32+r35))/r7);
  sb|=(r7==0)?2u:0u;
  uint r41=(r7==0u?0u:((r39+r35))/r7);
  sb|=(r7==0)?2u:0u;
  long r42=(as_type<long>(ulong((ulong(r10)*ulong(long(r41))))<<1)>>1);
  long r44=(((long(r39)<r42?1.0f:0.0f))!=0.0f?long(r39):r42);
  long r45=(as_type<long>(ulong((ulong(r44)+ulong(long(r41))))<<1)>>1);
  long r47=(((long(r39)<r45?1.0f:0.0f))!=0.0f?long(r39):r45);
  float r48=(r0<long(r32)?1.0f:0.0f);
  if(r48!=0.0f) {
    r54=((((buffer0[r0*1+0])==(as_type<float>(0x0u))))!=0.0f?(0u):(1u));
  }
  uint r55=(r48!=0.0f?r54:r18);
  float r56=(long(r14)==long(r15)?1.0f:0.0f);
  if(r56!=0.0f) {
    bool over58=r47>r44 && ulong(r47-r44)>4096uL;
    sb|=over58?1u:0u;
    r59=(0u);
    for(r58=r44;r58<(over58?r44:r47);r58++){
  uint r61=1073741823u,r63=0u;
  if(r56!=0.0f) {
    uint r62=(uint((long(buffer5[r58*1+0])))&r61);
    r63=(r59+r62);
  }
  uint next58_0=r63;
  r59=next58_0;
    }
    r65=r59;
  }
  uint r66=(r56!=0.0f?r65:r55);
  threadgroup uint scan66[1024];
  {uint v=r66;scan66[lid]=v;for(uint d=1u;d<1024u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan66[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan66[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan66_before=v-r66;scan66_total=scan66[1024-1];}
  uint r67=scan66_total;
  uint r68=scan66_before;
  float r69=(r10==long(r18)?1.0f:0.0f);
  float r70=(long(r14)==long(r18)?1.0f:0.0f);
  if(r70!=0.0f) {
    if(r69!=0.0f) {
      r71=(buffer5[uint(r8)]=r67,1u);
    }
    r73=(r69!=0.0f?r71:(0u));
  }
  if(r70==0.0f) {
    r75=(long(r14)==long((1u))?1.0f:0.0f);
    if(r75!=0.0f) {
      bool over76=r47>r44 && ulong(r47-r44)>4096uL;
      sb|=over76?1u:0u;
      r77=r68;
      for(r76=r44;r76<(over76?r44:r47);r76++){
  uint r79=1073741823u,r84=0u;
  if(r70==0.0f) {
    if(r75!=0.0f) {
      uint r82=(buffer5[((r37+uint(r76)))]=r77,1u);
      uint r83=(((uint((long(buffer5[r76*1+0])))&r79))*r82);
      r84=(r77+r83);
    }
  }
  uint next76_0=r84;
  r77=next76_0;
      }
      if(r69!=0.0f) {
        r88=(buffer2[(0u)]=r67,1u);
      }
      uint r90=(r69!=0.0f?r88:(0u));
      r91=((r77)+r90);
    }
    if(r75==0.0f) {
      r93=(long(r55)==long((1u))?1.0f:0.0f);
      if(r93!=0.0f) {
        r100=(buffer1[((((uint((long(buffer5[((r37+uint(r8)))*1+0])))&r96))+r68))]=(float(r0)),1u);
      }
      r102=(r93!=0.0f?r100:(0u));
    }
    r103=(r75!=0.0f?r91:r102);
  }
  uint r104=(r70!=0.0f?r73:r103);
  (void)float(r104);
}
