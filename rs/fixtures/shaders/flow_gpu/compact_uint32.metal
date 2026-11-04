#include <metal_stdlib>
using namespace metal;
kernel void compact_mask_uint32(
    device const uint* buffer0 [[buffer(0)]],
    device uint* buffer1 [[buffer(1)]],
    device uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    device uint* buffer5 [[buffer(5)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r7=1024u,r13=1073741823u,r15=1u,r18=0u,r22=1073741823u,r23=0u,r24=0u,r26=1073741823u,r31=0u,r35=1023u,r50=0u,r53=0u;
  long r57=0;
  uint r58=0,r64=0u,scan65_before=0u,scan65_total=0u,r70=0u,r72=0u;
  float r74=0.0f;
  long r75=0;
  uint r76=0,r87=0u,r90=0u;
  float r92=0.0f;
  uint r95=1073741823u,r98=0u,r100=0u,r101=0u;
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
    r53=((((long(buffer0[r0*1+0]))==long(r50)?1.0f:0.0f))!=0.0f?r50:(1u));
  }
  uint r54=(r48!=0.0f?r53:r18);
  float r55=(long(r14)==long(r15)?1.0f:0.0f);
  if(r55!=0.0f) {
    bool over57=r47>r44 && ulong(r47-r44)>4096uL;
    sb|=over57?1u:0u;
    r58=(0u);
    for(r57=r44;r57<(over57?r44:r47);r57++){
  uint r60=1073741823u,r62=0u;
  if(r55!=0.0f) {
    uint r61=(uint((long(buffer5[r57*1+0])))&r60);
    r62=(r58+r61);
  }
  uint next57_0=r62;
  r58=next57_0;
    }
    r64=r58;
  }
  uint r65=(r55!=0.0f?r64:r54);
  threadgroup uint scan65[1024];
  {uint v=r65;scan65[lid]=v;for(uint d=1u;d<1024u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan65[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan65[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan65_before=v-r65;scan65_total=scan65[1024-1];}
  uint r66=scan65_total;
  uint r67=scan65_before;
  float r68=(r10==long(r18)?1.0f:0.0f);
  float r69=(long(r14)==long(r18)?1.0f:0.0f);
  if(r69!=0.0f) {
    if(r68!=0.0f) {
      r70=(buffer5[uint(r8)]=r66,1u);
    }
    r72=(r68!=0.0f?r70:(0u));
  }
  if(r69==0.0f) {
    r74=(long(r14)==long((1u))?1.0f:0.0f);
    if(r74!=0.0f) {
      bool over75=r47>r44 && ulong(r47-r44)>4096uL;
      sb|=over75?1u:0u;
      r76=r67;
      for(r75=r44;r75<(over75?r44:r47);r75++){
  uint r78=1073741823u,r83=0u;
  if(r69==0.0f) {
    if(r74!=0.0f) {
      uint r81=(buffer5[((r37+uint(r75)))]=r76,1u);
      uint r82=(((uint((long(buffer5[r75*1+0])))&r78))*r81);
      r83=(r76+r82);
    }
  }
  uint next75_0=r83;
  r76=next75_0;
      }
      if(r68!=0.0f) {
        r87=(buffer2[(0u)]=r66,1u);
      }
      uint r89=(r68!=0.0f?r87:(0u));
      r90=((r76)+r89);
    }
    if(r74==0.0f) {
      r92=(long(r54)==long((1u))?1.0f:0.0f);
      if(r92!=0.0f) {
        r98=(buffer1[((((uint((long(buffer5[((r37+uint(r8)))*1+0])))&r95))+r67))]=uint(r0),1u);
      }
      r100=(r92!=0.0f?r98:(0u));
    }
    r101=(r74!=0.0f?r90:r100);
  }
  uint r102=(r69!=0.0f?r72:r101);
  (void)float(r102);
}
