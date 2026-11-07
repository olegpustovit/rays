#include <metal_stdlib>
using namespace metal;
kernel void incidence_partition(
    device const uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device uint* buffer2 [[buffer(2)]],
    device uint* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r6=1024u,r7=0u,r9=1u,r15=1023u,r30=0u;
  float r31=0.0f;
  long r32=0;
  uint r33=0u,r37=1u,r40=0u;
  long r43=0;
  uint r44=0,r48=0u,scan49_before=0u,scan49_total=0u,r53=0u;
  float r54=0.0f;
  uint r55=0u,r56=0u;
  float r58=0.0f;
  long r59=0;
  uint r60=0,r68=0u;
  float r69=0.0f;
  uint r70=2u,r72=0u,r74=0u;
  float r75=0.0f,r80=0.0f;
  uint r81=2u,r85=0u,r87=0u,r89=0u,r90=0u;
  long r0=long(gid);
  long r8=long(buffer4[r7*1+0]);
  long r10=long(buffer4[r9*1+0]);
  long r12=long(buffer4[(2u)*1+0]);
  long r13=(as_type<long>(ulong((long(r6)==0?0:r0/long(r6)))<<1)>>1);
  sb|=(r6==0)?2u:0u;
  long r14=(as_type<long>(ulong((long(r6)==0?0:r0%long(r6)))<<1)>>1);
  sb|=(r6==0)?2u:0u;
  long r17=(as_type<long>(ulong((long(r6)==0?0:((as_type<long>(ulong((ulong(r8)+ulong(long(r15))))<<1)>>1))/long(r6)))<<1)>>1);
  sb|=(r6==0)?2u:0u;
  long r19=(as_type<long>(ulong((long(r6)==0?0:((as_type<long>(ulong((ulong(r17)+ulong(long(r15))))<<1)>>1))/long(r6)))<<1)>>1);
  sb|=(r6==0)?2u:0u;
  long r20=(as_type<long>(ulong((ulong(r14)*ulong(r19)))<<1)>>1);
  long r22=(((r17<r20?1.0f:0.0f))!=0.0f?r17:r20);
  long r23=(as_type<long>(ulong((ulong(r22)+ulong(r19)))<<1)>>1);
  long r25=(((r17<r23?1.0f:0.0f))!=0.0f?r17:r23);
  float r27=(r12==long(r9)?1.0f:0.0f);
  float r29=(((r0<r8?1.0f:0.0f))!=0.0f&&((r27==0.0f))!=0.0f);
  if(r29!=0.0f) {
    r31=(r10==long(r30)?1.0f:0.0f);
    if(r31==0.0f) {
      r32=long(buffer1[r0*1+0]);
    }
    r33=(r31!=0.0f?uint(r0):uint(r32));
  }
  uint r34=(r29!=0.0f?r33:r7);
  if(r29!=0.0f) {
    long r36=(as_type<long>(ulong(long((ulong((long(buffer0[r34*1+0])))&0x7fffffffffffffffUL)>>(r10&63)))<<1)>>1);
    sb|=(r10<0 || r10>62)?4u:0u;
    r40=(((long(((uint(r36)&r37)))==long(r30)?1.0f:0.0f))!=0.0f?r37:r30);
  }
  uint r41=(r29!=0.0f?r40:r7);
  if(r27!=0.0f) {
    bool over43=r25>r22 && ulong(r25-r22)>4096uL;
    sb|=over43?1u:0u;
    r44=(0u);
    for(r43=r22;r43<(over43?r22:r25);r43++){
  uint r46=0u;
  if(r27!=0.0f) {
    long r45=long(buffer3[r43*1+0]);
    r46=(r44+uint(r45));
  }
  uint next43_0=r46;
  r44=next43_0;
    }
    r48=r44;
  }
  uint r49=(r27!=0.0f?r48:r41);
  threadgroup uint scan49[1024];
  {uint v=r49;scan49[lid]=v;for(uint d=1u;d<1024u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan49[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan49[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan49_before=v-r49;scan49_total=scan49[1024-1];}
  uint r50=scan49_total;
  uint r51=scan49_before;
  float r52=(r12==long(r7)?1.0f:0.0f);
  if(r52!=0.0f) {
    r54=(r14==long(r53)?1.0f:0.0f);
    if(r54!=0.0f) {
      r55=(buffer3[uint(r13)]=r50,1u);
    }
    r56=(r54!=0.0f?r55:r53);
  }
  if(r52==0.0f) {
    r58=(r12==long((1u))?1.0f:0.0f);
    if(r58!=0.0f) {
      bool over59=r25>r22 && ulong(r25-r22)>4096uL;
      sb|=over59?1u:0u;
      r60=r51;
      for(r59=r22;r59<(over59?r22:r25);r59++){
  uint r65=0u;
  if(r52==0.0f) {
    if(r58!=0.0f) {
      uint r63=(buffer3[((uint(r17)+uint(r59)))]=r60,1u);
      uint r64=(uint((long(buffer3[r59*1+0])))*r63);
      r65=(r60+r64);
    }
  }
  uint next59_0=r65;
  r60=next59_0;
      }
      r69=(r14==long(r68)?1.0f:0.0f);
      if(r69!=0.0f) {
        r72=(buffer3[((uint(r17)*r70))]=r50,1u);
      }
      uint r73=(r69!=0.0f?r72:r68);
      r74=((r60)+r73);
    }
    if(r58==0.0f) {
      r75=(r0<r8?1.0f:0.0f);
      if(r75!=0.0f) {
        uint r78=(uint((long(buffer3[((uint(r17)+uint(r13)))*1+0])))+r51);
        r80=(long(r41)==long((1u))?1.0f:0.0f);
        if(r80==0.0f) {
          uint r84=(uint(r0)-r78);
          r85=(uint((long(buffer3[((uint(r17)*r81))*1+0])))+r84);
        }
        r87=(buffer2[((r80!=0.0f?r78:r85))]=r34,1u);
      }
      r89=(r75!=0.0f?r87:(0u));
    }
    r90=(r58!=0.0f?r74:r89);
  }
  uint r91=(r52!=0.0f?r56:r90);
  (void)float(r91);
}
