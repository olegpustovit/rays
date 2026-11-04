#include <metal_stdlib>
using namespace metal;
kernel void topology_check(
    device const uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    device uint* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r6=256u,r7=0u,r9=1u,r11=2u,r15=4u;
  long r19=0,r21=0;
  uint r23=255u;
  int r26=0;
  uint r27=0,r85=0u;
  float r86=0.0f;
  uint r87=0u,r91=0u,r92=1u,r94=1u,r99=0u,r103=8u,r106=0u,scan111_before=0u,scan111_total=0u,scan116_before=0u,scan116_total=0u,scan121_before=0u;
  uint scan121_total=0u,r124=8u,scan127_before=0u,scan127_total=0u,r130=16u,scan133_before=0u,scan133_total=0u,r136=32u,scan139_before=0u,scan139_total=0u,r142=64u,scan145_before=0u;
  uint scan145_total=0u,r148=128u,scan151_before=0u,scan151_total=0u,scan156_before=0u,scan156_total=0u,r181=0u;
  long r0=long(lid);
  long r8=long(buffer4[r7*1+0]);
  long r10=long(buffer4[r9*1+0]);
  long r12=long(buffer4[r11*1+0]);
  long r14=long(buffer4[(3u)*1+0]);
  long r16=long(buffer4[r15*1+0]);
  float r17=(r10>r12?1.0f:0.0f);
  if(r17!=0.0f) {
    r19=(((r10>r14?1.0f:0.0f))!=0.0f?r10:r14);
  }
  if(r17==0.0f) {
    r21=(((r12>r14?1.0f:0.0f))!=0.0f?r12:r14);
  }
  long r25=(as_type<long>(ulong((long(r6)==0?0:((as_type<long>(ulong((ulong(((r17!=0.0f?r19:r21)))+ulong(long(r23))))<<1)>>1))/long(r6)))<<1)>>1);
  sb|=(r6==0)?2u:0u;
  bool over26=r25>long(r7) && ulong(r25-long(r7))>4096uL;
  sb|=over26?1u:0u;
  r27=r7;
  for(r26=long(r7);r26<(over26?long(r7):r25);r26++){
  uint r28=256u,r36=0u,r37=0u,r45=0u,r47=1u,r51=1u;
  float r54=0.0f,r55=0.0f;
  uint r64=0u;
  float r67=0.0f,r68=0.0f;
  uint r73=0u,r75=0u,r77=0u;
  long r29=(as_type<long>(ulong((ulong(r26)*ulong(long(r28))))<<1)>>1);
  long r30=(as_type<long>(ulong((ulong(r0)+ulong(r29)))<<1)>>1);
  float r31=(r30<r10?1.0f:0.0f);
  if(r31!=0.0f) {
    r36=((((long(buffer0[r30*1+0]))>=r8?1.0f:0.0f))!=0.0f?(16u):(0u));
  }
  float r39=(r30<r14?1.0f:0.0f);
  if(r39!=0.0f) {
    r45=((((long(buffer2[r30*1+0]))>long((2u))?1.0f:0.0f))!=0.0f?(64u):(0u));
  }
  float r49=(((as_type<long>(ulong((ulong(r30)+ulong(long(r47))))<<1)>>1))<r12?1.0f:0.0f);
  if(r49!=0.0f) {
    long r50=long(buffer1[r30*1+0]);
    long r53=long(buffer1[((uint(r30)+r51))*1+0]);
    r54=(r50>r53?1.0f:0.0f);
    if(r54==0.0f) {
      r55=(r30<r14?1.0f:0.0f);
      if(r55!=0.0f) {
        long r56=long(buffer2[r30*1+0]);
        long r57=(as_type<long>(ulong((ulong(r53)-ulong(r50)))<<1)>>1);
        r67=(r56==long(r64)?1.0f:0.0f);
        if(r67!=0.0f) {
          r68=(r57>r16?1.0f:0.0f);
        }
        uint r72=(((r67!=0.0f?r68:(as_type<float>(0x0u))))!=0.0f?(256u):r64);
        r73=(((((r57<long(((((r56==long((1u))?1.0f:0.0f))!=0.0f?(2u):(3u))))?1.0f:0.0f))!=0.0f?(128u):r64))|r72);
      }
      r75=(r55!=0.0f?r73:(0u));
    }
    r77=(r54!=0.0f?(32u):r75);
  }
  uint r78=(r49!=0.0f?r77:r37);
  uint r79=(((r39!=0.0f?r45:r37))|r78);
  uint r80=(((r31!=0.0f?r36:r37))|r79);
  uint next26_0=((r27|r80));
  r27=next26_0;
  }
  float r84=(r0==long(r7)?1.0f:0.0f);
  if(r84!=0.0f) {
    r86=(r12==long(r85)?1.0f:0.0f);
    if(r86==0.0f) {
      r91=((((long(buffer1[r87*1+0]))==long(r87)?1.0f:0.0f))!=0.0f?r87:(2u));
      r99=((((long(buffer1[((uint(r12)-r94))*1+0]))==r10?1.0f:0.0f))!=0.0f?r87:(4u));
    }
    uint r104=(((r12==((as_type<long>(ulong((ulong(r14)+ulong(long(r92))))<<1)>>1))?1.0f:0.0f))!=0.0f?r85:r103);
    uint r105=(((r86!=0.0f?r85:r99))|r104);
    r106=(((r86!=0.0f?r92:r91))|r105);
  }
  uint r107=(r84!=0.0f?r106:r7);
  uint r108=((r27)|r107);
  threadgroup uint scan111[256];
  {uint v=((((long(((r108&r9)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan111[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan111[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan111[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan111_before=v-((((long(((r108&r9)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan111_total=scan111[256-1];}
  uint r112=scan111_total;
  threadgroup uint scan116[256];
  {uint v=((((long(((r108&r11)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan116[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan116[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan116[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan116_before=v-((((long(((r108&r11)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan116_total=scan116[256-1];}
  uint r117=scan116_total;
  threadgroup uint scan121[256];
  {uint v=((((long(((r108&r15)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan121[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan121[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan121[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan121_before=v-((((long(((r108&r15)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan121_total=scan121[256-1];}
  uint r122=scan121_total;
  threadgroup uint scan127[256];
  {uint v=((((long(((r108&r124)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan127[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan127[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan127[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan127_before=v-((((long(((r108&r124)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan127_total=scan127[256-1];}
  uint r128=scan127_total;
  threadgroup uint scan133[256];
  {uint v=((((long(((r108&r130)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan133[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan133[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan133[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan133_before=v-((((long(((r108&r130)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan133_total=scan133[256-1];}
  uint r134=scan133_total;
  threadgroup uint scan139[256];
  {uint v=((((long(((r108&r136)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan139[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan139[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan139[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan139_before=v-((((long(((r108&r136)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan139_total=scan139[256-1];}
  uint r140=scan139_total;
  threadgroup uint scan145[256];
  {uint v=((((long(((r108&r142)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan145[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan145[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan145[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan145_before=v-((((long(((r108&r142)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan145_total=scan145[256-1];}
  uint r146=scan145_total;
  threadgroup uint scan151[256];
  {uint v=((((long(((r108&r148)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan151[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan151[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan151[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan151_before=v-((((long(((r108&r148)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan151_total=scan151[256-1];}
  uint r152=scan151_total;
  threadgroup uint scan156[256];
  {uint v=((((long(((r108&r6)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan156[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan156[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan156[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan156_before=v-((((long(((r108&r6)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan156_total=scan156[256-1];}
  uint r157=scan156_total;
  if(r84!=0.0f) {
    uint r159=(buffer3[r85]=r112,1u);
    uint r160=(buffer3[r92]=r117,1u);
    uint r163=(buffer3[(2u)]=r122,1u);
    uint r165=(buffer3[(3u)]=r128,1u);
    uint r166=(r163+r165);
    uint r169=(buffer3[(4u)]=r134,1u);
    uint r171=(buffer3[(5u)]=r140,1u);
    uint r174=(buffer3[(6u)]=r146,1u);
    uint r176=(buffer3[(7u)]=r152,1u);
    uint r177=(buffer3[r103]=r157,1u);
    uint r178=(r176+r177);
    uint r179=(r174+r178);
    uint r180=(((r169+r171))+r179);
    r181=(((((r159+r160))+r166))+r180);
  }
  uint r182=(r84!=0.0f?r181:r7);
  (void)float(r182);
}
