#include <metal_stdlib>
using namespace metal;
kernel void topology_check(
    device const uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    device uint* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r6=256u,r7=0u,r9=1u,r11=2u,r15=4u;
  long r19=0,r21=0,r28=0,r29=0;
  uint r30=0,r87=0u;
  float r88=0.0f;
  uint r89=0u,r93=0u,r94=1u,r96=1u,r101=0u,r108=0u,scan113_before=0u,scan113_total=0u,scan118_before=0u,scan118_total=0u,scan123_before=0u,scan123_total=0u;
  uint r126=8u,scan129_before=0u,scan129_total=0u,r132=16u,scan135_before=0u,scan135_total=0u,r138=32u,scan141_before=0u,scan141_total=0u,r144=64u,scan147_before=0u,scan147_total=0u;
  uint r150=128u,scan153_before=0u,scan153_total=0u,scan158_before=0u,scan158_total=0u,r162=9u,r167=1u,r171=2u,r174=3u,r179=4u,r182=5u,r186=6u;
  uint r189=7u,r192=8u,r198=0u;
  long r0=long(gid);
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
  long r24=long(buffer4[(5u)*1+0]);
  long r25=as_type<long>(ulong(r24)-ulong(long(r9)));
  long r27=(r24==0?0:r24==-1?as_type<long>(0ul-ulong((as_type<long>(ulong(((r17!=0.0f?r19:r21)))+ulong(r25))))):(as_type<long>(ulong(((r17!=0.0f?r19:r21)))+ulong(r25)))/r24);
  sb|=(r24==0)?2u:0u;
  r29=r0;
  r30=r7;
  for(r28=long(r7);r28<r27;r28++){
  uint r36=0u,r37=0u,r45=0u,r47=1u,r51=1u;
  float r54=0.0f,r55=0.0f;
  uint r64=0u;
  float r67=0.0f,r68=0.0f;
  uint r73=0u,r75=0u,r77=0u;
  float r31=(r29<r10?1.0f:0.0f);
  if(r31!=0.0f) {
    r36=((((long(buffer0[r29*1+0]))>=r8?1.0f:0.0f))!=0.0f?(16u):(0u));
  }
  float r39=(r29<r14?1.0f:0.0f);
  if(r39!=0.0f) {
    r45=((((long(buffer2[r29*1+0]))>long((2u))?1.0f:0.0f))!=0.0f?(64u):(0u));
  }
  float r49=((as_type<long>(ulong(r29)+ulong(long(r47))))<r12?1.0f:0.0f);
  if(r49!=0.0f) {
    long r50=long(buffer1[r29*1+0]);
    long r53=long(buffer1[((uint(r29)+r51))*1+0]);
    r54=(r50>r53?1.0f:0.0f);
    if(r54==0.0f) {
      r55=(r29<r14?1.0f:0.0f);
      if(r55!=0.0f) {
        long r56=long(buffer2[r29*1+0]);
        long r57=as_type<long>(ulong(r53)-ulong(r50));
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
  uint r80=(((r39!=0.0f?r45:r37))|r78);
  uint r81=(((r31!=0.0f?r36:r37))|r80);
  long next28_0=(as_type<long>(ulong(r29)+ulong(r24)));
  uint next28_1=((r30|r81));
  r29=next28_0;
  r30=next28_1;
  }
  float r86=(r0==long(r7)?1.0f:0.0f);
  if(r86!=0.0f) {
    r88=(r12==long(r87)?1.0f:0.0f);
    if(r88==0.0f) {
      r93=((((long(buffer1[r89*1+0]))==long(r89)?1.0f:0.0f))!=0.0f?r89:(2u));
      r101=((((long(buffer1[((uint(r12)-r96))*1+0]))==r10?1.0f:0.0f))!=0.0f?r89:(4u));
    }
    uint r106=(((r12==(as_type<long>(ulong(r14)+ulong(long(r94))))?1.0f:0.0f))!=0.0f?r87:(8u));
    uint r107=(((r88!=0.0f?r87:r101))|r106);
    r108=(((r88!=0.0f?r94:r93))|r107);
  }
  uint r109=(r86!=0.0f?r108:r7);
  uint r110=((r30)|r109);
  threadgroup uint scan113[256];
  {uint v=((((long(((r110&r9)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan113[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan113[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan113[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan113_before=v-((((long(((r110&r9)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan113_total=scan113[256-1];}
  uint r114=scan113_total;
  threadgroup uint scan118[256];
  {uint v=((((long(((r110&r11)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan118[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan118[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan118[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan118_before=v-((((long(((r110&r11)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan118_total=scan118[256-1];}
  uint r119=scan118_total;
  threadgroup uint scan123[256];
  {uint v=((((long(((r110&r15)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan123[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan123[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan123[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan123_before=v-((((long(((r110&r15)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan123_total=scan123[256-1];}
  uint r124=scan123_total;
  threadgroup uint scan129[256];
  {uint v=((((long(((r110&r126)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan129[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan129[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan129[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan129_before=v-((((long(((r110&r126)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan129_total=scan129[256-1];}
  uint r130=scan129_total;
  threadgroup uint scan135[256];
  {uint v=((((long(((r110&r132)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan135[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan135[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan135[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan135_before=v-((((long(((r110&r132)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan135_total=scan135[256-1];}
  uint r136=scan135_total;
  threadgroup uint scan141[256];
  {uint v=((((long(((r110&r138)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan141[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan141[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan141[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan141_before=v-((((long(((r110&r138)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan141_total=scan141[256-1];}
  uint r142=scan141_total;
  threadgroup uint scan147[256];
  {uint v=((((long(((r110&r144)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan147[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan147[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan147[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan147_before=v-((((long(((r110&r144)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan147_total=scan147[256-1];}
  uint r148=scan147_total;
  threadgroup uint scan153[256];
  {uint v=((((long(((r110&r150)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan153[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan153[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan153[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan153_before=v-((((long(((r110&r150)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan153_total=scan153[256-1];}
  uint r154=scan153_total;
  threadgroup uint scan158[256];
  {uint v=((((long(((r110&r6)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan158[lid]=v;for(uint d=1u;d<256u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan158[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan158[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan158_before=v-((((long(((r110&r6)))==long(r7)?1.0f:0.0f))!=0.0f?r7:r9));scan158_total=scan158[256-1];}
  uint r159=scan158_total;
  long r161=(long(r6)==0?0:long(r6)==-1?as_type<long>(0ul-ulong(r0)):r0/long(r6));
  sb|=(r6==0)?2u:0u;
  uint r163=(uint(r161)*r162);
  long r164=(long(r6)==0||long(r6)==-1?0:r0%long(r6));
  sb|=(r6==0)?2u:0u;
  float r165=(r164==long(r7)?1.0f:0.0f);
  if(r165!=0.0f) {
    uint r166=(buffer3[r163]=r114,1u);
    uint r169=(buffer3[((r163+r167))]=r119,1u);
    uint r173=(buffer3[((r163+r171))]=r124,1u);
    uint r176=(buffer3[((r163+r174))]=r130,1u);
    uint r177=(r173+r176);
    uint r181=(buffer3[((r163+r179))]=r136,1u);
    uint r184=(buffer3[((r163+r182))]=r142,1u);
    uint r188=(buffer3[((r163+r186))]=r148,1u);
    uint r191=(buffer3[((r163+r189))]=r154,1u);
    uint r194=(buffer3[((r163+r192))]=r159,1u);
    uint r195=(r191+r194);
    uint r196=(r188+r195);
    uint r197=(((r181+r184))+r196);
    r198=(((((r166+r169))+r177))+r197);
  }
  uint r199=(r165!=0.0f?r198:r7);
  (void)float(r199);
}
