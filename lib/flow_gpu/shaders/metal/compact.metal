// Generated from compact.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
kernel void compact_mask(
    device const float* buffer0 [[buffer(0)]],
    device float* buffer1 [[buffer(1)]],
    device uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device const uint* buffer4 [[buffer(4)]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r6=1024u,r7=1023u,r11=1073741823u,r13=0u,r17=1073741823u,r18=0u,r19=0u,r21=1073741823u,r26=0u,r37=0u,r38=0,r39=0;
  uint r40=0,r54=0u,scan55_before=0u,scan55_total=0u,r58=0,r59=0,r60=0,r75=0u,r79=0u;
  uint r8=(uint((long(lid)))&r7);
  float r14=(long(((uint((long(buffer3[(1u)*1+0])))&r11)))==long(r13)?1.0f:0.0f);
  if(r14!=0.0f) {
    r18=(uint((long(buffer3[(0u)*1+0])))&r17);
  }
  if(r14==0.0f) {
    uint r22=(uint((long(buffer3[r19*1+0])))&r21);
    uint r24=(uint((long(buffer4[r19*1+0])))&r21);
    r26=(((long(r22)<long(r24)?1.0f:0.0f))!=0.0f?r22:r24);
  }
  uint r27=(r14!=0.0f?r18:r26);
  uint r29=(r6==0u?0u:((r27+r7))/r6);
  sb|=(r6==0)?2u:0u;
  uint r30=(r8*r29);
  uint r32=(((long(r27)<long(r30)?1.0f:0.0f))!=0.0f?r27:r30);
  uint r33=(r32+r29);
  uint r35=(((long(r27)<long(r33)?1.0f:0.0f))!=0.0f?r27:r33);
  float r36=(long(r32)<long(r35)?1.0f:0.0f);
  if(r36!=0.0f) {
    r39=r32;
    r40=r37;
    for(r38=r37;r38<r29;r38++){
  uint r41=1u,r42=0u;
  float r45=0.0f;
  uint r46=1u,r47=0u,r48=0u;
  float r51=0.0f;
  if(r36!=0.0f) {
    r42=(r39+r41);
    r45=((buffer0[r39*1+0])==(as_type<float>(0x0u)));
    if(r45==0.0f) {
      r47=(r40+r46);
    }
    r48=(r45!=0.0f?r40:r47);
    r51=(((((long(r42)>=long(r35)?1.0f:0.0f))==0.0f))==0.0f);
  }
  bool stop38=r51!=0.0f;
  uint next38_0=r42;
  uint next38_1=r48;
  r39=next38_0;
  r40=next38_1;
  if(stop38)break;
    }
    r54=r40;
  }
  uint r55=(r36!=0.0f?r54:r13);
  threadgroup uint scan55[1024];
  {uint v=r55;scan55[lid]=v;for(uint d=1u;d<1024u;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?scan55[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;scan55[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);scan55_before=v-r55;scan55_total=scan55[1024-1];}
  uint r56=scan55_total;
  uint r57=scan55_before;
  if(r36!=0.0f) {
    r59=r32;
    r60=r57;
    for(r58=r37;r58<r29;r58++){
  uint r61=1u,r62=0u;
  float r65=0.0f;
  uint r68=0u,r69=0u;
  float r72=0.0f;
  if(r36!=0.0f) {
    r62=(r59+r61);
    r65=((buffer0[r59*1+0])==(as_type<float>(0x0u)));
    if(r65==0.0f) {
      uint r67=(buffer1[r60]=(float(r59)),1u);
      r68=(r60+r67);
    }
    r69=(r65!=0.0f?r60:r68);
    r72=(((((long(r62)>=long(r35)?1.0f:0.0f))==0.0f))==0.0f);
  }
  bool stop58=r72!=0.0f;
  uint next58_0=r62;
  uint next58_1=r69;
  r59=next58_0;
  r60=next58_1;
  if(stop58)break;
    }
    r75=r60;
  }
  float r77=(long(r8)==long(r13)?1.0f:0.0f);
  if(r77!=0.0f) {
    r79=(buffer2[(0u)]=r56,1u);
  }
  uint r80=(r77!=0.0f?r79:r13);
  uint r81=(((r36!=0.0f?r75:r57))+r80);
  (void)float(r81);
}
