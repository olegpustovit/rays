// Generated from normals.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
kernel void point_normal(
    device const float* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device float* buffer4 [[buffer(4)]],
    device const uint* buffer5 [[buffer(5)]],
    uint lid [[thread_position_in_grid]]) {
  uint sb=0u;
  uint r8=0u,r10=1073741823u;
  float r13=as_type<float>(0x0u);
  uint r15=1u;
  long r18=0;
  float r19=0,r20=0,r21=0,r80=0.0f,r81=as_type<float>(0x0u),r82=0.0f,r83=0.0f,r84=0.0f;
  uint r88=3u,r94=2u,r97=0u;
  long r0=long(lid);
  float r12=(r0<long(((uint((long(buffer5[r8*1+0])))&r10)))?1.0f:0.0f);
  if(r12!=0.0f) {
    long r14=long(buffer2[r0*1+0]);
    long r17=long(buffer2[((uint(r0)+r15))*1+0]);
    bool over18=r17>r14 && ulong(r17-r14)>4096uL;
    sb|=over18?1u:0u;
    r19=r13;
    r20=r13;
    r21=r13;
    for(r18=r14;r18<(over18?r14:r17);r18++){
  uint r23=3u,r29=1u,r32=2u;
  float r66=0.0f,r67=0.0f,r68=0.0f;
  if(r12!=0.0f) {
    long r24=(as_type<long>(ulong((long(r23)==0?0:(long(buffer3[r18*1+0]))/long(r23)))<<1)>>1);
    sb|=(r23==0)?2u:0u;
    uint r25=(uint(r24)*r23);
    uint r27=(uint((long(buffer1[r25*1+0])))*r23);
    float r28=buffer0[r27*1+0];
    float r31=buffer0[((r27+r29))*1+0];
    float r34=buffer0[((r27+r32))*1+0];
    uint r37=(uint((long(buffer1[((r25+r29))*1+0])))*r23);
    float r43=((buffer0[r37*1+0])-r28);
    float r44=((buffer0[((r37+r29))*1+0])-r31);
    float r45=((buffer0[((r37+r32))*1+0])-r34);
    uint r48=(uint((long(buffer1[((r25+r32))*1+0])))*r23);
    float r54=((buffer0[r48*1+0])-r28);
    float r55=((buffer0[((r48+r29))*1+0])-r31);
    float r56=((buffer0[((r48+r32))*1+0])-r34);
    r66=(r19+((((r44*r56))-((r45*r55)))));
    r67=(r20+((((r45*r54))-((r43*r56)))));
    r68=(r21+((((r43*r55))-((r44*r54)))));
  }
  float next18_0=r66;
  float next18_1=r67;
  float next18_2=r68;
  r19=next18_0;
  r20=next18_1;
  r21=next18_2;
    }
    float r70=r19;
    float r71=r20;
    float r72=r21;
    float r78=sqrt(fabs(((((((r70*r70))+((r71*r71))))+((r72*r72))))));
    r80=(r78<=(as_type<float>(0x219392efu)));
    if(r80==0.0f) {
      r82=(r78==0.0f?0.0f:r70/r78);
      r83=(r78==0.0f?0.0f:r71/r78);
      r84=(r78==0.0f?0.0f:r72/r78);
    }
    uint r89=(uint(r0)*r88);
    uint r90=(buffer4[r89]=((r80!=0.0f?r81:r82)),1u);
    uint r92=(buffer4[((r89+r15))]=((r80!=0.0f?r81:r83)),1u);
    uint r96=(buffer4[((r89+r94))]=((r80!=0.0f?r81:r84)),1u);
    r97=(((r90+r92))+r96);
  }
  uint r98=(r12!=0.0f?r97:r8);
  (void)float(r98);
}
