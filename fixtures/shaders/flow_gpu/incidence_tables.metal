#include <metal_stdlib>
using namespace metal;
kernel void incidence_tables(
    device const uint* buffer0 [[buffer(0)]],
    device const uint* buffer1 [[buffer(1)]],
    device const uint* buffer2 [[buffer(2)]],
    device const uint* buffer3 [[buffer(3)]],
    device uint* buffer4 [[buffer(4)]],
    device uint* buffer5 [[buffer(5)]],
    device uint* buffer6 [[buffer(6)]],
    device uint* buffer7 [[buffer(7)]],
    device const uint* buffer8 [[buffer(8)]],
    uint gid [[thread_position_in_grid]],
    uint lid [[thread_position_in_threadgroup]]) {
  uint sb=0u;
  uint r11=0u,r18=0u,r20=0;
  long r21=0,r22=0;
  uint r39=0u,r43=0u,r45=0;
  long r46=0,r47=0;
  uint r66=1u,r80=0u;
  long r0=long(gid);
  long r14=long(buffer8[(1u)*1+0]);
  float r17=(r0<=(long(buffer8[r11*1+0]))?1.0f:0.0f);
  if(r17!=0.0f) {
    r21=long(r18);
    r22=r14;
    for(r20=r18;r20<(32u);r20++){
  float r23=0.0f;
  uint r25=2u;
  float r29=0.0f;
  uint r30=1u;
  long r31=0,r32=0,r33=0,r34=0,r35=0;
  if(r17!=0.0f) {
    r23=(r21<r22?1.0f:0.0f);
    if(r23!=0.0f) {
      long r26=(as_type<long>(ulong((long(r25)==0?0:((as_type<long>(ulong((ulong(r21)+ulong(r22)))<<1)>>1))/long(r25)))<<1)>>1);
      sb|=(r25==0)?2u:0u;
      r29=((long(buffer0[(long(buffer3[r26*1+0]))*1+0]))<r0?1.0f:0.0f);
      if(r29!=0.0f) {
        r31=(as_type<long>(ulong((ulong(r26)+ulong(long(r30))))<<1)>>1);
      }
      r32=(r29!=0.0f?r31:r21);
      r33=(r29!=0.0f?r22:r26);
    }
    r34=(r23!=0.0f?r32:r21);
    r35=(r23!=0.0f?r33:r22);
  }
  long next20_0=r34;
  long next20_1=r35;
  r21=next20_0;
  r22=next20_1;
    }
    r39=(buffer4[uint(r0)]=uint((r21)),1u);
  }
  float r41=(r0<r14?1.0f:0.0f);
  if(r41!=0.0f) {
    long r42=long(buffer3[r0*1+0]);
    r46=long(r43);
    r47=(long(buffer8[(2u)*1+0]));
    for(r45=r43;r45<(32u);r45++){
  float r48=0.0f;
  uint r50=2u,r52=1u;
  float r55=0.0f;
  uint r56=1u;
  long r57=0,r58=0,r59=0,r60=0,r61=0;
  if(r41!=0.0f) {
    r48=(r46<r47?1.0f:0.0f);
    if(r48!=0.0f) {
      long r51=(as_type<long>(ulong((long(r50)==0?0:((as_type<long>(ulong((ulong(r46)+ulong(r47)))<<1)>>1))/long(r50)))<<1)>>1);
      sb|=(r50==0)?2u:0u;
      r55=((long(buffer1[((uint(r51)+r52))*1+0]))<=r42?1.0f:0.0f);
      if(r55!=0.0f) {
        r57=(as_type<long>(ulong((ulong(r51)+ulong(long(r56))))<<1)>>1);
      }
      r58=(r55!=0.0f?r57:r46);
      r59=(r55!=0.0f?r47:r51);
    }
    r60=(r48!=0.0f?r58:r46);
    r61=(r48!=0.0f?r59:r47);
  }
  long next45_0=r60;
  long next45_1=r61;
  r46=next45_0;
  r47=next45_1;
    }
    long r63=r46;
    long r65=long(buffer1[r63*1+0]);
    long r69=(as_type<long>(ulong((ulong((long(buffer1[((uint(r63)+r66))*1+0])))-ulong(r65)))<<1)>>1);
    uint r70=(buffer5[uint(r42)]=uint(r63),1u);
    uint r71=(buffer6[uint(r0)]=uint(r65),1u);
    uint r78=(buffer7[uint(r0)]=(((((((long(buffer2[r63*1+0]))==long(r43)?1.0f:0.0f))!=0.0f&&((r69>=long((3u))?1.0f:0.0f))!=0.0f))!=0.0f?uint(r69):r43)),1u);
    uint r79=(r71+r78);
    r80=(r70+r79);
  }
  uint r81=(r41!=0.0f?r80:r11);
  uint r82=(((r17!=0.0f?r39:r11))+r81);
  (void)float(r82);
}
