#include <metal_stdlib>
using namespace metal;
kernel void kernel_d2ebb1ebefd46c44f7f92f796969e36f(
  device const float* input0 [[buffer(0)]],
  device float* output [[buffer(1)]],
  constant uint* uniforms [[buffer(2)]],
  device atomic_uint* status [[buffer(3)]],
  device const int* table [[buffer(4)]],
  uint i [[thread_position_in_grid]]) {
  if(i>=uniforms[0]) return;
  uint sb=0u;
  float r4=as_type<float>(0x3e800000u);
  uint r5=3u;
  float r6=as_type<float>(0x0u),r7=as_type<float>(0x3f800000u);
  uint r8=0u,r9=0;
  float r10=0,r11=0,r12=0,r13=0;
  float r0=input0[i*1+0];
  sb|=!isfinite(r0);
  float r2=(r0*(as_type<float>(0x3ca3d70au)));
  sb|=!isfinite(r2);
  float r3=as_type<float>(uniforms[1]);
  sb|=!isfinite(r3);
  bool over9=long(r5)>long(r8) && ulong(long(r5)-long(r8))>4096uL;
  sb|=over9?1u:0u;
  r10=r6;
  r11=r6;
  r12=r7;
  r13=r7;
  for(r9=r8;r9<(over9?r8:r5);r9++){
  float r18=as_type<float>(0x7f800000u);
  uint r35=255u,r41=1u;
  float r57=as_type<float>(0x40c00000u),r59=as_type<float>(0x41700000u),r62=as_type<float>(0x41200000u),r79=as_type<float>(0x3f800000u);
  uint r84=15u,r86=8u,r89=12u,r91=14u,r95=4u;
  float r98=as_type<float>(0xbf800000u);
  uint r102=0u,r105=2u;
  float r273=0.0f;
  float r14=(r2*r12);
  float r15=(r3*r12);
  float r16=(r4*r12);
  float r25=((((fabs(r14))<r18))!=0.0f&&(((((fabs(r15))<r18))!=0.0f&&(((fabs(r16))<r18))!=0.0f))!=0.0f);
  if(r25!=0.0f) {
    float r28=(r14-(floor(r14)));
    float r31=(r15-(floor(r15)));
    float r34=(r16-(floor(r16)));
    uint r36=(uint((long(floor(r14))))&r35);
    uint r37=(uint((long(floor(r15))))&r35);
    uint r38=(uint((long(floor(r16))))&r35);
    uint r40=((uint(table[0+r36]))+r37);
    uint r44=((uint(table[0+((r36+r41))]))+r37);
    uint r46=((uint(table[0+r40]))+r38);
    uint r49=((uint(table[0+((r40+r41))]))+r38);
    uint r51=((uint(table[0+r44]))+r38);
    uint r54=((uint(table[0+((r44+r41))]))+r38);
    float r64=(((((r28*r28))*r28))*((((r28*((((r28*r57))-r59))))+r62)));
    float r71=(((((r31*r31))*r31))*((((r31*((((r31*r57))-r59))))+r62)));
    float r80=(r28-r79);
    float r81=(r31-r79);
    float r82=(r34-r79);
    uint r85=((uint(table[0+r46]))&r84);
    float r88=(((long(r85)<long(r86)?1.0f:0.0f))!=0.0f?r28:r31);
    float r97=(((long(r85)<long(r95)?1.0f:0.0f))!=0.0f?r31:((((((long(r85)==long(r89)?1.0f:0.0f))!=0.0f||((long(r85)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r28:r34)));
    float r109=(((((long(((r85&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r88:((r98*r88))))+((((long(((r85&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r97:((r98*r97)))));
    uint r111=((uint(table[0+r51]))&r84);
    float r113=(((long(r111)<long(r86)?1.0f:0.0f))!=0.0f?r80:r31);
    float r119=(((long(r111)<long(r95)?1.0f:0.0f))!=0.0f?r31:((((((long(r111)==long(r89)?1.0f:0.0f))!=0.0f||((long(r111)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r80:r34)));
    float r131=(r109+((r64*((((((((long(((r111&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r113:((r98*r113))))+((((long(((r111&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r119:((r98*r119))))))-r109)))));
    uint r133=((uint(table[0+r49]))&r84);
    float r135=(((long(r133)<long(r86)?1.0f:0.0f))!=0.0f?r28:r81);
    float r141=(((long(r133)<long(r95)?1.0f:0.0f))!=0.0f?r81:((((((long(r133)==long(r89)?1.0f:0.0f))!=0.0f||((long(r133)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r28:r34)));
    float r150=(((((long(((r133&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r135:((r98*r135))))+((((long(((r133&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r141:((r98*r141)))));
    uint r152=((uint(table[0+r54]))&r84);
    float r154=(((long(r152)<long(r86)?1.0f:0.0f))!=0.0f?r80:r81);
    float r160=(((long(r152)<long(r95)?1.0f:0.0f))!=0.0f?r81:((((((long(r152)==long(r89)?1.0f:0.0f))!=0.0f||((long(r152)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r80:r34)));
    uint r175=((uint(table[0+((r46+r41))]))&r84);
    float r177=(((long(r175)<long(r86)?1.0f:0.0f))!=0.0f?r28:r31);
    float r183=(((long(r175)<long(r95)?1.0f:0.0f))!=0.0f?r31:((((((long(r175)==long(r89)?1.0f:0.0f))!=0.0f||((long(r175)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r28:r82)));
    float r192=(((((long(((r175&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r177:((r98*r177))))+((((long(((r175&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r183:((r98*r183)))));
    uint r195=((uint(table[0+((r51+r41))]))&r84);
    float r197=(((long(r195)<long(r86)?1.0f:0.0f))!=0.0f?r80:r31);
    float r203=(((long(r195)<long(r95)?1.0f:0.0f))!=0.0f?r31:((((((long(r195)==long(r89)?1.0f:0.0f))!=0.0f||((long(r195)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r80:r82)));
    float r215=(r192+((r64*((((((((long(((r195&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r197:((r98*r197))))+((((long(((r195&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r203:((r98*r203))))))-r192)))));
    uint r218=((uint(table[0+((r49+r41))]))&r84);
    float r220=(((long(r218)<long(r86)?1.0f:0.0f))!=0.0f?r28:r81);
    float r226=(((long(r218)<long(r95)?1.0f:0.0f))!=0.0f?r81:((((((long(r218)==long(r89)?1.0f:0.0f))!=0.0f||((long(r218)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r28:r82)));
    float r235=(((((long(((r218&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r220:((r98*r220))))+((((long(((r218&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r226:((r98*r226)))));
    uint r238=((uint(table[0+((r54+r41))]))&r84);
    float r240=(((long(r238)<long(r86)?1.0f:0.0f))!=0.0f?r80:r81);
    float r246=(((long(r238)<long(r95)?1.0f:0.0f))!=0.0f?r81:((((((long(r238)==long(r89)?1.0f:0.0f))!=0.0f||((long(r238)==long(r91)?1.0f:0.0f))!=0.0f))!=0.0f?r80:r82)));
    float r261=(r131+((r71*((((r150+((r64*((((((((long(((r152&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r154:((r98*r154))))+((((long(((r152&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r160:((r98*r160))))))-r150))))))-r131)))));
    r273=min((max(((((((r261+((((((((r34*r34))*r34))*((((r34*((((r34*r57))-r59))))+r62))))*((((r215+((r71*((((r235+((r64*((((((((long(((r238&r41)))==long(r102)?1.0f:0.0f))!=0.0f?r240:((r98*r240))))+((((long(((r238&r105)))==long(r102)?1.0f:0.0f))!=0.0f?r246:((r98*r246))))))-r235))))))-r215))))))-r261))))))+r79))*(as_type<float>(0x3f000000u)))),(as_type<float>(0x0u)))),r79);
  }
  float next9_0=((r10+((((r25!=0.0f?r273:(as_type<float>(0x7fc00000u))))*r13))));
  float next9_1=((r11+r13));
  float next9_2=((r12*(as_type<float>(0x40000000u))));
  float next9_3=((r13*(as_type<float>(0x3f000000u))));
  r10=next9_0;
  r11=next9_1;
  r12=next9_2;
  r13=next9_3;
  }
  float r288=((r11)==0.0f?0.0f:(r10)/(r11));
  sb|=!isfinite(r288);
  if(sb!=0u||i==0) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);
  output[i*1+0]=r288;
}
