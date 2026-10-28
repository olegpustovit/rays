// Generated from scene.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}
struct raster_texture { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
struct scene2_vertex_out {
  float4 position [[position]];
  float4 color;
  float2 uv;
};
vertex scene2_vertex_out scene2_vertex(
    uint vid [[vertex_id]],
    device const uint* buffer0 [[buffer(0)]],
    device const float* buffer1 [[buffer(1)]]) {
  scene2_vertex_out o;
  uint sb=0u;
  uint r3=17u,r6=1u,r9=31u,r11=0u,r16=20u,r18=2047u,r20=1048575u,r22=32u;
  float r26=as_type<float>(0x59800000u),r31=0.0f;
  uint r34=1023u;
  float r38=0.0f;
  uint r40=2u;
  float r60=0.0f;
  uint r63=1023u;
  float r67=0.0f,r84=as_type<float>(0x0u),r85=as_type<float>(0x3f800000u);
  uint r86=12u,r89=24u,r91=255u;
  float r94=as_type<float>(0x437f0000u);
  uint r96=16u,r101=8u,r109=13u;
  float r129=0.0f;
  uint r132=1023u;
  float r136=0.0f;
  uint r138=15u;
  float r158=0.0f;
  uint r161=1023u;
  float r165=0.0f;
  uint r4=(uint((long(vid)))*r3);
  long r5=long(buffer0[r4*1+0]);
  long r8=long(buffer0[((r4+r6))*1+0]);
  long r10=(as_type<long>(ulong(long((ulong(r8)&0x7fffffffffffffffUL)>>(long(r9)&63)))<<1)>>1);
  sb|=(r9>62u)?4u:0u;
  float r12=(r10==long(r11)?1.0f:0.0f);
  float r15=(r12!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
  long r17=(as_type<long>(ulong(long((ulong(r8)&0x7fffffffffffffffUL)>>(long(r16)&63)))<<1)>>1);
  sb|=(r16>62u)?4u:0u;
  uint r19=(uint(r17)&r18);
  long r23=(as_type<long>(ulong((ulong(long(((uint(r8)&r20))))<<(long(r22)&63)))<<1)>>1);
  sb|=(r22>62u)?4u:0u;
  float r27=(r26==0.0f?0.0f:(float(((as_type<long>(ulong((r23|r5))<<1)>>1))))/r26);
  float r28=(long(r19)==long(r11)?1.0f:0.0f);
  if(r28!=0.0f) {
    r31=(r15*(ldexp(r27,int((as_type<float>(0xc47f8000u))))));
  }
  if(r28==0.0f) {
    r38=(r15*(ldexp((((as_type<float>(0x3f800000u))+r27)),int((float(((as_type<long>(ulong((ulong(long(r19))-ulong(long(r34))))<<1)>>1))))))));
  }
  float r39=(r28!=0.0f?r31:r38);
  uint r41=(r4+r40);
  long r42=long(buffer0[r41*1+0]);
  long r44=long(buffer0[((r41+r6))*1+0]);
  long r45=(as_type<long>(ulong(long((ulong(r44)&0x7fffffffffffffffUL)>>(long(r9)&63)))<<1)>>1);
  sb|=(r9>62u)?4u:0u;
  float r46=(r45==long(r11)?1.0f:0.0f);
  float r49=(r46!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
  long r50=(as_type<long>(ulong(long((ulong(r44)&0x7fffffffffffffffUL)>>(long(r16)&63)))<<1)>>1);
  sb|=(r16>62u)?4u:0u;
  uint r51=(uint(r50)&r18);
  long r53=(as_type<long>(ulong((ulong(long(((uint(r44)&r20))))<<(long(r22)&63)))<<1)>>1);
  sb|=(r22>62u)?4u:0u;
  float r56=(r26==0.0f?0.0f:(float(((as_type<long>(ulong((r53|r42))<<1)>>1))))/r26);
  float r57=(long(r51)==long(r11)?1.0f:0.0f);
  if(r57!=0.0f) {
    r60=(r49*(ldexp(r56,int((as_type<float>(0xc47f8000u))))));
  }
  if(r57==0.0f) {
    r67=(r49*(ldexp((((as_type<float>(0x3f800000u))+r56)),int((float(((as_type<long>(ulong((ulong(long(r51))-ulong(long(r63))))<<1)>>1))))))));
  }
  float r68=(r57!=0.0f?r60:r67);
  float r74=((fma((buffer1[r11*1+0]),r39,(((buffer1[r6*1+0])*r68))))+(buffer1[r40*1+0]));
  float r83=((fma((buffer1[(3u)*1+0]),r39,(((buffer1[(4u)*1+0])*r68))))+(buffer1[(5u)*1+0]));
  long r88=long(buffer0[((r4+r86))*1+0]);
  long r90=(as_type<long>(ulong(long((ulong(r88)&0x7fffffffffffffffUL)>>(long(r89)&63)))<<1)>>1);
  sb|=(r89>62u)?4u:0u;
  float r95=(r94==0.0f?0.0f:(float(((uint(r90)&r91))))/r94);
  long r97=(as_type<long>(ulong(long((ulong(r88)&0x7fffffffffffffffUL)>>(long(r96)&63)))<<1)>>1);
  sb|=(r96>62u)?4u:0u;
  float r100=(r94==0.0f?0.0f:(float(((uint(r97)&r91))))/r94);
  long r102=(as_type<long>(ulong(long((ulong(r88)&0x7fffffffffffffffUL)>>(long(r101)&63)))<<1)>>1);
  sb|=(r101>62u)?4u:0u;
  float r105=(r94==0.0f?0.0f:(float(((uint(r102)&r91))))/r94);
  float r108=(r94==0.0f?0.0f:(float(((uint(r88)&r91))))/r94);
  uint r110=(r4+r109);
  long r111=long(buffer0[r110*1+0]);
  long r113=long(buffer0[((r110+r6))*1+0]);
  long r114=(as_type<long>(ulong(long((ulong(r113)&0x7fffffffffffffffUL)>>(long(r9)&63)))<<1)>>1);
  sb|=(r9>62u)?4u:0u;
  float r115=(r114==long(r11)?1.0f:0.0f);
  float r118=(r115!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
  long r119=(as_type<long>(ulong(long((ulong(r113)&0x7fffffffffffffffUL)>>(long(r16)&63)))<<1)>>1);
  sb|=(r16>62u)?4u:0u;
  uint r120=(uint(r119)&r18);
  long r122=(as_type<long>(ulong((ulong(long(((uint(r113)&r20))))<<(long(r22)&63)))<<1)>>1);
  sb|=(r22>62u)?4u:0u;
  float r125=(r26==0.0f?0.0f:(float(((as_type<long>(ulong((r122|r111))<<1)>>1))))/r26);
  float r126=(long(r120)==long(r11)?1.0f:0.0f);
  if(r126!=0.0f) {
    r129=(r118*(ldexp(r125,int((as_type<float>(0xc47f8000u))))));
  }
  if(r126==0.0f) {
    r136=(r118*(ldexp((((as_type<float>(0x3f800000u))+r125)),int((float(((as_type<long>(ulong((ulong(long(r120))-ulong(long(r132))))<<1)>>1))))))));
  }
  float r137=(r126!=0.0f?r129:r136);
  uint r139=(r4+r138);
  long r140=long(buffer0[r139*1+0]);
  long r142=long(buffer0[((r139+r6))*1+0]);
  long r143=(as_type<long>(ulong(long((ulong(r142)&0x7fffffffffffffffUL)>>(long(r9)&63)))<<1)>>1);
  sb|=(r9>62u)?4u:0u;
  float r144=(r143==long(r11)?1.0f:0.0f);
  float r147=(r144!=0.0f?(as_type<float>(0x3f800000u)):(as_type<float>(0xbf800000u)));
  long r148=(as_type<long>(ulong(long((ulong(r142)&0x7fffffffffffffffUL)>>(long(r16)&63)))<<1)>>1);
  sb|=(r16>62u)?4u:0u;
  uint r149=(uint(r148)&r18);
  long r151=(as_type<long>(ulong((ulong(long(((uint(r142)&r20))))<<(long(r22)&63)))<<1)>>1);
  sb|=(r22>62u)?4u:0u;
  float r154=(r26==0.0f?0.0f:(float(((as_type<long>(ulong((r151|r140))<<1)>>1))))/r26);
  float r155=(long(r149)==long(r11)?1.0f:0.0f);
  if(r155!=0.0f) {
    r158=(r147*(ldexp(r154,int((as_type<float>(0xc47f8000u))))));
  }
  if(r155==0.0f) {
    r165=(r147*(ldexp((((as_type<float>(0x3f800000u))+r154)),int((float(((as_type<long>(ulong((ulong(long(r149))-ulong(long(r161))))<<1)>>1))))))));
  }
  float r166=(r155!=0.0f?r158:r165);
  o.position.x=r74;
  o.position.y=r83;
  o.position.z=r84;
  o.position.w=r85;
  o.color.x=r95;
  o.color.y=r100;
  o.color.z=r105;
  o.color.w=r108;
  o.uv.x=r137;
  o.uv.y=r166;
  return o;
}
fragment float4 scene2_fragment(
    scene2_vertex_out in [[stage_in]],
    constant raster_texture& arguments2 [[buffer(2)]]) {
  float4 colour;
  texture2d<float> texture2=arguments2.image;
  sampler sampler3=arguments2.sampling;
  uint sb=0u;
  float r10=((in.color.x)*(texture2.sample(sampler3,float2((in.uv.x),(in.uv.y))).x));
  float r11=((in.color.y)*(texture2.sample(sampler3,float2((in.uv.x),(in.uv.y))).y));
  float r12=((in.color.z)*(texture2.sample(sampler3,float2((in.uv.x),(in.uv.y))).z));
  float r13=((in.color.w)*(texture2.sample(sampler3,float2((in.uv.x),(in.uv.y))).w));
  colour.x=r10;
  colour.y=r11;
  colour.z=r12;
  colour.w=r13;
  return colour;
}
