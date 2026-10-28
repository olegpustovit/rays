// Generated from image.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.
#include <metal_stdlib>
using namespace metal;
struct image_vertex_out {
  float4 position [[position]];
};
vertex image_vertex_out image_vertex(
    uint vid [[vertex_id]]) {
  image_vertex_out o;
  uint sb=0u;
  float r11=as_type<float>(0x0u),r12=as_type<float>(0x3f800000u);
  long r0=long(vid);
  float r2=(r0==long((1u))?1.0f:0.0f);
  float r5=(r2!=0.0f?(as_type<float>(0x40400000u)):(as_type<float>(0xbf800000u)));
  float r7=(r0==long((2u))?1.0f:0.0f);
  float r10=(r7!=0.0f?(as_type<float>(0x40400000u)):(as_type<float>(0xbf800000u)));
  o.position.x=r5;
  o.position.y=r10;
  o.position.z=r11;
  o.position.w=r12;
  return o;
}
fragment float4 image_fragment(
    image_vertex_out in [[stage_in]],
    device const float* buffer0 [[buffer(0)]],
    device const float* buffer1 [[buffer(1)]]) {
  float4 colour;
  uint sb=0u;
  uint r11=4u;
  float r14=as_type<float>(0x0u),r16=as_type<float>(0x3f800000u),r18=as_type<float>(0x437f0000u),r23=as_type<float>(0x3f000000u),r25=as_type<float>(0x3f800000u),r26=0.0f,r28=0.0f;
  uint r30=1u;
  float r33=0.0f,r34=0.0f;
  uint r37=1u;
  float r47=as_type<float>(0x3f800000u),r48=0.0f,r50=0.0f;
  uint r52=1u;
  float r55=0.0f,r56=0.0f;
  uint r59=2u;
  float r69=as_type<float>(0x3f800000u),r70=0.0f,r72=0.0f;
  uint r74=1u;
  float r77=0.0f,r78=0.0f;
  uint r81=3u;
  float r91=as_type<float>(0x3f800000u),r92=0.0f,r94=0.0f;
  uint r96=1u;
  float r99=0.0f,r100=0.0f;
  long r6=long((buffer1[(0u)*1+0]));
  long r9=long((in.position.x));
  uint r12=(((((uint((long((in.position.y))))*uint(r6)))+uint(r9)))*r11);
  float r19=((min((max((buffer0[r12*1+0]),r14)),r16))*r18);
  long r20=long(floor(r19));
  float r21=floor(r19);
  float r22=(r19-r21);
  float r24=(r22>r23);
  if(r24!=0.0f) {
    r26=float((r20+1L));
  }
  if(r24==0.0f) {
    r28=(r22<(as_type<float>(0x3f000000u)));
    if(r28==0.0f) {
      uint r31=(uint((r20))&r30);
      float r32=float(r31);
      r33=float((r20+long(r31)));
    }
    r34=(r28!=0.0f?r21:r33);
  }
  float r36=(r18==0.0f?0.0f:((r24!=0.0f?r26:r34))/r18);
  float r42=((min((max((buffer0[((r12+r37))*1+0]),r14)),r16))*r18);
  long r43=long(floor(r42));
  float r44=floor(r42);
  float r45=(r42-r44);
  float r46=(r45>r23);
  if(r46!=0.0f) {
    r48=float((r43+1L));
  }
  if(r46==0.0f) {
    r50=(r45<(as_type<float>(0x3f000000u)));
    if(r50==0.0f) {
      uint r53=(uint((r43))&r52);
      float r54=float(r53);
      r55=float((r43+long(r53)));
    }
    r56=(r50!=0.0f?r44:r55);
  }
  float r58=(r18==0.0f?0.0f:((r46!=0.0f?r48:r56))/r18);
  float r64=((min((max((buffer0[((r12+r59))*1+0]),r14)),r16))*r18);
  long r65=long(floor(r64));
  float r66=floor(r64);
  float r67=(r64-r66);
  float r68=(r67>r23);
  if(r68!=0.0f) {
    r70=float((r65+1L));
  }
  if(r68==0.0f) {
    r72=(r67<(as_type<float>(0x3f000000u)));
    if(r72==0.0f) {
      uint r75=(uint((r65))&r74);
      float r76=float(r75);
      r77=float((r65+long(r75)));
    }
    r78=(r72!=0.0f?r66:r77);
  }
  float r80=(r18==0.0f?0.0f:((r68!=0.0f?r70:r78))/r18);
  float r86=((min((max((buffer0[((r12+r81))*1+0]),r14)),r16))*r18);
  long r87=long(floor(r86));
  float r88=floor(r86);
  float r89=(r86-r88);
  float r90=(r89>r23);
  if(r90!=0.0f) {
    r92=float((r87+1L));
  }
  if(r90==0.0f) {
    r94=(r89<(as_type<float>(0x3f000000u)));
    if(r94==0.0f) {
      uint r97=(uint((r87))&r96);
      float r98=float(r97);
      r99=float((r87+long(r97)));
    }
    r100=(r94!=0.0f?r88:r99);
  }
  float r102=(r18==0.0f?0.0f:((r90!=0.0f?r92:r100))/r18);
  colour.x=r36;
  colour.y=r58;
  colour.z=r80;
  colour.w=r102;
  return colour;
}
