#include <metal_stdlib>
using namespace metal;
struct MarksOut { float4 position [[position]]; float4 color; float4 color2; float2 local;
  float4 box [[flat]]; float4 params [[flat]]; float4 extra [[flat]]; };
inline float4 unpack(uint value){return unpack_unorm4x8_to_float(value).abgr;}
inline uint channel(float c){return uint(floor(clamp(c,0.,1.)*255.+.5));}
inline uint colour(const device uint *words,uint at,uint mode){
  if(mode==0u)return words[at];
  const device float *f=reinterpret_cast<const device float*>(words);
  return (channel(f[at])<<24)|(channel(f[at+1])<<16)|(channel(f[at+2])<<8)|255u;}
vertex MarksOut scene_vertex(uint vid [[vertex_id]],
    const device float *position [[buffer(0)]],const device float *extent [[buffer(1)]],
    const device uint *fill [[buffer(2)]],const device uint *stroke [[buffer(3)]],
    const device float *width [[buffer(4)]],const device uint *q [[buffer(5)]],
    const device float *affine [[buffer(6)]]){
  uint i=vid/4,corner=vid%4;float2 c=float2((corner==1||corner==2)?1.:0.,corner>=2?1.:0.);
  uint kind=q[0];
  float2 a=float2(position[i*q[1]],position[i*q[1]+1]);
  float sw=width[i*q[5]];
  uint stroke_word=colour(stroke,i*q[4],q[7]);
  MarksOut o;
  if(kind==5u){
    float2 b=float2(extent[i*q[2]],extent[i*q[2]+1]);
    o.color=unpack(stroke_word);o.color2=float4(0.);
    o.params=float4(0.,sw,1.,5.);
    float2 dir=b-a;float len=length(dir);
    dir=len>1e-6?dir/len:float2(1.,0.);float2 n=float2(-dir.y,dir.x);float h=sw*.5+1.5;
    float2 p=mix(a-dir*h,b+dir*h,c.x)+n*(c.y*2.-1.)*h;o.extra=float4(a,b);o.box=float4(0.);
    o.local=p;o.position=float4(affine[0]*p.x+affine[1]*p.y+affine[2],affine[3]*p.x+affine[4]*p.y+affine[5],0.,1.);return o;}
  float4 r;
  if(kind==4u){float rad=extent[i*q[2]];r=float4(a.x-rad,a.y-rad,a.x+rad,a.y+rad);}
  else{float2 size=float2(extent[i*q[2]],extent[i*q[2]+1]);r=float4(a.x,a.y,a.x+size.x,a.y+size.y);}
  float border=stroke_word==0u?0.:sw;
  o.color=unpack(colour(fill,i*q[3],q[6]));o.color2=unpack(stroke_word);
  o.params=float4(0.,border,1.,float(kind));o.extra=float4(0.);
  float grow=1.+border*.5;
  float4 g=r+float4(-grow,-grow,grow,grow);float2 p=mix(g.xy,g.zw,c);o.box=r;
  o.local=p;o.position=float4(affine[0]*p.x+affine[1]*p.y+affine[2],affine[3]*p.x+affine[4]*p.y+affine[5],0.,1.);return o;}
inline float round_box(float2 p,float2 half_size,float radius){float2 q=abs(p)-half_size+radius;
  return length(max(q,0.))+min(max(q.x,q.y),0.)-radius;}
inline float4 over(float4 top,float4 bottom){float a=top.a+bottom.a*(1.-top.a);
  if(a<=0.)return float4(0.);return float4((top.rgb*top.a+bottom.rgb*bottom.a*(1.-top.a))/a,a);}
fragment float4 scene_fragment(MarksOut in [[stage_in]]){
  uint kind=uint(in.params.w+.5);float2 p=in.local;
  if(kind==5u){float2 a=in.extra.xy,b=in.extra.zw,ab=b-a;
    float len=length(ab);if(len<=1e-6)discard_fragment();float2 dir=ab/len;
    float2 q=p-(a+b)*.5;float d=round_box(float2(dot(q,dir),dot(q,float2(-dir.y,dir.x))),float2(len*.5,in.params.y*.5),0.);
    float px=max(length(fwidth(p)),1e-4);
    float coverage=clamp(.5-d/px,0.,1.);return float4(in.color.rgb,in.color.a*coverage);}
  float4 box=in.box;float border=in.params.y;
  float2 center=(box.xy+box.zw)*.5,half_size=(box.zw-box.xy)*.5;
  float d=kind==4u?length(p-center)-half_size.x:round_box(p-center,half_size,0.);
  float px=max(length(fwidth(p))*.70710678,1e-4);
  float4 fill=float4(in.color.rgb,in.color.a*clamp(.5-d/px,0.,1.));
  if(border<=0.)return fill;
  float4 edge=float4(in.color2.rgb,in.color2.a*clamp(.5-(abs(d)-border*.5)/px,0.,1.));
  return over(edge,fill);}
