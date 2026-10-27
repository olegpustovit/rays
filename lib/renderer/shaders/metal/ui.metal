#include <metal_stdlib>
using namespace metal;
struct UiOut { float4 position [[position]]; float4 color; float4 color2; float2 local;
  float2 uv; float4 box [[flat]]; float4 params [[flat]]; float4 extra [[flat]]; };
inline float2 bezier(float2 p0,float2 p1,float2 p2,float2 p3,float t){float s=1.-t;
  return s*s*s*p0+3.*s*s*t*p1+3.*s*t*t*p2+t*t*t*p3;}
inline float4 unpack(uint value){return unpack_unorm4x8_to_float(value).abgr;}
vertex UiOut scene_vertex(uint vid [[vertex_id]],const device uchar *input [[buffer(0)]],const device float *affine [[buffer(6)]]){
  uint instance=vid/4,corner=vid%4;const device float *f=reinterpret_cast<const device float*>(input+instance*64);
  const device uint *u=reinterpret_cast<const device uint*>(input+instance*64);
  uint kind=u[10];float2 c=float2((corner==1||corner==2)?1.:0.,corner>=2?1.:0.);
  UiOut o;o.color=unpack(u[8]);o.color2=unpack(u[9]);o.uv=float2(0.);
  o.params=float4(f[11],f[12],f[13],float(kind));o.extra=float4(f[4],f[5],f[6],f[7]);
  float2 p;
  if(kind==2u||kind==5u){float2 a=float2(f[0],f[1]),b=float2(f[2],f[3]),d=float2(f[4],f[5]),e=float2(f[6],f[7]);
    float2 p0=kind==5u?a:bezier(a,b,d,e,f[14]),p1=kind==5u?b:bezier(a,b,d,e,f[15]);float2 dir=p1-p0;float len=length(dir);
    dir=len>1e-6?dir/len:float2(1.,0.);float2 n=float2(-dir.y,dir.x);float h=f[12]*.5+1.5;
    p=mix(p0-dir*h,p1+dir*h,c.x)+n*(c.y*2.-1.)*h;o.extra=float4(p0,p1);o.box=float4(0.);}
  else{float4 r=float4(f[0],f[1],f[2],f[3]);float grow=0.;
    if(kind==0u||kind==4u){grow=f[13]>0.?1.+f[12]*.5:(f[12]>0.?f[12]*.5:0.);}
    r+=float4(-grow,-grow,grow,grow);p=mix(r.xy,r.zw,c);o.box=float4(f[0],f[1],f[2],f[3]);
    if(kind==1u)o.uv=mix(float2(f[4],f[5]),float2(f[6],f[7]),c);}
  o.local=p;o.position=float4(affine[0]*p.x+affine[1]*p.y+affine[2],affine[3]*p.x+affine[4]*p.y+affine[5],0.,1.);return o;}
inline float round_box(float2 p,float2 half_size,float radius){float2 q=abs(p)-half_size+radius;
  return length(max(q,0.))+min(max(q.x,q.y),0.)-radius;}
inline float4 over(float4 top,float4 bottom){float a=top.a+bottom.a*(1.-top.a);
  if(a<=0.)return float4(0.);return float4((top.rgb*top.a+bottom.rgb*bottom.a*(1.-top.a))/a,a);}
fragment float4 scene_fragment(UiOut in [[stage_in]],texture2d<float> atlas [[texture(1)]],sampler sampling [[sampler(2)]]){
  uint kind=uint(in.params.w+.5);float2 p=in.local;
  if(kind==1u)return in.color*atlas.sample(sampling,in.uv/float2(atlas.get_width(),atlas.get_height()));
  if(kind==2u||kind==5u){float2 a=in.extra.xy,b=in.extra.zw,ab=b-a;float t=clamp(dot(p-a,ab)/max(dot(ab,ab),1e-12),0.,1.);
    float d=length(p-(a+ab*t))-in.params.y*.5;
    if(kind==5u){float len=length(ab);if(len<=1e-6)discard_fragment();float2 dir=ab/len;
      float2 q=p-(a+b)*.5;d=round_box(float2(dot(q,dir),dot(q,float2(-dir.y,dir.x))),float2(len*.5,in.params.y*.5),0.);}
    float px=max(length(fwidth(p)),1e-4);
    float coverage=clamp(.5-d/px,0.,1.);return float4(in.color.rgb,in.color.a*coverage);}
  if(kind==3u){float2 q=p-in.extra.xy;float s=in.extra.z,dot_size=in.extra.w;
    float2 cell=q-s*floor(q/s),dist=abs(cell-dot_size*.5);
    dist=min(dist,s-dist);float2 aa=max(fwidth(p),float2(1e-4));
    float2 coverage=clamp((dot_size*.5-dist)/aa+.5,0.,1.);
    return float4(in.color.rgb,in.color.a*coverage.x*coverage.y);}
  float4 box=in.box;float radius=in.params.x,border=in.params.y;
  if(in.params.z<=0.){
    if(border>0.){float h=border*.5;float2 q=p+1e-3;if(q.x>=box.x+h&&q.x<box.z-h&&q.y>=box.y+h&&q.y<box.w-h)discard_fragment();
      return in.color2;}
    return in.color;}
  float2 center=(box.xy+box.zw)*.5,half_size=(box.zw-box.xy)*.5;
  float d=kind==4u?length(p-center)-half_size.x:round_box(p-center,half_size,min(radius,min(half_size.x,half_size.y)));
  float px=max(length(fwidth(p))*.70710678,1e-4);
  float4 fill=float4(in.color.rgb,in.color.a*clamp(.5-d/px,0.,1.));
  if(border<=0.)return fill;
  float4 edge=float4(in.color2.rgb,in.color2.a*clamp(.5-(abs(d)-border*.5)/px,0.,1.));
  return over(edge,fill);}
