#include <metal_stdlib>
using namespace metal;
struct Out { float4 position [[position]];
#ifdef RAYS_POINTS
float point_size [[point_size]];
#endif
float4 color; float2 uv; float3 world; float3 normal; };
inline float scene_double(const device uchar *p){uint lo=*reinterpret_cast<const device uint*>(p);uint hi=*reinterpret_cast<const device uint*>(p+4);ulong bits=(ulong(hi)<<32)|ulong(lo);float sign=(hi>>31)==0?1.:-1.;int exponent=int((bits>>52)&0x7fful);ulong fraction=bits&0xffffffffffffful;if(exponent==0)return sign*ldexp(float(fraction)/4503599627370496.,-1022);return sign*ldexp(1.+float(fraction)/4503599627370496.,exponent-1023);}
inline float4 scene_mul(const device float *m,float4 v){return float4(dot(v,float4(m[0],m[1],m[2],m[3])),dot(v,float4(m[4],m[5],m[6],m[7])),dot(v,float4(m[8],m[9],m[10],m[11])),dot(v,float4(m[12],m[13],m[14],m[15])));}
vertex Out scene_vertex(uint i [[vertex_id]],uint instance_id [[instance_id]],const device uchar *input [[buffer(0)]],const device float *surface [[buffer(6)]],const device float *instances [[buffer(7)]],const device uchar *attributes [[buffer(11)]]){bool packed=surface[82]>.5;const device uchar*p=input+i*(packed?24:68);const device float*f=reinterpret_cast<const device float*>(p);const device float*transform=surface[83]>.5?instances+instance_id*48:surface;Out v;float4 local=packed?float4(f[0],f[1],f[2],1.):float4(scene_double(p),scene_double(p+8),scene_double(p+16),1.);float4 normal=packed?float4(f[3],f[4],f[5],0.):float4(scene_double(p+24),scene_double(p+32),scene_double(p+40),0.);v.world=scene_mul(transform+16,local).xyz;v.normal=normalize(scene_mul(transform+32,normal).xyz);v.position=scene_mul(transform,local);
#ifdef RAYS_POINTS
v.point_size=1.;
#endif
const device uchar*a=packed?attributes+i*12:p+48;v.color=unpack_unorm4x8_to_float(*reinterpret_cast<const device uint*>(a)).abgr;if(packed){const device float*uv=reinterpret_cast<const device float*>(a+4);v.uv=float2(uv[0],uv[1]);}else v.uv=float2(scene_double(p+52),scene_double(p+60));return v;}
inline void scene_light(Out v,const device float*l,thread float3&direction,thread float&strength){strength=l[8];if(l[0]<.5){direction=normalize(-float3(l[1],l[2],l[3]));}else if(l[0]<2.5){float3 delta=float3(l[1],l[2],l[3])-v.world;float distance=length(delta);direction=distance>0.?delta/distance:float3(0.);float attenuation=l[0]<1.5?l[9]+l[10]*distance+l[11]*distance*distance:l[15]+l[16]*distance+l[17]*distance*distance;strength=attenuation>0.?strength/attenuation:0.;if(l[0]>=1.5){float cosine=dot(-direction,normalize(float3(l[9],l[10],l[11])));strength*=cosine<l[13]?0.:pow(max(cosine,0.),l[14]);}}else{float3 axis=normalize(float3(l[9],l[10],l[11]));float3 reference=abs(axis.z)<.999?float3(0.,0.,1.):float3(0.,1.,0.);float3 u=normalize(cross(reference,axis));float3 w=cross(axis,u);int side=max(1,int(round(sqrt(l[14]))));float3 weighted=float3(0.);float total=0.;for(int row=0;row<side;row++)for(int column=0;column<side;column++){float a=(float(column)+.5)/float(side)-.5,b=(float(row)+.5)/float(side)-.5;float3 delta=float3(l[1],l[2],l[3])+a*l[12]*u+b*l[13]*w-v.world;float distance=length(delta);if(distance>0.){float3 ray=delta/distance;float facing=max(dot(axis,-ray),0.);float attenuation=l[15]+l[16]*distance+l[17]*distance*distance;float weight=attenuation>0.?l[8]*facing/(float(side*side)*attenuation):0.;weighted+=ray*weight;total+=weight;}}direction=length(weighted)>0.?normalize(weighted):float3(0.);strength=total;}}
inline float4 scene_surface(Out v,const device float*p,float4 tex,bool front,thread const float*visibility,bool shadowed){float3 n=normalize(v.normal);if(p[74]>.5&&!front)n=-n;float3 view=normalize(float3(p[48],p[49],p[50])-v.world);float3 primary=float3(p[64],p[65],p[66])+float3(p[52],p[53],p[54])*float3(p[69],p[70],p[71]);float3 spec=float3(0.);int count=min(int(p[73]),64);for(int i=0;i<count;i++){const device float*l=p+84+i*20;float3 direction;float strength;scene_light(v,l,direction,strength);if(shadowed)strength*=visibility[i];float diffuse=max(dot(n,direction),0.)*strength;primary+=float3(p[56],p[57],p[58])*float3(l[4],l[5],l[6])*diffuse;if(diffuse>0.){float3 halfv=normalize(direction+view);float shine=pow(max(dot(n,halfv),0.),p[68])*strength;spec+=float3(p[60],p[61],p[62])*float3(l[4],l[5],l[6])*shine;}}float3 modulation=v.color.rgb*tex.rgb;float3 rgb=p[75]>.5?primary*modulation+spec:(primary+spec)*modulation;return float4(rgb,p[59]*v.color.a*tex.a);}

inline float d_ggx(float noh, float a2) {
  float d = noh * noh * (a2 - 1.0f) + 1.0f;
  return a2 / (M_PI_F * d * d);
}
inline float v_smith(float nov, float nol, float a2) {
  float gv = nol * sqrt(nov * nov * (1.0f - a2) + a2);
  float gl = nov * sqrt(nol * nol * (1.0f - a2) + a2);
  return 0.5f / max(gv + gl, 1e-6f);
}
inline float3 fresnel(float3 f0, float voh) {
  float f = pow(1.0f - voh, 5.0f);
  return f0 + (1.0f - f0) * f;
}
// ks scales the specular lobe (see the mirror blend in scene_fragment).
inline float3 eval_brdf(float3 ns, float3 v, float3 l, float3 diffuse, float3 f0, float a2, float ks) {
  float3 h = normalize(l + v);
  float nol = max(dot(ns, l), 1e-4f), nov = max(dot(ns, v), 1e-4f);
  float noh = max(dot(ns, h), 0.0f), voh = max(dot(v, h), 1e-4f);
  float3 F = fresnel(f0, voh);
  return diffuse * (1.0f - F) / M_PI_F + ks * d_ggx(noh, a2) * v_smith(nov, nol, a2) * F;
}
inline uchar4 resolved_rgba(float3 linear, float exposure) {
  float3 c = linear * exposure;
  c = (c * (2.51f * c + 0.03f)) / (c * (2.43f * c + 0.59f) + 0.14f);
  c = pow(clamp(c, 0.0f, 1.0f), 1.0f / 2.2f);
  return uchar4(uchar(c.x * 255.0f + 0.5f), uchar(c.y * 255.0f + 0.5f),
                uchar(c.z * 255.0f + 0.5f), 255);
}
inline float4 world_output(float3 linear,float exposure,float alpha){return float4(float4(resolved_rgba(linear,exposure)).rgb/255.0f,alpha);}
inline float2 world_uv(float3 d){return float2(atan2(d.x,-d.z)/(2.*M_PI_F)+.5,acos(clamp(d.y,-1.,1.))/M_PI_F);}
inline float3 world_irradiance(const device float*w,float3 n){float x=n.x,y=n.y,z=n.z;float b[9]={0.282095,0.488603*y,0.488603*z,0.488603*x,1.092548*x*y,1.092548*y*z,0.315392*(3.*z*z-1.),1.092548*x*z,0.546274*(x*x-y*y)};float3 e=float3(0.);for(int i=0;i<9;i++)e+=float3(w[i*3],w[i*3+1],w[i*3+2])*b[i];return max(e,0.);}
// Karis' analytic split-sum environment BRDF (mobile approximation).
inline float3 env_brdf(float3 f0,float roughness,float nov){const float4 c0=float4(-1.,-.0275,-.572,.022);const float4 c1=float4(1.,.0425,1.04,-.04);float4 r=roughness*c0+c1;float a004=min(r.x*r.x,exp2(-9.28*nov))*r.x+r.y;float2 ab=float2(-1.04,1.04)*a004+r.zw;return f0*ab.x+ab.y;}
inline float world_map_depth(float4 c){float3 b=round(c.rgb*255.);return(b.x*65536.+b.y*256.+b.z)/16777215.;}
inline float world_shadow(Out v,const device float*w,texture2d<float> sun_map){if(w[39]<.5)return 1.;float4 clip=scene_mul(w+40,float4(v.world,1.));if(clip.w<=0.)return 1.;
  float2 uv=float2(clip.x/clip.w*.5+.5,.5-clip.y/clip.w*.5);float z=clip.z/clip.w;if(any(uv<0.)||any(uv>1.)||z<0.||z>1.)return 1.;
  float facing=clamp(dot(normalize(v.normal),float3(w[28],w[29],w[30])),0.,1.);float compare=z-(w[56]+w[57]*(1.-facing));
  int width=int(w[60]),height=int(w[61]),radius=int(w[59]);int2 c=int2(floor(uv*float2(width,height)));float visible=0.,count=0.;
  for(int y=-radius;y<=radius;y++)for(int x=-radius;x<=radius;x++){int2 q=c+int2(x,y);count+=1.;visible+=(q.x<0||q.y<0||q.x>=width||q.y>=height||compare<=(w[39]>1.5?world_map_depth(sun_map.read(uint2(q))):w[64+q.y*width+q.x]))?1.:0.;}
  return(1.-w[58])+w[58]*visible/count;}
// Kind 4: an extracted World rect, centre 1..3, half-extent axes 9..11 and
// 12..14, emitting normal 15..17, 2x2 samples, inverse-square falloff.
inline void world_light(Out v,const device float*l,thread float3&direction,thread float&strength){if(l[0]<3.5){scene_light(v,l,direction,strength);return;}
  float3 u=float3(l[9],l[10],l[11]),w=float3(l[12],l[13],l[14]),axis=float3(l[15],l[16],l[17]);float3 weighted=float3(0.);float total=0.;
  for(int row=0;row<2;row++)for(int column=0;column<2;column++){float3 delta=float3(l[1],l[2],l[3])+(float(column)-.5)*u+(float(row)-.5)*w-v.world;float d2=dot(delta,delta);if(d2>0.){float3 ray=delta*rsqrt(d2);float weight=l[8]*max(dot(axis,-ray),0.)/(4.*d2);weighted+=ray*weight;total+=weight;}}
  direction=length(weighted)>0.?normalize(weighted):float3(0.);strength=total;}
fragment float4 scene_fragment(Out value [[stage_in]],texture2d<float> image [[texture(1)]],sampler sampling [[sampler(2)]],const device float*w [[buffer(3)]],texture2d<float> specular [[texture(4)]],sampler specular_sampler [[sampler(5)]],const device float*p [[buffer(6)]],texture2d<float> sun_map [[texture(8)]],texture2d<float> environment [[texture(9)]],sampler environment_sampler [[sampler(10)]],bool front [[front_facing]]){
  float exposure=w[27];
  if(p[76]>1.5){float4 a=scene_mul(p+84,float4(value.world.xy,0.,1.)),b=scene_mul(p+84,float4(value.world.xy,1.,1.));float3 d=normalize(b.xyz/b.w-a.xyz/a.w);
    float3 radiance=w[35]>.5?float3(w[36],w[37],w[38]):image.sample(sampling,world_uv(d)).rgb;return world_output(radiance,exposure,1.);}
  float4 tex=image.sample(sampling,value.uv);float3 modulation=value.color.rgb*tex.rgb;float alpha=p[59]*value.color.a*tex.a;
  if(p[76]>.5)return float4(float3(p[64],p[65],p[66])*modulation,alpha);
  float3 n=normalize(value.normal);if(p[74]>.5&&!front)n=-n;float3 v=normalize(float3(p[48],p[49],p[50])-value.world);
  float3 albedo=pow(float3(p[56],p[57],p[58])*modulation,2.2);float rough=max(p[77],.03),a=rough*rough,a2=a*a;float3 f0=float3(p[78]);
  float nov=max(dot(n,v),1e-4);float3 spec=env_brdf(f0,rough,nov);
  // Below roughness 0.2 the reflection blends toward the full-resolution
  // camera map (emitters and sun disc included), so the analytic sun and
  // rect highlights fade by the same weight to avoid counting them twice.
  float ks=saturate(p[77]/.2);float2 r=world_uv(reflect(-v,n));
  float3 reflection=specular.sample(specular_sampler,r,level(max(rough,.2)*5.)).rgb;
  if(ks<1.)reflection=mix(environment.sample(environment_sampler,r).rgb,reflection,ks);
  float3 color=albedo/M_PI_F*world_irradiance(w,n)*(1.-spec)+reflection*spec;
  color+=pow(float3(p[64],p[65],p[66])*modulation,2.2);
  if(w[31]>.5){float3 l=float3(w[28],w[29],w[30]);float nol=dot(n,l);if(nol>0.)color+=eval_brdf(n,v,l,albedo,f0,a2,ks)*float3(w[32],w[33],w[34])*nol*world_shadow(value,w,sun_map);}
  int count=min(int(p[73]),64);for(int i=0;i<count;i++){const device float*l=p+84+i*20;float3 direction;float strength;world_light(value,l,direction,strength);float nol=dot(n,direction);
    if(nol>0.&&strength>0.)color+=eval_brdf(n,v,direction,albedo,f0,a2,ks)*float3(l[4],l[5],l[6])*strength*M_PI_F*nol;}
  return world_output(color,exposure,alpha);
}
