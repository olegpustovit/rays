#include <metal_stdlib>
using namespace metal;
float fade(float t) { return t*t*t*(t*(t*6.0f-15.0f)+10.0f); }
float blend(float a, float b, float t) { return a+t*(b-a); }
float gradient(int h, float x, float y, float z) {
  h &= 15;
  float u=h<8?x:y, v=h<4?y:((h==12||h==14)?x:z);
  return ((h&1)==0?u:-u)+((h&2)==0?v:-v);
}
float noise(float3 p, device const int* table) {
  if(!all(isfinite(p))) return as_type<float>(0x7fc00000u);
  float3 base=floor(p), q=p-base, f=float3(fade(q.x),fade(q.y),fade(q.z));
  int x=int(fmod(base.x,256.0f))&255, y=int(fmod(base.y,256.0f))&255, z=int(fmod(base.z,256.0f))&255;
  int a=table[x]+y, b=table[x+1]+y;
  int aa=table[a]+z, ab=table[a+1]+z, ba=table[b]+z, bb=table[b+1]+z;
  float x00=blend(gradient(table[aa],q.x,q.y,q.z),gradient(table[ba],q.x-1,q.y,q.z),f.x);
  float x10=blend(gradient(table[ab],q.x,q.y-1,q.z),gradient(table[bb],q.x-1,q.y-1,q.z),f.x);
  float x01=blend(gradient(table[aa+1],q.x,q.y,q.z-1),gradient(table[ba+1],q.x-1,q.y,q.z-1),f.x);
  float x11=blend(gradient(table[ab+1],q.x,q.y-1,q.z-1),gradient(table[bb+1],q.x-1,q.y-1,q.z-1),f.x);
  return clamp((blend(blend(x00,x10,f.y),blend(x01,x11,f.y),f.z)+1.0f)*0.5f,0.0f,1.0f);
}
float fbm(float3 p, int octaves, device const int* table) {
  float frequency=1.0f, amplitude=1.0f, sum=0.0f, weights=0.0f;
  for(int octave=0;octave<octaves;octave++) {
    sum+=noise(p*frequency,table)*amplitude; weights+=amplitude;
    frequency*=2.0f; amplitude*=0.5f;
  }
  return weights==0.0f?0.0f:sum/weights;
}
kernel void kernel_27273fd871c3de55fcf1d3f270adef74(
  device const float* input0 [[buffer(0)]],
  device float* output [[buffer(1)]],
  constant uint* uniforms [[buffer(2)]],
  device atomic_uint* status [[buffer(3)]],
  device const int* table [[buffer(4)]],
  uint i [[thread_position_in_grid]]) {
  if(i>=uniforms[0]) return;
  uint sb=0u;
  float r0=input0[i*1+0];
  sb|=!isfinite(r0);
  float r2=(r0*(as_type<float>(0x3ca3d70au)));
  sb|=!isfinite(r2);
  float r3=as_type<float>(uniforms[1]);
  sb|=!isfinite(r3);
  float r5=fbm(float3(r2,r3,(as_type<float>(0x3e800000u))),3,table+0);
  sb|=!isfinite(r5);
  if(sb!=0u||i==0) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);
  output[i*1+0]=r5;
}
