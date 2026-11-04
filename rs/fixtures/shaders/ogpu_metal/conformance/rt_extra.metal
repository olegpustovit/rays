#include <metal_stdlib>
#include <metal_raytracing>
using namespace metal;
using namespace raytracing;
struct Hit { uint hit; uint primitive; uint instance; float t; float2 barycentrics; };
struct Sphere { float4 center_radius; };
inline bool sphere_hit(ray r, Sphere s, thread float &t) {
  float3 oc = r.origin - s.center_radius.xyz; float b = dot(oc, r.direction); float cc = dot(oc, oc) - s.center_radius.w * s.center_radius.w; float disc = b * b - cc;
  if (disc < 0.0f) return false; t = -b - sqrt(disc); return t >= r.min_distance && t <= r.max_distance; }
struct Capsule { float4 a_radius; float4 b_radius; };
inline bool capsule_hit(ray r, Capsule c, thread float &t) {
  float3 ba = c.b_radius.xyz - c.a_radius.xyz; float3 oa = r.origin - c.a_radius.xyz;
  float baba = dot(ba, ba), bard = dot(ba, r.direction), baoa = dot(ba, oa);
  float rd2 = dot(r.direction, r.direction), rdoa = dot(r.direction, oa);
  float aa = baba * rd2 - bard * bard;
  float bb = baba * rdoa - baoa * bard;
  float cc = baba * dot(oa, oa) - baoa * baoa - c.a_radius.w * c.a_radius.w * baba;
  float disc = bb * bb - aa * cc; if (disc < 0.0f || aa <= 1e-8f) return false;
  float candidate = (-bb - sqrt(disc)) / aa; float y = baoa + candidate * bard;
  if (y > 0.0f && y < baba) { t = candidate; return t >= r.min_distance && t <= r.max_distance; }
  float3 oc = y <= 0.0f ? oa : r.origin - c.b_radius.xyz;
  float b = dot(r.direction, oc), cap_disc = b * b - rd2 * (dot(oc, oc) - c.a_radius.w * c.a_radius.w);
  if (cap_disc < 0.0f) return false; t = (-b - sqrt(cap_disc)) / rd2;
  return t >= r.min_distance && t <= r.max_distance; }
inline ray make_ray(const device float4 *rays, uint tid) { ray r; r.origin = rays[tid * 2].xyz; r.direction = rays[tid * 2 + 1].xyz; r.min_distance = 0.0f; r.max_distance = 100.0f; return r; }
kernel void trace_boxes(primitive_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], const device Sphere *spheres [[buffer(3)]], uint tid [[thread_position_in_grid]]) {
  ray r = make_ray(rays, tid); intersection_params params; intersection_query<> query(r, scene, params);
  while (query.next()) { float t; if (query.get_candidate_intersection_type() == intersection_type::bounding_box && sphere_hit(r, spheres[query.get_candidate_primitive_id()], t)) query.commit_bounding_box_intersection(t); }
  bool hit = query.get_committed_intersection_type() == intersection_type::bounding_box;
  hits[tid].hit = hit ? 1u : 0u; hits[tid].primitive = hit ? query.get_committed_primitive_id() : 0u;
  hits[tid].instance = 0u; hits[tid].t = hit ? query.get_committed_distance() : 0.0f;
  hits[tid].barycentrics = float2(0.0f); }
kernel void trace_capsules(primitive_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], const device Capsule *capsules [[buffer(3)]], uint tid [[thread_position_in_grid]]) {
  ray r = make_ray(rays, tid); intersection_params params; intersection_query<> query(r, scene, params);
  while (query.next()) { float t; uint p = query.get_candidate_primitive_id();
    if (query.get_candidate_intersection_type() == intersection_type::bounding_box && capsule_hit(r, capsules[p], t))
      query.commit_bounding_box_intersection(t); }
  bool hit = query.get_committed_intersection_type() == intersection_type::bounding_box;
  hits[tid].hit = hit ? 1u : 0u; hits[tid].primitive = hit ? query.get_committed_primitive_id() : 0u;
  hits[tid].instance = 0u; hits[tid].t = hit ? query.get_committed_distance() : 0.0f;
  hits[tid].barycentrics = float2(0.0f); }
kernel void trace_curves(primitive_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], uint tid [[thread_position_in_grid]]) {
  intersector<curve_data> query; auto hit = query.intersect(make_ray(rays, tid), scene);
  bool found = hit.type == intersection_type::curve; hits[tid].hit = found ? 1u : 0u;
  hits[tid].primitive = found ? hit.primitive_id : 0u; hits[tid].instance = 0u;
  hits[tid].t = found ? hit.distance : 0.0f; hits[tid].barycentrics = float2(0.0f); }
kernel void trace_motion(acceleration_structure<primitive_motion> scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], constant float &time [[buffer(3)]], uint tid [[thread_position_in_grid]]) {
  intersector<triangle_data, primitive_motion> query; auto hit = query.intersect(make_ray(rays, tid), scene, time);
  bool found = hit.type == intersection_type::triangle; hits[tid].hit = found ? 1u : 0u;
  hits[tid].primitive = found ? hit.primitive_id : 0u; hits[tid].instance = 0u;
  hits[tid].t = found ? hit.distance : 0.0f; hits[tid].barycentrics = found ? hit.triangle_barycentric_coord : float2(0.0f); }
kernel void trace_masked(instance_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], constant uint &mask [[buffer(3)]], uint tid [[thread_position_in_grid]]) {
  intersector<triangle_data, instancing> query; auto hit = query.intersect(make_ray(rays, tid), scene, mask);
  bool found = hit.type == intersection_type::triangle; hits[tid].hit = found ? 1u : 0u;
  hits[tid].primitive = found ? hit.primitive_id : 0u; hits[tid].instance = found ? hit.user_instance_id : 0u;
  hits[tid].t = found ? hit.distance : 0.0f; hits[tid].barycentrics = found ? hit.triangle_barycentric_coord : float2(0.0f); }
kernel void trace_motion_instances(acceleration_structure<instancing, instance_motion> scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], constant float &time [[buffer(3)]], uint tid [[thread_position_in_grid]]) {
  intersector<triangle_data, instancing, instance_motion> query; auto hit = query.intersect(make_ray(rays, tid), scene, time);
  bool found = hit.type == intersection_type::triangle; hits[tid].hit = found ? 1u : 0u;
  hits[tid].primitive = found ? hit.primitive_id : 0u; hits[tid].instance = found ? hit.user_instance_id : 0u;
  hits[tid].t = found ? hit.distance : 0.0f; hits[tid].barycentrics = found ? hit.triangle_barycentric_coord : float2(0.0f); }