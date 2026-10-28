open Rdk_core
(** Explicit bridge to Rays's renderer mesh. Triangle-ready point position and normal planes and
    indices are shared without copying. Point and vertex [N], [Cd], and [uv] attributes are
    preserved; primitive [Cd] is expanded to corners when neither vertex nor point [Cd] is present.
    RGB float3 [Cd] gets opaque alpha; float4 retains alpha. Vertex-owned attributes cause
    deterministic corner expansion because Rays's render mesh has one attribute tuple per render
    vertex. Both values remain immutable. *)

val to_mesh : Geometry.t -> (Rays.Mesh.t, Error.t) result

val to_trace_mesh : Geometry.t -> (Rays.Mesh.t, Error.t) result
(** Triangulate a geometry, compute the path tracer's smoothed vertex normals, and return the packed
    triangle mesh it consumes. *)

val of_mesh : Rays.Mesh.t -> (Geometry.t, Error.t) result
