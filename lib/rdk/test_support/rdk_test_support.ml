(* The helpers the rdk and sop tests share. A test file opens this module and keeps
   only the helpers that are its own. *)
module Box_generator = Box_generator
module Uv_sphere = Uv_sphere
module Plane_generators = Plane_generators
module Parametric_generators = Parametric_generators

open Rdk

let fail message = raise (Failure message)
let check condition message = if not condition then fail message
let get_ok = function Ok value -> value | Error error -> fail (Error.to_string error)
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)
let get = function Ok value -> value | Error message -> fail message
let get_string = function Ok value -> value | Error message -> fail message
let get_string_ok = function Ok value -> value | Error message -> fail message

let contains text pattern =
  let rec search offset =
    offset + String.length pattern <= String.length text
    && (String.sub text offset (String.length pattern) = pattern || search (offset + 1)) in
  pattern = "" || search 0

let near ?(epsilon = 1e-10) left right = abs_float (left -. right) <= epsilon

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s" code
      (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let positions geometry = Packed.Float3.View.view (Geometry.positions geometry)

(* [Analysis.bounds] was pruned from the library; the tests keep an independent
   bounding box of the points under its old name. *)
module Analysis = struct
  include Analysis
  type bounds = { min : Rays_math.Vec3.t; max : Rays_math.Vec3.t;
                  center : Rays_math.Vec3.t; size : Rays_math.Vec3.t }
  let bounds geometry =
    let p = positions geometry in
    if Array.length p.x = 0 then None else
    let lo a = Array.fold_left Float.min infinity a
    and hi a = Array.fold_left Float.max neg_infinity a in
    let open Rays_math in
    let min = Rays_math.Vec3.create (lo p.x) (lo p.y) (lo p.z)
    and max = Rays_math.Vec3.create (hi p.x) (hi p.y) (hi p.z) in
    Some { min; max;
      center = Rays_math.Vec3.create ((min.x +. max.x) /. 2.) ((min.y +. max.y) /. 2.)
          ((min.z +. max.z) /. 2.);
      size = Rays_math.Vec3.create (max.x -. min.x) (max.y -. min.y) (max.z -. min.z) }
end

let same_float_array left right =
  Array.length left = Array.length right
  && let same = ref true in
     for index = 0 to Array.length left - 1 do
       if Int64.bits_of_float left.(index) <> Int64.bits_of_float right.(index)
       then same := false
     done;
     !same

let compare_storage floats left right = match Attribute.storage left, Attribute.storage right with
  | Attribute.Float left, Attribute.Float right -> floats left right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      floats left.x right.x && floats left.y right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      floats left.x right.x && floats left.y right.y && floats left.z right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      floats left.x right.x && floats left.y right.y && floats left.z right.z
      && floats left.w right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && floats left.values right.values
  | _ -> false

let equal_storage left right = compare_storage ( = ) left right

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

(* Exact authored bytes, excluding allocation IDs and derived caches. *)
let geometry_bytes geometry =
  let encode value = Marshal.to_string value [Marshal.No_sharing] in
  let attribute attribute =
    let storage = match Attribute.storage attribute with
      | Float values -> encode values | Int values -> encode values | Text values -> encode values
      | Int_array values -> encode (Packed.Int_array.View.view values)
      | Float_array values -> encode (Packed.Float_array.View.view values)
      | Float2 values -> let v = Packed.Float2.View.view values in encode (v.x, v.y)
      | Float3 values -> let v = Packed.Float3.View.view values in encode (v.x, v.y, v.z)
      | Float4 values -> let v = Packed.Float4.View.view values in encode (v.x, v.y, v.z, v.w) in
    Attribute.owner attribute, Attribute.name attribute, Attribute.kind_name attribute, storage in
  let group group = Group.owner group, Group.name group,
    Array.init (Group.length group) (fun i -> Group.mem i group), Group.ordered_elements group in
  let edge_group group = Edge_group.name group,
    Array.init (Edge_group.length group) (fun i -> Edge_group.mem i group) in
  let p = Packed.Float3.View.view (Geometry.positions geometry) in
  encode (p.x, p.y, p.z, Topology.View.view (Geometry.topology geometry),
    List.map attribute (Geometry.attributes geometry), List.map group (Geometry.groups geometry),
    List.map edge_group (Geometry.edge_groups geometry))

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.length left = Group.length right
  && Group.ordered_elements left = Group.ordered_elements right
  && begin
    let equal = ref true in
    for element = 0 to Group.length left - 1 do
      if Group.mem element left <> Group.mem element right then equal := false
    done;
    !equal
  end

let equal_edge_group left right =
  String.equal (Edge_group.name left) (Edge_group.name right)
  && Edge_group.length left = Edge_group.length right
  && begin
    let equal = ref true in
    for edge = 0 to Edge_group.length left - 1 do
      if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
    done;
    !equal
  end

let compare_geometry floats left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right)
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  floats lp.x rp.x && floats lp.y rp.y && floats lp.z rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && compare_storage floats left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let equal_geometry left right = compare_geometry ( = ) left right

(* ULPs are measured at the larger magnitude, including across binade boundaries.
   Subnormal spacing is always 2^-1074; signed zero compares equal here, while
   [geometry_bytes] remains the bit-exact self-consistency gate. *)
let close_geometry ~ulps left right =
  if ulps < 0 then invalid_arg "close_geometry: negative ulps";
  let close left right =
    left = right || (Float.is_finite left && Float.is_finite right
      && let _, exponent = Float.frexp (Float.max (abs_float left) (abs_float right)) in
         let unit = Float.ldexp 1. (max (-1074) (exponent - 53)) in
         abs_float (left -. right) /. unit <= float_of_int ulps) in
  compare_geometry (fun left right ->
    Array.length left = Array.length right && Array.for_all2 close left right) left right

(* Polygon topologies for these fixtures: the builder offers polylines only. *)
(* Primitives in the order added, polygons and polylines mixed. *)
module Tb = struct
  type t = { point_count : int; mutable primitives : (Topology.primitive_kind * int array) list }
  let create ~point_count () = { point_count; primitives = [] }
  let add kind t points = t.primitives <- (kind, points) :: t.primitives
  let add_triangle t a b c = add Topology.Polygon t [| a; b; c |]
  let add_polygon t points = add Topology.Polygon t points
  let add_open_polyline t points = add Topology.Open_polyline t points
  let add_closed_polyline t points = add Topology.Closed_polyline t points
  let freeze t =
    let primitives = List.rev t.primitives in
    let offsets = List.fold_left (fun acc (_, p) -> (List.hd acc + Array.length p) :: acc) [ 0 ] primitives in
    Topology.create_owned ~point_count:t.point_count
      ~vertex_points:(Array.concat (List.map snd primitives))
      ~primitive_offsets:(Array.of_list (List.rev offsets))
      ~primitive_kinds:(Array.of_list (List.map fst primitives)) |> Result.get_ok
end

let equal_positions left right =
  let left = Packed.Float3.View.view (Geometry.positions left)
  and right = Packed.Float3.View.view (Geometry.positions right) in
  left.x = right.x && left.y = right.y && left.z = right.z

let near_array expected actual =
  Array.length expected = Array.length actual
  && Array.for_all2 (fun expected actual -> abs_float (expected -. actual) <= 1e-12)
       expected actual

let float_array_values ~owner ~name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float_array values -> Packed.Float_array.View.view values
       | _ -> fail ("unexpected non-float-array storage for " ^ name))
  | None -> fail ("missing float-array attribute " ^ name)



(* [Triangulate2d.run] with the defaults tests rely on; the SOP declaration owns the real ones. *)
let triangulate2d ?cancel ?(grain = 16_384) ?selection ?constraint_edges
    ?constraint_primitives ?(projection = Triangulate2d.Best_fit) ?(seed = 0L)
    ?(split_crossing_constraints = false) ?(flood_from_hull_boundary = false)
    ?(remove_outside_constraint_polygons = false)
    ?(silhouette_constraints = false) ?(remove_outside_silhouette = false)
    ?(ignore_non_constraint_points = false) ?(remove_duplicate_points = false)
    ?(refine = false) ?(allow_constraint_splitting = true)
    ?(minimum_angle = Float.pi /. 9.) ?maximum_area ?target_edge_length
    ?(minimum_edge_length = 0.) ?(maximum_new_points = 100_000)
    ?(regularization_steps = 0) ?(allow_movement_of_interior_input_points = false)
    ?(preserve_point_payload = true) ?(restore_original_point_positions = true)
    ?(keep_primitives = false) ?split_point_group ?refinement_point_group
    ?triangle_group ?constraint_group ?(remove_unused_points = false)
    ?(recompute_point_normals = false) geometry =
  Triangulate2d.run ?cancel
    { grain; selection; constraint_edges; constraint_primitives; projection; seed;
      split_crossing_constraints; flood_from_hull_boundary;
      remove_outside_constraint_polygons; silhouette_constraints;
      remove_outside_silhouette; ignore_non_constraint_points;
      remove_duplicate_points; refine; allow_constraint_splitting; minimum_angle;
      maximum_area; target_edge_length; minimum_edge_length; maximum_new_points;
      regularization_steps; allow_movement_of_interior_input_points;
      preserve_point_payload; restore_original_point_positions; keep_primitives;
      split_point_group; refinement_point_group; triangle_group; constraint_group;
      remove_unused_points; recompute_point_normals }
    geometry

(* [Ray.run] with the defaults tests rely on; the SOP declaration owns the real ones. *)
let ray ?cancel ?(grain = 16_384) ?selection ?collision_primitives
    ?(method_ = Ray.Ray_project) ?(direction = Ray.Ray_normal)
    ?(direction_mode = Ray.Ray_forward) ?(surface_hit = Ray.Ray_first_surface)
    ?(samples = 1) ?(jitter_scale = 1.) ?(seed = 0) ?(combine = Ray.Ray_average)
    ?(min_distance = 0.) ?max_distance ?(tolerance = 0.) ?(scale = 1.)
    ?(lift = 0.) ?distance_attribute ?primitive_attribute
    ?source_vertex_numbers_attribute ?source_vertex_weights_attribute
    ?hit_group ?normal_attribute ?point_pattern ?vertex_pattern
    ?primitive_pattern ?detail_pattern ?(match_groups = false) ~source ~collision () =
  Ray.run ?cancel
    { grain; selection; collision_primitives; method_; direction; direction_mode;
      surface_hit; samples; jitter_scale; seed; combine; min_distance;
      max_distance; tolerance; scale; lift; distance_attribute; primitive_attribute;
      source_vertex_numbers_attribute; source_vertex_weights_attribute; hit_group;
      normal_attribute; point_pattern; vertex_pattern; primitive_pattern;
      detail_pattern; match_groups }
    ~source ~collision ()

(* A deterministic height field on Y: a rough surface fixture for parallel-exactness tests. *)
let displace_y ~amplitude ~frequency ~seed geometry =
  let p = positions geometry in
  let y = Array.mapi (fun i y -> y +. amplitude *. sin (frequency *. (p.x.(i) *. 12.9898 +. p.z.(i) *. 78.233) +. float seed)) p.y in
  Geometry.with_positions (Packed.Float3.View.of_owned_exn ~x:(Array.copy p.x) ~y ~z:(Array.copy p.z)) geometry

(* [Rdk.Transform_ops] plus the whole-geometry [transform] the scene fixtures use. *)
module Transform_ops = struct
  include Rdk.Transform_ops
  let normalized_direction ?(grain = 16_384) matrix values =
    let source = Packed.Float3.View.view values in
    let count = Array.length source.x in
    let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
    let (m00,m01,m02,_), (m10,m11,m12,_), (m20,m21,m22,_), _ =
      Rays_math.Mat4.to_rows matrix in
    if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
        (fun index ->
          let vx = source.x.(index) and vy = source.y.(index)
          and vz = source.z.(index) in
          let ox = m00 *. vx +. m01 *. vy +. m02 *. vz
          and oy = m10 *. vx +. m11 *. vy +. m12 *. vz
          and oz = m20 *. vx +. m21 *. vy +. m22 *. vz in
          let length = sqrt ((ox *. ox) +. (oy *. oy) +. (oz *. oz)) in
          if length > 1e-20 then begin
            x.(index) <- ox /. length;
            y.(index) <- oy /. length;
            z.(index) <- oz /. length
          end);
    Packed.Float3.View.of_owned_exn ~x ~y ~z

  let transform ?grain matrix geometry =
    let transformed =
      let source = Packed.Float3.View.view (Geometry.positions geometry) in
      let count = Array.length source.x in
      let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
      let (m00,m01,m02,m03), (m10,m11,m12,m13),
          (m20,m21,m22,m23), (m30,m31,m32,m33) = Rays_math.Mat4.to_rows matrix in
      if count > 0 then Parallel.for_ ~chunk_size:(Option.value grain ~default:16_384) ~start:0
          ~finish:(count - 1) (fun index ->
        let vx = source.x.(index) and vy = source.y.(index) and vz = source.z.(index) in
        let ox = m00*.vx +. m01*.vy +. m02*.vz +. m03
        and oy = m10*.vx +. m11*.vy +. m12*.vz +. m13
        and oz = m20*.vx +. m21*.vy +. m22*.vz +. m23
        and ow = m30*.vx +. m31*.vy +. m32*.vz +. m33 in
        if abs_float ow <= 1e-12 then begin
          x.(index) <- ox; y.(index) <- oy; z.(index) <- oz
        end else begin
          x.(index) <- ox /. ow; y.(index) <- oy /. ow; z.(index) <- oz /. ow
        end);
      Geometry.with_positions (Packed.Float3.View.of_owned_exn ~x ~y ~z) geometry |> Result.get_ok in
    match Rays_math.Mat4.inverse matrix with
    | None -> transformed
        |> Geometry.without_attribute ~owner:Attribute.Point "N"
        |> Geometry.without_attribute ~owner:Attribute.Vertex "N"
    | Some inverse ->
        let normal_matrix = Rays_math.Mat4.transpose inverse in
        List.fold_left (fun output owner ->
          match Geometry.find_attribute ~owner "N" geometry with
          | None -> output
          | Some attribute ->
              (match Attribute.get (Attribute.normal ~owner) attribute with
               | None -> output
               | Some normals ->
                   let normals = normalized_direction ?grain normal_matrix normals in
                   let attribute = Attribute.create_key_owned
                       (Attribute.normal ~owner) normals |> get_string_ok in
                   Geometry.with_attribute attribute output |> get_string_ok))
          transformed [Attribute.Point; Attribute.Vertex]
end

let float3_values attribute = match Attribute.View.storage attribute with
  | Attribute.Float3 values -> Some values | _ -> None

(* The exact-edge counts of a topology index, over the public incidence counts. *)
module Topology_index = struct
  include Rdk.Topology_index
  let count_edges index keep =
    let count = ref 0 in
    for edge = 0 to edge_count index - 1 do
      if keep (edge_incidence_count index edge) then incr count
    done; !count
  let boundary_edge_count index = count_edges index (fun n -> n = 1)
  let non_manifold_edge_count index = count_edges index (fun n -> n > 2)
end

(* Optional-argument fronts over the parameter records of the rdk operations. *)
module Fuse_grid = struct
  include Rdk.Fuse_grid
  let fuse ?cancel ?(grain = 16_384) ?selection ?target_selection
      ?(targeting = Near_points) ?(using = Least_target_point)
      ?(tolerance = 1e-6) ?(position = Rdk_mesh.Fuse_reduce.Average_position)
      ?weight_attribute ?(attributes = Rdk_mesh.Fuse_reduce.Keep_first) ?(attribute_rules = [])
      ?(group_rules = []) ?(metric = Euclidean)
      ?(inclusive = true)
      ?(match_attributes = false) ?radius_attribute ?match_attribute
      ?(match_condition = Equal_attribute_values) ?(match_tolerance = 0.)
      ?(modify_target = false) ?(fuse_points = true)
      ?(keep_fused_points = false) ?snapped_group
      ?snapped_destination_attribute ?(remove_degenerate_primitives = false)
      ?(remove_unused_points_from_degenerate_primitives = false)
      ?(remove_all_unused_points = false) ?target geometry =
    Rdk.Fuse_grid.fuse { cancel; grain; selection; target_selection; targeting; using; tolerance; position; weight_attribute; attributes; attribute_rules; group_rules; metric; inclusive; match_attributes; radius_attribute; match_attribute; match_condition; match_tolerance; modify_target; fuse_points; keep_fused_points; snapped_group; snapped_destination_attribute; remove_degenerate_primitives; remove_unused_points_from_degenerate_primitives; remove_all_unused_points; target } geometry
end

module Subdivide = struct
  include Rdk.Subdivide
  let subdivide ?cancel ?grain ?(scheme = Catmull_clark) ?(iterations = 1)
      ?primitives ?(cracks = Subdivide_do_not_close)
      ?(consistent_topology = false) ?creases ?crease_primitives ?crease_weight
      ?(generate_resulting_creases = true) ?resulting_crease_group
      ?hole_primitives ?(remove_holes = true)
      ?(boundary_interpolation = Subdivide_boundary_edge_only)
      ?(face_varying_interpolation = Subdivide_fvar_all)
      ?triangle_policy:(triangle_subdivision = Subdivide_triangles_catmull_clark)
      ?(creasing_method = Subdivide_creasing_uniform)
      ?(treat_curves_as_independent = false)
      ?(recompute_point_normals = false) geometry =
    Rdk.Subdivide.subdivide {
      cancel; grain; scheme; iterations; primitives; cracks; consistent_topology; creases; crease_primitives; crease_weight; generate_resulting_creases; resulting_crease_group; hole_primitives; remove_holes; boundary_interpolation; face_varying_interpolation; triangle_policy = triangle_subdivision; creasing_method; treat_curves_as_independent; recompute_point_normals } geometry
end

module Boolean = struct
  include Rdk.Boolean
  let run ?cancel ?(grain = 16_384) ?(operation = Union)
      ?(left_treatment = Solid) ?(right_treatment = Solid)
      ?(resolve_left_self_intersections = false)
      ?(resolve_right_self_intersections = false)
      ?(point_conflict = Promote_to_vertex) ?(point_tolerance = 0.)
      ?(tiny_seam_threshold = 0.) ?(cleanup_max_batches = 8)
      ?(strict_cleanup = true)
      ?(seam_points = Shared_seam_points) ?(detriangulation = Triangles)
      ?(assume_flat = false) ?require_closed
      ?piece_attribute
      ?(left_piece_group = Some "boolean_left")
      ?(overlap_piece_group = Some "boolean_overlap")
      ?(right_piece_group = Some "boolean_right") ~right left =
    Rdk.Boolean.run {
      cancel; grain; operation; left_treatment; right_treatment; resolve_left_self_intersections; resolve_right_self_intersections; point_conflict; point_tolerance; tiny_seam_threshold; cleanup_max_batches; strict_cleanup; seam_points; detriangulation; assume_flat; require_closed; piece_attribute; left_piece_group; overlap_piece_group; right_piece_group; right } left
end

module Clean = struct
  include Rdk.Clean
  let run ?cancel ?(grain = 16_384) ?(epsilon = 1e-12)
      ?(remove_degenerate = true) ?consolidate_distance ?overlaps
      ?(reverse_winding = false) ?(remove_nan_points = false)
      ?(remove_unused_points = false) ?(delete_unused_groups = false)
      ?point_attributes ?vertex_attributes ?primitive_attributes ?detail_attributes
      ?point_groups ?vertex_groups ?primitive_groups ?edge_groups
      geometry =
    Rdk.Clean.run {
      cancel; grain; epsilon; remove_degenerate; consolidate_distance; overlaps; reverse_winding; remove_nan_points; remove_unused_points; delete_unused_groups; point_attributes; vertex_attributes; primitive_attributes; detail_attributes; point_groups; vertex_groups; primitive_groups; edge_groups } geometry
end

module Facet = struct
  include Rdk.Facet
  let run ?cancel ?grain ?selection ?pre_compute_normals
      ?make_normals_unit_length ?unique_points ?consolidate_distance
      ?consolidate_normals_distance ?remove_inline_points ?inline_distance
      ?orient_polygons ?cusp_angle ?remove_degenerate ?make_planar
      ?post_compute_normals ?reverse_normals geometry =
    Rdk.Facet.run {
      cancel; grain; selection; pre_compute_normals; make_normals_unit_length; unique_points; consolidate_distance; consolidate_normals_distance; remove_inline_points; inline_distance; orient_polygons; cusp_angle; remove_degenerate; make_planar; post_compute_normals; reverse_normals } geometry
end

module Remesh = struct
  include Rdk.Remesh
  let run ?cancel ?(grain = 16_384) ?(iterations = 3) ?(smoothing = 0.5)
      ?(project = true) ?(use_input_points_only = false) ?hard_points ?hard_edges
      ?target_size_attribute ?(preserve_uv_seams = true) ?(uv_attribute = "uv")
      ?output_hard_edges ?output_mesh_size ?output_quality
      ?(recompute_point_normals = true) ~target_length geometry =
    Rdk.Remesh.run {
      cancel; grain; iterations; smoothing; project; use_input_points_only; hard_points; hard_edges; target_size_attribute; preserve_uv_seams; uv_attribute; output_hard_edges; output_mesh_size; output_quality; recompute_point_normals; target_length } geometry
end

module Plane_clip = struct
  include Rdk.Plane_clip
  let clip ?cancel ?(grain = 16_384) ?(keep = Above)
      ?(snapping_tolerance = 1e-9) ?(fill = false)
      ?(split_connectivity = false) ?(clip_attribute = "P") ?(distance = 0.)
      ?selection ?(replace_existing_groups = true) ?clipped_edge_group ?cap_group
      ?clipped_group ?above_group ?below_group ~origin ~normal geometry =
    Rdk.Plane_clip.clip {
      cancel; grain; keep; snapping_tolerance; fill; split_connectivity; clip_attribute; distance; selection; replace_existing_groups; clipped_edge_group; cap_group; clipped_group; above_group; below_group; origin; normal } geometry
end

module Attribute_ops = struct
  include Rdk.Attribute_ops
  let interpolate ?cancel ?grain ?selection ?driver ?compute_weights
      ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
      ?(match_groups = false)
      ?pre_scale ?normalize_weights ?threshold ?blend
      ?(unmatched = Keep_target) ~target_owner ~attributes ~source ~target () =
    Rdk.Attribute_ops.interpolate {
      cancel; grain; selection; driver; compute_weights; point_pattern; vertex_pattern; primitive_pattern; detail_pattern; match_groups; pre_scale; normalize_weights; threshold; blend; unmatched; target_owner; attributes; source; target } ()
end
