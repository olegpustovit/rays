open Rays
open Rdk
open Sop
open Rdk_test_support

let get_ok = function Ok value -> value | Error message -> fail message

(* Index lists and bounds as the Lisp text the group nodes read. *)
let ints indices = String.concat " " (List.map string_of_int (Array.to_list indices))

let bounds_group ~name ~min ~max =
  let open Rays_math.Vec3 in
  Printf.sprintf "(sop/group_bounds :name %S :center [%s %s %s] :size [%s %s %s])" name
    (Lisp_sop.float ((min.x +. max.x) *. 0.5))
    (Lisp_sop.float ((min.y +. max.y) *. 0.5))
    (Lisp_sop.float ((min.z +. max.z) *. 0.5))
    (Lisp_sop.float (max.x -. min.x))
    (Lisp_sop.float (max.y -. min.y))
    (Lisp_sop.float (max.z -. min.z))

(* Append SOP steps (Lisp text) to an existing node. *)
let then_ steps node =
  Lisp_sop.node ~with_:[ ("prev", node) ] (Printf.sprintf "(-> (sop/ext_prev) %s)" steps)

let group_indices ~owner ~name indices node =
  then_
    (Printf.sprintf "(sop/ordered_group :owner %S :name %S :elements %S)" owner name (ints indices))
    node

let group_all ~owner ~name node =
  then_
    (Printf.sprintf "(sop/group_range :owner %S :name %S :range_mode \"From ends\")" owner name)
    node

let group_in_bounds ~name ~min ~max node = then_ (bounds_group ~name ~min ~max) node

(* Points of a [sop/curve]: Lisp reads a long [[x y z] ...] as one vector, so use a list. *)
let curve_list points =
  let text = Lisp_sop.curve_points points in
  "(list " ^ String.sub text 1 (String.length text - 2) ^ ")"

let merge_of inputs =
  let names = List.mapi (fun index node -> (Printf.sprintf "m%d" index, node)) inputs in
  Lisp_sop.node ~with_:names
    (Printf.sprintf "(sop/merge %s)"
       (String.concat " " (List.map (fun (key, _) -> Printf.sprintf "(sop/ext_%s)" key) names)))

let with_detail name storage geometry =
  let attribute = Attribute.create_owned ~owner:Attribute.Detail ~name storage |> get_ok in
  Geometry.with_attribute attribute geometry |> get_ok

let equal_storage left right =
  match (Attribute.storage left, Attribute.storage right) with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z && left.w = right.w
  | _ -> false

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)
  && equal_storage left right

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.length left = Group.length right
  && begin
    let equal = ref true in
    for index = 0 to Group.length left - 1 do
      if Group.mem index left <> Group.mem index right then equal := false
    done;
    !equal
  end
  && Group.ordered_elements left = Group.ordered_elements right

let equal_geometry left right =
  let left_positions = Packed.Float3.View.view (Geometry.positions left)
  and right_positions = Packed.Float3.View.view (Geometry.positions right)
  and left_topology = Topology.View.view (Geometry.topology left)
  and right_topology = Topology.View.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left) (Geometry.edge_groups right)

let cook domains graph =
  let context = Context.create ~domains ~grain:97 ~seed:42L () |> get_ok in
  let session = Session.create ~max_entries:16 ~max_payload_bytes:(256 * 1024 * 1024) |> get_ok in
  let result =
    match Session.cook session ~context graph with
    | Ok output -> Result.get_ok (Sop.Payload.geometry output.payload)
    | Error error -> fail (Diagnostic.error_to_string error)
  in
  Session.close session;
  result

let inline_geometry polygons =
  let point_count = polygons * 6 in
  let x = Array.make point_count 0.
  and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for primitive = 0 to polygons - 1 do
    let point = primitive * 6 and base = float_of_int primitive *. 4. in
    x.(point) <- base;
    x.(point + 1) <- base +. 1.;
    x.(point + 2) <- base +. 2.;
    x.(point + 3) <- base +. 3.;
    x.(point + 4) <- base +. 3.;
    z.(point + 4) <- 1.;
    x.(point + 5) <- base;
    z.(point + 5) <- 1.
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology =
    Topology.polygons_owned ~point_count ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (polygons + 1) (fun primitive -> primitive * 6))
    |> get_ok
  in
  Geometry.create ~positions ~topology () |> get_ok

let edge_flip_geometry pairs =
  let point_count = pairs * 4 and vertex_count = pairs * 6 in
  let x = Array.make point_count 0.
  and y = Array.make point_count 0.
  and z = Array.make point_count 0.
  and vertex_points = Array.make vertex_count 0 in
  for pair = 0 to pairs - 1 do
    let point = pair * 4 and vertex = pair * 6 in
    let origin_x = float_of_int (pair mod 250) *. 2.
    and origin_y = float_of_int (pair / 250) *. 2. in
    x.(point) <- origin_x;
    y.(point) <- origin_y;
    x.(point + 1) <- origin_x +. 1.;
    y.(point + 1) <- origin_y;
    x.(point + 2) <- origin_x +. 1.;
    y.(point + 2) <- origin_y +. 1.;
    x.(point + 3) <- origin_x;
    y.(point + 3) <- origin_y +. 1.;
    vertex_points.(vertex) <- point;
    vertex_points.(vertex + 1) <- point + 1;
    vertex_points.(vertex + 2) <- point + 2;
    vertex_points.(vertex + 3) <- point;
    vertex_points.(vertex + 4) <- point + 2;
    vertex_points.(vertex + 5) <- point + 3
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology =
    Topology.polygons_owned ~point_count ~vertex_points
      ~primitive_offsets:(Array.init ((pairs * 2) + 1) (fun face -> face * 3))
    |> get_ok
  in
  let index = Topology_index.create topology in
  let flip =
    Edge_group.init ~grain:97 ~topology ~index ~name:"flip_edges" (fun edge ->
        let a, b = Topology_index.edge_points index edge in
        abs (a - b) = 2 && min a b mod 4 = 0)
  in
  Geometry.create ~positions ~topology ~edge_groups:[ flip ] () |> get_ok

let edge_equalize_geometry edge_count =
  let point_count = edge_count * 2 in
  let x = Array.make point_count 0.
  and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for edge = 0 to edge_count - 1 do
    let point = edge * 2
    and base = float_of_int edge *. 3.
    and length = 0.5 +. (float_of_int (edge mod 17) *. 0.1) in
    x.(point) <- base;
    x.(point + 1) <- base +. length
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology =
    Topology.create_owned ~point_count ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (edge_count + 1) (fun edge -> edge * 2))
      ~primitive_kinds:(Array.make edge_count Topology.Open_polyline)
    |> get_ok
  in
  Geometry.create ~positions ~topology () |> get_ok

let edge_relax_reference geometry =
  let source = Packed.Float3.View.view (Geometry.positions geometry) in
  let x = Array.copy source.x in
  for edge = 0 to (Geometry.point_count geometry / 2) - 1 do
    let point = edge * 2 in
    x.(point + 1) <- x.(point) +. 0.8 +. (float_of_int (edge mod 11) *. 0.1)
  done;
  Geometry.with_positions (Packed.Float3.View.of_shared_exn ~x ~y:source.y ~z:source.z) geometry
  |> get_ok

let curve_chain_geometry segments =
  let point_count = segments + 1 in
  let positions =
    Packed.Float3.View.of_owned_exn
      ~x:(Array.init point_count (fun point -> float_of_int point *. 0.002))
      ~y:(Array.init point_count (fun point -> sin (float_of_int point *. 0.017)))
      ~z:(Array.init point_count (fun point -> cos (float_of_int point *. 0.011) *. 0.25))
  in
  let vertex_points =
    Array.init (segments * 2) (fun vertex ->
        let primitive = vertex / 2 in
        primitive + (vertex land 1))
  in
  let topology =
    Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets:(Array.init (segments + 1) (fun primitive -> primitive * 2))
      ~primitive_kinds:(Array.make segments Topology.Open_polyline)
    |> get_ok
  in
  let point_sample =
    Attribute.create_owned ~owner:Attribute.Point ~name:"curve_sample"
      (Attribute.Float
         (Array.init point_count (fun point ->
              let value = float_of_int (point mod 257) in
              value *. value)))
    |> get_ok
  and vertex_sample =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"curve_u"
      (Attribute.Float (Array.init (segments * 2) (fun vertex -> float_of_int (vertex land 1))))
    |> get_ok
  and primitive_sample =
    Attribute.create_owned ~owner:Attribute.Primitive ~name:"curve_id"
      (Attribute.Int (Array.init segments Fun.id))
    |> get_ok
  in
  Geometry.create ~positions ~topology
    ~attributes:[ point_sample; vertex_sample; primitive_sample ]
    ()
  |> get_ok

let run () =
  let generated_line =
    Lisp_sop.node
      {|(sop/line :points 500001 :origin [-10.0 2.0 3.0] :direction [1.0 2.0 3.0] :length 25.0)|}
  in
  let one = cook 1 generated_line and many = cook 4 generated_line in
  check (equal_geometry one many) "one-domain and four-domain Line geometry differ";
  check
    (Geometry.point_count one = 500_001 && Geometry.vertex_count one = 500_001)
    "Line exactness fixture cardinality";
  let generated_resample =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/curve %s)
     (sop/resample
       :use_segments false
       :use_maximum_segment_length true
       :maximum_segment_length 0.0009
       :curve_u_attribute "curveu"
       :curve_number_attribute "curvenum"
       :distance_attribute "distance"
       :tangent_attribute "tangent"))|}
         (curve_list
            (Array.init 5_001 (fun point ->
                 let t = float_of_int point *. 0.003 in
                 (t, sin (t *. 0.7), cos (t *. 0.43) *. 0.6)))))
  in
  let one = cook 1 generated_resample and many = cook 4 generated_resample in
  check (equal_geometry one many) "one-domain and four-domain advanced Resample geometry differ";
  check
    (Geometry.point_count one > 5_001
    && Geometry.find_attribute ~owner:Attribute.Point "tangent" one <> None)
    "advanced Resample exactness fixture cardinality";
  let generated_polyframe =
    Lisp_sop.node
      {|(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :columns 401
       :rows 301
       :uv_attribute "uv"
       :size 20.0)
     (sop/polyframe :orthogonal true :style "Attribute gradient"))|}
  in
  let one = cook 1 generated_polyframe and many = cook 4 generated_polyframe in
  check (equal_geometry one many) "one-domain and four-domain PolyFrame geometry differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Vertex "N" one <> None
    && Geometry.find_attribute ~owner:Attribute.Vertex "tangentu" one <> None
    && Geometry.find_attribute ~owner:Attribute.Vertex "tangentv" one <> None)
    "PolyFrame exactness fixture attributes";
  let generated_facet =
    Lisp_sop.node
      {|(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :columns 401
       :rows 301
       :uv_attribute "uv"
       :size 20.0)
     (sop/facet
       :cusp_mode "Auto"
       :inline_distance 0.0
       :consolidation "Normals"
       :pre_compute_normals true
       :make_normals_unit_length true
       :unique_points true
       :reverse_normals true))|}
  in
  let one = cook 1 generated_facet and many = cook 4 generated_facet in
  check (equal_geometry one many) "one-domain and four-domain Facet geometry differ";
  check
    (Geometry.point_count one = Geometry.vertex_count one
    && Geometry.find_attribute ~owner:Attribute.Point "uv" one <> None)
    "Facet exactness fixture cardinality";
  let generated_grouped_facet =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid
   :width_mode "Auto"
   :height_mode "Auto"
   :columns 401
   :rows 301
   :uv_attribute "uv"
   :size 20.0)
     (sop/ordered_group :owner "Primitives" :name "facet_even" :elements %S)
     (sop/facet :cusp_mode "Auto" :inline_distance 0.0 :group "facet_even" :pre_compute_normals true
       :unique_points true :reverse_normals true))|}
         (ints (Array.init (400 * 300) (fun primitive -> primitive * 2))))
  in
  let one = cook 1 generated_grouped_facet and many = cook 4 generated_grouped_facet in
  check (equal_geometry one many) "one-domain and four-domain grouped Facet geometry differ";
  let generated_inline_facet =
    Lisp_sop.node
      ~with_:[ ("in1689", Lisp_sop.snapshot (inline_geometry 40_000)) ]
      {|(sop/facet
   (sop/ext_in1689)
   :cusp_mode "Auto"
   :inline_distance 0.0
   :remove_inline_points true)|}
  in
  let one = cook 1 generated_inline_facet and many = cook 4 generated_inline_facet in
  check (equal_geometry one many) "one-domain and four-domain Facet inline removal differ";
  check
    (Geometry.point_count one = 160_000 && Geometry.vertex_count one = 160_000)
    "Facet inline exactness fixture cardinality";
  let generated_grid =
    Lisp_sop.node
      {|(sop/grid
   :counts "Point counts"
   :connectivity "Alternating triangles"
   :orientation "Custom axes"
   :horizontal [1.0 2.0 0.5]
   :vertical [-0.25 0.75 2.0]
   :center [3.0 -2.0 5.0]
   :width 40.0
   :height 25.0
   :rotation 0.37
   :uv_attribute "uv"
   :columns 701
   :rows 501)|}
  in
  let one = cook 1 generated_grid and many = cook 4 generated_grid in
  check (equal_geometry one many) "one-domain and four-domain advanced Grid geometry differ";
  check
    (Geometry.point_count one = 351_201 && Geometry.primitive_count one = 700_000)
    "advanced Grid exactness fixture cardinality";
  let generated_circle =
    Lisp_sop.node
      {|(sop/circle
   :arc "Sliced"
   :start_angle -0.7
   :end_angle 5.2
   :orientation "Custom axes"
   :horizontal [1.0 2.0 0.5]
   :vertical [-0.25 0.75 2.0]
   :reverse true
   :center [3.0 -2.0 5.0]
   :radius_x 40.0
   :radius_y 25.0
   :rotation 0.37
   :uniform_scale 1.2
   :segments 500000)|}
  in
  let one = cook 1 generated_circle and many = cook 4 generated_circle in
  check (equal_geometry one many) "one-domain and four-domain advanced Circle geometry differ";
  check
    (Geometry.point_count one = 500_002
    && Topology.primitive_kind (Geometry.topology one) 0 = Topology.Closed_polyline)
    "advanced Circle exactness fixture cardinality";
  let generated_box =
    Lisp_sop.node
      {|(sop/box :connectivity "Quads" :consolidate_points true :normals "Vertex"
   :center [3.0 -2.0 5.0] :rotation [0.3 0.5 0.7] :rotation_order "ZXY"
   :uniform_scale 1.2 :x_divisions 256 :y_divisions 192 :z_divisions 128
   :uv_attribute "uv" :face_groups "face" :size [40.0 25.0 18.0])|}
  in
  let one = cook 1 generated_box and many = cook 4 generated_box in
  check (equal_geometry one many) "one-domain and four-domain advanced Box geometry differ";
  check
    (Geometry.point_count one = 212_994
    && Geometry.primitive_count one = 212_992
    && List.length (Geometry.groups one) = 6)
    "advanced Box exactness fixture cardinality";
  let generated_sphere =
    Lisp_sop.node
      {|(sop/uv_sphere
   :radius [3.0 2.0 1.0]
   :connectivity "Alternating triangles"
   :unique_points_per_pole true
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotation [0.3 0.5 0.7]
   :rotation_order "YZX"
   :segments 256
   :rings 128)|}
  in
  let one = cook 1 generated_sphere and many = cook 4 generated_sphere in
  check (equal_geometry one many) "one-domain and four-domain advanced UV Sphere geometry differ";
  check
    (Geometry.point_count one = 33_024
    && Geometry.vertex_count one = 195_072
    && Geometry.primitive_count one = 65_024)
    "advanced UV Sphere exactness fixture cardinality";
  let generated_torus =
    Lisp_sop.node
      {|(sop/torus
   :connectivity "Alternating triangles"
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotation [0.3 0.5 0.7]
   :rotation_order "YZX"
   :u_start -0.7
   :u_end 4.8
   :v_start -1.2
   :v_end 2.1
   :u_wrap false
   :v_wrap false
   :u_end_caps true
   :v_end_cap true
   :rows 256
   :columns 128
   :major_radius 3.0
   :minor_radius 1.0)|}
  in
  let one = cook 1 generated_torus and many = cook 4 generated_torus in
  check (equal_geometry one many) "one-domain and four-domain advanced Torus geometry differ";
  check
    (Geometry.point_count one = 32_768
    && Geometry.vertex_count one = 196_096
    && Geometry.primitive_count one = 65_282)
    "advanced Torus exactness fixture cardinality";
  let generated_tube =
    Lisp_sop.node
      {|(sop/tube
   :connectivity "Alternating triangles"
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotation [0.3 0.5 0.7]
   :rotation_order "YZX"
   :radius_scale 1.2
   :rows 256
   :columns 128
   :top_radius 0.0
   :bottom_radius 3.0
   :height 5.0)|}
  in
  let one = cook 1 generated_tube and many = cook 4 generated_tube in
  check (equal_geometry one many) "one-domain and four-domain advanced Tube geometry differ";
  check
    (Geometry.point_count one = 32_769
    && Geometry.vertex_count one = 195_584
    && Geometry.primitive_count one = 65_153)
    "advanced Tube exactness fixture cardinality";
  let generated_platonic =
    Lisp_sop.node
      {|(sop/platonic
   :kind "Soccer ball"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotation [0.3 0.5 0.7]
   :rotation_order "YZX"
   :face_groups "face"
   :radius 4.0)|}
  in
  let one = cook 1 generated_platonic and many = cook 4 generated_platonic in
  check (equal_geometry one many) "one-domain and four-domain advanced Platonic geometry differ";
  check
    (Geometry.point_count one = 60
    && Geometry.vertex_count one = 180
    && Geometry.primitive_count one = 32)
    "advanced Platonic exactness fixture cardinality";
  let generated_spiral =
    Lisp_sop.node
      {|(sop/spiral
   :extent_mode "Height and pitch"
   :height -18.0
   :pitch -0.37
   :radius_mode "Logarithmic end"
   :start_radius 0.35
   :end_radius 8.0
   :height_ramp "0:0.8,0.35:1.2,0.7:0.55,1:1"
   :radius_scale 1.3
   :radius_ramp "0:1,0.4:0.6,1:1.15"
   :direction "Clockwise"
   :start_angle -0.7
   :divisions_mode "Per curve"
   :divisions 20000
   :uniform_angle false
   :spiral_count 5
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotation [0.3 0.5 0.7]
   :rotation_order "YZX"
   :uniform_scale 1.2
   :angle_attribute "angle"
   :x_axis_attribute "xaxis"
   :y_axis_attribute "yaxis"
   :tangent_attribute "tangent"
   :orient_attribute "orient"
   :distance_attribute "distance")|}
  in
  let one = cook 1 generated_spiral and many = cook 4 generated_spiral in
  check (equal_geometry one many) "one-domain and four-domain advanced Spiral geometry differ";
  check
    (Geometry.point_count one = 100_005
    && Geometry.vertex_count one = 100_005
    && Geometry.primitive_count one = 5)
    "advanced Spiral exactness fixture cardinality";
  let matrix =
    Rays_math.Mat4.mul
      (Rays_math.Mat4.translation (Rays_math.Vec3.create 2. 3. 4.))
      (Rays_math.Mat4.mul
         (Rays_math.Mat4.rotation ~axis:(Rays_math.Vec3.create 1. 2. 3.) 0.7)
         (Rays_math.Mat4.scaling (Rays_math.Vec3.create 1.25 0.75 1.5)))
  in
  let base =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 320 :rows 256 :size 20.0)|}
    |>
    let migration_matrix = matrix in
    fun migration_input ->
      Lisp_sop.node
        ~with_:[ ("migration_input", migration_input) ]
        (Printf.sprintf
           {|(sop/transform
   (sop/ext_migration_input)
   :mode "Matrix"
   :m00 %s
   :m01 %s
   :m02 %s
   :m03 %s
   :m10 %s
   :m11 %s
   :m12 %s
   :m13 %s
   :m20 %s
   :m21 %s
   :m22 %s
   :m23 %s
   :m30 %s
   :m31 %s
   :m32 %s
   :m33 %s)|}
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:0 ~column:0))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:0 ~column:1))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:0 ~column:2))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:0 ~column:3))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:1 ~column:0))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:1 ~column:1))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:1 ~column:2))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:1 ~column:3))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:2 ~column:0))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:2 ~column:1))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:2 ~column:2))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:2 ~column:3))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:3 ~column:0))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:3 ~column:1))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:3 ~column:2))
           (Lisp_sop.float (Rays_math.Mat4.get migration_matrix ~row:3 ~column:3)))
  in
  let graph =
    Lisp_sop.node
      ~with_:[ ("base", base) ]
      (Printf.sprintf
         {|(-> (sop/ext_base)
     (sop/color_by_height :low_red 45 :low_green 36 :low_blue 114
       :high_red 244 :high_green 124 :high_blue 42)
     %s
     (sop/group_edge_depth :depth 2 :point_group "middle" :name "middle_grown")
     (sop/group_unshared :owner "Points" :name "surface_boundary")
     (sop/group_boundary_components :prefix "boundary_loop")
     (sop/group_range :owner "Primitives" :name "all_faces" :range_mode "From ends")
     (sop/group_range :owner "Primitives" :name "connected_faces"
       :connectivity_mode "Connected with seams" :connectivity_tolerance 1e-06
       :use_collision true :collision_owner "Points"
       :collision_pattern "middle" :keep_boundary true :remove_other_regions true
       :use_filter true :filter_select 3 :filter_of 11 :filter_offset 2
       :start 7 :end_ 40000)
     (sop/group_promote_boundary :tolerance 1e-06 :keep_original true
       :include_unshared_edges true :name "connected_outline"
       :source "Primitives" :destination "Edges"
       :group "connected_faces")
     (sop/group_edges :use_max_angle true :use_min_angle true :name "surface_edges"
       :angle_basis "Incident edges" :min_angle 0.1 :max_angle 2.9)
     (sop/group_unshared :owner "Edges" :name "unshared_edges"))|}
         (bounds_group ~name:"middle"
            ~min:(Rays_math.Vec3.create (-5.) (-100.) (-5.))
            ~max:(Rays_math.Vec3.create 5. 100. 5.)))
  in
  let one = cook 1 graph and many = cook 4 graph in
  check (equal_geometry one many) "one-domain and four-domain procedural geometry differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "N" one <> None)
    "normal attribute missing from exactness fixture";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "Cd" one <> None)
    "color attribute missing from exactness fixture";
  check
    (List.map Group.name (Geometry.groups one)
    = [
        "middle";
        "middle_grown";
        "surface_boundary";
        "boundary_loop__0";
        "all_faces";
        "connected_faces";
      ])
    "group ordering changed";
  let lifecycle_reference =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 8 :rows 4 :size 2.0)
     (sop/set_float :name "reference_keep" :value 1.0)
     (sop/set_float :name "reference_drop" :value 2.0))|}
  in
  let lifecycle =
    Lisp_sop.node
      ~with_:[ ("lifecycle_reference", lifecycle_reference) ]
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 8 :rows 4 :size 2.0)
     (sop/set_float :name "reference_keep" :value 1.0)
     (sop/set_float :name "reference_drop" :value 2.0)
     (sop/set_float :name "temporary_point" :value 3.0)
     (sop/set_float :owner "Vertex" :name "temporary_vertex" :value 4.0)
     (sop/set_float :owner "Primitive" :name "temporary_primitive" :value 5.0)
     (sop/set_int :owner "Detail" :name "temporary_detail" :value 6)
     (sop/delete_attributes (sop/ext_lifecycle_reference) :point_pattern "^reference_keep")
     (sop/rename_attributes :rules "any\ttemporary_*\tfinal_*\terror")
     (sop/swap_attributes :rules "point\tfinal_point\treference_keep\tswap\npoint\tP\trest\tcopy\ndetail\tfinal_detail\tstored_detail\tmove"))|}
  in
  let one = cook 1 lifecycle and many = cook 4 lifecycle in
  check (equal_geometry one many)
    "one-domain and four-domain Attribute Delete/Rename/Swap geometry differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "reference_keep" one <> None
    && Geometry.find_attribute ~owner:Attribute.Point "reference_drop" one = None
    && Geometry.find_attribute ~owner:Attribute.Point "final_point" one <> None
    && Geometry.find_attribute ~owner:Attribute.Vertex "final_vertex" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "final_primitive" one <> None
    && Geometry.find_attribute ~owner:Attribute.Detail "final_detail" one = None
    && Geometry.find_attribute ~owner:Attribute.Detail "stored_detail" one <> None
    && Geometry.find_attribute ~owner:Attribute.Point "rest" one <> None)
    "Attribute Delete/Rename/Swap exactness fixture outputs";
  let blurred =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 240 :rows 160 :size 14.0)
     (sop/noise_displace :seed 311 :amplitude 0.9 :frequency 0.45)
     (sop/color_by_height
       :low_red 29
       :low_green 78
       :low_blue 216
       :high_red 249
       :high_green 115
       :high_blue 22)
     (sop/set_float :name "blur_weight" :value 0.8)
     (sop/set_float :name "blur_alpha" :value 0.9)
     (sop/group_range :name "blur_points" :range_mode "From ends")
     (sop/attribute_blur
       :group "blur_points"
       :iterations 6
       :method_ "Edge length"
       :mode "Custom steps"
       :odd_step 0.42
       :even_step -0.44
       :weight_attribute "blur_weight"
       :alpha_attribute "blur_alpha"
       :pin_borders true
       :attributes "P Cd"))|}
  in
  let one = cook 1 blurred and many = cook 4 blurred in
  check (equal_geometry one many) "one-domain and four-domain Attribute Blur geometry differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "Cd" one <> None)
    "Attribute Blur exactness fixture dropped color";
  let smoothed =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 300 :rows 220 :size 18.0)
     (sop/noise_displace :seed 313 :amplitude 1.1 :frequency 0.52)
     (sop/color_by_height
       :low_red 14
       :low_green 116
       :low_blue 144
       :high_red 250
       :high_green 204
       :high_blue 21)
     (sop/set_float :name "smooth_weight" :value 0.82)
     (sop/group_random :seed 317 :probability 0.72 :owner "Primitives" :name "smooth_faces")
     (sop/group_unshared :owner "Points" :name "smooth_locks")
     (sop/smooth
       :recompute_normals true
       :group "smooth_faces"
       :constrained_points "smooth_locks"
       :boundary "Pin group boundary"
       :iterations 8
       :method_ "Edge length"
       :mode "Custom steps"
       :odd_step 0.43
       :even_step -0.45
       :weight_attribute "smooth_weight"
       :attributes "P Cd"))|}
  in
  let one = cook 1 smoothed and many = cook 4 smoothed in
  check (equal_geometry one many) "one-domain and four-domain Smooth geometry differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "Cd" one <> None
    && Geometry.point_count one = 301 * 221)
    "Smooth exactness fixture changed payload/cardinality";
  let ray_collision =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 320 :rows 240 :size 20.0)
     (sop/noise_displace :seed 319 :amplitude 0.9 :frequency 0.37)
     (sop/color_by_height
       :low_red 6
       :low_green 182
       :low_blue 212
       :high_red 244
       :high_green 63
       :high_blue 94))|}
  in
  let projected =
    (Lisp_sop.node
       {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 320 :rows 240 :size 20.0)|}
    |>
    let migration_translation = Rays_math.Vec3.create 0. 3. 0. in
    fun migration_input ->
      Lisp_sop.node
        ~with_:[ ("migration_input", migration_input) ]
        (Printf.sprintf
           {|(sop/transform
   (sop/ext_migration_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|}
           (Lisp_sop.float migration_translation.Rays_math.Vec3.x)
           (Lisp_sop.float migration_translation.Rays_math.Vec3.y)
           (Lisp_sop.float migration_translation.Rays_math.Vec3.z)))
    |> fun ray_source ->
    Lisp_sop.node
      ~with_:[ ("ray_source", ray_source); ("ray_collision", ray_collision) ]
      (Printf.sprintf
         {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :direction "Vector"
   :direction_vector %s
   :tolerance 1e-09
   :distance_attribute "ray_distance"
   :primitive_attribute "source_primitive"
   :source_vertex_numbers_attribute "source_vertices"
   :source_vertex_weights_attribute "source_weights"
   :normal_attribute "hit_N"
   :point_pattern "Cd")|}
         (Lisp_sop.vec3 (Rays_math.Vec3.neg Rays_math.Vec3.unit_y)))
  in
  let one = cook 1 projected and many = cook 4 projected in
  check (equal_geometry one many) "one-domain and four-domain Ray geometry differ";
  check
    (Geometry.point_count one = 321 * 241
    && Geometry.find_attribute ~owner:Attribute.Point "Cd" one <> None)
    "Ray exactness fixture changed payload/cardinality";
  let grid_snapped =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 500 :rows 300 :size 20.0)
     (sop/noise_displace :seed 321 :amplitude 0.37 :frequency 0.29)
     (sop/snap_to_grid
       :spacing [0.03125 0.03125 0.03125]
       :offset [0.25 0.5 0.75]
       :snapped_group "snapped"))|}
  in
  let one = cook 1 grid_snapped and many = cook 4 grid_snapped in
  check (equal_geometry one many) "one-domain and four-domain procedural grid snap differ";
  check
    (Geometry.find_group ~owner:Group.Point "snapped" one <> None)
    "procedural grid snap exactness fixture dropped output group";
  let bounded =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 500 :rows 300 :size 30.0)
     (sop/noise_displace :seed 323 :amplitude 2.0 :frequency 0.23)
     (sop/bound
       :divisions_x 256
       :divisions_y 128
       :divisions_z 64
       :lower [0.25 0.5 0.75]
       :upper [0.75 0.5 0.25]
       :bounds_group "bounds"
       :center_attribute "bound_center"
       :radii_attribute "bound_radii"))|}
  in
  let one = cook 1 bounded and many = cook 4 bounded in
  check (equal_geometry one many) "one-domain and four-domain procedural Bound geometry differ";
  check
    (Geometry.point_count one = 116_486 && Geometry.primitive_count one = 229_376)
    "procedural Bound exactness cardinality";
  let expanded_groups =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 400 :rows 300 :size 20.0)
     %s
     (sop/group_expand :connectivity_tolerance 1e-06 :steps 24 :step_attribute "grow_step"
       :owner "Points" :group "seed")
     (sop/group_promotions :rules "point\tprimitive\tseed\tgrown_faces\ttrue\tfalse\tshared_edge\t0\tfalse\tfalse\tfalse\t\npoint\tedge\tseed\tgrown_edges\ttrue\tfalse\tall\t0\tfalse\tfalse\tfalse\t")
     (sop/group_expand :connectivity_tolerance 1e-06 :steps 3 :owner "Edges" :group "grown_edges"))|}
         (bounds_group ~name:"seed"
            ~min:(Rays_math.Vec3.create (-0.03) (-1.) (-20.))
            ~max:(Rays_math.Vec3.create 0.03 1. 20.)))
  in
  let one = cook 1 expanded_groups and many = cook 4 expanded_groups in
  check (equal_geometry one many) "one-domain and four-domain Group Expand/Promote geometry differ";
  check
    (Geometry.find_group ~owner:Group.Primitive "grown_faces" one <> None
    && Geometry.find_edge_group "grown_edges" one <> None)
    "Group Expand/Promote exactness fixture dropped outputs";
  let constrained_source =
    Rdk_test_support.Plane_generators.grid ~columns:400 ~rows:300 ~size:20. () |> get_rdk
  in
  let constrained_primitive_count = Geometry.primitive_count constrained_source in
  let region_attribute =
    Attribute.create_owned ~owner:Attribute.Primitive ~name:"region"
      (Attribute.Int
         (Array.init constrained_primitive_count (fun primitive ->
              if primitive < 3 * constrained_primitive_count / 4 then 0 else 1)))
    |> get_ok
  in
  let flow_attribute =
    Attribute.create_owned ~owner:Attribute.Primitive ~name:"flow"
      (Attribute.Float3
         (Packed.Float3.View.of_owned_exn
            ~x:(Array.make constrained_primitive_count 0.)
            ~y:(Array.make constrained_primitive_count 0.)
            ~z:(Array.make constrained_primitive_count 1.)))
    |> get_ok
  in
  let constrained_source =
    constrained_source
    |> Geometry.with_attribute region_attribute
    |> get_ok
    |> Geometry.with_attribute flow_attribute
    |> get_ok
  in
  let seed =
    Group.init ~grain:97 ~owner:Group.Primitive ~name:"seed" constrained_primitive_count
      (fun primitive -> primitive = 0)
  and containment =
    Group.init ~grain:97 ~owner:Group.Primitive ~name:"containment" constrained_primitive_count
      (fun primitive -> primitive < constrained_primitive_count / 2)
  in
  let constrained_source =
    constrained_source |> Geometry.with_group seed |> get_ok |> Geometry.with_group containment
    |> get_ok
  in
  let constrained_expand =
    Lisp_sop.node
      ~with_:[ ("constrained_source", Lisp_sop.snapshot constrained_source) ]
      {|(sop/group_expand (sop/ext_constrained_source)
       :connectivity_tolerance 1e-06 :flood true :step_attribute "constraint_step"
       :primitive_connectivity "Share edges"
       :normal_spread 0.1
       :use_normal_attribute true :normal_owner "Primitive" :normal_name "flow"
       :connectivity_attributes "primitive\tregion"
       :use_collision true :collision_owner "Primitives"
       :collision_group "containment" :collision_contain true :collision_allow_boundary true
       :owner "Primitives" :group "seed")|}
  in
  let one = cook 1 constrained_expand and many = cook 4 constrained_expand in
  check (equal_geometry one many)
    "one-domain and four-domain constrained Group Expand geometry differ";
  let constrained_group = Geometry.find_group ~owner:Group.Primitive "seed" one |> Option.get in
  check
    (Group.cardinality constrained_group > 0
    && Group.cardinality constrained_group < constrained_primitive_count
    && Geometry.find_attribute ~owner:Attribute.Primitive "constraint_step" one <> None)
    "constrained Group Expand exactness fixture ignored constraints";
  let instances =
    Lisp_sop.node
      {|(-> (sop/box :normals "Auto" :connectivity "Triangles")
     (sop/group_edges :name "instance_edges")
     (-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 80 :rows 60 :size 12.0)
         (sop/noise_displace :seed 17 :amplitude 0.8 :frequency 0.3)
         (sop/copy_to_points)))|}
  in
  let one = cook 1 instances and many = cook 4 instances in
  check (equal_geometry one many) "one-domain and four-domain copy-to-points geometry differ";
  check (Geometry.point_count one = 81 * 61 * 24) "copy-to-points exactness fixture cardinality";
  let mirrored =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/box :normals "Auto" :connectivity "Triangles")
     (sop/group_edges :name "box_edges")
     (sop/fuse
       :remove_unused_points_from_degenerate_primitives false
       :remove_degenerate_primitives false
       :tolerance 1e-09
       :attributes "Average numeric")
     (sop/mirror :origin %s :normal [1.0 1.0 0.0]))|}
         (Lisp_sop.vec3 Rays_math.Vec3.zero))
  in
  let one = cook 1 mirrored and many = cook 4 mirrored in
  check (equal_geometry one many) "one-domain and four-domain fuse/mirror geometry differ";
  let clipped =
    Lisp_sop.node
      {|(-> (sop/uv_sphere
       :radius_x_mode "Auto"
       :radius_y_mode "Auto"
       :radius_z_mode "Auto"
       :normals_mode "Auto"
       :uv_attribute ""
       :segments 96
       :rings 64
       :base_radius 2.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_edges :name "sphere_edges")
     (sop/clip
       :replace_existing_groups true
       :keep "All"
       :fill true
       :split_connectivity true
       :group_owner "Edge"
       :group "sphere_edges"
       :distance 0.075
       :clipped_edge_group "clip_edges"
       :cap_group "caps"
       :above_group "above"
       :below_group "below"
       :origin [0.0 0.15 0.0]
       :normal [0.3 1.0 0.2]))|}
  in
  let one = cook 1 clipped and many = cook 4 clipped in
  check (equal_geometry one many) "one-domain and four-domain filled clip geometry differ";
  (match Geometry.find_group ~owner:Group.Primitive "caps" one with
  | Some group -> check (Group.cardinality group = 2) "filled keep-all clip cap count"
  | None -> fail "filled keep-all clip cap group missing");
  (match Geometry.find_edge_group "clip_edges" one with
  | Some group -> check (Edge_group.cardinality group > 0) "filled keep-all clip edge count"
  | None -> fail "filled keep-all clip edge group missing");
  let subdivided =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [2.0 1.5 1.0])
     (sop/fuse
       :remove_unused_points_from_degenerate_primitives false
       :remove_degenerate_primitives false
       :tolerance 0.0
       :attributes "Average numeric")
     (sop/set_color :color [%s %s %s])
     (sop/group_edges :name "subdivision_edges")
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :iterations 3)
     (sop/normals :weighting "Face area" :owner "Point"))|}
         (Lisp_sop.float (56. /. 255.))
         (Lisp_sop.float (189. /. 255.))
         (Lisp_sop.float (248. /. 255.)))
  in
  let one = cook 1 subdivided and many = cook 4 subdivided in
  check (equal_geometry one many) "one-domain and four-domain Catmull-Clark geometry differ";
  check
    (Geometry.primitive_count one = 576)
    (Printf.sprintf "Catmull-Clark procedural fixture cardinality: %d"
       (Geometry.primitive_count one));
  let creased_subdivision =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)
     (sop/set_float :owner "Vertex" :name "creaseweight" :value 1.5)
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""))|}
  in
  let one = cook 1 creased_subdivision and many = cook 4 creased_subdivision in
  check (equal_geometry one many) "one-domain and four-domain semi-sharp subdivision differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Vertex "creaseweight" one <> None)
    "semi-sharp subdivision did not emit residual creases";
  let chaikin_subdivision =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)
     (sop/attribute_randomize
       :a [0.0 0.0 0.0]
       :b [4.0 0.0 0.0]
       :seed 8191
       :owner "Vertex"
       :name "creaseweight")
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :iterations 2
       :creasing_method "Chaikin"
       :resulting_crease_group "chaikin_creases"))|}
  in
  let one = cook 1 chaikin_subdivision and many = cook 4 chaikin_subdivision in
  check (equal_geometry one many) "one-domain and four-domain Chaikin subdivision differ";
  check
    (match Geometry.find_edge_group "chaikin_creases" one with
    | Some group -> Edge_group.cardinality group > 0
    | None -> false)
    "parallel Chaikin subdivision omitted its resulting crease group";
  let all_edge_source =
    Rdk_test_support.Plane_generators.grid
      ~connectivity:Rdk_test_support.Plane_generators.Grid_quads ~columns:120 ~rows:80 ~size:8. ()
    |> get_rdk
  in
  let all_edge_source_count =
    Array.length
      (Topology_index.create (Geometry.topology all_edge_source) |> Topology_index.View.view).edge_a
  in
  let all_edge_subdivision =
    Lisp_sop.node
      ~with_:[ ("all_edge_source", Lisp_sop.snapshot all_edge_source) ]
      {|(sop/subdivide
   (sop/ext_all_edge_source)
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :crease_weight 3.0
   :resulting_crease_group "all_edge_creases")|}
  in
  let one = cook 1 all_edge_subdivision and many = cook 4 all_edge_subdivision in
  check (equal_geometry one many) "one-domain and four-domain all-edge crease override differ";
  check
    (match Geometry.find_edge_group "all_edge_creases" one with
    | Some group -> Edge_group.cardinality group = all_edge_source_count * 4
    | None -> false)
    "parallel all-edge crease override omitted source-edge descendants";
  let dense_curve_source = curve_chain_geometry 50_000 in
  let shared_curves =
    Lisp_sop.node
      ~with_:[ ("dense_curve_source", Lisp_sop.snapshot dense_curve_source) ]
      {|(sop/subdivide
   (sop/ext_dense_curve_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2)|}
  in
  let one = cook 1 shared_curves and many = cook 4 shared_curves in
  check (equal_geometry one many)
    "one-domain and four-domain shared polygon-curve subdivision differ";
  check
    (Geometry.point_count one = 200_001 && Geometry.vertex_count one = 250_000)
    "parallel shared polygon-curve subdivision cardinality";
  let independent_curves =
    Lisp_sop.node
      ~with_:[ ("dense_curve_source", Lisp_sop.snapshot dense_curve_source) ]
      {|(sop/subdivide
   (sop/ext_dense_curve_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :treat_curves_as_independent true)|}
  in
  let one = cook 1 independent_curves and many = cook 4 independent_curves in
  check (equal_geometry one many)
    "one-domain and four-domain independent polygon-curve subdivision differ";
  check
    (Geometry.point_count one = 250_000 && Geometry.vertex_count one = 250_000)
    "parallel independent polygon-curve subdivision cardinality";
  let crease_topology =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)
     (sop/set_float :owner "Vertex" :name "creaseweight" :value 2.5))|}
  in
  let second_input_subdivision =
    Lisp_sop.node
      ~with_:[ ("crease_topology", crease_topology) ]
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)
     (sop/subdivide
       (sop/ext_crease_topology)
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :resulting_crease_group "dense_creases"))|}
  in
  let one = cook 1 second_input_subdivision and many = cook 4 second_input_subdivision in
  check (equal_geometry one many) "one-domain and four-domain second-input subdivision differ";
  check
    (match Geometry.find_edge_group "dense_creases" one with
    | Some group -> Edge_group.cardinality group > 0
    | None -> false)
    "parallel second-input subdivision omitted its resulting crease group";
  let hole_indices =
    Array.init (((120 * 80) + 6) / 7) (fun index -> index * 7)
    |> Array.to_list
    |> List.filter (fun primitive -> primitive < 120 * 80)
    |> Array.of_list
  in
  let holed_subdivision =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid
   :width_mode "Auto"
   :height_mode "Auto"
   :connectivity "Quads"
   :columns 120
   :rows 80
   :size 8.0)
     (sop/ordered_group :owner "Primitives" :name "subdivision_hole" :elements %S)
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :iterations 2))|}
         (ints hole_indices))
  in
  let one = cook 1 holed_subdivision and many = cook 4 holed_subdivision in
  check (equal_geometry one many) "one-domain and four-domain recursive hole subdivision differ";
  check
    (Geometry.primitive_count one = ((120 * 80) - Array.length hole_indices) * 16)
    "parallel recursive hole subdivision cardinality";
  let boundary_text = function
    | Subdivide.Subdivide_boundary_edge_only -> "Edge only"
    | Subdivide.Subdivide_boundary_edge_and_corner -> "Edge and corner"
    | Subdivide.Subdivide_boundary_none -> "None"
  in
  let fvar_text = function
    | Subdivide.Subdivide_fvar_none -> "None"
    | Subdivide.Subdivide_fvar_corners_only -> "Corners only"
    | Subdivide.Subdivide_fvar_corners_plus1 -> "Corners plus 1"
    | Subdivide.Subdivide_fvar_corners_plus2 -> "Corners plus 2"
    | Subdivide.Subdivide_fvar_boundaries -> "Boundaries"
    | Subdivide.Subdivide_fvar_all -> "All"
  in
  let boundary_fixture policy =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 120
       :rows 80
       :size 8.0)
     (sop/set_float :name "boundary_sample" :value 2.5)
     (sop/subdivide :crease_weight_mode "Auto" :face_varying_interpolation "All"
       :remove_holes true :generate_resulting_creases true :hole_group ""
       :boundary_interpolation %S))|}
         (boundary_text policy))
  in
  List.iter
    (fun policy ->
      let one = cook 1 (boundary_fixture policy) and many = cook 4 (boundary_fixture policy) in
      check (equal_geometry one many) "one-domain and four-domain point-boundary subdivision differ")
    [
      Subdivide.Subdivide_boundary_edge_only;
      Subdivide.Subdivide_boundary_edge_and_corner;
      Subdivide.Subdivide_boundary_none;
    ];
  let no_boundary_surface = cook 4 (boundary_fixture Subdivide.Subdivide_boundary_none) in
  check
    (Geometry.primitive_count no_boundary_surface = (120 - 2) * (80 - 2) * 4)
    "parallel None point-boundary subdivision cardinality";
  let fvar_source =
    Rdk_test_support.Plane_generators.grid
      ~connectivity:Rdk_test_support.Plane_generators.Grid_quads ~columns:120 ~rows:80 ~size:8. ()
    |> get_rdk
  in
  let fvar_topology = Topology.View.view (Geometry.topology fvar_source) in
  let fvar_index =
    Topology_index.create (Geometry.topology fvar_source) |> Topology_index.View.view
  in
  let fvar_values =
    Packed.Float2.of_owned
      ~x:
        (Array.init (Geometry.vertex_count fvar_source) (fun vertex ->
             float_of_int fvar_topology.vertex_points.(vertex)))
      ~y:
        (Array.init (Geometry.vertex_count fvar_source) (fun vertex ->
             let primitive = fvar_index.primitive_of_vertex.(vertex) in
             float_of_int (primitive mod 120 / 40)))
    |> get_ok
  in
  let fvar_attribute =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"fvar_uv" (Attribute.Float2 fvar_values)
    |> get_ok
  in
  let fvar_source = Geometry.with_attribute fvar_attribute fvar_source |> get_ok in
  let fvar_fixture policy =
    Lisp_sop.node
      ~with_:[ ("fvar_source", Lisp_sop.snapshot fvar_source) ]
      (Printf.sprintf
         {|(sop/subdivide (sop/ext_fvar_source) :crease_weight_mode "Auto"
       :boundary_interpolation "Edge only" :remove_holes true
       :generate_resulting_creases true :hole_group ""
       :face_varying_interpolation %S)|}
         (fvar_text policy))
  in
  List.iter
    (fun policy ->
      let one = cook 1 (fvar_fixture policy) and many = cook 4 (fvar_fixture policy) in
      check (equal_geometry one many) "one-domain and four-domain face-varying subdivision differ")
    [
      Subdivide.Subdivide_fvar_none;
      Subdivide.Subdivide_fvar_corners_only;
      Subdivide.Subdivide_fvar_corners_plus1;
      Subdivide.Subdivide_fvar_corners_plus2;
      Subdivide.Subdivide_fvar_boundaries;
      Subdivide.Subdivide_fvar_all;
    ];
  let smooth_triangles =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)
     (sop/set_float :owner "Vertex" :name "fvar_sample" :value 2.5)
     (sop/subdivide
       :crease_weight_mode "Auto"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :face_varying_interpolation "None"
       :triangle_policy "Smooth"))|}
  in
  let one = cook 1 smooth_triangles and many = cook 4 smooth_triangles in
  check (equal_geometry one many) "one-domain and four-domain Smooth Triangles subdivision differ";
  check
    (Geometry.primitive_count one = 120 * 80 * 2 * 3)
    "parallel Smooth Triangles subdivision cardinality";
  let detail_source =
    Rdk_test_support.Plane_generators.grid
      ~connectivity:Rdk_test_support.Plane_generators.Grid_triangles ~columns:120 ~rows:80 ~size:8.
      ()
    |> get_rdk
  in
  let detail_vertex_count = Geometry.vertex_count detail_source in
  let detail_source =
    Geometry.with_attribute
      (Attribute.create_owned ~owner:Attribute.Vertex ~name:"fvar_sample"
         (Attribute.Float
            (Array.init detail_vertex_count (fun vertex ->
                 let value = float_of_int (vertex mod 127) in
                 value *. value)))
      |> get_ok)
      detail_source
    |> get_ok
  in
  let detail_source =
    Geometry.with_attribute
      (Attribute.create_owned ~owner:Attribute.Vertex ~name:"creaseweight"
         (Attribute.Float
            (Array.init detail_vertex_count (fun vertex -> float_of_int (vertex * 17 mod 41) /. 10.)))
      |> get_ok)
      detail_source
    |> get_ok
  in
  let detail_source =
    detail_source
    |> with_detail "osd_scheme" (Attribute.Text [| "catmull-clark" |])
    |> with_detail "osd_vtxboundaryinterpolation" (Attribute.Int [| 2 |])
    |> with_detail "osd_fvarlinearinterpolation" (Attribute.Int [| 0 |])
    |> with_detail "osd_creasingmethod" (Attribute.Int [| 1 |])
    |> with_detail "osd_trianglesubdiv" (Attribute.Int [| 1 |])
  in
  let detail_overridden =
    Lisp_sop.node
      ~with_:[ ("detail_source", Lisp_sop.snapshot detail_source) ]
      {|(sop/subdivide
   (sop/ext_detail_source)
   :crease_weight_mode "Auto"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :scheme "Bilinear"
   :boundary_interpolation "None"
   :face_varying_interpolation "All"
   :resulting_crease_group "detail_override_creases")|}
  in
  let one = cook 1 detail_overridden and many = cook 4 detail_overridden in
  check (equal_geometry one many) "one-domain and four-domain detail-overridden subdivision differ";
  check
    (List.for_all
       (fun name -> Geometry.find_attribute ~owner:Attribute.Detail name one <> None)
       [
         "osd_scheme";
         "osd_vtxboundaryinterpolation";
         "osd_fvarlinearinterpolation";
         "osd_creasingmethod";
         "osd_trianglesubdiv";
       ])
    "parallel detail-overridden subdivision dropped its controls";
  check
    (match Geometry.find_edge_group "detail_override_creases" one with
    | Some group -> Edge_group.cardinality group > 0
    | None -> false)
    "parallel detail-overridden Chaikin subdivision dropped residual creases";
  let promoted =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 240 :rows 160 :size 12.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/promote_attributes :delete_source true :pattern "Cd"))|}
  in
  let one = cook 1 promoted and many = cook 4 promoted in
  check (equal_geometry one many) "one-domain and four-domain attribute promotion differ";
  let promoted_arrays =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 240 :rows 160 :size 12.0)
     (sop/enumerate :name "point_number")
     (sop/promote_attributes
       :method_ "Array of all"
       :pattern "point_number"
       :into_pattern "primitive_points"))|}
  in
  let one = cook 1 promoted_arrays and many = cook 4 promoted_arrays in
  check (equal_geometry one many) "one-domain and four-domain array promotion differ";
  (match Geometry.find_attribute ~owner:Attribute.Primitive "primitive_points" one with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Int_array values ->
          let values = Packed.Int_array.View.view values in
          check
            (Array.length values.offsets = Geometry.primitive_count one + 1
            && Array.length values.values = Geometry.vertex_count one)
            "procedural array promotion cardinality"
      | _ -> fail "procedural array promotion storage")
  | None -> fail "procedural array promotion output missing");
  let promoted_patterns =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 10.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/set_float :name "weight" :value 3.0)
     (sop/set_float :name "temporary" :value 9.0)
     (sop/promote_attributes :delete_source true :pattern "* ^temporary"))|}
  in
  let one = cook 1 promoted_patterns and many = cook 4 promoted_patterns in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Primitive "Cd" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "weight" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "temporary" one = None)
    "one-domain and four-domain pattern promotion differ";
  let renamed_patterns =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 10.0)
     (sop/set_float :name "sample_a" :value 3.0)
     (sop/set_float :name "sample_b" :value 9.0)
     (sop/promote_attributes
       :delete_source true
       :method_ "First"
       :pattern "sample_*"
       :into_pattern "reduced_*"
       :index_pattern "source_*"))|}
  in
  let one = cook 1 renamed_patterns and many = cook 4 renamed_patterns in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Primitive "reduced_a" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "reduced_b" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "source_a" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "source_b" one <> None
    && Geometry.find_attribute ~owner:Attribute.Point "sample_a" one = None)
    "one-domain and four-domain renamed pattern promotion differ";
  let piece_geometry =
    Rdk_test_support.Plane_generators.grid ~columns:200 ~rows:120 ~size:12. () |> get_rdk
  in
  let point_count = Geometry.point_count piece_geometry in
  let piece_values =
    Attribute.create_owned ~name:"piece_value" ~owner:Attribute.Point
      (Attribute.Int (Array.init point_count (fun point -> point * 37 mod 211)))
    |> get_ok
  and piece_ids =
    Attribute.create_owned ~name:"piece_id" ~owner:Attribute.Point
      (Attribute.Int (Array.init point_count (fun point -> point / 48)))
    |> get_ok
  in
  let piece_geometry =
    piece_geometry
    |> Geometry.with_attribute piece_values
    |> get_ok |> Geometry.with_attribute piece_ids |> get_ok
  in
  let piece_promoted =
    Lisp_sop.node
      ~with_:[ ("piece_geometry", Lisp_sop.snapshot piece_geometry) ]
      {|(sop/promote_attributes
   (sop/ext_piece_geometry)
   :method_ "Median"
   :piece_attribute "piece_id"
   :into_pattern "piece_median"
   :destination "Point"
   :pattern "piece_value")|}
  in
  let one = cook 1 piece_promoted and many = cook 4 piece_promoted in
  check (equal_geometry one many) "one-domain and four-domain piece median promotion differ";
  let transfer_source =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 12.0)
     (sop/noise_displace :seed 81 :amplitude 0.3 :frequency 0.4)
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_range :name "transfer_source" :range_mode "From ends"))|}
  in
  let transfer_target =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 160 :rows 100 :size 11.5)
     (sop/group_range :name "transfer_target" :range_mode "From ends"))|}
  in
  let transferred =
    Lisp_sop.node
      ~with_:[ ("transfer_source", transfer_source); ("transfer_target", transfer_target) ]
      {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :falloff "Smoothstep"
   :pattern "C* N"
   :mode "Inverse distance"
   :max_distance 0.2
   :source_group "transfer_source"
   :target_group "transfer_target")|}
  in
  let one = cook 1 transferred and many = cook 4 transferred in
  check (equal_geometry one many) "one-domain and four-domain attribute transfer differ";
  let owner_transfer_source =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/enumerate :owner "Primitive" :name "primitive_id")
     (sop/enumerate :owner "Vertex" :name "corner_id"))|}
    |> group_indices ~owner:"Vertices" ~name:"transfer_vertex_patch" (Array.init 1_000 Fun.id)
    |> then_ {|(sop/set_int :owner "Detail" :name "revision" :value 17)|}
  in
  let owner_transfer_target =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 100 :rows 64 :size 7.5)|}
  in
  let directly_copied =
    Lisp_sop.node
      ~with_:
        [
          ("owner_transfer_source", owner_transfer_source);
          ("owner_transfer_target", owner_transfer_target);
        ]
      {|(sop/attribute_copy
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :group_owner "Primitives"
   :rules "point\tCd\t\nvertex\tcorner_id\t\nprimitive\tprimitive_id\t\ndetail\trevision\t")|}
  in
  let one = cook 1 directly_copied and many = cook 4 directly_copied in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Point "Cd" one <> None
    && Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "primitive_id" one <> None
    && Geometry.find_attribute ~owner:Attribute.Detail "revision" one <> None)
    "one-domain and four-domain cross-owner Attribute Copy differ";
  let interpolate_source =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 2 :rows 2 :size 2.0)
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_range :name "hot" :range_mode "From ends"))|}
  in
  let interpolate_target =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 400 :rows 250 :size 8.0)
     (sop/set_int :name "source_primitive")
     (sop/set_vector :name "source_uvw" :value [0.25 0.25 0.0]))|}
  in
  let computed =
    Lisp_sop.node
      ~with_:
        [ ("interpolate_source", interpolate_source); ("interpolate_target", interpolate_target) ]
      {|(sop/attribute_interpolate
   (sop/ext_interpolate_source)
   (sop/ext_interpolate_target)
   :normalize_weights false
   :threshold 1e-06
   :primitive_attribute "source_primitive"
   :uvw_attribute "source_uvw"
   :compute_weights true
   :computed_numbers_attribute "computed_points"
   :computed_weights_attribute "computed_weights"
   :attributes "")|}
  in
  let interpolated =
    Lisp_sop.node
      ~with_:[ ("interpolate_source", interpolate_source); ("computed", computed) ]
      {|(sop/attribute_interpolate
   (sop/ext_interpolate_source)
   (sop/ext_computed)
   :normalize_weights false
   :threshold 1e-06
   :primitive_attribute "source_primitive"
   :uvw_attribute "source_uvw"
   :driver "Point weights"
   :numbers_attribute "computed_points"
   :weights_attribute "computed_weights"
   :point_pattern "P N Cd hot"
   :match_groups true
   :attributes "")|}
  in
  let one = cook 1 interpolated and many = cook 4 interpolated in
  check (equal_geometry one many) "one-domain and four-domain Attribute Interpolate differ";
  let primitive_transferred =
    Lisp_sop.node
      ~with_:
        [
          ("owner_transfer_source", owner_transfer_source);
          ("owner_transfer_target", owner_transfer_target);
        ]
      {|(sop/attribute_transfer
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :falloff "Smoothstep"
   :owner "Primitive"
   :use_names true
   :names "primitive_id"
   :max_distance 0.25)|}
  in
  let one = cook 1 primitive_transferred and many = cook 4 primitive_transferred in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Primitive "primitive_id" one <> None)
    "one-domain and four-domain primitive-barycenter transfer differ";
  let vertex_transferred =
    Lisp_sop.node
      ~with_:
        [
          ("owner_transfer_source", owner_transfer_source);
          ("owner_transfer_target", owner_transfer_target);
        ]
      {|(sop/attribute_transfer
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :falloff "Smoothstep"
   :owner "Vertex"
   :use_names true
   :names "corner_id"
   :max_distance 0.25
   :source_vertex_group_pattern "transfer_vertex_*")|}
  in
  let one = cook 1 vertex_transferred and many = cook 4 vertex_transferred in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" one <> None)
    "one-domain and four-domain vertex attribute transfer differ";
  let detail_transferred =
    Lisp_sop.node
      ~with_:
        [
          ("owner_transfer_source", owner_transfer_source);
          ("owner_transfer_target", owner_transfer_target);
        ]
      {|(sop/attribute_transfer
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :falloff "Smoothstep"
   :distance_mode "Auto"
   :owner "Detail"
   :use_names true
   :names "revision")|}
  in
  let one = cook 1 detail_transferred and many = cook 4 detail_transferred in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Detail "revision" one <> None)
    "one-domain and four-domain detail attribute transfer differ";
  let all_transferred =
    Lisp_sop.node
      ~with_:
        [
          ("owner_transfer_source", owner_transfer_source);
          ("owner_transfer_target", owner_transfer_target);
        ]
      {|(sop/attribute_transfer_all
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :point_pattern "Cd"
   :vertex_pattern "corner_*"
   :primitive_pattern "primitive_*"
   :detail_pattern "revision"
   :max_distance 0.1
   :blend_width 0.4
   :falloff "Uniform"
   :uniform_bias 0.75)|}
  in
  let one = cook 1 all_transferred and many = cook 4 all_transferred in
  check
    (equal_geometry one many
    && Geometry.find_attribute ~owner:Attribute.Point "Cd" one <> None
    && Geometry.find_attribute ~owner:Attribute.Vertex "corner_id" one <> None
    && Geometry.find_attribute ~owner:Attribute.Primitive "primitive_id" one <> None
    && Geometry.find_attribute ~owner:Attribute.Detail "revision" one <> None)
    "one-domain and four-domain multi-owner attribute transfer differ";
  let surface_source =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 160 :rows 100 :size 10.0)
     (sop/noise_displace :seed 29 :amplitude 0.4 :frequency 0.5)
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/measure)
     (sop/group_range :owner "Primitives" :name "surface_source" :range_mode "From ends"))|}
    |> group_indices ~owner:"Vertices" ~name:"surface_vertex_patch" (Array.init 1_000 Fun.id)
  in
  let surface_target =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 140 :rows 90 :size 9.5)|}
    |> (let migration_translation = Rays_math.Vec3.create 0. 0.35 0. in
        fun migration_input ->
          Lisp_sop.node
            ~with_:[ ("migration_input", migration_input) ]
            (Printf.sprintf
               {|(sop/transform
   (sop/ext_migration_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|}
               (Lisp_sop.float migration_translation.Rays_math.Vec3.x)
               (Lisp_sop.float migration_translation.Rays_math.Vec3.y)
               (Lisp_sop.float migration_translation.Rays_math.Vec3.z)))
    |> group_all ~owner:"Points" ~name:"surface_target"
  in
  let surface_transferred =
    Lisp_sop.node
      ~with_:[ ("surface_source", surface_source); ("surface_target", surface_target) ]
      {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "point\tCd\tCd\npoint\tN\tN\nprimitive\tarea\tsource_area"
   :max_distance 0.2
   :blend_width 0.8
   :falloff "Smoothstep"
   :distance_attribute "surface_distance"
   :source_group "surface_source"
   :source_vertex_group_pattern "surface_vertex_*"
   :target_group "surface_target")|}
  in
  let one = cook 1 surface_transferred and many = cook 4 surface_transferred in
  check (equal_geometry one many) "one-domain and four-domain surface attribute transfer differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "surface_distance" one <> None)
    "surface transfer distance attribute missing";
  let surface_vertex_target =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 140 :rows 90 :size 9.5)|}
    |> (let migration_translation = Rays_math.Vec3.create 0. 0.35 0. in
        fun migration_input ->
          Lisp_sop.node
            ~with_:[ ("migration_input", migration_input) ]
            (Printf.sprintf
               {|(sop/transform
   (sop/ext_migration_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|}
               (Lisp_sop.float migration_translation.Rays_math.Vec3.x)
               (Lisp_sop.float migration_translation.Rays_math.Vec3.y)
               (Lisp_sop.float migration_translation.Rays_math.Vec3.z)))
    |> group_all ~owner:"Vertices" ~name:"surface_vertices"
  in
  let vertex_transferred =
    Lisp_sop.node
      ~with_:
        [ ("surface_source", surface_source); ("surface_vertex_target", surface_vertex_target) ]
      {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_vertex_target)
   :target_owner "Vertex"
   :attributes "point\tCd\tCd\npoint\tN\tN\nprimitive\tarea\tsource_area"
   :max_distance 0.2
   :blend_width 0.8
   :distance_attribute "surface_distance"
   :source_group "surface_source"
   :source_vertex_group "surface_vertex_patch"
   :target_group "surface_vertices"
   :falloff "Smoothstep")|}
  in
  let one = cook 1 vertex_transferred and many = cook 4 vertex_transferred in
  check (equal_geometry one many) "one-domain and four-domain vertex surface transfer differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Vertex "surface_distance" one <> None)
    "vertex surface transfer distance attribute missing";
  let enumerate_source =
    Rdk_test_support.Plane_generators.grid ~columns:500 ~rows:300 ~size:20. () |> get_rdk
  in
  let enumerate_count = Geometry.point_count enumerate_source in
  let enumerate_piece =
    Attribute.create_owned ~owner:Attribute.Point ~name:"piece"
      (Attribute.Int (Array.init enumerate_count (fun point -> point * 31 mod 4093)))
    |> get_ok
  in
  let enumerate_source = Geometry.with_attribute enumerate_piece enumerate_source |> get_ok in
  let enumerated =
    Lisp_sop.snapshot enumerate_source
    |> group_in_bounds ~name:"middle"
         ~min:(Rays_math.Vec3.create (-5.) (-1.) (-10.))
         ~max:(Rays_math.Vec3.create 5. 1. 10.)
    |> then_
         {|(sop/enumerate :group "middle" :start 7 :step 3
           :piece_attribute "piece" :mode "Elements within pieces"
           :owner "Point" :name "selection_index")
         (sop/sort :group "middle" :descending true :owner "Points"
           :key "Attribute component" :attribute "selection_index" :component 0)|}
  in
  let one = cook 1 enumerated and many = cook 4 enumerated in
  check (equal_geometry one many)
    "one-domain and four-domain restricted piece enumeration/sort differ";
  let generated_attributes =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 500 :rows 300 :size 20.0)
     (sop/attribute_randomize
       :distribution "Log normal"
       :kind "Vector 4"
       :a [1.0 2.0 3.0]
       :a_w 4.0
       :b [0.2 0.4 0.6]
       :b_w 0.8
       :seed 31337
       :name "sample")
     (sop/attribute_remap
       :name "sample"
       :into "mapped"
       :kind "Vector 4"
       :output_min %s
       :output_max [1.0 1.0 1.0]
       :ramp "0:0,0.35:0.15,0.7:0.85,1:1"))|}
         (Lisp_sop.vec3 Rays_math.Vec3.zero))
  in
  let one = cook 1 generated_attributes and many = cook 4 generated_attributes in
  check (equal_geometry one many) "one-domain and four-domain Attribute Randomize/Remap differ";
  let extended_random =
    (Lisp_sop.node
       {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 500 :rows 300 :size 20.0)|}
    |> group_indices ~owner:"Vertices" ~name:"randomize_vertices" [| 0; 5; 11; 17; 23; 29 |]
    |>
    let migration_value_0 = Rays_math.Vec2.create 1. 1. in
    let migration_value_1 = Rays_math.Vec2.zero in
    let migration_value_2 = Rays_math.Vec2.create (-12.) (-12.) in
    let migration_value_3 = Rays_math.Vec2.create 12. 12. in
    fun migration_input ->
      Lisp_sop.node
        ~with_:[ ("migration_input", migration_input) ]
        (Printf.sprintf
           {|(sop/attribute_randomize
   (sop/ext_migration_input)
   :distribution "Cauchy"
   :kind "Vector 2"
   :a [%s %s 0.0]
   :b [%s %s 0.0]
   :selection_owner "Vertex"
   :selection_group "randomize_vertices"
   :seed 31338
   :use_minimum true
   :use_vector_limits true
   :minimum_vector [%s %s 0.0]
   :use_maximum true
   :maximum_vector [%s %s 0.0]
   :name "cauchy2")|}
           (Lisp_sop.float migration_value_1.Rays_math.Vec2.x)
           (Lisp_sop.float migration_value_1.Rays_math.Vec2.y)
           (Lisp_sop.float migration_value_0.Rays_math.Vec2.x)
           (Lisp_sop.float migration_value_0.Rays_math.Vec2.y)
           (Lisp_sop.float migration_value_2.Rays_math.Vec2.x)
           (Lisp_sop.float migration_value_2.Rays_math.Vec2.y)
           (Lisp_sop.float migration_value_3.Rays_math.Vec2.x)
           (Lisp_sop.float migration_value_3.Rays_math.Vec2.y)))
    |> fun migration_input ->
    Lisp_sop.node
      ~with_:[ ("migration_input", migration_input) ]
      (Printf.sprintf
         {|(sop/attribute_randomize
   (sop/ext_migration_input)
   :distribution "Custom discrete text"
   :text_entries %S
   :seed 31339
   :owner "Primitive"
   :name "label")|}
         (String.concat "\n"
            [
              String.concat "\t" [ "low"; Printf.sprintf "%.17g" 1. ];
              String.concat "\t" [ "high"; Printf.sprintf "%.17g" 3. ];
            ]))
  in
  let one = cook 1 extended_random and many = cook 4 extended_random in
  check (equal_geometry one many) "one-domain and four-domain extended Attribute Randomize differ";
  let deformed =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 500 :rows 300 :size 20.0)|}
    |> group_in_bounds ~name:"deform_center"
         ~min:(Rays_math.Vec3.create (-8.) (-1.) (-8.))
         ~max:(Rays_math.Vec3.create 8. 1. 8.)
    |> then_
         {|(sop/mountain :group "deform_center" :seed 711 :height 0.8
           :frequency [0.31 0.67 0.43]
           :offset [2.0 3.0 5.0] :octaves 6 :lacunarity 2.05
           :roughness 0.48 :height_attribute "mountain_height")
         (sop/peak :direction_attribute "" :group_owner "Point" :group "deform_center" :distance 0.05
           :recompute_normals true)|}
  in
  let one = cook 1 deformed and many = cook 4 deformed in
  check (equal_geometry one many) "one-domain and four-domain Peak/Mountain geometry differ";
  let bent =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 500 :rows 300 :size 20.0)|}
    |> group_in_bounds ~name:"bend_center"
         ~min:(Rays_math.Vec3.create (-8.) (-1.) (-10.))
         ~max:(Rays_math.Vec3.create 8. 1. 10.)
    |> then_
         {|(sop/bend :continuous_twist true :group_owner "Point" :group "bend_center"
           :origin [0.0 0.0 -10.0] :direction [0.0 0.0 1.0]
           :up [0.0 1.0 0.0] :length 20.0 :bend_angle 1.3 :twist_angle 2.1
           :capture_attribute "bend_capture" :recompute_normals true)|}
  in
  let one = cook 1 bent and many = cook 4 bent in
  check (equal_geometry one many) "one-domain and four-domain Bend geometry differ";
  let scattered =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 400 :rows 250 :size 20.0)
     (sop/attribute_randomize
       :a [0.05 0.0 0.0]
       :b [1.0 0.0 0.0]
       :seed 1337
       :name "scatter_density")
     (sop/group_random
       :seed 1338
       :probability 0.73
       :owner "Primitives"
       :name "scatter_surface")
     (sop/scatter
       :seed 1339
       :group "scatter_surface"
       :count 100000
       :use_density true
       :density_owner "Point"
       :density_attribute "scatter_density"
       :point_pattern "N scatter_density"
       :source_primitive_attribute "source_primitive"
       :source_vertex_numbers_attribute "source_vertices"
       :source_vertex_weights_attribute "source_weights"))|}
  in
  let one = cook 1 scattered and many = cook 4 scattered in
  check (equal_geometry one many) "one-domain and four-domain weighted Scatter geometry differ";
  let duplicated =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 4.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_edges :name "duplicate_edges")
     (sop/duplicate :copies 5 :m13 0.4))|}
  in
  let one = cook 1 duplicated and many = cook 4 duplicated in
  check (equal_geometry one many) "one-domain and four-domain materialized duplicate differ";
  let match_target =
    Lisp_sop.node
      {|(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [4.0 3.0 5.0])
     (sop/group_range :name "target_bounds" :range_mode "From ends"))|}
  in
  let utilities =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)|}
    |> group_indices ~owner:"Primitives" ~name:"doomed" (Array.init 1_000 (fun index -> index * 2))
    |> then_
         {|(sop/blast :compact_points true :owner "Primitives" :group "doomed")
         (sop/match_axis :from [0.0 0.0 1.0] :into [1.0 1.0 0.0])|}
    |> group_all ~owner:"Points" ~name:"move"
    |> group_all ~owner:"Points" ~name:"source_bounds"
    |> fun input ->
    Lisp_sop.node
      ~with_:[ ("input", input); ("match_target", match_target) ]
      {|(sop/match_size
   (sop/ext_input)
   (sop/ext_match_target)
   :fit "Match Z"
   :group "move"
   :source_group "source_bounds"
   :target_group "target_bounds"
   :translate_y false
   :justify [-1.0 0.0 1.0]
   :target_justify [1.0 0.0 -1.0]
   :offset [0.25 0.0 -0.5])|}
  in
  let one = cook 1 utilities and many = cook 4 utilities in
  check (equal_geometry one many) "one-domain and four-domain compact/match-size geometry differ";
  let blasted =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 12.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0))|}
    |> group_in_bounds ~name:"left_half"
         ~min:(Rays_math.Vec3.create (-6.) (-1.) (-6.))
         ~max:(Rays_math.Vec3.create 0. 1. 6.)
    |> then_ {|(sop/blast :owner "Points" :group "left_half" :compact_points true)|}
  in
  let one = cook 1 blasted and many = cook 4 blasted in
  check (equal_geometry one many) "one-domain and four-domain point Blast geometry differ";
  let curve_points =
    Array.init 2_001 (fun index ->
        let x = float_of_int index *. 0.01 in
        (x, sin x, 0.))
  in
  let healed_curve =
    Lisp_sop.node (Printf.sprintf {|(sop/curve %s)|} (curve_list curve_points))
    |> group_indices ~owner:"Vertices" ~name:"doomed"
         (Array.init 400 (fun index -> 1 + (index * 5)))
    |> then_ {|(sop/blast :policy "Heal primitives" :owner "Vertices" :group "doomed")|}
  in
  let one = cook 1 healed_curve and many = cook 4 healed_curve in
  check (equal_geometry one many) "one-domain and four-domain vertex-healed curve differ";
  check (Geometry.vertex_count one = 1_601) "vertex-healed curve cardinality";
  let curve_samples =
    Array.init 20_001 (fun index ->
        let x = float_of_int index *. 0.001 in
        (x, sin (x *. 0.7), cos (x *. 0.31)))
  in
  let carved_curve =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/curve %s)
     (sop/group_edges :name "curve_edges")
     (sop/carve :first 0.137 :last 0.863)
     (sop/ends :mode "Close straight"))|}
         (curve_list curve_samples))
  in
  let one = cook 1 carved_curve and many = cook 4 carved_curve in
  check (equal_geometry one many) "one-domain and four-domain carve/curve-ends geometry differ";
  check (Geometry.point_count one > 10_000) "carve exactness fixture unexpectedly small";
  let grouped_carve =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/curve %s)
     (sop/normals :weighting "Face area" :owner "Point")
     (-> (sop/grid
           :width_mode "Auto"
           :height_mode "Auto"
           :connectivity "Quads"
           :columns 128
           :rows 96
           :size 8.0)
         (sop/merge))
     (sop/group_range :owner "Primitives" :name "carve_curve" :end_ 0)
     (sop/carve :group "carve_curve" :first 0.137 :last 0.863))|}
         (curve_list curve_samples))
  in
  let one = cook 1 grouped_carve and many = cook 4 grouped_carve in
  check (equal_geometry one many) "one-domain and four-domain grouped Carve geometry differ";
  check
    (Geometry.primitive_count one = 12_289
    && Topology.primitive_kind (Geometry.topology one) 1 = Topology.Polygon)
    "grouped Carve unselected polygon/cardinality behavior";
  let cut_pieces =
    Lisp_sop.node
      ~with_:[ ("grouped_carve", grouped_carve) ]
      {|(sop/carve
   (sop/ext_grouped_carve)
   :group "carve_curve"
   :first 0.2
   :last 0.8
   :keep "Inside and outside")|}
  in
  let one = cook 1 cut_pieces and many = cook 4 cut_pieces in
  check (equal_geometry one many) "one-domain and four-domain Carve inside/outside pieces differ";
  check
    (Geometry.primitive_count one = 12_291
    && Topology.primitive_kind (Geometry.topology one) 3 = Topology.Polygon)
    "Carve inside/outside exactness cardinality/order";
  let extracted_points =
    Lisp_sop.node
      ~with_:[ ("grouped_carve", grouped_carve) ]
      {|(sop/carve
   (sop/ext_grouped_carve)
   :group "carve_curve"
   :first 0.1
   :last 0.9
   :extract_points true
   :divisions 10001)|}
  in
  let one = cook 1 extracted_points and many = cook 4 extracted_points in
  check (equal_geometry one many) "one-domain and four-domain Carve point extraction differ";
  check
    (Geometry.primitive_count one = 12_288
    && Geometry.point_count one > 10_001
    && Geometry.vertex_count one = 49_152)
    "Carve point extraction exactness cardinality";
  let joined_curve_parts =
    List.init 64 (fun slot ->
        let part = if slot = 0 then 0 else 1 + (slot * 20 mod 63) in
        let points =
          Array.init 129 (fun local ->
              let index = (part * 128) + local in
              let x = float_of_int index *. 0.002 in
              (x, sin (x *. 0.9), cos (x *. 0.37)))
        in
        Lisp_sop.node
          (Printf.sprintf {|(-> (sop/curve %s) (sop/group_edges :name "joined_edges"))|}
             (curve_list points)))
  in
  let joined_curves =
    merge_of joined_curve_parts
    |> then_
         {|(sop/join_curves :use_group_size true :connect_closest_ends true :group_size 9
           :keep_originals true)|}
  in
  let one = cook 1 joined_curves and many = cook 4 joined_curves in
  check (equal_geometry one many)
    "one-domain and four-domain global closest Curve Join geometry differ";
  check
    (Geometry.primitive_count one = 72 && Geometry.vertex_count one = 16_456)
    "Curve Join subgroup/keep exactness fixture cardinality";
  (match Geometry.find_edge_group "joined_edges" one with
  | Some group ->
      check
        (Edge_group.cardinality group = 8_248)
        "Curve Join keep-original/substituted-edge ancestry"
  | None -> fail "Curve Join dropped native edge group");
  let picked_ends =
    Array.init 64 (fun order ->
        {
          Curve_join.primitive = order * 13 mod 64;
          end_ =
            (if order land 1 = 0 then Curve_join.Join_curve_start else Curve_join.Join_curve_end);
        })
  in
  let picked_curves =
    merge_of joined_curve_parts
    |> then_
         (Printf.sprintf
            {|(sop/join_curves :use_group_size true :picked_ends %S :group_size 11 :keep_originals true)|}
            (Array.to_list picked_ends
            |> List.map (fun pick ->
                Printf.sprintf "%d:%s" pick.Rdk.Curve_join.primitive
                  (match pick.end_ with
                  | Rdk.Curve_join.Join_curve_start -> "start"
                  | Join_curve_end -> "end"))
            |> String.concat ","))
  in
  let one = cook 1 picked_curves and many = cook 4 picked_curves in
  check (equal_geometry one many) "one-domain and four-domain picked-end Curve Join geometry differ";
  check
    (Geometry.primitive_count one = 70 && Geometry.vertex_count one >= 16_400)
    "picked-end Curve Join subgroup/keep exactness fixture cardinality";
  let converted_lines =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 256 :rows 192 :size 16.0)
     (sop/group_edges :name "converted_edges")
     (sop/convert_line :length_attribute "edge_length"))|}
  in
  let one = cook 1 converted_lines and many = cook 4 converted_lines in
  check (equal_geometry one many) "one-domain and four-domain Convert Line geometry differ";
  check
    (Geometry.primitive_count one = 147_904 && Geometry.vertex_count one = 295_808)
    "Convert Line exactness fixture cardinality";
  (match Geometry.find_edge_group "converted_edges" one with
  | Some group ->
      check (Edge_group.cardinality group = 147_904) "Convert Line lost native edge membership"
  | None -> fail "Convert Line dropped native edge group");
  let connected_lines =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 256 :rows 192 :size 16.0)
     (sop/group_edges :name "connected_edges")
     (sop/convert_line
       :connect_path true
       :maximum_distance 0.0
       :length_attribute "path_length"))|}
  in
  let one = cook 1 connected_lines and many = cook 4 connected_lines in
  check (equal_geometry one many)
    "one-domain and four-domain connected Convert Line geometry differ";
  check
    (Geometry.primitive_count one > 0
    && Geometry.primitive_count one < 147_904
    && Geometry.find_attribute ~owner:Attribute.Primitive "path_length" one <> None)
    "connected Convert Line exactness fixture cardinality/length";
  (match Geometry.find_edge_group "connected_edges" one with
  | Some group ->
      check
        (Edge_group.cardinality group = 147_904)
        "connected Convert Line lost native edge membership"
  | None -> fail "connected Convert Line dropped native edge group");
  let many_wires =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 64 :rows 48 :size 10.0)
     (sop/set_float :name "wire_scale" :value 0.75)
     (sop/set_float :name "wire_v" :value 0.25)
     (sop/set_int :name "wire_seam" :value 2)
     (sop/set_vector :name "wire_up" :value %s)
     (sop/convert_line)
     (sop/sweep_circle
       :use_u_range false
       :use_v_range false
       :sides 6
       :scale_attribute "wire_scale"
       :seam_offset -1
       :seam_attribute "wire_seam"
       :v_attribute "wire_v"
       :up_attribute "wire_up"
       :caps true
       :cap_group "wire_caps"
       :radius 0.02))|}
         (Lisp_sop.vec3 Rays_math.Vec3.unit_y))
  in
  let one = cook 1 many_wires and many = cook 4 many_wires in
  check (equal_geometry one many) "one-domain and four-domain many-curve sweep geometry differ";
  check (Geometry.primitive_count one = 74_624) "many-curve sweep exactness fixture cardinality";
  (match Geometry.find_group ~owner:Group.Primitive "wire_caps" one with
  | Some group -> check (Group.cardinality group = 18_656) "many-curve sweep cap group cardinality"
  | None -> fail "many-curve sweep cap group missing");
  let general_backbone =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/curve %s)
     (sop/set_float :name "pscale" :value 0.85)
     (sop/group_edges :name "backbone_edges"))|}
         (curve_list
            (Array.init 5_001 (fun point ->
                 let t = float_of_int point *. 0.002 in
                 (0.3 *. sin (t *. 0.71), 0.2 *. cos (t *. 0.43), t)))))
  and general_profile =
    (* LISP GAP: sop/curve has no closed polyline (its lowering always writes closed=false), so
       the closed profile is an OCaml snapshot and the steps on top of it are Lisp. *)
    Lisp_sop.node
      ~with_:
        [
          ( "profile",
            Lisp_sop.snapshot
              (Rdk_test_support.Line_geometry_front.polyline ~closed:true
                 (Array.init 24 (fun point ->
                      let angle = 2. *. Float.pi *. float_of_int point /. 24. in
                      (0.08 *. cos angle, 0.08 *. sin angle, 0.)))
              |> get_rdk) );
        ]
      {|(-> (sop/ext_profile)
     (sop/set_int :name "profile_id" :value 17)
     (sop/group_edges :name "profile_edges"))|}
  in
  let general_sweep =
    Lisp_sop.node
      ~with_:[ ("general_backbone", general_backbone); ("general_profile", general_profile) ]
      {|(sop/sweep
   (sop/ext_general_backbone)
   (sop/ext_general_profile)
   :connectivity "Alternating triangles"
   :twist 2.3
   :caps true
   :cap_group "sweep_caps")|}
  in
  let one = cook 1 general_sweep and many = cook 4 general_sweep in
  check (equal_geometry one many) "one-domain and four-domain general-profile Sweep geometry differ";
  check
    (Geometry.point_count one = 120_024
    && Geometry.primitive_count one = 240_002
    && Geometry.find_attribute ~owner:Attribute.Point "cross_section_profile_id" one <> None)
    "general-profile Sweep exactness fixture cardinality/payload";
  let split_source =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 20 :size 4.0)
     (sop/group_edges :name "split_edges"))|}
  in
  let split_source =
    split_source |> group_indices ~owner:"Primitives" ~name:"split" (Array.init 100 Fun.id)
  in
  let branch selected =
    Lisp_sop.node
      ~with_:[ ("split_source", split_source) ]
      (Printf.sprintf
         {|(sop/blast (sop/ext_split_source) :selected %b :compact_points true :group "split")|}
         selected)
  in
  let selected_branch = branch false and remainder_branch = branch true in
  let selected_one = cook 1 selected_branch
  and selected_many = cook 4 selected_branch
  and remainder_one = cook 1 remainder_branch
  and remainder_many = cook 4 remainder_branch in
  check
    (equal_geometry selected_one selected_many && equal_geometry remainder_one remainder_many)
    "one-domain and four-domain split branches differ";
  check
    (Geometry.primitive_count selected_one = 100 && Geometry.primitive_count remainder_one = 300)
    "split selected/remainder cardinality";
  let merged =
    Lisp_sop.node
      ~with_:
        [
          ("graph", graph);
          ( "in1744",
            let migration_translation = Rays_math.Vec3.create 30. 0. 0. in
            Lisp_sop.node
              ~with_:[ ("graph", graph) ]
              (Printf.sprintf
                 {|(sop/transform (sop/ext_graph) :mode "Matrix" :m03 %s :m13 %s :m23 %s)|}
                 (Lisp_sop.float migration_translation.Rays_math.Vec3.x)
                 (Lisp_sop.float migration_translation.Rays_math.Vec3.y)
                 (Lisp_sop.float migration_translation.Rays_math.Vec3.z)) );
        ]
      {|(sop/merge (sop/ext_graph) (sop/ext_in1744))|}
  in
  let one = cook 1 merged and many = cook 4 merged in
  check (equal_geometry one many) "one-domain and four-domain merge geometry differ";
  let transfer_source =
    Lisp_sop.node
      {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 40 :rows 30 :size 8.0)|}
    |> group_indices ~owner:"Points" ~name:"transfer_points"
         (Array.init 200 (fun index -> index * 6))
    |> group_indices ~owner:"Primitives" ~name:"transfer_faces"
         (Array.init 300 (fun index -> index * 4))
    |> then_ {|(sop/group_edges :use_min_length true :name "transfer_edges" :min_length 0.1)|}
  in
  let transfer_target =
    transfer_source
    |>
    let migration_translation = Rays_math.Vec3.create 0.001 0. 0.001 in
    fun migration_input ->
      Lisp_sop.node
        ~with_:[ ("migration_input", migration_input) ]
        (Printf.sprintf
           {|(sop/transform
   (sop/ext_migration_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|}
           (Lisp_sop.float migration_translation.Rays_math.Vec3.x)
           (Lisp_sop.float migration_translation.Rays_math.Vec3.y)
           (Lisp_sop.float migration_translation.Rays_math.Vec3.z))
  in
  let transferred =
    Lisp_sop.node
      ~with_:[ ("transfer_source", transfer_source); ("transfer_target", transfer_target) ]
      {|(sop/group_transfer (sop/ext_transfer_source) (sop/ext_transfer_target)
       :use_rules true :distance 0.01 :conflict "Overwrite"
       :rules "point\ttransfer_points\tmapped_\nprimitive\ttransfer_faces\tmapped_\nedge\ttransfer_edges\tmapped_")|}
  in
  let one = cook 1 transferred and many = cook 4 transferred in
  check (equal_geometry one many) "one-domain and four-domain Group Transfer geometry differ";
  let columns = 121 in
  let base_elements =
    Array.init 32 (fun index ->
        let pair = index / 2 in
        if index land 1 = 0 then pair * 5 * columns else (pair * 5 * columns) + 120)
  in
  let paths =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 90 :size 20.0)
     (sop/ordered_group :name "waypoints" :elements %S)
     (sop/group_find_path
       :mode "Start/end pairs"
       :avoid_self_intersection false
       :base_group "waypoints"
       :name "paths"))|}
         (ints base_elements))
  in
  let one = cook 1 paths and many = cook 4 paths in
  check (equal_geometry one many) "one-domain and four-domain Group Find Path geometry differ";
  let primitive_elements =
    Array.init 16 (fun index ->
        let row = index / 2 * 10 in
        if index land 1 = 0 then row * 120 * 2 else (((row * 120) + 119) * 2) + 1)
  in
  let primitive_paths =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 90 :size 20.0)
     (sop/ordered_group :owner "Primitives" :name "face_waypoints" :elements %S)
     (sop/group_find_path
       :owner "Primitives"
       :mode "Start/end pairs"
       :avoid_self_intersection false
       :base_group "face_waypoints"
       :name "face_paths"))|}
         (ints primitive_elements))
  in
  let one = cook 1 primitive_paths and many = cook 4 primitive_paths in
  check (equal_geometry one many)
    "one-domain and four-domain primitive Group Find Path geometry differ";
  let attribute_boundaries =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 90 :size 20.0)
     (sop/enumerate :owner "Primitive" :name "face_id")
     (sop/group_from_attribute_boundary :tolerance 1e-06 :owner "Edges"
       :name "attribute_seams" :attributes "primitive\tface_id"))|}
  in
  let one = cook 1 attribute_boundaries and many = cook 4 attribute_boundaries in
  check (equal_geometry one many)
    "one-domain and four-domain Group from Attribute Boundary geometry differ";
  let named_count = 200_003 in
  let named_source =
    Rdk.Line_geometry.points (Array.init named_count (fun point -> (float_of_int point, 0., 0.)))
  in
  let piece_names =
    Attribute.create_owned ~owner:Attribute.Point ~name:"piece_name"
      (Attribute.Text
         (Array.init named_count (fun point ->
              if point mod 101 = 0 then "" else Printf.sprintf "piece_%02d" (point mod 32))))
    |> Result.get_ok
  in
  let named_source = Geometry.with_attribute piece_names named_source |> Result.get_ok in
  let named =
    Lisp_sop.node
      ~with_:[ ("named_source", Lisp_sop.snapshot named_source) ]
      {|(sop/groups_from_name (sop/ext_named_source) :owner "Point" :attribute "piece_name")|}
  in
  let one = cook 1 named and many = cook 4 named in
  check (equal_geometry one many) "one-domain and four-domain Groups from Name geometry differ";
  let round_trip =
    Lisp_sop.node
      ~with_:[ ("named", named) ]
      {|(sop/name_from_groups
   (sop/ext_named)
   :overlap "Last group"
   :attribute "round_trip"
   :delete_groups true
   :owner "Point")|}
  in
  let one = cook 1 round_trip and many = cook 4 round_trip in
  check (equal_geometry one many) "one-domain and four-domain Name from Groups geometry differ";
  let random_groups =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 20.0)
     (sop/group_random :seed 917 :probability 0.431 :name "random_points")
     (sop/group_random :seed 918 :probability 0.379 :owner "Vertices" :name "random_vertices")
     (sop/group_random
       :seed 919
       :probability 0.293
       :owner "Primitives"
       :name "random_primitives")
     (sop/group_random :seed 920 :probability 0.217 :owner "Edges" :name "random_edges"))|}
  in
  let one = cook 1 random_groups and many = cook 4 random_groups in
  check (equal_geometry one many) "one-domain and four-domain Group Random geometry differ";
  let bounded_groups =
    Lisp_sop.node
      ~with_:[ ("random_groups", random_groups) ]
      (Printf.sprintf
         {|(-> (sop/group_bounds
       (sop/ext_random_groups)
       :containment "Partially contained"
       :shape "Sphere"
       :center [1.0 0.0 -2.0]
       :radius 7.5
       :name "bounded_points")
     (sop/group_bounds
       :containment "Partially contained"
       :center [-0.5 0.0 1.0]
       :size [11.0 2.0 12.0]
       :owner "Vertices"
       :name "bounded_vertices")
     (sop/group_bounds
       :containment "Partially contained"
       :shape "Sphere"
       :center %s
       :radius 8.0
       :owner "Primitives"
       :name "bounded_primitives")
     (sop/group_bounds
       :containment "Partially contained"
       :center %s
       :size [8.0 2.0 8.0]
       :owner "Edges"
       :name "bounded_edges"))|}
         (Lisp_sop.vec3 Rays_math.Vec3.zero)
         (Lisp_sop.vec3 Rays_math.Vec3.zero))
  in
  let one = cook 1 bounded_groups and many = cook 4 bounded_groups in
  check (equal_geometry one many) "one-domain and four-domain Group Bounds geometry differ";
  let normal_groups =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [5.0 4.0 3.0])
     (sop/mountain :seed 929 :height 0.21 :frequency [0.7 1.1 0.9])
     (sop/group_normal
       :direction %s
       :spread_angle %s
       :include_opposite true
       :owner "Points"
       :name "vertical_points")
     (sop/group_normal
       :direction %s
       :spread_angle %s
       :include_opposite true
       :name "vertical_faces")
     (sop/group_normal
       :direction %s
       :spread_angle %s
       :include_opposite true
       :owner "Edges"
       :name "vertical_edges")
     (sop/group_non_planar :tolerance 0.001 :name "warped_faces")
     (sop/group_backface :viewpoint [4.0 3.0 5.0] :name "backfaces"))|}
         (Lisp_sop.vec3 Rays_math.Vec3.unit_y)
         (Lisp_sop.float (Float.pi /. 3.))
         (Lisp_sop.vec3 Rays_math.Vec3.unit_y)
         (Lisp_sop.float (Float.pi /. 3.))
         (Lisp_sop.vec3 Rays_math.Vec3.unit_y)
         (Lisp_sop.float (Float.pi /. 3.)))
  in
  let one = cook 1 normal_groups and many = cook 4 normal_groups in
  check (equal_geometry one many)
    "one-domain and four-domain Group Normal/Non-Planar geometry differ";
  let extruded =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 40 :rows 30 :size 8.0)
     (sop/group_range :owner "Primitives" :name "all" :range_mode "From ends")
     (sop/group_edges :name "extruded_edges")
     (sop/poly_extrude
       :group "all"
       :divide "Connected components"
       :divisions 3
       :front_group "extrude_front"
       :side_group "extrude_side"
       :front_boundary_group "front_rim"
       :back_boundary_group "back_rim"
       :distance 0.4)
     (sop/measure :total_attribute "surface_area")
     (sop/measure :attribute "boundary_length" :total_attribute "perimeter" :kind "Perimeter")
     (sop/connectivity))|}
  in
  let one = cook 1 extruded and many = cook 4 extruded in
  check (equal_geometry one many) "one-domain and four-domain extrude/analysis geometry differ";
  let cleaned =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 320 :rows 240 :size 12.0)
     (sop/set_float :name "temporary_weight" :value 1.0)
     (sop/group_random :seed 77 :probability 0.0 :name "empty_points")
     (sop/clean
       :consolidate_mode "Auto"
       :remove_unused_points false
       :remove_nan_points false
       :overlaps "Auto"
       :epsilon_mode "Auto"
       :reverse_winding true
       :point_attributes "temporary*"))|}
  in
  let one = cook 1 cleaned and many = cook 4 cleaned in
  check (equal_geometry one many) "one-domain and four-domain Clean geometry differ";
  let volume_graph =
    Lisp_sop.node
      {|(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [2.0 3.0 4.0])
     (sop/duplicate :copies 20000 :m03 3.0)
     (sop/measure :total_attribute "signed_volume" :kind "Signed volume"))|}
  in
  let one = cook 1 volume_graph and many = cook 4 volume_graph in
  check (equal_geometry one many) "one-domain and four-domain signed-volume geometry differ";
  let compacted =
    Lisp_sop.node
      ~with_:[ ("in1751", Lisp_sop.snapshot (Rdk.Line_geometry.points [| (8., 0., 0.) |])) ]
      {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [2.0 0.0 0.0]))
     (sop/group_edges :name "compact_edges")
     (-> (sop/group_edges (sop/ext_in1751) :name "compact_edges") (sop/merge))
     (sop/compact_points))|}
  in
  let one = cook 1 compacted and many = cook 4 compacted in
  check (equal_geometry one many) "one-domain and four-domain edge-aware point compaction differ";
  check (Geometry.point_count one = 3) "point compaction did not remove the unreferenced point";
  (match Geometry.find_edge_group "compact_edges" one with
  | Some group ->
      check
        (Edge_group.cardinality group = 2)
        "point compaction did not preserve native edge membership"
  | None -> fail "point compaction dropped its native edge group");
  let collapsed =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 240 :rows 180 :size 12.0)
     (sop/set_int :name "piece" :value 1)
     (sop/group_random :seed 911 :probability 0.045 :owner "Edges" :name "collapse_edges")
     (sop/edge_collapse :group "collapse_edges" :connectivity_attribute "piece"))|}
  in
  let one = cook 1 collapsed and many = cook 4 collapsed in
  check (equal_geometry one many) "one-domain and four-domain Edge Collapse geometry differ";
  check
    (Geometry.point_count one < 241 * 181 && Geometry.primitive_count one > 0)
    "parallel Edge Collapse fixture did not retain useful output";
  let reduced =
    Lisp_sop.node
      {|(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :counts "Point counts"
       :connectivity "Alternating triangles"
       :columns 120
       :rows 90
       :size 12.0)
     (sop/set_int :name "source_id" :value 17)
     (sop/group_edges :name "boundary" :incidence "Boundary")
     (sop/poly_reduce
       :ratio 0.37
       :equalize_lengths 1e-08
       :limit_normal_deviation true
       :max_normal_deviation 0.4
       :output_group "reduced"))|}
  in
  let one = cook 1 reduced and many = cook 4 reduced in
  check (equal_geometry one many) "one-domain and four-domain PolyReduce geometry differ";
  check
    (Geometry.primitive_count one < 119 * 89 * 2
    && Geometry.find_group ~owner:Group.Primitive "reduced" one <> None
    && Geometry.find_edge_group "boundary" one <> None)
    "parallel PolyReduce fixture lost cardinality or ancestry";
  let beveled =
    Lisp_sop.node
      {|(-> (sop/box :connectivity "Quads" :consolidate_points true
       :normals "None" :size [1.0 1.0 1.0])
     (sop/duplicate :copies 2000 :m03 1.5 :m13 0.0 :m23 0.0)
     (sop/set_float :owner "Point" :name "pscale" :value 1.0)
     (sop/group_edges :name "bevel_edges")
     (sop/poly_bevel :group "bevel_edges"
       :shape "Round" :convexity 0.8 :divisions 3
       :point_scale_attribute "pscale" :distance 0.08
       :edge_group "edge_fillets" :corner_group "corner_fillets"
       :offset_group "offset_edges"))|}
  in
  let one = cook 1 beveled and many = cook 4 beveled in
  check (equal_geometry one many) "one-domain and four-domain PolyBevel geometry differ";
  check
    (Geometry.point_count one = 2_001 * 72
    && Geometry.primitive_count one = 2_001 * 50
    && Geometry.find_group ~owner:Group.Primitive "edge_fillets" one <> None
    && Geometry.find_edge_group "offset_edges" one <> None)
    "parallel PolyBevel fixture lost cardinality or ancestry";
  let point_split =
    Lisp_sop.node
      {|(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 400
       :rows 300
       :size 12.0)
     (sop/point_split :attributes "" :tolerance 1e-05))|}
  in
  let one = cook 1 point_split and many = cook 4 point_split in
  check (equal_geometry one many) "one-domain and four-domain Point Split geometry differ";
  check
    (Geometry.point_count one = 400 * 300 * 4 && Geometry.vertex_count one = 400 * 300 * 4)
    "parallel Point Split fixture cardinality";
  let emission_points =
    Array.init 100_000 (fun point ->
        ( float_of_int (point mod 1_000) *. 0.01,
          float_of_int (point / 1_000) *. 0.01,
          float_of_int (point mod 17) *. 0.001 ))
  in
  let point_generate =
    Lisp_sop.node
      ~with_:[ ("emission_points", Lisp_sop.snapshot (Rdk.Line_geometry.points emission_points)) ]
      {|(-> (sop/set_float (sop/ext_emission_points) :name "density" :value 6.0)
     (sop/set_int :name "source_id" :value 17)
     (sop/point_generate
       :seed 929
       :generated_group "emitted"
       :copy_point_attributes "density source_id"
       :scale_attribute "density"))|}
  in
  let one = cook 1 point_generate and many = cook 4 point_generate in
  check (equal_geometry one many) "one-domain and four-domain Point Generate geometry differ";
  check
    (Geometry.point_count one = 600_000
    && Geometry.find_group ~owner:Group.Point "emitted" one <> None)
    "parallel Point Generate fixture cardinality";
  let point_replicate =
    Lisp_sop.node
      ~with_:[ ("emission_points", Lisp_sop.snapshot (Rdk.Line_geometry.points emission_points)) ]
      {|(-> (sop/set_float (sop/ext_emission_points) :name "density" :value 6.0)
     (sop/enumerate)
     (sop/set_vector :name "flow" :value [1.0 2.0 3.0])
     (sop/set_vector :name "scale" :value [0.75 1.25 1.5])
     (sop/point_replicate
       :seed 937
       :quasi_stratified true
       :generated_group "cloud"
       :copy_point_attributes "density id flow scale"
       :transform_attributes "flow"
       :points_per_point 1.0
       :scale_attribute "density"))|}
  in
  let one = cook 1 point_replicate and many = cook 4 point_replicate in
  check (equal_geometry one many) "one-domain and four-domain Point Replicate geometry differ";
  check
    (Geometry.point_count one = 600_000
    && Geometry.find_group ~owner:Group.Point "cloud" one <> None)
    "parallel Point Replicate fixture cardinality";
  let flipped =
    Lisp_sop.node
      ~with_:[ ("in1754", Lisp_sop.snapshot (edge_flip_geometry 20_000)) ]
      {|(sop/edge_flip (sop/ext_in1754) :group "flip_edges")|}
  in
  let one = cook 1 flipped and many = cook 4 flipped in
  check (equal_geometry one many) "one-domain and four-domain Edge Flip geometry differ";
  check
    (Geometry.point_count one = 80_000
    && Geometry.vertex_count one = 120_000
    && Geometry.primitive_count one = 40_000)
    "parallel Edge Flip fixture cardinality";
  let cusped =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 320 :rows 240 :size 12.0)
     (sop/set_int :name "source_id" :value 17)
     (sop/group_edges :name "cusp_edges")
     (sop/edge_cusp :group "cusp_edges"))|}
  in
  let one = cook 1 cusped and many = cook 4 cusped in
  check (equal_geometry one many) "one-domain and four-domain Edge Cusp geometry differ";
  check
    (Geometry.point_count one > 321 * 241 && Geometry.vertex_count one = 320 * 240 * 6)
    "parallel Edge Cusp fixture cardinality";
  let straightened =
    Lisp_sop.node
      {|(-> (sop/grid :columns 400 :rows 300 :width 14.0 :height 9.0)
     (sop/mountain :seed 919 :height 0.8 :frequency [0.7 1.1 0.9])
     (sop/group_edges :name "straighten_edges")
     (sop/edge_straighten :group "straighten_edges" :output_group "straightened"))|}
  in
  let one = cook 1 straightened and many = cook 4 straightened in
  check (equal_geometry one many) "one-domain and four-domain Edge Straighten geometry differ";
  check
    (Geometry.point_count one = 401 * 301
    && Geometry.find_edge_group "straightened" one |> Option.get |> Edge_group.cardinality > 0)
    "parallel Edge Straighten fixture cardinality";
  let equalized =
    Lisp_sop.node
      ~with_:[ ("in1755", Lisp_sop.snapshot (edge_equalize_geometry 50_000)) ]
      {|(-> (sop/group_edges (sop/ext_in1755) :name "equalize_edges")
     (sop/edge_equalize :group "equalize_edges" :output_group "equalized"))|}
  in
  let one = cook 1 equalized and many = cook 4 equalized in
  check (equal_geometry one many) "one-domain and four-domain Edge Equalize geometry differ";
  check
    (Geometry.point_count one = 100_000
    && Geometry.find_edge_group "equalized" one |> Option.get |> Edge_group.cardinality = 50_000)
    "parallel Edge Equalize fixture cardinality";
  let relax_source = edge_equalize_geometry 50_000 in
  let relaxed =
    Lisp_sop.node
      ~with_:
        [
          ("relax_source", Lisp_sop.snapshot relax_source);
          ("in1757", Lisp_sop.snapshot (edge_relax_reference relax_source));
        ]
      {|(sop/edge_relax (sop/ext_relax_source) (sop/ext_in1757) :iterations 20)|}
  in
  let one = cook 1 relaxed and many = cook 4 relaxed in
  check (equal_geometry one many) "one-domain and four-domain Edge Relax geometry differ";
  check
    (Geometry.point_count one = 100_000 && Geometry.primitive_count one = 50_000)
    "parallel Edge Relax fixture cardinality";
  let uv_mapped =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/uv_sphere
       :radius_x_mode "Auto"
       :radius_y_mode "Auto"
       :radius_z_mode "Auto"
       :normals_mode "Auto"
       :uv_attribute ""
       :segments 192
       :rings 96
       :base_radius 2.0)
     (sop/group_range :owner "Primitives" :name "uv_faces" :range_mode "From ends")
     (sop/group_edges :name "boundary_edges" :incidence "Boundary")
     (sop/uv_project
       :group "uv_faces"
       :u_min 0.1
       :u_max 0.9
       :v_min 0.2
       :v_max 0.8
       :projection "Spherical")
     (sop/uv_transform :scale_u 3.0 :scale_v 2.0 :angle 0.17)
     (sop/uv_auto_seam :angle %s :existing_uv "uv" :island_attribute "uv_island")
     (sop/uv_unitize :seams "uv_seams" :mode "Islands"))|}
         (Lisp_sop.float (Float.pi /. 3.)))
  in
  let one = cook 1 uv_mapped and many = cook 4 uv_mapped in
  check (equal_geometry one many)
    "one-domain and four-domain UV projection/transform/seam/unitize differ";
  check
    (Geometry.find_attribute ~owner:Attribute.Vertex "uv" one <> None)
    "parallel UV fixture missing vertex coordinates";
  let parameterized =
    Lisp_sop.node
      {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 96 :rows 64 :size 8.0)
     (sop/uv_flatten :iterations 400 :tolerance 1e-10)
     (sop/uv_relax :iterations 50 :tolerance 1e-12))|}
  in
  let one = cook 1 parameterized and many = cook 4 parameterized in
  check (equal_geometry one many) "one-domain and four-domain UV flatten/relax differ";
  print_endline "procedural parallel exactness test passed"
