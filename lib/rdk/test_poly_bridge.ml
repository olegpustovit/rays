open Rdk_test_support
open Rdk

let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let loop_pairs ?(pairs = 1) ?(source_points = 4) ?destination_points () =
  let destination_points = Option.value ~default:source_points destination_points in
  let points_per_pair = source_points + destination_points in
  let point_count = pairs * points_per_pair and primitive_count = pairs * 2 in
  let x = Array.make point_count 0.
  and y = Array.make point_count 0.
  and z = Array.make point_count 0.
  and vertex_points = Array.make point_count 0
  and primitive_offsets = Array.make (primitive_count + 1) 0 in
  let point_at = ref 0 and vertex_at = ref 0 and primitive_at = ref 0 in
  for pair = 0 to pairs - 1 do
    for side = 0 to 1 do
      let count = if side = 0 then source_points else destination_points in
      primitive_offsets.(!primitive_at) <- !vertex_at;
      for local = 0 to count - 1 do
        let point = !point_at + local
        and angle = 2. *. Float.pi *. float_of_int local /. float_of_int count in
        x.(point) <- (float_of_int pair *. 3.) +. cos angle;
        y.(point) <- float_of_int side;
        z.(point) <- sin angle;
        vertex_points.(!vertex_at + local) <- point
      done;
      point_at := !point_at + count;
      vertex_at := !vertex_at + count;
      incr primitive_at
    done
  done;
  primitive_offsets.(primitive_count) <- point_count;
  let topology =
    Topology.create_owned ~point_count ~vertex_points ~primitive_offsets
      ~primitive_kinds:(Array.make primitive_count Topology.Polygon)
    |> get
  in
  let point_attribute =
    Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float (Array.init point_count float_of_int))
    |> get
  and height_attribute =
    Attribute.create_owned ~owner:Attribute.Point ~name:"height" (Attribute.Float (Array.copy y))
    |> get
  and point_ids =
    Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int (Array.init point_count Fun.id))
    |> get
  and point_labels =
    Attribute.create_owned ~owner:Attribute.Point ~name:"side"
      (Attribute.Text
         (Array.init point_count (fun point ->
              if point mod points_per_pair < source_points then "source" else "destination")))
    |> get
  and normals =
    Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3
         (Packed.Float3.View.of_owned_exn ~x:(Array.make point_count 1.)
            ~y:(Array.make point_count 0.) ~z:(Array.make point_count 0.)))
    |> get
  and vertex_attribute =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int (Array.init point_count (fun value -> 100 + value)))
    |> get
  and vertex_height =
    Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner_height"
      (Attribute.Float (Array.copy y))
    |> get
  and primitive_attribute =
    Attribute.create_owned ~owner:Attribute.Primitive ~name:"surface"
      (Attribute.Int (Array.init primitive_count Fun.id))
    |> get
  and vertex_group =
    Group.init ~grain:1 ~owner:Group.Vertex ~name:"all_corners" point_count (fun _ -> true)
  and source_faces =
    Group.init ~grain:1 ~owner:Group.Primitive ~name:"source_faces" primitive_count
      (fun primitive -> primitive land 1 = 0)
  and source_points_group =
    Group.init ~grain:1 ~owner:Group.Point ~name:"source_points" point_count (fun point ->
        point mod points_per_pair < source_points)
  in
  let geometry =
    Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
      ~topology
      ~attributes:
        [
          point_attribute;
          height_attribute;
          point_ids;
          point_labels;
          normals;
          vertex_attribute;
          vertex_height;
          primitive_attribute;
        ]
      ~groups:[ vertex_group; source_faces; source_points_group ]
      ()
    |> get
  in
  let index = Topology_index.create topology in
  let source =
    Edge_group.init ~grain:1 ~topology ~index ~name:"source_edges" (fun edge ->
        let a, _ = Topology_index.edge_points index edge in
        let local = a mod points_per_pair in
        local < source_points)
  and destination =
    Edge_group.init ~grain:1 ~topology ~index ~name:"destination_edges" (fun edge ->
        let a, _ = Topology_index.edge_points index edge in
        let local = a mod points_per_pair in
        local >= source_points)
  in
  let geometry =
    Geometry.with_edge_group source geometry |> get |> Geometry.with_edge_group destination |> get
  in
  (geometry, source, destination)

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get |> Attribute.View.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let float_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get |> Attribute.View.storage with
  | Attribute.Float values -> values
  | _ -> fail (name ^ " storage")

let text_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get |> Attribute.View.storage with
  | Attribute.Text values -> values
  | _ -> fail (name ^ " storage")

let open_paths () =
  let positions =
    Packed.Float3.View.of_owned_exn ~x:[| 0.; 1.; 2.; 0.; 1.; 2. |] ~y:[| 0.; 0.; 0.; 1.; 1.; 1. |]
      ~z:(Array.make 6 0.)
  in
  let topology =
    Topology.create_owned ~point_count:6 ~vertex_points:[| 0; 1; 2; 3; 4; 5 |]
      ~primitive_offsets:[| 0; 3; 6 |]
      ~primitive_kinds:[| Topology.Open_polyline; Topology.Open_polyline |]
    |> get
  in
  let geometry = Geometry.create ~positions ~topology () |> get in
  let index = Topology_index.create topology in
  let source =
    Edge_group.init ~grain:1 ~topology ~index ~name:"source" (fun edge ->
        let a, _ = Topology_index.edge_points index edge in
        a < 3)
  and destination =
    Edge_group.init ~grain:1 ~topology ~index ~name:"destination" (fun edge ->
        let a, _ = Topology_index.edge_points index edge in
        a >= 3)
  in
  (geometry, source, destination)

let run () = print_endline "test_poly_bridge: ok"
