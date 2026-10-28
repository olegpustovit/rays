open Rdk_test_support
open Rdk

let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let rings ?(sections = 2) ?(points = 4) () =
  let count = sections * points in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  for section = 0 to sections - 1 do
    for local = 0 to points - 1 do
      let point = (section * points) + local in
      let angle = 2. *. Float.pi *. float_of_int local /. float_of_int points in
      let radius = 1. +. (0.08 *. sin (float_of_int section *. 0.31)) in
      x.(point) <- radius *. cos angle;
      y.(point) <- float_of_int section *. 0.2;
      z.(point) <- radius *. sin angle
    done
  done;
  let topology = Topology.create_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id)
      ~primitive_offsets:(Array.init (sections + 1) (fun value -> value * points))
      ~primitive_kinds:(Array.make sections Topology.Closed_polyline) |> get in
  let vertex = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int (Array.init count (fun value -> 100 + value))) |> get
  and primitive = Attribute.create_owned ~owner:Attribute.Primitive ~name:"section"
      (Attribute.Int (Array.init sections (fun value -> 10 + value))) |> get
  and corner_group = Group.init ~grain:1 ~owner:Group.Vertex ~name:"corners"
      count (fun _ -> true) in
  let geometry = Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
      ~topology ~attributes:[vertex; primitive] ~groups:[corner_group] () |> get in
  let index = Topology_index.create topology in
  let edges = Edge_group.init ~grain:1 ~topology ~index ~name:"section_edges"
      (fun _ -> true) in
  Geometry.with_edge_group edges geometry |> get

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.View.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let unequal_open () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.; 0.;0.6;1.4;2.|]
      ~y:[|0.;0.;0.; 1.;1.;1.;1.|] ~z:(Array.make 7 0.) in
  let topology = Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1;2; 3;4;5;6|] ~primitive_offsets:[|0;3;7|]
      ~primitive_kinds:[|Topology.Open_polyline;Topology.Open_polyline|] |> get in
  Geometry.create ~positions ~topology () |> get

let run () =
  print_endline "test_skin: ok"
