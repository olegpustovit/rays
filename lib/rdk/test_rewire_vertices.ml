open Rdk
open Rdk_test_support

let group owner name length members =
  let builder = Group.Builder.create ~owner ~name length in
  List.iter (fun member -> Group.Builder.set builder member true) members;
  Group.Builder.freeze builder

let add ~owner ~name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let source ?(free_point = false) () =
  let point_count = if free_point then 8 else 7 in
  let topology = Rdk_test_support.Tb.create ~point_count () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 2 1 3;
  Rdk_test_support.Tb.add_open_polyline topology [|4;5;6|];
  let topology = Rdk_test_support.Tb.freeze topology in
  let hard_index = Topology_index.create topology in
  let hard = Edge_group.init ~topology ~index:hard_index ~name:"hard"
      (fun edge ->
        let a, b = Topology_index.edge_points hard_index edge in
        a = 0 && b = 1) in
  let geometry = Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn
        ~x:(Array.init point_count float_of_int)
        ~y:(Array.make point_count 0.) ~z:(Array.make point_count 0.))
      ~topology ~edge_groups:[hard] () |> Result.get_ok in
  geometry
  |> add ~owner:Attribute.Point ~name:"targetpt"
       (Attribute.Int (if free_point then [|1;-1;-1;-1;-1;-1;-1;-1|]
         else [|3;-1;4;-1;5;6;-1|]))
  |> add ~owner:Attribute.Vertex ~name:"targetv"
       (Attribute.Int [|3;-1;-1;-1;-1;-1;6;-1;-1|])
  |> add ~owner:Attribute.Primitive ~name:"targetprim"
       (Attribute.Int [|4;-1;0|])
  |> add ~owner:Attribute.Point ~name:"weight"
       (Attribute.Float (Array.init point_count (fun point ->
          0.25 +. float_of_int point)))
  |> add ~owner:Attribute.Point ~name:"rows"
       (Attribute.Int_array (Packed.Int_array.create_owned
         ~offsets:(Array.init (point_count + 1) Fun.id)
         ~values:(Array.init point_count (fun point -> 100 + point))
         |> Result.get_ok))
  |> add ~owner:Attribute.Point ~name:"N"
       (Attribute.Float3 (Packed.Float3.View.of_owned_exn
         ~x:(Array.make point_count 0.) ~y:(Array.make point_count 1.)
         ~z:(Array.make point_count 0.)))
  |> add ~owner:Attribute.Vertex ~name:"N"
       (Attribute.Float3 (Packed.Float3.View.of_owned_exn
         ~x:(Array.make 9 0.) ~y:(Array.make 9 1.) ~z:(Array.make 9 0.)))
  |> Geometry.with_group (group Group.Point "marked" point_count
       (if free_point then [0;7] else [0]))
       |> Result.get_ok

let vertex_points geometry =
  (Topology.View.view (Geometry.topology geometry)).vertex_points

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing attribute " ^ name)

let scale_source count =
  let count = count - (count mod 3) in
  let primitive_count = count / 3 in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id)
      ~primitive_offsets:(Array.init (primitive_count + 1) (fun primitive ->
        primitive * 3)) |> Result.get_ok in
  Geometry.create
    ~positions:(Packed.Float3.View.of_owned_exn
      ~x:(Array.init count float_of_int) ~y:(Array.make count 0.)
      ~z:(Array.make count 0.)) ~topology () |> Result.get_ok
  |> add ~owner:Attribute.Point ~name:"target"
       (Attribute.Int (Array.init count (fun point ->
          if point mod 3 = 0 then point + 1 else -1)))

let run () =
  print_endline "Rewire Vertices tests passed"
