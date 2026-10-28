open Rdk_test_support
open Rdk

let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && match Attribute.View.storage left, Attribute.View.storage right with
     | Attribute.Float a, Attribute.Float b -> a = b
     | Attribute.Int a, Attribute.Int b -> a = b
     | Attribute.Text a, Attribute.Text b -> a = b
     | Attribute.Float2 a, Attribute.Float2 b ->
         Packed.Float2.View.view a = Packed.Float2.View.view b
     | Attribute.Float3 a, Attribute.Float3 b ->
         Packed.Float3.View.view a = Packed.Float3.View.view b
     | Attribute.Float4 a, Attribute.Float4 b ->
         Packed.Float4.View.view a = Packed.Float4.View.view b
     | Attribute.Int_array a, Attribute.Int_array b ->
         Packed.Int_array.View.view a = Packed.Int_array.View.view b
     | Attribute.Float_array a, Attribute.Float_array b ->
         Packed.Float_array.View.view a = Packed.Float_array.View.view b
     | _ -> false

let open_sections () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.; 0.;0.6;1.4;2.|]
      ~y:[|0.;0.;0.; 1.;1.;1.;1.|]
      ~z:[|0.;0.;0.; 0.;0.2;0.2;0.|] in
  let topology = Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1;2; 3;4;5;6|]
      ~primitive_offsets:[|0;3;7|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Open_polyline|]
      |> get in
  let point = Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float (Array.init 7 float_of_int)) |> get
  and vertex = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int_array (Packed.Int_array.create_owned
        ~offsets:(Array.init 8 (fun value -> value * 2))
        ~values:(Array.init 14 (fun value -> 100 + value)) |> get)) |> get
  and primitive = Attribute.create_owned ~owner:Attribute.Primitive ~name:"section"
      (Attribute.Int [|10;11|]) |> get
  and vertices = Group.ordered ~owner:Group.Vertex ~name:"curve_vertices"
      ~length:7 [|6;5;4;3;2;1;0|] |> get
  and primitives = Group.ordered ~owner:Group.Primitive ~name:"sections"
      ~length:2 [|1;0|] |> get in
  let geometry = Geometry.create ~positions ~topology
      ~attributes:[point;vertex;primitive] ~groups:[vertices;primitives] () |> get in
  let index = Topology_index.create topology in
  let edges = Edge_group.init ~grain:1 ~topology ~index ~name:"section_edges"
      (fun _ -> true) in
  Geometry.with_edge_group edges geometry |> get

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.View.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let ring_sections ?(reverse_second = false)
    ?(kind = Topology.Closed_polyline) count =
  let points = count * 4 in
  let x = Array.make points 0. and y = Array.make points 0.
  and z = Array.make points 0. in
  let corners = [|(-1.,-1.); (1.,-1.); (1.,1.); (-1.,1.)|] in
  for section = 0 to count - 1 do
    for local = 0 to 3 do
      let point = (section * 4) + local in
      x.(point) <- fst corners.(local); y.(point) <- float_of_int section;
      z.(point) <- snd corners.(local)
    done
  done;
  let vertex_points = Array.init points (fun vertex ->
    let section = vertex / 4 and local = vertex mod 4 in
    if reverse_second && section = 1 then section * 4 + ((5 - local) mod 4)
    else vertex) in
  let topology = Topology.create_owned ~point_count:points ~vertex_points
      ~primitive_offsets:(Array.init (count + 1) (fun value -> value * 4))
      ~primitive_kinds:(Array.make count kind) |> get in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get

let many_sections () =
  let sections = 96 and per_section = 129 in
  let count = sections * per_section in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  for section = 0 to sections - 1 do
    for local = 0 to per_section - 1 do
      let point = (section * per_section) + local
      and angle = 2. *. Float.pi *. float_of_int local
          /. float_of_int per_section in
      let radius = 1. +. (0.08 *. sin (float_of_int section *. 0.17)) in
      x.(point) <- radius *. cos angle;
      y.(point) <- float_of_int section *. 0.025;
      z.(point) <- radius *. sin angle
    done
  done;
  let topology = Topology.create_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id)
      ~primitive_offsets:(Array.init (sections + 1)
        (fun value -> value * per_section))
      ~primitive_kinds:(Array.make sections Topology.Closed_polyline) |> get in
  let rows = Packed.Int_array.create_owned
      ~offsets:(Array.init (count + 1) (fun value -> value))
      ~values:(Array.init count (fun value -> value mod 31)) |> get in
  let attribute = Attribute.create_owned ~owner:Attribute.Vertex ~name:"row"
      (Attribute.Int_array rows) |> get
  and group = Group.ordered ~owner:Group.Primitive ~name:"ordered_sections"
      ~length:sections (Array.init sections (fun value -> sections - value - 1))
      |> get in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology ~attributes:[attribute] ~groups:[group] () |> get

let run () =
  print_endline "test_poly_loft: ok"
