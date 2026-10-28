open Rdk_test_support
open Rdk


let close left right = Float.abs (left -. right) <= 1e-12
let array_close left right =
  Array.length left = Array.length right && Array.for_all2 close left right

let triangle xs =
  let topology = Rdk_test_support.Tb.create ~point_count:3 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  let positions = Packed.Float3.View.of_owned_exn ~x:(Array.copy xs)
      ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|] in
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> Result.get_ok

let points xs =
  let count = Array.length xs in
  Geometry.create
    ~positions:(Packed.Float3.View.of_owned_exn ~x:(Array.copy xs)
      ~y:(Array.make count 0.) ~z:(Array.make count 0.))
    ~topology:(Topology.empty ~point_count:count) () |> Result.get_ok

let add ~owner ~name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun attribute -> Geometry.with_attribute attribute geometry |> Result.get_ok

let scalar ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let vector2 ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.View.view values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let vector4 ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float4 values -> Packed.Float4.View.view values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let add_common ~target geometry =
  let point_alpha = if target then [|1.;0.;1.|] else [|1.;1.;0.|] in
  let density = if target then [|10.;20.;30.|] else [|2.;4.;6.|] in
  let scale = if target then 5. else 1. in
  geometry
  |> add ~owner:Attribute.Point ~name:"a" (Attribute.Float point_alpha)
  |> add ~owner:Attribute.Point ~name:"density" (Attribute.Float density)
  |> add ~owner:Attribute.Point ~name:"uv" (Attribute.Float2
      (Packed.Float2.of_owned ~x:(Array.copy density)
        ~y:(Array.map (fun value -> value *. 2.) density) |> Result.get_ok))
  |> add ~owner:Attribute.Point ~name:"Cd" (Attribute.Float4
      (Packed.Float4.of_owned ~x:(Array.copy density)
        ~y:(Array.map (fun value -> value *. 2.) density)
        ~z:(Array.map (fun value -> value *. 3.) density)
        ~w:(Array.make 3 scale) |> Result.get_ok))
  |> add ~owner:Attribute.Vertex ~name:"vertv"
      (Attribute.Float (if target then [|3.;4.;5.|] else [|1.;2.;3.|]))
  |> add ~owner:Attribute.Vertex ~name:"a" (Attribute.Float [|1.;1.;1.|])
  |> add ~owner:Attribute.Primitive ~name:"primv"
      (Attribute.Float [|if target then 6. else 2.|])
  |> add ~owner:Attribute.Primitive ~name:"a" (Attribute.Float [|1.|])
  |> add ~owner:Attribute.Detail ~name:"detailv"
      (Attribute.Float [|if target then 14. else 10.|])
  |> add ~owner:Attribute.Detail ~name:"a" (Attribute.Float [|1.|])

let fixtures () =
  let first = triangle [|0.;1.;2.|] |> add_common ~target:false
      |> add ~owner:Attribute.Point ~name:"solo"
        (Attribute.Float [|8.;8.;8.|])
      |> add ~owner:Attribute.Point ~name:"N" (Attribute.Float3
        (Packed.Float3.View.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|1.;1.;1.|] ~z:[|0.;0.;0.|]))
      |> add ~owner:Attribute.Vertex ~name:"N" (Attribute.Float3
        (Packed.Float3.View.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|1.;1.;1.|] ~z:[|0.;0.;0.|]))
      |> add ~owner:Attribute.Point ~name:"id" (Attribute.Int [|1;2;3|]) in
  let second = triangle [|10.;11.;12.|] |> add_common ~target:true
      |> add ~owner:Attribute.Point ~name:"target_only"
        (Attribute.Float [|8.;8.;8.|]) in
  first, second

let expect_error ?(code = "invalid_attribute_composite") work =
  match work () with
  | Error error -> check (String.equal (Error.code error) code)
      "Attribute Composite wrong structured error"
  | Ok _ -> fail "Attribute Composite accepted malformed input"

let run () =
  print_endline "Attribute Composite tests passed"
