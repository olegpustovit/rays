open Rdk

let fail message = prerr_endline message; exit 1
let check condition message = if not condition then fail message
let get_ok = function Ok value -> value | Error error ->
  fail (Error.to_string error)

let positions geometry =
  Packed.Float3.Private.view (Geometry.positions geometry)

let close left right = abs_float (left -. right) <= 1e-12
let array_close left right =
  Array.length left = Array.length right
  && Array.for_all2 close left right

let attribute owner name storage geometry =
  let value = Attribute.create_owned ~owner ~name storage |> function
    | Ok value -> value | Error message -> fail message in
  Geometry.with_attribute value geometry |> function
  | Ok value -> value | Error message -> fail message

let points xs =
  let count = Array.length xs in
  let positions = Packed.Float3.Private.of_owned_exn ~x:(Array.copy xs)
      ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
  Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) ()
  |> function Ok value -> value | Error message -> fail message

let point_group name count members =
  let builder = Group.Builder.create ~owner:Group.Point ~name count in
  List.iter (fun point -> Group.Builder.set builder point true) members;
  Group.Builder.freeze builder

let primitive_group name count =
  let builder = Group.Builder.create ~owner:Group.Primitive ~name count in
  Group.Builder.freeze builder

let point_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let test_weights_and_selection () =
  let source = points [|0.;1.;2.|]
  and first = points [|10.;11.;12.|]
  and second = points [|20.;21.;22.|] in
  let quarter = Blend_shapes.run
      ~shapes:[Blend_shapes.shape ~weight:0.25 first] source |> get_ok in
  check (array_close (positions quarter).x [|2.5;3.5;4.5|])
    "Blend Shapes single normalized weight";
  let identity = Blend_shapes.run
      ~shapes:[Blend_shapes.shape ~weight:0. first] source |> get_ok in
  check (identity == source) "Blend Shapes zero weight was not identity";
  let normalized = Blend_shapes.run ~shapes:[
      Blend_shapes.shape ~weight:0.75 first;
      Blend_shapes.shape ~weight:0.75 second] source |> get_ok in
  check (array_close (positions normalized).x [|15.;16.;17.|])
    "Blend Shapes normalized target average";
  let differenced = Blend_shapes.run ~mode:Blend_shapes.Blend_differencing ~shapes:[
      Blend_shapes.shape ~weight:1.5 first;
      Blend_shapes.shape ~weight:(-0.5) second] source |> get_ok in
  check (array_close (positions differenced).x [|5.;6.;7.|])
    "Blend Shapes differencing extrapolation";
  let selection = point_group "middle" 3 [1] in
  let selected = Blend_shapes.run ~points:selection
      ~shapes:[Blend_shapes.shape ~weight:1. first] source |> get_ok in
  check ((positions selected).x = [|0.;11.;2.|])
    "Blend Shapes point restriction"

let test_masks () =
  let source = points [|0.;1.;2.|]
      |> attribute Attribute.Point "mask" (Attribute.Float [|0.;0.5;1.|]) in
  let target = points [|10.;11.;12.|]
      |> attribute Attribute.Point "mask" (Attribute.Float [|1.;0.5;0.|]) in
  let first_mask = Blend_shapes.run
      ~masking:Blend_shapes.Blend_scale_from_attribute ~mask_attribute:"mask"
      ~shapes:[Blend_shapes.shape ~weight:0.5
        ~mask_source:Blend_shapes.Blend_mask_first_input target] source |> get_ok in
  check (array_close (positions first_mask).x [|0.;3.5;7.|])
    "Blend Shapes first-input scale mask";
  let set_mask = Blend_shapes.run ~masking:Blend_shapes.Blend_set_from_attribute
      ~mask_attribute:"mask"
      ~shapes:[Blend_shapes.shape ~weight:0.125
        ~mask_source:Blend_shapes.Blend_mask_first_input target] source |> get_ok in
  check (array_close (positions set_mask).x [|0.;6.;12.|])
    "Blend Shapes set mask";
  let target_mask = Blend_shapes.run
      ~masking:Blend_shapes.Blend_scale_from_attribute ~mask_attribute:"mask"
      ~shapes:[Blend_shapes.shape ~weight:0.5 target] source |> get_ok in
  check (array_close (positions target_mask).x [|5.;3.5;2.|])
    "Blend Shapes target scale mask"

let test_ids_and_attributes () =
  let source = points [|0.;1.;2.|]
      |> attribute Attribute.Point "id" (Attribute.Int [|10;20;30|])
      |> attribute Attribute.Point "density" (Attribute.Float [|0.;2.;4.|])
      |> attribute Attribute.Point "uv" (Attribute.Float2
        (Packed.Float2.of_owned ~x:[|0.;0.;0.|] ~y:[|0.;1.;2.|]
          |> function Ok value -> value | Error message -> fail message))
      |> attribute Attribute.Point "Cd" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|]))
      |> attribute Attribute.Point "orient" (Attribute.Float4
        (Packed.Float4.of_owned ~x:[|0.;0.;0.|] ~y:[|0.;0.;0.|]
          ~z:[|0.;0.;0.|] ~w:[|1.;1.;1.|]
          |> function Ok value -> value | Error message -> fail message))
      |> attribute Attribute.Point "tag" (Attribute.Text [|"a";"b";"c"|]) in
  let target = points [|300.;100.|]
      |> attribute Attribute.Point "id" (Attribute.Int [|30;10|])
      |> attribute Attribute.Point "density" (Attribute.Float [|30.;10.|])
      |> attribute Attribute.Point "uv" (Attribute.Float2
        (Packed.Float2.of_owned ~x:[|3.;1.|] ~y:[|30.;10.|]
          |> function Ok value -> value | Error message -> fail message))
      |> attribute Attribute.Point "Cd" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|3.;1.|]
          ~y:[|6.;2.|] ~z:[|9.;3.|]))
      |> attribute Attribute.Point "orient" (Attribute.Float4
        (Packed.Float4.of_owned ~x:[|0.6;0.2|] ~y:[|0.;0.|]
          ~z:[|0.;0.|] ~w:[|0.8;0.8|]
          |> function Ok value -> value | Error message -> fail message)) in
  let output = Blend_shapes.run ~point_id_attribute:"id"
      ~shapes:[Blend_shapes.shape ~weight:0.5 target] source |> get_ok in
  check (array_close (positions output).x [|50.;1.;151.|])
    "Blend Shapes ID position matching";
  check (array_close (point_float "density" output) [|5.;2.;17.|])
    "Blend Shapes scalar attribute matching";
  let uv = Geometry.find_attribute ~owner:Attribute.Point "uv" output
      |> Option.get |> Attribute.Private.storage in
  let cd = Geometry.find_attribute ~owner:Attribute.Point "Cd" output
      |> Option.get |> Attribute.Private.storage in
  let orient = Geometry.find_attribute ~owner:Attribute.Point "orient" output
      |> Option.get |> Attribute.Private.storage in
  (match uv, cd, orient with
   | Attribute.Float2 uv, Attribute.Float3 cd, Attribute.Float4 orient ->
       let uv = Packed.Float2.Private.view uv
       and cd = Packed.Float3.Private.view cd
       and orient = Packed.Float4.Private.view orient in
       check (array_close uv.x [|0.5;0.;1.5|]
           && array_close cd.y [|1.;0.;3.|]
           && array_close orient.x [|0.1;0.;0.3|])
         "Blend Shapes packed tuple attributes"
   | _ -> fail "Blend Shapes changed tuple storage");
  check (match Geometry.find_attribute ~owner:Attribute.Point "tag" output with
    | Some tag -> Attribute.Private.storage tag = Attribute.Text [|"a";"b";"c"|]
    | None -> false) "Blend Shapes changed discrete source attribute";
  let text_source = points [|0.;1.;2.|]
      |> attribute Attribute.Point "name" (Attribute.Text [|"a";"b";"c"|])
  and text_target = points [|30.;10.|]
      |> attribute Attribute.Point "name" (Attribute.Text [|"c";"a"|]) in
  let text_output = Blend_shapes.run ~point_id_attribute:"name"
      ~shapes:[Blend_shapes.shape ~weight:1. text_target] text_source |> get_ok in
  check ((positions text_output).x = [|10.;1.;30.|])
    "Blend Shapes text ID matching"

let test_normals_and_errors () =
  let source = points [|0.;1.|]
      |> attribute Attribute.Point "N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|0.;0.|] ~y:[|1.;1.|]
          ~z:[|0.;0.|]))
      |> attribute Attribute.Vertex "N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[||] ~y:[||] ~z:[||])) in
  let target = points [|2.;3.|] in
  let output = Blend_shapes.run ~attributes:"Cd"
      ~shapes:[Blend_shapes.shape ~weight:1. target] source |> get_ok in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
    "Blend Shapes retained stale normals";
  let expect work = match work () with
    | Error error -> check (Error.code error = "invalid_blend_shapes")
        "Blend Shapes wrong structured error"
    | Ok _ -> fail "Blend Shapes accepted malformed input" in
  expect (fun () -> Blend_shapes.run
    ~shapes:[Blend_shapes.shape ~weight:1. (points [|1.|])] source);
  expect (fun () -> Blend_shapes.run ~points:(primitive_group "bad" 0)
    ~shapes:[Blend_shapes.shape ~weight:1. target] source);
  expect (fun () -> Blend_shapes.run ~points:(point_group "short" 1 [0])
    ~shapes:[Blend_shapes.shape ~weight:1. target] source);
  expect (fun () -> Blend_shapes.run ~attributes:"["
    ~shapes:[Blend_shapes.shape ~weight:1. target] source);
  let missing_id = attribute Attribute.Point "id" (Attribute.Int [|0;1|])
      source in
  expect (fun () -> Blend_shapes.run ~point_id_attribute:"id"
    ~shapes:[Blend_shapes.shape ~weight:1. target] missing_id);
  let duplicate_id = target
      |> attribute Attribute.Point "id" (Attribute.Int [|1;1|]) in
  expect (fun () -> Blend_shapes.run ~point_id_attribute:"id"
    ~shapes:[Blend_shapes.shape ~weight:1. duplicate_id] missing_id);
  let bad_mask = target
      |> attribute Attribute.Point "mask" (Attribute.Float [|0.;nan|]) in
  expect (fun () -> Blend_shapes.run ~masking:Blend_shapes.Blend_scale_from_attribute
    ~mask_attribute:"mask" ~shapes:[Blend_shapes.shape ~weight:1. bad_mask] source);
  let wrong_attribute = target
      |> attribute Attribute.Point "value" (Attribute.Int [|1;2|]) in
  let float_source = source
      |> attribute Attribute.Point "value" (Attribute.Float [|1.;2.|]) in
  expect (fun () -> Blend_shapes.run
    ~shapes:[Blend_shapes.shape ~weight:1. wrong_attribute] float_source);
  let huge = points [|Float.max_float;Float.max_float|] in
  expect (fun () -> Blend_shapes.run ~mode:Blend_shapes.Blend_differencing
    ~shapes:[Blend_shapes.shape ~weight:2. huge] source);
  let cancel = Cancel.create () in Cancel.cancel cancel;
  (match Blend_shapes.run ~cancel
      ~shapes:[Blend_shapes.shape ~weight:1. target] source with
   | Error error -> check (Error.code error = "cancelled")
       "Blend Shapes cancellation code"
   | Ok _ -> fail "Blend Shapes ignored cancellation")

let test_parallel_scale () =
  let count = 100_000 in
  let source = points (Array.init count (fun point -> float_of_int point *. 0.01))
      |> attribute Attribute.Point "density"
        (Attribute.Float (Array.init count (fun point -> float_of_int (point mod 17)))) in
  let target = points (Array.init count (fun point ->
      10. +. float_of_int point *. 0.02))
      |> attribute Attribute.Point "density"
        (Attribute.Float (Array.init count (fun point -> float_of_int (point mod 23)))) in
  let run domains = Parallel.run ~domains (fun () ->
    Blend_shapes.run ~grain:127 ~shapes:[Blend_shapes.shape ~weight:0.37 target]
      source |> get_ok) in
  let one = run 1 and four = run 4 in
  check ((positions one).x = (positions four).x
      && point_float "density" one = point_float "density" four)
    "Blend Shapes differs across domain counts";
  check (Geometry.topology one == Geometry.topology source
      && Geometry.point_count one = count)
    "Blend Shapes scale cardinality or topology sharing"

let run () =
  test_weights_and_selection ();
  test_masks ();
  test_ids_and_attributes ();
  test_normals_and_errors ();
  test_parallel_scale ();
  print_endline "Blend Shapes tests passed"
