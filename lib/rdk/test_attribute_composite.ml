open Rdk

let fail message = prerr_endline message; exit 1
let check condition message = if not condition then fail message
let get_ok = function Ok value -> value | Error error ->
  fail (Error.to_string error)

let close left right = Float.abs (left -. right) <= 1e-12
let array_close left right =
  Array.length left = Array.length right && Array.for_all2 close left right

let triangle xs =
  let topology = Rdk_test_support.Tb.create ~point_count:3 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  let positions = Packed.Float3.Private.of_owned_exn ~x:(Array.copy xs)
      ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|] in
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> Result.get_ok

let points xs =
  let count = Array.length xs in
  Geometry.create
    ~positions:(Packed.Float3.Private.of_owned_exn ~x:(Array.copy xs)
      ~y:(Array.make count 0.) ~z:(Array.make count 0.))
    ~topology:(Topology.empty ~point_count:count) () |> Result.get_ok

let add ~owner ~name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun attribute -> Geometry.with_attribute attribute geometry |> Result.get_ok

let scalar ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let vector2 ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values -> Packed.Float2.Private.view values
       | _ -> fail ("wrong storage for " ^ name))
  | None -> fail ("missing attribute " ^ name)

let vector4 ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float4 values -> Packed.Float4.Private.view values
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
        (Packed.Float3.Private.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|1.;1.;1.|] ~z:[|0.;0.;0.|]))
      |> add ~owner:Attribute.Vertex ~name:"N" (Attribute.Float3
        (Packed.Float3.Private.of_owned_exn ~x:[|0.;0.;0.|]
          ~y:[|1.;1.;1.|] ~z:[|0.;0.;0.|]))
      |> add ~owner:Attribute.Point ~name:"id" (Attribute.Int [|1;2;3|]) in
  let second = triangle [|10.;11.;12.|] |> add_common ~target:true
      |> add ~owner:Attribute.Point ~name:"target_only"
        (Attribute.Float [|8.;8.;8.|]) in
  first, second

let test_mean_patterns_owners_and_position () =
  let first, second = fixtures () in
  let output = Attribute_composite.run ~operation:Attribute_composite.Composite_mean
      ~allow_position:true ~alpha_attribute:"a"
      ~point_attributes:"P density uv Cd solo target_only"
      ~vertex_attributes:"vertv" ~primitive_attributes:"primv"
      ~detail_attributes:"detailv"
      ~inputs:[Attribute_composite.input ~weight:1. second] first
    |> get_ok in
  check (array_close (Packed.Float3.Private.view
      (Geometry.positions output)).x [|5.;1.;12.|])
    "Attribute Composite mean P/alpha";
  check (array_close (scalar ~owner:Attribute.Point "density" output)
      [|6.;4.;30.|]) "Attribute Composite mean scalar";
  check (array_close (scalar ~owner:Attribute.Point "solo" output)
      [|4.;8.;0.|]) "Attribute Composite missing target attribute";
  check (array_close (scalar ~owner:Attribute.Point "target_only" output)
      [|4.;0.;8.|]) "Attribute Composite target-only attribute";
  let uv = vector2 ~owner:Attribute.Point "uv" output
  and cd = vector4 ~owner:Attribute.Point "Cd" output in
  check (array_close uv.x [|6.;4.;30.|]
      && array_close uv.y [|12.;8.;60.|]
      && array_close cd.z [|18.;12.;90.|]
      && array_close cd.w [|3.;1.;5.|])
    "Attribute Composite fixed-width vectors";
  check (array_close (scalar ~owner:Attribute.Vertex "vertv" output)
      [|2.;3.;4.|]
      && array_close (scalar ~owner:Attribute.Primitive "primv" output) [|4.|]
      && array_close (scalar ~owner:Attribute.Detail "detailv" output) [|12.|])
    "Attribute Composite owner domains";
  check (scalar ~owner:Attribute.Point "a" output = [|1.;1.;0.|])
    "Attribute Composite modified an excluded alpha attribute";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
    "Attribute Composite retained stale normals after P";
  check (Geometry.topology output == Geometry.topology first)
    "Attribute Composite rebuilt topology";
  let source_id = Geometry.find_attribute ~owner:Attribute.Point "id" first
      |> Option.get
  and output_id = Geometry.find_attribute ~owner:Attribute.Point "id" output
      |> Option.get in
  check (Attribute.storage_id source_id = Attribute.storage_id output_id)
    "Attribute Composite copied an untouched discrete field";
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_composite.run ~grain:1 ~operation:Attribute_composite.Composite_mean
      ~allow_position:true ~alpha_attribute:"a"
      ~point_attributes:"P density uv Cd solo target_only"
      ~vertex_attributes:"vertv" ~primitive_attributes:"primv"
      ~detail_attributes:"detailv"
      ~inputs:[Attribute_composite.input ~weight:1. second] first
    |> get_ok) in
  let one = run 1 and four = run 4 in
  let same_scalar owner name =
    scalar ~owner name one = scalar ~owner name four in
  let one_uv = vector2 ~owner:Attribute.Point "uv" one
  and four_uv = vector2 ~owner:Attribute.Point "uv" four
  and one_cd = vector4 ~owner:Attribute.Point "Cd" one
  and four_cd = vector4 ~owner:Attribute.Point "Cd" four in
  check (Packed.Float3.Private.view (Geometry.positions one)
        = Packed.Float3.Private.view (Geometry.positions four)
      && same_scalar Attribute.Point "density"
      && same_scalar Attribute.Point "solo"
      && same_scalar Attribute.Point "target_only"
      && one_uv = four_uv && one_cd = four_cd
      && same_scalar Attribute.Vertex "vertv"
      && same_scalar Attribute.Primitive "primv"
      && same_scalar Attribute.Detail "detailv")
    "Attribute Composite owner/component output differs across domain counts"

let test_identity_and_explicit_normals () =
  let first, second = fixtures () in
  let identity = Attribute_composite.run ~point_attributes:"^*"
      ~vertex_attributes:"^*" ~primitive_attributes:"^*"
      ~detail_attributes:"^*"
      ~inputs:[Attribute_composite.input ~weight:1. second] first
    |> get_ok in
  check (identity == first) "Attribute Composite empty selection was not identity";
  let output = Attribute_composite.run ~allow_position:true
      ~point_attributes:"P N" ~vertex_attributes:"^*"
      ~primitive_attributes:"^*" ~detail_attributes:"^*"
      ~inputs:[Attribute_composite.input ~weight:1. second] first
    |> get_ok in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output <> None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
    "Attribute Composite did not distinguish explicit point normals"

let test_extremes_and_alpha_folds () =
  let first, second = fixtures () in
  let input = Attribute_composite.input ~weight:(-1.) second in
  let maximum = Attribute_composite.run ~operation:Attribute_composite.Composite_maximum
      ~weight:2. ~point_attributes:"density" ~vertex_attributes:"^*"
      ~primitive_attributes:"^*" ~detail_attributes:"^*" ~inputs:[input] first
    |> get_ok
  and minimum = Attribute_composite.run ~operation:Attribute_composite.Composite_minimum
      ~weight:2. ~point_attributes:"density" ~vertex_attributes:"^*"
      ~primitive_attributes:"^*" ~detail_attributes:"^*" ~inputs:[input] first
    |> get_ok in
  check (array_close (scalar ~owner:Attribute.Point "density" maximum)
      [|4.;8.;12.|]) "Attribute Composite maximum";
  check (array_close (scalar ~owner:Attribute.Point "density" minimum)
      [|-10.;-20.;-30.|]) "Attribute Composite minimum";
  let weighted = Attribute_composite.input ~weight:1. second in
  let over = Attribute_composite.run ~operation:Attribute_composite.Composite_over ~weight:0.5
      ~alpha_attribute:"a" ~point_attributes:"density"
      ~vertex_attributes:"^*" ~primitive_attributes:"^*"
      ~detail_attributes:"^*" ~inputs:[weighted] first |> get_ok
  and under = Attribute_composite.run ~operation:Attribute_composite.Composite_under ~weight:0.5
      ~alpha_attribute:"a" ~point_attributes:"density"
      ~vertex_attributes:"^*" ~primitive_attributes:"^*"
      ~detail_attributes:"^*" ~inputs:[weighted] first |> get_ok in
  check (array_close (scalar ~owner:Attribute.Point "density" over)
      [|10.;2.;30.|]) "Attribute Composite Over";
  check (array_close (scalar ~owner:Attribute.Point "density" under)
      [|1.;20.;3.|]) "Attribute Composite Under";
  let no_alpha = triangle [|10.;11.;12.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float [|10.;20.;30.|]) in
  let input = Attribute_composite.input ~weight:1. no_alpha in
  let over = Attribute_composite.run ~operation:Attribute_composite.Composite_over
      ~alpha_attribute:"missing" ~point_attributes:"density"
      ~vertex_attributes:"^*" ~primitive_attributes:"^*"
      ~detail_attributes:"^*" ~inputs:[input] first |> get_ok
  and under = Attribute_composite.run ~operation:Attribute_composite.Composite_under
      ~alpha_attribute:"missing" ~point_attributes:"density"
      ~vertex_attributes:"^*" ~primitive_attributes:"^*"
      ~detail_attributes:"^*" ~inputs:[input] first |> get_ok in
  check (array_close (scalar ~owner:Attribute.Point "density" over)
      [|10.;20.;30.|] && array_close
      (scalar ~owner:Attribute.Point "density" under) [|2.;4.;6.|])
    "Attribute Composite missing alpha default"

let expect_error ?(code = "invalid_attribute_composite") work =
  match work () with
  | Error error -> check (String.equal (Error.code error) code)
      "Attribute Composite wrong structured error"
  | Ok _ -> fail "Attribute Composite accepted malformed input"

let test_errors_and_cancellation () =
  let first, second = fixtures () in
  expect_error (fun () -> Attribute_composite.run ~point_attributes:"["
    ~inputs:[Attribute_composite.input ~weight:1. second] first);
  expect_error (fun () -> Attribute_composite.run ~grain:0
    ~inputs:[Attribute_composite.input ~weight:1. second] first);
  expect_error (fun () -> Attribute_composite.run ~weight:nan
    ~inputs:[Attribute_composite.input ~weight:1. second] first);
  expect_error (fun () -> Attribute_composite.run ~alpha_attribute:" "
    ~inputs:[Attribute_composite.input ~weight:1. second] first);
  let short = points [|0.;1.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float [|1.;2.|]) in
  expect_error (fun () -> Attribute_composite.run ~point_attributes:"density"
    ~vertex_attributes:"^*" ~primitive_attributes:"^*"
    ~detail_attributes:"^*"
    ~inputs:[Attribute_composite.input ~weight:1. short] first);
  let wrong = triangle [|0.;1.;2.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float2 (Packed.Float2.of_owned ~x:[|1.;2.;3.|]
          ~y:[|1.;2.;3.|] |> Result.get_ok)) in
  expect_error (fun () -> Attribute_composite.run ~point_attributes:"density"
    ~vertex_attributes:"^*" ~primitive_attributes:"^*"
    ~detail_attributes:"^*"
    ~inputs:[Attribute_composite.input ~weight:1. wrong] first);
  let bad_alpha = triangle [|0.;1.;2.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float [|1.;2.;3.|])
      |> add ~owner:Attribute.Point ~name:"a"
        (Attribute.Float [|1.;nan;1.|]) in
  expect_error (fun () -> Attribute_composite.run ~alpha_attribute:"a"
    ~point_attributes:"density" ~vertex_attributes:"^*"
    ~primitive_attributes:"^*" ~detail_attributes:"^*"
    ~inputs:[Attribute_composite.input ~weight:1. bad_alpha] first);
  let wrong_alpha = triangle [|0.;1.;2.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float [|1.;2.;3.|])
      |> add ~owner:Attribute.Point ~name:"a" (Attribute.Int [|1;1;1|]) in
  expect_error (fun () -> Attribute_composite.run ~alpha_attribute:"a"
    ~point_attributes:"density" ~vertex_attributes:"^*"
    ~primitive_attributes:"^*" ~detail_attributes:"^*"
    ~inputs:[Attribute_composite.input ~weight:1. wrong_alpha] first);
  let bad_source = triangle [|0.;1.;2.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float [|1.;nan;3.|]) in
  expect_error (fun () -> Attribute_composite.run ~point_attributes:"density"
    ~vertex_attributes:"^*" ~primitive_attributes:"^*"
    ~detail_attributes:"^*"
    ~inputs:[Attribute_composite.input ~weight:1. bad_source] first);
  let huge = triangle [|0.;1.;2.|]
      |> add ~owner:Attribute.Point ~name:"density"
        (Attribute.Float [|Float.max_float;1.;1.|]) in
  expect_error (fun () -> Attribute_composite.run
    ~operation:Attribute_composite.Composite_maximum ~point_attributes:"density"
    ~vertex_attributes:"^*" ~primitive_attributes:"^*"
    ~detail_attributes:"^*" ~weight:Float.max_float
    ~inputs:[Attribute_composite.input ~weight:1. huge] first);
  expect_error (fun () -> Attribute_composite.run
    ~operation:Attribute_composite.Composite_mean ~point_attributes:"density"
    ~vertex_attributes:"^*" ~primitive_attributes:"^*"
    ~detail_attributes:"^*" ~weight:Float.max_float
    ~inputs:[Attribute_composite.input ~weight:Float.max_float second]
    first);
  check (try ignore (Attribute_composite.input ~weight:nan second); false
    with Invalid_argument _ -> true)
    "Attribute Composite accepted non-finite descriptor weight";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect_error ~code:"cancelled" (fun () -> Attribute_composite.run ~cancel
    ~inputs:[Attribute_composite.input ~weight:1. second] first)

let test_parallel_scale () =
  let count = 100_000 in
  let make offset =
    let positions = Array.init count (fun point ->
      offset +. (float_of_int point *. 0.001))
    and values = Array.init count (fun point ->
      offset +. float_of_int (point mod 101))
    and alpha = Array.init count (fun point ->
      float_of_int (point mod 7) /. 6.) in
    points positions
    |> add ~owner:Attribute.Point ~name:"value"
      (Attribute.Float values)
    |> add ~owner:Attribute.Point ~name:"alpha"
      (Attribute.Float alpha) in
  let first = make 0. and second = make 10. and third = make (-5.) in
  let run domains = Parallel.run ~domains (fun () ->
    Attribute_composite.run ~grain:127 ~operation:Attribute_composite.Composite_mean
      ~weight:0.2 ~alpha_attribute:"alpha" ~point_attributes:"P value"
      ~vertex_attributes:"^*" ~primitive_attributes:"^*"
      ~detail_attributes:"^*" ~allow_position:true
      ~inputs:[Attribute_composite.input ~weight:0.3 second;
        Attribute_composite.input ~weight:0.5 third] first |> get_ok) in
  let one = run 1 and four = run 4 in
  let one_p = Packed.Float3.Private.view (Geometry.positions one)
  and four_p = Packed.Float3.Private.view (Geometry.positions four) in
  check (one_p.x = four_p.x
      && scalar ~owner:Attribute.Point "value" one
        = scalar ~owner:Attribute.Point "value" four)
    "Attribute Composite differs across domain counts";
  check (Geometry.point_count one = count
      && Geometry.topology one == Geometry.topology first
      && Geometry.payload_bytes one = Geometry.payload_bytes first)
    "Attribute Composite scale cardinality/topology sharing"

let run () =
  test_mean_patterns_owners_and_position ();
  test_identity_and_explicit_normals ();
  test_extremes_and_alpha_folds ();
  test_errors_and_cancellation ();
  test_parallel_scale ();
  print_endline "Attribute Composite tests passed"
