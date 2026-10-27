open Rays
open Rdk

let fail message = prerr_endline message; exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let points values = Line_geometry.points values
let point_int name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Int values -> values | _ -> fail (name ^ " wrong storage"))
  | None -> fail ("missing " ^ name)
let point_float3 name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Float3 values -> Packed.Float3.Private.view values
      | _ -> fail (name ^ " wrong storage"))
  | None -> fail ("missing " ^ name)

let check_close actual expected message =
  if abs_float (actual -. expected) > 1e-12 then
    fail (Printf.sprintf "%s: expected %.17g, got %.17g"
      message expected actual)

let test_shapes_and_quantity () =
  let source = points [|0.,0.,0.; 10.,0.,0.|]
      |> Geometry.with_attribute (attribute Attribute.Point "density"
           (Attribute.Float [|2.;1.|])) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "id"
           (Attribute.Int [|70;90|])) |> Result.get_ok in
  let test shape label predicate =
    let output = Point_replication.run ~seed:(Rays_math.Rand.seed 41)
        ~keep_source_attributes:true ~shape ~size:(Rays_math.Vec3.create 2. 4. 6.)
        ~points_per_point:2. ~scale_attribute:"density" source |> get in
    check (Geometry.point_count output = 6
        && point_int "sourcepoint" output = [|0;0;0;0;1;1|]
        && point_int "sourceindex" output = [|0;1;2;3;0;1|])
      (label ^ " cardinality/provenance");
    let positions = Packed.Float3.Private.view (Geometry.positions output) in
    for point = 0 to 5 do
      let source_x = if point < 4 then 0. else 10. in
      check (predicate (positions.x.(point) -. source_x)
          positions.y.(point) positions.z.(point))
        (label ^ " shape bounds")
    done in
  test Point_replication.Replicate_box "box" (fun x y z ->
      abs_float x <= 1. && abs_float y <= 2. && abs_float z <= 3.);
  test Point_replication.Replicate_sphere "sphere" (fun x y z ->
      let x = x /. 2. and y = y /. 4. and z = z /. 6. in
      x*.x +. y*.y +. z*.z <= 0.250000000001);
  test Point_replication.Replicate_disk "disk" (fun x y z ->
      let x = x /. 2. and y = y /. 4. in
      x*.x +. y*.y <= 0.250000000001 && z = 0.);
  test Point_replication.Replicate_line "line" (fun x y z ->
      x = 0. && y = 0. && abs_float z <= 3.)

let test_point_shape_and_transformed_attributes () =
  let source = points [|10.,20.,30.|]
      |> Geometry.with_attribute (attribute Attribute.Point "scale"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|2.|] ~y:[|1.|] ~z:[|0.5|]))) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "orient"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:[|0.|] ~y:[|0.|] ~z:[|0.|] ~w:[|1.|] |> Result.get_ok)))
         |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "N"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.|] ~y:[|1.|] ~z:[|1.|]))) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "flow"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.|] ~y:[|2.|] ~z:[|3.|]))) |> Result.get_ok in
  let unchanged = Point_replication.run ~shape:Point_replication.Replicate_point
      ~points_per_point:2. source |> get in
  let positions = Packed.Float3.Private.view (Geometry.positions unchanged)
  and flow = point_float3 "flow" unchanged in
  check (positions.x = [|10.;10.|] && positions.y = [|20.;20.|]
      && positions.z = [|30.;30.|]) "Point Replicate point preset";
  check (flow.x = [|1.;1.|] && flow.y = [|2.;2.|]
      && flow.z = [|3.;3.|]) "Point Replicate default P-only transform pattern";
  let source = source |> Geometry.with_attribute
      (attribute Attribute.Point "v"
        (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[|4.|] ~y:[|5.|] ~z:[|6.|]))) |> Result.get_ok in
  let transformed = Point_replication.run ~shape:Point_replication.Replicate_point
      ~transform_attributes:"flow N v" ~points_per_point:1. source |> get in
  let flow = point_float3 "flow" transformed
  and normal = point_float3 "N" transformed
  and velocity = point_float3 "v" transformed in
  check (flow.x = [|2.|] && flow.y = [|2.|] && flow.z = [|1.5|])
    "Point Replicate vector attribute transform";
  let length = sqrt (0.25 +. 1. +. 4.) in
  check_close normal.x.(0) (0.5 /. length)
    "Point Replicate inverse-transpose normal x";
  check_close normal.y.(0) (1. /. length)
    "Point Replicate inverse-transpose normal y";
  check_close normal.z.(0) (2. /. length)
    "Point Replicate inverse-transpose normal z";
  check (velocity.x = [|8.|] && velocity.y = [|5.|]
      && velocity.z = [|3.|])
    "Point Replicate post-synthesis velocity transform"

let test_singular_normal_selection () =
  let source = points [|0.,0.,0.; 1.,0.,0.|]
      |> Geometry.with_attribute (attribute Attribute.Point "scale"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|0.;1.|] ~y:[|1.;1.|] ~z:[|1.;1.|]))) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "orient"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:[|0.;0.|] ~y:[|0.;0.|] ~z:[|0.;0.|] ~w:[|1.;1.|]
             |> Result.get_ok)))
         |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "N"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.;1.|] ~y:[|0.;0.|] ~z:[|0.;0.|]))) |> Result.get_ok in
  let nonsingular = Group.init ~grain:1 ~owner:Group.Point ~name:"selected"
      2 (fun point -> point = 1) in
  let output = Point_replication.run ~points:nonsingular
      ~shape:Point_replication.Replicate_point ~transform_attributes:"N"
      ~points_per_point:1. source |> get in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output <> None)
    "unselected singular source removed transformed normals";
  let output = Point_replication.run ~shape:Point_replication.Replicate_point
      ~transform_attributes:"N" ~points_per_point:1. source |> get in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None)
    "selected singular source retained transformed normals"

let test_standard_transform_custom_shape () =
  let source = points [|10.,0.,0.|]
      |> Geometry.with_attribute (attribute Attribute.Point "pscale"
           (Attribute.Float [|2.|])) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "N"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.|] ~y:[|0.|] ~z:[|0.|]))) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "up"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|0.|] ~y:[|1.|] ~z:[|0.|]))) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "trans"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.|] ~y:[|0.|] ~z:[|0.|]))) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "v"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|0.|] ~y:[|0.|] ~z:[|2.|]))) |> Result.get_ok in
  let custom = points [|1.,2.,3.|] in
  let output = Point_replication.run ~seed:(Rays_math.Rand.seed 9)
      ~shape:Point_replication.Replicate_custom ~custom_shape:custom
      ~inherit_velocity:0.5 ~radial_velocity:0.25
      ~keep_source_attributes:true ~points_per_point:1. source |> get in
  let position = Packed.Float3.Private.view (Geometry.positions output) in
  check (position.x = [|17.|] && position.y = [|4.|] && position.z = [|-2.|])
    "Point Replicate standard transform basis";
  check (point_int "shapeptnum" output = [|0|])
    "Point Replicate custom shape ancestry";
  let velocity = point_float3 "v" output in
  check (velocity.x = [|1.75|] && velocity.y = [|1.|]
      && velocity.z = [|0.5|]) "Point Replicate inherited/radial velocity"

let test_keep_input_group_and_hidden_metadata () =
  let source = points [|0.,0.,0.; 2.,0.,0.|]
      |> Geometry.with_attribute (attribute Attribute.Point "tag"
           (Attribute.Text [|"a";"b"|])) |> Result.get_ok in
  let output = Point_replication.run ~keep_input:true ~generated_group:"cloud"
      ~copy_point_attributes:"tag" ~shape:Point_replication.Replicate_line
      ~points_per_point:2. source |> get in
  check (Geometry.point_count output = 6
      && Geometry.find_attribute ~owner:Attribute.Point "sourcepoint" output = None
      && Geometry.find_attribute ~owner:Attribute.Point "sourceindex" output = None)
    "Point Replicate hidden metadata";
  let group = Geometry.find_group ~owner:Group.Point "cloud" output
      |> Option.get in
  check (Group.cardinality group = 4 && not (Group.mem 0 group)
      && Group.mem 2 group) "Point Replicate generated group"

let test_id_stability_across_reordering () =
  let make positions ids = points positions
      |> Geometry.with_attribute (attribute Attribute.Point "id"
           (Attribute.Int ids)) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "density"
           (Attribute.Float [|2.5;2.5|])) |> Result.get_ok in
  let first = make [|0.,0.,0.; 100.,0.,0.|] [|10;20|]
  and reversed = make [|100.,0.,0.; 0.,0.,0.|] [|20;10|] in
  let cook source = Point_replication.run ~seed:(Rays_math.Rand.seed 123)
      ~keep_source_attributes:true ~shape:Point_replication.Replicate_box
      ~points_per_point:1. ~scale_attribute:"density" source |> get in
  let signature source output =
    let ids = point_int "id" output and local = point_int "sourceindex" output
    and sourcepoint = point_int "sourcepoint" output in
    let source_positions = Packed.Float3.Private.view (Geometry.positions source)
    and output_positions = Packed.Float3.Private.view (Geometry.positions output) in
    Array.init (Geometry.point_count output) (fun point ->
      let source = sourcepoint.(point) in
      ids.(point), local.(point),
      output_positions.x.(point) -. source_positions.x.(source),
      output_positions.y.(point) -. source_positions.y.(source),
      output_positions.z.(point) -. source_positions.z.(source))
    |> Array.to_list |> List.sort compare in
  check (signature first (cook first) = signature reversed (cook reversed))
    "Point Replicate id-stable counts/clouds across point reorder"

let test_rest_stable_noise () =
  let make x = points [|x,0.,0.|]
      |> Geometry.with_attribute (attribute Attribute.Point "id"
           (Attribute.Int [|77|])) |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "rest"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|5.|] ~y:[|2.|] ~z:[|-1.|]))) |> Result.get_ok in
  let first = make 0. and moved = make 100. in
  let cook source = Point_replication.run ~seed:(Rays_math.Rand.seed 65) ~noise_seed:66
      ~noise_amplitude:(Rays_math.Vec3.create 0.25 0.2 0.15)
      ~noise_frequency:(Rays_math.Vec3.create 1.3 0.8 1.7) ~noise_turbulence:4
      ~shape:Point_replication.Replicate_sphere ~points_per_point:8. source |> get in
  let offsets source output =
    let count = Geometry.point_count output in
    let source = Packed.Float3.Private.view (Geometry.positions source)
    and output = Packed.Float3.Private.view (Geometry.positions output) in
    Array.init count (fun point ->
      output.x.(point) -. source.x.(0), output.y.(point) -. source.y.(0),
      output.z.(point) -. source.z.(0)) in
  let first = offsets first (cook first) and moved = offsets moved (cook moved) in
  check (Array.length first = Array.length moved
      && Array.for_all2 (fun (ax,ay,az) (bx,by,bz) ->
        abs_float (ax-.bx) <= 1e-13 && abs_float (ay-.by) <= 1e-13
        && abs_float (az-.bz) <= 1e-13) first moved)
    "Point Replicate rest-space noise moved with source"

let test_malformed_and_cancel () =
  let source = points [|0.,0.,0.|] in
  let expect label result = match result with
    | Error _ -> () | Ok _ -> fail ("Point Replicate accepted " ^ label) in
  expect "negative size" (Point_replication.run ~size:(Rays_math.Vec3.create (-1.) 1. 1.)
    ~points_per_point:1. source);
  expect "missing custom shape" (Point_replication.run ~shape:Point_replication.Replicate_custom
    ~points_per_point:1. source);
  expect "custom shape for builtin" (Point_replication.run ~custom_shape:source
    ~points_per_point:1. source);
  expect "invalid noise roughness" (Point_replication.run
    ~noise_amplitude:(Rays_math.Vec3.create 1. 1. 1.) ~noise_roughness:1.1
    ~points_per_point:1. source);
  expect "malformed transform attribute pattern" (Point_replication.run
    ~transform_attributes:"[" ~points_per_point:1. source);
  let nonfinite = source |> Geometry.with_attribute
      (attribute Attribute.Point "flow"
        (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[|nan|] ~y:[|0.|] ~z:[|0.|]))) |> Result.get_ok in
  expect "non-finite transformed vector" (Point_replication.run
    ~transform_attributes:"flow" ~points_per_point:1. nonfinite);
  let wrong_id = source |> Geometry.with_attribute
      (attribute Attribute.Point "id" (Attribute.Text [|"bad"|]))
      |> Result.get_ok in
  expect "wrong id storage" (Point_replication.run ~points_per_point:1. wrong_id);
  let cancel = Cancel.create () in Cancel.cancel cancel;
  (match Point_replication.run ~cancel ~points_per_point:10. source with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail ("unexpected cancellation: " ^ Error.to_string error)
   | Ok _ -> fail "Point Replicate ignored cancellation")

let geometry_hash geometry =
  let hash = ref 17 in
  let add value = hash := Rays_math.Hash.mix !hash value in
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.x;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.y;
  Array.iter (fun value -> add (Hashtbl.hash value)) positions.z;
  List.iter (fun attribute ->
    add (Hashtbl.hash (Attribute.name attribute, Attribute.kind_name attribute));
    match Attribute.Private.storage attribute with
    | Attribute.Float values -> Array.iter (fun value -> add (Hashtbl.hash value)) values
    | Attribute.Int values -> Array.iter add values
    | Attribute.Float3 values ->
        let values = Packed.Float3.Private.view values in
        Array.iter (fun value -> add (Hashtbl.hash value)) values.x;
        Array.iter (fun value -> add (Hashtbl.hash value)) values.y;
        Array.iter (fun value -> add (Hashtbl.hash value)) values.z
    | _ -> ()) (Geometry.attributes geometry);
  !hash

let test_parallel_exact () =
  let count = 100_000 in
  let source = points (Array.init count (fun point ->
      float_of_int (point mod 1_000) *. 0.01,
      float_of_int (point / 1_000) *. 0.01, 0.))
      |> Geometry.with_attribute (attribute Attribute.Point "id"
           (Attribute.Int (Array.init count (fun point -> point * 17))))
      |> Result.get_ok
      |> Geometry.with_attribute (attribute Attribute.Point "flow"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:(Array.make count 1.) ~y:(Array.make count 2.)
             ~z:(Array.make count 3.))))
      |> Result.get_ok in
  let cook domains = Parallel.run ~domains (fun () ->
      Point_replication.run ~grain:4096 ~seed:(Rays_math.Rand.seed 88)
        ~quasi_stratified:true ~shape:Point_replication.Replicate_sphere
        ~transform_attributes:"flow"
        ~noise_seed:89 ~noise_amplitude:(Rays_math.Vec3.create 0.1 0.2 0.15)
        ~points_per_point:6. source |> get) in
  let one = cook 1 and four = cook 4 in
  check (Geometry.point_count one = 600_000)
    "Point Replicate parallel fixture cardinality";
  check (geometry_hash one = geometry_hash four)
    "Point Replicate one/four-domain output differs"

let run () =
  test_shapes_and_quantity ();
  test_point_shape_and_transformed_attributes ();
  test_singular_normal_selection ();
  test_standard_transform_custom_shape ();
  test_keep_input_group_and_hidden_metadata ();
  test_id_stability_across_reordering ();
  test_rest_stable_noise ();
  test_malformed_and_cancel ();
  test_parallel_exact ();
  print_endline "point replicate tests passed"
