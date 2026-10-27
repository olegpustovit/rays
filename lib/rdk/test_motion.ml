open Rays
open Rdk

let fail message = prerr_endline ("test_motion: " ^ message); exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let points values = Line_geometry.points values

let add_int name values geometry =
  let attribute = Attribute.create_owned ~name ~owner:Attribute.Point
      (Attribute.Int values) |> get in
  Geometry.with_attribute attribute geometry |> get

let add_float3 name x y z geometry =
  let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  let attribute = Attribute.create_owned ~name ~owner:Attribute.Point
      (Attribute.Float3 values) |> get in
  Geometry.with_attribute attribute geometry |> get

let float3 name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.Private.view values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

let positions geometry = Packed.Float3.Private.view (Geometry.positions geometry)

let check_array expected actual message =
  if expected <> actual then fail message

let test_rest () =
  let source = points [|(0.,1.,2.); (3.,4.,5.)|]
      |> add_float3 "N" [|0.;1.|] [|1.;0.|] [|0.;0.|] in
  let stored = Motion.rest_position ~normals:Motion.Rest_normals_if_present
      Motion.Store_rest source |> get_rdk in
  let rest = float3 "rest" stored and rest_n = float3 "restN" stored in
  check_array [|0.;3.|] rest.x "stored rest P";
  check_array [|1.;0.|] rest_n.y "stored rest N";
  let source_positions = positions source in
  check (rest.x == source_positions.x && rest.y == source_positions.y
         && rest.z == source_positions.z)
    "rest storage did not share the immutable position plane";
  let deformed_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|10.;20.|] ~y:[|11.;21.|] ~z:[|12.;22.|] in
  let deformed = Geometry.with_positions deformed_positions stored |> get in
  let extracted = Motion.rest_position ~normals:Motion.Rest_normals_if_present
      Motion.Extract_rest deformed |> get_rdk in
  check_array [|0.;3.|] (positions extracted).x "extract rest P";
  check_array [|1.;0.|] (float3 "N" extracted).y "extract rest N";
  let swapped = Motion.rest_position Motion.Swap_rest deformed |> get_rdk in
  check_array [|0.;3.|] (positions swapped).x "swap output P";
  check_array [|10.;20.|] (float3 "rest" swapped).x "swap stored deformation";
  let reference = points [|(-1.,-2.,-3.); (-4.,-5.,-6.)|] in
  let referenced = Motion.rest_position ~reference Motion.Store_rest source
      |> get_rdk in
  check_array [|-1.;-4.|] (float3 "rest" referenced).x "reference rest P";
  let missing = points [|(9.,8.,7.)|] in
  let fallback = Motion.rest_position Motion.Extract_rest missing |> get_rdk in
  check_array [|9.|] (float3 "rest" fallback).x "missing rest did not store";
  (match Motion.rest_position ~reference:missing Motion.Store_rest source with
   | Error error -> check (Error.code error = "cardinality_mismatch")
       "rest mismatch diagnostic"
   | Ok _ -> fail "rest accepted mismatched reference")

let test_deformation () =
  let previous = points [|(0.,0.,0.); (1.,2.,3.)|]
  and current = points [|(1.,2.,3.); (3.,6.,9.)|] in
  let backward = Motion.point_velocity ~previous ~dt:0.5 current |> get_rdk in
  let v = float3 "v" backward in
  check_array [|2.;4.|] v.x "backward velocity x";
  check_array [|4.;8.|] v.y "backward velocity y";
  check_array [|6.;12.|] v.z "backward velocity z";
  let previous = points [|(0.,0.,0.)|]
  and current = points [|(1.,1.,1.)|]
  and next = points [|(4.,6.,8.)|] in
  let central = Motion.point_velocity ~previous ~next
      ~approximation:Motion.Central_difference ~dt:1.
      ~compute_acceleration:true current |> get_rdk in
  let v = float3 "v" central and a = float3 "accel" central in
  check_array [|2.|] v.x "central velocity";
  check_array [|3.|] v.y "central velocity y";
  check_array [|2.|] a.x "central acceleration";
  check_array [|4.|] a.y "central acceleration y";
  check_array [|6.|] a.z "central acceleration z"

let test_group_and_initializers () =
  let source = points [|(0.,0.,0.); (0.,0.,0.)|]
      |> add_float3 "v" [|9.;9.|] [|8.;8.|] [|7.;7.|]
      |> add_float3 "wind" [|1.;2.|] [|2.;3.|] [|3.;4.|] in
  let group = Group.init ~owner:Group.Point ~name:"first" 2
      (fun point -> point = 0) in
  let output = Motion.point_velocity ~points:group
      ~initialization:(Motion.From_attribute { name = "wind"; scale = 2. })
      ~add_velocity:(Rays_math.Vec3.create 1. 1. 1.) source |> get_rdk in
  let v = float3 "v" output in
  check_array [|3.;9.|] v.x "restricted initialized velocity x";
  check_array [|5.;8.|] v.y "restricted initialized velocity y";
  check_array [|7.;7.|] v.z "restricted initialized velocity z";
  let kept = Motion.point_velocity ~initialization:Motion.Keep_incoming source
      |> get_rdk in
  check (kept == source) "Keep Incoming identity allocated a new snapshot";
  let copied = Motion.point_velocity
      ~initialization:(Motion.From_attribute { name = "wind"; scale = 1. })
      ~velocity_attribute:"copied" source |> get_rdk in
  let wind = float3 "wind" source and copied = float3 "copied" copied in
  check (wind.x == copied.x && wind.y == copied.y && wind.z == copied.z)
    "unscaled attribute initialization did not share its packed plane";
  let zeroed = Motion.point_velocity ~dt:Float.nan
      ~initialization:(Motion.Set_value Rays_math.Vec3.zero) source |> get_rdk in
  let v = float3 "v" zeroed in
  check_array [|0.;0.|] v.x "Set Value ignores dt: x";
  check_array [|0.;0.|] v.y "Set Value ignores dt: y";
  check_array [|0.;0.|] v.z "Set Value ignores dt: z"

let test_matching () =
  let previous = points [|(10.,0.,0.); (20.,0.,0.)|]
      |> add_int "id" [|10;20|] in
  let current = points [|(23.,0.,0.); (12.,0.,0.); (30.,0.,0.)|]
      |> add_int "id" [|20;10;30|] in
  let output = Motion.point_velocity ~previous ~dt:1. ~match_attribute:"id"
      ~unmatched:Motion.Velocity_unmatched_zero current |> get_rdk in
  check_array [|3.;2.;0.|] (float3 "v" output).x
    "ID matching/reordered/unmatched velocity";
  (match Motion.point_velocity ~previous ~dt:1. ~match_attribute:"id" current with
   | Error error -> check (Error.code error = "invalid_deformation")
       "unmatched diagnostic code"
   | Ok _ -> fail "strict matching accepted an unmatched point");
  let duplicate = points [|(0.,0.,0.); (1.,0.,0.)|]
      |> add_int "id" [|10;10|] in
  (match Motion.point_velocity ~previous:duplicate ~dt:1.
      ~match_attribute:"id" current with
   | Error _ -> () | Ok _ -> fail "duplicate match keys were accepted");
  (match Motion.point_velocity ~previous:(points [|(0.,0.,0.)|]) current with
   | Error _ -> () | Ok _ -> fail "point-number cardinality mismatch accepted");
  let previous_text = points [|(1.,0.,0.); (2.,0.,0.)|] in
  let text_attribute values geometry =
    let attribute = Attribute.create_owned ~name:"name" ~owner:Attribute.Point
        (Attribute.Text values) |> get in
    Geometry.with_attribute attribute geometry |> get in
  let previous_text = text_attribute [|"left";"right"|] previous_text
  and current_text = text_attribute [|"right";"left"|]
      (points [|(5.,0.,0.); (3.,0.,0.)|]) in
  let text_output = Motion.point_velocity ~previous:previous_text ~dt:1.
      ~match_attribute:"name" current_text |> get_rdk in
  check_array [|3.;2.|] (float3 "v" text_output).x "text ID matching"

let test_cancellation () =
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  let geometry = points [|(0.,0.,0.)|] in
  (match Motion.point_velocity ~cancel ~previous:geometry geometry with
   | Error error -> check (Error.code error = "cancelled")
       "velocity cancellation diagnostic"
   | Ok _ -> fail "velocity ignored cancellation");
  (match Motion.rest_position ~cancel Motion.Store_rest geometry with
   | Error error -> check (Error.code error = "cancelled")
       "rest cancellation diagnostic"
   | Ok _ -> fail "rest ignored cancellation")

let test_domain_exactness () =
  let count = 200_003 in
  let previous = Kernel.generate_point_ranges count
      (fun ~first ~last ~x ~y ~z ->
        for point = first to last - 1 do
          let value = float point in
          x.(point) <- value; y.(point) <- value *. 0.5; z.(point) <- -.value
        done)
  and current = Kernel.generate_point_ranges count
      (fun ~first ~last ~x ~y ~z ->
        for point = first to last - 1 do
          let value = float point in
          x.(point) <- value +. 0.25;
          y.(point) <- (value *. 0.5) -. 0.5;
          z.(point) <- 1. -. value
        done) in
  let run domains = Parallel.run ~domains (fun () ->
      Motion.point_velocity ~grain:257 ~previous ~dt:0.25 current |> get_rdk) in
  let one = run 1 and many = run 4 in
  let one = float3 "v" one and many = float3 "v" many in
  check (one.x = many.x && one.y = many.y && one.z = many.z)
    "one-domain/multi-domain velocity drift";
  check_array [|1.|] [|one.x.(count - 1)|] "large velocity x cardinality";
  check_array [|-2.|] [|one.y.(count - 1)|] "large velocity y cardinality";
  check_array [|4.|] [|one.z.(count - 1)|] "large velocity z cardinality"

let run () =
  test_rest ();
  test_deformation ();
  test_group_and_initializers ();
  test_matching ();
  test_cancellation ();
  test_domain_exactness ();
  print_endline "test_motion: ok"
