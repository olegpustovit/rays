open Rdk

(* The removed accessors, over the borrowed view. *)
module Planar_constraints = struct
  include Planar_constraints
  let source_point_count value = point_count value - split_point_count value
  let approximate_x value = (View.view value).x
  let approximate_y value = (View.view value).y
  let insert_points value = (View.view value).insert_points
  let constraint_points value = (View.view value).constraint_points
  let constraint_winding value = (View.view value).constraint_winding
end

let fail format = Printf.ksprintf failwith format
let check condition format =
  Printf.ksprintf (fun message -> if not condition then failwith message) format
let get_string = function Ok value -> value | Error message -> fail "%s" message

let square_crossing () =
  let x = [|0.;2.;2.;0.|] and y = [|0.;0.;2.;2.|] in
  let value = Planar_constraints.build ~split_crossings:true ~x ~y
      ~segment_points:[|0;2; 1;3|] () |> get_string in
  check (Planar_constraints.source_point_count value = 4) "source cardinality";
  check (Planar_constraints.point_count value = 5) "crossing point cardinality";
  check (Planar_constraints.split_point_count value = 1) "split cardinality";
  check (Planar_constraints.insert_points value = [|4|]) "inserted point ids";
  let output_x = Planar_constraints.approximate_x value
  and output_y = Planar_constraints.approximate_y value in
  check (output_x.(4) = 1. && output_y.(4) = 1.)
    "crossing approximation is (%g,%g)" output_x.(4) output_y.(4);
  check (Planar_constraints.constraint_points value
      = [|0;4; 1;4; 2;4; 3;4|]) "atomic crossing constraints";
  let weighted = Planar_constraints.build ~split_crossings:true ~x ~y
      ~segment_points:[|0;2;1;3|] ~segment_winding:[|2;3|] ()
      |> get_string in
  check (Planar_constraints.constraint_winding weighted = [|2;3;-2;-3|])
    "crossing constraint winding propagation";
  check (Planar_constraints.split_source_first value 0 = 0
      && Planar_constraints.split_source_second value 0 = 2)
    "crossing ancestry";
  check (Planar_constraints.split_source_parameter value 0 = 0.5)
    "crossing interpolation parameter";
  value

let test_crossing_policy () =
  let x = [|0.;2.;2.;0.|] and y = [|0.;0.;2.;2.|] in
  match Planar_constraints.build ~split_crossings:false ~x ~y
      ~segment_points:[|0;2; 1;3|] () with
  | Error _ -> ()
  | Ok _ -> fail "crossing constraints succeeded when splitting was disabled"

let test_t_junction_and_overlap () =
  let x = [|0.;2.;1.;1.|] and y = [|0.;0.;0.;1.|] in
  let junction = Planar_constraints.build ~split_crossings:true ~x ~y
      ~segment_points:[|0;1; 2;3|] () |> get_string in
  check (Planar_constraints.split_point_count junction = 0)
    "T junction invented a point";
  check (Planar_constraints.constraint_points junction
      = [|0;2; 1;2; 2;3|]) "T junction was not atomized";
  let x = [|0.;3.;1.;2.|] and y = [|0.;0.;0.;0.|] in
  let overlap = Planar_constraints.build ~split_crossings:true ~x ~y
      ~segment_points:[|0;1; 2;3|] () |> get_string in
  check (Planar_constraints.constraint_points overlap
      = [|0;2; 1;3; 2;3|]) "collinear overlap was not atomized/deduplicated"

let test_embedded_source_points () =
  let x = [|0.;1.;2.;3.;4.;2.|]
  and y = [|0.;0.;0.;0.;0.;1.|] in
  let run domains = Parallel.run ~domains (fun () ->
      Planar_constraints.build ~grain:1 ~split_crossings:true ~x ~y
        ~segment_points:[|0;4|] ~embedded_points:[|0;1;2;3;4;5|] ()
      |> get_string) in
  let one = run 1 and four = run 4 in
  check (Planar_constraints.split_point_count one = 0)
    "embedded authored points invented constructed points";
  check (Planar_constraints.constraint_points one
      = [|0;1; 1;2; 2;3; 3;4|])
    "embedded authored points did not atomize the constraint";
  check (Planar_constraints.constraint_points four
      = Planar_constraints.constraint_points one)
    "embedded point atomization differs across domains";
  let maximum = Float.max_float in
  let extreme = Planar_constraints.build ~split_crossings:true
      ~x:[|-.maximum;0.;maximum|] ~y:[|0.;0.;0.|]
      ~segment_points:[|0;2|] ~embedded_points:[|0;1;2|] ()
      |> get_string in
  check (Planar_constraints.constraint_points extreme = [|0;1;1;2|])
    "maximum-range embedded point was not classified exactly";
  let tiny = Float.next_after 0. Float.infinity in
  let subnormal = Planar_constraints.build ~split_crossings:true
      ~x:[|0.;tiny;tiny +. tiny|] ~y:[|0.;0.;0.|]
      ~segment_points:[|0;2|] ~embedded_points:[|0;1;2|] ()
      |> get_string in
  check (Planar_constraints.constraint_points subnormal = [|0;1;1;2|])
    "subnormal embedded point was not classified exactly";
  let crossing = Planar_constraints.build ~grain:1 ~split_crossings:true
      ~x:[|0.;2.;2.;0.;1.|] ~y:[|0.;0.;2.;2.;1.|]
      ~segment_points:[|0;2;1;3|] ~embedded_points:[|0;1;2;3;4|] ()
      |> get_string in
  check (Planar_constraints.split_point_count crossing = 0
      && Planar_constraints.constraint_points crossing
         = [|0;4;1;4;2;4;3;4|])
    "authored point at a proper crossing was not preferred exactly";
  let duplicate = Planar_constraints.build ~split_crossings:true
      ~x:[|0.;1.;1.;2.|] ~y:[|0.;0.;0.;0.|]
      ~segment_points:[|0;3|] ~embedded_points:[|0;1;2;3|] ()
      |> get_string in
  check (Planar_constraints.constraint_points duplicate = [|0;1;1;3|])
    "exact-coordinate duplicate embedded points were not canonicalized"

let test_many_way_dedup_and_domains () =
  let x = [|0.;2.; 0.;2.; 1.;1.|]
  and y = [|0.;2.; 2.;0.; 0.;2.|] in
  let run domains = Parallel.run ~domains (fun () ->
      Planar_constraints.build ~split_crossings:true ~x ~y
        ~segment_points:[|0;1; 2;3; 4;5|] () |> get_string) in
  let one = run 1 and four = run 4 in
  check (Planar_constraints.split_point_count one = 1)
    "many-way crossing did not exact-deduplicate";
  check (Planar_constraints.approximate_x one
      = Planar_constraints.approximate_x four
      && Planar_constraints.approximate_y one
         = Planar_constraints.approximate_y four
      && Planar_constraints.constraint_points one
         = Planar_constraints.constraint_points four)
    "domain count changed planar arrangement"

let test_extreme_scales () =
  let maximum = Float.max_float in
  let large = Planar_constraints.build ~split_crossings:true
      ~x:[|-.maximum;maximum;0.;0.|] ~y:[|0.;0.;-.maximum;maximum|]
      ~segment_points:[|0;1;2;3|] () |> get_string in
  check (Planar_constraints.split_point_count large = 1
      && (Planar_constraints.approximate_x large).(4) = 0.
      && (Planar_constraints.approximate_y large).(4) = 0.)
    "large-coordinate exact crossing";
  check (Planar_constraints.split_source_parameter large 0 = 0.5)
    "large-coordinate split parameter";
  let tiny = Float.next_after 0. Float.infinity in
  let twice = tiny +. tiny in
  let small = Planar_constraints.build ~split_crossings:true
      ~x:[|0.;twice;twice;0.|] ~y:[|0.;0.;twice;twice|]
      ~segment_points:[|0;2;1;3|] () |> get_string in
  check (Planar_constraints.split_point_count small = 1
      && (Planar_constraints.approximate_x small).(4) = tiny
      && (Planar_constraints.approximate_y small).(4) = tiny)
    "subnormal-coordinate exact crossing"

let test_scale_and_ownership () =
  let segments = 20_000 in
  let x = Array.init (segments * 2) (fun point ->
      if point land 1 = 0 then 0. else 0.25)
  and y = Array.init (segments * 2) (fun point -> float_of_int (point / 2))
  and edges = Array.init (segments * 2) Fun.id in
  let value = Planar_constraints.build ~split_crossings:true ~x ~y
      ~segment_points:edges () |> get_string in
  check (Planar_constraints.split_point_count value = 0
      && Array.length (Planar_constraints.constraint_points value) = segments * 2)
    "disjoint scale cardinality";
  let point_count = 20_000 in
  let embedded = Planar_constraints.build ~grain:257 ~split_crossings:true
      ~x:(Array.init point_count float_of_int) ~y:(Array.make point_count 0.)
      ~segment_points:[|0;point_count - 1|]
      ~embedded_points:(Array.init point_count Fun.id) () |> get_string in
  check (Array.length (Planar_constraints.constraint_points embedded)
      = (point_count - 1) * 2)
    "embedded-point scale cardinality"

let test_validation_and_cancellation () =
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.|] ~y:[||]
      ~segment_points:[||] () with
   | Error _ -> () | Ok _ -> fail "mismatched coordinate planes succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0;0|] () with
   | Error _ -> () | Ok _ -> fail "degenerate segment succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0|] () with
   | Error _ -> () | Ok _ -> fail "odd endpoint array succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;infinity|]
      ~y:[|0.;0.|] ~segment_points:[|0;1|] () with
   | Error _ -> () | Ok _ -> fail "non-finite coordinate succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0;2|] () with
   | Error _ -> () | Ok _ -> fail "out-of-range endpoint succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0;1|] ~segment_winding:[||] () with
   | Error _ -> () | Ok _ -> fail "mismatched winding cardinality succeeded");
  (match Planar_constraints.build ~grain:0 ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0;1|] () with
   | Error _ -> () | Ok _ -> fail "non-positive grain succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0;1|] ~embedded_points:[|2|] () with
   | Error _ -> () | Ok _ -> fail "out-of-range embedded point succeeded");
  (match Planar_constraints.build ~split_crossings:true ~x:[|0.;1.|]
      ~y:[|0.;0.|] ~segment_points:[|0;1|] ~embedded_points:[|0;0|] () with
   | Error _ -> () | Ok _ -> fail "duplicate embedded point succeeded");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Planar_constraints.build ~cancel ~split_crossings:true
      ~x:[|0.;1.|] ~y:[|0.;0.|] ~segment_points:[|0;1|] () with
  | Error _ -> () | Ok _ -> fail "cancelled arrangement succeeded"

let run () =
  test_crossing_policy ();
  test_t_junction_and_overlap ();
  test_embedded_source_points ();
  test_many_way_dedup_and_domains ();
  test_extreme_scales ();
  test_scale_and_ownership ();
  test_validation_and_cancellation ()
