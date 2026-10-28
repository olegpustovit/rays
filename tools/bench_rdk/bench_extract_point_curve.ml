open Bench_util
open Rays
open Rdk

let curves = integer_env "RAYS_EXTRACT_CURVES" 1_000
let points_per_curve = max 2
    (integer_env "RAYS_EXTRACT_POINTS_PER_CURVE" 1_001)
let repeats = integer_env "RAYS_EXTRACT_REPEATS" 5
let domains = integer_env "RAYS_BENCH_DOMAINS"
    (Parallel.recommended_domains ())
let grain = integer_env "RAYS_EXTRACT_GRAIN" 16_384

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let fixture ~dense =
  let point_count = curves * points_per_curve in
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:(Array.init point_count (fun point ->
        Float.of_int (point mod points_per_curve)))
      ~y:(Array.init point_count (fun point ->
        Float.of_int (point / points_per_curve)))
      ~z:(Array.make point_count 0.) in
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (curves + 1)
        (fun primitive -> primitive * points_per_curve))
      ~primitive_kinds:(Array.make curves Topology.Open_polyline)
      |> Result.get_ok in
  let midpoint = (points_per_curve - 1) / 2 in
  let distance = Array.init point_count (fun point ->
    let local = point mod points_per_curve in
    if dense then if local land 1 = 0 then -1. else 1.
    else Float.of_int (local - midpoint)) in
  Geometry.create ~positions ~topology ~attributes:[
    attribute Attribute.Point "distance" (Attribute.Float distance);
    attribute Attribute.Point "weight"
      (Attribute.Float (Array.init point_count Float.of_int));
    attribute Attribute.Point "id"
      (Attribute.Int (Array.init point_count Fun.id));
    attribute Attribute.Primitive "cut"
      (Attribute.Float (Array.init curves (fun primitive ->
        if dense then if primitive land 1 = 0 then 0. else 0.25 else 0.)));
    attribute Attribute.Primitive "material"
      (Attribute.Int (Array.init curves (fun primitive -> primitive mod 31)))
  ] () |> Result.get_ok

let mix hash value = ((hash * 65_599) lxor value) land max_int

let hash geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let value = ref 17 in
  let floats values = Array.iter (fun item ->
    value := mix !value (Int64.to_int (Int64.bits_of_float item))) values in
  floats positions.x; floats positions.y; floats positions.z;
  List.iter (fun attribute -> match Attribute.Private.storage attribute with
    | Attribute.Float values -> floats values
    | Attribute.Int values -> Array.iter (fun item -> value := mix !value item) values
    | Attribute.Float2 values ->
        let values = Packed.Float2.Private.view values in
        floats values.x; floats values.y
    | Attribute.Float3 values ->
        let values = Packed.Float3.Private.view values in
        floats values.x; floats values.y; floats values.z
    | Attribute.Float4 values ->
        let values = Packed.Float4.Private.view values in
        floats values.x; floats values.y; floats values.z; floats values.w
    | Attribute.Text values -> Array.iter (fun item ->
        value := mix !value (Hashtbl.hash item)) values
    | Attribute.Int_array values ->
        let values = Packed.Int_array.Private.view values in
        Array.iter (fun item -> value := mix !value item) values.offsets;
        Array.iter (fun item -> value := mix !value item) values.values
    | Attribute.Float_array values ->
        let values = Packed.Float_array.Private.view values in
        Array.iter (fun item -> value := mix !value item) values.offsets;
        floats values.values) (Geometry.attributes geometry);
  !value

let measure name operation input =
  let times = Array.make repeats 0. and allocated = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and output_points = ref 0 and output_hash = ref 0 in
  Parallel.run ~domains (fun () ->
    for repeat = 0 to repeats - 1 do
      Gc.full_major ();
      let before = Gc.quick_stat () and bytes_before = Gc.allocated_bytes ()
      and started = Unix.gettimeofday () in
      let output = operation input |> get in
      times.(repeat) <- Unix.gettimeofday () -. started;
      allocated.(repeat) <- Gc.allocated_bytes () -. bytes_before;
      let after = Gc.quick_stat () in
      promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
      major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
      output_points := Geometry.point_count output;
      let current_hash = hash output in
      if repeat > 0 && current_hash <> !output_hash then
        failwith (name ^ ": nondeterministic output");
      output_hash := current_hash
    done);
  Printf.printf "%s,%d,%d,%d,%d,%d,%.6f,%.0f,%.0f,%.0f,%d,%d\n%!"
    name (Geometry.point_count input) curves domains grain repeats
    (median times) (median allocated) (median promoted) (median major)
    !output_points !output_hash

let run () =
  Printf.printf "case,input_points,curves,domains,grain,repeats,seconds,current_domain_allocated_bytes,promoted_bytes,major_bytes,output_points,hash\n";
  let sparse = fixture ~dense:false and dense = fixture ~dense:true in
  measure "constant_sparse"
    (Curve_topology.extract_point_from_curve ~grain ~distance_attribute:"distance") sparse;
  measure "constant_dense"
    (Curve_topology.extract_point_from_curve ~grain ~distance_attribute:"distance") dense;
  measure "varying_dense_payload"
    (Curve_topology.extract_point_from_curve ~grain
      ~cut:(Curve_topology.Extract_cut_primitive_attribute "cut")
      ~distance_attribute:"distance" ~point_attributes:"weight id"
      ~copy_primitive_attributes:true ~primitive_attributes:"material"
      ~curve_u_attribute:"u" ~number_cuts_attribute:"cuts"
      ~curve_number_attribute:"curve") dense
