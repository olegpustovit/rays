open Bench_util
open Rays
open Rdk

let elements = integer_env "RAYS_LAPLACIAN_POINTS" 250_000
let repeats = integer_env "RAYS_LAPLACIAN_REPEATS" 3
let domains = integer_env "RAYS_BENCH_DOMAINS"
    (Parallel.recommended_domains ())
let grain = integer_env "RAYS_LAPLACIAN_GRAIN" 16_384

let input () =
  let side = max 3 (int_of_float (sqrt (float_of_int elements))) in
  Parametric_generators.torus ~connectivity:Parametric_generators.Torus_quads ~rows:side ~columns:side
    ~major_radius:(float_of_int side *. 0.2) ~minor_radius:7. () |> get

let hash_float hash value =
  Int64.mul (Int64.logxor hash (Int64.bits_of_float value))
    0x100000001b3L

let output_hash geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "laplacian" geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.Private.view values in
           let hash = ref 0xcbf29ce484222325L in
           for point = 0 to Array.length values.x - 1 do
             hash := hash_float !hash values.x.(point);
             hash := hash_float !hash values.y.(point);
             hash := hash_float !hash values.z.(point)
           done;
           !hash
       | _ -> failwith "laplacian has wrong storage")
  | None -> failwith "laplacian is missing"

let measure name weighting normalize input =
  let times = Array.make repeats 0. and allocated = Array.make repeats 0.
  and promoted = Array.make repeats 0. and major = Array.make repeats 0.
  and hash = ref 0L in
  Parallel.run ~domains (fun () ->
    for repeat = 0 to repeats - 1 do
      Gc.full_major ();
      let before = Gc.quick_stat () and bytes_before = Gc.allocated_bytes ()
      and started = Unix.gettimeofday () in
      let output = Laplacian.run ~grain ~weighting ~normalize
          ~source:"P" input |> get in
      times.(repeat) <- Unix.gettimeofday () -. started;
      allocated.(repeat) <- Gc.allocated_bytes () -. bytes_before;
      let after = Gc.quick_stat () in
      promoted.(repeat) <- (after.promoted_words -. before.promoted_words) *. 8.;
      major.(repeat) <- (after.major_words -. before.major_words) *. 8.;
      let current = output_hash output in
      if repeat > 0 && current <> !hash then
        failwith (name ^ ": nondeterministic output");
      hash := current
    done);
  Printf.printf "%s,%d,%d,%d,%d,%d,%.6f,%.0f,%.0f,%.0f,%Ld\n%!"
    name (Geometry.point_count input) (Geometry.primitive_count input)
    domains grain repeats (median times) (median allocated)
    (median promoted) (median major) !hash

let run () =
  let input = input () in
  Printf.printf "case,points,primitives,domains,grain,repeats,seconds,current_domain_allocated_bytes,promoted_bytes,major_bytes,hash\n";
  measure "cotan_pointwise" Laplacian.Laplacian_cotan true input;
  measure "positive_cotan_pointwise" Laplacian.Laplacian_positive_cotan true input;
  measure "uniform_average" Laplacian.Laplacian_uniform true input
