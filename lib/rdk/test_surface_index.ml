open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let high_valence_surface () =
  let ring = 40 in
  let extra_points = 6 in
  let point_count = 1 + ring + extra_points in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for point = 0 to ring - 1 do
    let angle = 2. *. Float.pi *. float_of_int point /. float_of_int ring in
    x.(1 + point) <- cos angle;
    y.(1 + point) <- sin angle
  done;
  let extra = 1 + ring in
  x.(extra) <- -0.31; y.(extra) <- -0.11;
  x.(extra + 1) <- 0.29; y.(extra + 1) <- -0.09;
  x.(extra + 2) <- 0.02; y.(extra + 2) <- 0.34;
  x.(extra + 3) <- -0.27; y.(extra + 3) <- 0.13;
  x.(extra + 4) <- 0.32; y.(extra + 4) <- 0.15;
  x.(extra + 5) <- -0.01; y.(extra + 5) <- -0.30;
  let primitive_count = ring + 2 in
  let vertices = Array.make (primitive_count * 3) 0 in
  for primitive = 0 to ring - 1 do
    let offset = primitive * 3 in
    vertices.(offset) <- 0;
    vertices.(offset + 1) <- 1 + primitive;
    vertices.(offset + 2) <- 1 + ((primitive + 1) mod ring)
  done;
  let offset = ring * 3 in
  vertices.(offset) <- extra;
  vertices.(offset + 1) <- extra + 1;
  vertices.(offset + 2) <- extra + 2;
  vertices.(offset + 3) <- extra + 3;
  vertices.(offset + 4) <- extra + 4;
  vertices.(offset + 5) <- extra + 5;
  let topology = Topology.polygons_owned ~point_count ~vertex_points:vertices
      ~primitive_offsets:(Array.init (primitive_count + 1) (fun value -> value * 3))
      |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let sorted_keys triangle_count first second =
  let keys = Array.init (Array.length first) (fun pair ->
      (first.(pair) * triangle_count) + second.(pair)) in
  Array.sort Int.compare keys;
  keys

let filtered_reference surface first second =
  let retained = Array.make (Array.length first) false and count = ref 0 in
  for pair = 0 to Array.length first - 1 do
    let shared = ref false in
    for left = 0 to 2 do
      let point = Surface_index.View.triangle_point surface first.(pair) left in
      for right = 0 to 2 do
        if point = Surface_index.View.triangle_point surface second.(pair) right then
          shared := true
      done
    done;
    if not !shared then begin retained.(pair) <- true; incr count end
  done;
  let output_first = Array.make !count 0 and output_second = Array.make !count 0
  and output = ref 0 in
  for pair = 0 to Array.length first - 1 do
    if retained.(pair) then begin
      output_first.(!output) <- first.(pair);
      output_second.(!output) <- second.(pair);
      incr output
    end
  done;
  output_first, output_second

let run () =
  let centroids = Array.make 9 0. in
  let lower = Array.init 9 float_of_int
  and upper = Array.init 9 (fun item -> float_of_int item +. 1.) in
  let bounds = Rdk_spatial.Bounds3_index.create ~grain:16
      ~centroid_x:centroids ~centroid_y:centroids ~centroid_z:centroids
      ~item_min_x:lower ~item_min_y:lower ~item_min_z:lower
      ~item_max_x:upper ~item_max_y:upper ~item_max_z:upper () in
  if bounds.order <> Array.init 9 Fun.id
      || bounds.left <> [|1; -1; -1|]
      || bounds.right <> [|2; -1; -1|]
      || bounds.first <> [|0; 0; 4|]
      || bounds.count <> [|0; 4; 5|] then
    fail "median-split bounds changed stable tied-centroid order";
  let geometry = high_valence_surface () in
  let surface = Surface_index.create ~grain:1 geometry |> get in
  let raw_first, raw_second =
    Surface_index.View.overlapping_self_triangle_pairs
      ~grain:1 ~tolerance:0. surface in
  let reference_first, reference_second =
    filtered_reference surface raw_first raw_second in
  let one_first, one_second =
    Surface_index.View.overlapping_self_triangle_pairs_disjoint_topology
      ~grain:1 ~tolerance:0. surface in
  let many_first, many_second =
    Surface_index.View.overlapping_self_triangle_pairs_disjoint_topology
      ~grain:8 ~tolerance:0. surface in
  let triangles = Surface_index.triangle_count surface in
  let reference = sorted_keys triangles reference_first reference_second
  and one = sorted_keys triangles one_first one_second
  and many = sorted_keys triangles many_first many_second in
  if Array.length reference = 0 then
    fail "high-valence candidate fixture produced no disjoint pairs";
  if one <> reference then
    fail "disjoint-topology candidates differ from raw shared-point filtering";
  if many <> one then
    fail "disjoint-topology candidates differ across scheduling grains"
