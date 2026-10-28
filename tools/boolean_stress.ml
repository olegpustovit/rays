open Rays
open Rdk
open Rdk_test_support

type level = Quick | Standard | Full

type recipe = {
  name : string;
  left : Geometry.t;
  right : Geometry.t;
  left_treatment : Boolean_solid.treatment;
  right_treatment : Boolean_solid.treatment;
  seam_points : Boolean.seam_points;
  resolve_left : bool;
  resolve_right : bool;
}

type signature = {
  x : float array;
  y : float array;
  z : float array;
  vertices : int array;
  offsets : int array;
  kinds : bytes;
}

let fail format = Printf.ksprintf failwith format
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

let level_of_string = function
  | "quick" -> Quick
  | "standard" -> Standard
  | "full" -> Full
  | value -> fail "unknown stress level %S (expected quick, standard, or full)" value

let operation_name = function
  | Boolean_solid.Union -> "union"
  | Boolean_solid.Intersection -> "intersection"
  | Boolean_solid.Difference -> "difference"
  | Boolean_solid.Reverse_difference -> "reverse_difference"
  | Boolean_solid.Xor -> "xor"
  | Boolean_solid.Shatter -> "shatter"

let operation_of_string = function
  | "union" -> Boolean_solid.Union
  | "intersection" -> Boolean_solid.Intersection
  | "difference" -> Boolean_solid.Difference
  | "reverse_difference" -> Boolean_solid.Reverse_difference
  | "xor" -> Boolean_solid.Xor
  | "shatter" -> Boolean_solid.Shatter
  | value -> fail "unknown Boolean operation %S" value

let geometry positions vertex_points primitive_offsets =
  let point_count = Array.length positions in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  Array.iteri (fun point (px, py, pz) ->
      x.(point) <- px; y.(point) <- py; z.(point) <- pz) positions;
  let topology = Topology.polygons_owned ~point_count ~vertex_points
      ~primitive_offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let reverse_winding source =
  let positions = Geometry.positions source
  and view = Topology.View.view (Geometry.topology source) in
  let vertices = Array.copy view.vertex_points in
  for primitive = 0 to Array.length view.primitive_offsets - 2 do
    let first = view.primitive_offsets.(primitive)
    and last = view.primitive_offsets.(primitive + 1) - 1 in
    for local = 0 to (last - first - 1) / 2 do
      let a = first + local and b = last - local in
      let value = vertices.(a) in vertices.(a) <- vertices.(b); vertices.(b) <- value
    done
  done;
  let topology = Topology.polygons_owned ~point_count:(Geometry.point_count source)
      ~vertex_points:vertices ~primitive_offsets:(Array.copy view.primitive_offsets)
      |> get_string in
  Geometry.create ~positions ~topology () |> get_string

let outward source =
  let volume = Analysis.signed_volume source in
  if volume < 0. then reverse_winding source else source

let rotate (rx, ry, rz) (x, y, z) =
  let cx = cos rx and sx = sin rx and cy = cos ry and sy = sin ry
  and cz = cos rz and sz = sin rz in
  let y, z = ((cx *. y) -. (sx *. z)), ((sx *. y) +. (cx *. z)) in
  let x, z = ((cy *. x) +. (sy *. z)), ((-.sy *. x) +. (cy *. z)) in
  ((cz *. x) -. (sz *. y)), ((sz *. x) +. (cz *. y)), z

let transform ~center:(cx, cy, cz) ~rotation point =
  let x, y, z = rotate rotation point in x +. cx, y +. cy, z +. cz

let sphere ~latitude ~longitude ~center ~rotation ~radius:(rx, ry, rz) =
  let ring_count = latitude - 1 in
  let point_count = 2 + (ring_count * longitude) in
  let points = Array.make point_count (0., 0., 0.) in
  points.(0) <- transform ~center ~rotation (0., ry, 0.);
  for ring = 0 to ring_count - 1 do
    let theta = Float.pi *. float_of_int (ring + 1) /. float_of_int latitude in
    for column = 0 to longitude - 1 do
      let phi = 2. *. Float.pi *. float_of_int column /. float_of_int longitude in
      points.(1 + (ring * longitude) + column) <- transform ~center ~rotation
          (rx *. sin theta *. cos phi, ry *. cos theta,
           rz *. sin theta *. sin phi)
    done
  done;
  let south = point_count - 1 in
  points.(south) <- transform ~center ~rotation (0., -.ry, 0.);
  let triangle_count = 2 * longitude * (latitude - 1) in
  let vertices = Array.make (triangle_count * 3) 0 and cursor = ref 0 in
  let triangle a b c =
    vertices.(!cursor) <- a; vertices.(!cursor + 1) <- b;
    vertices.(!cursor + 2) <- c; cursor := !cursor + 3 in
  for column = 0 to longitude - 1 do
    let next = (column + 1) mod longitude in
    triangle 0 (1 + column) (1 + next)
  done;
  for ring = 0 to ring_count - 2 do
    let current = 1 + (ring * longitude)
    and next_ring = 1 + ((ring + 1) * longitude) in
    for column = 0 to longitude - 1 do
      let next = (column + 1) mod longitude in
      triangle (current + column) (next_ring + column) (next_ring + next);
      triangle (current + column) (next_ring + next) (current + next)
    done
  done;
  let last_ring = 1 + ((ring_count - 1) * longitude) in
  for column = 0 to longitude - 1 do
    let next = (column + 1) mod longitude in
    triangle south (last_ring + next) (last_ring + column)
  done;
  geometry points vertices (Array.init (triangle_count + 1) (fun i -> i * 3))
  |> outward

let torus ~major_segments ~minor_segments ~center ~rotation ~major ~minor =
  let point_count = major_segments * minor_segments in
  let points = Array.make point_count (0., 0., 0.) in
  let point u v = (u mod major_segments) * minor_segments + (v mod minor_segments) in
  for u = 0 to major_segments - 1 do
    let alpha = 2. *. Float.pi *. float_of_int u /. float_of_int major_segments in
    for v = 0 to minor_segments - 1 do
      let beta = 2. *. Float.pi *. float_of_int v /. float_of_int minor_segments in
      let radial = major +. (minor *. cos beta) in
      points.(point u v) <- transform ~center ~rotation
          (radial *. cos alpha, minor *. sin beta, radial *. sin alpha)
    done
  done;
  let triangle_count = 2 * major_segments * minor_segments in
  let vertices = Array.make (triangle_count * 3) 0 and cursor = ref 0 in
  let triangle a b c =
    vertices.(!cursor) <- a; vertices.(!cursor + 1) <- b;
    vertices.(!cursor + 2) <- c; cursor := !cursor + 3 in
  for u = 0 to major_segments - 1 do
    for v = 0 to minor_segments - 1 do
      let a = point u v and b = point u (v + 1)
      and c = point (u + 1) (v + 1) and d = point (u + 1) v in
      triangle a b c; triangle a c d
    done
  done;
  geometry points vertices (Array.init (triangle_count + 1) (fun i -> i * 3))
  |> outward

let star_prism ~teeth ~center:(cx, cy, cz) ~rotation ~inner ~outer ~depth =
  let ring = teeth * 2 in
  let points = Array.make (ring * 2) (0., 0., 0.) in
  for layer = 0 to 1 do
    let y = if layer = 0 then -.depth *. 0.5 else depth *. 0.5 in
    for i = 0 to ring - 1 do
      let angle = 2. *. Float.pi *. float_of_int i /. float_of_int ring in
      let radius = if i land 1 = 0 then outer else inner in
      points.((layer * ring) + i) <- transform ~center:(cx, cy, cz) ~rotation
          (radius *. cos angle, y, radius *. sin angle)
    done
  done;
  let primitive_count = 2 + ring in
  let vertex_count = ring * 6 in
  let vertices = Array.make vertex_count 0 and offsets = Array.make (primitive_count + 1) 0
  and cursor = ref 0 and primitive = ref 0 in
  let polygon values =
    offsets.(!primitive) <- !cursor;
    Array.iter (fun value -> vertices.(!cursor) <- value; incr cursor) values;
    incr primitive in
  polygon (Array.init ring (fun i -> ring - 1 - i));
  polygon (Array.init ring (fun i -> ring + i));
  for i = 0 to ring - 1 do
    let next = (i + 1) mod ring in
    polygon [|i; next; ring + next; ring + i|]
  done;
  offsets.(primitive_count) <- !cursor;
  geometry points vertices offsets |> outward

let box ~center ~rotation ~size ~divisions =
  Box_generator.box ~grain:64 ~center:(let x, y, z = center in Rays_math.Vec3.create x y z)
    ~rotation:(let x, y, z = rotation in Rays_math.Vec3.create x y z)
    ~size:(let x, y, z = size in Rays_math.Vec3.create x y z)
    ~x_divisions:divisions ~y_divisions:divisions ~z_divisions:divisions
    ~connectivity:Box_generator.Box_quads ~consolidate_points:true () |> get

let merge geometries =
  let point_count = Array.fold_left (fun n g -> n + Geometry.point_count g) 0 geometries
  and vertex_count = Array.fold_left (fun n g -> n + Geometry.vertex_count g) 0 geometries
  and primitive_count = Array.fold_left (fun n g -> n + Geometry.primitive_count g) 0 geometries in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and vertices = Array.make vertex_count 0
  and offsets = Array.make (primitive_count + 1) 0 in
  let point_base = ref 0 and vertex_base = ref 0 and primitive_base = ref 0 in
  Array.iter (fun source ->
      let positions = Packed.Float3.View.view (Geometry.positions source)
      and topology = Topology.View.view (Geometry.topology source) in
      Array.blit positions.x 0 x !point_base (Array.length positions.x);
      Array.blit positions.y 0 y !point_base (Array.length positions.y);
      Array.blit positions.z 0 z !point_base (Array.length positions.z);
      Array.iteri (fun i point -> vertices.(!vertex_base + i) <- !point_base + point)
        topology.vertex_points;
      for primitive = 0 to Geometry.primitive_count source - 1 do
        offsets.(!primitive_base + primitive) <-
          !vertex_base + topology.primitive_offsets.(primitive)
      done;
      point_base := !point_base + Geometry.point_count source;
      vertex_base := !vertex_base + Geometry.vertex_count source;
      primitive_base := !primitive_base + Geometry.primitive_count source)
    geometries;
  offsets.(primitive_count) <- vertex_count;
  let topology = Topology.polygons_owned ~point_count ~vertex_points:vertices
      ~primitive_offsets:offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let recipes level =
  let latitude, longitude, major_segments, minor_segments, divisions = match level with
    | Quick -> 8, 12, 12, 6, 2
    | Standard -> 16, 24, 24, 10, 6
    | Full -> 32, 48, 48, 18, 16 in
  let epsilon = Float.next_after 0. Float.infinity in
  [|
    { name = "near_coplanar_boxes";
      left = box ~center:(0., 0., 0.) ~rotation:(0., 0., 0.)
          ~size:(2., 2., 2.) ~divisions;
      right = box ~center:(1. -. epsilon, 0.17, -0.11) ~rotation:(0., 0., 0.)
          ~size:(2., 1.7, 1.8) ~divisions;
      left_treatment = Boolean_solid.Solid; right_treatment = Boolean_solid.Solid;
      seam_points = Boolean.Shared_seam_points;
      resolve_left = false; resolve_right = false };
    { name = "rotated_ellipsoids";
      left = sphere ~latitude ~longitude ~center:(0., 0., 0.)
          ~rotation:(0.17, 0.31, -0.11) ~radius:(1.7, 1.05, 1.3);
      right = sphere ~latitude ~longitude ~center:(0.83, 0.12, -0.19)
          ~rotation:(-0.23, 0.14, 0.37) ~radius:(1.25, 1.4, 0.9);
      left_treatment = Boolean_solid.Solid; right_treatment = Boolean_solid.Solid;
      seam_points = Boolean.Shared_seam_points;
      resolve_left = false; resolve_right = false };
    { name = "torus_star";
      left = torus ~major_segments ~minor_segments ~center:(0., 0., 0.)
          ~rotation:(0.23, -0.18, 0.07) ~major:1.25 ~minor:0.48;
      right = star_prism ~teeth:(match level with Quick -> 7 | Standard -> 17 | Full -> 31)
          ~center:(0.25, 0., 0.12) ~rotation:(0.41, 0.16, -0.22)
          ~inner:0.64 ~outer:1.55 ~depth:1.25;
      left_treatment = Boolean_solid.Solid; right_treatment = Boolean_solid.Solid;
      seam_points = Boolean.Shared_seam_points;
      resolve_left = false; resolve_right = false };
    { name = "disconnected_drill_bank";
      left = box ~center:(0., 0., 0.) ~rotation:(0.08, -0.13, 0.04)
          ~size:(5.2, 3.3, 2.7) ~divisions;
      right = merge (Array.init (match level with Quick -> 3 | Standard -> 7 | Full -> 15)
          (fun i ->
            let count = match level with Quick -> 3 | Standard -> 7 | Full -> 15 in
            let x = -2.2 +. (4.4 *. float_of_int i /. float_of_int (count - 1)) in
            torus ~major_segments ~minor_segments ~center:(x, 0., 0.)
              ~rotation:(Float.pi *. 0.5, 0., 0.) ~major:0.42 ~minor:0.16));
      left_treatment = Boolean_solid.Solid; right_treatment = Boolean_solid.Solid;
      seam_points = Boolean.Shared_seam_points;
      resolve_left = false; resolve_right = true }
  |]

let signature geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  { x = Array.copy positions.x; y = Array.copy positions.y; z = Array.copy positions.z;
    vertices = Array.copy topology.vertex_points;
    offsets = Array.copy topology.primitive_offsets;
    kinds = Bytes.copy topology.primitive_kinds }

let equal_signature a b =
  a.x = b.x && a.y = b.y && a.z = b.z
  && a.vertices = b.vertices && a.offsets = b.offsets
  && Bytes.equal a.kinds b.kinds

let validate_finite name geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  for point = 0 to Array.length positions.x - 1 do
    if not (Float.is_finite positions.x.(point) && Float.is_finite positions.y.(point)
        && Float.is_finite positions.z.(point)) then
      fail "%s: non-finite output point %d" name point
  done

let validate_closed_manifold name geometry =
  if Geometry.primitive_count geometry > 0 then begin
    let index = Topology_index.create (Geometry.topology geometry) in
    let boundary = Topology_index_front.boundary_edge_count index
    and non_manifold = Topology_index_front.non_manifold_edge_count index in
    if boundary <> 0 || non_manifold <> 0 then
      fail "%s: output is not closed two-manifold (boundary=%d non_manifold=%d)"
        name boundary non_manifold
  end

let validate_closed_even_incidence name geometry =
  let index = Topology_index.create (Geometry.topology geometry) in
  for edge = 0 to Topology_index.edge_count index - 1 do
    let incidence = Topology_index.edge_incidence_count index edge in
    if incidence < 2 || incidence land 1 <> 0 then
      fail "%s: edge %d has non-closed incidence %d" name edge incidence
  done

let validate_shatter_groups name geometry =
  let groups = [|"boolean_left"; "boolean_overlap"; "boolean_right"|]
      |> Array.map (fun group ->
        match Geometry.find_group ~owner:Group.Primitive group geometry with
        | Some value -> value | None -> fail "%s: missing Shatter group %s" name group) in
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    let count = Array.fold_left
        (fun n group -> n + if Group.mem primitive group then 1 else 0) 0 groups in
    if count <> 1 then fail "%s: Shatter primitive %d has %d piece memberships"
        name primitive count
  done

let volume geometry = abs_float (Analysis.signed_volume geometry)

let expected_volume operation ~left ~right ~intersection = match operation with
  | Boolean_solid.Union | Boolean_solid.Shatter -> left +. right -. intersection
  | Boolean_solid.Intersection -> intersection
  | Boolean_solid.Difference -> left -. intersection
  | Boolean_solid.Reverse_difference -> right -. intersection
  | Boolean_solid.Xor -> left +. right -. (2. *. intersection)

let validate_volume name expected actual =
  let tolerance = 1e-8 *. (1. +. max (abs_float expected) (abs_float actual)) in
  if abs_float (expected -. actual) > tolerance then
    fail "%s: volume identity differs (expected %.17g, got %.17g, tolerance %.3g)"
      name expected actual tolerance

let write_obj path geometry =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
      let positions = Packed.Float3.View.view (Geometry.positions geometry)
      and topology = Topology.View.view (Geometry.topology geometry) in
      for point = 0 to Array.length positions.x - 1 do
        Printf.fprintf channel "v %.17g %.17g %.17g\n" positions.x.(point)
          positions.y.(point) positions.z.(point)
      done;
      for primitive = 0 to Geometry.primitive_count geometry - 1 do
        output_char channel 'f';
        for vertex = topology.primitive_offsets.(primitive)
            to topology.primitive_offsets.(primitive + 1) - 1 do
          Printf.fprintf channel " %d" (topology.vertex_points.(vertex) + 1)
        done;
        output_char channel '\n'
      done)

let words line =
  let length = String.length line and values = ref [] and start = ref (-1) in
  let finish stop =
    if !start >= 0 then begin
      values := String.sub line !start (stop - !start) :: !values;
      start := -1
    end in
  for index = 0 to length - 1 do
    match line.[index] with
    | ' ' | '\t' | '\r' -> finish index
    | _ -> if !start < 0 then start := index
  done;
  finish length;
  List.rev !values

let obj_point_index ~path ~line ~point_count token =
  let slash = match String.index_opt token '/' with
    | None -> String.length token | Some index -> index in
  let raw = String.sub token 0 slash in
  let index = try int_of_string raw with Failure _ ->
    fail "%s:%d: invalid OBJ face index %S" path line token in
  let point = if index > 0 then index - 1 else point_count + index in
  if point < 0 || point >= point_count then
    fail "%s:%d: OBJ point index %d is outside 1..%d" path line index point_count;
  point

let load_obj path =
  let channel = open_in_bin path in
  let points = ref [] and faces = ref [] and line_number = ref 0 in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
      try
        while true do
          let line = input_line channel in
          incr line_number;
          match words line with
          | "v" :: sx :: sy :: sz :: _ ->
              let coordinate value = try float_of_string value with Failure _ ->
                fail "%s:%d: invalid OBJ coordinate %S" path !line_number value in
              let point = coordinate sx, coordinate sy, coordinate sz in
              let x, y, z = point in
              if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
                fail "%s:%d: non-finite OBJ point" path !line_number;
              points := point :: !points
          | "f" :: corners when List.length corners >= 3 ->
              let point_count = List.length !points in
              faces := Array.of_list (List.map
                  (obj_point_index ~path ~line:!line_number ~point_count) corners)
                :: !faces
          | "f" :: _ -> fail "%s:%d: OBJ face has fewer than three corners"
              path !line_number
          | _ -> ()
        done
      with End_of_file -> ());
  let points = Array.of_list (List.rev !points)
  and faces = Array.of_list (List.rev !faces) in
  if Array.length points = 0 || Array.length faces = 0 then
    fail "%s: OBJ must contain points and polygon faces" path;
  let vertex_count = Array.fold_left (fun count face -> count + Array.length face) 0 faces in
  let vertices = Array.make vertex_count 0
  and offsets = Array.make (Array.length faces + 1) 0 and cursor = ref 0 in
  Array.iteri (fun primitive face ->
      offsets.(primitive) <- !cursor;
      Array.iter (fun point -> vertices.(!cursor) <- point; incr cursor) face) faces;
  offsets.(Array.length faces) <- vertex_count;
  geometry points vertices offsets

let ensure_directory path =
  if Sys.file_exists path then begin
    if not (Sys.is_directory path) then fail "%s exists and is not a directory" path
  end else Unix.mkdir path 0o755

let write_case_manifest path recipes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
    output_string channel "case,resolve_left_self_intersections,resolve_right_self_intersections\n";
    Array.iter (fun recipe -> Printf.fprintf channel "%s,%b,%b\n"
        recipe.name recipe.resolve_left recipe.resolve_right) recipes)

let run_once ~domains ~grain operation recipe = Parallel.run ~domains (fun () ->
    Boolean_front.run ~grain ~operation
      ~left_treatment:recipe.left_treatment
      ~right_treatment:recipe.right_treatment
      ~seam_points:recipe.seam_points
      ~resolve_left_self_intersections:recipe.resolve_left
      ~resolve_right_self_intersections:recipe.resolve_right
      ~right:recipe.right recipe.left |> get)

let median values =
  let copy = Array.copy values in Array.sort Float.compare copy;
  copy.(Array.length copy / 2)

let csv value =
  if String.contains value ',' || String.contains value '"'
      || String.contains value '\n' then
    "\"" ^ String.concat "\"\"" (String.split_on_char '"' value) ^ "\""
  else value

let run_product ~domains ~grain ~repeats ~left_volume ~right_volume
    ~intersection_volume ~output_obj recipe operation =
  let label = recipe.name ^ "/" ^ operation_name operation in
  try
    let baseline = run_once ~domains:1 ~grain operation recipe in
    validate_finite label baseline;
    (match operation with
     | Boolean_solid.Shatter ->
         validate_shatter_groups label baseline;
         validate_closed_even_incidence label baseline
     | Boolean_solid.Xor -> validate_closed_even_incidence label baseline
     | Boolean_solid.Difference
       when recipe.left_treatment = Boolean_solid.Solid
         && recipe.right_treatment = Boolean_solid.Surface ->
         validate_closed_even_incidence label baseline
     | Boolean_solid.Reverse_difference
       when recipe.left_treatment = Boolean_solid.Surface
         && recipe.right_treatment = Boolean_solid.Solid ->
         validate_closed_even_incidence label baseline
    | Boolean_solid.Union | Boolean_solid.Intersection | Boolean_solid.Difference
     | Boolean_solid.Reverse_difference -> validate_closed_manifold label baseline);
    Option.iter (fun path -> write_obj path baseline) output_obj;
    Option.iter (fun intersection_volume ->
      validate_volume label
        (expected_volume operation ~left:left_volume ~right:right_volume
           ~intersection:intersection_volume)
        (volume baseline)) intersection_volume;
    let expected = signature baseline and times = Array.make repeats 0.
    and allocations = Array.make repeats 0. in
    for repeat = 0 to repeats - 1 do
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
      let output = run_once ~domains ~grain operation recipe in
      times.(repeat) <- Unix.gettimeofday () -. started;
      allocations.(repeat) <- Gc.allocated_bytes () -. allocated;
      if not (equal_signature expected (signature output)) then
        fail "%s: exact output differs between one and %d domains/repeat %d"
          label domains repeat
    done;
    Printf.printf "%s,%s,pass,%d,%d,%d,%.6f,%.0f,%.17g,\n%!"
      recipe.name (operation_name operation) domains
      (Geometry.point_count baseline) (Geometry.primitive_count baseline)
      (median times) (median allocations) (volume baseline);
    true
  with Failure message ->
    Printf.printf "%s,%s,fail,%d,,,,,,%s\n%!" recipe.name
      (operation_name operation) domains (csv message);
    false

let parse () =
  let level = ref Quick and domains = ref (Parallel.recommended_domains ())
  and repeats = ref 2 and grain = ref 128 and export_dir = ref None
  and operation = ref None and case_name = ref None
  and left_obj = ref None and right_obj = ref None
  and output_obj = ref None
  and resolve_left = ref false and resolve_right = ref false
  and left_surface = ref false and right_surface = ref false
  and split_seams = ref false in
  let specs = [
    "--level", Arg.String (fun value -> level := level_of_string value),
      "quick|standard|full generated corpus density";
    "--domains", Arg.Int (fun value -> domains := value), "parallel domain count";
    "--repeats", Arg.Int (fun value -> repeats := value), "determinism/timing repeats";
    "--grain", Arg.Int (fun value -> grain := value), "parallel scheduling grain";
    "--operation", Arg.String (fun value -> operation := Some (operation_of_string value)),
      "run one operation";
    "--export-dir", Arg.String (fun value -> export_dir := Some value),
      "write paired OBJ oracle inputs";
    "--case", Arg.String (fun value -> case_name := Some value),
      "case name for an external OBJ pair";
    "--left-obj", Arg.String (fun value -> left_obj := Some value),
      "left closed polygon OBJ for a real-model run";
    "--right-obj", Arg.String (fun value -> right_obj := Some value),
      "right closed polygon OBJ for a real-model run";
    "--output-obj", Arg.String (fun value -> output_obj := Some value),
      "write the one-domain Boolean result as OBJ";
    "--resolve-left-self-intersections", Arg.Set resolve_left,
      "enable exact left-input self-intersection resolution";
    "--resolve-right-self-intersections", Arg.Set resolve_right,
      "enable exact right-input self-intersection resolution";
    "--left-surface", Arg.Set left_surface,
      "treat an external left OBJ as a zero-volume surface";
    "--right-surface", Arg.Set right_surface,
      "treat an external right OBJ as a zero-volume surface";
    "--split-seam-points", Arg.Set split_seams,
      "duplicate Boolean seam points per incident primitive component" ] in
  Arg.parse specs (fun value -> fail "unexpected argument %s" value)
    "boolean_stress [options]";
  if !domains < 1 || !repeats < 1 || !grain < 1 then
    fail "domains, repeats, and grain must be positive";
  let external_recipe = match !case_name, !left_obj, !right_obj with
    | None, None, None -> None
    | Some name, Some left, Some right ->
        if String.trim name = "" then fail "external OBJ case name must not be empty";
        Some { name; left = load_obj left; right = load_obj right;
          left_treatment = if !left_surface then Boolean_solid.Surface else Boolean_solid.Solid;
          right_treatment = if !right_surface then Boolean_solid.Surface else Boolean_solid.Solid;
          seam_points = if !split_seams then Boolean.Split_seam_points
            else Boolean.Shared_seam_points;
          resolve_left = !resolve_left; resolve_right = !resolve_right }
    | _ -> fail "--case, --left-obj, and --right-obj must be supplied together" in
  !level, !domains, !repeats, !grain, !export_dir, !operation, !output_obj,
  external_recipe

let () =
  let level, domains, repeats, grain, export_dir, selected, output_obj,
      external_recipe = parse () in
  let recipes = match external_recipe with
    | Some recipe -> [|recipe|] | None -> recipes level in
  Option.iter (fun directory ->
      ensure_directory directory;
      write_case_manifest (Filename.concat directory "cases.csv") recipes;
      Array.iter (fun recipe ->
          write_obj (Filename.concat directory (recipe.name ^ "_left.obj")) recipe.left;
          write_obj (Filename.concat directory (recipe.name ^ "_right.obj")) recipe.right)
        recipes) export_dir;
  let operations = match selected with
    | Some operation -> [|operation|]
    | None -> [|Boolean_solid.Union; Boolean_solid.Intersection; Boolean_solid.Difference;
        Boolean_solid.Reverse_difference; Boolean_solid.Xor; Boolean_solid.Shatter|] in
  if Option.is_some output_obj
      && (Array.length recipes <> 1 || Array.length operations <> 1) then
    fail "--output-obj requires exactly one case and one operation";
  Printf.printf
    "case,operation,status,domains,points,primitives,median_seconds,median_current_domain_allocated_bytes,absolute_signed_volume,error\n%!";
  let failures = ref 0 in
  Array.iter (fun recipe ->
      (match recipe.left_treatment with
       | Boolean_solid.Solid -> validate_closed_manifold (recipe.name ^ "/left") recipe.left
       | Boolean_solid.Surface -> validate_finite (recipe.name ^ "/left") recipe.left);
      (match recipe.right_treatment with
       | Boolean_solid.Solid -> validate_closed_manifold (recipe.name ^ "/right") recipe.right
       | Boolean_solid.Surface -> validate_finite (recipe.name ^ "/right") recipe.right);
      let left_volume = volume recipe.left and right_volume = volume recipe.right in
      let intersection_volume = if recipe.resolve_left || recipe.resolve_right
          || recipe.left_treatment = Boolean_solid.Surface
          || recipe.right_treatment = Boolean_solid.Surface
        then None else
        try
          let intersection = run_once ~domains:1 ~grain Boolean_solid.Intersection recipe in
          validate_closed_manifold (recipe.name ^ "/intersection") intersection;
          Some (volume intersection)
        with Failure _ -> None in
      Array.iter (fun operation ->
        if not (run_product ~domains ~grain ~repeats ~left_volume ~right_volume
            ~intersection_volume ~output_obj recipe operation) then incr failures)
        operations) recipes;
  if !failures <> 0 then exit 1
