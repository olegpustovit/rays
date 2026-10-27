(* sop/scatter: one geometry input (pending draft). Custom fixtures add density attributes on every
   owner ("pd" point, "vd" vertex, "prd" primitive, "dd" detail), a point/vertex "N" and "Cd", and a
   triangle-only mesh (the direct path) next to polygons with five corners and a curve. *)
open Rdk
open Rdk_test_support
open Port_gate
module P = Param
let t name v = name, P.Text_value v
let c name v = name, P.Choice_value v
let b name v = name, P.Bool_value v
let i name v = name, P.Int_value v
let add owner name storage geometry =
  get_string_ok (Geometry.with_attribute (get_string_ok (Attribute.create_owned ~owner ~name storage)) geometry)
let count_of geometry = function
  | Attribute.Point -> Geometry.point_count geometry | Vertex -> Geometry.vertex_count geometry
  | Primitive -> Geometry.primitive_count geometry | Detail -> 1
let with_float owner name f geometry =
  add owner name (Attribute.Float (Array.init (count_of geometry owner) f)) geometry
let with_vec3 owner name f geometry =
  let n = count_of geometry owner in
  add owner name (Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.init n (fun k -> let x, _, _ = f k in x))
    ~y:(Array.init n (fun k -> let _, y, _ = f k in y)) ~z:(Array.init n (fun k -> let _, _, z = f k in z))
    |> get_string_ok)) geometry
let with_vec4 owner name f geometry =
  let n = count_of geometry owner in
  let plane g = Array.init n (fun k -> g (f k)) in
  add owner name (Attribute.Float4 (Packed.Float4.of_owned ~x:(plane (fun (x, _, _, _) -> x))
    ~y:(plane (fun (_, y, _, _) -> y)) ~z:(plane (fun (_, _, z, _) -> z)) ~w:(plane (fun (_, _, _, w) -> w))
    |> get_string_ok)) geometry
let densities geometry =
  geometry
  |> with_float Attribute.Point "pd" (fun k -> float (k mod 4) *. 0.75)
  |> with_float Attribute.Vertex "vd" (fun k -> float ((k * 3) mod 5) *. 0.5)
  |> with_float Attribute.Primitive "prd" (fun k -> [| 1.; 2.; 0.5; 1.; 0. |].(k mod 5))
  |> with_float Attribute.Detail "dd" (fun _ -> 2.)
  |> add Attribute.Point "intd" (Attribute.Int (Array.make (Geometry.point_count geometry) 1))
let polygons ~curve =
  let x = [| 0.; 1.; 0.; 2.; 3.; 3.; 2.; 5.; 5.9; 6.5; 6.; 5.2; 9.; 10.; 11.; 12.; 13.; 12. |]
  and y = [| 0.; 0.; 1.; 0.; 0.; 1.; 1.; 0.2; 0.; 0.6; 1.4; 1.; 0.; 1.; 0.; 0.; 0.; 1. |]
  and z = [| 0.; 0.; 0.; 0.5; 0.5; 0.5; 0.5; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 1.; 0.; 1.; 1. |] in
  let tb = Rdk_test_support.Tb.create ~point_count:18 () in
  Rdk_test_support.Tb.add_triangle tb 0 1 2;
  Rdk_test_support.Tb.add_polygon tb [| 3; 4; 5; 6 |];
  Rdk_test_support.Tb.add_polygon tb [| 7; 8; 9; 10; 11 |];
  if curve then Rdk_test_support.Tb.add_open_polyline tb [| 12; 13; 14 |]
  else Rdk_test_support.Tb.add_triangle tb 12 13 14;
  Rdk_test_support.Tb.add_triangle tb 15 16 17;
  get_string_ok (Geometry.create ~positions:(Packed.Float3.of_owned ~x ~y ~z |> get_string_ok)
    ~topology:(Rdk_test_support.Tb.freeze tb) ())
let normals owner g = with_vec3 owner "N" (fun k -> let a = float k in (sin a, cos a, 0.5 +. 0.1 *. a)) g
let colors owner g = with_vec4 owner "Cd" (fun k -> let a = float k in (a *. 0.1, 0.5, 1. -. a *. 0.05, 0.25 +. a *. 0.01)) g
let rich () = polygons ~curve:true |> fixture_groups |> fixture_attributes |> densities |> normals Attribute.Point
  |> colors Attribute.Vertex
let rich_vertex () = polygons ~curve:true |> fixture_groups |> fixture_attributes |> densities
  |> normals Attribute.Vertex |> colors Attribute.Point
let rich_plain () = polygons ~curve:false |> fixture_groups |> densities
let tris () =
  Plane_generators.grid ~connectivity:Grid_triangles ~columns:8 ~rows:8 ~size:4. () |> get_ok
  |> fixture_groups |> fixture_attributes |> densities |> normals Attribute.Point
let sliver () =
  let tb = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle tb 0 1 2;
  Rdk_test_support.Tb.add_triangle tb 3 4 5;
  get_string_ok (Geometry.create ~positions:(Packed.Float3.of_owned ~x:[| 0.; 1.; 0.; 0.; 1.; 2. |]
    ~y:[| 0.; 0.; 1.; 0.; 0.; 0. |] ~z:[| 0.; 0.; 0.; 0.; 0.; 0. |] |> get_string_ok)
    ~topology:(Rdk_test_support.Tb.freeze tb) ()) |> fixture_groups |> densities
(* A float "id" blocks interpolation of the same name into the integer id. *)
let bad_id () = rich_plain () |> with_float Attribute.Point "id" (fun k -> float k)
let bad_n () = rich_plain () |> with_float Attribute.Point "N" (fun k -> float k)
let inputs = List.map (fun (label, g) -> label, [g]) [
  "empty", fixture_groups (points 0); "single", fixture_groups (points 1);
  "grid4225", fixture_groups (grid ());
  "mixed", fixture_attributes (fixture_groups (polygon_polyline ()));
  "bad_id", bad_id (); "bad_n", bad_n (); "sliver", sliver (); "tris", tris (); "rich_vertex", rich_vertex ();
  "rich", rich ()]
let density owner name = [b "use_density" true; c "density_owner" owner; t "density_attribute" name]
let cases = [
  "count0", [i "count" 0]; "count1", [i "count" 1]; "count7", [i "count" 7];
  "count500", [i "count" 500; i "seed" 3];
  "seed", [i "seed" 9999]; "context", [b "context_seed" true];
  "group", [t "group" " a "; i "count" 40]; "group_a", [t "group" "dest_a"; i "count" 40];
  "group_missing", [t "group" "zzz"]; "group_empty_name", [t "group" "  "];
  "density_prim", density "Primitive" "prd"; "density_point", density "Point" "pd" @ [i "count" 300];
  "density_vertex", density "Vertex" "vd" @ [i "count" 300]; "density_detail", density "Detail" "dd";
  "density_default_owner", [b "use_density" true; t "density_attribute" "prd"];
  "density_missing", density "Point" "nothing"; "density_int", density "Point" "intd";
  "density_off", [b "use_density" false; c "density_owner" "Vertex"];
  "density_blank", [b "use_density" true; t "density_attribute" " "];
  "density_P", [b "use_density" true; t "density_attribute" "P"];
  "density_count0", density "Point" "nothing" @ [i "count" 0];
  "density_group", density "Point" "pd" @ [t "group" "a"; i "count" 200];
  "transfer_point", [t "point_pattern" "*"; i "count" 60]; "transfer_none_match", [t "point_pattern" "nothing"];
  "transfer_names", [t "point_pattern" " float vec3 text integer"; i "count" 33];
  "transfer_arrays", [t "point_pattern" "float_rows int_rows"; i "count" 25];
  "transfer_exclude", [t "point_pattern" "* ^float"];
  "transfer_P", [t "point_pattern" "P"]; "transfer_P_N", [t "point_pattern" "P N Cd"; i "count" 50];
  "transfer_N", [t "point_pattern" "N Cd"];
  "transfer_vertex", [t "vertex_pattern" "*"; i "count" 60];
  "transfer_vertex_N", [t "vertex_pattern" "N Cd"];
  "transfer_primitive", [t "primitive_pattern" "*"; i "count" 60];
  "transfer_detail", [t "detail_pattern" "*"; i "count" 60];
  "transfer_all", [t "point_pattern" "float integer"; t "vertex_pattern" "text vec2"; t "primitive_pattern" "vec3 vec4";
                   t "detail_pattern" "float_rows int_rows"];
  "transfer_duplicate", [t "point_pattern" "float"; t "vertex_pattern" "float"];
  "transfer_bad_pattern", [t "point_pattern" "["]; "transfer_bad_detail_pattern", [t "detail_pattern" "["];
  "transfer_mixed_bad", [t "point_pattern" "id"]; "transfer_n_all", [t "primitive_pattern" "N"];
  "groups", [t "point_pattern" "a"; b "match_groups" true; i "count" 80];
  "groups_all", [t "point_pattern" "*"; t "vertex_pattern" "*"; t "primitive_pattern" "*"; b "match_groups" true];
  "groups_without_pattern", [b "match_groups" true];
  "groups_detail_only", [t "detail_pattern" "*"; b "match_groups" true];
  "groups_names", [t "vertex_pattern" "dest_a old_x"; t "primitive_pattern" "a"; b "match_groups" true];
  "groups_vertex_only", [t "vertex_pattern" "*"; b "match_groups" true; i "count" 200];
  "groups_primitive_only", [t "primitive_pattern" "*"; b "match_groups" true; i "count" 200];
  "prov_primitive", [t "source_primitive_attribute" "src"; i "count" 30];
  "prov_vertices", [t "source_vertex_numbers_attribute" "sv"; t "source_vertex_weights_attribute" "sw"; i "count" 30];
  "prov_all", [t "source_primitive_attribute" " src "; t "source_vertex_numbers_attribute" "sv";
               t "source_vertex_weights_attribute" "sw"; t "point_pattern" "float"];
  "prov_one_side", [t "source_vertex_numbers_attribute" "sv"];
  "prov_other_side", [t "source_vertex_weights_attribute" "sw"];
  "prov_P", [t "source_primitive_attribute" "P"];
  "prov_same", [t "source_primitive_attribute" "x"; t "source_vertex_numbers_attribute" "x";
                t "source_vertex_weights_attribute" "y"];
  "prov_overrides_id", [t "source_primitive_attribute" "id"; i "count" 12];
  "prov_over_N", [t "source_primitive_attribute" "N"; i "count" 12];
  "prov_existing", [t "source_primitive_attribute" "float"; t "point_pattern" "float"; i "count" 12];
  "prov_existing_rows", [t "source_vertex_numbers_attribute" "int_rows"; t "source_vertex_weights_attribute" "float_rows";
                         t "point_pattern" "*"];
  "prov_blank_names", [t "source_primitive_attribute" " "; t "source_vertex_numbers_attribute" " ";
                       t "source_vertex_weights_attribute" " "];
  "everything", [t "point_pattern" "*"; t "vertex_pattern" "*"; t "primitive_pattern" "*"; t "detail_pattern" "*";
                 b "match_groups" true; t "source_primitive_attribute" "src";
                 t "source_vertex_numbers_attribute" "sv"; t "source_vertex_weights_attribute" "sw";
                 b "use_density" true; c "density_owner" "Point"; t "density_attribute" "pd"; i "count" 120; i "seed" 7];
]
let bench () =
  [get_string_ok (Plane_generators.grid ~connectivity:Grid_quads ~columns:710 ~rows:710 ~size:8. () |> Result.map_error Error.to_string)],
  [i "count" 1_000_000]
let () = Printexc.register_printer (fun e ->
  let r = Obj.repr e in
  if Obj.tag r = 0 && Obj.size r = 2 && Obj.is_block (Obj.field r 0) && Obj.tag (Obj.field r 0) = Obj.object_tag
     && (Obj.obj (Obj.field (Obj.field r 0) 0) : string) = "Flow_sop.Lower.Fail"
  then Some ("Lower.Fail " ^ Diagnostic.to_string (Obj.obj (Obj.field r 1))) else None)
(* Dropped case "density_nan" (a NaN point density): the language refuses non-finite array input with
   E_NONFINITE before the body can report scatter's invalid_geometry (PP.md work order 2, one contract for all ports). *)
(* Debugging aid: SC_INPUT=<index> and SC_CASES=a,b restrict the fixtures and cases. *)
let inputs = match Sys.getenv_opt "SC_INPUT" with Some n -> [List.nth inputs (int_of_string n)] | None -> inputs
let cases = match Sys.getenv_opt "SC_CASES" with
  | Some l -> let l = String.split_on_char ',' l in List.filter (fun (n, _) -> List.mem n l) cases | None -> cases
let () = register { key = "scatter"; native = Sop.Nodes.Scatter.factory; inputs; cases; ulps = 4; bench = Some bench }
