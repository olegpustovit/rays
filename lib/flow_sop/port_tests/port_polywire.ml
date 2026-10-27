(* sop/polywire (the same engine and fixtures as sop/sweep_circle: port_sweep_circle.ml reuses this file). The node has two native
   paths: Sweep_circle.run takes the legacy fast path of mesh/sweep_circle.ml when the group, segments,
   joint, smoothing, seam and UV-range options are all at the defaults of that path (use_u_range and
   use_v_range off), and the general engine curve/polywire.ml otherwise. Both are covered: the legacy cases
   switch the two range toggles off. Fixtures: open and closed curves of irregular lengths with every
   attribute the options read (point, vertex), shared points, unreferenced points, point/vertex normals and
   uv, a polygon (refused) and a repeated point (refused). *)
open Rdk
open Port_gate
open Port_loft_support
module P = Param

let tx name v = name, P.Text_value v
let fl name v = name, P.Float_value v
let it name v = name, P.Int_value v
let bo name v = name, P.Bool_value v

let ok = Rdk_test_support.get_string_ok
let add owner name storage geometry =
  Geometry.with_attribute (Attribute.create_owned ~owner ~name storage |> ok) geometry |> ok
let planes n f = Array.init n f
let f2 n fx fy = Attribute.Float2 (Packed.Float2.of_owned ~x:(planes n fx) ~y:(planes n fy) |> ok)
let f3 n fx fy fz = Attribute.Float3 (Packed.Float3.of_owned ~x:(planes n fx) ~y:(planes n fy) ~z:(planes n fz) |> ok)
let f4 n fx fy fz fw = Attribute.Float4 (Packed.Float4.of_owned ~x:(planes n fx) ~y:(planes n fy) ~z:(planes n fz)
  ~w:(planes n fw) |> ok)

(* Every named attribute the options read. *)
let with_options geometry =
  let np = Geometry.point_count geometry and nv = Geometry.vertex_count geometry in
  geometry
  |> add Attribute.Point "scale" (Attribute.Float (planes np (fun i -> 0.6 +. (0.1 *. float (i mod 5)))))
  |> add Attribute.Point "seam" (Attribute.Int (planes np (fun i -> (i mod 4) - 1)))
  |> add Attribute.Point "vv" (Attribute.Float (planes np (fun i -> 0.05 *. float i)))
  |> add Attribute.Point "up" (f3 np (fun i -> 0.3 +. (0.1 *. sin (float i))) (fun _ -> 0.2) (fun _ -> 1.))
  |> add Attribute.Point "divs" (Attribute.Int (planes np (fun i -> 3 + (i mod 4))))
  |> add Attribute.Point "segs" (Attribute.Int (planes np (fun i -> 1 + (i mod 3))))
  |> add Attribute.Point "smooth" (Attribute.Float (planes np (fun i -> if i mod 3 = 0 then 0. else 1.)))
  |> add Attribute.Point "mjs" (Attribute.Float (planes np (fun i -> 1.5 +. (0.5 *. float (i mod 3)))))
  |> add Attribute.Point "neg" (Attribute.Float (planes np (fun i -> if i = 2 then -0.5 else 1.)))
  |> add Attribute.Point "small" (Attribute.Int (planes np (fun i -> if i = 3 then 2 else 5)))
  |> add Attribute.Point "zero" (Attribute.Int (planes np (fun i -> if i = 3 then 0 else 2)))
  |> add Attribute.Vertex "sscale" (f2 nv (fun i -> 0.1 +. (0.05 *. float (i mod 3))) (fun i -> 0.7 +. (0.1 *. float (i mod 3))))
  |> add Attribute.Vertex "sbad" (f2 nv (fun _ -> 0.8) (fun _ -> 0.3))
  |> add Attribute.Vertex "sseam" (Attribute.Int (planes nv (fun i -> (i mod 5) - 2)))
  |> add Attribute.Vertex "uvr" (f4 nv (fun i -> 0.1 *. float (i mod 3)) (fun i -> 1. +. (0.2 *. float (i mod 2)))
       (fun i -> -0.2 +. (0.1 *. float (i mod 4))) (fun i -> 1.5 -. (0.1 *. float (i mod 3))))

let with_uv geometry =
  let nv = Geometry.vertex_count geometry in
  add Attribute.Vertex "uv" (f2 nv (fun i -> 0.25 *. float i) (fun i -> 1. -. (0.125 *. float i))) geometry

let shape = [sec ~kind:open_k 5 0.; sec 6 1. ~radius:1.3; sec ~kind:open_k 4 2. ~radius:0.7; sec 7 3. ~phase:0.5;
             sec ~kind:open_k 6 4. ~radius:0.9]
let curves = with_options (decorate (build shape))
let plain = with_options (build [sec ~kind:open_k 5 0.; sec 6 1. ~radius:1.3])
let closed_only = with_options (build [sec 5 0.; sec 6 1. ~radius:1.2])
let one_open = with_options (build [sec ~kind:open_k 7 0.])

(* Curves that share points, unreferenced points among them, and optionally a retained polygon. *)
let shared_geometry ~polygon =
  let vertex_points, offsets, kinds =
    if polygon then [|0; 1; 2; 3; 2; 4; 5; 4; 6; 7; 8|], [|0; 4; 7; 11|], [|Topology.Open_polyline; Open_polyline; Polygon|]
    else [|0; 1; 2; 3; 2; 4; 5; 4; 2; 6|], [|0; 4; 7; 10|], [|Topology.Open_polyline; Open_polyline; Closed_polyline|] in
  let topology = Topology.create_owned ~point_count:12 ~vertex_points ~primitive_offsets:offsets
    ~primitive_kinds:kinds |> ok in
  let n = 12 in
  let positions = Packed.Float3.of_owned ~x:(planes n (fun i -> float i *. 0.9 +. (0.3 *. sin (float i))))
    ~y:(planes n (fun i -> 0.4 *. float (i mod 3) +. cos (float i))) ~z:(planes n (fun i -> 0.25 *. float (i mod 4)))
    |> ok in
  with_options (decorate (Geometry.create ~positions ~topology () |> ok))

let repeated =
  let g = build [sec ~kind:open_k 5 0.; sec 6 1. ~radius:1.3] in
  let view = Packed.Float3.Private.view (Geometry.positions g) in
  let x = Array.copy view.x and y = Array.copy view.y and z = Array.copy view.z in
  x.(1) <- x.(0); y.(1) <- y.(0); z.(1) <- z.(0);
  with_options (Geometry.create ~positions:(Packed.Float3.of_owned ~x ~y ~z |> ok)
    ~topology:(Geometry.topology g) () |> ok)

(* The inputs of the gate must cook at their defaults (the cache check), so the refusals and the retained
   polygon are checked separately, before the table runs. *)
let polygons = with_options (fixture_attributes (fixture_groups (polygon_polyline ())))
let inputs = List.map (fun (label, g) -> label, [g]) [
  "empty", decorate (points 0); "one open", one_open; "closed", closed_only;
  "shared curves", shared_geometry ~polygon:false;
  "vertex normals", with_uv (with_normals Attribute.Vertex curves);
  "curves", curves ]

let legacy = [bo "use_u_range" false; bo "use_v_range" false]
let cases =
  let c label changes = label, changes in
  [ c "radius" [fl "radius" 0.25];
    c "sides" [it "sides" 5]; c "sides 3" [it "sides" 3]; c "sides 11" [it "sides" 11];
    c "no sides" [bo "use_sides" false]; c "sides over 4096" [it "sides" 5000];
    c "group" [tx "group" "dest_a"]; c "group padded" [tx "group" " dest_a "]; c "group missing" [tx "group" "nothing"];
    c "group empty selection" [tx "group" "old_x"];
    c "divisions attribute" [tx "divisions_attribute" "divs"];
    c "divisions attribute small" [tx "divisions_attribute" "small"];
    c "divisions attribute zero" [tx "divisions_attribute" "zero"];
    c "divisions attribute integer" [tx "divisions_attribute" "integer"];
    c "divisions attribute missing" [tx "divisions_attribute" "nothing"];
    c "divisions attribute float" [tx "divisions_attribute" "float"];
    c "segments" [it "segments" 3]; c "segments attribute" [tx "segments_attribute" "segs"];
    c "segments attribute zero" [tx "segments_attribute" "zero"];
    c "segments and attribute" [it "segments" 2; tx "segments_attribute" "segs"];
    c "segments over limit" [it "segments" 2000000];
    c "segment scales" [bo "use_segment_scales" true; it "segments" 4];
    c "segment scales range" [bo "use_segment_scales" true; fl "first_segment_scale" 0.2; fl "last_segment_scale" 0.8; it "segments" 5];
    c "segment scales two" [bo "use_segment_scales" true; fl "first_segment_scale" 0.2; fl "last_segment_scale" 0.8; it "segments" 2];
    c "segment scales equal" [bo "use_segment_scales" true; fl "first_segment_scale" 0.5; fl "last_segment_scale" 0.5; it "segments" 4];
    c "segment scales zero" [bo "use_segment_scales" true; fl "first_segment_scale" 0.; fl "last_segment_scale" 0.; it "segments" 3];
    c "segment scales reversed" [bo "use_segment_scales" true; fl "first_segment_scale" 0.8; fl "last_segment_scale" 0.2];
    c "segment scales attribute" [tx "segment_scales_attribute" "sscale"; it "segments" 4];
    c "segment scales attribute bad" [tx "segment_scales_attribute" "sbad"; it "segments" 4];
    c "segment scales attribute wrong" [tx "segment_scales_attribute" "vec3"];
    c "segment scales attribute missing" [tx "segment_scales_attribute" "nothing"];
    c "buckling" [bo "prevent_joint_buckling" true];
    c "buckling limit" [bo "prevent_joint_buckling" true; fl "maximum_joint_scale" 1.2];
    c "buckling attribute" [bo "prevent_joint_buckling" true; tx "maximum_joint_scale_attribute" "mjs"];
    c "buckling attribute without" [tx "maximum_joint_scale_attribute" "mjs"];
    c "buckling attribute bad" [bo "prevent_joint_buckling" true; tx "maximum_joint_scale_attribute" "neg"];
    c "buckling segments" [bo "prevent_joint_buckling" true; it "segments" 3; bo "use_segment_scales" true];
    c "buckling no smooth" [bo "prevent_joint_buckling" true; bo "smooth_point" false];
    c "no smooth" [bo "smooth_point" false];
    c "no smooth segments" [bo "smooth_point" false; it "segments" 3];
    c "smooth attribute" [tx "smooth_attribute" "smooth"];
    c "smooth attribute segments" [tx "smooth_attribute" "smooth"; it "segments" 2; bo "use_segment_scales" true];
    c "smooth attribute wrong" [tx "smooth_attribute" "integer"];
    c "valence 1" [bo "use_max_valence" true; it "max_valence" 1];
    c "valence 2" [bo "use_max_valence" true; it "max_valence" 2];
    c "valence 4" [bo "use_max_valence" true];
    c "scale attribute" [tx "scale_attribute" "scale"]; c "scale attribute negative" [tx "scale_attribute" "neg"];
    c "scale attribute integer" [tx "scale_attribute" "integer"];
    c "seam" [it "seam_offset" 3]; c "seam negative" [it "seam_offset" (-2)]; c "seam large" [it "seam_offset" 37];
    c "seam attribute" [tx "seam_attribute" "seam"]; c "seam both" [it "seam_offset" 2; tx "seam_attribute" "seam"];
    c "seam attribute float" [tx "seam_attribute" "float"];
    c "segment seam" [tx "segment_seam_attribute" "sseam"];
    c "segment seam divisions" [tx "segment_seam_attribute" "sseam"; tx "divisions_attribute" "divs"];
    c "segment seam missing" [tx "segment_seam_attribute" "nothing"];
    c "v attribute" [tx "v_attribute" "vv"]; c "v attribute no uv" [tx "v_attribute" "vv"; bo "generate_uv" false];
    c "v attribute missing" [tx "v_attribute" "nothing"];
    c "up attribute" [tx "up_attribute" "up"]; c "up attribute segments" [tx "up_attribute" "up"; it "segments" 3];
    c "up attribute integer" [tx "up_attribute" "integer"];
    c "no uv" [bo "generate_uv" false]; c "u range" [fl "u_min" 0.2; fl "u_max" 1.7];
    c "no u range" [bo "use_u_range" false]; c "v range" [fl "v_min" (-0.5); fl "v_max" 2.];
    c "no v range" [bo "use_v_range" false]; c "no ranges" legacy;
    c "uv range attribute" [tx "uv_range_attribute" "uvr"];
    c "uv range attribute no ranges" (tx "uv_range_attribute" "uvr" :: legacy);
    c "uv range attribute missing" [tx "uv_range_attribute" "nothing"];
    c "uv range attribute no uv" [tx "uv_range_attribute" "nothing"; bo "generate_uv" false];
    c "caps" [bo "caps" true]; c "caps group" [bo "caps" true; tx "cap_group" "ends"];
    c "caps no group" [bo "caps" true; tx "cap_group" ""];
    c "caps uv ranges" [bo "caps" true; tx "uv_range_attribute" "uvr"; tx "segment_seam_attribute" "sseam"];
    c "caps segments" [bo "caps" true; it "segments" 3];
    c "caps divisions" [bo "caps" true; tx "divisions_attribute" "divs"; it "seam_offset" 1];
    (* The legacy path: every option of it, with the toggles off. *)
    c "legacy" legacy; c "legacy sides" (it "sides" 6 :: legacy); c "legacy no sides" (bo "use_sides" false :: legacy);
    c "legacy caps" (bo "caps" true :: legacy); c "legacy caps group" (bo "caps" true :: tx "cap_group" "ends" :: legacy);
    c "legacy scale" (tx "scale_attribute" "scale" :: legacy); c "legacy scale bad" (tx "scale_attribute" "neg" :: legacy);
    c "legacy seam" (it "seam_offset" 3 :: tx "seam_attribute" "seam" :: legacy);
    c "legacy v" (tx "v_attribute" "vv" :: legacy); c "legacy up" (tx "up_attribute" "up" :: legacy);
    c "legacy up caps" (tx "up_attribute" "up" :: bo "caps" true :: legacy);
    c "legacy radius" (fl "radius" 0.3 :: legacy);
    c "legacy buckling off max" (fl "maximum_joint_scale" 3. :: legacy) ]

let bench () =
  let sections = List.init 6_000 (fun i -> sec ~kind:(if i mod 3 = 0 then closed_k else open_k) 12 (float i *. 0.01)
    ~radius:(1. +. (0.001 *. float (i mod 7)))) in
  [build sections], [it "sides" 8; bo "caps" true]

(* ---- scratch debugging (PW_DEBUG=1 [PW_INPUT=n] [PW_CHANGES=name=value,...]): first differences ---- *)
let debug () =
  let dir = Filename.concat (Filename.dirname Sys.executable_name) "../../../../../lib/flow_sop/nodes" in
  let node = List.find (fun (n : Port_gate.t) -> n.key = "polywire") (Port_gate.registered ()) in
  let changes = match Sys.getenv_opt "PW_CHANGES" with
    | None -> []
    | Some text -> List.filter_map (fun item -> match String.split_on_char '=' item with
        | [k; v] -> Some (k, (match v with "true" -> P.Bool_value true | "false" -> P.Bool_value false
            | _ -> (match int_of_string_opt v with Some i -> P.Int_value i
                | None -> (match float_of_string_opt v with Some f -> P.Float_value f | None -> P.Text_value v))))
        | _ -> None) (String.split_on_char ',' text) in
  let (_, geometries) = List.hd node.inputs in
  let ctx = Port_gate.context ~dir node geometries in
  let expected = ok (Port_gate.native ctx changes) in
  let actual = match Port_gate.lisp ctx changes with Ok b -> fst (ok (b false)) | Error m -> failwith m in
  let cook n = Port_gate.with_session (fun s -> match Port_gate.cook s 1 97 n with
    | Ok g -> Ok g | Error e -> Error (Diagnostic.error_to_string e)) in
  match cook expected, cook actual with
  | Error a, Error b -> Printf.printf "both errors:\n native %s\n lisp %s\n" a b
  | Error a, Ok _ -> Printf.printf "native error: %s\n" a
  | Ok _, Error b -> Printf.printf "lisp error: %s\n" b
  | Ok e, Ok g ->
    let ve = Topology.Private.view (Geometry.topology e) and vg = Topology.Private.view (Geometry.topology g) in
    Printf.printf "points %d/%d vertices %d/%d primitives %d/%d\n" (Geometry.point_count e) (Geometry.point_count g)
      (Array.length ve.vertex_points) (Array.length vg.vertex_points)
      (Bytes.length ve.primitive_kinds) (Bytes.length vg.primitive_kinds);
    let first_diff a b = let n = min (Array.length a) (Array.length b) in
      let rec go i = if i >= n then None else if a.(i) <> b.(i) then Some i else go (i + 1) in go 0 in
    (match first_diff ve.vertex_points vg.vertex_points with
     | Some i -> Printf.printf "vertex_points differ at %d: native %d lisp %d\n" i ve.vertex_points.(i) vg.vertex_points.(i)
     | None -> print_endline "vertex_points equal (common prefix)");
    (match first_diff ve.primitive_offsets vg.primitive_offsets with
     | Some i -> Printf.printf "primitive_offsets differ at %d: native %d lisp %d\n" i ve.primitive_offsets.(i) vg.primitive_offsets.(i)
     | None -> print_endline "primitive_offsets equal (common prefix)");
    if Bytes.compare ve.primitive_kinds vg.primitive_kinds <> 0 then print_endline "primitive kinds differ";
    let pe = Packed.Float3.Private.view (Geometry.positions e) and pg = Packed.Float3.Private.view (Geometry.positions g) in
    let n = min (Array.length pe.x) (Array.length pg.x) in
    let ds = ref [] in
    for i = 0 to n - 1 do
      if pe.x.(i) <> pg.x.(i) || pe.y.(i) <> pg.y.(i) || pe.z.(i) <> pg.z.(i) then ds := i :: !ds done;
    Printf.printf "positions differing at %d points; first: %s\n" (List.length !ds)
      (String.concat " " (List.take 12 (List.rev_map string_of_int !ds)));
    (match List.rev !ds with
     | i :: _ -> Printf.printf "  native (%.17g %.17g %.17g)\n  lisp   (%.17g %.17g %.17g)\n" pe.x.(i) pe.y.(i) pe.z.(i) pg.x.(i) pg.y.(i) pg.z.(i)
     | [] -> ());
    let names g = List.map (fun a -> Printf.sprintf "%s/%s" (match Attribute.owner a with Attribute.Point -> "P" | Vertex -> "V" | Primitive -> "F" | Detail -> "D") (Attribute.name a)) (Geometry.attributes g) in
    Printf.printf "attributes native: %s\nattributes lisp:   %s\n" (String.concat " " (names e)) (String.concat " " (names g));
    let group_names g = List.map (fun gr -> Group.name gr) (Geometry.groups g) in
    Printf.printf "groups native: %s\ngroups lisp:   %s\n" (String.concat " " (group_names e)) (String.concat " " (group_names g));
    List.iter (fun a ->
      match Geometry.find_attribute ~owner:(Attribute.owner a) (Attribute.name a) g with
      | None -> Printf.printf "  lisp lacks %s\n" (Attribute.name a)
      | Some b ->
        let report name w at = if w > 0. then Printf.printf "  %s: worst %g at %d\n" name w at in
        (match Attribute.Private.storage a, Attribute.Private.storage b with
         | Attribute.Float2 x, Attribute.Float2 y ->
           let vx = Packed.Float2.Private.view x and vy = Packed.Float2.Private.view y in
           let w = ref 0. and at = ref (-1) in
           for i = 0 to min (Array.length vx.x) (Array.length vy.x) - 1 do
             let d = Float.max (abs_float (vx.x.(i) -. vy.x.(i))) (abs_float (vx.y.(i) -. vy.y.(i))) in
             if d > !w || (Float.is_nan d && !at < 0) then (w := d; at := i) done;
           report (Attribute.name a) !w !at
         | Attribute.Float3 x, Attribute.Float3 y ->
           let vx = Packed.Float3.Private.view x and vy = Packed.Float3.Private.view y in
           let w = ref 0. and at = ref (-1) in
           for i = 0 to min (Array.length vx.x) (Array.length vy.x) - 1 do
             let d = Float.max (abs_float (vx.x.(i) -. vy.x.(i))) (Float.max (abs_float (vx.y.(i) -. vy.y.(i))) (abs_float (vx.z.(i) -. vy.z.(i)))) in
             if d > !w then (w := d; at := i) done;
           report (Attribute.name a) !w !at
         | Attribute.Float x, Attribute.Float y ->
           let w = ref 0. and at = ref (-1) in
           for i = 0 to min (Array.length x) (Array.length y) - 1 do
             let d = abs_float (x.(i) -. y.(i)) in if d > !w then (w := d; at := i) done;
           report (Attribute.name a) !w !at
         | Attribute.Int x, Attribute.Int y ->
           (match first_diff x y with Some i -> Printf.printf "  %s differs at %d: %d vs %d\n" (Attribute.name a) i x.(i) y.(i) | None -> ())
         | _ -> ())) (Geometry.attributes e);
    Printf.printf "close_geometry 4 ulps: %b\n" (Rdk_test_support.close_geometry ~ulps:4 e g)

let debugging = Sys.getenv_opt "PW_DEBUG" <> None
let make key native =
  if debugging then
    { key; native; inputs = [List.nth inputs (try int_of_string (Sys.getenv "PW_INPUT") with Not_found -> 0)];
      cases = []; ulps = 4; bench = None }
  else match Sys.getenv_opt "PW_CASES" with
    | Some text ->
      (* scratch: PW_CASES=prefix,prefix [PW_INPUT=n]: only the matching cases, on one fixture (or the last) *)
      let prefixes = String.split_on_char ',' text in
      let matches label = List.exists (fun p -> p = "*" || (String.length label >= String.length p && String.sub label 0 (String.length p) = p)) prefixes in
      { key; native;
        inputs = (match Sys.getenv_opt "PW_INPUT" with Some n -> [List.nth inputs (int_of_string n)] | None -> [List.nth inputs (List.length inputs - 1)]);
        cases = List.filter (fun (l, _) -> matches l) cases; ulps = 4; bench = None }
    | None -> { key; native; inputs; cases; ulps = 4; bench = Some bench }
let node = make "polywire" Sop.Nodes.Polywire.factory
let () = register node

let extra node =
  let dir = Filename.concat (Filename.dirname Sys.executable_name) "../../../../../lib/flow_sop/nodes" in
  let run (fixture, g) cases =
    let ctx = Port_gate.context ~dir node [g] in
    List.iter (fun (label, changes) ->
    try Port_gate.verify_case ~full:false ctx 4 (fixture ^ "/" ^ label) changes
    with Failure m -> Printf.printf "FAIL %s extra: %s\n%!" node.key m; exit 1) cases in
  run ("polygons", polygons) [
    "defaults", []; "group polyline", [tx "group" "dest_a"]; "group polygon", [tx "group" "a"];
    "group polyline caps", [tx "group" "dest_a"; bo "caps" true]; "group legacy", tx "group" "dest_a" :: legacy;
    "group none", [tx "group" "old_x"]; "group none legacy", tx "group" "old_x" :: legacy ];
  run ("shared polygon", shared_geometry ~polygon:true) [
    "defaults", []; "group", [tx "group" "dest_a"]; "group caps", [tx "group" "dest_a"; bo "caps" true];
    "group segments", [tx "group" "dest_a"; it "segments" 2; bo "use_segment_scales" true];
    "group no smooth", [tx "group" "dest_a"; bo "smooth_point" false]; "group other", [tx "group" "old_x"];
    "group valence", [tx "group" "dest_a"; bo "use_max_valence" true; it "max_valence" 1] ];
  run ("repeated", repeated) [
    "defaults", []; "legacy", legacy; "group", [tx "group" "dest_a"]; "segment scales", [bo "use_segment_scales" true];
    "no smooth", [bo "smooth_point" false] ];
  Printf.printf "PASS %s extra: retained polygon and refusals\n%!" node.key
let run_extra_for key node = if Array.length Sys.argv >= 2 && Sys.argv.(1) = key && not (Array.mem "--bench" Sys.argv) && Sys.getenv_opt "PW_DEBUG" = None && Sys.getenv_opt "PW_CASES" = None then extra node
let () = run_extra_for "polywire" node
let () = if Sys.getenv_opt "PW_DEBUG" <> None then at_exit (fun () -> try debug () with e -> print_endline (Printexc.to_string e))
