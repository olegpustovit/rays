(* sop/distance_from_target: one geometry input (pending draft).
   The gate renders every change as a scalar keyword, so it cannot set the grouped vec3 parameters
   (origin, direction): the Lisp side takes them as :origin [x y z], native as origin_x/_y/_z.
   Those are covered by [vector_cases] below, run after the gate with the same checks. *)
open Rdk_test_support
open Port_gate
module P = Param
let text name value = name, P.Text_value value
let choice name value = name, P.Choice_value value
let num name value = name, P.Float_value value
let cases =
  List.map (fun owner -> Printf.sprintf "group/%s" owner,
    [choice "affected_owner" owner; text "affected_group" "a"; text "mask_attribute" "mask"])
    ["Point"; "Vertex"; "Primitive"; "Edge"]
  @ List.concat_map (fun projection -> List.concat_map (fun falloff -> List.map (fun (r, extra) ->
      Printf.sprintf "%s/%s/%s" projection falloff r,
      [choice "projection" projection; choice "falloff" falloff; text "mask_attribute" "mask"] @ extra)
      ["max", []; "fixed", [choice "radius_mode" "Fixed"; num "radius" 3.5];
       "fixed_small", [choice "radius_mode" "Fixed"; num "radius" 0.4]])
      ["Linear"; "Quadratic"; "Cubic"]) ["Spherical"; "Cylindrical"; "Planar"]
  @ [ "signed", [choice "projection" "Planar"; choice "metric" "Signed"; text "mask_attribute" "mask"];
      "signed_only_mask", [choice "projection" "Planar"; choice "metric" "Signed";
                      text "distance_attribute" ""; text "mask_attribute" "mask"];
      "mask_only", [text "distance_attribute" ""; text "mask_attribute" "mask"];
      "names_trimmed", [text "distance_attribute" " dist "; text "mask_attribute" " m "];
      "existing_float", [text "distance_attribute" "float"; text "mask_attribute" "float_other"];
      "existing_float_group", [text "distance_attribute" "float"; text "mask_attribute" "float_other";
        text "affected_group" "a"];
      "existing_float_mask_fixed", [text "mask_attribute" "float"; choice "radius_mode" "Fixed"; num "radius" 0.6;
        text "affected_group" "a"];
      "existing_vec3_distance", [text "distance_attribute" "vec3"];
      "existing_vec3_mask", [text "mask_attribute" "vec3"];
      "existing_int_mask", [text "mask_attribute" "integer"];
      "missing_group", [text "affected_group" "nope"];
      "missing_edge_group", [choice "affected_owner" "Edge"; text "affected_group" "nope"];
      "fixed_zero_ignored_in_max", [num "radius" 0.; text "mask_attribute" "mask"] ]
(* Construction refusals surface when the node is lowered, which the gate's [Error _, Ok _] arm
   does not accept, so they run in [vector_cases] with the same native comparison. *)
let refusals =
  [ "refuse_signed_spherical", [choice "metric" "Signed"], [];
    "refuse_same_names", [text "mask_attribute" "distance"], [];
    "refuse_same_names_trimmed", [text "mask_attribute" " distance "], [];
    "refuse_no_outputs", [text "distance_attribute" " "], [];
    "refuse_P", [text "distance_attribute" "P"], [];
    "refuse_P_mask", [text "mask_attribute" " P "], [];
    "refuse_fixed_zero", [choice "radius_mode" "Fixed"; num "radius" 0.], [] ]
let bench () =
  [points 1_000_000], [choice "projection" "Cylindrical"; text "mask_attribute" "mask"; choice "falloff" "Cubic"]
let node = { key = "distance_from_target"; native = Sop.Nodes.Distance_from_target.factory;
  inputs = standard_inputs; cases; ulps = 4; bench = Some bench }
let () = register node

(* ---- grouped vec3 parameters ---- *)
let vector_cases =
  let origin = ["origin", (0.75, -0.5, 0.125)] and direction = ["direction", (0.3, 0.8, -0.4)] in
  List.concat_map (fun (projection, scalars) -> [
    projection ^ "/origin", scalars, origin;
    projection ^ "/direction", scalars, direction;
    projection ^ "/both", scalars, origin @ direction;
    projection ^ "/axis_x", scalars, ["direction", (1., 0., 0.)];
    projection ^ "/axis_z_origin", scalars, ["direction", (0., 0., -2.); "origin", (3., 0.25, 2.)];
    projection ^ "/fixed", scalars @ [choice "radius_mode" "Fixed"; num "radius" 2.5; choice "falloff" "Cubic"],
      origin @ direction])
    ["Spherical", [text "mask_attribute" "mask"];
     "Cylindrical", [choice "projection" "Cylindrical"; text "mask_attribute" "mask"];
     "Planar", [choice "projection" "Planar"; text "mask_attribute" "mask"];
     "Signed", [choice "projection" "Planar"; choice "metric" "Signed"; text "mask_attribute" "mask"]]
  @ refusals @ List.map (fun (label, changes) -> label, changes, []) cases @ [ "refuse_zero_planar", [choice "projection" "Planar"], ["direction", (0., 0., 0.)];
      "refuse_zero_cylindrical", [choice "projection" "Cylindrical"], ["direction", (0., 0., 0.)];
      "zero_spherical_ok", [], ["direction", (0., 0., 0.)];
      "tiny_direction", [choice "projection" "Planar"], ["direction", (0., 1e-300, 0.)];
      "huge_direction", [choice "projection" "Cylindrical"], ["direction", (1e200, 1e200, 0.)] ]
let components = function
  | "origin" -> ["origin_x"; "origin_y"; "origin_z"] | _ -> ["direction_x"; "direction_y"; "direction_z"]
let vector_check ~dir =
  let count = ref 0 in
  List.iter (fun (fixture, geometries) ->
    let ctx = context ~dir node geometries in
    List.iter (fun (label, scalars, vectors) -> incr count;
      let at what = Printf.sprintf "distance_from_target/%s/%s: %s" fixture label what in
      let natives = scalars @ List.concat_map (fun (name, (x, y, z)) ->
        List.map2 (fun c v -> num c v) (components name) [x; y; z]) vectors in
      let words = String.concat " " (List.map (fun (k, v) -> ":" ^ k ^ " " ^ render v) scalars
        @ List.map (fun (name, (x, y, z)) -> Printf.sprintf ":%s [%s %s %s]" name
          (Flow.Lisp.float x) (Flow.Lisp.float y) (Flow.Lisp.float z)) vectors) in
      let text = Printf.sprintf "(workspace port (graph g :context sop (sop/distance_from_target %s %s)))" ctx.head words in
      let build = match Flow.Syntax.parse text with
        | Error d -> Error (Diagnostic.to_string d)
        | Ok forms -> match Flow_sop.Catalog.check_workspace ~sources:ctx.sources ~ops:Flow_sop.Operators.all ctx.catalog forms with
          | Some w, ds when errors ds = [] ->
              (try match Flow_sop.Lower.of_checked ~reference:false ~factories:ctx.factories w with
               | Error d -> Error (Diagnostic.to_string d)
               | Ok lowered -> let g = List.hd lowered.graphs in
                   Result.map (fun n ->
                     check (Flow_sop.Catalog.descriptor (List.find (fun f -> key_of f = node.key) lowered.factories)
                       = Flow_sop.Catalog.descriptor node.native) "Catalog.descriptor differs from native";
                     n) (Sop.Edit_graph.compile_node g.network.geometry ~node_id:(Option.get g.root))
               with Invalid_argument m | Failure m -> Error m)
          | _, ds -> Error (String.concat "\n" (List.map Diagnostic.to_string ds)) in
      match native ctx natives, build with
      | Error _, Error _ -> ()
      | Error m, Ok _ -> fail (at ("native refused (" ^ m ^ ") but the Lisp node was accepted"))
      | Ok _, Error m -> fail (at ("Lisp refused a valid case: " ^ m))
      | Ok expected, Ok actual -> with_session (fun s -> match cook s 1 97 expected, cook s 1 97 actual with
          | Error e, Error g -> check (e.code = g.code) (at ("error code " ^ g.code ^ " but native " ^ e.code))
          | Error _, Ok _ -> fail (at "native error but Lisp succeeded")
          | Ok _, Error g -> fail (at ("Lisp error " ^ Diagnostic.error_to_string g ^ " but native succeeded"))
          | Ok e, Ok g ->
              check (Sop.Node.parameter_fields actual = Sop.Node.parameter_fields expected) (at "parameter_fields differ");
              check (Sop.Node.parameter_key actual = Sop.Node.parameter_key expected) (at "parameter_key differs");
              check (close_geometry ~ulps:4 e g) (at "Lisp differs from native beyond 4 ulps");
              let bytes = geometry_bytes g in
              List.iter (fun (domains, grain) -> Sop.Session.clear s;
                check (geometry_bytes (must (cook s domains grain actual)) = bytes)
                  (at (Printf.sprintf "bytes differ at domains=%d grain=%d" domains grain))) [1, 1; 4, 97; 4, 4096]))
      vector_cases) standard_inputs;
  Printf.printf "PASS distance_from_target vectors: %d cases per fixture\n%!" !count
let () = at_exit (fun () ->
  let args = List.tl (Array.to_list Sys.argv) in
  if List.mem "distance_from_target" args && not (List.mem "--bench" args) then begin
    let exe = Filename.dirname Sys.executable_name in
    let source = Filename.concat exe "../../../../../lib/flow_sop/nodes" in
    let dir = if Sys.file_exists source then source else Filename.concat exe "../nodes" in
    try vector_check ~dir with Failure m -> Printf.printf "FAIL %s\n%!" m; exit 1
  end)
