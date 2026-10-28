open Rays
open Sop
open Rdk_test_support

(* Rays Lisp is the contract: text the lowering refuses raises [Invalid_argument] here, so a test
   that expects a refused node says so with the same handler it always used. *)
module Lisp_sop = struct
  include Lisp_sop
  let node ?with_ text = match node_result ?with_ text with
    | Ok node -> node
    | Error message -> invalid_arg message
end

(* the engine's own extension point, as the removed typed custom constructor used it *)
let custom ?label ?(version = 1) ?(parameters = "")
    ?(cook_mode = Node.Generic) ?(dependencies = Context.Dependencies.static)
    ~operation inputs cook =
  Node.Private.make_geometry ?label ~operation ~version ~parameters ~cook_mode
    ~dependencies ~inputs:(Array.of_list inputs) (fun ~node_id:_ context geometries ->
      if Context.cancelled context then
        Error (Diagnostic.error ~code:"cancelled"
          "custom procedural node was cancelled before cooking")
      else match cook ~context (Array.copy geometries) with
        | Ok geometry -> Ok Node.Private.{ geometry; instances = None }
        | Error message ->
          Error (Diagnostic.error ~code:(operation ^ "_failed") ~cause:message ~hints:[]
            (operation ^ " could not produce valid geometry")))

(* What a native node's cache text said: a Lisp node's [Node.parameters] is its body digest, so the
   readable "name=value;..." form of the cook-relevant parameters is rebuilt from the schema view. *)
let readable node =
  let token label = String.map (function
    | 'a' .. 'z' | '0' .. '9' as c -> c | 'A' .. 'Z' as c -> Char.lowercase_ascii c | _ -> '_') label in
  match Node.parameter_fields node with
  | [] -> Node.parameters node
  | fields -> List.filter_map (fun (f : Param.field_view) ->
      if f.impact <> Param.Cook then None else Some (f.name ^ "=" ^ (match f.current with
        | Param.Bool_value b -> string_of_bool b
        | Int_value i -> string_of_int i
        | Float_value x -> Param.float_text x
        | Text_value text -> (match f.kind with Param.Choice_view _ -> token text | _ -> text)
        | Choice_value label -> token label))) fields
      |> String.concat ";"

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"
let edit_parameters graph ~node_id changes =
  Result.bind (Edit_graph.apply_parameters (Edit_graph.of_graph graph)
      ~node_id changes) (fun (document, effects) ->
    Result.map (fun graph -> graph, effects) (Edit_graph.compile document))
let cook_ok session context graph =
  match Session.cook session ~context graph with
  | Ok output -> output
  | Error error -> fail (Diagnostic.error_to_string error)

let context ?frame ?time ?seed ?domains ?grain ?cancel () =
  Context.create ?frame ?time ?seed ?domains ?grain ?cancel () |> get_ok

let session ?(entries = 64) ?(bytes = 64 * 1024 * 1024) () =
  Session.create ~max_entries:entries ~max_payload_bytes:bytes |> get_ok

(* The refusal message of a node text Lisp rejects, at lowering or at its first cook. *)
let refusal ?with_ text =
  match Lisp_sop.node_result ?with_ text with
  | Error message -> Some message
  | Ok node ->
      let evaluator = session () in
      let result = Session.cook evaluator ~context:(context ()) node in
      Session.close evaluator;
      (match result with Error error -> Some (Diagnostic.error_to_string error) | Ok _ -> None)

let equal_positions left right =
  let left = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions left)
  and right = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions right) in
  left.x = right.x && left.y = right.y && left.z = right.z

let contains text pattern =
  let text_length = String.length text and pattern_length = String.length pattern in
  let rec search index =
    index + pattern_length <= text_length
    && (String.sub text index pattern_length = pattern || search (index + 1))
  in
  pattern_length = 0 || search 0

(* a failure bound, never a pace: a loaded machine only takes longer *)
let wait_until ?(seconds = 60.) operation =
  let deadline = Unix.gettimeofday () +. seconds in
  let rec loop () =
    match operation () with
    | Some value -> value
    | None when Unix.gettimeofday () < deadline ->
        Unix.sleepf 0.001;
        loop ()
    | None -> fail "timed out waiting for background procedural cook"
  in
  loop ()

type inspectable_parameters = {
  translate_x : float;
  display_gain : float;
}

type encoded_parameters = { rules : int list }

let encoded_rules = Param.encoded ~equal:( = )
    ~encode:(fun values -> String.concat "," (List.map string_of_int values))
    ~decode:(fun text ->
      let text = String.trim text in
      if text = "" then Ok []
      else
        String.split_on_char ',' text
        |> List.fold_left (fun result token -> Result.bind result (fun values ->
          match int_of_string_opt (String.trim token) with
          | Some value -> Ok (value :: values)
          | None -> Error (Printf.sprintf "invalid integer %S" token))) (Ok [])
        |> Result.map List.rev)

let encoded_schema = Param.schema ~name:"encoded_rules"
    ~default:{ rules = [1; 2] } [
  Param.field ~name:"rules" ~kind:encoded_rules ~default:[1; 2]
    ~get:(fun value -> value.rules)
    ~set:(fun rules _value -> { rules }) ();
]

let inspectable_default = { translate_x = 0.; display_gain = 1. }

let inspectable_schema = Param.schema ~name:"inspectable_transform"
    ~default:inspectable_default [
  Param.field ~name:"translate_x" ~label:"Translate X"
    ~folder:["Transform"] ~kind:(Param.floating ~min:(-2.) ~max:2. ())
    ~default:0. ~get:(fun value -> value.translate_x)
    ~set:(fun translate_x value -> { value with translate_x }) ();
  Param.field ~name:"display_gain" ~label:"Display gain"
    ~folder:["Viewport"] ~impact:Param.View
    ~kind:(Param.floating ~min:0. ~max:2. ()) ~default:1.
    ~get:(fun value -> value.display_gain)
    ~set:(fun display_gain value -> { value with display_gain }) ();
]

let rec inspectable_transform ~label input parameters =
  (let migration_translation = Rays_math.Vec3.create parameters.translate_x 0. 0. in
Lisp_sop.node ~with_:["input", (input)] (Printf.sprintf {|(sop/transform (sop/ext_input) :mode "Matrix" :m03 %s :m13 %s :m23 %s)|} ((Lisp_sop.float migration_translation.Rays_math.Vec3.x)) ((Lisp_sop.float migration_translation.Rays_math.Vec3.y)) ((Lisp_sop.float migration_translation.Rays_math.Vec3.z))))
  |> Node.relabel label
  |> Node.parameterize ~schema:inspectable_schema ~values:parameters
       ~rebuild:(fun ~label ~inputs parameters -> match inputs with
         | [input] -> inspectable_transform ~label input parameters
         | _ -> invalid_arg "inspectable_transform expects one input")

let test_node_owned_parameters_and_graph_edit () =
  let source = Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|]) in
  let editable = inspectable_transform ~label:"move" source inspectable_default in
  let graph = Lisp_sop.node ~with_:["editable", editable]
      "(sop/merge (sop/ext_editable) (sop/ext_editable))" in
  let evaluator = session () in
  check (List.length (Node.parameter_fields editable) = 2
      && Node.has_parameters editable)
    "node-owned parameter metadata was not exposed";
  let original_infos = Node.inspect graph in
  let translated, effects = edit_parameters graph
      ~node_id:(Node.id editable)
      ["translate_x", Param.Float_value 1.5] |> get_ok in
  check (effects.cook && not effects.view && not effects.export)
    "graph edit lost cook impact";
  let edited = Option.get (Node.find translated ~node_id:(Node.id editable)) in
  check (Node.id edited = Node.id editable
      && List.map (fun info -> info.Node.id) (Node.inspect translated)
         = List.map (fun info -> info.Node.id) original_infos)
    "graph edit did not preserve logical node identities";
  (match Node.inputs translated with
   | [left; right] -> check (left == right && Node.id left = Node.id editable)
       "graph edit duplicated a shared subgraph"
   | _ -> fail "merge lost an input after the edit");
  let current = context () in
  ignore (cook_ok evaluator current graph);
  let before_edit = Session.stats evaluator in
  let output = cook_ok evaluator current translated in
  let after_edit = Session.stats evaluator in
  let positions = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
  check (positions.x = [|1.5; 1.5|])
    "edited node did not rebuild its cook closure";
  check (after_edit.hits > before_edit.hits)
    "stable graph edit did not reuse an unaffected cached input";
  let view_only, view_effects = edit_parameters translated
      ~node_id:(Node.id editable)
      ["display_gain", Param.Float_value 1.75] |> get_ok in
  check (not view_effects.cook && view_effects.view && not view_effects.export)
    "view-only node edit requested a cook";
  let before_view = Session.stats evaluator in
  ignore (cook_ok evaluator current view_only);
  check ((Session.stats evaluator).cooks = before_view.cooks)
    "view-only metadata invalidated cooked geometry";
  let view_node = Option.get (Node.find view_only ~node_id:(Node.id editable)) in
  let display = List.find (fun field -> field.Param.name = "display_gain")
      (Node.parameter_fields view_node) in
  check (display.current = Param.Float_value 1.75)
    "view-only node value was not retained for the inspector";
  Session.close evaluator

let test_encoded_parameter () =
  let initial = { rules = [1; 2] } in
  let field = Param.view encoded_schema initial |> List.hd in
  check (field.kind = Param.Text_view
      && field.current = Param.Text_value "1,2")
    "encoded parameter did not expose its deterministic text form";
  let edited, effects = Param.apply_all encoded_schema initial
      ["rules", Param.Text_value "3, 5, 8"] |> get_ok in
  check (edited.rules = [3; 5; 8] && effects.cook)
    "encoded parameter did not decode an inspector edit";
  check (Param.key encoded_schema initial
      <> Param.key encoded_schema edited)
    "encoded parameter did not participate in cache identity";
  check (Result.is_error (Param.apply_all encoded_schema initial
      ["rules", Param.Text_value "3,nope"]))
    "encoded parameter accepted an invalid structured edit"

let test_async_cook_latest_request () =
  let worker = Async_cook.create ~max_entries:4
      ~max_payload_bytes:4_000_000 |> get_ok in
  let slow = custom ~operation:"async_test_slow" ~version:1
      ~parameters:"delay=0.3" [Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|])]
      (fun ~context inputs ->
        Unix.sleepf 0.3;
        if Context.cancelled context then Error "cancelled"
        else Ok inputs.(0)) in
  let prepare output = Ok (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.Session.payload))) in
  let first = Async_cook.submit worker ~context:(context ~domains:2 ())
      ~node:slow ~prepare |> get_ok in
  ignore (wait_until (fun () -> match Async_cook.status worker with
    | Async_cook.Cooking _ -> Some ()
    | Idle -> None));
  let second = Async_cook.submit worker ~context:(context ~domains:2 ())
      ~node:(Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.; 1., 0., 0.; 2., 0., 0.|]))
      ~prepare |> get_ok in
  let completion = wait_until (fun () -> Async_cook.poll worker) in
  check (first <> second && completion.request_id = second)
    "async cook published a stale request";
  (match completion.result with
   | Ok 3 -> ()
   | Ok _ -> fail "async cook latest request returned the wrong geometry"
   | Error error -> fail (Async_cook.error_to_string error));
  check (Async_cook.status worker = Idle)
    "async cook remained busy after publishing the latest result";
  Async_cook.close worker;
  Async_cook.close worker

let test_lru_limits_and_lifetime () =
  let box = Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Triangles")|} in
  let graph = Lisp_sop.node ~with_:["box", box] "(sop/null (sop/ext_box))" in
  let evaluator = session ~entries:1 () in
  ignore (cook_ok evaluator (context ()) graph);
  let stats = Session.stats evaluator in
  check (stats.retained_entries = 1 && stats.evictions >= 1)
    "entry-bounded LRU did not evict";
  Session.clear evaluator;
  let stats = Session.stats evaluator in
  check (stats.retained_entries = 0 && stats.retained_payload_bytes = 0)
    "Session.clear retained payload";
  Session.close evaluator;
  (match Session.cook evaluator ~context:(context ()) graph with
   | Ok _ -> fail "closed session cooked"
   | Error error -> check (error.code = "session_closed") "closed session error code");
  let uncached = session ~entries:8 ~bytes:0 () in
  ignore (cook_ok uncached (context ()) box);
  check ((Session.stats uncached).retained_entries = 0)
    "payload budget retained oversized entry";
  Session.close uncached

let test_shared_input_memo () =
  let pick previous = custom ~operation:"pick_first" ~version:1
      [previous; previous] (fun ~context:_ inputs -> Ok inputs.(0)) in
  let rec chain node count = if count = 0 then node else chain (pick node) (count - 1) in
  let graph = chain (Lisp_sop.node {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 2 :rows 2 :size 1.0)|}) 30 in
  let evaluator = session () in
  ignore (cook_ok evaluator (context ()) graph);
  let stats = Session.stats evaluator in
  check (stats.misses = 31 && stats.hits = 0)
    "shared inputs were evaluated once per path instead of once per node";
  ignore (cook_ok evaluator (context ()) graph);
  let stats = Session.stats evaluator in
  check (stats.misses = 31 && stats.hits = 31)
    "shared inputs were looked up once per path on a warm cook";
  Session.close evaluator

let test_static_context_cache () =
  let graph =
    Lisp_sop.node {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 4 :rows 3 :size 5.0)|}
    |> (let migration_translation = Rays_math.Vec3.create 1. 2. 3. in
fun migration_input ->
  Lisp_sop.node ~with_:["migration_input", (migration_input)] (Printf.sprintf {|(sop/transform
   (sop/ext_migration_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|} ((Lisp_sop.float migration_translation.Rays_math.Vec3.x)) ((Lisp_sop.float migration_translation.Rays_math.Vec3.y)) ((Lisp_sop.float migration_translation.Rays_math.Vec3.z))))
  in
  let evaluator = session () in
  let first = cook_ok evaluator (context ~frame:0L ~time:0. ~seed:1L
      ~domains:1 ~grain:1 ()) graph in
  let after_first = Session.stats evaluator in
  check (after_first.cooks = 2 && after_first.misses = 2)
    "static graph first cook statistics";
  let second = cook_ok evaluator (context ~frame:99L ~time:42. ~seed:999L
      ~domains:4 ~grain:32 ()) graph in
  let after_second = Session.stats evaluator in
  check (after_second.cooks = 2 && after_second.hits = 2)
    "static graph was invalidated by unrelated context facts";
  check ((Result.get_ok (Sop.Payload.geometry first.payload)) == (Result.get_ok (Sop.Payload.geometry second.payload))) "static cache did not retain snapshot identity";
  Session.close evaluator

let test_grid_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/grid
   :counts "Point counts"
   :connectivity "Alternating triangles"
   :orientation "XY"
   :center [2.0 3.0 4.0]
   :width 6.0
   :height 2.0
   :rotation 0.25
   :uv_attribute "st"
   :columns 5
   :rows 3)|} in
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 15
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 48
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 16)
    "procedural Grid advanced cardinality";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "st"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Grid dropped normalized lattice coordinates";
  let rows_and_columns = Lisp_sop.node {|(sop/grid
   :width_mode "Auto"
   :height_mode "Auto"
   :counts "Point counts"
   :connectivity "Rows and columns"
   :orientation "YZ"
   :columns 4
   :rows 3
   :size 2.0)|}
      |> cook_ok evaluator current in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry rows_and_columns.payload)) = 12
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry rows_and_columns.payload)) = 24
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry rows_and_columns.payload)) = 7)
    "procedural Grid row-and-column topology";
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/grid
   :width_mode "Auto"
   :height_mode "Auto"
   :orientation "Custom axes"
   :horizontal %s
   :vertical %s
   :columns 2
   :rows 2)|} ((Lisp_sop.vec3 (Rays_math.Vec3.unit_x))) ((Lisp_sop.vec3 (Rays_math.Vec3.unit_x))))); false with Invalid_argument _ -> true)
    "procedural Grid refuses collinear custom axes at construction";
  Session.close evaluator

let test_circle_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/circle
   :arc "Sliced"
   :start_angle -0.4
   :end_angle 2.2
   :orientation "XY"
   :reverse true
   :center [2.0 3.0 4.0]
   :radius_x 3.0
   :rotation 0.25
   :uniform_scale 2.0
   :segments 8)|} in
  check (contains (readable graph) "arc=sliced")
    "procedural Circle cache identity omitted arc mode";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 10
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 10
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 1
      && Rdk.Topology.primitive_kind (Rdk.Geometry.topology (Result.get_ok (Sop.Payload.geometry output.payload))) 0
         = Rdk.Topology.Closed_polyline)
    "procedural sliced Circle topology";
  let open_arc = Lisp_sop.node (Printf.sprintf {|(sop/circle
   :radius_x_mode "Auto"
   :radius_y_mode "Auto"
   :arc "Open arc"
   :end_angle %s
   :orientation "YZ"
   :segments 12
   :radius 2.0)|} ((Lisp_sop.float Float.pi)))
      |> cook_ok evaluator current in
  (match Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry open_arc.payload)) with
   | Ok mesh -> check (Mesh.mode mesh = Mesh.Lines
         && Mesh.index_count mesh = 24)
       "procedural open Circle bridge"
   | Error error -> fail (Rdk.Error.to_string error));
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/circle
   :radius_x_mode "Auto"
   :radius_y_mode "Auto"
   :orientation "Custom axes"
   :horizontal %s
   :vertical %s
   :segments 8)|} ((Lisp_sop.vec3 Rays_math.Vec3.unit_x)) ((Lisp_sop.vec3 Rays_math.Vec3.unit_x)))); false with Invalid_argument _ -> true)
    "procedural Circle refuses collinear custom axes at construction";
  Session.close evaluator

let test_box_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/box :connectivity "Quads" :consolidate_points true
      :normals "Vertex" :center [2.0 3.0 4.0] :rotation [0.2 0.3 0.4]
      :rotation_order "YZX" :uniform_scale 1.5
      :x_divisions 2 :y_divisions 3 :z_divisions 4
      :uv_attribute "uv" :face_groups "side" :size [2.0 3.0 4.0])|} in
  check (contains (readable graph) "connectivity=quads"
      && contains (readable graph) "rotation_order=yzx")
    "procedural Box cache identity omitted advanced parameters";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 54
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 208
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 52
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "N"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "uv"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && List.length (Rdk.Geometry.groups (Result.get_ok (Sop.Payload.geometry output.payload))) = 6)
    "procedural divided Box output contract";
  let lattice = Lisp_sop.node {|(sop/box
   :normals "Auto"
   :connectivity "Lattice points"
   :x_divisions 2
   :y_divisions 3
   :z_divisions 4
   :size [2.0 3.0 4.0])|} |> cook_ok evaluator current in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry lattice.payload)) = 60
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry lattice.payload)) = 0)
    "procedural Box volume lattice";
  (* Lisp contract: the hard range of :x_divisions refuses zero before any cook *)
  (match Lisp_sop.node_result {|(sop/box :normals "Auto" :connectivity "Triangles" :x_divisions 0)|} with
   | Ok _ -> fail "procedural Box accepted zero divisions"
   | Error message ->
       check (contains message "E_HARD_RANGE" && contains message "x_divisions")
         "procedural Box division diagnostic");
  Session.close evaluator

let test_uv_sphere_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/uv_sphere
   :radius [2.0 1.5 0.75]
   :connectivity "Quads"
   :unique_points_per_pole true
   :triangular_poles false
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [2.0 3.0 4.0]
   :rotation [0.2 0.3 0.4]
   :rotation_order "ZXY"
   :uniform_scale 1.5
   :segments 12
   :rings 6)|} in
  check (contains (readable graph) "connectivity=quads"
      && contains (readable graph) "unique_points_per_pole=true"
      && contains (readable graph) "rotation_order=zxy")
    "procedural UV Sphere cache identity omitted advanced parameters";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 84
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 288
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 72
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "N"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "uv"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural advanced UV Sphere output contract";
  let points = Lisp_sop.node {|(sop/uv_sphere
   :radius_x_mode "Auto"
   :radius_y_mode "Auto"
   :radius_z_mode "Auto"
   :normals_mode "Auto"
   :connectivity "Points"
   :unique_points_per_pole true
   :segments 12
   :rings 6)|} |> cook_ok evaluator current in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry points.payload)) = 84
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry points.payload)) = 0
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "uv"
         (Result.get_ok (Sop.Payload.geometry points.payload)) <> None)
    "procedural UV Sphere point lattice";
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/uv_sphere :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero))));
      false with Invalid_argument _ -> true) "procedural UV Sphere refuses zero axis at construction";
  Session.close evaluator

let test_torus_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node (Printf.sprintf {|(sop/torus
   :connectivity "Alternating triangles"
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [2.0 3.0 4.0]
   :rotation [0.2 0.3 0.4]
   :rotation_order "ZXY"
   :uniform_scale 1.5
   :u_start 0.2
   :u_end 2.4
   :v_start %s
   :v_end %s
   :u_wrap false
   :v_wrap false
   :u_end_caps true
   :v_end_cap true
   :rows 8
   :columns 5
   :major_radius 3.0
   :minor_radius 1.0)|} ((Lisp_sop.float (-.Float.pi /. 2.))) ((Lisp_sop.float (Float.pi /. 2.)))) in
  check (contains (readable graph) "connectivity=alternating_triangles"
      && contains (readable graph) "rotation_order=zxy"
      && contains (readable graph) "u_end_caps=true"
      && contains (readable graph) "v_end_cap=true")
    "procedural Torus cache identity omitted advanced parameters";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 40
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 220
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 72
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "N"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "uv"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural capped Torus output contract";
  let points = Lisp_sop.node {|(sop/torus
   :normals_mode "Auto"
   :connectivity "Points"
   :rows 8
   :columns 5
   :major_radius 3.0
   :minor_radius 1.0)|} |> cook_ok evaluator current in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry points.payload)) = 40
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry points.payload)) = 0
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "uv"
         (Result.get_ok (Sop.Payload.geometry points.payload)) <> None)
    "procedural Torus point lattice";
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/torus :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero))));
      false with Invalid_argument _ -> true) "procedural Torus refuses zero axis at construction";
  Session.close evaluator

let test_tube_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/tube
   :connectivity "Alternating triangles"
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [2.0 3.0 4.0]
   :rotation [0.2 0.3 0.4]
   :rotation_order "ZXY"
   :radius_scale 1.5
   :rows 8
   :columns 5
   :top_radius 0.0
   :bottom_radius 3.0
   :height 4.0)|} in
  check (contains (readable graph) "connectivity=alternating_triangles"
      && contains (readable graph) "consolidate_cap_points=false"
      && contains (readable graph) "rotation_order=zxy"
      && contains (readable graph) "cap_group=caps")
    "procedural Tube cache identity omitted advanced parameters";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 41
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 200
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 66
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "N"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "uv"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "caps"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural capped Tube output contract";
  let points = Lisp_sop.node {|(sop/tube
   :normals_mode "Auto"
   :cap_group ""
   :consolidate_cap_points true
   :end_caps false
   :connectivity "Points"
   :rows 8
   :columns 5
   :top_radius 0.0
   :bottom_radius 3.0
   :height 4.0)|}
      |> cook_ok evaluator current in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry points.payload)) = 36
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry points.payload)) = 0
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "uv"
         (Result.get_ok (Sop.Payload.geometry points.payload)) <> None)
    "procedural Tube point lattice";
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/tube :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero))));
      false with Invalid_argument _ -> true) "procedural Tube refuses zero axis at construction";
  Session.close evaluator

let test_platonic_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/platonic
   :kind "Soccer ball"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [2.0 3.0 4.0]
   :rotation [0.2 0.3 0.4]
   :rotation_order "ZXY"
   :face_groups "face"
   :radius 3.0)|} in
  check (contains (readable graph) "kind=soccer_ball"
      && contains (readable graph) "normals=vertex"
      && contains (readable graph) "rotation_order=zxy"
      && contains (readable graph) "face_groups=face")
    "procedural Platonic cache identity omitted parameters";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 60
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 180
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 32
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "N"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "Cd"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "face_pentagons"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Platonic output contract";
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/platonic
   :normals "Point"
   :kind "Tetrahedron"
   :orientation "Custom axis"
   :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero)))); false
      with Invalid_argument _ -> true)
    "procedural Platonic refuses a zero axis at construction";
  Session.close evaluator

let test_spiral_generator_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(sop/spiral
   :extent_mode "Height and pitch"
   :height -6.0
   :pitch -0.75
   :radius_mode "Logarithmic end"
   :start_radius 0.4
   :end_radius 3.0
   :height_ramp "0:0.8,0.5:1.2,1:1"
   :radius_scale 1.3
   :radius_ramp "0:1,0.4:0.7,1:1.1"
   :direction "Clockwise"
   :start_angle 0.3
   :divisions_mode "Per curve"
   :divisions 40
   :uniform_angle false
   :spiral_count 3
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [2.0 3.0 4.0]
   :rotation [0.2 0.3 0.4]
   :rotation_order "ZXY"
   :uniform_scale 1.2
   :angle_attribute "angle"
   :x_axis_attribute "xaxis"
   :y_axis_attribute "yaxis"
   :tangent_attribute "tangent"
   :orient_attribute "orient"
   :distance_attribute "distance")|} in
  check (contains (readable graph) "extent_mode=height_and_pitch"
      && contains (readable graph) "radius_mode=logarithmic_end"
      && contains (readable graph) "height_ramp="
      && contains (readable graph) "uniform_angle=false"
      && contains (readable graph) "rotation_order=zxy"
      && contains (readable graph) "orient_attribute=orient")
    "procedural Spiral cache identity omitted advanced parameters";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 123
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 123
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 3
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "angle"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "xaxis"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "yaxis"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "tangent"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "orient"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "distance"
         (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Spiral output contract";
  check (try ignore (Lisp_sop.node (Printf.sprintf {|(sop/spiral :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero)))); false
      with Invalid_argument _ -> true)
    "procedural Spiral refuses a zero central axis at construction";
  Session.close evaluator

let test_declared_seed_dependency () =
  let graph =
    Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 5 :rows 5 :size 4.0)
     (sop/noise_displace :context_seed true :amplitude 0.8 :frequency 0.3))|}
  in
  let evaluator = session () in
  let first = cook_ok evaluator (context ~seed:10L ~domains:1 ()) graph in
  let second = cook_ok evaluator (context ~seed:11L ~domains:4 ~time:8. ()) graph in
  check (not (equal_positions (Result.get_ok (Sop.Payload.geometry first.payload)) (Result.get_ok (Sop.Payload.geometry second.payload))))
    "context seed did not change implicit noise";
  let stats = Session.stats evaluator in
  check (stats.cooks = 3 && stats.hits = 1)
    "seed-dependent node did not reuse its static input";
  let third = cook_ok evaluator (context ~seed:11L ~domains:2 ~grain:3
      ~time:200. ~frame:55L ()) graph in
  let stats = Session.stats evaluator in
  check (stats.cooks = 3 && stats.hits = 3)
    "noise node depended on undeclared context facts";
  check ((Result.get_ok (Sop.Payload.geometry second.payload)) == (Result.get_ok (Sop.Payload.geometry third.payload))) "seed cache identity";
  Session.close evaluator

let test_labeled_random_identity () =
  let make () = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 8 :rows 8 :size 4.0)
     (sop/noise_displace :context_seed true :amplitude 0.5 :frequency 0.2))|} |> Node.relabel "terrain-noise" in
  let first_graph = make () in
  ignore (Array.init 32 (fun index ->
    Lisp_sop.snapshot (Rdk.Line_geometry.points [|(float_of_int index, 0., 0.)|])));
  let second_graph = make () in
  let cook graph =
    let evaluator = session () in
    let output = cook_ok evaluator (context ~seed:88L ()) graph in
    Session.close evaluator;
    (Result.get_ok (Sop.Payload.geometry output.payload)) in
  check (equal_positions (cook first_graph) (cook second_graph))
    "explicit noise label did not stabilize random identity"

let test_switch_is_lazy () =
  let bad = Lisp_sop.node ~with_:["in9", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.)|]))] {|(sop/attribute_remap (sop/ext_in9) :name "missing")|} in
  let good = Lisp_sop.snapshot (Rdk.Line_geometry.points [| (0., 0., 0.) |]) in
  let graph = Lisp_sop.node ~with_:["bad", (bad); "good", (good)] {|(sop/switch (sop/ext_bad) (sop/ext_good) :input 1)|} in
  let evaluator = session () in
  let output = cook_ok evaluator (context ()) graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 1) "switch selected wrong input";
  check ((Session.stats evaluator).cooks = 2) "switch eagerly cooked unselected input";
  Session.close evaluator

let test_shared_payload_accounting () =
  let source = Lisp_sop.node {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 30 :rows 20 :size 3.0)|} in
  let moved = (let migration_translation = Rays_math.Vec3.create 1. 0. 0. in
Lisp_sop.node ~with_:["source", (source)] (Printf.sprintf {|(sop/transform (sop/ext_source) :mode "Matrix" :m03 %s :m13 %s :m23 %s)|} ((Lisp_sop.float migration_translation.Rays_math.Vec3.x)) ((Lisp_sop.float migration_translation.Rays_math.Vec3.y)) ((Lisp_sop.float migration_translation.Rays_math.Vec3.z)))) in
  let evaluator = session () and current = context () in
  let source_output = cook_ok evaluator current source in
  let moved_output = cook_ok evaluator current moved in
  let naive = Rdk.Geometry.payload_bytes (Result.get_ok (Sop.Payload.geometry source_output.payload))
      + Rdk.Geometry.payload_bytes (Result.get_ok (Sop.Payload.geometry moved_output.payload)) in
  let retained = (Session.stats evaluator).retained_payload_bytes in
  check (retained < naive) "session double-counted structurally shared RDK buffers";
  Session.clear evaluator;
  check ((Session.stats evaluator).retained_payload_bytes = 0)
    "payload reference accounting survived Session.clear";
  Session.close evaluator

let test_error_trace_and_cancellation () =
  check (try ignore (Lisp_sop.node {|(sop/grid :columns 0 :rows 2)|}); false
      with Invalid_argument _ -> true) "Grid refuses invalid counts at construction";
  let remap = Lisp_sop.node ~with_:["in14", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.)|]))]
      {|(sop/attribute_remap (sop/ext_in14) :name "missing")|} |> Node.relabel "bad-attribute" in
  let graph = Lisp_sop.node ~with_:["remap", remap] {|(sop/null (sop/ext_remap))|} |> Node.relabel "output" in
  let evaluator = session () in
  (match Session.cook evaluator ~context:(context ()) graph with
   | Ok _ -> fail "missing attribute cooked"
   | Error error ->
       check (error.code = "invalid_remap") "missing attribute error code";
       check (List.map (fun trace -> trace.Diagnostic.label) error.trace
              = ["output"; "bad-attribute"])
         "node trace does not describe root-to-failure path";
       check (error.cause <> None) "RDK cause was not preserved");
  let cancel = Context.Cancel.create () in
  Context.Cancel.cancel cancel;
  let cancelled = Lisp_sop.snapshot (Rdk.Line_geometry.points [| (0., 0., 0.) |]) in
  (match Session.cook evaluator ~context:(context ~cancel ()) cancelled with
   | Ok _ -> fail "cancelled graph cooked"
   | Error error -> check (error.code = "cancelled") "cancellation error code");
  Session.close evaluator

let test_inspection_sharing_and_bridge () =
  let shared = Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Triangles")|} |> Node.relabel "prototype" in
  let graph = Lisp_sop.node ~with_:["shared", (shared)] {|(sop/merge (sop/ext_shared) (sop/ext_shared))|} in
  let infos = Node.inspect graph in
  check (List.length infos = 2) "graph inspection duplicated shared subgraph";
  check (Node.id shared > 0 && Node.label shared = "prototype") "node identity/label";
  let evaluator = session () in
  let output = cook_ok evaluator (context ()) graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 48
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 24)
    "real merge cardinality";
  let colored = Lisp_sop.node {|(-> (sop/box :normals "Auto" :connectivity "Triangles")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0))|} in
  let colored = cook_ok evaluator (context ()) colored in
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "Cd"
      (Result.get_ok (Sop.Payload.geometry colored.payload)) <> None) "color_by_height did not create Cd";
  (match Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry colored.payload)) with
   | Error error -> fail (Rdk.Error.to_string error)
   | Ok mesh -> check (Mesh.index_count mesh = 36) "mesh bridge index count");
  Session.close evaluator

let test_snapshot_feedback_boundary () =
  let evaluator = session ~entries:4 () and current = context () in
  let initial = cook_ok evaluator current (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(1., 0., 0.)|])) in
  let previous = Lisp_sop.snapshot ((Result.get_ok (Sop.Payload.geometry initial.payload))) in
  let step = custom ~label:"solver-step" ~operation:"translate_step"
      ~version:1 ~parameters:"dx=0.25" [previous]
      (fun ~context:_ inputs ->
        match inputs with
        | [|geometry|] -> Ok (Rdk.Transform_ops.transform
            (Rays_math.Mat4.translation (Rays_math.Vec3.create 0.25 0. 0.)) geometry)
        | _ -> Error "solver step requires one snapshot") in
  let output = cook_ok evaluator current step in
  let x, _, _ = Rdk.Packed.Float3.get
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) 0 in
  check (x = 1.25) "snapshot feedback source/custom step";
  let source = List.hd (Node.inputs step) in
  check (Node.operation source = "snapshot"
      && contains (readable source)
           (string_of_int (Rdk.Geometry.data_id (Result.get_ok (Sop.Payload.geometry initial.payload)))))
    "snapshot source inspection identity";
  Session.close evaluator

let test_parallel_geometry_exactness () =
  let graph = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 80 :rows 70 :size 10.0)
     (sop/noise_displace :seed 42 :amplitude 0.6 :frequency 0.2)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 0 :high_green 255 :high_blue 255))|} in
  let cook domains =
    let evaluator = session () in
    let output = cook_ok evaluator (context ~domains ~grain:97 ()) graph in
    Session.close evaluator;
    (Result.get_ok (Sop.Payload.geometry output.payload))
  in
  let one = cook 1 and many = cook 4 in
  check (equal_positions one many) "one/multi-domain positions differ";
  let colors geometry =
    match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "Cd" geometry with
    | None -> fail "missing Cd"
    | Some attribute -> Rdk.Attribute.storage attribute
  in
  (match colors one, colors many with
   | Rdk.Attribute.Float4 left, Rdk.Attribute.Float4 right ->
       let left = Rdk.Packed.Float4.View.view left
       and right = Rdk.Packed.Float4.View.view right in
       check (left.x = right.x && left.y = right.y && left.z = right.z
           && left.w = right.w) "one/multi-domain colors differ"
   | _ -> fail "Cd has unexpected storage")

let test_poly_fill_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let graph = Lisp_sop.node {|(-> (sop/tube
       :normals_mode "Auto"
       :uv_attribute ""
       :cap_group ""
       :consolidate_cap_points true
       :connectivity "Quads"
       :end_caps false
       :rows 4
       :columns 8
       :top_radius 0.7)
     (sop/poly_fill :mode "Triangle fan" :unique_points true :patch_group "patch"))|} in
  let first = cook_ok evaluator current graph in
  let patch = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "patch"
      (Result.get_ok (Sop.Payload.geometry first.payload)) |> Option.get in
  check (Rdk.Group.cardinality patch = 16)
    "procedural Poly Fill patch cardinality";
  let topology_index = Rdk.Topology_index.create
      (Rdk.Geometry.topology (Result.get_ok (Sop.Payload.geometry first.payload))) in
  check (Rdk.Topology_index.boundary_edge_count topology_index = 32)
    "procedural unique Poly Fill did not detach its two patches";
  let before = Session.stats evaluator in
  ignore (cook_ok evaluator current graph);
  let after = Session.stats evaluator in
  check (after.hits > before.hits) "procedural Poly Fill did not cache";
  let missing = Lisp_sop.node {|(-> (sop/tube
       :normals_mode "Auto"
       :uv_attribute ""
       :cap_group ""
       :consolidate_cap_points true
       :connectivity "Quads"
       :end_caps false
       :top_radius 0.7)
     (sop/poly_fill :boundary_group "missing"))|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Poly Fill missing-group diagnostic"
   | Ok _ -> fail "procedural Poly Fill accepted a missing boundary group");
  Session.close evaluator

let test_poly_path_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:3 () in
  let positions = Rdk.Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.;2.;10.;11.;10.5|]
      ~y:[|0.;0.;0.;1.;0.;0.;1.|] ~z:(Array.make 7 0.) in
  let topology = Rdk.Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1;2; 2;3; 4;5;6|]
      ~primitive_offsets:[|0;3;5;8|]
      ~primitive_kinds:[|Rdk.Topology.Open_polyline;
        Rdk.Topology.Open_polyline; Rdk.Topology.Polygon|]
      |> Result.get_ok in
  let source = Rdk.Geometry.create ~positions ~topology () |> Result.get_ok in
  let graph = Lisp_sop.node ~with_:["source", (Lisp_sop.snapshot (source))] {|(sop/poly_path (sop/ext_source) :make_isolated_loops_closed true)|} in
  check (Node.operation graph = "poly_path"
      && contains (readable graph) "make_isolated_loops_closed=true")
    "procedural PolyPath operation/cache identity";
  let first = cook_ok evaluator current graph in
  check (Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry first.payload)) = 2
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry first.payload)) = 7
      && Rdk.Topology.primitive_kind (Rdk.Geometry.topology (Result.get_ok (Sop.Payload.geometry first.payload))) 1
         = Rdk.Topology.Polygon)
    "procedural PolyPath topology";
  let before = Session.stats evaluator in
  ignore (cook_ok evaluator current graph);
  let after = Session.stats evaluator in
  check (after.hits > before.hits) "procedural PolyPath did not cache";
  check (match (Lisp_sop.node ~with_:["source", (Lisp_sop.snapshot (source))] (Printf.sprintf {|(sop/poly_path (sop/ext_source) :maximum_distance %s)|} ((Lisp_sop.float Float.nan)))) with
    | _ -> false | exception Invalid_argument message -> contains message "E_NONFINITE")
    "procedural PolyPath refuses a non-finite distance at construction";
  Session.close evaluator

let test_revolve_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let profile = Lisp_sop.node {|(sop/curve (list [0.0 -1.0 0.0] [1.0 0.0 0.0] [0.0 1.0 0.0]))|} in
  let make () = Lisp_sop.node ~with_:["profile", (profile)] (Printf.sprintf {|(sop/revolve
   (sop/ext_profile)
   :connectivity "Alternating triangles"
   :caps true
   :uv_attribute "st"
   :divisions 16
   :origin %s
   :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero)) ((Lisp_sop.vec3 Rays_math.Vec3.unit_y))) in
  let graph = make () in
  check (Node.operation graph = "revolve"
      && contains (readable graph) "type=closed"
      && contains (readable graph) "connectivity=alternating_triangles"
      && contains (readable graph) "divisions=16")
    "procedural Revolve cache identity";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 18
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 32
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 96)
    "procedural Revolve pole topology";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "st"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Revolve dropped generated UVs";
  let caps = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "caps"
      (Result.get_ok (Sop.Payload.geometry output.payload)) |> Option.get in
  check (Rdk.Group.cardinality caps = 0)
    "procedural Revolve emitted degenerate pole caps";
  let before = Session.stats evaluator in
  ignore (cook_ok evaluator current graph);
  check ((Session.stats evaluator).hits > before.hits)
    "procedural Revolve did not cache";
  let missing = Lisp_sop.node ~with_:["profile", (profile)] (Printf.sprintf {|(sop/revolve
   (sop/ext_profile)
   :group "missing"
   :divisions 8
   :origin %s
   :axis %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.zero)) ((Lisp_sop.vec3 Rays_math.Vec3.unit_y))) in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Revolve missing-group diagnostic"
   | Ok _ -> fail "procedural Revolve accepted a missing group");
  Session.close evaluator

let test_sweep_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let backbone = Lisp_sop.node {|(sop/curve (list [0.0 0.0 0.0] [0.0 0.0 2.0]))|}
  (* Lisp gap: sop/curve is always open, so the closed section is an OCaml-built snapshot *)
  and cross_section = Lisp_sop.snapshot (Rdk.Line_geometry.polyline ~closed:true
      [|(-1.,-1.,0.); (1.,-1.,0.); (1.,1.,0.); (-1.,1.,0.)|] |> Result.get_ok) in
  let make () = Lisp_sop.node ~with_:["backbone", (backbone); "cross_section", (cross_section)] {|(sop/sweep
   (sop/ext_backbone)
   (sop/ext_cross_section)
   :connectivity "Alternating triangles"
   :tangent "Central difference"
   :twist 1.25
   :caps true
   :uv_attribute "st")|} in
  let graph = make () in
  check (Node.operation graph = "sweep"
      && contains (readable graph) "tangent=central_difference"
      && contains (readable graph) "connectivity=alternating_triangles"
      && contains (readable graph) "twist=")
    "procedural Sweep cache identity";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 8
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 10
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 32)
    "procedural Sweep topology";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "st"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Sweep dropped generated UVs";
  let caps = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "caps"
      (Result.get_ok (Sop.Payload.geometry output.payload)) |> Option.get in
  check (Rdk.Group.cardinality caps = 2) "procedural Sweep cap group";
  let before = Session.stats evaluator in
  ignore (cook_ok evaluator current graph);
  check ((Session.stats evaluator).hits > before.hits)
    "procedural Sweep did not cache";
  let missing = Lisp_sop.node ~with_:["backbone", (backbone); "cross_section", (cross_section)] {|(sop/sweep (sop/ext_backbone) (sop/ext_cross_section) :backbone_group "missing")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Sweep missing-group diagnostic"
   | Ok _ -> fail "procedural Sweep accepted a missing group");
  Session.close evaluator

let test_local_subdivide_contract () =
  let base = Rdk.Plane_generators.grid ~columns:3 ~rows:2 ~size:2. () |> Result.get_ok in
  let source_primitives = Rdk.Geometry.primitive_count base in
  let selected = Rdk.Group.ordered ~owner:Rdk.Group.Primitive ~name:"left"
      ~length:source_primitives [|0|] |> Result.get_ok in
  let source = match Rdk.Geometry.with_group selected base with
    | Ok source -> source
    | Error error -> fail error in
  let graph = Lisp_sop.node ~with_:["source", (Lisp_sop.snapshot (source))] {|(sop/subdivide
   (sop/ext_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :group "left"
   :scheme "Bilinear"
   :iterations 2)|} in
  check (Node.version graph = 1
      && contains (readable graph) "group=left"
      && contains (readable graph) "scheme=bilinear"
      && contains (readable graph) "boundary_interpolation=edge_only"
      && contains (readable graph) "face_varying_interpolation=all"
      && contains (readable graph) "triangle_policy=catmull_clark"
      && contains (readable graph) "creasing_method=uniform"
      && contains (readable graph) "recompute_point_normals=false")
    "procedural local Subdivide cache identity";
  let evaluator = session () and current = context ~domains:4 ~grain:1 () in
  let output = cook_ok evaluator current graph in
  let refined_primitives =
    Rdk.Topology.primitive_size (Rdk.Geometry.topology base) 0 * 4 in
  let output_primitives = Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload))
  and selected_primitives = Rdk.Group.cardinality
      (Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "left"
        (Result.get_ok (Sop.Payload.geometry output.payload)) |> Option.get) in
  check (output_primitives = source_primitives - 1 + refined_primitives
      && selected_primitives = refined_primitives)
    (Printf.sprintf
      "procedural local Subdivide topology/group ancestry (%d/%d from %d)"
      output_primitives selected_primitives source_primitives);
  let consistent_graph = Lisp_sop.node ~with_:["source", (Lisp_sop.snapshot (source))] {|(sop/subdivide
   (sop/ext_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :group "left"
   :consistent_topology true
   :cracks "Stitch, divide edges")|} in
  check (Node.version consistent_graph = 1
      && contains (readable consistent_graph) "consistent_topology=true" && contains (readable consistent_graph) "cracks=stitch__divide_edges")
    "procedural consistent Subdivide cache identity";
  let consistent = cook_ok evaluator current consistent_graph in
  check (Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry consistent.payload))
      > Rdk.Geometry.primitive_count source)
    "procedural consistent Stitch Divide appended no bridge primitives";
  let crease_input = Lisp_sop.node {|(-> (sop/curve (list [99.0 4.0 7.0] [-20.0 8.0 3.0]))
     (sop/group_range :owner "Primitives" :name "crease_edges" :range_mode "From ends"))|} in
  let crease_graph = Lisp_sop.node ~with_:["source", (Lisp_sop.snapshot (source)); "crease_input", (crease_input)] {|(sop/subdivide
   (sop/ext_source)
   (sop/ext_crease_input)
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :crease_group "crease_edges"
   :crease_weight 2.5
   :resulting_crease_group "remaining_creases")|} in
  check (Node.version crease_graph = 1
      && List.length (Node.inputs crease_graph) = 2
      && contains (readable crease_graph) "creases=true"
      && contains (readable crease_graph) "crease_group=crease_edges"
      && contains (readable crease_graph) "generate_resulting_creases=true")
    "procedural second-input Subdivide cache identity";
  let creased = cook_ok evaluator current crease_graph in
  check (match Rdk.Geometry.find_edge_group "remaining_creases" (Result.get_ok (Sop.Payload.geometry creased.payload)) with
    | Some group -> Rdk.Edge_group.cardinality group > 0 | None -> false)
    "procedural second-input Subdivide omitted resulting crease group";
  (* Explicit NaN literals are refused by the parameter's finite-value check. *)
  check (try ignore (Lisp_sop.node ~with_:["base", (Lisp_sop.snapshot (base))] (Printf.sprintf {|(sop/subdivide (sop/ext_base) :hole_group "" :crease_weight %s)|} ((Lisp_sop.float Float.nan))));
      false with Invalid_argument message -> contains message "E_NONFINITE")
    "procedural Subdivide accepted non-finite all-edge sharpness at construction";
  let triangle_source = Rdk.Plane_generators.grid ~connectivity:Rdk.Plane_generators.Grid_triangles
      ~columns:3 ~rows:2 ~size:3. () |> Result.get_ok in
  let add_detail name storage geometry =
    let attribute = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Detail
        ~name storage |> Result.get_ok in
    Rdk.Geometry.with_attribute attribute geometry |> Result.get_ok in
  let detail_source = triangle_source
      |> add_detail "osd_scheme" (Rdk.Attribute.Int [|0|])
      |> add_detail "osd_vtxboundaryinterpolation" (Rdk.Attribute.Int [|2|])
      |> add_detail "osd_fvarlinearinterpolation" (Rdk.Attribute.Int [|0|])
      |> add_detail "osd_creasingmethod" (Rdk.Attribute.Int [|1|])
      |> add_detail "osd_trianglesubdiv" (Rdk.Attribute.Int [|1|]) in
  let detail_graph = Lisp_sop.node ~with_:["detail_source", (Lisp_sop.snapshot (detail_source))] {|(sop/subdivide
   (sop/ext_detail_source)
   :crease_weight_mode "Auto"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :scheme "Bilinear"
   :boundary_interpolation "None"
   :face_varying_interpolation "All")|} in
  check (Node.version detail_graph = 1
      && contains (readable detail_graph) "scheme=bilinear"
      && contains (readable detail_graph) "triangle_policy=catmull_clark")
    "procedural detail-override Subdivide cache identity";
  let detail_output = cook_ok evaluator current detail_graph in
  let expected_detail = Rdk.Subdivide.subdivide ~iterations:2 ~remove_holes:true
      ~generate_resulting_creases:true
      ~boundary_interpolation:Rdk.Subdivide.Subdivide_boundary_edge_and_corner
      ~face_varying_interpolation:Rdk.Subdivide.Subdivide_fvar_none
      ~creasing_method:Rdk.Subdivide.Subdivide_creasing_chaikin
      ~triangle_policy:Rdk.Subdivide.Subdivide_triangles_smooth triangle_source
      |> Result.get_ok in
  check (equal_positions (Result.get_ok (Sop.Payload.geometry detail_output.payload)) expected_detail)
    "procedural Subdivide did not honor input detail overrides";
  check (List.for_all (fun name ->
      Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Detail name
        (Result.get_ok (Sop.Payload.geometry detail_output.payload)) <> None)
      ["osd_scheme"; "osd_vtxboundaryinterpolation";
       "osd_fvarlinearinterpolation"; "osd_creasingmethod";
       "osd_trianglesubdiv"])
    "procedural Subdivide dropped OpenSubdiv detail controls";
  let before_detail_hit = Session.stats evaluator in
  ignore (cook_ok evaluator current detail_graph);
  let after_detail_hit = Session.stats evaluator in
  check (after_detail_hit.hits > before_detail_hit.hits)
    "procedural detail-override Subdivide did not cache";
  let invalid_detail_source = triangle_source
      |> add_detail "osd_scheme" (Rdk.Attribute.Text [|"none"|]) in
  let invalid_detail = Lisp_sop.node ~with_:["invalid_detail_source", (Lisp_sop.snapshot (invalid_detail_source))] {|(sop/subdivide
   (sop/ext_invalid_detail_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group "")|} in
  (match Session.cook evaluator ~context:current invalid_detail with
   | Ok _ -> fail "procedural Subdivide accepted unsupported osd_scheme"
   | Error error -> check (error.code = "invalid_topology"
       && (match error.cause with
           | Some cause -> contains cause "osd_scheme"
           | None -> false))
       "procedural Subdivide osd_scheme diagnostic");
  let curve_positions = Rdk.Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;4.|] ~y:[|0.;2.;0.|] ~z:[|0.;0.;0.|] in
  let curve_topology = Rdk.Topology.create_owned ~point_count:3
      ~vertex_points:[|0;1; 1;2|] ~primitive_offsets:[|0;2;4|]
      ~primitive_kinds:[|Rdk.Topology.Open_polyline;
        Rdk.Topology.Open_polyline|] |> Result.get_ok in
  let curve_n = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"N"
      (Rdk.Attribute.Float3 (Rdk.Packed.Float3.View.of_owned_exn
        ~x:[|0.;0.;0.|] ~y:[|0.;0.;0.|] ~z:[|1.;1.;1.|]))
      |> Result.get_ok in
  let curve_source = Rdk.Geometry.create ~positions:curve_positions
      ~topology:curve_topology ~attributes:[curve_n] () |> Result.get_ok in
  let independent_curve_graph = Lisp_sop.node ~with_:["curve_source", (Lisp_sop.snapshot (curve_source))] {|(sop/subdivide
   (sop/ext_curve_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :treat_curves_as_independent true
   :recompute_point_normals true)|} in
  check (Node.version independent_curve_graph = 1
      && contains (readable independent_curve_graph)
           "treat_curves_as_independent=true"
      && contains (readable independent_curve_graph)
           "recompute_point_normals=true")
    "procedural polygon-curve Subdivide cache identity";
  let independent_curves = cook_ok evaluator current independent_curve_graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry independent_curves.payload)) = 6)
    "procedural independent curve subdivision did not split shared points";
  let recomputed_n = Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "N"
      (Result.get_ok (Sop.Payload.geometry independent_curves.payload)) |> Option.get |> Rdk.Attribute.storage in
  check (match recomputed_n with
    | Rdk.Attribute.Float3 values ->
        let values = Rdk.Packed.Float3.View.view values in
        Array.for_all (( = ) 0.) values.x
        && Array.for_all (( = ) 0.) values.y
        && Array.for_all (( = ) 0.) values.z
    | _ -> false)
    "procedural Subdivide did not forward normal recomputation";
  let before_curve_hit = Session.stats evaluator in
  ignore (cook_ok evaluator current independent_curve_graph);
  let after_curve_hit = Session.stats evaluator in
  check (after_curve_hit.hits > before_curve_hit.hits)
    "procedural polygon-curve Subdivide did not cache";
  let missing_hole = Lisp_sop.node ~with_:["base", (Lisp_sop.snapshot (base))] {|(sop/subdivide
   (sop/ext_base)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group "missing")|} in
  (match Session.cook evaluator ~context:current missing_hole with
   | Error error -> check (error.code = "missing_group")
       "procedural Subdivide missing hole-group diagnostic"
   | Ok _ -> fail "procedural Subdivide accepted a missing hole group");
  Session.close evaluator

let test_edge_divide_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.node {|(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 8
       :rows 6
       :size 4.0)
     (sop/set_float :name "weight" :value 0.75)
     (sop/group_edges :name "all_edges"))|} in
  let source_output = cook_ok evaluator current source in
  let shared_graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_divide (sop/ext_source) :group "all_edges" :divisions 3)|}
  and unique_graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_divide (sop/ext_source) :group "all_edges" :divisions 3 :share_points false)|} in
  check (Node.version shared_graph = 1
      && contains (readable shared_graph) "group=all_edges"
      && contains (readable shared_graph) "divisions=3"
      && contains (readable shared_graph) "share_points=true")
    "procedural Edge Divide cache identity";
  let shared = cook_ok evaluator current shared_graph
  and unique = cook_ok evaluator current unique_graph in
  check (Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry shared.payload))
         = Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry source_output.payload))
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry shared.payload))
         = Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry unique.payload))
      && Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry unique.payload))
         > Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry shared.payload)))
    "procedural Edge Divide shared/unique topology";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "weight"
      (Result.get_ok (Sop.Payload.geometry shared.payload)) <> None)
    "procedural Edge Divide dropped point payload";
  let no_group = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_divide (sop/ext_source) :divisions 4)|}
      |> cook_ok evaluator current in
  check ((Result.get_ok (Sop.Payload.geometry no_group.payload)) == (Result.get_ok (Sop.Payload.geometry source_output.payload)))
    "procedural Edge Divide empty group was not an identity";
  let missing = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_divide (sop/ext_source) :group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Divide missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Divide accepted a missing group");
  check (try ignore (Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_divide (sop/ext_source) :divisions 0)|}); false
    with Invalid_argument _ -> true)
    "procedural Edge Divide accepted zero divisions";
  Session.close evaluator

let test_edge_collapse_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  (* Lisp gap: sop/curve is always open, so the closed ring is an OCaml-built snapshot *)
  let source = Lisp_sop.node ~with_:["ring", Lisp_sop.snapshot (Rdk.Line_geometry.polyline ~closed:true
      [|(0.,0.,0.); (1.,0.,0.); (3.,2.,0.); (0.,2.,0.)|] |> Result.get_ok)] {|(-> (sop/ext_ring)
     (sop/set_int :name "piece" :value 3)
     (sop/group_edges
       :use_max_length true
       :use_min_length true
       :name "short_edge"
       :min_length 1.0))|} in
  let source_output = cook_ok evaluator current source in
  let graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_collapse (sop/ext_source) :group "short_edge" :connectivity_attribute "piece")|} in
  check (Node.version graph = 1
      && contains (readable graph) "group=short_edge"
      && contains (readable graph) "connectivity_attribute=piece"
      && contains (readable graph) "remove_degenerate_primitives=true"
      && contains (readable graph) "recompute_point_normals=true")
    "procedural Edge Collapse cache identity";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 3
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 3
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 1
      && Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload))
         < Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry source_output.payload)))
    "procedural Edge Collapse topology";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "piece"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Edge Collapse dropped point payload";
  let whole = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_collapse (sop/ext_source))|} |> cook_ok evaluator current in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry whole.payload)) = 0
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry whole.payload)) = 0)
    "procedural Edge Collapse omitted group did not select all edges";
  let missing = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_collapse (sop/ext_source) :group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Collapse missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Collapse accepted a missing group");
  let missing_attribute = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_collapse (sop/ext_source) :group "short_edge" :connectivity_attribute "absent")|} in
  (match Session.cook evaluator ~context:current missing_attribute with
   | Error error -> check (error.code = "invalid_topology")
       "procedural Edge Collapse missing-attribute diagnostic"
   | Ok _ -> fail "procedural Edge Collapse accepted a missing attribute");
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_collapse (sop/ext_source) :group " ")|}))
    "procedural Edge Collapse: Lisp accepts a blank group (the default, meaning none)";
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_collapse (sop/ext_source) :connectivity_attribute " ")|}))
    "procedural Edge Collapse: Lisp accepts a blank connectivity attribute (the default, meaning none)";
  Session.close evaluator

let test_edge_flip_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 1 :rows 1 :size 2.0)
     (sop/set_float :owner "Vertex" :name "uv_marker" :value 0.5)
     (sop/group_edges :name "interior" :incidence "Manifold"))|} in
  let source_output = cook_ok evaluator current source in
  let graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_flip (sop/ext_source) :group "interior")|} in
  check (Node.version graph = 1
      && contains (readable graph) "group=interior"
      && contains (readable graph) "cycles=1"
      && contains (readable graph) "cycle_vertex_attributes=true"
      && contains (readable graph) "recompute_point_normals=false")
    "procedural Edge Flip cache identity";
  let output = cook_ok evaluator current graph in
  let source_topology = Rdk.Topology.View.view
      (Rdk.Geometry.topology (Result.get_ok (Sop.Payload.geometry source_output.payload)))
  and output_topology = Rdk.Topology.View.view
      (Rdk.Geometry.topology (Result.get_ok (Sop.Payload.geometry output.payload))) in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 4
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 6
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload)) = 2
      && source_topology.vertex_points <> output_topology.vertex_points)
    "procedural Edge Flip topology";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "uv_marker"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_edge_group "interior" (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Edge Flip payload ancestry";
  let no_group = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_flip (sop/ext_source))|} |> cook_ok evaluator current in
  check ((Result.get_ok (Sop.Payload.geometry no_group.payload)) == (Result.get_ok (Sop.Payload.geometry source_output.payload)))
    "procedural Edge Flip omitted group was not an identity";
  let missing = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_flip (sop/ext_source) :group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Flip missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Flip accepted a missing group");
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_flip (sop/ext_source) :group " ")|}))
    "procedural Edge Flip: Lisp accepts a blank group (the default, meaning none)";
  check (try ignore (Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_flip (sop/ext_source) :cycles -1)|}); false
    with Invalid_argument _ -> true)
    "procedural Edge Flip accepted negative cycles";
  Session.close evaluator

let test_edge_cusp_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 2 :size 2.0)
     (sop/set_int :name "source_id" :value 7)
     (sop/group_edges :name "cusp_path"))|} in
  let source_output = cook_ok evaluator current source in
  let graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_cusp (sop/ext_source) :group "cusp_path")|} in
  check (Node.version graph = 1
      && contains (readable graph) "group=cusp_path"
      && contains (readable graph) "update_point_normals=true")
    "procedural Edge Cusp cache identity";
  let output = cook_ok evaluator current graph in
  check (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload))
         > Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry source_output.payload))
      && Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry output.payload))
         = Rdk.Geometry.vertex_count (Result.get_ok (Sop.Payload.geometry source_output.payload))
      && Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry output.payload))
         = Rdk.Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry source_output.payload)))
    "procedural Edge Cusp topology";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "source_id"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && Rdk.Geometry.find_edge_group "cusp_path" (Result.get_ok (Sop.Payload.geometry output.payload)) <> None)
    "procedural Edge Cusp payload ancestry";
  let no_group = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_cusp (sop/ext_source))|} |> cook_ok evaluator current in
  check ((Result.get_ok (Sop.Payload.geometry no_group.payload)) == (Result.get_ok (Sop.Payload.geometry source_output.payload)))
    "procedural Edge Cusp omitted group was not an identity";
  let missing = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_cusp (sop/ext_source) :group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Cusp missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Cusp accepted a missing group");
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_cusp (sop/ext_source) :group " ")|}))
    "procedural Edge Cusp: Lisp accepts a blank group (the default, meaning none)";
  Session.close evaluator

let test_edge_straighten_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.node {|(-> (sop/curve (list [-1.0 0.0 0.0] [-0.5 0.8 0.0] [0.0 1.1 0.0] [0.5 0.7 0.0] [1.0 0.0 0.0]))
     (sop/set_int :name "source_id" :value 9)
     (sop/group_edges :name "bend_edges"))|} in
  let graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_straighten (sop/ext_source) :group "bend_edges" :output_group "straightened")|} in
  check (Node.version graph = 1
      && contains (readable graph) "group=bend_edges"
      && contains (readable graph) "output_group=straightened")
    "procedural Edge Straighten cache identity";
  let output = cook_ok evaluator current graph in
  let positions = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
  let last = Array.length positions.x - 1 in
  let dx = positions.x.(last) -. positions.x.(0)
  and dy = positions.y.(last) -. positions.y.(0)
  and dz = positions.z.(last) -. positions.z.(0) in
  check (Array.for_all Fun.id (Array.init (last + 1) (fun point ->
      let px = positions.x.(point) -. positions.x.(0)
      and py = positions.y.(point) -. positions.y.(0)
      and pz = positions.z.(point) -. positions.z.(0) in
      abs_float ((py *. dz) -. (pz *. dy)) < 1e-12
      && abs_float ((pz *. dx) -. (px *. dz)) < 1e-12
      && abs_float ((px *. dy) -. (py *. dx)) < 1e-12)))
    "procedural Edge Straighten did not produce a line";
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "source_id"
      (Result.get_ok (Sop.Payload.geometry output.payload)) <> None
      && (Rdk.Geometry.find_edge_group "straightened" (Result.get_ok (Sop.Payload.geometry output.payload))
          |> Option.get |> Rdk.Edge_group.cardinality) = 4)
    "procedural Edge Straighten payload/output group";
  let all = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_straighten (sop/ext_source))|} |> cook_ok evaluator current in
  let all_positions = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry all.payload))) in
  check (all_positions.x = positions.x && all_positions.y = positions.y
      && all_positions.z = positions.z)
    "procedural Edge Straighten omitted group did not use all edges";
  let missing = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_straighten (sop/ext_source) :group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Straighten missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Straighten accepted a missing group");
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_straighten (sop/ext_source) :group " ")|}))
    "procedural Edge Straighten: Lisp accepts a blank group (the default, meaning none)";
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_straighten (sop/ext_source) :output_group " ")|}))
    "procedural Edge Straighten: Lisp accepts a blank output group (the default, meaning none)";
  Session.close evaluator

let test_edge_equalize_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [4.0 0.0 0.0] [6.0 0.0 0.0]))
     (sop/group_edges :name "uneven"))|} in
  let graph = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_equalize
   (sop/ext_source)
   :group "uneven"
   :iterations 80
   :tolerance 1e-07
   :output_group "equalized")|} in
  check (Node.version graph = 1
      && contains (readable graph) "group=uneven"
      && contains (readable graph) "method_=average"
      && contains (readable graph) "iterations=80"
      && contains (readable graph) "output_group=equalized")
    "procedural Edge Equalize cache identity";
  let output = cook_ok evaluator current graph in
  let p = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
  check (abs_float ((p.x.(1) -. p.x.(0)) -. 2.) < 1e-6
      && abs_float ((p.x.(2) -. p.x.(1)) -. 2.) < 1e-6
      && abs_float ((p.x.(3) -. p.x.(2)) -. 2.) < 1e-6)
    "procedural Edge Equalize lengths";
  check ((Rdk.Geometry.find_edge_group "equalized" (Result.get_ok (Sop.Payload.geometry output.payload))
          |> Option.get |> Rdk.Edge_group.cardinality) = 3)
    "procedural Edge Equalize output group";
  let cached = cook_ok evaluator current graph in
  check ((Result.get_ok (Sop.Payload.geometry cached.payload)) == (Result.get_ok (Sop.Payload.geometry output.payload)))
    "procedural Edge Equalize static cook was not cached";
  let missing = Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_equalize (sop/ext_source) :group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Equalize missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Equalize accepted a missing group");
  check (try ignore (Lisp_sop.node ~with_:["source", (source)] {|(sop/edge_equalize (sop/ext_source) :iterations 0)|}); false
    with Invalid_argument _ -> true)
    "procedural Edge Equalize accepted zero iterations";
  check (try ignore (Lisp_sop.node ~with_:["source", (source)] (Printf.sprintf {|(sop/edge_equalize (sop/ext_source) :tolerance %s)|} ((Lisp_sop.float nan)))); false
    with Invalid_argument message -> contains message "E_NONFINITE")
    "procedural Edge Equalize accepted a non-finite tolerance";
  check (Result.is_ok (Lisp_sop.node_result ~with_:["source", (source)] {|(sop/edge_equalize (sop/ext_source) :group " ")|}))
    "procedural Edge Equalize: Lisp accepts a blank group (the default, meaning none)";
  Session.close evaluator

let test_edge_relax_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.node {|(sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [3.0 0.0 0.0] [6.0 0.0 0.0]))|}
  and reference = Lisp_sop.node {|(sop/curve (list [0.0 0.0 0.0] [2.0 0.0 0.0] [3.0 0.0 0.0] [7.0 0.0 0.0]))|} in
  let graph = Lisp_sop.node ~with_:["source", (source); "reference", (reference)] {|(sop/edge_relax (sop/ext_source) (sop/ext_reference) :iterations 128 :tolerance 1e-07)|} in
  check (Node.version graph = 1
      && contains (readable graph) "iterations=128"
      && contains (readable graph) "target_mode=individual_lengths")
    "procedural Edge Relax cache identity";
  let output = cook_ok evaluator current graph in
  let p = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
  check (abs_float ((p.x.(1) -. p.x.(0)) -. 2.) < 1e-6
      && abs_float ((p.x.(2) -. p.x.(1)) -. 1.) < 1e-6
      && abs_float ((p.x.(3) -. p.x.(2)) -. 4.) < 1e-6)
    (Printf.sprintf "procedural Edge Relax reference lengths: %.9g %.9g %.9g"
      (p.x.(1) -. p.x.(0)) (p.x.(2) -. p.x.(1))
      (p.x.(3) -. p.x.(2)));
  let cached = cook_ok evaluator current graph in
  check ((Result.get_ok (Sop.Payload.geometry cached.payload)) == (Result.get_ok (Sop.Payload.geometry output.payload)))
    "procedural Edge Relax static cook was not cached";
  let missing = Lisp_sop.node ~with_:["source", (source); "reference", (reference)] {|(sop/edge_relax (sop/ext_source) (sop/ext_reference) :iterations 20 :pin_group "absent")|} in
  (match Session.cook evaluator ~context:current missing with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Relax missing-group diagnostic"
   | Ok _ -> fail "procedural Edge Relax accepted a missing group");
  check (try ignore (Lisp_sop.node ~with_:["source", (source); "reference", (reference)] {|(sop/edge_relax
   (sop/ext_source)
   (sop/ext_reference)
   :iterations 20
   :group_owner "Vertex"
   :group "v")|}); false with Invalid_argument _ -> true)
    "procedural Edge Relax accepted a vertex group";
  check (try ignore (Lisp_sop.node ~with_:["source", (source); "reference", (reference)] {|(sop/edge_relax (sop/ext_source) (sop/ext_reference) :iterations 20 :step_size 0.0)|}); false
    with Invalid_argument _ -> true)
    "procedural Edge Relax accepted zero step size";
  Session.close evaluator

let test_blend_shapes_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let source = Lisp_sop.snapshot (Rdk.Line_geometry.points [|0.,0.,0.;1.,0.,0.;2.,0.,0.|])
  and first = Lisp_sop.snapshot (Rdk.Line_geometry.points [|10.,0.,0.;11.,0.,0.;12.,0.,0.|])
  and second = Lisp_sop.snapshot (Rdk.Line_geometry.points [|20.,0.,0.;21.,0.,0.;22.,0.,0.|]) in
  let graph = Lisp_sop.node ~with_:["source", source; "first", first; "second", second]
      {|(sop/blend_shapes (sop/ext_source) (sop/ext_first) (sop/ext_second)
          :mask_attribute "" :mask_source "Shape" :mode "Differencing" :attributes "^*"
          :weight1 1.5 :weight2 -0.5)|} in
  check (Node.version graph = 1
      && contains (readable graph) "mode=differencing"
      && contains (readable graph) "weight1="
      && contains (readable graph) ";weight2="
      && contains (readable graph) "attributes=^*")
    "procedural Blend Shapes cache identity";
  let cooked = cook_ok evaluator current graph in
  let position = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry cooked.payload))) in
  check (position.x = [|5.;6.;7.|])
    "procedural Blend Shapes differencing result";
  let cached = cook_ok evaluator current graph in
  check ((Result.get_ok (Sop.Payload.geometry cached.payload)) == (Result.get_ok (Sop.Payload.geometry cooked.payload)))
    "procedural Blend Shapes static cook was not cached";
  (* Lisp contract: with no shape the node keeps its own identity and the geometry is unchanged *)
  check ((Result.get_ok (Sop.Payload.geometry (cook_ok evaluator current
        (Lisp_sop.node ~with_:["source", source] {|(sop/blend_shapes (sop/ext_source))|})).payload))
      == (Result.get_ok (Sop.Payload.geometry (cook_ok evaluator current source).payload)))
    "procedural Blend Shapes empty shape list changed the geometry";
  let missing_group = Lisp_sop.node ~with_:["source", source; "first", first]
      {|(sop/blend_shapes (sop/ext_source) (sop/ext_first) :group "absent" :weight1 1.0
          :mask_attribute "" :mask_source "Shape")|} in
  (match Session.cook evaluator ~context:current missing_group with
   | Error error -> check (error.code = "missing_group")
       "procedural Blend Shapes missing-group diagnostic"
   | Ok _ -> fail "procedural Blend Shapes accepted a missing point group");
  check (match Lisp_sop.node_result ~with_:["source", source; "first", first]
                 (Printf.sprintf {|(sop/blend_shapes (sop/ext_source) (sop/ext_first) :weight1 %s)|} (Lisp_sop.float nan)) with
    | Error message -> contains message "E_NONFINITE"
    | Ok _ -> false)
    "procedural Blend Shapes accepted a non-finite weight";
  Session.close evaluator;

  let point_count = 100_000 in
  let make offset scale =
    let x = Array.init point_count (fun point ->
      offset +. scale *. float_of_int point *. 0.001) in
    Rdk.Geometry.create
      ~positions:(Rdk.Packed.Float3.View.of_owned_exn ~x
        ~y:(Array.make point_count 0.) ~z:(Array.make point_count 0.))
      ~topology:(Rdk.Topology.empty ~point_count) () |> get_ok in
  let source_geometry = make 0. 1. and target_geometry = make 1. 1.5 in
  let exact = Lisp_sop.node ~with_:["blend_source", Lisp_sop.snapshot source_geometry; "blend_target", Lisp_sop.snapshot target_geometry]
      {|(sop/blend_shapes (sop/ext_blend_source) (sop/ext_blend_target) :weight1 0.37
          :mask_attribute "" :mask_source "Shape")|} in
  let cook domains =
    let evaluator = session () in
    let result = cook_ok evaluator (context ~domains ~grain:127 ()) exact in
    Session.close evaluator;
    (Result.get_ok (Sop.Payload.geometry result.payload)) in
  let one = cook 1 and four = cook 4 in
  check (Rdk.Packed.Float3.View.view (Rdk.Geometry.positions one)
      = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions four))
    "procedural Blend Shapes differs across domain counts";
  check (Rdk.Geometry.topology one == Rdk.Geometry.topology source_geometry)
    "procedural Blend Shapes rebuilt topology"

let test_attribute_composite_contract () =
  let add_float name values geometry =
    Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name
      (Rdk.Attribute.Float values) |> Result.get_ok
    |> fun attribute -> Rdk.Geometry.with_attribute attribute geometry
      |> Result.get_ok in
  let make positions values alpha =
    Rdk.Line_geometry.points positions
    |> add_float "value" values
    |> add_float "alpha" alpha in
  let first_geometry = make [|0.,0.,0.;1.,0.,0.;2.,0.,0.|]
      [|2.;4.;6.|] [|1.;1.;0.|]
  and second_geometry = make [|10.,0.,0.;11.,0.,0.;12.,0.,0.|]
      [|10.;20.;30.|] [|1.;0.;1.|] in
  let first = Lisp_sop.snapshot (first_geometry) and second = Lisp_sop.snapshot (second_geometry) in
  let graph = Lisp_sop.node ~with_:["first", first; "second", second]
      {|(sop/attribute_composite (sop/ext_first) (sop/ext_second)
          :operation "Mean" :weight 1.0 :detail_attributes "^*" :primitive_attributes "^*"
          :point_attributes "P value" :vertex_attributes "^*" :allow_position true
          :alpha_attribute "alpha" :weight1 1.0)|} in
  check (Node.version graph = 1
      && contains (readable graph) "operation=mean"
      && contains (readable graph) "point_attributes=P value"
      && contains (readable graph) "allow_position=true"
      && contains (readable graph) "alpha_attribute=alpha"
      && contains (readable graph) ";weight1=")
    "procedural Attribute Composite cache identity";
  let evaluator = session () and current = context ~domains:4 ~grain:2 () in
  let output = cook_ok evaluator current graph in
  let positions = Rdk.Packed.Float3.View.view
      (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
  let values = match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point
      "value" (Result.get_ok (Sop.Payload.geometry output.payload)) with
    | Some attribute -> (match Rdk.Attribute.View.storage attribute with
        | Rdk.Attribute.Float values -> values
        | _ -> fail "procedural Attribute Composite changed scalar storage")
    | None -> fail "procedural Attribute Composite dropped value" in
  check (positions.x = [|5.;1.;12.|] && values = [|6.;4.;30.|])
    "procedural Attribute Composite result";
  let cached = cook_ok evaluator current graph in
  check ((Result.get_ok (Sop.Payload.geometry cached.payload)) == (Result.get_ok (Sop.Payload.geometry output.payload)))
    "procedural Attribute Composite static cook was not cached";
  let scaled = Lisp_sop.node ~with_:["first", first]
      {|(sop/attribute_composite (sop/ext_first) :operation "Maximum" :weight 2.0
          :point_attributes "value" :detail_attributes "^*" :primitive_attributes "^*" :vertex_attributes "^*")|}
    |> cook_ok evaluator current in
  let scaled_values = match Rdk.Geometry.find_attribute
      ~owner:Rdk.Attribute.Point "value" (Result.get_ok (Sop.Payload.geometry scaled.payload)) with
    | Some attribute -> (match Rdk.Attribute.View.storage attribute with
        | Rdk.Attribute.Float values -> values | _ -> assert false)
    | None -> assert false in
  check (scaled_values = [|4.;8.;12.|])
    "procedural Attribute Composite empty additional-input semantics";
  check (match Lisp_sop.node_result ~with_:["first", first; "second", second]
                 (Printf.sprintf {|(sop/attribute_composite (sop/ext_first) (sop/ext_second) :weight1 %s)|} (Lisp_sop.float nan)) with
    | Error message -> contains message "E_NONFINITE"
    | Ok _ -> false)
    "procedural Attribute Composite accepted a non-finite input weight";
  Session.close evaluator;

  let point_count = 100_000 in
  let make_large offset =
    let x = Array.init point_count (fun point ->
      offset +. float_of_int point *. 0.001) in
    Rdk.Geometry.create
      ~positions:(Rdk.Packed.Float3.View.of_owned_exn ~x
        ~y:(Array.make point_count 0.) ~z:(Array.make point_count 0.))
      ~topology:(Rdk.Topology.empty ~point_count) () |> Result.get_ok
    |> add_float "value" (Array.init point_count (fun point ->
      offset +. float_of_int (point mod 97))) in
  let large_first = make_large 0. and large_second = make_large 7. in
  let exact = Lisp_sop.node ~with_:["large_first", Lisp_sop.snapshot large_first; "large_second", Lisp_sop.snapshot large_second]
      {|(sop/attribute_composite (sop/ext_large_first) (sop/ext_large_second)
          :operation "Over" :weight 0.25 :point_attributes "P value" :allow_position true
          :detail_attributes "^*" :primitive_attributes "^*" :vertex_attributes "^*" :weight1 0.75)|} in
  let cook domains =
    let evaluator = session () in
    let output = cook_ok evaluator (context ~domains ~grain:127 ()) exact in
    Session.close evaluator;
    (Result.get_ok (Sop.Payload.geometry output.payload)) in
  let one = cook 1 and four = cook 4 in
  check (Rdk.Packed.Float3.View.view (Rdk.Geometry.positions one)
      = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions four))
    "procedural Attribute Composite positions differ across domain counts";
  let value geometry = match Rdk.Geometry.find_attribute
      ~owner:Rdk.Attribute.Point "value" geometry with
    | Some attribute -> (match Rdk.Attribute.View.storage attribute with
        | Rdk.Attribute.Float values -> values | _ -> assert false)
    | None -> assert false in
  check (value one = value four
      && Rdk.Geometry.topology one == Rdk.Geometry.topology large_first)
    "procedural Attribute Composite payload differs across domain counts"

let test_edge_transport_contract () =
  let evaluator = session () and current = context ~domains:4 ~grain:7 () in
  let float_values owner name geometry =
    match Rdk.Geometry.find_attribute ~owner name geometry with
    | Some attribute -> (match Rdk.Attribute.View.storage attribute with
        | Rdk.Attribute.Float values -> values
        | _ -> fail ("unexpected Edge Transport storage for " ^ name))
    | None -> fail ("missing Edge Transport attribute " ^ name) in
  let network = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [3.0 0.0 0.0]))
     (sop/group_range :name "tip" :start 2 :end_ 2))|} in
  let network_graph = Lisp_sop.node ~with_:["network", (network)] {|(sop/edge_transport
   (sop/ext_network)
   :root_value "Hold"
   :roots "Root group"
   :root_group "tip"
   :operation "Total"
   :integrate_constant true
   :scale_by_edge_length true
   :attribute "distance")|} in
  check (Node.version network_graph = 1
      && contains (readable network_graph) "roots=root_group"
      && contains (readable network_graph) "root_group=tip"
      && contains (readable network_graph) "scale_by_edge_length=true")
    "procedural Edge Transport network cache identity";
  let network_output = cook_ok evaluator current network_graph in
  check (float_values Rdk.Attribute.Point "distance" (Result.get_ok (Sop.Payload.geometry network_output.payload))
      = [|3.;2.;0.|]) "procedural Edge Transport rooted network distance";
  let cached = cook_ok evaluator current network_graph in
  check ((Result.get_ok (Sop.Payload.geometry cached.payload)) == (Result.get_ok (Sop.Payload.geometry network_output.payload)))
    "procedural Edge Transport static cook was not cached";
  let backward_network = Lisp_sop.node ~with_:["network", (network)] {|(sop/edge_transport
   (sop/ext_network)
   :root_value "Hold"
   :direction "Backward"
   :operation "Total"
   :integrate_constant true
   :merge "Maximum"
   :attribute "depth")|} in
  check (contains (readable backward_network) "direction=backward"
      && contains (readable backward_network) "merge=maximum")
    "procedural Edge Transport backward cache identity";
  let backward_output = cook_ok evaluator current backward_network in
  check (float_values Rdk.Attribute.Point "depth" (Result.get_ok (Sop.Payload.geometry backward_output.payload))
      = [|2.;1.;0.|]) "procedural Edge Transport backward network";
  let missing_root = Lisp_sop.node ~with_:["network", (network)] {|(sop/edge_transport
   (sop/ext_network)
   :root_value "Hold"
   :roots "Root group"
   :root_group "absent"
   :attribute "distance")|} in
  (match Session.cook evaluator ~context:current missing_root with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Transport missing-root diagnostic"
   | Ok _ -> fail "procedural Edge Transport accepted a missing root group");
  check (Result.is_ok (Lisp_sop.node_result ~with_:["network", (network)] {|(sop/edge_transport
   (sop/ext_network)
   :root_value "Hold"
   :roots "Root group"
   :root_group " "
   :attribute "distance")|}))
    "procedural Edge Transport: Lisp accepts a blank root group (the default, meaning none)";

  let curves = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [3.0 0.0 0.0]))
     (-> (sop/curve (list [0.0 2.0 0.0] [2.0 2.0 0.0] [5.0 2.0 0.0])) (sop/merge)))|} in
  let curve_graph = Lisp_sop.node ~with_:["curves", (curves)] {|(sop/edge_transport_curves
   (sop/ext_curves)
   :root_value "Hold"
   :operation "Total"
   :integrate_constant true
   :scale_by_edge_length true
   :normalization "Per component"
   :attribute "distance")|} in
  check (Node.version curve_graph = 1
      && contains (readable curve_graph) "owner=point"
      && contains (readable curve_graph) "direction=forward"
      && contains (readable curve_graph) "normalization=per_component")
    "procedural Edge Transport Each Curve cache identity";
  let curve_output = cook_ok evaluator current curve_graph in
  check (float_values Rdk.Attribute.Point "distance" (Result.get_ok (Sop.Payload.geometry curve_output.payload))
      = [|0.;1. /. 3.;1.;0.;0.4;1.|])
    "procedural Edge Transport Each Curve normalized distance";
  let restricted = Lisp_sop.node ~with_:["curves", (curves)] {|(-> (sop/group_range (sop/ext_curves) :owner "Primitives" :name "first_curve" :end_ 0)
     (sop/edge_transport_curves
       :root_value "Hold"
       :primitive_group "first_curve"
       :operation "Total"
       :integrate_constant true
       :attribute "depth"))|}
      |> cook_ok evaluator current in
  check (float_values Rdk.Attribute.Point "depth" (Result.get_ok (Sop.Payload.geometry restricted.payload))
      = [|0.;1.;2.;1.;1.;1.|])
    "procedural Edge Transport Each Curve primitive group";
  let missing_curve_group = Lisp_sop.node ~with_:["curves", (curves)] {|(sop/edge_transport_curves (sop/ext_curves) :root_value "Hold" :primitive_group "absent")|} in
  (match Session.cook evaluator ~context:current missing_curve_group with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Transport curve-group diagnostic"
   | Ok _ -> fail "procedural Edge Transport accepted a missing curve group");
  check (match Lisp_sop.node_result ~with_:["curves", curves]
                 {|(sop/edge_transport_curves (sop/ext_curves) :root_value "Hold" :owner "Primitive" :attribute "value")|} with
    | Error _ -> true
    | Ok node -> (match Session.cook evaluator ~context:current node with Error _ -> true | Ok _ -> false))
    "procedural Edge Transport accepted a primitive attribute";

  let parent_positions = Rdk.Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.;2.;1.;10.;14.|]
      ~y:[|0.;0.;2.;0.;3.;0.;0.|] ~z:(Array.make 7 0.) in
  let parent_attribute = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
      ~name:"parent" (Rdk.Attribute.Int [|0;0;0;1;1;5;5|]) |> get_ok in
  let parent_geometry = Rdk.Geometry.create ~positions:parent_positions
      ~topology:(Rdk.Topology.empty ~point_count:7)
      ~attributes:[parent_attribute] () |> get_ok in
  let parent_source = Lisp_sop.snapshot (parent_geometry) in
  let parent_graph = Lisp_sop.node ~with_:["parent_source", (parent_source)] {|(sop/edge_transport_parent
   (sop/ext_parent_source)
   :root_value "Hold"
   :operation "Total"
   :integrate_constant true
   :scale_by_edge_length true
   :attribute "distance")|} in
  check (Node.version parent_graph = 1
      && contains (readable parent_graph) "parent_attribute=parent"
      && contains (readable parent_graph) "direction=forward"
      && contains (readable parent_graph) "merge=add")
    "procedural Edge Transport Parent cache identity";
  let parent_output = cook_ok evaluator current parent_graph in
  check (float_values Rdk.Attribute.Point "distance" (Result.get_ok (Sop.Payload.geometry parent_output.payload))
      = [|0.;1.;2.;2.;4.;0.;4.|])
    "procedural Edge Transport Parent distance";
  let backward_parent = Lisp_sop.node ~with_:["parent_source", (parent_source)] {|(sop/edge_transport_parent
   (sop/ext_parent_source)
   :root_value "Hold"
   :direction "Backward"
   :operation "Total"
   :integrate_constant true
   :attribute "depth")|}
      |> cook_ok evaluator current in
  check (float_values Rdk.Attribute.Point "depth" (Result.get_ok (Sop.Payload.geometry backward_parent.payload))
      = [|4.;2.;0.;0.;0.;1.;0.|])
    "procedural Edge Transport Parent backward merge";
  let missing_parent = Lisp_sop.node ~with_:["in284", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|0.,0.,0.|]))] {|(sop/edge_transport_parent
   (sop/ext_in284)
   :root_value "Hold"
   :operation "Total"
   :integrate_constant true
   :attribute "depth")|} in
  (match Session.cook evaluator ~context:current missing_parent with
   | Error error -> check (error.code = "invalid_edge_transport")
       "procedural Edge Transport missing-parent diagnostic"
   | Ok _ -> fail "procedural Edge Transport accepted a missing parent field");
  let missing_parent_group = Lisp_sop.node ~with_:["parent_source", (parent_source)] {|(sop/edge_transport_parent
   (sop/ext_parent_source)
   :root_value "Hold"
   :point_group "absent")|} in
  (match Session.cook evaluator ~context:current missing_parent_group with
   | Error error -> check (error.code = "missing_group")
       "procedural Edge Transport parent-group diagnostic"
   | Ok _ -> fail "procedural Edge Transport accepted a missing parent group");
  check (match refusal ~with_:["parent_source", (parent_source)] {|(sop/edge_transport_parent
   (sop/ext_parent_source)
   :root_value "Hold"
   :parent_attribute " ")|} with
    | Some message -> contains message "empty parent attribute"
    | None -> false)
    "procedural Edge Transport accepted an empty parent attribute";
  Session.close evaluator;

  let curve_size = 10 and curve_count = 10_000 in
  let point_count = curve_size * curve_count in
  let x = Array.init point_count (fun point ->
      float_of_int (point mod curve_size) *. 0.01)
  and y = Array.init point_count (fun point ->
      float_of_int (point / curve_size) *. 0.001)
  and z = Array.make point_count 0. in
  let topology = Rdk.Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (curve_count + 1)
        (fun curve -> curve * curve_size))
      ~primitive_kinds:(Array.make curve_count Rdk.Topology.Open_polyline)
      |> get_ok in
  let geometry = Rdk.Geometry.create
      ~positions:(Rdk.Packed.Float3.View.of_owned_exn ~x ~y ~z)
      ~topology () |> get_ok in
  let exact_graph = Lisp_sop.node ~with_:["geometry", (Lisp_sop.snapshot (geometry))] {|(sop/edge_transport_curves
   (sop/ext_geometry)
   :root_value "Hold"
   :operation "Total"
   :integrate_constant true
   :scale_by_edge_length true
   :attribute "distance")|} in
  let cook graph domains =
    let evaluator = session () in
    let result = cook_ok evaluator (context ~domains ~grain:127 ()) graph in
    Session.close evaluator;
    (Result.get_ok (Sop.Payload.geometry result.payload)) in
  let one = cook exact_graph 1 and four = cook exact_graph 4 in
  check (float_values Rdk.Attribute.Point "distance" one
      = float_values Rdk.Attribute.Point "distance" four)
    "procedural Edge Transport Each Curve differs across domain counts";
  check (Rdk.Geometry.point_count one = point_count
      && Rdk.Geometry.primitive_count one = curve_count)
    "procedural Edge Transport Each Curve exact scale cardinality";
  let parents = Array.init point_count (fun point ->
      if point mod curve_size = 0 then point else point - 1) in
  let parent_attribute = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
      ~name:"parent" (Rdk.Attribute.Int parents) |> get_ok in
  let parent_geometry = Rdk.Geometry.with_attribute parent_attribute geometry
      |> get_ok in
  let parent_exact = Lisp_sop.node ~with_:["parent_geometry", (Lisp_sop.snapshot (parent_geometry))] {|(sop/edge_transport_parent
   (sop/ext_parent_geometry)
   :root_value "Hold"
   :operation "Total"
   :integrate_constant true
   :scale_by_edge_length true
   :attribute "distance")|} in
  let parent_one = cook parent_exact 1 and parent_four = cook parent_exact 4 in
  check (float_values Rdk.Attribute.Point "distance" parent_one
      = float_values Rdk.Attribute.Point "distance" parent_four)
    "procedural Edge Transport Parent differs across domain counts";
  check (Rdk.Geometry.topology parent_one == Rdk.Geometry.topology parent_geometry)
    "procedural Edge Transport Parent rebuilt topology"

(* A node reachable through many paths is evaluated once per cook. *)

(* Copy to Points with pack: the cook is the source once plus one transform
   per target; a consumer receives it materialized; recooking hits. *)
let test_packed_copy_contract () =
  let session = Session.create ~max_entries:16 ~max_payload_bytes:(1 lsl 24) |> Result.get_ok in
  let context = Context.create ~seed:1L ~domains:1 () |> Result.get_ok in
  let source = Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Triangles" :size [1.0 1.0 1.0])|} in
  let targets = Lisp_sop.snapshot (Rdk.Line_geometry.points [| 0., 0., 0.; 2., 0., 0.; 4., 0., 0. |]) in
  let packed = Lisp_sop.node ~with_:["source", (source); "targets", (targets)] {|(sop/copy_to_points (sop/ext_source) (sop/ext_targets) :pack true)|} in
  let output = Session.cook session ~context packed |> Result.get_ok in
  let box_points = Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.payload)) in
  assert (match output.instances with
    | Some transforms -> Array.length transforms = 3
      && Rays_math.Vec3.nearly_equal (Rays_math.Mat4.transform_point transforms.(2) Rays_math.Vec3.zero)
           (Rays_math.Vec3.create 4. 0. 0.) ~eps:1e-12
    | None -> false);
  let consumer = Lisp_sop.node ~with_:["packed", (packed)] {|(sop/null (sop/ext_packed))|} in
  let materialized = Session.cook session ~context consumer |> Result.get_ok in
  assert (materialized.instances = None
    && Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry materialized.payload)) = 3 * box_points);
  let hits = (Session.stats session).hits in
  ignore (Session.cook session ~context consumer |> Result.get_ok);
  assert ((Session.stats session).hits > hits);
  Session.close session

(* A volatile node keeps one slot outside the LRU: it never evicts static
   entries, and a hit needs the same key. *)
let test_volatile_slots () =
  let evaluator = session ~entries:2 () in
  let static = Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|]) in
  let live_node x = custom ~operation:"volatile_test" ~version:1
    ~parameters:(Printf.sprintf "x=%g" x) [static] (fun ~context:_ inputs -> Ok inputs.(0)) in
  let first = live_node 0. in
  Session.set_volatile evaluator (fun id -> id = Node.id first);
  let c = context () in
  ignore (cook_ok evaluator c static);
  for step = 0 to 19 do
    let node = Node.Private.adopt_identity ~source:first (live_node (float step)) in
    ignore (cook_ok evaluator c node)
  done;
  let stats = Session.stats evaluator in
  check (stats.evictions = 0 && stats.retained_entries = 1 && stats.volatile_entries = 1
    && stats.volatile_misses = 20 && stats.volatile_hits = 0)
    "volatile node churned the LRU or kept more than one slot";
  ignore (cook_ok evaluator c static);
  let stats = Session.stats evaluator in
  check (stats.hits = 21 && stats.misses = 21) "the static entry was evicted by a volatile node";
  let same = Node.Private.adopt_identity ~source:first (live_node 19.) in
  ignore (cook_ok evaluator c same);
  check ((Session.stats evaluator).volatile_hits = 1) "the latest volatile entry did not hit";
  Session.set_volatile evaluator (fun _ -> false);
  check ((Session.stats evaluator).volatile_entries = 0) "set_volatile kept a stale slot";
  Session.close evaluator

(* E5: the volatile predicate travels with the cook request. Set while a cook
   is in flight, it is neither called nor installed off the worker's domain:
   the running cook keeps its slots and the next request applies it. *)
let test_async_volatile_on_worker () =
  let worker = Async_cook.create ~max_entries:4 ~max_payload_bytes:4_000_000 |> get_ok in
  let main = Domain.self () and off_worker = Atomic.make false in
  let release = Atomic.make true and entered = Atomic.make false in
  let node = custom ~operation:"async_volatile_test" ~version:1 ~parameters:""
      [Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|])] (fun ~context:_ inputs ->
        Atomic.set entered true;
        while not (Atomic.get release) do Unix.sleepf 0.001 done;
        Ok inputs.(0)) in
  let again () = Node.Private.adopt_identity ~source:node
      (custom ~operation:"async_volatile_test" ~version:1 ~parameters:"again"
        [Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|])] (fun ~context:_ inputs ->
          Atomic.set entered true;
          while not (Atomic.get release) do Unix.sleepf 0.001 done;
          Ok inputs.(0))) in
  let predicate answer id =
    if Domain.self () = main then Atomic.set off_worker true;
    answer && id = Node.id node in
  let prepare _ = Ok () in
  let cook node =
    ignore (Async_cook.submit worker ~context:(context ()) ~node ~prepare |> get_ok) in
  Async_cook.set_volatile worker (predicate true);
  cook node;
  ignore (Async_cook.await worker);
  check ((Async_cook.stats worker).volatile_entries = 1)
    "the request did not carry its volatile predicate";
  Atomic.set release false;
  Atomic.set entered false;
  cook (again ());
  ignore (wait_until (fun () -> if Atomic.get entered then Some () else None));
  Async_cook.set_volatile worker (predicate false);
  check (not (Atomic.get off_worker))
    "set_volatile ran the predicate on the caller's domain during a cook";
  Atomic.set release true;
  ignore (Async_cook.await worker);
  check ((Async_cook.stats worker).volatile_entries = 1)
    "set_volatile changed the session under the cook in flight";
  cook node;
  ignore (Async_cook.await worker);
  check ((Async_cook.stats worker).volatile_entries = 0 && not (Atomic.get off_worker))
    "the next request did not install the new volatile predicate on the worker";
  Async_cook.close worker

let test_async_await () =
  let worker = Async_cook.create ~max_entries:4 ~max_payload_bytes:4_000_000 |> get_ok in
  let node = Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.; 1., 0., 0.|]) in
  let prepare output = Ok (Rdk.Geometry.point_count (Result.get_ok (Sop.Payload.geometry output.Session.payload))) in
  ignore (Async_cook.submit worker ~context:(context ()) ~node ~prepare |> get_ok);
  let completion = Async_cook.await worker in
  check (completion.result = Ok 2 && Async_cook.status worker = Idle
    && Async_cook.poll worker = None) "await did not return the submitted cook";
  check ((Async_cook.stats worker).misses = 1) "await: the session did not cook";
  check (match Async_cook.await worker with _ -> false | exception Invalid_argument _ -> true)
    "await without a request returned";
  (* optional nodes: cooked after the required ones, a failing one is [None] and fails nothing *)
  let failing = custom ~operation:"async_test_fail" ~version:1 [Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|])]
      (fun ~context:_ _ -> Error "no geometry") in
  let other = Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.; 1., 0., 0.; 2., 0., 0.|]) in
  Async_cook.close worker;
  let worker = Async_cook.create ~max_entries:4 ~max_payload_bytes:4_000_000 |> get_ok in
  ignore (Async_cook.submit_some worker ~context:(context ()) ~nodes:[node]
    ~optional:[failing; other]
    ~prepare:(fun outputs optional -> Ok (List.length outputs, List.map Option.is_some optional)) |> get_ok);
  check ((Async_cook.await worker).result = Ok (1, [false; true]))
    "a failing optional node failed the request or hid the others";
  Async_cook.close worker

let run () =
  test_volatile_slots ();
  test_async_await ();
  test_async_volatile_on_worker ();
  test_async_cook_latest_request ();
  test_node_owned_parameters_and_graph_edit ();
  test_encoded_parameter ();
  test_lru_limits_and_lifetime ();
  test_shared_input_memo ();
  test_static_context_cache ();
  test_grid_generator_contract ();
  test_circle_generator_contract ();
  test_box_generator_contract ();
  test_uv_sphere_generator_contract ();
  test_torus_generator_contract ();
  test_tube_generator_contract ();
  test_platonic_generator_contract ();
  test_spiral_generator_contract ();
  test_declared_seed_dependency ();
  test_labeled_random_identity ();
  test_switch_is_lazy ();
  test_shared_payload_accounting ();
  test_error_trace_and_cancellation ();
  test_inspection_sharing_and_bridge ();
  test_snapshot_feedback_boundary ();
  test_parallel_geometry_exactness ();
  test_poly_fill_contract ();
  test_poly_path_contract ();
  test_revolve_contract ();
  test_sweep_contract ();
  test_local_subdivide_contract ();
  test_edge_divide_contract ();
  test_edge_collapse_contract ();
  test_edge_flip_contract ();
  test_edge_cusp_contract ();
  test_edge_straighten_contract ();
  test_edge_equalize_contract ();
  test_edge_relax_contract ();
  test_blend_shapes_contract ();
  test_attribute_composite_contract ();
  test_edge_transport_contract ();
  test_packed_copy_contract ();
  print_endline "procedural tests passed"
