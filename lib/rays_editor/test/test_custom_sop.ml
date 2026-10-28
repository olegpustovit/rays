open Rays
open Sop
open Test_support

let edit_parameters graph ~node_id changes =
  Result.bind (Edit_graph.apply_parameters (Edit_graph.of_graph graph)
      ~node_id changes) (fun (document, effects) ->
    Result.map (fun graph -> graph, effects) (Edit_graph.compile document))

type controls = {
  translate_x : float [@sop.default 0.] [@sop.label "Translate X"]
    [@sop.folder "Transform"] [@sop.min (-5.)] [@sop.max 5.];
  time_scale : float [@sop.default 0.] [@sop.label "Time scale"]
    [@sop.folder "Animation"] [@sop.min (-2.)] [@sop.max 2.];
} [@@deriving sop_params]

let cook session context node = match Session.cook session ~context node with
  | Ok output -> (Result.get_ok (Sop.Payload.geometry output.Session.payload))
  | Error error -> fail (Diagnostic.error_to_string error)

let x geometry =
  let positions = Rdk.Packed.Float3.Private.view
      (Rdk.Geometry.positions geometry) in
  positions.x.(0)

let run () =
  let input = Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|]) in
  let dependencies = Context.Dependencies.one Context.Dependencies.Time in
  let node = Custom.node ~label:"animated wrangle" ~operation:"animated_translate"
      ~schema:controls_schema ~values:{ translate_x = 1.; time_scale = 2. } [input]
      (fun ~label ~inputs ~parameters ->
        Custom.plain ~label ~version:2 ~parameters:(Param.key controls_schema parameters)
          ~cook_mode:(Node.Duplicate_input 0) ~dependencies
          ~operation:"animated_translate" inputs
          (fun ~context geometries ->
        if Array.length geometries <> 1 then Error "expected one input" else
        let offset = parameters.translate_x
            +. (parameters.time_scale *. Context.time context) in
        Ok (Rdk.Transform_ops.transform
          (Rays_math.Mat4.translation (Rays_math.Vec3.create offset 0. 0.)) geometries.(0)))) in
  if List.length (Node.parameter_fields node) <> 2
     || not (Node.has_parameters node)
     || not (Context.Dependencies.mem Context.Dependencies.Time
        (Node.dependencies node))
  then fail "custom SOP did not own PPX parameters or dependencies";
  let session = Session.create ~max_entries:8 ~max_payload_bytes:1_000_000
      |> Result.get_ok in
  let context time = Context.create ~time () |> Result.get_ok in
  if x (cook session (context 3.) node) <> 7. then
    fail "custom SOP did not receive parameters and timeline context";
  let before_key = Node.parameters node and node_id = Node.id node in
  let edited, effects = edit_parameters node ~node_id
      ["translate_x", Param.Float_value 4.] |> Result.get_ok in
  if not effects.cook || Node.id edited <> node_id
     || Node.parameters edited = before_key
  then fail "custom SOP edit lost stable identity or cache-key invalidation";
  if x (cook session (context 3.) edited) <> 10. then
    fail "custom SOP edit did not rebuild its cook closure";
  let fields = Node.parameter_fields edited in
  let translated = List.find (fun field -> field.Param.name = "translate_x")
      fields in
  if translated.current <> Param.Float_value 4. then
    fail "custom SOP inspector metadata did not retain edited values";
  Session.close session;
  let custom_factories =
    Edit_graph.factory ~key:"my_custom_sop" ~label:"My Custom SOP" ~category:[ "Custom" ]
      ~arity:0 (function [] -> Lisp_sop.snapshot (Rdk.Line_geometry.points [| 0., 0., 0. |]) | _ -> invalid_arg "none")
    :: Sop.Nodes.factories in
  let text = {|(workspace test_custom
    (graph scene :context scene
      (let* [cam (scene/camera :eye [1 2 3] :target [0 0 0])
             geom (scene/geometry (ref g))]
        (scene/merge cam geom)))
    (graph g :context sop
      (sop/my_custom_sop)))|} in
  let doc = match Rays_editor.Workspace.load ~factories:custom_factories text with
    | Ok d -> d | Error ds -> fail (String.concat "; " (List.map (fun (d : Diagnostic.t) -> d.message) ds)) in
  let base_cam = Rays.Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:5. () in
  let dec_cam = Rays_editor.Workspace.declared_camera ~factories:custom_factories doc base_cam in
  let eye = Rays.Camera.position (Rays.Easy_camera.camera dec_cam) in
  if not (Rays_math.Vec3.nearly_equal eye (Rays_math.Vec3.create 1. 2. 3.) ~eps:0.001) then
    fail "declared_camera with custom factories did not read camera";
  print_endline "custom SOP tests passed"
