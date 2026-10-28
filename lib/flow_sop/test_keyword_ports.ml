module Field = struct
  type parameters = unit
    [@@sop.node_key "test_field"] [@@sop.node_label "Test field"]
    [@@sop.node_category "Test"] [@@sop.node_inputs 2]
    [@@sop.node_slots "field, picture"] [@@sop.node_keywords "field, picture"]
    [@@sop.node_types ["fn(vec3,float)->float"; "image"]]
    [@@deriving sop_params, sop_node]
  let build = parameters_build (fun ~label _ field picture ->
    Sop.Node.Private.make ~label ~operation:"test_field" ~version:1
      ~parameters:"" ~cook_mode:Sop.Node.Generic
      ~dependencies:Sop.Context.Dependencies.static ~inputs:[|field; picture|]
      (fun ~node_id:_ context inputs ->
        Result.bind (Sop.Payload.image inputs.(1)) (fun _ ->
        Result.bind (Sop.Payload.kernel inputs.(0)) (fun kernel ->
        Result.bind (Sop.Kernel.prepare kernel
          [Vec3s [|2.; 0.; 0.|]; Floats [|0.|]]) (fun run ->
        Result.bind (run context) (function
          | Sop.Kernel.Floats values ->
              Sop.Node.Private.cook (Lisp_sop.snapshot (Rdk.Line_geometry.points (Array.map (fun x -> x, 0., 0.) values))) context [||]
          | _ -> assert false))))))
  let factory = parameters_factory build
end

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked catalog args =
  let forms = Flow.Syntax.parse ("(workspace w (graph g :context sop (sop/test_field " ^ args ^ ")))" ) |> ok in
  let workspace, _ = Flow.Workspace.check catalog forms in
  Option.is_some workspace

let () =
  let module Edit = Sop.Edit_graph in
  assert (Edit.factory_keyword_inputs Field.factory = ["field"; "picture"]);
  assert (Edit.factory_input_types Field.factory = ["fn(vec3,float)->float"; "image"]);
  let catalog = Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version (Field.factory :: Sop.Nodes.factories) |> ok in
  let text, _ = Flow_sop.Manifest.generate (Field.factory :: Sop.Nodes.factories) |> ok in
  let loaded, _ = Flow.Check.catalog_of_manifest text |> ok in
  assert (catalog = loaded);
  let kind = List.hd catalog.kinds in
  assert (kind.slots = [] && List.map (fun (p : Flow.Check.parameter) -> p.name) kind.parameters = ["field"; "picture"]);
  let forms = Flow.Syntax.parse "(workspace w (graph g :context sop (let* [arg0 1] nil)))" |> ok in
  let defaults = List.concat_map (fun (p : Flow.Check.parameter) ->
    let ty = match p.ty with
      | Some (Flow.Port_type.Fn signature) -> Flow.Ty.Fn (Some signature)
      | Some Image -> Flow.Ty.image | _ -> assert false in
    [Flow.Syntax.make (Kw p.name);
      Flow_graph.Flow_edit.default_for ~fresh:(Flow_graph.Flow_edit.fresh_name forms ~root:"g")
        ty p.name |> Option.get]) kind.parameters in
  let expr = Flow.Syntax.make (List (Flow.Syntax.make (Sym "sop/test_field") :: defaults)) in
  let forms, workspace = Flow_graph.Flow_edit.apply_checked catalog forms
      (Add_node {scope = ["g"]; name = "surface"; expr}) |> ok in
  let projection = Flow_graph.Projection.of_graph catalog workspace "g" in
  assert (List.exists (fun (n : Flow_graph.Projection.node) ->
    Option.fold ~none:false ~some:(fun (z : Flow_graph.Projection.zone) -> z.kind = Fn) n.zone) projection.nodes);
  let replacement = Flow.Syntax.parse "(fn [p amount] (+ p.x amount))" |> ok |> List.hd in
  ignore (Flow_graph.Flow_edit.apply_checked catalog forms
    (Set_arg {node = ["g";"surface"]; key = Kw "field"; sub = []; value = replacement}) |> ok);
  let args = ":field (fn [p amount] (+ p.x amount)) :picture (image/noise :width 2 :height 2)" in
  List.iter (fun catalog ->
    assert (checked catalog args);
    assert (not (checked catalog ":field (fn [p] p.x) :picture (image/noise)"));
    assert (not (checked catalog ":picture (image/noise)"));
    assert (not (checked catalog ":field (fn [p amount] (+ p.x amount))"))) [catalog; loaded];
  let node = Lisp_sop.node {|(-> (sop/box) (sop/null))|} in
  assert (List.for_all2 ( == ) (Sop.Node.inputs (Sop.Edit_graph.instantiate Field.factory [node; node] |> Result.get_ok)) [node; node]);
  let bad = Edit.factory ~key:"bad" ~label:"Bad" ~category:["Test"] ~arity:1
      ~slots:["field"] ~keyword_inputs:["field"] ~input_types:["fn(vec3,)->float"] (fun _ -> node) in
  assert (Result.is_error (Flow_sop.Catalog.of_factories ~version:1 [bad]))

let () =
  let module L = Flow_sop.Lower in
  let module S = Sop.Session in
  let module Edit = Sop.Edit_graph in
  let text offset = Printf.sprintf
    "(workspace w
       (defn consume :context sop [(f : fn)]
         (sop/test_field :field f :picture (image/noise :width 1 :height 1)))
       (graph g :context sop
       (let* [a (let* [offset (+ %d t) field (fn [p amount] (+ p.x offset))]
                  (consume field))
              b (let* [offset (+ 4 t) field (fn [p amount] (+ p.x offset))]
                  (sop/test_field :field field :picture (image/noise :width 1 :height 1)))]
         (sop/merge a b))))" offset in
  let lower ?(reference = false) ?previous offset =
    L.workspace ~factories:(Field.factory :: Sop.Nodes.factories) ~reference
      ?compiled_ids:(Option.map (fun (p : L.t) -> p.compiled_ids) previous)
      ?sites:(Option.map (fun (p : L.t) -> p.sites) previous)
      (Flow.Syntax.parse (text offset) |> ok) |> ok in
  let node (l : L.t) = let graph = List.hd l.graphs in
    Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> Result.get_ok in
  List.iter (fun domains ->
    let session = S.create ~max_entries:32 ~max_payload_bytes:65536 |> Result.get_ok in
    Fun.protect ~finally:(fun () -> S.close session) (fun () ->
      let cooked node time =
        let context = Sop.Context.create ~domains ~time () |> Result.get_ok in
        let output = match S.cook session ~context node with Ok x -> x
          | Error d -> failwith (Diagnostic.error_to_string d) in
        let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
        Array.init (Rdk.Geometry.point_count geometry)
          (fun i -> Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) i) in
      let first = lower 7 in
      let functions = first.plan.nodes |> Array.to_list |> List.filter_map (fun (n : Flow.Eval.node) ->
        if n.kind <> "sop/test_field" then None else match List.assoc "field" n.args with
          | Flow.Eval.Fn fn -> Some fn | _ -> assert false) |> Array.of_list in
      let converted = Parallel.run ~domains (fun () ->
        Parallel.map_array ~grain:1 (fun fn ->
          let value = Flow.Eval.Private.map_function
              ~signature:Flow.Ty.{params = [Vec3; Float]; result = Vec3} fn
              [Flow.Eval.Vec3_array [|2.;0.;0.|]; Float_array [|0.|]] |> ok in
          let program = Flow_sop.Attribute_kernel.prepare ~sources:[] [] value |> ok in
          let packed = Option.get (Flow_ir.Executor.packed_region program) in
          let live = Input.at_time 0.5 in
          let compiled = Flow_ir.Packed.force packed ~live |> ok in
          let reference = Flow_ir.Executor.force ~reference:true program ~live |> ok in
          assert (compiled = reference); compiled) functions) in
      assert (converted = [|Flow.Eval.Vec3_array [|9.5;9.5;9.5|];
        Vec3_array [|6.5;6.5;6.5|]|]);
      let initial = node first in
      assert (cooked initial 0. = [|9.,0.,0.; 6.,0.,0.|]);
      let cooks = (S.stats session).cooks in
      assert (cooked initial 0. = [|9.,0.,0.; 6.,0.,0.|]);
      assert ((S.stats session).cooks = cooks);
      assert (cooked initial 0.5 = [|9.5,0.,0.; 6.5,0.,0.|]);
      assert (cooked (node (lower ~reference:true 7)) 0.5 = [|9.5,0.,0.; 6.5,0.,0.|]);
      let edited = node (lower ~previous:first 10) in
      assert (Sop.Node.id edited = Sop.Node.id initial);
      assert (cooked edited 0. = [|12.,0.,0.; 6.,0.,0.|]))) [1;8];
  print_endline "keyword ports: generated Fn/Image metadata, compiled captures, live values and cache invalidation pass"

let () =
  let captured_geometry = "(workspace w (graph g :context sop
    (let* [source (sop/curve (list [1 0 0] [2 0 0]))
           sample {:bias (array/sum (sop/attr source :P))}
           field (fn [p amount] (+ p.x sample.bias.x))]
      (sop/test_field :field field :picture (image/noise :width 1 :height 1)))))" in
  let zone = "(workspace w (graph g :context sop
    (let* [source (sop/curve (list [1 0 0] [2 0 0]))
           samples (for [p (sop/point_list source)]
             (sop/test_field :field (fn [v amount] (+ v.x p.x))
               :picture (image/noise :width 1 :height 1)))]
      (sop/merge samples))))" in
  List.iter (fun (text, expected) -> List.iter (fun reference ->
    let l = Flow_sop.Lower.workspace ~reference
        ~factories:(Field.factory :: Sop.Nodes.factories)
        (Flow.Syntax.parse text |> ok) |> ok in
    let graph = List.hd l.graphs in
    let node = Sop.Edit_graph.compile_node graph.network.geometry
        ~node_id:(Option.get graph.root) |> Result.get_ok in
    List.iter (fun domains ->
      let session = Sop.Session.create ~max_entries:32 ~max_payload_bytes:65536 |> Result.get_ok in
      Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
        let context = Sop.Context.create ~domains () |> Result.get_ok in
        let output = match Sop.Session.cook session ~context node with
          | Ok x -> x | Error d -> failwith (Diagnostic.error_to_string d) in
        let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
        let points = Array.init (Rdk.Geometry.point_count geometry)
            (fun i -> Rdk.Packed.Float3.get (Rdk.Geometry.positions geometry) i) in
        assert (points = expected))) [1;8]) [false;true])
    [captured_geometry, [|5.,0.,0.|]; zone, [|3.,0.,0.; 4.,0.,0.|]]
