open Rays
module T = Flow.Ty
module C = Flow.Context
module W = Flow.Workspace
module D = Editor_document.Workspace_doc
module P = Flow_graph.Projection
module M = Pxui_graph.Node_menu
module L = Rays_editor.Private.Lisp_text

let checks = ref 0
let check condition message = incr checks; if not condition then failwith message
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let doc = function Ok x -> x | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
let contains text fragment =
  let rec go i = i + String.length fragment <= String.length text
    && (String.sub text i (String.length fragment) = fragment || go (i + 1)) in
  go 0

let frame : Frame.t = {
  input = { (Input.at_time 0.) with dt = 1. /. 60.; size = 1000, 700 };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0.;
}

let painted_color ui (color : Rays_math.Color.t) =
  let rgba = Int32.of_int ((color.r lsl 24) lor (color.g lsl 16) lor (color.b lsl 8) lor color.a) in
  let staged = Scene.Private.stage_native ~width:1000 ~height:700 (Pxui.Ui.scene ui) |> Result.get_ok in
  List.exists (function
    | Scene.Private.Ui_layer (batch, _) ->
        let bytes = Scene_command.Ui_batch.instances batch in
        let rec find i = i < Scene_command.Ui_batch.count batch
          && (Bytes.get_int32_le bytes (i * Scene_command.Ui_batch.instance_bytes + 32) = rgba || find (i + 1)) in
        find 0
    | _ -> false) staged.layers

let () =
  let geometry = T.geometry and context = C.sop in
  check (T.of_string "geometry" = Some geometry && T.of_syntax (Flow.Syntax.make (Sym "geometry")) = Some geometry)
    "built-in nominal type is absent from annotations";
  check (T.of_string (T.to_string (T.Record ["x", T.List geometry])) = Some (T.Record ["x", T.List geometry]))
    "nested nominal type did not round trip";
  check (Marshal.from_string (Marshal.to_string (geometry, context) []) 0 = (geometry, context))
    "built-in identities did not marshal";
  check (T.fits geometry geometry && not (T.fits geometry T.Float) && T.join geometry T.Float = None)
    "nominal type identity is not distinct";
  check (Result.is_error (C.of_string "shader") && C.of_qualified "sop/toy_emit" = Some context)
    "built-in context lookup is incorrect";
  let ops = [Flow.Op.{name = "sop/toy_emit"; ctx = context;
    signature = {pos = ["number", T.Float]; opt = []; rest = None; kw = ["gain", T.Float]};
    out = (fun _ -> geometry); out_args = None; any_num = false; choices = []; shape = Scalar; live = false;
    check = (fun _ -> ()); body = (fun ~live:_ ~node args -> node "sop/toy_emit" args);
    category = "Toy"; arithmetic = None; packed_extension = None}] in
  let catalog = Flow.Catalog.{version = 1; kinds = []} in
  let kernel = Flow.Op.{name = "sop/toy_kernel"; ctx = context;
    signature = {pos = []; opt = []; rest = None;
      kw = ["field", T.Fn (Some {params = [T.Vec3]; result = T.Float})]};
    out = (fun _ -> geometry); out_args = None; any_num = false; choices = []; shape = Scalar; live = false;
    check = (fun _ -> ()); body = (fun ~live:_ ~node args -> node "sop/toy_kernel" args);
    category = "Toy"; arithmetic = None; packed_extension = None} in
  let kernel_ops = kernel :: ops in
  let field_source = "(workspace w (graph g :context sop (let* [surface (sop/toy_kernel :field (fn [p] p.x))] surface)))" in
  let field_doc = doc (D.of_text ~ops:kernel_ops catalog field_source) in
  check (field_doc.checked.nodes=[]) "narrow custom catalog acquired embedded SOP definitions";
  let field_scope = P.of_graph catalog field_doc.checked "g" in
  check (List.exists (fun (n : P.node) ->
    Option.fold ~none:false ~some:(fun (z : P.zone) -> z.kind = P.Fn) n.zone) field_scope.nodes)
    "external function port did not project as a zone";
  let field_plan = ok (Flow.Eval.static field_doc.checked) in
  (match List.assoc "field" field_plan.plan.nodes.(0).args with
   | Flow.Eval.Fn fn ->
       let params, body = Option.get (Flow.Eval.Residual.function_body fn) in
       check (params = [W.Name "p", Some T.Vec3] && body.ty = T.Float)
         "external function port lost its specialization"
   | _ -> failwith "external function port lost its callable");
  List.iter (fun body -> check (Result.is_error (D.of_text ~ops:kernel_ops catalog
    ("(workspace w (graph g :context sop (sop/toy_kernel :field " ^ body ^ ")))")))
      "external function port accepted an invalid function") ["1.0"; "(fn [a b] a.x)"];
  let changed_field = ok (D.edit catalog field_doc (Flow_graph.Flow_edit.Set_arg {
    node = ["g"; "surface"]; key = Kw "field"; sub = [];
    value = List.hd (ok (Flow.Syntax.parse "(fn [p] (* p.x 2.0))"))})) in
  check (contains (D.to_text changed_field) "(* p.x 2.0)")
    "external function-port edit was not retained";
  let text = "(workspace w\n\
    (defn make :context sop [(n : float)] (sop/toy_emit n :gain 2.0))\n\
    (graph g :context sop (let* [first (sop/toy_emit 1.0 :gain 3.0) second (make 4.0)] second)))" in
  let workspace = doc (D.of_text ~ops catalog text) in
  let evaluated = ok (Flow.Eval.static ~record:true workspace.checked) in
  check (List.assoc "g" evaluated.results = Flow.Eval.Deferred (geometry, 1)
    && Array.length evaluated.plan.nodes = 2) "custom operator did not evaluate to its typed plan";
  check (List.for_all (fun (node : Flow.Eval.node) -> node.ty = geometry && node.kind = "sop/toy_emit")
    (Array.to_list evaluated.plan.nodes)) "custom plan lost its built-in result type";
  check (Option.fold ~none:false ~some:(fun op -> op == List.hd ops)
    (Flow.Op.find ~extra:ops "toy_emit" context)) "bare context operator did not resolve";
  let scope = P.of_graph catalog workspace.checked "g" in
  check (List.exists (fun (node : P.node) -> node.path = ["g"; "first"] && node.ty = geometry) scope.nodes)
    "custom node did not project";
  let edited = ok (D.edit catalog workspace (Flow_graph.Flow_edit.Set_arg {
    node = ["g"; "first"]; key = Kw "gain"; sub = []; value = Flow.Syntax.make (Num "7.0")})) in
  check (edited.checked.ops == ops && contains (D.to_text edited) ":gain 7.0")
    "checked edit discarded custom operators";
  let reloaded = doc (D.of_text ~ops catalog (D.to_text edited)) in
  check ((ok (Flow.Eval.static edited.checked)).plan = (ok (Flow.Eval.static reloaded.checked)).plan)
    "custom operator edit did not survive reload";
  let replaced = doc (D.of_text ~ops:[{(List.hd ops) with category = "Changed"}] catalog text) in
  check (not (P.same_graph workspace.checked replaced.checked "g")) "changed declarations reused stale projection";
  let color = (Pxui.Theme.ports Pxui.default_theme).geometry in
  check (M.port_color Pxui.default_theme geometry = color) "custom operator ignored its built-in result color";
  let ui = Pxui.Ui.create ~font_size:11 () in
  Fun.protect ~finally:(fun () -> Pxui.Ui.destroy ui) (fun () ->
    let view = Pxui_graph.Scope.create ~width:1000 ~height:700 ()
      |> Pxui_graph.Scope.with_scope ~key:"g" scope in
    ignore (Pxui.Ui.frame ui frame (fun ui -> Pxui_graph.Scope.update view ui frame));
    check (painted_color ui color) "custom card was not painted in its result color";
    let menu = M.create ~x:100 ~y:100 (M.of_ops ~extra:ops context) in
    check (M.Private.keys menu ~query:"toy" = ["=sop/toy_emit"]) "menu omitted custom operator";
    let next, _ = Pxui.Ui.frame ui frame (fun ui -> M.update menu ui ~bounds:(0, 0, 1000, 700)) in
    check (painted_color ui color) "menu ignored custom operator's result color";
    let menu = Option.get next in
    let next, _ = Pxui.Ui.frame ui {frame with input = { frame.input with events = [Input.Text_input "toy"] }}
      (fun ui -> M.update menu ui ~bounds:(0, 0, 1000, 700)) in
    let menu = Option.get next in
    let _, picked = Pxui.Ui.frame ui {frame with input = { frame.input with events = [Input.Key_pressed Input.Enter] }}
      (fun ui -> M.update menu ui ~bounds:(0, 0, 1000, 700)) in
    check (picked = Some "=sop/toy_emit") "custom menu entry could not be picked");
  let vocab = L.vocab ~ops [] in
  let prefix = "(workspace w (graph g :context sop (sop/toy_" in
  check (List.exists (fun (entry : Pxui.Ui.completion) -> entry.insert = "sop/toy_emit")
    (L.complete vocab prefix (String.length prefix))) "text pane omitted custom operator completion";
  let help = "(graph g :context sop (sop/toy_emit 1.0))" in
  let _, _, op_help = Option.get (L.describe vocab help 25) in
  check (contains op_help "number:float") "text pane omitted custom operator signature";
  let _, _, context_help = Option.get (L.describe vocab help 10) in
  List.iter (fun context ->
    let desc = C.descriptor context in
    check (desc.label <> "" && desc.group <> "" && T.of_string (T.to_string desc.result) = Some desc.result)
      ("incomplete context descriptor: " ^ desc.name);
    ignore (M.color Pxui.default_theme desc.color);
    check (contains context_help desc.name) ("context missing from help: " ^ desc.name)) (C.all ());
  check (Flow.Op.validate (ops @ ops) <> None) "duplicate immutable operators accepted";
  check (Result.is_error (D.of_text catalog text)) "extra operator leaked into default checker";
  let kind = Flow.Catalog.{qualified = "sop/item"; context; slots = [];
    parameters = []; outputs = []; facts = None} in
  let catalog = {catalog with kinds = [kind]} in
  let declared = doc (D.of_text catalog "(workspace w (graph g :context sop (sop/item)))") in
  check (List.assoc "g" (ok (Flow.Eval.static declared.checked)).results = Flow.Eval.Deferred (geometry, 0))
    "custom catalog item ignored its built-in context result type";
  let vocab = L.vocab [Flow_sop.Catalog.{qualified = "sop/item"; key = "item";
    operation = "item"; label = "Item"; category = ["Toy"]; slots = []; slot_types = []; keyword_inputs = []; fields = []}] in
  let prefix = "(workspace w (graph g :context sop (sop/" in
  check (List.exists (fun (entry : Pxui.Ui.completion) -> entry.insert = "sop/item")
    (L.complete vocab prefix (String.length prefix))) "catalog completion confused prefix with context identity";
  check (not (List.exists (fun (d : Diagnostic.t) -> d.code = "E_NAMESPACE")
    (snd (W.check {catalog with kinds = []}
      (Flow.Syntax.parse "(workspace w (graph g :context sop (sop/missing)))" |> Result.get_ok)))))
    "built-in catalog prefix was not recognized";
  let module E = Rays_editor.Editor in
  let host_op = Flow.Op.{name = "sop/toy_box"; ctx = context;
    signature = {pos = []; opt = []; rest = None; kw = []};
    out = (fun _ -> geometry); out_args = None; any_num = false; choices = []; shape = Scalar; live = false;
    check = (fun _ -> ()); body = (fun ~live:_ ~node _ -> node "sop/box" []);
    category = "Toy"; arithmetic = None; packed_extension = None} in
  let host_text = "(workspace host (graph g :context sop (let* [base (sop/box)] (sop/toy_box)))\n\
    (graph editor :context editor (ui/workspace (ui/split \"horizontal\" (ui/graph \"g\") (ui/lisp)))))" in
  let host_ops = Flow_sop.Operators.all @ ops @ [host_op] in
  let workspace = doc (Rays_editor.Workspace.load ~ops:host_ops host_text) in
  let presets = Filename.temp_dir "rays-domain-test" "" in
  let host = ref (match E.create ~ui_text_size:11 ~workspace ~await:true ~domains:1 ~presets
    ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Scene.D3.create []) () with
    | Ok host -> host | Error message -> failwith message) in
  Fun.protect ~finally:(fun () -> E.close !host) (fun () ->
    let step events = host := E.update !host { frame with input = { frame.input with events } } in
    step [];
    check (Option.is_some (E.node_box !host ["g"; "@result"])) "host omitted custom operator's card";
    let x, y, width, height = (E.panes !host frame).graph in
    let point = float (x + width / 2), float (y + height / 2) in
    step [Input.Pointer_moved point; Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)];
    step [Input.Key_pressed (Input.KeyChar '/')];
    step [Input.Key_pressed (Input.KeyChar 'a')];
    step [Input.Text_input "sop/toy_box"];
    step [Input.Key_pressed Input.Enter];
    let after = E.workspace !host in
    let count = Array.length (ok (Flow.Eval.static after.checked)).plan.nodes in
    check (contains (D.to_text after) "sop/toy_box" && count = 3)
      (Printf.sprintf "host menu did not insert custom operator (%d nodes): %s" count (D.to_text after));
    let path = Filename.concat presets "saved.rays" in
    Out_channel.with_open_bin path (fun oc -> output_string oc (D.to_text (E.workspace !host)));
    let loaded = Editor_document.Preset.load_with_ops ~ops:host_ops ~path ~factories:Sop.Nodes.factories
      ~settings:Editor_document.Settings.none () |> Result.get_ok in
    check (Array.length (loaded.doc.workspace.lowered).evaluated.plan.nodes = 3)
      "preset reload discarded custom operators");
  print_endline (Printf.sprintf "open domain: %d checks passed" !checks)
