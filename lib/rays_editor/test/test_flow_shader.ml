module E = Flow.Eval
module P = Flow_graph.Projection
module L = Editor_document.Layout_by_path
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let () =
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let doc = match Rays_editor.Workspace.load text with
    | Ok doc -> doc
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let catalog = Result.get_ok (Editor_document.Contexts.catalog ~version:1 Sop_catalog.Editor.factories) in
  let module N = Rays_editor.Private.Navigator in
  let outline = N.rows N.initial {N.workspace = doc.checked; active = Some "picture"; scope = None;
    records = None; probes = (fun _ -> 0); selected = []; chips = []; objects = [];
    root_detail = ""; layouts = None; notes = []} |> Array.to_list |> List.map N.describe in
  assert (List.mem "DEFINITIONS · used" outline);
  assert (not (List.exists (String.starts_with ~prefix:"GEOMETRY") outline));
  assert (List.exists (String.starts_with ~prefix:"λ grad · 1 use") outline);
  let scope = P.of_graph catalog doc.checked "picture" in
  assert (List.length scope.nodes = 9);
  let panels = ["amber"; "comet"; "lime"; "velvet"; "ember"; "echoes"] in
  List.iter (fun name -> assert (Option.is_some (P.find scope ["picture"; name]))) panels;
  let amber = Option.get (P.find scope ["picture"; "amber"]) in
  assert (amber.macro = Some "panel" && List.exists
    (fun (r : P.row) -> r.kind = P.Hole && r.label = "bands") amber.rows);
  let level path = Option.value ~default:P.Card (L.Path_map.find_opt path doc.layout.level) in
  let collapsed path = Option.value ~default:false (L.Path_map.find_opt path doc.layout.collapsed) in
  let layout = P.layout ~level ~collapsed scope in
  assert (layout.h < 800. && layout.w < 1000.);
  let evaluated = ok (E.static ~record:true doc.checked) in
  let probe = Flow_graph.Probe.make evaluated in
  List.iter (fun name -> assert (Option.is_some
    (Flow_graph.Probe.plan_node ~ty:Flow.Ty.drawing probe ["picture"; name] ~probes:[]))) panels;
  let images = Array.to_list evaluated.plan.nodes
    |> List.filter (fun (n : E.node) -> n.kind = "image/map") in
  let sites = List.map (fun (n : E.node) -> n.inst, n.site, n.iter) images in
  assert (List.length images = 25 && List.length (List.sort_uniq compare sites) = 25);
  let session = Result.get_ok (Sop.Session.create ~max_entries:0 ~max_payload_bytes:0) in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    let samples = List.map (fun (image : E.node) ->
      let fn = match List.assoc "function" image.args with E.Fn fn -> fn | _ -> assert false in
      let kernel = ok (Flow_sop.Image_kernel.prepare ~identity:image.id ~width:5 ~height:5
        ~fn ~sources:[] []) in
      let sample time =
        let context = Result.get_ok (Sop.Context.create ~time ~domains:1 ()) in
        let output = Result.get_ok (Sop.Session.cook session ~context (Flow_sop.Image_kernel.node kernel)) in
        let pixels = Result.get_ok (Sop.Payload.image output.payload) in
        Option.get (Sop.Image.Private.rgba8 pixels) |> Bytes.copy in
      sample 0., sample 1.) images in
    let backgrounds = List.map (fun i -> fst (List.nth samples i)) [0;4;8;12;16;21] in
    assert (List.length (List.sort_uniq Bytes.compare backgrounds) = 4);
    assert (List.length (List.filter (fun (a, b) -> a <> b) samples) >= 12));
  Printf.printf "flow_shader: 9 overview cards, 6 viewable panels, 25 independent kernels with live motion, layout %.0fx%.0f\n"
    layout.w layout.h
