(* The assertions the test_main modules share. A test opens this module unless it reports
   under its own name. *)

let fail message = raise (Failure message)
let check condition message = if not condition then fail message

(* Small test-only helpers shared by editor tests. The complete projection, cook and pane
   suites remain owned and run by their libraries; the editor tests use only these primitives. *)
module Workspace_cook = struct
  let encode value = Marshal.to_string value [Marshal.No_sharing]
  let geometry_bytes geometry =
    let open Rdk in
    let attribute (attribute : Attribute.t) =
      let payload = match Attribute.storage attribute with
        | Float values -> encode values | Int values -> encode values
        | Text values -> encode values
        | Int_array values -> encode (Packed.Int_array.View.view values)
        | Float_array values -> encode (Packed.Float_array.View.view values)
        | Float2 values -> let v = Packed.Float2.View.view values in encode (v.x, v.y)
        | Float3 values -> let v = Packed.Float3.View.view values in encode (v.x, v.y, v.z)
        | Float4 values -> let v = Packed.Float4.View.view values in encode (v.x, v.y, v.z, v.w) in
      Attribute.owner attribute, Attribute.name attribute, Attribute.kind_name attribute, payload in
    let groups = List.map (fun group -> Group.owner group, Group.name group,
      Array.init (Group.length group) (fun index -> Group.mem index group),
      Group.ordered_elements group) (Geometry.groups geometry) in
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    encode (positions.x, positions.y, positions.z,
      Topology.View.view (Geometry.topology geometry),
      List.map attribute (Geometry.attributes geometry), groups)

  let cook ~domains (graph : Flow_sop.Lower.graph) =
    match graph.root with
    | None -> None
    | Some root ->
        let compiled = match Sop.Edit_graph.compile_node graph.network.geometry ~node_id:root with
          | Ok compiled -> compiled | Error message -> failwith message in
        let context = Sop.Context.create ~domains ~grain:97 ~seed:42L () |> Result.get_ok in
        let session = Sop.Session.create ~max_entries:512
            ~max_payload_bytes:(256 * 1024 * 1024) |> Result.get_ok in
        Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
          match Sop.Session.cook session ~context compiled with
          | Ok output -> Some (Sop.Payload.geometry output.payload |> Result.get_ok)
          | Error error -> failwith (Diagnostic.error_to_string error))
end

module Projection = struct
  module P = Flow_graph.Projection
  let cases = "../../../specification/workspace/cases"
  let read path = In_channel.with_open_bin path In_channel.input_all
  let catalog = Editor_document.Kinds.catalog ~version:1 Sop.Nodes.factories |> Result.get_ok
  let workspace_of text = match Flow.Syntax.parse text with
    | Error d -> failwith (Diagnostic.to_string d)
    | Ok forms -> (match Flow.Workspace.check catalog forms with
        | Some workspace, _ -> workspace
        | None, diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics)))
  let load name = workspace_of (read (Filename.concat cases (name ^ ".lisp")))
  let scope workspace graph = P.of_graph catalog workspace graph
  let node workspace graph path = match P.find (scope workspace graph) (graph :: path) with
    | Some node -> node | None -> failwith (String.concat "/" (graph :: path) ^ " is not projected")
end

module Graph_pane = struct
  open Rays
  let frame () : Frame.t = {
    input = { (Input.at_time 0.) with frame = 0; tick = 0; size = 1000, 700;
      pointer = 0., 0.; keys = []; events = [] };
    pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0.;
  }
  let catalog = Editor_document.Kinds.catalog ~version:1 Sop.Nodes.factories |> Result.get_ok
  let load_workspace name =
    let path = Filename.concat "../../../specification/workspace/cases" (name ^ ".lisp") in
    let text = In_channel.with_open_bin path In_channel.input_all in
    match Flow.Syntax.parse text with
    | Error d -> failwith (Diagnostic.to_string d)
    | Ok forms -> (match Flow.Workspace.check catalog forms with
        | Some workspace, _ -> workspace | None, _ -> failwith (name ^ " did not check"))
  let ui = Pxui.Ui.create ~font_size:11 ()
  let step view (frame : Frame.t) =
    let settle = { frame with input = { frame.input with events = [] } } in
    let view, _ = Pxui.Ui.frame ui settle (fun ui -> Pxui_graph.Scope.update view ui settle) in
    Pxui.Ui.frame ui frame (fun ui -> Pxui_graph.Scope.update view ui frame)
  let recorded workspace = Flow_graph.Probe.make
      (Flow.Eval.static ~record:true workspace |> Result.get_ok)
  let scope_view ?probe workspace graph =
    let scope = Flow_graph.Projection.of_graph catalog workspace graph in
    (Pxui_graph.Scope.create ~width:1000 ~height:700 ()
    |> Pxui_graph.Scope.with_scope ?probe ~key:graph scope
    |> Pxui_graph.Scope.with_records (recorded workspace)), scope
  let click view (x, y) =
    let point = float x, float y in
    let events = [Input.Pointer_moved point; Input.Pointer_pressed (Input.LeftButton, point);
      Input.Pointer_released (Input.LeftButton, point)] in
    step view { (frame ()) with input = { (frame ()).input with pointer = point; events } }
end
