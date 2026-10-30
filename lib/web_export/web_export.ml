module W = Flow.Workspace
module D = Diagnostic
module Doc = Editor_document.Workspace_doc
module C = Flow_ir.Core
module E = Flow.Eval

type lowered = {
  program : C.program;
  span : D.span;
  root : Flow.Eval.residual option;
  plan : Flow.Eval.plan;
}

let ( let* ) = Result.bind

let children (term : W.term) =
  match term.node with
  | W.Bypass call -> call :: Option.to_list (W.bypass_input call)
  | _ -> W.children term

(* The first reference from the draw graph to a sop or scene graph, or a catalog call of those contexts. *)
let foreign (doc : Doc.t) (draw : W.graph) =
  let graphs = doc.checked.graphs @ doc.checked.defs in
  let seen = Hashtbl.create 8 in
  let rec visit (t : W.term) =
    let here =
      match t.node with
      | W.Graph_ref { graph; _ } -> (
          match List.find_opt (fun (g : W.graph) -> g.name = graph) graphs with
          | Some g when g.context = Flow.Context.sop || g.context = Flow.Context.scene ->
              Some (graph, g.context)
          | Some g when not (Hashtbl.mem seen g.name) ->
              Hashtbl.add seen g.name ();
              Option.map (fun x -> x) (visit_graph g)
          | _ -> None)
      | Call { kind; ctx; _ } when ctx = Flow.Context.sop || ctx = Flow.Context.scene ->
          Some (kind, ctx)
      | _ -> None
    in
    match here with
    | Some (name, ctx) -> Some (t, name, ctx)
    | None -> List.find_map visit (children t)
  and visit_graph (g : W.graph) = Option.map (fun (_, n, c) -> (n, c)) (visit g.body) in
  visit draw.body

let first_draw (doc : Doc.t) =
  match List.find_opt (fun (g : W.graph) -> g.context = Flow.Context.draw) doc.checked.graphs with
  | Some g -> Ok g
  | None -> Error (D.error ~code:"E_WEB_GRAPH" "The workspace has no draw graph to export.")

let fixed_region (region : C.Region.t) =
  Flow_ir.Packed.Host.argument_types region = [||]
  && not
       (Array.exists
          (fun (residual, term) ->
            E.frame_dependent (E.Residual (E.Residual.with_bindings residual ~term [])))
          region.sources
       || Array.exists E.frame_dependent region.uniforms)

let lower (doc : Doc.t) =
  let* draw = first_draw doc in
  let* () =
    match foreign doc draw with
    | Some ((t : W.term), name, ctx) ->
        Error
          (D.error ~span:t.form.span ~code:"E_WEB_GRAPH"
             (Printf.sprintf "The web export draws 2D canvas graphs only; this reaches the %s %s."
                (Flow.Context.name ctx) name))
    | None -> Ok ()
  in
  let* lowered = Flow_sop.Lower.of_checked ~inputs:doc.inputs doc.checked in
  let evaluated = lowered.evaluated in
  let value = List.assoc draw.name evaluated.results in
  (* as native: a region stays a region when the web can run it, fixed (export data) or bound per call *)
  let runnable r = fixed_region r || Flow_web.Js.bindable r in
  match Flow_ir.Core_draw.lower ~region_allowed:runnable evaluated.plan value with
  | Ok (program, root) -> Ok { program; span = draw.form.span; root; plan = evaluated.plan }
  | Error message ->
      Error
        (D.error ~span:draw.form.span ~code:"E_WEB_LOWER"
           ("This draw graph does not lower to the core: " ^ message))

(* ---- the files ---- *)

let span_of_region (r : C.Region.t) = r.term.form.span

(* Every statement of the program, nested ones included. *)
let all_statements (body : C.statement list) =
  let reversed = ref [] in
  C.iter (fun statement -> reversed := statement :: !reversed) body;
  List.rev !reversed

(* The frame facts a region may read, named as the region's Frame instructions name them. *)
let frame_facts = [ "t"; "frame/dt"; "frame/index"; "frame/width"; "frame/height" ]
let number x = Param.float_text x
let list f xs = "[" ^ String.concat ", " (List.map f xs) ^ "]"

type files = (string * string) list

(* Only frame-independent inputs and captures can become export data. The two
   different-frame probes below additionally check their prepared values. *)
type fixed = {
  packed : Flow_ir.Packed.t;
  view : Flow_ir.Packed.Host.view;
  arrays : E.value array;
  uniforms : E.value array;
  count : int;
  frame : (int * string * bool) list;
}

let rec region_instruction = function
  | C.Region.Guarded (_, body) -> region_instruction body
  | body -> body

let uniform_components = function
  | E.Float_array values | Vec2_array values | Vec3_array values | Vec4_array values -> values
  | E.Vec2 (x, y) -> [| x; y |]
  | Vec3 (x, y, z) -> [| x; y; z |]
  | Vec4 (x, y, z, w) -> [| x; y; z; w |]
  | value -> [| Flow.Value.num value |]

let fixed_inputs (r : C.Region.t) =
  let refuse code message = Error (D.error ~span:(span_of_region r) ~code message) in
  if not (fixed_region r) then
    refuse "E_WEB_CAPTURE"
      "This region reads a value that changes from frame to frame; the web export needs its inputs \
       fixed."
  else
    let packed = r in
    let view = Flow_ir.Packed.Host.view packed in
    let probe t frame =
      {
        (Input.at_time t) with
        dt = 0.25;
        frame;
        tick = frame;
        size = (640 + frame, 480);
        pointer = (100. +. t, 100.);
      }
    in
    let prepare live =
      try Flow_ir.Packed.Host.prepare_values packed ~live
      with Not_found | Invalid_argument _ | Failure _ ->
        Error
          (D.error ~span:(span_of_region r) ~code:"E_WEB_REGION"
             "This region could not be prepared.")
    in
    let* a = prepare (probe 0. 1) in
    let* b = prepare (probe 0.5 7) in
    if a.arrays <> b.arrays || a.uniforms <> b.uniforms || a.count <> b.count then
      refuse "E_WEB_CAPTURE"
        "This region reads a value that changes from frame to frame; the web export needs its \
         inputs fixed."
    else if
      Array.exists
        (fun value ->
          match value with
          | E.Int _ | Int_array _ | Text _ | Text_array _ -> false
          | _ -> Array.exists (fun x -> not (Float.is_finite x)) (uniform_components value))
        a.uniforms
    then refuse "E_WEB_REGION" "This region captures a non-finite value."
    else begin
      let frame =
        Array.to_list view.code
        |> List.mapi (fun slot instruction ->
            match region_instruction instruction with
            | C.Region.Frame name -> [ (slot, name, false) ]
            | Int_frame name -> [ (slot, name, true) ]
            | _ -> [])
        |> List.concat
      in
      match List.find_opt (fun (_, name, _) -> not (List.mem name frame_facts)) frame with
      | Some (_, name, _) ->
          refuse "E_WEB_REGION"
            ("This region reads the frame fact " ^ name
           ^ ", which the web runtime does not provide.")
      | None ->
          Ok { packed; view; arrays = a.arrays; uniforms = a.uniforms; count = a.count; frame }
    end

(* The data of a region as the JSON members both the region table and the GPU table carry. *)
let fixed_members ?kept ~data f =
  let texts values =
    "{\"texts\":" ^ list Flow_web.Kernel_table.json_string (Array.to_list values) ^ "}"
  in
  let placed =
    if Array.length f.arrays = 0 then "[]"
    else
      list
        (fun values ->
          match values with
          | E.Text_array values -> texts values
          | _ ->
              let offset, length = data values in
              Printf.sprintf "[%d, %d, %b]" offset length
                (match values with E.Int_array _ -> true | _ -> false))
        (Array.to_list f.arrays)
  in
  Printf.sprintf "\"count\": %d, %s\"inputs\": %s, \"uniforms\": %s, \"frame\": %s" f.count
    (match kept with Some n -> Printf.sprintf "\"kept\": %d, " n | None -> "")
    placed
    (list
       (fun (index, value) ->
         let integer =
           Array.exists
             (fun instruction ->
               match region_instruction instruction with
               | C.Region.Int_uniform slot -> slot = index
               | _ -> false)
             f.view.code
         in
         match (integer, value) with
         | _, E.Text_array values -> texts values
         | _, E.Text value -> "{\"text\":" ^ Flow_web.Kernel_table.json_string value ^ "}"
         | _, (E.Int_array _ | Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _) ->
             let offset, length = data value in
             Printf.sprintf "{\"table\":[%d,%d,%b]}" offset length
               (match value with E.Int_array _ -> true | _ -> false)
         | true, E.Int n -> "[" ^ Flow_web.Kernel_table.json_string (string_of_int n) ^ "]"
         | _ -> list number (Array.to_list (uniform_components value)))
       (Array.to_list (Array.mapi (fun i value -> (i, value)) f.uniforms)))
    (list
       (fun (slot, name, integer) ->
         Printf.sprintf "[%d, %s, %b]" slot (Flow_web.Kernel_table.json_string name) integer)
       f.frame)

(* A region the web target can run as a kernel: a static count of at least 1,024 and nothing the target refuses
   (the shared web target legality check admits it). *)
let web_kernel (f : fixed) =
  if f.count >= 1024 && Flow_web.Target.region f.packed ~count:(Some f.count) = [] then
    Result.to_option (Flow_web.Wgsl.kernel f.packed)
  else None

type region_entry = {
  prefix : string;
  fixed : fixed;
  kernel : Flow_web.Wgsl.wgsl option;
  width : int;
  region : C.Region.t;
}

let region_entry ~fast ~data (r : C.Region.t) =
  if not (fixed_region r) then
    (* bound per call: the frame's binding plan carries its sources; the table holds the template *)
    let packed = r in
    let view = Flow_ir.Packed.Host.view packed in
    let f = { packed; view; arrays = [||]; uniforms = [||]; count = 0; frame = [] } in
    Ok
      {
        prefix =
          Printf.sprintf "{\"source\": %s"
            (Flow_web.Kernel_table.json_string (Flow_web.Js.region ~fast packed));
        fixed = f;
        kernel = None;
        width = Array.length view.outputs.(0);
        region = r;
      }
  else
    let* f = fixed_inputs r in
    if r.result <> C.Region.Collect || Array.length f.view.outputs <> 1 then
      Error
        (D.error ~span:(span_of_region r) ~code:"E_WEB_REGION"
           "The web export runs collecting single-output regions only; this sum or fold needs a \
            later release.")
    else
      let skip =
        Array.to_list r.skip
        |> List.filter (fun i -> i >= 0 && i < f.count)
        |> List.sort_uniq Int.compare
      in
      let skipped = List.length skip in
      let textual = Array.exists C.Region.text_register f.view.code in
      let prefix =
        Printf.sprintf "{\"source\": %s, %s, \"length\": %d, \"width\": %d, \"integer\": %b"
          (Flow_web.Kernel_table.json_string (Flow_web.Js.region ~fast f.packed))
          (fixed_members ~kept:(f.count - skipped) ~data f)
          (Array.length f.view.code)
          (Array.length f.view.outputs.(0))
          (Array.length f.view.outputs.(0) = 1
          && C.Region.integer_register f.view.code.(f.view.outputs.(0).(0)))
        ^ if textual then ",\"text\":true,\"skip\":" ^ list string_of_int skip else ""
      in
      Ok
        {
          prefix;
          fixed = f;
          kernel = (if r.skip = [||] then web_kernel f else None);
          width = Array.length f.view.outputs.(0);
          region = r;
        }

(* A kernel the program dispatches: the regions it computes (one output each, in order) and their shared inputs. *)
type group = { id : string; members : int list; fixed : fixed; wgsl : Flow_web.Wgsl.wgsl }

let groups ~fast program (entries : region_entry array) =
  let _, groups =
    Flow_ir.Core_draw.device_groups ~fast program
      ~width:(fun k -> Option.map (fun _ -> entries.(k).width) entries.(k).kernel)
      ~legal:(fun region ->
        match fixed_inputs region with Ok fixed -> web_kernel fixed <> None | Error _ -> false)
  in
  List.map
    (fun (group : Flow_ir.Core_draw.group) ->
      let k = List.hd group.members in
      let fixed, wgsl =
        match group.members with
        | [ _ ] -> (entries.(k).fixed, Option.get entries.(k).kernel)
        | _ ->
            let fixed = Result.get_ok (fixed_inputs group.region) in
            (fixed, Option.get (web_kernel fixed))
      in
      { id = Printf.sprintf "k%d" k; members = group.members; fixed; wgsl })
    groups

let index_html name =
  Printf.sprintf
    {|<!doctype html>
<meta charset="utf-8">
<title>%s</title>
<style>html, body { margin: 0; height: 100%%; background: #080a10; } canvas { display: block; width: 100%%; height: 100%%; }</style>
<!-- This page needs cross-origin isolation (Cross-Origin-Opener-Policy: same-origin and
     Cross-Origin-Embedder-Policy: require-corp) for its worker pool; `npx serve DIR` sends them from serve.json. -->
<canvas id="canvas"></canvas>
<script type="module">
import { start } from "./runtime/runtime.js";
import * as program from "./program.js";
const data = await (await fetch("data.bin")).arrayBuffer();
await start(document.getElementById("canvas"), program, { data });
</script>
|}
    name

(* Image maps share the packed compiler with native images. One dummy UV is enough to compile the
   body; WGSL derives the real UV from the pixel index. Dimensions and scalar captures use the same
   core/JS lowering as the frame, so animated captures are evaluated rather than frozen at export. *)
let image_arguments ?(canvas = false) (lowered : lowered) (node : Flow.Eval.node) values =
  let results = ref [] in
  let source : C.source =
    {
      site = node.site;
      owner = node.site;
      instance = node.inst;
      prefix = [];
      iter = List.map (fun i -> C.Static_index i) node.iter;
      span = Some lowered.span;
    }
  in
  let rec residual = function
    | Flow.Eval.Residual r -> Some r
    | List xs -> Array.find_map residual xs
    | Record fields | Struct (_, _, fields) -> List.find_map (fun (_, v) -> residual v) fields
    | _ -> None
  in
  let root = List.find_map residual values in
  let root = match root with Some r -> Some r | None -> lowered.root in
  let* args =
    Flow_ir.Core_lower.lower_draw
      ~root:(lazy (Option.get root))
      (fun ctx ->
        results :=
          List.mapi
            (fun i value ->
              let dimension = if i = 0 then "width" else "height" in
              if canvas && i < 2 && not (List.mem_assoc dimension node.args) then begin
                let n = ctx.local Flow.Ty.Int in
                ctx.emit source (C.Assign (n, Frame (if i = 0 then Width else Height)));
                n
              end
              else ctx.value source value)
            values)
    |> Result.map_error (fun name ->
        D.error ~span:lowered.span ~code:"E_WEB_IMAGE"
          ("Image arguments do not lower to the core: " ^ name))
  in
  Ok { args with results = Array.of_list !results }

let map_entry (lowered : lowered) id =
  let module E = Flow.Eval in
  let span = lowered.span in
  let refuse message = Error (D.error ~span ~code:"E_WEB_IMAGE" message) in
  let node = lowered.plan.nodes.(id) in
  let arg name fallback = Option.value (List.assoc_opt name node.args) ~default:fallback in
  match arg "function" (E.Int 0) with
  | E.Fn fn -> (
      let* pixels =
        E.Host.map_function ~site:(node.inst, node.site, node.iter)
          ~signature:Flow.Ty.{ params = [ Vec2 ]; result = Vec4 }
          fn
          [ E.Vec2_array [| 0.5; 0.5 |] ]
      in
      match pixels with
      | E.Residual r ->
          let* packed =
            Flow_ir.Packed.compile_result r (E.Residual.residual_view r).term
            |> Result.map_error (function
              | d :: _ -> d
              | [] -> D.error ~span ~code:"E_WEB_IMAGE" "Image kernel did not compile.")
          in
          let region = packed in
          let* () =
            match Flow_web.Target.region packed ~count:None with d :: _ -> Error d | [] -> Ok ()
          in
          let* wgsl = Flow_web.Wgsl.kernel ~image:true packed in
          let values =
            [ arg "width" (E.Int 256); arg "height" (E.Int 256) ] @ Array.to_list region.uniforms
          in
          let* args = image_arguments { lowered with root = Some r } node values in
          let animated =
            List.exists E.frame_dependent values
            || Array.exists (function C.Region.Frame _ -> true | _ -> false) region.code
          in
          let frames =
            Array.to_list region.code
            |> List.mapi (fun slot -> function C.Region.Frame name -> [ (slot, name) ] | _ -> [])
            |> List.concat
          in
          let* () =
            match List.find_opt (fun (_, name) -> not (List.mem name frame_facts)) frames with
            | None -> Ok ()
            | Some (_, name) -> refuse ("Image kernel reads an unavailable frame fact: " ^ name)
          in
          Ok (args, animated, frames, wgsl)
      | _ -> refuse "Image map did not produce a packed pixel body.")
  | _ -> refuse "Image map needs a pixel function."

let files (doc : Doc.t) (lowered : lowered) : (files, D.t) result =
  (* the workspace's precision, as the editor reads it: fast unless (settings/config :precision "exact") *)
  let fast =
    match Editor_document.Lowering.plan_window doc lowered.plan with
    | Ok w -> w.fast
    | Error _ -> true
  in
  let data = Buffer.create 4096 in
  let stored = Hashtbl.create 16 in
  let store values =
    let offset = Buffer.length data in
    let length =
      match values with
      | E.Int_array values ->
          Array.iter (fun x -> Buffer.add_int64_le data (Int64.of_int x)) values;
          Array.length values
      | E.Float_array values | Vec2_array values | Vec3_array values | Vec4_array values ->
          Array.iter (fun x -> Buffer.add_int64_le data (Int64.bits_of_float x)) values;
          Array.length values
      | _ -> invalid_arg "Web region inputs must be packed arrays."
    in
    Hashtbl.replace stored values (offset, length);
    (offset, length)
  in
  (* A kernel's inputs are the data its regions already hold, when they are the same arrays. *)
  let shared values =
    match Hashtbl.find_opt stored values with Some placed -> placed | None -> store values
  in
  let kernels = ref [] and modules = ref [] and imports = ref [] in
  let states = Hashtbl.create 16 in
  let kernel id (w : Flow_web.Wgsl.wgsl) =
    kernels :=
      !kernels @ [ (Flow_web.Kernel_table.entry ~id w, "// kernel " ^ id ^ "\n" ^ w.source) ]
  in
  let emit_program prefix (program : C.program) =
    let slots =
      Array.map
        (fun (slot : C.state_slot) ->
          let key = (slot.source.instance, slot.prefix @ slot.zone, slot.plane) in
          match Hashtbl.find_opt states key with
          | Some id -> id
          | None ->
              let id = Hashtbl.length states in
              Hashtbl.add states key id;
              id)
        program.state_slots
    in
    let* entries =
      Array.to_list program.regions
      |> List.fold_left
           (fun acc r ->
             let* acc = acc in
             let* entry = region_entry ~fast ~data:store r in
             Ok (entry :: acc))
           (Ok [])
      |> Result.map (fun l -> Array.of_list (List.rev l))
    in
    (* fast precision: a region bound per call may run on the device (a resident fold's levels), from
     its fast kernel *)
    let fast_kernels =
      Array.mapi
        (fun k (r : C.Region.t) ->
          if (not fast) || fixed_region r then None
          else
            let packed = r in
            Result.to_option
              (Result.map
                 (fun w ->
                   let id = Printf.sprintf "%sf%d" prefix k in
                   kernel id w;
                   id)
                 (Flow_web.Wgsl.kernel_fast packed)))
        program.regions
    in
    (* a mark set's attributes go to the device from a fixed kernel (its inputs export data) or, in fast
     precision, a region bound per call (native fast precision's display regions) *)
    let good, _ =
      Flow_ir.Core_draw.device_regions program ~width:(fun k ->
          if entries.(k).kernel <> None || fast_kernels.(k) <> None then Some entries.(k).width
          else None)
    in
    let groups =
      groups ~fast program entries |> List.map (fun g -> { g with id = prefix ^ g.id })
    in
    let group_of = Hashtbl.create 8 in
    List.iter
      (fun g -> List.iteri (fun output k -> Hashtbl.replace group_of k (g, output)) g.members)
      groups;
    let* js =
      Flow_web.Js.frame ~fast ~kernel:(Array.get fast_kernels) ~state_slot:(Array.get slots)
        ~gpu:(Array.get good)
        ~store:(fun values ->
          let offset, length = store (E.Float_array values) in
          Printf.sprintf "new Float64Array(rt.data, %d, %d)" offset length)
        program
    in
    let region_text k (e : region_entry) =
      match Hashtbl.find_opt group_of k with
      | Some (g, output) ->
          Printf.sprintf "%s, \"kernel\": \"%s\", \"gpu\": {\"output\": %d}}" e.prefix g.id output
      | None ->
          Printf.sprintf "%s, \"kernel\": %s}" e.prefix
            (match e.kernel with Some _ -> Printf.sprintf "\"%sk%d\"" prefix k | None -> "null")
    in
    let regions =
      "export const regions = ["
      ^ String.concat ",\n  "
          (Array.to_list
             (Array.mapi
                (fun k e ->
                  let text = region_text k e in
                  if prefix = "" then text
                  else
                    "{\"id\": "
                    ^ Flow_web.Kernel_table.json_string (Printf.sprintf "%sr%d" prefix k)
                    ^ ", "
                    ^ String.sub text 1 (String.length text - 1))
                entries))
      ^ "];\n"
    in
    let table =
      if groups = [] then ""
      else
        "export const gpu = {"
        ^ String.concat ",\n  "
            (List.map
               (fun g -> Printf.sprintf "\"%s\": {%s}" g.id (fixed_members ~data:shared g.fixed))
               groups)
        ^ "};\n"
    in
    Array.iteri
      (fun k (e : region_entry) ->
        match Hashtbl.find_opt group_of k with
        | Some (g, _) -> if List.hd g.members = k then kernel g.id g.wgsl
        | None -> Option.iter (kernel (Printf.sprintf "%sk%d" prefix k)) e.kernel)
      entries;
    Ok (js ^ "\n" ^ regions ^ table)
  in
  let emit_module name program =
    let* js = emit_program (name ^ ":") program in
    let file = name ^ "-" ^ Digest.to_hex (Digest.string (js ^ Buffer.contents data)) ^ ".js" in
    modules := (file, js) :: !modules;
    imports := Printf.sprintf "import * as %s from \"./%s\";\n" name file :: !imports;
    Ok name
  in
  let image_ids program =
    all_statements program.C.body
    |> List.filter_map (fun (s : C.statement) ->
        match s.node with Draw (Image { source = _, id; _ }) -> Some id | _ -> None)
    |> List.sort_uniq Int.compare
  in
  let live program =
    program.C.state_slots <> [||]
    || List.exists
         (fun (s : C.statement) -> match s.node with Assign (_, Frame _) -> true | _ -> false)
         (all_statements program.body)
    || Array.exists
         (fun (r : C.Region.t) ->
           Array.exists (function C.Region.Frame _ -> true | _ -> false) r.code)
         program.regions
  in
  let images = Hashtbl.create 16 and order = ref [] and assets = ref [] in
  let refuse message = Error (D.error ~span:lowered.span ~code:"E_WEB_IMAGE" message) in
  let rec image active id =
    if List.mem id active then refuse "An image/render dependency cycle cannot be exported."
    else if Hashtbl.mem images id then Ok ()
    else if Hashtbl.length images + List.length active >= 64 then
      refuse "A web workspace owns at most 64 images."
    else
      let node = lowered.plan.nodes.(id) in
      let arg name fallback = Option.value (List.assoc_opt name node.args) ~default:fallback in
      let args values = image_arguments lowered node values in
      let name = Printf.sprintf "image%d" id in
      let* animated, metadata =
        match node.kind with
        | "image/map" ->
            let* args, animated, frames, wgsl = map_entry lowered id in
            let* module_name = emit_module name args in
            kernel name wgsl;
            Ok
              ( animated || live args,
                Printf.sprintf "kind: 'map', kernel: '%s', args: %s, frame: %s" name module_name
                  (list
                     (fun (slot, fact) ->
                       Printf.sprintf "[%d, %s]" slot (Flow_web.Kernel_table.json_string fact))
                     frames) )
        | "image/noise" ->
            let* args =
              args
                [
                  arg "width" (Flow.Eval.Int 256);
                  arg "height" (Int 256);
                  arg "frequency" (arg "freq" (Float 0.02));
                  arg "seed" (Int 0);
                ]
            in
            let* module_name = emit_module name args in
            Ok (live args, "kind: 'noise', args: " ^ module_name)
        | "image/render" ->
            let* drawing, _ =
              Flow_ir.Core_draw.lower lowered.plan (arg "drawing" (Flow.Eval.Int 0))
              |> Result.map_error (fun message ->
                  D.error ~span:lowered.span ~code:"E_WEB_IMAGE"
                    ("Image drawing does not lower to the core: " ^ message))
            in
            let* () =
              List.fold_left
                (fun acc child ->
                  let* () = acc in
                  image (id :: active) child)
                (Ok ()) (image_ids drawing)
            in
            let* args =
              image_arguments ~canvas:true lowered node
                [ arg "width" (Flow.Eval.Int 0); arg "height" (Int 0) ]
            in
            let* args_name = emit_module name args in
            let* drawing_name = emit_module ("drawing" ^ string_of_int id) drawing in
            let animated =
              live args || live drawing
              || List.exists (fun child -> fst (Hashtbl.find images child)) (image_ids drawing)
            in
            Ok
              ( animated,
                Printf.sprintf "kind: 'render', args: %s, drawing: %s" args_name drawing_name )
        | "image/load" -> (
            match arg "path" (Flow.Eval.Int 0) with
            | Flow.Eval.Text path ->
                let* decoded =
                  Sdl3_image.load_file path
                  |> Result.map_error (fun e ->
                      D.error ~span:lowered.span ~code:"E_WEB_IMAGE"
                        (Format.asprintf "Cannot bundle image %S: %a" path Sdl3_image.pp_error e))
                in
                assets := (id, decoded) :: !assets;
                Ok (false, "kind: 'load'")
            | _ -> refuse "A standalone export needs image/load paths known at export time.")
        | kind -> refuse ("Unknown image producer: " ^ kind)
      in
      Hashtbl.add images id (animated, metadata);
      order := id :: !order;
      Ok ()
  in
  let* () =
    List.fold_left
      (fun acc id ->
        let* () = acc in
        image [] id)
      (Ok ()) (image_ids lowered.program)
  in
  let* js = emit_program "" lowered.program in
  (* Native-decoded bytes preserve every supported codec, EXIF orientation and straight alpha.
     Store them after all f64 constants so every constant view remains aligned to eight bytes. *)
  List.iter
    (fun (id, (decoded : Sdl3.rgba)) ->
      while Buffer.length data mod 8 <> 0 do
        Buffer.add_char data '\000'
      done;
      let offset = Buffer.length data in
      Buffer.add_bytes data decoded.pixels;
      let animated, metadata = Hashtbl.find images id in
      Hashtbl.replace images id
        ( animated,
          metadata
          ^ Printf.sprintf ", offset: %d, length: %d, width: %d, height: %d" offset
              (Bytes.length decoded.pixels) decoded.width decoded.height ))
    (List.rev !assets);
  let image_table =
    if Hashtbl.length images = 0 then ""
    else
      "export const images = {"
      ^ String.concat ",\n  "
          (List.map
             (fun id ->
               let animated, metadata = Hashtbl.find images id in
               Printf.sprintf "'%d': {%s, animated: %b}" id metadata animated)
             (List.rev !order))
      ^ "};\n"
  in
  let name = Doc.name doc in
  Ok
    ([
       ("program.js", String.concat "" (List.rev !imports) ^ js ^ image_table);
       ("program.map", Flow_web.Js.frame_map lowered.program);
       ( "kernels.json",
         Flow_web.Kernel_table.table
           ~compaction:(List.assoc "compact.wgsl" Web_shaders.files)
           ~dispatch:(List.assoc "dispatch.wgsl" Web_shaders.files)
           (List.map fst !kernels) );
       ("kernels.wgsl", String.concat "\n" (List.map snd !kernels));
       ("data.bin", Buffer.contents data);
       ("index.html", index_html name);
       (* npx serve reads its headers from here: the worker pool's cross-origin isolation *)
       ( "serve.json",
         {|{"headers": [{"source": "**/*", "headers": [
  {"key": "Cross-Origin-Opener-Policy", "value": "same-origin"},
  {"key": "Cross-Origin-Embedder-Policy", "value": "require-corp"}]}]}
|}
       );
     ]
    @ List.rev !modules
    @ List.map (fun (file, text) -> ("runtime/" ^ file, text)) Web_runtime.files)

let load path =
  let read p = In_channel.with_open_bin p In_channel.input_all in
  let text = read path in
  let imports =
    match Doc.import_paths text with
    | Ok relative ->
        Ok (List.map (fun r -> (r, read (Filename.concat (Filename.dirname path) r))) relative)
    | Error ds -> Error ds
    | exception Sys_error message -> Error [ D.error ~code:"E_IMPORT" message ]
  in
  ( text,
    match imports with
    | Error ds -> Error ds
    | Ok imports -> (
        match
          Editor_document.Kinds.catalog ~version:Flow_sop.Manifest.version Sop.Nodes.factories
        with
        | Error d -> Error [ d ]
        | Ok catalog -> Result.map fst (Doc.check_text ~imports catalog text)) )
