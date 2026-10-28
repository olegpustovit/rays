module E = Flow.Eval
module C = Core

let device_regions (program : C.program) ~width =
  let widths = Array.init (Array.length program.regions) width in
  let uses = Array.make (Array.length program.locals) 0
  and writes = Array.make (Array.length program.locals) 0
  and attributes = Array.make (Array.length program.locals) 0
  and producers = Array.make (Array.length program.locals) (-1) in
  let bump counts local = counts.(local) <- counts.(local) + 1 in
  let invokes = ref [] and batches = ref [] in
  C.iter (fun (statement : C.statement) ->
    List.iter (bump uses) (C.reads statement);
    List.iter (bump writes) (C.writes statement);
    match statement.node with
    | Invoke {region; result; _} ->
        producers.(result) <- region;
        invokes := (region, result) :: !invokes
    | Draw (Batch {mark; args}) -> batches := (mark, args) :: !batches
    | _ -> ()) program.body;
  Array.iter (bump uses) program.results;
  let attribute mark name width = match mark, name, width with
    | (C.Circle | Rect), "positions", (2 | 3)
    | Line, ("from" | "to"), (2 | 3)
    | Circle, "radius", 1 | Rect, "sizes", (2 | 3)
    | (Circle | Rect), ("fill" | "stroke"), 3 | Line, "color", 3 -> true
    | _ -> false in
  List.iter (fun (mark, args) ->
    List.iter (fun (name, local) ->
      let region = producers.(local) in
      if region >= 0 then Option.iter (fun width ->
        if attribute mark name width then bump attributes local) widths.(region)) args)
    !batches;
  let eligible = Array.make (Array.length program.regions) false in
  List.iter (fun (region, _) -> eligible.(region) <- widths.(region) <> None) !invokes;
  List.iter (fun (region, local) ->
    if writes.(local) <> 1 || uses.(local) = 0 || uses.(local) <> attributes.(local) then
      eligible.(region) <- false) !invokes;
  let batches = List.rev !batches |> List.filter_map (fun (_, args) ->
    let regions = List.fold_left (fun regions (_, local) ->
      let region = producers.(local) in
      if region >= 0 && eligible.(region) && not (List.mem region regions)
      then region :: regions else regions) [] args |> List.rev in
    if regions = [] then None else Some regions) in
  eligible, batches

type group = { members : int list; results : int list; args : int list; region : C.Region.t }

let static_invokes (program : C.program) =
  let writes = Array.make (Array.length program.locals) 0
  and nested = Array.make (Array.length program.locals) false in
  let mark top statement =
    let slots = C.writes statement @ (match statement.C.node with State_write {value; _} -> [value] | _ -> []) in
    List.iter (fun slot -> writes.(slot) <- writes.(slot) + 1;
      if not top then nested.(slot) <- true) slots in
  List.iter (fun statement -> mark true statement; C.iter (mark false) (C.children statement)) program.body;
  let static = Array.make (Array.length program.locals) false in
  let once a = writes.(a) = 1 && not nested.(a) in
  let constant : C.expr -> bool = function
    | Constant _ -> true
    | Local a | Field (a, _) -> static.(a)
    | Prim (op, args) -> op <> Rand && List.for_all (fun (_, a) -> static.(a)) args
    | Vec ns | Items ns -> List.for_all (Array.get static) ns
    | Fields fs -> List.for_all (fun (_, a) -> static.(a)) fs
    | Frame _ | Call_index _ | Source _ | Host_source _ | Source_descriptor _ -> false in
  List.iter (fun (s : C.statement) -> match s.node with
    | Assign (a, e) when once a && constant e -> static.(a) <- true
    | Invoke {region; args; result} when once result && List.for_all (Array.get static) args ->
        let r = program.regions.(region) in
        static.(result) <- not (E.frame_dependent (E.Residual r.residual))
          && not (E.state_dependent (E.Residual r.residual))
          && Array.for_all (fun (source, _) -> not (E.frame_dependent (E.Residual source))) r.sources
    | _ -> ()) program.body;
  let invokes = Array.make (Array.length program.locals) false in
  List.iter (fun (s : C.statement) -> match s.node with
    | Invoke {result; _} -> invokes.(result) <- static.(result) | _ -> ()) program.body;
  invokes

(* Groups share a statement block and cross only infallible scalar setup. Delaying the
   earlier invocations to the last therefore preserves branches, side effects and errors. *)
let device_groups ?(approx=Flow.Workspace.Paths.empty) ?(fast=false) program ~width ~legal =
  let eligible, batches = device_regions program ~width in
  if batches=[] then eligible,[] else
  let info = Array.make (Array.length program.regions) None
  and occurrences = Array.make (Array.length program.regions) 0
  and writes = Array.make (Array.length program.locals) 0 in
  let rec block statements =
    let body = Array.of_list statements in
    Array.iteri (fun index (s : C.statement) ->
      List.iter (fun local -> writes.(local) <- writes.(local) + 1) (C.writes s);
      (match s.node with
       | Invoke {region; args; result} ->
           occurrences.(region) <- occurrences.(region) + 1;
           info.(region) <- Some (body, index, args, result)
       | _ -> ());
      match s.node with
      | If {yes; no; _} -> block yes; block no
      | Protect {body; handler; _} -> block body; block handler
      | Counted {init; body; _} -> block init; block body
      | Iterate {body; _} -> block body
      | State_read {init; _} -> block init
      | Diagnose_rows {handler; _} -> block handler
      | _ -> ()) body in
  block program.C.body;
  let static = static_invokes program in
  let rate k = match info.(k) with Some (_, _, _, result) -> static.(result) | None -> false in
  let precision k = fast ||
    let _, site, _ = Packed.site program.regions.(k) in Flow.Workspace.Paths.mem site approx in
  let single k = match info.(k) with
    | Some (_, _, args, result) -> {members=[k]; results=[result]; args; region=program.regions.(k)}
    | None -> assert false in
  let merge members =
    let first, _, _, _ = Option.get info.(List.hd members) in
    if not (List.for_all (fun k -> occurrences.(k) = 1 &&
      match info.(k) with Some (body, _, _, _) -> body == first | None -> false) members)
    then None else
    let members = List.sort (fun a b ->
      let _, a, _, _ = Option.get info.(a) and _, b, _, _ = Option.get info.(b) in compare a b) members in
    let args = List.concat_map (fun k -> let _, _, args, _ = Option.get info.(k) in args) members
      |> List.sort_uniq Int.compare in
    let indices = Hashtbl.create (List.length args) in
    List.iteri (fun index local -> Hashtbl.add indices local index) args;
    let at k = let _, index, _, _ = Option.get info.(k) in index in
    let first_index = at (List.hd members) and last_index = at (List.hd (List.rev members)) in
    let safe = ref true in
    for index = first_index to last_index do
      match first.(index).C.node with
      | Invoke {region; _} when List.mem region members -> ()
      | Assign (local, (Constant _ | Frame _ | Local _))
        when not (List.mem local args) || writes.(local) = 1 -> ()
      | _ -> safe := false
    done;
    if not !safe then None else
    let normalized = List.map (fun k ->
      let _, _, locals, _ = Option.get info.(k) in
      Packed.Host.bind_arguments ~validate:false program.regions.(k)
        (Array.of_list (List.map (fun local -> E.Source ("$core/argument", program.locals.(local),
          ["index", E.Int (Hashtbl.find indices local)])) locals))) members in
    if List.exists Result.is_error normalized then None else
    Option.bind (Core_fuse.merge (List.map Result.get_ok normalized)) (fun region ->
      if not (legal region) then None else
      Some {members; args; region; results=List.map (fun k -> let _, _, _, result = Option.get info.(k) in result) members}) in
  let taken = Array.make (Array.length program.regions) false in
  let groups = List.concat_map (fun members ->
    let members = List.filter (fun k -> not taken.(k)) members in
    List.iter (fun k -> taken.(k) <- true) members;
    List.concat_map (fun (is_static, approximate) ->
      match List.filter (fun k -> rate k = is_static && precision k = approximate) members with
      | [] -> []
      | [k] -> [single k]
      | members -> (match merge members with Some group -> [group] | None -> List.map single members))
      [true, false; true, true; false, false; false, true]) batches in
  eligible, groups

(* The lowering rules of the draw operators. A rule names the inputs it needs, supplies the
   defaults (white ink and fill, width 1) and writes statements; every check of a value is the
   executor's. *)
let white = E.Vec3 (1., 1., 1.)

exception Missing of string
exception Refused of string

let rec find_residual : E.value -> E.residual option = function
  | E.Residual r -> Some r
  | List xs -> Array.find_map find_residual xs
  | Record fields | Struct (_, _, fields) -> List.find_map (fun (_, v) -> find_residual v) fields
  | _ -> None

let lower ?region_allowed (plan : E.plan) (value : E.value) =
  let root = find_residual value |> function
    | Some r -> Some r
    | None -> Array.find_map (fun (n : E.node) -> List.find_map (fun (_, v) -> find_residual v) n.args) plan.nodes in
  let build (ctx : Core_lower.draw_context) =
    let none : C.source = {site = []; owner = []; instance = 0; prefix = []; iter = []; span = None} in
    let fail source code message =
      let n = ctx.local Flow.Ty.Any in
      ctx.emit source (C.Assign (n, Prim (Fail (code, message), []))) in
    let rec drawing = function
      | E.List xs -> Array.iter drawing xs
      | E.Deferred (Flow.Ty.Named "drawing", id) when id >= 0 && id < Array.length plan.nodes -> node plan.nodes.(id)
      | E.Residual _ -> raise (Refused "draw_dynamic")
      | _ -> fail none "E_TYPE" "The canvas needs a Drawing value."
    and node (n : E.node) =
      let source : C.source = {site = n.site; owner = n.site; instance = n.inst; prefix = [];
        iter = List.map (fun i -> C.Static_index i) n.iter; span = None} in
      if n.ty <> Flow.Ty.drawing then fail source "E_TYPE" "The canvas needs a Drawing node." else
      (* The data inputs, in written order, as drawing.ml forces them. *)
      let data = match n.kind with
        | "draw/merge" -> []
        | "draw/translate" | "draw/rotate" | "draw/scale" -> List.filter (fun (k, _) -> k <> "drawing") n.args
        | "draw/image" -> List.filter (fun (k, _) -> k <> "image") n.args
        | _ -> n.args in
      let locals = List.map (fun (k, v) -> k, ctx.value source v) data in
      let need key = match List.assoc_opt key locals with
        | Some n -> n | None -> raise (Missing (Printf.sprintf "%s needs %s." n.kind key)) in
      let optional key = List.assoc_opt key locals in
      let default key v = match optional key with Some n -> n | None -> ctx.value source v in
      let emit d = ctx.emit source (C.Draw d) in
      let style () =
        let fill = optional "fill" and stroke = optional "stroke" in
        let fill = if fill = None && stroke = None then Some (ctx.value source white) else fill in
        List.filter_map (fun (k, v) -> Option.map (fun n -> k, n) v) ["fill", fill; "stroke", stroke] in
      let push transform key =
        let operand = need key in
        emit (C.Push (transform, operand));
        (match List.assoc_opt "drawing" n.args with
         | Some v -> drawing v
         | None -> raise (Missing (Printf.sprintf "%s needs drawing." n.kind)));
        emit C.Pop in
      try match n.kind with
        | "draw/background" -> emit (Background (need "color"))
        | "draw/point" ->
            let at = need "at" in emit (Point {at; ink = default "color" white})
        | "draw/rect" ->
            let at = need "at" in let size = need "size" in
            emit (Mark {mark = Rect; args = ["at", at; "size", size] @ style ()})
        | "draw/circle" ->
            let at = need "at" in let radius = need "radius" in
            emit (Mark {mark = Circle; args = ["at", at; "radius", radius] @ style ()})
        | "draw/line" ->
            let from = need "from" in let dest = need "to" in
            emit (Mark {mark = Line; args = ["from", from; "to", dest;
              "color", default "color" white; "width", default "width" (E.Float 1.)]})
        | "draw/circles" ->
            let positions = need "positions" in
            emit (Batch {mark = Circle; args = ["positions", positions;
              "radius", default "radius" (E.Float 1.)] @ style ()})
        | "draw/rects" ->
            let positions = need "positions" in let sizes = need "sizes" in
            emit (Batch {mark = Rect; args = ["positions", positions; "sizes", sizes] @ style ()})
        | "draw/lines" ->
            let from = need "from" in let dest = need "to" in
            emit (Batch {mark = Line; args = ["from", from; "to", dest;
              "color", default "color" white; "width", default "width" (E.Float 1.)]})
        | "draw/points" ->
            let positions = need "positions" in emit (Points {positions; ink = default "color" white})
        | "draw/text" ->
            let at = need "at" in let text = need "text" in
            emit (Text_run {at; text; ink = default "color" white; size = default "size" (E.Int 16)})
        | "draw/image" ->
            let source = match List.assoc_opt "image" n.args with
              | Some (E.Deferred (ty, id)) -> ty, id
              | Some _ -> raise (Refused "draw_image")
              | None -> raise (Missing (Printf.sprintf "%s needs image." n.kind)) in
            emit (Image {source; at = default "at" (E.Vec3 (0., 0., 0.));
              scale = default "scale" (E.Float 1.); angle = default "angle" (E.Float 0.)})
        | "draw/rounded_rect" ->
            let at = need "at" in let size = need "size" in
            emit (Path {path = Rounded_rect; args = ["at", at; "size", size;
              "radius", default "radius" (E.Int 0)] @ style ()})
        | "draw/polygon" ->
            let points = need "points" in emit (Path {path = Polygon; args = ["points", points] @ style ()})
        | "draw/polyline" ->
            let points = need "points" in
            emit (Path {path = Polyline; args = ["points", points; "color", default "color" white]})
        | "draw/translate" -> push C.Translate "offset"
        | "draw/rotate" -> push C.Rotate "angle"
        | "draw/scale" -> push C.Scale "factor"
        | "draw/merge" -> List.iter (fun (_, v) -> drawing v) n.args
        | kind -> fail source "E_DRAW_KIND" (Printf.sprintf "No native drawing implementation for %s." kind)
      with Missing message -> fail source "E_DRAW_INPUT" message in
    drawing value in
  match Core_lower.lower_draw ?region_allowed ~root:(lazy (Option.get root)) build with
  | Ok program -> Ok (program, root)
  | Error name -> Error name
  | exception Refused name -> Error name
