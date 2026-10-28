(* A Lisp entry compiled as a stage: one Packed program of the entry over its element parameters, an
   output per leaf of its result record. Its array parameters are buffers (Raster's bindings). *)
module E = Flow.Eval
module W = Flow.Workspace

let fail message = Error (Diagnostic.error ~code:"E_RASTER" message)
let ( let* ) = Result.bind
let all results = List.fold_right (fun r acc -> Result.bind acc (fun acc -> Result.map (fun x -> x :: acc) r)) results (Ok [])

(* The entry's parameters, read from its authored [defn]. *)
let parameters forms entry =
  let rec find = function
    | [] -> None
    | ({ Flow.Syntax.node = List ({ node = Sym "defn"; _ } :: { node = Sym name; _ } :: rest); _ } : Flow.Syntax.t) :: _
      when name = entry ->
        List.find_map (fun (x : Flow.Syntax.t) -> match x.node with Vec params -> Some params | _ -> None) rest
    | { node = List items; _ } :: more -> (match find items with Some _ as found -> found | None -> find more)
    | _ :: more -> find more in
  match find forms with
  | None -> fail ("no defn " ^ entry)
  | Some params -> all (List.map (fun (p : Flow.Syntax.t) -> match p.node with
      | List [ { node = Sym name; _ }; { node = Sym ":"; _ }; ty ] ->
          (match Flow.Ty.of_syntax ty with Some ty -> Ok (name, ty)
           | None -> fail (entry ^ ": parameter " ^ name ^ " has no stage type"))
      | _ -> fail (entry ^ ": every stage parameter is typed: " ^ fst (Flow.Lisp.print [ p ]))) params)

(* The [index]th array parameter is a host buffer bound at [buffer(index)]. Before compiling, its
   placeholder becomes a distinct (index + 1)-element array, so the template keeps it a gather table
   (never a folded constant) and a gather's uniform names its buffer by length. *)
let dummy index = function
  | Flow.Ty.Array Int -> Ok (E.Int_array (Array.make (index + 1) 0))
  | Array Float -> Ok (E.Float_array (Array.make (index + 1) 0.))
  | ty -> fail ("a stage buffer is (array int) or (array float), not " ^ Flow.Ty.to_string ty)
let column = function
  | Flow.Ty.Int -> E.Int_array [| 0 |] | Float -> E.Float_array [| 0. |]
  | Vec2 -> E.Vec2_array [| 0.; 0. |] | Vec3 -> E.Vec3_array [| 0.; 0.; 0. |]
  | Vec4 -> E.Vec4_array [| 0.; 0.; 0.; 0. |] | _ -> invalid_arg "Raster.column"
let is_buffer (_, ty) = match ty with Flow.Ty.Array _ -> true | _ -> false

(* A stage output: a leaf of the entry's result, in field order (the program's outputs), [flat] when
   it sits under [:flat]. *)
type leaf = { path : string list; ty : Flow.Ty.t; flat : bool }
type stage = { params : (string * Flow.Ty.t) list; leaves : leaf list; program : Flow_ir.Packed.t;
  bindings : (string * int) list }

let rec leaves ?(flat = false) path = function
  | Flow.Ty.Record fields -> List.concat_map (fun (name, ty) ->
      leaves ~flat:(flat || name = "flat") (path @ [ name ]) ty) fields
  | ty -> [ { path; ty; flat } ]

(* A vertex stage's buffers bind in their order; a fragment buffer named like a vertex one shares its
   binding, any other follows the vertex stage's. *)
let compile ?(shared = []) ~declarations entry =
  let* forms = Flow.Syntax.parse declarations in
  let* params = parameters forms entry in
  let buffers = List.filter is_buffer params and elements = List.filter (fun p -> not (is_buffer p)) params in
  let next = ref (List.length shared) in
  let bindings = List.map (fun (name, _) -> name, match List.assoc_opt name shared with
    | Some slot -> slot | None -> let slot = !next in incr next; slot) buffers in
  let ops = List.mapi (fun index (_, ty) -> let name = "raster/buffer" ^ string_of_int index in
    { Flow.Op.noise3 with name; packed_extension = None; signature = { pos = []; opt = []; rest = None; kw = [] };
      out = (fun _ -> ty); shape = Flow.Op.Struct { splice = false }; check = (fun _ -> ());
      body = (fun ~live:_ ~node:_ _ -> Flow.Value.Source (name, ty, [])) }) buffers in
  let text = Printf.sprintf "(workspace raster %s (graph g :context value (let* [%s kernel (fn [%s] (%s %s))] 0.0)))"
    declarations (String.concat " " (List.mapi (fun index (name, _) -> Printf.sprintf "%s (raster/buffer%d)" name index) buffers))
    (String.concat " " (List.map (fun (name, ty) -> Printf.sprintf "(%s : %s)" name (Flow.Ty.to_string ty)) elements))
    entry (String.concat " " (List.map fst params)) in
  let* forms = Flow.Syntax.parse text in
  let* fn = match Flow.Workspace.check ~ops { Flow.Catalog.version = 1; kinds = [] } forms with
    | Some ws, [] ->
        let* evaluated = E.static ~record:true ws in
        (match snd (List.hd (List.assoc [ "g"; "kernel" ] evaluated.records)) with
         | E.Fn fn -> Ok fn | _ -> fail (entry ^ ": the stage is not a function"))
    | _, d :: _ -> Error d | _, [] -> fail (entry ^ ": unchecked") in
  let result = match E.Residual.function_body fn with Some (_, body) -> body.W.ty | None -> Flow.Ty.Any in
  let* bound = all (List.map (fun (name, ty) -> dummy (List.assoc name bindings) ty) buffers) in
  let fn = E.bind_captures (function
    | Flow.Value.Source (source, _, []) when String.starts_with ~prefix:"raster/buffer" source ->
        List.nth bound (int_of_string (String.sub source 13 (String.length source - 13)))
    | value -> value) fn in
  let* residual = E.map_kernel ~signature:{ params = List.map snd elements; result } fn
    (List.map (fun (_, ty) -> column ty) elements) in
  match residual with
  | E.Residual r -> (match Flow_ir.Packed.compile_template r (E.Residual.residual_view r).term with
      | Some program -> Ok { params; leaves = leaves [] result; program; bindings }
      | None -> (match Flow_ir.Packed.compile_result r (E.Residual.residual_view r).term with
          | Error (d :: _) -> Error d | _ -> fail (entry ^ " does not compile to a packed program")))
  | _ -> fail (entry ^ ": the stage did not defer")

