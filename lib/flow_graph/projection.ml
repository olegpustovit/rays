module S = Flow.Syntax
module W = Flow.Workspace
module Ty = Flow.Ty
module E = Flow_edit

type path = W.path
type chip = No_value | Const | Name of string | Inline of { glyph : string; text : string }
type row_kind = Arg | Rest | Add | Hole | Binder | Group_reader | Group_writer

type control = Plain | Range of float * float | Choice

type row = {
  label : string; key : E.arg_key; ty : Ty.t option; expr : S.t option; chip : chip;
  default : string option; socket : bool; kind : row_kind; control : control;
  folder : string;  head : bool;
}

type zone_kind = For | Fold | Scan | Sum | Let | Fn | State | Branch
type role = Var | Acc | Param | Capture

type rail_row = {
  name : string; names : string list; role : role; ty : Ty.t option;
  expr : S.t option; key : E.arg_key option;
}

type input = { path : path; name : string; ty : Ty.t; default : S.t option }

type lens = { steps : string array; error : string option; template : string }

type node = {
  path : path; name : string; binds : string list; head : string; rows : row list;
  outputs : (string * Ty.t) list; ty : Ty.t; note : string option; bypass : bool;
  macro : string option; lens : lens option; live : bool; invariant : bool; synthetic : bool;
  zone : zone option; expr : bool;
}
and zone = { kind : zone_kind; rail : rail_row list; scope : scope; yield_label : string;
             order : string option }
and scope = { path : path; inputs : input list; nodes : node list; result : result }
and result = Link of string | Node of path | Literal of S.t

(* the call, then each [expand_once] of it (at most 12 steps, like the study's lens) *)
let macro_lens (w : W.t) (call : S.t) =
  let state = Flow.Macro.state () in
  let text f = let t = fst (Flow.Lisp.print [ f ]) in
    if String.ends_with ~suffix:"\n" t then String.sub t 0 (String.length t - 1) else t in
  let rec go cur acc n =
    if n = 12 then acc, None else
    match Flow.Macro.expand_once ~state w.macros cur with
    | Error (d : Diagnostic.t) -> acc, Some d.message
    | Ok next -> if next == cur then acc, None else go next (text next :: acc) (n + 1) in
  let steps, error = go call [ text call ] 0 in
  let template = match S.head call with
    | Some name -> (match List.find_opt (fun (m : S.t) -> match S.children m with
        | _ :: { S.node = S.Sym n; _ } :: _ -> n = name | _ -> false) w.macros with
        | Some m -> text m | None -> "")
    | None -> "" in
  { steps = Array.of_list (List.rev steps); error; template }

(* ---- what the projection reads ---- *)

type cx = {
  w : W.t;
  catalog : Flow.Catalog.catalog;
  ctx : W.context;
  macros : (string * S.t) list;
  terms : (path, W.term) Hashtbl.t;  (* every bound term of the graph, by path *)
  folded : path -> bool;  (* an operator call written in an input stays text while its leaf is folded *)
  forms : (S.id, W.term) Hashtbl.t;  (* every term, by the form it checks: a nested node has no path *)
}

let pairs = S.pairs
let head_sym = S.head
let last (e : S.t) = List.nth (S.children e) (List.length (S.children e) - 1)
let root_of s = match String.index_opt s '.' with Some i -> String.sub s 0 i | None -> s
let is_zone_head = function "for" | "fold" | "scan" | "sum" | "state" -> true | _ -> false

let subterms (term : W.term) = match term.node with
  | W.Call_fn {args; _} -> args
  | W.Fn {body; _} -> [body]
  | _ -> W.children term

let rec collect tbl forms (t : W.term) =
  Option.iter (fun p -> Hashtbl.replace tbl p t) t.path;
  Hashtbl.replace forms t.form.id t;
  List.iter (collect tbl forms) (subterms t)

let same_graph (a : W.t) (b : W.t) name =
  if a == b then true else
  let graph workspace =
    match List.find_opt (fun (g : W.graph) -> g.name = name) workspace.W.graphs with
    | Some _ as graph -> graph
    | None -> List.find_opt (fun (g : W.graph) -> name = "def:" ^ g.name || name = g.name) workspace.defs in
  match graph a, graph b with
  | Some x, Some y when a.ops == b.ops && x.context = y.context && S.equal x.form y.form
      && List.equal S.equal a.macros b.macros
      && List.equal (fun (a : W.node_definition) (b : W.node_definition) ->
        a.kind = b.kind && a.fields = b.fields) a.nodes b.nodes ->
      let rec same (a : W.term) (b : W.term) = a.path = b.path && a.ty = b.ty
        && List.equal same (subterms a) (subterms b) in
      let root = if String.starts_with ~prefix:"def:" name then name
        else if List.exists (fun (g : W.graph) -> g == x) a.defs then "def:" ^ name else name in
      let under = function first :: _ -> first = root | [] -> false in
      let same_paths a b = W.Paths.for_all (fun path -> not (under path) || W.Paths.mem path b) a
        && W.Paths.for_all (fun path -> not (under path) || W.Paths.mem path a) b in
      same x.body y.body
      && List.equal (fun (n,a,x) (m,b,y) -> n = m && a = b && Option.equal same x y) x.inputs y.inputs
      && same_paths a.live b.live && same_paths a.invariant b.invariant
  | _ -> false

(* ---- chips and rows ---- *)

let chip c (e : S.t option) = match e with
  | None -> No_value
  | Some e ->
      let inline glyph = Inline { glyph; text = Flow.Lisp.flat e } in
      (match e.node with
       | S.Sym ("t" | "pi" | "true" | "false" | "nil") -> Const
       | S.Sym s -> Name s
       | S.Num _ | S.Str _ | S.Vec _ | S.Kw _ | S.Quote _ -> Const
       | S.Map _ -> inline "{}"
       | S.List _ ->
           (match head_sym e with
            | Some ("for") -> inline "for"
            | Some "sum" -> inline "Σ"
            | Some ("fold" | "scan") -> inline "⟲"
            | Some "fn" -> inline "λ"
            | Some "iterate" -> inline "⟲"
            | Some ("if" | "cond" | "case") -> inline "?"
            | Some h when List.mem_assoc h c.macros -> inline "◆"
            | _ -> inline "ƒ"))

let row c ?ty ?default ?(socket = true) ?(kind = Arg) ?(control = Plain) ?(folder = "") ?primary:_
    ?(head = false) label key expr =
  { label; key; ty; expr; chip = chip c expr; default; socket; kind; control; folder;  head }

let add c label key ?(socket = true) ty =
  row c ?ty ~socket ~kind:Add label key None

let ty_of_port = function
  | Flow.Port_type.Geometry -> Flow.Ty.geometry | Float -> Ty.Float | Int -> Ty.Int
  | Bool -> Ty.Bool | Vec3 -> Ty.Vec3
  | Image -> Ty.image | Fn signature -> Ty.Fn (Some signature)

let show_value = function
  | Param.Bool_value b -> string_of_bool b
  | Int_value i -> string_of_int i
  | Float_value f -> Printf.sprintf "%g" f
  | Text_value s | Choice_value s -> s

(* what a parameter's field is: a soft range paints a position line, a choice a chevron *)
let control_of (p : Flow.Catalog.parameter) = match p.fields with
  | [ (_, Param.Floating_view r, _) ] -> Range (r.soft_min, r.soft_max)
  | [ (_, Param.Integer_view r, _) ] -> Range (float r.soft_min, float r.soft_max)
  | [ (_, Param.Choice_view _, _) ] -> Choice
  | _ -> Plain

let default_text (p : Flow.Catalog.parameter) = match p.fields with
  | [ (_, _, v) ] -> Some (show_value v)
  | [ _; _; _ ] as fs -> Some ("[" ^ String.concat " " (List.map (fun (_, _, v) -> show_value v) fs) ^ "]")
  | _ -> None

(* a macro parameter the template uses as a loop or scope name *)
let rec binder p (e : S.t) =
  let unquoted (x : S.t) = match x.node with
    | S.Quote (S.Unquote, { S.node = S.Sym n; _ }) -> n = p | _ -> false in
  (match e.node with
   | S.List ({ S.node = S.Sym h; _ } :: vs) when is_zone_head h || h = "let*" ->
       List.exists (fun (v : S.t) -> match v.node with
         | S.Vec l -> List.exists (fun (b, _) -> unquoted b) (pairs l) | _ -> false) vs
   | _ -> false)
  || List.exists (binder p) (S.children e)

let macro_rows c (m : S.t) pos =
  let npos = List.length pos in
  match Flow.Macro.params m with
  | Error _ -> []
  | Ok (req, rest) ->
      let template = last m in
      let req_rows = List.mapi (fun i p ->
        row c ~kind:(if binder p template then Binder else Hole) p (E.Pos i) (List.nth_opt pos i)) req in
      let rest_rows = match rest with
        | None -> []
        | Some r ->
            let n = List.length req in
            List.drop n pos |> List.mapi (fun j a ->
              row c ~kind:Rest (if j = 0 then r else Printf.sprintf "%s %d" r (j + 1)) (E.Pos (n + j)) (Some a))
            |> fun rows -> rows @ [ add c ("+ " ^ r) (E.Pos npos) None ] in
      req_rows @ rest_rows

let kind_rows c (k : Flow.Catalog.kind) pos kws =
  let npos = List.length pos in
  let slot_rows = List.concat (List.mapi (fun i (s : Flow.Catalog.slot) ->
    let slot_ty = W.slot_ty ~slot:s k in
    let default = if slot_ty = Ty.geometry then None else
      Option.map (fun form -> String.trim (fst (Flow.Lisp.print [form]))) (Ty.default slot_ty) in
    (* the first geometry slot is the header's in-port, not a row of the body *)
    let head_of i = i = 0 && slot_ty = Flow.Ty.geometry in
    if s.rest then
      List.drop i pos |> List.mapi (fun j a ->
        row c ~ty:slot_ty ~kind:Rest ~head:(head_of i && j = 0)
          (if j = 0 then s.name else Printf.sprintf "%s %d" s.name (j + 1))
          (E.Pos (i + j)) (Some a))
      |> fun rows -> rows @ (match List.assoc_opt s.name kws with
          | None -> []
          | Some value -> [row c ~ty:slot_ty ~kind:Rest s.name (E.Kw s.name) (Some value)])
          @ [ add c ("+ " ^ s.name) (E.Pos (max npos i)) (Some slot_ty) ]
    else match List.nth_opt pos i, List.assoc_opt s.name kws with
      | Some a, _ -> [ row c ~ty:slot_ty ?default ~head:(head_of i) s.name (E.Pos i) (Some a) ]
      | None, Some a -> [ row c ~ty:slot_ty ?default ~head:(head_of i) s.name (E.Kw s.name) (Some a) ]
      | None, None ->
          [ row c ~ty:slot_ty ?default ~head:(head_of i) s.name (if s.required && i = npos then E.Pos i else E.Kw s.name) None ])
    k.slots) in
  (* a schema with no primary field takes the fields of its first folder as primary *)
  let any_primary = List.exists (fun (p : Flow.Catalog.parameter) -> p.primary) k.parameters in
  let first_folder = match k.parameters with p :: _ -> p.folder | [] -> [] in
  (* the arguments with no folder are the kind's own section, named as the inspector names it *)
  let kind_section (k : Flow.Catalog.kind) =
    String.capitalize_ascii (match String.rindex_opt k.qualified '/' with
      | Some i -> String.sub k.qualified (i + 1) (String.length k.qualified - i - 1) | None -> k.qualified) in
  let param_rows = List.map (fun (p : Flow.Catalog.parameter) ->
    let kind = if W.group_reader p then Group_reader else if W.group_writer k p then Group_writer else Arg in
    let material = k.qualified = "sop/material" && p.name = "material" in
    row c ~ty:(if material then Flow.Ty.material else match p.ty with Some t -> ty_of_port t | None -> Ty.Text) ?default:(default_text p)
      ~control:(control_of p) ~socket:true ~kind
      ~folder:(if p.folder = [] then kind_section k else String.concat " / " p.folder)
      ~primary:(if any_primary then p.primary else p.folder = first_folder)
      p.name (E.Kw p.name) (List.assoc_opt p.name kws)) k.parameters in
  slot_rows @ param_rows

let input_rows c (inputs : (string * Ty.t * W.term option) list) pos kws =
  List.mapi (fun i (n, ty, d) ->
    let default = Option.map (fun (t : W.term) -> Flow.Lisp.flat t.form) d in
    match List.nth_opt pos i with
    | Some a -> row c ~ty n (E.Pos i) (Some a)
    | None -> row c ~ty ?default n (E.Kw n) (List.assoc_opt n kws)) inputs

let call_rows c (e : S.t) h args =
  let args = S.attribute_args h args in
  let pos = E.positional args and kws = E.keywords args in
  let npos = List.length pos in
  let at i = List.nth_opt pos i in
  let posrow ?ty ?socket label i = row c ?ty ?socket label (E.Pos i) (at i) in
  let kwrow ?ty label =
    let choices=W.op_choices ~ops:c.w.ops h label in
    let literal=choices<>[] && Option.fold ~none:false
      ~some:(fun (op:Flow.Op.t)->op.out_args<>None)(Flow.Op.find ~extra:c.w.ops h c.ctx) in
    row c ?ty ~control:(if choices=[] then Plain else Choice) ~socket:(not literal)
      label (E.Kw label) (List.assoc_opt label kws) in
  let items label prefix ty =
    List.mapi (fun i a -> row c ?ty ~kind:Rest (label i) (E.Pos i) (Some a)) pos
    @ [ add c prefix (E.Pos npos) ty ] in
  let list_ty = Some (Ty.List Ty.Any) in
  match h with
  | "with_error" -> [posrow "protected" 0;posrow ~ty:(Ty.Fn None) "mapper" 1]
  | "error_rows" -> [posrow ~ty:(Ty.Array Ty.Int) "rows" 0;posrow ~ty:(Ty.Fn None) "mapper" 1;kwrow ~ty:Ty.Text "policy"]
  | "if" -> [ posrow ~ty:Ty.Bool "if" 0; row c "then" (E.Arm 0) (at 1); row c "else" (E.Arm (-1)) (at 2) ]
  | "list" -> items string_of_int "+ item" None
  | "str" -> items (fun _ -> "part") "+ part" None
  | "concat" -> items (fun i -> Printf.sprintf "list %d" (i + 1)) "+ list" list_ty
  | "cond" ->
      List.concat_map (fun i -> [ posrow ~ty:Ty.Bool "when" i; row c "then" (E.Arm (i / 2)) (at (i + 1)) ])
        (List.filter (fun i -> i mod 2 = 0) (List.init npos Fun.id))
      @ [ row c "else" (E.Arm (-1)) (List.assoc_opt "else" kws) ]
  | "case" ->
      posrow "of" 0
      :: List.concat_map (fun i -> [ posrow "is" i; row c "then" (E.Arm ((i - 1) / 2)) (at (i + 1)) ])
           (List.filter (fun i -> i mod 2 = 1) (List.init npos Fun.id))
      @ [ row c "else" (E.Arm (-1)) (List.assoc_opt "else" kws) ]
  | "map" | "filter" | "reduce" | "sort-by" ->
      let input_ty i fallback = match Hashtbl.find_opt c.forms e.id with
        | Some {W.node = Hof (_, inputs); _} ->
            Option.fold ~none:fallback ~some:(fun (t : W.term) -> Some t.ty) (List.nth_opt inputs i)
        | _ -> fallback in
      let labels = match h with
        | "map" -> [ "f"; "list"; "list 2"; "list 3" ] | "filter" -> [ "keep if"; "list" ]
        | "reduce" -> [ "f"; "start"; "list" ] | _ -> [ "key"; "list" ] in
      List.concat (List.mapi (fun i l ->
        if i < 2 || at i <> None || h = "reduce" then
          [ posrow ?ty:(input_ty i (if i = 0 then Some (Ty.Fn None) else if String.starts_with ~prefix:"list" l then list_ty else None)) l i ]
        else []) labels)
      @ (if h = "map" && npos < 4 then [ add c "+ list" (E.Pos npos) list_ty ] else [])
  | "iterate" ->
      let start = match Hashtbl.find_opt c.forms e.id with
        | Some {W.node = Hof (`Iterate, _ :: (init : W.term) :: _); _} -> Some init.ty
        | _ -> None in
      [ posrow ~ty:Ty.Int "count" 0; posrow ?ty:start "start" 1; posrow ~ty:(Ty.Fn None) "f" 2;
        kwrow ~ty:(Ty.Fn None) "until" ]
  | "get" -> [ posrow "record" 0; posrow ~socket:false "field" 1 ]
  | "assoc" -> posrow "record" 0 :: List.map (fun (k, _) -> kwrow k) kws
  | "values" -> List.map (fun (k, _) -> kwrow k) kws @ [ add c "+ output" (E.Kw "") ~socket:false None ]
  | "ref" ->
      let g = match at 0 with Some { S.node = S.Sym n; _ } -> List.find_opt (fun (g : W.graph) -> g.name = n) c.w.graphs
        | _ -> None in
      row c ~socket:false "graph" (E.Pos 0) (at 0)
      :: (match g with Some g -> input_rows c g.inputs [] kws | None -> [])
  | _ ->
      (match List.find_opt (fun (d : W.graph) -> d.name = h) c.w.defs with
       | Some d -> input_rows c d.inputs pos kws
       | None ->
           match List.assoc_opt h c.macros with
           | Some m -> macro_rows c m pos
           | None ->
               match W.op_signature ~ops:c.w.ops c.ctx h with
               | Some o ->
                   let np = List.length o.pos in
                   List.concat (List.mapi (fun i (l, ty) -> if i < np || i < npos then [ posrow ~ty l i ] else [])
                     (o.pos @ o.opt))
                   @ (match o.rest with
                      | Some (rn, ty) ->
                          List.drop np pos |> List.mapi (fun j a ->
                            row c ~ty ~kind:Rest (if j = 0 then rn else Printf.sprintf "%s %d" rn (j + 1)) (E.Pos (np + j)) (Some a))
                          |> fun rows -> rows @ [ add c ("+ " ^ rn) (E.Pos (max npos np)) (Some ty) ]
                      | None -> [])
                   @ List.map (fun (n, ty) -> kwrow ~ty n) o.kw
               | None ->
                   match Flow.Catalog.resolve_kind c.catalog c.ctx h with
                   | Ok k -> kind_rows c k pos kws
                   | Error _ ->
                       (* a call of a local fn reads the fn: a wire, so a used fn never looks unused *)
                       let callee, names = match Hashtbl.find_opt c.forms e.id, S.children e with
                         | Some { W.node = Call_fn { fn; _ }; _ }, f :: _ ->
                             (* its inputs are named by the fn's parameters.  ponytail: the first fn of
                                that name in the graph, so a shadowed name in a sibling scope reads the other *)
                             let params = Hashtbl.fold (fun (p : path) (t : W.term) found -> match found, t.node with
                               | None, Fn { params; _ } when p <> [] && List.nth p (List.length p - 1) = fn ->
                                   Some (List.map (function (W.Name n, _) -> n | _ -> "") params)
                               | _ -> found) c.terms None in
                             [ row c ~ty:(Ty.Fn None) ~socket:false "fn" (E.Pos (-1)) (Some f) ], Option.value ~default:[] params
                         | _ -> [], [] in
                       callee @ List.mapi (fun i a ->
                         let label = match List.nth_opt names i with Some n when n <> "" -> n | _ -> Printf.sprintf "arg%d" (i + 1) in
                         row c label (E.Pos i) (Some a)) pos)

let rows_of c (e : S.t) = match e.node, head_sym e with
  | S.Map l, _ ->
      List.filter_map (fun ((k : S.t), v) -> match k.node with
        | S.Kw k -> Some (row c k (E.Field k) (Some v)) | _ -> None) (pairs l)
      @ [ add c "+ field" (E.Field "") ~socket:false None ]
  | S.List (_ :: args), Some h -> call_rows c e h args
  | S.Vec _, _ ->
      let ty = Option.map (fun (term : W.term) -> term.ty) (Hashtbl.find_opt c.forms e.id) in
      [row c ?ty "value" E.Whole (Some e)]
  | S.Sym s, _ when not (List.mem s [ "t"; "pi"; "true"; "false"; "nil" ]) -> [ row c "from" E.Whole (Some e) ]
  | _ -> [ row c "value" E.Whole (Some e) ]

let head_label c (e : S.t) = match e.node with
  | S.Map _ -> "record" | S.Num _ -> "number" | S.Str _ -> "text" | S.Sym _ -> "link" | S.Vec _ -> "vector"
  | _ -> Option.value (head_sym e) ~default:(ignore c; "form")

(* ---- nodes, zones and scopes ---- *)

let scope_form (e : S.t) = match e.node, e.meta with
  | S.List [ { S.node = S.Sym "let*"; _ }; { S.node = S.Vec bs; _ }; res ], []
    when List.length bs mod 2 = 0 -> Some (pairs bs, res)
  | _ -> None

let zone_kind (pat : S.t option) (e : S.t) =
  if e.meta <> [] then None
  else match head_sym e, pat with
    | Some "for", _ -> Some For | Some "fold", _ -> Some Fold | Some "scan", _ -> Some Scan
    | Some "sum", _ -> Some Sum
    | Some "state", _ -> Some State
    | Some "let*", _ when scope_form e <> None -> Some Let
    | Some "fn", _ -> Some Fn
    | _ -> None

let outputs (pat : S.t option) ty = match pat with
  | None -> []
  | Some pat ->
      (match pat.node, ty with
       | S.Sym _, Ty.Record fs -> fs
       | S.Sym _, _ -> []
       | S.Vec _, _ ->
           let t = match ty with Ty.List e -> e | Ty.Vec2 | Ty.Vec3 | Ty.Vec4 -> Ty.Float | _ -> Ty.Any in
           List.map (fun n -> n, t) (E.pat_names pat)
       | _ -> List.map (fun n -> n, (match ty with Ty.Record fs -> Option.value (List.assoc_opt n fs) ~default:Ty.Any
                                      | _ -> Ty.Any)) (E.pat_names pat))

let rail_of c (kind : zone_kind) (e : S.t) (t : W.term option) ~visible =
  let bound = ref [] in
  let clause_ty k = match t with
    | Some { W.node = Loop { clauses; _ }; _ } ->
        Option.bind (List.nth_opt clauses k) (fun (_, (ct : W.term)) -> Ty.elem ct.ty)
    | _ -> None in
  let vars first = match List.nth_opt (S.children e) first with
    | Some { S.node = S.Vec l; _ } ->
        List.mapi (fun k ((p : S.t), x) ->
          bound := E.pat_names p @ !bound;
          { name = E.pat_key p; names = E.pat_names p; role = Var; ty = clause_ty k; expr = Some x;
            key = Some (E.Bv (first, 2 * k + 1)) }) (pairs l)
    | _ -> [] in
  let rows = match kind with
    | For | Sum -> vars 1
    | Fold | Scan | State ->
        let acc = match List.nth_opt (S.children e) 1 with
          | Some { S.node = S.Vec [ p; init ]; _ } ->
              bound := E.pat_names p @ !bound;
              let ty = match t with
                | Some { W.node = Loop { accs = (_, (a : W.term)) :: _; _ }; _ } -> Some a.ty
                | Some { W.node = State {init; _}; _} -> Some init.ty
                | _ -> None in
              [ { name = E.pat_key p; names = E.pat_names p; role = Acc; ty; expr = Some init;
                  key = Some (E.Bv (1, 1)) } ]
          | _ -> [] in
        acc @ (if kind = State then [] else vars 2)
    | Fn ->
        (match List.nth_opt (S.children e) 1 with
         | Some { S.node = S.Vec ps; _ } ->
             List.mapi (fun k (p : S.t) ->
               let pat = match p.node with S.List [ pat; { S.node = S.Sym ":"; _ }; _ ] -> pat | _ -> p in
               bound := E.pat_names pat @ !bound;
               let ty = match t with
                 | Some ({ W.node = Fn { params; zone = path; _ }; _ } as fn) ->
                     (match Option.bind (List.nth_opt params k) snd with
                      | Some _ as ty -> ty
                      | None when (match fn.ty with Ty.Fn (Some _) -> true | _ -> false) ->
                          (match fn.ty with Ty.Fn (Some signature) -> List.nth_opt signature.params k
                            | _ -> None)
                      | None ->
                          let parent = match List.rev path with
                            | leaf :: scope -> Option.map (fun i -> List.rev scope @ [String.sub leaf 0 i])
                                (String.rindex_opt leaf '#')
                            | [] -> None in
                          Option.bind parent (fun parent ->
                            match Hashtbl.find_opt c.terms parent with
                            | Some {W.node = Hof (`Iterate, f :: init :: _ :: until); _}
                              when f.form.id = fn.form.id
                                || List.exists (fun (u : W.term) -> u.form.id = fn.form.id) until ->
                                Some init.ty
                            | Some {W.node = Hof (`Reduce, f :: init :: source :: _); _} when f.form.id = fn.form.id ->
                                if k = 0 then Some init.ty else Ty.elem source.ty
                            | Some {W.node = Hof (_, f :: sources); _} when f.form.id = fn.form.id ->
                                Option.bind (List.nth_opt sources k) (fun (source : W.term) -> Ty.elem source.ty)
                            | _ -> None))
                 | _ -> None in
               { name = E.pat_key pat; names = E.pat_names pat; role = Param; ty; expr = None; key = None }) ps
         | _ -> [])
    | Let | Branch -> [] in
  let body = if kind = Let then e else last e in
  let captures = List.filter (fun n -> List.mem n visible && not (List.mem n !bound)) (E.free_names body) in
  ignore c;
  rows @ List.map (fun n ->
    { name = n; names = [ n ]; role = Capture; ty = None; expr = None; key = None }) captures

let yield_label = function
  | For | Scan -> "collect" | Fold | State -> "next" | Sum -> "add" | Let -> "result" | Fn -> "return"
  | Branch -> "then"

let is_vec (e : S.t) = match e.node with S.Vec _ -> true | _ -> false

let rec scope_of ?(card_result = false) c ~visible ~inputs (path : path) (body : S.t) : scope =
  let binds, res = match scope_form body with
    | Some (bs, res) -> bs, res
    | None -> [], body in
  let visible = List.concat_map (fun (p, _) -> E.pat_names p) binds @ visible in
  let nodes = List.concat_map (fun (pat, e) -> node_of c ~visible path (Some pat) e) binds in
  let result, extra = match res.node with
    | S.Sym s when List.mem (root_of s) visible -> Link s, []
    | S.List _ | S.Map _ | S.Vec _ when card_result || not (is_vec res) ->
        let ns = node_of c ~visible path None res in
        Node (path @ [ "@result" ]), ns
    | _ -> Literal res, [] in
  { path; inputs; nodes = nodes @ extra; result }

(* the node of a binding (or of the result), after the nodes of the calls nested in its inputs *)
and node_of c ~visible ?(expr = false) ?nested (scope_path : path) (pat : S.t option) (e : S.t) : node list =
  let name = match nested, pat with
    | Some leaf, _ -> leaf | None, Some p -> E.pat_key p | None, None -> "@result" in
  let p = scope_path @ [ name ] in
  let term = match nested with
    | Some _ -> Hashtbl.find_opt c.forms e.id
    | None -> Hashtbl.find_opt c.terms p in
  let ty = match term with Some t -> t.ty | None -> Ty.Any in
  let kind = zone_kind pat e in
  let zone = Option.map (fun k ->
    let rail = rail_of c k e term ~visible in
    let inner_visible = List.concat_map (fun (r : rail_row) -> if r.role = Capture then [] else r.names) rail @ visible in
    let body = if k = Let then e else last e in
    (* a loop over the points or pieces of geometry says how its elements are ordered *)
    let order = List.find_map (fun (r : rail_row) ->
      (* the collection is the call itself, or a name bound to it *)
      let call = match r.role, r.expr with
        | Var, Some ({ S.node = S.List ({ S.node = S.Sym ("sop/point_list" | "sop/piece_list"); _ } :: _); _ } as e) -> Some e
        | Var, Some { S.node = S.Sym n; _ } ->
            Hashtbl.fold (fun (path : path) (t : W.term) found ->
              if found = None && path <> [] && List.nth path (List.length path - 1) = n then
                (match t.form.node with
                 | S.List ({ S.node = S.Sym ("sop/point_list" | "sop/piece_list"); _ } :: _) -> Some t.form
                 | _ -> None)
              else found) c.terms None
        | _ -> None in
      Option.map (fun (e : S.t) ->
        let rec key = function
          | { S.node = S.Kw "key"; _ } :: { S.node = S.Str k; _ } :: _ -> Some ("by " ^ k)
          | _ :: rest -> key rest
          | [] -> None in
        Option.value ~default:"by index" (key (S.children e))) call) rail in
    { kind = k; rail; yield_label = yield_label k; order;
      scope = scope_of ~card_result:(k = Fn) c ~visible:inner_visible ~inputs:[] p body }) kind in
  let macro = match head_sym e with Some h when List.mem_assoc h c.macros -> Some h | _ -> None in
  let lens = Option.map (fun _ -> macro_lens c.w e) macro in
  (* a node call written in an input is a node of its own, wired to the row by its leaf *)
  let rows, inner = if kind <> None then [], [] else
    List.fold_left (fun (rows, inner) (r : row) -> match r.expr, r.key with
      (* not a macro's argument: that is a piece of its template, which may read the macro's names *)
      | Some a, (E.Pos _ | E.Kw _ | E.Arm _) when (r.kind = Arg || r.kind = Rest) && macro = None
          && (E.node_call a || (E.value_call a && (expr || not (c.folded (scope_path @ [ E.nested_leaf name r.key ]))))) ->
          let leaf = E.nested_leaf name r.key in
          { r with chip = Name leaf } :: rows, inner @ node_of c ~visible ~expr:(E.value_call a) ~nested:leaf scope_path None a
      | _ -> r :: rows, inner) ([], []) (rows_of c e) |> fun (rows, inner) -> List.rev rows, inner in
  inner @ [
  { path = p; name; binds = (match nested, pat with None, Some pt -> E.pat_names pt | _ -> [ name ]);
    head = (match kind with
      | Some Let -> "let*" | Some _ -> Option.get (head_sym e) | None -> head_label c e);
    rows; outputs = (if nested = None then outputs pat ty else []); ty;
    note = (match pat with Some { S.notes = (_ :: _ as l); _ } -> Some (String.concat "\n" l) | _ -> None);
    bypass = List.mem "bypass" e.meta; macro; lens;
    live = W.Paths.mem p c.w.live; invariant = W.Paths.mem p c.w.invariant;
    synthetic = pat = None && nested = None; zone; expr } ]

let anonymous (n : node) = E.nested n.name
let title (n : node) =
  if n.synthetic then "result"
  else if anonymous n then (match String.rindex_opt n.head '/' with
    | Some i -> String.sub n.head (i + 1) (String.length n.head - i - 1) | None -> n.head)
  else n.name

(* the names a row is wired from: the nested node in it, else every name its expression reads *)
let sources (r : row) = match r.chip, r.expr with
  | Name leaf, _ when E.nested leaf -> [ leaf ]
  | _, Some { S.node = S.Sym ("t" | "pi" | "true" | "false" | "nil"); _ } -> []  (* constants, not wires *)
  | _, Some e -> E.free_names e
  | _, None -> []

let with_arguments changes scope =
  let edits = Hashtbl.create (List.length changes) in
  List.iter (fun (path, key, expr) -> Hashtbl.replace edits path
    ((key, expr) :: Option.value ~default:[] (Hashtbl.find_opt edits path))) changes;
  let rec patch (scope : scope) =
    let nodes = List.map (fun (node : node) ->
      let zone = match node.zone with
        | Some zone -> let scope = patch zone.scope in
            if scope == zone.scope then node.zone else Some {zone with scope}
        | None -> None in
      let rows = match Hashtbl.find_opt edits node.path with
        | None -> node.rows
        | Some changes -> List.map (fun (row : row) -> match List.assoc_opt row.key changes with
            | None -> row | Some expr -> {row with expr = Some expr}) node.rows in
      if rows == node.rows && zone == node.zone then node else {node with rows; zone}) scope.nodes in
    if List.for_all2 ( == ) nodes scope.nodes then scope else {scope with nodes} in
  patch scope

let bypassable (n : node) =
  n.zone = None && (not n.synthetic) && n.macro = None
  && not (List.mem n.head [ "record"; "number"; "text"; "link"; "vector"; "list"; "str"; "cond"; "case" ])
  && (n.bypass
      || n.head = "if" || (match List.find_opt (fun (r : row) -> r.kind = Arg) n.rows with
          | Some { ty = Some ty; _ } -> Ty.fits ty n.ty
          | _ -> false))

let reorderable (n : node) =
  n.zone = None && List.mem n.head [ "list"; "str"; "scene/merge" ]

let of_graph ?(folded = fun _ -> true) (catalog : Flow.Catalog.catalog) (w : W.t) name =
  Flow.Phase_timer.measure "project" (fun () ->
  let catalog = {catalog with Flow.Catalog.kinds =
    List.map (fun (node : W.node_definition) -> node.kind) w.nodes @ catalog.kinds} in
  let def = String.starts_with ~prefix:"def:" name in
  let bare = if def then String.sub name 4 (String.length name - 4) else name in
  let g = match List.find_opt (fun (g : W.graph) -> g.name = bare) (if def then w.defs else w.graphs @ w.defs) with
    | Some g -> g
    | None -> invalid_arg ("Projection.of_graph: no graph " ^ name) in
  let root = [ (if List.exists (fun (d : W.graph) -> d == g) w.defs then "def:" ^ g.name else g.name) ] in
  let terms = Hashtbl.create 64 in
  let forms = Hashtbl.create 64 in
  collect terms forms g.body;
  let macros = List.filter_map (fun (m : S.t) -> match S.children m with
    | _ :: { S.node = S.Sym n; _ } :: _ -> Some (n, m) | _ -> None) w.macros in
  let c = { w; catalog; ctx = g.context; macros; terms; forms; folded } in
  let inputs = List.map (fun (n, ty, d) ->
    { path = root @ [ ":" ^ n ]; name = n; ty; default = Option.map (fun (t : W.term) -> t.form) d }) g.inputs in
  scope_of c ~visible:(List.map (fun (i : input) -> i.name) inputs) ~inputs root (last g.form))

let rec zones (s : scope) = List.concat_map (fun (n : node) -> match n.zone with
  | Some z -> n :: zones z.scope | None -> []) s.nodes

let rec find (s : scope) path = List.find_map (fun (n : node) ->
  if n.path = path then Some n
  else match n.zone with Some z -> find z.scope path | None -> None) s.nodes

(* ---- levels and exposure ---- *)

type level = Point | Chip | Card | Full

let level_name = function Point -> "point" | Chip -> "chip" | Card -> "card" | Full -> "full"
let level_of_name = function
  | "point" -> Some Point | "chip" -> Some Chip | "card" -> Some Card | "full" -> Some Full | _ -> None

(* a literal binding: the sheet's header-only value card (name, the value in a field, the out port) *)
let value_card (n : node) =
  n.zone = None && n.macro = None && not n.synthetic
  && (match n.rows with
      | [ { key = E.Whole; chip = Const; _ } ] -> true
      | _ -> false)

type line = Folder of int * string | Row of int * row

let driven (r : row) = match r.chip with
  | Name _ | Inline _ -> true
  | _ -> sources r <> []

(* the exposure rule of flow.md 5.1 for one row ({!Exposure.shown}); structural rows always show *)
let row_shown ?pin (r : row) =
  match r.kind with
  | Rest | Hole | Binder -> true
  | Add -> false
  | Arg | Group_reader | Group_writer ->
      Exposure.shown { slot = false; driven = driven r; pin;
                       differs = r.expr <> None; primary = false }

(* the body of a card at a level: what its rows are, in order.  The header slot is not a row. *)
let lines ?(pin = fun _ -> None) level (n : node) : line array =
  if n.zone <> None || value_card n then [||] else
  let body = List.filter (fun (_, (r : row)) -> not r.head) (List.mapi (fun i r -> i, r) n.rows) in
  match level with
  | Point | Chip -> [||]
  | Full ->
      (* every section is labelled, in the order its first row appears, and holds all its rows; the
         slots (no folder) stay above the first label *)
      let seen = ref [] in
      List.iter (fun (_, (r : row)) -> if not (List.mem r.folder !seen) then seen := !seen @ [ r.folder ]) body;
      let rank (r : row) = Option.value ~default:0 (List.find_index (( = ) r.folder) !seen) in
      let body = List.stable_sort (fun (_, a) (_, b) -> compare (rank a) (rank b)) body in
      let folders = ref 0 and current = ref "" in
      Array.of_list (List.concat_map (fun (i, (r : row)) ->
        let label =
          if r.folder <> "" && r.folder <> !current then begin
            current := r.folder; incr folders; [ Folder (!folders, r.folder) ] end
          else [] in
        label @ [ Row (i, r) ]) body)
  | Card ->
      let has_head = List.exists (fun (r : row) -> r.head) n.rows in
      (* a call with no header slot (a [list], a record) keeps its [+] row on the card *)
      (* a card shows its wired and written rows; [o] and [p] reveal the rest *)
      let shown = List.filter (fun (_, (r : row)) ->
        (r.kind = Add && not has_head) || row_shown ?pin:(pin r.label) r) body in
      Array.of_list (List.map (fun (i, r) -> Row (i, r)) shown)

(* the count the chip shows: rows with something written *)
let set_count (n : node) =
  List.length (List.filter (fun (r : row) -> (not r.head) && r.expr <> None && r.kind <> Add) n.rows)

(* ---- layout ---- *)

let row_height = 24.
let head_height = 24.
let foot_height = 24.
(* kit rev 3: the header overlaps the card's 1-point border, and the rows start 1 point above its
   bottom edge (the sheet's [.nh] has a -1 margin) *)
let body_top = 23.
let card_pad = 5.  (* 4 points of padding and the bottom border under the last row *)
let node_width = 196.
let lattice = 24.  (* the dot grid: cards sit on it, columns and rows of cards share its pitch *)
let point_size = 14.
let zone_pad_x = 48.  (* a zone's cards start this far inside its edge: the begin and end bars at 20, then a channel for their wires *)
let zone_pad_top = 52.  (* and this far below its top (the label row and 28 points of air) *)
let zone_pad_bottom = 8.
let column_gap = 92.  (* the room between columns before the lattice rounds the pitch up: 196 + 92 gives 288 *)
let row_gap = 36.  (* at least this between stacked cards *)
let snap v = Float.round (v /. lattice) *. lattice
let ceil_lattice v = Float.ceil (v /. lattice -. 1e-9) *. lattice

type item = Input of input | Item of node | Return

type placed = {
  item : item; path : path; x : float; y : float; w : float; h : float;
  collapsed : bool; inner : layout option;
  level : level;  (* the requested level; the pane draws less below its zoom caps *)
  lines : line array;
  shown : level;
}
and layout = { placed : placed list; w : float; h : float }

(* the rail rows a zone shows under its label row: its first loop variable is the label itself *)
let label_row (z : zone) = List.find_opt (fun (r : rail_row) -> r.role = Var) z.rail
let extra_rails (z : zone) =
  let label = label_row z in
  List.filter (fun (r : rail_row) ->
    r.role <> Capture && (match label with Some l -> l != r | None -> true)) z.rail
let rail_top (n : node) = match n.zone with
  | Some z -> zone_pad_top +. float (List.length (extra_rails z)) *. row_height
  | None -> zone_pad_top

let count l = float_of_int (List.length l)

let lens_width = 400.
let lens_height (l : lens) ~step =
  let shown = if step >= Array.length l.steps then l.template
    else l.steps.(max 0 step) in
  let lines = match l.error with Some _ when step < Array.length l.steps -> 2 | _ ->
    min 16 (1 + String.fold_left (fun a c -> if c = '\n' then a + 1 else a) 0 shown) in
  row_height +. 10. +. float lines *. 20. +. row_height  (* the kit's code lines are 20 apart *)

(* a card: the header alone, or the rows below it, [extra] points of footer or panel and the padding *)
let card_height ~rows ~extra =
  if rows = 0. && extra = 0. then head_height else body_top +. rows *. row_height +. extra +. card_pad

(* a point's box: the disc and the name beside it *)
let point_title (n : node) = if n.synthetic then "result" else if anonymous n then "node" else n.name
let point_width (n : node) = point_size +. 8. +. 7. *. float (String.length (point_title n))

let rec size ~foot ~at ~collapsed ~lens ~level ~pin (it : item) :
    float * float * bool * layout option * level * line array = match it with
  | Input _ -> node_width, head_height, false, None, Card, [||]
  | Return -> node_width -. 40., card_height ~rows:1. ~extra:0., false, None, Card, [||]
  | Item ({ zone = Some z; _ } as n) ->
      let rail = count z.rail in
      if collapsed n.path then
        node_width, card_height ~rows:(Float.max 1. rail) ~extra:(if foot then foot_height else 0.), true, None, Card, [||]
      else
        let (l : layout) = layout ~foot ~at ~collapsed ~lens ~level ~pin ~inner:true z.scope in
        zone_pad_x +. Float.max l.w 72. +. zone_pad_x,
        rail_top n +. Float.max l.h row_height +. zone_pad_bottom,
        false, Some l, Card, [||]
  | Item n ->
      let lvl = if value_card n then Card else level n.path in
      let ln = lines ~pin:(pin n.path) lvl n in
      let open_lens = match n.lens, lens n.path with
        | Some l, Some step -> Some (l, step) | _ -> None in
      (match lvl with
       | Point -> point_width n, point_size, false, None, lvl, ln
       | Chip -> node_width, head_height, false, None, lvl, ln
       | Card | Full ->
           (match open_lens with Some _ -> Float.max node_width lens_width | None -> node_width),
           card_height ~rows:((if n.note <> None then 1. else 0.) +. float (Array.length ln) +. count n.outputs)
             ~extra:((if foot && lvl = Full then foot_height else 0.)
                     +. (match open_lens with Some (l, step) -> lens_height l ~step | None -> 0.)),
           false, None, lvl, ln)

and layout ?(foot = false) ?(at = fun _ -> None) ?(prev = fun _ -> None) ?(collapsed = fun _ -> false) ?(lens = fun _ -> None)
    ?(level = fun _ -> Card) ?(pin = fun _ _ -> None) ?(inner = false) (s : scope) : layout =
  (* the return card only when the result is a literal: otherwise the displayed node is the result *)
  let with_return = s.inputs <> [] && (match s.result with Literal _ -> true | _ -> false) in
  let items =
    List.map (fun (i : input) -> Input i, i.path, [], [ i.name ], None) s.inputs
    @ List.map (fun (n : node) ->
        let deps = List.concat_map sources n.rows
          @ (match n.zone with
             | Some z -> List.concat_map (fun (r : rail_row) ->
                 match r.role, r.expr with
                 | Capture, _ -> [ r.name ] | _, Some e -> E.free_names e | _ -> []) z.rail
             | None -> []) in
        (* the node the header's in-port reads, which the card lines up with *)
        let head_src = List.find_map (fun (r : row) ->
          if r.head then (match sources r with a :: _ -> Some (root_of a) | [] -> None) else None) n.rows in
        Item n, n.path, deps, n.binds, head_src) s.nodes
    @ (if with_return then [ Return, s.path @ [ "@return" ], [], [], None ] else []) in
  let by_name = Hashtbl.create 16 in
  List.iteri (fun k (_, _, _, names, _) -> List.iter (fun n -> Hashtbl.replace by_name n k) names) items;
  let arr = Array.of_list items in
  let lvl = Array.make (Array.length arr) (-1) in
  let rec lv k =
    if lvl.(k) >= 0 then lvl.(k)
    else begin
      lvl.(k) <- 0;
      let _, _, deps, _, _ = arr.(k) in
      lvl.(k) <- List.fold_left (fun l d -> match Hashtbl.find_opt by_name (root_of d) with
        | Some j when j <> k -> max l (lv j + 1) | _ -> l) 0 deps;
      lvl.(k)
    end in
  Array.iteri (fun k _ -> ignore (lv k)) arr;
  (* a chain is a column: the first node call a card reads in a positional input (its chain input,
     as Flow_edit.chain_in, or a [map]'s function) is written in it, so it stacks right above it, the source on top.  The
     nested steps come before their holder in [arr], so walking back settles every holder first *)
  for k = Array.length arr - 1 downto 0 do
    (match arr.(k) with
     | Item n, _, _, _, _ ->
         (match List.find_map (fun (r : row) -> match r.chip, r.key with
            | Name leaf, E.Pos _ when E.nested leaf && r.kind = Arg -> Some leaf | _ -> None) n.rows with
          | Some leaf ->
              (match Hashtbl.find_opt by_name leaf with
               | Some j when j < k && (match arr.(j) with Item s, _, _, _, _ -> not s.expr | _ -> false) ->
                   lvl.(j) <- lvl.(k)
               | _ -> ())
          | None -> ())
     | _ -> ())
  done;
  let cols = Array.fold_left max 0 lvl + 1 in
  (* column 0 is ordered by the first item that reads each of its items (a node before an input on a
     tie, then as written), so what feeds one consumer stacks together and the chains stay level *)
  let consumer = Array.make (Array.length arr) max_int in
  Array.iteri (fun j (_, _, deps, _, _) ->
    List.iter (fun d -> match Hashtbl.find_opt by_name (root_of d) with
      | Some k when k <> j && j < consumer.(k) -> consumer.(k) <- j
      | _ -> ()) deps) arr;
  (* the graph's inputs stack in the order they are written, each no earlier than the one before *)
  let last = ref 0 in
  Array.iteri (fun k (it, _, _, _, _) -> match it with
    | Input _ -> last := max !last consumer.(k); consumer.(k) <- !last
    | _ -> ()) arr;
  let order k = (consumer.(k), match arr.(k) with (Input _, _, _, _, _) -> 1 | _ -> 0) in
  (* the items of each column, in item order *)
  let by_level = Array.make cols [] in
  for k = Array.length arr - 1 downto 0 do by_level.(lvl.(k)) <- k :: by_level.(lvl.(k)) done;
  let origin_x = if inner then 0. else lattice and origin_y = if inner then 0. else lattice in
  (* a zone's cards sit on the lattice row of the cards beside it: its edge is [rail_top] above, so
     the first row starts that far down when the scope has a zone of its own *)
  let lead_of it = match it with
    | Item ({ zone = Some _; _ } as n) when not inner && not (collapsed n.path) -> rail_top n
    | _ -> 0. in
  let floor_lattice v = Float.floor (v /. lattice +. 1e-9) *. lattice in
  let row_start = origin_y +. 0. in
  let row_start = if inner then row_start
    else floor_lattice (origin_y +. Array.fold_left (fun m (it, _, _, _, _) -> Float.max m (lead_of it)) 0. arr) in
  (* an edit that adds or removes a few nodes keeps every other node where it was: the nodes it adds are
     placed beside their consumer below.  Many new nodes, or none known, is a first layout. *)
  let sticky = not inner && Array.length arr <= 600 && (let known = ref 0 and fresh = ref 0 in
    Array.iter (fun (_, p, _, _, _) -> if at p <> None || prev p <> None then incr known else incr fresh) arr;
    !known > 0 && !fresh <= 3) in
  let fresh_items = ref [] in
  let placed = ref [] and x = ref origin_x and w = ref 0. and h = ref 0. in
  let pos_of = Hashtbl.create 16 in
  for l = 0 to cols - 1 do
    let y = ref row_start and cw = ref 0. and first = ref true in
    let members = by_level.(l) in
    let members = if l = 0 then List.stable_sort (fun a b -> compare (order a) (order b)) members else members in
    List.iter (fun k ->
      let it, path, deps, names, head_src = arr.(k) in
      let iw, ih, coll, inner_l, lvl_, ln = size ~foot ~at ~collapsed ~lens ~level ~pin it in
      let lead = lead_of it in
      (* the card it lines up with: its deepest source (the main chain), the header's on a tie *)
      let aligned =
        let best = List.fold_left (fun best d -> match Hashtbl.find_opt by_name (root_of d) with
          | Some j when j <> k && (match best with None -> true | Some (_, bl) -> lvl.(j) > bl) -> Some (root_of d, lvl.(j))
          | _ -> best) None deps in
        let head = match head_src with
          | Some name -> (match Hashtbl.find_opt by_name name with
              | Some j when (match best with Some (_, bl) -> lvl.(j) >= bl | None -> true) -> Some name
              | _ -> None)
          | None -> None in
        match head, best with
        | _ when lead > 0. || (match it with Item { zone = Some _; _ } -> true | _ -> false) ->
            (* a zone lines its first card up with the topmost of its deepest sources *)
            let rows = List.filter_map (fun d -> match Hashtbl.find_opt by_name (root_of d) with
              | Some j when j <> k && (match best with Some (_, bl) -> lvl.(j) = bl | None -> false) ->
                  Hashtbl.find_opt pos_of (root_of d)
              | _ -> None) deps in
            (match rows with [] -> None | r :: rest -> Some (List.fold_left Float.min r rest))
        | Some name, _ -> Hashtbl.find_opt pos_of name
        | None, Some (name, _) -> Hashtbl.find_opt pos_of name
        | None, None -> None in
      let row_min = if !first then !y else floor_lattice (!y +. lead) in
      let row = match aligned with Some sr when sr > row_min -> sr | _ -> row_min in
      let px, py = match at path with
        | Some (ax, ay) -> ax, ay
        | None ->
            (match (if sticky then prev path else None) with
             | Some (ax, ay, _, _) -> ax, ay
             | None -> if sticky then fresh_items := k :: !fresh_items; !x, row -. lead) in
      let row = py +. lead in
      first := false;
      placed := { item = it; path; x = px; y = py; w = iw; h = ih; collapsed = coll; inner = inner_l;
                  level = lvl_; lines = ln; shown = lvl_ } :: !placed;
      List.iter (fun nm -> Hashtbl.replace pos_of nm row) names;
      y := ceil_lattice (Float.max !y (py +. ih +. row_gap));
      cw := Float.max !cw iw;
      w := Float.max !w (px +. iw); h := Float.max !h (py +. ih)) members;
    if !cw > 0. then x := ceil_lattice (!x +. !cw +. column_gap)
  done;
  let placed = List.rev !placed in
  let placed = if not sticky then placed else begin
    let boxes = Array.of_list placed in
    let n = Array.length boxes in
    let index = Hashtbl.create n in
    Array.iteri (fun i (b : placed) -> Hashtbl.replace index b.path i) boxes;
    let hit a b = a.x < b.x +. b.w -. 1e-6 && b.x < a.x +. a.w -. 1e-6
                  && a.y < b.y +. b.h -. 1e-6 && b.y < a.y +. a.h -. 1e-6 in
    (* ponytail: quadratic scan, fine for the hundreds of cards of a scope; use the int grid beyond that *)
    let clear ?(solid = fun _ -> true) i = let rec go j y = if j >= n then y else
        if j <> i && solid j && hit { boxes.(i) with y } boxes.(j) then go 0 (ceil_lattice (boxes.(j).y +. boxes.(j).h +. row_gap))
        else go (j + 1) y in
      boxes.(i) <- { (boxes.(i)) with y = go 0 boxes.(i).y } in
    (* a new node goes one column left of the first node that reads it, on that node's row *)
    List.iter (fun k ->
      let _, p, _, _, _ = arr.(k) in
      match Hashtbl.find_opt index p with
      | Some i ->
          (match (if consumer.(k) = max_int then None else
                    let _, cp, _, _, _ = arr.(consumer.(k)) in Hashtbl.find_opt index cp) with
           | Some o -> boxes.(i) <- { (boxes.(i)) with x = boxes.(o).x -. boxes.(i).w -. column_gap; y = boxes.(o).y }
           | None -> ());
          clear i
      | _ -> ()) (List.rev !fresh_items);
    (* a card that grew (a footer, a level) pushes the one it now covers down, and that one the next; never a dragged one *)
    begin
      let order = List.sort (fun i j -> compare (boxes.(i).y, boxes.(i).x) (boxes.(j).y, boxes.(j).x))
          (List.init n Fun.id) in
      let grew = Array.map (fun (b : placed) -> match prev b.path with
        | Some (_, _, pw, ph) -> pw <> b.w || ph <> b.h | None -> false) boxes in
      let seen = Array.make n false in
      List.iter (fun i ->
        if at boxes.(i).path = None then begin
          let y = boxes.(i).y in
          clear ~solid:(fun j -> seen.(j) && grew.(j)) i;
          if boxes.(i).y <> y then grew.(i) <- true end;
        seen.(i) <- true) order end;
    Array.to_list boxes end in
  let w = List.fold_left (fun m (b : placed) -> Float.max m (b.x +. b.w)) 0. placed
  and h = List.fold_left (fun m (b : placed) -> Float.max m (b.y +. b.h)) 0. placed in
  let margin = if inner then 0. else lattice in
  { placed; w = w +. margin; h = h +. margin }
