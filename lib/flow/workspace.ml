module S = Syntax
module Smap = Map.Make (String)

module Paths = Set.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

type path = string list
type context = Context.t

let context_name = Context.name
let context_of_name s = Result.to_option (Context.of_string s)
let context_ty = Context.result

type pattern = Name of string | Seq of pattern list | Keys of string list
type diagnostic_policy = First | Range_last

type term = { path : path option; ty : Ty.t; node : node; form : S.t }

and node =
  | Lit of Param.value
  | Text of string
  | Nil
  | Time
  | Vec of term list
  | Ref_binding of string * string list
  | Call of { kind : string; ctx : Context.t; args : (string * term) list }
  | Op of { op : string; args : (string * term) list; skip : int list list }
  | Call_fn of { fn : string; args : term list; body : term option }
  | Fn_ref of string
  | Graph_ref of { graph : string; inputs : (string * term) list }
  | Let of (pattern * term) list * term
  | State of { binder : pattern; init : term; step : term; zone : path }
  | Loop of {
      kind : [ `For | `Fold | `Scan | `Sum ];
      accs : (pattern * term) list;
      clauses : (pattern * term) list;
      skip : int list list;
      body : term;
      zone : path;
    }
  | With_error of term * term
  | Error_rows of { rows : term; policy : diagnostic_policy; mapper : term }
  | If of term * term * term
  | Cond of (term * term) list * term
  | Case of term * (S.t * term) list * term
  | Fn of { params : (pattern * Ty.t option) list; body : term; zone : path; capture : term option }
  | Hof of [ `Map | `Filter | `Reduce | `Sort_by | `Iterate ] * term list
  | List_lit of term list
  | Record of (string * term) list
  | Get of term * string
  | Assoc of term * (string * term) list
  | Str of term list
  | List_op of string * term list
  | Bypass of term
  | Expanded of { macro : string; body : term }

let iter_children f (term : term) =
  match term.node with
  | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> ()
  | Vec terms | Hof (_, terms) | List_lit terms | Str terms | List_op (_, terms) ->
      List.iter f terms
  | Call { args; _ } | Op { args; _ } | Graph_ref { inputs = args; _ } | Record args ->
      List.iter (fun (_, term) -> f term) args
  | Call_fn { args; body; _ } ->
      List.iter f args;
      Option.iter f body
  | Let (bindings, body) ->
      List.iter (fun (_, term) -> f term) bindings;
      f body
  | State { init; step; _ } ->
      f init;
      f step
  | Loop { accs; clauses; body; _ } ->
      List.iter (fun (_, term) -> f term) accs;
      List.iter (fun (_, term) -> f term) clauses;
      f body
  | With_error (a, b) ->
      f a;
      f b
  | Error_rows { rows; mapper; _ } ->
      f rows;
      f mapper
  | If (a, b, c) ->
      f a;
      f b;
      f c
  | Cond (arms, default) ->
      List.iter
        (fun (test, value) ->
          f test;
          f value)
        arms;
      f default
  | Case (value, arms, default) ->
      f value;
      List.iter (fun (_, term) -> f term) arms;
      f default
  | Fn { body; capture; _ } ->
      f body;
      Option.iter f capture
  | Get (value, _) | Bypass value | Expanded { body = value; _ } -> f value
  | Assoc (value, fields) ->
      f value;
      List.iter (fun (_, term) -> f term) fields

let children term =
  let reversed = ref [] in
  iter_children (fun child -> reversed := child :: !reversed) term;
  List.rev !reversed

let map_children f (term : term) =
  let map_fields fields = List.map (fun (name, value) -> (name, f value)) fields in
  let node =
    match term.node with
    | (Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _) as node -> node
    | Vec terms -> Vec (List.map f terms)
    | Call ({ args; _ } as call) -> Call { call with args = map_fields args }
    | Op ({ args; _ } as op) -> Op { op with args = map_fields args }
    | Call_fn ({ args; body; _ } as call) ->
        Call_fn { call with args = List.map f args; body = Option.map f body }
    | Graph_ref ({ inputs; _ } as graph) -> Graph_ref { graph with inputs = map_fields inputs }
    | Let (bindings, body) ->
        Let (List.map (fun (pattern, value) -> (pattern, f value)) bindings, f body)
    | State ({ init; step; _ } as state) -> State { state with init = f init; step = f step }
    | Loop ({ accs; clauses; body; _ } as loop) ->
        let map_bindings bindings =
          List.map (fun (pattern, value) -> (pattern, f value)) bindings
        in
        Loop { loop with accs = map_bindings accs; clauses = map_bindings clauses; body = f body }
    | With_error (a, b) -> With_error (f a, f b)
    | Error_rows ({ rows; mapper; _ } as error_rows) ->
        Error_rows { error_rows with rows = f rows; mapper = f mapper }
    | If (a, b, c) -> If (f a, f b, f c)
    | Cond (arms, default) ->
        Cond (List.map (fun (test, value) -> (f test, f value)) arms, f default)
    | Case (value, arms, default) ->
        Case (f value, List.map (fun (pattern, value) -> (pattern, f value)) arms, f default)
    | Fn ({ body; capture; _ } as fn) ->
        Fn { fn with body = f body; capture = Option.map f capture }
    | Hof (kind, terms) -> Hof (kind, List.map f terms)
    | List_lit terms -> List_lit (List.map f terms)
    | Record fields -> Record (map_fields fields)
    | Get (value, field) -> Get (f value, field)
    | Assoc (value, fields) -> Assoc (f value, map_fields fields)
    | Str terms -> Str (List.map f terms)
    | List_op (op, terms) -> List_op (op, List.map f terms)
    | Bypass value -> Bypass (f value)
    | Expanded ({ body; _ } as expanded) -> Expanded { expanded with body = f body }
  in
  { term with node }

type graph = {
  name : string;
  context : context;
  inputs : (string * Ty.t * term option) list;
  body : term;
  form : S.t;
}

type node_definition = {
  kind : Catalog.kind;
  slots : Catalog.slot list;
  keyword_inputs : string list;
  fields : Param.field_view list;
  label : string;
  category : string list;
  description : string option;
  operation : string;
  version : int;
  body : graph;
  validations : (term * string * string) list;
  normalizers : (string * term) list;
}

(* Zero-state operators a defnode body reads from its cook context; Context_kernel specializes them. *)
let body_context_ops =
  [ "sop/context_seed"; "sop/provenance_base"; "sop/context_frame"; "sop/context_time" ]

(* Checked terms share subterms (expanded macros, remapped residuals): walk each node once, or a
   shared chain is visited exponentially often. *)
module Seen_term = Hashtbl.Make (struct
  type t = term

  let equal = ( == )
  let hash (t : term) = t.form.S.id
end)

let called_names ~include_ops root =
  let seen = Seen_term.create 256 and out = ref [] in
  let rec calls (t : term) =
    if not (Seen_term.mem seen t) then begin
      Seen_term.add seen t ();
      (match t.node with
      | Call { kind; _ } -> out := kind :: !out
      | Op { op; _ } when include_ops -> out := op :: !out
      | _ -> ());
      iter_children calls t
    end
  in
  calls root;
  List.sort_uniq String.compare !out

let kind_calls root = called_names ~include_ops:false root

let bypass_input term =
  let entries =
    match term.node with
    | Call { args; _ } | Op { args; _ } -> List.map snd args
    | Call_fn { args; _ } -> args
    | _ -> []
  in
  let rec first = function
    | { S.node = Kw _; _ } :: _ :: rest -> first rest
    | input :: _ -> Some input
    | [] -> None
  in
  match term.form.node with
  | S.List (_ :: args) ->
      Option.bind (first args) (fun input ->
          match List.find_opt (fun (t : term) -> t.form == input) entries with
          | Some _ as found -> found
          | None ->
              List.find_opt
                (fun (t : term) -> t.form.id = input.id && t.form.span = input.span)
                entries)
  | _ -> List.nth_opt entries 0

type packed_form = Candidate of Paths.t | Refused of Diagnostic.t list

type t = {
  name : string;
  graphs : graph list;
  defs : graph list;
  nodes : node_definition list;
  macros : S.t list;
  source : S.t list;
  live : Paths.t;
  invariant : Paths.t;
  approx : Paths.t;
  packed : (path * packed_form) list;
  packed_roots : (S.t * path) list;
  approx_reasons : (path * Diagnostic.t list) list;
  kind_fns : (string * (string * Context.t * Catalog.slot list)) list;
  ops : Op.t list;
}

let max_iterations = Op.max_iterations

(* ponytail: a guard against exponential call-site typing, not a language limit. *)
let max_steps = 400_000
let max_call_depth = 64

(* ---- names ---- *)

let special =
  [
    "workspace";
    "graph";
    "defn";
    "defnode";
    "defmacro";
    "let*";
    "ref";
    "for";
    "fold";
    "scan";
    "sum";
    "state";
    "if";
    "with_error";
    "error_rows";
    "values";
    "fn";
    "cond";
    "case";
    "list";
    "concat";
    "str";
    "get";
    "assoc";
    "map";
    "filter";
    "reduce";
    "sort-by";
    "iterate";
    "quote";
    "quasiquote";
    "unquote";
    "unquote-splicing";
  ]

let special_forms = special

(* String lists the checker asks of every head and symbol: typed equality, not polymorphic compare. *)
let rec mem_s s = function [] -> false | x :: rest -> String.equal s x || mem_s s rest

let rec assoc_s s = function
  | [] -> None
  | (n, x) :: rest -> if String.equal n s then Some x else assoc_s s rest

let rec prefix_from p s i =
  i = String.length p || (String.unsafe_get p i = String.unsafe_get s i && prefix_from p s (i + 1))

let has_prefix p s = String.length s >= String.length p && prefix_from p s 0
let index_names = Array.init 64 string_of_int
let index_name i = if i < 64 then index_names.(i) else string_of_int i
let has_empty l = List.exists (function [] -> true | _ :: _ -> false) l

let opaque_head h =
  mem_s h
    [
      "fn";
      "map";
      "filter";
      "reduce";
      "sort-by";
      "iterate";
      "if";
      "cond";
      "case";
      "with_error";
      "error_rows";
      "exact";
    ]

let reserved s = mem_s s special || mem_s s [ "t"; "pi"; "true"; "false"; "nil" ]
let type_name s = List.exists (fun (n, _) -> String.equal s n) (Ty.names ())

(* ---- helpers ---- *)

let replace_all ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if !i + n <= String.length s && String.sub s !i n = sub then (
      Buffer.add_string b by;
      i := !i + n)
    else (
      Buffer.add_char b s.[!i];
      incr i)
  done;
  Buffer.contents b

let show t = replace_all ~sub:"list:" ~by:"list of " (Ty.to_string t)
let plural n = if n = 1 then "" else "s"

let rec shape_ty = function
  | Ty.Named _ as ty -> Ty.shape ty
  | List e -> shape_ty e
  | Record fs -> List.exists (fun (_, t) -> shape_ty t) fs
  | _ -> false

let rec geometry_accumulator = function
  | ty when ty = Ty.geometry -> true
  | Ty.Record fields ->
      List.exists (fun (_, ty) -> geometry_accumulator ty) fields
      && List.for_all (fun (_, ty) -> (not (shape_ty ty)) || geometry_accumulator ty) fields
  | _ -> false

let last_segment id = match List.rev id with s :: _ -> s | [] -> ""
let path_text id = String.concat "/" id
let sym_of (x : S.t) = match x.node with S.Sym s -> Some s | _ -> None
let pattern_key (x : S.t) = match x.node with S.Sym s when x.meta = [] -> s | _ -> Lisp.flat x
let map_seq f xs = List.rev (List.fold_left (fun acc x -> f x :: acc) [] xs)

(* ---- checker state ---- *)

type v = {
  ty : Ty.t;
  live : bool;
  live_len : bool;
  approx : bool;
  approx_sources : Paths.t;
  packed_producers : Paths.t;
  state_dep : bool;
  vary : int list;
  len : int option;
  fn : callee option;
  groups : string list;
  list_fields : string list list;
}

and callee = Closure of closure | Def_fn of string | Op_fn of string | Kind_fn of Catalog.kind

and closure = {
  params : (S.t * Ty.t option) list;
  body : S.t;
  ccx : cx;
  cname : string;
  cid : path;
}

and cx = {
  env : v Smap.t;
  ctx : context;
  scope : string;
  path : path;
  in_def : bool;
  stack : string list;
  depth : int;
  zone : int;
  zbody : bool;
}

type arg = { key : string option; aform : S.t; aterm : term; av : v }

type signature = {
  sname : string;
  sctx : context;
  sparams : (string * Ty.t * S.t option) list;
  sbody : S.t;
  sform : S.t;
}

exception Budget

(* The checker's state after a library's forms: a check that starts from it checks only its own
   forms (Checker [start]). Tables are copied in, never shared. *)
type snapshot = {
  s_diags : Diagnostic.t list;
  s_live : Paths.t;
  s_invariant : Paths.t;
  s_roots : (path, packed_form) Hashtbl.t;
  s_links : (path, Paths.t) Hashtbl.t;
  s_root_forms : (int, (S.t * path) list) Hashtbl.t;
  s_steps : int;
  s_zones : int;
  s_body_memo : (string * string list * int * v list, term * v) Hashtbl.t;
  s_kind_fns : (string * (string * Context.t * Catalog.slot list)) list;
  s_inline_seen : (path, int) Hashtbl.t;
  s_input_paths : (path * string * int, path) Hashtbl.t;
  s_names : (string, unit) Hashtbl.t;
  s_sigs : (string, signature) Hashtbl.t;
  s_macro_tbl : (string, S.t) Hashtbl.t;
  s_macro_forms : S.t list;
  s_node_sources : (string, Defnode.t) Hashtbl.t;
  s_node_terms : node_definition list;
  s_def_terms : (string, graph) Hashtbl.t;
  s_def_defaults : (string * string, term * v) Hashtbl.t;
  s_items : (string * string * S.t) list;
  s_checked : (string, unit) Hashtbl.t;
  s_def_order : string list;
  s_count : int;
  s_budget : bool;
}

let leaf ?(live = false) ?(vary = []) ?(groups = []) ?len ty =
  {
    ty;
    live;
    live_len = false;
    approx = false;
    approx_sources = Paths.empty;
    packed_producers = Paths.empty;
    state_dep = false;
    vary;
    len;
    fn = None;
    groups;
    list_fields = [];
  }

let union a b = List.sort_uniq compare (a @ b)

(* the accumulator is sorted and unique already: an empty addition keeps it *)
let add_union a b = if b = [] then a else union a b

(* a vary list is always a union's result, sorted and unique: another one alone, or itself, is the union *)
let add_vary a b = if b = [] || a == b then a else if a = [] then b else union a b

let derive ty vs =
  {
    ty;
    live = List.exists (fun v -> v.live) vs;
    live_len = false;
    approx = false;
    approx_sources =
      List.fold_left (fun sources v -> Paths.union sources v.approx_sources) Paths.empty vs;
    packed_producers =
      List.fold_left (fun sources v -> Paths.union sources v.packed_producers) Paths.empty vs;
    state_dep = List.exists (fun v -> v.state_dep) vs;
    vary = List.fold_left (fun a v -> add_vary a v.vary) [] vs;
    len = None;
    fn = None;
    groups = List.fold_left (fun a v -> add_union a v.groups) [] vs;
    list_fields = List.fold_left (fun a v -> add_union a v.list_fields) [] vs;
  }

let approximate vs = List.exists (fun (v : v) -> v.approx) vs
let packed_array ty = Option.is_some (Packed_ops.array_width ty)

let rec packed_body ~ops (term : term) =
  let body = packed_body ~ops in
  match term.node with
  | Lit (Param.Int_value _ | Float_value _ | Bool_value _) | Time -> true
  | Text _ -> true
  | Ref_binding _ -> term.ty = Ty.Text || Option.is_some (Packed_ops.scalar_width term.ty)
  | Vec terms -> List.mem (List.length terms) [ 2; 3; 4 ] && List.for_all body terms
  | Get (value, field) ->
      (List.mem (value.ty, field)
         [
           (Ty.Vec2, "x");
           (Vec2, "y");
           (Vec3, "x");
           (Vec3, "y");
           (Vec3, "z");
           (Vec4, "x");
           (Vec4, "y");
           (Vec4, "z");
           (Vec4, "w");
         ]
      || match value.ty with Ty.Record fields -> List.mem_assoc field fields | _ -> false)
      && body value
  | Record fields -> List.for_all (fun (_, value) -> body value) fields
  | Call_fn { args; body = Some specialized; _ } -> List.for_all body args && body specialized
  | Op { op; args; _ } -> (
      match Option.bind (Op.find ~extra:ops op Context.value) Op.packed_kind with
      | Some Op.Frame -> args = [] && List.mem term.ty [ Ty.Float; Int; Bool ]
      | Some Noise3 ->
          List.for_all
            (fun (key, t) ->
              match (key, t.node) with
              | "position", _ -> body t
              | "seed", Lit (Param.Int_value _) -> true
              | "octaves", Lit (Param.Int_value n) -> Packed_ops.supported_noise_octaves n
              | ("lacunarity" | "gain"), Lit (Param.Float_value _) -> true
              | _ -> false)
            args
      | Some (Length | Normalize) -> (
          match args with [ (_, vector) ] -> vector.ty = Ty.Vec3 && body vector | _ -> false)
      | Some (Dot | Cross) ->
          List.length args = 2
          && List.for_all (fun (_, (t : term)) -> t.ty = Ty.Vec3 && body t) args
      | Some Exact -> ( match args with [ (_, t) ] -> body t | _ -> false)
      | Some (Fma | Checked_fma) -> List.length args = 3 && List.for_all (fun (_, t) -> body t) args
      | Some Trace -> List.length args = 6 && List.for_all (fun (_, t) -> body t) args
      | Some Stage -> List.for_all (fun (_, t) -> body t) args
      | Some Checked_mul -> List.length args = 2 && List.for_all (fun (_, t) -> body t) args
      | Some Gather -> (
          match args with
          | [ (_, array); (_, index) ] ->
              (array.ty = Ty.Array Ty.Text || Option.is_some (Packed_ops.array_width array.ty))
              && body index
          | _ -> false)
      | Some (Binary _ | Integer _ | Integer_compare _ | Rand_at) ->
          List.length args = 2 && List.for_all (fun (_, t) -> body t) args
      | Some (Unary _ | Convert _) ->
          List.length args = 1 && List.for_all (fun (_, t) -> body t) args
      | Some Constant_only | None -> false)
  | Let (bindings, result) ->
      List.for_all (fun (p, t) -> match p with Name _ -> body t | _ -> false) bindings
      && body result
  | If (test, yes, no) -> body test && body yes && body no
  | Cond (arms, default) ->
      body default && List.for_all (fun (test, value) -> body test && body value) arms
  | Case (scrutinee, arms, default) ->
      body scrutinee && body default
      && List.for_all
           (fun ((literal : S.t), value) ->
             (match literal.node with S.Num _ | S.Sym ("true" | "false") -> true | _ -> false)
             && body value)
           arms
  | Expanded { body = t; _ } -> body t
  | Bypass call ->
      Option.fold ~none:false
        ~some:(fun (input : term) ->
          (input.ty = term.ty
          || (term.ty = Ty.Float && List.mem input.ty [ Ty.Int; Bool ])
          || (List.mem term.ty [ Ty.Vec2; Vec3; Vec4 ] && List.mem input.ty [ Ty.Int; Float ]))
          && body input)
        (bypass_input call)
  | _ -> false

let rec list_fields = function
  | Ty.List _ -> [ [] ]
  | Record fields ->
      List.concat_map (fun (n, ty) -> List.map (fun path -> n :: path) (list_fields ty)) fields
  | _ -> []

let poison = leaf Ty.Any

let port_ty = function
  | Ty.Float -> Some Port_type.Float
  | Int -> Some Port_type.Int
  | Bool -> Some Port_type.Bool
  | Vec3 -> Some Port_type.Vec3
  | Ty.Named "geometry" -> Some Port_type.Geometry
  | Named "image" -> Some Port_type.Image
  | Fn (Some signature) -> Some (Port_type.Fn signature)
  | _ -> None

let ty_of_port = function
  | Port_type.Float -> Ty.Float
  | Int -> Ty.Int
  | Bool -> Ty.Bool
  | Vec3 -> Ty.Vec3
  | Geometry -> Ty.geometry
  | Image -> Ty.image
  | Fn signature -> Ty.Fn (Some signature)

let is_color (p : Catalog.parameter) =
  p.ty = Some Port_type.Vec3
  &&
  match p.fields with
  | [ (a, _, _); (b, _, _); (c, _, _) ] ->
      String.ends_with ~suffix:"_r" a && String.ends_with ~suffix:"_g" b
      && String.ends_with ~suffix:"_b" c
  | _ -> false

let param_ty (p : Catalog.parameter) =
  match p.ty with
  | Some Port_type.Vec3 when is_color p -> Ty.Color
  | Some t -> ty_of_port t
  | None -> Ty.Text

let kind_out (k : Catalog.kind) =
  if k.context <> Context.sop && k.context <> Context.value then Context.result k.context
  else
    match k.outputs with
    | [ (_, t) ] -> ty_of_port t
    | [] -> Ty.Any
    | outs -> Ty.Record (List.map (fun (n, t) -> (n, ty_of_port t)) outs)

(* the type of a kind's slots: geometry, a World layer's layer below, a scene/world's World, a root's scene *)
let slot_ty ?slot (k : Catalog.kind) =
  match Option.bind slot (fun (s : Catalog.slot) -> s.ty) with
  | Some ty -> ty
  | None -> (
      match k.qualified with
      | "scene/world" -> Ty.world
      | "scene/root" -> Ty.scene
      | _ -> if k.context = Context.scene then Ty.geometry else Context.result k.context)

let rec positional_slot slots index =
  match slots with
  | [] -> None
  | (slot : Catalog.slot) :: rest ->
      if index = 0 || slot.rest then Some slot else positional_slot rest (index - 1)

(* ponytail: the catalog has no group markers yet; a `group` parameter reads a group and
   `name` on a `sop/group_*` kind writes one. *)
let group_reader (p : Catalog.parameter) = p.name = "group"

let group_writer (k : Catalog.kind) (p : Catalog.parameter) =
  p.name = "name" && String.starts_with ~prefix:"sop/group_" k.qualified

let rec to_check (t : term) : Catalog.term =
  let ty = port_ty t.ty in
  match t.node with
  | Lit v -> { node = Catalog.Literal v; ty }
  | Text s -> { node = Catalog.Literal (Param.Text_value s); ty = None }
  | Nil -> { node = Catalog.Nil; ty }
  | Vec cs ->
      {
        node =
          Catalog.Vector
            (List.map
               (fun (c : term) ->
                 let c = to_check c in
                 if c.ty = None then { c with ty = Some Port_type.Float } else c)
               cs);
        ty;
      }
  | _ -> { node = Catalog.Reference ("_", "_"); ty }

let hex_colour s =
  let n = String.length s in
  n > 0
  && s.[0] = '#'
  && (n = 4 || n = 7 || n = 9)
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
       (String.sub s 1 (n - 1))

let validate_parameter ?ty report (p : Catalog.parameter) (term : term) =
  let want = param_ty p and have = Option.value ~default:term.ty ty in
  if have <> Ty.Any then begin
    if not (Ty.fits have want) then
      report Diagnostic.Error_severity "E_TYPE"
        (Printf.sprintf ":%s takes %s, but this is %s" p.name (show want) (show have))
    else
      match (want, term.node) with
      | Ty.Color, Text s ->
          if not (hex_colour s) then
            report Diagnostic.Error_severity "E_TYPE"
              (Printf.sprintf ":%s is a colour: write \"#rrggbb\" or a vec3, not \"%s\"" p.name s)
      | Ty.Color, _ ->
          if have = Ty.Vec3 then
            Catalog.validate_parameter report { p with ty = Some Port_type.Vec3 } (to_check term)
      | Ty.Vec3, _ when have <> Ty.Vec3 -> ()
      | Ty.Text, Text _ -> Catalog.validate_parameter report p (to_check term)
      | Ty.Text, _ -> ()
      | _ -> Catalog.validate_parameter report p (to_check term)
  end

let pairs = S.pairs
let is_kw (x : S.t) = match x.node with S.Kw _ -> true | _ -> false
let tm (x : S.t) ty node = { path = None; ty; node; form = x }

let rec pat_ir (p : S.t) =
  match p.node with
  | S.Sym n -> Name n
  | S.Vec ps -> Seq (List.map pat_ir ps)
  | S.Map [ _; { S.node = S.Vec ks; _ } ] -> Keys (List.filter_map sym_of ks)
  | _ -> Name "_"

let rec pat_names (p : S.t) =
  match p.node with
  | S.Sym n -> [ n ]
  | S.Vec ps -> List.concat_map pat_names ps
  | S.Map [ _; { S.node = S.Vec ks; _ } ] -> List.filter_map sym_of ks
  | _ -> []

(* A [:skip] value (register L16): a list of tuples of non-negative integers, a bare integer
   being a tuple of one.  A static list, so it never depends on t. *)
let skip_tuples (v : S.t) : int list list option =
  let int (n : S.t) =
    match n.node with
    | S.Num s -> ( match int_of_string_opt s with Some i when i >= 0 -> Some i | _ -> None)
    | _ -> None
  in
  let tuple (t : S.t) =
    match t.node with
    | S.Vec (_ :: _ as ns) ->
        let is = List.filter_map int ns in
        if List.length is = List.length ns then Some is else None
    | _ -> Option.map (fun i -> [ i ]) (int t)
  in
  match v.node with
  | S.Vec items when List.for_all (fun t -> tuple t <> None) items ->
      Some (List.map (fun t -> Option.get (tuple t)) items)
  | _ -> None

(* The checker proper: the names, types, macro and defnode tables of one workspace check and the
   mutually recursive inference over them (infer .. graph_info), then the form-level [driver]. *)
module Checker (P : sig
  val ops : Op.t list
  val library : bool
  val call_sites : bool
  val catalog : Catalog.catalog
  val children : int
  val start : snapshot option
end) =
struct
  open P

  let from f empty = match start with Some s -> f s | None -> empty
  let table f n = match start with Some s -> Hashtbl.copy (f s) | None -> Hashtbl.create n
  let find_op = Op.find ~extra:ops
  let diags = ref (from (fun s -> s.s_diags) [])

  let live = ref (from (fun s -> s.s_live) Paths.empty)
  and invariant = ref (from (fun s -> s.s_invariant) Paths.empty)

  let roots = table (fun s -> s.s_roots) 32
  and links = table (fun s -> s.s_links) 64
  and root_forms = table (fun s -> s.s_root_forms) 32

  let link path producers =
    if not (Paths.is_empty producers) then
      Hashtbl.replace links path
        (Paths.union producers (Option.value ~default:Paths.empty (Hashtbl.find_opt links path)))

  let remember_root (form : S.t) path =
    let forms = Option.value ~default:[] (Hashtbl.find_opt root_forms form.id) in
    if not (List.exists (fun (old, owner) -> old == form && owner = path) forms) then
      Hashtbl.replace root_forms form.id ((form, path) :: forms)

  let root path (form : S.t) reasons =
    let candidate = if reasons = [] then Candidate (Paths.singleton path) else Refused reasons in
    let merged =
      match (Hashtbl.find_opt roots path, candidate) with
      | Some (Candidate old), Candidate next -> Candidate (Paths.union old next)
      | Some (Candidate _ as old), Refused _ -> old
      | _, Candidate _ -> candidate
      | Some (Refused old), Refused next -> Refused (List.sort_uniq compare (old @ next))
      | None, _ -> candidate
    in
    Hashtbl.replace roots path merged;
    remember_root form path;
    link path (Paths.singleton path)

  let refusal (form : S.t) code message = Diagnostic.error ~span:form.span ~code message

  let source_refusal (term : term) =
    match term.ty with
    | Ty.Any | Array Any -> []
    | ty when packed_array ty -> []
    | ty ->
        [
          refusal term.form "E_PACKED_TYPE"
            ("Packed source requires Array Float/Vec2/Vec3/Vec4, got " ^ show ty ^ ".");
        ]

  let rec link_body producer (term : term) =
    let visit = link_body producer in
    let all = List.iter visit and fields fs = List.iter (fun (_, t) -> visit t) fs in
    match term.node with
    | Hof _ | Loop _ | Fn _ -> ()
    | Op { op; _ } when Option.bind (find_op op Context.value) Op.packed_kind = Some Op.Exact -> ()
    | _ -> (
        Option.iter (fun path -> link path (Paths.singleton producer)) term.path;
        match term.node with
        | Vec ts -> all ts
        | Get (t, _) | Bypass t | Expanded { body = t; _ } -> visit t
        | Op { args; _ } -> fields args
        | Let (bindings, result) ->
            fields bindings;
            visit result
        | With_error (a, b) -> all [ a; b ]
        | Error_rows { rows = a; mapper = b; _ } -> all [ a; b ]
        | If (c, a, b) -> all [ c; a; b ]
        | Cond (arms, d) ->
            List.iter (fun (a, b) -> all [ a; b ]) arms;
            visit d
        | Case (s, arms, d) ->
            visit s;
            fields arms;
            visit d
        | Call_fn { args; body; _ } ->
            all args;
            Option.iter visit body
        | _ -> ())

  let steps = ref (from (fun s -> s.s_steps) 0)
  and zones = ref (from (fun s -> s.s_zones) 0)

  let body_memo = table (fun s -> s.s_body_memo) 64

  (* The budget is per top-level definition: library node bodies are spliced into the
     workspace (all of them for the node manifest), so a fixed total grows too small. *)
  let max_steps = max_steps * max 1 (from (fun s -> s.s_count) 0 + children)

  let add sev (x : S.t) code msg =
    let span = if x.span.start = 0 && x.span.finish = 0 then None else Some x.span in
    diags :=
      (match sev with
      | Diagnostic.Error_severity -> Diagnostic.error ?span ~code msg
      | Diagnostic.Warning -> Diagnostic.warning ?span ~code msg)
      :: !diags

  let err x code msg = add Diagnostic.Error_severity x code msg

  let precision path (v : v) =
    if v.approx && Paths.is_empty v.approx_sources then
      { v with approx_sources = Paths.singleton path }
    else v

  let exact_sink cx x sink (v : v) =
    let v = precision cx.path v in
    if not (Paths.is_empty v.approx_sources) then
      err x "E_APPROX_SINK"
        (Printf.sprintf
           "Approximate values require (exact x) before catalog calls, exports, state or cache \
            keys. Producer: %s; sink: %s (%s)."
           (String.concat ", " (List.map path_text (Paths.elements v.approx_sources)))
           (path_text cx.path) sink)

  let bad x code msg =
    err x code msg;
    (tm x Ty.Any Nil, poison)

  let skip_of (v : S.t) : int list list =
    match skip_tuples v with
    | Some tuples -> tuples
    | None ->
        err v "E_SKIP"
          ":skip is a list of iteration tuples of non-negative integers, for example :skip [[0 1] \
           [2 0]].";
        []

  let mark id (v : v) =
    if v.live then live := Paths.add id !live;
    link id v.packed_producers

  (* the path of a let*, zone or fn written inline: [~for], then [~for~1], ... under one path,
     so two of them in one expression never share compiled ids *)
  let kind_fns = ref (from (fun s -> s.s_kind_fns) [])
  let inline_seen = table (fun s -> s.s_inline_seen) 16
  let input_paths = table (fun s -> s.s_input_paths) 16

  let inline cx segment =
    let key = cx.path @ [ segment ] in
    let n = Option.value ~default:0 (Hashtbl.find_opt inline_seen key) in
    Hashtbl.replace inline_seen key (n + 1);
    if n = 0 then key else cx.path @ [ Printf.sprintf "%s~%d" segment n ]

  let names = table (fun s -> s.s_names) 16
  let sigs = table (fun s -> s.s_sigs) 8
  let gsigs : (string, signature) Hashtbl.t = Hashtbl.create 8
  let macro_tbl = table (fun s -> s.s_macro_tbl) 8
  let macro_forms = ref (from (fun s -> s.s_macro_forms) [])
  let node_sources = table (fun s -> s.s_node_sources) 8
  let node_terms = ref (from (fun s -> s.s_node_terms) [])
  let kind_memo = Hashtbl.create 64

  let kind_of ctx head =
    let key = (context_name ctx, head) in
    match Hashtbl.find_opt kind_memo key with
    | Some r -> r
    | None ->
        let kinds =
          Hashtbl.fold
            (fun _ (d : Defnode.t) kinds -> d.kind :: kinds)
            node_sources catalog.Catalog.kinds
        in
        let r = Catalog.resolve_kind { catalog with kinds } ctx head in
        Hashtbl.add kind_memo key r;
        r

  let resolve_head cx s =
    match Hashtbl.find_opt sigs s with
    | Some d -> `Def d
    | None -> (
        match Hashtbl.find_opt macro_tbl s with
        | Some m -> `Macro m
        | None -> (
            match find_op s cx.ctx with
            | Some o -> `Op o
            | None -> ( match kind_of cx.ctx s with Ok k -> `Kind k | Error e -> `Missing e)))

  let known ~head s =
    reserved s || type_name s || s = "fn" || Hashtbl.mem names s
    || (head || not (Macro.valid_name s))
       && (List.exists (fun c -> find_op s c <> None) (Context.all ())
          ||
          match kind_of Context.sop s with
          | Ok _ | Error (("E_WRONG_CONTEXT" | "E_AMBIGUOUS"), _) -> true
          | Error _ -> false)

  let no_fn x (t : Ty.t) what =
    if Ty.has_fn t then
      err x "E_FN_ESCAPES"
        (Printf.sprintf
           "%s is a function; a function value cannot be stored or returned (E_FN_ESCAPES). Call \
            it where it is bound."
           what)

  let need (a : arg) what want =
    if
      Ty.fits a.av.ty want
      || (a.av.ty = Ty.Fn None && a.av.fn = None && match want with Ty.Fn _ -> true | _ -> false)
    then true
    else (
      err a.aform "E_TYPE"
        (Printf.sprintf "%s: expected %s, got %s." (what ()) (show want) (show a.av.ty));
      false)

  let field x (v : v) f name =
    let derived ty =
      let fields =
        List.filter_map (function n :: path when n = f -> Some path | _ -> None) v.list_fields
      in
      { v with ty; live_len = has_empty fields; list_fields = fields; len = None; fn = None }
    in
    match v.ty with
    | Ty.Any -> derived Ty.Any
    | Ty.Vec2 when f = "x" || f = "y" -> derived Ty.Float
    | Ty.Vec3 when f = "x" || f = "y" || f = "z" -> derived Ty.Float
    | Ty.Vec4 when mem_s f [ "x"; "y"; "z"; "w" ] -> derived Ty.Float
    | Ty.Record fs -> (
        match assoc_s f fs with
        | Some t -> derived t
        | None ->
            err x "E_FIELD"
              (Printf.sprintf "%s has no field %s. Fields: %s." name f
                 (if fs = [] then "none" else String.concat ", " (List.map fst fs)));
            poison)
    | _ ->
        err x "E_FIELD"
          (Printf.sprintf "%s has no output %s. A vec3 has .x .y .z; a record has its fields." name
             f);
        poison

  let check_pat cx (pat : S.t) seen what zone =
    let msg_shape p =
      if zone then
        Printf.sprintf "%s binds names; %s is not a name or pattern." what (pattern_key p)
      else
        Printf.sprintf "Invalid binding name %s. Bind a name, [a b] or {:keys [a b]}."
          (pattern_key p)
    in
    let ok = ref true in
    let name_ok (at : S.t) n =
      if (not (Macro.valid_name n)) || (reserved n && n <> "state") then begin
        ok := false;
        err at "E_BINDING"
          (if zone then Printf.sprintf "%s binds names; %s is not a valid one." what n
           else
             Printf.sprintf "Invalid binding name %s%s." n
               (if n = "t" then ": t is the context time" else ""))
      end
      else if Hashtbl.mem seen n then begin
        ok := false;
        err at "E_DUPLICATE" (Printf.sprintf "%s is bound twice in one %s." n what)
      end
      else if Smap.mem n cx.env then begin
        ok := false;
        err at "E_SHADOW"
          (if zone then Printf.sprintf "%s shadows an outer name. Pick another loop name." n
           else
             Printf.sprintf
               "%s shadows an outer name in %s. Rename it; the graph cannot show two nodes called \
                %s on one wire."
               n cx.scope n)
      end
      else Hashtbl.add seen n ()
    in
    let rec go (p : S.t) =
      match p.node with
      | S.Sym n -> name_ok p n
      | S.Vec (_ :: _ as ps) -> List.iter go ps
      | S.Map [ { S.node = S.Kw "keys"; _ }; { S.node = S.Vec (_ :: _ as ks); _ } ] ->
          List.iter
            (fun (k : S.t) ->
              match k.node with
              | S.Sym n -> name_ok k n
              | _ ->
                  ok := false;
                  err k "E_PATTERN" (msg_shape k))
            ks
      | _ ->
          ok := false;
          err p "E_PATTERN" (msg_shape p)
    in
    go pat;
    !ok

  (* bind [pat] to [v]; names get the path [pfx @ [marker ^ name]] *)
  let rec bind_parts (pat : S.t) (v : v) env pfx marker =
    match pat.node with
    | S.Sym n ->
        mark (pfx @ [ marker ^ n ]) v;
        Smap.add n v env
    | S.Vec ps ->
        let pk () = pattern_key pat in
        let et =
          if List.mem v.ty [ Ty.Vec2; Ty.Vec3; Ty.Vec4 ] then begin
            let width = match v.ty with Ty.Vec2 -> 2 | Ty.Vec4 -> 4 | _ -> 3 in
            if List.length ps > width then
              err pat "E_PATTERN"
                (Printf.sprintf "%s needs %d elements; a %s has %d." (pk ()) (List.length ps)
                   (show v.ty) width);
            Ty.Float
          end
          else
            match Ty.elem v.ty with
            | Some e -> e
            | None ->
                if v.ty <> Ty.Any then
                  err pat "E_PATTERN"
                    (Printf.sprintf "%s destructures a list or vec3; got %s." (pk ()) (show v.ty));
                Ty.Any
        in
        (match v.len with
        | Some n when (not (List.mem v.ty [ Ty.Vec2; Ty.Vec3; Ty.Vec4 ])) && n < List.length ps ->
            err pat "E_PATTERN"
              (Printf.sprintf "%s needs %d elements; the list has %d." (pk ()) (List.length ps) n)
        | _ -> ());
        let part = { v with ty = et; live_len = false; len = None; fn = None } in
        List.fold_left (fun env p -> bind_parts p part env pfx marker) env ps
    | S.Map [ _; { S.node = S.Vec ks; _ } ] ->
        let pk = pattern_key pat in
        (match v.ty with
        | Ty.Record _ | Ty.Any -> ()
        | t -> err pat "E_PATTERN" (Printf.sprintf "%s destructures a record; got %s." pk (show t)));
        List.fold_left
          (fun env (k : S.t) ->
            match k.node with S.Sym n -> bind_parts k (field k v n pk) env pfx marker | _ -> env)
          env ks
    | _ -> env

  let bind_pat (pat : S.t) v env pfx marker =
    (match pat.node with S.Sym _ -> () | _ -> mark (pfx @ [ marker ^ pattern_key pat ]) v);
    bind_parts pat v env pfx marker

  let poison_names (pat : S.t) env =
    List.fold_left (fun env n -> Smap.add n poison env) env (pat_names pat)

  let graph_terms : (string, graph) Hashtbl.t = Hashtbl.create 8
  let def_terms = table (fun s -> s.s_def_terms) 8

  (* [call_sites]: a [defn] named as a function value is checked on its own when it is met *)
  let check_def = ref (fun (_ : string) -> ())
  let ginfo : (string, v) Hashtbl.t = Hashtbl.create 8
  let gstack = ref []
  let def_defaults = table (fun s -> s.s_def_defaults) 8

  let unbound cx x s =
    let bound = List.map fst (Smap.bindings cx.env) in
    let recent = List.drop (max 0 (List.length bound - 6)) bound in
    bad x "E_UNBOUND"
      (Printf.sprintf "%s is not bound in %s.%s%s" s cx.scope
         (if bound = [] then "" else " Bound here: " ^ String.concat ", " recent ^ ".")
         (Catalog.suggestion s bound))

  let rec infer cx (x : S.t) : term * v =
    incr steps;
    if !steps > max_steps then raise Budget;
    let t, v = if x.meta <> [] then bypass cx x else plain cx x in
    (match t.node with
    | Hof _ | Loop _ | Op { op = "array/sum" | "image/map"; _ } ->
        Paths.iter (remember_root t.form) v.packed_producers
    | _ -> ());
    (t, precision cx.path v)

  and plain cx (x : S.t) : term * v =
    match x.node with
    | S.Num s -> (
        match S.number_value s with
        | Some f -> (
            match if String.contains s '.' then None else int_of_string_opt s with
            | Some n -> (tm x Ty.Int (Lit (Param.Int_value n)), leaf Ty.Int)
            | None -> (tm x Ty.Float (Lit (Param.Float_value f)), leaf Ty.Float))
        | _ -> bad x "E_TYPE" "Nonfinite number.")
    | S.Str s -> (tm x Ty.Text (Text s), leaf Ty.Text)
    | S.Kw k -> bad x "E_KEYWORD" (Printf.sprintf "Keyword :%s has no call to belong to." k)
    | S.Sym s -> symbol cx x s
    | S.Vec cs -> vector cx x cs
    | S.Map cs -> record cx x cs "A record"
    | S.Quote _ -> bad x "E_QUOTE" "Quotes belong in a defmacro template (`…)."
    | S.List [] -> bad x "E_EXPR" "Expected an expression."
    | S.List (h :: args) -> (
        match h.node with
        | S.Sym name -> call cx x name args
        | _ -> bad x "E_CALL" "A call head is a name.")

  and symbol cx x s =
    match s with
    | "t" -> (tm x Ty.Float Time, leaf ~live:true Ty.Float)
    | "pi" -> (tm x Ty.Float (Lit (Param.Float_value Float.pi)), leaf Ty.Float)
    | "true" | "false" -> (tm x Ty.Bool (Lit (Param.Bool_value (s = "true"))), leaf Ty.Bool)
    | "nil" -> (tm x Ty.geometry Nil, leaf Ty.geometry)
    | _ -> (
        let b, fs =
          if not (String.contains s '.') then (s, [])
          else match String.split_on_char '.' s with b :: fs -> (b, fs) | [] -> (s, [])
        in
        match Smap.find_opt b cx.env with
        | None when fs <> [] && List.mem b [ "t"; "pi"; "true"; "false"; "nil" ] ->
            bad x "E_FIELD"
              (Printf.sprintf "%s is a constant; it has no .%s." b (String.concat "." fs))
        | None -> unbound cx x b
        | Some v0 ->
            let v, _ = List.fold_left (fun (v, p) f -> (field x v f p, p ^ "." ^ f)) (v0, b) fs in
            (tm x v.ty (Ref_binding (b, fs)), if fs = [] then v else { v with fn = None }))

  and vector cx (x : S.t) (cs : S.t list) =
    let ty =
      match List.length cs with
      | 2 -> Some Ty.Vec2
      | 3 -> Some Ty.Vec3
      | 4 -> Some Ty.Vec4
      | _ -> None
    in
    if ty = None then
      bad x "E_VECTOR"
        (Printf.sprintf "A vector has 2, 3 or 4 components; this one has %d." (List.length cs))
    else begin
      let ty = Option.get ty in
      let rs = map_seq (infer cx) cs in
      List.iter2
        (fun c ((_, (v : v)) : term * v) ->
          match v.ty with
          | Ty.Int | Ty.Float | Ty.Any -> ()
          | t -> err c "E_TYPE" (Printf.sprintf "Vector components are numbers; got %s." (show t)))
        cs rs;
      let vs = List.map snd rs in
      (tm x ty (Vec (List.map fst rs)), { (derive ty vs) with approx = approximate vs })
    end

  and record cx (x : S.t) (items : S.t list) what =
    if List.length items mod 2 <> 0 then
      bad x "E_RECORD" (Printf.sprintf "%s is :key value pairs; a key has no value." what)
    else begin
      let fields = ref []
      and vs = ref []
      and terms = ref []
      and lengths = ref []
      and ok = ref true in
      List.iter
        (fun ((k : S.t), value) ->
          match k.node with
          | S.Kw n when Macro.valid_name n ->
              if List.mem_assoc n !fields then (
                ok := false;
                err k "E_RECORD" (Printf.sprintf "%s gives :%s twice." what n))
              else begin
                let t, v = infer cx value in
                no_fn value v.ty (Printf.sprintf "Record field :%s" n);
                fields := (n, v.ty) :: !fields;
                vs := v :: !vs;
                terms := (n, t) :: !terms;
                lengths :=
                  List.map
                    (fun p -> n :: p)
                    (if v.live_len then [] :: v.list_fields else v.list_fields)
                  @ !lengths
              end
          | _ ->
              ok := false;
              err k "E_RECORD"
                (Printf.sprintf "%s keys are keywords like :size; got %s." what (pattern_key k)))
        (pairs items);
      if not !ok then (tm x Ty.Any Nil, poison)
      else
        let ty = Ty.Record (List.rev !fields) in
        (tm x ty (Record (List.rev !terms)), { (derive ty !vs) with list_fields = !lengths })
    end

  and list_ cx (x : S.t) (args : S.t list) =
    let rs = map_seq (infer cx) args in
    let t =
      List.fold_left2
        (fun t a ((_, (v : v)) : term * v) ->
          no_fn a v.ty "A list element";
          match Ty.join t v.ty with
          | Some j -> j
          | None ->
              err a "E_TYPE"
                (Printf.sprintf "List elements share one type; got %s and %s." (show t) (show v.ty));
              t)
        Ty.Any args rs
    in
    let ty = Ty.List t in
    ( tm x ty (List_lit (List.map fst rs)),
      { (derive ty (List.map snd rs)) with len = Some (List.length args) } )

  and concat cx (x : S.t) (args : S.t list) =
    let rs = map_seq (infer cx) args in
    let t =
      List.fold_left2
        (fun t a ((_, (v : v)) : term * v) ->
          let vt = match v.ty with Ty.Any -> Ty.List Ty.Any | t -> t in
          if Ty.elem vt = None then (
            err a "E_TYPE" (Printf.sprintf "concat joins lists; got %s." (show v.ty));
            t)
          else
            match Ty.join t vt with
            | Some j -> j
            | None ->
                err a "E_TYPE"
                  (Printf.sprintf "concat joins lists of one type; got %s and %s." (show t)
                     (show v.ty));
                t)
        (Ty.List Ty.Any) args rs
    in
    let vs = List.map snd rs in
    let len =
      List.fold_left
        (fun a (v : v) -> match (a, v.len) with Some a, Some n -> Some (a + n) | _ -> None)
        (Some 0) vs
    in
    ( tm x t (List_op ("concat", List.map fst rs)),
      { (derive t vs) with live_len = List.exists (fun (v : v) -> v.live_len) vs; len } )

  and get cx (x : S.t) (args : S.t list) =
    match args with
    | [ r; { S.node = S.Kw f; _ } ] ->
        let rt, rv = infer cx r in
        (match rv.ty with
        | Ty.Record _ | Ty.Any -> ()
        | t -> err r "E_TYPE" (Printf.sprintf "get reads a record field; got %s." (show t)));
        let v = field x rv f (pattern_key r) in
        (tm x v.ty (Get (rt, f)), v)
    | _ -> bad x "E_ARGS" "get is (get record :field)."

  and assoc cx (x : S.t) (args : S.t list) =
    match args with
    | r :: (_ :: _ :: _ as rest) when List.length rest mod 2 = 0 -> (
        let rt, rv = infer cx r in
        match rv.ty with
        | Ty.Any -> (tm x Ty.Any (Assoc (rt, [])), { poison with live = rv.live })
        | Ty.Record fs0 ->
            let fs = ref fs0 and vs = ref [ rv ] and updates = ref [] in
            let lengths = ref rv.list_fields in
            List.iter
              (fun ((k : S.t), value) ->
                match k.node with
                | S.Kw n ->
                    let t, v = infer cx value in
                    no_fn value v.ty (Printf.sprintf "Record field :%s" n);
                    (match List.assoc_opt n !fs with
                    | Some want ->
                        if not (Ty.fits v.ty want) then
                          err value "E_TYPE"
                            (Printf.sprintf "assoc :%s is %s; got %s." n (show want) (show v.ty))
                    | None -> fs := !fs @ [ (n, v.ty) ]);
                    vs := v :: !vs;
                    updates := (n, t) :: !updates;
                    lengths :=
                      List.filter (function name :: _ -> name <> n | _ -> true) !lengths
                      @ List.map
                          (fun p -> n :: p)
                          (if v.live_len then [] :: v.list_fields else v.list_fields)
                | _ ->
                    err k "E_ARGS"
                      (Printf.sprintf "assoc keys are keywords; got %s." (pattern_key k)))
              (pairs rest);
            let ty = Ty.Record !fs in
            ( tm x ty (Assoc (rt, List.rev !updates)),
              { (derive ty !vs) with list_fields = !lengths } )
        | t -> bad x "E_TYPE" (Printf.sprintf "assoc updates a record; got %s." (show t)))
    | _ -> bad x "E_ARGS" "assoc is (assoc record :field value …)."

  and str cx (x : S.t) (args : S.t list) =
    let rs = map_seq (infer cx) args in
    (tm x Ty.Text (Str (List.map fst rs)), derive Ty.Text (List.map snd rs))

  and time_branch cx x test_live ty =
    if test_live && shape_ty ty then
      err x "E_TIME_BRANCH"
        (Printf.sprintf
           "A branch in %s picks between shapes by t. The network keeps its shape while playing; \
            pick a value instead, for example an if on a size or a colour."
           (path_text cx.path))

  and pure_boundary cx ~mapper code message term =
    let visited = Hashtbl.create 8 in
    let impure (t : term) = err t.form code message in
    let rec scan (t : term) =
      let all = List.iter scan and fields fields = List.iter (fun (_, t) -> scan t) fields in
      if mapper && Ty.is_cooked t.ty then impure t;
      match t.node with
      | State _ | Graph_ref _ -> impure t
      | Time when mapper -> impure t
      | Call { ctx; args; _ } ->
          if mapper || not (List.mem ctx [ Context.sop; Context.image ]) then impure t;
          fields args
      | Op { op; args; _ } ->
          (match find_op op cx.ctx with
          | Some op
            when if mapper then
                   op.ctx = Context.value && (not op.live)
                   && not (List.mem op.name body_context_ops)
                 else List.mem op.ctx [ Context.value; Context.sop; Context.image ] ->
              ()
          | _ -> impure t);
          fields args
      | Ref_binding (name, _) -> (
          match Smap.find_opt name cx.env with
          | Some value when mapper && (value.live || value.state_dep || Ty.is_cooked value.ty) ->
              impure t
          | _ -> ())
      | Fn_ref name -> helper t name
      | Call_fn { fn; args; body } -> (
          all args;
          match body with Some body -> scan body | None -> helper t fn)
      | Vec ts | List_lit ts | Str ts | List_op (_, ts) | Hof (_, ts) -> all ts
      | Let (bindings, body) ->
          fields bindings;
          scan body
      | Loop { accs; clauses; body; _ } ->
          fields accs;
          fields clauses;
          scan body
      | With_error (protected, handler) -> all [ protected; handler ]
      | Error_rows { rows; mapper = handler; _ } ->
          if mapper then impure t;
          all [ rows; handler ]
      | If (a, b, c) -> all [ a; b; c ]
      | Cond (arms, default) ->
          List.iter (fun (a, b) -> all [ a; b ]) arms;
          scan default
      | Case (value, arms, default) ->
          scan value;
          fields arms;
          scan default
      | Fn { body; capture; _ } ->
          scan body;
          Option.iter scan capture
      | Record fields_ -> fields fields_
      | Get (value, _) | Expanded { body = value; _ } | Bypass value -> scan value
      | Assoc (value, updates) ->
          scan value;
          fields updates
      | Lit _ | Text _ | Nil | Time -> ()
    and helper t name =
      if not (Hashtbl.mem visited name) then begin
        Hashtbl.add visited name ();
        match Hashtbl.find_opt def_terms name with
        | Some graph -> scan graph.body
        | None -> (
            match find_op name cx.ctx with
            | Some op when op.ctx = Context.value && ((not mapper) || not op.live) -> ()
            | _ -> impure t)
      end
    in
    scan term

  and with_error cx (x : S.t) args =
    match args with
    | [ protected; mapper ] ->
        let protected, pv = input cx "0" protected in
        let mapper, mv = fn_arg cx mapper in
        let payload =
          Ty.Record [ ("code", Ty.Text); ("message", Ty.Text); ("cause", Ty.List Ty.Text) ]
        in
        let argument =
          instantiate_fn ~coerce_params:false cx
            { key = None; aform = mapper.form; aterm = mapper; av = mv }
            { params = [ payload ]; result = payload }
        in
        let mapper, mv = (argument.aterm, argument.av) in
        (match mv.ty with
        | Ty.Fn (Some signature)
          when signature.params = [ payload ] && Ty.fits signature.result payload ->
            ()
        | _ ->
            err mapper.form "E_DIAGNOSTIC_MAP"
              "An error mapper takes and returns a diagnostic record.");
        no_fn protected.form pv.ty "A protected value";
        let rec cooked_container ty =
          Ty.is_cooked ty
          ||
          match ty with
          | Ty.List element -> cooked_container element
          | Record fields -> List.exists (fun (_, ty) -> cooked_container ty) fields
          | _ -> false
        in
        if cooked_container pv.ty && not (List.mem pv.ty [ Ty.geometry; Ty.image ]) then
          err protected.form "E_ERROR_SCOPE"
            "An error boundary completes a direct cooked result, not a cooked container.";
        pure_boundary cx ~mapper:false "E_ERROR_SCOPE"
          "An error boundary has no state, graph, drawing or UI effects." protected;
        if mv.live || mv.state_dep then
          err mapper.form "E_DIAGNOSTIC_MAP"
            "An error mapper is pure and independent of frame/state.";
        pure_boundary cx ~mapper:true "E_DIAGNOSTIC_MAP"
          "An error mapper is pure: no frame, state, context or cooked values." mapper;
        (tm x pv.ty (With_error (protected, mapper)), derive pv.ty [ pv; mv ])
    | _ -> bad x "E_ARITY" "with_error takes a protected value and a pure error mapper."

  and error_rows cx (x : S.t) args =
    let parse rows mapper policy =
      match policy.S.node with
      | S.Str "first" -> Some (rows, mapper, First)
      | S.Str "range-last" -> Some (rows, mapper, Range_last)
      | _ ->
          err policy "E_DIAGNOSTIC_POLICY" "Diagnostic policy is literal first or range-last.";
          None
    in
    let parsed =
      match args with
      | [ rows; mapper ] -> Some (rows, mapper, First)
      | [ rows; key; policy; mapper ] when key.S.node = S.Kw "policy" -> parse rows mapper policy
      | [ rows; mapper; key; policy ] when key.S.node = S.Kw "policy" -> parse rows mapper policy
      | _ -> None
    in
    match parsed with
    | None ->
        bad x "E_ARGS" "error_rows takes rows, a pure diagnostic mapper, and optional :policy text."
    | Some (rows, mapper, policy) ->
        let rows, rv = input cx "0" rows in
        if rv.ty <> Ty.Array Ty.Int then
          err rows.form "E_DIAGNOSTIC_ROWS" "Diagnostic rows require an integer 0/1 array.";
        let mapper, mv = fn_arg cx mapper in
        let payload =
          Ty.Record [ ("code", Ty.Text); ("message", Ty.Text); ("cause", Ty.List Ty.Text) ]
        in
        let argument =
          instantiate_fn ~coerce_params:false cx
            { key = None; aform = mapper.form; aterm = mapper; av = mv }
            { params = [ Ty.Int ]; result = payload }
        in
        let mapper, mv = (argument.aterm, argument.av) in
        (match mv.ty with
        | Ty.Fn (Some signature)
          when signature.params = [ Ty.Int ] && Ty.fits signature.result payload ->
            ()
        | _ ->
            err mapper.form "E_DIAGNOSTIC_MAP"
              "A row mapper takes an int and returns a diagnostic record.");
        if mv.live || mv.state_dep then
          err mapper.form "E_DIAGNOSTIC_MAP" "A row mapper is pure and independent of frame/state.";
        pure_boundary cx ~mapper:true "E_DIAGNOSTIC_MAP"
          "A row mapper is pure: no frame, state, context or cooked values." mapper;
        (tm x Ty.geometry (Error_rows { rows; policy; mapper }), derive Ty.geometry [ rv; mv ])

  and if_ cx (x : S.t) (args : S.t list) =
    match args with
    | [ c; a; b ] ->
        let ct, cv = branch_test cx "test" c in
        if not (Ty.fits cv.ty Ty.Bool) then
          err c "E_TYPE" (Printf.sprintf "if needs a bool condition, got %s." (show cv.ty));
        let at, av = branch cx "then" a and bt, bv = branch cx "else" b in
        if not (Ty.fits av.ty bv.ty || Ty.fits bv.ty av.ty) then
          err x "E_TYPE"
            (Printf.sprintf "Both branches of if must have one type: %s and %s." (show av.ty)
               (show bv.ty));
        no_fn a av.ty "An if branch";
        no_fn b bv.ty "An if branch";
        (* the type both branches fit: [(if c 3 [1 2 3])] is a list, not an int *)
        let ty =
          match Ty.join av.ty bv.ty with
          | Some t -> t
          | None -> if Ty.fits av.ty bv.ty then bv.ty else av.ty
        in
        time_branch cx x cv.live ty;
        let v = derive ty [ cv; av; bv ] in
        let lengths = if cv.live then union v.list_fields (list_fields ty) else v.list_fields in
        ( tm x ty (If (ct, at, bt)),
          {
            v with
            list_fields = lengths;
            approx = approximate [ cv; av; bv ];
            live_len = av.live_len || bv.live_len || has_empty lengths;
          } )
    | _ -> bad x "E_NO_ELSE" "if takes a condition, a then and an else."

  and cond cx (x : S.t) h (args : S.t list) =
    let usage =
      if h = "case" then "case is (case value literal result … :else result)."
      else "cond is (cond test value … :else value)."
    in
    let scrut, arms_forms =
      match (h, args) with
      | "case", s :: rest -> (Some s, rest)
      | "case", [] -> (None, [])
      | _ -> (None, args)
    in
    if (h = "case" && scrut = None) || arms_forms = [] || List.length arms_forms mod 2 <> 0 then
      bad x "E_COND" usage
    else begin
      let arms = pairs arms_forms in
      let last_test, _ = List.nth arms (List.length arms - 1) in
      if match last_test.node with S.Kw "else" -> false | _ -> true then
        bad x "E_NO_ELSE" (Printf.sprintf "%s needs a final :else arm, so it always has a value." h)
      else begin
        let sv = Option.map (branch_test cx "of") scrut in
        let vs = ref [] and ty = ref None in
        let test_live = ref (match sv with Some (_, v) -> v.live | None -> false) in
        let n = List.length arms in
        let terms =
          List.mapi
            (fun i ((c : S.t), e) ->
              let is_else = match c.node with S.Kw "else" -> true | _ -> false in
              if is_else && i < n - 1 then
                err c "E_COND" (Printf.sprintf ":else is the last arm of %s." h);
              let test =
                if is_else then None
                else
                  match sv with
                  | None ->
                      let t, v = branch_test cx ("test" ^ string_of_int i) c in
                      if not (Ty.fits v.ty Ty.Bool) then
                        err c "E_TYPE" (Printf.sprintf "cond tests are bool; got %s." (show v.ty));
                      if v.live then test_live := true;
                      vs := v :: !vs;
                      Some t
                  | Some (_, s) -> (
                      match c.node with
                      | S.Num _ | S.Str _ | S.Sym ("true" | "false") ->
                          let t, lv = infer cx c in
                          if not (Ty.fits lv.ty s.ty || Ty.fits s.ty lv.ty) then
                            err c "E_TYPE"
                              (Printf.sprintf "case compares %s with the %s literal %s." (show s.ty)
                                 (show lv.ty) (pattern_key c));
                          Some t
                      | _ ->
                          err c "E_CASE"
                            (Printf.sprintf
                               "case matches literal numbers, text or booleans; got %s."
                               (pattern_key c));
                          None)
              in
              let key =
                if is_else then "else" else if i = 0 then "then" else "then~" ^ string_of_int (i + 1)
              in
              let et, ev = branch cx key e in
              no_fn e ev.ty (Printf.sprintf "A %s arm" h);
              vs := ev :: !vs;
              (match !ty with
              | None -> ty := Some ev.ty
              | Some t -> (
                  match Ty.unify t ev.ty with
                  | Some j -> ty := Some j
                  | None ->
                      err e "E_TYPE"
                        (Printf.sprintf "All arms of %s must have one type: %s and %s." h (show t)
                           (show ev.ty))));
              (c, test, et))
            arms
        in
        let ty = Option.value !ty ~default:Ty.Any in
        time_branch cx x !test_live ty;
        let values = !vs @ match sv with Some (_, s) -> [ s ] | None -> [] in
        let v = derive ty values in
        let lengths = if !test_live then union v.list_fields (list_fields ty) else v.list_fields in
        let v =
          {
            v with
            list_fields = lengths;
            approx = approximate values;
            live_len = List.exists (fun v -> v.live_len) !vs || has_empty lengths;
          }
        in
        let default = match List.rev terms with (_, _, d) :: _ -> d | [] -> assert false in
        let node =
          match sv with
          | Some (st, _) ->
              Case
                ( st,
                  List.filter_map (fun ((c : S.t), t, e) -> Option.map (fun _ -> (c, e)) t) terms,
                  default )
          | None ->
              Cond (List.filter_map (fun (_, t, e) -> Option.map (fun t -> (t, e)) t) terms, default)
        in
        (tm x ty node, v)
      end
    end

  and branch_test cx key x =
    let path =
      match List.rev cx.path with
      | leaf :: outer -> List.rev outer @ [ leaf ^ "#" ^ key ]
      | [] -> [ "@result#" ^ key ]
    in
    binding cx x path

  and branch cx key (x : S.t) =
    let path =
      match List.rev cx.path with
      | leaf :: outer -> List.rev outer @ [ leaf ^ "#" ^ key ]
      | [] -> [ "@result#" ^ key ]
    in
    let t, v = body { cx with path } x in
    let t = if S.head x = Some "let*" then t else tm x v.ty (Let ([], t)) in
    mark path v;
    ({ t with path = Some path }, v)

  and body cx (b : S.t) : term * v =
    match (b.node, b.meta) with
    | S.List ({ S.node = S.Sym "let*"; _ } :: _), [] -> scope cx b cx.path
    | _ -> result cx b cx.path

  and result cx x path =
    let id = path @ [ "@result" ] in
    let t, v = binding cx x id in
    no_fn x v.ty (Printf.sprintf "The result of %s" cx.scope);
    (t, v)

  and binding cx (x : S.t) id : term * v =
    let cx = { cx with path = id } in
    let t, v =
      if x.meta <> [] then infer cx x
      else
        match x.node with
        | S.List ({ S.node = S.Sym ("for" | "fold" | "scan" | "sum"); _ } :: _) -> zone cx x id
        | S.List ({ S.node = S.Sym "let*"; _ } :: _) -> scope cx x id
        | S.List ({ S.node = S.Sym "state"; _ } :: _) -> state cx x id
        | S.List ({ S.node = S.Sym "fn"; _ } :: _) -> mk_fn cx x id "fn"
        | _ -> infer cx x
    in
    let v = precision id v in
    mark id v;
    ({ t with path = Some id }, v)

  and input cx key (x : S.t) : term * v =
    match S.head x with
    | Some h when String.contains h '/' || Hashtbl.mem macro_tbl h || opaque_head h ->
        let memo = (cx.path, key, x.id) in
        let id =
          match Hashtbl.find_opt input_paths memo with
          | Some id -> id
          | None ->
              let stem =
                match List.rev cx.path with
                | name :: scope -> List.rev scope @ [ name ^ "#" ^ key ]
                | [] -> [ "@result#" ^ key ]
              in
              (* Distinct maps in opaque expressions still need distinct function
                 sites; rechecking one closure body keeps its authored path. *)
              let n = Option.value ~default:0 (Hashtbl.find_opt inline_seen stem) in
              Hashtbl.replace inline_seen stem (n + 1);
              let id =
                if n = 0 then stem
                else
                  match List.rev stem with
                  | leaf :: scope -> List.rev scope @ [ Printf.sprintf "%s~%d" leaf n ]
                  | [] -> assert false
              in
              Hashtbl.add input_paths memo id;
              id
        in
        if opaque_head h then binding cx x id
        else
          let t, v = infer { cx with path = id } x in
          mark id v;
          (* Canvas previews need the inline result recorded at its card path.
             Scene and SOP lowering retain their operation's original site. *)
          ((if t.ty = Ty.drawing || t.ty = Ty.image then { t with path = Some id } else t), v)
    | _ -> infer cx x

  and state cx (x : S.t) id : term * v =
    match x.node with
    | S.List [ _; { S.node = S.Vec [ ({ S.node = S.Sym _; _ } as p); initial ]; _ }; step ] ->
        let seen = Hashtbl.create 1 in
        if not (check_pat cx p seen "state" true) then (tm x Ty.Any Nil, poison)
        else
          let init, iv = infer cx initial in
          exact_sink cx initial "state init" iv;
          no_fn initial iv.ty "A state seed";
          if iv.live then
            err initial "E_STATE_INIT" "A state seed is static; read the frame in the step.";
          if shape_ty iv.ty then
            err initial "E_STATE_TYPE" "State stores data, not deferred nodes or layouts.";
          let lengths = list_fields iv.ty in
          let env =
            bind_pat p
              {
                iv with
                live = true;
                state_dep = true;
                live_len = has_empty lengths;
                list_fields = lengths;
              }
              cx.env id ":"
          in
          let step, sv = body { cx with env; path = id; zbody = false } step in
          no_fn x sv.ty "A state step";
          let ty = Option.value ~default:iv.ty (Ty.unify iv.ty sv.ty) in
          if (not (Ty.fits sv.ty ty)) || shape_ty sv.ty then
            err x "E_STATE_TYPE" "A state step returns the seed's data type.";
          let v =
            {
              sv with
              ty;
              live = true;
              approx = false;
              approx_sources = Paths.empty;
              state_dep = true;
              packed_producers = Paths.empty;
              fn = None;
              list_fields = union sv.list_fields lengths;
              live_len = sv.live_len || has_empty lengths;
            }
          in
          (tm x ty (State { binder = pat_ir p; init; step; zone = id }), v)
    | _ -> bad x "E_STATE" "state is (state [s init] step)."

  and scope cx (x : S.t) path : term * v =
    match x.node with
    | S.List [ _; { S.node = S.Vec bs; _ }; res ] when List.length bs mod 2 = 0 ->
        let seen = Hashtbl.create 8 in
        let env = ref cx.env and binds = ref [] in
        List.iter
          (fun ((pat : S.t), (expr : S.t)) ->
            let ok = check_pat cx pat seen "let*" false in
            let id = path @ [ pattern_key pat ] in
            let inner = { cx with env = !env; path = id; zbody = false } in
            let t, v =
              match (pat.node, expr.node, expr.meta) with
              | S.Sym n, S.List ({ S.node = S.Sym "fn"; _ } :: _), [] ->
                  let t, v = mk_fn inner expr id n in
                  mark id v;
                  ({ t with path = Some id }, v)
              | _ -> binding inner expr id
            in
            if cx.zbody && not (List.exists (Int.equal cx.zone) v.vary) then
              invariant := Paths.add id !invariant;
            binds := (pat_ir pat, t) :: !binds;
            env := if ok then bind_pat pat v !env path "" else poison_names pat !env)
          (pairs bs);
        let rt, rv = result { cx with env = !env; zbody = false } res path in
        (tm x rv.ty (Let (List.rev !binds, rt)), rv)
    | _ -> bad x "E_LET" "let* needs [name expression …] and one result."

  and zone cx (x : S.t) id : term * v =
    match x.node with
    | S.List ({ S.node = S.Sym h; _ } :: args) -> (
        let kind = match h with "for" -> `For | "fold" -> `Fold | "scan" -> `Scan | _ -> `Sum in
        let is_acc = kind = `Fold || kind = `Scan in
        let shape =
          match args with
          | [ { S.node = S.Vec a; _ }; { S.node = S.Vec i; _ }; b ]
            when is_acc && List.length a mod 2 = 0 && List.length i mod 2 = 0 ->
              Some (pairs a, pairs i, b, [])
          | [ { S.node = S.Vec i; _ }; b ] when (not is_acc) && List.length i mod 2 = 0 ->
              Some ([], pairs i, b, [])
          | [ { S.node = S.Vec i; _ }; { S.node = S.Kw "skip"; _ }; sk; b ]
            when h = "for" && List.length i mod 2 = 0 ->
              Some ([], pairs i, b, skip_of sk)
          | _ -> None
        in
        match shape with
        | None ->
            bad x "E_ZONE"
              (if is_acc then Printf.sprintf "%s is (%s [acc init] [i collection] body)." h h
               else Printf.sprintf "%s is (%s [i collection …] body)." h h)
        | Some (accs, iters, b, skip) ->
            let seen = Hashtbl.create 8 in
            let ok =
              List.fold_left
                (fun ok ((p : S.t), _) -> check_pat cx p seen h true && ok)
                true (accs @ iters)
            in
            let ok =
              ok
              &&
              if iters = [] then (
                err x "E_ZONE" (Printf.sprintf "%s needs at least one [name collection] clause." h);
                false)
              else true
            in
            let ok =
              ok
              &&
              if is_acc && List.length accs <> 1 then (
                err x "E_ZONE"
                  (Printf.sprintf
                     "%s carries exactly one accumulator: [acc init]. Carry several values in a \
                      record: [{:keys [a b]} {:a 0 :b 1}]."
                     h);
                false)
              else true
            in
            if not ok then (tm x Ty.Any Nil, poison)
            else zone_typed cx x id h kind accs iters skip b)
    | _ -> bad x "E_ZONE" "Expected a loop form."

  and zone_typed cx x id h kind accs iters skip b =
    incr zones;
    let zid = !zones in
    let init =
      match accs with
      | [ (p, e) ] ->
          let t, v = infer cx e in
          no_fn e v.ty (Printf.sprintf "The %s accumulator" h);
          Some (p, t, v)
      | _ -> None
    in
    let name = last_segment id in
    let run acc_live =
      let env0 =
        match init with
        | Some (p, _, iv) ->
            bind_pat p
              { iv with live = acc_live; vary = union iv.vary [ zid ]; live_len = false }
              cx.env id ":"
        | None -> cx.env
      in
      let envs = ref env0 and clauses = ref [] and lens = ref [] and outer = ref [] in
      List.iter
        (fun ((p : S.t), (e : S.t)) ->
          let ct, cv = infer { cx with env = !envs } e in
          let et =
            match Ty.elem cv.ty with
            | Some t -> t
            | None ->
                if cv.ty <> Ty.Any then
                  err e "E_TYPE"
                    (Printf.sprintf
                       "%s in %s iterates a list (range, linspace, point_list …); got %s."
                       (pattern_key p) h (show cv.ty));
                Ty.Any
          in
          if
            cv.live_len && kind <> `Sum
            && (not
                  ((kind = `Fold || kind = `Scan)
                  && Option.fold ~none:false
                       ~some:(fun (_, _, initial) -> not (shape_ty initial.ty))
                       init))
            && match cv.ty with Ty.Array _ -> false | _ -> true
          then
            err e "E_TIME_COUNT"
              (Printf.sprintf
                 "The number of iterations of %s depends on t. Loop counts are fixed while \
                  playing; animate parameters instead, for example scale a piece to 0."
                 name);
          envs :=
            bind_pat p
              {
                cv with
                ty = et;
                live_len = false;
                len = None;
                fn = None;
                vary = union cv.vary [ zid ];
              }
              !envs id ":";
          clauses := (pat_ir p, ct) :: !clauses;
          lens := cv.len :: !lens;
          outer := cv :: !outer)
        iters;
      let inner =
        {
          cx with
          env = !envs;
          path = id;
          scope = cx.scope ^ " › " ^ name;
          zone = zid;
          zbody = true;
        }
      in
      let bt, bv = body inner b in
      (List.rev !clauses, List.rev !lens, !outer, bt, bv)
    in
    let acc_live0 = match init with Some (_, _, iv) -> iv.live | None -> false in
    let clauses, lens, outer, bt, bv =
      let ((_, _, _, _, bv) as r) = run acc_live0 in
      if init <> None && bv.live && not acc_live0 then run true else r
    in
    (* ponytail: literal counts only; a driven count is bounded when Eval runs the zone. *)
    if
      (not
         (List.exists
            (fun (_, (t : term)) -> match t.ty with Ty.Array _ -> true | _ -> false)
            clauses))
      && List.for_all (fun l -> match l with Some n -> n <= max_iterations | None -> false) lens
    then begin
      let total = List.fold_left (fun a l -> min (max_iterations + 1) (a * Option.get l)) 1 lens in
      if total > max_iterations then
        err x "E_ITER_BOUND" (Printf.sprintf "%s runs more than 4,096 iterations." name)
    end;
    let acc_ty = match init with Some (_, _, iv) -> Some iv.ty | None -> None in
    (match acc_ty with
    | Some at when not (Ty.fits bt.ty at) ->
        err x "E_ACC_TYPE"
          (Printf.sprintf "%s body must return the accumulator type %s; it returns %s." h (show at)
             (show bt.ty))
    | _ -> ());
    (* an int seed does not round a float body: the accumulator is the wider of the two *)
    let acc_ty = Option.map (fun at -> Option.value ~default:at (Ty.join at bt.ty)) acc_ty in
    let packed =
      List.exists (fun (_, (t : term)) -> match t.ty with Ty.Array _ -> true | _ -> false) clauses
    in
    let result_ty = match (kind, acc_ty) with `Fold, Some ty -> ty | _ -> bt.ty in
    let cooked_fold =
      kind = `Fold && cx.ctx = Context.sop && geometry_accumulator result_ty
      && List.exists (Hashtbl.mem node_sources) cx.stack
    in
    if packed && shape_ty result_ty && not cooked_fold then
      err x "E_ARRAY_TYPE"
        "Packed-array loops produce data; their length cannot create graph nodes.";
    let array_elem ty =
      match ty with
      | Ty.Int | Vec2 | Vec3 | Vec4 | Text -> ty
      | Float | Bool | Any -> Ty.Float
      | _ ->
          err x "E_ARRAY_TYPE" "Arrays hold ints, floats, vectors or text values.";
          Ty.Float
    in
    if
      kind = `Sum
      && not
           (match bt.ty with
           | Ty.Int | Ty.Float | Ty.Vec2 | Ty.Vec3 | Ty.Vec4 | Ty.Any -> true
           | _ -> false)
    then
      err x "E_TYPE"
        (Printf.sprintf "sum adds numbers or matching-width vectors; the body returns %s."
           (show bt.ty));
    let ty =
      match (kind, acc_ty) with
      | `Scan, Some at -> if packed then Ty.Array (array_elem at) else Ty.List at
      | (`For | `Scan), _ -> if packed then Ty.Array (array_elem bt.ty) else Ty.List bt.ty
      | `Sum, _ -> ( match bt.ty with Ty.Int | Vec2 | Vec3 | Vec4 -> bt.ty | _ -> Ty.Float)
      | `Fold, Some at -> at
      | `Fold, None -> Ty.Any
    in
    let init_vs = match init with Some (_, _, iv) -> [ iv ] | None -> [] in
    let ins = outer @ init_vs in
    let v =
      {
        (derive ty (bv :: ins)) with
        approx =
          kind = `For && skip = [] && packed_array ty
          && List.length clauses = 1
          && List.for_all (fun (p, _) -> match p with Name _ -> true | _ -> false) clauses
          && List.for_all (fun (_, (t : term)) -> packed_array t.ty) clauses
          && packed_body ~ops bt;
        vary =
          union
            (List.filter (( <> ) zid) bv.vary)
            (List.fold_left (fun a (v : v) -> union a v.vary) [] ins);
      }
    in
    let reasons =
      if kind <> `For || List.length clauses <> 1 then
        [ refusal x "E_GPU_FORM" "GPU kernels require collecting one-source loops." ]
      else
        List.concat_map
          (fun (pattern, source) ->
            match pattern with
            | Name _ -> source_refusal source
            | _ -> [ refusal x "E_PACKED_FORM" "Packed parameters require name patterns." ])
          clauses
    in
    root id x reasons;
    link_body id bt;
    ( tm x ty
        (Loop
           {
             kind;
             accs = (match init with Some (p, t, _) -> [ (pat_ir p, t) ] | None -> []);
             clauses;
             skip;
             body = bt;
             zone = id;
           }),
      { v with packed_producers = Paths.singleton id } )

  and mk_fn cx (x : S.t) id name : term * v =
    match x.node with
    | S.List [ _; { S.node = S.Vec ps; _ }; fbody ] ->
        let seen = Hashtbl.create 4 in
        let params =
          List.map
            (fun (p : S.t) ->
              let pat, ty =
                match p.node with
                | S.List [ pat; { S.node = S.Sym ":"; _ }; t ] -> (
                    match Ty.of_syntax t with
                    | Some ty -> (pat, Some ty)
                    | None ->
                        err p "E_PARAM"
                          (Printf.sprintf "Invalid fn parameter %s. Annotate it as (name : type)."
                             (pattern_key p));
                        (pat, None))
                | _ -> (p, None)
              in
              ignore (check_pat cx pat seen "fn" false);
              (pat, ty))
            ps
        in
        let c = { params; body = fbody; ccx = { cx with path = id }; cname = name; cid = id } in
        (* type the body with the declared types, or Any; call sites refine it *)
        let args =
          List.map
            (fun ((pat : S.t), ty) ->
              {
                key = None;
                aform = pat;
                aterm = tm pat (Option.value ty ~default:Ty.Any) (Ref_binding ("_", []));
                av = leaf (Option.value ty ~default:Ty.Any);
              })
            params
        in
        let bt, bv = call_closure cx x c args in
        let ir_params = List.map (fun (p, ty) -> (pat_ir p, ty)) params in
        ( tm x (Ty.Fn None) (Fn { params = ir_params; body = bt; zone = id; capture = None }),
          { (leaf ~live:bv.live (Ty.Fn None)) with state_dep = bv.state_dep; fn = Some (Closure c) }
        )
    | _ -> bad x "E_FN" "fn is (fn [params] body)."

  and call_closure cx x (c : closure) (args : arg list) : term * v =
    if List.length args <> List.length c.params then
      bad x "E_ARITY"
        (Printf.sprintf "%s takes %d argument%s; got %d." c.cname (List.length c.params)
           (plural (List.length c.params))
           (List.length args))
    else if cx.depth > max_call_depth then bad x "E_DEPTH" "Call depth exceeds 64."
    else begin
      let env = ref c.ccx.env in
      List.iter2
        (fun ((pat : S.t), ty) (a : arg) ->
          let v =
            match ty with
            | Some t ->
                ignore (need a (fun () -> Printf.sprintf "%s %s" c.cname (pattern_key pat)) t);
                { a.av with ty = Ty.coerce a.av.ty t }
            | None -> a.av
          in
          env := bind_pat pat v !env c.cid ":")
        c.params args;
      let inner =
        {
          c.ccx with
          env = !env;
          path = c.cid;
          scope = c.ccx.scope ^ " › " ^ c.cname;
          depth = cx.depth + 1;
          zbody = false;
          stack = List.sort_uniq compare (cx.stack @ c.ccx.stack);
        }
      in
      let bt, bv = body inner c.body in
      ( bt,
        {
          bv with
          fn = None;
          state_dep = bv.state_dep || List.exists (fun a -> a.av.state_dep) args;
        } )
    end

  and fn_arg cx (x : S.t) : term * v =
    let t, v =
      match x.node with
      | S.Sym s
        when x.meta = []
             && (not (String.contains s '.'))
             && (not (Smap.mem s cx.env))
             && not (reserved s) -> (
          match resolve_head cx s with
          | `Def _ ->
              if call_sites then !check_def s;
              (tm x (Ty.Fn None) (Fn_ref s), { (leaf (Ty.Fn None)) with fn = Some (Def_fn s) })
          | `Op o ->
              (tm x (Ty.Fn None) (Fn_ref s), { (leaf (Ty.Fn None)) with fn = Some (Op_fn o.name) })
          | `Kind k ->
              let slots = k.slots in
              if not (List.mem_assoc s !kind_fns) then
                kind_fns := (s, (k.qualified, k.context, slots)) :: !kind_fns;
              (tm x (Ty.Fn None) (Fn_ref s), { (leaf (Ty.Fn None)) with fn = Some (Kind_fn k) })
          | `Macro _ ->
              bad x "E_MACRO_AS_VALUE"
                (Printf.sprintf
                   "%s is a macro; a macro is not a function value. Wrap it: (fn [a] (%s a))." s s)
          | `Missing _ -> infer cx x)
      | _ -> infer cx x
    in
    (t, v)

  and fn_value cx x what =
    let t, v = fn_arg cx x in
    if not (Ty.fits v.ty (Ty.Fn None)) then
      err x "E_TYPE"
        (Printf.sprintf "%s expects a function (fn, a defn or an operator name); got %s." what
           (show v.ty));
    (t, v)

  and call_value cx x (f : v) (args : arg list) : term * v =
    match f.fn with
    | None ->
        let ty =
          match f.ty with
          | Ty.Fn (Some signature) ->
              if List.compare_lengths args signature.params <> 0 then
                err x "E_ARITY" "Function argument count differs from its declared signature."
              else
                List.iter2
                  (fun argument ty -> ignore (need argument (fun () -> "Function input") ty))
                  args signature.params;
              signature.result
          | _ -> Ty.Any
        in
        (tm x ty Nil, { (derive ty (f :: List.map (fun a -> a.av) args)) with fn = None })
    | Some (Closure c) -> call_closure cx x c args
    | Some (Def_fn n) -> apply_def cx x (Hashtbl.find sigs n) args
    | Some (Op_fn n) -> apply_op cx x (Option.get (find_op n cx.ctx)) args
    | Some (Kind_fn k) -> apply_kind cx x k args

  and dummy x ty = tm x ty (Ref_binding ("_", []))
  and value_arg x (v : v) = { key = None; aform = x; aterm = dummy x v.ty; av = v }

  and hof cx (x : S.t) h (args : S.t list) =
    let lo, hi = match h with "map" -> (2, 4) | "filter" | "sort-by" -> (2, 2) | _ -> (3, 3) in
    let n = List.length args in
    if n < lo || n > hi then
      bad x "E_ARITY"
        (match h with
        | "map" -> "map is (map f list …) with 1–3 lists."
        | "filter" -> "filter is (filter pred list)."
        | "reduce" -> "reduce is (reduce f init list)."
        | _ -> "sort-by is (sort-by key list).")
    else begin
      let f_form = List.hd args in
      let ft, fv =
        match S.head f_form with Some "fn" -> input cx "0" f_form | _ -> fn_value cx f_form h
      in
      let init = if h = "reduce" then Some (input cx "1" (List.nth args 1)) else None in
      (match init with Some (_, iv) -> no_fn x iv.ty "The reduce accumulator" | None -> ());
      let list_forms = List.drop (if h = "reduce" then 2 else 1) args in
      let lists =
        List.mapi
          (fun i (a : S.t) ->
            let t, v = input cx (string_of_int (i + if h = "reduce" then 2 else 1)) a in
            if Ty.elem v.ty = None && v.ty <> Ty.Any then
              err a "E_TYPE" (Printf.sprintf "%s iterates a list; got %s." h (show v.ty));
            (a, t, v))
          list_forms
      in
      let elem (_, _, (v : v)) =
        {
          v with
          ty = Option.value (Ty.elem v.ty) ~default:Ty.Any;
          live_len = false;
          len = None;
          fn = None;
        }
      in
      let call_args =
        match (init, lists) with
        | Some (_, iv), l :: _ -> [ value_arg x iv; value_arg x (elem l) ]
        | _ -> List.map (fun l -> value_arg x (elem l)) lists
      in
      let result, rv = call_value cx x fv call_args in
      let ft =
        match fv.fn with
        | Some (Op_fn _ | Kind_fn _) -> ft
        | _ ->
            (instantiate_fn ~coerce_params:false cx
               { key = None; aform = f_form; aterm = ft; av = fv }
               { Ty.params = List.map (fun (a : arg) -> a.av.ty) call_args; result = rv.ty })
              .aterm
      in
      let l0 = match lists with (_, _, v) :: _ -> v | [] -> poison in
      let packed =
        List.exists (fun (_, _, (v : v)) -> match v.ty with Ty.Array _ -> true | _ -> false) lists
      in
      if packed then begin
        List.iter
          (fun (a, _, (v : v)) ->
            match v.ty with
            | Ty.Array _ -> ()
            | _ -> err a "E_ARRAY_TYPE" "A packed map combines packed arrays.")
          lists;
        if shape_ty rv.ty then
          err x "E_ARRAY_TYPE" "Packed-array functions produce data, not graph nodes."
      end;
      let list_vs = List.map (fun (_, _, v) -> v) lists in
      let any_len = List.exists (fun (v : v) -> v.live_len) list_vs in
      let terms =
        (ft :: (match init with Some (t, _) -> [ t ] | None -> []))
        @ List.map (fun (_, t, _) -> t) lists
      in
      let kind, ty, live_len =
        match h with
        | "map" ->
            no_fn x rv.ty "The map result";
            let ty =
              if packed then
                Ty.Array
                  (match rv.ty with
                  | Ty.Int | Vec2 | Vec3 | Vec4 | Text -> rv.ty
                  | Float | Bool | Any -> Ty.Float
                  | _ ->
                      err x "E_ARRAY_TYPE" "Arrays hold ints, floats, vectors or text values.";
                      Ty.Float)
              else Ty.List rv.ty
            in
            (`Map, ty, any_len)
        | "filter" ->
            if not (Ty.fits rv.ty Ty.Bool) then
              err x "E_TYPE"
                (Printf.sprintf "filter's predicate returns bool; it returns %s." (show rv.ty));
            (`Filter, l0.ty, any_len || rv.live)
        | "sort-by" ->
            if not (match rv.ty with Ty.Int | Ty.Float | Ty.Bool | Ty.Any -> true | _ -> false)
            then
              err x "E_TYPE"
                (Printf.sprintf "sort-by's key returns a number; it returns %s." (show rv.ty));
            (`Sort_by, l0.ty, any_len)
        | _ ->
            let it = match init with Some (_, iv) -> iv.ty | None -> Ty.Any in
            no_fn x rv.ty "The reduce result";
            if not (Ty.fits rv.ty it) then
              err x "E_TYPE"
                (Printf.sprintf
                   "reduce's function must return the accumulator type %s; it returns %s." (show it)
                   (show rv.ty));
            (`Reduce, Option.value ~default:it (Ty.join it rv.ty), false)
      in
      let v =
        derive ty ((fv :: rv :: list_vs) @ match init with Some (_, iv) -> [ iv ] | None -> [])
      in
      let specialized =
        match result.node with Call_fn { body = Some body; _ } -> body | _ -> result
      in
      let eligible = packed_body ~ops specialized in
      let params =
        match fv.fn with
        | Some (Closure c) -> Some c.params
        | Some (Def_fn name) ->
            Option.map
              (fun d -> List.map (fun (name, ty, _) -> (S.make (S.Sym name), Some ty)) d.sparams)
              (Hashtbl.find_opt sigs name)
        | _ -> None
      in
      let inputs_fit =
        Option.fold ~none:true
          ~some:(fun params ->
            List.length params = List.length lists
            && List.for_all2
                 (fun ((p : S.t), annotation) (_, _, (v : v)) ->
                   (match p.node with S.Sym _ -> true | _ -> false)
                   && (annotation = None || annotation = Ty.elem v.ty))
                 params lists)
          params
      in
      let reasons =
        if kind <> `Map then [ refusal x "E_GPU_FORM" "GPU kernels require collecting maps." ]
        else
          List.concat_map (fun (_, t, _) -> source_refusal t) lists
          @ Option.fold ~none:[]
              ~some:(fun params ->
                if List.length params <> List.length lists then
                  [ refusal x "E_PACKED_FORM" "Packed parameter count differs from its sources." ]
                else
                  List.concat
                    (List.map2
                       (fun ((p : S.t), annotation) (_, _, (v : v)) ->
                         match (p.node, annotation, Ty.elem v.ty) with
                         | S.Sym _, _, None when v.ty = Ty.Any -> []
                         | S.Sym _, _, Some Ty.Any | S.Sym _, None, _ -> []
                         | S.Sym _, Some want, Some have when want = have -> []
                         | S.Sym _, _, _ ->
                             [
                               refusal x "E_PACKED_TYPE"
                                 "Packed parameter annotation differs from its source.";
                             ]
                         | _ ->
                             [
                               refusal x "E_PACKED_FORM" "Packed parameters require name patterns.";
                             ])
                       params lists))
              params
      in
      root cx.path x reasons;
      link_body cx.path specialized;
      (match fv.fn with
      | Some (Closure c) -> link c.cid (Paths.singleton cx.path)
      | Some (Def_fn name) -> link [ "def:" ^ name ] (Paths.singleton cx.path)
      | _ -> ());
      ( tm x ty (Hof (kind, terms)),
        {
          v with
          live_len;
          live = v.live || live_len;
          packed_producers = Paths.singleton cx.path;
          approx =
            kind = `Map && packed_array ty && inputs_fit
            && List.for_all (fun (_, _, (v : v)) -> packed_array v.ty) lists
            && eligible;
        } )
    end

  and iterate cx (x : S.t) (args : S.t list) =
    let parsed =
      match args with
      | [ n; init; f ] -> Some (n, init, f, None)
      | [ n; init; f; { S.node = S.Kw "until"; _ }; g ] -> Some (n, init, f, Some g)
      | _ -> None
    in
    match parsed with
    | None ->
        bad x "E_ARITY"
          "iterate is (iterate n init (fn [x] body)) with an optional :until (fn [prev next] bool)."
    | Some (n_form, init_form, f_form, until_form) ->
        let nt, nv = input cx "0" n_form in
        if (not (Ty.fits nv.ty Ty.Int)) && nv.ty <> Ty.Any then
          err n_form "E_TYPE" (Printf.sprintf "iterate's count is an int; got %s." (show nv.ty));
        let it, iv = input cx "1" init_form in
        no_fn x iv.ty "The iterate value";
        let fn_term key (form : S.t) =
          match S.head form with Some "fn" -> input cx key form | _ -> fn_value cx form "iterate"
        in
        let ft, fv = fn_term "2" f_form in
        let arg v = value_arg x { v with live_len = false; len = None; fn = None } in
        let result, rv = call_value cx x fv [ arg iv ] in
        no_fn x rv.ty "The iterate result";
        if not (Ty.fits rv.ty iv.ty) then
          err x "E_TYPE"
            (Printf.sprintf "iterate's function must return the value type %s; it returns %s."
               (show iv.ty) (show rv.ty));
        let ty = Option.value ~default:iv.ty (Ty.join iv.ty rv.ty) in
        if shape_ty ty then
          err x "E_ARRAY_TYPE" "iterate carries data; it cannot create graph nodes.";
        let instantiate key form t v params result_ty =
          match v.fn with
          | Some (Op_fn _ | Kind_fn _) -> t
          | _ ->
              (instantiate_fn ~coerce_params:false cx
                 { key = None; aform = form; aterm = t; av = v }
                 { Ty.params; result = result_ty })
                .aterm
              |> fun t ->
              ignore key;
              t
        in
        let ft = instantiate "2" f_form ft fv [ iv.ty ] rv.ty in
        let until =
          Option.map
            (fun g ->
              let gt, gv = fn_term ":until" g in
              let _, uv = call_value cx x gv [ arg iv; arg rv ] in
              if not (Ty.fits uv.ty Ty.Bool) then
                err g "E_TYPE"
                  (Printf.sprintf "iterate's :until returns bool; it returns %s." (show uv.ty));
              (instantiate ":until" g gt gv [ iv.ty; rv.ty ] Ty.Bool, gv, uv))
            until_form
        in
        let specialized =
          match result.node with Call_fn { body = Some body; _ } -> body | _ -> result
        in
        let extra = match until with Some (_, gv, uv) -> [ gv; uv ] | None -> [] in
        let v = derive ty (fv :: rv :: iv :: nv :: extra) in
        root cx.path x [ refusal x "E_GPU_FORM" "GPU kernels require collecting maps." ];
        link_body cx.path specialized;
        (match fv.fn with
        | Some (Closure c) -> link c.cid (Paths.singleton cx.path)
        | Some (Def_fn name) -> link [ "def:" ^ name ] (Paths.singleton cx.path)
        | _ -> ());
        let terms = ft :: it :: nt :: (match until with Some (t, _, _) -> [ t ] | None -> []) in
        ( tm x ty (Hof (`Iterate, terms)),
          { v with live_len = false; packed_producers = Paths.singleton cx.path } )

  and ref_ cx (x : S.t) (args : S.t list) =
    match args with
    | ({ S.node = S.Sym g; _ } as gf) :: rest ->
        if not (Hashtbl.mem gsigs g) then
          bad gf "E_UNKNOWN_GRAPH" (Printf.sprintf "Unknown graph reference: %s." g)
        else if cx.in_def then
          bad x "E_REF_IN_DEF"
            "Reusable functions cannot capture project graphs; add an explicit parameter."
        else begin
          let sg = Hashtbl.find gsigs g in
          let gv = graph_info x g in
          let overrides = ref [] and vs = ref [ gv ] in
          let rec go = function
            | ({ S.node = S.Kw k; _ } as kf) :: value :: more ->
                (match List.find_opt (fun (n, _, _) -> n = k) sg.sparams with
                | None ->
                    err kf "E_UNKNOWN_PARAM"
                      (Printf.sprintf "Graph %s has no input :%s. Inputs: %s." g k
                         (if sg.sparams = [] then "none"
                          else String.concat ", " (List.map (fun (n, _, _) -> n) sg.sparams)))
                | Some (_, ty, _) ->
                    let t, v = infer cx value in
                    exact_sink cx value ("ref " ^ g ^ " :" ^ k) v;
                    no_fn value v.ty (Printf.sprintf "Input :%s of %s" k g);
                    ignore (need (value_arg value v) (fun () -> Printf.sprintf ":%s of %s" k g) ty);
                    overrides := (k, t) :: !overrides;
                    vs := v :: !vs);
                go more
            | [] -> ()
            | y :: _ -> err y "E_ARGS" "ref takes a graph name, then :input value pairs."
          in
          go rest;
          let ty = context_ty sg.sctx in
          ( tm x ty (Graph_ref { graph = g; inputs = List.rev !overrides }),
            { (derive ty !vs) with groups = []; approx = gv.approx } )
        end
    | _ -> bad x "E_ARGS" "ref takes a graph name, then :input value pairs."

  and args_of cx fn_slot (forms : S.t list) : arg list =
    let read_fn form =
      match form.S.node with
      | S.List ({ node = S.Sym "list"; _ } :: elements) ->
          let entries = List.map (fn_arg cx) elements in
          ( tm form (Ty.List (Ty.Fn None)) (List_lit (List.map fst entries)),
            {
              (derive (Ty.List (Ty.Fn None)) (List.map snd entries)) with
              len = Some (List.length entries);
            } )
      | _ -> fn_arg cx form
    in
    let rec go i acc = function
      | [] -> List.rev acc
      | ({ S.node = S.Kw k; _ } as kf) :: value :: rest ->
          let t, v =
            if fn_slot None (Some k) && S.head value <> Some "fn" then read_fn value
            else input cx (":" ^ k) value
          in
          go i ({ key = Some k; aform = kf; aterm = t; av = v } :: acc) rest
      | ({ S.node = S.Kw k; _ } as kf) :: [] ->
          err kf "E_MISSING_VALUE" (Printf.sprintf ":%s has no value" k);
          List.rev acc
      | y :: rest ->
          let t, v =
            if fn_slot (Some i) None && S.head y <> Some "fn" then read_fn y
            else input cx (index_name i) y
          in
          go (i + 1) ({ key = None; aform = y; aterm = t; av = v } :: acc) rest
    in
    go 0 [] forms

  and call cx (x : S.t) name args : term * v =
    match name with
    | "let*" -> scope cx x (inline cx "~let")
    | "state" -> state cx x (inline cx "~state")
    | "for" | "fold" | "scan" | "sum" -> zone cx x (inline cx ("~" ^ name))
    | "fn" -> mk_fn cx x (inline cx "~fn") "fn"
    | "with_error" -> with_error cx x args
    | "error_rows" -> error_rows cx x args
    | "if" -> if_ cx x args
    | "cond" | "case" -> cond cx x name args
    | "ref" -> ref_ cx x args
    | "values" -> record cx x args "values"
    | "list" -> list_ cx x args
    | "concat" -> concat cx x args
    | "str" -> str cx x args
    | "get" -> get cx x args
    | "assoc" -> assoc cx x args
    | "map" | "filter" | "reduce" | "sort-by" -> hof cx x name args
    | "iterate" -> iterate cx x args
    | "quote" | "quasiquote" | "unquote" | "unquote-splicing" ->
        bad x "E_QUOTE" (Printf.sprintf "%s belongs in a defmacro template (`…)." name)
    | "workspace" | "graph" | "defn" | "defnode" | "defmacro" ->
        bad x "E_FORM" (Printf.sprintf "%s is a top-level form of a workspace." name)
    | _ -> (
        let local = Smap.find_opt name cx.env in
        match local with
        | Some lv when Ty.fits lv.ty (Ty.Fn None) ->
            if List.exists is_kw args then
              bad x "E_ARGS"
                (Printf.sprintf "%s is a local function; it takes positional arguments only." name)
            else begin
              let a = args_of cx (fun _ _ -> false) args in
              let result, rv = call_value cx x lv a in
              let body =
                match (lv.fn, result.node) with
                | Some (Closure _), _ -> Some result
                | Some (Def_fn _), Call_fn { body; _ } -> body
                | _ -> None
              in
              (tm x rv.ty (Call_fn { fn = name; args = List.map (fun a -> a.aterm) a; body }), rv)
            end
        | _ -> (
            match resolve_head cx name with
            | `Missing (code, msg) -> (
                match local with
                | Some lv ->
                    bad x "E_TYPE" (Printf.sprintf "%s is %s, not a function." name (show lv.ty))
                | None ->
                    let prefix = "Unknown node " ^ name ^ "." in
                    if code = "E_UNKNOWN_KIND" && String.starts_with ~prefix msg then
                      bad x code
                        (Printf.sprintf "Unknown operator “%s”.%s" name
                           (String.sub msg (String.length prefix)
                              (String.length msg - String.length prefix)))
                    else bad x code msg)
            | `Macro _ -> (
                match Macro.expand (List.rev !macro_forms) x with
                | Error d ->
                    diags := d :: !diags;
                    (tm x Ty.Any Nil, poison)
                | Ok y ->
                    let t, v = infer cx y in
                    (tm x t.ty (Expanded { macro = name; body = t }), v))
            | `Def d ->
                let fn_slot pos key =
                  let p =
                    match (pos, key) with
                    | Some i, _ -> List.nth_opt d.sparams i
                    | None, Some k -> List.find_opt (fun (n, _, _) -> n = k) d.sparams
                    | _ -> None
                  in
                  match p with Some (_, Ty.Fn None, _) -> true | _ -> false
                in
                apply_def cx x d (args_of cx fn_slot args)
            | `Op o when o.name = "scene/merge" ->
                (* [:skip] is read here: its value is a list of tuples, not an argument *)
                let rec split = function
                  | { S.node = S.Kw "skip"; _ } :: v :: rest ->
                      let s, r = split rest in
                      (skip_of v @ s, r)
                  | y :: rest ->
                      let s, r = split rest in
                      (s, y :: r)
                  | [] -> ([], [])
                in
                let skip, args = split args in
                apply_op ~skip cx x o (args_of cx (fun _ _ -> false) args)
            | `Op o ->
                let slots = o.signature.pos @ o.signature.opt in
                let fn_slot pos key =
                  let ty =
                    match (pos, key) with
                    | Some i, _ -> Option.map snd (List.nth_opt slots i)
                    | _, Some key -> List.assoc_opt key o.signature.kw
                    | _ -> None
                  in
                  match ty with Some (Ty.Fn _) -> true | _ -> false
                in
                apply_op cx x o (args_of cx fn_slot (S.attribute_args o.name args))
            | `Kind k ->
                let fn_slot pos key =
                  let slot =
                    match (pos, key) with
                    | Some i, _ -> positional_slot k.slots i
                    | _, Some name ->
                        List.find_opt (fun (s : Catalog.slot) -> s.name = name) k.slots
                    | _ -> None
                  in
                  Option.fold ~none:false
                    ~some:(fun (s : Catalog.slot) ->
                      match s.ty with Some (Ty.Fn _) -> true | _ -> false)
                    slot
                  || Option.fold ~none:false
                       ~some:(fun name ->
                         List.exists
                           (fun (p : Catalog.parameter) ->
                             p.name = name
                             && match p.ty with Some (Port_type.Fn _) -> true | _ -> false)
                           k.parameters)
                       key
                in
                apply_kind cx x k (args_of cx fn_slot args)))

  and apply_op ?(skip = []) cx x (o : Op.t) (args : arg list) : term * v =
    if mem_s o.name body_context_ops && not cx.in_def then
      err x "E_NODE_CONTEXT"
        "Context seed, frame and provenance base are available inside a defnode body.";
    let position = ref 0 and slots = o.signature.pos @ o.signature.opt in
    let args =
      List.map
        (fun a ->
          let ty =
            match a.key with
            | Some key -> assoc_s key o.signature.kw
            | None ->
                let i = !position in
                incr position;
                Option.map snd (List.nth_opt slots i)
          in
          match ty with Some (Ty.Fn (Some signature)) -> instantiate_fn cx a signature | _ -> a)
        args
    in
    if
      o.name <> "scene/trace"
      && (has_prefix "sop/" o.name || has_prefix "settings/" o.name || has_prefix "scene/" o.name)
    then begin
      (* The values of an attribute write enter the geometry through the SOP input sink, an
         implicit readback: the executor reads a GPU producer back once, so no (exact x). *)
      let values_slot = List.length o.signature.pos - 1 in
      let position = ref 0 in
      List.iter
        (fun a ->
          let slot =
            if a.key = None then (
              let i = !position in
              incr position;
              Some i)
            else None
          in
          let readback =
            o.name = "sop/with_attr"
            && (a.key = Some "values" || (a.key = None && slot = Some values_slot))
          in
          if not readback then exact_sink cx a.aform o.name a.av)
        args
    end;
    let args =
      if o.name = "ui/canvas" then
        List.map
          (fun a ->
            if a.key = None && a.av.ty = Ty.image then
              {
                a with
                aterm =
                  {
                    a.aterm with
                    ty = Ty.drawing;
                    node = Op { op = "draw/image"; args = [ ("image", a.aterm) ]; skip = [] };
                  };
                av = { a.av with ty = Ty.drawing };
              }
            else a)
          args
      else args
    in
    if o.ctx <> Context.value && o.ctx <> cx.ctx then
      bad x "E_WRONG_CONTEXT"
        (Printf.sprintf
           "%s belongs to %s; it cannot run in %s. Pass data through a typed input or ref." o.name
           (context_name o.ctx) (context_name cx.ctx))
    else begin
      let pos = List.filter (fun a -> a.key = None) args in
      let kws = List.filter (fun a -> a.key <> None) args in
      let slots = o.signature.pos @ o.signature.opt in
      let np = List.length pos in
      if o.signature.rest = None && (np < List.length o.signature.pos || np > List.length slots)
      then
        bad x "E_ARITY"
          (Printf.sprintf "%s takes %d%s positional input%s%s; got %d." o.name
             (List.length o.signature.pos)
             (if o.signature.opt <> [] then "–" ^ string_of_int (List.length slots) else "")
             (plural (List.length slots))
             (if o.signature.pos <> [] then
                " (" ^ String.concat ", " (List.map fst o.signature.pos) ^ ")"
              else "")
             np)
      else begin
        let nfix =
          if o.signature.rest <> None then List.length o.signature.pos else List.length slots
        in
        let named = ref [] in
        List.iteri
          (fun i a ->
            if i < nfix then begin
              let sname, sty = List.nth slots i in
              if
                not
                  (o.any_num
                  &&
                  match a.av.ty with
                  | Ty.Vec3 | Int | Float -> true
                  | Vec2 | Vec4 -> Option.is_some o.arithmetic
                  | _ -> false)
              then ignore (need a (fun () -> Printf.sprintf "%s %s" o.name sname) sty);
              named := (sname, a.aterm) :: !named
            end
            else begin
              let rname, rty = Option.get o.signature.rest in
              (match Ty.elem a.av.ty with
              | Some e when Ty.fits e rty ->
                  if a.av.live_len && shape_ty rty then
                    err a.aform "E_TIME_COUNT"
                      (Printf.sprintf
                         "The list passed to %s changes length with t. The network keeps its shape \
                          while playing; animate parameters instead, for example scale a piece to \
                          0."
                         o.name)
              | _ -> ignore (need a (fun () -> Printf.sprintf "%s %s" o.name rname) rty));
              named := (rname, a.aterm) :: !named
            end)
          pos;
        let seen = ref [] in
        List.iter
          (fun a ->
            let k = Option.get a.key in
            match assoc_s k o.signature.kw with
            | None ->
                err a.aform "E_UNKNOWN_PARAM"
                  (Printf.sprintf "%s has no parameter :%s.%s" o.name k
                     (if o.signature.kw = [] then ""
                      else
                        " Parameters: "
                        ^ String.concat " " (List.map (fun (n, _) -> ":" ^ n) o.signature.kw)
                        ^ "."))
            | Some want ->
                if mem_s k !seen then
                  err a.aform "E_DUPLICATE_PARAM" (Printf.sprintf ":%s is given twice" k)
                else begin
                  seen := k :: !seen;
                  ignore (need a (fun () -> Printf.sprintf "%s :%s" o.name k) want);
                  named := (k, a.aterm) :: !named
                end)
          kws;
        if o.any_num then begin
          let vectors =
            List.filter_map
              (fun a -> match a.av.ty with Ty.Vec2 | Vec3 | Vec4 -> Some a.av.ty | _ -> None)
              pos
            |> List.sort_uniq compare
          in
          if List.length vectors > 1 then
            err x "E_TYPE"
              (Printf.sprintf "%s needs matching vector widths; got %s." o.name
                 (String.concat " and " (List.map show vectors)))
        end;
        literal_checks x o args;
        let rec literal (term : term) =
          match term.node with
          | Lit value -> Some value
          | Text text -> Some (Param.Text_value text)
          | Expanded { body; _ } -> literal body
          | _ -> None
        in
        let ty =
          try
            Op.result_type o
              (List.rev_map (fun (name, (term : term)) -> (name, term.ty, literal term)) !named)
          with Diagnostic.Error d ->
            diags := { d with span = Some x.span } :: !diags;
            Ty.Any
        in
        let avs = List.map (fun a -> a.av) args in
        let nth_live_len i =
          match List.nth_opt pos i with Some a -> a.av.live_len | None -> false
        in
        let live_len =
          match o.name with
          | "range" | "linspace" -> List.exists (fun (v : v) -> v.live) avs
          | "take" | "drop" ->
              (match List.nth_opt pos 0 with Some a -> a.av.live | None -> false)
              || nth_live_len 1
          | "rest" | "reverse" -> nth_live_len 0
          | _ -> false
        in
        let int_of a = match a.aterm.node with Lit (Param.Int_value n) -> Some n | _ -> None in
        let len =
          match (o.name, List.map int_of pos) with
          | "range", [ Some n ] -> Some (max 0 n)
          | "range", [ Some a; Some b ] -> Some (max 0 (b - a))
          | "linspace", [ _; _; Some n ] -> Some (max 0 n)
          | _ -> None
        in
        let derived = derive ty avs in
        let capability = Op.packed_kind o in
        let exact = capability = Some Op.Exact in
        let v =
          {
            derived with
            live = o.live || List.exists (fun (v : v) -> v.live) avs;
            live_len;
            len;
            approx =
              (match capability with
              | Some (Op.Binary _ | Unary _ | Length | Noise3 | Fma) -> approximate avs
              | _ -> false);
            packed_producers =
              (if exact || shape_ty ty then Paths.empty else derived.packed_producers);
            approx_sources = (if exact || shape_ty ty then Paths.empty else derived.approx_sources);
            list_fields = (if o.live then list_fields ty else []);
          }
        in
        if o.name = "array/sum" then begin
          root cx.path x [ refusal x "E_GPU_FORM" "Ordered reductions stay on the CPU." ];
          ( tm x ty (Op { op = o.name; args = List.rev !named; skip }),
            { v with packed_producers = Paths.singleton cx.path } )
        end
        else if o.name = "image/map" then begin
          root cx.path x [];
          List.iter
            (fun a ->
              match a.aterm.node with
              | Fn { body; zone; _ } ->
                  link_body cx.path body;
                  link zone (Paths.singleton cx.path)
              | _ -> ())
            args;
          ( tm x ty (Op { op = o.name; args = List.rev !named; skip }),
            { v with packed_producers = Paths.singleton cx.path } )
        end
        else (tm x ty (Op { op = o.name; args = List.rev !named; skip }), v)
      end
    end

  and literal_checks x (o : Op.t) (args : arg list) =
    let pos = List.filter (fun a -> a.key = None) args in
    let int_of a = match a.aterm.node with Lit (Param.Int_value n) -> Some n | _ -> None in
    let num_of a =
      match a.aterm.node with
      | Lit (Param.Int_value n) -> Some (float_of_int n)
      | Lit (Param.Float_value f) -> Some f
      | _ -> None
    in
    match o.name with
    | "sop/context_seed" -> (
        match pos with
        | [ a ] -> (
            match a.aterm.node with
            | Text namespace when String.trim namespace <> "" -> ()
            | _ ->
                err a.aform "E_SEED_NAMESPACE" "Context seed requires a literal nonblank namespace."
            )
        | _ -> ())
    | "image/map" ->
        List.iter
          (fun a ->
            match a.av.ty with
            | Ty.Fn (Some { params = [ Ty.Vec2 ]; result = Ty.Vec4 }) | Ty.Fn None | Ty.Any -> ()
            | _ -> err a.aform "E_TYPE" "image/map needs a vec2-to-vec4 pixel function.")
          pos;
        List.iter
          (fun a ->
            match (a.key, int_of a) with
            | Some ("width" | "height"), Some n when n <= 0 ->
                err a.aform "E_RANGE" "Image dimensions must be positive."
            | _ -> ())
          args
    | "length" ->
        List.iter
          (fun a ->
            if a.av.ty <> Ty.Vec3 && a.av.ty <> Ty.Any then
              err a.aform "E_TYPE" "length needs a vec3.")
          pos
    | "material/standard" ->
        List.iter
          (fun a ->
            match (a.key, a.aterm.node) with
            | Some ("color" | "emission"), Text s when not (hex_colour s) ->
                err a.aform "E_TYPE" "A material colour is #rrggbb or a vec3."
            | Some "roughness", _ -> (
                match num_of a with
                | Some f when f < 0. || f > 1. ->
                    err a.aform "E_RANGE" "Material roughness must be in [0,1]."
                | _ -> ())
            | _ -> ())
          args
    | "range" -> (
        match List.map int_of pos with
        | [ Some n ] when n > max_iterations ->
            err x "E_ITER_BOUND" (Printf.sprintf "range 0‥%d exceeds 4,096 iterations." n)
        | [ Some a; Some b ] when b - a > max_iterations ->
            err x "E_ITER_BOUND" (Printf.sprintf "range %d‥%d exceeds 4,096 iterations." a b)
        | _ -> ())
    | "linspace" -> (
        match List.map int_of pos with
        | [ _; _; Some n ] when n > max_iterations ->
            err x "E_ITER_BOUND" "linspace exceeds 4,096 values."
        | _ -> ())
    | "ui/split" | "ui/split-at" ->
        (match pos with
        | { aterm = { node = Text axis; _ }; _ } :: _
          when not (List.mem axis (List.assoc "axis" o.choices)) ->
            err x "E_RANGE" "Split axis is horizontal or vertical."
        | _ -> ());
        (match (o.name, pos) with
        | "ui/split-at", _ :: r :: _ -> (
            match num_of r with
            | Some n when n < 0.1 || n > 0.9 -> err x "E_RANGE" "Split ratio is 0.1–0.9."
            | _ -> ())
        | _ -> ());
        (* one side fixed in points, never both *)
        let size key = List.find_opt (fun (a : arg) -> a.key = Some key) args in
        List.iter
          (fun a ->
            match num_of a with
            | Some n when n < 1. -> err a.aform "E_RANGE" "A fixed split size is 1 point or more."
            | _ -> ())
          (List.filter_map size [ "first_size"; "second_size" ]);
        if size "first_size" <> None && size "second_size" <> None then
          err x "E_RANGE" "A split fixes one side: :first_size or :second_size."
    | "ui/graph" -> (
        (* the graph the panel pins: a graph of the workspace, or [def:name] for a function *)
        match pos with
        | { aterm = { node = Text g; _ }; aform; _ } :: _
          when not
                 (Hashtbl.mem gsigs g
                 || String.starts_with ~prefix:"def:" g
                    && Hashtbl.mem sigs (String.sub g 4 (String.length g - 4))) ->
            err aform "E_UNKNOWN_GRAPH" (Printf.sprintf "Unknown graph: %s." g)
        | _ -> ())
    | _ -> ()

  and validate p a =
    validate_parameter ~ty:a.av.ty (fun sev code msg -> add sev a.aform code msg) p a.aterm

  and instantiate_fn ?(coerce_params = true) cx (a : arg) (signature : Ty.fn_signature) =
    match a.aterm.node with
    | List_lit entries ->
        let entries =
          List.map
            (fun (term : term) ->
              let aterm, av = fn_arg cx term.form in
              instantiate_fn ~coerce_params cx { a with aterm; av } signature)
            entries
        in
        let ty = Ty.List (Ty.Fn (Some signature)) in
        {
          a with
          aterm = { a.aterm with ty; node = List_lit (List.map (fun a -> a.aterm) entries) };
          av =
            { (derive ty (List.map (fun a -> a.av) entries)) with len = Some (List.length entries) };
        }
    | _ ->
        if a.av.fn = None then a
        else
          let names = List.mapi (fun i _ -> "__flow_port_" ^ string_of_int i) signature.params in
          let inputs =
            List.map2
              (fun name ty ->
                {
                  key = None;
                  aform = a.aform;
                  aterm = tm a.aform ty (Ref_binding (name, []));
                  av = leaf ty;
                })
              names signature.params
          in
          let closure =
            match a.av.fn with
            | Some (Closure c) -> Some c
            | Some (Def_fn name) ->
                let d = Hashtbl.find sigs name in
                Some
                  {
                    params =
                      List.map (fun (name, ty, _) -> (S.make (S.Sym name), Some ty)) d.sparams;
                    body = d.sbody;
                    ccx =
                      {
                        cx with
                        env = Smap.empty;
                        ctx = d.sctx;
                        in_def = true;
                        stack = d.sname :: cx.stack;
                        path = [ "def:" ^ name ];
                      };
                    cname = name;
                    cid = [ "def:" ^ name ];
                  }
            | _ -> None
          in
          let result, rv =
            match closure with
            | Some c -> call_closure cx a.aform c inputs
            | None -> call_value cx a.aform a.av inputs
          in
          if not (Ty.fits rv.ty signature.result) then
            err a.aform "E_TYPE"
              (Printf.sprintf "Function port returns %s; expected %s." (show rv.ty)
                 (show signature.result));
          let params, zone =
            match closure with
            | Some c when List.length c.params = List.length signature.params ->
                ( List.map2
                    (fun (p, annotation) ty ->
                      ( pat_ir p,
                        if coerce_params then
                          Some (Option.fold ~none:ty ~some:(Ty.coerce ty) annotation)
                        else annotation ))
                    c.params signature.params,
                  c.cid )
            | _ ->
                ( List.map2
                    (fun name ty -> (Name name, if coerce_params then Some ty else None))
                    names signature.params,
                  inline cx "~fn" )
          in
          let ty = Ty.Fn (Some { signature with result = rv.ty }) in
          {
            a with
            aterm =
              { a.aterm with ty; node = Fn { params; body = result; zone; capture = Some a.aterm } };
            av =
              {
                a.av with
                ty;
                live = a.av.live || rv.live;
                state_dep = a.av.state_dep || rv.state_dep;
                approx_sources = Paths.union a.av.approx_sources rv.approx_sources;
              };
          }

  and apply_kind cx x (k : Catalog.kind) (args : arg list) : term * v =
    let position = ref 0 in
    let args =
      List.map
        (fun a ->
          let slot =
            match a.key with
            | Some name -> List.find_opt (fun (s : Catalog.slot) -> s.name = name) k.slots
            | None ->
                let i = !position in
                incr position;
                positional_slot k.slots i
          in
          match Option.bind slot (fun (s : Catalog.slot) -> s.ty) with
          | Some (Ty.Fn (Some signature)) -> instantiate_fn cx a signature
          | _ -> (
              match a.key with
              | Some name -> (
                  match
                    List.find_opt (fun (p : Catalog.parameter) -> p.name = name) k.parameters
                  with
                  | Some { ty = Some (Port_type.Fn signature); _ } -> instantiate_fn cx a signature
                  | _ -> a)
              | None -> a))
        args
    in
    if not (Op.is_display_kind k.qualified) then
      List.iter (fun a -> exact_sink cx a.aform k.qualified a.av) args;
    let rest_index = List.find_index (fun (s : Catalog.slot) -> s.rest) k.slots in
    let short = Catalog.short k.qualified in
    let nslots = List.length k.slots in
    let seen = Hashtbl.create 8 in
    let out = ref [] and npos = ref 0 and writes = ref [] in
    let groups_in = List.fold_left (fun g a -> union g a.av.groups) [] args in
    let slot_arg ?(repeated = false) a (slot : Catalog.slot) =
      let ty = slot_ty ~slot k in
      if Ty.fits a.av.ty ty then ()
      else if repeated && match a.av.ty with Ty.List e -> Ty.fits e ty | _ -> false then begin
        if a.av.live_len then
          err a.aform "E_TIME_COUNT"
            (Printf.sprintf
               "The list passed to %s changes length with t. The network keeps its shape while \
                playing; animate parameters instead, for example scale a piece to 0."
               k.qualified)
      end
      else err a.aform "E_TYPE" (Printf.sprintf "Input %s takes %s" slot.name (show ty))
    in
    List.iter
      (fun a ->
        match a.key with
        | None ->
            let idx = !npos in
            incr npos;
            if Option.fold ~none:false ~some:(fun first -> idx >= first) rest_index then begin
              let slot = List.nth k.slots (Option.get rest_index) in
              slot_arg ~repeated:true a slot;
              out := (slot.name, a.aterm) :: !out
            end
            else if idx >= nslots then
              err a.aform "E_EXTRA_POSITIONAL"
                (Printf.sprintf "%s takes %d geometry input%s; this one is extra" short nslots
                   (plural nslots))
            else begin
              let s = List.nth k.slots idx in
              if Hashtbl.mem seen s.name then
                err a.aform "E_DUPLICATE_PARAM"
                  (Printf.sprintf "Input %s is given twice: by :%s and by position" s.name s.name)
              else begin
                Hashtbl.add seen s.name ();
                slot_arg a s;
                out := (s.name, a.aterm) :: !out
              end
            end
        | Some n ->
            if Hashtbl.mem seen n then
              err a.aform "E_DUPLICATE_PARAM" (Printf.sprintf ":%s is given twice" n)
            else begin
              Hashtbl.add seen n ();
              match
                ( List.find_opt (fun (s : Catalog.slot) -> s.name = n) k.slots,
                  List.find_opt (fun (p : Catalog.parameter) -> p.name = n) k.parameters )
              with
              | Some slot, _ ->
                  slot_arg ~repeated:slot.rest a slot;
                  out := (n, a.aterm) :: !out
              | None, Some p ->
                  (if k.qualified = "sop/material" && n = "material" && a.av.ty = Ty.material then
                     ()
                   else
                     match (p.ty, a.av.ty, a.av.fn) with
                     | Some (Port_type.Fn _), Ty.Fn None, None -> ()
                     | _ -> validate p a);
                  (match a.aterm.node with
                  | Text s when group_reader p && s <> "" && not (List.mem s groups_in) ->
                      add Diagnostic.Warning a.aform "W_UNKNOWN_GROUP"
                        (Printf.sprintf "Group \"%s\" is not made by any node upstream of %s." s
                           short)
                  | Text s when group_writer k p -> writes := s :: !writes
                  | _ -> ());
                  out := (n, a.aterm) :: !out
              | None, None ->
                  let names =
                    List.map (fun (s : Catalog.slot) -> s.name) k.slots
                    @ List.map (fun (p : Catalog.parameter) -> p.name) k.parameters
                  in
                  err a.aform "E_UNKNOWN_PARAM"
                    (Printf.sprintf "%s has no parameter :%s.%s" short n
                       (Catalog.suggestion n names))
            end)
      args;
    List.iter
      (fun (s : Catalog.slot) ->
        if s.required && (not s.rest) && not (Hashtbl.mem seen s.name) then
          if slot_ty ~slot:s k = Ty.geometry then out := (s.name, tm x Ty.geometry Nil) :: !out
          else err x "E_MISSING_INPUT" (Printf.sprintf "%s needs its %s input" short s.name))
      k.slots;
    List.iter
      (fun (p : Catalog.parameter) ->
        if
          p.fields = []
          && (match p.ty with Some (Port_type.Fn _ | Port_type.Image) -> true | _ -> false)
          && not (Hashtbl.mem seen p.name)
        then err x "E_MISSING_INPUT" (Printf.sprintf "%s needs its :%s input" short p.name))
      k.parameters;
    let ty = kind_out k in
    let v =
      {
        (derive ty (List.map (fun a -> a.av) args)) with
        groups = union groups_in !writes;
        approx_sources = Paths.empty;
        packed_producers = Paths.empty;
      }
    in
    (tm x ty (Call { kind = k.qualified; ctx = k.context; args = List.rev !out }), v)

  and def_default (d : signature) pname : (term * v) option =
    match Hashtbl.find_opt def_defaults (d.sname, pname) with
    | Some r -> Some r
    | None -> (
        match List.find_opt (fun (n, _, _) -> n = pname) d.sparams with
        | Some (_, ty, Some df) ->
            let cx =
              {
                env = Smap.empty;
                ctx = d.sctx;
                scope = "ƒ " ^ d.sname;
                path = [ "def:" ^ d.sname; ":" ^ pname ];
                in_def = true;
                stack = [ d.sname ];
                depth = 0;
                zone = 0;
                zbody = false;
              }
            in
            let t, v = infer cx df in
            if not (Ty.fits v.ty ty) then
              err df "E_TYPE"
                (Printf.sprintf "The default of %s input %s is %s; it is declared %s." d.sname pname
                   (show v.ty) (show ty));
            Hashtbl.replace def_defaults (d.sname, pname) (t, v);
            Some (t, v)
        | _ -> None)

  and apply_def cx x (d : signature) (args : arg list) : term * v =
    if d.sctx <> Context.value && d.sctx <> cx.ctx then
      bad x "E_WRONG_CONTEXT"
        (Printf.sprintf "%s belongs to %s; it cannot run in %s." d.sname (context_name d.sctx)
           (context_name cx.ctx))
    else if List.mem d.sname cx.stack then
      bad x "E_RECURSION"
        (Printf.sprintf "Recursive call: %s. Use fold for repetition."
           (String.concat " → " (List.rev (d.sname :: cx.stack))))
    else if cx.depth > max_call_depth then bad x "E_DEPTH" "Call depth exceeds 64."
    else begin
      let pos = List.filter (fun a -> a.key = None) args in
      let np = List.length pos and nparams = List.length d.sparams in
      let ok = ref true in
      if np > nparams then (
        ok := false;
        err x "E_ARITY"
          (Printf.sprintf "%s takes %d input%s; got %d positional." d.sname nparams (plural nparams)
             np));
      List.iter
        (fun a ->
          match a.key with
          | Some k when not (List.exists (fun (n, _, _) -> n = k) d.sparams) ->
              ok := false;
              err a.aform "E_UNKNOWN_PARAM"
                (Printf.sprintf "%s has no input :%s. Inputs: %s." d.sname k
                   (String.concat ", " (List.map (fun (n, _, _) -> n) d.sparams)))
          | _ -> ())
        args;
      if not !ok then (tm x Ty.Any Nil, poison)
      else begin
        let env = ref Smap.empty and terms = ref [] in
        List.iteri
          (fun i (pname, pty, _) ->
            let by_pos = List.nth_opt pos i
            and by_key = List.find_opt (fun a -> a.key = Some pname) args in
            (match (by_pos, by_key) with
            | Some _, Some k ->
                err k.aform "E_DUPLICATE_PARAM"
                  (Printf.sprintf "%s input %s is given twice." d.sname pname)
            | _ -> ());
            let given = match by_pos with Some a -> Some a | None -> by_key in
            let t, v =
              match given with
              | Some a ->
                  ignore (need a (fun () -> Printf.sprintf "%s :%s" d.sname pname) pty);
                  (a.aterm, { a.av with ty = Ty.coerce a.av.ty pty })
              | None -> (
                  match def_default d pname with
                  | Some (t, v) -> (t, v)
                  | None ->
                      err x "E_ARGS" (Printf.sprintf "%s needs :%s (%s)." d.sname pname (show pty));
                      (tm x pty Nil, leaf pty))
            in
            env := Smap.add pname v !env;
            terms := t :: !terms)
          d.sparams;
        let inner =
          {
            env = !env;
            ctx = d.sctx;
            stack = d.sname :: cx.stack;
            scope = "ƒ " ^ d.sname;
            path = [ "def:" ^ d.sname ];
            in_def = true;
            depth = cx.depth + 1;
            zone = 0;
            zbody = false;
          }
        in
        (* A body checked again with equal inputs (no function values) checks the same way: the first
           call's body term is shared, as checked terms already share subterms. *)
        let vs = List.rev_map (fun (pname, _, _) -> Smap.find pname inner.env) d.sparams in
        let key = (d.sname, inner.stack, inner.depth, vs) in
        let memo = List.for_all (fun v -> v.fn = None) vs in
        let bt, bv =
          match if memo then Hashtbl.find_opt body_memo key else None with
          | Some checked -> checked
          | None ->
              let checked = body inner d.sbody in
              if memo then Hashtbl.add body_memo key checked;
              checked
        in
        ( tm x bv.ty (Call_fn { fn = d.sname; args = List.rev !terms; body = Some bt }),
          {
            bv with
            fn = None;
            state_dep = bv.state_dep || List.exists (fun a -> a.av.state_dep) args;
          } )
      end
    end

  and bypass cx (x : S.t) : term * v =
    match List.filter (( <> ) "bypass") x.meta with
    | m :: _ ->
        bad x "E_META" (Printf.sprintf "Unknown metadata ^:%s. The only metadata is ^:bypass." m)
    | [] -> (
        let bare = { x with meta = [] } in
        match x.node with
        | S.List [ { S.node = S.Sym "if"; _ }; _; a; _ ] ->
            let _, checked = plain cx bare in
            let t, v = branch cx "then" a in
            ({ t with ty = checked.ty; form = x }, { v with ty = checked.ty })
        | S.List ({ S.node = S.Sym h; _ } :: args)
          when (not (List.mem h special)) && not (Hashtbl.mem macro_tbl h) -> (
            let rec first i = function
              | [] -> None
              | { S.node = S.Kw _; _ } :: _ :: rest -> first (i + 1) rest
              | _ :: _ -> Some i
            in
            match first 0 args with
            | None ->
                bad x "E_BYPASS"
                  (Printf.sprintf "can't bypass %s: it has no positional input to pass through." h)
            | Some _ -> (
                let t, v = plain cx bare in
                match bypass_input t with
                | Some a ->
                    if (not (Ty.fits a.ty v.ty)) || Ty.has_fn a.ty then
                      err x "E_BYPASS"
                        (Printf.sprintf "can't bypass %s: its input is %s but it returns %s." h
                           (show a.ty) (show v.ty));
                    (tm x v.ty (Bypass t), v)
                | _ -> (tm x v.ty (Bypass t), v)))
        | _ ->
            bad x "E_BYPASS"
              (Printf.sprintf
                 "can't bypass %s: only operator and function calls pass an input through."
                 (match x.node with
                 | S.List ({ S.node = S.Sym h; _ } :: _) -> h
                 | _ -> pattern_key { x with meta = [] })))

  and graph_info x name : v =
    match Hashtbl.find_opt ginfo name with
    | Some v -> v
    | None ->
        if List.mem name !gstack then begin
          err x "E_GRAPH_CYCLE"
            (Printf.sprintf "Graph cycle: %s." (String.concat " → " (List.rev (name :: !gstack))));
          leaf (context_ty (Hashtbl.find gsigs name).sctx)
        end
        else begin
          let sg = Hashtbl.find gsigs name in
          gstack := name :: !gstack;
          let cx0 =
            {
              env = Smap.empty;
              ctx = sg.sctx;
              scope = name;
              path = [ name ];
              in_def = false;
              stack = [];
              depth = 0;
              zone = 0;
              zbody = false;
            }
          in
          let env = ref Smap.empty and inputs = ref [] in
          List.iter
            (fun (n, ty, df) ->
              let cx = { cx0 with env = !env } in
              let dt, dv =
                match df with
                | Some d ->
                    let t, v = infer cx d in
                    if not (Ty.fits v.ty ty) then
                      err d "E_TYPE"
                        (Printf.sprintf "The default of %s input %s is %s; it is declared %s." name
                           n (show v.ty) (show ty));
                    (Some t, v)
                | None -> (None, leaf ty)
              in
              let v =
                {
                  (leaf ~live:dv.live ty) with
                  vary = [];
                  state_dep = dv.state_dep;
                  packed_producers = dv.packed_producers;
                  approx_sources = dv.approx_sources;
                }
              in
              mark [ name; ":" ^ n ] v;
              env := Smap.add n v !env;
              inputs := (n, ty, dt) :: !inputs)
            sg.sparams;
          let bt, bv = body { cx0 with env = !env } sg.sbody in
          let want = context_ty sg.sctx in
          if not (Ty.fits bv.ty want) then
            err sg.sbody "E_TYPE"
              (Printf.sprintf "%s must return %s, but its result is %s." name (show want)
                 (show bv.ty));
          gstack := List.tl !gstack;
          Hashtbl.replace graph_terms name
            { name; context = sg.sctx; inputs = List.rev !inputs; body = bt; form = sg.sform };
          let v =
            {
              (leaf ~live:bv.live want) with
              groups = [];
              approx = bv.approx;
              approx_sources = bv.approx_sources;
              packed_producers = bv.packed_producers;
              state_dep = bv.state_dep;
            }
          in
          Hashtbl.replace ginfo name v;
          v
        end

  let items = ref (from (fun s -> s.s_items) [])
  let checked = table (fun s -> s.s_checked) 8
  let def_order = ref (from (fun s -> s.s_def_order) [])

  let parse_sig head n (f : S.t) : signature option =
    let shape_error () =
      err f "E_SHAPE"
        (Printf.sprintf "Invalid shape for %s: expected (%s %s :context ctx [params] body)." n head
           n);
      None
    in
    match f.node with
    | S.List (_ :: _ :: { S.node = S.Kw "context"; _ } :: { S.node = S.Sym cname; _ } :: rest) -> (
        match context_of_name cname with
        | None ->
            err f "E_CONTEXT_UNKNOWN"
              (Printf.sprintf "Unknown context %s. Known contexts: %s." cname
                 (String.concat ", " (List.map Context.name (Context.all ()))));
            None
        | Some ctx -> (
            let shape =
              match rest with
              | [ b ] when head = "graph" -> Some ([], b)
              | [ { S.node = S.Vec ps; _ }; b ] -> Some (ps, b)
              | _ -> None
            in
            match shape with
            | None ->
                if head = "defn" && List.length rest = 1 then (
                  err f "E_SHAPE" (Printf.sprintf "defn %s needs a parameter vector." n);
                  None)
                else shape_error ()
            | Some (ps, b) ->
                let used = Hashtbl.create 4 in
                let sparams =
                  List.filter_map
                    (fun (p : S.t) ->
                      match p.node with
                      | S.List ({ S.node = S.Sym pn; _ } :: { S.node = S.Sym ":"; _ } :: tyf :: dflt)
                        when List.length dflt <= 1
                             && Macro.valid_name pn
                             && (not (reserved pn))
                             && not (Hashtbl.mem used pn) -> (
                          match Ty.of_syntax tyf with
                          | None ->
                              err p "E_PARAM"
                                (Printf.sprintf
                                   "Invalid parameter in %s. Each is (name : type default?)." n);
                              None
                          | Some ty ->
                              Hashtbl.add used pn ();
                              let df = match dflt with [ d ] -> Some d | _ -> None in
                              if Ty.has_fn ty && (ty <> Ty.Fn None || head = "graph") then begin
                                err p "E_FN_ESCAPES"
                                  (Printf.sprintf
                                     "%s input %s: a function value cannot be stored or returned \
                                      (E_FN_ESCAPES); %s."
                                     n pn
                                     (if head = "graph" then "graph inputs are data"
                                      else "only a defn input may have type fn"));
                                None
                              end
                              else if ty = Ty.Fn None && df <> None then begin
                                err p "E_PARAM"
                                  (Printf.sprintf
                                     "%s input %s: a fn input has no default; callers pass a \
                                      function."
                                     n pn);
                                None
                              end
                              else if head = "graph" && df = None then begin
                                err p "E_INPUT_DEFAULT"
                                  (Printf.sprintf
                                     "Graph input %s of %s needs a default, so the graph runs on \
                                      its own."
                                     pn n);
                                None
                              end
                              else Some (pn, ty, df))
                      | _ ->
                          err p "E_PARAM"
                            (Printf.sprintf
                               "Invalid parameter in %s. Each is (name : type default?)." n);
                          None)
                    ps
                in
                Some { sname = n; sctx = ctx; sparams; sbody = b; sform = f }))
    | _ -> shape_error ()

  (* metadata is for expressions ([^:bypass]); the build's [^:allow-warnings] sits on the workspace *)
  let unknown_meta (f : S.t) allowed =
    List.iter
      (fun m ->
        if not (List.mem m allowed) then
          add Diagnostic.Warning f "W_UNKNOWN_META"
            (Printf.sprintf "Unknown metadata ^:%s here; it is ignored." m))
      f.meta

  (* Declares and checks [children] after those already checked (a library's, from [start]). *)
  let phase children =
    List.iter (fun f -> unknown_meta f []) children;
    let fresh = ref [] in
    List.iter
      (fun (f : S.t) ->
        match f.node with
        | S.List
            ({ S.node = S.Sym (("graph" | "defn" | "defnode" | "defmacro") as head); _ }
            :: { S.node = S.Sym n; _ }
            :: _) ->
            if
              (if head = "defnode" then not (Defnode.valid_name n) else not (Macro.valid_name n))
              || reserved n || Hashtbl.mem names n
              || head = "defnode"
                 && List.exists (fun (k : Catalog.kind) -> k.qualified = n) catalog.kinds
              || match find_op n Context.value with Some o -> o.name = n | None -> false
            then err f "E_NAME" (Printf.sprintf "Invalid, reserved or duplicate name: %s." n)
            else (
              Hashtbl.add names n ();
              fresh := (head, n, f) :: !fresh)
        | _ -> err f "E_FORM" "Workspace children are graph, defn, defnode and defmacro forms.")
      children;
    items := !fresh @ !items;
    let items = List.rev !fresh in
    List.iter
      (fun (head, n, f) ->
        if head = "defnode" then
          match Defnode.parse f with
          | Ok d -> Hashtbl.replace node_sources n d
          | Error d -> diags := d :: !diags)
      items;
    Hashtbl.clear kind_memo;
    List.iter
      (fun (head, n, (f : S.t)) ->
        if head = "defmacro" then begin
          diags := List.rev_append (Macro.check ~known f) !diags;
          match Macro.params f with
          | Ok _ ->
              Hashtbl.replace macro_tbl n f;
              macro_forms := f :: !macro_forms
          | Error _ -> ()
        end)
      items;
    let graph_order = ref [] and defs = ref [] in
    List.iter
      (fun (head, n, f) ->
        if head <> "defmacro" && head <> "defnode" then
          match parse_sig head n f with
          | Some sg ->
              if head = "graph" then (
                Hashtbl.replace gsigs n sg;
                graph_order := n :: !graph_order)
              else (
                Hashtbl.replace sigs n sg;
                defs := n :: !defs)
          | None -> ())
      items;
    def_order := !defs @ !def_order;
    let graph_order = List.rev !graph_order and defs = List.rev !defs in
    (* With [call_sites] a body is typed where it is called (each call site checks it again with its
       arguments); its own term is a placeholder no call reaches, every call carrying its checked body. *)
    let def ~own n =
      let d = Hashtbl.find sigs n in
      let env =
        List.fold_left (fun env (pn, ty, _) -> Smap.add pn (leaf ty) env) Smap.empty d.sparams
      in
      let cx =
        {
          env;
          ctx = d.sctx;
          scope = "ƒ " ^ n;
          path = [ "def:" ^ n ];
          in_def = true;
          stack = [ n ];
          depth = 0;
          zone = 0;
          zbody = false;
        }
      in
      let inputs =
        List.map (fun (pn, ty, _) -> (pn, ty, Option.map fst (def_default d pn))) d.sparams
      in
      let bt =
        if own then (
          Hashtbl.replace checked n ();
          fst (body cx d.sbody))
        else tm d.sbody Ty.Any Nil
      in
      Hashtbl.replace def_terms n { name = n; context = d.sctx; inputs; body = bt; form = d.sform }
    in
    (check_def := fun n -> if Hashtbl.mem sigs n && not (Hashtbl.mem checked n) then def ~own:true n);
    List.iter (fun n -> if not (Hashtbl.mem checked n) then def ~own:(not call_sites) n) defs;
    List.iter
      (fun (head, n, _) ->
        if head = "defnode" then
          Option.iter
            (fun (d : Defnode.t) ->
              let env =
                List.fold_left
                  (fun env (pn, ty, _) -> Smap.add pn (leaf ty) env)
                  Smap.empty d.inputs
              in
              let cx =
                {
                  env;
                  ctx = Context.sop;
                  scope = n;
                  path = [ "node:" ^ n ];
                  in_def = true;
                  stack = [ n ];
                  depth = 0;
                  zone = 0;
                  zbody = false;
                }
              in
              let inputs =
                List.map
                  (fun (pn, ty, df) ->
                    let dt =
                      Option.map
                        (fun df ->
                          let t, v =
                            infer { cx with env = Smap.empty; path = cx.path @ [ ":" ^ pn ] } df
                          in
                          if not (Ty.fits v.ty ty) then
                            err df "E_TYPE" ("Invalid default for " ^ pn ^ ".");
                          t)
                        df
                    in
                    (pn, ty, dt))
                  d.inputs
              in
              let bt, bv = body cx d.body in
              if not (Ty.fits bv.ty Ty.geometry) then
                err d.body "E_TYPE" (n ^ " must return geometry.");
              let validations =
                List.map
                  (fun (v : Defnode.validation) ->
                    let t, value = infer cx v.predicate in
                    if
                      List.exists
                        (fun name -> List.mem name (called_names ~include_ops:true t))
                        body_context_ops
                    then
                      err v.predicate "E_NODE_CONTEXT"
                        "A validation checks parameters; context seed is available inside the body.";
                    if value.ty <> Ty.Bool then
                      err v.predicate "E_TYPE" "A validation predicate returns bool.";
                    (t, v.code, v.message))
                  d.validations
              in
              let normalizers =
                List.map
                  (fun (name, form) ->
                    let ncx = { cx with ctx = Context.value; env = Smap.empty } in
                    let term, value = fn_arg ncx form in
                    let argument =
                      instantiate_fn ~coerce_params:false ncx
                        { key = None; aform = form; aterm = term; av = value }
                        { params = [ Ty.Text ]; result = Ty.Text }
                    in
                    let term, value = (argument.aterm, argument.av) in
                    if value.ty <> Ty.Fn (Some { params = [ Ty.Text ]; result = Ty.Text }) then
                      err form "E_TYPE" "An encoded normalizer takes one text and returns text.";
                    pure_boundary ncx ~mapper:true "E_PARAM_NORMALIZE"
                      "An encoded normalizer is pure: no frame, state, context or cooked values."
                      term;
                    (name, term))
                  d.normalizers
              in
              node_terms :=
                {
                  kind = d.kind;
                  slots = d.slots;
                  keyword_inputs = d.keyword_inputs;
                  fields = d.fields;
                  label = d.label;
                  category = d.category;
                  description = d.description;
                  operation = d.operation;
                  version = d.version;
                  body = { name = n; context = Context.sop; inputs; body = bt; form = d.form };
                  validations;
                  normalizers;
                }
                :: !node_terms)
            (Hashtbl.find_opt node_sources n))
      items;
    graph_order

  let snapshot ~count ~budget =
    {
      s_diags = !diags;
      s_live = !live;
      s_invariant = !invariant;
      s_roots = roots;
      s_links = links;
      s_root_forms = root_forms;
      s_steps = !steps;
      s_zones = !zones;
      s_body_memo = body_memo;
      s_kind_fns = !kind_fns;
      s_inline_seen = inline_seen;
      s_input_paths = input_paths;
      s_names = names;
      s_sigs = sigs;
      s_macro_tbl = macro_tbl;
      s_macro_forms = !macro_forms;
      s_node_sources = node_sources;
      s_node_terms = !node_terms;
      s_def_terms = def_terms;
      s_def_defaults = def_defaults;
      s_items = !items;
      s_checked = checked;
      s_def_order = !def_order;
      s_count = from (fun s -> s.s_count) 0 + count;
      s_budget = budget;
    }

  let driver (ws : S.t) wname children =
    ignore wname;
    unknown_meta ws [ "allow-warnings" ];
    let graph_order = phase children in
    let items = List.rev !items in
    let visited = Hashtbl.create (List.length !node_terms) in
    let rec acyclic stack (d : node_definition) =
      if List.mem d.kind.qualified stack then
        err d.body.form "E_RECURSION"
          ("Recursive node call: " ^ String.concat " → " (List.rev (d.kind.qualified :: stack)))
      else if not (Hashtbl.mem visited d.kind.qualified) then begin
        List.iter
          (fun name ->
            Option.iter
              (acyclic (d.kind.qualified :: stack))
              (List.find_opt (fun (n : node_definition) -> n.kind.qualified = name) !node_terms))
          (kind_calls d.body.body);
        Hashtbl.add visited d.kind.qualified ()
      end
    in
    List.iter (acyclic []) !node_terms;
    List.iter (fun n -> ignore (graph_info (Hashtbl.find gsigs n).sform n)) graph_order;
    if (not library) && graph_order = [] && not (List.exists (fun (h, _, _) -> h = "graph") items)
    then err ws "E_NO_GRAPH" "A workspace needs at least one graph.";
    (graph_order, List.rev !def_order)
end

type prelude = snapshot

let prelude ?(ops = []) ?(library = false) ?(call_sites = false) catalog children =
  let module K = Checker (struct
    let ops = ops
    let library = library
    let call_sites = call_sites
    let catalog = catalog
    let children = List.length children
    let start = None
  end) in
  let budget = match K.phase children with _ -> false | exception Budget -> true in
  K.snapshot ~count:(List.length children) ~budget

let check ?(ops = []) ?(library = false) ?(call_sites = false) ?prelude catalog forms =
  Phase_timer.measure "check" (fun () ->
      match Op.validate ops with
      | Some error -> (None, [ error ])
      | None -> (
          let start = prelude in
          let module K = Checker (struct
            let ops = ops
            let library = library
            let call_sites = call_sites
            let catalog = catalog

            let children =
              match forms with
              | [ { S.node = S.List (_ :: _ :: children); _ } ] -> List.length children
              | _ -> 0

            let start = start
          end) in
          let open K in
          let outcome =
            match forms with
            | [
             ({
                S.node =
                  S.List
                    ({ S.node = S.Sym "workspace"; _ } :: { S.node = S.Sym wname; _ } :: children);
                _;
              } as ws);
            ]
              when Macro.valid_name wname -> (
                try
                  if Option.fold ~none:false ~some:(fun s -> s.s_budget) start then raise Budget;
                  let go, dos = driver ws wname children in
                  Some (wname, ws, go, dos)
                with Budget ->
                  err ws "E_BUDGET"
                    "The workspace is too large to check: call-site typing exceeded its budget. \
                     Reduce nested function calls.";
                  None)
            | [] ->
                diags := [ Diagnostic.error ~code:"E_NO_WORKSPACE" "Expected (workspace name …)." ];
                None
            | [ f ] ->
                err f "E_WORKSPACE" "Expected (workspace name …).";
                None
            | _ :: f :: _ ->
                err f "E_ONE_WORKSPACE" "A file holds one workspace form.";
                None
          in
          let seen = Hashtbl.create 16 in
          let diagnostics =
            List.filter
              (fun (d : Diagnostic.t) ->
                let key = (d.code, d.span, d.message) in
                if Hashtbl.mem seen key then false
                else (
                  Hashtbl.add seen key ();
                  true))
              (List.rev !diags)
          in
          let has_error =
            List.exists
              (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity)
              diagnostics
          in
          match outcome with
          | Some (wname, _, go, dos) when not has_error ->
              let get tbl = List.filter_map (Hashtbl.find_opt tbl) in
              let packed =
                Hashtbl.fold
                  (fun path producers entries ->
                    let classification =
                      match Hashtbl.find_opt roots path with
                      | Some (Refused _ as refusal) -> refusal
                      | Some (Candidate own) -> Candidate (Paths.union own producers)
                      | None -> Candidate producers
                    in
                    (path, classification) :: entries)
                  links []
                |> List.sort compare
              in
              let packed_roots =
                Hashtbl.fold (fun _ forms roots -> forms @ roots) root_forms []
                |> List.sort (fun (a, p) (b, q) ->
                    let n = compare p q in
                    if n = 0 then compare a.S.id b.S.id else n)
              in
              let approx_reasons =
                List.map
                  (fun (path, classification) ->
                    ( path,
                      match classification with
                      | Refused reasons -> reasons
                      | Candidate _ ->
                          [
                            Diagnostic.error ~code:"E_PACKED_PENDING"
                              "Actual producer captures have not been qualified.";
                          ] ))
                  packed
              in
              ( Some
                  {
                    name = wname;
                    graphs = get graph_terms go;
                    defs = get def_terms dos;
                    nodes = List.rev !node_terms;
                    macros = List.rev !macro_forms;
                    source = forms;
                    live = !live;
                    invariant = !invariant;
                    approx = Paths.empty;
                    packed;
                    packed_roots;
                    approx_reasons;
                    kind_fns = !kind_fns;
                    ops;
                  },
                diagnostics )
          | _ -> (None, diagnostics)))

let name_taken s = reserved s || Symbol.reserved s || Op.find s Context.value <> None || type_name s

type op_signature = Op.signature = {
  pos : (string * Ty.t) list;
  opt : (string * Ty.t) list;
  rest : (string * Ty.t) option;
  kw : (string * Ty.t) list;
}

let value_ops = List.map (fun (o : Op.t) -> o.name) (Op.of_context Context.value)

let op_choices ?(ops = []) name argument =
  match Op.find ~extra:ops name Context.value with
  | Some o -> Option.value (List.assoc_opt argument o.choices) ~default:[]
  | None -> []

let op_signature ?(ops = []) ctx name =
  Option.map (fun (o : Op.t) -> o.signature) (Op.find ~extra:ops name ctx)
