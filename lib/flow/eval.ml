module W = Workspace
module S = Syntax
module Smap = Map.Make (String)

module State_map = Map.Make (struct
  type t = int * W.path * int list

  let compare = compare
end)

module Names = Set.Make (String)

let rec pattern_names names = function
  | Workspace.Name name -> Names.add name names
  | Seq patterns -> List.fold_left pattern_names names patterns
  | Keys names' -> List.fold_left (fun names name -> Names.add name names) names names'

module Free_cache = Ephemeron.K1.Make (struct
  type t = Workspace.term

  let equal a b = a == b
  let hash (term : t) = Hashtbl.hash term.form.id
end)

let free_caches = Domain.DLS.new_key (fun () -> Free_cache.create 256)
let free_walks = Atomic.make 0

let free_names term =
  let free_cache = Domain.DLS.get free_caches in
  match Free_cache.find_opt free_cache term with
  | Some names -> names
  | None ->
      ignore (Atomic.fetch_and_add free_walks 1);
      let rec walk bound names (term : Workspace.term) =
        let all bound names terms = List.fold_left (walk bound) names terms in
        let fields bound names fields =
          List.fold_left (fun names (_, term) -> walk bound names term) names fields
        in
        let reference name = if Names.mem name bound then names else Names.add name names in
        let bindings bound names bindings =
          List.fold_left
            (fun (bound, names) (pattern, term) ->
              (pattern_names bound pattern, walk bound names term))
            (bound, names) bindings
        in
        match term.node with
        | Ref_binding (name, _) | Fn_ref name -> reference name
        | Call_fn { fn; args; _ } -> all bound (reference fn) args
        | Call { args; _ } | Op { args; _ } | Record args -> fields bound names args
        | Graph_ref { inputs; _ } -> fields bound names inputs
        | Vec terms | List_lit terms | Str terms | List_op (_, terms) | Hof (_, terms) ->
            all bound names terms
        | Let (steps, body) ->
            let bound, names = bindings bound names steps in
            walk bound names body
        | Loop { accs; clauses; body; _ } ->
            let bound, names = bindings bound names accs in
            let bound, names = bindings bound names clauses in
            walk bound names body
        | State { binder; init; step; _ } ->
            walk (pattern_names bound binder) (walk bound names init) step
        | Fn { capture = Some original; _ } -> walk bound names original
        | Fn { params; body; _ } ->
            walk
              (List.fold_left (fun bound (pattern, _) -> pattern_names bound pattern) bound params)
              names body
        | With_error (a, b) -> all bound names [ a; b ]
        | Error_rows { rows = a; mapper = b; _ } -> all bound names [ a; b ]
        | If (condition, yes, no) -> all bound names [ condition; yes; no ]
        | Cond (arms, default) ->
            List.fold_left
              (fun names (test, body) -> walk bound (walk bound names test) body)
              (walk bound names default) arms
        | Case (subject, arms, default) -> fields bound (all bound names [ subject; default ]) arms
        | Assoc (record, updates) -> fields bound (walk bound names record) updates
        | Get (body, _) | Expanded { body; _ } -> walk bound names body
        | Bypass call -> Option.fold ~none:names ~some:(walk bound names) (W.bypass_input call)
        | Lit _ | Text _ | Nil | Time -> names
      in
      let names = walk Names.empty Names.empty term in
      (* ponytail: bound weak memo; workloads above 16k distinct live terms recompute.
         An owner-scoped checked-workspace cache removes that ceiling if measured. *)
      if Free_cache.length free_cache >= 16_384 then Free_cache.clear free_cache;
      Free_cache.replace free_cache term names;
      names

let capture term env =
  Names.fold
    (fun name captured ->
      match Smap.find_opt name env with
      | None -> captured
      | Some value -> Smap.add name value captured)
    (free_names term) Smap.empty

let max_steps = 600_000
let max_iterations = W.max_iterations
let max_concat = 4_096
let max_depth = 64
let packed_block_size = 1024
let empty_live = Input.at_time 0.

exception Needs_t

open Value

let hash = Value.hash
let element_key zone = "$elem:" ^ String.concat "/" zone

type ('f, 'r) payload = ('f, 'r) Value.t =
  | Int of int
  | Float of float
  | Bool of bool
  | Text of string
  | Vec2 of float * float
  | Vec3 of float * float * float
  | Vec4 of float * float * float * float
  | List of ('f, 'r) payload array
  | Int_array of int array
  | Text_array of string array
  | Float_array of float array
  | Vec2_array of float array
  | Vec3_array of float array
  | Vec4_array of float array
  | Record of (string * ('f, 'r) payload) list
  | Deferred of Ty.t * int
  | No_geo
  | Struct of string * Ty.t * (string * ('f, 'r) payload) list
  | Source of string * Ty.t * (string * ('f, 'r) payload) list
  | Fn of 'f
  | Residual of 'r

type value = (fn, residual) payload

and fn =
  | Closure of {
      params : (W.pattern * Ty.t option) list;
      body : W.term;
      env : value Smap.t;
      zone : W.path;
      calls : int ref;
      fid : int;
      at : ctx;
    }
  | Named of { name : string; ncalls : int ref; fid : int }

(* Captures contain the term's lexically free names, including nested bodies. *)
and residual = {
  rid : int;
  rterm : W.term;
  renv : value Smap.t;
  rc : ctx;
  previous : bool;
  mutable live : st option; (* the evaluation that last offered this residual to the executor *)
  mutable dep_frame : int;
  mutable dep_state : int;
      (* memo of frame_dependent / state_dependent for this immutable residual: 0 unknown, 1 no, 2 yes *)
}

and state = {
  mutable source : string;
  mutable frame : int option;
  mutable before : value State_map.t;
  mutable next : value State_map.t;
  mutable host : state option;
  mutable epoch : int;
}

and ctx = {
  st : st;
  inst : int;
  prefix : W.path;
  base : W.path;
  route : string list;
  iter : int list;
  depth : int;
  rec_ : bool;
  data : bool;
  geometry : bool;
  host : bool;
  kernel_requests : bool;
  card : string option;
      (* the cook label of the inlined card whose body this is: its context operators read it *)
}

and st = {
  time : Input.t option; (* [None] while evaluating statically *)
  resolve : (value -> (value, Diagnostic.t) result) option;
  source : string Lazy.t;
  state : state option;
  mutable states : value list;
  mutable nfns : int;
  fn_calls : (int, int) Hashtbl.t;
  mutable steps : int;
  budget : int;
  mutable nodes : node list;
  mutable authored : int list;
  mutable nnodes : int;
  mutable cells : cell list;
  cache : (string, cell) Hashtbl.t;
  memo : (int, value) Hashtbl.t;
  mutable rids : int;
  record : bool;
  elems : value Smap.t; (* live evaluation: the element bound in each geometry zone *)
  recs : (W.path, int * (int list * value) list) Hashtbl.t;
  graphs : (string, W.graph) Hashtbl.t;
  defs : (string, W.graph) Hashtbl.t;
  kind_fns : (string * (string * Context.t * Catalog.slot list)) list;
  node_defs :
    ( string,
      (W.node_definition * string Lazy.t * ((string * value) list -> (string * value) list option))
      Lazy.t )
    Hashtbl.t;
      (* Lisp nodes whose bodies evaluate inline in the calling program, with the host's key of
         their declaration and canonicalization of their call parameters *)
  ops : Op.t list;
  find_op : string -> Context.t -> Op.t option;
      (* [Op.finder ops]: [ops] is a list, the whole SOP catalog in the editor *)
  mutable terms : ((int, W.term) Hashtbl.t * (string, unit) Hashtbl.t) option;
      (* helper and inlined node terms by form id, for a replayed body (built on its first
         replay), and the node kinds indexed *)
  mutable inlined : string list; (* the node kinds inlined so far, newest first *)
  mutable watching : (string, unit) Hashtbl.t list; (* the helpers each body being kept calls *)
}

and node = {
  id : int;
  inst : int;
  site : W.path;
  iter : int list;
  kind : string;
  ty : Ty.t;
  args : (string * value) list;
}

and cell = {
  cinst : int;
  cgraph : string;
  cdefault : bool;
  mutable cinputs : (string * value) list;
  mutable cresult : value;
}

(* An inline body's evaluation, kept for a later evaluation of the same card (declaration,
   canonical parameters, input shapes): its nodes in creation order and value, the kernels it
   offered, the id ranges it allocated and the site and iteration it ran at. *)
type inline_entry = {
  e_nodes : node list;
  e_authored : int list;
  e_value : value;
  e_observed : residual list;
  e_lo : int;
  e_count : int;
  e_inputs : int list;
  e_rid0 : int;
  e_nrids : int;
  e_fid0 : int;
  e_nfids : int;
  e_site : W.path;
  e_iter : int list;
  e_kinds : string list; (* the node kinds the body inlined, itself included *)
}

(* ponytail: a whole-table clear at 256 entries; an entry retains its evaluation's state *)
let inline_memo : (string, inline_entry) Hashtbl.t = Hashtbl.create 64
let inline_lock = Mutex.create ()

type live = Input.t
type instance = { graph : string; default : bool; inputs : (string * value) list; result : value }
type plan = { instances : instance array; nodes : node array }

type t = {
  plan : plan;
  authored : int array;
  results : (string * value) list;
  states : value list;
  records : (W.path * (int list * value) list) list;
}

let bulk_ids = Atomic.make (-1)
let state_epochs = Atomic.make 0
let fresh_state_epoch () = Atomic.fetch_and_add state_epochs 1

module Residual_effect = struct
  type _ Effect.t +=
    | Observe_kernel : residual -> unit Effect.t
    | Kernel_value : residual -> value option Effect.t
    | Bulk_static : residual -> value option Effect.t
    | Bulk_approx : residual -> value option Effect.t

  let handle : type result.
      ?observe:(residual -> unit) ->
      ?kernel:(residual -> value option) ->
      ?bulk:(residual -> value option) ->
      ?bulk_approx:(residual -> value option) ->
      (unit -> result) ->
      result =
   fun ?(observe = fun _ -> ()) ?(kernel = fun _ -> None) ?(bulk = fun _ -> None)
       ?(bulk_approx = fun _ -> None) f ->
    let resume callback continuation =
      match callback () with
      | value -> Effect.Deep.continue continuation value
      | exception exn ->
          Effect.Deep.discontinue_with_backtrace continuation exn (Printexc.get_raw_backtrace ())
    in
    Effect.Deep.match_with f ()
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Observe_kernel residual ->
                Some
                  (fun (continuation : (a, result) Effect.Deep.continuation) ->
                    resume (fun () -> observe residual) continuation)
            | Kernel_value residual ->
                Some
                  (fun (continuation : (a, result) Effect.Deep.continuation) ->
                    resume (fun () -> kernel residual) continuation)
            | Bulk_static residual ->
                Some
                  (fun (continuation : (a, result) Effect.Deep.continuation) ->
                    resume (fun () -> bulk residual) continuation)
            | Bulk_approx residual ->
                Some
                  (fun (continuation : (a, result) Effect.Deep.continuation) ->
                    resume (fun () -> bulk_approx residual) continuation)
            | _ -> None);
      }

  let handle_kernel : type result. ?kernel:(residual -> value option) -> (unit -> result) -> result
      =
   fun ?(kernel = fun _ -> None) f ->
    let resume callback continuation =
      match callback () with
      | value -> Effect.Deep.continue continuation value
      | exception exn ->
          Effect.Deep.discontinue_with_backtrace continuation exn (Printexc.get_raw_backtrace ())
    in
    Effect.Deep.match_with f ()
      {
        retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Kernel_value residual ->
                Some
                  (fun (continuation : (a, result) Effect.Deep.continuation) ->
                    resume (fun () -> kernel residual) continuation)
            | _ -> None);
      }
end

let rec copy_state s =
  {
    source = s.source;
    frame = s.frame;
    before = s.before;
    next = s.next;
    host = Option.map copy_state s.host;
    epoch = s.epoch;
  }

let create_state () =
  {
    source = "";
    frame = None;
    before = State_map.empty;
    next = State_map.empty;
    host = None;
    epoch = fresh_state_epoch ();
  }

let reset_frame s =
  s.source <- "";
  s.frame <- None;
  s.before <- State_map.empty;
  s.next <- State_map.empty;
  s.epoch <- fresh_state_epoch ()

let reset_state ?(host_state = true) s =
  reset_frame s;
  if host_state then s.host <- None

let fork_state s = copy_state s

let restore_state s saved =
  s.source <- saved.source;
  s.frame <- saved.frame;
  s.before <- saved.before;
  s.next <- saved.next;
  s.host <- Option.map copy_state saved.host;
  s.epoch <- saved.epoch

let state_stamp state =
  let rec append state =
    string_of_int state.epoch ^ match state.host with None -> "" | Some host -> ":" ^ append host
  in
  append state

let transaction s f =
  let saved = fork_state s in
  let result = f () in
  (match result with Ok _ -> () | Error _ -> restore_state s saved);
  result

let begin_frame s source frame =
  if source <> s.source || Option.fold ~none:false ~some:(fun old -> frame < old) s.frame then
    reset_frame s;
  s.source <- source;
  if s.frame <> Some frame then begin
    s.before <- State_map.union (fun _ _ newer -> Some newer) s.before s.next;
    s.next <- State_map.empty;
    s.frame <- Some frame
  end

let state_key c zone = (c.inst, c.prefix @ zone, c.iter)

let state_plane c =
  let state = Option.get c.st.state and live = Option.get c.st.time in
  if not c.host then (state, live.frame)
  else
    let plane =
      match state.host with
      | Some plane -> plane
      | None ->
          let plane = create_state () in
          state.host <- Some plane;
          plane
    in
    (plane, live.tick)

let fn_id st =
  let id = st.nfns in
  st.nfns <- id + 1;
  id

let find_op st name = st.find_op name Context.value

let fn_call c fid count =
  match c.st.time with
  | None ->
      let k = !count in
      incr count;
      k
  | Some _ ->
      let k = Option.value ~default:0 (Hashtbl.find_opt c.st.fn_calls fid) in
      Hashtbl.replace c.st.fn_calls fid (k + 1);
      k

let key_of v = Value.key_of ~residual:(fun r -> r.rid) v

let join_values xs =
  let t = elem_ty xs in
  Array.map (coerce_to t) xs

let rec pat_key = function
  | W.Name n -> n
  | W.Seq ps -> "[" ^ String.concat " " (List.map pat_key ps) ^ "]"
  | W.Keys ks -> "{:keys [" ^ String.concat " " ks ^ "]}"

let path_text p = String.concat "/" p

(* ---- str formatting (register C2) ---- *)

(* ---- context helpers ---- *)

let site c = c.prefix @ c.base @ List.rev c.route
let sub c seg = { c with route = seg :: c.route }

let span_of (x : W.term) =
  let s = x.form.span in
  if s.start = 0 && s.finish = 0 then None else Some s

let note c path v =
  let st = c.st in
  if st.record && c.rec_ then begin
    let n, l = match Hashtbl.find_opt st.recs path with Some p -> p | None -> (0, []) in
    if n < max_iterations then Hashtbl.replace st.recs path (n + 1, (c.iter, v) :: l)
  end

let mk_node c ?(authored = 0) ?(ty = Ty.geometry) kind args =
  let st = c.st in
  if c.data then fail "E_ARRAY_TYPE" "Packed-array functions produce data, not graph nodes.";
  if st.time <> None then
    failf "E_LIVE_GEOMETRY"
      "%s makes geometry; geometry cannot be created while evaluating a live value." kind;
  (* A geometry computed from cooked data (a fold over sop/attr_names) is only a value at cook time;
     a node reading it is deferred with it, up to the graph root, whose structural node cooks the lot. *)
  let rec cooked_geometry = function
    | Residual r -> r.rterm.W.ty = Ty.geometry
    | Record fs | Struct (_, _, fs) -> List.exists (fun (_, v) -> cooked_geometry v) fs
    | _ -> false
  in
  if st.time = None && List.exists (fun (_, v) -> cooked_geometry v) args then raise Needs_t;
  let n = { id = st.nnodes; inst = c.inst; site = site c; iter = c.iter; kind; ty; args } in
  st.nodes <- n :: st.nodes;
  st.authored <- authored :: st.authored;
  st.nnodes <- st.nnodes + 1;
  Deferred (ty, n.id)

let catalog_call c ?(authored = 0) kind ctx args =
  if (not (Context.supports_values ctx)) || ctx = Context.material then
    Struct (kind, Context.result ctx, args)
  else mk_node c ~authored ~ty:(Context.result ctx) kind args

(* ---- the evaluator ---- *)

let lookup_field name v f =
  match v with
  | Vec2 (x, _) when f = "x" -> Float x
  | Vec2 (_, y) when f = "y" -> Float y
  | Vec3 (x, _, _) when f = "x" -> Float x
  | Vec3 (_, y, _) when f = "y" -> Float y
  | Vec3 (_, _, z) when f = "z" -> Float z
  | Vec4 (x, _, _, _) when f = "x" -> Float x
  | Vec4 (_, y, _, _) when f = "y" -> Float y
  | Vec4 (_, _, z, _) when f = "z" -> Float z
  | Vec4 (_, _, _, w) when f = "w" -> Float w
  | Record fs -> (
      match List.assoc_opt f fs with
      | Some v -> v
      | None -> failf "E_FIELD" "%s has no field %s." name f)
  | _ -> failf "E_FIELD" "%s has no output %s. A vec3 has .x .y .z; a record has its fields." name f

let case_literal (x : S.t) =
  match x.node with
  | S.Num s -> (
      match if String.contains s '.' then None else int_of_string_opt s with
      | Some n -> Int n
      | None -> Float (float_of_string s))
  | S.Str s -> Text s
  | S.Sym s -> Bool (s = "true")
  | _ -> fail "E_CASE" "case matches literal numbers, text or booleans."

let case_matches sv lv =
  let tb = function Bool b -> b | v -> num v <> 0. in
  match (sv, lv) with
  | Bool _, _ | _, Bool _ -> tb sv = tb lv
  | Text a, Text b -> a = b
  | (Int _ | Float _), (Int _ | Float _) -> num sv = num lv
  | _ -> false

let add_values a b = Value.arith "sum" ( +. ) a b

(* Diagnostic scopes execute even when a let binding's result is unused.
   Defer the containing let, so cooking a static geometry result cannot prune
   its validation. Functions themselves are values; inspect bodies at calls. *)
let rec diagnostic_effect st env (term : W.term) =
  let boundary = diagnostic_effect st env in
  let any = List.exists boundary and fields xs = List.exists (fun (_, t) -> boundary t) xs in
  let named name =
    match Smap.find_opt name env with
    | Some (Fn (Closure cl)) -> diagnostic_effect cl.at.st cl.env cl.body
    | _ ->
        Option.fold ~none:false
          ~some:(fun (g : W.graph) -> boundary g.body)
          (Hashtbl.find_opt st.defs name)
  in
  match term.node with
  | W.With_error _ | W.Error_rows _ -> true
  | W.Call_fn { fn; args; body } -> any args || Option.fold ~none:(named fn) ~some:boundary body
  | W.Call { args; _ } | W.Op { args; _ } | W.Record args -> fields args
  | W.Graph_ref { inputs; _ } -> fields inputs
  | W.Vec ts | W.List_lit ts | W.Str ts | W.List_op (_, ts) | W.Hof (_, ts) -> any ts
  | W.Let (bindings, body) -> List.exists (fun (_, t) -> boundary t) bindings || boundary body
  | W.Loop { accs; clauses; body; _ } ->
      List.exists (fun (_, t) -> boundary t) (accs @ clauses) || boundary body
  | W.State { init; step; _ } -> any [ init; step ]
  | W.If (a, b, c) -> any [ a; b; c ]
  | W.Cond (arms, default) -> boundary default || List.exists (fun (a, b) -> any [ a; b ]) arms
  | W.Case (value, arms, default) -> any [ value; default ] || fields arms
  | W.Get (value, _) | W.Expanded { body = value; _ } -> boundary value
  | W.Bypass call -> Option.fold ~none:false ~some:boundary (W.bypass_input call)
  | W.Assoc (value, updates) -> boundary value || fields updates
  | W.Lit _ | W.Text _ | W.Nil | W.Time | W.Ref_binding _ | W.Fn_ref _ | W.Fn _ -> false

let resolve_source resolve source =
  match resolve source with
  | Error (d : Diagnostic.t) -> raise_diagnostic d
  | Ok value ->
      (match value with
      | Source _ | Struct (_, Ty.Array _, _) ->
          fail "E_DATA_SOURCE" "The host returned an unresolved data source."
      | _ -> ());
      if Value.ty_of source <> Ty.Any && Value.ty_of value <> Value.ty_of source then
        fail "E_DATA_SOURCE" "The host returned a data source of the wrong type.";
      Value.validate value;
      value

let rec is_live = function
  | Residual _ -> true
  | Source _ | Struct (_, Ty.Array _, _) -> true
  | List xs -> Array.exists is_live xs
  | Record fs | Struct (_, _, fs) -> List.exists (fun (_, v) -> is_live v) fs
  | _ -> false

(* the reserved argument of a card whose inlined body failed: its marshalled diagnostic *)
let node_error = "$node_error"
let card_kind = "$card"

(* the argument naming an inlined card's cook label on its context operators *)
let context_label = "$label"

(* The cooked inputs of an inlined call, in argument order. *)
let inline_inputs parameters vals =
  let rec ids = function
    | Deferred (_, id) -> [ id ]
    | List xs -> List.concat_map ids (Array.to_list xs)
    | _ -> []
  in
  List.concat_map (fun (name, v) -> if List.mem_assoc name parameters then [] else ids v) vals

(* What an inline body's evaluation is a function of: the declaration (the host's key), the
   canonical parameters and the shape of the inputs (cooked handles by type, not identity). None
   for a live or non-data argument, or outside the static evaluation. *)
let inline_key c ~key parameters vals =
  if c.st.time <> None || c.data then None
  else
    let rec shape = function
      | Deferred (ty, _) -> "D" ^ Ty.to_string ty
      | No_geo -> "N"
      | List xs -> "[" ^ String.concat "," (Array.to_list (Array.map shape xs)) ^ "]"
      | Source _ | Struct _ | Record _ | Fn _ | Residual _ -> raise Exit
      | v -> "=" ^ Value.key_of ~residual:(fun _ -> raise Exit) v
    in
    match
      Value.key_of ~residual:(fun _ -> raise Exit) (Record parameters)
      ^ "\000"
      ^ String.concat "\001"
          (List.filter_map
             (fun (name, v) ->
               if List.mem_assoc name parameters then None else Some (name ^ ":" ^ shape v))
             vals)
    with
    | k ->
        Some
          (Digest.string
             (Lazy.force key ^ "\000" ^ k ^ string_of_bool c.geometry ^ string_of_bool c.host))
    | exception _ -> None

(* The terms of the workspace's definitions (helpers and inlined node bodies, as evaluated) by
   form id, built once per evaluation. *)
let term_index st kinds =
  let index, indexed =
    match st.terms with
    | Some terms -> terms
    | None ->
        let terms = (Hashtbl.create 4096, Hashtbl.create 16) in
        st.terms <- Some terms;
        terms
  in
  let rec walk (x : W.term) =
    if x.form.id <> 0 && not (List.memq x (Hashtbl.find_all index x.form.id)) then begin
      Hashtbl.add index x.form.id x;
      List.iter walk (W.children x)
    end
    else if x.form.id = 0 then List.iter walk (W.children x)
  in
  let graph (g : W.graph) =
    walk g.body;
    List.iter (fun (_, _, d) -> Option.iter walk d) g.inputs
  in
  List.iter
    (fun kind ->
      if not (Hashtbl.mem indexed kind) then begin
        Hashtbl.add indexed kind ();
        if String.starts_with ~prefix:"def:" kind then
          Option.iter graph (Hashtbl.find_opt st.defs (String.sub kind 4 (String.length kind - 4)))
        else
          Option.iter
            (fun inlined ->
              let (d : W.node_definition), _, _ = Lazy.force inlined in
              graph d.body)
            (Hashtbl.find_opt st.node_defs kind)
      end)
    kinds;
  index

(* An inline body's kept evaluation, in this evaluation: its nodes at the next ids over these
   inputs, its residuals and functions at the next ids, its sites and iterations under this
   call's, its kernels offered again. The value equals what evaluating the body here gives. *)
let replay_inline c (e : inline_entry) inputs =
  let st = c.st in
  let lo = st.nnodes and rid0 = st.rids and fid0 = st.nfns in
  let site' = site c and iter' = c.iter in
  let inputs = List.combine e.e_inputs inputs in
  let id n =
    if n >= e.e_lo && n < e.e_lo + e.e_count then n - e.e_lo + lo
    else Option.value ~default:n (List.assoc_opt n inputs)
  in
  (* a kernel's or bulk function's negative id is global: each replay takes fresh ones *)
  let fresh = Hashtbl.create 8 in
  let bulk n =
    match Hashtbl.find_opt fresh n with
    | Some m -> m
    | None ->
        let m = Atomic.fetch_and_add bulk_ids (-1) in
        Hashtbl.add fresh n m;
        m
  in
  let rid r =
    if r < 0 then bulk r
    else if r > e.e_rid0 && r <= e.e_rid0 + e.e_nrids then r - e.e_rid0 + rid0
    else r
  in
  let fid f =
    if f < 0 then bulk f
    else if f >= e.e_fid0 && f < e.e_fid0 + e.e_nfids then f - e.e_fid0 + fid0
    else f
  in
  let rec rest prefix l =
    match (prefix, l) with
    | [], l -> Some l
    | x :: prefix, y :: l when x = y -> rest prefix l
    | _ -> None
  in
  let path p = match rest e.e_site p with Some tail -> site' @ tail | None -> p in
  let iter i = match rest e.e_iter i with Some tail -> iter' @ tail | None -> i in
  let ctx (x : ctx) = { x with st; inst = c.inst; prefix = path x.prefix; iter = iter x.iter } in
  (* a body's terms in this evaluation: its definitions check to the same form ids (library forms
     have fixed ids, an authored definition's are part of the key) *)
  let index = term_index st e.e_kinds in
  let term (t : W.term) =
    if t.form.id = 0 then t
    else
      match
        List.find_opt
          (fun (n : W.term) ->
            n.path = t.path && Obj.tag (Obj.repr n.node) = Obj.tag (Obj.repr t.node))
          (Hashtbl.find_all index t.form.id)
      with
      | Some n -> n
      | None -> raise Exit
  in
  let residuals = Hashtbl.create 16 and functions = Hashtbl.create 16 in
  let rec value = function
    | Deferred (ty, n) -> Deferred (ty, id n)
    | List xs -> List (Array.map value xs)
    | Record fs -> Record (fields fs)
    | Struct (n, ty, fs) -> Struct (n, ty, fields fs)
    | Source (n, ty, fs) -> Source (n, ty, fields fs)
    | Residual r -> Residual (residual r)
    | Fn f -> Fn (fn f)
    | v -> v
  and fields fs = List.map (fun (n, v) -> (n, value v)) fs
  and residual r =
    match List.find_opt (fun (o, _) -> o == r) (Hashtbl.find_all residuals r.rid) with
    | Some (_, m) -> m
    | None ->
        let m =
          {
            r with
            rid = rid r.rid;
            rterm = term r.rterm;
            renv = Smap.map value r.renv;
            rc = ctx r.rc;
            live = None;
          }
        in
        Hashtbl.add residuals r.rid (r, m);
        m
  and fn f =
    let key = match f with Closure cl -> cl.fid | Named n -> n.fid in
    match List.find_opt (fun (o, _) -> o == f) (Hashtbl.find_all functions key) with
    | Some (_, m) -> m
    | None ->
        let m =
          match f with
          | Closure cl ->
              Closure
                {
                  cl with
                  body = term cl.body;
                  env = Smap.map value cl.env;
                  zone = path cl.zone;
                  calls = ref !(cl.calls);
                  fid = fid cl.fid;
                  at = ctx cl.at;
                }
          | Named n -> Named { n with ncalls = ref !(n.ncalls); fid = fid n.fid }
        in
        Hashtbl.add functions key (f, m);
        m
  in
  (* everything is relocated before the evaluation changes: a term no longer found is no replay *)
  match
    let nodes =
      List.map
        (fun (n : node) ->
          {
            n with
            id = id n.id;
            inst = c.inst;
            site = path n.site;
            iter = iter n.iter;
            args = fields n.args;
          })
        e.e_nodes
    in
    (nodes, List.map residual e.e_observed, value e.e_value)
  with
  | exception Exit -> None
  | nodes, observed, v ->
      List.iter2
        (fun n form ->
          st.nodes <- n :: st.nodes;
          st.authored <- form :: st.authored)
        nodes e.e_authored;
      st.nnodes <- st.nnodes + e.e_count;
      st.rids <- st.rids + e.e_nrids;
      st.nfns <- st.nfns + e.e_nfids;
      st.inlined <- e.e_kinds @ st.inlined;
      List.iter
        (fun kind ->
          if String.starts_with ~prefix:"def:" kind then
            List.iter
              (fun t -> Hashtbl.replace t (String.sub kind 4 (String.length kind - 4)) ())
              st.watching)
        e.e_kinds;
      List.iter (fun r -> Effect.perform (Residual_effect.Observe_kernel r)) observed;
      Some v

exception Refused of Diagnostic.t

let rec concrete c v =
  match v with
  | Residual r -> ( match c.st.time with None -> raise Needs_t | Some _ -> force_res c r)
  | Source _ | Struct (_, Ty.Array _, _) -> (
      match (c.st.time, c.st.resolve) with
      | None, _ -> raise Needs_t
      | Some _, Some resolve -> resolve_source resolve v
      | Some _, None -> fail "E_DATA_SOURCE" "This data source needs its host's cooked data.")
  | v -> v

and force_res c r =
  match Hashtbl.find_opt c.st.memo r.rid with
  | Some v -> v
  | None ->
      let slow () =
        let c = { r.rc with st = c.st; kernel_requests = c.kernel_requests } in
        if r.previous then
          match r.rterm.node with
          | W.State { init; zone; _ } -> state_previous c r.renv zone init
          | _ -> assert false
        else ev c r.renv r.rterm
      in
      let v = slow () in
      Hashtbl.replace c.st.memo r.rid v;
      v

and ev c env (x : W.term) : value =
  let st = c.st in
  st.steps <- st.steps + 1;
  if st.steps > st.budget then
    failf "E_EVAL_BUDGET" "Evaluation budget exceeded (%d steps). Reduce iteration counts."
      st.budget;
  let c = match x.path with Some p -> { c with base = p; route = [] } | None -> c in
  match st.time with
  | None -> (
      (match x.node with
      | W.Hof ((`Map | `Reduce), _) | W.Loop _ | W.Op { op = "array/sum" | "image/map"; _ } ->
          Effect.perform
            (Residual_effect.Observe_kernel
               {
                 rid = Atomic.fetch_and_add bulk_ids (-1);
                 rterm = x;
                 renv = capture x env;
                 rc = c;
                 previous = false;
                 live = None;
                 dep_frame = 0;
                 dep_state = 0;
               })
      | _ -> ());
      let saved = st.nodes and saved_n = st.nnodes and saved_authored = st.authored in
      match ev_raw c env x with
      | v ->
          (match x.path with Some p -> note c p v | None -> ());
          v
      | exception Needs_t ->
          (* ponytail: nodes made by the abandoned attempt are dropped, its records are kept *)
          st.nodes <- saved;
          st.nnodes <- saved_n;
          st.authored <- saved_authored;
          st.rids <- st.rids + 1;
          let r =
            Residual
              {
                rid = st.rids;
                rterm = x;
                renv = capture x env;
                rc = c;
                previous = false;
                live = None;
                dep_frame = 0;
                dep_state = 0;
              }
          in
          (match x.node with W.State _ -> st.states <- r :: st.states | _ -> ());
          (* the record is the residual: a probe forces it at the time it shows *)
          (match x.path with Some p -> note c p r | None -> ());
          r
      | exception Diagnostic.Error d when d.span = None ->
          raise_diagnostic { d with span = span_of x })
  | Some live -> (
      let evaluate () = ev_raw c env x in
      let kernel =
        match x.node with
        | (W.Hof ((`Map | `Reduce), _) | W.Loop _ | W.Op { op = "array/sum" | "image/map"; _ })
          when c.kernel_requests ->
            let residual =
              {
                rid = Atomic.fetch_and_add bulk_ids (-1);
                rterm = x;
                renv = capture x env;
                rc = c;
                previous = false;
                live = None;
                dep_frame = 0;
                dep_state = 0;
              }
            in
            Effect.perform (Residual_effect.Kernel_value residual)
        | _ -> None
      in
      match match kernel with Some value -> value | None -> concrete c (evaluate ()) with
      | exception Diagnostic.Error d when d.span = None ->
          raise_diagnostic { d with span = span_of x }
      | v ->
          if st.record && c.rec_ then Option.iter (fun p -> note c p v) x.path;
          v)

and evs c env prefix ts = List.mapi (fun i t -> ev (sub c (prefix ^ string_of_int i)) env t) ts

and eval_named c env args =
  (* ponytail: O(n²) uniqueness scan bounded to 16 slots; wider/repeated
     signatures retain the hash path. Use declaration metadata if this
     small scan becomes measurable. *)
  let rec distinct = function
    | [] -> true
    | (name, _) :: rest -> (not (List.mem_assoc name rest)) && distinct rest
  in
  if List.compare_length_with args 16 <= 0 && distinct args then
    List.map (fun (name, term) -> (name, ev (sub c name) env term)) args
  else
    let counts = Hashtbl.create 4 in
    List.map
      (fun (name, t) ->
        let n = Option.value (Hashtbl.find_opt counts name) ~default:0 in
        Hashtbl.replace counts name (n + 1);
        let seg = if n = 0 then name else name ^ "#" ^ string_of_int n in
        (name, ev (sub c seg) env t))
      args

and field c v f name = lookup_field name (concrete c v) f

and bind_parts c ~mk pat v env =
  match pat with
  | W.Name n ->
      (match mk with Some mk -> note c (mk n) v | None -> ());
      Smap.add n v env
  | W.Seq ps ->
      let xs =
        match concrete c v with
        | List xs -> xs
        | Vec2 (a, b) -> [| Float a; Float b |]
        | Vec3 (a, b, d) -> [| Float a; Float b; Float d |]
        | Vec4 (a, b, d, e) -> [| Float a; Float b; Float d; Float e |]
        | _ -> failf "E_PATTERN" "%s destructures a list or vec3." (pat_key pat)
      in
      if Array.length xs < List.length ps then
        failf "E_PATTERN" "%s needs %d elements; the list has %d." (pat_key pat) (List.length ps)
          (Array.length xs);
      snd (List.fold_left (fun (i, env) p -> (i + 1, bind_parts c ~mk p xs.(i) env)) (0, env) ps)
  | W.Keys ks ->
      let r = concrete c v in
      List.fold_left
        (fun env k -> bind_parts c ~mk (W.Name k) (lookup_field (pat_key pat) r k) env)
        env ks

(* [whole]: also record the value of a destructuring pattern under its printed name *)
and bind_pat c ~mk ~whole pat v env =
  (match (pat, mk) with
  | (W.Seq _ | W.Keys _), Some mk when whole -> note c (mk (pat_key pat)) v
  | _ -> ());
  match pat with W.Name n when not whole -> Smap.add n v env | _ -> bind_parts c ~mk pat v env

and ev_raw c env (x : W.term) : value =
  match x.node with
  | W.Lit (Param.Int_value n) -> Int n
  | W.Lit (Param.Float_value f) -> Float f
  | W.Lit (Param.Bool_value b) -> Bool b
  | W.Lit _ -> fail "E_TYPE" "Unexpected literal."
  | W.Text s -> Text s
  | W.Nil -> No_geo
  | W.Time -> ( match c.st.time with Some t -> Float t.t | None -> raise Needs_t)
  | W.Vec [ a; b ] ->
      let f i t = num (concrete c (ev (sub c (string_of_int i)) env t)) in
      let a = f 0 a in
      let b = f 1 b in
      Vec2 (a, b)
  | W.Vec [ a; b; d ] ->
      let f i t = num (concrete c (ev (sub c (string_of_int i)) env t)) in
      let a = f 0 a in
      let b = f 1 b in
      let d = f 2 d in
      Vec3 (a, b, d)
  | W.Vec [ a; b; d; e ] ->
      let f i t = num (concrete c (ev (sub c (string_of_int i)) env t)) in
      let a = f 0 a in
      let b = f 1 b in
      let d = f 2 d in
      let e = f 3 e in
      Vec4 (a, b, d, e)
  | W.Vec cs ->
      failf "E_VECTOR" "A vector has 2, 3 or 4 components; this one has %d." (List.length cs)
  | W.Ref_binding (b, []) when String.starts_with ~prefix:"$elem:" b -> (
      match Smap.find_opt b c.st.elems with Some v -> v | None -> raise Needs_t)
  | W.Ref_binding (b, fs) -> (
      match Smap.find_opt b env with
      | None -> failf "E_UNBOUND" "%s is not bound." b
      | Some v -> fst (List.fold_left (fun (v, p) f -> (field c v f p, p ^ "." ^ f)) (v, b) fs))
  | W.Call { kind; ctx; args } -> (
      let vals = eval_named c env args in
      let vals =
        if kind = "sop/merge" then
          List.concat_map
            (fun (n, v) ->
              match concrete c v with
              | List xs -> List.map (fun g -> (n, g)) (Array.to_list xs)
              | v -> [ (n, v) ])
            vals
          |> List.filter (fun (_, v) -> match v with No_geo -> false | _ -> true)
        else vals
      in
      let call vals = catalog_call c ~authored:x.form.id kind ctx vals in
      match Hashtbl.find_opt c.st.node_defs kind with
      | Some inlined -> (
          let d, key, canonical = Lazy.force inlined in
          match canonical vals with
          | Some parameters -> inline_node c ~form:x.form.id ~key d parameters ~call vals
          | None -> call vals)
      | _ -> call vals)
  | W.Op { op = "scene/merge"; args; skip = _ :: _ as skip } ->
      (* register L16: the arguments at skipped tuples are not evaluated *)
      let args = List.filteri (fun p _ -> not (List.mem (c.iter @ [ p ]) skip)) args in
      apply_op c ~authored:x.form.id "scene/merge" (eval_named c env args)
  | W.Op { op; args; _ } -> apply_op c ~authored:x.form.id op (eval_named c env args)
  | W.Call_fn { fn; args; body } -> (
      let vals = evs c env "a" args in
      match Smap.find_opt fn env with
      | Some (Fn f) -> call_fn ?body c f vals
      | _ -> apply_def ?body c fn vals)
  (* ponytail: a catalog kind used as a function value keeps the name as written; lowering resolves it. *)
  | W.Fn_ref name -> Fn (Named { name; ncalls = ref 0; fid = fn_id c.st })
  | W.Graph_ref { graph; inputs } ->
      let over = eval_named c env inputs in
      graph_value ~rec_:(over = []) c graph over
  | W.Let (binds, res) ->
      let env =
        List.fold_left
          (fun env (pat, (t : W.term)) ->
            let v = ev c env t in
            let v =
              match v with Residual _ when diagnostic_effect c.st env t -> concrete c v | _ -> v
            in
            let pfx =
              match t.path with Some p -> List.take (max 0 (List.length p - 1)) p | None -> []
            in
            bind_pat c ~mk:(Some (fun n -> pfx @ [ n ])) ~whole:false pat v env)
          env binds
      in
      ev c env res
  | W.Loop l -> loop c env ~out:x.ty l.kind l.accs l.clauses l.skip l.body l.zone
  | W.State { binder; init; step; zone } -> (
      if c.geometry then
        fail "E_STATE_ELEMENT"
          "Frame state belongs to the workspace value lane; bind it outside a loop over cooked \
           geometry.";
      let st = c.st in
      let previous =
        match st.time with
        | None ->
            st.rids <- st.rids + 1;
            Residual
              {
                rid = st.rids;
                rterm = x;
                renv = capture x env;
                rc = c;
                previous = true;
                live = None;
                dep_frame = 0;
                dep_state = 0;
              }
        | Some _ -> state_previous c env zone init
      in
      let env =
        bind_pat c ~mk:(Some (fun n -> zone @ [ ":" ^ n ])) ~whole:true binder previous env
      in
      match st.time with
      | None ->
          ignore (ev c env step);
          raise Needs_t
      | Some _ -> (
          let state, _ = state_plane c in
          let key = state_key c zone in
          match State_map.find_opt key state.next with
          | Some v -> v
          | None ->
              let v = state_data c (coerce_to x.ty (ev c env step)) in
              state.next <- State_map.add key v state.next;
              state.epoch <- fresh_state_epoch ();
              v))
  | W.Error_rows { rows; policy; mapper } ->
      if c.st.time = None then raise Needs_t;
      let value = concrete_data c (ev (sub c "0") env rows) in
      Value.diagnose_rows value
        ~select:(fun rows first ->
          match (c.st.resolve, policy) with
          | None, W.First -> first
          | None, W.Range_last ->
              fail "E_DIAGNOSTIC_CONTEXT" "Range-last diagnosis needs its native host."
          | Some resolve, policy -> (
              let policy = match policy with W.First -> "first" | Range_last -> "range-last" in
              match
                resolve_source resolve
                  (Source
                     ("$core/diagnostic_index", Ty.Int, [ ("rows", rows); ("policy", Text policy) ]))
              with
              | Int index -> index
              | _ -> fail "E_DIAGNOSTIC_CONTEXT" "Host diagnosis returns one integer row."))
        ~map:(fun index ->
          match concrete c (ev (sub c "1") env mapper) with
          | Fn fn -> concrete_data c (call_fn c fn [ index ])
          | _ -> fail "E_DIAGNOSTIC_MAP" "A row mapper is a function.")
  | W.With_error (protected, mapper) ->
      if c.st.time = None then raise Needs_t;
      Value.with_error
        ~protected:(fun () ->
          let value = concrete_data c (ev (sub c "0") env protected) in
          match (value, c.st.resolve) with
          | Deferred (ty, _), Some resolve when Ty.is_cooked ty ->
              resolve_source resolve (Source ("$core/complete", ty, [ ("value", value) ]))
          | Deferred (ty, _), None when Ty.is_cooked ty ->
              fail "E_DATA_SOURCE" "A protected cooked result needs its host."
          | _ -> value)
        ~map:(fun diagnostic ->
          match concrete c (ev (sub c "1") env mapper) with
          | Fn fn -> concrete_data c (call_fn c fn [ diagnostic ])
          | _ -> fail "E_DIAGNOSTIC_MAP" "An error mapper is a function.")
  | W.If (cnd, a, b) ->
      if truthy (concrete c (ev (sub c "if") env cnd)) then ev (sub c "then") env a
      else ev (sub c "else") env b
  | W.Cond (arms, default) ->
      let rec go i = function
        | [] -> ev (sub c "else") env default
        | (t, e) :: rest ->
            if truthy (concrete c (ev (sub c ("test" ^ string_of_int i)) env t)) then
              ev (sub c ("arm" ^ string_of_int i)) env e
            else go (i + 1) rest
      in
      go 0 arms
  | W.Case (scrut, arms, default) ->
      let sv = concrete c (ev (sub c "case") env scrut) in
      let rec go i = function
        | [] -> ev (sub c "else") env default
        | (lit, e) :: rest ->
            if case_matches sv (case_literal lit) then ev (sub c ("arm" ^ string_of_int i)) env e
            else go (i + 1) rest
      in
      go 0 arms
  | W.Fn { params; body; zone; capture = original } -> (
      match original with
      | None ->
          Fn
            (Closure
               { params; body; env = capture x env; zone; calls = ref 0; fid = fn_id c.st; at = c })
      | Some original -> (
          match ev c env original with
          | Fn (Closure cl) -> Fn (Closure { cl with params; body })
          | Fn (Named n) ->
              Fn
                (Closure
                   { params; body; zone; env = Smap.empty; calls = n.ncalls; fid = n.fid; at = c })
          | _ -> fail "E_TYPE" "A function port requires a function."))
  | W.Hof (kind, f :: rest) -> hof c env x kind f rest
  | W.Hof (_, []) -> fail "E_ARITY" "A higher-order form takes a function."
  | W.List_lit ts -> List (join_values (Array.of_list (evs c env "" ts)))
  | W.Record fs -> Record (List.map (fun (n, t) -> (n, ev (sub c n) env t)) fs)
  | W.Get (r, f) -> field c (ev c env r) f (Lisp.flat r.form)
  | W.Assoc (r, ups) ->
      let fs =
        match concrete c (ev c env r) with
        | Record fs -> fs
        | _ -> fail "E_TYPE" "assoc updates a record."
      in
      let fs =
        List.fold_left
          (fun fs (n, t) ->
            let v = ev (sub c n) env t in
            match List.assoc_opt n fs with
            | Some old ->
                List.map (fun (k, x) -> if k = n then (k, coerce_like c old v) else (k, x)) fs
            | None -> fs @ [ (n, v) ])
          fs ups
      in
      Record fs
  | W.Str ts -> Text (String.concat "" (List.map (show_with (concrete c)) (evs c env "" ts)))
  | W.List_op (_, ts) ->
      let xs =
        List.concat_map (fun v -> Array.to_list (list_arg (concrete c v))) (evs c env "" ts)
      in
      let xs = join_values (Array.of_list xs) in
      if Array.length xs > max_concat then fail "E_ITER_BOUND" "concat exceeds 4,096 elements.";
      List xs
  | W.Bypass t -> (
      match W.bypass_input t with
      | Some input -> coerce_to t.ty (ev (sub c "bypass") env input)
      | None -> fail "E_BYPASS" "Nothing to pass through.")
  | W.Expanded { body; _ } -> ev c env body

and state_previous c env zone init =
  let state, frame = state_plane c in
  begin_frame state (Lazy.force c.st.source) frame;
  match State_map.find_opt (state_key c zone) state.before with
  | Some v -> state_data c v
  | None -> state_data c (ev c env init)

and state_data c value =
  match concrete c value with
  | Deferred _ | No_geo -> fail "E_STATE_TYPE" "State stores data, not deferred nodes or layouts."
  | Struct (_, ty, _) when Ty.shape ty ->
      fail "E_STATE_TYPE" "State stores data, not deferred nodes or layouts."
  | Fn _ -> fail "E_FN_ESCAPES" "A function value cannot be stored in state."
  | List values -> List (Array.map (state_data c) values)
  | Record fields -> Record (List.map (fun (name, value) -> (name, state_data c value)) fields)
  | Struct (name, ty, fields) ->
      Struct (name, ty, List.map (fun (name, value) -> (name, state_data c value)) fields)
  | value -> value

and concrete_data c value =
  match concrete c value with
  | List values -> List (Array.map (concrete_data c) values)
  | Record fields -> Record (List.map (fun (name, v) -> (name, concrete_data c v)) fields)
  | Struct (name, ty, fields) ->
      Struct (name, ty, List.map (fun (name, v) -> (name, concrete_data c v)) fields)
  | value -> value

(* Resolve only the old fields/elements that the new value actually updates. *)
and coerce_like c w v =
  match (concrete c w, concrete c v) with
  | List a, (List _ as v) when Array.length a = 0 -> v
  | List a, List b when Array.length a > 0 -> List (Array.map (coerce_like c a.(0)) b)
  | Record wf, Record fs ->
      Record
        (List.map
           (fun (name, v) ->
             (name, match List.assoc_opt name wf with Some old -> coerce_like c old v | None -> v))
           fs)
  | w, v -> coerce_to (Value.ty_of w) v

(* Numeric accumulators widen an Int seed; record updates retain its tag. *)
and widen_like c w v =
  let w = concrete c w and v = concrete c v in
  match (w, v) with Int _, Float _ -> v | _ -> coerce_like c w v

and apply_op c ?(authored = 0) name (vals : (string * value) list) : value =
  let o =
    match find_op c.st name with
    | Some o -> o
    | None -> failf "E_UNKNOWN" "Unknown operator %s." name
  in
  let vals =
    match c.card with
    | Some label when List.mem name W.body_context_ops && not (List.mem_assoc context_label vals) ->
        vals @ [ (context_label, Text label) ]
    | _ -> vals
  in
  let vals =
    match o.shape with
    | Op.Struct { splice = true } ->
        List.concat_map
          (fun (n, v) ->
            match concrete c v with
            | List xs -> List.map (fun x -> (n, x)) (Array.to_list xs)
            | v -> [ (n, v) ])
          vals
    | _ -> vals
  in
  let vals =
    match o.shape with
    | Op.Scalar when o.ctx = Context.value ->
        let rec data_type = function
          | Ty.Float | Int | Bool | Vec2 | Vec3 | Vec4 | Text | Color -> true
          | Ty.List ty | Array ty -> data_type ty
          | Ty.Record fields -> fields <> [] && List.for_all (fun (_, ty) -> data_type ty) fields
          | Ty.Any | Named _ | Fn _ -> false
        in
        let declared = o.signature.pos @ o.signature.opt @ o.signature.kw in
        List.map
          (fun (n, v) ->
            let ty =
              match List.assoc_opt n declared with
              | Some _ as ty -> ty
              | None -> (
                  match o.signature.rest with Some (name, ty) when name = n -> Some ty | _ -> None)
            in
            let deep =
              o.name = "equal?"
              || match ty with Some ((Ty.List _ | Record _) as ty) -> data_type ty | _ -> false
            in
            (n, if deep then concrete_data c v else concrete c v))
          vals
    | _ -> vals
  in
  o.check vals;
  let live =
    match c.st.time with
    | Some live -> live
    | None when o.live -> raise Needs_t
    | None -> empty_live
  in
  let scalar = function
    | Int n -> Some (Param.Int_value n)
    | Float n -> Some (Param.Float_value n)
    | Bool b -> Some (Param.Bool_value b)
    | Text text -> Some (Param.Text_value text)
    | _ -> None
  in
  let ty =
    Op.result_type o (List.map (fun (name, value) -> (name, Value.ty_of value, scalar value)) vals)
  in
  o.body ~live ~node:(mk_node c ~authored ~ty) vals

(* A card's body runs in the caller. Its parameters take the host's path (ranges, choices,
   encoded text) and its validations refuse the program, as a node build would; an error in the
   body itself is that card's cook error: the body's nodes are dropped and the card lowers as a node
   that fails with the diagnostic when cooked. Each body has its own step budget, as before. A
   call with live arguments, or one the host keeps as its own node, lowers as before. *)
and inline_node c ~form ~key (d : W.node_definition) parameters ~call vals =
  if c.depth > max_depth then fail "E_DEPTH" "Call depth exceeds 64.";
  let base = [ "node:" ^ d.kind.qualified ] in
  let label =
    match String.index_opt d.kind.qualified '/' with
    | Some i -> String.sub d.kind.qualified (i + 1) (String.length d.kind.qualified - i - 1)
    | None -> d.kind.qualified
  in
  (* a body's own probes are not recorded: its card is the authored record, and a replayed body
     (below) records nothing *)
  let c' =
    {
      c with
      prefix = site c;
      base;
      route = [];
      depth = c.depth + 1;
      card = Some label;
      rec_ = false;
    }
  in
  let rec flat = function
    | List xs -> List.concat_map flat (Array.to_list xs)
    | No_geo -> []
    | v -> [ v ]
  in
  let bind env (pname, ty, default) =
    let v =
      if List.exists (fun (s : Catalog.slot) -> s.rest && s.name = pname) d.slots then
        coerce_to ty
          (List
             (Array.of_list (List.concat_map (fun (n, v) -> if n = pname then flat v else []) vals)))
      else
        match (List.assoc_opt pname parameters, List.assoc_opt pname vals, default) with
        | Some v, _, _ | None, Some v, _ -> coerce_to ty v
        | None, None, Some t -> coerce_to ty (ev (sub c' pname) env t)
        | None, None, None -> No_geo
    in
    Smap.add pname v env
  in
  let refusal code message =
    Refused
      (Diagnostic.error ~code:"E_LOWER"
         (if code = "E_PARAM" then message else code ^ ": " ^ message))
  in
  let st = c.st in
  let steps = st.steps and nodes = st.nodes and authored = st.authored and nnodes = st.nnodes in
  let drop () =
    st.nodes <- nodes;
    st.authored <- authored;
    st.nnodes <- nnodes
  in
  st.steps <- 0;
  let key =
    inline_key c ~key:(lazy (Lazy.force key ^ string_of_int d.body.body.form.id)) parameters vals
  in
  let replay =
    Option.bind key (fun key ->
        Mutex.protect inline_lock (fun () -> Hashtbl.find_opt inline_memo key))
  in
  match Option.bind replay (fun entry -> replay_inline c entry (inline_inputs parameters vals)) with
  | Some v ->
      st.steps <- steps;
      card c ~form d parameters v
  | None -> (
      let inlined0 = st.inlined in
      st.inlined <- d.kind.qualified :: st.inlined;
      let rids = st.rids and fids = st.nfns and states = st.states and cells = st.cells in
      let observed = ref [] and cacheable = ref (key <> None) in
      let body () =
        let env = List.fold_left bind Smap.empty d.body.inputs in
        List.iter
          (fun (predicate, code, message) ->
            match concrete c' (ev (sub c' "validate") env predicate) with
            | Bool true -> ()
            | Bool false -> raise (refusal code message)
            | exception Diagnostic.Error e -> raise (refusal e.code e.message)
            | _ -> invalid_arg "A Lisp node validation must return bool")
          d.validations;
        ev c' env d.body.body
      in
      (* the kernels the body offers are offered again on a replay; a device approximation makes the
     body's value depend on the device, so it is not kept *)
      let helpers = Hashtbl.create 8 in
      let watched () =
        if key = None then body ()
        else begin
          st.watching <- helpers :: st.watching;
          Fun.protect ~finally:(fun () -> st.watching <- List.tl st.watching) @@ fun () ->
          Effect.Deep.match_with body ()
            {
              retc = Fun.id;
              exnc = raise;
              effc =
                (fun (type a) (e : a Effect.t) ->
                  match e with
                  | Residual_effect.Observe_kernel r ->
                      Some
                        (fun (k : (a, _) Effect.Deep.continuation) ->
                          observed := r :: !observed;
                          Effect.Deep.continue k (Effect.perform (Residual_effect.Observe_kernel r)))
                  | Residual_effect.Bulk_approx r ->
                      Some
                        (fun (k : (a, _) Effect.Deep.continuation) ->
                          let v = Effect.perform (Residual_effect.Bulk_approx r) in
                          if v <> None then cacheable := false;
                          Effect.Deep.continue k v)
                  | _ -> None);
            }
        end
      in
      match watched () with
      | Residual _
        when List.exists (fun (_, v) -> match v with Residual _ -> true | _ -> false) vals ->
          (* a structure that follows a per-frame argument: the card is rebuilt per frame, as before *)
          st.steps <- steps;
          drop ();
          call vals
      | v ->
          st.steps <- steps;
          (match key with
          | Some key when !cacheable && st.states == states && st.cells == cells ->
              let count = st.nnodes - nnodes in
              let rec take n l = if n = 0 then [] else List.hd l :: take (n - 1) (List.tl l) in
              let entry =
                {
                  e_nodes = List.rev (take count st.nodes);
                  e_authored = List.rev (take count st.authored);
                  e_value = v;
                  e_observed = List.rev !observed;
                  e_lo = nnodes;
                  e_count = count;
                  e_inputs = inline_inputs parameters vals;
                  e_rid0 = rids;
                  e_nrids = st.rids - rids;
                  e_fid0 = fids;
                  e_nfids = st.nfns - fids;
                  e_site = site c;
                  e_iter = c.iter;
                  e_kinds =
                    (let rec since = function
                       | l when l == inlined0 -> []
                       | kind :: rest -> kind :: since rest
                       | [] -> []
                     in
                     since st.inlined
                     @ Hashtbl.fold (fun name () kinds -> ("def:" ^ name) :: kinds) helpers []);
                }
              in
              Mutex.protect inline_lock (fun () ->
                  if Hashtbl.length inline_memo >= 256 then Hashtbl.reset inline_memo;
                  Hashtbl.replace inline_memo key entry)
          | _ -> ());
          card c ~form d parameters v
      | exception Needs_t ->
          (* a validation that reads a live argument *)
          st.steps <- steps;
          drop ();
          call vals
      | exception Refused e ->
          st.steps <- steps;
          raise (Diagnostic.Error e)
      | exception Diagnostic.Error e when e.code <> "cancelled" && e.code <> "E_CANCELLED" ->
          st.steps <- steps;
          drop ();
          (* ponytail: the diagnostic rides as marshalled text in a reserved argument *)
          call (vals @ [ (node_error, Text (Marshal.to_string e [])) ])
      | exception x ->
          st.steps <- steps;
          raise x)

(* The card marker: a plan node at the call site naming the card (its path, kind, canonical
   parameters), what the host keys per-card ids, display, probes, provenance, facts and caching by;
   the call's value is the marker. A body whose geometry is decided by cooked data (a residual)
   cooks behind it, one card's structure as before; otherwise it passes the body's result through. *)
and card c ~form (d : W.node_definition) parameters v =
  let marker () =
    let st = c.st in
    let n =
      {
        id = st.nnodes;
        inst = c.inst;
        site = site c;
        iter = c.iter;
        kind = card_kind;
        ty = Ty.geometry;
        args = [ ("value", v); ("kind", Text d.kind.qualified); ("parameters", Record parameters) ];
      }
    in
    st.nodes <- n :: st.nodes;
    st.authored <- form :: st.authored;
    st.nnodes <- st.nnodes + 1;
    Deferred (Ty.geometry, n.id)
  in
  if c.st.time <> None || c.data then v
  else
    match v with
    | Residual r when r.rterm.W.ty = Ty.geometry -> marker ()
    | Deferred (ty, _) when ty = Ty.geometry -> marker ()
    | _ -> v

and apply_def ?body c name (vals : value list) : value =
  let d =
    match Hashtbl.find_opt c.st.defs name with
    | Some d -> d
    | None -> failf "E_UNKNOWN" "Unknown function %s." name
  in
  if c.depth > max_depth then fail "E_DEPTH" "Call depth exceeds 64.";
  List.iter (fun t -> Hashtbl.replace t name ()) c.st.watching;
  let base = [ "def:" ^ name ] in
  let c' = { c with prefix = site c; base; route = []; depth = c.depth + 1 } in
  let env, _ =
    List.fold_left
      (fun (env, i) (pname, ty, default) ->
        let v =
          match (List.nth_opt vals i, default) with
          | Some v, _ -> coerce_to ty v
          | None, Some t -> coerce_to ty (ev (sub c' pname) env t)
          | None, None -> failf "E_ARGS" "%s needs :%s." name pname
        in
        note c' (base @ [ ":" ^ pname ]) v;
        (Smap.add pname v env, i + 1))
      (Smap.empty, 0) d.inputs
  in
  ev c' env (Option.value body ~default:d.body)

and call_fn ?body c f (vals : value list) : value =
  match f with
  | Closure cl ->
      if List.length vals <> List.length cl.params then
        failf "E_ARITY" "%s takes %d argument%s; got %d." (path_text cl.zone)
          (List.length cl.params)
          (if List.length cl.params = 1 then "" else "s")
          (List.length vals);
      if c.depth > max_depth then fail "E_DEPTH" "Call depth exceeds 64.";
      let k = fn_call c cl.fid cl.calls in
      let c' =
        {
          c with
          inst = cl.at.inst;
          prefix = cl.at.prefix;
          base = cl.zone;
          route = [];
          iter = c.iter @ [ k ];
          depth = c.depth + 1;
        }
      in
      let mk = Some (fun n -> cl.zone @ [ ":" ^ n ]) in
      let env =
        List.fold_left2
          (fun env (pat, ty) v ->
            let v = match ty with Some t -> coerce_to t v | None -> v in
            bind_pat c' ~mk ~whole:true pat v env)
          cl.env cl.params vals
      in
      ev c' env (Option.value body ~default:cl.body)
  | Named { name; ncalls; fid } ->
      let k = fn_call c fid ncalls in
      let c' = { c with iter = c.iter @ [ k ] } in
      if Hashtbl.mem c.st.defs name then apply_def ?body c' name vals
      else begin
        match find_op c.st name with
        | Some o ->
            let names = Array.of_list (List.map fst (o.signature.pos @ o.signature.opt)) in
            let named =
              List.mapi
                (fun i v ->
                  ( (if i < Array.length names then names.(i)
                     else
                       match o.signature.rest with
                       | Some (name, _) -> name
                       | None -> "$" ^ string_of_int i),
                    v ))
                vals
            in
            apply_op c' o.name named
        | None ->
            (* a catalog kind: the checker resolved its name and which input each argument is *)
            let kind, ctx, slots =
              match List.assoc_opt name c.st.kind_fns with
              | Some k -> k
              | None -> failf "E_UNKNOWN" "Unknown function %s." name
            in
            let slots = Array.of_list slots in
            let rest =
              if Array.length slots = 0 then None
              else
                let slot = slots.(Array.length slots - 1) in
                if slot.Catalog.rest then Some slot.name else None
            in
            if List.length vals > Array.length slots && Option.is_none rest then
              failf "E_ARITY" "%s takes at most %d inputs." kind (Array.length slots);
            let args =
              List.mapi
                (fun i v ->
                  ((if i < Array.length slots then slots.(i).Catalog.name else Option.get rest), v))
                vals
            in
            catalog_call c' kind ctx args
      end

and hof c env term kind f rest =
  let out = term.W.ty in
  let packed =
    List.exists (fun (t : W.term) -> match t.ty with Ty.Array _ -> true | _ -> false) rest
  in
  let numeric =
    out <> Ty.Array Ty.Text && not (List.exists (fun (t : W.term) -> t.ty = Ty.Array Ty.Text) rest)
  in
  let c = if packed then { c with data = true } else c in
  let saved_steps = c.st.steps and calls = ref 0 in
  let call fv values =
    if packed && !calls mod packed_block_size = 0 then c.st.steps <- saved_steps;
    incr calls;
    call_fn c fv values
  in
  let work () =
    let fv =
      match concrete c (ev (sub c "f") env f) with
      | Fn f -> f
      | _ -> fail "E_TYPE" "Expected a function."
    in
    (match (kind, fv) with
    | `Map, Closure cl
      when c.st.record && c.rec_ && match out with Ty.Array _ -> true | _ -> false ->
        c.st.rids <- c.st.rids + 1;
        let call =
          Residual
            {
              rid = c.st.rids;
              rterm = term;
              renv = capture term env;
              rc = c;
              previous = false;
              live = None;
              dep_frame = 0;
              dep_state = 0;
            }
        in
        note c (cl.zone @ [ "~calls" ]) (Record [ ("offset", Int !(cl.calls)); ("call", call) ])
    | _ -> ());
    let lists_of ts =
      List.mapi
        (fun i t ->
          let v = concrete c (ev (sub c ("l" ^ string_of_int i)) env t) in
          match v with
          | List xs -> (Array.length xs, fun k -> xs.(k))
          | Text_array _ | Int_array _ | Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _
            ->
              (array_length v, array_get v)
          | _ -> fail "E_TYPE" "Expected a list or packed array.")
        ts
    in
    match (kind, rest) with
    | `Map, ls -> (
        let ls = lists_of ls in
        let n = List.fold_left (fun n (length, _) -> min n length) max_int ls in
        let n = if ls = [] then 0 else n in
        let item k = call fv (List.map (fun (_, get) -> get k) ls) in
        match (out, c.st.time) with
        | Ty.Array Ty.Int, None when numeric && n > max_iterations -> (
            let head =
              match array_init Ty.Int max_iterations (fun k -> concrete c (item k)) with
              | Int_array data -> data
              | _ -> assert false
            in
            let whole =
              Effect.perform
                (Residual_effect.Bulk_static
                   {
                     rid = Atomic.fetch_and_add bulk_ids (-1);
                     rterm = term;
                     renv = capture term env;
                     rc = c;
                     previous = false;
                     live = None;
                     dep_frame = 0;
                     dep_state = 0;
                   })
            in
            match whole with
            | Some (Int_array data as value)
              when Array.length data = n
                   && Array.for_all2 Int.equal head (Array.sub data 0 max_iterations) ->
                (match fv with
                | Closure cl -> cl.calls := !(cl.calls) + (n - max_iterations)
                | Named f -> f.ncalls := !(f.ncalls) + (n - max_iterations));
                value
            | _ ->
                if n > Sys.max_array_length then
                  fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
                let data = Array.make n 0 in
                Array.blit head 0 data 0 max_iterations;
                for k = max_iterations to n - 1 do
                  Value.int_array_put data k (concrete c (item k))
                done;
                Int_array data)
        | Ty.Array e, None when numeric && n > max_iterations -> (
            (* A large static packed map. The walker makes the first 4,096 elements as always (their
              records, call indices and failures are the ordinary ones); the host may then supply
              the whole array, which is taken only when it has the walked elements bit for bit. Any
              other outcome walks the rest: the value never depends on the host. *)
            let head = array_init e max_iterations (fun k -> concrete c (item k)) in
            let width, walked =
              match head with
              | Vec2_array d -> (2, d)
              | Vec3_array d -> (3, d)
              | Vec4_array d -> (4, d)
              | Float_array d -> (1, d)
              | _ -> assert false
            in
            let same (d : float array) =
              Array.length d = n * width
              &&
              let i = ref 0 in
              while
                !i < Array.length walked
                && Int64.equal (Int64.bits_of_float d.(!i)) (Int64.bits_of_float walked.(!i))
              do
                incr i
              done;
              !i = Array.length walked
            in
            let tolerant (d : float array) =
              Array.length d = n * width
              &&
              let ok = ref true in
              Array.iteri
                (fun i w ->
                  let g = d.(i) in
                  if not (Float.abs (g -. w) <= 1e-4 *. (1. +. Float.abs w)) then ok := false)
                walked;
              !ok
            in
            let approx_whole =
              match
                Effect.perform
                  (Residual_effect.Bulk_approx
                     {
                       rid = Atomic.fetch_and_add bulk_ids (-1);
                       rterm = term;
                       renv = capture term env;
                       rc = c;
                       previous = false;
                       live = None;
                       dep_frame = 0;
                       dep_state = 0;
                     })
              with
              | Some (Float_array d as v) when width = 1 && tolerant d -> Some v
              | Some (Vec2_array d as v) when width = 2 && tolerant d -> Some v
              | Some (Vec3_array d as v) when width = 3 && tolerant d -> Some v
              | Some (Vec4_array d as v) when width = 4 && tolerant d -> Some v
              | _ -> None
            in
            let whole =
              if Option.is_some approx_whole then approx_whole
              else
                match
                  Effect.perform
                    (Residual_effect.Bulk_static
                       {
                         rid = Atomic.fetch_and_add bulk_ids (-1);
                         rterm = term;
                         renv = capture term env;
                         rc = c;
                         previous = false;
                         live = None;
                         dep_frame = 0;
                         dep_state = 0;
                       })
                with
                | Some (Float_array d as v) when width = 1 && same d -> Some v
                | Some (Vec2_array d as v) when width = 2 && same d -> Some v
                | Some (Vec3_array d as v) when width = 3 && same d -> Some v
                | Some (Vec4_array d as v) when width = 4 && same d -> Some v
                | _ -> None
            in
            match whole with
            | Some v ->
                (* the calls the walker would have made *)
                (match fv with
                | Closure cl -> cl.calls := !(cl.calls) + (n - max_iterations)
                | Named f -> f.ncalls := !(f.ncalls) + (n - max_iterations));
                v
            | None -> (
                if n > Sys.max_floatarray_length / width then
                  fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
                let data = Array.make (n * width) 0. in
                Array.blit walked 0 data 0 (Array.length walked);
                for k = max_iterations to n - 1 do
                  Value.array_put data width k (concrete c (item k))
                done;
                match width with
                | 2 -> Vec2_array data
                | 3 -> Vec3_array data
                | 4 -> Vec4_array data
                | _ -> Float_array data))
        | Ty.Array e, _ -> array_init e n (fun k -> concrete c (item k))
        | _ -> List (join_values (Array.init n item)))
    | `Filter, [ l ] -> (
        let count, get = List.hd (lists_of [ l ]) in
        match out with
        | Ty.Array e ->
            let indices = Array.make count 0 and kept = ref 0 in
            for i = 0 to count - 1 do
              if truthy (concrete c (call fv [ get i ])) then begin
                indices.(!kept) <- i;
                incr kept
              end
            done;
            array_init e !kept (fun i -> get indices.(i))
        | _ ->
            let xs = Array.init count get in
            List
              (Array.of_list
                 (List.filter (fun x -> truthy (concrete c (call fv [ x ]))) (Array.to_list xs))))
    | `Sort_by, [ l ] -> (
        let count, get = List.hd (lists_of [ l ]) in
        match out with
        | Ty.Array e ->
            let keys = Array.init count (fun i -> num (concrete c (call fv [ get i ]))) in
            let indices = Array.init count Fun.id in
            Array.stable_sort (fun a b -> Float.compare keys.(a) keys.(b)) indices;
            array_init e count (fun i -> get indices.(i))
        | _ ->
            let xs = Array.init count get in
            let keyed = Array.mapi (fun i x -> (num (concrete c (call fv [ x ])), i, x)) xs in
            let sorted =
              List.stable_sort (fun (a, _, _) (b, _, _) -> compare a b) (Array.to_list keyed)
            in
            List (Array.of_list (List.map (fun (_, _, x) -> x) sorted)))
    | `Reduce, [ init; l ] ->
        let init = ev (sub c "init") env init in
        let count, get = List.hd (lists_of [ l ]) in
        let acc = ref init in
        for i = 0 to count - 1 do
          let value = call fv [ !acc; get i ] in
          acc := widen_like c !acc (if packed then concrete c value else value)
        done;
        !acc
    | `Iterate, init :: n :: until ->
        let init = ev (sub c "init") env init in
        let passes = Value.int_of (concrete c (ev (sub c "n") env n)) in
        let stop =
          match until with
          | [] -> None
          | g :: _ -> (
              match concrete c (ev (sub c "until") env g) with
              | Fn f -> Some f
              | _ -> fail "E_TYPE" "Expected a function.")
        in
        let acc = ref init and pass = ref 0 and finished = ref false in
        while !pass < passes && not !finished do
          let next = widen_like c !acc (call fv [ !acc ]) in
          (match stop with
          | Some g -> finished := truthy (concrete c (call g [ !acc; next ]))
          | None -> ());
          acc := next;
          incr pass
        done;
        !acc
    | _ -> fail "E_ARITY" "A higher-order form got the wrong number of arguments."
  in
  if packed then Fun.protect ~finally:(fun () -> c.st.steps <- saved_steps) work else work ()

and loop c env ~out kind accs clauses skip body zone =
  let packed =
    List.exists (fun (_, (e : W.term)) -> match e.ty with Ty.Array _ -> true | _ -> false) clauses
  in
  let rec geometry_accumulator = function
    | ty when ty = Ty.geometry -> true
    | Ty.Record fields -> List.exists (fun (_, ty) -> geometry_accumulator ty) fields
    | _ -> false
  in
  let cooked_fold = kind = `Fold && geometry_accumulator out in
  let c = if packed && not cooked_fold then { c with data = true } else c in
  let saved_steps = c.st.steps in
  let cz = { c with base = zone; route = [] } in
  let init = match accs with [ (_, e) ] -> Some (ev (sub cz "init") env e) | _ -> None in
  let acc_pat = match accs with [ (p, _) ] -> Some p | _ -> None in
  let acc = ref init in
  let k = ref 0 and outs = ref [] and total = ref None and over_geometry = ref None in
  let clauses = Array.of_list clauses in
  let n = Array.length clauses in
  let width =
    match out with
    | Ty.Array Ty.Vec2 -> 2
    | Ty.Array Ty.Vec3 -> 3
    | Ty.Array Ty.Vec4 -> 4
    | Ty.Array _ -> 1
    | _ -> 0
  in
  let integer = out = Ty.Array Ty.Int in
  let text = out = Ty.Array Ty.Text in
  let integers = ref (if integer then Array.make 32 0 else [||]) in
  let texts = ref (if text then Array.make 32 "" else [||]) in
  let data = ref (if width = 0 || integer || text then [||] else Array.make (32 * width) 0.)
  and used = ref 0 in
  let grow buffer zero =
    if !used = Array.length !buffer then begin
      let capacity = Array.length !buffer in
      let next = min (Sys.max_array_length - capacity) capacity + capacity in
      if next <= !used then fail "E_ARRAY_RANGE" "Array exceeds native storage bounds.";
      let grown = Array.make next zero in
      Array.blit !buffer 0 grown 0 capacity;
      buffer := grown
    end
  in
  let collect v =
    if integer then begin
      grow integers 0;
      Value.int_array_put !integers !used (concrete c v);
      incr used
    end
    else if text then begin
      grow texts "";
      (match concrete c v with
      | Text value -> !texts.(!used) <- value
      | _ -> fail "E_ARRAY_TYPE" "Text arrays contain text values.");
      incr used
    end
    else if width = 0 then outs := v :: !outs
    else begin
      let v = concrete c v in
      if !used = Array.length !data / width then begin
        let next = Array.make (min Sys.max_floatarray_length (2 * Array.length !data)) 0. in
        if Array.length next / width <= !used then
          fail "E_ARRAY_RANGE" "Array exceeds native storage bounds.";
        Array.blit !data 0 next 0 (Array.length !data);
        data := next
      end;
      let at = width * !used in
      (match (width, v) with
      | 1, v -> !data.(at) <- fin "array loop" (num v)
      | 2, Vec2 (x, y) ->
          !data.(at) <- fin "array loop" x;
          !data.(at + 1) <- fin "array loop" y
      | 3, Vec3 (x, y, z) ->
          !data.(at) <- fin "array loop" x;
          !data.(at + 1) <- fin "array loop" y;
          !data.(at + 2) <- fin "array loop" z
      | 4, Vec4 (x, y, z, w) ->
          !data.(at) <- fin "array loop" x;
          !data.(at + 1) <- fin "array loop" y;
          !data.(at + 2) <- fin "array loop" z;
          !data.(at + 3) <- fin "array loop" w
      | _, (Int _ | Float _ | Bool _) ->
          let x = fin "array loop" (num v) in
          for j = 0 to width - 1 do
            !data.(at + j) <- x
          done
      | _ -> fail "E_ARRAY_TYPE" "Packed loop element has the wrong vector width.");
      incr used
    end
  in
  let mk = Some (fun v -> zone @ [ ":" ^ v ]) in
  let add a b = add_values (concrete c a) (concrete c b) in
  let rec go ci env items =
    if ci = n then begin
      if packed && !k mod packed_block_size = 0 then c.st.steps <- saved_steps;
      if (not packed) && !k >= max_iterations then
        failf "E_ITER_BOUND" "%s runs more than 4,096 iterations." (path_text zone);
      let ci' = { cz with iter = c.iter @ [ !k ] } in
      (* record the loop names at this iteration, in clause order *)
      let env = ref env in
      List.iteri
        (fun i item -> env := bind_pat ci' ~mk ~whole:true (fst clauses.(i)) item !env)
        (List.rev items);
      (match (acc_pat, !acc) with
      | Some p, Some a -> env := bind_pat ci' ~mk ~whole:true p a !env
      | _ -> ());
      if List.mem ci'.iter skip then incr k
      else begin
        let v = ev ci' !env body in
        let v = if packed then concrete c v else v in
        (match (kind, !acc) with
        | (`Fold | `Scan), Some a ->
            let a' = widen_like c a v in
            acc := Some a';
            if kind = `Scan then collect a'
        | `Sum, _ -> total := Some (match !total with None -> v | Some t -> add t v)
        | _ -> collect v);
        incr k
      end
    end
    else begin
      let p, e = clauses.(ci) in
      match concrete c (ev (sub cz ("in" ^ string_of_int ci)) env e) with
      | Struct (op, Ty.List _, fs) ->
          if kind <> `For || n <> 1 || skip <> [] then
            failf "E_ZONE"
              "%s: only a for with one clause and no :skip can iterate the elements of geometry."
              (path_text zone);
          over_geometry := Some (geometry_loop c cz env op fs p body zone)
      | v ->
          let count, get =
            match v with
            | List xs -> (Array.length xs, fun i -> xs.(i))
            | Text_array _ | Int_array _ | Float_array _ | Vec2_array _ | Vec3_array _
            | Vec4_array _ ->
                (array_length v, array_get v)
            | _ -> fail "E_TYPE" "Expected a list or packed array."
          in
          for i = 0 to count - 1 do
            let item = get i in
            go (ci + 1) (bind_pat c ~mk:None ~whole:false p item env) (item :: items)
          done
    end
  in
  let work () =
    go 0 env [];
    match !over_geometry with
    | Some v -> v
    | None -> (
        match kind with
        | `Fold -> Option.get !acc
        | `Sum -> ( match !total with None -> Int 0 | Some t -> concrete c t)
        | `For | `Scan -> (
            if integer then Int_array (Array.sub !integers 0 !used)
            else if text then Text_array (Array.sub !texts 0 !used)
            else if width = 0 then List (join_values (Array.of_list (List.rev !outs)))
            else
              let data = Array.sub !data 0 (!used * width) in
              match width with
              | 2 -> Vec2_array data
              | 3 -> Vec3_array data
              | 4 -> Vec4_array data
              | _ -> Float_array data))
  in
  if packed then Fun.protect ~finally:(fun () -> c.st.steps <- saved_steps) work else work ()

(* W8: [(for [p (sop/point_list g)] body)].  The count is known only when [g] cooks, so
   the body is evaluated once, as a template, with the element unknown: a point is a residual
   read from [st.elems] when forced, a piece a plan node [zone/element].  The template's nodes
   (ids [lo] .. [hi - 1]) are not part of the graph; the plan node [zone/points] or
   [zone/pieces] (a [Deferred (Geometry, id)], the merge of the elements) tells lowering how to cook them. *)
and geometry_loop c cz env op fs p body zone =
  if c.st.time <> None then
    failf "E_LIVE_GEOMETRY" "%s iterates geometry while evaluating a live value." (path_text zone);
  let src =
    match List.assoc_opt "geometry" fs with
    | Some (Deferred (Ty.Named "geometry", id)) -> id
    | _ -> failf "E_TYPE" "%s: %s needs geometry." (path_text zone) op
  in
  let key = List.assoc_opt "key" fs in
  let points = op = "sop/point_list" in
  let ci = { cz with iter = c.iter @ [ 0 ]; geometry = true } in
  let ekey = element_key zone in
  let lo = c.st.nnodes in
  let elem, elem_arg =
    if points then begin
      c.st.rids <- c.st.rids + 1;
      let term =
        { W.path = None; ty = Ty.Vec3; node = W.Ref_binding (ekey, []); form = body.W.form }
      in
      ( Residual
          {
            rid = c.st.rids;
            rterm = term;
            renv = Smap.empty;
            rc = ci;
            previous = false;
            live = None;
            dep_frame = 0;
            dep_state = 0;
          },
        Text ekey )
    end
    else
      match mk_node (sub ci "element") "zone/element" [] with
      | Deferred (Ty.Named "geometry", id) as g -> (g, Int id)
      | _ -> assert false
  in
  let mk = Some (fun v -> zone @ [ ":" ^ v ]) in
  let env = bind_pat ci ~mk ~whole:true p elem env in
  let v = ev ci env body in
  let root =
    match v with
    | Deferred (Ty.Named "geometry", id) -> id
    | Residual _ ->
        failf "E_ZONE"
          "%s: what a loop over geometry builds cannot depend on the element; only arguments can."
          (path_text zone)
    | _ -> failf "E_TYPE" "%s: the body of a loop over geometry returns geometry." (path_text zone)
  in
  let hi = c.st.nnodes in
  mk_node c
    (if points then "zone/points" else "zone/pieces")
    (("geometry", Deferred (Ty.geometry, src))
     :: (match key with Some k -> [ ("key", k) ] | None -> [])
    @ [ ("body", Int root); ("lo", Int lo); ("hi", Int hi); ("element", elem_arg) ])

and graph_value ?(rec_ = true) c name over =
  let st = c.st in
  let g =
    match Hashtbl.find_opt st.graphs name with
    | Some g -> g
    | None -> failf "E_UNKNOWN_GRAPH" "Unknown graph reference: %s." name
  in
  let over =
    List.map
      (fun (n, v) ->
        match List.find_opt (fun (m, _, _) -> m = n) g.inputs with
        | Some (_, ty, _) ->
            let rec fits ty v =
              match (ty, v) with
              | Ty.Fn (Some signature), Fn (Closure cl) ->
                  List.compare_lengths signature.params cl.params = 0
                  && List.for_all2
                       (fun expected (_, actual) ->
                         Option.fold ~none:false
                           ~some:(fun actual -> Ty.fits expected actual)
                           actual)
                       signature.params cl.params
                  && Ty.fits cl.body.ty signature.result
              | Ty.List element, List values -> Array.for_all (fits element) values
              | _ -> Ty.fits (Value.ty_of v) ty
            in
            if not (fits ty v) then
              failf "E_INPUT_TYPE" "Graph input %s of %s needs %s; got %s." n name (Ty.to_string ty)
                (Ty.to_string (Value.ty_of v));
            (n, coerce_to ty v)
        | None -> failf "E_INPUT_UNKNOWN" "Graph %s has no input %s." name n)
      over
  in
  if List.length over <> List.length (List.sort_uniq String.compare (List.map fst over)) then
    failf "E_INPUT_DUPLICATE" "Graph %s has duplicate input overrides." name;
  let key =
    name ^ "|"
    ^ String.concat "," (List.sort compare (List.map (fun (n, v) -> n ^ "=" ^ key_of v) over))
  in
  let run inst =
    if c.depth > max_depth then fail "E_DEPTH" "Call depth exceeds 64.";
    let c0 =
      {
        st;
        inst;
        prefix = [];
        base = [ name ];
        route = [];
        iter = [];
        depth = c.depth + 1;
        rec_;
        data = c.data;
        geometry = c.geometry;
        host = g.context = Context.host;
        kernel_requests = c.kernel_requests;
        card = c.card;
      }
    in
    let env, ins =
      List.fold_left
        (fun (env, ins) (n, ty, default) ->
          let v =
            match List.assoc_opt n over with
            | Some v -> v
            | None -> (
                match default with
                | Some t -> coerce_to ty (ev (sub c0 n) env t)
                | None -> failf "E_INPUT_DEFAULT" "Graph input %s of %s needs a default." n name)
          in
          note c0 [ name; ":" ^ n ] v;
          (Smap.add n v env, (n, v) :: ins))
        (Smap.empty, []) g.inputs
    in
    (ev c0 env g.body, List.rev ins)
  in
  match st.time with
  | Some _ ->
      let inst =
        match Hashtbl.find_opt st.cache key with Some cell -> cell.cinst | None -> c.inst
      in
      fst (run inst)
  | None -> (
      match Hashtbl.find_opt st.cache key with
      | Some cell -> cell.cresult
      | None ->
          let cid = List.length st.cells in
          let cell =
            { cinst = cid; cgraph = name; cdefault = over = []; cinputs = []; cresult = No_geo }
          in
          st.cells <- cell :: st.cells;
          Hashtbl.replace st.cache key cell;
          let v, ins = run cid in
          cell.cinputs <- ins;
          cell.cresult <- v;
          v)

(* ---- entry points ---- *)

let diagnostic code msg span = Diagnostic.error ?span ~code msg
let inline_nodes = Atomic.make true

let inline_parameters =
  Atomic.make (fun _ (d : W.node_definition) ->
      (d, lazy (Digest.string (Marshal.to_string d [])), fun vals -> Some vals))

(* ponytail: a node with callable slots still lowers as its own nested workspace *)
let inline_simple (d : W.node_definition) =
  List.for_all
    (fun (s : Catalog.slot) -> match s.ty with Some (Ty.Fn _) -> false | _ -> true)
    d.slots

let new_state ?(budget = max_steps) ~record ws =
  let graphs = Hashtbl.create 8 and defs = Hashtbl.create 8 in
  List.iter (fun (g : W.graph) -> Hashtbl.replace graphs g.name g) ws.W.graphs;
  List.iter (fun (g : W.graph) -> Hashtbl.replace defs g.name g) ws.W.defs;
  {
    time = None;
    resolve = None;
    source = lazy (Digest.string (fst (Lisp.print ws.W.source)));
    state = None;
    states = [];
    nfns = 0;
    fn_calls = Hashtbl.create 1;
    steps = 0;
    budget;
    nodes = [];
    authored = [];
    nnodes = 0;
    cells = [];
    cache = Hashtbl.create 8;
    memo = Hashtbl.create 1;
    rids = 0;
    record;
    elems = Smap.empty;
    recs = Hashtbl.create 64;
    graphs;
    defs;
    kind_fns = ws.W.kind_fns;
    node_defs =
      (let t = Hashtbl.create 8 in
       if Atomic.get inline_nodes then
         List.iter
           (fun (d : W.node_definition) ->
             if inline_simple d then
               Hashtbl.replace t d.kind.qualified (lazy (Atomic.get inline_parameters ws d)))
           ws.W.nodes;
       t);
    ops = ws.ops;
    find_op = Op.finder ws.ops;
    terms = None;
    inlined = [];
    watching = [];
  }

let live_state (st : st) ~state ?(elems = Smap.empty) (l : live) =
  {
    st with
    time = Some l;
    state = Some state;
    steps = 0;
    memo = Hashtbl.create 16;
    fn_calls = Hashtbl.create 16;
    record = false;
    elems;
  }

let root st =
  {
    st;
    inst = -1;
    prefix = [];
    base = [];
    route = [];
    iter = [];
    depth = 0;
    rec_ = true;
    data = false;
    geometry = false;
    host = false;
    kernel_requests = false;
    card = None;
  }

let protect f =
  try Ok (f ()) with
  | Diagnostic.Error d -> Error d
  | Needs_t ->
      Error (diagnostic "E_EVAL" "A live value was needed while evaluating statically." None)
  | Stack_overflow -> Error (diagnostic "E_DEPTH" "Evaluation is nested too deeply." None)
  | Out_of_memory -> Error (diagnostic "E_ARRAY_MEMORY" "Frame data exceeds available memory." None)

let static_impl ?budget ?(record = false) ?(inputs = []) ws =
  Phase_timer.measure "evaluate" (fun () ->
      protect (fun () ->
          if List.length inputs <> List.length (List.sort_uniq String.compare (List.map fst inputs))
          then fail "E_INPUT_DUPLICATE" "A graph receives more than one input override set.";
          List.iter
            (fun (name, _) ->
              if not (List.exists (fun (g : W.graph) -> g.name = name) ws.W.graphs) then
                failf "E_UNKNOWN_GRAPH" "Unknown graph input target: %s." name)
            inputs;
          List.iter (fun (_, ins) -> List.iter (fun (_, value) -> Value.validate value) ins) inputs;
          let st = new_state ?budget ~record ws in
          let c = root st in
          let results =
            List.map
              (fun (g : W.graph) ->
                let over = Option.value (List.assoc_opt g.name inputs) ~default:[] in
                (g.name, graph_value c g.name over))
              ws.W.graphs
          in
          let instances =
            st.cells |> List.rev
            |> List.map (fun cell ->
                {
                  graph = cell.cgraph;
                  default = cell.cdefault;
                  inputs = cell.cinputs;
                  result = cell.cresult;
                })
            |> Array.of_list
          in
          let records =
            Hashtbl.fold (fun p (_, l) acc -> (p, List.rev l) :: acc) st.recs []
            |> List.sort (fun (a, _) (b, _) -> compare a b)
          in
          {
            plan = { instances; nodes = Array.of_list (List.rev st.nodes) };
            results;
            records;
            authored = Array.of_list (List.rev st.authored);
            states = List.rev st.states;
          }))

let static ?record ?inputs ws = Residual_effect.handle (fun () -> static_impl ?record ?inputs ws)

(* Residuals are shared and immutable: each remembers its answer, or a let-bound array read by many later
   bindings is walked once per reader and the walk is exponential in the depth of the sharing, and the
   executor asks the same question of every region pair while fusing (Carve's plans). *)
let rec dependent frame = function
  | Residual r ->
      let known = if frame then r.dep_frame else r.dep_state in
      if known <> 0 then known = 2
      else begin
        let answer = term_dependent frame r.rc.st r.renv r.rterm in
        if frame then r.dep_frame <- (if answer then 2 else 1)
        else r.dep_state <- (if answer then 2 else 1);
        answer
      end
  | List xs -> Array.exists (dependent frame) xs
  | Record fs | Struct (_, _, fs) | Source (_, _, fs) ->
      List.exists (fun (_, v) -> dependent frame v) fs
  | Fn (Closure cl) -> term_dependent frame cl.at.st cl.env cl.body
  | _ -> false

and term_dependent frame st env (term : W.term) =
  let term_dep = term_dependent frame st env in
  let any = List.exists term_dep in
  let fields fs = any (List.map snd fs) in
  let definition name =
    match Hashtbl.find_opt st.defs name with
    | Some g -> term_dep g.body
    | None -> Option.fold ~none:false ~some:(dependent frame) (Smap.find_opt name env)
  in
  match term.node with
  | W.Time -> frame
  | W.State _ -> true
  | W.Ref_binding (name, _) ->
      Option.fold ~none:false ~some:(dependent frame) (Smap.find_opt name env)
  | W.Op { op; args; _ } -> (frame && (Option.get (find_op st op)).live) || fields args
  | W.Call { args; _ } | W.Record args -> fields args
  | W.Call_fn { fn; args; body } ->
      definition fn || Option.fold ~none:false ~some:term_dep body || any args
  | W.Fn_ref name -> definition name
  | W.Graph_ref { graph; inputs } -> (
      fields inputs
      ||
      match Hashtbl.find_opt st.graphs graph with
      | Some g ->
          term_dep g.body
          || List.exists (fun (_, _, d) -> Option.fold ~none:false ~some:term_dep d) g.inputs
      | None -> false)
  | W.Vec ts | W.List_lit ts | W.Str ts | W.List_op (_, ts) | W.Hof (_, ts) -> any ts
  | W.Let (bindings, body) -> any (List.map snd bindings) || term_dep body
  | W.Loop { accs; clauses; body; _ } -> any (List.map snd (accs @ clauses)) || term_dep body
  | W.With_error (a, b) -> any [ a; b ]
  | W.Error_rows { rows = a; mapper = b; _ } -> any [ a; b ]
  | W.If (c, a, b) -> any [ c; a; b ]
  | W.Cond (arms, d) -> any (d :: List.concat_map (fun (a, b) -> [ a; b ]) arms)
  | W.Case (s, arms, d) -> any (s :: d :: List.map snd arms)
  | W.Fn { capture = Some original; _ } -> term_dep original
  | W.Fn { body; _ } | W.Expanded { body; _ } | W.Get (body, _) -> term_dep body
  | W.Bypass call -> Option.fold ~none:false ~some:term_dep (W.bypass_input call)
  | W.Assoc (r, args) -> term_dep r || fields args
  | W.Lit _ | W.Text _ | W.Nil -> false

let frame_dependent = dependent true
let state_dependent = dependent false

(* one live state per call, made from the first residual met *)
let with_live ?state ?elems ?resolve ?(kernel_requests = false) (l : live)
    (f : (residual -> ctx) -> 'a) : ('a, Diagnostic.t) result =
  let state = match state with Some state -> state | None -> create_state () in
  let saved = fork_state state in
  let elems = Option.map Smap.of_list elems in
  let live = ref None in
  let ctx_of r =
    let st =
      match !live with
      | Some s -> s
      | None ->
          let s = { (live_state r.rc.st ~state ?elems l) with resolve } in
          live := Some s;
          s
    in
    { r.rc with st; kernel_requests }
  in
  match Input.validate l with
  | Error message -> Error (Diagnostic.error ~code:"E_FRAME" message)
  | Ok () ->
      let result = protect (fun () -> f ctx_of) in
      (match result with Ok _ -> () | Error _ -> restore_state state saved);
      result

let residual_eval r ~live =
  with_live live (fun ctx_of ->
      let c = ctx_of r in
      force_res c r)

let bind_live ?state ?elems ?resolve r ~live =
  with_live ?state ?elems ?resolve live (fun ctx_of ->
      let c = ctx_of r in
      r.live <- Some c.st)

let rec static_source_fields = function
  | Source (_, _, fields) | Struct (_, _, fields) | Record fields ->
      List.for_all (fun (_, value) -> static_source_fields value) fields
  | List values -> Array.for_all static_source_fields values
  | value -> not (is_live value)

let rec force_with ?state ?elems ?resolve ?on_walk v ~live =
  if not (is_live v) then Ok v
  else
    let general () =
      with_live ?state ?elems ?resolve live (fun ctx_of ->
          let rec go v =
            match v with
            | Residual r ->
                Option.iter (fun on_walk -> on_walk ()) on_walk;
                let c = ctx_of r in
                go (force_res c r)
            | Source _ | Struct (_, Ty.Array _, _) -> (
                match resolve with
                | Some resolve -> go (resolve_source resolve v)
                | None -> fail "E_DATA_SOURCE" "This data source needs its host's cooked data.")
            | List xs -> List (Array.map go xs)
            | Record fs -> Record (List.map (fun (n, x) -> (n, go x)) fs)
            | Struct (n, ty, fs) -> Struct (n, ty, List.map (fun (k, x) -> (k, go x)) fs)
            | v -> v
          in
          go v)
    in
    match (v, state, elems, resolve) with
    | (Source (_, _, fields) | Struct (_, Ty.Array _, fields)), None, None, Some resolve
      when List.for_all (fun (_, value) -> static_source_fields value) fields -> (
        match Input.validate live with
        | Error message -> Error (Diagnostic.error ~code:"E_FRAME" message)
        | Ok () ->
            Result.bind
              (protect (fun () -> resolve_source resolve v))
              (fun value ->
                if is_live value then force_with ?on_walk ~resolve value ~live else Ok value))
    | _ -> general ()

let force ?state ?elems v ~live = force_with ?state ?elems v ~live

let run ?record ?inputs ?state ?live ~time ws =
  let state = match state with Some state -> state | None -> create_state () in
  match static ?record ?inputs ws with
  | Error _ as e -> e
  | Ok s ->
      let live = Option.value ~default:(Input.at_time time) live in
      let f v =
        match force ~state v ~live with Ok v -> v | Error d -> raise (Diagnostic.Error d)
      in
      transaction state (fun () ->
          try
            let states = List.map f s.states in
            Ok
              {
                s with
                states;
                plan =
                  {
                    nodes =
                      Array.map
                        (fun n -> { n with args = List.map (fun (k, v) -> (k, f v)) n.args })
                        s.plan.nodes;
                    instances =
                      Array.map
                        (fun i ->
                          {
                            i with
                            inputs = List.map (fun (k, v) -> (k, f v)) i.inputs;
                            result = f i.result;
                          })
                        s.plan.instances;
                  };
                results = List.map (fun (n, v) -> (n, f v)) s.results;
                records =
                  List.map (fun (p, l) -> (p, List.map (fun (it, v) -> (it, f v)) l)) s.records;
              }
          with Diagnostic.Error d -> Error d)

let show v = show_with Fun.id v

module Host = struct
  let static_with_requests = static_impl
  let bind_live = bind_live

  let map_function ?site ?path ~(signature : Ty.fn_signature) fn arrays =
    protect (fun () ->
        match fn with
        | Named _ -> fail "E_KERNEL_FORM" "A bulk function needs an instantiated local body."
        | Closure cl ->
            if List.compare_lengths cl.params arrays <> 0 then
              fail "E_ARITY" "Bulk function inputs differ from its parameter count.";
            if
              List.compare_lengths signature.params arrays <> 0
              || not
                   (List.for_all2
                      (fun ty value -> Value.ty_of value = Ty.Array ty)
                      signature.params arrays)
            then
              fail "E_ARRAY_TYPE" "Bulk function columns must match its declared parameter types.";
            List.iter
              (function
                | Text_array _ | Int_array _ | Float_array _ | Vec2_array _ | Vec3_array _
                | Vec4_array _ ->
                    ()
                | _ -> fail "E_ARRAY_TYPE" "Bulk function inputs must be packed arrays.")
              arrays;
            List.iter Value.validate arrays;
            let counts = List.map array_length arrays in
            if List.exists (( <> ) (Option.value ~default:0 (List.nth_opt counts 0))) counts then
              fail "E_ARRAY_RANGE" "Bulk function inputs must have equal counts.";
            if not (Ty.fits cl.body.ty signature.result) then
              fail "E_KERNEL_FORM" "Bulk function body differs from its declared result type.";
            let result =
              match signature.result with
              | (Ty.Int | Float | Vec2 | Vec3 | Vec4 | Text) as ty -> ty
              (* a record of numeric leaves: one packed output each (a raster stage's varyings) *)
              | Record _ as ty when Ty.numeric_record ty -> ty
              | _ ->
                  fail "E_KERNEL_FORM"
                    "Bulk functions must return int, float, vec2, vec3, vec4, text or a record of \
                     numbers."
            in
            let binding name ty =
              W.{ path = None; ty; node = Ref_binding (name, []); form = cl.body.form }
            in
            let inputs =
              List.mapi (fun i value -> ("$kernel-input:" ^ string_of_int i, value)) arrays
            in
            let function_name = "$kernel-function" in
            let term =
              W.
                {
                  path;
                  ty = Ty.Array result;
                  form = cl.body.form;
                  node =
                    Hof
                      ( `Map,
                        binding function_name (Ty.Fn None)
                        :: List.map (fun (name, value) -> binding name (Value.ty_of value)) inputs
                      );
                }
            in
            let at =
              match site with
              | None -> cl.at
              | Some (inst, base, iter) -> { cl.at with inst; base; iter; prefix = []; route = [] }
            in
            Residual
              {
                rid = Atomic.fetch_and_add bulk_ids (-1);
                rterm = term;
                renv =
                  List.fold_left
                    (fun env (name, value) -> Smap.add name value env)
                    (Smap.singleton function_name (Fn fn))
                    inputs;
                rc = { at with data = true; rec_ = false };
                previous = false;
                live = None;
                dep_frame = 0;
                dep_state = 0;
              })

  let free_name_walks () = Atomic.get free_walks

  let map_function_bindings f = function
    | Closure cl -> Closure { cl with env = Smap.map f cl.env }
    | Named _ as fn -> fn

  let remap_function_geometry ~sources fn =
    (* Residuals are shared: each is rewritten once, or the copy is exponential in the depth of the sharing. *)
    let module Seen = Hashtbl.Make (struct
      type t = residual

      let equal = ( == )
      let hash (r : residual) = Hashtbl.hash r.rid
    end) in
    let seen = Seen.create 64 in
    let rec value = function
      | Deferred (ty, id) when Ty.is_geometry ty ->
          Deferred (ty, Option.value ~default:id (List.assoc_opt id sources))
      | Fn fn -> Fn (function_value fn)
      | Residual r -> (
          match Seen.find_opt seen r with
          | Some copy -> Residual copy
          | None ->
              let copy =
                {
                  r with
                  rid = Atomic.fetch_and_add bulk_ids (-1);
                  renv = Smap.map value r.renv;
                  live = None;
                  dep_frame = 0;
                  dep_state = 0;
                }
              in
              Seen.add seen r copy;
              Residual copy)
      | List values -> List (Array.map value values)
      | Record fields -> Record (List.map (fun (name, v) -> (name, value v)) fields)
      | Struct (name, ty, fields) ->
          Struct (name, ty, List.map (fun (name, v) -> (name, value v)) fields)
      | Source (name, ty, fields) ->
          Source (name, ty, List.map (fun (name, v) -> (name, value v)) fields)
      | v -> v
    and function_value = function
      | Closure cl -> Closure { cl with env = Smap.map value cl.env }
      | Named _ as fn -> fn
    in
    if sources = [] then fn else function_value fn

  let state_values state =
    List.map snd (State_map.bindings state.before) @ List.map snd (State_map.bindings state.next)

  let function_scope = function
    | Closure cl -> Some (cl.at.prefix @ cl.zone, cl.at.iter)
    | Named _ -> None

  let force_reference ?state ?elems ?resolve v ~live = force_with ?state ?elems ?resolve v ~live

  let force_reference_observed ?state ?elems ?resolve ~on_walk v ~live =
    force_with ?state ?elems ?resolve ~on_walk v ~live

  let map_probe ?state ?resolve ?(offset = 0) r ~live =
    with_live ?state ?resolve live (fun ctx_of ->
        let c = { (ctx_of r) with rec_ = false } in
        match r.rterm.node with
        | W.Hof (`Map, f :: sources) ->
            let fn =
              match concrete c (ev c r.renv f) with
              | Fn fn -> fn
              | _ -> fail "E_TYPE" "Expected a function."
            in
            let arrays = List.map (fun source -> concrete c (ev c r.renv source)) sources in
            let size = function List xs -> Array.length xs | v -> array_length v in
            let count = List.fold_left (fun n v -> min n (size v)) max_int arrays in
            let count = if arrays = [] then 0 else count in
            let get v k = match v with List xs -> xs.(k) | v -> array_get v k in
            let at k =
              protect (fun () ->
                  if k < 0 || k >= count then fail "E_LIST_RANGE" "Probe index is outside this map.";
                  let st =
                    {
                      c.st with
                      record = true;
                      recs = Hashtbl.create 16;
                      fn_calls = Hashtbl.create 1;
                      memo = Hashtbl.create 16;
                      steps = 0;
                    }
                  in
                  let fid = match fn with Closure cl -> cl.fid | Named n -> n.fid in
                  Hashtbl.add st.fn_calls fid (offset + k);
                  ignore
                    (concrete { c with st; rec_ = true }
                       (call_fn { c with st; rec_ = true } fn (List.map (fun v -> get v k) arrays)));
                  Hashtbl.fold
                    (fun path (_, values) all -> (path, List.rev values) :: all)
                    st.recs [])
            in
            (count, at)
        | _ -> fail "E_TYPE" "A map probe needs a checked map.")
end

let map_kernel = Host.map_function
let bind_captures = Host.map_function_bindings

module Residual = struct
  type _ Effect.t +=
    | Observe_kernel = Residual_effect.Observe_kernel
    | Kernel_value = Residual_effect.Kernel_value
    | Bulk_static = Residual_effect.Bulk_static
    | Bulk_approx = Residual_effect.Bulk_approx

  let handle = Residual_effect.handle
  let free_names term = Names.elements (free_names term)

  let with_bindings r ~term bindings =
    let env = List.fold_left (fun env (name, value) -> Smap.add name value env) r.renv bindings in
    {
      r with
      rid = Atomic.fetch_and_add bulk_ids (-1);
      rterm = term;
      renv = capture term env;
      previous = false;
      live = None;
      dep_frame = 0;
      dep_state = 0;
    }

  let bind_live = bind_live
  let function_bindings = function Closure cl -> Smap.bindings cl.env | Named _ -> []

  let compiled_function r ~instance ~prefix ~zone ~params ~body ~bindings =
    Closure
      {
        params;
        body;
        zone;
        env = Smap.of_seq (List.to_seq bindings);
        calls = ref 0;
        fid = Atomic.fetch_and_add bulk_ids (-1);
        at = { r.rc with inst = instance; prefix };
      }

  let function_id = function Closure cl -> cl.fid | Named n -> n.fid
  let function_body = function Closure cl -> Some (cl.params, cl.body) | Named _ -> None

  let function_origin = function
    | Closure cl -> Some (cl.at.inst, cl.at.prefix, cl.zone)
    | Named _ -> None

  let function_name = function Named n -> Some n.name | Closure _ -> None
  let residual_definition r name = Hashtbl.find_opt r.rc.st.defs name
  let residual_graph r name = Hashtbl.find_opt r.rc.st.graphs name

  let graph_instance r name =
    Option.map (fun cell -> cell.cinst) (Hashtbl.find_opt r.rc.st.cache (name ^ "|"))

  type residual_view = {
    term : W.term;
    bindings : (string * value) list;
    site : W.path;
    owner : W.path;
    iter : int list;
    instance : int;
    prefix : W.path;
    previous : bool;
    host : bool;
    geometry : bool;
  }

  let residual_view r =
    {
      term = r.rterm;
      bindings = Smap.bindings r.renv;
      site = site r.rc;
      owner = r.rc.base;
      iter = r.rc.iter;
      instance = r.rc.inst;
      prefix = r.rc.prefix;
      previous = r.previous;
      host = r.rc.host;
      geometry = r.rc.geometry;
    }

  let residual_id r = r.rid
  let live_ctx r = match r.live with Some st -> { r.rc with st } | None -> r.rc

  let checkpoint r =
    let c = live_ctx r in
    let saved = Option.map fork_state c.st.state in
    fun () -> Option.iter (fun saved -> restore_state (Option.get c.st.state) saved) saved

  let rerun ?on_walk r =
    Option.iter (fun on_walk -> on_walk ()) on_walk;
    let c = live_ctx r in
    let c = match r.rterm.path with Some p -> { c with base = p; route = [] } | None -> c in
    protect (fun () ->
        try
          if r.previous then
            match r.rterm.node with
            | W.State { init; zone; _ } -> state_previous c r.renv zone init
            | _ -> assert false
          else concrete c (ev_raw c r.renv r.rterm)
        with Diagnostic.Error d when d.span = None ->
          raise_diagnostic { d with span = span_of r.rterm })

  let state_ctx r ~instance ~path ~iter ~host =
    { (live_ctx r) with inst = instance; prefix = path; iter; host }

  let state_read r ~instance ~path ~iter ~host ~init =
    let c = state_ctx r ~instance ~path ~iter ~host in
    let state, frame = state_plane c in
    begin_frame state (Lazy.force c.st.source) frame;
    match State_map.find_opt (state_key c []) state.before with
    | Some v -> state_data c v
    | None -> state_data c (init ())

  let state_peek r ~instance ~path ~iter ~host =
    let c = state_ctx r ~instance ~path ~iter ~host in
    State_map.find_opt (state_key c []) (fst (state_plane c)).next

  let state_write r ~instance ~path ~iter ~host v =
    let c = state_ctx r ~instance ~path ~iter ~host in
    let v = state_data c v in
    let state, _ = state_plane c in
    state.next <- State_map.add (state_key c []) v state.next;
    state.epoch <- fresh_state_epoch ();
    v

  let residual_ops r = r.rc.st.ops

  let eval_term ?state ?elems ?resolve ?on_walk ?kernel r term ~live =
    Option.iter (fun on_walk -> on_walk ()) on_walk;
    Residual_effect.handle_kernel ?kernel (fun () ->
        with_live ?state ?elems ?resolve ~kernel_requests:(Option.is_some kernel) live
          (fun ctx_of ->
            let c = ctx_of r in
            concrete c (ev c r.renv term)))
end
