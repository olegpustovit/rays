module Operators = Operators
module Packed = Packed
module Core = Core
module Core_region = Core_region
module Core_fuse = Core_fuse
module Core_lower = Core_lower
module Core_exec = Core_exec
module Core_draw = Core_draw
module E = Flow.Eval
module W = Flow.Workspace
module V = Flow.Value
module Ty = Flow.Ty

module Gpu = struct
  type value={identity:int;count:int;width:int;stamp:int64;gpu_seconds:float option}
  type kernel={run:Packed.Private.inputs -> (value,Flow.Diagnostic.t)result;
    readback:value -> (E.value,Flow.Diagnostic.t)result}
  type backend={cost:Packed.t -> count:int -> float option;
    prepare:Packed.t -> (kernel,Flow.Diagnostic.t)result}
  type policy=Measured | Qualification
  let current : backend option Domain.DLS.key=Domain.DLS.new_key(fun()->None)
  let current_policy : policy Domain.DLS.key=Domain.DLS.new_key(fun()->Measured)
  let policy()=Domain.DLS.get current_policy
  let with_backend ?(policy=Measured) backend run = let previous=Domain.DLS.get current
    and previous_policy=Domain.DLS.get current_policy in
    Domain.DLS.set current(Some backend);
    Domain.DLS.set current_policy policy;
    Fun.protect ~finally:(fun()->Domain.DLS.set current previous;
      Domain.DLS.set current_policy previous_policy)run
  (* The initial domain owns the device. A host that pumps it publishes the backend as a service; the
     other domains (the cook workers) reach it through a mailbox the initial domain serves, so a worker
     waits for the device, never the interface for a worker. *)
  type job={run:unit->unit;mutable finished:bool}
  let lock=Mutex.create() and served=Condition.create()
  let jobs:job Queue.t=Queue.create()
  let service:backend option Atomic.t=Atomic.make None
  let proxied:(backend*backend)option Atomic.t=Atomic.make None
  let set_service backend=Atomic.set service backend
  let pump()=
    if not(Domain.is_main_domain())then invalid_arg "Flow_ir.Gpu.pump: initial domain required";
    let rec drain()=
      Mutex.lock lock;let job=Queue.take_opt jobs in Mutex.unlock lock;
      match job with None->()|Some job->
        (try job.run()with _->());
        Mutex.lock lock;job.finished<-true;Condition.broadcast served;Mutex.unlock lock;drain()in
    drain()
  let remote f=
    if Domain.is_main_domain()then Some(f())else begin
      let result=ref None in
      let job={run=(fun()->result:=Some(f()));finished=false}in
      Mutex.lock lock;Queue.add job jobs;
      while not job.finished do Condition.wait served lock done;
      Mutex.unlock lock;!result end
  let unavailable=Flow.Diagnostic.error ~code:"E_GPU" "The GPU service did not run the request."
  let proxy real=match Atomic.get proxied with
    |Some(source,proxy)when source==real->proxy|_->
    let proxy={cost=(fun packed ~count->Option.join(remote(fun()->real.cost packed ~count)));
      prepare=(fun packed->match remote(fun()->real.prepare packed)with
        |Some(Ok kernel)->Ok{run=(fun inputs->match remote(fun()->kernel.run inputs)with Some r->r|None->Error unavailable);
          readback=(fun value->match remote(fun()->kernel.readback value)with Some r->r|None->Error unavailable)}
        |Some(Error d)->Error d|None->Error unavailable)}in
    Atomic.set proxied(Some(real,proxy));proxy
  (* The backend a worker or the initial domain may use: its own scope, else the published service. *)
  let available()=match Domain.DLS.get current with
    |Some backend->Some backend
    |None->Option.map(fun real->if Domain.is_main_domain()then real else proxy real)(Atomic.get service)
end

(* Set once the executor can run a region on the device: the host side of [bulk_approx]. *)
let gpu_bulk : (approx:W.Paths.t -> E.residual -> E.value option) ref = ref (fun ~approx:_ _ -> None)

(* A static packed map of more than 4,096 elements runs as the kernel it would be at frame rate
   instead of through the walker for every element (about 22 microseconds and 79 KB each). The
   evaluator checks the result against the elements it walked and walks the rest itself when the
   kernel refuses, fails or reads anything live. *)
let static_map residual =
  let value = E.Residual residual in
  if E.frame_dependent value || E.state_dependent value then None else
  match Packed.compile residual (E.Private.residual_view residual).term with
  | None -> None
  | Some packed ->
      (match Packed.force packed ~live:(Frame_input.at_time 0.) with
       | Ok value -> Some value
       | Error _ | exception (V.Fail _ | V.Diagnostic_fail _ | Invalid_argument _ | Not_found | Failure _) -> None)

let static ?record ?inputs workspace =
  E.Private.static_with_kernels ?record ?inputs ~bulk:static_map workspace

let qualify_workspace ?record ?inputs ?observe (workspace : W.t) =
  let roots = Hashtbl.create 32 and index = Hashtbl.create 32 and outcomes = Hashtbl.create 32
  and callable_owners = Hashtbl.create 16 in
  let callable owner (body : W.term) =
    let entries = Option.value ~default:[] (Hashtbl.find_opt callable_owners body.form.id) in
    if not (List.exists (fun (form, path) -> form == body.form && path = owner) entries) then
      Hashtbl.replace callable_owners body.form.id ((body.form, owner) :: entries) in
  List.iter (fun (form, path) ->
    let entries = Option.value ~default:[] (Hashtbl.find_opt roots form.Flow.Syntax.id) in
    Hashtbl.replace roots form.id ((form, path) :: entries)) workspace.packed_roots;
  let add owner (term : W.term) =
    let producers = Option.value ~default:[] (Hashtbl.find_opt roots term.form.id)
      |> List.fold_left (fun paths (form, path) ->
          if form == term.form then W.Paths.add path paths else paths) W.Paths.empty in
    let producers = match term.path with
      | Some path when W.Paths.mem path producers -> W.Paths.singleton path
      | _ when W.Paths.mem owner producers -> W.Paths.singleton owner
      | _ -> producers in
    let entries = Option.value ~default:[] (Hashtbl.find_opt index term.form.id) in
    let same (form, base, _) = form == term.form && base = owner in
    let old = List.find_opt same entries |> Option.fold ~none:W.Paths.empty ~some:(fun (_,_,paths) -> paths) in
    Hashtbl.replace index term.form.id
      ((term.form, owner, W.Paths.union old producers) :: List.filter (fun e -> not (same e)) entries) in
  let rec walk collect owner (term : W.term) =
    let owner = Option.value term.path ~default:owner in
    let visit = walk collect in
    let all = List.iter (visit owner) and fields fs = List.iter (fun (_, t) -> visit owner t) fs in
    (if not collect then match term.node with
     | W.Hof ((`Map | `Reduce), _) | W.Loop _ | W.Op {op = ("array/sum" | "image/map"); _} -> add owner term
     | _ -> ());
    match term.node with
    | W.Vec ts | List_lit ts | Str ts | List_op (_,ts) | Hof (_,ts) -> all ts
    | Call {args;_} | Op {args;_} | Record args -> fields args
    | Graph_ref {inputs;_} -> fields inputs
    | Let (bindings, body) -> fields bindings; visit owner body
    | Loop {zone;accs;clauses;body;_} ->
        List.iter (fun (_,t) -> visit zone t) (accs @ clauses); visit zone body
    | Fn {zone;body;capture;_} ->
        if collect then callable zone body;
        Option.iter (visit owner) capture; visit zone body
    | Call_fn {args;body;_} -> all args; Option.iter (fun (body : W.term) ->
        let owners = Option.value ~default:[] (Hashtbl.find_opt callable_owners body.form.id)
          |> List.filter_map (fun (form, owner) -> if form == body.form then Some owner else None)
          |> List.sort_uniq compare in
        visit (match owners with [owner] -> owner | _ -> owner) body) body
    | State {init;step;zone;_} -> visit owner init; visit zone step
    | With_error(a,b) -> all[a;b]
    | Error_rows{rows=a;mapper=b;_} -> all[a;b]
    | If (c,a,b) -> all [c;a;b]
    | Cond (arms,d) -> List.iter (fun (a,b) -> all [a;b]) arms; visit owner d
    | Case (s,arms,d) -> visit owner s; fields arms; visit owner d
    | Assoc (r,updates) -> visit owner r; fields updates
    | Get (t,_) | Expanded {body=t;_} -> visit owner t
    | Bypass call -> Option.iter (visit owner) (W.bypass_input call)
    | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> () in
  List.iter (fun (graph : W.graph) -> callable ["def:" ^ graph.name] graph.body) workspace.defs;
  List.iter (fun collect -> List.iter (fun (graph : W.graph) ->
    let owner = if List.exists (fun (g : W.graph) -> g == graph) workspace.defs
      then ["def:" ^ graph.name] else [graph.name] in
    List.iter (fun (_,_,term) -> Option.iter (walk collect owner) term) graph.inputs;
    walk collect owner graph.body) (workspace.graphs @ workspace.defs)) [true;false];
  let pending path = [Flow.Diagnostic.error ~code:"E_PACKED_PENDING"
    ("No unambiguous static specialization was observed for producer " ^ String.concat "/" path ^ ".")] in
  let observed residual =
    let view = E.Private.residual_view residual in
    let producers = Option.value ~default:[] (Hashtbl.find_opt index view.term.form.id)
      |> List.find_opt (fun (form, owner, _) -> form == view.term.form && owner = view.owner)
      |> Option.fold ~none:W.Paths.empty ~some:(fun (_,_,paths) -> paths) in
    let producer = if W.Paths.cardinal producers = 1 then Some (W.Paths.choose producers) else None in
    (if producer = None then
      let possible = Option.value ~default:[] (Hashtbl.find_opt roots view.term.form.id)
        |> List.fold_left (fun paths (form, path) ->
            if form == view.term.form then W.Paths.add path paths else paths) producers in
      W.Paths.iter (fun path -> match Hashtbl.find_opt outcomes path with
        | Some (_ :: _) -> ()
        | _ -> Hashtbl.replace outcomes path (pending path)) possible);
    let adapted = match producer, view.term.node with
      | Some path, W.Op {op = "image/map"; args; _} ->
          let canonical = match Flow.Op.find ~extra:workspace.ops "image/map" Flow.Context.image,
              Flow.Op.find "image/map" Flow.Context.image with
            | Some actual, Some builtin -> actual == builtin | _ -> false in
          if not canonical then Error [Flow.Diagnostic.error ~code:"E_GPU_FORM"
            "Image qualification needs the canonical image/map declaration."]
          else (match List.assoc_opt "function" args with
            | None -> Error [Flow.Diagnostic.error ~code:"E_KERNEL_FORM" "Image map needs a pixel function."]
            | Some term ->
                let mapped = Result.map_error (fun d -> [d]) (Result.bind
                  (E.Private.eval_term residual term ~live:(Frame_input.at_time 0.)) (function
                    | E.Fn fn -> E.Private.map_function ~path
                        ~site:(view.instance, view.site, view.iter)
                        ~signature:Ty.{params=[Vec2];result=Vec4} fn [E.Vec2_array [|0.5;0.5|]]
                    | _ -> Error (Flow.Diagnostic.error ~code:"E_KERNEL_FORM" "Image map needs a pixel function."))) in
                Result.bind mapped (function E.Residual r -> Ok r
                    | _ -> Error [Flow.Diagnostic.error ~code:"E_KERNEL_FORM" "Image map did not produce a pixel program."]))
      | _ -> Ok residual in
    Option.iter (fun path ->
      let reasons = match adapted with
        | Error reasons -> reasons
        | Ok residual -> (match Packed.compile_result residual (E.Private.residual_view residual).term with
            | Error reasons -> reasons | Ok packed -> Packed.gpu_refusals ~fused:true packed) in
      let reasons = List.map (fun (d : Flow.Diagnostic.t) ->
        {d with span = (match d.span with Some _ -> d.span | None -> Some view.term.form.span);
          message = Printf.sprintf "%s (producer %s, instance %d, tuple [%s])" d.message
            (String.concat "/" path) view.instance (String.concat "," (List.map string_of_int view.iter))}) reasons in
      match Hashtbl.find_opt outcomes path with
      | Some ((d : Flow.Diagnostic.t) :: _) when d.code <> "E_PACKED_PENDING" || reasons = [] -> ()
      | _ -> Hashtbl.replace outcomes path reasons) producer;
    Option.iter (fun callback -> callback producer (Result.value adapted ~default:residual)) observe in
  (* A large static map the checker accepts as an approximable producer may run on the device and come
     back as a 32-bit array: the SOP-input readback of the owner decision, taken within a tolerance. *)
  let bulk_approx residual =
    if Domain.DLS.get Gpu.current = None then None else
    let value = E.Residual residual in
    if E.frame_dependent value || E.state_dependent value then None else
    match Packed.compile residual (E.Private.residual_view residual).term with
    | None -> None
    | Some packed ->
        let _, site, _ = Packed.site packed in        if List.exists (fun (path, form) -> path = site && (match form with W.Candidate _ -> true | W.Refused _ -> false))
             workspace.packed
        then !gpu_bulk ~approx:(W.Paths.singleton site) residual else None in
  let (let*) = Result.bind in
  let* evaluated = E.Private.static_with_kernels ?record ?inputs ~observe:observed ~bulk:static_map ~bulk_approx workspace in
  let packed_by_path = Hashtbl.create 64 in
  List.iter (fun (path, form) -> if not (Hashtbl.mem packed_by_path path) then Hashtbl.add packed_by_path path form) workspace.packed;
  let reasons path = match Hashtbl.find_opt packed_by_path path with
    | Some (W.Refused reasons) -> reasons
    | _ -> Option.value ~default:(pending path) (Hashtbl.find_opt outcomes path) in
  let approx, approx_reasons = List.fold_left (fun (accepted, rejected) (path, form) ->
    let refused = match form with
      | W.Refused reasons -> reasons
      | Candidate producers -> W.Paths.elements producers |> List.concat_map reasons in
    if refused = [] then W.Paths.add path accepted, rejected
    else accepted, (path, refused) :: rejected) (W.Paths.empty, []) workspace.packed in
  Ok ({workspace with approx; approx_reasons = List.rev approx_reasons}, evaluated)

type id = W.path * int list
module Count = struct
  type t = Static of int | Data of id | Unknown
end
type rate = Static | Frame | Event
type precision = Exact | Approx
type tier = Interp | Closure | Cpu_kernel | Gpu | Gpu_compile | Gpu_readback | Cooked
module Cost = struct
  type tier_cost = {fixed : float; per_element : float}
  (* P4 measured CPU table.
     CPU packed-map endpoints are 1,024 and 1M elements at one domain; the
     closure row is one compiled scalar expression. Only legal tiers compete.
     ponytail: one affine model per tier, not an instruction-aware scheduler;
     fit per-operation models only if placement mistakes are measured. *)
  let table = function
    | Interp -> {fixed=0.;per_element=0.710282087 /. 1_000_000.}
    | Closure -> {fixed=0.000000170;per_element=0.}
    | Cpu_kernel ->
        let per_element = (0.016522884 -. 0.000040054) /. (1_000_000. -. 1024.) in
        {fixed=0.000040054 -. per_element *. 1024.;per_element}
    (* P5 native rows (Apple M1,
       `bench_gpu`): pack, upload and synchronized
       dispatch at 1,024 and 1M elements, explicit readback, cold compile. *)
    | Gpu ->
        let per_element = (0.020215034 -. 0.000437975) /. (1_000_000. -. 1024.) in
        {fixed=0.000437975 -. per_element *. 1024.;per_element}
    | Gpu_readback ->
        let per_element = (0.007947922 -. 0.000010014) /. (1_000_000. -. 1024.) in
        {fixed=0.000010014 -. per_element *. 1024.;per_element}
    | Gpu_compile -> {fixed=0.008223057;per_element=0.}
    | Cooked -> {fixed=infinity;per_element=0.}
  let estimate tier ~count =
    if count < 0 then invalid_arg "Flow_ir.Cost.estimate: negative count";
    let cost = table tier in cost.fixed +. cost.per_element *. float count
  (* Per element beyond the producer, from the same log's editor rows at
     10,000 and 1M circles: CPU instance building and upload against the
     resident GPU circle conversion. A display sink competes route by route. *)
  let display_sink tier ~count =
    if count < 0 then invalid_arg "Flow_ir.Cost.display_sink: negative count";
    float count *. (match tier with
      | Gpu -> (0.028567791 -. 0.001480103) /. 990_000. -. (table Gpu).per_element
      | _ -> (0.903350830 -. 0.010123968) /. 990_000. -. (table Cpu_kernel).per_element)
  let cheapest ~legal ~count = match legal with
    | [] -> invalid_arg "Flow_ir.Cost.cheapest: no legal tier"
    | first :: rest -> List.fold_left (fun best tier ->
        if estimate tier ~count < estimate best ~count then tier else best) first rest
  let packed ~count = cheapest ~legal:[Interp;Cpu_kernel] ~count
end
type source = Constant of E.value | Frame_field of string | Input of string | State_previous of E.residual
type body = Operation of string | Vector | Field of string | List_value
  | Record_value of string list | Struct_value of string * Ty.t * string list
  | Reference of E.residual | Packed_map of Packed.t | Readback
type kernel = { body : body; elementwise : bool; requires_exact : bool }
type sink = Display of string | Export | Sop_input | State_seed | Cache_key
type role = Position | Target | Size | Fill | Stroke | Width
(* [output] picks one of the several outputs of a fused region node. *)
type attribute = { role : role; edge : string; output : int }
type marks = { set : string; attributes : attribute list }
type kind = Source of source | Kernel of kernel
  | Opaque of string * Flow.Check.kernel_facts option | Sink of sink | Marks of marks
type edge = { name : string; node : int }
type node = { id : id; ty : Ty.t; count : Count.t; rate : rate; precision : precision;
  kind : kind; args : edge list; scope : int list; invariant : bool;
  provenance : id list; tier : tier }
type t = { nodes : node array; roots : int array; groups : int array array }
type execution = { owner : id; sites : (int * W.path * int list) list;
  tier : tier; seconds : float }
module Profile = struct
  type t = {clock : unit -> float; executions : execution list Atomic.t}
  let create ~clock = {clock; executions = Atomic.make []}
  let executions profile = Atomic.get profile.executions
  let rec record profile execution =
    let previous = executions profile in
    let rec keep n = function
      | [] -> [] | _ when n = 0 -> []
      | old :: rest when old.sites = execution.sites && old.tier=execution.tier -> keep n rest
      | old :: rest -> old :: keep (n - 1) rest in
    (* ponytail: 512 recent groups, a linear scan once per group; index if
       profiling many small groups measurably costs a frame. *)
    let next = execution :: keep 511 previous in
    if not (Atomic.compare_and_set profile.executions previous next) then record profile execution
end

let rate_join a b = match a, b with
  | Event, _ | _, Event -> Event | Frame, _ | _, Frame -> Frame | _ -> Static
let attribute_edges (_ : t) (node : node) = match node.kind with
  | Marks m -> List.filter_map (fun a -> List.find_opt (fun e -> e.name = a.edge) node.args
      |> Option.map (fun e -> a, e.node)) m.attributes
  | _ -> []
let singleton_groups nodes = Array.init (Array.length nodes) (fun i -> [|i|])
let graph nodes roots = {nodes; roots; groups = singleton_groups nodes}

(* Indices are topological; refuse corrupt direct IR before any pass follows edges. *)
let validate ir =
  let n = Array.length ir.nodes in
  Array.iteri (fun i node ->
    List.iter (fun e -> if e.node < 0 || e.node >= i then
      invalid_arg "Flow_ir: arguments must reference earlier nodes") node.args;
    match node.count with Count.Static n when n < 0 ->
      invalid_arg "Flow_ir: negative count" | _ -> ()) ir.nodes;
  Array.iter (fun i -> if i < 0 || i >= n then invalid_arg "Flow_ir: invalid root") ir.roots

let rec plain = function
  | E.Fn _ | Residual _ | Deferred _ -> false
  | List xs -> Array.for_all plain xs
  | Record fs | Struct (_, _, fs) -> List.for_all (fun (_, v) -> plain v) fs
  | _ -> true

(* Constants are shared by content, but marshalling a large array twice per optimization costs more than the
   kernel that reads it. An array of at least [big_constant] elements is keyed by its physical identity within
   one [share] call (arrays are immutable): equal content in two distinct arrays is simply not merged. *)
let big_constant = 256
let rec has_big = function
  | E.Int_array a -> Array.length a >= big_constant
  | Float_array a -> Array.length a >= big_constant
  | Text_array a -> Array.length a >= big_constant
  | Vec2_array a | Vec3_array a | Vec4_array a -> Array.length a >= big_constant
  | List xs -> Array.exists has_big xs
  | Record fs | Struct (_, _, fs) -> List.exists (fun (_, v) -> has_big v) fs
  | _ -> false
let constant_key ids v =
  let piece s = string_of_int (String.length s) ^ ":" ^ s in
  let rec key v =
    if not (has_big v) then piece (Marshal.to_string v [Marshal.No_sharing]) else
    let identity o = match List.find_opt (fun (p, _) -> p == o) !ids with
      | Some (_, id) -> id
      | None -> let id = List.length !ids in ids := (o, id) :: !ids; id in
    match v with
    | E.Int_array a -> "#I" ^ string_of_int (identity (Obj.repr a))
    | Float_array a -> "#F" ^ string_of_int (identity (Obj.repr a))
    | Text_array a -> "#T" ^ string_of_int (identity (Obj.repr a))
    | Vec2_array a -> "#2" ^ string_of_int (identity (Obj.repr a))
    | Vec3_array a -> "#3" ^ string_of_int (identity (Obj.repr a))
    | Vec4_array a -> "#4" ^ string_of_int (identity (Obj.repr a))
    | List xs -> "[" ^ String.concat "," (List.map key (Array.to_list xs)) ^ "]"
    | Record fs -> "{" ^ String.concat "," (List.map (fun (n, v) -> piece n ^ key v) fs) ^ "}"
    | Struct (n, ty, fs) -> "S" ^ piece (Marshal.to_string (n, ty) []) ^ key (Record fs)
    | v -> piece (Marshal.to_string v [Marshal.No_sharing]) in
  key v

let share_key ids node =
  let kind = match node.kind with
    | Source (Constant v) when plain v -> Some ("constant", constant_key ids v)
    | Source (Frame_field name) -> Some ("frame", name)
    | Kernel {body = Operation name; _} -> Some ("op", name)
    | Kernel {body = Vector; _} -> Some ("vector", "")
    | Kernel {body = Field field; _} -> Some ("field", field)
    | Kernel {body = Readback; _} -> Some ("exact", "")
    | Kernel {body = List_value; _} -> Some ("list", "")
    | Kernel {body = Record_value fields; _} -> Some ("record", Marshal.to_string fields [])
    | Kernel {body = Struct_value (name, ty, fields); _} ->
        Some ("struct", Marshal.to_string (name, ty, fields) [])
    | _ -> None in
  Option.map (fun kind ->
    let requirements = match node.kind with Kernel k -> k.elementwise, k.requires_exact | _ -> false, false in
    Marshal.to_string (kind, requirements, node.ty, node.count, node.rate,
      node.precision, node.scope, node.args) [Marshal.No_sharing]) kind

let share ir =
  validate ir;
  let n = Array.length ir.nodes in
  let remap = Array.make n 0 and kept = Array.copy ir.nodes and count = ref 0 in
  let seen = Hashtbl.create n and ids = ref [] in
  Array.iteri (fun i node ->
    let node = {node with args = List.map (fun e -> {e with node = remap.(e.node)}) node.args} in
    let key = share_key ids node in
    match Option.bind key (Hashtbl.find_opt seen) with
    | Some existing ->
        remap.(i) <- existing;
        let old = kept.(existing) in
        kept.(existing) <- {old with provenance = List.sort_uniq compare (old.provenance @ node.provenance)}
    | None ->
        let index = !count in incr count; remap.(i) <- index; kept.(index) <- node;
        Option.iter (fun key -> Hashtbl.add seen key index) key) ir.nodes;
  graph (Array.sub kept 0 !count) (Array.map (Array.get remap) ir.roots)

let hoist ir =
  validate ir;
  let nodes = Array.copy ir.nodes in
  Array.iteri (fun i node ->
    match node.kind with
    | Kernel {body = (Reference _ | Packed_map _); _} | Source _ | Opaque _ | Sink _ | Marks _ -> ()
    | Kernel _ when node.invariant && node.rate = Static
        && List.for_all (fun e -> nodes.(e.node).rate = Static && nodes.(e.node).scope = []) node.args ->
        nodes.(i) <- {node with scope = []}
    | _ -> ()) ir.nodes;
  {ir with nodes; groups = singleton_groups nodes}

let prune ir =
  validate ir;
  let keep = Array.make (Array.length ir.nodes) false in
  Array.iter (fun i -> keep.(i) <- true) ir.roots;
  for i = Array.length ir.nodes - 1 downto 0 do
    if keep.(i) then List.iter (fun e -> keep.(e.node) <- true) ir.nodes.(i).args
  done;
  let remap = Array.make (Array.length ir.nodes) (-1) and count = ref 0 in
  Array.iteri (fun i live -> if live then (remap.(i) <- !count; incr count)) keep;
  let nodes = Array.copy ir.nodes in
  Array.iteri (fun i node -> if keep.(i) then
    nodes.(remap.(i)) <- {node with args = List.map (fun e -> {e with node = remap.(e.node)}) node.args}) ir.nodes;
  graph (Array.sub nodes 0 !count) (Array.map (Array.get remap) ir.roots)

let fusible = function Kernel {elementwise = true; body = (Operation _ | Vector | Field _); _} -> true | _ -> false
let same_count a b = match a, b with
  | Count.Static a, Count.Static b -> a = b
  | Count.Data a, Count.Data b -> a = b
  | _ -> false
let fuse ir =
  validate ir;
  let n = Array.length ir.nodes in
  let uses = Array.make n 0 and owner = Array.init n Fun.id and barriers = Array.make n 0 in
  Array.iter (fun i -> uses.(i) <- uses.(i) + 1) ir.roots;
  let barrier = ref 0 in
  Array.iteri (fun i node ->
    (match node.kind with Sink _ | Opaque _ | Marks _ -> incr barrier | _ -> ());
    barriers.(i) <- !barrier;
    List.iter (fun e -> uses.(e.node) <- uses.(e.node) + 1) node.args) ir.nodes;
  Array.iteri (fun i node -> if fusible node.kind then
    List.iter (fun e ->
      let before = ir.nodes.(e.node) in
      if uses.(e.node) = 1 && fusible before.kind && same_count before.count node.count
          && before.rate = node.rate && before.precision = node.precision
          && before.scope = node.scope && barriers.(e.node) = barriers.(i)
      then owner.(e.node) <- i) node.args) ir.nodes;
  for i = n - 1 downto 0 do if owner.(i) <> i then owner.(i) <- owner.(owner.(i)) done;
  let groups = Array.make n [] in
  for i = n - 1 downto 0 do groups.(owner.(i)) <- i :: groups.(owner.(i)) done;
  {ir with groups = Array.of_list (Array.fold_right (fun g acc ->
    if g = [] then acc else Array.of_list g :: acc) groups [])}

(* Attribute fusion: the region attributes of one mark set that agree on count (or origin),
   rate, scope and precision, and feed nothing else, become one region node with an output
   each (at the lowest member's index, so the order stays topological). Declining, for
   register limit or disagreement, leaves the members separate. *)
let fuse_attributes ir =
  validate ir;
  let uses = Array.make (Array.length ir.nodes) 0 in
  Array.iter (fun i -> uses.(i) <- uses.(i) + 1) ir.roots;
  Array.iter (fun node -> List.iter (fun e -> uses.(e.node) <- uses.(e.node) + 1) node.args) ir.nodes;
  let nodes = Array.copy ir.nodes in
  Array.iteri (fun m node -> match node.kind with
    | Marks marks ->
        let candidates = attribute_edges ir node |> List.filter_map (fun (a, i) ->
          match nodes.(i).kind with
          | Kernel {body = Packed_map p; elementwise = true; _} when uses.(i) = 1 -> Some (i, a, Packed.to_region p)
          | _ -> None) |> List.sort (fun (i, _, _) (j, _, _) -> compare i j) in
        let compatible i j = let a = nodes.(i) and b = nodes.(j) in
          same_count a.count b.count && a.rate = b.rate && a.scope = b.scope && a.precision = b.precision in
        let groups = List.fold_left (fun groups ((i, _, _) as member) ->
          let rec join = function
            | [] -> [[member]]
            | (((head, _, _) :: _) as group) :: rest when compatible head i
                && Option.is_some (Core_fuse.merge (List.map (fun (_, _, r) -> r) (group @ [member]))) ->
                (group @ [member]) :: rest
            | group :: rest -> group :: join rest in
          join groups) [] candidates in
        let args = ref node.args and attributes = ref marks.attributes in
        List.iter (function
          | [] | [_] -> ()
          | ((head, _, _) :: _) as members ->
              let merged = Option.get (Core_fuse.merge (List.map (fun (_, _, r) -> r) members)) in
              (match Packed.of_region merged with
               | Error _ -> ()
               | Ok packed ->
                   let first = nodes.(head) in
                   nodes.(head) <- {first with ty = Ty.Any;
                     kind = Kernel {body = Packed_map packed; elementwise = true; requires_exact = true};
                     provenance = List.sort_uniq compare (List.concat_map (fun (i, _, _) -> nodes.(i).provenance) members)};
                   List.iteri (fun output (_, (a : attribute), _) ->
                     args := List.map (fun e -> if e.name = a.edge then {e with node = head} else e) !args;
                     attributes := List.map (fun (b : attribute) ->
                       if b.edge = a.edge then {b with output} else b) !attributes) members)) groups;
        nodes.(m) <- {node with args = !args; kind = Marks {marks with attributes = !attributes}}
    | _ -> ()) ir.nodes;
  {ir with nodes; groups = singleton_groups nodes}

type target = { legal : node -> tier -> bool; cost : tier -> count:int -> float }
let native = {
  legal = (fun node tier -> match tier, node.kind, node.count with
    | Gpu, Kernel {body = Packed_map packed; _}, Count.Static count ->
        count >= 1024 && Packed.gpu_refusals ~fused:true packed = []
    | Gpu, Marks _, _ -> true
    | Gpu, _, _ -> false
    | _ -> true);
  cost = (fun tier ~count -> Cost.estimate tier ~count) }

let place ?(approx=W.Paths.empty) ?(gpu_cost=fun _ ~count:_ -> None) ?(target=native) ir =
  validate ir;
  let nodes = Array.copy ir.nodes in
  let exception Refused of Flow.Diagnostic.t in
  let cheapest node legal ~count =
    let legal = match List.filter (target.legal node) legal with [] -> legal | allowed -> allowed in
    List.fold_left (fun best tier -> if target.cost tier ~count < target.cost best ~count then tier else best)
      (List.hd legal) (List.tl legal) in
  let packed node ~count = cheapest node [Interp; Cpu_kernel] ~count in
  (* The SOP input sink reads a GPU producer back like (exact x): an implicit readback. *)
  let is_readback node = match node.kind with Kernel {body = Readback; _} | Sink Sop_input -> true | _ -> false in
  let regions_of node = List.map (fun (_, i) -> i) (attribute_edges ir node) in
  let gpu = Array.map (fun node ->
    match node.kind, node.count with
    | Kernel {body = Packed_map packed'; _}, Count.Static count
      when List.exists (fun (path, _) -> W.Paths.mem path approx) node.provenance ->
        target.legal node Gpu && Option.fold ~none:false ~some:(fun seconds ->
          Float.is_finite seconds && seconds >= 0. && seconds < target.cost (packed node ~count) ~count)
          (gpu_cost packed' ~count)
    | _ -> false) ir.nodes in
  (* A target without readback keeps a region on the CPU when its value must come back:
     a consumer that reads it exactly, or a mark set that is not wholly on the GPU. *)
  let uniform i = match ir.nodes.(i).kind, ir.nodes.(i).count with
    | Source (Constant _), _ | _, Count.Static 1 -> true | _ -> false in
  let changed = ref true in
  while !changed do
    changed := false;
    let demote i = if gpu.(i) then (gpu.(i) <- false; changed := true) in
    Array.iter (fun node ->
      if is_readback node && not (target.legal node Gpu_readback) then
        List.iter (fun e -> demote e.node) node.args;
      match node.kind with
      | Marks _ when not (target.legal node Gpu_readback) ->
          let regions = regions_of node in
          if not (List.for_all (fun i -> gpu.(i) || uniform i) regions) then List.iter demote regions
      | _ -> ()) ir.nodes
  done;
  try
    Array.iteri (fun i node ->
      let gpu = gpu.(i) in
      let approximate = List.exists (fun e -> nodes.(e.node).precision = Approx) node.args in
      let readback = is_readback node in
      let forbidden = match node.kind with
        | Opaque _ | Sink (Export | State_seed | Cache_key) | Source (State_previous _) -> true
        | Kernel k -> k.requires_exact && not readback
        | _ -> false in
      if approximate && forbidden then
        raise (Refused (Flow.Diagnostic.error ~code:"E_APPROX_SINK"
          "Approximate values require (exact x) before catalog calls, exports, state or cache keys."));
      let precision = if readback then Exact else if approximate || gpu then Approx else node.precision in
      let tier = match node.kind with
        | Opaque _ -> Cooked
        | Kernel {body = Packed_map _; _} when gpu -> Gpu
        | Kernel {body = Readback; _} when approximate && target.legal node Gpu_readback -> Gpu_readback
        | Kernel {body = Packed_map _; _} ->
            (match node.count with Count.Static count -> packed node ~count | _ -> Cpu_kernel)
        | Kernel {body = Reference r; _} -> if Result.is_ok (Core_lower.lower r) then
            cheapest node [Interp; Closure] ~count:(match node.count with Count.Static n -> n | _ -> 1)
            else Interp
        | Kernel _ -> Closure
        | Marks _ ->
            (* Like a kernel: the GPU when every attribute is a GPU region or a constant. *)
            let regions = regions_of node in
            let on_gpu i = nodes.(i).tier = Gpu in
            if List.exists on_gpu regions && List.for_all (fun i -> on_gpu i || uniform i) regions
               && target.legal node Gpu then Gpu
            else if List.exists (fun i -> not (uniform i)) regions then Cpu_kernel else Interp
        | _ -> Interp in
      nodes.(i) <- {node with precision; tier}) ir.nodes;
    Ok {ir with nodes}
  with Refused d -> Error d

let optimize ?target ir = Result.map fuse (place ?target (prune (fuse_attributes (share (hoist ir)))))

module Residuals = Hashtbl.Make (struct
  type t = E.residual
  let equal a b = a == b
  let hash = E.Private.residual_id
end)
type builder = {
  mutable rev : node list;
  mutable count : int;
  mutable checks : int list;
  slots : (int, node) Hashtbl.t;
  deferred : (int, int) Hashtbl.t;
  residuals : int Residuals.t;
  live : W.Paths.t;
  invariant : W.Paths.t;
  count_source : Packed.count_source;
}
let builder ?(count_source = fun _ _ -> None) live invariant = {rev = []; count = 0; checks = []; slots = Hashtbl.create 64;
  deferred = Hashtbl.create 64; residuals = Residuals.create 64; live; invariant; count_source}
let authored (path, iter) = match path with
  | namespace :: path when String.starts_with ~prefix:"@instance:" namespace -> path, iter
  | _ -> path, iter
let identity instance (path, iter) =
  if instance < 0 then path, iter else ("@instance:" ^ string_of_int instance) :: path, iter
let at_path (path, iter) replacement = match path with
  | namespace :: _ when String.starts_with ~prefix:"@instance:" namespace -> namespace :: replacement, iter
  | _ -> replacement, iter
let add ?(provenance = []) b id ty count kind args rate =
  let i = b.count in b.count <- i + 1;
  let rate = List.fold_left (fun r e -> rate_join r (Hashtbl.find b.slots e.node).rate) rate args in
  let origin = authored id in
  let rate = if W.Paths.mem (fst origin) b.live then rate_join Frame rate else rate in
  let scope = match kind, rate with Source (Constant _), Static -> [] | _ -> snd id in
  let provenance = List.sort_uniq compare (origin :: List.map authored provenance) in
  let n = {id; ty; count; kind; args; rate; precision = Exact; scope;
    invariant = W.Paths.mem (fst origin) b.invariant; provenance; tier = Interp} in
  b.rev <- n :: b.rev; Hashtbl.add b.slots i n; i
let kernel body = Kernel {body; elementwise = true; requires_exact = false}
let child (path, iter) name = path @ [name], iter
let value_count = function
  | E.Int_array xs -> Count.Static (Array.length xs)
  | Text_array xs -> Count.Static (Array.length xs)
  | Float_array xs -> Count.Static (Array.length xs)
  | Vec2_array xs -> Count.Static (Array.length xs / 2)
  | Vec3_array xs -> Count.Static (Array.length xs / 3)
  | Vec4_array xs -> Count.Static (Array.length xs / 4)
  | List xs -> Count.Static (Array.length xs)
  | _ -> Count.Static 1
let type_count id = function
  | Ty.Array _ | Ty.List _ -> Count.Data id | _ -> Count.Static 1
let term_count id (term : W.term) = type_count id term.ty
let rec value_ty = function
  | E.Residual r -> (E.Private.residual_view r).term.ty
  | List xs -> Ty.List (Array.fold_left (fun ty v ->
      Option.value ~default:Ty.Any (Ty.join ty (value_ty v))) Ty.Any xs)
  | Record fs -> Ty.Record (List.map (fun (name, v) -> name, value_ty v) fs)
  | v -> V.ty_of v

let rec value b id = function
  | E.Deferred (_, old) as v ->
      (match Hashtbl.find_opt b.deferred old with Some i -> i
       | None -> add b id (V.ty_of v) (Count.Static 1) (Source (Constant v)) [] Static)
  | Residual r -> residual b r
  | List xs ->
      let args = Array.mapi (fun i v -> {name = string_of_int i; node = value b (child id (string_of_int i)) v}) xs |> Array.to_list in
      add b id (value_ty (E.List xs)) (Count.Static (Array.length xs)) (kernel List_value) args Static
  | Record fs -> fields b id (value_ty (E.Record fs)) (Record_value (List.map fst fs)) fs
  | Struct (name, ty, fs) -> fields b id ty (Struct_value (name, ty, List.map fst fs)) fs
  | v -> add b id (V.ty_of v) (value_count v) (Source (Constant v)) [] Static
and fields b id ty body fs =
  let args = List.mapi (fun i (name, v) -> {name; node = value b (child id (name ^ "#" ^ string_of_int i)) v}) fs in
  add b id ty (type_count id ty) (kernel body) args Static
and residual b r =
  match Residuals.find_opt b.residuals r with
  | Some i -> i
  | None ->
      let view = E.Private.residual_view r in
      let id = identity view.instance (view.site, view.iter) in
      let make_reference () =
        let kind = if view.previous then Source (State_previous r)
          else Kernel {body = Reference r; elementwise = false; requires_exact = true} in
        let rate = if E.frame_dependent (E.Residual r) || E.state_dependent (E.Residual r) then Frame else Static in
        add b id view.term.ty (term_count id view.term) kind [] rate in
      let index = if view.previous || E.state_dependent (E.Residual r) then make_reference () else
        let exception Unsupported in
        let saved_checks = b.checks in
        let rec term env id (t : W.term) =
          let id = match t.path with Some p -> at_path id p | None -> id in
          let count = term_count id t in
          let args args = List.mapi (fun i (name, t) -> {name;
            node = term env (child id (name ^ "#" ^ string_of_int i)) t}) args in
          match t.node with
          | W.Lit (Param.Int_value n) -> value b id (E.Int n)
          | Lit (Param.Float_value n) -> value b id (E.Float n)
          | Lit (Param.Bool_value n) -> value b id (E.Bool n)
          | Text text -> value b id (E.Text text)
          | Nil -> value b id E.No_geo
          | Time -> add b id Ty.Float count (Source (Frame_field "t")) [] Frame
          | Ref_binding (name, fs) ->
              let input = match List.assoc_opt name env with
                | Some (`Index i) -> i
                | Some (`Value v) -> value b (child id ("@capture:" ^ name)) v
                | None -> raise Unsupported in
              List.fold_left (fun i field ->
                let ty = match (Hashtbl.find b.slots i).ty with
                  | Ty.Vec2 | Vec3 | Vec4 -> Ty.Float
                  | Ty.Record fields -> Option.value ~default:Ty.Any (List.assoc_opt field fields)
                  | _ -> Ty.Any in
                add b (child id field) ty (type_count (child id field) ty)
                  (kernel (Field field)) [{name = "value"; node = i}] Static) input fs
          | Vec ts when List.mem (List.length ts) [2;3;4] -> add b id t.ty count (kernel Vector)
              (args (List.mapi (fun i t -> string_of_int i, t) ts)) Static
          | (Hof ((`Map | `Reduce), _) | Loop _ | Op {op = "array/sum"; _}) ->
              (match Packed.compile ~count_source:b.count_source r t with
               | None -> raise Unsupported
               | Some program ->
                   let count = match Packed.static_count program with
                     | Some n -> Count.Static n | None -> Count.Data (Option.value ~default:id (Packed.count_origin program)) in
                   let rate = if E.frame_dependent (E.Residual r) then Frame else Static in
                   let kind = Kernel {body = Packed_map program;
                     elementwise = Packed.elementwise program; requires_exact = true} in
                   let provenance = List.map (fun (instance, site, iter) -> identity instance (site, iter))
                     (Packed.provenance program) in
                   add ~provenance b id t.ty count kind [] rate)
          | Op {op; args = ts; _} ->
              let op = match Flow.Op.find ~extra:(E.Private.residual_ops r) op Flow.Context.value with
                | Some op when Flow.Op.packed_kind op <> None -> op
                | _ -> raise Unsupported in
              let kind, rate = if op.live then Source (Frame_field op.name),
                  (if op.name = "frame/events" || op.name = "frame/input" then Event else Frame)
                else if op.name = "exact" then kernel Readback, Static
                else kernel (Operation op.name), Static in
              add b id t.ty count kind (args ts) rate
          | Let (bindings, result) ->
              let env = List.fold_left (fun env (pattern, t) -> match pattern with
                | W.Name name ->
                    let index = term env (child id name) t in
                    (* Unused evaluation can still fail. Only literal constants are total. *)
                    (match (Hashtbl.find b.slots index).kind with
                     | Source (Constant _) -> () | _ -> b.checks <- index :: b.checks);
                    (name, `Index index) :: env
                | _ -> raise Unsupported) env bindings in
              term env (child id "@result") result
          | Get (record, field) -> add b id t.ty count (kernel (Field field))
              [{name = "value"; node = term env (child id "value") record}] Static
          | Expanded {body; _} -> term env id body
          | Bypass call -> (match W.bypass_input call with
              | Some input when input.ty=t.ty -> term env id input
              | _ -> raise Unsupported)
          | _ -> raise Unsupported in
        try term (List.map (fun (name, v) -> name, `Value v) view.bindings) id view.term
        with Unsupported -> b.checks <- saved_checks; make_reference () in
      Residuals.add b.residuals r index; index

(* The draw operators that make a mark set, and the role each input plays. *)
let mark_roles = function
  | "draw/circles" -> ["positions", Position; "radius", Size; "fill", Fill; "stroke", Stroke]
  | "draw/rects" -> ["positions", Position; "sizes", Size; "fill", Fill; "stroke", Stroke]
  | "draw/lines" -> ["from", Position; "to", Target; "color", Stroke; "width", Width]
  | "draw/points" -> ["positions", Position; "color", Fill]
  | _ -> []
let marks_of set (args : edge list) = match mark_roles set with
  | [] -> None
  | roles -> Some (Marks {set; attributes = List.filter_map (fun e ->
      Option.map (fun role -> {role; edge = e.name; output = 0}) (List.assoc_opt e.name roles)) args})

let finish b roots = graph (Array.of_list (List.rev b.rev))
  (Array.append roots (Array.of_list (List.rev b.checks)))
let of_evaluation ws catalog evaluation =
  let b = builder ws.W.live ws.W.invariant in
  let facts = Hashtbl.create (List.length catalog.Flow.Check.kinds) in
  List.iter (fun (kind : Flow.Check.kind) ->
    Hashtbl.replace facts kind.qualified kind.facts;
    List.iter (fun alias -> Hashtbl.replace facts alias kind.facts) kind.aliases) catalog.kinds;
  Array.iter (fun (n : E.node) ->
    let id = identity n.inst (n.site, n.iter) in
    let args = List.mapi (fun i (name, v) -> {name; node = value b (child id (name ^ "#" ^ string_of_int i)) v}) n.args in
    let kind = match marks_of n.kind args with
      | Some marks -> marks
      | None -> if Flow.Op.is_display_kind n.kind then Sink (Display n.kind)
      else Opaque (n.kind, Option.join (Hashtbl.find_opt facts n.kind)) in
    let index = add b id n.ty (Count.Data id) kind args Static in
    Hashtbl.replace b.deferred n.id index) evaluation.E.plan.nodes;
  let roots = List.mapi (fun i (name, v) -> value b ([name; "@result"; string_of_int i], []) v) evaluation.results in
  let states = List.mapi (fun i v -> value b (["@state"; string_of_int i], []) v) evaluation.states in
  finish b (Array.of_list (roots @ states))

module Executor = struct
  type program = { ir : t; value : E.value; templates : Packed.t list Atomic.t;
    core : (E.residual * bool * Core_exec.prepared option) list Atomic.t; owner : unit ref;
    count_source : Packed.count_source; profile : Profile.t option; approx:W.Paths.t;
    sink:sink option;mutable gpu_kernel:(Gpu.backend*Packed.t*Gpu.kernel)option }
  let compile ?profile ?(strict = false) ?(approx=W.Paths.empty) ?sink ?(count_source = fun _ _ -> None) v =
    let b = builder ~count_source W.Paths.empty W.Paths.empty in
    let root = value b (["@value"], []) v in
    let rec packed_sources packed =
      let region = Packed.to_region packed in
      match Array.find_map (fun (r, term) -> source r term) region.sources with
      | Some _ as error -> error
      | None -> Array.find_map captured region.uniforms
    and captured = function
      | E.Residual r -> source r (E.Private.residual_view r).term
      | List values -> Array.find_map captured values
      | Record fields | Struct (_, _, fields) | Source (_, _, fields) ->
          List.find_map (fun (_, value) -> captured value) fields
      | _ -> None
    and source r (term : W.term) =
      let children terms = List.find_map (source r) terms in
      match term.node with
      | Hof ((`Map | `Reduce), _) | Loop _ | Op {op = "array/sum"; _} ->
          (match Packed.compile_result ~count_source r term with
           | Ok packed -> packed_sources packed
           | Error _ -> Some term.form.span)
      | Ref_binding (name, fields) ->
          let captured = List.fold_left (fun value field -> Option.bind value (function
            | E.Record fields | Struct (_, _, fields) -> List.assoc_opt field fields
            | _ -> None)) (List.assoc_opt name (E.Private.residual_view r).bindings) fields in
          Option.bind captured (function
            | E.Residual r -> source r (E.Private.residual_view r).term
            | _ -> None)
      | Op {args; _} | Record args -> children (List.map snd args)
      | Get (term, _) | Expanded {body = term; _} -> source r term
      | Bypass call -> Option.bind (W.bypass_input call) (source r)
      | Let (bindings, result) -> children (result :: List.map snd bindings)
      | With_error(a,b) -> children[a;b]
      | Error_rows{rows=a;mapper=b;_} -> children[a;b]
      | If (a,b,c) -> children [a;b;c]
      | Cond (arms,default) -> children(default::List.concat_map(fun(a,b)->[a;b])arms)
      | Case (value,arms,default) -> children(value::default::List.map snd arms)
      | Vec terms | List_lit terms | Str terms | List_op(_,terms) | Hof(_,terms) -> children terms
      | Assoc (record,updates) -> children(record::List.map snd updates)
      | Call_fn {args;body;_} -> children(args @ Option.to_list body)
      | Graph_ref {inputs;_} | Call {args=inputs;_} -> children(List.map snd inputs)
      | Fn {body;capture;_} -> children(body::Option.to_list capture)
      | State {init;step;_} -> children[init;step]
      | _ -> None in
    Result.bind (optimize (finish b [|root|])) (fun ir ->
      match if strict then Array.find_map (fun node -> match node.kind with
        | Kernel {body = Reference r; _} when E.state_dependent (E.Residual r) ->
            (* state-reading kernels are reference-evaluated by design (make_reference above) *)
            None
        | Kernel {body = Reference r; _} ->
            let term=(E.Private.residual_view r).term in
            (match Core_lower.lower ~strict:true r with
             | Ok core -> Array.find_map (fun region ->
                 match Packed.of_region region with
                 | Ok packed -> packed_sources packed
                 | Error _ -> Some term.form.span) core.Core.regions
             | Error _ -> Some term.form.span)
        | Source (State_previous r) -> Some (E.Private.residual_view r).term.form.span
        | Kernel {body = Packed_map packed; _} -> packed_sources packed
        | _ -> None) ir.nodes else None with
      | Some span ->
          Error (Flow.Diagnostic.error ~span ~code:"E_KERNEL_FORM"
            "Numeric node bodies must compile to packed kernels.")
      | None -> Ok {ir; value = v; templates = Atomic.make []; core = Atomic.make []; owner = ref (); count_source; profile;
          approx;sink;gpu_kernel=None})
  let graph p = p.ir
  (* One timing per packed group, recorded when the group runs. *)
  let measure ?(ran = ignore) profile = Option.map (fun profile -> profile.Profile.clock,
    (fun packed ~seconds ~reference ->
      ran ();
      let sites = Packed.provenance packed in
      let _, path, iter = Packed.site packed in
      Profile.record profile {owner = path, iter; sites; seconds; tier = if reference then Interp else Cpu_kernel})) profile
  let force ?state ?elems ?resolve ?(reference = false) ?(strict = false) program ~live =
    let result, packed_ran, fell_back, started =
      if not (E.is_live program.value) then
        let started = Option.map (fun profile -> profile.Profile.clock ()) program.profile in
        Ok program.value, false, reference, started
      else begin
    let packed_ran = ref false in
    let measure = measure ~ran:(fun () -> packed_ran := true) program.profile in
    let state = match state with Some state -> state | None -> E.create_state () in
    let fell_back = ref reference in
    let fallback () = fell_back := true; E.Private.force_reference ~state ?elems ?resolve program.value ~live in
    let force_with_packed value ~live =
      let execute_packed r live =
        let cached = Atomic.get program.templates in
        let prepared = match List.find_map (fun p -> Packed.rebind p r) cached with
          | Some p -> Some p
          | None ->
              let prepared = Packed.compile_template ~count_source:program.count_source r (E.Private.residual_view r).term in
              Option.iter (fun p ->
                let rec take n = function [] -> [] | _ when n = 0 -> [] | p :: rest -> p :: take (n - 1) rest in
                ignore (Atomic.compare_and_set program.templates cached (p :: take 31 cached))) prepared;
              prepared in
        match prepared with
        | Some p when strict || not (Option.fold ~none:false ~some:(fun count -> Cost.packed ~count <> Cpu_kernel) (Packed.static_count p)) ->
            Some (Packed.force ~strict ~state ?elems ?resolve ?measure p ~live)
        | _ ->
            (match strict, (E.Private.residual_view r).term.node with
             | true, (W.Hof ((`Map | `Reduce), _) | W.Loop _ | W.Op {op = "array/sum"; _}) ->
                 Some (Error (Flow.Diagnostic.error ~code:"E_KERNEL_FORM"
                   "Numeric node bodies must compile to packed kernels."))
             | _ -> None) in
      let execute r live =
        let core = try
          let cached = Atomic.get program.core in
          let rec find = function [] -> None | (key,mode,p)::rest -> if key == r && mode=strict then Some p else find rest in
          let prepared = match find cached with
            | Some p -> p
            | None ->
                let prepared = Option.bind (Result.to_option (Core_lower.lower ~strict r)) (fun core ->
                  (* Placement declines before execution, while state is untouched. *)
                  let declined = not strict && Array.exists (fun (region : Core.Region.t) ->
                    Option.fold ~none:false ~some:(fun count -> Cost.packed ~count <> Cpu_kernel)
                      region.count) core.Core.regions in
                  if declined then None else Some (Core_exec.prepare core)) in
                (* ponytail: linear lookup bounded to 64 immutable residuals;
                   use an identity map if profiling finds lookup significant. *)
                let rec take n = function [] -> [] | _ when n=0 -> [] | x::xs -> x::take (n-1) xs in
                ignore (Atomic.compare_and_set program.core cached ((r,strict,prepared)::take 63 cached));
                prepared in
          match prepared with
          | None -> None
          | Some p when not (Core_exec.has_regions p) ->
              let run live = Core_exec.execute ~root:r ~strict ?resolve p ~live in
              E.Private.set_core r ~owner:program.owner ?fast:(Core_exec.fast p) run;
              Some (run live)
          | Some p -> Some (Core_exec.execute ~root:r ~strict ?resolve ~force:(fun packed ->
              Packed.force ~strict ~state ?elems ?resolve ?measure packed ~live) p ~live)
          with _ -> None in
        match core with Some _ -> core | None -> execute_packed r live in
      E.Private.force_with_executor ~state ?elems ?resolve ~owner:program.owner
        ~compiled:true ~strict ~execute value ~live in
    let run () = if reference then fallback () else force_with_packed program.value ~live in
    let started = Option.map (fun profile -> profile.Profile.clock ()) program.profile in
    let result = run () in
    result, !packed_ran, !fell_back, started
      end in
    Option.iter (fun profile -> if not packed_ran then begin
      let root = program.ir.nodes.(program.ir.roots.(0)) in
      let tier = if fell_back then Interp else root.tier in
      let instance = match fst root.id with
        | namespace :: _ when String.starts_with ~prefix:"@instance:" namespace ->
            int_of_string (String.sub namespace 10 (String.length namespace - 10))
        | _ -> -1 in
      let sites = match root.kind with
        | Kernel {body = Packed_map packed; _} -> Packed.provenance packed
        | _ -> List.map (fun (path, iter) -> instance, path, iter) root.provenance in
      Profile.record profile {owner = authored root.id; sites; tier;
        seconds = max 0. (profile.clock () -. Option.get started)}
    end) program.profile;
    result
  let root_packed program =
    let rec find readback index =
      let node=program.ir.nodes.(index) in
      match node.kind with
      | Kernel {body=Packed_map packed;_} -> Some (packed,readback)
      | Kernel {body=Readback;_} -> (match node.args with
          | [{node;_}] -> find true node | _ -> None)
      | _ -> None in
    find false program.ir.roots.(0)
  let packed_root program = Option.map fst (root_packed program)
  let force_into_cpu ?state ?elems ?resolve ?(strict=false) ~column_source ~count ~outputs program ~live =
    match packed_root program with
    | Some packed when Packed.elementwise packed ->
        Result.map (fun () -> Some ()) (Packed.force_into ?state ?elems ?resolve
          ?measure:(measure program.profile) ~strict ~column_source ~count ~outputs packed ~live)
    | _ -> Ok None

  type displayed=Cpu of E.value | Gpu of Gpu.value
  (* A packed region on the GPU when the backend takes it and measures it cheaper. [cached] and
     [store] are the owner's one-kernel cache; [display] says a GPU array may leave the executor. *)
  let display_packed ?state ?resolve ~backend ~policy ~approx ~display ~profile ~cached ~store ~readback packed ~live =
    let cpu()=Ok None in
    let _,site,_=Packed.site packed in
    if not(W.Paths.mem site approx) || Packed.gpu_refusals ~fused:true packed <> []
      then cpu()else
    let cheaper count= count>=1024 && (policy=Gpu.Qualification ||
      Option.fold ~none:false ~some:(fun seconds->Float.is_finite seconds && seconds>=0.
        && seconds+.(if readback then Cost.estimate Gpu_readback ~count else Cost.display_sink Gpu ~count)
          <Cost.estimate(Cost.packed ~count) ~count
           +.(if readback then 0. else Cost.display_sink Cpu_kernel ~count))(backend.Gpu.cost packed ~count))in
    (* Static cardinality lets unmeasured placement decline before allocating
       producer inputs, especially the million-element exact SOP value lane. *)
    if Option.fold ~none:false ~some:(fun count->not(cheaper count))(Packed.static_count packed)
      then cpu()else
    let (let*)=Result.bind in
    let* inputs=Packed.Private.prepare ?state ?resolve packed ~live in
    if not(cheaper inputs.count) then cpu()else
    let* ()=if display||readback then Ok()else
      Error(Flow.Diagnostic.error ~code:"E_APPROX_SINK"
        "GPU arrays require a display sink or (exact x) before CPU, SOP, state, export or cache use.")in
    let profile ?(seconds=fun _->None) tier run=
      let start=Option.map(fun p->p.Profile.clock())profile in
      let result=run()in
      let elapsed=match result with Ok value->seconds value|Error _->None in
      Option.iter(fun p->let _,path,iter=Packed.site packed in
        Profile.record p {owner=(path,iter);sites=Packed.provenance packed;tier;
          seconds=(match elapsed with Some value when Float.is_finite value && value>=0.->value
            |_->max 0.(p.clock()-.Option.get start))})profile;result in
    let* kernel=match cached()with
      |Some(previous,p,kernel)when previous==backend && p==packed->Ok kernel
      |_->profile Gpu_compile(fun()->Result.map(fun kernel->store(backend,packed,kernel);kernel)
          (backend.prepare packed))in
    let* output=profile ~seconds:(fun output->output.Gpu.gpu_seconds) Gpu(fun()->kernel.run inputs)in
    if readback then profile Gpu_readback(fun()->Result.map(fun value->Some(Cpu value))(kernel.readback output))
    else Ok(Some(Gpu output))
  let try_display ?state ?resolve ?(reference=false) ?policy program ~live =
    let policy=Option.value ~default:(Domain.DLS.get Gpu.current_policy) policy in
    let cpu()=Ok None in
    if reference then cpu()else match Domain.DLS.get Gpu.current with None->cpu()|Some backend->
    match root_packed program with None->cpu()|Some (packed,readback)->
      display_packed ?state ?resolve ~backend ~policy ~approx:program.approx
        ~display:(match program.sink with Some(Display _)->true|_->false) ~profile:program.profile
        ~cached:(fun()->program.gpu_kernel) ~store:(fun k->program.gpu_kernel<-Some k)
        ~readback:(readback||program.sink=Some Sop_input) packed ~live
  (* A region of a draw program offered to the GPU as a mark attribute: a closure owns the kernels
     of at most 64 regions. [None] leaves the region to the CPU. *)
  let region_display ?profile ?(approx=W.Paths.empty) ?policy () =
    let kernels=ref[] in
    let rec take n=function []->[]|_ when n=0->[]|x::xs->x::take(n-1)xs in
    fun ?state ?resolve packed ~live->
      let policy=Option.value ~default:(Domain.DLS.get Gpu.current_policy) policy in
      match Domain.DLS.get Gpu.current with None->Ok None|Some backend->
      let find()=List.find_map(fun((_,p,_)as k)->if p==packed then Some k else None) !kernels in
      let result=display_packed ?state ?resolve ~backend ~policy ~approx ~display:true ~profile
        ~cached:find ~store:(fun((_,p,_)as k)->kernels:=k::take 63(List.filter(fun(_,q,_)->q!=p) !kernels))
        ~readback:false packed ~live in
      Result.map(Option.map(function Gpu v->v|Cpu _->assert false))result
  (* A Sop_input write in a cook may take its region from the device: pack the inputs here, dispatch
     and read back through the service, then copy the planes the writer asked for. The CPU region
     answers when the device declines, is unmeasured, or fails. *)
  let force_into ?state ?elems ?resolve ?(strict=false) ~column_source ~count ~outputs program ~live =
    let cpu()=force_into_cpu ?state ?elems ?resolve ~strict ~column_source ~count ~outputs program ~live in
    match program.sink,Array.length outputs,Gpu.available(),packed_root program with
    |Some Sop_input,1,Some backend,Some packed when Packed.elementwise packed->
        (match display_packed ?state ?resolve ~backend ~policy:(Domain.DLS.get Gpu.current_policy)
            ~approx:program.approx ~display:false ~profile:program.profile
            ~cached:(fun()->program.gpu_kernel) ~store:(fun k->program.gpu_kernel<-Some k)
            ~readback:true packed ~live with
         |Ok(Some(Cpu value))->
             let data,width=match value with
               |E.Float_array d->d,1|Vec2_array d->d,2|Vec3_array d->d,3|Vec4_array d->d,4|_->[||],0 in
             if width=0 || width<>Array.length outputs.(0) || Array.length data<>count*width then cpu()
             else begin
               Array.iteri(fun component plane->Option.iter(fun plane->
                 for i=0 to count-1 do plane.(i)<-data.(i*width+component) done)plane)outputs.(0);
               Ok(Some())
             end
         |Ok _|Error _->cpu())
    |_->cpu()
  let force_display ?state ?(reference=false) ?policy program ~live =
    Result.bind (try_display ?state ~reference ?policy program ~live) (function
      |Some value->Ok value
      |None->Result.map(fun value->Cpu value)(force ?state ~reference program ~live))
  (* A mark set's attributes, each computed at its own rate: a static attribute runs once
     and its value is kept; a fused region runs once per frame for all its outputs. *)
  type attribute_unit = { names : string list; fused : Packed.t option; rate : rate;
    mutable kept : E.value list option }
  type marks = { graph : t; programs : (string * program) list; units : attribute_unit list;
    profile : Profile.t option }
  let compile_marks ?profile ?(approx = W.Paths.empty) ?(count_source = fun _ _ -> None)
      ?(id = (["@marks"], [])) set attributes =
    let b = builder ~count_source W.Paths.empty W.Paths.empty in
    let args = List.mapi (fun i (name, v) -> {name; node = value b (child id (name ^ "#" ^ string_of_int i)) v}) attributes in
    match marks_of set args with
    | None -> Error (Flow.Diagnostic.error ~code:"E_DRAW_KIND" (set ^ " is not a mark set."))
    | Some kind ->
        let root = add b id Ty.drawing (Count.Data id) kind args Static in
        let ( let* ) = Result.bind in
        let* graph = optimize (finish b [|root|]) in
        let* programs = List.fold_right (fun (name, v) rest ->
          let* rest = rest in
          let* program = compile ?profile ~approx ~sink:(Display set) ~count_source v in
          Ok ((name, program) :: rest)) attributes (Ok []) in
        let node = graph.nodes.(graph.roots.(0)) in
        let edges = attribute_edges graph node in
        let units = List.fold_left (fun units ((a : attribute), i) ->
          if List.mem a.edge (List.concat_map (fun u -> u.names) units) then units else
          let shared = List.filter (fun ((_ : attribute), j) -> j = i) edges
            |> List.sort (fun ((a : attribute), _) ((b : attribute), _) -> compare a.output b.output) in
          let fused = match graph.nodes.(i).kind with
            | Kernel {body = Packed_map p; _} when List.length shared > 1 -> Some p
            | _ -> None in
          let names = if fused = None then [a.edge] else List.map (fun ((a : attribute), _) -> a.edge) shared in
          {names; fused; rate = graph.nodes.(i).rate; kept = None} :: units) [] edges |> List.rev in
        Ok {graph; programs; units; profile}
  let marks_graph m = m.graph
  let force_marks ?state m ~live =
    let state = match state with Some state -> state | None -> E.create_state () in
    let ( let* ) = Result.bind in
    let separate names = List.fold_right (fun name rest ->
      let* rest = rest in
      let* v = force ~state (List.assoc name m.programs) ~live in Ok (v :: rest)) names (Ok []) in
    let run u = match u.fused with
      | None -> separate u.names
      | Some packed ->
          (* A failure reruns the attributes one by one for the precise diagnostic. *)
          (match Packed.force_all ~state ?measure:(measure m.profile) packed ~live with
           | Ok values -> Ok values
           | Error _ -> separate u.names) in
    let* values = List.fold_right (fun u rest ->
      let* rest = rest in
      let* values = match u.kept with
        | Some values -> Ok values
        | None -> let* values = run u in
            if u.rate = Static then u.kept <- Some values; Ok values in
      Ok (List.combine u.names values @ rest)) m.units (Ok []) in
    Ok (List.map (fun (name, _) -> name, List.assoc name values) m.programs)

end

(* The device side of a static packed map: placed by the same measured cost as any readback, then
   read back once, so the walker receives an ordinary array (taken within a tolerance). *)
let () = gpu_bulk := fun ~approx residual ->
  match Domain.DLS.get Gpu.current with
  | None -> None
  | Some backend ->
      (match Packed.compile residual (E.Private.residual_view residual).term with
       | None -> None
       | Some packed ->
           (match Executor.display_packed ~backend ~policy:(Domain.DLS.get Gpu.current_policy) ~approx
                    ~display:false ~profile:None ~cached:(fun () -> None) ~store:ignore ~readback:true
                    packed ~live:(Frame_input.at_time 0.) with
            | Ok (Some (Executor.Cpu value)) -> Some value
            | Ok _ | Error _ -> None
            | exception (V.Fail _ | V.Diagnostic_fail _ | Invalid_argument _ | Not_found | Failure _) -> None))
