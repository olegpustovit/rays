type node_timing = {
  node_id : int;
  label : string;
  operation : string;
  input_points : int;
  points : int;
  seconds : float;
  cache_hit : bool;
}

type stats = {
  cooks : int;
  hits : int;
  misses : int;
  evictions : int;
  volatile_hits : int;
  volatile_misses : int;
  retained_entries : int;
  volatile_entries : int;
  retained_payload_bytes : int;
  last_node : node_timing option;
}

type output = {
  payload : Payload.t;
  instances : Rays_math.Mat4.t array option;
}

type delta = {
  source : int;
  positions : Rdk.Packed.Float3.t option;
  attributes : (Rdk.Attribute.owner * string * Rdk.Attribute.t option) list;
}
type entry = {
  output : output;
  components : (int * int) list;
  delta : delta option;
}

type cache_event =
  | Read of int option * string * entry option
  | Write of int option * string * entry
  | Materialized_read of output * Rdk.Geometry.t option
  | Materialized_write of output * Rdk.Geometry.t
  | Time of node_timing

module Entry_cache = Lru.Make (String)

(* Cook outputs share packed payload planes; the pool counts each plane once. *)
type payload_pool = { refs : (int, int * int) Hashtbl.t; mutable bytes : int }

type t = {
  max_entries : int;
  max_payload_bytes : int;
  cache : entry Entry_cache.t;
  payload : payload_pool;

  mutable cooks : int;
  mutable hits : int;
  mutable misses : int;
  mutable volatile : int -> bool;
  slots : (int, string * entry) Hashtbl.t;  (* volatile node id -> latest *)
  mutable volatile_hits : int;
  mutable volatile_misses : int;
  mutable evictions : int;
  mutable last_node : node_timing option;
  times : (int, node_timing) Hashtbl.t;
  times_lock : Mutex.t;
  mutable closed : bool;
  mutable materialized : (output * Rdk.Geometry.t) list;  (* see [input_geometry] *)
  mutable journal : cache_event list option;
  mutable parallel_override : bool option;
  mutable fanouts : int;
}

let release_components payload components =
  List.iter (fun (id, bytes) ->
    match Hashtbl.find_opt payload.refs id with
    | None -> ()
    | Some (_, 1) ->
        Hashtbl.remove payload.refs id;
        payload.bytes <- payload.bytes - bytes
    | Some (_, references) ->
        Hashtbl.replace payload.refs id (bytes, references - 1))
    components

let create ~max_entries ~max_payload_bytes =
  if max_entries < 0 then Error "Session.create: max_entries must be non-negative"
  else if max_payload_bytes < 0 then
    Error "Session.create: max_payload_bytes must be non-negative"
  else
    let payload = { refs = Hashtbl.create (min (max_entries * 4) 4096); bytes = 0 } in
    Ok {
    max_entries; max_payload_bytes;
    cache = Entry_cache.create max_entries
        ~release:(fun _ entry -> release_components payload entry.components);
    payload;

    cooks = 0; hits = 0; misses = 0; evictions = 0;
    volatile = (fun _ -> false); slots = Hashtbl.create 16; volatile_hits = 0; volatile_misses = 0;
    last_node = None; times = Hashtbl.create 64; times_lock = Mutex.create (); closed = false; materialized = [];
    journal = None; parallel_override = None; fanouts = 0;
  }

let insert session key entry =
  (match session.journal with None -> () | Some events ->
    session.journal <- Some (Write (None, key, entry) :: events));
  if session.max_entries > 0 then begin
    let components = Payload.payload_components entry.output.payload in
    let payload = session.payload in
    List.iter (fun (id, bytes) ->
      match Hashtbl.find_opt payload.refs id with
      | None ->
          Hashtbl.add payload.refs id (bytes, 1);
          payload.bytes <- payload.bytes + bytes
      | Some (known_bytes, references) ->
          Hashtbl.replace payload.refs id (known_bytes, references + 1))
      components;
    let before = Entry_cache.length session.cache
    and replaced = Option.is_some (Entry_cache.peek session.cache key) in
    Entry_cache.add session.cache key { entry with components };
    while payload.bytes > session.max_payload_bytes
          && Entry_cache.drop_oldest session.cache do () done;
    session.evictions <- session.evictions + before
      + (if replaced then 0 else 1) - Entry_cache.length session.cache
  end

let lookup session ~volatile node key =
  let result = if not volatile then
    (match Entry_cache.find session.cache key with
     | entry -> Some entry | exception Not_found -> None)
  else match Hashtbl.find_opt session.slots (Node.id node) with
    | Some (latest, output) when String.equal latest key -> Some output
    | _ -> None in
  (match session.journal with None -> () | Some events ->
    session.journal <- Some (Read ((if volatile then Some (Node.id node) else None), key, result) :: events));
  result

let store_slot session id key entry =
  (match session.journal with None -> () | Some events ->
    session.journal <- Some (Write (Some id, key, entry) :: events));
  if session.max_entries > 0 then begin
    (* bounded: one slot per node id, dropped wholesale past max_entries *)
    if Hashtbl.length session.slots >= session.max_entries then
      Hashtbl.reset session.slots;
    Hashtbl.replace session.slots id (key, entry)
  end

let store_volatile session node key entry = store_slot session (Node.id node) key entry

let set_volatile session predicate =
  session.volatile <- predicate;
  Hashtbl.filter_map_inplace (fun id slot ->
    if predicate id then Some slot else None) session.slots

(* An opaque, unambiguous cache identity: integers are fixed-width binary and
   every variable-length string is length-prefixed, so no field boundary can
   shift. It avoids [Printf] and [string_of_int] on this per-node, per-cook
   path. *)
let selective node inputs =
  let facts = Node.facts node in
  Array.length inputs = 1 && (match inputs.(0) with Payload.Geometry _ -> true | Image _ | Kernel _ -> false)
  && Node.Private.expand node = None
  && facts.topology = Node.Preserved && not (List.mem "*" facts.reads || List.mem "*" facts.writes)
  && (facts.cook_mode = Node.Duplicate_input 0 || facts.cook_mode = Node.Passthrough 0)

let cache_key node context inputs =
  let parameters = Node.Private.cache_parameters node
  and parameter_key = Node.parameter_key node
  and facts_key = Node.Private.cache_facts node in
  let buffer = Buffer.create (64 + String.length parameters
      + String.length parameter_key + String.length facts_key + (8 * Array.length inputs)) in
  let add_int value = Buffer.add_int64_le buffer (Int64.of_int value) in
  let add_sized value =
    add_int (String.length value); Buffer.add_string buffer value in
  add_int (Node.id node);
  add_int (Node.version node);
  add_sized (Node.operation node);
  add_sized parameters;
  add_sized parameter_key;
  add_int (Array.length inputs);
  let facts = Node.facts node in
  add_sized facts_key;
  if not (selective node inputs) then begin
    add_int 0;
    Array.iter (fun payload -> add_int (Payload.data_id payload)) inputs
  end else begin
    add_int 1;
    let geometry = Result.get_ok (Payload.geometry inputs.(0)) in
    add_int (Rdk.Topology.data_id (Rdk.Geometry.topology geometry));
    add_int (if List.mem "P" facts.reads then Rdk.Packed.Float3.data_id (Rdk.Geometry.positions geometry) else 0);
    let attributes = List.filter (fun attribute -> List.mem (Rdk.Attribute.name attribute) facts.reads)
      (Rdk.Geometry.attributes geometry) in
    add_int (List.length attributes);
    List.iter (fun attribute -> add_int (Rdk.Attribute.data_id attribute)) attributes;
    (* Presence and order affect set-initial, normal removal and output order.
       Unread payload IDs remain absent from the key. *)
    let layout = Rdk.Geometry.attributes geometry in
    add_int (List.length layout);
    List.iter (fun attribute ->
      add_int (match Rdk.Attribute.owner attribute with Point -> 0 | Vertex -> 1 | Primitive -> 2 | Detail -> 3);
      add_sized (Rdk.Attribute.name attribute)) layout;
    let groups = Rdk.Geometry.groups geometry and edges = Rdk.Geometry.edge_groups geometry in
    add_int (List.length groups); List.iter (fun group -> add_int (Rdk.Group.data_id group)) groups;
    add_int (List.length edges); List.iter (fun group -> add_int (Rdk.Edge_group.data_id group)) edges
  end;
  Buffer.add_string buffer
    (Context.cache_projection (Node.dependencies node) context);
  Buffer.contents buffer

exception Invalid_facts of string

let refuse_facts message = raise_notrace (Invalid_facts message)

let node_result f =
  try f () with
  | Invalid_facts message -> Error (Diagnostic.error ~code:"E_NODE_FACTS" message)
  | exn -> Error (Diagnostic.error ~code:"uncaught_node_exception"
      ~cause:(Printexc.to_string exn)
      "a procedural node raised an exception while cooking")

let delta node inputs (cooked : Node.Private.cooked) =
  if not (selective node inputs) then None else
  let source = Result.get_ok (Payload.geometry inputs.(0)) in
  let geometry = match cooked.payload with Geometry geometry -> geometry
    | Image _ | Kernel _ -> refuse_facts "component-cached geometry nodes must return geometry" in
  if Option.is_some cooked.instances then
    refuse_facts "component-cached nodes must return unpacked geometry";
  if Rdk.Geometry.topology source != Rdk.Geometry.topology geometry then
    refuse_facts "Preserved topology was replaced";
  let module G = Rdk.Geometry in
  let module A = Rdk.Attribute in
  if List.map Rdk.Group.data_id (G.groups source) <> List.map Rdk.Group.data_id (G.groups geometry)
    || List.map Rdk.Edge_group.data_id (G.edge_groups source) <>
       List.map Rdk.Edge_group.data_id (G.edge_groups geometry) then
    refuse_facts "component-cached nodes must preserve groups and edge groups";
  let before = Hashtbl.create 8 and after = Hashtbl.create 8 in
  let key attribute = A.owner attribute, A.name attribute in
  List.iter (fun attribute -> Hashtbl.add before (key attribute) attribute) (G.attributes source);
  List.iter (fun attribute -> Hashtbl.add after (key attribute) attribute) (G.attributes geometry);
  let attributes = ref [] in
  List.iter (fun attribute ->
    let owner, name = key attribute in
    let changed = match Hashtbl.find_opt before (owner, name) with
      | Some previous -> A.data_id previous <> A.data_id attribute | None -> true in
    if changed then begin
      if not (List.mem name (Node.facts node).writes) then
        refuse_facts ("undeclared write " ^ name);
      attributes := (owner, name, Some attribute) :: !attributes
    end) (G.attributes geometry);
  List.iter (fun attribute ->
    let owner, name = key attribute in
    if not (Hashtbl.mem after (owner, name)) then begin
      if not (List.mem name (Node.facts node).writes) then
        refuse_facts ("undeclared deletion " ^ name);
      attributes := (owner, name, None) :: !attributes
    end) (G.attributes source);
  let positions = if G.positions source == G.positions geometry then None else begin
    if not (List.mem "P" (Node.facts node).writes) then refuse_facts "undeclared P write";
    Some (G.positions geometry)
  end in
  Some {source = G.data_id source; positions; attributes = List.rev !attributes}

let refresh inputs entry =
  let payload, delta = match entry.delta with
    | Some delta when delta.source <> Payload.data_id inputs.(0) ->
        let source = Result.get_ok (Payload.geometry inputs.(0)) in
        let module G = Rdk.Geometry in
        let module A = Rdk.Attribute in
        let values = Hashtbl.create 8 in
        List.iter (fun attribute -> Hashtbl.add values (A.owner attribute, A.name attribute) attribute)
          (G.attributes source);
        List.iter (fun (owner, name, value) -> match value with
          | Some attribute -> Hashtbl.replace values (owner, name) attribute
          | None -> Hashtbl.remove values (owner, name)) delta.attributes;
        let attributes = List.map (fun attribute ->
          Hashtbl.find values (A.owner attribute, A.name attribute))
            (G.attributes (Result.get_ok (Payload.geometry entry.output.payload))) in
        let geometry = G.create ~positions:(Option.value ~default:(G.positions source) delta.positions)
          ~topology:(G.topology source) ~attributes
          ~groups:(G.groups source) ~edge_groups:(G.edge_groups source) ()
          |> function Ok geometry -> geometry | Error error ->
              refuse_facts ("cannot refresh cached components: " ^ error) in
        Payload.Geometry geometry, Some {delta with source = G.data_id source}
    | _ -> entry.output.payload, entry.delta in
  if payload == entry.output.payload then entry
  else {entry with output = {entry.output with payload}; delta}

let timing node ~geometries ~points ~seconds ~cache_hit = {
  node_id = Node.id node;
  label = Node.label node;
  operation = Node.operation node;
  input_points = Array.fold_left (fun n payload -> n + Payload.element_count payload) 0 geometries;
  points;
  seconds;
  cache_hit;
}

(* a cache hit keeps the node's last real cook time; bounded: a full table starts over *)
let times_capacity = 4096
let record_time session sample =
  (match session.journal with None -> () | Some events ->
    session.journal <- Some (Time sample :: events));
  Mutex.protect session.times_lock (fun () ->
    if Hashtbl.length session.times >= times_capacity then Hashtbl.reset session.times;
    Hashtbl.replace session.times sample.node_id sample)

let node_seconds session id =
  Mutex.protect session.times_lock (fun () ->
    Option.map (fun sample -> sample.seconds) (Hashtbl.find_opt session.times id))

let cancellation_error node =
  Diagnostic.error ~code:"cancelled" "procedural cook was cancelled"
  |> Diagnostic.prepend_trace (Node.trace node)

(* A packed input reaches its consumer materialized (the explicit boundary),
   once per packed output: the copy keeps a stable data id, so downstream
   cache keys still hit. ponytail: the last 8 packed outputs are kept; an
   LRU keyed by output identity if graphs pack more than that. *)
let input_geometry session (output : output) = match output.payload, output.instances with
  | (Payload.Image _ | Payload.Kernel _), _ | _, None -> Ok output.payload
  | Payload.Geometry source, Some transforms ->
      let existing = List.assq_opt output session.materialized in
      (match session.journal with None -> () | Some events ->
        session.journal <- Some (Materialized_read (output, existing) :: events));
      match existing with
      | Some geometry -> Ok (Payload.Geometry geometry)
      | None ->
          match Rdk.Instance_copy.materialize_instances ~transforms source with
          | Error error -> Error (Diagnostic.error ~code:(Rdk.Error.code error)
              ~cause:(Rdk.Error.to_string error) "packed instances could not be materialized")
          | Ok geometry ->
              (match session.journal with None -> () | Some events ->
                session.journal <- Some (Materialized_write (output, geometry) :: events));
              session.materialized <- (output, geometry)
                :: List.take 7 session.materialized;
              Ok (Payload.Geometry geometry)

(* [memo] holds this cook's results by node id so a node reachable through
   several paths is evaluated once; the physical check guards reused ids. *)
let memo_find memo node =
  List.find_opt (fun (seen, _) -> seen == node) (Hashtbl.find_all memo (Node.id node))

(* Snapshot metadata, not payload planes. Exact CLOCK touch bits matter when
   an earlier branch evicts an entry a later branch initially found. *)
let snapshot session =
  let payload = {refs=Hashtbl.copy session.payload.refs; bytes=session.payload.bytes} in
  {session with cache=Entry_cache.copy
      ~release:(fun _ entry -> release_components payload entry.components) session.cache;
    payload; slots=Hashtbl.copy session.slots; times=Hashtbl.create 16;
    times_lock=Mutex.create (); cooks=0;hits=0;misses=0;evictions=0;
    volatile_hits=0;volatile_misses=0;last_node=None;
    journal=None;parallel_override=Some false;fanouts=0}

let replay_event session = function
  | Read (volatile, key, expected) ->
      let observed = match volatile with
        | None -> (try Some (Entry_cache.find session.cache key) with Not_found -> None)
        | Some id -> (match Hashtbl.find_opt session.slots id with
            | Some (stored, entry) when stored=key -> Some entry | _ -> None) in
      (match observed, expected with
       | None,None -> true
       | Some a,Some b -> a.output==b.output && a.delta==b.delta
       | _ -> false)
  | Write (None,key,entry) -> insert session key entry; true
  | Write (Some id,key,entry) -> store_slot session id key entry; true
  | Materialized_read (output, expected) ->
      (match List.assq_opt output session.materialized,expected with
       | None,None -> true | Some a,Some b -> a==b | _ -> false)
  | Materialized_write (output, geometry) ->
      session.materialized <- (output,geometry) :: List.take 7 session.materialized; true
  | Time sample -> record_time session sample; true

let selected_inputs node =
  let inputs=Node.Private.input_array node in
  match Node.Private.input_policy node with All -> inputs | Only i -> [|inputs.(i)|]

let branch_min_seconds = 0.002
let branch_min_points = 10_000

type cached_prediction = Known of output | Deferred_refresh | Missing

let rec cached_output predictions memo session context node =
  match memo_find predictions node with
  | Some(_,output) -> output
  | None ->
  let output=match memo_find memo node with
  | Some (_,Ok output) -> Known output | Some (_,Error _) -> Missing
  | None ->
      let inputs=selected_inputs node in
      let input_predictions=Array.map(cached_output predictions memo session context) inputs in
      let values=Array.map(function Known output ->
          (match output.payload,output.instances with
           | (Payload.Image _ | Payload.Kernel _),_ | _,None -> Some output.payload
           | Geometry _,Some _ -> Option.map(fun geometry -> Payload.Geometry geometry)
               (List.assq_opt output session.materialized))
        | Deferred_refresh | Missing -> None) input_predictions in
      if Array.exists ((=) Deferred_refresh) input_predictions then Deferred_refresh
      else if Array.exists Option.is_none values then Missing else
      let geometries=Array.map Option.get values in
      let key=cache_key node context geometries in
      let found=if session.volatile(Node.id node) then
        (match Hashtbl.find_opt session.slots(Node.id node) with
         |Some(stored,entry) when stored=key -> Some entry |_->None)
        else Entry_cache.peek session.cache key in
      (match found with
       | None -> Missing
       | Some entry -> match entry.delta with
           |Some delta when delta.source<>Payload.data_id geometries.(0) -> Deferred_refresh
           |_->Known entry.output) in
  Hashtbl.add predictions (Node.id node) (node,output);
  output

(* The root's own time excludes its inputs. Placement budgets the uncached
   subtree, otherwise a cheap final operator hides an expensive branch. *)
let costly predictions memo session context node = match session.parallel_override with
  | Some true -> true | Some false -> false
  | None ->
      let visited=Hashtbl.create 16 and seconds=ref 0. and large=ref false in
      let enough () = !seconds>=branch_min_seconds || !large in
      let rec visit node =
        if not(enough()) && Option.is_none(memo_find visited node)
          && Option.is_none(memo_find memo node)
          && cached_output predictions memo session context node=Missing then begin
          Hashtbl.add visited (Node.id node) (node,());
          (match node_seconds session (Node.id node) with
           | Some elapsed -> seconds:= !seconds+.elapsed
           | None -> large:=Option.fold ~none:false ~some:(fun entry ->
               Payload.element_count entry.output.payload>=branch_min_points)
               (Entry_cache.find_first session.cache(fun key _ -> String.length key>=8 &&
                 String.get_int64_le key 0=Int64.of_int(Node.id node))));
          Array.iter visit (selected_inputs node)
        end in
      visit node;enough()

let rec evaluate memo session context node =
  match memo_find memo node with
  | Some (_, result) -> result
  | None ->
      let result = evaluate_uncached memo session context node in
      Hashtbl.add memo (Node.id node) (node, result);
      result

(* Complete the same sequential DFS prefix that reaches every shared node.
   Once it is memoized, the remaining branch DAGs have disjoint mutable state.
   ponytail: opaque expansions stay sequential until their concrete roots
   exist; broaden scheduling only if nested-zone measurements justify it. *)
and shared_prefix memo session context roots =
  let counts = Hashtbl.create 32 and opaque = ref false in
  Array.iter (fun root ->
    let visited = Hashtbl.create 16 in
    let rec visit node =
      if Option.is_none (memo_find memo node) && Option.is_none (memo_find visited node) then begin
        Hashtbl.add visited (Node.id node) (node, ());
        (match memo_find counts node with
         | Some (_, count) -> incr count
         | None -> Hashtbl.add counts (Node.id node) (node, ref 1));
        if Option.is_some (Node.Private.expand node) then opaque := true;
        Array.iter visit (selected_inputs node)
      end in
    visit root) roots;
  if !opaque then None else
  let pending = ref (Hashtbl.fold (fun _ (node,count) nodes ->
    if !count > 1 then node :: nodes else nodes) counts []) in
  (* ponytail: shared-node membership scans this coarse DAG frontier; use a
     physical-node set if large shared frontiers dominate cook measurements. *)
  let rec contains node =
    Option.is_none (memo_find memo node) &&
    (List.exists ((==) node) !pending || Array.exists contains (selected_inputs node)) in
  let rec prefix node =
    if !pending=[] || Option.is_some (memo_find memo node) then Ok ()
    else if List.exists ((==) node) !pending || not (contains node) then
      Result.map (fun _ -> pending := List.filter (fun shared ->
        Option.is_none (memo_find memo shared)) !pending) (evaluate memo session context node)
    else
      let children=selected_inputs node in
      let rec go i =
        if !pending=[] || i=Array.length children then Ok ()
        else match prefix children.(i) with
          | Error error -> Error (Diagnostic.prepend_trace (Node.trace node) error)
          | Ok () -> go (i+1) in
      go 0 in
  let rec go i =
    if !pending=[] || i=Array.length roots then Ok ()
    else Result.bind (prefix roots.(i)) (fun () -> go (i+1)) in
  Some (go 0)

(* Workers mutate only copied cache metadata and disjoint memo tables. Reads
   are validated against the current parent before any journal is applied;
   a prior branch's eviction may require a sequential recook. This keeps
   CLOCK order, retention and counters identical to input-order execution. *)
and evaluate_many memo session context roots =
  let count=Array.length roots in
  let candidates () =
    let predictions=Hashtbl.create 16 in
    Array.to_list (Array.mapi (fun i node ->
      if Option.is_none(memo_find memo node)
        && costly predictions memo session context node
        && cached_output predictions memo session context node=Missing
      then Some i else None) roots) |> List.filter_map Fun.id in
  let sequential () =
    let outputs=Array.make count None in
    let rec go i =
      if i=count then Ok (Array.map Option.get outputs)
      else Result.bind (evaluate memo session context roots.(i)) (fun output ->
        Result.bind (input_geometry session output) (fun geometry ->
          outputs.(i)<-Some(output,geometry); go(i+1))) in
    go 0 in
  if count<2 || Context.domains context<2 || session.parallel_override=Some false
    || List.length(candidates())<2 then sequential () else
  match shared_prefix memo session context roots with
  | None -> sequential ()
  | Some (Error error) -> Error error
  | Some (Ok ()) ->
      let indices=Array.of_list(candidates()) in
      if Array.length indices<2 then sequential () else begin
        session.fanouts<-session.fanouts+1;
        let transactions=Array.make count None in
        let workers=Array.map(fun _ -> snapshot session,Hashtbl.copy memo) indices in
        Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(Array.length indices-1)
          (fun worker_index ->
            let i=indices.(worker_index) in
            let local,local_memo=workers.(worker_index) in
            local.journal<-Some [];
            let result=Result.bind (evaluate local_memo local context roots.(i)) (fun output ->
              Result.map (fun geometry -> output,geometry) (input_geometry local output)) in
            transactions.(i)<-Some(local,local_memo,result));
        let outputs=Array.make count None in
        let rec cancelled_error i =
          if i=count then Error(cancellation_error roots.(0))
          else match transactions.(i) with
            | Some(_,_,Error error) -> Error error
            | Some(_,_,Ok _) -> cancelled_error(i+1)
            | None -> Error(cancellation_error roots.(i)) in
        let rec join i =
          if i=count then Ok (Array.map Option.get outputs)
          else if Context.cancelled context then cancelled_error i else
          let result=match transactions.(i) with
            | None -> Result.bind(evaluate memo session context roots.(i)) (fun output ->
                Result.map(fun geometry -> output,geometry)(input_geometry session output))
            | Some(local,local_memo,result) ->
                let events=List.rev(Option.get local.journal) in
                let compatible=Hashtbl.fold(fun _ (node,worker_result) compatible ->
                    compatible && match memo_find memo node with
                    | None -> true | Some(_,parent_result) -> worker_result==parent_result)
                    local_memo true in
                let probe=snapshot session in
                if compatible && List.for_all(replay_event probe) events then begin
                  List.iter(fun event -> ignore(replay_event session event)) events;
                  session.cooks<-session.cooks+local.cooks;
                  session.hits<-session.hits+local.hits;
                  session.misses<-session.misses+local.misses;
                  session.volatile_hits<-session.volatile_hits+local.volatile_hits;
                  session.volatile_misses<-session.volatile_misses+local.volatile_misses;
                  (match local.last_node with None -> () | Some _ -> session.last_node<-local.last_node);
                  Hashtbl.iter(fun id (node,value) ->
                    if Option.is_none(memo_find memo node) then Hashtbl.add memo id (node,value)) local_memo;
                  result
                end else Result.bind(evaluate memo session context roots.(i)) (fun output ->
                  Result.map(fun geometry -> output,geometry)(input_geometry session output)) in
          Result.bind result (fun output -> outputs.(i)<-Some output;join(i+1)) in
        join 0
      end

(* A zone expands on its caller; its concrete element roots use the same
   input-order join as ordinary independent branches. *)
and cook_node memo session context node geometries =
  match Node.Private.expand node with
  | None -> Node.Private.cook node context geometries
  | Some expand ->
      match expand context (Node.Private.input_array node) geometries with
      | Error _ as error -> error
      | Ok roots ->
          (match evaluate_many memo session context roots with
           | Error error -> Error (Diagnostic.prepend_trace (Node.trace node) error)
           | Ok outputs -> Node.Private.cook node context (Array.map snd outputs))

and evaluate_uncached memo session context node =
  if Context.cancelled context then Error (cancellation_error node)
  else
    match evaluate_many memo session context (selected_inputs node) with
    | Error error -> Error (Diagnostic.prepend_trace (Node.trace node) error)
    | Ok outputs ->
        let geometries = Array.map snd outputs in
        (* Zero-entry sessions need only the per-cook memo. Speculative worker
           journals still need semantic keys for ordered parent replay. *)
        let cache_active = session.max_entries > 0 || Option.is_some session.journal in
        let key = if cache_active then cache_key node context geometries else "" in
        let volatile = session.volatile (Node.id node) in
        match (if cache_active then lookup session ~volatile node key else None) with
        | Some entry ->
            (match node_result (fun () -> Ok (refresh geometries entry)) with
            | Error error -> Error (Diagnostic.prepend_trace (Node.trace node) error)
            | Ok refreshed ->
            if refreshed != entry then
              (if volatile then store_volatile session node key refreshed else insert session key refreshed);
            session.hits <- session.hits + 1;
            if volatile then session.volatile_hits <- session.volatile_hits + 1;
            session.last_node <- Some (timing node ~geometries
              ~points:(Payload.element_count refreshed.output.payload) ~seconds:0. ~cache_hit:true);
            Ok refreshed.output)
        | None ->
            session.misses <- session.misses + 1;
            if volatile then session.volatile_misses <- session.volatile_misses + 1;
            session.cooks <- session.cooks + 1;
            let started = Unix.gettimeofday () in
            let cooked =
              node_result (fun () ->
                match cook_node memo session context node geometries with
                | Error _ as error -> error
                | Ok cooked -> Ok (cooked, delta node geometries cooked))
            in
            let seconds = max 0. (Unix.gettimeofday () -. started) in
            let points = match cooked with Ok (output, _) -> Payload.element_count output.payload | Error _ -> 0 in
            let sample = timing node ~geometries ~points ~seconds ~cache_hit:false in
            session.last_node <- Some sample;
            record_time session sample;
            match cooked with
            | Error error -> Error (Diagnostic.prepend_trace (Node.trace node) error)
            | Ok _ when Context.cancelled context -> Error (cancellation_error node)
            | Ok (cooked, delta) ->
                let output = { payload = cooked.payload; instances = cooked.instances } in
                let entry = {output; components = []; delta} in
                if cache_active then
                  (if volatile then store_volatile session node key entry
                   else insert session key entry);
                Ok output

let cook session ~context node =
  if session.closed then
    Error (Diagnostic.error ~code:"session_closed"
      "cannot cook with a closed procedural session"
      |> Diagnostic.prepend_trace (Node.trace node))
  else
    Parallel.run ~domains:(Context.domains context) (fun () ->
      match evaluate (Hashtbl.create 64) session context node with
      | Error _ as error -> error
      | Ok output -> Ok output)

let stats session = {
  cooks = session.cooks;
  hits = session.hits;
  misses = session.misses;
  evictions = session.evictions;
  volatile_hits = session.volatile_hits;
  volatile_misses = session.volatile_misses;
  retained_entries = Entry_cache.length session.cache;
  volatile_entries = Hashtbl.length session.slots;
  retained_payload_bytes = session.payload.bytes;
  last_node = session.last_node;
}

let clear session =
  Mutex.protect session.times_lock (fun () -> Hashtbl.reset session.times);
  ();
  Entry_cache.clear session.cache;
  Hashtbl.reset session.slots;
  Hashtbl.clear session.payload.refs;
  session.payload.bytes <- 0

let close session =
  if not session.closed then begin
    clear session;
    session.closed <- true
  end

module Private = struct
  let set_parallel_override session value = session.parallel_override <- value
  let fanouts session = session.fanouts
  let cache_keys session =
    let keys=ref [] in
    Entry_cache.iter session.cache(fun key _ -> keys:=key::!keys);
    List.sort String.compare !keys
  let clear_cache_keep_timings session =
    Entry_cache.clear session.cache;
    Hashtbl.reset session.slots;
    Hashtbl.reset session.payload.refs;
    session.payload.bytes<-0;
    session.materialized<-[]
end
