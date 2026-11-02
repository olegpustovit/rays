let cache = Mem.cache
let plan = Mem.plan
let document = Mem.document

type shed = Home | Locked of Mutex.t | Never

(* What may touch a table at a frame boundary. A table's users already follow one rule, the one
   [shed] names, so the retreat follows it too: never a lock on the frame path that was not there. *)
let guarded shed drop =
  match shed with
  | Never -> fun () -> false
  | Locked lock ->
      fun () ->
        Mutex.try_lock lock
        && Fun.protect ~finally:(fun () -> Mutex.unlock lock) drop
  | Home ->
      let home = Domain.self () in
      fun () -> Domain.self () = home && drop ()

(* The retreat holds a table weakly: a table made per call dies with its caller, and what it still
   charged leaves with it. *)
let register owner shed ~rebuild ~charged table ~bytes ~last_use ~drop =
  if shed <> Never then begin
    let weak = Weak.create 1 in
    Weak.set weak 0 (Some table);
    let with_table default f =
      match Weak.get weak 0 with Some table -> f table | None -> default
    in
    Mem.register_evictor owner ~rebuild
      ~held:(fun () -> with_table 0 bytes)
      ~last_use:(fun () -> with_table 0 last_use)
      ~live:(fun () -> Weak.check weak 0)
      ~forget:(fun () ->
        Mem.release owner !charged;
        charged := 0)
      (guarded shed (fun () -> with_table false drop))
  end

module Make (K : Hashtbl.HashedType) = struct
  module Table = Hashtbl.Make (K)

  (* A circular ring: [oldest.older] is the newest entry. A hit only sets
     [touched] (one immediate store, no write barrier); eviction walks from
     the oldest and gives touched entries a second chance by moving them to
     the newest position, so the victim is still the least recently used
     untouched entry (CLOCK). *)
  type 'v node = {
    key : K.t;
    value : 'v;
    size : int;
    mutable touched : bool;
    mutable newer : 'v node;
    mutable older : 'v node;
  }

  type 'v t = {
    capacity : int;
    byte_capacity : int;
    owner : Mem.owner;
    charges : bool;  (** a {!copy} shares its source's values and charges nothing *)
    evictable : K.t -> 'v -> bool;
    release : K.t -> 'v -> unit;
    table : 'v node Table.t;
    (* [oldest] holds one node or nothing; mutating the cell instead of
       rebinding an option keeps every hit allocation-free. *)
    mutable oldest : 'v node array;
    mutable last_use : int;
    charged : int ref;  (** bytes on the table; outlives it so the ledger can be told it died *)
  }

  let charge_owner t size =
    t.charged := !(t.charged) + size;
    if t.charges then Mem.charge t.owner size

  let release_owner t size =
    t.charged := !(t.charged) - size;
    if t.charges then Mem.release t.owner size

  let unlink t node =
    if node.newer == node then t.oldest <- [||]
    else begin
      node.older.newer <- node.newer;
      node.newer.older <- node.older;
      if t.oldest.(0) == node then t.oldest.(0) <- node.newer
    end

  let link_newest t node =
    if Array.length t.oldest = 0 then begin
      node.newer <- node;
      node.older <- node;
      t.oldest <- [| node |]
    end
    else begin
      let oldest = t.oldest.(0) in
      let newest = oldest.older in
      node.older <- newest;
      node.newer <- oldest;
      newest.newer <- node;
      oldest.older <- node
    end

  let move_newest t node =
    if t.oldest.(0).older != node then begin
      unlink t node;
      link_newest t node
    end

  let drop t node =
    unlink t node;
    Table.remove t.table node.key;
    release_owner t node.size;
    t.release node.key node.value

  let over t = Table.length t.table > t.capacity || !(t.charged) > t.byte_capacity

  (* One lap at most: a touched or pinned entry moves to newest and loses
     its touch; the first untouched evictable entry is the victim. *)
  let victim t =
    if Array.length t.oldest = 0 then None
    else
      let rec lap remaining node =
        if remaining = 0 then None
        else if node.touched || not (t.evictable node.key node.value) then begin
          let next = node.newer in
          node.touched <- false;
          move_newest t node;
          if next == node then None else lap (remaining - 1) next
        end
        else Some node
      in
      lap (Table.length t.table) t.oldest.(0)

  let rec evict t =
    if over t then
      match victim t with
      | None -> ()
      | Some node ->
          drop t node;
          evict t

  let find t key =
    let node = Table.find t.table key in
    node.touched <- true;
    t.last_use <- Mem.epoch ();
    node.value

  let find_opt t key =
    match Table.find_opt t.table key with
    | None -> None
    | Some node ->
        node.touched <- true;
        t.last_use <- Mem.epoch ();
        Some node.value

  let peek t key =
    match Table.find_opt t.table key with None -> None | Some node -> Some node.value

  let add t ?(bytes = 0) key value =
    (* The same value again is its size changing, not a value leaving: it is not released. *)
    (match Table.find_opt t.table key with
    | Some old when old.value == value ->
        unlink t old;
        Table.remove t.table old.key;
        release_owner t old.size
    | Some old -> drop t old
    | None -> ());
    let rec node = { key; value; size = bytes; touched = false; newer = node; older = node } in
    Table.replace t.table key node;
    link_newest t node;
    t.last_use <- Mem.epoch ();
    charge_owner t bytes;
    evict t

  let remove t key = match Table.find_opt t.table key with None -> () | Some node -> drop t node

  let take t key =
    match Table.find_opt t.table key with
    | None -> None
    | Some node ->
        unlink t node;
        Table.remove t.table node.key;
        release_owner t node.size;
        Some node.value

  let drop_oldest t =
    match victim t with
    | None -> false
    | Some node ->
        drop t node;
        true

  let make ~owner ~charges ~byte_capacity ~evictable ~release capacity =
    {
      capacity;
      byte_capacity;
      owner;
      charges;
      evictable;
      release;
      table = Table.create (max 1 (min 1024 capacity));
      oldest = [||];
      last_use = 0;
      charged = ref 0;
    }

  let create ~owner ?(shed = Home) ?(rebuild = 1.) ?(charges = true) ?(byte_capacity = max_int)
      ?(evictable = fun _ _ -> true) ?(release = fun _ _ -> ()) capacity =
    let t = make ~owner ~charges ~byte_capacity ~evictable ~release capacity in
    register owner shed ~rebuild ~charged:t.charged t
      ~bytes:(fun t -> !(t.charged))
      ~last_use:(fun t -> t.last_use)
      ~drop:drop_oldest;
    t

  let find_first t f =
    if Array.length t.oldest = 0 then None
    else
      let oldest = t.oldest.(0) in
      let rec go node =
        if f node.key node.value then Some node.value
        else if node.newer == oldest then None
        else go node.newer
      in
      go oldest

  let iter t f =
    if Array.length t.oldest > 0 then begin
      let oldest = t.oldest.(0) in
      let rec go node =
        let next = node.newer in
        f node.key node.value;
        if next != oldest then go next
      in
      go oldest
    end

  let length t = Table.length t.table

  let copy ~release t =
    let result =
      make ~owner:t.owner ~charges:false ~byte_capacity:t.byte_capacity ~evictable:t.evictable
        ~release t.capacity
    in
    if Array.length t.oldest > 0 then begin
      let oldest = t.oldest.(0) in
      let rec go source =
        let rec node =
          {
            key = source.key;
            value = source.value;
            size = source.size;
            touched = source.touched;
            newer = node;
            older = node;
          }
        in
        Table.add result.table node.key node;
        link_newest result node;
        charge_owner result node.size;
        if source.newer != oldest then go source.newer
      in
      go oldest
    end;
    result

  let bytes t = !(t.charged)

  let rec clear t =
    if Array.length t.oldest > 0 then begin
      drop t t.oldest.(0);
      clear t
    end
end

module Identity_list = struct
  type ('key, 'value) t = {
    capacity : int;
    owner : Mem.owner;
    charges : bool;
    equal : 'key -> 'key -> bool;
    release : 'key -> 'value -> unit;
    mutable entries : ('key * 'value * int) list;
    mutable last_use : int;
    charged : int ref;
  }

  let charge t size =
    t.charged := !(t.charged) + size;
    if t.charges then Mem.charge t.owner size

  let discharge t size =
    t.charged := !(t.charged) - size;
    if t.charges then Mem.release t.owner size

  let drop_oldest t =
    match List.rev t.entries with
    | [] -> false
    | (key, value, size) :: rest ->
        t.entries <- List.rev rest;
        discharge t size;
        t.release key value;
        true

  let create ~owner ?(shed = Home) ?(rebuild = 1.) ?(release = fun _ _ -> ()) ~equal capacity =
    let t =
      {
        capacity = max 0 capacity;
        owner;
        charges = true;
        equal;
        release;
        entries = [];
        last_use = 0;
        charged = ref 0;
      }
    in
    register owner shed ~rebuild ~charged:t.charged t
      ~bytes:(fun t -> !(t.charged))
      ~last_use:(fun t -> t.last_use)
      ~drop:drop_oldest;
    t

  let split equal key entries =
    let rec go before = function
      | [] -> None
      | ((candidate, _, _) as entry) :: rest ->
          if equal key candidate then Some (entry, List.rev_append before rest)
          else go (entry :: before) rest
    in
    go [] entries

  let rec peek_entries equal key = function
    | [] -> None
    | (candidate, value, _) :: rest ->
        if equal key candidate then Some value else peek_entries equal key rest

  let peek t key = peek_entries t.equal key t.entries

  let find t key =
    match split t.equal key t.entries with
    | None -> None
    | Some (((_, value, _) as entry), rest) ->
        t.entries <- entry :: rest;
        t.last_use <- Mem.epoch ();
        Some value

  let remove t key =
    match split t.equal key t.entries with
    | None -> ()
    | Some ((old_key, value, size), rest) ->
        t.entries <- rest;
        discharge t size;
        t.release old_key value

  let add t ?(bytes = 0) key value =
    remove t key;
    let size = max 0 bytes in
    charge t size;
    t.last_use <- Mem.epoch ();
    let entries = (key, value, size) :: t.entries in
    t.entries <- List.take t.capacity entries;
    List.iter
      (fun (old_key, old_value, old_size) ->
        discharge t old_size;
        t.release old_key old_value)
      (List.drop t.capacity entries)

  let head t = match t.entries with [] -> None | (key, value, _) :: _ -> Some (key, value)

  let copy ~release t =
    {
      t with
      charges = false;
      release;
      entries = List.map (fun (k, v, s) -> (k, v, s)) t.entries;
      charged = ref !(t.charged);
    }

  let iter t f = List.iter (fun (key, value, _) -> f key value) (List.rev t.entries)

  let filter t keep =
    let rec loop kept = function
      | [] -> t.entries <- List.rev kept
      | ((key, value, size) as entry) :: rest ->
          if keep key value then loop (entry :: kept) rest
          else (
            discharge t size;
            t.release key value;
            loop kept rest)
    in
    loop [] t.entries

  let length t = List.length t.entries

  let clear t =
    List.iter
      (fun (key, value, size) ->
        discharge t size;
        t.release key value)
      t.entries;
    t.entries <- []
end
