module Cache = Lru.Make (Int)

let fail message =
  prerr_endline ("test_lru: " ^ message);
  exit 1

let check condition message = if not condition then fail message

let keys cache =
  let out = ref [] in
  Cache.iter cache (fun k _ -> out := k :: !out);
  List.rev !out

let () =
  let released = ref [] in
  let cache = Cache.create ~owner:(Lru.cache "test.lru.release") ~release:(fun k v -> released := (k, v) :: !released) 3 in
  Cache.add cache 1 "a";
  Cache.add cache 2 "b";
  Cache.add cache 3 "c";
  check (keys cache = [ 1; 2; 3 ]) "insertion order oldest first";
  check (Cache.find cache 1 = "a") "hit";
  check
    (Cache.find_opt cache 9 = None
    &&
      try
        ignore (Cache.find cache 9);
        false
      with Not_found -> true)
    "miss";
  check (keys cache = [ 1; 2; 3 ]) "hit only marks the entry";
  check (Cache.peek cache 2 = Some "b" && keys cache = [ 1; 2; 3 ]) "peek keeps order";
  let snapshot_released = ref [] in
  let snapshot =
    Cache.copy ~release:(fun k v -> snapshot_released := (k, v) :: !snapshot_released) cache
  in
  Cache.add snapshot 4 "d";
  check
    (keys snapshot = [ 3; 4; 1 ] && !snapshot_released = [ (2, "b") ])
    "snapshot preserves touch bits and second-chance eviction";
  check
    (keys cache = [ 1; 2; 3 ] && !released = [])
    "snapshot owns independent metadata and release";
  Cache.add cache 4 "d";
  check
    (keys cache = [ 3; 4; 1 ] && !released = [ (2, "b") ])
    "the found entry gets a second chance; the untouched oldest goes";
  Cache.add cache 3 "C";
  check (keys cache = [ 4; 1; 3 ] && List.hd !released = (3, "c")) "replace releases the old value";
  check (Cache.find_first cache (fun _ v -> v = "d") = Some "d") "find_first";
  check
    (Cache.take cache 3 = Some "C" && Cache.length cache = 2 && List.hd !released <> (3, "C"))
    "take does not release";
  Cache.add cache 3 "C";
  Cache.remove cache 4;
  check (Cache.length cache = 2 && List.hd !released = (4, "d")) "remove releases";
  check
    (Cache.drop_oldest cache
    && keys cache = [ 3 ]
    && Cache.drop_oldest cache
    && not (Cache.drop_oldest cache))
    "drop_oldest";
  (* Byte capacity and pinned entries. *)
  let pinned = ref 10 in
  let cache = Cache.create ~owner:(Lru.cache "test.lru.pin") ~byte_capacity:10 ~evictable:(fun k _ -> k <> !pinned) 100 in
  Cache.add cache ~bytes:4 10 "p";
  Cache.add cache ~bytes:4 11 "q";
  Cache.add cache ~bytes:4 12 "r";
  check
    (keys cache = [ 12; 10 ] && Cache.bytes cache = 8)
    "byte eviction skips the pinned entry (it moves to newest)";
  Cache.add cache ~bytes:11 13 "big";
  check
    (Cache.peek cache 13 = None && Cache.bytes cache = 4 && keys cache = [ 10 ])
    "oversize entry leaves at once after evicting what it can";
  pinned := -1;
  Cache.add cache ~bytes:8 14 "s";
  check (keys cache = [ 14 ] && Cache.bytes cache = 8) "unpinned entry evicts";
  let count = ref 0 in
  let cache = Cache.create ~owner:(Lru.cache "test.lru.count") ~release:(fun _ _ -> incr count) 8 in
  for i = 1 to 20 do
    Cache.add cache i i
  done;
  Cache.clear cache;
  check (!count = 20 && Cache.length cache = 0 && Cache.bytes cache = 0) "clear releases everything";
  let module Identity = Lru.Identity_list in
  let evicted = ref [] in
  let identity =
    Identity.create ~owner:(Lru.cache "test.lru.identity") ~equal:( == ) ~release:(fun key value -> evicted := (key, value) :: !evicted) 2
  in
  let a = [| 1 |] and b = [| 2 |] and c = [| 3 |] in
  Identity.add identity a "a";
  Identity.add identity b "b";
  check
    (Identity.find identity a = Some "a" && Identity.peek identity b = Some "b")
    "identity list finds typed physical keys";
  Identity.add identity c "c";
  check
    (Identity.peek identity b = None
    && Identity.peek identity a = Some "a"
    && !evicted = [ (b, "b") ])
    "identity list touches hits and releases the least recent entry";
  Identity.add identity a "updated";
  check
    (!evicted = [ (a, "a"); (b, "b") ] && Identity.length identity = 2)
    "identity list replacement releases once and stays bounded";
  Identity.clear identity;
  check
    (Identity.length identity = 0 && List.length !evicted = 4)
    "identity list clear releases its entries";
  Identity.add identity a "shared";
  let copied_releases = ref 0 in
  let copy = Identity.copy ~release:(fun _ _ -> incr copied_releases) identity in
  Identity.clear copy;
  check
    (!copied_releases = 1 && List.length !evicted = 4 && Identity.peek identity a = Some "shared")
    "identity copy uses its explicit release policy without changing the source";
  Identity.clear identity;
  check (List.length !evicted = 5) "identity source keeps its original release policy";
  (* Steady-state hits and re-adds of present keys allocate nothing. *)
  let cache = Cache.create ~owner:(Lru.cache "test.lru.alloc") 64 in
  for i = 0 to 63 do
    Cache.add cache i i
  done;
  let before = Gc.minor_words () in
  for round = 0 to 999 do
    ignore (Sys.opaque_identity (Cache.find cache (round land 63)))
  done;
  for round = 0 to 999 do
    ignore (Sys.opaque_identity (Cache.find cache (round * 7 land 63)))
  done;
  check (Gc.minor_words () -. before < 64.) "hit path allocates";
  (* Compare the new typed list lookup with the former list-cache lookup. *)
  let keys = Array.init 64 (fun i -> [| i |]) in
  let old_entries = Array.to_list (Array.map (fun key -> (key, key.(0))) keys) in
  let module Identity = Lru.Identity_list in
  let identity = Identity.create ~owner:(Lru.cache "test.lru.id_bench") ~equal:( == ) 64 in
  Array.iter (fun key -> Identity.add identity key key.(0)) keys;
  let rounds = 100_000 in
  let measure f =
    Gc.full_major ();
    let allocated_before = Gc.allocated_bytes () and started = Sys.time () in
    for i = 0 to rounds - 1 do
      ignore (Sys.opaque_identity (f keys.(i land 63)))
    done;
    (Sys.time () -. started, Gc.allocated_bytes () -. allocated_before)
  in
  let old_time, old_allocated =
    measure (fun key ->
        Option.map snd (List.find_opt (fun (candidate, _) -> candidate == key) old_entries))
  in
  let new_time, new_allocated = measure (Identity.peek identity) in
  Printf.printf "lru identity-list lookup (%d hits): old %.4fs/%.0fB new %.4fs/%.0fB\n%!" rounds
    old_time old_allocated new_time new_allocated;
  Mem.reset ();
  let owner = Mem.cache "test.lru" in
  let charged = Cache.create ~owner ~byte_capacity:10 8 in
  Cache.add charged ~bytes:4 1 "a";
  Cache.add charged ~bytes:4 2 "b";
  check (Mem.bytes owner = 8) "owner tracks cache bytes";
  Cache.add charged ~bytes:4 3 "c";
  check (Mem.bytes owner = 8 && Cache.bytes charged = 8) "eviction releases the owner charge";
  Cache.clear charged;
  check (Mem.bytes owner = 0) "clear releases the owner charge";
  (* A copy shares its source's values: no second charge, no evictor, and dropping it changes nothing. *)
  Cache.add charged ~bytes:4 1 "a";
  let attributed = Mem.attributed () and evictors = Mem.evictor_count () in
  let copied = ref (Some (Cache.copy ~release:(fun _ _ -> ()) charged)) in
  check (Mem.attributed () = attributed) "a copy charges nothing";
  check (Mem.evictor_count () = evictors) "a copy registers no evictor";
  Cache.clear (Option.get !copied);
  check (Mem.attributed () = attributed) "clearing a copy releases nothing";
  copied := None;
  Gc.full_major ();
  check (Mem.attributed () = attributed && Mem.evictor_count () = evictors) "a dropped copy leaves no trace";
  let list_owner = Mem.cache "test.lru.list" in
  let listed = Identity.create ~owner:list_owner ~equal:( == ) 4 in
  let key = ref 0 in
  Identity.add listed ~bytes:5 key "a";
  let attributed = Mem.attributed () and evictors = Mem.evictor_count () in
  let list_copy = Identity.copy ~release:(fun _ _ -> ()) listed in
  Identity.clear list_copy;
  check (Mem.attributed () = attributed && Mem.evictor_count () = evictors) "nor does an identity list's";
  check (Mem.bytes list_owner = 5) "the source still holds its charge";
  (* the same value added again changes its size, it does not leave *)
  let released = ref 0 in
  let resized = Cache.create ~owner ~release:(fun _ _ -> incr released) 8 in
  let value = "kept" in
  Cache.add resized ~bytes:1 1 value;
  let before = Mem.bytes owner in
  Cache.add resized ~bytes:9 1 value;
  check (!released = 0 && Cache.peek resized 1 = Some value) "re-adding a value does not release it";
  check (Mem.bytes owner = before + 8) "re-adding a value charges the difference";
  Cache.add resized ~bytes:1 1 "other";
  check (!released = 1) "a different value does";
  Cache.clear resized;
  (* a table made per call does not live on through the ledger, and takes its bytes along *)
  Mem.reset ();
  let dying = Mem.cache "test.lru.dying" in
  (let table = Cache.create ~owner:dying 4 in
   Cache.add table ~bytes:64 1 "x";
   check (Mem.bytes dying = 64 && Mem.evictor_count () = 1) "a table charges and registers");
  ignore (Mem.report ());
  ignore (Mem.report ());
  check (Mem.evictor_count () = 0) "a dead table's evictor leaves";
  check (Mem.bytes dying = 0) "a dead table's bytes leave";
  (* the retreat follows a table's own rule *)
  let budgeted name shed =
    Mem.reset ();
    let owner = Mem.cache name in
    let table = Cache.create ~owner ?shed 4 in
    Cache.add table ~bytes:64 1 "x";
    Mem.set_budget 1;
    (owner, table)
  in
  let usage () = 1 + Mem.attributed () in
  let owner, table = budgeted "test.lru.home" None in
  ignore (Mem.retreat ~usage ());
  check (Cache.length table = 0 && Mem.bytes owner = 0) "Home: dropped from its domain";
  let owner, table = budgeted "test.lru.away" None in
  ignore (Domain.join (Domain.spawn (fun () -> Mem.retreat ~usage ())));
  check (Cache.length table = 1 && Mem.bytes owner = 64) "Home: never from another domain";
  let lock = Mutex.create () in
  let owner, table = budgeted "test.lru.locked" (Some (Lru.Locked lock)) in
  Mutex.lock lock;
  ignore (Mem.retreat ~usage ());
  check (Cache.length table = 1) "Locked: skipped while its user holds the lock";
  Mutex.unlock lock;
  ignore (Mem.retreat ~usage ());
  check (Cache.length table = 0 && Mem.bytes owner = 0 && not (Mutex.try_lock lock && (Mutex.unlock lock; false)))
    "Locked: dropped once free, the lock given back";
  let owner, table = budgeted "test.lru.never" (Some Lru.Never) in
  ignore (Mem.retreat ~usage ());
  check (Cache.length table = 1 && Mem.bytes owner = 64) "Never: not dropped";
  Mem.reset ();
  print_endline "lru: order, eviction, bytes, pinning, release, allocation-free hits"
