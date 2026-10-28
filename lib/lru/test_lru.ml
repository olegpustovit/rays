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
  let cache = Cache.create ~release:(fun k v -> released := (k, v) :: !released) 3 in
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
  let cache = Cache.create ~byte_capacity:10 ~evictable:(fun k _ -> k <> !pinned) 100 in
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
  let cache = Cache.create ~release:(fun _ _ -> incr count) 8 in
  for i = 1 to 20 do
    Cache.add cache i i
  done;
  Cache.clear cache;
  check (!count = 20 && Cache.length cache = 0 && Cache.bytes cache = 0) "clear releases everything";
  let module Identity = Lru.Identity_list in
  let evicted = ref [] in
  let identity =
    Identity.create ~equal:( == ) ~release:(fun key value -> evicted := (key, value) :: !evicted) 2
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
  let cache = Cache.create 64 in
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
  let identity = Identity.create ~equal:( == ) 64 in
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
  print_endline "lru: order, eviction, bytes, pinning, release, allocation-free hits"
