(* One domain against the shared workers: every helper returns the sequential bytes, raises the
   exception a sequential run raises first, nests, and serves two owning domains at once. *)

let sequential f = Parallel.run ~domains:1 f
let wide f = Parallel.run ~domains:(max 2 (Parallel.recommended_domains ())) f

let same name f =
  let expected = sequential f in
  for _ = 1 to 3 do
    if wide f <> expected then failwith (name ^ ": differs from one domain")
  done;
  if f () <> expected then failwith (name ^ ": differs outside a run")

let () =
  let hash i = Hashtbl.hash (i * 7919) land 0xffff in
  List.iter
    (fun length ->
      same "init_array" (fun () -> Parallel.init_array ~grain:3 length hash);
      same "map_array" (fun () -> Parallel.map_array ~grain:5 hash (Array.init length Fun.id));
      same "for_" (fun () ->
          let out = Array.make length 0 in
          Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(length - 1) (fun i -> out.(i) <- hash i);
          out))
    [ 0; 1; 2; 7; 64; 1000; 100_003 ];
  same "nested" (fun () ->
      Parallel.init_array ~grain:1 40 (fun i ->
          let inner = Parallel.init_array ~grain:2 (100 + i) (fun j -> hash (i + j)) in
          Array.fold_left ( + ) 0 inner));
  same "both" (fun () -> Parallel.both (fun () -> hash 1) (fun () -> hash 2))

let () =
  (* the lowest failing index wins, as Array.init would raise it *)
  let failing () =
    let fail i = if i mod 997 = 500 then failwith (string_of_int i) else i in
    match Parallel.init_array ~grain:1 10_000 fail with
    | _ -> "none"
    | exception Failure index -> index
  in
  assert (sequential failing = "500");
  for _ = 1 to 5 do
    assert (wide failing = "500")
  done

let () =
  (* two owners at once: the main domain and a spawned one share the same workers *)
  let work seed () =
    Parallel.init_array ~grain:16 200_000 (fun i -> Hashtbl.hash (seed, i))
  in
  let expected = sequential (work 1) and expected' = sequential (work 2) in
  let other = Domain.spawn (fun () -> List.init 20 (fun _ -> wide (work 2) = expected')) in
  let mine = List.init 20 (fun _ -> wide (work 1) = expected) in
  assert (List.for_all Fun.id mine && List.for_all Fun.id (Domain.join other));
  print_endline
    "Parallel: one domain against the shared workers, lowest failing index, nesting, two owners"
