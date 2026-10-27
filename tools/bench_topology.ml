open Rdk

let () =
  let count = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000 in
  if count < 2 then invalid_arg "at least two vertices required";
  List.iter (fun domains -> Parallel.run ~domains (fun () ->
    (* Establish the cached pool before collecting constructor samples. *)
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:1 (fun _ -> ());
    List.iter (fun (name, bad_index) ->
      let samples = List.init 7 (fun _ ->
        let vertices = Array.init count Fun.id in
        Option.iter (fun index -> vertices.(index) <- count) bad_index;
        let offsets = [|0; count|] and kinds = [|Topology.Open_polyline|] in
        Gc.full_major ();
        let before = Gc.quick_stat () and allocated = Gc.allocated_bytes () in
        let started = Unix.gettimeofday () in
        let result = Topology.create_owned ~point_count:count ~vertex_points:vertices
          ~primitive_offsets:offsets ~primitive_kinds:kinds in
        let elapsed = Unix.gettimeofday () -. started and after = Gc.quick_stat () in
        (match result with
         | Ok topology -> assert (bad_index = None && Topology.vertex_count topology = count
             && Topology.point_of_vertex topology (count - 1) = count - 1)
         | Error message -> assert (bad_index <> None
             && message = "Topology: vertex references an invalid point"));
        elapsed, Gc.allocated_bytes () -. allocated,
        after.promoted_words -. before.promoted_words, after.major_words -. before.major_words)
        |> List.sort (fun (a,_,_,_) (b,_,_,_) -> Float.compare a b) in
      let seconds, bytes, promoted, major = List.nth samples 3 in
      Printf.printf "topology vertices=%d case=%s invalid_index=%d domains=%d trials=7 median_seconds=%.9f allocated_bytes=%.0f promoted_words=%.0f major_words=%.0f\n%!"
        count name (Option.value ~default:(-1) bad_index) domains seconds bytes promoted major)
      ["valid", None; "invalid_first", Some 0;
       "invalid_block_boundary", Some (min 65536 (count - 1));
       "invalid_last", Some (count - 1)])) [1; 4];
  if Array.exists ((=) "--copy") Sys.argv then begin
    let input = Array.init count (fun i -> if i mod 2 = 0 then i else -i) in
    List.iter (fun (name, copy) ->
      let samples = List.init 7 (fun _ ->
        Gc.full_major ();
        let before = Gc.quick_stat () and allocated = Gc.allocated_bytes () in
        let started = Unix.gettimeofday () in
        let output = copy input in
        let elapsed = Unix.gettimeofday () -. started and after = Gc.quick_stat () in
        assert (output = input && output != input);
        output.(0) <- min_int;
        assert (input.(0) = 0);
        elapsed, Gc.allocated_bytes () -. allocated,
        after.promoted_words -. before.promoted_words, after.major_words -. before.major_words)
        |> List.sort (fun (a,_,_,_) (b,_,_,_) -> Float.compare a b) in
      let seconds, bytes, promoted, major = List.nth samples 3 in
      Printf.printf "integer_copy elements=%d implementation=%s domains=1 trials=7 median_seconds=%.9f allocated_bytes=%.0f promoted_words=%.0f major_words=%.0f\n%!"
        count name seconds bytes promoted major)
      ["Array.copy", Array.copy; "typed_loop", (fun (input : int array) ->
        let output = Array.make (Array.length input) 0 in
        for i = 0 to Array.length input - 1 do output.(i) <- input.(i) done;
        output)]
  end
