module Task = Domainslib.Task

type pooled = {
  pool : Task.pool;
  execution_lock : Mutex.t;
}

type context =
  | Outside
  | Sequential
  | Pooled of pooled

let pools : ((Domain.id * int), pooled) Hashtbl.t = Hashtbl.create 4
let pools_lock = Mutex.create ()
let active_context = Domain.DLS.new_key (fun () -> Outside)
let stopped = ref false

let recommended_domains () = max 1 (Domain.recommended_domain_count ())

let get_pool domains =
  let key = Domain.self (), domains in
  Mutex.lock pools_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock pools_lock) (fun () ->
    if !stopped then invalid_arg "Parallel: pools have already been shut down";
    match Hashtbl.find_opt pools key with
    | Some pooled -> pooled
    | None ->
        let pooled = {
          pool = Task.setup_pool ~num_domains:(domains - 1) ();
          execution_lock = Mutex.create ();
        } in
        Hashtbl.add pools key pooled;
        pooled)

let release_current_domain_pools () =
  let owner = Domain.self () in
  Mutex.lock pools_lock;
  let owned = Hashtbl.fold (fun ((candidate, _) as key) pooled values ->
    if candidate = owner then (key, pooled) :: values else values) pools [] in
  List.iter (fun (key, _) -> Hashtbl.remove pools key) owned;
  Mutex.unlock pools_lock;
  List.iter (fun (_, pooled) -> Task.teardown_pool pooled.pool) owned

let with_context context operation =
  let previous = Domain.DLS.get active_context in
  Domain.DLS.set active_context context;
  Fun.protect
    ~finally:(fun () -> Domain.DLS.set active_context previous)
    operation

let () = at_exit (fun () ->
  Mutex.lock pools_lock;
  stopped := true;
  let values = Hashtbl.to_seq_values pools |> List.of_seq in
  Hashtbl.clear pools;
  Mutex.unlock pools_lock;
  List.iter (fun pooled -> Task.teardown_pool pooled.pool) values)

let run ?domains f =
  match Domain.DLS.get active_context with
  | Sequential | Pooled _ -> f ()
  | Outside ->
      let domains = Option.value ~default:(recommended_domains ()) domains in
      if domains <= 1 then with_context Sequential f
      else begin
        let pooled = get_pool domains in
        Mutex.lock pooled.execution_lock;
        Fun.protect ~finally:(fun () -> Mutex.unlock pooled.execution_lock)
          (fun () -> with_context (Pooled pooled) f)
      end

let parallel_for_pooled pooled ~chunk_size ~start ~finish body =
  let length = finish - start + 1 in
  let chunk_count = (length + chunk_size - 1) / chunk_size in
  let run_chunk chunk =
    let first = start + (chunk * chunk_size) in
    let last = min finish (first + chunk_size - 1) in
    with_context (Pooled pooled) (fun () ->
      for index = first to last do body index done)
  in
  Task.parallel_for pooled.pool ~start:0 ~finish:(chunk_count - 1)
    ~chunk_size:1 ~body:run_chunk

let rec map_array ?(grain = 64) f source =
  if grain <= 0 then invalid_arg "Parallel.map_array: grain must be positive";
  let length = Array.length source in
  if length = 0 then [||]
  else
    match Domain.DLS.get active_context with
    | Sequential -> Array.map f source
    | Outside when length < grain -> Array.map f source
    | Outside -> run (fun () -> map_array ~grain f source)
    | Pooled _ when length < grain -> Array.map f source
    | Pooled pooled ->
        let first = f source.(0) in
        let output = Array.make length first in
        if length > 1 then
          Task.run pooled.pool (fun _ ->
            parallel_for_pooled pooled ~start:1 ~finish:(length - 1)
              ~chunk_size:grain
              (fun index -> output.(index) <- f source.(index)));
        output

let rec init_array ?(grain = 64) length init =
  if grain <= 0 then invalid_arg "Parallel.init_array: grain must be positive";
  if length < 0 then invalid_arg "Parallel.init_array: negative length";
  if length = 0 then [||]
  else
    match Domain.DLS.get active_context with
    | Sequential -> Array.init length init
    | Outside when length < grain -> Array.init length init
    | Outside -> run (fun () -> init_array ~grain length init)
    | Pooled _ when length < grain -> Array.init length init
    | Pooled pooled ->
        let output = Array.make length (init 0) in
        if length > 1 then
          Task.run pooled.pool (fun _ ->
            parallel_for_pooled pooled ~start:1 ~finish:(length - 1)
              ~chunk_size:grain
              (fun index -> output.(index) <- init index));
        output

let rec for_ ?(chunk_size = 64) ~start ~finish body =
  if chunk_size <= 0 then invalid_arg "Parallel.for_: chunk_size must be positive";
  if finish < start then ()
  else
    match Domain.DLS.get active_context with
    | Sequential ->
        for index = start to finish do body index done
    | Outside when finish - start + 1 < chunk_size ->
        for index = start to finish do body index done
    | Outside -> run (fun () -> for_ ~chunk_size ~start ~finish body)
    | Pooled _ when finish - start + 1 < chunk_size ->
        for index = start to finish do body index done
    | Pooled pooled ->
        Task.run pooled.pool (fun _ ->
          parallel_for_pooled pooled ~chunk_size ~start ~finish body)

let rec both left right =
  match Domain.DLS.get active_context with
  | Sequential -> left (), right ()
  | Outside -> run (fun () -> both left right)
  | Pooled pooled ->
      Task.run pooled.pool (fun _ ->
        let left_task = Task.async pooled.pool
            (fun _ -> with_context (Pooled pooled) left) in
        let right_value = right () in
        Task.await pooled.pool left_task, right_value)
