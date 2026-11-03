(* One process-wide set of worker domains serves every caller. Domainslib made a pool per owning
   domain, so the editor ran its 24-wide pool beside the cook domain's 23-wide one: 47 domains on 24
   hardware threads, each stop-the-world waiting on all of them. Here a job is a contiguous index
   range cut into chunks; the caller and any idle worker claim chunks with one atomic counter, so
   jobs from different domains share the workers and nested jobs run on whoever claims them. *)

type job = {
  chunks : int;
  run_chunk : int -> unit;
  next : int Atomic.t;  (** the next unclaimed chunk *)
  finished : int Atomic.t;  (** chunks run or skipped *)
  helpers : int Atomic.t;  (** workers that joined, at most [width - 1] *)
  width : int;
  failed : (int * exn * Printexc.raw_backtrace) option Atomic.t;
      (** the lowest failing chunk, the one a sequential run would have raised *)
}

type context = Outside | Sequential | Pooled of int

let active_context = Domain.DLS.new_key (fun () -> Outside)
let recommended_domains () = max 1 (Domain.recommended_domain_count ())

(* A minor collection stops every domain, and on Windows each stop wakes every idle domain through the kernel (a
   stop-the-world leader measured 1.9 ms at 8 domains, 10 to 16 ms at 24). bench_isogrid at 24 domains ran 16838 minor
   collections in 64 flips (46 s wall) with the 256k-word default and 966 (31 s) with 8M words. Gc.set sizes only the
   calling domain and a spawned domain starts at the default, so each domain sets its own once: the main domain at
   program start (64 MB, where the serial evaluator allocates), a worker when it starts (8 MB). Windows only:
   the Mac keeps the runtime default until it is measured there. *)
let heap_ready = Domain.DLS.new_key (fun () -> false)

let ensure_heap () =
  if Sys.win32 && not (Domain.DLS.get heap_ready) then begin
    Domain.DLS.set heap_ready true;
    let words = if Domain.is_main_domain () then 8 * 1024 * 1024 else 1024 * 1024 in
    let gc = Gc.get () in
    Gc.set { gc with minor_heap_size = max gc.minor_heap_size words }
  end

let () = ensure_heap ()

(* The process-wide pool, as the Domainslib pool table was: jobs with unclaimed chunks, newest
   (often a nested one) first. [lock] guards them, [idle], [stopping] and [workers]; [pushed] counts
   submissions, so a spinning worker sees one without the lock. *)
let lock = Mutex.create ()
let wake = Condition.create ()
let jobs : job list ref = ref []
let pushed = Atomic.make 0
let idle = ref 0
let stopping = ref false
let workers : unit Domain.t list ref = ref []

let with_context context operation =
  let previous = Domain.DLS.get active_context in
  Domain.DLS.set active_context context;
  Fun.protect ~finally:(fun () -> Domain.DLS.set active_context previous) operation

(* Claim and run chunks until none is left. A chunk past the lowest failure is skipped: its result
   is never read. *)
let help job =
  let continue = ref true in
  while !continue do
    let chunk = Atomic.fetch_and_add job.next 1 in
    if chunk >= job.chunks then continue := false
    else begin
      (match Atomic.get job.failed with
      | Some (lowest, _, _) when lowest < chunk -> ()
      | _ -> (
          try with_context (Pooled job.width) (fun () -> job.run_chunk chunk)
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            let rec record () =
              let seen = Atomic.get job.failed in
              match seen with
              | Some (lowest, _, _) when lowest < chunk -> ()
              | _ ->
                  if not (Atomic.compare_and_set job.failed seen (Some (chunk, exn, bt))) then
                    record ()
            in
            record ()));
      Atomic.incr job.finished
    end
  done

let claimable () =
  List.find_opt
    (fun job -> Atomic.get job.next < job.chunks && Atomic.get job.helpers < job.width - 1)
    !jobs

(* An idle worker polls for a while before it parks: a frame's jobs come in bursts, and on Windows
   waking a parked domain goes through the kernel. *)
let rec worker () =
  let seen = Atomic.get pushed in
  let spins = ref 0 in
  while !spins < 20_000 && Atomic.get pushed = seen do
    Domain.cpu_relax ();
    incr spins
  done;
  Mutex.lock lock;
  let rec next () =
    if !stopping then None
    else
      match claimable () with
      | Some job ->
          Atomic.incr job.helpers;
          (* each joining worker wakes the next while chunks remain: a small job finishes before
             many workers wake, a large one reaches every worker in a few hops *)
          let left = job.chunks - Atomic.get job.next in
          if !idle > 0 && left > 1 && Atomic.get job.helpers < job.width - 1 then
            Condition.signal wake;
          Some job
      | None ->
          incr idle;
          Condition.wait wake lock;
          decr idle;
          next ()
  in
  let job = next () in
  Mutex.unlock lock;
  match job with
  | None -> ()
  | Some job ->
      help job;
      worker ()

let start_workers () =
  (* under [lock] *)
  if !workers = [] && not !stopping then
    workers :=
      List.init (recommended_domains () - 1) (fun _ ->
          Domain.spawn (fun () ->
              ensure_heap ();
              worker ()))

let () =
  at_exit (fun () ->
      Mutex.lock lock;
      stopping := true;
      Condition.broadcast wake;
      let running = !workers in
      workers := [];
      Mutex.unlock lock;
      List.iter Domain.join running)

(* Run [chunks] chunks with up to [width] participants, the caller among them, and return once all
   have run. The caller waits for chunks others claimed: they are short (about 16 per domain). *)
let execute ~width ~chunks run_chunk =
  let job =
    {
      chunks;
      run_chunk;
      next = Atomic.make 0;
      finished = Atomic.make 0;
      helpers = Atomic.make 0;
      width;
      failed = Atomic.make None;
    }
  in
  Mutex.lock lock;
  if !stopping then begin
    Mutex.unlock lock;
    invalid_arg "Parallel: the pool has already been shut down"
  end;
  start_workers ();
  jobs := job :: !jobs;
  Atomic.incr pushed;
  if !idle > 0 then Condition.signal wake;
  Mutex.unlock lock;
  help job;
  Mutex.lock lock;
  jobs := List.filter (fun other -> other != job) !jobs;
  Mutex.unlock lock;
  while Atomic.get job.finished < chunks do
    Domain.cpu_relax ()
  done;
  match Atomic.get job.failed with
  | Some (_, exn, bt) -> Printexc.raise_with_backtrace exn bt
  | None -> ()

let run ?domains f =
  match Domain.DLS.get active_context with
  | Sequential | Pooled _ -> f ()
  | Outside ->
      let domains = Option.value ~default:(recommended_domains ()) domains in
      let context =
        if domains <= 1 then Sequential else Pooled (min domains (recommended_domains ()))
      in
      with_context context f

(* About 16 chunks per participant, none smaller than [minimum]: the scheduler over-partitions so a
   slow chunk does not hold the job, and a range of one chunk stays on the caller. *)
let chunking ~width ~minimum length =
  let size = max minimum ((length + (16 * width) - 1) / (16 * width)) in
  (size, (length + size - 1) / size)

(* [Some width] when [length] elements are worth a job here *)
let pooled ~minimum length =
  match Domain.DLS.get active_context with
  | Sequential -> None
  | Outside | Pooled _ when length < minimum -> None
  | Outside -> if recommended_domains () <= 1 then None else Some (recommended_domains ())
  | Pooled width -> Some width

let range ~width ~minimum ~start ~finish body =
  let size, chunks = chunking ~width ~minimum (finish - start + 1) in
  if chunks <= 1 then
    for index = start to finish do
      body index
    done
  else
    execute ~width ~chunks (fun chunk ->
        let first = start + (chunk * size) in
        for index = first to min finish (first + size - 1) do
          body index
        done)

let map_array ?(grain = 64) f source =
  if grain <= 0 then invalid_arg "Parallel.map_array: grain must be positive";
  let length = Array.length source in
  match pooled ~minimum:grain length with
  | None -> Array.map f source
  | Some width ->
      let output = Array.make length (f source.(0)) in
      range ~width ~minimum:grain ~start:1 ~finish:(length - 1) (fun index ->
          output.(index) <- f source.(index));
      output

let init_array ?(grain = 64) length init =
  if grain <= 0 then invalid_arg "Parallel.init_array: grain must be positive";
  if length < 0 then invalid_arg "Parallel.init_array: negative length";
  match pooled ~minimum:grain length with
  | None -> Array.init length init
  | Some width ->
      let output = Array.make length (init 0) in
      range ~width ~minimum:grain ~start:1 ~finish:(length - 1) (fun index ->
          output.(index) <- init index);
      output

let for_ ?(chunk_size = 64) ~start ~finish body =
  if chunk_size <= 0 then invalid_arg "Parallel.for_: chunk_size must be positive";
  if finish >= start then
    match pooled ~minimum:chunk_size (finish - start + 1) with
    | None ->
        for index = start to finish do
          body index
        done
    | Some width -> range ~width ~minimum:chunk_size ~start ~finish body

let both left right =
  match pooled ~minimum:1 2 with
  | None -> (left (), right ())
  | Some width ->
      let left_value = ref None and right_value = ref None in
      execute ~width ~chunks:2 (function
        | 0 -> left_value := Some (left ())
        | _ -> right_value := Some (right ()));
      (Option.get !left_value, Option.get !right_value)
