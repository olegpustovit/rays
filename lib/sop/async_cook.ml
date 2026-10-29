type error = Cook_error of Diagnostic.t | Prepare_error of string | Uncaught_exception of string
type status = Idle | Cooking of { request_id : int; seconds : float; queued : bool }
type 'a completion = { request_id : int; seconds : float; result : ('a, error) result }

type 'a request = {
  id : int;
  context : Context.t;
  nodes : Node.t list;
  optional : Node.t list; (* cooked after [nodes]; one that fails is [None] *)
  volatile : int -> bool; (* installed in the session by the worker before this cook *)
  prepare : Session.output list -> Session.output option list -> ('a, string) result;
}

type active = { started : float; cancel : Rdk.Cancel.t }

type 'a t = {
  session : Session.t;
  mutex : Mutex.t;
  ready : Condition.t;
  finished : Condition.t; (* a request completed or the worker stopped *)
  mutable worker : unit Domain.t option;
  mutable pending : 'a request option;
  mutable active : active option;
  mutable completion : 'a completion option;
  mutable latest_id : int;
  mutable next_volatile : int -> bool; (* what the next request carries *)
  mutable stopping : bool;
  mutable closed : bool;
}

let error_to_string = function
  | Cook_error error -> Diagnostic.error_to_string error
  | Prepare_error message -> message
  | Uncaught_exception cause -> "background cook raised: " ^ cause

let with_lock value operation =
  Mutex.lock value.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock value.mutex) operation

let cancel_request request = Rdk.Cancel.cancel (Context.cancel_token request.context)

let execute session request =
  try
    let rec cook outputs = function
      | [] -> Ok (List.rev outputs)
      | node :: rest -> (
          match Session.cook session ~context:request.context node with
          | Error error -> Error (Cook_error error)
          | Ok output -> cook (output :: outputs) rest)
    in
    match cook [] request.nodes with
    | Error error -> Error error
    | Ok outputs -> (
        (* a footer reads what the cooks left in the cache.  A node nobody cached is cooked for it
           only while it is known to be cheap: one that took over 2 ms of its own (a chain fused
           into a kernel, run again on its own) is never cooked just to be counted. *)
        let optional =
          List.map
            (fun node ->
              match Session.cached session ~context:request.context node with
              | Some _ as hit -> hit
              | None -> (
                  match Session.node_seconds session (Node.id node) with
                  | Some seconds when seconds > 0.002 -> None
                  | _ ->
                      Result.to_option (Session.cook session ~context:request.context node)))
            request.optional
        in
        match request.prepare outputs optional with
        | Ok prepared -> Ok prepared
        | Error message -> Error (Prepare_error message))
  with exn -> Error (Uncaught_exception (Printexc.to_string exn))

(* [applied] is the predicate the session holds: only this domain touches the
   session, so a predicate set while a cook runs waits for the next request. *)
let rec worker_loop value applied =
  let request =
    with_lock value (fun () ->
        while (not value.stopping) && Option.is_none value.pending do
          Condition.wait value.ready value.mutex
        done;
        if value.stopping then None
        else
          let request = Option.get value.pending in
          value.pending <- None;
          value.active <-
            Some
              { started = Session.now value.session; cancel = Context.cancel_token request.context };
          Some request)
  in
  match request with
  | None -> ()
  | Some request ->
      if request.volatile != applied then Session.set_volatile value.session request.volatile;
      let started = Session.now value.session in
      let result = execute value.session request in
      let seconds = max 0. (Session.now value.session -. started) in
      with_lock value (fun () ->
          value.active <- None;
          if (not value.stopping) && request.id = value.latest_id then
            value.completion <- Some { request_id = request.id; seconds; result };
          Condition.broadcast value.finished);
      worker_loop value request.volatile

let none_volatile _ = false
let set_clock value clock = Session.set_clock value.session clock

let create ~max_entries ~max_payload_bytes =
  Result.map
    (fun session ->
      let value =
        {
          session;
          mutex = Mutex.create ();
          ready = Condition.create ();
          finished = Condition.create ();
          worker = None;
          pending = None;
          active = None;
          completion = None;
          latest_id = 0;
          next_volatile = none_volatile;
          stopping = false;
          closed = false;
        }
      in
      let worker =
        Domain.spawn (fun () ->
            Fun.protect ~finally:Parallel.release_current_domain_pools (fun () ->
                worker_loop value none_volatile))
      in
      value.worker <- Some worker;
      value)
    (Session.create ~max_entries ~max_payload_bytes)

let submit_some value ~context ~nodes ~optional ~prepare =
  with_lock value (fun () ->
      if value.closed || value.stopping then Error "Async_cook.submit: worker is closed"
      else begin
        Option.iter cancel_request value.pending;
        Option.iter (fun active -> Rdk.Cancel.cancel active.cancel) value.active;
        value.latest_id <- value.latest_id + 1;
        let id = value.latest_id in
        value.pending <-
          Some { id; context; nodes; optional; prepare; volatile = value.next_volatile };
        value.completion <- None;
        Condition.signal value.ready;
        Ok id
      end)

let submit_all value ~context ~nodes ~prepare =
  submit_some value ~context ~nodes ~optional:[] ~prepare:(fun outputs _ -> prepare outputs)

let submit value ~context ~node ~prepare =
  submit_all value ~context ~nodes:[ node ] ~prepare:(function
    | [ output ] -> prepare output
    | _ -> Error "Async_cook.submit: expected one output")

let set_volatile value predicate =
  with_lock value (fun () ->
      value.next_volatile <- predicate;
      value.pending <-
        Option.map (fun request -> { request with volatile = predicate }) value.pending)

let stats value = Session.stats value.session
let node_seconds value id = Session.node_seconds value.session id

let await value =
  with_lock value (fun () ->
      if value.closed || value.stopping then invalid_arg "Async_cook.await: worker is closed";
      if
        Option.is_none value.completion && Option.is_none value.pending
        && Option.is_none value.active
      then invalid_arg "Async_cook.await: nothing was submitted";
      while
        Option.is_none value.completion
        && (Option.is_some value.pending || Option.is_some value.active)
      do
        Condition.wait value.finished value.mutex
      done;
      match value.completion with
      | Some completion ->
          value.completion <- None;
          completion
      | None -> invalid_arg "Async_cook.await: request was superseded")

let poll value =
  with_lock value (fun () ->
      let completion = value.completion in
      value.completion <- None;
      completion)

let status value =
  with_lock value (fun () ->
      match (value.active, value.pending) with
      | None, None -> Idle
      | Some active, pending ->
          Cooking
            {
              request_id = value.latest_id;
              seconds = max 0. (Session.now value.session -. active.started);
              queued = Option.is_some pending;
            }
      | None, Some pending -> Cooking { request_id = pending.id; seconds = 0.; queued = true })

let close value =
  let worker =
    with_lock value (fun () ->
        if value.closed then None
        else begin
          value.closed <- true;
          value.stopping <- true;
          Option.iter cancel_request value.pending;
          Option.iter (fun active -> Rdk.Cancel.cancel active.cancel) value.active;
          value.pending <- None;
          value.completion <- None;
          Condition.broadcast value.ready;
          Condition.broadcast value.finished;
          value.worker
        end)
  in
  Option.iter (fun worker -> ignore (Domain.join worker)) worker;
  Option.iter (fun _ -> Session.close value.session) worker
