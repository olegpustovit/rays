module G = Flow_ir.Gpu

module Cache = Lru.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

type runner = {
  identity : int;
  run : Run.t;
  mutable stamp : int64;
  mutable output : Run.output option;
}

type stats = {
  runners_created : int;
  runners_released : int;
  pipeline_compilations : int;
  pipeline_releases : int;
  buffer_creations : int;
  input_uploads : int;
  input_uploaded_bytes : int;
  status_reads : int;
  readback_bytes : int;
  payload_readback_bytes : int;
  status_readback_bytes : int;
  count_readback_bytes : int;
  validation_readback_bytes : int;
  bounds_readback_bytes : int;
  materialisations : int;
  held_buffers : int;
  freed : int;
}

let zero =
  {
    runners_created = 0;
    runners_released = 0;
    pipeline_compilations = 0;
    pipeline_releases = 0;
    buffer_creations = 0;
    input_uploads = 0;
    input_uploaded_bytes = 0;
    status_reads = 0;
    readback_bytes = 0;
    payload_readback_bytes = 0;
    status_readback_bytes = 0;
    count_readback_bytes = 0;
    validation_readback_bytes = 0;
    bounds_readback_bytes = 0;
    materialisations = 0;
    held_buffers = 0;
    freed = 0;
  }

let add_run stats run =
  {
    stats with
    buffer_creations = stats.buffer_creations + Run.Private.buffer_creations run;
    input_uploads = stats.input_uploads + Run.Private.input_uploads run;
    input_uploaded_bytes = stats.input_uploaded_bytes + Run.Private.input_uploaded_bytes run;
    status_reads = stats.status_reads + Run.Private.status_reads run;
    readback_bytes = stats.readback_bytes + Run.Private.readback_bytes run;
    payload_readback_bytes =
      stats.payload_readback_bytes + Run.Private.readback_bytes run
      - Run.Private.bounds_readback_bytes run;
    status_readback_bytes = stats.status_readback_bytes + (4 * Run.Private.status_reads run);
    bounds_readback_bytes = stats.bounds_readback_bytes + Run.Private.bounds_readback_bytes run;
    materialisations = stats.materialisations + Run.Private.materialisations run;
    held_buffers = stats.held_buffers;
  }

(* A resident fold's slot: a site's Nth region in one fold run, with the compactions and retypings
   of its array. Its buffers and placeholders come back to it as the run closes and serve the next
   draw's run; what a run did not take (a size it outgrew) is destroyed then. [capacity] is the
   smallest power of two above the largest count it has held, 0 before it held one: it never
   shrinks while the site lives, so a count that falls back costs memory, never a cut. *)
type slot = {
  mutable capacity : int;
  mutable kept : bool;  (** false once its site left the host: its things are destroyed at close *)
  free : (int, Ogpu.Backend.buffer list) Hashtbl.t;  (** buffers by byte size *)
  arrays : (int * int, Obj.t list) Hashtbl.t;  (** placeholders by (kind, length) *)
  mutable held : placeholder list;  (** this run's placeholders in it *)
  mutable spent : (Ogpu.Backend.buffer * int) list;  (** this run's compaction scratch in it *)
  mutable spare : (Ogpu.Backend.buffer * int) list;
      (** this run's buffers it took back (ping-pong), for its next use in the run *)
  mutable spare_arrays : ((int * int) * Obj.t) list;  (** and their placeholders *)
}

and placeholder = {
  key : Obj.t;
  kind : int;
  uint32 : bool;
  buffer : Ogpu.Backend.buffer * int;
  length : int;
  cell : (Ogpu.Backend.buffer * int) option;
  slot : slot;
  counted : bool;  (** its count is the device's: it sizes the slot *)
  clamped : int;
      (** with its length, the slot's capacity, below its bound, the run's number of its region;
          else [max_int] *)
  mul : int;
  div : int;  (** its count is its cell's times [mul] over [div] (a range of a kept count) *)
  mutable owned : bool;  (** its buffers are its slot's (a range's buffer is the host's index buffer) *)
  mutable dead : bool;
      (** its slot was taken again in the run (nothing held it): only its count cell is still its *)
  origin : (Ogpu.Backend.buffer * int * int) option;
      (** after the loop: the device count it was dispatched with, its own count when uncut *)
}

(* a region site's runner (None when the device refuses its form), its slots, and the
   runner that draws it as a mark attribute over a fold's kept arrays ({!resident}'s show) *)
type site = {
  run : Run.t option;
  slots : slot Dynarray.t;
  mutable shown : runner option;
  mutable plan : Flow_ir.Packed.Host.plan option option;  (** from its first unbound region *)
  mutable shape : (int array * Flow_ir.Packed.instruction array) option;
      (** the uniform widths and code of the region: a site outlives its lowering, so it keeps no
          [Packed.t], which holds that lowering's whole program and source *)
  mutable inputs : Flow_ir.Packed.Host.inputs option;  (** the plan's inputs, refilled per call *)
}

type t = {
  gpu : Renderer.gpu;
  pipelines : Pipelines.t;
  runners : runner Cache.t;
  created : int ref;
  retired : stats ref;
  sites : (Flow_ir.Packed.instruction array * bool, site) Lru.Identity_list.t;
      (* resident fold sites by their code *)
  mutable flight : int64 list; (* fast dispatches' epochs, newest first *)
  mutable iota : (Ogpu.Backend.buffer * int) option;
  mutable iota_uint32 : (Ogpu.Backend.buffer * int) option;
      (* 0., 1., 2., ... on the device, for range sources *)
  mutable retired_buffers : Ogpu.Backend.buffer list;
      (* outgrown index buffers, destroyed at close *)
  buffers : (int, Ogpu.Backend.buffer list) Hashtbl.t; (* display normals' buffers by byte size *)
  mutable closed : bool;
  mutable resident_buffers : int;  (** buffers a resident fold's slots made *)
  mutable resident_payload_bytes : int;
  mutable count_bytes : int;
  mutable validation_bytes : int;
  mutable validation_status : Ogpu.Backend.buffer option;
  mutable showing : (Flow_ir.Packed.t -> live:Input.t -> G.value option option) option;
      (* the open resident fold's mark attributes, after its loop ({!show}) *)
  discarding : bool ref;  (** set by [close]: its runners' outputs are not read back *)
  domain : Domain.id;
  shared : Run.pool;  (** the kept outputs' buffers of every runner, recycled across edits *)
  mutable ownership : Run.t option;
  tables : (int array, Ogpu.Backend.buffer list option) Lru.Identity_list.t;
      (* a triangle mesh's corners and per-point corner CSR on the device, by its corners array *)
}

let next = Atomic.make 1

(* a slot's idle things are destroyed: none is in flight, since a fold run's close waits for it *)
(* A placeholder is a host array of its full length: the evaluator takes a device array's count from
   the block's header (Core_exec's [V.array_length], [array/count] and [array/range]), so the header
   must say it. Float arrays are never written, so those pages stay untouched; an Int one is zeroed
   for the collector. Kept slots' arrays are charged here. *)
let placeholders = Mem.cache "flow_gpu.placeholders"
let placeholder_bytes (kind, length) = 8 * max 1 length * max 1 kind

let purge slot =
  Hashtbl.iter
    (fun key arrays -> Mem.release placeholders (placeholder_bytes key * List.length arrays))
    slot.arrays;
  Hashtbl.iter
    (fun _ bs -> List.iter (fun b -> ignore (Ogpu.Backend.destroy_buffer b)) bs)
    slot.free;
  Hashtbl.reset slot.free;
  Hashtbl.reset slot.arrays

let retire slot =
  slot.kept <- false;
  purge slot

let create ~clock gpu =
  if not (Domain.is_main_domain ()) then invalid_arg "Host.create: initial domain required";
  let created = ref 0 and retired = ref zero and discarding = ref false in
  {
    gpu;
    pipelines = Pipelines.create ~clock (Renderer.gpu_device gpu);
    created;
    retired;
    sites =
      Lru.Identity_list.create ~owner:(Lru.cache "flow_gpu.sites")
        ~equal:(fun (a, u) (b, v) -> a == b && u = v)
        ~release:(fun _ site ->
          Option.iter
            (fun run ->
              Run.close ~discard:!discarding run;
              retired := add_run !retired run)
            site.run;
          Option.iter
            (fun (runner : runner) ->
              runner.output <- None;
              Run.close ~discard:!discarding runner.run;
              retired :=
                {
                  (add_run !retired runner.run) with
                  runners_released = !retired.runners_released + 1;
                })
            site.shown;
          Dynarray.iter retire site.slots)
        32;
    flight = [];
    iota = None;
    iota_uint32 = None;
    retired_buffers = [];
    buffers = Hashtbl.create 16;
    runners =
      Cache.create ~owner:(Lru.cache "flow_gpu.runners")
        ~release:(fun _ runner ->
          runner.output <- None;
          Run.close ~discard:!discarding runner.run;
          retired :=
            { (add_run !retired runner.run) with runners_released = !retired.runners_released + 1 })
        64;
    closed = false;
    resident_buffers = 0;
    resident_payload_bytes = 0;
    count_bytes = 0;
    validation_bytes = 0;
    validation_status = None;
    showing = None;
    discarding;
    domain = Domain.self ();
    shared = Run.pool ();
    ownership = None;
    tables =
      Lru.Identity_list.create ~owner:(Lru.cache "flow_gpu.tables") ~equal:( == )
        ~release:(fun _ ->
          Option.iter (List.iter (fun b -> ignore (Ogpu.Backend.destroy_buffer b))))
        8;
  }

let live t = (not t.closed) && Domain.self () = t.domain
let live_ok = live
let error message = Error (Diagnostic.error ~code:"E_GPU" message)

(* In fast precision a dispatch does not wait; its output is waited for when first borrowed or read,
   and at most two stay in flight (the queue admits three command buffers). *)
let complete t epoch =
  let queue = Renderer.gpu_queue t.gpu in
  if epoch <= Ogpu.Backend.completed_epoch queue then Ok ()
  else Result.map_error Run.Private.diagnostic (Ogpu.Backend.complete_through queue epoch)

(* Every command buffer the host commits without waiting (fast display regions, a resident fold's
   levels) shares one limit: before another, all but the newest in flight are waited for, so at most
   two are (the queue admits three). *)
let admit t =
  let queue = Renderer.gpu_queue t.gpu in
  t.flight <- List.filter (fun e -> e > Ogpu.Backend.completed_epoch queue) t.flight;
  match t.flight with
  | _ :: older :: _ -> Result.map (fun () -> t.flight <- List.take 1 t.flight) (complete t older)
  | _ -> Ok ()

let record t epoch = if epoch > 0L then t.flight <- epoch :: t.flight

(* The runner's current output for [value], not waited for. *)
let current t (value : G.value) =
  if not (live t) then None
  else
    let shown () =
      let found = ref None in
      Lru.Identity_list.iter t.sites (fun _ site ->
          match site.shown with
          | Some runner when runner.identity = value.identity -> found := Some runner
          | _ -> ());
      !found
    in
    match
      match Cache.find_first t.runners (fun _ runner -> runner.identity = value.identity) with
      | Some _ as found -> found
      | None -> shown ()
    with
    | Some runner when runner.stamp = value.stamp -> (
        match Option.bind runner.output (Run.select_output ~index:value.output) with
        | Some output
          when Run.count output = value.count
               && Run.width output = value.width
               && (Run.count output = 0 || Run.buffer output <> None) ->
            Some (runner, output)
        | _ -> None)
    | _ -> None

let output t (value : G.value) =
  match current t value with
  | None -> Ok None
  | Some (runner, output) ->
      Result.bind
        (complete t (Run.epoch output))
        (fun () ->
          if Run.count output = 0 then Ok (Some output)
          else
            Result.map
              (fun () -> Some output)
              (Run.Private.check_status runner.run ~epoch:(Run.epoch output)))

(* the index buffer of a range source: grown geometrically, kept by the host *)
let iota t length =
  match t.iota with
  | Some (buffer, capacity) when capacity >= length -> Some buffer
  | old -> (
      let capacity = max length (2 * Option.fold ~none:0 ~some:snd old) in
      let bytes = Bytes.create (capacity * 4) in
      for k = 0 to capacity - 1 do
        Bytes.set_int32_le bytes (k * 4) (Int32.bits_of_float (float k))
      done;
      match
        Ogpu.Backend.create_buffer (Renderer.gpu_device t.gpu)
          {
            label = Some "Flow resident indices";
            size = Int64.of_int (capacity * 4);
            usage = [ Storage; Copy_dst ];
          }
      with
      | Error _ -> None
      | Ok buffer ->
          if Result.is_error (Ogpu.Backend.write_buffer buffer ~offset:0L bytes) then (
            ignore (Ogpu.Backend.destroy_buffer buffer);
            None)
          else begin
            (* the old buffer may still be read by work in flight: keep it until the host closes *)
            Option.iter (fun (old, _) -> t.retired_buffers <- old :: t.retired_buffers) old;
            t.iota <- Some (buffer, capacity);
            Some buffer
          end)

let standard_backend t : G.backend =
  {
    residency = (fun ~fast:_ -> None);
    cost = (fun ?inputs:_ ~fast:_ _ ~count:_ -> None);
    prepare =
      (fun ~fast packed ->
        if not (live t) then error "GPU host is closed or called from another domain."
        else
          (* a range source ([Packed.Host.index_sources]) is the host's index buffer, never uploaded *)
          let index = Flow_ir.Packed.Host.index_sources packed in
          Result.bind (Emit.kernel ~fast packed) (fun msl ->
              Result.map
                (fun _compiled ->
                  (* Each prepared producer owns an output generation. Sharing only its
          shader source would overwrite an earlier drawing's borrowed output. *)
                  let key = string_of_int (Atomic.fetch_and_add next 1) in
                  let runner () =
                    match Cache.find_opt t.runners key with
                    | Some runner -> runner
                    | None ->
                        let runner =
                          {
                            identity = Atomic.fetch_and_add next 1;
                            run = Run.create ~pool:t.shared t.gpu t.pipelines msl;
                            stamp = 0L;
                            output = None;
                          }
                        in
                        incr t.created;
                        Cache.add t.runners key runner;
                        runner
                  in
                  ignore (runner ());
                  {
                    G.run =
                      (fun inputs ->
                        if not (live t) then
                          error "GPU host is closed or called from another domain."
                        else
                          let runner = runner () in
                          runner.output <- None;
                          Result.bind
                            (if fast then admit t else Ok ())
                            (fun () ->
                              Result.map
                                (fun output ->
                                  if fast then record t (Run.epoch output);
                                  runner.output <- Some output;
                                  runner.stamp <- Int64.succ runner.stamp;
                                  G.
                                    {
                                      identity = runner.identity;
                                      count = Run.count output;
                                      width = Run.width output;
                                      output = 0;
                                      stamp = runner.stamp;
                                      gpu_seconds = Run.gpu_seconds output;
                                    })
                                (Run.dispatch ~wait:(not fast)
                                   ~bound:(fun i -> if index.(i) then iota t inputs.count else None)
                                   runner.run inputs)));
                    readback =
                      (fun value ->
                        match output t value with
                        | Error diagnostic -> Error diagnostic
                        | Ok None -> error "GPU output was closed or superseded."
                        | Ok (Some output) -> Run.readback output);
                    planes =
                      (fun value ->
                        match current t value with
                        | None -> error "GPU output was closed or superseded."
                        | Some (_, output) -> Run.keep output);
                  })
                (Pipelines.get t.pipelines msl)));
  }

(* A resident fold: from a region of 256 elements, a loop's regions and compactions are encoded level
   by level, committed without a wait, into buffers the fold owns; nothing is read until the loop ends.  A
   region's value is a placeholder (an uninitialized array of its capacity, an upper bound) the table below maps,
   physically, to its buffer and to the count cell the device wrote, so the next region's count
   ([array/range] of an array's count) is taken on the device.  [finish] reads a count, then the
   array.

   A counted region's capacity is its slot's from earlier draws when it has one (no wait), else the
   bound. The kernel stops at its capacity and writes its count, so a count that reaches a capacity
   below the bound may have been cut: [finish] then raises [Resident_overflow] and the loop runs
   again, its regions from the first cut one on ([exact]) at their bounds. The device is
   deterministic, so the ones before it repeat their uncut counts (were one cut, [exact] would move
   before it, so the reruns end). The slots have grown at close.

   After the loop ({!settle}: its counts checked, nothing read), the arrays it kept may stay where
   they are for the rest of the draw: an [array/range] of a kept array's count is the device's index
   buffer counted by that array's cell ([range]), a later region over them runs here at its bound
   (no slot capacity, so nothing is cut), and a mark attribute over them is drawn by the site's
   [shown] runner, its grid and count record from the device count. [close], at the draw's end,
   hands everything back. *)

(* the smallest power of two above [n]: a count that fits leaves room, so reaching it means a cut *)
let above n =
  let rec go c = if c > n then c else go (2 * c) in
  go 1

(* a site keeps its first 64 ordinals' slots; a longer fold's later levels own theirs for one run *)
let site_slots = 64

type residency = {
  executor : Flow_ir.Core_exec.resident;
  retain_uint32 : Flow.Eval.value -> Run.uint32_holder option;
  retain_float : Flow.Eval.value -> Flow_ir.Gpu.planes option;
  uint32_stream : Flow.Eval.value -> (Ogpu.Backend.buffer * int) option;
}

let resident_scope ?(uint32 = false) ~fast t =
  let module E = Flow.Eval in
  let module P = Flow_ir.Packed.Host in
  let queue = Renderer.gpu_queue t.gpu in
  let read_count cell ~epoch =
    Result.map
      (fun count ->
        t.count_bytes <- t.count_bytes + 4;
        count)
      (Run.read_count queue cell ~epoch)
  in
  let table = ref [] and epoch = ref 0L and status_runs = ref [] in
  let integers = ref [] and floats = ref [] in
  let on_close = ref [] in
  let touched = ref [] and invoked = ref 0 and exact = ref max_int and cut = ref max_int in
  let extra_slots = ref [] in
  let pending = ref None and settled = ref false and post = ref false in
  (* a level's command buffer and its one compute encoder, which orders its dispatches *)
  let commands () =
    match !pending with
    | Some c -> Ok c
    | None -> (
        match Ogpu.Backend.begin_commands queue with
        | Error e -> Error (Run.Private.diagnostic e)
        | Ok c -> (
            match Ogpu.Backend.compute_encoder c with
            | Error e ->
                ignore (Ogpu.Backend.abandon c);
                Error (Run.Private.diagnostic e)
            | Ok encoder ->
                pending := Some (c, encoder);
                Ok (c, encoder)))
  in
  (* committed per level without waiting, so the device runs a level while the host encodes the
     next; at most two in flight (the queue admits three) *)
  let flush () =
    match !pending with
    | None -> Ok ()
    | Some (c, encoder) -> (
        pending := None;
        ignore (Ogpu.Backend.end_compute encoder);
        match admit t with
        | Error diagnostic ->
            ignore (Ogpu.Backend.abandon c);
            Error diagnostic
        | Ok () ->
            Result.map
              (fun (r : Ogpu.Backend.receipt) ->
                record t r.epoch;
                epoch := r.epoch)
              (Result.map_error Run.Private.diagnostic (Ogpu.Backend.commit c)))
  in
  let raw = function
    | E.Float_array a | Vec2_array a | Vec3_array a | Vec4_array a -> Some (Obj.repr a)
    | Int_array a -> Some (Obj.repr a)
    | _ -> None
  in
  let rec lookup key = function
    | [] -> None
    | p :: rest -> if p.key == key && not p.dead then Some p else lookup key rest
  in
  let find = function
    | E.Float_array a | Vec2_array a | Vec3_array a | Vec4_array a -> lookup (Obj.repr a) !table
    | Int_array a -> lookup (Obj.repr a) !table
    | _ -> None
  in
  (* a slot's capacities repeat the last draw's, so its placeholders come back from it *)
  let pooled slot kind length make =
    match List.find_opt (fun ((k, n), _) -> k = kind && n = length) slot.spare_arrays with
    | Some ((_, a) as x) ->
        slot.spare_arrays <- List.filter (fun y -> y != x) slot.spare_arrays;
        Obj.obj a
    | None -> (
        match Hashtbl.find_opt slot.arrays (kind, length) with
        | Some (a :: rest) ->
            Hashtbl.replace slot.arrays (kind, length) rest;
            Mem.release placeholders (placeholder_bytes (kind, length));
            Obj.obj a
        | _ -> make ())
  in
  let placeholder ?(counted = false) ?(clamped = max_int) ?origin slot ty buffer cell length =
    (* Empty arrays share OCaml's atom: a placeholder needs its own identity even at count zero. *)
    let capacity = max 1 length in
    let v : E.value =
      match ty with
      | Flow.Ty.Array Flow.Ty.Int ->
          Int_array (pooled slot 0 length (fun () -> Array.make capacity 0))
      | Array Float -> Float_array (pooled slot 1 length (fun () -> Array.create_float capacity))
      | Array Vec2 -> Vec2_array (pooled slot 2 length (fun () -> Array.create_float (2 * capacity)))
      | Array Vec3 -> Vec3_array (pooled slot 3 length (fun () -> Array.create_float (3 * capacity)))
      | _ -> Vec4_array (pooled slot 4 length (fun () -> Array.create_float (4 * capacity)))
    in
    let kind =
      match v with
      | E.Int_array _ -> 0
      | Float_array _ -> 1
      | Vec2_array _ -> 2
      | Vec3_array _ -> 3
      | _ -> 4
    in
    let p =
      {
        key = Option.get (raw v);
        kind;
        uint32 = uint32 && ty = Flow.Ty.Array Flow.Ty.Int;
        buffer;
        length;
        cell;
        slot;
        counted;
        clamped;
        mul = 1;
        div = 1;
        owned = true;
        dead = false;
        origin;
      }
    in
    table := p :: !table;
    slot.held <- p :: slot.held;
    v
  in
  let integer_source code source =
    uint32
    && Array.exists
         (fun code ->
           match snd (Trace.flat [] code) with
           | Flow_ir.Packed.Int_input i -> i = source
           | _ -> false)
         code
  in
  let iota ?(integer = uint32) length =
    if not integer then iota t length
    else
      match t.iota_uint32 with
      | Some (buffer, capacity) when capacity >= length -> Some buffer
      | old -> (
          let capacity = max 1 (max length (2 * Option.fold ~none:0 ~some:snd old)) in
          match
            Result.map fst (Ogpu.Backend.create_mapped_buffer (Renderer.gpu_device t.gpu)
              {
                label = Some "Flow uint32 indices";
                size = Int64.of_int (capacity * 4);
                usage = [ Storage; Copy_src; Copy_dst ];
              })
          with
          | Error _ -> None
          | Ok buffer -> (
              t.resident_buffers <- t.resident_buffers + 1;
              match Run.fill_indices queue t.pipelines buffer ~count:capacity with
              | Error _ ->
                  ignore (Ogpu.Backend.destroy_buffer buffer);
                  None
              | Ok () ->
                  Option.iter (fun (b, _) -> t.retired_buffers <- b :: t.retired_buffers) old;
                  t.iota_uint32 <- Some (buffer, capacity);
                  Some buffer))
  in
  let bound v = Option.map (fun p -> fst p.buffer) (find v) in
  let give_back slot (b, bytes) =
    Hashtbl.replace slot.free bytes
      (b :: Option.value ~default:[] (Hashtbl.find_opt slot.free bytes))
  in
  (* a slot's buffer sizes repeat the last draw's; close hands them back *)
  let alloc slot bytes =
    match List.find_opt (fun (_, n) -> n = bytes) slot.spare with
    | Some b ->
        slot.spare <- List.filter (fun x -> x != b) slot.spare;
        Ok b
    | None -> (
        match Hashtbl.find_opt slot.free bytes with
        | Some (b :: rest) ->
            Hashtbl.replace slot.free bytes rest;
            Ok (b, bytes)
        | _ -> (
            (* a large buffer comes from what retained outputs gave back; sizes by class, so a mesh
               whose count moves between frames finds one *)
            let large = bytes >= 1 lsl 20 in
            let device = Renderer.gpu_device t.gpu in
            match if large then Run.recycled t.shared ~device bytes else None with
            | Some (b, _, _) -> Ok (b, bytes)
            | None ->
            let size =
              if not large then bytes
              else
                let rec up n = if n >= bytes then n else up (2 * n) in
                up (1 lsl 20)
            in
            if large then Run.created t.shared size;
            Result.map
              (fun b ->
                t.resident_buffers <- t.resident_buffers + 1;
                (b, bytes))
              (Result.map_error Run.Private.diagnostic
                 (Result.map fst (Ogpu.Backend.create_mapped_buffer device
                    {
                      label = Some "Flow resident";
                      size = Int64.of_int size;
                      usage = [ Storage; Copy_src; Copy_dst ];
                    })))))
  in
  (* a site, its runner None when the device refuses its form; decided once per site *)
  let site (packed : Flow_ir.Packed.t) =
    let code = packed.code in
    let key = (code, uint32) in
    match Lru.Identity_list.peek t.sites key with
    | Some site -> site
    | None ->
        let ty = packed.term.ty in
        let run =
          if
            Flow_ir.Packed.gpu_refusals ~fused:true ~fast:true packed <> []
            || not
                 (List.mem ty
                    Flow.Ty.[ Array Int; Array Float; Array Vec2; Array Vec3; Array Vec4 ])
          then None
          else
            Result.to_option
              (Result.map
                 (fun msl -> Run.create t.gpu t.pipelines msl)
                 (if not uint32 then Emit.kernel ~fast:true packed
                  else
                    let inputs = ref [] and tables = ref [] in
                    Array.iter
                      (fun code ->
                        match snd (Trace.flat [] code) with
                        | Flow_ir.Packed.Int_input i -> inputs := i :: !inputs
                        | Array_prepare (u, Flow.Ty.Array Flow.Ty.Int) -> tables := u :: !tables
                        | _ -> ())
                      packed.code;
                    Emit.kernel ~fast:true ~uint32_scalars:true
                      ~uint32_inputs:(List.sort_uniq Int.compare !inputs)
                      ~uint32_tables:(List.sort_uniq Int.compare !tables)
                      ~uint32_outputs:(if ty = Flow.Ty.Array Flow.Ty.Int then [ 0 ] else [])
                      packed))
        in
        (* the 32 most recent region sites are kept; a re-prepared drawing has new sites *)
        let site =
          {
            run;
            slots = Dynarray.create ();
            shown = None;
            plan = None;
            shape = None;
            inputs = None;
          }
        in
        Lru.Identity_list.add t.sites key site;
        site
  in
  (* Ping-pong: a site's slot is taken again in a run once none of the draw's locals holds an array
     of it (a fold's level reads the last one's: two slots take turns), so a fold of any depth keeps
     about two sets of buffers a site, each sized by the largest count it held. What the last use
     wrote stays the device's (queue order), its count cell stays its own (the loop's end checks
     every count), its buffers and arrays go back at once. *)
  let held_by locals key =
    let holds = function
      | E.Float_array a | Vec2_array a | Vec3_array a | Vec4_array a -> Obj.repr a == key
      | Int_array a -> Obj.repr a == key
      | _ -> false
    in
    let rec go i = i < Array.length locals && (holds locals.(i) || go (i + 1)) in
    go 0
  in
  let take slot =
    let given = ref [] in
    List.iter
      (fun p ->
        if not p.dead then begin
          p.dead <- true;
          if p.owned && not (List.memq (fst p.buffer) !given) then begin
            given := fst p.buffer :: !given;
            slot.spare <- p.buffer :: slot.spare
          end;
          slot.spare_arrays <- ((p.kind, p.length), p.key) :: slot.spare_arrays
        end)
      slot.held;
    slot.held <- [];
    slot.spare <- List.rev_append slot.spent slot.spare;
    slot.spent <- []
  in
  let slot site ~locals =
    if not (List.memq site !touched) then touched := site :: !touched;
    let free s = List.for_all (fun p -> p.dead || not (held_by locals p.key)) s.held in
    let rec first k =
      if k >= Dynarray.length site.slots then None
      else
        let s = Dynarray.get site.slots k in
        if free s then Some s else first (k + 1)
    in
    match first 0 with
    | Some s ->
        take s;
        s
    | None ->
        let slot =
          {
            capacity = 0;
            kept = Dynarray.length site.slots < site_slots;
            free = Hashtbl.create 4;
            arrays = Hashtbl.create 4;
            held = [];
            spent = [];
            spare = [];
            spare_arrays = [];
          }
        in
        if slot.kept then Dynarray.add_last site.slots slot;
        slot
  in
  (* The device count bound of a region, if its sources allow the device to keep counts: [Error]
     when a source or uniform reads a placeholder in a form the host would evaluate over zeros or a
     capacity, or when two device counts bound it. *)
  (* a placeholder's device count; after the loop nothing is cut: it is the one it was dispatched
     with *)
  let placeholder_count p =
    match p.origin with
    | Some origin when !post -> Some origin
    | _ -> Option.map (fun (c, _) -> (c, p.mul, p.div)) p.cell
  in
  let device_count packed =
    let opaque = function
      | P.Opaque None -> !table <> []
      | Opaque (Some vs) -> List.exists (fun v -> find v <> None) vs
      | Value _ | Range _ -> false
    in
    if Array.exists opaque (P.source_shapes packed) || Array.exists opaque (P.uniform_shapes packed)
    then Error ()
    else
      let count = placeholder_count in
      let counts =
        Array.to_list (P.source_shapes packed)
        |> List.filter_map (function
          | P.Value v -> Option.bind (find v) count
          | Range (v, mul, div) ->
              Option.bind (find v) count |> Option.map (fun (c, m, d) -> (c, m * mul, d * div))
          | Opaque _ -> None)
      in
      match counts with
      | [] -> Ok None
      | (c, m, d) :: rest when List.for_all (fun (c', m', d') -> c' == c && m' = m && d' = d) rest
        ->
          Ok (Some (c, m, d))
      | _ -> Error ()
  in
  (* A region's inputs, through its site's plan (from the arguments, nothing bound or evaluated)
     or else bound and prepared: its device count, its count, whether it reads a placeholder, and
     its device inputs with the buffers bound in place of placeholders and ranges. *)
  let planned site (plan : P.plan) (args : E.value array) ~live =
    let device = ref None and agree = ref true and n = ref max_int and held = ref false in
    for s = 0 to Array.length plan.plan_sources - 1 do
      let i, mul, div =
        match plan.plan_sources.(s) with
        | P.Argument i -> (i, 1, 1)
        | Argument_range (i, mul, div) -> (i, mul, div)
      in
      match find args.(i) with
      | Some p -> (
          held := true;
          n := min !n (p.length * mul / div);
          match placeholder_count p with
          | Some (c, m, d) -> (
              match !device with
              | None -> device := Some (c, m * mul, d * div)
              | Some (c', m', d') ->
                  if not (c' == c && m' = m * mul && d' = d * div) then agree := false)
          | None -> ())
      | None -> n := min !n (Flow.Value.array_length args.(i) * mul / div)
    done;
    let inputs =
      match site.inputs with
      | Some inputs -> inputs
      | None ->
          let widths, code = Option.get site.shape in
          let inputs : P.inputs =
            {
              arrays = Array.make (Array.length plan.plan_sources) [||];
              uniforms = Array.map (fun w -> Array.make w 0.) widths;
              tables = Array.make (Array.length plan.plan_uniforms) [||];
              frame = Array.make (Array.length code) 0.;
              count = 0;
            }
          in
          site.inputs <- Some inputs;
          inputs
    in
    let bound_source s =
      match plan.plan_sources.(s) with
      | P.Argument i -> bound args.(i)
      | Argument_range _ -> iota ~integer:(integer_source (snd (Option.get site.shape)) s) !n
    in
    let bound_table u =
      Option.map
        (fun b -> (b, Array.length inputs.tables.(u)))
        (bound args.(plan.plan_uniforms.(u)))
    in
    for s = 0 to Array.length plan.plan_sources - 1 do
      inputs.arrays.(s) <-
        (match plan.plan_sources.(s) with
        | P.Argument i when find args.(i) = None -> Option.value ~default:[||] (P.floats args.(i))
        | _ -> [||])
    done;
    for u = 0 to Array.length plan.plan_uniforms - 1 do
      let v = args.(plan.plan_uniforms.(u)) in
      match P.floats v with
      | Some table -> inputs.tables.(u) <- table
      | None -> (
          inputs.tables.(u) <- [||];
          let c = inputs.uniforms.(u) in
          match v with
          | E.Vec2 (x, y) ->
              c.(0) <- x;
              c.(1) <- y
          | Vec3 (x, y, z) ->
              c.(0) <- x;
              c.(1) <- y;
              c.(2) <- z
          | Vec4 (x, y, z, w) ->
              c.(0) <- x;
              c.(1) <- y;
              c.(2) <- z;
              c.(3) <- w
          | v -> c.(0) <- Flow.Value.num v)
    done;
    Array.iter (fun (i, value) -> inputs.frame.(i) <- value live) plan.plan_frames;
    if not !agree then Error ()
    else Ok (!device, (if !n = max_int then 0 else !n), !held, inputs, bound_source, bound_table)
  in
  let prepared packed ~live =
    match device_count packed with
    | Error () -> Error ()
    | Ok count -> (
        let shapes = P.source_shapes packed in
        (* a range source is the device's index buffer: its length is known, it is not built *)
        let lengths i =
          match shapes.(i) with
          | P.Range (v, mul, div) ->
              let n = match find v with Some p -> p.length | None -> Flow.Value.array_length v in
              Some (n * mul / div)
          | _ -> None
        in
        let indices = P.index_sources packed in
        match P.prepare_values ~index:uint32 ~lengths packed ~live with
        | Error _ -> Error ()
        | Ok values ->
            let resident_input = Array.exists (fun v -> find v <> None) in
            let input i =
              match (bound values.arrays.(i), shapes.(i)) with
              | (Some _ as b), _ -> b
              | None, Range _ -> iota ~integer:(integer_source packed.code i) values.count
              | None, _ when uint32 && indices.(i) ->
                  iota ~integer:(integer_source packed.code i) values.count
              | None, _ -> None
            in
            (* a bound input is not uploaded: it needs no float copy, only its length *)
            let bound_inputs = Array.mapi (fun i _ -> input i) values.arrays in
            let inputs =
              P.device_inputs
                {
                  values with
                  arrays =
                    Array.mapi
                      (fun i v -> if bound_inputs.(i) = None then v else E.Float_array [||])
                      values.arrays;
                }
            in
            Ok
              ( count,
                values.count,
                resident_input values.arrays || resident_input values.uniforms,
                inputs,
                (fun i -> bound_inputs.(i)),
                fun u ->
                  Option.map
                    (fun b -> (b, Array.length inputs.tables.(u)))
                    (bound values.uniforms.(u)) ))
  in
  (* [packed] is the region with its arguments unbound ([args] are them) *)
  let off (packed : Flow_ir.Packed.t) reason =
    let _, path, _ = Flow_ir.Packed.site packed in
    Flow_ir.Fallback.note ~site:(String.concat "/" path) reason;
    None
  in
  let invoke packed args ~locals ~live =
    if (not (live_ok t)) || not fast then None
    else
      let site = site packed in
      if site.plan = None then begin
        site.plan <- Some (P.plan packed);
        site.shape <- Some ((P.view packed).uniform_widths, packed.code)
      end;
      match site.run with
      | None ->
          off packed
            (match Flow_ir.Packed.gpu_refusals ~fused:true ~fast:true packed with
            | d :: _ -> d.message
            | [] -> "its output type or kernel is refused")
      | Some run -> (
          let level =
            match site.plan with
            | Some (Some plan) -> planned site plan args ~live
            | _ -> (
                match P.bind_arguments ~validate:false packed args with
                | Ok packed -> prepared packed ~live
                | Error _ -> Error ())
          in
          match level with
          | Error () -> off packed "its inputs could not be bound"
          | Ok (count, n, held, inputs, bound_source, bound_table) -> (
              let slot = slot site ~locals and order = !invoked in
              incr invoked;
              (* the device sizes a counted dispatch's grid ([Run.dispatch ~count]), but bounds
                 double each level while real counts level off, and placeholders and buffers are
                 sized by the capacity: the slot's from earlier draws, or without one, past 256K
                 the device count is read (4 bytes, one wait) and becomes the bound; the data
                 stays put *)
              let n, clamped, length =
                match count with
                | Some _ when slot.capacity > 0 && order < !exact && not !post ->
                    ( min n slot.capacity,
                      (if slot.capacity < n then order else max_int),
                      slot.capacity )
                | Some (cell, mul, div) when n > 1 lsl 18 -> (
                    match flush () with
                    | Error d -> Flow.Value.raise_diagnostic d
                    | Ok () -> (
                        match read_count cell ~epoch:!epoch with
                        | Ok k ->
                            let n = min n (k * mul / div) in
                            (n, max_int, n)
                        | Error d -> Flow.Value.raise_diagnostic d))
                | _ -> (n, max_int, n)
              in
              (* from 256 elements a level's device work (no wait) is cheaper than the CPU's,
                 and a site whose deeper levels went to the device keeps its small ones there
                 too, so its arrays never cross; a capacity past 64M (upper bounds double with
                 each level) settles the counts on the host instead. Metal dispatches a grid of
                 any size, so the web's 4M reach (65,535 groups of 64) is not a limit here *)
              if n = 0 || not (held || n >= 256 || Dynarray.exists (fun s -> s.capacity > 0) site.slots)
              then None
              else if n > 1 lsl 26 then off packed "over 64M elements"
              else
                match commands () with
                | Error _ -> None
                | Ok (commands, encoder) -> (
                    let ty = packed.term.ty in
                    let width =
                      match ty with
                      | Flow.Ty.Array Vec2 -> 2
                      | Array Vec3 -> 3
                      | Array Vec4 -> 4
                      | _ -> 1
                    in
                    match (alloc slot (max 4 (length * width * 4)), alloc slot 24) with
                    | Error _, b | b, Error _ ->
                        Result.iter (give_back slot) b;
                        None
                    | Ok output, Ok cell -> (
                        let reset_status = not (List.exists (( == ) run) !status_runs) in
                        match
                          Run.dispatch ~uint32_cpu:uint32 ~commands ~encoder ~reset_status ?count
                            ~into:(fst output, fst cell)
                            ~bound:bound_source ~bound_table run { inputs with count = n }
                        with
                        | Error diagnostic ->
                            give_back slot output;
                            give_back slot cell;
                            Flow.Value.raise_diagnostic diagnostic
                        | Ok _ ->
                            settled := false;
                            if reset_status then status_runs := run :: !status_runs;
                            Some
                              (placeholder ~counted:(count <> None) ~clamped ?origin:count slot ty
                                 output (Some cell) length)))))
  in
  let compact mask =
    match find mask with
    | None -> None
    | Some p when uint32 && not p.uint32 -> None
    | Some p -> (
        let kept = alloc p.slot (max 4 (p.length * 4))
        and total = alloc p.slot 4
        and blocks = alloc p.slot (Run.compact_blocks p.length)
        and groups = alloc p.slot 12 in
        let give_all () =
          List.iter (Result.iter (give_back p.slot)) [ kept; total; blocks; groups ]
        in
        match (commands (), kept, total, blocks, groups) with
        | Ok (commands, encoder), Ok kept, Ok total, Ok blocks, Ok groups -> (
            match
              Run.compact ~uint32:p.uint32 ~encoder commands t.pipelines ~mask:(fst p.buffer)
                ~count:p.length ~count_in:(Option.map fst p.cell) ~kept:(fst kept)
                ~total:(fst total) ~blocks:(fst blocks) ~groups:(fst groups)
            with
            | Error _ ->
                give_all ();
                None
            | Ok () -> (
                settled := false;
                p.slot.spent <- blocks :: groups :: p.slot.spent;
                let v = placeholder p.slot (Flow.Ty.Array Flow.Ty.Int) kept (Some total) p.length in
                match flush () with Ok () -> Some v | Error d -> Flow.Value.raise_diagnostic d))
        | _ ->
            give_all ();
            None)
  in
  (* the device keeps integers as floats, so a retyped placeholder shares its buffer and count *)
  let coerce ty v =
    match (ty, find v) with
    | Flow.Ty.Array Flow.Ty.Float, Some p when not p.uint32 ->
        let v = E.Float_array (pooled p.slot 1 p.length (fun () -> Array.create_float (max 1 p.length))) in
        let alias = { p with key = Option.get (raw v); kind = 1 } in
        table := alias :: !table;
        p.slot.held <- alias :: p.slot.held;
        Some v
    | Flow.Ty.Array Flow.Ty.Int, Some p when p.uint32 -> Some v
    | _ -> None
  in
  let get = function Ok x -> x | Error d -> Flow.Value.raise_diagnostic d in
  let count p =
    match p.cell with
    | None -> p.length
    | Some (cell, _) -> min p.length (get (read_count cell ~epoch:!epoch) * p.mul / p.div)
  in
  (* the encoded work completed, no count cut and no status raised: what the host reads rests on
     every count before it, so a cut is checked before the statuses a cut array may raise *)
  let settle () =
    if not !settled then begin
      get (flush ());
      let first =
        List.fold_left
          (fun first p -> if p.clamped < first && count p = p.length then p.clamped else first)
          max_int !table
      in
      if first < max_int then begin
        cut := first;
        status_runs := [];
        raise Flow_ir.Core_exec.Resident_overflow
      end;
      let runners = !status_runs in
      status_runs := [];
      List.iter (fun run -> get (Run.Private.check_status run ~epoch:!epoch)) runners;
      settled := true
    end
  in
  let finish v =
    match find v with
    | None -> v
    | Some p -> (
        settle ();
        match List.find_opt (fun (key, _) -> key == p.key) !integers,
              List.find_opt (fun (key, _) -> key == p.key) !floats with
        | Some (_, (holder : Run.uint32_holder)), _ -> E.Int_array (holder.materialise ())
        | _, Some (_, (holder : Flow_ir.Gpu.planes)) ->
            let planes = holder.Flow_ir.Gpu.materialise () in
            let data = Array.init (holder.count * holder.width)
                (fun i -> planes.(i mod holder.width).(i / holder.width)) in
            (match holder.width with
            | 1 -> E.Float_array data | 2 -> Vec2_array data | 3 -> Vec3_array data | _ -> Vec4_array data)
        | None, None ->
        let length = count p in
        let width =
          match v with Vec2_array _ -> 2 | Vec3_array _ -> 3 | Vec4_array _ -> 4 | _ -> 1
        in
        if p.uint32 then begin
          let values = get (Run.read_uint32 queue (fst p.buffer) ~epoch:!epoch ~length) in
          t.resident_payload_bytes <- t.resident_payload_bytes + (length * 4);
          E.Int_array values
        end
        else
          let xs =
            get (Run.read_floats queue (fst p.buffer) ~epoch:!epoch ~length:(length * width))
          in
          t.resident_payload_bytes <- t.resident_payload_bytes + (length * width * 4);
          match v with
          | E.Int_array _ -> E.Int_array (Array.map int_of_float xs)
          | Float_array _ -> Float_array xs
          | Vec2_array _ -> Vec2_array xs
          | Vec3_array _ -> Vec3_array xs
          | _ -> Vec4_array xs)
  in
  let close () =
    ignore (flush ());
    if !epoch > Ogpu.Backend.completed_epoch queue then
      ignore (Ogpu.Backend.complete_through queue !epoch);
    List.iter (fun finish -> finish ()) !on_close;
    on_close := [];
    (* the device is done, so reading a counted slot's count waits for nothing: it is the slot's
       history (a cut count is its capacity, so the slot doubles; the run that follows records the
       rest) *)
    List.iter
      (fun p ->
        match p.cell with
        | Some (cell, _) when p.counted -> (
            match read_count cell ~epoch:!epoch with
            | Ok n -> p.slot.capacity <- max p.slot.capacity (above n)
            | Error _ -> ())
        | _ -> ())
      !table;
    t.showing <- None;
    post := false;
    settled := false;
    let touched_slots =
      List.concat_map (fun site -> Dynarray.to_list site.slots) !touched @ !extra_slots
    in
    extra_slots := [];
    touched := [];
    invoked := 0;
    exact := !cut;
    cut := max_int;
    (* back to their slots (a retyped placeholder shares its buffers), after what a slot kept and
       this run did not take, a size it outgrew, is destroyed; a slot its site no longer keeps
       destroys them *)
    let buffers =
      List.fold_left
        (fun bs ((_, (b, _)) as x) ->
          if List.exists (fun (_, (b', _)) -> b' == b) bs then bs else x :: bs)
        []
        (List.concat_map (fun s -> List.map (fun b -> (s, b)) (s.spent @ s.spare)) touched_slots
        @ List.concat_map
            (fun p ->
              if not p.owned then []
              else if p.dead then Option.fold ~none:[] ~some:(fun b -> [ (p.slot, b) ]) p.cell
              else List.map (fun b -> (p.slot, b)) (p.buffer :: Option.to_list p.cell))
            !table)
    in
    List.iter purge
      (List.fold_left
         (fun slots (s, _) -> if s.kept && not (List.memq s slots) then s :: slots else slots)
         [] buffers);
    List.iter
      (fun (slot, b) ->
        if slot.kept then give_back slot b else ignore (Ogpu.Backend.destroy_buffer (fst b)))
      buffers;
    List.iter
      (fun p ->
        if p.slot.kept && not p.dead then
          let k = (p.kind, p.length) in
          Mem.charge placeholders (placeholder_bytes k);
          Hashtbl.replace p.slot.arrays k
            (p.key :: Option.value ~default:[] (Hashtbl.find_opt p.slot.arrays k)))
      !table;
    List.iter
      (fun s ->
        if s.kept then
          List.iter
            (fun (k, a) ->
              Mem.charge placeholders (placeholder_bytes k);
              Hashtbl.replace s.arrays k
                (a :: Option.value ~default:[] (Hashtbl.find_opt s.arrays k)))
            s.spare_arrays;
        s.held <- [];
        s.spent <- [];
        s.spare <- [];
        s.spare_arrays <- [])
      touched_slots;
    table := [];
    integers := [];
    floats := []
  in
  (* after the loop: [array/range] of a kept array's count over [div] is the index buffer, counted
     by that array's cell *)
  let range_slot =
    lazy
      {
        capacity = 0;
        kept = false;
        free = Hashtbl.create 4;
        arrays = Hashtbl.create 4;
        held = [];
        spent = [];
        spare = [];
        spare_arrays = [];
      }
  in
  let range ty v ~div ~length =
    if uint32 && ty <> Flow.Ty.Array Flow.Ty.Int then None
    else
      match (v, find v) with
      | E.Int n, _ when uint32 && n >= 0 && div > 0 && length = n / div -> (
          match iota ~integer:true length with
          | None -> None
          | Some buffer ->
              let slot = Lazy.force range_slot in
              if not (List.memq slot !extra_slots) then extra_slots := slot :: !extra_slots;
              let value = placeholder slot ty (buffer, 0) None length in
              (match !table with
              | p :: rest -> table := { p with owned = false } :: rest
              | [] -> assert false);
              Some value)
      | _, Some p when !post && p.cell <> None && div > 0 -> (
          match iota length with
          | None -> None
          | Some buffer ->
              let kind, a =
                match ty with
                | Flow.Ty.Array Flow.Ty.Float ->
                    let a = pooled p.slot 1 length (fun () -> Array.create_float (max 1 length)) in
                    (1, E.Float_array a)
                | _ -> (0, E.Int_array (pooled p.slot 0 length (fun () -> Array.make (max 1 length) 0)))
              in
              table :=
                {
                  p with
                  key = Option.get (raw a);
                  kind;
                  buffer = (buffer, 0);
                  length;
                  counted = false;
                  clamped = max_int;
                  div = p.div * div;
                  owned = false;
                  dead = false;
                  origin = Option.map (fun (c, m, d) -> (c, m, d * div)) p.origin;
                }
                :: !table;
              Some a)
      | _ -> None
  in
  let fill ty ~length value =
    match ty, value with
    | Flow.Ty.Array Flow.Ty.Int, E.Int value
      when uint32 && value >= 0 && value <= 4294967295 && length >= 0
           && length <= Sys.max_string_length / 4 ->
        let slot = Lazy.force range_slot in
        if not (List.memq slot !extra_slots) then extra_slots := slot :: !extra_slots;
        let buffer = get (alloc slot (max 4 (length * 4))) in
        (try
           get (flush ());
           get (Run.fill_indices ~value queue t.pipelines (fst buffer) ~count:length);
           Some (placeholder slot ty buffer None length)
         with exn -> slot.spent <- buffer :: slot.spent; raise exn)
    | _ -> None
  in
  (* A mark attribute over kept arrays, drawn by its site's [shown] runner: bound to their buffers,
     its grid and count record from their device count, committed after the fold's last level.
     [None]: it reads none; [Some None]: it reads one but cannot run here. *)
  let show packed ~live =
    if uint32 then Some None
    else
      let held = function P.Value v | Range (v, _, _) -> find v <> None | Opaque _ -> false in
      if
        not
          (Array.exists held (P.source_shapes packed) || Array.exists held (P.uniform_shapes packed))
      then None
      else if (not (live_ok t)) || not !post then Some None
      else
        let site = site packed in
        Some
          (match (site.run, device_count packed) with
          | None, _ | _, Error () -> None
          | Some _, Ok count -> (
              let shapes = P.source_shapes packed in
              let lengths i =
                match shapes.(i) with
                | P.Range (v, mul, div) ->
                    let n =
                      match find v with Some p -> p.length | None -> Flow.Value.array_length v
                    in
                    Some (n * mul / div)
                | _ -> None
              in
              match P.prepare_values ~lengths packed ~live with
              | Error _ -> None
              | Ok values when values.count = 0 -> None
              | Ok values -> (
                  let runner =
                    match site.shown with
                    | Some runner -> Ok runner
                    | None ->
                        Result.map
                          (fun msl ->
                            let runner =
                              {
                                identity = Atomic.fetch_and_add next 1;
                                run = Run.create ~pool:t.shared t.gpu t.pipelines msl;
                                stamp = 0L;
                                output = None;
                              }
                            in
                            incr t.created;
                            site.shown <- Some runner;
                            runner)
                          (Emit.kernel ~fast:true packed)
                  in
                  let input i =
                    match (bound values.arrays.(i), shapes.(i)) with
                    | (Some _ as b), _ -> b
                    | None, Range _ -> iota values.count
                    | None, _ -> None
                  in
                  let bound_inputs = Array.mapi (fun i _ -> input i) values.arrays in
                  let inputs =
                    P.device_inputs
                      {
                        values with
                        arrays =
                          Array.mapi
                            (fun i v -> if bound_inputs.(i) = None then v else E.Float_array [||])
                            values.arrays;
                      }
                  in
                  match (runner, flush (), admit t) with
                  | Error _, _, _ | _, Error _, _ | _, _, Error _ -> None
                  | Ok runner, Ok (), Ok () -> (
                      runner.output <- None;
                      match
                        Run.dispatch ~wait:false ?count
                          ~bound:(fun i -> bound_inputs.(i))
                          ~bound_table:(fun u ->
                            Option.map
                              (fun b -> (b, Array.length inputs.tables.(u)))
                              (bound values.uniforms.(u)))
                          runner.run inputs
                      with
                      | Error _ -> None
                      | Ok output ->
                          record t (Run.epoch output);
                          runner.output <- Some output;
                          runner.stamp <- Int64.succ runner.stamp;
                          Some
                            G.
                              {
                                identity = runner.identity;
                                count = Run.count output;
                                width = Run.width output;
                                output = 0;
                                stamp = runner.stamp;
                                gpu_seconds = Run.gpu_seconds output;
                              }))))
  in
  let count_value value =
    match find value with
    | None -> None
    | Some p ->
        settle ();
        Some (count p)
  in
  let owner () =
    match t.ownership with
    | Some run -> run
    | None ->
        let run = Run.create ~pool:t.shared t.gpu t.pipelines Kernels.indices in
        incr t.created;
        t.ownership <- Some run;
        run
  in
  let detach p =
    let buffer = fst p.buffer in
    let same b = fst b == buffer in
    if Option.fold ~none:false ~some:same t.iota then t.iota <- None;
    if Option.fold ~none:false ~some:same t.iota_uint32 then t.iota_uint32 <- None;
    t.retired_buffers <- List.filter (fun b -> b != buffer) t.retired_buffers;
    List.iter (fun alias ->
        if fst alias.buffer == buffer then begin
          if alias.owned then
            Option.iter (fun cell -> on_close :=
                (fun () -> alias.slot.spent <- cell :: alias.slot.spent) :: !on_close) alias.cell;
          alias.owned <- false;
          alias.slot.held <- List.filter (fun p -> fst p.buffer != buffer) alias.slot.held
        end) (!table @ p.slot.held)
  in
  let retain_uint32 value =
    if not (live t) then None else
    match raw value with
    | None -> None
    | Some key ->
        (match List.find_opt (fun (k, _) -> k == key) !integers with
        | Some (_, holder) -> Some holder
        | None ->
            match find value with
            | Some p when p.uint32 ->
                settle ();
                let aliases = List.filter (fun a -> not a.dead && fst a.buffer == fst p.buffer) !table
                    |> List.map (fun a -> a, count a) in
                let length = List.fold_left (fun n (_, count) -> max n count) 0 aliases in
                let holder = get (Run.adopt_uint32 (owner ()) ~buffer:(fst p.buffer) ~count:length) in
                Option.iter (fun (_, _, _, unpin) -> on_close := unpin :: !on_close) (holder.pin ());
                let views = ref [] in
                let view count =
                  match List.assoc_opt count !views with
                  | Some view -> view
                  | None ->
                      let view = if List.for_all (fun (_, n) -> n = length) aliases then holder else
                          let cached = Atomic.make None in
                          { holder with count;
                            materialise = (fun () -> match Atomic.get cached with
                              | Some values -> values
                              | None ->
                                  let values = if count = 0 then [||] else
                                      let values = holder.materialise () in
                                      if count = length then values else Array.sub values 0 count in
                                  ignore (Atomic.compare_and_set cached None (Some values));
                                  Option.get (Atomic.get cached));
                            (* ponytail: shared views retire together; reference-count releases if GC pressure warrants it. *)
                            release = (fun ~held:_ -> holder.release ~held:true);
                            pin = (fun () -> Option.map (fun (b, _, valid, unpin) -> b, count, valid, unpin)
                                (holder.pin ())) } in
                      views := (count, view) :: !views;
                      view
                in
                integers := List.map (fun (a, count) -> (a.key, view count)) aliases @ !integers;
                detach p;
                Some (snd (List.find (fun (key, _) -> key == p.key) !integers))
            | _ -> None)
  in
  let retain_float value =
    if not (live t) then None else
    match raw value with
    | None -> None
    | Some key ->
        (match List.find_opt (fun (k, _) -> k == key) !floats with
        | Some (_, holder) -> Some holder
        | None ->
            match find value with
            | Some p when not p.uint32 && p.kind > 0 ->
                settle ();
                let aliases = List.filter (fun a -> not a.dead && fst a.buffer == fst p.buffer) !table in
                let holder = get (Run.adopt_float (owner ()) ~buffer:(fst p.buffer)
                                    ~count:(count p) ~width:p.kind) in
                Option.iter (function
                    | E.Float_array a | Vec2_array a | Vec3_array a | Vec4_array a ->
                        Option.iter (fun (_, _, _, unpin) -> on_close := unpin :: !on_close) (Run.pin a)
                    | _ -> ()) holder.resident;
                floats := List.map (fun a -> (a.key, holder)) aliases @ !floats;
                detach p;
                Some holder
            | _ -> None)
  in
  let settle () =
    settle ();
    post := true;
    t.showing <- Some show
  in
  { executor = { invoke; compact; coerce; finish; count = count_value; close; settle; range; fill };
    retain_uint32; retain_float;
    uint32_stream = (fun value -> match find value with
        | Some p when p.uint32 -> settle (); Some (fst p.buffer, count p)
        | _ -> None) }

let resident ?uint32 ~fast t = (resident_scope ?uint32 ~fast t).executor

let show t packed ~live = match t.showing with Some show -> show packed ~live | None -> None

let validation_status t =
  match t.validation_status with
  | Some buffer -> Ok buffer
  | None ->
      Result.map
        (fun buffer ->
          t.validation_status <- Some buffer;
          t.resident_buffers <- t.resident_buffers + 1;
          buffer)
        (Result.map_error Run.Private.diagnostic
           (Ogpu.Backend.create_buffer (Renderer.gpu_device t.gpu)
              { label = Some "Flow topology validation"; size = 36L;
                usage = [ Storage; Copy_src ] }))

let validate_topology t ~vertices ~offsets ~kinds ~point_count ~max_polygon_size =
  if not (live t) then error "GPU host is closed or called from another domain."
  else
    Result.bind (validation_status t) (fun status ->
        Run.validate_topology
          ~on_read:(fun bytes -> t.validation_bytes <- t.validation_bytes + bytes)
          (Renderer.gpu_queue t.gpu) t.pipelines ~vertices ~offsets ~kinds ~status
          ~point_count ~max_polygon_size)

let close t =
  if Domain.self () <> t.domain then invalid_arg "Host.close: creating domain required";
  if not t.closed then begin
    t.discarding := true;
    Cache.clear t.runners;
    Lru.Identity_list.clear t.sites;
    Option.iter (fun run ->
        Run.close ~discard:true run;
        t.retired := { (add_run !(t.retired) run) with
                      runners_released = !(t.retired).runners_released + 1 }) t.ownership;
    t.ownership <- None;
    Run.close_pool t.shared;
    List.iter
      (fun b -> ignore (Ogpu.Backend.destroy_buffer b))
      (Option.fold ~none:[] ~some:(fun (b, _) -> [ b ]) t.iota
      @ Option.fold ~none:[] ~some:(fun (b, _) -> [ b ]) t.iota_uint32
      @ t.retired_buffers);
    t.iota <- None;
    t.iota_uint32 <- None;
    Option.iter (fun b -> ignore (Ogpu.Backend.destroy_buffer b)) t.validation_status;
    t.validation_status <- None;
    t.retired_buffers <- [];
    Hashtbl.iter
      (fun _ bs -> List.iter (fun b -> ignore (Ogpu.Backend.destroy_buffer b)) bs)
      t.buffers;
    Hashtbl.reset t.buffers;
    Lru.Identity_list.clear t.tables;
    Pipelines.close t.pipelines;
    t.closed <- true
  end

(* Shared display storage: at most four free buffers per size and 64 sizes. *)
let return_display_buffer t bytes out =
  if Domain.self () <> t.domain then invalid_arg "Host display buffer released on another domain";
  let free = Option.value ~default:[] (Hashtbl.find_opt t.buffers bytes) in
  if live t && List.length free < 4
     && (Hashtbl.mem t.buffers bytes || Hashtbl.length t.buffers < 64)
  then Hashtbl.replace t.buffers bytes (out :: free)
  else ignore (Ogpu.Backend.destroy_buffer out)

let display_buffer t bytes =
  match Hashtbl.find_opt t.buffers bytes with
  | Some (b :: rest) -> Hashtbl.replace t.buffers bytes rest; Ok b
  | _ -> Result.map (fun buffer -> t.resident_buffers <- t.resident_buffers + 1; buffer)
      (Result.map_error Run.Private.diagnostic
        (Ogpu.Backend.create_buffer (Renderer.gpu_device t.gpu)
          { label = Some "Flow display storage"; size = Int64.of_int bytes; usage = [ Storage; Index ] }))

let triangle_indices t ~positions ~vertices:((_, nv) as vertices) ~offsets ~kinds:((_, np) as kinds) =
  if not (live t) then error "GPU host is closed or called from another domain."
  else if np <= 0 || np > (1 lsl 28) || nv < 3 * np || nv > 4 * np || nv > Sys.max_string_length / 12 then
    Error (Diagnostic.error ~code:"E_GPU_DISPLAY_TRIANGLES" "Resident display requires nonempty triangle/quad topology.")
  else
    Result.bind (validation_status t) (fun status ->
      let bytes = 3 * (nv - 2 * np) * 4 and scratch = ((np + 255) / 256) * 4 in
      Result.bind (display_buffer t bytes) (fun out ->
        let result = Result.bind (display_buffer t scratch) (fun blocks ->
          Fun.protect ~finally:(fun () -> return_display_buffer t scratch blocks) (fun () ->
            Run.triangle_indices ~on_read:(fun bytes -> t.validation_bytes <- t.validation_bytes + bytes)
              (Renderer.gpu_queue t.gpu) t.pipelines ~positions ~vertices ~offsets ~kinds
              ~indices:out ~blocks ~status)) in
        match result with
        | Error d -> return_display_buffer t bytes out; Error d
        | Ok count ->
            let released = ref false in
            Ok (out, count, fun () -> if not !released then begin
              released := true; return_display_buffer t bytes out end)))

(* A triangle mesh's display normals from its resident P (shaders/normals.rays), on the host's queue
   and waited for. The corners and each point's corners in ascending order (a counting sort: the
   CSR) are uploaded once per corners array; the normals buffer comes from the host's buffers by
   size and goes back at its release. *)
let normals t ~positions ~count ~corners =
  let module B = Ogpu.Backend in
  let ( let* ) = Option.bind in
  let device = Renderer.gpu_device t.gpu and queue = Renderer.gpu_queue t.gpu in
  let upload label values =
    let bytes = Bytes.create (4 * max 1 (Array.length values)) in
    Array.iteri (fun i v -> Bytes.set_int32_le bytes (i * 4) (Int32.of_int v)) values;
    let* buffer =
      Result.to_option
        (B.create_buffer device
           {
             label = Some label;
             size = Int64.of_int (Bytes.length bytes);
             usage = [ Storage; Copy_dst ];
           })
    in
    if Result.is_ok (B.write_buffer buffer ~offset:0L bytes) then Some buffer
    else (
      ignore (B.destroy_buffer buffer);
      None)
  in
  let tables () =
    let offsets = Array.make (count + 1) 0 in
    Array.iter (fun p -> offsets.(p + 1) <- offsets.(p + 1) + 1) corners;
    (* the stage's loop stops at 4,096 corners a point *)
    if Array.exists (fun n -> n > 4096) offsets then None
    else begin
      for p = 1 to count do
        offsets.(p) <- offsets.(p) + offsets.(p - 1)
      done;
      let next = Array.sub offsets 0 count and vertices = Array.make (Array.length corners) 0 in
      Array.iteri
        (fun v p ->
          vertices.(next.(p)) <- v;
          next.(p) <- next.(p) + 1)
        corners;
      let made =
        List.map
          (fun (label, a) -> upload label a)
          [
            ("Flow display corners", corners);
            ("Flow display point offsets", offsets);
            ("Flow display point corners", vertices);
          ]
      in
      if List.for_all Option.is_some made then Some (List.map Option.get made)
      else (
        List.iter (Option.iter (fun b -> ignore (B.destroy_buffer b))) made;
        None)
    end
  in
  let bytes = count * 12 in
  let give_back = return_display_buffer t bytes in
  let run out corners offsets vertices =
    let* compiled = Result.to_option (Pipelines.get t.pipelines Kernels.normals) in
    let* commands = Result.to_option (B.begin_commands queue) in
    let encoded =
      let* encoder = Result.to_option (B.compute_encoder commands) in
      let n = Bytes.create 4 in
      Bytes.set_int32_le n 0 (Int32.of_int count);
      let set =
        List.for_all Result.is_ok
          [
            B.set_pipeline encoder compiled.pipeline;
            B.set_buffer encoder ~index:0 positions;
            B.set_buffer encoder ~index:1 corners;
            B.set_buffer encoder ~index:2 offsets;
            B.set_buffer encoder ~index:3 vertices;
            B.set_buffer encoder ~index:4 out;
            B.set_bytes encoder ~index:5 n;
          ]
        && Result.is_ok (B.dispatch_threads encoder ~threads:(count, 1, 1) ~threadgroup:(256, 1, 1))
      in
      if Result.is_ok (B.end_compute encoder) && set then Some () else None
    in
    match encoded with
    | None ->
        ignore (B.abandon commands);
        None
    | Some () ->
        let* receipt = Result.to_option (B.commit commands) in
        Result.to_option (B.complete_through queue receipt.epoch)
  in
  if (not (live t)) || count <= 0 || Array.length corners mod 3 <> 0 then None
  else
    let tables =
      match Lru.Identity_list.find t.tables corners with
      | Some tables -> tables
      | None ->
          let made = tables () in
          Lru.Identity_list.add t.tables corners made;
          made
    in
    match tables with
    | Some [ corners; offsets; vertices ] -> (
        let out = Result.to_option (display_buffer t bytes) in
        let* out = out in
        match run out corners offsets vertices with
        | Some () ->
            let released = ref false in
            Some
              ( out,
                fun () ->
                  if not !released then begin
                    released := true;
                    give_back out
                  end )
        | None ->
            give_back out;
            None)
    | _ -> None

let stats t =
  if Domain.self () <> t.domain then invalid_arg "Host.stats: creating domain required";
  let totals = ref !(t.retired) in
  Cache.iter t.runners (fun _ runner -> totals := add_run !totals runner.run);
  Option.iter (fun run -> totals := add_run !totals run) t.ownership;
  (* a resident fold's runners and buffers count too: a frame's draw should make none *)
  Lru.Identity_list.iter t.sites (fun _ site ->
      Option.iter (fun run -> totals := add_run !totals run) site.run;
      Option.iter (fun (runner : runner) -> totals := add_run !totals runner.run) site.shown);
  {
    !totals with
    buffer_creations = !totals.buffer_creations + t.resident_buffers;
    readback_bytes = !totals.readback_bytes + t.resident_payload_bytes;
    payload_readback_bytes = !totals.payload_readback_bytes + t.resident_payload_bytes;
    count_readback_bytes = !totals.count_readback_bytes + t.count_bytes;
    validation_readback_bytes = !totals.validation_readback_bytes + t.validation_bytes;
    runners_created = !(t.created);
    pipeline_compilations = Pipelines.Private.compilations t.pipelines;
    pipeline_releases = Pipelines.Private.releases t.pipelines;
    held_buffers = (if t.closed then 0 else Run.Private.pool_held t.shared);
    freed = Run.Private.pool_freed t.shared;
  }

let forget_producer t identity =
  let found = ref None in
  Cache.iter t.runners (fun key runner -> if runner.identity = identity then found := Some key);
  Option.iter (Cache.remove t.runners) !found

let backend t =
  { (standard_backend t) with
    G.residency = (fun ~fast ->
        if not fast || not (live t) then None
        else
          let scope = resident_scope ~uint32:true ~fast t in
          let integers = ref [] in
          Some G.{
            executor = { scope.executor with close = (fun () -> scope.executor.close (); integers := []) };
            retain_uint32 = (fun value ->
                Option.map (fun (holder : Run.uint32_holder) ->
                    match List.find_opt (fun (native, _) -> native == holder) !integers with
                    | Some (_, retained) -> retained
                    | None ->
                        let retained = Run.integers holder in
                        integers := (holder, retained) :: !integers;
                        retained)
                  (scope.retain_uint32 value));
            retain_float = scope.retain_float;
            validate_topology = (fun ~point_count ~vertices ~offsets ~kinds ~max_polygon_size ->
                match scope.uint32_stream vertices, scope.uint32_stream offsets, scope.uint32_stream kinds with
                | Some vertices, Some offsets, Some kinds ->
                    Some (validate_topology t ~point_count ~vertices ~offsets ~kinds ~max_polygon_size)
                | _ -> None)
          }) }
