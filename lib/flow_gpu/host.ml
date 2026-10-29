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
    materialisations = stats.materialisations + Run.Private.materialisations run;
    held_buffers = stats.held_buffers;
  }

type t = {
  gpu : Renderer.gpu;
  pipelines : Pipelines.t;
  runners : runner Cache.t;
  created : int ref;
  retired : stats ref;
  sites : (Flow_ir.Packed.instruction array, Run.t option) Lru.Identity_list.t;
      (* resident fold runners by region site *)
  mutable flight : int64 list; (* fast dispatches' epochs, newest first *)
  pool : (int * int, Obj.t list) Hashtbl.t;
      (* resident placeholders by (kind, length), reused each draw *)
  mutable iota : (Ogpu.Backend.buffer * int) option;
      (* 0., 1., 2., ... on the device, for range sources *)
  mutable retired_buffers : Ogpu.Backend.buffer list;
      (* outgrown index buffers, destroyed at close *)
  buffers : (int, Ogpu.Backend.buffer list) Hashtbl.t;
      (* resident buffers by byte size, reused each draw *)
  mutable closed : bool;
  domain : Domain.id;
  shared : Run.pool;  (** the kept outputs' buffers of every runner, recycled across edits *)
  tables : (int array, Ogpu.Backend.buffer list option) Lru.Identity_list.t;
      (* a triangle mesh's corners and per-point corner CSR on the device, by its corners array *)
}

let next = Atomic.make 1

let create ~clock gpu =
  if not (Domain.is_main_domain ()) then invalid_arg "Host.create: initial domain required";
  let created = ref 0 and retired = ref zero in
  {
    gpu;
    pipelines = Pipelines.create ~clock (Renderer.gpu_device gpu);
    created;
    retired;
    sites = Lru.Identity_list.create ~equal:( == ) ~release:(fun _ -> Option.iter Run.close) 32;
    flight = [];
    pool = Hashtbl.create 16;
    iota = None;
    retired_buffers = [];
    buffers = Hashtbl.create 16;
    runners =
      Cache.create
        ~release:(fun _ runner ->
          runner.output <- None;
          Run.close runner.run;
          retired :=
            { (add_run !retired runner.run) with runners_released = !retired.runners_released + 1 })
        64;
    closed = false;
    domain = Domain.self ();
    shared = Run.pool ();
    tables =
      Lru.Identity_list.create ~equal:( == )
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
    match Cache.find_first t.runners (fun _ runner -> runner.identity = value.identity) with
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

let backend t : G.backend =
  {
    cost = (fun ~fast:_ _ ~count:_ -> None);
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
   region's value is a placeholder (zeros of its capacity, an upper bound) the table below maps,
   physically, to its buffer and to the count cell the device wrote, so the next region's count
   ([array/range] of an array's count) is taken on the device.  [finish] reads a count, then the
   array. *)
type placeholder = {
  key : Obj.t;
  kind : int;
  buffer : Ogpu.Backend.buffer * int;
  length : int;
  cell : (Ogpu.Backend.buffer * int) option;
}

let resident ~fast t : Flow_ir.Core_exec.resident =
  let module E = Flow.Eval in
  let module P = Flow_ir.Packed.Host in
  let queue = Renderer.gpu_queue t.gpu in
  let table = ref [] and epoch = ref 0L and status_runs = ref [] in
  let pending = ref None in
  let commands () =
    match !pending with
    | Some c -> Ok c
    | None ->
        Result.map
          (fun c ->
            pending := Some c;
            c)
          (Result.map_error Run.Private.diagnostic (Ogpu.Backend.begin_commands queue))
  in
  (* committed per level without waiting, so the device runs a level while the host encodes the
     next; at most two in flight (the queue admits three) *)
  let flush () =
    match !pending with
    | None -> Ok ()
    | Some c -> (
        pending := None;
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
  let find v = Option.bind (raw v) (fun key -> List.find_opt (fun p -> p.key == key) !table) in
  (* a draw's capacities repeat the last draw's, so placeholders come from the host's pool *)
  let pooled kind length make =
    match Hashtbl.find_opt t.pool (kind, length) with
    | Some (a :: rest) ->
        Hashtbl.replace t.pool (kind, length) rest;
        Obj.obj a
    | _ -> make ()
  in
  let placeholder ty buffer cell length =
    let v : E.value =
      match ty with
      | Flow.Ty.Array Flow.Ty.Int -> Int_array (pooled 0 length (fun () -> Array.make length 0))
      | Array Float -> Float_array (pooled 1 length (fun () -> Array.make length 0.))
      | Array Vec2 -> Vec2_array (pooled 2 length (fun () -> Array.make (2 * length) 0.))
      | Array Vec3 -> Vec3_array (pooled 3 length (fun () -> Array.make (3 * length) 0.))
      | _ -> Vec4_array (pooled 4 length (fun () -> Array.make (4 * length) 0.))
    in
    let kind =
      match v with
      | E.Int_array _ -> 0
      | Float_array _ -> 1
      | Vec2_array _ -> 2
      | Vec3_array _ -> 3
      | _ -> 4
    in
    table := { key = Option.get (raw v); kind; buffer; length; cell } :: !table;
    v
  in
  let iota = iota t in
  let bound v = Option.map (fun p -> fst p.buffer) (find v) in
  let give_back (b, bytes) =
    Hashtbl.replace t.buffers bytes
      (b :: Option.value ~default:[] (Hashtbl.find_opt t.buffers bytes))
  in
  (* a draw's buffer sizes repeat the last draw's; close hands them back *)
  let alloc bytes =
    match Hashtbl.find_opt t.buffers bytes with
    | Some (b :: rest) ->
        Hashtbl.replace t.buffers bytes rest;
        Ok (b, bytes)
    | _ ->
        Result.map
          (fun b -> (b, bytes))
          (Result.map_error Run.Private.diagnostic
             (Ogpu.Backend.create_buffer (Renderer.gpu_device t.gpu)
                {
                  label = Some "Flow resident";
                  size = Int64.of_int bytes;
                  usage = [ Storage; Copy_src; Copy_dst ];
                }))
  in
  (* a site's runner, or None when the device refuses its form; decided once per site *)
  let run packed =
    let code = (P.view packed).code in
    match Lru.Identity_list.peek t.sites code with
    | Some run -> run
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
                 (Emit.kernel ~fast:true packed))
        in
        (* the 32 most recent region sites are kept; a re-prepared drawing has new sites *)
        Lru.Identity_list.add t.sites code run;
        run
  in
  (* The device count bound of a region, if its sources allow the device to keep counts: [Error]
     when a source or uniform reads a placeholder in a form the host would evaluate over zeros or a
     capacity, or when two device counts bound it. *)
  let device_count packed =
    let opaque = function
      | P.Opaque None -> !table <> []
      | Opaque (Some vs) -> List.exists (fun v -> find v <> None) vs
      | Value _ | Range _ -> false
    in
    if Array.exists opaque (P.source_shapes packed) || Array.exists opaque (P.uniform_shapes packed)
    then Error ()
    else
      let counts =
        Array.to_list (P.source_shapes packed)
        |> List.filter_map (function
          | P.Value v -> Option.bind (find v) (fun p -> Option.map (fun (c, _) -> (c, 1, 1)) p.cell)
          | Range (v, mul, div) ->
              Option.bind (find v) (fun p -> Option.map (fun (c, _) -> (c, mul, div)) p.cell)
          | Opaque _ -> None)
      in
      match counts with
      | [] -> Ok None
      | (c, m, d) :: rest when List.for_all (fun (c', m', d') -> c' == c && m' = m && d' = d) rest
        ->
          Ok (Some (c, m, d))
      | _ -> Error ()
  in
  let invoke packed ~live =
    if (not (live_ok t)) || not fast then None
    else
      match run packed with
      | None -> None
      | Some run -> (
          match device_count packed with
          | Error () -> None
          | Ok count -> (
              let shapes = P.source_shapes packed in
              (* a range source is the device's index buffer: its length is known, it is not built *)
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
              | Ok values -> (
                  let resident_input = Array.exists (fun v -> find v <> None) in
                  (* from 256 elements a level's device work (no wait) is cheaper than the CPU's; a capacity past 4M
         (upper bounds double with each level) settles the counts on the host instead *)
                  if
                    values.count = 0
                    || values.count > 1 lsl 22
                    || not
                         (resident_input values.arrays || resident_input values.uniforms
                        || values.count >= 256)
                  then None
                  else
                    match commands () with
                    | Error _ -> None
                    | Ok commands -> (
                        let input i =
                          match (bound values.arrays.(i), shapes.(i)) with
                          | (Some _ as b), _ -> b
                          | None, Range _ -> iota values.count
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
                                  (fun i v ->
                                    if bound_inputs.(i) = None then v else E.Float_array [||])
                                  values.arrays;
                            }
                        in
                        let input i = bound_inputs.(i) in
                        let ty = packed.term.ty in
                        let width =
                          match ty with
                          | Flow.Ty.Array Vec2 -> 2
                          | Array Vec3 -> 3
                          | Array Vec4 -> 4
                          | _ -> 1
                        in
                        match (alloc (max 4 (values.count * width * 4)), alloc 24) with
                        | Error _, b | b, Error _ ->
                            Result.iter give_back b;
                            None
                        | Ok output, Ok cell -> (
                            let reset_status = not (List.exists (( == ) run) !status_runs) in
                            match
                              Run.dispatch ~commands ~reset_status ?count
                                ~into:(fst output, fst cell)
                                ~bound:input
                                ~bound_table:(fun u ->
                                  Option.map
                                    (fun b -> (b, Array.length inputs.tables.(u)))
                                    (bound values.uniforms.(u)))
                                run inputs
                            with
                            | Error diagnostic ->
                                give_back output;
                                give_back cell;
                                Flow.Value.raise_diagnostic diagnostic
                            | Ok _ ->
                                if reset_status then status_runs := run :: !status_runs;
                                Some (placeholder ty output (Some cell) values.count))))))
  in
  let compact mask =
    match find mask with
    | None -> None
    | Some p -> (
        match (commands (), alloc (max 4 (p.length * 4)), alloc 4) with
        | Error _, k, c ->
            Result.iter give_back k;
            Result.iter give_back c;
            None
        | _, Error _, c ->
            Result.iter give_back c;
            None
        | _, k, Error _ ->
            Result.iter give_back k;
            None
        | Ok commands, Ok kept, Ok total -> (
            match
              Run.compact commands t.pipelines ~mask:(fst p.buffer) ~count:p.length
                ~count_in:(Option.map fst p.cell) ~kept:(fst kept) ~total:(fst total)
            with
            | Error _ ->
                give_back kept;
                give_back total;
                None
            | Ok () -> (
                let v = placeholder (Flow.Ty.Array Flow.Ty.Int) kept (Some total) p.length in
                match flush () with Ok () -> Some v | Error d -> Flow.Value.raise_diagnostic d)))
  in
  (* the device keeps integers as floats, so a retyped placeholder shares its buffer and count *)
  let coerce ty v =
    match (ty, find v) with
    | Flow.Ty.Array Flow.Ty.Float, Some p ->
        let v = E.Float_array (pooled 1 p.length (fun () -> Array.make p.length 0.)) in
        table := { p with key = Option.get (raw v); kind = 1 } :: !table;
        Some v
    | _ -> None
  in
  let get = function Ok x -> x | Error d -> Flow.Value.raise_diagnostic d in
  let finish v =
    match find v with
    | None -> v
    | Some p -> (
        get (flush ());
        let runners = !status_runs in
        status_runs := [];
        List.iter (fun run -> get (Run.Private.check_status run ~epoch:!epoch)) runners;
        let length =
          match p.cell with
          | None -> p.length
          | Some (cell, _) -> min p.length (get (Run.read_count queue cell ~epoch:!epoch))
        in
        let width =
          match v with Vec2_array _ -> 2 | Vec3_array _ -> 3 | Vec4_array _ -> 4 | _ -> 1
        in
        let xs =
          get (Run.read_floats queue (fst p.buffer) ~epoch:!epoch ~length:(length * width))
        in
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
    (* the device is done with them: back to the pool (a retyped placeholder shares its buffers),
       at most four per size and 64 sizes *)
    if Hashtbl.length t.buffers > 64 then begin
      Hashtbl.iter
        (fun _ bs -> List.iter (fun b -> ignore (Ogpu.Backend.destroy_buffer b)) bs)
        t.buffers;
      Hashtbl.reset t.buffers
    end;
    List.iter
      (fun (b, bytes) ->
        let free = Option.value ~default:[] (Hashtbl.find_opt t.buffers bytes) in
        if List.length free < 4 then Hashtbl.replace t.buffers bytes (b :: free)
        else ignore (Ogpu.Backend.destroy_buffer b))
      (List.fold_left
         (fun bs (b, n) -> if List.exists (fun (b', _) -> b' == b) bs then bs else (b, n) :: bs)
         []
         (List.concat_map (fun p -> p.buffer :: Option.to_list p.cell) !table));
    (* placeholders go back to the pool, at most four per (kind, length) and 64 shapes *)
    if Hashtbl.length t.pool > 64 then Hashtbl.reset t.pool;
    List.iter
      (fun p ->
        let k = (p.kind, p.length) in
        let free = Option.value ~default:[] (Hashtbl.find_opt t.pool k) in
        if List.length free < 4 then Hashtbl.replace t.pool k (p.key :: free))
      !table;
    table := []
  in
  { invoke; compact; coerce; finish; close }

let close t =
  if Domain.self () <> t.domain then invalid_arg "Host.close: creating domain required";
  if not t.closed then begin
    Cache.clear t.runners;
    Run.close_pool t.shared;
    Lru.Identity_list.clear t.sites;
    List.iter
      (fun b -> ignore (Ogpu.Backend.destroy_buffer b))
      (Option.fold ~none:[] ~some:(fun (b, _) -> [ b ]) t.iota @ t.retired_buffers);
    t.iota <- None;
    t.retired_buffers <- [];
    Hashtbl.reset t.pool;
    Hashtbl.iter
      (fun _ bs -> List.iter (fun b -> ignore (Ogpu.Backend.destroy_buffer b)) bs)
      t.buffers;
    Hashtbl.reset t.buffers;
    Lru.Identity_list.clear t.tables;
    Pipelines.close t.pipelines;
    t.closed <- true
  end

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
  let give_back out =
    let free = Option.value ~default:[] (Hashtbl.find_opt t.buffers bytes) in
    if live t && List.length free < 4 then Hashtbl.replace t.buffers bytes (out :: free)
    else ignore (B.destroy_buffer out)
  in
  let run out corners offsets vertices =
    let* compiled = Result.to_option (Pipelines.get t.pipelines Emit.normals) in
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
        let out =
          match Hashtbl.find_opt t.buffers bytes with
          | Some (b :: rest) ->
              Hashtbl.replace t.buffers bytes rest;
              Some b
          | _ ->
              Result.to_option
                (B.create_buffer device
                   {
                     label = Some "Flow display normals";
                     size = Int64.of_int bytes;
                     usage = [ Storage ];
                   })
        in
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
  {
    !totals with
    runners_created = !(t.created);
    pipeline_compilations = Pipelines.Private.compilations t.pipelines;
    pipeline_releases = Pipelines.Private.releases t.pipelines;
    held_buffers = (if t.closed then 0 else Run.Private.pool_held t.shared);
    freed = Run.Private.pool_freed t.shared;
  }
