module G = Flow_ir.Gpu
module H = Flow_gpu.Host
module X = Renderer

module Slots = Lru.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end)

module Images = Lru.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

module Entries = Lru.Identity_list
module Measurements = Lru.Identity_list

type job = {
  run : unit -> unit;
  cancel : Diagnostic.t -> unit;
  mutable finished : bool;
  mutable error : exn option;
  waits : bool;
}

type image_stats = {
  mutable created : int;
  mutable closed : int;
  mutable buffers : int;
  mutable textures : int;
}

let close_image stats sink =
  stats.closed <- stats.closed + 1;
  stats.buffers <- stats.buffers + Flow_gpu.Image_sink.buffer_creations sink;
  stats.textures <- stats.textures + Flow_gpu.Image_sink.texture_creations sink;
  Flow_gpu.Image_sink.close sink

type t = {
  clock : unit -> float;
  mutable owner : (X.gpu * H.t) option;
  mark_sets : X.gpu_marks Slots.t;
  images : Flow_gpu.Image_sink.t Images.t;
  image_stats : image_stats;
  mutable closed_host : H.stats;
  mutable backend : G.backend option;
  mutable proxy : G.backend option;
  mutable closed : bool;
  mutable policy : G.policy;
  mutable measurement_ready : bool;
  jobs_lock : Mutex.t;
  jobs_ready : Condition.t;
  wake : Unix.file_descr * Unix.file_descr;
      (** a byte per queued request: {!wait} sleeps on it, so the owner wakes at once, not at a poll
      *)
  jobs : job Queue.t;
  mutable mailbox_closed : bool;
  measured : (string * int, float option) Measurements.t;
  entries : (Flow_ir.Packed.instruction array * bool, string option) Entries.t;
  dropped : string list Atomic.t;
      (* the keys of displays whose meshes were collected (any domain) *)
  mutable displays : int;  (** device meshes bound so far *)
  bound : (string, unit -> unit) Hashtbl.t; (* each live display's release, by its key *)
  mutable frames : int;
  mutable releasing : (int * (unit -> unit)) list;
      (* dropped displays, released at a later {!frame}: one that drew them may be in flight on the
         presenting queue, which this owner does not see *)
}

let create ?(clock = Unix.gettimeofday) () =
  let image_stats = { created = 0; closed = 0; buffers = 0; textures = 0 } in
  {
    clock;
    owner = None;
    mark_sets = Slots.create ~release:(fun _ -> X.close_gpu_marks) 64;
    images = Images.create ~release:(fun _ -> close_image image_stats) 64;
    image_stats;
    closed_host = H.zero;
    measurement_ready = false;
    backend = None;
    proxy = None;
    closed = false;
    policy = G.Measured;
    jobs_lock = Mutex.create ();
    jobs_ready = Condition.create ();
    wake =
      (let r, w = Unix.pipe ~cloexec:true () in
       Unix.set_nonblock r;
       Unix.set_nonblock w;
       (r, w));
    jobs = Queue.create ();
    mailbox_closed = false;
    measured = Measurements.create ~equal:(fun (a, n) (b, m) -> String.equal a b && n = m) 64;
    entries = Entries.create ~equal:(fun (a, fast) (b, other) -> a == b && fast = other) 64;
    dropped = Atomic.make [];
    displays = 0;
    bound = Hashtbl.create 8;
    frames = 0;
    releasing = [];
  }

let qualification t =
  if t.closed || not (Domain.is_main_domain ()) then
    invalid_arg "Workspace_gpu.qualification: open initial-domain owner required";
  t.policy <- G.Qualification

let error message = Diagnostic.error ~code:"E_GPU" message

let native (result : (_, Ogpu.Error.t) result) =
  Result.map_error (fun e -> error e.Ogpu.Error.message) result

let owner t =
  if t.closed || not (Domain.is_main_domain ()) then
    Error (error "Workspace GPU resources require their open initial-domain owner.")
  else
    match t.owner with
    | Some owner -> Ok owner
    | None ->
        Result.map
          (fun gpu ->
            let owner = (gpu, H.create ~clock:t.clock gpu) in
            t.owner <- Some owner;
            owner)
          (native (X.acquire_gpu ()))

(* The cost of a producer is measured on this device: pack, upload and one synchronized dispatch of the
   program over zeroed inputs of the real shape, median of three after a warm-up. A count is measured at
   the power of two above it, an upper bound on its cost, so a count that drifts frame to frame (a fold's
   survivors) reuses one measurement per size class instead of measuring again. At most 64 programs
   (by emitted kernel, and size class) are kept; one that cannot run is unmeasured and keeps the CPU. *)
let measure t ~fast packed ~count =
  let count =
    let rec up p = if p >= count then p else up (2 * p) in
    up 1
  in
  if not t.measurement_ready then None
  else
    (* a site's kernel entry, by its code: emitting it again (and digesting it) is per-draw work *)
    let code = (Flow_ir.Packed.Host.view packed).code in
    let entry =
      match Entries.peek t.entries (code, fast) with
      | Some entry -> entry
      | None ->
          let entry =
            Result.to_option
              (Result.map
                 (fun (msl : Flow_shader.Emit.msl) -> msl.entry)
                 (Flow_shader.Emit.kernel ~fast packed))
          in
          Entries.add t.entries (code, fast) entry;
          entry
    in
    match entry with
    | None -> None
    | Some entry -> (
        let key = (entry, count) in
        match Measurements.peek t.measured key with
        | Some seconds -> seconds
        | None ->
            let seconds =
              match owner t with
              | Error _ -> None
              | Ok (_, host) -> (
                  match (H.backend host).prepare ~fast packed with
                  | Error _ -> None
                  | Ok kernel -> (
                      let view = Flow_ir.Packed.Host.view packed in
                      let inputs : Flow_ir.Packed.Host.inputs =
                        {
                          arrays =
                            Array.map (fun width -> Array.make (count * width) 0.) view.widths;
                          uniforms =
                            Array.map (fun width -> Array.make width 0.) view.uniform_widths;
                          tables = Array.map (fun _ -> Array.make 4 0.) view.uniform_widths;
                          frame = Array.make (Array.length view.code) 0.;
                          count;
                        }
                      in
                      let once () =
                        let start = t.clock () in
                        match kernel.run inputs with
                        | Error _ -> None
                        | Ok _ -> Some (t.clock () -. start)
                      in
                      match once () with
                      | None -> None
                      | Some _ ->
                          let runs = List.filter_map (fun _ -> once ()) [ 1; 2; 3 ] in
                          if List.length runs < 3 then None
                          else Some (List.nth (List.sort Float.compare runs) 1)))
            in
            Measurements.add t.measured key seconds;
            seconds)

let backend t =
  match t.backend with
  | Some backend -> backend
  | None ->
      let backend : G.backend =
        {
          cost = (fun ~fast packed ~count -> measure t ~fast packed ~count);
          prepare =
            (fun ~fast packed ->
              Result.bind (owner t) (fun (_, host) -> (H.backend host).prepare ~fast packed));
        }
      in
      t.backend <- Some backend;
      backend

let install_test_backend t backend =
  if (not (Domain.is_main_domain ())) || t.closed then
    invalid_arg "Workspace_gpu.Private.install_test_backend: open initial-domain owner required";
  t.backend <- Some backend;
  t.proxy <- None

let route ?(waits = false) t f =
  if Domain.is_main_domain () then f ()
  else begin
    let result = ref None in
    let job =
      {
        run = (fun () -> result := Some (Ok (f ())));
        cancel = (fun diagnostic -> result := Some (Error (Diagnostic.Error diagnostic)));
        finished = false;
        waits;
        error = None;
      }
    in
    Mutex.lock t.jobs_lock;
    if t.mailbox_closed then begin
      Mutex.unlock t.jobs_lock;
      raise (Diagnostic.Error (error "The workspace GPU service is closed."))
    end;
    Queue.add job t.jobs;
    (try ignore (Unix.single_write_substring (snd t.wake) "." 0 1)
     with Unix.Unix_error _ -> () (* a full pipe is already a wake *));
    while not job.finished do
      Condition.wait t.jobs_ready t.jobs_lock
    done;
    Mutex.unlock t.jobs_lock;
    match (job.error, !result) with
    | Some exn, _ -> raise exn
    | None, Some (Ok value) -> value
    | None, Some (Error exn) -> raise exn
    | None, None -> raise (Diagnostic.Error (error "The workspace GPU request was cancelled."))
  end

let proxy t =
  match t.proxy with
  | Some proxy -> proxy
  | None ->
      let real = backend t in
      let route_result f = try f () with Diagnostic.Error d -> Error d in
      let proxy : G.backend =
        {
          cost =
            (fun ~fast packed ~count ->
              try route t (fun () -> real.cost ~fast packed ~count)
              with Diagnostic.Error _ -> None);
          prepare =
            (fun ~fast packed ->
              route_result (fun () ->
                  let prepared : (G.kernel, Diagnostic.t) result =
                    route t (fun () -> real.prepare ~fast packed)
                  in
                  Result.map
                    (fun (kernel : G.kernel) ->
                      ({
                         run =
                           (fun inputs ->
                             route_result (fun () -> route t (fun () -> kernel.run inputs)));
                         readback =
                           (fun value ->
                             route_result (fun () -> route t (fun () -> kernel.readback value)));
                         planes =
                           (fun value ->
                             route_result (fun () ->
                                 Result.map
                                   (fun (planes : G.planes) ->
                                     {
                                       planes with
                                       settle =
                                         (fun () ->
                                           route_result (fun () ->
                                               route ~waits:true t planes.settle));
                                       bounds =
                                         (fun () ->
                                           try route t planes.bounds
                                           with Diagnostic.Error _ -> None);
                                     })
                                   (route t (fun () -> kernel.planes value))));
                       }
                        : G.kernel))
                    prepared));
        }
      in
      t.proxy <- Some proxy;
      proxy

(* A cook's requests come in chains (a dispatch, its planes, the next dispatch), each sent a fraction
   of a millisecond after the last returns. The pump serves what is queued, then waits [follow_up]
   for the next link, so a chain takes one pump rather than a frame per request. A settle is not
   waited for: it blocks until the GPU finishes, which by the next pump it usually has. *)
let follow_up_seconds = 0.0005

let pump ?(follow_up = true) t =
  if not (Domain.is_main_domain ()) then invalid_arg "Workspace_gpu.pump: initial domain required";
  let take ~chained =
    Mutex.lock t.jobs_lock;
    let job =
      match Queue.peek_opt t.jobs with
      | Some job when chained && job.waits -> None
      | Some _ -> Queue.take_opt t.jobs
      | None -> None
    in
    Mutex.unlock t.jobs_lock;
    job
  in
  let serve job =
    (try job.run () with exn -> job.error <- Some exn);
    Mutex.lock t.jobs_lock;
    job.finished <- true;
    Condition.broadcast t.jobs_ready;
    Mutex.unlock t.jobs_lock
  in
  (* only the pump's first take may be a settle; everything after it is a follow-up *)
  let rec drain () =
    match take ~chained:false with
    | Some job ->
        serve job;
        if follow_up then follow (Unix.gettimeofday () +. follow_up_seconds) 1 else drain ()
    | None -> ()
  (* the wait reads the queue without its lock (a racy emptiness check; the take locks) and the
     clock every 64 spins, so it neither contends with the worker nor allocates *)
  and follow until spins =
    if not (Queue.is_empty t.jobs) then
      match take ~chained:true with
      | Some job ->
          serve job;
          follow (Unix.gettimeofday () +. follow_up_seconds) 1
      | None -> ()
    else if spins land 63 = 0 && Unix.gettimeofday () >= until then ()
    else begin
      Domain.cpu_relax ();
      follow until (spins + 1)
    end
  in
  drain ()

(* The owner sleeps until a request is queued or [timeout] seconds pass (the cook finishing signals
   nothing here, so a caller waiting on that passes a short timeout). *)
let wait t ~timeout =
  if not (Domain.is_main_domain ()) then invalid_arg "Workspace_gpu.wait: initial domain required";
  if Mutex.protect t.jobs_lock (fun () -> Queue.is_empty t.jobs) then begin
    match Unix.select [ fst t.wake ] [] [] timeout with
    | [], _, _ -> ()
    | _ -> (
        let drained = Bytes.create 64 in
        try
          while Unix.read (fst t.wake) drained 0 64 = 64 do
            ()
          done
        with Unix.Unix_error _ -> ())
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
  end

(* The frame is the pump: cook workers queue device requests, served here between frames' work. *)
let config ?(fast = false) t : G.config = { backend = Some (proxy t); fast; policy = t.policy }

let with_backend ?(fast = false) t run =
  let gpu = config ~fast t in
  pump t;
  (* Static lowering can ask for a cost before the render owner is serving frames. Defer the
     count-sized calibration until the persistent workspace service has entered its pump loop. *)
  t.measurement_ready <- true;
  Fun.protect ~finally:(fun () -> pump t) (fun () -> run gpu)

let resident gpu t =
  if not gpu.G.fast then None
  else Result.to_option (Result.map (fun (_, host) -> H.resident ~fast:gpu.fast host) (owner t))

let image t ~key ~width ~height ~publish value =
  Result.bind (owner t) (fun (gpu, host) ->
      Result.bind (H.output host value) (function
        | None -> Error (error "The image's GPU producer was closed or superseded.")
        | Some output -> (
            let convert sink =
              Result.bind (Flow_gpu.Image_sink.convert sink ~width ~height output) publish
            in
            match Images.find_opt t.images key with
            | Some sink -> convert sink
            | None when Images.length t.images >= 64 ->
                Error (error "A workspace owns at most 64 GPU image sinks.")
            | None ->
                Result.bind (Flow_gpu.Image_sink.create gpu) (fun sink ->
                    t.image_stats.created <- t.image_stats.created + 1;
                    let committed = ref false in
                    Fun.protect
                      ~finally:(fun () -> if not !committed then close_image t.image_stats sink)
                      (fun () ->
                        Result.map
                          (fun result ->
                            Images.add t.images key sink;
                            committed := true;
                            result)
                          (convert sink))))))

(* One [Marks]-family source per GPU mark set of a frame, numbered in drawing order. *)
let marks t : Marks_scene.gpu =
 fun ~slot ~kind ~count attributes ->
  Result.bind (owner t) (fun (gpu, host) ->
      let sink =
        match Slots.find_opt t.mark_sets slot with
        | Some sink -> Ok sink
        | None ->
            Result.map
              (fun sink ->
                Slots.add t.mark_sets slot sink;
                sink)
              (native (X.create_gpu_marks gpu))
      in
      Result.bind sink (fun sink ->
          let rec resolve values = function
            | [] -> Ok values
            | { Marks_scene.source = Device value; _ } :: rest ->
                Result.bind (H.output host value) (function
                  | None -> Error (error "GPU display output is closed or superseded.")
                  | Some output -> resolve ((value, output) :: values) rest)
            | _ :: rest -> resolve values rest
          in
          Result.bind
            (resolve [] (Array.to_list attributes))
            (fun outputs ->
              let output value =
                List.find_map
                  (fun (candidate, output) ->
                    if
                      candidate.G.identity = value.G.identity
                      && candidate.stamp = value.stamp && candidate.output = value.output
                    then Some output
                    else None)
                  outputs
              in
              let source ({ Marks_scene.source; stride } : Marks_scene.attribute) =
                match source with
                | Marks_scene.Bytes bytes -> X.Host (bytes, stride)
                | Device value ->
                    X.Device ((fun () -> Option.bind (output value) Flow_gpu.Run.buffer), stride)
              in
              (* positions a fast kernel wrote come with its count record: the set's instance count is
           the one the device wrote, drawn indirect *)
              let indirect =
                match attributes.(0).source with
                | Device value when Option.bind (output value) Flow_gpu.Run.count_record <> None ->
                    Some ((fun () -> Option.bind (output value) Flow_gpu.Run.count_record), 4L)
                | _ -> None
              in
              native (X.gpu_marks ?indirect sink ~kind ~count (Array.map source attributes)))))

let rec push cell value =
  let old = Atomic.get cell in
  if not (Atomic.compare_and_set cell old (value :: old)) then push cell value

let drop_display t key =
  match Hashtbl.find_opt t.bound key with
  | Some release ->
      Hashtbl.remove t.bound key;
      t.releasing <- (t.frames + 2, release) :: t.releasing
  | None -> ()

(* ponytail: a dropped display's buffers wait two frames, not for the presenting queue's completion,
   which needs the renderer's epoch; the renderer keeps one presented frame in flight *)
let frame ?keep t =
  if (not t.closed) && Domain.is_main_domain () then begin
    t.frames <- t.frames + 1;
    List.iter (drop_display t) (Atomic.exchange t.dropped []);
    Option.iter
      (fun keep ->
        let keep = Hashtbl.of_seq (Seq.map (fun key -> (key, ())) (List.to_seq keep)) in
        Hashtbl.fold
          (fun key _ dead -> if Hashtbl.mem keep key then dead else key :: dead)
          t.bound []
        |> List.iter (drop_display t))
      keep;
    let due, later = List.partition (fun (at, _) -> at <= t.frames) t.releasing in
    t.releasing <- later;
    List.iter (fun (_, release) -> release ()) due
  end

let held_displays t = Hashtbl.length t.bound + List.length t.releasing

(* A point cloud's or a triangle mesh's resident P (and N; a mesh without N takes normals made from
   P on the device, {!Flow_gpu.Host.normals}) as the renderer's device positions: pinned in the pool and
   registered while the mesh that keeps the returned closure lives. Initial domain, after the cook
   settled (its dispatch completed: explicit, not unified-memory, ordering). *)
let display t geometry =
  if t.closed || not (Domain.is_main_domain ()) then None
  else begin
    match (t.owner, Flow_sop.Attribute_kernel.resident_vec3 geometry "P") with
    | Some (gpu, host), Some positions -> (
        let count = Rdk.Geometry.point_count geometry in
        let pin placeholder =
          match Flow_gpu.Run.pin placeholder with
          | Some (buffer, n, valid, unpin) when n = count -> Some (buffer, valid, unpin)
          | Some (_, _, _, unpin) ->
              unpin ();
              None
          | None -> None
        in
        let pinned = pin positions in
        let normals =
          match Rdk.Geometry.find_attribute ~owner:Point "N" geometry with
          | None when Rdk.Geometry.primitive_count geometry = 0 -> Ok None
          | None -> (
              (* lit triangles without N: the host display's normals, made from P on the device *)
              match
                Option.bind pinned (fun (buffer, _, _) ->
                    H.normals host ~positions:buffer ~count
                      ~corners:
                        (Rdk.Topology.View.view (Rdk.Geometry.topology geometry)).vertex_points)
              with
              | Some (buffer, release) ->
                  let live = ref true in
                  Ok
                    (Some
                       ( buffer,
                         (fun () -> !live),
                         fun () ->
                           live := false;
                           release () ))
              | None -> Error ())
          | Some _ -> (
              match Option.bind (Flow_sop.Attribute_kernel.resident_vec3 geometry "N") pin with
              | Some pinned -> Ok (Some pinned)
              | None -> Error ())
        in
        match (pinned, normals) with
        | Some (buffer, valid, unpin), Ok normals -> (
            let unpin_all () =
              unpin ();
              Option.iter (fun (_, _, unpin) -> unpin ()) normals
            in
            match
              X.register_gpu_positions gpu ~positions:buffer
                ?normals:(Option.map (fun (b, _, _) -> b) normals)
                ~count
                ~valid:(fun () ->
                  valid () && Option.fold ~none:true ~some:(fun (_, valid, _) -> valid ()) normals)
                ()
            with
            | Error _ ->
                unpin_all ();
                None
            | Ok (key, unregister) ->
                t.displays <- t.displays + 1;
                Hashtbl.replace t.bound key (fun () ->
                    unregister ();
                    unpin_all ());
                let token = ref () in
                Gc.finalise (fun _ -> push t.dropped key) token;
                Some (key, normals <> None, fun () -> ignore (Sys.opaque_identity token)))
        | pinned, normals ->
            Option.iter (fun (_, _, unpin) -> unpin ()) pinned;
            Result.iter (Option.iter (fun (_, _, unpin) -> unpin ())) normals;
            None)
    | _ -> None
  end

let displays t = t.displays

let image_stats t =
  if not (Domain.is_main_domain ()) then
    invalid_arg "Workspace_gpu.image_stats: initial domain required";
  let host = Option.fold ~none:t.closed_host ~some:(fun (_, host) -> H.stats host) t.owner in
  let buffers = ref t.image_stats.buffers and textures = ref t.image_stats.textures in
  Images.iter t.images (fun _ sink ->
      buffers := !buffers + Flow_gpu.Image_sink.buffer_creations sink;
      textures := !textures + Flow_gpu.Image_sink.texture_creations sink);
  (host, t.image_stats.created, t.image_stats.closed, !buffers, !textures)

let close t =
  if not (Domain.is_main_domain ()) then invalid_arg "Workspace_gpu.close: initial domain required";
  if not t.closed then begin
    Mutex.lock t.jobs_lock;
    t.mailbox_closed <- true;
    let diagnostic = error "The workspace GPU service closed before the request ran." in
    while not (Queue.is_empty t.jobs) do
      let job = Queue.take t.jobs in
      job.cancel diagnostic;
      job.finished <- true
    done;
    Condition.broadcast t.jobs_ready;
    Mutex.unlock t.jobs_lock;
    Hashtbl.iter (fun _ release -> release ()) t.bound;
    Hashtbl.reset t.bound;
    List.iter (fun (_, release) -> release ()) t.releasing;
    t.releasing <- [];
    Unix.close (fst t.wake);
    Unix.close (snd t.wake);
    Slots.clear t.mark_sets;
    Images.clear t.images;
    Option.iter
      (fun (gpu, host) ->
        H.close host;
        t.closed_host <- H.stats host;
        X.release_gpu gpu)
      t.owner;
    t.owner <- None;
    t.closed <- true
  end

module Private = struct
  let install_test_backend = install_test_backend
  let on_owner_for_test = route
end

type policy = G.config

let default_policy = G.default
