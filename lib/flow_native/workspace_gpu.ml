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
  jobs : job Queue.t;
  mutable mailbox_closed : bool;
  measured : (string * int, float option) Measurements.t;
  entries : (Flow_ir.Packed.instruction array * bool, string option) Entries.t;
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
    jobs = Queue.create ();
    mailbox_closed = false;
    measured = Measurements.create ~equal:(fun (a, n) (b, m) -> String.equal a b && n = m) 64;
    entries = Entries.create ~equal:(fun (a, fast) (b, other) -> a == b && fast = other) 64;
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
   program over zeroed inputs of the real shape, median of three after a warm-up. At most 64 programs
   (by emitted kernel, and count) are kept; one that cannot run is unmeasured and keeps the CPU. *)
let measure t ~fast packed ~count =
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
                 (fun (msl : Flow_gpu.Emit.msl) -> msl.entry)
                 (Flow_gpu.Emit.kernel ~fast packed))
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

let route t f =
  if Domain.is_main_domain () then f ()
  else begin
    let result = ref None in
    let job =
      {
        run = (fun () -> result := Some (Ok (f ())));
        cancel = (fun diagnostic -> result := Some (Error (Diagnostic.Error diagnostic)));
        finished = false;
        error = None;
      }
    in
    Mutex.lock t.jobs_lock;
    if t.mailbox_closed then begin
      Mutex.unlock t.jobs_lock;
      raise (Diagnostic.Error (error "The workspace GPU service is closed."))
    end;
    Queue.add job t.jobs;
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
                       }
                        : G.kernel))
                    prepared));
        }
      in
      t.proxy <- Some proxy;
      proxy

let pump t =
  if not (Domain.is_main_domain ()) then invalid_arg "Workspace_gpu.pump: initial domain required";
  let rec drain () =
    Mutex.lock t.jobs_lock;
    let job = Queue.take_opt t.jobs in
    Mutex.unlock t.jobs_lock;
    match job with
    | None -> ()
    | Some job ->
        (try job.run () with exn -> job.error <- Some exn);
        Mutex.lock t.jobs_lock;
        job.finished <- true;
        Condition.broadcast t.jobs_ready;
        Mutex.unlock t.jobs_lock;
        drain ()
  in
  drain ()

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

let image_stats t =
  if not (Domain.is_main_domain ()) then
    invalid_arg "Workspace_gpu.image_stats: initial domain required";
  let host =
    Option.fold ~none:t.closed_host ~some:(fun (_, host) -> H.stats host) t.owner
  in
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
