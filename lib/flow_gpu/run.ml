module B = Ogpu.Backend
module P = Flow_ir.Packed

type slot = {
  mutable buffer : B.buffer option;
  mutable capacity : int;
  mutable bytes : bytes;
  mutable floats : B.floats option;
      (** a mapped slot's contents (inputs, gathers, outputs): written and read in place *)
  mutable uploaded : (float array * int) option;
}

(* An output buffer a lazy planar store holds after its dispatch completed and passed its status
   check: the runner hands the slot's buffer over and makes a fresh one for its next dispatch. The
   holder materialises it once (from any domain: an invalidate and a conversion, no queue work);
   collected, its buffer comes back through [released] and is destroyed on the initial domain. *)
type device_pressure

(* a block the GC counts as [bytes] of memory, as a Bigarray its data: a dead holder is collected as
   promptly as an array of its buffer's size *)
external device_pressure : int -> device_pressure = "rays_flow_gpu_device_pressure"

type kept = {
  id : int;
  kept_buffer : B.buffer;
  kept_floats : B.floats;
  kept_count : int;
  kept_width : int;
  lock : Mutex.t;  (** orders an invalidate against close's destroy; never held while converting *)
  planes : float array array option Atomic.t;
  mutable destroyed : bool;  (** its buffer is gone: closed (read first) or freed (never read) *)
  mutable retired : bool;  (** handed out once: it stays readable from then on *)
  placeholder : float array;
      (** one element of zeros: a later kernel's source that is this array reads the buffer *)
  kept_epoch : int64;
  settled : bool Atomic.t;  (** completed, its status read: CPU readers may read it *)
  readers : int Atomic.t;  (** conversions in progress: its buffer is not reused under them *)
  mutable thaw : unit -> unit;  (** materialise it (initial domain), so its buffer can be reused *)
  pressure : device_pressure; [@warning "-69"]  (** the buffer's size, told to the GC; never read *)
}

(* Buffers runners share (a host's, across edits): those held by kept outputs, by holder; free ones
   of any size class (capacity doubles, so a class is [length, 2 * length]), {!free_budget}; collected
   or read holders' ids, from any domain; retired holders, whose owner (a session) dropped them but
   which a reader may still hold: their buffers stay theirs until they are collected or read, and
   past {!retired_budget} bytes the oldest is read into owned planes so its buffer comes back. *)
type pool = {
  held : (int, B.buffer * B.floats * int * int Atomic.t) Hashtbl.t;
  mutable free : (B.buffer * B.floats * int) list;
  released : int list Atomic.t;
  retiring : (int * kept Weak.t) list Atomic.t;
  freed : int Atomic.t;  (** holders freed unread *)
  mutable retired : (int * kept Weak.t) list;
      (** oldest first, by id: only the one {!reclaim} reads is ever [Weak.get], since a get while
          the GC marks keeps the holder alive another cycle *)
}

let pool () =
  {
    held = Hashtbl.create 16;
    free = [];
    released = Atomic.make [];
    retiring = Atomic.make [];
    freed = Atomic.make 0;
    retired = [];
  }

(* Kept outputs a later dispatch may bind by their placeholder, on the initial domain. *)
let bindable : (float array * kept Weak.t) list ref = ref []

(* The held buffer a placeholder stands for, while its holder has not read it back (its buffer is
   then released) and it covers [length] floats. *)
let held_input array ~length =
  bindable := List.filter (fun (_, w) -> Weak.check w 0) !bindable;
  List.find_map
    (fun (p, w) ->
      if p != array then None
      else
        Option.bind (Weak.get w 0) (fun kept ->
            if
              Atomic.get kept.planes = None
              && (not kept.destroyed)
              && kept.kept_count * kept.kept_width >= length
            then Some kept.kept_buffer
            else None))
    !bindable

type t = {
  device : B.device;
  queue : B.queue;
  pipelines : Pipelines.t;
  msl : Emit.msl;
  inputs : slot array;
  outputs : slot array;
  table : slot;
  gathers : slot array;
  status : slot;
  counted : slot;
  uniforms : bytes;
  mutable closed : bool;
  mutable creations : int;
  mutable input_uploads : int;
  mutable input_uploaded_bytes : int;
  mutable status_reads : int;
  mutable readback_bytes : int;
  mutable status_checked : bool;
  mutable generation : int;
  mutable table_ready : bool;
  domain : Domain.id;
  mutable last_epoch : int64;
  mutable kept : kept Weak.t list;  (** live holders, materialised at close *)
  pool : pool;
  own_pool : bool;  (** made for this runner alone: closed with it *)
  materialised : int Atomic.t;
  mutable verdicts : (int64 * Diagnostic.t) list;
      (** failed statuses of superseded dispatches no one had settled, newest first, at most 8 *)
}

type scene = {
  accel : B.accel;
  vertices : B.buffer;
  spheres : B.buffer;
  sphere_count : int;
  instances : B.buffer option;
}

type extras = {
  vertices_end : B.buffer option;
  strands : B.buffer option;
  strand_count : int;
  curves : B.accel option;
}

let no_extras = { vertices_end = None; strands = None; strand_count = 0; curves = None }

type output = {
  owner : t;
  count : int;
  width : int;
  index : int;
  gpu_seconds : float option;
  generation : int;
  epoch : int64;
  check_status : bool;
}

let diagnostic error = Diagnostic.error ~code:"E_GPU" (Ogpu.Error.to_string error)
let get = function Ok value -> value | Error error -> raise (Diagnostic.Error (diagnostic error))
let slot () = { buffer = None; capacity = 0; bytes = Bytes.empty; floats = None; uploaded = None }

let nonfinite () =
  Diagnostic.Error
    (Diagnostic.error ~code:"E_KERNEL" "GPU inputs must remain finite when represented as float32.")

let kernel_status flags =
  (* The status word is an aggregate across parallel rows, so diagnostics have a stable
     class priority rather than claiming CPU row-order precedence. *)
  if Int32.logand flags 4l <> 0l then
    Diagnostic.error ~code:"E_RANGE" "Integer shift count must be within [0, 62]."
  else if Int32.logand flags 2l <> 0l then
    Diagnostic.error ~code:"E_RANGE" "Integer division by zero."
  else Diagnostic.error ~code:"E_KERNEL" "GPU output contains a nonfinite value."

let pack bytes offset value =
  let bits = Int32.bits_of_float value in
  if not (Float.is_finite (Int32.float_of_bits bits)) then raise (nonfinite ());
  Bytes.set_int32_le bytes offset bits

(* Straight into the mapped buffer the kernel reads: one float32 rounding per element, no staging
   bytes and no copy. A finite double past float32's range rounds to infinity and is refused. *)
let pack_floats (floats : B.floats) (values : float array) length =
  for index = 0 to length - 1 do
    Bigarray.Array1.unsafe_set floats index (Array.unsafe_get values index);
    if not (Float.is_finite (Bigarray.Array1.unsafe_get floats index)) then raise (nonfinite ())
  done

let unpack_array bytes length =
  let values = Array.create_float length and finite = ref true in
  for index = 0 to length - 1 do
    let value = Int32.float_of_bits (Bytes.get_int32_le bytes (index * 4)) in
    if not (Float.is_finite value) then finite := false;
    Array.unsafe_set values index value
  done;
  (values, !finite)

let create_owned ?pool:shared device queue pipelines (msl : Emit.msl) =
  if not (Domain.is_main_domain ()) then invalid_arg "Run.create: initial domain required";
  {
    device;
    queue;
    pipelines;
    msl;
    inputs = Array.map (fun _ -> slot ()) msl.input_widths;
    outputs = Array.map (fun _ -> slot ()) msl.output_widths;
    table = slot ();
    gathers = Array.map (fun _ -> slot ()) msl.gathers;
    status = slot ();
    counted = slot ();
    uniforms = Bytes.create msl.uniform_bytes;
    closed = false;
    creations = 0;
    input_uploads = 0;
    input_uploaded_bytes = 0;
    status_reads = 0;
    readback_bytes = 0;
    status_checked = false;
    generation = 0;
    table_ready = false;
    domain = Domain.self ();
    last_epoch = 0L;
    kept = [];
    pool = Option.value shared ~default:(pool ());
    own_pool = shared = None;
    materialised = Atomic.make 0;
    verdicts = [];
  }

let rec push cell value =
  let old = Atomic.get cell in
  if not (Atomic.compare_and_set cell old (value :: old)) then push cell value

(* Free buffers kept for reuse: any 8, or more while their bytes stay under this. A collection returns
   several dead holders' buffers at once, which the next cooks take. *)
let free_budget = 256 lsl 20

(* on the initial domain: recycle or destroy what collected or read holders gave back (a buffer a
   reader is still converting waits for the next drain) *)
let drain_pool pool =
  List.iter
    (fun id ->
      match Hashtbl.find_opt pool.held id with
      | Some (_, _, _, readers) when Atomic.get readers > 0 -> push pool.released id
      | Some (buffer, floats, capacity, _) ->
          Hashtbl.remove pool.held id;
          let free = List.fold_left (fun sum (_, _, c) -> sum + c) 0 pool.free in
          if List.length pool.free < 8 || free + capacity <= free_budget then
            pool.free <- (buffer, floats, capacity) :: pool.free
          else ignore (B.destroy_buffer buffer)
      | None -> ())
    (Atomic.exchange pool.released [])

let drain t =
  drain_pool t.pool;
  t.kept <- List.filter (fun w -> Weak.check w 0) t.kept

let take_free pool length =
  match
    List.find_opt (fun (_, _, capacity) -> capacity >= length && capacity <= 2 * length) pool.free
  with
  | Some held ->
      pool.free <- List.filter (( != ) held) pool.free;
      Some held
  | None -> None

(* Past this many bytes of retired holders still unread (alive: the GC is told each one's size, so a
   dead one is collected first), the oldest is read now (owned planes: its later readers see the same
   values) and its buffer comes back, so device memory stays bounded. ponytail: a constant, the
   device's working-set size if a smaller GPU needs it. *)
let retired_budget = 2 lsl 30

let reclaim pool =
  drain_pool pool;
  let held (id, w) = Weak.check w 0 && Hashtbl.mem pool.held id in
  pool.retired <- List.filter held (pool.retired @ List.rev (Atomic.exchange pool.retiring []));
  let bytes sum (id, _) =
    match Hashtbl.find_opt pool.held id with
    | Some (_, _, capacity, _) -> sum + capacity
    | None -> sum
  in
  if List.fold_left bytes 0 pool.retired > retired_budget then begin
    (match pool.retired with
    | (_, w) :: rest ->
        pool.retired <- rest;
        Option.iter (fun k -> if Atomic.get k.readers = 0 then k.thaw ()) (Weak.get w 0)
    | [] -> ());
    drain_pool pool
  end

let create ?pool gpu = create_owned ?pool (Renderer.gpu_device gpu) (Renderer.gpu_queue gpu)

let ensure ?(mapped = false) t slot length =
  (if mapped && slot.buffer = None && length > 0 then
     (* an output slot whose last buffer a holder took: a recycled buffer of the size class *)
     let found =
       match take_free t.pool length with
       | Some _ as found -> found
       | None ->
           reclaim t.pool;
           take_free t.pool length
     in
     match found with
     | Some (buffer, floats, capacity) ->
         slot.buffer <- Some buffer;
         slot.floats <- Some floats;
         slot.capacity <- capacity;
         slot.uploaded <- None
     | None -> ());
  if length > slot.capacity then begin
    let capacity = ref (max 256 slot.capacity) in
    while !capacity < length do
      if !capacity > Sys.max_string_length / 2 then capacity := length
      else capacity := !capacity * 2
    done;
    let descriptor : Ogpu.Types.buffer_descriptor =
      {
        label = Some "Flow packed compute";
        size = Int64.of_int !capacity;
        usage = [ Storage; Copy_src; Copy_dst ];
      }
    in
    let fresh, floats =
      if mapped then
        let buffer, floats = get (B.create_mapped_buffer t.device descriptor) in
        (buffer, Some floats)
      else (get (B.create_buffer t.device descriptor), None)
    in
    Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
    slot.uploaded <- None;
    slot.buffer <- Some fresh;
    slot.capacity <- !capacity;
    slot.bytes <- (if mapped then Bytes.empty else Bytes.create !capacity);
    slot.floats <- floats;
    t.creations <- t.creations + 1
  end;
  Option.get slot.buffer

(* [wait] false leaves the dispatch in flight (no completion wait, no status read): a resident fold
   chains its regions and waits once.  [bound i] / [bound_table u] name a device buffer that already
   holds input [i] / the table of uniform [u] (and its length in floats), which is then not uploaded. *)
let msl_scene t = t.msl.Emit.scene

let dispatch ?(wait = true) ?(reset_status = true) ?commands ?count ?into ?(bound = fun _ -> None)
    ?(bound_table = fun _ -> None) ?scene ?(extras = no_extras) ?(resident = []) t
    (values : P.Host.inputs) =
  (* a source another runner keeps on the device (its placeholder) is bound to its buffer *)
  let bound index =
    match bound index with
    | Some _ as b -> b
    | None when index < Array.length values.arrays && Array.length values.arrays.(index) <= 4 ->
        held_input values.arrays.(index)
          ~length:(values.count * t.msl.input_widths.(min index (Array.length t.inputs - 1)))
    | None -> None
  in
  (* the scene's buffers after its spheres, in Kernel_layout's order; the kernel's interface names at most
     as many (the tables its trace helpers read) *)
  let scene_tail =
    match scene with
    | None -> []
    | Some s ->
        List.filter_map
          (Option.map (fun b -> `Buffer b))
          [ s.instances; extras.vertices_end; extras.strands ]
        @ Option.fold ~none:[] ~some:(fun a -> [ `Accel a ]) extras.curves
  in
  let error message = Error (Diagnostic.error ~code:"E_KERNEL" message) in
  if t.closed || Domain.self () <> t.domain then
    error "GPU program owner is closed or called from another domain."
  else if
    values.count < 0
    || values.count > Int32.to_int Int32.max_int
    || values.count > Sys.max_string_length / 4 / Array.fold_left max 1 t.msl.output_widths
  then error "GPU count exceeds native storage bounds."
  else if
    msl_scene t <> None <> (scene <> None)
    ||
    match msl_scene t with
    | None -> false
    | Some b ->
        List.length
          (List.filter (fun (x : Ogpu.Shader.binding) -> x.binding > b + 2) t.msl.interface)
        > List.length scene_tail
  then error "GPU scene binding changed."
  else if Array.length values.arrays <> Array.length t.inputs then error "GPU input arity changed."
  else if
    Array.exists
      (fun index ->
        bound index = None
        && Array.length values.arrays.(index) < values.count * t.msl.input_widths.(index))
      (Array.init (Array.length t.inputs) Fun.id)
  then error "GPU input count changed."
  else begin
    (* Invalidate borrowed output before reallocating or submitting any write,
     including dispatches whose completion subsequently fails. *)
    t.generation <- t.generation + 1;
    drain t;
    if values.count = 0 then begin
      Ok
        {
          owner = t;
          count = 0;
          width = t.msl.output_widths.(0);
          index = 0;
          gpu_seconds = None;
          generation = t.generation;
          epoch = t.last_epoch;
          check_status = false;
        }
    end
    else
      try
        let msl = t.msl in
        let compiled =
          match Pipelines.get t.pipelines msl with
          | Ok compiled -> compiled
          | Error error -> raise (Diagnostic.Error error)
        in
        Bytes.set_int32_le t.uniforms 0 (Int32.of_int values.count);
        List.iter
          (fun (name, offset, width) ->
            if String.starts_with ~prefix:"uniform:" name then begin
              let index = int_of_string (String.sub name 8 (String.length name - 8)) in
              if
                index >= Array.length values.uniforms
                || Array.length values.uniforms.(index) <> width
              then
                raise
                  (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU uniform type changed."));
              for component = 0 to width - 1 do
                pack t.uniforms (offset + (component * 4)) values.uniforms.(index).(component)
              done
            end
            else
              match String.split_on_char ':' name with
              | [ "gather"; k; _ ] ->
                  let u, width = msl.gathers.(int_of_string k) in
                  if u >= Array.length values.tables then
                    raise
                      (Diagnostic.Error
                         (Diagnostic.error ~code:"E_KERNEL" "GPU gather table changed."));
                  let length =
                    match bound_table u with
                    | Some (_, length) -> length
                    | None -> Array.length values.tables.(u)
                  in
                  Bytes.set_int32_le t.uniforms offset (Int32.of_int (length / width))
              | [ "scene"; "strands" ] ->
                  Bytes.set_int32_le t.uniforms offset (Int32.of_int extras.strand_count)
              | [ "scene"; "spheres" ] ->
                  Bytes.set_int32_le t.uniforms offset
                    (Int32.of_int (match scene with Some s -> s.sphere_count | None -> 0))
              | [ "frame"; index; _ ] ->
                  let index = int_of_string index in
                  if index >= Array.length values.frame then
                    raise
                      (Diagnostic.Error
                         (Diagnostic.error ~code:"E_KERNEL" "GPU frame uniform changed."));
                  pack t.uniforms offset values.frame.(index)
              | _ -> assert false)
          msl.uniform_layout;
        (* a buffer this runner's last dispatch still reads is not rewritten under it *)
        if commands = None && t.last_epoch > B.completed_epoch t.queue then
          get (B.complete_through t.queue t.last_epoch);
        let buffers =
          Array.mapi
            (fun index slot ->
              match bound index with
              | Some buffer -> buffer
              | None ->
                  let length = values.count * msl.input_widths.(index) * 4 in
                  let buffer = ensure ~mapped:true t slot length in
                  let source = values.arrays.(index) in
                  if
                    not
                      (match slot.uploaded with
                      | Some (old, covered) -> old == source && covered = length
                      | None -> false)
                  then begin
                    (* A failed write may have partially replaced the previous source. *)
                    slot.uploaded <- None;
                    pack_floats (Option.get slot.floats) source (length / 4);
                    get (B.flush_mapped buffer ~length);
                    slot.uploaded <- Some (source, length);
                    t.input_uploads <- t.input_uploads + 1;
                    t.input_uploaded_bytes <- t.input_uploaded_bytes + length
                  end;
                  buffer)
            t.inputs
        in
        let outputs =
          match into with
          | Some (output, _) when Array.length t.outputs = 1 -> [| output |]
          | _ ->
              Array.mapi
                (fun k slot ->
                  ensure ~mapped:true t slot (values.count * msl.output_widths.(k) * 4))
                t.outputs
        in
        let status = ensure t t.status 4 in
        (* the device count words and cells of a fast kernel; without a [count] its own cell stands in *)
        let counted =
          Option.map
            (fun offset ->
              let out = match into with Some (_, cell) -> cell | None -> ensure t t.counted 24 in
              let cell, mul, div, use =
                match count with
                | Some (cell, mul, div) -> (cell, mul, div, 1)
                | None -> (out, 1, 1, 0)
              in
              List.iteri
                (fun k v -> Bytes.set_int32_le t.uniforms (offset + (4 * k)) (Int32.of_int v))
                [ mul; div; use ];
              (cell, out))
            msl.counts
        in
        if reset_status then begin
          (* the last dispatch's status, if no one settled it, is kept before this one resets it *)
          if (not t.status_checked) && t.last_epoch > 0L && t.status.buffer <> None then begin
            if t.last_epoch > B.completed_epoch t.queue then
              get (B.complete_through t.queue t.last_epoch);
            let flags = Bytes.get_int32_le (get (B.read_buffer status ~offset:0L ~length:4)) 0 in
            t.status_reads <- t.status_reads + 1;
            if flags <> 0l then
              t.verdicts <- List.take 8 ((t.last_epoch, kernel_status flags) :: t.verdicts)
          end;
          Bytes.set_int32_le t.status.bytes 0 0l;
          get (B.write_buffer status ~offset:0L t.status.bytes);
          t.status_checked <- false
        end;
        let table =
          if msl.table_seeds = [||] then None
          else begin
            let length = Array.length msl.table_seeds * 512 * 4 in
            let fresh = not t.table_ready in
            let buffer = ensure t t.table length in
            if fresh then begin
              Array.iteri
                (fun index seed ->
                  let permutation = Rays_math.Noise.permutation (Rays_math.Noise.create seed) in
                  Array.iteri
                    (fun entry value ->
                      Bytes.set_int32_le t.table.bytes
                        (((index * 512) + entry) * 4)
                        (Int32.of_int value))
                    permutation)
                msl.table_seeds;
              get (B.write_buffer buffer ~offset:0L t.table.bytes);
              t.table_ready <- true
            end;
            Some buffer
          end
        in
        (* the tables gathers read, uploaded when their array changed *)
        let gathers =
          Array.mapi
            (fun k slot ->
              match bound_table (fst msl.gathers.(k)) with
              | Some (buffer, _) -> buffer
              | None ->
                  let source = values.tables.(fst msl.gathers.(k)) in
                  let length = max 4 (Array.length source * 4) in
                  let buffer = ensure ~mapped:true t slot length in
                  if
                    not
                      (match slot.uploaded with
                      | Some (old, covered) -> old == source && covered = length
                      | None -> false)
                  then begin
                    slot.uploaded <- None;
                    pack_floats (Option.get slot.floats) source (Array.length source);
                    get (B.flush_mapped buffer ~length:(Array.length source * 4));
                    slot.uploaded <- Some (source, length);
                    t.input_uploads <- t.input_uploads + 1;
                    t.input_uploaded_bytes <- t.input_uploaded_bytes + length
                  end;
                  buffer)
            t.gathers
        in
        let queue = t.queue in
        let encode commands =
          let encoder = get (B.compute_encoder commands) in
          Fun.protect
            ~finally:(fun () -> ignore (B.end_compute encoder))
            (fun () ->
              get (B.set_pipeline encoder compiled.pipeline);
              Array.iteri (fun index buffer -> get (B.set_buffer encoder ~index buffer)) buffers;
              let index = Array.length buffers in
              Array.iteri
                (fun k output -> get (B.set_buffer encoder ~index:(index + k) output))
                outputs;
              let index = index + Array.length outputs in
              get (B.set_bytes encoder ~index t.uniforms);
              get (B.set_buffer encoder ~index:(index + 1) status);
              Option.iter (fun table -> get (B.set_buffer encoder ~index:(index + 2) table)) table;
              let index = index + 2 + if table = None then 0 else 1 in
              Array.iteri
                (fun k buffer -> get (B.set_buffer encoder ~index:(index + k) buffer))
                gathers;
              let index = index + Array.length gathers in
              Option.iter
                (fun (cell, out) ->
                  get (B.set_buffer encoder ~index cell);
                  get (B.set_buffer encoder ~index:(index + 1) out))
                counted;
              Option.iter
                (fun { accel; vertices; spheres; _ } ->
                  let index = Option.get msl.scene in
                  get (B.set_accel encoder ~index accel);
                  get (B.set_buffer encoder ~index:(index + 1) vertices);
                  get (B.set_buffer encoder ~index:(index + 2) spheres);
                  List.iteri
                    (fun k -> function
                      | `Buffer buffer -> get (B.set_buffer encoder ~index:(index + 3 + k) buffer)
                      | `Accel a -> get (B.set_accel encoder ~index:(index + 3 + k) a))
                    scene_tail;
                  get
                    (B.compute_use_accels encoder
                       ((accel :: Option.to_list extras.curves) @ resident)))
                scene;
              get
                (B.dispatch_threads encoder ~threads:(values.count, 1, 1) ~threadgroup:(256, 1, 1)))
        in
        match commands with
        | Some commands ->
            encode commands;
            Ok
              {
                owner = t;
                count = values.count;
                width = msl.output_widths.(0);
                index = 0;
                gpu_seconds = None;
                generation = t.generation;
                epoch = 0L;
                check_status = true;
              }
        | None ->
            let commands = get (B.begin_commands queue) in
            let committed = ref false in
            let receipt =
              Fun.protect
                ~finally:(fun () -> if not !committed then ignore (B.abandon commands))
                (fun () ->
                  encode commands;
                  let receipt = get (B.commit commands) in
                  committed := true;
                  receipt)
            in
            t.last_epoch <- receipt.epoch;
            if not wait then
              Ok
                {
                  owner = t;
                  count = values.count;
                  width = msl.output_widths.(0);
                  index = 0;
                  gpu_seconds = None;
                  generation = t.generation;
                  epoch = receipt.epoch;
                  check_status = true;
                }
            else begin
              get (B.complete_through queue receipt.epoch);
              let flags = get (B.read_buffer status ~offset:0L ~length:4) in
              t.status_reads <- t.status_reads + 1;
              let flags = Bytes.get_int32_le flags 0 in
              if flags <> 0l then raise (Diagnostic.Error (kernel_status flags));
              t.status_checked <- true;
              let output =
                {
                  owner = t;
                  count = values.count;
                  width = msl.output_widths.(0);
                  index = 0;
                  gpu_seconds = B.gpu_duration queue receipt;
                  generation = t.generation;
                  epoch = receipt.epoch;
                  check_status = true;
                }
              in
              Ok output
            end
      with
      | Diagnostic.Error error -> Error error
      | Out_of_memory -> error "GPU staging exceeds available memory."
  end

let select_output ~index output =
  if index < 0 || index >= Array.length output.owner.outputs then None
  else Some { output with index; width = output.owner.msl.output_widths.(index) }

let check_status t ~epoch =
  try
    if epoch > B.completed_epoch t.queue then get (B.complete_through t.queue epoch);
    if epoch < t.last_epoch then
      match List.assoc_opt epoch t.verdicts with Some d -> Error d | None -> Ok ()
    else begin
      if not t.status_checked then begin
        let flags = get (B.read_buffer (Option.get t.status.buffer) ~offset:0L ~length:4) in
        t.status_reads <- t.status_reads + 1;
        let flags = Bytes.get_int32_le flags 0 in
        if flags <> 0l then raise (Diagnostic.Error (kernel_status flags));
        t.status_checked <- true
      end;
      Ok ()
    end
  with Diagnostic.Error error -> Error error

(* A completed, status-checked output's slot and width. *)
let ready ?index output =
  let index = Option.value ~default:output.index index in
  if
    output.owner.closed
    || Domain.self () <> output.owner.domain
    || output.generation <> output.owner.generation
  then Error (Diagnostic.error ~code:"E_GPU" "GPU output was closed or superseded.")
  else if index < 0 || index >= Array.length output.owner.outputs then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output index is outside its producer.")
  else
    try
      (* a [~wait:false] dispatch is read once its queue has completed it *)
      if output.epoch > B.completed_epoch output.owner.queue then
        get (B.complete_through output.owner.queue output.epoch);
      (if output.check_status then
         match check_status output.owner ~epoch:output.epoch with
         | Ok () -> ()
         | Error diagnostic -> raise (Diagnostic.Error diagnostic));
      Ok (output.owner.outputs.(index), output.owner.msl.output_widths.(index))
    with Diagnostic.Error error -> Error error

(* A completed, status-checked output's mapped contents, [count * width] floats. *)
let completed ?index output =
  Result.bind (ready ?index output) @@ fun (slot, width) ->
  try
    let length = output.count * width in
    output.owner.readback_bytes <- output.owner.readback_bytes + (length * 4);
    match slot.floats with
    | Some floats when length > 0 ->
        get (B.invalidate_mapped (Option.get slot.buffer) ~length:(length * 4));
        Ok (`Floats (floats, width))
    | _ ->
        if length = 0 then Ok (`Floats (Bigarray.(Array1.create float32 c_layout 0), width))
        else
          Ok
            (`Bytes
               (get (B.read_buffer (Option.get slot.buffer) ~offset:0L ~length:(length * 4)), width))
  with Diagnostic.Error error -> Error error

let nonfinite_output () =
  Error (Diagnostic.error ~code:"E_KERNEL" "GPU output contains a nonfinite value.")

(* One pass from the float32 contents into doubles, the finite check folded in. *)
let unpack_floats (floats : B.floats) length =
  let values = Array.create_float length and finite = ref true in
  for index = 0 to length - 1 do
    let value = Bigarray.Array1.unsafe_get floats index in
    if not (Float.is_finite value) then finite := false;
    Array.unsafe_set values index value
  done;
  (values, !finite)

let readback ?index output =
  Result.bind (completed ?index output) (fun contents ->
      let count = output.count in
      let values, finite, width =
        match contents with
        | `Floats (floats, width) ->
            let values, finite = unpack_floats floats (count * width) in
            (values, finite, width)
        | `Bytes (bytes, width) ->
            let values, finite = unpack_array bytes (count * width) in
            (values, finite, width)
      in
      if not finite then nonfinite_output ()
      else
        Ok
          (match width with
          | 2 -> Flow.Eval.Vec2_array values
          | 3 -> Vec3_array values
          | 4 -> Vec4_array values
          | _ -> Float_array values))

(* [width] planes of [count] doubles from float32 contents, in parallel blocks *)
let to_planes ?(checked = false) (floats : B.floats) ~count ~width =
  let finite = Atomic.make true in
  let planes = Array.init width (fun _ -> Array.create_float count) in
  (* 64K-element blocks over the shared pool; each block writes its own range of every plane *)
  Parallel.for_ ~chunk_size:1 ~start:0
    ~finish:(((count + 65535) / 65536) - 1)
    (fun block ->
      let first = block * 65536 in
      let last = min count (first + 65536) - 1 in
      let ok = ref true in
      for component = 0 to width - 1 do
        let plane = Array.unsafe_get planes component in
        for i = first to last do
          let value = Bigarray.Array1.unsafe_get floats ((i * width) + component) in
          if (not checked) && not (Float.is_finite value) then ok := false;
          Array.unsafe_set plane i value
        done
      done;
      if not !ok then Atomic.set finite false);
  if Atomic.get finite then Ok planes else nonfinite_output ()

(* The output as [width] planes of [count] doubles, read where the kernel wrote them: what a planar
   store (an rdk attribute) owns, with no interleaved copy between. *)
let planes ?index output =
  Result.bind (completed ?index output) (fun contents ->
      let count = output.count in
      let floats, width =
        match contents with
        | `Floats (floats, width) -> (floats, width)
        | `Bytes (bytes, width) ->
            let floats = Bigarray.(Array1.create float32 c_layout (count * width)) in
            for i = 0 to (count * width) - 1 do
              floats.{i} <- Int32.float_of_bits (Bytes.get_int32_le bytes (4 * i))
            done;
            (floats, width)
      in
      to_planes floats ~count ~width)

(* a holder that no longer needs its buffer names it (a finaliser or a first read, any domain); the
   runner recycles or destroys it on its next dispatch *)
let release released id =
  let rec push () =
    let old = Atomic.get released in
    if not (Atomic.compare_and_set released old (id :: old)) then push ()
  in
  push ()

(* The planes, from any domain; racing readers may both convert, the first result wins. Once read,
   the holder no longer needs the buffer: it goes back to the runner at once, not at collection. *)
(* On the initial domain: the dispatch completed and its status read (once). *)
let settle t kept =
  if Atomic.get kept.settled || kept.destroyed then Ok ()
  else if Domain.self () <> t.domain then
    Error (Diagnostic.error ~code:"E_GPU" "A device output settles on its owner's domain.")
  else
    Result.map
      (fun () ->
        (* every output of the dispatch: its status covered them all *)
        List.iter
          (fun w ->
            Option.iter
              (fun other ->
                if other.kept_epoch = kept.kept_epoch then Atomic.set other.settled true)
              (Weak.get w 0))
          t.kept;
        Atomic.set kept.settled true)
      (check_status t ~epoch:kept.kept_epoch)

let materialise t kept =
  match Atomic.get kept.planes with
  | Some planes -> planes
  | None ->
      (* a reader on the initial domain waits for the kernel itself; any other reader runs after its
         cook settled (Sop.Session settles before every CPU node and at a cook's end) *)
      if not (Atomic.get kept.settled) then
        if Domain.self () = t.domain then begin
          if kept.kept_epoch > B.completed_epoch t.queue then
            ignore (B.complete_through t.queue kept.kept_epoch);
          Atomic.set kept.settled true
        end
        else invalid_arg "Run.materialise: a device output was read before its cook settled";
      if kept.destroyed then invalid_arg "Run.materialise: a device output was read after its free";
      let length = kept.kept_count * kept.kept_width in
      (* Nothing here can fail, so no reader (an rdk accessor on any domain) ever sees an error:
         the buffer is mapped, the range is its own, it is destroyed only under this lock after
         [destroyed] is set (its view stays valid), and the status check at [keep] covered the
         completion and every output's finiteness (the kernel flags a nonfinite output). *)
      Mutex.protect kept.lock (fun () ->
          if (not kept.destroyed) && length > 0 then
            match B.invalidate_mapped kept.kept_buffer ~length:(length * 4) with
            | Ok () -> ()
            | Error e -> invalid_arg ("Run.materialise: " ^ Ogpu.Error.to_string e));
      Atomic.incr kept.readers;
      let planes =
        Fun.protect
          ~finally:(fun () -> Atomic.decr kept.readers)
          (fun () ->
            match
              to_planes ~checked:true kept.kept_floats ~count:kept.kept_count ~width:kept.kept_width
            with
            | Ok planes -> planes
            | Error _ -> assert false)
      in
      if Atomic.compare_and_set kept.planes None (Some planes) then begin
        Atomic.incr t.materialised;
        release t.pool.released kept.id
      end;
      Option.get (Atomic.get kept.planes)

(* A settled, unread vec3 output's bounds, reduced where it is (shaders/bounds.rays): one group of
   1,024 lanes, each lane's min and max (of every 1,024th element) read back (24 KB at most) and finished here. On the
   owner's domain; [None] elsewhere, once read (its planes are then the host's to scan) or on any
   device failure. *)
let bounds_of t kept =
  if
    kept.kept_width <> 3 || kept.kept_count = 0 || t.closed
    || Domain.self () <> t.domain
    || kept.destroyed
    || Atomic.get kept.planes <> None
    || not (Atomic.get kept.settled)
  then None
  else
    let n = kept.kept_count in
    let lanes = min n 1024 in
    try
      let compiled =
        match Pipelines.get t.pipelines Emit.bounds with
        | Ok compiled -> compiled
        | Error error -> raise (Diagnostic.Error error)
      in
      let out =
        get
          (B.create_buffer t.device
             {
               label = Some "Flow bounds";
               size = Int64.of_int (lanes * 24);
               usage = [ Storage; Copy_src ];
             })
      in
      Fun.protect
        ~finally:(fun () -> ignore (B.destroy_buffer out))
        (fun () ->
          let count = Bytes.create 4 in
          Bytes.set_int32_le count 0 (Int32.of_int n);
          let commands = get (B.begin_commands t.queue) in
          let committed = ref false in
          let receipt =
            Fun.protect
              ~finally:(fun () -> if not !committed then ignore (B.abandon commands))
              (fun () ->
                let encoder = get (B.compute_encoder commands) in
                Fun.protect
                  ~finally:(fun () -> ignore (B.end_compute encoder))
                  (fun () ->
                    get (B.set_pipeline encoder compiled.pipeline);
                    get (B.set_buffer encoder ~index:0 kept.kept_buffer);
                    get (B.set_buffer encoder ~index:1 out);
                    get (B.set_bytes encoder ~index:2 count);
                    get (B.dispatch_threads encoder ~threads:(1024, 1, 1) ~threadgroup:(1024, 1, 1)));
                let receipt = get (B.commit commands) in
                committed := true;
                receipt)
          in
          get (B.complete_through t.queue receipt.epoch);
          let lane, finite =
            unpack_array (get (B.read_buffer out ~offset:0L ~length:(lanes * 24))) (lanes * 6)
          in
          t.readback_bytes <- t.readback_bytes + (lanes * 24);
          if not finite then None
          else begin
            let b = Array.sub lane 0 6 in
            for l = 1 to lanes - 1 do
              for c = 0 to 2 do
                b.(c) <- Float.min b.(c) lane.((l * 6) + c);
                b.(c + 3) <- Float.max b.(c + 3) lane.((l * 6) + c + 3)
              done
            done;
            Some b
          end)
    with Diagnostic.Error _ -> None

let next_kept = Atomic.make 0

(* Its owner (a session dropping the result) no longer needs it, from any domain. [held] (it was
   handed out, now or before): it is retired, still readable; its buffer comes back when it is
   collected or read, or when {!reclaim} reads it. Otherwise no one reads it again: its buffer comes
   back at once. *)
let release_kept t kept ~held =
  if Atomic.get kept.planes = None then
    let retire =
      Mutex.protect kept.lock (fun () ->
          if kept.destroyed then false
          else if held || kept.retired then (
            kept.retired <- true;
            true)
          else (
            kept.destroyed <- true;
            Atomic.incr t.pool.freed;
            release t.pool.released kept.id;
            false))
    in
    if retire then begin
      let w = Weak.create 1 in
      Weak.set w 0 (Some kept);
      push t.pool.retiring (kept.id, w)
    end

(* Hand the output's buffer to a lazy planar store: nothing is read or waited for now. *)
let keep ?index output : (Flow_ir.Gpu.planes, Diagnostic.t) result =
  let index = Option.value ~default:output.index index in
  let t = output.owner in
  if t.closed || Domain.self () <> t.domain || output.generation <> t.generation then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output was closed or superseded.")
  else if index < 0 || index >= Array.length t.outputs then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output index is outside its producer.")
  else
    let slot = t.outputs.(index) and width = t.msl.output_widths.(index) in
    match (slot.buffer, slot.floats) with
    | Some buffer, Some floats when output.count > 0 && output.epoch > 0L ->
        let kept =
          {
            id = Atomic.fetch_and_add next_kept 1;
            kept_buffer = buffer;
            kept_floats = floats;
            kept_count = output.count;
            kept_width = width;
            lock = Mutex.create ();
            planes = Atomic.make None;
            destroyed = false;
            retired = false;
            placeholder = Array.make width 0.;
            kept_epoch = output.epoch;
            settled = Atomic.make false;
            readers = Atomic.make 0;
            thaw = ignore;
            pressure = device_pressure slot.capacity;
          }
        in
        let capacity = slot.capacity in
        slot.buffer <- None;
        slot.floats <- None;
        slot.capacity <- 0;
        slot.uploaded <- None;
        let weak = Weak.create 1 in
        Weak.set weak 0 (Some kept);
        t.kept <- weak :: t.kept;
        kept.thaw <- (fun () -> ignore (materialise t kept));
        Hashtbl.replace t.pool.held kept.id (buffer, floats, capacity, kept.readers);
        (* any domain, any allocation: only name the holder; after close nothing drains it *)
        let id = kept.id and released = t.pool.released in
        Gc.finalise (fun _ -> release released id) kept;
        bindable := (kept.placeholder, weak) :: !bindable;
        let resident : Flow.Eval.value =
          match width with
          | 1 -> Float_array kept.placeholder
          | 2 -> Vec2_array kept.placeholder
          | 3 -> Vec3_array kept.placeholder
          | _ -> Vec4_array kept.placeholder
        in
        Ok
          {
            count = output.count;
            width;
            materialise = (fun () -> materialise t kept);
            resident = Some resident;
            settle = (fun () -> settle t kept);
            release = (fun ~held -> release_kept t kept ~held);
            bounds = (fun () -> bounds_of t kept);
          }
    | _ ->
        (* an empty, unmapped or caller-encoded output is read now *)
        Result.map
          (fun planes ->
            {
              Flow_ir.Gpu.count = output.count;
              width;
              materialise = (fun () -> planes);
              resident = None;
              settle = (fun () -> Ok ());
              release = (fun ~held:_ -> ());
              bounds = (fun () -> None);
            })
          (planes ~index output)

(* A display borrowing a settled kept output's buffer: a reader the pool waits for (no reuse, no
   thaw) until [unpin]; [valid] while the buffer exists (a close destroys it). Initial domain. *)
let pin placeholder =
  bindable := List.filter (fun (_, w) -> Weak.check w 0) !bindable;
  List.find_map
    (fun (p, w) ->
      if p != placeholder then None
      else
        Option.bind (Weak.get w 0) (fun kept ->
            if kept.destroyed || not (Atomic.get kept.settled) then None
            else begin
              Atomic.incr kept.readers;
              let pinned = Atomic.make true in
              Some
                ( kept.kept_buffer,
                  kept.kept_count,
                  (fun () -> Atomic.get pinned && not kept.destroyed),
                  fun () -> if Atomic.exchange pinned false then Atomic.decr kept.readers )
            end))
    !bindable

let buffer ?index output =
  let index = Option.value ~default:output.index index in
  if
    index < 0
    || index >= Array.length output.owner.outputs
    || output.owner.closed
    || Domain.self () <> output.owner.domain
    || output.generation <> output.owner.generation
  then None
  else output.owner.outputs.(index).buffer

(* the count record of a fast kernel's dispatch, while it is the runner's current output *)
let count_record output =
  if
    output.owner.closed
    || Domain.self () <> output.owner.domain
    || output.generation <> output.owner.generation
    || output.owner.msl.counts = None
  then None
  else output.owner.counted.buffer

let count output = output.count
let epoch output = output.epoch
let width output = output.width
let gpu_seconds output = output.gpu_seconds

let max_total_threads t =
  Result.map
    (fun (compiled : Pipelines.compiled) -> B.max_total_threads compiled.pipeline)
    (Pipelines.get t.pipelines t.msl)

(* on the initial domain, once its runners are closed *)
let close_pool pool =
  drain_pool pool;
  ignore (Atomic.exchange pool.released []);
  ignore (Atomic.exchange pool.retiring []);
  pool.retired <- [];
  Hashtbl.iter (fun _ (buffer, _, _, _) -> ignore (B.destroy_buffer buffer)) pool.held;
  Hashtbl.reset pool.held;
  List.iter (fun (buffer, _, _) -> ignore (B.destroy_buffer buffer)) pool.free;
  pool.free <- []

let close t =
  if Domain.self () <> t.domain then invalid_arg "Run.close: creating domain required";
  if not t.closed then begin
    (* a holder still alive reads its planes now, while its buffer exists (a finalised holder's
       weak entry can outlive it: only those still holding count) *)
    drain t;
    List.iter
      (fun weak ->
        Option.iter
          (fun kept ->
            match Hashtbl.find_opt t.pool.held kept.id with
            | Some (buffer, _, _, _) ->
                (try ignore (materialise t kept) with Diagnostic.Error _ -> ());
                Mutex.protect kept.lock (fun () -> kept.destroyed <- true);
                Hashtbl.remove t.pool.held kept.id;
                ignore (B.destroy_buffer buffer)
            | None -> ())
          (Weak.get weak 0))
      t.kept;
    t.kept <- [];
    if t.own_pool then close_pool t.pool;
    Array.iter
      (fun slot ->
        Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
        slot.buffer <- None;
        slot.bytes <- Bytes.empty;
        slot.floats <- None;
        slot.capacity <- 0;
        slot.uploaded <- None)
      (Array.concat [ t.inputs; t.outputs; t.gathers; [| t.table; t.status; t.counted |] ]);
    t.closed <- true
  end

module Private = struct
  let diagnostic = diagnostic
  let buffer_creations t = t.creations
  let input_uploads t = t.input_uploads
  let input_uploaded_bytes t = t.input_uploaded_bytes
  let status_reads t = t.status_reads
  let check_status t ~epoch = check_status t ~epoch
  let readback_bytes t = t.readback_bytes
  let materialisations t = Atomic.get t.materialised

  let held_buffers t =
    drain t;
    Hashtbl.length t.pool.held

  let pool_freed pool = Atomic.get pool.freed

  let pool_held pool =
    drain_pool pool;
    Hashtbl.length pool.held

  let create_owned = create_owned
end

(* The device [array/compact] of a resident fold: the kept indices stay in [kept], the count is read
   back (the one wait of a fold level). *)
let compact commands pipelines ~mask ~count ~count_in ~kept ~total =
  try
    let compiled =
      match Pipelines.get pipelines Emit.compaction with
      | Ok compiled -> compiled
      | Error error -> raise (Diagnostic.Error error)
    in
    let bound = Bytes.create 8 in
    Bytes.set_int32_le bound 0 (Int32.of_int count);
    Bytes.set_int32_le bound 4 (if count_in = None then 0l else 1l);
    let encoder = get (B.compute_encoder commands) in
    Fun.protect
      ~finally:(fun () -> ignore (B.end_compute encoder))
      (fun () ->
        get (B.set_pipeline encoder compiled.pipeline);
        get (B.set_buffer encoder ~index:0 mask);
        get (B.set_buffer encoder ~index:1 kept);
        get (B.set_buffer encoder ~index:2 total);
        get (B.set_bytes encoder ~index:3 bound);
        get (B.set_buffer encoder ~index:4 (Option.value ~default:total count_in));
        get (B.dispatch_threads encoder ~threads:(1024, 1, 1) ~threadgroup:(1024, 1, 1)));
    Ok ()
  with Diagnostic.Error error -> Error error

(* a count cell, once the queue has completed [epoch] *)
let read_count queue buffer ~epoch =
  try
    if epoch > B.completed_epoch queue then get (B.complete_through queue epoch);
    Ok
      (Int32.to_int (Bytes.get_int32_le (get (B.read_buffer buffer ~offset:0L ~length:4)) 0)
      land 0xffffffff)
  with Diagnostic.Error error -> Error error

(* read a device buffer the queue has finished writing *)
let read_floats queue buffer ~epoch ~length =
  try
    if epoch > B.completed_epoch queue then get (B.complete_through queue epoch);
    let values, _ =
      if length = 0 then ([||], true)
      else unpack_array (get (B.read_buffer buffer ~offset:0L ~length:(length * 4))) length
    in
    Ok values
  with Diagnostic.Error error -> Error error

(* The device scene of [scene/trace]: structures built through the OGPU acceleration API, with the static
   facts its kernel is emitted with. Motion builds motion structures where the device has
   [Ray_tracing_motion]; elsewhere the scene is built at the shutter midpoint and its kernel is static. *)
type instance = {
  first : int;
  triangles : int;
  transform : float array;
  transform_end : float array option;
}

type description = {
  triangles : bytes;
  triangles_end : bytes option;
  instances : instance array;
  sphere_data : bytes;
  strand_data : bytes;
}

type built = {
  scene : scene;
  extras : extras;
  instanced : bool;
  motion : bool;
  curves : [ `Loop | `Native ] option;
  release : unit -> unit;
}

let floats_of bytes =
  Array.init
    (Bytes.length bytes / 4)
    (fun k -> Int32.float_of_bits (Bytes.get_int32_le bytes (4 * k)))

let bytes_of floats =
  let bytes = Bytes.create (4 * Array.length floats) in
  Array.iteri (fun k v -> Bytes.set_int32_le bytes (4 * k) (Int32.bits_of_float v)) floats;
  bytes

let build_scene ?caps gpu d =
  let device = Renderer.gpu_device gpu and queue = Renderer.gpu_queue gpu in
  let caps = Option.value caps ~default:(B.capabilities device) in
  let moving =
    d.triangles_end <> None || Array.exists (fun i -> i.transform_end <> None) d.instances
  in
  let motion = moving && Ogpu.Caps.has caps Ray_tracing_motion in
  let ending i = Option.value i.transform_end ~default:i.transform in
  (* without motion support: the shutter midpoint, as rays_pathtracer's Mesh_data.shutter_midpoint *)
  let start, triangles =
    if moving && not motion then
      ( (fun i -> Flow.Ray_host.lerp i.transform (ending i) 0.5),
        match d.triangles_end with
        | None -> d.triangles
        | Some e -> bytes_of (Flow.Ray_host.lerp (floats_of d.triangles) (floats_of e) 0.5) )
    else ((fun i -> i.transform), d.triangles)
  in
  let buffers = ref [] and structures = ref [] in
  let release () =
    List.iter (fun a -> ignore (B.destroy_accel a)) !structures;
    List.iter (fun b -> ignore (B.destroy_buffer b)) !buffers
  in
  let buffer ?(memory = Ogpu.Types.Shared) size =
    let b =
      get
        (B.create_buffer ~memory device
           { label = Some "Flow scene"; size = Int64.of_int (max 16 size); usage = [ Storage ] })
    in
    buffers := b :: !buffers;
    b
  in
  let upload bytes =
    let b = buffer (Bytes.length bytes) in
    if Bytes.length bytes > 0 then get (B.write_buffer b ~offset:0L bytes);
    b
  in
  let accel descriptor =
    let a = get (B.create_accel device descriptor) in
    structures := a :: !structures;
    let scratch =
      buffer ~memory:Ogpu.Types.Device_local (Int64.to_int (B.accel_sizes a).build_scratch_size)
    in
    let commands = get (B.begin_commands queue) in
    let encoder = get (B.accel_encoder commands) in
    get (B.build_accel encoder a ~scratch ());
    get (B.end_accel encoder);
    get (B.complete_through queue (get (B.commit commands)).epoch);
    a
  in
  let shutter =
    { B.keyframes = 2; start_time = 0.; end_time = 1.; start_border = Clamp; end_border = Clamp }
  in
  try
    if d.instances <> [||] && d.triangles_end <> None then
      raise
        (Diagnostic.Error
           (Diagnostic.error ~code:"E_GPU"
              "scene/trace: vertex motion is a flat scene's; instances move by their transforms."));
    let vertices = upload triangles and spheres = upload d.sphere_data in
    let sphere_count = Bytes.length d.sphere_data / 16 in
    let triangles_at first count =
      B.Triangles
        {
          vertices;
          offset = Int64.of_int (36 * first);
          length = Int64.of_int (36 * count);
          vertex_stride = 12;
          vertex_count = 3 * count;
        }
    in
    let built =
      if d.instances = [||] then begin
        let count = Bytes.length triangles / 36 in
        match d.triangles_end with
        | Some e when motion ->
            let vertices_end = upload e in
            let accel =
              accel
                (B.Motion_blas
                   {
                     geometries =
                       [
                         B.Motion_triangles
                           {
                             keyframes =
                               [
                                 { buffer = vertices; offset = 0L };
                                 { buffer = vertices_end; offset = 0L };
                               ];
                             vertex_stride = 12;
                             vertex_count = 3 * count;
                           };
                       ];
                     motion = shutter;
                   })
            in
            {
              scene = { accel; vertices; spheres; sphere_count; instances = None };
              extras = { no_extras with vertices_end = Some vertices_end };
              instanced = false;
              motion = true;
              curves = None;
              release;
            }
        | _ ->
            {
              scene =
                {
                  accel = accel (B.Blas { geometries = [ triangles_at 0 count ] });
                  vertices;
                  spheres;
                  sphere_count;
                  instances = None;
                };
              extras = no_extras;
              instanced = false;
              motion = false;
              curves = None;
              release;
            }
      end
      else begin
        let prototypes =
          List.sort_uniq compare
            (Array.to_list (Array.map (fun i -> (i.first, i.triangles)) d.instances))
        in
        let blas =
          List.map
            (fun (first, count) -> accel (B.Blas { geometries = [ triangles_at first count ] }))
            prototypes
        in
        let records =
          Array.mapi
            (fun k i ->
              let structure_index =
                Option.get (List.find_index (( = ) (i.first, i.triangles)) prototypes)
              in
              {
                B.instance = { transform = start i; mask = 0xFFFF_FFFF; structure_index };
                user_id = k;
              })
            d.instances
        in
        let count = Array.length d.instances in
        let records, kind, motion_transforms =
          if motion then
            ( get
                (B.pack_motion_instances device
                   (Array.mapi
                      (fun k record ->
                        {
                          B.record;
                          transforms_start = 2 * k;
                          transforms_count = 2;
                          start_time = 0.;
                          end_time = 1.;
                          start_border = Clamp;
                          end_border = Clamp;
                        })
                      records)),
              B.Motion_instances,
              Some
                ( upload
                    (get
                       (B.pack_transforms
                          (Array.concat
                             (Array.to_list
                                (Array.map (fun i -> [| i.transform; ending i |]) d.instances))))),
                  0L,
                  2 * count ) )
          else (get (B.pack_instance_records device records), B.User_id_instances, None)
        in
        let accel =
          accel
            (B.Tlas_of
               {
                 instances = upload records;
                 offset = 0L;
                 instance_count = count;
                 kind;
                 structures = blas;
                 motion_transforms;
               })
        in
        (* per instance: its first triangle, then the world-to-object 3x4 (static) or both object-to-world
         keyframes (motion), padded to 16 or 32 floats *)
        let table =
          Array.concat
            (Array.to_list
               (Array.map
                  (fun i ->
                    let row =
                      if motion then Array.concat [ i.transform; ending i; Array.make 7 0. ]
                      else Array.append (Flow.Ray_host.world_to_object (start i)) [| 0.; 0.; 0. |]
                    in
                    Array.append [| float i.first |] row)
                  d.instances))
        in
        {
          scene =
            { accel; vertices; spheres; sphere_count; instances = Some (upload (bytes_of table)) };
          extras = no_extras;
          instanced = true;
          motion;
          curves = None;
          release;
        }
      end
    in
    (* strands: Metal round linear curves over the segment records themselves (stride 16, radius at .w) where
       the device has Ray_tracing_curves, else the kernel's in-loop capsule test over the same buffer *)
    let segments = Bytes.length d.strand_data / 32 in
    if segments = 0 then Ok built
    else begin
      let strands = upload d.strand_data in
      let native = Ogpu.Caps.has caps Ray_tracing_curves in
      let curves =
        if not native then None
        else
          let indices = Bytes.create (4 * segments) in
          for k = 0 to segments - 1 do
            Bytes.set_int32_le indices (4 * k) (Int32.of_int (2 * k))
          done;
          let indices = upload indices in
          Some
            (accel
               (B.Blas
                  {
                    geometries =
                      [
                        B.Curves
                          {
                            control_points = [ { buffer = strands; offset = 0L } ];
                            control_stride = 16;
                            control_point_count = 2 * segments;
                            radii = [ { buffer = strands; offset = 12L } ];
                            radius_stride = 16;
                            indices;
                            index_offset = 0L;
                            segment_count = segments;
                            control_points_per_segment = 2;
                            curve_type = Round_curve;
                            basis = Linear_basis;
                            caps = Sphere_caps;
                          };
                      ];
                  }))
      in
      Ok
        {
          built with
          extras = { built.extras with strands = Some strands; strand_count = segments; curves };
          curves = Some (if native then `Native else `Loop);
        }
    end
  with Diagnostic.Error error ->
    release ();
    Error error
