module B = Ogpu.Backend
module P = Flow_ir.Packed

type slot = {
  mutable buffer : B.buffer option;
  mutable capacity : int;
  mutable bytes : bytes;
  mutable mapped : bool;
      (** a mapped slot (inputs, gathers, outputs): its view is written and read in place *)
  mutable uploaded : (float array * int) option;
  mutable low : int;  (** dispatches in a row asking under a quarter of [capacity] *)
}

(* An output buffer a lazy planar store holds after its dispatch completed and passed its status
   check: the runner hands the slot's buffer over and makes a fresh one for its next dispatch. The
   holder materialises it once (from any domain: an invalidate and a conversion, no queue work);
   collected, its buffer comes back through [released] and is destroyed on the initial domain. *)
type device_pressure

(* a block the GC counts as [bytes] of memory, as a Bigarray its data: a dead holder is collected as
   promptly as an array of its buffer's size *)
external device_pressure : int -> device_pressure = "rays_flow_gpu_device_pressure"
external uint32_word : B.floats -> int -> int = "rays_flow_gpu_uint32_word" [@@noalloc]

(* What a holder's buffer has come to. Who only retires a holder (a session) or borrows its buffer (a
   display's pin) keeps this, never the holder: a live holder means a reader can still come, so only a
   reader may keep it alive, and a borrow's end then gives the buffer back with no further collection. *)
type fate = {
  lock : Mutex.t;  (** orders an invalidate against close's destroy; never held while converting *)
  planes : float array array option Atomic.t;
  mutable destroyed : bool;  (** its buffer is gone: closed (read first) or freed (never read) *)
  mutable retired : bool;  (** handed out once: it stays readable from then on *)
  readers : int Atomic.t;  (** conversions and pins in progress: its buffer is not reused under them *)
}

type kept = {
  id : int;
  kept_buffer : B.buffer;
  kept_count : int;
  kept_width : int;
  uint32 : bool;
  fate : fate;
  placeholder : float array;
      (** one element of zeros: a later kernel's source that is this array reads the buffer *)
  kept_epoch : int64;
  settled : bool Atomic.t;  (** completed, its status read: CPU readers may read it *)
  mutable bounds_memo : float array option option;  (** its box once reduced: it never changes *)
  mutable thaw : unit -> unit;  (** materialise it (initial domain), so its buffer can be reused *)
  pressure : device_pressure; [@warning "-69"]  (** the buffer's size, told to the GC; never read *)
}

(* Buffers runners share (a host's, across edits): those held by kept outputs, by holder; free ones
   of any size class (capacity doubles, so a class is [length, 2 * length]), {!free_budget}; collected
   or read holders' ids, from any domain; retired holders, whose owner (a session) dropped them but
   which a reader may still hold: their buffers stay theirs until they are collected or read, and
   past {!retired_budget_for} bytes the oldest is read into owned planes so its buffer comes back. *)
type pool = {
  held : (int, B.buffer * int * fate) Hashtbl.t;
  mutable free : (B.buffer * int) list;
  mutable free_ages : (B.buffer * int) list;  (** the {!tick} each free buffer came back *)
  mutable tick : int;  (** drains so far: an idle device's clock, no real one *)
  released : int list Atomic.t;
  retiring : (int * kept Weak.t) list Atomic.t;
  freed : int Atomic.t;  (** holders freed unread *)
  mutable retired : (int * kept Weak.t) list;
      (** oldest first, by id: only the one {!reclaim} reads is ever [Weak.get], since a get while
          the GC marks keeps the holder alive another cycle *)
  mutable collected_at : int;  (** bytes created by the pool's owner at its last requested cycle *)
  mutable orphans : kept Weak.t list;
      (** live holders of closed runners: settled, unread, theirs until {!close_pool} *)
  mutable created : int;
  mutable free_limit : int;  (** {!retired_budget_for}, known from the first {!reclaim} *)
}

let pool () =
  {
    held = Hashtbl.create 16;
    free = [];
    free_ages = [];
    tick = 0;
    released = Atomic.make [];
    retiring = Atomic.make [];
    freed = Atomic.make 0;
    retired = [];
    collected_at = 0;
    created = 0;
    free_limit = 512 lsl 20;
    orphans = [];
  }

(* Kept outputs a later dispatch may bind by their placeholder, on the initial domain. *)
let bindable : (float array * kept Weak.t) list ref = ref []

(* The held buffer a placeholder stands for and the floats it holds, while its holder has not read
   it back (its buffer is then released). *)
let held array =
  bindable := List.filter (fun (_, w) -> Weak.check w 0) !bindable;
  List.find_map
    (fun (p, w) ->
      if p != array then None
      else
        Option.bind (Weak.get w 0) (fun kept ->
            if Atomic.get kept.fate.planes = None && not kept.fate.destroyed then
              Some (kept.kept_buffer, kept.kept_count * kept.kept_width)
            else None))
    !bindable

(* ... as an input, when it covers [length] floats *)
let held_input array ~length =
  Option.bind (held array) (fun (buffer, floats) -> if floats >= length then Some buffer else None)

(* a uniform layout entry, parsed once per runner: a dispatch only writes it *)
type entry =
  | Uniform_at of int * int * int  (** uniform, offset, width *)
  | Gather_at of int * int  (** gather, offset *)
  | Strands_at of int
  | Spheres_at of int
  | Frame_at of int * int  (** frame instruction, offset *)

let entries (kernel : Emit.kernel) =
  Array.of_list
    (List.map
       (fun (name, offset, width) ->
         if String.starts_with ~prefix:"uniform:" name then
           Uniform_at (int_of_string (String.sub name 8 (String.length name - 8)), offset, width)
         else
           match String.split_on_char ':' name with
           | [ "gather"; k; _ ] -> Gather_at (int_of_string k, offset)
           | [ "scene"; "strands" ] -> Strands_at offset
           | [ "scene"; "spheres" ] -> Spheres_at offset
           | [ "frame"; index; _ ] -> Frame_at (int_of_string index, offset)
           | _ -> invalid_arg ("Run: uniform layout entry " ^ name))
       kernel.uniform_layout)

type t = {
  device : B.device;
  queue : B.queue;
  pipelines : Pipelines.t;
  kernel : Emit.kernel;
  inputs : slot array;
  outputs : slot array;
  table : slot;
  gathers : slot array;
  status : slot;
  counted : slot;
  groups : slot;  (** a counted dispatch's threadgroup counts, written on the device *)
  uniforms : bytes;
  layout : entry array;
  arguments : bytes;  (** a counted dispatch's arguments to Kernels.dispatch_arguments *)
  mutable closed : bool;
  mutable bounds_out : B.buffer option;  (** the reduction's lanes, made once and reused *)
  mutable creations : int;
  mutable input_uploads : int;
  mutable input_uploaded_bytes : int;
  mutable status_reads : int;
  mutable readback_bytes : int;
  mutable bounds_readback_bytes : int;
  materialised_bytes : int Atomic.t;
  mutable status_checked : bool;
  mutable generation : int;
  mutable table_ready : bool;
  domain : Domain.id;
  mutable last_epoch : int64;
  mutable kept : (int64 * bool Atomic.t * kept Weak.t) list;
      (** live holders, materialised at close, with each one's epoch and settled flag: a cook's
          settle marks its siblings through those, never [Weak.get] (a get while the GC marks keeps
          the holder alive that cycle, and every cook settles: no holder would ever be collected) *)
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
  prototypes : B.accel option;
}

let no_extras =
  { vertices_end = None; strands = None; strand_count = 0; curves = None; prototypes = None }

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
let slot () =
  { buffer = None; capacity = 0; bytes = Bytes.empty; mapped = false; uploaded = None; low = 0 }

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
  else if Int32.logand flags 8l <> 0l then
    Diagnostic.error ~code:"E_LIST_RANGE" "nth index is out of range or is not a whole number."
  else if Int32.logand flags 32l <> 0l then
    Diagnostic.error ~code:"E_ARRAY_RANGE" "array/slice needs a non-negative whole length."
  else if Int32.logand flags 16l <> 0l then
    Diagnostic.error ~code:"E_ARRAY_RANGE" "An array slice must be within its source."
  else if Int32.logand flags 64l <> 0l then
    Diagnostic.error ~code:"E_GPU_UINT32_RANGE" "GPU uint32 value must be within [0, 4294967295]."
  else Diagnostic.error ~code:"E_KERNEL" "GPU output contains a nonfinite value."

let pack bytes offset value =
  let bits = Int32.bits_of_float value in
  if not (Float.is_finite (Int32.float_of_bits bits)) then raise (nonfinite ());
  Bytes.set_int32_le bytes offset bits

let pack_uint32 bytes offset value =
  if (not (Float.is_integer value)) || value < 0. || value > 4294967295. then
    raise (Diagnostic.Error (kernel_status 64l));
  Bytes.set_int32_le bytes offset (Int32.of_int (int_of_float value))

let upload_uint32 buffer slot source count =
  let length = count * 4 in
  if Bytes.length slot.bytes <> length then slot.bytes <- Bytes.create length;
  for i = 0 to count - 1 do
    pack_uint32 slot.bytes (i * 4) source.(i)
  done;
  if count > 0 then get (B.write_buffer buffer ~offset:0L slot.bytes)

(* a mapped buffer's host view, made on a discrete device when first asked for *)
let view buffer = Option.get (B.mapped_view buffer)

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

let create_owned ?pool:shared device queue pipelines (kernel : Emit.kernel) =
  if not (Domain.is_main_domain ()) then invalid_arg "Run.create: initial domain required";
  {
    device;
    queue;
    pipelines;
    kernel;
    inputs = Array.map (fun _ -> slot ()) kernel.input_widths;
    outputs = Array.map (fun _ -> slot ()) kernel.output_widths;
    table = slot ();
    gathers = Array.map (fun _ -> slot ()) kernel.gathers;
    status = slot ();
    counted = slot ();
    groups = slot ();
    uniforms = Bytes.create kernel.uniform_bytes;
    layout = entries kernel;
    arguments = Bytes.create 16;
    closed = false;
    bounds_out = None;
    creations = 0;
    input_uploads = 0;
    input_uploaded_bytes = 0;
    status_reads = 0;
    readback_bytes = 0;
    bounds_readback_bytes = 0;
    materialised_bytes = Atomic.make 0;
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

(* Free buffers of one size class kept for reuse while their bytes stay under this, and all classes
   under the pool's [free_limit]. A collection returns several dead holders' buffers at once, which the
   next cooks take. Bytes only — never "any 8" of any size. *)
let free_budget = 256 lsl 20
let held_owner = Mem.plan ~pool:Device "flow_gpu.held"
let free_owner = Mem.cache ~pool:Device "flow_gpu.free"
let slot_owner = Mem.plan ~pool:Device "flow_gpu.slots"
let free_idle_drains = 1024
let slot_shrink_after = 240
let slot_shrink_floor = 1 lsl 20

(* on the initial domain: recycle or destroy what collected or read holders gave back (a buffer a
   reader is still converting waits for the next drain) *)
let drain_pool pool =
  pool.tick <- pool.tick + 1;
  if pool.tick mod 64 = 0 then pool.orphans <- List.filter (fun w -> Weak.check w 0) pool.orphans;
  (* a free buffer nothing has taken for this long goes back to the device *)
  if pool.free <> [] && pool.tick mod 64 = 0 then begin
    let stale (buffer, _) =
      pool.tick - Option.value ~default:pool.tick (List.assq_opt buffer pool.free_ages)
      > free_idle_drains
    in
    let old, young = List.partition stale pool.free in
    pool.free <- young;
    List.iter
      (fun (buffer, capacity) ->
        Mem.release free_owner capacity;
        pool.free_ages <- List.remove_assq buffer pool.free_ages;
        ignore (B.destroy_buffer buffer))
      old
  end;
  List.iter
    (fun id ->
      match Hashtbl.find_opt pool.held id with
      | Some (_, _, fate) when Atomic.get fate.readers > 0 -> push pool.released id
      | Some (buffer, capacity, _) ->
          Hashtbl.remove pool.held id;
          Mem.release held_owner capacity;
          (* the size class's own free bytes: another class's big buffers must not turn this
             one's away (they would be destroyed and made again each frame) *)
          let fits free limit = free + capacity <= max limit (8 * capacity) in
          let total, own =
            List.fold_left
              (fun (total, own) (_, c) -> (total + c, if c = capacity then own + c else own))
              (0, 0) pool.free
          in
          (* a frame of big outputs must find its buffers again *)
          if fits own free_budget && fits total pool.free_limit then begin
            Mem.charge free_owner capacity;
            (* a dead buffer: under RAYS_MEM_CLOBBER a wrong lifetime reads NaN at once *)
            if Mem.clobber () then
              Option.iter (fun v -> Bigarray.Array1.fill v Float.nan) (B.mapped_view buffer);
            (* the host is done with it: a discrete device drops its host copy *)
            ignore (B.release_mapped_view buffer);
            pool.free_ages <- (buffer, pool.tick) :: pool.free_ages;
            pool.free <- (buffer, capacity) :: pool.free
          end
          else ignore (B.destroy_buffer buffer)
      | None -> ())
    (Atomic.exchange pool.released [])

let drain t =
  drain_pool t.pool;
  t.kept <- List.filter (fun (_, _, w) -> Weak.check w 0) t.kept

let take_free pool length =
  match
    List.find_opt (fun (_, capacity) -> capacity >= length && capacity <= 2 * length) pool.free
  with
  | Some ((buffer, capacity) as held) ->
      pool.free <- List.filter (( != ) held) pool.free;
      pool.free_ages <- List.remove_assq buffer pool.free_ages;
      Mem.release free_owner capacity;
      Some held
  | None -> None

(* Past this many bytes of retired holders still unread, the oldest is read now (owned planes: its
   later readers see the same values) and its buffer comes back, so device memory stays bounded. A
   retired holder stays readable for as long as it is alive and nothing here can tell it has no future
   reader, so a live one is always read; only a collected one goes unread. A quarter of the device's
   working set, 512 MiB when it does not say. *)
let retired_budget_for device =
  let recommended = Int64.to_int (B.recommended_working_set device) in
  if recommended > 0 then recommended / 4 else 512 lsl 20

let reclaim pool ~device =
  pool.free_limit <- retired_budget_for device;
  drain_pool pool;
  let held (id, w) = Weak.check w 0 && Hashtbl.mem pool.held id in
  let refresh () =
    pool.retired <- List.filter held (pool.retired @ List.rev (Atomic.exchange pool.retiring []))
  in
  refresh ();
  let bytes sum (id, _) =
    match Hashtbl.find_opt pool.held id with
    | Some (_, capacity, _) -> sum + capacity
    | None -> sum
  in
  (* Only a collection can tell a dead retired holder from a live one, so device memory must not wait
     for the collector's own pace: past this many retired bytes, and once per that many bytes the pool
     has had to create, ask for one cycle. What it finds dead comes back unread. *)
  if
    List.fold_left bytes 0 pool.retired > free_budget
    && pool.created - pool.collected_at >= free_budget
  then begin
    pool.collected_at <- pool.created;
    Gc.major ();
    drain_pool pool;
    refresh ()
  end;
  if List.fold_left bytes 0 pool.retired > retired_budget_for device then begin
    (match pool.retired with
    | (_, w) :: rest ->
        pool.retired <- rest;
        Option.iter (fun k -> if Atomic.get k.fate.readers = 0 then k.thaw ()) (Weak.get w 0)
    | [] -> ());
    drain_pool pool
  end

let recycled pool ~device length =
  match take_free pool length with
  | Some _ as found -> found
  | None ->
      reclaim pool ~device;
      take_free pool length

let created pool bytes = pool.created <- pool.created + bytes

let create ?pool gpu = create_owned ?pool (Renderer.gpu_device gpu) (Renderer.gpu_queue gpu)

(* An input the device now holds elsewhere: its upload buffer and the host array it was compared to
   would otherwise stay pinned for the runner's life (a measurement's copy, a host P before the
   chain went resident). *)
let forget_input slot =
  Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
  slot.buffer <- None;
  slot.mapped <- false;
  slot.capacity <- 0;
  slot.uploaded <- None

let ensure ?(mapped = false) t slot length =
  (if mapped && slot.buffer = None && length > 0 then
     (* an output slot whose last buffer a holder took: a recycled buffer of the size class *)
     match recycled t.pool ~device:t.device length with
     | Some (buffer, capacity) ->
         slot.buffer <- Some buffer;
         slot.mapped <- true;
         slot.capacity <- capacity;
         Mem.charge slot_owner capacity;
         slot.uploaded <- None
     | None -> ());
  (* a slot that has asked for under a quarter of its size for this long comes back down *)
  if length > 0 && length * 4 < slot.capacity && slot.capacity > slot_shrink_floor then begin
    slot.low <- slot.low + 1;
    if slot.low >= slot_shrink_after then begin
      Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
      Mem.release slot_owner slot.capacity;
      slot.buffer <- None;
      slot.mapped <- false;
      slot.uploaded <- None;
      slot.capacity <- 0;
      slot.low <- 0
    end
  end
  else slot.low <- 0;
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
    if !capacity >= 1 lsl 20 then Mem.admit_bytes ~pool:Device !capacity;
    let fresh =
      if mapped then begin
        let buffer = get (B.create_mapped_buffer t.device descriptor) in
        created t.pool !capacity;
        buffer
      end
      else get (B.create_buffer t.device descriptor)
    in
    Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
    Mem.release slot_owner slot.capacity;
    Mem.charge slot_owner !capacity;
    slot.uploaded <- None;
    slot.buffer <- Some fresh;
    slot.capacity <- !capacity;
    slot.bytes <- (if mapped then Bytes.empty else Bytes.create !capacity);
    slot.mapped <- mapped;
    t.creations <- t.creations + 1
  end;
  Option.get slot.buffer

(* [wait] false leaves the dispatch in flight (no completion wait, no status read): a resident fold
   chains its regions and waits once.  [bound i] / [bound_table u] name a device buffer that already
   holds input [i] / the table of uniform [u] (and its length in floats), which is then not uploaded. *)
let kernel_scene t = t.kernel.Emit.scene

(* Encodes into [encoder] the threadgroup counts of a dispatch of at most [capacity] threads in
   groups of [group] whose count is the uint in [cell] x mul / div (Kernels.dispatch_arguments),
   into [groups]: its grid is sized on the device, nothing is read back. The caller sets its pipeline
   next. *)
let dispatch_arguments ?(bound = Bytes.create 16) encoder pipelines ~cell ~mul ~div ~capacity ~group
    ~groups =
  let compiled =
    match Pipelines.get pipelines Kernels.dispatch_arguments with
    | Ok compiled -> compiled
    | Error error -> raise (Diagnostic.Error error)
  in
  Bytes.set_int32_le bound 0 (Int32.of_int capacity);
  Bytes.set_int32_le bound 4 (Int32.of_int mul);
  Bytes.set_int32_le bound 8 (Int32.of_int div);
  Bytes.set_int32_le bound 12 (Int32.of_int group);
  get (B.set_pipeline encoder compiled.pipeline);
  get (B.set_buffer encoder ~index:0 cell);
  get (B.set_bytes encoder ~index:1 bound);
  get (B.set_buffer encoder ~index:2 groups);
  get (B.dispatch_threads encoder ~threads:(1, 1, 1) ~threadgroup:(1, 1, 1))

let dispatch ?(wait = true) ?(reset_status = true) ?(uint32_cpu = false) ?commands ?encoder ?count
    ?into ?(bound = fun _ -> None) ?(bound_table = fun _ -> None) ?scene ?(extras = no_extras)
    ?(resident = []) t (values : P.Host.inputs) =
  let explicit_input = bound and explicit_table = bound_table in
  (* a table another runner keeps on the device (its placeholder) is bound to its buffer *)
  let bound_table u =
    match bound_table u with
    | Some _ as b -> b
    | None
      when match Array.find_index (fun (index, _) -> index = u) t.kernel.gathers with
           | Some k -> t.kernel.gather_uint32.(k)
           | None -> false ->
        None
    | None when u < Array.length values.tables && Array.length values.tables.(u) <= 4 ->
        held values.tables.(u)
    | None -> None
  in
  (* a source another runner keeps on the device (its placeholder) is bound to its buffer *)
  let bound index =
    match bound index with
    | Some _ as b -> b
    | None when index >= 0 && index < Array.length t.kernel.input_uint32 && t.kernel.input_uint32.(index)
      ->
        None
    | None when index < Array.length values.arrays && Array.length values.arrays.(index) <= 4 ->
        held_input values.arrays.(index)
          ~length:(values.count * t.kernel.input_widths.(min index (Array.length t.inputs - 1)))
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
        @ List.filter_map (Option.map (fun a -> `Accel a)) [ extras.curves; extras.prototypes ]
  in
  let error message = Error (Diagnostic.error ~code:"E_KERNEL" message) in
  if t.closed || Domain.self () <> t.domain then
    error "GPU program owner is closed or called from another domain."
  else if
    values.count < 0
    || values.count > Int32.to_int Int32.max_int
    || values.count > Sys.max_string_length / 4 / Array.fold_left max 1 t.kernel.output_widths
  then error "GPU count exceeds native storage bounds."
  else if
    kernel_scene t <> None <> (scene <> None)
    ||
    match kernel_scene t with
    | None -> false
    | Some b ->
        List.length
          (List.filter (fun (x : Ogpu.Shader.binding) -> x.binding > b + 2) t.kernel.interface)
        > List.length scene_tail
  then error "GPU scene binding changed."
  else if Array.length values.arrays <> Array.length t.inputs then error "GPU input arity changed."
  else if
    values.count > 0
    &&
    let missing = ref false in
    Array.iteri
      (fun i typed ->
        if typed && explicit_input i = None && ((not uint32_cpu) || held values.arrays.(i) <> None)
        then missing := true)
      t.kernel.input_uint32;
    Array.iteri
      (fun k typed ->
        let u = fst t.kernel.gathers.(k) in
        if
          typed
          && explicit_table u = None
          && ((not uint32_cpu) || u >= Array.length values.tables || held values.tables.(u) <> None)
        then missing := true)
      t.kernel.gather_uint32;
    !missing
  then
    Error
      (Diagnostic.error ~code:"E_GPU_UINT32_BINDING"
         "Uint32 inputs require explicit device buffers.")
  else if
    let short = ref false in
    for index = 0 to Array.length t.inputs - 1 do
      if
        bound index = None
        && Array.length values.arrays.(index) < values.count * t.kernel.input_widths.(index)
      then short := true
    done;
    !short
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
          width = t.kernel.output_widths.(0);
          index = 0;
          gpu_seconds = None;
          generation = t.generation;
          epoch = t.last_epoch;
          check_status = false;
        }
    end
    else
      try
        let kernel = t.kernel in
        let compiled =
          match Pipelines.get t.pipelines kernel with
          | Ok compiled -> compiled
          | Error error -> raise (Diagnostic.Error error)
        in
        Bytes.set_int32_le t.uniforms 0 (Int32.of_int values.count);
        for e = 0 to Array.length t.layout - 1 do
          match t.layout.(e) with
          | Uniform_at (index, offset, width) ->
              if
                index >= Array.length values.uniforms
                || Array.length values.uniforms.(index) <> width
              then
                raise
                  (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU uniform type changed."));
              for component = 0 to width - 1 do
                let at = offset + (component * 4) in
                (if List.mem at kernel.uint32_uniforms then pack_uint32 else pack)
                  t.uniforms at
                  values.uniforms.(index).(component)
              done
          | Gather_at (k, offset) ->
              let u, width = kernel.gathers.(k) in
              if u >= Array.length values.tables then
                raise
                  (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU gather table changed."));
              let length =
                match bound_table u with
                | Some (_, length) -> length
                | None -> Array.length values.tables.(u)
              in
              Bytes.set_int32_le t.uniforms offset (Int32.of_int (length / width))
          | Strands_at offset ->
              Bytes.set_int32_le t.uniforms offset (Int32.of_int extras.strand_count)
          | Spheres_at offset ->
              Bytes.set_int32_le t.uniforms offset
                (Int32.of_int (match scene with Some s -> s.sphere_count | None -> 0))
          | Frame_at (index, offset) ->
              if index >= Array.length values.frame then
                raise
                  (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU frame uniform changed."));
              (if List.mem offset kernel.uint32_uniforms then pack_uint32 else pack)
                t.uniforms offset values.frame.(index)
        done;
        (* a buffer this runner's last dispatch still reads is not rewritten under it *)
        if commands = None && t.last_epoch > B.completed_epoch t.queue then
          get (B.complete_through t.queue t.last_epoch);
        let buffers =
          Array.mapi
            (fun index slot ->
              match bound index with
              | Some buffer ->
                  forget_input slot;
                  buffer
              | None ->
                  let length = values.count * kernel.input_widths.(index) * 4 in
                  let buffer = ensure ~mapped:(not kernel.input_uint32.(index)) t slot length in
                  let source = values.arrays.(index) in
                  if
                    not
                      (match slot.uploaded with
                      | Some (old, covered) -> old == source && covered = length
                      | None -> false)
                  then begin
                    (* A failed write may have partially replaced the previous source. *)
                    slot.uploaded <- None;
                    if kernel.input_uint32.(index) then upload_uint32 buffer slot source (length / 4)
                    else begin
                      pack_floats (view buffer) source (length / 4);
                      get (B.flush_mapped buffer ~length)
                    end;
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
                  ensure ~mapped:true t slot (values.count * kernel.output_widths.(k) * 4))
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
              Bytes.set_int32_le t.uniforms offset (Int32.of_int mul);
              Bytes.set_int32_le t.uniforms (offset + 4) (Int32.of_int div);
              Bytes.set_int32_le t.uniforms (offset + 8) (Int32.of_int use);
              (cell, out))
            kernel.counts
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
          if kernel.table_seeds = [||] then None
          else begin
            let length = Array.length kernel.table_seeds * 512 * 4 in
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
                kernel.table_seeds;
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
              match bound_table (fst kernel.gathers.(k)) with
              | Some (buffer, _) -> buffer
              | None ->
                  let source = values.tables.(fst kernel.gathers.(k)) in
                  let length = max 4 (Array.length source * 4) in
                  let buffer = ensure ~mapped:(not kernel.gather_uint32.(k)) t slot length in
                  if
                    not
                      (match slot.uploaded with
                      | Some (old, covered) -> old == source && covered = length
                      | None -> false)
                  then begin
                    slot.uploaded <- None;
                    if kernel.gather_uint32.(k) then
                      upload_uint32 buffer slot source (Array.length source)
                    else begin
                      pack_floats (view buffer) source (Array.length source);
                      get (B.flush_mapped buffer ~length:(Array.length source * 4))
                    end;
                    slot.uploaded <- Some (source, length);
                    t.input_uploads <- t.input_uploads + 1;
                    t.input_uploaded_bytes <- t.input_uploaded_bytes + length
                  end;
                  buffer)
            t.gathers
        in
        let queue = t.queue in
        let encode commands =
          let shared = encoder in
          let encoder =
            match shared with Some encoder -> encoder | None -> get (B.compute_encoder commands)
          in
          Fun.protect
            ~finally:(fun () -> if shared = None then ignore (B.end_compute encoder))
            (fun () ->
              (* a device count sizes the grid on the device: groups of 256 to it, not the capacity *)
              let groups =
                match (counted, count) with
                | Some (cell, _), Some (_, mul, div) ->
                    let groups = ensure t t.groups 12 in
                    dispatch_arguments ~bound:t.arguments encoder t.pipelines ~cell ~mul ~div
                      ~capacity:values.count ~group:256 ~groups;
                    Some groups
                | _ -> None
              in
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
                  let index = Option.get kernel.scene in
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
                       ((accel
                        :: (Option.to_list extras.curves @ Option.to_list extras.prototypes))
                       @ resident)))
                scene;
              get
                (match groups with
                | Some groups ->
                    B.dispatch_threadgroups_indirect encoder groups ~offset:0L
                      ~threadgroup:(256, 1, 1)
                | None ->
                    B.dispatch_threads encoder ~threads:(values.count, 1, 1) ~threadgroup:(256, 1, 1)))
        in
        match commands with
        | Some commands ->
            encode commands;
            Ok
              {
                owner = t;
                count = values.count;
                width = kernel.output_widths.(0);
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
                  width = kernel.output_widths.(0);
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
                  width = kernel.output_widths.(0);
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
  else Some { output with index; width = output.owner.kernel.output_widths.(index) }

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
      Ok (output.owner.outputs.(index), output.owner.kernel.output_widths.(index))
    with Diagnostic.Error error -> Error error

(* A completed, status-checked output's mapped contents, [count * width] floats. *)
let completed ?index output =
  Result.bind (ready ?index output) @@ fun (slot, width) ->
  try
    let length = output.count * width in
    output.owner.readback_bytes <- output.owner.readback_bytes + (length * 4);
    match slot.buffer with
    | Some buffer when slot.mapped && length > 0 ->
        get (B.invalidate_mapped buffer ~length:(length * 4));
        Ok (`Floats (view buffer, width))
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

let uint32_output ?index output =
  let index = Option.value ~default:output.index index in
  index >= 0
  && index < Array.length output.owner.kernel.output_uint32
  && output.owner.kernel.output_uint32.(index)

let read_uint32 queue buffer ~epoch ~length =
  try
    if epoch > B.completed_epoch queue then get (B.complete_through queue epoch);
    if length = 0 then Ok [||]
    else
      let bytes = get (B.read_buffer buffer ~offset:0L ~length:(length * 4)) in
      Ok
        (Array.init length (fun i ->
             Int32.to_int (Bytes.get_int32_le bytes (i * 4)) land 0xffffffff))
  with Diagnostic.Error d -> Error d

let uint32_values ?index output =
  Result.bind (ready ?index output) (fun (slot, _) ->
      try
        let values =
          if output.count = 0 then [||]
          else
            let values =
              match
                read_uint32 output.owner.queue (Option.get slot.buffer) ~epoch:output.epoch
                  ~length:output.count
              with
              | Ok values -> values
              | Error d -> raise (Diagnostic.Error d)
            in
            output.owner.readback_bytes <- output.owner.readback_bytes + (output.count * 4);
            values
        in
        Ok values
      with Diagnostic.Error d -> Error d)

let readback ?index output =
  if uint32_output ?index output then
    Result.map (fun values -> Flow.Eval.Int_array values) (uint32_values ?index output)
  else
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
  if uint32_output ?index output then
    Result.map (fun values -> [| Array.map float_of_int values |]) (uint32_values ?index output)
  else
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
  if Atomic.get kept.settled || kept.fate.destroyed then Ok ()
  else if Domain.self () <> t.domain then
    Error (Diagnostic.error ~code:"E_GPU" "A device output settles on its owner's domain.")
  else
    Result.map
      (fun () ->
        (* every output of the dispatch: its status covered them all *)
        List.iter
          (fun (epoch, settled, _) -> if epoch = kept.kept_epoch then Atomic.set settled true)
          t.kept;
        Atomic.set kept.settled true)
      (check_status t ~epoch:kept.kept_epoch)

let materialise t kept =
  match Atomic.get kept.fate.planes with
  | Some planes -> planes
  | None ->
      (* a reader on the initial domain waits for the kernel itself; any other reader runs after its
         cook settled (Sop.Session settles before every CPU node and at a cook's end) *)
      if not (Atomic.get kept.settled) then
        if Domain.self () = t.domain then
          (match settle t kept with Ok () -> () | Error d -> raise (Diagnostic.Error d))
        else invalid_arg "Run.materialise: a device output was read before its cook settled";
      if kept.fate.destroyed then invalid_arg "Run.materialise: a device output was read after its free";
      let length = kept.kept_count * kept.kept_width in
      if kept.uint32 then
        Mutex.protect kept.fate.lock (fun () ->
            match Atomic.get kept.fate.planes with
            | Some planes -> planes
            | None ->
                if kept.fate.destroyed then invalid_arg "Run.materialise: uint32 output was freed";
                if length > 0 then get (B.invalidate_mapped kept.kept_buffer ~length:(length * 4));
                let floats = view kept.kept_buffer in
                let planes = Array.init kept.kept_width (fun component ->
                    Array.init kept.kept_count (fun i ->
                        let index = i * kept.kept_width + component in
                        if kept.uint32 then
                          float_of_int (uint32_word floats index)
                        else Bigarray.Array1.unsafe_get floats index)) in
                ignore (Atomic.fetch_and_add t.materialised_bytes (length * 4));
                Atomic.incr t.materialised;
                Atomic.set kept.fate.planes (Some planes);
                release t.pool.released kept.id;
                planes)
      else begin
      (* Nothing here can fail, so no reader (an rdk accessor on any domain) ever sees an error:
         the buffer is mapped, the range is its own, it is destroyed only under this lock after
         [destroyed] is set (a view taken before stays valid), and the status check at [keep] covered
         the completion and every output's finiteness (the kernel flags a nonfinite output). *)
      let floats =
        Mutex.protect kept.fate.lock (fun () ->
            if length = 0 then Some (Bigarray.(Array1.create float32 c_layout 0))
            else if kept.fate.destroyed then None
            else
              match B.invalidate_mapped kept.kept_buffer ~length:(length * 4) with
              | Ok () ->
                  ignore (Atomic.fetch_and_add t.materialised_bytes (length * 4));
                  B.mapped_view kept.kept_buffer
              | Error e -> invalid_arg ("Run.materialise: " ^ Ogpu.Error.to_string e))
      in
      match floats with
      | None -> (
          (* destroyed after its read: the planes that read made *)
          match Atomic.get kept.fate.planes with
          | Some planes -> planes
          | None -> invalid_arg "Run.materialise: a device output was read after its free")
      | Some floats ->
      Atomic.incr kept.fate.readers;
      let planes =
        Fun.protect
          ~finally:(fun () -> Atomic.decr kept.fate.readers)
          (fun () ->
            match
              to_planes ~checked:true floats ~count:kept.kept_count ~width:kept.kept_width
            with
            | Ok planes -> planes
            | Error _ -> assert false)
      in
      if Atomic.compare_and_set kept.fate.planes None (Some planes) then begin
        Atomic.incr t.materialised;
        release t.pool.released kept.id
      end;
      Option.get (Atomic.get kept.fate.planes)
      end

(* A settled, unread vec3 output's bounds, reduced where it is (shaders/bounds.rays): one group of
   1,024 lanes, each lane's min and max (of every 1,024th element) read back (24 KB at most) and finished here. On the
   owner's domain; [None] elsewhere, once read (its planes are then the host's to scan) or on any
   device failure. Reduced once per output, into one buffer the runner keeps. *)
let bounds_of t kept =
  if
    kept.kept_width <> 3 || kept.kept_count = 0 || t.closed
    || Domain.self () <> t.domain
    || kept.fate.destroyed
    || Atomic.get kept.fate.planes <> None
    || not (Atomic.get kept.settled)
  then None
  else
    match kept.bounds_memo with
    | Some box -> box
    | None ->
        let n = kept.kept_count in
        let lanes = min n 1024 in
        let box =
          try
            let compiled =
              match Pipelines.get t.pipelines Kernels.bounds with
              | Ok compiled -> compiled
              | Error error -> raise (Diagnostic.Error error)
            in
            let out =
              match t.bounds_out with
              | Some out -> out
              | None ->
                  let out =
                    get
                      (B.create_buffer t.device
                         {
                           label = Some "Flow bounds";
                           size = Int64.of_int (1024 * 24);
                           usage = [ Storage; Copy_src ];
                         })
                  in
                  t.bounds_out <- Some out;
                  out
            in
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
                      get
                        (B.dispatch_threads encoder ~threads:(1024, 1, 1) ~threadgroup:(1024, 1, 1)));
                  let receipt = get (B.commit commands) in
                  committed := true;
                  receipt)
            in
            get (B.complete_through t.queue receipt.epoch);
            let lane, finite =
              unpack_array (get (B.read_buffer out ~offset:0L ~length:(lanes * 24))) (lanes * 6)
            in
            t.readback_bytes <- t.readback_bytes + (lanes * 24);
            t.bounds_readback_bytes <- t.bounds_readback_bytes + (lanes * 24);
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
            end
          with Diagnostic.Error _ -> None
        in
        kept.bounds_memo <- Some box;
        box

let next_kept = Atomic.make 0

(* Its owner (a session dropping the result) no longer needs it, from any domain. [held] (it was
   handed out, now or before): it is retired, still readable; its buffer comes back when it is
   collected or read, or when {!reclaim} reads it. Otherwise no one reads it again: its buffer comes
   back at once. *)
let release_kept t fate ~id ~weak ~held =
  if Atomic.get fate.planes = None then
    let retire =
      Mutex.protect fate.lock (fun () ->
          if fate.destroyed then false
          else if held || fate.retired then (
            fate.retired <- true;
            true)
          else (
            fate.destroyed <- true;
            Atomic.incr t.pool.freed;
            release t.pool.released id;
            false))
    in
    if retire then push t.pool.retiring (id, weak)

let hold t ~uint32 ~buffer ~capacity ~count ~width ~epoch ~settled =
  Mem.charge held_owner capacity;
  let id = Atomic.fetch_and_add next_kept 1 in
  let kept =
    { id;
      kept_buffer = buffer; kept_count = count; kept_width = width;
      uint32;
      fate =
        { lock = Mutex.create (); planes = Atomic.make None; destroyed = false; retired = false;
          readers = Atomic.make 0 };
      (* its contents are its id: a cache keying values by contents (Flow.Value.key_of) tells two
         held arrays apart *)
      placeholder = Array.make width (float id); kept_epoch = epoch;
      settled = Atomic.make settled; bounds_memo = None;
      thaw = ignore; pressure = device_pressure (if Sys.getenv_opt "NOPRESS" <> None then 0 else capacity) }
  in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some kept);
  t.kept <- (epoch, kept.settled, weak) :: t.kept;
  kept.thaw <- (fun () -> ignore (materialise t kept));
  Hashtbl.replace t.pool.held kept.id (buffer, capacity, kept.fate);
  let id = kept.id and released = t.pool.released and fate = kept.fate in
  Gc.finalise (fun _ -> release released id) kept;
  if not uint32 then bindable := (kept.placeholder, weak) :: !bindable;
  let resident : Flow.Eval.value =
    match width with
    | 1 -> Float_array kept.placeholder | 2 -> Vec2_array kept.placeholder
    | 3 -> Vec3_array kept.placeholder | _ -> Vec4_array kept.placeholder
  in
  ({ Flow_ir.Gpu.count; width;
     materialise = (fun () -> materialise t kept);
     resident = (if uint32 then None else Some resident);
     settle = (fun () -> settle t kept);
     release = (fun ~held -> release_kept t fate ~id ~weak ~held);
     bounds = (fun () -> bounds_of t kept) }, Some kept)

(* Hand the output's buffer to a lazy planar store: nothing is read or waited for now. *)
let keep_storage ~uint32 ?index output =
  let index = Option.value ~default:output.index index in
  let t = output.owner in
  if t.closed || Domain.self () <> t.domain || output.generation <> t.generation then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output was closed or superseded.")
  else if index < 0 || index >= Array.length t.outputs then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output index is outside its producer.")
  else if uint32_output ~index output <> uint32 then
    Error
      (Diagnostic.error ~code:"E_GPU_UINT32_KEEP"
         (if uint32 then "Expected a uint32 output for a typed topology holder."
          else "Uint32 outputs require a typed topology holder."))
  else
    let slot = t.outputs.(index) and width = t.kernel.output_widths.(index) in
    match slot.buffer with
    | Some buffer when slot.mapped && output.count > 0 && output.epoch > 0L ->
        let holder = hold t ~uint32 ~buffer ~capacity:slot.capacity
            ~count:output.count ~width ~epoch:output.epoch ~settled:false in
        Mem.release slot_owner slot.capacity;
        slot.buffer <- None;
        slot.mapped <- false;
        slot.capacity <- 0;
        slot.uploaded <- None;
        Ok holder
    | _ ->
        (* an empty, unmapped or caller-encoded output is read now *)
        Result.map
          (fun planes ->
            ({
              Flow_ir.Gpu.count = output.count;
              width;
              materialise = (fun () -> planes);
              resident = None;
              settle = (fun () -> Ok ());
              release = (fun ~held:_ -> ());
              bounds = (fun () -> None);
            }, None))
          (planes ~index output)

let keep ?index output = Result.map fst (keep_storage ~uint32:false ?index output)

(* The buffer, borrowed: [valid] until the unpin or its destruction. Closes over the fate only. *)
let borrow kept =
  let fate = kept.fate in
  Atomic.incr fate.readers;
  let pinned = Atomic.make true in
  ( kept.kept_buffer,
    kept.kept_count,
    (fun () -> Atomic.get pinned && not fate.destroyed),
    fun () -> if Atomic.exchange pinned false then Atomic.decr fate.readers )

let pin_kept kept =
  if kept.fate.destroyed || not (Atomic.get kept.settled) || Atomic.get kept.fate.planes <> None then None
  else Some (borrow kept)

type uint32_holder = {
  count : int;
  materialise : unit -> int array;
  settle : unit -> (unit, Diagnostic.t) result;
  release : held:bool -> unit;
  pin : unit -> (B.buffer * int * (unit -> bool) * (unit -> unit)) option;
}

type Flow_ir.Gpu.integer_storage += Native_uint32 of uint32_holder

let integers (holder : uint32_holder) : Flow_ir.Gpu.integers =
  { count = holder.count; storage = Some (Native_uint32 holder);
    materialise = holder.materialise; release = holder.release }

let pin_integers (holder : Flow_ir.Gpu.integers) =
  match holder.storage with
  | Some (Native_uint32 native) -> native.pin ()
  | Some _ | None -> None

let integer_holder ~domain ((planes : Flow_ir.Gpu.planes), kept) =
      let integers = Atomic.make None in
      { count = planes.count;
        materialise = (fun () ->
            match Atomic.get integers with
            | Some values -> values
            | None ->
                let values = Array.map int_of_float (planes.materialise ()).(0) in
                ignore (Atomic.compare_and_set integers None (Some values));
                Option.get (Atomic.get integers));
        settle = planes.settle;
        release = planes.release;
        pin = (fun () ->
            if Domain.self () <> domain then None else Option.bind kept pin_kept) }

let keep_uint32 ?index output =
  Result.map (integer_holder ~domain:output.owner.domain)
    (keep_storage ~uint32:true ?index output)

let adopt_storage t ~uint32 ~buffer ~count ~width =
  if t.closed || Domain.self () <> t.domain then
    Error (Diagnostic.error ~code:"E_GPU" "GPU owner is closed or called from another domain.")
  else if width < 1 || width > 4 || (uint32 && width <> 1) || count < 0
          || Int64.of_int count > Int64.div (B.buffer_size buffer) (Int64.of_int (4 * width)) then
    Error (Diagnostic.error ~code:"E_GPU" "Adopted array exceeds its buffer or has an invalid width.")
  else match B.release_mapped_view buffer with
  | Error _ -> Error (Diagnostic.error ~code:"E_GPU" "Adopted array requires mapped storage for worker reads.")
  | Ok () -> begin
    drain t;
    Ok (hold t ~uint32 ~buffer ~capacity:(Int64.to_int (B.buffer_size buffer))
          ~count ~width ~epoch:(B.completed_epoch t.queue) ~settled:true)
  end

let adopt_uint32 t ~buffer ~count =
  Result.map (integer_holder ~domain:t.domain)
    (adopt_storage t ~uint32:true ~buffer ~count ~width:1)

let adopt_float t ~buffer ~count ~width =
  Result.map fst (adopt_storage t ~uint32:false ~buffer ~count ~width)

(* A display borrowing a settled kept output's buffer: a reader the pool waits for (no reuse, no
   thaw) until [unpin]; [valid] while the buffer exists (a close destroys it). Initial domain. *)
let pin placeholder =
  bindable := List.filter (fun (_, w) -> Weak.check w 0) !bindable;
  List.find_map
    (fun (p, w) ->
      if p != placeholder then None
      else
        Option.bind (Weak.get w 0) (fun kept ->
            if kept.fate.destroyed || not (Atomic.get kept.settled) then None
            else Some (borrow kept)))
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
    || output.owner.kernel.counts = None
  then None
  else output.owner.counted.buffer

let count (output : output) = output.count
let epoch output = output.epoch
let width output = output.width
let gpu_seconds output = output.gpu_seconds

let max_total_threads t =
  Result.map
    (fun (compiled : Pipelines.compiled) -> B.max_total_threads compiled.pipeline)
    (Pipelines.get t.pipelines t.kernel)

(* on the initial domain, once its runners are closed *)
let close_pool pool =
  drain_pool pool;
  List.iter
    (fun w ->
      Option.iter
        (fun kept -> Mutex.protect kept.fate.lock (fun () -> kept.fate.destroyed <- true))
        (Weak.get w 0))
    pool.orphans;
  pool.orphans <- [];
  ignore (Atomic.exchange pool.released []);
  ignore (Atomic.exchange pool.retiring []);
  pool.retired <- [];
  Hashtbl.iter
    (fun _ (buffer, capacity, fate) ->
      (* a pin outlives a collected holder: its [valid] must see this *)
      Mutex.protect fate.lock (fun () -> fate.destroyed <- true);
      Mem.release held_owner capacity;
      ignore (B.destroy_buffer buffer))
    pool.held;
  Hashtbl.reset pool.held;
  List.iter
    (fun (buffer, capacity) ->
      Mem.release free_owner capacity;
      ignore (B.destroy_buffer buffer))
    pool.free;
  pool.free <- [];
  pool.free_ages <- []

let close ?(discard = false) t =
  if Domain.self () <> t.domain then invalid_arg "Run.close: creating domain required";
  if not t.closed then begin
    (* a holder still alive reads its planes now, while its buffer exists (a finalised holder's
       weak entry can outlive it: only those still holding count). On a shared pool its buffer
       outlives this runner: once settled it stays unread and the pool's, read only by a reader. *)
    drain t;
    List.iter
      (fun (_, _, weak) ->
        Option.iter
          (fun kept ->
            match Hashtbl.find_opt t.pool.held kept.id with
            | Some _ when (not discard) && (not t.own_pool) && settle t kept = Ok () ->
                t.pool.orphans <- weak :: t.pool.orphans
            | Some (buffer, capacity, _) ->
                (if not discard then try ignore (materialise t kept) with Diagnostic.Error _ -> ());
                Mutex.protect kept.fate.lock (fun () -> kept.fate.destroyed <- true);
                Mem.release held_owner capacity;
                Hashtbl.remove t.pool.held kept.id;
                ignore (B.destroy_buffer buffer)
            | None -> ())
          (Weak.get weak 0))
      t.kept;
    t.kept <- [];
    Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) t.bounds_out;
    t.bounds_out <- None;
    if t.own_pool then close_pool t.pool;
    Array.iter
      (fun slot ->
        Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
        Mem.release slot_owner slot.capacity;
        slot.buffer <- None;
        slot.bytes <- Bytes.empty;
        slot.mapped <- false;
        slot.capacity <- 0;
        slot.uploaded <- None)
      (Array.concat
         [ t.inputs; t.outputs; t.gathers; [| t.table; t.status; t.counted; t.groups |] ]);
    t.closed <- true
  end

module Private = struct
  let diagnostic = diagnostic
  let buffer_creations t = t.creations
  let input_uploads t = t.input_uploads
  let input_uploaded_bytes t = t.input_uploaded_bytes
  let status_reads t = t.status_reads
  let check_status t ~epoch = check_status t ~epoch
  let readback_bytes t = t.readback_bytes + Atomic.get t.materialised_bytes
  let bounds_readback_bytes t = t.bounds_readback_bytes
  let materialisations t = Atomic.get t.materialised

  let held_buffers t =
    drain t;
    Hashtbl.length t.pool.held

  let pool_freed pool = Atomic.get pool.freed
  let pool_free_floats pool = List.map (fun (buffer, _) -> view buffer) pool.free
  let pool_drain = drain_pool

  (* a free buffer of [bytes] put into the pool, as if a holder of that size had come back *)
  let pool_give pool device bytes =
    let descriptor : Ogpu.Types.buffer_descriptor =
      { label = Some "test free"; size = Int64.of_int bytes; usage = [ Storage; Copy_src; Copy_dst ] }
    in
    let buffer = get (B.create_mapped_buffer device descriptor) in
    Mem.charge free_owner bytes;
    pool.free_ages <- (buffer, pool.tick) :: pool.free_ages;
    pool.free <- (buffer, bytes) :: pool.free
  let slot_shrink_after = slot_shrink_after

  let pool_held pool =
    drain_pool pool;
    Hashtbl.length pool.held

  let create_owned = create_owned
end

(* The device [array/compact] of a resident fold: the kept indices stay in [kept], the count is read
   back (the one wait of a fold level). Three dispatches in order over blocks of 1,024: count each
   block, scan the counts in one group, scatter each block from its first slot. With [count_in] the
   first and last take their group count from it on the device, written into [groups] (12 bytes). *)
let fill_indices ?value queue pipelines buffer ~count =
  if count < 0 || Int64.of_int count > Int64.div (B.buffer_size buffer) 4L then
    Error (Diagnostic.error ~code:"E_GPU" "Integer fill count exceeds its buffer capacity.")
  else if count = 0 then Ok ()
  else
    try
      let compiled =
        match Pipelines.get pipelines Kernels.indices with
        | Ok p -> p
        | Error d -> raise (Diagnostic.Error d)
      in
      let commands = get (B.begin_commands queue) and committed = ref false in
      Fun.protect
        ~finally:(fun () -> if not !committed then ignore (B.abandon commands))
        (fun () ->
          let encoder = get (B.compute_encoder commands) in
          Fun.protect
            ~finally:(fun () -> ignore (B.end_compute encoder))
            (fun () ->
              get (B.set_pipeline encoder compiled.pipeline);
              get (B.set_buffer encoder ~index:0 buffer);
              let word = Bytes.create 12 in
              pack_uint32 word 0 (float count);
              pack_uint32 word 4 (if value = None then 0. else 1.);
              pack_uint32 word 8 (float (Option.value ~default:0 value));
              get (B.set_bytes encoder ~index:1 word);
              get (B.dispatch_threads encoder ~threads:(count, 1, 1) ~threadgroup:(256, 1, 1)));
          let receipt = get (B.commit commands) in
          committed := true;
          get (B.complete_through queue receipt.epoch));
      Ok ()
    with Diagnostic.Error d -> Error d

let compact_blocks count = 8 * max 1 ((count + 1023) / 1024)

let validate_topology ?(on_read = fun _ -> ()) queue pipelines ~vertices:(vertices, nv) ~offsets:(offsets, no)
    ~kinds:(kinds, nk) ~status ~point_count ~max_polygon_size =
  try
    List.iter
      (fun (buffer, count) ->
        if count < 0 || Int64.of_int count > Int64.div (B.buffer_size buffer) 4L then
          raise (Diagnostic.Error
                   (Diagnostic.error ~code:"E_TOPOLOGY" "Topology: array count exceeds buffer capacity")))
      [ (vertices, nv); (offsets, no); (kinds, nk); (status, 9) ];
    let bound = Bytes.create 20 in
    List.iteri (fun i n -> pack_uint32 bound (i * 4) (float n))
      [ point_count; nv; no; nk; max_polygon_size ];
    let compiled =
      match Pipelines.get pipelines Kernels.topology with
      | Ok p -> p
      | Error d -> raise (Diagnostic.Error d)
    in
    let commands = get (B.begin_commands queue) and committed = ref false in
    Fun.protect
      ~finally:(fun () -> if not !committed then ignore (B.abandon commands))
      (fun () ->
        let encoder = get (B.compute_encoder commands) in
        Fun.protect ~finally:(fun () -> ignore (B.end_compute encoder)) (fun () ->
            get (B.set_pipeline encoder compiled.pipeline);
            List.iteri (fun index buffer -> get (B.set_buffer encoder ~index buffer))
              [ vertices; offsets; kinds; status ];
            get (B.set_bytes encoder ~index:4 bound);
            get (B.dispatch_threads encoder ~threads:(256, 1, 1) ~threadgroup:(256, 1, 1)));
        let receipt = get (B.commit commands) in
        committed := true;
        get (B.complete_through queue receipt.epoch));
    let bytes = get (B.read_buffer status ~offset:0L ~length:36) in
    on_read 36;
    let failed i = Bytes.get_int32_le bytes (4 * i) <> 0l in
    let failure =
      List.find_opt (fun (bit, _, _) -> failed bit)
        [ (6, "E_TOPOLOGY_KIND", "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).");
          (0, "E_TOPOLOGY", "Topology: primitive offsets must contain the initial zero");
          (1, "E_TOPOLOGY", "Topology: first primitive offset must be zero");
          (2, "E_TOPOLOGY", "Topology: final primitive offset must equal vertex count");
          (3, "E_TOPOLOGY", "Topology: primitive kind count must match primitive count");
          (4, "E_TOPOLOGY", "Topology: vertex references an invalid point");
          (5, "E_TOPOLOGY", "Topology: primitive offsets are not ordered");
          (7, "E_TOPOLOGY", "Topology: primitive has too few vertices for its kind");
          (8, "E_TOPOLOGY", "Topology: polygon exceeds the supported vertex count") ]
    in
    match failure with
    | None -> Ok ()
    | Some (_, code, message) -> Error (Diagnostic.error ~code message)
  with Diagnostic.Error d -> Error d

let triangle_indices ?(on_read = fun _ -> ()) queue pipelines ~positions:(positions, points)
    ~vertices:(vertices, nv) ~offsets:(offsets, no) ~kinds:(kinds, np) ~indices ~blocks ~status =
  try
    let refuse message = raise (Diagnostic.Error (Diagnostic.error ~code:"E_GPU_DISPLAY_TRIANGLES" message)) in
    if points <= 0 || points > (1 lsl 30) || np <= 0 || np > (1 lsl 28) || no <> np + 1
      || nv < 3 * np || nv > 4 * np || nv > Sys.max_string_length / 12
    then refuse "Resident display requires nonempty triangle/quad topology.";
    let count = 3 * (nv - (2 * np)) in
    let groups = (np + 255) / 256 in
    List.iter (fun (buffer, words) ->
      if words < 0 || Int64.of_int words > Int64.div (B.buffer_size buffer) 4L then
        refuse "Resident display stream count exceeds buffer capacity.")
      [ positions, points * 3; vertices, nv; offsets, no; kinds, np;
        indices, count; blocks, groups; status, 1 ];
    let compiled = match Pipelines.get pipelines Kernels.triangles with
      | Ok p -> p | Error d -> raise (Diagnostic.Error d) in
    let bound = Bytes.create 16 in
    List.iteri (fun i n -> Bytes.set_int32_le bound (i * 4) (Int32.of_int n)) [ points; nv; np ];
    let commands = get (B.begin_commands queue) and committed = ref false in
    Fun.protect ~finally:(fun () -> if not !committed then ignore (B.abandon commands)) (fun () ->
      for pass = 0 to 1 do
        Bytes.set_int32_le bound 12 (Int32.of_int pass);
        let encoder = get (B.compute_encoder commands) in
        Fun.protect ~finally:(fun () -> ignore (B.end_compute encoder)) (fun () ->
          get (B.set_pipeline encoder compiled.pipeline);
          List.iteri (fun index buffer -> get (B.set_buffer encoder ~index buffer))
            [ positions; vertices; offsets; kinds; indices; blocks; status ];
          get (B.set_bytes encoder ~index:7 bound);
          get (B.dispatch_threads encoder ~threads:((if pass = 0 then groups * 256 else 256), 1, 1)
            ~threadgroup:(256, 1, 1)))
      done;
      let receipt = get (B.commit commands) in
      committed := true;
      get (B.complete_through queue receipt.epoch));
    let result = get (B.read_buffer status ~offset:0L ~length:4) in
    on_read 4;
    if Bytes.get_int32_le result 0 <> 0l then
      refuse "Resident display polygon is unsupported, degenerate or numerically ambiguous.";
    Ok count
  with Diagnostic.Error d -> Error d

let compact ?(uint32 = false) ?encoder commands pipelines ~mask ~count ~count_in ~kept ~total
    ~blocks ~groups =
  try
    let compiled =
      match
        Pipelines.get pipelines (if uint32 then Kernels.compaction_uint32 else Kernels.compaction)
      with
      | Ok compiled -> compiled
      | Error error -> raise (Diagnostic.Error error)
    in
    let shared = encoder in
    let encoder =
      match shared with Some encoder -> encoder | None -> get (B.compute_encoder commands)
    in
    Fun.protect
      ~finally:(fun () -> if shared = None then ignore (B.end_compute encoder))
      (fun () ->
        Option.iter
          (fun cell ->
            dispatch_arguments encoder pipelines ~cell ~mul:1 ~div:1 ~capacity:count ~group:1024
              ~groups)
          count_in;
        get (B.set_pipeline encoder compiled.pipeline);
        get (B.set_buffer encoder ~index:0 mask);
        get (B.set_buffer encoder ~index:1 kept);
        get (B.set_buffer encoder ~index:2 total);
        get (B.set_buffer encoder ~index:4 (Option.value ~default:total count_in));
        get (B.set_buffer encoder ~index:5 blocks);
        List.iter
          (fun pass ->
            let bound = Bytes.create 12 in
            Bytes.set_int32_le bound 0 (Int32.of_int count);
            Bytes.set_int32_le bound 4 (if count_in = None then 0l else 1l);
            Bytes.set_int32_le bound 8 (Int32.of_int pass);
            get (B.set_bytes encoder ~index:3 bound);
            get
              (if pass <> 1 && count_in <> None then
                 B.dispatch_threadgroups_indirect encoder groups ~offset:0L ~threadgroup:(1024, 1, 1)
               else
                 let groups = if pass = 1 then 1 else compact_blocks count / 8 in
                 B.dispatch_threads encoder ~threads:(1024 * groups, 1, 1) ~threadgroup:(1024, 1, 1)))
          [ 0; 1; 2 ]);
    Ok ()
  with Diagnostic.Error error -> Error error

let copy ~encoder pipelines ~src ~from ~dst ~into ~words =
  try
    if words > 0 then begin
      let compiled =
        match Pipelines.get pipelines Kernels.copy with
        | Ok compiled -> compiled
        | Error error -> raise (Diagnostic.Error error)
      in
      let bound = Bytes.create 12 in
      Bytes.set_int32_le bound 0 (Int32.of_int words);
      Bytes.set_int32_le bound 4 (Int32.of_int from);
      Bytes.set_int32_le bound 8 (Int32.of_int into);
      get (B.set_pipeline encoder compiled.pipeline);
      get (B.set_buffer encoder ~index:0 src);
      get (B.set_buffer encoder ~index:1 dst);
      get (B.set_bytes encoder ~index:2 bound);
      get (B.dispatch_threads encoder ~threads:(words, 1, 1) ~threadgroup:(256, 1, 1))
    end;
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
        let prototype_accel =
          if motion && Ogpu.Impl.backend = "vulkan" then begin
            let identity =
              [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0. |]
            in
            let instances =
              Array.mapi
                (fun structure_index _ ->
                  {
                    B.instance =
                      { transform = identity; mask = 0xFFFF_FFFF; structure_index };
                    user_id = structure_index;
                  })
                (Array.of_list prototypes)
            in
            let instance_buffer =
              upload (get (B.pack_instance_records device instances))
            in
            Some
              (accel
                 (B.Tlas_of
                    {
                      instances = instance_buffer;
                      offset = 0L;
                      instance_count = Array.length instances;
                      kind = B.User_id_instances;
                      structures = blas;
                      motion_transforms = None;
                    }))
          end
          else None
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
          extras = { no_extras with prototypes = prototype_accel };
          instanced = true;
          motion;
          curves = None;
          release;
        }
      end
    in
    (* Native curves use their curve structure; other devices use per-segment swept AABBs and
       resolve capsule candidates against the same strand records. *)
    let segments = Bytes.length d.strand_data / 32 in
    if segments = 0 then Ok built
    else begin
      let strands = upload d.strand_data in
      let native = Ogpu.Caps.has caps Ray_tracing_curves in
      let curves =
        if native then
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
        else begin
          let boxes = Bytes.create (segments * 24) in
          let value at = Int32.float_of_bits (Bytes.get_int32_le d.strand_data at) in
          for segment = 0 to segments - 1 do
            let record = segment * 32 in
            let radius = max (Float.abs (value (record + 12))) (Float.abs (value (record + 28))) in
            for axis = 0 to 2 do
              let a = value (record + axis * 4)
              and b = value (record + 16 + axis * 4) in
              let low = min a b -. radius and high = max a b +. radius in
              let epsilon = 1e-5 *. max 1. (max (Float.abs low) (Float.abs high)) in
              Bytes.set_int32_le boxes (segment * 24 + axis * 4)
                (Int32.bits_of_float (low -. epsilon));
              Bytes.set_int32_le boxes (segment * 24 + 12 + axis * 4)
                (Int32.bits_of_float (high +. epsilon))
            done
          done;
          let boxes = upload boxes in
          Some
            (accel
               (B.Blas
                  {
                    geometries =
                      [
                        B.Bounding_boxes
                          {
                            boxes = [ { buffer = boxes; offset = 0L } ];
                            stride = 24;
                            count = segments;
                            options = B.default_geometry_options;
                          };
                      ];
                  }))
        end
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
