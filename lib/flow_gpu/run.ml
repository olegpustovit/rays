module B = Ogpu.Backend
module P = Flow_ir.Packed
type slot = {mutable buffer : B.buffer option; mutable capacity : int; mutable bytes : bytes;
  mutable uploaded:(float array * int) option}
type t = {device:B.device; queue:B.queue; pipelines : Pipelines.t; msl : Emit.msl; inputs : slot array;
  outputs : slot array; table : slot; gathers : slot array; status:slot; counted:slot; uniforms : bytes; mutable closed : bool; mutable creations : int;
  mutable input_uploads:int; mutable input_uploaded_bytes:int;
  mutable status_reads:int; mutable readback_bytes:int;
  mutable status_checked:bool;
  mutable generation : int; mutable table_ready:bool; domain : Domain.id;
  mutable last_epoch : int64}
type scene = {accel:B.accel; vertices:B.buffer; spheres:B.buffer; sphere_count:int; instances:B.buffer option}
type output = {owner : t; count : int; width : int; index : int; gpu_seconds : float option; generation : int; epoch : int64; check_status:bool}
let diagnostic error = Diagnostic.error ~code:"E_GPU" (Ogpu.Error.to_string error)
let get = function Ok value -> value | Error error -> raise (Diagnostic.Error (diagnostic error))
let slot () = {buffer=None;capacity=0;bytes=Bytes.empty;uploaded=None}
let nonfinite () =
  Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU inputs must remain finite when represented as float32.")
let kernel_status flags =
  (* The status word is an aggregate across parallel rows, so diagnostics have a stable
     class priority rather than claiming CPU row-order precedence. *)
  if Int32.logand flags 4l <> 0l then
    Diagnostic.error ~code:"E_RANGE" "Integer shift count must be within [0, 62]."
  else if Int32.logand flags 2l <> 0l then
    Diagnostic.error ~code:"E_RANGE" "Integer division by zero."
  else Diagnostic.error ~code:"E_KERNEL" "GPU output contains a nonfinite value."
let pack bytes offset value =
  let bits=Int32.bits_of_float value in
  if not(Float.is_finite(Int32.float_of_bits bits))then raise(nonfinite());
  Bytes.set_int32_le bytes offset bits
(* One loop body keeps the float and its int32 bits unboxed; calling [pack]
   per element boxed both (96 MB/frame at one million vec3 inputs). *)
let pack_array bytes (values : float array) length =
  for index=0 to length-1 do
    let bits=Int32.bits_of_float(Array.unsafe_get values index) in
    if not(Float.is_finite(Int32.float_of_bits bits))then raise(nonfinite());
    Bytes.set_int32_le bytes (index*4) bits
  done
let unpack_array bytes length =
  let values=Array.make length 0. and finite=ref true in
  for index=0 to length-1 do
    let value=Int32.float_of_bits(Bytes.get_int32_le bytes (index*4)) in
    if not(Float.is_finite value) then finite:=false;
    Array.unsafe_set values index value
  done;
  values, !finite
let create_owned device queue pipelines (msl : Emit.msl) =
  if not(Domain.is_main_domain())then invalid_arg "Run.create: initial domain required";
  {device;queue;pipelines;msl;inputs=Array.map (fun _ -> slot ()) msl.input_widths;
    outputs=Array.map (fun _ -> slot ()) msl.output_widths;table=slot ();
    gathers=Array.map (fun _ -> slot ()) msl.gathers;status=slot();counted=slot();uniforms=Bytes.create msl.uniform_bytes;
    closed=false;creations=0;input_uploads=0;input_uploaded_bytes=0;
    status_reads=0;readback_bytes=0;status_checked=false;generation=0;table_ready=false;domain=Domain.self ();last_epoch=0L}
let create gpu=create_owned(Renderer.gpu_device gpu)(Renderer.gpu_queue gpu)
let ensure t slot length =
  if length>slot.capacity then begin
    let capacity = ref (max 256 slot.capacity) in
    while !capacity<length do
      if !capacity>Sys.max_string_length/2 then capacity:=length else capacity:= !capacity*2
    done;
    let bytes=Bytes.create !capacity in
    let fresh = get (B.create_buffer t.device
      {label=Some "Flow packed compute";size=Int64.of_int !capacity;usage=[Storage;Copy_src;Copy_dst]}) in
    Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
    slot.uploaded<-None;
    slot.buffer<-Some fresh;slot.capacity<- !capacity;slot.bytes<-bytes;
    t.creations<-t.creations+1
  end;
  Option.get slot.buffer
(* [wait] false leaves the dispatch in flight (no completion wait, no status read): a resident fold
   chains its regions and waits once.  [bound i] / [bound_table u] name a device buffer that already
   holds input [i] / the table of uniform [u], which is then not uploaded. *)
let msl_scene t = t.msl.Emit.scene
let dispatch ?(wait=true) ?(reset_status=true) ?commands ?count ?into ?(bound=fun _ -> None) ?(bound_table=fun _ -> None) ?scene ?(resident=[]) t (values : P.Private.inputs) =
  let error message = Error (Diagnostic.error ~code:"E_KERNEL" message) in
  if t.closed || Domain.self ()<>t.domain then error "GPU program owner is closed or called from another domain."
  else if values.count<0 || values.count>Int32.to_int Int32.max_int ||
    values.count>Sys.max_string_length/4/Array.fold_left max 1 t.msl.output_widths then error "GPU count exceeds native storage bounds."
  else if (msl_scene t<>None) <> (scene<>None) then error "GPU scene binding changed."
  else if Array.length values.arrays<>Array.length t.inputs then error "GPU input arity changed."
  else if Array.exists (fun index -> Array.length values.arrays.(index)<values.count*t.msl.input_widths.(index))
      (Array.init (Array.length t.inputs) Fun.id) then error "GPU input count changed."
  else begin
  (* Invalidate borrowed output before reallocating or submitting any write,
     including dispatches whose completion subsequently fails. *)
  t.generation<-t.generation+1;
  if values.count=0 then begin
    Ok {owner=t;count=0;width=t.msl.output_widths.(0);index=0;gpu_seconds=None;generation=t.generation;epoch=t.last_epoch;check_status=false}
  end
  else try
    let msl=t.msl in
    let compiled=match Pipelines.get t.pipelines msl with
      | Ok compiled -> compiled | Error error -> raise (Diagnostic.Error error) in
    Bytes.set_int32_le t.uniforms 0 (Int32.of_int values.count);
    List.iter (fun (name,offset,width) ->
      if String.starts_with ~prefix:"uniform:" name then begin
        let index=int_of_string (String.sub name 8 (String.length name-8)) in
        if index>=Array.length values.uniforms || Array.length values.uniforms.(index)<>width then
          raise (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU uniform type changed."));
        for component=0 to width-1 do pack t.uniforms (offset+component*4)
          values.uniforms.(index).(component) done
      end else match String.split_on_char ':' name with
        | ["gather";k;_] ->
            let u, width = msl.gathers.(int_of_string k) in
            if u>=Array.length values.tables then
              raise (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU gather table changed."));
            Bytes.set_int32_le t.uniforms offset (Int32.of_int (Array.length values.tables.(u)/width))
        | ["scene";"spheres"] ->
            Bytes.set_int32_le t.uniforms offset (Int32.of_int (match scene with Some s -> s.sphere_count | None -> 0))
        | ["frame";index;_] ->
            let index=int_of_string index in
            if index>=Array.length values.frame then
              raise (Diagnostic.Error (Diagnostic.error ~code:"E_KERNEL" "GPU frame uniform changed."));
            pack t.uniforms offset values.frame.(index)
        | _ -> assert false) msl.uniform_layout;
    (* a buffer this runner's last dispatch still reads is not rewritten under it *)
    if commands=None && t.last_epoch>B.completed_epoch t.queue then get (B.complete_through t.queue t.last_epoch);
    let buffers=Array.mapi (fun index slot -> match bound index with Some buffer -> buffer | None ->
      let length=values.count*msl.input_widths.(index)*4 in
      let buffer=ensure t slot length in
      let source=values.arrays.(index) in
      if not (match slot.uploaded with Some (old,covered)->old==source && covered=length | None->false) then begin
        (* A failed write may have partially replaced the previous source. *)
        slot.uploaded<-None;
        pack_array slot.bytes source (length/4);
        get (B.write_buffer buffer ~offset:0L slot.bytes);
        slot.uploaded<-Some(source,length);
        t.input_uploads<-t.input_uploads+1;
        t.input_uploaded_bytes<-t.input_uploaded_bytes+Bytes.length slot.bytes
      end;
      buffer) t.inputs in
    let outputs=match into with
      | Some (output,_) when Array.length t.outputs=1 -> [|output|]
      | _ -> Array.mapi (fun k slot -> ensure t slot (values.count*msl.output_widths.(k)*4)) t.outputs in
    let status=ensure t t.status 4 in
    (* the device count words and cells of a fast kernel; without a [count] its own cell stands in *)
    let counted=Option.map (fun offset ->
      let out=match into with Some (_,cell) -> cell | None -> ensure t t.counted 24 in
      let cell,mul,div,use=match count with Some (cell,mul,div) -> cell,mul,div,1 | None -> out,1,1,0 in
      List.iteri (fun k v -> Bytes.set_int32_le t.uniforms (offset+4*k) (Int32.of_int v)) [mul;div;use];
      cell,out) msl.counts in
    if reset_status then begin
      Bytes.set_int32_le t.status.bytes 0 0l;
      get(B.write_buffer status ~offset:0L t.status.bytes);
      t.status_checked<-false
    end;
    let table = if msl.table_seeds=[||] then None else begin
      let length=Array.length msl.table_seeds*512*4 in
      let fresh=not t.table_ready in
      let buffer=ensure t t.table length in
      if fresh then begin
        Array.iteri (fun index seed ->
          let permutation=Rays_math.Noise.Private.permutation (Rays_math.Noise.create seed) in
          Array.iteri (fun entry value -> Bytes.set_int32_le t.table.bytes ((index*512+entry)*4)
            (Int32.of_int value)) permutation) msl.table_seeds;
        get (B.write_buffer buffer ~offset:0L t.table.bytes);
        t.table_ready<-true
      end;Some buffer
    end in
    (* the tables gathers read, uploaded when their array changed *)
    let gathers=Array.mapi (fun k slot -> match bound_table (fst msl.gathers.(k)) with Some buffer -> buffer | None ->
      let source=values.tables.(fst msl.gathers.(k)) in
      let length=max 4 (Array.length source*4) in
      let buffer=ensure t slot length in
      if not (match slot.uploaded with Some (old,covered)->old==source && covered=length | None->false) then begin
        slot.uploaded<-None;
        pack_array slot.bytes source (Array.length source);
        get (B.write_buffer buffer ~offset:0L slot.bytes);
        slot.uploaded<-Some(source,length);
        t.input_uploads<-t.input_uploads+1;
        t.input_uploaded_bytes<-t.input_uploaded_bytes+Bytes.length slot.bytes
      end;
      buffer) t.gathers in
    let queue=t.queue in
    let encode commands=
      let encoder=get (B.compute_encoder commands) in
      Fun.protect ~finally:(fun () -> ignore (B.end_compute encoder)) (fun () ->
        get (B.set_pipeline encoder compiled.pipeline);
        Array.iteri (fun index buffer -> get (B.set_buffer encoder ~index buffer)) buffers;
        let index=Array.length buffers in
        Array.iteri (fun k output -> get (B.set_buffer encoder ~index:(index+k) output)) outputs;
        let index=index+Array.length outputs in
        get (B.set_bytes encoder ~index t.uniforms);
        get(B.set_buffer encoder ~index:(index+1) status);
        Option.iter (fun table -> get (B.set_buffer encoder ~index:(index+2) table)) table;
        let index=index+2+(if table=None then 0 else 1) in
        Array.iteri (fun k buffer -> get (B.set_buffer encoder ~index:(index+k) buffer)) gathers;
        let index=index+Array.length gathers in
        Option.iter (fun (cell,out) -> get (B.set_buffer encoder ~index cell);
          get (B.set_buffer encoder ~index:(index+1) out)) counted;
        Option.iter (fun {accel;vertices;spheres;instances;_} ->
          let index=Option.get msl.scene in
          get (B.set_accel encoder ~index accel);
          get (B.set_buffer encoder ~index:(index+1) vertices);
          get (B.set_buffer encoder ~index:(index+2) spheres);
          Option.iter (fun table -> get (B.set_buffer encoder ~index:(index+3) table)) instances;
          get (B.compute_use_accels encoder (accel :: resident))) scene;
        get (B.dispatch_threads encoder ~threads:(values.count,1,1) ~threadgroup:(256,1,1))) in
    match commands with
    | Some commands -> encode commands;
        Ok {owner=t;count=values.count;width=msl.output_widths.(0);index=0;gpu_seconds=None;
          generation=t.generation;epoch=0L;check_status=true}
    | None ->
    let commands=get (B.begin_commands queue) in
    let committed=ref false in
    let receipt=Fun.protect ~finally:(fun () -> if not !committed then ignore (B.abandon commands)) (fun () ->
      encode commands;
      let receipt=get (B.commit commands) in committed:=true;receipt) in
    t.last_epoch<-receipt.epoch;
    if not wait then Ok {owner=t;count=values.count;width=msl.output_widths.(0);index=0;gpu_seconds=None;
      generation=t.generation;epoch=receipt.epoch;check_status=true} else begin
    get (B.complete_through queue receipt.epoch);
    let flags=get(B.read_buffer status ~offset:0L ~length:4)in
    t.status_reads<-t.status_reads+1;
    let flags=Bytes.get_int32_le flags 0 in
    if flags<>0l then raise(Diagnostic.Error(kernel_status flags));
    t.status_checked<-true;
    let output={owner=t;count=values.count;width=msl.output_widths.(0);index=0;gpu_seconds=B.gpu_duration queue receipt;
      generation=t.generation;epoch=receipt.epoch;check_status=true}in
    Ok output end
  with Diagnostic.Error error -> Error error
    | Out_of_memory -> error "GPU staging exceeds available memory."
  end
let select_output ~index output =
  if index<0 || index>=Array.length output.owner.outputs then None
  else Some {output with index; width=output.owner.msl.output_widths.(index)}
let check_status t ~epoch =
  try
    if epoch>B.completed_epoch t.queue then get(B.complete_through t.queue epoch);
    if not t.status_checked then begin
      let flags=get(B.read_buffer (Option.get t.status.buffer) ~offset:0L ~length:4)in
      t.status_reads<-t.status_reads+1;
      let flags=Bytes.get_int32_le flags 0 in
      if flags<>0l then raise(Diagnostic.Error(kernel_status flags));
      t.status_checked<-true
    end;
    Ok ()
  with Diagnostic.Error error -> Error error
let readback ?index output =
  let index=Option.value ~default:output.index index in
  if output.owner.closed || Domain.self ()<>output.owner.domain || output.generation<>output.owner.generation then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output was closed or superseded.")
  else if index<0 || index>=Array.length output.owner.outputs then
    Error (Diagnostic.error ~code:"E_GPU" "GPU output index is outside its producer.")
  else try
    (* a [~wait:false] dispatch is read once its queue has completed it *)
    if output.epoch>B.completed_epoch output.owner.queue then get (B.complete_through output.owner.queue output.epoch);
    if output.check_status then
      (match check_status output.owner ~epoch:output.epoch with
       |Ok()->()|Error diagnostic->raise(Diagnostic.Error diagnostic));
    let width=output.owner.msl.output_widths.(index) in
    let length=output.count*width in
    let values,finite=if length=0 then [||],true else begin
      let bytes=get(B.read_buffer (Option.get output.owner.outputs.(index).buffer) ~offset:0L ~length:(length*4))in
      output.owner.readback_bytes<-output.owner.readback_bytes+length*4;
      unpack_array bytes length
    end in
    if not finite then
      Error (Diagnostic.error ~code:"E_KERNEL" "GPU output contains a nonfinite value.")
    else Ok (match width with 2 -> Flow.Eval.Vec2_array values
      | 3 -> Vec3_array values | 4 -> Vec4_array values | _ -> Float_array values)
  with Diagnostic.Error error -> Error error
let buffer ?index output =
  let index=Option.value ~default:output.index index in
  if index<0 || index>=Array.length output.owner.outputs || output.owner.closed || Domain.self()<>output.owner.domain || output.generation<>output.owner.generation
  then None else output.owner.outputs.(index).buffer
(* the count record of a fast kernel's dispatch, while it is the runner's current output *)
let count_record output = if output.owner.closed || Domain.self()<>output.owner.domain
  || output.generation<>output.owner.generation || output.owner.msl.counts=None then None
  else output.owner.counted.buffer
let count output = output.count
let epoch output = output.epoch
let width output = output.width
let gpu_seconds output = output.gpu_seconds
let max_total_threads t =
  Result.map (fun (compiled : Pipelines.compiled) -> B.max_total_threads compiled.pipeline) (Pipelines.get t.pipelines t.msl)
let close t =
  if Domain.self()<>t.domain then invalid_arg "Run.close: creating domain required";
  if not t.closed then begin
  Array.iter (fun slot -> Option.iter (fun buffer -> ignore (B.destroy_buffer buffer)) slot.buffer;
    slot.buffer<-None;slot.bytes<-Bytes.empty;slot.capacity<-0;slot.uploaded<-None)
    (Array.concat [t.inputs;t.outputs;t.gathers;[|t.table;t.status;t.counted|]]);
  t.closed<-true
end
module Private = struct
  let diagnostic = diagnostic
  let buffer_creations t = t.creations
  let input_uploads t = t.input_uploads
  let input_uploaded_bytes t = t.input_uploaded_bytes
  let status_reads t = t.status_reads
  let check_status t ~epoch = check_status t ~epoch
  let readback_bytes t = t.readback_bytes
  let create_owned = create_owned
end

(* The device [array/compact] of a resident fold: the kept indices stay in [kept], the count is read
   back (the one wait of a fold level). *)
let compact commands pipelines ~mask ~count ~count_in ~kept ~total =
  try
    let compiled = match Pipelines.get pipelines Emit.compaction with
      | Ok compiled -> compiled | Error error -> raise (Diagnostic.Error error) in
    let bound = Bytes.create 8 in
    Bytes.set_int32_le bound 0 (Int32.of_int count); Bytes.set_int32_le bound 4 (if count_in = None then 0l else 1l);
    let encoder = get (B.compute_encoder commands) in
    Fun.protect ~finally:(fun () -> ignore (B.end_compute encoder)) (fun () ->
      get (B.set_pipeline encoder compiled.pipeline);
      get (B.set_buffer encoder ~index:0 mask); get (B.set_buffer encoder ~index:1 kept);
      get (B.set_buffer encoder ~index:2 total); get (B.set_bytes encoder ~index:3 bound);
      get (B.set_buffer encoder ~index:4 (Option.value ~default:total count_in));
      get (B.dispatch_threads encoder ~threads:(1024,1,1) ~threadgroup:(1024,1,1)));
    Ok ()
  with Diagnostic.Error error -> Error error
(* a count cell, once the queue has completed [epoch] *)
let read_count queue buffer ~epoch =
  try
    if epoch > B.completed_epoch queue then get (B.complete_through queue epoch);
    Ok (Int32.to_int (Bytes.get_int32_le (get (B.read_buffer buffer ~offset:0L ~length:4)) 0) land 0xffffffff)
  with Diagnostic.Error error -> Error error
(* read a device buffer the queue has finished writing *)
let read_floats queue buffer ~epoch ~length =
  try
    if epoch > B.completed_epoch queue then get (B.complete_through queue epoch);
    let values, _ = if length = 0 then [||], true
      else unpack_array (get (B.read_buffer buffer ~offset:0L ~length:(length*4))) length in
    Ok values
  with Diagnostic.Error error -> Error error
