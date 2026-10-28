let get = function
  | Ok value -> value
  | Error error -> failwith (Ogpu.Error.to_string error)

let require condition message = if not condition then failwith message

let run driver =
  let open Ogpu in
  let device = get (Backend.create_device driver) in
  let capabilities = Backend.capabilities device in
  get (Caps.validate capabilities);
  let profile = capabilities in
  require (Caps.has profile Caps.Ray_tracing = capabilities.ray_tracing)
    "ray-tracing capability mismatch";
  (match Caps.require profile (Caps.Unknown "future") with
   | Error { Error.kind = Unsupported; _ } -> ()
   | _ -> failwith "unknown feature was not typed Unsupported");
  if not (Caps.has profile Caps.Compute_pipeline) then
    (match Caps.require profile Caps.Compute_pipeline with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unavailable compute was not typed Unsupported");
  let descriptor : Types.buffer_descriptor =
    { label = Some "conformance"; size = 16L; usage = [Copy_src; Copy_dst] } in
  let source = get (Backend.create_buffer device descriptor) in
  let queue = get (Backend.create_queue device) in
  require (Backend.completed_epoch queue = 0L)
    "new queue completed an epoch";
  (match Backend.poll_through queue 0L with
   | Error { Error.kind = Invalid_argument; _ } -> ()
   | _ -> failwith "zero completion epoch was accepted");
  let bytes = Bytes.init 16 (fun i -> Char.chr (i * 13 land 255)) in
  get (Backend.write_buffer source ~offset:0L bytes);
  require (get (Backend.read_buffer source ~offset:0L ~length:16) = bytes)
    "buffer round trip differs";
  (* Blit encoder: texture upload with padded rows, read back by the host,
     and the same at an inset origin. *)
  let rec poll receipt attempts =
    match get (Backend.status queue receipt) with
    | Backend.Completed -> ()
    | Backend.Pending when attempts = 0 ->
        failwith "submitted epoch did not complete"
    | Backend.Pending -> Unix.sleepf 0.001; poll receipt (attempts - 1) in
  let blit encode =
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.blit_encoder commands) in
    encode encoder;
    get (Backend.end_blit encoder);
    let receipt=get (Backend.commit commands) in
    poll receipt 1000; receipt in
  let upload=get (Backend.create_buffer device
    {label=Some "texture-transfer";size=512L;usage=[Copy_src;Copy_dst]}) in
  let texture_descriptor : Types.texture_descriptor =
    {label=Some "texture-round-trip";width=2;height=2;depth=1;
     mip_levels=1;sample_count=1;
     format=Rgba8_unorm;
     usage=[Texture_copy_src;Texture_copy_dst]} in
  let first=get (Backend.create_texture device texture_descriptor) in
  let padded=Bytes.make 512 '\000' in
  for i=0 to 7 do
    Bytes.set padded i (Char.chr (i*17+3));
    Bytes.set padded (256+i) (Char.chr (i*19+5))
  done;
  get (Backend.write_buffer upload ~offset:0L padded);
  let extent : Types.extent={width=2;height=2;depth=1} in
  let receipt = blit (fun encoder ->
    get (Backend.buffer_to_texture encoder ~src:upload ~bytes_per_row:256L
      ~bytes_per_image:512L ~dst:first ~extent ())) in
  require (Backend.completed_epoch queue = receipt.epoch)
    "queue completed epoch differs from receipt";
  require (get (Backend.poll_through queue receipt.epoch))
    "completed epoch did not remain complete";
  let compact=Bytes.create 16 in
  Bytes.blit padded 0 compact 0 8;
  Bytes.blit padded 256 compact 8 8;
  require (get (Backend.read_texture first ~bytes_per_row:8)=compact)
    "uploaded texture differs from its padded rows";
  let inset=get (Backend.create_texture device {texture_descriptor with width=4;height=4}) in
  ignore (blit (fun encoder ->
    get (Backend.buffer_to_texture encoder ~src:upload ~bytes_per_row:256L
      ~bytes_per_image:512L ~dst:inset ~origin:{Types.x=1;y=1;z=0} ~extent ())));
  let expected_image=Bytes.make 64 '\000' in
  Bytes.blit padded 0 expected_image 20 8;
  Bytes.blit padded 256 expected_image 36 8;
  require (get (Backend.read_texture inset ~bytes_per_row:16)=
    expected_image) "texture subregion upload changed other pixels";
  (* Pixel formats: a 4x4 three-mip texture per format, known texels per
     level (negative and above one for floats) uploaded with 256-byte rows;
     level 0 is read back exactly through the host read path. *)
  let format_rows=get (Backend.create_buffer device
    {label=Some"format-rows";size=1024L;usage=[Copy_src;Copy_dst]}) in
  let format_texture ?(width=4) ?(mip_levels=3) ?(usage=[Types.Texture_binding;Texture_copy_src;Texture_copy_dst]) format =
    Backend.create_texture device
      {label=Some"format";width;height=width;depth=1;mip_levels;sample_count=1;format;usage} in
  (* Rows of [texel] values, 256-byte pitch, encoded per format. *)
  let encode format ~width texel =
    let rows=Bytes.make 1024 '\000' and size=Types.texel_bytes format/4 in
    for y=0 to width-1 do for x=0 to width-1 do for c=0 to 3 do
      let offset=y*256+(x*4+c)*size and v=texel x y c in
      match format with
      | Types.Rgba8_unorm -> Bytes.set_uint8 rows offset (int_of_float v land 255)
      | Rgba16_float -> Bytes.set_uint16_le rows offset (Types.half_of_float v)
      | Rgba32_float -> Bytes.set_int32_le rows offset (Int32.bits_of_float v)
      | R8_unorm | Bgra8_unorm | Bgra8_unorm_srgb | Depth32_float ->
          failwith "conformance image writer requires an RGBA color format"
    done done done;
    rows in
  List.iter (fun format ->
    let texture=get (format_texture format) in
    for mip=0 to 2 do
      let width=4 lsr mip in
      let rows=encode format ~width (fun x y c ->
        if format=Types.Rgba8_unorm then float_of_int (x*7+y*31+c*13+mip*50)
        else float_of_int (x-2*y+3*c)*.1.5+.float_of_int mip*.100.-.7.) in
      get (Backend.write_buffer format_rows ~offset:0L rows);
      let extent : Types.extent={width;height=width;depth=1} in
      ignore (blit (fun encoder ->
        get (Backend.buffer_to_texture encoder ~src:format_rows ~bytes_per_row:256L
          ~bytes_per_image:1024L ~dst:texture ~mip ~extent ())));
      if mip=0 then begin
        let row=4*Types.texel_bytes format in
        let compact=Bytes.create (4*row) in
        for y=0 to 3 do Bytes.blit rows (y*256) compact (y*row) row done;
        require (get (Backend.read_texture texture ~bytes_per_row:row)=compact)
          "format host read differs"
      end
    done;
    get (Backend.destroy_texture texture)) [Types.Rgba8_unorm;Rgba16_float;Rgba32_float];
  (match format_texture ~mip_levels:1 ~usage:[Texture_binding;Render_attachment] Types.Rgba16_float with
   | Error { Error.kind = Unsupported; _ } -> ()
   | _ -> failwith "float render attachment was accepted");
  let half=get (format_texture Types.Rgba16_float) in
  let wide=get (format_texture ~width:32 ~mip_levels:1 Types.Rgba32_float) in
  let commands=get (Backend.begin_commands queue) in
  let encoder=get (Backend.blit_encoder commands) in
  (match Backend.buffer_to_texture encoder ~src:format_rows ~bytes_per_row:256L ~bytes_per_image:256L
     ~dst:wide ~extent:{width=32;height=1;depth=1} () with
   | Error { Error.kind = Invalid_argument; _ } -> ()
   | _ -> failwith "row pitch below the float texel width was accepted");
  get (Backend.end_blit encoder);
  get (Backend.abandon commands);
  (* Linear mip sampling of Rgba16_float at LOD 0, 0.5 and 1 from uniform
     levels: the exact levels and their average. *)
  if Caps.has profile Caps.Compute_pipeline then begin
    let levels=[|[|2.;-1.;0.5;4.|];[|4.;-3.;1.5;8.|];[|100.;100.;100.;100.|]|] in
    for mip=0 to 2 do
      let width=4 lsr mip in
      get (Backend.write_buffer format_rows ~offset:0L
        (encode Types.Rgba16_float ~width (fun _ _ c -> levels.(mip).(c))));
      ignore (blit (fun encoder ->
        get (Backend.buffer_to_texture encoder ~src:format_rows ~bytes_per_row:256L
          ~bytes_per_image:1024L ~dst:half ~mip ~extent:{width;height=width;depth=1} ())))
    done;
    let source=Bytes.of_string
      "#include <metal_stdlib>\nusing namespace metal;\nkernel void sample_lod(texture2d<float> image [[texture(0)]], device float4 *out [[buffer(1)]], uint i [[thread_position_in_grid]]) { constexpr sampler s(filter::linear, mip_filter::linear, address::clamp_to_edge); out[i] = image.sample(s, float2(0.3, 0.6), level(float(i) * 0.5)); }\n" in
    let interface : Shader.binding list=
      [{group=0;binding=0;kind=Shader.Sampled_texture;visibility=[Shader.Compute]};
       {group=0;binding=1;kind=Shader.Storage_buffer;visibility=[Shader.Compute]}] in
    let shader=get (Shader.create {backend="metal";label=Some"sample-lod";bytes=source;
      entry_points=[{name="sample_lod";stage=Shader.Compute}];bindings=interface}) in
    let library=get (Backend.create_library device shader) in
    let pipeline=get (Backend.create_compute_pipeline_from library ~entry:"sample_lod" ~interface ()) in
    let out=get (Backend.create_buffer device {label=Some"sample-lod";size=48L;usage=[Storage;Copy_src]}) in
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.compute_encoder commands) in
    get (Backend.set_pipeline encoder pipeline);
    get (Backend.set_texture encoder ~index:0 half);
    get (Backend.set_buffer encoder ~index:1 out);
    get (Backend.dispatch_threads encoder ~threads:(3,1,1) ~threadgroup:(3,1,1));
    get (Backend.end_compute encoder);
    poll (get (Backend.commit commands)) 1000;
    let values=get (Backend.read_buffer out ~offset:0L ~length:48) in
    Array.iteri (fun i expected -> Array.iteri (fun c expected ->
      let actual=Int32.float_of_bits (Bytes.get_int32_le values ((i*4+c)*4)) in
      require (Float.abs (actual-.expected)<=1e-3)
        (Printf.sprintf "Rgba16_float sample at LOD %g channel %d: %g, expected %g" (float_of_int i*.0.5) c actual expected))
      expected)
      [|levels.(0);Array.map2 (fun a b->(a+.b)*.0.5) levels.(0) levels.(1);levels.(1)|];
    get (Backend.destroy_buffer out);
    get (Backend.destroy_pipeline pipeline);
    get (Backend.destroy_library library)
  end;
  List.iter (fun texture -> get (Backend.destroy_texture texture)) [half;wide];
  get (Backend.destroy_buffer format_rows);
  (* Reusable libraries and immediate-mode encoders: one library, two
     pipelines by function constant, exact output, blit copy, and status
     polling on a non-blocking command buffer. *)
  let library_source=Bytes.of_string
    "#include <metal_stdlib>\nusing namespace metal;\nconstant bool TRIPLE [[function_constant(0)]];\nkernel void scale(device uint *values [[buffer(0)]], constant uint &bias [[buffer(1)]], uint i [[thread_position_in_grid]]) { values[i] = values[i] * (TRIPLE ? 3u : 2u) + bias; }\nkernel void fill(device uint *values [[buffer(0)]], uint i [[thread_position_in_grid]]) { values[i] = i; }\n" in
  let library_shader=get (Shader.create
    {backend="metal";label=Some"conformance-library";bytes=library_source;
     entry_points=[{name="scale";stage=Shader.Compute};{name="fill";stage=Shader.Compute}];
     bindings=[]}) in
  let library=get (Backend.create_library device library_shader) in
  let scale_interface : Shader.binding list=
    [{group=0;binding=0;kind=Shader.Storage_buffer;visibility=[Shader.Compute]};
     {group=0;binding=1;kind=Shader.Uniform_buffer;visibility=[Shader.Compute]}] in
  let rec poll_epoch receipt attempts =
    match get (Backend.status queue receipt) with
    | Backend.Completed -> ()
    | Backend.Pending when attempts = 0 -> failwith "encoded commands did not complete"
    | Backend.Pending -> Unix.sleepf 0.001; poll_epoch receipt (attempts - 1) in
  let words=get (Backend.create_buffer device
    {label=Some"encoded-words";size=16L;usage=[Storage;Copy_src;Copy_dst]}) in
  let local=get (Backend.create_buffer ~memory:Types.Device_local device
    {label=Some"device-local";size=16L;usage=[Storage;Copy_src;Copy_dst]}) in
  (match Backend.read_buffer local ~offset:0L ~length:16 with
   | Error { Error.kind = Unsupported; _ } -> ()
   | _ -> failwith "device-local buffer was host readable");
  (match Backend.write_buffer local ~offset:0L (Bytes.make 16 '\000') with
   | Error { Error.kind = Unsupported; _ } -> ()
   | _ -> failwith "device-local buffer was host writable");
  let bias=Bytes.create 4 in
  Bytes.set_int32_le bias 0 5l;
  if Caps.has profile Caps.Compute_pipeline then begin
    let triple=get (Backend.create_compute_pipeline_from library ~entry:"scale"
      ~constants:["TRIPLE",Shader.Bool true] ~interface:scale_interface ()) in
    let double=get (Backend.create_compute_pipeline_from library ~entry:"scale"
      ~constants:["TRIPLE",Shader.Bool false] ~interface:scale_interface ()) in assert (Backend.max_total_threads double >= 32);
    (match Backend.create_compute_pipeline_from library ~entry:"scale"
       ~constants:["TRIPLE",Shader.Bool true]
       ~interface:[{group=0;binding=0;kind=Shader.Storage_buffer;visibility=[Shader.Compute]}] () with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "mismatched library interface was accepted");
    (match Backend.create_compute_pipeline_from library ~entry:"missing" ~interface:[] () with
     | Error _ -> ()
     | Ok _ -> failwith "missing library entry was accepted");
    (match Backend.destroy_library library with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "library with live pipelines was destroyed");
    let run pipeline factor =
      let input=Bytes.create 16 in
      for i=0 to 3 do Bytes.set_int32_le input (i*4) (Int32.of_int i) done;
      get (Backend.write_buffer words ~offset:0L input);
      let commands=get (Backend.begin_commands queue) in
      let compute=get (Backend.compute_encoder commands) in
      (match Backend.dispatch_threads compute ~threads:(4,1,1) ~threadgroup:(4,1,1) with
       | Error { Error.kind = Invalid_state; _ } -> ()
       | _ -> failwith "dispatch without a pipeline was accepted");
      get (Backend.set_pipeline compute pipeline);
      get (Backend.set_buffer compute ~index:0 words);
      get (Backend.set_bytes compute ~index:1 bias);
      (match Backend.blit_encoder commands with
       | Error { Error.kind = Invalid_state; _ } -> ()
       | _ -> failwith "second encoder opened while one was recording");
      get (Backend.dispatch_threads compute ~threads:(4,1,1) ~threadgroup:(4,1,1));
      get (Backend.end_compute compute);
      let receipt=get (Backend.commit commands) in
      (match Backend.commit commands with
       | Error { Error.kind = Invalid_state; _ } -> ()
       | _ -> failwith "commands were committed twice");
      poll_epoch receipt 1000;
      let output=get (Backend.read_buffer words ~offset:0L ~length:16) in
      for i=0 to 3 do
        require (Bytes.get_int32_le output (i*4)=Int32.of_int (i*factor+5))
          "encoded compute output differs"
      done in
    run triple 3; run double 2; run triple 3;
    get (Backend.destroy_pipeline triple);
    get (Backend.destroy_pipeline double)
  end else begin
    (match Backend.create_compute_pipeline_from library ~entry:"scale" ~interface:scale_interface () with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported library pipeline was accepted");
    let commands=get (Backend.begin_commands queue) in
    (match Backend.compute_encoder commands with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported compute encoder was opened");
    get (Backend.abandon commands)
  end;
  (* Abandoned commands never execute and cannot be committed. *)
  get (Backend.write_buffer upload ~offset:0L (Bytes.make 512 '\255'));
  let abandoned=get (Backend.begin_commands queue) in
  let blit=get (Backend.blit_encoder abandoned) in
  get (Backend.buffer_to_texture blit ~src:upload ~bytes_per_row:256L
    ~bytes_per_image:512L ~dst:first ~extent ());
  get (Backend.abandon abandoned);
  (match Backend.commit abandoned with
   | Error { Error.kind = Invalid_state; _ } -> ()
   | _ -> failwith "abandoned commands were committed");
  require (get (Backend.read_texture first ~bytes_per_row:8)=compact)
    "abandoned commands executed";
  get (Backend.destroy_library library);
  (* Ray tracing: BLAS build, ray-query primitive id and t, TLAS instance ids;
     typed Unsupported otherwise. *)
  let ray_source=Bytes.of_string
    "#include <metal_stdlib>\n#include <metal_raytracing>\nusing namespace metal;\nusing namespace raytracing;\nstruct Hit { uint hit; uint primitive; uint instance; float t; };\nkernel void trace(primitive_acceleration_structure scene [[buffer(0)]], device const float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], uint i [[thread_position_in_grid]]) { ray r(rays[i*2].xyz, rays[i*2+1].xyz, 0.001f, 100.0f); intersector<triangle_data> isect; isect.assume_geometry_type(geometry_type::triangle); auto h = isect.intersect(r, scene); hits[i].hit = h.type == intersection_type::triangle ? 1u : 0u; hits[i].primitive = h.primitive_id; hits[i].instance = 0u; hits[i].t = h.distance; }\nkernel void trace_instances(instance_acceleration_structure scene [[buffer(0)]], device const float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], uint i [[thread_position_in_grid]]) { ray r(rays[i*2].xyz, rays[i*2+1].xyz, 0.001f, 100.0f); intersector<triangle_data, instancing> isect; isect.assume_geometry_type(geometry_type::triangle); auto h = isect.intersect(r, scene); hits[i].hit = h.type == intersection_type::triangle ? 1u : 0u; hits[i].primitive = h.primitive_id; hits[i].instance = h.instance_id; hits[i].t = h.distance; }\n" in
  let ray_shader=get (Shader.create
    {backend="metal";label=Some"conformance-rays";bytes=ray_source;
     entry_points=[{name="trace";stage=Shader.Compute};{name="trace_instances";stage=Shader.Compute}];
     bindings=[]}) in
  let ray_interface : Shader.binding list=
    [{group=0;binding=0;kind=Shader.Acceleration_structure;visibility=[Shader.Compute]};
     {group=0;binding=1;kind=Shader.Storage_buffer;visibility=[Shader.Compute]};
     {group=0;binding=2;kind=Shader.Storage_buffer;visibility=[Shader.Compute]}] in
  let put_f32 bytes offset value=Bytes.set_int32_le bytes offset (Int32.bits_of_float value) in
  (* Two triangles in z=0 (primitive 0 around x=0, primitive 1 around x=4). *)
  let vertex_bytes z=
    let bytes=Bytes.create 72 in
    Array.iteri (fun i v -> put_f32 bytes (i*4) v)
      [|-1.;-1.;z; 1.;-1.;z; 0.;1.;z;  3.;-1.;z; 5.;-1.;z; 4.;1.;z|];
    bytes in
  let vertices=get (Backend.create_buffer device
    {label=Some"ray-vertices";size=72L;usage=[Storage;Copy_dst]}) in
  get (Backend.write_buffer vertices ~offset:0L (vertex_bytes 0.));
  let ray_bytes=Bytes.create 128 in
  (* Rays from z=2 toward -z at x=0, x=4, x=8 (miss), x=12 (miss). *)
  Array.iteri (fun i v -> put_f32 ray_bytes (i*4) v)
    [|0.;0.;2.;0.; 0.;0.;-1.;0.;  4.;0.;2.;0.; 0.;0.;-1.;0.;  8.;0.;2.;0.; 0.;0.;-1.;0.;  12.;0.;2.;0.; 0.;0.;-1.;0.|];
  let rays=get (Backend.create_buffer device {label=Some"rays";size=128L;usage=[Storage;Copy_dst]}) in
  get (Backend.write_buffer rays ~offset:0L ray_bytes);
  let hits=get (Backend.create_buffer device {label=Some"hits";size=64L;usage=[Storage;Copy_src]}) in
  let blas_descriptor=Backend.Blas
    {geometries=[Backend.Triangles {vertices;offset=0L;length=72L;vertex_stride=12;vertex_count=6}]} in
  if Caps.has profile Caps.Ray_tracing then begin
    let ray_library=get (Backend.create_library device ray_shader) in
    let trace=get (Backend.create_compute_pipeline_from ray_library ~entry:"trace" ~interface:ray_interface ()) in
    let trace_instances=get (Backend.create_compute_pipeline_from ray_library ~entry:"trace_instances" ~interface:ray_interface ()) in
    let blas=get (Backend.create_accel device blas_descriptor) in
    let sizes=Backend.accel_sizes blas in
    require (sizes.structure_size>0L && sizes.build_scratch_size>=0L) "BLAS sizes are empty";
    let scratch=get (Backend.create_buffer ~memory:Types.Device_local device
      {label=Some"scratch";size=Int64.max 256L (Int64.mul 2L sizes.build_scratch_size);usage=[Storage]}) in
    let instances=get (Backend.pack_instance_records device
      [|{instance={transform=[|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.|];mask=0xFFFF_FFFF;structure_index=0};user_id=0};
        {instance={transform=[|1.;0.;0.;8.; 0.;1.;0.;0.; 0.;0.;1.;0.|];mask=0xFFFF_FFFF;structure_index=0};user_id=1}|]) in
    let stride=Bytes.length instances/2 in
    let instance_buffer=get (Backend.create_buffer device
      {label=Some"instances";size=Int64.of_int (2*stride);usage=[Storage;Copy_dst]}) in
    get (Backend.write_buffer instance_buffer ~offset:0L instances);
    let tlas=get (Backend.create_accel device
      (Backend.Tlas_of {instances=instance_buffer;offset=0L;instance_count=2;kind=User_id_instances;structures=[blas];motion_transforms=None})) in
    (match Backend.create_accel device
       (Backend.Tlas_of {instances=instance_buffer;offset=0L;instance_count=3;kind=User_id_instances;structures=[blas];motion_transforms=None}) with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "oversized TLAS instance count was accepted");
    let tlas_scratch=get (Backend.create_buffer ~memory:Types.Device_local device
      {label=Some"tlas-scratch";size=Int64.max 256L (Backend.accel_sizes tlas).build_scratch_size;usage=[Storage]}) in
    let commands=get (Backend.begin_commands queue) in
    let compute=get (Backend.compute_encoder commands) in
    get (Backend.set_pipeline compute trace);
    (match Backend.set_accel compute ~index:0 blas with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "unbuilt structure was bound");
    (match Backend.compute_use_accels compute [blas] with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "unbuilt structure was declared used");
    get (Backend.compute_use_accels compute []);
    get (Backend.end_compute compute);
    let build=get (Backend.accel_encoder commands) in
    (match Backend.build_accel build tlas ~scratch:tlas_scratch () with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "TLAS built before its BLAS");
    get (Backend.build_accel build blas ~scratch ());
    (match Backend.build_accel build blas ~scratch () with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "double build was accepted");
    get (Backend.build_accel build tlas ~scratch:tlas_scratch ());
    get (Backend.end_accel build);
    let query commands pipeline structure =
      let compute=get (Backend.compute_encoder commands) in
      get (Backend.set_pipeline compute pipeline);
      get (Backend.set_accel compute ~index:0 structure);
      get (Backend.compute_use_accels compute [blas]);
      get (Backend.set_buffer compute ~index:1 rays);
      get (Backend.set_buffer compute ~index:2 hits);
      get (Backend.dispatch_threads compute ~threads:(4,1,1) ~threadgroup:(4,1,1));
      get (Backend.end_compute compute) in
    query commands trace blas;
    let receipt=get (Backend.commit commands) in
    poll_epoch receipt 1000;
    let read_hits () =
      let bytes=get (Backend.read_buffer hits ~offset:0L ~length:64) in
      Array.init 4 (fun i ->
        Int32.to_int (Bytes.get_int32_le bytes (i*16)),
        Int32.to_int (Bytes.get_int32_le bytes (i*16+4)),
        Int32.to_int (Bytes.get_int32_le bytes (i*16+8)),
        Int32.float_of_bits (Bytes.get_int32_le bytes (i*16+12))) in
    let close a b=Float.abs (a-.b)<1e-4 in
    let primitive_hits=read_hits () in
    let hit0,prim0,_,t0=primitive_hits.(0) and hit1,prim1,_,t1=primitive_hits.(1)
    and hit2,_,_,_=primitive_hits.(2) and hit3,_,_,_=primitive_hits.(3) in
    require (hit0=1 && prim0=0 && close t0 2.) "primitive 0 hit differs";
    require (hit1=1 && prim1=1 && close t1 2.) "primitive 1 hit differs";
    require (hit2=0 && hit3=0) "misses reported hits";
    let commands=get (Backend.begin_commands queue) in
    query commands trace_instances tlas;
    poll_epoch (get (Backend.commit commands)) 1000;
    let instance_hits=read_hits () in
    let hit0,prim0,inst0,t0=instance_hits.(0) and hit1,prim1,inst1,t1=instance_hits.(1)
    and hit2,prim2,inst2,t2=instance_hits.(2) and hit3,prim3,inst3,t3=instance_hits.(3) in
    require (hit0=1 && prim0=0 && inst0=0 && close t0 2.) "instance 0 primitive 0 hit differs";
    require (hit1=1 && prim1=1 && inst1=0 && close t1 2.) "instance 0 primitive 1 hit differs";
    require (hit2=1 && prim2=0 && inst2=1 && close t2 2.) "instance 1 primitive 0 hit differs";
    require (hit3=1 && prim3=1 && inst3=1 && close t3 2.) "instance 1 primitive 1 hit differs";
    (* Bounding boxes resolved in the kernel's traversal loop, curves,
       motion keyframes, user-id instances with masks, and compaction. *)
    let extra_source=Bytes.of_string (String.concat "\n" [
      "#include <metal_stdlib>";"#include <metal_raytracing>";"using namespace metal;";"using namespace raytracing;";
      "struct Hit { uint hit; uint primitive; uint instance; float t; };";
      "struct Sphere { float4 center_radius; };";
      "inline bool sphere_hit(ray r, Sphere s, thread float &t) {";
      "  float3 oc = r.origin - s.center_radius.xyz; float b = dot(oc, r.direction); float cc = dot(oc, oc) - s.center_radius.w * s.center_radius.w; float disc = b * b - cc;";
      "  if (disc < 0.0f) return false; t = -b - sqrt(disc); return t >= r.min_distance && t <= r.max_distance; }";
      "inline ray make_ray(const device float4 *rays, uint tid) { ray r; r.origin = rays[tid * 2].xyz; r.direction = rays[tid * 2 + 1].xyz; r.min_distance = 0.0f; r.max_distance = 100.0f; return r; }";
      "kernel void trace_boxes(primitive_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], const device Sphere *spheres [[buffer(3)]], uint tid [[thread_position_in_grid]]) {";
      "  ray r = make_ray(rays, tid); intersection_params params; intersection_query<> query(r, scene, params);";
      "  while (query.next()) { float t; if (query.get_candidate_intersection_type() == intersection_type::bounding_box && sphere_hit(r, spheres[query.get_candidate_primitive_id()], t)) query.commit_bounding_box_intersection(t); }";
      "  hits[tid].hit = query.get_committed_intersection_type() == intersection_type::bounding_box ? 1u : 0u; hits[tid].primitive = query.get_committed_primitive_id(); hits[tid].instance = 0u; hits[tid].t = query.get_committed_distance(); }";
      "kernel void trace_curves(primitive_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], uint tid [[thread_position_in_grid]]) {";
      "  intersector<curve_data> query; auto hit = query.intersect(make_ray(rays, tid), scene);";
      "  hits[tid].hit = hit.type == intersection_type::curve ? 1u : 0u; hits[tid].primitive = hit.primitive_id; hits[tid].instance = 0u; hits[tid].t = hit.distance; }";
      "kernel void trace_motion(acceleration_structure<primitive_motion> scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], constant float &time [[buffer(3)]], uint tid [[thread_position_in_grid]]) {";
      "  intersector<primitive_motion> query; auto hit = query.intersect(make_ray(rays, tid), scene, time);";
      "  hits[tid].hit = hit.type == intersection_type::triangle ? 1u : 0u; hits[tid].primitive = hit.primitive_id; hits[tid].instance = 0u; hits[tid].t = hit.distance; }";
      "kernel void trace_masked(instance_acceleration_structure scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], constant uint &mask [[buffer(3)]], uint tid [[thread_position_in_grid]]) {";
      "  intersector<instancing> query; auto hit = query.intersect(make_ray(rays, tid), scene, mask);";
      "  hits[tid].hit = hit.type == intersection_type::triangle ? 1u : 0u; hits[tid].primitive = hit.primitive_id; hits[tid].instance = hit.user_instance_id; hits[tid].t = hit.distance; }";
      "kernel void trace_motion_instances(acceleration_structure<instancing, instance_motion> scene [[buffer(0)]], const device float4 *rays [[buffer(1)]], device Hit *hits [[buffer(2)]], constant float &time [[buffer(3)]], uint tid [[thread_position_in_grid]]) {";
      "  intersector<instancing, instance_motion> query; auto hit = query.intersect(make_ray(rays, tid), scene, time);";
      "  hits[tid].hit = hit.type == intersection_type::triangle ? 1u : 0u; hits[tid].primitive = hit.primitive_id; hits[tid].instance = hit.user_instance_id; hits[tid].t = hit.distance; }"]) in
    let extra_shader=get (Shader.create
      {backend="metal";label=Some"conformance-rt-extra";bytes=extra_source;
       entry_points=List.map (fun name -> {Shader.name;stage=Shader.Compute}) ["trace_boxes";"trace_curves";"trace_motion";"trace_masked";"trace_motion_instances"];
       bindings=[]}) in
    let extra=get (Backend.create_library device extra_shader) in
    let binding index kind : Shader.binding={group=0;binding=index;kind;visibility=[Shader.Compute]} in
    let boxes_pipeline=get (Backend.create_compute_pipeline_from extra ~entry:"trace_boxes"
      ~interface:(ray_interface@[binding 3 Shader.Storage_buffer]) ()) in
    let float_buffer label values=
      let bytes=Bytes.create (4*Array.length values) in
      Array.iteri (fun i v -> put_f32 bytes (i*4) v) values;
      let buffer=get (Backend.create_buffer device {label=Some label;size=Int64.of_int (Bytes.length bytes);usage=[Storage;Copy_dst]}) in
      get (Backend.write_buffer buffer ~offset:0L bytes); buffer in
    let spheres=float_buffer "spheres" [|0.;0.;0.;0.5; 4.;0.;0.;0.5|] in
    let boxes=float_buffer "boxes" [|-0.5;-0.5;-0.5;0.5;0.5;0.5; 3.5;-0.5;-0.5;4.5;0.5;0.5|] in
    let box_blas=get (Backend.create_accel device (Backend.Blas
      {geometries=[Backend.Bounding_boxes {boxes=[{buffer=boxes;offset=0L}];stride=24;count=2;options=Backend.default_geometry_options}]})) in
    let big_scratch=get (Backend.create_buffer ~memory:Types.Device_local device
      {label=Some"scratch-extra";size=1_048_576L;usage=[Storage]}) in
    let build_one structure=
      let commands=get (Backend.begin_commands queue) in
      let encoder=get (Backend.accel_encoder commands) in
      get (Backend.build_accel encoder structure ~scratch:big_scratch ());
      get (Backend.end_accel encoder);
      poll_epoch (get (Backend.commit commands)) 1000 in
    build_one box_blas;
    let query_with ?extra_bytes ?extra_buffer pipeline structure=
      let commands=get (Backend.begin_commands queue) in
      let compute=get (Backend.compute_encoder commands) in
      get (Backend.set_pipeline compute pipeline);
      get (Backend.set_accel compute ~index:0 structure);
      get (Backend.set_buffer compute ~index:1 rays);
      get (Backend.set_buffer compute ~index:2 hits);
      Option.iter (fun bytes -> get (Backend.set_bytes compute ~index:3 bytes)) extra_bytes;
      Option.iter (fun buffer -> get (Backend.set_buffer compute ~index:3 buffer)) extra_buffer;
      get (Backend.dispatch_threads compute ~threads:(4,1,1) ~threadgroup:(4,1,1));
      get (Backend.end_compute compute);
      poll_epoch (get (Backend.commit commands)) 1000;
      read_hits () in
    let box_hits=query_with ~extra_buffer:spheres boxes_pipeline box_blas in
    let hit0,prim0,_,t0=box_hits.(0) and hit1,prim1,_,t1=box_hits.(1) and hit2,_,_,_=box_hits.(2) and hit3,_,_,_=box_hits.(3) in
    require (hit0=1 && prim0=0 && close t0 1.5) "sphere 0 intersection differs";
    require (hit1=1 && prim1=1 && close t1 1.5) "sphere 1 intersection differs";
    require (hit2=0 && hit3=0) "bounding boxes reported false hits";
    (* Curves: one round linear segment along y at x=8 with radius 0.5, on
       devices that intersect curves; typed Unsupported elsewhere. *)
    let control=float_buffer "curve-points" [|8.;-1.;0.; 8.;1.;0.|] in
    let radii=float_buffer "curve-radii" [|0.5;0.5|] in
    let indices=get (Backend.create_buffer device {label=Some"curve-indices";size=4L;usage=[Storage;Copy_dst]}) in
    get (Backend.write_buffer indices ~offset:0L (Bytes.make 4 '\000'));
    let curve_descriptor=Backend.Blas
      {geometries=[Backend.Curves {control_points=[{buffer=control;offset=0L}];control_stride=12;control_point_count=2;
         radii=[{buffer=radii;offset=0L}];radius_stride=4;indices;index_offset=0L;segment_count=1;control_points_per_segment=2;
         curve_type=Round_curve;basis=Linear_basis;caps=Sphere_caps}]} in
    let curve_pipeline=get (Backend.create_compute_pipeline_from extra ~entry:"trace_curves" ~interface:ray_interface ()) in
    let curve_blas=
      if Caps.has profile Caps.Ray_tracing_curves then begin
        let curve_blas=get (Backend.create_accel device curve_descriptor) in
        build_one curve_blas;
        let curve_hits=query_with curve_pipeline curve_blas in
        let hit0,_,_,_=curve_hits.(0) and hit2,prim2,_,t2=curve_hits.(2) and hit3,_,_,_=curve_hits.(3) in
        require (hit0=0 && hit3=0) "curve reported false hits";
        require (hit2=1 && prim2=0 && close t2 1.5)
          (Printf.sprintf "curve intersection differs: %s"
            (String.concat ";" (Array.to_list (Array.map (fun (h,p,i,t) -> Printf.sprintf "%d/%d/%d/%g" h p i t) curve_hits))));
        Some curve_blas
      end else begin
        (match Backend.create_accel device curve_descriptor with
         | Error { Error.kind = Unsupported; _ } -> ()
         | _ -> failwith "curve geometry without curve support was accepted");
        None
      end in
    (* Motion: keyframes at z=0 and z=1.5; time selects the interpolated depth. *)
    let vertices0=get (Backend.create_buffer device {label=Some"motion-0";size=72L;usage=[Storage;Copy_dst]}) in
    let vertices1=get (Backend.create_buffer device {label=Some"motion-1";size=72L;usage=[Storage;Copy_dst]}) in
    get (Backend.write_buffer vertices0 ~offset:0L (vertex_bytes 0.));
    get (Backend.write_buffer vertices1 ~offset:0L (vertex_bytes 1.5));
    let motion={Backend.keyframes=2;start_time=0.;end_time=1.;start_border=Clamp;end_border=Clamp} in
    (match Backend.create_accel device (Backend.Motion_blas
       {geometries=[Backend.Motion_triangles {keyframes=[{buffer=vertices0;offset=0L}];vertex_stride=12;vertex_count=6}];motion}) with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "keyframe count mismatch was accepted");
    let motion_blas=get (Backend.create_accel device (Backend.Motion_blas
      {geometries=[Backend.Motion_triangles {keyframes=[{buffer=vertices0;offset=0L};{buffer=vertices1;offset=0L}];vertex_stride=12;vertex_count=6}];motion})) in
    build_one motion_blas;
    let motion_pipeline=get (Backend.create_compute_pipeline_from extra ~entry:"trace_motion" ~interface:(ray_interface@[binding 3 Shader.Uniform_buffer]) ()) in
    let time_bytes t=let bytes=Bytes.create 4 in put_f32 bytes 0 t; bytes in
    List.iter (fun (time,expected) ->
      let motion_hits=query_with ~extra_bytes:(time_bytes time) motion_pipeline motion_blas in
      let hit0,prim0,_,t0=motion_hits.(0) and hit1,_,_,t1=motion_hits.(1) in
      require (hit0=1 && prim0=0 && close t0 expected && hit1=1 && close t1 expected)
        (Printf.sprintf "motion intersection at time %g differs" time)) [0.,2.;1.,0.5;0.5,1.25];
    (* User-id instances with masks over the triangle BLAS. *)
    let identity=[|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.|] in
    let shifted=[|1.;0.;0.;8.; 0.;1.;0.;0.; 0.;0.;1.;0.|] in
    let records=get (Backend.pack_instance_records device
      [|{instance={transform=identity;mask=1;structure_index=0};user_id=7};
        {instance={transform=shifted;mask=2;structure_index=0};user_id=9}|]) in
    require (Bytes.length records=2*68) "user-id record stride differs";
    let record_buffer=get (Backend.create_buffer device {label=Some"user-id-instances";size=Int64.of_int (Bytes.length records);usage=[Storage;Copy_dst]}) in
    get (Backend.write_buffer record_buffer ~offset:0L records);
    let masked_tlas=get (Backend.create_accel device (Backend.Tlas_of
      {instances=record_buffer;offset=0L;instance_count=2;kind=User_id_instances;structures=[blas];motion_transforms=None})) in
    build_one masked_tlas;
    let masked_pipeline=get (Backend.create_compute_pipeline_from extra ~entry:"trace_masked" ~interface:(ray_interface@[binding 3 Shader.Uniform_buffer]) ()) in
    let mask_bytes mask=let bytes=Bytes.create 4 in Bytes.set_int32_le bytes 0 (Int32.of_int mask); bytes in
    let masked=query_with ~extra_bytes:(mask_bytes 1) masked_pipeline masked_tlas in
    let hit0,_,inst0,t0=masked.(0) and hit1,_,inst1,_=masked.(1) and hit2,_,_,_=masked.(2) and hit3,_,_,_=masked.(3) in
    require (hit0=1 && hit1=1 && inst0=7 && inst1=7 && close t0 2. && hit2=0 && hit3=0) "mask 1 selected the wrong instance";
    let masked=query_with ~extra_bytes:(mask_bytes 2) masked_pipeline masked_tlas in
    let hit0,_,_,_=masked.(0) and hit2,_,inst2,t2=masked.(2) and hit3,_,inst3,_=masked.(3) in
    require (hit0=0 && hit2=1 && hit3=1 && inst2=9 && inst3=9 && close t2 2.) "mask 2 selected the wrong instance";
    (* Motion instances: one instance whose transform keyframes slide it from
       the origin to x=8, so time decides which ray pair hits it. *)
    let keyframes=get (Backend.pack_transforms [|identity;shifted|]) in
    let keyframe_buffer=get (Backend.create_buffer device {label=Some"instance-keyframes";size=Int64.of_int (Bytes.length keyframes);usage=[Storage;Copy_dst]}) in
    get (Backend.write_buffer keyframe_buffer ~offset:0L keyframes);
    let motion_records=get (Backend.pack_motion_instances device
      [|{record={instance={transform=identity;mask=1;structure_index=0};user_id=5};
         transforms_start=0;transforms_count=2;start_time=0.;end_time=1.;start_border=Clamp;end_border=Clamp}|]) in
    require (Bytes.length motion_records=44) "motion instance record stride differs";
    let motion_record_buffer=get (Backend.create_buffer device {label=Some"motion-instances";size=Int64.of_int (Bytes.length motion_records);usage=[Storage;Copy_dst]}) in
    get (Backend.write_buffer motion_record_buffer ~offset:0L motion_records);
    (match Backend.create_accel device (Backend.Tlas_of
      {instances=motion_record_buffer;offset=0L;instance_count=1;kind=Motion_instances;structures=[blas];motion_transforms=None}) with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "motion instances without keyframe transforms were accepted");
    let motion_tlas=get (Backend.create_accel device (Backend.Tlas_of
      {instances=motion_record_buffer;offset=0L;instance_count=1;kind=Motion_instances;structures=[blas];
       motion_transforms=Some (keyframe_buffer,0L,2)})) in
    build_one motion_tlas;
    let motion_instances_pipeline=get (Backend.create_compute_pipeline_from extra ~entry:"trace_motion_instances" ~interface:(ray_interface@[binding 3 Shader.Uniform_buffer]) ()) in
    let at_start=query_with ~extra_bytes:(time_bytes 0.) motion_instances_pipeline motion_tlas in
    let hit0,_,inst0,t0=at_start.(0) and hit2,_,_,_=at_start.(2) in
    require (hit0=1 && inst0=5 && close t0 2. && hit2=0) "motion instance at time 0 is not at the origin";
    let at_end=query_with ~extra_bytes:(time_bytes 1.) motion_instances_pipeline motion_tlas in
    let hit0,_,_,_=at_end.(0) and hit2,_,inst2,t2=at_end.(2) in
    require (hit0=0 && hit2=1 && inst2=5 && close t2 2.) "motion instance at time 1 did not reach x=8";
    (* Compaction of the triangle BLAS reproduces its hits. *)
    let size_buffer=get (Backend.create_buffer device {label=Some"compacted-size";size=8L;usage=[Storage;Copy_src]}) in
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.accel_encoder commands) in
    (match Backend.write_compacted_size encoder blas ~dst:size_buffer ~offset:4L () with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "unaligned compacted size offset was accepted");
    get (Backend.write_compacted_size encoder blas ~dst:size_buffer ());
    get (Backend.end_accel encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    let compact_size=Bytes.get_int64_le (get (Backend.read_buffer size_buffer ~offset:0L ~length:8)) 0 in
    require (compact_size>0L && compact_size<=sizes.structure_size) "compacted size is out of range";
    let compacted=get (Backend.create_accel device (Backend.Sized {size=compact_size;template=blas})) in
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.accel_encoder commands) in
    (match Backend.build_accel encoder compacted ~scratch:big_scratch () with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "sized structure accepted a build");
    get (Backend.compact_accel encoder ~src:blas ~dst:compacted);
    get (Backend.end_accel encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    let structure_hits=query_with trace compacted in
      let hit0,prim0,_,t0=structure_hits.(0) and hit1,prim1,_,t1=structure_hits.(1) and hit2,_,_,_=structure_hits.(2) in
      require (hit0=1 && prim0=0 && close t0 2. && hit1=1 && prim1=1 && close t1 2. && hit2=0)
      "compacted structure hits differ from the source";
    List.iter (fun destroy -> get (destroy ())) [
      (fun () -> Backend.destroy_pipeline boxes_pipeline);
      (fun () -> Backend.destroy_pipeline curve_pipeline);(fun () -> Backend.destroy_pipeline motion_pipeline);
      (fun () -> Backend.destroy_pipeline masked_pipeline);(fun () -> Backend.destroy_pipeline motion_instances_pipeline);
      (fun () -> Backend.destroy_accel motion_tlas);(fun () -> Backend.destroy_buffer motion_record_buffer);
      (fun () -> Backend.destroy_buffer keyframe_buffer);
      (fun () -> Backend.destroy_accel compacted);
      (fun () -> Backend.destroy_accel masked_tlas);(fun () -> Backend.destroy_accel motion_blas);
      (fun () -> Option.fold ~none:(Ok ()) ~some:Backend.destroy_accel curve_blas);(fun () -> Backend.destroy_accel box_blas);
      (fun () -> Backend.destroy_buffer size_buffer);
      (fun () -> Backend.destroy_buffer record_buffer);(fun () -> Backend.destroy_buffer vertices1);
      (fun () -> Backend.destroy_buffer vertices0);(fun () -> Backend.destroy_buffer indices);
      (fun () -> Backend.destroy_buffer radii);(fun () -> Backend.destroy_buffer control);
      (fun () -> Backend.destroy_buffer big_scratch);(fun () -> Backend.destroy_buffer boxes);
      (fun () -> Backend.destroy_buffer spheres);(fun () -> Backend.destroy_library extra)];
    (match Backend.destroy_accel blas with
     | Ok () -> ()
     | Error error -> failwith (Error.to_string error));
    get (Backend.destroy_accel tlas);
    get (Backend.destroy_buffer instance_buffer);
    get (Backend.destroy_buffer tlas_scratch);
    get (Backend.destroy_buffer scratch);
    get (Backend.destroy_pipeline trace);
    get (Backend.destroy_pipeline trace_instances);
    get (Backend.destroy_library ray_library)
  end else begin
    (match Backend.create_accel device blas_descriptor with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported acceleration structure was created");
    let commands=get (Backend.begin_commands queue) in
    (match Backend.accel_encoder commands with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported acceleration encoder was opened");
    get (Backend.abandon commands);
    require (Bytes.length (get (Backend.pack_instance_records device
      [|{instance={transform=[|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.|];mask=1;structure_index=0};user_id=3}|]))=68)
      "user-id instance packing differs";
    require (Bytes.length (get (Backend.pack_motion_instances device
      [|{record={instance={transform=[|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.|];mask=1;structure_index=0};user_id=3};
         transforms_start=0;transforms_count=2;start_time=0.;end_time=1.;start_border=Clamp;end_border=Clamp}|]))=44)
      "motion instance packing differs"
  end;
  get (Backend.destroy_buffer vertices);
  get (Backend.destroy_buffer rays);
  get (Backend.destroy_buffer hits);
  get (Backend.destroy_buffer words);
  get (Backend.destroy_buffer local);
  (* Render path: samplers, portable render pipelines, the render encoder
     with exact pixels, batched draws, indirect command buffers, argument
     buffers, depth load/clear, and encoded texture blits. *)
  let sampler_descriptor : Types.sampler_descriptor=
    {label=Some"conformance-sampler";min_filter=Nearest;mag_filter=Nearest;mip_filter=No_mip;
     address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;max_anisotropy=1} in
  let sampler=get (Backend.create_sampler device sampler_descriptor) in
  let render_source=Bytes.of_string
    "#include <metal_stdlib>\nusing namespace metal;\nstruct V { float4 position [[position]]; float2 uv; };\nvertex V flat_vertex(uint i [[vertex_id]], const device float *offsets [[buffer(0)]]) { float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}}; V v; v.position=float4(p[i]+float2(offsets[0],offsets[1]),0.,1.); v.uv=(p[i]+1.)*.5; return v; }\nfragment float4 flat_fragment(V v [[stage_in]], constant float4 &color [[buffer(1)]]) { return color; }\nstruct Args { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };\nfragment float4 argument_fragment(V v [[stage_in]], constant Args &args [[buffer(1)]]) { return args.image.sample(args.sampling, v.uv); }\nfragment float4 textured_fragment(V v [[stage_in]], texture2d<float> image [[texture(1)]], sampler sampling [[sampler(2)]]) { return image.sample(sampling, v.uv); }\n" in
  let render_shader entries bindings=get (Shader.create
    {backend="metal";label=Some"conformance-render";bytes=render_source;entry_points=entries;bindings}) in
  let vertex=render_shader [{name="flat_vertex";stage=Shader.Vertex}]
    [{group=0;binding=0;kind=Shader.Storage_buffer;visibility=[Shader.Vertex]}] in
  let fragment_flat=render_shader [{name="flat_fragment";stage=Shader.Fragment}]
    [{group=0;binding=1;kind=Shader.Uniform_buffer;visibility=[Shader.Fragment]}] in
  let fragment_argument=render_shader [{name="argument_fragment";stage=Shader.Fragment}]
    [{group=0;binding=1;kind=Shader.Storage_buffer;visibility=[Shader.Fragment]}] in
  let fragment_textured=render_shader [{name="textured_fragment";stage=Shader.Fragment}]
    [{group=0;binding=1;kind=Shader.Sampled_texture;visibility=[Shader.Fragment]};
     {group=0;binding=2;kind=Shader.Sampler;visibility=[Shader.Fragment]}] in
  let render_layout entries=
    let group=get (Binding.create_layout entries) in
    get (Binding.create_pipeline_layout ~device:(Backend.device_handle device) ~capabilities [0,group]) in
  let vertex_entry : Binding.layout_entry={binding=0;kind=Binding.Buffer;visibility=[Binding.Vertex]} in
  let flat_layout=render_layout [vertex_entry;{binding=1;kind=Binding.Buffer;visibility=[Binding.Fragment]}] in
  let textured_layout=render_layout
    [vertex_entry;{binding=1;kind=Binding.Texture;visibility=[Binding.Fragment]};
     {binding=2;kind=Binding.Sampler;visibility=[Binding.Fragment]}] in
  let render_descriptor ?(layout=flat_layout) fragment entry : Pipeline.render_descriptor=
    {backend="metal";label=Some"conformance-render";layout;vertex;vertex_entry="flat_vertex";
     fragment=Some fragment;fragment_entry=Some entry;color_format=Rgba8_unorm;depth_format=No_depth;sample_count=1} in
  let target_descriptor : Types.texture_descriptor=
    {label=Some"render-target";width=4;height=4;depth=1;mip_levels=1;sample_count=1;
     format=Rgba8_unorm;
     usage=[Texture_binding;Render_attachment;Texture_copy_src]} in
  let target=get (Backend.create_texture device target_descriptor) in
  let offsets=get (Backend.create_buffer device {label=Some"offsets";size=8L;usage=[Storage]}) in
  get (Backend.write_buffer offsets ~offset:0L (Bytes.make 8 '\000'));
  let color_bytes r g b a=let bytes=Bytes.create 16 in
    List.iteri (fun i v->Bytes.set_int32_le bytes (i*4) (Int32.bits_of_float v)) [r;g;b;a]; bytes in
  let pixel bytes x y=Bytes.sub_string bytes ((y*4+x)*4) 4 in
  let expect_pixels expected message=
    let pixels=get (Backend.read_texture target ~bytes_per_row:16) in
    require (pixel pixels 0 0=expected && pixel pixels 3 3=expected) message in
  let clear_target r g b a=
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Clear;store=Store;clear=(r,g,b,a)}];depth=None}) in
    get (Backend.end_render encoder);
    poll_epoch (get (Backend.commit commands)) 1000 in
  if Caps.has profile Caps.Render_pipeline then begin
    let flat=get (Backend.create_render_pipeline device (render_descriptor fragment_flat "flat_fragment")) in
    clear_target 0.125 0.25 0.5 1.;
    expect_pixels "\x20\x40\x80\xff" "clear-only render pass pixels differ";
    (* One full-screen triangle through the immediate encoder with a fragment
       uniform buffer. *)
    let color=get (Backend.create_buffer device {label=Some"color";size=16L;usage=[Uniform]}) in
    let set_color encoder r g b a=
      get (Backend.write_buffer color ~offset:0L (color_bytes r g b a));
      get (Backend.set_stage_buffer encoder Fragment ~index:1 color) in
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
    (match Backend.draw encoder ~primitive:Triangle_list ~first:0 ~count:3 () with
     | Error { Error.kind = Invalid_state; _ } -> ()
     | _ -> failwith "draw without a render pipeline was accepted");
    get (Backend.set_render_pipeline encoder flat);
    get (Backend.set_stage_buffer encoder Vertex ~index:0 offsets);
    set_color encoder 1. 0. 0.5 1.;
    (* A viewport may hang over the target; an empty one is refused. *)
    (match Backend.set_viewport encoder {x=0;y=0;width=0;height=4} with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "empty viewport was accepted");
    get (Backend.set_viewport encoder {x= -2;y= -2;width=8;height=8});
    get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
    get (Backend.set_scissor encoder {x=0;y=0;width=4;height=4});
    get (Backend.set_cull encoder Cull_none);
    get (Backend.draw encoder ~primitive:Triangle_list ~first:0 ~count:3 ());
    get (Backend.end_render encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    expect_pixels "\xff\x00\x80\xff" "immediate triangle pixels differ";
    (* The same triangle as one indexed batch draw. *)
    let indices=get (Backend.create_buffer device {label=Some"indices";size=12L;usage=[Index]}) in
    let index_bytes=Bytes.create 12 in
    List.iteri (fun i v->Bytes.set_int32_le index_bytes (i*4) v) [0l;1l;2l];
    get (Backend.write_buffer indices ~offset:0L index_bytes);
    get (Backend.write_buffer color ~offset:0L (color_bytes 0. 1. 0. 1.));
    clear_target 0. 0. 0. 1.;
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
    get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
    get (Backend.draw_batch encoder
      [|{pipeline=flat;buffers=[|Vertex,0,offsets,0L;Fragment,1,color,0L|];primitive=Triangle_list;
         index=Some (Uint32,indices,0L,3L);vertex_start=0;vertex_count=3;instances=1}|]);
    get (Backend.end_render encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    expect_pixels "\x00\xff\x00\xff" "batched indexed triangle pixels differ";
    (* The same indexed triangle, its counts read from a buffer when the draw runs (index count
       3, one instance, at offset 4); a misaligned argument range is refused. *)
    let arguments=get (Backend.create_buffer device {label=Some"draw arguments";size=24L;usage=[Storage]}) in
    let argument_bytes=Bytes.create 24 in
    List.iteri (fun i v->Bytes.set_int32_le argument_bytes (i*4) v) [0l;3l;1l;0l;0l;0l];
    get (Backend.write_buffer arguments ~offset:0L argument_bytes);
    get (Backend.write_buffer color ~offset:0L (color_bytes 1. 1. 0. 1.));
    clear_target 0. 0. 0. 1.;
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
    get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
    get (Backend.set_render_pipeline encoder flat);
    get (Backend.set_stage_buffer encoder Vertex ~index:0 offsets);
    get (Backend.set_stage_buffer encoder Fragment ~index:1 color);
    (match Backend.draw_indexed_indirect encoder ~primitive:Triangle_list ~index_type:Uint32 indices ~offset:0L
        arguments ~arguments_offset:6L with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "misaligned indirect arguments were accepted");
    get (Backend.draw_indexed_indirect encoder ~primitive:Triangle_list ~index_type:Uint32 indices ~offset:0L
      arguments ~arguments_offset:4L);
    get (Backend.end_render encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    expect_pixels "\xff\xff\x00\xff" "indirect indexed triangle pixels differ";
    (* Indirect command buffer: one recorded draw, executed twice. *)
    let indirect=get (Backend.create_render_pipeline ~indirect:true device (render_descriptor fragment_flat "flat_fragment")) in
    let icb=get (Backend.create_icb device ~max_commands:2) in
    (match Backend.icb_set_pipeline icb ~index:2 indirect with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "out-of-range indirect command index was accepted");
    get (Backend.write_buffer color ~offset:0L (color_bytes 0. 0. 1. 1.));
    get (Backend.icb_set_pipeline icb ~index:0 indirect);
    get (Backend.icb_set_buffer icb ~index:0 Vertex ~slot:0 offsets);
    get (Backend.icb_set_buffer icb ~index:0 Fragment ~slot:1 color);
    get (Backend.icb_draw_indexed icb ~index:0 ~primitive:Triangle_list ~index_type:Uint32 indices ~offset:0L ~count:3L ());
    clear_target 0. 0. 0. 1.;
    for _=1 to 2 do
      let commands=get (Backend.begin_commands queue) in
      let encoder=get (Backend.render_encoder commands
        {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
      get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
      (match Backend.execute_icb encoder icb ~location:0 ~length:1 with
       | Error { Error.kind = Invalid_state; _ } -> ()
       | _ -> failwith "indirect execution without a pipeline was accepted");
      get (Backend.set_render_pipeline encoder indirect);
      get (Backend.use_resources encoder [`Buffer offsets;`Buffer color;`Buffer indices]);
      get (Backend.execute_icb encoder icb ~location:0 ~length:1);
      get (Backend.end_render encoder);
      poll_epoch (get (Backend.commit commands)) 1000
    done;
    expect_pixels "\x00\x00\xff\xff" "indirect command pixels differ";
    (* Argument buffer: a 1x1 texture and sampler bound through one buffer. *)
    let argument_pipeline=get (Backend.create_render_pipeline ~indirect:true device
      (render_descriptor fragment_argument "argument_fragment")) in
    let argument=get (Backend.create_argument argument_pipeline Fragment ~index:1) in
    require (Backend.argument_length argument>0) "argument length is empty";
    let argument_buffer=get (Backend.create_buffer device
      {label=Some"arguments";size=Int64.of_int (max 256 (Backend.argument_length argument));usage=[Storage]}) in
    let texel=get (Backend.create_texture device
      {label=Some"texel";width=1;height=1;depth=1;mip_levels=1;sample_count=1;format=Rgba8_unorm;usage=[Texture_binding;Texture_copy_dst]}) in
    let texel_upload=get (Backend.create_buffer device {label=Some"texel-upload";size=256L;usage=[Copy_src]}) in
    get (Backend.write_buffer texel_upload ~offset:0L (Bytes.init 256 (fun i->if i<4 then "\x40\x80\xc0\xff".[i] else '\000')));
    let commands=get (Backend.begin_commands queue) in
    let blit=get (Backend.blit_encoder commands) in
    get (Backend.buffer_to_texture blit ~src:texel_upload ~bytes_per_row:256L ~bytes_per_image:256L ~dst:texel
      ~extent:{width=1;height=1;depth=1} ());
    get (Backend.end_blit blit);
    poll_epoch (get (Backend.commit commands)) 1000;
    get (Backend.argument_texture argument argument_buffer ~offset:0L ~slot:0 texel);
    get (Backend.argument_sampler argument argument_buffer ~offset:0L ~slot:1 sampler);
    clear_target 0. 0. 0. 1.;
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
    get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
    get (Backend.set_render_pipeline encoder argument_pipeline);
    get (Backend.set_stage_buffer encoder Vertex ~index:0 offsets);
    get (Backend.set_stage_buffer encoder Fragment ~index:1 argument_buffer);
    get (Backend.use_resources encoder [`Texture texel]);
    get (Backend.draw encoder ~primitive:Triangle_list ~first:0 ~count:3 ());
    get (Backend.end_render encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    expect_pixels "\x40\x80\xc0\xff" "argument-buffer sampled pixels differ";
    (* Direct texture and sampler binding. *)
    let textured=get (Backend.create_render_pipeline device (render_descriptor ~layout:textured_layout fragment_textured "textured_fragment")) in
    clear_target 0. 0. 0. 1.;
    let commands=get (Backend.begin_commands queue) in
    let encoder=get (Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
    get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
    get (Backend.set_render_pipeline encoder textured);
    get (Backend.set_stage_buffer encoder Vertex ~index:0 offsets);
    get (Backend.set_stage_texture encoder Fragment ~index:1 texel);
    get (Backend.set_stage_sampler encoder Fragment ~index:2 sampler);
    get (Backend.draw encoder ~primitive:Triangle_list ~first:0 ~count:3 ());
    get (Backend.end_render encoder);
    poll_epoch (get (Backend.commit commands)) 1000;
    expect_pixels "\x40\x80\xc0\xff" "directly sampled pixels differ";
    (* Depth: a cleared depth attachment at 0.25 with Less rejects a triangle
       at z=0.5 when loaded, and admits it after a clear to 1. *)
    let depth_descriptor : Types.texture_descriptor=
      {label=Some"depth";width=4;height=4;depth=1;mip_levels=1;sample_count=1;format=Rgba8_unorm;usage=[Render_attachment]} in
    let depth=get (Backend.create_depth_texture device depth_descriptor) in
    let depth_shader=render_shader [{name="flat_vertex";stage=Shader.Vertex}]
      [{group=0;binding=0;kind=Shader.Storage_buffer;visibility=[Shader.Vertex]}] in
    let depth_pipeline=get (Backend.create_render_pipeline device
      {(render_descriptor fragment_flat "flat_fragment") with vertex=depth_shader;depth_format=Depth32_float}) in
    let render_depth ~load ~clear expected message=
      clear_target 0. 0. 0. 1.;
      let commands=get (Backend.begin_commands queue) in
      let encoder=get (Backend.render_encoder commands
        {colors=[{texture=target;resolve=None;load=Load;store=Store;clear=(0.,0.,0.,0.)}];
         depth=Some {depth_texture=depth;depth_load=load;depth_store=Store;depth_clear=clear}}) in
      get (Backend.set_viewport encoder {x=0;y=0;width=4;height=4});
      get (Backend.set_render_pipeline encoder depth_pipeline);
      get (Backend.set_depth_state encoder (Some {depth_compare=Less;depth_write=true}));
      get (Backend.set_stage_buffer encoder Vertex ~index:0 offsets);
      set_color encoder 1. 1. 0. 1.;
      get (Backend.draw encoder ~primitive:Triangle_list ~first:0 ~count:3 ());
      get (Backend.end_render encoder);
      poll_epoch (get (Backend.commit commands)) 1000;
      expect_pixels expected message in
    render_depth ~load:Clear ~clear:1. "\xff\xff\x00\xff" "depth clear-to-far rejected the triangle";
    render_depth ~load:Clear ~clear:0. "\x00\x00\x00\xff" "depth clear-to-near admitted the triangle";
    render_depth ~load:Load ~clear:1. "\x00\x00\x00\xff" "depth load ignored the retained near depth";
    (* Multisampling: [Store] with a resolve texture resolves and keeps the
       samples, so a following [Load] pass resolves the same colour; [Discard]
       cannot resolve. *)
    let msaa=get (Backend.create_texture device
      {target_descriptor with label=Some"msaa";sample_count=4;usage=[Render_attachment]}) in
    let msaa_pass load store clear=
      let commands=get (Backend.begin_commands queue) in
      let encoder=get (Backend.render_encoder commands
        {colors=[{texture=msaa;resolve=Some target;load;store;clear}];depth=None}) in
      get (Backend.end_render encoder);
      poll_epoch (get (Backend.commit commands)) 1000 in
    msaa_pass Clear Store (0.125,0.25,0.5,1.);
    expect_pixels "\x20\x40\x80\xff" "stored multisample pass did not resolve";
    clear_target 0. 0. 0. 1.;
    msaa_pass Load Resolve (0.,0.,0.,0.);
    expect_pixels "\x20\x40\x80\xff" "Store with a resolve texture did not keep the samples";
    let commands=get (Backend.begin_commands queue) in
    (match Backend.render_encoder commands
       {colors=[{texture=msaa;resolve=Some target;load=Clear;store=Discard;clear=(0.,0.,0.,1.)}];depth=None} with
     | Error { Error.kind = Invalid_argument; _ } -> ()
     | _ -> failwith "a discarded attachment was allowed to resolve");
    get (Backend.abandon commands);
    get (Backend.destroy_texture msaa);
    get (Backend.destroy_texture depth);
    get (Backend.destroy_pipeline depth_pipeline);
    get (Backend.destroy_pipeline textured);
    get (Backend.destroy_argument argument);
    get (Backend.destroy_buffer argument_buffer);
    get (Backend.destroy_buffer texel_upload);
    get (Backend.destroy_texture texel);
    get (Backend.destroy_pipeline argument_pipeline);
    get (Backend.destroy_icb icb);
    get (Backend.destroy_pipeline indirect);
    get (Backend.destroy_buffer color);
    get (Backend.destroy_buffer indices);
    get (Backend.destroy_buffer arguments);
    get (Backend.destroy_pipeline flat)
  end else begin
    (match Backend.create_render_pipeline device (render_descriptor fragment_flat "flat_fragment") with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported render pipeline was created");
    (match Backend.create_icb device ~max_commands:1 with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported indirect command buffer was created");
    let commands=get (Backend.begin_commands queue) in
    (match Backend.render_encoder commands
       {colors=[{texture=target;resolve=None;load=Clear;store=Store;clear=(0.,0.,0.,1.)}];depth=None} with
     | Error { Error.kind = Unsupported; _ } -> ()
     | _ -> failwith "unsupported render encoder was opened");
    get (Backend.abandon commands)
  end;
  get (Backend.destroy_buffer offsets);
  get (Backend.destroy_texture target);
  get (Backend.destroy_sampler sampler);
  let other_queue = get (Backend.create_queue device) in
  require (Backend.completed_epoch other_queue = 0L)
    "new queue inherited another queue's completion";
  let other_commands=get (Backend.begin_commands other_queue) in
  let other_blit=get (Backend.blit_encoder other_commands) in
  get (Backend.buffer_to_texture other_blit ~src:upload ~bytes_per_row:256L
    ~bytes_per_image:512L ~dst:first ~extent ());
  get (Backend.end_blit other_blit);
  let other = get (Backend.commit other_commands) in
  require (other.epoch = 1L) "queue epochs are not independent";
  let rec poll_other attempts =
    match get (Backend.status other_queue other) with
    | Backend.Completed -> ()
    | Backend.Pending when attempts = 0 ->
        failwith "second queue did not complete"
    | Backend.Pending -> Unix.sleepf 0.001; poll_other (attempts - 1) in
  poll_other 1000;
  get (Backend.destroy_buffer source);
  (match Backend.read_buffer source ~offset:0L ~length:1 with
   | Error { Error.kind = Stale_handle; _ } -> ()
   | _ -> failwith "destroyed buffer remained readable");
  get (Backend.destroy_buffer upload);
  get (Backend.destroy_texture first);
  get (Backend.destroy_texture inset);
  get (Backend.destroy_queue other_queue);
  get (Backend.destroy_queue queue);
  get (Backend.destroy_device device)
