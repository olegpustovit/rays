type error_kind = Invalid_argument | Unsupported | Backend | Resource | Destroyed
type error = { operation:string; kind:error_kind; message:string }
let pp_error formatter value =
  Format.fprintf formatter "%s: %s" value.operation value.message
let fail operation kind message = Error { operation; kind; message }
let backend operation value =
  Error { operation; kind=Backend; message=Ogpu.Error.to_string value }
let resource operation value =
  Error { operation; kind=Resource; message=Format.asprintf "%a" Runtime_resources.pp_error value }

type configuration = { logical_width:int; logical_height:int;
  drawable_width:int; drawable_height:int; title:string;vsync:bool;high_density:bool }
let default_configuration = { logical_width=640; logical_height=480;
  drawable_width=640; drawable_height=480; title="Rays";vsync=true; high_density=true }
type family = Scene_execution.pipeline_family = Scene2 | Scene2_textured | Scene3 | Scene3_points | Scene3_textured | Scene3_shadow |
  Scene3_stencil | Scene3_textured_stencil | Scene3_shadow_stencil | Scene3_world | Ui | Marks
type blend = Ogpu.Pipeline.blend = Replace | Alpha | Add | Multiply | Screen | Subtract
type draw = { family:family; blend:blend; texture:Scene_execution.sampled_texture option;
  auxiliary:Scene_execution.auxiliary_resource option;vertex_attributes:(string*bytes)option;
  samples:int;value:Scene_execution.draw }
type gpu={gpu_device:Ogpu.Backend.device;gpu_queue:Ogpu.Backend.queue;
  gpu_shared:bool;mutable gpu_released:bool}
let gpu_marks_capacity=64
let next_gpu_marks=ref 0
(* A [Marks]-family source: the six attribute buffers of one mark set. A host attribute is
   uploaded into a buffer the source owns; a device attribute is another producer's output,
   bound directly and valid only while that producer's output is current. *)
type marks_source=Host of bytes*int|Device of (unit->Ogpu.Backend.buffer option)*int
type gpu_marks={marks_id:int;marks_gpu:gpu;marks_domain:Domain.id;mutable marks_closed:bool;
  mutable marks_stamp:int64;mutable marks_count:int;
  marks_owned:Ogpu.Backend.buffer option array;marks_last:bytes array;
  mutable marks_device:((unit->Ogpu.Backend.buffer option)*Ogpu.Backend.buffer)list;
  mutable marks_registration:Scene_execution.Private.gpu_vertices option}
let gpu_marks_registry:(int,gpu_marks)Hashtbl.t=Hashtbl.create 16
let valid_gpu_marks value=not value.marks_closed&&not value.marks_gpu.gpu_released&&
  value.marks_domain=Domain.self()&&List.for_all(fun(source,input)->
    match source()with Some current->current==input|None->false)value.marks_device
let gpu_marks_key value=Printf.sprintf"gpu:vertices:marks:%d:%Ld"value.marks_id value.marks_stamp
let unregister_marks value=
  Option.iter Scene_execution.Private.unregister_gpu_vertices value.marks_registration;
  value.marks_registration<-None
let close_gpu_marks value=
  if value.marks_domain<>Domain.self()then invalid_arg"GPU marks closed on another domain";
  if not value.marks_closed then begin
    value.marks_closed<-true;unregister_marks value;value.marks_device<-[];
    Hashtbl.remove gpu_marks_registry value.marks_id;
    Array.iteri(fun i->Option.iter(fun b->ignore(Ogpu.Backend.destroy_buffer b);value.marks_owned.(i)<-None))
      value.marks_owned
  end
module Int_table=Lru.Make(Int)
module Structural_key(T:sig type t end)=struct type t=T.t let equal=(=) let hash=Hashtbl.hash end
(* texture (physical), destination, transform, clip, uv, framebuffer *)
module Quad_key=struct
  type t=Scene_execution.sampled_texture*Scene_command.Render_ir.rect*
    Scene_command.Render_ir.transform*(int*int*int*int)*(float*float*float*float)*(int*int*int*int)
  let equal ((t,d,x,c,u,f):t) ((t',d',x',c',u',f'):t)=t==t'&&d=d'&&x=x'&&c=c'&&u=u'&&f=f'
  let hash ((t,d,x,c,u,f):t)=Hashtbl.hash(t.Scene_execution.key,d,x,c,u,f) end
module Quad_table=Lru.Make(Quad_key)
module Quad_payload_table=Lru.Make(Structural_key(struct
  type t=Scene_command.Render_ir.rect*Scene_command.Render_ir.transform*(int*int*int*int)*(float*float*float*float) end))
type cached_scene2_quad_payload={payload_mesh_key:string;payload_vertices:bytes;payload_indices:bytes}
type resource=Image of Runtime_resources.Image.t|Text of Runtime_resources.Text.t
  |Canvas of Runtime_resources.Canvas.t
type scene2_resource_stamp=
  |Image_stamp of int*Runtime_resources.Image.t*int
  |Text_stamp of int*Runtime_resources.Text.t*int
  |Canvas_stamp of int*Runtime_resources.Canvas.t*int
type cached_scene2_plan={mutable plan_ir_id:int;plan_command_count:int;
  plan_source_bytes:int;plan_density:int;
  plan_extent:int*int*int*int;plan_resources:scene2_resource_stamp list;
  mutable plan_ir:Scene_command.Render_ir.t;plan_draws:draw list}
let prepared_draw ~family ?(blend=Replace) ?texture ?auxiliary ?vertex_attributes ?(samples=1) value =
  {family;blend;texture;auxiliary;vertex_attributes;samples;value}

let default_state viewport scissor = { Scene_execution.viewport; scissor;
  cull=Ogpu.Render_pass.Cull_none; depth_compare=Ogpu.Render_pass.Always;
  depth_write=false; depth_load=Ogpu.Render_pass.Load; depth_clear=1.;
  transform_uniforms=None; stencil_state=None; stencil_load=Ogpu.Render_pass.Load;
  stencil_clear=0 }
let put_float bytes offset value = Bytes.set_int64_le bytes offset (Int64.bits_of_float value)
(* Scene2 keeps affine coefficients as f64 through command lowering.  The
   native execution boundary narrows these values to the six-f32 Metal ABI. *)
let identity_affine_uniforms=let bytes=Bytes.make 24 '\000'in
  Bytes.set_int32_le bytes 0(Int32.bits_of_float 1.);
  Bytes.set_int32_le bytes 16(Int32.bits_of_float 1.);bytes
module Command = Scene_command.Render_ir
let write_affine bytes (transform:Scene_command.Render_ir.transform)=
  let put index value=Bytes.set_int32_le bytes(index*4)(Int32.bits_of_float value)in
  put 0 transform.xx;put 1 transform.yx;put 2 transform.tx;
  put 3 transform.xy;put 4 transform.yy;put 5 transform.ty
let affine_uniforms (transform:Command.transform)=
  if transform.xx=1.&&transform.xy=0.&&transform.yx=0.&&transform.yy=1.&&
    transform.tx=0.&&transform.ty=0. then identity_affine_uniforms else
  let bytes=Bytes.make 24 '\000'in
  write_affine bytes {Scene_command.Render_ir.xx=transform.xx;xy=transform.xy;
    yx=transform.yx;yy=transform.yy;tx=transform.tx;ty=transform.ty};bytes
let identity_transform (transform:Command.transform)=
  transform.xx=1.&&transform.xy=0.&&transform.yx=0.&&transform.yy=1.&&
  transform.tx=0.&&transform.ty=0.
let scene2_vertex_stride=24
let mesh_of_geometry number transform ~viewport clip (geometry:Command.geometry) =
  let count=Array.length geometry.vertices/2 in
  let vertices=Bytes.make(count*scene2_vertex_stride)'\000' in
  for index=0 to count-1 do
    let x=geometry.vertices.(index*2) and y=geometry.vertices.(index*2+1) in
    let offset=index*scene2_vertex_stride in
    put_float vertices offset x;put_float vertices(offset+8)y;
    Bytes.set_int32_le vertices(offset+16)geometry.color
  done;
  let indices=Bytes.create(Array.length geometry.indices*4) in
  Array.iteri(fun index value->Bytes.set_int32_le indices(index*4)(Int32.of_int value))geometry.indices;
  let vx,vy,vw,vh=viewport and sx,sy,sw,sh=clip in
  { family=Scene2;blend=Alpha;texture=None;auxiliary=None;vertex_attributes=None;samples=1; value={Scene_execution.mesh={key=Printf.sprintf "ir-%Ld-%d" 0L number;
      vertices;vertex_count=count;indices;index_count=Array.length geometry.indices;primitive=Triangle_list};
      state={(default_state (vx,vy,vw,vh) (sx,sy,sw,sh))with
        transform_uniforms=(if identity_transform transform then None else Some(affine_uniforms transform))}} }

(* Dense runs share transform, clip, blend, and painter order: pack their
   per-vertex colors directly into one draw. Two passes, O(vertices + indices
   + commands) work and final-buffer storage. *)
let mesh_of_geometry_run number transform ~viewport clip commands first stop =
  let vertex_count=ref 0 and index_count=ref 0 in
  for i=first to stop-1 do match commands.(i) with
    |Scene_command.Render_ir.Geometry g ->
        vertex_count:= !vertex_count+Array.length g.vertices/2;
        index_count:= !index_count+Array.length g.indices
    |_->assert false
  done;
  let vertices=Bytes.make (!vertex_count*scene2_vertex_stride) '\000'
  and indices=Bytes.create (!index_count*4) in
  let vertex_offset=ref 0 and index_offset=ref 0 in
  for i=first to stop-1 do match commands.(i) with
    |Scene_command.Render_ir.Geometry g ->
        let count=Array.length g.vertices/2 in
        for j=0 to count-1 do
          let offset=(!vertex_offset+j)*scene2_vertex_stride in
          put_float vertices offset g.vertices.(j*2);
          put_float vertices (offset+8) g.vertices.(j*2+1);
          Bytes.set_int32_le vertices (offset+16) g.color
        done;
        for j=0 to Array.length g.indices-1 do
          Bytes.set_int32_le indices ((!index_offset+j)*4)
            (Int32.of_int (g.indices.(j)+ !vertex_offset))
        done;
        vertex_offset:= !vertex_offset+count;
        index_offset:= !index_offset+Array.length g.indices
    |_->assert false
  done;
  {family=Scene2;blend=Alpha;texture=None;auxiliary=None;vertex_attributes=None;samples=1;
   value={Scene_execution.mesh={key=Printf.sprintf "ir-run-%d" number;
     vertices;vertex_count= !vertex_count;indices;index_count= !index_count;primitive=Triangle_list};
     state={(default_state viewport clip) with
       transform_uniforms=(if identity_transform transform then None
         else Some (affine_uniforms transform))}}}
let compose_raster (a:Scene_command.Render_ir.transform) (b:Scene_command.Render_ir.transform)=
  if b.xx=1.&&b.xy=0.&&b.yx=0.&&b.yy=1.&&b.tx=0.&&b.ty=0. then a
  else if a.xx=1.&&a.xy=0.&&a.yx=0.&&a.yy=1.&&a.tx=0.&&a.ty=0. then b
  else {Scene_command.Render_ir.xx=a.xx*.b.xx+.a.yx*.b.xy;
  xy=a.xy*.b.xx+.a.yy*.b.xy;yx=a.xx*.b.yx+.a.yx*.b.yy;
  yy=a.xy*.b.yx+.a.yy*.b.yy;tx=a.xx*.b.tx+.a.yx*.b.ty+.a.tx;
  ty=a.xy*.b.tx+.a.yy*.b.ty+.a.ty}

type presentation_facts=Runtime.presentation_facts={logical_width:int;
  logical_height:int;drawable_width:int;drawable_height:int;
  pixel_density:float;}
(* An offscreen target leases its device (see [acquire_device]). *)
type runtime=Window of Runtime.t|Offscreen of Runtime.t*bool
let target=function Window runtime|Offscreen(runtime,_)->runtime
type submission_state=Open|Closed
exception Resource_resolver_raised of exn
type snapshot_cache_entry={mutable snapshot_generation:int;
  snapshot_width:int;snapshot_height:int;
  mutable snapshot_texture:Scene_execution.sampled_texture}
(* key, density *)
module Snapshot_table=Lru.Make(Structural_key(struct type t=string*int end))
module Segment_table=Lru.Make(struct type t=int64 let equal=Int64.equal let hash=Hashtbl.hash end)
type retained_scene2_segment={segment_version:int64;
  segment_density:int;segment_width:int;segment_height:int;
  segment_draws:draw list}
let scene2_plan_byte_capacity=64*1024*1024
let retained_scene2_segment_capacity=256
let retained_scene2_segment_byte_capacity=64*1024*1024
let snapshot_cache_capacity=256
let snapshot_cache_byte_capacity=64*1024*1024
(* Full-window snapshots change frequently and already have bounded double
   buffers in [Image].  Keeping entries below 1 MiB admits UI/text assets but
   deliberately leaves a 640x480 RGBA frame (1,228,800 bytes) transient. *)
let snapshot_cache_entry_byte_capacity=1024*1024
let sampled_texture_bytes (texture:Scene_execution.sampled_texture)=
  if Option.is_some texture.gpu then 0 else Array.fold_left(fun total level->
    let bytes=Bytes.length level.Scene_execution.bytes in
    if total>snapshot_cache_entry_byte_capacity-bytes then
      snapshot_cache_entry_byte_capacity+1 else total+bytes)0 texture.levels
(* The last bytes one UI batch position lowered to. Unchanged content reuses
   them, so the executor's mesh cache hits by physical identity instead of
   hashing every instance each frame. *)
(* [ui_source] is the batch the vertices were last compared with: a published batch is immutable, so
   the same one again is unchanged without comparing its bytes, and [ui_vertices] may be its bytes. *)
type ui_slot={mutable ui_vertices:bytes;mutable ui_indices:bytes;
  mutable ui_affine:bytes;mutable ui_source:bytes}
let ui_slot_capacity=256
type t = { runtime:runtime;
  assets:Runtime_resources.Assets.t; mutable dead:bool;
  snapshots:snapshot_cache_entry Snapshot_table.t;
  scene2_quad_cache:draw Quad_table.t;
  scene2_quad_payload_cache:cached_scene2_quad_payload Quad_payload_table.t;
  scene2_plan_cache:cached_scene2_plan Int_table.t;
  scene2_plan_by_ir:(int,cached_scene2_plan)Hashtbl.t;
  retained_scene2_segments:retained_scene2_segment Segment_table.t;
  mutable retained_scene2_segment_hits:int64;
  mutable retained_scene2_segment_misses:int64;
  mutable submissions:submission list;
  mutable last_step_draws:draw list;
  mutable last_step_prepared:Scene_execution.sampled_draw list;
  mutable scene2_out_slots:draw array;
  mutable scene2_out_list:draw list;
  ui_slots:ui_slot Int_table.t;
  mutable retained_views:retained_view list;
}
(* A 3D layer that kept the same prepared identity on consecutive frames: its draws render once
   into an offscreen target the size of the window and later frames composite that texture, so
   a frame that only changes the UI never shades the view again.  The identity is the key
   (prepared layers are immutable), the window's drawable size is checked on every use, and the
   list is capped, so nothing here can outlive or disagree with the scene it stands for. *)
and retained_view={view_layer:Scene_execution.prepared_scene3;view_drawable:int*int;
  mutable view_target:retained_target option;mutable view_failed:bool}
and retained_target={target_exec:t;target_canvas:Runtime_resources.Canvas.t;
  target_ir:Scene_command.Render_ir.t}
and submission={owner:t;mutable submission_state:submission_state;
  mutable image_leases:Runtime_resources.Image.Private.lease list;
  mutable ui_layers:int}
and batch={batch_owner:submission;batch_draws:draw list}
(* ponytail: one active native window; pass an explicit renderer token if sketches
   start owning multiple windows concurrently. *)
let active_window : t option ref=ref None
(* Device leases: the active window's device when one exists, otherwise one
   lazily created headless device shared by every lease and released with the
   last one. Offscreen executions (Canvas) and GPU film producers both lease. *)
let headless_gpu:(Ogpu.Backend.device*int ref)option ref=ref None
let release_headless()=match !headless_gpu with
  |Some(device,count)->decr count;
      if !count<=0 then(headless_gpu:=None;ignore(Ogpu.Backend.destroy_device device))
  |None->()
(* Live leases of the active window's device; the window refuses to destroy
   that device while any remain. *)
let shared_leases=ref 0
let acquire_device operation=
  let shared=match !active_window with
    |Some{runtime=Window runtime;dead=false;_}->Some(Runtime.device runtime)
    |Some _|None->None in
  match shared with
  |Some(Ok device)->incr shared_leases;Ok(device,true)
  |Some(Error e)->backend operation e
  |None->(match !headless_gpu with
    |Some(device,count)->incr count;Ok(device,false)
    |None->let driver,_=Ogpu.Impl.create_driver()in
        match Ogpu.Backend.create_device driver with
        |Error e->backend operation e
        |Ok device->headless_gpu:=Some(device,ref 1);Ok(device,false))
let release_device shared=if shared then decr shared_leases else release_headless()
type lease_policy=Copy_image_snapshots|Retain_image_snapshots of submission
let valid_configuration operation (configuration:configuration)=
  let positive x=x>0 in
  if not(List.for_all positive[configuration.logical_width;configuration.logical_height;
      configuration.drawable_width;configuration.drawable_height])then
    fail operation Invalid_argument"dimensions must be positive"
  else Ok()
let finish_create runtime=
  let scene2_plan_by_ir=Hashtbl.create 16 in
  {runtime;assets=Runtime_resources.Assets.create();
      dead=false;
      snapshots=Snapshot_table.create snapshot_cache_capacity
        ~byte_capacity:snapshot_cache_byte_capacity;
      scene2_quad_cache=Quad_table.create 1024;
      scene2_quad_payload_cache=Quad_payload_table.create 256;
      scene2_plan_cache=Int_table.create 16~byte_capacity:scene2_plan_byte_capacity
        ~release:(fun _ plan->match Hashtbl.find_opt scene2_plan_by_ir plan.plan_ir_id with
          |Some current when current==plan->Hashtbl.remove scene2_plan_by_ir plan.plan_ir_id
          |_->());
      scene2_plan_by_ir;
      retained_scene2_segments=Segment_table.create retained_scene2_segment_capacity
        ~byte_capacity:retained_scene2_segment_byte_capacity;
      retained_scene2_segment_hits=0L;retained_scene2_segment_misses=0L;
      submissions=[];last_step_draws=[];last_step_prepared=[];
      scene2_out_slots=[||];scene2_out_list=[];ui_slots=Int_table.create ui_slot_capacity;
      retained_views=[]}
let create (configuration:configuration) =
  let operation="Rays_execution.create" in
  match valid_configuration operation configuration with Error _ as error->error|Ok()->
    match Runtime.create ~vsync:configuration.vsync ~hidden:false ~high_density:configuration.high_density
        ~title:configuration.title ~width:configuration.logical_width
        ~height:configuration.logical_height () with
    |Error e->backend operation e
    |Ok runtime->match Runtime.set_resizable runtime true with
      |Error e->ignore(Runtime.destroy runtime);backend operation e
      |Ok()->
        let value=finish_create(Window runtime) in
        active_window:=Some value;Ok value
let create_offscreen (configuration:configuration)=
  let operation="Rays_execution.create_offscreen"in
  match valid_configuration operation configuration with Error _ as error->error|Ok()->
  match acquire_device operation with Error _ as error->error|Ok(device,lease_shared)->
  match Runtime.create_offscreen ~device ~title:configuration.title
      ~logical_width:configuration.logical_width
      ~logical_height:configuration.logical_height
      ~width:configuration.drawable_width
      ~height:configuration.drawable_height () with
  |Error error->release_device lease_shared;backend operation error
  |Ok runtime->Ok(finish_create(Offscreen(runtime,lease_shared)))
let ensure operation value=if value.dead then fail operation Destroyed"coordinator is destroyed"else Ok()
let release_image_leases leases=
  List.iter Runtime_resources.Image.Private.release_snapshot leases
let close_submission submission=
  if submission.submission_state=Open then begin
    submission.submission_state<-Closed;
    release_image_leases submission.image_leases;
    submission.image_leases<-[];
    submission.owner.submissions<-
      List.filter((!=)submission)submission.owner.submissions
  end
let begin_submission value=
  match ensure"Rays_execution.Private.begin_submission"value with
  |Error _ as e->e
  |Ok()->
      let submission={owner=value;submission_state=Open;image_leases=[];ui_layers=0}in
      value.submissions<-submission::value.submissions;
      Ok submission
let ensure_submission operation submission=
  if submission.submission_state=Closed then
    fail operation Invalid_argument"submission is already closed"
  else ensure operation submission.owner
let lease_checkpoint=function
  |Copy_image_snapshots->None
  |Retain_image_snapshots submission->Some submission.image_leases
let rollback_leases policy checkpoint=match policy,checkpoint with
  |Copy_image_snapshots,_->()
  |Retain_image_snapshots submission,Some before->
      let rec release=function
        |leases when leases==before->()
        |lease::rest->
            Runtime_resources.Image.Private.release_snapshot lease;
            release rest
        |[]->()in
      release submission.image_leases;
      submission.image_leases<-before
  |Retain_image_snapshots _,None->assert false
type stats=Runtime.stats
let stats value=match ensure"Rays_execution.stats"value with Error _ as e->e|Ok()->
  Ok(Runtime.stats(target value.runtime))
let presentation_facts value=match ensure"Rays_execution.presentation_facts"value with
  |Error _ as e->e
  |Ok()->match Runtime.presentation_facts(target value.runtime)with
    |Ok _ as facts->facts
    |Error error->backend"Rays_execution.presentation_facts"error
let window operation call value=match ensure operation value with Error _ as e->e|Ok()->
  match value.runtime with
  |Offscreen _->fail operation Unsupported"operation requires a presentation window"
  |Window runtime->Result.map_error(fun error->{operation;kind=Backend;
      message=Ogpu.Error.to_string error})(call runtime)
let show value=window"Rays_execution.show"Runtime.show value
let set_relative_mouse value enabled=window"Rays_execution.set_relative_mouse"
  (fun runtime->Runtime.set_relative_mouse runtime enabled) value
let set_window_background value color=window"Rays_execution.set_window_background"
  (fun runtime->Runtime.set_window_background runtime color)value
let set_cursor value shape=window"Rays_execution.set_cursor"
  (fun runtime->Runtime.set_cursor runtime shape)value
let set_text_input value area=window"Rays_execution.set_text_input"
  (fun runtime->Runtime.set_text_input runtime area)value
let visible value=window"Rays_execution.visible"Runtime.visible value
type dialog_kind=Runtime.dialog_kind=Open_file|Open_files|Save_file|Open_folder
type dialog_filter=Runtime.dialog_filter={name:string;pattern:string}
let show_dialog value ?filters ?default_location kind=window"Rays_execution.show_dialog"
  (fun runtime->Runtime.show_dialog runtime?filters?default_location kind)value
(* A size event reached the input source: the next frame re-reads the window.
   An offscreen target has no window to announce anything. *)
let window_changed value=match value.runtime with
  |Window runtime->Runtime.window_changed runtime
  |Offscreen _->()
let snapshot value ~lease_policy ~density source =
  let operation="Rays_execution.lower_scene2"in
  if density<=0 then fail operation Invalid_argument"density must be positive"else
  let find key generation=
    match Snapshot_table.find value.snapshots(key,density)with
    |entry when entry.snapshot_generation=generation->
        Some(entry.snapshot_width,entry.snapshot_height,entry.snapshot_texture)
    |_->None
    |exception Not_found->None in
  let store key generation width height texture bytes=
    if bytes<=snapshot_cache_entry_byte_capacity then
      Snapshot_table.add value.snapshots~bytes(key,density)
        {snapshot_generation=generation;snapshot_width=width;snapshot_height=height;
         snapshot_texture=texture}
    else Snapshot_table.remove value.snapshots(key,density)in
  let texture_key key generation=
    key^":"^string_of_int density^":"^string_of_int generation in
  let finish ?(copy=true) key generation width height pixels =
    match find key generation with
    |Some cached->Ok cached
    |None->
        let find_key()=match Snapshot_table.find value.snapshots(key,density)with
          |entry when entry.snapshot_width=width&&entry.snapshot_height=height->Some entry
          |_->None
          |exception Not_found->None in
        match find_key()with
        |Some entry->
            entry.snapshot_generation<-generation;
            let bytes=if copy then Bytes.copy pixels else pixels in
            (* The execution key carries the generation: same-key bytes are
               never re-hashed below, and a new generation recycles the
               same-shape texture with one upload. *)
            let texture={entry.snapshot_texture with
              key=texture_key key generation;
              levels=[|{entry.snapshot_texture.levels.(0) with bytes}|]}in
            entry.snapshot_texture<-texture;
            Ok(width,height,texture)
        |None->
        let sampler:Ogpu.Types.sampler_descriptor={label=Some"scene-image";min_filter=Linear;mag_filter=Linear;
          mip_filter=No_mip;address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;max_anisotropy=1}in
        let bytes=if copy then Bytes.copy pixels else pixels in
        let texture:Scene_execution.sampled_texture={key=texture_key key generation;
          levels=[|{width;height;bytes}|];sampler;gpu=None}in
        store key generation width height texture(Bytes.length bytes);
        Ok(width,height,texture)in
  match source with
  |Image image->
      if Runtime_resources.Image.destroyed image then
        fail operation Destroyed"image snapshot is destroyed"
      else let key="image:"^string_of_int(Runtime_resources.Image.identity image)
      and generation=Runtime_resources.Image.generation image in
      (match Runtime_resources.Image.Private.gpu_snapshot image with
      |Error e->resource operation e
      |Ok(Some(width,height,generation,gpu))->
          let sampler:Ogpu.Types.sampler_descriptor={label=Some"scene-image";
            min_filter=Linear;mag_filter=Linear;mip_filter=No_mip;
            address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;
            max_anisotropy=1}in
          Ok(width,height,{Scene_execution.key=key^":"^string_of_int density^":"^
            string_of_int generation;levels=[|{width;height;bytes=Bytes.empty}|];
            sampler;gpu=Some gpu})
      |Ok None->
      (match find key generation with
      |Some cached->Ok cached
      |None->match Runtime_resources.Image.Private.borrow_snapshot image with
      |Ok(width,height,borrowed_generation,pixels,lease)->
          let sampler:Ogpu.Types.sampler_descriptor={label=Some"scene-image";min_filter=Linear;mag_filter=Linear;
            mip_filter=No_mip;address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;max_anisotropy=1}in
          let cacheable=Bytes.length pixels<=snapshot_cache_entry_byte_capacity in
          let bytes=if cacheable then
            Fun.protect
              ~finally:(fun()->Runtime_resources.Image.Private.release_snapshot lease)
              (fun()->Bytes.copy pixels)
          else match lease_policy with
            |Retain_image_snapshots submission->
                submission.image_leases<-lease::submission.image_leases;
                pixels
            |Copy_image_snapshots->
                Fun.protect
                  ~finally:(fun()->Runtime_resources.Image.Private.release_snapshot lease)
                  (fun()->Bytes.copy pixels)in
          (* The generation is part of the GPU texture key: a replaced image
             (same identity, new pixels) then misses the executor's static
             image fast path and re-uploads into its same-shape texture. *)
          let texture={Scene_execution.key=key^":"^string_of_int density^":"^
              string_of_int borrowed_generation;
            levels=[|{width;height;bytes}|];sampler;gpu=None}in
          if cacheable then
            store key borrowed_generation width height texture(Bytes.length bytes);
          Ok(width,height,texture)
      |Error e->resource operation e))
  |Text text->
      (* Text snapshots are immutable and uniquely identified, like images:
         a hit touches no pixels, a miss copies them once. *)
      let key="text:"^string_of_int(Runtime_resources.Text.Private.identity text)
      and generation=Runtime_resources.Text.generation text in
      (match if Runtime_resources.Text.destroyed text then None
         else find key generation with
       |Some cached->Ok cached
       |None->match Runtime_resources.Text.size text,
           Runtime_resources.Text.Private.borrow_pixels text with
        |Ok(width,height),Ok pixels->finish key generation width height pixels
        |Error e,_|_,Error e->resource operation e)
  |Canvas canvas->
      let key="canvas:"^string_of_int(Runtime_resources.Canvas.Private.identity canvas)in
      (* A canvas rendered on this window's device is sampled in place. *)
      let gpu=match value.runtime,Runtime_resources.Canvas.Private.gpu_snapshot canvas with
        |Window runtime,Some(width,height,generation,texture)->
            let device_id,_=Ogpu.Backend.Private.texture_driver_token texture in
            (match Runtime.device runtime with
             |Ok device when Ogpu.Handle.device_id(Ogpu.Backend.device_handle device)=device_id->
                 Some(width,height,generation,texture)
             |_->None)
        |_->None in
      (match gpu with
       |Some(width,height,generation,texture)->
           let sampler:Ogpu.Types.sampler_descriptor={label=Some"scene-image";
             min_filter=Linear;mag_filter=Linear;mip_filter=No_mip;
             address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;
             max_anisotropy=1}in
           Ok(width,height,{Scene_execution.key=key^":"^string_of_int density^":"^
             string_of_int generation;levels=[|{width;height;bytes=Bytes.empty}|];
             sampler;gpu=Some texture})
       |None->
      match Runtime_resources.Canvas.snapshot canvas with
       |Ok(width,height,generation,pixels)->finish~copy:false key generation width height pixels
       |Error e->resource operation e)
(* PXUI instances: one [Ui] draw per Ui_batch batch. Glyphs sit on exact
   physical texels, so every UI texture samples nearest. *)
let ui_sampler:Ogpu.Types.sampler_descriptor={label=Some"ui";min_filter=Nearest;
  mag_filter=Nearest;mip_filter=No_mip;address_u=Clamp_to_edge;
  address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;max_anisotropy=1}
let ui_white_texture={Scene_execution.key="ui-white";
  levels=[|{width=1;height=1;bytes=Bytes.make 4 '\255'}|];sampler=ui_sampler;
  gpu=None}
let ui_indices=ref Bytes.empty
let ui_index_bytes count=
  let needed=count*24 in
  if Bytes.length!ui_indices<needed then begin
    let capacity=max needed(2*Bytes.length!ui_indices)in
    let bytes=Bytes.create capacity in
    (* corners 0 1 2 0 2 3 of each quad, written without a closure or a list per quad *)
    for quad=0 to capacity/24-1 do
      let base=quad*4 and at=quad*24 in
      Bytes.set_int32_le bytes at(Int32.of_int base);
      Bytes.set_int32_le bytes(at+4)(Int32.of_int(base+1));
      Bytes.set_int32_le bytes(at+8)(Int32.of_int(base+2));
      Bytes.set_int32_le bytes(at+12)(Int32.of_int base);
      Bytes.set_int32_le bytes(at+16)(Int32.of_int(base+2));
      Bytes.set_int32_le bytes(at+20)(Int32.of_int(base+3))
    done;
    ui_indices:=bytes
  end;
  (* The table is never written after it is made, so a batch that needs all of it shares it. *)
  if Bytes.length!ui_indices=needed then !ui_indices else Bytes.sub!ui_indices 0 needed
let same_bytes_range source offset target=
  let length=Bytes.length target in
  let rec loop index=index>=length||
    (Int64.equal(Bytes.get_int64_le source(offset+index))
       (Bytes.get_int64_le target index)&&loop(index+8))in
  length mod 8=0&&loop 0
let ui_affine_scratch=Bytes.make 24 '\000'

let lower_scene2_uncached value ~lease_policy ~density ~resource:resolve ir =
  match ensure"Rays_execution.lower_scene2"value with Error _ as e->e|Ok()->
  let checkpoint=lease_checkpoint lease_policy in
  let release_new_leases()=rollback_leases lease_policy checkpoint in
  let identity={Scene_command.Render_ir.xx=1.;xy=0.;yx=0.;yy=1.;tx=0.;ty=0.}in
  let facts=presentation_facts value|>Result.get_ok in
  let native_projection={Scene_command.Render_ir.xx=2./.float facts.logical_width;xy=0.;yx=0.;
    yy=(-2.)/.float facts.logical_height;tx=(-1.);ty=1.} in
  let render_transform transform=compose_raster native_projection transform in
  let framebuffer=(0,0,facts.logical_width,facts.logical_height) in
  let transforms=ref[identity]and clips=ref[framebuffer]
  and blend=ref Alpha and number=ref 0 and failure=ref None in
  let emit draw=
    let draw=if draw.blend= !blend then draw else {draw with blend= !blend} in
    let i= !number in
    if i>=Array.length value.scene2_out_slots then
      value.scene2_out_slots<-Array.append value.scene2_out_slots
        (Array.make(max 8(Array.length value.scene2_out_slots))draw);
    value.scene2_out_slots.(i)<-draw;
    incr number in
  let point transform x y=transform.Scene_command.Render_ir.xx*.x+.transform.yx*.y+.transform.tx,
    transform.xy*.x+.transform.yy*.y+.transform.ty in
  let clip_live()=let _,_,width,height=List.hd!clips in width>0&&height>0 in
  let quad (texture:Scene_execution.sampled_texture)
      (destination:Scene_command.Render_ir.rect) (u0,v0,u1,v1 as uv) =
    let transform=render_transform(List.hd!transforms)in
    let clip=List.hd!clips in
    let quad_key=texture,destination,transform,clip,uv,framebuffer in
    match Quad_table.find value.scene2_quad_cache quad_key with
    |draw->draw
    |exception Not_found->
    let payload_key=destination,transform,clip,uv in
    let vertices,indices,mesh_key=match Quad_payload_table.find value.scene2_quad_payload_cache payload_key with
    |cached->cached.payload_vertices,cached.payload_indices,cached.payload_mesh_key
    |exception Not_found->
    let x0,y0=point transform destination.Scene_command.Render_ir.x destination.y
    and x1,y0'=point transform(destination.x+.destination.width)destination.y
    and x1',y1=point transform(destination.x+.destination.width)(destination.y+.destination.height)
    and x0',y1'=point transform destination.x(destination.y+.destination.height)in
    let vertices=Bytes.make(68*4)'\000'in
    let put index x y u v=let offset=index*68 in put_float vertices offset x;put_float vertices(offset+8)y;
      put_float vertices(offset+40)1.;Bytes.set_int32_le vertices(offset+48)0xffffffffl;put_float vertices(offset+52)u;put_float vertices(offset+60)v in
    put 0 x0 y0 u0 v0;put 1 x1 y0' u1 v0;put 2 x1' y1 u1 v1;put 3 x0' y1' u0 v1;
    let indices=Bytes.create 24 in List.iteri(fun i n->Bytes.set_int32_le indices(i*4)(Int32.of_int n))[0;1;2;0;2;3];
    let cx,cy,cw,ch=clip in
    let mesh_key=Printf.sprintf
      "quad:%.8g,%.8g,%.8g,%.8g:%.8g,%.8g,%.8g,%.8g,%.8g,%.8g:%d,%d,%d,%d:%.8g,%.8g,%.8g,%.8g"
      destination.x destination.y destination.width destination.height
      transform.xx transform.xy transform.yx transform.yy transform.tx transform.ty
      cx cy cw ch u0 v0 u1 v1 in
    Quad_payload_table.add value.scene2_quad_payload_cache payload_key
      {payload_mesh_key=mesh_key;payload_vertices=vertices;payload_indices=indices};
    vertices,indices,mesh_key in
    let draw={family=Scene2_textured;blend=Alpha;texture=Some texture;auxiliary=None;vertex_attributes=None;samples=1;
      value={Scene_execution.mesh={key=mesh_key;vertices;vertex_count=4;indices;index_count=6;primitive=Triangle_list};state=default_state framebuffer clip}}in
    if sampled_texture_bytes texture<=snapshot_cache_entry_byte_capacity then
      Quad_table.add value.scene2_quad_cache quad_key draw;
    draw in
  let image (command:Scene_command.Render_ir.image) = match resolve command.Scene_command.Render_ir.resource_id with None->failure:=Some"resource id is unbound"|Some source->
    match snapshot value~lease_policy~density source with Error e->failure:=Some(Format.asprintf"%a"pp_error e)|Ok(width,height,texture)->
      let s=command.source in if width<=0||height<=0 then failure:=Some"resource extent is invalid"else
      let u0=s.x/.float width and v0=s.y/.float height and u1=(s.x+.s.width)/.float width and v1=(s.y+.s.height)/.float height in
      let draw=quad texture command.destination(u0,v0,u1,v1)in
      emit draw in
  let commands=Scene_command.Render_ir.Private.commands_readonly ir in
  let cursor=ref 0 in
  while !cursor<Array.length commands && !failure=None do
    let first= !cursor in
    let command=commands.(first) in
    incr cursor;
    (match command with
    |Scene_command.Render_ir.Clear _->()
    |Set_blend mode->blend:=(match mode with Scene_command.Render_ir.Replace->Replace
        |Copy->Replace|Alpha|Source_over->Alpha|Add->Add|Multiply->Multiply
        |Screen->Screen|Subtract->Subtract)
    |Push_transform transform->transforms:=compose_raster(List.hd!transforms)transform::!transforms
    |Pop_transform->(match!transforms with _::(_::_ as rest)->transforms:=rest|_->())
    |Push_clip rect->let px,py,pw,ph=List.hd!clips and x=int_of_float(floor rect.x)
      and y=int_of_float(floor rect.y)and right=int_of_float(ceil(rect.x+.rect.width))
      and bottom=int_of_float(ceil(rect.y+.rect.height))in
      let x=min(px+pw)(max px x)and y=min(py+ph)(max py y)in
      let right=min(px+pw)right and bottom=min(py+ph)bottom in
      clips:=(x,y,max 0(right-x),max 0(bottom-y))::!clips
    |Pop_clip->(match!clips with _::(_::_ as rest)->clips:=rest|_->())
    |Shapes shapes->
        let gpu_key=match Scene_command.Shape_batch.gpu shapes with
          |None->None
          |Some token->
            let identity=Scene_command.Shape_batch.Private.gpu_identity token in
            let runtime=match value.runtime with Window r|Offscreen(r,_)->r in
            (match Hashtbl.find_opt gpu_marks_registry identity with
            |Some source when valid_gpu_marks source&&
              source.marks_stamp=Scene_command.Shape_batch.Private.gpu_stamp token&&
              source.marks_count=Scene_command.Shape_batch.Private.gpu_count token->
                (match Runtime.device runtime with
                |Ok device when device==source.marks_gpu.gpu_device->Some(gpu_marks_key source)
                |_->failure:=Some"GPU shapes belong to another device";None)
            |_->failure:=Some"GPU shapes are stale or closed";None)in
        if !failure=None&&clip_live() && Scene_command.Shape_batch.count shapes>0 then begin
          let count=Scene_command.Shape_batch.count shapes in
          let instances=Scene_command.Shape_batch.instances shapes in
          let slot_key=(-1)- !number in
          let slot=match Int_table.find value.ui_slots slot_key with
            |slot->slot
            |exception Not_found->
                let slot={ui_vertices=Bytes.empty;ui_indices=Bytes.empty;ui_affine=Bytes.empty;ui_source=Bytes.empty}in
                Int_table.add value.ui_slots slot_key slot;slot in
          if slot.ui_source!=instances then begin
            (* changed content takes the batch's own bytes (immutable once published), not a copy *)
            if Bytes.length slot.ui_vertices<>Bytes.length instances ||
               not(Bytes.equal slot.ui_vertices instances)then
              slot.ui_vertices<-instances;
            slot.ui_source<-instances
          end;
          if Bytes.length slot.ui_indices<>count*24 then
            slot.ui_indices<-ui_index_bytes count;
          write_affine ui_affine_scratch(render_transform(List.hd !transforms));
          if not(Bytes.equal slot.ui_affine ui_affine_scratch)then
            slot.ui_affine<-Bytes.copy ui_affine_scratch;
          let family=if gpu_key=None then Ui else Marks in
          emit {family;blend= !blend;texture=(if family=Marks then None else Some ui_white_texture);
            auxiliary=None;vertex_attributes=None;samples=1;
            value={Scene_execution.mesh={key=(match gpu_key with Some key->key|None->"shapes:"^string_of_int slot_key);
              vertices=(match gpu_key with None->slot.ui_vertices|Some _->Bytes.empty);
              vertex_count=4*count;indices=slot.ui_indices;
              index_count=6*count;primitive=Triangle_list};
              state={(default_state framebuffer(List.hd !clips))with
                transform_uniforms=Some slot.ui_affine}}}
        end
    |Geometry geometry->
        (* A run of more than 64 geometries shares transform, clip, and blend:
           pack it into one draw. Shorter runs keep one draw per geometry so a
           moving member does not re-upload its unchanged neighbours. *)
        let stop=ref !cursor in
        while !stop<Array.length commands &&
          (match commands.(!stop) with Geometry _->true|_->false) do incr stop done;
        if !stop-first>64 then begin
          cursor:= !stop;
          if clip_live() then emit (mesh_of_geometry_run !number
            (render_transform (List.hd !transforms))
            ~viewport:framebuffer (List.hd !clips) commands first !stop)
        end else if clip_live() then
          emit (mesh_of_geometry !number (render_transform (List.hd !transforms))
            ~viewport:framebuffer (List.hd !clips) geometry)
    |Image command->if clip_live()then image command
    |Glyphs glyphs->if clip_live()&&Array.length glyphs.glyphs>0 then match resolve glyphs.resource_id with None->failure:=Some"glyph resource id is unbound"|Some source->
        match snapshot value~lease_policy~density source with Error e->failure:=Some(Format.asprintf"%a"pp_error e)|Ok(width,height,texture)->
          Array.iter(fun(glyph:Scene_command.Render_ir.glyph)->
            let destination={Scene_command.Render_ir.x=glyph.x;y=glyph.y;width=float width;height=float height}in
            let draw=quad texture destination(0.,0.,1.,1.)in
            emit draw)glyphs.glyphs)
  done;
  match!failure with Some message->release_new_leases();fail"Rays_execution.lower_scene2"Resource message
  |None->
      let n= !number in
      let rec prefix_same i=function
        |[]->i=n
        |d::rest->i<n&&value.scene2_out_slots.(i)==d&&prefix_same(i+1)rest in
      if prefix_same 0 value.scene2_out_list then Ok value.scene2_out_list
      else
        let rec loop i acc=if i<0 then acc else loop(i-1)(value.scene2_out_slots.(i)::acc)in
        let list=loop(n-1)[]in
        value.scene2_out_list<-list;
        Ok list
let scene2_plan_source_bytes commands=
  Array.fold_left(fun total->function
    |Scene_command.Render_ir.Geometry g->total+Array.length g.vertices*(Sys.word_size/8)+
      Array.length g.indices*(Sys.word_size/8)
    |Glyphs g->total+Array.length g.glyphs*24
    |Shapes s->total+Bytes.length(Scene_command.Shape_batch.instances s)
    |_->total+32)0 commands
let same_scene2_resource_stamps left right=
  let rec loop left right=match left,right with
  |[],[]->true
  |Text_stamp(id,a,g)::xs,Text_stamp(id',b,g')::ys->
      id=id'&&a==b&&g=g'&&loop xs ys
  |Image_stamp(id,a,g)::xs,Image_stamp(id',b,g')::ys->
      id=id'&&a==b&&g=g'&&loop xs ys
  |Canvas_stamp(id,a,g)::xs,Canvas_stamp(id',b,g')::ys->
      id=id'&&a==b&&g=g'&&loop xs ys
  |_->false in loop left right
let scene2_resource_stamps resolve commands=
  let ids=ref[]and missing=ref false in
  Array.iter(function
    |Scene_command.Render_ir.Image image->ids:=image.resource_id::!ids
    |Glyphs glyphs->ids:=glyphs.resource_id::!ids|_->())commands;
  let stamps=List.filter_map(fun id->
    match resolve id with
    |None->missing:=true;None
    |Some(Image image)->if Result.is_error(Runtime_resources.Image.Private.gpu_snapshot image)then(
        missing:=true;None)else
      Some(Image_stamp(id,image,Runtime_resources.Image.generation image))
    |Some(Text text)->if Runtime_resources.Text.destroyed text then(
        missing:=true;None)else
      Some(Text_stamp(id,text,Runtime_resources.Text.generation text))
    |Some(Canvas canvas)->if Runtime_resources.Canvas.destroyed canvas then(
        missing:=true;None)else
      Some(Canvas_stamp(id,canvas,Runtime_resources.Canvas.generation canvas)))
    (List.sort_uniq Int.compare!ids)in
  not!missing,stamps
(* A plan holding a GPU shape token is lowered again every time: the token's
   source can go stale, and only lowering checks that. *)
let has_gpu_shapes commands=Array.exists(function
  |Scene_command.Render_ir.Shapes shapes->Scene_command.Shape_batch.gpu shapes<>None
  |_->false)commands
let lower_scene2_with_policy value ~lease_policy ~density ~resource:resolve ir =
  if value.dead then lower_scene2_uncached value~lease_policy~density~resource:resolve ir else
  let commands=Scene_command.Render_ir.Private.commands_readonly ir in
  let ir_id=Scene_command.Render_ir.Private.identity ir in
  let facts=presentation_facts value|>Result.get_ok in
  let extent=facts.logical_width,facts.logical_height,
    facts.drawable_width,facts.drawable_height in
  let same_context plan=plan.plan_density=density&&plan.plan_extent=extent in
  let slow ()=
    let fingerprint=ref(Hashtbl.hash commands)and command_count=Array.length commands in
    let mix value=fingerprint:=(!fingerprint*65599)lxor value in
    for index=0 to command_count-1 do match Array.unsafe_get commands index with
      |Scene_command.Render_ir.Push_transform transform->
        mix(Int64.to_int(Int64.bits_of_float transform.xx));
        mix(Int64.to_int(Int64.bits_of_float transform.xy));
        mix(Int64.to_int(Int64.bits_of_float transform.yx));
        mix(Int64.to_int(Int64.bits_of_float transform.yy));
        mix(Int64.to_int(Int64.bits_of_float transform.tx));
        mix(Int64.to_int(Int64.bits_of_float transform.ty))
      |Geometry geometry->mix(Hashtbl.hash geometry.vertices);
        mix(Hashtbl.hash geometry.indices);mix(Int32.to_int geometry.color)
      |_->()
    done;
    let fingerprint= !fingerprint in
    let cacheable,resources=scene2_resource_stamps resolve commands in
    let exact plan=
      plan.plan_command_count=command_count&&same_context plan&&
      same_scene2_resource_stamps plan.plan_resources resources&&
      Scene_command.Render_ir.Private.commands_readonly plan.plan_ir=commands in
    let rebind plan=
      if plan.plan_ir_id<>ir_id then begin
        (match Hashtbl.find_opt value.scene2_plan_by_ir plan.plan_ir_id with
         |Some current when current==plan->Hashtbl.remove value.scene2_plan_by_ir plan.plan_ir_id
         |_->());
        plan.plan_ir_id<-ir_id;plan.plan_ir<-ir;
        Hashtbl.replace value.scene2_plan_by_ir ir_id plan
      end in
    let hit=match Int_table.find value.scene2_plan_cache fingerprint with
      |plan when exact plan->Some plan|_->None|exception Not_found->None in
    if cacheable then match hit with
    |Some plan->rebind plan;Ok plan.plan_draws
    |None->
      (match lower_scene2_uncached value~lease_policy~density~resource:resolve ir with
      |Error _ as error->error
      |Ok draws as result->
        if List.exists(fun draw->match draw.texture with
          |Some texture->sampled_texture_bytes texture>snapshot_cache_entry_byte_capacity
          |None->false)draws||has_gpu_shapes commands then result else
        let plan={plan_ir_id=ir_id;
          plan_command_count=command_count;
          plan_source_bytes=scene2_plan_source_bytes commands;plan_density=density;
          plan_extent=extent;plan_resources=resources;
          plan_ir=ir;plan_draws=draws}in
        Int_table.add value.scene2_plan_cache~bytes:plan.plan_source_bytes fingerprint plan;
        Hashtbl.replace value.scene2_plan_by_ir ir_id plan;
        result)
    else lower_scene2_uncached value~lease_policy~density~resource:resolve ir in
  match Hashtbl.find_opt value.scene2_plan_by_ir ir_id with
  |Some plan when same_context plan&&plan.plan_resources=[]->Ok plan.plan_draws
  |Some plan when same_context plan->let cacheable,resources=scene2_resource_stamps resolve commands in
      if cacheable&&same_scene2_resource_stamps plan.plan_resources resources
      then Ok plan.plan_draws else slow()
  |_->slow()
let lower_scene2 value ~density ~resource ir=
  lower_scene2_with_policy value~lease_policy:Copy_image_snapshots
    ~density~resource ir
let replay_step ?clear ~identity ~version value=
  match ensure"Rays_execution.Private.replay"value with
  |Error _ as error->error
  |Ok()->
      match Runtime.replay_prepared_sampled_resources ?clear ~identity ~version
          (target value.runtime) with
      |Error error->backend"Rays_execution.Private.replay"error
      |Ok None->Ok None
      |Ok(Some _)->Ok(Some())
let step_core ?after_prepare ?clear ?identity ?version value draws=
  let after_prepare=Option.value after_prepare ~default:Fun.id in
  match ensure"Rays_execution.step"value with Error _ as e->e|Ok()->
  match presentation_facts value with Error _ as error->after_prepare();error|Ok f->
    let runtime=target value.runtime in
    let replayed=match identity,version with
      |Some identity,Some version->
          Runtime.replay_prepared_sampled_resources ?clear ~identity ~version runtime
      |_->Ok None in
    let rendered=match replayed with
    |Error error->after_prepare();Error error
    |Ok(Some presented)->after_prepare();Ok presented
    |Ok None->
    let rec same_draws left right=match left,right with
      |[],[]->true|x::xs,y::ys->x==y&&same_draws xs ys|_->false in
    let draws=
      if same_draws draws value.last_step_draws then value.last_step_prepared
      else
        let prepared=List.map(fun x->let draw=x.value in let state=draw.Scene_execution.state in
          let viewport=match state.viewport with _,_,w,h when w<0||h<0->0,0,f.logical_width,f.logical_height|x->x in
          let scissor=match state.scissor with _,_,w,h when w<0||h<0->0,0,f.logical_width,f.logical_height|x->x in
          let draw=if viewport=state.viewport&&scissor=state.scissor then draw else
            {draw with Scene_execution.state={state with viewport;scissor}}in
          {Scene_execution.family=x.family;blend=x.blend;texture=x.texture;
            auxiliary=x.auxiliary;vertex_attributes=x.vertex_attributes;samples=x.samples;draw})draws in
        value.last_step_draws<-draws;value.last_step_prepared<-prepared;prepared in
    match identity,version with
    |None,None->Runtime.render_sampled_resources ~after_prepare ?clear runtime draws
    |Some identity,Some version->Runtime.render_prepared_sampled_resources ~after_prepare ?clear
        ~identity~version runtime draws
    |_->after_prepare();Error(Ogpu.Error.make"Rays_execution.step"Ogpu.Error.Invalid_argument
        "prepared identity and version must be supplied together")in
    match rendered with Error e->backend"Rays_execution.step"e
    |Ok _->Ok()
let step ?clear value draws=step_core ?clear value draws
let lower_scene2_submission submission ~density ~resource ir=
  match ensure_submission"Rays_execution.Private.lower_scene2"submission with
  |Error _ as e->close_submission submission;e
  |Ok()->
      let protected_resource id=try resource id with
        |(Out_of_memory|Stack_overflow|Sys.Break)as exn->raise exn
        |exn->raise(Resource_resolver_raised exn)in
      (try match lower_scene2_with_policy submission.owner
          ~lease_policy:(Retain_image_snapshots submission)~density
          ~resource:protected_resource ir with
       |Ok draws->Ok{batch_owner=submission;batch_draws=draws}
       |Error _ as error->close_submission submission;error
       with Resource_resolver_raised exn->
         close_submission submission;
         fail"Rays_execution.Private.lower_scene2"Resource
           ("resource resolver raised: "^Printexc.to_string exn)
       |exn->close_submission submission;raise exn)
let retained_scene2_segment_bytes draws=List.fold_left(fun total draw->
  let value=draw.value in
  total+Bytes.length value.Scene_execution.mesh.vertices+
    Bytes.length value.mesh.indices+
    Option.fold~none:0~some:Bytes.length value.state.transform_uniforms+
    Option.fold~none:0~some:sampled_texture_bytes draw.texture+
    Option.fold~none:0~some:(fun auxiliary->
      Bytes.length auxiliary.Scene_execution.buffer+
      sampled_texture_bytes auxiliary.texture)draw.auxiliary)0 draws
let lower_scene2_segment submission ~identity ~version ~cacheable ~density
    ~resource ir=
  let operation="Rays_execution.Private.lower_scene2_segment"in
  if identity<=0L||version<0L then begin
    close_submission submission;
    fail operation Invalid_argument"segment identity/version is invalid"
  end else match ensure_submission operation submission with
  |Error _ as error->close_submission submission;error
  |Ok()->
      let owner=submission.owner in
      match presentation_facts owner with
      |Error _ as error->close_submission submission;error
      |Ok facts->
      let find()=match Segment_table.find owner.retained_scene2_segments identity with
      |entry when entry.segment_version=version&&entry.segment_density=density&&
          entry.segment_width=facts.logical_width&&
          entry.segment_height=facts.logical_height->
          Some entry.segment_draws
      |_->None
      |exception Not_found->None in
      match if cacheable then find() else None with
      |Some draws->
          owner.retained_scene2_segment_hits<-
            Int64.succ owner.retained_scene2_segment_hits;
          Ok{batch_owner=submission;batch_draws=draws}
      |None->match lower_scene2_submission submission~density~resource ir with
        |Error _ as error->error
        |Ok batch->
            if cacheable then owner.retained_scene2_segment_misses<-
              Int64.succ owner.retained_scene2_segment_misses;
            if cacheable then begin
              let entry={segment_version=version;
                segment_density=density;segment_width=facts.logical_width;
                segment_height=facts.logical_height;
                segment_draws=batch.batch_draws}in
              Segment_table.add owner.retained_scene2_segments
                ~bytes:(retained_scene2_segment_bytes batch.batch_draws)identity entry
            end;
            Ok batch
let lower_ui submission ~density ~resource ui=
  let operation="Rays_execution.Private.lower_ui"in
  match ensure_submission operation submission with
  |Error _ as error->close_submission submission;error
  |Ok()->
  match presentation_facts submission.owner with
  |Error _ as error->close_submission submission;error
  |Ok facts->
  let width=float facts.logical_width and height=float facts.logical_height in
  let framebuffer=0,0,facts.logical_width,facts.logical_height in
  let instances=Scene_command.Ui_batch.instances ui in
  let texture id=
    if id<=0 then Ok ui_white_texture else match resource id with
    |None->fail operation Resource"UI texture resource id is unbound"
    |Some source->
        match snapshot submission.owner
          ~lease_policy:(Retain_image_snapshots submission)~density source with
        |Error _ as error->error
        |Ok(_,_,texture)->Ok{texture with Scene_execution.sampler=ui_sampler}in
  let layer=submission.ui_layers in
  submission.ui_layers<-layer+1;
  let slots=submission.owner.ui_slots in
  let draw index (batch:Scene_command.Ui_batch.batch)=
    Result.map(fun texture->
      write_affine ui_affine_scratch{Scene_command.Render_ir.xx=2./.width;
        xy=0.;yx=0.;yy=(-2.)/.height;tx=(-1.);ty=1.};
      let first=batch.first*Scene_command.Ui_batch.instance_bytes
      and length=batch.count*Scene_command.Ui_batch.instance_bytes in
      let slot_key=layer*65536+index in
      let slot=match Int_table.find slots slot_key with
        |slot->slot
        |exception Not_found->
            let slot={ui_vertices=Bytes.empty;ui_indices=Bytes.empty;
              ui_affine=Bytes.empty;ui_source=Bytes.empty}in
            Int_table.add slots slot_key slot;slot in
      if Bytes.length slot.ui_vertices<>length||
         not(same_bytes_range instances first slot.ui_vertices)then
        slot.ui_vertices<-Bytes.sub instances first length;
      if Bytes.length slot.ui_indices<>batch.count*24 then
        slot.ui_indices<-ui_index_bytes batch.count;
      if not(Bytes.equal slot.ui_affine ui_affine_scratch)then
        slot.ui_affine<-Bytes.copy ui_affine_scratch;
      let scissor=match batch.clip with
        |None->framebuffer
        |Some clip->
            let x=max 0(int_of_float(Float.floor clip.x))
            and y=max 0(int_of_float(Float.floor clip.y))in
            let right=min facts.logical_width
                (int_of_float(Float.ceil(clip.x+.clip.width)))
            and bottom=min facts.logical_height
                (int_of_float(Float.ceil(clip.y+.clip.height)))in
            x,y,max 0(right-x),max 0(bottom-y)in
      {family=Ui;blend=Alpha;texture=Some texture;auxiliary=None;vertex_attributes=None;samples=1;
       value={Scene_execution.mesh={key="ui:"^string_of_int slot_key;
         vertices=slot.ui_vertices;vertex_count=4*batch.count;
         indices=slot.ui_indices;index_count=6*batch.count;
         primitive=Ogpu.Render_pass.Triangle_list};
         state={(default_state framebuffer scissor)with
           transform_uniforms=Some slot.ui_affine}}})(texture batch.texture)in
  let batches=Scene_command.Ui_batch.batches ui in
  let rec build index reversed=
    if index=Array.length batches then Ok(List.rev reversed) else
    let batch=batches.(index)in
    let _,_,scissor_width,scissor_height=match batch.clip with
      |None->framebuffer
      |Some clip->0,0,int_of_float clip.width,int_of_float clip.height in
    if batch.count=0||scissor_width<=0||scissor_height<=0
    then build(index+1)reversed
    else match draw index batch with
      |Error _ as error->error
      |Ok value->build(index+1)(value::reversed)in
  match build 0[]with
  |Error _ as error->close_submission submission;error
  |Ok draws->Ok{batch_owner=submission;batch_draws=draws}
let adopt_draws submission draws=
  match ensure_submission"Rays_execution.Private.adopt_draws"submission with
  |Error _ as error->close_submission submission;error
  |Ok()->Ok{batch_owner=submission;batch_draws=draws}
let step_submission ?clear ?identity ?version submission batches=
  match ensure_submission"Rays_execution.Private.step"submission with
  |Error _ as e->close_submission submission;e
  |Ok()->Fun.protect~finally:(fun()->close_submission submission)
      (fun()->
        if List.exists(fun batch->batch.batch_owner!=submission)batches then
          fail"Rays_execution.Private.step"Invalid_argument
            "draw batch belongs to another submission"
        else
          let draws=match batches with
            |[batch]->batch.batch_draws
            |_->List.rev(List.fold_left(fun reversed batch->
                List.rev_append batch.batch_draws reversed)[]batches)in
          step_core ~after_prepare:(fun()->close_submission submission)
            ?clear ?identity ?version submission.owner draws)
let capture value=match ensure"Rays_execution.capture"value with Error _ as e->e|Ok()->
  match presentation_facts value with Error _ as error->error|Ok facts->
  match Runtime.read_pixels(target value.runtime)
      ~bytes_per_row:(facts.drawable_width*4)with
  |Ok x->Ok x|Error e->backend"Rays_execution.capture"e
let offscreen_target value=
  let operation="Rays_execution.offscreen_target"in
  match ensure operation value with Error _ as e->e|Ok()->
  match value.runtime with
  |Offscreen(runtime,_)->(match Runtime.target runtime with
      |Ok texture->Ok texture|Error e->backend operation e)
  |Window _->fail operation Unsupported"operation requires an offscreen execution"
let rec destroy value=if value.dead then Ok()else
  let () = release_views value in
  match value.runtime with
  |Window _ when !shared_leases>0->
      fail"Rays_execution.destroy"Invalid_argument
        (string_of_int !shared_leases^
         " GPU lease(s) still share this window's device; release them first")
  |Window _|Offscreen _->(
  List.iter close_submission value.submissions;
  match Runtime_resources.Assets.destroy value.assets with Error e->resource"Rays_execution.destroy"e|Ok()->
    Snapshot_table.clear value.snapshots;
    Quad_table.clear value.scene2_quad_cache;
    Quad_payload_table.clear value.scene2_quad_payload_cache;
    Int_table.clear value.scene2_plan_cache;Hashtbl.reset value.scene2_plan_by_ir;
    Segment_table.clear value.retained_scene2_segments;
    value.last_step_draws<-[];
    value.last_step_prepared<-[];value.scene2_out_slots<-[||];
    value.scene2_out_list<-[];Int_table.clear value.ui_slots;value.dead<-true;
    (match !active_window with Some current when current==value->active_window:=None|_->());
    let destroyed=Runtime.destroy(target value.runtime)in
    (match value.runtime with
     |Offscreen(_,lease_shared)->release_device lease_shared|Window _->());
    match destroyed with Ok()->Ok()|Error e->backend"Rays_execution.destroy"e)
and release_view view=match view.view_target with
  |Some target->
      view.view_target<-None;
      ignore(Runtime_resources.Canvas.destroy target.target_canvas);
      ignore(destroy target.target_exec)
  |None->()
and release_views value=
  let views=value.retained_views in
  value.retained_views<-[];
  List.iter release_view views
let retained_view_capacity=4
(* The first sight of a layer draws it directly; the second renders it offscreen. [None] when the
   target cannot be made: the view then stays direct for as long as the layer does. *)
let make_retained_target (f:presentation_facts) draws=
  let dw=f.drawable_width and dh=f.drawable_height in
  match create_offscreen{logical_width=f.logical_width;logical_height=f.logical_height;
      drawable_width=dw;drawable_height=dh;title="Rays retained view";vsync=false;high_density=true}with
  |Error _->None
  |Ok exec->
      let fail_with canvas=
        Option.iter(fun c->ignore(Runtime_resources.Canvas.destroy c))canvas;
        ignore(destroy exec);None in
      match step~clear:(0.,0.,0.,0.)exec draws,offscreen_target exec,
          Runtime_resources.Canvas.create~width:dw~height:dh with
      |Ok(),Ok texture,Ok canvas->
          (match Runtime_resources.Canvas.Private.publish_gpu canvas texture,
              Scene_command.Render_ir.create[|Scene_command.Render_ir.Image{resource_id=1;
                source={x=0.;y=0.;width=float dw;height=float dh};
                destination={x=0.;y=0.;width=float f.logical_width;height=float f.logical_height}}|]with
          |Ok(),Ok ir->Some{target_exec=exec;target_canvas=canvas;target_ir=ir}
          |_->fail_with(Some canvas))
      |_,_,Ok canvas->fail_with(Some canvas)
      |_->fail_with None
let adopt_retained_view submission ~density ~layer draws=
  let value=submission.owner in
  let direct()=adopt_draws submission draws in
  match value.runtime with
  |Offscreen _->direct()
  |Window _->
  match presentation_facts value with
  |Error _->direct()
  |Ok f->
  let drawable=f.drawable_width,f.drawable_height in
  let current=match List.find_opt(fun view->view.view_layer==layer)value.retained_views with
    |Some view when view.view_drawable=drawable->Some view
    |Some view->
        release_view view;
        value.retained_views<-List.filter((!=)view)value.retained_views;None
    |None->None in
  match current with
  |None->
      let kept=List.filteri(fun index _->index<retained_view_capacity-1)value.retained_views in
      List.iter(fun view->if not(List.memq view kept)then release_view view)value.retained_views;
      value.retained_views<-{view_layer=layer;view_drawable=drawable;view_target=None;
        view_failed=false}::kept;
      direct()
  |Some view->
      (match view.view_target with
       |None when not view.view_failed->
           (match make_retained_target f draws with
            |Some target->view.view_target<-Some target
            |None->view.view_failed<-true)
       |_->());
      match view.view_target with
      |None->direct()
      |Some target->
          lower_scene2_submission submission ~density
            ~resource:(function 1->Some(Canvas target.target_canvas)|_->None)
            target.target_ir
(* GPU film leases own their own queue, so their frame pacing never couples
   with presentation. *)
let acquire_gpu()=
  let operation="Rays_execution.acquire_gpu"in
  match acquire_device operation with
  |Error _ as e->e
  |Ok(device,gpu_shared)->
      match Ogpu.Backend.create_queue device with
      |Error e->release_device gpu_shared;backend operation e
      |Ok queue->Ok{gpu_device=device;gpu_queue=queue;gpu_shared;gpu_released=false}
let gpu_device gpu=gpu.gpu_device
let gpu_queue gpu=gpu.gpu_queue
let gpu_shared gpu=gpu.gpu_shared
let release_gpu gpu=if not gpu.gpu_released then begin
  List.iter close_gpu_marks(Hashtbl.fold(fun _ value all->if value.marks_gpu==gpu then value::all else all)
    gpu_marks_registry[]);
  gpu.gpu_released<-true;
  ignore(Ogpu.Backend.destroy_queue gpu.gpu_queue);
  release_device gpu.gpu_shared
end
module Private=struct
  type nonrec gpu_marks=gpu_marks
  type nonrec marks_source=marks_source=Host of bytes*int|Device of (unit->Ogpu.Backend.buffer option)*int
  let create_gpu_marks gpu=
    let operation="Rays_execution.create_gpu_marks"in
    if gpu.gpu_released then fail operation Destroyed"GPU lease is released"
    else if Hashtbl.length gpu_marks_registry>=gpu_marks_capacity|| !next_gpu_marks=max_int then
      fail operation Resource"GPU marks capacity reached"
    else begin
      incr next_gpu_marks;
      let value={marks_id= !next_gpu_marks;marks_gpu=gpu;marks_domain=Domain.self();marks_closed=false;
        marks_stamp=0L;marks_count=0;marks_owned=Array.make 6 None;marks_last=Array.make 6 Bytes.empty;
        marks_device=[];marks_registration=None}in
      Hashtbl.add gpu_marks_registry value.marks_id value;Ok value
    end
  let gpu_marks ?indirect value ~kind ~count attributes=
    let operation="Rays_execution.gpu_marks"in
    if value.marks_closed||value.marks_gpu.gpu_released then
      fail operation Destroyed"GPU marks or lease is closed"
    else if value.marks_domain<>Domain.self()then
      fail operation Invalid_argument"GPU marks used on another domain"
    else if count<=0||count>1_000_000||Array.length attributes<>5 then
      fail operation Invalid_argument"a mark set has one to 1,000,000 instances and five attributes"
    else begin
      let changed=ref(value.marks_registration=None||value.marks_count<>count)in
      let device=ref[]in
      let stale=ref false in
      let upload index bytes=
        let owned=value.marks_owned.(index)in
        let size=Int64.of_int(max 16(Bytes.length bytes))in
        let buffer=match owned with
          |Some b when Ogpu.Backend.buffer_size b>=size->Ok b
          |_->
            let capacity=Int64.of_int(max 16(2*Bytes.length bytes))in
            Result.map(fun b->
              Option.iter(fun old->ignore(Ogpu.Backend.destroy_buffer old))owned;
              value.marks_owned.(index)<-Some b;value.marks_last.(index)<-Bytes.empty;changed:=true;b)
              (Ogpu.Backend.create_buffer value.marks_gpu.gpu_device
                {label=Some"Marks attribute";size=capacity;usage=[Vertex;Storage;Copy_dst]})in
        Result.bind(Result.map_error(fun e->{operation;kind=Backend;message=Ogpu.Error.to_string e})buffer)(fun b->
          if value.marks_last.(index)==bytes||Bytes.equal value.marks_last.(index)bytes then Ok b
          else Result.map(fun()->value.marks_last.(index)<-bytes;changed:=true;b)
            (Result.map_error(fun e->{operation;kind=Backend;message=Ogpu.Error.to_string e})
              (Ogpu.Backend.write_buffer b~offset:0L bytes))) in
      let strides=Array.make 5 0 in
      let rec bind index=if index=5 then Ok[]else begin
        let buffer=match attributes.(index)with
          |Host(bytes,stride)->strides.(index)<-stride;upload index bytes
          |Device(source,stride)->strides.(index)<-stride;
              (match source()with
               |Some b->device:=(source,b):: !device;Ok b
               |None->stale:=true;fail operation Resource"GPU display output is stale or closed")in
        Result.bind buffer(fun b->Result.map(fun rest->b::rest)(bind(index+1)))end in
      (* the device's index count, read by the draw, joins the inputs whose identity the token keeps *)
      let indirect=match indirect with
        |None->Ok None
        |Some(source,offset)->(match source()with
          |Some b->device:=(source,b):: !device;Ok(Some(b,offset))
          |None->stale:=true;fail operation Resource"GPU display count is stale or closed")in
      Result.bind indirect(fun indirect->
      Result.bind(bind 0)(fun buffers->
        let params=Bytes.create 32 in
        (* A device colour is a region's float r, g, b (mode 1); a host one is packed words. *)
        let mode index=match attributes.(index)with Device _->1|Host _->0 in
        Array.iteri(fun i word->Bytes.set_int32_le params(i*4)(Int32.of_int word))
          [|kind;strides.(0);strides.(1);strides.(2);strides.(3);strides.(4);mode 2;mode 3|];
        Result.bind(upload 5 params)(fun params_buffer->
          let device=List.rev !device in
          if not !changed&&valid_gpu_marks value&&List.length device=List.length value.marks_device
             &&List.for_all2(fun(_,a)(_,b)->a==b)device value.marks_device then
            Ok(Scene_command.Shape_batch.Private.gpu_token~identity:value.marks_id~count
              ~stamp:value.marks_stamp)
          else begin
            unregister_marks value;
            value.marks_stamp<-Int64.succ value.marks_stamp;value.marks_count<-count;
            value.marks_device<-device;
            match Scene_execution.Private.register_gpu_vertices ~extra:(List.tl buffers@[params_buffer])?indirect
              ~key:(gpu_marks_key value)~device:value.marks_gpu.gpu_device~buffer:(List.hd buffers)
              ~vertex_count:(4*count)~valid:(fun()->valid_gpu_marks value)()with
            |Error e->value.marks_device<-[];backend operation e
            |Ok registration->value.marks_registration<-Some registration;
                Ok(Scene_command.Shape_batch.Private.gpu_token~identity:value.marks_id~count
                  ~stamp:value.marks_stamp)
          end)))
    end
  let close_gpu_marks=close_gpu_marks
  let encoded_draws_for_test (batch:batch)=Scene_execution.Private.coalesced_draws_for_test(List.map(fun x->
    {Scene_execution.family=x.family;blend=x.blend;texture=x.texture;auxiliary=x.auxiliary;
     vertex_attributes=x.vertex_attributes;samples=x.samples;draw=x.value})batch.batch_draws)
  let snapshot_count_for_test value=Snapshot_table.length value.snapshots
  type nonrec submission=submission
  type nonrec batch=batch
  let begin_submission=begin_submission
  let lower_scene2=lower_scene2_submission
  let lower_scene2_segment=lower_scene2_segment
  let lower_ui=lower_ui
  let adopt_draws=adopt_draws
  let adopt_retained_view=adopt_retained_view
  let step=step_submission
  let replay=replay_step
  let cancel=close_submission
  let retained_scene2_segment_stats value=
    Segment_table.length value.retained_scene2_segments,
    value.retained_scene2_segment_hits,value.retained_scene2_segment_misses
end
