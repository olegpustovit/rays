open Scene_execution
module Command = Scene_command.Render_ir
type blend = Ogpu.Pipeline.blend = Replace | Alpha | Add | Multiply | Screen | Subtract

let default_state viewport scissor = { Scene_execution.viewport; scissor;
  cull=Ogpu.Render_pass.Cull_none; depth_compare=Ogpu.Render_pass.Always;
  depth_write=false; depth_load=Ogpu.Render_pass.Load; depth_clear=1.;
  transform_uniforms=None}
let put_float bytes offset value = Bytes.set_int64_le bytes offset (Int64.bits_of_float value)
let identity_affine_uniforms=let bytes=Bytes.make 24 '\000'in
  Bytes.set_int32_le bytes 0(Int32.bits_of_float 1.);
  Bytes.set_int32_le bytes 16(Int32.bits_of_float 1.);bytes
let write_affine bytes (transform:Command.transform)=
  let put index value=Bytes.set_int32_le bytes(index*4)(Int32.bits_of_float value)in
  put 0 transform.xx;put 1 transform.yx;put 2 transform.tx;
  put 3 transform.xy;put 4 transform.yy;put 5 transform.ty
let affine_uniforms (transform:Command.transform)=
  if transform.xx=1.&&transform.xy=0.&&transform.yx=0.&&transform.yy=1.&&
    transform.tx=0.&&transform.ty=0. then identity_affine_uniforms else
  let bytes=Bytes.make 24 '\000'in
  write_affine bytes transform;bytes
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
  {Scene_execution.family=Scene2;blend=Alpha;texture=None;auxiliary=None;vertex_attributes=None;samples=1; draw={Scene_execution.mesh={key=Printf.sprintf "ir-%Ld-%d" 0L number;
      vertices;vertex_count=count;indices;index_count=Array.length geometry.indices;primitive=Triangle_list};
      state={(default_state (vx,vy,vw,vh) (sx,sy,sw,sh))with
        transform_uniforms=(if identity_transform transform then None else Some(affine_uniforms transform))}} }
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
  {Scene_execution.family=Scene2;blend=Alpha;texture=None;auxiliary=None;vertex_attributes=None;samples=1;
   draw={Scene_execution.mesh={key=Printf.sprintf "ir-run-%d" number;
     vertices;vertex_count= !vertex_count;indices;index_count= !index_count;primitive=Triangle_list};
     state={(default_state viewport clip) with
       transform_uniforms=(if identity_transform transform then None
         else Some (affine_uniforms transform))}}}
let compose_raster (a:Command.transform) (b:Command.transform)=
  if b.xx=1.&&b.xy=0.&&b.yx=0.&&b.yy=1.&&b.tx=0.&&b.ty=0. then a
  else if a.xx=1.&&a.xy=0.&&a.yx=0.&&a.yy=1.&&a.tx=0.&&a.ty=0. then b
  else {Command.xx=a.xx*.b.xx+.a.yx*.b.xy;
  xy=a.xy*.b.xx+.a.yy*.b.xy;yx=a.xx*.b.yx+.a.yx*.b.yy;
  yy=a.xy*.b.yx+.a.yy*.b.yy;tx=a.xx*.b.tx+.a.yx*.b.ty+.a.tx;
  ty=a.xy*.b.tx+.a.yy*.b.ty+.a.ty}

(* PXUI instances: one [Ui] draw per batch. Glyphs sample nearest physical texels. *)
let ui_sampler:Ogpu.Types.sampler_descriptor={label=Some"ui";min_filter=Nearest;
  mag_filter=Nearest;mip_filter=No_mip;address_u=Clamp_to_edge;
  address_v=Clamp_to_edge;lod_min=0.;lod_max=0.;max_anisotropy=1}
let ui_white_texture={Scene_execution.key="ui-white";
  levels=[|{width=1;height=1;bytes=Bytes.make 4 '\255'}|];sampler=ui_sampler;
  gpu=None}
let same_bytes_range source offset target=
  let length=Bytes.length target in
  let rec loop index=index>=length||
    (Int64.equal(Bytes.get_int64_le source(offset+index))
       (Bytes.get_int64_le target index)&&loop(index+8))in
  length mod 8=0&&loop 0
module Int_table=Lru.Make(Int)
module Structural_key(T:sig type t end)=struct type t=T.t let equal=(=) let hash=Hashtbl.hash end
module Quad_key=struct
  type t=Scene_execution.sampled_texture*Scene_command.Render_ir.rect*
    Scene_command.Render_ir.transform*(int*int*int*int)*(float*float*float*float)*(int*int*int*int)
  let equal ((t,d,x,c,u,f):t) ((t',d',x',c',u',f'):t)=t==t'&&d=d'&&x=x'&&c=c'&&u=u'&&f=f'
  let hash ((t,d,x,c,u,f):t)=Hashtbl.hash(t.Scene_execution.key,d,x,c,u,f)
end
module Quad_table=Lru.Make(Quad_key)
module Quad_payload_table=Lru.Make(Structural_key(struct
  type t=Scene_command.Render_ir.rect*Scene_command.Render_ir.transform*
    (int*int*int*int)*(float*float*float*float)
end))
type cached_quad_payload={payload_mesh_key:string;payload_vertices:bytes;payload_indices:bytes}
type ui_slot={mutable ui_vertices:bytes;mutable ui_indices:bytes;
  mutable ui_affine:bytes;mutable ui_source:bytes}
let ui_slot_capacity=256
let snapshot_cache_entry_byte_capacity=1024*1024
let sampled_texture_bytes (texture:Scene_execution.sampled_texture)=
  if Option.is_some texture.gpu then 0 else Array.fold_left(fun total level->
    let bytes=Bytes.length level.Scene_execution.bytes in
    if total>snapshot_cache_entry_byte_capacity-bytes then
      snapshot_cache_entry_byte_capacity+1 else total+bytes)0 texture.levels
type t={quad_cache:Scene_execution.sampled_draw Quad_table.t;
  quad_payload_cache:cached_quad_payload Quad_payload_table.t;
  mutable out_slots:Scene_execution.sampled_draw array;
  mutable out_list:Scene_execution.sampled_draw list;
  ui_slots:ui_slot Int_table.t;mutable ui_indices_scratch:bytes;
  ui_affine_scratch:bytes}
let create ()={quad_cache=Quad_table.create 1024;
  quad_payload_cache=Quad_payload_table.create 256;
  out_slots=[||];out_list=[];ui_slots=Int_table.create ui_slot_capacity;
  ui_indices_scratch=Bytes.empty;ui_affine_scratch=Bytes.make 24 '\000'}
let write_ui_affine value transform=
  write_affine value.ui_affine_scratch transform;value.ui_affine_scratch
let ui_index_bytes value count=
  let needed=count*24 in
  if Bytes.length value.ui_indices_scratch<needed then begin
    let capacity=max needed(2*Bytes.length value.ui_indices_scratch)in
    let bytes=Bytes.create capacity in
    for quad=0 to capacity/24-1 do
      let base=quad*4 and at=quad*24 in
      Bytes.set_int32_le bytes at(Int32.of_int base);
      Bytes.set_int32_le bytes(at+4)(Int32.of_int(base+1));
      Bytes.set_int32_le bytes(at+8)(Int32.of_int(base+2));
      Bytes.set_int32_le bytes(at+12)(Int32.of_int base);
      Bytes.set_int32_le bytes(at+16)(Int32.of_int(base+2));
      Bytes.set_int32_le bytes(at+20)(Int32.of_int(base+3))
    done;
    value.ui_indices_scratch<-bytes
  end;
  if Bytes.length value.ui_indices_scratch=needed then value.ui_indices_scratch
  else Bytes.sub value.ui_indices_scratch 0 needed
let clear value=
  Quad_table.clear value.quad_cache;
  Quad_payload_table.clear value.quad_payload_cache;
  value.out_slots<-[||];value.out_list<-[];Int_table.clear value.ui_slots
let find_ui_slot value key=
  match Int_table.find value.ui_slots key with
  |slot->slot
  |exception Not_found->
      let slot={ui_vertices=Bytes.empty;ui_indices=Bytes.empty;
        ui_affine=Bytes.empty;ui_source=Bytes.empty}in
      Int_table.add value.ui_slots key slot;slot
let lower_scene2 state ~(facts:Runtime.presentation_facts) ~density ~snapshot ~resolve_gpu ~rollback ~resource:resolve commands =
  let identity={Scene_command.Render_ir.xx=1.;xy=0.;yx=0.;yy=1.;tx=0.;ty=0.}in
  let native_projection={Scene_command.Render_ir.xx=2./.float facts.logical_width;xy=0.;yx=0.;
    yy=(-2.)/.float facts.logical_height;tx=(-1.);ty=1.} in
  let render_transform transform=compose_raster native_projection transform in
  let framebuffer=(0,0,facts.logical_width,facts.logical_height) in
  let transforms=ref[identity]and clips=ref[framebuffer]
  and blend=ref Alpha and number=ref 0 and failure=ref None in
  let emit draw=
    let draw=if draw.blend= !blend then draw else {draw with blend= !blend} in
    let i= !number in
    if i>=Array.length state.out_slots then
      state.out_slots<-Array.append state.out_slots
        (Array.make(max 8(Array.length state.out_slots))draw);
    state.out_slots.(i)<-draw;
    incr number in
  let point transform x y=transform.Scene_command.Render_ir.xx*.x+.transform.yx*.y+.transform.tx,
    transform.xy*.x+.transform.yy*.y+.transform.ty in
  let clip_live()=let _,_,width,height=List.hd!clips in width>0&&height>0 in
  let quad (texture:Scene_execution.sampled_texture)
      (destination:Scene_command.Render_ir.rect) (u0,v0,u1,v1 as uv) =
    let transform=render_transform(List.hd!transforms)in
    let clip=List.hd!clips in
    let quad_key=texture,destination,transform,clip,uv,framebuffer in
    match Quad_table.find state.quad_cache quad_key with
    |draw->draw
    |exception Not_found->
    let payload_key=destination,transform,clip,uv in
    let vertices,indices,mesh_key=match Quad_payload_table.find state.quad_payload_cache payload_key with
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
    Quad_payload_table.add state.quad_payload_cache payload_key
      {payload_mesh_key=mesh_key;payload_vertices=vertices;payload_indices=indices};
    vertices,indices,mesh_key in
    let draw={Scene_execution.family=Scene2_textured;blend=Alpha;texture=Some texture;auxiliary=None;vertex_attributes=None;samples=1;
      draw={Scene_execution.mesh={key=mesh_key;vertices;vertex_count=4;indices;index_count=6;primitive=Triangle_list};state=default_state framebuffer clip}}in
    if sampled_texture_bytes texture<=snapshot_cache_entry_byte_capacity then
      Quad_table.add state.quad_cache quad_key draw;
    draw in
  let image (command:Scene_command.Render_ir.image) = match resolve command.Scene_command.Render_ir.resource_id with None->failure:=Some"resource id is unbound"|Some source->
    match snapshot ~density source with Error e->failure:=Some(Ogpu.Error.to_string e)|Ok(width,height,texture)->
      let s=command.source in if width<=0||height<=0 then failure:=Some"resource extent is invalid"else
      let u0=s.x/.float width and v0=s.y/.float height and u1=(s.x+.s.width)/.float width and v1=(s.y+.s.height)/.float height in
      let draw=quad texture command.destination(u0,v0,u1,v1)in
      emit draw in
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
        let gpu_key=match resolve_gpu shapes with Ok key->key|Error message->failure:=Some message;None in
        if !failure=None&&clip_live() && Scene_command.Ui_batch.count shapes>0 then begin
          let count=Scene_command.Ui_batch.count shapes in
          let instances=Scene_command.Ui_batch.instances shapes in
          let slot_key=(-1)- !number in
          let slot=find_ui_slot state slot_key in
          if slot.ui_source!=instances then begin
            (* changed content takes the batch's own bytes (immutable once published), not a copy *)
            if Bytes.length slot.ui_vertices<>Bytes.length instances ||
               not(Bytes.equal slot.ui_vertices instances)then
              slot.ui_vertices<-instances;
            slot.ui_source<-instances
          end;
          if Bytes.length slot.ui_indices<>count*24 then
            slot.ui_indices<-ui_index_bytes state count;
          let affine=write_ui_affine state(render_transform(List.hd !transforms))in
          if not(Bytes.equal slot.ui_affine affine)then
            slot.ui_affine<-Bytes.copy affine;
          let family=if gpu_key=None then Ui else Marks in
          emit {family;blend= !blend;texture=(if family=Marks then None else Some ui_white_texture);
            auxiliary=None;vertex_attributes=None;samples=1;
            draw={Scene_execution.mesh={key=(match gpu_key with Some key->key|None->"shapes:"^string_of_int slot_key);
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
        match snapshot ~density source with Error e->failure:=Some(Ogpu.Error.to_string e)|Ok(width,height,texture)->
          Array.iter(fun(glyph:Scene_command.Render_ir.glyph)->
            let destination={Scene_command.Render_ir.x=glyph.x;y=glyph.y;width=float width;height=float height}in
            let draw=quad texture destination(0.,0.,1.,1.)in
            emit draw)glyphs.glyphs)
  done;
  match!failure with Some message->rollback();Error(Ogpu.Error.make "Renderer.lower_scene2" Ogpu.Error.Invalid_state message)
  |None->
      let n= !number in
      let rec prefix_same i=function
        |[]->i=n
        |d::rest->i<n&&state.out_slots.(i)==d&&prefix_same(i+1)rest in
      if prefix_same 0 state.out_list then Ok state.out_list
      else
        let rec loop i acc=if i<0 then acc else loop(i-1)(state.out_slots.(i)::acc)in
        let list=loop(n-1)[]in
        state.out_list<-list;
        Ok list
