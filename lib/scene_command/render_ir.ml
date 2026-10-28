type blend=Source_over|Copy|Replace|Alpha|Add|Multiply|Screen|Subtract
type rect={x:float;y:float;width:float;height:float}
type transform={xx:float;xy:float;yx:float;yy:float;tx:float;ty:float}
type geometry={vertices:float array;indices:int array;color:int32}
type image={resource_id:int;source:rect;destination:rect}
type glyph={glyph_id:int;x:float;y:float}
type glyphs={resource_id:int;color:int32;glyphs:glyph array}
type command=Clear of int32|Set_blend of blend|Push_clip of rect|Pop_clip|Push_transform of transform|Pop_transform|Geometry of geometry|Shapes of Ui_batch.t|Image of image|Glyphs of glyphs
type t={id:int;commands:command array}
let next_id=Atomic.make 1
type error=Non_finite|Invalid_extent|Invalid_cardinality|Invalid_index of int|Invalid_resource_id of int|Invalid_glyph_id of int|Unbalanced_clip|Unbalanced_transform|Complexity_limit
let finite=Float.is_finite
let valid_rect (r:rect)=finite r.x&&finite r.y&&finite r.width&&finite r.height&&r.width>=0.&&r.height>=0.
let finite_rect (r:rect)=finite r.x&&finite r.y&&finite r.width&&finite r.height
let finite_transform (t:transform)=finite t.xx&&finite t.xy&&finite t.yx&&finite t.yy&&finite t.tx&&finite t.ty
let finite_vertices values=
  let index=ref 0 and valid=ref true in
  while !valid&& !index<Array.length values do
    valid:=finite(Array.unsafe_get values !index);incr index
  done;
  !valid
let valid_indices ~vertex_count values=
  let index=ref 0 and invalid=ref None in
  while !invalid=None&& !index<Array.length values do
    let value=Array.unsafe_get values !index in
    if value<0||value>=vertex_count then invalid:=Some value;
    incr index
  done;
  !invalid
let copy_command=function Clear c->Clear c|Set_blend b->Set_blend b|Push_clip r->Push_clip r|Pop_clip->Pop_clip|Push_transform t->Push_transform t|Pop_transform->Pop_transform|Geometry g->Geometry{g with vertices=Array.copy g.vertices;indices=Array.copy g.indices}|Shapes s->Shapes(Ui_batch.copy s)|Image i->Image i|Glyphs g->Glyphs{g with glyphs=Array.copy g.glyphs}
let create_internal ~copy input=
  if Array.length input>1_048_576 then Error Complexity_limit else
  let clip=ref 0 and transform=ref 0 and failure=ref None in
  let fail e=if !failure=None then failure:=Some e in
  Array.iter(fun command->match command with
  |Clear _|Set_blend _->()
  |Shapes s->if not(Ui_batch.valid_shapes s)then fail Invalid_cardinality
  |Push_clip r->if valid_rect r then incr clip else fail(if finite_rect r then Invalid_extent else Non_finite)
  |Pop_clip->if !clip=0 then fail Unbalanced_clip else decr clip
  |Push_transform t->if finite_transform t then incr transform else fail Non_finite
  |Pop_transform->if !transform=0 then fail Unbalanced_transform else decr transform
  |Geometry g->if Array.length g.vertices mod 2<>0||Array.length g.indices mod 3<>0 then fail Invalid_cardinality else if not(finite_vertices g.vertices)then fail Non_finite else Option.iter(fun i->fail(Invalid_index i))(valid_indices~vertex_count:(Array.length g.vertices/2)g.indices)
  |Image i->if i.resource_id<=0 then fail(Invalid_resource_id i.resource_id)else if not(valid_rect i.source&&valid_rect i.destination)then fail(if finite_rect i.source&&finite_rect i.destination then Invalid_extent else Non_finite)
  |Glyphs g->if g.resource_id<=0 then fail(Invalid_resource_id g.resource_id)else Array.iter(fun p->if p.glyph_id<0 then fail(Invalid_glyph_id p.glyph_id)else if not(finite p.x&&finite p.y)then fail Non_finite)g.glyphs)input;
  if !failure=None&& !clip<>0 then failure:=Some Unbalanced_clip;
  if !failure=None&& !transform<>0 then failure:=Some Unbalanced_transform;
  match !failure with Some e->Error e|None->
    let commands=if copy then Array.map copy_command input else input in
    Ok{id=Atomic.fetch_and_add next_id 1;
      commands}
let create input=create_internal~copy:true input
let identity t=t.id
let commands_readonly t=t.commands
let create_owned input=create_internal~copy:false input
let serialize value =
  let buffer = Buffer.create 256 in
  let byte = Buffer.add_uint8 buffer
  and i32 = Buffer.add_int32_le buffer
  and i64 = Buffer.add_int64_le buffer in
  let int value = i64 (Int64.of_int value)
  and float value = i64 (Int64.bits_of_float value) in
  let rect (r : rect) = List.iter float [r.x; r.y; r.width; r.height]
  and transform (t : transform) = List.iter float [t.xx; t.xy; t.yx; t.yy; t.tx; t.ty] in
  i32 0x52324931l;
  int (Array.length value.commands);
  Array.iter (function
    | Clear color -> byte 0; i32 color
    | Set_blend blend ->
        byte 8;
        byte (match blend with Source_over -> 0 | Copy -> 1 | Replace -> 2
          | Alpha -> 3 | Add -> 4 | Multiply -> 5 | Screen -> 6 | Subtract -> 7)
    | Push_clip r -> byte 1; rect r
    | Pop_clip -> byte 2
    | Push_transform t -> byte 3; transform t
    | Pop_transform -> byte 4
    | Geometry geometry ->
        byte 5;
        i32 geometry.color;
        int (Array.length geometry.vertices);
        Array.iter float geometry.vertices;
        int (Array.length geometry.indices);
        Array.iter int geometry.indices
    | Shapes shapes ->
        byte 9;
        int (Ui_batch.count shapes);
        (match Ui_batch.marks shapes with
         | None -> byte 0
         | Some gpu ->
             byte 1;
             int (Marks.gpu_identity gpu);
             i64 (Marks.gpu_stamp gpu));
        let bytes = Ui_batch.instances shapes in
        int (Bytes.length bytes);
        Buffer.add_bytes buffer bytes
    | Image image ->
        byte 6; int image.resource_id; rect image.source; rect image.destination
    | Glyphs glyphs ->
        byte 7; int glyphs.resource_id; i32 glyphs.color;
        int (Array.length glyphs.glyphs);
        Array.iter (fun glyph -> int glyph.glyph_id; float glyph.x; float glyph.y)
          glyphs.glyphs) value.commands;
  Buffer.to_bytes buffer
