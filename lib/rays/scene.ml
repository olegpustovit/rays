type blend=Replace|Alpha|Add|Multiply
type text_node={x:int;y:int;value:string;color:Color.t;size:int;wrap:int option;align:Font.alignment;provided_font:Font.t option;mutable automatic:Font.Private.automatic option;mutable rendered:Image.t option}
and view3d_node={viewport:(int*int*int*int)option;camera:Camera.t;scene:Scene3.t;mutable rendered3d:Image.t option}
and image_node={image:Image.t;x:int;y:int;scale:float;angle:float}
and display_list_node={segment:Scene_command.Display_list.t;
  resources:(int*Rays_execution.resource)list}
and ui_node={ui:Scene_command.Ui_batch.t;
  ui_resources:(int*Rays_execution.resource)list}
and node=Group of t|Clear of Color.t|Geometry of Scene_command.Render_ir.geometry|Text of text_node|Image of image_node
 |Display_list of display_list_node|Ui of ui_node|Shapes of Scene_command.Shape_batch.t
 |View3d of view3d_node|Region of int*int*int*int*bool*int|Layer_break
 |Translate of int*int*t|Rotate of float*t|Scale of float*float*t|Affine of Scene_command.Render_ir.transform*t|Clip of int*int*int*int*t|Blend of blend*t
and t=node list
let empty=[]let group x=Group x let clear c=Clear c
let default_color=Color.white
let rgba c=Int32.logor(Int32.shift_left(Int32.of_int c.Color.r)24)(Int32.logor(Int32.shift_left(Int32.of_int c.g)16)(Int32.logor(Int32.shift_left(Int32.of_int c.b)8)(Int32.of_int c.a)))
type path_geometry_key={points:(int*int)list;closed:bool;stroke_width:int64;
  fill_rgba:int32 option;stroke_rgba:int32 option}
module Path_geometry_key=struct
  type t=path_geometry_key
  let equal left right=
    left.closed=right.closed&&left.stroke_width=right.stroke_width&&
    left.fill_rgba=right.fill_rgba&&left.stroke_rgba=right.stroke_rgba&&
    left.points=right.points
  let hash value=Hashtbl.hash(value.points,value.closed,value.stroke_width,
    value.fill_rgba,value.stroke_rgba)
end
module Path_geometry_table=Lru.Make(Path_geometry_key)
let path_geometry_cache_capacity=256
let path_geometry_caches=Domain.DLS.new_key(fun()->
  Path_geometry_table.create path_geometry_cache_capacity)
let path_geometry_cached key make=
  let cache=Domain.DLS.get path_geometry_caches in
  match Path_geometry_table.find cache key with
  |value->value
  |exception Not_found->let value=make()in Path_geometry_table.add cache key value;value
(* Small structural keys: polymorphic hash, exact equality on a hit. *)
module Structural_key(T:sig type t end)=struct
  type t=T.t let equal=(=) let hash=Hashtbl.hash end
let path_geometry_key ~points ~closed ~width ~fill ~stroke=
  {points;closed;stroke_width=Int64.bits_of_float width;
   fill_rgba=Option.map rgba fill;stroke_rgba=Option.map rgba stroke}
module Rounded_table=Lru.Make(Structural_key(struct
  type t=int*int*int*int32 option*int32 option end))
let rounded_cache_capacity=256
let rounded_caches=Domain.DLS.new_key(fun()->Rounded_table.create rounded_cache_capacity)
let rounded_cached key make=
  let cache=Domain.DLS.get rounded_caches in
  match Rounded_table.find cache key with
  |value->value
  |exception Not_found->let value=make()in Rounded_table.add cache key value;value
let line ~from_ ~to_ ?(color=default_color)?(width=1)()=
  let width=float(max 1 width)and points=[from_;to_]in
  let key=path_geometry_key~points~closed:false~width~fill:None~stroke:(Some color)in
  path_geometry_cached key(fun()->Geometry(Scene_command.Shape2.line ~from_
    ~to_ ~width:(int_of_float width) ~color:(rgba color)))
let polygon points ?fill ?stroke()=
  let fill=match fill,stroke with None,None->Some default_color|_->fill in
  let key=path_geometry_key~points~closed:true~width:1.~fill~stroke in
  path_geometry_cached key(fun()->Group(Array.to_list(Array.map(fun geometry->
    Geometry geometry)(Scene_command.Shape2.polygon points
      ~fill:(Option.map rgba fill) ~stroke:(Option.map rgba stroke)))))
let polyline points ?(color=default_color)()=
  let key=path_geometry_key~points~closed:false~width:1.~fill:None~stroke:(Some color)in
  path_geometry_cached key(fun()->Geometry(Scene_command.Shape2.polyline points
    ~color:(rgba color)))
let point ~at:(x,y) ?(color=default_color)()=polygon[x,y;x+1,y;x+1,y+1;x,y+1]~fill:color()
let rect ~at:(x,y)~w~h ?fill ?stroke()=polygon[x,y;x+w,y;x+w,y+h;x,y+h]?fill?stroke()
let rounded_rect ~at:(x,y) ~w ~h ~radius ?fill ?stroke()=
  let radius=max 0(min radius(min(abs w)(abs h)/2))in
  let key=w,h,radius,Option.map rgba fill,Option.map rgba stroke in
  let geometry=rounded_cached key(fun()->
    Array.to_list(Array.map(fun geometry->Geometry geometry)
      (Scene_command.Shape2.rounded_rect ~width:w ~height:h ~radius
        ~fill:(Option.map rgba fill) ~stroke:(Option.map rgba stroke))))in
  Translate(x,y,geometry)
module Ellipse_table=Lru.Make(Structural_key(struct
  type t=int*int*int*int*int32 option*int32 option end))
let ellipse_cache_capacity=256
let ellipse_caches=Domain.DLS.new_key(fun()->Ellipse_table.create ellipse_cache_capacity)
let ellipse_cached key make=
  let cache=Domain.DLS.get ellipse_caches in
  match Ellipse_table.find cache key with
  |value->value
  |exception Not_found->let value=make()in Ellipse_table.add cache key value;value
let ellipse ~at:(cx,cy as at) ~rx ~ry ?fill ?stroke()=
  let key=cx,cy,rx,ry,Option.map rgba fill,Option.map rgba stroke in
  ellipse_cached key(fun()->Group(Array.to_list(Array.map(fun geometry->
    Geometry geometry)(Scene_command.Shape2.ellipse ~center:at ~rx ~ry
      ~fill:(Option.map rgba fill) ~stroke:(Option.map rgba stroke)))))
let circle ~at ~radius ?fill ?stroke()=ellipse~at~rx:radius~ry:radius?fill?stroke()
let path ?fill value=let points=Path.points~steps:20 value in if Path.is_closed value then polygon points?fill()else polyline points()
let text ~at:(x,y) ?(color=default_color) ?(size=16) value=Text{x;y;value;color;size;wrap=None;align=Font.Left;provided_font=None;automatic=None;rendered=None}
let image image ~at:(x,y) ?(scale=1.) ?(angle=0.) ()=
  if not(Float.is_finite scale&&Float.is_finite angle)||scale<=0. then invalid_arg"Scene.image: invalid transform";
  Image{image;x;y;scale;angle}
let view3d ?viewport ~camera scene=View3d{viewport;camera;scene;rendered3d=None}
let display_list ?(images=[]) segment=
  let images=Array.of_list images in
  let bound=Hashtbl.create(Array.length images)in
  Array.iter(fun(id,image)->
    if id<=0 then invalid_arg"Scene.display_list: resource ID must be positive";
    if Hashtbl.mem bound id then
      invalid_arg"Scene.display_list: duplicate resource ID";
    Hashtbl.add bound id image)images;
  Array.iter(function
    |Scene_command.Render_ir.Image image->
        if not(Hashtbl.mem bound image.resource_id)then
          invalid_arg"Scene.display_list: unbound image resource"
    |Glyphs glyphs->
        if not(Hashtbl.mem bound glyphs.resource_id)then
          invalid_arg"Scene.display_list: unbound glyph resource"
    |_->())(Scene_command.Render_ir.Private.commands_readonly
      (Scene_command.Display_list.render_ir segment));
  let resources=Array.fold_right(fun(id,image) rest->
    (id,Rays_execution.Image(Image.Private.resource image))::rest)
    images[]in
  Display_list{segment;resources}
let text_input_region ~at:(x,y)~w~h ?(focused=false) ?(cursor=0)()=
  if cursor<0 then invalid_arg"Scene.text_input_region: negative cursor offset";
  Region(x,y,w,h,focused,cursor)
let translate x y nodes=Translate(x,y,nodes)let rotate a nodes=Rotate(a,nodes)let scale x y nodes=Scale(x,y,nodes)
let clip ~at:(x,y)~w~h nodes=Clip(x,y,w,h,nodes)
(* The same picture: what a node draws, not its render caches; resources and images by identity.
   A value structural equality cannot read (a closure, an opaque handle) counts as different. *)
let rec same (a:t) (b:t) = a==b || (List.compare_lengths a b=0 && List.for_all2 same_node a b)
and same_node a b =
  let eq x y = x==y || (try x=y with Invalid_argument _ -> false) in
  (* a resource is wrapped afresh each frame: its handle is what stays *)
  let handle (x:Rays_execution.resource) (y:Rays_execution.resource) = match x,y with
    | Image a,Image b -> a==b | Text a,Text b -> a==b | Canvas a,Canvas b -> a==b | _ -> false in
  let res r r' = List.compare_lengths r r'=0 && List.for_all2 (fun (i,x) (j,y) -> i=j && handle x y) r r' in
  match a,b with
  | Group x,Group y -> same x y
  | Clear x,Clear y -> x=y
  | Geometry x,Geometry y -> eq x y
  | Text x,Text y -> x.x=y.x && x.y=y.y && x.value=y.value && x.color=y.color && x.size=y.size
      && x.wrap=y.wrap && x.align=y.align && x.provided_font==y.provided_font
  | Image x,Image y -> x.image==y.image && x.x=y.x && x.y=y.y && x.scale=y.scale && x.angle=y.angle
  | Display_list x,Display_list y -> eq x.segment y.segment && res x.resources y.resources
  | Ui x,Ui y -> eq x.ui y.ui && res x.ui_resources y.ui_resources
  | Shapes x,Shapes y -> eq x y
  | View3d x,View3d y -> x.viewport=y.viewport && eq x.camera y.camera && eq x.scene y.scene
  | Region (a,b,c,d,e,f),Region (a',b',c',d',e',f') -> a=a'&&b=b'&&c=c'&&d=d'&&e=e'&&f=f'
  | Layer_break,Layer_break -> true
  | Translate (x,y,t),Translate (x',y',t') -> x=x' && y=y' && same t t'
  | Rotate (r,t),Rotate (r',t') -> r=r' && same t t'
  | Scale (x,y,t),Scale (x',y',t') -> x=x' && y=y' && same t t'
  | Affine (m,t),Affine (m',t') -> eq m m' && same t t'
  | Clip (a,b,c,d,t),Clip (a',b',c',d',t') -> a=a'&&b=b'&&c=c'&&d=d' && same t t'
  | Blend (m,t),Blend (m',t') -> m=m' && same t t'
  | _ -> false

module Private=struct
  let same=same
 let display_list=display_list
 let shapes batch=Shapes batch
 let geometry geometry=Geometry geometry
 let transform transform nodes=Affine(transform,nodes)
  module Ui_batch = Scene_command.Ui_batch
 let layer_break=Layer_break
 let ui ?(images=[]) batch=
   List.iter(fun id->
     if not(List.mem_assoc id images)then
       invalid_arg"Scene.Private.ui: unbound texture resource")
     (Scene_command.Ui_batch.textures batch);
   Ui{ui=batch;ui_resources=List.map(fun(id,image)->
     id,Rays_execution.Image(Image.Private.resource image))images}
 type native_layer=
  |Scene2_layer of Scene_command.Render_ir.t*(int*Rays_execution.resource)list
  |Scene2_segment of Scene_command.Display_list.t*
      (int*Rays_execution.resource)list
  |Scene3_layer of Scene_execution.prepared_scene3
  |Ui_layer of Scene_command.Ui_batch.t*(int*Rays_execution.resource)list
 type staged_native={clear:float*float*float*float;scene2:Scene_command.Render_ir.t;
   resources:(int*Rays_execution.resource)list;
   scene3:Scene_execution.prepared_scene3 list;layers:native_layer list;
   mesh_images:Runtime_resources.Image.t list;
   retained:(string*int64)option}
 type native_error=Message of string|Resource of Runtime_resources.error
 let pp_native_error formatter=function
   |Message message->Format.pp_print_string formatter message
   |Resource error->Runtime_resources.pp_error formatter error
 let native_segment_version segment resources=
   let mix stamp value=Int64.logxor(Int64.mul stamp 0x100000001b3L)
       (Int64.of_int value)in
   Int64.logand Int64.max_int(List.fold_left(fun stamp->function
     |_,Rays_execution.Image image->
         mix(mix stamp(Runtime_resources.Image.identity image))
           (Runtime_resources.Image.generation image)
     |_,Text text->
         mix(mix stamp(Runtime_resources.Text.Private.identity text))
           (Runtime_resources.Text.generation text)
     |_,Canvas canvas->
         mix(mix stamp(Runtime_resources.Canvas.Private.identity canvas))
           (Runtime_resources.Canvas.generation canvas))
     (Scene_command.Display_list.version segment)resources)
 let renderer=ref(fun(_ : t)->())let install_renderer value=renderer:=value
 let text_image ?(density=1) (node : text_node) =
   match node.rendered with
   | Some image -> image
   | None ->
       let image=match node.provided_font with
       |Some font->Result.get_ok(Font.cached_text?wrap:node.wrap~align:node.align~density font node.value(Font.Blended node.color))
       |None->let automatic=Result.get_ok(Font.Private.borrow_automatic?wrap:node.wrap~align:node.align~density~size:node.size node.value(Font.Blended node.color))in
         node.automatic<-Some automatic;Font.Private.automatic_image automatic in
       node.rendered <- Some image;
       image

 type command_builder={mutable values:Scene_command.Render_ir.command array;
   mutable length:int}
 let command_builder()={values=Array.make 32(Scene_command.Render_ir.Clear 0l);length=0}
 let emit builder command=
   if builder.length=Array.length builder.values then(
     let values=Array.make(2*builder.length)(Scene_command.Render_ir.Clear 0l)in
     Array.blit builder.values 0 values 0 builder.length;builder.values<-values);
   Array.unsafe_set builder.values builder.length command;
   builder.length<-builder.length+1
 let image_command builder image x y scale angle=
  let width,height=Image.get_size image in let rect={Scene_command.Render_ir.x=0.;y=0.;width=float width;height=float height}in
    let destination={Scene_command.Render_ir.x=float x;y=float y;width=float width*.scale;height=float height*.scale}in
    let command=Scene_command.Render_ir.Image{resource_id=Image.Private.identity image;source=rect;destination}in
    if angle<>0. then(
      let px=destination.x+.destination.width*.0.5 and py=destination.y+.destination.height*.0.5 and c=cos angle and s=sin angle in
      let xx=c and xy=(-.s)and yx=s and yy=c in
      emit builder(Scene_command.Render_ir.Push_transform{xx;xy;yx;yy;tx=px-.xx*.px-.yx*.py;ty=py-.xy*.px-.yy*.py});
      emit builder command;emit builder Scene_command.Render_ir.Pop_transform)
    else emit builder command
 let commands ?(density=1) scene=
  let builder=command_builder()in
  let rec nodes=function
   |[]->()
   |Clear c::xs->emit builder(Scene_command.Render_ir.Clear(rgba c));nodes xs
   |Geometry g::xs->emit builder(Scene_command.Render_ir.Geometry g);nodes xs
   |Shapes s::xs->emit builder(Scene_command.Render_ir.Shapes s);nodes xs
   |Display_list node::xs->
       Array.iter (emit builder) (Scene_command.Render_ir.Private.commands_readonly
         (Scene_command.Display_list.render_ir node.segment));
       nodes xs
   |Image node::xs->image_command builder node.image node.x node.y node.scale node.angle;nodes xs
   |Text node::xs->
       let scale=1./.float(max 1 density)in
       image_command builder(text_image~density node)node.x node.y scale 0.;nodes xs
   |Group g::xs->nodes g;nodes xs
   |Translate(x,y,g)::xs->emit builder(Scene_command.Render_ir.Push_transform{xx=1.;xy=0.;yx=0.;yy=1.;tx=float x;ty=float y});nodes g;emit builder Scene_command.Render_ir.Pop_transform;nodes xs
   |Scale(x,y,g)::xs->emit builder(Scene_command.Render_ir.Push_transform{xx=x;xy=0.;yx=0.;yy=y;tx=0.;ty=0.});nodes g;emit builder Scene_command.Render_ir.Pop_transform;nodes xs
   |Affine(t,g)::xs->emit builder(Scene_command.Render_ir.Push_transform t);nodes g;emit builder Scene_command.Render_ir.Pop_transform;nodes xs
   |Rotate(a,g)::xs->let c=cos a and s=sin a in emit builder(Scene_command.Render_ir.Push_transform{xx=c;xy=s;yx=(-.s);yy=c;tx=0.;ty=0.});nodes g;emit builder Scene_command.Render_ir.Pop_transform;nodes xs
   |Clip(x,y,w,h,g)::xs->emit builder(Scene_command.Render_ir.Push_clip{x=float x;y=float y;width=float w;height=float h});nodes g;emit builder Scene_command.Render_ir.Pop_clip;nodes xs
   |Blend(mode,g)::xs->let mode=match mode with Replace->Scene_command.Render_ir.Replace|Alpha->Alpha|Add->Add|Multiply->Multiply in emit builder(Scene_command.Render_ir.Set_blend mode);nodes g;emit builder(Scene_command.Render_ir.Set_blend Scene_command.Render_ir.Alpha);nodes xs
   |(View3d _|Ui _|Region _|Layer_break)::xs->nodes xs in
  nodes scene;Array.sub builder.values 0 builder.length
 let rec text_regions scene=List.concat_map(function Region(x,y,w,h,f,c)->[x,y,w,h,f,c]|Group g|Translate(_,_,g)|Rotate(_,g)|Scale(_,_,g)|Affine(_,g)|Clip(_,_,_,_,g)|Blend(_,g)->text_regions g|_->[])scene
 let image_resources ?(density=1) scene=
   let rec nodes acc=function
    |[]->acc
    |Image node::rest->nodes((Image.Private.identity node.image,Rays_execution.Image(Image.Private.resource node.image))::acc)rest
    |Text node::rest->let image=text_image ~density node in nodes((Image.Private.identity image,Rays_execution.Image(Image.Private.resource image))::acc)rest
    |Display_list node::rest->nodes(List.rev_append node.resources acc)rest
    |Group nested::rest|Translate(_,_,nested)::rest|Rotate(_,nested)::rest
    |Scale(_,_,nested)::rest|Affine(_,nested)::rest|Clip(_,_,_,_,nested)::rest|Blend(_,nested)::rest->
        nodes(nodes acc nested)rest
    |_::rest->nodes acc rest in
   List.rev(nodes[]scene)

 let stage_materialized ?(density=1) scene =
   match scene with
   |[Display_list node]->Ok(Scene_command.Display_list.render_ir node.segment,
      node.resources)
   |_->
      try
        match Scene_command.Render_ir.Private.create_owned
          (commands ~density scene)with
        |Error e->Error("invalid scene description: "^Scene_command.Render_ir.(match e with
          |Non_finite->"non-finite value"|Invalid_extent->"negative extent"
          |Invalid_cardinality->"bad vertex/index count"|Invalid_index i->"index "^string_of_int i
          |Invalid_resource_id i->"resource id "^string_of_int i|Invalid_glyph_id i->"glyph id "^string_of_int i
          |Unbalanced_clip->"unbalanced clip"
          |Unbalanced_transform->"unbalanced transform"|Complexity_limit->"over 1,048,576 commands"))
        |Ok ir->Ok(ir,image_resources ~density scene)
      with
      |Failure message->Error message
      |Invalid_argument message->Error message

 let stage ~width ~height scene =
   if width <= 0 || height <= 0 then Error "invalid scene extent"
   else
     try stage_materialized scene with
     |Failure message->Error message
     |Invalid_argument message->Error message

 let unpack_clear color=
   let channel shift=Int32.(to_int(logand(shift_right_logical color shift)0xffl))in
   float(channel 24)/.255.,float(channel 16)/.255.,
   float(channel 8)/.255.,float(channel 0)/.255.

 type layer_item=Two_node of node|Segment_node of display_list_node
   |Three_node of view3d_node|Ui_item of ui_node|Break
 let ordered_items scene=
   let rec add wrap direct acc nodes=List.fold_left(fun acc node->match node with
     |View3d view->Three_node view::acc
     |Ui node->Ui_item node::acc
     |Display_list node when direct->Segment_node node::acc
     |Layer_break->Break::acc
     |Group nodes->add wrap direct acc nodes
     |Translate(x,y,nodes)->add(fun node->wrap(Translate(x,y,[node])))false acc nodes
     |Rotate(angle,nodes)->add(fun node->wrap(Rotate(angle,[node])))false acc nodes
     |Scale(x,y,nodes)->add(fun node->wrap(Scale(x,y,[node])))false acc nodes
     |Affine(t,nodes)->add(fun node->wrap(Affine(t,[node])))false acc nodes
     |Clip(x,y,w,h,nodes)->add(fun node->wrap(Clip(x,y,w,h,[node])))false acc nodes
     |Blend(mode,nodes)->add(fun node->wrap(Blend(mode,[node])))false acc nodes
     |node->Two_node(wrap node)::acc)acc nodes in
   List.rev(add Fun.id true[]scene)

 let grouped_items items=
   let flush nodes acc=if nodes=[]then acc else `Two(List.rev nodes)::acc in
   let rec loop nodes acc=function
     |[]->List.rev(flush nodes acc)
     |Two_node node::rest->loop(node::nodes)acc rest
     |Segment_node node::rest->loop[](`Segment node::flush nodes acc)rest
     |Three_node view::rest->loop[](`Three view::flush nodes acc)rest
     |Ui_item node::rest->loop[](`Ui node::flush nodes acc)rest
     |Break::rest->loop[](flush nodes acc)rest in
   loop[][]items

 let rec same_native_scene left right=
   left==right||match left,right with
   |[],[]->true
   |left::lefts,right::rights->same_native_node left right&&
       same_native_scene lefts rights
   |_ ->false
 and same_native_node left right=
   left==right||match left,right with
   |Clear left,Clear right->left=right
   |Geometry left,Geometry right->left==right||left=right
   |Display_list left,Display_list right->
       left.segment==right.segment&&left.resources==right.resources
   |Ui left,Ui right->left.ui==right.ui&&left.ui_resources==right.ui_resources
   |Shapes left,Shapes right->left==right
   |Image left,Image right->left.image==right.image&&left.x=right.x&&
       left.y=right.y&&left.scale=right.scale&&left.angle=right.angle
   |Text left,Text right->left.x=right.x&&left.y=right.y&&
       left.value=right.value&&left.color=right.color&&left.size=right.size&&
       left.wrap=right.wrap&&left.align=right.align&&
       left.provided_font==right.provided_font
   |Group left,Group right->same_native_scene left right
   |Translate(lx,ly,left),Translate(rx,ry,right)->lx=rx&&ly=ry&&
       same_native_scene left right
   |Rotate(la,left),Rotate(ra,right)->la=ra&&same_native_scene left right
   |Scale(lx,ly,left),Scale(rx,ry,right)->lx=rx&&ly=ry&&
       same_native_scene left right
   |Affine(left_t,left),Affine(right_t,right)->left_t=right_t&&same_native_scene left right
   |Clip(lx,ly,lw,lh,left),Clip(rx,ry,rw,rh,right)->
       lx=rx&&ly=ry&&lw=rw&&lh=rh&&same_native_scene left right
   |Blend(lm,left),Blend(rm,right)->lm=rm&&same_native_scene left right
   |View3d left,View3d right->left.viewport=right.viewport&&
       left.camera==right.camera&&left.scene==right.scene
   |Region(lx,ly,lw,lh,lf,lc),Region(rx,ry,rw,rh,rf,rc)->
       lx=rx&&ly=ry&&lw=rw&&lh=rh&&lf=rf&&lc=rc
   |Layer_break,Layer_break->true
   |_ ->false

 (* Immutable textures and shadow maps carry an identity, so their packed
    RGBA levels and depth resources are converted once and keyed by id. *)
 module Id_table=Lru.Make(Int)
 let converted_byte_capacity=64*1024*1024
 let texture_level_tables=Domain.DLS.new_key(fun()->
   Id_table.create 64~byte_capacity:converted_byte_capacity)
 let texture_levels texture=
   let cache=Domain.DLS.get texture_level_tables in
   let id=Texture.Private.identity texture in
   match Id_table.find cache id with
   |levels->levels
   |exception Not_found->
       let levels=Texture.Private.levels texture|>Array.map(fun(w,h,pixels)->let bytes=Bytes.create(w*h*4)in Array.iteri(fun index color->Bytes.set_int32_be bytes(index*4)(Int32.of_int((color.Color.r lsl 24)lor(color.g lsl 16)lor(color.b lsl 8)lor color.a)))pixels;{Scene_execution.width=w;height=h;bytes})in
       let bytes=Array.fold_left(fun total (level:Scene_execution.texture_level)->total+Bytes.length level.bytes)0 levels in
       Id_table.add cache~bytes id levels;levels
 let shadow_resource_tables=Domain.DLS.new_key(fun()->
   Id_table.create 16~byte_capacity:converted_byte_capacity)
 let shadow_resource shadow=
   let cache=Domain.DLS.get shadow_resource_tables in
   let id=Shadow3.Private.identity shadow in
   match Id_table.find cache id with
   |resource->Ok resource
   |exception Not_found->
       let source=Shadow3.Private.snapshot shadow in
       let projection=Scene3_native_lowering.native_clip_projection source.view_projection in
       let matrix=Array.init 16(fun index->Mat4.get projection~row:(index/4)~column:(index mod 4))in
       let snapshot:Scene_execution.shadow_snapshot={width=source.width;height=source.height;depths=source.depths;matrix;bias={constant=source.bias;slope=source.normal_bias};kernel=(match source.filter with Hard->Tap1|Pcf_3x3->Tap9|Pcf_5x5->Tap25);strength=source.strength}in
       match Scene_execution.shadow_resource~key:(string_of_int id)snapshot with
       |Error _ as error->error
       |Ok resource->Id_table.add cache~bytes:(source.width*source.height*4)id resource;Ok resource
 (* Structural parts only: physically compared parts (geometry arrays, images,
    fonts, cameras, Scene3 values) fall back to the exact compare on a hit. *)
 let rec hash_native_scene nodes=List.fold_left(fun hash node->
     hash*65599 lxor hash_native_node node)17 nodes
 and hash_native_node=function
   |Clear color->Hashtbl.hash(0,color)
   |Geometry geometry->Hashtbl.hash(1,Array.length geometry.vertices,Array.length geometry.indices,geometry.color)
   |Display_list _->3
   |Ui _->4
   |Shapes _->16
   |Image node->Hashtbl.hash(5,node.x,node.y,node.scale,node.angle)
   |Text node->Hashtbl.hash(6,node.x,node.y,node.value,node.color,node.size,node.wrap,node.align)
   |Group nodes->7 lxor hash_native_scene nodes
   |Translate(x,y,nodes)->Hashtbl.hash(8,x,y)lxor hash_native_scene nodes
   |Rotate(angle,nodes)->Hashtbl.hash(9,angle)lxor hash_native_scene nodes
   |Scale(x,y,nodes)->Hashtbl.hash(10,x,y)lxor hash_native_scene nodes
   |Affine(t,nodes)->Hashtbl.hash(14,t)lxor hash_native_scene nodes
   |Clip(x,y,w,h,nodes)->Hashtbl.hash(11,x,y,w,h)lxor hash_native_scene nodes
   |Blend(mode,nodes)->Hashtbl.hash(12,mode)lxor hash_native_scene nodes
   |View3d node->Hashtbl.hash(13,node.viewport)
   |Region(x,y,w,h,f,c)->Hashtbl.hash(14,x,y,w,h,f,c)
   |Layer_break->15
 module Layer_key=struct
   type nonrec t=t*int
   let equal(left,ld)(right,rd)=ld=rd&&same_native_scene left right
   let hash(nodes,density)=hash_native_scene nodes*31+density end
 module Layer_table=Lru.Make(Layer_key)
 let scene2_layer_cache_capacity=64
 let scene2_layer_cache_byte_capacity=64*1024*1024
 let scene2_layer_caches=Domain.DLS.new_key(fun()->
   Layer_table.create scene2_layer_cache_capacity~byte_capacity:scene2_layer_cache_byte_capacity)
 let scene2_layer_bytes ir=Array.fold_left(fun total->function
   |Scene_command.Render_ir.Shapes s->total+Bytes.length(Scene_command.Shape_batch.instances s)
   |Scene_command.Render_ir.Geometry geometry->total+
       Array.length geometry.vertices*(Sys.word_size/8)+
       Array.length geometry.indices*(Sys.word_size/8)
   |_->total+32)0(Scene_command.Render_ir.Private.commands_readonly ir)
 let stage_scene2_layer ~density nodes=
   let cache=Domain.DLS.get scene2_layer_caches in
   let resources=image_resources~density nodes in
   let versions=List.map(function
     |id,Rays_execution.Image image->id,Runtime_resources.Image.identity image,Runtime_resources.Image.generation image
     |id,Text text->id,Runtime_resources.Text.Private.identity text,Runtime_resources.Text.generation text
     |id,Canvas canvas->id,Runtime_resources.Canvas.Private.identity canvas,Runtime_resources.Canvas.generation canvas)resources in
   match (try Some(Layer_table.find cache(nodes,density))with Not_found->None)with
   |Some(ir,old)when old=versions->Ok(ir,resources)
   |_->
       match stage_materialized ~density nodes with
       |Error _ as error->error
       |Ok(ir,_)as result->
           let bytes=scene2_layer_bytes ir+List.length versions*7*(Sys.word_size/8)in
           Layer_table.add cache~bytes(nodes,density)(ir,versions);
           result

 let empty_ir=Result.get_ok(Scene_command.Render_ir.Private.create_owned[||])
 let rec validate_native_resources=function
   |[]->Ok()
   |(_,Rays_execution.Image image)::rest->
       Result.bind(Runtime_resources.Image.Private.gpu_snapshot image)
         (fun _->validate_native_resources rest)
   |_::rest->validate_native_resources rest
 let rec validate_native_images=function
   |[]->Ok()
   |Image node::rest->
       Result.bind(Runtime_resources.Image.Private.gpu_snapshot
         (Image.Private.resource node.image))(fun _->validate_native_images rest)
   |(Group nested|Translate(_,_,nested)|Rotate(_,nested)|Scale(_,_,nested)|Affine(_,nested)
     |Clip(_,_,_,_,nested)|Blend(_,nested))::rest->
       Result.bind(validate_native_images nested)(fun()->validate_native_images rest)
   |Display_list node::rest->Result.bind(validate_native_resources node.resources)
       (fun()->validate_native_images rest)
   |Ui node::rest->Result.bind(validate_native_resources node.ui_resources)
       (fun()->validate_native_images rest)
   |_::rest->validate_native_images rest
 let stage_native_uncached_with ~aggregate ?(density=1) ~width ~height scene =
   match validate_native_images scene with Error error->Error(Resource error)|Ok()->
   if width<=0||height<=0 then Error(Message "invalid scene extent")else
   let grouped=grouped_items(ordered_items scene)in
   let materialized=match grouped with
   |[`Two _]->stage_materialized ~density scene
   |_->if aggregate then stage_materialized ~density scene else Ok(empty_ir,[])in
   match materialized with Error message->Error(Message message)
   |Ok(scene2,resources)->
   let failure=ref None and mesh_images=ref[] in
   let callbacks:Scene3_native_lowering.resources={
     texture=(fun value->
       let key="texture:"^string_of_int(Texture.Private.identity value.Scene3.value)in
       let source=match Texture.Private.image value.Scene3.value with
       |None->Ok(key,texture_levels value.Scene3.value,None)
       |Some image->(match Runtime_resources.Image.Private.gpu_snapshot image with
         |Error error->Error(Scene3_native_lowering.Resource error)
         |Ok None->Error Scene3_native_lowering.Unsupported_texture
         |Ok(Some(width,height,generation,gpu))->
             mesh_images:=image::!mesh_images;
             Ok(key^":"^string_of_int generation,
               [|{Scene_execution.width;height;bytes=Bytes.empty}|],Some gpu))in
       Result.map(fun(key,levels,gpu)->
         let address=function Texture.Clamp->Ogpu.Types.Clamp_to_edge|Repeat->Repeat|Mirror->Mirror_repeat in
         let min_filter,mag_filter,mip_filter=match value.filter with
           |Texture.Nearest->Ogpu.Types.Nearest,Ogpu.Types.Nearest,Ogpu.Types.No_mip
           |Texture.Bilinear->Ogpu.Types.Linear,Ogpu.Types.Linear,Ogpu.Types.No_mip
           |Texture.Trilinear->Ogpu.Types.Linear,Ogpu.Types.Linear,Ogpu.Types.Linear_mip in
         let sampler:Ogpu.Types.sampler_descriptor={label=Some"scene3-texture";
           min_filter;mag_filter;mip_filter;address_u=address value.wrap_u;
           address_v=address value.wrap_v;lod_min=0.;lod_max=float(Array.length levels-1);
           max_anisotropy=1}in
         {Scene_execution.key;levels;sampler;gpu})source);
     shadow=(fun value->match shadow_resource value with Error _->Error Unsupported_shadow|Ok resource->Ok{Scene_execution.key=resource.texture.key;buffer=resource.parameters;texture=resource.texture;environment=None;sun_shadow=None})}in
   let layers=match grouped with
   |[`Two _]->[Scene2_layer(scene2,resources)]
   |_->List.filter_map(fun item->if!failure<>None then None else match item with
       |`Two nodes->(match stage_scene2_layer ~density nodes with
         |Ok(ir,resources)->Some(Scene2_layer(ir,resources))
         |Error message->failure:=Some(Message message);None)
       |`Segment node->Some(Scene2_segment(node.segment,node.resources))
       |`Ui node->Some(Ui_layer(node.ui,node.ui_resources))
       |`Three node->
         let viewport=Option.value node.viewport~default:(0,0,width,height)in
         (match Scene3_native_lowering.prepare~resources:callbacks~camera:node.camera
            ~viewport node.scene with
          |Ok prepared->Some(Scene3_layer prepared)
          |Error error->failure:=Some(match error with
             |Scene3_native_lowering.Resource error->Resource error
             |error->Message("native View3d lowering failed: "^
             (match error with
              |Scene3_native_lowering.Unsupported_mode->"render mode"
              |Unsupported_texture->"borrowed GPU texture needs an explicit CPU snapshot"
              |Unsupported_shadow->"shadow"
              |Too_many_lights->"more than 64 lights"|Invalid_mesh->"mesh"
              |Invalid_viewport->"viewport"|Resource _->assert false)));None))grouped in
   let clear=ref(0.,0.,0.,0.)and seen_draw=ref false in
   List.iter(function
     |Scene3_layer _|Ui_layer _->seen_draw:=true
     |Scene2_segment(segment,_)->
         Array.iter(function
           |Scene_command.Render_ir.Clear color->
               if!seen_draw then failure:=Some(Message"native Clear after drawing is unsupported")
               else clear:=unpack_clear color
           |Geometry _|Shapes _|Image _|Glyphs _->seen_draw:=true
           |Set_blend _|Push_clip _|Pop_clip|Push_transform _|Pop_transform->())
           (Scene_command.Render_ir.Private.commands_readonly
             (Scene_command.Display_list.render_ir segment))
     |Scene2_layer(ir,_)->Array.iter(function
       |Scene_command.Render_ir.Clear color->
          if!seen_draw then failure:=Some(Message"native Clear after drawing is unsupported")
          else clear:=unpack_clear color
       |Geometry _|Shapes _|Image _|Glyphs _->seen_draw:=true
       |Set_blend _|Push_clip _|Pop_clip|Push_transform _|Pop_transform->())
       (Scene_command.Render_ir.Private.commands_readonly ir))layers;
   match!failure with Some message->Error message|None->
   let scene3=List.filter_map(function Scene3_layer prepared->Some prepared|_->None)layers in
   let retained=match layers with
   |[Scene2_segment(segment,resources)]->Some
       ("scene2-segment:"^Int64.to_string(Scene_command.Display_list.id segment),
        native_segment_version segment resources)
   |_->None in
   Ok{clear= !clear;scene2;resources;scene3;layers;mesh_images= !mesh_images;retained}

 let stage_native_uncached ?density ~width ~height scene=
   stage_native_uncached_with ~aggregate:true ?density ~width ~height scene
 let stage_native_render_uncached ?density ~width ~height scene=
   stage_native_uncached_with ~aggregate:false ?density ~width ~height scene

 type native_stage_cache_entry={cached_scene:t;cached_density:int;
   cached_width:int;cached_height:int;cached_resource_stamp:int;
   cached_aggregate:bool;
   cached_stage:staged_native}
 let native_stage_cache_capacity=16
 let native_stage_cache_byte_capacity=256*1024*1024
 let native_stage_caches=Domain.DLS.new_key(fun()->ref[])
 let next_native_stage_identity=ref 0L
 let native_stage_bytes stage=List.fold_left(fun total->function
   |Scene2_segment(segment,_)->total+Scene_command.Display_list.source_bytes segment
   |Scene2_layer(ir,_)->total+Array.fold_left(fun total->function
       |Scene_command.Render_ir.Shapes s->total+Bytes.length(Scene_command.Shape_batch.instances s)
   |Scene_command.Render_ir.Geometry geometry->total+
           Array.length geometry.vertices*(Sys.word_size/8)+
           Array.length geometry.indices*(Sys.word_size/8)
       |_->total+32)0(Scene_command.Render_ir.Private.commands_readonly ir)
   |Ui_layer(ui,_)->total+Bytes.length(Scene_command.Ui_batch.instances ui)
   |Scene3_layer prepared->total+Array.fold_left(fun total entry->
       total+Bytes.length entry.Scene_execution.draw.mesh.vertices+
       Bytes.length entry.draw.mesh.indices+
       Option.fold~none:0~some:Bytes.length entry.draw.state.transform_uniforms)
       0 prepared.Scene_execution.entries)0 stage.layers
 let trim_native_stage_cache values=
   let rec loop count bytes kept=function
   |[]->List.rev kept
   |entry::rest->
       let size=native_stage_bytes entry.cached_stage in
       if count<native_stage_cache_capacity&&
          size<=native_stage_cache_byte_capacity-bytes
       then loop(count+1)(bytes+size)(entry::kept)rest
       else loop count bytes kept rest in
   loop 0 0[]values
 let rec resource_stamp_loop stamp=function
  |[]->stamp
  |(id,Rays_execution.Image image)::rest->
      resource_stamp_loop
        ((((stamp*65599)lxor id)*65599 lxor
          Runtime_resources.Image.identity image)*65599 lxor
          Runtime_resources.Image.generation image)rest
  |(id,Text text)::rest->resource_stamp_loop
      ((((stamp*65599)lxor id)*65599 lxor
        Runtime_resources.Text.Private.identity text)*65599 lxor
        Runtime_resources.Text.generation text)rest
  |(id,Canvas canvas)::rest->resource_stamp_loop
      ((((stamp*65599)lxor id)*65599 lxor
        Runtime_resources.Canvas.Private.identity canvas)*65599 lxor
        Runtime_resources.Canvas.generation canvas)rest
 let native_resource_stamp stage=List.fold_left(fun stamp->function
   |Scene2_layer(_,resources)|Scene2_segment(_,resources)|Ui_layer(_,resources)->
       resource_stamp_loop stamp resources
   |Scene3_layer _->stamp)
     (List.fold_left(fun stamp image->
       ((stamp*65599)lxor Runtime_resources.Image.identity image)*65599 lxor
       Runtime_resources.Image.generation image)0x345678 stage.mesh_images)stage.layers
 let rec find_native_stage aggregate scene density width height=function
  |[]->None
  |entry::rest->
      if entry.cached_aggregate=aggregate&&
         same_native_scene entry.cached_scene scene&&
         entry.cached_density=density&&
         entry.cached_width=width&&entry.cached_height=height&&
         entry.cached_resource_stamp=native_resource_stamp entry.cached_stage
      then Some entry else
        find_native_stage aggregate scene density width height rest
 let native_scene_cacheable scene=
   let rec nodes=function
    |[]->true
    |View3d view::rest->Scene3.Private.cacheable view.scene&&nodes rest
    |Group nested::rest|Translate(_,_,nested)::rest|Rotate(_,nested)::rest
    |Scale(_,_,nested)::rest|Affine(_,nested)::rest|Clip(_,_,_,_,nested)::rest|Blend(_,nested)::rest->
        nodes nested&&nodes rest
    |Display_list _::rest|Ui _::rest|Shapes _::rest|Region _::rest|Layer_break::rest|Clear _::rest
    |Geometry _::rest|Text _::rest
    |Image _::rest->nodes rest in
   nodes scene
 let stage_native_internal ~aggregate ?(density=1) ~width ~height scene =
   let cacheable=match scene with
   |[Clear _;View3d view]->Scene3.Private.cacheable view.scene
   |_->native_scene_cacheable scene in
   let uncached=if aggregate then stage_native_uncached
     else stage_native_render_uncached in
   if not cacheable then uncached~density~width~height scene else
   let cache=Domain.DLS.get native_stage_caches in
   match find_native_stage aggregate scene density width height !cache with
   |Some entry->Ok entry.cached_stage
   |None->
       match uncached~density~width~height scene with
       |Error _ as error->error
       |Ok stage->
           let stage=match stage.retained with
           |Some _->stage
           |None->
               next_native_stage_identity:=Int64.succ!next_native_stage_identity;
               {stage with retained=Some
                 ("scene-stage:"^Int64.to_string!next_native_stage_identity,1L)}in
           let stamp=native_resource_stamp stage in
           cache:=List.filter(fun entry->not(same_native_scene entry.cached_scene scene&&
             entry.cached_aggregate=aggregate&&
             entry.cached_density=density&&entry.cached_width=width&&
             entry.cached_height=height))!cache;
           cache:={cached_scene=scene;cached_density=density;cached_width=width;
             cached_height=height;cached_resource_stamp=stamp;
             cached_aggregate=aggregate;
             cached_stage=stage}::!cache;
           cache:=trim_native_stage_cache!cache;
           Ok stage

 let stage_native ~width ~height scene=
   Result.map_error(Format.asprintf "%a" pp_native_error)
     (stage_native_internal ~aggregate:true ~width ~height scene)
 let stage_native_render_checked ?density ~width ~height scene=
   stage_native_internal ~aggregate:false ?density ~width ~height scene
 let stage_native_render ~width ~height scene=
   Result.map_error(Format.asprintf "%a" pp_native_error)
     (stage_native_render_checked ~width ~height scene)

 let to_ir scene = Result.map fst (stage ~width:640 ~height:480 scene)
 let rec release scene =
   List.iter
     (function
       | Text node ->
           Option.iter Font.Private.release_automatic node.automatic;
           node.automatic <- None;
           node.rendered <- None
       | View3d node ->
           Option.iter Image.destroy node.rendered3d;
           node.rendered3d <- None
       | Group nodes | Translate (_, _, nodes) | Rotate (_, nodes)
       | Scale (_, _, nodes) | Affine (_, nodes) | Clip (_, _, _, _, nodes) | Blend (_, nodes) ->
           release nodes
       | Clear _ | Geometry _ | Image _
       | Display_list _ | Ui _ | Shapes _ | Region _
       | Layer_break -> ())
     scene
end
let render scene=(!Private.renderer) scene
