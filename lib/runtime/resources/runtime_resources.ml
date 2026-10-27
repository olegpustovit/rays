open Ogpu.Error

type error = Ogpu.Error.t
let pp_error=Ogpu.Error.pp
let error operation kind message=Error(Ogpu.Error.make operation kind message)
let rec ensure_directory path =
  if path = "" || path = "." then Ok ()
  else
    match Unix.stat path with
    | { Unix.st_kind = Unix.S_DIR; _ } -> Ok ()
    | _ -> Error (path ^ ": not a directory")
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        let parent = Filename.dirname path in
        if parent = path then Error (path ^ ": no parent directory")
        else
          Result.bind (ensure_directory parent) (fun () ->
            try Unix.mkdir path 0o755; Ok () with
            | Unix.Unix_error (Unix.EEXIST, _, _) -> ensure_directory path
            | Unix.Unix_error (error, _, _) -> Error (path ^ ": " ^ Unix.error_message error))
    | exception Unix.Unix_error (error, _, _) -> Error (path ^ ": " ^ Unix.error_message error)

let write_atomic ~filename write =
  try
    let filename = try Unix.realpath filename with Unix.Unix_error _ -> filename in
    let directory = Filename.dirname filename in
    Result.bind (ensure_directory directory) (fun () ->
      let temporary, channel = Filename.open_temp_file ~temp_dir:directory ".rays-" ".tmp" in
      close_out channel;
      Fun.protect ~finally:(fun () ->
        try Sys.remove temporary with Sys_error _ -> ()) (fun () ->
        Result.bind (write temporary) (fun () ->
          let fd = Unix.openfile temporary [Unix.O_RDONLY] 0 in
          Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd);
          Unix.rename temporary filename;
          Ok ())))
  with Sys_error message -> Error message
     | Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)

let write_text ~filename text =
  write_atomic ~filename (fun temporary ->
    try
      Out_channel.with_open_bin temporary (fun channel -> output_string channel text);
      Ok ()
    with Sys_error message -> Error message)

let main operation callback=
  if not(Sdl3.Thread.is_initial_domain())||not(Sdl3.Thread.is_sdl_main_thread())then
    error operation Wrong_domain "resource operation requires the initial SDL domain"
  else callback()
let next=Atomic.make 1
let fresh_identity()=Atomic.fetch_and_add next 1
let valid_storage width height bytes=
  width>0&&height>0&&width<=max_int/4&&height<=max_int/(width*4)
  &&Bytes.length bytes=width*height*4

module Image=struct
  type gpu_source={texture:Ogpu.Backend.texture;borrow:unit->Ogpu.Backend.texture option}
  type t={identity:int;mutable generation:int;mutable width:int;mutable height:int;
    mutable rgba:bytes;mutable spare:bytes option;mutable leases:(bytes*int)list;
    mutable gpu:gpu_source option;mutable readbacks:int;
    mutable canvas_owner:int option;
    mutable canvas_returns:(bytes*(bytes->unit))list;mutable dead:bool}
  type lease={owner:t;bytes:bytes;mutable released:bool}
  let identity x=x.identity and generation x=x.generation and destroyed x=x.dead
  let live op x f=main op(fun()->if x.dead then error op Destroyed"image is destroyed"else f())
  let owned ~width ~height ~rgba={identity=fresh_identity();generation=1;width;height;rgba;gpu=None;
    readbacks=0;spare=None;leases=[];canvas_owner=None;canvas_returns=[];dead=false}
  let create ~width ~height ~rgba=main"Image.create"(fun()->
    if not(valid_storage width height rgba)then error"Image.create"Invalid_argument"invalid RGBA extent or storage"
    else Ok(owned ~width ~height ~rgba:(Bytes.copy rgba)))
  (* The decoder hands over a buffer nobody else holds: it becomes the image's
     storage without another copy. *)
  let load_file path=main"Image.load_file"(fun()->match Sdl3_image.load_file path with
    |Error e->error"Image.load_file"Decode(Format.asprintf"%a"Sdl3_image.pp_error e)
    |Ok decoded->
        if valid_storage decoded.width decoded.height decoded.pixels then
          Ok(owned~width:decoded.width~height:decoded.height~rgba:decoded.pixels)
        else error"Image.load_file"Decode"decoded image has invalid RGBA storage")
  let size x=live"Image.size"x(fun()->Ok(x.width,x.height))
  let validate_gpu operation ~width ~height source =
    if width<=0||height<=0||width>Sys.max_string_length/4/height then
      error operation Invalid_argument "invalid GPU image extent"
    else match source()with
    |None->error operation Destroyed "GPU image source was closed or superseded"
    |Some texture->
        let descriptor=Ogpu.Backend.Private.texture_descriptor texture in
        if Ogpu.Backend.Private.texture_destroyed texture then
          error operation Destroyed "GPU image texture is destroyed"
        else if descriptor.width<>width||descriptor.height<>height||descriptor.depth<>1
          ||descriptor.sample_count<>1||descriptor.format<>Ogpu.Types.Rgba8_unorm
          ||not(List.mem Ogpu.Types.Texture_binding descriptor.usage)
          ||not(List.mem Ogpu.Types.Texture_copy_src descriptor.usage)then
          error operation Invalid_argument "GPU image texture shape, format or usage is invalid"
        else Ok texture
  let gpu_texture operation x source =
    Result.bind(validate_gpu operation ~width:x.width ~height:x.height source.borrow)(fun texture->
      if texture==source.texture then Ok texture
      else error operation Destroyed "GPU image source changed without publication")
  let gpu_pixels operation x=match x.gpu with
    |None->Ok(Bytes.copy x.rgba)
    |Some source->Result.bind(gpu_texture operation x source)(fun texture->
        match Ogpu.Backend.read_texture texture ~bytes_per_row:(x.width*4) with
        |Ok bytes->x.readbacks<-x.readbacks+1;Ok bytes
        |Error e->Error e)
  let pixels x=live"Image.pixels"x(fun()->gpu_pixels"Image.pixels"x)
  let leased x bytes=List.memq bytes(List.map fst x.leases)
  let return_canvas_storage x bytes=match List.assq_opt bytes x.canvas_returns with
    |None->false
    |Some callback->
        x.canvas_returns<-List.remove_assq bytes x.canvas_returns;
        callback bytes;true
  let register_canvas_storage x bytes callback=
    x.canvas_returns<-(bytes,callback)::List.remove_assq bytes x.canvas_returns
  let canvas_storage x bytes=List.mem_assq bytes x.canvas_returns
  let writable x length=
    if Bytes.length x.rgba=length&&x.canvas_owner=None&&
       not(canvas_storage x x.rgba)&&not(leased x x.rgba)then Ok x.rgba else
    match x.spare with
    |Some bytes when Bytes.length bytes=length&&not(leased x bytes)->x.spare<-None;Ok bytes
    |_->if List.length x.leases<2 then Ok(Bytes.create length)
        else error"Image.borrow_snapshot"Invalid_argument"both bounded image snapshot buffers are leased"
  let install x bytes=
    let old=x.rgba and old_owned=x.canvas_owner=None in x.rgba<-bytes;x.canvas_owner<-None;
    if old!=bytes&&not(leased x old)then
      if not(return_canvas_storage x old)&&old_owned then x.spare<-Some old
  let borrow_snapshot x=live"Image.borrow_snapshot"x(fun()->
    Result.map(fun bytes->
      let count=Option.value(List.assq_opt bytes x.leases)~default:0 in
      x.leases<-(bytes,count+1)::List.remove_assq bytes x.leases;
      x.width,x.height,x.generation,bytes,{owner=x;bytes;released=false})
      (match x.gpu with None->Ok x.rgba|Some _->gpu_pixels"Image.borrow_snapshot"x))
  let release_snapshot lease=if not lease.released then begin
    lease.released<-true;
    let x=lease.owner and count=Option.value(List.assq_opt lease.bytes lease.owner.leases)~default:0 in
    x.leases<-List.remove_assq lease.bytes x.leases;
    if count>1 then x.leases<-(lease.bytes,count-1)::x.leases
    else if lease.bytes!=x.rgba then
      if not(return_canvas_storage x lease.bytes)&&not x.dead&&x.gpu=None then
        x.spare<-Some lease.bytes
  end
  let replace x ~width ~height ~rgba=live"Image.replace"x(fun()->
    if not(valid_storage width height rgba)then error"Image.replace"Invalid_argument"invalid RGBA replacement"
    else match writable x(Bytes.length rgba)with Error _ as e->e|Ok bytes->
      Bytes.blit rgba 0 bytes 0(Bytes.length rgba);install x bytes;
      x.width<-width;x.height<-height;x.gpu<-None;x.generation<-x.generation+1;Ok())
  let of_gpu ~width ~height ~source=main"Image.of_gpu"(fun()->
    Result.map(fun texture->let x=owned ~width ~height ~rgba:Bytes.empty in
      x.gpu<-Some{texture;borrow=source};x)
      (validate_gpu "Image.of_gpu" ~width ~height source))
  let replace_gpu_source x ~width ~height ~source=live"Image.replace_gpu_source"x(fun()->
    Result.map(fun texture->
      install x Bytes.empty;x.spare<-None;
      x.width<-width;x.height<-height;x.gpu<-Some{texture;borrow=source};
      x.generation<-x.generation+1)
      (validate_gpu "Image.replace_gpu_source" ~width ~height source))
  let replace_gpu x texture=replace_gpu_source x ~width:x.width ~height:x.height
    ~source:(fun()->Some texture)
  let gpu_snapshot x=live"Image.gpu_snapshot"x(fun()->match x.gpu with
    |None->Ok None
    |Some source->Result.map(fun texture->Some(x.width,x.height,x.generation,texture))
        (gpu_texture "Image.gpu_snapshot" x source))
  let replace_owned target source=main"Image.replace_owned"(fun()->
    if target.dead then error"Image.replace_owned"Destroyed"target image is destroyed"
    else if source.dead then error"Image.replace_owned"Destroyed"source image is destroyed"
    else if Option.is_some source.gpu then error"Image.replace_owned"Invalid_argument"GPU image cannot transfer CPU storage"
    else if target==source then error"Image.replace_owned"Invalid_argument"source and target images must differ"
    else if leased source source.rgba then
      error"Image.replace_owned"Invalid_argument"source image storage is leased"
    else begin
      (* Both values and the complete replacement have been validated before
         mutation.  Moving the byte storage makes replacement transactional
         without another full-frame copy. *)
      target.width<-source.width;
      target.height<-source.height;
      let moved=source.rgba in
      install target moved;
      target.gpu<-None;
      (match List.assq_opt moved source.canvas_returns with
       |None->()
       |Some callback->
           source.canvas_returns<-List.remove_assq moved source.canvas_returns;
           register_canvas_storage target moved callback);
      target.generation<-target.generation+1;
      source.rgba<-Bytes.empty;
      source.dead<-true;
      Ok()
    end)
  let destroy x=main"Image.destroy"(fun()->if x.dead then Ok()else(
    x.dead<-true;
    List.iter(fun(bytes,_)->if not(leased x bytes)then
      ignore(return_canvas_storage x bytes))x.canvas_returns;
    x.rgba<-Bytes.empty;x.spare<-None;x.canvas_owner<-None;x.gpu<-None;Ok()))
  module Private=struct
    type nonrec lease=lease
    let of_owned_rgba=owned
    let of_gpu=of_gpu
    let replace_gpu_source=replace_gpu_source
    let replace_gpu=replace_gpu
    let gpu_snapshot=gpu_snapshot
    let borrow_snapshot=borrow_snapshot
    let release_snapshot=release_snapshot
    let readbacks x=x.readbacks
    let cpu_storage_bytes x=Bytes.length x.rgba+Option.fold ~none:0 ~some:Bytes.length x.spare
  end
end

module Png=struct
  let add_u32 b x=for shift=3 downto 0 do Buffer.add_char b(Char.chr((x lsr(shift*8))land 255))done
  let crc bytes=let crc=ref 0xffffffffl in Bytes.iter(fun c->crc:=Int32.logxor !crc(Int32.of_int(Char.code c));for _=1 to 8 do crc:=Int32.logxor(Int32.shift_right_logical !crc 1)(if Int32.logand !crc 1l<>0l then 0xedb88320l else 0l)done)bytes;Int32.logxor !crc 0xffffffffl
  let adler bytes=let a=ref 1 and b=ref 0 in Bytes.iter(fun c->a:=(!a+Char.code c)mod 65521;b:=(!b+ !a)mod 65521)bytes;(!b lsl 16)lor !a
  let chunk out kind data=add_u32 out(Bytes.length data);Buffer.add_string out kind;Buffer.add_bytes out data;let all=Bytes.of_string(kind^Bytes.to_string data)in add_u32 out(Int32.to_int(crc all))
  let encode ~width ~height rgba=let raw=Bytes.create(height*(1+width*4))in for y=0 to height-1 do Bytes.set raw(y*(1+width*4))'\000';Bytes.blit rgba(y*width*4)raw(y*(1+width*4)+1)(width*4)done;let z=Buffer.create(Bytes.length raw+32)in Buffer.add_string z"\x78\x01";let p=ref 0 in while !p<Bytes.length raw do let n=min 65535(Bytes.length raw- !p)in Buffer.add_char z(Char.chr(if !p+n=Bytes.length raw then 1 else 0));Buffer.add_char z(Char.chr(n land 255));Buffer.add_char z(Char.chr(n lsr 8));let q=(lnot n)land 65535 in Buffer.add_char z(Char.chr(q land 255));Buffer.add_char z(Char.chr(q lsr 8));Buffer.add_subbytes z raw !p n;p:=!p+n done;add_u32 z(adler raw);let out=Buffer.create(Buffer.length z+64)in Buffer.add_string out"\x89PNG\r\n\x1a\n";let ihdr=Buffer.create 13 in add_u32 ihdr width;add_u32 ihdr height;Buffer.add_string ihdr"\x08\x06\x00\x00\x00";chunk out"IHDR"(Bytes.of_string(Buffer.contents ihdr));chunk out"IDAT"(Bytes.of_string(Buffer.contents z));chunk out"IEND"Bytes.empty;Bytes.of_string(Buffer.contents out)
end

module Canvas=struct
  type t={identity:int;mutable generation:int;width:int;height:int;
    mutable rgba:bytes;mutable spare:bytes option;mutable mirror:Image.t option;
    mutable blocked:Image.t option;
    (* The GPU frame this canvas last rendered (owned by its execution); CPU
       pixels are read back lazily while [cpu_stale]. A CPU mutation makes
       the CPU bank authoritative again and forgets the texture. *)
    mutable gpu:Ogpu.Backend.texture option;mutable cpu_stale:bool;
    mutable captures:int;mutable readbacks:int;
    mutable dead:bool}
  let generation x=x.generation and destroyed x=x.dead
  let live op x f=main op(fun()->if x.dead then error op Destroyed"canvas is destroyed"else f())
  let storage operation width height=
    if width<=0||height<=0||width>max_int/4||height>max_int/(width*4)then
      error operation Invalid_argument"invalid canvas extent"
    else Ok(Bytes.make(width*height*4)'\000')
  let create ~width ~height=main"Canvas.create"(fun()->Result.map(fun rgba->{identity=fresh_identity();generation=1;width;height;rgba;spare=None;mirror=None;blocked=None;gpu=None;cpu_stale=false;captures=0;readbacks=0;dead=false})(storage"Canvas.create"width height))
  let of_rgba_bytes ~width ~height ~rgba=main"Canvas.of_rgba_bytes"(fun()->
    if not(valid_storage width height rgba)then
      error"Canvas.of_rgba_bytes"Invalid_argument"invalid RGBA extent or storage"
    else Ok{identity=fresh_identity();generation=1;width;height;rgba;spare=None;
      mirror=None;blocked=None;gpu=None;cpu_stale=false;captures=0;readbacks=0;dead=false})
  let size x=live"Canvas.size"x(fun()->Ok(x.width,x.height))
  let valid_mirror x image=not image.Image.dead&&image.canvas_owner=Some x.identity&&
    image.rgba==x.rgba
  let accept_returned_storage x bytes=
    if not x.dead&&Bytes.length bytes=Bytes.length x.rgba&&bytes!=x.rgba&&x.spare=None
    then x.spare<-Some bytes
  let publish_storage x image=
    image.Image.canvas_owner<-Some x.identity;
    Image.register_canvas_storage image x.rgba(accept_returned_storage x)
  let detach_mirror x=(match x.mirror with
    |Some image when valid_mirror x image->image.canvas_owner<-None
    |_->());x.mirror<-None
  let discard_invalid_mirror x=match x.mirror with
    |Some image when not(valid_mirror x image)->x.mirror<-None
    |_->()
  let detach_for_mutation x=match x.mirror with
    |Some image when valid_mirror x image->
        let replacement=match x.spare with
          |Some spare when Bytes.length spare=Bytes.length x.rgba->x.spare<-None;Bytes.blit x.rgba 0 spare 0(Bytes.length x.rgba);spare
          |_->Bytes.copy x.rgba in
        detach_mirror x;x.rgba<-replacement
    |Some _->x.mirror<-None|None->()
  let detach_for_overwrite x=match x.mirror with
    |Some image when valid_mirror x image->
        let replacement=match x.spare with
          |Some spare when Bytes.length spare=Bytes.length x.rgba->
              x.spare<-None;spare
          |_->Bytes.create(Bytes.length x.rgba)in
        detach_mirror x;x.rgba<-replacement
    |Some _->x.mirror<-None|None->()
  let write_color bytes offset color=Bytes.set_int32_be bytes offset color
  let sync_cpu op x=
    if not x.cpu_stale then Ok()else
    match x.gpu with
    |None->x.cpu_stale<-false;Ok()
    |Some texture->
        detach_for_overwrite x;
        (match Ogpu.Backend.read_texture_into texture~bytes_per_row:(x.width*4)~destination:x.rgba with
         |Ok()->x.cpu_stale<-false;x.readbacks<-x.readbacks+1;Ok()
         |Error e->Error e)
  let cpu_mutates op x=match sync_cpu op x with Error _ as e->e|Ok()->x.gpu<-None;Ok()
  let set_pixel x ~x:px ~y:py color=live"Canvas.set_pixel"x(fun()->if px<0||py<0||px>=x.width||py>=x.height then error"Canvas.set_pixel"Invalid_argument"pixel is out of bounds"else match cpu_mutates"Canvas.set_pixel"x with Error _ as e->e|Ok()->(detach_for_mutation x;write_color x.rgba((py*x.width+px)*4)color;x.generation<-x.generation+1;Ok()))
  let snapshot x=live"Canvas.snapshot"x(fun()->match sync_cpu"Canvas.snapshot"x with Error _ as e->e|Ok()->
    Ok(x.width,x.height,x.generation,Bytes.copy x.rgba))
  let capture x=live"Canvas.capture"x(fun()->match sync_cpu"Canvas.capture"x with Error _ as e->e|Ok()->
    discard_invalid_mirror x;
    let result=if x.mirror=None&&x.blocked=None then begin
      let image:Image.t={identity=fresh_identity();generation=1;
        width=x.width;height=x.height;rgba=x.rgba;spare=None;leases=[];gpu=None;readbacks=0;
        canvas_owner=None;canvas_returns=[];dead=false}in
      publish_storage x image;
      x.mirror<-Some image;Ok image
    end else
      Image.create~width:x.width~height:x.height~rgba:x.rgba in
    Result.map(fun image->x.captures<-x.captures+1;image)result)
  let save_png x path=live"Canvas.save_png"x(fun()->match sync_cpu"Canvas.save_png"x with Error _ as e->e|Ok()->try let bytes=Png.encode~width:x.width~height:x.height x.rgba in let out=open_out_bin path in Fun.protect~finally:(fun()->close_out_noerr out)(fun()->output_bytes out bytes);Ok()with Sys_error m->error"Canvas.save_png"Io m)
  module Private=struct
    let identity x=x.identity
    let pixel_stats x=x.captures,x.readbacks
    let publish_gpu x texture=live"Canvas.Private.publish_gpu"x(fun()->
      let descriptor=Ogpu.Backend.Private.texture_descriptor texture in
      if Ogpu.Backend.Private.texture_destroyed texture||
         descriptor.width<>x.width||descriptor.height<>x.height||
         not(List.mem Ogpu.Types.Texture_binding descriptor.usage) then
        error"Canvas.Private.publish_gpu"Invalid_argument"GPU texture shape or usage is invalid"
      else(x.gpu<-Some texture;x.cpu_stale<-true;x.generation<-x.generation+1;Ok()))
    let gpu_snapshot x=match live "Canvas.Private.gpu_snapshot" x(fun()->
      Ok(Option.map(fun texture->x.width,x.height,x.generation,texture)x.gpu))with
      |Ok snapshot->snapshot|Error _->None
  end
  let destroy x=main"Canvas.destroy"(fun()->if x.dead then Ok()else(x.gpu<-None;x.cpu_stale<-false;detach_mirror x;x.blocked<-None;x.spare<-None;x.rgba<-Bytes.empty;x.dead<-true;Ok()))
end

module Text=struct
  type t={generation:int;width:int;height:int;mutable rgba:bytes;mutable dead:bool}
  let generation x=x.generation and destroyed x=x.dead
  let live op x f=main op(fun()->if x.dead then error op Destroyed"text snapshot is destroyed"else f())
  let owned width height rgba={generation=fresh_identity();width;height;rgba;dead=false}
  let size x=live"Text.size"x(fun()->Ok(x.width,x.height))
  let pixels x=live"Text.pixels"x(fun()->Ok(Bytes.copy x.rgba))
  let destroy x=main"Text.destroy"(fun()->if x.dead then Ok()else(x.dead<-true;x.rgba<-Bytes.empty;Ok()))
  module Private=struct
    let identity x=x.generation
    let borrow_pixels x=live"Text.Private.borrow_pixels"x(fun()->Ok x.rgba)
    let into_image x=live"Text.Private.into_image"x(fun()->
      let image=Image.Private.of_owned_rgba ~width:x.width ~height:x.height ~rgba:x.rgba in
      x.dead<-true;x.rgba<-Bytes.empty;Ok image)
  end
end

module Font=struct
  type hinting=Normal_hinting|Light_hinting|Mono_hinting|None_hinting|Light_subpixel_hinting
  type glyph_metrics={advance:int}
  type alignment=Sdl3_ttf.Font.alignment=Left|Center|Right
  type metrics={ascent:int}
  type t={raw:Sdl3_ttf.Font.t;base_size:float;mutable generation:int;
    mutable density:int;mutable align:alignment;mutable dead:bool}
  let users=ref 0
  let live op x f=main op(fun()->if x.dead then error op Destroyed"font is destroyed"else f())
  let ttf op result=match result with Ok x->Ok x|Error e->error op Decode(Format.asprintf"%a"Sdl3_ttf.pp_error e)
  let ensure_init op=match Sdl3_ttf.Init.initialized()with
    |Error e->error op Decode(Format.asprintf"%a"Sdl3_ttf.pp_error e)
    |Ok true->Ok()|Ok false->ttf op(Sdl3_ttf.Init.init())
  let open_file ~path ~size=main"Font.open_file"(fun()->
    if not(Float.is_finite size)||size<=0. then error"Font.open_file"Invalid_argument"font size must be finite and positive"
    else match ensure_init"Font.open_file"with Error _ as e->e|Ok()->match ttf"Font.open_file"(Sdl3_ttf.Font.open_file~path~size)with
      |Error _ as e->e|Ok raw->incr users;Ok{raw;base_size=size;generation=1;density=1;align=Left;dead=false})
  let open_system ~size=main"Font.open_system"(fun()->match ttf"Font.open_system"(Sdl3_ttf.Font.system_path())with Error _ as e->e|Ok path->open_file~path~size)
  let invalidate x=x.generation<-x.generation+1
  let mutate op x call=live op x(fun()->match ttf op(call())with Error _ as e->e|Ok()->invalidate x;Ok())
  let hinting=function Normal_hinting->Sdl3_ttf.Font.Normal_hinting|Light_hinting->Light_hinting|Mono_hinting->Mono_hinting|None_hinting->None_hinting|Light_subpixel_hinting->Light_subpixel_hinting
  let set_hinting x value=mutate"Font.set_hinting"x(fun()->Sdl3_ttf.Font.set_hinting x.raw(hinting value))
  let glyph_metrics x glyph=live"Font.glyph_metrics"x(fun()->match ttf"Font.glyph_metrics"(Sdl3_ttf.Font.glyph_metrics x.raw glyph)with Error _ as e->e|Ok m->Ok{advance=m.advance})
  let valid_utf8 text=
    let n=String.length text in let rec loop i=if i=n then true else let c=Char.code text.[i]in
      let continuation j= j<n && Char.code text.[j]land 0xc0=0x80 in
      if c<0x80 then loop(i+1)else if c>=0xc2&&c<=0xdf&&continuation(i+1)then loop(i+2)
      else if c>=0xe0&&c<=0xef&&continuation(i+1)&&continuation(i+2)then let c1=Char.code text.[i+1]in if(c=0xe0&&c1<0xa0)||(c=0xed&&c1>=0xa0)then false else loop(i+3)
      else if c>=0xf0&&c<=0xf4&&continuation(i+1)&&continuation(i+2)&&continuation(i+3)then let c1=Char.code text.[i+1]in if(c=0xf0&&c1<0x90)||(c=0xf4&&c1>=0x90)then false else loop(i+4)else false in loop 0
  let set_density x density=
    if density=x.density then Ok()else match ttf"Font.render"(Sdl3_ttf.Font.set_size_dpi x.raw~size:x.base_size~horizontal:(72*density)~vertical:(72*density))with Error _ as e->e|Ok()->x.density<-density;Ok()
  let set_align x align=
    if align=x.align then Ok()else match ttf"Font.render"(Sdl3_ttf.Font.set_wrap_alignment x.raw align)with Error _ as e->e|Ok()->x.align<-align;Ok()
  (* Logical-point measurements: density 1, matching [render ~density:1]. *)
  let metrics x=live"Font.metrics"x(fun()->match set_density x 1 with Error _ as e->e|Ok()->Result.map(fun(m:Sdl3_ttf.Font.metrics)->{ascent=m.ascent})(ttf"Font.metrics"(Sdl3_ttf.Font.metrics x.raw)))
  let render x ?wrap_width ?(align=Left) ~density ~color text=live"Font.render"x(fun()->
    if density<=0||density>16 then error"Font.render"Invalid_argument"density must be in 1..16"
    else if not(valid_utf8 text)then error"Font.render"Invalid_argument"text is not strict UTF-8"
    else match wrap_width with Some width when width<=0->error"Font.render"Invalid_argument"wrap width must be positive"|_->
      match set_density x density with Error _ as e->e|Ok()->
      match set_align x align with Error _ as e->e|Ok()->
      let rendered=match wrap_width with None->Sdl3_ttf.Font.render_blended x.raw~color text|Some width->Sdl3_ttf.Font.render_blended_wrapped x.raw~color~wrap_width:(width*density) text in
      match ttf"Font.render"rendered with Error _ as e->e|Ok None->Ok None|Ok(Some pixels)->Ok(Some(Text.owned pixels.width pixels.height pixels.pixels)))
  (* Metrics at the backing density a matching [render] rasterizes with. *)
  let glyph_metrics_at x ~density glyph=live"Font.glyph_metrics_at"x(fun()->
    if density<=0||density>16 then error"Font.glyph_metrics_at"Invalid_argument"density must be in 1..16"
    else match set_density x density with Error _ as e->e|Ok()->glyph_metrics x glyph)
  let destroy x=main"Font.destroy"(fun()->if x.dead then Ok()else(match ttf"Font.destroy"(Sdl3_ttf.Font.destroy x.raw)with Error _ as e->e|Ok()->x.dead<-true;decr users;if!users=0 then ignore(Sdl3_ttf.Init.quit());Ok()))
end

module Audio=struct
  type t={mixer:Sdl3_mixer.Mixer.t;channels:Sdl3_mixer.Channels.t;
    music:Sdl3_mixer.Music.t;
    mutable dead:bool}
  and sample={owner:t;
    raw:Sdl3_mixer.Audio.t;mutable dead:bool}
  let mix_error op e=error op Decode(Format.asprintf"%a"Sdl3_mixer.pp_error e)
  let result op=function Ok x->Ok x|Error e->mix_error op e
  let live op x f=main op(fun()->if x.dead then error op Destroyed"audio owner is destroyed"else f())
  let sample_live op x f=live op x.owner(fun()->if x.dead then error op Destroyed"audio sample is destroyed"else f())
  let attach op ~max_channels mixer=
    match result op(Sdl3_mixer.Channels.create mixer~count:max_channels)with Error e->ignore(Sdl3_mixer.Mixer.destroy mixer);Error e|Ok channel_bank->
    match result op(Sdl3_mixer.Music.create mixer)with Error e->ignore(Sdl3_mixer.Channels.destroy channel_bank);ignore(Sdl3_mixer.Mixer.destroy mixer);Error e|Ok music->Ok{mixer;channels=channel_bank;music;dead=false}
  let create op ~max_channels mixer=main op(fun()->
    match result op(Sdl3_mixer.Init.init())with Error _ as e->e|Ok()->
    match result op(mixer())with Error _ as e->e|Ok mixer->attach op~max_channels mixer)
  let create_device ~max_channels=create"Audio.create_device"~max_channels Sdl3_mixer.Mixer.create_device
  let load_sample_bytes x bytes=live"Audio.load_sample_bytes"x(fun()->match result"Audio.load_sample_bytes"(Sdl3_mixer.Audio.load_bytes x.mixer(Bytes.copy bytes))with Error _ as e->e|Ok raw->Ok{owner=x;raw;dead=false})
  let valid_volume x=Float.is_finite x&&x>=0.&&x<=1.
  let play_sample x ?(loops=0)?(volume=1.) sample=sample_live"Audio.play_sample"sample(fun()->if not(valid_volume volume)then error"Audio.play_sample"Invalid_argument"volume must be in 0..1"else match result"Audio.play_sample"(Sdl3_mixer.Channels.play x.channels ~loops ~fade_in_ms:0 sample.raw)with Error _ as e->e|Ok channel->match result"Audio.play_sample"(Sdl3_mixer.Channels.set_volume x.channels channel volume)with Error _ as e->e|Ok()->Ok channel)
  let destroy_sample (x:sample)=main"Audio.destroy_sample"(fun()->if x.dead then Ok()else match result"Audio.destroy_sample"(Sdl3_mixer.Audio.destroy x.raw)with Error _ as e->e|Ok()->x.dead<-true;Ok())
  let destroy x=main"Audio.destroy"(fun()->if x.dead then Ok()else match result"Audio.destroy"(Sdl3_mixer.Music.destroy x.music)with Error _ as e->e|Ok()->match result"Audio.destroy"(Sdl3_mixer.Channels.destroy x.channels)with Error _ as e->e|Ok()->match result"Audio.destroy"(Sdl3_mixer.Mixer.destroy x.mixer)with Error _ as e->e|Ok()->x.dead<-true;ignore(Sdl3_mixer.Init.quit());Ok())
end

module Assets=struct
  type t={mutable hooks:(unit->(unit,error)result)list;mutable dead:bool}
  let create()={hooks=[];dead=false}
  let destroy x=main"Assets.destroy"(fun()->if x.dead then Ok()else let first=ref None in List.iter(fun hook->match hook()with Ok()->()|Error e->if !first=None then first:=Some e)x.hooks;x.hooks<-[];x.dead<-true;match!first with None->Ok()|Some e->Error e)
end
