type render_mode=Blended of Rays_math.Color.t
type hinting=Normal_hinting|Light_hinting|Mono_hinting|None_hinting
type alignment=Left|Center|Right
(* density, size, text, wrap, align, rgba *)
module Text_key=struct
  type t=int*int*string*int option*alignment*(int*int*int*int)
  let equal=(=) let hash=Hashtbl.hash end
module Text_cache=Lru.Make(Text_key)
type t={resource:Runtime_resources.Font.t;
  mutable generation:int;
  cache:Image.t Text_cache.t}
let text_cache_capacity=256
let text_cache()=Text_cache.create ~release:(fun _ image->ignore(Image.destroy image))text_cache_capacity
let message operation error=`Msg(Format.asprintf"%s: %a"operation Runtime_resources.pp_error error)
let make ?source:_ _size=function Ok resource->Ok{resource;generation=1;cache=text_cache()}|Error error->Error(message"Font.load"error)
let load path size=make ~source:path size(Runtime_resources.Font.open_file ~path ~size:(float size))
let system ?(size=16)()=make size(Runtime_resources.Font.open_system ~size:(float size))
let rgba(Blended c)=c.Rays_math.Color.r,c.g,c.b,c.a
(* Invalid UTF-8 becomes U+FFFD rather than failing mid-frame. *)
let sanitize text=if String.is_valid_utf_8 text then text else begin
  let buffer=Buffer.create(String.length text)in
  let rec loop i=if i<String.length text then begin
    let d=String.get_utf_8_uchar text i in
    Buffer.add_utf_8_uchar buffer(Uchar.utf_decode_uchar d);loop(i+Uchar.utf_decode_length d)end in
  loop 0;Buffer.contents buffer end
let resource_align=function Left->Runtime_resources.Font.Left|Center->Center|Right->Right
let image_of_text text=match Runtime_resources.Text.Private.into_image text with
  |Ok image->Image.Private.of_resource image|>Result.map_error(fun message->`Msg message)
  |Error error->Error(message"Font.image"error)
let paint ?(density=1) ?wrap ?(align=Left) font text mode=match Runtime_resources.Font.render font.resource ?wrap_width:wrap ~align:(resource_align align) ~density ~color:(rgba mode)(sanitize text)with
  |Error error->Error(message"Font.render_text"error)|Ok None->
      Image.create ~width:1 ~height:1()|>Result.map_error(fun message->`Msg message)
  |Ok(Some value)->let result=image_of_text value in ignore(Runtime_resources.Text.destroy value);result
let key ?wrap ?(align=Left) ?(density=1) ?(size=0) text mode:Text_key.t=density,size,text,wrap,align,rgba mode
let cached_text ?wrap ?(align=Left) ?(density=1) font text mode=let key=key ?wrap ~align ~density text mode in match Text_cache.find font.cache key with image->Ok image|exception Not_found->
  match paint ~density ?wrap ~align font text mode with Error _ as error->error|Ok image->
    Text_cache.add font.cache key image;Ok image
let clear_cache font=Text_cache.clear font.cache
let resource_hinting=function
  |Normal_hinting->Runtime_resources.Font.Normal_hinting
  |Light_hinting->Runtime_resources.Font.Light_hinting
  |Mono_hinting->Runtime_resources.Font.Mono_hinting
  |None_hinting->Runtime_resources.Font.None_hinting
let set_hinting font value=match Runtime_resources.Font.set_hinting font.resource(resource_hinting value)with Ok()->clear_cache font;();font.generation<-font.generation+1;Ok()|Error e->Error(message"Font.set_hinting"e)
let destroy font=
  clear_cache font;ignore(Runtime_resources.Font.destroy font.resource)
module Private=struct
 type automatic_entry={image:Image.t;mutable references:int;mutable cached:bool}
 module Font_cache=Lru.Make(Int)
 type cache_context={automatic_cache:automatic_entry Text_cache.t;
   automatic_fonts:t Font_cache.t;mutable automatic_references:int}
 and automatic={entry:automatic_entry;context:cache_context option;mutable released:bool}
 let capacity=256 and font_capacity=32
 let current_cache=Domain.DLS.new_key(fun()->None)
 let create_cache_context()={
   automatic_cache=Text_cache.create capacity
     ~evictable:(fun _ entry->entry.references=0)
     ~release:(fun _ entry->entry.cached<-false;
       if entry.references=0 then ignore(Image.destroy entry.image));
   automatic_fonts=Font_cache.create font_capacity ~release:(fun _ font->destroy font);
   automatic_references=0}
 let with_cache_context context f=
   let previous=Domain.DLS.get current_cache in
   Domain.DLS.set current_cache(Some context);
   Fun.protect~finally:(fun()->Domain.DLS.set current_cache previous)f
 let clear_cache_context context=
   Text_cache.clear context.automatic_cache;
   Font_cache.clear context.automatic_fonts
 let font context size=
   match Font_cache.find context.automatic_fonts size with
   |value->Ok value
   |exception Not_found->
     match system~size()with Error _ as error->error|Ok value->Font_cache.add context.automatic_fonts size value;Ok value
 let borrow_automatic ?wrap ?(align=Left) ?(density=1) ~size text mode=
   let key=key?wrap~align~density~size text mode in
   let paint_uncached()=match system~size()with Error _ as error->error|Ok font->
     Fun.protect~finally:(fun()->destroy font)(fun()->paint~density?wrap~align font text mode)
     |>Result.map(fun image->{entry={image;references=1;cached=false};context=None;released=false})in
   match Domain.DLS.get current_cache with
   |None->paint_uncached()
   |Some context->(match Text_cache.find context.automatic_cache key with
     |entry->entry.references<-entry.references+1;context.automatic_references<-context.automatic_references+1;
       Ok{entry;context=Some context;released=false}
     |exception Not_found->match font context size with Error _ as error->error|Ok font->
       match paint~density?wrap~align font text mode with Error _ as error->error|Ok image->
        (* Borrowed entries remain pinned until their scene is released. *)
        let can_cache=Text_cache.length context.automatic_cache<capacity||
          Option.is_some(Text_cache.find_first context.automatic_cache(fun _ entry->entry.references=0))in
        let entry={image;references=1;cached=can_cache}in
        if can_cache then Text_cache.add context.automatic_cache key entry;
        context.automatic_references<-context.automatic_references+1;
        Ok{entry;context=Some context;released=false})
 let automatic_image handle=handle.entry.image
 let release_automatic handle=if not handle.released then begin
   handle.released<-true;handle.entry.references<-handle.entry.references-1;
   Option.iter(fun context->context.automatic_references<-context.automatic_references-1)handle.context;
   if handle.entry.references=0&&not handle.entry.cached then ignore(Image.destroy handle.entry.image)
  end
 let automatic_counts()=
  match Domain.DLS.get current_cache with
  |None->0,0,0
  |Some context->Text_cache.length context.automatic_cache,Font_cache.length context.automatic_fonts,
    context.automatic_references
 let generation font=font.generation
 type glyph={glyph_width:int;glyph_height:int;glyph_advance:int;glyph_alpha:bytes}
 (* One code point rendered exactly as [render_text] rasterizes it inside a
    string at [density], so glyph-by-glyph composition at pen advances
    reproduces whole-string pixels. *)
 let glyph ?(density=1) font code=
   if not(Uchar.is_valid code)then Error(`Msg"Font.glyph: invalid code point")else
   match Runtime_resources.Font.glyph_metrics_at font.resource~density code with
   |Error error->Error(message"Font.glyph"error)
   |Ok metrics->
     let text=let buffer=Buffer.create 4 in
       Buffer.add_utf_8_uchar buffer(Uchar.of_int code);Buffer.contents buffer in
     match Runtime_resources.Font.render font.resource~density
         ~color:(255,255,255,255)text with
     |Error error->Error(message"Font.glyph"error)
     |Ok None->Ok{glyph_width=0;glyph_height=0;glyph_advance=metrics.advance;
         glyph_alpha=Bytes.empty}
     |Ok(Some rendered)->
       Fun.protect~finally:(fun()->ignore(Runtime_resources.Text.destroy rendered))
         (fun()->match Runtime_resources.Text.size rendered,
             Runtime_resources.Text.pixels rendered with
           |Ok(width,height),Ok rgba->
               Ok{glyph_width=width;glyph_height=height;
                 glyph_advance=metrics.advance;
                 glyph_alpha=Bytes.init(width*height)(fun index->
                   Bytes.get rgba(index*4+3))}
           |Error error,_|_,Error error->Error(message"Font.glyph"error))
end
let metrics font=match Runtime_resources.Font.metrics font.resource with
  |Ok m->m|Error e->let `Msg text=message"Font.metrics"e in invalid_arg text
let get_ascent font=(metrics font).ascent
