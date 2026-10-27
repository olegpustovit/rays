type t={resource:Runtime_resources.Image.t;mutable width:int;mutable height:int}
let message operation error=Format.asprintf"%s: %a"operation Runtime_resources.pp_error error
let map operation=function Ok value->Ok value|Error error->Error(message operation error)
let of_resource resource=Result.map(fun(width,height)->{resource;width;height})
  (Runtime_resources.Image.size resource |> Result.map_error(message"Image.of_resource"))
let load path=match Runtime_resources.Image.load_file path with
  |Ok resource->of_resource resource|Error error->Error(message"Image.load"error)
let load_exn path=match load path with Ok value->value|Error message->failwith message
let create ~width ~height ?(color=Rays_math.Color.transparent) ()=
  if width<=0||height<=0||width>max_int/4||height>max_int/(width*4)then
    Error"Image.create: invalid image extent"else
  let rgba=Bytes.create(width*height*4)in
  for index=0 to width*height-1 do let offset=index*4 in
    Bytes.set rgba offset(Char.chr color.Rays_math.Color.r);
    Bytes.set rgba(offset+1)(Char.chr color.g);
    Bytes.set rgba(offset+2)(Char.chr color.b);
    Bytes.set rgba(offset+3)(Char.chr color.a)done;
  match Runtime_resources.Image.create ~width ~height ~rgba with
  |Ok resource->Ok{resource;width;height}
  |Error error->Error(message"Image.create"error)
let upload_rgba ?into ~width ~height ~rgba ()=match into with
  |None->(match Runtime_resources.Image.create~width~height~rgba with
    |Ok resource->Ok{resource;width;height}|Error error->Error(message"Image.upload_rgba"error))
  |Some image->Result.map(fun()->image.width<-width;image.height<-height;image)
      (map"Image.upload_rgba"(Runtime_resources.Image.replace image.resource~width~height~rgba))
let destroy image=map"Image.destroy"(Runtime_resources.Image.destroy image.resource)
let get_size image=map"Image.get_size"(Runtime_resources.Image.size image.resource)
let get_width image=Result.map fst(get_size image)
let get_height image=Result.map snd(get_size image)
module Private=struct
  let replace target source=match Runtime_resources.Image.replace_owned target.resource source.resource with
    |Ok()->target.width<-source.width;target.height<-source.height;Ok()
    |Error error->Error(message"Image.Private.replace"error)
  let identity image=Runtime_resources.Image.identity image.resource
  let pixels image=map"Image.pixels"(Runtime_resources.Image.pixels image.resource)
  let of_resource=of_resource
  let resource image=image.resource
  let dimensions image=image.width,image.height
end
