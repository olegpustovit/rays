type kind = Color | Depth
type entry = { kind:kind; samples:int; texture:Ogpu.Backend.texture }
type t = {
  device:Ogpu.Backend.device;
  sample_counts:int list;
  configuration:Ogpu.Surface.configuration;
  mutable entries:entry list;
  mutable dead:bool;
}

let error message = Error (Ogpu.Error.make "Scene_attachment_pool" Ogpu.Error.Invalid_argument message)

let create ~device ~configuration ~sample_counts =
  if sample_counts=[] || List.exists ((>=) 0) sample_counts ||
     List.length (List.sort_uniq Int.compare sample_counts) <> List.length sample_counts
  then invalid_arg "Scene_attachment_pool.create: sample counts must be unique and positive";
  { device; sample_counts; configuration; entries=[]; dead=false }

let descriptor configuration kind samples : Ogpu.Types.texture_descriptor =
  let prefix = match kind with Color -> "color" | Depth -> "depth" in
  { label=Some (Printf.sprintf "scene-execution-%s-%d" prefix samples);
    width=configuration.Ogpu.Surface.physical_width;
    height=configuration.physical_height;
    depth=1; mip_levels=1; sample_count=samples;
    format=Rgba8_unorm;
    usage=[Ogpu.Types.Render_attachment] }

let allocate value configuration kind samples =
  let descriptor = descriptor configuration kind samples in
  match kind with
  | Color -> Ogpu.Backend.create_texture value.device descriptor
  | Depth -> Ogpu.Backend.create_depth_texture value.device descriptor

let destroy_entries entries =
  List.iter (fun entry -> ignore (Ogpu.Backend.destroy_texture entry.texture)) entries

let acquire_many value keys =
  if value.dead then error "pool is destroyed"
  else if List.exists (fun (_,samples)->not(List.mem samples value.sample_counts)) keys then error "unsupported sample count"
  else
    let rec loop made textures=function
      |[]->value.entries<-List.rev_append made value.entries;Ok(List.rev textures)
      |(kind,samples)::rest->
          let matches entry=entry.kind=kind&&entry.samples=samples in
          let found=match List.find_opt matches made with
            |Some _ as entry->entry
            |None->List.find_opt matches value.entries in
          match found with
          |Some entry->loop made(entry.texture::textures)rest
          |None->match allocate value value.configuration kind samples with
            |Error failure->destroy_entries made;Error failure
            |Ok texture->loop({kind;samples;texture}::made)(texture::textures)rest in
    loop[][]keys

let acquire value kind ~samples =
  Result.map List.hd(acquire_many value[kind,samples])

let destroy value =
  if not value.dead then begin
    value.dead <- true;
    destroy_entries value.entries;
    value.entries <- []
  end

let allocated value =
  List.rev_map (fun entry -> entry.kind,entry.samples,Ogpu.Backend.texture_id entry.texture) value.entries
