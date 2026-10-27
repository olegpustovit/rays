module B=Ogpu.Backend
module I=Runtime_resources.Image
let get=function Ok x->x|Error e->failwith(Format.asprintf "%a" Runtime_resources.pp_error e)
let native=function Ok x->x|Error e->failwith(Ogpu.Error.to_string e)
let () =
  let base,live=Ogpu.Impl.create_driver()in
  let reads=ref 0 in
  let read_error=ref None in
  let bytes=Bytes.init(65*3*4)(fun i->Char.chr(i mod 256))in
  let driver=B.{create_device=(fun()->Result.map(fun raw->{raw with
    create_texture=(fun descriptor->Result.map(fun(resource:B.driver_resource)->
      if descriptor.Ogpu.Types.width=65 && descriptor.height=3 then native(resource.write 0L bytes);
      {resource with
       read=(fun offset length->incr reads;match !read_error with
         |Some error->Error error|None->resource.read offset length);
       read_into=(fun offset destination destination_offset length->incr reads;
         match !read_error with
         |Some error->Error error
         |None->resource.read_into offset destination destination_offset length)})
      (raw.create_texture descriptor))})(base.create_device()))}in
  let device=native(B.create_device driver)in
  let descriptor:Ogpu.Types.texture_descriptor={label=None;width=65;height=3;depth=1;
    mip_levels=1;sample_count=1;format=Rgba8_unorm;
    usage=[Texture_binding;Texture_copy_src;Texture_copy_dst]}in
  let textures=ref []in
  let texture descriptor=let texture=native(B.create_texture device descriptor)in
    textures:=texture:: !textures;texture in
  let first=texture descriptor and second=texture descriptor in
  let resized=texture{descriptor with width=17;height=5}in
  let source=ref(Some first)in
  let before=Gc.allocated_bytes()in
  let image=get(I.Private.of_gpu ~width:65 ~height:3 ~source:(fun()-> !source))in
  assert(Gc.allocated_bytes()-.before<4096.);
  assert(I.Private.cpu_storage_bytes image=0 && I.Private.readbacks image=0 && !reads=0);
  let identity=I.identity image and generation=I.generation image in
  let snapshot()=match get(I.Private.gpu_snapshot image)with
    |Some(w,h,g,t)->assert((w,h,g)=(fst(get(I.size image)),snd(get(I.size image)),I.generation image));t
    |None->assert false in
  assert(snapshot()==first && !reads=0);
  assert(get(I.pixels image)=bytes && !reads=1 && I.Private.readbacks image=1);
  let _,_,_,saved,lease=get(I.Private.borrow_snapshot image)in
  assert(saved=bytes && !reads=2);
  source:=Some second;
  assert(Result.is_error(I.Private.gpu_snapshot image) && Result.is_error(I.pixels image));
  assert(!reads=2 && I.generation image=generation);
  get(I.Private.replace_gpu_source image ~width:65 ~height:3 ~source:(fun()-> !source));
  assert(I.identity image=identity && I.generation image=generation+1 && snapshot()==second);
  source:=None;
  assert(Result.is_error(I.Private.gpu_snapshot image));
  assert(Result.is_error(I.Private.borrow_snapshot image));
  assert(Result.is_error(I.Private.replace_gpu_source image ~width:65 ~height:3 ~source:(fun()-> !source)));
  assert(I.generation image=generation+1 && !reads=2);
  get(I.Private.replace_gpu_source image ~width:17 ~height:5 ~source:(fun()->Some resized));
  assert(I.identity image=identity && get(I.size image)=(17,5) && I.generation image=generation+2);
  assert(snapshot()==resized && saved=bytes && I.Private.cpu_storage_bytes image=0);
  I.Private.release_snapshot lease;I.Private.release_snapshot lease;
  assert(I.Private.cpu_storage_bytes image=0);
  List.iter(fun(w,h)->assert(Result.is_error(I.Private.of_gpu ~width:w ~height:h
    ~source:(fun()->Some first)))) [0,3;65,0;max_int,3;65,max_int;17,5];
  List.iter(fun descriptor->
    let bad=texture descriptor in
    assert(Result.is_error(I.Private.of_gpu ~width:65 ~height:3 ~source:(fun()->Some bad)));
    assert(Result.is_error(I.Private.replace_gpu_source image ~width:65 ~height:3 ~source:(fun()->Some bad)));
    assert(get(I.size image)=(17,5) && I.generation image=generation+2 && snapshot()==resized))
    [{descriptor with format=Rgba16_float};{descriptor with format=Rgba32_float};
     {descriptor with depth=2};{descriptor with sample_count=4};{descriptor with usage=[Texture_copy_src]};
     {descriptor with usage=[Texture_binding]}];
  assert(Domain.join(Domain.spawn(fun()->match I.Private.of_gpu ~width:65 ~height:3
    ~source:(fun()->assert false)with Error{kind=Ogpu.Error.Wrong_domain;_}->true|_->false)));
  assert(Domain.join(Domain.spawn(fun()->match I.Private.gpu_snapshot image with
    |Error{kind=Ogpu.Error.Wrong_domain;_}->true|_->false)));
  let cpu=Bytes.make(17*5*4)'\x31'in
  get(I.replace image ~width:17 ~height:5 ~rgba:cpu);
  assert(get(I.Private.gpu_snapshot image)=None && get(I.pixels image)=cpu && !reads=2);
  let _,_,_,cpu_saved,cpu_lease=get(I.Private.borrow_snapshot image)in
  get(I.Private.replace_gpu_source image ~width:17 ~height:5 ~source:(fun()->Some resized));
  assert(cpu_saved=cpu && I.Private.cpu_storage_bytes image=0);
  I.Private.release_snapshot cpu_lease;
  assert(I.Private.cpu_storage_bytes image=0);
  get(I.Private.replace_gpu_source image ~width:17 ~height:5 ~source:(fun()->Some resized));
  native(B.destroy_texture resized);
  assert(Result.is_error(I.Private.gpu_snapshot image) && Result.is_error(I.pixels image));
  assert(!reads=2);
  get(I.destroy image);get(I.destroy image);
  assert(not(B.Private.texture_destroyed first) && not(B.Private.texture_destroyed second));
  let large=texture{descriptor with width=1024;height=1024}in
  let before=Gc.allocated_bytes()in
  let large_image=get(I.Private.of_gpu ~width:1024 ~height:1024 ~source:(fun()->Some large))in
  assert(Gc.allocated_bytes()-.before<4096. && I.Private.cpu_storage_bytes large_image=0 && !reads=2);
  get(I.destroy large_image);
  let backend_error=Ogpu.Error.make "driver.read_texture" Ogpu.Error.Device_lost "injected readback failure" in
  read_error:=Some backend_error;
  let failed_image=get(I.Private.of_gpu ~width:1024 ~height:1024 ~source:(fun()->Some large))in
  (match I.pixels failed_image with
   |Error error when error=backend_error->()
   |_->failwith"Image.pixels changed the backend readback error");
  let failed_canvas=get(Runtime_resources.Canvas.create ~width:1024 ~height:1024)in
  get(Runtime_resources.Canvas.Private.publish_gpu failed_canvas large);
  (match Runtime_resources.Canvas.snapshot failed_canvas with
   |Error error when error=backend_error->()
   |_->failwith"Canvas.snapshot changed the backend readback error");
  read_error:=None;
  get(Runtime_resources.Canvas.destroy failed_canvas);
  get(I.destroy failed_image);
  let destroyed=get(I.create ~width:1 ~height:1 ~rgba:(Bytes.make 4 '\x5a'))in
  let _,_,_,saved,lease=get(I.Private.borrow_snapshot destroyed)in
  get(I.destroy destroyed);I.Private.release_snapshot lease;I.Private.release_snapshot lease;
  assert(saved=Bytes.make 4 '\x5a' && I.Private.cpu_storage_bytes destroyed=0);
  List.iter(fun texture->native(B.destroy_texture texture)) !textures;
  native(B.destroy_device device);assert(live()=0);
  print_endline "GPU image backing: storage-free publication, checked source lifetime/resize, CPU transitions, leases, domain and borrowed ownership pass"
