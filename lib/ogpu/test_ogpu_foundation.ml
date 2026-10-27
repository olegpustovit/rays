let fail message=raise(Failure message)
let expect kind=function Error(error:Ogpu.Error.t)when error.kind=kind->()|_->fail"unexpected result"
let run () =
  if Ogpu.Impl.backend <> "mock" then fail "mock implementation tag changed";
  let d1=Ogpu.Handle.create_device()and d2=Ogpu.Handle.create_device()in
  let h=Ogpu.Handle.create ~device:d1 in
  (match Ogpu.Handle.validate_for ~operation:"test" d1 h with Ok()->()|_->fail"live handle rejected");
  expect Ogpu.Error.Cross_device(Ogpu.Handle.validate_for ~operation:"test" d2 h);
  Ogpu.Handle.destroy h;Ogpu.Handle.destroy h;
  Ogpu.Handle.destroy_device d1;Ogpu.Handle.destroy_device d1;
  let caps=Ogpu.Caps.minimum_m1 in
  let bad_buffers=[0L;Int64.minus_one;Int64.max_int]in
  List.iter(fun size->expect Ogpu.Error.Invalid_argument(Ogpu.Validation.validate_buffer caps {label=None;size;usage=[Ogpu.Types.Copy_dst]}))bad_buffers;
  let bad_textures=[(0,1,1,1,1);(1,0,1,1,1);(1,1,0,1,1);(1,1,1,0,1);(1,1,1,1,0);(20000,1,1,1,1)]in
  List.iter(fun(width,height,depth,mip_levels,sample_count)->expect Ogpu.Error.Invalid_argument(Ogpu.Validation.validate_texture caps {label=None;width;height;depth;mip_levels;sample_count;format=Rgba8_unorm;usage=[Ogpu.Types.Texture_binding]}))bad_textures;
  print_endline"OGPU handle and descriptor foundation passed"
