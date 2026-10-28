module P=Rays_pathtracer
let rgb=P.Linear_color.rgb
let get=function Ok value->value|Error message->failwith message
let rays_mesh geometry = get (Result.map_error Rdk.Error.to_string
  (Rdk_rays.Rays_mesh.to_trace_mesh geometry))
let execution=function Ok value->value|Error error->
  failwith(Format.asprintf"%a"Renderer.pp_error error)
let live_handles=Ogpu.Impl.live_handles

let run () =
  let baseline=live_handles()in
  let cube=get(Result.map_error Rdk.Error.to_string
    (Rdk_test_support.Box_generator.box ~size:(Rays_math.Vec3.create 1. 1. 1.) ()))in
  let scene={ P.objects=[rays_mesh cube,P.material (rgb 0.8 0.6 0.4)]; spheres = []; strands = [];
    environment={sky=rgb 0.6 0.7 0.8;ground=rgb 0.1 0.1 0.1;panels=[]};
    lights=[]}in
  let camera=Rays.Camera.perspective ~fov_y:0.9
    ~at:(Rays_math.Vec3.create 0. 0. 4.)
    ~target:(Rays_math.Vec3.create 0. 0. 0.) ()in
  let standalone=get(P.create ~width:48 ~height:32 scene)in
  let expected=Fun.protect ~finally:(fun()->P.destroy standalone)(fun()->
    get(P.render standalone camera);get(P.flush standalone);
    assert(P.samples standalone=1);
    Test_pathtracer.pixels standalone)in
  assert(Bytes.length expected=48*32*4);
  assert(live_handles()=baseline);
  let config={Renderer.default_configuration with
    logical_width=48;logical_height=32;drawable_width=48;drawable_height=32;
    title="GPU film test"} in
  let coordinator=execution(Renderer.create config)in
  (* A live shared lease keeps the window's device alive. *)
  let lease=execution(Renderer.acquire_gpu())in
  assert(Renderer.gpu_shared lease);
  assert(Result.is_error(Renderer.destroy coordinator));
  Renderer.release_gpu lease;
  Fun.protect ~finally:(fun()->execution(Renderer.destroy coordinator))
    (fun()->
      let tracer=get(P.create ~width:48 ~height:32 scene)in
      Fun.protect ~finally:(fun()->P.destroy tracer)(fun()->
        get(P.render tracer camera);get(P.flush tracer);
        let image=Rays.Image.Private.resource(P.image tracer)in
        let texture=match Runtime_resources.Image.gpu_snapshot image with
          |Ok(Some(_,_,_,texture))->texture
          |_->failwith"traced film was published through CPU image storage"in
        let direct=match Ogpu.Backend.read_texture texture ~bytes_per_row:(48*4)with
          |Ok bytes->bytes|Error error->failwith(Ogpu.Error.to_string error)in
        assert(P.samples tracer=1);
        assert(direct=expected);
        assert(Test_pathtracer.pixels tracer=direct);
        let canvas=match Rays.Canvas.create ~width:48 ~height:32 with
          |Ok canvas->canvas|Error error->failwith error in
        Fun.protect ~finally:(fun()->ignore(Rays.Canvas.destroy canvas))(fun()->
          get(Rays.Canvas.render canvas Rays.Scene.[
            image (P.image tracer) ~at:(0,0) ()]);
          let copied=get(Rays.Canvas.to_image canvas)in
          let sampled=Fun.protect ~finally:(fun()->ignore(Rays.Image.destroy copied))
            (fun()->Bytes.copy(get(Rays.Image.Private.pixels copied)))in
          assert(sampled=direct));
        let colors=Hashtbl.create 16 in
        Bytes.iteri(fun index byte->if index mod 4=0 then Hashtbl.replace colors byte())direct;
        assert(Hashtbl.length colors>1);
        let textures=Hashtbl.create 2 in
        Hashtbl.add textures (Ogpu.Backend.texture_id texture) ();
        for _=1 to 30 do
          get(P.render tracer camera);get(P.flush tracer);
          match Runtime_resources.Image.gpu_snapshot image with
          |Ok(Some(_,_,_,texture))->Hashtbl.replace textures
              (Ogpu.Backend.texture_id texture)()
          |_->failwith"GPU film reverted to CPU storage"
        done;
        assert(Hashtbl.length textures=3)));
  let after=live_handles()in
  assert(after=baseline);
  print_endline"GPU film: exact standalone/shared/Canvas bytes, direct texture, readback, zero live delta"
