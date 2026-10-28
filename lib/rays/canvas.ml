type t={resource:Runtime_resources.Canvas.t;
  mutable execution:Renderer.t option;mutable destroyed:bool;
  caches:Scene.Private.cache_context;
  world_caches:World.Private.cache_context;
  native_caches:Native_scene_lowering.cache_context;
  mutable epoch:int;mutable completed:bool}

let message operation error=Format.asprintf"%s: %a"operation Runtime_resources.pp_error error
let renderer_message operation error=Format.asprintf"%s: %a"operation Renderer.pp_error error
let create ~width ~height=match Runtime_resources.Canvas.create ~width ~height with
  |Ok resource->Ok{resource;execution=None;destroyed=false;caches=Scene.Private.create_cache_context();
      world_caches=World.Private.create_cache_context();
      native_caches=Native_scene_lowering.create_cache_context();epoch=0;completed=false}
  |Error error->Error(message"Canvas.create"error)
let create_exn ~width ~height=match create ~width ~height with Ok value->value|Error value->failwith value
let size value=Result.map_error(message"Canvas.size")(Runtime_resources.Canvas.size value.resource)
let invalidate value=
  match size value with
  |Error _ as error->error
  |Ok _->value.epoch<-value.epoch+1;value.completed<-false;Ok()
let execution ?(density=1) value=
  if value.destroyed then Error"Canvas.render: canvas is destroyed"else
  match value.execution with
  |Some execution->Ok execution
  |None->Result.bind(size value)(fun(width,height)->
      let configuration={Renderer.
        logical_width=width/density;logical_height=height/density;drawable_width=width;
        drawable_height=height;title="Rays Canvas";vsync=false;high_density=true}in
      Renderer.create_offscreen configuration
      |>Result.map_error(renderer_message"Canvas.render")
      |>Result.map(fun execution->value.execution<-Some execution;execution))
let rec render ?(density=1) value scene=
  Scene.Private.with_cache_context value.caches(fun()->
    World.Private.with_cache_context value.world_caches(fun()->
      Native_scene_lowering.with_cache_context value.native_caches(fun()->render_uncached~density value scene)))
and render_uncached ~density value scene=
  Result.bind(invalidate value)(fun()->
    if density<1 then Error"Canvas.render: density must be positive"else
    Result.bind(execution~density value)(fun execution->
      Result.bind(size value)(fun(width,height)->
        let staged=Native_scene_lowering.render~execution~density
          ~width:(width/density)~height:(height/density)scene
          |>Result.map_error(Format.asprintf"Canvas.render: %a"Native_scene_lowering.pp_error)in
        Result.bind staged(fun _->
          Result.bind(Renderer.offscreen_target execution
            |>Result.map_error(renderer_message"Canvas.render"))(fun texture->
            Runtime_resources.Canvas.publish_gpu value.resource texture
            |>Result.map_error(message"Canvas.render")))
        |>Result.map(fun()->value.completed<-true))))
let snapshot value=match Runtime_resources.Canvas.capture value.resource with
  |Error error->Error(message"Canvas.capture"error)
  |Ok image->Fun.protect~finally:(fun()->ignore(Runtime_resources.Image.destroy image))(fun()->
      Runtime_resources.Image.pixels image|>Result.map_error(message"Canvas.pixels"))
let color_at bytes offset=Rays_math.Color.rgba(Char.code(Bytes.get bytes offset))
  (Char.code(Bytes.get bytes(offset+1)))(Char.code(Bytes.get bytes(offset+2)))
  (Char.code(Bytes.get bytes(offset+3)))
let pixel value ~x ~y=Result.bind(size value)(fun(w,h)->
  if x<0||y<0||x>=w||y>=h then Ok None else
  Result.map(fun bytes->Some(color_at bytes((y*w+x)*4)))(snapshot value))
let pixels value=Result.bind(size value)(fun(w,h)->
  Result.map(fun bytes->Array.init(w*h)(fun index->color_at bytes(index*4)))(snapshot value))
let to_image value=match Runtime_resources.Canvas.capture value.resource with
  |Ok image->Image.Private.of_resource image
  |Error error->Error(message"Canvas.to_image"error)
module Private=struct
  let invalidate=invalidate
  let pixel_stats value=Runtime_resources.Canvas.pixel_stats value.resource
  let gpu_source value=
    match Runtime_resources.Canvas.size value.resource with
    |Error error->Error(message"Canvas.gpu_source"error)
    |Ok _ when value.destroyed || not value.completed->Error"Canvas.gpu_source: no completed GPU frame"
    |Ok _->match Runtime_resources.Canvas.gpu_snapshot value.resource with
      |None->Error"Canvas.gpu_source: no completed GPU frame"
      |Some(width,height,generation,texture)->
          let epoch=value.epoch in
          Ok(width,height,fun()->
            if value.destroyed || not value.completed || value.epoch<>epoch then None else
            match Runtime_resources.Canvas.gpu_snapshot value.resource with
            |Some(_,_,current,source)when current=generation && source==texture->Some texture
            |_->None)
  type native_stats=Renderer.stats
  let native_stats value=match value.execution with
    |None->Ok Renderer.zero_stats
    |Some execution->Renderer.stats execution
      |>Result.map_error(renderer_message"Canvas.Private.native_stats")
end
let save_png value path=Runtime_resources.write_atomic ~filename:path (fun temporary->
  Result.map_error (message"Canvas.save_png")
    (Runtime_resources.Canvas.save_png value.resource temporary))
let save_screen_png=Canvas_runtime.save
let destroy value=if value.destroyed then Ok()else
  Result.bind(size value)(fun _->
    value.epoch<-value.epoch+1;value.completed<-false;
    let destroy_renderer=match value.execution with
      |None->Ok()
      |Some execution->Renderer.destroy execution
        |>Result.map_error(renderer_message"Canvas.destroy")in
    Result.bind destroy_renderer(fun()->
      value.execution<-None;
      Result.map(fun()->
        Scene.Private.clear_cache_context value.caches;
        value.destroyed<-true)
        (Runtime_resources.Canvas.destroy value.resource
          |>Result.map_error(message"Canvas.destroy"))))
