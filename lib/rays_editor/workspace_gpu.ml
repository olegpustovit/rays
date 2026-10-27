module G=Flow_ir.Gpu
module H=Flow_gpu.Host
module X=Rays_execution
module P=X.Private
module Slots=Lru.Make(struct type t=int let equal=Int.equal let hash=Hashtbl.hash end)
module Images=Lru.Make(struct type t=string let equal=String.equal let hash=Hashtbl.hash end)
type image_stats={mutable created:int;mutable closed:int;mutable buffers:int;mutable textures:int}
let close_image stats sink=
  stats.closed<-stats.closed+1;
  stats.buffers<-stats.buffers+Flow_gpu.Image_sink.Private.buffer_creations sink;
  stats.textures<-stats.textures+Flow_gpu.Image_sink.Private.texture_creations sink;
  Flow_gpu.Image_sink.close sink
type t={mutable owner:(X.gpu * H.t)option; mark_sets:P.gpu_marks Slots.t;
  images:Flow_gpu.Image_sink.t Images.t;
  image_stats:image_stats;mutable closed_host:H.Private.stats;
  mutable backend:G.backend option; mutable closed:bool; mutable policy:G.policy;
  mutable measured:((string*int)*float option)list;
  mutable entries:(Flow_ir.Packed.instruction array*bool*string option)list}
let create ()=let image_stats={created=0;closed=0;buffers=0;textures=0}in
  {owner=None;
  mark_sets=Slots.create ~release:(fun _->P.close_gpu_marks)64;
  images=Images.create ~release:(fun _->close_image image_stats)64;
  image_stats;closed_host=H.Private.zero;
  backend=None;closed=false;policy=G.Measured;measured=[];entries=[]}
let qualification t=
  if t.closed || not(Domain.is_main_domain())then invalid_arg "Workspace_gpu.qualification: open initial-domain owner required";
  t.policy<-G.Qualification
let error message=Flow.Diagnostic.error ~code:"E_GPU" message
let native result=Result.map_error(fun e->error e.X.message)result
let owner t =
  if t.closed || not(Domain.is_main_domain())then Error(error "Workspace GPU resources require their open initial-domain owner.")else
  match t.owner with Some owner->Ok owner|None->
    Result.map(fun gpu->let owner=gpu,H.create ~clock:Unix.gettimeofday gpu in
      t.owner<-Some owner;owner)(native(X.acquire_gpu()))
(* The cost of a producer is measured on this device: pack, upload and one synchronized dispatch of the
   program over zeroed inputs of the real shape, median of three after a warm-up. At most 64 programs
   (by emitted kernel, and count) are kept; one that cannot run is unmeasured and keeps the CPU. *)
let measure t packed ~count=
  (* a site's kernel entry, by its code: emitting it again (and digesting it) is per-draw work *)
  let code=(Flow_ir.Packed.Private.view packed).code and fast=Domain.DLS.get G.fast in
  let entry=match List.find_opt(fun(c,f,_)->c==code&&f=fast)t.entries with Some(_,_,entry)->entry|None->
    let entry=Result.to_option(Result.map(fun(msl:Flow_gpu.Emit.msl)->msl.entry)(Flow_gpu.Emit.kernel ~fast packed))in
    t.entries<-(code,fast,entry)::List.filteri(fun i _->i<63)t.entries;entry in
  match entry with None->None|Some entry->
  let key=entry,count in
  match List.assoc_opt key t.measured with Some seconds->seconds|None->
  let seconds=match owner t with Error _->None|Ok(_,host)->
    (match (H.backend host).prepare packed with Error _->None|Ok kernel->
      let view=Flow_ir.Packed.Private.view packed in
      let inputs:Flow_ir.Packed.Private.inputs={
        arrays=Array.map(fun width->Array.make(count*width)0.)view.widths;
        uniforms=Array.map(fun width->Array.make width 0.)view.uniform_widths;
        tables=Array.map(fun _->Array.make 4 0.)view.uniform_widths;
        frame=Array.make(Array.length view.code)0.;count}in
      let once()=let start=Unix.gettimeofday()in
        match kernel.run inputs with Error _->None|Ok _->Some(Unix.gettimeofday()-.start)in
      (match once()with None->None|Some _->
        let runs=List.filter_map(fun _->once())[1;2;3]in
        if List.length runs<3 then None else Some(List.nth(List.sort Float.compare runs)1)))in
  t.measured<-(key,seconds)::(if List.length t.measured>=64 then List.filteri(fun i _->i<63)t.measured else t.measured);
  seconds
let backend t = match t.backend with Some backend->backend|None->
  let backend:G.backend={cost=(fun packed ~count->measure t packed ~count);
    prepare=(fun packed->Result.bind(owner t)(fun(_,host)->(H.backend host).prepare packed))}in
  t.backend<-Some backend;backend
(* The frame is the pump: cook workers queue device requests, served here between frames' work. *)
let with_backend ?(fast=false) t run=
  G.set_service(Some(backend t));
  G.pump();
  let previous=Domain.DLS.get G.fast in
  Domain.DLS.set G.fast fast;
  G.with_backend ~policy:t.policy (backend t)(fun()->
    Fun.protect ~finally:(fun()->Domain.DLS.set G.fast previous;G.pump()) run)
let resident t=if not(Domain.DLS.get G.fast) then None else
  Result.to_option(Result.map(fun(_,host)->H.resident host)(owner t))
let image t ~key ~width ~height ~publish value=
  Result.bind(owner t)(fun(gpu,host)->match H.output host value with
    |None->Error(error "The image's GPU producer was closed or superseded.")
    |Some output->
        let convert sink=Result.bind(Flow_gpu.Image_sink.convert sink ~width ~height output)publish in
        match Images.find_opt t.images key with
        |Some sink->convert sink
        |None when Images.length t.images>=64->Error(error "A workspace owns at most 64 GPU image sinks.")
        |None->Result.bind(Flow_gpu.Image_sink.create gpu)(fun sink->
            t.image_stats.created<-t.image_stats.created+1;
            let committed=ref false in
            Fun.protect ~finally:(fun()->if not !committed then close_image t.image_stats sink)(fun()->
              Result.map(fun result->Images.add t.images key sink;committed:=true;result)(convert sink))))
(* One [Marks]-family source per GPU mark set of a frame, numbered in drawing order. *)
let marks t : Marks_scene.gpu=fun ~slot ~kind ~count attributes->
  Result.bind(owner t)(fun(gpu,host)->
    let sink=match Slots.find_opt t.mark_sets slot with Some sink->Ok sink|None->
      Result.map(fun sink->Slots.add t.mark_sets slot sink;sink)(native(P.create_gpu_marks gpu))in
    Result.bind sink(fun sink->
      let source({Marks_scene.source;stride}:Marks_scene.attribute)=match source with
        |Marks_scene.Bytes bytes->P.Host(bytes,stride)
        |Device value->P.Device((fun()->Option.bind(H.output host value)Flow_gpu.Run.buffer),stride)in
      (* positions a fast kernel wrote come with its count record: the set's instance count is
         the one the device wrote, drawn indirect *)
      let indirect=match attributes.(0).source with
        |Device value when Option.bind(H.output host value)Flow_gpu.Run.count_record<>None->
            Some((fun()->Option.bind(H.output host value)Flow_gpu.Run.count_record),4L)
        |_->None in
      native(P.gpu_marks ?indirect sink~kind~count(Array.map source attributes))))
let image_stats t=
  if not(Domain.is_main_domain())then invalid_arg "Workspace_gpu.image_stats: initial domain required";
  let host=Option.fold ~none:t.closed_host ~some:(fun(_,host)->H.Private.stats host)t.owner in
  let buffers=ref t.image_stats.buffers and textures=ref t.image_stats.textures in
  Images.iter t.images(fun _ sink->
    buffers:= !buffers+Flow_gpu.Image_sink.Private.buffer_creations sink;
    textures:= !textures+Flow_gpu.Image_sink.Private.texture_creations sink);
  host,t.image_stats.created,t.image_stats.closed,!buffers,!textures
let close t=
  if not(Domain.is_main_domain())then invalid_arg "Workspace_gpu.close: initial domain required";
  G.set_service None;G.pump();
  if not t.closed then begin
  Slots.clear t.mark_sets;
  Images.clear t.images;
  Option.iter(fun(gpu,host)->H.close host;t.closed_host<-H.Private.stats host;X.release_gpu gpu)t.owner;
  t.owner<-None;t.closed<-true
end
