open Rays
module E=Flow.Eval
module I=Flow_ir.Executor
module V=Flow.Value
module CPU=Sop.Image
module L=Flow_sop.Lower
module P=Sop
module Sources=Lru.Make(struct type t=int let equal=Int.equal let hash=Hashtbl.hash end)
let capture_limit=64*1024*1024
type captured={geometry:Rdk.Geometry.t;attributes:(string*E.value)list;bytes:int}
type source={network:Flow_sop.Network.t;cone:Flow_sop.Network.t;id:int;
  mutable lane:Flow_sop.Value_lane.t;mutable state_stamp:string;mutable live:Input.t;
  mutable geometry:P.Edit_graph.t option;
  mutable compiled:P.Edit_graph.compiled option;mutable root:P.Node.t option;
  mutable dependencies:P.Context.Dependencies.t;mutable projection:string;
  mutable revision:int;mutable materializer:P.Node.t option}
type request={mutable active:(E.plan*int)list;mutable pending_images:int;
  mutable session:P.Session.t option;mutable contexts:(E.plan*L.image_context)list;
  mutable cooked:(int*int*captured)list}
type stamp={serial:int;frame:Input.t;state_stamp:string;sources:(int*int)list}
type snapshot={payload:CPU.t;stamp:stamp}
type entry={mutable image:Image.t option;mutable cpu:snapshot option;
  mutable texture:(Texture.t,string)result Lazy.t option;mutable borrowed:Texture.t option;
  mutable display:stamp option;mutable canvas:Canvas.t option}
type t={resources:Workspace_resources.t;gpu:Workspace_gpu.t;seed:int64;grain:int;domains:int;mutable plan:E.plan option;mutable serial:int;
  mutable lowered:L.t option;mutable request:request option;mutable revision:int;
  source_cache:source Sources.t;capture_cache:(int*captured) Sources.t;
  mutable source_ids:int list array;
  mutable freshness:(bool*bool)array;
  mutable qualification:(E.plan * Flow.Workspace.Paths.t * Flow.Workspace.path option array) option;
  mutable arguments:I.program option array;mutable drawings:Drawing.prepared option array;
  mutable maps:(int * int * E.fn * Flow_sop.Image_kernel.t) option array;
  mutable sizes:(bool * bool) array;
  mutable resolved:entry option array;
  mutable entries:(string*entry)list;
  mutable source_cooks:int;mutable attribute_flattens:int;
  mutable canvases_created:int;mutable canvases_destroyed:int;
  mutable captures:int;mutable readbacks:int}
let create ?(seed=0L) ?(grain=16384) ?(domains=Parallel.recommended_domains()) ~gpu resources=
  let capture_cache=Sources.create ~byte_capacity:capture_limit 64 in
  let source_cache=Sources.create ~release:(fun id _->Sources.remove capture_cache id)64 in
  {resources;gpu;seed;grain;domains;plan=None;serial=0;
  lowered=None;request=None;revision=0;source_cache;capture_cache;source_ids=[||];freshness=[||];
  qualification=None;
  arguments=[||];drawings=[||];maps=[||];sizes=[||];resolved=[||];entries=[];
  source_cooks=0;attribute_flattens=0;canvases_created=0;canvases_destroyed=0;captures=0;readbacks=0}
let capture_stats t=
  if not(Domain.is_main_domain())then invalid_arg "Workspace_images.capture_stats: initial domain required";
  Sources.length t.source_cache,Sources.length t.capture_cache,
  Sources.bytes t.capture_cache,t.source_cooks,t.attribute_flattens
let error message=Diagnostic.error ~code:"E_IMAGE" message
let message result=Result.map_error error result
let (let*)=Result.bind
let cook_context t (live:Input.t)=Result.map_error error(Sop.Context.create ~seed:t.seed ~grain:t.grain ~domains:t.domains
  ~input:live ~time:live.Input.t ~frame:(Int64.of_int live.frame)())
let diagnostic result=result
let with_request t ?context plan run=match t.request with
  |Some request->run request
  |None->let request={active=[];pending_images=0;session=None;
      contexts=Option.fold ~none:[] ~some:(fun context->[plan,context])context;cooked=[]}in
      t.request<-Some request;
      Fun.protect ~finally:(fun()->Option.iter P.Session.close request.session;t.request<-None)(fun()->run request)
let with_active request plan id run=
  if List.exists(fun(previous,node)->previous==plan && node=id)request.active then
    Error(Diagnostic.error ~code:"E_IMAGE_CYCLE" "Image and geometry dependencies form a cycle.")
  else let previous=request.active in
    request.active<-(plan,id)::previous;
    Fun.protect ~finally:(fun()->request.active<-previous)run
let session request=match request.session with
  |Some session->Ok session
  |None->Result.map(fun session->request.session<-Some session;session)
      (Result.map_error error(P.Session.create ~max_entries:0 ~max_payload_bytes:0))
let source_context t request plan id=
  let contains (context:L.image_context)=Option.fold ~none:false
    ~some:(fun cid->Option.is_some(P.Edit_graph.find context.network.geometry ~node_id:cid))
      (Flow_sop.Network.Int_map.find_opt id context.compiled)in
  match List.find_opt(fun(previous,context)->previous==plan && contains context)request.contexts with
  |Some(_,context)->Ok context
  |None->(match t.lowered with
    |Some lowered when lowered.plan==plan->Result.map(fun context->
        request.contexts<-(plan,context)::request.contexts;context)(L.source_context lowered ~node:id)
    |_->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Capture needs its current owning network."))
let prepare_source ?gpu t request ~state ~live plan id=with_active request plan id(fun()->
  let* owner=source_context t request plan id in
  let state_stamp=E.state_stamp state in
  let* source=match Sources.find_opt t.source_cache id with
    |Some source when source.network==owner.network->Ok source
    |_->let* cone,root=L.source_cone owner ~node:id in
        let source={network=owner.network;cone;id=root;
          lane=Flow_sop.Value_lane.create ~state:(E.fork_state state)();state_stamp;live;geometry=None;
          compiled=None;root=None;dependencies=P.Context.Dependencies.static;projection="";
          revision=0;materializer=None}in
        Sources.add t.source_cache id source;Ok source in
  if source.state_stamp<>state_stamp || (source.cone.states<>[] && not(Input.equal source.live live)) then begin
    source.lane<-Flow_sop.Value_lane.create ~state:(E.fork_state state)();source.state_stamp<-state_stamp
  end;
  let* resolved=Flow_sop.Value_lane.resolve ?gpu source.lane ~live ~time:live.Input.t source.cone in
  let compiled=match source.geometry,source.compiled with
    |Some geometry,Some compiled when geometry==resolved.geometry->compiled
    |_->P.Edit_graph.compile_all ?previous:source.compiled resolved.geometry in
  let* root=Result.map_error(Diagnostic.error ~code:"E_DATA_SOURCE")
    (P.Edit_graph.compiled_node compiled ~node_id:source.id)in
  let unchanged=Option.fold ~none:false ~some:((==)root)source.root in
  let dependencies=if unchanged then source.dependencies else P.Node.graph_dependencies root in
  let* context=cook_context t live in
  let projection=P.Context.cache_projection dependencies context in
  if not unchanged || source.projection<>projection then begin
    t.revision<-t.revision+1;source.revision<-t.revision;
    Sources.remove t.capture_cache id;
    source.materializer<-Some(match source.materializer with
      |None->Flow_sop.Attribute_kernel.materialized_source root
      |Some consumer->P.Node.Private.rebuild_with_inputs consumer [|root|])
  end;
  source.compiled<-Some compiled;source.root<-Some root;source.geometry<-Some resolved.geometry;source.live<-live;
  source.dependencies<-dependencies;source.projection<-projection;
  Ok source)
let retain_capture t id revision captured=
  match Sources.peek t.source_cache id with
  |Some source when source.revision=revision->
      if captured.bytes>capture_limit then Sources.remove t.capture_cache id
      else Sources.add t.capture_cache ~bytes:captured.bytes id (revision,captured)
  |_->()
let captured t request ~live id (source:source)=
  match List.find_opt(fun(node,revision,_)->node=id && revision=source.revision)request.cooked with
  |Some(_,_,captured)->Ok captured
  |None->let* captured=match Sources.find_opt t.capture_cache id with
      |Some(revision,captured)when revision=source.revision->Ok captured
      |_->let* session=session request in
          let* context=cook_context t live in
          let* output=diagnostic(P.Session.cook session ~context (Option.get source.materializer))in
          let* geometry=diagnostic(P.Payload.geometry output.payload)in
          t.source_cooks<-t.source_cooks+1;
          Ok{geometry;attributes=[];bytes=Rdk.Geometry.payload_bytes geometry}in
      request.cooked<-(id,source.revision,captured)::request.cooked;
      retain_capture t id source.revision captured;Ok captured
let resolve_capture t request ~live sources value=match value with
  |E.Struct("sop/attr",_,args)->(match List.assoc_opt "geometry" args,List.assoc_opt "attribute" args with
    |Some(E.Deferred(ty,id)),Some(E.Text name)when Flow.Ty.is_geometry ty->
        let* source=match List.assoc_opt id sources with Some source->Ok source|None->
          Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Attribute source was not prepared.")in
        let* captured=captured t request ~live id source in
        (match List.assoc_opt name captured.attributes with Some value->Ok value|None->
          let* value=Flow_sop.Attribute_kernel.resolve ~geometry:(fun node->
            if node=id then Some captured.geometry else None)value in
          t.attribute_flattens<-t.attribute_flattens+1;
          let size=match value with E.Vec3_array values->Array.length values*8+16|_->0 in
          let bytes=if size>max_int-captured.bytes then max_int else captured.bytes+size in
          let captured={captured with attributes=(name,value)::captured.attributes;bytes}in
          request.cooked<-(id,source.revision,captured)::List.filter(fun(node,_,_)->node<>id)request.cooked;
          retain_capture t id source.revision captured;Ok value)
    |_->Error(Diagnostic.error ~code:"E_ATTR_TYPE" "sop/attr needs geometry and an attribute name."))
  |_->Error(Diagnostic.error ~code:"E_DATA_SOURCE" "Unknown packed data source.")
let create_canvas t ~width ~height=Result.map(fun canvas->
  t.canvases_created<-t.canvases_created+1;canvas)(message(Canvas.create ~width ~height))
let destroy_canvas t canvas=
  let captures,readbacks=Canvas.pixel_stats canvas in
  ignore(Canvas.destroy canvas);
  t.canvases_destroyed<-t.canvases_destroyed+1;
  t.captures<-t.captures+captures;t.readbacks<-t.readbacks+readbacks
let render_stats t=
  let captures,readbacks=List.fold_left(fun(c,r)(_,entry)->match entry.canvas with
    |None->c,r|Some canvas->let captures,readbacks=Canvas.pixel_stats canvas in
        c+captures,r+readbacks)(t.captures,t.readbacks)t.entries in
  t.canvases_created,t.canvases_destroyed,captures,readbacks
let authored_key prefix (node:E.node)=prefix^Marshal.to_string(node.inst,node.site,node.iter)[Marshal.No_sharing]
let invalidate_display entry=entry.display<-None;Option.iter(fun canvas->ignore(Canvas.invalidate canvas))entry.canvas
module Functions=Hashtbl.Make(struct type t=E.fn let equal=(==) let hash=E.Residual.function_id end)
module Residuals=Hashtbl.Make(struct type t=E.residual let equal=(==) let hash=E.Residual.residual_id end)
let size_dependencies plan root=
  let nodes=Array.make(Array.length plan.E.nodes)false
  and functions=Functions.create 8 and residuals=Residuals.create 8 in
  let width=ref false and height=ref false in
  let rec visit value=if not(!width && !height)then match value with
    |E.Deferred(_,id)when id>=0 && id<Array.length nodes && not nodes.(id)->
        nodes.(id)<-true;let node=plan.nodes.(id)in
        if node.kind="image/render"then begin
          width:= !width || not(List.mem_assoc "width" node.args);
          height:= !height || not(List.mem_assoc "height" node.args)
        end;
        List.iter(fun(_,value)->visit value)node.args
    |E.List values->Array.iter visit values
    |E.Record fields|E.Struct(_,_,fields)->List.iter(fun(_,value)->visit value)fields
    |E.Fn fn when not(Functions.mem functions fn)->Functions.add functions fn();
        List.iter(fun(_,value)->visit value)(E.Residual.function_bindings fn)
    |E.Residual residual when not(Residuals.mem residuals residual)->Residuals.add residuals residual();
        List.iter(fun(_,value)->visit value)(E.Residual.residual_view residual).bindings
    |_->()in
  visit(E.Deferred(Flow.Ty.image,root));!width,!height
let image_dependencies plan root=
  let nodes=Array.make(Array.length plan.E.nodes)false and ids=ref[]
  and stateful=ref false and dynamic=ref false
  and functions=Functions.create 8 and residuals=Residuals.create 8 in
  let rec visit value=
    stateful:= !stateful || E.state_dependent value;
    dynamic:= !dynamic || E.is_live value || E.frame_dependent value;
    match value with
    |E.Deferred(ty,id)when Flow.Ty.is_geometry ty->ids:=id:: !ids
    |E.Deferred(_,id)when id>=0 && id<Array.length nodes && not nodes.(id)->
        nodes.(id)<-true;List.iter(fun(_,value)->visit value)plan.nodes.(id).args
    |E.List values->Array.iter visit values
    |E.Record fields|E.Struct(_,_,fields)->List.iter(fun(_,value)->visit value)fields
    |E.Fn fn when not(Functions.mem functions fn)->Functions.add functions fn();
        List.iter(fun(_,value)->visit value)(E.Residual.function_bindings fn)
    |E.Residual residual when not(Residuals.mem residuals residual)->Residuals.add residuals residual();
        List.iter(fun(_,value)->visit value)(E.Residual.residual_view residual).bindings
    |_->()in
  visit(E.Deferred(Flow.Ty.image,root));List.sort_uniq Int.compare !ids,(!stateful,!dynamic || !stateful)
let current ?(sources=[]) t ~state ~live plan (node:E.node) (old:stamp)=
  if not(Option.fold ~none:false ~some:((==)plan)t.plan) || old.serial<>t.serial
    || node.id<0 || node.id>=Array.length t.freshness then false else
  let stateful,dynamic=t.freshness.(node.id) in
  let prepared=match node.kind with
    |"image/map"->old.serial=t.serial && Option.is_some t.maps.(node.id)
    |"image/render"->
        Option.fold ~none:false ~some:((==)plan)t.plan && old.serial=t.serial
        && Option.is_some t.drawings.(node.id)
        && (let width,height=t.sizes.(node.id)in
          (not width || fst old.frame.size=fst live.Input.size)
          && (not height || snd old.frame.size=snd live.size))
    |_->true in
  prepared && old.sources=sources && (not dynamic || (Input.equal live old.frame
    && old.state_stamp=(if stateful then E.state_stamp state else "")))
let bind t (lowered:Flow_sop.Lower.t) =
  t.lowered<-Some lowered;
  if not(Option.fold ~none:false ~some:(fun (plan,approx,image_sites)->plan==lowered.plan
      && approx==lowered.approx && image_sites==lowered.image_sites)t.qualification)then begin
    t.qualification<-Some(lowered.plan,lowered.approx,lowered.image_sites);t.plan<-None
  end
let bytes_of_image image =
  match CPU.rgba8 image with Some bytes->bytes|None->
    let rgba=CPU.storage image in
    (* clamp, scale, round ties to even: the same bytes Sop.Image.of_vec4 writes *)
    Bytes.init(Array.length rgba)(fun i->let q=Float.max 0.(Float.min 1. rgba.(i))*.255. in
      let n=int_of_float(Float.floor q)in let f=q-.float n in
      Char.chr(if f>0.5||(f=0.5&&n land 1=1)then n+1 else n))
let cpu_of_image image =
  let* bytes=message(Image.pixels image)in
  let width,height=Image.dimensions image in
  let rgba=Array.init(Bytes.length bytes)(fun i->float(Char.code(Bytes.get bytes i))/.255.)in
  Result.map_error(fun d->error d.Diagnostic.message)(CPU.of_owned_rgba ~width ~height rgba)
let texture payload=lazy(
  let bytes=bytes_of_image payload in
  Texture.create_owned ~width:(CPU.width payload) ~height:(CPU.height payload)
    (Array.init(Bytes.length bytes/4)(fun i->let o=4*i in
      Rays_math.Color.rgba(Char.code(Bytes.get bytes o))(Char.code(Bytes.get bytes(o+1)))
        (Char.code(Bytes.get bytes(o+2)))(Char.code(Bytes.get bytes(o+3))))))
let publish t key entry ~width ~height ~source=
  let* image=match entry.image with
    |None->Workspace_resources.image t.resources key(fun()->
        match Runtime_resources.Image.of_gpu ~width ~height ~source with
          |Error error->Error(Format.asprintf "%a" Runtime_resources.pp_error error)
        |Ok image->let accepted=ref false in
          Fun.protect ~finally:(fun()->if not !accepted then ignore(Runtime_resources.Image.destroy image))(fun()->
            Result.bind(Texture.of_image image)(fun view->
              Result.map(fun public_image->entry.borrowed<-Some view;accepted:=true;public_image)
                (Image.of_resource image))))
    |Some image->Result.map(fun()->image)
        (Result.map_error(fun e->error(Format.asprintf "%a" Runtime_resources.pp_error e))
          (Runtime_resources.Image.replace_gpu_source (Image.resource image)
            ~width ~height ~source))in
  let* borrowed=match entry.borrowed with Some view->Ok view|None->
    message(Texture.of_image(Image.resource image))in
  entry.image<-Some image;entry.borrowed<-Some borrowed;
  entry.texture<-Some(lazy(Ok borrowed));Ok()
let prepare t plan =
  match t.plan with Some old when old==plan->Ok()|_->
    let programs=Array.map(fun(n:E.node)->if n.ty=Flow.Ty.image then
      Result.map Option.some (I.compile(E.Record n.args))else Ok None)plan.E.nodes in
    match Array.find_opt Result.is_error programs with Some(Error d)->Error d|_->
      t.plan<-Some plan;t.serial<-t.serial+1;t.arguments<-Array.map Result.get_ok programs;
      t.drawings<-Array.make(Array.length plan.nodes)None;
      t.maps<-Array.make(Array.length plan.nodes)None;
      t.sizes<-Array.map(fun(n:E.node)->if n.kind="image/render"then size_dependencies plan n.id else false,false)plan.nodes;
      Sources.clear t.source_cache;Sources.clear t.capture_cache;
      let dependencies=Array.map(fun(n:E.node)->if n.ty=Flow.Ty.image then image_dependencies plan n.id
        else [],(false,false))plan.nodes in
      t.source_ids<-Array.map fst dependencies;t.freshness<-Array.map snd dependencies;
      t.resolved<-Array.make(Array.length plan.nodes)None;Ok()
let peek t plan id=if t.resources.closed || not(Option.fold ~none:false ~some:((==)plan)t.plan)
  || id<0 || id>=Array.length t.resolved then None else Option.bind t.resolved.(id)(fun entry->entry.image)
let rec resolve ?gpu ?context t ~display ~state ~live (plan:E.plan) value =
  if not(Domain.is_main_domain())then Error(error "Image resources must resolve on the initial domain.")else
  if t.resources.closed then Error(error "Workspace image resources are closed.")else
  L.with_images ?gpu ~metadata:(fun plan id->Option.map Image.dimensions(peek t plan id))
    (fun ?gpu ?context plan ~state ~live value->Result.map(fun entry->(Option.get entry.cpu).payload)
      (resolve ?gpu ?context t ~display:false ~state ~live plan value))(fun()->
  with_request t ?context plan(fun request->
  let run()=E.transaction state(fun()->try
    (match value with
    |E.Deferred(Flow.Ty.Named "image",id)when display && id>=0 && id<Array.length plan.nodes->
        let node=plan.nodes.(id)in
        if node.kind="image/render"then
          Option.iter(fun entry->
            if not(Option.fold ~none:false ~some:(fun old->current ~sources:old.sources t ~state ~live plan node old)entry.display)then
              invalidate_display entry)(List.assoc_opt(authored_key "render:" node)t.entries)
    |_->());
    let* ()=prepare t plan in
    match value with
    |E.Deferred(Flow.Ty.Named "image",id)when id>=0 && id<Array.length plan.nodes->
        let* forced=I.force ~state (Option.get t.arguments.(id)) ~live in
        let args=match forced with E.Record args->args|_->assert false in
        let int key default=Option.fold ~none:default ~some:V.int_of(List.assoc_opt key args)in
        let node=plan.nodes.(id)in
        if node.kind="exact" then begin
          let* child=resolve t ~display:true ~state ~live plan (List.assoc "value" args)in
          let source=Option.get child.image in
          let width,height=Image.dimensions source in
          let resource=Image.resource source in
          let* _=Result.map_error(fun e->error(Format.asprintf "%a" Runtime_resources.pp_error e))
            (Runtime_resources.Image.gpu_snapshot resource)in
          let key=authored_key "exact:" node ^ Marshal.to_string
            (t.serial,Image.identity source,Runtime_resources.Image.generation resource,width,height)
            [Marshal.No_sharing]in
          let result=match List.assoc_opt key t.entries with
            |Some entry->Ok entry
            |None->
                if List.length t.entries+request.pending_images>=64 then
                  Error(error "A workspace owns at most 64 image snapshots.")else begin
                request.pending_images<-request.pending_images+1;
                Fun.protect ~finally:(fun()->request.pending_images<-request.pending_images-1)(fun()->
                  let payload=ref None in
                  let* image=Workspace_resources.image t.resources key(fun()->
                    (* Admission precedes the read. Pixels returns owned bytes;
                       the native image copies them and the CPU snapshot owns them. *)
                    let ( let* )=Result.bind in
                    let* bytes=Image.pixels source in
                    let* cpu=Result.map_error(fun d->d.Diagnostic.message)
                      (CPU.of_owned_rgba8 ~width ~height bytes)in
                    let* image=Image.upload_rgba ~width ~height ~rgba:bytes()in
                    payload:=Some cpu;Ok image)in
                  let payload=Option.get !payload in
                  let stamp={serial=t.serial;frame=live;state_stamp="";sources=[]}in
                  let entry={image=Some image;cpu=Some{payload;stamp};texture=Some(texture payload);
                    borrowed=None;display=Some stamp;canvas=None}in
                  t.entries<-(key,entry)::t.entries;Ok entry)
                end in
          Result.map(fun entry->t.resolved.(id)<-Some entry;entry)result
        end else begin
        let* sources=List.fold_left(fun result source->let* sources=result in
          let* prepared=prepare_source ?gpu t request ~state ~live plan source in
          Ok((source,prepared)::sources))(Ok[])t.source_ids.(id)in
        let source_stamp=List.map(fun(id,(source:source))->id,source.revision)sources in
        let key=match node.kind with
          |"image/load"->(match List.assoc "path" args with E.Text path->"load:"^path|_->V.fail "E_IMAGE" "Image path is text.")
          |"image/noise" when List.exists(fun(_,v)->E.is_live v)node.args->Printf.sprintf "noise:%d:%d" t.serial id
          |"image/noise"->"noise:"^Marshal.to_string args [Marshal.No_sharing]
          |"image/render"->authored_key "render:" node
          |"image/map"->authored_key "map:" node
          |_->V.fail "E_IMAGE" "Unknown image producer."in
        let stateful,_=t.freshness.(node.id) in
        let previous=List.assoc_opt key t.entries in
        let state_stamp=if stateful then E.state_stamp state else "" in
        let stamp={serial=t.serial;frame=live;state_stamp;sources=source_stamp}in
        let valid=current ~sources:source_stamp t ~state ~live plan node in
        let result=(match previous with Some entry when
          (if display then Option.fold ~none:false ~some:valid entry.display
           else Option.fold ~none:false ~some:(fun cpu->valid cpu.stamp)entry.cpu)->Ok entry
        |_->
          let reserved=previous=None in
          let* ()=if reserved && List.length t.entries+request.pending_images>=64 then
            Error(error "A workspace owns at most 64 image snapshots.")else Ok()in
          if reserved then request.pending_images<-request.pending_images+1;
          Fun.protect ~finally:(fun()->if reserved then request.pending_images<-request.pending_images-1)(fun()->
          let entry=match previous with Some entry->entry|None->
            {image=None;cpu=None;texture=None;borrowed=None;display=None;canvas=None}in
          if display then invalidate_display entry;
          let upload ~width ~height payload=
            match entry.image with
            |Some image->message(Image.upload_rgba ~into:image ~width ~height ~rgba:(bytes_of_image payload)())
            |None->Workspace_resources.image t.resources key(fun()->Image.upload_rgba ~width ~height ~rgba:(bytes_of_image payload)())in
          let* ()=if node.kind="image/map" then begin
            let width=int "width" 256 and height=int "height" 256 in
            let fn=match List.assoc "function" args with E.Fn fn->fn
              |_->V.fail "E_IMAGE" "Image map needs a pixel function."in
            let ids=Flow_sop.Attribute_kernel.sources(E.Fn fn)in
            let inputs=List.map(fun id->Option.get(List.assoc id sources).root)ids in
            let prepare()=
                  let path,approx=match t.qualification with
                    |Some(bound,approx,image_sites)when bound==plan->image_sites.(id),approx
                    |_->None,Flow.Workspace.Paths.empty in
                  Result.map(fun p->t.maps.(id)<-Some(width,height,fn,p);p)
                    (Flow_sop.Image_kernel.prepare ?path ~approx ~site:(node.inst,node.site,node.iter)
                      ~identity:(Sop.Node.Private.fresh_id()) ~width ~height ~fn ~sources:ids inputs)in
            let* prepared=match t.maps.(id)with
              |Some(w,h,f,p)when w=width && h=height && f==fn->
                  (match Flow_sop.Image_kernel.with_inputs p inputs with
                    |Ok p->t.maps.(id)<-Some(width,height,fn,p);Ok p|Error _->prepare())
              |_->prepare()in
            let cook()=match entry.cpu with Some cpu when valid cpu.stamp->Ok cpu.payload|_->
              let source=Flow_sop.Image_kernel.node ~state:(E.fork_state state) prepared in
              let* context=cook_context t live in
              let* session=session request in
              let* payload=(
                let* cooked=diagnostic(Sop.Session.cook session ~context source)in
                diagnostic(Sop.Payload.image cooked.payload))in
              entry.cpu<-Some{payload;stamp};Ok payload in
            if not display then Result.map ignore(cook())else
            let selected()=match gpu with
              |Some gpu->I.try_display ~gpu ~state ~resolve:(resolve_capture t request ~live sources)
                  (Flow_sop.Image_kernel.program prepared) ~live
              |None->Workspace_gpu.with_backend t.gpu(fun gpu->
                  I.try_display ~gpu ~state ~resolve:(resolve_capture t request ~live sources)
                    (Flow_sop.Image_kernel.program prepared) ~live)in
            let* selected=selected()in
            match selected with
            |None->let* payload=cook()in let* image=upload ~width ~height payload in
                entry.image<-Some image;entry.texture<-Some(texture payload);Ok()
            |Some(Cpu _)->Error(error "Image display unexpectedly requested a GPU readback.")
            |Some(Gpu value)->
                Workspace_gpu.image t.gpu ~key ~width ~height value ~publish:(fun output->
                let source()=Flow_gpu.Image_sink.texture output in
                publish t key entry ~width ~height ~source)
          end else if node.kind="image/render"then begin
              let width=int "width" (fst live.Input.size)and height=int "height" (snd live.size)in
              if width<=0||height<=0||width>Sys.max_string_length/4/height then Error(error
                "Image dimensions exceed native storage bounds.")else
              let drawing=List.assoc "drawing" args in
              let* prepared=match t.drawings.(id)with Some p->Ok p|None->
                Result.map(fun p->t.drawings.(id)<-Some p;p)(Drawing.prepare plan drawing)in
              let temporary=ref[]in
              Fun.protect ~finally:(fun()->List.iter(fun image->ignore(Image.destroy image))!temporary)(fun()->
              let* scene=Drawing.render_prepared ~state
                ~image:(fun value->
                  let* child=resolve t ~display ~state ~live plan value in
                  if display then Ok(Option.get child.image)else
                  let payload=(Option.get child.cpu).payload in
                  let width=CPU.width payload and height=CPU.height payload in
                  if Option.fold ~none:false ~some:((==)(Option.get child.cpu).stamp)child.display then
                    Ok(Option.get child.image)else match child.image with
                  |Some image when Runtime_resources.Image.gpu_snapshot
                      (Image.resource image)=Ok None->
                      child.display<-None;
                      message(Image.upload_rgba ~into:image ~width ~height ~rgba:(bytes_of_image payload)())
                  |None->
                      let key=fst(List.find(fun(_,candidate)->candidate==child)t.entries)in
                      let* image=Workspace_resources.image t.resources key(fun()->
                        Image.upload_rgba ~width ~height ~rgba:(bytes_of_image payload)())in
                      child.image<-Some image;Ok image
                  |Some _->let* image=message(Image.upload_rgba ~width ~height ~rgba:(bytes_of_image payload)())in
                      temporary:=image:: !temporary;Ok image)
                prepared ~live ~size:(width,height)in
              if display then begin
                let previous=entry.canvas in
                let* canvas,created=match previous with
                  |Some canvas->let* size=message(Canvas.size canvas)in
                      if size=(width,height)then Ok(canvas,false)
                      else Result.map(fun canvas->canvas,true)(create_canvas t~width~height)
                  |_->Result.map(fun canvas->canvas,true)(create_canvas t ~width ~height)in
                let accepted=ref false in
                Fun.protect ~finally:(fun()->if created && not !accepted then destroy_canvas t canvas)(fun()->
                  let* ()=message(Canvas.render canvas scene)in
                  let* width,height,source=message(Canvas.gpu_source canvas)in
                  let* ()=publish t key entry ~width ~height ~source in
                  entry.canvas<-Some canvas;accepted:=true;
                  if created then Option.iter(destroy_canvas t)previous;
                  Ok())
              end else
              let* canvas=create_canvas t ~width ~height in
              Fun.protect ~finally:(fun()->destroy_canvas t canvas)(fun()->
                let* ()=message(Canvas.render canvas scene)in
                let* image=message(Canvas.to_image canvas)in
                Fun.protect ~finally:(fun()->ignore(Image.destroy image))(fun()->
                  let* payload=cpu_of_image image in entry.cpu<-Some{payload;stamp};Ok())))
          end else begin
          entry.display<-None;
          let* image,payload=match node.kind with
          |"image/load"->let path=match List.assoc "path" args with E.Text path->path|_->assert false in
              let* image=Workspace_resources.image t.resources key (fun()->Image.load path)in
              let* payload=cpu_of_image image in Ok(image,payload)
          |"image/noise"->
              let factory=Sop.Image_nodes.noise_factory in
              let* source=Result.map_error error(Sop.Edit_graph.instantiate factory [])in
              let changes=Sop.Edit_graph.factory_fields factory |> List.filter_map(fun(field:Param.field_view)->
                Option.map(fun value->field.name,match field.kind with
                  | Integer_view _->Param.Int_value(V.int_of value)
                  | Floating_view _->Param.Float_value(V.num value)
                  | _->assert false)(List.assoc_opt field.name args))in
              let* source,_=Result.map_error error(Sop.Node.apply_parameters source changes)in
              let* context=Result.map_error error(Sop.Context.create ())in
              let* cooked=Result.map_error(fun d->error d.Diagnostic.message)
                (Sop.Node.Private.cook source context [||])in
              let* payload=Result.map_error(fun d->error d.Diagnostic.message)(Sop.Payload.image cooked.payload)in
              let width=Sop.Image.width payload and height=Sop.Image.height payload in
              let* image=upload ~width ~height payload in
              Ok(image,payload)
          |_->assert false in
          entry.cpu<-Some{payload;stamp};entry.image<-Some image;
          entry.texture<-Some(texture payload);Ok()end in
          if display then entry.display<-Some stamp;
          if previous=None then t.entries<-(key,entry)::t.entries;Ok entry))in
        Result.map(fun entry->t.resolved.(id)<-Some entry;entry)result
        end
    |_->Error(error "Expected an image value.")
    with Diagnostic.Error d -> Error d
      |Not_found->Error(error "Image source is not bound to a compiled input."))in
  let result=match value with E.Deferred(_,id)->with_active request plan id run|_->run()in
  if display && Result.is_error result then (match value with
    |E.Deferred(_,id)when id>=0 && id<Array.length plan.nodes->
        let node=plan.nodes.(id)in
        let prefix=match node.kind with "image/render"->Some "render:"|"image/map"->Some "map:"|_->None in
        Option.iter(fun prefix->Option.iter invalidate_display(List.assoc_opt(authored_key prefix node)t.entries))prefix
    |_->());result))
let image ?gpu ?(display=true) t ~state ~live plan value=
  let* entry=resolve ?gpu t ~display ~state ~live plan value in
  if display then Ok(Option.get entry.image)else
  let cpu=Option.get entry.cpu in
  if Option.fold ~none:false ~some:((==)cpu.stamp)entry.display then Ok(Option.get entry.image)else
  let width=CPU.width cpu.payload and height=CPU.height cpu.payload in
  let* image=match entry.image with
    |Some image->message(Image.upload_rgba ~into:image ~width ~height ~rgba:(bytes_of_image cpu.payload)())
    |None->let key=fst(List.find(fun(_,candidate)->candidate==entry)t.entries)in
        Workspace_resources.image t.resources key(fun()->Image.upload_rgba ~width ~height ~rgba:(bytes_of_image cpu.payload)())in
  entry.image<-Some image;entry.texture<-Some(texture cpu.payload);entry.display<-Some cpu.stamp;Ok image
let payload t ?gpu ?context plan ~state ~live value=Result.map(fun entry->(Option.get entry.cpu).payload)(resolve
  ?gpu ?context t ~display:false ~state ~live plan value)
let texture ?gpu t ~state ~live plan value=Result.bind(resolve ?gpu t ~display:true ~state ~live plan
  value)(fun entry->message(Lazy.force(Option.get entry.texture)))
let close t=
  List.iter(fun(_,entry)->Option.iter(destroy_canvas t)entry.canvas)t.entries;
  t.plan<-None;t.qualification<-None;t.arguments<-[||];t.drawings<-[||];
  t.lowered<-None;Sources.clear t.source_cache;Sources.clear t.capture_cache;t.source_ids<-[||];t.freshness<-[||];
  t.maps<-[||];t.sizes<-[||];t.resolved<-[||];t.entries<-[]
