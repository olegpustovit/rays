module E = Flow.Eval
module P = Sop
open Test_support
let cooked = function Ok x -> x | Error d -> failwith (Diagnostic.error_to_string d)
let parse text = Flow.Syntax.parse text |> ok
let catalog = Flow.Catalog.{version=1;kinds=[]}
let check text =
  match Flow.Workspace.check catalog (parse text) with
  | Some w, _ -> w
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let fn text =
  let w = check text in
  let evaluated = E.static w |> ok in
  let producer = Array.find_opt (fun (n:E.node) -> n.kind="image/map") evaluated.plan.nodes |> Option.get in
  match List.assoc "function" producer.args with E.Fn f -> f | _ -> assert false
let prepare ?(width=387) ?(height=91) fn =
  Flow_sop.Image_kernel.prepare ~identity:(P.Node.Private.fresh_id ()) ~width ~height ~fn ~sources:[] [] |> ok
let cook ?(time=0.) domains prepared =
  let session = P.Session.create ~max_entries:0 ~max_payload_bytes:0 |> Result.get_ok in
  Fun.protect ~finally:(fun () -> P.Session.close session) (fun () ->
    let context = P.Context.create ~domains ~time () |> Result.get_ok in
    let output = P.Session.cook session ~context (Flow_sop.Image_kernel.node prepared) |> cooked in
    P.Payload.image output.payload |> cooked)
let bytes image = P.Image.rgba8 image |> Option.get
let () =
  let text = "(workspace w (graph img :context image (let* [picture
    (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 387 :height 91)] picture)))" in
  let w = check text in
  let projection = Flow_graph.Projection.of_graph catalog w "img" in
  let card = List.find (fun (n:Flow_graph.Projection.node) -> n.path=["img";"picture"]) projection.nodes in
  assert (List.exists (fun (r:Flow_graph.Projection.row) -> r.key=Flow_graph.Flow_edit.Pos 0
    && r.ty=Some (Flow.Ty.Fn (Some {params=[Flow.Ty.Vec2];result=Flow.Ty.Vec4}))) card.rows);
  assert (List.exists (fun (n:Flow_graph.Projection.node) ->
    Option.fold ~none:false ~some:(fun (z:Flow_graph.Projection.zone) -> z.kind=Fn) n.zone) projection.nodes);
  let _, edited = Flow_graph.Flow_edit.apply_checked catalog (parse text)
    (Set_arg {node=["img";"picture"];key=Pos 0;sub=[];
      value=List.hd (parse "(fn [uv] [uv.y uv.x 0.5 1])")}) |> ok in
  ignore (E.static edited |> ok);
  ignore (Flow_graph.Flow_edit.apply_checked catalog (parse text)
    (Set_arg {node=["img";"picture"];key=Kw "width";sub=[];value=List.hd (parse "65")}) |> ok);
  List.iter (fun body ->
    let text = "(workspace w (graph img :context image (image/map "^body^")))" in
    if fst (Flow.Workspace.check catalog (parse text))<>None then failwith ("Unexpectedly accepted: "^body))
    ["(fn [uv] uv.x)";"(fn [(uv : vec3)] [uv.x uv.y 0 1])";"(fn [u v] [0 0 0 1])"];
  (let text = "(workspace w (defn tex :context image [(f : fn)] (image/map f :width 8 :height 8)) (graph img :context image (tex (fn [uv] [uv.x uv.y 0 1]))))" in
   if fst (Flow.Workspace.check catalog (parse text))=None then failwith "image/map through a defn fn parameter was refused");
  let kernel = prepare (fn text) in
  let packed = match Flow_ir.Executor.packed_region (Flow_sop.Image_kernel.program kernel) with
    | Some p -> p | None -> assert false in
  let inputs = Flow_ir.Packed.Host.prepare packed ~live:(Input.at_time 0.) |> ok in
  let saved = Array.copy inputs.arrays.(0) in
  let one = cook 1 kernel and eight = cook 8 kernel in
  assert (bytes one=bytes eight && P.Image.payload_bytes one=387*91*4);
  assert (inputs.arrays.(0)=saved);
  let reference = Flow_ir.Executor.force ~reference:true (Flow_sop.Image_kernel.program kernel)
      ~live:(Input.at_time 0.) |> ok in
  let reference = match reference with E.Vec4_array xs -> xs | _ -> assert false in
  let uv_byte i size =
    let numerator = (2*i+1)*255 and denominator = 2*size in
    let n = numerator/denominator and remainder = numerator mod denominator in
    if 2*remainder > denominator || (2*remainder=denominator && n mod 2=1) then n+1 else n in
  for y=0 to 90 do for x=0 to 386 do
    let i=(y*387+x)*4 in
    let u=(float x+.0.5)/.387. and v=(float y+.0.5)/.91. in
    assert (reference.(i)=u && reference.(i+1)=v && reference.(i+2)=0.5 && reference.(i+3)=1.);
    List.iteri (fun c n -> assert (Char.code (Bytes.get (bytes one) (i+c))=n))
      [uv_byte x 387;uv_byte y 91;128;255]
  done done;
  let constant = fn "(workspace w (graph img :context image
    (image/map (fn [uv] [0.25 0.5 0.75 1]))))" |> prepare in
  let expected = Bytes.init (387*91*4) (fun i ->
    Char.chr (match i mod 4 with 0 -> 64 | 1 -> 128 | 2 -> 191 | _ -> 255)) in
  List.iter (fun domains -> assert (bytes (cook domains constant)=expected)) [1;8];
  let live = fn "(workspace w (defn render :context image [(bias : float)]
      (image/map (fn [uv] [(+ uv.x bias) uv.y 0.5 1]) :width 65 :height 3))
      (graph img :context image (render (* t 0.25))))" |> prepare ~width:65 ~height:3 in
  assert (bytes (cook ~time:0. 1 live)=bytes (cook ~time:0. 8 live));
  assert (bytes (cook ~time:1. 1 live)=bytes (cook ~time:1. 8 live));
  assert (bytes (cook ~time:0. 1 live)<>bytes (cook ~time:1. 1 live));
  let named = fn "(workspace w (defn pixel :context value [(uv : vec2)] [uv.x uv.y t 1])
    (graph img :context image (image/map pixel :width 65 :height 3)))" |> prepare ~width:65 ~height:3 in
  assert (bytes (cook ~time:0.5 1 named)=bytes (cook ~time:0.5 8 named));
  List.iter (fun (width,height) ->
    assert (Result.is_error (Flow_sop.Image_kernel.prepare ~identity:0 ~width ~height
      ~fn:(fn text) ~sources:[] []))) [0,1;1,0;max_int,2;1,max_int];
  let converted = fn "(workspace w (graph img :context image
    (image/map (fn [uv] [(floor uv.x) (floor uv.y) 0 1]))))" |> prepare ~width:65 ~height:3 in
  let expected = Bytes.init (65*3*4) (fun i -> if i mod 4=3 then Char.chr 255 else Char.chr 0) in
  List.iter (fun domains -> assert (bytes (cook domains converted)=expected)) [1;8];
  let unsupported = fn "(workspace w (graph img :context image (image/map (fn [uv] [(value/rand uv.x) 0 0 1]))))" in
  assert (Result.is_error (Flow_sop.Image_kernel.prepare ~identity:0 ~width:1 ~height:1
    ~fn:unsupported ~sources:[] []));
  let stateful = fn "(workspace w (graph img :context image
    (let* [bias (state [n 0.0] (+ n 0.1))]
      (image/map (fn [uv] [uv.x uv.y bias 1])))))" in
  (match Flow_sop.Image_kernel.prepare ~identity:0 ~width:1 ~height:1 ~fn:stateful ~sources:[] [] with
   | Error d -> assert (d.code="E_PACKED_STATE") | Ok _ -> assert false);
  print_endline "image/map: typed function, graph gestures, UV orientation, reference/packed/domain bytes and live captures pass"

let () =
  let w=check "(workspace pixels
    (defn render :context image [(bias : float)]
      (let* [picture (image/map (fn [uv] [(+ uv.x bias) uv.y t 1]) :width 65 :height 3)] picture))
    (defn invoke :context image [(alias : fn)] (alias 0.125))
    (graph img :context image (invoke :alias render)))" in
  let lowered=Flow_sop.Lower.of_checked ~factories:[] w |> ok in
  let producer=Array.find_opt(fun(n:E.node)->n.kind="image/map")lowered.plan.nodes |> Option.get in
  let path=lowered.image_sites.(producer.id) |> Option.get in
  assert(path=["def:render";"picture"] && path<>producer.site);
  assert(Flow.Workspace.Paths.mem path lowered.approx);
  let fn=match List.assoc "function" producer.args with E.Fn fn->fn|_->assert false in
  let prepare ?path approx=Flow_sop.Image_kernel.prepare ?path ~approx
    ~site:(producer.inst,producer.site,producer.iter) ~identity:0 ~width:387 ~height:91 ~fn ~sources:[] [] |> ok in
  let qualified=prepare ~path lowered.approx in
  let unqualified=prepare Flow.Workspace.Paths.empty in
  let no_provenance=prepare lowered.approx in
  let calls=ref 0 in
  let backend:Flow_ir.Gpu.backend={cost=(fun ~fast:_ _ ~count:_->Some 0.);prepare=(fun ~fast:_ packed->
    assert(Flow_ir.Packed.site packed=(producer.inst,path,producer.iter));
    Ok{run=(fun inputs->incr calls;Ok Flow_ir.Gpu.{identity=1;count=inputs.count;width=4;
      output=0;stamp=Int64.of_int !calls;gpu_seconds=None});readback=(fun _->assert false)})}in
  let gpu={Flow_ir.Gpu.default with backend=Some backend;policy=Qualification} in
    List.iter(fun kernel->assert(ok(Flow_ir.Executor.try_display ~gpu
      (Flow_sop.Image_kernel.program kernel) ~live:(Input.at_time 0.5))=None))
      [unqualified;no_provenance];
    assert(!calls=0);
    (match ok(Flow_ir.Executor.try_display ~gpu
        (Flow_sop.Image_kernel.program qualified) ~live:(Input.at_time 0.5))with
     |Some(Gpu output)->assert(output.width=4 && output.count=387*91)|_->assert false);
    assert(!calls=1);
  let exact=cook ~time:0.5 1 qualified in
  assert(bytes exact=bytes(cook ~time:0.5 8 qualified));
  assert(bytes exact=bytes(cook ~time:0.5 1 unqualified));
  assert(bytes exact=bytes(cook ~time:0.5 1 no_provenance));
  let copied=Bytes.copy(bytes exact)in
  ignore(cook ~time:0.75 1 qualified);
  assert(bytes exact=copied);
  print_endline "image lowering: named-call authored provenance, qualified display selection and independent exact CPU snapshots pass"

let () =
  let module W=Flow.Workspace in
  let w=check "(workspace pixels (graph img :context image
    (let* [a (image/map (fn [uv] [uv.x uv.y t 1]))
      b (map (fn [x] (+ x t)) (array/float 4))] a)))" in
  let graph=List.hd w.graphs in
  let bindings,body=match graph.body.node with W.Let(bindings,body)->bindings,body|_->assert false in
  let a=["img";"a"] in
  let b=List.assoc (W.Name "b") bindings in
  (* Hand-built conflicting kernel observations at one runtime site must poison the
     bridge independently of the qualifier's authored-path conclusions. *)
  let roots=(b.form,["unknown:a"])::(b.form,["unknown:b"])::
    List.filter(fun(form,_)->form!=b.form)w.packed_roots in
  let bindings=List.map(fun(name,(term:W.term))->
    name,if name=W.Name "b"then {term with path=Some a}else term)bindings in
  List.iter(fun bindings->
    let graph={graph with body={graph.body with node=W.Let(bindings,body)}}in
    let lowered=Flow_sop.Lower.of_checked ~factories:[] {w with graphs=[graph];packed_roots=roots} |> ok in
    assert(W.Paths.mem a lowered.approx);
    let images=Array.to_list lowered.plan.nodes |> List.filter(fun(n:E.node)->n.kind="image/map")in
    assert(List.length images=1);
    List.iter(fun(n:E.node)->assert(n.site=a && lowered.image_sites.(n.id)=None))images)
    [bindings;List.rev bindings];
  print_endline "image lowering: missing provenance is sticky before and after a successful observation"
