open Rdk
open Rdk_test_support
module E = Flow.Eval
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
module Session = Sop.Session
let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let factories = Sop.Nodes.factories
let catalog = Flow_sop.Catalog.of_factories ~version:1 factories |> flow_ok
let source ?(columns = 33) ?(amp = "0.8") body = Printf.sprintf
  "(workspace kernel (graph g :context sop (let* [g (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns %d :rows 2) amp %s freq 0.16 result %s] result)))"
  columns amp body
let noise_body = "(sop/with_attr g :P (exact (map (fn [p n] (+ p (* n (* amp (noise3 (* p freq)))))) (sop/attr g :P) (sop/attr g :N))))"
let lower ?previous ?gpu text =
  let forms = Flow.Syntax.parse text |> flow_ok in
  let compiled_ids = Option.map (fun (l : L.t) -> l.compiled_ids) previous
  and sites = Option.map (fun (l : L.t) -> l.sites) previous in
  match L.workspace ~factories ?gpu ?compiled_ids ?sites forms with
  | Ok lowered -> lowered
  | Error d -> failwith (text ^ "\n" ^ Diagnostic.to_string d)
let compiled (l : L.t) =
  let graph = List.hd l.graphs in
  Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok
let context ~domains time = Sop.Context.create ~domains ~time ~grain:257 () |> get_string_ok
let session () = Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
let cook session ~domains ~time node = Session.cook session ~context:(context ~domains time) node
let geometry = function Ok (output : Session.output) -> (Result.get_ok (Sop.Payload.geometry output.payload))
  | Error d -> failwith (Diagnostic.error_to_string d)
let reference (l : L.t) node =
  let n = Array.find_opt (fun (n : E.node) -> n.kind = "sop/with_attr") l.plan.nodes |> Option.get in
  let values = List.assoc "values" n.args in
  let sources = match List.assoc "geometry" n.args with E.Deferred (_, id) ->
    id :: List.filter ((<>) id) (Flow_sop.Attribute_kernel.sources values) | _ -> assert false in
  Flow_sop.Attribute_kernel.node ~reference:true ~source:"reference"
    ~name:"P" ~values ~sources (Sop.Node.inputs node)
let stages program = Option.fold ~none:0 ~some:(fun (r : Flow_ir.Core.Region.t) -> List.length r.stages)
  (Flow_ir.Executor.packed_root program)

let () =
  let pack width planes=match width with
    |1->Attribute.Float planes.(0)
    |2->Float2(Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1)|>get_string_ok)
    |3->Float3(Packed.Float3.View.of_owned_exn ~x:planes.(0) ~y:planes.(1) ~z:planes.(2))
    |_->Float4(Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) ~w:planes.(3)|>get_string_ok) in
  let array width values=match width with 1->E.Float_array values|2->Vec2_array values|3->Vec3_array values|_->Vec4_array values
  and row width values=match width with 1->E.Float values.(0)|2->Vec2(values.(0),values.(1))
    |3->Vec3(values.(0),values.(1),values.(2))|_->Vec4(values.(0),values.(1),values.(2),values.(3)) in
  List.iter(fun count->
    let base=Line_geometry.points(Array.init count(fun i->float i,0.,0.)) in
    let base=if count=6 then Geometry.create ~positions:(Geometry.positions base)
      ~topology:(Topology.polygons_owned ~point_count:6 ~vertex_points:[|0;1;2;3;4;5|] ~primitive_offsets:[|0;3;6|]|>get_string_ok)()
        |>get_string_ok else base in
    let owners=if count=6 then [Attribute.Point,"point",6;Vertex,"vertex",6;Primitive,"primitive",2;Detail,"detail",1]
      else [Attribute.Point,"point",count] in
    List.iter(fun(owner,owner_name,count)->List.iter(fun(kind,width)->
      let replacements=Array.init(count*width)(fun i->if i mod width=0 then -0. else float(i mod width)+.0.25)
      and defaults=Array.init width(fun i->float i+.0.75) in
      let values=array width replacements and default=row width defaults in
      let old=Array.init width(fun c->Array.init count(fun i->
        if c=width-1 then [|Int64.float_of_bits 0x7ff8000000001234L;infinity;neg_infinity;-0.|].(i mod 4)
        else float i+.float c)) in
      List.iter(fun mode->
        let original=if mode="absent" then base else Geometry.with_attribute
          (Attribute.create_owned ~owner ~name:"selected" (if mode="wrong" then Int(Array.make count 7) else pack width old)|>get_string_ok)base|>get_string_ok in
        let before=geometry_bytes original in
        List.iter(fun select->
          let mask=Array.init count select in
          let expected=Array.init width(fun c->Array.init count(fun i->
            if mask.(i)=1 then replacements.(i*width+c) else if mode="matching" then old.(c).(i) else defaults.(c))) in
          let expected=Geometry.with_attribute(Attribute.create_owned ~owner ~name:"selected"(pack width expected)|>get_string_ok)original|>get_string_ok in
          List.iter(fun domains->Parallel.run ~domains(fun()->
            let output=Flow_sop.Attribute_kernel.write_selected ~grain:97 ~owner ~kind "selected"
              ~values ~selection:(E.Int_array mask) ~default original|>flow_ok in
            assert(geometry_bytes output=geometry_bytes expected);
            assert(geometry_bytes original=before))) [1;8];
          if count<=6 then begin
            let literal=if width=1 then string_of_float defaults.(0) else
              "["^String.concat " "(Array.to_list(Array.map string_of_float defaults))^"]" in
            let values_literal=if width=1 then "-0.0" else "["^String.concat " "(List.init width(fun c->if c=0 then "-0.0" else string_of_float(float c+.0.25)))^"]" in
            let mask_literal="(array/int "^string_of_int count^" "^string_of_int(select 0)^")" in
            if count=0 || Array.for_all((=)(select 0))mask then
              List.iter(fun wrapped->List.iter(fun reference->
                let replacement=if kind="vec2" then Printf.sprintf
                  "(exact (map (fn [(i : int)] %s) (array/int %d 0)))" values_literal count
                  else Printf.sprintf "(array/%s %d %s)" kind count values_literal in
                let expression=Printf.sprintf "(sop/with_attr geo :selected %s :owner %s :type %s :selection %s :default %s)"
                  replacement owner_name kind mask_literal literal in
                let text=if wrapped then "(workspace selected_writer (defnode sop/selected_writer [(geo : geometry)] "^expression^
                  ") (graph g :context sop (sop/selected_writer (sop/ext_source))))"
                  else "(workspace selected_writer (graph g :context sop (let* [geo (sop/ext_source)] "^expression^")))" in
                let input=Lisp_sop.snapshot original in
                let factory=Edit.factory ~key:"ext_source" ~label:"Source" ~category:["Test"] ~arity:0(fun _->Ok input) in
                let node=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse text|>flow_ok)|>flow_ok|>compiled in
                let session=session() in
                Fun.protect ~finally:(fun()->Session.close session)(fun()->
                  assert(geometry_bytes(cook session ~domains:1 ~time:0. node|>geometry)=geometry_bytes expected))) [false;true]) [false;true]
          end)[(fun _->0);(fun _->1);(fun i->i mod 2)];
        let invalid selection default values code=
          (match Flow_sop.Attribute_kernel.write_selected ~grain:97 ~owner ~kind "selected" ~selection ~default ~values original with
           |Error d->assert(d.code=code)|Ok _->assert false);
          assert(geometry_bytes original=before) in
        invalid(E.Int_array(Array.make(count+1)0))default values "E_ATTR_COUNT";
        invalid(E.Int_array(Array.make count 0))(E.Text "wrong")values "E_ATTR_TYPE";
        if count>0 then begin
          invalid(E.Int_array(Array.make count 2))default values "E_ATTR_SELECTION";
          let bad=Array.copy replacements in bad.(0)<-nan;
          invalid(E.Int_array(Array.make count 0))default(array width bad)"E_NONFINITE";
          let late=Array.make count 0 in late.(count-1)<- -1;
          invalid(E.Int_array late)default(array width bad)"E_ATTR_SELECTION"
        end)["absent";"wrong";"matching"])
      ["float",1;"vec2",2;"vec3",3;"vec4",4])owners)[0;6;32769];
  print_endline "Selected attribute writes: all numeric owners, native nonfinite untouched bytes, explicit defaults, strict/reference and one/eight-domain ownership/refusals pass"

let () =
  let original=Line_geometry.points[|0.,0.,0.;1.,0.,0.;2.,0.,0.;3.,0.,0.;4.,0.,0.;5.,0.,0.|] in
  let group=Group.init ~owner:Group.Point ~name:"chosen" 6(fun i->i mod 2=0) in
  let mask_geometry=Geometry.with_group group original|>get_string_ok in
  let text={|(workspace live_selected
    (defnode sop/live_selected [(geo : geometry) (mask_geo : geometry)]
      (sop/with_attr geo :live (array/float (sop/count geo :points) 1.5) :type float
        :selection (exact (map (fn [(x : int)] (if (> t 0) x (int/sub 1 x)))
          (sop/group mask_geo :name "chosen"))) :default (+ 0.25 (* t 0.125))))
    (graph g :context sop (sop/live_selected (sop/ext_input) (sop/ext_mask))))|} in
  let input=Lisp_sop.snapshot original and mask=Lisp_sop.snapshot mask_geometry in
  let factories=Edit.factory ~key:"ext_input" ~label:"Input" ~category:["Test"] ~arity:0(fun _->Ok input)::
    Edit.factory ~key:"ext_mask" ~label:"Mask" ~category:["Test"] ~arity:0(fun _->Ok mask)::factories in
  List.iter(fun reference->
    let node=L.workspace ~reference ~factories(Flow.Syntax.parse text|>flow_ok)|>flow_ok|>compiled in
    let session=session() in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->List.iter(fun time->List.iter(fun domains->
      let expected=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"live"
        (Float(Array.init 6(fun i->if (i mod 2=0)=(time>0.) then 1.5 else 0.25+.time*.0.125)))|>get_string_ok)original|>get_string_ok in
      assert(geometry_bytes(cook session ~domains ~time node|>geometry)=geometry_bytes expected)) [1;8]) [-1.;1.;2.;-1.])) [false;true];
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse{|(workspace selected_projection (graph g :context sop
    (let* [geo (sop/line) mask (array/int 2 1)
      result (sop/with_attr :selection mask :default -0.0 geo :weight (array/float 2 1.0) :type float)] result)))|}|>flow_ok in
  let workspace=match checker catalog forms with Some w,[]->w|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  let card workspace=Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog workspace "g")["g";"result"]|>Option.get in
  assert((card workspace).ty=Flow.Ty.geometry);
  List.iter(fun(key,value)->ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"result"];key;sub=[];value=Flow.Syntax.make value})|>flow_ok))
    [Kw "default",Flow.Syntax.Num "0.5";Kw "selection",Sym "mask"];
  List.iter(fun suffix->
    let source="(workspace bad_selection (graph g :context sop (sop/with_attr (sop/line) :weight (array/float 2 1.0) :type float "^suffix^")))" in
    let _,ds=checker catalog(Flow.Syntax.parse source|>flow_ok) in
    assert(List.exists(fun(d:Diagnostic.t)->d.code="E_ATTR_SELECTION" || d.code="E_ATTR_TYPE")ds))
    [":selection (array/int 2 1)";":default 1.0";":selection (array/int 2 1) :default \"wrong\""];
  print_endline "Selected writer: captured geometry masks, changing live defaults/selection and paired typed graph gestures pass"

let () =
  let original=Line_geometry.points[|Int64.float_of_bits 0x7ff8000000001234L,0.,0.;
    infinity,neg_infinity,-0.;-0.,0.,Float.min_float|] in
  let original=Geometry.with_group(Group.init ~owner:Point ~name:"chosen" 3((=)0))original|>get_string_ok in
  let before=geometry_bytes original and positions=Packed.Float3.View.view(Geometry.positions original) in
  let x=Array.copy positions.x and y=Array.copy positions.y and z=Array.copy positions.z in
  x.(0)<- -0.;y.(0)<-0.5;z.(0)<-Float.min_float;
  let expected=Geometry.with_positions(Packed.Float3.View.of_owned_exn ~x ~y ~z)original|>get_string_ok|>geometry_bytes in
  let input=Lisp_sop.snapshot original in
  let factories=Edit.factory ~key:"ext_raw_positions" ~label:"Raw positions" ~category:["Test"] ~arity:0(fun _->Ok input)::factories in
  let forms=Flow.Syntax.parse{|(workspace selected_positions
    (defnode sop/selected_positions [(geo : geometry)]
      (sop/with_attr geo :P (array/vec3 (sop/count geo :points) [-0.0 0.5 2.2250738585072014e-308])
        :selection (sop/group geo :name "chosen") :default [0.0 0.0 0.0]))
    (graph g :context sop (sop/selected_positions (sop/ext_raw_positions))))|}|>flow_ok in
  List.iter(fun reference->let node=L.workspace ~reference ~factories forms|>flow_ok|>compiled in
    let session=session() in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->List.iter(fun domains->
      assert(geometry_bytes(cook session ~domains ~time:0. node|>geometry)=expected);
      assert(geometry_bytes original=before);Session.clear session)[1;8]))[false;true];
  print_endline "Selected canonical P: overwritten NaN and untouched infinity/signed-zero bytes match native positions without mutating input"

let () =
  let count=32769 in
  let original=Line_geometry.points(Array.init count(fun i->float i,0.,0.)) in
  let original=Geometry.with_group(Group.init ~owner:Point ~name:"chosen" count(fun i->i mod 2=0))original|>get_string_ok in
  let input=Lisp_sop.snapshot original in
  let factories=Edit.factory ~key:"ext_selected_allocation" ~label:"Selected input" ~category:["Test"] ~arity:0(fun _->Ok input)::factories in
  let forms=Flow.Syntax.parse{|(workspace selected_host
    (defnode sop/selected_host [(geo : geometry)]
      (if (> (array/count (sop/group_names geo "")) 0)
        (sop/with_attr geo :selected (array/vec4 (sop/count geo :points) [0.2 -0.0 0.6 0.8]) :type vec4
          :selection (sop/group geo :name "chosen") :default [1.0 1.0 1.0 1.0]) geo))
    (graph g :context sop (sop/selected_host (sop/ext_selected_allocation))))|}|>flow_ok in
  let node=L.workspace ~factories forms|>flow_ok|>compiled in
  List.iter(fun domains->let session=session() in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->
      Gc.full_major();let bytes=Gc.allocated_bytes() in
      let output=cook session ~domains ~time:0. node|>geometry in
      let bytes=Gc.allocated_bytes()-.bytes in
      assert(bytes<512.*.float count);
      let colors=Geometry.find_attribute ~owner:Point "selected" output|>Option.get|>Attribute.storage in
      match colors with Float4 colors->let colors=Packed.Float4.View.view colors in
        for i=0 to count-1 do assert(colors.x.(i)=(if i mod 2=0 then 0.2 else 1.)) done
      |_->assert false))[1;8];
  print_endline "Selected host dispatch: deferred masks hydrate before concrete writes without serializing a new dense node"

let () =
  let evaluate body =
    let forms=Flow.Syntax.parse ("(workspace checked_attribute (graph g :context sop [(geo : geometry nil)] "^
      "(let* [tested "^body^"] geo)))") |>flow_ok in
    let checked=match Flow.Workspace.check ~ops:Flow_sop.Operators.all Flow.Catalog.{version=1;kinds=[]} forms with
      |Some checked,[]->checked|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
    E.static ~record:true ~inputs:["g",["geo",E.Deferred(Flow.Ty.geometry,7)]]checked |>flow_ok
      |>fun evaluation->List.assoc["g";"tested"]evaluation.records |>List.hd |>snd in
  let bits value=Marshal.to_string value [Marshal.No_sharing] in
  let small=Line_geometry.points(Array.init 6(fun i->float i,0.,0.)) in
  let topology=Topology.polygons_owned ~point_count:6 ~vertex_points:[|0;1;2;3;4;5|]
    ~primitive_offsets:[|0;3;6|] |>get_string_ok in
  let small=Geometry.create ~positions:(Geometry.positions small) ~topology () |>get_string_ok in
  let fixtures=[small,[Attribute.Point,"point",6;Vertex,"vertex",6;Primitive,"primitive",2;Detail,"detail",1];
    Line_geometry.points[||],[Attribute.Point,"point",0];
    Line_geometry.points(Array.init 32769(fun i->float i,0.,0.)),[Attribute.Point,"point",32769]] in
  List.iter(fun(original,owners)->List.iter(fun(owner,owner_name,count)->
    List.iter(fun(kind,width)->List.iter(fun bad->
      let planes=Array.init width(fun c->Array.init count(fun i->
        if i mod 2=0 && c=width-1 then bad else if c=0 then -0. else Float.max_float)) in
      let storage=match kind with
        |"float"->Attribute.Float planes.(0)
        |"vec2"->Float2(Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1)|>get_string_ok)
        |"vec3"->Float3(Packed.Float3.View.of_owned_exn ~x:planes.(0) ~y:planes.(1) ~z:planes.(2))
        |_->Float4(Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) ~w:planes.(3)|>get_string_ok) in
      let attribute=Attribute.create_owned ~owner ~name:"checked" storage |>get_string_ok in
      let geo=Geometry.with_attribute attribute original |>get_string_ok in
      let before=geometry_bytes geo in
      let query=Printf.sprintf "(sop/attr_checked geo :checked :owner %s :type %s)" owner_name kind in
      let value=evaluate query in
      (match value with E.Record fields ->
        List.iter(fun(component,kind)->
          let node=Flow_sop.Attribute_kernel.node ~strict:true ~kind ~source:"checked-source-facts"
            ~name:"copy" ~values:(List.assoc component fields) ~sources:[7][Lisp_sop.snapshot geo] in
          assert((Sop.Node.facts node).reads=["checked"]))["values",kind;"valid","int"]
       |_->assert false);
      let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun id->if id=7 then Some geo else None) in
      let values=Array.init(count*width)(fun at->let i=at/width and c=at mod width in
        if i mod 2=0 then 0. else planes.(c).(i)) in
      let values=match width with 1->E.Float_array values|2->Vec2_array values|3->Vec3_array values|_->Vec4_array values in
      let expected=E.Record["values",values;"valid",E.Int_array(Array.init count(fun i->i mod 2))] in
      let program=Flow_ir.Executor.compile ~strict:true value |>flow_ok in
      let live=Input.at_time 0. in
      List.iter(fun domains->Parallel.run ~domains(fun()->
        let force reference=if reference then E.Host.force_reference ~resolve value ~live
          else Flow_ir.Executor.force ~strict:true ~resolve program ~live in
        List.iter(fun reference->
          let actual=force reference |>flow_ok in
          assert(bits actual=bits expected);
          (match actual with E.Record fields->
            (match List.assoc "values" fields with
             |E.Float_array a|Vec2_array a|Vec3_array a|Vec4_array a->Array.fill a 0(Array.length a)99.
             |_->assert false);
            (match List.assoc "valid" fields with E.Int_array a->Array.fill a 0(Array.length a)99|_->assert false)
           |_->assert false);
          assert(bits(force reference |>flow_ok)=bits expected);
          assert(geometry_bytes geo=before)) [false;true]))[1;8];
      if count>0 then begin
        let ordinary=evaluate(Printf.sprintf "(sop/attr geo :checked :owner %s :type %s)" owner_name kind) in
        let ordinary_program=Flow_ir.Executor.compile ~strict:true ordinary |>flow_ok in
        List.iter(function Error d->assert(d.Diagnostic.code="E_NONFINITE")|Ok _->assert false)
          [E.Host.force_reference ~resolve ordinary ~live;
           Flow_ir.Executor.force ~strict:true ~resolve ordinary_program ~live]
      end;
      if count=6 && bad=infinity then begin
        let input=Lisp_sop.snapshot geo in
        let body=Printf.sprintf {|(workspace checked_write
          (defnode sop/checked_write [(geo : geometry)]
            (let* [data %s cleaned (sop/with_attr geo :cleaned (exact (map (fn [x] x) data.values)) :owner %s :type %s)]
              (sop/with_attr cleaned :valid (exact (map (fn [(x : int)] x) data.valid)) :owner %s :type int)))
          (graph g :context sop (sop/checked_write (sop/ext_source))))|}query owner_name kind owner_name in
        let factory=Edit.factory ~key:"ext_source" ~label:"Source" ~category:["Test"] ~arity:0(fun _->Ok input) in
        List.iter(fun reference->
          let l=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse body |>flow_ok)|>flow_ok in
          let node=compiled l in
          let session=session() in
          Fun.protect ~finally:(fun()->Session.close session)(fun()->List.iter(fun domains->
            let output=cook session ~domains ~time:0. node |>geometry in
            let read component ty name=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some output)
              (E.Source("sop/attr",ty,["geometry",E.Deferred(Flow.Ty.geometry,7);"attribute",E.Text name;
                "owner",E.Text owner_name;"type",E.Text component]))|>flow_ok in
            assert(bits(read kind(Flow.Ty.Array(if width=1 then Float else if width=2 then Vec2 else if width=3 then Vec3 else Vec4))"cleaned")=bits values);
            assert(read "int" Flow.Ty.(Array Int)"valid"=E.Int_array(Array.init count(fun i->i mod 2)));
            Session.clear session)[1;8]))[false;true]
      end)
      [Int64.float_of_bits 0x7ff8000000001234L;infinity;neg_infinity])
      ["float",1;"vec2",2;"vec3",3;"vec4",4])owners)fixtures;
  let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun id->if id=7 then Some small else None) in
  let live=Input.at_time 0. in
  let missing=evaluate "(sop/attr_checked geo :missing)" in
  List.iter(function Error d->assert(d.Diagnostic.code="E_ATTR_TYPE")|Ok _->assert false)
    [E.Host.force_reference ~resolve missing ~live;
     Flow_ir.Executor.force ~strict:true ~resolve(Flow_ir.Executor.compile ~strict:true missing |>flow_ok) ~live];
  let lazy_query=evaluate "(if (> t 0) (sop/attr_checked geo :missing) (sop/attr_checked geo :P))" in
  let program=Flow_ir.Executor.compile ~strict:true lazy_query |>flow_ok in
  List.iter(fun reference->
    let force time=let live=Input.at_time time in
      if reference then E.Host.force_reference ~resolve lazy_query ~live
      else Flow_ir.Executor.force ~strict:true ~resolve program ~live in
    ignore(force (-1.) |>flow_ok);
    (match force 1. with Error d->assert(d.code="E_ATTR_TYPE")|Ok _->assert false);
    ignore(force (-1.) |>flow_ok))[false;true];
  (match E.Host.force_reference ~resolve:(Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None)) missing ~live with
   |Error d->assert(d.code="E_DATA_SOURCE")|Ok _->assert false);
  List.iter(fun kind->
    let source="(workspace bad_checked (graph g :context sop (let* [geo (sop/line) data (sop/attr_checked geo :P :type "^kind^")] geo)))" in
    let _,ds=Flow.Workspace.check ~ops:Flow_sop.Operators.all Flow.Catalog.{version=1;kinds=[]}(Flow.Syntax.parse source |>flow_ok) in
    assert(List.exists(fun(d:Diagnostic.t)->d.code="E_ATTR_TYPE")ds))
    ["int";"text";"float_array";"int_array";"(if true \"float\" \"vec3\")"];
  print_endline "Checked attributes: numeric/all-owner finite classification, raw bytes, strict/reference/domain copies and ordinary nonfinite guards pass"

let () =
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace checked_projection (graph g :context sop
    (let* [geo (sop/line) data (sop/attr_checked geo :P :owner point :type vec3)] geo)))|} |>flow_ok in
  let checked=match checker catalog forms with Some checked,[]->checked|_->assert false in
  let card workspace=Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog workspace "g")["g";"data"]|>Option.get in
  assert((card checked).ty=Flow.Ty.Record["values",Array Vec3;"valid",Array Int]);
  let edited,checked=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"data"];key=Kw "type";sub=[];value=Flow.Syntax.make(Str "float")})|>flow_ok in
  assert((card checked).ty=Flow.Ty.Record["values",Array Float;"valid",Array Int]);
  List.iter(fun(key,value)->ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog edited
    (Set_arg{node=["g";"data"];key;sub=[];value=Flow.Syntax.make(Str value)})|>flow_ok))
    [Pos 1,"weight";Kw "owner","vertex"];
  ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog edited
    (Connect{node=["g";"data"];key=Pos 0;src="geo";iter=false})|>flow_ok);
  assert(Result.is_error(Flow_graph.Flow_edit.apply_checked ~checker catalog edited
    (Set_arg{node=["g";"data"];key=Kw "type";sub=[];value=Flow.Syntax.make(Str "text")})));
  print_endline "Checked attributes: record card and all argument gestures retain typed projection"

let () =
  let geo=Line_geometry.points[|0.,0.,0.|] in
  let write data=Flow_sop.Attribute_kernel.write ~grain:97 ~owner:Attribute.Point
    ~kind:"float_array" "csr" (E.Record["offsets",E.Int_array[|0;Array.length data|];
      "values",E.Float_array data]) geo in
  let data=[|-0.;0.;Float.max_float|] in
  let written=write data |> flow_ok in
  let stored=Geometry.find_attribute ~owner:Attribute.Point "csr" written |>Option.get
    |>Attribute.View.storage in
  (match stored with Attribute.Float_array stored->
    let stored=Packed.Float_array.View.view stored in
    assert(stored.values!=data && stored.offsets=[|0;3|]);
    Array.iteri(fun i x->assert(Int64.bits_of_float x=Int64.bits_of_float data.(i)))stored.values
   |_->assert false);
  List.iter(fun(index,value)->let invalid=Array.copy data in invalid.(index)<-value;
    match write invalid with
    |Error d->assert(d.code="E_NONFINITE" && d.message="Attribute values must be finite.")
    |Ok _->assert false)[0,nan;2,infinity;2,neg_infinity];
  assert(Geometry.find_attribute ~owner:Attribute.Point "csr" geo=None);
  let empty=Flow_sop.Attribute_kernel.write ~grain:97 ~owner:Attribute.Point ~kind:"float_array"
    "csr" (E.Record["offsets",E.Int_array[|0|];"values",E.Float_array[||]])(Line_geometry.points[||]) in
  ignore(flow_ok empty);
  print_endline "CSR writer finite scan: empty/finite signed-zero owned rows and first/last nonfinite refusals pass"

let () =
  List.iter(fun count->
    let geo=Line_geometry.points(Array.init count(fun i->float i,-0.,1.)) in
    let before=geometry_bytes geo and input=Lisp_sop.snapshot geo in
    List.iter(fun kind->
      let row=if kind="float_array" then "(array/float 16 -0.0)" else "(array/range 16 :type int)" in
      let text=Printf.sprintf {|(workspace csr_owned
        (defnode sop/csr_owned [(geo : geometry)]
          (let* [n (sop/count geo :points)
                 offsets (exact (map (fn [i] (int/mul i 16)) (array/range (int/add n 1) :type int)))
                 data (exact (for [i (array/range n :type int) x %s] x))]
            (sop/with_attr geo :csr {:offsets offsets :values data} :type %s)))
        (graph g :context sop (sop/csr_owned (sop/ext_csr_input))))|} row kind in
      let make reference=
        let factory=Edit.factory ~key:"ext_csr_input" ~label:"CSR input" ~category:["Test"] ~arity:0(fun _->Ok input) in
        L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse text |>flow_ok)
        |>flow_ok |>compiled in
      let authored=make false and reference=make true in
      let offsets=Array.init(count+1)(fun i->i*16) in
      let storage=if kind="float_array" then
        Attribute.Float_array(Packed.Float_array.create_owned ~offsets ~values:(Array.make(count*16)(-0.)) |>get_string_ok)
      else Attribute.Int_array(Packed.Int_array.create_owned ~offsets ~values:(Array.init(count*16)(fun i->i mod 16)) |>get_string_ok) in
      let attribute=Attribute.create_owned ~name:"csr" ~owner:Point storage |>get_string_ok in
      let expected=Geometry.with_attribute attribute geo |>get_string_ok |>geometry_bytes in
      let session=session() in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        List.iter(fun domains->
          let actual=cook session ~domains ~time:0. authored |>geometry in
          assert(geometry_bytes actual=expected);
          assert(geometry_bytes(cook session ~domains ~time:0. reference |>geometry)=expected);
          (* Fresh computed buffers remain independent of the immutable source. *)
          assert(geometry_bytes geo=before);
          Session.clear session)[1;8]))["float_array";"int_array"])
    [0;1;32769];
  let source=Lisp_sop.snapshot(Line_geometry.points[|0.,0.,0.|]) in
  let invalid=Lisp_sop.workspace_node ~with_:["csr_input",source]
    {|(workspace csr_error (defnode sop/csr_error [(geo : geometry)]
       (sop/with_attr geo :csr
         {:offsets (exact (map (fn [i] i) (array/range 3 :type int)))
          :values (exact (map (fn [x] (exp x)) (array/float 1 1000.0)))} :type float_array))
       (graph g :context sop (sop/csr_error (sop/ext_csr_input))))|} in
  let session=session() in
  Fun.protect ~finally:(fun()->Session.close session)(fun()->
    match cook session ~domains:1 ~time:0. invalid with
    |Error d->assert(d.code="E_NONFINITE")|Ok _->assert false);
  print_endline "Fresh CSR Collect outputs: Float/Int empty/multi-block exact reference/domain bytes, immutable inputs and producer-before-count errors pass"

let () =
  List.iter(fun count->
    let original=Kernel.generate_point_ranges count(fun ~first ~last ~x ~y ~z->
      for i=first to last-1 do x.(i)<-float i;y.(i)<- -0.;z.(i)<-0. done) in
    let before=geometry_bytes original and input=Lisp_sop.snapshot original in
    List.iter(fun(kind,body)->
      let forms=Flow.Syntax.parse(Printf.sprintf
        "(workspace fills (graph g :context sop [(geo : geometry nil)] (let* [data %s mask (array/int (sop/count geo :points) (int (/ 1 (- t 2))))] geo)))" body) |>flow_ok in
      let checked=match Flow.Workspace.check ~ops:Flow_sop.Operators.all Flow.Catalog.{version=1;kinds=[]}forms with
        |Some checked,[]->checked|_,diagnostics->fail(String.concat "; "(List.map Diagnostic.to_string diagnostics)) in
      let evaluation=E.static ~record:true ~inputs:["g",["geo",E.Deferred(Flow.Ty.geometry,7)]]checked |>flow_ok in
      let values=List.assoc["g";"data"]evaluation.records |>List.hd |>snd in
      let mask=List.assoc["g";"mask"]evaluation.records |>List.hd |>snd in
      let default=match kind with "vec3"->E.Vec3(-0.,0.,0.)|"vec4"->E.Vec4(-0.,0.,0.,0.)|_->E.Float(-0.) in
      let node selected reference=Flow_sop.Attribute_kernel.node ~strict:true ~reference ~kind
        ?selection:(if selected then Some mask else None) ?default:(if selected then Some default else None)
        ~source:"constant-fill-proof" ~name:"fill" ~values ~sources:[7][input] in
      List.iter(fun selected->
      let direct=node selected false and reference=node selected true in
      List.iter(fun time->List.iter(fun domains->let session=session() in
        Fun.protect ~finally:(fun()->Session.close session)(fun()->
          let actual=cook session ~domains ~time direct and expected=cook session ~domains ~time reference in
          check(match actual,expected with
            |Ok a,Ok b->geometry_bytes(Result.get_ok(Sop.Payload.geometry a.payload))=
              geometry_bytes(Result.get_ok(Sop.Payload.geometry b.payload))
            |Error a,Error b->a.code=b.code && a.message=b.message|_->false)
            "Constant constructor direct/reference result or error differs")) [1;8]) [0.;2.;3.5])
        (if List.mem kind ["float";"vec3";"vec4"] then [false;true] else [false]);
      check(before=geometry_bytes original) "Constant constructor mutated input")
      (List.concat_map(fun(kind,body)->[kind,body;kind,"(let* [filled "^body^"] (exact filled))"])
      ["float","(array/float (sop/count geo :points) (sqrt (- 1 t)))";
       "float","(array/float (int/add (sop/count geo :points) 1) t)";
       "float","(array/float (int/sub (sop/count geo :points) 10) t)";
       "int","(array/int (sop/count geo :points) (int t))";
       "text","(array/text (sop/count geo :points) (if (> t 1) \"猫\" \"😀\"))";
       "vec3","(array/vec3 (sop/count geo :points) [t -0.0 2.5])";
       "vec4","(array/vec4 (sop/count geo :points) [t -0.0 2.5 -3.0])"]))
    [0;1;32769];
  print_endline "Constant constructors: strict scalar forces, owned outputs, lazy/empty/count/error and one/eight-domain reference parity"

let () =
  let positions=Packed.Float3.View.of_owned_exn ~x:[|0.;1.;0.;2.|]
    ~y:[|0.;0.;1.;2.|] ~z:[|0.;0.;0.;0.|] in
  let topology=Topology.polygons_owned ~point_count:4 ~vertex_points:[|0;1;2|]
    ~primitive_offsets:[|0;3|] |> get_string_ok in
  let original=Geometry.create ~positions ~topology () |> get_string_ok in
  let before=geometry_bytes original in
  let input=Lisp_sop.snapshot original in
  let owners=[Attribute.Point,"point",4;Vertex,"vertex",3;Primitive,"primitive",1;Detail,"detail",1] in
  let value kind count =
    let floats width=Array.init (count*width) (fun i->if i mod 3=0 then -0. else float i+.0.125) in
    match kind with
    | "float"->E.Float_array(floats 1)|"vec2"->E.Vec2_array(floats 2)
    | "vec3"->E.Vec3_array(floats 3)|"vec4"->E.Vec4_array(floats 4)
    | "int"->E.Int_array(Array.init count (fun i->if i mod 2=0 then max_int else min_int))
    | "text"->E.Text_array(Array.init count(fun i->[|"";"é日本🐈";"a\nb \"q\"";"literal :keyword"|].(i mod 4)))
    | "float_array"|"int_array" ->
        (* Ragged rows include empty rows and a 16-element matrix row. *)
        let offsets=Array.make (count+1) 0 in
        for i=0 to count-1 do offsets.(i+1)<-offsets.(i)+(if i=0 then 16 else if i mod 2=0 then 2 else 0) done;
        let n=offsets.(count) in
        E.Record["offsets",E.Int_array offsets;"values",(if kind="float_array" then E.Float_array(floats 1 |> fun data->Array.init n (fun i->data.(i mod count)))
          else E.Int_array(Array.init n(fun i->max_int-i)))]
    | _->assert false in
  List.iter(fun(owner,owner_name,count)->List.iter(fun kind->
    let values=value kind count in
    let first=ref None in
    List.iter(fun domains->
      let session=session() in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        let node values=Flow_sop.Attribute_kernel.node ~owner ~kind
          ~source:(owner_name^":"^kind) ~name:"typed" ~values ~sources:[0] [input] in
        let output=cook session ~domains ~time:0. (node values)|>geometry in
        let reread=if kind="float_array"||kind="int_array" then
          Printf.sprintf "(sop/attr geo :typed :owner %s :type %s)" owner_name kind else
          Printf.sprintf "(exact (map (fn [value] value) (sop/attr geo :typed :owner %s :type %s)))" owner_name kind in
        let authored=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot output]
          (Printf.sprintf "(workspace typed_roundtrip (defnode sop/typed_roundtrip [(geo : geometry)] (sop/with_attr geo :copy %s :owner %s :type %s)) (graph g :context sop (sop/typed_roundtrip (sop/ext_source))))"
            reread owner_name kind) in
        let copied=cook session ~domains ~time:0. authored|>geometry in
        let expected=Geometry.find_attribute ~owner "typed" output|>Option.get
          |>Attribute.with_name "copy"|>get_string_ok in
        assert(geometry_bytes copied=geometry_bytes(Geometry.with_attribute expected output|>get_string_ok));
        let args=["geometry",E.Deferred(Flow.Ty.geometry,0);"attribute",E.Text"typed";
          "owner",E.Text owner_name;"type",E.Text kind] in
        let resolve value=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some output) value |> flow_ok in
        let read=match kind with "float_array"|"int_array"->
          E.Record(List.map(fun component->component,resolve(E.Struct("sop/attr",Flow.Ty.Any,args@["component",E.Text component]))) ["offsets";"values"])
          |_->resolve(E.Struct("sop/attr",Flow.Ty.Any,args)) in
        assert(Marshal.to_string read [Marshal.No_sharing]=Marshal.to_string values [Marshal.No_sharing]);
        if kind="text" then begin
          let reference=Flow_sop.Attribute_kernel.node ~owner ~kind ~reference:true
            ~source:"text-reference" ~name:"copy"
            ~values:(E.Struct("sop/attr",Flow.Ty.Array Flow.Ty.Text,args))
            ~sources:[0] [Lisp_sop.snapshot output] in
          assert(geometry_bytes(cook session ~domains ~time:0. reference|>geometry)=geometry_bytes copied);
          let retained=geometry_bytes output in
          (match read with E.Text_array data->Array.fill data 0(Array.length data)"read-mutated"|_->assert false);
          assert(geometry_bytes output=retained);
          (match cook session ~domains ~time:0. (node(E.Float_array(Array.make count 0.))) with
            |Error d->assert(d.code="E_ATTR_TYPE")|Ok _->assert false)
        end;
        let wrong=if kind="text" then "float" else "text" in
        assert(resolve(E.Source("sop/has_attr",Flow.Ty.Bool,args))=E.Bool true);
        assert(resolve(E.Source("sop/has_attr",Flow.Ty.Bool,("type",E.Text wrong)::List.remove_assoc "type" args))=E.Bool false);
        assert(resolve(E.Source("sop/has_attr",Flow.Ty.Bool,("attribute",E.Text"missing")::List.remove_assoc "attribute" args))=E.Bool false);
        (match Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some output)
          (E.Struct("sop/attr",Flow.Ty.Any,("type",E.Text wrong)::List.remove_assoc "type" args)) with
          |Error d->assert(d.code="E_ATTR_TYPE")|Ok _->assert false);
        (match Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some output)
          (E.Struct("sop/attr",Flow.Ty.Any,("attribute",E.Text"missing")::List.remove_assoc "attribute" args)) with
          |Error d->assert(d.code="E_ATTR_TYPE")|Ok _->assert false);
        let bytes=geometry_bytes output in
        (match !first with None->first:=Some bytes|Some previous->assert(previous=bytes));
        assert(geometry_bytes original=before);
        let bad=match kind with "float_array"|"int_array"->
          (match values with E.Record fields->E.Record(("offsets",E.Int_array[|0|])::List.remove_assoc "offsets" fields)|_->assert false)
          |_->value kind (count+1) in
        (match cook session ~domains ~time:0. (node bad) with
          |Error d->assert(d.code="E_ATTR_COUNT")|Ok _->assert false);
        (match values with E.Record fields->
          let offsets=match List.assoc"offsets"fields with E.Int_array offsets->offsets|_->assert false in
          List.iter(fun(index,value)->let invalid=Array.copy offsets in invalid.(index)<-value;
            let values=E.Record(("offsets",E.Int_array invalid)::List.remove_assoc"offsets"fields) in
            match cook session ~domains ~time:0. (node values) with
            |Error d->assert(d.code="E_ATTR_COUNT")|Ok _->assert false)
            [0,1;count,(-1);count,max_int]
          |_->());
        let rec mutate=function
          |E.Int_array data->Array.fill data 0(Array.length data)0
          |E.Text_array data->Array.fill data 0(Array.length data)"mutated"
          |E.Float_array data|Vec2_array data|Vec3_array data|Vec4_array data->Array.fill data 0(Array.length data)99.
          |E.Record fields->List.iter(fun(_,v)->mutate v)fields|_->assert false in
        let fresh=value kind count in
        let owned=cook session ~domains ~time:0. (node fresh)|>geometry in
        let retained=geometry_bytes owned in
        mutate fresh;
        assert(geometry_bytes owned=retained && geometry_bytes original=before))) [1;8])
    ["float";"vec2";"vec3";"vec4";"int";"text";"float_array";"int_array"]) owners;
  print_endline "Typed attributes: all owners, tuple/Int/Text/CSR exact round trips, strict reads, queries and one/eight-domain bytes"

let () =
  let count=4097 in
  let positions=Packed.Float3.View.of_owned_exn ~x:(Array.make count 0.)
    ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
  let original=Geometry.create ~positions ~topology:(Topology.empty ~point_count:count)
    ~attributes:[Attribute.create_owned ~name:"name" ~owner:Attribute.Point
      (Text(Array.init count(fun i->if i mod 2=0 then "é日本" else "")))|>get_string_ok] ()|>get_string_ok in
  let text={|(workspace text_map
    (defnode sop/text_map [(geo : geometry)]
      (sop/with_attr geo :copy
        (map (fn [value] (str value " suffix")) (sop/attr geo :name :type text)) :type text))
    (graph g :context sop (sop/text_map (sop/ext_source))))|} in
  let expected=Geometry.with_attribute(Attribute.create_owned ~name:"copy" ~owner:Attribute.Point
    (Text(Array.init count(fun i->(if i mod 2=0 then "é日本" else "")^" suffix")))|>get_string_ok)original|>get_string_ok in
  List.iter(fun domains->let session=session() in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->
      let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot original] text in
      assert(geometry_bytes(cook session ~domains ~time:0. node|>geometry)=geometry_bytes expected))) [1;8];
  print_endline "Strict authored text attribute map preserves UTF-8 beyond 4096 elements at one/eight domains"

let () =
  let positions=Packed.Float3.View.of_owned_exn ~x:[||] ~y:[||] ~z:[||] in
  let empty=Geometry.create ~positions ~topology:(Topology.empty ~point_count:0) ()|>get_string_ok in
  let input=Lisp_sop.snapshot empty in
  let cases count=["float",E.Float_array(Array.make count (-0.));
    "vec2",E.Vec2_array(Array.make(count*2)(-0.));"vec3",E.Vec3_array(Array.make(count*3)(-0.));
    "vec4",E.Vec4_array(Array.make(count*4)(-0.));"int",E.Int_array(Array.make count max_int);
    "text",E.Text_array(Array.make count "é日本");
    "float_array",E.Record["offsets",E.Int_array(Array.make(count+1)0);"values",E.Float_array[||]];
    "int_array",E.Record["offsets",E.Int_array(Array.make(count+1)0);"values",E.Int_array[||]]] in
  List.iter(fun(owner,count)->List.iter(fun(kind,values)->
    let node=Flow_sop.Attribute_kernel.node ~owner ~kind ~source:"empty-owner" ~name:"empty"
      ~values ~sources:[0] [input] in
    List.iter(fun domains->let session=session() in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        let output=cook session ~domains ~time:0. node|>geometry in
        let attr=Geometry.find_attribute ~owner "empty" output|>Option.get in
        assert(Attribute.length attr=count && Geometry.point_count output=0);
        let value=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some output)
          (E.Source("sop/has_attr",Flow.Ty.Bool,["geometry",E.Deferred(Flow.Ty.geometry,0);"attribute",E.Text"P"]))|>flow_ok in
        assert(value=E.Bool true))) [1;8])(cases count))
    [Attribute.Point,0;Vertex,0;Primitive,0;Detail,1];
  print_endline "Empty geometry retains zero owner counts, one detail row and canonical P presence"

let () =
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let check forms=match checker catalog forms with
    |Some workspace,diagnostics when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics)->workspace
    |_,diagnostics->failwith(String.concat "\n"(List.map Diagnostic.to_string diagnostics)) in
  let forms=Flow.Syntax.parse {|(workspace typed_projection (graph g :context sop
    (let* [geo (sop/line) typed (sop/attr :owner point geo :P :type vec3)] geo)))|}|>flow_ok in
  let card workspace=Flow_graph.Projection.find (Flow_graph.Projection.of_graph catalog workspace "g") ["g";"typed"]|>Option.get in
  assert((card(check forms)).ty=Flow.Ty.Array Flow.Ty.Vec3);
  let edited,workspace=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"typed"];key=Kw "type";sub=[];value=Flow.Syntax.make(Str "float")})|>flow_ok in
  assert((card workspace).ty=Flow.Ty.Array Flow.Ty.Float);
  assert((card(check edited)).ty=Flow.Ty.Array Flow.Ty.Float);
  let edited,workspace=Flow_graph.Flow_edit.apply_checked ~checker catalog edited
    (Set_arg{node=["g";"typed"];key=Kw "type";sub=[];value=Flow.Syntax.make(Str "text")})|>flow_ok in
  assert((card workspace).ty=Flow.Ty.Array Flow.Ty.Text);
  assert((card(check edited)).ty=Flow.Ty.Array Flow.Ty.Text);
  let forms=Flow.Syntax.parse {|(workspace typed_range (graph g :context host
    (let* [typed (array/range 4 :type int)] typed)))|}|>flow_ok in
  assert((card(check forms)).ty=Flow.Ty.Array Flow.Ty.Int);
  let _,workspace=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"typed"];key=Kw "type";sub=[];value=Flow.Syntax.make(Str "float")})|>flow_ok in
  assert((card workspace).ty=Flow.Ty.Array Flow.Ty.Float);
  let forms=Flow.Syntax.parse {|(workspace dynamic_type (graph g :context sop
    (let* [geo (sop/line) typed (sop/attr geo :P :type (if true "vec3" "float"))] geo)))|}|>flow_ok in
  let _,diagnostics=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all catalog forms in
  assert(List.exists(fun(d:Diagnostic.t)->d.code="E_ATTR_TYPE")diagnostics);
  print_endline "Attribute/range type edits recheck graph projection; computed storage selectors are refused"

let () =
  let count=32769 in
  let positions=Packed.Float3.View.of_owned_exn ~x:(Array.make count 0.)
    ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
  let original=Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) ()|>get_string_ok in
  let input=Lisp_sop.snapshot original in
  List.iter(fun(kind,width)->
    let values=Array.init(count*width)(fun i->if i mod 13=0 then -0. else float i*.0.125) in
    let value=match width with 1->E.Float_array values|2->E.Vec2_array values|_->E.Vec4_array values in
    let first=ref None in
    List.iter(fun domains->let session=session() in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        let node=Flow_sop.Attribute_kernel.node ~strict:true ~kind ~source:"wide-multiblock"
          ~name:"wide" ~values:value ~sources:[0] [input] in
        let output=cook session ~domains ~time:0. node|>geometry in
        let read=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some output)
          (E.Struct("sop/attr",Flow.Ty.Any,["geometry",E.Deferred(Flow.Ty.geometry,0);
            "attribute",E.Text"wide";"type",E.Text kind]))|>flow_ok in
        assert(Marshal.to_string read [Marshal.No_sharing]=Marshal.to_string value [Marshal.No_sharing]);
        let bytes=geometry_bytes output in
        (match !first with None->first:=Some bytes|Some previous->assert(previous=bytes)))) [1;8])
    ["float",1;"vec2",2;"vec4",4];
  print_endline "Float/Vec2/Vec4 multi-block owner writes and reads preserve signed-zero bytes at one/eight domains"

let () =
  let bare=Line_geometry.points [|0.,0.,0.;1.,0.,0.|] in
  let tuple count f=Packed.Float3.View.of_owned_exn
    ~x:(Array.init count(fun i->let x,_,_=f i in x))
    ~y:(Array.init count(fun i->let _,y,_=f i in y))
    ~z:(Array.init count(fun i->let _,_,z=f i in z)) in
  let normal storage=Geometry.with_attribute
    (Attribute.create_owned ~owner:Point ~name:"N" storage|>get_string_ok) bare|>get_string_ok in
  List.iter(fun original->
    let node=Lisp_sop.workspace_node ~with_:["source",Lisp_sop.snapshot original]
      {|(workspace query_guard
       (defnode sop/query_guard [(geo : geometry)]
        (sop/with_attr geo :Cd (exact
          (if (sop/has_attr geo :N :type vec3)
            (map (fn [n] n) (sop/attr geo :N :type vec3))
            (array/vec3 2 [0.25 0.5 0.75])))))
       (graph g :context sop (sop/query_guard (sop/ext_source))))|} in
    assert((Sop.Node.facts node).reads=["N"]);
    let first=ref None in
    List.iter(fun domains->let session=session() in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        let output=cook session ~domains ~time:0. node|>geometry in
        let expected=match Geometry.find_attribute ~owner:Point "N" original with
          |Some attribute->(match Attribute.View.storage attribute with Float3 normals->normals
            |_->tuple 2(fun _->0.25,0.5,0.75))
          |None->tuple 2(fun _->0.25,0.5,0.75) in
        let actual=Geometry.find_attribute ~owner:Point "Cd" output|>Option.get
          |>float3_values|>Option.get in
        for i=0 to 1 do assert(Packed.Float3.get actual i=Packed.Float3.get expected i) done;
        let bytes=geometry_bytes output in
        (match !first with None->first:=Some bytes|Some previous->assert(previous=bytes)))) [1;8])
    [bare;normal(Attribute.Float[|1.;2.|]);
      normal(Attribute.Float3(tuple 2(fun i->float i,1.,2.)))];
  print_endline "Cooked has_attr Bool selects lazy typed reads; missing/wrong normals keep explicit fallback and truthful reads"

let () =
  let positions=Packed.Float3.View.of_owned_exn ~x:[|0.;1.;0.;2.|]
    ~y:[|0.;0.;1.;2.|] ~z:[|0.;0.;0.;0.|] in
  let topology=Topology.polygons_owned ~point_count:4 ~vertex_points:[|0;1;2|]
    ~primitive_offsets:[|0;3|]|>get_string_ok in
  let attribute owner name storage=Attribute.create_owned ~owner ~name storage|>get_string_ok in
  let csr=Packed.Float_array.create_owned ~offsets:[|0;16;16;18;18|]
    ~values:(Array.init 18 float)|>get_string_ok in
  let original=Geometry.create ~positions ~topology ~attributes:[
    attribute Point "scalar" (Float[|1.;2.;3.;4.|]);attribute Vertex "scalar" (Float[|1.;2.;3.|]);
    attribute Point "csr" (Float_array csr)] ()|>get_string_ok in
  let source=Lisp_sop.snapshot original in
  let owner_zip=Lisp_sop.node ~with_:["source",source]
    {|(let* [geo (sop/ext_source)] (sop/with_attr geo :out
      (exact (map (fn [p v] (+ p v))
        (sop/attr geo :scalar :type float :owner point)
        (sop/attr geo :scalar :type float :owner vertex))) :type float))|} in
  let csr_zip=Lisp_sop.node ~with_:["source",source]
    {|(let* [geo (sop/ext_source) csr (sop/attr geo :csr :type float_array)]
      (sop/with_attr geo :out (exact (map (fn [p offset] (+ p (float offset)))
        (sop/attr geo :scalar :type float) csr.offsets)) :type float))|} in
  List.iter(fun domains->let session=session() in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->
      (match cook session ~domains ~time:0. owner_zip with Error d->assert(d.code="E_ATTR_COUNT")|Ok _->assert false);
      let output=cook session ~domains ~time:0. csr_zip|>geometry in
      let values=Geometry.find_attribute ~owner:Point "out" output|>Option.get
        |>Attribute.get(Attribute.key ~name:"out" ~owner:Point Attribute.float)|>Option.get in
      assert(values=[|1.;18.;19.;22.|]))) [1;8];
  print_endline "Packed count origins distinguish point/vertex counts and CSR offsets from owner rows"

let () =
  let fixture detail =
    let geometry = Line_geometry.points [|0., 0., 0.; 1., 0., 0.|] in
    let attribute owner name values = Attribute.create_owned ~owner ~name (Attribute.Float values)
      |> get_string_ok in
    List.fold_left (fun geometry attribute -> Geometry.with_attribute attribute geometry |> get_string_ok)
      geometry [attribute Point "N" [|1.; 2.|]; attribute Detail "N" [|detail|];
        attribute Point "Cd" [|0.5; 0.75|]] in
  let first = fixture 1. in
  let changed = Geometry.with_attribute
    (Attribute.create_owned ~owner:Detail ~name:"N" (Attribute.Float [|2.|]) |> get_string_ok)
    first |> get_string_ok in
  let node = Lisp_sop.node ~with_:["source", Lisp_sop.snapshot first]
    {|(sop/delete_attr (sop/ext_source) "N" :owner "point")|} in
  assert ((Sop.Node.facts node).reads=[]);
  List.iter (fun domains ->
    let session = session () in
    Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
      ignore (cook session ~domains ~time:0. node |> geometry);
      let source = Lisp_sop.snapshot changed in
      ignore (cook session ~domains ~time:0. source |> geometry);
      let before = Session.stats session in
      let result = cook session ~domains ~time:0.
        (Sop.Node.Private.rebuild_with_inputs node [|source|]) |> geometry in
      assert ((Session.stats session).misses=before.misses && (Session.stats session).hits=before.hits+2);
      let expected = Attribute_ops.delete ~point_pattern:"N" changed |> get_ok in
      assert (geometry_bytes result=geometry_bytes expected);
      List.iter (fun pattern ->
        let node = Lisp_sop.node ~with_:["source", source]
          (Printf.sprintf "(sop/delete_attr (sop/ext_source) %S :owner \"point\")" pattern) in
        assert ((Sop.Node.facts node).reads=["*"] && (Sop.Node.facts node).writes=["*"]);
        let result = cook session ~domains ~time:0. node |> geometry in
        let expected = Attribute_ops.delete ~point_pattern:pattern changed |> get_ok in
        assert (geometry_bytes result=geometry_bytes expected)) ["N*"; "N Cd"; "^Cd"])) [1;8];
  print_endline "Attribute deletion: selective refresh carries same-name owners; patterns stay conservative at one/eight domains"

let () =
  List.iter (fun count ->
    let positions = Packed.Float3.View.of_owned_exn
      ~x:(Array.make count 0.) ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
    let original = Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) ()
      |> get_string_ok in
    let before = geometry_bytes original in
    let input = Sop.Node.Private.make_geometry ~operation:"test.xyz_write" ~version:1
      ~parameters:"" ~cook_mode:Sop.Node.Generator
      ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ -> Ok Sop.Node.Private.{geometry=original;instances=None}) in
    let values = Array.init (count * 3) (fun j ->
      if j mod 17 = 0 then -0. else match j mod 3 with
      | 0 -> float_of_int (j / 3) +. 0.125
      | 1 -> -.float_of_int (j / 3) -. 0.25
      | _ -> float_of_int (j / 3) *. 0.5 +. 0.375) in
    let expected = Marshal.to_string values [Marshal.No_sharing] in
    let first = ref None in
    List.iter (fun domains ->
      let session = session () in
      Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
        let source = Array.copy values in
        let node values = Flow_sop.Attribute_kernel.node ~source:"xyz-write"
          ~name:"Cd" ~values:(E.Vec3_array values) ~sources:[0] [input] in
        let output = cook session ~domains ~time:0. (node source) |> geometry in
        let packed = Geometry.find_attribute ~owner:Attribute.Point "Cd" output |> Option.get
          |> float3_values
          |> Option.get |> Packed.Float3.View.view in
        for i = 0 to count - 1 do
          assert (Int64.bits_of_float packed.x.(i) = Int64.bits_of_float values.(3*i));
          assert (Int64.bits_of_float packed.y.(i) = Int64.bits_of_float values.(3*i+1));
          assert (Int64.bits_of_float packed.z.(i) = Int64.bits_of_float values.(3*i+2))
        done;
        let bytes = geometry_bytes output in
        (match !first with None -> first:=Some bytes | Some first -> assert (first=bytes));
        assert (Marshal.to_string source [Marshal.No_sharing]=expected);
        Array.fill source 0 (Array.length source) 99.;
        assert (geometry_bytes output=bytes && geometry_bytes original=before);
        let failure values code = match cook session ~domains ~time:0. (node values) with
          | Error d -> assert (d.code=code && geometry_bytes original=before)
          | Ok _ -> assert false in
        failure (Array.make (count*3+3) 0.) "E_ATTR_COUNT";
        if count>0 then begin
          let bad = Array.copy values in bad.(Array.length bad-1)<-Float.infinity;
          failure bad "E_NONFINITE"
        end)) [1;8]) [0;1;32769];
  print_endline "XYZ attribute writes: empty/multi-block, signed-zero/domain bytes, ownership and error immutability pass"

let () =
  List.iter (fun count ->
    let plane component=Array.init count (fun i ->
      if (i+component) mod 7=0 then -0. else float_of_int (i+component)*.0.125) in
    let positions=Packed.Float3.View.of_owned_exn ~x:(plane 0) ~y:(plane 1) ~z:(plane 2) in
    let original=Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) () |> get_string_ok in
    let before=geometry_bytes original in
    let input=Sop.Node.Private.make_geometry ~operation:"test.borrowed_planes" ~version:1
      ~parameters:"" ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ -> Ok Sop.Node.Private.{geometry=original;instances=None}) in
    let make name body=
      let lowered=lower (source (Printf.sprintf "(sop/with_attr g :%s (exact %s))" name body)) in
      let call=Array.find_opt (fun (n:E.node)->n.kind="sop/with_attr") lowered.plan.nodes |> Option.get in
      let id=match List.assoc "geometry" call.args with E.Deferred (_,id)->id|_->assert false in
      Flow_sop.Attribute_kernel.node ~strict:true ~source:body ~name
        ~values:(List.assoc "values" call.args) ~sources:[id] [input] in
    let session=session () in
    Fun.protect ~finally:(fun ()->Session.close session) (fun ()->
      List.iter (fun domains ->
        let output=cook session ~domains ~time:0. (make "P" "(map (fn [p] p) (sop/attr g :P))") |> geometry in
        assert (geometry_bytes output=before);
        let old=Packed.Float3.View.view positions and fresh=Packed.Float3.View.view (Geometry.positions output) in
        assert (old.x==fresh.x && old.y==fresh.y && old.z==fresh.z);
        let painted=cook session ~domains ~time:0. (make "Cd" "(map (fn [p] p) (sop/attr g :P))") |> geometry in
        let copied=Geometry.find_attribute ~owner:Attribute.Point "Cd" painted |> Option.get
          |> float3_values |> Option.get
          |> Packed.Float3.View.view in
        assert (old.x!=copied.x && old.y!=copied.y && old.z!=copied.z);
        assert (Marshal.to_string [|old.x;old.y;old.z|] [Marshal.No_sharing]
          =Marshal.to_string [|copied.x;copied.y;copied.z|] [Marshal.No_sharing]);
        if count>1 then begin
          let short=make "P" "(map (fn [p q] p) (sop/attr g :P) (array/vec3 1))" in
          match cook session ~domains ~time:0. short with
          | Error d -> assert (d.code="E_ATTR_COUNT") | Ok _ -> assert false
        end;
        assert (geometry_bytes original=before)) [1;8])) [0;1;32769];
  print_endline "Borrowed exact planes: omitted-lane counts, shared P, owned attributes and signed-zero/domain bytes pass"

let () =
  let lowered=lower {|(workspace captures
    (graph mesh :context sop [(offset : float 0.015625)]
      (let* [geo (sop/box :consolidate_points true :center [(+ offset (* t 0.015625)) 0 0])
             p (reduce + [0 0 0] (sop/attr geo :P))
             picture (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)]
        (sop/attr_from_image geo picture :attribute "Cd")))
    (graph other :context sop (ref mesh :offset 0.03125)))|} in
  let graph=List.find(fun(g:L.graph)->g.name="mesh" && not g.default)lowered.graphs in
  let producer=Array.find_opt(fun(n:E.node)->n.kind="image/map" && n.inst=graph.instance)
      lowered.plan.nodes |> Option.get in
  let fn=match List.assoc "function" producer.args with E.Fn fn->fn|_->assert false in
  let sources=Flow_sop.Attribute_kernel.sources(E.Fn fn)in
  assert(List.length sources=1);
  let source=List.hd sources in
  let context=L.source_context lowered ~node:source |> flow_ok in
  assert(context.network==graph.network);
  let network,root=L.source_cone context ~node:source |> flow_ok in
  assert(List.length(Edit.inspect network.geometry)=1);
  assert(not(Flow_sop.Port.Map.is_empty network.drives));
  assert(Flow_sop.Network.Int_map.is_empty network.frame_nodes);
  assert(not(Flow_sop.Network.Int_map.is_empty context.network.frame_nodes));
  List.iter(fun context->match L.source_cone context ~node:source with
    |Error d->assert(d.code="E_DATA_SOURCE")|Ok _->assert false)
    [L.{context with compiled=Flow_sop.Network.Int_map.empty};
     L.{context with compiled=Flow_sop.Network.Int_map.singleton source (Sop.Node.Private.fresh_id())}];
  List.iter(fun id->match L.source_context lowered ~node:id with
    |Error d->assert(d.code="E_DATA_SOURCE")|Ok _->assert false)[-1;Array.length lowered.plan.nodes;producer.id];
  let resolve time=
    let live=Input.at_time time in
    let lane=Flow_sop.Value_lane.create()in
    (* A whole-network resolution would call the deliberately absent image provider. *)
    let resolved=Flow_sop.Value_lane.resolve lane ~live ~time network |> flow_ok in
    Edit.compile_node resolved.geometry ~node_id:root |> get_string_ok in
  let input=resolve 0. in
  let kernel=Flow_sop.Image_kernel.prepare ~identity:1 ~width:7 ~height:3 ~fn ~sources [input] |> flow_ok in
  let bytes domains kernel=
    let session=session()in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->
      let image=cook session ~domains ~time:0. (Flow_sop.Image_kernel.node kernel)
        |> Result.get_ok |> fun output->Sop.Payload.image output.payload |> Result.get_ok in
      Sop.Image.rgba8 image |> Option.get)in
  let initial=bytes 1 kernel in
  assert(initial=bytes 8 kernel);
  if Char.code(Bytes.get initial 0)<>82 then
    failwith(Printf.sprintf "override first red: expected 82, got %d" (Char.code(Bytes.get initial 0)));
  let current=resolve 1. in
  assert(current!=input);
  assert(Flow_sop.Attribute_kernel.source_origins ~sources [current]
    =Flow_sop.Attribute_kernel.source_origins ~sources [input]);
  let rebound=Flow_sop.Image_kernel.with_inputs kernel [current] |> flow_ok in
  assert(Flow_sop.Image_kernel.program rebound==Flow_sop.Image_kernel.program kernel);
  let independently_prepared=Flow_sop.Image_kernel.prepare ~identity:2 ~width:7 ~height:3
    ~fn ~sources [current] |> flow_ok in
  let changed=bytes 1 rebound in
  assert(changed<>initial && changed=bytes 8 rebound && changed=bytes 1 independently_prepared);
  List.iter(fun inputs->match Flow_sop.Image_kernel.with_inputs kernel inputs with
    |Error d->assert(d.code="E_DATA_SOURCE")|Ok _->assert false)
    [[];[Lisp_sop.node {|(sop/box)|}]];
  let session=session()in
  Fun.protect ~finally:(fun()->Session.close session)(fun()->
    let original=cook session ~domains:1 ~time:0. input |> geometry in
    let transforms=[|Rays_math.Mat4.translation(Rays_math.Vec3.create 1. 0. 0.);
      Rays_math.Mat4.translation(Rays_math.Vec3.create 3. 0. 0.)|]in
    let packed=Sop.Node.Private.make_geometry ~operation:"test.capture.instances" ~version:1
      ~parameters:"" ~cook_mode:Sop.Node.Generator
      ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _->Ok Sop.Node.Private.{geometry=original;instances=Some transforms})in
    let consumer=Flow_sop.Attribute_kernel.materialized_source packed in
    List.iter(fun domains->
      let output=cook session ~domains ~time:0. consumer |> Result.get_ok in
      assert(output.instances=None);
      let materialized=Sop.Payload.geometry output.payload |> Result.get_ok in
      let expected=Rdk.Instance_copy.materialize_instances ~transforms original |> get_ok in
      assert(Geometry.point_count materialized=2*Geometry.point_count original);
      assert(geometry_bytes materialized=geometry_bytes expected);
      let attribute=E.Struct("sop/attr",Flow.Ty.Array Flow.Ty.Vec3,
        ["geometry",E.Deferred(Flow.Ty.geometry,source);"attribute",E.Text "P"])in
      let positions=Flow_sop.Attribute_kernel.resolve ~geometry:(fun id->
        if id=source then Some materialized else None)attribute |> flow_ok in
      match positions with E.Vec3_array xs->assert(Array.length xs=6*Geometry.point_count original)|_->assert false)
      [1;8]);
  print_endline "image capture foundation: live override cone, no downstream image callback, shared program proof and materialized instance P pass"

let () =
  let text = "(workspace field (graph g :context sop
    (sop/iso_surface :field (fn [p] (- (length p) (+ 1 t)))
      :resolution [24 24 24] :min [-2.1 -2 -2] :max [2 2 2] :iso 0)))" in
  let lowered = lower text in
  let call = Array.find_opt (fun (n : E.node) -> n.kind = "sop/iso_surface")
      lowered.plan.nodes |> Option.get in
  let fn = match List.assoc "field" call.args with E.Fn fn -> fn | _ -> assert false in
  (* Independent index decomposition pins all 65^3 positions and x-fast order. *)
  let positions = Array.init (25 * 25 * 25 * 3) (fun j ->
    let i = j / 3 in
    let index = match j mod 3 with 0 -> i mod 25 | 1 -> (i / 25) mod 25 | _ -> i / (25*25) in
    (* x is a non-dyadic lattice: the probe must use the same fused multiply-add as the cook. *)
    if j mod 3 = 0 then Float.fma (float_of_int index) (4.1 /. 24.) (-2.1)
    else Float.fma (float_of_int index) (4. /. 24.) (-2.)) in
  let values = E.Host.map_function ~signature:Flow.Ty.{params = [Vec3]; result = Float}
      fn [E.Vec3_array positions] |> flow_ok in
  let program = Flow_sop.Attribute_kernel.prepare ~sources:[] [] values |> flow_ok in
  let packed = Option.get (Flow_ir.Executor.packed_region program) in
  assert (Flow_ir.Packed.static_count packed = Some (25 * 25 * 25));
  (* Separately rounded squares, as the Lisp primitives: arm64 ocamlopt would otherwise fuse a +. b *. c. *)
  let sq v = Sys.opaque_identity (v *. v) in
  List.iter (fun time ->
    let live = Input.at_time time in
    let expected = Array.init (25 * 25 * 25) (fun i ->
      let x = positions.(3*i) and y = positions.(3*i+1) and z = positions.(3*i+2) in
      sqrt ((sq x +. sq y) +. sq z) -. (1. +. time)) in
    let reference = Flow_ir.Executor.force ~reference:true program ~live |> flow_ok in
    assert (reference = E.Float_array expected);
    let dense = let cook_session = session () in
      Fun.protect ~finally:(fun () -> Session.close cook_session) (fun () ->
        geometry_bytes (cook cook_session ~domains:1 ~time (compiled lowered) |> geometry)) in
    List.iter (fun domains ->
      let actual = Parallel.run ~domains (fun () ->
        Flow_ir.Packed.force packed ~live |> flow_ok) in
      assert (actual = reference);
      let cook_session = session () in
      Fun.protect ~finally:(fun () -> Session.close cook_session) (fun () ->
        assert (geometry_bytes (cook cook_session ~domains ~time (compiled lowered) |> geometry) = dense));
      let reference_lowered = L.workspace ~factories ~reference:true
          (Flow.Syntax.parse text |> flow_ok) |> flow_ok in
      let cook_session = session () in
      Fun.protect ~finally:(fun () -> Session.close cook_session) (fun () ->
        assert (geometry_bytes (cook cook_session ~domains ~time (compiled reference_lowered) |> geometry) = dense)))
      [1;8]) [0.;0.25];
  let forms = Flow.Syntax.parse text |> flow_ok in
  let checked, diagnostics = Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms in
  assert (diagnostics = []);
  let pane = Flow_graph.Projection.of_graph catalog (Option.get checked) "g" in
  let surface = List.find (fun (n : Flow_graph.Projection.node) -> n.head = "sop/iso_surface") pane.nodes in
  assert (List.exists (fun (row : Flow_graph.Projection.row) -> row.label = "field") surface.rows);
  let zone = List.find (fun (n : Flow_graph.Projection.node) ->
    Option.fold ~none:false ~some:(fun (z : Flow_graph.Projection.zone) -> z.kind = Fn) n.zone) pane.nodes in
  assert (List.exists (fun (r : Flow_graph.Projection.rail_row) -> r.name = "p" && r.ty = Some Flow.Ty.Vec3)
    (Option.get zone.zone).rail);
  let calls = L.field_calls ~live:(Input.at_time 0.25)
      ~resolve:(fun v -> Ok v) lowered in
  let forced_calls = ref 0 in
  let bulk_calls path outer = List.map (fun (offset, count, at) ->
    offset, count, (fun k -> incr forced_calls; at k)) (calls path outer) in
  let probe = Flow_graph.Probe.make ~live:(Input.at_time 0.25) ~bulk_calls lowered.evaluated in
  let counts = Flow_graph.Probe.counts probe pane ~probe:(fun _ -> 0) in
  let count = match List.assoc_opt zone.path counts with
    | Some count -> count
    | None -> let scope, iter = E.Host.function_scope fn |> Option.get in
        failwith (Printf.sprintf "field probe: projection %s, function %s/%s, counts %s"
          (String.concat "/" zone.path) (String.concat "/" scope)
          (String.concat "," (List.map string_of_int iter))
          (String.concat ";" (List.map (fun (p,n) -> String.concat "/" p ^ ":" ^ string_of_int n) counts))) in
  assert (count = 25*25*25);
  let k = 7000 in
  let x = positions.(3*k) and y = positions.(3*k+1) and z = positions.(3*k+2) in
  let sample = sqrt ((sq x +. sq y) +. sq z) -. 1.25 in
  assert (Flow_graph.Probe.at probe (zone.path @ [":p"]) ~probes:[k] = Some (Value (E.Vec3 (x,y,z))));
  assert (Flow_graph.Probe.at probe (zone.path @ ["@result"]) ~probes:[k] = Some (Value (E.Float sample)));
  assert (!forced_calls = 1);
  let body = Flow_graph.Projection.find pane (zone.path @ ["@result"]) |> Option.get in
  ignore (Flow_graph.Probe.footer probe body ~probes:[k]);
  assert (!forced_calls <= 65);
  let forced = !forced_calls in
  ignore (Flow_graph.Probe.footer probe body ~probes:[k]);
  assert (!forced_calls = forced);
  ignore (Flow_graph.Flow_edit.apply_checked ~ops:Flow_sop.Operators.all catalog forms
    (Set_arg {node = surface.path; key = Kw "resolution"; sub = [];
      value = Flow.Syntax.parse "[8 9 7]" |> flow_ok |> List.hd}) |> flow_ok);
  print_endline "field SOP: complete grid kernel/reference/domain/mesh parity and graph gestures pass"

let () =
  (* Non-dyadic, off-centre samples expose accidental multiply/add contraction. *)
  let positions = Array.init (130 * 257 * 3 * 3) (fun j ->
    let i = j / 3 in
    match j mod 3 with
    | 0 -> Float.fma (float_of_int (i mod 130)) (5. /. 129.) (-2.)
    | 1 -> Float.fma (float_of_int ((i / 130) mod 257)) (3.5 /. 256.) (-1.5)
    | _ -> Float.fma (float_of_int (i / (130 * 257))) (2.7 /. 2.) (-1.)) in
  let prepare expression =
    let lowered = lower ("(workspace rounding (graph g :context sop
      (sop/iso_surface :field (fn [p] " ^ expression ^ ") :resolution [129 256 2]
        :min [-2 -1.5 -1] :max [3 2 1.7])))") in
    let call = Array.find_opt (fun (n : E.node) -> n.kind = "sop/iso_surface")
      lowered.plan.nodes |> Option.get in
    let fn = match List.assoc "field" call.args with E.Fn fn -> fn | _ -> assert false in
    let values = E.Host.map_function ~signature:Flow.Ty.{params = [Vec3]; result = Float}
      fn [E.Vec3_array positions] |> flow_ok in
    Flow_sop.Attribute_kernel.prepare ~sources:[] [] values |> flow_ok in
  let derived = prepare "(- (length (- p [0.2 -0.3 0.1])) (+ 0.8 t))"
  and primitives = prepare "(let* [q (- p [0.2 -0.3 0.1])]
    (- (sqrt (+ (+ (* q.x q.x) (* q.y q.y)) (* q.z q.z))) (+ 0.8 t)))" in
  let same_bits actual expected = match actual, expected with
    | E.Float_array actual, E.Float_array expected ->
        assert (Array.length actual = Array.length expected);
        Array.iteri (fun i value ->
          if Int64.bits_of_float value <> Int64.bits_of_float expected.(i) then
            failwith (Printf.sprintf "field length rounding differs at sample %d: %.17g / %.17g"
              i value expected.(i))) actual
    | _ -> assert false in
  List.iter (fun time ->
    let live = Input.at_time time in
    let expected = Flow_ir.Executor.force ~reference:true primitives ~live |> flow_ok in
    same_bits (Flow_ir.Executor.force ~reference:true derived ~live |> flow_ok) expected;
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      List.iter (fun program ->
        same_bits (Flow_ir.Executor.force program ~live |> flow_ok) expected)
        [derived; primitives])) [1;8]) [0.;0.25];
  print_endline "field length: asymmetric lattice matches primitive/scalar/packed float64 bits"

let () =
  let text = "(workspace probe (graph g :context sop
    (let* [field (fn [p] p.x)
           preview (map field (array/vec3 2 [1 0 0]))
           a (sop/iso_surface :field field :resolution [1 1 1])
           b (sop/iso_surface :field field :resolution [2 1 1])]
      (sop/merge a b))))" in
  let lowered = lower text in
  let forms = Flow.Syntax.parse text |> flow_ok in
  let workspace, ds = Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms in
  assert (ds = []);
  let pane = Flow_graph.Projection.of_graph catalog (Option.get workspace) "g" in
  let calls = L.field_calls ~live:(Input.at_time 0.) ~resolve:(fun v -> Ok v) lowered in
  let selectors = calls ["g";"field"] [] in
  assert (List.map (fun (offset,count,_) -> offset,count) selectors = [0,8; 8,12]);
  let probe = Flow_graph.Probe.make ~bulk_calls:calls lowered.evaluated in
  assert (List.assoc ["g";"field"] (Flow_graph.Probe.counts probe pane ~probe:(fun _ -> 0)) = 22);
  List.iter (fun (k,expected) ->
    assert (Flow_graph.Probe.at probe ["g";"field";"@result"] ~probes:[k]
      = Some (Value (E.Float expected)))) [0,1.; 1,1.; 2,(-2.); 9,2.; 10,(-2.); 21,2.];
  assert (Flow_graph.Probe.at probe ["g";"field";"@result"] ~probes:[22] = None)

let () =
  let lowered = lower "(workspace probe_state (graph g :context sop
    (let* [total (state [s 0.0] (+ s 1.0))
           field (fn [p] (+ p.x total))]
      (sop/iso_surface :field field :resolution [1 1 1]))))" in
  let state = E.create_state () in
  let before = E.state_stamp state in
  let calls = L.field_calls ~state ~live:(Input.at_time 1.) ~resolve:(fun v -> Ok v) lowered in
  let probe = Flow_graph.Probe.make ~state ~live:(Input.at_time 1.) ~bulk_calls:calls lowered.evaluated in
  assert (Flow_graph.Probe.at probe ["g";"field";"@result"] ~probes:[0] = Some (Value (E.Float (-1.))));
  assert (E.state_stamp state = before)

let () =
  (* Real Lisp lowering, not a hand-built Readback IR: GPU callbacks run before
     worker cooking, and workers receive only an owned exact CPU snapshot. *)
  let count=2048 and calls=ref 0 and reads=ref 0 and time=ref 0. in
  let backend:Flow_ir.Gpu.backend={cost=(fun ~fast:_ _ ~count:_->Some 0.);
    prepare=(fun ~fast:_ _->assert(Domain.is_main_domain());Ok {
      run=(fun inputs->assert(Domain.is_main_domain());assert(inputs.count=count);
        incr calls;Ok {identity=1;count;width=3;output=0;stamp=Int64.of_int !calls;gpu_seconds=None});
      readback=(fun _->assert(Domain.is_main_domain());incr reads;
        Ok(E.Vec3_array(Array.init(count*3)(fun j->if j mod 3=0 then float(j/3)+. !time else 0.))))})} in
  let body wrap="(sop/with_attr g :P ("^wrap^
    "(map (fn [i] [(+ i t) 0 0]) (array/range 2048))))" in
  let gpu={Flow_ir.Gpu.default with backend=Some backend} in
  let exact=lower ~gpu (source ~columns:1024 (body "exact ")) in
  let network=(List.hd exact.graphs).network in
  let lane=Flow_sop.Value_lane.create() in
  List.iter(fun t->time:=t;
    let resolved=Flow_sop.Value_lane.resolve ~gpu lane ~time:t network |> flow_ok in
    let node=Edit.compile_node resolved.geometry ~node_id:(Option.get(List.hd exact.graphs).root)
      |>get_string_ok in
    List.iter(fun domains->let session=session()in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        let positions=cook session ~domains ~time:t node |> geometry |>Geometry.positions in
        for i=0 to count-1 do assert(Packed.Float3.get positions i=(float i+.t,0.,0.))done)) [1;8]) [0.;0.5];
  assert(!calls=2 && !reads=2);
  let unmeasured={backend with cost=(fun ~fast:_ _ ~count:_->None)}in
  let unmeasured_gpu={Flow_ir.Gpu.default with backend=Some unmeasured}in
  let unmeasured_network=(List.hd(lower ~gpu:unmeasured_gpu
    (source ~columns:1024(body "exact "))).graphs).network in
  ignore(Flow_sop.Value_lane.resolve ~gpu:unmeasured_gpu
    (Flow_sop.Value_lane.create()) ~time:0.75 unmeasured_network|>flow_ok);
  assert(!calls=2);
  time:=0.75;
  let qualification_gpu={unmeasured_gpu with policy=Flow_ir.Gpu.Qualification}in
  let qualification_network=(List.hd(lower ~gpu:qualification_gpu
    (source ~columns:1024(body "exact "))).graphs).network in
  ignore(Flow_sop.Value_lane.resolve ~gpu:qualification_gpu
    (Flow_sop.Value_lane.create()) ~time:0.75 qualification_network|>flow_ok);
  assert(!calls=3 && !reads=3);
  let baseline= !calls in
  let cpu=Flow_sop.Value_lane.create()in
  let cpu_network=(List.hd(lower(source ~columns:1024(body "exact "))).graphs).network in
  ignore(Flow_sop.Value_lane.resolve cpu ~time:1. cpu_network |>flow_ok);
  let reference=L.workspace ~factories ~reference:true
    (Flow.Syntax.parse(source ~columns:1024(body "exact "))|>flow_ok)|>flow_ok in
  ignore(Flow_sop.Value_lane.resolve ~gpu
    (Flow_sop.Value_lane.create()) ~time:1. (List.hd reference.graphs).network|>flow_ok);
  assert(!calls=baseline);
  (* G.1: the SOP input sink is an implicit readback, so the bare map needs no (exact x). *)
  let implicit=lower ~gpu (source ~columns:1024
    "(sop/with_attr g :P (map (fn [i] [(+ i t) 0 0]) (array/range 2048)))")in
  let before= !calls in
  time:=1.;
  let resolved=Flow_sop.Value_lane.resolve ~gpu
    (Flow_sop.Value_lane.create()) ~time:1. (List.hd implicit.graphs).network |> flow_ok in
  assert(!calls=before+1 && !reads=before+1);
  let node=Edit.compile_node resolved.geometry ~node_id:(Option.get(List.hd implicit.graphs).root) |>get_string_ok in
  let session=session()in
  Fun.protect ~finally:(fun()->Session.close session)(fun()->
    let positions=cook session ~domains:8 ~time:1. node |> geometry |>Geometry.positions in
    for i=0 to count-1 do assert(Packed.Float3.get positions i=(float i+.1.,0.,0.))done);
  (* A catalog parameter still requires (exact x). *)
  (match Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog
      (Flow.Syntax.parse(source ~columns:1024
        "(sop/box :size (array/sum (map (fn [i] (+ i t)) (array/range 2048))))")|> flow_ok) with
    |None, ds->assert(List.exists(fun(d:Diagnostic.t)->d.code="E_APPROX_SINK")ds)
    |_->assert false)
let () =
  assert (Result.is_error (Flow_sop.Attribute_kernel.prepare ~sources:[0] [] E.No_geo));
  List.iter (fun (other, expected_stages) ->
    let body = "(let* [other " ^ other ^
      " a (map (fn [p] (+ p [t 0 0])) (sop/attr g :P))" ^
      " b (map (fn [p] (* p (+ t 1))) (sop/attr other :P))]" ^
      " (sop/with_attr g :P (exact (map (fn [left right] (+ left right)) a b))))" in
    let l = lower (source ~columns:1025 body) in
    let node = compiled l in
    let call = Array.find_opt (fun (n : E.node) -> n.kind = "sop/with_attr") l.plan.nodes |> Option.get in
    let values = List.assoc "values" call.args in
    let main = match List.assoc "geometry" call.args with E.Deferred (_, id) -> id | _ -> assert false in
    let sources = main :: List.filter ((<>) main) (Flow_sop.Attribute_kernel.sources values) in
    let inputs = Sop.Node.inputs node in
    let prepare inputs = Flow_sop.Attribute_kernel.prepare ~sources inputs values |> flow_ok in
    assert (stages (Flow_ir.Executor.compile values |> flow_ok) = 1);
    let program = prepare inputs in
    assert (stages program = expected_stages);
    (* Cooked Core calls use Source descriptors. Their captured count proofs,
       selective reads and borrowed columns must match legacy Struct reads. *)
    let forms=Flow.Syntax.parse {|(workspace descriptors
      (graph g :context value [(pa : (array vec3) (array/vec3 0)) (pb : (array vec3) (array/vec3 0))]
        (let* [a (map (fn [p] (+ p [t 0 0])) pa)
               b (map (fn [p] (* p (+ t 1))) pb)
               tested (exact (map (fn [left right] (+ left right)) a b))] 0.0)))|} |>flow_ok in
    let checked=match Flow.Workspace.check catalog forms with
      |Some checked,[]->checked|_,diagnostics->failwith(String.concat "; " (List.map Diagnostic.to_string diagnostics)) in
    let other=Option.value ~default:main (List.find_opt ((<>) main) sources) in
    let marker id=E.Source("sop/attr",Flow.Ty.Array Flow.Ty.Vec3,
      ["geometry",E.Deferred(Flow.Ty.geometry,id);"attribute",E.Text "P"]) in
    let evaluation=E.static ~record:true ~inputs:["g",["pa",marker main;"pb",marker other]] checked |>flow_ok in
    let modern=List.assoc ["g";"tested"] evaluation.records |>List.hd |>snd in
    let modern_program = Flow_sop.Attribute_kernel.prepare ~sources inputs modern |> flow_ok in
    assert (stages modern_program = expected_stages);
    let modern_node = Flow_sop.Attribute_kernel.node ~source:"source-descriptor-proof"
      ~name:"P" ~values:modern ~sources inputs in
    assert ((Sop.Node.facts modern_node).reads = ["P"]);
    if expected_stages = 3 && List.length inputs > 1 then begin
      let change field = List.mapi (fun i node -> if i = 0 then node else
        Sop.Node.Private.with_facts (field (Sop.Node.facts node)) node) inputs in
      assert (stages (prepare (change (fun facts -> {facts with topology = Changed}))) = 1);
      assert (stages (prepare (change (fun facts -> {facts with elementwise = None}))) = 1);
      assert (stages (prepare (change (fun facts -> {facts with elementwise = Primitives}))) = 3)
    end;
    let session = session () in
    Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
      let source_geometries = List.map (fun input -> cook session ~domains:1 ~time:0. input |> geometry) inputs in
      let resolve = Flow_sop.Attribute_kernel.resolve ~geometry:(fun id ->
        Option.bind (List.find_index ((=) id) sources) (fun index -> List.nth_opt source_geometries index)) in
      List.iter (fun time ->
        let live = Input.at_time time in
        let expected = E.Host.force_reference ~resolve values ~live |> flow_ok in
        List.iter (fun domains -> Parallel.run ~domains (fun () ->
          let actual = Flow_ir.Executor.force ~resolve program ~live |> flow_ok in
          let modern = Flow_ir.Executor.force ~strict:true ~resolve modern_program ~live |> flow_ok in
          assert (Marshal.to_string modern [Marshal.No_sharing] = Marshal.to_string expected [Marshal.No_sharing]);
          assert (Marshal.to_string actual [Marshal.No_sharing] = Marshal.to_string expected [Marshal.No_sharing])))
          [1;8]) [0.;0.125;1.25;7.]))
    ["g", 3; "(sop/transform g :translate [0 0 1])", 3;
     "(sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns 1025 :rows 2)", 1;
     "(sop/scatter (sop/grid) :count 2050 :seed 9)", 1]
let () =
  let empty = Kernel.generate_point_ranges 0 (fun ~first:_ ~last:_ ~x:_ ~y:_ ~z:_ -> ()) in
  let empty_input = Sop.Node.Private.make_geometry ~operation:"empty_fixture" ~version:1
    ~parameters:"" ~cook_mode:Sop.Node.Generator
    ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _ -> Ok Sop.Node.Private.{geometry=empty; instances=None}) in
  let empty_write = Flow_sop.Attribute_kernel.node ~source:"empty"
    ~name:"P" ~values:(E.Struct ("sop/attr", Flow.Ty.Array Flow.Ty.Vec3,
      ["geometry", E.Deferred (Flow.Ty.geometry,0); "attribute", E.Text "P"]))
    ~sources:[0] [empty_input] in
  let output = Sop.Node.Private.cook empty_write (context ~domains:8 0.) [|Sop.Payload.Geometry empty|] in
  (match output with Ok output -> assert (geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload)) = geometry_bytes empty)
    | Error d -> failwith (Diagnostic.error_to_string d));
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  let cancelled = Sop.Context.create ~cancel ~domains:1 () |> get_string_ok in
  (match Sop.Node.Private.cook empty_write cancelled [|Sop.Payload.Geometry empty|] with
   | Error d -> assert (d.code = "E_CANCELLED") | Ok _ -> assert false);
  List.iter (fun columns ->
    List.iter (fun amp ->
      let l = lower (source ~columns ~amp noise_body) in
      let node = compiled l and session = session () in
      Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
        let ref_node = reference l node in
        List.iter (fun time ->
          let base = cook session ~domains:1 ~time (List.hd (Sop.Node.inputs node)) |> geometry in
          let before = geometry_bytes base in
          List.iter (fun domains -> Parallel.run ~domains (fun () ->
            let output = cook session ~domains ~time node |> geometry in
            assert (List.exists (fun (report : Flow_ir.execution) -> report.tier = Cpu_kernel)
              (Flow_ir.Profile.executions l.profile));
            let reference = cook session ~domains ~time ref_node |> geometry in
            assert (geometry_bytes output = geometry_bytes reference);
            assert (Geometry.topology output == Geometry.topology base))) [1;8];
          assert (geometry_bytes base = before)) [0.;0.125;1.25;7.]))
      ["0.8"; "(+ 0.8 (* t 0.1))"]) [1; 511; 512; 1025; 8193];
  let l = lower (source noise_body) in
  let disconnected = lower "(workspace w (graph g :context sop (sop/with_attr nil :P (array/vec3 4))))" in
  let graph = List.hd disconnected.graphs in
  assert (Result.is_error (Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root)));
  let named_body = "(let* [f (fn [p n] (+ p (* n (* amp (noise3 (* p freq))))))] (sop/with_attr g :P (exact (map f (sop/attr g :P) (sop/attr g :N)))))" in
  let named = lower (source ~columns:1025 named_body) in
  let call = Array.find_opt (fun (n : E.node) -> n.kind = "sop/with_attr") named.plan.nodes |> Option.get in
  assert (not (Flow.Eval.frame_dependent (List.assoc "values" call.args)));
  let session = session () in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    let node = compiled named in
    let a = cook session ~domains:1 ~time:0. node |> geometry in
    let b = cook session ~domains:8 ~time:0. (reference named node) |> geometry in
    assert (geometry_bytes a = geometry_bytes b);
    let first = cook session ~domains:1 ~time:0. (compiled l) |> geometry |> geometry_bytes in
    let changed = lower ~previous:l (source ~amp:"1.8" noise_body) in
    let second = cook session ~domains:1 ~time:0. (compiled changed) |> geometry |> geometry_bytes in
    assert (first <> second);
    List.iter (fun (body, code) ->
      let l = lower (source body) in
      match cook session ~domains:1 ~time:0. (compiled l) with
      | Error d -> assert (d.code = code)
      | Ok _ -> failwith ("accepted " ^ code))
      ["(sop/with_attr g :P (array/vec3 1))", "E_ATTR_COUNT";
       "(sop/with_attr g :P (sop/attr g :missing))", "E_ATTR_TYPE"];
    let l = lower (source "(sop/with_attr g :Cd (exact (map (fn [p] (* p 0.5)) (sop/attr g :P))))") in
    let output = cook session ~domains:8 ~time:0. (compiled l) |> geometry in
    let cd = Geometry.find_attribute ~owner:Attribute.Point "Cd" output |> Option.get in
    assert (Attribute.length cd = Geometry.point_count output);
    let l = lower (source "(let* [painted (sop/with_attr g :Cd (sop/attr g :P))] (sop/with_attr painted :P (sop/attr painted :Cd)))") in
    let output = cook session ~domains:1 ~time:0. (compiled l) |> geometry in
    assert (Geometry.find_attribute ~owner:Attribute.Point "Cd" output <> None);
    let l = lower (source ~amp:"(state [a 0.8] (+ a 0.1))" noise_body) in
    let graph = List.hd l.graphs and lane = Flow_sop.Value_lane.create () in
    List.iter (fun frame ->
      let time = float frame *. 0.125 in
      let live = { (Input.at_time time) with frame } in
      let resolved = Flow_sop.Value_lane.resolve ~live lane ~time graph.network |> flow_ok in
      let node = Edit.compile_node resolved.geometry ~node_id:(Option.get graph.root) |> get_string_ok in
      let context = Sop.Context.create ~input:live ~time ~frame:(Int64.of_int frame)
        ~domains:8 () |> get_string_ok in
      let output = Session.cook session ~context node |> geometry in
      assert (Geometry.find_attribute ~owner:Attribute.Point "N" output = None))
      [0;1;2]);
  let forms = Flow.Syntax.parse (source noise_body) |> flow_ok in
  let bare = lower (source "(with_attr g :P (attr g :P))") in
  assert (Array.exists (fun (n : E.node) -> n.kind = "sop/with_attr") bare.plan.nodes);
  let reader = lower (source "(let* [arrays (map sop/attr (list g) (list \"P\"))] (sop/with_attr g :P (first arrays)))") in
  let s = Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok in
  Fun.protect ~finally:(fun () -> Session.close s) (fun () ->
    let node = compiled reader in
    let original = cook s ~domains:1 ~time:0. (List.hd (Sop.Node.inputs node)) |> geometry in
    let copy = cook s ~domains:8 ~time:0. node |> geometry in
    assert (geometry_bytes original = geometry_bytes copy));
  let ws = match Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms with
    | Some w, [] -> w | _ -> assert false in
  let pane = Flow_graph.Projection.of_graph catalog ws "g" in
  let result = Flow_graph.Projection.find pane ["g";"result"] |> Option.get in
  assert (List.map (fun (r : Flow_graph.Projection.row) -> r.label) result.rows = ["geometry";"attribute";"values";"owner";"type";"selection";"default"]);
  let fn = List.find (fun (node : Flow_graph.Projection.node) ->
    Option.fold ~none:false ~some:(fun (zone : Flow_graph.Projection.zone) -> zone.kind = Fn) node.zone)
      pane.nodes in
  let rails = (Option.get fn.zone).rail in
  assert (List.for_all (fun name -> List.exists (fun (rail : Flow_graph.Projection.rail_row) ->
    rail.name = name && rail.ty = Some Flow.Ty.Vec3) rails) ["p"; "n"]);
  let large = lower (source ~columns:8193 ~amp:"(+ 0.8 (* t 0.1))" noise_body) in
  let probe_session = Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok in
  Fun.protect ~finally:(fun () -> Session.close probe_session) (fun () ->
    let node = compiled large in
    let base = cook probe_session ~domains:8 ~time:1.25 (List.hd (Sop.Node.inputs node)) |> geometry in
    let geometry_id = Array.find_opt (fun (n : E.node) -> n.kind = "sop/grid") large.plan.nodes
      |> Option.get |> fun n -> n.id in
    let reads = ref 0 in
    let resolve value = incr reads;
      Flow_sop.Attribute_kernel.resolve ~geometry:(fun id -> if id = geometry_id then Some base else None) value in
    let checked = match Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog
      (Flow.Syntax.parse (source ~columns:8193 ~amp:"(+ 0.8 (* t 0.1))" noise_body) |> flow_ok) with
      | Some checked, ds when List.for_all (fun (d : Diagnostic.t) -> d.severity <> Diagnostic.Error_severity) ds -> checked
      | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
    let pane = Flow_graph.Projection.of_graph catalog checked "g" in
    let probe = Flow_graph.Probe.make ~time:1.25 ~resolve large.evaluated in
    assert (List.assoc fn.path
      (Flow_graph.Probe.counts probe pane ~probe:(fun _ -> 0)) = Geometry.point_count base);
    let body = Flow_graph.Projection.find pane (fn.path @ ["@result"]) |> Option.get in
    let before = geometry_bytes base in
    let footer = Flow_graph.Probe.footer probe body ~probes:[16001] in
    assert (footer.runs = Some (Geometry.point_count base));
    assert (match Flow_graph.Probe.at probe body.path ~probes:[16001] with
      | Some (Flow_graph.Probe.Value (E.Vec3 _)) -> true | _ -> false);
    assert (!reads = 2);
    ignore (Flow_graph.Probe.footer probe body ~probes:[16001]);
    assert (!reads = 2 && geometry_bytes base = before);
    let _, changed = Flow_graph.Flow_edit.apply_checked ~ops:Flow_sop.Operators.all catalog
      checked.source (Flow_graph.Flow_edit.Set_arg {node = body.path; key = Pos 0; sub = [];
        value = Flow.Syntax.make (Sym "n")}) |> flow_ok in
    let edited = E.static ~record:true changed |> flow_ok in
    let probe = Flow_graph.Probe.make ~time:1.25 ~resolve edited in
    assert ((Flow_graph.Probe.footer probe body ~probes:[16001]).value <> footer.value));
  let _, changed = Flow_graph.Flow_edit.apply_checked ~ops:Flow_sop.Operators.all catalog forms
    (Flow_graph.Flow_edit.Set_arg {node=["g";"result"]; key=Pos 1; sub=[];
      value=Flow.Syntax.make (Str "Cd")}) |> flow_ok in
  let pane = Flow_graph.Projection.of_graph catalog changed "g" in
  let result = Flow_graph.Projection.find pane ["g";"result"] |> Option.get in
  assert ((List.nth result.rows 1).expr |> Option.get |> Flow.Lisp.flat = "\"Cd\"");
  List.iter (fun (op : Flow.Op.t) ->
    assert (Flow.Op.validate [op] = None);
    let args = List.map (fun (name, ty) -> name, match ty with
      | Flow.Ty.Text -> E.Text (match List.assoc_opt name op.choices with
          | Some (choice::_) -> choice | _ -> "P")
      | Array Int -> E.Int_array [||]
      | Array _ -> E.Vec3_array [||]
      | _ when name="values" && op.name="sop/with_attr" -> E.Vec3_array [||]
      | _ -> E.Deferred (Flow.Ty.geometry,0)) op.signature.pos in
    let args=if op.name="sop/mesh" || op.name="sop/mesh_planes" then
        ["points",(if op.name="sop/mesh" then E.Vec3_array[||] else
          E.Record(List.map(fun axis->axis,E.Float_array[||])["x";"y";"z"]));"vertex_points",E.Int_array[||];
         "primitive_offsets",E.Int_array[|0|];"kinds",E.Int_array[||]]
      else if op.name="sop/remap" then
        ["source",E.Deferred(Flow.Ty.geometry,0);"target",E.Deferred(Flow.Ty.geometry,0);
         "points",E.Int_array[||];"vertices",E.Int_array[||];"primitives",E.Int_array[||]]
      else if List.mem_assoc "name" op.signature.kw then ("name",E.Text "selected")::args else args in
    op.check args;
    let result = op.body ~live:(Input.at_time 0.)
      ~node:(fun _ _ -> E.Deferred (Flow.Ty.geometry,1)) args in
    assert (Flow.Ty.fits (Flow.Value.ty_of result) (op.out (List.map (fun (_,v) -> Flow.Value.ty_of v) args))))
    (List.filter (fun (op : Flow.Op.t) -> op.ctx = Flow.Context.sop) Flow_sop.Operators.all);
  print_endline "Cooked attribute kernel: native/reference/CPU parity, source ownership, cache invalidation, errors and graph edits passed"

let () =
  List.iter(fun count->
    let original=Line_geometry.points(Array.init count(fun i->float i,0.,0.)) in
    let before=geometry_bytes original in
    List.iter(fun(kind,fill)->
      let name="array/"^kind in
      let marker=Flow.Op.constant_array_descriptor name ["count",E.Int count;"value",fill] in
      if kind="float" then begin
        let selection=E.Int_array(Array.make count 1) in
        (match Flow_sop.Attribute_kernel.write_selected ~grain:97 ~owner:Attribute.Point ~kind "constant"
          ~values:marker ~selection ~default:(E.Float nan)original with
         |Error d->assert(d.code="E_NONFINITE")|Ok _->assert false);
        assert(geometry_bytes original=before)
      end;
      let dense=(Option.get(Flow.Op.find name Flow.Context.value)).body ~live:(Input.at_time 0.)
        ~node:(fun _ _->assert false)["count",E.Int count;"value",fill] in
      let resolved=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None)marker|>flow_ok in
      assert(Flow.Value.key_of ~residual:E.Residual.residual_id resolved=Flow.Value.key_of ~residual:E.Residual.residual_id dense);
      List.iter(fun domains->Parallel.run ~domains(fun()->
        let actual=Flow_sop.Attribute_kernel.write_constant_source ~grain:97 ~owner:Attribute.Point ~kind
          "constant" marker original|>flow_ok|>Option.get in
        let expected=Flow_sop.Attribute_kernel.write ~grain:97 ~owner:Attribute.Point ~kind "constant" dense original|>flow_ok in
        assert(geometry_bytes actual=geometry_bytes expected);
        if List.mem kind["float";"vec3";"vec4"]then begin
          List.iter(fun select->
          let selection=E.Int_array(Array.init count select) in
          let actual=Flow_sop.Attribute_kernel.write_constant_source ~grain:97 ~owner:Attribute.Point ~kind "constant"
            ~selection ~default:fill marker original|>flow_ok|>Option.get in
          let expected=Flow_sop.Attribute_kernel.write_selected ~grain:97 ~owner:Attribute.Point ~kind "constant"
            ~selection ~default:fill ~values:dense original|>flow_ok in
          assert(geometry_bytes actual=geometry_bytes expected))
            [(fun _->0);(fun _->1);(fun i->i mod 2)]
        end;
        assert(geometry_bytes original=before))) [1;4;8])
      ["float",E.Float(-0.);"vec3",Vec3(-0.,1.,Float.next_after 0. 1.);
       "vec4",Vec4(-0.,1.,2.,Float.next_after 0. 1.);"int",Int 9007199254740993;"text",Text "猫😀"])
    [0;1;32769];
  List.iter(fun source->match Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None)source with
    |Error d->assert(d.code="E_ARRAY_TYPE")|Ok _->assert false)
    [E.Source("$core/constant-array/array/vec4",Flow.Ty.Array Flow.Ty.Int,["count",Int 1;"value",Vec4(0.,0.,0.,0.)]);
     Source("$core/constant-array/array/vec4",Flow.Ty.Array Flow.Ty.Vec4,[]);
     Source("$core/constant-array/missing",Flow.Ty.Array Flow.Ty.Float,["count",Int 1])];
  print_endline "Constant storage descriptors: full/selected numeric and exact Int/Text, resolver backstop, malformed types, owned bytes at1/4/8"

let ()=
  let mesh="(sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0))"in
  let text=Printf.sprintf {|(workspace remap_census (graph g :context sop
    (sop/remap %s %s :points (array/int 0) :vertices (array/int 0) :primitives (array/int 0))))|}mesh mesh in
  let node=compiled(lower text)and s=session()in
  Fun.protect ~finally:(fun()->Session.close s)(fun()->
    let output=cook s ~domains:1 ~time:0. node|>geometry in
    assert(Geometry.point_count output=0 && Geometry.vertex_count output=0 && Geometry.primitive_count output=0))

(* Full planar values are copied per scalar plane; tuple-array and selected
   writers retain their independent contracts. *)
let planar_storage planes = match Array.length planes with
  |2->Attribute.Float2(Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1)|>get_string_ok)
  |3->Float3(Packed.Float3.View.of_owned_exn ~x:planes.(0) ~y:planes.(1) ~z:planes.(2))
  |_->Float4(Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) ~w:planes.(3)|>get_string_ok)
let planar_record planes = E.Record(List.mapi(fun i data->[|"x";"y";"z";"w"|].(i),E.Float_array data)(Array.to_list planes))
let planar_oracle ~owner ~kind:_ name planes original =
  if owner=Attribute.Point && name="P" then
    match planar_storage(Array.map Array.copy planes) with Attribute.Float3 positions->Geometry.with_positions positions original|>get_string_ok
      |_->assert false
  else Geometry.with_attribute(Attribute.create_owned ~owner ~name(planar_storage(Array.map Array.copy planes))|>get_string_ok)original|>get_string_ok

let ()=
  List.iter(fun n->
    let base=Line_geometry.points(Array.init n(fun i->float i,-0.,0.)) in
    let base=if n=6 then Geometry.create ~positions:(Geometry.positions base)
      ~topology:(Topology.create_owned ~point_count:n ~vertex_points:[|0;1;2;3;4;5|]
        ~primitive_offsets:[|0;3;6|] ~primitive_kinds:[|Topology.Polygon;Open_polyline|]|>get_string_ok)()|>get_string_ok else base in
    let base=Geometry.with_group(Group.ordered ~owner:Group.Point ~name:"retained" ~length:n
      (if n=0 then[||]else if n=1 then[|0|]else[|n-1;0|])|>get_string_ok)base|>get_string_ok in
    let raw=Attribute.create_owned ~owner:Detail ~name:"untouched" (Float[|Int64.float_of_bits 0x7ff8000000001234L|])|>get_string_ok in
    let text=Attribute.create_owned ~owner:Detail ~name:"bytes" (Text[|"\255\000é"|])|>get_string_ok in
    let matrix=Packed.Float_array.create_owned ~offsets:[|0;16|] ~values:(Array.init 16(fun i->if i=0 then -0. else float i))|>get_string_ok in
    let original=List.fold_left(fun geo attribute->Geometry.with_attribute attribute geo|>get_string_ok)base
      [raw;text;Attribute.create_owned ~owner:Detail ~name:"matrix"(Float_array matrix)|>get_string_ok] in
    let before=geometry_bytes original in
    List.iter(fun(owner,owner_name,count)->List.iter(fun(kind,width)->
      let data=Array.init width(fun c->Array.init count(fun i->if i mod 3=0 then -0.
        else if i mod 3=1 then Float.next_after 0. 1. else float c+.float i*.0.125)) in
      let values=planar_record data in
      let expected=planar_oracle ~owner ~kind "planes" data original in
      List.iter(fun domains->List.iter(fun grain->Parallel.run ~domains(fun()->
        let output=Flow_sop.Attribute_kernel.write ~grain ~owner ~kind "planes" values original|>flow_ok in
        assert(geometry_bytes output=geometry_bytes expected && geometry_bytes original=before);
        let storage=Geometry.find_attribute ~owner "planes" output|>Option.get|>Attribute.View.storage in
        let owned=match storage with
          |Float2 p->let p=Packed.Float2.View.view p in [|p.x;p.y|]
          |Float3 p->let p=Packed.Float3.View.view p in [|p.x;p.y;p.z|]
          |Float4 p->let p=Packed.Float4.View.view p in [|p.x;p.y;p.z;p.w|]
          |_->assert false in
        if count>0 then begin
          Array.iteri(fun c plane->assert(plane!=data.(c)))owned;
          owned.(0).(0)<-77.;assert(Int64.bits_of_float data.(0).(0)=Int64.bits_of_float(-0.))
        end)) [1;97;4096]) [1;4;8];
      let error values code message = match Flow_sop.Attribute_kernel.write ~grain:97 ~owner ~kind "bad" values original with
        |Error d->assert(d.code=code);Option.iter(fun text->assert(d.message=text))message
        |Ok _->assert false in
      let fields=match values with E.Record fields->fields|_->assert false in
      List.iter(fun fields->error(E.Record fields)"E_ATTR_TYPE"None)
        [List.tl fields;("extra",E.Float_array[||])::fields;List.hd fields::fields;
         ("x",E.Int_array(Array.make count 0))::List.remove_assoc "x" fields;
         ("x",E.Vec3_array[||])::List.remove_assoc "x" fields];
      let bad=Array.copy data in bad.(0)<-Array.make(count+1)nan;
      error(planar_record bad)"E_ATTR_COUNT"(Some(Printf.sprintf "bad needs %d %s values; got %d." count owner_name (count+1)));
      if count>0 then begin
        let bad=Array.map Array.copy data in bad.(width-1).(count-1)<-infinity;
        error(planar_record bad)"E_NONFINITE"(Some"Attribute values must be finite.");
        let alias=Array.make width data.(0) in
        let output=Flow_sop.Attribute_kernel.write ~grain:97 ~owner ~kind "aliased"(planar_record alias)original|>flow_ok in
        let output=Geometry.find_attribute ~owner "aliased" output|>Option.get|>Attribute.View.storage in
        let a,b=match output with Float2 p->let p=Packed.Float2.View.view p in p.x,p.y
          |Float3 p->let p=Packed.Float3.View.view p in p.x,p.y
          |Float4 p->let p=Packed.Float4.View.view p in p.x,p.y|_->assert false in
        assert(a!=b && a!=data.(0));a.(0)<-99.;assert(Int64.bits_of_float b.(0)=Int64.bits_of_float(-0.))
      end;
      if owner=Point then begin
        if width=3 then assert(geometry_bytes(Flow_sop.Attribute_kernel.write ~grain:97 ~owner ~kind "P" values original|>flow_ok)
          =geometry_bytes(planar_oracle ~owner ~kind "P" data original))
        else match Flow_sop.Attribute_kernel.write ~grain:97 ~owner ~kind "P" values original with
          |Error d->assert(d.code="E_ATTR_TYPE" && d.message="Canonical point P requires vec3 storage.")|Ok _->assert false
      end;
      (match Flow_sop.Attribute_kernel.write_selected ~grain:97 ~owner ~kind "planes" ~values
        ~selection:(E.Int_array(Array.make count 1)) ~default:(if width=2 then E.Vec2(0.,0.) else if width=3 then Vec3(0.,0.,0.) else Vec4(0.,0.,0.,0.)) original with
       |Error d->assert(d.code="E_ATTR_TYPE")|Ok _->assert false);
      assert(geometry_bytes original=before)) ["vec2",2;"vec3",3;"vec4",4])
      [Attribute.Point,"point",n;Vertex,"vertex",Geometry.vertex_count original;
       Primitive,"primitive",Geometry.primitive_count original;Detail,"detail",1]) [0;1;6;32769];
  let op=List.find(fun(op:Flow.Op.t)->op.name="sop/with_attr")Flow_sop.Operators.all in
  List.iter(fun(kind,names)->
    let args ty=["type",Flow.Ty.Text,Some(Param.Text_value kind);"values",ty,None] in
    List.iter(fun fields->assert(Flow.Op.result_type op(args(Flow.Ty.Record fields))=Flow.Ty.geometry))
      [List.map(fun name->name,Flow.Ty.Array Flow.Ty.Float)names;List.map(fun name->name,Flow.Ty.Any)names];
    List.iter(fun ty->try ignore(Flow.Op.result_type op(args ty));assert false with Diagnostic.Error d -> assert(d.code="E_ATTR_TYPE"))
      [Flow.Ty.Record(List.map(fun name->name,Flow.Ty.Array Flow.Ty.Int)names);
       Flow.Ty.Record(("extra",Flow.Ty.Any)::List.map(fun name->name,Flow.Ty.Array Flow.Ty.Float)names)])
    ["vec2",["x";"y"];"vec3",["x";"y";"z"];"vec4",["x";"y";"z";"w"]];
  print_endline "Full planar attributes: all owners/vec widths, native raw payload bytes, positive ownership, capacity type boundary and exact1/4/8-domain errors"

let ()=
  let original=Line_geometry.points(Array.init 65537(fun i->float i,-0.,0.)) in
  let data=Array.init 65537(fun i->float i*.0.125) in
  let original=Geometry.with_attribute(Attribute.create_owned ~owner:Attribute.Point ~name:"scalar"(Float data)|>get_string_ok)original|>get_string_ok in
  let before=geometry_bytes original and cooks=ref 0 in
  let source=Sop.Node.Private.make_geometry ~operation:"test.planar_source" ~version:1 ~parameters:""
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _->incr cooks;Ok Sop.Node.Private.{geometry=original;instances=None}) in
  let factory=Edit.factory ~key:"ext_planar" ~label:"Planar" ~category:["Test"] ~arity:0(fun _->Ok source) in
  let source_text={|(workspace planar_sources
    (defnode sop/planar_sources [(geo : geometry)]
      (let* [make (fn [(xs : (array float))]
                     {:z (array/float (array/count xs) -0.0)
                      :y (exact (map (fn [(x : float)] (+ x (* t 0.25))) xs)) :x xs})
             planes (make (sop/attr geo :scalar :type float)) alias planes]
        (sop/with_attr geo :planar alias)))
    (graph g :context sop (sop/planar_sources (sop/ext_planar))))|} in
  List.iter(fun reference->
    let lowered=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse source_text|>flow_ok)|>flow_ok in
    let graph=List.hd lowered.graphs and lane=Flow_sop.Value_lane.create() in
    let session=session() in
    Fun.protect ~finally:(fun()->Session.close session)(fun()->
      List.iter(fun time->List.iter(fun domains->List.iter(fun grain->
        let network=Flow_sop.Value_lane.resolve lane ~time graph.network|>flow_ok in
        let node=Edit.compile_node network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
        let context=Sop.Context.create ~time ~domains ~grain()|>get_string_ok in
        Session.clear session;cooks:=0;
        let output=Session.cook session ~context node|>geometry in
        assert(!cooks=1);
        let expected=planar_oracle ~owner:Point ~kind:"vec3" "planar"
          [|data;Array.map(fun x->x+.time*.0.25)data;Array.make(Array.length data)(-0.)|] original in
        assert(geometry_bytes output=geometry_bytes expected && geometry_bytes original=before);
        let stats=Session.stats session in ignore(Session.cook session ~context node|>geometry);
        assert((Session.stats session).misses=stats.misses);
        let token=Sop.Context.Cancel.create() in Sop.Context.Cancel.cancel token;
        let cancelled=Sop.Context.create ~time ~domains ~grain ~cancel:token()|>get_string_ok in
        (match Session.cook session ~context:cancelled node with Error d->assert(d.code="E_CANCELLED"||d.code="cancelled")|Ok _->assert false)
      ) [1;97;4096]) [1;4;8]) [0.;1.;0.];
      let small=Line_geometry.points[|0.,-0.,0.;1.,-0.,0.|] in
      let small=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"scalar"(Float[|0.5;1.5|])|>get_string_ok)small|>get_string_ok in
      let node=Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
      let node=Sop.Node.Private.rebuild_with_inputs node[|Lisp_sop.snapshot small|] in
      let expected=planar_oracle ~owner:Point ~kind:"vec3" "planar"[|[|0.5;1.5|];[|0.5;1.5|];[|-0.;-0.|]|]small in
      assert(geometry_bytes(cook session ~domains:4 ~time:0. node|>geometry)=geometry_bytes expected))) [false;true];
  (* Both sources hydrate in authored Record order only in the reached arm. *)
  let lazy_text={|(workspace planar_lazy
    (defnode sop/planar_lazy [(geo : geometry)]
      (if (sop/has_attr geo :enabled :owner detail :type int)
        (sop/with_attr geo :bad {:x (sop/attr geo :missing :type float)
          :y (array/float (sop/count geo :points)) :z (array/float (sop/count geo :points))}) geo))
    (graph g :context sop (sop/planar_lazy (sop/ext_planar))))|} in
  List.iter(fun reference->
    let node=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse lazy_text|>flow_ok)|>flow_ok|>compiled in
    let session=session() in Fun.protect ~finally:(fun()->Session.close session)(fun()->
      assert(geometry_bytes(cook session ~domains:4 ~time:(-1.) node|>geometry)=before);
      let enabled=Geometry.with_attribute(Attribute.create_owned ~owner:Detail ~name:"enabled"(Int[|1|])
        |>get_string_ok)original|>get_string_ok in
      let selected=Sop.Node.Private.rebuild_with_inputs node[|Lisp_sop.snapshot enabled|]in
      (match cook session ~domains:4 ~time:1. selected with Error d->assert(d.code="E_ATTR_TYPE")|Ok _->assert false);
      assert(geometry_bytes(cook session ~domains:4 ~time:(-1.) node|>geometry)=before))) [false;true];
  print_endline "Full planar attributes: real strict/reference cooked-source/Fn/alias hydration, live rebinding, repeated-child sharing, lazy failures and cancellation"

let ()=
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let forms=Flow.Syntax.parse {|(workspace planar_projection (graph g :context sop
    (let* [geo (sop/line) planes_values {:x (array/float 2) :y (array/float 2) :z (array/float 2)}
      result (sop/with_attr geo :planes planes_values)] result)))|}|>flow_ok in
  let check forms=match checker catalog forms with Some w,[]->w|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds)) in
  let checked=check forms in
  let pane=Flow_graph.Projection.of_graph catalog checked "g" in
  let card=Flow_graph.Projection.find pane["g";"result"]|>Option.get in
  assert(card.ty=Flow.Ty.geometry);
  let record=Flow_graph.Projection.find pane["g";"planes_values"]|>Option.get in
  assert(record.ty=Flow.Ty.Record["x",Array Float;"y",Array Float;"z",Array Float]);
  let syntax text=List.hd(Flow.Syntax.parse text|>flow_ok) in
  List.iter(fun name->ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"planes_values"];key=Field name;sub=[];value=syntax"(array/float 2 -0.0)"})|>flow_ok)) ["x";"y";"z"];
  List.iter(fun(key,value)->ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"result"];key;sub=[];value=syntax value})|>flow_ok))
    [Pos 2,"{:z (array/float 2) :x (array/float 2) :y (array/float 2)}";
     Pos 1,"\"dynamic-name\"";Kw"owner","\"vertex\"";Kw"type","\"vec3\""];
  ignore(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Connect{node=["g";"result"];key=Pos 2;src="planes_values";iter=false})|>flow_ok);
  List.iter(fun gesture->assert(Result.is_error(Flow_graph.Flow_edit.apply_checked ~checker catalog forms gesture)))
    [Set_arg{node=["g";"planes_values"];key=Field"x";sub=[];value=syntax"(array/int 2)"};
     Set_arg{node=["g";"result"];key=Kw"type";sub=[];value=syntax"\"vec2\""};
     Set_arg{node=["g";"result"];key=Pos 2;sub=[];value=syntax"{:x (array/float 2) :y (array/float 2) :extra (array/float 2)}"}];
  let printed=fst(Flow.Lisp.print forms) in ignore(check(Flow.Syntax.parse printed|>flow_ok));
  let _,inserted=Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Add_node{scope=["g"];name="another";expr=syntax"(sop/with_attr geo :planes2 planes_values)"})|>flow_ok in
  assert((Flow_graph.Projection.find(Flow_graph.Projection.of_graph catalog inserted "g")["g";"another"]|>Option.get).ty=Flow.Ty.geometry);
  List.iter(fun name->let op=List.find(fun(op:Flow.Op.t)->op.name=name)Flow_sop.Operators.all in
    List.iter(fun attribute->op.check["geometry",E.No_geo;"attribute",E.Text attribute]) ["\194\160";"\255"];
    try op.check["geometry",E.No_geo;"attribute",E.Text" \t\r\n"];assert false with Diagnostic.Error d -> assert(d.code="E_ATTR_NAME"))
    ["sop/attr";"sop/attr_checked";"sop/has_attr";"sop/with_attr"];
  print_endline "Full planar attributes: typed projected Record/geometry cards, field/socket/link edits, exact-key atomic refusal and native byte-blank names"

let ()=
  let s=session() in
  let grid=Fun.protect ~finally:(fun()->Session.close s)(fun()->
    cook s ~domains:1 ~time:0.(Lisp_sop.node"(sop/grid :columns 64 :rows 64 :connectivity \"Quads\")")|>geometry) in
  assert(Geometry.point_count grid=4225 && Geometry.vertex_count grid=16384 && Geometry.primitive_count grid=4096);
  List.iter(fun grid->List.iter(fun(owner,owner_name,count)->
    let data=Array.init 4(fun c->Array.init count(fun i->if c=0 then -0. else float i+.float c*.0.125)) in
    let original=List.fold_left(fun geo c->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name:([|"px";"py";"pz";"pw"|].(c))(Float data.(c))|>get_string_ok)geo|>get_string_ok)grid [0;1;2;3] in
    let before=geometry_bytes original in
    let input=Lisp_sop.snapshot original in
    let factory=Edit.factory ~key:"ext_planes" ~label:"Planes" ~category:["Test"] ~arity:0(fun _->Ok input)in
    List.iter(fun(kind,width)->
      let fields=String.concat" "(List.init width(fun c->Printf.sprintf ":%s (sop/attr geo :%s :owner %s :type float)"
        [|"x";"y";"z";"w"|].(c)[|"px";"py";"pz";"pw"|].(c)owner_name))in
      let values=E.Record(List.init width(fun c->[|"x";"y";"z";"w"|].(c),
        E.Source("sop/attr",Flow.Ty.Array Flow.Ty.Float,["geometry",E.Deferred(Flow.Ty.geometry,0);
          "attribute",E.Text[|"px";"py";"pz";"pw"|].(c);"owner",E.Text owner_name;"type",E.Text"float"])))in
      let direct=Flow_sop.Attribute_kernel.node ~owner ~kind ~strict:true ~source:"planar-facts"
        ~name:"planes" ~values ~sources:[0][input]in
      assert((Sop.Node.facts direct).reads=List.sort String.compare(List.init width(fun c->[|"px";"py";"pz";"pw"|].(c))));
      assert((Sop.Node.facts direct).writes=["planes"] && (Sop.Node.facts direct).topology=Preserved);
      let expected=planar_oracle ~owner ~kind "planes"(Array.sub data 0 width)original in
      List.iter(fun reference->
        let text=Printf.sprintf {|(workspace planar_owners
          (defnode sop/planar_owners [(geo : geometry)]
            (let* [planes_values {%s}] (sop/with_attr geo :planes planes_values :owner %s :type %s)))
          (graph g :context sop (sop/planar_owners (sop/ext_planes))))|} fields owner_name kind in
        let wrapped=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse text|>flow_ok)|>flow_ok|>compiled in
        let s=session()in Fun.protect ~finally:(fun()->Session.close s)(fun()->
          List.iter(fun domains->List.iter(fun grain->let context=Sop.Context.create ~domains ~grain()|>get_string_ok in
            assert(geometry_bytes(Session.cook s ~context wrapped|>geometry)=geometry_bytes expected);
            assert(geometry_bytes(Session.cook s ~context direct|>geometry)=geometry_bytes expected);
            assert(geometry_bytes original=before)) [1;97;4096]) [1;4;8])) [false;true])
      ["vec2",2;"vec3",3;"vec4",4])
    [Attribute.Point,"point",Geometry.point_count grid;Vertex,"vertex",Geometry.vertex_count grid;
     Primitive,"primitive",Geometry.primitive_count grid;Detail,"detail",1])
    [grid;Line_geometry.points[||];Line_geometry.points[|0.,-0.,0.|];
     Line_geometry.points(Array.init 32769(fun i->float i,-0.,0.))];
  (* A geometry fold forces a new cooked Record on each selected write; its
     planes may be captured from the original child without count aliasing. *)
  let source=Lisp_sop.snapshot(Line_geometry.points[|0.,0.,0.;1.,0.,0.|])in
  let fold=Lisp_sop.workspace_node ~with_:["planes",source]{|(workspace planar_fold
    (defnode sop/planar_fold [(geo : geometry)]
      (fold [out geo] [i (range 2)]
        (sop/with_attr out :folded {:x (array/float (sop/count geo :points) (float i))
          :y (array/float (sop/count geo :points) -0.0) :z (array/float (sop/count geo :points) 0.25)})))
    (graph g :context sop (sop/planar_fold (sop/ext_planes))))|}in
  let s=session()in Fun.protect ~finally:(fun()->Session.close s)(fun()->List.iter(fun domains->
    let geo=cook s ~domains ~time:0. fold|>geometry in
    let attr=Geometry.find_attribute ~owner:Point "folded"geo|>Option.get|>Attribute.View.storage in
    match attr with Float3 p->let p=Packed.Float3.View.view p in
      assert(p.x=[|1.;1.|] && Int64.bits_of_float p.y.(0)=Int64.bits_of_float(-0.) && p.z=[|0.25;0.25|])
    |_->assert false)[1;4;8]);
  print_endline "Full planar attributes: real4225-point Quad owner/source identities, strict/reference domain/grain facts and geometry fold"

let ()=
  let original=Line_geometry.points[|0.,0.,0.;1.,0.,0.|] in
  let original=Geometry.with_attribute(Attribute.create_owned ~owner:Detail ~name:"tag"(Int[|1|])|>get_string_ok)original|>get_string_ok in
  let values=E.Record["x",E.Float_array[|-0.;0.25|];"y",E.Float_array[|1.;2.|];"z",E.Float_array[|3.;4.|]] in
  let input=Lisp_sop.snapshot original in
  let node=Flow_sop.Attribute_kernel.node ~strict:true ~source:"planar-component-cache"
    ~name:"planes" ~values ~sources:[0][input]in
  assert((Sop.Node.facts node).reads=[] && (Sop.Node.facts node).writes=["planes"]);
  let changed=Geometry.with_attribute(Attribute.create_owned ~owner:Detail ~name:"tag"(Int[|2|])|>get_string_ok)original|>get_string_ok in
  let s=session()in Fun.protect ~finally:(fun()->Session.close s)(fun()->
    ignore(cook s ~domains:1 ~time:0. node|>geometry);
    let input=Lisp_sop.snapshot changed in ignore(cook s ~domains:1 ~time:0. input|>geometry);
    let stats=Session.stats s in
    let output=cook s ~domains:4 ~time:0.(Sop.Node.Private.rebuild_with_inputs node[|input|])|>geometry in
    assert((Session.stats s).misses=stats.misses && (Session.stats s).hits=stats.hits+2);
    let expected=planar_oracle ~owner:Point ~kind:"vec3" "planes"[|[|-0.;0.25|];[|1.;2.|];[|3.;4.|]|]changed in
    assert(geometry_bytes output=geometry_bytes expected));
  let factory=Edit.factory ~key:"ext_state_planes" ~label:"State planes" ~category:["Test"] ~arity:0(fun _->Ok (Lisp_sop.snapshot original))in
  let text={|(workspace planar_state
    (graph g :context sop
      (let* [geo (sop/ext_state_planes) tick (state [previous 0.0] (+ previous 0.5))
        planes_values {:x (array/float (sop/count geo :points) tick)
          :y (array/float (sop/count geo :points) -0.0)
          :z (array/float (sop/count geo :points) 0.25)}]
        (sop/with_attr geo :state_planes planes_values))))|}in
  List.iter(fun reference->let lowered=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse text|>flow_ok)|>flow_ok in
    let graph=List.hd lowered.graphs and lane=Flow_sop.Value_lane.create()in
    let s=session()in Fun.protect ~finally:(fun()->Session.close s)(fun()->List.iter(fun frame->
      let live={(Input.at_time(float frame)) with frame;tick=frame}in
      let network=Flow_sop.Value_lane.resolve lane ~live ~time:(float frame)graph.network|>flow_ok in
      let node=Edit.compile_node network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
      let context=Sop.Context.create ~input:live ~frame:(Int64.of_int frame) ~time:(float frame) ~domains:4()|>get_string_ok in
      let actual=Session.cook s ~context node|>geometry in
      let expected=planar_oracle ~owner:Point ~kind:"vec3" "state_planes"
        [|Array.make 2(0.5*.float(frame+1));[|-0.;-0.|];[|0.25;0.25|]|]original in
      assert(geometry_bytes actual=geometry_bytes expected);
      assert(geometry_bytes(Session.cook s ~context node|>geometry)=geometry_bytes expected)) [0;1;2]))[false;true];
  print_endline "Full planar attributes: unrelated component refresh, prepared count rebinding, frame state and same-frame stability"

let ()=
  let original=Line_geometry.points[|0.,0.,0.;1.,0.,0.|]in
  let input=Lisp_sop.snapshot original in
  let factory=Edit.factory ~key:"ext_planar_error" ~label:"Planar error" ~category:["Test"] ~arity:0(fun _->Ok input)in
  List.iter(fun reference->List.iter(fun(body,code,message)->
    let text=Printf.sprintf {|(workspace planar_error
      (defnode sop/planar_error [(geo : geometry)]
        (if (sop/has_attr geo :P) (sop/with_attr geo :out %s) geo))
      (graph g :context sop (sop/planar_error (sop/ext_planar_error))))|}body in
    let node=L.workspace ~reference ~factories:(factory::factories)(Flow.Syntax.parse text|>flow_ok)|>flow_ok|>compiled in
    let s=session()in Fun.protect ~finally:(fun()->Session.close s)(fun()->List.iter(fun domains->
      match cook s ~domains ~time:0. node with
      |Error d->assert(d.code=code);Option.iter(fun text->assert(d.message=text))message
      |Ok _->assert false)[1;4;8]))
    ["{:y (sop/attr geo :missing_y :type float) :x (sop/attr geo :missing_x :type float) :z (array/float 1)}",
      "E_ATTR_TYPE",Some"point attribute missing_y is missing or is not float.";
     "{:z (array/float 1) :x (array/float 3) :y (array/float 2)}",
      "E_ATTR_COUNT",Some"out needs 2 point values; got 3.";
     "{:z (array/float 1 (exp 1000.0)) :x (array/float 3) :y (array/float 2)}",
      "E_NONFINITE",None]) [false;true];
  let body=E.Record["z",E.Float_array[||];"y",E.Float_array[||];"x",E.Float_array[|1.;2.;3.|]]in
  (match Flow_sop.Attribute_kernel.write ~grain:97 ~owner:Point ~kind:"vec3" "out"body original with
   |Error d->assert(d.code="E_ATTR_COUNT"&&d.message="out needs 2 point values; got 3.")|Ok _->assert false);
  print_endline "Full planar attributes: authored source/producer order, canonical count error order and host finite-before-count trust boundary"

let ()=
  (* Session rejects an already-cancelled cook before the host call. Invoke the
     actual structural node with immutable captured payloads to test Lower.call
     itself, including options that must beat deferred Record hydration. *)
  let original=Line_geometry.points[|0.,0.,0.|]in
  let factory=Edit.factory ~key:"ext_planar_mixed" ~label:"Planar mixed" ~category:["Test"] ~arity:0
    (fun _->Ok (Lisp_sop.snapshot original))in
  let factories=factory::factories in
  let catalog=Flow_sop.Catalog.of_factories ~version:1 factories|>flow_ok in
  let source owner=Printf.sprintf {|(workspace planar_mixed
    (graph g :context sop (let* [geo (sop/ext_planar_mixed)
      planes_values {:x (sop/attr geo :missing :type float)
        :y (sop/attr geo :missing :type float) :z (sop/attr geo :missing :type float)}]
      (if (sop/has_attr geo :P)
        (sop/with_attr geo :out planes_values :owner "%s" :type vec3) geo))))|}owner in
  let checked owner=match Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all catalog
      (Flow.Syntax.parse(source owner)|>flow_ok)with
    |Some checked,[]->checked|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
  (* Invalid type literals are correctly rejected by ordinary checking. This
     single typed Text replacement exercises the malformed internal host-call
     boundary without loosening that authored literal rule. *)
  let invalid_type (checked:Flow.Workspace.t)=
    let graph=List.hd checked.graphs in
    let body=match graph.body.node with
      |Flow.Workspace.Let(bindings,choice)->
          let choice=match choice.node with
            |Flow.Workspace.If(test,yes,no)->
                let rec replace (term:Flow.Workspace.term)=match term.node with
                  |Flow.Workspace.Let(bindings,body)->{term with node=Let(bindings,replace body)}
                  |Flow.Workspace.Op call when call.op="sop/with_attr"->
                      assert(List.length(List.filter(fun(name,_)->name="type")call.args)=1);
                      let args=List.map(fun(name,(value:Flow.Workspace.term))->name,
                        if name="type"then {value with node=Text "invalid-storage"}else value)call.args in
                      {term with node=Op{call with args}}
                  |_->assert false in
                {choice with node=If(test,replace yes,no)}
            |_->assert false in
          {graph.body with node=Let(bindings,choice)}
      |_->assert false in
    {checked with graphs=[{graph with body}]}in
  let cases=[checked "invalid-owner","E_ATTR_OWNER","Attribute owner is point, vertex, primitive or detail.";
    invalid_type(checked "point"),"E_ATTR_TYPE","Attribute type is float, int, vec2, vec3, vec4, text, float_array or int_array.";
    checked "point","E_ATTR_TYPE","point attribute missing is missing or is not float."]in
  List.iter(fun reference->List.iter(fun(checked,code,message)->
    let lowered=L.of_checked ~reference ~factories checked|>flow_ok in
    let node=compiled lowered in
    assert(Sop.Node.operation node="flow.geometry_select" && List.length(Sop.Node.inputs node)=1);
    let payloads=[|Sop.Payload.Geometry original|]in
    List.iter(fun domains->
      let context=Sop.Context.create ~domains ~grain:97()|>get_string_ok in
      (match Sop.Node.Private.cook node context payloads with
       |Error d->assert(d.code=code && d.message=message)|Ok _->assert false);
      let token=Sop.Context.Cancel.create()in Sop.Context.Cancel.cancel token;
      let cancelled=Sop.Context.create ~domains ~grain:97 ~cancel:token()|>get_string_ok in
      (match Sop.Node.Private.cook node cancelled payloads with
       |Error d->assert(d.code="E_CANCELLED" && d.message="Attribute kernel cancelled.")|Ok _->assert false)) [1;4;8]
  )cases) [false;true];
  print_endline "Full planar host mixed failures: actual Lower call cancellation before invalid options before failing Record source hydration"

let () =
  (* G.1: a defnode body qualifies as a packed producer like an inline map, and its attribute
     write needs no (exact x): the SOP input sink reads a GPU result back. *)
  let text=
    "(workspace gen (defnode sop/gen_shift [(offset : float 0.5)]
       (let* [g (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns 1024 :rows 2)]
         (sop/with_attr g :P (map (fn [i] [(+ i offset) 0 0]) (array/range 2048)))))
       (graph g :context sop (sop/gen_shift :offset 1.0)))" in
  let forms=Flow.Syntax.parse text |> flow_ok in
  match Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms with
  | Some checked, ds ->
      assert(List.for_all(fun(d:Diagnostic.t)->d.severity<>Diagnostic.Error_severity)ds);
      assert(List.exists(fun(path,candidate)->List.mem "node:sop/gen_shift" path
        &&(match candidate with Flow.Workspace.Candidate _->true|_->false))checked.packed);
      let lowered=lower text in
      let session=session() in
      Fun.protect ~finally:(fun()->Session.close session)(fun()->
        let positions=cook session ~domains:4 ~time:0. (compiled lowered) |> geometry |>Geometry.positions in
        for i=0 to 2047 do assert(Packed.Float3.get positions i=(float i+.1.,0.,0.))done);
      print_endline "SOP input sink: a bare map needs no (exact x), including inside a defnode body; catalog parameters still refuse GPU values"
  | None, ds -> failwith(String.concat "; "(List.map Diagnostic.to_string ds))
