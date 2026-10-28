open Flow
open Flow_graph
let catalog=Catalog.{version=1;kinds=[]}
let ok=function Ok x->x|Error d->failwith(Diagnostic.to_string d)
let form text=List.hd(ok(Syntax.parse text))
let check source=match Workspace.check catalog source with Some w,[]->w|_,ds->failwith(String.concat ";"(List.map Diagnostic.to_string ds))
let types=[Ty.Int;Float;Vec2;Vec3;Vec4;Text]
let array ty count=match ty with
 |Ty.Int->"(array/int "^count^" 9007199254740993)"
 |Float->"(array/float "^count^" -0.)"
 |Vec2->"(map (fn [x] [x -0.]) (array/float "^count^"))"
 |Vec3->"(array/vec3 "^count^" [1 -0. 2])"
 |Vec4->"(array/vec4 "^count^" [1 -0. 2 3])"
 |Text->"(array/text "^count^" \"é日本😀\")"
 |_->assert false
let ()=
 List.iter(fun ty->List.iter(fun(name,args,key,replacement,expected_count)->
  let source=ok(Syntax.parse("(workspace composition (graph g :context host (let* [tested ("^name^" "^args^")] tested)))"))in
  let checked=check source in
  let card=Option.get(Projection.find(Projection.of_graph catalog checked "g")["g";"tested"])in
  assert(card.head=name && card.ty=Ty.Array ty);
  assert((List.hd card.rows).ty=Some(Ty.Array Ty.Any) && (List.hd card.rows).socket);
  if name="array/slice"then List.iter(fun key->assert(List.exists(fun(row:Projection.row)->row.key=key && row.ty=Some Ty.Int)card.rows))[Pos 1;Pos 2];
  let changed,checked=ok(Flow_edit.apply_checked catalog source(Set_arg{node=card.path;key;sub=[];value=form replacement}))in
  let result=List.assoc "g"(ok(Eval.run ~time:0. checked)).results in
  assert(Value.ty_of result=Ty.Array ty && Value.array_length result=expected_count);
  let saved,_=Lisp.print changed in let loaded=check(ok(Syntax.parse saved))in
  assert(Marshal.to_string(List.assoc "g"(ok(Eval.run ~time:0. loaded)).results)[Marshal.No_sharing]=
    Marshal.to_string result[Marshal.No_sharing]);
  assert(Result.is_error(Flow_edit.apply_checked catalog source(Set_arg{node=card.path;key=Pos 0;sub=[];value=form "3"})));
  if name="array/slice"then assert(Result.is_error(Flow_edit.apply_checked catalog source
    (Set_arg{node=card.path;key=Pos 1;sub=[];value=form "\"bad\""}))))
 ["array/concat",array ty "3"^" "^array ty "2",Pos 1,array ty "4",7;
  "array/slice",array ty "3"^" 0 3",Pos 2,"1",1];
 let default=Option.get(Flow_edit.default_for(Ty.Array ty)"source")in
 let expression=Syntax.make(Syntax.List[Syntax.make(Syntax.Sym "array/concat");default;default])in
 let source=ok(Syntax.parse "(workspace menu (graph g :context host (array/int 0)))")in
 let _,added=ok(Flow_edit.apply_checked catalog source(Add_node{scope=["g"];name="joined";expr=expression}))in
 let card=Option.get(Projection.find(Projection.of_graph catalog added "g")["g";"joined"])in
 assert(card.ty=Ty.Array ty))types;
 (* Inline composition is an ordinary nested card in the map's Fn zone and its
    bounds can be edited directly; it is not hidden in a host callback. *)
 let source=ok(Syntax.parse "(workspace inline (graph g :context host (let* [table (array/int 13) tested (map (fn [(i : int)] (array/nth (array/slice (array/concat table table) 0 13) i)) (array/range 13 :type int))] tested)))")in
 let checked=check source in
 let rec find head(scope:Projection.scope)=List.find_map(fun(node:Projection.node)->
  if node.head=head then Some node else Option.bind node.zone(fun zone->find head zone.scope))scope.nodes in
 let pane=Projection.of_graph catalog checked "g"in
 let slice=Option.get(find "array/slice" pane)and concat=Option.get(find "array/concat" pane)in
 assert(slice.ty=Ty.Array Ty.Int && concat.ty=Ty.Array Ty.Int);
 let _,edited=ok(Flow_edit.apply_checked catalog source(Set_arg{node=slice.path;key=Pos 1;sub=[];value=form "13"}))in
 assert(Value.array_length(List.assoc "g"(ok(Eval.run ~time:0. edited)).results)=13);
 print_endline "C6 graph: typed concat/slice cards, defaults, nested projection, direct edits and saved-source round trip"
