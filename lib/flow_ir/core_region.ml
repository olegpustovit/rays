(* Lowers a checked packed form (map, reduce, numeric loop, array/sum) to a core
   region. It reads terms; everything after it reads the region only. *)
module E = Flow.Eval
module W = Flow.Workspace
module V = Flow.Value
module Ty = Flow.Ty
open Core.Region

type expression = { registers : int array; constant : E.value option;
  fields : (string * expression) list }
exception Unsupported
(* Compile-time refusals must bypass the lazy runtime [Diagnostic.Error] handler below. *)
exception Compile_refused of Diagnostic.t

let instruction_key = function
  | Const value -> `Constant (Int64.bits_of_float value)
  | Guarded (guards,Const value) -> `Guarded_constant (guards,Int64.bits_of_float value)
  | instruction -> `Instruction instruction

let width_of ty = match Flow.Packed_ops.scalar_width ty with
  | Some width -> width | None -> raise Unsupported
let components = function
  | E.Vec2 (x,y) -> [|x;y|] | Vec3 (x,y,z) -> [|x;y;z|]
  | Vec4 (x,y,z,w) -> [|x;y;z;w|] | value -> [|V.num value|]

let field (at:W.term) e name = match List.assoc_opt name e.fields with
  | Some value -> value
  | None ->
      let component=match name with "x"->0|"y"->1|"z"->2|"w"->3|_->(-1)in
      if Array.length e.registers>=2 && component>=0 && component<Array.length e.registers then
        {registers=[|e.registers.(component)|];constant=None;fields=[]}
      else raise(Compile_refused(Diagnostic.error ~span:at.form.span ~code:"E_PACKED_TYPE"
        (Printf.sprintf "Component %s is unavailable on a %d-component packed value."
          name (Array.length e.registers))))

let rec leaves ~coerce (at:W.term) ty value = match ty with
  | Ty.Record fields -> List.concat_map (fun (name,ty) -> leaves ~coerce at ty (field at value name)) fields
  | Ty.Int | Text | Float | Bool | Vec2 | Vec3 | Vec4 ->
      let value=coerce at ty value in
      let scalar=match ty with Ty.Vec2 | Vec3 | Vec4 -> Ty.Float | ty -> ty in
      Array.to_list (Array.map (fun register -> register,scalar) value.registers)
  | _ -> raise(Compile_refused(Diagnostic.error ~span:at.form.span ~code:"E_PACKED_TYPE"
      "Carried loop values require scalar, vector or record leaves."))

let rec shape ty next = match ty with
  | Ty.Record fields -> {registers=[||];constant=None;
      fields=List.map (fun (name,ty) -> name,shape ty next) fields}
  | _ ->
      let scalar,width=match ty with
        | Ty.Text -> Ty.Text,1 | Ty.Vec2 | Vec3 | Vec4 -> Ty.Float,width_of ty
        | ty -> ty,1 in
      {registers=Array.init width (fun _ -> next scalar);constant=None;fields=[]}

let rec same_carry_type a b = match a,b with
  |Ty.Record a,Ty.Record b->List.length a=List.length b && List.for_all(fun(name,ty)->
      Option.fold ~none:false ~some:(same_carry_type ty)(List.assoc_opt name b))a
  |_->a=b

let carried_initial ~coerce (seed:W.term) ty value =
  (* ponytail: mixed carries stay in Core until Region retains their initial and later numeric kinds. *)
  if not(same_carry_type seed.ty ty)then raise(Compile_refused(Diagnostic.error
    ~span:seed.form.span ~code:"E_PACKED_TYPE"
    "Carried numeric widening requires the structured Core representation."));
  Array.of_list(List.map fst(leaves ~coerce seed ty value))

let finish_loops finishes code = Array.mapi(fun slot instruction->
  let rec patch=function
    |Loop_begin loop->Loop_begin{loop with finish=Hashtbl.find finishes slot}
    |Guarded(guards,body)->Guarded(guards,patch body)
    |instruction->instruction in
  patch instruction)code

let carried_loop ~emit ~scope ~finishes ty (control:loop) step =
  if control.initial=[||] then raise(Compile_refused(Diagnostic.error
    ~span:control.span ~code:"E_PACKED_TYPE" "A carried loop requires at least one scalar leaf."));
  let header=emit(Loop_begin control) and previous= !scope in
  Fun.protect ~finally:(fun ()->scope:=previous)(fun ()->
    scope:=header::previous;
    let component=ref 0 in
    let carried=shape ty(fun ty->let index= !component in incr component;
      emit(Loop_carry(header,index,ty)))in
    let next,stop=step header carried in
    Hashtbl.add finishes header(emit(Loop_end(header,next,stop))));
  let component=ref 0 in
  shape ty(fun ty->let index= !component in incr component;
    emit(Loop_result(header,index,ty)))

(* A record result (a raster stage's varyings) is one output per numeric leaf, in field order;
   any other result is the single output [lower] builds. *)
let rec record_outputs ~as_float e = function
  | Ty.Record fields -> List.concat_map (fun (name, ty) -> match List.assoc_opt name e.fields with
      | Some field -> (match ty with Ty.Record _ -> record_outputs ~as_float field ty | _ -> [ (as_float field).registers ])
      | None -> raise (Compile_refused (Diagnostic.error ~code:"E_PACKED_TYPE" ("Packed record result lacks " ^ name ^ ".")))) fields
  | _ -> []

let rec integer_work = function
  | Int_const _ -> false
  | Int_input _ | Int_uniform _ | Int_frame _ | Int_accumulator | Int_binary _ | Int_compare _
  | Int_abs _ | Convert _ | Int_select _ | Rand_at _ | Trace _ | Raise _
  | Array_prepare _ | Gather _ | Int_gather _
  | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _
  | Loop_begin _ | Loop_carry _ | Loop_end _ | Loop_result _ -> true
  | Guarded (_,instruction) -> integer_work instruction
  | _ -> false

let rec paths (view:E.Residual.residual_view) sites (t:W.term) =
  let sites=match t.path with Some path->(view.instance,path,view.iter)::sites|None->sites in
  let children=match t.node with Bypass call->Option.to_list(W.bypass_input call)|_->W.children t in
  List.fold_left(paths view)sites children

let iteration_skips iter skip = List.filter_map(fun tuple->match List.rev tuple with
  |k::prefix when List.rev prefix=iter->Some k|_->None)skip
  |> List.sort_uniq Int.compare |> Array.of_list

let canonical_range residual =
  match Flow.Op.find ~extra:(E.Residual.residual_ops residual) "range" Flow.Context.value,
      Flow.Op.find "range" Flow.Context.value with
  |Some actual,Some builtin->actual==builtin|_->false

let captured_value bindings name fields =
  let value = match List.assoc_opt name bindings with Some v -> v | None -> raise Unsupported in
  List.fold_left (fun value field -> match value, field with
    | E.Record fs, field | Struct (_, _, fs), field ->
        (match List.assoc_opt field fs with Some v -> v | None -> raise Unsupported)
    | E.Fn fn, field ->
        (match List.assoc_opt field (E.Residual.function_bindings fn) with
         | Some v -> v | None -> raise Unsupported)
    | E.Vec3 (x, _, _), "x" | Vec3 (_, x, _), "y" | Vec3 (_, _, x), "z" -> E.Float x
    | E.Vec2 (x,_), "x" | Vec2 (_,x), "y" -> E.Float x
    | E.Vec4 (x,_,_,_), "x" | Vec4 (_,x,_,_), "y"
    | Vec4 (_,_,x,_), "z" | Vec4 (_,_,_,x), "w" -> E.Float x
    | _ -> raise Unsupported) value fields

let definition_body ~(at:W.term) residual name = match E.Residual.residual_definition residual name with
  |Some definition->List.map(fun(name,ty,_)->W.Name name,Some ty)definition.inputs,definition.body,None
  |None->raise(Compile_refused(Diagnostic.error ~span:at.W.form.span ~code:"E_PACKED_FUNCTION"
      ("Packed helper "^name^" is unavailable.")))

let helper_body ~(at:W.term) residual bindings fn (name,path) =
  match try Some(captured_value bindings name path)with Unsupported->None with
  |Some(E.Fn callable)->(match E.Residual.function_body callable with
      |Some(params,body)->params,body,Some(name,path)
      |None->(match E.Residual.function_name callable with
          |Some name->definition_body ~at residual name
          |None->raise(Compile_refused(Diagnostic.error ~span:at.W.form.span ~code:"E_PACKED_FUNCTION"
              "Captured helper has no checked body."))))
  |_->definition_body ~at residual fn

let bind_parameters ~coerce (at:W.term) params values env =
  let refuse message=raise(Compile_refused(Diagnostic.error ~span:at.form.span ~code:"E_PACKED_FUNCTION" message))in
  if List.compare_lengths params values<>0 then refuse "Packed helper arguments do not match its checked parameters.";
  List.fold_left2(fun env(pattern,ty)value->match pattern with
    |W.Name name->(name,Option.fold ~none:value ~some:(fun ty->coerce at ty value)ty)::env
    |_->raise(Compile_refused(Diagnostic.error ~span:at.form.span ~code:"E_PACKED_FORM"
        "Packed helper parameters require name patterns.")))env params values

let rec literal_function (fn:W.term)=match fn.node with
  |Fn{capture=Some value;_}|Expanded{body=value;_}->literal_function value
  |Fn{capture=None;_}->true|_->false

let count_of residual (s : W.term) = match s.node with
  | W.Ref_binding (name, fields) ->
      (try match captured_value (E.Residual.residual_view residual).bindings name fields with
       | E.Int_array _ | Text_array _ | Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _ as v -> Some (V.array_length v) | _ -> None
       with Unsupported -> None)
  | Op {op = ("array/int" | "array/text" | "array/float" | "array/vec3" | "array/vec4" | "array/range"); args; _} ->
      (match List.assoc_opt "count" args with
       | Some {node = W.Lit (Param.Int_value n); _} when n >= 0 -> Some n | _ -> None)
  | _ -> None

let count_of_sources iteration counts =
  let combine a b = match iteration with Zip -> min a b | Product ->
    if b <> 0 && a > max_int / b then raise Unsupported else a * b in
  if counts = [||] || (iteration = Product && Array.exists ((=) (Some 0)) counts) then Some 0
  else if Array.for_all Option.is_some counts then
    Some (Array.fold_left (fun n c -> combine n (Option.get c))
      (if iteration = Zip then max_int else 1) counts) else None

let rec pattern_names = function
  | W.Name name -> [name] | W.Keys names -> names | W.Seq ps -> List.concat_map pattern_names ps
let rec reads_names names (term : W.term) =
  let any = List.exists (reads_names names) and fields fs = List.map snd fs in
  match term.node with
  | W.Ref_binding (name, _) -> List.mem name names
  | Fn {params; body; _} ->
      let bound = List.concat_map (fun (p, _) -> pattern_names p) params in
      reads_names (List.filter (fun n -> not (List.mem n bound)) names) body
  | Let (bindings, body) ->
      let rec loop names = function
        | [] -> reads_names names body
        | (p, t) :: rest -> reads_names names t ||
            loop (List.filter (fun n -> not (List.mem n (pattern_names p))) names) rest in
      loop names bindings
  | Op {args; _} | Call {args; _} | Record args -> any (fields args)
  | Call_fn {args; _} -> any args
  | Graph_ref {inputs; _} -> any (fields inputs)
  | Vec ts | List_lit ts | Str ts | List_op (_, ts) | Hof (_, ts) -> any ts
  | Loop {accs; clauses; body; _} -> any (body :: fields (accs @ clauses))
  | State {init; step; _} -> any [init; step]
  | With_error(a,b) -> any[a;b]
  | Error_rows{rows=a;mapper=b;_} -> any[a;b]
  | If (c, a, b) -> any [c; a; b]
  | Cond (arms, d) -> any (d :: List.concat_map (fun (a, b) -> [a; b]) arms)
  | Case (s, arms, d) -> any (s :: d :: fields arms)
  | Get (body, _) | Expanded {body; _} -> reads_names names body
  | Bypass call -> Option.fold ~none:false ~some:(reads_names names) (W.bypass_input call)
  | Assoc (r, args) -> any (r :: fields args)
  | Lit _ | Text _ | Nil | Time | Fn_ref _ -> false

let function_body residual ~bindings ~result_ty ?reduction (f : W.term) =
  let refuse ~(at:W.term) code message = raise(Compile_refused(Diagnostic.error
    ~span:at.W.form.span ~code message))in
  let captured_body (f : W.term) =
    match E.Residual.eval_term residual f ~live:(Input.at_time 0.) with
    | Ok (E.Fn fn) -> (match E.Residual.function_body fn with
        | Some (params, body) -> params, body, E.Residual.function_bindings fn
        | None -> refuse ~at:f "E_PACKED_FUNCTION" "Captured function has no instantiated packed body.")
    | Error d -> raise (Compile_refused d)
    | Ok _ -> refuse ~at:f "E_PACKED_FUNCTION" "Captured callable did not resolve to a function." in
  match f.node with
    | W.Fn {capture = Some _; _} -> captured_body f
    | W.Fn {params; body; _} -> params, body, bindings
    | W.Ref_binding (name, []) ->
        (match List.assoc_opt name bindings with
         | Some (E.Fn fn) -> (match E.Residual.function_body fn with
             | Some (params, body) -> params, body, E.Residual.function_bindings fn
             | None -> refuse ~at:f "E_PACKED_FUNCTION" ("Function " ^ name ^ " has no instantiated packed body."))
         | _ -> refuse ~at:f "E_PACKED_FUNCTION" ("Function binding " ^ name ^ " is unavailable."))
    | W.Fn_ref "+" when Option.is_some reduction ->
        (match Flow.Op.find ~extra:(E.Residual.residual_ops residual) "+" Flow.Context.value with
         | Some op when Flow.Op.packed_kind op = Some (Flow.Op.Binary Add) ->
             let seed, source = Option.get reduction in
             let item_ty = match source.W.ty with Ty.Array ty -> ty | _ ->
               refuse ~at:source "E_PACKED_TYPE" "Packed reduce requires a numeric array." in
             let arg name ty = {f with path=None;ty;node=W.Ref_binding(name,[])} in
             let body = {f with ty=result_ty;node=W.Op {op=op.name;
               args=["a",arg "@accumulator" seed.W.ty;"b",arg "@item" item_ty];skip=[]}} in
             [W.Name "@accumulator",None;W.Name "@item",None],body,bindings
         | _ -> refuse ~at:f "E_PACKED_FUNCTION" "Packed named reduce requires canonical addition.")
    | W.Fn_ref name -> let params,body,_=definition_body ~at:f residual name in params,body,bindings
    | W.Op {op="first";_} -> captured_body f
    | _ -> refuse ~at:f "E_PACKED_FUNCTION" "Packed kernels require a function with an instantiated body."

let hsl ~emit ~as_float ~expression args =
  (* Flow.Op.hsl_channels, step for step, with a select per hue sector *)
  let arg name = (as_float (expression (List.assoc name args))).registers.(0) in
  let h = arg "h" and s = arg "s" and l = arg "l" in
  let k x = emit (Const x) and bin op a b = emit (Binary (op, a, b)) in
  let unit v = bin Max (k 0.) (bin Min (k 1.) v) and abs v = emit (Unary (Abs, v)) in
  let hue = bin Div (bin Mod h (k 360.)) (k 60.) in
  let s = unit s and l = unit l in
  let chroma = bin Mul (bin Sub (k 1.) (abs (bin Sub (bin Mul (k 2.) l) (k 1.)))) s in
  let x = bin Mul chroma (bin Sub (k 1.) (abs (bin Sub (bin Mod hue (k 2.)) (k 1.)))) and zero = k 0. in
  let sector c0 c1 c2 c3 c4 c5 =
    let pick limit yes no = emit (Select (bin Lt hue (k limit), yes, no)) in
    pick 1. c0 (pick 2. c1 (pick 3. c2 (pick 4. c3 (pick 5. c4 c5)))) in
  let m = bin Sub l (bin Div chroma (k 2.)) in
  let byte v = bin Div (emit (Convert (To_float, emit (Convert (Truncate,
    bin Add (bin Mul (unit (bin Add v m)) (k 255.)) (k 0.5)))))) (k 255.) in
  {registers = [| byte (sector chroma x zero zero x chroma); byte (sector x chroma chroma x zero zero);
                  byte (sector zero zero x chroma chroma x) |]; constant = None; fields = []}

let operation ~emit ~as_float ~as_int ~literal ~is_table ~component ~guards
    ~expression residual (t:W.term) op args =
  let refuse ~(at:W.term) code message = raise(Compile_refused(Diagnostic.error
    ~span:at.W.form.span ~code message))in
  let declaration, kind = match Flow.Op.find ~extra:(E.Residual.residual_ops residual) op Flow.Context.value with
    | Some o -> (match Flow.Op.packed_kind o with
        | Some kind -> o, kind | None -> refuse ~at:t "E_PACKED_OPERATOR"
            ("Operator " ^ op ^ " is not a canonical scalar declaration or validated packed intrinsic."))
    | _ -> refuse ~at:t "E_PACKED_OPERATOR" ("Operator " ^ op ^ " is unavailable.") in
  let expressions = List.map (fun (_, t) -> expression t) args in
  if List.for_all (fun e -> Option.is_some e.constant) expressions && not declaration.live then
    literal (declaration.body ~live:(Input.at_time 0.)
      ~node:(fun name _ -> refuse ~at:t "E_PACKED_CONSTANT" ("Constant operator " ^ op ^ " attempted to create node " ^ name ^ "."))
      (List.map2 (fun (name, _) e -> name, Option.get e.constant) args expressions))
  else if declaration.live && args = [] && List.mem t.ty [Ty.Float; Ty.Int; Ty.Bool] then
    {registers = [|emit (if t.ty=Ty.Int then Int_frame declaration.name else Frame declaration.name)|]; constant = None;fields=[]}
  else if kind=Flow.Op.Gather then begin
    match expressions with
    | [array;index] when is_table array ->
        let index=as_int index in
        if Array.length index.registers<>1 then refuse ~at:t "E_PACKED_TYPE" "Array indices require a scalar integer.";
        let prepared=array.registers.(0) and index=index.registers.(0) in
        let registers=if t.ty=Ty.Int then [|emit(Int_gather(prepared,index))|]
          else if t.ty=Ty.Text then [|emit(Text_gather(prepared,index))|]
          else let width=width_of t.ty in Array.init width(fun component->emit(Gather(prepared,index,width,component))) in
        {registers;constant=None;fields=[]}
    | _ -> refuse ~at:t "E_PACKED_CAPTURE" "Array lookup requires an invariant numeric table."
  end
  else if kind=Flow.Op.Trace then begin
    let registers=Array.of_list(List.concat_map(fun e->Array.to_list e.registers)
      (List.mapi(fun i e->if i=0 then as_int e else as_float e) expressions))in
    if Array.length registers<>10 then refuse ~at:t "E_PACKED_TYPE" "scene/trace needs an integer scene and scalar or vec3 arguments.";
    let field k={registers=[|emit(Trace(k,registers))|];constant=None;fields=[]} in
    {registers=[||];constant=None;fields=["t",field 0;"prim",{registers=[|emit(Convert(Truncate,(field 1).registers.(0)))|];constant=None;fields=[]};
      "u",field 2;"v",field 3;"normal",{registers=[|emit(Trace(4,registers));emit(Trace(5,registers));emit(Trace(6,registers))|];constant=None;fields=[]};
      "hit",field 7;
      "instance",{registers=[|emit(Convert(Truncate,(field 8).registers.(0)))|];constant=None;fields=[]}]}
  end
  else if kind=Flow.Op.Stage then begin
    let operands=Array.concat(List.map2(fun(name,(a:W.term))e->if name="texture"then[||]
      else (if a.ty=Ty.Int then as_int e else as_float e).registers)args expressions)in
    let texture()=match List.assoc_opt "texture" args with
      | Some {W.node=W.Lit(Param.Int_value n);_} -> n
      | _ -> refuse ~at:t "E_PACKED_TYPE" (op ^ " names its texture slot with an integer literal.") in
    let stage s={registers=[|emit(Stage(s,operands))|];constant=None;fields=[]} in
    match op with
    | "gpu/fwidth" -> stage Fwidth
    | "gpu/trace-any" -> stage Trace_any | "gpu/trace-curves-any" -> stage Trace_curves_any
    | "gpu/trace-triangles" | "gpu/trace-curves" ->
        let names=if op="gpu/trace-curves" then ["t";"prim";"u";"hit"] else ["t";"prim";"u";"v";"hit";"instance"] in
        {registers=[||];constant=None;fields=List.mapi(fun k name->name,
          stage(if op="gpu/trace-curves" then Trace_curves k else Trace_triangles k))names}
    | "gpu/scene-vertex" | "gpu/scene-vertex-end" ->
        {registers=Array.init 3(fun c->emit(Stage(Scene_vertex(op="gpu/scene-vertex-end",c),operands)));constant=None;fields=[]}
    | "gpu/scene-instance" -> stage Scene_instance
    | "gpu/scene-sphere" | "gpu/scene-strand" ->
        {registers=Array.init 4(fun c->emit(Stage((if op="gpu/scene-sphere" then Scene_sphere c else Scene_strand c),operands)));constant=None;fields=[]}
    | "gpu/sample" | "gpu/sample-level" | "gpu/read" -> let t=texture() in
        let s c=match op with "gpu/sample" -> Sample(t,c) | "gpu/read" -> Read(t,c) | _ -> Sample_level(t,c) in
        {registers=Array.init 4(fun c->emit(Stage(s c,operands)));constant=None;fields=[]}
    | _ -> let t=texture() in
        {registers=Array.init 2(fun c->emit(Stage(Texture_size(t,c),operands)));constant=None;fields=[]}
  end
  else if kind=Flow.Op.Checked_mul || kind=Flow.Op.Checked_fma then begin
    (* ponytail: one destination per instruction computes both projections
       separately; co-compute pairs only if duplicate arithmetic measures hot. *)
    let arguments=List.map as_float expressions in
    let instruction projection=match kind,arguments with
      | Flow.Op.Checked_mul,[a;b] -> Checked_mul(projection,a.registers.(0),b.registers.(0))
      | Flow.Op.Checked_fma,[a;b;c] -> Checked_fma(projection,a.registers.(0),b.registers.(0),c.registers.(0))
      | _ -> refuse ~at:t "E_PACKED_OPERATOR" "Checked arithmetic needs scalar arguments." in
    let field projection={registers=[|emit(instruction projection)|];constant=None;fields=[]} in
    {registers=[||];constant=None;fields=["value",field Value;"valid",field Valid]}
  end
  else begin
    let width = List.fold_left (fun n e -> max n (Array.length e.registers)) 1 expressions in
    let registers = match kind, expressions with
      | Flow.Op.Integer operation, [a;b] ->
          let a=as_int a and b=as_int b in
          [|emit (Int_binary(operation,a.registers.(0),b.registers.(0)))|]
      | Flow.Op.Integer_compare operation, [a;b] ->
          let a=as_int a and b=as_int b in
          [|emit (Int_compare(operation,a.registers.(0),b.registers.(0)))|]
      | Flow.Op.Rand_at, [a;b] ->
          let a=as_int a and b=as_int b in
          [|emit (Rand_at(a.registers.(0),b.registers.(0)))|]
      | Flow.Op.Fma, [a;b;c] ->
          let a=as_float a and b=as_float b and c=as_float c in
          [|emit(Fma(a.registers.(0),b.registers.(0),c.registers.(0)))|]
      | Flow.Op.Convert conversion, [a] ->
          [|emit (Convert(conversion,a.registers.(0)))|]
      | Flow.Op.Noise3, a::rest when Array.length a.registers=3 ->
          let configuration=List.map2(fun (name,_) expression ->
            name,match expression.constant with Some value->value|None->
              refuse ~at:t "E_PACKED_CONSTANT" ("Noise configuration " ^ name ^ " must be constant."))
            (List.tl args) rest in
          let seed,octaves,lacunarity,gain=try Flow.Op.noise_settings configuration with
            | Diagnostic.Error d -> refuse ~at:t "E_PACKED_CONSTANT" d.message in
          if not (Flow.Packed_ops.supported_noise_octaves octaves) then
            refuse ~at:t "E_PACKED_CONSTANT" (Printf.sprintf "Packed noise octaves must be 1 to 64, got %d." octaves);
          [|emit (Noise3 (a.registers.(0), a.registers.(1), a.registers.(2), seed, octaves, lacunarity, gain))|]
      | Flow.Op.Exact, [a] -> a.registers
      | Flow.Op.Length, [a] when Array.length a.registers = 3 ->
          let square i = emit (Binary (Mul, a.registers.(i), a.registers.(i))) in
          let xx = square 0 and yy = square 1 and zz = square 2 in
          let xy = emit (Binary (Add, xx, yy)) in
          let sum = emit (Binary (Add, xy, zz)) in
          [|emit (Unary (Sqrt, sum))|]
      | Flow.Op.Dot, [a;b] when Array.length a.registers=3 && Array.length b.registers=3 ->
          let product i=emit(Binary(Mul,a.registers.(i),b.registers.(i)))in
          let x=product 0 and y=product 1 and z=product 2 in
          let xy=emit(Binary(Add,x,y))in [|emit(Binary(Add,xy,z))|]
      | Flow.Op.Cross, [a;b] when Array.length a.registers=3 && Array.length b.registers=3 ->
          let negative=emit(Const(-1.)) in
          let component i j=let p=emit(Binary(Mul,a.registers.(i),b.registers.(j)))
            and q=emit(Binary(Mul,negative,a.registers.(j)))in emit(Fma(q,b.registers.(i),p))in
          [|component 1 2;component 2 0;component 0 1|]
      | Flow.Op.Normalize, [a] when Array.length a.registers=3 ->
          let square i=emit(Binary(Mul,a.registers.(i),a.registers.(i)))in
          let x=square 0 and y=square 1 and z=square 2 in
          let xy=emit(Binary(Add,x,y))in let norm=emit(Unary(Sqrt,emit(Binary(Add,xy,z))))in
          let zero=emit(Const 0.)in let empty=emit(Binary(Eq,norm,zero))in
          let previous = !guards in
          let scaled = Fun.protect ~finally:(fun () -> guards:=previous) (fun () ->
            guards:=previous @ [empty,false];
            let inverse=emit(Binary(Div,emit(Const 1.),norm))in
            Array.map(fun r -> emit(Binary(Mul,r,inverse)))a.registers) in
          Array.map(fun r -> emit(Select(empty,zero,r)))scaled
      | Flow.Op.Binary op, [a; b] ->
          let a=as_float a and b=as_float b in
          Array.init width (fun i ->
            let register=emit (Binary (op, component a i, component b i)) in
            if t.ty=Ty.Int then emit (Convert(Integer_result declaration.name,register)) else register)
      | Flow.Op.Unary Abs, [a] when t.ty=Ty.Int -> [|emit (Int_abs a.registers.(0))|]
      | Flow.Op.Unary op, [a] when width = 1 ->
          let a=as_float a in [|emit (Unary (op, a.registers.(0)))|]
      | _ -> refuse ~at:t "E_PACKED_OPERATOR" (Printf.sprintf
          "Dynamic operator %s has no packed instruction for %d arguments at width %d." op (List.length expressions) width) in
    {registers; constant = None;fields=[]}
  end

let lower ?(fusion = true) ?(dynamic = false)
    ?(count_source = fun _ _ -> None) residual (term : W.term) =
  let view = E.Residual.residual_view residual in
  let refuse ?(at = term) code message =
    raise (Compile_refused (Diagnostic.error ~span:at.form.span ~code message)) in
  try
    if view.previous then refuse "E_PACKED_STATE" "Previous-state values require reference evaluation.";
    if E.state_dependent (E.Residual residual) then
      refuse "E_PACKED_STATE" "State-dependent kernels require reference evaluation.";
    let function_body = function_body residual ~bindings:view.bindings ~result_ty:term.ty in
    let params, accumulator, body, sources, bindings, iteration, result, skip = match term.node with
      | W.Hof (`Map, f :: sources) ->
          let params, body, bindings = function_body f in
          params, None, body, sources, bindings, Zip, Collect, []
      | W.Hof (`Reduce, [f; seed; source]) ->
          let params, body, bindings = function_body ~reduction:(seed,source) f in
          (match params with
           | acc :: params -> params, Some acc, body, [source], bindings, Zip, Accumulate (seed, false), []
           | _ -> refuse "E_PACKED_FORM" "Packed reduce requires an accumulator parameter.")
      | W.Loop {kind; accs; clauses; body; skip; _} ->
          let accumulator, result = match kind, accs with
            | `For, [] -> None, Collect | `Sum, [] -> None, Sum false
            | (`Fold | `Scan), [p, seed] -> Some (p, None), Accumulate (seed, kind = `Scan)
            | _ -> refuse "E_PACKED_FORM" "Packed fold/scan requires exactly one accumulator." in
          List.map (fun (p, _) -> p, None) clauses, accumulator, body,
            List.map snd clauses, view.bindings, Product, result, skip
      | W.Op {op = "array/sum"; args = [_, source]; _} ->
          let ty = match source.ty with Ty.Array ty -> ty | _ ->
            refuse ~at:source "E_PACKED_TYPE" ("array/sum source is " ^ Ty.to_string source.ty ^ ", not an array.") in
          let body = {source with ty; node = W.Ref_binding ("@item", [])} in
          [W.Name "@item", None], None, body, [source], view.bindings, Zip, Sum true, []
      | _ -> refuse "E_PACKED_FORM" "Packed kernels require map, reduce, a numeric loop or array/sum." in
    let sources = Array.of_list sources in
    let binding_fn = match term.node with
      | W.Hof (_, {node = W.Ref_binding (name, []); _} :: _) -> Some name | _ -> None in
    let names = List.filter_map (function W.Name name, _ -> Some name | _ -> None)
      (params @ Option.to_list accumulator) in
    (* ponytail: correlated clauses stay interpreted; lower their changing source counts before tiling them. *)
    if iteration = Product && Array.exists (reads_names names) sources then
      refuse "E_PACKED_FORM" "Correlated product sources require reference evaluation.";
    let widths = Array.map (fun (s : W.term) -> match
        (if s.ty=Ty.Array Ty.Text then Some 1 else Flow.Packed_ops.array_width s.ty) with
      | Some width -> width
      | None -> refuse ~at:s "E_PACKED_TYPE" ("Packed source must be a float/vector array, got " ^ Ty.to_string s.ty ^ ".")) sources in
    if Array.length sources <> List.length params then
      refuse "E_PACKED_FORM" (Printf.sprintf "Packed kernel has %d sources but %d parameters."
        (Array.length sources) (List.length params));
    let code = ref [] and size = ref 0 and uniforms = ref [] and nuniforms = ref 0 and uniform_names = ref [] in
    let shared = Hashtbl.create 16 and loop_finishes = Hashtbl.create 4 in
    let control_scope = ref [] in
    let guards = ref [] and integer_registers = Hashtbl.create 16 and text_registers = Hashtbl.create 8
    and bottom_registers = Hashtbl.create 4
    and array_registers = Hashtbl.create 8 in
    let table_uniforms = Hashtbl.create 8 in
    let emit instruction =
      let control = match instruction with Loop_begin _ | Loop_carry _ | Loop_end _ -> true | _ -> false in
      let instruction = if !guards=[] then instruction else Guarded (!guards,instruction) in
      (* As in fusion, compare constant bits too: +0 and -0 remain distinct. *)
      let key = !control_scope,instruction_key instruction in
      match if control then None else Hashtbl.find_opt shared key with
      | Some id -> id
      | None ->
          (* ponytail: cap scratch at 512 KiB per block; measure larger bodies before raising it. *)
          if !size >= Flow.Packed_ops.register_limit then
            refuse "E_PACKED_LIMIT" (Printf.sprintf "Packed kernel exceeds %d registers." Flow.Packed_ops.register_limit);
          let id = !size in incr size; code := instruction :: !code;
          Hashtbl.add integer_registers id (Core.Region.integer_register instruction);
          Hashtbl.add text_registers id (Core.Region.text_register instruction);
          let rec bottom = function Raise(Ty.Any,_,_,_)->true|Guarded(_,body)->bottom body|_->false in
          Hashtbl.add bottom_registers id (bottom instruction);
          (match instruction with Array_prepare(_,ty) | Guarded(_,Array_prepare(_,ty)) ->
            Hashtbl.add array_registers id ty | _ -> ());
          if not control then Hashtbl.add shared key id;
          id in
    let array_type = function E.Residual r -> (E.Residual.residual_view r).term.ty
      | value -> V.ty_of value in
    let table at name value =
      let ty=array_type value in
      (match ty with Ty.Array (Ty.Int|Text|Float|Vec2|Vec3|Vec4) -> ()
       | _ -> refuse ~at "E_PACKED_CAPTURE" "Kernel lookup tables require integer, float or vector arrays.");
      let i=match if fst name="" then None else Hashtbl.find_opt table_uniforms name with
        |Some i->i
        |None->let i= !nuniforms in incr nuniforms;uniforms:=value::!uniforms;
            uniform_names:=name::!uniform_names;
            if fst name<>"" then Hashtbl.add table_uniforms name i;i in
      {registers=[|emit(Array_prepare(i,ty))|];constant=None;fields=[]} in
    let is_table e=Array.length e.registers=1 && Hashtbl.mem array_registers e.registers.(0) in
    let rec literal ?(at = term) v =
      match v with
      | E.Int_array _ | Text_array _ | Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _ -> table at ("",[]) v
      | E.Record fields -> {registers=[||];constant=Some v;
          fields=List.map(fun(name,value)->name,literal ~at value)fields}
      | _ ->
      let registers = match v with
        | E.Int _ -> [||] (* Emit a constant only after its integer/float use is known. *)
        | E.Text text -> [|emit(Text_const text)|]
        | Float _ | Bool _ | Vec2 _ | Vec3 _ | Vec4 _ -> Array.map (fun f -> emit (Const f)) (components v)
        | _ -> refuse ~at "E_PACKED_TYPE" ("Packed constant has unsupported type " ^ Ty.to_string (V.ty_of v) ^ ".") in
      {registers; constant = Some v; fields=[]} in
    let as_float e = if is_table e then refuse "E_PACKED_TYPE" "An array handle cannot be used as a number.";
      if Array.exists(fun r->Hashtbl.find text_registers r)e.registers then refuse "E_PACKED_TYPE" "A Text leaf cannot be used as a number.";
      match e.constant with
      | Some (E.Int n) -> literal (E.Float (float n))
      | _ -> {e with registers=Array.map (fun register ->
          if Hashtbl.find integer_registers register then emit (Convert(To_float,register)) else register) e.registers} in
    let as_int e = if is_table e then refuse "E_PACKED_TYPE" "An array handle cannot be used as an integer.";
      if Array.exists(fun r->Hashtbl.find text_registers r)e.registers then refuse "E_PACKED_TYPE" "A Text leaf cannot be used as an integer.";
      match e.constant with
      | Some value ->
          let value=V.coerce_to Ty.Int value in
          (match value with E.Int n -> {registers=[|emit (Int_const n)|];constant=Some value;fields=[]}
           | _ -> assert false)
      | None -> {e with registers=Array.map (fun register ->
          if Hashtbl.find integer_registers register then register else emit (Convert(Coerce_int,register))) e.registers} in
    let as_text e =
      if Array.length e.registers=1 && Hashtbl.find bottom_registers e.registers.(0) then
        (* The guarded Raise has already run before this unreachable value.
           A typed placeholder keeps bottom branches out of numeric banks. *)
        {registers=[|emit(Text_const "")|];constant=None;fields=[]}
      else begin
      if Array.length e.registers<>1 || not(Hashtbl.find text_registers e.registers.(0)) then
        refuse "E_PACKED_TYPE" "Packed Text leaves require text values.";
      e end in
    let input i width = {registers = Array.init width (fun j ->
      if sources.(i).W.ty=Ty.Array Ty.Int then emit (Int_input i)
      else if sources.(i).W.ty=Ty.Array Ty.Text then emit(Text_input i)
      else emit (Input (i, width, j))); constant = None;fields=[]} in
    let uniform at name v = match array_type v with
      | Ty.Array _ -> table at name v
      | _ -> match v with
      | E.Residual _ | E.Source _ | E.Int _ | Text _ | Float _ | Bool _ | Vec2 _ | Vec3 _ | Vec4 _
        when dynamic || E.is_live v || (match v with E.Source _ -> true | _ -> false) ->
          let ty = match v with E.Residual r -> (E.Residual.residual_view r).term.ty | v -> V.ty_of v in
          let width = try if ty=Ty.Text then 1 else width_of ty with Unsupported ->
            refuse ~at "E_PACKED_CAPTURE" ("Capture " ^ String.concat "." (fst name :: snd name)
              ^ " has unsupported type " ^ Ty.to_string ty ^ ".") in
          let i = !nuniforms in incr nuniforms; uniforms := v :: !uniforms;
          uniform_names := name :: !uniform_names;
          {registers = Array.init width (fun j ->
            if ty=Ty.Int then emit (Int_uniform i)
            else if ty=Ty.Text then emit(Text_uniform i) else emit (Uniform (i, j))); constant = None;fields=[]}
      | E.Int _ | Text _ | Float _ | Bool _ | Vec2 _ | Vec3 _ | Vec4 _ -> literal ~at v
      | _ -> refuse ~at "E_PACKED_CAPTURE" ("Capture " ^ String.concat "." (fst name :: snd name)
          ^ " has unsupported type " ^ Ty.to_string (V.ty_of v) ^ ".") in
    let env = List.mapi (fun i (pattern, annotation) ->
      match pattern with
      | W.Name name when annotation = None || annotation = Some
          (Option.get (Ty.elem sources.(i).W.ty)) ->
          name, input i widths.(i)
      | W.Name name -> refuse "E_PACKED_TYPE" (Printf.sprintf
          "Packed parameter %s annotation %s does not match its %d-component source."
          name (Option.fold ~none:"none" ~some:Ty.to_string annotation) widths.(i))
      | _ -> refuse "E_PACKED_FORM" "Packed parameters require name patterns.") params in
    let env = match accumulator, result with
      | Some (W.Name name, annotation), Accumulate (seed, _) ->
          if not (List.mem seed.ty [Ty.Float;Ty.Int;Ty.Vec2;Ty.Vec3;Ty.Vec4]) then
            refuse ~at:seed "E_PACKED_TYPE" ("Packed accumulator " ^ name ^ " has unsupported seed type " ^ Ty.to_string seed.ty ^ ".");
          let width = width_of seed.ty in
          if annotation <> None && annotation <> Some seed.ty
              && not (seed.ty = Ty.Int && annotation = Some Ty.Float) then
            refuse ~at:seed "E_PACKED_TYPE" (Printf.sprintf
              "Packed accumulator %s annotation %s does not match seed type %s."
              name (Option.fold ~none:"none" ~some:Ty.to_string annotation) (Ty.to_string seed.ty));
          let integer=seed.ty=Ty.Int && body.ty=Ty.Int in
          (name, {registers = Array.init width (fun i -> emit (if integer then Int_accumulator else Accumulator i)); constant = None;fields=[]}) :: env
      | None, _ -> env | _ -> refuse "E_PACKED_FORM" "Packed accumulator requires a name pattern." in
    let captures = Hashtbl.create 16 in
    let capture_scope = ref None and call_depth = ref 0 in
    let capture_path name fields = match !capture_scope with
      | None -> name,fields | Some (root,path) -> root,path @ (name :: fields) in
    let rec capture at name fields = match Hashtbl.find_opt captures (name, fields, !guards, !control_scope) with
      | Some e -> e
      | None ->
          let value = try captured_value bindings name fields with Unsupported ->
            refuse ~at "E_PACKED_CAPTURE" ("Captured binding or field " ^ String.concat "." (name :: fields) ^ " is unavailable.") in
          let e = match value with
            | E.Record fs -> {registers=[||];constant=None;
                fields=List.map (fun (key,_) -> key,capture at name (fields @ [key])) fs}
            | _ -> uniform at (name, fields) value in
          Hashtbl.add captures (name, fields, !guards, !control_scope) e; e in
    let captured at name fields = let name,fields = capture_path name fields in
      capture at name fields in
    let invariant_value at =
      let bindings=match !capture_scope with
        |None->bindings
        |Some(name,fields)->(match captured_value bindings name fields with
            |E.Fn fn->E.Residual.function_bindings fn|_->raise Unsupported) in
      uniform at ("",[]) (E.Residual(E.Residual.with_bindings residual ~term:at bindings)) in
    let component e i = e.registers.(if Array.length e.registers = 1 then 0 else i) in
    let rec coerce at ty value = match ty,value.constant with
      | Ty.Array _,_ when is_table value && Hashtbl.find array_registers value.registers.(0)=ty -> value
      | Ty.Int,Some _ -> as_int value
      | _,Some constant -> literal ~at (V.coerce_to ty constant)
      | Ty.Record fields,_ -> {value with fields=List.map (fun (name,ty) ->
          name,coerce at ty (field at value name)) fields}
      | Ty.Int,_ -> as_int value
      | Ty.Text,_ -> as_text value
      | Ty.Float,_ -> as_float value
      | Ty.Bool,_ ->
          let value = as_float value in
          if Array.length value.registers <> 1 then
            refuse ~at "E_PACKED_TYPE" "Packed boolean argument requires a scalar.";
          {value with registers=[|emit(Unary(Not,emit(Unary(Not,value.registers.(0)))))|]}
      | (Ty.Vec2 | Vec3 | Vec4),_ ->
          let value = as_float value and width = width_of ty in
          if Array.length value.registers=width then value
          else if Array.length value.registers=1 then
            {value with registers=Array.make width value.registers.(0)}
          else refuse ~at "E_PACKED_TYPE" "Packed function argument has the wrong vector width."
      | _ -> refuse ~at "E_PACKED_TYPE" ("Packed function argument has unsupported type " ^ Ty.to_string ty ^ ".") in
    let rec expression env (t : W.term) =
      try match t.node with
      | W.Lit (Param.Int_value n) -> literal ~at:t (E.Int n)
      | Lit (Param.Float_value f) -> literal ~at:t (E.Float f)
      | Lit (Param.Bool_value b) -> literal ~at:t (E.Bool b)
      | Text text -> literal ~at:t(E.Text text)
      | Ref_binding (name, fields) ->
          (match List.assoc_opt name env with
           | Some e -> List.fold_left (field t) e fields
           | None ->
               (* A record may contain a deferred vector. Bind that vector as a
                  uniform before projecting its component. *)
               let direct = try let name,fields = capture_path name fields in
                 ignore (captured_value bindings name fields); true
                 with Unsupported -> false in
               if direct then captured t name fields else
               match List.rev fields with
               | component :: prefix -> field t (captured t name (List.rev prefix)) component
               | [] -> refuse ~at:t "E_PACKED_CAPTURE" ("Captured binding " ^ name ^ " is unavailable."))
      | Time -> {registers = [|emit (Frame "t")|]; constant = None;fields=[]}
      | Vec ts when List.mem (List.length ts) [2;3;4] ->
          let components = List.map (fun t -> as_float (expression env t)) ts in
          if not (List.for_all (fun e -> Array.length e.registers = 1) components) then
            refuse ~at:t "E_PACKED_TYPE" "Packed vector components must be scalar values.";
          {registers = Array.of_list (List.map (fun e -> e.registers.(0)) components); constant = None;fields=[]}
      | Record fs -> {registers=[||];constant=None;
          fields=List.map (fun (name,value) -> name,expression env value) fs}
      | Assoc (value,updates) ->
          let value = expression env value in
          let updates = List.map (fun (name,term) -> name,expression env term) updates in
          let fields = List.map (fun (name,old) -> name,
            Option.value ~default:old (List.assoc_opt name updates)) value.fields in
          {value with fields=fields @ List.filter (fun (name,_) -> not (List.mem_assoc name fields)) updates}
      | Get (value, f) -> field t (expression env value) f
      | Loop {kind=`Fold;accs=[W.Name accumulator,seed];
          clauses=[W.Name index_name,({node=Op {op="range";args;skip=[]};_} as source)];
          body;skip=[];_} when canonical_range residual ->
          let initial=carried_initial ~coerce seed t.ty(expression env seed)in
          let bound term=(as_int (expression env term)).registers.(0) in
          let first,last=match List.map snd args with
            | [last] -> let first=emit(Int_const 0) in first,bound last
            | [first;last] -> let first=bound first in first,bound last
            | _ -> refuse ~at:source "E_PACKED_FORM" "A counted loop requires one or two range bounds." in
          carried_loop ~emit ~scope:control_scope ~finishes:loop_finishes t.ty
            {first;last;finish=(-1);initial;
             limit=Some Flow.Op.max_iterations;span=source.form.span}(fun header carried->
            let index={registers=[|header|];constant=None;fields=[]} in
            let value=expression ((index_name,index)::(accumulator,carried)::env) body in
            Array.of_list(List.map fst(leaves ~coerce body t.ty value)),None)
      | Hof(`Iterate,fn::seed::count::until) ->
          let initial=carried_initial ~coerce seed t.ty(expression env seed)in
          let last=(as_int(expression env count)).registers.(0)in
          let first=emit(Int_const 0)in
          carried_loop ~emit ~scope:control_scope ~finishes:loop_finishes t.ty
            {first;last;finish=(-1);initial;limit=None;span=count.form.span}(fun _ carried->
              let value=coerce t t.ty(inline env t fn[carried])in
              let stop=match until with
                |[]->None
                |predicate::_->Some((coerce predicate Ty.Bool(inline env t predicate[carried;value])).registers.(0))in
              Array.of_list(List.map fst(leaves ~coerce t t.ty value)),stop)
      | Cond (arms, default) ->
          expression env (List.fold_right (fun (condition, yes) no ->
            {t with node = If (condition, yes, no)}) arms default)
      | Case (scrutinee, arms, default) ->
          let boolean (value : W.term) = {value with ty = Ty.Bool;
            node = Op {op = "not"; args = ["x", {value with ty = Ty.Bool;
              node = Op {op = "not"; args = ["x", value]; skip = []}}]; skip = []}} in
          expression env (List.fold_right (fun ((literal : Flow.Syntax.t), yes) no ->
            let value, ty = match literal.node with
              | Num s -> (Param.Float_value (float_of_string s), Ty.Float)
              | Sym ("true" | "false" as s) -> Param.Bool_value (s = "true"), Ty.Bool
              | _ -> refuse ~at:t "E_PACKED_FORM" "Packed case arms require numeric or boolean literals." in
            let right : W.term = {path = None; ty; node = Lit value; form = literal} in
            let left, right = if scrutinee.ty = Ty.Bool || ty = Ty.Bool then
              boolean scrutinee, boolean right else scrutinee, right in
            let condition = {t with ty = Ty.Bool;
              node = Op {op = "="; args = ["a", left; "b", right]; skip = []}} in
            {t with node = If (condition, yes, no)}) arms default)
      | Op {op=("array/concat"|"array/slice"|"array/count" as op);_}
          when (match op,t.ty with "array/count",Ty.Int -> true
            |("array/concat"|"array/slice"),Ty.Array _->true|_->false)
          && not(List.exists(fun name->List.mem_assoc name env)(E.Residual.free_names t))
          && Option.bind(Flow.Op.find ~extra:(E.Residual.residual_ops residual) op Flow.Context.value)
              Flow.Op.packed_kind=Some Flow.Op.Constant_only ->
          (* Preserve checked argument order and copying. Invariant composition
             and its count use the existing selected table/integer captures. *)
          invariant_value t
      | If _ when (match t.ty with Ty.Array _ -> true | _ -> false)
          && not(List.exists(fun name->List.mem_assoc name env)(E.Residual.free_names t)) ->
          (* An invariant array choice is a lazy table preparation, not a numeric
             select. Keep its checked conditional and captures until a selected
             lane reaches Array_prepare; row-dependent choices still refuse. *)
          invariant_value t
      | If (condition, yes, no) ->
          let condition = as_float (expression env condition) in
          (match condition.constant with
           | Some v -> expression env (if V.truthy v then yes else no)
           | None ->
               let branch selected body =
                 let previous = !guards in
                 guards := previous @ [condition.registers.(0),selected];
                 Fun.protect ~finally:(fun()->guards:=previous) (fun()->expression env body) in
               let yes = branch true yes and no = branch false no in
               if Array.length condition.registers <> 1 then
                 refuse ~at:t "E_PACKED_TYPE" "Packed conditions require a scalar value.";
               let rec select ty yes no = match ty with
                 | Ty.Record fields -> {registers=[||];constant=None;
                     fields=List.map (fun (name,ty) -> name,select ty (field t yes name) (field t no name)) fields}
                 | _ ->
                     let yes,no=if ty=Ty.Int || ty=Ty.Text then coerce t ty yes,coerce t ty no else as_float yes,as_float no in
                     let width = max (Array.length yes.registers) (Array.length no.registers) in
                     {registers = Array.init width (fun i ->
                       emit ((if ty=Ty.Int then fun c a b->Int_select(c,a,b)
                         else if ty=Ty.Text then fun c a b->Text_select(c,a,b) else fun c a b->Select(c,a,b))
                         condition.registers.(0) (component yes i) (component no i))); constant = None;fields=[]} in
               select t.ty yes no)
      | Call_fn {fn;args;body=specialized} ->
          if !call_depth >= 64 then refuse ~at:t "E_PACKED_LIMIT" "Packed helper call depth exceeds 64.";
          let values = List.map (expression env) args in
          let params,body,scope = helper_body ~at:t residual bindings fn(capture_path fn [])in
          let env = bind_parameters ~coerce t params values []in
          let previous = !capture_scope in
          capture_scope := scope; incr call_depth;
          Fun.protect ~finally:(fun () -> capture_scope := previous; decr call_depth)
            (fun () -> expression env (Option.value ~default:body specialized))
      | Op {op="error"; args; _} when Option.bind
          (Flow.Op.find ~extra:(E.Residual.residual_ops residual) "error" Flow.Context.value)
          Flow.Op.packed_kind=Some Flow.Op.Constant_only ->
          let text (arg:W.term) = match arg.node with
            | Text text -> text
            | Ref_binding(name,fields) -> (match captured_value bindings name fields with
                |E.Text text->text|_->refuse ~at:arg "E_PACKED_CONSTANT" "Packed error code and message must be constant text.")
            | _ -> refuse ~at:arg "E_PACKED_CONSTANT" "Packed error code and message must be constant text." in
          let code=text(List.assoc "code" args) and message=text(List.assoc "message" args) in
          let cause=Option.map text (List.assoc_opt "cause" args) in
          {registers=[|emit(Raise(t.ty,code,message,cause))|];constant=None;fields=[]}
      | Op {op="color/hsl"; args; _} when List.map fst args = ["h";"s";"l"] ->
          hsl ~emit ~as_float ~expression:(expression env) args
      | Op {op; args; _} -> operation ~emit ~as_float ~as_int ~is_table ~component ~guards
          ~literal:(fun value->literal ~at:t value) ~expression:(expression env) residual t op args
      | Let (bindings, result) ->
          let env = List.fold_left (fun env (p, t) -> match p with
            | W.Name n -> (n, expression env t) :: env
            | _ -> refuse ~at:t "E_PACKED_FORM" "Packed let bindings require name patterns.") env bindings in
          expression env result
      | Expanded {body; _} -> expression env body
      | Bypass call ->
          (match W.bypass_input call with
           | Some input when input.ty=t.ty -> expression env input
           | Some input when t.ty=Ty.Float && List.mem input.ty [Ty.Int;Bool] -> as_float(expression env input)
           | Some input when List.mem t.ty [Ty.Vec2;Vec3;Vec4]
               && List.mem input.ty [Ty.Int;Float] ->
               let e=as_float(expression env input) in
               {registers=Array.make (width_of t.ty) e.registers.(0);constant=None;fields=[]}
           | _ -> refuse ~at:t "E_PACKED_TYPE" "Bypass coercion requires reference evaluation.")
      | _ -> refuse ~at:t "E_PACKED_FORM" ("Unsupported packed body form " ^
          Option.value ~default:(Ty.to_string t.ty) (Flow.Syntax.head t.form) ^ ".")
      with Diagnostic.Error d ->
        if !guards<>[] || !control_scope<>[] then {registers=[|emit(Raise(t.ty,d.code,d.message,d.cause))|];constant=None;fields=[]}
        else raise(Diagnostic.Error {d with span=Some(Option.value ~default:t.form.span d.span)})
    and inline env at fn values =
      if !call_depth>=64 then refuse ~at "E_PACKED_LIMIT" "Packed helper call depth exceeds 64.";
      let params,body,_=function_body fn in
      let env=if literal_function fn then env else[]in
      let env=bind_parameters ~coerce at params values env in
      let rec scope (fn:W.term)=match fn.node with
        |Ref_binding(name,fields)->Some(capture_path name fields)
        |Fn{capture=Some value;_}->scope value
        |Fn_ref _->None|_-> !capture_scope in
      let previous= !capture_scope in
      capture_scope:=scope fn;incr call_depth;
      Fun.protect ~finally:(fun()->capture_scope:=previous;decr call_depth)(fun()->expression env body)in
    let result_ty = match term.ty with Ty.Array ty -> ty | ty -> ty in
    let output_expression = expression env body in
    let outputs = record_outputs ~as_float output_expression result_ty in
    let output = (if result_ty=Ty.Text then as_text output_expression else if result_ty=Ty.Int && body.ty=Ty.Int then
      as_int output_expression else if body.ty=Ty.Float && term.ty=Ty.Int || outputs<>[] then output_expression
      else as_float output_expression).registers in
    if outputs=[] && not (List.mem result_ty [Ty.Int;Ty.Text;Ty.Float;Ty.Vec2;Ty.Vec3;Ty.Vec4]) then
      refuse "E_PACKED_TYPE" ("Packed result must be float or vector, got " ^ Ty.to_string result_ty ^ ".");
    let width = if result_ty=Ty.Text then 1 else if outputs<>[] then 0 else width_of result_ty in
    (match result with
     | Sum _ | Accumulate _ when not (List.mem body.ty [Ty.Int;Ty.Float;Ty.Vec2;Ty.Vec3;Ty.Vec4]) ->
         refuse ~at:body "E_PACKED_TYPE" ("Packed reduction body has unsupported type " ^ Ty.to_string body.ty ^ ".")
     | _ -> ());
    (* array_init also broadcasts a scalar to the requested vector width. *)
    let output = if Array.length output = 1 && width > 1 then Array.make width output.(0) else output in
    if Array.length output <> width then refuse "E_PACKED_TYPE" (Printf.sprintf
      "Packed result has %d components but its type requires %d." (Array.length output) width);
    let counts = Array.map (count_of residual) sources in
    let count = try count_of_sources iteration counts with Unsupported ->
      refuse "E_PACKED_LIMIT" "Packed product source count exceeds the integer limit." in
    let sites = paths view [view.instance, Option.value ~default:view.site term.path, view.iter] body in
    let sites = match term.node with
      | W.Hof (_, {node = Fn {zone; _}; _} :: _) -> (view.instance, zone, view.iter) :: sites
      | Hof (_, {node = Ref_binding (name, []); _} :: _) ->
          (match List.assoc_opt name view.bindings with Some (E.Fn f) ->
            (match E.Residual.function_body f with Some (_, {path = Some p; _}) ->
              (view.instance, List.take (max 0 (List.length p - 1)) p, view.iter) :: sites
            | _ -> sites) | _ -> sites)
      | _ -> sites in
    let skip = iteration_skips view.iter skip in
    let code = finish_loops loop_finishes (Array.of_list (List.rev !code)) in
    let code = if Array.exists integer_work code then code else Array.map (function
      | Guarded (guards,Int_const n) -> Guarded (guards,Const(float n))
      | Int_const n -> Const(float n)
      | instruction -> instruction) code in
    Core.Region.make ~residual ~term ~sources:(Array.map (fun t -> residual, t) sources)
      ~widths ~uniforms:(Array.of_list (List.rev !uniforms)) ~uniform_names:(Array.of_list (List.rev !uniform_names))
      ~code ~outputs:(if outputs=[] then [|output|] else Array.of_list outputs) ~count ~origin:None ~iteration ~result ~skip ~stages:[residual, term]
      ~fusion ~dynamic ~count_source ~body ~binding_fn ~sites
  with
  | Compile_refused d -> Error [{d with span=Some(Option.value ~default:term.form.span d.span)}]
  | Diagnostic.Error d -> Error [{d with span=Some(Option.value ~default:term.form.span d.span)}]
  | Invalid_argument message -> Error [Diagnostic.error ~span:term.form.span ~code:"E_PACKED_LAYOUT"
      ("Packed layout is unsupported: " ^ message)]
  | Unsupported -> Error [Diagnostic.error ~span:term.form.span ~code:"E_PACKED_FORM"
      "Packed form requires reference evaluation."]

(* A frame-fold template keeps its captures as uniforms: rebinding reads the next step's bindings. *)
let rebind (t : Core.Region.t) residual =
  let view = E.Residual.residual_view residual in
  if not t.dynamic || List.length t.stages <> 1 || t.term != view.term || view.previous
      || E.state_dependent (E.Residual residual) then None else
  try
    let captured = match t.term.node with
      | W.Hof (_, ({node=Fn {capture=Some _;_};_} as f)::_) -> Some f
      | _ -> None in
    let bindings = match captured,t.binding_fn with
      | Some f,_ ->
          (match E.Residual.eval_term residual f ~live:(Input.at_time 0.) with
           | Ok (E.Fn fn) when Option.fold ~none:false ~some:(fun (_,body) -> body == t.body)
               (E.Residual.function_body fn) -> E.Residual.function_bindings fn
           | _ -> raise Unsupported)
      | None,None -> view.bindings
      | None,Some name -> (match List.assoc_opt name view.bindings with
          | Some (E.Fn f) when Option.fold ~none:false ~some:(fun (_, body) -> body == t.body)
              (E.Residual.function_body f) -> E.Residual.function_bindings f
          | _ -> raise Unsupported) in
    let uniforms = Array.mapi (fun i (name, fields) ->
      if name="" then t.uniforms.(i) else captured_value bindings name fields) t.uniform_names in
    let sources = Array.map (fun (r, term) ->
      if r != t.residual then raise Unsupported; residual, term) t.sources in
    let count = count_of_sources t.iteration (Array.map (fun (r, s) -> count_of r s) sources) in
    match Core.Region.Private.with_captures t ~residual ~uniforms ~sources ~stages:[residual, t.term] ~count with
    | Ok region -> Some region
    | Error _ -> None
  with Unsupported -> None
