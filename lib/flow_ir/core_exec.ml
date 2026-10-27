module C = Core
module V = Flow.Value
module E = Flow.Eval

(* A resident fold's device: inside a qualifying top-level loop of a drawing, a region's array may stay
   on the GPU, standing in the locals as a placeholder (zeros of its capacity) the device knows. *)
type resident = {
  invoke : Packed.t -> live:Input.t -> E.value option;  (* run on the device; None leaves it to the CPU *)
  compact : E.value -> E.value option;  (* array/compact of a placeholder mask, on the device *)
  coerce : Flow.Ty.t -> E.value -> E.value option;  (* a placeholder retyped, on the device *)
  finish : E.value -> E.value;  (* the host array a placeholder stands for; any other value as it is *)
  close : unit -> unit;  (* the loop is over: forget its placeholders *)
}

type state = {
  values : E.value array; mutable builders : bool ref option array;  (* allocated by the first builder, so a call that builds nothing pays nothing *) calls : int array;
  live : Input.t; root : E.residual option;
  force : (Packed.t -> (E.value, Diagnostic.t) result) option;
  resolve : (E.value -> (E.value, Diagnostic.t) result) option;
  marks : Core_marks.sink option;  (* the table the draw statements of a draw program fill *)
  device : (Packed.t -> ((int * int * int) array option, Diagnostic.t) result) option;
  resident : resident option; mutable resident_on : bool;
}
type prepared = { program : C.program; body : state -> unit;
  pool : E.value array option Atomic.t;  (* the locals of a large program, reused across calls: a fresh one is allocated on the major heap *)
  stateless : bool  (* no state slot and no region: a failure leaves the evaluation state untouched *) }
exception Non_finite

let yes = V.Bool true and no = V.Bool false
let seal st n = if Array.length st.builders>0 then Option.iter (fun writable -> writable:=false) st.builders.(n)

(* Resolved once per prepared program, not per executed statement. *)
let operator name = match Flow.Op.find name Flow.Context.value with
  | Some op -> op
  | None -> V.fail "E_CORE" ("Unsupported core primitive: " ^ name)
let apply_op op st args =
  op.Flow.Op.body ~live:st.live ~node:(fun _ _ -> V.fail "E_CORE" "Core primitives cannot create catalog nodes.") args

let generic_named name args =
  let op = lazy (operator name) in
  fun st -> apply_op (Lazy.force op) st (List.map (fun (n,i) -> n,st.values.(i)) args)

(* A local of a plain scalar type never holds a list builder, so it needs no ownership bookkeeping. *)
let plain : Flow.Ty.t -> bool = function Float | Int | Bool | Vec2 | Vec3 | Vec4 | Text -> true | _ -> false

let primitive ~track ~locals op args =
  let get st = List.map (fun (name,n) -> name,st.values.(n)) args in
  let first = match args with (_,n)::_ -> n | [] -> -1 in
  let seal_args = match op with C.Sequence_get | Sequence_length | Sequence_build -> false | _ -> true in
  let sealed = List.filter (fun (_,n) -> not (plain locals.(n))) args in
  let sealing st = List.iter (fun (_,n) -> seal st n) sealed in
  let a () = List.assoc "a" args and b () = List.assoc "b" args in
  let a_ key = List.assoc key args in
  let name = C.primitive_name op in
  let generic () =
    let op = lazy (operator name) in
    fun st -> apply_op (Lazy.force op) st (get st) in
  let body : state -> E.value = match op with
    | C.Coerce ty -> fun st -> V.coerce_to ty st.values.(first)
    | Truthy -> fun st -> V.Bool (V.truthy st.values.(first))
    | Show -> fun st -> V.Text (V.show_with Fun.id st.values.(first))
    | Text_concat -> fun st -> V.Text (String.concat "" (List.map (function _,V.Text s -> s
        | _ -> V.fail "E_TYPE" "Expected text.") (get st)))
    | Fail (code,message) -> fun _ -> V.fail code message
    | Fail_diagnostic d -> fun _ -> V.raise_diagnostic d
    | Error ->
        let code=List.assoc "code" args and message=List.assoc "message" args in
        fun st -> (match st.values.(code),st.values.(message) with
          | V.Text code,V.Text message ->
              let cause=match List.assoc_opt "cause" args with None->None|Some index->
                (match st.values.(index) with V.Text cause->Some cause|_->V.fail "E_TYPE" "error cause must be text.") in
              V.raise_diagnostic(Diagnostic.error ?cause ~code message)
          | _ -> V.fail "E_TYPE" "error needs text code and message.")
    | Rand_at ->
        let seed=List.assoc "seed" args and index=List.assoc "index" args in
        fun st -> (match st.values.(seed),st.values.(index) with
          | V.Int seed,V.Int index -> V.Float(Flow.Packed_ops.rand_at seed index)
          | _ -> V.fail "E_TYPE" "rand/at needs integer seed and index.")
    | Array_reduce operation ->
        let values=a_ "array" in
        fun st -> Flow.Op.Private.reduce_array operation st.values.(values)
    | Array_segment operation ->
        let values=a_ "values" and offsets=a_ "offsets" in
        fun st -> Flow.Op.Private.reduce_array
          ~rows:(fun ~start ~finish f -> Parallel.for_ ~start ~finish f) operation
          ~offsets:st.values.(offsets) st.values.(values)
    | Array_segment_argmax ->
        let values=a_ "values" and offsets=a_ "offsets" in
        fun st -> Flow.Op.Private.segment_argmax
          ~rows:(fun ~start ~finish f -> Parallel.for_ ~start ~finish f)
          ~offsets:st.values.(offsets) st.values.(values)
    | Remainder -> let a=a () and b=b () in
        fun st -> V.Float (V.fin "remainder" (Float.rem (V.num st.values.(a)) (V.num st.values.(b))))
    | Array_count when List.map fst args=["array"] -> fun st -> V.Int (V.array_length st.values.(first))
    | Array_nth when List.map fst args=["array";"index"] ->
        (* A whole index inside native storage, as the operator checks; anything else is its own body. *)
        let array=a_ "array" and index=a_ "index" and slow=generic () in
        fun st -> (match st.values.(index) with
          | V.Int i when i>=0 && i<=Sys.max_floatarray_length -> V.array_get st.values.(array) i
          | _ -> slow st)
    | Sequence_get -> generic_named "nth" args | Sequence_length -> generic_named "count" args
    | Sequence_build ->
        let count = List.assoc "count" args and width = List.assoc_opt "width" args
        and integer = List.assoc_opt "type" args and cooked=List.assoc_opt "cooked" args in
        (* With a width the builder is packed storage, which has no list cap. *)
        fun st ->
          let count=V.int_of st.values.(count) in
          let element=match integer with None -> Flow.Ty.Float
            | Some n -> (match st.values.(n) with V.Text "int" -> Flow.Ty.Int
              | Text "float" -> Float | Text "text" -> Text
              | _ -> V.fail "E_ARRAY_TYPE" "Core array type is int, float or text.") in
          (match width with
           | None ->
               if element<>Flow.Ty.Float then V.fail "E_ARRAY_TYPE" "Typed builders require width one.";
               let limit=match cooked with None->4096|Some n->(match st.values.(n) with V.Bool true->Sys.max_array_length|Bool false->4096
                   |_->V.fail "E_TYPE" "Core cooked collection flag must be boolean.") in
               if count<0 || count>limit then V.fail "E_ITER_BOUND" "Core list length exceeds its bound.";
               V.List (Array.make count V.No_geo)
           | Some width ->
               let width=V.int_of st.values.(width) in
               if width<1 || width>4 || element<>Flow.Ty.Float && width<>1 then
                 V.fail "E_ARRAY_TYPE" "Core packed width is one to four; integers require width one.";
               if element=Flow.Ty.Int then V.array_init Flow.Ty.Int count (fun _ -> V.Int 0)
               else if element=Flow.Ty.Text then V.array_init Flow.Ty.Text count (fun _ -> V.Text "") else begin
               if count<0 || count>Sys.max_floatarray_length/width then
                 V.fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
               let data=Array.make (count*width) 0. in
               (match width with 2 -> V.Vec2_array data | 3 -> Vec3_array data | 4 -> Vec4_array data
                | _ -> Float_array data) end)
    | Sequence_slice ->
        let list = List.assoc "list" args and first = List.assoc "first" args and count = List.assoc "count" args in
        fun st ->
          let xs=V.list_arg st.values.(list) in
          let first=V.int_of st.values.(first) and count=V.int_of st.values.(count) in
          if first<0 || count<0 || first>Array.length xs || count>Array.length xs-first then
            V.fail "E_LIST_RANGE" "A list slice must be within its source.";
          V.List (Array.sub xs first count)
    | Sequence_concat ->
        fun st ->
          let arrays=List.map (fun (_,n) -> V.list_arg st.values.(n)) args in
          let count=List.fold_left (fun n xs -> if Array.length xs>4096-n then
            V.fail "E_ITER_BOUND" "Core concatenation exceeds 4,096 values."; n+Array.length xs) 0 arrays in
          let xs=Array.make count V.No_geo and at=ref 0 in
          List.iter (fun part -> Array.blit part 0 xs !at (Array.length part);at:= !at+Array.length part) arrays;
          let ty=V.elem_ty xs in V.List (Array.map (V.coerce_to ty) xs)
    | Color_parse ->
        fun st ->
          let c=Flow.Op.drawing_color st.values.(first) in
          V.Record ["r",Int c.r;"g",Int c.g;"b",Int c.b;"a",Int c.a]
    | Color_byte mode ->
        fun st ->
          let f=V.fin "color" (V.num st.values.(first)) in
          let f=match mode with C.Clamped -> max 0. (min 1. f) | Unclamped -> f in
          V.Int (max 0 (min 255 (int_of_float (f *. 255. +. 0.5))))
    | Numeric (Lt|Le|Gt|Ge|Eq|And|Or as compare) when List.map fst args=["a";"b"] ->
        (* The operators' own bodies, without building an argument list per call. *)
        let a=a () and b=b () in
        let test : E.value -> E.value -> bool = match compare with
          | Lt -> fun x y -> V.num x < V.num y | Le -> fun x y -> V.num x <= V.num y
          | Gt -> fun x y -> V.num x > V.num y | Ge -> fun x y -> V.num x >= V.num y
          | Eq -> fun x y -> V.num x = V.num y
          | And -> fun x y -> V.truthy x && V.truthy y | _ -> fun x y -> V.truthy x || V.truthy y in
        fun st -> if test st.values.(a) st.values.(b) then yes else no
    | Equal when List.map fst args=["a";"b"] ->
        let a=a () and b=b () in
        fun st -> (try if st.values.(a) = st.values.(b) then yes else no
          with Invalid_argument _ -> V.fail "E_TYPE" "Functions cannot be compared.")
    | Numeric (Add|Sub|Mul as op) when List.map fst args=["a";"b"] ->
        (* Two Floats whose result is finite, without the generic dispatch; anything else is the operator's own body. *)
        (match Flow.Op.arith name with
         | Some arithmetic ->
             let a=a () and b=b () in
             let slow st = arithmetic.apply st.values.(a) st.values.(b) in
             (match op with
              | Add -> fun st -> (match st.values.(a),st.values.(b) with
                  | V.Float x,V.Float y -> let r=x +. y in if Float.is_finite r then V.Float r else slow st
                  | _ -> slow st)
              | Sub -> fun st -> (match st.values.(a),st.values.(b) with
                  | V.Float x,V.Float y -> let r=x -. y in if Float.is_finite r then V.Float r else slow st
                  | _ -> slow st)
              | _ -> fun st -> (match st.values.(a),st.values.(b) with
                  | V.Float x,V.Float y -> let r=x *. y in if Float.is_finite r then V.Float r else slow st
                  | _ -> slow st))
         | None -> generic ())
    | Math (Sin|Cos as op) ->
        let x=first and slow=generic () in
        (match op with
         | Sin -> fun st -> (match st.values.(x) with
             | V.Float v -> let r=sin v in if Float.is_finite r then V.Float r else slow st
             | _ -> slow st)
         | _ -> fun st -> (match st.values.(x) with
             | V.Float v -> let r=cos v in if Float.is_finite r then V.Float r else slow st
             | _ -> slow st))
    | Numeric _ when List.map fst args=["a";"b"] ->
        (match Flow.Op.arith name with
         | Some arithmetic -> let a=a () and b=b () in fun st -> arithmetic.apply st.values.(a) st.values.(b)
         | None -> generic ())
    | _ -> generic ()
  in
  if track && seal_args && sealed<>[] then fun st -> sealing st; body st else body

let builds statements =
  let found = ref false in
  C.iter (fun (s : C.statement) -> match s.node with
    | Assign (_, Prim (Sequence_build, _)) -> found := true
    | _ -> ()) statements;
  !found

 (* A top-level loop of a live program keeps its last results keyed by what it reads from outside
   itself, so a drawing whose loop inputs did not change skips it (the folds of recursive_rectangles
   while the pointer rests). [footprint] is those reads and every local the loop assigns, or [None]
   for a loop that reads the frame, state, a host or a random stream, or draws. Call counters are
   per execution and reset inside the loop. ponytail: a later statement reading the same call slot
   would see the counter as it was before the hit; no lowering shares a slot across statements today. *)
(* what a region reads besides its Core arguments: a host source, or a residual over the frame or
   state, is an input the loop's key does not see *)
let rec hidden = function
  | E.Source (name, _, _) -> name <> "$core/argument"
  | E.Residual r as v -> E.frame_dependent v || E.state_dependent v
      || List.exists (fun (_, v) -> hidden v) (E.Residual.residual_view r).bindings
  | E.Deferred _ -> true
  | E.List xs -> Array.exists hidden xs
  | E.Record fs | E.Struct (_, _, fs) -> List.exists (fun (_, v) -> hidden v) fs
  | _ -> false

let footprint (program : C.program) (s : C.statement) =
  let reads = ref [] and writes = ref [] in
  let rec frame_free = function
    | C.Region.Frame _ | Int_frame _ | Rand_at _ -> false
    | Guarded (_, i) -> frame_free i
    | _ -> true in
  let validate (s : C.statement) = match s.node with
    | Assign (_, Prim ((Rand | Rand_at), _)) -> raise Exit
    | Assign (_, (Frame _ | Source _ | Host_source _ | Source_descriptor _)) -> raise Exit
    | Invoke {region; _} ->
        let r = program.regions.(region) in
        if not (Array.for_all frame_free r.code) || Array.exists hidden r.uniforms
           || Array.exists (fun (residual, _) -> hidden (E.Residual residual)) r.sources then raise Exit
    | Draw _ | State_read _ | State_peek _ | State_write _ -> raise Exit
    | _ -> () in
  let collect (statement : C.statement) =
    validate statement;
    reads := C.reads statement @ !reads;
    writes := C.writes statement @ !writes in
  match C.iter collect [s] with
  | () ->
      let writes = List.sort_uniq compare !writes in
      Some (Array.of_list (List.filter (fun n -> not (List.mem n writes)) (List.sort_uniq compare !reads)),
            Array.of_list writes)
  | exception Exit -> None

(* Every local a statement mentions, its nested statements' included. *)
let mentions acc (s : C.statement) =
  let all = ref acc in
  C.iter (fun statement ->
    let slots = C.reads statement @ C.writes statement in
    let slots = match statement.node with Reset_call n -> n :: slots | _ -> slots in
    all := List.rev_append slots !all) [s];
  !all

(* A loop whose arrays can stay on the device between its statements: its body holds regions, scalar
   statements, ifs, array/compact, retyping and array/count, and no statement reads an array's contents
   on the host. *)
let resident_loop (program : C.program) (s : C.statement) =
  let array n = match program.locals.(n) with Flow.Ty.Array _ -> true | _ -> false in
  let scalars = List.for_all (fun n -> not (array n)) in
  let expr = function
    | C.Constant _ | Local _ | Field _ | Frame _ | Call_index _ -> true
    | Prim ((Array_compact | Coerce _ | Array_count), _) -> true
    | Prim (_, args) -> scalars (List.map snd args)
    | Vec ns | Items ns -> scalars ns
    | Fields xs -> scalars (List.map snd xs)
    | Source _ | Host_source _ | Source_descriptor _ -> false in
  let rec ok (s : C.statement) = match s.node with
    | Assign (_, e) -> expr e
    | If _ -> List.for_all ok (C.children s)
    | Invoke _ -> true
    | _ -> false in
  match s.node with
  | Iterate {body; _} -> List.for_all ok body && List.exists (fun (s : C.statement) ->
      match s.node with Invoke _ -> true | _ -> false) body
  | _ -> false

(* A resident loop's writes that later statements read: the ones its end reads back. *)
let resident_live (program : C.program) (s : C.statement) =
  match footprint program s with
  | Some (_, writes) when resident_loop program s ->
      let outside = List.fold_left mentions [] (List.filter (fun t -> t != s) program.body) in
      Some (Array.of_list (List.filter (fun n -> List.mem n outside) (Array.to_list writes)))
  | _ -> None

(* the same input: a scalar by its bits (NaN and -0 stay distinct), a packed array up to 1M
   numbers by its bits (an array rebuilt each frame with the same contents), a list up to 4,096 items
   item by item (a range, a frame's event list), a record field by field, anything else by identity *)
let same_floats x y =
  x == y || (Array.length x = Array.length y && Array.length x <= 1_048_576 &&
             let rec go i = i < 0 || (Int64.equal (Int64.bits_of_float x.(i)) (Int64.bits_of_float y.(i)) && go (i - 1)) in
             go (Array.length x - 1))
let rec same_input a b = a == b || match a, b with
  | V.List x, V.List y -> Array.length x = Array.length y && Array.length x <= 4096 && Array.for_all2 same_input x y
  | V.Record x, V.Record y ->
      List.compare_lengths x y = 0 && List.for_all2 (fun (n, a) (m, b) -> String.equal n m && same_input a b) x y
  | V.Float_array x, V.Float_array y | V.Vec2_array x, V.Vec2_array y
  | V.Vec3_array x, V.Vec3_array y | V.Vec4_array x, V.Vec4_array y -> same_floats x y
  | V.Int x, V.Int y -> x = y | V.Bool x, V.Bool y -> x = y
  | V.Float x, V.Float y -> Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | V.Vec2 (a, b), V.Vec2 (c, d) -> Int64.(equal (bits_of_float a) (bits_of_float c) && equal (bits_of_float b) (bits_of_float d))
  | V.Vec3 (a, b, c), V.Vec3 (d, e, f) ->
      Int64.(equal (bits_of_float a) (bits_of_float d) && equal (bits_of_float b) (bits_of_float e)
             && equal (bits_of_float c) (bits_of_float f))
  | _ -> false

(* ponytail: 4 entries per loop and no byte cap: the results are references to arrays the loop made
   anyway, at most 4 of them kept alive; add a byte cap if a loop's results grow past tens of MB *)
let remember (reads, writes) run =
  let table = Lru.Identity_list.create ~equal:(fun left right ->
    Array.length left = Array.length right && Array.for_all2 same_input left right) 4 in
  fun st ->
    let key = Array.map (fun n -> st.values.(n)) reads in
    match Lru.Identity_list.peek table key with
    | Some values -> Array.iteri (fun i n -> st.values.(n) <- values.(i)) writes
    | None ->
        run st;
        let values = Array.map (fun n -> st.values.(n)) writes in
        (* a region left on the GPU names a buffer of this draw's mark table, cleared by the next *)
        if not (Array.exists Core_marks.is_device values) then
        Lru.Identity_list.add table key values

let bind_arguments st packed args = Packed.Private.bind_arguments packed
  (Array.of_list (List.map (Array.get st.values) args))

let prepare_invoke packed ~args ~result ~on_device ~keep =
  let packed=if args=[] then Packed.Private.bind_arguments packed [||] else Ok packed in
  let kept=ref None in
  fun st ->
    if Array.length st.builders>0 then st.builders.(result)<-None;
    match !kept with
    | Some v when st.marks<>None -> st.values.(result)<-v
    | _ ->
    let fail = V.raise_diagnostic in
    let packed=match packed with Ok p -> p | Error d -> fail d in
    let bind ?validate packed = if args=[] then packed else match Packed.Private.bind_arguments ?validate packed
      (Array.of_list(List.map(fun n->st.values.(n))args)) with Ok p->p | Error d->fail d in
    let resident=match st.resident with
      | Some r when st.resident_on -> (match r.invoke (bind ~validate:false packed) ~live:st.live with
          | Some v -> st.values.(result)<-v; true
          | None -> List.iter (fun n -> st.values.(n) <- r.finish st.values.(n)) args; false)
      | _ -> false in
    if not resident then
    let packed=bind packed in
    let device=match st.device,st.marks with
      | Some device,Some sink when on_device ->
          (match device packed with
           | Ok (Some [|(token,count,width)|]) ->
               st.values.(result)<-Core_marks.device_value sink ~token ~count ~width; true
           | Ok (Some _) -> V.fail "E_CORE_DEVICE" "A single region needs one device output."
           | Ok None -> false
           | Error d -> fail d)
      | _ -> false in
    if not device then
      (match (match st.force with Some force -> force packed | None -> Packed.force packed ~live:st.live) with
       | Ok v -> st.values.(result)<-v; if keep && st.marks<>None then kept:=Some v
       | Error d -> fail d)

let grouped st = st.device<>None && st.marks<>None && not st.resident_on

let prepare_group (group : Core_draw.group) fallback =
  let results=Array.of_list group.results in
  let original st = Array.iter (fun (span,run) ->
    try run st with Diagnostic.Error d when d.span=None -> V.raise_diagnostic {d with span}) fallback in
  fun st -> if not (grouped st) then (snd fallback.(Array.length fallback-1)) st else
    let device=Option.get st.device and sink=Option.get st.marks in
    let offered=Result.bind (bind_arguments st group.region group.args) device in
    match offered with
    | Ok (Some outputs) when Array.length outputs=Array.length results &&
        Array.for_all (fun index -> let _, count, width=outputs.(index) in
          count>=0 && width=Array.length group.region.outputs.(index))
          (Array.init (Array.length outputs) Fun.id) ->
        Array.iteri (fun index (token,count,width) ->
          let result=results.(index) in
          if Array.length st.builders>0 then st.builders.(result)<-None;
          st.values.(result)<-Core_marks.device_value sink ~token ~count ~width) outputs
    | Ok _ | Error _ -> original st

let prepare ?(remember_loops=false) ?(approx=Flow.Workspace.Paths.empty) ?(fast=false) (program : C.program) =
  let track = builds program.body in
  let device_ok, groups = Core_draw.device_groups ~approx ~fast program ~width:(fun region ->
    match program.regions.(region).outputs with
    | [|output|] -> Some (Array.length output) | _ -> None)
    ~legal:(fun region -> Packed.gpu_refusals ~fused:true ~fast region = []) in
  let fused=Array.make (Array.length program.regions) None
  and individual=Array.make (Array.length program.regions) None in
  List.iter (fun (group : Core_draw.group) -> if List.length group.members>1 then
    List.iter (fun region -> fused.(region)<-Some group) group.members) groups;
  let static_ok = Core_draw.static_invokes program in
  let constants = Array.map C.value_of_literal program.constants in
  let expression : C.expr -> state -> E.value = function
    | C.Constant n -> let v=constants.(n) in fun _ -> v
    | Source value -> fun st ->
        (match E.Private.force_reference ?resolve:st.resolve value ~live:st.live with
         | Ok value -> value | Error d -> V.raise_diagnostic d)
    | Host_source {name;ty;args}->fun st->
        let args=List.map(fun(name,index)->name,st.values.(index))args in
        if name="$core/complete" && ty=Flow.Ty.geometry && args=["value",V.No_geo] then V.No_geo else
        let value=E.Source(name,ty,args) in
        (match E.Private.force_reference ?resolve:st.resolve value ~live:st.live with
         |Ok value->value|Error d->V.raise_diagnostic d)
    | Source_descriptor {name;ty;args}->fun st->
        let args=List.map(fun(name,index)->name,st.values.(index))args in
        let prefix="$core/constant-array/" in
        if String.starts_with ~prefix name then
          let operation=String.sub name(String.length prefix)(String.length name-String.length prefix) in
          let value=Flow.Op.Private.constant_array_descriptor operation args in
          if V.ty_of value<>ty then V.fail "E_ARRAY_TYPE" "Constant array descriptor has the wrong declared type.";
          value
        else E.Source(name,ty,args)
    | Local n -> fun st -> st.values.(n)
    | Call_index slot -> fun st -> let n=st.calls.(slot) in st.calls.(slot)<-n+1; V.Int n
    | Frame field ->
        let live = match field with
          | C.Dt -> Some "frame/dt" | Index -> Some "frame/index" | Width -> Some "frame/width"
          | Height -> Some "frame/height" | Pointer_x -> Some "pointer/x" | Pointer_y -> Some "pointer/y"
          | Input -> Some "frame/input" | Time | Tick -> None in
        (match field,live with
         | C.Time,_ -> fun st -> V.Float st.live.t
         | Dt,_ -> fun st -> V.Float st.live.dt | Index,_ -> fun st -> V.Int st.live.frame
         | Width,_ -> fun st -> V.Int (fst st.live.size) | Height,_ -> fun st -> V.Int (snd st.live.size)
         | Pointer_x,_ -> fun st -> V.Float (fst st.live.pointer) | Pointer_y,_ -> fun st -> V.Float (snd st.live.pointer)
         | Tick,_ -> fun st -> V.Int st.live.tick
         | _,Some name -> let op = lazy (operator name) in fun st -> apply_op (Lazy.force op) st []
         | _ -> assert false)
    | Prim (op,args) -> primitive ~track ~locals:program.locals op args
    | Vec ns -> fun st ->
        (match List.map (fun n -> V.num st.values.(n)) ns with
        | [x;y] -> V.Vec2 (x,y) | [x;y;z] -> V.Vec3 (x,y,z) | [x;y;z;w] -> V.Vec4 (x,y,z,w)
        | _ -> V.fail "E_CORE" "Invalid vector width.")
    | Fields xs -> fun st -> V.Record (List.map (fun (name,n) -> seal st n;name,st.values.(n)) xs)
    | Items ns -> fun st ->
        let xs=Array.of_list (List.map (fun n -> seal st n;st.values.(n)) ns) in
        if Array.length xs>4096 && not(List.for_all(fun n->program.locals.(n)=Flow.Ty.geometry)ns) then
          V.fail "E_ITER_BOUND" "Core list exceeds 4,096 values.";
        let ty=V.elem_ty xs in V.List (Array.map (V.coerce_to ty) xs)
    | Field (n,name) ->
        let slow st = match st.values.(n),name with
        | V.Vec2 (x,_),"x" | Vec2 (_,x),"y"
        | Vec3 (x,_,_),"x" | Vec3 (_,x,_),"y" | Vec3 (_,_,x),"z"
        | Vec4 (x,_,_,_),"x" | Vec4 (_,x,_,_),"y" | Vec4 (_,_,x,_),"z" | Vec4 (_,_,_,x),"w" -> V.Float x
        | Record xs,name -> (match List.find_opt (fun (n,_) -> String.equal n name) xs with Some (_,v) -> v | None -> V.fail "E_FIELD" "Missing core field.")
        | _ -> V.fail "E_FIELD" "Invalid core field." in
        (match name with
         | "x" -> fun st -> (match st.values.(n) with V.Vec2 (x,_) | Vec3 (x,_,_) | Vec4 (x,_,_,_) -> V.Float x | _ -> slow st)
         | "y" -> fun st -> (match st.values.(n) with V.Vec2 (_,x) | Vec3 (_,x,_) | Vec4 (_,x,_,_) -> V.Float x | _ -> slow st)
         | _ -> slow) in
  let slot_keys = Array.map (fun (s : C.state_slot) ->
    let path=s.prefix @ s.zone and host=s.plane=`Host in
    if List.for_all (function C.Static_index _ -> true | Local_index _ -> false) s.source.iter then
      let iter=List.map (function C.Static_index i -> i | Local_index _ -> assert false) s.source.iter in
      fun _ -> s.source.instance,path,iter,host
    else fun st -> s.source.instance,path,
      List.map (function C.Static_index i -> i | Local_index l -> V.int_of st.values.(l)) s.source.iter,host)
    program.state_slots in
  let slot_key st n = slot_keys.(n) st in
  let state_root st = match st.root with Some r -> r
    | None -> V.fail "E_CORE_STATE" "Core state needs its residual's state." in
  let writes = Array.make (Array.length program.locals) 0 in
  let bump n = writes.(n)<-writes.(n)+1 in
  let count statements = C.iter (fun (s : C.statement) -> match s.node with
    | Reset_call _ | Store _ | Draw _ -> ()
    | State_write {value; _} -> bump value
    | _ -> List.iter bump (C.writes s)) statements in
  count program.body;
  let hoisted (s : C.statement) = match s.node with
    | Assign (n,(Constant _ | Frame (Time|Dt|Index|Tick|Width|Height|Pointer_x|Pointer_y))) -> writes.(n)=1
    | _ -> false in
  let in_loop = ref false in
  (* Constants and frame facts a loop body sets once are set when the loop starts, not per iteration. *)
  let rec invariant statements = List.concat_map (fun (s : C.statement) -> match s.node with
    | Assign (n,e) when hoisted s -> [n, expression e]
    | If {yes;no;_} -> invariant yes @ invariant no
    | _ -> []) statements in
  let rec block statements =
    let statements = if !in_loop then List.filter (fun s -> not (hoisted s)) statements else statements in
    let top = not !in_loop in
    let run = Array.of_list (List.map (fun (s : C.statement) -> match s.node with
      | (Iterate _ | Counted _ | Invoke _) when top && remember_loops &&
          (match s.node with Invoke {region;_} -> fused.(region)=None | _ -> true) ->
          let run = statement s in
          let run = match footprint program s with
            | Some (_, _) when resident_loop program s ->
                let live = Option.get (resident_live program s) in
                fun st -> (match st.resident with
                | None -> run st
                | Some r ->
                    st.resident_on <- true;
                    Fun.protect ~finally:(fun () -> st.resident_on <- false; r.close ()) (fun () ->
                      run st;
                      (* the loop's arrays that later statements read come back once, as it ends *)
                      Array.iter (fun n -> st.values.(n) <- r.finish st.values.(n)) live))
            | _ -> run in
          (match footprint program s with Some f -> remember f run | None -> run)
      | _ -> statement s) statements)
    and spans = Array.of_list (List.map (fun (s : C.statement) -> s.source.span) statements) in
    (* The innermost block that sees an unlocated failure gives it its statement's span. *)
    fun st -> let i=ref 0 in
      try while !i<Array.length run do run.(!i) st; incr i done
      with Diagnostic.Error d when d.span=None -> V.raise_diagnostic {d with span=spans.(!i)}
  and loop_body body =
    let inner = !in_loop in in_loop := true;
    let run = block body in in_loop := inner;
    let setup = Array.of_list (invariant body) in
    (fun st -> Array.iter (fun (n,value) -> st.values.(n)<-value st) setup), run
  and plain_block statements =
    let inner = !in_loop in in_loop := false;
    let run = block statements in in_loop := inner; run
  and statement (s : C.statement) =
    (match s.node with
    | Assign (n,(Prim ((Array_compact | Coerce _ as op),[_,arg]) as e)) when (match op with
        | C.Coerce (Flow.Ty.Array _) | Array_compact -> true | _ -> false) ->
        (* in a resident fold the device compacts and retypes its own arrays; the host otherwise *)
        let value=expression e in
        fun st -> st.values.(n) <- (match st.resident with
          | Some r when st.resident_on ->
              (match (match op with C.Array_compact -> r.compact st.values.(arg)
                      | C.Coerce ty -> r.coerce ty st.values.(arg) | _ -> None) with
               | Some v -> v
               | None -> st.values.(arg) <- r.finish st.values.(arg); value st)
          | _ -> value st)
    | Assign (n,(Prim (Array_count,[_,arg]) as e)) ->
        (* a resident array's length is a capacity: its count is the host array's *)
        let value=expression e in
        fun st -> (match st.resident with
          | Some r when st.resident_on -> st.values.(arg) <- r.finish st.values.(arg)
          | _ -> ());
          st.values.(n) <- value st
    | Assign (n,e) ->
        let value=expression e in
        let ownership = match e with Prim (Sequence_build,_) -> `Fresh | Local old -> `Alias old | _ -> `None in
        if not track || plain program.locals.(n) then fun st -> st.values.(n)<-value st else
        fun st ->
          let v=value st in
          (match ownership with
           | `Fresh -> if Array.length st.builders=0 then st.builders<-Array.make (Array.length program.locals) None;
               st.builders.(n)<-Some (ref true)
           | `Alias old -> if Array.length st.builders>0 then st.builders.(n)<-st.builders.(old)
           | `None -> if Array.length st.builders>0 then st.builders.(n)<-None);
          st.values.(n)<-v
    | Reset_call slot -> fun st -> st.calls.(slot)<-0
    | Draw d ->
        let run = Core_marks.compile d in
        fun st -> (match st.marks with
          | Some sink -> run st.values sink
          | None -> V.fail "E_CORE" "Drawing statements need a mark table.")
    | Diagnose_rows{rows;policy;index;handler;diagnostic;result}->
        let handler=plain_block handler in
        fun st->st.values.(result)<-V.Private.diagnose_rows st.values.(rows)
          ~select:(fun rows first->match st.resolve,policy with
            |None,Flow.Workspace.First->first
            |None,Flow.Workspace.Range_last->V.fail "E_DIAGNOSTIC_CONTEXT" "Range-last diagnosis needs its native host."
            |Some resolve,policy->
                let policy=match policy with Flow.Workspace.First->"first"|Range_last->"range-last" in
                (match resolve(V.Source("$core/diagnostic_index",Flow.Ty.Int,["rows",rows;"policy",V.Text policy]))with
                  |Ok(V.Int index)->index|Error error->V.raise_diagnostic error
                  |_->V.fail "E_DIAGNOSTIC_CONTEXT" "Host diagnosis returns one integer row."))
          ~map:(fun value->st.values.(index)<-value;handler st;seal st diagnostic;st.values.(diagnostic))
    | Protect {body;value;failure;handler;mapped;result}->
        let body=plain_block body and handler=plain_block handler in
        fun st->st.values.(result)<-V.Private.with_error
          ~protected:(fun()->body st;seal st value;st.values.(value))
          ~map:(fun diagnostic->st.values.(failure)<-diagnostic;handler st;seal st mapped;st.values.(mapped))
    | If {condition;yes;no} ->
        let yes=block yes and no=block no in
        fun st -> (if V.truthy st.values.(condition) then yes else no) st
    | Iterate {sequence;index;item;limit;body} ->
        let setup,body=loop_body body in
        fun st ->
          let source=st.values.(sequence) in
          let count=match source with V.List xs -> Array.length xs | source -> V.array_length source in
          if count>limit then V.fail "E_ITER_BOUND" "Core iteration exceeds its bound.";
          setup st;
          for i=0 to count-1 do
            st.values.(index)<-V.Int i;
            st.values.(item)<-(match source with V.List xs -> xs.(i) | source -> V.array_get source i);
            body st
          done
    | Counted {first;last;count;index;item;limit;stop;init;body} ->
        let init=plain_block init and setup,body=loop_body body in
        fun st ->
          let first=V.int_of st.values.(first) and last=V.int_of st.values.(last) in
          let length=Int64.sub(Int64.of_int last)(Int64.of_int first)in
          if length>Int64.of_int limit then V.fail "E_ITER_BOUND" "Core iteration exceeds its bound.";
          let length=if length<=0L then 0 else Int64.to_int length in
          st.values.(count)<-V.Int length;
          init st;
          setup st;
          (match stop with
           |None->for i=0 to length-1 do
               st.values.(index)<-V.Int i;st.values.(item)<-V.Int(first+i);body st
             done
           |Some stop->let i=ref 0 in
               while !i<length && not(V.truthy st.values.(stop))do
                 st.values.(index)<-V.Int !i;st.values.(item)<-V.Int(first+ !i);body st;incr i
               done)
    | Store {sequence;index;value} ->
        fun st ->
          if not (Option.fold ~none:false ~some:(fun writable -> !writable) (if Array.length st.builders>0 then st.builders.(sequence) else None)) then
            V.fail "E_CORE_OWNERSHIP" "Core stores require an unpublished local builder.";
          let i=V.int_of st.values.(index) in
          (match st.values.(sequence) with
           | V.Text_array data ->
               if i<0 || i>=Array.length data then V.fail "E_LIST_RANGE" "Core store is outside its sequence.";
               (match st.values.(value) with V.Text text -> data.(i)<-text
                | _ -> V.fail "E_ARRAY_TYPE" "Text arrays contain text values.")
           | V.Int_array data ->
               if i<0 || i>=Array.length data then V.fail "E_LIST_RANGE" "Core store is outside its sequence.";
               V.int_array_put data i st.values.(value)
           | V.Float_array data | Vec2_array data | Vec3_array data | Vec4_array data as packed ->
               if i<0 || i>=V.array_length packed then V.fail "E_LIST_RANGE" "Core store is outside its sequence.";
               V.array_put data (Array.length data/V.array_length packed) i st.values.(value)
           | _ ->
               let xs=V.list_arg st.values.(sequence) in
               if i<0 || i>=Array.length xs then V.fail "E_LIST_RANGE" "Core store is outside its sequence.";
               seal st value;
               xs.(i)<-st.values.(value))
    | State_read {slot;result;init;initial} ->
        let init=plain_block init in
        fun st ->
          let instance,path,iter,host=slot_key st slot in
          if Array.length st.builders>0 then st.builders.(result)<-None;
          st.values.(result)<-E.Residual.state_read (state_root st) ~instance ~path ~iter ~host
            ~init:(fun () -> init st; st.values.(initial))
    | State_peek {slot;found;value} ->
        fun st ->
          let instance,path,iter,host=slot_key st slot in
          let pending=E.Residual.state_peek (state_root st) ~instance ~path ~iter ~host in
          st.values.(found)<-V.Bool (Option.is_some pending);
          st.values.(value)<-Option.value ~default:V.No_geo pending
    | State_write {slot;value} ->
        fun st ->
          let instance,path,iter,host=slot_key st slot in
          seal st value;
          st.values.(value)<-E.Residual.state_write (state_root st) ~instance ~path ~iter ~host st.values.(value)
    | Invoke {region;args;result} ->
        let run=prepare_invoke program.regions.(region) ~args ~result
          ~on_device:device_ok.(region) ~keep:static_ok.(result) in
        individual.(region)<-Some (s.source.span,run);
        (match fused.(region) with
         | None -> run
         | Some group when List.hd (List.rev group.members)<>region ->
             fun st -> if not (grouped st) then run st
         | Some group -> prepare_group group
             (Array.of_list (List.map (fun region -> Option.get individual.(region)) group.members)))) in
  {program; body=block program.body;
   pool=Atomic.make None; stateless=Array.length program.state_slots=0 && Array.length program.regions=0}

let has_regions prepared = Array.length prepared.program.regions>0

let general root force resolve prepared live =
  let program=prepared.program in
  match Input.validate live with
    | Error message -> Error (Diagnostic.error ~code:"E_FRAME" message)
    | Ok () ->
        let n=Array.length program.locals in
        let pooled=n>256 in
        let values=match (if pooled then Atomic.exchange prepared.pool None else None) with
          | Some values -> values | None -> Array.make n E.No_geo in
        let release () = if pooled then Atomic.set prepared.pool (Some values) in
        let st = {values;
          builders=[||]; calls=Array.make program.call_slots 0;
          live; root; force; resolve; marks=None; device=None; resident=None; resident_on=false} in
        let result = try prepared.body st;
          if Array.length program.results<>1 then V.fail "E_CORE" "Expected one core result.";
          Ok st.values.(program.results.(0))
        with Diagnostic.Error d -> Error d
          | Not_found | Invalid_argument _ | Failure _ -> Error (Diagnostic.error ~code:"E_CORE" "Invalid core program.") in
        release (); result

let attempt ?root ?force ?resolve prepared ~live = general root force resolve prepared live

let fast prepared =
  let needs_resolver = ref false in
  C.iter (fun (statement : C.statement) -> match statement.node with
    | Assign (_, (Source _ | Host_source _ | Source_descriptor _)) -> needs_resolver := true
    | _ -> ()) prepared.program.body;
  if prepared.stateless && not !needs_resolver then Some (fun live -> attempt prepared ~live) else None

(* State needs its residual even in strict mode; strict failures restore it without
   entering the tree walker. Ordinary failures retain the evaluator's fallback. *)
let execute ?root ?(strict=false) ?(rerun_on_error=true) ?on_walk ?force ?resolve prepared ~live = match root with
  | None -> attempt ?force ?resolve prepared ~live
  | Some r ->
      let restore = if prepared.stateless then ignore else E.Residual.checkpoint r in
      (match (try attempt ~root:r ?force ?resolve prepared ~live with
       | Diagnostic.Error _ | Invalid_argument _ | Not_found | Failure _ ->
         Error (Diagnostic.error ~code:"E_CORE" "Core execution failed.")) with
       | Ok _ as ok -> ok
       | Error _ as error when strict || resolve<>None || not rerun_on_error -> restore (); error
       | Error _ -> restore (); E.Residual.rerun ?on_walk r)

let run ?root ?strict ?force ?resolve program ~live = execute ?root ?strict ?force ?resolve (prepare program) ~live

module Buffers = Core_marks.Buffers

(* A draw program (Core_draw.lower) fills a mark table. A failure is the core's own
   diagnostic: there is no walker for a drawing, so nothing is rerun. *)
let draw ?root ?force ?resolve ?device ?resident ?image prepared ~live ~size ~buffers =
  match Input.validate live with
  | Error message -> Error (Diagnostic.error ~code:"E_FRAME" message)
  | Ok () ->
      let program=prepared.program in
      let sink=Core_marks.sink ?image:(Option.map (fun resolve value ->
        match resolve value with
        | Ok resource -> resource
        | Error (d : Diagnostic.t) -> V.raise_diagnostic d) image) ~size buffers in
      let st = {values=Array.make (Array.length program.locals) E.No_geo;
        builders=[||]; calls=Array.make program.call_slots 0; live; root; force; resolve; marks=Some sink; device;
        resident; resident_on=false} in
      (try prepared.body st; Ok (Core_marks.finish sink)
       with Diagnostic.Error d -> Error d
         | Not_found | Invalid_argument _ | Failure _ -> Error (Diagnostic.error ~code:"E_CORE" "Invalid core program."))
