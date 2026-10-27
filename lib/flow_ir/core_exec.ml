module C = Core
module V = Flow.Value
module E = Flow.Eval

(* A resident fold's device: inside a qualifying top-level loop of a drawing, a region's array may stay
   on the GPU, standing in the locals as a placeholder (zeros of its capacity) the device knows. *)
type resident = {
  invoke : Packed.t -> live:Frame_input.t -> E.value option;  (* run on the device; None leaves it to the CPU *)
  compact : E.value -> E.value option;  (* array/compact of a placeholder mask, on the device *)
  coerce : Flow.Ty.t -> E.value -> E.value option;  (* a placeholder retyped, on the device *)
  finish : E.value -> E.value;  (* the host array a placeholder stands for; any other value as it is *)
  close : unit -> unit;  (* the loop is over: forget its placeholders *)
}

type state = {
  values : E.value array; mutable builders : bool ref option array;  (* allocated by the first builder, so a call that builds nothing pays nothing *) calls : int array;
  live : Frame_input.t; root : E.residual option;
  force : (Packed.t -> (E.value, Flow.Diagnostic.t) result) option;
  resolve : (E.value -> (E.value, Flow.Diagnostic.t) result) option;
  marks : Core_marks.sink option;  (* the table the draw statements of a draw program fill *)
  device : (Packed.t -> ((int * int * int) option, Flow.Diagnostic.t) result) option;
  resident : resident option; mutable resident_on : bool;
}
(* The top-level Invoke statements of a draw program whose result cannot change between frames:
   the region does not depend on the frame, its arguments are locals written once from constants
   and such results, and nothing else writes the result. Their values are computed once. *)
let static_invokes (program : C.program) =
  let n = Array.length program.locals in
  let writes = Array.make n 0 and nested = Array.make n false in
  let rec walk top (s : C.statement) =
    let w a = writes.(a) <- writes.(a) + 1; if not top then nested.(a) <- true in
    match s.node with
    | Assign (a, _) | Invoke {result = a; _} | Store {sequence = a; _} | State_write {value = a; _} -> w a
    | Diagnose_rows{index;result;handler;_}->w index;w result;List.iter(walk false)handler
    | Protect {body;handler;failure;result;_}->w failure;w result;List.iter(walk false)body;List.iter(walk false)handler
    | If {yes; no; _} -> List.iter (walk false) yes; List.iter (walk false) no
    | Iterate {index; item; body; _} -> w index; w item; List.iter (walk false) body
    | Counted {count; index; item; init; body; _} ->
        List.iter w [count; index; item]; List.iter (walk false) init; List.iter (walk false) body
    | State_read {result; init; _} -> w result; List.iter (walk false) init
    | State_peek {found; value; _} -> w found; w value
    | Draw _ | Reset_call _ -> () in
  List.iter (walk true) program.body;
  let static = Array.make n false in
  let once a = writes.(a) = 1 && not nested.(a) in
  let constant : C.expr -> bool = function
    | Constant _ -> true
    | Local a | Field (a, _) -> static.(a)
    | Prim (op, args) -> op <> Rand && List.for_all (fun (_, a) -> static.(a)) args
    | Vec ns | Items ns -> List.for_all (fun a -> static.(a)) ns
    | Fields fs -> List.for_all (fun (_, a) -> static.(a)) fs
    | Frame _ | Call_index _ | Source _ | Host_source _ | Source_descriptor _ -> false in
  let region_static (r : C.region) =
    not (E.frame_dependent (E.Residual r.residual)) && not (E.state_dependent (E.Residual r.residual))
    && Array.for_all (fun (source, _) -> not (E.frame_dependent (E.Residual source))) r.sources in
  List.iter (fun (s : C.statement) -> match s.node with
    | Assign (a, e) when once a && constant e -> static.(a) <- true
    | Invoke {region; args; result} when once result && List.for_all (fun a -> static.(a)) args
                                         && region_static program.regions.(region) -> static.(result) <- true
    | _ -> ()) program.body;
  let invokes = Array.make n false in
  List.iter (fun (s : C.statement) -> match s.node with
    | Invoke {result; _} -> invokes.(result) <- static.(result) | _ -> ()) program.body;
  invokes

(* A region whose every result is read only as the positions, sizes, radius, from, to or a colour of a
   mark set may run on the GPU instead; the others are read by the frame program. *)
let device_regions (program : C.program) =
  let n = Array.length program.locals in
  let eligible = Array.make n false and other = Array.make n false in
  let use l = other.(l) <- true in
  let expression : C.expr -> unit = function
    | Constant _ | Frame _ | Call_index _ | Source _ -> ()
    | Local a | Field (a, _) -> use a
    | Prim (_, args) | Fields args | Host_source {args;_} | Source_descriptor {args;_} -> List.iter (fun (_, a) -> use a) args
    | Vec ns | Items ns -> List.iter use ns in
  let draw : C.draw -> unit = function
    | Batch {args; _} -> List.iter (fun (name, a) ->
        if List.mem name ["positions"; "from"; "to"; "sizes"; "radius"; "fill"; "stroke"; "color"] then eligible.(a) <- true else use a) args
    | Background a | Push (_, a) -> use a
    | Point {at; ink} -> use at; use ink
    | Mark {args; _} | Path {args; _} -> List.iter (fun (_, a) -> use a) args
    | Points {positions; ink} -> use positions; use ink
    | Text_run {at; text; ink; size} -> List.iter use [at; text; ink; size]
    | Image {at; scale; angle; _} -> List.iter use [at; scale; angle]
    | Pop -> () in
  let rec statement (s : C.statement) = match s.node with
    | Draw d -> draw d
    | Assign (_, e) -> expression e
    | Reset_call _ | State_peek _ -> ()
    | Protect {body;value;handler;mapped;_}->use value;use mapped;List.iter statement body;List.iter statement handler
    | Diagnose_rows{rows;diagnostic;handler;_}->use rows;use diagnostic;List.iter statement handler
    | If {condition; yes; no} -> use condition; List.iter statement yes; List.iter statement no
    | Iterate {sequence; body; _} -> use sequence; List.iter statement body
    | Counted {first; last; init; body; _} -> use first; use last; List.iter statement init; List.iter statement body
    | Store {sequence; value; _} -> use sequence; use value
    | State_read {init; initial; _} -> use initial; List.iter statement init
    | State_write {value; _} -> use value
    | Invoke {args; _} -> List.iter use args in
  List.iter statement program.body;
  Array.iter use program.results;
  let ok = Array.make (Array.length program.regions) true in
  let rec regions (s : C.statement) = match s.node with
    | Invoke {region; result; _} -> if not eligible.(result) || other.(result) then ok.(region) <- false
    | Protect {body;handler;_}->List.iter regions body;List.iter regions handler
    | Diagnose_rows{handler;_}->List.iter regions handler
    | If {yes; no; _} -> List.iter regions yes; List.iter regions no
    | Iterate {body; _} -> List.iter regions body
    | Counted {init; body; _} -> List.iter regions init; List.iter regions body
    | State_read {init; _} -> List.iter regions init
    | _ -> () in
  List.iter regions program.body;
  ok
(* A straight-line program of Float arithmetic on the frame time is one tree of unboxed
   operations. It only answers when every step is a finite Float; any other outcome
   (a non-finite step, an invalid frame) runs the general statements, which raise the
   operators' own diagnostics. *)
type tree = Const of float | Time | Add of tree * tree | Sub of tree * tree | Mul of tree * tree
  | Sin of tree | Cos of tree | Sqrt of tree
exception Non_finite
(* Postfix code on a flat float stack: no boxed intermediate floats. *)
type op = Push of float | Push_time | Add_op | Sub_op | Mul_op | Sin_op | Cos_op | Sqrt_op
  | Add_k of float | Sub_k of float | Mul_k of float | Add_t | Sub_t | Mul_t  (* right operand inline *)
let rec flatten tree rest = match tree with
  | Const c -> Push c :: rest | Time -> Push_time :: rest
  | Add (a,b) -> binary a b Add_op (fun c -> Add_k c) Add_t rest
  | Sub (a,b) -> binary a b Sub_op (fun c -> Sub_k c) Sub_t rest
  | Mul (a,b) -> binary a b Mul_op (fun c -> Mul_k c) Mul_t rest
  | Sin a -> flatten a (Sin_op :: rest) | Cos a -> flatten a (Cos_op :: rest)
  | Sqrt a -> flatten a (Sqrt_op :: rest)
and binary a b op konst time rest = match b with
  | Const c -> flatten a (konst c :: rest) | Time -> flatten a (time :: rest)
  | _ -> flatten a (flatten b (op :: rest))
let depth code = snd (List.fold_left (fun (d,m) op -> let d = match op with
  | Push _ | Push_time -> d+1 | Add_op | Sub_op | Mul_op -> d-1 | _ -> d in d, max m d) (0,0) code)
let code_value (code,size) t =
  let stack=Array.create_float size and top=ref 0 in
  for i=0 to Array.length code-1 do
    (match Array.unsafe_get code i with
     | Push c -> Array.unsafe_set stack !top c; incr top
     | Push_time -> Array.unsafe_set stack !top t; incr top
     | Sin_op -> let r=sin (Array.unsafe_get stack (!top-1)) in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Cos_op -> let r=cos (Array.unsafe_get stack (!top-1)) in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Sqrt_op -> Array.unsafe_set stack (!top-1) (sqrt (Float.abs (Array.unsafe_get stack (!top-1))))
     | Add_op -> decr top; let r=Array.unsafe_get stack (!top-1) +. Array.unsafe_get stack !top in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Sub_op -> decr top; let r=Array.unsafe_get stack (!top-1) -. Array.unsafe_get stack !top in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Mul_op -> decr top; let r=Array.unsafe_get stack (!top-1) *. Array.unsafe_get stack !top in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Add_k c -> let r=Array.unsafe_get stack (!top-1) +. c in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Sub_k c -> let r=Array.unsafe_get stack (!top-1) -. c in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Mul_k c -> let r=Array.unsafe_get stack (!top-1) *. c in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Add_t -> let r=Array.unsafe_get stack (!top-1) +. t in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Sub_t -> let r=Array.unsafe_get stack (!top-1) -. t in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r
     | Mul_t -> let r=Array.unsafe_get stack (!top-1) *. t in
         if not (Float.is_finite r) then raise Non_finite; Array.unsafe_set stack (!top-1) r)
  done;
  Array.unsafe_get stack 0

type prepared = { program : C.program; body : state -> unit;
  code : (op array * int) option;
  pool : E.value array option Atomic.t;  (* the locals of a large program, reused across calls: a fresh one is allocated on the major heap *)
  stateless : bool  (* no state slot and no region: a failure leaves the evaluation state untouched *) }

let yes = V.Bool true and no = V.Bool false
let seal st n = if Array.length st.builders>0 then Option.iter (fun writable -> writable:=false) st.builders.(n)

(* Resolved once per prepared program, not per executed statement. *)
let operator name = match Flow.Op.find ~extra:Operators.all name Flow.Context.value with
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
              V.raise_diagnostic(Flow.Diagnostic.error ?cause ~code message)
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
          ~rows:(fun ~start ~finish f -> Rays_math.Parallel.for_ ~start ~finish f) operation
          ~offsets:st.values.(offsets) st.values.(values)
    | Array_segment_argmax ->
        let values=a_ "values" and offsets=a_ "offsets" in
        fun st -> Flow.Op.Private.segment_argmax
          ~rows:(fun ~start ~finish f -> Rays_math.Parallel.for_ ~start ~finish f)
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
          let c=Operators.drawing_color st.values.(first) in
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

(* Each local is a tree and whether it is a Float: Int constants only meet a Float operand,
   where the operators convert them exactly as the general path does. *)
let scalar_tree (program : C.program) constants =
  if Array.length program.state_slots>0 || Array.length program.regions>0 || Array.length program.results<>1 then None else
  let locals=Array.make (Array.length program.locals) None in
  let operand = function Some v -> v | None -> raise Exit in
  try
    List.iter (fun (s : C.statement) -> match s.node with
      | Assign (n,e) ->
          locals.(n)<-Some (match e with
            | C.Constant k -> (match constants.(k) with V.Float f -> Const f,true | V.Int i -> Const (float_of_int i),false
                | _ -> raise Exit)
            | Frame Time -> Time,true
            | Local m -> operand locals.(m)
            | Prim (Numeric (Add|Sub|Mul as op),["a",a;"b",b]) ->
                let (x,fx),(y,fy)=operand locals.(a),operand locals.(b) in
                if not (fx || fy) then raise Exit;
                (match op with Add -> Add (x,y) | Sub -> Sub (x,y) | _ -> Mul (x,y)),true
            | Prim (Math (Sin|Cos|Sqrt as m),["x",x]) ->
                let x,_=operand locals.(x) in (match m with Sin -> Sin x | Cos -> Cos x | _ -> Sqrt x),true
            | _ -> raise Exit)
      | _ -> raise Exit) program.body;
    (match locals.(program.results.(0)) with
     | Some (t,true) -> let code=flatten t [] in if List.length code<=64 then Some (Array.of_list code, depth code) else None
     | _ -> None)
  with Exit -> None

let rec builds (statements : C.statement list) = List.exists (fun (s : C.statement) -> match s.node with
  | Assign (_,Prim (Sequence_build,_)) -> true
  | Protect {body;handler;_}->builds body || builds handler
  | Diagnose_rows{handler;_}->builds handler
  | If {yes;no;_} -> builds yes || builds no
  | Iterate {body;_} -> builds body
  | Counted {init;body;_} -> builds init || builds body
  | State_read {init;_} -> builds init
  | _ -> false) statements

(* A pure packed map lowered inside a state step is a Counted body rather than an
   Invoke. Compile its numeric statements to blocks here, leaving the shared IR alone.
   Unsupported layouts, escaping loop locals and observable effects keep the general
   executor. A failing block also reruns that executor, preserving lane/statement error order. *)
let counted_kernel (program : C.program) loop_id index item body =
  let block_size=1024 and n=Array.length program.locals in
  let written=Array.make n false and escaped=Array.make n false in
  let use n=escaped.(n)<-true in
  let expr use : C.expr -> unit = function
    | Constant _ | Frame _ | Call_index _ | Source _ -> ()
    | Local n | Field (n,_) -> use n
    | Prim (_,args) | Fields args | Host_source {args;_} | Source_descriptor {args;_} -> List.iter (fun (_,n) -> use n) args
    | Vec ns | Items ns -> List.iter use ns in
  let rec outside (s : C.statement) = if s.id<>loop_id then match s.node with
    | Assign (_,e) -> expr use e
    | Protect {body;value;handler;mapped;_}->use value;use mapped;List.iter outside body;List.iter outside handler
    | Diagnose_rows{rows;diagnostic;handler;_}->use rows;use diagnostic;List.iter outside handler
    | If {condition;yes;no} -> use condition;List.iter outside yes;List.iter outside no
    | Counted {first;last;init;body;_} -> use first;use last;List.iter outside init;List.iter outside body
    | Iterate {sequence;body;_} -> use sequence;List.iter outside body
    | Store {sequence;index;value} -> List.iter use [sequence;index;value]
    | Invoke {args;_} -> List.iter use args
    | State_read {initial;init;_} -> use initial;List.iter outside init
    | State_write {value;_} -> use value
    | State_peek _ | Reset_call _ -> ()
    | Draw d -> (match d with
        | Background a -> use a | Point {at;ink} -> use at;use ink
        | Mark {args;_} | Batch {args;_} | Path {args;_} -> List.iter (fun (_,n) -> use n) args
        | Points {positions;ink} -> use positions;use ink
        | Text_run {at;text;ink;size} -> List.iter use [at;text;ink;size]
        | Image {at;scale;angle;_} -> List.iter use [at;scale;angle]
        | Push (_,a) -> use a | Pop -> ()) in
  let rec writes (s : C.statement) = match s.node with
    | Assign (n,_) -> written.(n)<-true
    | If {yes;no;_} -> List.iter writes yes;List.iter writes no
    | Store _ -> () | _ -> raise Exit in
  try
    List.iter writes body;List.iter outside program.body;Array.iter use program.results;
    Array.iter (fun (slot : C.state_slot) -> List.iter (function
      | C.Local_index n -> use n | Static_index _ -> ()) slot.source.iter) program.state_slots;
    if Array.exists Fun.id (Array.mapi (fun n w -> w && escaped.(n)) written) then raise Exit;
    (* Every read of a loop-local value must be dominated by its write in the same
       lane. A scan, accumulator or branch-carried value keeps sequential execution. *)
    let rec defined available statements = List.iter (fun (s : C.statement) ->
      let read n=if not available.(n) then raise Exit in
      match s.node with
      | Assign (n,e) -> expr read e;available.(n)<-true
      | If {condition;yes;no} ->
          read condition;
          let y=Array.copy available and n=Array.copy available in
          defined y yes;defined n no;
          Array.iteri (fun i yes -> available.(i)<-yes && n.(i)) y
      | Store {sequence;index;value} -> List.iter read [sequence;index;value]
      | _ -> raise Exit) statements in
    defined (Array.map not written) body;
    let offsets=Array.make n (-1) and registers=ref 0 and inputs=ref [] and stores=ref []
    and captures=ref [] and masks=ref 0 in
    let width n=match Flow.Packed_ops.scalar_width program.locals.(n) with Some w -> w | None -> raise Exit in
    let reg n =
      if n=index || n=item then raise Exit;
      let w=width n in
      if offsets.(n)<0 then begin
        offsets.(n)<- !registers;registers:= !registers+w;
        if not written.(n) && n<>index && n<>item then captures:=n::!captures
      end; offsets.(n)*block_size,w in
    let at n component = let offset,w=reg n in
      if component>=w && w<>1 then raise Exit;offset+(if w=1 then 0 else component*block_size) in
    let scalar n=if width n<>1 then raise Exit else at n 0 in
    let add_array list n = if not (List.mem n !list) then list:=n::!list;
      match Flow.Packed_ops.array_width program.locals.(n) with Some w -> w | None -> raise Exit in
    let array_index n = if n<>item && n<>index then raise Exit in
    (* [work] holds scratch, resolved packed buffers, branch masks and private call counters. *)
    let rec compile statements =
      let code=Array.of_list (List.map instruction statements) in
      fun work first lanes count -> Array.iter (fun run -> run work first lanes count) code
    and instruction (s : C.statement) = match s.node with
      | Assign (dst,e) ->
          let d,w=reg dst in
          (match e with
           | Constant k ->
               let components=match program.constants.(k) with
                 | C.Int n when abs_float (float n)<9007199254740992. -> [|float n|]
                 | Float f -> [|f|] | Bool b -> [|if b then 1. else 0.|] | Vector v -> v
                 | _ -> raise Exit in
               if Array.length components<>w then raise Exit;
               fun (scratch,_,_,_) _ lanes count ->
                 for c=0 to w-1 do for j=0 to count-1 do scratch.(d+c*block_size+lanes.(j))<-components.(c) done done
           | Frame field ->
               let integer n=if abs_float (float n)>=9007199254740992. then raise Exit else float n in
               let get = match field with
                 | C.Time -> fun live -> live.Frame_input.t | Dt -> fun live -> live.Frame_input.dt
                 | Index -> fun live -> integer live.Frame_input.frame | Tick -> fun live -> integer live.Frame_input.tick
                 | Width -> fun live -> integer (fst live.Frame_input.size) | Height -> fun live -> integer (snd live.Frame_input.size)
                 | Pointer_x -> fun live -> fst live.Frame_input.pointer | Pointer_y -> fun live -> snd live.Frame_input.pointer
                 | Input -> raise Exit in
               fun (scratch,_,_,(_,live,_)) _ lanes count ->
                 let value=get live in for j=0 to count-1 do scratch.(d+lanes.(j))<-value done
           | Call_index slot ->
               (* A live call index used by the numeric body would change its integer semantics. *)
               if List.exists (fun (s : C.statement) -> match s.node with
                 | Assign (_,Call_index _) -> false | _ ->
                     let saved=escaped.(dst) in escaped.(dst)<-false;outside s;
                     let read=escaped.(dst) in escaped.(dst)<-saved;read) body then raise Exit;
               fun (_,_,_,(calls,_,_)) _ _ count -> calls.(slot)<-calls.(slot)+count
           | Local src | Prim ((Coerce (Float|Vec2|Vec3|Vec4)|Float_convert),[_,src]) ->
               let offsets=Array.init w (at src) in
               (match e with
                | Local _ when program.locals.(src)<>program.locals.(dst) -> raise Exit
                | Prim (Float_convert,_) when w<>1 || width src<>1 -> raise Exit
                | Prim (Coerce want,_) when want<>program.locals.(dst) ||
                    (w=1 && width src<>1) ||
                    (w>1 && program.locals.(src)<>want &&
                      not (List.mem program.locals.(src) [Flow.Ty.Float;Int])) -> raise Exit
                | _ -> ());
               fun (scratch,_,_,_) _ lanes count ->
                 for c=0 to w-1 do for j=0 to count-1 do let i=lanes.(j) in scratch.(d+c*block_size+i)<-scratch.(offsets.(c)+i) done done
           | Field (src,name) ->
               let c=match name with "x"->0 | "y"->1 | "z"->2 | "w"->3 | _->raise Exit in
               if width src<=c || width src=1 then raise Exit;
               let a=at src c in
               fun (scratch,_,_,_) _ lanes count -> for j=0 to count-1 do let i=lanes.(j) in scratch.(d+i)<-scratch.(a+i) done
           | Vec ns ->
               if List.length ns<>w then raise Exit;
               let src=Array.of_list (List.map scalar ns) in
               fun (scratch,_,_,_) _ lanes count ->
                 for c=0 to w-1 do for j=0 to count-1 do let i=lanes.(j) in scratch.(d+c*block_size+i)<-scratch.(src.(c)+i) done done
           | Prim (Array_nth,["array",array;"index",idx]) ->
               let input_width=add_array inputs array in array_index idx;
               if input_width<>w then raise Exit;
               fun (scratch,arrays,_,(_,_,read)) first lanes count ->
                 let data=arrays.(array) in
                 if count>0 then read.(array)<-true;
                 for c=0 to w-1 do for j=0 to count-1 do let i=lanes.(j) in
                   scratch.(d+c*block_size+i)<-data.((first+i)*w+c) done done
           | Prim (Numeric op,["a",a;"b",b]) ->
               let comparison=match op with Lt|Le|Gt|Ge|Eq|And|Or -> true | _ -> false in
               if comparison && (w<>1 || width a<>1 || width b<>1) then raise Exit;
               if not comparison && w<>max (width a) (width b) then raise Exit;
               if not comparison && program.locals.(a)=Flow.Ty.Int && program.locals.(b)=Flow.Ty.Int then raise Exit;
               let aa=Array.init w (at a) and bb=Array.init w (at b) in
               fun (scratch,_,_,_) _ lanes count ->
                 for c=0 to w-1 do for j=0 to count-1 do let i=lanes.(j) in
                   let x=scratch.(aa.(c)+i) and y=scratch.(bb.(c)+i) in
                   let value=match op with
                     | Add->x+.y | Sub->x-.y | Mul->x*.y | Div->if y=0. then 0. else x/.y
                     | Mod->if y=0. then 0. else Float.rem (Float.rem x y+.y) y
                     | Pow->Float.pow (Float.abs x) y | Min->Float.min x y | Max->Float.max x y
                     | Lt->if x<y then 1. else 0. | Le->if x<=y then 1. else 0.
                     | Gt->if x>y then 1. else 0. | Ge->if x>=y then 1. else 0.
                     | Eq->if x=y then 1. else 0. | And->if x<>0. && y<>0. then 1. else 0.
                     | Or->if x<>0. || y<>0. then 1. else 0.
                     | Atan2->Float.atan2 x y | Hypot->Float.hypot x y in
                   if not (Float.is_finite value) then raise Non_finite;
                   scratch.(d+c*block_size+i)<-value done done
           | Prim (Math op,[_,a]) ->
               if w<>1 || width a<>1 then raise Exit;let a=scalar a in
               fun (scratch,_,_,_) _ lanes count -> for j=0 to count-1 do let i=lanes.(j) in
                 let x=scratch.(a+i) in let value=match op with
                   | Sin->sin x | Cos->cos x | Sqrt->sqrt (Float.abs x) | Abs->Float.abs x | Not->if x=0. then 1. else 0.
                   | Tan->tan x | Asin->asin x | Acos->acos x | Exp->exp x | Log->log x
                   | Sign->if x>0. then 1. else if x<0. then -1. else x in
                 if not (Float.is_finite value) then raise Non_finite;scratch.(d+i)<-value done
           | Prim (Truthy,[_,src]) ->
               let a=scalar src in fun (scratch,_,_,_) _ lanes count -> for j=0 to count-1 do
                 let i=lanes.(j) in scratch.(d+i)<-(if scratch.(a+i)<>0. then 1. else 0.) done
           | _ -> raise Exit)
      | If {condition;yes;no} ->
          let a=scalar condition and k= !masks in masks:=k+2;
          let yes=compile yes and no=compile no in
          fun ((scratch,_,masks,_) as work) first lanes count ->
            let yes_lanes=masks.(k) and no_lanes=masks.(k+1) and ny=ref 0 and nn=ref 0 in
            for j=0 to count-1 do let i=lanes.(j) in
              if scratch.(a+i)<>0. then (yes_lanes.(!ny)<-i;incr ny) else (no_lanes.(!nn)<-i;incr nn) done;
            yes work first yes_lanes !ny;no work first no_lanes !nn
      | Store {sequence;index=idx;value} ->
          let w=add_array stores sequence in array_index idx;
          let src=Array.init w (at value) in
          fun (scratch,arrays,_,_) first lanes count ->
            let data=arrays.(sequence) in
            for c=0 to w-1 do for j=0 to count-1 do let i=lanes.(j) in
              let x=scratch.(src.(c)+i) in if not (Float.is_finite x) then raise Non_finite;
              data.((first+i)*w+c)<-x done done
      | _ -> raise Exit in
    let run=compile body in
    if !stores=[] then raise Exit;
    let captures=Array.of_list !captures and arrays=Array.of_list (List.sort_uniq Int.compare (!inputs @ !stores)) in
    let pool=Atomic.make None in
    Some (fun st first length ->
      let buffers=Array.make n [||] in
      let valid=Array.for_all (fun a -> match st.values.(a),Flow.Packed_ops.array_width program.locals.(a) with
        | V.Float_array data,Some (1 as w) | Vec2_array data,Some (2 as w)
        | Vec3_array data,Some (3 as w) | Vec4_array data,Some (4 as w) ->
            buffers.(a)<-data; Array.length data mod w=0 && first>=0 && length<=Array.length data/w-first
            && (not (List.mem a !stores) || Array.length st.builders>0 &&
              Option.fold ~none:false ~some:(fun writable -> !writable) st.builders.(a))
        | _ -> false) arrays in
      let disjoint=List.for_all (fun output -> List.for_all (fun input ->
        buffers.(output)!=buffers.(input)) !inputs) !stores in
      if not valid || not disjoint || first<>0 || first+length>=9007199254740992 then false else
      let saved=match Atomic.exchange pool None with Some work -> work | None ->
        Array.make (!registers*block_size) 0.,Array.init !masks (fun _ -> Array.make block_size 0),Array.init block_size Fun.id in
      let scratch,masks,lanes=saved in
      let fill n =
        let offset,w=reg n in
        let fill c x=Array.fill scratch (offset+c*block_size) block_size x in
        match program.locals.(n),st.values.(n) with
        | Flow.Ty.Float,V.Float x -> fill 0 x
        | Int,V.Int x when abs_float (float x)<9007199254740992. -> fill 0 (float x)
        | Bool,V.Bool b -> fill 0 (if b then 1. else 0.)
        | Vec2,V.Vec2 (x,y) -> fill 0 x;fill 1 y
        | Vec3,V.Vec3 (x,y,z) -> fill 0 x;fill 1 y;fill 2 z
        | Vec4,V.Vec4 (x,y,z,w) -> fill 0 x;fill 1 y;fill 2 z;fill 3 w
        | _ -> ignore w;raise Exit in
      let result=try
        Array.iter fill captures;
        let calls=Array.copy st.calls in
        let read=Array.make n false in
        let work=scratch,buffers,masks,(calls,st.live,read) in
        let start=ref 0 in
        while !start<length do
          let count=min block_size (length- !start) in
          (* The two loop counters remain exact integers; only their array-address use
             enters this tier, so they need no float registers. *)
          run work (first+ !start) lanes count;start:= !start+count
        done;
        Array.blit calls 0 st.calls 0 (Array.length calls);
        Array.iteri (fun n touched -> if touched then seal st n) read;true
      with Non_finite | Exit -> false in
      Atomic.set pool (Some saved);result)
  with Exit -> None

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
      || List.exists (fun (_, v) -> hidden v) (E.Private.residual_view r).bindings
  | E.Deferred _ -> true
  | E.List xs -> Array.exists hidden xs
  | E.Record fs | E.Struct (_, _, fs) -> List.exists (fun (_, v) -> hidden v) fs
  | _ -> false

let footprint (program : C.program) (s : C.statement) =
  let reads = ref [] and writes = ref [] in
  let read n = reads := n :: !reads and write n = writes := n :: !writes in
  let rec frame_free = function
    | C.Region.Frame _ | Int_frame _ | Rand_at _ -> false
    | Guarded (_, i) -> frame_free i
    | _ -> true in
  let expr = function
    | C.Constant _ -> () | Local n | Field (n, _) -> read n
    | Prim ((Rand | Rand_at), _) -> raise Exit
    | Prim (_, args) -> List.iter (fun (_, n) -> read n) args
    | Vec ns | Items ns -> List.iter read ns
    | Fields xs -> List.iter (fun (_, n) -> read n) xs
    | Call_index _ -> ()  (* a per-execution call counter the loop resets itself *)
    | Frame _ | Source _ | Host_source _ | Source_descriptor _ -> raise Exit in
  let rec go (s : C.statement) = match s.node with
    | Assign (n, e) -> expr e; write n
    | If {condition; yes; no} -> read condition; List.iter go yes; List.iter go no
    | Iterate {sequence; index; item; body; _} -> read sequence; write index; write item; List.iter go body
    | Counted {first; last; count; index; item; init; body; _} ->
        read first; read last; write count; write index; write item; List.iter go init; List.iter go body
    (* the array a loop fills is its output: a hit hands back the filled one *)
    | Store {sequence; index; value} -> write sequence; read index; read value
    | Invoke {region; args; result} ->
        let r = program.regions.(region) in
        if not (Array.for_all frame_free r.code) || Array.exists hidden r.uniforms
           || Array.exists (fun (residual, _) -> hidden (E.Residual residual)) r.sources then raise Exit;
        List.iter read args; write result
    | Protect {body; value; failure; handler; mapped; result} ->
        List.iter go body; List.iter go handler; read value; read mapped; write failure; write result
    | Diagnose_rows {rows; index; handler; diagnostic; result; _} ->
        read rows; write index; List.iter go handler; read diagnostic; write result
    | Reset_call _ -> ()
    | Draw _ | State_read _ | State_peek _ | State_write _ -> raise Exit in
  match go s with
  | () ->
      let writes = List.sort_uniq compare !writes in
      Some (Array.of_list (List.filter (fun n -> not (List.mem n writes)) (List.sort_uniq compare !reads)),
            Array.of_list writes)
  | exception Exit -> None

(* Every local a statement mentions, its nested statements' included. *)
let rec mentions acc (s : C.statement) =
  let expr acc = function
    | C.Constant _ | Call_index _ | Frame _ | Source _ -> acc
    | Local n | Field (n, _) -> n :: acc
    | Prim (_, args) | Host_source {args; _} | Source_descriptor {args; _} -> List.map snd args @ acc
    | Vec ns | Items ns -> ns @ acc
    | Fields xs -> List.map snd xs @ acc in
  let block acc = List.fold_left mentions acc in
  match s.node with
  | Assign (n, e) -> expr (n :: acc) e
  | Reset_call n -> n :: acc
  | Draw d -> (match d with
      | Background n | Push (_, n) -> n :: acc
      | Point {at; ink} -> at :: ink :: acc
      | Mark {args; _} | Batch {args; _} | Path {args; _} -> List.map snd args @ acc
      | Points {positions; ink} -> positions :: ink :: acc
      | Text_run {at; text; ink; size} -> at :: text :: ink :: size :: acc
      | Image {source = (_, n); at; scale; angle} -> n :: at :: scale :: angle :: acc
      | Pop -> acc)
  | Protect {body; value; failure; handler; mapped; result} ->
      block (block (value :: failure :: mapped :: result :: acc) body) handler
  | Diagnose_rows {rows; index; handler; diagnostic; result; _} ->
      block (rows :: index :: diagnostic :: result :: acc) handler
  | If {condition; yes; no} -> block (block (condition :: acc) yes) no
  | Iterate {sequence; index; item; body; _} -> block (sequence :: index :: item :: acc) body
  | Counted {first; last; count; index; item; init; body; _} ->
      block (block (first :: last :: count :: index :: item :: acc) init) body
  | Store {sequence; index; value} -> sequence :: index :: value :: acc
  | State_read {result; init; initial; _} -> block (result :: initial :: acc) init
  | State_peek {found; value; _} -> found :: value :: acc
  | State_write {value; _} -> value :: acc
  | Invoke {args; result; _} -> result :: args @ acc

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
    | If {yes; no; _} -> List.for_all ok yes && List.for_all ok no
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
  let table = ref [] in
  fun st ->
    let key = Array.map (fun n -> st.values.(n)) reads in
    match List.find_opt (fun (k, _) -> Array.for_all2 same_input k key) !table with
    | Some (_, values) -> Array.iteri (fun i n -> st.values.(n) <- values.(i)) writes
    | None ->
        run st;
        let values = Array.map (fun n -> st.values.(n)) writes in
        (* a region left on the GPU names a buffer of this draw's mark table, cleared by the next *)
        if not (Array.exists Core_marks.is_device values) then
        table := (key, values) :: List.filteri (fun i _ -> i < 3) !table

let prepare ?(fused=true) ?(remember_loops=false) (program : C.program) =
  let track = builds program.body in
  let device_ok = device_regions program in
  let static_ok = static_invokes program in
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
  let rec count statements = List.iter (fun (s : C.statement) -> match s.node with
    | Assign (n,_) | State_write {value=n;_} | Invoke {result=n;_} -> bump n
    | Reset_call _ | Store _ | Draw _ -> ()
    | Protect {body;handler;failure;result;_}->bump failure;bump result;count body;count handler
    | Diagnose_rows{index;result;handler;_}->bump index;bump result;count handler
    | If {yes;no;_} -> count yes; count no
    | Iterate {index;item;body;_} -> bump index; bump item; count body
    | Counted {count=c;index;item;init;body;_} -> bump c; bump index; bump item; count init; count body
    | State_read {result;init;_} -> bump result; count init
    | State_peek {found;value;_} -> bump found; bump value) statements in
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
      | (Iterate _ | Counted _ | Invoke _) when top && remember_loops ->
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
      with V.Fail (code,message,None) -> raise (V.Fail (code,message,spans.(!i)))
        | V.Diagnostic_fail d when d.span=None -> V.raise_diagnostic {d with span=spans.(!i)}
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
    | Counted {first;last;count;index;item;limit;init;body} ->
        let kernel=if fused && init=[] then counted_kernel program s.id index item body else None in
        let init=plain_block init and setup,body=loop_body body in
        fun st ->
          let first=V.int_of st.values.(first) and last=V.int_of st.values.(last) in
          let length=max 0 (last-first) in
          if length>limit then V.fail "E_ITER_BOUND" "Core iteration exceeds its bound.";
          st.values.(count)<-V.Int length;
          init st;
          setup st;
          let done_=match kernel with Some run when length>0 -> run st first length | _ -> false in
          if not done_ then for i=0 to length-1 do
            st.values.(index)<-V.Int i;
            st.values.(item)<-V.Int (first+i);
            body st
          done else begin st.values.(index)<-V.Int (length-1);st.values.(item)<-V.Int (last-1) end
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
          st.values.(result)<-E.Private.state_read (state_root st) ~instance ~path ~iter ~host
            ~init:(fun () -> init st; st.values.(initial))
    | State_peek {slot;found;value} ->
        fun st ->
          let instance,path,iter,host=slot_key st slot in
          let pending=E.Private.state_peek (state_root st) ~instance ~path ~iter ~host in
          st.values.(found)<-V.Bool (Option.is_some pending);
          st.values.(value)<-Option.value ~default:V.No_geo pending
    | State_write {slot;value} ->
        fun st ->
          let instance,path,iter,host=slot_key st slot in
          seal st value;
          st.values.(value)<-E.Private.state_write (state_root st) ~instance ~path ~iter ~host st.values.(value)
    | Invoke {region;args;result} ->
        let packed=Packed.of_region program.regions.(region) in
        let packed=if args=[] then Result.bind packed(fun packed ->
          Result.map_error(fun d->[d])(Packed.Private.bind_arguments packed[||])) else packed in
        let on_device=device_ok.(region) and keep=static_ok.(result) in
        let kept=ref None in
        fun st ->
          if Array.length st.builders>0 then st.builders.(result)<-None;
          match !kept with
          | Some v when st.marks<>None -> st.values.(result)<-v
          | _ ->
          let fail (d : Flow.Diagnostic.t) = V.raise_diagnostic d in
          let packed=match packed with Ok p -> p | Error (d :: _) -> fail d | Error [] -> assert false in
          let bind ?validate packed = if args=[] then packed else match Packed.Private.bind_arguments ?validate packed
            (Array.of_list(List.map(fun n->st.values.(n))args)) with Ok p->p | Error d->fail d in
          let unbound=packed in
          let resident=match st.resident with
            | Some r when st.resident_on -> (match r.invoke (bind ~validate:false unbound) ~live:st.live with
                | Some v -> st.values.(result)<-v; true
                | None -> List.iter (fun n -> st.values.(n) <- r.finish st.values.(n)) args; false)
            | _ -> false in
          if not resident then
          let packed=bind unbound in
          let device=match st.device,st.marks with
            | Some device,Some sink when on_device ->
                (match device packed with
                 | Ok (Some (token,count,width)) ->
                     st.values.(result)<-Core_marks.device_value sink ~token ~count ~width; true
                 | Ok None -> false
                 | Error d -> fail d)
            | _ -> false in
          if not device then
            (match (match st.force with Some force -> force packed | None -> Packed.force packed ~live:st.live) with
             | Ok v -> st.values.(result)<-v; if keep && st.marks<>None then kept:=Some v
             | Error d -> fail d)) in
  {program; body=block program.body; code=if fused then scalar_tree program constants else None;
   pool=Atomic.make None; stateless=Array.length program.state_slots=0 && Array.length program.regions=0}

let has_regions prepared = Array.length prepared.program.regions>0

let general root force resolve prepared live =
  let program=prepared.program in
  match Frame_input.validate live with
    | Error message -> Error (Flow.Diagnostic.error ~code:"E_FRAME" message)
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
        with V.Fail (code,message,span) -> Error (Flow.Diagnostic.error ?span ~code message)
          | V.Diagnostic_fail d -> Error d
          | Not_found | Invalid_argument _ | Failure _ -> Error (Flow.Diagnostic.error ~code:"E_CORE" "Invalid core program.") in
        release (); result

let attempt ?root ?force ?resolve prepared ~live =
  (* The postfix code reads only the time and every step is finite-checked, so frame facts it
     never reads need no validation: an invalid frame ends in the walker's own result anyway. *)
  match prepared.code with
  | None -> general root force resolve prepared live
  | Some code -> (match code_value code live.t with r -> Ok (V.Float r)
      | exception Non_finite -> general root force resolve prepared live)

let fast prepared = if prepared.stateless then Some (fun live -> attempt prepared ~live) else None

(* State needs its residual even in strict mode; strict failures restore it without
   entering the tree walker. Ordinary failures retain the evaluator's fallback. *)
let execute ?root ?(strict=false) ?force ?resolve prepared ~live = match root with
  | None -> attempt ?force ?resolve prepared ~live
  | Some r ->
      let restore = if prepared.stateless then ignore else E.Private.checkpoint r in
      (match (try attempt ~root:r ?force ?resolve prepared ~live with _ ->
         Error (Flow.Diagnostic.error ~code:"E_CORE" "Core execution failed.")) with
       | Ok _ as ok -> ok
       | Error _ as error when strict || resolve<>None -> restore (); error
       | Error _ -> restore (); E.Private.rerun r)

let run ?root ?strict ?force ?resolve program ~live = execute ?root ?strict ?force ?resolve (prepare program) ~live

module Buffers = Core_marks.Buffers

(* A draw program (Core_draw.lower) fills a mark table. A failure is the core's own
   diagnostic: there is no walker for a drawing, so nothing is rerun. *)
let draw ?root ?force ?resolve ?device ?resident ?image prepared ~live ~size ~buffers =
  match Frame_input.validate live with
  | Error message -> Error (Flow.Diagnostic.error ~code:"E_FRAME" message)
  | Ok () ->
      let program=prepared.program in
      let sink=Core_marks.sink ?image:(Option.map (fun resolve value ->
        match resolve value with
        | Ok resource -> resource
        | Error (d : Flow.Diagnostic.t) -> V.raise_diagnostic d) image) ~size buffers in
      let st = {values=Array.make (Array.length program.locals) E.No_geo;
        builders=[||]; calls=Array.make program.call_slots 0; live; root; force; resolve; marks=Some sink; device;
        resident; resident_on=false} in
      (try prepared.body st; Ok (Core_marks.finish sink)
       with V.Fail (code,message,span) -> Error (Flow.Diagnostic.error ?span ~code message)
         | V.Diagnostic_fail d -> Error d
         | Not_found | Invalid_argument _ | Failure _ -> Error (Flow.Diagnostic.error ~code:"E_CORE" "Invalid core program."))
