(** Serializable structured frame programs. Locals and constants are numbered in
    authored traversal order. Values are immutable after publication. *)
type literal =
  | Int of int | Float of float | Bool of bool | Text of string
  | Vector of float array | List of literal array
  | Record of (string * literal) list
  | Array of { width : int; data : float array }
  | Int_array of int array
  | Text_array of string array
  | Empty_geometry

type integer = Flow.Packed_ops.integer = Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ishl | Ishr
type byte_mode = Clamped | Unclamped
type primitive =
  | Numeric of Flow.Packed_ops.binary | Math of Flow.Packed_ops.unary
  | Remainder | Exp | Floor | Ceil | Round | Int_convert | Float_convert
  | Integer of integer | Integer_compare of Flow.Packed_ops.comparison | Equal | Show | Text_concat | Text_blank
  | Text_trim | Text_lower | Text_repr | Text_table_decode | Text_table_encode
  | Text_split | Text_tokens | Text_to_float | Text_to_int
  | Sequence_length | Sequence_get | Sequence_build | Sequence_slice | Sequence_concat
  | Array_max_length | Array_range | Array_int | Array_text | Array_float | Array_vec3 | Array_vec4 | Array_nth | Array_count | Array_slice | Array_concat
  | Array_reduce of Flow.Op.reduction | Array_segment of Flow.Op.reduction
  | Array_segment_argmax | Next_after
  | Array_scatter_add | Array_compact | Array_scan_sum | Array_grade | Array_index_of
  | Rand | Rand_at | Fma | Checked_mul | Checked_fma | Noise3 | Color_parse | Color_byte of byte_mode
  | Coerce of Flow.Ty.t | Truthy | Error | Fail of string * string | Fail_diagnostic of Diagnostic.t

type frame_field = Time | Dt | Index | Tick | Width | Height | Pointer_x | Pointer_y | Input

type expr =
  | Constant of int | Local of int | Frame of frame_field
  | Call_index of int
  | Source of Flow.Eval.value
  | Host_source of { name : string; ty : Flow.Ty.t; args : (string * int) list }
  | Source_descriptor of { name : string; ty : Flow.Ty.t; args : (string * int) list }
  | Prim of primitive * (string * int) list
  | Vec of int list | Fields of (string * int) list | Items of int list
  | Field of int * string

type iteration = Static_index of int | Local_index of int
type source = {
  site : Flow.Workspace.path; owner : Flow.Workspace.path;
  instance : int; prefix : Flow.Workspace.path;
  iter : iteration list; span : Diagnostic.span option;
}

(* Drawing statements: operands are locals holding the operator's own inputs, with every
   default already resolved by the lowering (Core_draw); the executor checks and encodes them. *)
type mark = Rect | Circle | Line
type transform = Translate | Rotate | Scale
type path = Rounded_rect | Polygon | Polyline
type draw =
  | Background of int
  | Point of { at : int; ink : int }
  | Mark of { mark : mark; args : (string * int) list }
  | Batch of { mark : mark; args : (string * int) list }
  | Points of { positions : int; ink : int }
  | Path of { path : path; args : (string * int) list }
  | Text_run of { at : int; text : int; ink : int; size : int }
  | Image of { source : Flow.Ty.t * int; at : int; scale : int; angle : int }
  | Push of transform * int
  | Pop

type statement = { id : int; source : source; node : statement_node }
and statement_node =
  | Draw of draw
  | Assign of int * expr
  | Reset_call of int
  | Protect of { body : statement list; value : int; failure : int;
      handler : statement list; mapped : int; result : int }
  | Diagnose_rows of { rows : int; policy : Flow.Workspace.diagnostic_policy; index : int;
      handler : statement list; diagnostic : int; result : int }
  | If of { condition : int; yes : statement list; no : statement list }
  | Iterate of { sequence : int; index : int; item : int; limit : int; body : statement list }
  | Counted of { first : int; last : int; count : int; index : int; item : int;
      limit : int; stop : int option; init : statement list; body : statement list }
  | Store of { sequence : int; index : int; value : int }
  | State_read of { slot : int; result : int; init : statement list; initial : int }
  | State_peek of { slot : int; found : int; value : int }
      (** [found] := whether this frame already holds a pending write; [value] := it. *)
  | State_write of { slot : int; value : int }
  | Invoke of { region : int; args : int list; result : int }

type state_slot = {
  source : source; prefix : Flow.Workspace.path; zone : Flow.Workspace.path;
  plane : [ `Frame | `Host ]; ty : Flow.Ty.t;
}
module Region = struct
  type projection = Value | Valid
  type conversion = Flow.Packed_ops.conversion = To_float | Truncate | Floor | Ceil | Round | Coerce_int | Integer_result of string
  type loop = {first:int; last:int; finish:int; initial:int array; limit:int option;
    span:Diagnostic.span}
  type stage = Fwidth | Sample of int * int | Texture_size of int * int
    (** [fwidth x]; component c of texture t sampled at (u, v) (its sampler at t + 1); axis c of
        texture t's size in texels *)
  type instruction =
    | Const of float | Input of int * int * int
    | Uniform of int * int | Frame of string | Accumulator of int
    | Binary of Flow.Packed_ops.binary * int * int | Unary of Flow.Packed_ops.unary * int
    | Noise3 of int * int * int * int * int * float * float | Fma of int * int * int | Rand_at of int * int | Select of int * int * int
    | Checked_mul of projection * int * int | Checked_fma of projection * int * int * int
    | Trace of int * int array
    | Stage of stage * int array
      (** One field (0 t, 1 prim, 2 u, 3 v, 4-6 normal, 7 hit, 8 instance) of a host ray query: registers
          scene id, origin, direction, tmin, tmax, time. *)
    | Array_prepare of int * Flow.Ty.t
    | Gather of int * int * int * int | Int_gather of int * int
    | Text_const of string | Text_input of int | Text_uniform of int
    | Text_gather of int * int | Text_select of int * int * int
    | Int_const of int | Int_input of int | Int_uniform of int | Int_frame of string | Int_accumulator
    | Int_binary of Flow.Packed_ops.integer * int * int
    | Int_compare of Flow.Packed_ops.comparison * int * int | Int_abs of int
    | Convert of conversion * int | Int_select of int * int * int
    | Guarded of (int * bool) list * instruction
    | Raise of Flow.Ty.t * string * string * string option
    | Loop_begin of loop
    | Loop_carry of int * int * Flow.Ty.t
    | Loop_end of int * int array * int option
    | Loop_result of int * int * Flow.Ty.t
  let rec integer_register = function
    | Int_const _ | Int_input _ | Int_uniform _ | Int_frame _ | Int_accumulator
    | Int_binary _ | Int_abs _ | Int_select _ | Int_gather _ | Raise (Flow.Ty.Int,_,_,_) -> true
    | Convert (To_float,_) -> false | Convert _ -> true
    | Guarded (_,instruction) -> integer_register instruction
    | Loop_begin _ | Loop_carry (_,_,Flow.Ty.Int) | Loop_result (_,_,Flow.Ty.Int) -> true
    | _ -> false
  let rec text_register = function
    | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _
    | Raise (Flow.Ty.Text,_,_,_) -> true
    | Guarded (_,instruction) -> text_register instruction
    | Loop_carry (_,_,Flow.Ty.Text) | Loop_result (_,_,Flow.Ty.Text) -> true
    | _ -> false
  let rec operands = function
    | Binary (_,a,b) | Int_binary (_,a,b) | Int_compare (_,a,b) | Rand_at(a,b) | Checked_mul(_,a,b)
    | Gather(a,b,_,_) | Int_gather(a,b) | Text_gather(a,b) -> [a;b]
    | Trace (_,registers) | Stage (_,registers) -> Array.to_list registers
    | Unary (_,a) | Int_abs a | Convert (_,a) -> [a]
    | Noise3 (a,b,c,_,_,_,_) | Fma(a,b,c) | Select (a,b,c) | Int_select (a,b,c) | Text_select(a,b,c) | Checked_fma(_,a,b,c) -> [a;b;c]
    | Guarded (guards,instruction) -> List.map fst guards @ operands instruction
    | Loop_begin {first;last;initial;_} -> first::last::Array.to_list initial
    | Loop_carry (loop,_,_) -> [loop]
    | Loop_end (loop,values,stop) -> loop::Array.to_list values @ Option.to_list stop
    | Loop_result (loop,component,_) -> [loop;loop+1+component]
    | _ -> []
  type iteration = Zip | Product
  type result = Collect | Sum of bool | Accumulate of Flow.Workspace.term * bool
  type origin = Flow.Workspace.path * int list
  type count_source = Flow.Eval.residual -> Flow.Workspace.term -> origin option
  type t = {
    residual : Flow.Eval.residual; term : Flow.Workspace.term;
    sources : (Flow.Eval.residual * Flow.Workspace.term) array; widths : int array;
    uniforms : Flow.Eval.value array; uniform_names : (string * string list) array;
    code : instruction array; dependent : bool array option Atomic.t;
    outputs : int array array; (* component registers of each output; [Collect] regions only may have several *)
    count : int option; origin : origin option;
    iteration : iteration; result : result; skip : int array;
    stages : (Flow.Eval.residual * Flow.Workspace.term) list;
    fusion : bool; dynamic : bool; count_source : count_source;
    body : Flow.Workspace.term; binding_fn : string option;
    sites : (int * Flow.Workspace.path * int list) list }
  let dependent_of code =
    let dependent = Array.make (Array.length code) false in
    let rec accumulator = function Accumulator _ | Int_accumulator -> true
      | Guarded (_, body) -> accumulator body | _ -> false in
    Array.iteri (fun id instruction ->
      dependent.(id) <- accumulator instruction
        || List.exists (fun register -> dependent.(register)) (operands instruction);
      let rec finish = function
        | Loop_end (loop,_,_) ->
            let carried=ref dependent.(loop) in
            for slot=loop+1 to id do carried:= !carried || dependent.(slot) done;
            Array.fill dependent loop (id-loop+1) !carried
        | Guarded (_,body) -> finish body
        | _ -> () in
      finish instruction) code;
    dependent
  let dependent_flags r = match Atomic.get r.dependent with
    | Some flags -> flags
    | None ->
        let flags = dependent_of r.code in
        if Atomic.compare_and_set r.dependent None (Some flags) then flags
        else Option.get (Atomic.get r.dependent)

  let controls_valid code outputs =
    let n=Array.length code in
    let scopes=Array.make n [] and closed=Array.make n false in
    let active=ref [] and valid=ref true in
    let rec bare=function Guarded(_,body)->bare body|body->body in
    let rec guards=function Guarded(gs,body)->gs@guards body|_->[] in
    let inside outer inner=List.for_all(fun scope->List.mem scope inner)outer in
    let header id=if id<0 || id>=n then None else match bare code.(id)with
      |Loop_begin loop->Some loop|_->None in
    let scalar id ty=if id<0 || id>=n then false else match ty with
      |Flow.Ty.Int->integer_register code.(id)
      |Text->text_register code.(id)
      |Float|Bool->not(integer_register code.(id) || text_register code.(id))
      |_->false in
    Array.iteri(fun slot instruction->
      scopes.(slot)<- !active;
      let reads=operands instruction in
      if List.exists(fun id->id<0 || id>=slot)reads then valid:=false
      else if (match bare instruction with Loop_result _->false|_->true) &&
        List.exists(fun id->not(inside scopes.(id) !active))reads then valid:=false;
      match bare instruction with
      |Loop_begin loop->
          if loop.finish<=slot+Array.length loop.initial || loop.finish>=n
            || loop.initial=[||] || loop.first<0 || loop.first>=slot || loop.last<0 || loop.last>=slot
            || not(integer_register code.(loop.first) && integer_register code.(loop.last))
            || Option.fold ~none:false ~some:((>)0)loop.limit then valid:=false
          else begin
            (match bare code.(loop.finish)with
             |Loop_end(owner,values,_)when owner=slot && Array.length values=Array.length loop.initial->()
             |_->valid:=false);
            Array.iteri(fun component initial->match bare code.(slot+1+component)with
              |Loop_carry(owner,index,ty)when owner=slot && index=component && scalar initial ty->()
              |_->valid:=false)loop.initial
          end;
          active:=slot:: !active;scopes.(slot)<- !active
      |Loop_carry(loop,component,ty)->
          (match !active,header loop with
           |owner::_,Some control when owner=loop && component>=0 && component<Array.length control.initial
             && slot=loop+1+component && scalar control.initial.(component)ty
             && guards instruction=guards code.(loop)->()
           |_->valid:=false)
      |Loop_end(loop,values,stop)->
          (match !active,header loop with
           |owner::rest,Some control when owner=loop && control.finish=slot
             && control.finish>loop+Array.length control.initial && control.finish<n
             && Array.length values=Array.length control.initial && guards instruction=guards code.(loop)->
               Array.iteri(fun component value->match bare code.(loop+1+component)with
                 |Loop_carry(_,_,ty)when scalar value ty->()|_->valid:=false)values;
               if Option.fold ~none:false ~some:(fun value->not(scalar value Flow.Ty.Bool))stop then valid:=false;
               closed.(loop)<-true;active:=rest
           |_->valid:=false)
      |Loop_result(loop,component,ty)->
          (match header loop with
           |Some control when loop<slot && closed.(loop) && component>=0
             && loop+1+component<n
             && component<Array.length control.initial && inside (List.tl scopes.(loop)) !active
             && guards instruction=guards code.(loop)->
               (match bare code.(loop+1+component)with
                |Loop_carry(_,_,stored)when stored=ty->()|_->valid:=false)
           |_->valid:=false)
      |_->())code;
    !valid && !active=[] && Array.for_all(Array.for_all(fun id->
      id>=0 && id<n && scopes.(id)=[] && match bare code.(id)with
      |Loop_begin _|Loop_carry _|Loop_end _->false|_->true))outputs

  let validate (r : t) =
    let invalid message =
      Result.Error [Diagnostic.error ~span:r.term.form.span ~code:"E_PACKED_LAYOUT" message] in
    let ok slot id = id >= 0 && id < slot in
    let operands_ok = ref true and controls = ref false in
    let rec bare = function Guarded (_, body) -> bare body | body -> body in
    let array_handle id = id >= 0 && id < Array.length r.code
      && (match bare r.code.(id) with Array_prepare _ -> true | _ -> false) in
    let table_type id = match bare r.code.(id) with
      | Array_prepare (_, ty) -> ty | _ -> Flow.Ty.Any in
    let text id = id >= 0 && id < Array.length r.code && text_register r.code.(id) in
    let rec guards = function Guarded (gs, body) -> gs @ guards body | _ -> [] in
    let prepared_for selected array =
      List.for_all (fun guard -> List.mem guard selected) (guards r.code.(array)) in
    let rec valid ?(selected = []) slot instruction =
      if not (List.for_all (ok slot) (operands instruction)) then operands_ok := false;
      (match instruction with
       | Gather _ | Int_gather _ | Text_gather _ | Guarded _ -> ()
       | _ -> if List.exists array_handle (operands instruction) then operands_ok := false);
      (match instruction with
       | Text_select _ | Guarded _ | Loop_begin _ | Loop_end _ | Loop_result _ -> ()
       | _ -> if List.exists text (operands instruction) then operands_ok := false);
      match instruction with
      | Array_prepare (input, ty) ->
          if input < 0 || input >= Array.length r.uniforms
            || not (match ty with Flow.Ty.Array (Flow.Ty.Int | Text | Float | Vec2 | Vec3 | Vec4) -> true | _ -> false)
          then operands_ok := false
          else
            let actual = match r.uniforms.(input) with
              | Flow.Eval.Residual residual -> (Flow.Eval.Residual.residual_view residual).term.ty
              | value -> Flow.Value.ty_of value in
            if actual <> ty then operands_ok := false
      | Gather (array, index, width, component) ->
          if not (ok slot array && ok slot index && array_handle array)
            || not (integer_register r.code.(index)) || component < 0 || component >= width
          then operands_ok := false
          else if not (prepared_for selected array) || table_type array = Flow.Ty.Array Flow.Ty.Int
            || Flow.Packed_ops.array_width (table_type array) <> Some width then operands_ok := false
      | Int_gather (array, index) ->
          if not (ok slot array && ok slot index && array_handle array)
            || not (integer_register r.code.(index)) then operands_ok := false
          else if not (prepared_for selected array) || table_type array <> Flow.Ty.Array Flow.Ty.Int
          then operands_ok := false
      | Text_gather (array, index) ->
          if not (ok slot array && ok slot index && array_handle array)
            || not (integer_register r.code.(index)) then operands_ok := false
          else if not (prepared_for selected array) || table_type array <> Flow.Ty.Array Flow.Ty.Text
          then operands_ok := false
      | Text_select (condition, yes, no) ->
          if array_handle condition || text condition || not (text yes && text no) then operands_ok := false
      | Text_input input ->
          if input < 0 || input >= Array.length r.sources || input >= Array.length r.widths
            || r.widths.(input) <> 1 || (snd r.sources.(input)).Flow.Workspace.ty <> Flow.Ty.Array Flow.Ty.Text
          then operands_ok := false
      | Text_uniform input ->
          if input < 0 || input >= Array.length r.uniforms then operands_ok := false
          else
            let ty = match r.uniforms.(input) with
              | Flow.Eval.Residual residual -> (Flow.Eval.Residual.residual_view residual).term.ty
              | value -> Flow.Value.ty_of value in
            if ty <> Flow.Ty.Text then operands_ok := false
      | Input (input, width, component) ->
          if input < 0 || input >= Array.length r.widths || width <> r.widths.(input)
            || component < 0 || component >= width
            || input < Array.length r.sources && (snd r.sources.(input)).Flow.Workspace.ty = Flow.Ty.Array Flow.Ty.Text
          then operands_ok := false
      | Int_input input ->
          if input < 0 || input >= Array.length r.sources || input >= Array.length r.widths
            || r.widths.(input) <> 1 || (snd r.sources.(input)).Flow.Workspace.ty <> Flow.Ty.Array Flow.Ty.Int
          then operands_ok := false
      | Uniform (input, component) ->
          if input < 0 || input >= Array.length r.uniforms || component < 0 || component >= 4 then operands_ok := false
          else if (match r.uniforms.(input) with
            | Flow.Eval.Residual residual -> (Flow.Eval.Residual.residual_view residual).term.ty
            | value -> Flow.Value.ty_of value) = Flow.Ty.Text then operands_ok := false
      | Int_uniform input ->
          if input < 0 || input >= Array.length r.uniforms then operands_ok := false
          else
            let ty = match r.uniforms.(input) with
              | Flow.Eval.Residual residual -> (Flow.Eval.Residual.residual_view residual).term.ty
              | value -> Flow.Value.ty_of value in
            if ty <> Flow.Ty.Int then operands_ok := false
      | Accumulator component -> if component < 0 || component >= 4 then operands_ok := false
      | Loop_begin _ | Loop_carry _ | Loop_end _ | Loop_result _ -> controls:=true
      | Guarded (guards, body) ->
          if List.exists (fun (id, _) -> array_handle id || text id) guards then operands_ok := false;
          valid ~selected:(selected @ guards) slot body
      | _ -> () in
    Array.iteri valid r.code;
    if Array.length r.widths <> Array.length r.sources
      || not (Array.for_all (fun width -> width >= 1 && width <= 4) r.widths) then
      invalid "Region inputs need matching sources and widths from 1 to 4."
    else if not !operands_ok || (!controls && not(controls_valid r.code r.outputs)) then
      invalid "Region instruction reads an invalid input or a register defined later."
    else if Array.length r.code > Flow.Packed_ops.register_limit then
      invalid (Printf.sprintf "Region exceeds %d registers." Flow.Packed_ops.register_limit)
    else if r.outputs = [||] || (Array.length r.outputs > 1 && r.result <> Collect) then
      invalid "A region has one output, or several only when it collects."
    else if not (Array.for_all (fun output -> Array.length output >= 1 && Array.length output <= 4
        && Array.for_all (fun id -> ok (Array.length r.code) id && not (array_handle id)) output
        && (not (Array.exists text output) || Array.length output = 1)) r.outputs) then
      invalid "Region output needs 1 to 4 existing scalar registers."
    else if r.result <> Collect && Array.exists text_register r.code then
      invalid "Text leaves require collecting kernels."
    else Result.Ok r

  let make ~residual ~term ~sources ~widths ~uniforms ~uniform_names ~code ~outputs ~count ~origin
      ~iteration ~result ~skip ~stages ~fusion ~dynamic ~count_source ~body ~binding_fn ~sites =
    validate {residual; term; sources; widths; uniforms; uniform_names; code; dependent = Atomic.make None; outputs;
      count; origin; iteration; result; skip; stages; fusion; dynamic; count_source; body; binding_fn; sites}

  let rebuild t ?residual ?term ?sources ?widths ?uniforms ?uniform_names ?code ?outputs ?count ?origin
      ?iteration ?result ?skip ?stages ?fusion ?dynamic ?count_source ?body ?binding_fn ?sites () =
    make ~residual:(Option.value residual ~default:t.residual)
      ~term:(Option.value term ~default:t.term) ~sources:(Option.value sources ~default:t.sources)
      ~widths:(Option.value widths ~default:t.widths) ~uniforms:(Option.value uniforms ~default:t.uniforms)
      ~uniform_names:(Option.value uniform_names ~default:t.uniform_names)
      ~code:(Option.value code ~default:t.code) ~outputs:(Option.value outputs ~default:t.outputs)
      ~count:(Option.value count ~default:t.count) ~origin:(Option.value origin ~default:t.origin)
      ~iteration:(Option.value iteration ~default:t.iteration) ~result:(Option.value result ~default:t.result)
      ~skip:(Option.value skip ~default:t.skip) ~stages:(Option.value stages ~default:t.stages)
      ~fusion:(Option.value fusion ~default:t.fusion) ~dynamic:(Option.value dynamic ~default:t.dynamic)
      ~count_source:(Option.value count_source ~default:t.count_source) ~body:(Option.value body ~default:t.body)
      ~binding_fn:(Option.value binding_fn ~default:t.binding_fn) ~sites:(Option.value sites ~default:t.sites)
  module Private = struct
    let with_captures (t : t) ~residual ~uniforms ~sources ~stages ~count =
      let diagnostic = Diagnostic.error ~span:t.term.form.span ~code:"E_CORE_ARGUMENT"
        "A rebound region changed its validated capture layout." in
      let ty = function
        | Flow.Eval.Residual residual -> (Flow.Eval.Residual.residual_view residual).term.ty
        | value -> Flow.Value.ty_of value in
      if Array.length uniforms <> Array.length t.uniforms
        || not (Array.for_all2 (fun before after -> ty before = ty after) t.uniforms uniforms)
        || Array.length sources <> Array.length t.sources
        || not (Array.for_all2 (fun (_, before) (_, after) -> before.Flow.Workspace.ty = after.Flow.Workspace.ty)
          t.sources sources)
        || List.length stages <> List.length t.stages
      then Result.Error diagnostic
      else Result.Ok {t with residual; uniforms; sources; stages; count}
  end
end
type region = Region.t

type program = {
  constants : literal array; locals : Flow.Ty.t array;
  body : statement list; results : int array;
  state_slots : state_slot array; regions : region array;
  call_slots : int;
}

(* These describe one statement only. [iter] supplies the recursive preorder,
   leaving passes free to keep their own branch and scope handling. *)
let reads (s : statement) =
  let expr = function
    | Constant _ | Call_index _ | Frame _ | Source _ -> []
    | Local n | Field (n, _) -> [n]
    | Prim (_, args) | Host_source {args; _} | Source_descriptor {args; _} -> List.map snd args
    | Vec ns | Items ns -> ns
    | Fields xs -> List.map snd xs in
  let draw = function
    | Background n | Push (_, n) -> [n]
    | Point {at; ink} -> [at; ink]
    | Mark {args; _} | Batch {args; _} | Path {args; _} -> List.map snd args
    | Points {positions; ink} -> [positions; ink]
    | Text_run {at; text; ink; size} -> [at; text; ink; size]
    | Image {at; scale; angle; _} -> [at; scale; angle]
    | Pop -> [] in
  match s.node with
  | Draw d -> draw d
  | Assign (_, e) -> expr e
  | Reset_call _ | State_peek _ -> []
  | Protect {value; mapped; _} -> [value; mapped]
  | Diagnose_rows {rows; diagnostic; _} -> [rows; diagnostic]
  | If {condition; _} -> [condition]
  | Iterate {sequence; _} -> [sequence]
  | Counted {first; last; stop; _} -> first::last::Option.to_list stop
  | Store {index; value; _} -> [index; value]
  | State_read {initial; _} -> [initial]
  | State_write {value; _} -> [value]
  | Invoke {args; _} -> args

let writes (s : statement) = match s.node with
  | Assign (n, _) -> [n]
  | Reset_call _ | Draw _ | State_write _ -> []
  | Protect {failure; result; _} -> [failure; result]
  | Diagnose_rows {index; result; _} -> [index; result]
  | If _ -> []
  | Iterate {index; item; _} -> [index; item]
  | Counted {count; index; item; _} -> [count; index; item]
  | Store {sequence; _} -> [sequence]
  | State_read {result; _} -> [result]
  | State_peek {found; value; _} -> [found; value]
  | Invoke {result; _} -> [result]

let children (s : statement) = match s.node with
  | Protect {body; handler; _} -> body @ handler
  | Diagnose_rows {handler; _} -> handler
  | If {yes; no; _} -> yes @ no
  | Iterate {body; _} -> body
  | Counted {init; body; _} -> init @ body
  | State_read {init; _} -> init
  | Draw _ | Assign _ | Reset_call _ | Store _ | State_peek _ | State_write _ | Invoke _ -> []

let iter f statements =
  let rec block statements = List.iter (fun s -> f s; block (children s)) statements in
  block statements

let primitive_name = function
  | Numeric op -> (match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "mod" | Pow -> "pow"
      | Min -> "min" | Max -> "max" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      | Eq -> "=" | And -> "and" | Or -> "or" | Atan2 -> "atan2" | Hypot -> "hypot")
  | Math op -> (match op with Sin -> "sin" | Cos -> "cos" | Sqrt -> "sqrt" | Abs -> "abs" | Not -> "not"
      | Tan -> "tan" | Asin -> "asin" | Acos -> "acos" | Exp -> "exp" | Log -> "log" | Sign -> "sign")
  | Remainder -> "remainder" | Exp -> "exp" | Floor -> "floor" | Ceil -> "ceil" | Round -> "round"
  | Int_convert -> "int" | Float_convert -> "float"
  | Integer op -> Flow.Packed_ops.integer_name op
  | Integer_compare op -> Flow.Packed_ops.comparison_name op
  | Equal -> "equal?" | Show -> "show" | Text_concat -> "text/concat"
  | Text_blank -> "text/blank?"
  | Text_trim -> "text/trim" | Text_lower -> "text/lower" | Text_repr -> "text/repr"
  | Text_table_decode -> "text/table_decode" | Text_table_encode -> "text/table_encode"
  | Text_split -> "text/split" | Text_tokens -> "text/tokens"
  | Text_to_float -> "text/to_float" | Text_to_int -> "text/to_int"
  | Array_reduce operation | Array_segment operation as primitive ->
      let suffix=match operation with Flow.Op.Sum -> "sum" | Minimum -> "min" | Maximum -> "max" in
      "array/" ^ (match primitive with Array_segment _ -> "segment_" | _ -> "") ^ suffix
  | Array_segment_argmax -> "array/segment_argmax" | Next_after -> "float/next_after"
  | Array_grade -> "array/grade" | Array_index_of -> "array/index_of"
  | Array_compact -> "array/compact" | Array_scan_sum -> "array/scan_sum"
  | Array_scatter_add -> "array/scatter_add"
  | Sequence_length -> "sequence/length" | Sequence_get -> "sequence/get"
  | Sequence_build -> "sequence/build" | Sequence_slice -> "sequence/slice" | Sequence_concat -> "sequence/concat"
  | Array_max_length -> "array/max_length" | Array_range -> "array/range" | Array_int -> "array/int" | Array_float -> "array/float" | Array_vec3 -> "array/vec3" | Array_vec4 -> "array/vec4"
  | Array_text -> "array/text"
  | Array_nth -> "array/nth" | Array_count -> "array/count" | Array_slice -> "array/slice" | Array_concat -> "array/concat"
  | Rand -> "value/rand" | Rand_at -> "rand/at" | Fma -> "fma" | Noise3 -> "noise3" | Color_parse -> "color/parse" | Color_byte _ -> "color/byte"
  | Checked_mul -> "float/mul_checked" | Checked_fma -> "float/fma_checked"
  | Coerce _ -> "coerce" | Truthy -> "truthy" | Error -> "error" | Fail _ | Fail_diagnostic _ -> "fail"

let frame_name = function
  | Time -> "t" | Dt -> "dt" | Index -> "frame" | Tick -> "tick" | Width -> "width" | Height -> "height"
  | Pointer_x -> "pointer/x" | Pointer_y -> "pointer/y" | Input -> "input"

exception Not_literal
let literal_of_value value =
  let rec convert : Flow.Eval.value -> literal = function
    | Int n -> Int n | Float x -> Float x | Bool b -> Bool b | Text s -> Text s
    | Vec2 (x,y) -> Vector [|x;y|] | Vec3 (x,y,z) -> Vector [|x;y;z|]
    | Vec4 (x,y,z,w) -> Vector [|x;y;z;w|]
    | List xs when Array.length xs <= 4096 -> List (Array.map convert xs)
    | Record xs -> Record (List.map (fun (n,v) -> n,convert v) xs)
    | Int_array data -> Int_array data
    | Text_array data -> Text_array data
    | Float_array data -> Array {width=1;data} | Vec2_array data -> Array {width=2;data}
    | Vec3_array data -> Array {width=3;data} | Vec4_array data -> Array {width=4;data}
    | No_geo -> Empty_geometry | _ -> raise Not_literal in
  try Some (convert value) with Not_literal -> None

let rec value_of_literal : literal -> Flow.Eval.value = function
  | Int n -> Int n | Float x -> Float x | Bool b -> Bool b | Text s -> Text s
  | Vector [|x;y|] -> Vec2 (x,y) | Vector [|x;y;z|] -> Vec3 (x,y,z)
  | Vector [|x;y;z;w|] -> Vec4 (x,y,z,w)
  | List xs -> List (Array.map value_of_literal xs)
  | Record xs -> Record (List.map (fun (n,v) -> n,value_of_literal v) xs)
  | Int_array data -> Int_array data
  | Text_array data -> Text_array data
  | Array {width=1;data} -> Float_array data | Array {width=2;data} -> Vec2_array data
  | Array {width=3;data} -> Vec3_array data | Array {width=4;data} -> Vec4_array data
  | Empty_geometry -> No_geo | Vector _ | Array _ -> invalid_arg "Core.value_of_literal: width"

let pp fmt p =
  let out format = Format.fprintf fmt format in
  let separated render xs = List.iteri (fun i x -> if i > 0 then out " "; render x) xs in
  let local n = out "%%%d" n in
  let rec literal = function
    | Int n -> out "%d" n | Float f -> out "%s" (Flow.Lisp.float f)
    | Bool b -> out "%b" b | Text s -> out "%S" s
    | Vector xs -> out "vec["; separated (fun x -> out "%s" (Flow.Lisp.float x)) (Array.to_list xs); out "]"
    | List xs -> out "list["; separated literal (Array.to_list xs); out "]"
    | Record xs -> out "record{"; separated (fun (n,v) -> out "%S:" n; literal v) xs; out "}"
    | Int_array data -> out "array:int["; separated (fun n -> out "%d" n) (Array.to_list data); out "]"
    | Text_array data -> out "array:text["; separated (fun s -> out "%S" s) (Array.to_list data); out "]"
    | Array {width;data} -> out "array%d[" width;
        separated (fun x -> out "%s" (Flow.Lisp.float x)) (Array.to_list data); out "]"
    | Empty_geometry -> out "nil" in
  let expression = function
    | Constant n -> out "$%d" n | Local n -> local n | Frame f -> out "frame.%s" (frame_name f)
    | Call_index n -> out "call[%d]++" n
    | Source value -> out "source<%s>" (Flow.Ty.to_string (Flow.Value.ty_of value))
    | Host_source {name;ty;args}->out "host_source<%s %S>(" (Flow.Ty.to_string ty) name;
        separated (fun(name,n)->out "%S:" name;local n)args;out ")"
    | Source_descriptor {name;ty;args}->out "source_descriptor<%s %S>(" (Flow.Ty.to_string ty) name;
        separated (fun(name,n)->out "%S:" name;local n)args;out ")"
    | Prim (op,args) ->
        out "%s" (primitive_name op);
        (match op with Coerce ty -> out "<%s>" (Flow.Ty.to_string ty)
          | Color_byte Clamped -> out "<clamped>" | Color_byte Unclamped -> out "<unclamped>"
          | Fail (code,message) -> out "<%S %S>" code message
          | Fail_diagnostic d -> out "<%S %S cause=%S>" d.code d.message (Option.value ~default:"" d.cause)
          | _ -> ());
        out "("; separated (fun (name,n) -> out "%S:" name; local n) args; out ")"
    | Vec ns -> out "vec["; separated local ns; out "]"
    | Fields xs -> out "record{"; separated (fun (name,n) -> out "%S:" name; local n) xs; out "}"
    | Items ns -> out "list["; separated local ns; out "]"
    | Field (n,name) -> local n; out ".%S" name in
  let draw d = match d with
    | Background c -> out "draw/background "; local c
    | Point {at;ink} -> out "draw/point at="; local at; out " ink="; local ink
    | Mark {mark;args} | Batch {mark;args} ->
        let plural = match d with Batch _ -> "s" | _ -> "" in
        out "draw/%s%s" (match mark with Rect -> "rect" | Circle -> "circle" | Line -> "line") plural;
        List.iter (fun (name,n) -> out " %s=" name; local n) args
    | Points {positions;ink} -> out "draw/points positions="; local positions; out " ink="; local ink
    | Path {path;args} ->
        out "draw/%s" (match path with Rounded_rect -> "rounded_rect" | Polygon -> "polygon" | Polyline -> "polyline");
        List.iter (fun (name,n) -> out " %s=" name; local n) args
    | Text_run {at;text;ink;size} ->
        out "draw/text at="; local at; out " text="; local text; out " ink="; local ink; out " size="; local size
    | Image {source=(ty,id);at;scale;angle} ->
        out "draw/image %s#%d at=" (Flow.Ty.to_string ty) id; local at; out " scale="; local scale;
        out " angle="; local angle
    | Push (t,n) ->
        out "push %s " (match t with Translate -> "translate" | Rotate -> "rotate" | Scale -> "scale"); local n
    | Pop -> out "pop" in
  let path xs = out "["; separated (fun s -> out "%S" s) xs; out "]" in
  let source s =
    out " @"; path s.site; out " owner="; path s.owner; out " instance=%d iter=[" s.instance;
    separated (function Static_index n -> out "%d" n | Local_index n -> local n) s.iter; out "]";
    if s.prefix <> [] then (out " prefix="; path s.prefix);
    Option.iter (fun (s : Diagnostic.span) -> out " span=%d..%d" s.start s.finish) s.span in
  let rec statements depth xs =
    List.iter (fun s ->
      out "%s#%d " (String.make (depth * 2) ' ') s.id;
      let nested label xs = out "%s {\n" label; statements (depth+1) xs;
        out "%s}" (String.make (depth * 2) ' ') in
      (match s.node with
      | Assign (n,e) -> local n; out " = "; expression e
      | Reset_call n -> out "call[%d] <- 0" n
      | Protect {body;value;failure;handler;mapped;result}->
          local result;out " = protect ";nested "body" body;out " value=";local value;
          out " error=";local failure;out " ";nested "map" handler;out " value=";local mapped
      | Diagnose_rows {rows;policy;index;handler;diagnostic;result}->
          local result;out " = diagnose_rows ";local rows;out " policy=%s index="
            (match policy with Flow.Workspace.First->"first"|Range_last->"range-last");local index;
          out " ";nested "map" handler;out " diagnostic=";local diagnostic
      | If {condition;yes;no} -> out "if "; local condition; out " "; nested "then" yes;
          out " "; nested "else" no
      | Iterate {sequence;index;item;limit;body} -> out "iterate "; local sequence;
          out " index="; local index; out " item="; local item; out " limit=%d " limit; nested "do" body
      | Counted {first;last;count;index;item;limit;stop;init;body} ->
          out "counted "; local first; out ".."; local last;
          out " count="; local count; out " index="; local index; out " item="; local item;
          out " limit=%d " limit;Option.iter(fun n->out "stop=";local n;out " ")stop;
          nested "init" init; out " "; nested "do" body
      | Store {sequence;index;value} -> local sequence; out "["; local index; out "] <- "; local value
      | State_read {slot;result;init;initial} -> local result; out " = state[%d] " slot;
          nested "init" init; out " value="; local initial
      | State_peek {slot;found;value} -> local found; out " = state[%d] pending " slot; local value
      | State_write {slot;value} -> out "state[%d] <- " slot; local value
      | Invoke {region;args;result} -> local result; out " = region[%d](" region; separated local args; out ")"
      | Draw d -> draw d);
      source s.source; out "\n") xs in
  out "core\n";
  if p.call_slots > 0 then out "  calls %d\n" p.call_slots;
  Array.iteri (fun n v -> out "  $%d = " n; literal v; out "\n") p.constants;
  Array.iteri (fun n ty -> out "  %%%d : %s\n" n (Flow.Ty.to_string ty)) p.locals;
  Array.iteri (fun n (s : state_slot) -> out "  state[%d] : %s plane=%s prefix=" n (Flow.Ty.to_string s.ty)
    (match s.plane with `Frame -> "frame" | `Host -> "host"); path s.prefix; out " zone="; path s.zone;
    source s.source; out "\n") p.state_slots;
  Array.iteri (fun n (r : Region.t) ->
    let names a = String.concat " " (Array.to_list (Array.map string_of_int a)) in
    out "  region[%d] %s %s count=%s widths=[%s] output=[%s] uniforms=%d skip=[%s] stages=%d\n" n
      (match r.iteration with Region.Zip -> "zip" | Product -> "product")
      (match r.result with Collect -> "collect" | Sum zero -> if zero then "sum0" else "sum"
        | Accumulate (_, scan) -> if scan then "scan" else "fold")
      (match r.count with Some c -> string_of_int c | None -> "?")
      (names r.widths) (String.concat " | " (Array.to_list (Array.map names r.outputs))) (Array.length r.uniforms) (names r.skip) (List.length r.stages);
    let rec instruction (x : Region.instruction) = match x with
       | Const f -> out "%s" (Flow.Lisp.float f)
       | Input (s, w, c) -> out "input[%d].%d/%d" s c w
       | Uniform (u, c) -> out "uniform[%d].%d" u c
       | Frame name -> out "frame.%s" name
       | Accumulator c -> out "accumulator.%d" c
       | Binary (op, a, b) -> out "%s(r%d r%d)" (primitive_name (Numeric op)) a b
       | Unary (op, a) -> out "%s(r%d)" (primitive_name (Math op)) a
       | Noise3 (x, y, z, seed, octaves, lacunarity, gain) -> out "noise3(r%d r%d r%d seed=%d octaves=%d%s)" x y z seed octaves (if lacunarity = 2. && gain = 0.5 then "" else Printf.sprintf " lacunarity=%h gain=%h" lacunarity gain)
       | Rand_at(seed,index) -> out "rand_at(r%d r%d)" seed index
       | Fma(a,b,c) -> out "fma(r%d r%d r%d)" a b c
       | Checked_mul(p,a,b) -> out "mul_checked.%s(r%d r%d)" (match p with Value->"value"|Valid->"valid") a b
       | Array_prepare(i,ty) -> out "array_prepare u%d:%s" i (Flow.Ty.to_string ty)
       | Gather(a,i,w,c) -> out "gather(r%d r%d width=%d component=%d)" a i w c
       | Int_gather(a,i) -> out "int_gather(r%d r%d)" a i
       | Text_const s -> out "text_const %S" s
       | Text_input i -> out "text_input in%d" i
       | Text_uniform i -> out "text_uniform u%d" i
       | Text_gather(a,i) -> out "text_gather(r%d r%d)" a i
       | Text_select(c,a,b) -> out "text_select(r%d r%d r%d)" c a b
       | Trace (field,r) -> out "trace.%d(%s)" field (String.concat " " (List.map (Printf.sprintf "r%d") (Array.to_list r)))
       | Stage (_,r) -> out "stage(%s)" (String.concat " " (List.map (Printf.sprintf "r%d") (Array.to_list r)))
       | Checked_fma(p,a,b,c) -> out "fma_checked.%s(r%d r%d r%d)" (match p with Value->"value"|Valid->"valid") a b c
       | Select (c, a, b) | Int_select (c,a,b) -> out "select(r%d r%d r%d)" c a b
       | Int_const n -> out "int:%d" n | Int_input i -> out "input:int[%d]" i
       | Int_uniform i -> out "uniform:int[%d]" i | Int_accumulator -> out "accumulator:int"
       | Int_frame name -> out "frame:int[%s]" name
       | Int_binary (op,a,b) -> out "%s(r%d r%d)" (Flow.Packed_ops.integer_name op) a b
       | Int_compare (op,a,b) -> out "%s(r%d r%d)" (Flow.Packed_ops.comparison_name op) a b
       | Int_abs a -> out "int:abs(r%d)" a
       | Convert (op,a) -> out "%s(r%d)" (match op with
           | To_float -> "float" | Truncate -> "int" | Floor -> "floor" | Ceil -> "ceil"
           | Round -> "round" | Coerce_int -> "coerce:int" | Integer_result name -> "int-result:"^name) a
       | Guarded (guards,body) -> out "guard[";
           separated (fun (c,yes)->out "r%d=%b" c yes) guards;out "] ";instruction body
       | Raise (ty,code,message,cause) -> out "error<%s %S %S" (Flow.Ty.to_string ty) code message;
           Option.iter(fun cause->out " cause=%S" cause)cause;out ">"
       | Loop_begin l -> out "loop r%d..r%d end=%d" l.first l.last l.finish
       | Loop_carry (loop,component,ty) -> out "carry r%d.%d:%s" loop component (Flow.Ty.to_string ty)
       | Loop_end (loop,_,stop) -> out "next r%d%s" loop
           (Option.fold ~none:"" ~some:(Printf.sprintf " until r%d") stop)
       | Loop_result (loop,component,ty) -> out "result r%d.%d:%s" loop component (Flow.Ty.to_string ty) in
    Array.iteri (fun i x ->out "    r%d = " i;instruction x;
      out "\n") r.code) p.regions;
  statements 1 p.body;
  out "  return "; separated local (Array.to_list p.results); out "\n"
