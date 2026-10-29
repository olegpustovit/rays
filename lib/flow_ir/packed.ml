module E = Flow.Eval
module W = Flow.Workspace
module V = Flow.Value
module Ty = Flow.Ty

type binary = Flow.Packed_ops.binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Min
  | Max
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | And
  | Or
  | Atan2
  | Hypot
  | Ldexp

type unary = Flow.Packed_ops.unary =
  | Sin
  | Cos
  | Sqrt
  | Abs
  | Not
  | Tan
  | Asin
  | Acos
  | Exp
  | Log
  | Sign
  | Rsqrt

type conversion = Core.Region.conversion =
  | To_float
  | Truncate
  | Floor
  | Ceil
  | Round
  | Coerce_int
  | Integer_result of string

type projection = Core.Region.projection = Value | Valid

type loop = Core.Region.loop = {
  first : int;
  last : int;
  finish : int;
  initial : int array;
  limit : int option;
  span : Diagnostic.span;
}

type instruction = Core.Region.instruction =
  | Const of float
  | Input of int * int * int
  | Uniform of int * int
  | Frame of string
  | Accumulator of int
  | Binary of binary * int * int
  | Unary of unary * int
  | Noise3 of int * int * int * int * int * float * float
  | Fma of int * int * int
  | Rand_at of int * int
  | Select of int * int * int
  | Checked_mul of projection * int * int
  | Checked_fma of projection * int * int * int
  | Trace of int * int array
  | Stage of Core.Region.stage * int array
  | Array_prepare of int * Ty.t
  | Gather of int * int * int * int
  | Int_gather of int * int
  | Text_const of string
  | Text_input of int
  | Text_uniform of int
  | Text_gather of int * int
  | Text_select of int * int * int
  | Int_const of int
  | Int_input of int
  | Int_uniform of int
  | Int_frame of string
  | Int_accumulator
  | Int_binary of Flow.Packed_ops.integer * int * int
  | Int_compare of Flow.Packed_ops.comparison * int * int
  | Int_abs of int
  | Convert of conversion * int
  | Int_select of int * int * int
  | Guarded of (int * bool) list * instruction
  | Raise of Ty.t * string * string * string option
  | Loop_begin of loop
  | Loop_carry of int * int * Ty.t
  | Loop_end of int * int array * int option
  | Loop_result of int * int * Ty.t

type iteration = Core.Region.iteration = Zip | Product
type result = Core.Region.result = Collect | Sum of bool | Accumulate of W.term * bool
type origin = W.path * int list
type count_source = E.residual -> W.term -> origin option
type column = { data : float array; offset : int; stride : int }
type t = Core.Region.t

let operands = Core.Region.operands

(* a raster-stage intrinsic (Region.Stage) has no CPU value *)
let stage_refused () =
  raise
    (Diagnostic.Error
       (Diagnostic.error ~code:"E_GPU_STAGE"
          "A raster-stage intrinsic runs only in a fragment stage."))

let rec typed_instruction = function
  | Int_const _ -> false (* compile-time configuration literals may be unused *)
  | Int_input _ | Int_uniform _ | Int_frame _ | Int_accumulator | Int_binary _ | Int_compare _
  | Int_abs _ | Convert _ | Int_select _ | Raise _ | Rand_at _ | Trace _ | Array_prepare _
  | Gather _ | Int_gather _ | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _
  | Text_select _ ->
      true
  | Loop_begin _ | Loop_carry _ | Loop_end _ | Loop_result _ -> true
  | Guarded (_, body) -> typed_instruction body
  | _ -> false

let integer_program (t : t) =
  Array.exists typed_instruction t.code
  || Array.exists (Array.exists (fun r -> Core.Region.integer_register t.code.(r))) t.outputs
  || Array.exists (fun (_, term) -> term.W.ty = Ty.Array Ty.Int) t.sources

let[@inline always] binary_apply op x y =
  match op with
  | Add -> x +. y
  | Sub -> x -. y
  | Mul -> x *. y
  | Div -> if y = 0. then 0. else x /. y
  | Mod -> if y = 0. then 0. else Float.rem (Float.rem x y +. y) y
  | Pow -> Float.pow (Float.abs x) y
  | Min -> Float.min x y
  | Max -> Float.max x y
  | Lt -> if x < y then 1. else 0.
  | Le -> if x <= y then 1. else 0.
  | Gt -> if x > y then 1. else 0.
  | Ge -> if x >= y then 1. else 0.
  | Eq -> if x = y then 1. else 0.
  | And -> if x <> 0. && y <> 0. then 1. else 0.
  | Or -> if x <> 0. || y <> 0. then 1. else 0.
  | Atan2 -> Float.atan2 x y
  | Hypot -> Float.hypot x y
  | Ldexp -> Float.ldexp x (int_of_float y)

let[@inline always] unary_apply op x =
  match op with
  | Sin -> sin x
  | Cos -> cos x
  | Sqrt -> sqrt (Float.abs x)
  | Abs -> Float.abs x
  | Not -> if x = 0. then 1. else 0.
  | Tan -> tan x
  | Asin -> asin x
  | Acos -> acos x
  | Exp -> exp x
  | Log -> log x
  | Sign -> if x > 0. then 1. else if x < 0. then -1. else x
  | Rsqrt -> 1. /. sqrt (Float.abs x)

let[@inline always] checked_projection projection value =
  let valid = Float.is_finite value in
  match projection with Value -> if valid then value else 0. | Valid -> if valid then 1. else 0.

let convert_int op x =
  match op with
  | Truncate -> V.to_int "int" (Float.trunc x)
  | Floor -> V.to_int "floor" (Float.floor x)
  | Ceil -> V.to_int "ceil" (Float.ceil x)
  | Round -> V.to_int "round" (Float.round x)
  | Coerce_int -> V.to_int "int" (Float.floor (x +. 0.5))
  | Integer_result name -> V.to_int name (Float.floor (x +. 0.5))
  | To_float -> assert false

exception Unsupported = Core_region.Unsupported

let width_of = Core_region.width_of
let components = Core_region.components
let zipped (t : t) = t.iteration = Zip || Array.length t.sources = 1
let rec unguarded = function Guarded (_, body) -> unguarded body | instruction -> instruction

let output_reachable (t : t) =
  let used = Array.make (Array.length t.code) false in
  let rec use slot =
    if not used.(slot) then begin
      used.(slot) <- true;
      List.iter use (operands t.code.(slot));
      match unguarded t.code.(slot) with
      | Loop_begin loop ->
          for body = slot + 1 to loop.finish do
            use body
          done
      | _ -> ()
    end
  in
  Array.iter (Array.iter use) t.outputs;
  used

let finite_checks (t : t) =
  (* GPU qualification rejects nonfinite constants. These operations cannot
     introduce a nonfinite result; their operands are checked at their source. *)
  Array.map
    (fun instruction ->
      match unguarded instruction with
      | Const _ | Select _ | Array_prepare _
      | Unary ((Abs | Sqrt | Not), _)
      | Loop_begin _ | Loop_carry _ | Loop_end _ | Loop_result _ | Checked_mul _ | Checked_fma _
      | Binary ((Min | Max | Lt | Le | Gt | Ge | Eq | And | Or), _, _) ->
          false
      | body when Core.Region.text_register body -> false
      | _ -> true)
    t.code

let gpu_refusals ?(fused = false) ?(fast = false) (t : t) =
  let reasons = ref [] in
  let refuse message = reasons := Diagnostic.error ~code:"E_GPU_FORM" message :: !reasons in
  if Array.exists Core.Region.text_register t.code then refuse "Text kernels stay on the CPU.";
  if
    (not fast)
    && Array.exists
         (fun instruction -> match unguarded instruction with Trace _ -> true | _ -> false)
         t.code
  then refuse "scene/trace needs fast precision and a bound acceleration structure.";
  (* fast precision (the workspace default) takes gathers and int32 on the device *)
  if
    (not fast)
    && Array.exists
         (fun instruction ->
           match unguarded instruction with
           | Array_prepare _ | Gather _ | Int_gather _ -> true
           | _ -> false)
         t.code
  then refuse "Indexed array reads stay on the CPU.";
  if (not fast) && integer_program t && not (Array.exists Core.Region.text_register t.code) then
    refuse "Integer sources, uniforms and instructions stay on the CPU.";
  if
    fast
    && Array.exists
         (fun instruction ->
           match unguarded instruction with Rand_at _ | Int_accumulator -> true | _ -> false)
         t.code
  then refuse "Random streams and integer accumulators stay on the CPU.";
  if
    Array.exists
      (fun instruction ->
        match unguarded instruction with Binary (Hypot, _, _) -> true | _ -> false)
      t.code
  then refuse "Hypot requires the CPU binary64 implementation.";
  if not fused then begin
    if
      Array.exists
        (fun instruction -> match unguarded instruction with Fma _ -> true | _ -> false)
        t.code
    then refuse "FMA requires exact single-rounding binary64 arithmetic.";
    if
      Array.exists
        (fun instruction ->
          match unguarded instruction with Checked_mul _ | Checked_fma _ -> true | _ -> false)
        t.code
    then refuse "Checked arithmetic requires binary64 overflow status."
  end;
  if t.result <> Collect || (not (zipped t)) || t.skip <> [||] then
    refuse "GPU kernels require Collect with Zip iteration and no skipped elements.";
  if
    Array.exists
      (fun instruction ->
        match unguarded instruction with Accumulator _ | Int_accumulator -> true | _ -> false)
      t.code
  then refuse "Ordered accumulators stay on the CPU.";
  let used = output_reachable t in
  let bad_noise = ref false and bad_constant = ref false in
  Array.iteri
    (fun i instruction ->
      if used.(i) then
        match unguarded instruction with
        | Noise3 (_, _, _, _, octaves, lacunarity, gain) ->
            if octaves > 1 && (lacunarity <> 2. || gain <> 0.5) then bad_noise := true
            else if not (Flow.Packed_ops.supported_gpu_noise_octaves octaves) then bad_noise := true
        | Const value -> if not (Flow.Packed_ops.finite_float32 value) then bad_constant := true
        | _ -> ())
    t.code;
  if !bad_noise then refuse "GPU noise supports 1 to 32 octaves with lacunarity 2 and gain 0.5.";
  if !bad_constant then refuse "Nonfinite float32 shader constants require reference evaluation.";
  List.rev !reasons

let compile_impl ?through_exact ?(fusion = true) ?(dynamic = false)
    ?(count_source = fun _ _ -> None) residual term =
  Result.map
    (fun region -> if fusion then Core_fuse.fuse ?through_exact region else region)
    (Core_region.lower ~fusion ~dynamic ~count_source residual term)

let static_count (t : t) = t.count
let count_origin (t : t) = t.origin

let traced trace result =
  Option.iter (fun f -> f result) trace;
  result

let compile_result ?trace ?through_exact ?fusion ?count_source r term =
  traced trace (compile_impl ?through_exact ?fusion ?count_source r term)

let compile ?trace ?through_exact ?fusion ?count_source r term =
  compile_result ?trace ?through_exact ?fusion ?count_source r term |> Result.to_option

let compile_template ?trace ?count_source r term =
  traced trace (compile_impl ~dynamic:true ?count_source r term) |> Result.to_option

let rebind (t : t) residual = Core_region.rebind t residual
let elementwise (t : t) = match t.result with Collect -> true | Sum _ | Accumulate _ -> false
let provenance (t : t) = List.sort_uniq compare t.sites

let site (t : t) =
  let view = E.Residual.residual_view t.residual in
  (view.instance, Option.value ~default:view.site t.term.path, view.iter)

let reference ?state ?elems ?resolve (t : t) ~live =
  E.Residual.eval_term ?state ?elems ?resolve t.residual t.term ~live

let reference_observed ?state ?elems ?resolve ?on_walk (t : t) ~live =
  E.Residual.eval_term ?state ?elems ?resolve ?on_walk t.residual t.term ~live

let nonfinite () =
  raise
    (Diagnostic.Error
       (Diagnostic.error ~code:"E_NONFINITE" "Packed kernel produced a nonfinite result."))

let get = function Ok x -> x | Error d -> raise (Diagnostic.Error d)

let rec force_values ?state ?elems ?resolve ~live ~residual = function
  | value when not (E.is_live value) -> Ok value
  | E.Residual r -> Result.bind (residual r) (force_values ?state ?elems ?resolve ~live ~residual)
  | E.Source _ as value ->
      Result.bind
        (E.Host.force_reference ?state ?elems ?resolve value ~live)
        (force_values ?state ?elems ?resolve ~live ~residual)
  | E.Struct (_, Ty.Array _, _) as value ->
      Result.bind
        (E.Host.force_reference ?state ?elems ?resolve value ~live)
        (force_values ?state ?elems ?resolve ~live ~residual)
  | E.List values ->
      let out = Array.make (Array.length values) E.No_geo in
      let rec loop i =
        if i = Array.length values then Ok (E.List out)
        else
          Result.bind
            (force_values ?state ?elems ?resolve ~live ~residual values.(i))
            (fun value ->
              out.(i) <- value;
              loop (i + 1))
      in
      loop 0
  | E.Record fields ->
      let rec loop acc = function
        | [] -> Ok (E.Record (List.rev acc))
        | (name, value) :: rest ->
            Result.bind (force_values ?state ?elems ?resolve ~live ~residual value) (fun value ->
                loop ((name, value) :: acc) rest)
      in
      loop [] fields
  | E.Struct (name, ty, fields) ->
      let rec loop acc = function
        | [] -> Ok (E.Struct (name, ty, List.rev acc))
        | (field, value) :: rest ->
            Result.bind (force_values ?state ?elems ?resolve ~live ~residual value) (fun value ->
                loop ((field, value) :: acc) rest)
      in
      loop [] fields
  | value -> Ok value

let borrowed_count width columns =
  if Array.length columns <> width then
    V.fail "E_ARRAY_TYPE" "Borrowed columns changed the packed element width.";
  let length c =
    if c.stride <= 0 || c.offset < 0 || c.offset > Array.length c.data then
      V.fail "E_ARRAY_RANGE" "Borrowed column offset and stride must describe an in-bounds span."
    else if c.offset = Array.length c.data then 0
    else 1 + ((Array.length c.data - 1 - c.offset) / c.stride)
  in
  let count = length columns.(0) in
  if not (Array.for_all (fun c -> length c = count) columns) then
    V.fail "E_ARRAY_COUNT" "Borrowed components need the same element count.";
  count

let rec integer_range residual (term : W.term) =
  match term.node with
  | W.Ref_binding (name, []) -> (
      match List.assoc_opt name (E.Residual.residual_view residual).bindings with
      | Some (E.Residual nested) -> integer_range nested (E.Residual.residual_view nested).term
      | _ -> None)
  | W.Op { op = "array/range"; args; skip = [] } when term.ty = Ty.Array Ty.Int -> (
      match (List.assoc_opt "type" args, List.assoc_opt "count" args) with
      | Some { node = W.Text "int" | Lit (Param.Text_value "int"); _ }, Some count ->
          Some (residual, count)
      | _ -> None)
  | _ -> None

let block_layout ~count ~limit =
  let block_size = max 1 (min count limit) in
  let blocks_per_chunk = 16 in
  let blocks = if count = 0 then 0 else ((count - 1) / block_size) + 1 in
  let chunks = if blocks = 0 then 0 else ((blocks - 1) / blocks_per_chunk) + 1 in
  (block_size, blocks, blocks_per_chunk, chunks)

let typed_inputs ?column_source ~evaluate (t : t) =
  let borrowed = Array.make (Array.length t.sources) None in
  let ranges = Array.make (Array.length t.sources) None in
  let empty_product = ref false in
  let lengths = Array.make (Array.length t.sources) 0 in
  let inputs =
    Array.mapi
      (fun i (r, term) ->
        if !empty_product then E.No_geo
        else begin
          borrowed.(i) <-
            (match (column_source, term.W.ty) with
            | Some source, Ty.Array (Ty.Float | Vec2 | Vec3 | Vec4) -> get (source r term)
            | _ -> None);
          let value =
            if borrowed.(i) <> None then E.No_geo
            else
              match integer_range r term with
              | Some (r, count) ->
                  ranges.(i) <-
                    Some
                      (Flow.Op.array_count ~limit:Sys.max_array_length "array/range"
                         (evaluate r count));
                  E.No_geo
              | None -> evaluate r term
          in
          lengths.(i) <-
            (match (ranges.(i), borrowed.(i)) with
            | Some count, _ -> count
            | None, None -> V.array_length value
            | None, Some columns -> borrowed_count t.widths.(i) columns);
          if t.iteration = Product && lengths.(i) = 0 then empty_product := true;
          value
        end)
      t.sources
  in
  (inputs, borrowed, ranges, lengths)

type loop_scratch = {
  floats : float array;
  integer : int array;
  texts : string array;
  banks : bool array;
  text_offsets : int array;
  block_size : int;
  next_float : float array;
  next_int : int array;
  next_text : string array;
}

let copy_loop s source target lane =
  if s.text_offsets.(target) >= 0 then
    s.texts.((s.text_offsets.(target) * s.block_size) + lane) <-
      s.texts.((s.text_offsets.(source) * s.block_size) + lane)
  else if s.banks.(target) then
    s.integer.((target * s.block_size) + lane) <- s.integer.((source * s.block_size) + lane)
  else s.floats.((target * s.block_size) + lane) <- s.floats.((source * s.block_size) + lane)

let execute_loop s ~(execute : ?dependent:bool -> int -> int -> int -> int -> unit) ~code header
    (loop : loop) first last =
  match unguarded code.(loop.finish) with
  | Loop_end (_, next, stop) ->
      let count = Array.length loop.initial in
      for lane = first to last - 1 do
        let lo = s.integer.((loop.first * s.block_size) + lane)
        and hi = s.integer.((loop.last * s.block_size) + lane) in
        let length =
          try Flow.Op.range_length ~limit:(Option.value ~default:max_int loop.limit) lo hi
          with Diagnostic.Error d -> V.raise_diagnostic { d with span = Some loop.span }
        in
        for component = 0 to count - 1 do
          copy_loop s loop.initial.(component) (header + 1 + component) lane
        done;
        let iteration = ref 0 and finished = ref false in
        while !iteration < length && not !finished do
          s.integer.((header * s.block_size) + lane) <- lo + !iteration;
          execute (header + 1 + count) loop.finish lane (lane + 1);
          (finished :=
             match stop with
             | None -> false
             | Some slot -> s.floats.((slot * s.block_size) + lane) <> 0.);
          for component = 0 to count - 1 do
            let target = header + 1 + component and value = next.(component) in
            if s.text_offsets.(target) >= 0 then
              s.next_text.(component) <- s.texts.((s.text_offsets.(value) * s.block_size) + lane)
            else if s.banks.(target) then
              s.next_int.(component) <- s.integer.((value * s.block_size) + lane)
            else s.next_float.(component) <- s.floats.((value * s.block_size) + lane)
          done;
          for component = 0 to count - 1 do
            let target = header + 1 + component in
            if s.text_offsets.(target) >= 0 then
              s.texts.((s.text_offsets.(target) * s.block_size) + lane) <- s.next_text.(component)
            else if s.banks.(target) then
              s.integer.((target * s.block_size) + lane) <- s.next_int.(component)
            else s.floats.((target * s.block_size) + lane) <- s.next_float.(component)
          done;
          incr iteration
        done
      done
  | _ -> assert false

let rec typed_total ranges inputs = function
  | Int_const _ | Text_const _ | Int_abs _ | Int_compare _ | Int_select _ | Text_select _ -> true
  | Const x -> Float.is_finite x
  | Int_input i -> (
      ranges.(i) <> None || match inputs.(i) with E.Int_array _ -> true | _ -> false)
  | Text_input i -> ( match inputs.(i) with E.Text_array _ -> true | _ -> false)
  | Int_binary ((Iadd | Isub | Imul | Iand | Ior | Ixor), _, _) -> true
  | Guarded (_, body) -> typed_total ranges inputs body
  | _ -> false

let trace_field field (h : Flow.Ray_host.hit) =
  match field with
  | 0 -> h.t
  | 1 -> float h.prim
  | 2 -> h.u
  | 3 -> h.v
  | 4 -> h.nx
  | 5 -> h.ny
  | 6 -> h.nz
  | 7 -> if h.hit then 1. else 0.
  | _ -> float h.instance

(* one host query per ray: the eight fields of a lane share its last (scene, query) *)
let trace_hit traced j id q =
  match traced.(j) with
  | Some (i, k, h) when i = id && k = q -> h
  | _ ->
      let h = Flow.Ray_host.query id q in
      traced.(j) <- Some (id, q, h);
      h

let run_typed ?column_source ?plane_outputs ?expected_count ~evaluate ~uniform ~seed (t : t) ~live =
  let carried =
    Array.fold_left
      (fun count instruction ->
        match unguarded instruction with
        | Loop_begin loop -> max count (Array.length loop.initial)
        | _ -> count)
      0 t.code
  in
  let gather =
    Array.exists
      (fun instruction ->
        match unguarded instruction with
        | Array_prepare _ | Gather _ | Int_gather _ | Loop_begin _ -> true
        | body -> Core.Region.text_register body)
      t.code
  in
  let cache_guards =
    carried = 0 && t.result = Collect
    && Array.exists (function Guarded _ -> true | _ -> false) t.code
  in
  let inputs, borrowed, ranges, lengths = typed_inputs ?column_source ~evaluate t in
  (* A leading total prefix only writes private banks. Keep every potentially
     failing instruction and lazy capture in the existing reference lane order. *)
  let total = typed_total ranges inputs in
  let total_prefix =
    if gather && t.result = Collect then begin
      let length = ref 0 in
      while !length < Array.length t.code && total t.code.(!length) do
        incr length
      done;
      !length
    end
    else 0
  in
  let count =
    if inputs = [||] then 0
    else if t.iteration = Product && Array.exists (( = ) 0) lengths then 0
    else
      Array.fold_left
        (fun n length ->
          match t.iteration with
          | Zip -> min n length
          | Product ->
              if length <> 0 && n > max_int / length then
                V.fail "E_ARRAY_RANGE" "Kernel iteration count exceeds native bounds.";
              n * length)
        (if t.iteration = Zip then max_int else 1)
        lengths
  in
  let strides = Array.make (Array.length inputs) 1 in
  for i = Array.length inputs - 2 downto 0 do
    strides.(i) <- strides.(i + 1) * lengths.(i + 1)
  done;
  let skip =
    Array.to_list t.skip
    |> List.filter (fun i -> i >= 0 && i < count)
    |> List.sort_uniq Int.compare |> Array.of_list
  in
  let skipped_before n =
    let first = ref 0 and last = ref (Array.length skip) in
    while !first < !last do
      let middle = (!first + !last) / 2 in
      if skip.(middle) < n then first := middle + 1 else last := middle
    done;
    !first
  in
  let kept = count - Array.length skip in
  Option.iter
    (fun n ->
      if n <> kept then V.fail "E_ARRAY_COUNT" "Column sink count differs from the kernel count.")
    expected_count;
  let collecting = match t.result with Collect | Accumulate (_, true) -> true | _ -> false in
  let integer_outputs =
    Array.map
      (fun regs -> Array.length regs = 1 && Core.Region.integer_register t.code.(regs.(0)))
      t.outputs
  in
  let text_outputs =
    Array.map
      (fun regs -> Array.length regs = 1 && Core.Region.text_register t.code.(regs.(0)))
      t.outputs
  in
  Option.iter
    (fun outputs ->
      if t.result <> Collect || Array.length outputs <> Array.length t.outputs then
        V.fail "E_ARRAY_TYPE" "Column sinks require matching Collect outputs.";
      Array.iteri
        (fun k lanes ->
          if text_outputs.(k) then
            V.fail "E_ARRAY_TYPE" "Float column sinks cannot store Text outputs.";
          if integer_outputs.(k) || Array.length lanes <> Array.length t.outputs.(k) then
            V.fail "E_ARRAY_TYPE" "Float column sinks cannot store integer outputs.";
          Array.iter
            (Option.iter (fun a ->
                 if Array.length a <> kept then
                   V.fail "E_ARRAY_COUNT" "Column sink count differs from the kernel count."))
            lanes)
        outputs)
    plane_outputs;
  let outputs =
    Array.mapi
      (fun k registers ->
        if (not collecting) || plane_outputs <> None then E.No_geo
        else if text_outputs.(k) then begin
          if kept > Sys.max_array_length then
            V.fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
          E.Text_array (Array.make kept "")
        end
        else if integer_outputs.(k) then begin
          if kept > Sys.max_array_length then
            V.fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
          E.Int_array (Array.make kept 0)
        end
        else
          let width = Array.length registers in
          if kept > Sys.max_floatarray_length / width then
            V.fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
          let data = Array.make (kept * width) 0. in
          match width with
          | 2 -> E.Vec2_array data
          | 3 -> E.Vec3_array data
          | 4 -> E.Vec4_array data
          | _ -> E.Float_array data)
      t.outputs
  in
  let banks = Array.map Core.Region.integer_register t.code in
  let text_count = ref 0 in
  let text_offsets =
    Array.map
      (fun instruction ->
        if Core.Region.text_register instruction then (
          let i = !text_count in
          incr text_count;
          i)
        else -1)
      t.code
  in
  let[@inline always] finite x =
    if Float.is_finite x then x else V.fail "E_NONFINITE" "packed produced a nonfinite value."
  in
  let concrete = function
    | E.Int _ | Text _ | Float _ | Bool _ | Vec2 _ | Vec3 _ | Vec4 _ | Int_array _ | Text_array _
    | Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _ ->
        true
    | _ -> false
  in
  let scalar = function
    | E.Int _ | Float _ | Bool _ | Vec2 _ | Vec3 _ | Vec4 _ -> true
    | _ -> false
  in
  let uniforms =
    Array.map
      (fun value -> if concrete value then Lazy.from_val value else lazy (uniform value))
      t.uniforms
  in
  let uniform_components =
    Array.mapi
      (fun i value ->
        if scalar t.uniforms.(i) then Lazy.from_val (components t.uniforms.(i))
        else lazy (components (Lazy.force value)))
      uniforms
  in
  let prepared = Array.map (fun _ -> Atomic.make None) t.uniforms in
  let prepare i ty =
    match Atomic.get prepared.(i) with
    | Some value -> value
    | None ->
        let value = Lazy.force uniforms.(i) in
        if V.ty_of value <> ty then
          V.fail "E_ARRAY_TYPE" "Kernel lookup table changed its packed element type.";
        V.validate value;
        (* ponytail: concrete immutable tables may be checked by simultaneous
           first readers; serialize that scan only if domain-count cost measures hot. *)
        Atomic.set prepared.(i) (Some value);
        value
  in
  let table register =
    match unguarded t.code.(register) with
    | Array_prepare (i, _) -> (
        match Atomic.get prepared.(i) with
        | Some value -> value
        | None -> V.fail "E_PACKED_LAYOUT" "Array lookup needs its prepared table.")
    | _ -> V.fail "E_PACKED_LAYOUT" "Array lookup needs a table handle."
  in
  let row array index =
    if index < 0 || index > Sys.max_array_length then
      V.fail "E_ARRAY_RANGE" "array/nth needs a non-negative whole length.";
    if index >= V.array_length array then
      V.fail "E_ARRAY_RANGE" "Array index is outside its length.";
    index
  in
  let accumulator =
    ref (Option.value ~default:(if integer_outputs.(0) then E.Int 0 else E.Float 0.) seed)
  in
  let initialized = ref (match t.result with Sum zero -> zero | _ -> true) in
  (* a block's scratch holds every register for each of its lanes: a large kernel takes fewer lanes,
     so scratch stays near 64K slots (512 KiB a bank) *)
  let block_size, blocks, blocks_per_chunk, chunks =
    block_layout ~count ~limit:(max 64 (min 1024 (65536 / max 1 (Array.length t.code))))
  in
  let dependent_flags = if count = 0 then [||] else Core.Region.dependent_flags t in
  let chunk chunk =
    let integer = Array.make (Array.length t.code * block_size) 0
    and floats = Array.make (Array.length t.code * block_size) 0.
    and traced = Array.make block_size None in
    let texts = Array.make (!text_count * block_size) "" in
    let loops =
      if carried = 0 then None
      else
        Some
          {
            floats;
            integer;
            texts;
            banks;
            text_offsets;
            block_size;
            next_float = Array.make carried 0.;
            next_int = Array.make carried 0;
            next_text = Array.make carried "";
          }
    in
    let[@inline always] fill_int offset count value =
      if count = 1 then integer.(offset) <- value else Array.fill integer offset count value
    in
    let[@inline always] fill_float offset count value =
      if count = 1 then floats.(offset) <- value else Array.fill floats offset count value
    in
    let[@inline always] fill_text offset count value =
      if count = 1 then texts.(offset) <- value else Array.fill texts offset count value
    in
    let[@inline always] numeric r j =
      let at = (r * block_size) + j in
      if banks.(r) then float integer.(at) else floats.(at)
    in
    let rec selected j = function
      | [] -> true
      | (r, yes) :: rest -> numeric r j <> 0. = yes && selected j rest
    in
    let frame =
      Array.map
        (fun instruction ->
          lazy
            (match unguarded instruction with
            | Int_frame name | Frame name -> (
                if name = "t" then E.Float live.Input.t
                else
                  match Flow.Op.find name Flow.Context.value with
                  | Some op -> op.body ~live ~node:(fun _ _ -> raise Unsupported) []
                  | None -> raise Unsupported)
            | _ -> E.No_geo))
        t.code
    in
    let tables = Hashtbl.create 8 in
    let noise seed =
      match Hashtbl.find_opt tables seed with
      | Some n -> n
      | None ->
          let n = Rays_math.Noise.create seed in
          Hashtbl.add tables seed n;
          n
    in
    let scratch = Rays_math.Noise.create_fbm3_scratch () in
    let contiguous = t.iteration = Zip && skip = [||] in
    let indices = if contiguous then [||] else Array.make block_size 0 in
    for block = chunk * blocks_per_chunk to min blocks ((chunk + 1) * blocks_per_chunk) - 1 do
      let start = block * block_size and limit = min count ((block + 1) * block_size) in
      let before = skipped_before start in
      let length =
        if skip = [||] then begin
          if not contiguous then
            for j = 0 to limit - start - 1 do
              indices.(j) <- start + j
            done;
          limit - start
        end
        else
          let skipped = ref before and length = ref 0 in
          for i = start to limit - 1 do
            if !skipped < Array.length skip && skip.(!skipped) = i then incr skipped
            else (
              indices.(!length) <- i;
              incr length)
          done;
          !length
      in
      let source j i =
        let index = if contiguous then start + j else indices.(j) in
        if t.iteration = Zip then index else index / strides.(i) mod lengths.(i)
      in
      let guard_spans = ref (if cache_guards then Some (Hashtbl.create 16) else None) in
      let rec execute slot instruction first last =
        let at = slot * block_size in
        match instruction with
        | Loop_begin loop ->
            execute_loop (Option.get loops) ~execute:execute_range ~code:t.code slot loop first last
        | Loop_result (loop, component, _) ->
            for lane = first to last - 1 do
              copy_loop (Option.get loops) (loop + 1 + component) slot lane
            done
        | Loop_carry _ | Loop_end _ -> assert false
        | Text_const value ->
            fill_text ((text_offsets.(slot) * block_size) + first) (last - first) value
        | Text_uniform i ->
            if first < last then begin
              let value =
                match Lazy.force uniforms.(i) with
                | E.Text text -> text
                | _ -> V.fail "E_ARRAY_TYPE" "Kernel input changed its packed element type."
              in
              fill_text ((text_offsets.(slot) * block_size) + first) (last - first) value
            end
        | Text_input i ->
            let input =
              match inputs.(i) with
              | E.Text_array data -> data
              | _ -> V.fail "E_ARRAY_TYPE" "Kernel input changed its packed element type."
            in
            for j = first to last - 1 do
              texts.((text_offsets.(slot) * block_size) + j) <- input.(source j i)
            done
        | Text_gather (array, index) ->
            let array = table array in
            let data = match array with E.Text_array data -> data | _ -> assert false in
            for j = first to last - 1 do
              texts.((text_offsets.(slot) * block_size) + j) <-
                data.(row array integer.((index * block_size) + j))
            done
        | Text_select (condition, yes, no) ->
            for j = first to last - 1 do
              let register = if numeric condition j <> 0. then yes else no in
              texts.((text_offsets.(slot) * block_size) + j) <-
                texts.((text_offsets.(register) * block_size) + j)
            done
        | Guarded (guards, body) when last = first + 1 ->
            (* Ordered gather execution visits one lane; bypass the span
               scanner without inspecting any later lane. *)
            if selected first guards then execute slot body first last
        | Guarded (guards, body) -> (
            (* Publish spans after their first execution: scanning ahead of a
             failing body would change lazy predicate evaluation. *)
            let cached =
              match !guard_spans with
              | None -> None
              | Some cache -> Hashtbl.find_opt cache (guards, first, last)
            in
            match cached with
            | Some spans ->
                for i = 0 to (Array.length spans / 2) - 1 do
                  execute slot body spans.(2 * i) spans.((2 * i) + 1)
                done
            | None -> (
                let saved =
                  match !guard_spans with
                  | Some cache when Hashtbl.length cache < Array.length t.code ->
                      Some (ref [||], ref 0)
                  | _ -> None
                in
                let next = ref first in
                while !next < last do
                  while !next < last && not (selected !next guards) do
                    incr next
                  done;
                  let first = !next in
                  while !next < last && selected !next guards do
                    incr next
                  done;
                  if first < !next then begin
                    execute slot body first !next;
                    match saved with
                    | None -> ()
                    | Some (spans, used) ->
                        if !used = Array.length !spans then begin
                          let capacity = min (2 * block_size) (max 4 (2 * Array.length !spans)) in
                          let grown = Array.make capacity 0 in
                          Array.blit !spans 0 grown 0 !used;
                          spans := grown
                        end;
                        !spans.(!used) <- first;
                        !spans.(!used + 1) <- !next;
                        used := !used + 2
                  end
                done;
                match (!guard_spans, saved) with
                | Some cache, Some (spans, used) when Hashtbl.length cache < Array.length t.code ->
                    Hashtbl.replace cache (guards, first, last) (Array.sub !spans 0 !used)
                | _ -> ()))
        | Raise (_, code, message, cause) ->
            if first < last then V.raise_diagnostic (Diagnostic.error ?cause ~code message)
        | Array_prepare (i, ty) -> if first < last then ignore (prepare i ty)
        | Int_gather (array, index) -> (
            let array = table array in
            match array with
            | E.Int_array data ->
                for j = first to last - 1 do
                  integer.(at + j) <- data.(row array integer.((index * block_size) + j))
                done
            | _ -> V.fail "E_ARRAY_TYPE" "Integer lookup needs integer storage.")
        | Gather (array, index, width, component) ->
            let array = table array in
            let data =
              match (array, width) with
              | E.Float_array data, 1 | Vec2_array data, 2 | Vec3_array data, 3 | Vec4_array data, 4
                ->
                  data
              | _ -> V.fail "E_ARRAY_TYPE" "Float lookup changed its packed element width."
            in
            for j = first to last - 1 do
              let index = row array integer.((index * block_size) + j) in
              floats.(at + j) <- finite data.((index * width) + component)
            done
        | Int_const n -> fill_int (at + first) (last - first) n
        | Const x -> if first < last then fill_float (at + first) (last - first) (finite x)
        | Int_input i when contiguous -> (
            match (ranges.(i), inputs.(i)) with
            | Some _, _ ->
                for j = first to last - 1 do
                  integer.(at + j) <- start + j
                done
            | None, E.Int_array data ->
                if last = first + 1 then integer.(at + first) <- data.(start + first)
                else Array.blit data (start + first) integer (at + first) (last - first)
            | None, value ->
                for j = first to last - 1 do
                  integer.(at + j) <-
                    (match V.array_get value (start + j) with
                    | E.Int n -> n
                    | _ -> V.fail "E_ARRAY_TYPE" "Kernel input changed its packed element type.")
                done)
        | Int_input i ->
            for j = first to last - 1 do
              integer.(at + j) <-
                (match (ranges.(i), inputs.(i)) with
                | Some _, _ -> source j i
                | None, E.Int_array data -> data.(source j i)
                | None, value -> (
                    match V.array_get value (source j i) with
                    | E.Int n -> n
                    | _ -> V.fail "E_ARRAY_TYPE" "Kernel input changed its packed element type."))
            done
        | Input (i, width, component) -> (
            let load data =
              for j = first to last - 1 do
                let x = data.((source j i * width) + component) in
                if not (Float.is_finite x) then
                  V.fail "E_NONFINITE" "packed produced a nonfinite value.";
                floats.(at + j) <- x
              done
            in
            match (borrowed.(i), inputs.(i)) with
            | Some columns, _ ->
                let column = columns.(component) in
                for j = first to last - 1 do
                  let x = column.data.(column.offset + (source j i * column.stride)) in
                  if not (Float.is_finite x) then
                    V.fail "E_NONFINITE" "packed produced a nonfinite value.";
                  floats.(at + j) <- x
                done
            | None, E.Float_array data when width = 1 -> load data
            | None, E.Vec2_array data when width = 2 -> load data
            | None, E.Vec3_array data when width = 3 -> load data
            | None, E.Vec4_array data when width = 4 -> load data
            | None, value ->
                for j = first to last - 1 do
                  floats.(at + j) <-
                    finite (components (V.array_get value (source j i))).(component)
                done)
        | Int_uniform i -> (
            if first < last then
              match Lazy.force uniforms.(i) with
              | E.Int n -> fill_int (at + first) (last - first) n
              | _ -> V.fail "E_ARRAY_TYPE" "Kernel uniform changed its integer type.")
        | Int_frame _ -> (
            if first < last then
              match Lazy.force frame.(slot) with
              | E.Int n -> fill_int (at + first) (last - first) n
              | _ -> raise Unsupported)
        | Uniform (i, c) ->
            if first < last then
              fill_float (at + first) (last - first)
                (finite (Lazy.force uniform_components.(i)).(c))
        | Frame _ ->
            if first < last then
              fill_float (at + first) (last - first) (finite (V.num (Lazy.force frame.(slot))))
        | Int_accumulator -> (
            match !accumulator with
            | E.Int n -> fill_int (at + first) (last - first) n
            | _ -> assert false)
        | Accumulator c ->
            fill_float (at + first) (last - first) (finite (components !accumulator).(c))
        | Int_binary (op, a, b) ->
            let a = a * block_size and b = b * block_size in
            for j = first to last - 1 do
              integer.(at + j) <- Flow.Packed_ops.integer_apply op integer.(a + j) integer.(b + j)
            done
        | Int_compare (op, a, b) ->
            let a = a * block_size and b = b * block_size in
            for j = first to last - 1 do
              floats.(at + j) <-
                (if Flow.Packed_ops.comparison_apply op integer.(a + j) integer.(b + j) then 1.
                 else 0.)
            done
        | Int_abs a ->
            let a = a * block_size in
            for j = first to last - 1 do
              integer.(at + j) <- abs integer.(a + j)
            done
        | Convert (To_float, a) ->
            for j = first to last - 1 do
              floats.(at + j) <- finite (numeric a j)
            done
        | Convert (op, a) ->
            for j = first to last - 1 do
              integer.(at + j) <- convert_int op (numeric a j)
            done
        | Int_select (c, a, b) ->
            let a = a * block_size and b = b * block_size in
            for j = first to last - 1 do
              integer.(at + j) <- integer.((if numeric c j <> 0. then a else b) + j)
            done
        | Select (c, a, b) ->
            for j = first to last - 1 do
              floats.(at + j) <- finite (numeric (if numeric c j <> 0. then a else b) j)
            done
        | Binary (op, a, b) when (not banks.(a)) && not banks.(b) -> (
            let a = a * block_size and b = b * block_size in
            match op with
            | Add ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (floats.(a + j) +. floats.(b + j))
                done
            | Sub ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (floats.(a + j) -. floats.(b + j))
                done
            | Mul ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (floats.(a + j) *. floats.(b + j))
                done
            | Div ->
                for j = first to last - 1 do
                  let y = floats.(b + j) in
                  floats.(at + j) <- finite (if y = 0. then 0. else floats.(a + j) /. y)
                done
            | Mod ->
                for j = first to last - 1 do
                  let y = floats.(b + j) in
                  floats.(at + j) <-
                    finite (if y = 0. then 0. else Float.rem (Float.rem floats.(a + j) y +. y) y)
                done
            | Pow ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (Float.pow (Float.abs floats.(a + j)) floats.(b + j))
                done
            | Min ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (Float.min floats.(a + j) floats.(b + j))
                done
            | Max ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (Float.max floats.(a + j) floats.(b + j))
                done
            | Atan2 ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (Float.atan2 floats.(a + j) floats.(b + j))
                done
            | (Hypot | Ldexp) as o ->
                for j = first to last - 1 do
                  floats.(at + j) <- finite (binary_apply o floats.(a + j) floats.(b + j))
                done
            | Lt ->
                for j = first to last - 1 do
                  floats.(at + j) <- (if floats.(a + j) < floats.(b + j) then 1. else 0.)
                done
            | Le ->
                for j = first to last - 1 do
                  floats.(at + j) <- (if floats.(a + j) <= floats.(b + j) then 1. else 0.)
                done
            | Gt ->
                for j = first to last - 1 do
                  floats.(at + j) <- (if floats.(a + j) > floats.(b + j) then 1. else 0.)
                done
            | Ge ->
                for j = first to last - 1 do
                  floats.(at + j) <- (if floats.(a + j) >= floats.(b + j) then 1. else 0.)
                done
            | Eq ->
                for j = first to last - 1 do
                  floats.(at + j) <- (if floats.(a + j) = floats.(b + j) then 1. else 0.)
                done
            | And ->
                for j = first to last - 1 do
                  floats.(at + j) <-
                    (if floats.(a + j) <> 0. && floats.(b + j) <> 0. then 1. else 0.)
                done
            | Or ->
                for j = first to last - 1 do
                  floats.(at + j) <-
                    (if floats.(a + j) <> 0. || floats.(b + j) <> 0. then 1. else 0.)
                done)
        | Fma (a, b, c) when (not banks.(a)) && (not banks.(b)) && not banks.(c) ->
            let a = a * block_size and b = b * block_size and c = c * block_size in
            for j = first to last - 1 do
              floats.(at + j) <- finite (Float.fma floats.(a + j) floats.(b + j) floats.(c + j))
            done
        | Checked_mul (p, a, b) when (not banks.(a)) && not banks.(b) -> (
            let a = a * block_size and b = b * block_size in
            match p with
            | Value ->
                for j = first to last - 1 do
                  let a = finite floats.(a + j) and b = finite floats.(b + j) in
                  floats.(at + j) <- checked_projection Value (Sys.opaque_identity (a *. b))
                done
            | Valid ->
                for j = first to last - 1 do
                  let a = finite floats.(a + j) and b = finite floats.(b + j) in
                  floats.(at + j) <- checked_projection Valid (Sys.opaque_identity (a *. b))
                done)
        | Checked_fma (p, a, b, c) when (not banks.(a)) && (not banks.(b)) && not banks.(c) -> (
            let a = a * block_size and b = b * block_size and c = c * block_size in
            match p with
            | Value ->
                for j = first to last - 1 do
                  let a = finite floats.(a + j)
                  and b = finite floats.(b + j)
                  and c = finite floats.(c + j) in
                  floats.(at + j) <- checked_projection Value (Float.fma a b c)
                done
            | Valid ->
                for j = first to last - 1 do
                  let a = finite floats.(a + j)
                  and b = finite floats.(b + j)
                  and c = finite floats.(c + j) in
                  floats.(at + j) <- checked_projection Valid (Float.fma a b c)
                done)
        | Binary (op, a, b) ->
            for j = first to last - 1 do
              floats.(at + j) <- finite (binary_apply op (numeric a j) (numeric b j))
            done
        | Fma (a, b, c) ->
            for j = first to last - 1 do
              floats.(at + j) <- finite (Float.fma (numeric a j) (numeric b j) (numeric c j))
            done
        | Checked_mul (p, a, b) ->
            for j = first to last - 1 do
              let a = finite (numeric a j) and b = finite (numeric b j) in
              floats.(at + j) <- checked_projection p (Sys.opaque_identity (a *. b))
            done
        | Checked_fma (p, a, b, c) ->
            for j = first to last - 1 do
              let a = finite (numeric a j)
              and b = finite (numeric b j)
              and c = finite (numeric c j) in
              floats.(at + j) <- checked_projection p (Float.fma a b c)
            done
        | Trace (field, r) ->
            for j = first to last - 1 do
              floats.(at + j) <-
                finite
                  (trace_field field
                     (trace_hit traced j
                        (int_of_float (numeric r.(0) j))
                        (Array.init 9 (fun k -> numeric r.(k + 1) j))))
            done
        | Stage _ -> stage_refused ()
        | Rand_at (seed, index) ->
            let seed = seed * block_size and index = index * block_size in
            for j = first to last - 1 do
              floats.(at + j) <- Flow.Packed_ops.rand_at integer.(seed + j) integer.(index + j)
            done
        | Unary (op, a) ->
            for j = first to last - 1 do
              floats.(at + j) <- finite (unary_apply op (numeric a j))
            done
        | Noise3 (x, y, z, seed, octaves, lacunarity, gain) ->
            for j = first to last - 1 do
              floats.(at + j) <-
                finite
                  (Rays_math.Noise.fbm3_with_scratch scratch (noise seed) ~octaves ~lacunarity ~gain
                     ~x:(numeric x j) ~y:(numeric y j) ~z:(numeric z j))
            done
      and execute_range ?dependent first_slot last_slot first last =
        if carried = 0 then
          match dependent with
          | None ->
              for slot = first_slot to last_slot - 1 do
                execute slot t.code.(slot) first last
              done
          | Some value ->
              for slot = first_slot to last_slot - 1 do
                if dependent_flags.(slot) = value then execute slot t.code.(slot) first last
              done
        else begin
          let slot = ref first_slot in
          while !slot < last_slot do
            let instruction = t.code.(!slot) in
            if match dependent with None -> true | Some value -> dependent_flags.(!slot) = value
            then execute !slot instruction first last;
            slot :=
              match unguarded instruction with Loop_begin loop -> loop.finish + 1 | _ -> !slot + 1
          done
        end
      in
      let execute_all dependent first last =
        execute_range ~dependent 0 (Array.length t.code) first last
      in
      if gather && t.result = Collect then begin
        (* Speculate: run the block instruction-major. Without an error the values equal
           the lane-major ones, because lanes are independent and instructions pure; on
           any exception the block is redone lane-major below, so the first error in lane
           order is the one reported. Spans cached from a failed speculation may hold
           garbage guards, so the cache is dropped for the redo. *)
        let speculated =
          try
            execute_range 0 (Array.length t.code) 0 length;
            true
          with Diagnostic.Error _ | Invalid_argument _ | Not_found | Failure _ -> false
        in
        if not speculated then begin
          guard_spans := None;
          for slot = 0 to total_prefix - 1 do
            execute slot t.code.(slot) 0 length
          done;
          let cached i ty =
            match Atomic.get prepared.(i) with
            | Some value when V.ty_of value = ty -> Some value
            | _ -> None
          in
          let cached_table register =
            match unguarded t.code.(register) with
            | Array_prepare (i, ty) -> cached i ty
            | _ -> None
          in
          let valid_indices guards index count =
            let j = ref 0 in
            while
              !j < length
              && ((not (selected !j guards))
                 ||
                 let index = integer.((index * block_size) + !j) in
                 index >= 0 && index <= Sys.max_array_length && index < count)
            do
              incr j
            done;
            !j = length
          in
          let rec safe guards = function
            | Guarded (more, body) -> safe (if guards = [] then more else guards @ more) body
            | Array_prepare (i, ty) -> cached i ty <> None
            | Text_gather (array, index) -> (
                match cached_table array with
                | Some (E.Text_array data) -> valid_indices guards index (Array.length data)
                | _ -> false)
            | Int_gather (array, index) -> (
                match cached_table array with
                | Some (E.Int_array data) -> valid_indices guards index (Array.length data)
                | _ -> false)
            | Gather (array, index, width, _) -> (
                match (cached_table array, width) with
                | Some (E.Float_array data), 1
                | Some (E.Vec2_array data), 2
                | Some (E.Vec3_array data), 3
                | Some (E.Vec4_array data), 4 ->
                    Array.length data mod width = 0
                    && valid_indices guards index (Array.length data / width)
                | _ -> false)
            | body -> total body
          in
          (* Only already published, validated tables can extend the prefix.
           A failed bounds proof leaves the whole remaining suffix lane-major. *)
          let prefix = ref total_prefix in
          while !prefix < Array.length t.code && safe [] t.code.(!prefix) do
            (match unguarded t.code.(!prefix) with
            | Array_prepare _ -> () (* Publication already completed its only effect. *)
            | _ -> execute !prefix t.code.(!prefix) 0 length);
            incr prefix
          done;
          for j = 0 to length - 1 do
            execute_range !prefix (Array.length t.code) j (j + 1)
          done
        end
      end
      else if not gather then execute_all false 0 length;
      (* Instructions validate selected float results before collection. *)
      if t.result = Collect then
        Array.iteri
          (fun k registers ->
            let output_index = start - before in
            match (plane_outputs, outputs.(k)) with
            | None, E.Text_array data ->
                Array.blit texts
                  (text_offsets.(registers.(0)) * block_size)
                  data output_index length
            | None, E.Int_array data ->
                Array.blit integer (registers.(0) * block_size) data output_index length
            | None, E.Float_array data when not banks.(registers.(0)) ->
                Array.blit floats (registers.(0) * block_size) data output_index length
            | Some outputs, _ ->
                Array.iteri
                  (fun c r ->
                    Option.iter
                      (fun data ->
                        if banks.(r) then
                          for j = 0 to length - 1 do
                            data.(output_index + j) <- float integer.((r * block_size) + j)
                          done
                        else Array.blit floats (r * block_size) data output_index length)
                      outputs.(k).(c))
                  registers
            | None, (E.Float_array data | Vec2_array data | Vec3_array data | Vec4_array data) ->
                let width = Array.length registers in
                Array.iteri
                  (fun c r ->
                    let at = r * block_size in
                    if banks.(r) then
                      for j = 0 to length - 1 do
                        data.(((output_index + j) * width) + c) <- float integer.(at + j)
                      done
                    else
                      for j = 0 to length - 1 do
                        data.(((output_index + j) * width) + c) <- floats.(at + j)
                      done)
                  registers
            | _ -> assert false)
          t.outputs
      else
        for j = 0 to length - 1 do
          if gather then execute_range 0 (Array.length t.code) j (j + 1)
          else if t.result <> Collect then execute_all true j (j + 1);
          let output_index = start - before + j in
          for k = 0 to Array.length t.outputs - 1 do
            let registers = t.outputs.(k) in
            if t.result <> Collect then begin
              let value =
                if integer_outputs.(k) then E.Int integer.((registers.(0) * block_size) + j)
                else
                  let xs = Array.map (fun r -> finite (numeric r j)) registers in
                  match xs with
                  | [| x; y |] -> E.Vec2 (x, y)
                  | [| x; y; z |] -> E.Vec3 (x, y, z)
                  | [| x; y; z; w |] -> E.Vec4 (x, y, z, w)
                  | _ -> E.Float xs.(0)
              in
              accumulator :=
                match t.result with
                | Collect -> assert false
                | Accumulate _ -> value
                | Sum _ -> (
                    if not !initialized then value
                    else
                      match (!accumulator, value) with
                      | E.Int a, E.Int b when t.result = Sum true ->
                          E.Int (Flow.Packed_ops.integer_apply Iadd a b)
                      | a, b -> V.arith "array/sum" ( +. ) a b)
            end;
            if collecting then
              match (plane_outputs, outputs.(k)) with
              | Some outputs, _ ->
                  Array.iteri
                    (fun c r ->
                      Option.iter
                        (fun data -> data.(output_index) <- finite (numeric r j))
                        outputs.(k).(c))
                    registers
              | None, E.Int_array data ->
                  data.(output_index) <- integer.((registers.(0) * block_size) + j)
              | None, (E.Float_array data | Vec2_array data | Vec3_array data | Vec4_array data) ->
                  for c = 0 to Array.length registers - 1 do
                    data.((output_index * Array.length registers) + c) <-
                      finite (numeric registers.(c) j)
                  done
              | _ -> assert false
          done;
          if t.result <> Collect then initialized := true
        done
    done
  in
  (* A Cartesian projection repeats one checked row. Its unused outer input
     is concrete integers or a checked range; removing those loads cannot hide
     a type or finite-value error. Other programs use the block loop. *)
  let repeated_row =
    match (t.iteration, t.result, skip, inputs, t.code, t.outputs) with
    | ( Product,
        Collect,
        [||],
        [| outer; E.Float_array row |],
        [| Int_input 0; Input (1, 1, 0) |],
        [| [| 1 |] |] )
    | ( Product,
        Collect,
        [||],
        [| outer; E.Float_array row |],
        [| Input (1, 1, 0); Int_input 0 |],
        [| [| 0 |] |] )
      when match (ranges.(0), outer) with Some _, _ | None, E.Int_array _ -> true | _ -> false ->
        Some (lengths.(0), row)
    | _ -> None
  in
  (* Exact integer scaling needs one pass and no temporary register banks. *)
  let scaled_column =
    match (t.iteration, t.result, skip, inputs, t.code, t.outputs, outputs) with
    | ( Zip,
        Collect,
        [||],
        [| input |],
        ( [| Int_input 0; Int_const factor; Int_binary (Imul, 0, 1) |]
        | [| Int_const factor; Int_input 0; Int_binary (Imul, 0, 1) |] ),
        [| [| 2 |] |],
        [| E.Int_array output |] )
      when match (ranges.(0), input) with Some _, _ | None, E.Int_array _ -> true | _ -> false ->
        Some (input, factor, output)
    | _ -> None
  in
  (match repeated_row with
  | Some (outer_count, row) ->
      if count > 0 then begin
        Array.iter
          (fun x ->
            if not (Float.is_finite x) then
              V.fail "E_NONFINITE" "packed produced a nonfinite value.")
          row;
        let output =
          match (plane_outputs, outputs.(0)) with
          | Some columns, _ -> columns.(0).(0)
          | None, E.Float_array data -> Some data
          | _ -> assert false
        in
        Option.iter
          (fun data ->
            Parallel.for_ ~chunk_size:4096 ~start:0 ~finish:(outer_count - 1) (fun i ->
                Array.blit row 0 data (i * Array.length row) (Array.length row)))
          output
      end
  | None -> (
      match scaled_column with
      | Some (E.Int_array input, factor, output) ->
          Parallel.for_ ~chunk_size:4096 ~start:0 ~finish:(count - 1) (fun i ->
              output.(i) <- input.(i) * factor)
      | Some (_, factor, output) ->
          Parallel.for_ ~chunk_size:4096 ~start:0 ~finish:(count - 1) (fun i ->
              output.(i) <- i * factor)
      | None ->
          (* Lazy captures cannot be forced from a worker domain. The first chunk runs
         here, in reference order, and forces whichever captures its lanes select;
         when every capture is then a value the remaining chunks run in parallel. *)
          let settled () =
            Array.for_all (fun lazy_value -> Lazy.is_val lazy_value) uniforms
            && (try
                  Array.iteri
                    (fun i lazy_value ->
                      if Lazy.is_val lazy_value then ignore (Lazy.force uniform_components.(i)))
                    uniforms;
                  true
                with Diagnostic.Error _ | Invalid_argument _ | Not_found | Failure _ -> false)
            && Array.for_all (fun lazy_value -> Lazy.is_val lazy_value) uniform_components
          in
          let first_chunk =
            if chunks = 0 || Array.for_all concrete t.uniforms then 0
            else (
              chunk 0;
              1)
          in
          if t.result = Collect && count >= 4096 && (first_chunk = 0 || settled ()) then
            if gather then begin
              let errors = Array.make chunks None in
              Parallel.for_ ~chunk_size:1 ~start:first_chunk ~finish:(chunks - 1) (fun i ->
                  try chunk i with exn -> errors.(i) <- Some exn);
              Array.iter (function None -> () | Some exn -> raise exn) errors
            end
            else Parallel.for_ ~chunk_size:1 ~start:first_chunk ~finish:(chunks - 1) chunk
          else
            for i = first_chunk to chunks - 1 do
              chunk i
            done));
  if plane_outputs <> None then []
  else if collecting then Array.to_list outputs
  else
    [
      (if kept = 0 then
         match (t.result, seed) with
         | Accumulate _, Some seed -> seed
         | Sum false, _ -> E.Int 0
         | _ -> !accumulator
       else !accumulator);
    ]

let ordered_add (t : t) =
  let output = t.outputs.(0) in
  let width = Array.length output in
  match t.result with
  | Accumulate (_, false)
    when width >= 1 && width <= 4 && t.iteration = Zip
         && Array.length t.sources = 1
         && t.widths = [| width |]
         && Array.length t.skip = 0
         && Array.length t.code = 3 * width
         && Array.length (Core.Region.dependent_flags t) = 3 * width ->
      let dependent = Core.Region.dependent_flags t and matched = ref true in
      for k = 0 to width - 1 do
        if
          t.code.(k) <> Input (0, width, k)
          || dependent.(k)
          || t.code.(width + k) <> Accumulator k
          || (not dependent.(width + k))
          || t.code.((2 * width) + k) <> Binary (Add, width + k, k)
          || (not dependent.((2 * width) + k))
          || output.(k) <> (2 * width) + k
        then matched := false
      done;
      !matched
  | _ -> false

(* Within one outermost run, a lazily forced capture is evaluated again by every region and capture that
   reads it (a map feeding a minimum, a maximum and the map that scales by both ran three times). A residual
   term is a function of its captured values, the frame, the host handles behind [resolve] and the frame
   state, so the results of one outermost run are shared between its nested evaluations when all four are
   physically the same and the term reads no state, and dropped when that run ends. *)
module Scope = struct
  type t = {
    resolve : (E.value -> (E.value, Diagnostic.t) Stdlib.result) option;
    live : Input.t;
    state : E.state;
    strict : bool;
    mutable depth : int;
    mutable entries : (E.residual * W.term * E.value) list;
    mutable count : int;
  }

  let capacity = 32
  let current : t option ref Domain.DLS.key = Domain.DLS.new_key (fun () -> ref None)

  let active : (W.term * E.Residual.residual_view) list Domain.DLS.key =
    Domain.DLS.new_key (fun () -> [])

  let requests : (W.term * E.Residual.residual_view) list Domain.DLS.key =
    Domain.DLS.new_key (fun () -> [])

  let same_context (view : E.Residual.residual_view) (other : E.Residual.residual_view) =
    view.instance = other.instance && view.site = other.site && view.prefix = other.prefix
    && view.owner = other.owner
    && List.equal (fun (a, x) (b, y) -> String.equal a b && x == y) view.bindings other.bindings

  let request_active term (view : E.Residual.residual_view) =
    List.exists
      (fun (other_term, (other : E.Residual.residual_view)) ->
        term == other_term && same_context view other)
      (Domain.DLS.get requests)

  let with_request term view f =
    let active = Domain.DLS.get requests in
    Domain.DLS.set requests ((term, view) :: active);
    Fun.protect ~finally:(fun () -> Domain.DLS.set requests active) f

  let same_resolve a b =
    match (a, b) with None, None -> true | Some a, Some b -> a == b | _ -> false

  let enter ~resolve ~live ~state ~strict =
    let current = Domain.DLS.get current in
    match !current with
    | Some scope
      when same_resolve scope.resolve resolve
           && scope.live == live && scope.state == state && scope.strict = strict ->
        scope.depth <- scope.depth + 1;
        Some scope
    | Some _ -> None
    | None ->
        let scope = { resolve; live; state; strict; depth = 1; entries = []; count = 0 } in
        current := Some scope;
        Some scope

  let leave = function
    | None -> ()
    | Some scope ->
        scope.depth <- scope.depth - 1;
        if scope.depth = 0 then Domain.DLS.get current := None

  (* The same residual can be met as distinct records, under distinct ids (a walker's kernel request
     numbers each one afresh), so identity is the same term, bindings (physically) and site. *)
  let same_residual a b =
    a == b
    ||
    let x = E.Residual.residual_view a and y = E.Residual.residual_view b in
    x.term == y.term && x.previous = y.previous && x.instance = y.instance && x.iter = y.iter
    && x.prefix = y.prefix && x.owner = y.owner && x.host = y.host && x.geometry = y.geometry
    && List.equal (fun (n, v) (m, w) -> String.equal n m && v == w) x.bindings y.bindings

  let find scope residual term =
    List.find_map
      (fun (r, t, v) -> if t == term && same_residual r residual then Some v else None)
      scope.entries

  let add scope residual term value =
    if scope.count < capacity then begin
      scope.entries <- (residual, term, value) :: scope.entries;
      scope.count <- scope.count + 1
    end
end

exception Not_streaming

(* Select the narrower streaming loops only when every register belongs to the supported
   noise/scale/output shape. Other programs stay on the general block executor. *)
let detect_streaming (t : t) skip noise_scale collect_product =
  let streaming =
    try
      if t.result <> Collect || t.iteration <> Zip || skip <> [||] then raise Not_streaming;
      let allowed =
        Array.map (function Input _ | Const _ | Uniform _ | Frame _ -> true | _ -> false) t.code
      in
      let input id =
        match t.code.(id) with
        | Input (source, width, component) -> (source, width, component)
        | _ -> raise Not_streaming
      in
      let scalar id =
        match t.code.(id) with Const _ | Uniform _ | Frame _ -> id | _ -> raise Not_streaming
      in
      let shared = ref None in
      let products =
        Array.mapi
          (fun k output ->
            Array.mapi
              (fun component register ->
                let base, a, b =
                  match collect_product.(k).(component) with
                  | Some p -> p
                  | None -> raise Not_streaming
                in
                let direction, scale =
                  match (t.code.(a), t.code.(b)) with
                  | Input _, _ -> (input a, b)
                  | _, Input _ -> (input b, a)
                  | _ -> raise Not_streaming
                in
                (match !shared with
                | None -> shared := Some scale
                | Some old when old = scale -> ()
                | _ -> raise Not_streaming);
                allowed.(register) <- true;
                (match t.code.(register) with
                | Binary (Add, x, y) -> allowed.(if x = base then y else x) <- true
                | _ -> raise Not_streaming);
                (input base, direction))
              output)
          t.outputs
      in
      let scale = match !shared with Some scale -> scale | None -> raise Not_streaming in
      let noise, amplitude =
        match t.code.(scale) with
        | Binary (Mul, a, b) -> (
            match (t.code.(a), t.code.(b)) with
            | Noise3 _, _ -> (a, scalar b)
            | _, Noise3 _ -> (b, scalar a)
            | _ -> raise Not_streaming)
        | _ -> raise Not_streaming
      in
      allowed.(scale) <- true;
      allowed.(noise) <- true;
      let x, y, z, frequency =
        match t.code.(noise) with
        | Noise3 (x, y, z, _, 1, _, _) -> (
            match noise_scale.(noise) with
            | Some (a, b, c, f) ->
                allowed.(x) <- true;
                allowed.(y) <- true;
                allowed.(z) <- true;
                (input a, input b, input c, Some (scalar f))
            | None -> (input x, input y, input z, None))
        | _ -> raise Not_streaming
      in
      if not (Array.for_all Fun.id allowed) then raise Not_streaming;
      Some (noise, amplitude, x, y, z, frequency, products)
    with Not_streaming -> None
  in
  let scalar_streaming =
    try
      if t.result <> Collect || t.iteration <> Zip || skip <> [||] || Option.is_some streaming then
        raise Not_streaming;
      let allowed =
        Array.map (function Input _ | Const _ | Uniform _ | Frame _ -> true | _ -> false) t.code
      in
      let input id =
        match t.code.(id) with Input (i, w, c) -> (i, w, c) | _ -> raise Not_streaming
      in
      let scalar id =
        match t.code.(id) with Const _ | Uniform _ | Frame _ -> id | _ -> raise Not_streaming
      in
      let scaled id =
        match t.code.(id) with
        | Binary (Mul, a, b) -> (
            match (t.code.(a), t.code.(b)) with
            | Input _, _ ->
                allowed.(id) <- true;
                (input a, Some (scalar b))
            | _, Input _ ->
                allowed.(id) <- true;
                (input b, Some (scalar a))
            | _ -> raise Not_streaming)
        | Input _ -> (input id, None)
        | _ -> raise Not_streaming
      in
      let shared = ref None in
      let outputs =
        Array.map
          (Array.map (fun id ->
               match t.code.(id) with
               | Input _ -> (input id, None)
               | Binary (Add, a, b) ->
                   let base, expression =
                     match (t.code.(a), t.code.(b)) with
                     | Input _, _ -> (input a, b)
                     | _, Input _ -> (input b, a)
                     | _ -> raise Not_streaming
                   in
                   (match !shared with
                   | None -> shared := Some expression
                   | Some old when old = expression -> ()
                   | _ -> raise Not_streaming);
                   allowed.(id) <- true;
                   (base, Some expression)
               | _ -> raise Not_streaming))
          t.outputs
      in
      let root = match !shared with Some id -> id | None -> raise Not_streaming in
      let transform, a =
        match t.code.(root) with
        | Binary (Mul, a, b) -> (
            match (t.code.(a), t.code.(b)) with
            | Binary (Sub, _, _), _ -> (a, scalar b)
            | _, Binary (Sub, _, _) -> (b, scalar a)
            | _ -> raise Not_streaming)
        | _ -> raise Not_streaming
      in
      let gain, bias =
        match t.code.(transform) with
        | Binary (Sub, a, b) -> (a, scalar b)
        | _ -> raise Not_streaming
      in
      let noise, g =
        match t.code.(gain) with
        | Binary (Mul, a, b) -> (
            match (t.code.(a), t.code.(b)) with
            | Noise3 _, _ -> (a, scalar b)
            | _, Noise3 _ -> (b, scalar a)
            | _ -> raise Not_streaming)
        | _ -> raise Not_streaming
      in
      allowed.(root) <- true;
      allowed.(transform) <- true;
      allowed.(gain) <- true;
      allowed.(noise) <- true;
      let x, y, z, f =
        match t.code.(noise) with
        | Noise3 (x, y, z, _, 1, _, _) ->
            let x, f = scaled x and y, h = scaled y in
            if f <> h then raise Not_streaming;
            let z =
              match t.code.(z) with
              | Const z when z = 0. -> None
              | _ ->
                  let z, h = scaled z in
                  if h <> f then raise Not_streaming;
                  Some z
            in
            (x, y, z, f)
        | _ -> raise Not_streaming
      in
      if not (Array.for_all Fun.id allowed) then raise Not_streaming;
      Some (noise, x, y, z, f, g, bias, a, outputs)
    with Not_streaming -> None
  in
  (streaming, scalar_streaming)

(* What one kernel launch has settled before its chunks run (run_all) and what one block of a chunk
   owns; the chunk, block and instruction classes below read these instead of closing over run_all. *)
type launch = {
  t : t;
  inputs : float array array;
  borrowed : column array option array;
  lengths : int array;
  strides : int array;
  columns : column array array;
  uniforms : float array array;
  frame : float array;
  widths : int array;
  width : int;
  outputs : float array array;
  plane_outputs : float array option array array option;
  skip : int array;
  count : int;
  collecting : bool;
  accumulator : float array;
  initialized : bool ref;
  ordered_add : bool;
  streaming :
    (int
    * int
    * (int * int * int)
    * (int * int * int)
    * (int * int * int)
    * int option
    * ((int * int * int) * (int * int * int)) array array)
    option;
  scalar_streaming :
    (int
    * (int * int * int)
    * (int * int * int)
    * (int * int * int) option
    * int option
    * int
    * int
    * int
    * ((int * int * int) * int option) array array)
    option;
  block_size : int;
  blocks : int;
  blocks_per_chunk : int;
  dependent_flags : bool array;
  omitted : bool array;
  skipped_before : int -> int;
  noise_scale : (int * int * int * int) option array;
  collect_product : (int * int * int) option array array;
}

type block = {
  scratch : float array;
  noise_scratch : Rays_math.Noise.fbm3_scratch;
  tables : Rays_math.Noise.t option array;
  contiguous : bool;
  indices : int array;
  start : int;
  length : int;
  output_start : int;
}

(* A summed or accumulated chunk whose additions stay in reference order. *)
let chunk_ordered r chunk =
  let { t; inputs; width; accumulator; blocks_per_chunk; block_size; count; _ } = r in
  let source = inputs.(0) in
  let first = chunk * blocks_per_chunk * block_size
  and last = min count ((chunk + 1) * blocks_per_chunk * block_size) - 1 in
  if width = 3 then begin
    let x = ref accumulator.(0) and y = ref accumulator.(1) and z = ref accumulator.(2) in
    for i = first to last do
      let offset = i * 3 in
      let next = !x +. source.(offset) in
      if not (Float.is_finite next) then nonfinite ();
      x := next;
      let next = !y +. source.(offset + 1) in
      if not (Float.is_finite next) then nonfinite ();
      y := next;
      let next = !z +. source.(offset + 2) in
      if not (Float.is_finite next) then nonfinite ();
      z := next
    done;
    accumulator.(0) <- !x;
    accumulator.(1) <- !y;
    accumulator.(2) <- !z
  end
  else
    for i = first to last do
      for component = 0 to width - 1 do
        let x = accumulator.(component) +. source.((i * width) + component) in
        if not (Float.is_finite x) then nonfinite ();
        accumulator.(component) <- x
      done
    done

(* One instruction over lanes first..last of a block. *)
let rec block_instruction r b slot instruction first last =
  let {
    t;
    inputs;
    borrowed;
    lengths;
    strides;
    columns;
    uniforms;
    frame;
    skip;
    accumulator;
    noise_scale;
    block_size;
    _;
  } =
    r
  in
  let { scratch; noise_scratch; tables; contiguous; indices; start; _ } = b in
  let[@inline always] load input component index =
    let column = columns.(input).(component) in
    Array.unsafe_get column.data (column.offset + (index * column.stride))
  in
  let at = slot * block_size in
  match instruction with
  | Guarded (conditions, body) ->
      let rec selected j = function
        | [] -> true
        | (register, yes) :: rest ->
            scratch.((register * block_size) + j) <> 0. = yes && selected j rest
      in
      let next = ref first in
      while !next < last do
        while !next < last && not (selected !next conditions) do
          incr next
        done;
        let first = !next in
        while !next < last && selected !next conditions do
          incr next
        done;
        if first < !next then block_instruction r b slot body first !next
      done
  | Int_const n -> Array.fill scratch (at + first) (last - first) (float n)
  | Const x -> Array.fill scratch (at + first) (last - first) x
  | Array_prepare _ | Gather _ | Int_gather _ | Loop_begin _ | Loop_carry _ | Loop_end _
  | Loop_result _ ->
      raise Unsupported
  | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _ ->
      raise Unsupported
  | Input (input, width, component) ->
      let source = inputs.(input) in
      if borrowed.(input) <> None then
        for j = first to last - 1 do
          let i =
            if contiguous then start + j
            else if t.iteration = Zip then indices.(j)
            else indices.(j) / strides.(input) mod lengths.(input)
          in
          Array.unsafe_set scratch (at + j) (load input component i)
        done
      else if t.iteration = Zip && skip = [||] then
        for j = first to last - 1 do
          (* Layouts were checked by [Core.Region.make], and Zip's count is the
                   shortest complete input. These spans are within both arrays. *)
          Array.unsafe_set scratch (at + j)
            (Array.unsafe_get source (((start + j) * width) + component))
        done
      else
        for j = first to last - 1 do
          let i =
            if t.iteration = Zip then indices.(j)
            else indices.(j) / strides.(input) mod lengths.(input)
          in
          scratch.(at + j) <- source.((i * width) + component)
        done
  | Uniform (uniform, component) ->
      Array.fill scratch (at + first) (last - first) uniforms.(uniform).(component)
  | Frame _ -> Array.fill scratch (at + first) (last - first) frame.(slot)
  | Accumulator component ->
      for j = first to last - 1 do
        scratch.(at + j) <- accumulator.(component)
      done
  | Select (condition, yes, no) ->
      let condition = condition * block_size and yes = yes * block_size and no = no * block_size in
      for j = first to last - 1 do
        scratch.(at + j) <- scratch.((if scratch.(condition + j) <> 0. then yes else no) + j)
      done
  | Noise3 (x, y, z, _, octaves, lacunarity, gain) ->
      let noise = Option.get tables.(slot) in
      let x, y, z, frequency =
        match noise_scale.(slot) with
        | Some (x, y, z, f) -> (x, y, z, scratch.((f * block_size) + first))
        | None -> (x, y, z, 1.)
      in
      if octaves = 1 then
        Rays_math.Noise.sample3_into noise ~first ~last ~frequency ~x:scratch
          ~x_offset:(x * block_size) ~y:scratch ~y_offset:(y * block_size) ~z:scratch
          ~z_offset:(z * block_size) ~output:scratch ~output_offset:at ()
      else
        for j = first to last - 1 do
          scratch.(at + j) <-
            Rays_math.Noise.fbm3_with_scratch noise_scratch noise ~octaves ~lacunarity ~gain
              ~x:(scratch.((x * block_size) + j) *. frequency)
              ~y:(scratch.((y * block_size) + j) *. frequency)
              ~z:(scratch.((z * block_size) + j) *. frequency)
        done;
      if noise_scale.(slot) <> None then
        for j = first to last - 1 do
          if not (Float.is_finite scratch.(at + j)) then nonfinite ()
        done
  | Binary (operation, a, b) -> (
      let a = a * block_size and b = b * block_size in
      match operation with
      | Add ->
          for j = first to last - 1 do
            let x = Array.unsafe_get scratch (a + j) +. Array.unsafe_get scratch (b + j) in
            if not (Float.is_finite x) then nonfinite ();
            Array.unsafe_set scratch (at + j) x
          done
      | Sub ->
          for j = first to last - 1 do
            let x = scratch.(a + j) -. scratch.(b + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Mul ->
          for j = first to last - 1 do
            let x = Array.unsafe_get scratch (a + j) *. Array.unsafe_get scratch (b + j) in
            if not (Float.is_finite x) then nonfinite ();
            Array.unsafe_set scratch (at + j) x
          done
      | Div ->
          for j = first to last - 1 do
            let y = scratch.(b + j) in
            let x = if y = 0. then 0. else scratch.(a + j) /. y in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Mod ->
          for j = first to last - 1 do
            let y = scratch.(b + j) in
            let x = if y = 0. then 0. else Float.rem (Float.rem scratch.(a + j) y +. y) y in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Pow ->
          for j = first to last - 1 do
            let x = Float.pow (Float.abs scratch.(a + j)) scratch.(b + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Min ->
          for j = first to last - 1 do
            let x = Float.min scratch.(a + j) scratch.(b + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Max ->
          for j = first to last - 1 do
            let x = Float.max scratch.(a + j) scratch.(b + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Atan2 | Hypot | Ldexp ->
          for j = first to last - 1 do
            let x = binary_apply operation scratch.(a + j) scratch.(b + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Lt | Le | Gt | Ge | Eq | And | Or ->
          for j = first to last - 1 do
            let x = scratch.(a + j) and y = scratch.(b + j) in
            let result =
              match operation with
              | Lt -> x < y
              | Le -> x <= y
              | Gt -> x > y
              | Ge -> x >= y
              | Eq -> x = y
              | And -> x <> 0. && y <> 0.
              | Or -> x <> 0. || y <> 0.
              | _ -> assert false
            in
            scratch.(at + j) <- (if result then 1. else 0.)
          done)
  | Fma (a, b, c) ->
      let a = a * block_size and b = b * block_size and c = c * block_size in
      for j = first to last - 1 do
        let value = Float.fma scratch.(a + j) scratch.(b + j) scratch.(c + j) in
        if not (Float.is_finite value) then nonfinite ();
        scratch.(at + j) <- value
      done
  | Checked_mul (p, a, b) ->
      let a = a * block_size and b = b * block_size in
      for j = first to last - 1 do
        let a = scratch.(a + j) and b = scratch.(b + j) in
        if not (Float.is_finite a && Float.is_finite b) then nonfinite ();
        scratch.(at + j) <- checked_projection p (Sys.opaque_identity (a *. b))
      done
  | Checked_fma (p, a, b, c) ->
      let a = a * block_size and b = b * block_size and c = c * block_size in
      for j = first to last - 1 do
        let a = scratch.(a + j) and b = scratch.(b + j) and c = scratch.(c + j) in
        if not (Float.is_finite a && Float.is_finite b && Float.is_finite c) then nonfinite ();
        scratch.(at + j) <- checked_projection p (Float.fma a b c)
      done
  | Unary (operation, a) -> (
      let a = a * block_size in
      match operation with
      | Sin ->
          for j = first to last - 1 do
            let x = sin scratch.(a + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Cos ->
          for j = first to last - 1 do
            let x = cos scratch.(a + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Sqrt ->
          for j = first to last - 1 do
            scratch.(at + j) <- sqrt (Float.abs scratch.(a + j))
          done
      | Abs ->
          for j = first to last - 1 do
            scratch.(at + j) <- Float.abs scratch.(a + j)
          done
      | Tan | Asin | Acos | Exp | Log | Sign | Rsqrt ->
          for j = first to last - 1 do
            let x = unary_apply operation scratch.(a + j) in
            if not (Float.is_finite x) then nonfinite ();
            scratch.(at + j) <- x
          done
      | Not ->
          for j = first to last - 1 do
            scratch.(at + j) <- (if scratch.(a + j) = 0. then 1. else 0.)
          done)
  | Int_input _ | Int_uniform _ | Int_frame _ | Int_accumulator | Int_binary _ | Int_compare _
  | Int_abs _ | Convert _ | Int_select _ | Raise _ | Rand_at _ | Trace _ | Stage _ ->
      raise Unsupported

let block_execute r b dependent first last =
  for slot = 0 to Array.length r.t.code - 1 do
    if r.dependent_flags.(slot) = dependent && not r.omitted.(slot) then
      block_instruction r b slot r.t.code.(slot) first last
  done

(* Noise3 scaled by one amplitude and added to a base: sampled straight into the outputs. *)
let block_streaming r b
    (noise, amplitude, (xi, _, xc), (yi, _, yc), (zi, _, zc), frequency, products) =
  let { t; inputs; columns; uniforms; frame; outputs; plane_outputs; block_size; _ } = r in
  let { scratch; tables; start; length; output_start; _ } = b in
  let scalar id =
    match t.code.(id) with
    | Const x -> x
    | Uniform (i, c) -> uniforms.(i).(c)
    | Frame _ -> frame.(id)
    | _ -> assert false
  in
  let x = columns.(xi).(xc) and y = columns.(yi).(yc) and z = columns.(zi).(zc) in
  Rays_math.Noise.sample3_into
    (Option.get tables.(noise))
    ~first:0 ~last:length ~x:x.data
    ~x_offset:(x.offset + (start * x.stride))
    ~x_stride:x.stride ~y:y.data
    ~y_offset:(y.offset + (start * y.stride))
    ~y_stride:y.stride ~z:z.data
    ~z_offset:(z.offset + (start * z.stride))
    ~z_stride:z.stride
    ~frequency:(Option.fold ~none:1. ~some:scalar frequency)
    ~output:scratch ();
  let amplitude = scalar amplitude in
  for j = 0 to length - 1 do
    let noise = scratch.(j) in
    scratch.(j) <- noise *. amplitude;
    (* Multiplication cannot make NaN/infinity finite, so this check
               also rejects every nonfinite noise sample. *)
    if not (Float.is_finite scratch.(j)) then nonfinite ()
  done;
  Array.iteri
    (fun k components ->
      let width = Array.length components and output = outputs.(k) in
      if plane_outputs <> None then
        Array.iteri
          (fun component ((bi, _, bc), (di, _, dc)) ->
            let { data = base; offset = bo; stride = bs } = columns.(bi).(bc)
            and { data = direction; offset = doff; stride = ds } = columns.(di).(dc) in
            let destination = (Option.get plane_outputs).(k).(component) in
            for j = 0 to length - 1 do
              scratch.(block_size + j) <-
                Array.unsafe_get direction (doff + ((start + j) * ds)) *. scratch.(j);
              if not (Float.is_finite scratch.(block_size + j)) then nonfinite ();
              let x = Array.unsafe_get base (bo + ((start + j) * bs)) +. scratch.(block_size + j) in
              if not (Float.is_finite x) then nonfinite ();
              match destination with
              | Some data -> Array.unsafe_set data (output_start + j) x
              | None -> ()
            done)
          components
      else
        let vector =
          if width = 3 then
            let (bi, bw, bx), (di, dw, dx) = components.(0)
            and (bj, bv, by), (dj, dv, dy) = components.(1)
            and (bk, bu, bz), (dk, du, dz) = components.(2) in
            if bi = bj && bj = bk && bw = bv && bv = bu && di = dj && dj = dk && dw = dv && dv = du
            then Some (bi, bw, bx, by, bz, di, dw, dx, dy, dz)
            else None
          else None
        in
        match vector with
        | Some (bi, bw, bx, by, bz, di, dw, dx, dy, dz) ->
            let base = inputs.(bi) and direction = inputs.(di) in
            for j = 0 to length - 1 do
              let at = (output_start + j) * 3
              and b = (start + j) * bw
              and d = (start + j) * dw
              and scale = scratch.(j) in
              Array.unsafe_set output at (Array.unsafe_get direction (d + dx) *. scale);
              Array.unsafe_set output (at + 1) (Array.unsafe_get direction (d + dy) *. scale);
              Array.unsafe_set output (at + 2) (Array.unsafe_get direction (d + dz) *. scale);
              Array.unsafe_set output at
                (Array.unsafe_get base (b + bx) +. Array.unsafe_get output at);
              Array.unsafe_set output (at + 1)
                (Array.unsafe_get base (b + by) +. Array.unsafe_get output (at + 1));
              Array.unsafe_set output (at + 2)
                (Array.unsafe_get base (b + bz) +. Array.unsafe_get output (at + 2));
              (* A nonfinite product remains nonfinite after addition. Checking
                   the three sums therefore also rejects every invalid product. *)
              if
                not
                  (Float.is_finite (Array.unsafe_get output at)
                  && Float.is_finite (Array.unsafe_get output (at + 1))
                  && Float.is_finite (Array.unsafe_get output (at + 2)))
              then nonfinite ()
            done
        | None ->
            Array.iteri
              (fun component ((bi, bw, bc), (di, dw, dc)) ->
                let base = inputs.(bi) and direction = inputs.(di) in
                for j = 0 to length - 1 do
                  let at = ((output_start + j) * width) + component in
                  Array.unsafe_set output at
                    (Array.unsafe_get direction (((start + j) * dw) + dc) *. scratch.(j));
                  if not (Float.is_finite (Array.unsafe_get output at)) then nonfinite ();
                  Array.unsafe_set output at
                    (Array.unsafe_get base (((start + j) * bw) + bc) +. Array.unsafe_get output at);
                  if not (Float.is_finite (Array.unsafe_get output at)) then nonfinite ()
                done)
              components)
    products

(* Noise3 under gain, bias and amplitude, in the same direct form. *)
let block_scalar_streaming r b
    (noise, (xi, _, xc), (yi, _, yc), z, frequency, gain, bias, amplitude, products) =
  let { t; inputs; columns; uniforms; frame; outputs; plane_outputs; block_size; _ } = r in
  let { scratch; tables; start; length; output_start; _ } = b in
  let scalar id =
    match t.code.(id) with
    | Const x -> x
    | Uniform (i, c) -> uniforms.(i).(c)
    | Frame _ -> frame.(id)
    | _ -> assert false
  in
  let x = columns.(xi).(xc) and y = columns.(yi).(yc) in
  let z, z_offset, z_stride =
    match z with
    | Some (i, _, c) ->
        let z = columns.(i).(c) in
        (z.data, z.offset + (start * z.stride), z.stride)
    | None -> (scratch, block_size, 1)
  in
  Rays_math.Noise.sample3_into
    (Option.get tables.(noise))
    ~first:0 ~last:length ~x:x.data
    ~x_offset:(x.offset + (start * x.stride))
    ~x_stride:x.stride ~y:y.data
    ~y_offset:(y.offset + (start * y.stride))
    ~y_stride:y.stride ~z ~z_offset ~z_stride
    ~frequency:(Option.fold ~none:1. ~some:scalar frequency)
    ~output:scratch ();
  let gain = scalar gain and bias = scalar bias and amplitude = scalar amplitude in
  for j = 0 to length - 1 do
    scratch.(j) <- scratch.(j) *. gain;
    scratch.(j) <- scratch.(j) -. bias;
    scratch.(j) <- scratch.(j) *. amplitude;
    (* Mul/Sub/Mul propagate every invalid intermediate to the final
               scale. Stores retain each separate binary64 rounding. *)
    if not (Float.is_finite scratch.(j)) then nonfinite ()
  done;
  Array.iteri
    (fun k components ->
      let width = Array.length components and output = outputs.(k) in
      if plane_outputs <> None then
        Array.iteri
          (fun component ((bi, _, bc), expression) ->
            let { data = base; offset; stride } = columns.(bi).(bc) in
            let destination = (Option.get plane_outputs).(k).(component) in
            match expression with
            | None ->
                for j = 0 to length - 1 do
                  let x = Array.unsafe_get base (offset + ((start + j) * stride)) in
                  if not (Float.is_finite x) then nonfinite ();
                  match destination with
                  | Some data -> Array.unsafe_set data (output_start + j) x
                  | None -> ()
                done
            | Some _ ->
                for j = 0 to length - 1 do
                  let x = Array.unsafe_get base (offset + ((start + j) * stride)) +. scratch.(j) in
                  if not (Float.is_finite x) then nonfinite ();
                  match destination with
                  | Some data -> Array.unsafe_set data (output_start + j) x
                  | None -> ()
                done)
          components
      else
        Array.iteri
          (fun component ((bi, bw, bc), expression) ->
            let base = inputs.(bi) in
            match expression with
            | None ->
                for j = 0 to length - 1 do
                  let x = Array.unsafe_get base (((start + j) * bw) + bc) in
                  if not (Float.is_finite x) then nonfinite ();
                  Array.unsafe_set output (((output_start + j) * width) + component) x
                done
            | Some _ ->
                for j = 0 to length - 1 do
                  let at = ((output_start + j) * width) + component in
                  Array.unsafe_set output at
                    (Array.unsafe_get base (((start + j) * bw) + bc) +. scratch.(j));
                  if not (Float.is_finite (Array.unsafe_get output at)) then nonfinite ()
                done)
          components)
    products

(* Every other block: the instructions in order, then their results stored or accumulated. *)
let block_store r b =
  let {
    t;
    widths;
    outputs;
    plane_outputs;
    collecting;
    accumulator;
    initialized;
    collect_product;
    block_size;
    _;
  } =
    r
  in
  let { scratch; start; length; output_start; _ } = b in
  let[@inline always] store k component index value =
    match plane_outputs with
    | Some planes -> (
        match planes.(k).(component) with
        | Some data -> Array.unsafe_set data index value
        | None -> ())
    | None -> Array.unsafe_set outputs.(k) ((index * widths.(k)) + component) value
  in
  block_execute r b false 0 length;
  if t.result = Collect && plane_outputs <> None then
    Array.iteri
      (fun k registers ->
        Array.iteri
          (fun component register ->
            let source = register * block_size in
            (match collect_product.(k).(component) with
            | None -> ()
            | Some (base, a, b) ->
                let base = base * block_size and a = a * block_size and b = b * block_size in
                for j = 0 to length - 1 do
                  scratch.(source + j) <- scratch.(a + j) *. scratch.(b + j);
                  if not (Float.is_finite scratch.(source + j)) then nonfinite ();
                  scratch.(source + j) <- scratch.(base + j) +. scratch.(source + j)
                done);
            for j = 0 to length - 1 do
              let x = scratch.(source + j) in
              if not (Float.is_finite x) then nonfinite ();
              store k component (output_start + j) x
            done)
          registers)
      t.outputs
  else if t.result = Collect then
    Array.iteri
      (fun k registers ->
        let output = outputs.(k) in
        if Array.exists Option.is_some collect_product.(k) then
          Array.iteri
            (fun component product ->
              let width = Array.length registers in
              match product with
              | Some (base, a, b) ->
                  let base = base * block_size and a = a * block_size and b = b * block_size in
                  for j = 0 to length - 1 do
                    let at = ((output_start + j) * width) + component in
                    (* Store the product before adding: the array boundary preserves
                       its binary64 rounding, including cancellation and signed zero. *)
                    Array.unsafe_set output at
                      (Array.unsafe_get scratch (a + j) *. Array.unsafe_get scratch (b + j));
                    if not (Float.is_finite (Array.unsafe_get output at)) then nonfinite ();
                    Array.unsafe_set output at
                      (Array.unsafe_get scratch (base + j) +. Array.unsafe_get output at);
                    if not (Float.is_finite (Array.unsafe_get output at)) then nonfinite ()
                  done
              | None ->
                  let source = registers.(component) * block_size in
                  for j = 0 to length - 1 do
                    let x = scratch.(source + j) in
                    if not (Float.is_finite x) then nonfinite ();
                    output.(((output_start + j) * width) + component) <- x
                  done)
            collect_product.(k)
        else if Array.length registers = 3 then begin
          let x = registers.(0) * block_size
          and y = registers.(1) * block_size
          and z = registers.(2) * block_size in
          for j = 0 to length - 1 do
            let px = scratch.(x + j) and py = scratch.(y + j) and pz = scratch.(z + j) in
            if not (Float.is_finite px && Float.is_finite py && Float.is_finite pz) then
              nonfinite ();
            let at = (output_start + j) * 3 in
            output.(at) <- px;
            output.(at + 1) <- py;
            output.(at + 2) <- pz
          done
        end
        else
          for component = 0 to Array.length registers - 1 do
            let source = registers.(component) * block_size and width = Array.length registers in
            for j = 0 to length - 1 do
              let x = scratch.(source + j) in
              if not (Float.is_finite x) then nonfinite ();
              output.(((output_start + j) * width) + component) <- x
            done
          done)
      t.outputs
  else
    for j = 0 to length - 1 do
      (match t.result with Accumulate _ -> block_execute r b true j (j + 1) | _ -> ());
      for k = 0 to Array.length t.outputs - 1 do
        let registers = t.outputs.(k) in
        let output = outputs.(k) and width = widths.(k) in
        for component = 0 to width - 1 do
          let x = scratch.((registers.(component) * block_size) + j) in
          if not (Float.is_finite x) then nonfinite ();
          (match t.result with
          | Collect -> ()
          | Accumulate _ -> accumulator.(component) <- x
          | Sum _ ->
              let total = if !initialized then accumulator.(component) +. x else x in
              if not (Float.is_finite total) then nonfinite ();
              accumulator.(component) <- total);
          if collecting then output.(((output_start + j) * width) + component) <- x
        done
      done;
      match t.result with Sum _ -> initialized := true | _ -> ()
    done

let run_chunk r chunk =
  let {
    t;
    count;
    skip;
    skipped_before;
    streaming;
    scalar_streaming;
    plane_outputs;
    block_size;
    blocks;
    blocks_per_chunk;
    _;
  } =
    r
  in
  if r.ordered_add then chunk_ordered r chunk
  else begin
    let scratch =
      Array.make
        ((if Option.is_some streaming then if plane_outputs = None then 1 else 2
          else if Option.is_some scalar_streaming then 2
          else Array.length t.code)
        * block_size)
        0.
    in
    let noise_scratch = Rays_math.Noise.create_fbm3_scratch () in
    let tables =
      Array.map
        (fun instruction ->
          match unguarded instruction with
          | Noise3 (_, _, _, seed, _, _, _) -> Some (Flow.Op.noise_table seed)
          | _ -> None)
        t.code
    in
    let contiguous = t.iteration = Zip && skip = [||] in
    let indices = if contiguous then [||] else Array.make block_size 0 in
    for block = chunk * blocks_per_chunk to min blocks ((chunk + 1) * blocks_per_chunk) - 1 do
      let start = block * block_size and limit = min count ((block + 1) * block_size) in
      let length, output_start =
        if contiguous then (limit - start, start)
        else
          let skipped = ref (skipped_before start) and length = ref 0 in
          for i = start to limit - 1 do
            if !skipped < Array.length skip && skip.(!skipped) = i then incr skipped
            else begin
              indices.(!length) <- i;
              incr length
            end
          done;
          (!length, start - skipped_before start)
      in
      let b =
        { scratch; noise_scratch; tables; contiguous; indices; start; length; output_start }
      in
      match streaming with
      | Some payload -> block_streaming r b payload
      | None when Option.is_some scalar_streaming ->
          block_scalar_streaming r b (Option.get scalar_streaming)
      | None -> block_store r b
    done
  end

let rec run_all ?dispatch ?column_source ?plane_outputs ?expected_count ?state ?elems ?resolve
    ?measure ?on_walk ?(strict = false) (t : t) ~live =
  let state = match state with Some state -> state | None -> E.create_state () in
  let scope = if elems <> None then None else Scope.enter ~resolve ~live ~state ~strict in
  Fun.protect ~finally:(fun () -> Scope.leave scope) @@ fun () ->
  let work () =
    let rec evaluate residual (term : W.term) =
      let view = E.Residual.residual_view residual in
      let key = (term, view) in
      let active = Scope.active and stack = Domain.DLS.get Scope.active in
      let same_active ((t : W.term), (other : E.Residual.residual_view)) =
        t == term && Scope.same_context view other && view.iter = other.iter
      in
      let looped =
        match term.node with
        | W.Lit _ | Text _ | Nil | Time | Fn_ref _ -> false
        | _ -> List.exists same_active stack
      in
      if looped then begin
        let numeric_loop =
          match term.node with
          | W.Hof ((`Map | `Reduce), _) | Loop _ | Op { op = "array/sum"; _ } -> true
          | _ -> false
        in
        if strict && numeric_loop then
          Error
            (Diagnostic.error ~code:"E_KERNEL_FORM"
               "Numeric node bodies must compile to packed kernels.")
        else
          match dispatch with
          | Some dispatch -> dispatch residual term
          | None ->
              E.Residual.eval_term ~state ?elems ?resolve ?on_walk ~kernel:execute_request residual
                term ~live
      end
      else begin
        Domain.DLS.set active (key :: stack);
        Fun.protect
          ~finally:(fun () -> Domain.DLS.set active stack)
          (fun () ->
            match scope with
            | None -> evaluate_once residual term
            | Some scope -> (
                match Scope.find scope residual term with
                | Some value -> Ok value
                | None ->
                    let result = evaluate_once residual term in
                    (match result with
                    | Ok value when not (E.state_dependent (E.Residual residual)) ->
                        Scope.add scope residual term value
                    | _ -> ());
                    result))
      end
    and evaluate_once residual (term : W.term) =
      let nested =
        match term.node with
        | W.Ref_binding (name, []) -> (
            match List.assoc_opt name (E.Residual.residual_view residual).bindings with
            | Some (E.Residual r) ->
                let view = E.Residual.residual_view r in
                Some (r, view.term)
            | _ -> None)
        | _ -> None
      in
      match nested with
      | Some (residual, term) -> evaluate residual term
      | None
        when match term.node with W.Lit _ | Text _ | Nil | Time | Fn_ref _ -> true | _ -> false ->
          E.Residual.eval_term ~state ?elems ?resolve ?on_walk ~kernel:execute_request residual term
            ~live
      | None when dispatch <> None -> Option.get dispatch residual term
      | None -> (
          match
            compile_impl ~fusion:t.fusion ~dynamic:t.dynamic ~count_source:t.count_source residual
              term
          with
          | Ok program -> force ~strict ~state ?elems ?resolve ?measure program ~live
          | Error _ -> (
              match (strict, term.node) with
              (* a fold over a list (a matrix inverse's four pivots) is value work, not a body *)
              | true, Loop { clauses; _ }
                when List.for_all
                       (fun (_, (c : W.term)) -> match c.ty with Ty.List _ -> true | _ -> false)
                       clauses ->
                  E.Residual.eval_term ~state ?elems ?resolve ?on_walk residual term ~live
              | true, (W.Hof ((`Map | `Reduce), _) | Loop _ | Op { op = "array/sum"; _ }) ->
                  Error
                    (Diagnostic.error ~code:"E_KERNEL_FORM"
                       "Numeric node bodies must compile to packed kernels.")
              | false, (W.Hof ((`Map | `Reduce), _) | Loop _ | Op { op = "array/sum"; _ }) ->
                  E.Residual.eval_term ~state ?elems ?resolve ?on_walk ~kernel:execute_request
                    residual term ~live
              | _ -> uniform (E.Residual (E.Residual.with_bindings residual ~term []))))
    and execute_request residual =
      let view = E.Residual.residual_view residual in
      if Scope.request_active view.term view then
        if strict then
          raise
            (Diagnostic.Error
               (Diagnostic.error ~code:"E_KERNEL_FORM"
                  "A nested numeric kernel request cannot be compiled."))
        else None
      else
        Scope.with_request view.term view (fun () ->
            match evaluate residual view.term with
            | Ok value -> Some value
            | Error diagnostic when strict -> raise (Diagnostic.Error diagnostic)
            | Error _ -> None)
    and uniform value =
      force_values ~state ?elems ?resolve ~live
        ~residual:(fun residual -> evaluate residual (E.Residual.residual_view residual).term)
        value
    in
    E.Residual.handle ~kernel:execute_request ~bulk:execute_request (fun () ->
        let seed =
          match t.result with
          | Accumulate (seed, _) -> Some (get (evaluate t.residual seed))
          | _ -> None
        in
        (* ponytail: guarded live captures keep sequential blocks; preparing only
       selected captures on the main domain is needed before parallel execution. *)
        let guarded_capture =
          Array.exists
            (function
              | Guarded (_, body) -> (
                  match unguarded body with
                  | Uniform (index, _) -> (
                      match t.uniforms.(index) with
                      | E.Float _ | Bool _ | Vec2 _ | Vec3 _ | Vec4 _ -> false
                      | _ -> true)
                  | _ -> false)
              | _ -> false)
            t.code
        in
        if integer_program t || guarded_capture then
          run_typed ?column_source ?plane_outputs ?expected_count ~seed ~live t
            ~evaluate:(fun r term -> get (evaluate r term))
            ~uniform:(fun value -> get (uniform value))
        else
          let borrowed = Array.make (Array.length t.sources) None in
          let lengths = Array.make (Array.length t.sources) 0 in
          let empty_product = ref false in
          let inputs =
            Array.mapi
              (fun i (residual, term) ->
                if !empty_product then [||]
                else begin
                  borrowed.(i) <-
                    (match column_source with
                    | None -> None
                    | Some source -> get (source residual term));
                  let data =
                    match borrowed.(i) with
                    | Some columns ->
                        lengths.(i) <- borrowed_count t.widths.(i) columns;
                        [||]
                    | None ->
                        let value = get (evaluate residual term) in
                        let data =
                          match (value, t.widths.(i)) with
                          | E.Float_array xs, 1
                          | Vec2_array xs, 2
                          | Vec3_array xs, 3
                          | Vec4_array xs, 4 ->
                              xs
                          | _ ->
                              V.fail "E_ARRAY_TYPE" "Kernel input changed its packed element type."
                        in
                        lengths.(i) <- Array.length data / t.widths.(i);
                        data
                  in
                  if t.iteration = Product && lengths.(i) = 0 then empty_product := true;
                  data
                end)
              t.sources
          in
          let columns =
            Array.mapi
              (fun i array ->
                match borrowed.(i) with
                | None ->
                    Array.init t.widths.(i) (fun component ->
                        { data = array; offset = component; stride = t.widths.(i) })
                | Some columns ->
                    if Array.length columns <> t.widths.(i) then
                      V.fail "E_ARRAY_TYPE" "Borrowed columns changed the packed element width.";
                    columns)
              inputs
          in
          let count =
            if t.iteration = Product && Array.exists (( = ) 0) lengths then 0
            else
              Array.fold_left
                (fun a b ->
                  match t.iteration with
                  | Zip -> min a b
                  | Product ->
                      if b <> 0 && a > max_int / b then
                        V.fail "E_ARRAY_RANGE" "Kernel iteration count exceeds native bounds.";
                      a * b)
                (if t.iteration = Zip then max_int else 1)
                lengths
          in
          let count = if inputs = [||] then 0 else count in
          let strides = Array.make (Array.length inputs) 1 in
          if t.iteration = Product && count > 0 then
            for i = Array.length inputs - 2 downto 0 do
              strides.(i) <- strides.(i + 1) * lengths.(i + 1)
            done;
          let uniforms =
            if count = 0 then [||]
            else Array.map (fun value -> components (get (uniform value))) t.uniforms
          in
          let frame =
            Array.map
              (fun instruction ->
                match unguarded instruction with
                | Frame "t" -> live.Input.t
                | Frame name -> (
                    match Flow.Op.find name Flow.Context.value with
                    | Some op -> V.num (op.body ~live ~node:(fun _ _ -> raise Unsupported) [])
                    | None -> raise Unsupported)
                | _ -> 0.)
              t.code
          in
          let started = Option.map (fun (clock, _) -> clock ()) measure in
          let widths = Array.map Array.length t.outputs in
          let width = widths.(0) in
          List.iter
            (fun (_, (term : W.term)) ->
              match term.ty with
              | Ty.Array ty ->
                  let width = width_of ty in
                  if count > Sys.max_floatarray_length / width then
                    V.fail "E_ARRAY_RANGE" "Kernel intermediate exceeds native storage bounds."
              | _ -> ())
            t.stages;
          let skip =
            Array.to_list t.skip |> List.filter (fun i -> i >= 0 && i < count) |> Array.of_list
          in
          let skipped_before n =
            let lo = ref 0 and hi = ref (Array.length skip) in
            while !lo < !hi do
              let mid = (!lo + !hi) / 2 in
              if skip.(mid) < n then lo := mid + 1 else hi := mid
            done;
            !lo
          in
          let kept = count - Array.length skip in
          Option.iter
            (fun expected ->
              if kept <> expected then
                V.fail "E_ARRAY_COUNT" "Column sink count differs from the kernel count.")
            expected_count;
          let collecting =
            match t.result with Collect | Accumulate (_, true) -> true | _ -> false
          in
          if
            collecting
            && Array.exists (fun width -> kept > Sys.max_floatarray_length / width) widths
          then V.fail "E_ARRAY_RANGE" "Kernel output exceeds native storage bounds.";
          Option.iter
            (fun planes ->
              if
                (not collecting) || t.result <> Collect
                || Array.length planes <> Array.length widths
              then V.fail "E_ARRAY_TYPE" "Column sinks require matching Collect outputs.";
              Array.iteri
                (fun k components ->
                  if Array.length components <> widths.(k) then
                    V.fail "E_ARRAY_TYPE" "Column sink width differs from the output.";
                  Array.iter
                    (Option.iter (fun data ->
                         if Array.length data <> kept then
                           V.fail "E_ARRAY_COUNT" "Column sink count differs from the kernel count."))
                    components)
                planes)
            plane_outputs;
          let outputs =
            Array.map
              (fun width ->
                Array.make (if collecting && plane_outputs = None then kept * width else 0) 0.)
              widths
          in
          let accumulator =
            match seed with
            | Some ((E.Vec2 _ | Vec3 _ | Vec4 _) as value) -> components value
            | Some v -> Array.make width (V.num v)
            | None -> Array.make width 0.
          in
          let initialized = ref (match t.result with Sum zero -> zero | _ -> true) in
          let ordered_add = ordered_add t in
          let uses = Array.make (Array.length t.code) 0 in
          let use id = uses.(id) <- uses.(id) + 1 in
          Array.iter (fun instruction -> List.iter use (operands instruction)) t.code;
          Array.iter (Array.iter use) t.outputs;
          let omitted = Array.make (Array.length t.code) false in
          let rec guards = function
            | Guarded (conditions, body) -> conditions @ guards body
            | _ -> []
          in
          let guards = Array.map guards t.code in
          (* Scaling and sampling must have the same scope: moving an unconditional
       multiplication into an inactive noise branch would hide its overflow. *)
          let scale conditions register =
            match unguarded t.code.(register) with
            | Binary (Mul, a, b) when uses.(register) = 1 && guards.(register) = conditions ->
                let constant id =
                  match unguarded t.code.(id) with
                  | Const _ | Uniform _ | Frame _ -> true
                  | _ -> false
                in
                if constant b then Some (a, b) else if constant a then Some (b, a) else None
            | _ -> None
          in
          let noise_scale =
            Array.mapi
              (fun slot instruction ->
                match unguarded instruction with
                | Noise3 (x, y, z, _, _, _, _) -> (
                    match (scale guards.(slot) x, scale guards.(slot) y, scale guards.(slot) z) with
                    | Some (a, f), Some (b, g), Some (c, h) when f = g && g = h ->
                        omitted.(x) <- true;
                        omitted.(y) <- true;
                        omitted.(z) <- true;
                        Some (a, b, c, f)
                    | _ -> None)
                | _ -> None)
              t.code
          in
          let collect_product =
            Array.map
              (Array.map (fun register ->
                   if t.result <> Collect || uses.(register) <> 1 then None
                   else
                     match t.code.(register) with
                     | Binary (Add, a, b) -> (
                         let product base id =
                           match t.code.(id) with
                           | Binary (Mul, x, y) when uses.(id) = 1 ->
                               omitted.(id) <- true;
                               omitted.(register) <- true;
                               Some (base, x, y)
                           | _ -> None
                         in
                         match product a b with Some _ as p -> p | None -> product b a)
                     | _ -> None))
              t.outputs
          in
          let streaming, scalar_streaming = detect_streaming t skip noise_scale collect_product in
          let block_size, blocks, blocks_per_chunk, chunks = block_layout ~count ~limit:1024 in
          let dependent_flags = if count = 0 then [||] else Core.Region.dependent_flags t in
          let r =
            {
              t;
              inputs;
              borrowed;
              lengths;
              strides;
              columns;
              uniforms;
              frame;
              widths;
              width;
              outputs;
              plane_outputs;
              skip;
              count;
              collecting;
              accumulator;
              initialized;
              ordered_add;
              streaming;
              scalar_streaming;
              block_size;
              blocks;
              blocks_per_chunk;
              dependent_flags;
              omitted;
              skipped_before;
              noise_scale;
              collect_product;
            }
          in
          let chunk = run_chunk r in
          (match t.result with
          | Collect -> Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(chunks - 1) chunk
          | Sum _ | Accumulate _ ->
              (* The fixed tree composes ordered chunk spans, retaining reference additions
            instead of reassociating partial sums (which changes cancellation and signed zero). *)
              let rec ordered first last =
                if last - first = 1 then chunk first
                else if first < last then begin
                  let middle = first + ((last - first) / 2) in
                  ordered first middle;
                  ordered middle last
                end
              in
              ordered 0 chunks);
          let value =
            if plane_outputs <> None then []
            else if collecting then
              Array.to_list
                (Array.mapi
                   (fun k output ->
                     match widths.(k) with
                     | 2 -> E.Vec2_array output
                     | 3 -> E.Vec3_array output
                     | 4 -> E.Vec4_array output
                     | _ -> E.Float_array output)
                   outputs)
            else
              [
                (match (t.result, seed) with
                | Accumulate _, Some seed when kept = 0 -> seed
                | Sum false, _ when kept = 0 -> E.Int 0
                | _ -> (
                    match width with
                    | 2 -> E.Vec2 (accumulator.(0), accumulator.(1))
                    | 3 -> E.Vec3 (accumulator.(0), accumulator.(1), accumulator.(2))
                    | 4 ->
                        E.Vec4 (accumulator.(0), accumulator.(1), accumulator.(2), accumulator.(3))
                    | _ -> E.Float accumulator.(0)));
              ]
          in
          Option.iter
            (fun (clock, report) ->
              report t ~seconds:(max 0. (clock () -. Option.get started)) ~reference:false)
            measure;
          value)
  in
  let result =
    E.transaction state (fun () ->
        try Ok (work ()) with
        | Unsupported ->
            Error (Diagnostic.error ~code:"E_KERNEL" "Kernel needs reference evaluation.")
        | Diagnostic.Error d -> Error d
        | Invalid_argument message ->
            Error
              (Diagnostic.error ~code:"E_KERNEL" ("Kernel needs reference evaluation: " ^ message))
        | Out_of_memory ->
            Error (Diagnostic.error ~code:"E_ARRAY_MEMORY" "Frame data exceeds available memory."))
  in
  match result with
  | Ok _ -> result
  | Error _ when strict || plane_outputs <> None || Array.length t.outputs > 1 -> result
  | Error _ ->
      let started = Option.map (fun (clock, _) -> clock ()) measure in
      let result = reference_observed ~state ?elems ?resolve ?on_walk t ~live in
      Option.iter
        (fun (clock, report) ->
          report t ~seconds:(max 0. (clock () -. Option.get started)) ~reference:true)
        measure;
      Result.map (fun value -> [ value ]) result

and force ?dispatch ?state ?elems ?resolve ?measure ?on_walk ?strict t ~live =
  match run_all ?dispatch ?state ?elems ?resolve ?measure ?on_walk ?strict t ~live with
  | Ok [ value ] -> Ok value
  | Ok _ -> Error (Diagnostic.error ~code:"E_KERNEL" "Program has several outputs; use force_all.")
  | Error _ as error -> error

let force_all ?dispatch ?state ?elems ?resolve ?measure ?on_walk ?strict t ~live =
  run_all ?dispatch ?state ?elems ?resolve ?measure ?on_walk ?strict t ~live

let force_into ?state ?elems ?resolve ?measure ?strict ~column_source ~count ~outputs t ~live =
  Result.map
    (fun _ -> ())
    (run_all ~column_source ~plane_outputs:outputs ~expected_count:count ?state ?elems ?resolve
       ?measure ?strict t ~live)

module Host = struct
  let argument_types (t : t) =
    let types = Hashtbl.create 8 and visited = Hashtbl.create 8 in
    let rec value = function
      | E.Source ("$core/argument", ty, [ ("index", E.Int index) ]) -> (
          if index < 0 then V.fail "E_CORE_ARGUMENT" "Invalid compiled argument index.";
          match Hashtbl.find_opt types index with
          | Some old when old <> ty -> V.fail "E_CORE_ARGUMENT" "Compiled argument types disagree."
          | _ -> Hashtbl.replace types index ty)
      | E.Source ("$core/argument", _, _) ->
          V.fail "E_CORE_ARGUMENT" "Invalid compiled argument declaration."
      | E.Residual r -> residual r
      | Fn fn -> List.iter (fun (_, v) -> value v) (E.Residual.function_bindings fn)
      | List values -> Array.iter value values
      | Record fields | Struct (_, _, fields) | Source (_, _, fields) ->
          List.iter (fun (_, v) -> value v) fields
      | _ -> ()
    and residual r =
      let id = E.Residual.residual_id r in
      if not (Hashtbl.mem visited id) then begin
        Hashtbl.add visited id ();
        List.iter (fun (_, v) -> value v) (E.Residual.residual_view r).bindings
      end
    in
    residual t.residual;
    Array.iter value t.uniforms;
    Array.iter (fun (r, _) -> residual r) t.sources;
    List.iter (fun (r, _) -> residual r) t.stages;
    Array.init (Hashtbl.length types) (fun index ->
        match Hashtbl.find_opt types index with
        | Some ty -> ty
        | None -> V.fail "E_CORE_ARGUMENT" "Compiled argument indices must be contiguous.")

  let bind_arguments ?(validate = true) (t : t) arguments =
    try
      let types = argument_types t in
      if Array.length types <> Array.length arguments then
        V.fail "E_CORE_ARGUMENT" "Compiled region argument count differs from its declaration.";
      Array.iteri
        (fun index value ->
          if V.ty_of value <> types.(index) then
            V.fail "E_CORE_ARGUMENT"
              "Compiled region argument storage differs from its declared type.";
          if validate then V.validate value)
        arguments;
      if types = [||] then Ok t
      else
        let rec value = function
          | E.Source ("$core/argument", _, [ ("index", E.Int index) ]) -> arguments.(index)
          | Residual r -> Residual (residual r)
          | Fn fn -> Fn (E.Host.map_function_bindings value fn)
          | List values -> List (Array.map value values)
          | Record fields -> Record (List.map (fun (name, v) -> (name, value v)) fields)
          | Struct (name, ty, fields) ->
              Struct (name, ty, List.map (fun (name, v) -> (name, value v)) fields)
          | Source (name, ty, fields) ->
              Source (name, ty, List.map (fun (name, v) -> (name, value v)) fields)
          | v -> v
        and residual r =
          let view = E.Residual.residual_view r in
          E.Residual.with_bindings r ~term:view.term
            (List.map (fun (name, v) -> (name, value v)) view.bindings)
        in
        Core.Region.Private.with_captures t ~residual:(residual t.residual)
          ~uniforms:(Array.map value t.uniforms)
          ~sources:(Array.map (fun (r, term) -> (residual r, term)) t.sources)
          ~stages:(List.map (fun (r, term) -> (residual r, term)) t.stages)
          ~count:t.count
    with Diagnostic.Error d -> Error d

  let ordered_add = ordered_add
  let reference = reference
  let output_reachable = output_reachable
  let finite_checks = finite_checks

  type view = {
    code : instruction array;
    widths : int array;
    outputs : int array array;
    uniform_widths : int array;
    collecting : bool;
  }

  let view (t : t) =
    let uniform_widths = Array.make (Array.length t.uniforms) 1 in
    let rec instruction = function
      | Uniform (index, component) ->
          uniform_widths.(index) <- max uniform_widths.(index) (component + 1)
      | Guarded (_, body) -> instruction body
      | _ -> ()
    in
    Array.iter instruction t.code;
    {
      code = t.code;
      widths = t.widths;
      outputs = t.outputs;
      uniform_widths;
      collecting = (match t.result with Collect -> true | _ -> false);
    }

  type shape = Value of E.value | Range of E.value * int * int | Opaque of E.value list option

  (* What a source or uniform reads: a value itself; [array/range] of an array's count times [mul]
     over [div]; or a term over the listed values ([None]: a form not traversed here). *)
  let shape (residual : E.residual) (term : W.term) =
    let binding r n = List.assoc_opt n (E.Residual.residual_view r).bindings in
    let rec refs r (term : W.term) acc =
      match (term.node, acc) with
      | _, None -> None
      | (Lit _ | Text _ | Nil | Time | Fn_ref _), _ -> acc
      | Ref_binding (n, _), Some vs -> (
          match binding r n with
          | Some (E.Residual r') -> refs r' (E.Residual.residual_view r').term acc
          | Some v -> Some (v :: vs)
          | None -> None)
      | (Op { args; _ } | Call { args; _ }), _ ->
          List.fold_left (fun acc (_, a) -> refs r a acc) acc args
      | Vec ts, _ -> List.fold_left (fun acc a -> refs r a acc) acc ts
      | If (a, b, c), _ -> refs r a (refs r b (refs r c acc))
      | _ -> None
    in
    let value r (term : W.term) =
      match term.node with
      | Ref_binding (n, []) -> (
          match binding r n with Some (E.Residual _) | None -> None | Some v -> Some v)
      | _ -> None
    in
    let int (term : W.term) =
      match term.node with Lit (Param.Int_value k) when k > 0 -> Some k | _ -> None
    in
    let count (term : W.term) =
      match term.node with
      | Op { op = "array/count"; args = [ (_, x) ]; _ } -> value residual x
      | _ -> None
    in
    let scaled (term : W.term) =
      match term.node with
      | Op { op = "array/count"; _ } -> Option.map (fun v -> (v, 1, 1)) (count term)
      | Op { op = "*" | "int/mul"; args = [ (_, a); (_, b) ]; _ } -> (
          match (count a, int b, count b, int a) with
          | Some v, Some k, _, _ | _, _, Some v, Some k -> Some (v, k, 1)
          | _ -> None)
      | Op { op = "int/div"; args = [ (_, a); (_, b) ]; _ } -> (
          match (count a, int b) with Some v, Some k -> Some (v, 1, k) | _ -> None)
      | _ -> None
    in
    match (value residual term, term.node) with
    | Some v, _ -> Value v
    | None, Op { op = "array/range"; args = [ (_, n) ]; _ } when scaled n <> None ->
        let v, mul, div = Option.get (scaled n) in
        Range (v, mul, div)
    | None, _ -> Opaque (refs residual term (Some []))

  let source_shapes (t : t) = Array.map (fun (r, term) -> shape r term) t.sources

  let uniform_shapes (t : t) =
    Array.map
      (function E.Residual r -> shape r (E.Residual.residual_view r).term | v -> Value v)
      t.uniforms

  type inputs = {
    arrays : float array array;
    uniforms : float array array;
    tables : float array array;
    frame : float array;
    count : int;
  }

  type value_inputs = {
    arrays : E.value array;
    uniforms : E.value array;
    frame : E.value array;
    count : int;
  }

  let forwards (p : t) =
    Array.length p.sources = 1
    && p.count = None && p.iteration = Zip && p.result = Collect && p.skip = [||]
    && Array.length p.outputs = 1
    && (snd p.sources.(0)).W.ty = p.term.W.ty
    && Array.length p.outputs.(0) = p.widths.(0)
    && Array.for_all Fun.id
         (Array.mapi
            (fun c r ->
              match unguarded p.code.(r) with
              | Input (0, w, c') -> w = p.widths.(0) && c' = c
              | _ -> false)
            p.outputs.(0))

  let prepare_values ?state ?resolve ?measure ?(lengths = fun _ -> None) ?resident
      ?(source = fun _ _ -> None) (t : t) ~live =
    let state = match state with Some state -> state | None -> E.create_state () in
    E.transaction state (fun () ->
        try
          let rec evaluate residual term = evaluate_with resolve residual term
          and evaluate_with resolve residual term =
            match term.W.node with
            | W.Ref_binding (name, []) -> (
                match List.assoc_opt name (E.Residual.residual_view residual).bindings with
                | Some (E.Residual residual) ->
                    evaluate_with resolve residual (E.Residual.residual_view residual).term
                | _ -> materialize resolve residual term)
            | _ -> materialize resolve residual term
          and materialize resolve residual term =
            match
              compile_impl ~fusion:t.fusion ~dynamic:t.dynamic ~count_source:t.count_source residual
                term
            with
            | Ok program when forwards program ->
                (* a region that only copies its one source (a branch chosen by constants) is that
                   source: the same array, which a device holding it does not upload again *)
                let residual, term = program.sources.(0) in
                evaluate_with resolve residual term
            | Ok program -> get (force ~state ?resolve ?measure program ~live)
            | Error _ -> get (E.Residual.eval_term ~state ?resolve residual term ~live)
          in
          (* A source that is one read of data the device holds ([resident]) is that data's
             placeholder, bound on the device, with its count: tried with a resolve answering
             placeholders, kept only when the source is exactly the one placeholder it was handed
             (anything computed over a placeholder is evaluated again with the caller's resolve). *)
          let placed = Array.make (Array.length t.sources) None in
          let evaluate_source i residual term =
            match (resident, resolve) with
            | None, _ | _, None -> evaluate residual term
            | Some resident, Some resolve -> (
                let handed = ref [] in
                let speculative v =
                  match resident v with
                  | Some (p, n) ->
                      handed := (p, n) :: !handed;
                      Ok p
                  | None -> resolve v
                in
                match evaluate_with (Some speculative) residual term with
                | value when !handed = [] -> value
                | value -> (
                    match !handed with
                    | [ (p, n) ] when value == p ->
                        placed.(i) <- Some n;
                        value
                    | _ -> evaluate residual term)
                | exception (Diagnostic.Error _ | Unsupported | Invalid_argument _)
                  when !handed <> [] ->
                    evaluate residual term)
          in
          let empty_product = ref false in
          (* a source whose length the caller knows (a range it binds on the device) is not built *)
          let known = Array.mapi (fun i _ -> lengths i) t.sources in
          let arrays =
            Array.mapi
              (fun i (residual, term) ->
                let value =
                  if known.(i) <> None then
                    match term.W.ty with
                    | Ty.Array Ty.Int -> E.Int_array [||]
                    | Array Float -> Float_array [||]
                    | _ -> raise Unsupported
                  else if !empty_product then
                    match term.W.ty with
                    | Ty.Array Ty.Int -> E.Int_array [||]
                    | Array Text -> Text_array [||]
                    | Array Float -> Float_array [||]
                    | Array Vec2 -> Vec2_array [||]
                    | Array Vec3 -> Vec3_array [||]
                    | Array Vec4 -> Vec4_array [||]
                    | _ -> raise Unsupported
                  else
                    match source residual term with
                    | Some value -> value
                    | None -> evaluate_source i residual term
                in
                if t.iteration = Product && V.array_length value = 0 && known.(i) = None then
                  empty_product := true;
                value)
              t.sources
          in
          let length i value =
            match (known.(i), placed.(i)) with
            | Some n, _ | None, Some n -> n
            | None, None -> V.array_length value
          in
          Array.iteri
            (fun index value ->
              if V.ty_of value <> (snd t.sources.(index)).W.ty then
                V.fail "E_ARRAY_TYPE" "Kernel input changed its packed element type.")
            arrays;
          let count =
            if arrays = [||] || !empty_product then 0
            else
              snd
                (Array.fold_left
                   (fun (i, count) value ->
                     let length = length i value in
                     ( i + 1,
                       match t.iteration with
                       | Zip -> min count length
                       | Product ->
                           if length <> 0 && count > max_int / length then
                             V.fail "E_ARRAY_RANGE" "Kernel iteration count exceeds native bounds.";
                           count * length ))
                   (0, if t.iteration = Zip then max_int else 1)
                   arrays)
          in
          (* a residual several uniforms read (a matrix, its inverse) is evaluated once: each
             uniform's live bindings are their values, made once per call; a binding that fails
             made eagerly stays as it was, made only where the term reads it. So do the residuals
             inside a record or list binding: a live matrix's rows, which every read of the matrix
             (each row's uniform, the identity test, the inverse) made again, 2.4 ms a frame *)
          let made = Hashtbl.create 8 in
          let rec uniform residual =
            let id = E.Residual.residual_id residual in
            match Hashtbl.find_opt made id with
            | Some value -> value
            | None ->
                let view = E.Residual.residual_view residual in
                let bindings =
                  List.map (fun (name, value) -> (name, binding value)) view.bindings
                in
                let residual =
                  if List.for_all2 (fun (_, a) (_, b) -> a == b) view.bindings bindings then
                    residual
                  else E.Residual.with_bindings residual ~term:view.term bindings
                in
                (* a uniform that is no bulk form is no kernel: it is made by reference *)
                let value =
                  match view.term.node with
                  | W.Hof ((`Map | `Reduce), _) | Loop _ | Op { op = "array/sum"; _ } ->
                      evaluate residual view.term
                  | _ -> get (E.Residual.eval_term ~state ?resolve residual view.term ~live)
                in
                Hashtbl.replace made id value;
                value
          and binding value =
            match value with
            | E.Residual residual when not (E.state_dependent value) -> (
                try uniform residual
                with Diagnostic.Error _ | Unsupported | Invalid_argument _ -> value)
            | Record fields ->
                let made = List.map (fun (name, v) -> (name, binding v)) fields in
                if List.for_all2 (fun (_, a) (_, b) -> a == b) fields made then value
                else E.Record made
            | List items ->
                let made = Array.map binding items in
                if Array.for_all2 ( == ) items made then value else E.List made
            | _ -> value
          in
          let uniforms =
            Array.map
              (fun value ->
                get
                  (force_values ~state ?resolve ~live
                     ~residual:(fun residual -> Ok (uniform residual))
                     value))
              t.uniforms
          in
          let frame =
            Array.map
              (fun instruction ->
                match unguarded instruction with
                | Frame "t" -> E.Float live.Input.t
                | Frame name | Int_frame name -> (
                    match Flow.Op.find name Flow.Context.value with
                    | Some op -> op.body ~live ~node:(fun _ _ -> raise Unsupported) []
                    | None -> raise Unsupported)
                | _ -> E.Float 0.)
              t.code
          in
          Ok ({ arrays; uniforms; frame; count } : value_inputs)
        with
        | Unsupported ->
            Error (Diagnostic.error ~code:"E_KERNEL" "Kernel inputs need reference evaluation.")
        | Diagnostic.Error d -> Error d
        | Invalid_argument message ->
            Error
              (Diagnostic.error ~code:"E_KERNEL"
                 ("Kernel inputs need reference evaluation: " ^ message))
        | Out_of_memory ->
            Error (Diagnostic.error ~code:"E_ARRAY_MEMORY" "Frame data exceeds available memory."))

  let device_inputs (values : value_inputs) =
    let floats = function
      | E.Float_array values | Vec2_array values | Vec3_array values | Vec4_array values ->
          Some values
      | E.Int_array values -> Some (Array.map float_of_int values)
      | _ -> None
    in
    ({
       arrays = Array.map (fun v -> Option.get (floats v)) values.arrays;
       uniforms =
         Array.map (fun v -> if floats v = None then components v else [||]) values.uniforms;
       tables = Array.map (fun v -> Option.value ~default:[||] (floats v)) values.uniforms;
       frame = Array.map V.num values.frame;
       count = values.count;
     }
      : inputs)

  (* A device kernel's inputs as 32-bit floats: integer arrays and scalars travel as floats (exact to
     2^24, the fast precision's contract), an array-valued uniform as the table a gather reads. *)
  let prepare ?(fast = false) ?state ?resolve ?measure ?resident ?source (t : t) ~live =
    if Array.exists Core.Region.text_register t.code || (integer_program t && not fast) then
      Error
        (Diagnostic.error ~code:"E_GPU_FORM"
           (if Array.exists Core.Region.text_register t.code then "Text kernels stay on the CPU."
            else "Integer sources, uniforms and instructions stay on the CPU."))
    else
      Result.map device_inputs (prepare_values ?state ?resolve ?measure ?resident ?source t ~live)
end
