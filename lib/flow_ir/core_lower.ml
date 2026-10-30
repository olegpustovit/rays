module E = Flow.Eval
module W = Flow.Workspace
module V = Flow.Value
module C = Core
module Names = Map.Make (String)

exception Unsupported of string

let refuse name = raise (Unsupported name)

let failure (d : Diagnostic.t) =
  if d.span = None && d.position = None && d.cause = None && d.hints = [] && d.trace = [] then
    C.Fail (d.code, d.message)
  else C.Fail_diagnostic d

let rec closure_free : E.value -> bool = function
  | V.Fn _ | Residual _ -> false
  | List values -> Array.for_all closure_free values
  | Record fields | Struct (_, _, fields) | Source (_, _, fields) ->
      List.for_all (fun (_, v) -> closure_free v) fields
  | _ -> true

let geometry_collection ty = ty = Flow.Ty.List Flow.Ty.geometry

let state_step_candidate parent (candidate : W.term) =
  match (E.Residual.residual_view parent).term.node with
  | W.State { step; _ } ->
      let rec direct term =
        term == candidate
        ||
        match term.W.node with
        | W.If _ | Cond _ | Case _ -> false
        | _ -> List.exists direct (W.children term)
      in
      direct step
  | _ -> false

type cache_key =
  | Residual_key of int
  | Cooked_key of Flow.Ty.t * int
  | Cooked_list_key of E.value array
  | Frame_key of C.frame_field

type lowered =
  | Known of E.value
  | Computed of int * Flow.Ty.t
  | Record_fields of (string * lowered) list
  | Record_choice of lowered * lowered * lowered
  | Selected of lowered array * lowered
  | Callable of callable

and callable =
  | Closure of {
      params : (W.pattern * Flow.Ty.t option) list;
      body : W.term;
      env : lowered Names.t;
      instance : int;
      prefix : W.path;
      zone : W.path;
      identity : int;
    }
  | Named of { name : string; identity : int }

let frame emit statements cache source ty field =
  let key = Frame_key field in
  match Hashtbl.find_opt cache key with
  | Some value -> value
  | None ->
      let value = emit statements source ty (C.Frame field) in
      Hashtbl.add cache key value;
      value

(* The expansions by name (noise3, color/hsl, color/gradient) are for the shipped operators only:
   a workspace operator that reuses the name keeps its own body and declines. *)
let shipped (o : Flow.Op.t) =
  List.memq o Flow.Op.all || List.memq o (Flow.Op.of_context Flow.Context.value)

let packed_candidate (x : W.term) =
  let text_input terms =
    List.exists (fun (term : W.term) -> term.ty = Flow.Ty.Array Flow.Ty.Text) terms
  in
  let rec geometry_accumulator = function
    | ty when ty = Flow.Ty.geometry -> true
    | Flow.Ty.Record fields -> List.exists (fun (_, ty) -> geometry_accumulator ty) fields
    | _ -> false
  in
  let numeric =
    (not (geometry_collection x.ty))
    && x.ty <> Flow.Ty.Array Flow.Ty.Text
    && x.ty <> Flow.Ty.Text
    &&
    match x.node with
    | Loop { kind = `Fold; _ } when geometry_accumulator x.ty -> false
    | W.Hof (_, _ :: rest) when text_input rest -> false
    | Loop { clauses; _ } when text_input (List.map snd clauses) -> false
    | W.Hof ((`Map | `Reduce), _ :: rest) ->
        List.exists
          (fun (term : W.term) -> match term.ty with Flow.Ty.Array _ -> true | _ -> false)
          rest
    | Loop { clauses; _ } ->
        List.exists
          (fun (_, (term : W.term)) -> match term.ty with Flow.Ty.Array _ -> true | _ -> false)
          clauses
    | Op { op = "array/sum"; _ } -> true
    | _ -> false
  in
  let text =
    match x.node with
    | W.Hof (`Map, _ :: rest) -> x.ty = Flow.Ty.Array Flow.Ty.Text || text_input rest
    | Loop { kind = `For; clauses; _ } ->
        x.ty = Flow.Ty.Array Flow.Ty.Text || text_input (List.map snd clauses)
    | _ -> false
  in
  (numeric, text)

type draw_context = {
  value : C.source -> E.value -> int; (* the local holding a constant or residual value *)
  local : Flow.Ty.t -> int;
  emit : C.source -> C.statement_node -> unit;
}

let constant_storage (program : C.program) =
  let uses = Array.make (Array.length program.locals) []
  and writes = Array.make (Array.length program.locals) 0 in
  let use n consumer = uses.(n) <- consumer :: uses.(n) in
  let unsafe n = use n None in
  let rec scan (statement : C.statement) =
    (match statement.node with
    | Assign (dst, expression) -> (
        incr_write dst;
        match expression with
        | Local src when program.locals.(src) = program.locals.(dst) -> use src (Some dst)
        | Host_source { ty; args; _ } when Flow.Ty.is_cooked ty ->
            List.iter
              (fun (_, src) ->
                match program.locals.(src) with
                | Flow.Ty.Array _ -> use src (Some (-1))
                | _ -> unsafe src)
              args
        | Constant _ | Frame _ | Call_index _ | Source _ -> ()
        | Local src | Field (src, _) -> unsafe src
        | Prim (_, args) | Fields args | Host_source { args; _ } | Source_descriptor { args; _ } ->
            List.iter (fun (_, src) -> unsafe src) args
        | Vec args | Items args -> List.iter unsafe args)
    | If { condition; _ } -> unsafe condition
    | Counted { first; last; count; index; item; stop; _ } ->
        List.iter unsafe (first :: last :: Option.to_list stop);
        List.iter incr_write [ count; index; item ]
    | Iterate { sequence; index; item; _ } ->
        unsafe sequence;
        List.iter incr_write [ index; item ]
    | Store { sequence; index; value } -> List.iter unsafe [ sequence; index; value ]
    | Invoke { args; result; _ } ->
        List.iter unsafe args;
        incr_write result
    | State_read { result; initial; _ } ->
        unsafe initial;
        incr_write result
    | State_peek { found; value; _ } -> List.iter incr_write [ found; value ]
    | State_write { value; _ } -> unsafe value
    | Diagnose_rows { rows; index; diagnostic; result; _ } ->
        List.iter unsafe [ rows; diagnostic ];
        List.iter incr_write [ index; result ]
    | Protect { value; failure; mapped; result; _ } ->
        List.iter unsafe [ value; mapped ];
        List.iter incr_write [ failure; result ]
    | Reset_call _ -> ()
    | Draw draw -> (
        match draw with
        | Background n -> unsafe n
        | Point { at; ink } -> List.iter unsafe [ at; ink ]
        | Mark { args; _ } | Batch { args; _ } | Path { args; _ } ->
            List.iter (fun (_, n) -> unsafe n) args
        | Points { positions; ink } -> List.iter unsafe [ positions; ink ]
        | Text_run { at; text; ink; size } -> List.iter unsafe [ at; text; ink; size ]
        | Image { at; scale; angle; _ } -> List.iter unsafe [ at; scale; angle ]
        | Push (_, n) -> unsafe n
        | Pop -> ()));
    List.iter scan (C.children statement)
  and incr_write n = writes.(n) <- writes.(n) + 1 in
  List.iter scan program.body;
  Array.iter unsafe program.results;
  Array.iter
    (fun (slot : C.state_slot) ->
      List.iter (function C.Local_index n -> unsafe n | Static_index _ -> ()) slot.source.iter)
    program.state_slots;
  let rec safe seen n =
    writes.(n) = 1
    && (not (List.mem n seen))
    && uses.(n) <> []
    && List.for_all
         (function Some -1 -> true | Some next -> safe (n :: seen) next | None -> false)
         uses.(n)
  in
  let rec rewrite (statement : C.statement) =
    let node : C.statement_node =
      match statement.node with
      | Assign
          ( dst,
            Prim
              (((Array_int | Array_text | Array_float | Array_vec3 | Array_vec4) as primitive), args)
          )
        when safe [] dst ->
          Assign
            ( dst,
              Source_descriptor
                {
                  name = "$core/constant-array/" ^ C.primitive_name primitive;
                  ty = program.locals.(dst);
                  args;
                } )
      | If arms -> If { arms with yes = List.map rewrite arms.yes; no = List.map rewrite arms.no }
      | Diagnose_rows scope -> Diagnose_rows { scope with handler = List.map rewrite scope.handler }
      | Protect scope ->
          Protect
            {
              scope with
              body = List.map rewrite scope.body;
              handler = List.map rewrite scope.handler;
            }
      | Counted loop ->
          Counted { loop with init = List.map rewrite loop.init; body = List.map rewrite loop.body }
      | Iterate loop -> Iterate { loop with body = List.map rewrite loop.body }
      | State_read state -> State_read { state with init = List.map rewrite state.init }
      | node -> node
    in
    { statement with node }
  in
  { program with body = List.map rewrite program.body }

let sequence_operation ~primitive ~ensure ~sequence_count ~sequence_get ~iterate ~store ~hold
    ~update ~branch ~coerce ~local ~number ~statement statements cache source ty name args =
  let arg key = List.assoc key args in
  let prim ty op args = primitive statements cache source ty op args in
  let float value = prim Flow.Ty.Float C.Float_convert [ ("x", value) ] in
  match name with
  | "range" ->
      let first, last =
        match List.assoc_opt "end" args with
        | Some last -> (arg "count", last)
        | None -> (Known (V.Int 0), arg "count")
      in
      let first = ensure statements cache source first in
      let last = ensure statements cache source last in
      let count = local Flow.Ty.Int
      and index = local Flow.Ty.Int
      and item = local Flow.Ty.Int
      and id = number () in
      let init = ref [] in
      let result =
        primitive init cache source ty C.Sequence_build [ ("count", Computed (count, Flow.Ty.Int)) ]
      in
      let body = ref [] in
      store body cache source result (Computed (index, Flow.Ty.Int)) (Computed (item, Flow.Ty.Int));
      statements :=
        statement id source
          (C.Counted
             {
               first;
               last;
               count;
               index;
               item;
               limit = 4096;
               stop = None;
               init = List.rev !init;
               body = List.rev !body;
             })
        :: !statements;
      result
  | "take" | "drop" | "rest" ->
      let sequence = arg "list" in
      let count = sequence_count statements cache source sequence in
      let wanted =
        if name = "rest" then Known (V.Int 1)
        else coerce statements cache source Flow.Ty.Int (float (arg "n"))
      in
      let retained =
        prim Flow.Ty.Int (C.Numeric Flow.Packed_ops.Max) [ ("a", Known (V.Int 0)); ("b", wanted) ]
      in
      let retained =
        prim Flow.Ty.Int (C.Numeric Flow.Packed_ops.Min) [ ("a", retained); ("b", count) ]
      in
      let first, length =
        if name = "take" then (Known (V.Int 0), retained)
        else
          ( retained,
            prim Flow.Ty.Int (C.Numeric Flow.Packed_ops.Sub) [ ("a", count); ("b", retained) ] )
      in
      prim ty C.Sequence_slice [ ("list", sequence); ("first", first); ("count", length) ]
  | "reverse" ->
      let sequence = arg "list" in
      let count = sequence_count statements cache source sequence in
      let result = prim ty C.Sequence_build [ ("count", count) ] in
      iterate statements cache source sequence (fun body cache index item ->
          let last =
            primitive body cache source Flow.Ty.Int (C.Numeric Flow.Packed_ops.Sub)
              [ ("a", count); ("b", Known (V.Int 1)) ]
          in
          let reverse =
            primitive body cache source Flow.Ty.Int (C.Numeric Flow.Packed_ops.Sub)
              [ ("a", last); ("b", index) ]
          in
          store body cache source result reverse item);
      result
  | "first" | "last" ->
      let sequence = arg "list" in
      let index =
        if name = "first" then Known (V.Int 0)
        else
          prim Flow.Ty.Int (C.Numeric Flow.Packed_ops.Sub)
            [ ("a", sequence_count statements cache source sequence); ("b", Known (V.Int 1)) ]
      in
      sequence_get statements cache source sequence index
  | "array/sum" ->
      let zero =
        match ty with
        | Flow.Ty.Int -> V.Int 0
        | Vec2 -> V.Vec2 (0., 0.)
        | Vec3 -> V.Vec3 (0., 0., 0.)
        | Vec4 -> V.Vec4 (0., 0., 0., 0.)
        | _ -> V.Float 0.
      in
      let total = hold ?ty:(Some ty) statements cache source (Known zero) in
      iterate statements cache source (arg "array") (fun body cache _ item ->
          update body cache source total
            (primitive body cache source ty
               (if ty = Flow.Ty.Int then C.Integer Flow.Packed_ops.Iadd
                else C.Numeric Flow.Packed_ops.Add)
               [ ("a", total); ("b", item) ]));
      total
  | "linspace" ->
      let a = float (arg "from") in
      let b = float (arg "to") in
      let n = arg "count" in
      let first = ensure statements cache source (Known (V.Int 0)) in
      let last = ensure statements cache source n in
      let count = local Flow.Ty.Int
      and index = local Flow.Ty.Int
      and item = local Flow.Ty.Int
      and id = number () in
      let init = ref [] in
      let result =
        primitive init cache source ty C.Sequence_build [ ("count", Computed (count, Flow.Ty.Int)) ]
      in
      let body = ref [] in
      let single =
        primitive body cache source Flow.Ty.Bool (C.Numeric Flow.Packed_ops.Eq)
          [ ("a", n); ("b", Known (V.Int 1)) ]
      in
      let value =
        branch body cache source Flow.Ty.Float single
          (fun _ _ -> a)
          (fun body cache ->
            let p ty op args = primitive body cache source ty op args in
            let k = p Flow.Ty.Float C.Float_convert [ ("x", Computed (item, Flow.Ty.Int)) ] in
            let steps =
              p Flow.Ty.Float C.Float_convert
                [
                  ( "x",
                    p Flow.Ty.Int (C.Numeric Flow.Packed_ops.Sub)
                      [ ("a", n); ("b", Known (V.Int 1)) ] );
                ]
            in
            let span = p Flow.Ty.Float (C.Numeric Flow.Packed_ops.Sub) [ ("a", b); ("b", a) ] in
            let scaled =
              p Flow.Ty.Float (C.Numeric Flow.Packed_ops.Mul) [ ("a", span); ("b", k) ]
            in
            p Flow.Ty.Float (C.Numeric Flow.Packed_ops.Add)
              [
                ("a", a);
                ( "b",
                  p Flow.Ty.Float (C.Numeric Flow.Packed_ops.Div) [ ("a", scaled); ("b", steps) ] );
              ])
      in
      store body cache source result (Computed (index, Flow.Ty.Int)) value;
      statements :=
        statement id source
          (C.Counted
             {
               first;
               last;
               count;
               index;
               item;
               limit = 4096;
               stop = None;
               init = List.rev !init;
               body = List.rev !body;
             })
        :: !statements;
      result
  | _ -> refuse name

let color_operation ~primitive ~field ~branch ~emit ~ensure statements cache source ty name args =
  let arg key = List.assoc key args in
  let float value =
    primitive statements cache source Flow.Ty.Float C.Float_convert [ ("x", value) ]
  in
  let vec components =
    emit statements source Flow.Ty.Vec3
      (C.Vec (List.map (ensure statements cache source) components))
  in
  match name with
  | "color/hsl" ->
      let h = float (arg "h") in
      let s = float (arg "s") in
      let l = float (arg "l") in
      let f x = Known (V.Float x) in
      let c ty op args = primitive statements cache source ty op args in
      let calc2 op a b = c Flow.Ty.Float (C.Numeric op) [ ("a", a); ("b", b) ] in
      let unit v = calc2 Flow.Packed_ops.Max (f 0.) (calc2 Flow.Packed_ops.Min (f 1.) v) in
      let hue = calc2 Flow.Packed_ops.Div (calc2 Flow.Packed_ops.Mod h (f 360.)) (f 60.) in
      let sat = unit s in
      let light = unit l in
      let abs v = c Flow.Ty.Float (C.Math Flow.Packed_ops.Abs) [ ("x", v) ] in
      let chroma =
        calc2 Flow.Packed_ops.Mul
          (calc2 Flow.Packed_ops.Sub (f 1.)
             (abs (calc2 Flow.Packed_ops.Sub (calc2 Flow.Packed_ops.Mul (f 2.) light) (f 1.))))
          sat
      in
      let x =
        calc2 Flow.Packed_ops.Mul chroma
          (calc2 Flow.Packed_ops.Sub (f 1.)
             (abs (calc2 Flow.Packed_ops.Sub (calc2 Flow.Packed_ops.Mod hue (f 2.)) (f 1.))))
      in
      let rgb body cache r g b =
        emit body source Flow.Ty.Vec3 (C.Vec (List.map (ensure body cache source) [ r; g; b ]))
      in
      let zero = f 0. in
      let arm limit yes no =
        branch statements cache source Flow.Ty.Vec3
          (c Flow.Ty.Bool (C.Numeric Flow.Packed_ops.Lt) [ ("a", hue); ("b", f limit) ])
          yes no
      in
      let triple =
        arm 1.
          (fun b k -> rgb b k chroma x zero)
          (fun _ _ ->
            arm 2.
              (fun b k -> rgb b k x chroma zero)
              (fun _ _ ->
                arm 3.
                  (fun b k -> rgb b k zero chroma x)
                  (fun _ _ ->
                    arm 4.
                      (fun b k -> rgb b k zero x chroma)
                      (fun _ _ ->
                        arm 5. (fun b k -> rgb b k x zero chroma) (fun b k -> rgb b k chroma zero x)))))
      in
      let m = calc2 Flow.Packed_ops.Sub light (calc2 Flow.Packed_ops.Div chroma (f 2.)) in
      let channel name =
        let byte =
          c Flow.Ty.Int (C.Color_byte C.Clamped)
            [
              ( "x",
                calc2 Flow.Packed_ops.Add
                  (field statements cache source Flow.Ty.Float triple name)
                  m );
            ]
        in
        calc2 Flow.Packed_ops.Div (c Flow.Ty.Float C.Float_convert [ ("x", byte) ]) (f 255.)
      in
      let r = channel "x" in
      let g = channel "y" in
      let b = channel "z" in
      vec [ r; g; b ]
  | "color/gradient" ->
      (* Constant opaque stops only; anything else keeps the walker's diagnostics. *)
      let colors =
        match arg "stops" with
        | Known (V.List xs) when Array.length xs > 0 -> (
            try Array.map Flow.Op.drawing_color xs with Diagnostic.Error _ -> refuse name)
        | _ -> refuse name
      in
      if Array.exists (fun (c : Rays_math.Color.t) -> c.a <> 255) colors then refuse name;
      let n = Array.length colors in
      let channels (color : Rays_math.Color.t) =
        let r, g, b, _ = Rays_math.Color.to_floats color in
        V.Vec3 (r, g, b)
      in
      if n = 1 then Known (channels colors.(0))
      else begin
        let f x = Known (V.Float x) in
        let c ty op args = primitive statements cache source ty op args in
        let calc2 op a b = c Flow.Ty.Float (C.Numeric op) [ ("a", a); ("b", b) ] in
        let unit v = calc2 Flow.Packed_ops.Max (f 0.) (calc2 Flow.Packed_ops.Min (f 1.) v) in
        let position =
          calc2 Flow.Packed_ops.Mul (unit (float (arg "x"))) (f (float_of_int (n - 1)))
        in
        let left =
          c Flow.Ty.Int (C.Numeric Flow.Packed_ops.Min)
            [ ("a", Known (V.Int (n - 2))); ("b", c Flow.Ty.Int C.Int_convert [ ("x", position) ]) ]
        in
        let pct =
          unit
            (calc2 Flow.Packed_ops.Sub position (c Flow.Ty.Float C.Float_convert [ ("x", left) ]))
        in
        let inverse = calc2 Flow.Packed_ops.Sub (f 1.) pct in
        let pick body cache k =
          let blend (c1 : Rays_math.Color.t) (c2 : Rays_math.Color.t) pick_channel =
            let p ty op args = primitive body cache source ty op args in
            let m op a b = p Flow.Ty.Float (C.Numeric op) [ ("a", a); ("b", b) ] in
            let a = m Flow.Packed_ops.Mul (f (float_of_int (pick_channel c1))) inverse in
            let b = m Flow.Packed_ops.Mul (f (float_of_int (pick_channel c2))) pct in
            let byte =
              p Flow.Ty.Int C.Int_convert
                [ ("x", m Flow.Packed_ops.Add (m Flow.Packed_ops.Add a b) (f 0.5)) ]
            in
            m Flow.Packed_ops.Div (p Flow.Ty.Float C.Float_convert [ ("x", byte) ]) (f 255.)
          in
          let c1 = colors.(k) and c2 = colors.(k + 1) in
          let r = blend c1 c2 (fun c -> c.r) in
          let g = blend c1 c2 (fun c -> c.g) in
          let b = blend c1 c2 (fun c -> c.b) in
          emit body source Flow.Ty.Vec3 (C.Vec (List.map (ensure body cache source) [ r; g; b ]))
        in
        let rec arms body cache k =
          if k = n - 2 then pick body cache k
          else
            let here =
              primitive body cache source Flow.Ty.Bool (C.Numeric Flow.Packed_ops.Eq)
                [ ("a", left); ("b", Known (V.Int k)) ]
            in
            branch body cache source Flow.Ty.Vec3 here
              (fun body cache -> pick body cache k)
              (fun body cache -> arms body cache (k + 1))
        in
        arms statements cache 0
      end
  | _ -> refuse name

(* [draw] writes the program's statements in place of lowering one residual; only a residual
   reached through [value] forces [root], which then stands for the whole evaluation. *)
module Lowerer (P : sig
  val trace : ((Packed.t, Diagnostic.t list) result -> unit) option
  val budget : int option
  val strict : bool
  val region_allowed : C.Region.t -> bool
  val draw : (draw_context -> unit) option
  val root : E.residual Lazy.t
  val lowered : E.residual -> unit
end) =
struct
  open P

  let constants = ref []
  and locals = ref []
  and next_constant = ref 0
  and next_local = ref 0
  and next_statement = ref 0

  let budget = ref (match budget with Some b -> b | None -> if draw = None then 2048 else 32768)
  and active = Hashtbl.create 8

  let calls = Hashtbl.create 8
  and next_call = ref 0
  and next_function = ref (-1)

  let mixed_numeric = Hashtbl.create 8
  let current = ref None

  let regions = ref []
  and slots = ref []
  and next_slot = ref 0
  and plane = ref `Frame
  and geometry = ref false

  let new_identity () =
    let n = !next_function in
    decr next_function;
    n

  let call_slot identity =
    match Hashtbl.find_opt calls identity with
    | Some n -> n
    | None ->
        let n = !next_call in
        incr next_call;
        Hashtbl.add calls identity n;
        n

  let local ty =
    let n = !next_local in
    incr next_local;
    locals := ty :: !locals;
    n

  let number () =
    let n = !next_statement in
    incr next_statement;
    n

  let statement id source node : C.statement = { id; source; node }

  let emit statements source ty expression =
    let n = local ty in
    if
      ty = Flow.Ty.Float
      &&
      match expression with
      | C.Prim (Numeric _, _) | Prim (Sequence_get, _) | Field _ -> true
      | Local old -> Hashtbl.mem mixed_numeric old
      | _ -> false
    then Hashtbl.add mixed_numeric n ();
    statements := statement (number ()) source (Assign (n, expression)) :: !statements;
    Computed (n, ty)

  let rec ensure statements cache source = function
    | Computed (n, _) -> n
    | Callable _ -> refuse "fn"
    | Selected (items, index) ->
        ensure statements cache source
          (select statements cache source
             (type_of (Selected (items, index)))
             items index
             (fun _ _ v -> v))
    | Known (V.Residual r) -> ensure statements cache source (residual statements cache r)
    | Known ((V.Source _ | Struct (_, Flow.Ty.Array _, _)) as value) when closure_free value -> (
        match emit statements source (V.ty_of value) (Source value) with
        | Computed (n, _) -> n
        | _ -> assert false)
    | Known (V.Source (name, ty, fields) | Struct (name, (Flow.Ty.Array _ as ty), fields)) -> (
        (* a capture inside a source is lowered here, so the program holds plain data only *)
        let args =
          List.map (fun (field, v) -> (field, ensure statements cache source (Known v))) fields
        in
        match emit statements source ty (Host_source { name; ty; args }) with
        | Computed (n, _) -> n
        | _ -> assert false)
    | Known (V.Deferred (ty, id)) when Flow.Ty.is_cooked ty ->
        let key = Cooked_key (ty, id) in
        let value =
          match Hashtbl.find_opt cache key with
          | Some value -> value
          | None ->
              let value =
                emit statements source ty
                  (Source (V.Source ("$core/cooked", ty, [ ("id", V.Int id) ])))
              in
              Hashtbl.add cache key value;
              value
        in
        ensure statements cache source value
    | Known (V.Record fields) ->
        ensure statements cache source
          (Record_fields (List.map (fun (name, v) -> (name, Known v)) fields))
    | Known (V.List items) when C.literal_of_value (V.List items) = None ->
        let ty = type_of (Known (V.List items)) in
        if Array.length items > 4096 && not (geometry_collection ty) then refuse "list_limit";
        let cooked =
          geometry_collection ty
          && Array.for_all
               (function
                 | V.Deferred (ty, _) -> ty = Flow.Ty.geometry | V.No_geo -> true | _ -> false)
               items
        in
        let key = Cooked_list_key items in
        let result =
          match if cooked then Hashtbl.find_opt cache key else None with
          | Some result -> result
          | None ->
              let result =
                primitive statements cache source ty C.Sequence_build
                  [ ("count", Known (V.Int (Array.length items))) ]
              in
              Array.iteri
                (fun index item ->
                  store statements cache source result (Known (V.Int index)) (Known item))
                items;
              if cooked then Hashtbl.add cache key result;
              result
        in
        ensure statements cache source result
    | Record_choice (condition, yes, no) as value ->
        let condition = ensure statements cache source condition in
        let id = number () and result = local (type_of value) in
        let arm value =
          let body = ref [] and cache = Hashtbl.copy cache in
          let n = ensure body cache source value in
          body := statement (number ()) source (Assign (result, Local n)) :: !body;
          List.rev !body
        in
        let yes = arm yes in
        let no = arm no in
        statements := statement id source (If { condition; yes; no }) :: !statements;
        result
    | Record_fields fields -> (
        let ty = Flow.Ty.Record (List.map (fun (name, v) -> (name, type_of v)) fields) in
        let fields = List.map (fun (name, v) -> (name, ensure statements cache source v)) fields in
        match emit statements source ty (Fields fields) with
        | Computed (n, _) -> n
        | _ -> assert false)
    | Known value -> (
        let value_literal =
          match C.literal_of_value value with Some v -> v | None -> refuse "capture"
        in
        let index = !next_constant in
        incr next_constant;
        constants := value_literal :: !constants;
        match emit statements source (V.ty_of value) (Constant index) with
        | Computed (n, _) -> n
        | _ -> assert false)

  and primitive statements cache source ty op args =
    (* Geometry rest slots already have native collection capacity. This internal
       builder flag follows their checked type, never an authored count option. *)
    let args =
      if op = C.Sequence_build && geometry_collection ty then
        args @ [ ("cooked", Known (V.Bool true)) ]
      else args
    in
    let args = List.map (fun (name, v) -> (name, ensure statements cache source v)) args in
    emit statements source ty (Prim (op, args))

  and known_values args =
    let rec all = function
      | [] -> Some []
      | (name, Known v) :: xs when C.literal_of_value v <> None ->
          Option.map (fun xs -> (name, v) :: xs) (all xs)
      | _ -> None
    in
    all args

  and field statements cache source ty value name =
    match resolve statements cache value with
    | Selected (items, index) ->
        select statements cache source ty items index (fun body cache v ->
            field body cache source ty v name)
    | Record_choice (condition, yes, no) as value ->
        let ty =
          match type_of value with
          | Flow.Ty.Record fields -> Option.value ~default:ty (List.assoc_opt name fields)
          | _ -> ty
        in
        branch statements cache source ty condition
          (fun body cache -> field body cache source ty yes name)
          (fun body cache -> field body cache source ty no name)
    | Record_fields fields -> (
        match List.assoc_opt name fields with
        | Some v -> resolve statements cache v
        | None -> refuse "get")
    | Known v ->
        let v =
          match (v, name) with
          | V.Record xs, name -> (
              match List.assoc_opt name xs with Some v -> v | None -> refuse "get")
          | Vec2 (x, _), "x" -> V.Float x
          | Vec2 (_, y), "y" -> Float y
          | Vec3 (x, _, _), "x" -> V.Float x
          | Vec3 (_, y, _), "y" -> Float y
          | Vec3 (_, _, z), "z" -> Float z
          | Vec4 (x, _, _, _), "x" -> V.Float x
          | Vec4 (_, y, _, _), "y" -> Float y
          | Vec4 (_, _, z, _), "z" -> Float z
          | Vec4 (_, _, _, w), "w" -> Float w
          | _ -> refuse "get"
        in
        resolve statements cache (Known v)
    | Callable _ -> refuse "get"
    | Computed (n, value_ty) ->
        let ty =
          match value_ty with
          | Flow.Ty.Vec2 | Vec3 | Vec4 -> Flow.Ty.Float
          | Record fields -> Option.value ~default:ty (List.assoc_opt name fields)
          | _ -> ty
        in
        emit statements source ty (Field (n, name))

  and coerce statements cache source ty = function
    | Selected (items, index) ->
        select statements cache source ty items index (fun body cache v ->
            coerce body cache source ty v)
    | Record_choice _ as value ->
        map_fields statements cache source value (fun body cache fields ->
            match coerce body cache source ty (Record_fields fields) with
            | Record_fields fields -> fields
            | _ -> fields)
    | Known value -> (
        try Known (V.coerce_to ty value)
        with Diagnostic.Error d -> primitive statements cache source ty (failure d) [])
    | Record_fields fields as v -> (
        match ty with
        | Flow.Ty.Any -> v
        | Record wanted ->
            Record_fields
              (List.map
                 (fun (name, v) ->
                   ( name,
                     match List.assoc_opt name wanted with
                     | Some ty -> coerce statements cache source ty v
                     | None -> v ))
                 fields)
        | _ -> v)
    | Callable _ as v when Flow.Ty.has_fn ty || ty = Flow.Ty.Any -> v
    | Callable _ -> refuse "fn"
    | Computed (n, have) as v
      when ty = Flow.Ty.Any || (have = ty && not (Hashtbl.mem mixed_numeric n)) ->
        v
    | value -> primitive statements cache source ty (Coerce ty) [ ("value", value) ]

  and source_of_view (view : E.Residual.residual_view) : C.source =
    {
      site = List.drop (List.length view.prefix) view.site;
      owner = view.owner;
      instance = view.instance;
      prefix = view.prefix;
      iter = List.map (fun n -> C.Static_index n) view.iter;
      span = None;
    }

  and type_of = function
    | Selected (items, _) ->
        Array.fold_left
          (fun ty v -> Option.value ~default:ty (Flow.Ty.join ty (type_of v)))
          Flow.Ty.Any items
    | Known (V.Residual r) -> (E.Residual.residual_view r).term.ty
    | Known (V.List items) ->
        Flow.Ty.List
          (Array.fold_left
             (fun ty v -> Option.value ~default:ty (Flow.Ty.join ty (type_of (Known v))))
             Flow.Ty.Any items)
    | Known v -> V.ty_of v
    | Computed (_, ty) -> ty
    | Callable _ -> Flow.Ty.Fn None
    | Record_fields fields -> Flow.Ty.Record (List.map (fun (name, v) -> (name, type_of v)) fields)
    | Record_choice (_, yes, no) ->
        Option.value ~default:Flow.Ty.Any (Flow.Ty.join (type_of yes) (type_of no))

  and primitive_of_name name =
    match Flow.Packed_ops.binary name with
    | Some op -> Some (C.Numeric op)
    | None -> (
        match Flow.Packed_ops.unary name with
        | Some op -> Some (C.Math op)
        | None -> (
            match name with
            | "exp" -> Some C.Exp
            | "floor" -> Some Floor
            | "ceil" -> Some Ceil
            | "round" -> Some Round
            | "int" -> Some Int_convert
            | "float" -> Some Float_convert
            | "equal?" -> Some Equal
            | "text/blank?" -> Some Text_blank
            | "text/trim" -> Some Text_trim
            | "text/lower" -> Some Text_lower
            | "text/repr" -> Some Text_repr
            | "error" -> Some Error
            | "fma" -> Some Fma
            | "float/next_after" -> Some Next_after
            | "float/mul_checked" -> Some Checked_mul
            | "float/fma_checked" -> Some Checked_fma
            | "text/table_decode" -> Some Text_table_decode
            | "text/table_encode" -> Some Text_table_encode
            | "text/split" -> Some Text_split
            | "text/tokens" -> Some Text_tokens
            | "text/to_float" -> Some Text_to_float
            | "text/to_int" -> Some Text_to_int
            | "array/grade" -> Some Array_grade
            | "array/index_of" -> Some Array_index_of
            | "array/compact" -> Some Array_compact
            | "array/scan_sum" -> Some Array_scan_sum
            | "array/scatter_add" -> Some Array_scatter_add
            | "array/min" -> Some (Array_reduce Minimum)
            | "array/max" -> Some (Array_reduce Maximum)
            | "array/segment_sum" -> Some (Array_segment Sum)
            | "array/segment_min" -> Some (Array_segment Minimum)
            | "array/segment_max" -> Some (Array_segment Maximum)
            | "array/segment_argmax" -> Some Array_segment_argmax
            | "int/add" -> Some (Integer Iadd)
            | "int/sub" -> Some (Integer Isub)
            | "int/mul" -> Some (Integer Imul)
            | "int/div" -> Some (Integer Idiv)
            | "int/mod" -> Some (Integer Imod)
            | "int/and" -> Some (Integer Iand)
            | "int/or" -> Some (Integer Ior)
            | "int/xor" -> Some (Integer Ixor)
            | "int/shl" -> Some (Integer Ishl)
            | "int/shr" -> Some (Integer Ishr)
            | "int/lt" -> Some (Integer_compare Ilt)
            | "int/le" -> Some (Integer_compare Ile)
            | "int/gt" -> Some (Integer_compare Igt)
            | "int/ge" -> Some (Integer_compare Ige)
            | "int/eq" -> Some (Integer_compare Ieq)
            | "array/max_length" -> Some Array_max_length
            | "array/range" -> Some Array_range
            | "array/int" -> Some Array_int
            | "array/text" -> Some Array_text
            | "array/float" -> Some Array_float
            | "array/vec3" -> Some Array_vec3
            | "array/vec4" -> Some Array_vec4
            | "array/nth" -> Some Array_nth
            | "array/count" -> Some Array_count
            | "array/slice" -> Some Array_slice
            | "array/concat" -> Some Array_concat
            | "value/rand" -> Some Rand
            | "rand/at" -> Some Rand_at
            | "noise3" -> Some Noise3
            | _ -> None))

  and residual statements cache r =
    let id = E.Residual.residual_id r in
    let key = Residual_key id in
    match Hashtbl.find_opt cache key with
    | Some value -> value
    | None ->
        let view = E.Residual.residual_view r in
        if Hashtbl.mem active id then refuse "cycle";
        Hashtbl.add active id ();
        let saved = (!plane, !geometry) in
        let saved_current = !current in
        current := Some r;
        plane := if view.host then `Host else `Frame;
        geometry := view.geometry;
        Fun.protect
          ~finally:(fun () ->
            Hashtbl.remove active id;
            plane := fst saved;
            geometry := snd saved;
            current := saved_current)
          (fun () ->
            let env =
              Names.of_seq (List.to_seq (List.map (fun (name, v) -> (name, Known v)) view.bindings))
            in
            let source = source_of_view view in
            (* A draw's captured arrays need explicit producers, especially resident folds.
               Scalar-only captures retain the direct packed path and its grouping. *)
            let draw_arrays =
              draw <> None
              && List.exists
                   (fun name ->
                     match List.assoc_opt name view.bindings with
                     | Some (V.Residual r) -> (
                         match (E.Residual.residual_view r).term.ty with
                         | Flow.Ty.Array _ -> true
                         | _ -> false)
                     | _ -> false)
                   (E.Residual.free_names view.term)
            in
            let packed = if view.previous || draw_arrays then None else packed_region r view in
            let value =
              if Option.is_some packed then region statements (Option.get packed) view
              else if view.previous then
                state_term statements cache
                  { source with C.span = Some view.term.form.span }
                  env view.term true
              else term statements cache source env view.term
            in
            Hashtbl.add cache key value;
            lowered r;
            value)

  and descriptor statements cache source = function
    | Known (V.Source (name, ty, fields) | V.Struct (name, (Flow.Ty.Array _ as ty), fields))
      when storage_argument ty ->
        emit statements source ty
          (Source_descriptor
             {
               name;
               ty;
               args =
                 List.map
                   (fun (name, value) -> (name, ensure statements cache source (Known value)))
                   fields;
             })
    | Known (V.Record fields) ->
        Record_fields
          (List.map
             (fun (name, value) -> (name, descriptor statements cache source (Known value)))
             fields)
    | Record_fields fields ->
        Record_fields
          (List.map (fun (name, value) -> (name, descriptor statements cache source value)) fields)
    | value -> value

  and storage_argument = function
    | Flow.Ty.Array _ -> true
    | Record fields -> List.exists (fun (_, ty) -> storage_argument ty) fields
    | _ -> false

  and term ?(borrow = false) statements cache source env (x : W.term) =
    decr budget;
    if !budget < 0 then refuse "budget";
    let source =
      { source with C.site = Option.value x.path ~default:source.site; span = Some x.form.span }
    in
    let compile = term statements cache source env in
    let numeric_candidate, text_candidate = packed_candidate x in
    let candidate = numeric_candidate || text_candidate in
    (* A State step's maps are deferred past the static pass. Compile those nested kernels
       under their state residual; other nested candidates keep the existing strict/draw gate. *)
    let state_parent =
      Option.fold ~none:false ~some:(fun parent -> state_step_candidate parent x) !current
    in
    (* a kernel reading a state value takes it as an argument, lowered once before the region *)
    let reads_state () =
      candidate
      && List.exists
           (fun name ->
             match Names.find_opt name env with
             | Some (Known (V.Residual r)) -> E.state_dependent (V.Residual r)
             | _ -> false)
           (E.Residual.free_names x)
    in
    let packed =
      if (not candidate) || not (strict || draw <> None || state_parent || reads_state ()) then None
      else
        Option.bind !current (fun parent ->
            let names = E.Residual.free_names x in
            let arguments = ref [] in
            let rec capture = function
              (* in a drawing, and for any state-dependent capture, a captured residual is lowered first and
           passed as an argument: the map then reads no state itself, even when the value it was handed did *)
              | Known (V.Residual r) when draw <> None || E.state_dependent (V.Residual r) ->
                  capture (residual statements cache r)
              (* a record local is passed field by field: Packed takes scalars, not a record argument *)
              | Computed (id, Flow.Ty.Record fields) when draw <> None ->
                  capture
                    (Record_fields
                       (List.map
                          (fun (name, ty) -> (name, emit statements source ty (Field (id, name))))
                          fields))
              | Known value -> value
              | Computed (id, ty) ->
                  let index =
                    match List.find_index (( = ) id) !arguments with
                    | Some index -> index
                    | None ->
                        let index = List.length !arguments in
                        arguments := !arguments @ [ id ];
                        index
                  in
                  V.Source ("$core/argument", ty, [ ("index", V.Int index) ])
              | Record_fields fields ->
                  V.Record (List.map (fun (name, v) -> (name, capture v)) fields)
              | Callable (Closure cl) ->
                  V.Fn
                    (E.Residual.compiled_function parent ~instance:cl.instance ~prefix:cl.prefix
                       ~zone:cl.zone ~params:cl.params ~body:cl.body
                       ~bindings:
                         (List.map (fun (name, v) -> (name, capture v)) (Names.bindings cl.env)))
              | _ -> refuse "packed_capture"
            in
            try
              let bindings =
                List.filter_map
                  (fun name -> Option.map (fun v -> (name, capture v)) (Names.find_opt name env))
                  names
              in
              let r = E.Residual.with_bindings parent ~term:x bindings in
              if E.state_dependent (V.Residual r) then None
              else
                Option.bind (Packed.compile ?trace r x) (fun p ->
                    let region = p in
                    if region_allowed region then Some (r, region, !arguments) else None)
            with Unsupported _ -> None)
    in
    match packed with
    | Some (r, packed, args) -> region ~args statements packed (E.Residual.residual_view r)
    | None -> (
        if numeric_candidate && strict then refuse "packed_kernel";
        match x.node with
        | W.Lit (Param.Int_value n) -> Known (V.Int n)
        | Lit (Param.Float_value f) -> Known (V.Float f)
        | Lit (Param.Bool_value b) -> Known (V.Bool b)
        | Text s -> Known (V.Text s)
        | Nil -> Known V.No_geo
        | Record fields ->
            Record_fields
              (List.map
                 (fun (name, t) -> (name, term ~borrow statements cache source env t))
                 fields)
        | List_lit items -> (
            if List.length items > 4096 && not (geometry_collection x.ty) then refuse "list_limit";
            let items = List.map compile items in
            match known_values (List.map (fun v -> ("", v)) items) with
            | Some items ->
                let values = Array.of_list (List.map snd items) in
                let ty = V.elem_ty values in
                Known (V.List (Array.map (V.coerce_to ty) values))
            | None ->
                emit statements source x.ty
                  (Items (List.map (ensure statements cache source) items)))
        | List_op (_, items) ->
            primitive statements cache source x.ty Sequence_concat
              (List.map (fun item -> ("list", compile item)) items)
        | Get (value, name) ->
            field statements cache source x.ty (term ~borrow statements cache source env value) name
        | Assoc (record, updates) ->
            let record = compile record in
            List.fold_left
              (fun record (name, t) ->
                let value = compile t in
                map_fields statements cache source record (fun body cache fields ->
                    match List.assoc_opt name fields with
                    | Some old ->
                        let value = coerce_like body cache source old value in
                        List.map (fun (n, v) -> (n, if n = name then value else v)) fields
                    | None -> fields @ [ (name, value) ]))
              record updates
        | Str ts -> (
            let values = List.map compile ts in
            let texts =
              List.map
                (fun value ->
                  match value with
                  | Known v when C.literal_of_value v <> None ->
                      Known (V.Text (V.show_with Fun.id v))
                  | _ -> primitive statements cache source Flow.Ty.Text Show [ ("value", value) ])
                values
            in
            match known_values (List.map (fun v -> ("", v)) texts) with
            | Some texts ->
                Known
                  (V.Text
                     (String.concat ""
                        (List.map
                           (fun (_, v) -> match v with V.Text s -> s | _ -> assert false)
                           texts)))
            | None ->
                primitive statements cache source Flow.Ty.Text Text_concat
                  (List.map (fun v -> ("part", v)) texts))
        | Time -> frame emit statements cache source Flow.Ty.Float Time
        | State _ -> state_term statements cache source env x false
        | Vec ts when List.length ts >= 2 && List.length ts <= 4 -> (
            let args = List.map compile ts in
            match known_values (List.map (fun v -> ("", v)) args) with
            | Some values ->
                Known
                  (C.value_of_literal
                     (Vector (Array.of_list (List.map (fun (_, v) -> V.num v) values))))
            | _ ->
                emit statements source x.ty (Vec (List.map (ensure statements cache source) args)))
        | Ref_binding (name, _) when String.starts_with ~prefix:"$elem:" name -> refuse "element"
        | Ref_binding (name, fields) ->
            let start =
              match Names.find_opt name env with
              | Some v -> resolve statements cache v
              | None -> refuse "binding"
            in
            let value =
              List.fold_left (fun v name -> field statements cache source x.ty v name) start fields
            in
            if borrow then descriptor statements cache source value else value
        (* a program runs over values at hand: nothing it reads is live *)
        | Op { op = "live?"; _ } -> Known (V.Bool false)
        | Op { op; args; _ } -> (
            let o =
              match
                Flow.Op.find
                  ~extra:(E.Residual.residual_ops (Lazy.force root))
                  op Flow.Context.value
              with
              | Some o -> o
              | _ -> refuse op
            in
            let args =
              List.map
                (fun (n, t) ->
                  ( n,
                    if Flow.Ty.is_cooked x.ty && storage_argument t.W.ty then
                      term ~borrow:true statements cache source env t
                    else compile t ))
                args
            in
            let rec data_source = function
              | V.Source _ | Struct (_, Flow.Ty.Array _, _) -> true
              | V.Record fields -> List.exists (fun (_, v) -> data_source v) fields
              | _ -> false
            in
            let prototype =
              if o.live then None
              else
                try
                  Some
                    (o.body ~live:(Input.at_time 0.)
                       ~node:(fun _ _ -> refuse op)
                       (List.mapi
                          (fun i (name, value) ->
                            ( name,
                              match value with
                              | Known v -> v
                              | _ ->
                                  V.Source ("$core/argument", type_of value, [ ("index", V.Int i) ])
                            ))
                          args))
                with Unsupported _ | Diagnostic.Error _ | Invalid_argument _ -> None
            in
            let rec host = function
              | V.Source ("$core/argument", _, [ ("index", V.Int index) ]) ->
                  snd (List.nth args index)
              | (V.Source (name, ty, fields) | Struct (name, (Flow.Ty.Array _ as ty), fields)) as
                value ->
                  if
                    List.for_all
                      (fun (_, v) -> match host v with Known _ -> true | _ -> false)
                      fields
                  then Known value
                  else
                    let args =
                      List.map
                        (fun (name, v) -> (name, ensure statements cache source (host v)))
                        fields
                    in
                    emit statements source ty
                      (if borrow && storage_argument ty then Source_descriptor { name; ty; args }
                       else Host_source { name; ty; args })
              | V.Record fields -> Record_fields (List.map (fun (name, v) -> (name, host v)) fields)
              | value -> Known value
            in
            if Flow.Ty.is_cooked x.ty then
              emit statements source x.ty
                (Host_source
                   {
                     name = "$core/operator";
                     ty = x.ty;
                     args =
                       List.map
                         (fun (name, v) -> (name, ensure statements cache source v))
                         (("$name", Known (V.Text op)) :: args);
                   })
            else
              match prototype with
              | Some value when data_source value ->
                  let value = host value in
                  if borrow then descriptor statements cache source value else value
              | _ -> (
                  if o.ctx <> Flow.Context.value || o.shape <> Flow.Op.Scalar || not (shipped o)
                  then refuse op;
                  match known_values args with
                  | Some values when not o.live -> (
                      try
                        Known (o.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> refuse op) values)
                      with Diagnostic.Error d ->
                        primitive statements cache source x.ty (failure d) [])
                  | _ -> operation statements cache source x.ty op args))
        | Call { kind; ctx; args } ->
            if not (Flow.Ty.is_cooked x.ty) then refuse "call";
            let args =
              List.map
                (fun (name, t) ->
                  ( name,
                    ensure statements cache source
                      (term ~borrow:(storage_argument t.W.ty) statements cache source env t) ))
                args
            in
            emit statements source x.ty
              (Host_source
                 {
                   name = "$core/catalog";
                   ty = x.ty;
                   args =
                     ("$name", ensure statements cache source (Known (V.Text kind)))
                     :: ( "$context",
                          ensure statements cache source (Known (V.Text (Flow.Context.name ctx))) )
                     :: args;
                 })
        | Let (bindings, body) ->
            let env =
              List.fold_left
                (fun env (pattern, value) ->
                  let value = term statements cache source env value in
                  bind statements cache source env pattern value)
                env bindings
            in
            term ~borrow statements cache source env body
        | Error_rows { rows; policy; mapper } ->
            let rows = compile rows |> ensure statements cache source in
            let handler = ref [] and handler_cache = Hashtbl.create 16 in
            let index = local Flow.Ty.Int in
            let mapper = term handler handler_cache source env mapper |> as_callable source in
            let payload =
              Flow.Ty.Record [ ("code", Text); ("message", Text); ("cause", List Text) ]
            in
            let diagnostic =
              call handler handler_cache source payload None mapper
                [ Computed (index, Flow.Ty.Int) ]
              |> ensure handler handler_cache source
            in
            let result = local x.ty in
            statements :=
              statement (number ()) source
                (Diagnose_rows
                   { rows; policy; index; handler = List.rev !handler; diagnostic; result })
              :: !statements;
            Computed (result, x.ty)
        | With_error (protected, mapper) ->
            let body = ref [] and handler = ref [] in
            let body_cache = Hashtbl.create 16 and handler_cache = Hashtbl.create 16 in
            let protected_value = term body body_cache source env protected in
            let value = ensure body body_cache source protected_value in
            let complete =
              Flow.Ty.is_cooked protected.ty
              && match protected_value with Known V.No_geo -> false | _ -> true
            in
            let value =
              if complete then
                emit body source protected.ty
                  (Host_source
                     { name = "$core/complete"; ty = protected.ty; args = [ ("value", value) ] })
                |> ensure body body_cache source
              else value
            in
            let payload =
              Flow.Ty.Record [ ("code", Text); ("message", Text); ("cause", List Text) ]
            in
            let failure = local payload in
            let mapper = term handler handler_cache source env mapper |> as_callable source in
            let mapped =
              call handler handler_cache source payload None mapper [ Computed (failure, payload) ]
              |> ensure handler handler_cache source
            in
            let result = local x.ty in
            statements :=
              statement (number ()) source
                (Protect
                   {
                     body = List.rev !body;
                     value;
                     failure;
                     handler = List.rev !handler;
                     mapped;
                     result;
                   })
              :: !statements;
            Computed (result, x.ty)
        | If (condition, yes, no) ->
            branch statements cache source x.ty (compile condition)
              (fun body cache -> term ~borrow body cache source env yes)
              (fun body cache -> term ~borrow body cache source env no)
        | Cond (arms, default) ->
            let rec arms_lower statements cache = function
              | [] -> term ~borrow statements cache source env default
              | (condition, value) :: rest ->
                  let condition = term statements cache source env condition in
                  branch statements cache source x.ty condition
                    (fun body cache -> term ~borrow body cache source env value)
                    (fun body cache -> arms_lower body cache rest)
            in
            arms_lower statements cache arms
        | Case (scrutinee, arms, default) ->
            let scrutinee = compile scrutinee in
            let rec arms_lower statements cache = function
              | [] -> term ~borrow statements cache source env default
              | (lit, value) :: rest ->
                  let lit : E.value =
                    match lit.Flow.Syntax.node with
                    | Str s -> V.Text s
                    | Sym s -> Bool (s = "true")
                    | Num s -> (
                        match if String.contains s '.' then None else int_of_string_opt s with
                        | Some n -> Int n
                        | None -> Float (float_of_string s))
                    | _ -> refuse "case"
                  in
                  let condition =
                    match scrutinee with
                    | Known sv when C.literal_of_value sv <> None ->
                        let boolean = function V.Bool b -> b | v -> V.num v <> 0. in
                        Known
                          (V.Bool
                             (match (sv, lit) with
                             | Bool _, _ | _, Bool _ -> boolean sv = boolean lit
                             | Text a, Text b -> a = b
                             | (Int _ | Float _), (Int _ | Float _) -> V.num sv = V.num lit
                             | _ -> false))
                    | _ when type_of scrutinee = Flow.Ty.Bool || V.ty_of lit = Flow.Ty.Bool ->
                        let a =
                          primitive statements cache source Flow.Ty.Bool Truthy
                            [ ("value", scrutinee) ]
                        in
                        let b = Known (V.Bool (V.truthy lit)) in
                        primitive statements cache source Flow.Ty.Bool Equal [ ("a", a); ("b", b) ]
                    | _
                      when List.mem (type_of scrutinee) [ Flow.Ty.Int; Float ]
                           && List.mem (V.ty_of lit) [ Flow.Ty.Int; Float ] ->
                        primitive statements cache source Flow.Ty.Bool (Numeric Eq)
                          [ ("a", scrutinee); ("b", Known lit) ]
                    | _ ->
                        primitive statements cache source Flow.Ty.Bool Equal
                          [ ("a", scrutinee); ("b", Known lit) ]
                  in
                  branch statements cache source x.ty condition
                    (fun body cache -> term ~borrow body cache source env value)
                    (fun body cache -> arms_lower body cache rest)
            in
            arms_lower statements cache arms
        | Call_fn { fn; args; body } ->
            let values = List.map compile args in
            let value =
              match Names.find_opt fn env with
              | Some f ->
                  call ~borrow statements cache source x.ty body (as_callable source f) values
              | None -> definition ~borrow statements cache source fn body values
            in
            if borrow then descriptor statements cache source value else value
        | Fn_ref name ->
            let identity = new_identity () in
            statements :=
              statement (number ()) source (Reset_call (call_slot identity)) :: !statements;
            Callable (Named { name; identity })
        | Fn { params; body; zone; capture } ->
            let closure =
              match capture with
              | Some original -> (
                  match as_callable source (compile original) with
                  | Closure cl -> Closure { cl with params; body }
                  | Named n ->
                      Closure
                        {
                          params;
                          body;
                          zone;
                          env = Names.empty;
                          instance = source.instance;
                          prefix = source.prefix;
                          identity = n.identity;
                        })
              | None ->
                  let identity = new_identity () in
                  statements :=
                    statement (number ()) source (Reset_call (call_slot identity)) :: !statements;
                  Closure
                    {
                      params;
                      body;
                      zone;
                      env;
                      instance = source.instance;
                      prefix = source.prefix;
                      identity;
                    }
            in
            Callable closure
        | Expanded { body; _ } -> term ~borrow statements cache source env body
        | Bypass inner -> (
            match W.bypass_input inner with
            | Some input ->
                coerce statements cache source inner.ty
                  (term ~borrow statements cache source env input)
            | None -> refuse "bypass")
        (* A drawing's filter of a range: its predicate as an Int mask map (its free names read from the
       lowering's scope, as the inline fn reads them), which packs like any map (all cores, or the
       device in fast precision), then array/compact keeps the indices.  Any other filter, or a mask
       that does not pack, keeps the element loop below. *)
        | Hof
            ( `Filter,
              [
                ({ node = Fn ({ body; _ } as f); _ } as fn);
                ({
                   node = Op { op = "array/range"; _ };
                   ty = Flow.Ty.Array ((Flow.Ty.Int | Flow.Ty.Float) as elem);
                   _;
                 } as items);
              ] )
          when draw <> None && x.ty = Flow.Ty.Array elem ->
            let lit n =
              { body with node = Lit (Param.Int_value n); ty = Flow.Ty.Int; path = None }
            in
            let mask_fn =
              {
                fn with
                ty = Flow.Ty.Fn (Some { params = [ elem ]; result = Flow.Ty.Int });
                node =
                  Fn
                    {
                      f with
                      capture = None;
                      body =
                        { body with node = If (body, lit 1, lit 0); ty = Flow.Ty.Int; path = None };
                    };
              }
            in
            let mask =
              term statements cache source env
                {
                  x with
                  node = Hof (`Map, [ mask_fn; items ]);
                  ty = Flow.Ty.Array Flow.Ty.Int;
                  path = None;
                }
            in
            let kept =
              primitive statements cache source (Flow.Ty.Array Flow.Ty.Int) Array_compact
                [ ("mask", mask) ]
            in
            if elem = Flow.Ty.Int then kept
            else primitive statements cache source x.ty (Coerce x.ty) [ ("value", kept) ]
        | Hof (kind, fn :: rest) ->
            let fn = as_callable source (compile fn) in
            hof statements cache source env x.ty kind fn rest
        | Hof (_, []) -> refuse "higher_order"
        | Graph_ref { graph; inputs = [] } ->
            (* The referenced graph's body in its own default instance, as the live walker re-runs it. *)
            let g =
              match E.Residual.residual_graph (Lazy.force root) graph with
              | Some g -> g
              | None -> refuse "ref"
            in
            let zone = [ graph ] in
            let source =
              {
                source with
                C.instance =
                  Option.value ~default:source.instance
                    (E.Residual.graph_instance (Lazy.force root) graph);
                prefix = [];
                site = zone;
                owner = zone;
                iter = [];
              }
            in
            let saved = !plane in
            plane := if g.context = Flow.Context.host then `Host else `Frame;
            let env =
              List.fold_left
                (fun env (name, ty, default) ->
                  match default with
                  | Some t ->
                      Names.add name
                        (coerce statements cache source ty (term statements cache source env t))
                        env
                  | None -> refuse "ref")
                Names.empty g.inputs
            in
            let value = term statements cache source env g.body in
            plane := saved;
            value
        | Loop
            {
              kind = (`For | `Sum) as kind;
              accs = [];
              clauses = [ (W.Name name, items) ];
              skip = [];
              body;
              zone;
            } -> (
            let items = compile items in
            match items with
            | Known (V.List items) when Array.length items <= 32 ->
                let values =
                  Array.mapi
                    (fun index item ->
                      let source = { source with C.iter = source.iter @ [ Static_index index ] } in
                      term statements cache source (Names.add name (Known item) env) body)
                    items
                in
                if Array.for_all (function Known _ -> true | _ -> false) values then
                  let values = Array.map (function Known v -> v | _ -> assert false) values in
                  Known
                    (if kind = `For then
                       let ty = V.elem_ty values in
                       V.List (Array.map (V.coerce_to ty) values)
                     else if Array.length values = 0 then V.Int 0
                     else
                       Array.fold_left (V.arith "sum" ( +. )) values.(0)
                         (Array.sub values 1 (Array.length values - 1)))
                else if kind = `For then
                  emit statements source x.ty
                    (Items (Array.to_list (Array.map (ensure statements cache source) values)))
                else if Array.length values = 0 then Known (V.Int 0)
                else
                  Array.fold_left
                    (fun acc v ->
                      primitive statements cache source x.ty (Numeric Add) [ ("a", acc); ("b", v) ])
                    values.(0)
                    (Array.sub values 1 (Array.length values - 1))
            | _ ->
                loop statements cache source env x.ty kind []
                  [ (W.Name name, None) ]
                  [] body zone (Some items))
        | Loop { kind; accs; clauses; skip; body; zone } ->
            loop statements cache source env x.ty kind accs
              (List.map (fun (p, t) -> (p, Some t)) clauses)
              skip body zone None
        | node ->
            refuse
              (match node with
              | Vec _ -> "vector"
              | Lit _ -> "literal"
              | Graph_ref _ -> "ref"
              | _ -> assert false))

  and packed_region r (view : E.Residual.residual_view) : C.Region.t option =
    (* Packed-array forms run as one region through Packed.force, unchanged; list
       loops stay structured core. A refusal by Packed leaves the structured path. *)
    let packed ty = match ty with Flow.Ty.Array _ -> true | _ -> false in
    let sources =
      match view.term.node with
      | W.Hof ((`Map | `Reduce), _ :: rest) -> Some rest
      | Loop { clauses; _ } -> Some (List.map snd clauses)
      | Op { op = "array/sum"; args; _ } -> Some (List.map snd args)
      | _ -> None
    in
    match sources with
    (* Core keeps state in its own statements: a region over a state-reading residual stays structured *)
    | Some _ when E.state_dependent (V.Residual r) -> None
    | Some sources when packed view.term.ty || List.exists (fun (s : W.term) -> packed s.ty) sources
      -> (
        try
          Option.bind (Packed.compile ?trace r view.term) (fun packed ->
              let region = packed in
              if region_allowed region then Some region else None)
        with Diagnostic.Error _ | Invalid_argument _ | Not_found | Failure _ -> None)
    | _ -> None

  and region ?(args = []) statements packed (view : E.Residual.residual_view) =
    let index = List.length !regions in
    regions := !regions @ [ packed ];
    let source = { (source_of_view view) with C.span = Some view.term.form.span } in
    let result = local view.term.ty in
    statements :=
      statement (number ()) source (Invoke { region = index; args; result }) :: !statements;
    Computed (result, view.term.ty)

  and state_term statements cache source env (x : W.term) previous_only =
    match x.node with
    | W.State { binder; init; step; zone } ->
        if !geometry then refuse "state";
        let ty = x.ty in
        let numeric n = if List.mem ty [ Flow.Ty.Int; Float ] then Hashtbl.add mixed_numeric n () in
        let slot = !next_slot in
        incr next_slot;
        slots := { C.source; prefix = source.prefix; zone; plane = !plane; ty } :: !slots;
        let initial_body = ref [] and initial_cache = Hashtbl.copy cache in
        let initial =
          ensure initial_body initial_cache source (term initial_body initial_cache source env init)
        in
        let previous = local ty in
        numeric previous;
        statements :=
          statement (number ()) source
            (State_read { slot; result = previous; init = List.rev !initial_body; initial })
          :: !statements;
        let previous = Computed (previous, ty) in
        if previous_only then previous
        else begin
          let env = bind statements cache source env binder previous in
          let found = local Flow.Ty.Bool and pending = local ty and result = local ty in
          numeric pending;
          numeric result;
          statements :=
            statement (number ()) source (State_peek { slot; found; value = pending })
            :: !statements;
          let id = number () in
          let yes = [ statement (number ()) source (Assign (result, Local pending)) ] in
          let body = ref [] and step_cache = Hashtbl.copy cache in
          let value =
            ensure body step_cache source
              (coerce body step_cache source ty (term body step_cache source env step))
          in
          body := statement (number ()) source (State_write { slot; value }) :: !body;
          body := statement (number ()) source (Assign (result, Local value)) :: !body;
          statements :=
            statement id source (If { condition = found; yes; no = List.rev !body }) :: !statements;
          Computed (result, ty)
        end
    | _ -> refuse "state"

  and resolve statements cache = function
    | Known (V.Residual r) -> residual statements cache r
    | v -> v

  and sequence_count statements cache source sequence =
    let sequence = resolve statements cache sequence in
    match sequence with
    | Selected (items, index) ->
        select statements cache source Flow.Ty.Int items index (fun body cache v ->
            sequence_count body cache source v)
    | Known (V.List xs) -> Known (V.Int (Array.length xs))
    | Known
        ((V.Text_array _ | Int_array _ | Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _)
         as v) ->
        Known (V.Int (V.array_length v))
    | _ ->
        let packed = match type_of sequence with Flow.Ty.Array _ -> true | _ -> false in
        primitive statements cache source Flow.Ty.Int
          (if packed then Array_count else Sequence_length)
          [ ((if packed then "array" else "list"), sequence) ]

  and sequence_get statements cache source sequence index =
    let sequence = resolve statements cache sequence in
    match (sequence, index) with
    | Selected (items, at), index ->
        select statements cache source
          (Option.value ~default:Flow.Ty.Any (Flow.Ty.elem (type_of sequence)))
          items at
          (fun body cache v -> sequence_get body cache source v index)
    | Known (V.List items), Known index -> (
        let op = Option.get (Flow.Op.find "nth" Flow.Context.value) in
        try
          resolve statements cache
            (Known
               (op.body ~live:(Input.at_time 0.)
                  ~node:(fun _ _ -> refuse "nth")
                  [ ("list", V.List items); ("index", index) ]))
        with Diagnostic.Error d -> primitive statements cache source Flow.Ty.Any (failure d) [])
    | Known (V.List items), index when C.literal_of_value (V.List items) = None ->
        if Array.length items > 4096 then refuse "list_limit";
        let indices = Known (V.List (Array.init (Array.length items) (fun i -> V.Int i))) in
        let index = sequence_get statements cache source indices index in
        Selected (Array.map (fun v -> Known v) items, index)
    | _ ->
        let ty = Option.value ~default:Flow.Ty.Any (Flow.Ty.elem (type_of sequence)) in
        let packed = match type_of sequence with Flow.Ty.Array _ -> true | _ -> false in
        primitive statements cache source ty
          (if packed then Array_nth else Sequence_get)
          [ ((if packed then "array" else "list"), sequence); ("index", index) ]

  and iterate statements cache source sequence f =
    let sequence = resolve statements cache sequence in
    match sequence with
    | Known (V.List items) when C.literal_of_value (V.List items) = None ->
        counted statements cache source (Known (V.Int 0))
          (Known (V.Int (Array.length items)))
          (fun body cache index ->
            f body cache index (Selected (Array.map (fun v -> Known v) items, index)))
    | _ ->
        let sequence_id = ensure statements cache source sequence in
        let index = local Flow.Ty.Int in
        let ty = Option.value ~default:Flow.Ty.Any (Flow.Ty.elem (type_of sequence)) in
        let item = local ty and id = number () in
        if ty = Flow.Ty.Float then Hashtbl.add mixed_numeric item ();
        let body = ref [] in
        f body (Hashtbl.copy cache) (Computed (index, Flow.Ty.Int)) (Computed (item, ty));
        let limit =
          match type_of sequence with
          | Flow.Ty.Array (Int | Text) -> Sys.max_array_length
          | Array element ->
              Sys.max_floatarray_length
              / Option.value ~default:1 (Flow.Packed_ops.scalar_width element)
          | _ -> 4096
        in
        statements :=
          statement id source
            (Iterate { sequence = sequence_id; index; item; limit; body = List.rev !body })
          :: !statements

  and select statements cache source ty items index f =
    (* ponytail: selecting a heterogeneous lazy capture scans at most 4,096
       arms; use typed captured buffers if profiles justify compiling this case. *)
    let rec choose statements cache at =
      if at = Array.length items then
        primitive statements cache source ty
          (Fail ("E_LIST_RANGE", "Index is outside its list."))
          []
      else
        let matches =
          primitive statements cache source Flow.Ty.Bool (Numeric Eq)
            [ ("a", index); ("b", Known (V.Int at)) ]
        in
        branch statements cache source ty matches
          (fun body cache -> f body cache items.(at))
          (fun body cache -> choose body cache (at + 1))
    in
    choose statements cache 0

  and store statements cache source sequence index value =
    let sequence = ensure statements cache source sequence in
    let index = ensure statements cache source index in
    let value = ensure statements cache source value in
    statements := statement (number ()) source (Store { sequence; index; value }) :: !statements

  and assign statements cache source target value =
    let value = ensure statements cache source value in
    statements := statement (number ()) source (Assign (target, Local value)) :: !statements

  and hold ?ty statements cache source value =
    match resolve statements cache value with
    | Record_fields fields ->
        Record_fields
          (List.map
             (fun (name, v) ->
               let ty =
                 match ty with
                 | Some (Flow.Ty.Record fields) -> List.assoc_opt name fields
                 | _ -> None
               in
               (name, hold ?ty statements cache source v))
             fields)
    | Known (V.Record fields) ->
        hold ?ty statements cache source
          (Record_fields (List.map (fun (n, v) -> (n, Known v)) fields))
    | value ->
        let n = ensure statements cache source value in
        let ty = Option.value ~default:(type_of value) ty in
        let result = emit statements source ty (Local n) in
        (match result with
        | Computed (n, _) when List.mem ty [ Flow.Ty.Int; Float ] -> Hashtbl.add mixed_numeric n ()
        | _ -> ());
        result

  and update statements cache source target value =
    match target with
    | Computed (n, _) -> assign statements cache source n value
    | Record_fields fields ->
        let values = record_fields statements cache source value in
        if List.map fst fields <> List.map fst values then refuse "accumulator_layout";
        (* Read every next field before overwriting any accumulator field. *)
        let values = List.map (fun (name, v) -> (name, hold statements cache source v)) values in
        List.iter2
          (fun (_, target) (_, value) -> update statements cache source target value)
          fields values
    | _ -> refuse "accumulator"

  and widen_like statements cache source old value =
    let old = resolve statements cache old and value = resolve statements cache value in
    match (type_of old, type_of value) with
    | (Flow.Ty.Int | Float), (Flow.Ty.Int | Float) ->
        let is_float =
          primitive statements cache source Flow.Ty.Bool Equal
            [ ("a", value); ("b", coerce statements cache source Flow.Ty.Float value) ]
        in
        branch statements cache source Flow.Ty.Float is_float
          (fun _ _ -> value)
          (fun body cache -> coerce_like body cache source old value)
    | _ -> coerce_like statements cache source old value

  and increment statements cache source value =
    let next =
      primitive statements cache source Flow.Ty.Int (Numeric Add)
        [ ("a", value); ("b", Known (V.Int 1)) ]
    in
    update statements cache source value next

  and counted ?(limit = 4096) ?stop statements cache source first last f =
    let first = ensure statements cache source first
    and last = ensure statements cache source last in
    let stop = Option.map (ensure statements cache source) stop in
    let count = local Flow.Ty.Int
    and index = local Flow.Ty.Int
    and item = local Flow.Ty.Int
    and id = number () in
    let body = ref [] in
    f body (Hashtbl.copy cache) (Computed (item, Flow.Ty.Int));
    statements :=
      statement id source
        (Counted { first; last; count; index; item; limit; stop; init = []; body = List.rev !body })
      :: !statements

  and slice statements cache source ty sequence first count =
    primitive statements cache source ty Sequence_slice
      [ ("list", sequence); ("first", first); ("count", count) ]

  and hof statements cache source env ty kind fn rest =
    let compile = term statements cache source env in
    match (kind, rest) with
    | `Reduce, [ init; items ] ->
        let accumulator = hold ~ty statements cache source (compile init) in
        let items = compile items in
        iterate statements cache source items (fun body cache _ item ->
            let value = call body cache source ty None fn [ accumulator; item ] in
            let value = widen_like body cache source accumulator value in
            update body cache source accumulator value);
        accumulator
    | `Iterate, init :: count :: until ->
        (* Lower the body once; [:until] stops the counted scope after committing its next value. *)
        let accumulator = hold ~ty statements cache source (compile init) in
        let count = compile count in
        let stop = List.map (fun g -> as_callable source (compile g)) until in
        let finished =
          match stop with
          | [] -> None
          | _ -> Some (hold statements cache source (Known (V.Bool false)))
        in
        counted ~limit:max_int ?stop:finished statements cache source (Known (V.Int 0)) count
          (fun body cache _ ->
            let value = call body cache source ty None fn [ accumulator ] in
            let value = widen_like body cache source accumulator value in
            match (stop, finished) with
            | g :: _, Some finished ->
                let next = hold ~ty body cache source value in
                let reached = call body cache source Flow.Ty.Bool None g [ accumulator; next ] in
                update body cache source finished reached;
                update body cache source accumulator next
            | _ -> update body cache source accumulator value);
        accumulator
    | `Sort_by, [ items ] -> sort_by statements cache source ty fn (compile items)
    | (`Map | `Filter), items ->
        (* A packed result is packed storage built element by element. *)
        let width =
          match ty with
          | Flow.Ty.Array (Int | Text | Float | Any) -> 1
          | Array Vec2 -> 2
          | Array Vec3 -> 3
          | Array Vec4 -> 4
          | Array _ -> refuse "packed_higher_order"
          | _ -> 0
        in
        let items =
          List.map
            (fun item ->
              let value = compile item in
              match value with
              | Known (V.List xs)
                when geometry_collection (type_of value)
                     && Array.for_all
                          (function
                            | V.Deferred (ty, _) -> ty = Flow.Ty.geometry
                            | V.No_geo -> true
                            | _ -> false)
                          xs ->
                  (* Homogeneous handles need no lazy heterogeneous selection tree.
                 Publish their builder before the loop; residual elements stay lazy. *)
                  hold statements cache source value
              | value -> value)
            items
        in
        let counts = List.map (sequence_count statements cache source) items in
        let count =
          match counts with
          | [] -> refuse "higher_order"
          | first :: rest ->
              List.fold_left
                (fun count next ->
                  primitive statements cache source Flow.Ty.Int (Numeric Min)
                    [ ("a", count); ("b", next) ])
                first rest
        in
        let result =
          primitive statements cache source ty Sequence_build
            (("count", count)
            ::
            (if width > 0 then
               [ ("width", Known (V.Int width)) ]
               @
               match ty with
               | Flow.Ty.Array Int -> [ ("type", Known (V.Text "int")) ]
               | Array Text -> [ ("type", Known (V.Text "text")) ]
               | _ -> []
             else []))
        in
        let kept = hold statements cache source (Known (V.Int 0)) in
        counted
          ~limit:
            (if ty = Flow.Ty.Array Flow.Ty.Int || ty = Flow.Ty.Array Flow.Ty.Text then
               Sys.max_array_length
             else if width > 0 then Sys.max_floatarray_length
             else if geometry_collection ty then Sys.max_array_length
             else 4096)
          statements cache source (Known (V.Int 0)) count
          (fun body cache index ->
            let args =
              List.map (fun sequence -> sequence_get body cache source sequence index) items
            in
            let item = List.hd args in
            let result_ty =
              if kind = `Map then Option.value ~default:Flow.Ty.Any (Flow.Ty.elem ty)
              else Flow.Ty.Bool
            in
            let value = call body cache source result_ty None fn args in
            if kind = `Map then store body cache source result index value
            else
              ignore
                (branch body cache source Flow.Ty.Int value
                   (fun body cache ->
                     store body cache source result kept item;
                     increment body cache source kept;
                     Known (V.Int 0))
                   (fun _ _ -> Known (V.Int 0))));
        if kind = `Filter && width > 0 then
          primitive statements cache source ty Array_slice
            [ ("array", result); ("first", Known (V.Int 0)); ("count", kept) ]
        else if kind = `Filter then slice statements cache source ty result (Known (V.Int 0)) kept
        else if width > 0 || geometry_collection ty then result
        else primitive statements cache source ty Sequence_concat [ ("list", result) ]
    | _ -> refuse "higher_order"

  and sort_by statements cache source ty fn items =
    if match ty with Flow.Ty.Array _ -> true | _ -> false then refuse "packed_higher_order";
    let number n = Known (V.Int n) in
    let calc statements cache op a b =
      primitive statements cache source Flow.Ty.Int (Numeric op) [ ("a", a); ("b", b) ]
    in
    let count = sequence_count statements cache source items in
    let build ty =
      primitive statements cache source (Flow.Ty.List ty) Sequence_build [ ("count", count) ]
    in
    let keys = build Flow.Ty.Float in
    let current = hold statements cache source (build Flow.Ty.Int) in
    let scratch = hold statements cache source (build Flow.Ty.Int) in
    iterate statements cache source items (fun body cache index item ->
        let key = call body cache source Flow.Ty.Float None fn [ item ] in
        let key = primitive body cache source Flow.Ty.Float Float_convert [ ("x", key) ] in
        store body cache source keys index key;
        store body cache source current index index);
    let widths = Known (V.List (Array.init 12 (fun i -> V.Int (1 lsl i)))) in
    iterate statements cache source widths (fun body cache _ width ->
        let needed = calc body cache Lt width count in
        ignore
          (branch body cache source Flow.Ty.Int needed
             (fun body cache ->
               let twice = calc body cache Mul width (number 2) in
               let rounded = calc body cache Sub (calc body cache Add count twice) (number 1) in
               let blocks =
                 primitive body cache source Flow.Ty.Int (Integer Idiv)
                   [ ("a", rounded); ("b", twice) ]
               in
               counted body cache source (number 0) blocks (fun body cache block ->
                   let start = calc body cache Mul block twice in
                   let middle = calc body cache Min (calc body cache Add start width) count in
                   let finish = calc body cache Min (calc body cache Add start twice) count in
                   let left = hold body cache source start
                   and right = hold body cache source middle in
                   counted body cache source start finish (fun body cache target ->
                       let left_available = calc body cache Lt left middle in
                       let choose_left =
                         branch body cache source Flow.Ty.Bool left_available
                           (fun body cache ->
                             let right_available = calc body cache Lt right finish in
                             branch body cache source Flow.Ty.Bool right_available
                               (fun body cache ->
                                 let li = sequence_get body cache source current left in
                                 let ri = sequence_get body cache source current right in
                                 let a = sequence_get body cache source keys li in
                                 let b = sequence_get body cache source keys ri in
                                 calc body cache Le a b)
                               (fun _ _ -> Known (V.Bool true)))
                           (fun _ _ -> Known (V.Bool false))
                       in
                       ignore
                         (branch body cache source Flow.Ty.Int choose_left
                            (fun body cache ->
                              let value = sequence_get body cache source current left in
                              store body cache source scratch target value;
                              increment body cache source left;
                              number 0)
                            (fun body cache ->
                              let value = sequence_get body cache source current right in
                              store body cache source scratch target value;
                              increment body cache source right;
                              number 0))));
               let old = hold body cache source current in
               update body cache source current scratch;
               update body cache source scratch old;
               number 0)
             (fun _ _ -> number 0)));
    let result = primitive statements cache source ty Sequence_build [ ("count", count) ] in
    counted statements cache source (number 0) count (fun body cache index ->
        let at = sequence_get body cache source current index in
        let value = sequence_get body cache source items at in
        store body cache source result index value);
    result

  and loop statements cache source env ty kind accs clauses skip body zone first =
    let zone_source = { source with C.site = zone; owner = zone } in
    let init =
      match accs with
      | [ (pattern, value) ] ->
          let acc_ty =
            if kind = `Scan then Option.value ~default:Flow.Ty.Any (Flow.Ty.elem ty) else ty
          in
          Some
            ( pattern,
              hold ~ty:acc_ty statements cache zone_source
                (term statements cache zone_source env value) )
      | [] -> None
      | _ -> refuse "accumulator"
    in
    let attempted = hold statements cache zone_source (Known (V.Int 0)) in
    let kept = hold statements cache zone_source (Known (V.Int 0)) in
    let total =
      if kind = `Sum then Some (hold statements cache zone_source (Known (V.Int 0))) else None
    in
    let collecting = kind = `For || kind = `Scan in
    let array_loop =
      List.exists
        (fun (_, value) ->
          match value with
          | Some (value : W.term) -> ( match value.ty with Flow.Ty.Array _ -> true | _ -> false)
          | None ->
              Option.fold ~none:false
                ~some:(fun value -> match type_of value with Flow.Ty.Array _ -> true | _ -> false)
                first)
        clauses
    in
    let text_sources =
      if collecting && ty = Flow.Ty.Array Flow.Ty.Text then begin
        (* ponytail: preallocate invariant clause products; dependent collections need
         a growing structured builder rather than re-evaluating sources to count. *)
        let rec invariant (value : W.term) =
          let all = List.for_all invariant in
          match value.node with
          | Lit _ | Text _ | Nil | Time | Ref_binding _ -> true
          | Op { op; args; _ } ->
              op <> "value/rand"
              && (match
                    Flow.Op.find
                      ~extra:(E.Residual.residual_ops (Lazy.force root))
                      op Flow.Context.value
                  with
                | Some op -> shipped op
                | None -> false)
              && List.for_all (fun (_, x) -> invariant x) args
          | Vec xs | List_lit xs | Str xs -> all xs
          | Record xs -> List.for_all (fun (_, x) -> invariant x) xs
          | Get (x, _) | Expanded { body = x; _ } -> invariant x
          | If (a, b, c) -> all [ a; b; c ]
          | _ -> false
        in
        let count = ref (Known (V.Int 1)) in
        let sources =
          List.mapi
            (fun index (_, value) ->
              let source_ty =
                match value with Some value -> value.W.ty | None -> type_of (Option.get first)
              in
              let items =
                if index = 0 then
                  match value with
                  | Some value -> term statements cache zone_source env value
                  | None -> Option.get first
                else begin
                  (match value with
                  | Some value
                    when invariant value
                         && List.for_all
                              (fun name -> Names.mem name env)
                              (E.Residual.free_names value) ->
                      ()
                  | _ -> refuse "text_collect_dependent_clause");
                  let empty =
                    match source_ty with
                    | Flow.Ty.Array elem -> Known (V.array_init elem 0 (fun _ -> assert false))
                    | Flow.Ty.List _ -> Known (V.List [||])
                    | _ -> refuse "text_collect_source"
                  in
                  let nonempty =
                    primitive statements cache zone_source Flow.Ty.Bool (Integer_compare Igt)
                      [ ("a", !count); ("b", Known (V.Int 0)) ]
                  in
                  branch statements cache zone_source source_ty nonempty
                    (fun body cache -> term body cache zone_source env (Option.get value))
                    (fun _ _ -> empty)
                end
              in
              let size = sequence_count statements cache zone_source items in
              let zero =
                primitive statements cache zone_source Flow.Ty.Bool (Integer_compare Ieq)
                  [ ("a", size); ("b", Known (V.Int 0)) ]
              in
              count :=
                branch statements cache zone_source Flow.Ty.Int zero
                  (fun _ _ -> Known (V.Int 0))
                  (fun body cache ->
                    let limit =
                      primitive body cache zone_source Flow.Ty.Int (Integer Idiv)
                        [ ("a", Known (V.Int Sys.max_array_length)); ("b", size) ]
                    in
                    let over =
                      primitive body cache zone_source Flow.Ty.Bool (Integer_compare Igt)
                        [ ("a", !count); ("b", limit) ]
                    in
                    branch body cache zone_source Flow.Ty.Int over
                      (fun body cache ->
                        primitive body cache zone_source Flow.Ty.Int
                          (Fail ("E_ARRAY_COUNT", "Array count is too large."))
                          [])
                      (fun body cache ->
                        primitive body cache zone_source Flow.Ty.Int (Integer Imul)
                          [ ("a", !count); ("b", size) ]));
              items)
            clauses
        in
        Some (sources, !count)
      end
      else None
    in
    let output =
      if collecting then begin
        match text_sources with
        | Some (_, count) ->
            Some
              (primitive statements cache zone_source ty Sequence_build
                 [ ("count", count); ("width", Known (V.Int 1)); ("type", Known (V.Text "text")) ])
        | None ->
            if match ty with Flow.Ty.Array _ -> true | _ -> false then refuse "packed_collect";
            Some
              (primitive statements cache zone_source ty Sequence_build
                 [ ("count", Known (V.Int 4096)) ])
      end
      else None
    in
    let rec clauses_lower statements cache env sources = function
      | [] ->
          let at = ensure statements cache zone_source attempted in
          let iteration = { zone_source with C.iter = source.iter @ [ Local_index at ] } in
          if not array_loop then begin
            let over =
              primitive statements cache iteration Flow.Ty.Bool (Numeric Ge)
                [ ("a", attempted); ("b", Known (V.Int 4096)) ]
            in
            ignore
              (branch statements cache iteration Flow.Ty.Int over
                 (fun body cache ->
                   primitive body cache iteration Flow.Ty.Int
                     (Fail ("E_ITER_BOUND", "Loop runs more than 4,096 iterations."))
                     [])
                 (fun _ _ -> Known (V.Int 0)))
          end;
          let skipped =
            List.fold_left
              (fun skipped tuple ->
                if List.length tuple <> List.length iteration.iter then skipped
                else
                  let matches =
                    List.fold_left2
                      (fun matches index expected ->
                        let index =
                          match index with
                          | C.Static_index n -> Known (V.Int n)
                          | Local_index n -> Computed (n, Flow.Ty.Int)
                        in
                        let eq =
                          primitive statements cache iteration Flow.Ty.Bool (Numeric Eq)
                            [ ("a", index); ("b", Known (V.Int expected)) ]
                        in
                        primitive statements cache iteration Flow.Ty.Bool (Numeric And)
                          [ ("a", matches); ("b", eq) ])
                      (Known (V.Bool true)) iteration.iter tuple
                  in
                  primitive statements cache iteration Flow.Ty.Bool (Numeric Or)
                    [ ("a", skipped); ("b", matches) ])
              (Known (V.Bool false)) skip
          in
          ignore
            (branch statements cache iteration Flow.Ty.Int skipped
               (fun _ _ -> Known (V.Int 0))
               (fun statements cache ->
                 let env =
                   match init with
                   | None -> env
                   | Some (pattern, accumulator) ->
                       bind statements cache iteration env pattern accumulator
                 in
                 let value = term statements cache iteration env body in
                 let value =
                   match init with
                   | Some (_, accumulator) ->
                       let value = widen_like statements cache iteration accumulator value in
                       update statements cache iteration accumulator value;
                       accumulator
                   | None -> value
                 in
                 (match total with
                 | Some total ->
                     let empty =
                       primitive statements cache iteration Flow.Ty.Bool (Numeric Eq)
                         [ ("a", kept); ("b", Known (V.Int 0)) ]
                     in
                     let next =
                       branch statements cache iteration ty empty
                         (fun _ _ -> value)
                         (fun body cache ->
                           primitive body cache iteration ty (Numeric Add)
                             [ ("a", total); ("b", value) ])
                     in
                     update statements cache iteration total next
                 | None -> ());
                 Option.iter
                   (fun output -> store statements cache iteration output kept value)
                   output;
                 increment statements cache iteration kept;
                 Known (V.Int 0)));
          increment statements cache iteration attempted
      | (pattern, value) :: rest ->
          let items =
            match sources with
            | items :: _ -> items
            | [] -> (
                match value with
                | Some value -> term statements cache zone_source env value
                | None -> Option.get first)
          in
          iterate statements cache zone_source items (fun body cache _ item ->
              let env = bind body cache source env pattern item in
              clauses_lower body cache env (match sources with [] -> [] | _ :: rest -> rest) rest)
    in
    clauses_lower statements cache env
      (match text_sources with None -> [] | Some (xs, _) -> xs)
      clauses;
    match (output, total, init) with
    | Some output, _, _ ->
        if ty = Flow.Ty.Array Flow.Ty.Text then
          primitive statements cache zone_source ty Array_slice
            [ ("array", output); ("first", Known (V.Int 0)); ("count", kept) ]
        else
          let output = slice statements cache zone_source ty output (Known (V.Int 0)) kept in
          primitive statements cache zone_source ty Sequence_concat [ ("list", output) ]
    | _, Some total, _ -> total
    | _, _, Some (_, accumulator) -> accumulator
    | _ -> refuse "loop"

  and record_fields statements cache source value =
    match resolve statements cache value with
    | Record_fields fields -> fields
    | Known (V.Record fields) -> List.map (fun (name, v) -> (name, Known v)) fields
    | Record_choice _ as value ->
        record_fields statements cache source
          (Computed (ensure statements cache source value, type_of value))
    | Computed (_, Flow.Ty.Record fields) as value ->
        List.map (fun (name, ty) -> (name, field statements cache source ty value name)) fields
    | _ -> refuse "record"

  and map_fields statements cache source value f =
    match resolve statements cache value with
    | Selected (items, index) ->
        select statements cache source (type_of value) items index (fun body cache v ->
            map_fields body cache source v f)
    | Record_choice (condition, yes, no)
      when match (type_of yes, type_of no) with Flow.Ty.Record _, Record _ -> true | _ -> false ->
        branch statements cache source (type_of value) condition
          (fun body cache -> map_fields body cache source yes f)
          (fun body cache -> map_fields body cache source no f)
    | value -> Record_fields (f statements cache (record_fields statements cache source value))

  and coerce_like statements cache source old value =
    let old = resolve statements cache old and value = resolve statements cache value in
    match (old, value) with
    | _ when match (type_of old, type_of value) with Flow.Ty.List _, List _ -> true | _ -> false ->
        let count = sequence_count statements cache source old in
        let empty =
          primitive statements cache source Flow.Ty.Bool (Numeric Eq)
            [ ("a", count); ("b", Known (V.Int 0)) ]
        in
        branch statements cache source (type_of value) empty
          (fun _ _ -> value)
          (fun body cache ->
            let old_item = sequence_get body cache source old (Known (V.Int 0)) in
            let count = sequence_count body cache source value in
            let result =
              primitive body cache source (type_of value) Sequence_build [ ("count", count) ]
            in
            iterate body cache source value (fun body cache index item ->
                let value = coerce_like body cache source old_item item in
                store body cache source result index value);
            result)
    | Known old, Known value when C.literal_of_value old <> None && C.literal_of_value value <> None
      ->
        coerce statements cache source (V.ty_of old) (Known value)
    | _
      when match (type_of old, type_of value) with Flow.Ty.Record _, Record _ -> true | _ -> false
      ->
        let old_fields = match type_of old with Flow.Ty.Record fs -> fs | _ -> assert false in
        map_fields statements cache source value (fun body cache fields ->
            List.map
              (fun (name, v) ->
                ( name,
                  match List.assoc_opt name old_fields with
                  | Some ty -> coerce_like body cache source (field body cache source ty old name) v
                  | None -> v ))
              fields)
    | Known old, _ -> coerce statements cache source (V.ty_of old) value
    | _
      when (type_of old = Flow.Ty.Float
           || match old with Computed (n, Flow.Ty.Int) -> Hashtbl.mem mixed_numeric n | _ -> false)
           && List.mem (type_of value) [ Flow.Ty.Int; Float; Bool ] ->
        let is_float =
          primitive statements cache source Flow.Ty.Bool Equal
            [ ("a", old); ("b", coerce statements cache source Flow.Ty.Float old) ]
        in
        branch statements cache source Flow.Ty.Float is_float
          (fun body cache -> coerce body cache source Flow.Ty.Float value)
          (fun body cache -> coerce body cache source Flow.Ty.Int value)
    | _ -> coerce statements cache source (type_of old) value

  and branch statements cache source ty condition yes no =
    match condition with
    | Known v when C.literal_of_value v <> None -> (if V.truthy v then yes else no) statements cache
    | condition ->
        let condition = ensure statements cache source condition in
        let id = number () in
        if match ty with Flow.Ty.Record _ -> false | _ -> true then begin
          let result = local ty in
          if ty = Flow.Ty.Float then Hashtbl.add mixed_numeric result ();
          let arm lower =
            let body = ref [] and cache = Hashtbl.copy cache in
            let value = ensure body cache source (lower body cache) in
            body := statement (number ()) source (Assign (result, Local value)) :: !body;
            List.rev !body
          in
          let yes = arm yes in
          let no = arm no in
          statements := statement id source (If { condition; yes; no }) :: !statements;
          Computed (result, ty)
        end
        else begin
          let yes_body = ref [] and no_body = ref [] in
          let yes_cache = Hashtbl.copy cache and no_cache = Hashtbl.copy cache in
          let a = yes yes_body yes_cache in
          let b = no no_body no_cache in
          let fields = function
            | Record_fields fs -> Some fs
            | Known (V.Record fs) -> Some (List.map (fun (n, v) -> (n, Known v)) fs)
            | _ -> None
          in
          let rec join ty a b =
            match (fields a, fields b) with
            | Some fs, Some gs when List.map fst fs = List.map fst gs ->
                Record_fields
                  (List.map2
                     (fun (name, a) (_, b) ->
                       let ty =
                         Option.value ~default:Flow.Ty.Any (Flow.Ty.join (type_of a) (type_of b))
                       in
                       (name, join ty a b))
                     fs gs)
            | _ when match ty with Flow.Ty.Record _ -> true | _ -> false ->
                Record_choice (Computed (condition, Flow.Ty.Bool), a, b)
            | _ ->
                let result = local ty in
                if ty = Flow.Ty.Float then Hashtbl.add mixed_numeric result ();
                let assign body cache v =
                  let value = ensure body cache source v in
                  body := statement (number ()) source (Assign (result, Local value)) :: !body
                in
                assign yes_body yes_cache a;
                assign no_body no_cache b;
                Computed (result, ty)
          in
          let result = join ty a b in
          statements :=
            statement id source (If { condition; yes = List.rev !yes_body; no = List.rev !no_body })
            :: !statements;
          result
        end

  and bind statements cache source env pattern value =
    match pattern with
    | W.Name name -> Names.add name value env
    | Keys keys ->
        let value = resolve statements cache value in
        List.fold_left
          (fun env name ->
            Names.add name (field statements cache source Flow.Ty.Any value name) env)
          env keys
    | Seq patterns ->
        let value = resolve statements cache value in
        let parts =
          match value with
          | Known v -> (
              match v with
              | V.List xs -> Array.to_list (Array.map (fun v -> Known v) xs)
              | Vec2 (x, y) -> [ Known (Float x); Known (Float y) ]
              | Vec3 (x, y, z) -> [ Known (Float x); Known (Float y); Known (Float z) ]
              | Vec4 (x, y, z, w) ->
                  [ Known (Float x); Known (Float y); Known (Float z); Known (Float w) ]
              | _ -> refuse "pattern")
          | Computed (_, ty) -> (
              match ty with
              | Flow.Ty.Vec2 | Vec3 | Vec4 ->
                  let fields =
                    match ty with
                    | Flow.Ty.Vec2 -> [ "x"; "y" ]
                    | Vec3 -> [ "x"; "y"; "z" ]
                    | _ -> [ "x"; "y"; "z"; "w" ]
                  in
                  List.map (field statements cache source Flow.Ty.Float value) fields
              | List elem ->
                  let count =
                    primitive statements cache source Flow.Ty.Int Sequence_length
                      [ ("list", value) ]
                  in
                  let short =
                    primitive statements cache source Flow.Ty.Bool (Numeric Lt)
                      [ ("a", count); ("b", Known (V.Int (List.length patterns))) ]
                  in
                  ignore
                    (branch statements cache source Flow.Ty.Int short
                       (fun body _ ->
                         primitive body cache source Flow.Ty.Int
                           (Fail ("E_PATTERN", "Not enough elements to destructure."))
                           [])
                       (fun _ _ -> Known (V.Int 0)));
                  List.mapi
                    (fun index _ ->
                      primitive statements cache source elem Sequence_get
                        [ ("list", value); ("index", Known (V.Int index)) ])
                    patterns
              | _ -> refuse "pattern")
          | Selected _ as value ->
              List.mapi
                (fun index _ -> sequence_get statements cache source value (Known (V.Int index)))
                patterns
          | Callable _ | Record_fields _ | Record_choice _ -> refuse "pattern"
        in
        if List.length parts < List.length patterns then refuse "pattern";
        let rec bind_all env patterns parts =
          match (patterns, parts) with
          | [], _ -> env
          | p :: ps, v :: vs -> bind_all (bind statements cache source env p v) ps vs
          | _ -> refuse "pattern"
        in
        bind_all env patterns parts

  and as_callable source = function
    | Callable f -> f
    | Known (V.Fn f) -> (
        let identity = E.Residual.function_id f in
        match (E.Residual.function_body f, E.Residual.function_origin f) with
        | Some (params, body), Some (instance, prefix, zone) ->
            let env =
              Names.of_seq
                (List.to_seq
                   (List.map (fun (n, v) -> (n, Known v)) (E.Residual.function_bindings f)))
            in
            Closure { params; body; env; instance; prefix; zone; identity }
        | _ -> (
            match E.Residual.function_name f with
            | Some name -> Named { name; identity }
            | None -> refuse "fn"))
    | _ ->
        ignore source;
        refuse "fn"

  and call ?(borrow = false) statements cache source ty specialized fn values =
    let identity = match fn with Closure cl -> cl.identity | Named n -> n.identity in
    let index =
      ensure statements cache source
        (emit statements source Flow.Ty.Int (Call_index (call_slot identity)))
    in
    let source = { source with C.iter = source.iter @ [ Local_index index ] } in
    match fn with
    | Closure cl ->
        if List.compare_lengths cl.params values <> 0 then refuse "call_fn";
        let source =
          {
            source with
            C.instance = cl.instance;
            prefix = cl.prefix;
            site = cl.zone;
            owner = cl.zone;
          }
        in
        let env =
          List.fold_left2
            (fun env (pattern, ty) value ->
              let value =
                match ty with None -> value | Some ty -> coerce statements cache source ty value
              in
              bind statements cache source env pattern value)
            cl.env cl.params values
        in
        term ~borrow statements cache source env (Option.value specialized ~default:cl.body)
    | Named n -> (
        match E.Residual.residual_definition (Lazy.force root) n.name with
        | Some _ -> definition ~borrow statements cache source n.name specialized values
        | None -> (
            let o =
              match
                Flow.Op.find
                  ~extra:(E.Residual.residual_ops (Lazy.force root))
                  n.name Flow.Context.value
              with
              | Some o when o.ctx = Flow.Context.value && o.shape = Flow.Op.Scalar && shipped o -> o
              | _ -> refuse n.name
            in
            let names = Array.of_list (List.map fst (o.signature.pos @ o.signature.opt)) in
            let args =
              List.mapi
                (fun i v ->
                  ( (if i < Array.length names then names.(i)
                     else match o.signature.rest with Some (n, _) -> n | None -> refuse n.name),
                    v ))
                values
            in
            match known_values args with
            | Some args when not o.live ->
                Known (o.body ~live:(Input.at_time 0.) ~node:(fun _ _ -> refuse n.name) args)
            | _ -> operation statements cache source ty n.name args))

  and definition ?(borrow = false) statements cache source name specialized values =
    let declaration =
      match E.Residual.residual_definition (Lazy.force root) name with
      | Some d -> d
      | None -> refuse "call_fn"
    in
    if List.compare_lengths declaration.inputs values <> 0 then refuse "call_fn";
    let zone = [ "def:" ^ name ] in
    let source =
      { source with C.prefix = source.prefix @ source.site; site = zone; owner = zone }
    in
    let env =
      List.fold_left2
        (fun env (name, ty, _) value ->
          Names.add name (coerce statements cache source ty value) env)
        Names.empty declaration.inputs values
    in
    term ~borrow statements cache source env (Option.value specialized ~default:declaration.body)

  and operation statements cache source ty name args =
    let prim ty op args = primitive statements cache source ty op args in
    let arg key = List.assoc key args in
    let float v = prim Flow.Ty.Float Float_convert [ ("x", v) ] in
    let calc op a b = prim Flow.Ty.Float (Numeric op) [ ("a", a); ("b", b) ] in
    let vec components =
      emit statements source Flow.Ty.Vec3
        (Vec (List.map (ensure statements cache source) components))
    in
    match primitive_of_name name with
    | Some op -> prim ty op args
    | None -> (
        match name with
        | "exact" -> arg "value"
        | "count" -> sequence_count statements cache source (arg "list")
        | "nth" -> sequence_get statements cache source (arg "list") (arg "index")
        | "range" | "take" | "drop" | "rest" | "reverse" | "first" | "last" | "array/sum"
        | "linspace" ->
            sequence_operation ~primitive ~ensure ~sequence_count ~sequence_get ~iterate ~store
              ~hold ~update ~branch ~coerce ~local ~number ~statement statements cache source ty
              name args
        | "pointer/down" | "key/down" -> (
            let wanted, names =
              if name = "key/down" then (arg "key", "keys")
              else
                ( Option.value ~default:(Known (V.Text "left")) (List.assoc_opt "button" args),
                  "buttons" )
            in
            match (name, wanted) with
            | "pointer/down", Known (V.Text b)
              when not (List.mem b [ "left"; "right"; "middle"; "x1"; "x2" ]) ->
                prim Flow.Ty.Bool
                  (Fail ("E_RANGE", "Pointer button is left, right, middle, x1 or x2."))
                  []
            | "pointer/down", Known (V.Text _) | "key/down", _ ->
                let input =
                  emit statements source
                    (Flow.Ty.Record
                       [
                         ("buttons", Flow.Ty.List Flow.Ty.Text); ("keys", Flow.Ty.List Flow.Ty.Text);
                       ])
                    (Frame Input)
                in
                let held = field statements cache source (Flow.Ty.List Flow.Ty.Text) input names in
                let found = hold statements cache source (Known (V.Bool false)) in
                iterate statements cache source held (fun body cache _ item ->
                    let same =
                      primitive body cache source Flow.Ty.Bool Equal [ ("a", item); ("b", wanted) ]
                    in
                    update body cache source found
                      (primitive body cache source Flow.Ty.Bool (Numeric Or)
                         [ ("a", found); ("b", same) ]));
                found
            | _ -> refuse name)
        | "value/hsv" ->
            let h = float (arg "h") in
            let s = float (arg "s") in
            let v = float (arg "v") in
            let c ty op args = primitive statements cache source ty op args in
            let one = Known (V.Float 1.) in
            let wrapped =
              c Flow.Ty.Float Remainder
                [
                  ( "a",
                    c Flow.Ty.Float (Numeric Add)
                      [ ("a", c Flow.Ty.Float Remainder [ ("a", h); ("b", one) ]); ("b", one) ] );
                  ("b", one);
                ]
            in
            let scaled =
              c Flow.Ty.Float (Numeric Mul) [ ("a", wrapped); ("b", Known (V.Float 6.)) ]
            in
            let sector =
              c Flow.Ty.Int Int_convert [ ("x", c Flow.Ty.Float Floor [ ("x", scaled) ]) ]
            in
            let f =
              c Flow.Ty.Float (Numeric Sub)
                [ ("a", scaled); ("b", c Flow.Ty.Float Float_convert [ ("x", sector) ]) ]
            in
            let m = c Flow.Ty.Int (Integer Imod) [ ("a", sector); ("b", Known (V.Int 6)) ] in
            let build body cache k =
              let p ty op args = primitive body cache source ty op args in
              let mul a b = p Flow.Ty.Float (Numeric Mul) [ ("a", a); ("b", b) ]
              and sub a b = p Flow.Ty.Float (Numeric Sub) [ ("a", a); ("b", b) ] in
              let low () = mul v (sub one s) in
              let falling () = mul v (sub one (mul f s)) in
              let rising () = mul v (sub one (mul (sub one f) s)) in
              let components =
                match k with
                | 0 ->
                    let t = rising () in
                    let p = low () in
                    [ v; t; p ]
                | 1 ->
                    let q = falling () in
                    let p = low () in
                    [ q; v; p ]
                | 2 ->
                    let p = low () in
                    let t = rising () in
                    [ p; v; t ]
                | 3 ->
                    let p = low () in
                    let q = falling () in
                    [ p; q; v ]
                | 4 ->
                    let t = rising () in
                    let p = low () in
                    [ t; p; v ]
                | _ ->
                    let p = low () in
                    let q = falling () in
                    [ v; p; q ]
              in
              emit body source Flow.Ty.Vec3 (Vec (List.map (ensure body cache source) components))
            in
            let rec arm body cache k =
              if k = 5 then build body cache k
              else
                let is =
                  primitive body cache source Flow.Ty.Bool (Numeric Eq)
                    [ ("a", m); ("b", Known (V.Int k)) ]
                in
                branch body cache source Flow.Ty.Vec3 is
                  (fun body cache -> build body cache k)
                  (fun body cache -> arm body cache (k + 1))
            in
            arm statements cache 0
        | "frame/dt" -> frame emit statements cache source ty Dt
        | "frame/width" -> frame emit statements cache source ty Width
        | "frame/height" -> frame emit statements cache source ty Height
        | "frame/index" -> frame emit statements cache source ty Index
        | "frame/input" -> frame emit statements cache source ty Input
        | "pointer/x" -> frame emit statements cache source ty Pointer_x
        | "pointer/y" -> frame emit statements cache source ty Pointer_y
        | "value/lerp" ->
            let u = float (arg "u") in
            let inverse = calc Sub (Known (V.Float 1.)) u in
            let blend a b =
              let left = calc Mul a inverse in
              let right = calc Mul b u in
              calc Add left right
            in
            if ty = Flow.Ty.Vec3 then
              let component v name =
                match v with
                | Known (V.Vec3 _) -> field statements cache source Flow.Ty.Float v name
                | Known _ -> float v
                | Computed (_, Flow.Ty.Vec3) -> field statements cache source Flow.Ty.Float v name
                | _ -> float v
              in
              vec
                (List.map
                   (fun name ->
                     let a = component (arg "a") name in
                     let b = component (arg "b") name in
                     blend a b)
                   [ "x"; "y"; "z" ])
            else
              let a = float (arg "a") in
              let b = float (arg "b") in
              blend a b
        | "value/polar" ->
            let r = float (arg "radius") in
            let a = float (arg "angle") in
            let cosine = prim Flow.Ty.Float (Math Cos) [ ("x", a) ] in
            let sine = prim Flow.Ty.Float (Math Sin) [ ("x", a) ] in
            let x = calc Mul r cosine in
            let z = calc Mul r sine in
            vec [ x; Option.value ~default:(Known (V.Float 0.)) (List.assoc_opt "height" args); z ]
        | "color/hsl" | "color/gradient" ->
            color_operation ~primitive ~field ~branch ~emit ~ensure statements cache source ty name
              args
        | "dot" -> (
            let products =
              List.map
                (fun component ->
                  let a = field statements cache source Flow.Ty.Float (arg "a") component in
                  let b = field statements cache source Flow.Ty.Float (arg "b") component in
                  calc Mul a b)
                [ "x"; "y"; "z" ]
            in
            match products with [ x; y; z ] -> calc Add (calc Add x y) z | _ -> assert false)
        | "cross" -> (
            let a =
              List.map (field statements cache source Flow.Ty.Float (arg "a")) [ "x"; "y"; "z" ]
            in
            let b =
              List.map (field statements cache source Flow.Ty.Float (arg "b")) [ "x"; "y"; "z" ]
            in
            match (a, b) with
            | [ x; y; z ], [ u; v; w ] ->
                let component a b c d =
                  prim Flow.Ty.Float Fma
                    [ ("a", calc Mul (Known (V.Float (-1.))) c); ("b", d); ("c", calc Mul a b) ]
                in
                vec [ component y w z v; component z u x w; component x v y u ]
            | _ -> assert false)
        | "length" | "normalize" -> (
            let components =
              List.map
                (field statements cache source Flow.Ty.Float (arg "vector"))
                [ "x"; "y"; "z" ]
            in
            let squares = List.map (fun v -> calc Mul v v) components in
            match squares with
            | [ x; y; z ] ->
                let xy = calc Add x y in
                let sum = calc Add xy z in
                let size = prim Flow.Ty.Float (Math Sqrt) [ ("x", sum) ] in
                if name = "length" then size
                else
                  let zero = Known (V.Float 0.) in
                  let empty = prim Flow.Ty.Bool (Numeric Eq) [ ("a", size); ("b", zero) ] in
                  branch statements cache source Flow.Ty.Vec3 empty
                    (fun _ _ -> Known (V.Vec3 (0., 0., 0.)))
                    (fun body cache ->
                      let reciprocal =
                        primitive body cache source Flow.Ty.Float (Numeric Div)
                          [ ("a", Known (V.Float 1.)); ("b", size) ]
                      in
                      let values =
                        List.map
                          (fun value ->
                            primitive body cache source Flow.Ty.Float (Numeric Mul)
                              [ ("a", value); ("b", reciprocal) ])
                          components
                      in
                      emit body source Flow.Ty.Vec3
                        (Vec (List.map (ensure body cache source) values)))
            | _ -> assert false)
        | _ -> refuse name)

  let result =
    try
      let body = ref [] in
      let cache = Hashtbl.create 16 in
      let results =
        match draw with
        | Some build ->
            build
              {
                value = (fun source v -> ensure body cache source (Known v));
                local;
                emit = (fun source node -> body := statement (number ()) source node :: !body);
              };
            [||]
        | None ->
            let root = Lazy.force root in
            let result = residual body cache root in
            let view = E.Residual.residual_view root in
            let source = { (source_of_view view) with C.span = Some view.term.form.span } in
            [| ensure body cache source result |]
      in
      Ok
        (constant_storage
           {
             C.constants = Array.of_list (List.rev !constants);
             locals = Array.of_list (List.rev !locals);
             body = List.rev !body;
             results;
             state_slots = Array.of_list (List.rev !slots);
             regions = Array.of_list !regions;
             call_slots = !next_call;
           })
    with
    | Unsupported name -> Error name
    | Diagnostic.Error _ | Not_found | Invalid_argument _ | Failure _ -> Error "constant"
end

let lower_with ?trace ?budget ?(lowered = ignore) ~strict ~region_allowed ~draw root =
  let module L = Lowerer (struct
    let trace = trace
    let budget = budget
    let strict = strict
    let region_allowed = region_allowed
    let draw = draw
    let root = root
    let lowered = lowered
  end) in
  L.result

let lower ?trace ?(strict = false) ?budget ?lowered root =
  lower_with ?trace ?budget ?lowered ~strict
    ~region_allowed:(fun _ -> true)
    ~draw:None (Lazy.from_val root)

let lower_draw ?(region_allowed = fun _ -> true) ~root build =
  lower_with ~strict:false ~region_allowed ~draw:(Some build) root
