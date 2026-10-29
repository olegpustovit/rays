(* Fusion on regions: a map-like region absorbs the Collect + Zip regions that feed
   it when their counts or origins agree, sharing equal instructions, within the
   register limit. Declining leaves the region unfused; it never fails. *)
module E = Flow.Eval
module W = Flow.Workspace
open Core.Region

let rec bare = function Guarded (_, body) -> bare body | body -> body

let source_input (term : W.term) source width component =
  match term.ty with
  | Flow.Ty.Array Flow.Ty.Int -> Int_input source
  | Flow.Ty.Array Flow.Ty.Text -> Text_input source
  | _ -> Input (source, width, component)

(* An accumulating register program: instructions equal up to registers share one
   register, and exceeding the register limit raises [Core_region.Unsupported]. *)
type builder = {
  emit : instruction -> int;
  append_source : E.residual * W.term -> int -> int;
  append_uniforms : E.value array -> int;
  append_code : Core.Region.t -> (int -> int -> int -> int) -> int -> int array array;
  finish : unit -> (E.residual * W.term) array * int array * E.value array * instruction array;
}

let builder () =
  let sources = ref [] and widths = ref [] and nsources = ref 0 in
  let uniforms = ref [] and nuniforms = ref 0 and code = ref [] and ncode = ref 0 in
  let shared = Hashtbl.create 64 and emitted = Hashtbl.create 64 in
  let scope = ref [] and finishes = Hashtbl.create 8 in
  let emit instruction =
    let key = (!scope, Core_region.instruction_key instruction) in
    let control =
      match bare instruction with Loop_begin _ | Loop_carry _ | Loop_end _ -> true | _ -> false
    in
    match if control then None else Hashtbl.find_opt shared key with
    | Some id -> id
    | None ->
        if !ncode >= Flow.Packed_ops.register_limit then raise Core_region.Unsupported;
        let id = !ncode in
        incr ncode;
        code := instruction :: !code;
        Hashtbl.add emitted id instruction;
        if not control then Hashtbl.add shared key id;
        id
  in
  let append_source source width =
    let i = !nsources in
    incr nsources;
    sources := source :: !sources;
    widths := width :: !widths;
    i
  in
  let append_uniforms values =
    let offset = !nuniforms in
    Array.iter
      (fun v ->
        incr nuniforms;
        uniforms := v :: !uniforms)
      values;
    offset
  in
  let append_code p input uniform =
    let registers = Array.make (Array.length p.code) 0 in
    Array.iteri
      (fun i instruction ->
        let at n = registers.(n) in
        let input_register = function
          | Input (source, width, component) ->
              let register = input source width component in
              Some
                (if Core.Region.integer_register (Hashtbl.find emitted register) then
                   emit (Convert (To_float, register))
                 else register)
          | Int_input source ->
              let register = input source 1 0 in
              if not (Core.Region.integer_register (Hashtbl.find emitted register)) then
                raise Core_region.Unsupported;
              Some register
          | Text_input source ->
              let register = input source 1 0 in
              if not (Core.Region.text_register (Hashtbl.find emitted register)) then
                raise Core_region.Unsupported;
              Some register
          | _ -> None
        in
        let rec remap instruction =
          match instruction with
          | Uniform (n, component) -> Uniform (uniform + n, component)
          | Array_prepare (n, ty) -> Array_prepare (uniform + n, ty)
          | Gather (a, i, w, c) -> Gather (at a, at i, w, c)
          | Int_gather (a, i) -> Int_gather (at a, at i)
          | Text_gather (a, i) -> Text_gather (at a, at i)
          | Text_select (c, a, b) -> Text_select (at c, at a, at b)
          | Text_uniform n -> Text_uniform (uniform + n)
          | Int_uniform n -> Int_uniform (uniform + n)
          | Binary (op, a, b) -> Binary (op, at a, at b)
          | Int_binary (op, a, b) -> Int_binary (op, at a, at b)
          | Int_compare (op, a, b) -> Int_compare (op, at a, at b)
          | Unary (op, a) -> Unary (op, at a)
          | Int_abs a -> Int_abs (at a)
          | Convert (op, a) -> Convert (op, at a)
          | Noise3 (a, b, c, seed, octaves, lacunarity, gain) ->
              let live = function Fixed v -> Fixed v | Live r -> Live (at r) in
              Noise3 (at a, at b, at c, seed, live octaves, live lacunarity, live gain)
          | Rand_at (seed, index) -> Rand_at (at seed, at index)
          | Fma (a, b, c) -> Fma (at a, at b, at c)
          | Checked_mul (p, a, b) -> Checked_mul (p, at a, at b)
          | Trace (field, r) -> Trace (field, Array.map at r)
          | Stage (stage, r) -> Stage (stage, Array.map at r)
          | Checked_fma (p, a, b, c) -> Checked_fma (p, at a, at b, at c)
          | Select (a, b, c) -> Select (at a, at b, at c)
          | Int_select (a, b, c) -> Int_select (at a, at b, at c)
          | Loop_begin loop ->
              Loop_begin
                {
                  loop with
                  first = at loop.first;
                  last = at loop.last;
                  initial = Array.map at loop.initial;
                  finish = -1;
                }
          | Loop_carry (owner, component, ty) -> Loop_carry (at owner, component, ty)
          | Loop_end (owner, next, stop) ->
              Loop_end (at owner, Array.map at next, Option.map at stop)
          | Loop_result (owner, component, ty) -> Loop_result (at owner, component, ty)
          | Guarded (guards, body) ->
              Guarded (List.map (fun (c, yes) -> (at c, yes)) guards, remap body)
          | instruction -> instruction
        in
        (* The producer already ran eagerly. Alias its result, including a guarded
         input, rather than copying an instruction across a loop boundary. *)
        registers.(i) <-
          (match input_register (bare instruction) with
          | Some register -> register
          | None -> emit (remap instruction));
        match bare instruction with
        | Loop_begin _ -> scope := registers.(i) :: !scope
        | Loop_end (owner, _, _) ->
            Hashtbl.add finishes (at owner) registers.(i);
            scope := List.tl !scope
        | _ -> ())
      p.code;
    Array.map (Array.map (Array.get registers)) p.outputs
  in
  let finish () =
    ( Array.of_list (List.rev !sources),
      Array.of_list (List.rev !widths),
      Array.of_list (List.rev !uniforms),
      Core_region.finish_loops finishes (Array.of_list (List.rev !code)) )
  in
  { emit; append_source; append_uniforms; append_code; finish }

let rec source_region ~through_exact ~dynamic ~count_source
    ((residual : E.residual), (term : W.term)) =
  match term.node with
  | W.Ref_binding (name, []) -> (
      match List.assoc_opt name (E.Residual.residual_view residual).bindings with
      | Some (E.Residual r) ->
          source_region ~through_exact ~dynamic ~count_source (r, (E.Residual.residual_view r).term)
      | _ -> None)
  | W.Op { op = "exact"; args = [ (_, value) ]; _ } when through_exact ->
      source_region ~through_exact ~dynamic ~count_source (residual, value)
  | _ ->
      Core_region.lower ~dynamic ~count_source residual term
      |> Result.to_option
      |> Option.map (fuse ~through_exact)

and fuse ?(through_exact = false) (t : Core.Region.t) : Core.Region.t =
  if t.skip <> [||] || (t.iteration = Product && Array.length t.sources <> 1) then t
  else
    let children =
      Array.map
        (source_region ~through_exact ~dynamic:t.dynamic ~count_source:t.count_source)
        t.sources
    in
    let origins =
      Array.mapi
        (fun i -> function
          | Some p -> p.origin
          | None ->
              let r, s = t.sources.(i) in
              t.count_source r s)
        children
    in
    let origin =
      if origins <> [||] && Option.is_some origins.(0) && Array.for_all (( = ) origins.(0)) origins
      then origins.(0)
      else None
    in
    let counts =
      Array.mapi
        (fun i -> function
          | Some p -> p.count
          | None ->
              let r, s = t.sources.(i) in
              Core_region.count_of r s)
        children
    in
    let count =
      if Array.for_all Option.is_some counts && counts <> [||] then
        Some (Array.fold_left (fun n c -> min n (Option.get c)) max_int counts)
      else t.count
    in
    let t =
      Result.get_ok
        (Core.Region.rebuild t ~count
           ~origin:(match t.result with Collect -> origin | _ -> None)
           ())
    in
    let can_fuse = function
      | Some p
        when p.result = Collect && p.iteration = Zip && p.skip = [||] && Array.length p.outputs = 1
        ->
          Array.length t.sources = 1
          || (Option.is_some p.count && p.count = count)
          || (Option.is_some origin && p.origin = origin)
      | _ -> false
    in
    if not (Array.exists can_fuse children) then t
    else
      try
        let b = builder () in
        let stages = ref t.stages and sites = ref t.sites in
        let inputs =
          Array.mapi
            (fun i child ->
              if can_fuse child then begin
                let p = Option.get child in
                let inputs =
                  Array.mapi (fun i source -> b.append_source source p.widths.(i)) p.sources
                in
                let uniform = b.append_uniforms p.uniforms in
                stages := !stages @ p.stages;
                sites := p.sites @ !sites;
                (b.append_code p
                   (fun i width component ->
                     b.emit (source_input (snd p.sources.(i)) inputs.(i) width component))
                   uniform).(0)
              end
              else begin
                let source = b.append_source t.sources.(i) t.widths.(i) in
                Array.init t.widths.(i) (fun component ->
                    b.emit (source_input (snd t.sources.(i)) source t.widths.(i) component))
              end)
            children
        in
        let uniform = b.append_uniforms t.uniforms in
        let outputs = b.append_code t (fun i _ component -> inputs.(i).(component)) uniform in
        let sources, widths, uniforms, code = b.finish () in
        Core.Region.rebuild t ~sources ~widths ~uniforms ~code ~outputs ~iteration:Zip
          ~stages:!stages ~sites:!sites ()
        |> Result.value ~default:t
      with Core_region.Unsupported -> t

(* Sibling regions of one mark set: Collect + Zip regions of equal static count or origin
   become one region with every output, sharing equal instructions. [None] when they do not
   agree or the register limit declines; the regions then stay separate. *)
let merge (regions : Core.Region.t list) : Core.Region.t option =
  match regions with
  | [] -> None
  | first :: _ -> (
      let one (r : Core.Region.t) =
        r.result = Collect && r.iteration = Zip && r.skip = [||]
        && Array.length r.outputs = 1
        && r.dynamic = first.dynamic
      in
      let same_count (r : Core.Region.t) =
        (Option.is_some first.count && r.count = first.count)
        || (Option.is_some first.origin && r.origin = first.origin)
      in
      if not (List.for_all (fun r -> one r && same_count r) regions) then None
      else
        try
          let b = builder () in
          let outputs =
            List.map
              (fun (r : Core.Region.t) ->
                let inputs =
                  Array.mapi (fun i source -> b.append_source source r.widths.(i)) r.sources
                in
                let uniform = b.append_uniforms r.uniforms in
                (b.append_code r
                   (fun i width component ->
                     b.emit (source_input (snd r.sources.(i)) inputs.(i) width component))
                   uniform).(0))
              regions
          in
          let sources, widths, uniforms, code = b.finish () in
          Core.Region.rebuild first ~sources ~widths ~uniforms ~code
            ~outputs:(Array.of_list outputs)
            ~stages:(List.concat_map (fun (r : Core.Region.t) -> r.stages) regions)
            ~sites:(List.concat_map (fun (r : Core.Region.t) -> r.sites) regions)
            ()
          |> Result.to_option
        with Core_region.Unsupported -> None)

(* A device region binds what varies. A source that fills one constant ([array/int n 1], a static
   vector) becomes that constant, and sources that read the same attribute ([sop/attr g :P] bound
   twice) are one upload. *)
let rec resolved (residual, (term : W.term)) =
  match term.node with
  | W.Ref_binding (name, []) -> (
      match List.assoc_opt name (E.Residual.residual_view residual).bindings with
      | Some (E.Residual r) -> resolved (r, (E.Residual.residual_view r).term)
      | Some value -> `Value value
      | None -> `Term (residual, term))
  | _ -> `Term (residual, term)

let fill source width =
  match resolved source with
  | `Term (residual, { node = W.Op { op; args; _ }; _ }) -> (
      let value =
        match List.assoc_opt "value" args with
        | Some { node = W.Lit (Param.Int_value n); _ } -> Some (E.Int n)
        | Some { node = W.Lit (Param.Float_value x); _ } -> Some (E.Float x)
        | Some ({ node = W.Ref_binding (_, []); _ } as t) -> (
            match resolved (residual, t) with `Value v -> Some v | `Term _ -> None)
        | _ -> None
      in
      match (op, value, width) with
      | "array/int", Some (E.Int n), 1 -> Some [| Int_const n |]
      | "array/float", Some (E.Float x), 1 -> Some [| Const x |]
      | "array/float", Some (E.Int n), 1 -> Some [| Const (float n) |]
      | "array/vec3", Some (E.Vec3 (x, y, z)), 3 -> Some [| Const x; Const y; Const z |]
      | "array/vec4", Some (E.Vec4 (x, y, z, w)), 4 -> Some [| Const x; Const y; Const z; Const w |]
      | _ -> None)
  | _ -> None

let attribute_read source =
  match resolved source with
  | `Value ((E.Struct ("sop/attr", _, fields) | E.Source ("sop/attr", _, fields)) as v)
    when List.for_all (fun (_, f) -> match f with E.Residual _ -> false | _ -> true) fields ->
      Some (Flow.Value.key_of ~residual:E.Residual.residual_id v)
  | _ -> None

let compact_sources (t : Core.Region.t) =
  let n = Array.length t.sources in
  let fills = Array.mapi (fun i source -> fill source t.widths.(i)) t.sources in
  let reads = Array.map attribute_read t.sources in
  let canonical =
    Array.init n (fun i ->
        let rec find j =
          if j >= i then i
          else if
            fills.(j) = None
            && reads.(i) <> None
            && reads.(j) = reads.(i)
            && t.widths.(j) = t.widths.(i)
          then j
          else find (j + 1)
        in
        if fills.(i) <> None then i else find 0)
  in
  let changed =
    Array.exists Option.is_some fills || Array.exists2 ( <> ) canonical (Array.init n Fun.id)
  in
  if t.iteration <> Zip || (not changed) || Array.for_all Option.is_some fills then t
  else
    try
      let b = builder () in
      let slots = Array.make n (-1) in
      Array.iteri
        (fun i source ->
          if fills.(i) = None && canonical.(i) = i then
            slots.(i) <- b.append_source source t.widths.(i))
        t.sources;
      let registers =
        Array.init n (fun i ->
            match fills.(i) with
            | Some constants -> Array.map b.emit constants
            | None ->
                let j = canonical.(i) in
                Array.init t.widths.(i) (fun component ->
                    b.emit (source_input (snd t.sources.(j)) slots.(j) t.widths.(j) component)))
      in
      let uniform = b.append_uniforms t.uniforms in
      let outputs = b.append_code t (fun i _ component -> registers.(i).(component)) uniform in
      let sources, widths, uniforms, code = b.finish () in
      Core.Region.rebuild t ~sources ~widths ~uniforms ~code ~outputs () |> Result.value ~default:t
    with Core_region.Unsupported -> t
