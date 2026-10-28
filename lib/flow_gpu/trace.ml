(* The device side of scene/trace. The region keeps a scene/trace as its host fields (Trace), which the
   CPU answers through Flow.Ray_host; a traced kernel instead inlines trace.rays' trace-hit (trace-occluded
   for a ray whose only read field is hit), compiled once a scene variant, in place of those fields. MSL
   and SPIR-V then print the intersection helpers from the region IR like any helper: only the ray
   queries and the scene's table reads are the printers' (the gpu/trace-* and gpu/scene-* intrinsics). *)
module P = Flow_ir.Packed

let rec flat guards = function
  | P.Guarded (more, body) -> flat (guards @ more) body
  | body -> (guards, body)

let wrap guards instruction = if guards = [] then instruction else P.Guarded (guards, instruction)
let fail message = Error (Diagnostic.error ~code:"E_GPU_FORM" message)

(* an instruction with every register operand through [f] *)
let rec remap f = function
  | P.Binary (op, a, b) -> P.Binary (op, f a, f b)
  | Unary (op, a) -> Unary (op, f a)
  | Int_binary (op, a, b) -> Int_binary (op, f a, f b)
  | Int_compare (op, a, b) -> Int_compare (op, f a, f b)
  | Int_abs a -> Int_abs (f a)
  | Convert (op, a) -> Convert (op, f a)
  | Gather (a, i, w, c) -> Gather (f a, f i, w, c)
  | Int_gather (a, i) -> Int_gather (f a, f i)
  | Text_gather (a, i) -> Text_gather (f a, f i)
  | Text_select (c, a, b) -> Text_select (f c, f a, f b)
  | Noise3 (a, b, c, seed, octaves, lacunarity, gain) ->
      Noise3 (f a, f b, f c, seed, octaves, lacunarity, gain)
  | Rand_at (seed, index) -> Rand_at (f seed, f index)
  | Fma (a, b, c) -> Fma (f a, f b, f c)
  | Checked_mul (p, a, b) -> Checked_mul (p, f a, f b)
  | Checked_fma (p, a, b, c) -> Checked_fma (p, f a, f b, f c)
  | Trace (field, r) -> Trace (field, Array.map f r)
  | Stage (stage, r) -> Stage (stage, Array.map f r)
  | Select (c, a, b) -> Select (f c, f a, f b)
  | Int_select (c, a, b) -> Int_select (f c, f a, f b)
  | Loop_begin loop ->
      Loop_begin
        { loop with first = f loop.first; last = f loop.last; initial = Array.map f loop.initial }
  | Loop_carry (owner, component, ty) -> Loop_carry (f owner, component, ty)
  | Loop_end (owner, next, stop) -> Loop_end (f owner, Array.map f next, Option.map f stop)
  | Loop_result (owner, component, ty) -> Loop_result (f owner, component, ty)
  | Guarded (guards, body) -> Guarded (List.map (fun (c, yes) -> (f c, yes)) guards, remap f body)
  | instruction -> instruction

(* The spliced [code] (in order) as [program]'s region, each loop closed at its Loop_end, with [back]:
   each new register's original slot (-1 for a helper's), and [map]: each original slot's register. *)
let rebuild (program : P.t) code back map =
  let code = Array.of_list code in
  Array.iteri
    (fun slot i ->
      match flat [] i with
      | _, P.Loop_end (owner, _, _) ->
          let rec close = function
            | P.Loop_begin loop -> P.Loop_begin { loop with finish = slot }
            | P.Guarded (g, body) -> P.Guarded (g, close body)
            | other -> other
          in
          code.(owner) <- close code.(owner)
      | _ -> ())
    code;
  match
    Flow_ir.Core.Region.rebuild program ~code
      ~outputs:(Array.map (Array.map (fun x -> map.(x))) program.outputs)
      ()
  with
  | Ok expanded -> Ok (expanded, fun slot -> back.(slot))
  | Error (d :: _) -> Error d
  | Error [] -> fail "the kernel with its spliced helpers did not rebuild"

(* The helper of one scene variant: trace-hit's fields t, prim, u, v, normal, hit and instance (the
   host's Trace fields 0 to 8), or trace-occluded's hit, as (output, component) of its program. *)
let variants = Hashtbl.create 8
and lock = Mutex.create ()

let helper ~instanced ~motion ~strands ~spheres ~occluded =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt variants (instanced, motion, strands, spheres, occluded) with
      | Some compiled -> compiled
      | None ->
          let call =
            if occluded then
              Printf.sprintf "{:hit (trace-occluded o d tmin tmax time %d %b)}" strands spheres
            else
              Printf.sprintf "(trace-hit o d tmin tmax time %b %b %d %b)" instanced motion strands
                spheres
          in
          let declarations =
            Trace_source.source
            ^ Printf.sprintf
                "(defn trace-entry :context value [(o : vec3) (d : vec3) (tmin : float) (tmax : \
                 float) (time : float)] %s)"
                call
          in
          let compiled =
            Result.bind (Lisp_stage.compile ~declarations "trace-entry")
              (fun (stage : Lisp_stage.stage) ->
                let program = stage.program in
                let field name =
                  match
                    List.find_index (fun (leaf : Lisp_stage.leaf) -> leaf.path = name) stage.leaves
                  with
                  | Some k -> k
                  | None -> invalid_arg "Trace.helper"
                in
                let fields =
                  if occluded then Array.make 9 (field [ "hit" ], 0)
                  else
                    Array.init 9 (fun f ->
                        match f with
                        | 0 -> (field [ "t" ], 0)
                        | 1 -> (field [ "prim" ], 0)
                        | 2 -> (field [ "u" ], 0)
                        | 3 -> (field [ "v" ], 0)
                        | 4 | 5 | 6 -> (field [ "n" ], f - 4)
                        | 7 -> (field [ "hit" ], 0)
                        | _ -> (field [ "inst" ], 0))
                in
                if
                  Array.exists
                    (fun i ->
                      match snd (flat [] i) with
                      | P.Uniform _ | Array_prepare _ | Frame _ | Int_uniform _ -> true
                      | Int_frame name -> not (Flow_ir.Kernel_layout.scene_fact name)
                      | _ -> false)
                    program.code
                then fail "trace.rays: the trace helpers read no uniforms or frame facts"
                else Ok (program, fields))
          in
          Hashtbl.replace variants (instanced, motion, strands, spheres, occluded) compiled;
          compiled)

(* The program with each ray (its Trace fields of one guard set, in one loop) replaced by its helper,
   spliced before the ray's first field, and [back]: the original slot of each new register (-1 for a
   helper's), which names the host's frame values. *)
let expand ~instanced ~motion ~curves ~spheres (program : P.t) =
  let code = program.code in
  let n = Array.length code in
  if not (Array.exists (fun i -> match snd (flat [] i) with P.Trace _ -> true | _ -> false) code)
  then Ok (program, Fun.id)
  else
    let used =
      (Flow_ir.Kernel_layout.plan ~fast:true ~instanced ~motion ~curves:(curves <> None) program)
        .used
    in
    let strands = match curves with None -> 0 | Some `Loop -> 1 | Some `Native -> 2 in
    (* the innermost loop around each slot *)
    let scope = Array.make n (-1) and stack = ref [] in
    Array.iteri
      (fun slot i ->
        scope.(slot) <- (match !stack with top :: _ -> top | [] -> -1);
        match snd (flat [] i) with
        | P.Loop_begin _ -> stack := slot :: !stack
        | Loop_end _ -> stack := List.tl !stack
        | _ -> ())
      code;
    let rays = Hashtbl.create 8 in
    Array.iteri
      (fun slot i ->
        match flat [] i with
        | guards, P.Trace (field, r) ->
            let key = (scope.(slot), guards, Array.to_list r) in
            let first, fields = Option.value (Hashtbl.find_opt rays key) ~default:(slot, []) in
            Hashtbl.replace rays key (first, if used.(slot) then field :: fields else fields)
        | _ -> ())
      code;
    let starts = Hashtbl.create 8 in
    Hashtbl.iter (fun key (first, fields) -> Hashtbl.replace starts first (key, fields)) rays;
    let out = ref [] and size = ref 0 and back = ref [] in
    let push old instruction =
      out := instruction :: !out;
      back := old :: !back;
      incr size;
      !size - 1
    in
    let map = Array.make n (-1) and field_of = Hashtbl.create 16 in
    try
      for slot = 0 to n - 1 do
        (match Hashtbl.find_opt starts slot with
        | None -> ()
        | Some (((_, guards, r) as key), fields) ->
            let r = Array.of_list r in
            let occluded = List.for_all (( = ) 7) fields in
            let helper_program, outputs =
              match helper ~instanced ~motion ~strands ~spheres ~occluded with
              | Ok compiled -> compiled
              | Error d -> raise (Diagnostic.Error d)
            in
            let guards = List.map (fun (c, yes) -> (map.(c), yes)) guards in
            let hm = Array.make (Array.length helper_program.code) (-1) in
            (* elements o, d, tmin, tmax, time are the trace's operands after its scene *)
            let element k c = map.(r.((match k with 0 -> 1 | 1 -> 4 | k -> k + 5) + c)) in
            Array.iteri
              (fun j i ->
                match flat [] i with
                | _, P.Input (k, _, c) -> hm.(j) <- element k c
                | inner, body ->
                    hm.(j) <-
                      push (-1)
                        (wrap
                           (guards @ List.map (fun (c, yes) -> (hm.(c), yes)) inner)
                           (remap (fun x -> hm.(x)) body)))
              helper_program.code;
            Hashtbl.replace field_of key (fun field ->
                let k, c = outputs.(if occluded then 0 else field) in
                hm.(helper_program.outputs.(k).(c))));
        match flat [] code.(slot) with
        | guards, P.Trace (field, r) ->
            map.(slot) <- Hashtbl.find field_of (scope.(slot), guards, Array.to_list r) field
        | _ -> map.(slot) <- push slot (remap (fun x -> map.(x)) code.(slot))
      done;
      rebuild program (List.rev !out) (Array.of_list (List.rev !back)) map
    with Diagnostic.Error d -> Error d

(* a frame fact's uniform word named by its slot in the host's program, before the trace helpers *)
let frames back uniform_layout =
  List.map
    (fun (name, offset, width) ->
      match String.split_on_char ':' name with
      | [ "frame"; slot; fact ] ->
          (Printf.sprintf "frame:%d:%s" (back (int_of_string slot)) fact, offset, width)
      | _ -> (name, offset, width))
    uniform_layout
