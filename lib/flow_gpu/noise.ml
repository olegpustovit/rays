(* The device side of noise3. A kernel inlines noise.rays' noise-fbm, compiled once an octave count, at
   each Noise3, its permutation gathers becoming reads of the seed's table entries (Region.Noise_table):
   MSL and SPIR-V print the noise from the region IR like any helper. *)
module P = Flow_ir.Packed

let variants = Hashtbl.create 4
and lock = Mutex.create ()

(* the helper of an octave count, [fractal] taking its lacunarity and gain as inputs 3 and 4; a
   live count ([None]) is input 5, its fold's bound *)
let helper ~fractal octaves =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt variants (octaves, fractal) with
      | Some compiled -> compiled
      | None ->
          let declarations =
            Noise_source.source
            ^
            match octaves with
            | None ->
                "(defn noise-entry :context value [(table : (array int)) (x : float) (y : float) \
                 (z : float) (l : float) (g : float) (o : int)] (noise-fbm-live table [x y z] o l \
                 g))"
            | Some octaves ->
                if fractal then
                  Printf.sprintf
                    "(defn noise-entry :context value [(table : (array int)) (x : float) (y : \
                     float) (z : float) (l : float) (g : float)] (noise-fbm-fractal table [x y z] \
                     %d l g))"
                    octaves
                else
                  Printf.sprintf
                    "(defn noise-entry :context value [(table : (array int)) (x : float) (y : \
                     float) (z : float)] (noise-fbm table [x y z] %d))"
                    octaves
          in
          let compiled =
            Result.map
              (fun (stage : Lisp_stage.stage) -> stage.program)
              (Lisp_stage.compile ~declarations "noise-entry")
          in
          Hashtbl.replace variants (octaves, fractal) compiled;
          compiled)

(* The program with each Noise3 replaced by its helper, and [back] as Trace.expand's but naming the
   register that replaces a Noise3 by its slot. *)
let expand (program : P.t) =
  let code = program.code in
  if
    not
      (Array.exists
         (fun i -> match snd (Trace.flat [] i) with P.Noise3 _ -> true | _ -> false)
         code)
  then Ok (program, Fun.id)
  else
    let out = ref [] and size = ref 0 and back = ref [] in
    let push old instruction =
      out := instruction :: !out;
      back := old :: !back;
      incr size;
      !size - 1
    in
    let map = Array.make (Array.length code) (-1) and results = ref [] in
    try
      Array.iteri
        (fun slot instruction ->
          match Trace.flat [] instruction with
          | guards, P.Noise3 (x, y, z, seed, octaves, lacunarity, gain) ->
              let count = match octaves with P.Fixed n -> Some n | Live _ -> None in
              let fractal = count = None || lacunarity <> P.Fixed 2. || gain <> P.Fixed 0.5 in
              let helper =
                match helper ~fractal count with
                | Ok compiled -> compiled
                | Error d -> raise (Diagnostic.Error d)
              in
              let guards = List.map (fun (c, yes) -> (map.(c), yes)) guards in
              let register = function P.Fixed v -> push (-1) (P.Const v) | Live r -> map.(r) in
              let operands =
                Array.append
                  [| map.(x); map.(y); map.(z) |]
                  (if fractal then [| register lacunarity; register gain |] else [||])
              in
              let operands =
                match octaves with
                | Fixed _ -> operands
                | Live r -> Array.append operands [| map.(r) |]
              in
              let hm = Array.make (Array.length helper.code) (-1) in
              Array.iteri
                (fun j i ->
                  match Trace.flat [] i with
                  | _, (P.Input (k, _, _) | Int_input k) -> hm.(j) <- operands.(k)
                  | _, P.Array_prepare _ -> ()
                  | inner, body ->
                      let body =
                        match body with
                        | P.Int_gather (_, index) -> P.Stage (Noise_table seed, [| index |])
                        | body -> body
                      in
                      hm.(j) <-
                        push (-1)
                          (Trace.wrap
                             (guards @ List.map (fun (c, yes) -> (hm.(c), yes)) inner)
                             (Trace.remap (fun r -> hm.(r)) body)))
                helper.code;
              map.(slot) <- hm.(helper.outputs.(0).(0));
              results := slot :: !results
          | _ -> map.(slot) <- push slot (Trace.remap (fun r -> map.(r)) instruction))
        code;
      let back = Array.of_list (List.rev !back) in
      List.iter (fun slot -> back.(map.(slot)) <- slot) !results;
      Trace.rebuild program (List.rev !out) back map
    with Diagnostic.Error d -> Error d

(* A kernel's scene/trace and noise3 helpers spliced, [back] naming each register's slot in [program]. *)
let helpers ~instanced ~motion ~curves ~spheres program =
  Result.bind (Trace.expand ~instanced ~motion ~curves ~spheres program) (fun (program, traced) ->
      Result.map
        (fun (program, noised) ->
          ( program,
            fun slot ->
              let s = noised slot in
              if s < 0 then -1 else traced s ))
        (expand program))

(* An exact kernel checks every operation finite; a helper's need not be, its operands are checked at
   their source and a nonfinite coordinate makes its result NaN, the register that replaces a Noise3. *)
let checked back (layout : Flow_ir.Kernel_layout.t) =
  {
    layout with
    finite_checks = Array.mapi (fun slot check -> check && back slot >= 0) layout.finite_checks;
  }
