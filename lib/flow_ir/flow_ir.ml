module Packed = Packed
module Core = Core
module Core_region = Core_region
module Core_fuse = Core_fuse
module Core_lower = Core_lower
module Core_exec = Core_exec
module Core_draw = Core_draw
module Kernel_layout = Kernel_layout
module E = Flow.Eval
module W = Flow.Workspace
module V = Flow.Value
module Ty = Flow.Ty

module Gpu = struct
  type value = {
    identity : int;
    count : int;
    width : int;
    output : int;
    stamp : int64;
    gpu_seconds : float option;
  }

  type planes = {
    count : int;
    width : int;
    materialise : unit -> float array array;
    resident : E.value option;
    settle : unit -> (unit, Diagnostic.t) result;
    release : held:bool -> unit;
    bounds : unit -> float array option;
  }

  type kernel = {
    run : Packed.Host.inputs -> (value, Diagnostic.t) result;
    readback : value -> (E.value, Diagnostic.t) result;
    planes : value -> (planes, Diagnostic.t) result;
  }

  let planes_of readback (value : value) =
    Result.bind (readback value) (function
      | E.Float_array data | Vec2_array data | Vec3_array data | Vec4_array data ->
          let width = value.width in
          let count = Array.length data / max 1 width in
          let planes =
            Array.init width (fun c -> Array.init count (fun i -> data.((i * width) + c)))
          in
          Ok
            {
              count;
              width;
              materialise = (fun () -> planes);
              resident = None;
              settle = (fun () -> Ok ());
              release = (fun ~held:_ -> ());
              bounds = (fun () -> None);
            }
      | _ -> Error (Diagnostic.error ~code:"E_GPU" "GPU output is not a float array."))

  type backend = {
    cost : fast:bool -> Packed.t -> count:int -> float option;
    prepare : fast:bool -> Packed.t -> (kernel, Diagnostic.t) result;
  }

  type policy = Measured | Qualification
  type config = { backend : backend option; fast : bool; policy : policy }

  let default = { backend = None; fast = false; policy = Measured }
end

type id = W.path * int list
type tier = Interp | Core | Cpu_kernel | Gpu | Gpu_compile | Gpu_readback

module Cost = struct
  type tier_cost = { fixed : float; per_element : float }

  let table = function
    | Interp -> { fixed = 0.; per_element = 0.710282087 /. 1_000_000. }
    | Core -> { fixed = 0.000000170; per_element = 0. }
    | Cpu_kernel ->
        let per_element = (0.016522884 -. 0.000040054) /. (1_000_000. -. 1024.) in
        { fixed = 0.000040054 -. (per_element *. 1024.); per_element }
    | Gpu ->
        let per_element = (0.020215034 -. 0.000437975) /. (1_000_000. -. 1024.) in
        { fixed = 0.000437975 -. (per_element *. 1024.); per_element }
    | Gpu_readback ->
        let per_element = (0.007947922 -. 0.000010014) /. (1_000_000. -. 1024.) in
        { fixed = 0.000010014 -. (per_element *. 1024.); per_element }
    | Gpu_compile -> { fixed = 0.008223057; per_element = 0. }

  let estimate tier ~count =
    if count < 0 then invalid_arg "Flow_ir.Cost.estimate: negative count";
    let cost = table tier in
    cost.fixed +. (cost.per_element *. float count)

  let display_sink tier ~count =
    if count < 0 then invalid_arg "Flow_ir.Cost.display_sink: negative count";
    float count
    *.
    match tier with
    | Gpu -> ((0.028567791 -. 0.001480103) /. 990_000.) -. (table Gpu).per_element
    | _ -> ((0.903350830 -. 0.010123968) /. 990_000.) -. (table Cpu_kernel).per_element

  let cheapest ~legal ~count =
    match legal with
    | [] -> invalid_arg "Flow_ir.Cost.cheapest: no legal tiers"
    | first :: rest ->
        List.fold_left
          (fun best tier -> if estimate tier ~count < estimate best ~count then tier else best)
          first rest

  let packed ~count = cheapest ~legal:[ Interp; Cpu_kernel ] ~count

  (* A fused SOP write on the device (fast precision; specification/sop.md, "Device placement"):
     a region of at least 2^18 elements that samples noise. Upload, dispatch and readback cost about
     what a light arithmetic map costs on the CPU blocks, so only noise (about 0.1 microseconds an
     element and octave on the CPU) pays for the trip. [Qualification] places every legal region. *)
  let sop_device policy (region : Core.Region.t) ~count =
    policy = Gpu.Qualification
    || count >= 1 lsl 18
       && Array.exists
            (fun instruction ->
              let rec noise = function
                | Core.Region.Noise3 _ -> true
                | Guarded (_, body) -> noise body
                | _ -> false
              in
              noise instruction)
            region.code
end

(* A static packed map of more than 4,096 elements runs as the kernel it would be at frame rate
   instead of through the walker for every element (about 22 microseconds and 79 KB each). The
   evaluator checks the result against the elements it walked and walks the rest itself when the
   kernel refuses, fails or reads anything live. *)
let static_map residual =
  let value = E.Residual residual in
  if E.frame_dependent value || E.state_dependent value then None
  else
    match Packed.compile residual (E.Residual.residual_view residual).term with
    | None -> None
    | Some packed -> (
        match Packed.force packed ~live:(Input.at_time 0.) with
        | Ok value -> Some value
        | Error _ | (exception (Diagnostic.Error _ | Invalid_argument _ | Not_found | Failure _)) ->
            None)

let static ?record ?inputs workspace =
  E.Residual.handle ~bulk:static_map (fun () ->
      E.Host.static_with_requests ?record ?inputs workspace)

let qualify_workspace ?record ?inputs ?observe ?(gpu = Gpu.default) (workspace : W.t) =
  let roots = Hashtbl.create 32
  and index = Hashtbl.create 32
  and outcomes = Hashtbl.create 32
  and callable_owners = Hashtbl.create 16 in
  let callable owner (body : W.term) =
    let entries = Option.value ~default:[] (Hashtbl.find_opt callable_owners body.form.id) in
    if not (List.exists (fun (form, path) -> form == body.form && path = owner) entries) then
      Hashtbl.replace callable_owners body.form.id ((body.form, owner) :: entries)
  in
  List.iter
    (fun (form, path) ->
      let entries = Option.value ~default:[] (Hashtbl.find_opt roots form.Flow.Syntax.id) in
      Hashtbl.replace roots form.id ((form, path) :: entries))
    workspace.packed_roots;
  let add owner (term : W.term) =
    let producers =
      Option.value ~default:[] (Hashtbl.find_opt roots term.form.id)
      |> List.fold_left
           (fun paths (form, path) -> if form == term.form then W.Paths.add path paths else paths)
           W.Paths.empty
    in
    let producers =
      match term.path with
      | Some path when W.Paths.mem path producers -> W.Paths.singleton path
      | _ when W.Paths.mem owner producers -> W.Paths.singleton owner
      | _ -> producers
    in
    let entries = Option.value ~default:[] (Hashtbl.find_opt index term.form.id) in
    let same (form, base, _) = form == term.form && base = owner in
    let old =
      List.find_opt same entries
      |> Option.fold ~none:W.Paths.empty ~some:(fun (_, _, paths) -> paths)
    in
    Hashtbl.replace index term.form.id
      ((term.form, owner, W.Paths.union old producers)
      :: List.filter (fun e -> not (same e)) entries)
  in
  let rec walk collect owner (term : W.term) =
    let owner = Option.value term.path ~default:owner in
    let visit = walk collect in
    let all = List.iter (visit owner) and fields fs = List.iter (fun (_, t) -> visit owner t) fs in
    (if not collect then
       match term.node with
       | W.Hof ((`Map | `Reduce), _) | W.Loop _ | W.Op { op = "array/sum" | "image/map"; _ } ->
           add owner term
       | _ -> ());
    match term.node with
    | W.Vec ts | List_lit ts | Str ts | List_op (_, ts) | Hof (_, ts) -> all ts
    | Call { args; _ } | Op { args; _ } | Record args -> fields args
    | Graph_ref { inputs; _ } -> fields inputs
    | Let (bindings, body) ->
        fields bindings;
        visit owner body
    | Loop { zone; accs; clauses; body; _ } ->
        List.iter (fun (_, t) -> visit zone t) (accs @ clauses);
        visit zone body
    | Fn { zone; body; capture; _ } ->
        if collect then callable zone body;
        Option.iter (visit owner) capture;
        visit zone body
    | Call_fn { args; body; _ } ->
        all args;
        Option.iter
          (fun (body : W.term) ->
            let owners =
              Option.value ~default:[] (Hashtbl.find_opt callable_owners body.form.id)
              |> List.filter_map (fun (form, owner) ->
                  if form == body.form then Some owner else None)
              |> List.sort_uniq compare
            in
            visit (match owners with [ owner ] -> owner | _ -> owner) body)
          body
    | State { init; step; zone; _ } ->
        visit owner init;
        visit zone step
    | With_error (a, b) -> all [ a; b ]
    | Error_rows { rows = a; mapper = b; _ } -> all [ a; b ]
    | If (c, a, b) -> all [ c; a; b ]
    | Cond (arms, d) ->
        List.iter (fun (a, b) -> all [ a; b ]) arms;
        visit owner d
    | Case (s, arms, d) ->
        visit owner s;
        fields arms;
        visit owner d
    | Assoc (r, updates) ->
        visit owner r;
        fields updates
    | Get (t, _) | Expanded { body = t; _ } -> visit owner t
    | Bypass call -> Option.iter (visit owner) (W.bypass_input call)
    | Lit _ | Text _ | Nil | Time | Ref_binding _ | Fn_ref _ -> ()
  in
  List.iter (fun (graph : W.graph) -> callable [ "def:" ^ graph.name ] graph.body) workspace.defs;
  List.iter
    (fun collect ->
      List.iter
        (fun (graph : W.graph) ->
          let owner =
            if List.exists (fun (g : W.graph) -> g == graph) workspace.defs then
              [ "def:" ^ graph.name ]
            else [ graph.name ]
          in
          List.iter (fun (_, _, term) -> Option.iter (walk collect owner) term) graph.inputs;
          walk collect owner graph.body)
        (workspace.graphs @ workspace.defs))
    [ true; false ];
  let pending path =
    [
      Diagnostic.error ~code:"E_PACKED_PENDING"
        ("No unambiguous static specialization was observed for producer " ^ String.concat "/" path
       ^ ".");
    ]
  in
  let observed residual =
    let view = E.Residual.residual_view residual in
    let producers =
      Option.value ~default:[] (Hashtbl.find_opt index view.term.form.id)
      |> List.find_opt (fun (form, owner, _) -> form == view.term.form && owner = view.owner)
      |> Option.fold ~none:W.Paths.empty ~some:(fun (_, _, paths) -> paths)
    in
    let producer =
      if W.Paths.cardinal producers = 1 then Some (W.Paths.choose producers) else None
    in
    (if producer = None then
       let possible =
         Option.value ~default:[] (Hashtbl.find_opt roots view.term.form.id)
         |> List.fold_left
              (fun paths (form, path) ->
                if form == view.term.form then W.Paths.add path paths else paths)
              producers
       in
       W.Paths.iter
         (fun path ->
           match Hashtbl.find_opt outcomes path with
           | Some (_ :: _) -> ()
           | _ -> Hashtbl.replace outcomes path (pending path))
         possible);
    let adapted =
      match (producer, view.term.node) with
      | Some path, W.Op { op = "image/map"; args; _ } -> (
          let canonical =
            match
              ( Flow.Op.find ~extra:workspace.ops "image/map" Flow.Context.image,
                Flow.Op.find "image/map" Flow.Context.image )
            with
            | Some actual, Some builtin -> actual == builtin
            | _ -> false
          in
          if not canonical then
            Error
              [
                Diagnostic.error ~code:"E_GPU_FORM"
                  "Image qualification needs the canonical image/map declaration.";
              ]
          else
            match List.assoc_opt "function" args with
            | None ->
                Error [ Diagnostic.error ~code:"E_KERNEL_FORM" "Image map needs a pixel function." ]
            | Some term ->
                let mapped =
                  Result.map_error
                    (fun d -> [ d ])
                    (Result.bind
                       (E.Residual.eval_term residual term ~live:(Input.at_time 0.))
                       (function
                         | E.Fn fn ->
                             E.Host.map_function ~path
                               ~site:(view.instance, view.site, view.iter)
                               ~signature:Ty.{ params = [ Vec2 ]; result = Vec4 }
                               fn
                               [ E.Vec2_array [| 0.5; 0.5 |] ]
                         | _ ->
                             Error
                               (Diagnostic.error ~code:"E_KERNEL_FORM"
                                  "Image map needs a pixel function.")))
                in
                Result.bind mapped (function
                  | E.Residual r -> Ok r
                  | _ ->
                      Error
                        [
                          Diagnostic.error ~code:"E_KERNEL_FORM"
                            "Image map did not produce a pixel program.";
                        ]))
      | _ -> Ok residual
    in
    Option.iter
      (fun path ->
        let reasons =
          match adapted with
          | Error reasons -> reasons
          | Ok residual -> (
              match Packed.compile_result residual (E.Residual.residual_view residual).term with
              | Error reasons -> reasons
              | Ok packed -> Packed.gpu_refusals ~fused:true ~fast:gpu.fast packed)
        in
        let reasons =
          List.map
            (fun (d : Diagnostic.t) ->
              {
                d with
                span = (match d.span with Some _ -> d.span | None -> Some view.term.form.span);
                message =
                  Printf.sprintf "%s (producer %s, instance %d, tuple [%s])" d.message
                    (String.concat "/" path) view.instance
                    (String.concat "," (List.map string_of_int view.iter));
              })
            reasons
        in
        match Hashtbl.find_opt outcomes path with
        | Some ((d : Diagnostic.t) :: _) when d.code <> "E_PACKED_PENDING" || reasons = [] -> ()
        | _ -> Hashtbl.replace outcomes path reasons)
      producer;
    Option.iter (fun callback -> callback producer (Result.value adapted ~default:residual)) observe
  in
  (* A large static map the checker accepts as an approximable producer may run on the device and come
     back as a 32-bit array: the SOP-input readback of the owner decision, taken within a tolerance. *)
  let bulk_approx residual =
    if gpu.backend = None then None
    else
      let value = E.Residual residual in
      if E.frame_dependent value || E.state_dependent value then None
      else
        match Packed.compile residual (E.Residual.residual_view residual).term with
        | None -> None
        | Some packed ->
            let _, site, _ = Packed.site packed in
            if
              List.exists
                (fun (path, form) ->
                  path = site && match form with W.Candidate _ -> true | W.Refused _ -> false)
                workspace.packed
            then
              match gpu.backend with
              | None -> None
              | Some backend -> (
                  let fast = gpu.fast
                  and count = Option.value ~default:0 (Packed.static_count packed) in
                  let placed =
                    count >= 1024
                    && (gpu.policy = Gpu.Qualification
                       || Option.fold ~none:false
                            ~some:(fun seconds ->
                              Float.is_finite seconds && seconds >= 0.
                              && seconds +. Cost.estimate Gpu_readback ~count
                                 < Cost.estimate (Cost.packed ~count) ~count)
                            (backend.cost ~fast packed ~count))
                  in
                  if (not placed) || Packed.gpu_refusals ~fused:true ~fast packed <> [] then None
                  else
                    match Packed.Host.prepare ~fast packed ~live:(Input.at_time 0.) with
                    | Error _ -> None
                    | Ok inputs -> (
                        let placed =
                          inputs.count >= 1024
                          && (gpu.policy = Gpu.Qualification
                             || Option.fold ~none:false
                                  ~some:(fun seconds ->
                                    Float.is_finite seconds && seconds >= 0.
                                    && seconds +. Cost.estimate Gpu_readback ~count:inputs.count
                                       < Cost.estimate (Cost.packed ~count:inputs.count)
                                           ~count:inputs.count)
                                  (backend.cost ~fast packed ~count:inputs.count))
                        in
                        if not placed then None
                        else
                          match backend.prepare ~fast packed with
                          | Error _ -> None
                          | Ok kernel -> (
                              match kernel.run inputs with
                              | Error _ -> None
                              | Ok output -> Result.to_option (kernel.readback output))))
            else None
  in
  let ( let* ) = Result.bind in
  let* evaluated =
    E.Residual.handle ~observe:observed ~bulk:static_map ~bulk_approx (fun () ->
        E.Host.static_with_requests ?record ?inputs workspace)
  in
  let packed_by_path = Hashtbl.create 64 in
  List.iter
    (fun (path, form) ->
      if not (Hashtbl.mem packed_by_path path) then Hashtbl.add packed_by_path path form)
    workspace.packed;
  let reasons path =
    match Hashtbl.find_opt packed_by_path path with
    | Some (W.Refused reasons) -> reasons
    | _ -> Option.value ~default:(pending path) (Hashtbl.find_opt outcomes path)
  in
  let approx, approx_reasons =
    List.fold_left
      (fun (accepted, rejected) (path, form) ->
        let refused =
          match form with
          | W.Refused reasons -> reasons
          | Candidate producers -> W.Paths.elements producers |> List.concat_map reasons
        in
        if refused = [] then (W.Paths.add path accepted, rejected)
        else (accepted, (path, refused) :: rejected))
      (W.Paths.empty, []) workspace.packed
  in
  Ok ({ workspace with approx; approx_reasons = List.rev approx_reasons }, evaluated)

type execution = {
  owner : id;
  sites : (int * W.path * int list) list;
  tier : tier;
  seconds : float;
}

module Profile = struct
  type t = { clock : unit -> float; executions : execution list Atomic.t }

  let create ~clock = { clock; executions = Atomic.make [] }
  let executions profile = Atomic.get profile.executions

  let rec record profile execution =
    let previous = executions profile in
    let rec keep n = function
      | [] -> []
      | _ when n = 0 -> []
      | old :: rest when old.sites = execution.sites && old.tier = execution.tier -> keep n rest
      | old :: rest -> old :: keep (n - 1) rest
    in
    (* ponytail: 512 recent groups, a linear scan once per group; index if
       profiling many small groups measurably costs a frame. *)
    let next = execution :: keep 511 previous in
    if not (Atomic.compare_and_set profile.executions previous next) then record profile execution
end

type sink = Display of string | Sop_input

module Executor = struct
  module Residuals = Hashtbl.Make (struct
    type t = E.residual

    let equal a b = a == b
    let hash = E.Residual.residual_id
  end)

  type candidate = Core of Core_exec.prepared | Packed of Packed.t | Reference

  module Candidate_key = Hashtbl.Make (struct
    type t = E.residual * bool

    let equal (a, strict_a) (b, strict_b) = a == b && strict_a = strict_b
    let hash (residual, strict) = Hashtbl.hash (E.Residual.residual_id residual, strict)
  end)

  let direct_region ?trace ?through_exact ?(readback = false) ~count_source value =
    let rec find seen residual (term : W.term) =
      if
        List.exists
          (fun (seen_residual, seen_term) -> seen_residual == residual && seen_term == term)
          seen
      then None
      else
        let seen = (residual, term) :: seen in
        match term.node with
        | Expanded { body; _ } -> find seen residual body
        | Bypass call -> Option.bind (W.bypass_input call) (find seen residual)
        | Op { op = "exact"; args = [ ("value", value) ] } when readback ->
            Option.map (fun (packed, _) -> (packed, true)) (find seen residual value)
        | Hof ((`Map | `Reduce), _) | Loop _ | Op { op = "array/sum"; _ } ->
            Option.map
              (fun packed -> (packed, false))
              (Packed.compile ?trace ?through_exact ~count_source residual term)
        | _ -> None
    in
    match value with
    | E.Residual residual -> find [] residual (E.Residual.residual_view residual).term
    | _ -> None

  type program = {
    value : E.value;
    candidates : candidate Candidate_key.t;
    candidates_lock : Mutex.t;
    root_core : (bool * Core_exec.prepared) option Atomic.t;
    packed_region : Packed.t option;
    packed_root : (Packed.t * bool) option;
    trace : ((Packed.t, Diagnostic.t list) result -> unit) option;
    count_source : Packed.count_source;
    profile : Profile.t option;
    approx : W.Paths.t;
    sink : sink option;
    mutable gpu_kernel : (Gpu.backend * bool * Packed.t * Gpu.kernel) option;
    record_region : (string list * Packed.t) option Lazy.t;
  }

  let compile ?trace ?profile ?(strict = false) ?(approx = W.Paths.empty) ?sink
      ?(count_source = fun _ _ -> None) v =
    let rec unsupported_source residual (term : W.term) =
      let children terms = List.find_map (unsupported_source residual) terms in
      match term.node with
      | Hof ((`Map | `Reduce), _) | Loop _ | Op { op = "array/sum"; _ } -> (
          match Packed.compile_result ?trace ~count_source residual term with
          | Error _ -> Some term.form.span
          | Ok packed -> unsupported_packed_source packed)
      | Ref_binding (name, fields) ->
          let captured =
            List.fold_left
              (fun value field ->
                Option.bind value (function
                  | E.Record fields | Struct (_, _, fields) -> List.assoc_opt field fields
                  | _ -> None))
              (List.assoc_opt name (E.Residual.residual_view residual).bindings)
              fields
          in
          Option.bind captured (function
            | E.Residual residual ->
                unsupported_source residual (E.Residual.residual_view residual).term
            | _ -> None)
      | Op { args; _ } | Record args -> children (List.map snd args)
      | Get (term, _) | Expanded { body = term; _ } -> unsupported_source residual term
      | Bypass call -> Option.bind (W.bypass_input call) (unsupported_source residual)
      | Let (bindings, result) -> children (result :: List.map snd bindings)
      | With_error (a, b) -> children [ a; b ]
      | Error_rows { rows; mapper; _ } -> children [ rows; mapper ]
      | If (a, b, c) -> children [ a; b; c ]
      | Cond (arms, default) -> children (default :: List.concat_map (fun (a, b) -> [ a; b ]) arms)
      | Case (value, arms, default) -> children (value :: default :: List.map snd arms)
      | Vec terms | List_lit terms | Str terms | List_op (_, terms) | Hof (_, terms) ->
          children terms
      | Assoc (record, updates) -> children (record :: List.map snd updates)
      | Call_fn { args; body; _ } -> children (args @ Option.to_list body)
      | Graph_ref { inputs; _ } | Call { args = inputs; _ } -> children (List.map snd inputs)
      | Fn { body; capture; _ } -> children (body :: Option.to_list capture)
      | State { init; step; _ } -> children [ init; step ]
      | _ -> None
    and unsupported_packed_source packed =
      let packed : Core.Region.t = packed in
      match
        Array.find_map (fun (residual, term) -> unsupported_source residual term) packed.sources
      with
      | Some _ as error -> error
      | None -> Array.find_map unsupported_captured packed.uniforms
    and unsupported_captured = function
      | E.Residual residual -> unsupported_source residual (E.Residual.residual_view residual).term
      | E.List values -> Array.find_map unsupported_captured values
      | E.Record fields | E.Struct (_, _, fields) | E.Source (_, _, fields) ->
          List.find_map (fun (_, value) -> unsupported_captured value) fields
      | _ -> None
    in
    let validate_residual residual =
      let view = E.Residual.residual_view residual in
      (* Per-element Vec3 placeholders are runtime uniforms, not nested kernels. *)
      let element_capture =
        match view.term.node with
        | Ref_binding (name, [])
          when view.term.ty = Ty.Vec3 && String.starts_with ~prefix:"$elem:" name ->
            true
        | _ -> false
      in
      if view.previous || element_capture || E.state_dependent (E.Residual residual) then Ok ()
      else
        match Core_lower.lower ?trace ~strict:true residual with
        | Ok core when Array.find_map unsupported_packed_source core.Core.regions = None -> Ok ()
        | Ok _ ->
            Error
              (Diagnostic.error ~span:view.term.form.span ~code:"E_KERNEL_FORM"
                 "Numeric node bodies must compile to packed kernels.")
        | Error _ ->
            Error
              (Diagnostic.error ~span:view.term.form.span ~code:"E_KERNEL_FORM"
                 "Numeric node bodies must compile to packed kernels.")
    in
    (* a residual shared by many (a matrix several bindings read) is validated once *)
    let validated = Hashtbl.create 16 in
    let rec validate_value = function
      | E.Residual residual when Hashtbl.mem validated (E.Residual.residual_id residual) -> Ok ()
      | E.Residual residual ->
          Hashtbl.replace validated (E.Residual.residual_id residual) ();
          Result.bind (validate_residual residual) (fun () ->
              validate_fields (E.Residual.residual_view residual).bindings)
      | E.List values -> validate_array values 0
      | E.Record fields | E.Struct (_, _, fields) | E.Source (_, _, fields) ->
          validate_fields fields
      | E.Fn fn -> validate_fields (E.Residual.function_bindings fn)
      | _ -> Ok ()
    and validate_array values i =
      if i = Array.length values then Ok ()
      else Result.bind (validate_value values.(i)) (fun () -> validate_array values (i + 1))
    and validate_fields = function
      | [] -> Ok ()
      | (_, value) :: rest -> Result.bind (validate_value value) (fun () -> validate_fields rest)
    in
    let checked = if strict then validate_value v else Ok () in
    Result.map
      (fun () ->
        let packed_region = Option.map fst (direct_region ?trace ~count_source v) in
        let packed_root =
          match packed_region with
          | Some packed -> Some (packed, false)
          | None -> direct_region ?trace ~readback:true ~count_source v
        in
        {
          value = v;
          candidates = Candidate_key.create 32;
          candidates_lock = Mutex.create ();
          root_core = Atomic.make None;
          packed_region;
          packed_root;
          trace;
          count_source;
          profile;
          approx;
          sink;
          gpu_kernel = None;
          record_region =
            lazy
              (match v with
              | E.Record fields ->
                  let regions =
                    List.filter_map
                      (fun (name, value) ->
                        Option.bind
                          (direct_region ?trace ~through_exact:true ~readback:true ~count_source
                             value) (fun (packed, _) ->
                            if Packed.elementwise packed then Some (name, packed) else None))
                      fields
                  in
                  if List.length regions <> List.length fields then None
                  else
                    Option.map
                      (fun merged -> (List.map fst regions, merged))
                      (Option.map Core_fuse.compact_sources
                         (Core_fuse.merge (List.map snd regions)))
              | _ -> None);
        })
      checked

  let packed_region p = p.packed_region
  let root_packed p = p.packed_root
  let packed_root p = Option.map fst (root_packed p)
  let root_is_packed_or_readback p = Option.is_some (root_packed p)

  let fresh_csr_fields p =
    match p.value with
    | E.Record fields ->
        List.filter_map
          (fun (name, value) ->
            if List.mem name [ "offsets"; "values" ] then
              Option.bind
                (direct_region ?trace:p.trace ~readback:true ~count_source:p.count_source value)
                (fun (packed, _) -> if Packed.elementwise packed then Some name else None)
            else None)
          fields
    | _ -> []

  (* One timing per packed group, recorded when the group runs. *)
  let measure ?(ran = ignore) profile =
    Option.map
      (fun profile ->
        ( profile.Profile.clock,
          fun packed ~seconds ~reference ->
            ran ();
            let sites = Packed.provenance packed in
            let _, path, iter = Packed.site packed in
            Profile.record profile
              {
                owner = (path, iter);
                sites;
                seconds;
                tier = (if reference then Interp else Cpu_kernel);
              } ))
      profile

  let force_impl ?lower_counter ?walker_counter ?state ?elems ?resolve ?(reference = false)
      ?(strict = false) program ~live =
    let fast =
      if state <> None || elems <> None || resolve <> None || reference || program.profile <> None
      then None
      else
        match Atomic.get program.root_core with
        | Some (cached_strict, prepared) when cached_strict = strict -> Some prepared
        | _ -> None
    in
    match fast with
    | Some prepared when E.is_live program.value -> (
        match Core_exec.execute prepared ~live with
        | Ok _ as result -> result
        | Error _ when not strict ->
            let count_walk () = Option.iter incr walker_counter in
            E.Host.force_reference_observed ~on_walk:count_walk program.value ~live
        | Error _ as error -> error)
    | _ ->
        let result, packed_ran, fell_back, started =
          if not (E.is_live program.value) then
            let started = Option.map (fun profile -> profile.Profile.clock ()) program.profile in
            (Ok program.value, false, reference, started)
          else begin
            let packed_ran = ref false in
            let measure = measure ~ran:(fun () -> packed_ran := true) program.profile in
            let state = match state with Some state -> state | None -> E.create_state () in
            let fell_back = ref reference in
            let count_walk () = Option.iter incr walker_counter in
            let fallback () =
              fell_back := true;
              E.Host.force_reference_observed ~state ?elems ?resolve ~on_walk:count_walk
                program.value ~live
            in
            let force_with_packed value ~live =
              let forced_one = ref None and forced_many = ref None in
              let find_forced residual =
                match (!forced_many, !forced_one) with
                | Some table, _ -> Residuals.find_opt table residual
                | None, Some (old, value) when old == residual -> Some value
                | None, _ -> None
              in
              let remember_forced residual value =
                match (!forced_many, !forced_one) with
                | Some table, _ -> Residuals.replace table residual value
                | None, None -> forced_one := Some (residual, value)
                | None, Some (old, old_value) ->
                    let table = Residuals.create 16 in
                    Residuals.replace table old old_value;
                    Residuals.replace table residual value;
                    forced_many := Some table
              in
              let operation_one = ref None and operation_many = ref None in
              let operation_candidates_lock = Mutex.create () in
              let dispatch_nested = ref None in
              let dispatch residual term =
                match !dispatch_nested with
                | Some dispatch -> dispatch residual term
                | None ->
                    Error
                      (Diagnostic.error ~code:"E_CORE_FORM"
                         "Residual dispatcher is not initialized.")
              in
              let candidate r =
                let key = (r, strict) in
                Mutex.lock operation_candidates_lock;
                Fun.protect
                  ~finally:(fun () -> Mutex.unlock operation_candidates_lock)
                  (fun () ->
                    let cached =
                      match (!operation_many, !operation_one) with
                      | Some table, _ -> Candidate_key.find_opt table key
                      | None, Some ((old, old_strict), candidate)
                        when old == r && old_strict = strict ->
                          Some candidate
                      | None, _ -> None
                    in
                    match cached with
                    | Some candidate -> candidate
                    | None ->
                        Mutex.lock program.candidates_lock;
                        let result =
                          Fun.protect
                            ~finally:(fun () -> Mutex.unlock program.candidates_lock)
                            (fun () ->
                              match Candidate_key.find_opt program.candidates key with
                              | Some candidate -> candidate
                              | None ->
                                  Option.iter (fun count -> incr count) lower_counter;
                                  let candidate =
                                    try
                                      match Core_lower.lower ?trace:program.trace ~strict r with
                                      | Ok core ->
                                          (* Placement declines before execution, while state is untouched. *)
                                          let declined =
                                            (not strict)
                                            && Array.exists
                                                 (fun (region : Core.Region.t) ->
                                                   Option.fold ~none:false
                                                     ~some:(fun count ->
                                                       Cost.packed ~count <> Cpu_kernel)
                                                     region.count)
                                                 core.Core.regions
                                          in
                                          if declined then
                                            match
                                              Packed.compile_template ?trace:program.trace
                                                ~count_source:program.count_source r
                                                (E.Residual.residual_view r).term
                                            with
                                            | Some packed -> Packed packed
                                            | None -> Reference
                                          else Core (Core_exec.prepare core)
                                      | Error _ -> (
                                          match
                                            Packed.compile_template ?trace:program.trace
                                              ~count_source:program.count_source r
                                              (E.Residual.residual_view r).term
                                          with
                                          | Some packed -> Packed packed
                                          | None -> Reference)
                                    with
                                    | Diagnostic.Error _ | Invalid_argument _ | Not_found
                                    | Failure _
                                    ->
                                      Reference
                                  in
                                  if Candidate_key.length program.candidates >= 128 then
                                    Candidate_key.clear program.candidates;
                                  Candidate_key.replace program.candidates key candidate;
                                  candidate)
                        in
                        (match (result, program.value) with
                        | Core prepared, E.Residual root when root == r && Core_exec.direct prepared
                          ->
                            Atomic.set program.root_core (Some (strict, prepared))
                        | _ -> ());
                        (match (!operation_many, !operation_one) with
                        | Some table, _ -> Candidate_key.replace table key result
                        | None, None -> operation_one := Some (key, result)
                        | None, Some (old_key, old_result) ->
                            let table = Candidate_key.create 32 in
                            Candidate_key.replace table old_key old_result;
                            Candidate_key.replace table key result;
                            operation_many := Some table);
                        result)
              in
              let execute r live =
                match candidate r with
                | Core p when not (Core_exec.has_regions p) ->
                    Some
                      (Core_exec.execute ~root:r ~strict ~rerun_on_error:false ~on_walk:count_walk
                         ?resolve p ~live)
                | Core p ->
                    Some
                      (Core_exec.execute ~root:r ~strict ~rerun_on_error:false ~on_walk:count_walk
                         ?resolve
                         ~force:(fun packed ->
                           Packed.force ~dispatch ~strict ~state ?elems ?resolve ?measure
                             ~on_walk:count_walk packed ~live)
                         p ~live)
                | Packed p
                  when strict
                       || not
                            (Option.fold ~none:false
                               ~some:(fun count -> Cost.packed ~count <> Cpu_kernel)
                               (Packed.static_count p)) ->
                    Some
                      (Packed.force ~dispatch ~strict ~state ?elems ?resolve ?measure
                         ~on_walk:count_walk p ~live)
                | Packed _ | Reference -> (
                    match (strict, (E.Residual.residual_view r).term.node) with
                    | true, (W.Hof ((`Map | `Reduce), _) | W.Loop _ | W.Op { op = "array/sum"; _ })
                      ->
                        Some
                          (Error
                             (Diagnostic.error ~code:"E_KERNEL_FORM"
                                "Numeric node bodies must compile to packed kernels."))
                    | _ -> None)
              in
              let rec walk value =
                match value with
                | E.Residual r -> (
                    match find_forced r with
                    | Some value -> Ok value
                    | None ->
                        let result =
                          Result.bind (E.Residual.bind_live ~state ?elems ?resolve r ~live)
                            (fun () ->
                              let restore = E.Residual.checkpoint r in
                              match execute r live with
                              | Some (Ok value) -> Ok value
                              | Some (Error d) ->
                                  if strict then Error d
                                  else begin
                                    restore ();
                                    fell_back := true;
                                    E.Host.force_reference_observed ~state ?elems ?resolve
                                      ~on_walk:count_walk (E.Residual r) ~live
                                  end
                              | None ->
                                  if strict && not (E.state_dependent (E.Residual r)) then begin
                                    restore ();
                                    Error
                                      (Diagnostic.error ~code:"E_CORE_FORM"
                                         "This value needs a compiled executor.")
                                  end
                                  else begin
                                    restore ();
                                    fell_back := true;
                                    E.Host.force_reference_observed ~state ?elems ?resolve
                                      ~on_walk:count_walk (E.Residual r) ~live
                                  end)
                        in
                        Result.bind result (fun value ->
                            remember_forced r value;
                            walk value))
                | E.Source _ ->
                    Result.bind
                      (E.Host.force_reference_observed ~state ?elems ?resolve ~on_walk:count_walk
                         value ~live)
                      walk
                | E.Struct (_, Flow.Ty.Array _, _) ->
                    Result.bind
                      (E.Host.force_reference_observed ~state ?elems ?resolve ~on_walk:count_walk
                         value ~live)
                      walk
                | E.List values ->
                    let out = Array.make (Array.length values) E.No_geo in
                    let rec loop i =
                      if i = Array.length values then Ok (E.List out)
                      else
                        Result.bind
                          (walk values.(i))
                          (fun value ->
                            out.(i) <- value;
                            loop (i + 1))
                    in
                    loop 0
                | E.Record fields -> Result.map (fun fields -> E.Record fields) (map_result fields)
                | E.Struct (name, ty, fields) ->
                    Result.map (fun fields -> E.Struct (name, ty, fields)) (map_result fields)
                | value -> Ok value
              and map_result fields =
                let rec loop acc = function
                  | [] -> Ok (List.rev acc)
                  | (name, value) :: rest ->
                      Result.bind (walk value) (fun value -> loop ((name, value) :: acc) rest)
                in
                loop [] fields
              in
              dispatch_nested :=
                Some
                  (fun residual term ->
                    let view = E.Residual.residual_view residual in
                    let residual =
                      if view.term == term then residual
                      else E.Residual.with_bindings residual ~term view.bindings
                    in
                    walk (E.Residual residual));
              E.transaction state (fun () -> walk value)
            in
            let run () = if reference then fallback () else force_with_packed program.value ~live in
            let started = Option.map (fun profile -> profile.Profile.clock ()) program.profile in
            let result = run () in
            (result, !packed_ran, !fell_back, started)
          end
        in
        Option.iter
          (fun profile ->
            if not packed_ran then begin
              let id, sites =
                match program.value with
                | E.Residual residual ->
                    let view = E.Residual.residual_view residual in
                    let sites =
                      match packed_root program with
                      | Some packed -> Packed.provenance packed
                      | None -> [ (view.instance, view.site, view.iter) ]
                    in
                    ((view.site, view.iter), sites)
                | _ -> (([ "@value" ], []), [])
              in
              let tier =
                if fell_back then Interp
                else
                  match packed_root program with
                  | Some packed ->
                      Cost.packed ~count:(Option.value ~default:1 (Packed.static_count packed))
                  | None -> Core
              in
              let path =
                match fst id with
                | namespace :: rest when String.starts_with ~prefix:"@instance:" namespace -> rest
                | path -> path
              in
              Profile.record profile
                {
                  owner = (path, snd id);
                  sites;
                  tier;
                  seconds = max 0. (profile.clock () -. Option.get started);
                }
            end)
          program.profile;
        result

  let force ?state ?elems ?resolve ?reference ?strict program ~live =
    force_impl ?state ?elems ?resolve ?reference ?strict program ~live

  module Private = struct
    let force_with_lower_attempts ?state ?elems ?resolve ?reference ?strict program ~live =
      let count = ref 0 in
      let result =
        force_impl ~lower_counter:count ?state ?elems ?resolve ?reference ?strict program ~live
      in
      (result, !count)

    let force_with_walker_counts ?state ?elems ?resolve ?reference ?strict program ~live =
      let lower_attempts = ref 0 and walker_evaluations = ref 0 in
      let result =
        force_impl ~lower_counter:lower_attempts ~walker_counter:walker_evaluations ?state ?elems
          ?resolve ?reference ?strict program ~live
      in
      (result, !lower_attempts, !walker_evaluations)
  end

  let force_into_cpu ?state ?elems ?resolve ?(strict = false) ~column_source ~count ~outputs program
      ~live =
    match packed_root program with
    | Some packed when Packed.elementwise packed ->
        Result.map
          (fun () -> Some ())
          (Packed.force_into ?state ?elems ?resolve ?measure:(measure program.profile) ~strict
             ~column_source ~count ~outputs packed ~live)
    | _ -> Ok None

  type displayed = Cpu of E.value | Gpu of Gpu.value

  (* A packed region on the GPU when the backend takes it and measures it cheaper. [cached] and
     [store] are the owner's one-kernel cache; [display] says a GPU array may leave the executor. *)
  let display_packed ?state ?resolve ?source ~gpu ~approx ~display ~profile ~cached ~store ~readback
      packed ~live =
    match gpu.Gpu.backend with
    | None -> Ok None
    | Some backend ->
        let cpu () = Ok None in
        let fast = gpu.fast in
        let _, site, _ = Packed.site packed in
        (* fast precision: a displayed region needs no qualified path, its only use is the screen *)
        if
          (not ((fast && display) || W.Paths.mem site approx))
          || Packed.gpu_refusals ~fused:true ~fast packed <> []
        then cpu ()
        else
          let cheaper count =
            count >= 1024
            && (gpu.policy = Gpu.Qualification
               || Option.fold ~none:false
                    ~some:(fun seconds ->
                      Float.is_finite seconds && seconds >= 0.
                      && (seconds
                         +.
                         if readback then Cost.estimate Gpu_readback ~count
                         else Cost.display_sink Gpu ~count)
                         < Cost.estimate (Cost.packed ~count) ~count
                           +. if readback then 0. else Cost.display_sink Cpu_kernel ~count)
                    (Option.bind gpu.backend (fun backend -> backend.Gpu.cost ~fast packed ~count))
               )
          in
          (* Static cardinality lets unmeasured placement decline before allocating
       producer inputs, especially the million-element exact SOP value lane. *)
          if
            Option.fold ~none:false
              ~some:(fun count -> not (cheaper count))
              (Packed.static_count packed)
          then cpu ()
          else
            let ( let* ) = Result.bind in
            let* inputs = Packed.Host.prepare ~fast ?state ?resolve ?source packed ~live in
            if not (cheaper inputs.count) then cpu ()
            else
              let* () =
                if display || readback then Ok ()
                else
                  Error
                    (Diagnostic.error ~code:"E_APPROX_SINK"
                       "GPU arrays require a display sink or (exact x) before CPU, SOP, state, \
                        export or cache use.")
              in
              let profile ?(seconds = fun _ -> None) tier run =
                let start = Option.map (fun p -> p.Profile.clock ()) profile in
                let result = run () in
                let elapsed = match result with Ok value -> seconds value | Error _ -> None in
                Option.iter
                  (fun p ->
                    let _, path, iter = Packed.site packed in
                    Profile.record p
                      {
                        owner = (path, iter);
                        sites = Packed.provenance packed;
                        tier;
                        seconds =
                          (match elapsed with
                          | Some value when Float.is_finite value && value >= 0. -> value
                          | _ -> max 0. (p.clock () -. Option.get start));
                      })
                  profile;
                result
              in
              let* kernel =
                match cached () with
                | Some (previous, previous_fast, p, kernel)
                  when previous == backend && previous_fast = fast && p == packed ->
                    Ok kernel
                | _ ->
                    profile Gpu_compile (fun () ->
                        Result.map
                          (fun kernel ->
                            store (backend, fast, packed, kernel);
                            kernel)
                          (backend.prepare ~fast packed))
              in
              let* output =
                profile
                  ~seconds:(fun output -> output.Gpu.gpu_seconds)
                  Gpu
                  (fun () -> kernel.run inputs)
              in
              if readback then
                profile Gpu_readback (fun () ->
                    Result.map (fun value -> Some (Cpu value)) (kernel.readback output))
              else Ok (Some (Gpu output))

  let try_display ?state ?resolve ?(reference = false) ?(gpu = Gpu.default) program ~live =
    let cpu () = Ok None in
    if reference then cpu ()
    else
      match gpu.backend with
      | None -> cpu ()
      | Some _ -> (
          match root_packed program with
          | None -> cpu ()
          | Some (packed, readback) ->
              display_packed ?state ?resolve ~gpu ~approx:program.approx
                ~display:(match program.sink with Some (Display _) -> true | _ -> false)
                ~profile:program.profile
                ~cached:(fun () -> program.gpu_kernel)
                ~store:(fun k -> program.gpu_kernel <- Some k)
                ~readback:(readback || program.sink = Some Sop_input)
                packed ~live)

  (* A region of a draw program offered to the GPU as a mark attribute: a closure owns the kernels
     of at most 64 regions. [None] leaves the region to the CPU. *)
  let region_display ?(gpu = Gpu.default) ?profile ?(approx = W.Paths.empty) () =
    let kernels = ref [] in
    let module Ranges = Lru.Make (Int) in
    let ranges = Ranges.create ~byte_capacity:(64 * 1024 * 1024) 16 in
    let rec source residual (term : W.term) =
      match term.node with
      | Ref_binding (name, []) -> (
          match List.assoc_opt name (E.Residual.residual_view residual).bindings with
          | Some (E.Residual r) -> source r (E.Residual.residual_view r).term
          | _ -> None)
      | Op
          { op = "array/range"; args = [ ("count", { node = Lit (Param.Int_value count); _ }) ]; _ }
        when term.ty = Ty.Array Ty.Float && count >= 0 && count <= 64 * 1024 * 1024 / 8 -> (
          match Ranges.find_opt ranges count with
          | Some value -> Some value
          | None -> (
              match E.Residual.eval_term residual term ~live:(Input.at_time 0.) with
              | Ok value ->
                  Ranges.add ranges ~bytes:(count * 8) count value;
                  Some value
              | Error d -> V.raise_diagnostic d))
      | _ -> None
    in
    fun ?state ?resolve packed ~live ->
      match gpu.backend with
      | None -> Ok None
      | Some _ ->
          (* a draw binds a fresh [packed] each frame over its site's code: kernels are kept by the code *)
          let code p = (Packed.Host.view p).code in
          let find () =
            List.find_map
              (fun (fast, b, p, kernel) ->
                if fast = gpu.fast && code p == code packed then Some (b, fast, packed, kernel)
                else None)
              !kernels
          in
          let result =
            display_packed ?state ?resolve ~source ~gpu ~approx ~display:true ~profile ~cached:find
              ~store:(fun (b, _, p, kernel) ->
                kernels :=
                  (gpu.fast, b, p, kernel)
                  :: List.take 63
                       (List.filter
                          (fun (f, _, q, _) -> f <> gpu.fast || code q != code p)
                          !kernels))
              ~readback:false packed ~live
          in
          Result.map (Option.map (function Gpu v -> v | Cpu _ -> assert false)) result

  (* In fast precision a region a drawing computes with (a fold's maps, not a mark attribute) may run
     on the device and come back as a 32-bit array. Kernels are kept by the region's form, not its
     identity: a fold binds a new program each pass with the same code. At most 16 forms.
     ponytail: a fixed 4,096-element threshold, not the measured cost; use [backend.cost] if small
     regions are misplaced. *)
  let region_readback ?(gpu = Gpu.default) () =
    let kernels = ref [] in
    fun packed ~live ->
      let ( let* ) = Result.bind in
      if not gpu.fast then Ok None
      else
        match gpu.backend with
        | None -> Ok None
        | Some backend ->
            if Packed.gpu_refusals ~fused:true ~fast:gpu.fast packed <> [] then Ok None
            else
              let* inputs = Packed.Host.prepare ~fast:gpu.fast packed ~live in
              if inputs.count < 4096 then Ok None
              else
                let view = Packed.Host.view packed in
                let key = (view.code, view.widths, view.outputs) in
                let* kernel =
                  match
                    List.find_opt
                      (fun (f, k, b, _) -> f = gpu.fast && b == backend && k = key)
                      !kernels
                  with
                  | Some (_, _, _, kernel) -> Ok kernel
                  | None ->
                      let* kernel = backend.prepare ~fast:gpu.fast packed in
                      kernels := (gpu.fast, key, backend, kernel) :: List.take 15 !kernels;
                      Ok kernel
                in
                let* output = kernel.run inputs in
                (* the device carries integers as floats: an Int map comes back as the Int array it is *)
                Result.map
                  (fun value ->
                    Some
                      (match (value, packed.term.ty) with
                      | E.Float_array xs, Ty.Array Ty.Int -> E.Int_array (Array.map int_of_float xs)
                      | value, _ -> value))
                  (kernel.readback output)

  (* A Sop_input write in a cook may take its region from the device: pack the inputs here, dispatch
     and read back through the service, then copy the planes the writer asked for. The CPU region
     answers when the device declines, is unmeasured, or fails. *)
  let force_into ?state ?elems ?resolve ?(gpu = Gpu.default) ?(strict = false) ~column_source ~count
      ~outputs program ~live =
    let cpu () =
      force_into_cpu ?state ?elems ?resolve ~strict ~column_source ~count ~outputs program ~live
    in
    match (program.sink, Array.length outputs, gpu.backend, packed_root program) with
    | Some Sop_input, 1, Some _, Some packed when Packed.elementwise packed -> (
        match
          display_packed ?state ?resolve ~gpu ~approx:program.approx ~display:false
            ~profile:program.profile
            ~cached:(fun () -> program.gpu_kernel)
            ~store:(fun k -> program.gpu_kernel <- Some k)
            ~readback:true packed ~live
        with
        | Ok (Some (Cpu value)) ->
            let data, width =
              match value with
              | E.Float_array d -> (d, 1)
              | Vec2_array d -> (d, 2)
              | Vec3_array d -> (d, 3)
              | Vec4_array d -> (d, 4)
              | _ -> ([||], 0)
            in
            if width = 0 || width <> Array.length outputs.(0) || Array.length data <> count * width
            then cpu ()
            else begin
              Array.iteri
                (fun component plane ->
                  Option.iter
                    (fun plane ->
                      for i = 0 to count - 1 do
                        plane.(i) <- data.((i * width) + component)
                      done)
                    plane)
                outputs.(0);
              Ok (Some ())
            end
        | Ok _ | Error _ -> cpu ())
    | _ -> cpu ()

  (* A fused SOP write's record ({!Cost.sop_device}): its elementwise fields over one count are one
     region, one dispatch and a readback per output, by field name. [None] leaves the record to
     the CPU: the device declines or fails, or the cost model keeps it there. *)
  let record_on_device ?state ?resolve ?resident ?(gpu = Gpu.default) program ~live =
    match (gpu.backend, Lazy.force program.record_region) with
    | Some backend, Some (names, merged)
      when gpu.fast && Packed.gpu_refusals ~fused:true ~fast:true merged = [] -> (
        let ( let* ) = Result.bind in
        let device =
          (* a source another write keeps on this device is bound there, not read back *)
          let* inputs =
            Packed.Host.prepare ~fast:true ~index:true ?state ?resolve ?resident merged ~live
          in
          if not (Cost.sop_device gpu.policy merged ~count:inputs.count) then Ok None
          else
            let* kernel =
              match program.gpu_kernel with
              | Some (b, true, p, kernel) when b == backend && p == merged -> Ok kernel
              | _ ->
                  Result.map
                    (fun kernel ->
                      program.gpu_kernel <- Some (backend, true, merged, kernel);
                      kernel)
                    (backend.prepare ~fast:true merged)
            in
            let* output = kernel.run inputs in
            let rec read i = function
              | [] -> Ok []
              | name :: rest ->
                  let* value =
                    kernel.planes
                      { output with output = i; width = Array.length merged.outputs.(i) }
                  in
                  Result.map (fun values -> (name, value) :: values) (read (i + 1) rest)
            in
            Result.map Option.some (read 0 names)
        in
        match device with Ok _ as placed -> placed | Error _ -> Ok None)
    | _ -> Ok None

  let force_display ?state ?(reference = false) ?(gpu = Gpu.default) program ~live =
    Result.bind (try_display ?state ~reference ~gpu program ~live) (function
      | Some value -> Ok value
      | None -> Result.map (fun value -> Cpu value) (force ?state ~reference program ~live))
end

(* The device side of a static packed map: placed by the same measured cost as any readback, then
   read back once, so the walker receives an ordinary array (taken within a tolerance). *)
