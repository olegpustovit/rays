(* HOW TO PORT A SOP NODE (the whole verification is this directory).
   Porting a node adds exactly two files and edits no shared file:
     lib/flow_sop/nodes/<key>.rays   the defnode
     lib/flow_sop/port_tests/port_<key>.ml   one registration, see port_points.ml
   The dune file picks up every .ml here; the toplevel `Port_gate.register` runs at start.

   Registration: { key; native; inputs; cases; ulps; bench }
     native  the OCaml oracle factory, e.g. Sop.Nodes.Grid.factory
     inputs  (label, geometries) fixtures; one geometry per node input.
             no_input | standard_inputs (empty, single point, 4225-point quad grid, mixed
             polygon+polyline with point/vertex/primitive/edge groups and attributes of
             every owner/type) | your own list (use `fixture_*` helpers)
     cases   (label, parameter changes); "defaults" (no changes) is always added.
             Cover every parameter and every branch; changes are rendered as keywords.
     ulps    float tolerance against native (default 4; a larger one needs a stated reason)
     bench   Some (fun () -> geometries, changes) for the 1,000,000-element timing, or None

   Run (repo root, or `dune build --root .` in a worktree):
     dune build ./lib/flow_sop/port_tests/main.exe        then
     _build/default/lib/flow_sop/port_tests/main.exe KEY            quick gate:
                                                                    all cases on the last fixture,
                                                                    defaults on the others
     _build/default/lib/flow_sop/port_tests/main.exe KEY --full     every case on every fixture
     _build/default/lib/flow_sop/port_tests/main.exe KEY --bench    timing, fails above 2.0x
     _build/default/lib/flow_sop/port_tests/main.exe                every accepted node
     _build/default/lib/flow_sop/port_tests/main.exe --capture-goldens DIR
                                                                    Lisp cases plus portable input files
     dune build @lib/flow_sop/port_tests/ports                      same, as a dune alias
   Checks, for each fixture x case:
     oracle        Lisp vs native cook: integer data/groups/names exactly, floats within
                   ulps (Rdk_test_support.close_geometry); a native error must be a Lisp
                   error with the same code; a native refusal at construction must be
                   refused by the checker/lowering.
     self          Lisp bytes (geometry_bytes) equal at domains 1/4 x grains 1/97/4096 and
                   between the reference interpreter and the CPU kernel tier.
   Once per node:
     parameters    Catalog.descriptor and Node.parameter_fields equal native's (every case)
     cache         parameter_key equals native's; a changed parameter misses a session
                   cache, an unchanged one hits
     refusals      every float parameter refuses nan/inf with native's exact error *)
open Rdk
open Rdk_test_support
module N = Sop.Node
module P = Param

type changes = (string * P.value) list
type t = {
  key : string;
  native : Sop.Edit_graph.factory;
  inputs : (string * Geometry.t list) list;
  cases : (string * changes) list;
  ulps : int;
  bench : (unit -> Geometry.t list * changes) option;
}

let registry : t list ref = ref []
let register node = registry := node :: !registry
(* A node with image slots lists its images per fixture label ("bench" for --bench); they follow
   the geometries as trailing inputs on both sides. *)
let image_registry : (string * (string -> Sop.Image.t list)) list ref = ref []
let register_images key images = image_registry := (key, images) :: !image_registry
let registered () = List.rev !registry

(* ---- fixtures ---- *)
let points count = Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
  for i = first to last - 1 do x.(i) <- float i *. 0.125; y.(i) <- -0.; z.(i) <- 0.25 done)
let polygon_polyline () =
  let topology = Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
    ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon; Open_polyline|] |> get_string_ok in
  Geometry.create ~positions:(Geometry.positions (points 6)) ~topology () |> get_string_ok
let grid () = Plane_generators.grid ~connectivity:Grid_quads ~columns:64 ~rows:64 ~size:8. () |> get_ok
(* Groups "a", "dest_a", "old_x", a literal-glob name and a unicode name, on points,
   vertices, primitives and edges (ordered, reversed), plus a detail text attribute. *)
let fixture_groups geometry =
  let names = ["a", 0; "dest_a", 1; "old_x", 2; "literal *?[]^\\", 0; "\195\169\231\140\171", 1] in
  let put owner name phase geometry =
    let count = match owner with Group.Point -> Geometry.point_count geometry
      | Vertex -> Geometry.vertex_count geometry | Primitive -> Geometry.primitive_count geometry in
    let indices = List.filter (fun i -> i mod 3 = phase) (List.init count Fun.id) |> List.rev |> Array.of_list in
    Geometry.with_group (Group.ordered ~owner ~name ~length:count indices |> get_string_ok) geometry |> get_string_ok in
  let geometry = List.fold_left (fun g (name, phase) ->
    g |> put Group.Vertex name phase |> put Group.Point name phase |> put Group.Primitive name phase) geometry names in
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  let geometry = List.fold_left (fun g (name, phase) -> Geometry.with_edge_group
    (Edge_group.init ~topology ~index ~name (fun i -> i mod 3 = phase)) g |> get_string_ok) geometry names in
  Geometry.with_attribute (Attribute.create_owned ~owner:Attribute.Detail ~name:"untouched"
    (Attribute.Text [|"UTF8"|]) |> get_string_ok) geometry |> get_string_ok
(* One attribute of every storage type on every owner. *)
let fixture_attributes geometry =
  List.fold_left (fun geometry owner ->
    let n = match owner with Attribute.Point -> Geometry.point_count geometry
      | Vertex -> Geometry.vertex_count geometry | Primitive -> Geometry.primitive_count geometry | Detail -> 1 in
    let values = Array.init n (fun i -> if i mod 2 = 0 then -0. else float i *. 0.125)
    and integers = Array.init n (fun i -> if i mod 2 = 0 then max_int else min_int) in
    let plane () = Array.copy values in
    List.fold_left (fun geometry (name, storage) -> Geometry.with_attribute
      (Attribute.create_owned ~owner ~name storage |> get_string_ok) geometry |> get_string_ok) geometry
      ["float", Attribute.Float values; "integer", Int integers; "text", Text (Array.make n "\195\169\231\140\171");
       "vec2", Float2 (Packed.Float2.of_owned ~x:(plane ()) ~y:(plane ()) |> get_string_ok);
       "vec3", Float3 (Packed.Float3.of_owned ~x:(plane ()) ~y:(plane ()) ~z:(plane ()) |> get_string_ok);
       "vec4", Float4 (Packed.Float4.of_owned ~x:(plane ()) ~y:(plane ()) ~z:(plane ()) ~w:(plane ()) |> get_string_ok);
       "float_rows", Float_array (Packed.Float_array.create_owned ~offsets:(Array.init (n + 1) Fun.id)
         ~values:(plane ()) |> get_string_ok);
       "int_rows", Int_array (Packed.Int_array.create_owned ~offsets:(Array.init (n + 1) Fun.id)
         ~values:(Array.copy integers) |> get_string_ok)])
    geometry [Attribute.Point; Vertex; Primitive; Detail]
let no_input = ["none", []]
let standard_inputs = List.map (fun (label, g) -> label, [g]) [
  "empty", fixture_groups (points 0); "single", fixture_groups (points 1);
  "grid4225", fixture_groups (grid ());
  "mixed", fixture_attributes (fixture_groups (polygon_polyline ()))]

(* ---- building nodes ---- *)
let checked = function Ok v -> v | Error d -> fail (Diagnostic.to_string d)
let errors ds = List.filter (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds
let render = function
  | P.Bool_value b -> string_of_bool b | Int_value n -> string_of_int n
  | Float_value f -> Flow.Lisp.float f
  | Text_value s | Choice_value s -> Flow.Lisp.flat (Flow.Syntax.make (Str s))
let snapshots geometries = List.map Lisp_sop.snapshot geometries

(* Everything that depends only on the node and one input fixture. *)
type ctx = { node : t; sources : Flow.Syntax.t list; factories : Sop.Edit_graph.factory list;
             catalog : Flow.Check.catalog; head : string; native_inputs : N.t list;
             ops : Flow.Op.t list; geometry_count : int; image_count : int }
let key_of f = Sop.Edit_graph.factory_key f
let image_node image = Sop.Node.Private.make ~operation:"image/external" ~version:1
  ~parameters:(string_of_int (Sop.Image.data_id image)) ~cook_mode:Sop.Node.Generator
  ~dependencies:Sop.Context.Dependencies.static ~inputs:[||] (fun ~node_id:_ _ _ ->
    Ok Sop.Node.Private.{payload = Sop.Payload.Image image; instances = None})
let context ?(label = "") ~dir:_ node geometries =
  let others = List.filter (fun f -> key_of f <> node.key) Sop_catalog.Editor.factories in
  let sources = Flow_sop.Catalog.sources () |> checked in
  check (List.length others = List.length Sop_catalog.Editor.factories)
    (node.key ^ ": Lisp port must not retain a native catalog factory");
  let images = match List.assoc_opt node.key !image_registry with Some f -> f label | None -> [] in
  let natives = snapshots geometries @ List.map image_node images in
  let inputs = List.mapi (fun i n -> Sop.Edit_graph.factory ~key:(Printf.sprintf "port_in%d" i)
    ~label:"Input" ~category:["Test"] ~arity:0 (fun _ -> n)) (snapshots geometries) in
  let image_factories = List.mapi (fun i image -> let n = image_node image in
    Sop.Edit_graph.factory ~key:(Printf.sprintf "port_img%d" i) ~label:"Image" ~category:["Test"] ~arity:0
      (fun _ -> n)) images in
  let image_ops = List.mapi (fun i _ ->
    let prototype = Option.get (Flow.Op.find "image/noise" Flow.Context.value) in
    { prototype with Flow.Op.name = Printf.sprintf "image/port_img%d" i;
      signature = {pos = []; opt = []; rest = None; kw = []};
      check = (fun _ -> ()); packed_extension = None;
      body = (fun ~live:_ ~node _ -> node (Printf.sprintf "sop/port_img%d" i) []) }) images in
  let factories = inputs @ image_factories @ others in
  { node; sources; factories; native_inputs = natives; ops = image_ops @ Flow_sop.Operators.all;
    catalog = Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> checked;
    geometry_count = List.length geometries; image_count = List.length images;
    head = String.concat " " (List.mapi (fun i _ -> Printf.sprintf "(sop/port_in%d)" i) (snapshots geometries)
      @ List.mapi (fun i _ -> Printf.sprintf "(image/port_img%d)" i) images) }

(* A fixture with fewer geometries than the factory has slots leaves the trailing
   (optional) slots absent on both sides. *)
let native_initial ctx =
  let missing = Sop.Edit_graph.factory_arity ctx.node.native - List.length ctx.native_inputs in
  if missing <= 0 then Sop.Edit_graph.instantiate ctx.node.native ctx.native_inputs
  else Sop.Edit_graph.instantiate_optional ctx.node.native
    (List.map Option.some ctx.native_inputs @ List.init missing (fun _ -> None))
let native ctx changes =
  Result.bind (native_initial ctx) (fun n -> Result.map fst (N.apply_parameters n changes))

(* Changes name physical fields (center_x); Lisp takes a vec3 group as :center [x y z],
   its untouched components at their defaults. *)
let keywords ctx changes =
  let fields = match native_initial ctx with Ok n -> N.parameter_fields n | Error m -> fail m in
  let value (f : P.field_view) = Option.value (List.assoc_opt f.name changes) ~default:f.current in
  let groups = List.filter_map (fun (k, _) ->
    Option.bind (List.find_opt (fun (f : P.field_view) -> f.name = k) fields) (fun f -> Option.map fst f.vec3)) changes
    |> List.sort_uniq String.compare in
  List.filter_map (fun (k, v) ->
    match List.find_opt (fun (f : P.field_view) -> f.name = k) fields with
    | Some { vec3 = Some _; _ } -> None
    | _ -> Some (":" ^ k ^ " " ^ render v)) changes
  @ List.map (fun group -> Printf.sprintf ":%s [%s]" group (String.concat " " (List.map (fun i ->
      render (value (List.find (fun (f : P.field_view) -> f.vec3 = Some (group, i)) fields))) [0; 1; 2]))) groups

let lisp_source ctx changes =
  let words = String.concat " " (keywords ctx changes) in
  Printf.sprintf "(workspace port (graph g :context sop (sop/%s %s %s)))" ctx.node.key ctx.head words

(* Check and lower once; also return the reference-toggle builder used by parity tests. *)
let lisp_compiled ?(phase = fun _ _ -> ()) ?(initial_reference = false) ctx changes =
  let timed name f =
    let started = Unix.gettimeofday () in
    let result = f () in
    phase name (Unix.gettimeofday () -. started);
    result in
  let text = lisp_source ctx changes in
  match timed "parse" (fun () -> Flow.Syntax.parse text) with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok forms -> match timed "check" (fun () -> Flow_sop.Catalog.check_workspace ~sources:ctx.sources ~ops:ctx.ops ctx.catalog forms) with
    | Some w, ds when errors ds = [] ->
      let build reference = match timed "lower" (fun () ->
        Flow_sop.Lower.of_checked ~provenance_tags:false ~reference ~factories:ctx.factories w) with
          | Error d -> Error (Diagnostic.to_string d)
          | Ok lowered -> let g = List.hd lowered.graphs in
              Result.map (fun n -> n, lowered.factories)
                (timed "compile_node" (fun () ->
                   Sop.Edit_graph.compile_node g.network.geometry ~node_id:(Option.get g.root))) in
        (* Build the selected tier once so a construction refusal is seen. *)
        (match (try build initial_reference with Invalid_argument m | Failure m -> Error m) with
         | Error m -> Error m | Ok node -> Ok (node, build))
    | _, ds -> Error (String.concat "\n" (List.map Diagnostic.to_string (match errors ds with [] -> ds | e -> e)))

let lisp ctx changes = Result.map snd (lisp_compiled ctx changes)

let symbol value = Flow.Syntax.make (Flow.Syntax.Sym value)
let string value = Flow.Syntax.make (Flow.Syntax.Str value)
let form values = Flow.Syntax.make (Flow.Syntax.List values)
let tagged name values = form (symbol name :: values)
let write_once path contents =
  if Sys.file_exists path then
    check (In_channel.with_open_bin path In_channel.input_all = contents)
      ("frozen fixture changed without review: " ^ path)
  else Out_channel.with_open_bin path (fun channel -> output_string channel contents)

let frozen_fixture out_dir kind value encode decode equal =
  let contents = encode value in
  let rebuilt = decode contents in
  check (equal value rebuilt) ("fixture serializer failed round-trip for " ^ kind);
  let digest = Digest.to_hex (Digest.string contents) in
  let relative = "inputs/" ^ kind ^ "-" ^ digest ^ ".sexp" in
  let inputs_dir = Filename.concat out_dir "inputs" in
  if not (Sys.file_exists inputs_dir) then Unix.mkdir inputs_dir 0o755;
  write_once (Filename.concat out_dir relative) contents;
  relative

type fixture_cache = {
  mutable geometries : (Geometry.t * string) list;
  mutable geometry_count : int;
  mutable images : (Sop.Image.t * string) list;
  mutable image_count : int;
}
let fixture_cache_capacity = 256
let create_fixture_cache () = { geometries = []; geometry_count = 0; images = []; image_count = 0 }

let frozen_geometry cache out_dir geometry =
  match List.find_opt (fun (cached, _) -> cached == geometry) cache.geometries with
  | Some (_, relative) -> relative
  | None ->
      let relative = frozen_fixture out_dir "geometry" geometry Fixture_codec.encode
        Fixture_codec.decode
        (fun left right -> geometry_bytes left = geometry_bytes right) in
      if cache.geometry_count < fixture_cache_capacity then begin
        cache.geometries <- (geometry, relative) :: cache.geometries;
        cache.geometry_count <- cache.geometry_count + 1
      end;
      relative

let frozen_image cache out_dir image =
  match List.find_opt (fun (cached, _) -> cached == image) cache.images with
  | Some (_, relative) -> relative
  | None ->
      let relative = frozen_fixture out_dir "image" image Fixture_codec.encode_image
        Fixture_codec.decode_image
        (fun left right -> Sop.Image.width left = Sop.Image.width right
          && Sop.Image.height left = Sop.Image.height right
          && Option.map Bytes.to_string (Sop.Image.Private.rgba8 left)
             = Option.map Bytes.to_string (Sop.Image.Private.rgba8 right)
          && Array.map Int64.bits_of_float (Sop.Image.Private.storage left)
             = Array.map Int64.bits_of_float (Sop.Image.Private.storage right)) in
      if cache.image_count < fixture_cache_capacity then begin
        cache.images <- (image, relative) :: cache.images;
        cache.image_count <- cache.image_count + 1
      end;
      relative

let stable_error message =
  let bracketed = match String.index_opt message '[' with
    | Some bracket -> Option.map (fun finish -> bracket + 1, finish)
        (String.index_from_opt message (bracket + 1) ']')
    | None -> None in
  let code, body = match bracketed with
    | Some (start, finish) ->
      String.sub message start (finish - start),
      String.trim (String.sub message (finish + 1) (String.length message - finish - 1))
    | None ->
        let code_end = ref 0 in
        while !code_end < String.length message
            && (match message.[!code_end] with 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false) do
          incr code_end
        done;
        if !code_end >= 3 && String.starts_with ~prefix:"E_" message then begin
          let rest = String.trim (String.sub message !code_end (String.length message - !code_end)) in
          if String.starts_with ~prefix:"at " rest then
            let line = match String.index_opt rest ':' with
              | None -> None | Some first -> String.index_from_opt rest (first + 1) ':' in
            (match line with
             | Some colon -> String.sub message 0 !code_end,
                 String.trim (String.sub rest (colon + 1) (String.length rest - colon - 1))
             | None -> String.sub message 0 !code_end, rest)
          else String.sub message 0 !code_end,
            (if String.starts_with ~prefix:":" rest then String.trim (String.sub rest 1 (String.length rest - 1)) else rest)
        end else match String.index_opt message ':' with
          | Some colon -> String.sub message 0 colon,
              String.trim (String.sub message (colon + 1) (String.length message - colon - 1))
          | None -> "E_LISP", message in
  code, body

let replay_source ctx changes =
  let words = String.concat " " (keywords ctx changes) in
  let geometry_inputs = List.init ctx.geometry_count (fun i -> Printf.sprintf "(sop/ext_port_in%d)" i) in
  let image_inputs = List.init ctx.image_count (fun i -> Printf.sprintf "(image/ext_port_img%d)" i) in
  Printf.sprintf "(sop/%s %s %s)" ctx.node.key (String.concat " " (geometry_inputs @ image_inputs)) words

let with_session f =
  let s = Sop.Session.create ~max_entries:64 ~max_payload_bytes:512_000_000 |> get_string_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close s) (fun () -> f s)
let with_capture_session f =
  let s = Sop.Session.create ~max_entries:0 ~max_payload_bytes:0 |> get_string_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close s) (fun () -> f s)
let cook s domains grain node =
  let context = Sop.Context.create ~domains ~grain () |> get_string_ok in
  Result.bind (Sop.Session.cook s ~context node) (fun o ->
    Result.bind (Sop.Payload.geometry o.Sop.Session.payload) (fun g -> match o.Sop.Session.instances with
      | None -> Ok g
      | Some transforms -> (* packed result: compared as its materialized copies *)
          Result.map_error (fun e -> Diagnostic.error ~code:"E_INSTANCES" (Rdk.Error.to_string e))
            (Rdk.Instance_copy.materialize_instances ~transforms g)))
let must = function Ok g -> g | Error d -> fail (Diagnostic.error_to_string d)

(* Capture time uses registry builders once. Every row then contains a literal Lisp call and
   references portable checked input files; the check path need not load the native registry. *)
let capture_goldens ~dir ~out_dir ~cache node =
  let cases = ("defaults", []) :: node.cases in
  List.concat_map (fun (fixture, geometries) ->
    let ctx = context ~label:fixture ~dir node geometries in
    (* Inputs do not vary between cases. Serialize and validate each fixture once, not once per
       parameter row (the large geometry codec allocates heavily during its round-trip check). *)
    let fixture_codec_started = Unix.gettimeofday () in
    let geometry_inputs = List.map (fun geometry ->
      tagged "geometry" [string (frozen_geometry cache out_dir geometry)]) geometries in
    let images = match List.assoc_opt node.key !image_registry with
      | Some build -> build fixture | None -> [] in
    let image_inputs = List.map (fun image ->
      tagged "image" [string (frozen_image cache out_dir image)]) images in
    let fixture_codec_elapsed = Unix.gettimeofday () -. fixture_codec_started in
    let inputs = tagged "inputs" (geometry_inputs @ image_inputs) in
    let input_sizes = geometries |> List.mapi (fun i geometry ->
      Printf.sprintf "g%d:p%d/v%d/f%d" i (Geometry.point_count geometry)
        (Geometry.vertex_count geometry) (Geometry.primitive_count geometry))
      |> String.concat "," in
    Printf.eprintf "fixture_codec node=%s fixture=%S inputs=[%s] in %.4fs\n%!"
      node.key fixture input_sizes fixture_codec_elapsed;
    List.mapi (fun index (label, changes) ->
      let gc_before = Gc.quick_stat () in
      Printf.eprintf "starting node=%s fixture=%S case=%S (%d/%d) inputs=[%s] live_words=%d\n%!"
        node.key fixture label (index + 1) (List.length cases) input_sizes gc_before.Gc.live_words;
      let started = Unix.gettimeofday () in
      let source = replay_source ctx changes in
      let output_size = ref "not-cooked" in
      let phases = Hashtbl.create 4 in
      let phase name elapsed = Hashtbl.replace phases name elapsed in
      let outcome = Fun.protect ~finally:Flow_sop.Lower.Private.clear_cache (fun () ->
        (* Goldens freeze Lisp semantics through the reference evaluator; parity tests separately
           require the packed path to match before these rows can replace the old oracle checks. *)
        match lisp_compiled ~phase ~initial_reference:true ctx changes with
        | Error message ->
            output_size := "refused";
            let code, message = stable_error message in tagged "error" [string code; string message]
        | Ok ((node, _), _build) ->
            (* Capture Lisp meaning independently of the packed compiler. The port parity gate
               separately checks that packed execution matches this reference path. A one-cook
               session cannot hit its cache, so disable it to avoid key construction and retention. *)
            let cook_started = Unix.gettimeofday () in
            let cooked = with_capture_session (fun session -> cook session 4 97 node) in
            phase "cook" (Unix.gettimeofday () -. cook_started);
            (match cooked with
             | Error error ->
                 output_size := "error";
                 tagged "error" [string error.Diagnostic.code; string error.message]
             | Ok geometry ->
                 output_size := Printf.sprintf "p%d/v%d/f%d" (Geometry.point_count geometry)
                   (Geometry.vertex_count geometry) (Geometry.primitive_count geometry);
                 let codec_started = Unix.gettimeofday () in
                 let bytes = geometry_bytes geometry in
                 let digest = Digest.to_hex (Digest.string bytes) in
                 phase "geometry_bytes_digest" (Unix.gettimeofday () -. codec_started);
                 tagged "ok" [string digest])) in
      let gc_after = Gc.quick_stat () in
      Printf.eprintf "captured node=%s fixture=%S case=%S in %.2fs phases=parse:%.4f check:%.4f lower:%.4f compile_node:%.4f cook:%.4f geometry_bytes_digest:%.4f output=%s major_words_delta=%.0f live_words=%d\n%!"
        node.key fixture label (Unix.gettimeofday () -. started)
        (Option.value (Hashtbl.find_opt phases "parse") ~default:0.)
        (Option.value (Hashtbl.find_opt phases "check") ~default:0.)
        (Option.value (Hashtbl.find_opt phases "lower") ~default:0.)
        (Option.value (Hashtbl.find_opt phases "compile_node") ~default:0.)
        (Option.value (Hashtbl.find_opt phases "cook") ~default:0.)
        (Option.value (Hashtbl.find_opt phases "geometry_bytes_digest") ~default:0.)
        !output_size
        (gc_after.Gc.major_words -. gc_before.Gc.major_words) gc_after.Gc.live_words;
      Flow.Lisp.flat (tagged "golden" [string node.key; string fixture; string label; string source; inputs; outcome]))
      cases) node.inputs

let coverage_rows nodes =
  List.concat_map (fun node ->
    List.concat_map (fun (fixture, _) ->
      List.map (fun (label, _) ->
        Flow.Lisp.flat (tagged "case" [string node.key; string fixture; string label]))
        (("defaults", []) :: node.cases))
      node.inputs)
    nodes

(* ---- checks ---- *)
let changed (v : P.field_view) = match v.kind, v.current with
  | P.Toggle_view, P.Bool_value b -> P.Bool_value (not b)
  | Integer_view r, Int_value n -> Int_value (if Some n = r.hard_max then n - 1 else n + 1)
  | Floating_view r, Float_value x -> Float_value (if Some x = r.hard_max then x -. 0.25 else x +. 0.25)
  | Text_view, Text_value s -> Text_value (s ^ "_changed")
  | Choice_view o, Choice_value s -> Choice_value (Option.get (Array.find_opt (( <> ) s) o))
  | _ -> fail (v.name ^ ": view and value disagree")

let cache_identity node =
  with_session (fun s ->
    let ok n = Result.is_ok (cook s 1 97 n) and misses () = (Sop.Session.stats s).misses in
    (* A failed cook is not cached, so a fixture whose default errors natively checks only edits. *)
    let cooked = ok node in let before = misses () in ignore (ok node);
    if cooked then check (misses () = before) "cache: equal parameters did not hit";
    List.iter (fun (v : P.field_view) ->
      (* A single edit the node's own validator refuses has no cache entry to test. *)
      match N.apply_parameters node [v.name, changed v] with Error _ -> () | Ok (edited, _) ->
      let before = misses () in let cooked = ok edited in
      check (misses () = before + 1) ("cache: changed " ^ v.name ^ " did not miss");
      ignore (ok edited);
      check (misses () = before + (if cooked then 1 else 2)) ("cache: edited " ^ v.name ^ " did not then hit"))
      (N.parameter_fields node))

let refusals ~native node =
  List.iter (fun (v : P.field_view) -> match v.kind, v.current with
    | P.Floating_view _, P.Float_value x -> List.iter (fun bad ->
        let probe n = N.apply_parameters n [v.name, P.Float_value (x +. 0.25); v.name, P.Float_value bad] in
        match probe node, probe native with
        | Error a, Error b -> check (a = b) ("refusal: " ^ v.name ^ " error differs from native: " ^ a ^ " / " ^ b)
        | _ -> fail ("refusal: " ^ v.name ^ " accepted a non-finite value"))
        [Float.nan; Float.infinity; Float.neg_infinity]
    | _ -> ()) (N.parameter_fields node)

let verify_case ~full ctx ulps label changes =
  let at what = Printf.sprintf "%s/%s: %s" ctx.node.key label what in
  let protect what f = try f () with Failure m -> fail (at what ^ ": " ^ m) in
  match native ctx changes, lisp ctx changes with
  | Error _, Error _ -> ()
  | Error m, Ok _ -> fail (at "oracle: native refused (" ^ m ^ ") but the Lisp node was accepted")
  | Ok _, Error m -> fail (at "oracle: Lisp refused a valid case: " ^ m)
  | Ok expected, Ok build ->
      let actual, _ = get_string_ok (build false) and walked, _ = get_string_ok (build true) in
      protect "parameters" (fun () -> check (N.parameter_fields actual = N.parameter_fields expected)
        "parameter_fields differ from native");
      protect "cache" (fun () -> check (N.parameter_key actual = N.parameter_key expected)
        "parameter_key differs from native");
      with_session (fun s -> match cook s 1 97 expected, cook s 1 97 actual with
        | Error e, Error g -> protect "oracle" (fun () -> check (e.code = g.code)
            ("error code " ^ g.code ^ " but native " ^ e.code))
        | Error e, Ok _ -> fail (at ("oracle: native error " ^ Diagnostic.error_to_string e ^ " but Lisp succeeded"))
        | Ok _, Error g -> fail (at ("oracle: Lisp error " ^ Diagnostic.error_to_string g ^ " but native succeeded"))
        | Ok e, Ok g ->
            (if not (close_geometry ~ulps e g) && Sys.getenv_opt "PORT_DIFF" <> None then begin
              (let counts h = Printf.sprintf "p%d v%d f%d" (Rdk.Geometry.point_count h) (Rdk.Geometry.vertex_count h) (Rdk.Geometry.primitive_count h) in
               let attrs h = String.concat "," (List.map (fun a -> Rdk.Attribute.name a) (Rdk.Geometry.attributes h)) in
               let groups h = String.concat "," (List.map (fun gr -> Printf.sprintf "%s/%d" (Rdk.Group.name gr) (Rdk.Group.cardinality gr)) (Rdk.Geometry.groups h)) in
               if counts e <> counts g || attrs e <> attrs g || groups e <> groups g then
                 Printf.eprintf "native %s [%s] [%s]\nlisp   %s [%s] [%s]\n" (counts e) (attrs e) (groups e) (counts g) (attrs g) (groups g);
               List.iter (fun gr -> match Rdk.Geometry.find_group ~owner:(Rdk.Group.owner gr) (Rdk.Group.name gr) g with
                 | None -> ()
                 | Some other ->
                     let members h = List.init (Rdk.Group.length h) Fun.id |> List.filter (fun i -> Rdk.Group.mem i h) in
                     let order h = match Rdk.Group.Private.order_view h with None -> [] | Some o -> Array.to_list o in
                     if members gr <> members other then Printf.eprintf "group %s members differ\n" (Rdk.Group.name gr)
                     else if order gr <> order other then
                       Printf.eprintf "group %s order native [%s] lisp [%s]\n" (Rdk.Group.name gr)
                         (String.concat " " (List.map string_of_int (order gr))) (String.concat " " (List.map string_of_int (order other)))) (Rdk.Geometry.groups e);
               List.iter (fun a -> match Rdk.Geometry.find_attribute ~owner:(Rdk.Attribute.owner a) (Rdk.Attribute.name a) g with
                 | Some b -> (match Rdk.Attribute.Private.storage a, Rdk.Attribute.Private.storage b with
                     | Rdk.Attribute.Int x, Rdk.Attribute.Int y when x <> y -> Printf.eprintf "int attr %s differs\n" (Rdk.Attribute.name a)
                     | Rdk.Attribute.Text x, Rdk.Attribute.Text y when x <> y -> Printf.eprintf "text attr %s differs\n" (Rdk.Attribute.name a)
                     | Rdk.Attribute.Float x, Rdk.Attribute.Float y when x <> y -> Printf.eprintf "float attr %s differs\n" (Rdk.Attribute.name a)
                     | Rdk.Attribute.Int_array x, Rdk.Attribute.Int_array y when Rdk.Packed.Int_array.Private.view x <> Rdk.Packed.Int_array.Private.view y ->
                         Printf.eprintf "int_array attr %s differs\n" (Rdk.Attribute.name a)
                     | _ -> ())
                 | None -> ()) (Rdk.Geometry.attributes e);
               let tv h = let v = Rdk.Topology.Private.view (Rdk.Geometry.topology h) in (v.vertex_points, v.primitive_offsets, v.primitive_kinds) in
               if tv e <> tv g then prerr_endline "topology differs";
               Printf.eprintf "edge groups native %d lisp %d\n" (List.length (Rdk.Geometry.edge_groups e)) (List.length (Rdk.Geometry.edge_groups g)));
              (let x = Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions e) and y = Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions g) in
                if Array.length x.x <> Array.length y.x then Printf.eprintf "P count %d vs %d\n" (Array.length x.x) (Array.length y.x) else
                Array.iteri (fun i v -> if v <> y.x.(i) || x.y.(i) <> y.y.(i) || x.z.(i) <> y.z.(i) then
                  Printf.eprintf "P[%d] native (%h %h %h) lisp (%h %h %h)\n" i v x.y.(i) x.z.(i) y.x.(i) y.y.(i) y.z.(i)) x.x);
              List.iter (fun a -> match Rdk.Geometry.find_attribute ~owner:(Rdk.Attribute.owner a) (Rdk.Attribute.name a) g with
                | Some b -> (match Rdk.Attribute.Private.storage a, Rdk.Attribute.Private.storage b with
                    | Rdk.Attribute.Float3 x, Rdk.Attribute.Float3 y ->
                        let x = Rdk.Packed.Float3.Private.view x and y = Rdk.Packed.Float3.Private.view y in
                        Array.iteri (fun i v -> if v <> y.x.(i) || x.y.(i) <> y.y.(i) || x.z.(i) <> y.z.(i) then
                          Printf.eprintf "%s[%d] native (%h %h %h) lisp (%h %h %h)\n" (Rdk.Attribute.name a) i v x.y.(i) x.z.(i) y.x.(i) y.y.(i) y.z.(i)) x.x
                    | _ -> ())
                | None -> Printf.eprintf "missing %s\n" (Rdk.Attribute.name a)) (Rdk.Geometry.attributes e) end);
            protect "oracle" (fun () -> check (close_geometry ~ulps e g)
              (Printf.sprintf "Lisp differs from native beyond %d ulps" ulps));
            let bytes = geometry_bytes g in
            protect "self" (fun () ->
              check (geometry_bytes (must (cook s 1 97 walked)) = bytes) "reference interpreter differs from kernel tier";
              List.iter (fun (domains, grain) -> Sop.Session.clear s;
                check (geometry_bytes (must (cook s domains grain actual)) = bytes)
                  (Printf.sprintf "bytes differ at domains=%d grain=%d" domains grain)) (if full then [1, 1; 1, 97; 1, 4096; 4, 1; 4, 97; 4, 4096] else [4, 1; 4, 4096])))

(* [quick]: every case on the last fixture, defaults (full matrix) on the others. The
   full cross product is the `ports` alias and `main.exe KEY --full`. *)
let run_gate ?(quick = false) ~dir node =
  let all = ("defaults", []) :: node.cases and count = ref 0 in
  let last = List.length node.inputs - 1 in
  List.iteri (fun i (fixture, geometries) ->
    let cases = if quick && i <> last then [List.hd all] else all in
    let ctx = context ~label:fixture ~dir node geometries in
    let tag what = fun f -> try f () with Failure m -> fail (Printf.sprintf "%s/%s: %s: %s" node.key fixture what m) in
    List.iter (fun (label, changes) -> incr count;
      tag label (fun () -> verify_case ~full:(label = "defaults") ctx node.ulps (fixture ^ "/" ^ label) changes)) cases;
    tag "defaults" (fun () ->
      let actual, factories = match lisp ctx [] with Ok b -> get_string_ok (b false) | Error m -> fail m in
      let expected = get_string_ok (native ctx []) in
      tag "cache" (fun () -> cache_identity actual);
      tag "refusals" (fun () -> refusals ~native:expected actual);
      if i = 0 then tag "parameters" (fun () ->
        let lisp_factory = List.find (fun f -> key_of f = node.key) factories in
        check (Flow_sop.Catalog.descriptor lisp_factory = Flow_sop.Catalog.descriptor node.native)
          "Catalog.descriptor differs from native")))
    node.inputs;
  Printf.printf "PASS %s%s: %d fixtures, %d cases run\n%!" node.key (if quick then " (quick)" else "")
    (List.length node.inputs) !count

let run_bench ~dir node = match node.bench with
  | None -> Printf.printf "| %s | - | no bench registered | - | - |\n%!" node.key; true
  | Some build -> let geometries, changes = build () in
      let ctx = context ~label:"bench" ~dir node geometries in
      let actual = match lisp ctx changes with Ok b -> fst (get_string_ok (b false)) | Error m -> fail m
      and expected = get_string_ok (native ctx changes) in
      let sample domains n = List.init 3 (fun _ -> with_session (fun s -> Gc.full_major ();
        let t = Unix.gettimeofday () in ignore (must (cook s domains 4096 n)); Unix.gettimeofday () -. t))
        |> List.sort Float.compare |> fun l -> List.nth l 1 in
      List.fold_left (fun ok domains ->
        let nt = sample domains expected and lt = sample domains actual in
        Printf.printf "| %s | %d | %.6f | %.6f | %.3f |\n%!" node.key domains nt lt (lt /. nt);
        ok && lt /. nt <= 2.) true [1; 4]
