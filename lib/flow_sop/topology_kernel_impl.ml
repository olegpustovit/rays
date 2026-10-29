module E = Flow.Eval
module G = Rdk.Geometry

let error code message = Error (Diagnostic.error ~code message)

(* A topology's component arrays, remembered by the topology's identity (both immutable): a recook
   over the same topology reads the same arrays, so a device converts and uploads its tables once. *)
let kept_components : Rdk.Topology.t -> (unit -> (string, E.value) Hashtbl.t * Mutex.t) -> _ =
  Flow.Value.remember ()

let kept geometry kind component make =
  let table, lock =
    kept_components (G.topology geometry) (fun () -> (Hashtbl.create 8, Mutex.create ()))
  in
  let key = kind ^ component in
  match Mutex.protect lock (fun () -> Hashtbl.find_opt table key) with
  | Some value -> Ok value
  | None ->
      Result.map
        (fun value ->
          Mutex.protect lock (fun () -> Hashtbl.replace table key value);
          value)
        (make geometry component)

let topology geometry component =
  let view = Rdk.Topology.View.view (G.topology geometry) in
  match component with
  | "vertex_points" -> Ok (E.Int_array (Array.copy view.vertex_points))
  | "primitive_offsets" -> Ok (E.Int_array (Array.copy view.primitive_offsets))
  | "kinds" ->
      Ok
        (E.Int_array
           (Array.init (Bytes.length view.primitive_kinds) (fun i ->
                Char.code (Bytes.get view.primitive_kinds i))))
  | _ -> error "E_DATA_SOURCE" "Topology source requires vertex_points, primitive_offsets or kinds."

(* The point incidence (CSR of vertices by point) is the Point_index's, without the edges a
   Topology_index hashes: a million-cell grid's normals ask only for it. *)
let adjacency geometry component =
  let array a = Ok (E.Int_array (Array.copy a)) in
  let points () = Rdk.Point_index.View.view (Rdk.Point_index.create (G.topology geometry)) in
  let view () = Rdk.Topology_index.View.view (Rdk.Topology_index.create (G.topology geometry)) in
  match component with
  | "primitive_of_vertex" -> array (points ()).primitive_of_vertex
  | "point_vertex_offsets" -> array (points ()).point_offsets
  | "point_vertices" -> array (points ()).point_vertices
  | "next_vertex" -> array (view ()).next_vertex
  | "previous_vertex" -> array (view ()).previous_vertex
  | "edge_of_vertex" -> array (view ()).edge_of_vertex
  | "opposite_vertex" -> array (view ()).opposite_vertex
  | "edge_a" -> array (view ()).edge_a
  | "edge_b" -> array (view ()).edge_b
  | "point_edge_offsets" -> array (view ()).point_edge_offsets
  | "point_edges" -> array (view ()).point_edges
  | _ -> error "E_DATA_SOURCE" "Unknown adjacency component."

(* The union-find of [Analysis.classify_connectivity]: [over] edges labels
   points, [over] points labels primitives; a 0/1 mask keeps the labelled
   elements (-1 elsewhere, not counted). *)
(* Rdk's shared ear clipping, every primitive in order (triangles included, no shortcut): vertex
   corners per triangle, primitive offsets into them, and ok 0 where the clipper refused (curve,
   degenerate or non-simple), which then emits no triangles. *)
let triangles geometry component =
  let topology = Rdk.Topology.View.view (G.topology geometry) in
  let positions = Rdk.Packed.Float3.View.view (G.positions geometry) in
  let count = Bytes.length topology.primitive_kinds in
  let offsets = Array.make (count + 1) 0 and ok = Array.make count 1 in
  let primitive = ref [] and a = ref [] and b = ref [] and c = ref [] in
  let scratch = Rdk_core.Polygon_triangulation.create_scratch () in
  for p = 0 to count - 1 do
    let rows = ref [] in
    (match
       Rdk_core.Polygon_triangulation.primitive ~positions ~topology ~scratch p
         ~emit:(fun _ x y z -> rows := (x, y, z) :: !rows)
     with
    | Ok () ->
        List.iter
          (fun (x, y, z) ->
            primitive := p :: !primitive;
            a := x :: !a;
            b := y :: !b;
            c := z :: !c)
          (List.rev !rows)
    | Error _ ->
        ok.(p) <- 0;
        rows := []);
    offsets.(p + 1) <- offsets.(p) + List.length !rows
  done;
  (* ponytail: list accumulation; pre-size from corner counts if this shows in a profile. *)
  let array l = Ok (E.Int_array (Array.of_list (List.rev l))) in
  match component with
  | "offsets" -> Ok (E.Int_array offsets)
  | "ok" -> Ok (E.Int_array ok)
  | "primitive" -> array !primitive
  | "a" -> array !a
  | "b" -> array !b
  | "c" -> array !c
  | _ -> error "E_DATA_SOURCE" "Triangles source requires offsets, ok, primitive, a, b or c."

let components geometry args =
  let owner =
    match List.assoc_opt "over" args with
    | None | Some (E.Text "edges") ->
        Ok (Rdk.Attribute.Point, Rdk.Group.Point, G.point_count geometry)
    | Some (E.Text "points") ->
        Ok (Rdk.Attribute.Primitive, Rdk.Group.Primitive, G.primitive_count geometry)
    | _ -> error "E_COMPONENTS_OVER" "Components :over is edges or points."
  in
  Result.bind owner (fun (owner, group_owner, count) ->
      let group =
        match List.assoc_opt "mask" args with
        | None -> Ok None
        | Some (E.Int_array mask) when Array.length mask = count ->
            if Array.exists (fun bit -> bit <> 0 && bit <> 1) mask then
              error "E_COMPONENTS_MASK" "Components mask holds only 0 and 1."
            else
              Ok
                (Some (Rdk.Group.init ~owner:group_owner ~name:"mask" count (fun i -> mask.(i) = 1)))
        | Some (E.Int_array _) ->
            error "E_COMPONENTS_MASK"
              "Components mask length differs from the labelled element count."
        | Some _ -> error "E_COMPONENTS_MASK" "Components mask is an integer array."
      in
      (* [:seams] is a 0/1 edge mask (one entry per [sop/edges] edge): a seam edge joins nothing, and
       primitives are then joined along shared non-seam edges instead of shared points. *)
      let seams =
        match List.assoc_opt "seams" args with
        | None -> Ok None
        | Some (E.Int_array mask) ->
            let topology = G.topology geometry in
            let index = Rdk.Topology_index.create topology in
            if Array.length mask <> Rdk.Topology_index.edge_count index then
              error "E_COMPONENTS_SEAMS" "Components seams length differs from the edge count."
            else if Array.exists (fun bit -> bit <> 0 && bit <> 1) mask then
              error "E_COMPONENTS_SEAMS" "Components seams hold only 0 and 1."
            else
              Ok (Some (Rdk.Edge_group.init ~topology ~index ~name:"seams" (fun i -> mask.(i) = 1)))
        | Some _ -> error "E_COMPONENTS_SEAMS" "Components seams is an integer array."
      in
      Result.bind seams (fun seams ->
          Result.bind group (fun group ->
              let primitives = if owner = Rdk.Attribute.Primitive then group else None
              and points = if owner = Rdk.Attribute.Point then group else None in
              match
                Rdk.Analysis.classify_connectivity ?primitives ?points ?seams owner geometry
              with
              | Error e -> error "E_COMPONENTS" (Rdk.Error.to_string e)
              | Ok (ids, n) -> (
                  match List.assoc_opt "component" args with
                  | Some (E.Text "count") -> Ok (E.Int n)
                  | Some (E.Text "id") -> Ok (E.Int_array ids)
                  | _ -> error "E_DATA_SOURCE" "Components source requires id or count."))))

(* Flow arrays remain reusable immutable inputs: owned native constructors
   receive copies, never the evaluator's retained input buffers. *)
(* A static point array's planes, remembered by its identity (both are immutable): a recook of the
   same grid builds the same positions, which downstream reads and a device holding them reuse. *)
let point_planes : float array -> (unit -> Rdk.Packed.Float3.t) -> Rdk.Packed.Float3.t =
  Flow.Value.remember ()

(* the same for static planes, keyed by x; y and z must be the ones remembered with it *)
let plane_copies :
    float array ->
    (unit -> float array * float array * Rdk.Packed.Float3.t) ->
    float array * float array * Rdk.Packed.Float3.t =
  Flow.Value.remember ()

let mesh_points = function
  | E.Vec3_array points as value ->
      Flow.Value.validate value;
      let count = Array.length points / 3 in
      let planes () =
        let x = Array.create_float count
        and y = Array.create_float count
        and z = Array.create_float count in
        for i = 0 to count - 1 do
          x.(i) <- points.(3 * i);
          y.(i) <- points.((3 * i) + 1);
          z.(i) <- points.((3 * i) + 2)
        done;
        Rdk.Packed.Float3.View.of_owned_exn ~x ~y ~z
      in
      Ok (count, fun () -> if count > 4096 then point_planes points planes else planes ())
  | E.Record fields when List.length fields = 3 -> (
      match (List.assoc_opt "x" fields, List.assoc_opt "y" fields, List.assoc_opt "z" fields) with
      | Some (E.Float_array x), Some (E.Float_array y), Some (E.Float_array z) ->
          (* Reuse the ordinary finite-array boundary before structural count
              validation. Retained Flow planes are never adopted by geometry. *)
          if max (Array.length x) (max (Array.length y) (Array.length z)) <= 65536 then
            Flow.Value.validate (E.Record fields)
          else begin
            let fields = Array.of_list fields in
            let errors =
              Parallel.init_array ~grain:1 (Array.length fields) (fun index ->
                  let value = snd fields.(index) in
                  if index = 0 then (
                    Flow.Value.validate value;
                    None)
                  else
                    try
                      Flow.Value.validate value;
                      None
                    with Diagnostic.Error _ as exn -> Some exn)
            in
            Array.iter (Option.iter raise) errors
          end;
          if Array.length x <> Array.length y || Array.length x <> Array.length z then
            error "E_ARRAY_COUNT" "Mesh coordinate planes need the same element count."
          else begin
            let count = Array.length x in
            Ok
              ( count,
                fun () ->
                  let copy () =
                    Rdk.Packed.Float3.View.of_owned_exn ~x:(Array.copy x) ~y:(Array.copy y)
                      ~z:(Array.copy z)
                  in
                  if count <= 4096 then copy ()
                  else
                    match plane_copies x (fun () -> (y, z, copy ())) with
                    | y', z', positions when y' == y && z' == z -> positions
                    | _ -> copy () )
          end
      | _, _, _ -> error "E_TOPOLOGY_TYPE" "Mesh planes require exactly x, y and z float arrays.")
  | E.Record _ -> error "E_TOPOLOGY_TYPE" "Mesh planes require exactly x, y and z float arrays."
  | _ ->
      error "E_TOPOLOGY_TYPE"
        "Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays."

let mesh args =
  match
    ( List.assoc_opt "points" args,
      List.assoc_opt "vertex_points" args,
      List.assoc_opt "primitive_offsets" args,
      List.assoc_opt "kinds" args )
  with
  | Some points, Some (E.Int_array vertices), Some (E.Int_array offsets), Some (E.Int_array kinds)
    -> (
      try
        Result.bind (mesh_points points) (fun (count, positions) ->
            if Array.exists (fun kind -> kind < 0 || kind > 2) kinds then
              error "E_TOPOLOGY_KIND"
                "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline)."
            else
              let kinds =
                Array.map
                  (function 0 -> Rdk.Topology.Polygon | 1 -> Open_polyline | _ -> Closed_polyline)
                  kinds
              in
              Result.bind
                (Result.map_error
                   (Diagnostic.error ~code:"E_TOPOLOGY")
                   (Rdk.Topology.create_owned ~point_count:count
                      ~vertex_points:(Array.copy vertices) ~primitive_offsets:(Array.copy offsets)
                      ~primitive_kinds:kinds))
                (fun topology ->
                  let positions = positions () in
                  Result.map_error
                    (Diagnostic.error ~code:"E_TOPOLOGY")
                    (G.create ~positions ~topology ())))
      with Diagnostic.Error diagnostic -> Error diagnostic)
  | _ ->
      error "E_TOPOLOGY_TYPE"
        "Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays."

let count geometry = function
  | "points" -> Ok (G.point_count geometry)
  | "vertices" -> Ok (G.vertex_count geometry)
  | "primitives" -> Ok (G.primitive_count geometry)
  | "edges" -> Ok (Rdk.Topology_index.edge_count (Rdk.Topology_index.create (G.topology geometry)))
  | _ -> error "E_TOPOLOGY_COUNT" "Count element is points, vertices, primitives or edges."

let resolve ~geometry = function
  | E.Source
      ( (( "sop/count" | "sop/edges" | "sop/topology" | "sop/adjacency" | "sop/components"
         | "sop/triangles" ) as name),
        _,
        args ) -> (
      match List.assoc_opt "geometry" args with
      | Some (E.Deferred (ty, id)) when Flow.Ty.is_geometry ty -> (
          match geometry id with
          | None -> error "E_DATA_SOURCE" "Topology source has not been cooked."
          | Some geometry -> (
              if name = "sop/count" then
                match List.assoc_opt "element" args with
                | Some (E.Text element) -> Result.map (fun n -> E.Int n) (count geometry element)
                | _ ->
                    error "E_TOPOLOGY_COUNT"
                      "Count element is points, vertices, primitives or edges."
              else if name = "sop/topology" then
                match List.assoc_opt "component" args with
                | Some (E.Text component) -> kept geometry "topology:" component topology
                | _ ->
                    error "E_DATA_SOURCE"
                      "Topology source requires vertex_points, primitive_offsets or kinds."
              else if name = "sop/components" then components geometry args
              else if name = "sop/triangles" then
                match List.assoc_opt "component" args with
                | Some (E.Text component) -> triangles geometry component
                | _ -> error "E_DATA_SOURCE" "Triangles source requires a component name."
              else if name = "sop/adjacency" then
                match List.assoc_opt "component" args with
                | Some (E.Text component) -> kept geometry "adjacency:" component adjacency
                | _ -> error "E_DATA_SOURCE" "Adjacency source requires a component name."
              else
                let index =
                  Rdk.Topology_index.View.view (Rdk.Topology_index.create (G.topology geometry))
                in
                match List.assoc_opt "component" args with
                | Some (E.Text "a") -> Ok (E.Int_array (Array.copy index.edge_a))
                | Some (E.Text "b") -> Ok (E.Int_array (Array.copy index.edge_b))
                | _ -> error "E_DATA_SOURCE" "Edge source requires endpoint component a or b."))
      | _ -> error "E_DATA_SOURCE" "Topology source requires cooked geometry.")
  | _ -> error "E_DATA_SOURCE" "Unknown topology data source."

module R = Rdk_core.Topology_remap

let remap ?cancel ~grain ~source ~target args =
  let refuse code message = raise (Diagnostic.Error (Diagnostic.error ~code message)) in
  let result code = function Ok value -> value | Error message -> refuse code message in
  let check () = Rdk.Cancel.check_opt cancel in
  try
    check ();
    if grain <= 0 then refuse "E_REMAP" "Remap grain must be positive.";
    let positions =
      match List.assoc_opt "positions" args with
      | None | Some (E.Text "target") -> false
      | Some (E.Text "mapped") -> true
      | _ -> refuse "E_REMAP_POLICY" "Remap positions is target or mapped."
    in
    (* A plain map is one source row per new element (-1: a created default); a weighted map
       {:a :b :t} mixes rows a and b at t (rules in R.blend_attribute). *)
    let mapping name source_count target_count =
      let bounded label map =
        if Array.length map <> target_count then
          refuse "E_REMAP_COUNT" ("Remap " ^ name ^ " map must match target owner count.");
        Array.iteri
          (fun row index ->
            if row land 4095 = 0 then check ();
            if (index < if label = "" then -1 else 0) || index >= source_count then
              refuse "E_REMAP_INDEX" ("Remap " ^ name ^ " source index is out of bounds."))
          map
      in
      let mix =
        match name with
        | "points" -> "point_mix"
        | "vertices" -> "vertex_mix"
        | _ -> "primitive_mix"
      in
      match
        match List.assoc_opt name args with
        | Some _ as plain -> plain
        | None -> List.assoc_opt mix args
      with
      | Some (E.Int_array map) ->
          bounded "" map;
          `Plain map
      | Some (E.Record fields) -> (
          match
            ( List.sort compare (List.map fst fields),
              List.assoc_opt "a" fields,
              List.assoc_opt "b" fields,
              List.assoc_opt "t" fields )
          with
          | [ "a"; "b"; "t" ], Some (E.Int_array a), Some (E.Int_array b), Some (E.Float_array t) ->
              if Array.length t <> target_count then
                refuse "E_REMAP_COUNT" ("Remap " ^ name ^ " map must match target owner count.");
              bounded "a" a;
              bounded "b" b;
              Array.iteri
                (fun row weight ->
                  if row land 4095 = 0 then check ();
                  if not (Float.is_finite weight) then
                    refuse "E_REMAP_WEIGHT" ("Remap " ^ name ^ " weight must be finite."))
                t;
              `Weighted (a, b, t)
          | _ ->
              refuse "E_REMAP_TYPE"
                ("Remap " ^ mix ^ " is {:a :b :t} integer, integer and float arrays."))
      | _ -> refuse "E_REMAP_TYPE" ("Remap " ^ name ^ " requires an integer array.")
    in
    let point_carry = mapping "points" (G.point_count source) (G.point_count target) in
    let vertex_carry = mapping "vertices" (G.vertex_count source) (G.vertex_count target) in
    let primitive_carry =
      mapping "primitives" (G.primitive_count source) (G.primitive_count target)
    in
    let target_topology = G.topology target and source_topology = G.topology source in
    (* Explicit CSR is checked even if there are no source edge groups. *)
    let edge_groups =
      match List.assoc_opt "edges" args with
      | None when G.edge_groups source = [] -> []
      | None when source_topology == target_topology -> G.edge_groups source
      | None -> refuse "E_REMAP_EDGES" "Remap changed topology requires explicit edge ancestry."
      | Some (E.Record fields) -> (
          match fields with
          | [ ("offsets", E.Int_array offsets); ("values", E.Int_array ancestors) ]
          | [ ("values", E.Int_array ancestors); ("offsets", E.Int_array offsets) ] ->
              if G.edge_groups source = [] then []
              else
                let source_index = Rdk.Topology_index.create ?cancel source_topology
                and target_index = Rdk.Topology_index.create ?cancel target_topology in
                result "E_REMAP_EDGES"
                  (R.edge_groups_from_ancestry ?cancel ~grain ~source_index ~target_topology
                     ~target_index ~offsets ~ancestors (G.edge_groups source))
          | _ -> refuse "E_REMAP_EDGES" "Remap edges requires offsets and values integer arrays.")
      | Some _ -> refuse "E_REMAP_EDGES" "Remap edges requires offsets and values integer arrays."
    in
    let identity map =
      let same = ref true in
      Array.iteri
        (fun i value ->
          if i land 4095 = 0 then check ();
          if i <> value then same := false)
        map;
      !same
    in
    let same carry count =
      match carry with
      | `Plain map -> Array.length map = count && identity map
      | `Weighted _ -> false
    in
    let point_same = same point_carry (G.point_count source)
    and vertex_same = same vertex_carry (G.vertex_count source)
    and primitive_same = same primitive_carry (G.primitive_count source) in
    let attributes =
      List.map
        (fun attribute ->
          check ();
          match Rdk.Attribute.owner attribute with
          | Detail -> attribute
          | Point when point_same -> attribute
          | Vertex when vertex_same -> attribute
          | Primitive when primitive_same -> attribute
          | owner -> (
              match
                match owner with
                | Point -> point_carry
                | Vertex -> vertex_carry
                | _ -> primitive_carry
              with
              | `Plain map -> R.attribute ?cancel ~grain ~defaults:true map attribute
              | `Weighted (a, b, t) -> R.blend_attribute ?cancel ~grain a b t attribute))
        (G.attributes source)
    in
    let groups =
      List.map
        (fun group ->
          check ();
          match Rdk.Group.owner group with
          | Point when point_same -> group
          | Vertex when vertex_same -> group
          | Primitive when primitive_same -> group
          | owner -> (
              match
                match owner with
                | Point -> point_carry
                | Vertex -> vertex_carry
                | _ -> primitive_carry
              with
              | `Plain map -> R.group ?cancel ~grain map group
              | `Weighted (a, b, t) -> R.blend_group ?cancel ~grain a b t group))
        (G.groups source)
    in
    let positions =
      if not positions then G.positions target
      else if point_same then G.positions source
      else
        match point_carry with
        | `Weighted (a, b, t) -> R.blend_positions ?cancel ~grain a b t (G.positions source)
        | `Plain point_map ->
            let p = Rdk.Packed.Float3.View.view (G.positions source) in
            Rdk.Packed.Float3.View.of_owned_exn
              ~x:(R.select_float ?cancel ~grain ~missing:0. point_map p.x)
              ~y:(R.select_float ?cancel ~grain ~missing:0. point_map p.y)
              ~z:(R.select_float ?cancel ~grain ~missing:0. point_map p.z)
    in
    (* [:lead true]: attributes already on the target come first, ahead of the carried ones. *)
    let attributes =
      match List.assoc_opt "lead" args with
      | Some (E.Bool true) -> G.attributes target @ attributes
      | None | Some (E.Bool false) -> attributes
      | _ -> refuse "E_REMAP_LEAD" "Remap lead is a Boolean."
    in
    (* [:lead_groups true]: the groups and edge groups already on the target come first. *)
    let groups, edge_groups =
      match List.assoc_opt "lead_groups" args with
      | Some (E.Bool true) -> (G.groups target @ groups, G.edge_groups target @ edge_groups)
      | None | Some (E.Bool false) -> (groups, edge_groups)
      | _ -> refuse "E_REMAP_LEAD" "Remap lead_groups is a Boolean."
    in
    check ();
    let output =
      result "E_REMAP"
        (G.create ~positions ~topology:target_topology ~attributes ~groups ~edge_groups ())
    in
    check ();
    Ok output
  with Diagnostic.Error diagnostic -> Error diagnostic

(* Native [merge] on a list of cooked geometries: the same [Rdk.Mesh_merge.run],
   so attribute and group policies, errors and provenance tagging are native's. *)
let concat ?cancel ~grain ~sources args =
  let text name =
    match List.assoc_opt name args with
    | None -> Ok ""
    | Some (E.Text value) -> Ok (String.trim value)
    | Some _ -> error "E_CONCAT_TYPE" ("Concat " ^ name ^ " must be text.")
  in
  let base =
    match List.assoc_opt "source_base" args with
    | None -> Ok 0
    | Some (E.Int value) -> Ok value
    | Some _ -> error "E_CONCAT_TYPE" "Concat source_base must be an integer."
  in
  let tag_base =
    match List.assoc_opt "tag_base" args with
    | None -> Ok (-1)
    | Some (E.Int value) -> Ok value
    | Some _ -> error "E_CONCAT_TYPE" "Concat tag_base must be an integer."
  in
  Result.bind (text "source_attribute") (fun attribute ->
      Result.bind base (fun source_base ->
          Result.bind tag_base (fun tag_base ->
              if grain <= 0 then error "E_CONCAT" "Concat grain must be positive."
              else
                Result.map_error
                  (fun e ->
                    Diagnostic.error ~code:(Rdk.Error.code e) ~cause:(Rdk.Error.to_string e)
                      ~hints:(Rdk.Error.hints e)
                      (Rdk.Error.operation e ^ " could not produce valid geometry"))
                  (Rdk.Mesh_merge.run_tagged
                     {
                       cancel;
                       grain = Some grain;
                       tag_base;
                       source_attribute = (if attribute = "" then None else Some attribute);
                       source_base;
                     }
                     sources))))
