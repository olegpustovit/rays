
(* [missing] is the value of a row whose source index is -1 (a created element); without it a
   negative index is out of bounds, as it always was. *)
let select ?cancel ~grain ?missing mapping source =
  let count = Array.length mapping in
  if count = 0 then [||]
  else begin
    let output = Array.make count (match missing with
      | Some value -> value | None -> source.(mapping.(0))) in
    let ranges = ((count - 1) / grain) + 1 in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let first = range * grain in
      let last = min count (first + grain) in
      for index = first to last - 1 do
        if index land 4095 = 0 then Cancel.check_opt cancel;
        let from = mapping.(index) in
        if from >= 0 || missing = None then output.(index) <- source.(from)
      done);
    output
  end

let select_float ?cancel ~grain ?missing mapping (source : float array) =
  let count = Array.length mapping in
  let output = Array.make count (Option.value missing ~default:0.) in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(count - 1) (fun index ->
    if index land 4095 = 0 then Cancel.check_opt cancel;
    let from = mapping.(index) in
    if from >= 0 || missing = None then output.(index) <- source.(from));
  output

let attribute_result ?cancel ~grain ?(defaults = false) mapping attribute =
  let select ?cancel ~grain ~missing mapping source =
    if defaults then select ?cancel ~grain ~missing mapping source else select ?cancel ~grain mapping source
  and select_float ?cancel ~grain mapping source =
    if defaults then select_float ?cancel ~grain ~missing:0. mapping source else select_float ?cancel ~grain mapping source in
  let storage = match Attribute.View.storage attribute with
    | Attribute.Float values -> Attribute.Float (select_float ?cancel ~grain mapping values)
    | Attribute.Int values -> Attribute.Int (select ?cancel ~grain ~missing:0 mapping values)
    | Attribute.Text values -> Attribute.Text (select ?cancel ~grain ~missing:"" mapping values)
    | Attribute.Int_array values ->
        Attribute.Int_array (Ragged_ops.remap_int ?cancel ~grain mapping values)
    | Attribute.Float_array values ->
        Attribute.Float_array (Ragged_ops.remap_float ?cancel ~grain mapping values)
    | Attribute.Float2 values ->
        let values = Packed.Float2.View.view values in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(select_float ?cancel ~grain mapping values.x)
          ~y:(select_float ?cancel ~grain mapping values.y) |> Result.get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.View.view values in
        Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:(select_float ?cancel ~grain mapping values.x)
          ~y:(select_float ?cancel ~grain mapping values.y)
          ~z:(select_float ?cancel ~grain mapping values.z))
    | Attribute.Float4 values ->
        let values = Packed.Float4.View.view values in
        Attribute.Float4 (Packed.Float4.of_owned
          ~x:(select_float ?cancel ~grain mapping values.x)
          ~y:(select_float ?cancel ~grain mapping values.y)
          ~z:(select_float ?cancel ~grain mapping values.z)
          ~w:(select_float ?cancel ~grain mapping values.w) |> Result.get_ok) in
  Attribute.create_owned ~name:(Attribute.name attribute)
    ~owner:(Attribute.owner attribute) storage

let attribute ?cancel ~grain ?defaults mapping value =
  attribute_result ?cancel ~grain ?defaults mapping value |> Result.get_ok

(* Weighted carry: row [i] of a new owner is the mix of source rows [left.(i)] and [right.(i)] at
   [weight.(i)]. Floats and vectors use [a + (b - a) * t], or [a * (1 - t) + b * t] when the delta is
   not finite; a vec3 named [N] is renormalised when its length exceeds 1e-20; everything else
   (Int, Text, ragged rows, group membership) takes [left] when [t < 0.5] and [right] otherwise. *)
let blend_ranges ?cancel ~grain count operation =
  if grain <= 0 then invalid_arg "Topology_remap.blend: grain must be positive";
  let ranges = (count + grain - 1) / grain in
  if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
    Cancel.check_opt cancel;
    operation (range * grain) (min count ((range + 1) * grain)))

let blend_float ?cancel ~grain left right weight source =
  let output = Array.make (Array.length weight) 0. in
  blend_ranges ?cancel ~grain (Array.length output) (fun first last ->
    for index = first to last - 1 do
      let left_value = source.(left.(index)) and right_value = source.(right.(index)) in
      let delta = right_value -. left_value in
      output.(index) <- if Float.is_finite delta
        then left_value +. (delta *. weight.(index))
        else (left_value *. (1. -. weight.(index))) +. (right_value *. weight.(index))
    done);
  output

let nearest_rows left right weight =
  Array.init (Array.length weight) (fun index ->
    (if weight.(index) < 0.5 then left else right).(index))

let blend_attribute ?cancel ~grain left right weight attribute =
  let nearest () = nearest_rows left right weight in
  let float = blend_float ?cancel ~grain left right weight in
  let storage = match Attribute.View.storage attribute with
    | Attribute.Float values -> Attribute.Float (float values)
    | Attribute.Int values ->
        Attribute.Int (if Array.length weight = 0 then [||] else select ?cancel ~grain (nearest ()) values)
    | Attribute.Text values ->
        Attribute.Text (if Array.length weight = 0 then [||] else select ?cancel ~grain (nearest ()) values)
    | Attribute.Int_array values ->
        Attribute.Int_array (Ragged_ops.remap_int ?cancel ~grain (nearest ()) values)
    | Attribute.Float_array values ->
        Attribute.Float_array (Ragged_ops.remap_float ?cancel ~grain (nearest ()) values)
    | Attribute.Float2 values ->
        let values = Packed.Float2.View.view values in
        Attribute.Float2 (Packed.Float2.of_owned ~x:(float values.x) ~y:(float values.y) |> Result.get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.View.view values in
        let x = float values.x and y = float values.y and z = float values.z in
        if String.equal (Attribute.name attribute) "N" then
          blend_ranges ?cancel ~grain (Array.length x) (fun first last ->
            for index = first to last - 1 do
              let length = sqrt (x.(index) *. x.(index) +. y.(index) *. y.(index) +. z.(index) *. z.(index)) in
              if length > 1e-20 then begin
                x.(index) <- x.(index) /. length;
                y.(index) <- y.(index) /. length;
                z.(index) <- z.(index) /. length
              end
            done);
        Attribute.Float3 (Packed.Float3.View.of_owned_exn ~x ~y ~z)
    | Attribute.Float4 values ->
        let values = Packed.Float4.View.view values in
        Attribute.Float4 (Packed.Float4.of_owned ~x:(float values.x) ~y:(float values.y)
          ~z:(float values.z) ~w:(float values.w) |> Result.get_ok) in
  Attribute.create_owned ~name:(Attribute.name attribute)
    ~owner:(Attribute.owner attribute) storage |> Result.get_ok

let drop_point_vertex_normals =
  [Attribute.Point, "N"; Attribute.Vertex, "N"]

let mapped_attribute ?cancel ~grain ~point_map ~vertex_map ~primitive_map
    ~drop value =
  let owner = Attribute.owner value in
  if List.exists (fun (drop_owner, name) ->
      drop_owner = owner && String.equal name (Attribute.name value)) drop
  then Ok None
  else match owner with
    | Attribute.Detail -> Ok (Some value)
    | Attribute.Point ->
        Result.map Option.some (attribute_result ?cancel ~grain point_map value)
    | Attribute.Vertex ->
        Result.map Option.some (attribute_result ?cancel ~grain vertex_map value)
    | Attribute.Primitive ->
        Result.map Option.some (attribute_result ?cancel ~grain primitive_map value)

let group ?cancel ~grain mapping group =
  let output = Group.init ~grain ~owner:(Group.owner group)
      ~name:(Group.name group) (Array.length mapping)
      (fun output ->
        if output land 4095 = 0 then Cancel.check_opt cancel;
        Group.mem mapping.(output) group) in
  Group.View.remap_order ~source:group ~source_of_target:mapping output

let blend_group ?cancel ~grain left right weight value =
  group ?cancel ~grain (nearest_rows left right weight) value

let blend_positions ?cancel ~grain left right weight positions =
  let p = Packed.Float3.View.view positions in
  let blend = blend_float ?cancel ~grain left right weight in
  Packed.Float3.View.of_owned_exn ~x:(blend p.x) ~y:(blend p.y) ~z:(blend p.z)

let mapped_group ?cancel ~grain ~point_map ~vertex_map ~primitive_map value =
  let mapping = match Group.owner value with
    | Group.Point -> point_map
    | Group.Vertex -> vertex_map
    | Group.Primitive -> primitive_map in
  group ?cancel ~grain mapping value

let attributes ?cancel ~grain ~point_map ~vertex_map ~primitive_map geometry =
  List.map (fun value -> match Attribute.owner value with
    | Attribute.Point -> attribute ?cancel ~grain point_map value
    | Attribute.Vertex -> attribute ?cancel ~grain vertex_map value
    | Attribute.Primitive -> attribute ?cancel ~grain primitive_map value
    | Attribute.Detail -> value) (Geometry.attributes geometry)

let groups ?cancel ~grain ~point_map ~vertex_map ~primitive_map geometry =
  List.map (fun value -> match Group.owner value with
    | Group.Point -> group ?cancel ~grain point_map value
    | Group.Vertex -> group ?cancel ~grain vertex_map value
    | Group.Primitive -> group ?cancel ~grain primitive_map value)
    (Geometry.groups geometry)

let edge_groups ?cancel ~source_topology ~target_topology ~point_map groups =
  match groups with
  | [] -> Ok []
  | groups ->
      let source_index = Topology_index.create ?cancel source_topology
      and target_index = Topology_index.create ?cancel target_topology in
      let rec loop result = function
        | [] -> Ok (List.rev result)
        | group :: rest ->
            Result.bind (Edge_group.remap ?cancel ~source_index
                ~target_topology ~target_index ~point_map group)
              (fun group -> loop (group :: result) rest) in
      loop [] groups

let preserving_points ?cancel ~grain ~topology ~vertex_map ~primitive_map geometry =
  let source_topology = Geometry.topology geometry in
  if Topology.point_count topology <> Geometry.point_count geometry then
    Error "Topology_remap.preserving_points: point count changed"
  else begin
    let attributes = List.map (fun value -> match Attribute.owner value with
      | Attribute.Point | Attribute.Detail -> value
      | Attribute.Vertex -> attribute ?cancel ~grain vertex_map value
      | Attribute.Primitive -> attribute ?cancel ~grain primitive_map value)
        (Geometry.attributes geometry)
    and groups = List.map (fun value -> match Group.owner value with
      | Group.Point -> value
      | Group.Vertex -> group ?cancel ~grain vertex_map value
      | Group.Primitive -> group ?cancel ~grain primitive_map value)
        (Geometry.groups geometry) in
    let edge_groups = match Geometry.edge_groups geometry with
      | [] -> Ok []
      | values -> edge_groups ?cancel ~source_topology ~target_topology:topology
          ~point_map:(Array.init (Geometry.point_count geometry) Fun.id) values in
    Result.bind edge_groups (fun edge_groups ->
      Geometry.create ~positions:(Geometry.positions geometry) ~topology
        ~attributes ~groups ~edge_groups ())
  end

let split_point_edge_groups ?cancel ~grain ~source_index ~target_topology groups =
  if grain <= 0 then invalid_arg
      "Topology_remap.split_point_edge_groups: grain must be positive";
  match groups with
  | [] -> Ok []
  | groups ->
      let source = Topology_index.View.view source_index
      and target = Topology.View.view target_topology in
      if Array.length target.vertex_points <> Array.length source.next_vertex then
        Error "split-point edge remap requires unchanged corner cardinality"
      else begin
        let affinity_error = List.find_opt (fun group ->
          Edge_group.topology_data_id group
            <> Topology_index.topology_data_id source_index) groups in
        match affinity_error with
        | Some group -> Error (Printf.sprintf
            "split-point edge group %S belongs to a different topology"
            (Edge_group.name group))
        | None ->
            let lookup = Topology_edge_lookup.create ?cancel target in
            let edge_count = Topology_edge_lookup.count lookup in
            let source_of_target = Array.make edge_count (-1)
            and failure = ref None in
            for source_edge = 0 to Array.length source.edge_a - 1 do
              if source_edge land 4_095 = 0 then Cancel.check_opt cancel;
              for at = source.edge_offsets.(source_edge)
                  to source.edge_offsets.(source_edge + 1) - 1 do
                let vertex = source.edge_vertices.(at) in
                let next = source.next_vertex.(vertex) in
                if next < 0 then failure := Some
                    "source topology exposed an incomplete edge"
                else begin
                  let target_edge = Topology_edge_lookup.find lookup
                      ~a:target.vertex_points.(vertex)
                      ~b:target.vertex_points.(next) in
                  if target_edge < 0 then failure := Some
                      "target topology lost a source corner edge"
                  else if source_of_target.(target_edge) >= 0
                      && source_of_target.(target_edge) <> source_edge then
                    failure := Some
                      "target split edge has conflicting source ancestry"
                  else source_of_target.(target_edge) <- source_edge
                end
              done
            done;
            (match !failure with
             | Some message -> Error message
             | None ->
                 let missing = ref (-1) in
                 for edge = 0 to edge_count - 1 do
                   if source_of_target.(edge) < 0 then missing := edge
                 done;
                 if !missing >= 0 then Error (Printf.sprintf
                     "target split edge %d has no source ancestry" !missing)
                 else
                   Ok (List.map (fun group ->
                     let byte_count = (edge_count + 7) / 8 in
                     let bits = Bytes.make byte_count '\000' in
                     if byte_count > 0 then Parallel.for_
                         ~chunk_size:(max 1 (grain / 8)) ~start:0
                         ~finish:(byte_count - 1) (fun byte ->
                           if byte land 511 = 0 then Cancel.check_opt cancel;
                           let value = ref 0 in
                           for bit = 0 to 7 do
                             let edge = (byte lsl 3) + bit in
                             if edge < edge_count
                                 && Edge_group.mem source_of_target.(edge) group
                             then value := !value lor (1 lsl bit)
                           done;
                           Bytes.unsafe_set bits byte (Char.unsafe_chr !value));
                     Edge_group.View.of_owned_bits ~topology:target_topology
                       ~edge_count ~name:(Edge_group.name group) bits) groups))
      end

(* The existing Rewire Vertices edge consumer, with its CSR trust boundary
   made explicit so other exact-ancestry callers can reuse it. *)
let edge_groups_from_ancestry ?cancel ~grain ~source_index ~target_topology
    ~target_index ~offsets ~ancestors groups =
  Cancel.check_opt cancel;
  let target_edges = Topology_index.edge_count target_index
  and source_edges = Topology_index.edge_count source_index in
  if grain <= 0 then Error "Topology_remap.edge_groups_from_ancestry: grain must be positive"
  else if Topology.data_id target_topology <> Topology_index.topology_data_id target_index then
    Error "Topology_remap.edge_groups_from_ancestry: target index affinity mismatch"
  else if Array.length offsets <> target_edges + 1 || offsets.(0) <> 0
      || offsets.(target_edges) <> Array.length ancestors then
    Error "Topology_remap.edge_groups_from_ancestry: invalid offsets"
  else begin
    let invalid = ref false in
    for edge = 0 to target_edges - 1 do
      if edge land 4095 = 0 then Cancel.check_opt cancel;
      if offsets.(edge) < 0 || offsets.(edge + 1) < offsets.(edge) then invalid := true
    done;
    for at = 0 to Array.length ancestors - 1 do
      if at land 4095 = 0 then Cancel.check_opt cancel;
      if ancestors.(at) < 0 || ancestors.(at) >= source_edges then invalid := true
    done;
    if !invalid then Error "Topology_remap.edge_groups_from_ancestry: invalid source ancestry"
    else if List.exists (fun group -> Edge_group.topology_data_id group
        <> Topology_index.topology_data_id source_index || Edge_group.length group <> source_edges) groups then
      Error "Topology_remap.edge_groups_from_ancestry: source group affinity mismatch"
    else Ok (List.map (fun group ->
      let byte_count = (target_edges + 7) / 8 in
      let bits = Bytes.make byte_count '\000' in
      (* Keep the original Rewire byte-loop schedule and cancellation cadence. *)
      Support.parallel_for ?cancel ~grain byte_count (fun byte ->
        let packed = ref 0 in
        for bit = 0 to 7 do
          let edge = (byte lsl 3) + bit in
          if edge < target_edges then begin
            let member = ref false and at = ref offsets.(edge) in
            while not !member && !at < offsets.(edge + 1) do
              if !at land 4095 = 0 then Cancel.check_opt cancel;
              member := Edge_group.mem ancestors.(!at) group;
              incr at
            done;
            if !member then packed := !packed lor (1 lsl bit)
          end
        done;
        Bytes.unsafe_set bits byte (Char.unsafe_chr !packed));
      Edge_group.View.of_owned_bits ~topology:target_topology
        ~edge_count:target_edges ~name:(Edge_group.name group) bits) groups)
  end
