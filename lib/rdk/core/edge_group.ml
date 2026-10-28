
type t = {
  topology_id : int;
  name : string;
  length : int;
  bits : bytes;
  data_id : int;
}
type edge_group = t

let byte_count length = (length + 7) / 8
let require_name name =
  if String.trim name = "" then invalid_arg "Edge_group: empty name"
let require_index topology index =
  if Topology.data_id topology <> Topology_index.topology_data_id index then
    invalid_arg "Edge_group: topology index belongs to a different topology"
let name value = value.name
let topology_data_id value = value.topology_id
let length value = value.length
let data_id value = value.data_id
let with_name name value =
  require_name name;
  if String.equal name value.name then value
  else { value with name; data_id = Data_id.fresh () }
let payload_bytes value = Bytes.length value.bits
let mem edge value =
  edge >= 0 && edge < value.length
  && Char.code (Bytes.get value.bits (edge lsr 3))
       land (1 lsl (edge land 7)) <> 0

let init ?(grain = 4096) ~topology ~index ~name predicate =
  require_name name;
  require_index topology index;
  if grain <= 0 then invalid_arg "Edge_group.init: grain must be positive";
  let length = Topology_index.edge_count index in
  let bytes_count = byte_count length in
  let bits = Bytes.make bytes_count '\000' in
  if bytes_count > 0 then
    Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(bytes_count - 1) (fun byte ->
        let base = byte * 8 and value = ref 0 in
        for bit = 0 to min 7 (length - base - 1) do
          if predicate (base + bit) then value := !value lor (1 lsl bit)
        done;
        Bytes.set bits byte (Char.chr !value));
  { topology_id = Topology.data_id topology; name; length; bits;
    data_id = Data_id.fresh () }

let cardinality value =
  let popcount byte =
    let byte = ref byte and count = ref 0 in
    while !byte <> 0 do
      byte := !byte land (!byte - 1);
      incr count
    done;
    !count in
  let count = ref 0 in
  Bytes.iter (fun byte -> count := !count + popcount (Char.code byte)) value.bits;
  !count

let combine operation left right =
  if left.topology_id <> right.topology_id || left.length <> right.length then
    Error "Edge_group: topology identities and lengths must match"
  else
    let count = Bytes.length left.bits in
    let bits = Bytes.make count '\000' in
    if count > 0 then
      Parallel.for_ ~chunk_size:4096 ~start:0 ~finish:(count - 1) (fun index ->
        Bytes.unsafe_set bits index
          (Char.chr (operation
            (Char.code (Bytes.unsafe_get left.bits index))
            (Char.code (Bytes.unsafe_get right.bits index)))));
    Ok { left with bits; data_id = Data_id.fresh () }
let union = combine (lor)
let intersection = combine (land)
let difference = combine (fun left right -> left land lnot right)
let symmetric_difference = combine (lxor)

let iter operation value =
  for edge = 0 to value.length - 1 do
    if mem edge value then operation edge
  done

module Builder = struct
  type nonrec t = {
    topology_id : int;
    name : string;
    length : int;
    bits : bytes;
    mutable frozen : bool;
  }
  let create ~topology ~index ~name =
    require_name name;
    require_index topology index;
    let length = Topology_index.edge_count index in
    { topology_id = Topology.data_id topology; name; length;
      bits = Bytes.make (byte_count length) '\000'; frozen = false }
  let set value edge member =
    if value.frozen then invalid_arg "Edge_group.Builder.set: builder is frozen";
    if edge < 0 || edge >= value.length then
      invalid_arg "Edge_group.Builder.set: invalid edge";
    let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
    let current = Char.code (Bytes.get value.bits byte) in
    Bytes.set value.bits byte
      (Char.chr (if member then current lor mask else current land lnot mask))
  let freeze value =
    if value.frozen then invalid_arg "Edge_group.Builder.freeze: already frozen";
    value.frozen <- true;
    { topology_id = value.topology_id; name = value.name; length = value.length;
      bits = value.bits; data_id = Data_id.fresh () }
end

module View = struct
  let of_owned_bits ~topology ~edge_count ~name bits =
    require_name name;
    if edge_count < 0 || Bytes.length bits <> byte_count edge_count then
      invalid_arg "Edge_group.View.of_owned_bits: invalid packed length";
    if edge_count land 7 <> 0 && Bytes.length bits > 0 then begin
      let valid_mask = (1 lsl (edge_count land 7)) - 1 in
      let last = Bytes.length bits - 1 in
      if Char.code (Bytes.get bits last) land lnot valid_mask <> 0 then
        invalid_arg "Edge_group.View.of_owned_bits: set padding bits"
    end;
    { topology_id = Topology.data_id topology; name; length = edge_count; bits;
      data_id = Data_id.fresh () }

end

let remap ?cancel ~source_index ~target_topology ~target_index ~point_map value =
  if value.topology_id <> Topology_index.topology_data_id source_index then
    Error "Edge_group.remap: source index does not match the group topology"
  else if Topology.data_id target_topology
      <> Topology_index.topology_data_id target_index then
    Error "Edge_group.remap: target index does not match target topology"
  else if Array.length point_map <> Topology_index.point_count source_index then
    Error "Edge_group.remap: point map length does not match source topology"
  else begin
    let builder = Builder.create ~topology:target_topology ~index:target_index
        ~name:value.name in
    for edge = 0 to value.length - 1 do
      if edge land 16_383 = 0 then Cancel.check_opt cancel;
      if mem edge value then begin
        let a, b = Topology_index.edge_points source_index edge in
        let a = point_map.(a) and b = point_map.(b) in
        if a >= 0 && b >= 0 then
          match Topology_index.find_edge target_index ~a ~b with
          | None -> ()
          | Some target -> Builder.set builder target true
      end
    done;
    Ok (Builder.freeze builder)
  end

let replicate_exact_copies ?cancel ~source_topology ~source_index
    ~target_topology ~copies value =
  if copies < 0 then Error "Edge_group.replicate_exact_copies: negative copy count"
  else if Topology.data_id source_topology
      <> Topology_index.topology_data_id source_index
      || value.topology_id <> Topology.data_id source_topology then
    Error "Edge_group.replicate_exact_copies: source topology/index/group mismatch"
  else
    let source = Topology.View.view source_topology
    and target = Topology.View.view target_topology in
    let source_vertices = Array.length source.vertex_points
    and source_primitives = Bytes.length source.primitive_kinds in
    let expected_points = source.point_count * copies
    and expected_vertices = source_vertices * copies
    and expected_primitives = source_primitives * copies in
    if copies <> 0
        && (expected_points / copies <> source.point_count
            || expected_vertices / copies <> source_vertices
            || expected_primitives / copies <> source_primitives) then
      Error "Edge_group.replicate_exact_copies: output cardinality overflow"
    else if target.point_count <> expected_points
        || Array.length target.vertex_points <> expected_vertices
        || Bytes.length target.primitive_kinds <> expected_primitives then
      Error "Edge_group.replicate_exact_copies: target cardinality mismatch"
    else begin
      let valid = ref (Array.length target.primitive_offsets
        = expected_primitives + 1) in
      let copy = ref 0 in
      while !valid && !copy < copies do
        if !copy land 255 = 0 then Cancel.check_opt cancel;
        let point_offset = !copy * source.point_count
        and vertex_offset = !copy * source_vertices
        and primitive_offset = !copy * source_primitives in
        let vertex = ref 0 in
        while !valid && !vertex < source_vertices do
          valid := target.vertex_points.(vertex_offset + !vertex)
            = source.vertex_points.(!vertex) + point_offset;
          incr vertex
        done;
        let primitive = ref 0 in
        while !valid && !primitive < source_primitives do
          valid := target.primitive_offsets.(primitive_offset + !primitive)
              = source.primitive_offsets.(!primitive) + vertex_offset
            && Bytes.get target.primitive_kinds (primitive_offset + !primitive)
              = Bytes.get source.primitive_kinds !primitive;
          incr primitive
        done;
        incr copy
      done;
      if !valid then
        valid := target.primitive_offsets.(expected_primitives) = expected_vertices;
      if not !valid then
        Error "Edge_group.replicate_exact_copies: target is not exact copy-major topology"
      else begin
        let source_edges = Topology_index.edge_count source_index in
        if value.length <> source_edges then
          Error "Edge_group.replicate_exact_copies: source edge count mismatch"
        else
          let length = source_edges * copies in
          if copies <> 0 && length / copies <> source_edges then
            Error "Edge_group.replicate_exact_copies: edge cardinality overflow"
          else begin
            let bits = Bytes.make (byte_count length) '\000' in
            for edge = 0 to source_edges - 1 do
              if edge land 16_383 = 0 then Cancel.check_opt cancel;
              if mem edge value then
                for copy = 0 to copies - 1 do
                  let target_edge = (copy * source_edges) + edge in
                  let byte = target_edge lsr 3
                  and mask = 1 lsl (target_edge land 7) in
                  Bytes.set bits byte (Char.chr
                    (Char.code (Bytes.get bits byte) lor mask))
                done
            done;
            Ok { topology_id = Topology.data_id target_topology;
              name = value.name; length; bits; data_id = Data_id.fresh () }
          end
      end
    end
