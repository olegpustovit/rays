
type owner = Points | Primitives

type key =
  | X | Y | Z
  | Distance_to of Rays_math.Vec3.t
  | Along_vector of Rays_math.Vec3.t
  | Attribute_component of { name : string; component : int }
  | By_vertex_order
  | By_primitive_index
  | Spatial_locality
  | Random of int64
  | Index_attribute of string
  | Reverse
  | Shift of int


let fail message = raise (Error.Fail message)
let get_ok = function Ok value -> value | Error message -> fail message

let count geometry = function
  | Points -> Geometry.point_count geometry
  | Primitives -> Geometry.primitive_count geometry

let group_owner = function Points -> Group.Point | Primitives -> Group.Primitive

let remap_attribute ?cancel ?(grain = 16_384) point_map vertex_map
    primitive_map attribute =
  let mapping = match Attribute.owner attribute with
    | Attribute.Point -> Some point_map
    | Attribute.Vertex -> Some vertex_map
    | Attribute.Primitive -> Some primitive_map
    | Attribute.Detail -> None in
  match mapping with
  | None -> attribute
  | Some mapping -> Topology_remap.attribute ?cancel ~grain mapping attribute

let remap_group ?cancel ?(grain = 16_384) point_map vertex_map
    primitive_map group =
  let mapping = match Group.owner group with
    | Group.Point -> point_map
    | Group.Vertex -> vertex_map
    | Group.Primitive -> primitive_map in
  Topology_remap.group ?cancel ~grain mapping group

let apply_points ?cancel ?grain permutation geometry =
  let source_topology = Geometry.topology geometry in
  let point_count = Geometry.point_count geometry in
  let inverse = Array.make point_count 0 in
  Array.iteri (fun output source -> inverse.(source) <- output) permutation;
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:(Topology_remap.select_float ?cancel
        ~grain:(Option.value ~default:16_384 grain) permutation positions.x)
      ~y:(Topology_remap.select_float ?cancel
        ~grain:(Option.value ~default:16_384 grain) permutation positions.y)
      ~z:(Topology_remap.select_float ?cancel
        ~grain:(Option.value ~default:16_384 grain) permutation positions.z) in
  let topology = Topology.Private.view source_topology in
  let vertex_points = Array.make (Array.length topology.vertex_points) 0 in
  if Array.length vertex_points > 0 then Parallel.for_ ?chunk_size:grain ~start:0
      ~finish:(Array.length vertex_points - 1) (fun vertex ->
        if vertex land 4095 = 0 then Cancel.check_opt cancel;
        vertex_points.(vertex) <- inverse.(topology.vertex_points.(vertex)));
  let topology = Topology.Private.create_validated_owned ~point_count
      ~vertex_points ~primitive_offsets:(Array.copy topology.primitive_offsets)
      ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
  let attributes = List.map (fun attribute ->
      if Attribute.owner attribute = Attribute.Point then
        remap_attribute ?cancel ?grain permutation [||] [||] attribute
      else attribute) (Geometry.attributes geometry)
  and groups = List.map (fun group ->
      if Group.owner group = Group.Point then
        remap_group ?cancel ?grain permutation [||] [||] group
      else group) (Geometry.groups geometry) in
  let edge_groups = Topology_remap.edge_groups ?cancel ~source_topology
      ~target_topology:topology ~point_map:inverse (Geometry.edge_groups geometry) |> get_ok in
  Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups () |> get_ok

let apply_primitives ?cancel ?(grain = 16_384) permutation geometry =
  let source_topology = Geometry.topology geometry in
  let source = Topology.Private.view source_topology in
  let primitive_count = Array.length permutation in
  let offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    let source_primitive = permutation.(primitive) in
    offsets.(primitive + 1) <- offsets.(primitive)
      + source.primitive_offsets.(source_primitive + 1)
      - source.primitive_offsets.(source_primitive)
  done;
  let vertex_count = offsets.(primitive_count) in
  let vertex_points = Array.make vertex_count 0
  and vertex_map = Array.make vertex_count 0
  and kinds = Bytes.make primitive_count '\000' in
  if primitive_count > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 8))
      ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
        if primitive land 4095 = 0 then Cancel.check_opt cancel;
        let source_primitive = permutation.(primitive)
        and output_first = offsets.(primitive) in
        let source_first = source.primitive_offsets.(source_primitive)
        and source_last = source.primitive_offsets.(source_primitive + 1) in
        Bytes.set kinds primitive (Bytes.get source.primitive_kinds source_primitive);
        for source_vertex = source_first to source_last - 1 do
          let output_vertex = output_first + source_vertex - source_first in
          vertex_points.(output_vertex) <- source.vertex_points.(source_vertex);
          vertex_map.(output_vertex) <- source_vertex
        done);
  let topology = Topology.Private.create_validated_owned
      ~point_count:(Geometry.point_count geometry) ~vertex_points
      ~primitive_offsets:offsets ~primitive_kinds:kinds in
  let attributes = List.map (fun attribute ->
      match Attribute.owner attribute with
      | Attribute.Vertex | Attribute.Primitive ->
          remap_attribute ?cancel ~grain [||] vertex_map permutation attribute
      | Attribute.Point | Attribute.Detail -> attribute)
      (Geometry.attributes geometry)
  and groups = List.map (fun group -> match Group.owner group with
      | Group.Vertex | Group.Primitive ->
          remap_group ?cancel ~grain [||] vertex_map permutation group
      | Group.Point -> group) (Geometry.groups geometry) in
  let edge_groups = Topology_remap.edge_groups ?cancel ~source_topology
      ~target_topology:topology
      ~point_map:(Array.init (Geometry.point_count geometry) Fun.id)
      (Geometry.edge_groups geometry) |> get_ok in
  Geometry.create ~positions:(Geometry.positions geometry) ~topology
    ~attributes ~groups ~edge_groups () |> get_ok

let primitive_centers ?cancel ~grain geometry =
  let topology = Topology.Private.view (Geometry.topology geometry)
  and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Geometry.primitive_count geometry in
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  if count > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 8)) ~start:0
      ~finish:(count - 1) (fun primitive ->
        if primitive land 4095 = 0 then Cancel.check_opt cancel;
        let first = topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1) in
        let sx = ref 0. and sy = ref 0. and sz = ref 0. in
        for vertex = first to last - 1 do
          let point = topology.vertex_points.(vertex) in
          sx := !sx +. positions.x.(point); sy := !sy +. positions.y.(point);
          sz := !sz +. positions.z.(point)
        done;
        let scale = 1. /. float_of_int (last - first) in
        x.(primitive) <- !sx *. scale; y.(primitive) <- !sy *. scale;
        z.(primitive) <- !sz *. scale);
  x, y, z

let finite_vec3 value = Float.is_finite value.Rays_math.Vec3.x
  && Float.is_finite value.y && Float.is_finite value.z

let point_vertex_order ?cancel geometry =
  let topology = Topology.Private.view (Geometry.topology geometry) in
  let values = Array.make (Geometry.point_count geometry) (-1)
  and next = ref 0 in
  for vertex = 0 to Array.length topology.vertex_points - 1 do
    if vertex land 16_383 = 0 then Cancel.check_opt cancel;
    let point = topology.vertex_points.(vertex) in
    if values.(point) < 0 then begin
      values.(point) <- !next;
      incr next
    end
  done;
  values

let point_primitive_index ?cancel geometry =
  let topology = Topology.Private.view (Geometry.topology geometry) in
  let values = Array.make (Geometry.point_count geometry) (-1) in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    if primitive land 16_383 = 0 then Cancel.check_opt cancel;
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      let point = topology.vertex_points.(vertex) in
      if values.(point) < 0 then values.(point) <- primitive
    done
  done;
  values

let[@inline] morton_spread value =
  let open Int64 in
  let value = logand (of_int value) 0x1fffffL in
  let value = logand (logor value (shift_left value 32)) 0x1f00000000ffffL in
  let value = logand (logor value (shift_left value 16)) 0x1f0000ff0000ffL in
  let value = logand (logor value (shift_left value 8)) 0x100f00f00f00f00fL in
  let value = logand (logor value (shift_left value 4)) 0x10c30c30c30c30c3L in
  logand (logor value (shift_left value 2)) 0x1249249249249249L

let spatial_locality_values ?cancel ~grain x y z =
  let count = Array.length x in
  let min_x = ref Float.infinity and min_y = ref Float.infinity
  and min_z = ref Float.infinity and max_x = ref Float.neg_infinity
  and max_y = ref Float.neg_infinity and max_z = ref Float.neg_infinity in
  for index = 0 to count - 1 do
    if index land 16_383 = 0 then Cancel.check_opt cancel;
    let x = x.(index) and y = y.(index) and z = z.(index) in
    if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
      fail "sort spatial coordinates must be finite";
    if x < !min_x then min_x := x; if x > !max_x then max_x := x;
    if y < !min_y then min_y := y; if y > !max_y then max_y := y;
    if z < !min_z then min_z := z; if z > !max_z then max_z := z
  done;
  let quantize minimum maximum value =
    if minimum = maximum then 0
    else
      let scale = max (abs_float minimum) (abs_float maximum) in
      let lower = minimum /. scale and upper = maximum /. scale in
      let extent = upper -. lower in
      let normalized = if scale = 0. || extent = 0. then 0.
        else ((value /. scale) -. lower) /. extent in
      int_of_float (floor ((max 0. (min 1. normalized) *. 2_097_151.) +. 0.5)) in
  let values = Array.make count 0L in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun index ->
        if index land 4095 = 0 then Cancel.check_opt cancel;
        let x = morton_spread (quantize !min_x !max_x x.(index))
        and y = morton_spread (quantize !min_y !max_y y.(index))
        and z = morton_spread (quantize !min_z !max_z z.(index)) in
        values.(index) <- Int64.logor x
            (Int64.logor (Int64.shift_left y 1) (Int64.shift_left z 2)));
  values

let component_values ?cancel ~grain owner key geometry =
  let coordinates () = match owner with
    | Points ->
        let view = Packed.Float3.Private.view (Geometry.positions geometry) in
        view.x, view.y, view.z
    | Primitives -> primitive_centers ?cancel ~grain geometry in
  match key with
  | X | Y | Z as axis ->
      let x, y, z = coordinates () in
      `Float (match axis with X -> x | Y -> y | Z -> z | _ -> assert false)
  | Distance_to point ->
      if not (finite_vec3 point) then fail "sort reference point must be finite";
      let x, y, z = coordinates () in
      `Float (Array.init (Array.length x) (fun index ->
        let dx = x.(index) -. point.Rays_math.Vec3.x and dy = y.(index) -. point.y
        and dz = z.(index) -. point.z in
        (dx *. dx) +. (dy *. dy) +. (dz *. dz)))
  | Along_vector vector ->
      if not (finite_vec3 vector) || Rays_math.Vec3.length_sq vector <= 1e-30 then
        fail "sort vector must be finite and non-zero";
      let vector = Rays_math.Vec3.normalize vector in
      let x, y, z = coordinates () in
      `Float (Array.init (Array.length x) (fun index -> (x.(index) *. vector.x)
        +. (y.(index) *. vector.y) +. (z.(index) *. vector.z)))
  | Attribute_component { name; component } ->
      let attribute_owner = match owner with Points -> Attribute.Point
        | Primitives -> Attribute.Primitive in
      let attribute = Geometry.find_attribute ~owner:attribute_owner name geometry
        |> Option.to_result ~none:("missing sort attribute " ^ name) |> get_ok in
      let invalid width = if component < 0 || component >= width then
          fail "sort attribute component is out of range" in
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> invalid 1; `Float values
       | Attribute.Int values -> invalid 1; `Int values
       | Attribute.Int_array _ | Attribute.Float_array _ -> fail
           "sort key attribute cannot use array storage"
       | Attribute.Text values -> invalid 1; `Text values
       | Attribute.Float2 values -> invalid 2;
           let v = Packed.Float2.Private.view values in `Float (if component = 0 then v.x else v.y)
       | Attribute.Float3 values -> invalid 3;
           let v = Packed.Float3.Private.view values in
           `Float (if component = 0 then v.x else if component = 1 then v.y else v.z)
       | Attribute.Float4 values -> invalid 4;
           let v = Packed.Float4.Private.view values in
           `Float (if component = 0 then v.x else if component = 1 then v.y
             else if component = 2 then v.z else v.w))
  | By_vertex_order ->
      if owner <> Points then fail "By Vertex Order is only valid for points";
      `Int (point_vertex_order ?cancel geometry)
  | By_primitive_index ->
      if owner <> Points then fail "By Primitive Index is only valid for points";
      `Int (point_primitive_index ?cancel geometry)
  | Spatial_locality ->
      let x, y, z = coordinates () in
      `Int64 (spatial_locality_values ?cancel ~grain x y z)
  | Random _ | Index_attribute _ | Reverse | Shift _ -> assert false

let reverse_in_place values =
  for index = 0 to (Array.length values / 2) - 1 do
    let opposite = Array.length values - 1 - index in
    let value = values.(index) in
    values.(index) <- values.(opposite);
    values.(opposite) <- value
  done

let integer_attribute owner name geometry =
  let attribute_owner = match owner with Points -> Attribute.Point
    | Primitives -> Attribute.Primitive in
  match Geometry.find_attribute ~owner:attribute_owner name geometry with
  | None -> fail ("missing sort index attribute " ^ name)
  | Some attribute -> match Attribute.Private.storage attribute with
    | Attribute.Int values -> values
    | _ -> fail ("sort index attribute " ^ name ^ " must be integer")

let order_from_indices ?selection ~count ~sources values =
  if Array.length values <> count then
    fail "sort index attribute length mismatch";
  let source_count = Array.length sources in
  let local_of_slot = Array.make count (-1) in
  Array.iteri (fun local slot -> local_of_slot.(slot) <- local) sources;
  let ordered = Array.make source_count 0
  and seen = Bytes.make source_count '\000' in
  Array.iter (fun source ->
    let target = values.(source) in
    let local = if target < 0 || target >= count then -1
      else local_of_slot.(target) in
    if local < 0 || Bytes.get seen local <> '\000' then
      fail (match selection with
        | None -> "sort index attribute must be a permutation of 0..N-1"
        | Some _ ->
            "restricted sort index attribute must be a permutation of selected slots");
    Bytes.set seen local '\001';
    ordered.(local) <- source) sources;
  ordered

let install_order target source =
  Array.blit source 0 target 0 (Array.length source)

let randomize_in_place ?cancel seed values =
  let random = Rays_math.Rand.seed64 seed in
  for index = Array.length values - 1 downto 1 do
    if index land 4095 = 0 then Cancel.check_opt cancel;
    let unit = Rays_math.Rand.float_at random ~index:(index lxor 0x6c8e9cf5) in
    let other = int_of_float (unit *. float_of_int (index + 1)) in
    let value = values.(index) in
    values.(index) <- values.(other);
    values.(other) <- value
  done

let sort ?cancel ?(grain = 16_384) ?selection ?(descending = false)
    ?output_indices ?(combine_indices = false) ~owner ~key geometry =
  Rdk_core.Error.guard ~operation:"sort" ~code:"invalid_sort" @@ fun () ->
  try
    if grain <= 0 then invalid_arg "Rdk.Ordering.sort: grain must be positive";
    Cancel.check_opt cancel;
    let count = count geometry owner in
    (match selection with
     | Some group when Group.owner group <> group_owner owner
         || Group.length group <> count -> fail "sort selection owner/length mismatch"
     | None | Some _ -> ());
    if combine_indices && output_indices = None then
      fail "combine_indices requires output_indices";
    (match key with
     | Reverse | Shift _ | Random _ | Index_attribute _ when combine_indices ->
         fail "combine_indices requires a value-based sort key"
     | _ -> ());
    Option.iter (fun name ->
      if String.trim name = "" || name = "P" then
        fail "sort output index attribute name must be non-empty and not P")
      output_indices;
    (match key, selection with
     | Random _, Some _ -> fail "random sort does not support a restricted group"
     | _ -> ());
    let slots = match selection with
      | None -> [||]
      | Some group ->
          let values = Array.make (Group.cardinality group) 0 and next = ref 0 in
          Group.iter (fun index -> values.(!next) <- index; incr next) group; values in
    let sources = match selection with
      | None -> Array.init count Fun.id
      | Some _ -> Array.copy slots in
    (match output_indices with
     | Some name when combine_indices ->
         let values = integer_attribute owner name geometry in
         install_order sources
           (order_from_indices ?selection ~count ~sources values)
     | None | Some _ -> ());
    (match key with
     | Reverse ->
         reverse_in_place sources;
         if descending then reverse_in_place sources
     | Shift offset when Array.length sources > 0 ->
         let length = Array.length sources in
         let offset = ((offset mod length) + length) mod length in
         let original = Array.copy sources in
         for index = 0 to length - 1 do
           let source = ((index - offset) + length) mod length in
           sources.(index) <- original.(source)
         done;
         if descending then reverse_in_place sources
     | Shift _ -> ()
     | Random seed ->
         randomize_in_place ?cancel seed sources;
         if descending then reverse_in_place sources
     | Index_attribute name ->
         install_order sources (order_from_indices ?selection ~count ~sources
           (integer_attribute owner name geometry));
         if descending then reverse_in_place sources
     | key ->
         let values = component_values ?cancel ~grain owner key geometry in
         (match values with
          | `Float values -> Array.iter (fun source ->
              if not (Float.is_finite values.(source)) then
                fail "sort keys must be finite") sources
          | `Int _ | `Int64 _ | `Text _ -> ());
         let compare left right =
           let compared = match values with
             | `Float values -> Float.compare values.(left) values.(right)
             | `Int values -> Int.compare values.(left) values.(right)
             | `Int64 values -> Int64.compare values.(left) values.(right)
             | `Text values -> String.compare values.(left) values.(right) in
           if descending then -compared else compared in
         Array.stable_sort compare sources);
    let permutation = match selection with
      | None -> sources
      | Some _ ->
          let permutation = Array.init count Fun.id in
          Array.iteri (fun index slot -> permutation.(slot) <- sources.(index)) slots;
          permutation in
    match output_indices with
    | None -> Ok (match owner with
        | Points -> apply_points ?cancel ~grain permutation geometry
        | Primitives -> apply_primitives ?cancel ~grain permutation geometry)
    | Some name ->
        let ranks = Array.make count 0 in
        if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(count - 1) (fun target ->
              if target land 4095 = 0 then Cancel.check_opt cancel;
              ranks.(permutation.(target)) <- target);
        let attribute_owner = match owner with Points -> Attribute.Point
          | Primitives -> Attribute.Primitive in
        let attribute = Attribute.create_owned ~name ~owner:attribute_owner
            (Attribute.Int ranks) |> get_ok in
        Geometry.with_attribute attribute geometry
  with Error.Fail message | Invalid_argument message -> Error message
