
type scheme = Catmull_clark | Loop | Bilinear
type boundary_interpolation =
  | Subdivide_boundary_none
  | Subdivide_boundary_edge_only
  | Subdivide_boundary_edge_and_corner
type face_varying_interpolation =
  | Subdivide_fvar_none
  | Subdivide_fvar_corners_only
  | Subdivide_fvar_corners_plus1
  | Subdivide_fvar_corners_plus2
  | Subdivide_fvar_boundaries
  | Subdivide_fvar_all
type triangle_policy =
  | Subdivide_triangles_catmull_clark
  | Subdivide_triangles_smooth
type creasing_method =
  | Subdivide_creasing_uniform
  | Subdivide_creasing_chaikin
type crack_policy =
  | Subdivide_do_not_close
  | Subdivide_pull_no_edge_division
  | Subdivide_pull_divide_edges of float
  | Subdivide_pull_triangulate of float
  | Subdivide_stitch_no_edge_division
  | Subdivide_stitch_divide_edges
  | Subdivide_stitch_triangulate

let edge_ancestry_prefix = "__rdk_subdivide_edge_ancestry_"


let fail message = raise (Error.Fail ("Rdk_mesh.Subdivide.subdivide: " ^ message))
let get_ok = function Ok value -> value | Error message -> fail message

let checked_add name a b =
  if b < 0 || a > max_int - b then fail (name ^ " exceeds integer limits");
  a + b

let checked_mul name a b =
  if a < 0 || b < 0 || (a <> 0 && b > max_int / a) then
    fail (name ^ " exceeds integer limits");
  a * b

let run ?(grain = 16_384) ?cancel count operation =
  if grain <= 0 then invalid_arg "Rdk_mesh.Subdivide.subdivide: grain must be positive";
  if count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) (fun index ->
      Cancel.poll cancel index 4095;
      operation index)

type plan = {
  scheme : scheme;
  source : Geometry.t;
  source_topology : Topology.View.view;
  index : Topology_index.View.view;
  source_points : int;
  source_vertices : int;
  source_primitives : int;
  edge_count : int;
  face_offset : int;
  output_points : int;
  point_offsets : int array;
  point_sources : int array;
  point_weights : float array;
  point_representative : int array;
  edge_ancestry_attribute : string option;
  boundary_interpolation : boundary_interpolation;
  triangle_subdivision : triangle_policy;
  creasing_method : creasing_method;

  creases : crease_plan option;
  vertex_kind : bytes;
  vertex_a : int array;
  vertex_b : int array;
  vertex_primitive : int array;
  vertex_edge_source : int array;
  primitive_source : int array;
  topology : Topology.t;
}

and crease_plan = {
  method_ : creasing_method;
  edge_sharpness : float array;
  child_edge_sharpness : float array;
  has_edge_creases : bool;

  vertex_masks : vertex_mask_plan;
}

and vertex_mask_plan = {
  parent_kind : bytes;
  child_kind : bytes;
  transition_factor : float array;
  parent_a : int array;
  parent_b : int array;
  child_a : int array;
  child_b : int array;
}

let float_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | None -> None
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> Some values
       | _ -> fail (Printf.sprintf "%s %s must use float storage"
           (match owner with Attribute.Point -> "point" | Attribute.Vertex -> "vertex"
            | Attribute.Primitive -> "primitive" | Attribute.Detail -> "detail") name))

let validate_sharpness name values =
  Array.iter (fun value ->
    if not (Float.is_finite value) || value < 0. then
      fail (name ^ " values must be finite and non-negative")) values

let crease_data ?edge_override geometry topology index =
  let edge_count = Array.length index.Topology_index.View.edge_a in
  let vertex_weights = float_attribute Attribute.Vertex "creaseweight" geometry
  and primitive_weights = float_attribute Attribute.Primitive "creaseweight" geometry
  and corner_weights = float_attribute Attribute.Point "cornerweight" geometry in
  Option.iter (fun value ->
    if not (Float.is_finite value) || value < 0. then
      fail "crease override must be finite and non-negative") edge_override;
  Option.iter (validate_sharpness "vertex creaseweight") vertex_weights;
  Option.iter (validate_sharpness "primitive creaseweight") primitive_weights;
  Option.iter (validate_sharpness "point cornerweight") corner_weights;
  if edge_override = None && vertex_weights = None && primitive_weights = None
     && corner_weights = None
  then None
  else begin
    let edge_sharpness = match edge_override with
      | Some value -> Array.make edge_count value
      | None ->
          let edge_sharpness = Array.make edge_count 0. in
          Option.iter (fun values ->
            for vertex = 0
                to Array.length topology.Topology.View.vertex_points - 1 do
              let edge = index.edge_of_vertex.(vertex) in
              if edge >= 0 then edge_sharpness.(edge) <-
                  max edge_sharpness.(edge) values.(vertex)
            done) vertex_weights;
          Option.iter (fun values ->
            for primitive = 0
                to Bytes.length topology.Topology.View.primitive_kinds - 1 do
              let weight = values.(primitive) in
              for vertex = topology.primitive_offsets.(primitive)
                  to topology.primitive_offsets.(primitive + 1) - 1 do
                let edge = index.edge_of_vertex.(vertex) in
                if edge >= 0 then edge_sharpness.(edge) <-
                    max edge_sharpness.(edge) weight
              done
            done) primitive_weights;
          edge_sharpness in
    let corner_sharpness = match corner_weights with
      | Some values -> Array.copy values
      | None -> Array.make topology.point_count 0. in
    Some (edge_sharpness, corner_sharpness,
      Option.is_some edge_override || Option.is_some vertex_weights
        || Option.is_some primitive_weights,
      Option.is_some corner_weights)
  end

let same_topology left right =
  left.Topology.View.point_count = right.Topology.View.point_count
  && left.vertex_points = right.vertex_points
  && left.primitive_offsets = right.primitive_offsets
  && Bytes.equal left.primitive_kinds right.primitive_kinds

let validate_crease_selection selection creases =
  if Group.owner selection <> Group.Primitive then
    fail "crease selection must be a primitive group";
  let expected = Geometry.primitive_count creases in
  if Group.length selection <> expected then
    fail (Printf.sprintf
      "crease selection length %d does not match the crease primitive count %d"
      (Group.length selection) expected)

let validate_hole_group holes geometry =
  if Group.owner holes <> Group.Primitive then
    fail "subdivision holes must be a primitive group";
  let expected = Geometry.primitive_count geometry in
  if Group.length holes <> expected then
    fail (Printf.sprintf
      "subdivision hole group length %d does not match the primitive count %d"
      (Group.length holes) expected)

let apply_crease_input ?cancel ?grain ?selection ?override source creases =
  Option.iter (fun value ->
    if not (Float.is_finite value) || value < 0. then
      fail "crease override must be finite and non-negative") override;
  Option.iter (fun selection -> validate_crease_selection selection creases) selection;
  let source_topology = Topology.View.view (Geometry.topology source)
  and crease_topology = Topology.View.view (Geometry.topology creases) in
  let topology_identical = same_topology source_topology crease_topology in
  let crease_vertex_weights = float_attribute Attribute.Vertex
      "creaseweight" creases
  and crease_primitive_weights = float_attribute Attribute.Primitive
      "creaseweight" creases in
  Option.iter (validate_sharpness "second-input vertex creaseweight")
    crease_vertex_weights;
  Option.iter (validate_sharpness "second-input primitive creaseweight")
    crease_primitive_weights;
  if override = None
     && (crease_vertex_weights <> None || crease_primitive_weights <> None)
     && not topology_identical then
    fail "creaseweight attributes require the source and crease inputs to have identical topology";
  let contributes = override <> None || crease_vertex_weights <> None
      || crease_primitive_weights <> None in
  if not contributes then source
  else begin
    let source_index_value = Topology_index.create ?cancel
        (Geometry.topology source) in
    let crease_index_value = if topology_identical then source_index_value
      else Topology_index.create ?cancel (Geometry.topology creases) in
    let source_index = Topology_index.View.view source_index_value
    and crease_index = Topology_index.View.view crease_index_value in
    let source_edge_count = Array.length source_index.edge_a in
    let edge_sharpness = match crease_data source source_topology source_index with
      | Some (values, _, _, _) -> Array.copy values
      | None -> Array.make source_edge_count 0. in
    let assigned = Bytes.make source_edge_count '\000'
    and assigned_weights = Array.make source_edge_count 0. in
    let selected primitive = match selection with
      | None -> true
      | Some selection -> Group.mem primitive selection in
    let contribution vertex primitive = match override with
      | Some weight -> weight
      | None ->
          max
            (match crease_vertex_weights with
             | None -> 0. | Some values -> values.(vertex))
            (match crease_primitive_weights with
             | None -> 0. | Some values -> values.(primitive)) in
    if topology_identical then begin
      if Array.length crease_index.edge_a <> source_edge_count then
        fail "identical crease topology produced a different edge index";
      for edge = 0 to source_edge_count - 1 do
        if crease_index.edge_a.(edge) <> source_index.edge_a.(edge)
           || crease_index.edge_b.(edge) <> source_index.edge_b.(edge) then
          fail "identical crease topology produced unstable edge numbering"
      done;
      run ?grain ?cancel source_edge_count (fun edge ->
        for at = crease_index.edge_offsets.(edge)
            to crease_index.edge_offsets.(edge + 1) - 1 do
          let vertex = crease_index.edge_vertices.(at) in
          let primitive = crease_index.primitive_of_vertex.(vertex) in
          if selected primitive then begin
            let value = contribution vertex primitive in
            if Bytes.get assigned edge = '\000'
               || value > assigned_weights.(edge) then begin
              Bytes.set assigned edge '\001';
              assigned_weights.(edge) <- value
            end
          end
        done)
    end else
      for primitive = 0 to Bytes.length crease_topology.primitive_kinds - 1 do
        Cancel.poll cancel primitive 4095;
        if selected primitive then begin
          let first = crease_topology.primitive_offsets.(primitive)
          and last = crease_topology.primitive_offsets.(primitive + 1) in
          for vertex = first to last - 1 do
            let crease_edge = crease_index.edge_of_vertex.(vertex) in
            if crease_edge >= 0 then begin
              let a = crease_index.edge_a.(crease_edge)
              and b = crease_index.edge_b.(crease_edge) in
              match Topology_index.find_edge source_index_value ~a ~b with
              | None -> ()
              | Some source_edge ->
                  let weight = contribution vertex primitive in
                  if Bytes.get assigned source_edge = '\000'
                     || weight > assigned_weights.(source_edge) then begin
                    Bytes.set assigned source_edge '\001';
                    assigned_weights.(source_edge) <- weight
                  end
            end
          done
        end
      done;
    let changed = ref false in
    for edge = 0 to source_edge_count - 1 do
      if Bytes.get assigned edge <> '\000' then begin
        if edge_sharpness.(edge) <> assigned_weights.(edge) then changed := true;
        edge_sharpness.(edge) <- assigned_weights.(edge)
      end
    done;
    if not !changed
       && Geometry.find_attribute ~owner:Attribute.Vertex "creaseweight" source <> None
       && Geometry.find_attribute ~owner:Attribute.Primitive "creaseweight" source = None
    then source
    else begin
      let values = Array.make (Array.length source_topology.vertex_points) 0. in
      for vertex = 0 to Array.length values - 1 do
        let edge = source_index.edge_of_vertex.(vertex) in
        if edge >= 0 then values.(vertex) <- edge_sharpness.(edge)
      done;
      let attribute = Attribute.create_owned ~name:"creaseweight"
          ~owner:Attribute.Vertex (Attribute.Float values) |> get_ok in
      source
      |> Geometry.without_attribute ~owner:Attribute.Primitive "creaseweight"
      |> Geometry.with_attribute attribute
      |> get_ok
    end
  end

let clamp_sharpness value =
  if value <= 0. then 0. else if value >= 1. then 1. else value

let build_child_edge_sharpness ?cancel ?grain method_ index point_count
    edge_sharpness =
  let child = Array.make (Array.length edge_sharpness * 2) 0. in
  run ?grain ?cancel point_count (fun point ->
    let first = index.Topology_index.View.point_edge_offsets.(point)
    and last = index.point_edge_offsets.(point + 1) in
    let sharp_sum = ref 0. and sharp_count = ref 0 in
    if method_ = Subdivide_creasing_chaikin then
      for at = first to last - 1 do
        let weight = edge_sharpness.(index.point_edges.(at)) in
        if weight > 0. then begin
          sharp_sum := !sharp_sum +. weight;
          incr sharp_count
        end
      done;
    for at = first to last - 1 do
      let edge = index.point_edges.(at) in
      let parent = edge_sharpness.(edge) in
      let value = match method_ with
        | Subdivide_creasing_uniform -> max 0. (parent -. 1.)
        | Subdivide_creasing_chaikin ->
            if parent <= 0. then 0.
            else if !sharp_count <= 1 then max 0. (parent -. 1.)
            else
              let others = (!sharp_sum -. parent)
                  /. float_of_int (!sharp_count - 1) in
              max 0. (((0.75 *. parent) +. (0.25 *. others)) -. 1.) in
      let side = if index.Topology_index.View.edge_a.(edge) = point then 0 else 1 in
      child.((edge * 2) + side) <- value
    done);
  child

let build_vertex_sharp_plan ?cancel ?grain method_ index edge_sharpness
    child_edge_sharpness corner_sharpness =
  let point_count = Array.length corner_sharpness in
  let parent_kind = Bytes.make point_count '\000'
  and child_kind = Bytes.make point_count '\000'
  and transition_factor = Array.make point_count 1.
  and parent_a = Array.init point_count Fun.id
  and parent_b = Array.init point_count Fun.id
  and child_a = Array.init point_count Fun.id
  and child_b = Array.init point_count Fun.id in
  let child_weight point edge =
    let side = if index.Topology_index.View.edge_a.(edge) = point then 0 else 1 in
    child_edge_sharpness.((edge * 2) + side) in
  run ?grain ?cancel point_count (fun point ->
    let first = index.Topology_index.View.point_edge_offsets.(point)
    and last = index.point_edge_offsets.(point + 1) in
    let classify ~child =
      let count = ref 0 and a = ref point and b = ref point in
      for at = first to last - 1 do
        let edge = index.point_edges.(at) in
        let weight = if child then child_weight point edge
          else edge_sharpness.(edge) in
        if weight > 0. then begin
          let neighbor = if index.edge_a.(edge) = point then index.edge_b.(edge)
            else index.edge_a.(edge) in
          if !count = 0 then a := neighbor;
          b := neighbor;
          incr count
        end
      done;
      let corner = if child then max 0. (corner_sharpness.(point) -. 1.)
        else corner_sharpness.(point) in
      let kind = if corner > 0. || !count > 2 then '\002'
        else if !count = 2 then '\001' else '\000' in
      kind, !a, !b, corner in
    let p_kind, p_a, p_b, parent_corner = classify ~child:false
    and c_kind, c_a, c_b, child_corner = classify ~child:true in
    Bytes.set parent_kind point p_kind;
    Bytes.set child_kind point c_kind;
    parent_a.(point) <- p_a; parent_b.(point) <- p_b;
    child_a.(point) <- c_a; child_b.(point) <- c_b;
    if p_kind <> c_kind then begin
      let sum = ref 0. and count = ref 0 in
      if parent_corner > 0. && child_corner <= 0. then begin
        sum := parent_corner; count := 1
      end;
      for at = first to last - 1 do
        let edge = index.point_edges.(at) in
        let parent = edge_sharpness.(edge) in
        let child = child_weight point edge in
        let transitioned = match method_ with
          | Subdivide_creasing_uniform -> parent > 0. && parent <= 1.
          | Subdivide_creasing_chaikin -> parent > 0. && child <= 0. in
        if transitioned then begin sum := !sum +. parent; incr count end
      done;
      transition_factor.(point) <- if !count = 0 then 0.
        else clamp_sharpness (!sum /. float_of_int !count)
    end);
  { parent_kind; child_kind; transition_factor; parent_a; parent_b;
    child_a; child_b }

let primitive_size topology primitive =
  topology.Topology.View.primitive_offsets.(primitive + 1)
  - topology.primitive_offsets.(primitive)

let not_finite value = not (Float.is_finite value)

let validate ?cancel scheme geometry topology index =
  if topology.Topology.View.point_count = 0
     || Bytes.length topology.primitive_kinds = 0 then
    fail "input must contain polygon geometry";
  let point_marks = Array.make topology.point_count (-1) in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 1023;
    if Bytes.get topology.primitive_kinds primitive <> '\000' then
      fail "only polygon primitives are supported";
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let size = last - first in
    if size < 3 then fail "polygons must contain at least three vertices";
    if scheme = Loop && size <> 3 then
      fail "Loop subdivision requires triangle polygons";
    for vertex = first to last - 1 do
      let next = if vertex + 1 = last then first else vertex + 1 in
      if topology.vertex_points.(vertex) = topology.vertex_points.(next) then
        fail "a polygon contains a zero-length topology edge"
    done;
    for vertex = first to last - 1 do
      let point = topology.vertex_points.(vertex) in
      if point_marks.(point) = primitive then
        fail "a polygon references the same point more than once";
      point_marks.(point) <- primitive
    done
  done;
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  if Array.exists not_finite positions.x || Array.exists not_finite positions.y
     || Array.exists not_finite positions.z then
    fail "positions must be finite";
  let edge_offsets = index.Topology_index.View.edge_offsets in
  for edge = 0 to Array.length index.edge_a - 1 do
    let incidence = edge_offsets.(edge + 1) - edge_offsets.(edge) in
    if incidence > 2 then fail "non-manifold edges are not supported"
  done;
  let face_marks = Array.make (Bytes.length topology.primitive_kinds) (-1)
  and fan_stack = Array.make (Array.length topology.vertex_points) 0 in
  for point = 0 to topology.point_count - 1 do
    Cancel.poll cancel point 4095;
    let edge_first = index.point_edge_offsets.(point)
    and edge_last = index.point_edge_offsets.(point + 1) in
    let boundary = ref 0 in
    for at = edge_first to edge_last - 1 do
      let edge = index.point_edges.(at) in
      if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 1 then
        incr boundary
    done;
    let faces = index.point_offsets.(point + 1) - index.point_offsets.(point)
    and edges = edge_last - edge_first in
    if !boundary <> 0 && !boundary <> 2 then
      fail "a boundary vertex does not have exactly two boundary edges";
    if (!boundary = 0 && edges <> faces)
       || (!boundary = 2 && edges <> faces + 1) then
      fail "a vertex has non-manifold polygon incidence";
    if faces > 0 then begin
      let stack_count = ref 1 and reached = ref 0 in
      fan_stack.(0) <- index.point_vertices.(index.point_offsets.(point));
      while !stack_count > 0 do
        decr stack_count;
        let corner = fan_stack.(!stack_count) in
        let primitive = index.primitive_of_vertex.(corner) in
        if face_marks.(primitive) <> point then begin
          face_marks.(primitive) <- point; incr reached;
          let outgoing = index.edge_of_vertex.(corner)
          and incoming = index.edge_of_vertex.(index.previous_vertex.(corner)) in
          for side = 0 to 1 do
            let edge = if side = 0 then outgoing else incoming in
            if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2 then
              for at = index.edge_offsets.(edge) to index.edge_offsets.(edge + 1) - 1 do
                let directed = index.edge_vertices.(at) in
                let other_primitive = index.primitive_of_vertex.(directed) in
                if other_primitive <> primitive && face_marks.(other_primitive) <> point then begin
                  let adjacent =
                    if topology.vertex_points.(directed) = point then directed
                    else index.next_vertex.(directed) in
                  if adjacent < 0 || topology.vertex_points.(adjacent) <> point then
                    fail "edge incidence is inconsistent at a vertex";
                  fan_stack.(!stack_count) <- adjacent; incr stack_count
                end
              done
          done
        end
      done;
      if !reached <> faces then fail "a vertex has disconnected polygon fans"
    end
  done

let boundary_neighbors index point =
  let first = index.Topology_index.View.point_edge_offsets.(point)
  and last = index.point_edge_offsets.(point + 1) in
  let a = ref (-1) and b = ref (-1) and count = ref 0 in
  for at = first to last - 1 do
    let edge = index.point_edges.(at) in
    if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 1 then begin
      let neighbor = if index.edge_a.(edge) = point then index.edge_b.(edge)
        else index.edge_a.(edge) in
      if !count = 0 then a := neighbor else if !count = 1 then b := neighbor;
      incr count
    end
  done;
  !a, !b, !count

let boundary_corner boundary_interpolation index point =
  boundary_interpolation = Subdivide_boundary_edge_and_corner
  && index.Topology_index.View.point_offsets.(point + 1)
     - index.point_offsets.(point) = 1

let point_term_count boundary_interpolation scheme topology index
    source_points edge_count output =
  if output < source_points then begin
    let point = output in
    match scheme with
    | Bilinear -> 1
    | Loop ->
        let _, _, boundary = boundary_neighbors index point in
        if boundary = 2 then
          if boundary_corner boundary_interpolation index point then 1 else 3
        else checked_add "Loop stencil size" 1
          (index.Topology_index.View.point_edge_offsets.(point + 1)
           - index.point_edge_offsets.(point))
    | Catmull_clark ->
        let _, _, boundary = boundary_neighbors index point in
        if boundary = 2 then
          if boundary_corner boundary_interpolation index point then 1 else 3
        else begin
          let edge_count = index.point_edge_offsets.(point + 1)
            - index.point_edge_offsets.(point) in
          let corners = ref 0 in
          for at = index.point_offsets.(point) to index.point_offsets.(point + 1) - 1 do
            let vertex = index.point_vertices.(at) in
            let primitive = index.primitive_of_vertex.(vertex) in
            corners := !corners + primitive_size topology primitive
          done;
          checked_add "Catmull-Clark stencil size" 1
            (checked_add "Catmull-Clark stencil size"
              (checked_mul "Catmull-Clark stencil size" edge_count 2) !corners)
        end
  end else if output < source_points + edge_count then begin
    let edge = output - source_points in
    match scheme with
    | Bilinear -> 2
    | Loop ->
        if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2
        then 4 else 2
    | Catmull_clark ->
        if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2 then begin
          let count = ref 2 in
          for at = index.edge_offsets.(edge) to index.edge_offsets.(edge + 1) - 1 do
            let vertex = index.edge_vertices.(at) in
            count := checked_add "Catmull-Clark edge stencil size" !count
              (primitive_size topology index.primitive_of_vertex.(vertex))
          done;
          !count
        end else 2
  end else
    primitive_size topology (output - source_points - edge_count)

let fill_point_stencil boundary_interpolation triangle_subdivision scheme topology index
    source_points edge_count
    offsets sources weights representatives output =
  let cursor = ref offsets.(output) in
  let add source weight =
    sources.(!cursor) <- source; weights.(!cursor) <- weight; incr cursor in
  if output < source_points then begin
    let point = output in
    representatives.(output) <- point;
    match scheme with
    | Bilinear -> add point 1.
    | Loop ->
        let left, right, boundary = boundary_neighbors index point in
        if boundary = 2 then begin
          if boundary_corner boundary_interpolation index point then add point 1.
          else begin add point 0.75; add left 0.125; add right 0.125 end
        end else begin
          let first = index.point_edge_offsets.(point)
          and last = index.point_edge_offsets.(point + 1) in
          let n = last - first in
          if n = 0 then add point 1.
          else begin
            let nf = float_of_int n in
            let angle = 2. *. Float.pi /. nf in
            let beta = (0.625 -. ((0.375 +. (0.25 *. cos angle)) ** 2.)) /. nf in
            add point (1. -. (nf *. beta));
            for at = first to last - 1 do
              let edge = index.point_edges.(at) in
              add (if index.edge_a.(edge) = point then index.edge_b.(edge)
                   else index.edge_a.(edge)) beta
            done
          end
        end
    | Catmull_clark ->
        let left, right, boundary = boundary_neighbors index point in
        if boundary = 2 then begin
          if boundary_corner boundary_interpolation index point then add point 1.
          else begin add point 0.75; add left 0.125; add right 0.125 end
        end else begin
          let edge_first = index.point_edge_offsets.(point)
          and edge_last = index.point_edge_offsets.(point + 1)
          and face_first = index.point_offsets.(point)
          and face_last = index.point_offsets.(point + 1) in
          let n = edge_last - edge_first and faces = face_last - face_first in
          if n = 0 || faces = 0 then add point 1.
          else begin
            let nf = float_of_int n in
            add point ((nf -. 3.) /. nf);
            let edge_weight = 1. /. (nf *. nf) in
            for at = edge_first to edge_last - 1 do
              let edge = index.point_edges.(at) in
              add index.edge_a.(edge) edge_weight;
              add index.edge_b.(edge) edge_weight
            done;
            let face_count = float_of_int faces in
            for at = face_first to face_last - 1 do
              let vertex = index.point_vertices.(at) in
              let primitive = index.primitive_of_vertex.(vertex) in
              let first = topology.Topology.View.primitive_offsets.(primitive)
              and last = topology.primitive_offsets.(primitive + 1) in
              let weight = 1. /. (nf *. face_count *. float_of_int (last - first)) in
              for corner = first to last - 1 do
                add topology.vertex_points.(corner) weight
              done
            done
          end
        end
  end else if output < source_points + edge_count then begin
    let edge = output - source_points in
    let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
    representatives.(output) <- min a b;
    match scheme with
    | Bilinear -> add a 0.5; add b 0.5
    | Loop ->
        if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2 then begin
          add a 0.375; add b 0.375;
          for at = index.edge_offsets.(edge) to index.edge_offsets.(edge + 1) - 1 do
            let vertex = index.edge_vertices.(at) in
            let next = index.next_vertex.(vertex) in
            add topology.vertex_points.(index.next_vertex.(next)) 0.125
          done
        end else begin add a 0.5; add b 0.5 end
    | Catmull_clark ->
        if index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 2 then begin
          let first_incidence = index.edge_offsets.(edge) in
          let face_weight = match triangle_subdivision with
            | Subdivide_triangles_catmull_clark -> 0.25
            | Subdivide_triangles_smooth ->
                let primitive_weight at =
                  let vertex = index.edge_vertices.(at) in
                  let primitive = index.primitive_of_vertex.(vertex) in
                  if primitive_size topology primitive = 3 then 0.470 else 0.25
                in
                0.5 *. (primitive_weight first_incidence
                  +. primitive_weight (first_incidence + 1)) in
          let vertex_weight = 0.5 *. (1. -. (2. *. face_weight)) in
          add a vertex_weight; add b vertex_weight;
          for at = index.edge_offsets.(edge) to index.edge_offsets.(edge + 1) - 1 do
            let vertex = index.edge_vertices.(at) in
            let primitive = index.primitive_of_vertex.(vertex) in
            let first = topology.Topology.View.primitive_offsets.(primitive)
            and last = topology.primitive_offsets.(primitive + 1) in
            let weight = face_weight /. float_of_int (last - first) in
            for corner = first to last - 1 do
              add topology.vertex_points.(corner) weight
            done
          done
        end else begin add a 0.5; add b 0.5 end
  end else begin
    let primitive = output - source_points - edge_count in
    let first = topology.Topology.View.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let weight = 1. /. float_of_int (last - first) in
    representatives.(output) <- topology.vertex_points.(first);
    for vertex = first to last - 1 do add topology.vertex_points.(vertex) weight done
  end;
  if !cursor <> offsets.(output + 1) then fail "internal stencil cardinality mismatch"

let make_topology ?cancel ?grain ?holes scheme
    (topology : Topology.View.view) (index : Topology_index.View.view)
    source_points edge_count face_offset =
  let visible primitive = match holes with
    | None -> true
    | Some holes -> not (Group.mem primitive holes) in
  match scheme with
  | Catmull_clark | Bilinear ->
      let source_primitive_count = Bytes.length topology.primitive_kinds in
      let visible_offsets = match holes with
        | None -> None
        | Some _ ->
            let offsets = Array.make (source_primitive_count + 1) 0 in
            for primitive = 0 to source_primitive_count - 1 do
              offsets.(primitive + 1) <- if visible primitive then
                checked_add "visible subdivision corner count" offsets.(primitive)
                  (primitive_size topology primitive)
              else offsets.(primitive)
            done;
            Some offsets in
      let output_primitives = match visible_offsets with
        | None -> Array.length topology.Topology.View.vertex_points
        | Some offsets -> offsets.(source_primitive_count) in
      let output_vertices = checked_mul "output vertex count" output_primitives 4 in
      let vertex_points = Array.make output_vertices 0
      and primitive_offsets = Array.init (output_primitives + 1) (fun i -> i * 4)
      and primitive_kinds = Bytes.make output_primitives '\000'
      and vertex_kind = Bytes.make output_vertices '\000'
      and vertex_a = Array.make output_vertices 0
      and vertex_b = Array.make output_vertices (-1)
      and vertex_primitive = Array.make output_vertices (-1)
      and vertex_edge_source = Array.make output_vertices (-1)
      and primitive_source = Array.make output_primitives 0 in
      run ?grain ?cancel (Array.length topology.vertex_points) (fun vertex ->
        let primitive = index.Topology_index.View.primitive_of_vertex.(vertex)
        and previous = index.previous_vertex.(vertex)
        and next = index.next_vertex.(vertex) in
        if visible primitive then begin
          let output_primitive = match visible_offsets with
            | None -> vertex
            | Some offsets -> offsets.(primitive)
                + vertex - topology.primitive_offsets.(primitive) in
          let edge = index.edge_of_vertex.(vertex)
          and previous_edge = index.edge_of_vertex.(previous)
          and at = output_primitive * 4 in
          vertex_points.(at) <- topology.vertex_points.(vertex);
          vertex_points.(at + 1) <- source_points + edge;
          vertex_points.(at + 2) <- face_offset + primitive;
          vertex_points.(at + 3) <- source_points + previous_edge;
          Bytes.set vertex_kind at '\000'; vertex_a.(at) <- vertex;
          vertex_edge_source.(at) <- edge;
          Bytes.set vertex_kind (at + 1) '\001'; vertex_a.(at + 1) <- vertex;
          vertex_b.(at + 1) <- next;
          Bytes.set vertex_kind (at + 2) '\002';
          vertex_primitive.(at + 2) <- primitive;
          Bytes.set vertex_kind (at + 3) '\001'; vertex_a.(at + 3) <- previous;
          vertex_b.(at + 3) <- vertex;
          vertex_edge_source.(at + 3) <- previous_edge;
          primitive_source.(output_primitive) <- primitive
        end
      );
      vertex_kind, vertex_a, vertex_b, vertex_primitive, vertex_edge_source,
      primitive_source,
      Topology.View.create_validated_owned
        ~point_count:(checked_add "output point count" face_offset
          (Bytes.length topology.primitive_kinds))
        ~vertex_points ~primitive_offsets ~primitive_kinds
  | Loop ->
      let source_primitives = Bytes.length topology.Topology.View.primitive_kinds in
      let visible_offsets = match holes with
        | None -> None
        | Some _ ->
            let offsets = Array.make (source_primitives + 1) 0 in
            for primitive = 0 to source_primitives - 1 do
              offsets.(primitive + 1) <- offsets.(primitive)
                + if visible primitive then 1 else 0
            done;
            Some offsets in
      let visible_primitives = match visible_offsets with
        | None -> source_primitives
        | Some offsets -> offsets.(source_primitives) in
      let output_primitives = checked_mul "output primitive count" visible_primitives 4 in
      let output_vertices = checked_mul "output vertex count" visible_primitives 12 in
      let vertex_points = Array.make output_vertices 0
      and primitive_offsets = Array.init (output_primitives + 1) (fun i -> i * 3)
      and primitive_kinds = Bytes.make output_primitives '\000'
      and vertex_kind = Bytes.make output_vertices '\000'
      and vertex_a = Array.make output_vertices 0
      and vertex_b = Array.make output_vertices (-1)
      and vertex_primitive = Array.make output_vertices (-1)
      and vertex_edge_source = Array.make output_vertices (-1)
      and primitive_source = Array.make output_primitives 0 in
      let set vertex point kind a b primitive =
        vertex_points.(vertex) <- point; Bytes.set vertex_kind vertex kind;
        vertex_a.(vertex) <- a; vertex_b.(vertex) <- b;
        vertex_primitive.(vertex) <- primitive in
      run ?grain ?cancel source_primitives (fun primitive ->
        if visible primitive then begin
        let output_source = match visible_offsets with
          | None -> primitive
          | Some offsets -> offsets.(primitive) in
        let first = topology.primitive_offsets.(primitive) in
        let va = first and vb = first + 1 and vc = first + 2 in
        let a = topology.vertex_points.(va) and b = topology.vertex_points.(vb)
        and c = topology.vertex_points.(vc) in
        let ab = source_points + index.edge_of_vertex.(va)
        and bc = source_points + index.edge_of_vertex.(vb)
        and ca = source_points + index.edge_of_vertex.(vc)
        and at = output_source * 12 in
        set at a '\000' va (-1) primitive;
        vertex_edge_source.(at) <- index.edge_of_vertex.(va);
        set (at+1) ab '\001' va vb primitive;
        set (at+2) ca '\001' vc va primitive;
        vertex_edge_source.(at+2) <- index.edge_of_vertex.(vc);
        set (at+3) b '\000' vb (-1) primitive;
        vertex_edge_source.(at+3) <- index.edge_of_vertex.(vb);
        set (at+4) bc '\001' vb vc primitive;
        set (at+5) ab '\001' va vb primitive;
        vertex_edge_source.(at+5) <- index.edge_of_vertex.(va);
        set (at+6) c '\000' vc (-1) primitive;
        vertex_edge_source.(at+6) <- index.edge_of_vertex.(vc);
        set (at+7) ca '\001' vc va primitive;
        set (at+8) bc '\001' vb vc primitive;
        vertex_edge_source.(at+8) <- index.edge_of_vertex.(vb);
        set (at+9) ab '\001' va vb primitive;
        set (at+10) bc '\001' vb vc primitive;
        set (at+11) ca '\001' vc va primitive;
        for child = 0 to 3 do
          primitive_source.((output_source * 4) + child) <- primitive
        done
        end
      );
      vertex_kind, vertex_a, vertex_b, vertex_primitive, vertex_edge_source,
      primitive_source,
      Topology.View.create_validated_owned ~point_count:(source_points + edge_count)
        ~vertex_points ~primitive_offsets ~primitive_kinds

let make_plan ?cancel ?grain ?edge_ancestry_attribute ?edge_crease_override ?holes
    boundary_interpolation triangle_subdivision creasing_method scheme source =
  let topology = Topology.View.view (Geometry.topology source) in
  let topology_index = Topology_index.create ?cancel (Geometry.topology source) in
  let index = Topology_index.View.view topology_index in
  validate ?cancel scheme source topology index;
  let source_points = topology.point_count
  and source_vertices = Array.length topology.vertex_points
  and source_primitives = Bytes.length topology.primitive_kinds
  and edge_count = Array.length index.edge_a in
  let face_offset = checked_add "output point count" source_points edge_count in
  let output_points = match scheme with
    | Loop -> face_offset
    | Catmull_clark | Bilinear ->
        checked_add "output point count" face_offset source_primitives in
  let point_offsets = Array.make (output_points + 1) 0 in
  for output = 0 to output_points - 1 do
    point_offsets.(output + 1) <- checked_add "subdivision stencil storage"
        point_offsets.(output)
        (point_term_count boundary_interpolation scheme topology index
          source_points edge_count output)
  done;
  let point_sources = Array.make point_offsets.(output_points) 0
  and point_weights = Array.make point_offsets.(output_points) 0.
  and point_representative = Array.make output_points 0 in
  run ?grain ?cancel output_points (fun output ->
    fill_point_stencil boundary_interpolation triangle_subdivision scheme topology
      index source_points edge_count point_offsets point_sources point_weights
      point_representative output
  );
  let creases = match crease_data ?edge_override:edge_crease_override
      source topology index with
    | None -> None
    | Some (edge_sharpness, corner_sharpness, has_edge_creases,
        _has_corner_creases) ->
        let child_edge_sharpness = build_child_edge_sharpness ?cancel ?grain
            creasing_method index source_points edge_sharpness in
        let vertex_masks = build_vertex_sharp_plan ?cancel ?grain
            creasing_method index edge_sharpness child_edge_sharpness
            corner_sharpness in
        Some { method_ = creasing_method; edge_sharpness; child_edge_sharpness; has_edge_creases;
           vertex_masks } in
  let vertex_kind, vertex_a, vertex_b, vertex_primitive, vertex_edge_source,
      primitive_source,
      output_topology =
    make_topology ?cancel ?grain ?holes scheme topology index source_points edge_count
      face_offset in
  { scheme; source; source_topology = topology; index; source_points; source_vertices;
    source_primitives; edge_count; face_offset; output_points; point_offsets;
    point_sources; point_weights; point_representative; edge_ancestry_attribute;
    boundary_interpolation; triangle_subdivision; creasing_method;  creases;
    vertex_kind; vertex_a;
    vertex_b; vertex_primitive; vertex_edge_source; primitive_source;
    topology = output_topology }

let creased_vertex_value creases source point smooth =
  let masks = creases.vertex_masks in
  let value kind a b = match kind with
    | '\002' -> source.(point)
    | '\001' -> (source.(point) *. 0.75)
        +. ((source.(a) +. source.(b)) *. 0.125)
    | _ -> smooth in
  let parent_kind = Bytes.get masks.parent_kind point in
  let parent = value parent_kind masks.parent_a.(point) masks.parent_b.(point) in
  let child_kind = Bytes.get masks.child_kind point in
  if parent_kind = child_kind then parent
  else
    let child = value child_kind masks.child_a.(point) masks.child_b.(point) in
    let factor = masks.transition_factor.(point) in
    child +. ((parent -. child) *. factor)

let creased_edge_factor creases edge =
  let parent = creases.edge_sharpness.(edge) in
  if creases.method_ = Subdivide_creasing_chaikin
     && parent > 0. && parent < 1.
     && creases.child_edge_sharpness.(edge * 2) > 0.
     && creases.child_edge_sharpness.((edge * 2) + 1) > 0.
  then 1.
  else clamp_sharpness parent

let point_float ?cancel ?grain plan source =
  let output = Array.make plan.output_points 0. in
  run ?grain ?cancel plan.output_points (fun point ->
    let sum = ref 0. in
    for at = plan.point_offsets.(point) to plan.point_offsets.(point + 1) - 1 do
      sum := !sum +. source.(plan.point_sources.(at)) *. plan.point_weights.(at)
    done;
    let value = !sum in
    output.(point) <-
      if plan.scheme = Bilinear then value
      else if point < plan.source_points then
        (match plan.creases with None -> value
         | Some creases -> creased_vertex_value creases source point value)
      else if point < plan.source_points + plan.edge_count then begin
        match plan.creases with
        | None -> value
        | Some creases when not creases.has_edge_creases -> value
        | Some creases ->
          let edge = point - plan.source_points in
          let factor = creased_edge_factor creases edge in
          if factor = 0. then value else
            let midpoint = (source.(plan.index.edge_a.(edge))
              +. source.(plan.index.edge_b.(edge))) *. 0.5 in
            value +. ((midpoint -. value) *. factor)
      end else value);
  output

type fvar_point_plan = {
  fvar_scheme : scheme;
  fvar_source_points : int;
  fvar_edge_count : int;
  fvar_output_points : int;
  fvar_point_offsets : int array;
  fvar_point_sources : int array;
  fvar_point_weights : float array;
  fvar_index : Topology_index.View.view;
  fvar_creases : crease_plan option;
}

type fvar_refinement =
  | Linear_fvar of plan
  | Isolated_fvar of {
      geometry_refinement : plan;
      corner_sharpness : float array;
    }
  | Continuous_fvar of {
      geometry_refinement : plan;
      source_vertex_of_point : int array;
      pinned_points : bytes;
      output_point_of_vertex : int array;
    }
  | Split_fvar of {
      refinement : fvar_point_plan;
      source_vertex_of_point : int array;
      output_point_of_vertex : int array;
    }

let fvar_refinement ?cancel ?grain mode plan equal =
  let topology = plan.source_topology and index = plan.index in
  let vertex_count = plan.source_vertices in
  let continuous_source = Array.make plan.source_points (-1)
  and discontinuous_points = Bytes.make plan.source_points '\000' in
  run ?grain ?cancel plan.source_points (fun point ->
    let first = index.point_offsets.(point)
    and last = index.point_offsets.(point + 1) in
    if first < last then begin
      let reference = index.point_vertices.(first) in
      continuous_source.(point) <- reference;
      let at = ref (first + 1) in
      while !at < last && Bytes.get discontinuous_points point = '\000' do
        if not (equal reference index.point_vertices.(!at)) then
          Bytes.set discontinuous_points point '\001';
        incr at
      done
    end);
  if not (Bytes.exists (fun value -> value <> '\000') discontinuous_points)
  then begin
    let pinned_points = Bytes.make plan.source_points '\000' in
    run ?grain ?cancel plan.source_points (fun point ->
      let faces = index.point_offsets.(point + 1) - index.point_offsets.(point) in
      let _, _, boundary_edges = boundary_neighbors index point in
      let pinned = match mode with
        | Subdivide_fvar_none -> false
        | Subdivide_fvar_corners_only
        | Subdivide_fvar_corners_plus1
        | Subdivide_fvar_corners_plus2 -> faces = 1
        | Subdivide_fvar_boundaries -> boundary_edges > 0
        | Subdivide_fvar_all -> assert false in
      if pinned then Bytes.set pinned_points point '\001');
    Continuous_fvar {
      geometry_refinement = plan;
      source_vertex_of_point = continuous_source;
      pinned_points;
      output_point_of_vertex =
        (Topology.View.view plan.topology).vertex_points;
    }
  end else begin
  let parents = Array.init vertex_count Fun.id and ranks = Bytes.make vertex_count '\000' in
  let rec find value =
    let parent = parents.(value) in
    if parent = value then value
    else begin
      let root = find parent in
      parents.(value) <- root;
      root
    end in
  let union left right =
    let left = find left and right = find right in
    if left <> right then begin
      let left_rank = Char.code (Bytes.get ranks left)
      and right_rank = Char.code (Bytes.get ranks right) in
      if left_rank < right_rank then parents.(left) <- right
      else begin
        parents.(right) <- left;
        if left_rank = right_rank then
          Bytes.set ranks left (Char.chr (left_rank + 1))
      end
    end in
  let corner_at_point directed point =
    if topology.vertex_points.(directed) = point then directed
    else begin
      let next = index.next_vertex.(directed) in
      if next < 0 || topology.vertex_points.(next) <> point then
        fail "face-varying edge incidence is inconsistent";
      next
    end in
  (* Every point owns a disjoint set of source corners, so these union-find
     updates can run concurrently without synchronization. *)
  run ?grain ?cancel plan.source_points (fun point ->
    for at = index.point_edge_offsets.(point)
        to index.point_edge_offsets.(point + 1) - 1 do
      let edge = index.point_edges.(at) in
      let first = index.edge_offsets.(edge)
      and last = index.edge_offsets.(edge + 1) in
      if last - first = 2 then begin
        let left = corner_at_point index.edge_vertices.(first) point
        and right = corner_at_point index.edge_vertices.(first + 1) point in
        if equal left right then union left right
      end
    done);
  let root_seen = Bytes.make vertex_count '\000'
  and component_counts = Array.make plan.source_points 0 in
  run ?grain ?cancel plan.source_points (fun point ->
    let count = ref 0 in
    for at = index.point_offsets.(point) to index.point_offsets.(point + 1) - 1 do
      let root = find index.point_vertices.(at) in
      if Bytes.get root_seen root = '\000' then begin
        Bytes.set root_seen root '\001';
        incr count
      end
    done;
    component_counts.(point) <- !count);
  let component_offsets = Array.make (plan.source_points + 1) 0 in
  for point = 0 to plan.source_points - 1 do
    component_offsets.(point + 1) <- checked_add "face-varying value count"
        component_offsets.(point) component_counts.(point)
  done;
  let fvar_point_count = component_offsets.(plan.source_points) in
  let root_target = Array.make vertex_count (-1)
  and source_vertex_of_point = Array.make fvar_point_count 0
  and original_point_of_fvar = Array.make fvar_point_count 0 in
  run ?grain ?cancel plan.source_points (fun point ->
    let next = ref component_offsets.(point) in
    for at = index.point_offsets.(point) to index.point_offsets.(point + 1) - 1 do
      let vertex = index.point_vertices.(at) in
      let root = find vertex in
      if root_target.(root) < 0 then begin
        root_target.(root) <- !next;
        source_vertex_of_point.(!next) <- vertex;
        original_point_of_fvar.(!next) <- point;
        incr next
      end
    done;
    if !next <> component_offsets.(point + 1) then
      fail "face-varying component cardinality mismatch");
  let fvar_vertex_points = Array.make vertex_count 0 in
  run ?grain ?cancel vertex_count (fun vertex ->
    let target = root_target.(find vertex) in
    if target < 0 then fail "face-varying corner has no value component";
    fvar_vertex_points.(vertex) <- target);
  let component_faces = Array.make fvar_point_count 0 in
  run ?grain ?cancel plan.source_points (fun point ->
    for at = index.point_offsets.(point) to index.point_offsets.(point + 1) - 1 do
      let component = fvar_vertex_points.(index.point_vertices.(at)) in
      component_faces.(component) <- component_faces.(component) + 1
    done);
  if Array.for_all (fun faces -> faces = 1) component_faces then begin
    let corner_weights = Array.make fvar_point_count 0. in
    (match float_attribute Attribute.Point "cornerweight" plan.source with
     | None -> ()
     | Some source ->
         run ?grain ?cancel fvar_point_count (fun component ->
           corner_weights.(component) <-
             source.(original_point_of_fvar.(component))));
    run ?grain ?cancel fvar_point_count (fun component ->
      let point = original_point_of_fvar.(component) in
      let source_faces = index.point_offsets.(point + 1)
          - index.point_offsets.(point) in
      if mode <> Subdivide_fvar_none
         || (plan.boundary_interpolation = Subdivide_boundary_edge_and_corner
             && source_faces = 1) then
        corner_weights.(component) <- max_float);
    if Array.for_all (fun weight -> weight >= 1.) corner_weights
    then Linear_fvar plan
    else begin
      let corner_sharpness = Array.init vertex_count (fun vertex ->
        corner_weights.(fvar_vertex_points.(vertex))) in
      Isolated_fvar { geometry_refinement = plan; corner_sharpness }
    end
  end else begin
  let fvar_topology = Topology.View.create_validated_owned
      ~point_count:fvar_point_count ~vertex_points:fvar_vertex_points
      ~primitive_offsets:topology.primitive_offsets
      ~primitive_kinds:topology.primitive_kinds in
  let fvar_index_value = Topology_index.create ?cancel fvar_topology in
  let fvar_index = Topology_index.View.view fvar_index_value in
  let corner_weights = Array.make fvar_point_count 0. in
  (match float_attribute Attribute.Point "cornerweight" plan.source with
   | None -> ()
   | Some source ->
       run ?grain ?cancel fvar_point_count (fun point ->
         corner_weights.(point) <- source.(original_point_of_fvar.(point))));
  let component_boundary_edges = Array.make fvar_point_count 0 in
  run ?grain ?cancel fvar_point_count (fun point ->
    let count = ref 0 in
    for at = fvar_index.point_edge_offsets.(point)
        to fvar_index.point_edge_offsets.(point + 1) - 1 do
      let edge = fvar_index.point_edges.(at) in
      if fvar_index.edge_offsets.(edge + 1) - fvar_index.edge_offsets.(edge) = 1
      then incr count
    done;
    component_boundary_edges.(point) <- !count);
  let point_has_corner_component = Bytes.make plan.source_points '\000' in
  run ?grain ?cancel plan.source_points (fun point ->
    for component = component_offsets.(point) to component_offsets.(point + 1) - 1 do
      if component_faces.(component) = 1 then
        Bytes.set point_has_corner_component point '\001'
    done);
  let pin point = corner_weights.(point) <- max_float in
  run ?grain ?cancel fvar_point_count (fun fvar_point ->
    let point = original_point_of_fvar.(fvar_point) in
    let source_faces = index.point_offsets.(point + 1) - index.point_offsets.(point) in
    let components = component_counts.(point) in
    let _, _, source_boundary_edges = boundary_neighbors index point in
    let policy_pin = match mode with
      | Subdivide_fvar_none -> false
      | Subdivide_fvar_corners_only -> component_faces.(fvar_point) = 1
      | Subdivide_fvar_corners_plus1 ->
          component_faces.(fvar_point) = 1 || components > 2
      | Subdivide_fvar_corners_plus2 ->
          component_faces.(fvar_point) = 1 || components > 2
          || (components = 1 && component_boundary_edges.(fvar_point) > 0
              && source_boundary_edges = 0)
          || (components = 2
              && Bytes.get point_has_corner_component point <> '\000')
      | Subdivide_fvar_boundaries -> component_boundary_edges.(fvar_point) > 0
      | Subdivide_fvar_all -> assert false in
    let geometry_corner =
      plan.boundary_interpolation = Subdivide_boundary_edge_and_corner
      && source_faces = 1 in
    if policy_pin || geometry_corner then pin fvar_point);
  (* Plus modes inherit semi-sharpness from the other of two regions when
     exactly one region contains a sharp interior edge. Seam and geometric
     boundary edges are deliberately excluded. *)
  (match mode, plan.creases with
   | (Subdivide_fvar_corners_plus1 | Subdivide_fvar_corners_plus2),
       Some creases when creases.has_edge_creases ->
       let internal_sharpness = Array.make fvar_point_count 0. in
       run ?grain ?cancel fvar_point_count (fun point ->
         let sharpness = ref 0. in
         for at = fvar_index.point_edge_offsets.(point)
             to fvar_index.point_edge_offsets.(point + 1) - 1 do
           let edge = fvar_index.point_edges.(at) in
           if fvar_index.edge_offsets.(edge + 1)
                - fvar_index.edge_offsets.(edge) = 2 then begin
             let vertex = fvar_index.edge_vertices.(fvar_index.edge_offsets.(edge)) in
             let source_edge = index.edge_of_vertex.(vertex) in
             if source_edge >= 0 then
               sharpness := max !sharpness creases.edge_sharpness.(source_edge)
           end
         done;
         internal_sharpness.(point) <- !sharpness);
       run ?grain ?cancel plan.source_points (fun point ->
         if component_counts.(point) = 2 then begin
           let left = component_offsets.(point) in
           let right = left + 1 in
           if internal_sharpness.(left) = 0.
              && internal_sharpness.(right) > 0.
              && component_faces.(right) > 1 then
             corner_weights.(left) <- max corner_weights.(left)
                 internal_sharpness.(right);
           if internal_sharpness.(right) = 0.
              && internal_sharpness.(left) > 0.
              && component_faces.(left) > 1 then
             corner_weights.(right) <- max corner_weights.(right)
                 internal_sharpness.(left)
         end)
   | _ -> ());
  let every_component_pinned = Array.for_all
      (fun weight -> weight >= 1.) corner_weights in
  let every_component_is_one_face = Array.for_all
      (fun faces -> faces = 1) component_faces in
  let every_edge_is_boundary =
    let boundary = ref true in
    for edge = 0 to Array.length fvar_index.edge_a - 1 do
      if fvar_index.edge_offsets.(edge + 1) - fvar_index.edge_offsets.(edge) <> 1
      then boundary := false
    done;
    !boundary in
  if every_component_pinned && every_edge_is_boundary then Linear_fvar plan
  else if every_component_is_one_face && every_edge_is_boundary then begin
    let corner_sharpness = Array.init vertex_count (fun vertex ->
      corner_weights.(fvar_vertex_points.(vertex))) in
    Isolated_fvar { geometry_refinement = plan; corner_sharpness }
  end
  else begin
  let attributes = ref [] in
  (match plan.creases with
   | Some creases when creases.has_edge_creases ->
       let weights = Array.make vertex_count 0. in
       run ?grain ?cancel vertex_count (fun vertex ->
         let edge = index.edge_of_vertex.(vertex) in
         if edge >= 0 then weights.(vertex) <- creases.edge_sharpness.(edge));
       let attribute = Attribute.create_owned ~owner:Attribute.Vertex
           ~name:"creaseweight" (Attribute.Float weights) |> get_ok in
       attributes := attribute :: !attributes
   | _ -> ());
  if Array.exists (fun weight -> weight > 0.) corner_weights then begin
    let attribute = Attribute.create_owned ~owner:Attribute.Point
        ~name:"cornerweight" (Attribute.Float corner_weights) |> get_ok in
    attributes := attribute :: !attributes
  end;
  let zeros = Array.make fvar_point_count 0. in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:zeros ~y:zeros ~z:zeros in
  let geometry = Geometry.create ~positions ~topology:fvar_topology
      ~attributes:!attributes () |> get_ok in
  let fvar_topology_view = Topology.View.view fvar_topology in
  let edge_count = Array.length fvar_index.edge_a in
  let face_offset = checked_add "face-varying output point count"
      fvar_point_count edge_count in
  let output_points = match plan.scheme with
    | Loop -> face_offset
    | Catmull_clark | Bilinear -> checked_add "face-varying output point count"
        face_offset plan.source_primitives in
  let point_offsets = Array.make (output_points + 1) 0 in
  for output = 0 to output_points - 1 do
    point_offsets.(output + 1) <- checked_add "face-varying stencil storage"
        point_offsets.(output)
        (point_term_count Subdivide_boundary_edge_only plan.scheme
          fvar_topology_view fvar_index fvar_point_count edge_count output)
  done;
  let point_sources = Array.make point_offsets.(output_points) 0
  and point_weights = Array.make point_offsets.(output_points) 0.
  and point_representatives = Array.make output_points 0 in
  run ?grain ?cancel output_points (fun output ->
    fill_point_stencil Subdivide_boundary_edge_only plan.triangle_subdivision
      plan.scheme fvar_topology_view fvar_index fvar_point_count edge_count
      point_offsets point_sources point_weights point_representatives output);
  let creases = match crease_data geometry fvar_topology_view fvar_index with
    | None -> None
    | Some (edge_sharpness, corner_sharpness, has_edge_creases,
        _has_corner_creases) ->
        let child_edge_sharpness = build_child_edge_sharpness ?cancel ?grain
            plan.creasing_method fvar_index fvar_point_count edge_sharpness in
        let vertex_masks = build_vertex_sharp_plan ?cancel ?grain
            plan.creasing_method fvar_index edge_sharpness child_edge_sharpness
            corner_sharpness in
        Some { method_ = plan.creasing_method; edge_sharpness; child_edge_sharpness; has_edge_creases;
           vertex_masks } in
  let output_count = Bytes.length plan.vertex_kind in
  let output_point_of_vertex = Array.make output_count 0 in
  run ?grain ?cancel output_count (fun output ->
    output_point_of_vertex.(output) <- match Bytes.get plan.vertex_kind output with
      | '\000' -> fvar_vertex_points.(plan.vertex_a.(output))
      | '\001' ->
          let edge = fvar_index.edge_of_vertex.(plan.vertex_a.(output)) in
          if edge < 0 then fail "face-varying output edge has no source edge";
          fvar_point_count + edge
      | _ -> face_offset + plan.vertex_primitive.(output));
  let refinement = {
    fvar_scheme = plan.scheme;
    fvar_source_points = fvar_point_count;
    fvar_edge_count = edge_count;
    fvar_output_points = output_points;
    fvar_point_offsets = point_offsets;
    fvar_point_sources = point_sources;
    fvar_point_weights = point_weights;
    fvar_index;
    fvar_creases = creases;
  } in
  Split_fvar { refinement; source_vertex_of_point;
    output_point_of_vertex }
  end
  end
  end

let fvar_point_float ?cancel ?grain plan source =
  let output = Array.make plan.fvar_output_points 0. in
  run ?grain ?cancel plan.fvar_output_points (fun point ->
    let sum = ref 0. in
    for at = plan.fvar_point_offsets.(point)
        to plan.fvar_point_offsets.(point + 1) - 1 do
      sum := !sum +. source.(plan.fvar_point_sources.(at))
          *. plan.fvar_point_weights.(at)
    done;
    let value = !sum in
    output.(point) <-
      if plan.fvar_scheme = Bilinear then value
      else if point < plan.fvar_source_points then
        (match plan.fvar_creases with None -> value
         | Some creases -> creased_vertex_value creases source point value)
      else if point < plan.fvar_source_points + plan.fvar_edge_count then begin
        match plan.fvar_creases with
        | None -> value
        | Some creases when not creases.has_edge_creases -> value
        | Some creases ->
            let edge = point - plan.fvar_source_points in
            let factor = creased_edge_factor creases edge in
            if factor = 0. then value else
              let midpoint = (source.(plan.fvar_index.edge_a.(edge))
                +. source.(plan.fvar_index.edge_b.(edge))) *. 0.5 in
              value +. ((midpoint -. value) *. factor)
      end else value);
  output

let linear_vertex_float ?cancel ?grain plan source =
  let count = Bytes.length plan.vertex_kind in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun vertex ->
    match Bytes.get plan.vertex_kind vertex with
    | '\000' -> output.(vertex) <- source.(plan.vertex_a.(vertex))
    | '\001' -> output.(vertex) <-
        (source.(plan.vertex_a.(vertex)) +. source.(plan.vertex_b.(vertex))) *. 0.5
    | _ ->
        let primitive = plan.vertex_primitive.(vertex) in
        let first = plan.source_topology.primitive_offsets.(primitive)
        and last = plan.source_topology.primitive_offsets.(primitive + 1) in
        let sum = ref 0. in
        for corner = first to last - 1 do sum := !sum +. source.(corner) done;
        output.(vertex) <- !sum /. float_of_int (last - first));
  output

let isolated_vertex_float ?cancel ?grain plan corner_sharpness source =
  let count = Bytes.length plan.vertex_kind in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun output_vertex ->
    match Bytes.get plan.vertex_kind output_vertex with
    | '\000' ->
        let vertex = plan.vertex_a.(output_vertex) in
        let previous = plan.index.previous_vertex.(vertex)
        and next = plan.index.next_vertex.(vertex) in
        let smooth = (0.75 *. source.(vertex))
            +. (0.125 *. source.(previous)) +. (0.125 *. source.(next)) in
        let factor = clamp_sharpness corner_sharpness.(vertex) in
        output.(output_vertex) <- smooth
            +. ((source.(vertex) -. smooth) *. factor)
    | '\001' -> output.(output_vertex) <-
        (source.(plan.vertex_a.(output_vertex))
          +. source.(plan.vertex_b.(output_vertex))) *. 0.5
    | _ ->
        let primitive = plan.vertex_primitive.(output_vertex) in
        let first = plan.source_topology.primitive_offsets.(primitive)
        and last = plan.source_topology.primitive_offsets.(primitive + 1) in
        let sum = ref 0. in
        for corner = first to last - 1 do sum := !sum +. source.(corner) done;
        output.(output_vertex) <- !sum /. float_of_int (last - first));
  output

let fvar_float ?cancel ?grain fvar source =
  match fvar with
  | Linear_fvar plan -> linear_vertex_float ?cancel ?grain plan source
  | Isolated_fvar fvar -> isolated_vertex_float ?cancel ?grain
      fvar.geometry_refinement fvar.corner_sharpness source
  | Continuous_fvar fvar ->
      let source_values = Array.init
          (Array.length fvar.source_vertex_of_point) (fun point ->
        let vertex = fvar.source_vertex_of_point.(point) in
        if vertex < 0 then 0. else source.(vertex)) in
      let refined = point_float ?cancel ?grain
          fvar.geometry_refinement source_values in
      run ?grain ?cancel (Array.length source_values) (fun point ->
        if Bytes.get fvar.pinned_points point <> '\000' then
          refined.(point) <- source_values.(point));
      let output = Array.make (Array.length fvar.output_point_of_vertex) 0. in
      run ?grain ?cancel (Array.length output) (fun vertex ->
        output.(vertex) <- refined.(fvar.output_point_of_vertex.(vertex)));
      output
  | Split_fvar fvar ->
      let source_values = Array.init (Array.length fvar.source_vertex_of_point)
          (fun point -> source.(fvar.source_vertex_of_point.(point))) in
      let refined = fvar_point_float ?cancel ?grain fvar.refinement source_values in
      let output = Array.make (Array.length fvar.output_point_of_vertex) 0. in
      run ?grain ?cancel (Array.length output) (fun vertex ->
        output.(vertex) <- refined.(fvar.output_point_of_vertex.(vertex)));
      output

let vertex_float ?cancel ?grain plan source =
  linear_vertex_float ?cancel ?grain plan source

let primitive_float ?cancel ?grain plan source =
  let count = Array.length plan.primitive_source in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun primitive ->
    output.(primitive) <- source.(plan.primitive_source.(primitive)));
  output

let discrete_map ?cancel ?grain plan owner source =
  let count, source_of = match owner with
    | Attribute.Point -> plan.output_points,
        (fun output -> plan.point_representative.(output))
    | Attribute.Vertex -> Bytes.length plan.vertex_kind,
        (fun output ->
          match Bytes.get plan.vertex_kind output with
          | '\000' -> plan.vertex_a.(output)
          | '\001' -> min plan.vertex_a.(output) plan.vertex_b.(output)
          | _ -> plan.source_topology.primitive_offsets.(plan.vertex_primitive.(output)))
    | Attribute.Primitive -> Array.length plan.primitive_source,
        (fun output -> plan.primitive_source.(output))
    | Attribute.Detail -> 1, (fun _ -> 0) in
  let output = Array.make count source.(0) in
  run ?grain ?cancel count (fun index -> output.(index) <- source.(source_of index));
  output

let vertex_edge_discrete_map ?cancel ?grain plan source default =
  let count = Bytes.length plan.vertex_kind in
  let output = Array.make count default in
  run ?grain ?cancel count (fun vertex ->
    let edge = plan.vertex_edge_source.(vertex) in
    if edge >= 0 then begin
      let directed = plan.index.edge_vertices.(plan.index.edge_offsets.(edge)) in
      output.(vertex) <- source.(directed)
    end);
  output

let numeric_map ?cancel ?grain plan owner source = match owner with
  | Attribute.Point -> point_float ?cancel ?grain plan source
  | Attribute.Vertex -> vertex_float ?cancel ?grain plan source
  | Attribute.Primitive -> primitive_float ?cancel ?grain plan source
  | Attribute.Detail -> Array.copy source

let remap_attribute ?cancel ?grain ?(generate_resulting_creases = true)
    ?(face_varying_interpolation = Subdivide_fvar_all) plan attribute =
  let owner = Attribute.owner attribute in
  if owner = Attribute.Vertex
     && plan.edge_ancestry_attribute = Some (Attribute.name attribute)
  then begin
    let values = match Attribute.View.storage attribute with
      | Attribute.Int values -> values
      | _ -> fail "internal edge ancestry must use integer storage" in
    Some (Attribute.create_owned ~name:(Attribute.name attribute)
      ~owner:Attribute.Vertex
      (Attribute.Int (vertex_edge_discrete_map ?cancel ?grain plan values (-1)))
      |> get_ok)
  end else if String.equal (Attribute.name attribute) "creaseweight"
     && (owner = Attribute.Vertex || owner = Attribute.Primitive) then None
  else if String.equal (Attribute.name attribute) "cornerweight"
          && owner = Attribute.Point then begin
    let values = match Attribute.View.storage attribute with
      | Attribute.Float values -> values
      | _ -> assert false in
    if not generate_resulting_creases
       || not (Array.exists (fun value -> value > 1.) values) then None
    else begin
      let output = Array.make plan.output_points 0. in
      run ?grain ?cancel plan.source_points (fun point ->
        output.(point) <- max 0. (values.(point) -. 1.));
      Some (Attribute.create_owned ~name:"cornerweight" ~owner:Attribute.Point
        (Attribute.Float output) |> get_ok)
    end
  end else
    let storage = match Attribute.View.storage attribute with
      | Attribute.Float values ->
          let values = if owner = Attribute.Vertex
              && face_varying_interpolation <> Subdivide_fvar_all
              && plan.scheme <> Bilinear then
            let fvar = fvar_refinement ?cancel ?grain
                face_varying_interpolation plan
                (fun left right -> Float.equal values.(left) values.(right)) in
            fvar_float ?cancel ?grain fvar values
          else numeric_map ?cancel ?grain plan owner values in
          Attribute.Float values
      | Attribute.Int values -> Attribute.Int (discrete_map ?cancel ?grain plan owner values)
      | Attribute.Text values -> Attribute.Text (discrete_map ?cancel ?grain plan owner values)
      | Attribute.Int_array values ->
          let mapping = discrete_map ?cancel ?grain plan owner
              (Array.init (Packed.Int_array.length values) Fun.id) in
          Attribute.Int_array (Ragged_ops.remap_int ?cancel ?grain mapping values)
      | Attribute.Float_array values ->
          let mapping = discrete_map ?cancel ?grain plan owner
              (Array.init (Packed.Float_array.length values) Fun.id) in
          Attribute.Float_array
            (Ragged_ops.remap_float ?cancel ?grain mapping values)
      | Attribute.Float2 values ->
          let view = Packed.Float2.View.view values in
          let fvar = if owner = Attribute.Vertex
              && face_varying_interpolation <> Subdivide_fvar_all
              && plan.scheme <> Bilinear then
            Some (fvar_refinement ?cancel ?grain
              face_varying_interpolation plan (fun left right ->
                Float.equal view.x.(left) view.x.(right)
                && Float.equal view.y.(left) view.y.(right)))
          else None in
          let map values = match fvar with
            | None -> numeric_map ?cancel ?grain plan owner values
            | Some fvar -> fvar_float ?cancel ?grain fvar values in
          Attribute.Float2 (Packed.Float2.of_owned
            ~x:(map view.x) ~y:(map view.y) |> get_ok)
      | Attribute.Float3 values ->
          let view = Packed.Float3.View.view values in
          let fvar = if owner = Attribute.Vertex
              && face_varying_interpolation <> Subdivide_fvar_all
              && plan.scheme <> Bilinear then
            Some (fvar_refinement ?cancel ?grain
              face_varying_interpolation plan (fun left right ->
                Float.equal view.x.(left) view.x.(right)
                && Float.equal view.y.(left) view.y.(right)
                && Float.equal view.z.(left) view.z.(right)))
          else None in
          let map values = match fvar with
            | None -> numeric_map ?cancel ?grain plan owner values
            | Some fvar -> fvar_float ?cancel ?grain fvar values in
          Attribute.Float3 (Packed.Float3.View.of_owned_exn
            ~x:(map view.x) ~y:(map view.y) ~z:(map view.z))
      | Attribute.Float4 values ->
          let view = Packed.Float4.View.view values in
          let fvar = if owner = Attribute.Vertex
              && face_varying_interpolation <> Subdivide_fvar_all
              && plan.scheme <> Bilinear then
            Some (fvar_refinement ?cancel ?grain
              face_varying_interpolation plan (fun left right ->
                Float.equal view.x.(left) view.x.(right)
                && Float.equal view.y.(left) view.y.(right)
                && Float.equal view.z.(left) view.z.(right)
                && Float.equal view.w.(left) view.w.(right)))
          else None in
          let map values = match fvar with
            | None -> numeric_map ?cancel ?grain plan owner values
            | Some fvar -> fvar_float ?cancel ?grain fvar values in
          Attribute.Float4 (Packed.Float4.of_owned
            ~x:(map view.x) ~y:(map view.y) ~z:(map view.z)
            ~w:(map view.w) |> get_ok) in
    Some (Attribute.create_owned ~name:(Attribute.name attribute) ~owner storage |> get_ok)

let child_crease_sharpness plan =
  match plan.creases with
  | None -> None
  | Some creases when not creases.has_edge_creases -> None
  | Some creases ->
      if Array.exists (fun value -> value > 0.) creases.child_edge_sharpness
      then Some creases.child_edge_sharpness else None

let output_child_slot plan vertex source_edge =
  let topology = Topology.View.view plan.topology in
  let primitive = match plan.scheme with
    | Catmull_clark | Bilinear -> vertex / 4
    | Loop -> vertex / 3 in
  let next = if vertex + 1 = topology.primitive_offsets.(primitive + 1)
    then topology.primitive_offsets.(primitive) else vertex + 1 in
  let a = topology.vertex_points.(vertex)
  and b = topology.vertex_points.(next) in
  let point = if a < plan.source_points then a
    else if b < plan.source_points then b
    else fail "subdivision child edge omitted its source endpoint" in
  let side = if point = plan.index.edge_a.(source_edge) then 0
    else if point = plan.index.edge_b.(source_edge) then 1
    else fail "subdivision child edge endpoint ancestry is inconsistent" in
  (source_edge * 2) + side

let resulting_crease_attribute ?cancel ?grain plan child_sharpness =
  match child_sharpness with
  | None -> None
  | Some child_sharpness ->
    let count = Array.length plan.vertex_edge_source in
    let values = Array.make count 0. in
    run ?grain ?cancel count (fun vertex ->
      let edge = plan.vertex_edge_source.(vertex) in
      if edge >= 0 then
        values.(vertex) <- child_sharpness.(output_child_slot plan vertex edge));
    Some (Attribute.create_owned ~name:"creaseweight" ~owner:Attribute.Vertex
      (Attribute.Float values) |> get_ok)

type output_edge_plan = {
  child_edges : int array;
  output_edge_count : int;
}

let output_edge_plan ?cancel plan =
  let internal_count = match plan.scheme with
    | Catmull_clark | Bilinear -> plan.source_vertices
    | Loop -> checked_mul "subdivision internal edge count"
        plan.source_primitives 3 in
  let child_count = checked_mul "subdivision child edge count" plan.edge_count 2 in
  let logical_count = checked_add "subdivision edge count" child_count internal_count in
  let ordinal_of_key = Array.make logical_count (-1) in
  let next_ordinal = ref 0 in
  let topology = Topology.View.view plan.topology in
  for vertex = 0 to Array.length topology.vertex_points - 1 do
    Cancel.poll cancel vertex 16_383;
      let source_edge = plan.vertex_edge_source.(vertex) in
      let key = if source_edge >= 0 then begin
      output_child_slot plan vertex source_edge
    end else match plan.scheme with
      | Catmull_clark | Bilinear ->
          let output_primitive = vertex / 4 in
          let source_vertex = plan.vertex_a.(output_primitive * 4) in
          let slot = vertex land 3 in
          let source_vertex = if slot = 1 then source_vertex
            else if slot = 2 then plan.index.previous_vertex.(source_vertex)
            else fail "Catmull-Clark internal edge classification failed" in
          child_count + source_vertex
      | Loop ->
          let output_source = vertex / 12 and slot = vertex mod 12 in
          let source_primitive = plan.primitive_source.(output_source * 4) in
          let local = match slot with
            | 1 | 11 -> 0
            | 4 | 9 -> 1
            | 7 | 10 -> 2
            | _ -> fail "Loop internal edge classification failed" in
          child_count + (source_primitive * 3) + local in
    if ordinal_of_key.(key) < 0 then begin
      ordinal_of_key.(key) <- !next_ordinal;
      incr next_ordinal
    end
  done;
  { child_edges = Array.sub ordinal_of_key 0 child_count;
    output_edge_count = !next_ordinal }

let set_edge_bit bits edge =
  let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
  Bytes.set bits byte (Char.chr (Char.code (Bytes.get bits byte) lor mask))

let edge_group_from_source ?cancel plan edge_plan ~name selected =
  let bits = Bytes.make ((edge_plan.output_edge_count + 7) / 8) '\000' in
  for edge = 0 to plan.edge_count - 1 do
    Cancel.poll cancel edge 16_383;
    if selected edge then begin
      let left = edge_plan.child_edges.(edge * 2)
      and right = edge_plan.child_edges.((edge * 2) + 1) in
      if left >= 0 then set_edge_bit bits left;
      if right >= 0 then set_edge_bit bits right
    end
  done;
  Edge_group.View.of_owned_bits ~topology:plan.topology
    ~edge_count:edge_plan.output_edge_count ~name bits

let resulting_crease_group ?cancel ~name plan edge_plan child_sharpness =
  let bits = Bytes.make ((edge_plan.output_edge_count + 7) / 8) '\000' in
  Option.iter (fun sharpness ->
    for child = 0 to Array.length sharpness - 1 do
      Cancel.poll cancel child 16_383;
      if sharpness.(child) > 0. then begin
        let output_edge = edge_plan.child_edges.(child) in
        if output_edge >= 0 then set_edge_bit bits output_edge
      end
    done) child_sharpness;
  Edge_group.View.of_owned_bits ~topology:plan.topology
    ~edge_count:edge_plan.output_edge_count ~name bits

let remap_group ?cancel ?grain plan group =
  let owner = Group.owner group in
  let count = match owner with Group.Point -> plan.output_points
    | Group.Vertex -> Bytes.length plan.vertex_kind
    | Group.Primitive -> Array.length plan.primitive_source in
  let target = Group.init ?grain ~owner ~name:(Group.name group) count
      (fun output ->
        Cancel.poll cancel output 4095;
        match owner with
        | Group.Point ->
            if output < plan.source_points then Group.mem output group
            else if output < plan.source_points + plan.edge_count then begin
              let edge = output - plan.source_points in
              Group.mem plan.index.edge_a.(edge) group
              && Group.mem plan.index.edge_b.(edge) group
            end else begin
              let primitive = output - plan.face_offset in
              let first = plan.source_topology.primitive_offsets.(primitive)
              and last = plan.source_topology.primitive_offsets.(primitive + 1) in
              let all = ref true in
              for vertex = first to last - 1 do
                if not (Group.mem plan.source_topology.vertex_points.(vertex) group) then
                  all := false
              done;
              !all
            end
        | Group.Vertex ->
            (match Bytes.get plan.vertex_kind output with
             | '\000' -> Group.mem plan.vertex_a.(output) group
             | '\001' -> Group.mem plan.vertex_a.(output) group
                 && Group.mem plan.vertex_b.(output) group
             | _ ->
                 let primitive = plan.vertex_primitive.(output) in
                 let first = plan.source_topology.primitive_offsets.(primitive)
                 and last = plan.source_topology.primitive_offsets.(primitive + 1) in
                 let all = ref true in
                 for vertex = first to last - 1 do
                   if not (Group.mem vertex group) then all := false
                 done;
                 !all)
        | Group.Primitive -> Group.mem plan.primitive_source.(output) group) in
  if not (Group.is_ordered group) then target
  else
    let source_of_target = match owner with
      | Group.Primitive -> plan.primitive_source
      | Group.Point -> Array.init count (fun output ->
          if output < plan.source_points then output else -1)
      | Group.Vertex -> Array.init count (fun output ->
          if Bytes.get plan.vertex_kind output = '\000'
          then plan.vertex_a.(output) else -1) in
    Group.View.remap_order ~source:group ~source_of_target target

let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let same_attribute_slot left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)

let same_attribute_schema left right =
  same_attribute_slot left right
  && String.equal (Attribute.kind_name left) (Attribute.kind_name right)

let find_attribute_like template geometry =
  Geometry.attributes geometry
  |> List.find_opt (same_attribute_slot template)

let checked_total name values =
  List.fold_left (checked_add name) 0 values

let concatenate_part_attribute ?cancel ?(extra_sources = [||]) template parts =
  let owner = Attribute.owner template in
  if owner = Attribute.Detail then Some template
  else begin
    let counts = List.map (fun geometry -> owner_count geometry owner) parts in
    let base_total = checked_total "combined attribute cardinality" counts in
    let total = checked_add "combined attribute cardinality" base_total
        (Array.length extra_sources) in
    let attributes = List.map (fun geometry ->
      match find_attribute_like template geometry with
      | None -> None
      | Some attribute when same_attribute_schema template attribute ->
          Some attribute
      | Some _ -> fail (Printf.sprintf
          "attribute %s changes storage while combining a local subdivision"
          (Attribute.name template))) parts in
    let with_parts operation =
      let offset = ref 0 in
      List.iter2 (fun count attribute ->
        Cancel.check_opt cancel;
        operation !offset count attribute;
        offset := checked_add "combined attribute offset" !offset count)
        counts attributes
    in
    let copy_extras output =
      Array.iteri (fun extra source ->
        if source < 0 || source >= base_total then
          fail "combined attribute ancestry is out of range";
        output.(base_total + extra) <- output.(source)) extra_sources in
    let storage = match Attribute.View.storage template with
      | Attribute.Float _ ->
          let output = Array.make total 0. in
          with_parts (fun offset count -> function
            | None -> ()
            | Some attribute ->
                let values = match Attribute.View.storage attribute with
                  | Attribute.Float values -> values | _ -> assert false in
                if Array.length values <> count then fail "float attribute cardinality mismatch";
                Array.blit values 0 output offset count);
          copy_extras output;
          Attribute.Float output
      | Attribute.Int _ ->
          let output = Array.make total 0 in
          with_parts (fun offset count -> function
            | None -> ()
            | Some attribute ->
                let values = match Attribute.View.storage attribute with
                  | Attribute.Int values -> values | _ -> assert false in
                if Array.length values <> count then fail "integer attribute cardinality mismatch";
                Array.blit values 0 output offset count);
          copy_extras output;
          Attribute.Int output
      | Attribute.Text _ ->
          let output = Array.make total "" in
          with_parts (fun offset count -> function
            | None -> ()
            | Some attribute ->
                let values = match Attribute.View.storage attribute with
                  | Attribute.Text values -> values | _ -> assert false in
                if Array.length values <> count then fail "text attribute cardinality mismatch";
                Array.blit values 0 output offset count);
          copy_extras output;
          Attribute.Text output
      | Attribute.Float2 _ ->
          let x = Array.make total 0. and y = Array.make total 0. in
          with_parts (fun offset count -> function
            | None -> ()
            | Some attribute ->
                let values = match Attribute.View.storage attribute with
                  | Attribute.Float2 values -> Packed.Float2.View.view values
                  | _ -> assert false in
                if Array.length values.x <> count then fail "float2 attribute cardinality mismatch";
                Array.blit values.x 0 x offset count;
                Array.blit values.y 0 y offset count);
          copy_extras x; copy_extras y;
          Attribute.Float2 (Packed.Float2.of_owned ~x ~y |> get_ok)
      | Attribute.Float3 _ ->
          let x = Array.make total 0. and y = Array.make total 0.
          and z = Array.make total 0. in
          with_parts (fun offset count -> function
            | None -> ()
            | Some attribute ->
                let values = match Attribute.View.storage attribute with
                  | Attribute.Float3 values -> Packed.Float3.View.view values
                  | _ -> assert false in
                if Array.length values.x <> count then fail "float3 attribute cardinality mismatch";
                Array.blit values.x 0 x offset count;
                Array.blit values.y 0 y offset count;
                Array.blit values.z 0 z offset count);
          copy_extras x; copy_extras y; copy_extras z;
          Attribute.Float3 (Packed.Float3.View.of_owned_exn ~x ~y ~z)
      | Attribute.Float4 _ ->
          let x = Array.make total 0. and y = Array.make total 0.
          and z = Array.make total 0. and w = Array.make total 0. in
          with_parts (fun offset count -> function
            | None -> ()
            | Some attribute ->
                let values = match Attribute.View.storage attribute with
                  | Attribute.Float4 values -> Packed.Float4.View.view values
                  | _ -> assert false in
                if Array.length values.x <> count then fail "float4 attribute cardinality mismatch";
                Array.blit values.x 0 x offset count;
                Array.blit values.y 0 y offset count;
                Array.blit values.z 0 z offset count;
                Array.blit values.w 0 w offset count);
          copy_extras x; copy_extras y; copy_extras z; copy_extras w;
          Attribute.Float4 (Packed.Float4.of_owned ~x ~y ~z ~w |> get_ok)
      | Attribute.Int_array _ ->
          let value_total = checked_total "combined integer-array payload"
              (List.map (function None -> 0 | Some attribute ->
                match Attribute.View.storage attribute with
                | Attribute.Int_array values -> Packed.Int_array.value_count values
                | _ -> assert false) attributes) in
          let extra_values = Array.fold_left (fun total source ->
              if source < 0 || source >= base_total then
                fail "combined integer-array ancestry is out of range";
              let rec locate offset counts attributes = match counts, attributes with
                | count :: counts, attribute :: attributes ->
                    if source < offset + count then
                      (match attribute with None -> 0 | Some attribute ->
                        let values = match Attribute.View.storage attribute with
                          | Attribute.Int_array values ->
                              Packed.Int_array.View.view values
                          | _ -> assert false in
                        values.offsets.(source - offset + 1)
                        - values.offsets.(source - offset))
                    else locate (offset + count) counts attributes
                | [], [] -> fail "combined integer-array ancestry is out of range"
                | _ -> assert false in
              checked_add "combined integer-array payload" total
                (locate 0 counts attributes)) 0 extra_sources in
          let value_total = checked_add "combined integer-array payload"
              value_total extra_values in
          let offsets = Array.make (checked_add "combined integer-array offsets" total 1) 0
          and values = Array.make value_total 0 in
          let row_at = ref 0 and value_at = ref 0 in
          List.iter2 (fun count -> function
            | None ->
                for row = 0 to count - 1 do
                  offsets.(!row_at + row + 1) <- !value_at
                done;
                row_at := !row_at + count
            | Some attribute ->
                let source = match Attribute.View.storage attribute with
                  | Attribute.Int_array values -> Packed.Int_array.View.view values
                  | _ -> assert false in
                if Array.length source.offsets <> count + 1 then
                  fail "integer-array attribute cardinality mismatch";
                for row = 0 to count - 1 do
                  Cancel.poll cancel row 4095;
                  offsets.(!row_at + row + 1) <- !value_at + source.offsets.(row + 1)
                done;
                Array.blit source.values 0 values !value_at (Array.length source.values);
                row_at := !row_at + count;
                value_at := !value_at + Array.length source.values) counts attributes;
          Array.iteri (fun extra source ->
            let first = offsets.(source) and last = offsets.(source + 1) in
            let length = last - first in
            Array.blit values first values !value_at length;
            value_at := !value_at + length;
            offsets.(base_total + extra + 1) <- !value_at) extra_sources;
          Attribute.Int_array
            (Packed.Int_array.View.create_validated_owned ~offsets ~values)
      | Attribute.Float_array _ ->
          let value_total = checked_total "combined float-array payload"
              (List.map (function None -> 0 | Some attribute ->
                match Attribute.View.storage attribute with
                | Attribute.Float_array values -> Packed.Float_array.value_count values
                | _ -> assert false) attributes) in
          let extra_values = Array.fold_left (fun total source ->
              if source < 0 || source >= base_total then
                fail "combined float-array ancestry is out of range";
              let rec locate offset counts attributes = match counts, attributes with
                | count :: counts, attribute :: attributes ->
                    if source < offset + count then
                      (match attribute with None -> 0 | Some attribute ->
                        let values = match Attribute.View.storage attribute with
                          | Attribute.Float_array values ->
                              Packed.Float_array.View.view values
                          | _ -> assert false in
                        values.offsets.(source - offset + 1)
                        - values.offsets.(source - offset))
                    else locate (offset + count) counts attributes
                | [], [] -> fail "combined float-array ancestry is out of range"
                | _ -> assert false in
              checked_add "combined float-array payload" total
                (locate 0 counts attributes)) 0 extra_sources in
          let value_total = checked_add "combined float-array payload"
              value_total extra_values in
          let offsets = Array.make (checked_add "combined float-array offsets" total 1) 0
          and values = Array.make value_total 0. in
          let row_at = ref 0 and value_at = ref 0 in
          List.iter2 (fun count -> function
            | None ->
                for row = 0 to count - 1 do
                  offsets.(!row_at + row + 1) <- !value_at
                done;
                row_at := !row_at + count
            | Some attribute ->
                let source = match Attribute.View.storage attribute with
                  | Attribute.Float_array values -> Packed.Float_array.View.view values
                  | _ -> assert false in
                if Array.length source.offsets <> count + 1 then
                  fail "float-array attribute cardinality mismatch";
                for row = 0 to count - 1 do
                  Cancel.poll cancel row 4095;
                  offsets.(!row_at + row + 1) <- !value_at + source.offsets.(row + 1)
                done;
                Array.blit source.values 0 values !value_at (Array.length source.values);
                row_at := !row_at + count;
                value_at := !value_at + Array.length source.values) counts attributes;
          Array.iteri (fun extra source ->
            let first = offsets.(source) and last = offsets.(source + 1) in
            let length = last - first in
            Array.blit values first values !value_at length;
            value_at := !value_at + length;
            offsets.(base_total + extra + 1) <- !value_at) extra_sources;
          Attribute.Float_array
            (Packed.Float_array.View.create_validated_owned ~offsets ~values)
    in
    Some (Attribute.create_owned ~name:(Attribute.name template) ~owner storage
      |> get_ok)
  end

type triangle_append = {
  triangle_points : int array;
  triangle_vertex_sources : int array;
  triangle_primitive_sources : int array;
}

type point_weld = {
  old_to_output : int array;
  output_source : int array;
  merge_offsets : int array;
  merge_members : int array;
  bias : float;
}

let locate_part offsets source =
  let part = ref 0 in
  while !part + 1 < Array.length offsets
      && source >= offsets.(!part + 1) do
    incr part
  done;
  if !part + 1 >= Array.length offsets || source < offsets.(!part) then
    fail "combined point ancestry is out of range";
  !part, source - offsets.(!part)

let welded_float_plane ?cancel ?grain weld point_offsets sources =
  let count = Array.length weld.output_source in
  let value source =
    let part, local = locate_part point_offsets source in
    match sources.(part) with None -> 0. | Some values -> values.(local) in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun point ->
    let first = weld.merge_offsets.(point)
    and last = weld.merge_offsets.(point + 1) in
    let coarse = value weld.output_source.(point) in
    if first = last then output.(point) <- coarse
    else begin
      let sum = ref 0. in
      for at = first to last - 1 do sum := !sum +. value weld.merge_members.(at) done;
      let refined = !sum /. float_of_int (last - first) in
      output.(point) <- ((1. -. weld.bias) *. coarse) +. (weld.bias *. refined)
    end);
  output

let welded_representatives weld =
  Array.mapi (fun point source ->
    let first = weld.merge_offsets.(point) in
    if first = weld.merge_offsets.(point + 1) || weld.bias < 0.5 then source
    else weld.merge_members.(first)) weld.output_source

let concatenate_welded_point_attribute ?cancel ?grain template parts
    point_offsets weld =
  let parts = Array.of_list parts in
  let attributes = Array.map (fun geometry ->
    match find_attribute_like template geometry with
    | None -> None
    | Some attribute when same_attribute_schema template attribute ->
        Some attribute
    | Some _ -> fail (Printf.sprintf
        "attribute %s changes storage while combining a local subdivision"
        (Attribute.name template))) parts in
  let representatives = lazy (welded_representatives weld) in
  let representative_rows storage =
    let mapping = Lazy.force representatives in
    Array.map (fun source ->
      let part, local = locate_part point_offsets source in
      match attributes.(part) with
      | None -> None
      | Some attribute -> Some (local, storage attribute)) mapping in
  let storage = match Attribute.View.storage template with
    | Attribute.Float _ ->
        let sources = Array.map (Option.map (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Float values -> values | _ -> assert false)) attributes in
        Attribute.Float (welded_float_plane ?cancel ?grain weld point_offsets sources)
    | Attribute.Int _ ->
        let rows = representative_rows (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Int values -> values | _ -> assert false) in
        Attribute.Int (Array.map (function None -> 0
          | Some (local, values) -> values.(local)) rows)
    | Attribute.Text _ ->
        let rows = representative_rows (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Text values -> values | _ -> assert false) in
        Attribute.Text (Array.map (function None -> ""
          | Some (local, values) -> values.(local)) rows)
    | Attribute.Float2 _ ->
        let plane select = Array.map (Option.map (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Float2 values -> select (Packed.Float2.View.view values)
          | _ -> assert false)) attributes in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float2.View.x)))
          ~y:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float2.View.y))) |> get_ok)
    | Attribute.Float3 _ ->
        let plane select = Array.map (Option.map (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Float3 values -> select (Packed.Float3.View.view values)
          | _ -> assert false)) attributes in
        Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float3.View.x)))
          ~y:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float3.View.y)))
          ~z:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float3.View.z))))
    | Attribute.Float4 _ ->
        let plane select = Array.map (Option.map (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Float4 values -> select (Packed.Float4.View.view values)
          | _ -> assert false)) attributes in
        Attribute.Float4 (Packed.Float4.of_owned
          ~x:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float4.View.x)))
          ~y:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float4.View.y)))
          ~z:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float4.View.z)))
          ~w:(welded_float_plane ?cancel ?grain weld point_offsets
            (plane (fun values -> values.Packed.Float4.View.w))) |> get_ok)
    | Attribute.Int_array _ ->
        let rows : (int * Packed.Int_array.View.view) option array =
          representative_rows (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Int_array values -> Packed.Int_array.View.view values
          | _ -> assert false) in
        let offsets = Array.make (Array.length rows + 1) 0 in
        Array.iteri (fun row -> function
          | None -> offsets.(row + 1) <- offsets.(row)
          | Some (local, (values : Packed.Int_array.View.view)) ->
              offsets.(row + 1) <- checked_add "combined integer-array payload"
                  offsets.(row) (values.offsets.(local + 1) - values.offsets.(local))) rows;
        let values = Array.make offsets.(Array.length rows) 0 in
        Array.iteri (fun row -> function
          | None -> ()
          | Some (local, (source : Packed.Int_array.View.view)) ->
              let first = source.offsets.(local) in
              Array.blit source.values first values offsets.(row)
                (source.offsets.(local + 1) - first)) rows;
        Attribute.Int_array
          (Packed.Int_array.View.create_validated_owned ~offsets ~values)
    | Attribute.Float_array _ ->
        let rows : (int * Packed.Float_array.View.view) option array =
          representative_rows (fun attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Float_array values -> Packed.Float_array.View.view values
          | _ -> assert false) in
        let offsets = Array.make (Array.length rows + 1) 0 in
        Array.iteri (fun row -> function
          | None -> offsets.(row + 1) <- offsets.(row)
          | Some (local, (values : Packed.Float_array.View.view)) ->
              offsets.(row + 1) <- checked_add "combined float-array payload"
                  offsets.(row) (values.offsets.(local + 1) - values.offsets.(local))) rows;
        let values = Array.make offsets.(Array.length rows) 0. in
        Array.iteri (fun row -> function
          | None -> ()
          | Some (local, (source : Packed.Float_array.View.view)) ->
              let first = source.offsets.(local) in
              Array.blit source.values first values offsets.(row)
                (source.offsets.(local + 1) - first)) rows;
        Attribute.Float_array
          (Packed.Float_array.View.create_validated_owned ~offsets ~values) in
  Attribute.create_owned ~name:(Attribute.name template) ~owner:Attribute.Point
    storage |> get_ok

let combine_parts ?cancel ?grain ?append ?weld ~source parts =
  let base_point_count = checked_total "combined point count"
      (List.map Geometry.point_count parts)
  and base_vertex_count = checked_total "combined vertex count"
      (List.map Geometry.vertex_count parts)
  and base_primitive_count = checked_total "combined primitive count"
      (List.map Geometry.primitive_count parts) in
  let added_vertices, added_primitives = match append with
    | None -> 0, 0
    | Some append ->
        let vertices = Array.length append.triangle_points in
        if vertices mod 3 <> 0
           || Array.length append.triangle_vertex_sources <> vertices
           || Array.length append.triangle_primitive_sources <> vertices / 3 then
          fail "combined stitch triangle storage is inconsistent";
        vertices, vertices / 3 in
  let parts_array = Array.of_list parts in
  let point_offsets = Array.make (Array.length parts_array + 1) 0 in
  Array.iteri (fun part geometry ->
    point_offsets.(part + 1) <- checked_add "combined point offsets"
        point_offsets.(part) (Geometry.point_count geometry)) parts_array;
  let point_count = match weld with
    | None -> base_point_count
    | Some weld ->
        if Array.length weld.old_to_output <> base_point_count
            || Array.length weld.merge_offsets
               <> Array.length weld.output_source + 1 then
          fail "combined point weld cardinality is inconsistent";
        Array.length weld.output_source in
  let vertex_count = checked_add "combined vertex count" base_vertex_count
      added_vertices
  and primitive_count = checked_add "combined primitive count"
      base_primitive_count added_primitives in
  let px = Array.make point_count 0. and py = Array.make point_count 0.
  and pz = Array.make point_count 0.
  and vertex_points = Array.make vertex_count 0
  and primitive_offsets = Array.make (checked_add "combined primitive offsets"
      primitive_count 1) 0
  and primitive_kinds = Bytes.make primitive_count '\000' in
  let point_at = ref 0 and vertex_at = ref 0 and primitive_at = ref 0 in
  List.iter (fun geometry ->
    Cancel.check_opt cancel;
    let positions = Packed.Float3.View.view (Geometry.positions geometry)
    and topology = Topology.View.view (Geometry.topology geometry) in
    let points = Geometry.point_count geometry
    and vertices = Geometry.vertex_count geometry
    and primitives = Geometry.primitive_count geometry in
    (match weld with
     | None ->
         Array.blit positions.x 0 px !point_at points;
         Array.blit positions.y 0 py !point_at points;
         Array.blit positions.z 0 pz !point_at points
     | Some _ -> ());
    run ?grain ?cancel vertices (fun vertex ->
      let source_point = topology.vertex_points.(vertex) + !point_at in
      vertex_points.(!vertex_at + vertex) <- match weld with
        | None -> source_point
        | Some weld -> weld.old_to_output.(source_point));
    run ?grain ?cancel primitives (fun primitive ->
      primitive_offsets.(!primitive_at + primitive + 1) <-
        topology.primitive_offsets.(primitive + 1) + !vertex_at;
      Bytes.set primitive_kinds (!primitive_at + primitive)
        (Bytes.get topology.primitive_kinds primitive));
    point_at := !point_at + points;
    vertex_at := !vertex_at + vertices;
    primitive_at := !primitive_at + primitives) parts;
  Option.iter (fun append ->
    (match weld with
     | None -> Array.blit append.triangle_points 0 vertex_points
         base_vertex_count added_vertices
     | Some weld -> run ?grain ?cancel added_vertices (fun added ->
         vertex_points.(base_vertex_count + added) <-
           weld.old_to_output.(append.triangle_points.(added))));
    run ?grain ?cancel added_primitives (fun added ->
      primitive_offsets.(base_primitive_count + added + 1) <-
        base_vertex_count + ((added + 1) * 3))) append;
  (match weld with
   | None -> ()
   | Some weld ->
       let position_sources = Array.map (fun geometry ->
         let values = Packed.Float3.View.view (Geometry.positions geometry) in
         Some values) parts_array in
       let plane select = Array.map (Option.map select) position_sources in
       Array.blit (welded_float_plane ?cancel ?grain weld point_offsets
           (plane (fun values -> values.Packed.Float3.View.x))) 0 px 0 point_count;
       Array.blit (welded_float_plane ?cancel ?grain weld point_offsets
           (plane (fun values -> values.Packed.Float3.View.y))) 0 py 0 point_count;
       Array.blit (welded_float_plane ?cancel ?grain weld point_offsets
           (plane (fun values -> values.Packed.Float3.View.z))) 0 pz 0 point_count);
  let topology = Topology.View.create_validated_owned ~point_count
      ~vertex_points ~primitive_offsets ~primitive_kinds in
  let keep_template _attribute = true in
  let source_templates = List.filter keep_template (Geometry.attributes source) in
  let extra_templates = ref [] in
  List.iter (fun geometry ->
    List.iter (fun attribute ->
      if keep_template attribute
         && not (List.exists (same_attribute_slot attribute) source_templates)
         && not (List.exists (same_attribute_slot attribute) !extra_templates)
      then extra_templates := attribute :: !extra_templates)
      (Geometry.attributes geometry)) parts;
  let templates = source_templates @ List.rev !extra_templates in
  let attributes = List.filter_map
      (fun template ->
        let extra_sources = match append, Attribute.owner template with
          | Some append, Attribute.Vertex -> append.triangle_vertex_sources
          | Some append, Attribute.Primitive -> append.triangle_primitive_sources
          | None, _ | Some _, (Attribute.Point | Attribute.Detail) -> [||] in
        match weld, Attribute.owner template with
        | Some weld, Attribute.Point ->
            Some (concatenate_welded_point_attribute ?cancel ?grain template
              parts point_offsets weld)
        | _ -> concatenate_part_attribute ?cancel ~extra_sources template parts)
      templates in
  let groups = List.map (fun template ->
    let owner = Group.owner template in
    let count geometry = match owner with
      | Group.Point -> Geometry.point_count geometry
      | Group.Vertex -> Geometry.vertex_count geometry
      | Group.Primitive -> Geometry.primitive_count geometry in
    let base_total = checked_total "combined group cardinality"
        (List.map count parts) in
    let extra_sources = match append, owner with
      | Some append, Group.Vertex -> append.triangle_vertex_sources
      | Some append, Group.Primitive -> append.triangle_primitive_sources
      | None, _ | Some _, Group.Point -> [||] in
    let total = match weld, owner with
      | Some weld, Group.Point -> Array.length weld.output_source
      | _ -> checked_add "combined group cardinality" base_total
          (Array.length extra_sources) in
    let builder = Group.Builder.create ~owner ~name:(Group.name template) base_total in
    let offset = ref 0 and ordered = ref false in
    let sources = List.map (fun geometry ->
      let group = match Geometry.find_group ~owner (Group.name template) geometry with
        | Some group -> group
        | None -> fail "local subdivision dropped a group" in
      Group.iter (fun element -> Group.Builder.set builder (!offset + element) true) group;
      if Group.is_ordered group then ordered := true;
      let result = !offset, group in
      offset := !offset + count geometry;
      result) parts in
    let base_target = Group.Builder.freeze builder in
    let target = match weld, owner with
      | Some weld, Group.Point ->
          Group.init ?grain ~owner ~name:(Group.name template) total (fun output ->
            let selected = ref (Group.mem weld.output_source.(output) base_target) in
            for at = weld.merge_offsets.(output)
                to weld.merge_offsets.(output + 1) - 1 do
              if Group.mem weld.merge_members.(at) base_target then selected := true
            done;
            !selected)
      | _ when Array.length extra_sources = 0 -> base_target
      | _ -> Group.init ?grain ~owner ~name:(Group.name template) total
          (fun output ->
            if output < base_total then Group.mem output base_target
            else Group.mem extra_sources.(output - base_total) base_target) in
    if not !ordered then target
    else begin
      let ancestry = match weld, owner with
        | Some weld, Group.Point -> Array.init total (fun output ->
            let primary = weld.output_source.(output) in
            if Group.mem primary base_target then primary
            else begin
              let selected = ref (-1) in
              for at = weld.merge_offsets.(output)
                  to weld.merge_offsets.(output + 1) - 1 do
                let source = weld.merge_members.(at) in
                if !selected < 0 && Group.mem source base_target then
                  selected := source
              done;
              !selected
            end)
        | _ -> Array.init total (fun output ->
            if output < base_total then output
            else extra_sources.(output - base_total)) in
      let base_ordered =
        let order = Array.make (Group.cardinality base_target) 0 and at = ref 0 in
        List.iter (fun (offset, group) ->
          Group.iter_ordered (fun element ->
            order.(!at) <- offset + element; incr at) group) sources;
        Group.View.with_owned_order order base_target in
      Group.View.remap_order ~source:base_ordered
        ~source_of_target:ancestry target
    end) (Geometry.groups source) in
  let edge_groups = match Geometry.edge_groups source with
    | [] -> []
    | templates ->
        let target_index = Topology_index.create ?cancel topology in
        List.map (fun template ->
          let builder = Edge_group.Builder.create ~topology ~index:target_index
              ~name:(Edge_group.name template) in
          let point_offset = ref 0 in
          List.iter (fun geometry ->
            let source_topology = Geometry.topology geometry in
            let source_index = Topology_index.create ?cancel source_topology in
            let group = match Geometry.find_edge_group
                (Edge_group.name template) geometry with
              | Some group -> group
              | None -> fail "local subdivision dropped an edge group" in
            Edge_group.iter (fun edge ->
              let a, b = Topology_index.edge_points source_index edge in
              let a = a + !point_offset and b = b + !point_offset in
              let a, b = match weld with
                | None -> a, b
                | Some weld -> weld.old_to_output.(a), weld.old_to_output.(b) in
              match Topology_index.find_edge target_index
                  ~a ~b with
              | None -> fail "local subdivision edge ancestry is inconsistent"
              | Some target -> Edge_group.Builder.set builder target true) group;
            point_offset := !point_offset + Geometry.point_count geometry) parts;
          Edge_group.Builder.freeze builder) templates in
  let positions = Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz in
  Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups () |> get_ok

let once ?cancel ?grain ?edge_ancestry_attribute ?edge_crease_override
    ?(generate_resulting_creases = true) ?resulting_crease_group_name
    ?(remove_holes = true)
    ?(boundary_interpolation = Subdivide_boundary_edge_only)
    ?(face_varying_interpolation = Subdivide_fvar_all)
    ?(triangle_subdivision = Subdivide_triangles_catmull_clark)
    ?(creasing_method = Subdivide_creasing_uniform) scheme geometry =
  let holes = if not remove_holes then None
    else Geometry.find_group ~owner:Group.Primitive "subdivision_hole" geometry in
  Option.iter (fun holes -> validate_hole_group holes geometry) holes;
  let plan = make_plan ?cancel ?grain ?edge_ancestry_attribute
      ?edge_crease_override ?holes
      boundary_interpolation triangle_subdivision creasing_method scheme geometry in
  let positions_source = Packed.Float3.View.view (Geometry.positions geometry) in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(point_float ?cancel ?grain plan positions_source.x)
      ~y:(point_float ?cancel ?grain plan positions_source.y)
      ~z:(point_float ?cancel ?grain plan positions_source.z) in
  let attributes = Geometry.attributes geometry
    |> List.filter_map (remap_attribute ?cancel ?grain
        ~generate_resulting_creases ~face_varying_interpolation plan) in
  let child_sharpness = if generate_resulting_creases
      then child_crease_sharpness plan else None in
  let attributes = match if generate_resulting_creases
      then resulting_crease_attribute ?cancel ?grain plan child_sharpness
      else None with
    | None -> attributes | Some attribute -> attributes @ [attribute] in
  let groups = Geometry.groups geometry |> List.map (remap_group ?cancel ?grain plan) in
  let source_groups = Geometry.edge_groups geometry in
  let edge_plan = match source_groups, resulting_crease_group_name with
    | [], None -> None
    | _ -> Some (output_edge_plan ?cancel plan) in
  let edge_groups = match source_groups, edge_plan with
    | [], _ -> []
    | _, None -> assert false
    | source_groups, Some edge_plan ->
        List.map (fun group ->
          if Edge_group.topology_data_id group
              <> Topology.data_id (Geometry.topology geometry) then
            fail "edge group belongs to a different topology";
          edge_group_from_source ?cancel plan edge_plan
            ~name:(Edge_group.name group) (fun edge -> Edge_group.mem edge group))
          source_groups in
  let edge_groups = match resulting_crease_group_name, edge_plan with
    | None, _ -> edge_groups
    | Some name, Some edge_plan ->
        let generated = resulting_crease_group ?cancel ~name plan edge_plan
            child_sharpness in
        List.filter (fun group -> not (String.equal (Edge_group.name group) name))
          edge_groups @ [generated]
    | Some _, None -> assert false in
  Geometry.create ~positions ~topology:plan.topology ~attributes ~groups
    ~edge_groups () |> get_ok

let validate_primitive_selection primitives geometry =
  if Group.owner primitives <> Group.Primitive then
    fail "selection must be a primitive group";
  let expected = Geometry.primitive_count geometry in
  if Group.length primitives <> expected then
    fail (Printf.sprintf
      "selection length %d does not match the primitive count %d"
      (Group.length primitives) expected)

let find_root parents value =
  let root = ref value in
  while parents.(!root) <> !root do root := parents.(!root) done;
  let current = ref value in
  while parents.(!current) <> !root do
    let next = parents.(!current) in
    parents.(!current) <- !root;
    current := next
  done;
  !root

let union_roots parents ranks left right =
  let left = find_root parents left and right = find_root parents right in
  if left <> right then
    if ranks.(left) < ranks.(right) then parents.(left) <- right
    else begin
      parents.(right) <- left;
      if ranks.(left) = ranks.(right) then ranks.(left) <- ranks.(left) + 1
    end

let select_point_array ?cancel ?grain mapping source =
  let count = Array.length mapping in
  if count = 0 then [||]
  else begin
    let output = Array.make count source.(mapping.(0)) in
    run ?grain ?cancel count (fun point -> output.(point) <- source.(mapping.(point)));
    output
  end

let duplicate_point_attribute ?cancel ?grain point_source attribute =
  if Attribute.owner attribute <> Attribute.Point then attribute
  else let storage = match Attribute.View.storage attribute with
    | Attribute.Float values ->
        Attribute.Float (select_point_array ?cancel ?grain point_source values)
    | Attribute.Int values ->
        Attribute.Int (select_point_array ?cancel ?grain point_source values)
    | Attribute.Text values ->
        Attribute.Text (select_point_array ?cancel ?grain point_source values)
    | Attribute.Float2 values ->
        let values = Packed.Float2.View.view values in
        Attribute.Float2 (Packed.Float2.of_owned
          ~x:(select_point_array ?cancel ?grain point_source values.x)
          ~y:(select_point_array ?cancel ?grain point_source values.y) |> get_ok)
    | Attribute.Float3 values ->
        let values = Packed.Float3.View.view values in
        Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:(select_point_array ?cancel ?grain point_source values.x)
          ~y:(select_point_array ?cancel ?grain point_source values.y)
          ~z:(select_point_array ?cancel ?grain point_source values.z))
    | Attribute.Float4 values ->
        let values = Packed.Float4.View.view values in
        Attribute.Float4 (Packed.Float4.of_owned
          ~x:(select_point_array ?cancel ?grain point_source values.x)
          ~y:(select_point_array ?cancel ?grain point_source values.y)
          ~z:(select_point_array ?cancel ?grain point_source values.z)
          ~w:(select_point_array ?cancel ?grain point_source values.w) |> get_ok)
    | Attribute.Int_array values ->
        Attribute.Int_array (Ragged_ops.remap_int ?cancel ?grain point_source values)
    | Attribute.Float_array values ->
        Attribute.Float_array (Ragged_ops.remap_float ?cancel ?grain point_source values) in
  Attribute.create_owned ~name:(Attribute.name attribute) ~owner:Attribute.Point
    storage |> get_ok

let split_disconnected_point_fans ?cancel ?grain geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  let vertex_count = Array.length topology.vertex_points in
  if vertex_count = 0 then geometry
  else begin
    let topology_index = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view topology_index in
    let parents = Array.init vertex_count Fun.id
    and ranks = Array.make vertex_count 0 in
    for edge = 0 to Array.length index.edge_a - 1 do
      Cancel.poll cancel edge 16_383;
      let first = index.edge_offsets.(edge) and last = index.edge_offsets.(edge + 1) in
      if last - first >= 2 then begin
        let reference = index.edge_vertices.(first) in
        let reference_next = index.next_vertex.(reference) in
        if reference_next < 0 then fail "topology index exposed an incomplete edge";
        let reference_a = topology.vertex_points.(reference) in
        for at = first + 1 to last - 1 do
          let vertex = index.edge_vertices.(at) in
          let next = index.next_vertex.(vertex) in
          if next < 0 then fail "topology index exposed an incomplete edge";
          if topology.vertex_points.(vertex) = reference_a then begin
            union_roots parents ranks reference vertex;
            union_roots parents ranks reference_next next
          end else begin
            union_roots parents ranks reference next;
            union_roots parents ranks reference_next vertex
          end
        done
      end
    done;
    let roots = Array.init vertex_count (find_root parents) in
    let root_target = Array.make vertex_count (-1)
    and root_source = Array.make vertex_count (-1) and extra = ref 0 in
    for point = 0 to topology.point_count - 1 do
      Cancel.poll cancel point 4095;
      let first = index.point_offsets.(point) and last = index.point_offsets.(point + 1) in
      let first_component = ref true in
      for at = first to last - 1 do
        let vertex = index.point_vertices.(at) in
        let root = roots.(vertex) in
        if root_target.(root) < 0 then begin
          root_source.(root) <- point;
          if !first_component then begin
            root_target.(root) <- point;
            first_component := false
          end else begin
            root_target.(root) <- checked_add "split-fan point index"
                topology.point_count !extra;
            incr extra
          end
        end
      done
    done;
    if !extra = 0 then geometry
    else begin
      let output_points = checked_add "split-fan point count" topology.point_count !extra in
      let vertex_points = Array.make vertex_count 0
      and point_source = Array.init output_points (fun point ->
        if point < topology.point_count then point else 0) in
      Array.iteri (fun root target ->
        if target >= topology.point_count then point_source.(target) <- root_source.(root))
        root_target;
      run ?grain ?cancel vertex_count (fun vertex ->
        let root = roots.(vertex) in
        let target = root_target.(root) in
        if target < 0 then fail "split-fan corner has no component";
        vertex_points.(vertex) <- target);
      let output_topology = Topology.View.create_validated_owned
          ~point_count:output_points ~vertex_points
          ~primitive_offsets:(Array.copy topology.primitive_offsets)
          ~primitive_kinds:(Bytes.copy topology.primitive_kinds) in
      let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
      let positions = Packed.Float3.View.of_owned_exn
          ~x:(select_point_array ?cancel ?grain point_source source_positions.x)
          ~y:(select_point_array ?cancel ?grain point_source source_positions.y)
          ~z:(select_point_array ?cancel ?grain point_source source_positions.z) in
      let attributes = Geometry.attributes geometry
        |> List.map (duplicate_point_attribute ?cancel ?grain point_source) in
      let groups = Geometry.groups geometry |> List.map (fun group ->
        if Group.owner group <> Group.Point then group
        else begin
          let target = Group.init ?grain ~owner:Group.Point ~name:(Group.name group)
              output_points (fun point ->
                Cancel.poll cancel point 4095;
                Group.mem point_source.(point) group) in
          Group.View.remap_order ~source:group ~source_of_target:point_source target
        end) in
      let edge_groups = match Geometry.edge_groups geometry with
        | [] -> []
        | source_groups ->
            let target_index = Topology_index.create ?cancel output_topology in
            List.map (fun group ->
              if Edge_group.topology_data_id group
                  <> Topology_index.topology_data_id topology_index then
                fail "edge group belongs to a different topology";
              let builder = Edge_group.Builder.create ~topology:output_topology
                  ~index:target_index ~name:(Edge_group.name group) in
              Edge_group.iter (fun edge ->
                for at = index.edge_offsets.(edge) to index.edge_offsets.(edge + 1) - 1 do
                  let vertex = index.edge_vertices.(at) in
                  let next = index.next_vertex.(vertex) in
                  if next < 0 then fail "topology index exposed an incomplete edge";
                  match Topology_index.find_edge target_index
                      ~a:vertex_points.(vertex) ~b:vertex_points.(next) with
                  | None -> fail "split-fan edge ancestry is inconsistent"
                  | Some target -> Edge_group.Builder.set builder target true
                done) group;
              Edge_group.Builder.freeze builder) source_groups in
      Geometry.create ~positions ~topology:output_topology ~attributes ~groups
        ~edge_groups () |> get_ok
    end
  end

type pull_plan = {
  ancestry_attribute : string;
  source_topology : Topology.t;
  source_index : Topology_index.t;
  source_positions : Packed.Float3.t;
  selected_edge_start : int array;
  unselected_primitive_of_edge : int array;
  unselected_point_of_source : int array;
  unselected_primitive_of_source : int array;
}

let fresh_edge_ancestry_name geometry =
  let rec choose suffix =
    let name = edge_ancestry_prefix ^ string_of_int suffix in
    match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
    | None -> name
    | Some _ -> choose (suffix + 1) in
  choose 0

let annotate_pull_interfaces ?cancel primitives geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  let topology_index = Topology_index.create ?cancel topology_value in
  let index = Topology_index.View.view topology_index in
  let interface_edges = Bytes.make (Array.length index.edge_a) '\000'
  and unselected_primitive_of_edge = Array.make (Array.length index.edge_a) (-1)
  and interface_count = ref 0 in
  for edge = 0 to Array.length index.edge_a - 1 do
    Cancel.poll cancel edge 16_383;
    let selected_count = ref 0 and unselected_count = ref 0
    and non_polygon = ref false in
    for at = index.edge_offsets.(edge) to index.edge_offsets.(edge + 1) - 1 do
      let primitive = index.primitive_of_vertex.(index.edge_vertices.(at)) in
      if Bytes.get topology.primitive_kinds primitive <> '\000' then
        non_polygon := true;
      if Group.mem primitive primitives then incr selected_count
      else begin
        incr unselected_count;
        if unselected_primitive_of_edge.(edge) < 0 then
          unselected_primitive_of_edge.(edge) <- primitive
      end
    done;
    if !selected_count > 0 && !unselected_count > 0 then begin
      if !selected_count <> 1 || !unselected_count <> 1 then
        fail (Printf.sprintf
          "crack closure requires one selected and one unselected face on interface edge %d"
          edge);
      if !non_polygon then
        fail "crack closure supports polygon surrounding faces only";
      Bytes.set interface_edges edge '\001'; incr interface_count
    end
  done;
  if !interface_count = 0 then geometry, None
  else begin
    let name = fresh_edge_ancestry_name geometry in
    let ancestry = Array.make (Array.length topology.vertex_points) (-1)
    and selected_edge_start = Array.make (Array.length index.edge_a) (-1) in
    for vertex = 0 to Array.length ancestry - 1 do
      Cancel.poll cancel vertex 16_383;
      let edge = index.edge_of_vertex.(vertex) in
      if edge >= 0 && Bytes.get interface_edges edge <> '\000' then begin
        ancestry.(vertex) <- edge;
        let primitive = index.primitive_of_vertex.(vertex) in
        if Group.mem primitive primitives then
          selected_edge_start.(edge) <- topology.vertex_points.(vertex)
      end
    done;
    let attribute = Attribute.create_owned ~name ~owner:Attribute.Vertex
        (Attribute.Int ancestry) |> get_ok in
    let annotated = Geometry.with_attribute attribute geometry |> get_ok in
    let unselected_used_points = Bytes.make topology.point_count '\000' in
    for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
      Cancel.poll cancel primitive 4095;
      if not (Group.mem primitive primitives) then
        for vertex = topology.primitive_offsets.(primitive)
            to topology.primitive_offsets.(primitive + 1) - 1 do
          Bytes.set unselected_used_points topology.vertex_points.(vertex) '\001'
        done
    done;
    let unselected_point_of_source = Array.make topology.point_count (-1)
    and point_at = ref 0 in
    for point = 0 to topology.point_count - 1 do
      if Bytes.get unselected_used_points point <> '\000' then begin
        unselected_point_of_source.(point) <- !point_at; incr point_at
      end
    done;
    let primitive_count = Bytes.length topology.primitive_kinds in
    let unselected_primitive_of_source = Array.make primitive_count (-1)
    and primitive_at = ref 0 in
    for primitive = 0 to primitive_count - 1 do
      if not (Group.mem primitive primitives) then begin
        unselected_primitive_of_source.(primitive) <- !primitive_at;
        incr primitive_at
      end
    done;
    annotated, Some { ancestry_attribute = name; source_topology = topology_value;
      source_index = topology_index;
      source_positions = Geometry.positions geometry; selected_edge_start;
      unselected_primitive_of_edge; unselected_point_of_source;
      unselected_primitive_of_source }
  end

type interface_chains = {
  edge_offsets : int array;
  points : int array;
  vertices : int array;
}

let interface_chains ?cancel plan refined =
  let ancestry = match Geometry.find_attribute ~owner:Attribute.Vertex
      plan.ancestry_attribute refined with
    | Some attribute ->
        (match Attribute.View.storage attribute with
         | Attribute.Int values -> values
         | _ -> fail "internal edge ancestry changed storage")
    | None -> fail "local subdivision dropped internal edge ancestry" in
  let topology = Topology.View.view (Geometry.topology refined) in
  if Array.length ancestry <> Array.length topology.vertex_points then
    fail "internal edge ancestry cardinality mismatch";
  let source_index = Topology_index.View.view plan.source_index in
  let source_edges = Array.length source_index.edge_a in
  let counts = Array.make source_edges 0 in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 4095;
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      let edge = ancestry.(vertex) in
      if edge >= 0 then begin
        if edge >= source_edges then fail "internal edge ancestry is out of range";
        counts.(edge) <- checked_add "interface descendant edge count"
            counts.(edge) 1
      end
    done
  done;
  let record_offsets = Array.make (source_edges + 1) 0
  and edge_offsets = Array.make (source_edges + 1) 0 in
  for edge = 0 to source_edges - 1 do
    record_offsets.(edge + 1) <- checked_add "interface descendant storage"
        record_offsets.(edge) counts.(edge);
    edge_offsets.(edge + 1) <- checked_add "interface chain storage"
        edge_offsets.(edge) (if counts.(edge) = 0 then 0 else counts.(edge) + 1)
  done;
  let descendants = record_offsets.(source_edges) in
  let starts = Array.make descendants 0 and ends = Array.make descendants 0
  and start_vertices = Array.make descendants 0
  and end_vertices = Array.make descendants 0
  and cursor = Array.copy record_offsets in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    for vertex = first to last - 1 do
      let edge = ancestry.(vertex) in
      if edge >= 0 then begin
        let next = if vertex + 1 = last then first else vertex + 1 in
        let at = cursor.(edge) in
        starts.(at) <- topology.vertex_points.(vertex);
        ends.(at) <- topology.vertex_points.(next);
        start_vertices.(at) <- vertex;
        end_vertices.(at) <- next;
        cursor.(edge) <- at + 1
      end
    done
  done;
  let points = Array.make edge_offsets.(source_edges) 0
  and vertices = Array.make edge_offsets.(source_edges) 0
  and next_point = Array.make topology.point_count (-1)
  and record_of_start = Array.make topology.point_count (-1)
  and incoming = Bytes.make topology.point_count '\000' in
  for edge = 0 to source_edges - 1 do
    let first = record_offsets.(edge) and last = record_offsets.(edge + 1) in
    let count = last - first in
    if count > 0 then begin
      Cancel.poll cancel edge 4095;
      for at = first to last - 1 do
        let start = starts.(at) and finish = ends.(at) in
        if next_point.(start) >= 0 || Bytes.get incoming finish <> '\000' then
          fail "interface descendants do not form a simple directed chain";
        next_point.(start) <- finish;
        record_of_start.(start) <- at;
        Bytes.set incoming finish '\001'
      done;
      let chain_start = ref (-1) in
      for at = first to last - 1 do
        let point = starts.(at) in
        if Bytes.get incoming point = '\000' then begin
          if !chain_start >= 0 && !chain_start <> point then
            fail "interface descendants contain multiple chains";
          chain_start := point
        end
      done;
      if !chain_start < 0 then fail "interface descendants form a closed loop";
      let output = edge_offsets.(edge) and current = ref !chain_start in
      for local = 0 to count - 1 do
        let record = record_of_start.(!current) in
        if record < first || record >= last then
          fail "interface descendant chain is disconnected";
        points.(output + local) <- !current;
        vertices.(output + local) <- start_vertices.(record);
        current := next_point.(!current);
        if local + 1 = count then begin
          points.(output + local + 1) <- !current;
          vertices.(output + local + 1) <- end_vertices.(record)
        end
      done;
      for at = first to last - 1 do
        let start = starts.(at) and finish = ends.(at) in
        next_point.(start) <- -1;
        record_of_start.(start) <- -1;
        Bytes.set incoming finish '\000'
      done
    end
  done;
  { edge_offsets; points; vertices }

let interpolate_array ?cancel ?grain left right weight source =
  let count = Array.length left in
  let output = Array.make count 0. in
  run ?grain ?cancel count (fun target ->
    let a = left.(target) and b = right.(target) in
    if a = b then output.(target) <- source.(a)
    else begin
      let t = weight.(target) in
      output.(target) <- ((1. -. t) *. source.(a)) +. (t *. source.(b))
    end);
  output

let representative_map left right weight =
  Array.mapi (fun target source ->
    if weight.(target) < 0.5 then source else right.(target)) left

let interpolate_owned_attribute ?cancel ?grain ~point_left ~point_right
    ~point_weight ~vertex_left ~vertex_right ~vertex_weight ~primitive_source
    attribute =
  let owner = Attribute.owner attribute in
  if owner = Attribute.Detail then attribute
  else begin
    let left, right, weight = match owner with
      | Attribute.Point -> point_left, point_right, point_weight
      | Attribute.Vertex -> vertex_left, vertex_right, vertex_weight
      | Attribute.Primitive ->
          primitive_source, primitive_source,
          Array.make (Array.length primitive_source) 0.
      | Attribute.Detail -> assert false in
    let representatives () = representative_map left right weight in
    let storage = match Attribute.View.storage attribute with
      | Attribute.Float values ->
          Attribute.Float (interpolate_array ?cancel ?grain left right weight values)
      | Attribute.Int values ->
          Attribute.Int (select_point_array ?cancel ?grain
            (representatives ()) values)
      | Attribute.Text values ->
          Attribute.Text (select_point_array ?cancel ?grain
            (representatives ()) values)
      | Attribute.Float2 values ->
          let values = Packed.Float2.View.view values in
          Attribute.Float2 (Packed.Float2.of_owned
            ~x:(interpolate_array ?cancel ?grain left right weight values.x)
            ~y:(interpolate_array ?cancel ?grain left right weight values.y)
            |> get_ok)
      | Attribute.Float3 values ->
          let values = Packed.Float3.View.view values in
          Attribute.Float3 (Packed.Float3.View.of_owned_exn
            ~x:(interpolate_array ?cancel ?grain left right weight values.x)
            ~y:(interpolate_array ?cancel ?grain left right weight values.y)
            ~z:(interpolate_array ?cancel ?grain left right weight values.z))
      | Attribute.Float4 values ->
          let values = Packed.Float4.View.view values in
          Attribute.Float4 (Packed.Float4.of_owned
            ~x:(interpolate_array ?cancel ?grain left right weight values.x)
            ~y:(interpolate_array ?cancel ?grain left right weight values.y)
            ~z:(interpolate_array ?cancel ?grain left right weight values.z)
            ~w:(interpolate_array ?cancel ?grain left right weight values.w)
            |> get_ok)
      | Attribute.Int_array values ->
          Attribute.Int_array
            (Ragged_ops.remap_int ?cancel ?grain (representatives ()) values)
      | Attribute.Float_array values ->
          Attribute.Float_array
            (Ragged_ops.remap_float ?cancel ?grain (representatives ()) values) in
    Attribute.create_owned ~name:(Attribute.name attribute) ~owner storage |> get_ok
  end

type divided_boundary = {
  geometry : Geometry.t;

  points : int array;
  vertices : int array;
  source_primitive_to_output : int array;
}

let divide_unselected_edges ?cancel ?grain plan (chains : interface_chains)
    source : divided_boundary =
  let topology = Topology.View.view plan.source_topology
  and source_index = Topology_index.View.view plan.source_index in
  let source_edges = Array.length source_index.edge_a in
  let edge_segments edge =
    let length = chains.edge_offsets.(edge + 1) - chains.edge_offsets.(edge) in
    if length = 0 then 0 else length - 1 in
  let base_points = Array.fold_left (fun count point ->
      if point < 0 then count else max count (point + 1)) 0
      plan.unselected_point_of_source
  and primitive_count = Array.fold_left (fun count primitive ->
      if primitive < 0 then count else max count (primitive + 1)) 0
      plan.unselected_primitive_of_source in
  let point_source = Array.make base_points 0 in
  Array.iteri (fun source target ->
    if target >= 0 then point_source.(target) <- source)
    plan.unselected_point_of_source;
  let point_base = Array.make source_edges (-1) and extra_points = ref 0 in
  for edge = 0 to source_edges - 1 do
    let segments = edge_segments edge in
    if segments > 0 then begin
      point_base.(edge) <- checked_add "divided boundary point base"
          base_points !extra_points;
      extra_points := checked_add "divided boundary point count"
          !extra_points (segments - 1)
    end
  done;
  let point_count = checked_add "divided boundary point count"
      base_points !extra_points in
  let point_left = Array.init point_count (fun point ->
      if point < base_points then point_source.(point) else 0)
  and point_right = Array.init point_count (fun point ->
      if point < base_points then point_source.(point) else 0)
  and point_weight = Array.make point_count 0.
  and chain_points = Array.make (Array.length chains.points) 0 in
  for edge = 0 to source_edges - 1 do
    let first = chains.edge_offsets.(edge)
    and last = chains.edge_offsets.(edge + 1) in
    let segments = last - first - 1 in
    if segments >= 1 then begin
      let source_a = plan.selected_edge_start.(edge) in
      if source_a < 0 then fail "divided boundary edge has no selected orientation";
      let edge_a = source_index.edge_a.(edge) and edge_b = source_index.edge_b.(edge) in
      let source_b = if source_a = edge_a then edge_b
        else if source_a = edge_b then edge_a
        else fail "divided boundary source orientation is inconsistent" in
      let coarse_a = plan.unselected_point_of_source.(source_a)
      and coarse_b = plan.unselected_point_of_source.(source_b) in
      if coarse_a < 0 || coarse_b < 0 then
        fail "divided boundary endpoint was removed from the coarse region";
      chain_points.(first) <- coarse_a;
      chain_points.(last - 1) <- coarse_b;
      for local = 1 to segments - 1 do
        let output = point_base.(edge) + local - 1 in
        chain_points.(first + local) <- output;
        point_left.(output) <- source_a;
        point_right.(output) <- source_b;
        point_weight.(output) <- float_of_int local /. float_of_int segments
      done
    end
  done;
  let vertex_output = Array.make (Array.length topology.vertex_points) (-1)
  and vertex_count = ref 0 in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    if plan.unselected_primitive_of_source.(primitive) >= 0 then
      for vertex = topology.primitive_offsets.(primitive)
          to topology.primitive_offsets.(primitive + 1) - 1 do
        vertex_output.(vertex) <- !vertex_count;
        incr vertex_count;
        let edge = source_index.edge_of_vertex.(vertex) in
        if edge >= 0 && edge_segments edge > 0 then
          vertex_count := checked_add "divided boundary vertex count"
              !vertex_count (edge_segments edge - 1)
      done
  done;
  let vertex_points = Array.make !vertex_count 0
  and vertex_left = Array.make !vertex_count 0
  and vertex_right = Array.make !vertex_count 0
  and vertex_weight = Array.make !vertex_count 0.
  and primitive_offsets = Array.make (primitive_count + 1) 0
  and primitive_kinds = Bytes.make primitive_count '\000'
  and primitive_source = Array.make primitive_count 0
  and chain_vertices = Array.make (Array.length chains.vertices) (-1) in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    let target = plan.unselected_primitive_of_source.(primitive) in
    if target >= 0 then begin
      primitive_offsets.(target) <- vertex_output.(topology.primitive_offsets.(primitive));
      Bytes.set primitive_kinds target (Bytes.get topology.primitive_kinds primitive);
      primitive_source.(target) <- primitive
    end
  done;
  primitive_offsets.(primitive_count) <- !vertex_count;
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    if plan.unselected_primitive_of_source.(primitive) >= 0 then
      for vertex = topology.primitive_offsets.(primitive)
          to topology.primitive_offsets.(primitive + 1) - 1 do
        Cancel.poll cancel vertex 16_383;
        let output = vertex_output.(vertex) in
        let source_point = topology.vertex_points.(vertex) in
        let output_point = plan.unselected_point_of_source.(source_point) in
        if output_point < 0 then fail "coarse corner point ancestry was removed";
        vertex_points.(output) <- output_point;
        vertex_left.(output) <- vertex;
        vertex_right.(output) <- vertex;
        let edge = source_index.edge_of_vertex.(vertex) in
        if edge >= 0 && edge_segments edge > 0 then begin
          let first = chains.edge_offsets.(edge)
          and last = chains.edge_offsets.(edge + 1) in
          let segments = last - first - 1 in
          let next = source_index.next_vertex.(vertex) in
          if next < 0 then fail "divided boundary cannot split an incomplete edge";
          let source_a = plan.selected_edge_start.(edge) in
          let forward = source_point = source_a in
          for local = 1 to segments - 1 do
            let target = output + local in
            let chain_local = if forward then local else segments - local in
            vertex_points.(target) <- chain_points.(first + chain_local);
            vertex_left.(target) <- vertex;
            vertex_right.(target) <- next;
            vertex_weight.(target) <- float_of_int local /. float_of_int segments
          done;
          if primitive = plan.unselected_primitive_of_edge.(edge) then begin
            let first_vertex, last_vertex = if forward
              then vertex_output.(vertex), vertex_output.(next)
              else vertex_output.(next), vertex_output.(vertex) in
            chain_vertices.(first) <- first_vertex;
            chain_vertices.(last - 1) <- last_vertex;
            for local = 1 to segments - 1 do
              chain_vertices.(first + local) <- if forward then output + local
                else output + segments - local
            done
          end
        end
      done
  done;
  Array.iter (fun vertex -> if vertex < 0 then
      fail "divided boundary chain has no coarse face-varying corner") chain_vertices;
  let source_positions = Packed.Float3.View.view (Geometry.positions source) in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(interpolate_array ?cancel ?grain point_left point_right point_weight
        source_positions.x)
      ~y:(interpolate_array ?cancel ?grain point_left point_right point_weight
        source_positions.y)
      ~z:(interpolate_array ?cancel ?grain point_left point_right point_weight
        source_positions.z) in
  let output_topology = Topology.View.create_validated_owned ~point_count
      ~vertex_points ~primitive_offsets ~primitive_kinds in
  let attributes = Geometry.attributes source |> List.map
      (interpolate_owned_attribute ?cancel ?grain ~point_left ~point_right
        ~point_weight ~vertex_left ~vertex_right ~vertex_weight
        ~primitive_source) in
  let groups = Geometry.groups source |> List.map (fun group ->
    let owner = Group.owner group in
    let left, right, weight = match owner with
      | Group.Point -> point_left, point_right, point_weight
      | Group.Vertex -> vertex_left, vertex_right, vertex_weight
      | Group.Primitive -> primitive_source, primitive_source,
          Array.make primitive_count 0. in
    let target = Group.init ?grain ~owner ~name:(Group.name group)
        (Array.length left) (fun output ->
          Group.mem left.(output) group && Group.mem right.(output) group) in
    let source_of_target = Array.mapi (fun output source ->
        if weight.(output) = 0. && source = right.(output) then source else -1) left in
    if Group.is_ordered group then
      Group.View.remap_order ~source:group ~source_of_target target
    else target) in
  let target_index = Topology_index.create ?cancel output_topology in
  let edge_groups = Geometry.edge_groups source |> List.map (fun group ->
    let builder = Edge_group.Builder.create ~topology:output_topology
        ~index:target_index ~name:(Edge_group.name group) in
    Edge_group.iter (fun edge ->
      if edge_segments edge > 0 then begin
        let first = chains.edge_offsets.(edge)
        and last = chains.edge_offsets.(edge + 1) in
        for at = first to last - 2 do
          match Topology_index.find_edge target_index
              ~a:chain_points.(at) ~b:chain_points.(at + 1) with
          | None -> fail "divided boundary dropped a child native edge"
          | Some child -> Edge_group.Builder.set builder child true
        done
      end else begin
        let a = plan.unselected_point_of_source.(source_index.edge_a.(edge))
        and b = plan.unselected_point_of_source.(source_index.edge_b.(edge)) in
        if a >= 0 && b >= 0 then
          match Topology_index.find_edge target_index ~a ~b with
          | None -> ()
          | Some child -> Edge_group.Builder.set builder child true
      end) group;
    Edge_group.Builder.freeze builder) in
  let geometry = Geometry.create ~positions ~topology:output_topology ~attributes
      ~groups ~edge_groups () |> get_ok in
  { geometry;
    points = chain_points; vertices = chain_vertices;
    source_primitive_to_output =
      Array.copy plan.unselected_primitive_of_source }

let remap_divided_attribute ?cancel ?grain vertex_map primitive_map attribute =
  let mapping = match Attribute.owner attribute with
    | Attribute.Point | Attribute.Detail -> None
    | Attribute.Vertex -> Some vertex_map
    | Attribute.Primitive -> Some primitive_map in
  match mapping with
  | None -> attribute
  | Some mapping ->
      let storage = match Attribute.View.storage attribute with
        | Attribute.Float values ->
            Attribute.Float (select_point_array ?cancel ?grain mapping values)
        | Attribute.Int values ->
            Attribute.Int (select_point_array ?cancel ?grain mapping values)
        | Attribute.Text values ->
            Attribute.Text (select_point_array ?cancel ?grain mapping values)
        | Attribute.Float2 values ->
            let values = Packed.Float2.View.view values in
            Attribute.Float2 (Packed.Float2.of_owned
              ~x:(select_point_array ?cancel ?grain mapping values.x)
              ~y:(select_point_array ?cancel ?grain mapping values.y) |> get_ok)
        | Attribute.Float3 values ->
            let values = Packed.Float3.View.view values in
            Attribute.Float3 (Packed.Float3.View.of_owned_exn
              ~x:(select_point_array ?cancel ?grain mapping values.x)
              ~y:(select_point_array ?cancel ?grain mapping values.y)
              ~z:(select_point_array ?cancel ?grain mapping values.z))
        | Attribute.Float4 values ->
            let values = Packed.Float4.View.view values in
            Attribute.Float4 (Packed.Float4.of_owned
              ~x:(select_point_array ?cancel ?grain mapping values.x)
              ~y:(select_point_array ?cancel ?grain mapping values.y)
              ~z:(select_point_array ?cancel ?grain mapping values.z)
              ~w:(select_point_array ?cancel ?grain mapping values.w) |> get_ok)
        | Attribute.Int_array values ->
            Attribute.Int_array (Ragged_ops.remap_int ?cancel ?grain mapping values)
        | Attribute.Float_array values ->
            Attribute.Float_array
              (Ragged_ops.remap_float ?cancel ?grain mapping values) in
      Attribute.create_owned ~name:(Attribute.name attribute)
        ~owner:(Attribute.owner attribute) storage |> get_ok

let remap_divided_group ?grain vertex_map primitive_map group =
  let mapping = match Group.owner group with
    | Group.Point -> None
    | Group.Vertex -> Some vertex_map
    | Group.Primitive -> Some primitive_map in
  match mapping with
  | None -> group
  | Some mapping ->
      let target = Group.init ?grain ~owner:(Group.owner group)
          ~name:(Group.name group) (Array.length mapping)
          (fun output -> Group.mem mapping.(output) group) in
      if Group.is_ordered group then
        Group.View.remap_order ~source:group ~source_of_target:mapping target
      else target

let triangulate_divided_boundary ?cancel ?grain ?pull_bias
    ~consistent_topology plan
    (chains : interface_chains) (divided : divided_boundary) ~refined =
  let geometry = divided.geometry in
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  let boundary = Bytes.make (Bytes.length topology.primitive_kinds) '\000' in
  for edge = 0 to Array.length chains.edge_offsets - 2 do
    if chains.edge_offsets.(edge + 1) > chains.edge_offsets.(edge) then begin
      let source_primitive = plan.unselected_primitive_of_edge.(edge) in
      let primitive = divided.source_primitive_to_output.(source_primitive) in
      if primitive < 0 then fail "triangulated boundary primitive was removed";
      Bytes.set boundary primitive '\001'
    end
  done;
  let output_vertices = ref 0 and output_primitives = ref 0 in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 1023;
    let corners = topology.primitive_offsets.(primitive + 1)
        - topology.primitive_offsets.(primitive) in
    if Bytes.get boundary primitive = '\000' then begin
      output_vertices := checked_add "boundary triangulation vertices"
          !output_vertices corners;
      incr output_primitives
    end else begin
      if Bytes.get topology.primitive_kinds primitive <> '\000' then
        fail "Triangulate crack closure requires polygon surrounding faces";
      if corners < 3 then fail "Triangulate crack closure found an invalid polygon";
      let triangles = corners - 2 in
      output_vertices := checked_add "boundary triangulation vertices"
          !output_vertices (checked_mul "boundary triangulation vertices" triangles 3);
      output_primitives := checked_add "boundary triangulation primitives"
          !output_primitives triangles
    end
  done;
  let vertex_points = Array.make !output_vertices 0
  and vertex_map = Array.make !output_vertices 0
  and primitive_map = Array.make !output_primitives 0
  and primitive_offsets = Array.make (!output_primitives + 1) 0
  and primitive_kinds = Bytes.make !output_primitives '\000'
  and first_vertex_output = Array.make (Array.length topology.vertex_points) (-1)
  and first_primitive_output = Array.make (Bytes.length topology.primitive_kinds) (-1) in
  let vertex_at = ref 0 and primitive_at = ref 0 in
  let begin_primitive kind primitive =
    if first_primitive_output.(primitive) < 0 then
      first_primitive_output.(primitive) <- !primitive_at;
    primitive_map.(!primitive_at) <- primitive;
    Bytes.set primitive_kinds !primitive_at kind in
  let emit_vertex source_vertex =
    vertex_points.(!vertex_at) <- topology.vertex_points.(source_vertex);
    vertex_map.(!vertex_at) <- source_vertex;
    if first_vertex_output.(source_vertex) < 0 then
      first_vertex_output.(source_vertex) <- !vertex_at;
    incr vertex_at in
  let end_primitive () =
    incr primitive_at;
    primitive_offsets.(!primitive_at) <- !vertex_at in
  let emit_triangle primitive a b c =
    begin_primitive '\000' primitive;
    emit_vertex a; emit_vertex b; emit_vertex c;
    end_primitive () in
  let scratch = Polygon_triangulation.create_scratch () in
  let source_positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let positions = match pull_bias with
    | Some bias when not consistent_topology ->
        let refined_positions = Packed.Float3.View.view
            (Geometry.positions refined) in
        let x = Array.copy source_positions.x and y = Array.copy source_positions.y
        and z = Array.copy source_positions.z
        and counts = Array.make topology.point_count 0
        and sx = Array.make topology.point_count 0.
        and sy = Array.make topology.point_count 0.
        and sz = Array.make topology.point_count 0. in
        for at = 0 to Array.length chains.points - 1 do
          let coarse = divided.points.(at) and point = chains.points.(at) in
          counts.(coarse) <- counts.(coarse) + 1;
          sx.(coarse) <- sx.(coarse) +. refined_positions.x.(point);
          sy.(coarse) <- sy.(coarse) +. refined_positions.y.(point);
          sz.(coarse) <- sz.(coarse) +. refined_positions.z.(point)
        done;
        for point = 0 to topology.point_count - 1 do
          if counts.(point) > 0 then begin
            let inverse = 1. /. float_of_int counts.(point) in
            x.(point) <- ((1. -. bias) *. x.(point))
                +. (bias *. sx.(point) *. inverse);
            y.(point) <- ((1. -. bias) *. y.(point))
                +. (bias *. sy.(point) *. inverse);
            z.(point) <- ((1. -. bias) *. z.(point))
                +. (bias *. sz.(point) *. inverse)
          end
        done;
        Packed.Float3.View.{ x; y; z }
    | None | Some _ -> source_positions in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    Cancel.poll cancel primitive 255;
    if Bytes.get boundary primitive = '\000' then begin
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      begin_primitive (Bytes.get topology.primitive_kinds primitive) primitive;
      for vertex = first to last - 1 do emit_vertex vertex done;
      end_primitive ()
    end else if consistent_topology then begin
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      for vertex = first + 1 to last - 2 do
        emit_triangle primitive first vertex (vertex + 1)
      done
    end else
      match Polygon_triangulation.primitive ?cancel ~positions ~topology
          ~scratch primitive
          ~emit:(fun _ a b c -> emit_triangle primitive a b c) with
      | Ok () -> ()
      | Error message -> fail ("Triangulate crack closure: " ^ message)
  done;
  if !vertex_at <> !output_vertices || !primitive_at <> !output_primitives then
    fail "boundary triangulation output cardinality mismatch";
  let output_topology = Topology.View.create_validated_owned
      ~point_count:topology.point_count ~vertex_points ~primitive_offsets
      ~primitive_kinds in
  let attributes = Geometry.attributes geometry |> List.map
      (remap_divided_attribute ?cancel ?grain vertex_map primitive_map) in
  let groups = Geometry.groups geometry |> List.map
      (remap_divided_group ?grain vertex_map primitive_map) in
  let source_index = Topology_index.create ?cancel topology_value
  and target_index = Topology_index.create ?cancel output_topology in
  let point_map = Array.init topology.point_count Fun.id in
  let edge_groups = Geometry.edge_groups geometry |> List.map (fun group ->
    Edge_group.remap ?cancel ~source_index ~target_topology:output_topology
      ~target_index ~point_map group |> get_ok) in
  let geometry = Geometry.create ~positions:(Geometry.positions geometry)
      ~topology:output_topology ~attributes ~groups ~edge_groups () |> get_ok in
  let vertices = Array.map (fun vertex ->
      let output = first_vertex_output.(vertex) in
      if output < 0 then fail "triangulated boundary lost a chain corner";
      output) divided.vertices
  and source_primitive_to_output = Array.map (fun primitive ->
      if primitive < 0 then -1
      else first_primitive_output.(primitive)) divided.source_primitive_to_output in
  { divided with geometry; vertices; source_primitive_to_output }

