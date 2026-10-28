open Rdk_core
open Rdk_ops
open Rays

type 'a source = Point of 'a | Vertex of 'a

let mesh_vec3 (view : Packed.Float3.View.view) : Mesh.Private.vec3_view =
  { x = view.x; y = view.y; z = view.z }

let standard geometry ~name ~point_kind ~vertex_kind =
  let read owner expected =
    match Geometry.find_attribute ~owner name geometry with
    | None -> Ok None
    | Some attribute ->
        (match expected (Attribute.View.storage attribute) with
         | Some value -> Ok (Some value)
         | None -> Error (Printf.sprintf
             "Rdk.Rays_mesh.to_mesh: %s %s attribute has storage %s"
             (match owner with Attribute.Point -> "point" | Vertex -> "vertex"
              | Primitive -> "primitive" | Detail -> "detail")
             name (Attribute.kind_name attribute))) in
  match read Attribute.Vertex vertex_kind with
  | Error _ as error -> error
  | Ok (Some value) -> Ok (Some (Vertex value))
  | Ok None -> Result.map (Option.map (fun value -> Point value))
      (read Attribute.Point point_kind)

let float2 = function Attribute.Float2 value -> Some value | _ -> None
let float3 = function Attribute.Float3 value -> Some value | _ -> None
let float4 = function
  | Attribute.Float4 value -> Some value
  | Attribute.Float3 value ->
      let v = Packed.Float3.View.view value in
      Some (Packed.Float4.of_owned ~x:(Array.copy v.x) ~y:(Array.copy v.y)
        ~z:(Array.copy v.z) ~w:(Array.make (Packed.Float3.length value) 1.) |> Result.get_ok)
  | _ -> None

let source_has_vertex = function Some (Vertex _) -> true | _ -> false

let expanded_vec3 topology count source =
  let values = match source with Point values | Vertex values -> values in
  let input = Packed.Float3.View.view values in
  let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
  for vertex = 0 to count - 1 do
    let source_index = match source with
      | Point _ -> Topology.point_of_vertex topology vertex
      | Vertex _ -> vertex in
    x.(vertex) <- input.x.(source_index);
    y.(vertex) <- input.y.(source_index);
    z.(vertex) <- input.z.(source_index)
  done;
  Packed.Float3.View.of_owned_exn ~x ~y ~z

let color_array ~expanded topology count source =
  let values = match source with Point values | Vertex values -> values in
  let input = Packed.Float4.View.view values in
  Array.init count (fun vertex ->
    let index = match source with
      | Point _ -> if expanded then Topology.point_of_vertex topology vertex else vertex
      | Vertex _ -> vertex in
    Rays_math.Color.of_floats input.x.(index) input.y.(index) input.z.(index) input.w.(index))

let tex_coord_array ~expanded topology count source =
  let values = match source with Point values | Vertex values -> values in
  let input = Packed.Float2.View.view values in
  Array.init count (fun vertex ->
    let index = match source with
      | Point _ -> if expanded then Topology.point_of_vertex topology vertex else vertex
      | Vertex _ -> vertex in
    Rays_math.Vec2.create input.x.(index) input.y.(index))

let all_curves topology =
  let curves = ref true in
  for primitive = 0 to Topology.primitive_count topology - 1 do
    if Topology.primitive_kind topology primitive = Topology.Polygon then
      curves := false
  done;
  !curves

let curve_indices topology ~expanded =
  let edge_count = ref 0 in
  for primitive = 0 to Topology.primitive_count topology - 1 do
    let size = Topology.primitive_size topology primitive in
    edge_count := !edge_count + size - 1
      + (if Topology.primitive_kind topology primitive = Topology.Closed_polyline
         then 1 else 0)
  done;
  let indices = Array.make (!edge_count * 2) 0 and at = ref 0 in
  let index vertex = if expanded then vertex else Topology.point_of_vertex topology vertex in
  for primitive = 0 to Topology.primitive_count topology - 1 do
    let first, last = Topology.primitive_vertex_range topology primitive in
    for vertex = first to last - 2 do
      indices.(!at) <- index vertex;
      indices.(!at + 1) <- index (vertex + 1);
      at := !at + 2
    done;
    if Topology.primitive_kind topology primitive = Topology.Closed_polyline then begin
      indices.(!at) <- index (last - 1);
      indices.(!at + 1) <- index first;
      at := !at + 2
    end
  done;
  indices

let curve_mesh geometry =
  let topology = Geometry.topology geometry in
  match standard geometry ~name:"N" ~point_kind:float3 ~vertex_kind:float3,
        standard geometry ~name:"Cd" ~point_kind:float4 ~vertex_kind:float4,
        standard geometry ~name:"uv" ~point_kind:float2 ~vertex_kind:float2 with
  | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message
  | Ok normals, Ok colors, Ok tex_coords ->
      let expand = source_has_vertex normals || source_has_vertex colors
        || source_has_vertex tex_coords in
      let count = if expand then Geometry.vertex_count geometry
        else Geometry.point_count geometry in
      let indices = curve_indices topology ~expanded:expand in
      if expand then
        let positions = expanded_vec3 topology count
            (Point (Geometry.positions geometry))
            |> Packed.Float3.View.view |> mesh_vec3 in
        let normals = Option.map (fun source ->
          expanded_vec3 topology count source
          |> Packed.Float3.View.view |> mesh_vec3)
            normals in
        let colors = Option.map (color_array ~expanded:true topology count) colors
        and tex_coords = Option.map
            (tex_coord_array ~expanded:true topology count) tex_coords in
        Mesh.Private.create_packed_owned ~mode:Mesh.Lines ~indices
          ?normals ?colors ?tex_coords positions
      else
        let positions = Geometry.positions geometry
            |> Packed.Float3.View.view |> mesh_vec3 in
        let normals = Option.map (function
          | Point values -> Packed.Float3.View.view values |> mesh_vec3
          | Vertex _ -> assert false) normals in
        let colors = Option.map (color_array ~expanded:false topology count) colors
        and tex_coords = Option.map
            (tex_coord_array ~expanded:false topology count) tex_coords in
        Mesh.Private.create_packed_shared ~mode:Mesh.Lines ~indices
          ?normals ?colors ?tex_coords positions

let rec to_mesh_raw_impl geometry =
  let topology = Geometry.topology geometry in
  if Topology.primitive_count topology <> 0 && all_curves topology then
    curve_mesh geometry
  else if Topology.primitive_count topology <> 0 && not (Topology.all_triangles topology)
  then Result.bind
      (Result.map_error Error.to_string (Triangulate.run geometry))
      to_mesh_raw_impl
  else
    match standard geometry ~name:"N" ~point_kind:float3 ~vertex_kind:float3,
          standard geometry ~name:"Cd" ~point_kind:float4 ~vertex_kind:float4,
          standard geometry ~name:"uv" ~point_kind:float2 ~vertex_kind:float2 with
    | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message
    | Ok normals, Ok colors, Ok tex_coords ->
        let has_primitives = Topology.primitive_count topology <> 0 in
        let expand = has_primitives
          && (source_has_vertex normals || source_has_vertex colors
              || source_has_vertex tex_coords) in
        if expand then
          let count = Topology.vertex_count topology in
          let position_source = Point (Geometry.positions geometry) in
          let positions = expanded_vec3 topology count position_source
              |> Packed.Float3.View.view |> mesh_vec3 in
          let normals = Option.map (fun source ->
            expanded_vec3 topology count source
            |> Packed.Float3.View.view |> mesh_vec3) normals in
          let colors = Option.map (color_array ~expanded:true topology count) colors
          and tex_coords = Option.map
              (tex_coord_array ~expanded:true topology count) tex_coords in
          let indices = Array.init count Fun.id in
          Mesh.Private.create_packed_owned ~mode:Mesh.Triangles ~indices
            ?normals ?colors ?tex_coords positions
        else
          let positions = Geometry.positions geometry
              |> Packed.Float3.View.view |> mesh_vec3 in
          let normals = Option.map (function
            | Point values -> Packed.Float3.View.view values |> mesh_vec3
            | Vertex _ -> assert false) normals in
          let count = Geometry.point_count geometry in
          let colors = Option.map (color_array ~expanded:false topology count) colors
          and tex_coords = Option.map
              (tex_coord_array ~expanded:false topology count) tex_coords in
          if not has_primitives then
            Mesh.Private.create_packed_shared ~mode:Mesh.Points ?normals ?colors
              ?tex_coords positions
          else
            let view = Topology.View.view topology in
            Mesh.Private.create_packed_shared ~mode:Mesh.Triangles
              ~indices:view.vertex_points ?normals ?colors ?tex_coords positions

let protected operation work =
  Result.map_error (Error.make ~operation ~code:"mesh_conversion") (work ())

let to_trace_mesh geometry =
  protected "to_trace_mesh" (fun () ->
    let ( let* ) = Result.bind in
    let* triangles = Triangulate.run geometry |> Result.map_error Error.to_string in
    let* triangles = Normal_ops.run ~owner:Attribute.Vertex
        ~cusp_angle:(Float.pi /. 4.5) triangles |> Result.map_error Error.to_string in
    to_mesh_raw_impl triangles)

let rdk_float3 (view : Mesh.Private.vec3_view) =
  Packed.Float3.View.of_shared_exn ~x:view.x ~y:view.y ~z:view.z

let color_attribute colors =
  let count = Array.length colors in
  let r = Array.make count 0. and g = Array.make count 0.
  and b = Array.make count 0. and a = Array.make count 0. in
  Array.iteri (fun index color ->
    let cr, cg, cb, ca = Rays_math.Color.to_floats color in
    r.(index) <- cr; g.(index) <- cg; b.(index) <- cb; a.(index) <- ca) colors;
  let values = Packed.Float4.of_owned ~x:r ~y:g ~z:b ~w:a |> Result.get_ok in
  Attribute.create_key_owned (Attribute.color ~owner:Attribute.Point) values
  |> Result.get_ok

let tex_coord_attribute tex_coords =
  let count = Array.length tex_coords in
  let u = Array.make count 0. and v = Array.make count 0. in
  Array.iteri (fun index point ->
    u.(index) <- point.Rays_math.Vec2.x; v.(index) <- point.y)
    tex_coords;
  let values = Packed.Float2.of_owned ~x:u ~y:v |> Result.get_ok in
  Attribute.create_key_owned (Attribute.tex_coord ~owner:Attribute.Point) values
  |> Result.get_ok

let of_mesh_raw_impl mesh =
  let view = Mesh.Private.packed_view mesh in
  match view.mode with
  | Mesh.Points | Mesh.Triangles ->
      let positions = rdk_float3 view.vertices in
      let topology_result =
        match view.mode with
        | Mesh.Points -> Ok (Topology.empty ~point_count:(Packed.Float3.length positions))
        | Mesh.Triangles ->
            if Array.length view.indices mod 3 <> 0 then
              Error "Rdk.Rays_mesh.of_mesh: malformed triangle index count"
            else
              let primitive_count = Array.length view.indices / 3 in
              let offsets = Array.init (primitive_count + 1) (fun index ->
                index * 3) in
              Topology.View.polygons_shared
                ~point_count:(Packed.Float3.length positions)
                ~vertex_points:view.indices ~primitive_offsets:offsets
        | _ -> assert false in
      Result.bind topology_result (fun topology ->
        let attributes = ref [] in
        Option.iter (fun normals ->
          let values = rdk_float3 normals in
          let attribute = Attribute.create_key_owned
              (Attribute.normal ~owner:Attribute.Point) values |> Result.get_ok in
          attributes := attribute :: !attributes) view.normals;
        Option.iter (fun colors ->
          attributes := color_attribute colors :: !attributes) view.colors;
        Option.iter (fun tex_coords ->
          attributes := tex_coord_attribute tex_coords :: !attributes) view.tex_coords;
        Geometry.create ~positions ~topology ~attributes:(List.rev !attributes) ())
  | Mesh.Lines | Mesh.Line_strip | Mesh.Line_loop
  | Mesh.Triangle_strip | Mesh.Triangle_fan ->
      Error "Rdk.Rays_mesh.of_mesh: expand non-point/non-triangle modes first"

let to_mesh geometry =
  protected "to_mesh" (fun () ->
    (* A primitive color belongs to all of its corners, never to a shared
       point: expanding it preserves material boundaries between faces. *)
    let geometry =
      if Geometry.find_attribute ~owner:Attribute.Vertex "Cd" geometry <> None
         || Geometry.find_attribute ~owner:Attribute.Point "Cd" geometry <> None
         || Geometry.find_attribute ~owner:Attribute.Primitive "Cd" geometry = None
      then Ok geometry
      else
        let topology = Topology.View.view (Geometry.topology geometry) in
        let corner_primitive = Array.make (Array.length topology.vertex_points) 0 in
        for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
          Array.fill corner_primitive topology.primitive_offsets.(primitive)
            (topology.primitive_offsets.(primitive + 1) - topology.primitive_offsets.(primitive)) primitive
        done;
        let cd = Option.get (Geometry.find_attribute ~owner:Attribute.Primitive "Cd" geometry) in
        let spread = Topology_remap.attribute ~grain:16_384 corner_primitive cd in
        Result.bind (Attribute.create_owned ~name:"Cd" ~owner:Attribute.Vertex (Attribute.storage spread))
          (fun corners -> Geometry.with_attribute corners geometry) in
    Result.bind geometry to_mesh_raw_impl)

let of_mesh mesh =
  protected "of_mesh" (fun () -> of_mesh_raw_impl mesh)
