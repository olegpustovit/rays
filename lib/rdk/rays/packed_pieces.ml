open Rays
open Rdk_core
open Rdk_attrib
open Rdk_mesh

type t = {
  base_mesh : Mesh.t;
  base : Mesh.Private.packed_view;
  piece_of_vertex : int array;
  center_x : float array;
  center_y : float array;
  center_z : float array;
  offset_x : float array;
  offset_y : float array;
  offset_z : float array;
}

let piece_count value = Array.length value.center_x
let vertex_count value = Array.length value.piece_of_vertex

let dense_int values =
  let table = Hashtbl.create (max 16 (Array.length values / 4)) in
  let count = ref 0 in
  let dense = Array.map (fun value -> match Hashtbl.find_opt table value with
    | Some index -> index
    | None ->
        let index = !count in
        incr count;
        Hashtbl.add table value index;
        index) values in
  dense, !count

let dense_text values =
  let table = Hashtbl.create (max 16 (Array.length values / 4)) in
  let count = ref 0 in
  let dense = Array.map (fun value -> match Hashtbl.find_opt table value with
    | Some index -> index
    | None ->
        let index = !count in
        incr count;
        Hashtbl.add table value index;
        index) values in
  dense, !count

let primitive_pieces ~piece_attribute geometry =
  match Geometry.find_attribute ~owner:Attribute.Primitive
      piece_attribute geometry with
  | None -> Error (Printf.sprintf
      "Packed_pieces: missing primitive piece attribute %S" piece_attribute)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> Ok (dense_int values)
       | Attribute.Text values -> Ok (dense_text values)
       | _ -> Error (Printf.sprintf
           "Packed_pieces: primitive piece attribute %S must be int or text"
           piece_attribute))

let expand_triangles_preserving_attributes mesh =
  let source = Mesh.Private.packed_view mesh in
  if source.mode <> Mesh.Triangles then
    invalid_arg "Packed_pieces: expected a triangulated render mesh";
  let count = Array.length source.indices in
  let select plane = Array.init count (fun vertex ->
      plane.(source.indices.(vertex))) in
  let select3 (values : Mesh.Private.vec3_view) : Mesh.Private.vec3_view = {
    x = select values.x; y = select values.y; z = select values.z;
  } in
  let vertices = select3 source.vertices
  and normals = Option.map select3 source.normals
  and colors = Option.map select source.colors
  and tex_coords = Option.map select source.tex_coords in
  Mesh.Private.create_packed_owned ~mode:Mesh.Triangles
    ~indices:(Array.init count Fun.id) ?normals ?colors ?tex_coords vertices
  |> Result.get_ok

let of_geometry ~piece_attribute geometry =
  if String.trim piece_attribute = "" then
    invalid_arg "Packed_pieces.of_geometry: empty piece attribute";
  let triangulated =
    if Topology.all_triangles (Geometry.topology geometry) then
      Ok geometry
    else Result.map_error Error.to_string
        (Triangulate.run geometry)
  in
  Result.bind triangulated (fun geometry ->
    Result.bind (primitive_pieces ~piece_attribute geometry)
      (fun (primitive_piece, piece_count) ->
        if piece_count = 0 then Error "Packed_pieces: geometry has no pieces"
        else
          Result.bind
            (Result.map_error Error.to_string
               (Rays_mesh.to_mesh geometry))
            (fun mesh ->
              let expanded = expand_triangles_preserving_attributes mesh in
              let base = Mesh.Private.packed_view expanded in
              let expected = Geometry.primitive_count geometry * 3 in
              if base.mode <> Mesh.Triangles
                  || Array.length base.vertices.x <> expected then
                Error "Packed_pieces: triangle render ancestry is inconsistent"
              else
                let piece_of_vertex = Array.init expected (fun vertex ->
                    primitive_piece.(vertex / 3)) in
                let min_x = Array.make piece_count infinity
                and min_y = Array.make piece_count infinity
                and min_z = Array.make piece_count infinity
                and max_x = Array.make piece_count neg_infinity
                and max_y = Array.make piece_count neg_infinity
                and max_z = Array.make piece_count neg_infinity in
                for vertex = 0 to expected - 1 do
                  let piece = piece_of_vertex.(vertex) in
                  let x = base.vertices.x.(vertex)
                  and y = base.vertices.y.(vertex)
                  and z = base.vertices.z.(vertex) in
                  min_x.(piece) <- Float.min min_x.(piece) x;
                  min_y.(piece) <- Float.min min_y.(piece) y;
                  min_z.(piece) <- Float.min min_z.(piece) z;
                  max_x.(piece) <- Float.max max_x.(piece) x;
                  max_y.(piece) <- Float.max max_y.(piece) y;
                  max_z.(piece) <- Float.max max_z.(piece) z
                done;
                let center_x = Array.init piece_count (fun piece ->
                    0.5 *. (min_x.(piece) +. max_x.(piece)))
                and center_y = Array.init piece_count (fun piece ->
                    0.5 *. (min_y.(piece) +. max_y.(piece)))
                and center_z = Array.init piece_count (fun piece ->
                    0.5 *. (min_z.(piece) +. max_z.(piece))) in
                let origin =
                  let all_min values = Array.fold_left Float.min infinity values
                  and all_max values = Array.fold_left Float.max neg_infinity values in
                  Rays_math.Vec3.create
                    (0.5 *. (all_min min_x +. all_max max_x))
                    (0.5 *. (all_min min_y +. all_max max_y))
                    (0.5 *. (all_min min_z +. all_max max_z))
                in
                Ok {
                  base_mesh = expanded;
                  base;
                  piece_of_vertex;
                  center_x;
                  center_y;
                  center_z;
                  offset_x = Array.map (fun value -> value -. origin.x) center_x;
                  offset_y = Array.map (fun value -> value -. origin.y) center_y;
                  offset_z = Array.map (fun value -> value -. origin.z) center_z;
                })))

let mesh ?(noise_amount = 0.) ?(noise_frequency = 1.) ?(noise_seed = 0)
    ~amount value =
  if not (Float.is_finite amount && Float.is_finite noise_amount
      && Float.is_finite noise_frequency) then
    invalid_arg "Packed_pieces.mesh: controls must be finite";
  if amount = 0. then value.base_mesh
  else
    let noise = Rays_math.Noise.create noise_seed in
    let scale = Array.init (piece_count value) (fun piece ->
      if noise_amount = 0. then 1.
      else
        let sample = Rays_math.Noise.fbm3 ~octaves:3 noise
            ~x:(value.center_x.(piece) *. noise_frequency)
            ~y:(value.center_y.(piece) *. noise_frequency)
            ~z:(value.center_z.(piece) *. noise_frequency) in
        max 0. (1. +. (noise_amount *. ((2. *. sample) -. 1.)))) in
    let displaced offset vertex =
      let piece = value.piece_of_vertex.(vertex) in
      amount *. scale.(piece) *. offset.(piece) in
    let count = vertex_count value in
    let positions = {
      Mesh.Private.x = Array.init count (fun vertex ->
        value.base.vertices.x.(vertex) +. displaced value.offset_x vertex);
      y = Array.init count (fun vertex ->
        value.base.vertices.y.(vertex) +. displaced value.offset_y vertex);
      z = Array.init count (fun vertex ->
        value.base.vertices.z.(vertex) +. displaced value.offset_z vertex);
    } in
    Mesh.Private.create_packed_shared ~mode:value.base.mode
      ~indices:value.base.indices ?normals:value.base.normals
      ?colors:value.base.colors ?tex_coords:value.base.tex_coords positions
    |> Result.get_ok

type explosion = {
  amount : float;
  scale : Rays_math.Vec3.t;
  piece_attribute : string;
  noise_amount : float;
  noise_frequency : float;
  noise_seed : int;
}

let explosion_default = {
  amount = 0.32; scale = Rays_math.Vec3.create 1. 1. 1.; piece_attribute = "piece";
  noise_amount = 0.; noise_frequency = 0.8; noise_seed = 0;
}

let explosion ~operation ~fields =
  if not (String.equal operation "exploded_view") then None
  else
    let float name fallback = List.find_map (fun field ->
      if field.Param.name = name then match field.current with
        | Param.Float_value value -> Some value
        | _ -> None
      else None) fields |> Option.value ~default:fallback
    and int name fallback = List.find_map (fun field ->
      if field.Param.name = name then match field.current with
        | Param.Int_value value -> Some value
        | _ -> None
      else None) fields |> Option.value ~default:fallback
    and text name fallback = List.find_map (fun field ->
      if field.Param.name = name then match field.current with
        | Param.Text_value value -> Some value
        | _ -> None
      else None) fields |> Option.value ~default:fallback in
    if List.exists (fun field -> field.Param.name = "amount") fields then
      Some {
        amount = float "amount" explosion_default.amount;
        scale = Rays_math.Vec3.create
            (float "scale_x" explosion_default.scale.x)
            (float "scale_y" explosion_default.scale.y)
            (float "scale_z" explosion_default.scale.z);
        piece_attribute = text "piece_attribute"
            explosion_default.piece_attribute;
        noise_amount = float "noise_amount" explosion_default.noise_amount;
        noise_frequency = float "noise_frequency"
            explosion_default.noise_frequency;
        noise_seed = int "noise_seed" explosion_default.noise_seed;
      }
    else None

let mesh_for_explosion parameters value =
  match parameters with
  | None -> value.base_mesh
  | Some parameters ->
      let scaled = { value with
        offset_x = Array.map (( *. ) parameters.scale.x) value.offset_x;
        offset_y = Array.map (( *. ) parameters.scale.y) value.offset_y;
        offset_z = Array.map (( *. ) parameters.scale.z) value.offset_z;
      } in
      mesh ~amount:parameters.amount ~noise_amount:parameters.noise_amount
        ~noise_frequency:parameters.noise_frequency
        ~noise_seed:parameters.noise_seed scaled
