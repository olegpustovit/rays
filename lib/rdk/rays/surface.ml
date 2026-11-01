open Rays
open Rdk_core
open Rdk_ops
open Rdk_ops

(* [Device]: resident positions, normals/colours and optional topology, bound on the initial domain. *)
type preview = Pieces of Packed_pieces.t | Mesh of Mesh.t | Device of Geometry.t

exception Conversion_error of string

type t = {
  preview : preview;
  transforms : Rays_math.Mat4.t array option;
  batches : (Material.t * int array) list;
  host_reason : string option;
}

(* Point Cd stays with P/N on device. Resident polygon topology is offered to the
   checked index kernel; unsupported layouts take the host bridge. *)
(* whether every primitive is a triangle, the last topology's answer kept: a deforming mesh's frames
   share their topology *)
let last_topology = Atomic.make None

let triangles topology =
  match Atomic.get last_topology with
  | Some (known, answer) when known == topology -> answer
  | _ ->
      let answer = Topology.all_triangles topology in
      Atomic.set last_topology (Some (topology, answer));
      answer

let device_color geometry =
  match Geometry.find_attribute ~owner:Attribute.Point "Cd" geometry with
  | None -> true
  | Some a -> Attribute.resident a && List.mem (Attribute.kind_name a) [ "float3"; "float4" ]

let device_refusal geometry =
  if not (Packed.Float3.resident (Geometry.positions geometry)) then
    Some "Positions are on the CPU."
  else if Geometry.point_count geometry = 0 then Some "Geometry is empty."
  else if
    match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
    | None -> false
    | Some n -> not (Attribute.resident n && Attribute.kind_name n = "float3")
  then Some "Point normals are not a resident float3 stream."
  else if not (device_color geometry) then
    Some "Point colours are not a resident float3/float4 stream."
  else if
    Geometry.primitive_count geometry <> 0
    &&
    if Topology.resident (Geometry.topology geometry) then
      Topology.holder (Geometry.topology geometry) = None
      || Geometry.find_attribute ~owner:Attribute.Point "N" geometry = None
    else not (triangles (Geometry.topology geometry))
  then Some "Topology requires the host bridge (resident polygons also require point normals)."
  else
    List.find_map
      (fun (owner, name) ->
        if Geometry.find_attribute ~owner name geometry = None then None
        else Some ("Attribute " ^ name ^ " requires the host bridge."))
      [
        (Attribute.Point, "uv");
        (Attribute.Vertex, "N");
        (Attribute.Vertex, "Cd");
        (Attribute.Vertex, "uv");
        (Attribute.Primitive, "Cd");
        (Attribute.Primitive, "piece");
        (Attribute.Primitive, "material_color");
        (Attribute.Primitive, "material_roughness");
        (Attribute.Primitive, "material_emission");
        (Attribute.Primitive, "material_round");
      ]

let rec of_geometry ?instances geometry =
  match device_refusal geometry with
  | None ->
      Ok { preview = Device geometry; transforms = instances; batches = []; host_reason = None }
  | Some reason -> of_host ?instances ~reason geometry

and of_host ?instances ~reason geometry =
  let ( let* ) = Result.bind in
  let needs_triangles =
    List.exists
      (fun name -> Geometry.find_attribute ~owner:Attribute.Primitive name geometry <> None)
      [ "piece"; "material_color"; "material_roughness"; "material_emission"; "material_round" ]
  in
  let* geometry =
    if
      (not needs_triangles)
      || Topology.all_triangles (Geometry.topology geometry)
      || Geometry.primitive_count geometry = 0
    then Ok geometry
    else
      Triangulate.run { cancel = None; grain = None; primitives = None } geometry
      |> Result.map_error Error.to_string
  in
  let* preview =
    match Geometry.find_attribute ~owner:Attribute.Primitive "piece" geometry with
    | Some _ when Geometry.primitive_count geometry > 0 ->
        Packed_pieces.of_geometry ~piece_attribute:"piece" geometry
        |> Result.map (fun p -> Pieces p)
    | Some _ | None ->
        Rays_mesh.to_mesh geometry
        |> Result.map (fun m -> Mesh m)
        |> Result.map_error Error.to_string
  in
  let count = Geometry.primitive_count geometry in
  let* roughness =
    match Geometry.find_attribute ~owner:Attribute.Primitive "material_roughness" geometry with
    | None -> Ok None
    | Some a -> (
        match Attribute.View.storage a with
        | Attribute.Float a -> Ok (Some a)
        | _ -> Error "material_roughness must be primitive float")
  in
  let* round =
    match Geometry.find_attribute ~owner:Attribute.Primitive "material_round" geometry with
    | None -> Ok None
    | Some a -> (
        match Attribute.View.storage a with
        | Attribute.Float a -> Ok (Some a)
        | _ -> Error "material_round must be primitive float")
  in
  let* emission =
    match Geometry.find_attribute ~owner:Attribute.Primitive "material_emission" geometry with
    | None -> Ok None
    | Some a -> (
        match Attribute.View.storage a with
        | Attribute.Float4 a -> Ok (Some a)
        | _ -> Error "material_emission must be primitive float4")
  in
  let* color =
    match Geometry.find_attribute ~owner:Attribute.Primitive "material_color" geometry with
    | None -> Ok None
    | Some a -> (
        match Attribute.View.storage a with
        | Attribute.Float4 a -> Ok (Some a)
        | _ -> Error "material_color must be primitive float4")
  in
  let valid x = Float.is_finite x && x >= 0. && x <= 1. in
  let valid_tuple =
    Option.fold ~none:true ~some:(fun a ->
        let v = Packed.Float4.View.view a in
        Array.for_all valid v.x && Array.for_all valid v.y && Array.for_all valid v.z)
  in
  let has_material = roughness <> None || emission <> None || color <> None || round <> None in
  let* () =
    if Option.fold ~none:true ~some:(Array.for_all (fun x -> Float.is_finite x && x >= 0.)) round
    then Ok ()
    else Error "material_round must be finite and non-negative"
  in
  let* () =
    if
      Option.fold ~none:true ~some:(Array.for_all valid) roughness
      && valid_tuple color && valid_tuple emission
    then Ok ()
    else Error "material channels must be finite and in [0,1]"
  in
  let* () =
    if (not has_material) || Topology.all_triangles (Geometry.topology geometry) || count = 0 then
      Ok ()
    else Error "material surfaces require polygon geometry"
  in
  let batches =
    if not has_material then []
    else
      let table = Hashtbl.create 8 and order = ref [] in
      for primitive = 0 to count - 1 do
        let rough = match roughness with None -> sqrt (2. /. 34.) | Some a -> a.(primitive) in
        let er, eg, eb =
          match emission with
          | None -> (0., 0., 0.)
          | Some a ->
              let r, g, b, _ = Packed.Float4.get a primitive in
              (r, g, b)
        in
        let r, g, b =
          match color with
          | None -> (1., 1., 1.)
          | Some a ->
              let r, g, b, _ = Packed.Float4.get a primitive in
              (r, g, b)
        in
        let round = match round with None -> 0. | Some a -> a.(primitive) in
        let key = (rough, r, g, b, er, eg, eb, round) in
        let indices =
          match Hashtbl.find_opt table key with
          | Some indices -> indices
          | None ->
              let indices = ref [] in
              Hashtbl.add table key indices;
              order := key :: !order;
              indices
        in
        indices := primitive :: !indices
      done;
      List.rev_map
        (fun ((rough, r, g, b, er, eg, eb, round) as key) ->
          let primitives = List.rev !(Hashtbl.find table key) |> Array.of_list in
          let indices =
            Array.init (Array.length primitives * 3) (fun i -> (primitives.(i / 3) * 3) + (i mod 3))
          in
          let shininess = max 0. ((2. /. max 0.0001 (rough *. rough)) -. 2.) in
          ( Material.create
              ~diffuse:(Rays_math.Color.of_floats r g b 1.)
              ~shininess ~round
              ~emissive:(Rays_math.Color.of_floats er eg eb 1.)
              (),
            indices ))
        !order
  in
  Ok { preview; transforms = instances; batches; host_reason = Some reason }

let host_reason surface = surface.host_reason

(* the host mesh of a device preview, reading P back *)
let host geometry = Rays_mesh.to_mesh geometry |> Result.map_error Error.to_string
let converted = function Ok value -> value | Error message -> raise (Conversion_error message)

let scene3_result ?explosion ?device surface =
  let ( let* ) = Result.bind in
  let* mesh =
    match surface.preview with
    | Mesh mesh -> Ok mesh
    | Pieces pieces -> Ok (Packed_pieces.mesh_for_explosion explosion pieces)
    | Device geometry -> (
        let count = Geometry.point_count geometry in
        let mode = if Geometry.primitive_count geometry = 0 then Mesh.Points else Mesh.Triangles in
        match Option.bind device (fun device -> device geometry) with
        | Some (key, normals, device_index_count, device_color_width, keep) ->
            let indices =
              if mode = Mesh.Triangles && device_index_count = None then
                Some (Topology.View.view (Geometry.topology geometry)).vertex_points
              else None
            in
            Mesh.View.create_device ~mode ?indices ?device_index_count ~device_color_width
              ~host:(fun () -> converted (host geometry))
              ~bounds:(fun () -> Geometry_view.bounds geometry)
              ~derived:
                (mode = Mesh.Triangles && Geometry.find_attribute ~owner:Point "N" geometry = None)
              ~key ~normals ~keep count
        | None ->
            (* no device to bind (another renderer, a closed runner): the host copy *)
            host geometry)
  in
  let draw material mesh =
    match surface.transforms with
    | None -> Scene.D3.mesh ~material ~cull:Scene.D3.Cull_none mesh
    | Some transforms -> Scene.D3.instances_array ~material ~cull:Scene.D3.Cull_none mesh transforms
  in
  let drawings =
    match surface.batches with
    | [] -> [ draw Material.default mesh ]
    | batches ->
        let view = Mesh.View.packed_view mesh in
        List.map
          (fun (material, offsets) ->
            let indices = Array.map (Array.get view.indices) offsets in
            let mesh =
              Mesh.View.create_packed_shared ~mode:view.mode ~indices ?normals:view.normals
                ?colors:view.colors ?tex_coords:view.tex_coords view.vertices
              |> Result.get_ok
            in
            draw material mesh)
          batches
  in
  Ok (Scene.D3.create drawings)

let scene3 ?explosion ?device surface = converted (scene3_result ?explosion ?device surface)
