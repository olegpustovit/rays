open Rays
open Rdk_core
open Rdk_attrib
open Rdk_mesh

type preview = Pieces of Packed_pieces.t | Mesh of Mesh.t
type t = { preview : preview; transforms : Rays_math.Mat4.t array option;
           batches : (Material.t * int array) list }

let of_geometry ?instances geometry =
  let ( let* ) = Result.bind in
  let needs_triangles = List.exists (fun name ->
    Geometry.find_attribute ~owner:Attribute.Primitive name geometry <> None)
    ["piece"; "material_color"; "material_roughness"; "material_emission"; "material_round"] in
  let* geometry = if not needs_triangles || Topology.all_triangles (Geometry.topology geometry)
      || Geometry.primitive_count geometry = 0 then Ok geometry
    else Triangulate.run geometry |> Result.map_error Error.to_string in
  let* preview = match Geometry.find_attribute ~owner:Attribute.Primitive "piece" geometry with
    | Some _ when Geometry.primitive_count geometry > 0 -> Packed_pieces.of_geometry ~piece_attribute:"piece" geometry
        |> Result.map (fun p -> Pieces p)
    | Some _ | None -> Rays_mesh.to_mesh geometry
        |> Result.map (fun m -> Mesh m) |> Result.map_error Error.to_string in
  let count = Geometry.primitive_count geometry in
  let* roughness = match Geometry.find_attribute ~owner:Attribute.Primitive "material_roughness" geometry with
    | None -> Ok None
    | Some a -> (match Attribute.View.storage a with
        | Attribute.Float a -> Ok (Some a) | _ -> Error "material_roughness must be primitive float") in
  let* round = match Geometry.find_attribute ~owner:Attribute.Primitive "material_round" geometry with
    | None -> Ok None
    | Some a -> (match Attribute.View.storage a with
        | Attribute.Float a -> Ok (Some a) | _ -> Error "material_round must be primitive float") in
  let* emission = match Geometry.find_attribute ~owner:Attribute.Primitive "material_emission" geometry with
    | None -> Ok None
    | Some a -> (match Attribute.View.storage a with
        | Attribute.Float4 a -> Ok (Some a) | _ -> Error "material_emission must be primitive float4") in
  let* color = match Geometry.find_attribute ~owner:Attribute.Primitive "material_color" geometry with
    | None -> Ok None
    | Some a -> (match Attribute.View.storage a with
        | Attribute.Float4 a -> Ok (Some a) | _ -> Error "material_color must be primitive float4") in
  let valid x = Float.is_finite x && x >= 0. && x <= 1. in
  let valid_tuple = Option.fold ~none:true ~some:(fun a ->
    let v = Packed.Float4.View.view a in
    Array.for_all valid v.x && Array.for_all valid v.y && Array.for_all valid v.z) in
  let has_material = roughness <> None || emission <> None || color <> None || round <> None in
  let* () = if Option.fold ~none:true ~some:(Array.for_all (fun x -> Float.is_finite x && x >= 0.)) round then Ok ()
    else Error "material_round must be finite and non-negative" in
  let* () = if Option.fold ~none:true ~some:(Array.for_all valid) roughness
      && valid_tuple color && valid_tuple emission then Ok ()
    else Error "material channels must be finite and in [0,1]" in
  let* () = if not has_material || Topology.all_triangles (Geometry.topology geometry)
      || count = 0 then Ok () else Error "material surfaces require polygon geometry" in
  let batches = if not has_material then [] else
    let table = Hashtbl.create 8 and order = ref [] in
    for primitive = 0 to count - 1 do
      let rough = match roughness with None -> sqrt (2. /. 34.) | Some a -> a.(primitive) in
      let er,eg,eb = match emission with None -> 0.,0.,0.
        | Some a -> let r,g,b,_ = Packed.Float4.get a primitive in r,g,b in
      let r,g,b = match color with None -> 1.,1.,1.
        | Some a -> let r,g,b,_ = Packed.Float4.get a primitive in r,g,b in
      let round = match round with None -> 0. | Some a -> a.(primitive) in
      let key = rough,r,g,b,er,eg,eb,round in
      let indices = match Hashtbl.find_opt table key with
        | Some indices -> indices
        | None -> let indices = ref [] in Hashtbl.add table key indices;
            order := key :: !order; indices in
      indices := primitive :: !indices
    done;
    List.rev_map (fun ((rough,r,g,b,er,eg,eb,round) as key) ->
      let primitives = List.rev !(Hashtbl.find table key) |> Array.of_list in
      let indices = Array.init (Array.length primitives * 3) (fun i -> primitives.(i / 3) * 3 + i mod 3) in
      let shininess = max 0. (2. /. max 0.0001 (rough *. rough) -. 2.) in
      Material.create ~diffuse:(Rays_math.Color.of_floats r g b 1.) ~shininess ~round
        ~emissive:(Rays_math.Color.of_floats er eg eb 1.) (), indices) !order in
  Ok {preview; transforms = instances; batches}

let scene3 ?explosion surface =
  let mesh = match surface.preview with
    | Mesh mesh -> mesh | Pieces pieces -> Packed_pieces.mesh_for_explosion explosion pieces in
  let draw material mesh = match surface.transforms with
    | None -> Scene.D3.mesh ~material ~cull:Scene.D3.Cull_none mesh
    | Some transforms -> Scene.D3.instances_array ~material ~cull:Scene.D3.Cull_none mesh transforms in
  let drawings = match surface.batches with
    | [] -> [draw Material.default mesh]
    | batches ->
        let view = Mesh.Private.packed_view mesh in
        List.map (fun (material, offsets) ->
          let indices = Array.map (Array.get view.indices) offsets in
          let mesh = Mesh.Private.create_packed_shared ~mode:view.mode ~indices
              ?normals:view.normals ?colors:view.colors ?tex_coords:view.tex_coords view.vertices
            |> Result.get_ok in
          draw material mesh) batches in
  Scene.D3.create drawings
