type render_mode = Faces | Wireframe | Vertices
type cull = Cull_none | Cull_back | Cull_front
type shading = Smooth | Flat
type comparison =
  | Never
  | Less
  | Equal
  | Less_equal
  | Greater
  | Not_equal
  | Greater_equal
  | Always
type depth_state = {
  comparison : comparison;
  write : bool;
}
type blend = Replace | Alpha | Add | Multiply | Screen | Subtract

type texture = {
  value : Texture.t;
  filter : Texture.filter;
  wrap_u : Texture.wrap;
  wrap_v : Texture.wrap;
}

type node =
  | Mesh of
      Mesh.t * Material.t * texture option
      * render_mode * cull * shading
  | Instances of
      Mesh.t * Material.t * texture option
      * render_mode * cull * shading * Rays_math.Mat4.t array
  | Group of node list
  | Transform of Rays_math.Mat4.t * node list
  | Depth_state of depth_state * node list
  | Blend_state of blend * node list

type t = {
  nodes : node list;
  lights : Light.t list;
  ambient : Rays_math.Color.t;
  separate_specular : bool;
  samples : int;
  world : World.baked option;
}

let depth_state ?(write = true) () = { comparison = Less; write }

let default_depth = depth_state ()

let empty = {
  nodes = [];
  lights = [];
  ambient = Rays_math.Color.black;
  separate_specular = false;
  samples = 1;
  world = None;
}

let create ?(lights = []) ?(ambient = Rays_math.Color.rgb 32 32 32)
    ?(separate_specular = false)
    ?(samples = 1) nodes =
  if not (List.mem samples [1; 4; 9; 16]) then
    invalid_arg "Scene3.create: samples must be 1, 4, 9, or 16";
  {
    nodes;
    lights;
    ambient;
    separate_specular;
    samples;
    world = None;
  }

let textured ?(filter = Texture.Bilinear) value =
  { value; filter; wrap_u = Texture.Clamp; wrap_v = Texture.Clamp }

let mesh ?(material = Material.default) ?texture ?(mode = Faces)
    ?(cull = Cull_back) ?(shading = Smooth) value =
  Mesh (value, material, texture, mode, cull, shading)

let instances_array ?(material = Material.default) ?(mode = Faces)
    ?(cull = Cull_back) ?(shading = Smooth) value transforms =
  Instances (value, material, None, mode, cull, shading,
    Array.copy transforms)

let nodes scene = scene.nodes
let group nodes = Group nodes
let transform matrix nodes = Transform (matrix, nodes)
let translate value nodes = transform (Rays_math.Mat4.translation value) nodes
let with_depth state nodes = Depth_state (state, nodes)
let with_blend blend nodes = Blend_state (blend, nodes)

let plane ?cull ~width ~height () =
  mesh ?cull (Mesh.plane ~width ~height ())

let with_world baked scene = { scene with world = Some baked }

module Private = struct
  let with_texture texture scene =
    let rec node=function
      |Mesh(mesh,material,_,mode,cull,shading)->Mesh(mesh,material,Some texture,mode,cull,shading)
      |Instances(mesh,material,_,mode,cull,shading,transforms)->Instances(mesh,material,Some texture,mode,cull,shading,transforms)
      |Group children->Group(List.map node children)
      |Transform(matrix,children)->Transform(matrix,List.map node children)
      |Depth_state(state,children)->Depth_state(state,List.map node children)
      |Blend_state(state,children)->Blend_state(state,List.map node children)in
    {scene with nodes=List.map node scene.nodes}
  type drawing = {
    mesh : Mesh.t;
    material : Material.t;
    texture : texture option;
    mode : render_mode;
    cull : cull;

    depth : depth_state;

    blend : blend;
    transform : Rays_math.Mat4.t;
  }

  let cacheable scene =
    let rec nodes = function
      | [] -> true
      | Mesh (_, _, None, _, _, _) :: rest -> nodes rest
      | Instances (_, _, None, _, _, _, _) :: rest -> nodes rest
      | Group nested :: rest
      | Transform (_, nested) :: rest
      | Depth_state (_, nested) :: rest
      | Blend_state (_, nested) :: rest -> nodes nested && nodes rest
      | Mesh (_, _, Some _, _, _, _) :: _
      | Instances (_, _, Some _, _, _, _, _) :: _ -> false in
    nodes scene.nodes

  let drawings scene =
    let rec flatten parent depth blend acc = function
      | [] -> acc
      | Mesh (mesh, material, texture, mode, cull, _shading) :: rest ->
          flatten parent depth blend
            ({ mesh; material; texture; mode; cull;
               depth;   blend;
               transform = parent } :: acc)
            rest
      | Instances
          (mesh, material, texture, mode, cull, _shading, transforms)
        :: rest ->
          let acc =
            Array.fold_left
              (fun acc transform ->
                {
                  mesh;
                  material;
                  texture;
                  mode;
                  cull;

                  depth;

                  blend;
                  transform = Rays_math.Mat4.mul parent transform;
                }
                :: acc)
              acc transforms
          in
          flatten parent depth blend acc rest
      | Group nodes :: rest ->
          flatten parent depth blend
            (flatten parent depth blend acc nodes) rest
      | Transform (matrix, nodes) :: rest ->
          let transform = Rays_math.Mat4.mul parent matrix in
          flatten parent depth blend
            (flatten transform depth blend acc nodes) rest
      | Depth_state (state, nodes) :: rest ->
          flatten parent depth blend
            (flatten parent state blend acc nodes) rest
      | Blend_state (state, nodes) :: rest ->
          flatten parent depth blend
            (flatten parent depth state acc nodes) rest
    in
    List.rev
      (flatten Rays_math.Mat4.identity default_depth Alpha [] scene.nodes)

  let iter_batches operation scene =
    let rec visit parent depth blend = function
      | [] -> ()
      | Mesh (mesh, material, texture, mode, cull, _shading) :: rest ->
          operation { mesh; material; texture; mode; cull;
            depth;   blend; transform = parent } None;
          visit parent depth blend rest
      | Instances (mesh, material, texture, mode, cull, _shading,
          transforms) :: rest ->
          operation { mesh; material; texture; mode; cull;
            depth;   blend; transform = parent }
            (Some transforms);
          visit parent depth blend rest
      | Group nodes :: rest ->
          visit parent depth blend nodes;
          visit parent depth blend rest
      | Transform (matrix, nodes) :: rest ->
          visit (Rays_math.Mat4.mul parent matrix) depth blend nodes;
          visit parent depth blend rest
      | Depth_state (state, nodes) :: rest ->
          visit parent state blend nodes;
          visit parent depth blend rest
      | Blend_state (state, nodes) :: rest ->
          visit parent depth state nodes;
          visit parent depth blend rest in
    visit Rays_math.Mat4.identity default_depth Alpha scene.nodes

  let lights scene = scene.lights
  let ambient scene = scene.ambient
  let separate_specular scene = scene.separate_specular
  let depth_clear _ = 1.
  let samples scene = scene.samples
  let world scene = scene.world
end
