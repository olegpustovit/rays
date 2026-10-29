type mode = Points | Lines | Line_strip | Line_loop | Triangles | Triangle_strip | Triangle_fan

module Vec3_buffer = struct
  type t = { x : float array; y : float array; z : float array }

  let length values = Array.length values.x
  let get values index = Rays_math.Vec3.create values.x.(index) values.y.(index) values.z.(index)

  let of_array values =
    let count = Array.length values in
    let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
    Array.iteri
      (fun index value ->
        x.(index) <- value.Rays_math.Vec3.x;
        y.(index) <- value.y;
        z.(index) <- value.z)
      values;
    { x; y; z }

  let to_array values = Array.init (length values) (get values)
  let to_list values = Array.to_list (to_array values)

  let init ?(parallel = false) count make =
    let x = Array.make count 0. and y = Array.make count 0. and z = Array.make count 0. in
    let fill index =
      let value = make index in
      x.(index) <- value.Rays_math.Vec3.x;
      y.(index) <- value.y;
      z.(index) <- value.z
    in
    if count > 0 then
      if parallel then Parallel.for_ ~chunk_size:4096 ~start:0 ~finish:(count - 1) fill
      else
        for index = 0 to count - 1 do
          fill index
        done;
    { x; y; z }
end

type device = {
  key : string;
  count : int;
  device_normals : bool;
  keep : unit -> unit;
  host : (unit -> t) option;
      (* the mesh as the host builds it (reading the positions back), made once, for a reader of
         vertices (a path tracer, a sun fit, a centroid) *)
}

and t = {
  mode : mode;
  vertices : Vec3_buffer.t;
  indices : int array;
  normals : Vec3_buffer.t option;
  colors : Rays_math.Color.t array option;
  tex_coords : Rays_math.Vec2.t array option;
  device : device option;
      (* positions (and normals) a device holds: [vertices] is then empty; [keep] stays with the
         mesh, which its producer watches to release its buffers *)
}

(* a device mesh's host copy where a reader needs vertices; any other mesh itself *)
let readable mesh = match mesh.device with Some { host = Some host; _ } -> host () | _ -> mesh

let validate_attribute_array name vertex_count = function
  | None -> Ok None
  | Some values when Array.length values = vertex_count -> Ok (Some values)
  | Some values ->
      Error
        (Printf.sprintf "Mesh.create: %s count (%d) must match vertex count (%d)" name
           (Array.length values) vertex_count)

let create_owned ?(mode = Triangles) ?indices ?normals ?colors ?tex_coords vertices =
  let vertex_count = Array.length vertices in
  let indices = Option.value indices ~default:(Array.init vertex_count Fun.id) in
  match Array.find_opt (fun index -> index < 0 || index >= vertex_count) indices with
  | Some index -> Error (Printf.sprintf "Mesh.create: index %d is outside the vertex array" index)
  | None -> (
      match
        ( validate_attribute_array "normal" vertex_count normals,
          validate_attribute_array "color" vertex_count colors,
          validate_attribute_array "texture coordinate" vertex_count tex_coords )
      with
      | Ok normals, Ok colors, Ok tex_coords ->
          Ok
            {
              mode;
              vertices = Vec3_buffer.of_array vertices;
              indices;
              normals = Option.map Vec3_buffer.of_array normals;
              colors;
              tex_coords;
              device = None;
            }
      | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message)

let create_packed_owned ?(mode = Triangles) ?indices ?normals ?colors ?tex_coords
    (vertices : Vec3_buffer.t) =
  let vertex_count = Vec3_buffer.length vertices in
  let valid_vec3_attribute name = function
    | None -> Ok None
    | Some values when Vec3_buffer.length values = vertex_count -> Ok (Some values)
    | Some values ->
        Error
          (Printf.sprintf "Mesh.create: %s count (%d) must match vertex count (%d)" name
             (Vec3_buffer.length values) vertex_count)
  in
  let indices = Option.value indices ~default:(Array.init vertex_count Fun.id) in
  match Array.find_opt (fun index -> index < 0 || index >= vertex_count) indices with
  | Some index -> Error (Printf.sprintf "Mesh.create: index %d is outside the vertex array" index)
  | None -> (
      match
        ( valid_vec3_attribute "normal" normals,
          validate_attribute_array "color" vertex_count colors,
          validate_attribute_array "texture coordinate" vertex_count tex_coords )
      with
      | Ok normals, Ok colors, Ok tex_coords ->
          Ok { mode; vertices; indices; normals; colors; tex_coords; device = None }
      | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message)

let mode (mesh : t) = mesh.mode
let vertices (mesh : t) = Vec3_buffer.to_list (readable mesh).vertices

let vertex_count (mesh : t) =
  match mesh.device with Some d -> d.count | None -> Vec3_buffer.length mesh.vertices

let index_count (mesh : t) = Array.length mesh.indices

let centroid (mesh : t) =
  let mesh = readable mesh in
  let count = Vec3_buffer.length mesh.vertices in
  if count = 0 then None
  else
    let x = ref 0. and y = ref 0. and z = ref 0. in
    for index = 0 to count - 1 do
      x := !x +. mesh.vertices.x.(index);
      y := !y +. mesh.vertices.y.(index);
      z := !z +. mesh.vertices.z.(index)
    done;
    let inverse = 1. /. float_of_int count in
    Some (Rays_math.Vec3.create (!x *. inverse) (!y *. inverse) (!z *. inverse))

let vertex index (mesh : t) =
  let mesh = readable mesh in
  if index < 0 || index >= Vec3_buffer.length mesh.vertices then None
  else Some (Vec3_buffer.get mesh.vertices index)

let with_mode mode mesh = { mesh with mode }

let rec recalculate_normals mesh =
  if Option.is_some mesh.device then mesh else recalculate_host_normals mesh

and recalculate_host_normals mesh =
  let vertex_count = Vec3_buffer.length mesh.vertices in
  let nx = Array.make vertex_count 0.
  and ny = Array.make vertex_count 0.
  and nz = Array.make vertex_count 0. in
  let add_triangle a b c =
    let abx = mesh.vertices.x.(b) -. mesh.vertices.x.(a)
    and aby = mesh.vertices.y.(b) -. mesh.vertices.y.(a)
    and abz = mesh.vertices.z.(b) -. mesh.vertices.z.(a)
    and acx = mesh.vertices.x.(c) -. mesh.vertices.x.(a)
    and acy = mesh.vertices.y.(c) -. mesh.vertices.y.(a)
    and acz = mesh.vertices.z.(c) -. mesh.vertices.z.(a) in
    let x = (aby *. acz) -. (abz *. acy)
    and y = (abz *. acx) -. (abx *. acz)
    and z = (abx *. acy) -. (aby *. acx) in
    nx.(a) <- nx.(a) +. x;
    ny.(a) <- ny.(a) +. y;
    nz.(a) <- nz.(a) +. z;
    nx.(b) <- nx.(b) +. x;
    ny.(b) <- ny.(b) +. y;
    nz.(b) <- nz.(b) +. z;
    nx.(c) <- nx.(c) +. x;
    ny.(c) <- ny.(c) +. y;
    nz.(c) <- nz.(c) +. z
  in
  let values = mesh.indices and count = Array.length mesh.indices in
  (match mesh.mode with
  | Triangles ->
      for face = 0 to (count / 3) - 1 do
        let offset = face * 3 in
        add_triangle values.(offset) values.(offset + 1) values.(offset + 2)
      done
  | Triangle_strip ->
      for face = 0 to count - 3 do
        if face land 1 = 0 then add_triangle values.(face) values.(face + 1) values.(face + 2)
        else add_triangle values.(face + 1) values.(face) values.(face + 2)
      done
  | Triangle_fan ->
      for face = 0 to count - 3 do
        add_triangle values.(0) values.(face + 1) values.(face + 2)
      done
  | Points | Lines | Line_strip | Line_loop -> ());
  let normals =
    Vec3_buffer.init ~parallel:true vertex_count (fun index ->
        let x = nx.(index) and y = ny.(index) and z = nz.(index) in
        let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
        if length <= 1e-18 then Rays_math.Vec3.zero
        else Rays_math.Vec3.create (x /. length) (y /. length) (z /. length))
  in
  { mesh with normals = Some normals }

let positive name value =
  if (not (Float.is_finite value)) || value <= 0. then
    invalid_arg ("Mesh." ^ name ^ ": dimensions must be finite and positive")

let plane ~width ~height () =
  positive "plane" width;
  positive "plane" height;
  let x = width *. 0.5 and y = height *. 0.5 in
  let vertices =
    [|
      Rays_math.Vec3.create (-.x) (-.y) 0.;
      Rays_math.Vec3.create x (-.y) 0.;
      Rays_math.Vec3.create (-.x) y 0.;
      Rays_math.Vec3.create x y 0.;
    |]
  and normals = Array.make 4 Rays_math.Vec3.unit_z
  and tex_coords =
    [|
      Rays_math.Vec2.create 0. 1.;
      Rays_math.Vec2.create 1. 1.;
      Rays_math.Vec2.create 0. 0.;
      Rays_math.Vec2.create 1. 0.;
    |]
  and indices = [| 0; 1; 3; 0; 3; 2 |] in
  match create_owned ~indices ~normals ~tex_coords vertices with
  | Ok mesh -> mesh
  | Error message -> invalid_arg message

let device_identity = Atomic.make [||]
let device_checked = Atomic.make ([||], 0)

module View = struct
  type vec3_view = Vec3_buffer.t = { x : float array; y : float array; z : float array }

  type view = {
    mode : mode;
    vertices : Rays_math.Vec3.t array;
    normals : Rays_math.Vec3.t array option;
    colors : Rays_math.Color.t array option;
    tex_coords : Rays_math.Vec2.t array option;
  }

  let view (mesh : t) =
    let mesh = readable mesh in
    {
      mode = mesh.mode;
      vertices = Vec3_buffer.to_array mesh.vertices;
      normals = Option.map Vec3_buffer.to_array mesh.normals;
      colors = mesh.colors;
      tex_coords = mesh.tex_coords;
    }

  type packed_view = {
    mode : mode;
    vertices : vec3_view;
    indices : int array;
    normals : vec3_view option;
    colors : Rays_math.Color.t array option;
    tex_coords : Rays_math.Vec2.t array option;
  }

  let unread (mesh : t) =
    {
      mode = mesh.mode;
      vertices = mesh.vertices;
      indices = mesh.indices;
      normals = mesh.normals;
      colors = mesh.colors;
      tex_coords = mesh.tex_coords;
    }

  let packed_view mesh = unread (readable mesh)
  let create_owned ?mode ?indices vertices = create_owned ?mode ?indices vertices
  let create_packed_owned = create_packed_owned
  let create_packed_shared = create_packed_owned

  let create_device ?(mode = Triangles) ?indices ?colors ?tex_coords ?host ~key ~normals ~keep count
      =
    let host =
      Option.map
        (fun make ->
          let lock = Mutex.create () and made = ref None in
          fun () ->
            Mutex.protect lock (fun () ->
                match !made with
                | Some mesh -> mesh
                | None ->
                    let mesh = make () in
                    made := Some mesh;
                    mesh))
        host
    in
    (* a frame's device mesh has the last one's indices: the same cloud identity, the same
       topology's corners, so neither is made or checked again *)
    let indices =
      match indices with
      | Some indices -> indices
      | None -> (
          match Atomic.get device_identity with
          | identity when Array.length identity = count -> identity
          | _ ->
              let identity = Array.init count Fun.id in
              Atomic.set device_identity identity;
              identity)
    in
    let checked =
      match Atomic.get device_checked with
      | known, n when known == indices && n = count -> None
      | _ ->
          let bad = Array.find_opt (fun index -> index < 0 || index >= count) indices in
          if bad = None then Atomic.set device_checked (indices, count);
          bad
    in
    if key = "" || count <= 0 then Error "Mesh.create_device: a device mesh has vertices"
    else
      match checked with
      | Some index ->
          Error (Printf.sprintf "Mesh.create_device: index %d is outside the vertex array" index)
      | None -> (
          match
            ( validate_attribute_array "color" count colors,
              validate_attribute_array "texture coordinate" count tex_coords )
          with
          | Ok colors, Ok tex_coords ->
              Ok
                {
                  mode;
                  vertices = { x = [||]; y = [||]; z = [||] };
                  indices;
                  normals = None;
                  colors;
                  tex_coords;
                  device = Some { key; count; device_normals = normals; keep; host };
                }
          | Error message, _ | _, Error message -> Error message)

  let device (mesh : t) = Option.map (fun d -> (d.key, d.device_normals)) mesh.device

  let triangle_count (mesh : t) =
    match mesh.mode with
    | Triangles -> Array.length mesh.indices / 3
    | Triangle_strip | Triangle_fan -> max 0 (Array.length mesh.indices - 2)
    | Points | Lines | Line_strip | Line_loop -> 0
end
