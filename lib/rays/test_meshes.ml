(* Meshes the 3D tests draw. The typed generators are gone from the public
   API (geometry comes from SOP graphs); these build the same shapes through
   the packed constructor so every test shares one definition. *)
open Rays

let packed ?mode ?colors ~indices ~(normals : Rays_math.Vec3.t array) (positions : Rays_math.Vec3.t array) =
  let planes (values : Rays_math.Vec3.t array) : Mesh.Private.vec3_view =
    { x = Array.map (fun (p : Rays_math.Vec3.t) -> p.x) values;
      y = Array.map (fun (p : Rays_math.Vec3.t) -> p.y) values;
      z = Array.map (fun (p : Rays_math.Vec3.t) -> p.z) values } in
  match Mesh.Private.create_packed_owned ?mode ~indices ~normals:(planes normals) ?colors
          (planes positions) with
  | Ok mesh -> mesh
  | Error message -> failwith message

(* Every vertex faces +Z. *)
let facing_z ?mode ?colors ~indices positions =
  packed ?mode ?colors ~indices ~normals:(Array.map (fun _ -> Rays_math.Vec3.unit_z) positions) positions

(* A latitude/longitude sphere about the origin, wound as the other meshes. *)
let sphere ?(segments = 24) ?(rings = 16) ~radius () =
  let row = segments + 1 in
  let normals = Array.init (row * (rings + 1)) (fun index ->
    let theta = float (index mod row) /. float segments *. 2. *. Float.pi
    and phi = float (index / row) /. float rings *. Float.pi in
    Rays_math.Vec3.create (sin phi *. cos theta) (cos phi) (sin phi *. sin theta)) in
  let indices = Array.make (segments * rings * 6) 0 in
  for cell = 0 to (segments * rings) - 1 do
    let a = (cell / segments * row) + (cell mod segments) in
    let b = a + 1 and d = a + row in
    let c = d + 1 in
    Array.blit [| a; b; c; a; c; d |] 0 indices (cell * 6) 6
  done;
  packed ~indices ~normals (Array.map (fun normal -> Rays_math.Vec3.scale normal radius) normals)

(* An axis-aligned box about the origin: four vertices a face, outward normals,
   counter-clockwise seen from outside. *)
let box ~width ~height ~depth () =
  let half = Rays_math.Vec3.create (width /. 2.) (height /. 2.) (depth /. 2.) in
  let faces = [ Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_z; Rays_math.Vec3.neg Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z, Rays_math.Vec3.unit_y;
                Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_z, Rays_math.Vec3.unit_x; Rays_math.Vec3.neg Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z;
                Rays_math.Vec3.unit_z, Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_y; Rays_math.Vec3.neg Rays_math.Vec3.unit_z, Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_x ] in
  let corner (n : Rays_math.Vec3.t) (u : Rays_math.Vec3.t) (v : Rays_math.Vec3.t) su sv =
    let along (a : Rays_math.Vec3.t) s = Rays_math.Vec3.scale a s in
    let p = Rays_math.Vec3.add n (Rays_math.Vec3.add (along u su) (along v sv)) in
    Rays_math.Vec3.create (p.x *. half.x) (p.y *. half.y) (p.z *. half.z) in
  let positions = Array.concat (List.map (fun (n, u, v) ->
      [| corner n u v (-1.) (-1.); corner n u v 1. (-1.); corner n u v 1. 1.;
         corner n u v (-1.) 1. |]) faces)
  and normals = Array.concat (List.map (fun (n, _, _) -> Array.make 4 n) faces)
  and indices = Array.concat (List.mapi (fun face _ ->
      let o = face * 4 in [| o; o + 1; o + 2; o; o + 2; o + 3 |]) faces) in
  packed ~indices ~normals positions
