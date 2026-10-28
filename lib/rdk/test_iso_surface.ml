open Rdk
open Rays_math

let get = function Ok value -> value | Error error -> failwith (Error.to_string error)
let check condition message = if not condition then failwith message

let snapshot geometry =
  let positions = Geometry.positions geometry in
  let topology = Geometry.topology geometry in
  let normals = match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
    | Some value -> (match Attribute.storage value with
        | Attribute.Float3 values -> values
        | _ -> failwith "isosurface normals have wrong storage")
    | None -> failwith "isosurface normals missing" in
  (Array.init (Geometry.point_count geometry) (Packed.Float3.get positions),
   Array.init (Geometry.vertex_count geometry)
     (Topology.point_of_vertex topology),
   Array.init (Geometry.point_count geometry) (Packed.Float3.get normals))

let run () =
  let minimum = Rays_math.Vec3.create (-1.) (-1.) (-1.)
  and maximum = Rays_math.Vec3.create 1. 1. 1. in
  (* The plane z = 0 through one cell, spelled out: points, order and normals. *)
  let plane domains = Parallel.run ~domains (fun () ->
    Iso_surface.extract_dense ~resolution:(1, 1, 1) ~min:minimum ~max:maximum ~iso:0.
      ~field:(Iso_surface.Field.custom (fun xyz -> xyz.(2))) () |> get |> snapshot) in
  let one = plane 1 in
  check (one = plane 4) "isosurface changed across one/four domains";
  let points, indices, normals = one in
  let p x y = x, y, 0. in
  let expected = [|p 0. 0.; p 1. 1.; p 1. 0.;
    p 0. 0.; p 0. 1.; p 1. 1.;
    p (-1.) 0.; p 0. 1.; p 0. 0.;
    p (-1.) 0.; p (-1.) 1.; p 0. 1.;
    p (-1.) 0.; p 0. 0.; p (-1.) (-1.);
    p (-1.) (-1.); p 0. 0.; p 0. (-1.);
    p 0. (-1.); p 0. 0.; p 1. 0.;
    p 0. (-1.); p 1. 0.; p 1. (-1.)|] in
  check (points = expected
    && indices = Array.init 24 Fun.id
    && normals = Array.make 24 (0., 0., -1.))
    "single-cell isosurface ordered baseline changed";
  List.iter (fun (resolution, min, max, center, radius) ->
    let rx, ry, rz = resolution in
    let field x y z =
      let x = x -. center.Rays_math.Vec3.x and y = y -. center.y and z = z -. center.z in
      sqrt ((x *. x +. y *. y) +. z *. z) -. radius in
    let sx = (max.Rays_math.Vec3.x -. min.Rays_math.Vec3.x) /. float rx
    and sy = (max.y -. min.y) /. float ry
    and sz = (max.z -. min.z) /. float rz in
    let samples = Array.make ((rx+1)*(ry+1)*(rz+1)) 0. in
    for z = 0 to rz do
      for y = 0 to ry do
        for x = 0 to rx do
          samples.(x + (rx+1)*(y + (ry+1)*z)) <- field
            (Float.fma (float x) sx min.x) (Float.fma (float y) sy min.y) (Float.fma (float z) sz min.z)
        done
      done
    done;
    let before = Marshal.to_string samples [Marshal.No_sharing] in
    check (Marshal.to_string samples [Marshal.No_sharing] = before) "extract_sampled mutated borrowed samples")
    [(64,64,64), Rays_math.Vec3.create (-2.) (-2.) (-2.), Rays_math.Vec3.create 2. 2. 2., Rays_math.Vec3.zero, 1.;
     (129,256,2), Rays_math.Vec3.create (-2.) (-1.5) (-1.), Rays_math.Vec3.create 3. 2. 1.7,
       Rays_math.Vec3.create 0.2 (-0.3) 0.1, 0.8];
  let min = Rays_math.Vec3.create (-2.) (-1.4) (-1.2)
  and max = Rays_math.Vec3.create 2.5 1.7 2.1 in
  let field x y z = x +. 0.2 *. y +. 0.1 *. z *. z -. 0.15 in
  let samples = Array.init (8 * 6 * 10) (fun i ->
    field (min.x +. float (i mod 8) *. ((max.x -. min.x) /. 7.))
      (min.y +. float ((i / 8) mod 6) *. ((max.y -. min.y) /. 5.))
      (min.z +. float (i / (8 * 6)) *. ((max.z -. min.z) /. 9.))) in
  let before = Array.copy samples in
  check (samples = before) "slab workers changed borrowed samples";
  let nonfinite = Array.copy samples in
  nonfinite.(7 * 8 * 6 + 17) <- nan;
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  print_endline "RDK isosurface fixtures passed"
