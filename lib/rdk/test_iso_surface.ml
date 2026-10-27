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
  let corner_to_flat = [|0;1;3;2;4;5;7;6|] in
  let masks = Buffer.create 262144 in
  List.iter (fun smooth -> List.iter (fun inside ->
    for mask = 0 to 255 do
      let samples = Array.make 8 (-1.) in
      Array.iteri (fun corner flat ->
        if mask land (1 lsl corner) <> 0 then samples.(flat) <- inside) corner_to_flat;
      let expected = List.fold_left (fun triangles corners ->
        let positives = List.fold_left (fun count corner ->
          count + if mask land (1 lsl corner) <> 0 then 1 else 0) 0 corners in
        triangles + match positives with 0 | 4 -> 0 | 1 | 3 -> 1 | 2 -> 2
          | _ -> assert false) 0
          [[0;1;2;6];[0;2;3;6];[0;3;7;6];[0;7;4;6];[0;4;5;6];[0;5;1;6]] in
      let result = Iso_surface.extract_sampled ~smooth ~resolution:(1,1,1)
          ~min:minimum ~max:maximum ~iso:0. ~samples () in
      Buffer.add_string masks (Marshal.to_string (smooth, inside, mask,
        match result with
        | Ok geometry -> Ok (Rdk_test_support.geometry_bytes geometry)
        | Error error -> Error (Error.message error)) [Marshal.No_sharing]);
      match result with
      | Error error -> check (expected = 0 && Error.message error =
          "Iso3.extract: the requested isosurface is empty")
          "cube mask returned an unexpected empty-surface error"
      | Ok geometry -> check (expected > 0 && Geometry.primitive_count geometry = expected)
          "cube mask triangle count differs from independent tetrahedron counts"
    done) [1.;0.]) [true;false];
  let mask_hash = Digest.to_hex (Digest.string (Buffer.contents masks)) in
  check (mask_hash = "4efce5e9c56d8d5fb9ead44e339bded7")
    ("all-mask full geometry golden changed: " ^ mask_hash);
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
  List.iter (fun domains -> Parallel.run ~domains (fun () ->
    let min = Rays_math.Vec3.create (-1.) (-1.) (-3.) and max = Rays_math.Vec3.create 1. 1. 3. in
    let dense = Iso_surface.extract_dense ~resolution:(1,1,3) ~min ~max ~iso:0.
        ~field:(Iso_surface.Field.custom (fun xyz -> xyz.(2))) () |> get |> snapshot in
    let samples = [| -3.;-3.;-3.;-3.; -1.;-1.;-1.;-1.;
                     1.;1.;1.;1.; 3.;3.;3.;3. |] in
    let sampled = Iso_surface.extract_sampled ~resolution:(1,1,3) ~min ~max ~iso:0.
        ~samples () |> get |> snapshot in
    check (dense = one && sampled = one)
      "empty/crossing/empty slabs changed the independent ordered plane golden")) [1;8];
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
    List.iter (fun smooth ->
      let expected = Parallel.run ~domains:1 (fun () ->
        Iso_surface.extract_sampled ~grain:max_int ~smooth ~resolution ~min ~max ~iso:0. ~samples ()
        |> get |> Rdk_test_support.geometry_bytes) in
      List.iter (fun grain -> List.iter (fun domains -> Parallel.run ~domains (fun () ->
        let sampled = Iso_surface.extract_sampled ~grain ~smooth ~resolution ~min ~max ~iso:0. ~samples ()
          |> get in
        check (Rdk_test_support.geometry_bytes sampled = expected)
          "sampled isosurface bytes differ by layout, normals, grain or domains")) [1;8]) [16_384;257;max_int]) [true;false];
    let dense = Iso_surface.extract_dense ~resolution ~min ~max ~iso:0.
      ~field:(Iso_surface.Field.custom (fun xyz -> field xyz.(0) xyz.(1) xyz.(2))) () |> get in
    check (Rdk_test_support.geometry_bytes dense
        = Rdk_test_support.geometry_bytes (Iso_surface.extract_sampled ~resolution ~min ~max ~iso:0. ~samples () |> get))
      "dense isosurface differs from the sampled lattice";
    check (Marshal.to_string samples [Marshal.No_sharing] = before) "extract_sampled mutated borrowed samples")
    [(64,64,64), Rays_math.Vec3.create (-2.) (-2.) (-2.), Rays_math.Vec3.create 2. 2. 2., Rays_math.Vec3.zero, 1.;
     (129,256,2), Rays_math.Vec3.create (-2.) (-1.5) (-1.), Rays_math.Vec3.create 3. 2. 1.7,
       Rays_math.Vec3.create 0.2 (-0.3) 0.1, 0.8];
  let resolution = 7,5,9 in
  let min = Rays_math.Vec3.create (-2.) (-1.4) (-1.2)
  and max = Rays_math.Vec3.create 2.5 1.7 2.1 in
  let field x y z = x +. 0.2 *. y +. 0.1 *. z *. z -. 0.15 in
  let samples = Array.init (8 * 6 * 10) (fun i ->
    field (min.x +. float (i mod 8) *. ((max.x -. min.x) /. 7.))
      (min.y +. float ((i / 8) mod 6) *. ((max.y -. min.y) /. 5.))
      (min.z +. float (i / (8 * 6)) *. ((max.z -. min.z) /. 9.))) in
  let before = Array.copy samples in
  List.iter (fun smooth ->
    let expected = Parallel.run ~domains:1 (fun () ->
      Iso_surface.extract_sampled ~grain:max_int ~smooth ~resolution ~min ~max ~iso:0. ~samples ()
      |> get |> Rdk_test_support.geometry_bytes) in
    List.iter (fun grain -> List.iter (fun domains -> Parallel.run ~domains (fun () ->
      let actual = Iso_surface.extract_sampled ~grain ~smooth ~resolution ~min ~max
        ~iso:0. ~samples () |> get |> Rdk_test_support.geometry_bytes in
      check (actual = expected) "sampled slab chunk seam changed complete geometry or normals"))
      [1;8]) [35;70;315;max_int]) [true;false];
  check (samples = before) "slab workers changed borrowed samples";
  let nonfinite = Array.copy samples in
  nonfinite.(7 * 8 * 6 + 17) <- nan;
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  List.iter (fun domains -> Parallel.run ~domains (fun () ->
    (match Iso_surface.extract_sampled ~grain:70 ~resolution ~min ~max ~iso:0.
        ~samples:nonfinite () with
     | Error error -> check (Error.message error = "Iso3.extract: field returned a non-finite sample")
         "nonfinite slab worker returned the wrong typed error"
     | Ok _ -> failwith "nonfinite slab worker succeeded");
    (match Iso_surface.extract_sampled ~cancel:cancelled ~grain:70 ~resolution ~min ~max
        ~iso:0. ~samples () with
     | Error error -> check (Error.code error = "cancelled") "cancelled slab worker returned the wrong error"
     | Ok _ -> failwith "cancelled slab workers succeeded"))) [1;8];
  List.iter (fun (resolution, grain, samples) ->
    check (Result.is_error (Iso_surface.extract_sampled ~grain ~resolution
      ~min:minimum ~max:maximum ~iso:0. ~samples ())) "invalid sampled lattice accepted")
    [(1,1,1),16_384,[||]; (1,1,1),16_384,Array.make 7 0.;
     (1,1,1),16_384,Array.make 9 0.; (1,1,1),0,Array.make 8 0.;
     (0,1,1),16_384,[||]; (Sys.max_array_length-1,Sys.max_array_length-1,2),16_384,[||];
     (1,1,1),16_384,Array.make 8 nan];
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Iso_surface.extract_sampled ~cancel ~resolution:(1,1,1)
      ~min:minimum ~max:maximum ~iso:0. ~samples:(Array.make 8 0.) () with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> failwith "cancelled sampled isosurface succeeded");
  print_endline "RDK isosurface fixtures passed"
