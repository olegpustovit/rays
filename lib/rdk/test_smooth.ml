open Rays
open Rdk
open Rdk_test_support

let add_attribute attribute geometry =
  Geometry.with_attribute attribute geometry |> get_string_ok

let point_float name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing point attribute " ^ name)

let two_curves () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 2.; 10.; 11.; 12.|]
      ~y:[|0.; 3.; 0.; 0.; 4.; 0.|]
      ~z:(Array.make 6 0.) in
  let topology = Topology.Builder.create ~point_count:6
      ~vertex_capacity:6 ~primitive_capacity:2 () in
  Topology.Builder.add_open_polyline topology [|0; 1; 2|];
  Topology.Builder.add_open_polyline topology [|3; 4; 5|];
  Geometry.create ~positions ~topology:(Topology.Builder.freeze topology) ()
  |> get_string_ok

let perturbed_grid () =
  let geometry = Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> get_ok in
  let source = positions geometry in
  let values = Packed.Float3.View.of_owned_exn ~x:(Array.copy source.x)
      ~y:(Array.mapi (fun point y -> if point = 4 then 3. else y) source.y)
      ~z:(Array.copy source.z) in
  Geometry.with_positions values geometry |> get_string_ok

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let equal_geometry left right =
  let left_positions = positions left and right_positions = positions right
  and left_topology = Topology.View.view (Geometry.topology left)
  and right_topology = Topology.View.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)

let run () =
  let curves = two_curves () in
  let heat = Attribute.create_owned ~name:"heat" ~owner:Attribute.Point
      (Attribute.Float [|0.; 6.; 0.; 0.; 8.; 0.|]) |> get_string_ok in
  let curves = add_attribute heat curves in
  let smooth = Smooth.run ~iterations:1
      ~mode:(Attribute_ops.Laplacian 1.) ~attributes:"P heat" curves |> get_ok in
  let p = positions smooth and heat = point_float "heat" smooth in
  check (p.y = [|3.; 0.; 3.; 4.; 0.; 4.|])
    "uniform Smooth did not average curve neighbors";
  check (heat = [|6.; 0.; 6.; 8.; 0.; 8.|])
    "Smooth did not process matching point attributes";

  let first = Group.init ~owner:Group.Primitive ~name:"first" 2
      (fun primitive -> primitive = 0) in
  let selected = Smooth.run ~primitives:first ~iterations:1
      ~mode:(Attribute_ops.Laplacian 1.) ~attributes:"P" curves |> get_ok in
  let selected = positions selected and source = positions curves in
  check (selected.y.(0) = 3. && selected.y.(1) = 0. && selected.y.(2) = 3.)
    "primitive-restricted Smooth missed selected points";
  for point = 3 to 5 do
    check (selected.x.(point) = source.x.(point)
        && selected.y.(point) = source.y.(point)
        && selected.z.(point) = source.z.(point))
      "primitive-restricted Smooth changed an unselected curve"
  done;

  let constrained = Group.init ~owner:Group.Point ~name:"locked" 6
      (fun point -> point = 1 || point = 4) in
  let locked = Smooth.run ~constrained_points:constrained ~iterations:3
      ~mode:(Attribute_ops.Laplacian 0.5) ~attributes:"P" curves |> get_ok in
  check ((positions locked).y.(1) = 3. && (positions locked).y.(4) = 4.)
    "constrained points moved";

  let grid = perturbed_grid () in
  let free = Smooth.run ~iterations:1 ~mode:(Attribute_ops.Laplacian 1.)
      ~attributes:"P" grid |> get_ok in
  let pinned = Smooth.run ~boundary:Smooth.Smooth_unshared ~iterations:1
      ~mode:(Attribute_ops.Laplacian 1.) ~attributes:"P" grid |> get_ok in
  let source = positions grid and free = positions free and pinned = positions pinned in
  check (pinned.y.(4) < source.y.(4)) "interior point was not smoothed";
  for point = 0 to 8 do
    if point <> 4 then begin
      check (pinned.x.(point) = source.x.(point)
          && pinned.y.(point) = source.y.(point)
          && pinned.z.(point) = source.z.(point))
        "unshared-boundary Smooth moved a boundary point"
    end
  done;
  check (Array.exists2 (fun left right -> left <> right) free.y source.y)
    "free Smooth was unexpectedly an identity";

  let region = Plane_generators.grid ~columns:4 ~rows:4 ~size:4. () |> get_ok in
  let region_source = positions region in
  let region = Geometry.with_positions
      (Packed.Float3.View.of_owned_exn ~x:(Array.copy region_source.x)
        ~y:(Array.mapi (fun point y -> if point = 12 then 4. else y)
          region_source.y)
        ~z:(Array.copy region_source.z)) region |> get_string_ok in
  let region_source = positions region in
  let center_faces = Group.init ~owner:Group.Primitive ~name:"center_faces"
      (Geometry.primitive_count region) (fun primitive ->
        let cell = primitive / 2 in
        let x = cell mod 4 and row = cell / 4 in
        x >= 1 && x <= 2 && row >= 1 && row <= 2) in
  let group_locked = Smooth.run ~primitives:center_faces
      ~boundary:Smooth.Smooth_group_boundary ~iterations:2
      ~mode:(Attribute_ops.Laplacian 0.5) ~attributes:"P" region |> get_ok in
  let group_locked = positions group_locked in
  check (group_locked.y.(12) < region_source.y.(12))
    "group-boundary Smooth did not update the selected interior";
  List.iter (fun point ->
    check (group_locked.x.(point) = region_source.x.(point)
        && group_locked.y.(point) = region_source.y.(point)
        && group_locked.z.(point) = region_source.z.(point))
      "group-boundary Smooth moved a selected boundary point")
    [6; 7; 8; 11; 13; 16; 17; 18];
  List.iter (fun point ->
    check (group_locked.x.(point) = region_source.x.(point)
        && group_locked.y.(point) = region_source.y.(point)
        && group_locked.z.(point) = region_source.z.(point))
      "group-boundary Smooth moved an unselected point")
    [0; 1; 2; 3; 4; 5; 9; 10; 14; 15; 19; 20; 21; 22; 23; 24];

  let with_normals = Normal_ops.run grid |> get_ok in
  let recomputed = Smooth.run ~boundary:Smooth.Smooth_unshared ~iterations:1
      ~mode:(Attribute_ops.Laplacian 0.5) ~attributes:"P" with_normals |> get_ok in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" recomputed <> None)
    "Smooth did not recompute existing normals";
  let invalidated = Smooth.run ~recompute_normals:false ~iterations:1
      ~mode:(Attribute_ops.Laplacian 0.5) ~attributes:"P" with_normals |> get_ok in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" invalidated = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" invalidated = None)
    "Smooth retained stale normals";
  let authored_n = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.; 1.; 0.|] ~y:(Array.make 6 0.)
      ~z:(Array.make 6 0.) in
  let authored_n = Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3 authored_n) |> get_string_ok in
  let smoothed_n = curves |> add_attribute authored_n
      |> Smooth.run ~iterations:1 ~mode:(Attribute_ops.Laplacian 1.)
           ~attributes:"P N" |> get_ok in
  (match Geometry.find_attribute ~owner:Attribute.Point "N" smoothed_n with
   | Some attribute ->
       (match Attribute.View.storage attribute with
        | Attribute.Float3 values ->
            let values = Packed.Float3.View.view values in
            check (values.x = [|1.; 0.; 1.; 1.; 0.; 1.|])
              "explicitly smoothed N was replaced by geometric normals"
        | _ -> fail "smoothed N has unexpected storage")
   | None -> fail "explicitly smoothed N was removed");

  let wrong_owner = Group.init ~owner:Group.Point ~name:"wrong"
      (Geometry.point_count grid) (fun _ -> true) in
  expect_code "invalid_selection" (Smooth.run ~primitives:wrong_owner
      ~attributes:"P" grid);
  let wrong_length = Group.init ~owner:Group.Point ~name:"wrong_length" 1
      (fun _ -> true) in
  expect_code "invalid_selection" (Smooth.run
      ~constrained_points:wrong_length ~attributes:"P" grid);
  expect_code "invalid_parameter" (Smooth.run ~grain:0 ~attributes:"P" grid);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (Smooth.run ~cancel:cancelled ~attributes:"P" grid);

  let scale = Plane_generators.grid ~columns:400 ~rows:250 ~size:40. () |> get_ok
      |> Deform.noise_displace ~amplitude:0.8 ~frequency:0.41 ~seed:73 |> get_ok in
  let point_count = Geometry.point_count scale in
  let weight = Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float (Array.init point_count (fun point ->
        0.25 +. (float_of_int (point mod 257) /. 256.)))) |> get_string_ok in
  let scale = add_attribute weight scale in
  let run domains = Parallel.run ~domains (fun () ->
    Smooth.run ~grain:1024 ~iterations:8 ~method_:Attribute_ops.Edge_length
      ~mode:(Attribute_ops.Custom_steps { odd = 0.43; even = -0.45 })
      ~weight_attribute:"weight" ~boundary:Smooth.Smooth_unshared
      ~attributes:"P weight" scale |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Smooth geometry differ";
  check (Geometry.point_count one = point_count
      && Geometry.vertex_count one = Geometry.vertex_count scale
      && Geometry.primitive_count one = Geometry.primitive_count scale)
    "Smooth changed topology cardinality"
