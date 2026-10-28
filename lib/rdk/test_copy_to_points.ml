open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let run () =
  let _target_n = Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|1.;0.|] ~y:[|0.;1.|] ~z:[|0.;0.|])) |> get_ok
  and _target_up = Attribute.create_owned ~name:"up" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|0.;0.|] ~y:[|0.;0.|] ~z:[|1.;0.|])) |> get_ok in
  let _velocity = Attribute.create_owned ~name:"v" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|0.|] ~y:[|1.|] ~z:[|0.|])) |> get_ok
  and _velocity_up = Attribute.create_owned ~name:"up" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|0.|] ~y:[|0.|] ~z:[|1.|])) |> get_ok in
  let quarter_turn = sqrt 0.5 in
  let _target_rot = Attribute.create_owned ~name:"rot" ~owner:Attribute.Point
      (Attribute.Float4 (Packed.Float4.of_owned
        ~x:[|0.|] ~y:[|0.|] ~z:[|quarter_turn|] ~w:[|quarter_turn|]
        |> get_ok)) |> get_ok
  and _target_trans = Attribute.create_owned ~name:"trans" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|3.|] ~y:[|0.|] ~z:[|0.|])) |> get_ok
  and _target_pivot = Attribute.create_owned ~name:"pivot" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|1.|] ~y:[|0.|] ~z:[|0.|])) |> get_ok
  and _target_pscale = Attribute.create_owned ~name:"pscale" ~owner:Attribute.Point
      (Attribute.Float [|2.|]) |> get_ok in
  let restricted_source = Mesh_merge.run [
      Line_geometry.polyline [|(0.,0.,0.); (1.,0.,0.)|] |> get_ok;
      Line_geometry.polyline [|(10.,0.,0.); (12.,0.,0.)|] |> get_ok]
      |> get_ok in
  let _restricted_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|0.;1.;10.;12.|]) |> get_ok
  and _restricted_id = Attribute.create_owned ~name:"piece_id"
      ~owner:Attribute.Primitive (Attribute.Int [|100;200|]) |> get_ok
  and _restricted_detail = Attribute.create_owned ~name:"detail_id"
      ~owner:Attribute.Detail (Attribute.Int [|77|]) |> get_ok in
  let source_second = Group.ordered ~owner:Group.Primitive ~name:"source_second"
      ~length:2 [|1|] |> get_ok in
  let restricted_source = Geometry.with_group source_second restricted_source
      |> get_ok in
  let restricted_targets = Line_geometry.points
      [|(0.,0.,0.); (0.,10.,0.); (0.,20.,0.)|] in
  let restricted_scale = Attribute.create_owned ~name:"pscale"
      ~owner:Attribute.Point (Attribute.Float [|1.;9.;2.|]) |> get_ok in
  let restricted_targets = Geometry.with_attribute restricted_scale
      restricted_targets |> get_ok in
  let _restricted_source_bytes = geometry_bytes restricted_source
  and _restricted_targets_bytes = geometry_bytes restricted_targets in
  let cancelled_piece = Cancel.create () in
  Cancel.cancel cancelled_piece;
  let _wrong_source_owner = Group.init ~owner:Group.Point ~name:"wrong" 4
      (fun _ -> true)
  and _wrong_target_owner = Group.init ~owner:Group.Primitive ~name:"wrong" 0
      (fun _ -> false) in
  let transfer_source = Line_geometry.polyline [|(0.,0.,0.); (1.,0.,0.)|] |> get_ok in
  let source_weight = Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float [|10.;20.|]) |> get_ok
  and source_corner = Attribute.create_owned ~name:"corner" ~owner:Attribute.Vertex
      (Attribute.Int [|1;2|]) |> get_ok
  and source_density = Attribute.create_owned ~name:"density"
      ~owner:Attribute.Primitive (Attribute.Float [|2.|]) |> get_ok
  and source_profile = Attribute.create_owned ~name:"profile"
      ~owner:Attribute.Point (Attribute.Float_array
        (Packed.Float_array.create_owned ~offsets:[|0;2;4|]
          ~values:[|10.;20.;30.;40.|] |> get_ok)) |> get_ok
  and source_disabled = Attribute.create_owned ~name:"disabled"
      ~owner:Attribute.Point (Attribute.Int [|7;8|]) |> get_ok in
  let transfer_source = transfer_source
      |> Geometry.with_attribute source_weight |> get_ok
      |> Geometry.with_attribute source_corner |> get_ok
      |> Geometry.with_attribute source_density |> get_ok
      |> Geometry.with_attribute source_profile |> get_ok
      |> Geometry.with_attribute source_disabled |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point ~name:"union_mask" 2
           (fun point -> point = 0)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Vertex
           ~name:"intersect_mask" 2 (fun vertex -> vertex = 0)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Primitive
           ~name:"subtract_mask" 1 (fun _ -> true)) |> get_ok in
  let transfer_targets = Line_geometry.points [|(0.,0.,0.); (0.,10.,0.)|] in
  let target_weight = Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float [|3.;4.|]) |> get_ok
  and target_corner = Attribute.create_owned ~name:"corner" ~owner:Attribute.Point
      (Attribute.Int [|100;200|]) |> get_ok
  and target_density = Attribute.create_owned ~name:"density" ~owner:Attribute.Point
      (Attribute.Float [|5.;7.|]) |> get_ok
  and target_label = Attribute.create_owned ~name:"label" ~owner:Attribute.Point
      (Attribute.Text [|"first";"second"|]) |> get_ok
  and target_gain = Attribute.create_owned ~name:"gain" ~owner:Attribute.Point
      (Attribute.Float [|2.;3.|]) |> get_ok
  and target_profile = Attribute.create_owned ~name:"profile"
      ~owner:Attribute.Point (Attribute.Float_array
        (Packed.Float_array.create_owned ~offsets:[|0;2;4|]
          ~values:[|1.;2.;3.;4.|] |> get_ok)) |> get_ok
  and target_disabled = Attribute.create_owned ~name:"disabled"
      ~owner:Attribute.Point (Attribute.Int [|100;200|]) |> get_ok in
  let transfer_targets = transfer_targets
      |> Geometry.with_attribute target_weight |> get_ok
      |> Geometry.with_attribute target_corner |> get_ok
      |> Geometry.with_attribute target_density |> get_ok
      |> Geometry.with_attribute target_label |> get_ok
      |> Geometry.with_attribute target_gain |> get_ok
      |> Geometry.with_attribute target_profile |> get_ok
      |> Geometry.with_attribute target_disabled |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point ~name:"union_mask" 2
           (fun point -> point = 1)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point
           ~name:"intersect_mask" 2 (fun point -> point = 1)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point
           ~name:"subtract_mask" 2 (fun point -> point = 1)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point ~name:"copy_mask" 2
           (fun point -> point = 1)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point
           ~name:"missing_multiply" 2 (fun point -> point = 1)) |> get_ok
      |> Geometry.with_group (Group.init ~owner:Group.Point
           ~name:"missing_subtract" 2 (fun point -> point = 1)) |> get_ok in
  let _transfer_source_bytes = geometry_bytes transfer_source
  and _transfer_targets_bytes = geometry_bytes transfer_targets in
  print_endline "RDK Copy to Points tests passed"
