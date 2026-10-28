open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let run () =
  let grid = Plane_generators.grid ~columns:4 ~rows:4 ~size:2. () |> get_ok in
  let float_values name geometry =
    match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | Some attribute -> (
        match Attribute.View.storage attribute with
        | Attribute.Float values -> values
        | _ -> fail ("unexpected storage for " ^ name))
    | None -> fail ("missing point attribute " ^ name)
  in
  let blur_curve values =
    let geometry =
      Line_geometry_front.polyline [| (0., 0., 0.); (1., 0., 0.); (3., 0., 0.) |] |> get_ok
    in
    let attribute =
      Attribute.create_owned ~name:"value" ~owner:Attribute.Point (Attribute.Float values) |> get_ok
    in
    Geometry.with_attribute attribute geometry |> get_ok
  in
  let uniform_blur =
    Attribute_ops_front.blur_points ~iterations:1 ~mode:(Attribute_blur.Laplacian 1.)
      ~pattern:"value"
      (blur_curve [| 0.; 10.; 0. |])
    |> get_ok
  in
  if float_values "value" uniform_blur <> [| 10.; 0.; 10. |] then fail "uniform Attribute Blur";
  let custom =
    Attribute_ops_front.blur_points ~iterations:2
      ~mode:(Attribute_blur.Custom_steps { odd = 1.; even = 0. })
      ~pattern:"value"
      (blur_curve [| 0.; 10.; 0. |])
    |> get_ok
  in
  if float_values "value" custom <> [| 10.; 0.; 10. |] then
    fail "Attribute Blur custom odd/even steps";
  let tuple_source = blur_curve [| 0.; 10.; 0. |] in
  let uv =
    Attribute.create_owned ~name:"f_uv" ~owner:Attribute.Point
      (Attribute.Float2 (Packed.Float2.of_owned ~x:[| 0.; 10.; 0. |] ~y:[| 2.; 4.; 2. |] |> get_ok))
    |> get_ok
  and color =
    Attribute.create_owned ~name:"f_color" ~owner:Attribute.Point
      (Attribute.Float4
         (Packed.Float4.of_owned ~x:[| 0.; 1.; 0. |] ~y:[| 0.; 2.; 0. |] ~z:[| 0.; 3.; 0. |]
            ~w:[| 1.; 1.; 1. |]
         |> get_ok))
    |> get_ok
  in
  let tuple_source =
    tuple_source |> Geometry.with_attribute uv |> get_ok |> Geometry.with_attribute color |> get_ok
  in
  let tuple_blur =
    Attribute_ops_front.blur_points ~mode:(Attribute_blur.Laplacian 1.) ~pattern:"f_*" tuple_source
    |> get_ok
  in
  (match
     ( Geometry.find_attribute ~owner:Attribute.Point "f_uv" tuple_blur,
       Geometry.find_attribute ~owner:Attribute.Point "f_color" tuple_blur )
   with
  | Some uv, Some color ->
      let uv =
        match Attribute.View.storage uv with
        | Attribute.Float2 values -> Packed.Float2.View.view values
        | _ -> fail "Attribute Blur float2 storage"
      and color =
        match Attribute.View.storage color with
        | Attribute.Float4 values -> Packed.Float4.View.view values
        | _ -> fail "Attribute Blur float4 storage"
      in
      if
        uv.x <> [| 10.; 0.; 10. |]
        || uv.y <> [| 4.; 2.; 4. |]
        || color.z <> [| 3.; 0.; 3. |]
        || color.w <> [| 1.; 1.; 1. |]
      then fail "Attribute Blur tuple planes"
  | _ -> fail "Attribute Blur dropped tuple attributes");
  let smoothed_grid =
    Attribute_ops_front.blur_points ~iterations:1 ~mode:(Attribute_blur.Laplacian 0.25) ~pattern:"P"
      grid
    |> get_ok
  in
  if
    Geometry.find_attribute ~owner:Attribute.Point "N" smoothed_grid <> None
    || Geometry.find_attribute ~owner:Attribute.Vertex "N" smoothed_grid <> None
  then fail "position Attribute Blur retained stale normals";
  (match Attribute_ops_front.blur_points ~iterations:(-1) ~pattern:"P" grid with
  | Error error when Error.code error = "invalid_blur" -> ()
  | _ -> fail "Attribute Blur accepted negative iterations");
  (match Attribute_ops_front.blur_points ~pattern:"value" (blur_curve [| Float.nan; 1.; 2. |]) with
  | Error error when Error.code error = "invalid_blur" -> ()
  | _ -> fail "Attribute Blur accepted non-finite source data");
  ()
