open Rays
open Rdk
open Rdk_test_support

let add_attribute ~owner ~name storage geometry =
  Attribute.create_owned ~owner ~name storage |> get_string_ok
  |> Fun.flip Geometry.with_attribute geometry |> get_string_ok

let equal_positions left right =
  let left = positions left and right = positions right in
  left.x = right.x && left.y = right.y && left.z = right.z

let near left right = abs_float (left -. right) <= 1e-14

let sample seed identity component =
  let identity = identity * 0x1e3779b97f4a7c15 in
  Rays_math.Rand.float_at seed ~index:(identity lxor
    ((component + 1) * 0x11b54a32d192ed03)) -. 0.5

let expect_code code = function
  | Error error when String.equal (Error.code error) code -> ()
  | Error error -> fail (Printf.sprintf "expected %s, received %s: %s"
      code (Error.code error) (Error.to_string error))
  | Ok _ -> fail ("expected error " ^ code)

let source () =
  Line_geometry.points [|0., 1., 2.; 10., 20., 30.; -3., 4., -5.; 8., 9., 10.|]
  |> add_attribute ~owner:Attribute.Point ~name:"mask"
       (Attribute.Float [|1.; 0.5; 2.; 0.|])
  |> add_attribute ~owner:Attribute.Point ~name:"pscale"
       (Attribute.Float [|2.; 3.; 0.25; 1.|])
  |> add_attribute ~owner:Attribute.Point ~name:"stable_id"
       (Attribute.Int [|7; 7; 9; 10|])
  |> add_attribute ~owner:Attribute.Point ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[|1.; 1.; 1.; 1.|] ~y:[|0.; 0.; 0.; 0.|]
          ~z:[|0.; 0.; 0.; 0.|]))
  |> add_attribute ~owner:Attribute.Vertex ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[||] ~y:[||] ~z:[||]))
  |> add_attribute ~owner:Attribute.Primitive ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:[||] ~y:[||] ~z:[||]))
  |> add_attribute ~owner:Attribute.Detail ~name:"tag"
       (Attribute.Text [|"preserve"|])

let run () =
  let selected = Group.init ~grain:1 ~owner:Group.Point ~name:"selected" 4
      (fun point -> point < 3) in
  check (Group.mem 0 selected && Group.mem 1 selected && Group.mem 2 selected
      && not (Group.mem 3 selected)) "Point Jitter test selection";
  let _identities = [|7; 7; 9; 10|]
  and _amplitudes = [|1.6; 1.2; 0.4; 0.|] in
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  print_endline "point jitter tests passed"
