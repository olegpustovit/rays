open Rdk
open Rdk_test_support

let triangulate x y = Delaunay2.build ~seed:17L ~x ~y () |> get_string

let cdt ?workspace ?(flood = false) ?winding ?(remove_outside_polygons = false) x y constraints =
  let seed = triangulate x y in
  let view = Delaunay2.View.view seed in
  Planar_cdt.build ?workspace ~point_count:(Array.length x)
    ~orient:(fun a b c -> Predicates.orient2d_packed ~x ~y a b c)
    ~incircle:(fun a b c d -> Predicates.incircle_packed ~x ~y a b c d)
    ~bounds_overlap:(fun a b u v ->
      max (min x.(a) x.(b)) (min x.(u) x.(v)) <= min (max x.(a) x.(b)) (max x.(u) x.(v))
      && max (min y.(a) y.(b)) (min y.(u) y.(v)) <= min (max y.(a) y.(b)) (max y.(u) y.(v)))
    ~triangle_points:view.triangle_points ~flood_from_hull_boundary:flood
    ~constraint_points:constraints ?constraint_winding:winding
    ~remove_outside_constraint_polygons:remove_outside_polygons ()

let test_crossing_rejected () =
  let x = [| 0.; 1.; 1.; 0. |] and y = [| 0.; 0.; 1.; 1. |] in
  match cdt x y [| 0; 2; 1; 3 |] with
  | Error _ -> ()
  | Ok _ -> failf "crossing constraints succeeded without splitting"

let run () = test_crossing_rejected ()
