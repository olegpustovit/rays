open Rays
open Rdk
open Rdk_test_support

let float3_attribute geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute -> (
      match Attribute.View.storage attribute with
      | Attribute.Float3 values -> Packed.Float3.View.view values
      | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let check_outward_and_regular name radius geometry =
  let point = positions geometry and topology = Geometry.topology geometry in
  let edge_length = ref None in
  for p = 0 to Geometry.point_count geometry - 1 do
    let length =
      sqrt
        ((point.x.(p) *. point.x.(p))
        +. (point.y.(p) *. point.y.(p))
        +. (point.z.(p) *. point.z.(p)))
    in
    check (near length radius) (name ^ " circumsphere radius")
  done;
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    let first, last = Topology.primitive_vertex_range topology primitive in
    let nx = ref 0.
    and ny = ref 0.
    and nz = ref 0.
    and cx = ref 0.
    and cy = ref 0.
    and cz = ref 0. in
    for vertex = first to last - 1 do
      let next = if vertex + 1 = last then first else vertex + 1 in
      let a = Topology.point_of_vertex topology vertex
      and b = Topology.point_of_vertex topology next in
      nx := !nx +. ((point.y.(a) -. point.y.(b)) *. (point.z.(a) +. point.z.(b)));
      ny := !ny +. ((point.z.(a) -. point.z.(b)) *. (point.x.(a) +. point.x.(b)));
      nz := !nz +. ((point.x.(a) -. point.x.(b)) *. (point.y.(a) +. point.y.(b)));
      cx := !cx +. point.x.(a);
      cy := !cy +. point.y.(a);
      cz := !cz +. point.z.(a);
      let dx = point.x.(a) -. point.x.(b)
      and dy = point.y.(a) -. point.y.(b)
      and dz = point.z.(a) -. point.z.(b) in
      let length = sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
      match !edge_length with
      | None -> edge_length := Some length
      | Some expected -> check (near expected length) (name ^ " edge length")
    done;
    check ((!nx *. !cx) +. (!ny *. !cy) +. (!nz *. !cz) > 1e-12) (name ^ " inward polygon")
  done

let run () = print_endline "Platonic tests passed"
