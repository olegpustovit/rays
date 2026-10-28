(* What an editor reads of a cooked geometry: summaries, bounds, the sheet's columns, a wire mesh,
   picking and the probed-iteration tint.  Pure over [Rdk_core.Geometry.t]; no UI. *)
open Rdk_core
open Rays
module A = Rdk_core.Attribute
module G = Rdk_core.Geometry
module P = Rdk_core.Packed

type t = G.t
type surface = Rdk_geom.Surface_index.t

let error_string = Rdk_core.Error.to_string
let points_of_nothing = Rdk_ops.Line_geometry.points [||]
let point_count = G.point_count

let bounds geometry =
  let points = G.positions geometry in
  let count = P.Float3.length points in
  if count = 0 then None else begin
    let x, y, z = P.Float3.get points 0 in
    let lo = [| x; y; z |] and hi = [| x; y; z |] in
    for index = 1 to count - 1 do
      let x, y, z = P.Float3.get points index in
      lo.(0) <- Float.min lo.(0) x; lo.(1) <- Float.min lo.(1) y;
      lo.(2) <- Float.min lo.(2) z; hi.(0) <- Float.max hi.(0) x;
      hi.(1) <- Float.max hi.(1) y; hi.(2) <- Float.max hi.(2) z
    done;
    Some (Rays_math.Vec3.create lo.(0) lo.(1) lo.(2), Rays_math.Vec3.create hi.(0) hi.(1) hi.(2))
  end

type summary = { points : int; prims : int; groups : string list; attributes : (string * string * string * int) list }

let summary g =
  { points = P.Float3.length (G.positions g); prims = G.primitive_count g;
    groups = List.sort_uniq compare (List.map Rdk_core.Group.name (G.groups g));
    attributes = List.map (fun a ->
      let owner = match A.owner a with Point -> "point" | Vertex -> "vertex"
        | Primitive -> "primitive" | Detail -> "detail" in
      owner, A.name a, A.kind_name a, A.length a) (G.attributes g) }

(* a polygon of n corners fans into n - 2 triangles: a count of corners and faces, not a walk *)
let triangles g = max 0 (G.vertex_count g - (2 * G.primitive_count g))

let wire_mesh geometry =
  let topology = G.topology geometry in
  let edges = Rdk_core.Topology_index.create topology in
  let points = G.positions geometry in
  let vertices = Array.init (P.Float3.length points) (fun index ->
    let x, y, z = P.Float3.get points index in Rays_math.Vec3.create x y z) in
  let indices = Array.make (Rdk_core.Topology_index.edge_count edges * 2) 0 in
  for edge = 0 to Rdk_core.Topology_index.edge_count edges - 1 do
    let a, b = Rdk_core.Topology_index.edge_points edges edge in
    indices.(edge * 2) <- a; indices.(edge * 2 + 1) <- b
  done;
  Mesh.Private.create_owned ~mode:Mesh.Lines ~indices vertices

(* ---- the sheet ---- *)

let owner_of = function 0 -> A.Point | 1 -> A.Vertex | 2 -> A.Primitive | _ -> A.Detail

let count owner geometry = match owner_of owner with
  | A.Point -> G.point_count geometry | Vertex -> G.vertex_count geometry
  | Primitive -> G.primitive_count geometry | Detail -> 1

(* Attribute.get copies scalar and text arrays. Borrow immutable storage only
   for this frame's visible cells; no array is mutated or retained by the pane. *)
let columns owner geometry =
  let owner = owner_of owner in
  let number = Param.float_text ~decimal:true in
  let tuple name axes get = List.mapi (fun i axis -> name ^ "." ^ axis,
      fun row -> number (get row i)) axes in
  let attributes = List.concat_map (fun attribute ->
    if A.owner attribute <> owner then [] else
    let name = A.name attribute and length = A.length attribute in
    let guard cell row = if row < length then cell row else "" in
    let columns = match Attribute.View.storage attribute with
      | Float values -> [name, fun row -> number values.(row)]
      | Int values -> [name, fun row -> string_of_int values.(row)]
      | Text values -> [name, fun row -> values.(row)]
      | Int_array values -> [name, fun row -> String.concat " " (Array.to_list (Array.map string_of_int (P.Int_array.get values row)))]
      | Float_array values -> [name, fun row -> String.concat " " (Array.to_list (Array.map number (P.Float_array.get values row)))]
      | Float2 values -> tuple name ["x"; "y"] (fun row component -> let x, y = P.Float2.get values row in if component = 0 then x else y)
      | Float3 values -> tuple name ["x"; "y"; "z"] (fun row component -> let x, y, z = P.Float3.get values row in match component with 0 -> x | 1 -> y | _ -> z)
      | Float4 values -> tuple name ["x"; "y"; "z"; "w"] (fun row component -> let x, y, z, w = P.Float4.get values row in match component with 0 -> x | 1 -> y | 2 -> z | _ -> w) in
    List.map (fun (name, cell) -> name, guard cell) columns) (G.attributes geometry) in
  let groups = List.filter_map (fun group ->
    let matches = match Rdk_core.Group.owner group, owner with
      | Point, A.Point | Vertex, Vertex | Primitive, Primitive -> true | _ -> false in
    if matches then Some ("group:" ^ Rdk_core.Group.name group,
      fun row -> if row < Rdk_core.Group.length group && Rdk_core.Group.mem row group then "1" else "0") else None)
      (G.groups geometry) in
  let positions = if owner <> A.Point then [] else tuple "P" ["x"; "y"; "z"]
      (fun row component -> let x, y, z = P.Float3.get (G.positions geometry) row in
        match component with 0 -> x | 1 -> y | _ -> z) in
  Array.of_list (("index", string_of_int) :: positions @ attributes @ groups)

(* ---- picking ---- *)

let tags ~attribute geometry =
  Option.bind (G.find_attribute ~owner:A.Primitive attribute geometry) (fun a ->
    match Attribute.View.storage a with A.Int values -> Some values | _ -> None)

let surface geometry = Result.to_option (Rdk_geom.Surface_index.create geometry)

let cast ~attribute surface geometry ~origin ~direction =
  match surface, tags ~attribute geometry with
  | Some surface, Some tags ->
      (match Rdk_geom.Surface_index.raycast surface ~origin ~direction with
       | Ok (Some hit) -> Some (hit.distance, tags.(hit.primitive))
       | Ok None | Error _ -> None)
  | _ -> None

let cast_material surface geometry ~origin ~direction =
  match surface with
  | Some surface ->
      (match Rdk_geom.Surface_index.raycast surface ~origin ~direction with
       | Ok (Some hit) ->
           let path = match G.find_attribute ~owner:A.Primitive "shop_materialpath" geometry with
             | Some a -> (match A.storage a with
                 | A.Text paths when paths.(hit.primitive) <> "" -> Some paths.(hit.primitive)
                 | _ -> None)
             | None -> None in
           Some (hit.distance, path)
       | Ok None | Error _ -> None)
  | None -> None

let dim = 0.3 and mix = 0.6

(* The primitives whose tag is in [lit] take the [accent] tint over their own colour, the rest are
   dimmed: a vertex colour per corner, so a prepare that reads [Cd] draws it without knowing about
   picking. *)
let tint ~attribute ~accent:(ar, ag, ab) ~lit geometry =
  match tags ~attribute geometry with
  | Some tags ->
      let topology = G.topology geometry in
      let corners = Rdk_core.Topology.vertex_count topology in
      let base = match G.find_attribute ~owner:A.Vertex "Cd" geometry, G.find_attribute ~owner:A.Point "Cd" geometry with
        | Some a, _ -> (match Attribute.View.storage a with
            | A.Float4 c -> Some (`Vertex (Packed.Float4.View.view c)) | _ -> None)
        | None, Some a -> (match Attribute.View.storage a with
            | A.Float4 c -> Some (`Point (Packed.Float4.View.view c)) | _ -> None)
        | None, None -> None in
      let r = Array.make corners 1. and g = Array.make corners 1.
      and b = Array.make corners 1. and w = Array.make corners 1. in
      for primitive = 0 to Array.length tags - 1 do
        let first, last = Rdk_core.Topology.primitive_vertex_range topology primitive in
        let lit = lit tags.(primitive) in
        for corner = first to last - 1 do
          let cr, cg, cb, cw = match base with
            | None -> 1., 1., 1., 1.
            | Some (`Vertex c) -> c.x.(corner), c.y.(corner), c.z.(corner), c.w.(corner)
            | Some (`Point c) ->
                let p = Rdk_core.Topology.point_of_vertex topology corner in
                c.x.(p), c.y.(p), c.z.(p), c.w.(p) in
          let shade own accent =
            if lit then own *. (1. -. mix) +. accent *. mix else own *. dim in
          r.(corner) <- shade cr ar; g.(corner) <- shade cg ag;
          b.(corner) <- shade cb ab; w.(corner) <- cw
        done
      done;
      (match P.Float4.of_owned ~x:r ~y:g ~z:b ~w with
       | Error _ -> geometry
       | Ok colors ->
           match Result.bind (A.create_key_owned (A.color ~owner:A.Vertex) colors)
               (fun attribute -> G.with_attribute attribute geometry) with
           | Ok geometry -> geometry
           | Error _ -> geometry)
  | None -> geometry
