(* Viewport provenance.  Every collecting merge of a workspace tags
   its primitives with [Flow_sop.Lower.tag]; this module reads the tag back
   from the displayed geometry, casts a CPU ray to find the primitive under the
   pointer, and tints the primitives of the probed iteration.

   ponytail: a CPU ray over [Rdk.Surface_index] (a BVH of the displayed
   geometry, built at the first click and kept with the piece), not an ID
   buffer.  The upgrade path when meshes pass about 1M triangles is an OGPU
   ID-buffer feature (skill add-ogpu-feature). *)
open Rays

module Set = Set.Make (Int)

let tags geometry =
  Option.bind (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive
    Flow_sop.Lower.source_attribute geometry) (fun attribute ->
      match Rdk.Attribute.View.storage attribute with
      | Rdk.Attribute.Int values -> Some values | _ -> None)

let same_set a b = a == b || Set.equal a b

(* [None] when the geometry cannot be picked (degenerate polygons). *)
let surface geometry = Result.to_option (Rdk.Surface_index.create geometry)

(* The nearest hit as (distance, tag); tags of a geometry without provenance
   are absent, so it never picks. *)
let cast surface geometry ~origin ~direction =
  match surface, tags geometry with
  | Some surface, Some tags ->
      (match Rdk.Surface_index.raycast surface ~origin ~direction with
       | Ok (Some hit) -> Some (hit.distance, tags.(hit.primitive))
       | Ok None | Error _ -> None)
  | _ -> None

(* The nearest hit as (distance, shop_materialpath of that primitive): what Alt-click reads.  It needs
   no provenance tags. *)
let cast_material surface geometry ~origin ~direction =
  match surface with
  | Some surface ->
      (match Rdk.Surface_index.raycast surface ~origin ~direction with
       | Ok (Some hit) ->
           let path = match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "shop_materialpath" geometry with
             | Some a -> (match Rdk.Attribute.storage a with
                 | Rdk.Attribute.Text paths when paths.(hit.primitive) <> "" -> Some paths.(hit.primitive)
                 | _ -> None)
             | None -> None in
           Some (hit.distance, path)
       | Ok None | Error _ -> None)
  | None -> None

(* The primitives whose tag is in [lit] take the selection tint over their own
   colour, the rest are dimmed: a vertex colour per corner, so a prepare that
   reads [Cd] (as [to_mesh] does) draws it without knowing about picking. *)
let dim = 0.3 and mix = 0.6

let tint (output : Sop.Session.output) lit =
  Result.map (fun geometry ->
  match tags geometry with
  | Some tags when not (Set.is_empty lit) ->
      let topology = Rdk.Geometry.topology geometry in
      let corners = Rdk.Topology.vertex_count topology in
      let base = match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "Cd" geometry,
                       Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "Cd" geometry with
        | Some a, _ -> (match Rdk.Attribute.View.storage a with
            | Rdk.Attribute.Float4 c -> Some (`Vertex (Rdk.Packed.Float4.View.view c)) | _ -> None)
        | None, Some a -> (match Rdk.Attribute.View.storage a with
            | Rdk.Attribute.Float4 c -> Some (`Point (Rdk.Packed.Float4.View.view c)) | _ -> None)
        | None, None -> None in
      let r = Array.make corners 1. and g = Array.make corners 1.
      and b = Array.make corners 1. and w = Array.make corners 1. in
      let ar, ag, ab, _ = Rays_math.Color.to_floats Pxui.Theme.default.accent in
      for primitive = 0 to Array.length tags - 1 do
        let first, last = Rdk.Topology.primitive_vertex_range topology primitive in
        let lit = Set.mem tags.(primitive) lit in
        for corner = first to last - 1 do
          let cr, cg, cb, cw = match base with
            | None -> 1., 1., 1., 1.
            | Some (`Vertex c) -> c.x.(corner), c.y.(corner), c.z.(corner), c.w.(corner)
            | Some (`Point c) ->
                let p = Rdk.Topology.point_of_vertex topology corner in
                c.x.(p), c.y.(p), c.z.(p), c.w.(p) in
          let shade own accent =
            if lit then own *. (1. -. mix) +. accent *. mix else own *. dim in
          r.(corner) <- shade cr ar; g.(corner) <- shade cg ag;
          b.(corner) <- shade cb ab; w.(corner) <- cw
        done
      done;
      (match Rdk.Packed.Float4.of_owned ~x:r ~y:g ~z:b ~w with
       | Error _ -> output
       | Ok colors ->
           match Result.bind (Rdk.Attribute.create_key_owned
               (Rdk.Attribute.color ~owner:Rdk.Attribute.Vertex) colors)
               (fun attribute -> Rdk.Geometry.with_attribute attribute geometry) with
           | Ok geometry -> { output with payload=Sop.Payload.Geometry geometry }
           | Error _ -> output)
  | _ -> output) (Sop.Payload.geometry output.payload)
