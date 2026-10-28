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
module GV = Rdk_rays.Geometry_view

let tags = GV.tags ~attribute:Flow_sop.Lower.source_attribute
let same_set a b = a == b || Set.equal a b
let surface = GV.surface

(* The nearest hit as (distance, tag); tags of a geometry without provenance
   are absent, so it never picks. *)
let cast = GV.cast ~attribute:Flow_sop.Lower.source_attribute

(* The nearest hit as (distance, shop_materialpath of that primitive): what Alt-click reads.  It needs
   no provenance tags. *)
let cast_material = GV.cast_material

(* The primitives whose tag is in [lit] take the selection tint over their own
   colour, the rest are dimmed: a vertex colour per corner, so a prepare that
   reads [Cd] (as [to_mesh] does) draws it without knowing about picking. *)
let dim = 0.3
and mix = 0.6

let tint (output : Sop.Session.output) lit =
  Result.map
    (fun geometry ->
      if Set.is_empty lit then output
      else
        let accent =
          let r, g, b, _ = Rays_math.Color.to_floats Pxui.Theme.default.accent in
          (r, g, b)
        in
        let tinted =
          GV.tint ~attribute:Flow_sop.Lower.source_attribute ~accent
            ~lit:(fun tag -> Set.mem tag lit)
            geometry
        in
        if tinted == geometry then output else { output with payload = Sop.Payload.Geometry tinted })
    (Sop.Payload.geometry output.payload)
