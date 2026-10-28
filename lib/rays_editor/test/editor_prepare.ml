(* The editor model is concrete over [Cook.prepared] (the cooked geometry and its surface); tests
   that do not look at what was cooked prepare an empty one, the rest prepare as the app does. *)

module Cook = Rays_editor__.Cook

let of_geometry ?instances geometry =
  Result.map
    (fun surface -> { Cook.geometry; surface })
    (Rdk_rays.Surface.of_geometry ?instances geometry)

let empty_prepared =
  let ( let* ) = Result.bind in
  let* positions = Rdk.Packed.Float3.of_owned ~x:[||] ~y:[||] ~z:[||] in
  let* geometry = Rdk.Geometry.create ~positions ~topology:(Rdk.Topology.empty ~point_count:0) () in
  of_geometry geometry

(* the prepare of a test that never reads what was cooked *)
let empty (_ : Rays_editor.Settings.t) (_ : Sop.Session.output) = empty_prepared

(* the cooked geometry, as the app prepares it *)
let surface (_ : Rays_editor.Settings.t) (output : Sop.Session.output) =
  Result.bind
    (Result.map_error Diagnostic.error_to_string (Sop.Payload.geometry output.payload))
    (of_geometry ?instances:output.instances)

(* the x of the centre of a prepared geometry's mesh *)
let centroid geometry =
  Option.bind (Result.to_option (Rdk_rays.Rays_mesh.to_mesh geometry)) Rays.Mesh.centroid

(* the first point of a prepared geometry *)
let first_point (prepared : Cook.prepared) =
  Rdk.Packed.Float3.get (Rdk.Geometry.positions prepared.geometry) 0

(* a prepared that is the single point (x, 0, 0): a test marks what a prepare saw with it *)
let at x =
  let ( let* ) = Result.bind in
  let* positions = Rdk.Packed.Float3.of_owned ~x:[| x |] ~y:[| 0. |] ~z:[| 0. |] in
  let* geometry = Rdk.Geometry.create ~positions ~topology:(Rdk.Topology.empty ~point_count:1) () in
  of_geometry geometry

let x_of prepared =
  let x, _, _ = first_point prepared in
  x

(* the meshes a prepared surface draws *)
let meshes (prepared : Cook.prepared) =
  List.map
    (fun (d : Rays.Scene.D3.Walk.drawing) -> d.mesh)
    (Rays.Scene.D3.Walk.drawings (Rdk_rays.Surface.scene3 prepared.surface))
