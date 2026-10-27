(* A triangle wound backwards with authored vertex normals facing -Z, as a
   Boolean leaves a reversed shard: the terminal render mesh keeps the
   authored normals instead of deriving them from the winding. *)
let get=function Ok value->value|Error _->failwith"terminal normal fixture"

let geometry () =
  let positions=Rdk.Packed.Float3.Private.of_owned_exn
    ~x:[|-0.5;0.5;0.|]~y:[|-0.5;-0.5;0.5|]~z:[|0.;0.;0.|]in
  let topology=get(Rdk.Topology.polygons_owned~point_count:3
    ~vertex_points:[|0;2;1|]~primitive_offsets:[|0;3|])in
  let values=Rdk.Packed.Float3.Private.of_owned_exn
    ~x:[|0.;0.;0.|]~y:[|0.;0.;0.|]~z:[|-1.;-1.;-1.|]in
  let normal=get(Rdk.Attribute.create_owned~name:"N"~owner:Rdk.Attribute.Vertex
    (Rdk.Attribute.Float3 values))in
  get(Rdk.Geometry.create~positions~topology~attributes:[normal]())

let mesh geometry=Result.map_error Rdk.Error.to_string(Rdk_rays.Rays_mesh.to_mesh geometry)

let snapshot domains = Parallel.run~domains(fun()->
  let view=Rays.Mesh.Private.packed_view(get(mesh(geometry())))in
  let normals=Option.get view.normals in
  if normals.x<>[|0.;0.;0.|]||normals.y<>[|0.;0.;0.|]||normals.z<>[|-1.;-1.;-1.|]then
    failwith"reversed authored N was overwritten";
  if view.indices<>[|0;1;2|]then failwith"terminal mesh reordered its one triangle";
  (* Render vertices follow the primitive's vertex order: points 0, 2, 1. *)
  if view.vertices.x<>[|-0.5;0.;0.5|]||view.vertices.y<>[|-0.5;0.5;-0.5|]then
    failwith"terminal mesh rewound the reversed triangle";
  Array.copy view.indices,Array.copy normals.x,Array.copy normals.y,Array.copy normals.z)

let malformed () =
  let source=geometry()in
  let bad=get(Rdk.Attribute.create_owned~name:"N"~owner:Rdk.Attribute.Vertex
    (Rdk.Attribute.Float2(get(Rdk.Packed.Float2.of_owned~x:[|0.;0.;0.|]
      ~y:[|0.;0.;0.|]))))in
  let source=Rdk.Geometry.without_attribute~owner:Rdk.Attribute.Vertex"N"source in
  let source=get(Rdk.Geometry.with_attribute bad source)in
  match mesh source with Error _->()|Ok _->failwith"malformed terminal N was accepted"

let run () =
  let one=snapshot 1 and four=snapshot 4 in
  if one<>four then failwith"terminal normal domain drift";
  let workers=Array.init 4(fun _->Domain.spawn(fun()->snapshot 4))in
  Array.iter(fun worker->if Domain.join worker<>one then
    failwith"terminal normal worker drift")workers;
  malformed();
  print_endline"terminal reversed Boolean normals survive the render mesh"
