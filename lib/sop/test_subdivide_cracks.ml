(* subdivide_cracks is the native partial-group (crack closing) path of subdivide: the same
   kernel and parameters, so every cook equals subdivide's bit for bit. *)
open Sop
let ok = Result.get_ok

let cook node =
  let session = ok (Session.create ~max_entries:0 ~max_payload_bytes:0) in
  let output = match Session.cook session ~context:(ok (Context.create ())) node with Ok v -> v | Error e -> failwith (Diagnostic.error_to_string e) in
  Session.close session;
  ok (Payload.geometry output.payload)

let () =
  let box = ok (Rdk.Box_generator.box ~connectivity:Rdk.Box_generator.Box_quads
    ~consolidate_points:true ~normals:Rdk.Box_generator.Box_no_normals
    ~size:(Rays_math.Vec3.create 2. 2. 2.) ()) in
  let count = Rdk.Geometry.primitive_count box in
  let group = Rdk.Group.init ~owner:Rdk.Group.Primitive ~name:"part" count (fun p -> p < 2) in
  let input = Lisp_sop.snapshot (ok (Rdk.Geometry.with_group group box)) in
  let make factory values =
    let node = ok (Edit_graph.instantiate_optional factory [Some input; None]) in
    fst (ok (Node.apply_parameters node values)) in
  List.iter (fun cracks ->
    let values = [ "group", Param.Text_value "part"; "hole_group", Param.Text_value "";
                   "cracks", Param.Choice_value cracks ] in
    let a = cook (make Nodes.Subdivide.factory values)
    and b = cook (make Nodes.Subdivide_cracks.factory values) in
    assert (Rdk.Geometry.point_count a = Rdk.Geometry.point_count b);
    assert (Rdk.Geometry.primitive_count a = Rdk.Geometry.primitive_count b);
    assert (Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions a)
            = Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions b)))
    ["Do not close"; "Pull, no edge division"; "Stitch, divide edges"];
  print_endline "subdivide_cracks: equals subdivide on a partial group for three crack policies"
