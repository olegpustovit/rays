open Rays

(* The 23 graphs of the parity test as it stood at 3baf0095, re-authored in Rays Lisp: every
   keyword below is the argument the typed constructor was given there, and an argument it
   left out is the declared default in both. The three inputs only OCaml can build (a torus
   with alternating triangles, a curve with a point signal, a grid with a target attribute)
   and the two bare point clouds enter as snapshots, as they did. Checked when re-authored:
   each graph cooks to the point, primitive and vertex counts and the bit-identical
   positions the typed graph cooked to in a build of 3baf0095. *)

let sop = Lisp_sop.node
let float = Lisp_sop.float

let grid = {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 8 :rows 6 :size 2.0)|}
let box = {|(sop/box :normals "None" :connectivity "Quads" :consolidate_points true
   :size [1.8 1.8 1.8])|}
let bend = {|(sop/bend :up [0.0 1.0 0.0] :direction [0.0 0.0 1.0] :continuous_twist true
   :length 2.0 :bend_angle 0.5)|}
let curve = {|(sop/curve (list [-1.5 -0.4 0.0] [-0.7 0.5 0.0] [0.0 -0.3 0.0] [0.6 0.6 0.0]
   [1.5 -0.2 0.0]))|}
let wire = {|(sop/polywire :use_sides true :use_max_valence false :use_u_range false
   :use_v_range false :cap_group "" :sides 6 :radius 0.04 :caps true)|}
let marker = {|(sop/uv_sphere :radius_x_mode "Auto" :radius_y_mode "Auto" :radius_z_mode "Auto"
   :normals_mode "Auto" :uv_attribute "" :segments 8 :rings 5 :base_radius 0.08)|}
let red = {|(sop/set_color :owner "Point" :color [1.0 0.0 0.0])|}
let blue = {|(sop/set_color :owner "Point" :color [0.0 0.0 1.0])|}

(* The grid turned about x: the four entries a rotation leaves off the identity. *)
let tilted =
  let m = Rays_math.Mat4.rotation_x 0.8 in
  let entry row column = float (Rays_math.Mat4.get m ~row ~column) in
  Printf.sprintf {|(-> %s (sop/transform :mode "Matrix" :m11 %s :m12 %s :m21 %s :m22 %s))|}
    grid (entry 1 1) (entry 1 2) (entry 2 1) (entry 2 2)

let torus () =
  Rdk.Parametric_generators.torus
    ~connectivity:Rdk.Parametric_generators.Torus_alternating_triangles
    ~rows:16 ~columns:12 ~major_radius:1. ~minor_radius:0.3 ()
  |> Result.get_ok |> Lisp_sop.snapshot

let signal_curve () =
  let count = 97 in
  let positions = Rdk.Packed.Float3.Private.of_owned_exn
      ~x:(Array.init count (fun point ->
        -2.2 +. (4.4 *. float_of_int point /. float_of_int (count - 1))))
      ~y:(Array.init count (fun point -> 0.16 *. sin (float_of_int point *. 0.19)))
      ~z:(Array.make count 0.) in
  let topology = Rdk.Topology.create_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id) ~primitive_offsets:[|0;count|]
      ~primitive_kinds:[|Rdk.Topology.Open_polyline|] |> Result.get_ok in
  let signal = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
      ~name:"signal" (Rdk.Attribute.Float (Array.init count (fun point ->
        sin (float_of_int point *. 0.31))))
      |> Result.get_ok in
  Rdk.Geometry.create ~positions ~topology ~attributes:[signal] ()
  |> Result.get_ok |> Lisp_sop.snapshot

let rewire_source () =
  let geometry = Rdk.Plane_generators.grid ~connectivity:Rdk.Plane_generators.Grid_quads
      ~columns:3 ~rows:3 ~size:2. () |> Result.get_ok in
  let targets = Array.init (Rdk.Geometry.point_count geometry)
      (fun point -> if point = 0 then 1 else -1) in
  let target = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
      ~name:"targetpt" (Rdk.Attribute.Int targets) |> Result.get_ok in
  Rdk.Geometry.with_attribute target geometry |> Result.get_ok |> Lisp_sop.snapshot

let points values = Lisp_sop.snapshot (Rdk.Line_geometry.points values)

let nonempty geometry =
  if Rdk.Geometry.point_count geometry = 0
     || Rdk.Geometry.primitive_count geometry = 0 then
    failwith "cooked geometry is empty"

let point_attribute name geometry =
  nonempty geometry;
  if Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point name geometry = None
  then failwith ("missing point attribute " ^ name)

let primitive_attribute name geometry =
  nonempty geometry;
  if Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive name geometry = None
  then failwith ("missing primitive attribute " ^ name)

let primitive_group name geometry =
  nonempty geometry;
  if Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive name geometry = None
  then failwith ("missing primitive group " ^ name)

let rows = [
  "attribute_composite", (fun () -> sop (Printf.sprintf
    {|(let* [target (-> %s %s %s)
             input (-> %s %s)]
        (sop/attribute_composite input target :point_attributes "P Cd" :allow_position true
          :weight1 0.5))|} grid bend red grid blue)),
    point_attribute "Cd";
  "attribute_fade", (fun () -> sop (Printf.sprintf
    {|(-> %s (sop/attribute_fade :fade_in 2.0 :fade_hold 2.0 :fade_out 2.0 :visualize true))|}
    grid)), point_attribute "fade";
  "attribute_laplacian", (fun () -> sop ~with_:["torus", torus ()]
    {|(-> (sop/ext_torus) (sop/attribute_laplacian :output "" :source "P"))|}),
    point_attribute "laplacian";
  "attribute_mirror", (fun () -> sop (Printf.sprintf
    {|(-> %s %s
        (sop/attribute_mirror :owner "Point" :attributes "Cd" :method_ "Plane"
          :origin [0.0 0.0 0.0] :normal [1.0 0.0 0.0] :distance 0.0 :tolerance %s))|}
    grid red (float 1e-10))), point_attribute "Cd";
  "blend_shapes", (fun () -> sop (Printf.sprintf
    {|(let* [rest %s
             bent (-> %s %s)]
        (sop/blend_shapes rest bent :weight1 0.5 :mask_attribute "" :mask_source "Shape"))|}
    grid grid bend)), nonempty;
  "boolean_detect", (fun () -> sop (Printf.sprintf
    {|(let* [source %s
             collision %s]
        (sop/boolean_detect source collision :intersecting_group "crossing"
          :self_intersecting_group ""))|} grid tilted)), primitive_group "crossing";
  "boolean", (fun () -> sop (Printf.sprintf
    {|(let* [solid %s
             cutter (sop/box :normals "None" :connectivity "Triangles" :size [1.2 1.2 1.2]
                      :center [0.5 0.0 0.0])]
        (sop/boolean solid cutter :operation "Subtract B from A"))|} box)), nonempty;
  "circle_from_edges", (fun () -> sop (Printf.sprintf
    {|(-> (sop/curve (list [-1.0 -1.0 0.0] [1.0 -1.0 0.0] [1.0 1.0 0.0] [-1.0 1.0 0.0])
            :closed true)
        (sop/circle_from_edges :use_radius true :radius 1.0)
        %s)|} wire)), nonempty;
  "convex_hull", (fun () -> sop ~with_:["corners", points
      [|(-1.,-1.,-1.);(1.,-1.,-1.);(1.,1.,-1.);(-1.,1.,-1.);
        (-1.,-1.,1.);(1.,-1.,1.);(1.,1.,1.);(-1.,1.,1.)|]]
    {|(-> (sop/ext_corners) (sop/convex_hull))|}), nonempty;
  "edge_equalize", (fun () -> sop (Printf.sprintf
    {|(-> %s (sop/edge_equalize :iterations 12 :output_group "equalized") %s)|} curve wire)),
    nonempty;
  "edge_relax", (fun () -> sop (Printf.sprintf
    {|(let* [source %s
             reference (sop/curve (list [-1.5 -0.4 0.0] [-0.7 0.0 0.0] [0.0 0.1 0.0]
                         [0.6 0.2 0.0] [1.5 -0.2 0.0]))]
        (-> (sop/edge_relax source reference :iterations 12) %s))|} curve wire)), nonempty;
  "edge_transport", (fun () -> sop (Printf.sprintf
    {|(-> %s
        (sop/set_float :owner "Point" :name "distance" :value 0.0)
        (sop/edge_transport :attribute "distance" :operation "Total")
        (sop/peak :direction_attribute "" :mask_attribute "distance" :distance 0.25))|} grid)),
    point_attribute "distance";
  "extract_centroid", (fun () -> sop (Printf.sprintf
    {|(let* [marker %s
             targets (-> %s (sop/extract_centroid :run_over "Primitives"
                              :method_ "Bounding box"))]
        (sop/copy_to_points marker targets))|} marker box)), nonempty;
  "extract_point_curve", (fun () -> sop ~with_:["signal_curve", signal_curve ()]
    (Printf.sprintf
    {|(let* [marker %s
             targets (-> (sop/ext_signal_curve)
                       (sop/extract_point_from_curve :distance_attribute "signal"))]
        (sop/copy_to_points marker targets))|} marker)), nonempty;
  "graph_color", (fun () -> sop (Printf.sprintf
    {|(-> %s (sop/graph_color :connectivity "Primitives by point"))|} grid)),
    primitive_attribute "color";
  "intersection_analysis", (fun () -> sop (Printf.sprintf
    {|(let* [marker %s
             source %s
             collision %s
             targets (sop/intersection_analysis source collision)]
        (sop/copy_to_points marker targets))|} marker grid tilted)), nonempty;
  "measure_curvature", (fun () -> sop ~with_:["torus", torus ()]
    {|(-> (sop/ext_torus) (sop/measure_curvature))|}), point_attribute "curvature";
  "poly_cut", (fun () -> sop ~with_:["signal_curve", signal_curve ()] (Printf.sprintf
    {|(-> (sop/ext_signal_curve)
        (sop/poly_cut :element "Points" :strategy "Remove" :detection "Attribute crossing"
          :attribute "signal" :value 0.0)
        %s)|} wire)), nonempty;
  "procedural", (fun () -> sop (Printf.sprintf {|(-> %s %s %s)|} box bend blue)), nonempty;
  "remesh", (fun () -> sop
    {|(-> (sop/box :normals "None" :connectivity "Triangles" :consolidate_points true)
        (sop/remesh :target_length 0.5 :iterations 1 :project true :output_quality "quality"))|}),
    primitive_attribute "quality";
  "rewire_vertices", (fun () -> sop ~with_:["rewire_source", rewire_source ()]
    {|(-> (sop/ext_rewire_source)
        (sop/rewire_vertices :owner "Point" :target_attribute "targetpt"
          :original_point_attribute "origpt"))|}), (fun geometry ->
        nonempty geometry;
        if Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Vertex
            "origpt" geometry = None then failwith "missing vertex attribute origpt");
  "separate_pieces", (fun () ->
    let piece id x = Printf.sprintf
      {|(-> %s (sop/set_int :owner "Primitive" :name "piece" :value %d)
          (sop/transform :mode "Matrix" :m03 %s :m13 0.0 :m23 0.0))|} box id (float x) in
    sop (Printf.sprintf
      {|(let* [left %s
               right %s]
          (-> (sop/merge left right)
            (sop/separate_pieces :owner "Primitive" :piece_attribute "piece"
              :translation_attribute "separation" :gap 0.2)))|}
      (piece 1 (-0.4)) (piece 2 0.4))), primitive_attribute "separation";
  "triangulate2d", (fun () -> sop ~with_:["scattered", points
      [|(-1.,-1.,0.);(1.,-1.,0.);(1.,1.,0.);(-1.,1.,0.);
        (0.,0.,0.);(0.4,0.2,0.);(-0.5,0.3,0.)|]]
    {|(-> (sop/ext_scattered)
        (sop/triangulate_2d :projection "XY" :triangle_group "triangles"))|}),
    primitive_group "triangles";
]

let cook domains graph =
  let session = Sop.Session.create ~max_entries:8
      ~max_payload_bytes:(64 * 1024 * 1024) |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    let context = Sop.Context.create ~seed:2026L ~frame:5L ~domains ~grain:17 ()
        |> Result.get_ok in
    match Sop.Session.cook session ~context graph with
    | Ok output -> (Result.get_ok (Sop.Payload.geometry output.payload))
    | Error error -> failwith (Diagnostic.error_to_string error))

let scene geometry =
  let mesh = Rdk_rays.Rays_mesh.to_mesh geometry |> Result.get_ok in
  let camera = Camera.perspective ~at:(Rays_math.Vec3.create 3.5 3. 5.)
      ~target:Rays_math.Vec3.zero () in
  Scene.[clear (Rays_math.Color.hex_exn "#020617");
    view3d ~camera (Scene3.create ~samples:1 [
      Scene3.mesh ~cull:Scene3.Cull_none
        ~material:(Material.unlit Rays_math.Color.white) mesh])]

(* One window per domain count: every graph is one frame of the same export. *)
let export directory domains geometries =
  let config = {Sketch.default_config with width=256; height=192;
    domains=Some domains} in
  let scenes = Array.of_list (List.map scene geometries) in
  ignore (Result.get_ok (Sketch.export_state ~config ~directory ~prefix:"parity"
    ~frames:(Array.length scenes) ~init:(fun _ -> 0)
    ~update:(fun _ (frame : Frame.t) -> frame.input.frame - 1)
    ~view:(fun index _ -> scenes.(max 0 (min index (Array.length scenes - 1)))) ()))

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let run () =
  let root = Filename.temp_file "rays-sop-parity-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let one = Filename.concat root "one" and four = Filename.concat root "four" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun directory ->
      if Sys.file_exists directory then begin
        Sys.readdir directory |> Array.iter (fun file ->
          Sys.remove (Filename.concat directory file));
        Unix.rmdir directory
      end) [one; four];
    Unix.rmdir root) (fun () ->
      let cooked = List.map (fun (name, graph, check) ->
        let cook domains =
          try cook domains (graph ()) with
          | Failure message -> failwith (name ^ ": " ^ message) in
        let one_geometry = cook 1 and four_geometry = cook 4 in
        (try check one_geometry; check four_geometry with
         | Failure message -> failwith (name ^ ": " ^ message));
        if Rdk.Geometry.point_count one_geometry
           <> Rdk.Geometry.point_count four_geometry
           || Rdk.Geometry.primitive_count one_geometry
              <> Rdk.Geometry.primitive_count four_geometry then
          failwith (name ^ ": cook cardinality differs by domain count");
        name, one_geometry, four_geometry) rows in
      export one 1 (List.map (fun (_, geometry, _) -> geometry) cooked);
      export four 4 (List.map (fun (_, _, geometry) -> geometry) cooked);
      (* Each frame shows its own graph, so the comparison is per graph. *)
      if read (Filename.concat one "parity-000000.png")
         = read (Filename.concat one "parity-000001.png") then
        failwith "parity export repeated one graph across frames";
      List.iteri (fun index (name, _, _) ->
        let filename = Printf.sprintf "parity-%06d.png" index in
        let one_png = read (Filename.concat one filename)
        and four_png = read (Filename.concat four filename) in
        if not (String.equal one_png four_png) then
          failwith (name ^ ": one/four-domain PNG bytes differ");
        if String.length one_png < 100 then
          failwith (name ^ ": PNG is empty")) cooked);
  Printf.printf "SOP render parity: %d graphs, one/four-domain PNGs equal\n%!"
    (List.length rows)
