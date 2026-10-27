open Sop
module A = Rdk.Attribute
let ok = Result.get_ok
let rgba = [|1.;0.;0.;0.2; 0.;1.;0.;0.4; 0.;0.;1.;0.6; 1.;1.;1.;0.8|]
let image = ok (Image.create ~width:2 ~height:2 ~rgba)
let image_node image = Node.Private.make ~operation:"image/test" ~version:1
  ~parameters:(string_of_int (Image.data_id image)) ~cook_mode:Node.Generator
  ~dependencies:Context.Dependencies.static ~inputs:[||]
  (fun ~node_id:_ _ _ -> Ok Node.Private.{payload=Payload.Image image; instances=None})
let geometry ?(float3 = false) ?(name = "uv") us vs =
  let count = Array.length us in
  let values = if float3 then A.Float3 (ok (Rdk.Packed.Float3.of_owned
    ~x:(Array.copy us) ~y:(Array.copy vs) ~z:(Array.make count 7.)))
    else A.Float2 (ok (Rdk.Packed.Float2.of_owned ~x:(Array.copy us) ~y:(Array.copy vs))) in
  let attribute = ok (A.create_owned ~name ~owner:A.Point values) in
  ok (Rdk.Geometry.with_attribute attribute (Rdk.Line_geometry.points (Array.make count (0.,0.,0.))))
let values geometry =
  let attribute = Option.get (Rdk.Geometry.find_attribute ~owner:A.Point "sample" geometry) in
  Option.get (A.get (A.key ~name:"sample" ~owner:A.Point A.float) attribute)
let channel_name = function
  | Rdk.Attribute_ops.Red -> "r" | Green -> "g" | Blue -> "b" | Alpha -> "a" | Luminance -> "luminance"
let cook ?(channel = Rdk.Attribute_ops.Red) ?(uv = "uv") ?(grain=257) domains source image =
  let session = ok (Session.create ~max_entries:0 ~max_payload_bytes:0) in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    let node = Lisp_sop.node
        ~with_:["src", Lisp_sop.snapshot source; "image/pixels", image_node image]
        (Printf.sprintf
          "(sop/attr_from_image (sop/ext_src) :image (image/ext_pixels) :attribute \"sample\" :channel %S :uv %S)"
          (channel_name channel) uv) in
    let output = ok (Session.cook session ~context:(ok (Context.create ~domains ~grain ())) node) in
    let geometry = ok (Payload.geometry output.payload) in
    assert (Rdk.Topology.data_id (Rdk.Geometry.topology geometry) = Rdk.Topology.data_id (Rdk.Geometry.topology source));
    assert (Rdk.Packed.Float3.data_id (Rdk.Geometry.positions geometry) = Rdk.Packed.Float3.data_id (Rdk.Geometry.positions source));
    geometry)
let close a b = assert (abs_float (a -. b) < 1e-12)
let run () =
  let source = geometry [|0.;1.;0.;1.;0.5;-4.;5.;0.25|] [|0.;0.;1.;1.;0.5;3.;-2.;0.75|] in
  let image_cooks = ref 0 in
  let image_source = Node.Private.make ~operation:"image/external" ~version:1 ~parameters:"pixels"
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
      ~inputs:[|Lisp_sop.snapshot source|] (fun ~node_id:_ _ inputs ->
        assert (Array.length inputs = 1 && Result.is_ok (Payload.geometry inputs.(0)));
        incr image_cooks;
        Ok Node.Private.{payload=Payload.Image image; instances=None}) in
  let node = Lisp_sop.node
      ~with_:["src", Lisp_sop.snapshot source; "image/pixels", image_source]
      "(let* [pixels (image/ext_pixels)] (sop/attr_from_image (sop/ext_src) :image pixels :attribute \"sample\"))" in
  assert (!image_cooks = 0 && List.exists ((==) image_source) (Node.inputs node));
  let session = ok (Session.create ~max_entries:8 ~max_payload_bytes:65536) in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    let output = ok (Session.cook session ~context:(ok (Context.create ())) node) in
    close (values (ok (Payload.geometry output.payload))).(0) 1.);
  assert (!image_cooks = 1);
  List.iter (fun (channel, expected) ->
    let sampled = cook ~channel 1 source image |> values in
    Array.iter2 close sampled expected)
    [Rdk.Attribute_ops.Red, [|1.;0.;0.;1.;0.5;0.;0.;0.375|];
     Green, [|0.;1.;0.;1.;0.5;0.;1.;0.25|];
     Blue, [|0.;0.;1.;1.;0.5;1.;0.;0.75|];
     Alpha, [|0.2;0.4;0.6;0.8;0.5;0.6;0.4;0.55|]];
  let luminance = cook ~channel:Rdk.Attribute_ops.Luminance 1 source image |> values in
  List.iteri (fun i v -> close luminance.(i) v) [0.2126;0.7152;0.0722;1.;0.5];
  let custom = geometry ~float3:true ~name:"texture" [|0.5|] [|0.5|] in
  close (values (cook ~uv:"texture" 1 custom image)).(0) 0.5;
  let one = ok (Image.create ~width:1 ~height:1 ~rgba:[|0.3;0.2;0.1;1.|]) in
  Array.iter (fun v -> close v 0.3) (values (cook 1 source one));
  let vertical = ok (Image.create ~width:1 ~height:2 ~rgba:[|0.;0.;0.;1.; 1.;0.;0.;1.|]) in
  close (values (cook 1 custom vertical ~uv:"texture")).(0) 0.5;
  let count = 100_003 in
  let dense = geometry (Array.init count (fun i -> float (i mod 257) /. 128. -. 0.5))
    (Array.init count (fun i -> float (i mod 131) /. 65. -. 0.5)) in
  let one = values (cook 1 dense image) and eight = values (cook 8 dense image) in
  assert (Marshal.to_string one [Marshal.No_sharing] = Marshal.to_string eight [Marshal.No_sharing]);
  let bytes = Bytes.init 16 (fun i -> Char.chr (int_of_float (rgba.(i) *. 255.))) in
  let packed_image = ok (Image.Private.of_owned_rgba8 ~width:2 ~height:2 bytes) in
  let expanded_image = ok (Image.create ~width:2 ~height:2 ~rgba:(Image.Private.storage packed_image)) in
  List.iter (fun channel ->
    let exact = values (cook ~channel 1 dense expanded_image) in
    assert (values (cook ~channel 1 dense packed_image)=exact);
    assert (values (cook ~channel 8 dense packed_image)=exact))
    [Rdk.Attribute_ops.Red;Green;Blue;Alpha;Luminance];
  assert (Image.payload_bytes packed_image=16);
  assert (Image.Private.storage image = rgba);
  let node = ok (Sop.Edit_graph.instantiate Nodes.Attr_from_image.factory
      [Lisp_sop.snapshot source; image_node image]) in
  let facts = Node.facts node in
  assert (facts.cook_mode = Node.Duplicate_input 0 && facts.elementwise = Node.Points
    && facts.reads = ["uv"] && facts.writes = ["image"] && facts.topology = Node.Preserved && facts.exact);
  let context = ok (Context.create ()) in
  (match Node.Private.cook node context [|Payload.Image image; Payload.Geometry source|] with
   | Error e -> assert (e.code = "E_PAYLOAD") | Ok _ -> assert false);
  let sample ?(width=2) ?(height=2) ?(rgba=rgba) source =
    Rdk.Attribute_ops.from_image ~attribute:"sample" ~channel:Red ~width ~height ~rgba source in
  List.iter (fun result -> assert (Result.is_error result))
    [sample ~width:0 source; sample ~width:max_int source; sample ~rgba:[||] source;
     sample ~rgba:[|nan;0.;0.;1.|] ~width:1 ~height:1 source;
     sample ~rgba:[|2.;0.;0.;1.|] ~width:1 ~height:1 source;
     sample (geometry [|nan|] [|0.|]);
     sample (Rdk.Geometry.without_attribute ~owner:A.Point "uv" source);
     sample (ok (Rdk.Geometry.with_attribute (ok (A.create_owned ~name:"uv" ~owner:A.Point
       (A.Float (Array.make 8 0.)))) source))];
  let cancel = Rdk.Cancel.create () in Rdk.Cancel.cancel cancel;
  (match Rdk.Attribute_ops.from_image ~cancel ~attribute:"sample" ~channel:Red ~width:2 ~height:2 ~rgba source with
   | Error error -> assert (Rdk.Error.code error = "cancelled") | Ok _ -> assert false);
  (* image_texels: one point per texel, row-major, P = (x, y, width), "rgba" holding the decoded floats. *)
  let texels = ok (Sop.Edit_graph.instantiate Nodes.Image_texels.factory [image_node image]) in
  let carrier = ok (Payload.geometry (ok (Session.cook (ok (Session.create ~max_entries:0 ~max_payload_bytes:0))
    ~context:(ok (Context.create ())) texels)).payload) in
  assert (Rdk.Geometry.point_count carrier = 4);
  let planes = Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions carrier) in
  assert (planes.x = [|0.;1.;0.;1.|] && planes.y = [|0.;0.;1.;1.|] && planes.z = [|2.;2.;2.;2.|]);
  (match Rdk.Attribute.Private.storage (Option.get (Rdk.Geometry.find_attribute ~owner:A.Point "rgba" carrier)) with
   | A.Float4 packed -> let view = Rdk.Packed.Float4.Private.view packed in
       assert (view.x = [|1.;0.;0.;1.|] && view.w = [|0.2;0.4;0.6;0.8|])
   | _ -> assert false);
  (* pack_instances: the source unchanged, drawn at the affine rows r0 r1 r2 of the second input. *)
  (let row name x y z w =
     ok (A.create_owned ~name ~owner:A.Point (A.Float4 (ok (Rdk.Packed.Float4.of_owned ~x ~y ~z ~w)))) in
   let carrier = List.fold_left (fun g a -> ok (Rdk.Geometry.with_attribute a g))
       (Rdk.Line_geometry.points [|(0., 0., 0.); (1., 0., 0.)|])
       [row "r0" [|2.;1.|] [|0.;0.|] [|0.;0.|] [|5.;6.|];
        row "r1" [|0.;0.|] [|3.;1.|] [|0.;0.|] [|0.;0.|];
        row "r2" [|0.;0.|] [|0.;0.|] [|1.;1.|] [|0.;1.|]] in
   let packed = Rdk.Line_geometry.points [|(1., 1., 1.)|] in
   let node = ok (Edit_graph.instantiate Nodes.Pack_instances.factory
     [Lisp_sop.snapshot packed; Lisp_sop.snapshot carrier]) in
   let session = ok (Session.create ~max_entries:0 ~max_payload_bytes:0) in
   let output = ok (Session.cook session ~context:(ok (Context.create ())) node) in
   Session.close session;
   assert (Rdk.Geometry.point_count (ok (Payload.geometry output.payload)) = 1);
   match output.instances with
   | Some [|a; b|] ->
       let m x ~row ~column = Rays_math.Mat4.get x ~row ~column in
       assert (m a ~row:0 ~column:0 = 2. && m a ~row:0 ~column:3 = 5. && m a ~row:1 ~column:1 = 3.
         && m b ~row:2 ~column:3 = 1. && m b ~row:3 ~column:3 = 1.)
   | _ -> assert false);
  print_endline "attr_from_image: channels, bilinear/clamp/single-axis UV, exact domains, typed payloads, malformed inputs and cancellation pass"
let benchmark () =
  let count = 1_000_000 in
  let source = geometry (Array.init count (fun i -> float (i mod 257) /. 256.))
    (Array.init count (fun i -> float (i mod 131) /. 130.)) in
  let reference = values (cook ~grain:16384 1 source image) in
  List.iter (fun domains ->
    ignore (cook ~grain:16384 domains source image);
    let allocated = ref 0. in
    let seconds = Array.init 7 (fun _ ->
      let before = Gc.allocated_bytes () and start = Unix.gettimeofday () in
      let output = cook ~grain:16384 domains source image |> values in
      let elapsed = Unix.gettimeofday () -. start in
      allocated := !allocated +. Gc.allocated_bytes () -. before;
      assert (Marshal.to_string reference [Marshal.No_sharing] = Marshal.to_string output [Marshal.No_sharing]);
      elapsed) in
    Array.sort Float.compare seconds;
    Printf.printf "attr_from_image points=%d domains=%d grain=16384 trials=7 median_ms=%.3f caller_alloc_bytes=%.0f exact=true\n%!"
      count domains (seconds.(3) *. 1000.) (!allocated /. 7.)) [1;8]
let () = if Array.exists ((=) "--bench") Sys.argv then benchmark () else run ()
