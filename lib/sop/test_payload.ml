open Sop

let ok = function Ok value -> value | Error error -> failwith (Diagnostic.error_to_string error)
let session bytes = Result.get_ok (Session.create ~max_entries:1 ~max_payload_bytes:bytes)
let context domains = Result.get_ok (Context.create ~domains ~grain:17 ())

let bytes image =
  Marshal.to_string
    (Image.width image, Image.height image, Image.storage image)
    [ Marshal.No_sharing ]

let () =
  let source = [| 0.1; 0.2; 0.3; 1. |] in
  let image = ok (Image.create ~width:1 ~height:1 ~rgba:source) in
  source.(0) <- 0.9;
  assert (Image.storage image = [| 0.1; 0.2; 0.3; 1. |]);
  let conversion domains =
    let context = context domains in
    let channels =
      [| -1.; 0.; 0.5 /. 255.; 1.5 /. 255.; 2.5 /. 255.; 127.5 /. 255.; 254.5 /. 255.; 2. |]
    in
    let rgba = Array.init (257 * 131 * 4) (fun i -> channels.(i mod Array.length channels)) in
    let node =
      Node.Build.make ~operation:"image/conversion-test" ~version:1 ~parameters:""
        ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
        (fun ~node_id:_ context _ ->
          Result.map
            (fun image -> Node.Build.{ payload = Payload.Image image; instances = None })
            (Image.of_vec4 ~context ~width:257 ~height:131 rgba))
    in
    let session = session 0 in
    Fun.protect
      ~finally:(fun () -> Session.close session)
      (fun () ->
        let output = ok (Session.cook session ~context node) in
        ok (Payload.image output.payload))
  in
  let one = conversion 1 and eight = conversion 8 in
  let converted = Option.get (Image.rgba8 one) in
  assert (converted = Option.get (Image.rgba8 eight));
  let expected = [| 0; 0; 0; 2; 2; 128; 254; 255 |] in
  Bytes.iteri (fun i c -> assert (Char.code c = expected.(i mod 8))) converted;
  assert (Image.payload_bytes one = Bytes.length converted);
  let expanded = Image.storage one in
  assert (expanded.(3) = 2. /. 255. && expanded.(5) = 128. /. 255.);
  expanded.(3) <- 0.;
  assert ((Image.storage one).(3) = 2. /. 255.);
  List.iter
    (fun bad ->
      assert (
        Result.is_error
          (Image.of_vec4 ~context:(context 1) ~width:1 ~height:1 [| bad; 0.; 0.; 1. |])))
    [ nan; infinity; neg_infinity ];
  assert (Result.is_error (Image.of_owned_rgba8 ~width:max_int ~height:2 Bytes.empty));
  assert (Result.is_error (Image.of_owned_rgba8 ~width:1 ~height:1 Bytes.empty));
  let cancel = Context.Cancel.create () in
  Context.Cancel.cancel cancel;
  let cancelled = Context.create ~cancel () |> Result.get_ok in
  (match Image.of_vec4 ~context:cancelled ~width:1 ~height:1 [| 0.; 0.; 0.; 1. |] with
  | Error d -> assert (d.code = "E_CANCELLED")
  | Ok _ -> assert false);
  List.iter
    (fun (width, height, rgba) -> assert (Result.is_error (Image.create ~width ~height ~rgba)))
    [
      (0, 1, [||]);
      (max_int, 2, [||]);
      (1, 1, [| nan; 0.; 0.; 1. |]);
      (1, 1, [| 0.; 0.; 0.; 2. |]);
      (1, 1, [||]);
    ];
  let noise values =
    let node = Edit_graph.instantiate Image_nodes.noise_factory [] |> Result.get_ok in
    Node.apply_parameters node values |> Result.get_ok |> fst
  in
  assert (
    (List.find
       (fun (field : Param.field_view) -> field.name = "width")
       (Node.parameter_fields (noise [ ("width", Param.Int_value 0) ])))
      .current = Param.Int_value 1);
  let node =
    noise
      [
        ("width", Param.Int_value 32);
        ("height", Param.Int_value 24);
        ("frequency", Param.Float_value 0.12);
        ("seed", Param.Int_value 31);
      ]
  in
  let one = session 65536 and eight = session 65536 in
  Fun.protect
    ~finally:(fun () ->
      Session.close one;
      Session.close eight)
    (fun () ->
      let first = ok (Session.cook one ~context:(context 1) node) in
      let parallel = ok (Session.cook eight ~context:(context 8) node) in
      let image = ok (Payload.image first.payload) in
      assert (bytes image = bytes (ok (Payload.image parallel.payload)));
      assert ((Session.stats one).retained_payload_bytes = 32 * 24 * 4 * 8);
      let second = ok (Session.cook one ~context:(context 1) node) in
      assert (second.payload == first.payload);
      assert ((Session.stats one).hits = 1 && (Session.stats one).cooks = 1);
      (* a footer peeks: the cached output itself, nothing cooked, nothing counted; an uncooked
         node has none *)
      let before = Session.stats one in
      (match Session.cached one ~context:(context 1) node with
      | Some peeked -> assert (peeked.payload == first.payload)
      | None -> failwith "a cooked node had no cached output");
      let other = noise [ ("width", Param.Int_value 5); ("height", Param.Int_value 5) ] in
      assert (Session.cached one ~context:(context 1) other = None);
      assert (Session.stats one = before);
      ignore
        (ok
           (Session.cook one ~context:(context 1)
              (noise [ ("width", Param.Int_value 8); ("height", Param.Int_value 4) ])));
      assert ((Session.stats one).retained_entries = 1 && (Session.stats one).evictions = 1);
      (* CLOCK protects the just-hit image once, dropping the newest entry. *)
      assert ((Session.stats one).retained_payload_bytes = Image.payload_bytes image);
      ignore
        (ok
           (Session.cook one ~context:(context 1)
              (noise [ ("width", Param.Int_value 4); ("height", Param.Int_value 4) ])));
      assert ((Session.stats one).retained_payload_bytes = 4 * 4 * 4 * 8);
      match
        Session.cook one ~context:(context 1)
          (Lisp_sop.node ~with_:[ ("node", node) ] {|(sop/transform (sop/ext_node))|})
      with
      | Error error -> assert (error.code = "E_PAYLOAD")
      | Ok _ -> assert false);
  let tiny = session 1 in
  Fun.protect
    ~finally:(fun () -> Session.close tiny)
    (fun () ->
      ignore (ok (Session.cook tiny ~context:(context 1) node));
      assert (
        (Session.stats tiny).retained_entries = 0 && (Session.stats tiny).retained_payload_bytes = 0));
  let image_input =
    Node.Build.make ~operation:"image/instances-test" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ ->
        Ok
          Node.Build.
            { payload = Payload.Image image; instances = Some [| Rays_math.Mat4.identity |] })
  in
  let passthrough =
    Node.Build.make ~operation:"image/pass-test" ~version:1 ~parameters:"" ~cook_mode:Node.Generic
      ~dependencies:Context.Dependencies.static ~inputs:[| image_input |]
      (fun ~node_id:_ _ inputs -> Ok Node.Build.{ payload = inputs.(0); instances = None })
  in
  let passing = session 65536 in
  Fun.protect
    ~finally:(fun () -> Session.close passing)
    (fun () ->
      let output = ok (Session.cook passing ~context:(context 1) passthrough) in
      assert (ok (Payload.image output.payload) == image));
  print_endline
    "payload: immutable image, exact domains, cache/eviction accounting and typed geometry refusal \
     passed"
