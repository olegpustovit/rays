module B = Ogpu.Backend

let get = function Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e)
let diagnostic = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d)

let run gpu pipelines ?host ?(kind = 0) ?(bad_vertex = false) ?(mixed = false) ~copies ~accept shape
    =
  let corners = Array.length shape and buffers = ref [] in
  let offsets = Array.make (copies + 1) 0 in
  for i = 0 to copies - 1 do
    offsets.(i + 1) <- (offsets.(i) + if mixed && i mod 2 = 0 then 3 else corners)
  done;
  let point_count = offsets.(copies) in
  let xs = Array.make point_count 0.
  and ys = Array.make point_count 0.
  and zs = Array.make point_count 0. in
  for i = 0 to copies - 1 do
    for at = offsets.(i) to offsets.(i + 1) - 1 do
      let x, y, z = shape.(at - offsets.(i)) in
      xs.(at) <- x;
      ys.(at) <- y;
      zs.(at) <- z
    done
  done;
  let vertices = Array.init point_count Fun.id in
  if bad_vertex then vertices.(0) <- point_count;
  let buffer bytes =
    let b =
      get
        (B.create_buffer (Renderer.gpu_device gpu)
           {
             label = Some "resident triangle test";
             size = Int64.of_int (max 4 bytes);
             usage = [ Storage; Index; Copy_src; Copy_dst ];
           })
    in
    buffers := b :: !buffers;
    b
  in
  let upload bytes =
    let b = buffer (Bytes.length bytes) in
    get (B.write_buffer b ~offset:0L bytes);
    b
  in
  let words values =
    let bytes = Bytes.create (Array.length values * 4) in
    Array.iteri (fun i v -> Bytes.set_int32_le bytes (i * 4) (Int32.of_int v)) values;
    upload bytes
  in
  Fun.protect
    ~finally:(fun () -> List.iter (fun b -> get (B.destroy_buffer b)) !buffers)
    (fun () ->
      let bytes = Bytes.create (point_count * 12) in
      for i = 0 to point_count - 1 do
        List.iteri
          (fun k v -> Bytes.set_int32_le bytes (((i * 3) + k) * 4) (Int32.bits_of_float v))
          [ xs.(i); ys.(i); zs.(i) ]
      done;
      let p = upload bytes
      and v = words vertices
      and o = words offsets
      and k = words (Array.make copies kind) in
      let out = buffer (max 0 (3 * (point_count - (2 * copies)) * 4))
      and blocks = buffer ((copies + 255) / 256 * 4)
      and status = buffer 4 in
      let reads = ref 0 in
      let result =
        Flow_gpu.Run.triangle_indices
          ~on_read:(fun bytes -> reads := !reads + bytes)
          (Renderer.gpu_queue gpu) pipelines ~positions:(p, point_count) ~vertices:(v, point_count)
          ~offsets:(o, copies + 1)
          ~kinds:(k, copies) ~indices:out ~blocks ~status
      in
      Option.iter
        (fun host ->
          let before = Flow_gpu.Host.stats host in
          let invoke () =
            Flow_gpu.Host.triangle_indices host ~positions:(p, point_count)
              ~vertices:(v, point_count)
              ~offsets:(o, copies + 1)
              ~kinds:(k, copies)
          in
          (match (invoke (), result) with
          | Ok (buffer, count, release), Ok expected ->
              Fun.protect ~finally:release (fun () ->
                  assert (count = expected);
                  assert (
                    get (B.read_buffer buffer ~offset:0L ~length:(count * 4))
                    = get (B.read_buffer out ~offset:0L ~length:(count * 4))));
              release ();
              let warm = Flow_gpu.Host.stats host in
              let reused, _, release = diagnostic (invoke ()) in
              assert (B.buffer_id reused = B.buffer_id buffer);
              release ();
              assert ((Flow_gpu.Host.stats host).buffer_creations = warm.buffer_creations);
              assert ((Flow_gpu.Host.stats host).pipeline_compilations = warm.pipeline_compilations)
          | Error d, Error expected -> assert (d.code = expected.code)
          | _ -> failwith "host triangle ownership wrapper changed the result");
          let after = Flow_gpu.Host.stats host in
          assert (after.payload_readback_bytes = before.payload_readback_bytes);
          assert (
            after.validation_readback_bytes - before.validation_readback_bytes
            = if accept then 8 else !reads))
        host;
      if accept then begin
        let count = diagnostic result in
        assert (!reads = 4);
        let positions = Result.get_ok (Rdk.Packed.Float3.of_owned ~x:xs ~y:ys ~z:zs) in
        let topology =
          Result.get_ok
            (Rdk.Topology.polygons_owned ~point_count ~vertex_points:vertices
               ~primitive_offsets:offsets)
        in
        let geometry = Result.get_ok (Rdk.Geometry.create ~positions ~topology ()) in
        let expected =
          match Rdk.Triangulate.run { cancel = None; grain = None; primitives = None } geometry with
          | Ok g -> (Rdk.Topology.View.view (Rdk.Geometry.topology g)).vertex_points
          | Error e -> failwith (Rdk.Error.to_string e)
        in
        let actual = get (B.read_buffer out ~offset:0L ~length:(count * 4)) in
        assert (count = Array.length expected);
        Array.iteri
          (fun i word ->
            let got = Int32.to_int (Bytes.get_int32_le actual (i * 4)) in
            if got <> word then
              failwith (Printf.sprintf "quad index %d: device %d vs host %d" i got word))
          expected;
        Option.iter
          (fun host ->
            let normals, release =
              Option.get
                (Flow_gpu.Host.normals host ~positions:p ~count:point_count ~corners:expected)
            in
            Fun.protect ~finally:release (fun () ->
                let bytes = get (B.read_buffer normals ~offset:0L ~length:(point_count * 12)) in
                for i = 0 to point_count - 1 do
                  assert (Int32.float_of_bits (Bytes.get_int32_le bytes (i * 12)) = 0.);
                  assert (Int32.float_of_bits (Bytes.get_int32_le bytes ((i * 12) + 4)) = 0.);
                  assert (Int32.float_of_bits (Bytes.get_int32_le bytes ((i * 12) + 8)) = 1.)
                done))
          host
      end
      else begin
        assert (match result with Error d -> d.code = "E_GPU_DISPLAY_TRIANGLES" | Ok _ -> false);
        assert (!reads = if copies = 0 || corners > 4 then 0 else 4)
      end)

let () =
  let baseline = Ogpu.Impl.live_handles () in
  let gpu =
    match Renderer.acquire_gpu () with Ok g -> g | Error e -> failwith (Ogpu.Error.to_string e)
  in
  Fun.protect
    ~finally:(fun () -> Renderer.release_gpu gpu)
    (fun () ->
      let pipelines =
        Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu)
      in
      Fun.protect
        ~finally:(fun () -> Flow_gpu.Pipelines.close pipelines)
        (fun () ->
          let square = [| (0., 0., 0.); (2., 0., 0.); (2., 2., 0.); (0., 2., 0.) |]
          and concave = [| (0., 0., 0.); (2., 0., 0.); (0.5, 0.5, 0.); (0., 2., 0.) |] in
          let host = Flow_gpu.Host.create ~clock:Unix.gettimeofday gpu in
          Fun.protect
            ~finally:(fun () -> Flow_gpu.Host.close host)
            (fun () ->
              run gpu pipelines ~host ~copies:1 ~accept:true square;
              run gpu pipelines ~host ~copies:1 ~accept:true concave;
              run gpu pipelines ~host ~copies:2 ~accept:true square;
              run gpu pipelines ~host ~copies:1 ~accept:true square;
              run gpu pipelines ~host ~kind:1 ~copies:1 ~accept:false square;
              run gpu pipelines ~host ~bad_vertex:true ~copies:1 ~accept:false square;
              run gpu pipelines ~host ~copies:0 ~accept:false square;
              run gpu pipelines ~host ~copies:1 ~accept:false
                [| (0., 0., 0.); (0., 0., 0.); (2., 2., 0.); (0., 2., 0.) |]);
          List.iter
            (fun shape ->
              for rotation = 0 to 3 do
                let shape = Array.init 4 (fun i -> shape.((i + rotation) mod 4)) in
                List.iter
                  (fun shape ->
                    List.iter
                      (fun project ->
                        run gpu pipelines ~copies:1 ~accept:true (Array.map project shape))
                      [ Fun.id; (fun (x, y, z) -> (y, z, x)); (fun (x, y, z) -> (z, x, y)) ])
                  [ shape; Array.init 4 (fun i -> shape.(3 - i)) ]
              done)
            [ square; concave; [| (0., 0., 0.); (2., 0., 0.); (2., 2., 0.25); (0., 2., 0.) |] ];
          List.iter
            (fun copies -> run gpu pipelines ~copies ~accept:true square)
            [ 255; 256; 257; 1025; 65537 ];
          run gpu pipelines ~copies:257 ~accept:true [| (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) |];
          run gpu pipelines ~mixed:true ~copies:1025 ~accept:true square;
          run gpu pipelines ~copies:0 ~accept:false square;
          run gpu pipelines ~copies:1 ~accept:false
            [| (0., 0., 0.); (0., 0., 0.); (2., 2., 0.); (0., 2., 0.) |];
          run gpu pipelines ~copies:1 ~accept:false (Array.map (fun (x, y, _) -> (x, y, y)) square);
          run gpu pipelines ~copies:1 ~accept:false
            (Array.map (fun (x, y, z) -> (x +. 10000., y, z)) square);
          List.iter
            (fun x ->
              run gpu pipelines ~copies:1 ~accept:false
                [| (x, 0., 0.); (2., 0., 0.); (2., 2., 0.); (0., 2., 0.) |])
            [ nan; infinity ]));
  assert (Ogpu.Impl.live_handles () = baseline);
  print_endline
    "Resident triangle indices: exact host ear order, winding/projection/concavity, partial \
     groups, two-pass reduction and conservative fallback"
