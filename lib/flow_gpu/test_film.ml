(* P8.S2 film accumulation: a one-output kernel whose input is bound to the film buffer and whose output is
   written `~into` the same buffer is a read-modify-write float accumulation, each thread reading and writing
   only its own pixel. A second kernel resolves the film (divide by the sample count) and Image_sink turns it
   into RGBA8, ties to even. No new sink code: the existing bound/into dispatch is the contract. *)
module B = Ogpu.Backend
module G = Flow_gpu
let get = Test_program.ok
let native = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e)
let width = 64 and height = 48
let n = width * height
let source = Printf.sprintf "(map (fn [x] [x 1 2 3]) (array/range %d))" n
let () =
  let accumulate = Test_program.compile ~fusion:false (Printf.sprintf
    "(map (fn [(p : vec4)] (+ p [(* t 0.25) 0.125 0.5 1])) %s)" source)
  and resolve = Test_program.compile ~fusion:false (Printf.sprintf
    "(map (fn [(p : vec4)] [(+ (/ p.x p.w) (* 0. t)) (/ p.y p.w) (/ p.z p.w) 1.]) %s)" source) in
  match Renderer.acquire_gpu () with
  | Error e -> Format.eprintf "%a@." Renderer.pp_error e; exit 2
  | Ok lease -> Fun.protect ~finally:(fun () -> Renderer.release_gpu lease) (fun () ->
      let device = Renderer.gpu_device lease in
      let buffer size = native (B.create_buffer device { label = Some "film"; size = Int64.of_int size; usage = [ Ogpu.Types.Storage ] }) in
      let film = buffer (n * 16) and cell = buffer 24 in
      native (B.write_buffer film ~offset:0L (Bytes.make (n * 16) '\000'));
      let pipelines = G.Pipelines.create ~clock:Unix.gettimeofday device in
      let sink = get (G.Image_sink.create lease) in
      let run program = G.Run.create lease pipelines (get (G.Emit.kernel ~fast:true program)) in
      let accumulator = run accumulate and resolver = run resolve in
      let prepare program time = get (Flow_ir.Packed.Private.prepare ~fast:true program ~live:(Input.at_time time)) in
      let bound i = if i = 0 then Some film else None in
      for frame = 1 to 4 do
        ignore (get (G.Run.dispatch accumulator ~bound ~into:(film, cell) (prepare accumulate (float frame))))
      done;
      let floats = native (B.read_buffer film ~offset:0L ~length:(n * 16)) in
      let word k = Int32.float_of_bits (Bytes.get_int32_le floats (4 * k)) in
      for pixel = 0 to n - 1 do
        assert (word (4 * pixel) = 2.5 && word (4 * pixel + 1) = 0.5 && word (4 * pixel + 2) = 2. && word (4 * pixel + 3) = 4.)
      done;
      (* resolve: 2.5/4 -> 159, 0.5/4 -> 32, 2/4 -> 128 (127.5 rounds to even), alpha 255 *)
      let resolved = get (G.Run.dispatch resolver ~bound (prepare resolve 1.)) in
      let image = get (G.Image_sink.convert sink ~width ~height resolved) in
      let texture = Option.get (G.Image_sink.texture image) in
      let pixels = native (B.read_texture texture ~bytes_per_row:(width * 4)) in
      for pixel = 0 to n - 1 do
        assert (Bytes.get_uint8 pixels (4 * pixel) = 159 && Bytes.get_uint8 pixels (4 * pixel + 1) = 32
                && Bytes.get_uint8 pixels (4 * pixel + 2) = 128 && Bytes.get_uint8 pixels (4 * pixel + 3) = 255)
      done;
      Printf.printf "film: 4 in-place accumulation dispatches over %d pixels and an RGBA8 resolve match exactly\n" n;
      G.Run.close accumulator; G.Run.close resolver; G.Image_sink.close sink; G.Pipelines.close pipelines;
      ignore (B.destroy_buffer film); ignore (B.destroy_buffer cell))
