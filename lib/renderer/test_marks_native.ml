(* The Marks pipeline family must light exactly the pixels the Ui_batch path (Ui family, 64-byte
   instances) lights for the same marks: rect, circle and line, with and without a stroke, at 63, 64,
   65 and 100,000 instances, at 1x and 2x density. Inputs are multiples of 1/8 so that binary32
   sums such as x + width are exact; see specification/marks.md. Every channel is compared. *)
module B = Ogpu.Backend
module Sb = Scene_command.Ui_batch.Builder

let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let native_get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let width = 64
and height = 40

let eighth n = float n /. 8.

let rgba r g b a =
  Int32.(
    logor
      (shift_left (of_int (r land 255)) 24)
      (logor
         (shift_left (of_int (g land 255)) 16)
         (logor (shift_left (of_int (b land 255)) 8) (of_int (a land 255)))))

let fill_word i = rgba (i * 13) ((i * 29) + 60) ((i * 7) + 120) (40 + (i * 11 mod 200))
let stroke_word i = rgba ((i * 3) + 200) ((i * 5) + 17) (i * 19) (60 + (i * 7 mod 190))

let x i = eighth (i * 37 mod 512) -. 2.
and y i = eighth (i * 53 mod 320) -. 2.

let rect_size i = (eighth (2 + (i * 5 mod 48)), eighth (2 + (i * 3 mod 40)))
let radius i = eighth (2 + (i mod 38))
let circle_width i = 1. +. float (i mod 3)
let line_end i = (x i +. eighth ((i * 11 mod 64) - 32), y i +. eighth ((i * 17 mod 48) - 24))
let line_width i = float (1 + (i mod 4)) /. 2.

type kind = Rect | Circle | Line

(* The attribute arrays of one mark set and its params block. *)
type attributes = {
  position : float array;
  extent : float array;
  fill : bytes;
  stroke : bytes;
  width : float array;
  params : int array;
  rgb : bool;
}

let floats a =
  let b = Bytes.create (4 * Array.length a) in
  Array.iteri (fun i v -> Bytes.set_int32_le b (4 * i) (Int32.bits_of_float v)) a;
  b

let words a =
  let b = Bytes.create (4 * Array.length a) in
  Array.iteri (fun i v -> Bytes.set_int32_le b (4 * i) v) a;
  b

(* [styled]: a stroke (rect, circle) or per-instance colour and width (line). [rgb]: float fill colours. *)
let attributes kind ~styled ~rgb n =
  let position =
    Array.init (3 * n) (fun k -> match k mod 3 with 0 -> x (k / 3) | 1 -> y (k / 3) | _ -> 0.)
  in
  let extent =
    match kind with
    | Rect ->
        Array.init (2 * n) (fun k ->
            let w, h = rect_size (k / 2) in
            if k mod 2 = 0 then w else h)
    | Circle -> Array.init n radius
    | Line ->
        Array.init (3 * n) (fun k ->
            let ex, ey = line_end (k / 3) in
            match k mod 3 with 0 -> ex | 1 -> ey | _ -> 0.)
  in
  let channel word shift =
    float (Int32.to_int (Int32.shift_right_logical word shift) land 255) /. 255.
  in
  let fill =
    if rgb then
      floats (Array.init (3 * n) (fun k -> channel (fill_word (k / 3)) (24 - (8 * (k mod 3)))))
    else words (Array.init n fill_word)
  in
  let stroke_words =
    match (kind, styled) with
    | (Rect | Circle), false -> [| 0l |]
    | _, true -> Array.init n stroke_word
    | Line, false -> [| rgba 90 200 40 220 |]
  in
  let width =
    match (kind, styled) with
    | Rect, _ -> [| 1. |]
    | Circle, false -> [| 1. |]
    | Circle, true -> Array.init n circle_width
    | Line, false -> [| 1.5 |]
    | Line, true -> Array.init n line_width
  in
  let kind_word = match kind with Rect -> 0 | Circle -> 4 | Line -> 5 in
  let stride_of count = if count = 1 then 0 else 1 in
  let params =
    [|
      kind_word;
      3;
      (match kind with Rect -> 2 | Circle -> 1 | Line -> 3);
      (if rgb then 3 else 1);
      stride_of (Array.length stroke_words);
      stride_of (Array.length width);
      (if rgb then 1 else 0);
      0;
    |]
  in
  { position; extent; fill; stroke = words stroke_words; width; params; rgb }

let batch kind a n =
  let b = Sb.create ~capacity:n () in
  let fill_word i = if a.rgb then Int32.logor (fill_word i) 255l else fill_word i in
  let stroke_at i =
    if Bytes.length a.stroke = 4 then Bytes.get_int32_le a.stroke 0
    else Bytes.get_int32_le a.stroke (4 * i)
  in
  for i = 0 to n - 1 do
    match kind with
    | Rect ->
        let w, h = rect_size i in
        Sb.shape_rect b ~x:(x i) ~y:(y i) ~width:w ~height:h ~fill:(fill_word i)
          ~stroke:(stroke_at i) ()
    | Circle ->
        Sb.circle b ~x:(x i) ~y:(y i) ~radius:(radius i) ~fill:(fill_word i) ~stroke:(stroke_at i)
          ~stroke_width:(if Array.length a.width = 1 then a.width.(0) else circle_width i)
          ()
    | Line ->
        let ex, ey = line_end i in
        Sb.line b ~x0:(x i) ~y0:(y i) ~x1:ex ~y1:ey ~color:(stroke_at i)
          ~width:(if Array.length a.width = 1 then a.width.(0) else line_width i)
  done;
  Sb.publish b

let quad_indices n =
  let bytes = Bytes.create (24 * n) in
  for quad = 0 to n - 1 do
    List.iteri
      (fun slot corner ->
        Bytes.set_int32_le bytes (((quad * 6) + slot) * 4) (Int32.of_int ((4 * quad) + corner)))
      [ 0; 1; 2; 0; 2; 3 ]
  done;
  bytes

let counter = ref 0

let marks_draw ?drawn gpu_vertices_owner device a n =
  let upload label bytes =
    let buffer =
      native_get
        (B.create_buffer device
           {
             label = Some label;
             size = Int64.of_int (Bytes.length bytes);
             usage = [ Vertex; Storage; Copy_dst ];
           })
    in
    native_get (B.write_buffer buffer ~offset:0L bytes);
    buffer
  in
  let buffers =
    List.map2 upload
      [ "position"; "extent"; "fill"; "stroke"; "width"; "params" ]
      [
        floats a.position;
        floats a.extent;
        a.fill;
        a.stroke;
        floats a.width;
        words (Array.map Int32.of_int a.params);
      ]
  in
  incr counter;
  let key = Printf.sprintf "marks-native-%d" !counter in
  (* [drawn]: a count record as a fast kernel writes it, {n, 6n, 1, 0, 0, 0}, the draw's from byte 4 *)
  let record =
    Option.map
      (fun drawn ->
        upload "count" (words (Array.map Int32.of_int [| drawn; 6 * drawn; 1; 0; 0; 0 |])))
      drawn
  in
  let buffers = buffers @ Option.to_list record in
  let source =
    native_get
      (Scene_execution.Private.register_gpu_vertices gpu_vertices_owner
         ~extra:(List.drop 1 (List.take 6 buffers))
         ?indirect:(Option.map (fun b -> (b, 4L)) record)
         ~key ~device ~buffer:(List.hd buffers) ~vertex_count:(4 * n)
         ~valid:(fun () -> true)
         ())
  in
  let affine = floats [| 2. /. float width; 0.; -1.; 0.; -2. /. float height; 1. |] in
  let state : Scene_execution.state =
    {
      viewport = (0, 0, width, height);
      scissor = (0, 0, width, height);
      cull = Cull_none;
      depth_compare = Always;
      depth_write = false;
      depth_load = Load;
      depth_clear = 1.;
      transform_uniforms = Some affine;
    }
  in
  let mesh : Scene_execution.mesh =
    {
      key = Gpu_vertices key;
      vertices = Bytes.empty;
      vertex_count = 4 * n;
      indices = quad_indices n;
      index_count = 6 * n;
      primitive = Triangle_list;
    }
  in
  let draw = Renderer.prepared_draw ~family:Marks ~blend:Alpha { mesh; state } in
  ( draw,
    fun () ->
      Scene_execution.Private.unregister_gpu_vertices source;
      List.iter (fun b -> ignore (B.destroy_buffer b)) buffers )

let name = function Rect -> "rect" | Circle -> "circle" | Line -> "line"

let run () =
  let live_handles = Ogpu.Impl.live_handles in
  let baseline = live_handles () in
  List.iter
    (fun density ->
      let execution =
        get
          (Renderer.create_offscreen
             {
               Renderer.logical_width = width;
               logical_height = height;
               drawable_width = width * density;
               drawable_height = height * density;
               title = "marks-native";
               vsync = false;
               high_density = true;
             })
      in
      let gpu = get (Renderer.acquire_gpu ()) in
      Fun.protect
        ~finally:(fun () ->
          Renderer.release_gpu gpu;
          ignore (get (Renderer.destroy execution)))
        (fun () ->
          let clear = (0.125, 0.25, 0.375, 1.) in
          List.iter
            (fun kind ->
              List.iter
                (fun styled ->
                  List.iter
                    (fun rgb ->
                      List.iter
                        (fun n ->
                          let a = attributes kind ~styled ~rgb n in
                          let reference =
                            Test_ui_pipeline.capture ~clear execution (fun submission ->
                                Renderer.Native.lower_scene2 submission ~density
                                  ~resource:(fun _ -> None)
                                  (Result.get_ok
                                     (Scene_command.Render_ir.create
                                        [|
                                          Scene_command.Render_ir.Set_blend Alpha;
                                          Shapes (batch kind a n);
                                        |])))
                          in
                          let draw, release =
                            marks_draw
                              (Renderer.Native.gpu_vertices_owner execution)
                              (Renderer.gpu_device gpu) a n
                          in
                          let marks =
                            Fun.protect ~finally:release (fun () ->
                                Test_ui_pipeline.capture ~clear execution (fun submission ->
                                    Renderer.Native.adopt_draws submission [ draw ]))
                          in
                          let painted = ref 0 in
                          Bytes.iteri
                            (fun i v -> if i mod 4 <> 3 && v <> '\000' then incr painted)
                            reference;
                          if !painted < 50 && n >= 63 then failwith "marks parity scene is empty";
                          if reference <> marks then begin
                            let first = ref (-1) in
                            Bytes.iteri
                              (fun i v -> if !first < 0 && v <> Bytes.get marks i then first := i)
                              reference;
                            failwith
                              (Printf.sprintf
                                 "Marks differs from Ui_batch: %s styled=%b rgb=%b n=%d density=%d \
                                  at pixel (%d,%d) channel %d"
                                 (name kind) styled rgb n density
                                 (!first / 4 mod (width * density))
                                 (!first / 4 / (width * density))
                                 (!first mod 4))
                          end)
                        [ 63; 64; 65; 100_000 ])
                    [ false; true ])
                [ false; true ])
            [ Rect; Circle; Line ];
          (* An indirect set registered for 100 rects whose device record says 63 draws the first 63,
         as the host batch of 63 does. *)
          let a = attributes Rect ~styled:true ~rgb:false 100 in
          let reference =
            Test_ui_pipeline.capture ~clear execution (fun submission ->
                Renderer.Native.lower_scene2 submission ~density
                  ~resource:(fun _ -> None)
                  (Result.get_ok
                     (Scene_command.Render_ir.create
                        [| Scene_command.Render_ir.Set_blend Alpha; Shapes (batch Rect a 63) |])))
          in
          let draw, release =
            marks_draw ~drawn:63
              (Renderer.Native.gpu_vertices_owner execution)
              (Renderer.gpu_device gpu) a 100
          in
          let marks =
            Fun.protect ~finally:release (fun () ->
                Test_ui_pipeline.capture ~clear execution (fun submission ->
                    Renderer.Native.adopt_draws submission [ draw ]))
          in
          if reference <> marks then
            failwith
              (Printf.sprintf "indirect marks differ from the 63-rect batch at density %d" density);
          assert (
            Scene_execution.Private.gpu_vertex_count_for_test
              (Renderer.Native.gpu_vertices_owner execution)
            = 0)))
    [ 1; 2 ];
  if live_handles () <> baseline then failwith "marks native handle delta";
  print_endline
    "Marks family: pixel parity with Ui_batch for rect, circle and line, with and without stroke, \
     63/64/65/100000 instances, 1x/2x, a device-count indirect draw, zero handles"
