type kind = Rect | Textured | Wire | Grid

type clip = { x : float; y : float; width : float; height : float }

type batch = {
  first : int;
  count : int;
  clip : clip option;
  texture : int;
}

type t = { bytes : bytes; count : int; batches : batch array; marks : Marks.gpu_token option;
           mutable shapes_checked : bool }

let instance_bytes = 64
let instances value = value.bytes
let count value = value.count
let batches value = value.batches
let empty = { bytes = Bytes.empty; count = 0; batches = [||]; marks = None; shapes_checked = false }
let marks value = value.marks

let copy value = { value with bytes = Bytes.copy value.bytes; shapes_checked = false }
let of_marks token =
  let count = Marks.gpu_count token in
  { bytes = Bytes.empty; count; marks = Some token;
    batches = [| { first = 0; count; clip = None; texture = 0 } |]; shapes_checked = false }
let marks_identity = Marks.gpu_identity
let marks_count = Marks.gpu_count
let marks_stamp = Marks.gpu_stamp
let valid_shapes value =
  match value.marks with
  | Some token -> value.count = Marks.gpu_count token && Bytes.length value.bytes = 0
  | None -> value.shapes_checked ||
      Bytes.length value.bytes = value.count * instance_bytes &&
      let rec loop i = i = value.count ||
        let base = i * instance_bytes in
        let kind = Bytes.get_int32_le value.bytes (base + 40) in
        let f word = Int32.float_of_bits (Bytes.get_int32_le value.bytes (base + word * 4)) in
        (kind = 0l || kind = 4l || kind = 5l) &&
        Float.is_finite (f 0) && Float.is_finite (f 1) &&
        Float.is_finite (f 2) && Float.is_finite (f 3) &&
        Float.is_finite (f 11) && f 11 >= 0. &&
        Float.is_finite (f 12) && f 12 >= 0. &&
        Float.is_finite (f 13) &&
        (kind = 5l || (f 2 >= f 0 && f 3 >= f 1)) && loop (i + 1)
      in let valid = loop 0 in if valid then value.shapes_checked <- true; valid

let textures value =
  Array.fold_left (fun ids batch ->
    if batch.texture > 0 && not (List.mem batch.texture ids)
    then batch.texture :: ids else ids) [] value.batches
  |> List.rev

let kind_code = function Rect -> 0l | Textured -> 1l | Wire -> 2l | Grid -> 3l

let float value ~instance ~word =
  Int32.float_of_bits
    (Bytes.get_int32_le value.bytes ((instance * instance_bytes) + (word * 4)))

module Builder = struct
  type batch_table = t

  type t = {
    mutable bytes : bytes;
    mutable length : int;
    mutable batches : batch list;
    mutable open_first : int;
    mutable open_texture : int;
    mutable clip : clip option;
    mutable shared : bool;
    (* [shape_append] validates the Render_ir subset as it writes each instance. *)
    mutable render_ir_valid : bool;
  }

  let create ?(capacity = 256) () =
    { bytes = Bytes.create (max 1 capacity * instance_bytes); length = 0;
      batches = []; open_first = 0; open_texture = 0; clip = None; shared = false;
      render_ir_valid = true }

  let close builder =
    if builder.length > builder.open_first then
      builder.batches <- { first = builder.open_first;
        count = builder.length - builder.open_first; clip = builder.clip;
        texture = builder.open_texture }
        :: builder.batches;
    builder.open_first <- builder.length;
    builder.open_texture <- 0

  let reset builder =
    if builder.shared then begin
      builder.bytes <- Bytes.create (Bytes.length builder.bytes);
      builder.shared <- false
    end;
    builder.length <- 0; builder.batches <- []; builder.open_first <- 0;
    builder.open_texture <- 0; builder.clip <- None; builder.render_ir_valid <- true

  let set_clip builder clip =
    if clip <> builder.clip then begin close builder; builder.clip <- clip end

  (* Untextured instances never split a batch; a second texture does. *)
  let use_texture builder texture =
    if builder.open_texture <> 0 && builder.open_texture <> texture then
      close builder;
    builder.open_texture <- texture

  let reserve builder =
    let needed = (builder.length + 1) * instance_bytes in
    if needed > Bytes.length builder.bytes then begin
      let grown = Bytes.create (max needed (2 * Bytes.length builder.bytes)) in
      Bytes.blit builder.bytes 0 grown 0 (builder.length * instance_bytes);
      builder.bytes <- grown; builder.shared <- false
    end;
    let offset = builder.length * instance_bytes in
    Bytes.fill builder.bytes offset instance_bytes '\000';
    builder.length <- builder.length + 1;
    offset

  let[@inline always] set_float bytes offset word value =
    Bytes.set_int32_le bytes (offset + (word * 4)) (Int32.bits_of_float value)
  let[@inline always] set_word bytes offset word value =
    Bytes.set_int32_le bytes (offset + (word * 4)) value

  let[@inline always] quad builder offset x0 y0 x1 y1 =
    set_float builder.bytes offset 0 x0; set_float builder.bytes offset 1 y0;
    set_float builder.bytes offset 2 x1; set_float builder.bytes offset 3 y1

  let rect builder ~x ~y ~width ~height ?(color = 0l) ?(border_color = 0l)
      ?(border = 0.) ?(radius = 0.) ?(anti_alias = false) () =
    builder.render_ir_valid <- false;
    if width > 0. && height > 0. then begin
      let offset = reserve builder in
      quad builder offset x y (x +. width) (y +. height);
      set_word builder.bytes offset 8 color;
      set_word builder.bytes offset 9 border_color;
      set_word builder.bytes offset 10 (kind_code Rect);
      set_float builder.bytes offset 11 radius;
      set_float builder.bytes offset 12 border;
      set_float builder.bytes offset 13
        (if anti_alias || radius > 0. then 1. else 0.)
    end

  let textured builder ~texture ~x ~y ~width ~height ~u0 ~v0 ~u1 ~v1 ~color =
    builder.render_ir_valid <- false;
    if texture <= 0 then invalid_arg "Ui_batch.textured: texture must be positive";
    if width > 0. && height > 0. then begin
      use_texture builder texture;
      let offset = reserve builder in
      quad builder offset x y (x +. width) (y +. height);
      set_float builder.bytes offset 4 u0; set_float builder.bytes offset 5 v0;
      set_float builder.bytes offset 6 u1; set_float builder.bytes offset 7 v1;
      set_word builder.bytes offset 8 color;
      set_word builder.bytes offset 10 (kind_code Textured)
    end

  let wire builder (x0, y0) (x1, y1) (x2, y2) (x3, y3) ~width ~color =
    builder.render_ir_valid <- false;
    let distance ax ay bx by = Float.hypot (bx -. ax) (by -. ay) in
    let length = distance x0 y0 x1 y1 +. distance x1 y1 x2 y2
      +. distance x2 y2 x3 y3 in
    let segments = max 4 (min 48 (int_of_float (Float.ceil (length /. 16.)))) in
    let point t =
      let s = 1. -. t in
      let a = s *. s *. s and b = 3. *. s *. s *. t and c = 3. *. s *. t *. t
      and d = t *. t *. t in
      (a *. x0) +. (b *. x1) +. (c *. x2) +. (d *. x3),
      (a *. y0) +. (b *. y1) +. (c *. y2) +. (d *. y3) in
    (* Skip chords whose padded bounds miss the clip, in screen space. *)
    let visible (ax, ay) (bx, by) = match builder.clip with
      | None -> true
      | Some clip ->
          let pad = width +. 2. in
          Float.min ax bx -. pad <= clip.x +. clip.width
          && Float.max ax bx +. pad >= clip.x
          && Float.min ay by -. pad <= clip.y +. clip.height
          && Float.max ay by +. pad >= clip.y in
    for segment = 0 to segments - 1 do
      let t0 = float_of_int segment /. float_of_int segments
      and t1 = float_of_int (segment + 1) /. float_of_int segments in
      if visible (point t0) (point t1) then begin
      let offset = reserve builder in
      quad builder offset x0 y0 x1 y1;
      set_float builder.bytes offset 4 x2; set_float builder.bytes offset 5 y2;
      set_float builder.bytes offset 6 x3; set_float builder.bytes offset 7 y3;
      set_word builder.bytes offset 8 color;
      set_word builder.bytes offset 10 (kind_code Wire);
      set_float builder.bytes offset 12 width;
      set_float builder.bytes offset 14 t0;
      set_float builder.bytes offset 15 t1
      end
    done

  let grid builder ~x ~y ~width ~height ~origin_x ~origin_y ~spacing ~dot
      ~color =
    builder.render_ir_valid <- false;
    if width > 0. && height > 0. && spacing > 0. then begin
      let offset = reserve builder in
      quad builder offset x y (x +. width) (y +. height);
      set_float builder.bytes offset 4 origin_x;
      set_float builder.bytes offset 5 origin_y;
      set_float builder.bytes offset 6 spacing;
      set_float builder.bytes offset 7 dot;
      set_word builder.bytes offset 8 color;
      set_word builder.bytes offset 10 (kind_code Grid)
    end

  let publish builder =
    close builder;
    let length = builder.length * instance_bytes in
    let bytes = if length = Bytes.length builder.bytes then begin
      builder.shared <- true; builder.bytes
    end else Bytes.sub builder.bytes 0 length in
    { bytes;
      count = builder.length;
      batches = Array.of_list (List.rev builder.batches); marks = None;
      shapes_checked = builder.render_ir_valid }

  let[@inline always] shape_append builder kind x0 y0 x1 y1 fill stroke stroke_width =
    if not (Float.is_finite x0 && Float.abs x0 <= 3.4028234663852886e38 &&
      Float.is_finite y0 && Float.abs y0 <= 3.4028234663852886e38 &&
      Float.is_finite x1 && Float.abs x1 <= 3.4028234663852886e38 &&
      Float.is_finite y1 && Float.abs y1 <= 3.4028234663852886e38 &&
      Float.is_finite stroke_width && Float.abs stroke_width <= 3.4028234663852886e38) || stroke_width < 0. then
      invalid_arg "Ui_batch: invalid coordinates";
    if builder.length >= 1_000_000 then invalid_arg "Ui_batch: instance limit";
    let offset = builder.length * instance_bytes in
    if offset + instance_bytes > Bytes.length builder.bytes then begin
      let grown = Bytes.create (min (1_000_000 * instance_bytes) (2 * Bytes.length builder.bytes)) in
      Bytes.blit builder.bytes 0 grown 0 offset; builder.bytes <- grown; builder.shared <- false
    end;
    Bytes.fill builder.bytes offset instance_bytes '\000';
    set_float builder.bytes offset 0 x0; set_float builder.bytes offset 1 y0;
    set_float builder.bytes offset 2 x1; set_float builder.bytes offset 3 y1;
    set_word builder.bytes offset 8 fill; set_word builder.bytes offset 9 stroke;
    set_word builder.bytes offset 10 kind; set_float builder.bytes offset 12 stroke_width;
    set_float builder.bytes offset 13 1.; builder.length <- builder.length + 1

  let shape_rect builder ~x ~y ~width ~height ~fill ~stroke () =
    if not (Float.is_finite width && Float.is_finite height) || width < 0. || height < 0. then
      invalid_arg "Ui_batch extent";
    shape_append builder 0l x y (x +. width) (y +. height) fill stroke (if stroke = 0l then 0. else 1.)

  let circle builder ~x ~y ~radius ?fill ?stroke ?(stroke_width = 1.) () =
    if not (Float.is_finite radius) || radius < 0. then invalid_arg "Ui_batch radius";
    let fill = Option.value fill ~default:(-1l) and stroke = Option.value stroke ~default:0l in
    shape_append builder 4l (x -. radius) (y -. radius) (x +. radius) (y +. radius)
      fill stroke (if stroke = 0l then 0. else stroke_width)

  let line builder ~x0 ~y0 ~x1 ~y1 ~color ~width =
    shape_append builder 5l x0 y0 x1 y1 color 0l width

  type floats = float array * int
  let circles builder ~count ~position:(p, ps) ~radius:(r, rs) ~fill ~stroke ~stroke_width:(w, ws) =
    for i = 0 to count - 1 do
      let x = p.(ps * i) and y = p.(ps * i + 1) and radius = r.(rs * i) and width = w.(ws * i) in
      if not (Float.is_finite radius) || radius < 0. then invalid_arg "Ui_batch radius";
      shape_append builder 4l (x -. radius) (y -. radius) (x +. radius) (y +. radius)
        fill stroke (if stroke = 0l then 0. else width)
    done
  let rects builder ~count ~position:(p, ps) ~size:(s, ss) ~fill ~stroke =
    for i = 0 to count - 1 do
      let x = p.(ps * i) and y = p.(ps * i + 1) and width = s.(ss * i) and height = s.(ss * i + 1) in
      if not (Float.is_finite width && Float.is_finite height) || width < 0. || height < 0. then
        invalid_arg "Ui_batch extent";
      shape_append builder 0l x y (x +. width) (y +. height) fill stroke (if stroke = 0l then 0. else 1.)
    done
  let lines builder ~count ~from:(a, as_) ~to_:(b, bs) ~color ~width:(w, ws) =
    for i = 0 to count - 1 do
      shape_append builder 5l a.(as_ * i) a.(as_ * i + 1) b.(bs * i) b.(bs * i + 1) color 0l w.(ws * i)
    done
end
