open Rays
module M = Scene_command.Marks
module B = Scene_command.Ui_batch.Builder
module V = Flow.Value

let color (c : int32) =
  let channel shift = Int32.(to_int (logand (shift_right_logical c shift) 0xffl)) in
  Rays_math.Color.rgba (channel 24) (channel 16) (channel 8) (channel 0)

let rgba (c : Rays_math.Color.t) =
  Int32.of_int ((c.r lsl 24) lor (c.g lsl 16) lor (c.b lsl 8) lor c.a)

(* An attribute as a function of the instance index; a buffer is looked up once. *)
let data buffers (b : M.buffer) = Flow_ir.Core_exec.Buffers.find buffers b.id
let no_device () = V.fail "E_GPU" "A device attribute needs a GPU host."

let pairs buffers = function
  | M.Constant p -> fun _ -> p
  | Buffer b ->
      let d = data buffers b in
      fun i -> (d.(b.width * i), d.((b.width * i) + 1))
  | Device _ -> no_device ()

let floats buffers = function
  | M.Constant x -> fun _ -> x
  | Buffer b ->
      let d = data buffers b in
      fun i -> d.(i)
  | Device _ -> no_device ()

let colours buffers = function
  | M.Constant c -> fun _ -> c
  | Buffer b ->
      let d = data buffers b in
      fun i -> rgba (Flow.Op.drawing_color (V.Vec3 (d.(3 * i), d.((3 * i) + 1), d.((3 * i) + 2))))
  | Device _ -> no_device ()

(* The GPU route: attributes in the layout of the [Marks] family (specification/marks.md). *)
type source = Bytes of bytes | Device of Flow_ir.Gpu.value
type attribute = { source : source; stride : int }

type gpu =
  slot:int ->
  kind:int ->
  count:int ->
  attribute array ->
  (Scene_command.Marks.gpu_token, Diagnostic.t) result

(* A host float becomes binary32: a nonfinite or over-large one is refused, as [Ui_batch.Builder] does. *)
let float_bytes n f =
  let b = Bytes.create (4 * n) in
  for i = 0 to n - 1 do
    let v = f i in
    if (not (Float.is_finite v)) || Float.abs v > 3.4028234663852886e38 then
      V.fail "E_DRAW_RANGE" "A drawing coordinate must be finite and fit a 32-bit float.";
    Bytes.set_int32_le b (4 * i) (Int32.bits_of_float v)
  done;
  b

let word_bytes n f =
  let b = Bytes.create (4 * n) in
  for i = 0 to n - 1 do
    Bytes.set_int32_le b (4 * i) (f i)
  done;
  b

let host bytes stride = { source = Bytes bytes; stride }

(* The bytes of a host array are kept while the array is physically the same: a static attribute
   of a device set is converted once, and the unchanged bytes are not uploaded again. Keyed by the
   set's slot and the attribute's place in it. *)
module Kept = Lru.Make (struct
  type t = int * int

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type converted = (float array * bytes) Kept.t

let kept (converted : converted option) key (d : float array) build =
  match converted with
  | None -> build ()
  | Some table -> (
      match Kept.find_opt table key with
      | Some (old, bytes) when old == d -> bytes
      | _ ->
          let bytes = build () in
          Kept.add table key (d, bytes);
          bytes)

(* Floats of [width] per instance: constant pairs and scalars take stride 0. *)
let vectors buffers ~device ~kept count (a : (float * float) M.attr) =
  match a with
  | M.Constant (x, y) -> host (float_bytes 3 (function 0 -> x | 1 -> y | _ -> 0.)) 0
  | Buffer b ->
      let d = data buffers b in
      host (kept d (fun () -> float_bytes (b.width * count) (Array.get d))) b.width
  | Device b -> { source = Device (device b.id); stride = b.width }

let scalars buffers ~device ~kept count (a : float M.attr) =
  match a with
  | M.Constant x -> host (float_bytes 1 (fun _ -> x)) 0
  | Buffer b ->
      let d = data buffers b in
      host (kept d (fun () -> float_bytes count (Array.get d))) 1
  | Device b -> { source = Device (device b.id); stride = 1 }

(* A device colour is float r, g, b (the shader's mode 1); a host one is packed words (mode 0). *)
let words buffers ~device ~kept count (a : int32 M.attr) =
  match a with
  | M.Constant c -> host (word_bytes 1 (fun _ -> c)) 0
  | Buffer b ->
      let c = colours buffers a in
      host (kept (data buffers b) (fun () -> word_bytes count c)) 1
  | Device b -> { source = Device (device b.id); stride = b.width }

(* Rect, circle and line sets with a device attribute: position, extent, fill, stroke, width. *)
let gpu_set buffers ~device ~gpu ~converted ~slot count (kind : M.kind) =
  let at place d build = kept converted (slot, place) d build in
  let vec place = vectors buffers ~device ~kept:(at place) count
  and one place = scalars buffers ~device ~kept:(at place) count
  and word place = words buffers ~device ~kept:(at place) count in
  let kind_word, attributes =
    match kind with
    | Rect { position; size; fill; stroke; _ } ->
        ( 0,
          [|
            vec 0 position;
            vec 1 size;
            word 2 fill;
            word 3 stroke;
            host (float_bytes 1 (fun _ -> 1.)) 0;
          |] )
    | Circle { position; radius; fill; stroke; stroke_width } ->
        (4, [| vec 0 position; one 1 radius; word 2 fill; word 3 stroke; one 4 stroke_width |])
    | Line { position; end_; stroke; stroke_width } ->
        ( 5,
          [|
            vec 0 position;
            vec 1 end_;
            host (word_bytes 1 (fun _ -> 0l)) 0;
            word 3 stroke;
            one 4 stroke_width;
          |] )
    | Point _ -> assert false
  in
  match gpu ~slot ~kind:kind_word ~count attributes with
  | Ok token -> Scene.shapes (Scene_command.Ui_batch.of_marks token)
  | Error (d : Diagnostic.t) -> V.raise_diagnostic d

let has_device (kind : M.kind) =
  let d : type a. a M.attr -> bool = function M.Device _ -> true | _ -> false in
  match kind with
  | Rect { position; size; fill; stroke; _ } -> d position || d size || d fill || d stroke
  | Circle { position; radius; fill; stroke; stroke_width } ->
      d position || d radius || d stroke_width || d fill || d stroke
  | Line { position; end_; stroke; stroke_width } ->
      d position || d end_ || d stroke_width || d stroke
  | Point _ -> false

let int_coordinate x =
  if (not (Float.is_finite x)) || x >= float max_int || x < float min_int then
    V.fail "E_DRAW_RANGE" "A drawing coordinate must fit an integer."
  else int_of_float x

(* An attribute as the builder's packed floats: a constant is its one element with stride 0. *)
let packed2 buffers : (float * float) M.attr -> B.floats = function
  | M.Constant (x, y) -> ([| x; y |], 0)
  | Buffer b -> (data buffers b, b.width)
  | Device _ -> no_device ()

let packed1 buffers : float M.attr -> B.floats = function
  | M.Constant x -> ([| x |], 0)
  | Buffer b -> (data buffers b, 1)
  | Device _ -> no_device ()

(* A set with constant colours is written by the builder from the arrays themselves, with no
   allocation per mark; a per-instance colour goes mark by mark through [drawing_color]. *)
let shapes buffers count (kind : M.kind) =
  let b = B.create ~capacity:count () in
  (match kind with
  | Rect { position; size; fill = Constant fill; stroke = Constant stroke; _ } ->
      B.rects b ~count ~position:(packed2 buffers position) ~size:(packed2 buffers size) ~fill
        ~stroke
  | Circle { position; radius; fill = Constant fill; stroke = Constant stroke; stroke_width } ->
      B.circles b ~count ~position:(packed2 buffers position) ~radius:(packed1 buffers radius) ~fill
        ~stroke ~stroke_width:(packed1 buffers stroke_width)
  | Line { position; end_; stroke = Constant color; stroke_width } ->
      B.lines b ~count ~from:(packed2 buffers position) ~to_:(packed2 buffers end_) ~color
        ~width:(packed1 buffers stroke_width)
  | Rect { position; size; fill; stroke; _ } ->
      let position = pairs buffers position
      and size = pairs buffers size
      and fill = colours buffers fill
      and stroke = colours buffers stroke in
      for i = 0 to count - 1 do
        let x, y = position i and width, height = size i in
        B.shape_rect b ~x ~y ~width ~height ~fill:(fill i) ~stroke:(stroke i) ()
      done
  | Circle { position; radius; fill; stroke; stroke_width } ->
      let position = pairs buffers position
      and radius = floats buffers radius
      and fill = colours buffers fill
      and stroke = colours buffers stroke
      and stroke_width = floats buffers stroke_width in
      for i = 0 to count - 1 do
        let x, y = position i in
        B.circle b ~x ~y ~radius:(radius i) ~fill:(fill i) ~stroke:(stroke i)
          ~stroke_width:(stroke_width i) ()
      done
  | Line { position; end_; stroke; stroke_width } ->
      let position = pairs buffers position
      and end_ = pairs buffers end_
      and color = colours buffers stroke
      and width = floats buffers stroke_width in
      for i = 0 to count - 1 do
        let x0, y0 = position i and x1, y1 = end_ i in
        B.line b ~x0 ~y0 ~x1 ~y1 ~color:(color i) ~width:(width i)
      done
  | Point _ -> assert false);
  Scene.shapes (B.publish b)

(* Point instances retain their logical subpixel positions in Ui_batch. *)
let points buffers count position fill =
  let position = pairs buffers position and ink = color fill in
  let batch = Scene_command.Ui_batch.Builder.create ~capacity:count () in
  for i = 0 to count - 1 do
    let x, y = position i in
    Scene_command.Ui_batch.Builder.shape_rect batch ~x ~y ~width:1. ~height:1. ~fill:(rgba ink)
      ~stroke:0l ()
  done;
  if count = 0 then None else Some (Scene.shapes (Scene_command.Ui_batch.Builder.publish batch))

(* The scene node of a mark set, kept while its description and the physical attribute arrays are the
   same as the previous frame's: a batch whose data did not change is not built again. *)
module Nodes = Lru.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end)

type cache = { nodes : (M.entry * float array list * Scene.node) Nodes.t; converted : converted }

(* a frame's mark sets and device attributes are the working set: more than this and the oldest go *)
let cache () : cache = { nodes = Nodes.create 1024; converted = Kept.create 4096 }
let cache_size cache = Nodes.length cache.nodes

let arrays buffers (kind : M.kind) =
  let one : type a. a M.attr -> float array list = function
    | M.Buffer b -> [ data buffers b ]
    | _ -> []
  in
  match kind with
  | Rect r -> one r.position @ one r.size @ one r.fill @ one r.stroke @ one r.stroke_width
  | Circle c -> one c.position @ one c.radius @ one c.fill @ one c.stroke @ one c.stroke_width
  | Line l -> one l.position @ one l.end_ @ one l.stroke @ one l.stroke_width
  | Point p -> one p.position

let cached cache buffers ordinal (entry : M.entry) kind build =
  match cache with
  | None -> build ()
  | Some { nodes = table; _ } -> (
      let data = arrays buffers kind in
      match Nodes.find_opt table ordinal with
      | Some (previous, old, node)
        when previous = entry && List.length old = List.length data && List.for_all2 ( == ) old data
        ->
          node
      | _ ->
          let node = build () in
          Nodes.add table ordinal (entry, data, node);
          node)

let scene ~buffers ?image ?gpu ?cache ?(device = fun _ -> no_device ()) marks =
  let fail code message = Error (Diagnostic.error ~code message) in
  match M.validate marks with
  | Error message -> fail "E_DRAW_RANGE" message
  | Ok () -> (
      (* Open pushes, innermost first, each with the nodes drawn inside it, newest first. *)
      let stack = ref []
      and top = ref [] in
      let add node = top := node :: !top in
      let open_ close =
        stack := (close, !top) :: !stack;
        top := []
      in
      let close () =
        match !stack with
        | (wrap, outer) :: rest ->
            stack := rest;
            top := wrap (List.rev !top) :: outer
        | [] -> assert false
      in
      let integer x = int_coordinate x in
      let slot = ref 0 and sets = ref 0 in
      let set entry kind build =
        incr sets;
        cached cache buffers !sets entry kind build
      in
      try
        Array.iter
          (function
            | M.Mark_set { count; kind = Point { position; fill } } as entry -> (
                (* A point set with no points has no node; it is never kept. *)
                match
                  set entry
                    (Point { position; fill })
                    (fun () ->
                      match points buffers count position fill with
                      | Some node -> node
                      | None -> raise Exit)
                with
                | node -> add node
                | exception Exit -> ())
            | Mark_set { count; kind } when has_device kind -> (
                match gpu with
                | None -> no_device ()
                | Some gpu ->
                    add
                      (gpu_set buffers ~device ~gpu
                         ~converted:(Option.map (fun c -> c.converted) cache)
                         ~slot:!slot count kind);
                    incr slot)
            | Mark_set { count; kind } as entry ->
                add (set entry kind (fun () -> shapes buffers count kind))
            | Geometry { vertices; indices; color } ->
                add (Scene.geometry { vertices; indices; color })
            | Image { resource; x; y; scale; angle } -> (
                match image with
                | Some resolve -> add (Scene.image (resolve resource) ~at:(x, y) ~scale ~angle ())
                | None ->
                    V.fail "E_IMAGE" "Drawing images need an environment-owned image resolver.")
            | Text { x; y; size; color = c; text } ->
                add (Scene.text ~at:(x, y) ~color:(color c) ~size text)
            | Push_transform { xx; xy; yx; yy; tx; ty } ->
                open_ (Scene.transform { Scene_command.Render_ir.xx; xy; yx; yy; tx; ty })
            | Push_clip { x; y; width; height } ->
                let x = integer x and y = integer y and w = integer width and h = integer height in
                open_ (Scene.clip ~at:(x, y) ~w ~h)
            | Pop_transform | Pop_clip -> close ())
          marks;
        Ok (List.rev !top)
      with
      | Diagnostic.Error d -> Error d
      | Invalid_argument message | Failure message -> fail "E_DRAW_RANGE" message)
