type t = { id : int64; version : int64; ir : Render_ir.t; source_bytes : int }
type segment = t

let next_id = Atomic.make 1L

let rec fresh_id () =
  let value = Atomic.get next_id in
  if value = Int64.max_int then invalid_arg "Display_list.fresh_id: identity space exhausted";
  if Atomic.compare_and_set next_id value (Int64.succ value) then value else fresh_id ()

module Builder = struct
  type published = { id : int64; version : int64; value : t }

  type t = {
    mutable opcodes : bytes;
    mutable values : float array;
    mutable colors : int32 array;
    mutable integers : int array;
    mutable geometries : Render_ir.geometry option array;
    mutable glyph_runs : Render_ir.glyph array option array;
    mutable length : int;
    mutable high_water : int;
    mutable growths : int;
    mutable published : published option;
  }

  let minimum_capacity = 16

  let create () =
    let capacity = minimum_capacity in
    {
      opcodes = Bytes.make capacity '\000';
      values = Array.make (capacity * 8) 0.;
      colors = Array.make capacity Int32.zero;
      integers = Array.make capacity 0;
      geometries = Array.make capacity None;
      glyph_runs = Array.make capacity None;
      length = 0;
      high_water = 0;
      growths = 0;
      published = None;
    }

  let reset value =
    Array.fill value.geometries 0 value.length None;
    Array.fill value.glyph_runs 0 value.length None;
    value.length <- 0;
    value.published <- None

  let grow value capacity =
    let old_capacity = Bytes.length value.opcodes in
    let opcodes = Bytes.make capacity '\000' in
    Bytes.blit value.opcodes 0 opcodes 0 old_capacity;
    value.opcodes <- opcodes;
    let values = Array.make (capacity * 8) 0. in
    Array.blit value.values 0 values 0 (old_capacity * 8);
    value.values <- values;
    let colors = Array.make capacity Int32.zero in
    Array.blit value.colors 0 colors 0 old_capacity;
    value.colors <- colors;
    let integers = Array.make capacity 0 in
    Array.blit value.integers 0 integers 0 old_capacity;
    value.integers <- integers;
    let geometries = Array.make capacity None in
    Array.blit value.geometries 0 geometries 0 old_capacity;
    value.geometries <- geometries;
    let glyph_runs = Array.make capacity None in
    Array.blit value.glyph_runs 0 glyph_runs 0 old_capacity;
    value.glyph_runs <- glyph_runs;
    value.growths <- value.growths + 1

  let reserve value required =
    if required < 0 then invalid_arg "Display_list.reserve: negative capacity";
    if required > 1_048_576 then invalid_arg "Display_list.reserve: capacity exceeds command limit";
    if required > Bytes.length value.opcodes then begin
      let capacity = ref (Bytes.length value.opcodes) in
      while !capacity < required do
        capacity := !capacity * 2
      done;
      grow value !capacity
    end

  let ensure value = reserve value (value.length + 1)

  let append value opcode v0 v1 v2 v3 v4 v5 v6 v7 ~color ~integer ~geometry ~glyphs =
    ensure value;
    let index = value.length and offset = value.length * 8 in
    Bytes.unsafe_set value.opcodes index (Char.chr opcode);
    Array.unsafe_set value.values offset v0;
    Array.unsafe_set value.values (offset + 1) v1;
    Array.unsafe_set value.values (offset + 2) v2;
    Array.unsafe_set value.values (offset + 3) v3;
    Array.unsafe_set value.values (offset + 4) v4;
    Array.unsafe_set value.values (offset + 5) v5;
    Array.unsafe_set value.values (offset + 6) v6;
    Array.unsafe_set value.values (offset + 7) v7;
    Array.unsafe_set value.colors index color;
    Array.unsafe_set value.integers index integer;
    Array.unsafe_set value.geometries index geometry;
    Array.unsafe_set value.glyph_runs index glyphs;
    value.length <- index + 1;
    value.high_water <- max value.high_water value.length;
    value.published <- None

  let finite4 x y width height =
    Float.is_finite x && Float.is_finite y && Float.is_finite width && Float.is_finite height

  let push_clip value ~x ~y ~width ~height =
    if (not (finite4 x y width height)) || width < 0. || height < 0. then
      invalid_arg "Display_list.push_clip: invalid extent";
    append value 1 x y width height 0. 0. 0. 0. ~color:Int32.zero ~integer:0 ~geometry:None
      ~glyphs:None

  let pop_clip value =
    append value 2 0. 0. 0. 0. 0. 0. 0. 0. ~color:Int32.zero ~integer:0 ~geometry:None ~glyphs:None

  let geometry value (geometry : Render_ir.geometry) =
    let geometry =
      {
        geometry with
        vertices = Array.copy geometry.vertices;
        indices = Array.copy geometry.indices;
      }
    in
    append value 8 0. 0. 0. 0. 0. 0. 0. 0. ~color:Int32.zero ~integer:0 ~geometry:(Some geometry)
      ~glyphs:None

  let valid_rect (rect : Render_ir.rect) =
    finite4 rect.x rect.y rect.width rect.height && rect.width >= 0. && rect.height >= 0.

  let image value ~resource_id ~(source : Render_ir.rect) ~(destination : Render_ir.rect) =
    if resource_id <= 0 then invalid_arg "Display_list.image: resource ID must be positive";
    if not (valid_rect source && valid_rect destination) then
      invalid_arg "Display_list.image: invalid rectangle";
    append value 9 source.x source.y source.width source.height destination.x destination.y
      destination.width destination.height ~color:Int32.zero ~integer:resource_id ~geometry:None
      ~glyphs:None

  let command value index =
    let offset = index * 8 in
    let x = Array.unsafe_get value.values offset
    and y = Array.unsafe_get value.values (offset + 1)
    and width = Array.unsafe_get value.values (offset + 2)
    and height = Array.unsafe_get value.values (offset + 3) in
    match Char.code (Bytes.unsafe_get value.opcodes index) with
    | 0 ->
        Render_ir.Geometry
          {
            vertices = [| x; y; x +. width; y; x +. width; y +. height; x; y +. height |];
            indices = [| 0; 1; 2; 0; 2; 3 |];
            color = Array.unsafe_get value.colors index;
          }
    | 1 -> Render_ir.Push_clip { x; y; width; height }
    | 2 -> Render_ir.Pop_clip
    | 4 -> Render_ir.Clear (Array.unsafe_get value.colors index)
    | 5 ->
        Render_ir.Set_blend
          (match Array.unsafe_get value.integers index with
          | 0 -> Source_over
          | 1 -> Copy
          | 2 -> Replace
          | 3 -> Alpha
          | 4 -> Add
          | 5 -> Multiply
          | 6 -> Screen
          | 7 -> Subtract
          | _ -> assert false)
    | 6 ->
        Render_ir.Push_transform
          {
            xx = x;
            xy = y;
            yx = width;
            yy = height;
            tx = Array.unsafe_get value.values (offset + 4);
            ty = Array.unsafe_get value.values (offset + 5);
          }
    | 7 -> Render_ir.Pop_transform
    | 8 -> Render_ir.Geometry (Option.get (Array.unsafe_get value.geometries index))
    | 9 ->
        Render_ir.Image
          {
            resource_id = Array.unsafe_get value.integers index;
            source = { x; y; width; height };
            destination =
              {
                x = Array.unsafe_get value.values (offset + 4);
                y = Array.unsafe_get value.values (offset + 5);
                width = Array.unsafe_get value.values (offset + 6);
                height = Array.unsafe_get value.values (offset + 7);
              };
          }
    | 10 ->
        Render_ir.Glyphs
          {
            resource_id = Array.unsafe_get value.integers index;
            color = Array.unsafe_get value.colors index;
            glyphs = Option.get (Array.unsafe_get value.glyph_runs index);
          }
    | _ -> assert false

  let publish value ~id ~version =
    if id <= 0L then invalid_arg "Display_list.publish: ID must be positive";
    if version < 0L then invalid_arg "Display_list.publish: version must be non-negative";
    match value.published with
    | Some published when published.id = id && published.version = version -> Ok published.value
    | _ -> (
        let commands = Array.init value.length (command value) in
        match Render_ir.create_owned commands with
        | Error error -> Error error
        | Ok ir ->
            let source_bytes =
              Bytes.length value.opcodes
              + (Array.length value.values * (Sys.word_size / 8))
              + (Array.length value.colors * 4)
              + (Array.length value.integers * (Sys.word_size / 8))
              + Array.fold_left
                  (fun total -> function
                    | None -> total
                    | Some geometry ->
                        total
                        + (Array.length geometry.Render_ir.vertices * (Sys.word_size / 8))
                        + (Array.length geometry.indices * (Sys.word_size / 8)))
                  0 value.geometries
              + Array.fold_left
                  (fun total -> function
                    | None -> total | Some glyphs -> total + (Array.length glyphs * 24))
                  0 value.glyph_runs
            in
            let segment = { id; version; ir; source_bytes } in
            value.published <- Some { id; version; value = segment };
            Ok segment)
end

let id value = value.id
let version value = value.version
let source_bytes value = value.source_bytes
let render_ir value = value.ir
