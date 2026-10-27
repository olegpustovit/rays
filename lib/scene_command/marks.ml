type buffer = { id : int; count : int; width : int }
type 'a attr = Constant of 'a | Buffer of buffer | Device of buffer
type kind =
  | Rect of { position : (float * float) attr; size : (float * float) attr;
              fill : int32 attr; stroke : int32 attr; stroke_width : float attr }
  | Circle of { position : (float * float) attr; radius : float attr;
                fill : int32 attr; stroke : int32 attr; stroke_width : float attr }
  | Line of { position : (float * float) attr; end_ : (float * float) attr;
              stroke : int32 attr; stroke_width : float attr }
  | Point of { position : (float * float) attr; fill : int32 }
type entry =
  | Mark_set of { count : int; kind : kind }
  | Geometry of { vertices : float array; indices : int array; color : int32 }
  | Image of { resource : int; x : int; y : int; scale : float; angle : float }
  | Text of { x : int; y : int; size : int; color : int32; text : string }
  | Push_transform of { xx : float; xy : float; yx : float; yy : float;
                        tx : float; ty : float }
  | Pop_transform
  | Push_clip of { x : float; y : float; width : float; height : float }
  | Pop_clip
type t = entry array

let max_count = 1_000_000
let max_entries = 1_048_576
let max_f32 = 3.4028234663852886e38

type gpu_token = { identity : int; count : int; stamp : int64 }

module Private = struct
  let gpu_token ~identity ~count ~stamp =
    if identity <= 0 || count < 0 || count > max_count || stamp < 0L then
      invalid_arg "Marks.gpu_token: invalid identity, count or stamp";
    { identity; count; stamp }
  let gpu_identity value = value.identity
  let gpu_count value = value.count
  let gpu_stamp value = value.stamp
end
let finite v = Float.is_finite v
let bounded v = finite v && Float.abs v <= max_f32

exception Bad of string
let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

let check_entry i = function
  | Mark_set { count; kind } ->
    if count < 0 || count > max_count then bad "entry %d: count %d out of range" i count;
    let buf name widths (b : buffer) =
      if b.id <= 0 then bad "entry %d: %s buffer id must be positive" i name;
      if b.count <> count then bad "entry %d: %s buffer count differs from %d" i name count;
      if not (List.mem b.width widths) then bad "entry %d: %s buffer width %d" i name b.width in
    let pair name widths nonneg = function
      | Buffer b | Device b -> buf name widths b
      | Constant (x, y) ->
        if not (bounded x && bounded y) then bad "entry %d: %s not finite" i name;
        if nonneg && (x < 0. || y < 0.) then bad "entry %d: %s negative" i name in
    let scalar name = function
      | Buffer b | Device b -> buf name [1] b
      | Constant v ->
        if not (bounded v) then bad "entry %d: %s not finite" i name;
        if v < 0. then bad "entry %d: %s negative" i name in
    let colour name = function Buffer b | Device b -> buf name [3] b | Constant _ -> () in
    (match kind with
     | Rect r ->
       pair "position" [2; 3] false r.position; pair "size" [2; 3] true r.size;
       colour "fill" r.fill; colour "stroke" r.stroke; scalar "stroke_width" r.stroke_width
     | Circle c ->
       pair "position" [2; 3] false c.position; scalar "radius" c.radius;
       colour "fill" c.fill; colour "stroke" c.stroke; scalar "stroke_width" c.stroke_width
     | Line l ->
       pair "position" [2; 3] false l.position; pair "end" [2; 3] false l.end_;
       colour "stroke" l.stroke; scalar "stroke_width" l.stroke_width
     | Point p -> pair "position" [2; 3] false p.position)
  | Geometry g ->
    let n = Array.length g.vertices in
    if n land 1 <> 0 then bad "entry %d: odd vertex float count" i;
    if Array.length g.indices mod 3 <> 0 then bad "entry %d: index count not a multiple of 3" i;
    Array.iter (fun v -> if not (finite v) then bad "entry %d: nonfinite vertex" i) g.vertices;
    Array.iter (fun x -> if x < 0 || x >= n / 2 then bad "entry %d: index %d out of range" i x)
      g.indices
  | Image m ->
    if m.resource <= 0 then bad "entry %d: image resource must be positive" i;
    if not (finite m.scale && m.scale > 0.) then bad "entry %d: image scale" i;
    if not (finite m.angle) then bad "entry %d: image angle" i
  | Text _ -> ()
  | Push_transform t ->
    if not (List.for_all finite [t.xx; t.xy; t.yx; t.yy; t.tx; t.ty]) then
      bad "entry %d: nonfinite transform" i
  | Push_clip c ->
    if not (List.for_all finite [c.x; c.y; c.width; c.height]) then bad "entry %d: nonfinite clip" i;
    if c.width < 0. || c.height < 0. then bad "entry %d: negative clip extent" i
  | Pop_transform | Pop_clip -> ()

let validate table =
  try
    if Array.length table > max_entries then bad "more than %d entries" max_entries;
    let tr = ref 0 and cl = ref 0 in
    Array.iteri (fun i e ->
      check_entry i e;
      match e with
      | Push_transform _ -> incr tr
      | Push_clip _ -> incr cl
      | Pop_transform -> if !tr = 0 then bad "entry %d: pop_transform without push" i; decr tr
      | Pop_clip -> if !cl = 0 then bad "entry %d: pop_clip without push" i; decr cl
      | _ -> ()) table;
    if !tr <> 0 || !cl <> 0 then bad "unbalanced push at end of table";
    Ok ()
  with Bad s -> Error s

let serialize table =
  let b = Buffer.create 256 in
  let byte x = Buffer.add_uint8 b x in
  let i32 x = Buffer.add_int32_le b x in
  let int x = Buffer.add_int64_le b (Int64.of_int x) in
  let float x = Buffer.add_int64_le b (Int64.bits_of_float x) in
  let attr const = function
    | Constant v -> byte 0; const v
    | Buffer r -> byte 1; int r.id; int r.count; int r.width
    | Device r -> byte 2; int r.id; int r.count; int r.width in
  let pair (x, y) = float x; float y in
  i32 0x524d4b31l;
  int (Array.length table);
  Array.iter (function
    | Mark_set { count; kind } ->
      byte 0;
      (match kind with
       | Rect r ->
         byte 0; int count; attr pair r.position; attr pair r.size;
         attr i32 r.fill; attr i32 r.stroke; attr float r.stroke_width
       | Circle c ->
         byte 1; int count; attr pair c.position; attr float c.radius;
         attr i32 c.fill; attr i32 c.stroke; attr float c.stroke_width
       | Line l ->
         byte 2; int count; attr pair l.position; attr pair l.end_;
         attr i32 l.stroke; attr float l.stroke_width
       | Point p ->
         byte 3; int count; attr pair p.position; attr i32 (Constant p.fill))
    | Geometry g ->
      byte 1; i32 g.color; int (Array.length g.vertices); Array.iter float g.vertices;
      int (Array.length g.indices); Array.iter int g.indices
    | Image m -> byte 2; int m.resource; int m.x; int m.y; float m.scale; float m.angle
    | Text t ->
      byte 3; int t.x; int t.y; int t.size; i32 t.color;
      int (String.length t.text); Buffer.add_string b t.text
    | Push_transform t ->
      byte 4; List.iter float [t.xx; t.xy; t.yx; t.yy; t.tx; t.ty]
    | Pop_transform -> byte 5
    | Push_clip c -> byte 6; List.iter float [c.x; c.y; c.width; c.height]
    | Pop_clip -> byte 7) table;
  Buffer.to_bytes b
