type ('f, 'r) t =
  | Int of int
  | Float of float
  | Bool of bool
  | Text of string
  | Vec2 of float * float
  | Vec3 of float * float * float
  | Vec4 of float * float * float * float
  | List of ('f, 'r) t array
  | Int_array of int array
  | Text_array of string array
  | Float_array of float array
  | Vec2_array of float array
  | Vec3_array of float array
  | Vec4_array of float array
  | Record of (string * ('f, 'r) t) list
  | Deferred of Ty.t * int
  | No_geo
  | Struct of string * Ty.t * (string * ('f, 'r) t) list
  | Source of string * Ty.t * (string * ('f, 'r) t) list
  | Fn of 'f
  | Residual of 'r

let raise_diagnostic (d : Diagnostic.t) = raise (Diagnostic.Error d)
let fail code msg = raise (Diagnostic.Error (Diagnostic.error ~code msg))
let failf code fmt = Printf.ksprintf (fail code) fmt

(* ---- hash: bit-exact port of the study's [hash] (iteration.md 2.2) ---- *)

let hash xs = float_of_int (Rays_math.Hash.float_sequence32 xs mod 1_000_000) /. 1_000_000.

(* ---- numbers ---- *)

(* ponytail: [Int] is an OCaml int but int arithmetic runs on doubles and comes back through
   [to_int], so results beyond 2^62 are an E_NONFINITE error where the study would go inexact. *)
let round x = Float.floor (x +. 0.5)

let to_int name x =
  if Float.is_finite x && Float.abs x < 4e18 then int_of_float x
  else failf "E_NONFINITE" "%s produced a nonfinite value." name

let fin name r =
  if Float.is_finite r then r else failf "E_NONFINITE" "%s produced a nonfinite value." name

let num = function
  | Int n -> float_of_int n
  | Float f -> f
  | Bool b -> if b then 1. else 0.
  | _ -> fail "E_TYPE" "Expected a number."

let truthy = function
  | Bool b -> b
  | Int n -> n <> 0
  | Float f -> f <> 0.
  | _ -> fail "E_TYPE" "Expected a bool."

let int_of = function Int n -> n | v -> to_int "int" (round (num v))

(* JS [(+x).toFixed(4)] then [String]: up to 4 decimals, trailing zeros removed;
   an exact tie rounds away from zero, as [toFixed] does (printf would round to even). *)
let fmt4 x =
  if Float.is_nan x then "NaN"
  else if not (Float.is_finite x) then if x > 0. then "Infinity" else "-Infinity"
  else begin
    let ax = Float.abs x in
    let s40 = Printf.sprintf "%.40f" ax in
    let dot = String.index s40 '.' in
    let tail = String.sub s40 (dot + 1) 40 in
    let tie = tail.[4] = '5' && String.for_all (( = ) '0') (String.sub tail 5 35) in
    let s = Printf.sprintf "%.4f" (if tie then ax +. 1e-6 else ax) in
    let n = ref (String.length s) in
    while s.[!n - 1] = '0' do
      decr n
    done;
    if s.[!n - 1] = '.' then decr n;
    let s = String.sub s 0 !n in
    if x < 0. && s <> "0" then "-" ^ s else s
  end

let rec ty_of = function
  | Int _ -> Ty.Int
  | Float _ -> Ty.Float
  | Bool _ -> Ty.Bool
  | Text _ -> Ty.Text
  | Vec2 _ -> Ty.Vec2
  | Vec3 _ -> Ty.Vec3
  | Vec4 _ -> Ty.Vec4
  | List xs -> Ty.List (elem_ty xs)
  | Int_array _ -> Ty.Array Ty.Int
  | Text_array _ -> Ty.Array Ty.Text
  | Float_array _ -> Ty.Array Ty.Float
  | Vec2_array _ -> Ty.Array Ty.Vec2
  | Vec3_array _ -> Ty.Array Ty.Vec3
  | Vec4_array _ -> Ty.Array Ty.Vec4
  | Record fs -> Ty.Record (List.map (fun (n, v) -> (n, ty_of v)) fs)
  | Deferred (ty, _) -> ty
  | No_geo -> Ty.geometry
  | Struct (_, ty, _) | Source (_, ty, _) -> ty
  | Fn _ -> Ty.Fn None
  | Residual _ -> Ty.Any

and elem_ty xs =
  Array.fold_left (fun t x -> match Ty.join t (ty_of x) with Some j -> j | None -> t) Ty.Any xs

(* [need]: convert a value to a wanted static type (a residual is left for its own evaluation) *)
let rec coerce_to want v =
  match (want, v) with
  | Ty.Any, _ | _, (Residual _ | Source _) -> v
  | Ty.Int, Float f -> Int (to_int "int" (round f))
  | Ty.Int, Bool b -> Int (if b then 1 else 0)
  | Ty.Float, Int n -> Float (float_of_int n)
  | Ty.Float, Bool b -> Float (if b then 1. else 0.)
  | Ty.Bool, (Int _ | Float _) -> Bool (truthy v)
  | Ty.Vec3, (Int _ | Float _) ->
      let f = num v in
      Vec3 (f, f, f)
  | Ty.Vec2, (Int _ | Float _) ->
      let f = num v in
      Vec2 (f, f)
  | Ty.Vec4, (Int _ | Float _) ->
      let f = num v in
      Vec4 (f, f, f, f)
  | Ty.List e, List xs -> List (Array.map (coerce_to e) xs)
  | Ty.Array Ty.Float, Int_array xs -> Float_array (Array.map float_of_int xs)
  | Ty.Record wf, Record fs ->
      Record
        (List.map
           (fun (n, x) ->
             match List.assoc_opt n wf with Some w -> (n, coerce_to w x) | None -> (n, x))
           fs)
  | _ -> v

let rec show_with conc v =
  match conc v with
  | Int n -> string_of_int n
  | Float f -> fmt4 f
  | Bool b -> if b then "true" else "false"
  | Text s -> s
  | Vec2 (x, y) -> "[" ^ String.concat " " [ fmt4 x; fmt4 y ] ^ "]"
  | Vec3 (x, y, z) -> "[" ^ String.concat " " [ fmt4 x; fmt4 y; fmt4 z ] ^ "]"
  | Vec4 (x, y, z, w) -> "[" ^ String.concat " " [ fmt4 x; fmt4 y; fmt4 z; fmt4 w ] ^ "]"
  | List xs -> "[" ^ String.concat " " (List.map (show_with conc) (Array.to_list xs)) ^ "]"
  | Int_array xs -> Printf.sprintf "%d × int" (Array.length xs)
  | Text_array xs -> Printf.sprintf "%d × text" (Array.length xs)
  | Float_array xs -> Printf.sprintf "%d × float" (Array.length xs)
  | Vec2_array xs -> Printf.sprintf "%d × vec2" (Array.length xs / 2)
  | Vec3_array xs -> Printf.sprintf "%d × vec3" (Array.length xs / 3)
  | Vec4_array xs -> Printf.sprintf "%d × vec4" (Array.length xs / 4)
  | Record fs ->
      "{" ^ String.concat " " (List.map (fun (k, x) -> ":" ^ k ^ " " ^ show_with conc x) fs) ^ "}"
  | Residual _ -> "?"
  | v -> Ty.to_string (ty_of v)

let rec key_of ~residual = function
  | Int n -> "i" ^ string_of_int n
  | Float f -> Printf.sprintf "f%h" f
  | Bool b -> if b then "T" else "F"
  | Text s -> Printf.sprintf "s%d:%s" (String.length s) s
  | Vec2 (a, b) -> Printf.sprintf "v2:%h,%h" a b
  | Vec3 (a, b, c) -> Printf.sprintf "v%h,%h,%h" a b c
  | Vec4 (a, b, c, d) -> Printf.sprintf "v4:%h,%h,%h,%h" a b c d
  | List xs -> "[" ^ String.concat "," (List.map (key_of ~residual) (Array.to_list xs)) ^ "]"
  | Int_array xs -> "AI" ^ Marshal.to_string xs []
  | Text_array xs -> "AT" ^ Marshal.to_string xs []
  | Float_array xs -> "A" ^ Marshal.to_string xs []
  | Vec2_array xs -> "V2" ^ Marshal.to_string xs []
  | Vec3_array xs -> "V" ^ Marshal.to_string xs []
  | Vec4_array xs -> "V4" ^ Marshal.to_string xs []
  | Record fs ->
      "{" ^ String.concat "," (List.map (fun (n, v) -> n ^ "=" ^ key_of ~residual v) fs) ^ "}"
  | Deferred (Ty.Named "geometry", n) -> "g" ^ string_of_int n
  | Deferred (Ty.Named "image", n) -> "image:" ^ string_of_int n
  | Deferred (ty, n) -> "node:" ^ Ty.to_string ty ^ ":" ^ string_of_int n
  | No_geo -> "G"
  | Struct (n, _, fs) -> "S" ^ n ^ key_of ~residual (Record fs)
  | Source (n, ty, fs) -> "source:" ^ n ^ ":" ^ Ty.to_string ty ^ key_of ~residual (Record fs)
  | Fn _ -> "fn"
  | Residual r -> "r" ^ string_of_int (residual r)

let comps = function
  | Vec3 (x, y, z) -> (x, y, z)
  | v ->
      let s = num v in
      (s, s, s)

let is_vec = function Vec2 _ | Vec3 _ | Vec4 _ -> true | _ -> false

let arith name f a b =
  match (a, b) with
  | (Vec2 _ | Vec4 _), _ | _, (Vec2 _ | Vec4 _) -> (
      let ty = ty_of (if is_vec a then a else b) in
      let apply a b = fin name (f a b) in
      let promote v = if is_vec v then v else coerce_to ty (Float (num v)) in
      match (promote a, promote b) with
      | Vec2 (ax, ay), Vec2 (bx, by) -> Vec2 (apply ax bx, apply ay by)
      | Vec4 (ax, ay, az, aw), Vec4 (bx, by, bz, bw) ->
          Vec4 (apply ax bx, apply ay by, apply az bz, apply aw bw)
      | _ -> fail "E_TYPE" "Vector arithmetic needs matching widths or a number.")
  | _ ->
      if is_vec a || is_vec b then begin
        let ax, ay, az = comps a and bx, by, bz = comps b in
        Vec3 (fin name (f ax bx), fin name (f ay by), fin name (f az bz))
      end
      else begin
        let r = fin name (f (num a) (num b)) in
        match (a, b) with
        | Int _, Int _ when name <> "/" && name <> "pow" -> Int (to_int name (round r))
        | _ -> Float r
      end

let list_arg = function
  | List xs -> xs
  | Deferred (Ty.Named "geometry", _) ->
      fail "E_TYPE"
        "A loop over geometry yields its merged geometry, not a list; give it to sop/merge."
  | _ -> fail "E_TYPE" "Expected a list."

let array_length = function
  | Int_array xs -> Array.length xs
  | Text_array xs -> Array.length xs
  | Float_array xs -> Array.length xs
  | Vec2_array xs when Array.length xs mod 2 = 0 -> Array.length xs / 2
  | Vec3_array xs when Array.length xs mod 3 = 0 -> Array.length xs / 3
  | Vec4_array xs when Array.length xs mod 4 = 0 -> Array.length xs / 4
  | Vec2_array _ -> fail "E_ARRAY_TYPE" "Packed vec2 storage has two coordinates per element."
  | Vec3_array _ -> fail "E_ARRAY_TYPE" "Packed vec3 storage has three coordinates per element."
  | Vec4_array _ -> fail "E_ARRAY_TYPE" "Packed vec4 storage has four coordinates per element."
  | _ -> fail "E_ARRAY_TYPE" "Expected a packed array."

let array_get xs index =
  if index < 0 || index >= array_length xs then
    fail "E_ARRAY_RANGE" "Array index is outside its length.";
  match xs with
  | Int_array xs -> Int xs.(index)
  | Text_array xs -> Text xs.(index)
  | Float_array xs -> Float xs.(index)
  | Vec2_array xs ->
      let i = index * 2 in
      Vec2 (xs.(i), xs.(i + 1))
  | Vec3_array xs ->
      let i = index * 3 in
      Vec3 (xs.(i), xs.(i + 1), xs.(i + 2))
  | Vec4_array xs ->
      let i = index * 4 in
      Vec4 (xs.(i), xs.(i + 1), xs.(i + 2), xs.(i + 3))
  | _ -> assert false

let array_put data width index v =
  let at = width * index in
  match (width, v) with
  | 1, v -> data.(at) <- fin "array" (num v)
  | 2, Vec2 (x, y) ->
      data.(at) <- fin "array" x;
      data.(at + 1) <- fin "array" y
  | 3, Vec3 (x, y, z) ->
      data.(at) <- fin "array" x;
      data.(at + 1) <- fin "array" y;
      data.(at + 2) <- fin "array" z
  | 4, Vec4 (x, y, z, w) ->
      data.(at) <- fin "array" x;
      data.(at + 1) <- fin "array" y;
      data.(at + 2) <- fin "array" z;
      data.(at + 3) <- fin "array" w
  | _, (Int _ | Float _ | Bool _) ->
      let x = fin "array" (num v) in
      for j = 0 to width - 1 do
        data.(at + j) <- x
      done
  | _ -> fail "E_ARRAY_TYPE" "Packed array element has the wrong vector width."

let int_array_put data index value =
  match coerce_to Ty.Int value with
  | Int n -> data.(index) <- n
  | _ -> fail "E_ARRAY_TYPE" "Packed integer arrays contain integer values."

let array_init ty count f =
  if ty = Ty.Text then begin
    if count < 0 || count > Sys.max_array_length then
      fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
    Text_array
      (Array.init count (fun i ->
           match f i with
           | Text s -> s
           | _ -> fail "E_ARRAY_TYPE" "Text arrays contain text values."))
  end
  else if ty = Ty.Int then begin
    if count < 0 || count > Sys.max_array_length then
      fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
    let data = Array.make count 0 in
    for i = 0 to count - 1 do
      int_array_put data i (f i)
    done;
    Int_array data
  end
  else begin
    let width =
      match ty with
      | Ty.Vec2 -> 2
      | Vec3 -> 3
      | Vec4 -> 4
      | Float | Any -> 1
      | _ -> fail "E_ARRAY_TYPE" "Packed arrays contain float, vec2, vec3 or vec4 values."
    in
    if count < 0 || count > Sys.max_floatarray_length / width then
      fail "E_ARRAY_RANGE" "Array length is outside native storage bounds.";
    let data = Array.make (count * width) 0. in
    for i = 0 to count - 1 do
      array_put data width i (f i)
    done;
    match width with
    | 2 -> Vec2_array data
    | 3 -> Vec3_array data
    | 4 -> Vec4_array data
    | _ -> Float_array data
  end

let rec validate = function
  | Float f -> ignore (fin "input" f)
  | Vec2 (x, y) -> List.iter (fun x -> ignore (fin "input" x)) [ x; y ]
  | Vec3 (x, y, z) -> List.iter (fun x -> ignore (fin "input" x)) [ x; y; z ]
  | Vec4 (x, y, z, w) -> List.iter (fun x -> ignore (fin "input" x)) [ x; y; z; w ]
  | (Float_array xs | Vec2_array xs | Vec3_array xs | Vec4_array xs) as v ->
      ignore (array_length v);
      let length = Array.length xs in
      let i = ref 0 in
      while !i < length && Float.is_finite xs.(!i) do
        incr i
      done;
      if !i < length then ignore (fin "array input" xs.(!i))
  | List xs -> Array.iter validate xs
  | Record fs | Struct (_, _, fs) | Source (_, _, fs) -> List.iter (fun (_, v) -> validate v) fs
  | _ -> ()

let with_error ~protected ~map =
  let cancelled (d : Diagnostic.t) = d.code = "cancelled" || d.code = "E_CANCELLED" in
  let payload (d : Diagnostic.t) =
    Record
      [
        ("code", Text d.code);
        ("message", Text d.message);
        ("cause", List (match d.cause with None -> [||] | Some cause -> [| Text cause |]));
      ]
  in
  let inherit_envelope (original : Diagnostic.t) (mapped : Diagnostic.t) =
    {
      mapped with
      hints = original.hints;
      trace = original.trace;
      span = (match mapped.span with Some _ -> mapped.span | None -> original.span);
      position =
        (match mapped.position with Some _ -> mapped.position | None -> original.position);
    }
  in
  let handle (original : Diagnostic.t) =
    if cancelled original then raise_diagnostic original;
    let result =
      try map (payload original) with
      | Diagnostic.Error mapped when cancelled mapped -> raise_diagnostic mapped
      | Diagnostic.Error mapped -> raise_diagnostic (inherit_envelope original mapped)
    in
    let malformed () =
      raise_diagnostic
        {
          original with
          code = "E_DIAGNOSTIC_MAP";
          message = "An error mapper returns code, message and zero or one text causes.";
        }
    in
    match result with
    | Record fields when List.length fields = 3 -> (
        match
          ( List.assoc_opt "code" fields,
            List.assoc_opt "message" fields,
            List.assoc_opt "cause" fields )
        with
        | Some (Text code), Some (Text message), Some (List causes) ->
            let cause =
              match causes with [||] -> None | [| Text cause |] -> Some cause | _ -> malformed ()
            in
            raise_diagnostic { original with code; message; cause }
        | _ -> malformed ())
    | _ -> malformed ()
  in
  try protected () with Diagnostic.Error diagnostic -> handle diagnostic

(* Validate the entire immutable mask before reporting any selected row. *)
let error_row_index ?grain ?(check = fun () -> ()) value =
  let rows =
    match value with
    | Int_array rows -> rows
    | _ -> fail "E_DIAGNOSTIC_ROWS" "Diagnostic rows require an integer 0/1 array."
  in
  Option.iter
    (fun grain ->
      if grain <= 0 then fail "E_DIAGNOSTIC_CONTEXT" "Diagnostic grain must be positive.")
    grain;
  check ();
  let first = ref (-1) in
  for i = 0 to Array.length rows - 1 do
    if i land 4095 = 0 then check ();
    if rows.(i) <> 0 && rows.(i) <> 1 then
      fail "E_DIAGNOSTIC_ROWS" "Diagnostic row values must be 0 or 1.";
    if rows.(i) = 1 && !first < 0 then first := i
  done;
  if !first < 0 then None
  else
    match grain with
    | None -> Some !first
    | Some grain ->
        let last = !first + min (grain - (!first mod grain)) (Array.length rows - !first) in
        let selected = ref !first in
        for i = !first + 1 to last - 1 do
          if i land 4095 = 0 then check ();
          if rows.(i) = 1 then selected := i
        done;
        check ();
        Some !selected

let diagnose_rows ~select ~map value =
  (* ponytail: this host-independent validation has no native cancellation
     token; add an execution check if full-mask validation dominates latency.
     Native selection polls its token, and Session checks entry/completion. *)
  match error_row_index value with
  | None -> No_geo
  | Some first ->
      let index = select value first in
      (match value with
      | Int_array rows when index >= 0 && index < Array.length rows && rows.(index) = 1 -> ()
      | _ -> fail "E_DIAGNOSTIC_CONTEXT" "Host diagnosis returned a non-failing row.");
      with_error
        ~protected:(fun () -> fail "E_DIAGNOSTIC_ROW" "A diagnostic row failed.")
        ~map:(fun _ -> map (Int index))
