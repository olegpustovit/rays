type impact = Cook | View | Export

type effects = {
  cook : bool;
  view : bool;
  export : bool;
}

let no_effects = { cook = false; view = false; export = false }

let add_impact impact effects = match impact with
  | Cook -> { effects with cook = true }
  | View -> { effects with view = true }
  | Export -> { effects with export = true }

let union_effects left right = {
  cook = left.cook || right.cook;
  view = left.view || right.view;
  export = left.export || right.export;
}

let has_effects value = value.cook || value.view || value.export

type int_range = {
  soft_min : int;
  soft_max : int;
  hard_min : int option;
  hard_max : int option;
}

type float_range = {
  soft_min : float;
  soft_max : float;
  hard_min : float option;
  hard_max : float option;
}

type 'a choice = {
  options : (string * 'a) array;
  equal : 'a -> 'a -> bool;
}

type 'a encoding = {
  encode : 'a -> string;
  decode : string -> ('a, string) result;
  equal : 'a -> 'a -> bool;
}

type _ kind =
  | Toggle : bool kind
  | Integer : int_range -> int kind
  | Floating : float_range -> float kind
  | Text : string kind
  | Choice : 'a choice -> 'a kind
  | Index_choice : string array -> int kind
  | Encoded : 'a encoding -> 'a kind

type 'record field = Field : {
  name : string;
  label : string;
  description : string option;
  folder : string list;
  impact : impact;
  primary : bool;
  unit : string option;
  vec3 : (string * int) option;
  kind : 'value kind;
  default : 'value;
  get : 'record -> 'value;
  set : 'value -> 'record -> 'record;
} -> 'record field

type 'record schema = {
  name : string;
  default : 'record;
  fields : 'record field list;
  by_name : (string, 'record field) Hashtbl.t;
}

type value =
  | Bool_value of bool
  | Int_value of int
  | Float_value of float
  | Text_value of string
  | Choice_value of string

type kind_view =
  | Toggle_view
  | Integer_view of int_range
  | Floating_view of float_range
  | Text_view
  | Choice_view of string array

type field_view = {
  name : string;
  label : string;
  description : string option;
  folder : string list;
  impact : impact;
  primary : bool;
  unit : string option;
  vec3 : (string * int) option;
  kind : kind_view;
  default : value;
  current : value;
}

let group_vec3 ?(infer = false) fields =
  let names = List.map (fun (field : field_view) -> field.name) fields in
  let floating (field : field_view) = match field.kind with Floating_view _ -> true | _ -> false in
  let rec inferred = function
    | (x : field_view) :: y :: z :: rest when floating x && floating y && floating z
        && x.vec3 = None && y.vec3 = None && z.vec3 = None
        && x.folder = y.folder && x.folder = z.folder ->
        let group = List.find_map (fun (sx, sy, sz) ->
          if not (String.ends_with ~suffix:sx x.name) then None else
          let stem = String.sub x.name 0 (String.length x.name - 2) in
          if y.name = stem ^ sy && z.name = stem ^ sz
          then Some (if List.mem stem names then stem ^ "_color" else stem) else None)
          ["_x", "_y", "_z"; "_r", "_g", "_b"] in
        (match group with
         | Some group -> {x with vec3 = Some (group, 0)} :: {y with vec3 = Some (group, 1)}
             :: {z with vec3 = Some (group, 2)} :: inferred rest
         | None -> x :: inferred (y :: z :: rest))
    | field :: rest -> field :: inferred rest
    | [] -> [] in
  let fields = if infer then inferred fields else fields in
  let rec group seen result = function
    | [] -> Ok (List.rev result)
    | (field : field_view) :: rest ->
        match field.vec3 with
        | None -> group seen ([field] :: result) rest
        | Some (name, 0) when not (List.mem name names || List.mem name seen) ->
            (match rest with
             | y :: z :: rest when y.vec3 = Some (name, 1) && z.vec3 = Some (name, 2)
                 && field.folder = y.folder && field.folder = z.folder
                 && List.for_all (fun (field : field_view) ->
                      match field.kind with Floating_view _ -> true | _ -> false) [field; y; z] ->
                 group (name :: seen) ([field; y; z] :: result) rest
             | _ -> Error ("Invalid vec3 group " ^ name))
        | Some (name, _) -> Error ("Invalid or overlapping vec3 group " ^ name) in
  group [] [] fields

let nonblank what value =
  if String.trim value = "" then invalid_arg (what ^ " must not be blank")

let validate_int_range (range : int_range) =
  if range.soft_max <= range.soft_min then
    invalid_arg "Param.integer: soft max must be greater than soft min";
  (match range.hard_min, range.hard_max with
   | Some low, Some high when high < low ->
       invalid_arg "Param.integer: hard max must be at least hard min"
   | _ -> ());
  Option.iter (fun low ->
    if range.soft_min < low then invalid_arg
        "Param.integer: soft min must not be below hard min") range.hard_min;
  Option.iter (fun high ->
    if range.soft_max > high then invalid_arg
        "Param.integer: soft max must not exceed hard max") range.hard_max

let validate_float_range (range : float_range) =
  if not (Float.is_finite range.soft_min && Float.is_finite range.soft_max)
     || range.soft_max <= range.soft_min
  then invalid_arg
      "Param.floating: soft bounds must be finite and increasing";
  Option.iter (fun value -> if not (Float.is_finite value) then invalid_arg
      "Param.floating: hard bounds must be finite") range.hard_min;
  Option.iter (fun value -> if not (Float.is_finite value) then invalid_arg
      "Param.floating: hard bounds must be finite") range.hard_max;
  (match range.hard_min, range.hard_max with
   | Some low, Some high when high < low ->
       invalid_arg "Param.floating: hard max must be at least hard min"
   | _ -> ());
  Option.iter (fun low ->
    if range.soft_min < low then invalid_arg
        "Param.floating: soft min must not be below hard min") range.hard_min;
  Option.iter (fun high ->
    if range.soft_max > high then invalid_arg
        "Param.floating: soft max must not exceed hard max") range.hard_max

let integer ?hard_min ?hard_max ~min:soft_min ~max:soft_max () =
  let range : int_range = { soft_min; soft_max; hard_min; hard_max } in
  validate_int_range range;
  Integer range

let floating ?hard_min ?hard_max ~min:soft_min ~max:soft_max () =
  let range : float_range = { soft_min; soft_max; hard_min; hard_max } in
  validate_float_range range;
  Floating range

let choice ~equal values =
  if values = [] then invalid_arg "Param.choice: options must not be empty";
  let options = Array.of_list values in
  Array.iteri (fun index (label, _) ->
    nonblank "Param.choice label" label;
    for previous = 0 to index - 1 do
      if fst options.(previous) = label then invalid_arg
          (Printf.sprintf "Param.choice: duplicate label %S" label)
    done) options;
  Choice { options; equal }

let index_choice labels =
  match choice ~equal:Int.equal (List.mapi (fun index label -> label, index) labels) with
  | Choice choice -> Index_choice (Array.map fst choice.options)
  | _ -> assert false

let encoded ~equal ~encode ~decode = Encoded { encode; decode; equal }

let clamp_int (range : int_range) value =
  let value = match range.hard_min with
    | Some low -> max low value | None -> value in
  match range.hard_max with Some high -> min high value | None -> value

let clamp_float (range : float_range) value =
  if not (Float.is_finite value) then Error "float parameter must be finite"
  else
    let value = match range.hard_min with
      | Some low -> Float.max low value | None -> value in
    Ok (match range.hard_max with
      | Some high -> Float.min high value | None -> value)

let normalize_value : type value. value kind -> value -> (value, string) result =
  fun kind value -> match kind with
  | Toggle | Text | Encoded _ -> Ok value
  | Integer range -> Ok (clamp_int range value)
  | Index_choice labels ->
      if value >= 0 && value < Array.length labels then Ok value
      else Error "choice parameter index is out of bounds"
  | Floating range -> clamp_float range value
  | Choice choice ->
      if Array.exists (fun (_, option) -> choice.equal option value)
          choice.options
      then Ok value else Error "choice parameter value is not an option"

let equal_kind : type value. value kind -> value -> value -> bool =
  fun kind left right -> match kind with
  | Toggle | Integer _ | Index_choice _ | Floating _ | Text -> left = right
  | Choice choice -> choice.equal left right
  | Encoded encoding -> encoding.equal left right

let normalize_field record (Field field) =
  let original = field.get record in
  let unchanged : type value. value kind -> value -> value -> bool =
    fun kind original value -> match kind with
      | Floating _ -> original = value && (value <> 0.
          || Int64.bits_of_float original = Int64.bits_of_float value)
      | _ -> equal_kind kind original value in
  Result.map (fun value ->
    if unchanged field.kind original value then record else field.set value record)
    (normalize_value field.kind original)

let default_label name =
  let value = Bytes.of_string name in
  Bytes.iteri (fun index character ->
    if character = '_' || character = '-' then Bytes.set value index ' ')
    value;
  let value = Bytes.to_string value in
  if value = "" then value
  else String.mapi (fun index character ->
    if index = 0 then Char.uppercase_ascii character else character) value

let validate_vec3 : type a. a kind -> (string * int) option -> unit = fun kind ->
  Option.iter (fun (group, component) ->
    nonblank "Param.field vec3 group" group;
    if component < 0 || component > 2 then invalid_arg "Param.field: vec3 component must be 0, 1 or 2";
    match kind with Floating _ -> ()
      | _ -> invalid_arg "Param.field: vec3 components must be float fields")

let field ~name ?label ?description ?(folder = []) ?(impact = Cook)
    ?(primary = false) ?unit ?vec3
    ~kind ~default ~get ~set () =
  nonblank "Param.field name" name;
  List.iter (nonblank "Param.field folder") folder;
  let label = Option.value ~default:(default_label name) label in
  nonblank "Param.field label" label;
  validate_vec3 kind vec3;
  let default = match normalize_value kind default with
    | Ok value -> value
    | Error message -> invalid_arg ("Param.field " ^ name ^ ": " ^ message)
  in
  Field { name; label; description; folder; impact; primary; unit; vec3; kind; default; get; set }

let schema ~name ~default fields =
  nonblank "Param.schema name" name;
  let seen = Hashtbl.create (List.length fields) in
  List.iter (fun (Field field) ->
    if Hashtbl.mem seen field.name then invalid_arg
        (Printf.sprintf "Param.schema: duplicate field %S" field.name);
    Hashtbl.add seen field.name ()) fields;
  let by_name = Hashtbl.create (List.length fields) in
  List.iter (fun ((Field field) as packed) ->
    Hashtbl.add by_name field.name packed) fields;
  let schema = { name; default; fields; by_name } in
  let default = match
      List.fold_left (fun result field ->
        Result.bind result (fun record ->
          normalize_field record field)) (Ok default) fields
    with
    | Ok value -> value
    | Error message -> invalid_arg ("Param.schema " ^ name ^ ": " ^ message)
  in
  { schema with default }

let default (value : 'record schema) = value.default
let fields (value : 'record schema) = value.fields

let choice_label (choice : 'a choice) value =
  match Array.find_opt (fun (_, candidate) -> choice.equal candidate value)
      choice.options with
  | Some (label, _) -> label
  | None -> invalid_arg "Param.view: choice value is not an option"

let view_field : type record. record -> record field -> field_view =
  fun record (Field field) ->
    let kind, default, current = match field.kind with
      | Toggle -> Toggle_view, Bool_value field.default,
          Bool_value (field.get record)
      | Integer range -> Integer_view range, Int_value field.default,
          Int_value (field.get record)
      | Floating range -> Floating_view range, Float_value field.default,
          Float_value (field.get record)
      | Text -> Text_view, Text_value field.default,
          Text_value (field.get record)
      | Index_choice labels -> Choice_view labels, Int_value field.default,
          Int_value (field.get record)
      | Choice choice ->
          Choice_view (Array.map fst choice.options),
          Choice_value (choice_label choice field.default),
          Choice_value (choice_label choice (field.get record))
      | Encoded encoding -> Text_view,
          Text_value (encoding.encode field.default),
          Text_value (encoding.encode (field.get record))
    in
    { name = field.name; label = field.label;
      description = field.description; folder = field.folder;
      impact = field.impact; primary = field.primary; unit = field.unit; vec3 = field.vec3; kind; default; current }

let view schema record = List.map (view_field record) schema.fields

let float_text ?(decimal = false) value =
  if Float.is_nan value then "NaN"
  else if not (Float.is_finite value) then if value > 0. then "Infinity" else "-Infinity"
  else
    let bits = Int64.bits_of_float value in
    let same text = Int64.bits_of_float (float_of_string text) = bits in
    let rec shortest lo hi best =
      if lo > hi then best else
      let precision = (lo + hi) / 2 in
      let text = Printf.sprintf "%.*g" precision value in
      if same text
      then shortest lo (precision - 1) text
      else shortest (precision + 1) hi best in
    let text = Printf.sprintf "%.15g" value in
    let text = if not (same text) then
      let text = Printf.sprintf "%.16g" value in
      if same text then text else Printf.sprintf "%.17g" value
    else
      let count = ref 0 and started = ref false in
      let last = Option.value ~default:(String.length text) (String.index_opt text 'e') in
      for i = 0 to last - 1 do
        let c = text.[i] in
        if c >= '0' && c <= '9' then begin
          if c <> '0' then started := true;
          if !started then incr count
        end
      done;
      if !count <= 1 then text else
      let shorter = Printf.sprintf "%.*g" (!count - 1) value in
      if same shorter then shortest 1 (!count - 2) shorter else text in
    let dotted text = if String.contains text '.' then text else text ^ ".0" in
    match String.index_opt text 'e' with
    | None ->
        if decimal then dotted text
        else if value = 0. || Float.abs value >= 0.01 then text
        else
          let start = if text.[0] = '-' then 1 else 0 in
          let first = ref (start + 2) in
          while text.[!first] = '0' do incr first done;
          let n = String.length text - !first in
          let scientific = (if start = 1 then "-" else "")
            ^ String.make 1 text.[!first]
            ^ (if n = 1 then "" else "." ^ String.sub text (!first + 1) (n - 1))
            ^ "e-" ^ string_of_int (!first - start - 1) in
          if String.length scientific < String.length text then scientific else text
    | Some at ->
        let exponent = int_of_string (String.sub text (at + 1) (String.length text - at - 1)) in
        let scientific = String.sub text 0 at ^ "e" ^ string_of_int exponent in
        let negative = text.[0] = '-' in
        let mantissa = String.sub text (if negative then 1 else 0)
          (at - if negative then 1 else 0) in
        let point = Option.value ~default:(String.length mantissa) (String.index_opt mantissa '.') in
        let digits = String.concat "" (String.split_on_char '.' mantissa) in
        let shifted = point + exponent and n = String.length digits in
        let plain_length = (if negative then 1 else 0)
          + (if shifted <= 0 then 2 - shifted + n else if shifted >= n then shifted else n + 1) in
        if not decimal && plain_length > String.length scientific then scientific else
        (if negative then "-" else "")
        ^ (if shifted <= 0 then "0." ^ String.make (-shifted) '0' ^ digits
           else if shifted >= n then digits ^ String.make (shifted - n) '0' ^ (if decimal then ".0" else "")
           else String.sub digits 0 shifted ^ "." ^ String.sub digits shifted (n - shifted))

let append_token buffer token =
  Printf.bprintf buffer "%d:%s" (String.length token) token

let append_field buffer record (Field field) =
  append_token buffer field.name;
  match field.kind with
  | Toggle -> Buffer.add_string buffer (if field.get record then "b1" else "b0")
  | Integer _ -> Printf.bprintf buffer "i%d;" (field.get record)
  | Index_choice _ -> Printf.bprintf buffer "i%d;" (field.get record)
  | Floating _ -> Printf.bprintf buffer "f%s;" (float_text (field.get record))
  | Text -> Buffer.add_char buffer 's'; append_token buffer (field.get record)
  | Choice choice ->
      Buffer.add_char buffer 'c';
      append_token buffer (choice_label choice (field.get record))
  | Encoded encoding ->
      Buffer.add_char buffer 'e';
      append_token buffer (encoding.encode (field.get record))

let encode_key ~cook_only (schema : 'record schema) record =
  let buffer = Buffer.create 128 in
  append_token buffer schema.name;
  List.iter (fun (Field field as value) ->
    if not cook_only || field.impact = Cook then
      append_field buffer record value) schema.fields;
  Buffer.contents buffer

let key schema record = encode_key ~cook_only:false schema record
let cook_key schema record = encode_key ~cook_only:true schema record

let cook_text (schema : 'record schema) record =
  let token label = String.map (fun c -> match c with
      | 'a' .. 'z' | '0' .. '9' -> c
      | 'A' .. 'Z' -> Char.lowercase_ascii c
      | _ -> '_') label in
  schema.fields |> List.filter_map (fun (Field field) ->
    if field.impact <> Cook then None else
    Some (field.name ^ "=" ^ (match field.kind with
      | Toggle -> string_of_bool (field.get record)
      | Integer _ -> string_of_int (field.get record)
      | Index_choice _ -> string_of_int (field.get record)
      | Floating _ -> float_text (field.get record)
      | Text -> field.get record
      | Choice choice -> token (choice_label choice (field.get record))
      | Encoded encoding -> encoding.encode (field.get record))))
  |> String.concat ";"

let apply_field : type record value.
    record -> value kind -> value -> (value -> record -> record) -> impact ->
    value -> (record * impact option, string) result =
  fun record kind old_value set impact candidate ->
    Result.map (fun value ->
      if equal_kind kind old_value value then record, None
      else set value record, Some impact) (normalize_value kind candidate)

let apply schema record ~name value =
  match Hashtbl.find_opt schema.by_name name with
  | None -> Ok None
  | Some (Field field) ->
        let old_value = field.get record in
        let result = match field.kind, value with
          | Toggle, Bool_value value ->
              apply_field record field.kind old_value field.set field.impact value
          | Index_choice _, Int_value value ->
              apply_field record field.kind old_value field.set field.impact value
          | Integer _, Int_value value ->
              apply_field record field.kind old_value field.set field.impact value
          | Floating _, Float_value value ->
              apply_field record field.kind old_value field.set field.impact value
          | Text, Text_value value ->
              apply_field record field.kind old_value field.set field.impact value
          | Choice choice, Choice_value label ->
              (match Array.find_opt (fun (candidate, _) -> candidate = label)
                  choice.options with
               | None -> Error (Printf.sprintf
                   "parameter %S has no choice %S" name label)
               | Some (_, value) -> apply_field record field.kind old_value
                   field.set field.impact value)
          | Encoded encoding, Text_value text ->
              Result.bind (encoding.decode text) (fun value ->
                apply_field record field.kind old_value field.set field.impact
                  value)
          | _ -> Error (Printf.sprintf "parameter %S received the wrong type" name)
        in
        Result.map Option.some result

let apply_all schema record changes =
  let step (record, effects) (name, value) =
    Result.bind (apply schema record ~name value) (function
      | None -> Ok (record, effects)
      | Some (record, None) -> Ok (record, effects)
      | Some (record, Some impact) ->
          Ok (record, add_impact impact effects))
  in
  List.fold_left (fun result change ->
    Result.bind result (fun state -> step state change))
    (Ok (record, no_effects)) changes

let normalize schema record =
  List.fold_left (fun result field ->
    Result.bind result (fun record ->
      normalize_field record field)) (Ok record) schema.fields

module String_table = struct
  let encode rows =
    let encode_cell value =
      let buffer = Buffer.create (String.length value) in
      String.iter (function
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '\t' -> Buffer.add_string buffer "\\t"
        | '\n' -> Buffer.add_string buffer "\\n"
        | character -> Buffer.add_char buffer character) value;
      Buffer.contents buffer in
    rows |> List.map (fun row -> String.concat "\t" (List.map encode_cell row))
         |> String.concat "\n"

  let decode text =
    if text = "" then Ok [] else
    let rows = ref [] and row = ref [] and cell = Buffer.create 32 in
    let finish_cell () =
      row := Buffer.contents cell :: !row;
      Buffer.clear cell in
    let finish_row () = finish_cell (); rows := List.rev !row :: !rows; row := [] in
    let escaped = ref false and error = ref None in
    String.iter (fun character -> if Option.is_none !error then
      if !escaped then begin
        escaped := false;
        match character with
        | '\\' -> Buffer.add_char cell '\\'
        | 't' -> Buffer.add_char cell '\t'
        | 'n' -> Buffer.add_char cell '\n'
        | character -> error := Some (Printf.sprintf
            "unsupported table escape \\%c" character)
      end else match character with
        | '\\' -> escaped := true
        | '\t' -> finish_cell ()
        | '\n' -> finish_row ()
        | character -> Buffer.add_char cell character) text;
    match !error with
    | Some error -> Error error
    | None when !escaped -> Error "trailing table escape"
    | None -> finish_row (); Ok (List.rev !rows)

end

module Number = struct
  type kind = Int | Float

  let valid kind text = match kind with
    | Int -> int_of_string_opt text <> None
    | Float -> (match float_of_string_opt text with Some v -> Float.is_finite v | None -> false)

  let fraction (lo, hi) text = match float_of_string_opt text with
    | Some v when hi > lo -> Some (Float.max 0. (Float.min 1. ((v -. lo) /. (hi -. lo))))
    | _ -> None

  let show v = float_text ~decimal:true v

  (* the digits a text writes after its point *)
  let decimals text = match String.index_opt text '.' with
    | Some i when not (String.exists (fun c -> c = 'e' || c = 'E') text) -> min 6 (String.length text - i - 1)
    | _ -> 0

  let scrub kind ?range origin dx fine =
    (* a drag stays inside the soft range; a value typed outside it is not pulled in *)
    let held v moved = match range with
      | Some (lo, hi) when hi > lo -> Float.max (Float.min lo v) (Float.min (Float.max hi v) moved)
      | _ -> moved in
    match kind, float_of_string_opt origin with
    | _, None -> origin
    | Int, Some v ->
        let n = Float.round (dx /. 6.) in
        if n = 0. then origin else string_of_int (int_of_float (held v (Float.round v +. n)))
    | Float, Some v ->
        let step = (match range with Some (lo, hi) when hi > lo -> (hi -. lo) /. 200. | _ -> 0.05)
                   /. (if fine then 10. else 1.) in
        (* the result is rounded to the step's digits, or the origin's when it wrote more *)
        let digits = max (decimals origin) (max 0 (min 6 (int_of_float (Float.ceil (-. Float.log10 step))))) in
        let unit = 10. ** float digits in
        let delta = Float.round (dx *. step *. unit) /. unit in
        if delta = 0. then origin else float_text ~decimal:true (held v (Float.round ((v +. delta) *. unit) /. unit))
end
