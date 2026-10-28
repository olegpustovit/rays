(* prims_fixtures.exe OUTDIR writes one JSON file per frame primitive of
   specification/core.md, named after its JavaScript export (lib/web_export/runtime/prims.js):
   "+" is add.json, "int/mul" is int_mul.json, "equal?" is equal.json.
   Each result is computed by the native body, run through Core_exec.

   File: {"primitive": NAME, "export": NAME, "cases": [CASE ...]}
   CASE: {"static": [JSON...], "args": [VALUE...], "result": VALUE | {"error": {"code","message"}}}
   "static" (only coerce, color/byte, fail) are the leading arguments fixed by
   lowering: call export(...static, ...args). A failure is the code and message
   native raised; compare the code exactly.

   VALUE encoding (lossless, tagged where JSON numbers would be ambiguous):
     Int         {"int": "-123"}        decimal text, always, never a JSON number
     Float       12.5                   JSON number; "-0" is negative zero
                 {"float": "NaN" | "Infinity" | "-Infinity"} when not finite
     Bool        true | false           Text   "string" (UTF-8)
     Vec2/3/4    {"vec": [FLOAT ...]}   List   [VALUE ...]
     Record      {"record": [["name", VALUE] ...]}  in field order
     Packed array {"array": WIDTH, "data": [FLOAT ...]}  interleaved components
     Empty geometry (sequence/build filler) null
   TYPE (coerce): "any" "int" "float" "bool" "text" "vec2" "vec3" "vec4" "other",
     {"list": TYPE}, {"record": [["name", TYPE] ...]}. *)
module C = Flow_ir.Core
module E = Flow.Eval
module P = Flow.Packed_ops
module T = Flow.Ty

let inventory = [
  C.Numeric P.Add; Numeric P.Sub; Numeric P.Mul; Numeric P.Div; Numeric P.Mod; Numeric P.Pow;
  Numeric P.Min; Numeric P.Max; Remainder; Math P.Sin; Math P.Cos; Math P.Sqrt; Exp; Math P.Abs;
  Numeric P.Atan2; Numeric P.Hypot; Math P.Tan; Math P.Asin; Math P.Acos; Math P.Log; Math P.Sign;
  Floor; Ceil; Round; Int_convert; Float_convert;
  Numeric P.Lt; Numeric P.Le; Numeric P.Gt; Numeric P.Ge; Numeric P.Eq; Numeric P.And; Numeric P.Or; Math P.Not;
  Integer P.Iadd; Integer P.Isub; Integer P.Imul; Integer P.Idiv; Integer P.Imod;
  Integer P.Iand; Integer P.Ior; Integer P.Ixor; Integer P.Ishl; Integer P.Ishr;
  Integer_compare P.Ilt; Integer_compare P.Ile; Integer_compare P.Igt;
  Integer_compare P.Ige; Integer_compare P.Ieq;
  Equal; Show; Text_concat; Text_blank; Text_trim; Text_lower; Text_repr; Text_table_decode; Text_table_encode;
  Text_split; Text_tokens; Text_to_float; Text_to_int;
  Sequence_length; Sequence_get; Sequence_build; Sequence_slice; Sequence_concat;
  Array_reduce Minimum; Array_reduce Maximum; Array_segment Sum; Array_segment Minimum; Array_segment Maximum; Array_segment_argmax;
  Array_grade; Array_index_of; Array_compact; Array_scan_sum; Array_scatter_add; Array_max_length; Array_range; Array_int; Array_text; Array_float; Array_vec3; Array_vec4; Array_nth; Array_count; Array_slice; Array_concat;
  Rand; Rand_at; Fma; Next_after; Checked_mul; Checked_fma; Noise3; Color_parse; Color_byte Clamped; Coerce T.Any; Truthy; Error; Fail ("", "") ]

let export name = match name with
  | "+" -> "add" | "-" -> "sub" | "*" -> "mul" | "/" -> "div" | "<" -> "lt" | "<=" -> "le"
  | ">" -> "gt" | ">=" -> "ge" | "=" -> "eq" | "equal?" -> "equal"
  | "text/blank?" -> "text_blank"
  | _ -> String.map (function '/' -> '_' | c -> c) name

(* ---- JSON ---- *)
let str = Flow_web.Kernel_table.json_string
let seq f xs = "[" ^ String.concat "," (List.map f xs) ^ "]"
let flt x =
  if Float.is_nan x then {|{"float":"NaN"}|}
  else if x = infinity then {|{"float":"Infinity"}|}
  else if x = neg_infinity then {|{"float":"-Infinity"}|}
  else Param.float_text x
let rec json : E.value -> string = function
  | Int n -> Printf.sprintf {|{"int":"%d"}|} n
  | Float x -> flt x
  | Bool b -> string_of_bool b
  | Text s -> str s
  | Vec2 (x, y) -> {|{"vec":|} ^ seq flt [x; y] ^ "}"
  | Vec3 (x, y, z) -> {|{"vec":|} ^ seq flt [x; y; z] ^ "}"
  | Vec4 (x, y, z, w) -> {|{"vec":|} ^ seq flt [x; y; z; w] ^ "}"
  | List xs -> seq json (Array.to_list xs)
  | Record fs -> {|{"record":|} ^ seq (fun (n, v) -> "[" ^ str n ^ "," ^ json v ^ "]") fs ^ "}"
  | Float_array d -> packed 1 d | Vec2_array d -> packed 2 d
  | Vec3_array d -> packed 3 d | Vec4_array d -> packed 4 d
  | Int_array d -> {|{"array":"int","data":|} ^ seq (fun n->str(string_of_int n)) (Array.to_list d) ^ "}"
  | Text_array d -> {|{"array":"text","data":|} ^ seq str (Array.to_list d) ^ "}"
  | No_geo -> "null"
  | _ -> failwith "prims_fixtures: unsupported value"
and packed w d = Printf.sprintf {|{"array":%d,"data":%s}|} w (seq flt (Array.to_list d))

(* Raw geometry fixtures compare storage bits, not arithmetic Float tolerance. *)
let rec raw_json = function
  | E.Float_array values -> raw_packed 1 values
  | E.Vec2_array values -> raw_packed 2 values
  | E.Vec3_array values -> raw_packed 3 values
  | E.Vec4_array values -> raw_packed 4 values
  | E.Record fields -> {|{"record":|} ^ seq (fun(name,value)->
      "["^str name^","^raw_json value^"]") fields ^ "}"
  | E.List values -> seq raw_json (Array.to_list values)
  | value -> json value
and raw_packed width values = Printf.sprintf {|{"array":%d,"bits":%s}|} width
  (seq (fun value->str(Printf.sprintf "%016Lx" (Int64.bits_of_float value))) (Array.to_list values))

let rec ty_json : T.t -> string = function
  | Any -> {|"any"|} | Int -> {|"int"|} | Float -> {|"float"|} | Bool -> {|"bool"|} | Text -> {|"text"|}
  | Vec2 -> {|"vec2"|} | Vec3 -> {|"vec3"|} | Vec4 -> {|"vec4"|}
  | List e -> {|{"list":|} ^ ty_json e ^ "}"
  | Record fs -> {|{"record":|} ^ seq (fun (n, t) -> "[" ^ str n ^ "," ^ ty_json t ^ "]") fs ^ "}"
  | _ -> {|"other"|}

(* ---- native evaluation: one assignment through Core_exec ---- *)
let arg_name prim i = match prim, i with
  | C.Numeric _, _ | Integer _, _ | Integer_compare _, _ | Equal, _ | Remainder, _ -> if i = 0 then "a" else "b"
  | Math P.Not, _ -> "a"
  | Math _, _ | Floor, _ | Ceil, _ | Round, _ | Int_convert, _ | Float_convert, _ -> "x"
  | Array_max_length, _ -> "type"
  | Array_range, _ -> if i = 0 then "count" else "type"
  | Sequence_build, _ -> [|"count";"width";"type"|].(i)
  | Array_int, _ | Array_text, _ | Array_float, _ | Array_vec3, _ | Array_vec4, _ -> if i = 0 then "count" else "value"
  | Array_count, _ -> "array"
  | Text_blank, _ | Text_trim, _ | Text_lower, _ | Text_repr, _ -> "text"
  | Text_table_decode, _ -> "text" | Text_table_encode, _ -> "rows"
  | Text_split, 0 -> "text" | Text_split, _ -> "separator"
  | Text_tokens, _ | Text_to_float, _ | Text_to_int, _ -> "text"
  | Array_reduce _, _ -> "array"
  | Rand_at, 0 -> "seed" | Rand_at, _ -> "index"
  | Fma, _ | Checked_fma, _ | Checked_mul, _ -> [|"a";"b";"c"|].(i)
  | Next_after, _ -> if i = 0 then "x" else "y"
  | Error, 0 -> "code" | Error, 1 -> "message" | Error, _ -> "cause"
  | Array_grade, _ -> [|"keys";"descending"|].(i) | Array_index_of, _ -> [|"keys";"queries"|].(i)
  | Array_compact, _ -> "mask" | Array_scan_sum, _ -> "array"
  | Array_scatter_add, _ -> [|"indices";"values";"size"|].(i)
  | (Array_segment _ | Array_segment_argmax), _ -> if i=0 then "values" else "offsets"
  | Array_nth, _ -> if i = 0 then "array" else "index"
  | Exp, _ -> "x"
  | Array_slice, _ -> [|"array"; "first"; "count"|].(i)
  | Array_concat, _ -> if i = 0 then "first" else "second"
  | Sequence_slice, _ -> [|"list"; "first"; "count"|].(i)
  | Noise3, _ -> [|"position"; "seed"; "octaves"; "lacunarity"; "gain"|].(i)
  | _ -> Printf.sprintf "x%d" i
let source : C.source = { site = []; owner = []; instance = 0; prefix = []; iter = []; span = None }
let run prim args =
  let n = List.length args in
  let st id node : C.statement = { id; source; node } in
  let program : C.program = {
    constants = Array.of_list (List.map (fun v -> Option.get (C.literal_of_value v)) args);
    locals = Array.make (n + 1) T.Any;
    body = List.mapi (fun i _ -> st i (C.Assign (i, C.Constant i))) args
      @ [st n (C.Assign (n, C.Prim (prim, List.mapi (fun i _ -> arg_name prim i, i) args)))];
    results = [| n |]; state_slots = [||]; regions = [||]; call_slots = 0 } in
  match (fun p -> Flow_ir.Core_exec.(execute (prepare p))) program ~live:(Input.at_time 0.) with
  | Ok v -> json v
  | Error (d : Diagnostic.t) -> Printf.sprintf {|{"error":{"code":%s,"message":%s%s}}|} (str d.code) (str d.message)
      (Option.fold ~none:"" ~some:(fun cause->",\"cause\":"^str cause)d.cause)

(* ---- cases ---- *)
let cases : (string, string list ref) Hashtbl.t = Hashtbl.create 64
let add ?(static = []) prim args =
  let name = C.primitive_name prim in
  let r = match Hashtbl.find_opt cases name with
    | Some r -> r | None -> let r = ref [] in Hashtbl.add cases name r; r in
  r := Printf.sprintf {|{"static":%s,"args":%s,"result":%s}|} (seq Fun.id static)
         (seq json args) (run prim args) :: !r

let add_host name args result =
  let r=match Hashtbl.find_opt cases name with Some r->r | None->let r=ref [] in Hashtbl.add cases name r;r in
  r:=Printf.sprintf {|{"static":[],"args":%s,"result":%s}|} (seq json args) result :: !r

(* array/sum lowers to a typed loop, rather than a standalone Core primitive. *)
let integer_sum values =
  let st id node : C.statement = {id;source;node} in
  let program : C.program = {
    constants=[|Option.get(C.literal_of_value(E.Int_array values));C.Int 0|];
    locals=[|T.Array T.Int;T.Int;T.Int;T.Int|];
    body=[st 0 (Assign(0,Constant 0));st 1 (Assign(1,Constant 1));
      st 2 (Iterate{sequence=0;index=2;item=3;limit=max_int;
        body=[st 3 (Assign(1,Prim(Integer P.Iadd,["a",1;"b",3])))]})];
    results=[|1|];state_slots=[||];regions=[||];call_slots=0} in
  match (fun p -> Flow_ir.Core_exec.(execute (prepare p))) program ~live:(Input.at_time 0.) with
  | Ok value -> json value
  | Error d -> failwith (Diagnostic.to_string d)

let int n = E.Int n and flt_ x = E.Float x and txt s = E.Text s
let v2 x y = E.Vec2 (x, y) and v3 x y z = E.Vec3 (x, y, z) and v4 x y z w = E.Vec4 (x, y, z, w)
let rec_ fs = E.Record fs
let ints = [0; 1; -1; 2; 3; -3; 7; -7; max_int; min_int; max_int - 1; min_int + 1;
  1 lsl 53; (1 lsl 53) + 1; 1 lsl 61; -(1 lsl 61); 1 lsl 31; 4_000_000_000_000_000_000;
  4_611_686_018_427_387_903 / 3]
let scalars = [int 0; int 7; int (-7); int 3; int max_int; int min_int; int ((1 lsl 53) + 1);
  flt_ 0.; flt_ (-0.); flt_ 2.5; flt_ (-2.5); flt_ 1e300; flt_ nan; flt_ infinity; E.Bool true; txt "x"]
let vectors = [v2 1. 2.; v3 1. 2. 3.; v4 1. 2. 3. 4.; v3 1e308 (-1e308) 0.5]
let unary_samples = [int 0; int 1; int (-1); int 7; int (-7); int max_int; int min_int; int ((1 lsl 53) + 1);
  flt_ 0.; flt_ (-0.); flt_ 0.5; flt_ (-0.5); flt_ 1.5; flt_ 2.5; flt_ (-2.5); flt_ (-2.7);
  flt_ 0.49999999999999994; flt_ 4e18; flt_ (-4e18); flt_ 3.9e18; flt_ 1e19; flt_ 1e300; flt_ nan;
  flt_ infinity; flt_ neg_infinity; flt_ 1e-300; flt_ 710.; flt_ 0.1; flt_ 123456.789;
  E.Bool true; E.Bool false; txt "x"; v3 1. 2. 3.; E.List [||]]
let pairs xs ys = List.concat_map (fun x -> List.map (fun y -> [x; y]) ys) xs
let binary prim =
  List.iter (add prim) (pairs scalars scalars);
  List.iter (add prim) (pairs vectors [int 2; flt_ 0.5; E.Bool true; txt "x"]);
  List.iter (add prim) (pairs [int 2; flt_ 0.5; E.Bool true; txt "x"] vectors);
  List.iter (add prim) (pairs vectors vectors)
let unary prim = List.iter (fun v -> add prim [v]) unary_samples
let arr1 xs = E.Float_array (Array.of_list xs)
let arr2 xs = E.Vec2_array (Array.of_list xs)
let arr3 xs = E.Vec3_array (Array.of_list xs)
let arr4 xs = E.Vec4_array (Array.of_list xs)
let digits xs = E.List (Array.of_list xs)
let counts = [int 0; int 1; int 5; flt_ 3.; flt_ 2.5; flt_ (-1.); int (-1); flt_ nan; E.Bool true;
  txt "x"; flt_ 1e300; flt_ 1e17; v3 1. 2. 3.]
let show_values = [flt_ 0.03125; flt_ (-0.03125); flt_ 17179869184.03125; flt_ 1e21; flt_ (-0.0);
  flt_ 0.00005; flt_ 0.; flt_ nan; flt_ infinity; flt_ neg_infinity; flt_ 1e-7; flt_ 0.99995;
  flt_ 123456.789; flt_ 1e300; flt_ 5e-324; flt_ (0.1 +. 0.2); flt_ 0.00015; flt_ 2.5; flt_ (-1e-5);
  flt_ 1.00005; flt_ 4503599627370497.5; flt_ 17179869184.0312501; flt_ 1.7976931348623157e308;
  int 0; int (-5); int max_int; int min_int; E.Bool true; E.Bool false; txt ""; txt "a b \"q\" é日";
  v2 1. 0.5; v3 0.03125 (-0.) nan; v4 1. 2. 3. 4.; E.List [||]; digits [int 1; flt_ 2.; txt "x"];
  digits [digits [int 1]; E.List [||]]; rec_ []; rec_ [("a", int 1); ("b", digits [E.Bool true])];
  arr1 [1.; 2.]; arr2 [1.; 2.; 3.; 4.]; arr3 [1.; 2.; 3.]; arr4 []; arr1 []; arr2 [1.; 2.; 3.];
  E.No_geo]

let () =
  let outdir = match Sys.argv with [| _; dir |] -> dir
    | _ -> prerr_endline "usage: prims_fixtures OUTDIR"; exit 2 in
  List.iter binary [C.Numeric P.Add; Numeric P.Sub; Numeric P.Mul; Numeric P.Div; Numeric P.Mod;
    Numeric P.Pow; Numeric P.Min; Numeric P.Max; Numeric P.Lt; Numeric P.Le; Numeric P.Gt;
    Numeric P.Ge; Numeric P.Eq; Numeric P.And; Numeric P.Or];
  List.iter (fun p -> List.iter (add p) (pairs scalars scalars)) [C.Remainder];
  add C.Remainder [flt_ 7.5; flt_ 2.]; add C.Remainder [flt_ (-7.5); flt_ 2.];
  add C.Remainder [flt_ 7.5; flt_ (-2.)]; add C.Remainder [flt_ 1e300; flt_ 3.];
  List.iter unary [C.Math P.Sin; Math P.Cos; Math P.Sqrt; Exp; Math P.Abs; Floor; Ceil; Round;
    Math P.Tan; Math P.Asin; Math P.Acos; Math P.Log; Math P.Sign;
    Int_convert; Float_convert; Math P.Not; Truthy];
  List.iter (fun p -> List.iter (add p) (pairs scalars scalars)) [C.Numeric P.Atan2; Numeric P.Hypot];
  List.iter (fun (a,b) ->
    add (C.Numeric P.Hypot) [flt_ a;flt_ b];
    add (C.Numeric P.Atan2) [flt_ a;flt_ b])
    [0.,0.;-0.,0.;0.,-0.;-0.,-0.;1e308,1e308;1e-300,1e-300;
     Int64.float_of_bits 1L,Int64.float_of_bits 1L];
  (* integer operations: grid of boundary values, then conversions of non-Int operands *)
  List.iter (fun op -> let p = C.Integer op in
    List.iter (add p) (pairs (List.map int ints) (List.map int ints));
    List.iter (add p) (pairs [flt_ 2.5; flt_ (-2.5); flt_ 1e19; flt_ nan; E.Bool true; txt "x"; flt_ 3.5]
      [int 3; int (-2)]);
    List.iter (add p) (pairs [int 7; int (-7)] [flt_ 2.5; flt_ (-0.4); E.Bool true; flt_ infinity; txt "x"]))
    [P.Iadd; Isub; Imul; Idiv; Imod; Iand; Ior; Ixor; Ishl; Ishr];
  List.iter (fun comparison -> List.iter (add(C.Integer_compare comparison))
    (pairs (List.map int ints) (List.map int ints))) [P.Ilt;Ile;Igt;Ige;Ieq];
  List.iter (fun shift -> List.iter(fun value ->
    List.iter(fun amount -> add(C.Integer shift)[int value;int amount])
      [-65;-64;-63;-1;0;1;62;63;64;65]) ints) [P.Ishl;Ishr];
  (* equal?, show, text/concat *)
  let eqs = [int 1; int 1; int max_int; int (max_int - 1); flt_ 1.; flt_ 0.; flt_ (-0.); flt_ nan;
    E.Bool true; E.Bool false; txt "a"; txt ""; txt "A"; v2 1. 2.; v3 1. 2. 3.; v3 1. 2. 4.;
    v4 1. 2. 3. 4.; E.List [||]; digits [int 1]; digits [flt_ 1.]; digits [int 1; int 2];
    digits [flt_ nan]; rec_ [("a", int 1); ("b", int 2)]; rec_ [("b", int 2); ("a", int 1)];
    rec_ [("a", int 1)]; rec_ []; arr1 [1.; 2.]; arr2 [1.; 2.]; arr1 []; arr3 []; arr1 [nan];
    arr1 [0.]; arr1 [-0.]; E.No_geo] in
  List.iter (add C.Equal) (pairs eqs eqs);
  List.iter (fun v -> add C.Show [v]) show_values;
  List.iter (add C.Text_concat) [[]; [txt ""]; [txt "a"]; [txt "a"; txt ""; txt "bc"];
    [txt "é"; txt "日本"; txt "\n\"\\"]; [txt "a"; int 1]; [flt_ 1.]; [txt "a"; txt "b"; E.Bool true]];
  List.iter (fun s -> add C.Text_blank [txt s])
    ["";" ";"\t";"\n";"\r";"\012";" \t\n\r\012";"\011";"\000";" ";"　";"é日本🐈";" a ";"a\n"];
  List.iter (fun value -> add C.Text_blank [value]) [int 0;E.Bool false;E.No_geo];
  List.iter(fun primitive->List.iter(fun text->add primitive[txt text])
    ["";" \t\r\n\012";" \tA B\r\n";"\011A\011";"\000A\000";"  A 　";"éÉΣİ日本😀ABCZaz";"A\nB";"\rA\012"];
    List.iter(fun value->add primitive[value])[int 0;E.Bool false;E.No_geo;digits[txt "A"]])
    [C.Text_trim;C.Text_lower;C.Text_repr];
  List.iter(fun text->add C.Text_table_decode[txt text])
    ["";"a\tb\nc\td";"é日本😀\tline\\n\\t\\\\";"\t";"\n";"\\q";"\\é";"\\😀";"abc\\";"\\\\\\n\\t"];
  List.iter(fun rows->add C.Text_table_encode[rows])
    [digits[];digits[digits[]];digits[digits[txt ""]];digits[digits[txt "a";txt "b"]];
     digits[digits[txt "é日本😀";txt "\t\n\\"];digits[txt ""]];digits[digits[];digits[]];
     int 0;digits[int 0];digits[digits[int 0]]];
  add C.Text_table_decode[int 0];
  List.iter(fun(text,separator)->add C.Text_split[txt text;txt separator])
    ["","," ;"a,b,,c",",";",a,",",";"a::b::","::";"a:::b","::";"é,日本,😀",",";"abc","abc";"a\tb","\t"];
  List.iter(fun value->add C.Text_split[value;txt ","];add C.Text_split[txt "a";value])[int 0;E.Bool false;E.No_geo];
  add C.Text_split[txt "a";txt ""];
  List.iter(fun text->add C.Text_tokens[txt text])
    ["";" ";"0 1,2\t3\n4";"  1,,2 \n\n";"a\rb";"é,日本\t😀";"1\r\n2"];
  List.iter(fun primitive->List.iter(fun text->add primitive[txt text])
    ["";" ";"0";"-0";"+5";" 12 ";"1.5";"-2.25e3";"1e5";"1E-2";".5";"5.";"1.e2";".";"e5";"1e";"1e+";"+";"-";"1_0";
     "0x10";"0b1";"nan";"inf";"-infinity";"1e999";"1e-999";"4611686018427387903";"4611686018427387904";
     "-4611686018427387904";"-4611686018427387905";"99999999999999999999";"１２";"1 2";"1,2";"\t7\n";"\0111"];
    List.iter(fun value->add primitive[value])[int 0;E.Bool false;E.No_geo])
    [C.Text_to_float;C.Text_to_int];
  add C.Text_tokens[int 0];
  List.iter(fun primitive->List.iter(fun array->add primitive[array])
    [E.Int_array[||];E.Int_array[|0|];E.Int_array[|1|];E.Int_array[|0;1;0;1|];
     E.Int_array[|1;1;1|];E.Int_array[|1;2;0|];E.Int_array[|-1;1|];
     E.Int_array[|max_int;1;min_int;9007199254740993|];
     E.Int_array(Array.init 32769(fun i->i mod 2));
     arr1[];arr1[0.;1.];E.Vec3_array[|0.;1.|];E.Text_array[|"0";"1"|];
     int 1;digits[int 0;int 1];E.Bool false;E.No_geo]) [C.Array_compact;C.Array_scan_sum];
  let text_keys=[|"";"a";"a";"x\000y";"\255\128";"é日本😀";"\240\144\128\128";"\238\128\128";
    String.init 256 Char.chr|]in
  let byte_keys=Array.init 256(fun i->String.make 1(Char.chr i))in
  let grade_keys=[E.Int_array[||];E.Int_array[|min_int;max_int;9007199254740993;0;0;-1|];
    arr1[0.;-0.;Float.next_after 0. 1.;Float.max_float;-.Float.max_float;1.;1.];
    arr1[nan];arr1[infinity];arr1[neg_infinity];E.Text_array text_keys;E.Text_array byte_keys;
    E.Vec3_array[|0.;1.|];E.Text "not array"]in
  List.iter(fun keys->List.iter(fun descending->add C.Array_grade[keys;E.Bool descending])[false;true])grade_keys;
  List.iter(fun(keys,queries)->add C.Array_index_of[keys;queries])
    [E.Int_array[|4;4;7|],E.Int_array[|4;7;9;4|];E.Int_array[||],E.Int_array[|min_int;max_int|];
     E.Int_array[|min_int;max_int;9007199254740993|],E.Int_array[|9007199254740993;min_int;7|];
     E.Text_array text_keys,E.Text_array(Array.append text_keys[|"missing"|]);
     E.Text_array byte_keys,E.Text_array(Array.append byte_keys[|"missing"|]);
     E.Text_array[||],E.Text_array[|"";"x\000y"|];E.Int_array[|1|],E.Int_array[||];
     E.Text_array[|"1"|],E.Int_array[|1|];arr1[1.],arr1[1.];E.Int 0,E.Int_array[||];
     E.Int_array[||],E.Text "bad"];
  let arrays=[arr1[];
arr1[0.;-0.;0.];arr1[1e16;1.;-1e16];
    E.Int_array[|min_int;max_int;9007199254740993;-9007199254740993|];
    arr2[3.;-0.;1.;0.];arr3[3.;2.;-0.;1.;4.;0.];arr4[3.;2.;1.;-0.;1.;4.;5.;0.];
    arr1[nan];arr1[infinity];E.Text_array[|"x"|]] in
  List.iter(fun operation->List.iter(fun array->add(C.Array_reduce operation)[array])arrays)
    [Flow.Op.Minimum;Maximum];
  List.iter(fun operation->List.iter(fun array->
    let count=Flow.Value.array_length array in
    List.iter(fun offsets->add(C.Array_segment operation)[array;E.Int_array offsets])
      [[|0;count|];[|0;0;count|];[|0|];[||];[|0;count+1;count|];[|0;-1;count|];[|0;max_int;count|]];
    add(C.Array_segment operation)[array;arr1[0.;float count]]) arrays)
    [Flow.Op.Sum;Minimum;Maximum];
  List.iter(fun array->
    let count=Flow.Value.array_length array in
    List.iter(fun offsets->add C.Array_segment_argmax [array;E.Int_array offsets])
      [[|0;count|];[|0;0;count|];[|0|];[||];[|0;count+1;count|];[|0;-1;count|];[|0;max_int;count|];
       [|0;count/2;count|]];
    add C.Array_segment_argmax [array;arr1[0.;float count]])
    (arrays @ [E.Int_array[|2;5;5;1;5|]; arr1[1.;3.;3.;-0.;0.;2.;3.]; arr1[0.;-0.;0.]]);
  List.iter(fun values->let n=Flow.Value.array_length values in
    let indices=E.Int_array(Array.init n(fun i->i mod 2)) in
    List.iter(fun size->add C.Array_scatter_add[indices;values;int size])[0;1;2;4;-1;max_int];
    add C.Array_scatter_add[E.Int_array[||];values;int 2];
    add C.Array_scatter_add[arr1[];values;int 2];
    add C.Array_scatter_add[indices;values;flt_ 2.]) arrays;
  List.iter(fun(indices,values,size)->add C.Array_scatter_add[indices;values;int size])
    [E.Int_array[|0;0;1|],arr1[Float.max_float;Float.max_float;1.],1;
     E.Int_array[|1;0;0|],arr1[1.;Float.max_float;Float.max_float],1;
     E.Int_array[|0;0;0|],arr1[1e16;1.;-1e16],1;
     E.Int_array[|0;0|],E.Int_array[|max_int;1|],1;
     E.Int_array[|0|],E.Int_array[|9007199254740993|],1;
     E.Int_array[|0|],E.Vec3_array[|1.;2.|],1];
  (* sequences *)

  let l3 = digits [int 1; int 2; int 3] in
  let mixed = digits [int 1; flt_ 2.; txt "z"] in
  List.iter (fun v -> add C.Sequence_length [v])
    [E.List [||]; l3; mixed; digits [E.List [||]]; int 3; txt "abc"; arr1 [1.]];
  List.iter (fun l -> List.iter (fun i -> add C.Sequence_get [l; i])
    [int (-1); int 0; int 1; int 2; int 3; flt_ 1.; flt_ 1.5; flt_ 0.5; flt_ 2.5; flt_ nan; flt_ infinity;
     flt_ 1e19; flt_ 1e-5; flt_ 123456789.5; E.Bool true; txt "x"; int max_int])
    [E.List [||]; l3; digits [txt "a"; v3 1. 2. 3.]; int 4];
  List.iter (fun c -> add C.Sequence_build [c])
    [int (-1); int 0; int 1; int 5; int 4096; int 4097; flt_ 2.5; flt_ 0.4; E.Bool true; txt "x"; flt_ 1e19];
  (* a width allocates packed storage, with no list cap *)
  List.iter (fun w -> List.iter (fun c -> add C.Sequence_build [c; int w])
    [int (-1); int 0; int 1; int 5; int 4097; int 100_000; flt_ 2.5; flt_ 1e19]) [1; 2; 3; 4];
  List.iter (fun l ->
    List.iter (fun (f, c) -> add C.Sequence_slice [l; int f; int c])
      [(0, 0); (0, 3); (1, 2); (3, 0); (4, 0); (2, 2); (-1, 1); (1, -1); (0, 4); (3, 1)])
    [l3; E.List [||]; mixed];
  add C.Sequence_slice [l3; flt_ 0.5; flt_ 1.4]; add C.Sequence_slice [int 3; int 0; int 0];
  add C.Sequence_slice [l3; txt "x"; int 0];
  let bools n = E.List (Array.make n (E.Bool true)) in
  List.iter (add C.Sequence_concat) [[]; [l3]; [l3; mixed]; [E.List [||]; E.List [||]];
    [digits [int 1]; digits [flt_ 2.]]; [digits [int 1]; digits [txt "a"]];
    [digits [flt_ 1.]; digits [int 2]; digits [E.Bool true]];
    [digits [v2 1. 2.]; digits [v2 3. 4.]];
    [digits [rec_ [("a", int 1)]]; digits [rec_ [("a", flt_ 2.)]]];
    [digits [digits [int 1]]; digits [digits [flt_ 2.]]];
    [digits [rec_ [("a", int 1); ("b", int 2)]]; digits [rec_ [("b", flt_ 1.); ("a", flt_ 2.)]]];
    [l3; int 4]; [bools 2048; bools 2048]; [bools 2048; bools 2049]; [bools 4096; E.List [||]]];
  (* packed arrays *)
  List.iter (fun c -> add C.Array_text [c]) counts;
  List.iter (fun value -> add C.Array_text [int 3; value])
    [txt ""; txt "snow"; txt "\226\152\131"; int 1; E.Bool true];
  let text_arrays = List.map (fun xs -> E.Text_array (Array.of_list xs))
    [[]; [""]; ["a"; "\226\152\131"; "b"]] in
  List.iter (fun array ->
    add C.Array_count [array];
    List.iter (fun index -> add C.Array_nth [array; int index]) [-1;0;1;3];
    List.iter (fun (first,count) -> add C.Array_slice [array; int first; int count])
      [0,0;0,1;1,2;0,19;-1,1;1,-1]) text_arrays;
  List.iter (fun pair -> match pair with [a;b] -> add C.Array_concat [a;b] | _ -> assert false)
    (pairs text_arrays (text_arrays @ [arr1 [1.]]));
  add C.Array_max_length [];
  List.iter(fun kind->add C.Array_max_length [kind])
    [txt "int";txt "text";txt "float";txt "vec2";txt "vec3";txt "vec4";
     txt "unknown";txt "";int 1;E.Bool true;E.Text_array [|"float"|]];
  List.iter (fun c -> add C.Array_range [c]) counts;

  List.iter (fun c -> add C.Array_range [c;txt "int"];
    add C.Array_int [c]) (int 4097::counts);
  List.iter(fun kind -> add C.Array_range[int 3;kind]) [txt "float";txt "invalid";int 1];
  List.iter(fun value -> add C.Array_int[int 3;int value]) ints;
  List.iter(fun value -> add C.Array_int[int 2;value])
    [flt_ 2.5;flt_ (-2.5);flt_ nan;flt_ infinity;txt "x";E.Bool true];
  List.iter(fun c -> add C.Sequence_build[c;int 1;txt "int"])
    [int(-1);int 0;int 3;int 4097];
  List.iter(fun width -> add C.Sequence_build[int 3;int width;txt "int"]) [0;2;3;4];
  let integer_arrays=List.map(fun xs -> E.Int_array(Array.of_list xs))
    [[];[0];[min_int;max_int;((1 lsl 53)+1)];ints] in
  List.iter(fun array -> add C.Array_count[array];
    List.iter(fun index -> add C.Array_nth[array;int index]) [-1;0;1;3;19];
    List.iter(fun(first,count) -> add C.Array_slice[array;int first;int count])
      [0,0;0,1;1,2;0,19;-1,1;1,-1];
    match array with E.Int_array values -> add_host "array/sum" [array] (integer_sum values)
      | _ -> assert false) integer_arrays;
  List.iter(fun(a,b) -> add C.Array_concat[a;b])
    (pairs integer_arrays (integer_arrays@[arr1[1.]])
      |>List.map(function[a;b]->a,b|_->assert false));
  List.iter(fun values -> add_host "array/sum" [E.Int_array values] (integer_sum values))
    [Array.make 4097 max_int;[|max_int;1;min_int;-1|];[|(1 lsl 53)+1;1;-(1 lsl 53)|]];
  List.iter (fun (c, v) -> add C.Array_float [c; v]) [(int 3, flt_ 2.5); (int 2, int 7);
    (int 2, E.Bool true); (int 0, flt_ nan); (int 1, flt_ nan); (int 2, flt_ infinity);
    (int 2, v3 1. 2. 3.); (int 2, txt "x"); (flt_ 2.5, flt_ 1.); (int 3, flt_ (-0.))];
  add C.Array_float [int 3]; add C.Array_float [flt_ nan]; add C.Array_float [int 0];
  List.iter (fun (c, v) -> add C.Array_vec3 [c; v]) [(int 2, v3 1. 2. 3.); (int 2, flt_ 1.5);
    (int 2, int 3); (int 2, E.Bool true); (int 2, v2 1. 2.); (int 2, v4 1. 2. 3. 4.); (int 2, txt "x");
    (int 0, txt "x"); (int 1, v3 nan 0. 0.); (int 1, flt_ infinity); (int (-1), flt_ 1.);
    (flt_ 1.5, flt_ 1.); (int 3, flt_ (-0.))];
  add C.Array_vec3 [int 2]; add C.Array_vec3 [int 0];
  List.iter (add C.Array_vec4) [[int 2;v4 1. 2. 3. 4.]; [int 3;flt_ (-0.)];
    [int 2;int 7];[int 2;E.Bool true];[int 2;v3 1. 2. 3.];
    [int 0;txt "ignored"];[int 1;v4 nan 0. 0. 0.];[int (-1);flt_ 1.];
    [flt_ 1.5;flt_ 1.];[int 2];[int 0]];
  let arrays = [arr1 [1.; 2.; 3.]; arr2 [1.; 2.; 3.; 4.]; arr3 [1.; 2.; 3.; 4.; 5.; 6.];
    arr4 [1.; 2.; 3.; 4.]; arr1 []; arr2 []; arr3 []; arr4 []] in
  List.iter (fun v -> add C.Array_count [v])
    (arrays @ [arr2 [1.; 2.; 3.]; arr3 [1.; 2.; 3.; 4.]; arr4 [1.]; int 3; digits [int 1]; txt "x"]);
  List.iter (fun a -> List.iter (fun i -> add C.Array_nth [a; i])
    [int 0; int 1; int 2; int 3; int (-1); flt_ 1.; flt_ 1.5; flt_ nan; E.Bool true; txt "x"])
    (arrays @ [int 1; digits [int 1]; arr3 [1.; 2.]]);
  List.iter (fun a -> List.iter (fun (f, c) -> add C.Array_slice [a; int f; int c])
    [(0, 0); (0, 1); (1, 1); (1, 2); (2, 0); (3, 0); (4, 0); (0, 3); (1, 3); (-1, 1); (1, -1)])
    (arrays @ [int 1; digits [int 1]]);
  add C.Array_slice [arr1 [1.; 2.; 3.]; flt_ 0.5; int 1]; add C.Array_slice [arr1 [1.]; txt "x"; int 0];
  add C.Array_slice [int 1; flt_ 0.5; int 1];
  add C.Array_slice [arr1 [nan; 1.]; int 0; int 2];
  List.iter (fun (a, b) -> add C.Array_concat [a; b]) (pairs arrays arrays |> List.map (function [a; b] -> a, b | _ -> assert false));
  add C.Array_concat [arr1 [1.]; int 2]; add C.Array_concat [int 2; arr1 [1.]];
  add C.Array_concat [arr2 [1.; 2.; 3.]; arr2 [1.]];
  (* hash, noise, colors *)
  List.iter(add C.Error)[[txt "E_RULES";txt "exact é日本😀\t\n"];
    [txt "E_CAUSE";txt "message";txt "reason é日本😀\t\n"];[txt "E_CAUSE";txt "message";txt ""];
    [txt "E_CAUSE";txt "message";int 0];
    [txt "";txt ""];[txt "CODE";txt "message"];
    [int 0;txt "message"];[txt "CODE";int 0];[E.Bool false;txt "message"]];
  List.iter (add C.Fma) (List.map (List.map flt_) [
    [1.0000000000000002;0.9999999999999998;-1.];[-0.;1.;-0.];[0.;-1.;0.];
    [Float.max_float;2.;-.Float.max_float];[Float.max_float;2.;0.];
    [Float.min_float;0.5;0.];[Int64.float_of_bits 1L;0.5;0.];
    [infinity;0.;1.];[1.;nan;0.];[1.;1.;neg_infinity]]);
  add C.Fma [txt "bad";flt_ 1.;flt_ 0.];
  List.iter (add C.Next_after) (List.map (List.map flt_) [
    [1.;2.];[1.;0.];[1.;1.];[-1.;-2.];[-1.;0.];[0.;1.];[0.;-1.];[-0.;1.];[0.;-0.];[-0.;0.];
    [Float.max_float;0.];[-.Float.max_float;0.];[Float.max_float;Float.max_float];
    [Int64.float_of_bits 1L;0.];[Int64.float_of_bits 1L;1.];[-.Int64.float_of_bits 1L;-1.];[0.;Int64.float_of_bits 1L];
    [Float.min_float;0.];[-.Float.min_float;0.];[1.;infinity];[nan;1.];[1.;nan];[1e300;-1e300]]);
  add C.Next_after [txt "bad";flt_ 1.];
  List.iter(fun(a,b,c)->
    add C.Checked_mul[flt_ a;flt_ b];
    add C.Checked_fma[flt_ a;flt_ b;flt_ c]) [
      Float.max_float,2.,0.;Float.max_float,2.,-.Float.max_float;
      Float.max_float,2.,Float.max_float;
      1.+.2.**(-27.),1.-.2.**(-27.),-1.;
      -0.,1.,-0.;0.,-1.,0.;Int64.float_of_bits 1L,0.5,0.;
      -.Int64.float_of_bits 1L,0.5,-0.;0.37,0.83,0.19];
  List.iter(fun bad->
    List.iter(fun(a,b)->add C.Checked_mul[a;b])[bad,flt_ 1.;flt_ 1.,bad];
    List.iter(fun(a,b,c)->add C.Checked_fma[a;b;c])
      [bad,flt_ 1.,flt_ 0.;flt_ 1.,bad,flt_ 0.;flt_ 1.,flt_ 1.,bad])
    [flt_ nan;flt_ infinity;flt_ neg_infinity;txt "bad";v3 1. 2. 3.];
  List.iter(fun seed->List.iter(fun index->add C.Rand_at[int seed;int index])
    [min_int;min_int+1;-(1 lsl 53)-1;0;1;(1 lsl 53)+1;max_int])
    [min_int;min_int+1;0;1;42;max_int];
  List.iter(add C.Rand_at)[[flt_ 1.;int 0];[int 0;flt_ 1.];[E.Bool false;int 0];[int 0;txt "0"]];
  List.iter (add C.Rand) [[]; [flt_ 0.]; [flt_ 1.]; [flt_ 2.]; [flt_ (-1.)]; [flt_ 0.5]; [flt_ 1.; flt_ 2.];
    [flt_ 2.; flt_ 1.]; [flt_ 1.; flt_ 2.; flt_ 3.]; [int 5]; [int 5; int 7; int 9; int 11]; [E.Bool true];
    [flt_ 1e303]; [flt_ nan]; [flt_ infinity]; [flt_ 4294.967296]; [flt_ 4294967296.]; [flt_ (-4294967296.)];
    [flt_ 1e15]; [flt_ 1e9; flt_ (-1e9)]; [flt_ 0.123456789]; [flt_ 1e-7]; [flt_ 3.14159; flt_ 2.71828];
    [flt_ 12345.6789]; [txt "x"]; [flt_ 1.; txt "x"]; [v3 1. 2. 3.]];
  let positions = [(0., 0., 0.); (0.5, 0.5, 0.5); (1.5, 2.25, -3.75); (-0.1, -0.2, -0.3); (100.3, 200.7, 300.1);
    (255.5, 256.5, 257.5); (-255.5, -256.5, -257.5); (1e10, -1e10, 1e10); (0.123, 4.567, 8.9);
    (1e15 +. 0.5, 3., 7.); (1e19, -1e19, 5e18); (1e300, 0., 0.); (-1e300, 1., 2.); (1e308, 1e308, 1e308)] in
  List.iter (fun (x, y, z) -> let p = v3 x y z in
    add C.Noise3 [p]; add C.Noise3 [p; int 7]; add C.Noise3 [p; int (-1); int 3];
    add C.Noise3 [p; int 0; int 2]; add C.Noise3 [p; int max_int; int 32];
    add C.Noise3 [p; int 5; int 4; flt_ 1.7; flt_ 0.62]; add C.Noise3 [p; int 5; int 6; flt_ 3.; flt_ 0.]) positions;
  List.iter (add C.Noise3) [[v3 0.3 0.6 0.9; int min_int; int 1]; [v3 0.3 0.6 0.9; int 42; int 4];
    [v3 0.3 0.6 0.9; flt_ 2.5; flt_ 2.4]; [v3 0.3 0.6 0.9; int 0; int 0]; [v3 0.3 0.6 0.9; int 0; int 33];
    [v3 0.3 0.6 0.9; int 0; int (-1)]; [v3 nan 0. 0.]; [v3 0. infinity 0.]; [v2 0. 0.]; [flt_ 1.];
    [v3 0.3 0.6 0.9; txt "x"]; [v3 0.3 0.6 0.9; int 0; E.Bool true]; [v3 0.3 0.6 0.9; E.Bool true];
    [v3 0.3 0.6 0.9; int 1; int 3; flt_ 0.]; [v3 0.3 0.6 0.9; int 1; int 3; flt_ (-1.)]; [v3 0.3 0.6 0.9; int 1; int 3; flt_ infinity];
    [v3 0.3 0.6 0.9; int 1; int 3; flt_ nan]; [v3 0.3 0.6 0.9; int 1; int 3; flt_ 2.; flt_ 1.]; [v3 0.3 0.6 0.9; int 1; int 3; flt_ 2.; flt_ 1.01];
    [v3 0.3 0.6 0.9; int 1; int 3; flt_ 2.; flt_ (-0.1)]; [v3 0.3 0.6 0.9; int 1; int 3; flt_ 2.; flt_ nan]; [v3 0.3 0.6 0.9; int 1; int 1; flt_ 9.; flt_ 0.3];
    [v3 0.3 0.6 0.9; int 1; int 3; int 3; int 0]; [v3 0.3 0.6 0.9; int 1; int 3; txt "x"]];
  List.iter (fun s -> add C.Color_parse [txt s])
    ["#fff"; "fff"; "#ffff"; "#ff0000"; "#FF000080"; "ff000080"; "#abcdef"; "#AbCdEf12"; "f_0000"; "#f_f_f_";
     "#_f0000"; "#+f0000"; "#-f0000"; "#ff"; ""; "#"; "#12345"; "#1234567"; "#ggg"; "#zzzzzz"; "#0x0000";
     " fff"; "#é1"; "日本"; "##fff"; "#ff 000"; "#00000_"; "#__0000"; "#ffffff__"; "#12_"; "#_12"];
  List.iter (fun v -> add C.Color_parse [v])
    [v3 1. 0. 0.5; v3 0. 0. 0.; v3 1e20 (-1e20) 0.5; v3 0.001 (-0.002) 0.4999; v3 1e300 1e300 1e300;
     v3 3e16 2e16 1e16; v3 (-3e16) (-2e16) (-1e16); v3 0.00196 0.00195 0.998; v3 nan 0. 0.; v3 0. infinity 0.;
     v3 0. 0. neg_infinity; v3 nan nan nan; v3 1.5 2. (-1.);
     E.List [| E.Float 1.; E.Float 0.; E.Float 0.5; E.Float 0.5 |];
     E.List [| E.Float 1e20; E.Float (-1e20); E.Float 0.5; E.Float 1. |];
     E.List [| E.Int 1; E.Bool true; E.Float 0.2; E.Int 0 |];
     E.List [| E.Float nan; E.Float 0.; E.Float 0.; E.Float 0. |];
     E.List [| E.Float 0.; E.Float 0.; E.Float 0.; E.Float infinity |];
     E.List [| E.Float nan; E.Float 0.; E.Float 0.; E.Text "x" |];
     E.List [| E.Float 0.; E.Float 0.; E.Float 0.; E.Text "x" |];
     E.List [| E.Float 0.; E.Float 0.; E.Float 0. |]; E.List [||]; E.List [| E.Float 0.; E.Float 0.; E.Float 0.; E.Float 0.; E.Float 0. |];
     v4 1. 0. 0. 1.; v2 1. 0.; flt_ 1.; int 1; E.Bool true; digits [txt "a"; txt "b"; txt "c"; txt "d"]];
  let channels = [0.; 1.; 0.5; 0.001960784; 0.00196; 0.002; 0.5 /. 255.; 1.5 /. 255.; 254.5 /. 255.; 0.999;
    1.001; -0.001; -0.002; -1.; 2.; 1e20; -1e20; 1e300; -1e300; 1e16; 2e16; 3e16; -2e16; -3e16;
    3.6e16; 9.1e18; 1.8e16; 2e19; nan; infinity; neg_infinity; -0.] in
  List.iter (fun (mode, name) ->
    List.iter (fun x -> add ~static:[str name] (C.Color_byte mode) [flt_ x]) channels;
    List.iter (fun v -> add ~static:[str name] (C.Color_byte mode) [v]) [int 0; int 1; int 2; E.Bool true; txt "x"; v3 1. 1. 1.])
    [(C.Clamped, "clamped"); (C.Unclamped, "unclamped")];
  (* coerce, fail *)
  let rt = T.Record [("a", T.Float); ("b", T.Int)] in
  let values = [int 0; int 3; int (-2); int max_int; flt_ 2.5; flt_ (-2.5); flt_ 0.49999999999999994;
    flt_ 0.5; flt_ 1e19; flt_ 4e18; flt_ nan; flt_ 0.; flt_ (-0.); flt_ infinity; E.Bool true; E.Bool false; txt "x";
    v2 1. 2.; v3 1. 2. 3.; E.List [||]; digits [flt_ 1.5; int 2; E.Bool true]; digits [txt "a"];
    rec_ [("a", int 1); ("b", flt_ 2.5)]; rec_ [("b", flt_ 1.5); ("c", int 4); ("a", E.Bool true)]; rec_ [];
    arr1 [1.]; E.No_geo] in
  List.iter (fun ty -> List.iter (fun v -> add ~static:[ty_json ty] (C.Coerce ty) [v]) values)
    [T.Any; Int; Float; Bool; Text; Vec2; Vec3; Vec4; T.List T.Int; T.List T.Float; T.List T.Bool;
     T.List (T.List T.Int); rt; T.Record []; T.Array T.Float; T.Color];
  List.iter (fun (c, m) -> add ~static:[str c; str m] (C.Fail (c, m)) [])
    [("E_X", "boom"); ("", ""); ("E_UNICODE", "é日\n\"q\"")];
  (* Geometry host operations have a separate immutable record protocol; they
     are not frame-core primitives and do not enlarge the intrinsic inventory. *)
  List.iter (fun seed -> List.iter (fun label ->
    let context=E.Record ["seed",E.Text (Int64.to_string seed);"label",E.Text label] in
    let result=Flow_sop.Operators.seed_for ~seed ~namespace:"noise_displace" ~label () in
    add_host "sop/context_seed" [E.Text "noise_displace";context] (json (E.Int result)))
    ["Noise Displace";"result";"é日";""]) [0L;1L;(-1L);42L;Int64.max_int;Int64.min_int];
  let geometry =
    let positions=Rdk.Packed.Float3.View.of_owned_exn ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] ~z:[|0.;0.;0.|] in
    let topology=Rdk.Topology.polygons_owned ~point_count:3 ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|] |> Result.get_ok in
    let attributes=List.concat_map (fun (owner,count) -> List.map (fun name ->
      Rdk.Attribute.create_owned ~name ~owner (Rdk.Attribute.Float (Array.init count float)) |> Result.get_ok)
      ["N";"Cd";"mask_1";"mask_2";"mask_A";"é";"a b";"a*";"z"])
      [Rdk.Attribute.Point,3;Vertex,3;Primitive,1;Detail,1] in
    Rdk.Geometry.create ~positions ~topology ~attributes () |> Result.get_ok in
  let owner_name=function Rdk.Attribute.Point->"point"|Vertex->"vertex"|Primitive->"primitive"|Detail->"detail" in
  let geometry_value geometry =
    let topology=Rdk.Topology.View.view (Rdk.Geometry.topology geometry) in
    let positions=Rdk.Packed.Float3.View.view (Rdk.Geometry.positions geometry) in
    let positions=Array.init (Array.length positions.x*3) (fun i -> match i mod 3 with
      |0->positions.x.(i/3)|1->positions.y.(i/3)|_->positions.z.(i/3)) in
    let tuple width planes =
      let data=Array.init(Array.length planes.(0)*width)(fun i->planes.(i mod width).(i/width)) in
      match width with 2->E.Vec2_array data|3->E.Vec3_array data|_->E.Vec4_array data in
    let storage = function
      | Rdk.Attribute.Float values -> "float",E.Float_array values
      | Int values -> "int",E.Int_array values
      | Text values -> "text",E.Text_array values
      | Float2 values -> let v=Rdk.Packed.Float2.View.view values in "vec2",tuple 2 [|v.x;v.y|]
      | Float3 values -> let v=Rdk.Packed.Float3.View.view values in "vec3",tuple 3 [|v.x;v.y;v.z|]
      | Float4 values -> let v=Rdk.Packed.Float4.View.view values in "vec4",tuple 4 [|v.x;v.y;v.z;v.w|]
      | Float_array values -> let v=Rdk.Packed.Float_array.View.view values in
          "float_array",E.Record["offsets",E.Int_array v.offsets;"values",E.Float_array v.values]
      | Int_array values -> let v=Rdk.Packed.Int_array.View.view values in
          "int_array",E.Record["offsets",E.Int_array v.offsets;"values",E.Int_array v.values] in
    E.Record ["positions",E.Vec3_array positions;
      "vertex_points",E.List (Array.map (fun n->E.Int n) topology.vertex_points);
      "primitive_offsets",E.List (Array.map (fun n->E.Int n) topology.primitive_offsets);
      "primitive_kinds",E.Text (Bytes.to_string topology.primitive_kinds);
      "attributes",E.List (Array.of_list (List.map (fun attribute->
        let kind,values=storage(Rdk.Attribute.View.storage attribute) in
        E.Record ["owner",E.Text (owner_name (Rdk.Attribute.owner attribute));"name",E.Text (Rdk.Attribute.name attribute);
          "type",E.Text kind;"values",values]) (Rdk.Geometry.attributes geometry)));
      "groups",E.List(Array.of_list(List.map(fun group->
        let owner=match Rdk.Group.owner group with Point->"point"|Vertex->"vertex"|Primitive->"primitive" in
        E.Record["owner",E.Text owner;"name",E.Text(Rdk.Group.name group);
          "mask",E.Int_array(Array.init(Rdk.Group.length group)(fun i->if Rdk.Group.mem i group then 1 else 0));
          "ordered",E.Bool(Rdk.Group.is_ordered group);
          "indices",E.Int_array(Option.value ~default:[||](Rdk.Group.ordered_elements group))])
        (Rdk.Geometry.groups geometry)));
      "edge_groups",E.List(Array.of_list(List.map(fun group->
        E.Record["name",E.Text(Rdk.Edge_group.name group);
          "mask",E.Int_array(Array.init(Rdk.Edge_group.length group)(fun i->if Rdk.Edge_group.mem i group then 1 else 0))])
        (Rdk.Geometry.edge_groups geometry)))] in
  let delete=List.find (fun (op:Flow.Op.t)->op.name="sop/delete_attr") Flow_sop.Operators.all in
  List.iter (fun owner->List.iter (fun pattern->
    let args=["geometry",E.No_geo;"attribute",E.Text pattern;"owner",E.Text owner] in
    let result=try
      delete.check args;
      let for_owner key=if key=owner then pattern else "" in
      match Rdk.Attribute_ops.delete ~point_pattern:(for_owner "point") ~vertex_pattern:(for_owner "vertex")
        ~primitive_pattern:(for_owner "primitive") ~detail_pattern:(for_owner "detail") geometry with
      |Ok g->json (geometry_value g)
      |Error e->Printf.sprintf {|{"error":{"code":"E_ATTR_DELETE","message":%s}}|} (str (Rdk.Error.to_string e))
      with Diagnostic.Error d -> Printf.sprintf {|{"error":{"code":%s,"message":%s}}|} (str d.code) (str d.message) in
    add_host "sop/delete_attr" [geometry_value geometry;E.Text pattern;E.Text owner] result)
    ["N";"*";"mask_?";"mask_[1-2]";"mask_[!1]";"^N";"* ^mask_* mask_1";"a\\ b";"a\\*";
     "?";"??";"[a-z]*";"Cd N";"[";"\\";""]) ["point";"vertex";"primitive";"detail";"invalid"];
  let diagnostic(d:Diagnostic.t)=Printf.sprintf {|{"error":{"code":%s,"message":%s}}|}(str d.code)(str d.message) in
  let host_result f=try match f() with Ok value->json value|Error d->diagnostic d
    with Diagnostic.Error diagnostic_->diagnostic diagnostic_ in
  let op name=List.find(fun(op:Flow.Op.t)->op.name=name)Flow_sop.Operators.all in
  let validation name args =
    let keys=if name="text/glob_error" then ["pattern"] else ["pattern";"replacement"] in
    add_host name args (host_result(fun()->Ok((op name).body
      ~live:(Input.at_time 0.) ~node:(fun _ _->assert false) (List.combine keys args)))) in
  List.iter(fun pattern->validation "text/glob_error"[E.Text pattern])
    ["";"*";"^N Cd*";"é日本😀";"x\\*";"[a-z]";"[!a-z]";"[";"[]";"[!]";"[z-a]";"^";"x\\";"N ["];
  List.iter(validation "text/glob_error") [[E.Int 0];[E.Bool false]];
  List.iter(fun(pattern,replacement)->validation "text/glob_rewrite_error"[E.Text pattern;E.Text replacement])
    ["*","x*";"é*","猫*";"a?","b?";"[ab]","[cd]";"[!ab]","[cd]";
     "*","?";"??","?";"*?","*";"","";"a b","x";
     "^a","x";"a","^x";"*","x";"a","*";"[","x";"a","[";"a\\","x";"a","x\\"];
  List.iter(validation "text/glob_rewrite_error") [[E.Int 0;E.Text "x"];[E.Text "x";E.Int 0]];
  List.iter(fun(pattern,replacement)->validation "text/glob_rewrite_set_error"[E.Text pattern;E.Text replacement])
    ["*","x*";"a* b*","x* y*";"a*\tb*","x*\ty*";"a*\nb*","x*  y*";"a\\ b","c";"a\\ b*","x\\ y*";
     "^a* b*","y*";"a* ^ab b*","x* y*";"^a","x";"","";"a b","x";"a","x y";"a b","x ^y";"a b","^x y";
     "[","x";"a","[";"a\\","x";"a","x\\";"a [","x y";"a ^","x";"\195\169* \231\140\171*","x* y*";"*_??? a*","old_???_* b*"];
  List.iter(validation "text/glob_rewrite_set_error") [[E.Int 0;E.Text "x"];[E.Text "x";E.Int 0]];
  let text_operation name keys args =
    add_host name args (host_result(fun()->Ok((op name).body
      ~live:(Input.at_time 0.) ~node:(fun _ _->assert false) (List.combine keys args)))) in
  List.iter(fun text->text_operation "text/glob_literal" ["text"] [E.Text text])
    (["";"ordinary";"-";"[a-z]";"\\";"\\\\";"^*?[]";"é猫😀";"\011"]
     @List.init 128(fun byte->"p"^String.make 1(Char.chr byte)^"q"));
  List.iter(text_operation "text/glob_literal" ["text"]) [[E.Int 0];[E.Bool false]];
  List.iter(fun name->
    List.iter(fun text->text_operation name ["text"] [E.Text text])
      ["";"ok";"_x1";"A_9";"1a";"9";"a b";"a-b";"é猫😀";"a\000b";"\n";"_";"Zz09_"];
    List.iter(text_operation name ["text"]) [[E.Int 0];[E.Bool false]])
    ["text/group_name_valid";"text/group_name_forced"];
  List.iter(fun(pattern,replacement,text)->text_operation "text/glob_rewrite"
    ["pattern";"replacement";"text"] [E.Text pattern;E.Text replacement;E.Text text])
    ["*_???","old_???_*","tag_abc";"[a-c]*","*[a-c]","aé猫";
     "a\\*","b","a*";"*","x*","";"a*","x*","unmatched";
     "é*","猫*","é😀";"*","x","name";"[","x","name";"a b","x","a";
     "^a","x","a";"a","x\\","a"];
  List.iter(text_operation "text/glob_rewrite" ["pattern";"replacement";"text"])
    [[E.Int 0;E.Text "x";E.Text "a"];[E.Text "a";E.Int 0;E.Text "a"];[E.Text "a";E.Text "x";E.Int 0]];
  List.iter(fun(pattern,replacement,text)->text_operation "text/glob_rewrite_set"
    ["pattern";"replacement";"text"] [E.Text pattern;E.Text replacement;E.Text text])
    ["a* b*","x* y*","abc";"a* b*","x* y*","bcd";"a* b*","x* y*","zzz";"a*\tb*","x*\ty*","bq";"a*\nb*","x*\ny*","aq";
     "a* *c","x* y*","abc";"*c a*","y* x*","abc";"a\\ b*","x\\ y*","a bz";"a\\ b*","x\\ y*","a_bz";
     "^a* *","x*","ab";"* ^a*","x*","ab";"* ^a*","x*","bb";"a* ^ab*","x*","abc";"a* ^ab*","x*","acd";
     "\195\169* \231\140\171*","x* y*","\195\169\230\151\165";"\195\169* \231\140\171*","x* y*","\231\140\171\195\169";
     "*_??? a*","old_???_* b*","tag_abc";"*_??? a*","old_???_* b*","atag";
     "a* b*","x*","abc";"","","abc";"[","x","abc";"a b","x","a"];
  List.iter(text_operation "text/glob_rewrite_set" ["pattern";"replacement";"text"])
    [[E.Int 0;E.Text "x";E.Text "a"];[E.Text "a";E.Int 0;E.Text "a"];[E.Text "a";E.Text "x";E.Int 0]];
  List.iter(fun(pattern,replacement,text)->text_operation "text/glob_rewrite_set_matches"
    ["pattern";"replacement";"text"] [E.Text pattern;E.Text replacement;E.Text text])
    ["a* b*","x* y*","abc";"a* b*","x* y*","zzz";"^a* b*","y*","ab";"^a* b*","y*","zz";"^a* b*","y*","bb";
     "a\\ b*","x\\ y*","a bz";"a*\tb*","x*\ty*","bq";"[","x","abc";"a b","x","a"];
  List.iter(text_operation "text/glob_rewrite_set_matches" ["pattern";"replacement";"text"])
    [[E.Int 0;E.Text "x";E.Text "a"];[E.Text "a";E.Text "x";E.Int 0]];
  let read geometry name owner kind query =
    let args=["geometry",E.Deferred(T.geometry,0);"attribute",E.Text name;"owner",E.Text owner]
      @(if kind="" then [] else ["type",E.Text kind]) in
    (op(if query then "sop/has_attr" else "sop/attr")).check args;
    let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
    if query then resolve(E.Source("sop/has_attr",T.Bool,args)) else
    if kind="float_array"||kind="int_array" then
      Result.bind(resolve(E.Struct("sop/attr",T.Array T.Int,args@["component",E.Text"offsets"]))) (fun offsets->
      Result.map(fun values->E.Record["offsets",offsets;"values",values])
        (resolve(E.Struct("sop/attr",T.Any,args@["component",E.Text"values"]))))
    else resolve(E.Struct("sop/attr",T.Any,args)) in
  let write geometry name owner kind values =
    (op "sop/with_attr").check ["geometry",E.No_geo;"attribute",E.Text name;
      "owner",E.Text owner;"type",E.Text kind;"values",values];
    Result.bind(Flow_sop.Attribute_kernel.owner owner)(fun owner->
      let input=Sop.Node.Private.make_geometry ~operation:"core_fixture.attribute" ~version:1
        ~parameters:"" ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
        (fun ~node_id:_ _ _->Ok Sop.Node.Private.{geometry;instances=None}) in
      let node=Flow_sop.Attribute_kernel.node ~owner ~kind ~reference:true ~source:"host_fixture"
        ~name ~values ~sources:[0] [input] in
      let session=Sop.Session.create ~max_entries:8 ~max_payload_bytes:1_000_000|>Result.get_ok in
      Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
        let context=Sop.Context.create()|>Result.get_ok in
        match Sop.Session.cook session ~context node with
        |Error d->Error(Diagnostic.error ~code:d.code d.message)
        |Ok output->Ok(geometry_value(Result.get_ok(Sop.Payload.geometry output.payload))))) in
  let checked_read geometry name owner kind =
    let args=["geometry",E.Deferred(T.geometry,0);"attribute",E.Text name;
      "owner",E.Text owner;"type",E.Text kind] in
    (op "sop/attr_checked").check args;
    let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
    Result.bind(resolve(E.Source("sop/attr_checked",T.Any,args@["component",E.Text"values"]))) (fun values->
      Result.map(fun valid->E.Record["values",values;"valid",valid])
        (resolve(E.Source("sop/attr_checked",T.Array T.Int,args@["component",E.Text"valid"])))) in
  let selected_write geometry name owner kind values selection default =
    (op "sop/with_attr").check ["geometry",E.No_geo;"attribute",E.Text name;
      "owner",E.Text owner;"type",E.Text kind;"values",values;"selection",selection;"default",default];
    Result.bind(Flow_sop.Attribute_kernel.owner owner)(fun owner->
      Result.map geometry_value(Flow_sop.Attribute_kernel.write_selected ~grain:97 ~owner ~kind name
        ~values ~selection ~default geometry)) in
  List.iter(fun(owner,count)->List.iter(fun kind->
    let floats width=Array.init(count*width)(fun i->if i mod 2=0 then -0. else float i+.0.25) in
    let values=match kind with
      |"float"->E.Float_array(floats 1)|"vec2"->E.Vec2_array(floats 2)
      |"vec3"->E.Vec3_array(floats 3)|"vec4"->E.Vec4_array(floats 4)
      |"int"->E.Int_array(Array.init count(fun i->if i mod 2=0 then max_int else min_int))
      |"text"->E.Text_array(Array.init count(fun i->[|"";"é日本🐈";"line\n\"quote\""|].(i mod 3)))
      |"float_array"|"int_array"->E.Record["offsets",E.Int_array(Array.init(count+1)(fun i->i*16));
          "values",(if kind="float_array" then E.Float_array(floats 16)
            else E.Int_array(Array.init(count*16)(fun i->max_int-i)))]
      |_->assert false in
    add_host "sop/with_attr" [geometry_value geometry;E.Text"typed";E.Text owner;E.Text kind;values]
      (host_result(fun()->write geometry "typed" owner kind values));
    let bad=match values with
      |E.Record fields->E.Record(("offsets",E.Int_array[|0|])::List.remove_assoc "offsets" fields)
      |E.Int_array _->E.Int_array[||]|Float_array _->E.Float_array[||]
      |Text_array _->E.Text_array[||]
      |Vec2_array _->E.Vec2_array[||]|Vec3_array _->E.Vec3_array[||]|Vec4_array _->E.Vec4_array[||]
      |_->assert false in
    let writes=("typed",bad)::(if kind="vec3" then ["P",values] else []) in
    List.iter(fun(name,values)->add_host "sop/with_attr"
      [geometry_value geometry;E.Text name;E.Text owner;E.Text kind;values]
      (host_result(fun()->write geometry name owner kind values)))writes;
    let fresh=match kind with
      |"float"->Rdk.Attribute.Float(floats 1)|"int"->Int(Array.init count(fun i->max_int-i))
      |"vec2"->Float2(Result.get_ok(Rdk.Packed.Float2.of_owned ~x:(floats 1) ~y:(floats 1)))
      |"vec3"->Float3(Rdk.Packed.Float3.View.of_owned_exn ~x:(floats 1) ~y:(floats 1) ~z:(floats 1))
      |"vec4"->Float4(Result.get_ok(Rdk.Packed.Float4.of_owned ~x:(floats 1) ~y:(floats 1) ~z:(floats 1) ~w:(floats 1)))
      |"float_array"->Float_array(Result.get_ok(Rdk.Packed.Float_array.create_owned
          ~offsets:(Array.init(count+1)(fun i->if i=0 then 0 else 2)) ~values:[|(-0.);0.25|]))
      |"int_array"->Int_array(Result.get_ok(Rdk.Packed.Int_array.create_owned
          ~offsets:(Array.init(count+1)(fun i->if i=0 then 0 else 2)) ~values:[|max_int;min_int|]))
      |"text"->Text(Array.init count(fun i->[|"";"é日本🐈";"line\n\"quote\""|].(i mod 3)))
      |_->assert false in
    let owner_enum=Flow_sop.Attribute_kernel.owner owner|>Result.get_ok in
    let typed=Rdk.Geometry.with_attribute
      (Rdk.Attribute.create_owned ~owner:owner_enum ~name:"typed" fresh|>Result.get_ok)geometry|>Result.get_ok in
    let wrong=if kind="text" then "float" else "text" in
    List.iter(fun name->List.iter(fun requested->
      add_host "sop/attr" [geometry_value typed;E.Text name;E.Text owner;E.Text requested]
        (host_result(fun()->read typed name owner requested false));
      add_host "sop/attr_checked" [geometry_value typed;E.Text name;E.Text owner;E.Text requested]
        (host_result(fun()->checked_read typed name owner requested))) [kind;wrong]) ["typed";"missing"];
    if List.mem kind ["float";"vec2";"vec3";"vec4"] then
      List.iter(fun bad->
        let width=match kind with "float"->1|"vec2"->2|"vec3"->3|_->4 in
        let planes=Array.init width(fun c->Array.init count(fun i->
          if i=0 && c=width-1 then bad else if c=0 then -0. else Float.max_float)) in
        let storage=match kind with
          |"float"->Rdk.Attribute.Float planes.(0)
          |"vec2"->Float2(Result.get_ok(Rdk.Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1)))
          |"vec3"->Float3(Rdk.Packed.Float3.View.of_owned_exn ~x:planes.(0) ~y:planes.(1) ~z:planes.(2))
          |_->Float4(Result.get_ok(Rdk.Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) ~w:planes.(3))) in
        let bad_geometry=Rdk.Geometry.with_attribute
          (Rdk.Attribute.create_owned ~owner:owner_enum ~name:"typed" storage|>Result.get_ok)geometry|>Result.get_ok in
        add_host "sop/attr_checked" [geometry_value bad_geometry;E.Text"typed";E.Text owner;E.Text kind]
          (host_result(fun()->checked_read bad_geometry "typed" owner kind));
        let replacements=Array.init(count*width)(fun i->if i mod width=0 then -0. else float(i mod width)+.0.25) in
        let replacements=match width with 1->E.Float_array replacements|2->Vec2_array replacements|3->Vec3_array replacements|_->Vec4_array replacements in
        let default=match width with 1->E.Float 0.75|2->Vec2(0.75,1.75)|3->Vec3(0.75,1.75,2.75)|_->Vec4(0.75,1.75,2.75,3.75) in
        List.iter(fun geo->List.iter(fun bits->
          let mask=E.Int_array(Array.init count bits) in
          add_host "sop/with_attr" [geometry_value geo;E.Text"typed";E.Text owner;E.Text kind;replacements;mask;default]
            (host_result(fun()->selected_write geo "typed" owner kind replacements mask default)))
          [(fun _->0);(fun _->1);(fun i->i mod 2)]) [bad_geometry;geometry];
        List.iter(fun mask->add_host "sop/with_attr"
          [geometry_value bad_geometry;E.Text"typed";E.Text owner;E.Text kind;replacements;mask;default]
          (host_result(fun()->selected_write bad_geometry "typed" owner kind replacements mask default)))
          [E.Int_array(Array.make(count+1)0);E.Int_array(Array.make count 2)];
        let nonfinite=match width with 1->E.Float nan|2->Vec2(nan,1.)|3->Vec3(nan,1.,1.)|_->Vec4(nan,1.,1.,1.) in
        add_host "sop/with_attr" [geometry_value bad_geometry;E.Text"typed";E.Text owner;E.Text kind;replacements;E.Int_array(Array.make count 0);nonfinite]
          (host_result(fun()->selected_write bad_geometry "typed" owner kind replacements(E.Int_array(Array.make count 0))nonfinite))) [nan;infinity;neg_infinity];
    List.iter(fun name->List.iter(fun requested->
      add_host "sop/has_attr" ([geometry_value typed;E.Text name;E.Text owner]
        @(if requested="" then [] else [E.Text requested]))
        (host_result(fun()->read typed name owner requested true))) [kind;wrong;""]) ["typed";"missing";"P"])
    ["float";"int";"vec2";"vec3";"vec4";"text";"float_array";"int_array"])
    ["point",3;"vertex",3;"primitive",1;"detail",1];
  (* Full planar vector writes use independently packed native storage as the
     valid oracle, including empty owner rows. Invalid cases exercise the same
     direct writer boundary as the JS geometry-host primitive. *)
  let planar_pack planes=match Array.length planes with
    |2->Rdk.Attribute.Float2(Rdk.Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1)|>Result.get_ok)
    |3->Float3(Rdk.Packed.Float3.View.of_owned_exn ~x:planes.(0) ~y:planes.(1) ~z:planes.(2))
    |_->Float4(Rdk.Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) ~w:planes.(3)|>Result.get_ok) in
  let planar_record planes=E.Record(List.mapi(fun c data->[|"x";"y";"z";"w"|].(c),E.Float_array data)(Array.to_list planes)) in
  let planar_add args expected =
    let name="sop/with_attr" in
    let entries=match Hashtbl.find_opt cases name with Some entries->entries|None->
      let entries=ref []in Hashtbl.add cases name entries;entries in
    entries:=Printf.sprintf {|{"static":[],"args":%s,"result":%s,"planar_text_hex":"ff00c3a9"}|}
      (seq raw_json args)(raw_json expected)::!entries in
  let planar_write geo name owner kind values=
    (op "sop/with_attr").check["geometry",E.No_geo;"attribute",E.Text name;"type",E.Text kind;"owner",E.Text owner;"values",values];
    Result.bind(Flow_sop.Attribute_kernel.owner owner)(fun owner->
      Result.map geometry_value(Flow_sop.Attribute_kernel.write ~grain:97 ~owner ~kind name values geo)) in
  let empty=Rdk.Geometry.create
    ~positions:(Rdk.Packed.Float3.View.of_owned_exn ~x:[||] ~y:[||] ~z:[||])
    ~topology:(Rdk.Topology.empty ~point_count:0)()|>Result.get_ok in
  let bytes=Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Detail ~name:"planar_bytes"
    (Rdk.Attribute.Text[|"\255\000é"|])|>Result.get_ok in
  List.iter(fun geo->
    let geo=Rdk.Geometry.with_attribute bytes geo|>Result.get_ok in
    List.iter(fun(owner,count)->List.iter(fun(kind,width)->
    let planes=Array.init width(fun c->Array.init count(fun i->if i mod 2=0 then -0. else float c+.Float.next_after 0. 1.)) in
    let values=planar_record planes and owner_enum=Flow_sop.Attribute_kernel.owner owner|>Result.get_ok in
    let oracle name=if name="P" && owner="point" then match planar_pack(Array.map Array.copy planes)with
      |Rdk.Attribute.Float3 positions->Rdk.Geometry.with_positions positions geo|>Result.get_ok|_->assert false
      else Rdk.Geometry.with_attribute(Rdk.Attribute.create_owned ~owner:owner_enum ~name
        (planar_pack(Array.map Array.copy planes))|>Result.get_ok)geo|>Result.get_ok in
    let valid name values =
      let expected=geometry_value(oracle name) in
      let actual=planar_write geo name owner kind values|>Result.get_ok in
      assert(json actual=json expected);
      planar_add [geometry_value geo;E.Text name;E.Text owner;E.Text kind;values] expected in
    valid "planar" values;
    valid "planar"(match values with E.Record fields->E.Record(List.rev fields)|_->assert false);
    if kind="vec3" || owner<>"point"then valid "P" values;
    let cases=match values with E.Record fields->
      [E.Record(List.tl fields);E.Record(("extra",E.Float_array[||])::fields);
       E.Record(("x",E.Int_array(Array.make count 0))::List.remove_assoc "x" fields);
       E.Record(("x",E.Vec3_array[||])::List.remove_assoc "x" fields);
       E.Record(("x",E.Float_array(Array.make(count+1)nan))::List.remove_assoc "x" fields)]
      @(if count=0 then []else List.map(fun bad->let data=Array.map Array.copy planes in
          data.(width-1).(count-1)<-bad;planar_record data)[nan;infinity;neg_infinity])
      |_->assert false in
    List.iter(fun values->add_host "sop/with_attr"[geometry_value geo;E.Text"planar";E.Text owner;E.Text kind;values]
      (host_result(fun()->planar_write geo "planar" owner kind values)))cases;
    if owner="point" && kind<>"vec3"then add_host "sop/with_attr"
      [geometry_value geo;E.Text"P";E.Text owner;E.Text kind;values](host_result(fun()->planar_write geo "P" owner kind values));
    let default=if width=2 then E.Vec2(0.,0.)else if width=3 then Vec3(0.,0.,0.)else Vec4(0.,0.,0.,0.)in
    add_host "sop/with_attr"[geometry_value geo;E.Text"planar";E.Text owner;E.Text kind;values;E.Int_array(Array.make count 1);default]
      (host_result(fun()->selected_write geo "planar" owner kind values(E.Int_array(Array.make count 1))default)))
      ["vec2",2;"vec3",3;"vec4",4])
      ["point",Rdk.Geometry.point_count geo;"vertex",Rdk.Geometry.vertex_count geo;
       "primitive",Rdk.Geometry.primitive_count geo;"detail",1]) [empty;geometry];
  (* Native String.trim treats NBSP and arbitrary bytes as real names. *)
  List.iter(fun name->
    let values=E.Record["x",E.Float_array[|(-0.);1.;2.|];"y",E.Float_array[|0.;1.;2.|]] in
    add_host "sop/with_attr"[geometry_value geometry;E.Text name;E.Text"point";E.Text"vec2";values]
      (host_result(fun()->planar_write geometry name "point" "vec2" values));
    if String.trim name<>"" then begin
      let geo=Flow_sop.Attribute_kernel.write ~grain:97 ~owner:Rdk.Attribute.Point ~kind:"vec2" name values geometry|>Result.get_ok in
      List.iter(fun query->let operation=if query then "sop/has_attr"else"sop/attr"in
        add_host operation[geometry_value geo;E.Text name;E.Text"point";E.Text"vec2"]
          (host_result(fun()->read geo name "point" "vec2"query))) [false;true];
      add_host "sop/attr_checked"[geometry_value geo;E.Text name;E.Text"point";E.Text"vec2"]
        (host_result(fun()->checked_read geo name "point" "vec2"))
    end)["\194\160";"\255";" \t\r\n"];
  (* Cooked topology sources use the same immutable geometry protocol. *)
  let topology_fixture points vertices offsets kinds =
    let topology=Rdk.Topology.create_owned ~point_count:points ~vertex_points:vertices
      ~primitive_offsets:offsets ~primitive_kinds:kinds |>Result.get_ok in
    let positions=Rdk.Packed.Float3.View.of_owned_exn
      ~x:(Array.init points float) ~y:(Array.make points 0.) ~z:(Array.make points 0.) in
    Rdk.Geometry.create ~positions ~topology () |>Result.get_ok in
  let topology_fixtures = [
    topology_fixture 0 [||] [|0|] [||];
    topology_fixture 5 [|0;1;2;2;1;3|] [|0;3;6|] [|Rdk.Topology.Polygon;Polygon|];
    topology_fixture 3 [|0;1;1;2;1|] [|0;5|] [|Rdk.Topology.Open_polyline|];
    topology_fixture 4 [|3;2;1;0|] [|0;4|] [|Rdk.Topology.Closed_polyline|];
    topology_fixture 6 [|0;1;2;0;1;3;1;0;4;0;1;5|] [|0;3;6;9;12|]
      [|Rdk.Topology.Polygon;Open_polyline;Closed_polyline;Polygon|]] in
  let mesh_fields geometry =
    let topology=Rdk.Topology.View.view(Rdk.Geometry.topology geometry)
    and positions=Rdk.Packed.Float3.View.view(Rdk.Geometry.positions geometry) in
    ["points",E.Vec3_array(Array.init(Array.length positions.x*3)(fun i->
        match i mod 3 with 0->positions.x.(i/3)|1->positions.y.(i/3)|_->positions.z.(i/3)));
      "vertex_points",E.Int_array topology.vertex_points;
      "primitive_offsets",E.Int_array topology.primitive_offsets;
      "kinds",E.Int_array(Array.init(Bytes.length topology.primitive_kinds)
        (fun i->Char.code(Bytes.get topology.primitive_kinds i)))] in
  let mesh ?(planes=false) fields =
    let source = if planes then {|(workspace mesh (graph g :context sop
      [(points : {:x (array float) :y (array float) :z (array float)}
        {:x (array/float 0) :y (array/float 0) :z (array/float 0)})
       (vertex_points : (array int) (array/int 0))
       (primitive_offsets : (array int) (array/int 1)) (kinds : (array int) (array/int 0))]
      (sop/mesh_planes :points points :vertex_points vertex_points
        :primitive_offsets primitive_offsets :kinds kinds)))|}
    else {|(workspace mesh (graph g :context sop
      [(points : (array vec3) (array/vec3 0)) (vertex_points : (array int) (array/int 0))
       (primitive_offsets : (array int) (array/int 1)) (kinds : (array int) (array/int 0))]
      (sop/mesh :points points :vertex_points vertex_points
        :primitive_offsets primitive_offsets :kinds kinds)))|} in
    Result.bind(Flow.Syntax.parse source)(fun forms->
      Result.bind(Flow_sop.Lower.workspace ~inputs:["g",fields] forms)(fun lowered->
        let graph=List.hd lowered.graphs in
        match Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) with
        |Error message->Error(Diagnostic.error ~code:"E_DATA_SOURCE" message)
        |Ok node->
            let session=Sop.Session.create ~max_entries:8 ~max_payload_bytes:1_000_000 |>Result.get_ok in
            Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
              match Sop.Session.cook session ~context:(Sop.Context.create() |>Result.get_ok)node with
              |Error d->Error(Diagnostic.error ?cause:d.cause ~code:d.code d.message)
              |Ok output->Ok(geometry_value(Result.get_ok(Sop.Payload.geometry output.payload)))))) in
  let mesh_case ?(planes=false) fields=add_host (if planes then "sop/mesh_planes" else "sop/mesh")
    (List.map snd fields)(host_result(fun()->mesh ~planes fields)) in
  let mesh_plane_fields geometry=
    let p=Rdk.Packed.Float3.View.view(Rdk.Geometry.positions geometry)in
    ("points",E.Record["x",E.Float_array p.x;"y",E.Float_array p.y;"z",E.Float_array p.z])
      ::List.tl(mesh_fields geometry)in
  List.iter(fun geometry->
    let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
    let handle=E.Deferred(T.geometry,0) in
    List.iter(fun element->add_host "sop/count" [geometry_value geometry;E.Text element]
      (host_result(fun()->
        let args=["geometry",handle;"element",E.Text element] in
        (op "sop/count").check args;
        resolve(E.Source("sop/count",T.Int,args))))) ["points";"vertices";"primitives";"edges";"detail"];
    let edge component=resolve(E.Source("sop/edges",T.Array T.Int,
      ["geometry",handle;"component",E.Text component])) in
    add_host "sop/edges" [geometry_value geometry]
      (host_result(fun()->Result.bind(edge "a")(fun a->Result.map(fun b->E.Record["a",a;"b",b])(edge "b"))));
    let component name=resolve(E.Source("sop/topology",T.Array T.Int,
      ["geometry",handle;"component",E.Text name])) in
    add_host "sop/topology" [geometry_value geometry]
      (host_result(fun()->Result.bind(component "vertex_points")(fun vertices->
        Result.bind(component "primitive_offsets")(fun offsets->
          Result.map(fun kinds->E.Record["vertex_points",vertices;
            "primitive_offsets",offsets;"kinds",kinds])(component "kinds")))));
    add_host "sop/adjacency" [geometry_value geometry]
      (host_result(fun()->
        let args=["geometry",handle] in
        let rec gather acc=function
          |[]->Ok(E.Record(List.rev acc))
          |name::rest->Result.bind(resolve(E.Source("sop/adjacency",T.Array T.Int,
              ("component",E.Text name)::args)))(fun value->gather((name,value)::acc)rest) in
        gather [] ["primitive_of_vertex";"next_vertex";"previous_vertex";"edge_of_vertex";"opposite_vertex";"edge_a";"edge_b";"point_edge_offsets";"point_edges";"point_vertex_offsets";"point_vertices"]));
    let components over mask=
      let tail=("over",E.Text over)::(match mask with Some m->["mask",m]|None->[]) in
      let get component=resolve(E.Source("sop/components",(if component="id" then T.Array T.Int else T.Int),
        ("geometry",handle)::("component",E.Text component)::tail)) in
      add_host "sop/components" (geometry_value geometry::E.Text over::(match mask with Some m->[m]|None->[]))
        (host_result(fun()->Result.bind(get "id")(fun id->Result.map(fun count->E.Record["id",id;"count",count])(get "count")))) in
    List.iter(fun(over,count)->
      components over None;
      components over (Some(E.Int_array(Array.make count 1)));
      components over (Some(E.Int_array(Array.init count (fun i->i land 1))));
      components over (Some(E.Int_array(Array.init count (fun i->if i mod 3=1 then 0 else 1))));
      components over (Some(E.Int_array(Array.make count 0)));
      components over (Some(E.Int_array(Array.make (count+1) 1)));
      components over (Some(E.Int_array(Array.make count 2))))
      ["edges",Rdk.Geometry.point_count geometry;"points",Rdk.Geometry.primitive_count geometry];
    components "faces" None;
    (* E.1-E.3: pure array sources over the adjacency record; the host twin takes
       the same concrete record and arrays. *)
    let adjacency=match
      List.fold_left(fun acc name->Result.bind acc (fun fields->
        Result.map(fun value->(name,value)::fields)(resolve(E.Source("sop/adjacency",T.Array T.Int,
          ["geometry",handle;"component",E.Text name])))))(Ok[])
        ["primitive_of_vertex";"next_vertex";"previous_vertex";"edge_of_vertex";"opposite_vertex";
         "edge_a";"edge_b";"point_edge_offsets";"point_edges";"point_vertex_offsets";"point_vertices"]
    with Ok fields->E.Record(List.rev fields)|Error d->failwith(Diagnostic.to_string d) in
    let points=Rdk.Geometry.point_count geometry
    and edges=Rdk.Topology_index.edge_count(Rdk.Topology_index.create(Rdk.Geometry.topology geometry)) in
    let graph ?positional name ty components args=
      add_host name (match positional with Some p->p|None->List.map snd args)
        (host_result(fun()->
          let get component=resolve(E.Source(name,ty,("component",E.Text component)::args)) in
          let rec gather acc=function
            |[]->Ok(E.Record(List.rev acc))
            |component::rest->Result.bind(get component)(fun value->gather((component,value)::acc)rest) in
          gather [] components)) in
    let paths ?radius lengths roots=
      graph "graph/shortest_paths" T.Any ["distance";"parent";"path";"order"]
        (["adjacency",adjacency;"lengths",lengths;"roots",roots]@(match radius with Some r->["radius",r]|None->[])) in
    (* :hops forms; the JavaScript twin takes the options positionally, null when absent. *)
    let hop_paths ?radius ?(hops=true) ?blocked_points ?blocked_edges ?target ?ties lengths roots=
      let opt name=function Some v->[name,v]|None->[] in
      let named=["adjacency",adjacency;"lengths",lengths;"roots",roots]@opt "radius" radius
        @(if hops then ["hops",E.Bool true] else [])@opt "blocked_points" blocked_points
        @opt "blocked_edges" blocked_edges@opt "target" target@opt "ties" ties in
      let slot=function Some v->v|None->E.No_geo in
      graph "graph/shortest_paths" T.Any ["distance";"parent";"path";"order"] named
        ~positional:[adjacency;lengths;roots;slot radius;E.Bool hops;slot blocked_points;slot blocked_edges;slot target;slot ties] in
    let unit=E.Float_array(Array.make edges 1.) and mixed=E.Float_array(Array.init edges (fun i->float(1+i mod 2)))
    and ramp=E.Float_array(Array.init edges (fun i->0.25*.float i)) in
    List.iter(fun roots->if roots<=points then List.iter(fun lengths->
      paths lengths (E.Int_array(Array.init roots Fun.id)))[unit;mixed;ramp])[0;1;2;5];
    paths ~radius:(E.Float 1.5) unit (E.Int_array(if points>0 then [|0|] else [||]));
    paths ~radius:(E.Float 0.) unit (E.Int_array(if points>0 then [|0;0|] else [||]));
    paths ~radius:(E.Float (-1.)) unit (E.Int_array [||]);
    paths (E.Float_array(Array.make (edges+1) 1.)) (E.Int_array [||]);
    paths (E.Float_array(Array.make edges (-1.))) (E.Int_array [||]);
    if edges>0 then paths (E.Float_array(Array.init edges (fun i->if i=0 then -1. else 1.))) (E.Int_array [||]);
    paths unit (E.Int_array [|points|]);
    paths unit (E.Int_array [|-1|]);
    paths unit (E.Int_array [||]);
    paths unit (E.Int_array(Array.make 3 (-0)));
    if points>0 then hop_paths ~hops:false ~target:(E.Int (points-1)) unit (E.Int_array [|0|]);
    if points>0 then hop_paths ~hops:false ~target:(E.Int 0) mixed (E.Int_array [|0|]);
    hop_paths ~hops:false ~target:(E.Int points) unit (E.Int_array [||]);
    hop_paths ~hops:false ~target:(E.Int (-1)) unit (E.Int_array [||]);
    hop_paths ~hops:false ~target:(E.Float 0.) unit (E.Int_array [||]);
    List.iter(fun lengths->
      List.iter(fun roots->if roots<=points then begin
        hop_paths lengths (E.Int_array(Array.init roots Fun.id));
        if points>0 then List.iter(fun target->
          hop_paths ~target:(E.Int target) lengths (E.Int_array(Array.init roots Fun.id));
          hop_paths ~target:(E.Int target) ~blocked_points:(E.Int_array(Array.init points (fun i->i land 1)))
            lengths (E.Int_array(Array.init roots Fun.id));
          hop_paths ~target:(E.Int target) ~blocked_edges:(E.Int_array(Array.init edges (fun i->if i mod 3=1 then 1 else 0)))
            lengths (E.Int_array(Array.init roots Fun.id));
          hop_paths ~blocked_points:(E.Int_array(Array.init points (fun i->if i mod 3=0 then 1 else 0)))
            ~blocked_edges:(E.Int_array(Array.init edges (fun i->i land 1))) lengths (E.Int_array(Array.init roots Fun.id)))
          [0;points-1;points/2]
      end)[0;1;2])[unit;mixed;ramp];
    hop_paths ~hops:false ~blocked_points:(E.Int_array(Array.make points 0)) unit (E.Int_array [||]);
    hop_paths ~radius:(E.Float 1.) unit (E.Int_array [||]);
    hop_paths ~blocked_points:(E.Int_array(Array.make (points+1) 0)) unit (E.Int_array [||]);
    hop_paths ~blocked_points:(E.Int_array(Array.make points 2)) unit (E.Int_array [||]);
    hop_paths ~blocked_edges:(E.Int_array(Array.make (edges+1) 0)) unit (E.Int_array [||]);
    hop_paths ~target:(E.Int points) unit (E.Int_array [||]);
    hop_paths ~target:(E.Float 1.) unit (E.Int_array [||]);
    hop_paths unit (E.Int_array [|points|]);
    let lowest=E.Text "lowest" in
    let zeros=E.Float_array(Array.init edges (fun i->float(i mod 3))) in
    List.iter(fun lengths->
      List.iter(fun roots->if roots<=points then begin
        hop_paths ~hops:false ~ties:lowest lengths (E.Int_array(Array.init roots Fun.id));
        if points>0 then List.iter(fun target->
          hop_paths ~hops:false ~ties:lowest ~target:(E.Int target) lengths (E.Int_array(Array.init roots Fun.id));
          hop_paths ~hops:false ~ties:lowest ~target:(E.Int target)
            ~blocked_edges:(E.Int_array(Array.init edges (fun i->if i mod 3=1 then 1 else 0)))
            lengths (E.Int_array(Array.init roots Fun.id)))
          [0;points-1;points/2]
      end)[0;1;2;3])[unit;mixed;zeros];
    hop_paths ~hops:false ~ties:(E.Text "first") unit (E.Int_array [|0|]);
    hop_paths ~hops:false ~ties:(E.Text "other") unit (E.Int_array [||]);
    hop_paths ~hops:false ~ties:lowest ~radius:(E.Float 1.) unit (E.Int_array [||]);
    hop_paths ~hops:false ~ties:lowest ~blocked_points:(E.Int_array(Array.make points 0)) unit (E.Int_array [||]);
    hop_paths ~ties:lowest unit (E.Int_array [||]);
    let forest ?mask parents=
      graph "graph/forest" T.Any ["order";"depth";"root";"offsets";"children"]
        (["parents",parents]@(match mask with Some m->["mask",m]|None->[]))
        ~positional:[parents;(match mask with Some m->m|None->E.No_geo)] in
    List.iter(fun parents->
      let n=Array.length parents in
      forest (E.Int_array parents);
      forest ~mask:(E.Int_array(Array.make n 1)) (E.Int_array parents);
      forest ~mask:(E.Int_array(Array.init n (fun i->i land 1))) (E.Int_array parents);
      forest ~mask:(E.Int_array(Array.init n (fun i->if i mod 3=1 then 0 else 1))) (E.Int_array parents);
      forest ~mask:(E.Int_array(Array.make n 0)) (E.Int_array parents))
      [[||];[|0|];[|-1|];[|-1;0;0;1;1;2|];[|5;0;1;2;3;4|];[|1;0;1;9;-4;4|];[|-1;-1;1;1;3;2;2;7|];[|3;3;3;3;0;4;4;5|]];
    forest ~mask:(E.Int_array [|1|]) (E.Int_array [|0;0|]);
    forest ~mask:(E.Int_array [|2;0|]) (E.Int_array [|0;0|]);
    forest (E.Float_array [|0.|]);
    let chains mask=graph "graph/chains" T.Any ["offsets";"values";"closed"]
      (["adjacency",adjacency]@(match mask with Some m->["mask",m]|None->[])) in
    chains None;
    chains (Some(E.Int_array(Array.make edges 1)));
    chains (Some(E.Int_array(Array.init edges (fun i->i land 1))));
    chains (Some(E.Int_array(Array.init edges (fun i->if i mod 3=1 then 0 else 1))));
    chains (Some(E.Int_array(Array.make edges 0)));
    chains (Some(E.Int_array(Array.make (edges+1) 1)));
    chains (Some(E.Int_array(Array.make edges 2)));
    let rows_of lists=
      let offsets=Array.make (Array.length lists+1) 0 in
      Array.iteri(fun i l->offsets.(i+1)<-offsets.(i)+List.length l)lists;
      E.Record["offsets",E.Int_array offsets;"values",E.Int_array(Array.of_list(List.concat(Array.to_list lists)))] in
    let color rows order=graph "graph/color" T.Any ["color";"count"]["adjacency",rows;"order",order] in
    List.iter(fun n->
      let path=rows_of(Array.init n (fun i->List.filter(fun j->j>=0&&j<n)[i-1;i+1]))
      and clique=rows_of(Array.init n (fun i->List.filter((<>)i)(List.init n Fun.id)))
      and ring=rows_of(Array.init n (fun i->[(i+n-1) mod n;(i+1) mod n])) in
      List.iter(fun rows->
        color rows (E.Int_array(Array.init n Fun.id));
        color rows (E.Int_array(Array.init n (fun i->n-1-i)));
        color rows (E.Int_array(Array.init (n/2) (fun i->2*i)))) [path;clique;ring])[0;1;2;3;6;11];
    color (rows_of [|[0];[0]|]) (E.Int_array [|0;1|]);
    color (rows_of [|[1];[0]|]) (E.Int_array [|1;0;1|]);
    color (rows_of [|[1];[0]|]) (E.Int_array [|2|]);
    color (rows_of [|[1];[0]|]) (E.Int_array [|-1|]);
    color (E.Record["offsets",E.Int_array [|0;1|];"values",E.Int_array [|5|]]) (E.Int_array [|0|]);
    color (E.Record["offsets",E.Int_array [|1;1|];"values",E.Int_array [||]]) (E.Int_array [||]);
    color (E.Record["offsets",E.Int_array [|0;2;1|];"values",E.Int_array [|0;0|]]) (E.Int_array [||]);
    color (E.Record["offsets",E.Int_array [||];"values",E.Int_array [||]]) (E.Int_array [||]);
    mesh_case(mesh_fields geometry);
    mesh_case ~planes:true(mesh_plane_fields geometry))
    topology_fixtures;
  let base=mesh_fields(List.nth topology_fixtures 1) in
  List.iter(fun(key,value)->mesh_case(List.map(fun(name,old)->
    name,if name=key then value else old)base))
    ["primitive_offsets",E.Int_array[||];"primitive_offsets",E.Int_array[|1;3;6|];
      "primitive_offsets",E.Int_array[|0;3;5|];"kinds",E.Int_array[|0|];
      "vertex_points",E.Int_array[|0;1;2;2;1;9007199254740993|];
      "primitive_offsets",E.Int_array[|0;7;6|];"primitive_offsets",E.Int_array[|0;2;6|];
      "kinds",E.Int_array[|-1;0|];"kinds",E.Int_array[|3;0|];
      "kinds",E.Int_array[|9007199254740993;0|];
      "points",E.Vec3_array[|1.|];
      "points",E.Vec3_array[|nan;0.;0.|];
      "points",E.Vec3_array[|infinity;0.;0.|];
      "points",E.Vec3_array[|neg_infinity;0.;0.|]];
  let plane_base=mesh_plane_fields(List.nth topology_fixtures 1)in
  List.iter(fun(key,value)->mesh_case ~planes:true(List.map(fun(name,old)->
    name,if name=key then value else old)plane_base))
    ["primitive_offsets",E.Int_array[||];"primitive_offsets",E.Int_array[|1;3;6|];
     "primitive_offsets",E.Int_array[|0;3;5|];"kinds",E.Int_array[|0|];
     "vertex_points",E.Int_array[|0;1;2;2;1;9007199254740993|];
     "primitive_offsets",E.Int_array[|0;7;6|];"primitive_offsets",E.Int_array[|0;2;6|];
     "kinds",E.Int_array[|-1;0|];"kinds",E.Int_array[|3;0|];
     "points",E.Record["x",E.Float_array[||];"y",E.Float_array[|0.|];"z",E.Float_array[||]];
     "points",E.Record["x",E.Float_array[||];"y",E.Float_array[|nan|];"z",E.Float_array[||]]];
  List.iter(fun axis->List.iter(fun bad->
    let p=Rdk.Packed.Float3.View.view(Rdk.Geometry.positions(List.nth topology_fixtures 1))in
    let coordinates=E.Record(List.map(fun(name,values)->let values=Array.copy values in
      if name=axis then values.(0)<-bad;name,E.Float_array values)["x",p.x;"y",p.y;"z",p.z])in
    mesh_case ~planes:true(("points",coordinates)::List.tl plane_base)) [nan;infinity;neg_infinity]) ["x";"y";"z"];
  List.iter(fun name->let args=["geometry",E.No_geo] @
      (if name="sop/count" then ["element",E.Text"points"] else ["component",E.Text"a"]) in
    add_host name (E.No_geo::(if name="sop/count" then [E.Text"points"] else []))
      (host_result(fun()->Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None)
        (E.Source(name,(if name="sop/count" then T.Int else T.Array T.Int),args))))) ["sop/count";"sop/edges";"sop/topology"];
  (* Group host fixtures execute the production lowering for mask writes. *)
  let group_write geometry owner name mask order =
    let base=op "sop/with_group" in
    let rec payload : type f r. E.value -> (f,r) Flow.Value.t = function
      | E.Int_array values -> Flow.Value.Int_array values
      | E.Bool value -> Flow.Value.Bool value
      | E.Record fields -> Flow.Value.Record(List.map(fun(name,value)->name,payload value)fields)
      | _ -> assert false in
    let constant name ty value={base with Flow.Op.name=name;ctx=Flow.Context.value;
      signature={pos=[];opt=[];rest=None;kw=[]};out=(fun _->ty);out_args=None;
      choices=[];check=(fun _->());body=(fun ~live:_ ~node:_ _->payload value)} in
    let input=Sop.Node.Private.make_geometry ~operation:"core_fixture.groups" ~version:1
      ~parameters:"" ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _->Ok Sop.Node.Private.{geometry;instances=None}) in
    let factory=Sop.Edit_graph.factory ~key:"fixture" ~label:"Fixture" ~category:["Test"] ~arity:0 (fun _->Ok input) in
    let geometry_op={base with Flow.Op.name="sop/fixture";
      signature={pos=[];opt=[];rest=None;kw=[]};out_args=None;choices=[];check=(fun _->());
      body=(fun ~live:_ ~node _->node "sop/fixture" [])} in
    let order_ops=Option.to_list(Option.map(fun order->constant "fixture/order"(Flow.Value.ty_of order)order)order) in
    let forms=Flow.Syntax.parse("(workspace groups (graph g :context sop (sop/with_group (sop/fixture) "^
      "(fixture/mask) :name "^Flow.Lisp.flat(Flow.Syntax.make(Str name))^" :owner "^owner^
      (if order=None then "" else " :order (fixture/order)")^")))") in
    Result.bind forms(fun forms->
      match Flow.Workspace.check ~ops:(geometry_op::constant "fixture/mask"(Flow.Value.ty_of mask)mask::order_ops @Flow_sop.Operators.all)
        {Flow.Catalog.version=1;kinds=[]} forms with
      | None,diagnostics->Error(List.find(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)diagnostics)
      | Some checked,_->Result.bind(Flow_sop.Lower.of_checked ~factories:[factory] checked)(fun lowered->
          let graph=List.hd lowered.graphs in
          match Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) with
          | Error message->Error(Diagnostic.error ~code:"E_DATA_SOURCE" message)
          | Ok node->
              let session=Sop.Session.create ~max_entries:8 ~max_payload_bytes:1_000_000 |>Result.get_ok in
              Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
                match Sop.Session.cook session ~context:(Sop.Context.create() |>Result.get_ok)node with
                | Error d->Error(Diagnostic.error ~code:d.code d.message)
                | Ok output->Ok(geometry_value(Result.get_ok(Sop.Payload.geometry output.payload)))))) in
  List.iter(fun geometry->List.iter(fun(owner,count,native_owner)->
    let mask=Array.init count(fun i->if i mod 2=0 then 1 else 0) in
    let names=["wall_a";"wall_b";"wall_1";"floor";"literal*";"žarek";"\194\160";"\227\128\128"] in
    let grouped=List.fold_left(fun geometry name->match native_owner with
      | Some owner->Rdk.Geometry.with_group(Rdk.Group.init ~owner ~name count(fun i->mask.(i)=1))geometry |>Result.get_ok
      | None->let topology=Rdk.Geometry.topology geometry in
          Rdk.Geometry.with_edge_group(Rdk.Edge_group.init ~topology ~index:(Rdk.Topology_index.create topology)
            ~name(fun i->mask.(i)=1))geometry |>Result.get_ok)geometry names in
    let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some grouped) in
    let args name=["geometry",E.Deferred(T.geometry,0);"owner",E.Text owner;"name",E.Text name] in
    List.iter(fun name->
      add_host "sop/group" [geometry_value grouped;E.Text name;E.Text owner]
        (host_result(fun()->resolve(E.Source("sop/group",T.Array T.Int,args name))));
      List.iter(fun target->add_host "sop/group"
        [geometry_value grouped;E.Text name;E.Text owner;target]
        (host_result(fun()->resolve(E.Source("sop/group",T.Array T.Int,
          args name@["as_owner",target])))))
        [E.Text"point";E.Text"edge";E.Int 1];
      add_host "sop/group_order" [geometry_value grouped;E.Text name;E.Text owner]
        (host_result(fun()->Result.bind(resolve(E.Source("sop/group_order",T.Bool,args name@["component",E.Text"ordered"])))
          (fun ordered->Result.map(fun indices->E.Record["ordered",ordered;"indices",indices])
            (resolve(E.Source("sop/group_order",T.Array T.Int,args name@["component",E.Text"indices"])))))))
      ["wall_a";"missing";"\194\160";"\227\128\128";" \t\r\n\012"];
    List.iter(fun(name,owner_value)->
      add_host "sop/group" [geometry_value grouped;name;owner_value;E.Text"point"]
        (host_result(fun()->resolve(E.Source("sop/group",T.Array T.Int,
          ["geometry",E.Deferred(T.geometry,0);"name",name;"owner",owner_value;"as_owner",E.Text"point"])))))
      [E.Int 1,E.Text owner;E.Text"wall_a",E.Int 1;E.Text"",E.Int 1];
    add_host "sop/group" [E.No_geo;E.Text"";E.Text owner;E.Text"point"]
      (host_result(fun()->resolve(E.Source("sop/group",T.Array T.Int,
        ["geometry",E.No_geo;"name",E.Text"";"owner",E.Text owner;"as_owner",E.Text"point"]))));
    List.iter(fun pattern->add_host "sop/group_names" [geometry_value grouped;E.Text pattern;E.Text owner]
      (host_result(fun()->resolve(E.Source("sop/group_names",T.Array T.Text,
        ["geometry",E.Deferred(T.geometry,0);"owner",E.Text owner;"pattern",E.Text pattern])))))
      ["";"*";"wall*";"wall_[a-b]";"wall_[!0-9]";"* ^wall*";"^wall* wall_b";"literal\\*";"ž*";"[";"\\";"[z-a]"];
    let selected=Array.to_list(Array.mapi(fun i value->i,value)mask) |>List.filter_map(fun(i,value)->if value=1 then Some i else None)
      |>List.rev |>Array.of_list in
    List.iter(fun(mask,order)->add_host "sop/with_group"
      ([geometry_value grouped;mask;E.Text "wall_a";E.Text owner]@Option.to_list order)
      (host_result(fun()->group_write grouped owner "wall_a" mask order)))
      [E.Int_array mask,None;E.Int_array(Array.make count 0),Some(E.Record["ordered",E.Bool false;"indices",E.Int_array[||]]);
       E.Int_array mask,Some(E.Int_array selected);
       E.Int_array mask,Some(E.Record["ordered",E.Bool true;"indices",E.Int_array selected]);
       E.Int_array(Array.make count 0),Some(E.Int_array[||]);
       E.Int_array(Array.make(count+1)0),None;E.Int_array(Array.make count 2),None;
       E.Int_array mask,Some(E.Int_array[|count|]);E.Int_array mask,Some(E.Int_array[|0;0|])];
    Option.iter(fun native_owner->List.iter(fun indices->
      let geometry=Rdk.Geometry.with_group(Rdk.Group.ordered ~owner:native_owner ~name:"wall_a" ~length:count indices
        |>Result.get_ok)grouped |>Result.get_ok in
      add_host "sop/group_order" [geometry_value geometry;E.Text "wall_a";E.Text owner]
        (json(E.Record["ordered",E.Bool true;"indices",E.Int_array indices]))) [selected;[||]])native_owner)
    ["point",Rdk.Geometry.point_count geometry,Some Rdk.Group.Point;
     "vertex",Rdk.Geometry.vertex_count geometry,Some Vertex;
     "primitive",Rdk.Geometry.primitive_count geometry,Some Primitive;
     "edge",Rdk.Topology_index.edge_count(Rdk.Topology_index.create(Rdk.Geometry.topology geometry)),None])
    topology_fixtures;
  List.iter(fun name->let selector=if name="sop/group_names" then "pattern" else "name" in
    let args=["geometry",E.No_geo;selector,E.Text"selected";"component",E.Text"ordered"] in
    add_host name [E.No_geo;E.Text"selected"]
      (host_result(fun()->Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None)
        (E.Source(name,T.Any,args))))) ["sop/group";"sop/group_names";"sop/group_order"];
  (* Metadata mutations preserve immutable payloads, owner identity and store order. *)
  let metadata_geometry=
    let ordinary owner name indices=Rdk.Group.ordered ~owner ~name ~length:(match owner with
      Rdk.Group.Point->3|Vertex->3|Primitive->1) indices |>Result.get_ok in
    let geometry=List.fold_left(fun geometry group->Rdk.Geometry.with_group group geometry |>Result.get_ok) geometry
      [ordinary Point "wall_a" [|2;0|];ordinary Point "wall_b" [|1;0|];
       ordinary Vertex "wall_a" [|1|];ordinary Primitive "wall_a" [|0|];ordinary Point "empty" [||]] in
    let topology=Rdk.Geometry.topology geometry in
    List.fold_left(fun geometry name->Rdk.Geometry.with_edge_group
      (Rdk.Edge_group.init ~topology ~index:(Rdk.Topology_index.create topology) ~name (fun i->i=0)) geometry |>Result.get_ok)
      geometry ["wall_a";"wall_b";"literal*"] in
  let entries geometry pattern owner =
    let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some geometry) in
    let column component=resolve(E.Source("sop/group_entries",T.Array T.Text,
      ["geometry",E.Deferred(T.geometry,0);"pattern",pattern;"owner",owner;"component",E.Text component])) in
    Result.bind(column "owners")(fun owners->Result.map(fun names->E.Record["owners",owners;"names",names])(column "names")) in
  List.iter(fun owner->List.iter(fun pattern->add_host "sop/group_entries"
    [geometry_value metadata_geometry;E.Text pattern;E.Text owner]
    (host_result(fun()->entries metadata_geometry(E.Text pattern)(E.Text owner))))
    ["";"*";"wall*";"* ^wall_a";"literal\\*";"missing";"[";"\\";"[z-a]"])
    ["any";"point";"vertex";"primitive";"edge";" Points ";"invalid"];
  List.iter(fun(pattern,owner)->add_host "sop/group_entries"
    [geometry_value metadata_geometry;pattern;owner]
    (host_result(fun()->entries metadata_geometry pattern owner)))
    [E.Int 0,E.Text "any";E.Text "*",E.Int 0];
  add_host "sop/group_entries" [E.No_geo;E.Text "*"]
    (host_result(fun()->Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->None)
      (E.Source("sop/group_entries",T.Array T.Text,["geometry",E.No_geo;"pattern",E.Text"*";"component",E.Text"names"]))));
  let large_inventory=Rdk.Geometry.create ~positions:(Rdk.Geometry.positions geometry)
    ~topology:(Rdk.Geometry.topology geometry)
    ~groups:(List.init 5001(fun i->Rdk.Group.init ~owner:Rdk.Group.Point ~name:("entry_"^string_of_int i) 3(fun _->false)))()
    |>Result.get_ok in
  add_host "sop/group_entries" [geometry_value large_inventory;E.Text "*";E.Text "any"]
    (host_result(fun()->entries large_inventory(E.Text "*")(E.Text "any")));
  let metadata_result name args native = host_result(fun()->
    (op name).check (("geometry",E.No_geo)::args);
    Result.map geometry_value (Result.map_error(fun error->Diagnostic.error ~code:(Rdk.Error.code error)
      (Rdk.Error.operation error^" could not produce valid geometry")) (native()))) in
  let owners=["point",Some Rdk.Attribute.Point;"vertex",Some Vertex;"primitive",Some Primitive;
    "detail",Some Detail;"any",None] in
  List.iter(fun name->
    let args=["pattern",E.Text "missing";"replacement",E.Text "renamed";
      "owner",E.Text "any";"conflict",E.Text "sKip"] in
    add_host name [geometry_value metadata_geometry;E.Text "missing";E.Text "renamed";E.Text "any";E.Text "sKip"]
      (host_result(fun()->(op name).check (("geometry",E.No_geo)::args);Ok E.No_geo)))
    ["sop/rename_attr";"sop/rename_group"];
  List.iter(fun(owner,selected)->
    List.iter(fun name->add_host "sop/attr_type" [geometry_value metadata_geometry;E.Text name;E.Text owner]
      (host_result(fun()->let args=["geometry",E.Deferred(T.geometry,0);"attribute",E.Text name;"owner",E.Text owner] in
        (op "sop/attr_type").check args;
        Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some metadata_geometry)(E.Source("sop/attr_type",T.Text,args)))))
      ["P";"N";"missing";"missing é"];
    List.iter(fun pattern->add_host "sop/attr_names" [geometry_value metadata_geometry;E.Text pattern;E.Text owner]
      (host_result(fun()->let args=["geometry",E.Deferred(T.geometry,0);"pattern",E.Text pattern;"owner",E.Text owner] in
        (op "sop/attr_names").check args;
        Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some metadata_geometry)(E.Source("sop/attr_names",T.Array T.Text,args)))))
      ["";"*";"mask_* ^mask_A";"??";"[";"\\"];
    add_host "sop/attr_entries" [geometry_value metadata_geometry;E.Text owner]
      (host_result(fun()->let args=["geometry",E.Deferred(T.geometry,0);"owner",E.Text owner] in
        (op "sop/attr_entries").check args;
        let resolve=Flow_sop.Attribute_kernel.resolve ~geometry:(fun _->Some metadata_geometry) in
        let column component=resolve(E.Source("sop/attr_entries",T.Array T.Text,args@["component",E.Text component])) in
        Result.bind(column "name")(fun name->Result.bind(column "type")(fun ty->
          Result.map(fun owners->E.Record["name",name;"type",ty;"owner",owners])(column "owner")))));
    List.iter(fun(pattern,replacement)->List.iter(fun(conflict,policy)->
      add_host "sop/rename_attr" [geometry_value metadata_geometry;E.Text pattern;E.Text replacement;E.Text owner;E.Text conflict]
        (metadata_result "sop/rename_attr" ["pattern",E.Text pattern;"replacement",E.Text replacement;"owner",E.Text owner;"conflict",E.Text conflict]
          (fun()->Rdk.Attribute_ops.rename ~rules:[{rename_attribute_owner=selected;rename_attribute_pattern=pattern;
            rename_attribute_replacement=replacement;rename_attribute_conflict=policy}] metadata_geometry)))
      ["skip",Rdk.Attribute_lifecycle.Attribute_rename_skip;"error",Attribute_rename_error;"overwrite",Attribute_rename_overwrite])
      ["N","Cd";"mask_*","new_*";"mask_?","new_?";"mask_[1-2]","new_[1-2]";
       "missing","new";"N","P";"a\\*","literal";"*?","?*";"N Cd","x";"*","x";"[","x"];
    Option.iter(fun selected->List.iter(fun(source,destination)->List.iter(fun(method_,method_enum)->
      add_host "sop/swap_attr" [geometry_value metadata_geometry;E.Text source;E.Text destination;E.Text owner;E.Text method_]
        (metadata_result "sop/swap_attr" ["source",E.Text source;"destination",E.Text destination;"owner",E.Text owner;"method",E.Text method_]
          (fun()->Rdk.Attribute_ops.swap ~rules:[{swap_attribute_owner=selected;swap_attribute_source=source;
            swap_attribute_destination=destination;swap_attribute_method=method_enum}] metadata_geometry)))
      ["swap",Rdk.Attribute_lifecycle.Attribute_swap;"move",Attribute_move;"copy",Attribute_copy])
      ["N","Cd";"N","new";"missing","Cd";"missing","absent";"P","new";"N","P";
       "mask_*","copy_*";"N","N";"[","x"]) selected) owners;
  List.iter(fun reference->List.iter(fun inverse->List.iter(fun point->
    let reference_value=match reference with None->E.No_geo|Some geometry->geometry_value geometry in
    add_host "sop/delete_attrs" [geometry_value metadata_geometry;reference_value;E.Bool inverse;E.Text point;E.Text"";E.Text"N";E.Text"*"]
      (metadata_result "sop/delete_attrs" ["inverse",E.Bool inverse;"point",E.Text point;"vertex",E.Text"";
        "primitive",E.Text"N";"detail",E.Text"*"]
        (fun()->Rdk.Attribute_ops.delete ?reference ~delete_non_selected:inverse
          ~point_pattern:point ~vertex_pattern:"" ~primitive_pattern:"N" ~detail_pattern:"*" metadata_geometry)))
    ["";"*";"^N";"mask_* ^mask_1";"["]) [false;true]) [None;Some metadata_geometry];
  List.iter(fun(owner,selected)->
    List.iter(fun(pattern,replacement)->List.iter(fun(conflict,policy)->
      add_host "sop/rename_group" [geometry_value metadata_geometry;E.Text pattern;E.Text replacement;E.Text owner;E.Text conflict]
        (metadata_result "sop/rename_group" ["pattern",E.Text pattern;"replacement",E.Text replacement;"owner",E.Text owner;"conflict",E.Text conflict]
          (fun()->Rdk.Group_ops.rename ~rules:[{rename_owner=selected;rename_pattern=pattern;
            rename_replacement=replacement;rename_conflict=policy}] metadata_geometry)))
      ["skip",Rdk.Group_ops.Rename_skip;"error",Rename_error;"overwrite",Rename_overwrite;"union",Rename_union])
      ["wall_a","wall_b";"wall_*","new_*";"wall_?","new_?";"missing","absent";"literal\\*","literal";"*","x"];
    List.iter(fun pattern->List.iter(fun unused->add_host "sop/delete_group"
      [geometry_value metadata_geometry;E.Text pattern;E.Text owner;E.Bool unused]
      (metadata_result "sop/delete_group" ["pattern",E.Text pattern;"owner",E.Text owner;"unused",E.Bool unused]
        (fun()->Rdk.Group_ops.delete ~rules:[{delete_owner=selected;delete_pattern=pattern}]
          ~delete_unused:unused metadata_geometry))) [false;true]) ["";"wall*";"^wall_a";"*"])
    ["point",Some Rdk.Group_ops.Group_points;"vertex",Some Group_vertices;
      "primitive",Some Group_primitives;"edge",Some Group_edges;"any",None];
  List.iter(fun(from,into)->add_host "sop/rename_edge" [geometry_value metadata_geometry;E.Text from;E.Text into]
    (host_result(fun()->try Result.map geometry_value(Result.map_error(fun _->Diagnostic.error
      ~code:"rename_edge_group_failed" "rename_edge_group could not produce valid geometry")
      (Rdk.Geometry.rename_edge_group ~from ~into metadata_geometry))
      with Invalid_argument _->Error(Diagnostic.error ~code:"uncaught_node_exception"
        "a procedural node raised an exception while cooking"))))
    ["wall_a","new";"wall_a","wall_b";"wall_a","wall_a";"missing","missing";"literal*","new*";"wall_a"," "];
  List.iter(fun name->add_host "sop/delete_edge" [geometry_value metadata_geometry;E.Text name]
    (json(geometry_value(Rdk.Geometry.without_edge_group name metadata_geometry)))) ["wall_a";"missing";"*";""];
(* This adapter is fixture plumbing, not an authored operator: its grain is
   supplied by the same private host context used by native diagnosis. *)
let diagnostic_json(d:Diagnostic.t)=Printf.sprintf
  {|{"error":{"code":%s,"message":%s%s}}|}(str d.code)(str d.message)
  (Option.fold ~none:"" ~some:(fun cause->",\"cause\":"^str cause)d.cause) in
let diagnose rows grain =
  try json(Flow.Value.diagnose_rows rows
    ~select:(fun rows _->Option.get(Flow.Value.error_row_index ~grain rows))
    ~map:(function E.Int index->E.Record["code",E.Text"E_ROW";
        "message",E.Text("bad "^string_of_int index);
        "cause",E.List[|E.Text("raw 日本😀 "^string_of_int index)|]]|_->assert false))
  with Diagnostic.Error d -> diagnostic_json d in
List.iter(fun bad->
  let count=32769 in
  let x=Array.init count(fun index->if List.mem index bad then Float.nan else -0.)
  and y=Array.make count 0. and z=Array.make count 0. in
  (* Only x is invalid where flagged; keep every other row wholly finite. *)
  Array.fill y 0 count 0.;
  let directions=Rdk.Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let positions=Rdk.Packed.Float3.View.of_owned_exn
    ~x:(Array.make count 0.) ~y:(Array.make count 0.) ~z:(Array.make count 0.) in
  let topology=Rdk.Topology.empty ~point_count:count in
  let geometry=Rdk.Geometry.create ~positions ~topology()|>Result.get_ok in
  let geometry=Rdk.Geometry.with_attribute
    (Rdk.Attribute.create_owned ~name:"raw" ~owner:Point(Float3 directions)|>Result.get_ok)
    geometry|>Result.get_ok in
  let validity=match Result.get_ok(checked_read geometry "raw" "point" "vec3")with
    |E.Record fields->(match List.assoc "valid" fields with E.Int_array rows->rows|_->assert false)
    |_->assert false in
  let flags=E.Int_array(Array.map(fun valid->1-valid)validity)in
  List.iter(fun grain->add_host "$host/diagnostic_rows"
    [geometry_value geometry;E.Record["grain",E.Int grain]](diagnose flags grain)) [1;97;4096])
  [[];[0;1;96;97;98];[96;97;98];[4095;4096;4097];[32768]];
  let words=[|0L;Int64.min_int;1L;0x7ff0000000000000L;0xfff0000000000000L;0x7ff8000000000011L;0x7ff8000000000022L|] in
  let raw i=Int64.float_of_bits words.(i mod Array.length words) in
  let remap_source count =
    let open Rdk in
    let get_string_ok = Result.get_ok in
    let topology=if count<3 then Topology.empty ~point_count:count else
      Topology.create_owned ~point_count:count ~vertex_points:[|0;1;2|]
        ~primitive_offsets:[|0;3|] ~primitive_kinds:[|Topology.Polygon|] |>get_string_ok in
    let attrs=List.concat_map(fun owner->let n=match owner with Attribute.Point->count
        |Vertex->Topology.vertex_count topology|Primitive->Topology.primitive_count topology|Detail->1 in
      let floats k=Array.init n(fun i->raw(i+k)) in
      let offsets=Array.init(n+1)(fun i->i*16) in
      let kinds=["float",Attribute.Float(floats 0);"int",Int(Array.init n(fun i->(1 lsl 55)+i));
        "text",Text(Array.init n(fun i->if i mod 2=0 then "猫\255" else "😀\000"));
        "vec2",Float2(Packed.Float2.of_owned ~x:(floats 1) ~y:(floats 2)|>get_string_ok);
        "vec3",Float3(Packed.Float3.View.of_owned_exn ~x:(floats 3) ~y:(floats 4) ~z:(floats 5));
        "vec4",Float4(Packed.Float4.of_owned ~x:(floats 2) ~y:(floats 3) ~z:(floats 4) ~w:(floats 5)|>get_string_ok);
        "mat16",Float_array(Packed.Float_array.create_owned ~offsets
          ~values:(Array.init(n*16)raw)|>get_string_ok);
        "ragged_float",Float_array(Packed.Float_array.create_owned
          ~offsets:(Array.init(n+1)(fun i->i/2)) ~values:(Array.init(n/2)raw)|>get_string_ok);
        "ragged",Int_array(Packed.Int_array.create_owned
          ~offsets:(Array.init(n+1)(fun i->3*(i/3)+(i mod 3)*(i mod 3-1)/2))
          ~values:(Array.init(3*(n/3)+(n mod 3)*(n mod 3-1)/2)(fun i->(1 lsl 55)+i))|>get_string_ok)] in
      List.map(fun(name,storage)->Attribute.create_owned ~name ~owner storage |>get_string_ok)kinds)
      [Attribute.Point;Vertex;Primitive;Detail] in
    let groups=List.concat_map(fun owner->let n=match owner with Group.Point->count
        |Vertex->Topology.vertex_count topology|Primitive->Topology.primitive_count topology in
      [Group.init ~owner ~name:"plain" n(fun i->i mod 2=0);
       Group.ordered ~owner ~name:"ordered" ~length:n (Array.init n(fun i->n-1-i))|>get_string_ok;
       Group.ordered ~owner ~name:"ordered_empty" ~length:n [||]|>get_string_ok])
      [Group.Point;Vertex;Primitive] in
    let index=Topology_index.create topology in
    let edge_groups=[Edge_group.init ~topology ~index ~name:"edges"(fun i->i mod 2=0)] in
    Geometry.create ~positions:(Packed.Float3.View.of_owned_exn
      ~x:(Array.init count raw) ~y:(Array.init count(fun i->raw(i+1))) ~z:(Array.init count(fun i->raw(i+2))))
      ~topology ~attributes:attrs ~groups ~edge_groups () |>get_string_ok in
  let remap_target copies source =
    let open Rdk in
    let get_string_ok = Result.get_ok in
    let t=Topology.View.view(Geometry.topology source) in
    let points=Geometry.point_count source and vertices=Geometry.vertex_count source
    and primitives=Geometry.primitive_count source in
    let topology=Topology.create_owned ~point_count:(copies*points)
      ~vertex_points:(Array.init(copies*vertices)(fun i->t.vertex_points.(i mod vertices)+(i/vertices)*points))
      ~primitive_offsets:(Array.init(copies*primitives+1)(fun i->if i=copies*primitives then copies*vertices
        else t.primitive_offsets.(i mod primitives)+(i/primitives)*vertices))
      ~primitive_kinds:(Array.init(copies*primitives)(fun _->Topology.Polygon)) |>get_string_ok in
    Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x:(Array.make(copies*points)3.)
      ~y:(Array.make(copies*points)(-0.)) ~z:(Array.make(copies*points)7.)) ~topology () |>get_string_ok
 in
(* Opaque affinity survives independently encoded source/target records. This
   host-only token is not a Lisp argument or ordinary authored numeric data. *)
let remap_geometry geometry = match geometry_value geometry with
  |E.Record fields->E.Record(fields@["$topology_token",E.Text
      ("rdk-topology:"^string_of_int(Rdk.Topology.data_id(Rdk.Geometry.topology geometry)))])
  |_->assert false in
let remap source target fields =
  let snapshot geometry = Sop.Node.Private.make_geometry ~operation:"fixture.remap_source"
    ~version:1 ~parameters:"" ~cook_mode:Sop.Node.Generator
    ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _->Ok Sop.Node.Private.{geometry;instances=None}) in
  let factory key geometry=Sop.Edit_graph.factory ~key ~label:key ~category:["Test"] ~arity:0
    (fun _->Ok (snapshot geometry)) in
  let text=Printf.sprintf {|(workspace fixture_remap (graph g :context sop
    [(points : (array int) (array/int 0)) (vertices : (array int) (array/int 0))
     (primitives : (array int) (array/int 0)) (positions : text "target")
     (edges : {:offsets (array int) :values (array int)}
       {:offsets (array/int 1) :values (array/int 0)})]
    (sop/remap (sop/src) (sop/dst) :points points :vertices vertices
      :primitives primitives :positions positions%s)))|}
      (if List.mem_assoc "edges" fields then " :edges edges" else "") in
  Result.bind(Flow.Syntax.parse text)(fun forms->
    Result.bind(Flow_sop.Lower.workspace ~reference:true
      ~factories:(factory "src" source::factory "dst" target::Sop.Nodes.factories) ~inputs:["g",fields] forms)(fun lowered->
      let graph=List.hd lowered.graphs in
      match Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) with
      |Error message->Error(Diagnostic.error ~code:"E_DATA_SOURCE" message)
      |Ok node->let session=Sop.Session.create ~max_entries:8 ~max_payload_bytes:1_000_000|>Result.get_ok in
          Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
            match Sop.Session.cook session ~context:(Sop.Context.create()|>Result.get_ok)node with
            |Error d->Error(Diagnostic.error ?cause:d.cause ~code:d.code d.message)
            |Ok output->Ok(Result.get_ok(Sop.Payload.geometry output.payload))))) in
let remap_case source target fields =
  let values=remap_geometry source::remap_geometry target::List.map snd fields in
  let result,text_bytes=match remap source target fields with
    |Error d->diagnostic d,[]
    |Ok value->
        (* Independent native byte oracle, not Text through the JSON codec. *)
        let hex text=String.init(2*String.length text)(fun i->
          "0123456789abcdef".[(Char.code text.[i/2] lsr (if i mod 2=0 then 4 else 0)) land 15]) in
        let text_bytes=Rdk.Geometry.attributes value |>List.filter_map(fun attribute->
          match Rdk.Attribute.View.storage attribute with
          |Rdk.Attribute.Text strings->Some(E.Record[
              "owner",E.Text(match Rdk.Attribute.owner attribute with
                Point->"point"|Vertex->"vertex"|Primitive->"primitive"|Detail->"detail");
              "name",E.Text(Rdk.Attribute.name attribute);
              "hex",E.List(Array.init(min 7(Array.length strings))(fun i->E.Text(hex strings.(i))))])
          |_->None) in
        raw_json(remap_geometry value),text_bytes in
  (* add_host normally encodes Float values numerically. Override only these
     raw-copy arguments, preserving the existing fixture protocol elsewhere. *)
  let body=Printf.sprintf {|{"static":[],"args":%s,"result":%s,"native_text_bytes":%s}|}
    (seq raw_json values) result (seq json text_bytes) in
  let name="sop/remap" in
  let cases_=match Hashtbl.find_opt cases name with Some cases_->cases_|None->
    let cases_=ref [] in Hashtbl.add cases name cases_;cases_ in
  cases_:=body :: !cases_ in
List.iter(fun count->let source=remap_source count in
  List.iter(fun copies->let target=remap_target copies source in
    let source_points=Rdk.Geometry.point_count source and source_vertices=Rdk.Geometry.vertex_count source
    and source_primitives=Rdk.Geometry.primitive_count source in
    let edge_count=Rdk.Topology_index.edge_count(Rdk.Topology_index.create(Rdk.Geometry.topology target))
    and source_edges=Rdk.Topology_index.edge_count(Rdk.Topology_index.create(Rdk.Geometry.topology source)) in
    let fields=["points",E.Int_array(Array.init(Rdk.Geometry.point_count target)(fun i->i mod source_points));
      "vertices",E.Int_array(Array.init(Rdk.Geometry.vertex_count target)(fun i->i mod source_vertices));
      "primitives",E.Int_array(Array.init(Rdk.Geometry.primitive_count target)(fun i->i mod source_primitives))] in
    List.iter(fun policy->remap_case source target(fields@["positions",E.Text policy;
      "edges",E.Record["offsets",E.Int_array(Array.init(edge_count+1)Fun.id);
        "values",E.Int_array(Array.init edge_count(fun i->i mod source_edges))]])) ["target";"mapped"])
    [0;1;2]) [0;3;9];
(* Lean, independently cooked native case for late ordered-group cancellation.
   The small census above already carries every storage/owner combination. *)
let large_count=32769 in
let large_topology=Rdk.Topology.empty ~point_count:large_count in
let large_text=[|"猫\255";"😀\000";"\192\128\254";"\239\191\189"|] in
let large_attributes=[
  Rdk.Attribute.create_owned ~owner:Point ~name:"float"
    (Float(Array.init large_count raw))|>Result.get_ok;
  Rdk.Attribute.create_owned ~owner:Point ~name:"text"
    (Text(Array.init large_count(fun i->large_text.(i mod Array.length large_text))))|>Result.get_ok] in
let large_order=Rdk.Group.ordered ~owner:Point ~name:"ordered" ~length:large_count
  (Array.init large_count(fun i->large_count-1-i))|>Result.get_ok in
let large_source=Rdk.Geometry.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn
  ~x:(Array.init large_count raw) ~y:(Array.make large_count(-0.)) ~z:(Array.make large_count 0.))
  ~topology:large_topology ~attributes:large_attributes ~groups:[large_order]()|>Result.get_ok in
let large_target=Rdk.Geometry.create ~positions:(Rdk.Packed.Float3.View.of_owned_exn
  ~x:(Array.make large_count 0.) ~y:(Array.make large_count 0.) ~z:(Array.make large_count 0.))
  ~topology:large_topology()|>Result.get_ok in
remap_case large_source large_target ["points",E.Int_array(Array.init large_count Fun.id);
  "vertices",E.Int_array[||];"primitives",E.Int_array[||];"positions",E.Text"mapped";
  "edges",E.Record["offsets",E.Int_array[|0|];"values",E.Int_array[||]]];
let source=remap_source 3 and target=remap_target 1(remap_source 3) in
let fields=["points",E.Int_array[|2;1;0|];"vertices",E.Int_array[|1;2;0|];
  "primitives",E.Int_array[|0|];"positions",E.Text"mapped";
  "edges",E.Record["offsets",E.Int_array[|0;0;2;4|];"values",E.Int_array[|0;1;1;1|]]] in
remap_case source target fields;
List.iter(fun(name,value)->remap_case source target
  (List.map(fun(key,old)->key,if key=name then value else old)fields))
  ["points",E.Int_array[||];"points",E.Int_array[|-2;0;1|];"points",E.Int_array[|-1;0;1|];"vertices",E.Int_array[|2;-1;0|];"primitives",E.Int_array[|-1|];"vertices",E.Int_array[|0;1;3|];
   "primitives",E.Int_array[|1|];"positions",E.Text"other";
   "edges",E.Record["offsets",E.Int_array[|0;2;1;3|];"values",E.Int_array[|0;1;2|]];
   "edges",E.Record["offsets",E.Int_array[|0;1;2;3|];"values",E.Int_array[|0;1;3|]]];
(* Native and host accept shared topology with changed P, but reject equal
   connectivity allocated under a distinct native topology identity. *)
let source=remap_source 3 in
let changed_p=Rdk.Geometry.with_positions(Rdk.Packed.Float3.View.of_owned_exn
  ~x:[|8.;7.;6.|] ~y:[|-0.;0.;-0.|] ~z:[|2.;3.;4.|])source|>Result.get_ok in
let unrelated=remap_target 1 source in
let fields=["points",E.Int_array[|0;1;2|];"vertices",E.Int_array[|0;1;2|];
  "primitives",E.Int_array[|0|];"positions",E.Text"target"] in
remap_case source changed_p fields;
remap_case source unrelated fields;
  (* write *)
  assert (List.length inventory = 102);
  if not (Sys.file_exists outdir) then Sys.mkdir outdir 0o755;
  List.iter (fun name ->
    let body = String.concat ",\n" (List.rev !(Hashtbl.find cases name)) in
    Out_channel.with_open_bin (Filename.concat outdir (export name ^ ".json")) (fun oc ->
      Printf.fprintf oc "{\"primitive\":%s,\"export\":%s,\"cases\":[\n%s\n]}\n" (str name) (str (export name)) body))
    (List.map C.primitive_name inventory @ ["array/sum";"sop/context_seed";"sop/delete_attr";"sop/attr";"sop/attr_checked";"sop/with_attr";"sop/has_attr";"sop/count";"sop/edges";"sop/topology";"sop/adjacency";"sop/components";"graph/shortest_paths";"graph/forest";"graph/chains";"graph/color";"sop/mesh";"sop/mesh_planes";"sop/remap";
      "sop/group";"sop/group_names";"sop/group_order";"sop/with_group";
      "sop/attr_type";"sop/attr_names";"sop/attr_entries";"sop/rename_attr";"sop/swap_attr";"sop/delete_attrs";
      "sop/rename_group";"sop/delete_group";"sop/rename_edge";"sop/delete_edge";"text/glob_error";"text/glob_rewrite_error";
      "text/glob_literal";"text/glob_rewrite";"text/glob_rewrite_set_error";"text/glob_rewrite_set";"text/glob_rewrite_set_matches";"text/group_name_valid";"text/group_name_forced";"sop/group_entries";"$host/diagnostic_rows"])
