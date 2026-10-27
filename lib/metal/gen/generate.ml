open Registry

let fail format = Printf.ksprintf failwith format

let c_name name = "caml_rays_metal_gen_" ^ name

(* Every generated native call: its OCaml/C name, receiver, arguments,
   result, and how the stub reaches the SDK. *)
type body = Send of string * bool | Read of string | Write of string

type call =
  { name : string
  ; recv : string
  ; objc : string
  ; args : ty list
  ; ret : ty option
  ; body : body
  ; since : (int * int) option
  ; feature : Ogpu_core.Caps.feature
  ; pool : bool
  }

let calls entries =
  List.concat_map
    (function
      | Method { recv; objc; sel; args; ret; error; pool; ocaml; since; feature } ->
          [ { name = ocaml; recv; objc; args; ret; body = Send (sel, error); since; feature; pool } ]
      | Class_method { objc; sel; args; ret; ocaml; since; feature } ->
          [ { name = ocaml; recv = "Class"; objc; args; ret; body = Send (sel, false); since; feature; pool = true } ]
      | Property { recv; objc; name; ty; access; ocaml; since; feature } ->
          let get = { name = ocaml; recv; objc; args = []; ret = Some ty; body = Read name; since; feature; pool = true }
          and set = { name = "set_" ^ ocaml; recv; objc; args = [ ty ]; ret = None; body = Write name; since; feature; pool = true } in
          (match access with Get -> [ get ] | Set -> [ set ] | Get_set -> [ get; set ])
      | Native _ | Enum _ | Record _ -> [])
    entries

let selector_pieces sel = List.filter (( <> ) "") (String.split_on_char ':' sel)

(* the SDK struct and fields of the [Record] entry named [name] *)
let record_fields name =
  match
    List.find_map
      (function Record { ocaml; sdk; fields; _ } when ocaml = name -> Some (sdk, fields) | _ -> None)
      entries
  with
  | Some record -> record
  | None -> fail "unknown registry record %s" name

let validate_call ~ocaml ~native ~selectors ~check_feature call =
  if Hashtbl.mem ocaml call.name then fail "duplicate OCaml binding %s" call.name;
  Hashtbl.add ocaml call.name ();
  let symbol = c_name call.name in
  if Hashtbl.mem native symbol then fail "duplicate C binding %s" symbol;
  Hashtbl.add native symbol ();
  check_feature call.name call.feature;
  if call.recv = "" || call.objc = "" then fail "call %s needs a receiver kind and type" call.name;
  if not call.pool && (call.ret <> None || List.mem Str call.args
                      || (match call.body with Send (_, true) -> true | _ -> false)) then
    fail "call %s needs an autorelease pool for native temporaries" call.name;
  (match call.ret with Some (Rec _ | Tuple _) -> fail "record result of %s is unsupported" call.name | _ -> ());
  List.iter (function Rec record | Tuple record -> ignore (record_fields record) | _ -> ()) call.args;
  match call.body with
  | Send (sel, error) ->
      if sel = "" then fail "empty Metal selector for %s" call.name;
      if Hashtbl.mem selectors (call.objc, sel) then
        fail "duplicate Metal selector %s.%s" call.objc sel;
      Hashtbl.add selectors (call.objc, sel) ();
      let expected = List.length call.args + (if error then 1 else 0) in
      let pieces = if String.contains sel ':' then List.length (selector_pieces sel) else 0 in
      if pieces <> expected then
        fail "selector %s takes %d arguments, %s gives %d" sel pieces call.name expected
  | Read property | Write property ->
      if property = "" || String.contains property ':' then
        fail "bad Metal property name for %s" call.name;
      if Hashtbl.mem selectors (call.objc, property ^ (if call.args = [] then "" else "=")) then
        fail "duplicate Metal property %s.%s" call.objc property;
      Hashtbl.add selectors (call.objc, property ^ (if call.args = [] then "" else "=")) ()

(* ---- OCaml side ---- *)

let ty_ocaml = function
  | Scalar Bool -> "bool" | Scalar Int -> "int"
  | Scalar Nsuint_int -> "int"
  | Scalar (Nsuint | Nsint) | Enum_of _ -> "int64"
  | Scalar (Float | Double) -> "float"
  | Str -> "string"
  | Obj _ -> "Types.handle"
  | Opt_obj _ -> "Types.handle option"
  | Rec record -> "Record." ^ record ^ ".t"
  | Tuple record -> "(" ^ String.concat " * " (List.map (fun (_, scalar) ->
      match scalar with Bool -> "bool" | Int | Nsuint_int -> "int"
      | Nsuint | Nsint -> "int64" | Float | Double -> "float") (snd (record_fields record))) ^ ")"

let external_decl call =
  let types = if call.recv = "Class" then
      (match call.args with [] -> ["unit"] | args -> List.map ty_ocaml args)
    else "Types.handle" :: List.map ty_ocaml call.args in
  let ret = match call.ret with None -> "unit" | Some t -> ty_ocaml t in
  let names =
    if List.length types > 5 then Printf.sprintf "%S %S" (c_name call.name ^ "_bytecode") (c_name call.name)
    else Printf.sprintf "%S" (c_name call.name) in
  Printf.sprintf "  external %s : %s -> (%s, string) result = %s\n"
    call.name (String.concat " -> " types) ret names

(* ---- C side ---- *)

let scalar_c v = function
  | Bool -> Printf.sprintf "static_cast<BOOL>(Bool_val(%s))" v
  | Int -> Printf.sprintf "static_cast<int>(Long_val(%s))" v
  | Nsuint -> Printf.sprintf "static_cast<NSUInteger>(Int64_val(%s))" v
  | Nsuint_int -> Printf.sprintf "static_cast<NSUInteger>(Long_val(%s))" v
  | Nsint -> Printf.sprintf "static_cast<NSInteger>(Int64_val(%s))" v
  | Float -> Printf.sprintf "static_cast<float>(Double_val(%s))" v
  | Double -> Printf.sprintf "Double_val(%s)" v

(* argument [i] as a native expression; a string was converted to [text<i>] *)
let arg_c i t =
  let v = Printf.sprintf "arg%d" i in
  match t with
  | Scalar s -> scalar_c v s
  | Enum_of c -> Printf.sprintf "static_cast<%s>(Int64_val(%s))" c v
  | Str -> Printf.sprintf "text%d" i
  | Obj kind -> Printf.sprintf "object_of_handle(%s, Handle_kind::%s)" v kind
  | Opt_obj kind ->
      Printf.sprintf "(Is_none(%s) ? nil : object_of_handle(Some_val(%s), Handle_kind::%s))" v v kind
  | Rec name | Tuple name ->
      let sdk, fields = record_fields name in
      (* an all-float OCaml record is a flat float array *)
      let flat = (match t with Rec _ -> true | _ -> false)
        && List.for_all (fun (_, s) -> s = Float || s = Double) fields in
      let field index (_, scalar) =
        if flat then
          Printf.sprintf "static_cast<%s>(Double_field(%s, %d))"
            (if scalar = Float then "float" else "double") v index
        else scalar_c (Printf.sprintf "Field(%s, %d)" v index) scalar in
      Printf.sprintf "%s{%s}" sdk (String.concat ", " (List.mapi field fields))

let native_type = function
  | Scalar Bool -> "BOOL" | Scalar Int -> "int"
  | Scalar Nsuint -> "NSUInteger" | Scalar Nsint -> "NSInteger"
  | Scalar Nsuint_int -> "NSUInteger"
  | Scalar Float -> "float" | Scalar Double -> "double"
  | Enum_of c -> c
  | Str -> "NSString *"
  | Obj _ | Opt_obj _ -> "id"
  | Rec name | Tuple name -> fst (record_fields name)

(* statements turning [native_result] into [copied_result] *)
let copy_result = function
  | None -> "copied_result = Val_unit;\n"
  | Some (Scalar Bool) -> "copied_result = Val_bool(native_result);\n"
  | Some (Scalar Int) -> "copied_result = Val_long(native_result);\n"
  | Some (Scalar Nsuint_int) ->
      "if (native_result > static_cast<NSUInteger>(Max_long))\n          CAMLreturn(result_error_text(\"Metal returned a value outside OCaml int range\"));\n        copied_result = Val_long(native_result);\n"
  | Some (Scalar Nsuint) ->
      "if (native_result > static_cast<NSUInteger>(INT64_MAX))\n\
      \          CAMLreturn(result_error_text(\"Metal returned a value outside signed 64-bit range\"));\n\
      \        copied_result = caml_copy_int64(static_cast<std::int64_t>(native_result));\n"
  | Some (Scalar Nsint | Enum_of _) ->
      "copied_result = caml_copy_int64(static_cast<std::int64_t>(native_result));\n"
  | Some (Scalar (Float | Double)) -> "copied_result = caml_copy_double(native_result);\n"
  | Some Str -> "copied_result = caml_copy_string(native_result.UTF8String ?: \"\");\n"
  | Some (Obj kind) ->
      Printf.sprintf
        "if (native_result == nil)\n\
        \          CAMLreturn(result_error_text(\"Metal returned nil\"));\n\
        \        copied_result = allocate_handle(native_result, Handle_kind::%s);\n" kind
  | Some (Opt_obj kind) ->
      Printf.sprintf
        "if (native_result == nil) {\n\
        \          copied_result = Val_none;\n\
        \        } else {\n\
        \          handle = allocate_handle(native_result, Handle_kind::%s);\n\
        \          copied_result = caml_alloc_some(handle);\n\
        \        }\n" kind
  | Some (Rec _ | Tuple _) -> fail "records are arguments only"

let call_body call =
  let texts =
    String.concat ""
      (List.mapi
         (fun i -> function
           | Str ->
               Printf.sprintf
                 "        NSString *text%d = string_from_ocaml(arg%d);\n\
                 \        if (text%d == nil)\n\
                 \          CAMLreturn(result_error_text(\"Metal string argument is not valid UTF-8\"));\n"
                 i i i
           | _ -> "")
         call.args) in
  let args = List.mapi arg_c call.args in
  let assign expr = match call.ret with
    | None -> Printf.sprintf "        %s;\n" expr
    | Some t -> Printf.sprintf "        const %s native_result = %s;\n" (native_type t) expr in
  texts ^
  match call.body with
  | Read property -> assign ("object." ^ property) ^ "        " ^ copy_result call.ret
  | Write property ->
      Printf.sprintf "        object.%s = %s;\n" property (List.hd args) ^ "        " ^ copy_result None
  | Send (sel, error) ->
      let args = if error then args @ [ "&native_error" ] else args in
      let message =
        if args = [] then sel
        else String.concat " " (List.map2 (fun p a -> p ^ ":" ^ a) (selector_pieces sel) args) in
      let send = Printf.sprintf "[%s %s]" (if call.recv = "Class" then call.objc else "object") message in
      if not error then assign send ^ "        " ^ copy_result call.ret
      else
        "        NSError *native_error = nil;\n"
        ^ (match call.ret with
           | None ->
               Printf.sprintf "        if (!%s)\n          CAMLreturn(result_error(error_description(native_error, @\"%s failed\")));\n" send sel
           | Some _ ->
               assign send
               ^ Printf.sprintf "        if (native_error != nil)\n          CAMLreturn(result_error(error_description(native_error, @\"%s failed\")));\n" sel)
        ^ "        " ^ copy_result call.ret

let emit_call buffer call =
  let args = List.mapi (fun i _ -> Printf.sprintf "arg%d" i) call.args in
  let params = if call.recv = "Class" then (if args = [] then ["unit"] else args)
    else "raw_handle" :: args in
  let rec chunks = function
    | [] -> []
    | l -> List.take 5 l :: chunks (List.drop 5 l) in
  let roots = match chunks params with
    | [] -> ""
    | first :: rest ->
        Printf.sprintf "  CAMLparam%d(%s);\n" (List.length first) (String.concat ", " first)
        ^ String.concat "" (List.map (fun c ->
            Printf.sprintf "  CAMLxparam%d(%s);\n" (List.length c) (String.concat ", " c)) rest) in
  let guard, close_guard, unavailable = match call.since with
    | None -> "", "", ""
    | Some (major, minor) ->
        Printf.sprintf "    if (@available(macOS %d.%d, *)) {\n" major minor, "    }\n",
        Printf.sprintf "    CAMLreturn(result_error_text(\"Metal %s requires macOS %d.%d\"));\n"
          call.name major minor in
  Printf.bprintf buffer
    "extern \"C\" CAMLprim value %s(%s) {\n%s  CAMLlocal3(result, copied_result, handle);\n%s%s      @try {\n%s%s        result = result_ok(copied_result);\n        CAMLreturn(result);\n      } @catch (NSException *exception) {\n        @autoreleasepool { CAMLreturn(result_error(exception.reason)); }\n      }\n%s%s%s}\n"
    (c_name call.name) (String.concat ", " (List.map (( ^ ) "value ") params)) roots
    (if call.pool then "  @autoreleasepool {\n" else "")
    guard (if call.recv = "Class" then "" else Printf.sprintf "        %s object = object_of_handle(raw_handle, Handle_kind::%s);\n" call.objc call.recv)
    (call_body call) close_guard unavailable (if call.pool then "  }\n" else "");
  if List.length params > 5 then
    Printf.bprintf buffer
      "extern \"C\" CAMLprim value %s_bytecode(value *argv, int argn) {\n  (void)argn;\n  return %s(%s);\n}\n"
      (c_name call.name) (c_name call.name)
      (String.concat ", " (List.init (List.length params) (Printf.sprintf "argv[%d]")))


let validate_feature_map mappings =
  let seen = Hashtbl.create 32 in
  List.iter
    (fun (feature, symbols) ->
      (match feature with
       | Ogpu_core.Caps.Unknown _ ->
           fail "unknown OGPU feature map entry: %s" (feature_name feature)
       | _ -> ());
      if Hashtbl.mem seen feature then
        fail "duplicate OGPU feature map entry: %s" (feature_name feature);
      Hashtbl.add seen feature ();
      if symbols = [] then
        fail "empty Metal symbol map entry: %s" (feature_name feature);
      List.iter
        (function
          | Protocol name | Class name when name <> "" -> ()
          | _ -> fail "empty Metal SDK type in feature map")
        symbols)
    mappings;
  if List.length mappings <> 11 then fail "OGPU feature map is incomplete"

let validate entries =
  validate_feature_map feature_map;
  let ocaml = Hashtbl.create 32
  and native = Hashtbl.create 32
  and selectors = Hashtbl.create 32
  and sdk_symbols = Hashtbl.create 256 in
  let check_feature name = function
    | Ogpu_core.Caps.Unknown _ -> fail "unknown OGPU feature for %s" name
    | feature when not (List.mem_assoc feature feature_map) ->
        fail "unmapped OGPU feature for %s" name
    | _ -> ()
  in
  List.iter
    (function
      | Native { ocaml = name; signature; primitives; reason } ->
          if Hashtbl.mem ocaml name then fail "duplicate OCaml binding %s" name;
          Hashtbl.add ocaml name ();
          if reason = "" || primitives = [] || List.length primitives > 2 then
            fail "native binding %s needs primitives and a reason" name;
          ignore (Parse.core_type (Lexing.from_string signature));
          List.iter (fun symbol ->
              if Hashtbl.mem native symbol then fail "duplicate C binding %s" symbol;
              Hashtbl.add native symbol ()) primitives
      | Enum { sdk; ocaml = name; cases; feature } ->
          check_feature name feature;
          if Hashtbl.mem ocaml name then fail "duplicate OCaml binding %s" name;
          Hashtbl.add ocaml name ();
          if cases = [] then fail "empty enum %s" sdk;
          List.iter
            (fun (case_name, symbol, _, _) ->
              if Hashtbl.mem ocaml case_name then
                fail "duplicate OCaml binding %s" case_name;
              Hashtbl.add ocaml case_name ();
              if Hashtbl.mem sdk_symbols symbol then
                fail "duplicate SDK enum case %s" symbol;
              Hashtbl.add sdk_symbols symbol ())
            cases
      | Record { sdk; ocaml = name; fields; feature } ->
          check_feature name feature;
          if sdk = "" || fields = [] then fail "empty Metal record %s" name;
          if Hashtbl.mem ocaml name then fail "duplicate OCaml binding %s" name;
          Hashtbl.add ocaml name ();
          let seen_fields = Hashtbl.create 8 in
          List.iter
            (fun (field, scalar) ->
              if field = "" || Hashtbl.mem seen_fields field then
                fail "duplicate or empty record field %s.%s" name field;
              Hashtbl.add seen_fields field ();
              match scalar with
              | Bool | Int | Nsuint | Nsuint_int | Nsint | Float | Double -> ())
            fields
      | Class_method _ | Method _ | Property _ -> ())
    entries;
  List.iter (validate_call ~ocaml ~native ~selectors ~check_feature) (calls entries)

let contains_token source token =
  let length = String.length token in
  let rec search offset =
    match String.index_from_opt source offset token.[0] with
    | None -> false
    | Some index ->
        if
          index + length <= String.length source
          && String.sub source index length = token
          && (index + length = String.length source
             || not
                  (match source.[index + length] with
                   | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
                   | _ -> false))
        then true
        else search (index + 1)
  in
  search 0

let read path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let used_bindings source =
  let lexbuf = Lexing.from_string source in
  let tree = Parse.implementation lexbuf in
  let used = Hashtbl.create 32 in
  let expr iterator expression =
    (match expression.Parsetree.pexp_desc with
     | Pexp_ident { txt =
         (Longident.Ldot (Longident.Ldot (Longident.Lident "Metal_raw", "Registry"), name)
          | Longident.Ldot (Longident.Lident "Metal_raw", name)
          | Longident.Ldot (Longident.Lident "Registry", name)); _ } ->
         Hashtbl.replace used name ()
     | _ -> ());
    Ast_iterator.default_iterator.expr iterator expression
  in
  let iterator = { Ast_iterator.default_iterator with expr } in
  iterator.structure iterator tree;
  used

let used_enum_modules source =
  let tree = Parse.implementation (Lexing.from_string source) in
  let used = Hashtbl.create 16 in
  let rec visit_path = function
    | Longident.Lident _ -> ()
    | Longident.Ldot (prefix, name) ->
        if String.starts_with ~prefix:"Mtl_" name then
          Hashtbl.replace used name ();
        visit_path prefix
    | Longident.Lapply (left, right) ->
        visit_path left;
        visit_path right
  in
  let expr iterator expression =
    (match expression.Parsetree.pexp_desc with
     | Pexp_ident identifier -> visit_path identifier.txt
     | _ -> ());
    Ast_iterator.default_iterator.expr iterator expression
  in
  let module_expr iterator expression =
    (match expression.Parsetree.pmod_desc with
     | Pmod_ident identifier -> visit_path identifier.txt
     | _ -> ());
    Ast_iterator.default_iterator.module_expr iterator expression
  in
  let typ iterator core_type =
    (match core_type.Parsetree.ptyp_desc with
     | Ptyp_constr (identifier, _) -> visit_path identifier.txt
     | _ -> ());
    Ast_iterator.default_iterator.typ iterator core_type
  in
  let iterator = { Ast_iterator.default_iterator with expr; module_expr; typ } in
  iterator.structure iterator tree;
  used

let write path body =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output body)

let emit_enums_ml buffer entries =
  Buffer.add_string buffer "module Enum = struct\n";
  List.iter
    (function
      | Enum { ocaml; cases; _ } ->
          Printf.bprintf buffer "  module %s = struct\n    type t = int64\n" ocaml;
          List.iter
            (fun (name, _, bits, _) ->
              Printf.bprintf buffer "    let %s : t = 0x%016LxL\n" name bits)
            cases;
          Buffer.add_string buffer
            "    let to_int64 (value : t) = value\n    let of_int64 bits = match bits with\n";
          let unique =
            List.sort_uniq Int64.compare
              (List.map (fun (_, _, bits, _) -> bits) cases)
          in
          List.iter
            (fun bits -> Printf.bprintf buffer "    | 0x%016LxL -> Some bits\n" bits)
            unique;
          Buffer.add_string buffer "    | _ -> None\n  end\n"
      | Native _ | Class_method _ | Method _ | Property _ | Record _ -> ())
    entries;
  Buffer.add_string buffer "end\n"

let emit_enums_mli buffer entries =
  Buffer.add_string buffer "module Enum : sig\n";
  List.iter
    (function
      | Enum { ocaml; cases; _ } ->
          Printf.bprintf buffer "  module %s : sig\n    type t = private int64\n" ocaml;
          List.iter
            (fun (name, _, _, _) ->
              Printf.bprintf buffer "    val %s : t\n" name)
            cases;
          Buffer.add_string buffer
            "    val to_int64 : t -> int64\n    val of_int64 : int64 -> t option\n  end\n"
      | Native _ | Class_method _ | Method _ | Property _ | Record _ -> ())
    entries;
  Buffer.add_string buffer "end\n"

let record_field_types = function
  | Bool -> "bool", "BOOL"
  | Int -> "int", "int"
  | Nsuint -> "int64", "NSUInteger"
  | Nsuint_int -> "int", "NSUInteger"
  | Nsint -> "int64", "NSInteger"
  | Float -> "float", "float"
  | Double -> "float", "double"

let emit_records ~signature buffer entries =
  Buffer.add_string buffer (if signature then "module Record : sig\n" else "module Record = struct\n");
  List.iter
    (function
      | Record { ocaml; fields; _ } ->
          Printf.bprintf buffer "  module %s %s\n    type t = {\n"
            ocaml (if signature then ": sig" else "= struct");
          List.iter
            (fun (field, scalar) ->
              let ocaml_type, _ = record_field_types scalar in
              Printf.bprintf buffer "      %s : %s;\n" field ocaml_type)
            fields;
          Buffer.add_string buffer "    }\n  end\n"
      | Native _ | Class_method _ | Enum _ | Method _ | Property _ -> ())
    entries;
  Buffer.add_string buffer "end\n"

let emit_native buffer entries =
  List.iter (function
      | Native { ocaml; signature; primitives; _ } ->
          Printf.bprintf buffer "  external %s : %s = %s\n" ocaml signature
            (String.concat " " (List.map (Printf.sprintf "%S") primitives))
      | _ -> ()) entries

let emit_ml entries =
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer
    "(* Generated from lib/metal/gen/registry.ml. Do not edit. *)\n";
  emit_enums_ml buffer entries;
  emit_records ~signature:false buffer entries;
  Buffer.add_string buffer "module Make (Types : module type of Metal_raw_types) = struct\n  open Types\n";
  List.iter (fun call -> Buffer.add_string buffer (external_decl call)) (calls entries);
  emit_native buffer entries;
  Buffer.add_string buffer "end\n";
  Buffer.contents buffer

let emit_mli entries =
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer
    "(* Generated from lib/metal/gen/registry.ml. Do not edit. *)\n";
  emit_enums_mli buffer entries;
  emit_records ~signature:true buffer entries;
  Buffer.add_string buffer "module Make (Types : module type of Metal_raw_types) : sig\n  open Types\n";
  List.iter (fun call -> Buffer.add_string buffer (external_decl call)) (calls entries);
  emit_native buffer entries;
  Buffer.add_string buffer "end\n";
  Buffer.contents buffer

let emit_c entries =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "/* Generated from lib/metal/gen/registry.ml. Do not edit. */\n";
  List.iter
    (function
      | Enum { cases; _ } ->
          List.iter
            (fun (_, symbol, bits, (major, minor, _)) ->
              Printf.bprintf buffer
                "#if __MAC_OS_X_VERSION_MAX_ALLOWED >= %d\n#pragma clang diagnostic push\n#pragma clang diagnostic ignored \"-Wunguarded-availability-new\"\n#pragma clang diagnostic ignored \"-Wdeprecated-declarations\"\nstatic_assert(static_cast<unsigned long long>(%s) == 0x%016LxULL);\n#pragma clang diagnostic pop\n#endif\n"
                ((major * 10000) + (minor * 100)) symbol bits)
            cases
      | Record { sdk; fields; _ } ->
          Printf.bprintf buffer "static_assert(std::is_standard_layout_v<%s>);\n" sdk;
          List.iter
            (fun (field, scalar) ->
              let _, native_type = record_field_types scalar in
              Printf.bprintf buffer
                "static_assert(std::is_same_v<decltype(%s{}.%s), %s>);\n"
                sdk field native_type)
            fields
      | Native _ | Class_method _ | Method _ | Property _ -> ())
    entries;
  List.iter (emit_call buffer) (calls entries);
  Buffer.contents buffer

let emit_feature_checks () =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer
    "/* Checked OGPU feature map from lib/metal/gen/registry.ml. */\n#pragma clang diagnostic push\n#pragma clang diagnostic ignored \"-Wunguarded-availability-new\"\n";
  List.iter
    (fun (_, symbols) ->
      List.iter
        (function
          | Protocol name ->
              Printf.bprintf buffer
                "static_assert(sizeof(id<%s>) == sizeof(void *));\n" name
          | Class name ->
              Printf.bprintf buffer
                "static_assert(sizeof(%s *) == sizeof(void *));\n" name)
        symbols)
    feature_map;
  Buffer.add_string buffer "#pragma clang diagnostic pop\n";
  Buffer.contents buffer

let self_test () =
  let factory = Class_method
      { objc = "MTLBlitPassDescriptor"; sel = "blitPassDescriptor"; args = []
      ; ret = Some (Obj "Blit_pass_descriptor"); ocaml = "test_factory"
      ; since = Some (10, 15); feature = Ogpu_core.Caps.Buffer } in
  let custom = Native
      { ocaml = "test_custom"; signature = "handle -> bytes -> (unit, string) result"
      ; primitives = [ "caml_test_custom" ]; reason = "byte buffer lifetime" } in
  let tuple = Method
      { recv = "Compute_encoder"; objc = "id<MTLComputeCommandEncoder>"
      ; sel = "dispatchThreads:threadsPerThreadgroup:"
      ; args = [ Tuple "Mtl_size_int"; Tuple "Mtl_size_int" ]; ret = None; error = false; pool = false
      ; ocaml = "test_tuple"; since = None; feature = Ogpu_core.Caps.Compute_pipeline } in
  validate [factory; custom; tuple];
  let native = emit_c [factory; tuple] and ml = emit_ml [factory; custom; tuple] in
  if not (contains_token native "[MTLBlitPassDescriptor blitPassDescriptor]")
     || not (contains_token native "@available(macOS 10.15, *)")
     || not (contains_token ml "test_factory : unit -> (Types.handle, string) result")
     || not (contains_token ml "test_custom : handle -> bytes -> (unit, string) result = \"caml_test_custom\"")
     || not (contains_token ml "Types.handle -> (int * int * int) -> (int * int * int)")
     || not (contains_token native "MTLSize{static_cast<NSUInteger>(Long_val(Field(arg0, 0)))")
  then fail "class/native/tuple ABI generation regressed";
  if contains_token (emit_c [tuple]) "  @autoreleasepool {\n" then
    fail "pool-free tuple call gained a success-path autorelease pool";
  (try validate [custom; custom]; fail "duplicate native name was accepted"
   with Failure message -> if not (String.starts_with ~prefix:"duplicate OCaml binding" message) then raise (Failure message));
  let high_arity = Method
      { recv = "Indirect_render_command"; objc = "id<MTLIndirectRenderCommand>"
      ; sel = "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:"
      ; args = [Enum_of "MTLPrimitiveType"; Scalar Nsuint; Scalar Nsuint; Scalar Nsuint; Scalar Nsuint]
      ; ret = None; error = false; pool = false; ocaml = "test_high_arity"; since = None
      ; feature = Ogpu_core.Caps.Render_pipeline } in
  validate [high_arity];
  if not (contains_token (emit_ml [high_arity]) "caml_rays_metal_gen_test_high_arity_bytecode")
     || not (contains_token (emit_c [high_arity]) "CAMLxparam1(arg4)") then
    fail "high arity native/bytecode ABI regressed";
  validate entries;
  (try
     validate_feature_map (List.hd feature_map :: feature_map);
     fail "duplicate feature map entry was accepted"
   with Failure message ->
     if not (String.starts_with ~prefix:"duplicate OGPU feature map entry" message) then
       fail "unexpected feature map error: %s" message);
  (try
     validate_feature_map
       ((Ogpu_core.Caps.Unknown "future", [ Protocol "MTLDevice" ]) :: List.tl feature_map);
     fail "unknown feature map entry was accepted"
   with Failure message ->
     if not (String.starts_with ~prefix:"unknown OGPU feature map entry" message) then
       fail "unexpected feature map error: %s" message);
  let checked_types = emit_feature_checks () in
  if not (contains_token checked_types "id<MTLIntersectionFunctionTable>")
     || not (contains_token checked_types "id<MTLAccelerationStructure>")
  then fail "feature map lost SDK type checks";
  let record =
    Record
      { sdk = "MTLSize"
      ; ocaml = "Mtl_size"
      ; fields = [ "width", Nsuint; "height", Nsuint; "depth", Nsuint ]
      ; feature = Ogpu_core.Caps.Compute_pipeline
      }
  in
  validate [ record ];
  if not (contains_token (emit_ml [ record ]) "module Mtl_size = struct")
     || not (contains_token (emit_c [ record ]) "decltype(MTLSize{}.width), NSUInteger")
  then fail "record generation lost its OCaml or SDK type check";
  (try
     validate (entries @ entries);
     fail "duplicate binding was accepted"
   with Failure message ->
     if not (String.starts_with ~prefix:"duplicate OCaml binding" message) then
       fail "unexpected duplicate error: %s" message);
  (match List.find_opt (function Property _ -> true | _ -> false) entries with
   | Some (Property property) ->
       (try
          validate (Property { property with ocaml = "duplicate_property" } :: entries);
          fail "duplicate Metal property was accepted"
        with Failure message ->
          if not (String.starts_with ~prefix:"duplicate Metal property" message)
          then fail "unexpected property error: %s" message)
   | _ -> fail "registry self-test has no property");
  let native = emit_c entries in
  if
    not (contains_token native "object.maxThreadgroupMemoryLength")
    || not (contains_token native "@available(macOS 10.13, *)")
    || not (contains_token native "const BOOL native_result = object.supportIndirectCommandBuffers")
    || not (contains_token native "Val_bool(native_result)")
  then fail "generated property lost its typed read or availability guard";
  (* a method with object, enum and NSError arguments, and an owned result *)
  let method_ =
    Method
      { recv = "Device"; objc = "id<MTLDevice>"; sel = "newBufferWithLength:options:"
      ; args = [ Scalar Nsuint; Enum_of "MTLResourceOptions" ]; ret = Some (Obj "Buffer")
      ; error = false; pool = true; ocaml = "device_new_buffer"; since = None
      ; feature = Ogpu_core.Caps.Buffer } in
  validate [ method_ ];
  let native = emit_c [ method_ ] and ml = emit_ml [ method_ ] in
  if not (contains_token native
            "[object newBufferWithLength:static_cast<NSUInteger>(Int64_val(arg0)) options:static_cast<MTLResourceOptions>(Int64_val(arg1))]")
     || not (contains_token native "allocate_handle(native_result, Handle_kind::Buffer)")
     || not (contains_token ml "external device_new_buffer : Types.handle -> int64 -> int64 -> (Types.handle, string) result")
  then fail "generated method lost its typed message or owned result";
  (match method_ with
   | Method m ->
       (try validate [ Method { m with args = [ Scalar Nsuint ] } ];
          fail "selector arity mismatch was accepted"
        with Failure message ->
          if not (String.starts_with ~prefix:"selector newBufferWithLength:options: takes 2" message)
          then fail "unexpected arity error: %s" message)
   | _ -> ());
  (* a record passed by value and a checked UTF-8 string argument *)
  let dispatch =
    Method
      { recv = "Compute_encoder"; objc = "id<MTLComputeCommandEncoder>"
      ; sel = "dispatchThreads:threadsPerThreadgroup:"
      ; args = [ Rec "Mtl_size_int"; Rec "Mtl_size_int" ]; ret = None; error = false; pool = true
      ; ocaml = "compute_encoder_dispatch_threads"; since = None
      ; feature = Ogpu_core.Caps.Compute_pipeline }
  and label =
    Property
      { recv = "Buffer"; objc = "id<MTLBuffer>"; name = "label"; ty = Str; access = Set
      ; ocaml = "buffer_label"; since = None; feature = Ogpu_core.Caps.Buffer } in
  validate [ dispatch; label ];
  let native = emit_c [ dispatch; label ] and ml = emit_ml [ dispatch ] in
  if not (contains_token native
            "MTLSize{static_cast<NSUInteger>(Long_val(Field(arg0, 0))), static_cast<NSUInteger>(Long_val(Field(arg0, 1)))")
     || not (contains_token ml "Types.handle -> Record.Mtl_size_int.t -> Record.Mtl_size_int.t -> (unit, string) result")
     || not (contains_token native "NSString *text0 = string_from_ocaml(arg0)")
     || not (contains_token native "object.label = text0")
  then fail "generated record or string argument lost its conversion";
  (match dispatch with
   | Method m ->
       (try validate [ Method { m with ret = Some (Rec "Mtl_size_int") } ];
          fail "record result was accepted"
        with Failure message ->
          if not (String.starts_with ~prefix:"record result" message)
          then fail "unexpected record error: %s" message)
   | _ -> ());
  let name = "device_max_threadgroup_memory_length" in
  let source =
    "(* Metal_raw.Registry." ^ name ^ " *)\n"
    ^ "let call handle = Metal_raw.Registry." ^ name ^ " handle\n"
  in
  if not (Hashtbl.mem (used_bindings source) name) then
    fail "safe-layer call was not found";
  if
    Hashtbl.mem
      (used_bindings ("(* Metal_raw.Registry." ^ name ^ " *)\nlet unused = ()\n"))
      name
  then fail "comment was mistaken for a safe-layer call";
  let source = "(* Enum.Mtl_data_type *)\nmodule Data_type = Metal_gen.Enum.Mtl_data_type\n" in
  if not (Hashtbl.mem (used_enum_modules source) "Mtl_data_type") then
    fail "enum alias was not found";
  if Hashtbl.mem (used_enum_modules "(* Enum.Mtl_data_type *)\nlet x = ()\n") "Mtl_data_type"
  then fail "comment was mistaken for an enum reference"

let () =
  try
    match Array.to_list Sys.argv with
    | [ _; "--self-test" ] -> self_test ()
    | [ _; source; raw; ml; mli; native; feature_checks ] ->
        validate entries;
        let raw_source = read raw in
        if List.exists (fun item -> match item.Parsetree.pstr_desc with
            | Pstr_primitive _ -> true | _ -> false)
            (Parse.implementation (Lexing.from_string raw_source)) then
          fail "raw externals must be declared in the registry";
        let source = read source ^ "\n" ^ raw_source in
        let used = used_bindings source in
        let used_enums = used_enum_modules source in
        List.iter
          (function
            | Native { ocaml; _ } ->
                if not (Hashtbl.mem used ocaml) then
                  fail "registry native %s is not used by metal.ml" ocaml
            | Class_method _ | Method _ | Property _ -> ()
            | Enum { ocaml; _ } ->
                if not (Hashtbl.mem used_enums ocaml) then
                  fail "registry enum %s is not used by metal.ml" ocaml
            | Record { ocaml; _ } ->
                if not (Hashtbl.mem used_enums ocaml)
                   && not (List.exists (fun call -> List.exists (function Rec name | Tuple name -> name = ocaml | _ -> false) call.args) (calls entries)) then
                  fail "registry record %s is not used by metal.ml" ocaml)
          entries;
        List.iter (fun call ->
            if not (Hashtbl.mem used call.name) then
              fail "registry binding %s is not used by metal.ml" call.name)
          (calls entries);
        write ml (emit_ml entries);
        write mli (emit_mli entries);
        write native (emit_c entries);
        write feature_checks (emit_feature_checks ())
    | _ -> fail "usage: generate [--self-test | METAL_ML RAW_ML ML MLI NATIVE FEATURE_CHECKS]"
  with Failure message ->
    prerr_endline message;
    exit 1
