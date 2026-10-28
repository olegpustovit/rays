(* Generates [generated_abi.h] for one SDL binding at build time, and pins the
   struct layouts the stubs read.

   Inputs, all checked in and none generated:
   - packaging/sdl3.lock: the version floor the header asserts;
   - lib/sdl3/abi.sexp: the reviewed size, alignment and field offsets of
     every SDL struct the stubs read.

   The stubs are scanned for what they read ([event->key.scancode],
   [surface->pitch]) and call, so the layout contract is exactly the set of
   fields in use: a newer SDL that leaves those alone builds unchanged, one
   that moves them fails the build and names the field. [accept] re-probes
   the installed headers and rewrites abi.sexp for review. *)

module Spec = Generator_spec

let fail format = Printf.ksprintf failwith format
let read_file = Sdl3_lock.read_file
let write_file = Sdl3_lock.write_file
let is_identifier = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false

let identifier_at text index =
  let length = String.length text in
  let rec stop index =
    if index < length && is_identifier text.[index] then stop (index + 1) else index
  in
  let stop = stop index in
  (String.sub text index (stop - index), stop)

(* Every offset where [needle] starts a token: not preceded by an identifier
   character. *)
let occurrences text needle =
  let n = String.length needle and length = String.length text in
  let rec go index acc =
    if index + n > length then List.rev acc
    else if String.sub text index n = needle && (index = 0 || not (is_identifier text.[index - 1]))
    then go (index + n) ((index + n) :: acc)
    else go (index + 1) acc
  in
  go 0 []

let add_field table structure field =
  let fields = Option.value (Hashtbl.find_opt table structure) ~default:[] in
  if not (List.mem field fields) then Hashtbl.replace table structure (fields @ [ field ])

(* The struct fields one stub file reads, in order of first use. *)
let reads text =
  let table = Hashtbl.create 32 in
  List.iter
    (fun index ->
      let member, stop = identifier_at text index in
      match List.assoc_opt member Spec.event_members with
      | Some structure when stop < String.length text && text.[stop] = '.' ->
          let field, _ = identifier_at text (stop + 1) in
          if field <> "" then begin
            add_field table structure "type";
            add_field table structure field
          end
      | Some _ | None -> ())
    (occurrences text "event->");
  List.iter
    (fun (variable, structure) ->
      List.iter
        (fun index ->
          let field, _ = identifier_at text index in
          if field <> "" then add_field table structure field)
        (occurrences text (variable ^ "->")))
    Spec.pointer_structs;
  if occurrences text "SDL_Event" <> [] then add_field table "SDL_Event" "";
  List.iter
    (fun (structure, fields) ->
      if occurrences text structure <> [] then List.iter (add_field table structure) fields)
    Spec.constructed_structs;
  Hashtbl.fold (fun structure fields acc -> (structure, fields) :: acc) table []
  |> List.sort compare

let uses_name text name = occurrences text name <> []

(* --- abi.sexp --- *)

type layout = { size : int; alignment : int; offsets : (string * int) list }

let read_abi path =
  let number text =
    match int_of_string_opt text with
    | Some value -> value
    | None -> fail "%s: %S is not a number" path text
  in
  match Sdl3_lock.parse_sexps (read_file path) with
  | [ Sdl3_lock.List entries ] ->
      List.map
        (function
          | Sdl3_lock.List [ Atom name; Atom size; Atom alignment; List offsets ] ->
              ( name,
                {
                  size = number size;
                  alignment = number alignment;
                  offsets =
                    List.map
                      (function
                        | Sdl3_lock.List [ Atom field; Atom offset ] -> (field, number offset)
                        | _ -> fail "%s: malformed field in %s" path name)
                      offsets;
                } )
          | _ -> fail "%s: malformed entry" path)
        entries
  | _ -> fail "%s: expected one list of struct entries" path

let write_abi path layouts =
  let output = Buffer.create 4096 in
  Buffer.add_string output
    "; The reviewed ABI of every SDL struct the stubs read: size, alignment\n\
     ; and the offset of each field in use. lib/sdl3/generate.exe turns it\n\
     ; into static asserts at build time; a newer SDL that moves one fails the\n\
     ; build and names it. Rewrite with\n\
     ;   dune exec lib/sdl3/generate.exe -- accept\n\
     ; and review the diff. Never edit by hand.\n\
     (";
  List.iteri
    (fun index (name, layout) ->
      if index > 0 then Buffer.add_string output "\n ";
      Printf.bprintf output "(%s %d %d (" name layout.size layout.alignment;
      List.iteri
        (fun index (field, offset) ->
          if index > 0 then Buffer.add_char output ' ';
          Printf.bprintf output "(%s %d)" field offset)
        layout.offsets;
      Buffer.add_string output "))")
    layouts;
  Buffer.add_string output ")\n";
  write_file path (Buffer.contents output)

(* --- emit --- *)

let stub_text root (spec : Spec.component) =
  String.concat "\n" (List.map (fun stubs -> read_file (Filename.concat root stubs)) spec.stubs)

let all_reads root =
  let merged = Hashtbl.create 32 in
  List.iter
    (fun spec ->
      List.iter
        (fun (structure, fields) -> List.iter (add_field merged structure) fields)
        (reads (stub_text root spec)))
    Spec.all;
  Hashtbl.fold (fun structure fields acc -> (structure, fields) :: acc) merged []
  |> List.sort compare

(* The stubs and abi.sexp must name exactly the same fields. *)
let check_abi_against_stubs ~abi_path abi used =
  let problems = ref [] in
  let problem format = Printf.ksprintf (fun text -> problems := text :: !problems) format in
  List.iter
    (fun (structure, fields) ->
      match List.assoc_opt structure abi with
      | None -> problem "%s is read by the stubs but not pinned" structure
      | Some layout ->
          List.iter
            (fun field ->
              if field <> "" && not (List.mem_assoc field layout.offsets) then
                problem "%s.%s is read by the stubs but not pinned" structure field)
            fields)
    used;
  List.iter
    (fun (structure, layout) ->
      match List.assoc_opt structure used with
      | None -> problem "%s is pinned but no stub reads it" structure
      | Some fields ->
          List.iter
            (fun (field, _) ->
              if not (List.mem field fields) then
                problem "%s.%s is pinned but no stub reads it" structure field)
            layout.offsets)
    abi;
  if !problems <> [] then
    fail "%s is out of step with the stubs:\n  %s\nrun: dune exec lib/sdl3/generate.exe -- accept"
      abi_path
      (String.concat "\n  " (List.rev !problems))

let emit ~root ~lock_path ~abi_path ~(spec : Spec.component) ~output =
  let lock = Sdl3_lock.read lock_path in
  let major, minor, patch = (Sdl3_lock.find lock spec.key).Sdl3_lock.floor in
  let abi = read_abi abi_path in
  check_abi_against_stubs ~abi_path abi (all_reads root);
  let text = stub_text root spec in
  let buffer = Buffer.create 8192 in
  let add format = Printf.bprintf buffer format in
  add
    "/* Generated by lib/sdl3/generate.exe from packaging/sdl3.lock and\n\
    \   lib/sdl3/abi.sexp; do not edit. */\n\
     #ifndef RAYS_%s_GENERATED_ABI_H\n\
     #define RAYS_%s_GENERATED_ABI_H\n\
     #include <SDL3/SDL.h>\n"
    (String.uppercase_ascii spec.key) (String.uppercase_ascii spec.key);
  if spec.include_file <> "SDL3/SDL.h" then add "#include <%s>\n" spec.include_file;
  add "#include <stddef.h>\n#include \"sdl3_probed.h\"\n";
  add "_Static_assert(%s == %d, \"%s major changed: review the bindings\");\n" spec.major_macro
    major spec.key;
  add
    "_Static_assert(%s >= SDL_VERSIONNUM(%d, %d, %d), \"%s is older than the floor in \
     packaging/sdl3.lock\");\n"
    spec.version_macro major minor patch spec.key;
  add
    "#if RAYS_SDL3_PROBED_VERSION != 0\n\
     _Static_assert(%s == RAYS_SDL3_PROBED_VERSION, \"pkg-config and the headers in use disagree: \
     the native libraries changed under an old build directory\");\n\
     #endif\n"
    spec.version_macro;
  List.iter
    (fun (name, typedef, signature) ->
      if uses_name text name then begin
        (* "ret (SDLCALL *)(args)" names a pointer type; split to typedef it. *)
        let marker = "(SDLCALL *)" in
        let at =
          let n = String.length marker in
          let rec go i = if String.sub signature i n = marker then i else go (i + 1) in
          go 0
        in
        let result = String.sub signature 0 at in
        let arguments =
          String.sub signature
            (at + String.length marker)
            (String.length signature - at - String.length marker)
        in
        add "typedef %s(SDLCALL *%s)%s;\n" result typedef arguments;
        add
          "_Static_assert(_Generic(&%s, %s: 1, default: 0), \"%s signature/calling convention \
           changed\");\n"
          name typedef name
      end)
    spec.signatures;
  let used = reads text in
  List.iter
    (fun (structure, fields) ->
      match List.assoc_opt structure abi with
      | None -> ()
      | Some layout ->
          add "_Static_assert(sizeof(%s) == %d, \"%s size changed\");\n" structure layout.size
            structure;
          add "_Static_assert(_Alignof(%s) == %d, \"%s alignment changed\");\n" structure
            layout.alignment structure;
          List.iter
            (fun field ->
              if field <> "" then
                add "_Static_assert(offsetof(%s, %s) == %d, \"%s.%s offset changed\");\n" structure
                  field (List.assoc field layout.offsets) structure field)
            fields)
    used;
  add "#endif\n";
  write_file output (Buffer.contents buffer)

(* --- accept --- *)

let rec remove_directory path =
  Sys.readdir path
  |> Array.iter (fun name ->
      let entry = Filename.concat path name in
      if Sys.is_directory entry then remove_directory entry else Sys.remove entry);
  Unix.rmdir path

let command_text program arguments =
  let directory = Filename.temp_dir "rays-sdl3-" "" in
  Fun.protect
    ~finally:(fun () -> remove_directory directory)
    (fun () ->
      let stdout_path = Filename.concat directory "stdout" in
      let stderr_path = Filename.concat directory "stderr" in
      let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
      let stdout_fd = Unix.openfile stdout_path flags 0o600 in
      let stderr_fd = Unix.openfile stderr_path flags 0o600 in
      let pid =
        Fun.protect
          ~finally:(fun () ->
            Unix.close stdout_fd;
            Unix.close stderr_fd)
          (fun () ->
            Unix.create_process_env program
              (Array.of_list (program :: arguments))
              (Unix.environment ()) Unix.stdin stdout_fd stderr_fd)
      in
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 -> read_file stdout_path
      | _, _ ->
          fail "%s failed: %s"
            (String.concat " " (program :: arguments))
            (String.trim (read_file stderr_path)))

let words text =
  String.split_on_char ' ' (String.trim text)
  |> List.concat_map (String.split_on_char '\n')
  |> List.filter (fun word -> word <> "")

(* The headers [accept] probes: the explicit directory when given, else
   pkg-config's. *)
let include_flags include_dir =
  match include_dir with
  | Some directory when String.trim directory <> "" -> [ "-I" ^ Unix.realpath directory ]
  | Some _ | None -> words (command_text "pkg-config" [ "--cflags"; "sdl3" ])

let probe_source structures =
  let output = Buffer.create 4096 in
  Buffer.add_string output
    "#include <SDL3/SDL.h>\n#include <stddef.h>\n#include <stdio.h>\nint main(void) {\n";
  List.iter
    (fun (structure, fields) ->
      Printf.bprintf output "  printf(\"%s %%zu %%zu\", sizeof(%s), _Alignof(%s));\n" structure
        structure structure;
      List.iter
        (fun field ->
          if field <> "" then
            Printf.bprintf output "  printf(\" %s %%zu\", offsetof(%s, %s));\n" field structure
              field)
        fields;
      Buffer.add_string output "  printf(\"\\n\");\n")
    structures;
  Buffer.add_string output "  return 0;\n}\n";
  Buffer.contents output

let accept ~root ~abi_path ~include_dir ~clang =
  let used = all_reads root in
  let compiler =
    match clang with Some value when String.trim value <> "" -> value | Some _ | None -> "clang"
  in
  let directory = Filename.temp_dir "rays-sdl3-abi-" "" in
  let output =
    Fun.protect
      ~finally:(fun () -> remove_directory directory)
      (fun () ->
        let source = Filename.concat directory "probe.c" in
        let executable = Filename.concat directory "probe" in
        write_file source (probe_source used);
        ignore (command_text compiler (include_flags include_dir @ [ source; "-o"; executable ]));
        command_text executable [])
  in
  let layouts =
    String.split_on_char '\n' output
    |> List.filter (fun line -> line <> "")
    |> List.map (fun line ->
        match words line with
        | name :: size :: alignment :: fields ->
            let rec pairs = function
              | field :: offset :: rest -> (field, int_of_string offset) :: pairs rest
              | [] -> []
              | [ _ ] -> fail "malformed probe line %S" line
            in
            ( name,
              {
                size = int_of_string size;
                alignment = int_of_string alignment;
                offsets = pairs fields;
              } )
        | _ -> fail "malformed probe line %S" line)
  in
  write_abi abi_path layouts;
  Printf.printf "wrote %s: %d structs\n%!" abi_path (List.length layouts)

(* --- command line --- *)

let usage () =
  fail
    "usage: generate.exe emit --root R --component <sdl3|sdl3_image|sdl3_ttf|sdl3_mixer> --lock \
     FILE --abi FILE --output FILE\n\
    \       generate.exe accept [--root R] [--abi FILE] [--include-dir D] [--clang C]"

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  let rec options = function
    | [] -> []
    | name :: value :: rest when String.starts_with ~prefix:"--" name ->
        (name, value) :: options rest
    | _ -> usage ()
  in
  try
    match arguments with
    | "emit" :: rest ->
        let given = options rest in
        let get name =
          match List.assoc_opt name given with Some value -> value | None -> usage ()
        in
        emit ~root:(get "--root") ~lock_path:(get "--lock") ~abi_path:(get "--abi")
          ~spec:(Spec.component (get "--component"))
          ~output:(get "--output")
    | "accept" :: rest ->
        let given = options rest in
        let root = Option.value (List.assoc_opt "--root" given) ~default:"." in
        let abi_path =
          Option.value (List.assoc_opt "--abi" given)
            ~default:(Filename.concat root "lib/sdl3/abi.sexp")
        in
        accept ~root ~abi_path
          ~include_dir:(List.assoc_opt "--include-dir" given)
          ~clang:(List.assoc_opt "--clang" given)
    | _ -> usage ()
  with
  | Failure message | Sys_error message ->
      prerr_endline message;
      exit 1
  | Unix.Unix_error (error, name, argument) ->
      Printf.eprintf "%s(%s): %s\n%!" name argument (Unix.error_message error);
      exit 1
