let fail format = Printf.ksprintf failwith format

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains source needle =
  let n = String.length needle in
  let rec loop i =
    i + n <= String.length source && (String.sub source i n = needle || loop (i + 1))
  in
  loop 0

let () =
  let root = ref "." in
  Arg.parse
    [ ("--root", Arg.Set_string root, "repository root") ]
    (fun value -> raise (Arg.Bad value))
    "check_runtime_licenses";
  let root = Unix.realpath !root in
  let at path = Filename.concat root path in
  let manifest = Yojson.Safe.from_file (at "tools/packaging/runtime_license_manifest.json") in
  let open Yojson.Safe.Util in
  if manifest |> member "schema" |> to_int <> 1 then fail "license manifest schema drift";
  let surfaces = manifest |> member "surfaces" |> to_list in
  List.iter
    (fun item ->
      if
        item |> member "name" |> to_string = ""
        || item |> member "license" |> to_string = ""
        || item |> member "provenance" |> to_string = ""
        || item |> member "bundled" |> to_bool
      then fail "incomplete or bundled license row")
    surfaces;
  let names = List.map (fun item -> item |> member "name" |> to_string) surfaces in
  List.iter
    (fun required ->
      if not (List.mem required names) then fail "missing license surface %s" required)
    [ "runtime"; "SDL3"; "SDL3_image"; "SDL3_ttf"; "SDL3_mixer"; "Metal"; "OGPU" ];
  if not (contains (read (at "LICENSE")) "MIT License") then fail "root MIT license drift";
  let docs = read (at "specification/licenses.md")
  and backend = read (at "specification/backend.md") in
  List.iter
    (fun needle -> if not (contains docs needle) then fail "license docs missing %s" needle)
    [ "runtime"; "zlib license"; "Apple SDK"; "not vendor" ];
  if not (contains backend "native Metal runtime") then fail "architecture docs omit native runtime";
  print_endline
    "Runtime-next license gate: exact surfaces, provenance, non-bundled declarations passed"
