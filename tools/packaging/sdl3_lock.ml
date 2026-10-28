type version = int * int * int
type sexp = Atom of string | List of sexp list

let fail format = Printf.ksprintf failwith format

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let write_file path contents =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () -> output_string output contents)

let parse_sexps text =
  let length = String.length text in
  let rec skip index =
    if index >= length then index
    else
      match text.[index] with
      | ' ' | '\t' | '\n' | '\r' -> skip (index + 1)
      | ';' ->
          let rec line index =
            if index >= length || text.[index] = '\n' then index else line (index + 1)
          in
          skip (line index)
      | _ -> index
  in
  let atom_end index =
    let rec go index =
      if index >= length then index
      else
        match text.[index] with
        | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' -> index
        | _ -> go (index + 1)
    in
    go index
  in
  let rec expression index =
    let index = skip index in
    if index >= length then fail "unexpected end of s-expression"
    else if text.[index] = '(' then items (index + 1) []
    else if text.[index] = ')' then fail "unbalanced ) at offset %d" index
    else
      let stop = atom_end index in
      (Atom (String.sub text index (stop - index)), stop)
  and items index acc =
    let index = skip index in
    if index >= length then fail "missing ) at end of file"
    else if text.[index] = ')' then (List (List.rev acc), index + 1)
    else
      let value, index = expression index in
      items index (value :: acc)
  in
  let rec top index acc =
    let index = skip index in
    if index >= length then List.rev acc
    else
      let value, index = expression index in
      top index (value :: acc)
  in
  top 0 []

let parse_version value : version =
  match String.split_on_char '.' (String.trim value) with
  | major :: minor :: patch :: _ -> (
      try (int_of_string major, int_of_string minor, int_of_string patch)
      with Failure _ -> fail "non-numeric version %S" value)
  | _ -> fail "expected a major.minor.patch version, got %S" value

let version_string (major, minor, patch) = Printf.sprintf "%d.%d.%d" major minor patch
let version_number (major, minor, patch) = (major * 1_000_000) + (minor * 1_000) + patch
let stable (_, minor, patch) = minor mod 2 = 0 && patch mod 2 = 0

type entry = { floor : version; tested : version }

let field name fields =
  match
    List.find_map
      (function List [ Atom key; Atom value ] when key = name -> Some value | _ -> None)
      fields
  with
  | Some value -> parse_version value
  | None -> fail "lock entry lacks (%s X.Y.Z)" name

let read path =
  match parse_sexps (read_file path) with
  | [ List entries ] ->
      List.map
        (function
          | List (Atom key :: fields) ->
              (key, { floor = field "floor" fields; tested = field "tested" fields })
          | _ -> fail "%s: malformed lock entry" path)
        entries
  | _ -> fail "%s: expected one list of component entries" path

let find lock key =
  match List.assoc_opt key lock with
  | Some entry -> entry
  | None -> fail "sdl3.lock has no entry for %s" key

let key_of_package package = String.map (function '-' -> '_' | c -> c) package

let set_tested text ~key version =
  let lines = String.split_on_char '\n' text in
  let marker = "(" ^ key ^ " " in
  let contains line needle =
    let n = String.length needle and l = String.length line in
    let rec go i = i + n <= l && (String.sub line i n = needle || go (i + 1)) in
    go 0
  in
  let replaced = ref false in
  let rewrite line =
    if !replaced || (not (contains line marker)) || not (contains line "(tested ") then line
    else begin
      replaced := true;
      let start =
        let n = String.length "(tested " in
        let rec go i = if String.sub line i n = "(tested " then i else go (i + 1) in
        go 0
      in
      let stop = String.index_from line start ')' in
      String.sub line 0 start ^ "(tested " ^ version_string version
      ^ String.sub line stop (String.length line - stop)
    end
  in
  let result = String.concat "\n" (List.map rewrite lines) in
  if not !replaced then fail "sdl3.lock has no one-line entry for %s" key;
  result

let check_installed ~lock ~key ~compiled ~linked =
  let { floor; _ } = find lock key in
  let series (major, minor, _) = (major, minor) in
  if compare compiled floor < 0 then
    Error
      (Printf.sprintf "%s headers %s are older than the floor %s" key (version_string compiled)
         (version_string floor))
  else if compare linked floor < 0 then
    Error
      (Printf.sprintf "linked %s %s is older than the floor %s" key (version_string linked)
         (version_string floor))
  else if compare linked compiled < 0 then
    Error
      (Printf.sprintf "linked %s %s is older than its headers %s" key (version_string linked)
         (version_string compiled))
  else if series linked <> series compiled then
    Error
      (Printf.sprintf "linked %s %s and headers %s are different series" key (version_string linked)
         (version_string compiled))
  else Ok ()
