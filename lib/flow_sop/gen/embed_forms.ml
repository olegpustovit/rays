(* The node library's parsed forms, marshalled into [Node_sources.forms] at build time: every process
   that loads the catalog unmarshals them instead of parsing 2 MB of text. A form that does not parse
   fails the build. Words repeat across the library, so equal strings are one string. *)
let intern =
  let known = Hashtbl.create 4096 in
  fun text ->
    match Hashtbl.find_opt known text with
    | Some shared -> shared
    | None ->
        Hashtbl.add known text text;
        text

let rec shared (form : Flow.Syntax.t) =
  let node : Flow.Syntax.node =
    match form.node with
    | Sym text -> Sym (intern text)
    | Kw text -> Kw (intern text)
    | Num text -> Num (intern text)
    | Str text -> Str (intern text)
    | List children -> List (List.map shared children)
    | Vec children -> Vec (List.map shared children)
    | Map children -> Map (List.map shared children)
    | Quote (kind, inner) -> Quote (kind, shared inner)
  in
  {
    form with
    node;
    notes = List.map intern form.notes;
    meta = List.map intern form.meta;
    tail = List.map intern form.tail;
  }

let () =
  (* sorted: the library's order fixes its forms' ids *)
  let files = List.sort compare (List.tl (Array.to_list Sys.argv)) in
  let parsed =
    List.map
      (fun file ->
        match Flow.Syntax.parse (In_channel.with_open_bin file In_channel.input_all) with
        | Ok forms -> (Filename.basename file, List.map shared forms)
        | Error diagnostic ->
            prerr_endline (file ^ ": " ^ Diagnostic.to_string diagnostic);
            exit 1)
      files
  in
  Printf.printf "let forms = %S\n" (Marshal.to_string (parsed : (string * Flow.Syntax.t list) list) [])
