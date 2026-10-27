(* Registry inspection and unused bridge macro removal. *)
open Parsetree

let read path = In_channel.with_open_bin path In_channel.input_all

let write path body =
  let body = Str.global_replace (Str.regexp "[ \t]+$") "" body in
  let body = String.trim body ^ "\n" in
  Out_channel.with_open_bin path (fun out -> output_string out body)

let parse text = Parse.implementation (Lexing.from_string text)

let contains text part =
  try
    ignore (Str.search_forward (Str.regexp_string part) text 0);
    true
  with Not_found -> false

let replace source edits =
  let buffer = Buffer.create (String.length source) in
  let position =
    List.fold_left
      (fun position (s, e, replacement) ->
        Buffer.add_substring buffer source position (s - position);
        Buffer.add_string buffer replacement;
        e)
      0 (List.sort compare edits)
  in
  Buffer.add_substring buffer source position (String.length source - position);
  Buffer.contents buffer

let audit () =
  let tree = parse (read "lib/metal/gen/registry.ml") in
  let counts = Hashtbl.create 4 in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun self expression ->
          (match expression.pexp_desc with
          | Pexp_construct
              ( {
                  txt =
                    Longident.Lident (("Native" | "Class_method" | "Method" | "Property") as kind);
                  _;
                },
                Some { pexp_desc = Pexp_record (fields, _); _ } ) ->
              let string name =
                List.find_map
                  (fun (field, value) ->
                    if Longident.last field.Location.txt = name then
                      match value.pexp_desc with
                      | Pexp_constant { pconst_desc = Pconst_string (text, _, _); _ } -> Some text
                      | _ -> None
                    else None)
                  fields
                |> Option.value ~default:""
              in
              Hashtbl.replace counts kind
                (1 + Option.value (Hashtbl.find_opt counts kind) ~default:0);
              Printf.printf "%s\t%s\t%s\n" kind (string "ocaml") (string "reason")
          | _ -> ());
          Ast_iterator.default_iterator.expr self expression);
    }
  in
  iterator.structure iterator tree;
  Hashtbl.iter (fun kind count -> Printf.printf "%s: %d\n" kind count) counts

let rec drop_unused_macros path =
  let source = read path in
  let define = Str.regexp "^#define[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" in
  let undef = Str.regexp "^#undef[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)[^\n]*" in
  let ranges = Hashtbl.create 32 in
  let add name range =
    Hashtbl.replace ranges name (range :: Option.value (Hashtbl.find_opt ranges name) ~default:[])
  in
  let rec scan re definition offset =
    match Str.search_forward re source offset with
    | start ->
        let name = Str.matched_group 1 source in
        let rec line_end offset =
          match String.index_from_opt source offset '\n' with
          | None -> String.length source
          | Some stop when definition && stop > 0 && source.[stop - 1] = '\\' -> line_end (stop + 1)
          | Some stop -> stop + 1
        in
        let stop = line_end (Str.match_end ()) in
        add name (start, stop);
        scan re definition stop
    | exception Not_found -> ()
  in
  scan define true 0;
  scan undef false 0;
  let edits = ref [] in
  Hashtbl.iter
    (fun name locations ->
      let rest = replace source (List.map (fun (s, e) -> (s, e, "")) locations) in
      if
        List.exists (fun prefix -> String.starts_with ~prefix name) [ "RAYS_"; "COMPUTE_"; "C35_" ]
        && not
             (try
                ignore (Str.search_forward (Str.regexp ("\\b" ^ name ^ "\\b")) rest 0);
                true
              with Not_found -> false)
      then edits := List.map (fun (s, e) -> (s, e, "")) locations @ !edits)
    ranges;
  if !edits <> [] then (
    Printf.printf "removed %d unused macro directives\n%!" (List.length !edits);
    write path (replace source !edits);
    drop_unused_macros path)

let self_test () =
  let path = Filename.temp_file "metal-macros" ".mm" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      write path
        "#define CAML_NAME_SPACE\n\
         #define RAYS_INNER(x) x\n\
         #define RAYS_OUTER(x) RAYS_INNER(x)\n\
         #define RAYS_LIVE(x) x\n\
         #define RAYS_DEAD(x) \\\n\
         +  { x; }\n\
         #undef RAYS_DEAD\n\
         RAYS_LIVE(1)\n";
      drop_unused_macros path;
      let result = read path in
      assert (contains result "CAML_NAME_SPACE");
      assert (contains result "#define RAYS_LIVE");
      assert (not (contains result "RAYS_INNER"));
      assert (not (contains result "RAYS_DEAD")))
