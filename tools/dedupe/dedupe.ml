(* Removes copy-pasted top-level helpers in favour of one shared module.

     dedupe [--dry-run] [--only NAME,...] (--open MODULE | --qualify PREFIX) SHARED.ml FILE.ml...
     dedupe --show FILE.ml NAME        print how one definition is compared
     dedupe [--dry-run] --take FILE.ml...   List.filteri (fun i _ -> i < 7) to List.take 7

   A top-level [let NAME] in FILE goes when it is the same definition as [let NAME] in
   SHARED.ml. "The same" is decided from the compiler's output, not from the text:
   - the two parse trees are equal once locations and doc comments are dropped and the names
     the definition binds itself are numbered in order (so [search]/[loop] spellings match);
   - every identifier, constructor and label in the two definitions resolves, through the
     shapes in the .cmt files, to the same declaration: the same global one, the same local
     one by position, or a helper of the file that was itself matched to SHARED's.
   An identifier the compiler could not resolve keeps the definition, as does a second
   definition of the name in the file or a [val] for it in the file's interface.

   --open MODULE puts [open MODULE] where the first removed helper stood. A file is skipped
   when that open could capture anything: a use of a shared name that resolves outside the
   file. --qualify PREFIX rewrites each use to PREFIX.NAME instead and opens nothing.

   Run [dune build @check] first: a .cmt older than its source is refused. Then add the
   shared library to the dune stanza by hand and rebuild; the compiler and the tests are the
   last check. *)

let read path = In_channel.with_open_bin path In_channel.input_all
let write path text = Out_channel.with_open_bin path (fun channel -> output_string channel text)
let fail format = Printf.ksprintf (fun message -> prerr_endline ("dedupe: " ^ message); exit 2) format

let rec walk directory visit =
  Array.iter (fun name ->
    let path = Filename.concat directory name in
    if Sys.is_directory path then walk path visit else visit path)
    (Sys.readdir directory)

(* ---------- the compiler's view of a source file ---------- *)

(* source path -> its .cmt and whether a ppx rewrote the source first *)
let cmts : (string, Cmt_format.cmt_infos * string * bool) Hashtbl.t = Hashtbl.create 4096
let shapes : (string, Shape.t) Hashtbl.t = Hashtbl.create 4096

(* The project's units, and the standard library's so that [raise] or [String.sub] resolve. *)
let load_cmts () =
  let read ~project path =
    if Filename.check_suffix path ".cmt" then
      match Cmt_format.read_cmt path with
      | exception _ -> ()
      | info ->
          Option.iter (Hashtbl.replace shapes info.cmt_modname) info.cmt_impl_shape;
          if project then
            Option.iter (fun source ->
              if Filename.check_suffix source ".pp.ml" then
                Hashtbl.replace cmts (Filename.chop_suffix source ".pp.ml" ^ ".ml") (info, path, true)
              else Hashtbl.replace cmts source (info, path, false)) info.cmt_sourcefile in
  walk "_build/default" (read ~project:true);
  Array.iter (fun name -> read ~project:false (Filename.concat Config.standard_library name))
    (Sys.readdir Config.standard_library)

module Reduce = Shape_reduce.Make (struct
  let fuel = 10
  let read_unit_shape ~unit_name = Hashtbl.find_opt shapes unit_name
end)

let rec final depth : Shape_reduce.result -> Shape.Uid.t option = function
  | Resolved uid -> Some uid
  | Resolved_alias (_, rest) -> final depth rest
  | Unresolved shape when depth = 0 -> final 1 (Reduce.reduce_for_uid Env.empty shape)
  | Unresolved _ | Approximated _ | Internal_error_missing_uid -> None

type definition = {
  name : string;
  start : int;  (* byte range of the structure item in the source *)
  stop : int;
  item : Parsetree.structure_item;
  uid : Shape.Uid.t option;
}

type source = {
  path : string;
  text : string;
  unit_name : string;
  definitions : definition list;  (* top-level single [let NAME], in order *)
  top_values : (Shape.Uid.t, string) Hashtbl.t;  (* every top-level value *)
  declared : Location.t Shape.Uid.Tbl.t;  (* where a non-value local is declared *)
  occurrences : (int * int * Longident.t * Shape.Uid.t option) array;  (* by start *)
  bound : (string * int) list;  (* every name a top-level [let] binds, with its offset *)
  opens : int list;  (* offsets of top-level [open] and [include] *)
}

let declaration_location : Typedtree.item_declaration -> Location.t = function
  | Value v -> v.val_loc | Value_binding v -> v.vb_loc | Type t -> t.typ_loc
  | Constructor c -> c.cd_loc | Extension_constructor e -> e.ext_loc | Label l -> l.ld_loc
  | Module m -> m.md_loc | Module_substitution m -> m.ms_loc | Module_binding m -> m.mb_loc
  | Module_type m -> m.mtd_loc | Class c -> c.ci_loc | Class_type c -> c.ci_loc

let load path =
  let text = read path in
  let info, cmt, preprocessed = match Hashtbl.find_opt cmts path with
    | Some found -> found
    | None -> fail "%s: no .cmt under _build/default (run dune build @check)" path in
  (* a preprocessed unit's digest is of the ppx output (locations still name the source),
     so its freshness is by time *)
  let fresh = if preprocessed then (Unix.stat cmt).st_mtime >= (Unix.stat path).st_mtime
    else info.cmt_source_digest = Some (Digest.string text) in
  if not fresh then
    fail "%s: its .cmt is older than the source (run dune build @check)" path;
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf path;
  let parsed = Parse.implementation lexbuf in
  let top_values = Hashtbl.create 64 in
  (match info.cmt_annots with
   | Implementation structure ->
       List.iter (fun (item : Typedtree.structure_item) -> match item.str_desc with
         | Tstr_value (_, bindings) ->
             List.iter (fun (id, _, _, uid) -> Hashtbl.replace top_values uid (Ident.name id))
               (Typedtree.let_bound_idents_full bindings)
         | _ -> ()) structure.str_items
   | _ -> fail "%s: its .cmt holds no implementation" path);
  let uid_of name =
    Hashtbl.fold (fun uid bound found -> if bound = name then Some uid else found) top_values None in
  let definitions = List.filter_map (fun (item : Parsetree.structure_item) ->
    match item.pstr_desc with
    | Pstr_value (_, [{ pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; _ }]) ->
        Some { name; start = item.pstr_loc.loc_start.pos_cnum;
               stop = item.pstr_loc.loc_end.pos_cnum; item; uid = uid_of name }
    | _ -> None) parsed in
  let declared = Shape.Uid.Tbl.create 256 in
  Shape.Uid.Tbl.iter (fun uid declaration ->
    Shape.Uid.Tbl.replace declared uid (declaration_location declaration)) info.cmt_uid_to_decl;
  let occurrences = List.filter_map (fun ((lid : Longident.t Location.loc), result) ->
    if lid.loc.loc_ghost then None
    else Some (lid.loc.loc_start.pos_cnum, lid.loc.loc_end.pos_cnum, lid.txt, final 0 result))
    info.cmt_ident_occurrences |> Array.of_list in
  Array.stable_sort compare occurrences;
  let bound = ref [] and opens = ref [] in
  List.iter (fun (item : Parsetree.structure_item) ->
    let start = item.pstr_loc.loc_start.pos_cnum in
    match item.pstr_desc with
    | Pstr_value (_, bindings) ->
        let names = { Ast_iterator.default_iterator with
          pat = (fun self pattern ->
            (match pattern.ppat_desc with
             | Ppat_var name | Ppat_alias (_, name) -> bound := (name.txt, start) :: !bound
             | _ -> ());
            Ast_iterator.default_iterator.pat self pattern);
          expr = (fun _ _ -> ()) } in
        List.iter (fun (binding : Parsetree.value_binding) -> names.pat names binding.pvb_pat)
          bindings
    | Pstr_open _ | Pstr_include _ -> opens := start :: !opens
    | _ -> ()) parsed;
  { path; text; unit_name = info.cmt_modname; definitions; top_values; declared; occurrences;
    bound = !bound; opens = !opens }

(* ---------- one definition, in a form two files can compare ---------- *)

(* Locations and doc comments dropped; the names bound inside numbered by first appearance. *)
let shape_of (item : Parsetree.structure_item) =
  let bound = Hashtbl.create 16 in
  let collect = { Ast_iterator.default_iterator with
    pat = (fun self pattern ->
      (match pattern.ppat_desc with
       | Ppat_var name | Ppat_alias (_, name) -> Hashtbl.replace bound name.txt ()
       | _ -> ());
      Ast_iterator.default_iterator.pat self pattern) } in
  collect.structure_item collect item;
  let numbers = Hashtbl.create 16 in
  let number name =
    if not (Hashtbl.mem bound name) then name
    else match Hashtbl.find_opt numbers name with
      | Some numbered -> numbered
      | None ->
          let numbered = Printf.sprintf "v%d__" (Hashtbl.length numbers) in
          Hashtbl.replace numbers name numbered; numbered in
  let open Ast_mapper in
  let mapper = { default_mapper with
    location = (fun _ _ -> Location.none);
    attributes = (fun self attributes ->
      default_mapper.attributes self (List.filter (fun (attribute : Parsetree.attribute) ->
        not (List.mem attribute.attr_name.txt ["ocaml.doc"; "ocaml.text"])) attributes));
    pat = (fun self pattern ->
      let pattern = match pattern.ppat_desc with
        | Ppat_var name -> { pattern with ppat_desc = Ppat_var { name with txt = number name.txt } }
        | Ppat_alias (inner, name) ->
            { pattern with ppat_desc = Ppat_alias (inner, { name with txt = number name.txt }) }
        | _ -> pattern in
      default_mapper.pat self pattern);
    expr = (fun self expression ->
      let expression = match expression.pexp_desc with
        | Pexp_ident ({ txt = Lident name; _ } as lid) ->
            { expression with pexp_desc = Pexp_ident { lid with txt = Lident (number name) } }
        | _ -> expression in
      default_mapper.expr self { expression with pexp_loc_stack = [] }) } in
  Pprintast.string_of_structure [mapper.structure_item mapper item]

(* What each name in a definition resolves to. [matched] maps the file's own top-level
   helpers that stand for SHARED's to their names. *)
let resolution source ~matched definition =
  let locals = Hashtbl.create 16 and unknown = ref false in
  let inside (location : Location.t) =
    location.loc_start.pos_cnum >= definition.start && location.loc_end.pos_cnum <= definition.stop in
  let tokens = Array.to_list source.occurrences |> List.filter_map (fun (start, stop, lid, uid) ->
    if start < definition.start || stop > definition.stop then None
    else Some (match uid with
      | None -> unknown := true; "?" ^ String.concat "." (Longident.flatten lid)
      | Some (Shape.Uid.Item { comp_unit; _ } as uid) when comp_unit = source.unit_name ->
          (match Hashtbl.find_opt matched uid with
           | Some name -> "shared:" ^ name
           | None ->
               let nested = not (Hashtbl.mem source.top_values uid)
                 && (match Shape.Uid.Tbl.find_opt source.declared uid with
                     | Some location -> inside location
                     | None -> true) in
               if not nested then begin
                 unknown := true;
                 "file:" ^ String.concat "." (Longident.flatten lid)
               end else match Hashtbl.find_opt locals uid with
                 | Some index -> Printf.sprintf "local:%d" index
                 | None ->
                     let index = Hashtbl.length locals in
                     Hashtbl.replace locals uid index; Printf.sprintf "local:%d" index)
      | Some uid -> Format.asprintf "%a" Shape.Uid.print uid)) in
  tokens, !unknown

(* ---------- the edit ---------- *)

let is_blank character = character = ' ' || character = '\t'

(* Drop [start, stop) with the rest of its line and one of the blank lines around it. *)
let cut text (start, stop) replacement =
  let length = String.length text in
  let stop = ref stop in
  while !stop < length && is_blank text.[!stop] do incr stop done;
  if !stop < length && text.[!stop] = '\n' then incr stop;
  let before = String.sub text 0 start and after = String.sub text !stop (length - !stop) in
  let blank_before = before = "" || String.ends_with ~suffix:"\n\n" before in
  if replacement = "" then
    before ^ (if blank_before && String.starts_with ~prefix:"\n" after
      then String.sub after 1 (String.length after - 1) else after)
  else begin
    (* the open joins the opens above it and is followed by a blank line *)
    let trimmed = String.length before - 1 |> fun last ->
      let last = ref last in
      while !last >= 0 && before.[!last] = '\n' do decr last done;
      String.sub before 0 (!last + 1) in
    let last_line = match String.rindex_opt trimmed '\n' with
      | Some index -> String.sub trimmed (index + 1) (String.length trimmed - index - 1)
      | None -> trimmed in
    let before = if String.starts_with ~prefix:"open " last_line then trimmed ^ "\n" else before in
    before ^ replacement
    ^ (if after = "" || String.starts_with ~prefix:"\n" after then after else "\n" ^ after)
  end

(* Edits never overlap; applied from the end of the file so earlier offsets stay valid.
   [`Line] removes a definition with its line, [`Text] replaces a name in place. *)
let apply text edits =
  List.sort (fun ((a, _), _, _) ((b, _), _, _) -> compare b a) edits
  |> List.fold_left (fun text ((start, stop), replacement, kind) -> match kind with
      | `Line -> cut text (start, stop) replacement
      | `Text -> String.sub text 0 start ^ replacement
          ^ String.sub text stop (String.length text - stop)) text

type mode = Open of string | Qualify of string

let dedupe ~dry_run ~mode ~only shared paths =
  let shared_names = Hashtbl.create 32 in
  let shared_matched = Hashtbl.create 32 in
  List.iter (fun definition ->
    Hashtbl.replace shared_names definition.name definition;
    Option.iter (fun uid -> Hashtbl.replace shared_matched uid definition.name) definition.uid)
    shared.definitions;
  let shared_form = Hashtbl.create 32 in
  List.iter (fun definition -> if only = [] || List.mem definition.name only then begin
    let tokens, unknown = resolution shared ~matched:shared_matched definition in
    if unknown then fail "%s: %s names something the compiler did not resolve: %s"
        shared.path definition.name (String.concat " " tokens);
    Hashtbl.replace shared_form definition.name (shape_of definition.item, tokens)
  end) shared.definitions;
  let removed_total = ref 0 and lines_total = ref 0 in
  List.iter (fun path ->
    let source = load path in
    let count name = List.length (List.filter (fun d -> d.name = name) source.definitions) in
    (* a helper the file's interface exports stays *)
    let exported =
      let interface = path ^ "i" in
      if not (Sys.file_exists interface) then []
      else List.filter_map (fun (item : Parsetree.signature_item) -> match item.psig_desc with
        | Psig_value value -> Some value.pval_name.txt
        | _ -> None) (Parse.interface (Lexing.from_string (read interface))) in
    let matched = Hashtbl.create 16 in
    let removed = List.filter (fun definition ->
      match Hashtbl.find_opt shared_form definition.name, definition.uid with
      | Some (form, tokens), Some uid
        when count definition.name = 1 && not (List.mem definition.name exported) ->
          let own_tokens, unknown = resolution source ~matched definition in
          let same = not unknown && shape_of definition.item = form && own_tokens = tokens in
          if same then Hashtbl.replace matched uid definition.name;
          same
      | _ -> false) source.definitions in
    (* a shared name used in the file and resolved outside it: [open] would capture it *)
    let is_removed name = List.exists (fun d -> d.name = name) removed in
    let captured = match mode, removed with
      | Qualify _, _ | _, [] -> []
      | Open _, first :: _ ->
          let last = List.fold_left (fun stop d -> max stop d.start) first.start removed in
          (* a use of a shared name that resolves outside the file *)
          (Array.to_list source.occurrences |> List.filter_map (fun (_, _, lid, uid) ->
            match (lid : Longident.t), uid with
            | Lident name, Some (Shape.Uid.Item { comp_unit; _ })
              when Hashtbl.mem shared_names name && comp_unit <> source.unit_name -> Some name
            | Lident name, (None | Some (Predef _ | Internal | Compilation_unit _))
              when Hashtbl.mem shared_names name -> Some name
            | _ -> None))
          (* a helper the file keeps, bound above where the open goes: the open would hide it *)
          @ List.filter_map (fun (name, start) ->
              if Hashtbl.mem shared_names name && not (is_removed name) && start < first.start
              then Some name else None) source.bound
          (* an open between the removed helpers could bind one of their names *)
          @ List.filter_map (fun start ->
              if start > first.start && start < last then Some "an open between the helpers"
              else None) source.opens in
    if removed = [] then ()
    else if captured <> [] then
      Printf.printf "%s: skipped, the open would capture %s\n" path
        (String.concat ", " (List.sort_uniq compare captured))
    else begin
      let lines = List.fold_left (fun total definition ->
        total + 1 + List.length (String.split_on_char '\n'
          (String.sub source.text definition.start (definition.stop - definition.start))) - 1)
        0 removed in
      removed_total := !removed_total + List.length removed;
      lines_total := !lines_total + lines;
      Printf.printf "%s: %s\n" path (String.concat " " (List.map (fun d -> d.name) removed));
      let first = List.hd removed in
      let edits = List.map (fun definition ->
        (definition.start, definition.stop),
        (match mode with
         | Open name when definition == first -> "open " ^ name ^ "\n"
         | Open _ | Qualify _ -> ""), `Line) removed in
      let uses = match mode with
        | Open _ -> []
        | Qualify prefix -> Array.to_list source.occurrences |> List.filter_map
            (fun (start, stop, lid, uid) ->
              match (lid : Longident.t), Option.bind uid (Hashtbl.find_opt matched) with
              | Lident name, Some shared_name when name = shared_name
                  && not (List.exists (fun d -> start >= d.start && stop <= d.stop) removed) ->
                  Some ((start, stop), prefix ^ "." ^ name, `Text)
              | _ -> None) in
      if not dry_run then write path (apply source.text (edits @ uses))
    end) paths;
  Printf.printf "%d definitions, %d lines\n" !removed_total !lines_total

(* [List.filteri (fun i _ -> i < N) xs] with a literal N >= 0 is [List.take N xs] (OCaml 5.3).
   A computed bound stays: [List.take] raises on a negative one where the filter gives []. *)
let take ~dry_run paths =
  let total = ref 0 in
  List.iter (fun path ->
    let source = load path in
    let stdlib_list start = Array.exists (fun (at, _, _, uid) -> at = start && match uid with
      | Some (Shape.Uid.Item { comp_unit = "Stdlib__List"; _ }) -> true
      | _ -> false) source.occurrences in
    let edits = ref [] in
    let iterator = { Ast_iterator.default_iterator with
      expr = (fun self expression ->
        (match expression.pexp_desc with
         | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Ldot (Lident "List", "filteri"); loc }; _ },
             (Nolabel, ({ pexp_desc = Pexp_function (
               [{ pparam_desc = Pparam_val (Nolabel, None,
                    { ppat_desc = Ppat_var { txt = index; _ }; _ }); _ };
                { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_any; _ }); _ }],
               None,
               Pfunction_body { pexp_desc = Pexp_apply (
                 { pexp_desc = Pexp_ident { txt = Lident "<"; _ }; _ },
                 [Nolabel, { pexp_desc = Pexp_ident { txt = Lident used; _ }; _ };
                  Nolabel, { pexp_desc = Pexp_constant
                    { pconst_desc = Pconst_integer (bound, None); _ }; _ }]); _ }); _ }
               as predicate)) :: _)
           when index = used && stdlib_list loc.loc_start.pos_cnum ->
             (* the predicate's range with its parentheses, wherever the parser put them *)
             let start = ref predicate.pexp_loc.loc_start.pos_cnum
             and stop = ref predicate.pexp_loc.loc_end.pos_cnum in
             if source.text.[!start] <> '(' then begin
               while source.text.[!start] <> '(' do decr start done;
               while source.text.[!stop - 1] <> ')' do incr stop done
             end;
             edits := ((loc.loc_start.pos_cnum, loc.loc_end.pos_cnum), "List.take", `Text)
               :: ((!start, !stop), " " ^ bound ^ " ", `Text) :: !edits
         | _ -> ());
        Ast_iterator.default_iterator.expr self expression) } in
    let lexbuf = Lexing.from_string source.text in
    Lexing.set_filename lexbuf path;
    iterator.structure iterator (Parse.implementation lexbuf);
    if !edits <> [] then begin
      Printf.printf "%s: %d\n" path (List.length !edits / 2);
      total := !total + (List.length !edits / 2);
      if not dry_run then write path (apply source.text !edits)
    end) paths;
  Printf.printf "%d calls\n" !total

let show path name =
  let source = load path in
  let matched = Hashtbl.create 16 in
  Hashtbl.iter (fun uid name -> Hashtbl.replace matched uid name) source.top_values;
  List.iter (fun definition -> if definition.name = name then begin
    print_endline (shape_of definition.item);
    let tokens, _ = resolution source ~matched definition in
    print_endline (String.concat "\n" tokens)
  end) source.definitions

let () =
  let dry_run = ref false and mode = ref None and rest = ref [] and showing = ref false
  and only = ref [] and taking = ref false in
  Arg.parse [
    "--dry-run", Arg.Set dry_run, " report without writing";
    "--open", Arg.String (fun name -> mode := Some (Open name)), "MODULE open it in each file";
    "--qualify", Arg.String (fun prefix -> mode := Some (Qualify prefix)), "PREFIX name each use";
    "--only", Arg.String (fun names -> only := String.split_on_char ',' names),
      "NAME,... consider these definitions of SHARED only";
    "--show", Arg.Set showing, " print one definition's compared form";
    "--take", Arg.Set taking, " rewrite List.filteri prefixes to List.take in FILE...";
  ] (fun argument -> rest := argument :: !rest)
    "dedupe [--dry-run] (--open MODULE | --qualify PREFIX) SHARED.ml FILE.ml...";
  load_cmts ();
  match !showing, !mode, List.rev !rest with
  | _ when !taking -> take ~dry_run:!dry_run (List.rev !rest)
  | true, _, [path; name] -> show path name
  | false, Some mode, shared :: paths ->
      let shared = load shared in
      dedupe ~dry_run:!dry_run ~mode ~only:!only shared (List.filter (fun path -> path <> shared.path) paths)
  | _ -> fail "usage: dedupe [--dry-run] (--open MODULE | --qualify PREFIX) SHARED.ml FILE.ml..."
