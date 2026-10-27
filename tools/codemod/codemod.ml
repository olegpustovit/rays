(* Compiler-driven dead-code codemods.

   dead-exports [--users-exclude SUBSTR]... DIR...
     Reads every .cmt under _build/default, resolves each identifier
     occurrence to its defining uid through compiler shapes, and prints
     "file.mli<TAB>Path.name" for every value exported by a unit under DIR
     that no other source file references. A unit passed to a functor or
     packed as a first-class module counts as used whole. An exclude written
     "!SUBSTR" puts back the files containing it (a test that stays a user).
   dead-fields [--apply] [--users-exclude SUBSTR]... [--skip TYPE.FIELD]... DIR...
     Prints "file.mli<TAB>type.field" for every field of a record exported
     by a unit under DIR that no implementation reads; --apply removes it
     and its [field = e] in every record literal. --skip keeps a field that
     is unread on purpose.
   dead-types DIR...
     Prints "file.mli<TAB>Path.type" for every exported type that no
     implementation or interface names (report only).
   dead-optionals [--users-exclude SUBSTR]... DIR...
     Prints "file.mli<TAB>Path.name<TAB>?label" for every optional parameter
     of an exported value that no application passes (report only).
   drop-optionals [--users-exclude SUBSTR]... DIR...
     Removes those parameters: the signature loses them and the function
     binds the default in its body.
   drop-vals FILE.mli NAME...     Remove [val]/[external] items (with docs);
                                  "module:Path" a submodule, "type:Path.t" a
                                  type declared on its own.
   drop-unused < build-log        Remove the let bindings named by warning 32
                                  (unused value) in a dune build log.
   rename [--dry-run] OLD NEW     Rename the project: old/Old/OLD become
                                  new/New/NEW in file contents and paths.

   The compiler is the oracle: prune with drop-vals, rebuild, feed the log to
   drop-unused, repeat until the build is clean. *)

let read_file path = In_channel.with_open_bin path In_channel.input_all
let write_file path s = Out_channel.with_open_bin path (fun oc -> output_string oc s)

let source_file path =
  if Filename.check_suffix path ".pp.ml" then Filename.chop_suffix path ".pp.ml" ^ ".ml" else path

let rec walk dir f =
  Array.iter
    (fun name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then (
        if name.[0] <> '.' || Filename.check_suffix name "objs" || name = "byte" then walk path f)
      else f path)
    (Sys.readdir dir)

(* ---------- dead-exports ---------- *)

(* [--users-exclude S] drops files whose path contains S; a pattern written
   [!S] puts back the files that contain S (an exception to the others) *)
let excluded_by excludes file =
  let has s =
    let n = String.length s and m = String.length file in
    let rec go k = k + n <= m && (String.sub file k n = s || go (k + 1)) in
    go 0
  in
  let kept, dropped = List.partition (fun s -> s <> "" && s.[0] = '!') excludes in
  List.exists has dropped
  && not (List.exists (fun s -> has (String.sub s 1 (String.length s - 1))) kept)

let dead_exports ?(modules = false) ~excludes dirs =
  let build = "_build/default" in
  let cmts = ref [] in
  walk build (fun p -> if Filename.check_suffix p ".cmt" then cmts := p :: !cmts);
  let infos =
    List.filter_map
      (fun p -> match Cmt_format.read_cmt p with info -> Some (p, info) | exception _ -> None)
      !cmts
  in
  let shapes = Hashtbl.create 4096 in
  List.iter
    (fun (_, (i : Cmt_format.cmt_infos)) ->
      Option.iter (Hashtbl.replace shapes i.cmt_modname) i.cmt_impl_shape)
    infos;
  let source (i : Cmt_format.cmt_infos) = source_file (Option.value i.cmt_sourcefile ~default:"") in
  let excluded = excluded_by excludes in
  (* uid -> source files that reference it *)
  let users : (Shape.Uid.t, string list) Hashtbl.t = Hashtbl.create 65536 in
  let unresolved_names = Hashtbl.create 4096 in
  (* "Module.name" -> files naming it, however it resolved: recursive modules
     resolve to their signature uid, so their values fall back to names *)
  let qualified : (string, string list) Hashtbl.t = Hashtbl.create 65536 in
  (* units applied to a functor or packed as a first-class module: their
     values are reached through the parameter's signature, which no
     identifier names, so the whole unit counts as used *)
  let through_signature : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (_, (i : Cmt_format.cmt_infos)) ->
      match i.cmt_annots with
      | Implementation tree when not (excluded (source i)) ->
          let rec named (m : Typedtree.module_expr) =
            match m.mod_desc with
            | Tmod_ident (path, _) ->
                let name = Path.last path in
                (* [Lib__Unit] and [Unit] name the same unit *)
                let unit_ =
                  match List.rev (Str.split (Str.regexp_string "__") name) with
                  | u :: _ -> u
                  | [] -> name
                in
                Hashtbl.replace through_signature unit_ ()
            | Tmod_constraint (m, _, _, _) -> named m
            | _ -> ()
          in
          let it =
            {
              Tast_iterator.default_iterator with
              module_expr =
                (fun self (m : Typedtree.module_expr) ->
                  (match m.mod_desc with Tmod_apply (_, arg, _) -> named arg | _ -> ());
                  Tast_iterator.default_iterator.module_expr self m);
              expr =
                (fun self (e : Typedtree.expression) ->
                  (match e.exp_desc with Texp_pack m -> named m | _ -> ());
                  Tast_iterator.default_iterator.expr self e);
            }
          in
          it.structure it tree
      | _ -> ())
    infos;
  List.iter
    (fun (_, (i : Cmt_format.cmt_infos)) ->
      let file = source i in
      if not (excluded file) then begin
        let module R = Shape_reduce.Make (struct
          let fuel = 10
          let read_unit_shape ~unit_name = Hashtbl.find_opt shapes unit_name
        end) in
        let rec uids depth (r : Shape_reduce.result) =
          match r with
          | Resolved u | Approximated (Some u) -> [ u ]
          | Resolved_alias (u, r) -> u :: uids depth r
          | Unresolved s when depth = 0 -> uids 1 (R.reduce_for_uid Env.empty s)
          | _ -> []
        in
        List.iter
          (fun ((lid : Longident.t Location.loc), r) ->
            (match List.rev (Longident.flatten lid.txt) with
            | name :: m :: _ ->
                let k = m ^ "." ^ name in
                let prev = Option.value (Hashtbl.find_opt qualified k) ~default:[] in
                if not (List.mem file prev) then Hashtbl.replace qualified k (file :: prev)
            | _ -> ());
            match uids 0 r with
            | [] ->
                (* keyed "Module.name" when qualified, else the bare name *)
                let key =
                  match List.rev (Longident.flatten lid.txt) with
                  | name :: m :: _ -> m ^ "." ^ name
                  | name :: [] -> name
                  | [] -> ""
                in
                Hashtbl.replace unresolved_names key ()
            | us ->
                List.iter
                  (fun u ->
                    let prev = Option.value (Hashtbl.find_opt users u) ~default:[] in
                    if not (List.mem file prev) then Hashtbl.replace users u (file :: prev))
                  us)
          i.cmt_ident_occurrences
      end)
    infos;
  let under file =
    List.exists
      (fun d ->
        let d = if Filename.check_suffix d "/" then d else d ^ "/" in
        String.length file > String.length d && String.sub file 0 (String.length d) = d)
      dirs
  in
  let rec head s = match (s : Shape.t).desc with Alias s -> head s | _ -> s in
  let find shape name kind =
    match (head shape).desc with
    | Struct map -> Shape.Item.Map.find_opt (Shape.Item.make name kind) map
    | _ -> None
  in
  let results = ref [] in
  List.iter
    (fun (p, (i : Cmt_format.cmt_infos)) ->
      let ml = source i in
      let unit_name = String.capitalize_ascii (Filename.remove_extension (Filename.basename ml)) in
      let cmti = Filename.chop_suffix p ".cmt" ^ ".cmti" in
      if Hashtbl.mem through_signature unit_name then ()
      else if under ml && Filename.check_suffix ml ".ml" && Sys.file_exists cmti then
        match (i.cmt_impl_shape, (Cmt_format.read_cmt cmti).cmt_annots) with
        | Some shape, Interface sg ->
            let mli = Filename.chop_suffix ml ".ml" ^ ".mli" in
            if Sys.file_exists mli then
              (* returns (values, dead values) under this signature; with
             [modules], a submodule whose values are all dead is reported
             whole as "module:Path" instead of value by value *)
              let rec scan ?(in_rec = false) prefix shape items =
                List.fold_left
                  (fun (total, dead) -> function
                    | Types.Sig_value (id, _, _) -> (
                        let name = Ident.name id in
                        match find shape name Shape.Sig_component_kind.Value with
                        | Some { uid = Some uid; _ } ->
                            let used =
                              match Hashtbl.find_opt users uid with
                              | Some files -> List.exists (fun f -> f <> ml) files
                              | None -> false
                            in
                            let owner =
                              match List.rev (String.split_on_char '.' prefix) with
                              | "" :: m :: _ -> m
                              | _ -> unit_name
                            in
                            let used =
                              used
                              || in_rec
                                 &&
                                 match Hashtbl.find_opt qualified (owner ^ "." ^ name) with
                                 | Some files -> List.exists (fun f -> f <> ml) files
                                 | None -> false
                            in
                            if
                              (not used)
                              && (not (Hashtbl.mem unresolved_names name))
                              && not (Hashtbl.mem unresolved_names (owner ^ "." ^ name))
                            then begin
                              results := (mli, prefix ^ name) :: !results;
                              (total + 1, dead + 1)
                            end
                            else (total + 1, dead)
                        | _ -> (total + 1, dead))
                    | Sig_module (id, _, { md_type = Mty_signature items; _ }, rec_status, _) -> (
                        match find shape (Ident.name id) Shape.Sig_component_kind.Module with
                        | Some sub ->
                            let path = prefix ^ Ident.name id in
                            let before = !results in
                            let in_rec = in_rec || rec_status <> Types.Trec_not in
                            let t, d = scan ~in_rec (path ^ ".") sub items in
                            if modules && t > 0 && t = d then
                              results := (mli, "module:" ^ path) :: before;
                            (total + t, dead + d)
                        | None -> (total, dead))
                    | _ -> (total, dead))
                  (0, 0) items
              in
              ignore (scan "" shape sg.sig_type)
        | _ -> ()
      else if under ml && Filename.check_suffix ml ".ml" && Sys.file_exists ml && not (excluded ml)
      then
        (* no .mli: every top-level value is an export, reported against the .ml *)
        match i.cmt_impl_shape with
        | Some shape -> (
            match (head shape).desc with
            | Struct map ->
                Shape.Item.Map.iter
                  (fun item (s : Shape.t) ->
                    if Shape.Item.kind item = Shape.Sig_component_kind.Value then
                      match s.uid with
                      | Some uid ->
                          let name = Shape.Item.name item in
                          (* without an interface, a use in the same file counts *)
                          let used = Hashtbl.mem users uid in
                          if
                            (not used)
                            && (not (Hashtbl.mem unresolved_names name))
                            && not (Hashtbl.mem unresolved_names (unit_name ^ "." ^ name))
                          then results := (ml, name) :: !results
                      | None -> ())
                  map
            | _ -> ())
        | None -> ())
    infos;
  List.sort compare !results

(* ---------- text removal ---------- *)

let parse_with f path =
  let src = read_file path in
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf path;
  Lexer.init ();
  (src, f lexbuf)

(* Byte range of an item plus its attached doc comments, widened to whole
   lines when nothing else shares them. *)
let range src (loc : Location.t) (attrs : Parsetree.attributes) =
  let s =
    List.fold_left
      (fun a (at : Parsetree.attribute) -> min a at.attr_loc.loc_start.pos_cnum)
      loc.loc_start.pos_cnum attrs
  in
  let e =
    List.fold_left
      (fun a (at : Parsetree.attribute) -> max a at.attr_loc.loc_end.pos_cnum)
      loc.loc_end.pos_cnum attrs
  in
  let rec back i = if i > 0 && (src.[i - 1] = ' ' || src.[i - 1] = '\t') then back (i - 1) else i in
  let rec fwd i =
    if i < String.length src && (src.[i] = ' ' || src.[i] = '\t') then fwd (i + 1) else i
  in
  let s' = back s and e' = fwd e in
  if (s' = 0 || src.[s' - 1] = '\n') && (e' >= String.length src || src.[e'] = '\n') then
    (s', min (String.length src) (e' + 1))
  else (s, e)

let remove_ranges ?(subst = []) path src ranges =
  let edits = List.sort_uniq compare (List.map (fun (s, e) -> (s, e, "")) ranges @ subst) in
  let b = Buffer.create (String.length src) in
  let pos =
    List.fold_left
      (fun pos (s, e, text) ->
        if s < pos then max pos e
        else (
          Buffer.add_string b (String.sub src pos (s - pos));
          Buffer.add_string b text;
          e))
      0 edits
  in
  Buffer.add_string b (String.sub src pos (String.length src - pos));
  (* collapse runs of blank lines left behind *)
  let out = Buffer.contents b in
  let out = Str.global_replace (Str.regexp "[ \t]+$") "" out in
  let re = Str.regexp "\n\n\n+" in
  let out = Str.global_replace (Str.regexp "\n\n+\\'") "\n" out in
  write_file path (Str.global_replace re "\n\n" out)

let drop_vals path names =
  let src, sg = parse_with Parse.interface path in
  let ranges = ref [] and found = ref [] in
  (* a module whose types a type declaration elsewhere still names stays *)
  let type_refs = Hashtbl.create 16 in
  let it =
    {
      Ast_iterator.default_iterator with
      type_declaration =
        (fun self td ->
          let inner =
            {
              Ast_iterator.default_iterator with
              typ =
                (fun self' (t : Parsetree.core_type) ->
                  (match t.ptyp_desc with
                  | Ptyp_constr ({ txt; _ }, _) -> (
                      match Longident.flatten txt with
                      | m :: _ :: _ -> Hashtbl.replace type_refs m ()
                      | _ -> ())
                  | _ -> ());
                  Ast_iterator.default_iterator.typ self' t);
            }
          in
          inner.type_declaration inner td;
          Ast_iterator.default_iterator.type_declaration self td);
    }
  in
  it.signature it sg;
  (* ... and so does one a live val (one not being dropped) still names *)
  let refs_of_type (t : Parsetree.core_type) =
    let inner =
      {
        Ast_iterator.default_iterator with
        typ =
          (fun self (t : Parsetree.core_type) ->
            (match t.ptyp_desc with
            | Ptyp_constr ({ txt; _ }, _) -> (
                match Longident.flatten txt with
                | m :: _ :: _ -> Hashtbl.replace type_refs m ()
                | _ -> ())
            | _ -> ());
            Ast_iterator.default_iterator.typ self t);
      }
    in
    inner.typ inner t
  in
  let rec live prefix items =
    List.iter
      (fun (item : Parsetree.signature_item) ->
        match item.psig_desc with
        | Psig_value vd when not (List.mem (prefix ^ vd.pval_name.txt) names) ->
            refs_of_type vd.pval_type
        | Psig_module
            { pmd_name = { txt = Some m; _ }; pmd_type = { pmty_desc = Pmty_signature s; _ }; _ }
          when not (List.mem ("module:" ^ prefix ^ m) names) ->
            live (prefix ^ m ^ ".") s
        | Psig_recmodule mds ->
            List.iter
              (function
                | {
                    Parsetree.pmd_name = { txt = Some m; _ };
                    pmd_type = { pmty_desc = Pmty_signature s; _ };
                    _;
                  }
                  when not (List.mem ("module:" ^ prefix ^ m) names) ->
                    live (prefix ^ m ^ ".") s
                | _ -> ())
              mds
        | _ -> ())
      items
  in
  live "" sg;
  let kept = ref [] in
  let names =
    List.filter
      (fun n ->
        let keep =
          String.length n > 7
          && String.sub n 0 7 = "module:"
          && Hashtbl.mem type_refs
               (List.hd
                  (List.rev (String.split_on_char '.' (String.sub n 7 (String.length n - 7)))))
        in
        if keep then begin
          Printf.eprintf "drop-vals: %s: %s kept for its types; its values go\n" path n;
          kept := (String.sub n 7 (String.length n - 7) ^ ".") :: !kept
        end;
        not keep)
      names
  in
  let in_kept prefix = List.exists (fun k -> String.starts_with ~prefix:k prefix) !kept in
  let dead_module prefix m =
    let n = "module:" ^ prefix ^ m in
    if List.mem n names then (
      found := n :: !found;
      true)
    else false
  in
  let and_before p =
    let rec go i = if i >= 3 && String.sub src (i - 3) 3 = "and" then i - 3 else go (i - 1) in
    go p
  in
  let rec go prefix items =
    List.iter
      (fun (item : Parsetree.signature_item) ->
        match item.psig_desc with
        | Psig_value vd when in_kept prefix ->
            found := (prefix ^ vd.pval_name.txt) :: !found;
            ranges := range src item.psig_loc vd.pval_attributes :: !ranges
        | Psig_value vd when List.mem (prefix ^ vd.pval_name.txt) names ->
            found := (prefix ^ vd.pval_name.txt) :: !found;
            ranges := range src item.psig_loc vd.pval_attributes :: !ranges
        | Psig_type (_, [ td ]) when List.mem ("type:" ^ prefix ^ td.ptype_name.txt) names ->
            found := ("type:" ^ prefix ^ td.ptype_name.txt) :: !found;
            ranges := range src item.psig_loc td.ptype_attributes :: !ranges
        | Psig_module { pmd_name = { txt = Some m; _ }; pmd_attributes; _ }
          when dead_module prefix m ->
            ranges := range src item.psig_loc pmd_attributes :: !ranges
        | Psig_module
            { pmd_name = { txt = Some m; _ }; pmd_type = { pmty_desc = Pmty_signature s; _ }; _ } ->
            go (prefix ^ m ^ ".") s
        | Psig_recmodule mds ->
            let name (md : Parsetree.module_declaration) =
              Option.value md.pmd_name.txt ~default:"_"
            in
            let dead = List.map (fun md -> dead_module prefix (name md)) mds in
            if List.for_all Fun.id dead then ranges := range src item.psig_loc [] :: !ranges
            else begin
              let starts =
                List.map
                  (fun (md : Parsetree.module_declaration) -> md.pmd_name.loc.loc_start.pos_cnum)
                  mds
              in
              List.iteri
                (fun i (md : Parsetree.module_declaration) ->
                  if List.nth dead i then
                    if i = 0 then ranges := (List.nth starts 0, List.nth starts 1) :: !ranges
                    else
                      ranges :=
                        (and_before (List.nth starts i), md.pmd_loc.loc_end.pos_cnum) :: !ranges
                  else
                    match md.pmd_type.pmty_desc with
                    | Pmty_signature s -> go (prefix ^ name md ^ ".") s
                    | _ -> ())
                mds
            end
        | _ -> ())
      items
  in
  go "" sg;
  List.iter
    (fun n -> if not (List.mem n !found) then Printf.eprintf "drop-vals: %s: %s not found\n" path n)
    names;
  if !ranges <> [] then remove_ranges path src !ranges

(* Remove top-level [external]s and single [let]s named in [names] from an
   .ml without an interface. *)
let drop_lets path names =
  let src, str = parse_with Parse.implementation path in
  let ranges = ref [] in
  List.iter
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_primitive vd when List.mem vd.pval_name.txt names ->
          ranges := range src item.pstr_loc vd.pval_attributes :: !ranges
      | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var v; _ }; pvb_attributes; _ } ])
        when List.mem v.txt names ->
          ranges := range src item.pstr_loc pvb_attributes :: !ranges
      | _ -> ())
    str;
  if !ranges <> [] then remove_ranges path src !ranges

(* ---------- dead-stubs: C stubs no [external] names ---------- *)

(* Every [extern "C" ... caml_*(...) { ... }] definition and one-line
   PREFIX_MACRO(caml_*, ...) instance in [mm] whose name no OCaml external in
   [roots] (or generated _build .ml) mentions, and no other kept stub calls. *)
let dead_stubs ?(apply = true) ~roots mm =
  let src = read_file mm in
  let named = Hashtbl.create 1024 in
  let re = Str.regexp "\"\\(caml_[A-Za-z0-9_]+\\)\"" in
  let scan file =
    let s = read_file file in
    let rec go i =
      match Str.search_forward re s i with
      | j ->
          Hashtbl.replace named (Str.matched_group 1 s) ();
          go (j + 1)
      | exception Not_found -> ()
    in
    go 0
  in
  List.iter
    (fun root ->
      walk root (fun p ->
          if Filename.check_suffix p ".ml" || Filename.check_suffix p ".mli" then scan p))
    roots;
  (* definitions: find "caml_name(" at a definition head, then its body braces *)
  let def =
    Str.regexp "^\\(extern \"C\" \\)?\\(CAMLprim \\)?value[ \n]*\\(caml_[A-Za-z0-9_]+\\)[ \n]*("
  in
  let n = String.length src in
  let defs = ref [] in
  let rec find i =
    match Str.search_forward def src i with
    | start -> (
        let name = Str.matched_group 3 src in
        match String.index_from_opt src (Str.match_end ()) '{' with
        | Some open_ ->
            let rec close k depth =
              if k >= n then n
              else
                match src.[k] with
                | '{' -> close (k + 1) (depth + 1)
                | '}' -> if depth = 1 then k + 1 else close (k + 1) (depth - 1)
                | _ -> close (k + 1) depth
            in
            let stop = close open_ 0 in
            defs := (name, start, stop) :: !defs;
            find stop
        | None -> ())
    | exception Not_found -> ()
  in
  find 0;
  let macro = Str.regexp "^[A-Z][A-Z0-9_]+(\\(caml_[A-Za-z0-9_]+\\)" in
  let rec findm i =
    match Str.search_forward macro src i with
    | start ->
        let name = Str.matched_group 1 src in
        (* balanced parens: macro calls may span lines *)
        let rec close k depth =
          if k >= n then n
          else
            match src.[k] with
            | '(' -> close (k + 1) (depth + 1)
            | ')' -> if depth = 1 then k + 1 else close (k + 1) (depth - 1)
            | _ -> close (k + 1) depth
        in
        let stop = close (String.index_from src start '(') 0 in
        (* NAME_MACRO(name)(params){ body }: take the params and body too *)
        let stop = if stop < n && src.[stop] = '(' then close stop 0 else stop in
        let stop =
          if stop < n && src.[stop] = '{' then
            let rec braces k depth =
              if k >= n then n
              else
                match src.[k] with
                | '{' -> braces (k + 1) (depth + 1)
                | '}' -> if depth = 1 then k + 1 else braces (k + 1) (depth - 1)
                | _ -> braces (k + 1) depth
            in
            braces stop 0
          else stop
        in
        let stop = if stop < n && src.[stop] = '\n' then stop + 1 else stop in
        defs := (name, start, stop) :: !defs;
        findm stop
    | exception Not_found -> ()
  in
  findm 0;
  (* a stub also counts as used when a kept stub body calls it *)
  let live = Hashtbl.copy named in
  let calls body name =
    let needle = name ^ "(" in
    let rec go i =
      i + String.length needle <= String.length body
      && (String.sub body i (String.length needle) = needle || go (i + 1))
    in
    go 0
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun (name, s, e) ->
        if Hashtbl.mem live name then
          List.iter
            (fun (other, _, _) ->
              if (not (Hashtbl.mem live other)) && calls (String.sub src s (e - s)) other then (
                Hashtbl.replace live other ();
                changed := true))
            !defs)
      !defs
  done;
  let dead = List.filter (fun (name, _, _) -> not (Hashtbl.mem live name)) !defs in
  List.iter (fun (name, _, _) -> Printf.printf "%s\n" name) dead;
  if apply then remove_ranges mm src (List.map (fun (_, s, e) -> (s, e)) dead)

(* [s, e) of a record field (declaration or [f = e] in a literal) widened to
   take one separating ';'. *)
let field_range src s e =
  let n = String.length src in
  let rec fwd i =
    if i < n && (src.[i] = ' ' || src.[i] = '\n' || src.[i] = '\t') then fwd (i + 1) else i
  in
  let rec back i =
    if i > 0 && (src.[i - 1] = ' ' || src.[i - 1] = '\n' || src.[i - 1] = '\t') then back (i - 1)
    else i
  in
  let f = fwd e in
  if e > 0 && src.[e - 1] = ';' then (s, e)
  else if f < n && src.[f] = ';' then (s, f + 1)
  else
    let b = back s in
    if b > 0 && src.[b - 1] = ';' then (b - 1, e) else (s, e)

(* ---------- drop-unused (warnings 32 value, 33 open, 34 type, 60 module) ---------- *)

let header =
  Str.regexp "File \"\\([^\"]+\\)\", lines? \\([0-9]+\\)[-0-9]*, characters \\([0-9]+\\)-[0-9]+:"

let warning = Str.regexp ".*[Ww]arning \\([0-9]+\\) "

let contains s sub =
  let n = String.length sub and m = String.length s in
  let rec go k = k + n <= m && (String.sub s k n = sub || go (k + 1)) in
  go 0

(* (file, line, col, warning) for every diagnostic in a dune log; warning 0
   marks a hard error. *)
let diagnostics log =
  let rec go acc = function
    | l :: rest when Str.string_match header l 0 ->
        let file = Str.matched_group 1 l
        and line = int_of_string (Str.matched_group 2 l)
        and col = int_of_string (Str.matched_group 3 l) in
        let rec kind = function
          | w :: r when Str.string_match warning w 0 ->
              let n = int_of_string (Str.matched_group 1 w) in
              (* 69 also reports "never mutated"; only unread fields go *)
              let unread = match r with m :: _ -> contains (w ^ m) "never read" | [] -> false in
              if
                n = 69
                && not (unread || contains w "is never used" || contains w "unused record field")
              then 690
              else n
          | w :: _ when String.length w >= 5 && String.sub w 0 5 = "Error" -> 0
          | w :: r when not (Str.string_match header w 0) -> kind r
          | _ -> 0
        in
        go ((file, line, col, kind rest) :: acc) rest
    | _ :: rest -> go acc rest
    | [] -> List.rev acc
  in
  go [] (String.split_on_char '\n' log)

(* The typed tree of [file] from its .cmt under _build/default. *)
let cmt_for file =
  let found = ref None in
  let dir = Filename.concat "_build/default" (Filename.dirname file) in
  if Sys.file_exists dir then
    walk dir (fun p ->
        if !found = None && Filename.check_suffix p ".cmt" then
          match Cmt_format.read_cmt p with
          | { cmt_sourcefile = Some f; cmt_annots = Implementation s; _ } when source_file f = file
            ->
              found := Some s
          | _ | (exception _) -> ());
  !found

let handled = [ 26; 27; 32; 33; 34; 37; 39; 60; 69; 690 ]

let drop_unused log =
  let by_file = Hashtbl.create 64 in
  List.iter
    (fun (f, l, c, w) ->
      if List.mem w handled then
        Hashtbl.replace by_file f
          ((l, c, w) :: Option.value (Hashtbl.find_opt by_file f) ~default:[]))
    (diagnostics log);
  let total = ref 0 in
  Hashtbl.iter
    (fun file warned ->
      if Sys.file_exists file && Filename.check_suffix file ".ml" then begin
        let src, str = parse_with Parse.implementation file in
        let pos (loc : Location.t) =
          (loc.loc_start.pos_lnum, loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
        in
        let hit kinds loc =
          List.exists (fun (l, c, w) -> (l, c) = pos loc && List.mem w kinds) warned
        in
        let at = hit [ 32; 33; 34; 60; 69 ] in
        let params_hit = hit [ 27 ] in
        let locals_hit (p : Parsetree.pattern) =
          match p.ppat_desc with Ppat_var v -> hit [ 26 ] v.loc | _ -> false
        in
        let rec pat_hit (p : Parsetree.pattern) =
          match p.ppat_desc with
          | Ppat_var v -> at v.loc
          | Ppat_constraint (p, _) -> pat_hit p
          | _ -> false
        in
        let pending_subst = ref [] in
        let ranges = ref [] in
        let add loc attrs = ranges := range src loc attrs :: !ranges in
        let rec go items =
          List.iter
            (fun (item : Parsetree.structure_item) ->
              match item.pstr_desc with
              | Pstr_value (_, [ vb ]) when pat_hit vb.pvb_pat ->
                  add item.pstr_loc vb.pvb_attributes
              | Pstr_value (_, vbs) when List.exists (fun vb -> pat_hit vb.Parsetree.pvb_pat) vbs ->
                  if List.for_all (fun vb -> pat_hit vb.Parsetree.pvb_pat) vbs then
                    add item.pstr_loc []
                  else
                    (* [let rec a = .. and b = ..]: cut a dead binding together with
                 the [and] that joins it to its neighbour *)
                    let starts =
                      List.map
                        (fun (vb : Parsetree.value_binding) ->
                          vb.pvb_pat.ppat_loc.loc_start.pos_cnum)
                        vbs
                    in
                    let and_before p =
                      let rec go i =
                        if i >= 3 && String.sub src (i - 3) 3 = "and" then i - 3 else go (i - 1)
                      in
                      go p
                    in
                    List.iteri
                      (fun i (vb : Parsetree.value_binding) ->
                        if pat_hit vb.pvb_pat then
                          let e = vb.pvb_expr.pexp_loc.loc_end.pos_cnum in
                          if i = 0 then ranges := (List.nth starts 0, List.nth starts 1) :: !ranges
                          else ranges := (and_before (List.nth starts i), e) :: !ranges)
                      vbs
              | Pstr_primitive vd when at vd.pval_name.loc || at item.pstr_loc ->
                  add item.pstr_loc vd.pval_attributes
              | Pstr_open _ when at item.pstr_loc -> add item.pstr_loc []
              | Pstr_type (_, tds)
                when List.exists
                       (fun (td : Parsetree.type_declaration) ->
                         at td.ptype_loc || at td.ptype_name.loc)
                       tds ->
                  let dead (td : Parsetree.type_declaration) =
                    at td.ptype_loc || at td.ptype_name.loc
                  in
                  if List.for_all dead tds then add item.pstr_loc []
                  else begin
                    (* [type a = .. and b = ..]: drop dead members; when the first goes,
                 the first survivor's [and] becomes [type] *)
                    List.iter
                      (fun (td : Parsetree.type_declaration) ->
                        if dead td then add td.ptype_loc td.ptype_attributes)
                      tds;
                    if dead (List.hd tds) then
                      match List.find_opt (fun td -> not (dead td)) tds with
                      | Some td ->
                          let s = td.ptype_loc.loc_start.pos_cnum in
                          if String.length src > s + 3 && String.sub src s 3 = "and" then
                            pending_subst := (s, s + 3, "type") :: !pending_subst
                      | None -> ()
                  end
              | Pstr_module mb when at mb.pmb_name.loc || at item.pstr_loc ->
                  add item.pstr_loc mb.pmb_attributes
              | Pstr_module { pmb_expr = me; _ } -> modexpr me
              | Pstr_recmodule mbs ->
                  List.iter (fun (mb : Parsetree.module_binding) -> modexpr mb.pmb_expr) mbs
              | Pstr_modtype { pmtd_type = Some mt; _ } -> modtype mt
              | _ -> ())
            items
        and modexpr (me : Parsetree.module_expr) =
          match me.pmod_desc with
          | Pmod_structure s -> go s
          | Pmod_constraint (me, mt) ->
              modexpr me;
              modtype mt
          | _ -> ()
        (* [val]s of local signatures, e.g. [module M : sig val x : t end = ...] *)
        and modtype (mt : Parsetree.module_type) =
          match mt.pmty_desc with
          | Pmty_signature items ->
              List.iter
                (fun (item : Parsetree.signature_item) ->
                  match item.psig_desc with
                  | Psig_value vd when at item.psig_loc || at vd.pval_name.loc ->
                      add item.psig_loc vd.pval_attributes
                  | Psig_module { pmd_type; _ } -> modtype pmd_type
                  | _ -> ())
                items
          | _ -> ()
        in
        go str;
        (* Unread record fields: drop the declaration, and use the typed tree
         (.cmt) to find exactly the literals [f = e] and assignments [r.f <- e]
         that target that declaration. 690: a mutable field that is never
         mutated loses its [mutable]. *)
        let field_range = field_range src in
        let subst = ref !pending_subst in
        let dead_decls = ref [] in
        (* a record whose every field is dead is a dead type: rather than empty
         it, silence 69 on it so the loop goes on, and leave it for a person
         (grep [@@warning "-69"]) *)
        let labels ?(td : Parsetree.type_declaration option)
            (lds : Parsetree.label_declaration list) =
          let dead =
            List.filter
              (fun (ld : Parsetree.label_declaration) ->
                hit [ 69 ] ld.pld_loc || hit [ 69 ] ld.pld_name.loc)
              lds
          in
          if dead <> [] && List.length dead = List.length lds then begin
            Printf.eprintf "drop-unused: %s:%d: every field dead; record marked\n" file
              (List.hd lds).pld_loc.loc_start.pos_lnum;
            Option.iter
              (fun (td : Parsetree.type_declaration) ->
                let e = td.ptype_loc.loc_end.pos_cnum in
                subst := (e, e, " [@@warning \"-69\"]") :: !subst)
              td
          end
          else
            List.iter
              (fun (ld : Parsetree.label_declaration) ->
                let s = ld.pld_loc.loc_start.pos_cnum in
                if List.memq ld dead then begin
                  dead_decls := s :: !dead_decls;
                  ranges := field_range s ld.pld_loc.loc_end.pos_cnum :: !ranges
                end
                else if
                  hit [ 690 ] ld.pld_loc
                  && String.length src > s + 8
                  && String.sub src s 8 = "mutable "
                then subst := (s, s + 8, "") :: !subst)
              lds
        in
        let it =
          {
            Ast_iterator.default_iterator with
            type_declaration =
              (fun self (td : Parsetree.type_declaration) ->
                (match td.ptype_kind with
                | Ptype_record lds -> labels ~td lds
                | Ptype_variant cds ->
                    List.iter
                      (fun (cd : Parsetree.constructor_declaration) ->
                        match cd.pcd_args with Pcstr_record lds -> labels lds | _ -> ())
                      cds
                | _ -> ());
                Ast_iterator.default_iterator.type_declaration self td);
          }
        in
        it.structure it str;
        (if !dead_decls <> [] then
           match cmt_for file with
           | None -> Printf.eprintf "drop-unused: %s: no .cmt; field uses left alone\n" file
           | Some tree ->
               let dead (l : Types.label_description) =
                 List.mem l.lbl_loc.loc_start.pos_cnum !dead_decls
                 && l.lbl_loc.loc_start.pos_fname = file
               in
               let it =
                 {
                   Tast_iterator.default_iterator with
                   expr =
                     (fun self (e : Typedtree.expression) ->
                       (match e.exp_desc with
                       | Texp_record { fields; _ } ->
                           Array.iter
                             (function
                               | l, Typedtree.Overridden (lid, v) when dead l ->
                                   ranges :=
                                     field_range lid.loc.loc_start.pos_cnum
                                       (max lid.loc.loc_end.pos_cnum v.exp_loc.loc_end.pos_cnum)
                                     :: !ranges
                               | _ -> ())
                             fields
                       | Texp_setfield (_, _, l, _) when dead l ->
                           subst :=
                             (e.exp_loc.loc_start.pos_cnum, e.exp_loc.loc_end.pos_cnum, "()")
                             :: !subst
                       | _ -> ());
                       Tast_iterator.default_iterator.expr self e);
                 }
               in
               it.structure it tree);
        (* 37: a constructor that is never built disappears with the match arms
         (or or-pattern alternatives) that only it reaches. *)
        let bar_range s e =
          let n = String.length src in
          let rec fwd i =
            if i < n && (src.[i] = ' ' || src.[i] = '\n' || src.[i] = '\t') then fwd (i + 1) else i
          in
          let rec back i =
            if i > 0 && (src.[i - 1] = ' ' || src.[i - 1] = '\n' || src.[i - 1] = '\t') then
              back (i - 1)
            else i
          in
          let b = back s and f = fwd e in
          if src.[s] = '|' then (s, e)
          else if b > 0 && src.[b - 1] = '|' then (b - 1, e)
          else if f < n && src.[f] = '|' then (s, f + 1)
          else (s, e)
        in
        let all_ctors = ref [] and dead_ctors = ref [] in
        let it =
          {
            Ast_iterator.default_iterator with
            type_declaration =
              (fun self (td : Parsetree.type_declaration) ->
                (match td.ptype_kind with
                | Ptype_variant cds ->
                    List.iter
                      (fun (cd : Parsetree.constructor_declaration) ->
                        all_ctors := cd.pcd_name.txt :: !all_ctors)
                      cds;
                    let dead =
                      List.filter
                        (fun (cd : Parsetree.constructor_declaration) ->
                          hit [ 37 ] cd.pcd_loc || hit [ 37 ] cd.pcd_name.loc)
                        cds
                    in
                    (* a variant with every constructor dead is a dead type; left for a person *)
                    if dead <> [] && List.length dead = List.length cds then begin
                      Printf.eprintf "drop-unused: %s:%d: every constructor dead; variant marked\n"
                        file td.ptype_loc.loc_start.pos_lnum;
                      let e = td.ptype_loc.loc_end.pos_cnum in
                      subst := (e, e, " [@@warning \"-37\"]") :: !subst
                    end
                    else dead_ctors := dead @ !dead_ctors
                | _ -> ());
                Ast_iterator.default_iterator.type_declaration self td);
          }
        in
        it.structure it str;
        let dead_names =
          List.filter_map
            (fun (cd : Parsetree.constructor_declaration) ->
              if List.length (List.filter (( = ) cd.pcd_name.txt) !all_ctors) = 1 then begin
                ranges :=
                  bar_range cd.pcd_loc.loc_start.pos_cnum cd.pcd_loc.loc_end.pos_cnum :: !ranges;
                Some cd.pcd_name.txt
              end
              else None)
            !dead_ctors
        in
        (* a pattern that needs a dead constructor anywhere can never match *)
        let rec only_dead (p : Parsetree.pattern) =
          match p.ppat_desc with
          | Ppat_construct (lid, arg) -> (
              List.mem (Longident.last lid.txt) dead_names
              || match arg with Some (_, a) -> only_dead a | None -> false)
          | Ppat_tuple ps -> List.exists only_dead ps
          | Ppat_record (fs, _) -> List.exists (fun (_, p) -> only_dead p) fs
          | Ppat_or (a, b) -> only_dead a && only_dead b
          | Ppat_alias (p, _) | Ppat_constraint (p, _) -> only_dead p
          | _ -> false
        in
        let rec or_alts (p : Parsetree.pattern) =
          match p.ppat_desc with Ppat_or (a, b) -> or_alts a @ or_alts b | _ -> [ p ]
        in
        let cases (cs : Parsetree.case list) =
          List.iter
            (fun (c : Parsetree.case) ->
              if only_dead c.pc_lhs then
                ranges :=
                  bar_range c.pc_lhs.ppat_loc.loc_start.pos_cnum c.pc_rhs.pexp_loc.loc_end.pos_cnum
                  :: !ranges
              else begin
                (* dead alternatives of or-patterns at any depth *)
                let pit =
                  {
                    Ast_iterator.default_iterator with
                    pat =
                      (fun self (p : Parsetree.pattern) ->
                        (match p.ppat_desc with
                        | Ppat_or _ ->
                            List.iter
                              (fun (a : Parsetree.pattern) ->
                                if only_dead a then
                                  ranges :=
                                    bar_range a.ppat_loc.loc_start.pos_cnum
                                      a.ppat_loc.loc_end.pos_cnum
                                    :: !ranges)
                              (or_alts p)
                        | _ -> ());
                        Ast_iterator.default_iterator.pat self p);
                  }
                in
                pit.pat pit c.pc_lhs
              end)
            cs
        in
        let it =
          {
            Ast_iterator.default_iterator with
            expr =
              (fun self (e : Parsetree.expression) ->
                (match e.pexp_desc with
                | Pexp_match (_, cs) | Pexp_try (_, cs) -> cases cs
                | Pexp_function (_, _, Pfunction_cases (cs, _, _)) -> cases cs
                | _ -> ());
                Ast_iterator.default_iterator.expr self e);
          }
        in
        if dead_names <> [] then it.structure it str;
        (* 26: an unused [let x = e in body] local becomes [body] (ReviewED via
         the printed list; e must not be needed for effect). 27: an unused
         parameter is renamed [_x] / [~x:_]. *)
        let it =
          {
            Ast_iterator.default_iterator with
            expr =
              (fun self (e : Parsetree.expression) ->
                (match e.pexp_desc with
                | Pexp_let (_, [ vb ], body) when locals_hit vb.pvb_pat ->
                    Printf.printf "  26 %s:%d: dropped local\n" file e.pexp_loc.loc_start.pos_lnum;
                    ranges :=
                      (e.pexp_loc.loc_start.pos_cnum, body.pexp_loc.loc_start.pos_cnum) :: !ranges
                | Pexp_function (params, _, _) ->
                    List.iter
                      (fun (p : Parsetree.function_param) ->
                        match p.pparam_desc with
                        | Pparam_val
                            ( label,
                              _,
                              {
                                ppat_desc =
                                  Ppat_var v | Ppat_constraint ({ ppat_desc = Ppat_var v; _ }, _);
                                _;
                              } )
                          when params_hit v.loc ->
                            let s = v.loc.loc_start.pos_cnum and e = v.loc.loc_end.pos_cnum in
                            (* a label named like its variable ([~x], [?x], [?(x = d)], [~(x : t)])
                      keeps the label: the whole parameter becomes [~x:_] / [?x:_] *)
                            let whole =
                              (p.pparam_loc.loc_start.pos_cnum, p.pparam_loc.loc_end.pos_cnum)
                            in
                            let (s, e), text =
                              match label with
                              | Labelled l when l = v.txt -> (whole, "~" ^ l ^ ":_")
                              | Optional l when l = v.txt -> (whole, "?" ^ l ^ ":_")
                              | _ -> ((s, e), "_" ^ v.txt)
                            in
                            Printf.printf "  27 %s:%d: %s\n" file v.loc.loc_start.pos_lnum text;
                            subst := (s, e, text) :: !subst
                        | _ -> ())
                      params
                | _ -> ());
                Ast_iterator.default_iterator.expr self e);
            pat =
              (fun self (p : Parsetree.pattern) ->
                (match p.ppat_desc with
                | Ppat_var v
                  when hit [ 26; 27 ] v.loc
                       && (not
                             (List.exists
                                (fun (s, _, _) ->
                                  s = v.loc.loc_start.pos_cnum || s = v.loc.loc_start.pos_cnum - 1)
                                !subst))
                       && not
                            (List.exists
                               (fun (s, _) ->
                                 s <= v.loc.loc_start.pos_cnum && v.loc.loc_end.pos_cnum <= s + 0)
                               !ranges) ->
                    let s = v.loc.loc_start.pos_cnum in
                    if
                      not
                        (s > 0
                        && (src.[s - 1] = '~'
                           || src.[s - 1] = '?'
                           || s > 1
                              && src.[s - 1] = '('
                              && (src.[s - 2] = '~' || src.[s - 2] = '?')))
                    then begin
                      Printf.printf "  27 %s:%d: _%s\n" file v.loc.loc_start.pos_lnum v.txt;
                      subst := (s, v.loc.loc_end.pos_cnum, "_" ^ v.txt) :: !subst
                    end
                | _ -> ());
                Ast_iterator.default_iterator.pat self p);
          }
        in
        if List.exists (fun (_, _, w) -> w = 26 || w = 27) warned then it.structure it str;
        (* 39: a [let rec] that no longer calls itself loses its [rec] *)
        List.iter
          (fun (l, c, w) ->
            if w = 39 then begin
              let rec line_start i k =
                if k = l then i
                else
                  match String.index_from_opt src i '\n' with
                  | Some j -> line_start (j + 1) (k + 1)
                  | None -> i
              in
              let p = line_start 0 1 + c in
              let rec back i =
                if i > 0 && (src.[i - 1] = ' ' || src.[i - 1] = '\n') then back (i - 1) else i
              in
              let q = back p in
              if q >= 3 && String.sub src (q - 3) 3 = "rec" then subst := (q - 3, p, "") :: !subst
            end)
          warned;
        total := !total + List.length !ranges + List.length !subst;
        if !ranges <> [] || !subst <> [] then remove_ranges ~subst:!subst file src !ranges
      end)
    by_file;
  !total

(* ---------- dead-fields: fields of exported records nothing reads ---------- *)

(* The compiler does not warn about an unread field of an exported record.
   Lists "file.mli<TAB>type.field" for every field of a record declared in an
   interface under [dirs] that no implementation outside [excludes] reads
   (field access or record pattern); building it is not reading it. With
   [apply], the declaration goes from the .mli and the .ml, and [field = e]
   from every record literal; the values those named are then ordinary unused
   code for [prune]. A record with every field dead is listed as "type.*" and
   left for a person. *)
let dead_fields ~apply ~excludes ~skip dirs =
  let cmts = ref [] in
  walk "_build/default" (fun p ->
      if Filename.check_suffix p ".cmt" || Filename.check_suffix p ".cmti" then cmts := p :: !cmts);
  let infos =
    List.filter_map
      (fun p ->
        match Cmt_format.read_cmt p with
        | i -> Some (source_file (Option.value i.cmt_sourcefile ~default:""), i)
        | exception _ -> None)
      !cmts
  in
  let excluded = excluded_by excludes in
  let under file =
    List.exists
      (fun d ->
        String.starts_with ~prefix:(if Filename.check_suffix d "/" then d else d ^ "/") file)
      dirs
  in
  (* a label is keyed by its declaring unit, record type and name: the .ml and
     the .mli declare it twice *)
  let key (l : Types.label_description) =
    match Types.get_desc l.lbl_res with
    | Tconstr (path, _, _) ->
        Some
          ( Filename.remove_extension (source_file l.lbl_loc.loc_start.pos_fname),
            Path.last path,
            l.lbl_name )
    | _ -> None
  in
  let read = Hashtbl.create 1024 in
  let literals = ref [] in
  List.iter
    (fun (file, (i : Cmt_format.cmt_infos)) ->
      match i.cmt_annots with
      | Implementation tree ->
          let it =
            {
              Tast_iterator.default_iterator with
              expr =
                (fun self (e : Typedtree.expression) ->
                  (match e.exp_desc with
                  | Texp_field (_, _, l) when not (excluded file) ->
                      Option.iter (fun k -> Hashtbl.replace read k ()) (key l)
                  | Texp_record { fields; _ } ->
                      literals :=
                        ( file,
                          List.filter_map
                            (function
                              | ( l,
                                  Typedtree.Overridden
                                    ((lid : Longident.t Location.loc), (v : Typedtree.expression)) )
                                ->
                                  Option.map
                                    (fun k ->
                                      ( k,
                                        lid.txt,
                                        lid.loc.loc_start.pos_cnum,
                                        max lid.loc.loc_end.pos_cnum v.exp_loc.loc_end.pos_cnum ))
                                    (key l)
                              | _ -> None)
                            (Array.to_list fields) )
                        :: !literals
                  | _ -> ());
                  Tast_iterator.default_iterator.expr self e);
              pat =
                (fun (type k) self (p : k Typedtree.general_pattern) ->
                  (match p.pat_desc with
                  | Tpat_record (fields, _) when not (excluded file) ->
                      List.iter
                        (fun (_, l, _) -> Option.iter (fun k -> Hashtbl.replace read k ()) (key l))
                        fields
                  | _ -> ());
                  Tast_iterator.default_iterator.pat self p);
            }
          in
          it.structure it tree
      | _ -> ())
    infos;
  let dead = ref [] and whole = ref [] in
  List.iter
    (fun (file, (i : Cmt_format.cmt_infos)) ->
      match i.cmt_annots with
      | Interface sg when under file && Sys.file_exists file ->
          let unit_ = Filename.remove_extension file in
          let rec scan items =
            List.iter
              (function
                | Types.Sig_type
                    (id, { type_kind = Type_record (lds, _); type_manifest = None; _ }, _, _) ->
                    let ty = Ident.name id in
                    let unread =
                      List.filter
                        (fun (ld : Types.label_declaration) ->
                          (not (Hashtbl.mem read (unit_, ty, Ident.name ld.ld_id)))
                          && not (List.mem (ty ^ "." ^ Ident.name ld.ld_id) skip))
                        lds
                    in
                    if unread <> [] && List.length unread = List.length lds then
                      whole := (file, ty) :: !whole
                    else
                      List.iter
                        (fun (ld : Types.label_declaration) ->
                          dead := (unit_, ty, Ident.name ld.ld_id) :: !dead)
                        unread
                | Sig_module (_, _, { md_type = Mty_signature items; _ }, _, _) -> scan items
                | _ -> ())
              items
          in
          scan sg.sig_type
      | _ -> ())
    infos;
  let dead = List.sort_uniq compare !dead in
  List.iter (fun (u, ty, f) -> Printf.printf "%s.mli\t%s.%s\n" u ty f) dead;
  List.iter (fun (file, ty) -> Printf.printf "%s\t%s.*\n" file ty) (List.sort_uniq compare !whole);
  if apply then begin
    let ranges = Hashtbl.create 16 in
    let add file r =
      Hashtbl.replace ranges file (r :: Option.value (Hashtbl.find_opt ranges file) ~default:[])
    in
    let sources = Hashtbl.create 16 in
    let src file =
      match Hashtbl.find_opt sources file with
      | Some s -> s
      | None ->
          let s = read_file file in
          Hashtbl.replace sources file s;
          s
    in
    (* a literal names its type through one qualified field, [{ M.a = ..; b = .. }]:
       when that field goes, the first survivor takes the qualifier *)
    let inserts = Hashtbl.create 16 in
    List.iter
      (fun (file, fields) ->
        if Sys.file_exists file then begin
          let gone, kept = List.partition (fun (k, _, _, _) -> List.mem k dead) fields in
          List.iter (fun (_, _, s, e) -> add file (field_range (src file) s e)) gone;
          let qualifier =
            List.find_map (fun (_, lid, _, _) ->
                match lid with
                | Longident.Ldot (m, _) -> Some (String.concat "." (Longident.flatten m) ^ ".")
                | _ -> None)
          in
          match
            ( qualifier gone,
              qualifier kept,
              List.sort (fun (_, _, a, _) (_, _, b, _) -> compare a b) kept )
          with
          | Some q, None, (_, _, s, _) :: _ ->
              Hashtbl.replace inserts file
                ((s, s, q) :: Option.value (Hashtbl.find_opt inserts file) ~default:[])
          | _ -> ()
        end)
      !literals;
    let declarations file (tds : Parsetree.type_declaration list) =
      List.iter
        (fun (td : Parsetree.type_declaration) ->
          match td.ptype_kind with
          | Ptype_record lds ->
              List.iter
                (fun (ld : Parsetree.label_declaration) ->
                  if
                    List.mem
                      (Filename.remove_extension file, td.ptype_name.txt, ld.pld_name.txt)
                      dead
                  then
                    add file
                      (field_range (src file) ld.pld_loc.loc_start.pos_cnum
                         ld.pld_loc.loc_end.pos_cnum))
                lds
          | _ -> ())
        tds
    in
    List.iter
      (fun unit_ ->
        let mli = unit_ ^ ".mli" and ml = unit_ ^ ".ml" in
        if Sys.file_exists mli then begin
          let it =
            {
              Ast_iterator.default_iterator with
              type_declaration = (fun _ td -> declarations mli [ td ]);
            }
          in
          it.signature it (snd (parse_with Parse.interface mli))
        end;
        if Sys.file_exists ml then begin
          let it =
            {
              Ast_iterator.default_iterator with
              type_declaration = (fun _ td -> declarations ml [ td ]);
            }
          in
          it.structure it (snd (parse_with Parse.implementation ml))
        end)
      (List.sort_uniq compare (List.map (fun (u, _, _) -> u) dead));
    Hashtbl.iter
      (fun file rs ->
        remove_ranges
          ~subst:(Option.value (Hashtbl.find_opt inserts file) ~default:[])
          file (src file) rs)
      ranges
  end

(* ---------- dead-optionals: optional arguments no call passes ---------- *)

(* Prints "file.mli<TAB>Path.name<TAB>?label" for every optional parameter of
   a value exported by a unit under [dirs] that no application outside
   [excludes] passes. A value that is used other than by being applied (passed
   along, aliased) is skipped: its calls are not all visible. Report only: the
   parameter and its plumbing are removed by hand, outermost wrapper first (a
   wrapper that forwards [?x] counts as passing it). *)
let is_none (a : Typedtree.expression) =
  match a.exp_desc with Texp_construct (_, { cstr_name = "None"; _ }, []) -> true | _ -> false

let dead_optionals ~excludes dirs =
  let cmts = ref [] in
  walk "_build/default" (fun p -> if Filename.check_suffix p ".cmt" then cmts := p :: !cmts);
  let infos =
    List.filter_map
      (fun p -> match Cmt_format.read_cmt p with i -> Some (p, i) | exception _ -> None)
      !cmts
  in
  let shapes = Hashtbl.create 4096 in
  List.iter
    (fun (_, (i : Cmt_format.cmt_infos)) ->
      Option.iter (Hashtbl.replace shapes i.cmt_modname) i.cmt_impl_shape)
    infos;
  let source (i : Cmt_format.cmt_infos) = source_file (Option.value i.cmt_sourcefile ~default:"") in
  let excluded = excluded_by excludes in
  let passed : (Shape.Uid.t * string, unit) Hashtbl.t = Hashtbl.create 4096 in
  let escapes : (Shape.Uid.t, unit) Hashtbl.t = Hashtbl.create 4096 in
  List.iter
    (fun (_, (i : Cmt_format.cmt_infos)) ->
      let file = source i in
      match i.cmt_annots with
      | Implementation tree when not (excluded file) ->
          let module R = Shape_reduce.Make (struct
            let fuel = 10
            let read_unit_shape ~unit_name = Hashtbl.find_opt shapes unit_name
          end) in
          let rec uids depth (r : Shape_reduce.result) =
            match r with
            | Resolved u | Approximated (Some u) -> [ u ]
            | Resolved_alias (u, r) -> u :: uids depth r
            | Unresolved s when depth = 0 -> uids 1 (R.reduce_for_uid Env.empty s)
            | _ -> []
          in
          let at = Hashtbl.create 1024 in
          List.iter
            (fun ((lid : Longident.t Location.loc), r) ->
              Hashtbl.replace at lid.loc.loc_start.pos_cnum (uids 0 r))
            i.cmt_ident_occurrences;
          let heads = Hashtbl.create 256 in
          let of_ident (lid : Longident.t Location.loc) =
            Option.value (Hashtbl.find_opt at lid.loc.loc_start.pos_cnum) ~default:[]
          in
          let it =
            {
              Tast_iterator.default_iterator with
              expr =
                (fun self (e : Typedtree.expression) ->
                  (match e.exp_desc with
                  | Texp_apply ({ exp_desc = Texp_ident (_, lid, _); _ }, args) ->
                      Hashtbl.replace heads lid.loc.loc_start.pos_cnum ();
                      List.iter
                        (fun u ->
                          List.iter
                            (function
                              | Asttypes.Optional l, Some (a : Typedtree.expression)
                                when not (a.exp_loc.loc_ghost && is_none a) ->
                                  Hashtbl.replace passed (u, l) ()
                              | _ -> ())
                            args)
                        (of_ident lid)
                  | Texp_ident (_, lid, _) when not (Hashtbl.mem heads lid.loc.loc_start.pos_cnum)
                    ->
                      List.iter (fun u -> Hashtbl.replace escapes u ()) (of_ident lid)
                  | _ -> ());
                  Tast_iterator.default_iterator.expr self e);
            }
          in
          it.structure it tree
      | _ -> ())
    infos;
  let under file =
    List.exists
      (fun d ->
        String.starts_with ~prefix:(if Filename.check_suffix d "/" then d else d ^ "/") file)
      dirs
  in
  let rec head s = match (s : Shape.t).desc with Alias s -> head s | _ -> s in
  let find shape name kind =
    match (head shape).desc with
    | Struct map -> Shape.Item.Map.find_opt (Shape.Item.make name kind) map
    | _ -> None
  in
  let rec optionals ty =
    match Types.get_desc ty with
    | Tarrow (Optional l, _, rest, _) -> l :: optionals rest
    | Tarrow (_, _, rest, _) -> optionals rest
    | _ -> []
  in
  let results = ref [] in
  List.iter
    (fun (p, (i : Cmt_format.cmt_infos)) ->
      let ml = source i in
      let cmti = Filename.chop_suffix p ".cmt" ^ ".cmti" in
      if under ml && Filename.check_suffix ml ".ml" && Sys.file_exists cmti then
        match (i.cmt_impl_shape, (Cmt_format.read_cmt cmti).cmt_annots) with
        | Some shape, Interface sg ->
            let mli = Filename.chop_suffix ml ".ml" ^ ".mli" in
            let rec scan prefix shape items =
              List.iter
                (function
                  | Types.Sig_value (id, vd, _) -> (
                      match find shape (Ident.name id) Shape.Sig_component_kind.Value with
                      | Some { uid = Some uid; _ } when not (Hashtbl.mem escapes uid) ->
                          List.iter
                            (fun l ->
                              if not (Hashtbl.mem passed (uid, l)) then
                                results := (mli, prefix ^ Ident.name id, l) :: !results)
                            (optionals vd.val_type)
                      | _ -> ())
                  | Sig_module (id, _, { md_type = Mty_signature items; _ }, _, _) -> (
                      match find shape (Ident.name id) Shape.Sig_component_kind.Module with
                      | Some sub -> scan (prefix ^ Ident.name id ^ ".") sub items
                      | None -> ())
                  | _ -> ())
                items
            in
            scan "" shape sg.sig_type
        | _ -> ())
    infos;
  let results = List.sort_uniq compare !results in
  List.iter (fun (f, n, l) -> Printf.printf "%s\t%s\t?%s\n" f n l) results;
  results

(* drop-optionals: each parameter [dead_optionals] lists leaves the
   signature, and the function binds its default instead ([?(x = d)] becomes
   [let x = (d) in], [?x] becomes [let x = None in]), so behaviour is the
   same and what the constant makes dead is then ordinary unused code. A
   function whose body is a [function] match is printed and left alone. *)
let drop_optionals ~excludes dirs =
  let by_file = Hashtbl.create 16 in
  List.iter
    (fun (mli, name, label) ->
      Hashtbl.replace by_file mli
        ((name, label) :: Option.value (Hashtbl.find_opt by_file mli) ~default:[]))
    (List.filter (fun (mli, _, _) -> Sys.file_exists mli) (dead_optionals ~excludes dirs));
  Hashtbl.iter
    (fun mli wanted ->
      let ml = Filename.chop_suffix mli ".mli" ^ ".ml" in
      let labels path = List.filter_map (fun (n, l) -> if n = path then Some l else None) wanted in
      (* interface: cut [?l:ty ->] out of the arrow chain *)
      let src, sg = parse_with Parse.interface mli in
      let ranges = ref [] in
      let rec arrows dead (t : Parsetree.core_type) =
        match t.ptyp_desc with
        | Ptyp_arrow (label, _, rest) ->
            (match label with
            | Optional l when List.mem l dead ->
                ranges :=
                  (t.ptyp_loc.loc_start.pos_cnum, rest.ptyp_loc.loc_start.pos_cnum) :: !ranges
            | _ -> ());
            arrows dead rest
        | Ptyp_poly (_, t) -> arrows dead t
        | _ -> ()
      in
      let rec signature prefix items =
        List.iter
          (fun (item : Parsetree.signature_item) ->
            match item.psig_desc with
            | Psig_value vd -> (
                match labels (prefix ^ vd.pval_name.txt) with
                | [] -> ()
                | dead -> arrows dead vd.pval_type)
            | Psig_module md -> declaration prefix md
            | Psig_recmodule mds -> List.iter (declaration prefix) mds
            | _ -> ())
          items
      and declaration prefix (md : Parsetree.module_declaration) =
        match (md.pmd_name.txt, md.pmd_type.pmty_desc) with
        | Some m, Pmty_signature s -> signature (prefix ^ m ^ ".") s
        | _ -> ()
      in
      signature "" sg;
      if !ranges <> [] then remove_ranges mli src !ranges;
      (* implementation: the parameter goes, its default is bound in the body *)
      let src, str = parse_with Parse.implementation ml in
      let ranges = ref [] and subst = ref [] in
      let text (loc : Location.t) =
        String.sub src loc.loc_start.pos_cnum (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
      in
      let binding path (vb : Parsetree.value_binding) dead =
        match vb.pvb_expr.pexp_desc with
        | Pexp_function (params, _, Pfunction_body body) ->
            List.iter
              (fun (p : Parsetree.function_param) ->
                match p.pparam_desc with
                | Pparam_val (Optional l, default, pat) when List.mem l dead ->
                    let n = String.length src in
                    let rec fwd i = if i < n && src.[i] = ' ' then fwd (i + 1) else i in
                    ranges :=
                      (p.pparam_loc.loc_start.pos_cnum, fwd p.pparam_loc.loc_end.pos_cnum)
                      :: !ranges;
                    let value =
                      match default with Some d -> "(" ^ text d.pexp_loc ^ ")" | None -> "None"
                    in
                    let at = body.pexp_loc.loc_start.pos_cnum in
                    subst :=
                      (at, at, Printf.sprintf "let %s = %s in " (text pat.ppat_loc) value) :: !subst
                | _ -> ())
              params
        | _ -> Printf.printf "  left %s %s: not a plain function\n" ml path
      in
      let rec structure prefix items =
        List.iter
          (fun (item : Parsetree.structure_item) ->
            match item.pstr_desc with
            | Pstr_value (_, vbs) ->
                List.iter
                  (fun (vb : Parsetree.value_binding) ->
                    match vb.pvb_pat.ppat_desc with
                    | Ppat_var v | Ppat_constraint ({ ppat_desc = Ppat_var v; _ }, _) -> (
                        match labels (prefix ^ v.txt) with
                        | [] -> ()
                        | dead -> binding (prefix ^ v.txt) vb dead)
                    | _ -> ())
                  vbs
            | Pstr_module mb -> module_binding prefix mb
            | Pstr_recmodule mbs -> List.iter (module_binding prefix) mbs
            | _ -> ())
          items
      and module_binding prefix (mb : Parsetree.module_binding) =
        let rec body (me : Parsetree.module_expr) =
          match me.pmod_desc with
          | Pmod_structure s ->
              Option.iter (fun m -> structure (prefix ^ m ^ ".") s) mb.pmb_name.txt
          | Pmod_constraint (me, _) -> body me
          | _ -> ()
        in
        body mb.pmb_expr
      in
      structure "" str;
      if !ranges <> [] || !subst <> [] then remove_ranges ~subst:!subst ml src !ranges)
    by_file

(* ---------- dead-types: exported types nothing names ---------- *)

(* Prints "file.mli<TAB>Path.type" for every type declared in an interface
   under [dirs] that no implementation and no interface names (its own
   declarations apart). A type exported from an .mli never gets the compiler's
   unused-type warning; delete the listed ones from the .mli by hand and
   [prune] takes the implementation. Types that only each other name show up
   round by round. Report only. *)
let dead_types dirs =
  let files = ref [] in
  walk "_build/default" (fun p ->
      if Filename.check_suffix p ".cmt" || Filename.check_suffix p ".cmti" then files := p :: !files);
  let infos =
    List.filter_map
      (fun p -> match Cmt_format.read_cmt p with i -> Some (p, i) | exception _ -> None)
      !files
  in
  let shapes = Hashtbl.create 4096 in
  List.iter
    (fun (p, (i : Cmt_format.cmt_infos)) ->
      if Filename.check_suffix p ".cmt" then
        Option.iter (Hashtbl.replace shapes i.cmt_modname) i.cmt_impl_shape)
    infos;
  let named : (Shape.Uid.t, unit) Hashtbl.t = Hashtbl.create 65536 in
  List.iter
    (fun (_, (i : Cmt_format.cmt_infos)) ->
      let module R = Shape_reduce.Make (struct
        let fuel = 10
        let read_unit_shape ~unit_name = Hashtbl.find_opt shapes unit_name
      end) in
      let rec uids depth (r : Shape_reduce.result) =
        match r with
        | Resolved u | Approximated (Some u) -> [ u ]
        | Resolved_alias (u, r) -> u :: uids depth r
        | Unresolved s when depth = 0 -> uids 1 (R.reduce_for_uid Env.empty s)
        | _ -> []
      in
      List.iter
        (fun (_, r) -> List.iter (fun u -> Hashtbl.replace named u ()) (uids 0 r))
        i.cmt_ident_occurrences)
    infos;
  let under file =
    List.exists
      (fun d ->
        String.starts_with ~prefix:(if Filename.check_suffix d "/" then d else d ^ "/") file)
      dirs
  in
  (* a type is named through itself, a constructor or a field, as declared
     in the interface or in the implementation *)
  let uses (decl : Types.type_declaration) =
    Hashtbl.mem named decl.type_uid
    ||
    match decl.type_kind with
    | Type_variant (cds, _) ->
        List.exists
          (fun (cd : Types.constructor_declaration) ->
            Hashtbl.mem named cd.cd_uid
            ||
            match cd.cd_args with
            | Cstr_record lds ->
                List.exists (fun (ld : Types.label_declaration) -> Hashtbl.mem named ld.ld_uid) lds
            | _ -> false)
          cds
    | Type_record (lds, _) ->
        List.exists (fun (ld : Types.label_declaration) -> Hashtbl.mem named ld.ld_uid) lds
    | _ -> false
  in
  let results = ref [] in
  List.iter
    (fun (p, (i : Cmt_format.cmt_infos)) ->
      let ml = source_file (Option.value i.cmt_sourcefile ~default:"") in
      let cmti = Filename.remove_extension p ^ ".cmti" in
      if Filename.check_suffix p ".cmt" && under ml && Sys.file_exists cmti then
        match (i.cmt_annots, (Cmt_format.read_cmt cmti).cmt_annots) with
        | Implementation tree, Interface sg ->
            let mli = Filename.chop_suffix ml ".ml" ^ ".mli" in
            let rec scan prefix impl items =
              List.iter
                (function
                  | Types.Sig_type (id, decl, _, _) ->
                      let in_impl =
                        List.exists
                          (function
                            | Types.Sig_type (other, d, _, _) ->
                                Ident.name other = Ident.name id && uses d
                            | _ -> false)
                          impl
                      in
                      if (not in_impl) && not (uses decl) then
                        results := (mli, prefix ^ Ident.name id) :: !results
                  | Sig_module (id, _, { md_type = Mty_signature items; _ }, _, _) -> (
                      match
                        List.find_map
                          (function
                            | Types.Sig_module (other, _, { md_type = Mty_signature sub; _ }, _, _)
                              when Ident.name other = Ident.name id ->
                                Some sub
                            | _ -> None)
                          impl
                      with
                      | Some sub -> scan (prefix ^ Ident.name id ^ ".") sub items
                      | None -> ())
                  | _ -> ())
                items
            in
            scan "" tree.str_type sg.sig_type
        | _ -> ())
    infos;
  let results = List.sort_uniq compare !results in
  List.iter (fun (f, n) -> Printf.printf "%s\t%s\n" f n) results;
  results

(* drop-types: the listed types leave their interfaces (one declared with
   [and] stays and is reported); the build then says which were still needed,
   and [prune] removes the implementation of the rest. *)
let drop_types dirs =
  let by_file = Hashtbl.create 8 in
  List.iter
    (fun (f, n) ->
      Hashtbl.replace by_file f
        (("type:" ^ n) :: Option.value (Hashtbl.find_opt by_file f) ~default:[]))
    (dead_types dirs);
  Hashtbl.iter (fun f names -> if Sys.file_exists f then drop_vals f names) by_file

(* ---------- cut-tests: a test that stops compiling goes ---------- *)

let is_test file =
  let base = Filename.basename file in
  (String.length base > 5 && String.sub base 0 5 = "test_")
  || (String.length file > 5 && String.sub file 0 5 = "test/")

(* Remove the top-level item holding each hard error in a test file; inside
   an entry point ([run], [main], [tests], [let () =]) cut only the innermost
   sequence statement or local binding, or empty the body to [()]. Returns
   edits made. *)
let cut_tests log =
  let errors =
    List.filter
      (fun (f, _, _, w) -> w = 0 && is_test f && Sys.file_exists f && Filename.check_suffix f ".ml")
      (diagnostics log)
  in
  let by_file = Hashtbl.create 16 in
  List.iter
    (fun (f, l, c, _) ->
      Hashtbl.replace by_file f ((l, c) :: Option.value (Hashtbl.find_opt by_file f) ~default:[]))
    errors;
  let total = ref 0 in
  Hashtbl.iter
    (fun file points ->
      let src, str = parse_with Parse.implementation file in
      let offset (l, c) =
        (* byte offset of line l, column c *)
        let rec go i line =
          if line = l then i + c
          else
            match String.index_from_opt src i '\n' with
            | Some j -> go (j + 1) (line + 1)
            | None -> i
        in
        go 0 1
      in
      let ranges = ref [] and subst = ref [] in
      List.iter
        (fun point ->
          let p = offset point in
          let inside (loc : Location.t) = loc.loc_start.pos_cnum <= p && p < loc.loc_end.pos_cnum in
          match List.find_opt (fun (i : Parsetree.structure_item) -> inside i.pstr_loc) str with
          | None -> ()
          | Some item ->
              let entry =
                match item.pstr_desc with
                | Pstr_value (_, vbs) ->
                    List.exists
                      (fun (vb : Parsetree.value_binding) ->
                        match vb.pvb_pat.ppat_desc with
                        | Ppat_var { txt = "run" | "main" | "tests"; _ } -> true
                        | Ppat_construct ({ txt = Lident "()"; _ }, None) -> true
                        | _ -> false)
                      vbs
                | Pstr_eval _ -> true
                | _ -> false
              in
              if not entry then ranges := range src item.pstr_loc [] :: !ranges
              else begin
                let best = ref None in
                let it =
                  {
                    Ast_iterator.default_iterator with
                    expr =
                      (fun self (e : Parsetree.expression) ->
                        (match e.pexp_desc with
                        | Pexp_sequence (a, b) when inside a.pexp_loc ->
                            best :=
                              Some (a.pexp_loc.loc_start.pos_cnum, b.pexp_loc.loc_start.pos_cnum)
                        | Pexp_sequence (a, b)
                          when inside b.pexp_loc
                               &&
                               match b.pexp_desc with
                               | Pexp_sequence _ | Pexp_let _ -> false
                               | _ -> true ->
                            best := Some (a.pexp_loc.loc_end.pos_cnum, b.pexp_loc.loc_end.pos_cnum)
                        (* an error in what a local is bound to: the binding goes and
                    the next rounds cut the statements that named it *)
                        | Pexp_let (_, vbs, body)
                          when List.exists
                                 (fun (vb : Parsetree.value_binding) ->
                                   inside vb.pvb_expr.pexp_loc || inside vb.pvb_pat.ppat_loc)
                                 vbs ->
                            best :=
                              Some
                                ( (List.hd vbs).pvb_loc.loc_start.pos_cnum,
                                  body.pexp_loc.loc_start.pos_cnum )
                        | _ -> ());
                        Ast_iterator.default_iterator.expr self e);
                  }
                in
                it.structure_item it item;
                match !best with
                | Some r -> ranges := r :: !ranges
                | None -> (
                    match item.pstr_desc with
                    | Pstr_value (_, vbs) ->
                        List.iter
                          (fun (vb : Parsetree.value_binding) ->
                            let rec body (e : Parsetree.expression) =
                              match e.pexp_desc with
                              | Pexp_function (_, _, Pfunction_body b) -> body b
                              | _ -> e
                            in
                            let b = body vb.pvb_expr in
                            if inside b.pexp_loc then
                              subst :=
                                (b.pexp_loc.loc_start.pos_cnum, b.pexp_loc.loc_end.pos_cnum, "()")
                                :: !subst)
                          vbs
                    | _ -> ranges := range src item.pstr_loc [] :: !ranges)
              end)
        points;
      Printf.printf "  cut %s: %d\n" file (List.length !ranges + List.length !subst);
      total := !total + List.length !ranges + List.length !subst;
      remove_ranges ~subst:!subst file src !ranges)
    by_file;
  !total

(* The Metal generator refuses registry entries metal.ml no longer calls:
   "registry <kind> NAME is not used by metal.ml". Drop those entries. *)
let registry = "lib/metal/gen/registry.ml"

let drop_registry_entries log =
  let re = Str.regexp "registry [a-z]+ \\([A-Za-z_0-9]+\\) is not used" in
  let names =
    List.filter_map
      (fun l -> if Str.string_match re l 0 then Some (Str.matched_group 1 l) else None)
      (String.split_on_char '\n' log)
  in
  if names = [] || not (Sys.file_exists registry) then 0
  else begin
    let src, str = parse_with Parse.implementation registry in
    let ranges = ref [] in
    let it =
      {
        Ast_iterator.default_iterator with
        expr =
          (fun self (e : Parsetree.expression) ->
            (match e.pexp_desc with
            | Pexp_construct
                ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ elt; _ ]; _ }) -> (
                match elt.pexp_desc with
                | Pexp_construct (_, Some { pexp_desc = Pexp_record (fields, _); _ })
                  when List.exists
                         (fun ((lid : Longident.t Location.loc), (v : Parsetree.expression)) ->
                           Longident.last lid.txt = "ocaml"
                           &&
                           match v.pexp_desc with
                           | Pexp_constant { pconst_desc = Pconst_string (s, _, _); _ } ->
                               List.mem s names
                           | _ -> false)
                         fields ->
                    let s = elt.pexp_loc.loc_start.pos_cnum and e = elt.pexp_loc.loc_end.pos_cnum in
                    (* take the separating ';' before, or the one after for the head *)
                    let rec back i =
                      if i > 0 && (src.[i - 1] = ' ' || src.[i - 1] = '\n') then back (i - 1) else i
                    in
                    let b = back s in
                    if b > 0 && src.[b - 1] = ';' then ranges := (b - 1, e) :: !ranges
                    else begin
                      let rec fwd i =
                        if i < String.length src && (src.[i] = ' ' || src.[i] = '\n') then
                          fwd (i + 1)
                        else i
                      in
                      let f = fwd e in
                      ranges :=
                        (s, if f < String.length src && src.[f] = ';' then f + 1 else e) :: !ranges
                    end
                | _ -> ())
            | _ -> ());
            Ast_iterator.default_iterator.expr self e);
      }
    in
    it.structure it str;
    Printf.printf "  registry: dropped %s\n" (String.concat ", " names);
    remove_ranges registry src !ranges;
    List.length !ranges
  end

(* ---------- drop-c-unused: clang's -Wunused-function as the oracle ---------- *)

(* "path:line:col: error: unused function 'name'" -> remove that definition
   (return type on the previous line included) through its closing brace. *)
let drop_c_unused ~dir log =
  let re =
    Str.regexp
      "^\\(\\./\\)?\\([^:]+\\):\\([0-9]+\\):[0-9]+: error: unused \\(function\\|variable\\) \
       '\\([A-Za-z0-9_]+\\)'"
  in
  let hits =
    List.filter_map
      (fun l ->
        if Str.string_match re l 0 then
          Some (Filename.concat dir (Str.matched_group 2 l), int_of_string (Str.matched_group 3 l))
        else None)
      (String.split_on_char '\n' log)
  in
  let by_file = Hashtbl.create 4 in
  List.iter
    (fun (f, l) ->
      Hashtbl.replace by_file f (l :: Option.value (Hashtbl.find_opt by_file f) ~default:[]))
    (List.sort_uniq compare hits);
  let total = ref 0 in
  Hashtbl.iter
    (fun file lines ->
      if Sys.file_exists file then begin
        let src = read_file file in
        let n = String.length src in
        let line_start l =
          let rec go i k =
            if k = l then i
            else
              match String.index_from_opt src i '\n' with Some j -> go (j + 1) (k + 1) | None -> n
          in
          go 0 1
        in
        let ranges =
          List.filter_map
            (fun l ->
              let s = line_start l in
              (* a return type alone on the line above belongs to the definition *)
              let s =
                if s > 1 then
                  let p = line_start (l - 1) in
                  let above = String.trim (String.sub src p (s - p)) in
                  if
                    above <> ""
                    && (not (List.mem above.[String.length above - 1] [ ';'; '}'; '{'; '/' ]))
                    && not (String.length above > 1 && String.sub above 0 2 = "//")
                  then p
                  else s
                else s
              in
              (* a variable ends at its ';', a function at its body's closing brace *)
              let semi = String.index_from_opt src s ';'
              and brace = String.index_from_opt src s '{' in
              match (brace, semi) with
              | Some b, Some c when c < b -> Some (s, c + 1)
              | Some b, _ ->
                  let rec close k depth =
                    if k >= n then n
                    else
                      match src.[k] with
                      | '{' -> close (k + 1) (depth + 1)
                      | '}' -> if depth = 1 then k + 1 else close (k + 1) (depth - 1)
                      | _ -> close (k + 1) depth
                  in
                  let e = close b 0 in
                  Some (s, if e < n && src.[e] = '\n' then e + 1 else e)
              | None, Some c -> Some (s, c + 1)
              | None, None -> None)
            lines
        in
        total := !total + List.length ranges;
        remove_ranges file src ranges
      end)
    by_file;
  !total

(* ---------- prune: iterate to a fixpoint with the compiler as oracle ---------- *)

let build target =
  let log = Filename.temp_file "codemod" ".log" in
  let code =
    Sys.command (Printf.sprintf "dune build --root . --profile codemod %s > %s 2>&1" target log)
  in
  let text = read_file log in
  Sys.remove log;
  (code, text)

let prune ?(cut = false) ~modules ~excludes ~target dirs =
  let last = ref [] in
  let rec loop round =
    let code, log = build target in
    let dropped = drop_unused log in
    let dropped = if dropped = 0 && code <> 0 then drop_registry_entries log else dropped in
    let dropped = if dropped = 0 && code <> 0 && cut then cut_tests log else dropped in
    if dropped > 0 then (
      Printf.printf "round %d: %d unused items removed\n%!" round dropped;
      loop (round + 1))
    else if code <> 0 then begin
      let hard =
        List.filter (fun (_, _, _, w) -> w = 0 || not (List.mem w handled)) (diagnostics log)
      in
      Printf.printf "round %d: build fails with %d unhandled diagnostics; stopping\n" round
        (List.length hard);
      List.iter (fun (f, l, c, w) -> Printf.printf "  %s:%d:%d warning=%d\n" f l c w) hard;
      if hard = [] then print_string log;
      exit 1
    end
    else begin
      let dead = dead_exports ~modules ~excludes dirs in
      (* a [Private] test hook that only tests reach stays while the code it
         observes does: it goes when nothing at all uses it *)
      let dead =
        if excludes = [] || not (List.exists (fun (_, n) -> contains n "Private.") dead) then dead
        else
          let unused = dead_exports ~modules ~excludes:[] dirs in
          List.filter (fun ((_, n) as d) -> (not (contains n "Private.")) || List.mem d unused) dead
      in
      if dead = [] then Printf.printf "round %d: fixpoint\n" round
      else if dead = !last then (
        Printf.printf "round %d: no progress on %d exports; stopping\n" round (List.length dead);
        exit 1)
      else begin
        let by_file = Hashtbl.create 16 in
        List.iter
          (fun (f, n) ->
            Hashtbl.replace by_file f (n :: Option.value (Hashtbl.find_opt by_file f) ~default:[]))
          dead;
        last := dead;
        List.iter (fun (f, n) -> Printf.printf "  drop %s %s\n" f n) dead;
        Hashtbl.iter
          (fun f names ->
            if Sys.file_exists f then
              if Filename.check_suffix f ".ml" then drop_lets f names else drop_vals f names)
          by_file;
        Printf.printf "round %d: %d dead exports dropped\n%!" round (List.length dead);
        loop (round + 1)
      end
    end
  in
  loop 0

(* ---------- rename: a project-wide textual rename ---------- *)

(* old, Old and OLD become fresh, Fresh and FRESH *)
let rename_text old fresh =
  let cases s =
    let s = String.lowercase_ascii s in
    [ s; String.capitalize_ascii s; String.uppercase_ascii s ]
  in
  let pairs = List.map2 (fun o n -> (Str.regexp_string o, n)) (cases old) (cases fresh) in
  fun s -> List.fold_left (fun s (o, n) -> Str.global_substitute o (fun _ -> n) s) s pairs

let replace_text old fresh = Str.global_substitute (Str.regexp_string old) (fun _ -> fresh)

let rename_self_test () =
  assert (
    rename_text "foo" "bar" "Foo_x FOO_Y lib/foo/foo.ml food" = "Bar_x BAR_Y lib/bar/bar.ml bard");
  assert (
    replace_text "Foo.f ~x:Rdk.X" "Foo.f ~x:(Foo.Owner Rdk.X)" "Foo.f ~x:Rdk.X; sop.f ~x:rdk.x"
    = "Foo.f ~x:(Foo.Owner Rdk.X); sop.f ~x:rdk.x");
  print_endline "rename: ok"

(* Rewrites the contents and the path of every text file git tracks or would
   track; binary files only move. Nothing is staged, so [git status] reviews
   it and [git add -A] accepts it. *)
let rename ?(exact = false) ~dry old fresh =
  let sub = if exact then replace_text old fresh else rename_text old fresh in
  let listing = Filename.temp_file "codemod" ".files" in
  if
    Sys.command ("git ls-files -z --cached --others --exclude-standard > " ^ Filename.quote listing)
    <> 0
  then exit 1;
  let files =
    String.split_on_char '\000' (read_file listing)
    |> List.filter (fun f -> f <> "" && Sys.file_exists f && not (Sys.is_directory f))
  in
  Sys.remove listing;
  let moves =
    List.filter_map
      (fun f ->
        let g = sub f in
        if g = f then None else Some (f, g))
      files
  in
  List.iter
    (fun (f, g) ->
      if Sys.file_exists g then (
        Printf.eprintf "%s: %s already exists\n" f g;
        exit 1))
    moves;
  let edited = ref 0 in
  List.iter
    (fun f ->
      let s = read_file f in
      if not (String.contains s '\000') then begin
        let t = sub s in
        if t <> s then (
          incr edited;
          if dry then Printf.printf "edit %s\n" f else write_file f t)
      end)
    files;
  let rec mkdir_p d =
    if not (Sys.file_exists d) then (
      mkdir_p (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  let rec rmdir_up d =
    if
      d <> "."
      &&
        try
          Sys.rmdir d;
          true
        with Sys_error _ -> false
    then rmdir_up (Filename.dirname d)
  in
  List.iter
    (fun (f, g) ->
      if dry then Printf.printf "move %s -> %s\n" f g
      else (
        mkdir_p (Filename.dirname g);
        Sys.rename f g;
        rmdir_up (Filename.dirname f)))
    moves;
  Printf.printf "%d files edited, %d moved\n" !edited (List.length moves)

let source_path path =
  let rec trim path =
    if String.starts_with ~prefix:"../" path then trim (String.sub path 3 (String.length path - 3))
    else path
  in
  trim path

let binding_lengths file source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf file;
  Lexer.init ();
  let tree =
    try Parse.implementation lexbuf
    with Syntaxerr.Error _ as error ->
      Location.report_exception Format.err_formatter error;
      failwith ("cannot measure bindings in " ^ file)
  in
  let counts = Hashtbl.create 32 and found = ref [] in
  let iterator =
    {
      Ast_iterator.default_iterator with
      structure_item =
        (fun self item ->
          (match item.Parsetree.pstr_desc with
          | Pstr_value (_, bindings) ->
              List.iter
                (fun binding ->
                  match binding.Parsetree.pvb_pat.ppat_desc with
                  | Ppat_var name ->
                      let count = 1 + Option.value (Hashtbl.find_opt counts name.txt) ~default:0 in
                      Hashtbl.replace counts name.txt count;
                      let location = binding.pvb_loc in
                      let length = location.loc_end.pos_lnum - location.loc_start.pos_lnum + 1 in
                      found :=
                        (source_path file, name.txt ^ "#" ^ string_of_int count, length) :: !found
                  | _ -> ())
                bindings
          | _ -> ());
          Ast_iterator.default_iterator.structure_item self item);
    }
  in
  iterator.structure iterator tree;
  List.rev !found

let source_lengths paths =
  let found = ref [] in
  let check path =
    if Filename.check_suffix path ".ml" then found := binding_lengths path (read_file path) @ !found
  in
  List.iter (fun path -> if Sys.is_directory path then walk path check else check path) paths;
  List.sort compare !found

let print_lengths rows =
  List.iter (fun (file, name, length) -> Printf.printf "%s\t%s\t%d\n" file name length) rows

let length_budget path =
  String.split_on_char '\n' (read_file path)
  |> List.filter_map (fun line ->
      if line = "" then None
      else
        match String.split_on_char '\t' line with
        | [ file; name; length ] -> Some (file, name, int_of_string length)
        | _ -> failwith ("invalid function length budget: " ^ line))

let binding_budget limits (file, name, _) =
  List.find_map (fun (f, n, size) -> if f = file && n = name then Some size else None) limits
  |> Option.value ~default:300

let check_lengths ?(prune = false) budget paths =
  let limits = length_budget budget in
  let rows = source_lengths paths |> List.filter (fun (_, _, length) -> length > 300) in
  let failed = ref false in
  List.iter
    (fun ((file, name, length) as row) ->
      let limit = binding_budget limits row in
      if length > limit then begin
        failed := true;
        Printf.eprintf "%s: %s is %d lines (budget %d)\n" file name length limit
      end)
    rows;
  if !failed then exit 1;
  if prune then print_lengths rows
  else
    List.iter
      (fun (file, name, limit) ->
        if not (List.mem (file, name, limit) rows) then begin
          failed := true;
          Printf.eprintf
            "%s: reduce/remove stale length budget for %s with function-lengths --prune\n" file name
        end)
      limits;
  if !failed then exit 1

let lengths_self_test () =
  let source =
    "let f x =\n  let nested y = y in\n  nested x\nmodule M = struct\nlet f x = x\nend\n"
  in
  assert (
    binding_lengths "../../lib/fixture.ml" source
    = [ ("lib/fixture.ml", "f#1", 3); ("lib/fixture.ml", "f#2", 1) ]);
  assert (binding_lengths "empty.ml" "" = []);
  let limits = [ ("lib/fixture.ml", "f#1", 350) ] in
  let allowed row =
    let _, _, length = row in
    length <= binding_budget limits row
  in
  assert (allowed ("lib/fixture.ml", "f#1", 350));
  assert (allowed ("lib/fixture.ml", "f#1", 320));
  assert (not (allowed ("lib/fixture.ml", "f#1", 351)));
  assert (not (allowed ("lib/fixture.ml", "new#1", 301)))

let history_marker =
  Str.regexp "\\bP[0-9]+\\.[0-9]+\\b\\|\\bG[0-9]+\\b\\|20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]"

let history_comments file source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf file;
  Lexer.init ();
  let found = ref [] in
  let check body location =
    match Str.search_forward history_marker body 0 with
    | _ -> found := (location, Str.matched_string body) :: !found
    | exception Not_found -> ()
  in
  let rec scan () =
    match Lexer.token_with_comments lexbuf with
    | Parser.EOF -> List.rev !found
    | COMMENT (body, location) ->
        check body location;
        scan ()
    | DOCSTRING doc ->
        check (Docstrings.docstring_body doc) (Docstrings.docstring_loc doc);
        scan ()
    | _ -> scan ()
  in
  try scan ()
  with Lexer.Error _ as error ->
    Location.report_exception Format.err_formatter error;
    failwith ("cannot scan comments in " ^ file)

let history_comments_self_test () =
  let source =
    {history|let text = "G5 2026-10-27 P3.2"
let shader = {|G5 2026-10-27|}
(* plain (* nested *) comment *)
(** P3.2 history *)
(* nested (* G5 history *) comment *)
(* dated 2026-10-27 *)
|history}
  in
  assert (List.map snd (history_comments "fixture.ml" source) = [ "P3.2"; "G5"; "2026-10-27" ])

let check_history_comments paths =
  let found = ref [] in
  let check path =
    if Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli" then
      List.iter
        (fun (location, marker) -> found := (path, location, marker) :: !found)
        (history_comments path (read_file path))
  in
  List.iter (fun path -> if Sys.is_directory path then walk path check else check path) paths;
  List.iter
    (fun (path, location, marker) ->
      Printf.eprintf "%s:%d: comment contains history marker %s\n" path
        location.Location.loc_start.pos_lnum marker)
    (List.rev !found);
  if !found <> [] then exit 1

let link_target text =
  let text = String.trim text in
  let text =
    if String.starts_with ~prefix:"<" text then
      match String.index_opt text '>' with
      | Some end_ -> String.sub text 1 (end_ - 1)
      | None -> text
    else match String.index_opt text ' ' with Some end_ -> String.sub text 0 end_ | None -> text
  in
  let scheme = Str.regexp "^[A-Za-z][A-Za-z0-9+.-]*:" in
  if
    text = ""
    || text.[0] = '#'
    || String.starts_with ~prefix:"//" text
    || Str.string_match scheme text 0
  then None
  else
    let end_ = String.index_opt text '#' |> Option.value ~default:(String.length text) in
    let path = String.sub text 0 end_ in
    let decoded = Buffer.create (String.length path) in
    let rec decode i =
      if i < String.length path then
        if path.[i] = '%' && i + 2 < String.length path then (
          match int_of_string_opt ("0x" ^ String.sub path (i + 1) 2) with
          | Some byte ->
              Buffer.add_char decoded (Char.chr byte);
              decode (i + 3)
          | None ->
              Buffer.add_char decoded path.[i];
              decode (i + 1))
        else (
          Buffer.add_char decoded path.[i];
          decode (i + 1))
    in
    decode 0;
    Some (Buffer.contents decoded)

let markdown_links source =
  let inline = Str.regexp "]([ \t]*\\(<[^>]*>\\|[^) \t]+\\)" in
  let reference = Str.regexp "^[ \t]*\\[[^]]+\\]:[ \t]*\\(<[^>]*>\\|[^ \t]+\\)" in
  let fenced = ref None and found = ref [] in
  String.split_on_char '\n' source
  |> List.iteri (fun index line ->
      let trimmed = String.trim line in
      let fence =
        List.find_opt (fun prefix -> String.starts_with ~prefix trimmed) [ "```"; "~~~" ]
      in
      match (!fenced, fence) with
      | None, Some prefix -> fenced := Some prefix
      | Some prefix, Some close when prefix = close -> fenced := None
      | Some _, _ -> ()
      | None, _ ->
          let add target =
            Option.iter (fun target -> found := (index + 1, target) :: !found) (link_target target)
          in
          if Str.string_match reference line 0 then add (Str.matched_group 1 line);
          let rec scan offset =
            match Str.search_forward inline line offset with
            | _ ->
                let target = Str.matched_group 1 line and next = Str.match_end () in
                add target;
                scan next
            | exception Not_found -> ()
          in
          scan 0);
  List.rev !found

let missing_links file source =
  markdown_links source
  |> List.filter (fun (_, target) ->
      let target =
        if Filename.is_relative target then Filename.concat (Filename.dirname file) target
        else "." ^ target
      in
      not (Sys.file_exists target))

let check_spec_links paths =
  let failed = ref false in
  let check path =
    if
      Filename.basename path = "AGENTS.md"
      || (String.starts_with ~prefix:"specification/" path && Filename.check_suffix path ".md")
    then
      List.iter
        (fun (line, target) ->
          failed := true;
          Printf.eprintf "%s:%d: missing link target %s\n" path line target)
        (missing_links path (read_file path))
  in
  List.iter (fun path -> if Sys.is_directory path then walk path check else check path) paths;
  if !failed then exit 1

let links_self_test () =
  assert (
    markdown_links
      "[core](../core.md#x) ![image](<a%20b.png>)\n\
       [ref]: ../flow.md\n\
       ```md\n\
       [x](missing)\n\
       ```\n\
       [x](https://example.com/a) [anchor](#ok)"
    = [ (1, "../core.md"); (1, "a b.png"); (2, "../flow.md") ]);
  let file = Filename.temp_file "rays-link-check-" ".md" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      let name = Filename.basename file in
      assert (
        missing_links file ("[ok](" ^ name ^ ") [bad](" ^ name ^ ".missing)")
        = [ (1, name ^ ".missing") ]))

let () =
  let modules = ref false and cut = ref false in
  let rec split ex target dirs = function
    | "--modules" :: r ->
        modules := true;
        split ex target dirs r
    | "--cut-tests" :: r ->
        cut := true;
        split ex target dirs r
    | "--users-exclude" :: s :: r -> split (s :: ex) target dirs r
    | "--target" :: t :: r -> split ex t dirs r
    | d :: r -> split ex target (d :: dirs) r
    | [] -> (ex, target, List.rev dirs)
  in
  match Array.to_list Sys.argv |> List.tl with
  | [ "spec-links"; "--self-test" ] -> links_self_test ()
  | "spec-links" :: paths when paths <> [] -> check_spec_links paths
  | [ "function-lengths"; "--self-test" ] -> lengths_self_test ()
  | "function-lengths" :: "--snapshot" :: paths ->
      source_lengths paths |> List.filter (fun (_, _, length) -> length > 300) |> print_lengths
  | "function-lengths" :: "--prune" :: budget :: paths -> check_lengths ~prune:true budget paths
  | "function-lengths" :: budget :: paths -> check_lengths budget paths
  | [ "history-comments"; "--self-test" ] -> history_comments_self_test ()
  | "history-comments" :: dirs when dirs <> [] -> check_history_comments dirs
  | [ "metal-registry"; "--self-test" ] -> Metal_registry.self_test ()
  | [ "metal-registry"; "--audit" ] -> Metal_registry.audit ()
  | [ "metal-registry"; "--drop-unused-macros" ] ->
      Metal_registry.drop_unused_macros "lib/metal/metal_bridge.mm"
  | "dead-exports" :: args ->
      let excludes, _, dirs = split [] "" [] args in
      List.iter
        (fun (f, n) -> Printf.printf "%s\t%s\n" f n)
        (dead_exports ~modules:!modules ~excludes dirs)
  | "prune" :: args ->
      let excludes, target, dirs = split [] "@check" [] args in
      prune ~cut:!cut ~modules:!modules ~excludes ~target dirs
  | "drop-vals" :: path :: names -> drop_vals path names
  | "dead-fields" :: args ->
      let apply = List.mem "--apply" args in
      let rec skips = function
        | "--skip" :: s :: r ->
            let k, r = skips r in
            (s :: k, r)
        | a :: r ->
            let k, r = skips r in
            (k, a :: r)
        | [] -> ([], [])
      in
      let skip, args = skips (List.filter (( <> ) "--apply") args) in
      let excludes, _, dirs = split [] "" [] args in
      dead_fields ~apply ~excludes ~skip dirs
  | "dead-types" :: dirs -> ignore (dead_types dirs)
  | "drop-types" :: dirs -> drop_types dirs
  | "dead-optionals" :: args ->
      let excludes, _, dirs = split [] "" [] args in
      ignore (dead_optionals ~excludes dirs)
  | "drop-optionals" :: args ->
      let excludes, _, dirs = split [] "" [] args in
      drop_optionals ~excludes dirs
  | "dead-stubs" :: "--report" :: mm :: roots -> dead_stubs ~apply:false ~roots mm
  | "dead-stubs" :: mm :: roots -> dead_stubs ~roots mm
  | [ "drop-c-unused"; dir ] ->
      (* loop: rebuild, feed the log, until nothing is removed *)
      let rec go () =
        let code, log = build "@all" in
        let k = drop_c_unused ~dir log in
        if k > 0 then (
          Printf.printf "removed %d\n%!" k;
          go ())
        else if code <> 0 then (
          prerr_string log;
          exit 1)
      in
      go ()
  | [ "drop-unused" ] -> Printf.printf "%d removed\n" (drop_unused (In_channel.input_all stdin))
  | [ "rename"; "--self-test" ] -> rename_self_test ()
  | [ "rename"; "--exact"; "--dry-run"; old; fresh ] -> rename ~exact:true ~dry:true old fresh
  | [ "rename"; "--exact"; old; fresh ] -> rename ~exact:true ~dry:false old fresh
  | [ "rename"; "--dry-run"; old; fresh ] -> rename ~dry:true old fresh
  | [ "rename"; old; fresh ] -> rename ~dry:false old fresh
  | _ ->
      prerr_endline
        "usage: codemod (dead-exports | prune [--users-exclude S] [--target ALIAS]) DIR...\n\
        \       codemod dead-fields [--apply] [--users-exclude S] [--skip TYPE.FIELD] DIR...\n\
        \       codemod (dead-types | drop-types) DIR...\n\
        \       codemod (dead-optionals | drop-optionals) [--users-exclude S] DIR...\n\
        \       codemod drop-vals FILE.mli NAME... | drop-unused < log\n\
        \       codemod dead-stubs [--report] BRIDGE.mm ROOT... | drop-c-unused DIR\n\
        \       codemod metal-registry [--audit | --drop-unused-macros]\n\
        \       codemod history-comments PATH... | history-comments --self-test\n\
        \       codemod function-lengths [--prune] BUDGET PATH...\n\
        \       codemod spec-links PATH... | spec-links --self-test\n\
        \       codemod rename [--dry-run] OLD NEW | rename --self-test";
      exit 2
