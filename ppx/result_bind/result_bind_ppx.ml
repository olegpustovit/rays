(* Lower Metal's result syntax before typing: the classic OCaml compiler does
   not eliminate let-operator continuation allocations, even with inline hints. *)
open Ast_helper
open Parsetree

let name value = Location.mknoloc (Longident.Lident value)
let variable value = Pat.var (Location.mknoloc value)
let identifier value = Exp.ident (name value)
let names = Hashtbl.create 256

let fresh prefix =
  let rec choose value =
    if Hashtbl.mem names value then choose (value ^ "_")
    else (
      Hashtbl.add names value ();
      value)
  in
  choose prefix

let mapper =
  {
    Ast_mapper.default_mapper with
    structure =
      (fun self items ->
        let items =
          List.filter_map
            (fun item ->
              match item.pstr_desc with
              | Pstr_value
                  ( Nonrecursive,
                    [
                      {
                        pvb_pat = { ppat_desc = Ppat_var name; _ };
                        pvb_expr = { pexp_desc = Pexp_ident { txt; _ }; _ };
                        _;
                      };
                    ] )
                when (name.txt = "let*" && txt = Longident.Ldot (Longident.Lident "Result", "bind"))
                     || (name.txt = "native_result" && txt = Longident.Lident "native") ->
                  None
              | _ -> Some (Ast_mapper.default_mapper.structure_item self item))
            items
        in
        let used = ref false in
        let iterator =
          {
            Ast_iterator.default_iterator with
            expr =
              (fun self expression ->
                (match expression.pexp_desc with
                | Pexp_ident { txt = Longident.Lident "on_main"; _ } -> used := true
                | _ -> ());
                Ast_iterator.default_iterator.expr self expression);
          }
        in
        iterator.structure iterator items;
        if !used then items
        else
          List.filter
            (fun item ->
              match item.pstr_desc with
              | Pstr_value
                  ( Nonrecursive,
                    [
                      {
                        pvb_pat = { ppat_desc = Ppat_var name; _ };
                        pvb_expr = { pexp_desc = Pexp_function _; _ };
                        _;
                      };
                    ] ) ->
                  name.txt <> "on_main"
              | _ -> true)
            items);
    expr =
      (fun self expression ->
        match expression.pexp_desc with
        | Pexp_apply
            ( { pexp_desc = Pexp_ident { txt = Longident.Lident "on_main"; _ }; _ },
              [
                (Nolabel, operation);
                ( Nolabel,
                  {
                    pexp_desc =
                      Pexp_function
                        ( [
                            {
                              pparam_desc =
                                Pparam_val
                                  ( Nolabel,
                                    None,
                                    {
                                      ppat_desc =
                                        Ppat_construct ({ txt = Longident.Lident "()"; _ }, None);
                                      _;
                                    } );
                              _;
                            };
                          ],
                          None,
                          Pfunction_body body );
                    pexp_attributes = [];
                    _;
                  } );
              ] )
          when match operation.pexp_desc with Pexp_ident _ | Pexp_constant _ -> true | _ -> false ->
            let failure = fresh "__metal_main_failure" in
            let result =
              Exp.match_ ~loc:expression.pexp_loc
                (Exp.apply (identifier "before_main") [ (Nolabel, operation) ])
                [
                  Exp.case
                    (Pat.alias
                       (Pat.construct (name "Error") (Some ([], Pat.any ())))
                       (Location.mknoloc failure))
                    (identifier failure);
                  Exp.case
                    (Pat.construct (name "Ok") (Some ([], Pat.construct (name "()") None)))
                    body;
                ]
            in
            self.expr self { result with pexp_attributes = expression.pexp_attributes }
        | Pexp_letop { let_ = binding; ands = []; body } when binding.pbop_op.txt = "let*" ->
            let loc = expression.pexp_loc in
            let input, failure =
              match binding.pbop_exp.pexp_desc with
              | Pexp_apply
                  ( { pexp_desc = Pexp_ident { txt = Longident.Lident "native_result"; _ }; _ },
                    [ (Nolabel, operation); (Nolabel, input) ] ) ->
                  let message = fresh "__metal_native_message" in
                  ( input,
                    Exp.case
                      (Pat.construct (name "Error") (Some ([], variable message)))
                      (Exp.apply (identifier "native_error")
                         [ (Nolabel, operation); (Nolabel, identifier message) ]) )
              | _ ->
                  let failure = fresh "__metal_result_failure" in
                  ( binding.pbop_exp,
                    Exp.case
                      (Pat.alias
                         (Pat.construct (name "Error") (Some ([], Pat.any ())))
                         (Location.mknoloc failure))
                      (identifier failure) )
            in
            let result =
              Exp.match_ ~loc input
                [ failure; Exp.case (Pat.construct (name "Ok") (Some ([], binding.pbop_pat))) body ]
            in
            self.expr self { result with pexp_attributes = expression.pexp_attributes }
        | _ -> Ast_mapper.default_mapper.expr self expression);
  }

let () =
  let path, source, text =
    match Array.to_list Sys.argv with
    | [ _; "--text"; path ] -> (path, path, true)
    | [ _; "--source-dir"; directory; path ] ->
        (path, Filename.concat directory (Filename.basename path), false)
    | [ _; path ] -> (path, path, false)
    | _ -> failwith "usage: result_bind_ppx [--text | --source-dir DIR] FILE.ml"
  in
  let emit kind printer tree =
    if text then Format.printf "%a@." printer tree
    else begin
      let temporary = Filename.temp_file "rays-result-bind-ast-" ".ast" in
      Fun.protect
        ~finally:(fun () -> Sys.remove temporary)
        (fun () ->
          Location.input_name := source;
          Pparse.write_ast kind temporary tree;
          output_string stdout (In_channel.with_open_bin temporary In_channel.input_all))
    end
  in
  In_channel.with_open_bin path (fun channel ->
      let lexbuf = Lexing.from_channel channel in
      Location.init lexbuf source;
      if Filename.check_suffix path ".mli" then
        emit Pparse.Signature Pprintast.signature (Parse.interface lexbuf)
      else
        let tree = Parse.implementation lexbuf in
        let iterator =
          {
            Ast_iterator.default_iterator with
            expr =
              (fun self expression ->
                (match expression.pexp_desc with
                | Pexp_ident { txt = Longident.Lident name; _ } -> Hashtbl.replace names name ()
                | _ -> ());
                Ast_iterator.default_iterator.expr self expression);
            pat =
              (fun self pattern ->
                (match pattern.ppat_desc with
                | Ppat_var name | Ppat_alias (_, name) -> Hashtbl.replace names name.txt ()
                | _ -> ());
                Ast_iterator.default_iterator.pat self pattern);
          }
        in
        iterator.structure iterator tree;
        emit Pparse.Structure Pprintast.structure (mapper.structure mapper tree))
