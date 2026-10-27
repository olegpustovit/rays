(* Compiler-tree comparison for result syntax. Cleanup and error recovery
   branches must remain structurally equal. *)
open Parsetree

let parse path source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf path;
  Lexer.init ();
  Parse.implementation lexbuf

let ident name e =
  e.pexp_attributes = []
  &&
  match e.pexp_desc with
  | Pexp_ident { txt = Longident.Lident value; _ } -> value = name
  | _ -> false

let constructor name p =
  match p.ppat_desc with
  | Ppat_construct ({ txt = Longident.Lident value; _ }, Some ([], payload))
    when value = name && p.ppat_attributes = [] ->
      Some payload
  | _ -> None

let unit_pattern p =
  match p.ppat_desc with
  | Ppat_construct ({ txt = Longident.Lident "()"; _ }, None) -> true
  | _ -> false

let bind_pattern p = match p.ppat_desc with Ppat_var name -> name.txt = "let*" | _ -> false

let result_bind expression =
  let is_bind expression =
    match expression.pexp_desc with
    | Pexp_ident { txt = Longident.Ldot (Longident.Lident "Result", "bind"); _ } -> true
    | _ -> false
  in
  if is_bind expression then true
  else
    match expression.pexp_desc with
    | Pexp_function
        ( [
            { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var a; _ }); _ };
            { pparam_desc = Pparam_val (Nolabel, None, { ppat_desc = Ppat_var b; _ }); _ };
          ],
          None,
          Pfunction_body { pexp_desc = Pexp_apply (fn, [ (Nolabel, x); (Nolabel, y) ]); _ } ) ->
        is_bind fn && ident a.txt x && ident b.txt y
    | _ -> false

let propagation case =
  if case.pc_guard <> None || case.pc_rhs.pexp_attributes <> [] then None
  else
    match (case.pc_lhs.ppat_desc, case.pc_rhs.pexp_desc) with
    | Ppat_alias (p, name), _ when ident name.txt case.pc_rhs -> (
        match constructor "Error" p with Some { ppat_desc = Ppat_any; _ } -> Some None | _ -> None)
    | _, Pexp_apply (fn, [ (Nolabel, operation); (Nolabel, message) ]) when ident "native_error" fn
      -> (
        match constructor "Error" case.pc_lhs with
        | Some { ppat_desc = Ppat_var name; _ } when ident name.txt message -> (
            (* Evaluating the operation eagerly is safe only for inert values. *)
            match operation.pexp_desc with
            | Pexp_ident _ | Pexp_constant _ -> Some (Some operation)
            | _ -> None)
        | _ -> None)
    | _ -> None

(* Compare compiler trees after canonicalizing only the syntax being migrated.
   Cleanup branches and all other expressions must remain structurally equal. *)
let canonical tree =
  let open Ast_helper in
  let name value = Location.mknoloc (Longident.Lident value) in
  let variable value = Pat.var (Location.mknoloc value) in
  let identifier value = Exp.ident (name value) in
  let result input operation pattern body =
    let failure =
      match operation with
      | None ->
          Exp.case
            (Pat.alias
               (Pat.construct (name "Error") (Some ([], Pat.any ())))
               (Location.mknoloc "__failure"))
            (identifier "__failure")
      | Some operation ->
          Exp.case
            (Pat.construct (name "Error") (Some ([], variable "__message")))
            (Exp.apply (identifier "native_error")
               [ (Nolabel, operation); (Nolabel, identifier "__message") ])
    in
    Exp.match_ input [ failure; Exp.case (Pat.construct (name "Ok") (Some ([], pattern))) body ]
  in
  let mapper =
    {
      Ast_mapper.default_mapper with
      structure =
        (fun self items ->
          let items = Ast_mapper.default_mapper.structure self items in
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
          let expression = Ast_mapper.default_mapper.expr self expression in
          match expression.pexp_desc with
          | Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Longident.Lident "on_main"; _ }; _ },
                [
                  (Nolabel, operation);
                  ( Nolabel,
                    {
                      pexp_desc =
                        Pexp_function
                          ( [ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ],
                            None,
                            Pfunction_body body );
                      pexp_attributes = [];
                      _;
                    } );
                ] )
            when unit_pattern pattern
                 &&
                 match operation.pexp_desc with
                 | Pexp_ident _ | Pexp_constant _ -> true
                 | _ -> false ->
              result
                (Exp.apply (identifier "before_main") [ (Nolabel, operation) ])
                None pattern body
          | Pexp_letop { let_ = binding; ands = []; body } when binding.pbop_op.txt = "let*" ->
              result binding.pbop_exp None binding.pbop_pat body
          | Pexp_let (Nonrecursive, [ binding ], body)
            when bind_pattern binding.pvb_pat && result_bind binding.pvb_expr ->
              body
          | Pexp_apply
              ( {
                  pexp_desc =
                    Pexp_ident { txt = Longident.Ldot (Longident.Lident "Result", "bind"); _ };
                  _;
                },
                [
                  (Nolabel, input);
                  ( Nolabel,
                    {
                      pexp_desc =
                        Pexp_function
                          ( [ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ],
                            None,
                            Pfunction_body body );
                      _;
                    } );
                ] ) ->
              result input None pattern body
          | Pexp_match (input, cases) -> (
              let failures, successes = List.partition (fun c -> propagation c <> None) cases in
              match (failures, successes) with
              | [ failure ], [ success ] -> (
                  match (constructor "Ok" success.pc_lhs, success.pc_guard) with
                  | Some pattern, None ->
                      result input (Option.get (propagation failure)) pattern success.pc_rhs
                  | _ -> expression)
              | [ failure ], _ :: _
                when List.for_all
                       (fun c ->
                         match constructor "Ok" c.pc_lhs with
                         | Some p -> unit_pattern p
                         | _ -> false)
                       successes -> (
                  let rec branches = function
                    | [ { pc_guard = None; pc_rhs; _ } ] -> Some pc_rhs
                    | { pc_guard = Some guard; pc_rhs; _ } :: rest ->
                        Option.map
                          (fun rest -> Exp.ifthenelse guard pc_rhs (Some rest))
                          (branches rest)
                    | _ -> None
                  in
                  match branches successes with
                  | Some body ->
                      result input
                        (Option.get (propagation failure))
                        (Pat.construct (name "()") None)
                        body
                  | None -> expression)
              | _ -> expression)
          | _ -> expression);
    }
  in
  Format.asprintf "%a" Pprintast.structure (mapper.structure mapper tree)

let verify before after ppx =
  let read path = In_channel.with_open_bin path In_channel.input_all in
  let temporary = Filename.temp_file "rays-result-bind-verify-" ".ml" in
  Fun.protect
    ~finally:(fun () -> Sys.remove temporary)
    (fun () ->
      if
        Sys.command
          (Filename.quote ppx ^ " --text " ^ Filename.quote after ^ " > " ^ Filename.quote temporary)
        <> 0
      then failwith "result-bind preprocessing failed";
      let original = canonical (parse before (read before))
      and migrated = canonical (parse after (read temporary)) in
      if original <> migrated then begin
        Out_channel.with_open_bin "/tmp/rays-result-bind-original.ast" (fun channel ->
            output_string channel original);
        Out_channel.with_open_bin "/tmp/rays-result-bind-migrated.ast" (fun channel ->
            output_string channel migrated);
        failwith
          "result-bind changed the compiler tree beyond result syntax (see \
           /tmp/rays-result-bind-*.ast)"
      end;
      print_endline "result-bind: entire compiler tree matches after result-syntax normalization")

let self_test () =
  let normalized source = canonical (parse "fixture.ml" source) in
  let matched = normalized "let f x = match x with Error _ as e -> e | Ok y -> Ok (y + 1)" in
  assert (matched = normalized "let f x = Result.bind x (fun y -> Ok (y + 1))");
  assert (matched = normalized "let f x = let ( let* ) = Result.bind in let* y = x in Ok (y + 1)");
  assert (matched <> normalized "let f x = Result.bind x (fun y -> Ok (y + 2))");
  assert (matched <> normalized "let f x = match x with Error _ -> Ok 0 | Ok y -> Ok (y + 1)")
