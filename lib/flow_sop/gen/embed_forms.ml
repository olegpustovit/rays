(* The node library's parsed forms, marshalled one by one into [Node_sources.forms] at build time, with
   an index ([Node_sources.index]) of what a process needs without them: each form's name, size, the
   names it mentions, its offset, and a defnode's declaration without its Lisp. A process unmarshals
   the forms a workspace reaches. A form that does not parse, or a form the library may not hold,
   fails the build. Words repeat inside a form, so equal strings are one string. *)
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
  let forms =
    List.concat_map
      (fun (file, forms) ->
        List.concat_map
          (fun (form : Flow.Syntax.t) ->
            match form.node with
            | Flow.Syntax.List ({ node = Flow.Syntax.Sym "workspace"; _ } :: _ :: children) ->
                List.map (fun child -> (file, child)) children
            | _ -> [ (file, form) ])
          forms)
      parsed
  in
  let empty = List.hd (Result.get_ok (Flow.Syntax.parse "()")) in
  let rec size (form : Flow.Syntax.t) =
    List.fold_left (fun n child -> n + size child) 1 (Flow.Syntax.children form)
  in
  let rec mentions into (form : Flow.Syntax.t) =
    (match form.node with Flow.Syntax.Sym name -> Hashtbl.replace into (intern name) () | _ -> ());
    List.iter (mentions into) (Flow.Syntax.children form)
  in
  let blob = Buffer.create (1 lsl 20) in
  let index =
    List.map
      (fun (file, (form : Flow.Syntax.t)) ->
        let name =
          match form.node with
          | Flow.Syntax.List
              ({ node = Sym ("defnode" | "defn" | "defmacro"); _ } :: { node = Sym name; _ } :: _) ->
              intern name
          | _ ->
              prerr_endline (file ^ ": node sources contain only defnode, defn and defmacro forms");
              exit 1
        in
        let header =
          if Flow.Syntax.head form <> Some "defnode" then None
          else
            match Flow.Defnode.parse form with
            | Ok node ->
                Some
                  {
                    node with
                    inputs = [];
                    body = empty;
                    validations = [];
                    normalizers = [];
                    form = empty;
                  }
            | Error diagnostic ->
                prerr_endline (file ^ ": " ^ Diagnostic.to_string diagnostic);
                exit 1
        in
        let names = Hashtbl.create 16 in
        mentions names form;
        let offset = Buffer.length blob in
        Buffer.add_string blob (Marshal.to_string form []);
        ( name,
          size form,
          List.sort compare (Hashtbl.fold (fun name () names -> name :: names) names []),
          offset,
          header ))
      forms
  in
  Printf.printf "let forms = %S\n" (Buffer.contents blob);
  Printf.printf "let index = %S\n"
    (Marshal.to_string
       (index : (string * int * string list * int * Flow.Defnode.t option) list)
       [])
