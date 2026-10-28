open Flow
open Syntax

let get = function
  | Ok value -> value
  | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)

let one source =
  match get (parse source) with [ form ] -> form | _ -> failwith ("expected one form: " ^ source)

let rec ids x = x.id :: List.concat_map ids (children x)

let () =
  (* atoms keep their spelling *)
  let forms = get (parse "[-.5 1. 2.0 3 - 1e3 :kw \"s\\n\\\"q\\\"\" a/b :]") in
  (match forms with
  | [
   {
     node =
       Vec
         [
           { node = Num "-.5"; _ };
           { node = Num "1."; _ };
           { node = Num "2.0"; _ };
           { node = Num "3"; _ };
           { node = Sym "-"; _ };
           { node = Num "1e3"; _ };
           { node = Kw "kw"; _ };
           { node = Str "s\n\"q\""; _ };
           { node = Sym "a/b"; _ };
           { node = Sym ":"; _ };
         ];
     _;
   };
  ] ->
      ()
  | _ -> failwith "atoms misread");
  (* vectors of any length, maps, quote forms *)
  (match (one "{:a 1 :b [1 2 3 4]}").node with
  | Map [ { node = Kw "a"; _ }; _; { node = Kw "b"; _ }; { node = Vec [ _; _; _; _ ]; _ } ] -> ()
  | _ -> failwith "map misread");
  (match (one "(f 'a `(b ~c ~@d))").node with
  | List
      [
        _;
        { node = Quote (Plain, { node = Sym "a"; _ }); _ };
        {
          node =
            Quote
              ( Quasi,
                {
                  node =
                    List
                      [
                        _;
                        { node = Quote (Unquote, _); _ };
                        { node = Quote (Splice, { node = Sym "d"; _ }); _ };
                      ];
                  _;
                } );
          _;
        };
      ] ->
      ()
  | _ -> failwith "quotes misread");
  (* [~i] and [step#] are ordinary symbol characters *)
  match (one "[a' step# ~i]").node with
  | Vec
      [
        { node = Sym "a'"; _ };
        { node = Sym "step#"; _ };
        { node = Quote (Unquote, { node = Sym "i"; _ }); _ };
      ] ->
      ()
  | _ -> failwith "symbol characters misread"

let () =
  (* meta flags keep their order and span the flag *)
  let source = "(a ^:bypass ^:x (b c))" in
  (match (one source).node with
  | List [ _; ({ meta = [ "bypass"; "x" ]; node = List _; _ } as m) ] ->
      assert (String.sub source m.span.start (m.span.finish - m.span.start) = "^:bypass ^:x (b c)")
  | _ -> failwith "meta misread");
  (* comments are notes on the next form; before a bracket they are the tail *)
  let forms =
    get (parse ";; head\n; second\n(a ; on b\n b\n  ;\n c ; trailing\n )\n; after\n(d)\n; end\n")
  in
  (match forms with
  | [
   { notes = [ "head"; "second" ]; tail = [ "trailing" ]; node = List [ _; b; c ]; _ };
   { notes = [ "after" ]; tail = [ "end" ]; node = List [ _ ]; _ };
  ] ->
      assert (b.notes = [ "on b" ]);
      assert (c.notes = [ "" ])
  | _ -> failwith "notes misplaced");
  (match (one "(a ; end\n)").tail with [ "end" ] -> () | _ -> failwith "tail");
  (* a comment between a prefix and its form moves before the prefix *)
  match (one "(f ; c\n ' ; d\n x)").node with
  | List [ _; { notes = [ "c"; "d" ]; node = Quote (Plain, { notes = []; _ }); _ } ] -> ()
  | _ -> failwith "prefix comment"

let () =
  (* ids: unique, reading order, renumber gives a disjoint range *)
  let a = one "(a [b c] 'd)" in
  assert (ids a = [ 0; 1; 2; 3; 4; 5; 6 ]);
  let b, next = renumber 100 a in
  assert (ids b = [ 100; 101; 102; 103; 104; 105; 106 ] && next = 107);
  assert (b.span = a.span);
  let second = get (parse "x y") in
  assert (List.map (fun f -> f.id) second = [ 0; 1 ]);
  assert ((make (Sym "z")).id = 0)

let () =
  let fails source code offset =
    match parse source with
    | Ok _ -> failwith ("accepted: " ^ source)
    | Error d ->
        assert (d.code = code);
        assert (d.position = Some (Diagnostic.position_of_offset source offset));
        assert (Option.map (fun s -> s.Diagnostic.start) d.span = Some offset)
  in
  fails "(" "E_UNCLOSED" 0;
  fails "(a\n  [b" "E_UNCLOSED" 5;
  fails "{:a 1" "E_UNCLOSED" 0;
  fails "(a]" "E_UNEXPECTED" 2;
  fails "[a}" "E_UNEXPECTED" 2;
  fails ")" "E_UNEXPECTED" 0;
  fails "\"open" "E_UNCLOSED" 0;
  fails "(a \"\\q\")" "E_UNEXPECTED" 4;
  fails "^ x" "E_UNEXPECTED" 0;
  fails "(a ^:bypass)" "E_UNEXPECTED" 3;
  fails "'" "E_UNEXPECTED" 1;
  fails (String.make 300 '(' ^ String.make 300 ')') "E_DEPTH" 257;
  assert (Result.is_ok (parse (String.make 250 '(' ^ String.make 250 ')')));
  (* a -> chain nests as deep as it is long *)
  let chain n = "(-> x" ^ String.concat "" (List.init n (fun _ -> " (f)")) ^ ")" in
  assert (Result.is_ok (parse (chain 200)));
  fails (chain 100_000) "E_DEPTH" 0;
  (* the printer's marker bytes are refused anywhere, comments and strings included *)
  fails "(a) ; \001" "E_UNEXPECTED" 6;
  fails "\"\003\"" "E_UNEXPECTED" 1;
  (* numbers with an exponent *)
  List.iter (fun w -> assert (number w)) [ "1e-14"; "2e+06"; "6.1E-17"; "-3.3e7"; "1.e2" ];
  List.iter (fun w -> assert (not (number w))) [ "e5"; "1e"; "1e+"; "1e5x"; "-"; "." ];
  assert (get (parse "") = [] && get (parse "; only a comment") = []);
  (* The checked reader and the editor share tolerant lexical tokens. A bad escape keeps its
     original diagnostic span, while the editor still receives later tokens for coloring. *)
  let source = "(a \"bad\\q\" b)" in
  let tokens = Text_lexer.lex source in
  let issue = Array.find_map (fun token -> token.Text_lexer.issue) tokens |> Option.get in
  assert (issue.code = "E_UNEXPECTED" && issue.start = 7 && issue.finish = 9);
  assert (
    List.exists
      (fun token ->
        match token.Text_lexer.kind with
        | Text_lexer.Word (Text_lexer.Symbol "b") -> token.start = 11
        | _ -> false)
      (Array.to_list tokens));
  (match parse source with
  | Error diagnostic ->
      assert (
        diagnostic.code = issue.code
        && Option.map (fun span -> span.Diagnostic.start) diagnostic.span = Some issue.start)
  | Ok _ -> failwith "accepted invalid string escape");
  (* the message names the opening line *)
  match parse "(a\n(b]" with
  | Error d ->
      assert (d.position = Some Diagnostic.{ line = 2; col = 3 });
      assert (String.length d.message > 0)
  | Ok _ -> failwith "accepted"
