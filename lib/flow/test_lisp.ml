open Flow

let get = function
  | Ok value -> value
  | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)

let print source = fst (Lisp.print (get (Syntax.parse source)))
let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  let text = String.init 256 Char.chr in
  let form = Syntax.make (Syntax.Str text) in
  List.iter
    (fun printed ->
      assert (not (String.exists (fun c -> c >= '\001' && c <= '\003') printed));
      match get (Syntax.parse printed) with
      | [ { Syntax.node = Str decoded; _ } ] -> assert (decoded = text)
      | _ -> assert false)
    [ Lisp.flat form; fst (Lisp.print [ form ]) ];
  List.iter
    (fun source -> assert (Result.is_error (Syntax.parse source)))
    [ "\"\\256\""; "\"\\999\""; "\"\\01\""; "\"\\1ab\""; "\"\\12\""; "\"\\00\"" ];
  for byte = 0 to 255 do
    match get (Syntax.parse (Printf.sprintf "\"\\%03d\"" byte)) with
    | [ { Syntax.node = Str decoded; _ } ] -> assert (decoded = String.make 1 (Char.chr byte))
    | _ -> assert false
  done

let first_difference expected actual =
  let lines s = String.split_on_char '\n' s in
  let rec go n = function
    | x :: xs, y :: ys when x = y -> go (n + 1) (xs, ys)
    | x :: _, y :: _ -> Printf.sprintf "line %d\nwant %s\ngot  %s" n x y
    | _ -> "length differs"
  in
  go 1 (lines expected, lines actual)

(* (source, text printed by the study's pp) *)
let golden =
  [
    ( {golden|(sop/box ; the size
 :size [1 2 3] :center [0 0 0])|golden},
      {golden|(sop/box
  ; the size
  :size [1 2 3]
  :center [0 0 0])|golden} );
    ( {golden|(sop/box :size [1 2 3]
 ; where it sits
 :center [0 0 0])|golden},
      {golden|(sop/box
  :size [1 2 3]
  ; where it sits
  :center [0 0 0])|golden} );
    ( {golden|(sop/box :size [1 2 3]
 :center ; on top
 [0 0 0])|golden},
      {golden|(sop/box
  :size [1 2 3]
  :center
    ; on top
    [0 0 0])|golden} );
    ( {golden|(let* [a 1 ; one
 b 2
 ; last
 ] (+ a b))|golden},
      {golden|(let* [a 1
       ; one
       b 2
       ; last
       ]
  (+ a b))|golden} );
    ( {golden|(let* [a 1
 b
 ; the value
 2] (+ a b))|golden},
      {golden|(let* [a 1
       b
         ; the value
         2]
  (+ a b))|golden} );
    ( {golden|(let* [a 1 b 2 c 3] (+ a b c))|golden},
      {golden|(let* [a 1
       b 2
       c 3]
  (+ a b c))|golden} );
    ({golden|[1 ; first
 2 3]|golden}, {golden|[1
 ; first
 2
 3]|golden});
    ({golden|[1 2 3
 ; done
 ]|golden}, {golden|[1
 2
 3
 ; done
 ]|golden});
    ( {golden|{:a 1 ; note on b
 :b 2 :c
 ; on value
 3}|golden},
      {golden|{:a 1
 ; note on b
 :b 2
 :c
   ; on value
   3}|golden} );
    ({golden|{:a 1 :b 2}|golden}, {golden|{:a 1 :b 2}|golden});
    ( {golden|(cond ; first
 (= f 0) big.frame (= f 1) small.pane
 ; otherwise
 :else small.frame)|golden},
      {golden|(cond
  ; first
  (= f 0) big.frame
  (= f 1) small.pane
  ; otherwise
  :else small.frame)|golden}
    );
    ( {golden|(case (mod f 3) 0 "#b0680f" 1 "#285f77" :else "#6b50ae")|golden},
      {golden|(case (mod f 3) 0 "#b0680f" 1 "#285f77" :else "#6b50ae")|golden} );
    ( {golden|(case ; why
 x 0 "a" :else "b")|golden},
      {golden|(case
  ; why
  x
  0 "a"
  :else "b")|golden} );
    ( {golden|(if ; test
 (> x 1) (sop/box) (sop/uv_sphere))|golden},
      {golden|(if
    ; test
    (> x 1)
    (sop/box)
    (sop/uv_sphere))|golden} );
    ( {golden|(if (> x 1) (sop/box :size [1 1 1] :center [0 0 0] :rotation [0 0.5 0]) (sop/uv_sphere :radius 2 :segments 12 :rings 8))|golden},
      {golden|(if (> x 1)
    (sop/box :size [1 1 1] :center [0 0 0] :rotation [0 0.5 0])
    (sop/uv_sphere :radius 2 :segments 12 :rings 8))|golden}
    );
    ( {golden|(fold [acc 0] [i (range 10)] ; step
 (+ acc i))|golden},
      {golden|(fold [acc 0]
      [i (range 10)]
  ; step
  (+ acc i))|golden} );
    ( {golden|(fold [shape (sop/box)] [i (range steps)] (sop/merge shape (sop/transform shape :rotate [0 0.1 0] :uniform_scale 0.9)))|golden},
      {golden|(fold [shape (sop/box)]
      [i (range steps)]
  (sop/merge shape (sop/transform shape :rotate [0 0.1 0] :uniform_scale 0.9)))|golden}
    );
    ( {golden|(for [i (range 3) j (range 4)] (sop/uv_sphere :radius 0.1 :center [i j 0] :segments 8 :rings 4 :rotation [0 1 0]))|golden},
      {golden|(for [i (range 3)
      j (range 4)]
  (sop/uv_sphere :radius 0.1 :center [i j 0] :segments 8 :rings 4 :rotation [0 1 0]))|golden}
    );
    ( {golden|(sum [k (range 5)] (/ 1 (+ k 1)))|golden},
      {golden|(sum [k (range 5)] (/ 1 (+ k 1)))|golden} );
    ({golden|(fn [x y] (* x y))|golden}, {golden|(fn [x y] (* x y))|golden});
    ({golden|(fn [i] ; scale
 (* i 2))|golden}, {golden|(fn [i]
  ; scale
  (* i 2))|golden});
    ( {golden|(defmacro twice [x] `(+ ~x ~x))|golden},
      {golden|(defmacro twice [x]
  `(+ ~x ~x))|golden} );
    ( {golden|(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))|golden},
      {golden|(defmacro radial [i n body]
  `(sop/merge (for [~i (range ~n)]
                (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))|golden}
    );
    ( {golden|(defmacro spl [xs] `(sop/merge ~@xs))|golden},
      {golden|(defmacro spl [xs]
  `(sop/merge ~@xs))|golden} );
    ( {golden|(graph g :context sop (sop/box))|golden},
      {golden|(graph g :context sop
  (sop/box))|golden} );
    ( {golden|(graph g :context sop [(n : int 3)] ; body
 (sop/box))|golden},
      {golden|(graph g :context sop [(n : int 3)]
  ; body
  (sop/box))|golden} );
    ( {golden|(graph g :context sop [(n : int 3)] (sop/box)
 ; end
)|golden},
      {golden|(graph g :context sop [(n : int 3)]
  (sop/box)
  ; end
  )|golden} );
    ( {golden|(nums 2.0 .5 1. 0.50 -3 -0.25 1.500000)|golden},
      {golden|(nums 2.0 0.5 1.0 0.5 -3 -0.25 1.5)|golden} );
    ({golden|(str "a\"b\\c\n\td" "é · ü")|golden}, {golden|(str "a\"b\\c\n\td" "é · ü")|golden});
    ( {golden|(sop/merge aaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccc ddddddddddddddddddddddd)|golden},
      {golden|(sop/merge aaaaaaaaaaaaaaaaaaaaaaaa
           bbbbbbbbbbbbbbbbbbbbbbbbbbbbb
           cccccccccccccccccccccccccc
           ddddddddddddddddddddddd)|golden}
    );
    ( {golden|(some-long-function-name-here-x aaaaaaaaaaaaaaaaaaaaa :key bbbbbbbbbbbbbbbbbbbbbbbb :other (ccccccccccccccc dddddddddddddd))|golden},
      {golden|(some-long-function-name-here-x aaaaaaaaaaaaaaaaaaaaa
    :key bbbbbbbbbbbbbbbbbbbbbbbb
    :other (ccccccccccccccc dddddddddddddd))|golden}
    );
    ( {golden|(sop/uv_sphere :radius [0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45] :segments 12)|golden},
      {golden|(sop/uv_sphere :radius [0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45 0.04 0.12 0.45]
               :segments 12)|golden}
    );
    ( {golden|(let* [{:keys [shape y]} st [l m r] xs
 p (sop/transform shape :translate [0 y 0] :rotate [0 (* f 0.3) 0] :uniform_scale 0.5)] p)|golden},
      {golden|(let* [{:keys [shape y]} st
       [l m r] xs
       p (sop/transform shape
                        :translate [0 y 0]
                        :rotate [0 (* f 0.3) 0]
                        :uniform_scale 0.5)]
  p)|golden}
    );
    ( {golden|(workspace w
 ; a
 ; b

 ;; c
 (graph g :context sop (sop/box)) (defn h :context value [(x : float)] x)
 ; trailing
)|golden},
      {golden|(workspace w

  ; a
  ; b
  ; c
  (graph g :context sop
    (sop/box))

  (defn h :context value [(x : float)]
    x)
  ; trailing
  )|golden}
    );
    ({golden|(vec3 1 2 3)|golden}, {golden|(vec3 1 2 3)|golden});
    ({golden|()|golden}, {golden|()|golden});
    ( {golden|(sop/merge (for [r (range rings) m (range moons)] (let* [radius (+ 0.6 (* r 0.35))] (sop/uv_sphere :radius radius))))|golden},
      {golden|(sop/merge (for [r (range rings)
                 m (range moons)]
             (let* [radius (+ 0.6 (* r 0.35))] (sop/uv_sphere :radius radius))))|golden}
    );
  ]

let cases = "../../specification/workspace/cases"

let () =
  let files =
    List.filter (fun f -> Filename.check_suffix f ".lisp") (Array.to_list (Sys.readdir cases))
    |> List.sort compare
  in
  assert (List.length files = 12);
  List.iter
    (fun file ->
      let source = read (Filename.concat cases file) in
      let text = print source in
      (* the fixtures are the canonical text, and printing is a fixed point *)
      if text <> source then
        failwith (file ^ ": fixture is not canonical, " ^ first_difference source text);
      assert (print text = text);
      String.split_on_char '\n' text
      |> List.iter (fun line ->
          assert (String.length line = 0 || line.[String.length line - 1] <> ' ')))
    files

let () =
  List.iter
    (fun (source, expected) ->
      let text = print source in
      let expected = expected ^ "\n" in
      if text <> expected then failwith (source ^ ": " ^ first_difference expected text);
      assert (print text = text))
    golden

(* Deliberate differences from the study's printer: it drops a lone empty
   comment, rewrites 007 and rounds 0.0000001 to 0; here nothing is lost. *)
let () =
  assert (print "(a\n ;\n b)" = "(a\n  ;\n  b)\n");
  assert (print "(nums 007 0.0000001 1e3 -.5)" = "(nums 007 0.0000001 1e3 -0.5)\n");
  assert (print "(quote ('a `b ~c ~@d))" = "(quote ('a `b ~c ~@d))\n");
  assert (
    print "(a ^:bypass (sop/subdivide x :iterations 1) ^:x ^:y z)"
    = "(a ^:bypass (sop/subdivide x :iterations 1) ^:x ^:y z)\n");
  assert (print "x ; note\n(a)\n" = "x\n\n; note\n(a)\n")

(* Spans: every form's byte range in the printed text. *)
let () =
  let forms = get (Syntax.parse "; hi\n(a ^:bypass (b [1 2]) \"s\")") in
  let text, spans = Lisp.print forms in
  assert (text = "; hi\n(a ^:bypass (b [1 2]) \"s\")\n");
  let at id =
    let s = List.assoc id spans in
    String.sub text s.Diagnostic.start (s.finish - s.start)
  in
  let ids = List.map fst spans in
  assert (List.length ids = 8 && List.sort_uniq compare ids = ids);
  assert (at 0 = "(a ^:bypass (b [1 2]) \"s\")");
  assert (at 1 = "a");
  assert (at 2 = "^:bypass (b [1 2])");
  assert (at 4 = "[1 2]");
  assert (at 7 = "\"s\"");
  (* spans follow layout: a broken form has line breaks inside its span *)
  let broken =
    get
      (Syntax.parse
         (String.concat " " (List.init 30 (fun i -> Printf.sprintf "(f%d :k %d)" i i))
         |> Printf.sprintf "(all %s)"))
  in
  let text, spans = Lisp.print broken in
  let s = List.assoc 0 spans in
  assert (s.start = 0 && s.finish = String.length text - 1 && String.contains text '\n');
  assert (
    Lisp.flat (List.hd broken)
    = String.concat ""
        [
          "(all "; String.concat " " (List.init 30 (fun i -> Printf.sprintf "(f%d :k %d)" i i)); ")";
        ])

(* A loop's :skip (register L16) prints between its clauses and its body, flat or broken, and
   the text reads back to the same forms. *)
let () =
  let again text = fst (Lisp.print (get (Syntax.parse text))) in
  let short = "(for [i (range 4)] :skip [1 3] (scene/light :name \"a\"))" in
  assert (print short = short ^ "\n");
  let long =
    "(for [i (range 4) j (range 3) k (range 5)] :skip [[0 1 2] [1 1 1] [3 2 4]] (scene/light :name \
     \"lamp\" :translate [i j k] :intensity 30))"
  in
  let text = print long in
  assert (String.contains text '\n');
  assert (again text = text);
  assert (Lisp.flat (List.hd (get (Syntax.parse (again text)))) = long)

(* The threading form: [(-> x (f a) (g b))] reads as [(g (f x a) b)]; the printer threads a chain
   of three calls or more and leaves shorter ones, notes and flags nested.  Spans of the steps
   are their clauses. *)
let () =
  let nested = "(sop/c (sop/b (sop/a x :k 1) :j 2))" in
  let threaded = "(-> x\n    (sop/a :k 1)\n    (sop/b :j 2)\n    (sop/c))\n" in
  let flat text = Lisp.flat (List.hd (get (Syntax.parse text))) in
  assert (flat threaded = nested);
  assert (flat "(-> x (sop/a))" = "(sop/a x)");
  assert (flat "(-> x)" = "x");
  assert (print nested = threaded);
  assert (print threaded = threaded);
  (* two calls stay nested; a note or a flag in the chain keeps it nested *)
  assert (print "(sop/b (sop/a x))" = "(sop/b (sop/a x))\n");
  assert (print "(-> x (sop/a) (sop/b))" = "(sop/b (sop/a x))\n");
  (* a note or a flag on a step stays on the step's line, and the chain still threads *)
  assert (
    print "(-> x (sop/a) ; why\n (sop/b) (sop/c))"
    = "(-> x\n    (sop/a)\n    ; why\n    (sop/b)\n    (sop/c))\n");
  assert (
    print "(-> x (sop/a) ^:bypass (sop/b) (sop/c))"
    = "(-> x\n    (sop/a)\n    ^:bypass (sop/b)\n    (sop/c))\n");
  assert (
    print "(-> ; start\n x (sop/a) (sop/b) (sop/c))"
    = "(->\n    ; start\n    x\n    (sop/a)\n    (sop/b)\n    (sop/c))\n");
  (* only node calls thread; a keyword first argument starts the chain *)
  assert (print "(* (+ (- a 1) 2) 3)" = "(* (+ (- a 1) 2) 3)\n");
  (* a layout is a tree of containers: ui/ calls nest *)
  assert (
    print "(ui/floating (ui/tile (ui/viewport (ref scene))))"
    = "(ui/floating (ui/tile (ui/viewport (ref scene))))\n");
  assert (
    print "(sop/d (sop/c (sop/b (sop/a :k 1))))"
    = "(-> (sop/a :k 1)\n    (sop/b)\n    (sop/c)\n    (sop/d))\n");
  (* a chain inside a binding, and its spans *)
  let text, spans =
    Lisp.print (get (Syntax.parse "(let* [g nope] (sop/c (sop/b (sop/a g :k 1))))"))
  in
  assert (String.length text > 0 && List.length spans > 0);
  List.iter
    (fun (_, (s : Diagnostic.span)) -> assert (s.start >= 0 && s.finish <= String.length text))
    spans;
  (* errors *)
  assert (Result.is_error (Syntax.parse "(->)"));
  assert (Result.is_error (Syntax.parse "(-> x 5)"));
  assert (Result.is_error (Syntax.parse "(-> x g)"))

(* One float spelling: shortest digits that read back, a point, never an exponent. *)
let () =
  List.iter
    (fun (x, text) ->
      if Lisp.float x <> text then
        failwith (Printf.sprintf "float %h printed %s, want %s" x (Lisp.float x) text))
    [
      (1., "1.0");
      (-0.5, "-0.5");
      (1e-14, "0.00000000000001");
      (2e6, "2000000.0");
      (6.1e-17, "0.000000000000000061");
      (1234567.89, "1234567.89");
      (0.1, "0.1");
      (-3.3e-7, "-0.00000033");
      (1e21, "1000000000000000000000.0");
      (Float.nan, "NaN");
      (Float.infinity, "Infinity");
      (Float.neg_infinity, "-Infinity");
    ];
  List.iter
    (fun x ->
      let text = Lisp.float x in
      assert (Syntax.number text && float_of_string text = x);
      assert (print text = text ^ "\n"))
    [ 1e-14; 2e6; 6.1e-17; 0.30000000000000004; 1.7976931348623157e308; 5e-324; -123.456e-9 ];
  (* a hand-written exponent is read and kept *)
  assert (print "[1e-14 2E+06]" = "[1e-14 2E+06]\n")

(* Active scrubs replace printed atoms by ID, keeping comments and repairing
   every ancestor/following span, even with UTF-8 and a change of token width. *)
let () =
  let source = get (Syntax.parse "[1 ; a note\n \"é\" true -3]") in
  let text, spans = Lisp.print source in
  let root = List.hd source in
  let kids = Syntax.children root in
  let number = List.hd kids and string = List.nth kids 1 in
  let a = { number with Syntax.node = Num "1234" }
  and b = { string with Syntax.node = Str "λ\n\"" } in
  let patched, repaired = Option.get (Lisp.patch_atoms (text, spans) [ b; a ]) in
  let expected = { root with Syntax.node = Vec (a :: b :: List.tl (List.tl kids)) } in
  let canonical, expected_spans = Lisp.print [ expected ] in
  assert (patched = canonical && repaired = expected_spans);
  assert (Lisp.patch_atoms (text, spans) [ a; a ] = None);
  assert (Lisp.patch_atoms (text, spans) [ Syntax.make (Num "2") ] = None);
  assert (Lisp.patch_atoms (text, spans) [ { a with node = Num "1e999" } ] = None);
  assert (Lisp.patch_atoms (text, spans) [ { a with meta = [ "bypass" ] } ] = None);
  assert (Lisp.patch_atoms (text, spans) [ root ] = None)

(* Every checked-in workspace: printing is a fixed point and loses no comment. *)
let () =
  let comments text =
    List.filter_map
      (fun line ->
        match String.index_opt line ';' with
        | Some i -> Some (String.trim (String.sub line i (String.length line - i)))
        | None -> None)
      (String.split_on_char '\n' text)
    |> List.sort compare
  in
  let check name source =
    let once = print source in
    let twice = print once in
    if once <> twice then
      failwith (name ^ ": print is not a fixed point: " ^ first_difference once twice);
    let norm c =
      (* [;x] and [;; x] print as [; x] *)
      let k = ref 0 in
      while !k < String.length c && c.[!k] = ';' do
        incr k
      done;
      String.trim (String.sub c !k (String.length c - !k))
    in
    let want = List.sort compare (List.map norm (comments source))
    and got = List.sort compare (List.map norm (comments once)) in
    if want <> got then
      failwith
        (Printf.sprintf "%s: print lost a comment (%d written, %d printed)" name (List.length want)
           (List.length got))
  in
  let dir = "../../sketches" in
  Array.iter
    (fun sketch ->
      let path = Filename.concat dir sketch in
      if Filename.check_suffix path ".rays" then check path (read path))
    (Sys.readdir dir);
  (* comments in every position the printer lays out specially *)
  List.iter
    (fun source -> check source source)
    [
      "(graph g [a ; first\n b] -> geometry (sop/box))";
      "(graph g {:kind :sop} [a ; first\n b] -> geometry (sop/box))";
      "(defn f [x ; the x\n y] -> number (+ x y))";
      "(defmacro m [a ; why\n b] `(+ ~a ~b))";
      "(fn [x ; arg\n ] (+ x 1))";
      "(f ^:bypass ; between\n (sop/box))";
      "(f ' ; quoted\n a)";
      "(-> x ; one\n (sop/a) ; two\n (sop/b) (sop/c) ; end\n )";
      "(let* [[a ; in a pattern\n b] xs] a)";
      "(workspace w ; a\n (graph g [] -> geometry (sop/box)) ; tail\n )\n; after";
    ]
