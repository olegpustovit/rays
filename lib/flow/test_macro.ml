open Flow

let parse source =
  match Syntax.parse source with Ok forms -> forms | Error d -> failwith (Diagnostic.to_string d)

let one source = List.hd (parse source)

(* [count] and [range] are operators a binding could be named after: free only as a head *)
let known ~head s =
  List.mem s [ "+"; "*"; "/"; "-"; "sop/merge"; "sop/transform"; "sop/box"; "for"; "let*" ]
  || (head && List.mem s [ "range"; "count" ])

let check source = Macro.check ~known (one source)
let flat = Lisp.flat

let contains text (d : Diagnostic.t) =
  let n = String.length text and m = String.length d.message in
  let rec at i = i + n <= m && (String.sub d.message i n = text || at (i + 1)) in
  at 0

let radial =
  "(defmacro radial [i n body] `(sop/merge (for [~i (range ~n)] (sop/transform ~body :rotate [0 (* \
   (/ ~i ~n) 6.2832) 0]))))"

let add1 = "(defmacro add1 [a] `(let* [t# ~a] (+ t# 1)))"

let expand ms call =
  match Macro.expand (parse ms) (one call) with
  | Ok x -> flat x
  | Error d -> failwith (Diagnostic.to_string d)

(* a definition the checker refuses *)
let rejects source code text =
  match check source with
  | [ d ] when d.code = code && contains text d -> ()
  | ds -> failwith (source ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds))

(* a call the expander refuses *)
let refuses ms call code text =
  match Macro.expand (parse ms) (one call) with
  | Error d when d.code = code && contains text d -> ()
  | Error d -> failwith (Diagnostic.to_string d)
  | Ok x -> failwith ("accepted: " ^ flat x)

let () =
  (* hygiene: a caller's names arrive only through holes *)
  assert (check radial = []);
  assert (
    expand radial "(radial k 3 (sop/box))"
    = "(sop/merge (for [k (range 3)] (sop/transform (sop/box) :rotate [0 (* (/ k 3) 6.2832) 0])))");
  assert (
    expand radial "(radial j 2 (sop/box :size (+ j 1)))"
    = "(sop/merge (for [j (range 2)] (sop/transform (sop/box :size (+ j 1)) :rotate [0 (* (/ j 2) \
       6.2832) 0])))");
  rejects "(defmacro m [x] `(+ ~x y))" "E_MACRO_CAPTURE" "would capture a name from the call site";
  rejects "(defmacro m [x] `(let* [tmp ~x] tmp))" "E_MACRO_CAPTURE" "write tmp# for a fresh name";
  assert (check "(defmacro m [x] `(let* [tmp# ~x] tmp#))" = []);
  rejects "(defmacro m [x] `(+ ~x k.y))" "E_MACRO_CAPTURE" "k.y";
  rejects "(defmacro m [x] `(+ ~x 1#x#))" "E_MACRO_TEMPLATE" "not a valid fresh name"

let () =
  (* unquoting *)
  rejects "(defmacro m [x] `(+ ~x ~(+ 1 2)))" "E_MACRO_UNQUOTE"
    "macros unquote only their parameters";
  rejects "(defmacro m [x] `(+ ~x ~z))" "E_MACRO_UNQUOTE" "not one";
  rejects "(defmacro m [x & xs] `(+ ~x ~xs))" "E_MACRO_UNQUOTE"
    "splice the rest parameter with ~@xs";
  rejects "(defmacro m [x & xs] `(+ ~@x ~@xs))" "E_MACRO_UNQUOTE" "splices the rest parameter only";
  rejects "(defmacro m [x] `(+ ~@x))" "E_MACRO_UNQUOTE" "~@";
  rejects "(defmacro m [x] `(+ ~x `1))" "E_MACRO_TEMPLATE" "nested quoting";
  rejects "(defmacro m [x x] `(+ ~x 1))" "E_MACRO_PARAM" "repeated parameter";
  rejects "(defmacro m [& a b] `(+ 1 1))" "E_MACRO_PARAM" "rest parameter";
  rejects "(defmacro m [] )" "E_MACRO_SHAPE" "defmacro name";
  assert (expand "(defmacro all [& xs] `(+ 0 (+ ~@xs)))" "(all 1 2)" = "(+ 0 (+ 1 2))");
  assert (expand "(defmacro all [& xs] `(+ 0 (+ ~@xs)))" "(all)" = "(+ 0 (+))")

let () =
  (* fresh names are deterministic and distinct *)
  let call = "(+ (add1 1) (add1 (add1 2)))" in
  let a = expand add1 call in
  assert (a = "(+ (let* [t__1 1] (+ t__1 1)) (let* [t__2 (let* [t__3 2] (+ t__3 1))] (+ t__2 1)))");
  assert (a = expand add1 call);
  (* t# is t__1: never the reserved name t *)
  assert ((not (String.length a = 0)) && not (List.mem "t" (String.split_on_char ' ' a)));
  (* the counter starts at 1 for each call and is shared by the nested steps *)
  assert (expand add1 "(add1 5)" = "(let* [t__1 5] (+ t__1 1))");
  (* a stepper with one state reaches the same text as expand *)
  let ms = parse (radial ^ add1) in
  let state = Macro.state () in
  let step x =
    match Macro.expand_once ~state ms x with
    | Ok y -> y
    | Error d -> failwith (Diagnostic.to_string d)
  in
  let x = one "(radial k 2 (add1 1))" in
  let s1 = step x in
  assert (
    flat s1
    = "(sop/merge (for [k (range 2)] (sop/transform (add1 1) :rotate [0 (* (/ k 2) 6.2832) 0])))");
  let s2 = step s1 in
  assert (step s2 == s2);
  assert (flat s2 = expand (radial ^ add1) "(radial k 2 (add1 1))");
  (* the new forms take the span of the call *)
  let call = List.hd (parse "\n\n(add1 5)") in
  match Macro.expand (parse add1) call with
  | Ok { Syntax.span; _ } -> assert (span = call.span)
  | Error _ -> assert false

let () =
  (* limits *)
  assert (Macro.max_depth = 32 && Macro.max_forms = 5000);
  refuses "(defmacro m [x] `(m ~x))" "(m 2)" "E_MACRO_DEPTH" "32 nested expansions";
  let nested = String.concat "" (List.init 12 (fun _ -> "(d ")) ^ "1" ^ String.make 12 ')' in
  refuses "(defmacro d [x] `(+ ~x ~x))" nested "E_MACRO_SIZE" "5,000 forms";
  (* below the limit is fine *)
  ignore
    (expand "(defmacro d [x] `(+ ~x ~x))"
       (String.concat "" (List.init 5 (fun _ -> "(d ")) ^ "1" ^ String.make 5 ')'));
  refuses radial "(radial k 2)" "E_MACRO_ARITY" "expects 3 arguments; got 2";
  refuses "(defmacro m [x & r] `(+ ~x ~@r))" "(m)" "E_MACRO_ARITY" "at least 1 argument; got 0"

let () =
  (* a template is quoted: there is no unquoted form *)
  rejects "(defmacro twice [x] (+ x x))" "E_MACRO_TEMPLATE" "the template is quoted";
  (* an operator a caller can bind is free as a head and a capture as an argument *)
  assert (check "(defmacro n [xs] `(count ~xs))" = []);
  rejects "(defmacro n [x] `(+ count ~x))" "E_MACRO_CAPTURE" "count would capture";
  (* a macro call is left alone inside quotes and defmacro forms *)
  assert (expand add1 "(defmacro other [a] (+ a a))" = "(defmacro other [a] (+ a a))")

let () =
  (* macro_params *)
  (match Macro.params (one "(defmacro m [a b & rest] `(+ ~a ~b))") with
  | Ok (req, Some "rest") -> assert (req = [ "a"; "b" ])
  | _ -> assert false);
  (match Macro.params (one "(defmacro m [a b] `(+ ~a ~b))") with
  | Ok ([ "a"; "b" ], None) -> ()
  | _ -> assert false);
  assert (
    Macro.valid_name "sort-by" && Macro.valid_name "a1_b"
    && (not (Macro.valid_name "A"))
    && not (Macro.valid_name "1a"))

let () = print_endline "Flow macros: hygiene, unquoting, fresh names, stepping and limits pass"
