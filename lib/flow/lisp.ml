open Syntax

type spans = (Syntax.id * Diagnostic.span) list

let width = 84

(* A form's text is wrapped in \001 id \002 ... \003 while it is built, so
   layout code can measure and align by visible width; [print] strips the
   markers and turns them into spans. *)
let wrap x s = Printf.sprintf "\001%d\002%s\003" x.id s

let vis s =
  let count = ref 0 and skipping = ref false in
  String.iter
    (fun c ->
      match c with
      | '\001' -> skipping := true
      | '\002' -> skipping := false
      | '\003' -> ()
      | c when !skipping -> ignore c
      | c -> if Char.code c land 0xC0 <> 0x80 then incr count)
    s;
  !count

let sp n = String.make (max n 0) ' '
let float = Param.float_text ~decimal:true

let number raw =
  if List.mem raw [ "NaN"; "Infinity"; "-Infinity" ] then raw
  else if String.exists (fun c -> c = 'e' || c = 'E') raw then raw
  else if String.contains raw '.' then float (float_of_string raw)
  else raw

let string text =
  let buf = Buffer.create (String.length text + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\001' .. '\003' as c -> Buffer.add_string buf (Printf.sprintf "\\%03d" (Char.code c))
      | c -> Buffer.add_char buf c)
    text;
  Buffer.add_char buf '"';
  Buffer.contents buf

let prefix = function Plain -> "'" | Quasi -> "`" | Unquote -> "~" | Splice -> "~@"
let meta_pre x = String.concat "" (List.map (fun m -> "^:" ^ m ^ " ") x.meta)
let kids x = children x
let is_kw x = match x.node with Kw _ -> true | _ -> false
let head x = Option.value ~default:"" (Syntax.head x)

(* flat: one line, notes ignored; [core_flat] leaves the form's own marker and flags to the caller. *)
let rec core_flat x =
  let join o c = o ^ String.concat " " (List.map flat_ (kids x)) ^ c in
  match x.node with
  | Sym s -> s
  | Kw k -> ":" ^ k
  | Num n -> number n
  | Str s -> string s
  | List _ -> join "(" ")"
  | Vec _ -> join "[" "]"
  | Map _ -> join "{" "}"
  | Quote (k, y) -> prefix k ^ flat_ y

and flat_ x = wrap x (meta_pre x ^ core_flat x)

(* A comment block, each line followed by a break and indent to [col]. *)
let nl lines col =
  String.concat "" (List.map (fun l -> (if l = "" then ";" else "; " ^ l) ^ "\n" ^ sp col) lines)

let note_at y col = nl y.notes col
let with_notes y col s = note_at y col ^ s
let own x = x.tail <> [] || List.exists (fun y -> y.notes <> []) (kids x)
let rec deep x = own x || List.exists deep (kids x)
let end_n x col = if x.tail = [] then "" else "\n" ^ sp col ^ nl x.tail col
let nth = List.nth
let is_vec x = match x.node with Vec _ -> true | _ -> false
let is_map x = match x.node with Map _ -> true | _ -> false
let bind_forms = [ "let*"; "for"; "sum" ]

(* A call of a node kind (a head with a "/") whose first operand is a call of its own, as far down
   as it goes: [(g (f x a) b)] prints [(-> x (f a) (g b))] from three calls up.  A step keeps
   its notes, flags and tail on its own line.  Returns the start and the steps (outermost last,
   each without its first operand). *)
let threadable x =
  match x.node with
  | List ({ node = Sym h; _ } :: first :: _) ->
      (* a layout is a tree of containers, not a pipeline: [ui/] calls nest *)
      String.contains h '/' && (not (String.starts_with ~prefix:"ui/" h)) && not (is_kw first)
  | _ -> false

let rec spine x =
  match x.node with
  | List (h :: first :: args) when threadable x ->
      let start, steps = spine first in
      (start, { x with node = List (h :: args) } :: steps)
  | _ -> (x, [])

let spine x =
  let start, steps = spine x in
  (start, List.rev steps)

(* more atoms than columns: the one-line text cannot fit, so it is not built to find out *)
let wide x =
  let left = ref width in
  let rec go x =
    decr left;
    !left >= 0 && List.for_all go (kids x)
  in
  not (go x)

let rec pp x ind =
  let pre = meta_pre x in
  wrap x (pre ^ core x (ind + String.length pre))

and core x ind =
  let flat = lazy (core_flat x) in
  let narrow = lazy (not (wide x)) in
  let fits ind limit = Lazy.force narrow && vis (Lazy.force flat) + ind <= limit in
  let ks = kids x in
  let len = List.length ks in
  let deep_ = deep in
  let deep = deep x in
  let end_n = end_n x in
  let no_notes ys = List.for_all (fun y -> y.notes = []) ys in
  (* a child on its own line at [col], after its notes *)
  let line k col = "\n" ^ sp col ^ with_notes (nth ks k) col (pp (nth ks k) col) in
  match x.node with
  | Sym _ | Kw _ | Num _ | Str _ -> Lazy.force flat
  | Quote (k, y) -> prefix k ^ pp y (ind + String.length (prefix k))
  | Vec _ | Map _ ->
      if (not deep) && (is_vec x || fits ind width) then Lazy.force flat
      else
        let col = ind + 1 in
        let rec parts = function
          | [] -> []
          | key :: value :: rest when is_map x ->
              let k = pp key col in
              (note_at key col ^ k
              ^
              if value.notes <> [] then
                "\n" ^ sp (col + 2) ^ note_at value (col + 2) ^ pp value (col + 2)
              else " " ^ pp value (col + vis k + 1))
              :: parts rest
          | y :: rest -> (note_at y col ^ pp y col) :: parts rest
        in
        let o, c = if is_vec x then ("[", "]") else ("{", "}") in
        o
        ^ (match ks with y :: _ when y.notes <> [] -> "\n" ^ sp col | _ -> "")
        ^ String.concat ("\n" ^ sp col) (parts ks)
        ^ end_n col ^ c
  | List [] -> Lazy.force flat
  | List (h :: _) ->
      let hd = head x in
      let bind_vec v col =
        let rec rows = function
          | [] -> []
          | name :: rest ->
              let n = if deep_ name then pp name (col + 1) else flat_ name in
              let pad = col + 1 + vis n + 1 in
              let value, rest =
                match rest with
                | [] -> ("", [])
                | v :: rest ->
                    ( (if v.notes <> [] then
                         "\n" ^ sp (col + 3) ^ note_at v (col + 3) ^ pp v (col + 3)
                       else " " ^ pp v pad),
                      rest )
              in
              (note_at name (col + 1) ^ n ^ value) :: rows rest
        in
        let items = kids v in
        wrap v
          ("["
          ^ (match items with y :: _ when y.notes <> [] -> "\n" ^ sp (col + 1) | _ -> "")
          ^ String.concat ("\n" ^ sp (col + 1)) (rows items)
          ^ (if v.tail = [] then "" else "\n" ^ sp (col + 1) ^ nl v.tail (col + 1))
          ^ "]")
      in
      let vec_at k = k < len && is_vec (nth ks k) in
      let vec_len k = match (nth ks k).node with Vec v -> List.length v | _ -> 0 in
      let tail k col = line k col in
      let slots_plain n = no_notes (List.take (n - 1) (List.drop 1 ks)) in
      (* slots printed flat: no comment on or inside them *)
      let slots_flat n =
        slots_plain n && not (List.exists deep_ (List.take (n - 1) (List.drop 1 ks)))
      in
      let special = List.mem hd [ "graph"; "defn"; "defmacro" ] in
      let start, steps = spine x in
      if List.length steps >= 3 then begin
        let col = ind + 4 and last = List.length steps - 1 in
        (if start.notes = [] then "(-> " else "(->\n" ^ sp col ^ note_at start col)
        ^ pp start col
        ^ String.concat ""
            (List.mapi
               (fun i s ->
                 "\n" ^ sp col ^ if i = last then core s col else note_at s col ^ pp s col)
               steps)
        ^ ")"
      end
      else if hd = "workspace" && len >= 2 && slots_plain 2 then
        "(workspace "
        ^ flat_ (nth ks 1)
        ^ String.concat ""
            (List.map
               (fun y -> "\n\n" ^ sp (ind + 2) ^ note_at y (ind + 2) ^ pp y (ind + 2))
               (List.drop 2 ks))
        ^ end_n (ind + 2)
        ^ ")"
      else if
        (not deep)
        && (fits ind width
           || (fits 0 44 && (not (List.mem hd bind_forms)) && hd <> "fold" && hd <> "scan"))
        && (not special)
        && not (hd = "let*" && len > 1 && vec_at 1 && vec_len 1 > 2)
      then Lazy.force flat
      else if List.mem hd bind_forms && vec_at 1 && len = 3 && slots_plain 2 then
        let col = ind + String.length hd + 2 in
        "(" ^ hd ^ " " ^ bind_vec (nth ks 1) col ^ tail 2 (ind + 2) ^ end_n (ind + 2) ^ ")"
      else if hd = "for" && vec_at 1 && len = 5 && is_kw (nth ks 2) && slots_plain 4 then
        (* [:skip tuples] stays with the bindings, the body below *)
        let col = ind + String.length hd + 2 in
        "(for "
        ^ bind_vec (nth ks 1) col
        ^ "\n"
        ^ sp (ind + 2)
        ^ flat_ (nth ks 2)
        ^ " "
        ^ pp (nth ks 3) (ind + 2 + vis (flat_ (nth ks 2)) + 1)
        ^ tail 4 (ind + 2)
        ^ end_n (ind + 2)
        ^ ")"
      else if (hd = "fold" || hd = "scan") && vec_at 1 && vec_at 2 && len = 4 && slots_plain 3 then
        let col = ind + String.length hd + 2 in
        "(" ^ hd ^ " "
        ^ bind_vec (nth ks 1) col
        ^ "\n" ^ sp col
        ^ bind_vec (nth ks 2) col
        ^ tail 3 (ind + 2)
        ^ end_n (ind + 2)
        ^ ")"
      else if hd = "if" && len = 4 && no_notes [ nth ks 0 ] then
        let c = ind + 4 in
        "(if"
        ^ (if (nth ks 1).notes <> [] then "\n" ^ sp c ^ note_at (nth ks 1) c else " ")
        ^ pp (nth ks 1) c
        ^ tail 2 c ^ tail 3 c ^ end_n c ^ ")"
      else if
        (hd = "graph" || hd = "defn") && (len = 5 || (len = 6 && vec_at 4)) && slots_flat (len - 1)
      then
        "(" ^ hd ^ " "
        ^ flat_ (nth ks 1)
        ^ " "
        ^ flat_ (nth ks 2)
        ^ " "
        ^ flat_ (nth ks 3)
        ^ (if len = 6 then " " ^ flat_ (nth ks 4) else "")
        ^ tail (len - 1) (ind + 2)
        ^ end_n (ind + 2)
        ^ ")"
      else if hd = "defmacro" && len = 4 && slots_flat 3 then
        "(defmacro "
        ^ flat_ (nth ks 1)
        ^ " "
        ^ flat_ (nth ks 2)
        ^ tail 3 (ind + 2)
        ^ end_n (ind + 2)
        ^ ")"
      else if hd = "fn" && len = 3 && vec_at 1 && slots_flat 2 then
        "(fn " ^ flat_ (nth ks 1) ^ tail 2 (ind + 2) ^ end_n (ind + 2) ^ ")"
      else if ((hd = "cond" && len >= 1) || (hd = "case" && len >= 2)) && no_notes [ nth ks 0 ] then
        let col = ind + 2 in
        let first = if hd = "case" then 2 else 1 in
        let rec rows = function
          | [] -> []
          | a :: rest ->
              let s = pp a col in
              let value, rest =
                match rest with
                | [] -> ("", [])
                | v :: rest ->
                    ( (if v.notes <> [] || String.contains s '\n' then
                         "\n" ^ sp (col + 2) ^ note_at v (col + 2) ^ pp v (col + 2)
                       else " " ^ pp v (col + vis s + 1)),
                      rest )
              in
              (note_at a col ^ s ^ value) :: rows rest
        in
        let opening =
          if hd = "case" then
            let s = nth ks 1 in
            "(case"
            ^ (if s.notes <> [] then "\n" ^ sp col ^ note_at s col else " ")
            ^ pp s (if s.notes <> [] then col else ind + 6)
          else "(cond"
        in
        opening
        ^ String.concat "" (List.map (fun r -> "\n" ^ sp col ^ r) (rows (List.drop first ks)))
        ^ end_n col ^ ")"
      else begin
        (* a call: the first positional argument on the head line, keyword
           pairs aligned below *)
        let rec units = function
          | [] -> []
          | k :: v :: rest when is_kw k -> [ k; v ] :: units rest
          | y :: rest -> [ y ] :: units rest
        in
        let units = units (List.tl ks) in
        let hs = flat_ h in
        if units = [] && x.tail = [] then Lazy.force flat
        else if own x then
          let col = ind + 2 in
          let unit = function
            | [ k; v ] ->
                note_at k col ^ flat_ k
                ^
                if v.notes <> [] then "\n" ^ sp (col + 2) ^ note_at v (col + 2) ^ pp v (col + 2)
                else " " ^ pp v (col + vis (flat_ k) + 1)
            | u ->
                let y = List.hd u in
                note_at y col ^ pp y col
          in
          "("
          ^ (if h.notes <> [] then note_at h (ind + 1) else "")
          ^ hs
          ^ String.concat "" (List.map (fun u -> "\n" ^ sp col ^ unit u) units)
          ^ end_n col ^ ")"
        else
          let col = ind + 1 + vis hs + 1 in
          let col = if col > ind + 18 then ind + 4 else col in
          let unit = function
            | [ k; v ] -> flat_ k ^ " " ^ pp v (col + vis (flat_ k) + 1)
            | u -> pp (List.hd u) col
          in
          let first = unit (List.hd units) in
          "(" ^ hs
          ^ (if col = ind + 4 && List.length units > 1 && vis first > 30 then "\n" ^ sp col else " ")
          ^ String.concat ("\n" ^ sp col) (List.map unit units)
          ^ ")"
      end

(* Strip the markers, collecting spans. *)
let finish text =
  let buf = Buffer.create (String.length text) in
  let stack = ref [] and spans = ref [] in
  let i = ref 0 and n = String.length text in
  while !i < n do
    (match text.[!i] with
    | '\001' ->
        let j = String.index_from text !i '\002' in
        stack :=
          (int_of_string (String.sub text (!i + 1) (j - !i - 1)), Buffer.length buf) :: !stack;
        i := j
    | '\003' -> (
        match !stack with
        | (id, start) :: rest ->
            spans := (id, Diagnostic.{ start; finish = Buffer.length buf }) :: !spans;
            stack := rest
        | [] -> ())
    | c -> Buffer.add_char buf c);
    incr i
  done;
  (Buffer.contents buf, List.sort (fun (a, _) (b, _) -> compare a b) !spans)

let print forms =
  Phase_timer.measure "print" (fun () ->
      let text = String.concat "\n\n" (List.map (fun x -> note_at x 0 ^ pp x 0) forms) ^ "\n" in
      finish text)

let flat x = fst (finish (flat_ x))

(* An active token scrub keeps the printer's line breaks until release. *)
let patch_atoms (text, spans) values =
  let by_id = Hashtbl.create (List.length spans) in
  List.iter (fun (id, span) -> Hashtbl.replace by_id id span) spans;
  let edits =
    List.map
      (fun (value : Syntax.t) ->
        let atom =
          match value.node with
          | Num n -> Syntax.number n && Option.is_some (Syntax.number_value n)
          | Str _ | Sym ("true" | "false") -> true
          | _ -> false
        in
        match (atom, value.meta, Hashtbl.find_opt by_id value.id) with
        | true, [], Some span when value.id <> 0 -> Some (span, flat value)
        | _ -> None)
      values
  in
  if List.exists Option.is_none edits then None
  else
    let edits = Array.of_list (List.map Option.get edits) in
    Array.sort (fun (a, _) (b, _) -> Int.compare a.Diagnostic.start b.start) edits;
    let valid = ref true and at = ref 0 and delta = ref 0 in
    let shifts =
      Array.map
        (fun ((span : Diagnostic.span), value) ->
          if span.start < !at || span.finish <= span.start || span.finish > String.length text then
            valid := false;
          at := span.finish;
          delta := !delta + String.length value - (span.finish - span.start);
          (span.finish, !delta))
        edits
    in
    if not !valid then None
    else
      let out = Buffer.create (max 0 (String.length text + !delta)) in
      at := 0;
      Array.iter
        (fun ((span : Diagnostic.span), value) ->
          Buffer.add_substring out text !at (span.start - !at);
          Buffer.add_string out value;
          at := span.finish)
        edits;
      Buffer.add_substring out text !at (String.length text - !at);
      let shift position =
        let lo = ref 0 and hi = ref (Array.length shifts) in
        while !lo < !hi do
          let mid = (!lo + !hi) / 2 in
          if fst shifts.(mid) <= position then lo := mid + 1 else hi := mid
        done;
        position + if !lo = 0 then 0 else snd shifts.(!lo - 1)
      in
      Some
        ( Buffer.contents out,
          List.map
            (fun (id, (span : Diagnostic.span)) ->
              (id, Diagnostic.{ start = shift span.start; finish = shift span.finish }))
            spans )
