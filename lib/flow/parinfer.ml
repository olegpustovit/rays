let closer_of = function '(' -> ')' | '[' -> ']' | _ -> '}'

(* Parinfer's indent mode: the closing brackets at the end of each line (its paren trail) are
   inferred from the indentation of the code lines that follow, so a form holds exactly the
   lines indented past its opener, and a closer that matches nothing is dropped.  Strings (over
   as many lines as they run) and comments are left alone; on the caret's line the trail starts no earlier than the caret, so
   typing before a closer is never undone.  [parinfer text caret] returns the text and where
   [caret] lands in it.  ponytail: the text is scanned once per edited frame. *)
let rewrite text caret =
  let n = String.length text in
  let is_closer c = c = ')' || c = ']' || c = '}' in
  let is_space c = c = ' ' || c = '\t' || c = '\r' in
  (* the lines: (start, stop) byte ranges without the break *)
  let lines =
    let rec go start acc =
      let stop = match String.index_from_opt text start '\n' with Some j -> j | None -> n in
      let acc = (start, stop) :: acc in
      if stop >= n then List.rev acc else go (stop + 1) acc
    in
    Array.of_list (go 0 [])
  in
  let count = Array.length lines in
  let code_end = Array.make count 0 in
  (* where a line's inferred closers go *)
  let trail = Array.make count "" in
  (* the closers inferred for each line *)
  let removed = ref [] in
  (* bytes dropped: the old trails and the closers matching nothing *)
  let stack = ref [] in
  (* the open brackets: closer and column, innermost first *)
  let last_code = ref (-1) in
  (* the last line holding code *)
  let append line closer = trail.(line) <- trail.(line) ^ String.make 1 closer in
  let in_string = ref false in
  (* a string stays open over line breaks *)
  Array.iteri
    (fun index (start, stop) ->
      (* which bytes are code: not in a string, not in the comment *)
      let code = Array.make (stop - start) true in
      let opened_in_string = !in_string in
      let comment = ref stop and i = ref start in
      while !i < !comment do
        let c = text.[!i] in
        if !in_string then begin
          code.(!i - start) <- false;
          if c = '\\' && !i + 1 < stop then (
            code.(!i + 1 - start) <- false;
            incr i)
          else if c = '"' then in_string := false
        end
        else if c = '"' then (
          in_string := true;
          code.(!i - start) <- false)
        else if c = ';' then comment := !i;
        incr i
      done;
      let is_code i = i < !comment && code.(i - start) in
      (* the paren trail: the closers ending the code, with the blanks among them *)
      let te = ref !comment in
      while !te > start && is_code (!te - 1) && is_space text.[!te - 1] do
        decr te
      done;
      let ts = ref !te in
      while
        !ts > start && is_code (!ts - 1) && (is_closer text.[!ts - 1] || is_space text.[!ts - 1])
      do
        decr ts
      done;
      code_end.(index) <- !ts;
      (* on the caret's line the trail starts no earlier than the caret *)
      let ts = if caret > !ts && caret < !te then caret else !ts in
      let trail_here = ref false in
      for i = ts to !te - 1 do
        if is_closer text.[i] then trail_here := true
      done;
      let ts, te = if !trail_here then (ts, !te) else (!te, !te) in
      if !trail_here && ts > code_end.(index) then code_end.(index) <- ts;
      (* a line whose first code byte lies before the trail is a code line: its indentation
       closes every bracket opened at that column or further right *)
      let first = ref start in
      while !first < ts && is_space text.[!first] do
        incr first
      done;
      (* a line that begins inside a string is its text: its indentation closes nothing *)
      let is_code_line = !first < ts && !first < !comment && not opened_in_string in
      if is_code_line then begin
        let x = !first - start in
        let rec pop () =
          match !stack with
          | (closer, column) :: rest when column >= x ->
              stack := rest;
              if !last_code >= 0 then append !last_code closer;
              pop ()
          | _ -> ()
        in
        pop ()
      end;
      for i = start to ts - 1 do
        if is_code i then begin
          let c = text.[i] in
          if c = '(' || c = '[' || c = '{' then stack := (closer_of c, i - start) :: !stack
          else if is_closer c then begin
            match !stack with
            | (closer, _) :: rest when closer = c -> stack := rest
            | _ -> removed := i :: !removed
          end
        end
      done;
      for i = ts to te - 1 do
        removed := i :: !removed
      done;
      (* closers go after the last thing written, never into a string still open at the line's end *)
      if !first < ts && !first < !comment && not !in_string then last_code := index)
    lines;
  List.iter (fun (closer, _) -> if !last_code >= 0 then append !last_code closer) !stack;
  (* the text again: the kept bytes, each line's closers at its code end *)
  let removed = List.sort_uniq compare !removed in
  let out = Buffer.create (n + 8) in
  let shift = ref 0 in
  (* bytes inserted before the caret, minus bytes removed before it *)
  let skip = ref removed in
  Array.iteri
    (fun index (start, stop) ->
      for i = start to stop do
        if i = code_end.(index) && trail.(index) <> "" then begin
          Buffer.add_string out trail.(index);
          if i < caret then shift := !shift + String.length trail.(index)
        end;
        if i < stop || (i = n && stop = n && i < n) then begin
          match !skip with
          | r :: rest when r = i ->
              skip := rest;
              if i < caret then decr shift
          | _ -> Buffer.add_char out text.[i]
        end
        else if i = stop && i < n then Buffer.add_char out '\n'
      done)
    lines;
  (Buffer.contents out, caret + !shift)
