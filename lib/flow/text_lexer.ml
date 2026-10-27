type word = Symbol of string | Keyword of string | Number of string
type kind = Open of char | Close of char | Prefix of char | Meta of string
  | Comment of string | Word of word | String of string | End
type issue = { start : int; finish : int; code : string; message : string }
type token = { kind : kind; start : int; stop : int; issue : issue option }

let separator = function
  | ' ' | '\t' | '\r' | '\n' | '(' | ')' | '[' | ']' | '{' | '}' | ';' | '"' -> true
  | _ -> false

let number text =
  let nonfinite = text = "NaN" || text = "Infinity" || text = "-Infinity" in
  nonfinite ||
  let n = String.length text in
  let digit c = c >= '0' && c <= '9' in
  let i = ref (if n > 0 && text.[0] = '-' then 1 else 0) in
  let before = ref 0 and after = ref 0 in
  while !i < n && digit text.[!i] do incr before; incr i done;
  if !i < n && text.[!i] = '.' then begin
    incr i;
    while !i < n && digit text.[!i] do incr after; incr i done
  end;
  let mantissa = !before + !after > 0 in
  if mantissa && !i < n && (text.[!i] = 'e' || text.[!i] = 'E') then begin
    incr i;
    if !i < n && (text.[!i] = '+' || text.[!i] = '-') then incr i;
    let exponent = !i in
    while !i < n && digit text.[!i] do incr i done;
    !i = n && !i > exponent
  end else !i = n && mantissa

let lex source =
  let n = String.length source in
  let token kind start stop issue = { kind; start; stop; issue } in
  let ending start = token End start start None in
      let word_end from =
        let j = ref from in
        while !j < n && not (separator source.[!j]) do incr j done;
        !j in
      let string start =
        let buffer = Buffer.create 32 in
        let issue = ref None in
        let note value = if !issue = None then issue := Some value in
        let rec read j =
          if j >= n then begin
            note {start;finish=n;code="E_UNCLOSED";message="This string is never closed"};
            n
          end else match source.[j] with
          | '"' -> j + 1
          | '\\' when j + 1 >= n ->
              note {start;finish=n;code="E_UNCLOSED";message="This string is never closed"}; n
          | '\\' ->
              let add c = Buffer.add_char buffer c; read (j + 2) in
              (match source.[j + 1] with
               | '"' -> add '"' | '\\' -> add '\\' | 'n' -> add '\n'
               | 't' -> add '\t' | 'r' -> add '\r'
               | '0'..'9' when j+3<n && source.[j+2]>='0' && source.[j+2]<='9'
                   && source.[j+3]>='0' && source.[j+3]<='9' ->
                   let byte=int_of_string(String.sub source (j+1) 3) in
                   if byte > 255 then begin
                     note {start=j;finish=j+4;code="E_UNEXPECTED";
                       message="A decimal byte escape must be 000 to 255."};
                     read (j + 4)
                   end else begin Buffer.add_char buffer (Char.chr byte); read (j + 4) end
               | _ ->
                   note {start=j;finish=j+2;code="E_UNEXPECTED";message="Unknown string escape"};
                   read (j + 2))
          | c -> Buffer.add_char buffer c; read (j + 1) in
        let stop = read (start + 1) in
        token (String (Buffer.contents buffer)) start stop !issue, stop in
      let rec scan i reversed =
        let add kind start stop reversed = token kind start stop None :: reversed in
        if i >= n then List.rev (ending n :: reversed)
        else match source.[i] with
        | ' ' | '\t' | '\r' | '\n' -> scan (i + 1) reversed
        | ';' ->
            let finish = match String.index_from_opt source i '\n' with Some j -> j | None -> n in
            let content = ref i in
            while !content < finish && source.[!content] = ';' do incr content done;
            if !content < finish && source.[!content] = ' ' then incr content;
            let text = String.trim (String.sub source !content (finish - !content)) in
            scan finish (add (Comment text) i finish reversed)
        | ('(' | '[' | '{') as c -> scan (i + 1) (add (Open c) i (i + 1) reversed)
        | (')' | ']' | '}') as c -> scan (i + 1) (add (Close c) i (i + 1) reversed)
        | ('\'' | '`') as c -> scan (i + 1) (add (Prefix c) i (i + 1) reversed)
        | '~' when i + 1 < n && source.[i + 1] = '@' ->
            scan (i + 2) (add (Prefix '@') i (i + 2) reversed)
        | '~' -> scan (i + 1) (add (Prefix '~') i (i + 1) reversed)
        | '^' ->
            let finish = word_end (i + 1) in
            let valid = finish > i + 2 && source.[i + 1] = ':' in
            let issue = if valid then None else Some {start=i;finish=max finish (i+1);code="E_UNEXPECTED";
              message="^ is followed by a keyword, as in ^:bypass"} in
            let name = if valid then String.sub source (i + 2) (finish - i - 2) else "" in
            scan finish (token (Meta name) i finish issue :: reversed)
        | '"' ->
            let value, finish = string i in
            scan finish (value :: reversed)
        | _ ->
            let finish = word_end i in
            let text = String.sub source i (finish - i) in
            let word = if String.length text > 1 && text.[0] = ':' then Keyword (String.sub text 1 (String.length text - 1))
              else if number text then Number text else Symbol text in
            scan finish (add (Word word) i finish reversed) in
      let tokens = Array.of_list (scan 0 []) in
      (* Controls are invalid even inside comments and strings. Keep them as issue-only End
         tokens: strict parsing sees them, while tolerant editor coloring ignores them. *)
      let controls = ref [] in
      String.iteri (fun at c -> if c >= '\001' && c <= '\003' then
        controls := token End at (at + 1) (Some {start=at;finish=at+1;code="E_UNEXPECTED";
          message=Printf.sprintf "Control byte %d is not allowed in a workspace" (Char.code c)}) :: !controls) source;
      if !controls = [] then tokens else
        Array.of_list (List.stable_sort (fun a b -> compare a.start b.start)
          (Array.to_list tokens @ !controls))
