type atom = Literal of char | Any | Star | Class of { bits : bytes; negated : bool }
type term = { excluded : bool; atoms : atom array }
type t = { initially_selected : bool; terms : term array }
type capture_kind = Capture_star | Capture_any of int | Capture_classes of atom array
type capture_group = { first_atom : int; atom_count : int; kind : capture_kind }
type replacement_atom = Replacement_literal of char | Replacement_capture of int

type rewrite = {
  match_atoms : atom array;
  capture_of_atom : int array;
  capture_groups : capture_group array;
  replacement_atoms : replacement_atom array;
}

type rewrite_set = rewrite array

let whitespace = function ' ' | '\t' | '\n' | '\r' | '\012' -> true | _ -> false

let split_terms source =
  let length = String.length source and terms = ref [] and failure = ref None in
  let index = ref 0 in
  while !index < length && !failure = None do
    while !index < length && whitespace source.[!index] do
      incr index
    done;
    if !index < length then begin
      let buffer = Buffer.create 16 and escaped = ref false in
      while !index < length && (!escaped || not (whitespace source.[!index])) do
        let character = source.[!index] in
        Buffer.add_char buffer character;
        incr index;
        if !escaped then escaped := false else if character = '\\' then escaped := true
      done;
      if !escaped then failure := Some "dangling backslash escape"
      else terms := Buffer.contents buffer :: !terms
    end
  done;
  match !failure with
  | Some message -> Error ("Attribute_pattern: " ^ message)
  | None -> Ok (Array.of_list (List.rev !terms))

let escaped_character token index limit =
  if index >= limit then Error "missing class character"
  else
    (* Bind the character before building the pair: with the bounds-checked read written inside the
       constructor, ocamlopt 5.3.0 (arm64) intermittently stored a clobbered byte after a minor GC, which put
       a class member at the wrong bit (a flaky glob such as "[fa]*" missing "f"). *)
    let character = token.[index] in
    if character = '\\' then
      if index + 1 >= limit then Error "dangling class escape"
      else
        let escaped = token.[index + 1] in
        Ok (escaped, index + 2)
    else Ok (character, index + 1)

let class_atom token first =
  let length = String.length token
  and closing = ref (-1)
  and index = ref first
  and escaped = ref false in
  while !index < length && !closing < 0 do
    let character = token.[!index] in
    if !escaped then escaped := false
    else if character = '\\' then escaped := true
    else if character = ']' then closing := !index;
    incr index
  done;
  if !closing < 0 then Error "unterminated character class"
  else if !closing = first then Error "empty character class"
  else
    let negated, content_first =
      if token.[first] = '!' || token.[first] = '^' then (true, first + 1) else (false, first)
    in
    if content_first = !closing then Error "empty negated character class"
    else
      let bits = Bytes.make 32 '\000' and cursor = ref content_first and failure = ref None in
      while !cursor < !closing && !failure = None do
        match escaped_character token !cursor !closing with
        | Error message -> failure := Some message
        | Ok (left, after_left) ->
            if after_left < !closing && token.[after_left] = '-' && after_left + 1 < !closing then
              match escaped_character token (after_left + 1) !closing with
              | Error message -> failure := Some message
              | Ok (right, after_right) ->
                  if Char.code left > Char.code right then
                    failure := Some "descending character-class range"
                  else begin
                    for code = Char.code left to Char.code right do
                      Support.Bits.set bits code
                    done;
                    cursor := after_right
                  end
            else begin
              Support.Bits.set bits (Char.code left);
              cursor := after_left
            end
      done;
      match !failure with
      | Some message -> Error message
      | None -> Ok (Class { bits; negated }, !closing + 1)

let compile_term raw =
  let excluded, first = if String.length raw > 0 && raw.[0] = '^' then (true, 1) else (false, 0) in
  if first = String.length raw then Error "empty exclusion term"
  else
    let atoms = ref [] and index = ref first and failure = ref None in
    while !index < String.length raw && !failure = None do
      match raw.[!index] with
      | '\\' ->
          if !index + 1 = String.length raw then failure := Some "dangling backslash escape"
          else begin
            atoms := Literal raw.[!index + 1] :: !atoms;
            index := !index + 2
          end
      | '*' ->
          (match !atoms with Star :: _ -> () | _ -> atoms := Star :: !atoms);
          incr index
      | '?' ->
          atoms := Any :: !atoms;
          incr index
      | '[' -> (
          match class_atom raw (!index + 1) with
          | Error message -> failure := Some message
          | Ok (atom, next) ->
              atoms := atom :: !atoms;
              index := next)
      | character ->
          atoms := Literal character :: !atoms;
          incr index
    done;
    match !failure with
    | Some message -> Error message
    | None -> Ok { excluded; atoms = Array.of_list (List.rev !atoms) }

let compile source =
  Result.bind (split_terms source) (fun raw_terms ->
      let terms = Array.make (Array.length raw_terms) { excluded = false; atoms = [||] } in
      let failure = ref None in
      Array.iteri
        (fun index raw ->
          if !failure = None then
            match compile_term raw with
            | Ok term -> terms.(index) <- term
            | Error message ->
                failure :=
                  Some (Printf.sprintf "Attribute_pattern: term %d: %s" (index + 1) message))
        raw_terms;
      match !failure with
      | Some message -> Error message
      | None -> Ok { initially_selected = Array.length terms = 0 || terms.(0).excluded; terms })

let atom_matches atom character =
  match atom with
  | Literal expected -> character = expected
  | Any -> true
  | Class { bits; negated } -> Support.Bits.mem bits (Char.code character) <> negated
  | Star -> assert false

let rec trailing_stars atoms atom atom_count =
  if atom = atom_count then true
  else
    match atoms.(atom) with
    | Star -> trailing_stars atoms (atom + 1) atom_count
    | Literal _ | Any | Class _ -> false

let rec match_atoms atoms name atom_count name_length atom character last_star retry_character =
  if character = name_length then trailing_stars atoms atom atom_count
  else if atom < atom_count then
    match atoms.(atom) with
    | Star -> match_atoms atoms name atom_count name_length (atom + 1) character atom character
    | candidate when atom_matches candidate name.[character] ->
        match_atoms atoms name atom_count name_length (atom + 1) (character + 1) last_star
          retry_character
    | _ when last_star >= 0 && retry_character < name_length ->
        match_atoms atoms name atom_count name_length (last_star + 1) (retry_character + 1)
          last_star (retry_character + 1)
    | _ -> false
  else if last_star >= 0 && retry_character < name_length then
    match_atoms atoms name atom_count name_length (last_star + 1) (retry_character + 1) last_star
      (retry_character + 1)
  else false

let term_matches term name =
  match_atoms term.atoms name (Array.length term.atoms) (String.length name) 0 0 (-1) 0

let rec evaluate terms name index selected =
  if index = Array.length terms then selected
  else
    let term = terms.(index) in
    evaluate terms name (index + 1) (if term_matches term name then not term.excluded else selected)

let apply ~selected value name = evaluate value.terms name 0 selected
let matches value name = apply ~selected:value.initially_selected value name

let capture_groups atoms =
  let capture_of_atom = Array.make (Array.length atoms) (-1)
  and groups = ref []
  and atom = ref 0
  and capture = ref 0 in
  let add first atom_count kind =
    for index = first to first + atom_count - 1 do
      capture_of_atom.(index) <- !capture
    done;
    groups := { first_atom = first; atom_count; kind } :: !groups;
    incr capture
  in
  while !atom < Array.length atoms do
    match atoms.(!atom) with
    | Literal _ -> incr atom
    | Star ->
        add !atom 1 Capture_star;
        incr atom
    | Any ->
        let first = !atom in
        while !atom < Array.length atoms && atoms.(!atom) = Any do
          incr atom
        done;
        add first (!atom - first) (Capture_any (!atom - first))
    | Class _ ->
        let first = !atom in
        while
          !atom < Array.length atoms
          && match atoms.(!atom) with Class _ -> true | Literal _ | Any | Star -> false
        do
          incr atom
        done;
        let count = !atom - first in
        add first count (Capture_classes (Array.sub atoms first count))
  done;
  (Array.of_list (List.rev !groups), capture_of_atom)

let capture_kind_equal left right =
  match (left, right) with
  | Capture_star, Capture_star -> true
  | Capture_any left, Capture_any right -> left = right
  | Capture_classes left, Capture_classes right -> left = right
  | Capture_star, (Capture_any _ | Capture_classes _)
  | Capture_any _, (Capture_star | Capture_classes _)
  | Capture_classes _, (Capture_star | Capture_any _) ->
      false

let compile_rewrite ~pattern ~replacement =
  let one_term label source =
    Result.bind (split_terms source) (fun terms ->
        if Array.length terms <> 1 then
          Error (Printf.sprintf "Attribute_pattern: %s must contain exactly one glob term" label)
        else
          Result.bind
            (compile_term terms.(0))
            (fun term ->
              if term.excluded then
                Error (Printf.sprintf "Attribute_pattern: %s cannot be an exclusion" label)
              else Ok term.atoms))
  in
  Result.bind (one_term "rewrite pattern" pattern) (fun match_atoms ->
      Result.bind (one_term "rewrite replacement" replacement) (fun replacement_source ->
          let capture_groups, capture_of_atom = capture_groups match_atoms
          and replacement_groups, replacement_of_atom = capture_groups replacement_source in
          let capture_count = Array.length capture_groups
          and replacement_count = Array.length replacement_groups in
          if capture_count <> replacement_count then
            Error
              (Printf.sprintf
                 "Attribute_pattern: rewrite wildcard count differs (%d source, %d replacement)"
                 capture_count replacement_count)
          else begin
            let used = Array.make capture_count false
            and replacement_capture = Array.make replacement_count (-1)
            and failure = ref None in
            Array.iteri
              (fun replacement_index replacement_group ->
                if !failure = None then begin
                  let found = ref (-1) and source_index = ref 0 in
                  while !source_index < capture_count && !found < 0 do
                    if
                      (not used.(!source_index))
                      && capture_kind_equal capture_groups.(!source_index).kind
                           replacement_group.kind
                    then found := !source_index;
                    incr source_index
                  done;
                  if !found < 0 then begin
                    source_index := 0;
                    while !source_index < capture_count && !found < 0 do
                      if not used.(!source_index) then found := !source_index;
                      incr source_index
                    done
                  end;
                  if !found < 0 then
                    failure := Some "Attribute_pattern: rewrite wildcard mapping failed"
                  else begin
                    used.(!found) <- true;
                    replacement_capture.(replacement_index) <- !found
                  end
                end)
              replacement_groups;
            match !failure with
            | Some message -> Error message
            | None ->
                let atoms = ref [] and atom = ref 0 in
                while !atom < Array.length replacement_source do
                  match replacement_source.(!atom) with
                  | Literal character ->
                      atoms := Replacement_literal character :: !atoms;
                      incr atom
                  | Any | Star | Class _ ->
                      let group = replacement_of_atom.(!atom) in
                      atoms := Replacement_capture replacement_capture.(group) :: !atoms;
                      atom :=
                        replacement_groups.(group).first_atom
                        + replacement_groups.(group).atom_count
                done;
                Ok
                  {
                    match_atoms;
                    capture_of_atom;
                    capture_groups;
                    replacement_atoms = Array.of_list (List.rev !atoms);
                  }
          end))

let capture_match rewrite name =
  let atoms = rewrite.match_atoms in
  let atom_count = Array.length atoms and name_length = String.length name in
  let starts = Array.make (Array.length rewrite.capture_groups) 0
  and lengths = Array.make (Array.length rewrite.capture_groups) 0 in
  let set_capture atom start length =
    let capture = rewrite.capture_of_atom.(atom) in
    if capture >= 0 then begin
      starts.(capture) <- start;
      lengths.(capture) <- length
    end
  in
  let set_character atom character =
    let capture = rewrite.capture_of_atom.(atom) in
    if capture >= 0 then begin
      let group = rewrite.capture_groups.(capture) in
      if atom = group.first_atom then starts.(capture) <- character;
      lengths.(capture) <- character - starts.(capture) + 1
    end
  in
  let rec trailing atom character =
    if atom = atom_count then Some (starts, lengths)
    else
      match atoms.(atom) with
      | Star ->
          set_capture atom character 0;
          trailing (atom + 1) character
      | Literal _ | Any | Class _ -> None
  in
  let rec scan atom character last_star retry_character =
    if character = name_length then trailing atom character
    else if atom < atom_count then
      match atoms.(atom) with
      | Star ->
          set_capture atom character 0;
          scan (atom + 1) character atom character
      | candidate when atom_matches candidate name.[character] ->
          set_character atom character;
          scan (atom + 1) (character + 1) last_star retry_character
      | _ when last_star >= 0 && retry_character < name_length ->
          let retry_character = retry_character + 1 in
          set_capture last_star
            starts.(rewrite.capture_of_atom.(last_star))
            (retry_character - starts.(rewrite.capture_of_atom.(last_star)));
          scan (last_star + 1) retry_character last_star retry_character
      | _ -> None
    else if last_star >= 0 && retry_character < name_length then (
      let retry_character = retry_character + 1 in
      set_capture last_star
        starts.(rewrite.capture_of_atom.(last_star))
        (retry_character - starts.(rewrite.capture_of_atom.(last_star)));
      scan (last_star + 1) retry_character last_star retry_character)
    else None
  in
  scan 0 0 (-1) 0

let rewrite rule name =
  match capture_match rule name with
  | None -> None
  | Some (starts, lengths) ->
      let output = Buffer.create (String.length name + 16) in
      Array.iter
        (function
          | Replacement_literal character -> Buffer.add_char output character
          | Replacement_capture capture ->
              Buffer.add_substring output name starts.(capture) lengths.(capture))
        rule.replacement_atoms;
      Some (Buffer.contents output)

let compile_rewrite_set ~pattern ~replacement =
  Result.bind (split_terms pattern) (fun source_terms ->
      Result.bind (split_terms replacement) (fun replacement_terms ->
          let rec positive_terms index output =
            if index = Array.length source_terms then Ok (Array.of_list (List.rev output))
            else
              match compile_term source_terms.(index) with
              | Error message ->
                  Error
                    (Printf.sprintf "Attribute_pattern: rewrite source term %d: %s" (index + 1)
                       message)
              | Ok term ->
                  positive_terms (index + 1)
                    (if term.excluded then output else source_terms.(index) :: output)
          in
          Result.bind (positive_terms 0 []) (fun positive ->
              if Array.length positive = 0 then
                Error "Attribute_pattern: rewrite pattern has no positive terms"
              else if Array.length positive <> Array.length replacement_terms then
                Error
                  (Printf.sprintf
                     "Attribute_pattern: rewrite term count differs (%d positive source, %d \
                      replacement)"
                     (Array.length positive) (Array.length replacement_terms))
              else
                let rec compile index output =
                  if index = Array.length positive then Ok (Array.of_list (List.rev output))
                  else
                    Result.bind
                      (compile_rewrite ~pattern:positive.(index)
                         ~replacement:replacement_terms.(index))
                      (fun rule -> compile (index + 1) (rule :: output))
                in
                compile 0 [])))

let rewrite_set rules name =
  let output = ref None in
  Array.iter
    (fun rule -> match rewrite rule name with None -> () | Some value -> output := Some value)
    rules;
  !output
