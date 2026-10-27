Every authored head counted for the core has exactly one lowering rule.
The check reads the live corpus and specification, without a pinned snapshot.

  $ ../../core_census.exe ../../../sketches ../../../specification > census.txt
  $ cp ../../../specification/core.md core.md
  $ cat > rules.ml <<'EOF'
  > let lines path = In_channel.with_open_text path (fun channel ->
  >   let rec read acc = match In_channel.input_line channel with
  >     | None -> List.rev acc | Some line -> read (line :: acc) in read [])
  > let heads = lines Sys.argv.(1) |> List.filter_map (fun line ->
  >   match String.split_on_char ' ' line with "head" :: head :: _ -> Some head | _ -> None)
  > let rules = ["primitive"; "compiler rule"; "region"; "intrinsic"; "refused on web"]
  > let rows = lines Sys.argv.(2) |> List.filter_map (fun line ->
  >   if String.starts_with ~prefix:"| `" line then
  >     match String.split_on_char '`' line with _ :: head :: rest ->
  >       if List.exists (fun rule -> String.starts_with ~prefix:(" | " ^ rule ^ " |")
  >         (String.concat "`" rest)) rules then Some head else None
  >     | _ -> None
  >   else None)
  > let () =
  >   let failed = List.fold_left (fun failed head ->
  >     match List.length (List.filter ((=) head) rows) with
  >     | 1 -> failed
  >     | 0 -> Printf.eprintf "missing core rule: %s\n" head; true
  >     | _ -> Printf.eprintf "duplicate core rule: %s\n" head; true) false heads in
  >   if heads = [] then (prerr_endline "empty core census"; exit 1);
  >   if failed then exit 1;
  >   print_endline "census heads all have unique lowering rules"
  > EOF
  $ ocaml rules.ml census.txt core.md
  census heads all have unique lowering rules

A newly encountered head must fail until its lowering rule is specified.

  $ printf 'head future/operator 1\n' >> census.txt
  $ ocaml rules.ml census.txt core.md
  missing core rule: future/operator
  [1]
